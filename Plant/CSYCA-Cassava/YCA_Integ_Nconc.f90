!***************************************************************************************************************************
! This is the code from the section (DYNAMIC == INTEGR) lines 6148 - 6202 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine YCA_Integ_Nconc calculates N concentrations. 
!***************************************************************************************************************************
    
    SUBROUTINE YCA_Integ_Nconc ( &
        ISWNIT        & 
        )

! 2023-01-25 chp removed unused variables
!       BRSTAGE        

        USE YCA_First_Trans_m
        USE YCA_Control_Plant
        USE YCA_Control_Leaf
        
        IMPLICIT NONE
        EXTERNAL WARNING
        
        CHARACTER(LEN=1) ISWNIT   
        INTEGER :: BR                      ! Index for branch number/cohorts#          ! (From SeasInit)  
        INTEGER :: LF                      ! Loop counter leaves            #          !LPM 21MAR15 to add a leaf counter
!       REAL  BRSTAGE 
        INTEGER    :: Lcount                   ! counter for iterations in leaves alive
        
        !-----------------------------------------------------------------------
        !         Calculate nitrogen concentrations
        !-----------------------------------------------------------------------
                    
        IF (ISWNIT /= 'N') THEN
            
            !LPM 22MAY2015 The leaf nitrogen concentration tends to keep the same value through the growing season however in the future
            !could be considered different leaf nitrogen concentration based on the leaf age (maybe is it not needed that detail?)
            !LPM 30NOV2020 Modify leaf N concentration based on leaf age. Keep node(0,0)%LNCX with mean value from the SPE file to 
            !define NFG
            node(0,0)%LNCX = (LNCXS(0) + LNCXS(1))/2.0                                                    
            node(0,0)%LNCM = (LNCMN(0) + LNCMN(1))/2.0   
            !LPM 22MAY2015 The root nitrogenc concentration tends to keep constant through the growing season
            RNCX = (RNCXS(0) + RNCXS(1))/2.0 
            RNCM = (RNCMN(0) + RNCMN(1))/2.0
            !LPM 22MAY2015 the stem nitrogen concentration changes according with the canopy level age (non-lignified to lignified)
            IF((LLIFATT+LLIFSTT) > ZERO) THEN
                DO BR = 0, BRSTAGEINT                                                                                        
                 DO LF = 1, LNUMSIMSTG(BR)
                    IF (isLeafExpanding(node(BR,LF))) THEN
                         node(BR,LF)%SNCX = SNCXS(0)
                         node(BR,LF)%SNCM = SNCMN(0)
                         node(BR,LF)%LNCX = LNCXS(0)
                         node(BR,LF)%LNCM = LNCMN(0)
                    ELSE
                         node(BR,LF)%SNCX = SNCXS(0) + AMAX1(SNCXS(1)-SNCXS(0),((node(BR,LF)%LAGETT-LLIFGTT)*(SNCXS(1)-SNCXS(0)) / (LLIFATT+LLIFSTT) ))
                         node(BR,LF)%SNCM = SNCMN(0) + AMAX1(SNCMN(1)-SNCMN(0),((node(BR,LF)%LAGETT-LLIFGTT)*(SNCMN(1)-SNCMN(0)) / (LLIFATT+LLIFSTT) ))
                         node(BR,LF)%LNCX = LNCXS(0) + AMAX1(LNCXS(1)-LNCXS(0),((node(BR,LF)%LAGETT-LLIFGTT)*(LNCXS(1)-LNCXS(0)) / (LLIFATT+LLIFSTT) ))
                         node(BR,LF)%LNCM = LNCMN(0) + AMAX1(LNCMN(1)-LNCMN(0),((node(BR,LF)%LAGETT-LLIFGTT)*(LNCMN(1)-LNCMN(0)) / (LLIFATT+LLIFSTT) ))
                    ENDIF
                 ENDDO
                ENDDO
            ENDIF
            ! N concentrations
            ! Not reseting all to zero an leaving last value just in case it can't be calculated
            RANC = AMAX1(0.0,RANC)
            node%LANC = AMAX1(0.0,node%LANC)
            node%SANC = AMAX1(0.0,node%SANC)
            VANC = AMAX1(0.0,VANC)
            VCNC = AMAX1(0.0,VCNC)
            VMNC = AMAX1(0.0,VMNC)
            
            IF (RTWT > ZERO) RANC = ROOTN / RTWT        !EQN 017
            !IF (LFWT > ZERO) LANC = LEAFN / LFWT        !EQN 243 


            IF ((woodyWeight()) > ZERO .AND. (STWTP+CRWTP) > ZERO .AND. LFWT > ZERO) THEN
                DO BR = 0, BRSTAGEINT                                                                                        
                    DO LF = 1, LNUMSIMSTG(BR)
                        IF (node(BR,LF)%NODEWT*(woodyWeight())/(STWTP+CRWTP) > 0.0) THEN
                            node(BR,LF)%SANC = node(BR,LF)%STEMNN / (node(BR,LF)%NODEWT*(woodyWeight())/(STWTP+CRWTP))
                        ENDIF
                        !LPM 01FEB2021 adding restriction to avoid considering leaves that are almost falling
                        IF (isLeafActive(node(BR,LF)) .AND. leafAreaLeftToSenesce(node(BR,LF)) > 0.0) THEN
                            node(BR,LF)%LANC = node(BR,LF)%LEAFNN / ((leafAreaLeftToSenesce(node(BR,LF))/LAWL(1)) / (1.0-LPEFR)) 
                            IF (node(BR,LF)%LANC < 0.0) THEN 
                                WRITE(Message(1),'(A27,F4.1)') 'LANC below 0 with value of ',node(BR,LF)%LANC
                                WRITE(Message(2),'(A27,2F8.3)') 'LEAFN,LFWT had values of  ',node(BR,LF)%LEAFNN,LFWT
                                WRITE(Message(3),'(A27,2I5)') 'Branch, leaf number       ',BR,LF
                                CALL WARNING(3,'CSYCA',MESSAGE)
                                node(BR,LF)%LANC = AMAX1(0.0,node(BR,LF)%LANC)
                            ENDIF
                        ENDIF
                    ENDDO
                ENDDO
            ENDIF
            IF (VWAD > 0.0) VANC = VNAD/VWAD                                                                          !EQN 020

            
            SCNCT = 0.0
            SCNMT = 0.0
            LCNCT = 0.0
            LCNMT = 0.0 
            IF ((LFWT+woodyWeight()) > ZERO .AND. (STWTP+CRWTP) > ZERO) THEN
                DO BR = 0, BRSTAGEINT                                                                                        
                    DO LF = 1, LNUMSIMSTG(BR)
                        node(BR,LF)%SCNC = (node(BR,LF)%NODEWT*(woodyWeight())/(STWTP+CRWTP))*node(BR,LF)%SNCX
                        SCNCT =  SCNCT + node(BR,LF)%SCNC
                        node(BR,LF)%SCNM = (node(BR,LF)%NODEWT*(woodyWeight())/(STWTP+CRWTP))*node(BR,LF)%SNCM
                        SCNMT =  SCNMT + node(BR,LF)%SCNM
                        node(BR,LF)%LCNC = ((leafAreaLeftToSenesce(node(BR,LF))/LAWL(1)) / (1.0-LPEFR))*node(BR,LF)%LNCX
                        LCNCT =  LCNCT + node(BR,LF)%LCNC
                        node(BR,LF)%LCNM = ((leafAreaLeftToSenesce(node(BR,LF))/LAWL(1)) / (1.0-LPEFR))*node(BR,LF)%LNCM
                        LCNMT =  LCNMT + node(BR,LF)%LCNM
                    ENDDO
                ENDDO
                VCNC = (LCNCT+SCNCT)/ &                      !EQN 021
                (AMAX1(0.0,LFWT)+AMAX1(0.0,woodyWeight()))
                VMNC = (LCNMT+SCNMT)/ &                      !EQN 022
                (AMAX1(0.0,LFWT)+AMAX1(0.0,woodyWeight()))
            ENDIF  
            
            
            
            SDNC = 0.0
            SRANC = 0.0
            IF (SEEDRS > 0.0) SDNC = SEEDN/(SEEDRS+SDCOAT)
            IF (SRWT > 0) SRANC = SROOTN/SRWT
            node%LNCR = 0.0
            node%SNCR = 0.0
            RNCR = 0.0
            Lcount = 0
            DO BR = 0, BRSTAGEINT                                                                              !LPM25MAY2015 To consider different N concentration by node according with node age                                                                       
                DO LF = 1, LNUMSIMSTG(BR)  
                    IF (isLeafAlive(node(BR,LF))) THEN
                        Lcount = Lcount + 1 
                        IF (node(BR,LF)%SNCX > 0.0) THEN
                            node(BR,LF)%SNCR = AMAX1(0.0,AMIN1(1.0,node(BR,LF)%SANC/node(BR,LF)%SNCX))
                        ENDIF
                        IF (node(BR,LF)%LNCX > 0.0) THEN
                            node(BR,LF)%LNCR = AMAX1(0.0,AMIN1(1.0,node(BR,LF)%LANC/node(BR,LF)%LNCX))
                        ENDIF
                    ENDIF
                ENDDO
            ENDDO
            !SNCRM = SUM(SNCR, MASK = SNCR >= 0.0)/MAX(1,COUNT(SNCR))
            !SNCRM = 1.0
            SNCRM = SUM(node%SNCR)/ MAX(1,Lcount)
            LNCRM = SUM(node%LNCR)/ MAX(1,Lcount)
            IF (RNCX > 0.0) RNCR = AMAX1(0.0,AMIN1(1.0,RANC/RNCX))
        ELSE
            node%LNCR = 1.0
            node%SNCR = 1.0
            RNCR = 1.0
            LNCRM = 1.0
            SNCRM = 1.0
        ENDIF
        
    END SUBROUTINE YCA_Integ_Nconc
            
