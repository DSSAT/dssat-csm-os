!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.INTEGR) lines 6148 - 6202 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine YCA_Integ_Nconc calculates N concentrations. 
!***************************************************************************************************************************
    
    SUBROUTINE YCA_Integ_Nconc ( &
        ISWNIT    ,    BRSTAGE        & 
        )
        
        USE YCA_First_Trans_m
        
        IMPLICIT NONE
        
        CHARACTER(LEN=1) ISWNIT   
        REAL  BRSTAGE 
        
        !-----------------------------------------------------------------------
        !         Calculate nitrogen concentrations
        !-----------------------------------------------------------------------
                    
        IF (ISWNIT /= 'N') THEN
            
            !LPM 22MAY2015 The leaf nitrogen concentration tends to keep the same value through the growing season however in the future
            !could be considered different leaf nitrogen concentration based on the leaf age (maybe is it not needed that detail?)
            LNCX = (LNCXS(0) + LNCXS(1))/2.0                                                    
            LNCM = (LNCMN(0) + LNCMN(1))/2.0   
            !LPM 22MAY2015 The root nitrogenc concentration tends to keep constant through the growing season
            RNCX = (RNCXS(0) + RNCXS(1))/2.0 
            RNCM = (RNCMN(0) + RNCMN(1))/2.0
            !LPM 22MAY2015 the stem nitrogen concentration changes according with the canopy level age (non-lignified to lignified)
            IF((LLIFATT+LLIFSTT) > ZERO) THEN
                DO BR = 0, BRSTAGE                                                                                        
                 DO LF = 1, LNUMSIMSTG(BR)
                    plant(BR,LF)%SNCX = SNCXS(0) + (plant(BR,LF)%LAGETT*(SNCXS(1)-SNCXS(0))/(LLIFATT+LLIFSTT))
                    plant(BR,LF)%SNCM = SNCMN(0) + (plant(BR,LF)%LAGETT*(SNCMN(1)-SNCMN(0))/(LLIFATT+LLIFSTT))
                 ENDDO
                ENDDO
            ENDIF
            ! N concentrations
            ! Not reseting all to zero an leaving last value just in case it can't be calculated
            RANC = AMAX1(0.0,RANC)
            LANC = AMAX1(0.0,LANC)
            plant%SANC = AMAX1(0.0,plant%SANC)
            VANC = AMAX1(0.0,VANC)
            VCNC = AMAX1(0.0,VCNC)
            VMNC = AMAX1(0.0,VMNC)
            
            IF (RTWT > ZERO) RANC = ROOTN / RTWT        !EQN 017
            IF (LFWT > ZERO) LANC = LEAFN / LFWT        !EQN 243 

            IF ((STWT+CRWT) > ZERO .AND. (STWTP+CRWTP) > ZERO) THEN
                DO BR = 0, BRSTAGE                                                                                        
                    DO LF = 1, LNUMSIMSTG(BR)
                        IF (plant(BR,LF)%NODEWT*(STWT+CRWT)/(STWTP+CRWTP) > 0.0) THEN
                            plant(BR,LF)%SANC = plant(BR,LF)%STEMNN / (plant(BR,LF)%NODEWT*(STWT+CRWT)/(STWTP+CRWTP))
                        ENDIF
                    ENDDO
                ENDDO
            ENDIF
            IF (VWAD > 0.0) VANC = VNAD/VWAD                                                                          !EQN 020
            IF (LANC < 0.0) THEN 
                WRITE(Message(1),'(A27,F4.1)') 'LANC below 0 with value of ',LANC
                WRITE(Message(2),'(A27,2F5.1)') 'LEAFN,LFWT had values of   ',LEAFN,LFWT
                CALL WARNING(2,'CSYCA',MESSAGE)
                LANC = AMAX1(0.0,LANC)
            ENDIF
            
            SCNCT = 0.0
            SCNMT = 0.0
            IF ((LFWT+STWT+CRWT) > ZERO .AND. (STWTP+CRWTP) > ZERO) THEN
                DO BR = 0, BRSTAGE                                                                                        
                    DO LF = 1, LNUMSIMSTG(BR)
                        plant(BR,LF)%SCNC = (plant(BR,LF)%NODEWT*(STWT+CRWT)/(STWTP+CRWTP))*plant(BR,LF)%SNCX
                        SCNCT =  SCNCT + plant(BR,LF)%SCNC
                        plant(BR,LF)%SCNM = (plant(BR,LF)%NODEWT*(STWT+CRWT)/(STWTP+CRWTP))*plant(BR,LF)%SNCM
                        SCNMT =  SCNMT + plant(BR,LF)%SCNM
                    ENDDO
                ENDDO
                VCNC = (LNCX*AMAX1(0.0,LFWT)+SCNCT)/ &                      !EQN 021
                (AMAX1(0.0,LFWT)+AMAX1(0.0,STWT+CRWT))
                VMNC = (LNCM*AMAX1(0.0,LFWT)+SCNMT)/ &                      !EQN 022
                (AMAX1(0.0,LFWT)+AMAX1(0.0,STWT+CRWT))
            ENDIF  
            
            
            
            SDNC = 0.0
            SRANC = 0.0
            IF (SEEDRS.GT.0.0) SDNC = SEEDN/(SEEDRS+SDCOAT)
            IF (SRWT.GT.0) SRANC = SROOTN/SRWT
            LNCR = 0.0
            plant%SNCR = 0.0
            RNCR = 0.0
            IF (LNCX.GT.0.0) LNCR = AMAX1(0.0,AMIN1(1.0,LANC/LNCX))
            DO BR = 0, BRSTAGE                                                                              !LPM25MAY2015 To consider different N concentration by node according with node age                                                                       
                DO LF = 1, LNUMSIMSTG(BR)  
                    IF (plant(BR,LF)%SNCX > 0.0) THEN
                        plant(BR,LF)%SNCR = AMAX1(0.0,AMIN1(1.0,plant(BR,LF)%SANC/plant(BR,LF)%SNCX))
                    ENDIF
                ENDDO
            ENDDO
            !SNCRM = SUM(SNCR, MASK = SNCR.GE.0.0)/MAX(1,COUNT(SNCR))
            SNCRM = 1.0
            IF (RNCX > 0.0) RNCR = AMAX1(0.0,AMIN1(1.0,RANC/RNCX))
        ELSE
            LNCR = 1.0
            plant%SNCR = 1.0
            RNCR = 1.0
        ENDIF
        
    END SUBROUTINE YCA_Integ_Nconc
            
