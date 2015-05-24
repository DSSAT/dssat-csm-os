!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.INTEGR) lines 6148 - 6202 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module CS_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine CS_Integ_Nconc calculates N concentrations. 
!***************************************************************************************************************************
    
    SUBROUTINE CS_Integ_Nconc ( &
        ISWNIT    ,    BRSTAGE        & 
        )
        
        USE CS_First_Trans_m
        
        IMPLICIT NONE
        
        CHARACTER(LEN=1) ISWNIT              
        
        !-----------------------------------------------------------------------
        !         Calculate nitrogen concentrations
        !-----------------------------------------------------------------------
                    
        IF (ISWNIT.NE.'N') THEN
            ! Critical and minimum N concentrations
            !LNCX = LNCXS(0) + DSTAGE*(LNCXS(1)-LNCXS(0))                                                               !EQN 014
            !SNCX = SNCXS(0) + DSTAGE*(SNCXS(1)-SNCXS(0))                                                               !EQN 015
            !RNCX = RNCXS(0) + DSTAGE*(RNCXS(1)-RNCXS(0))                                                               !EQN 016
            !LNCM = LNCMN(0) + DSTAGE*(LNCMN(1)-LNCMN(0))                                                               !EQN 011
            !SNCM = SNCMN(0) + DSTAGE*(SNCMN(1)-SNCMN(0))                                                               !EQN 012
            !RNCM = RNCMN(0) + DSTAGE*(RNCMN(1)-RNCMN(0))                                                               !EQN 013
            
            !LPM 22MAY2015 The leaf nitrogen concentration tends to keep the same value through the growing season however in the future
            !could be considered different leaf nitrogen concentration based on the leaf age (maybe is it not needed that detail?)
            LNCX = (LNCXS(0) + LNCXS(1))/2.0                                                    
            LNCM = (LNCMN(0) + LNCMN(1))/2.0   
            !LPM 22MAY2015 The root nitrogenc concentration tends to keep constant through the growing season
            RNCX = (RNCXS(0) + RNCXS(1))/2.0 
            RNCM = (RNCMN(0) + RNCMN(1))/2.0
            !LPM 22MAY2015 the stem nitrogen concentration changes according with the canopy level age (non-lignified to lignified)
            DO BR = 0, BRSTAGE                                                                                        
                DO LF = 1, LNUMSIMSTG(BR)
                    SNCX(BR,LF) = SNCXS(0) + (LAGETT(BR,LF)*(SNCXS(1)-SNCXS(0))/(LLIFATT+LLIFSTT))
                    SNCM(BR,LF) = SNCMN(0) + (LAGETT(BR,LF)*(SNCMN(1)-SNCMN(0))/(LLIFATT+LLIFSTT))
                ENDDO
            ENDDO
            ! N concentrations
            RANC = 0.0
            LANC = 0.0
            SANC = 0.0
            VANC = 0.0
            VCNC = 0.0
            VMNC = 0.0
            IF (RTWT.GT.1.0E-5) RANC = ROOTN / RTWT                                                                    !EQN 017
            IF (LFWT.GT.1.0E-5) LANC = LEAFN / LFWT                                                                    !EQN 243
            IF (STWT+CRWT.GT.1.0E-5) SANC = STEMN / (STWT+CRWT)
            IF (STWT+CRWT.GT.1.0E-5) THEN
                DO BR = 0, BRSTAGE                                                                                        
                    DO LF = 1, LNUMSIMSTG(BR)
                       SANC(BR,LF) = STEMNN(BR,LF) / (NODEWT(BR,LF)*(STWT+CRWT)/(STWTP+CRWTP))
                    ENDDO
                ENDDO
            ENDIF
            IF (VWAD.GT.0.0) VANC = VNAD/VWAD                                                                          !EQN 020
            IF (LANC.LT.0.0) THEN 
                WRITE(Message(1),'(A27,F4.1)') 'LANC below 0 with value of ',LANC
                WRITE(Message(2),'(A27,2F5.1)') 'LEAFN,LFWT had values of   ',LEAFN,LFWT
                CALL WARNING(2,'CSCAS',MESSAGE)
                LANC = AMAX1(0.0,LANC)
            ENDIF
            IF (LFWT+STWT+CRWT.GT.0.0) VCNC = (LNCX*AMAX1(0.0,LFWT)+SNCX*AMAX1(0.0,STWT+CRWT))/ &                      !EQN 021
                (AMAX1(0.0,LFWT)+AMAX1(0.0,STWT+CRWT))
            IF (LFWT+STWT+CRWT.GT.0.0) VMNC = (LNCM*AMAX1(0.0,LFWT)+SNCM*AMAX1(0.0,STWT+CRWT))/ &                      !EQN 022
                (AMAX1(0.0,LFWT)+AMAX1(0.0,STWT+CRWT))
                
            SDNC = 0.0
            SRANC = 0.0
            IF (SEEDRS.GT.0.0) SDNC = SEEDN/(SEEDRS+SDCOAT)
            IF (SRWT.GT.0) SRANC = SROOTN/SRWT
            LNCR = 0.0
            SNCR = 0.0
            RNCR = 0.0
            IF (LNCX.GT.0.0) LNCR = AMAX1(0.0,AMIN1(1.0,LANC/LNCX))
            IF (SNCX.GT.0.0) SNCR = AMAX1(0.0,AMIN1(1.0,SANC/SNCX))
            IF (RNCX.GT.0.0) RNCR = AMAX1(0.0,AMIN1(1.0,RANC/RNCX))
        ELSE
            LNCR = 1.0
            SNCR = 1.0
            RNCR = 1.0
        ENDIF
        
    END SUBROUTINE CS_Integ_Nconc
            
