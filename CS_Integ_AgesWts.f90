!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.INTEGR) lines 5534 - 5736 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module CS_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine CS_Integ_AgesWts calls subroutines to update the seasonal data with the data for the current day: ages and 
! dry weights. 
!***************************************************************************************************************************
    
    SUBROUTINE CS_Integ_AgesWts ( &
        NLAYR,      BRSTAGE       & 
        )
        
        USE CS_First_Trans_m
    
        IMPLICIT NONE
        
        INTEGER NLAYR
        REAL BRSTAGE, TEMP
        
        !-----------------------------------------------------------------------
        !         Update ages
        !-----------------------------------------------------------------------
        IF (YEARDOY.GT.PLYEARDOY) THEN
            DAP = DAP + 1
            IF (EYEARDOY.GT.0) DAE = DAE + 1
        ENDIF
        TVI1 = LNUMSOLDESTA
        LNUMSOLDESTA = -99
        ! Leaves that present at beginning of day
        !DO L = 1,LNUMSG
        !    LAGETT(L) = LAGETT(L) + TTLFLIFE*EMRGFR                                                                    !EQN 358
        !    ! Accelerated senescence at base of dense leaf canopy
        !    IF (LAI.GT.LAIXX) THEN
        !        IF (L.EQ.TVI1) THEN
        !            ! Increase age if deep shading at base of canopy
        !            ! (Maximum accelerated ageing set in SPE file)
        !            ! Accelerated ageing of lowermost active leaf
        !            IF (LAGETT(L).LT.LLIFGTT+LLIFATT) THEN
        !                LAGETT(L) = AMIN1(LAGETT(L)+LLIFX,LLIFGTT+LLIFATT)                                             !EQN 359
        !                LLIFXUNUSED = (LAGETT(L)+LLIFX)-(LLIFGTT+LLIFATT)                                              !EQN 360a
        !            ELSE
        !                LLIFXUNUSED = LLIFX                                                                            !EQN 360b
        !            ENDIF
        !        ENDIF
        !        ! If not all acceleration used up
        !        IF (L.EQ.TVI1+1) THEN
        !            IF (LLIFXUNUSED.GT.0.0) THEN
        !                IF (LAGETT(L).LT.LLIFGTT+LLIFATT) LAGETT(L) = AMIN1(LAGETT(L)+LLIFXUNUSED,LLIFGTT+LLIFATT)     !EQN 361
        !            ENDIF
        !        ENDIF
        !    ENDIF
        !    LAGEP(L) = LAGEP(L) + (TTLFLIFE*EMRGFR)/PHINT                                                              !EQN 362
        !    ! Days growing
        !    !IF (LAGETT(L).LE.LLIFGTT) THEN
        !    !    DGLF(L) = DGLF(L) + EMRGFR                                                                             !EQN 363a LPM 21MAR15 We will assume a fix growing duration
        !    !ELSE  
        !    !    IF (LAGETT(L)-TTLFLIFE*EMRGFR.LT.LLIFGTT) THEN
        !    !        TVR1 = (LLIFGTT-(LAGETT(L)-TTLFLIFE*EMRGFR))/(TTLFLIFE*EMRGFR)
        !    !        DGLF(L) = DGLF(L) + TVR1                                                                           !EQN 363b
        !    !    ENDIF  
        !    !ENDIF
        !    IF (DGLF(L).LT.LLIFGD) THEN                                                                                     !LPM 21MAR15 DGLF will have a maximum value of 10 days
        !        DGLF(L) = DGLF(L) + EMRGFR
        !    ENDIF
        !    
        !    ! Days active
        !    IF (LAGETT(L).GT.LLIFGTT.AND.LAGETT(L).LT.LLIFGTT+LLIFATT) THEN
        !        IF (LNUMSOLDESTA.LT.0) THEN
        !            LNUMSOLDESTA = L
        !        ENDIF
        !        IF (LAGETT(L)-TTLFLIFE*EMRGFR.LT.LLIFGTT) THEN
        !            TVR1 = (LLIFGTT-(LAGETT(L)-TTLFLIFE*EMRGFR))/(TTLFLIFE*EMRGFR)
        !            DALF(L) = DALF(L) + (1.0-TVR1)                                                                     !EQN 364a
        !        ELSE
        !            IF (LAGETT(L).LE.LLIFGTT+LLIFATT) THEN
        !                DALF(L) = DALF(L) + 1.0                                                                        !EQN 364b
        !            ELSE
        !                IF (LAGETT(L)-TTLFLIFE*EMRGFR.LT.LLIFGTT+LLIFATT) THEN
        !                    TVR1 = ((LLIFGTT+LLIFATT)-(LAGETT(L)-TTLFLIFE*EMRGFR))/(TTLFLIFE*EMRGFR)
        !                    DALF(L) = DALF(L) + TVR1                                                                   !EQN 364c
        !                ENDIF
        !            ENDIF
        !        ENDIF
        !    ENDIF
        !    ! Days senescing
        !    IF (LAGETT(L).GT.LLIFGTT+LLIFATT) THEN
        !        IF (LAGETT(L)-TTLFLIFE*EMRGFR.LT.LLIFGTT+LLIFATT) THEN
        !            TVR1 = ((LLIFGTT+LLIFATT)-(LAGETT(L)-TTLFLIFE*EMRGFR))/(TTLFLIFE*EMRGFR)
        !            DSLF(L) = DSLF(L) + (1.0-TVR1)                                                                     !EQN 365a
        !        ELSE
        !            IF (LAGETT(L).LE.LLIFGTT+LLIFATT+LLIFSTT) THEN
        !                DSLF(L) = DSLF(L) + 1.0                                                                        !EQN 365b
        !            ELSE
        !                IF (LAGETT(L)-TTLFLIFE*EMRGFR.LT.LLIFGTT+LLIFATT+LLIFSTT) THEN
        !                    TVR1 = ((LLIFGTT+LLIFATT+LLIFSTT)-(LAGETT(L)-TTLFLIFE*EMRGFR))/(TTLFLIFE*EMRGFR)
        !                    DSLF(L) = DSLF(L) + TVR1                                                                   !EQN 365c
        !                    LDEATHDAP(L) = DAP
        !                ENDIF
        !            ENDIF
        !        ENDIF
        !    ENDIF
        !ENDDO
        !IF (LNUMG.GT.0.0) THEN                                                                             
        !    IF (LNUMSG.LT.LNUMX) THEN
        !        LAGETT(LNUMSG+1) = LAGETT(LNUMSG+1)+(TTLFLIFE*EMRGFR)*AMAX1(0.0,LNUMG-LNUMNEED)/LNUMG                  !EQN 366
        !        LAGEP(LNUMSG+1)=LAGEP(LNUMSG+1)+AMAX1(0.0,LNUMG-LNUMNEED)                                              !EQN 367
        !        DGLF(LNUMSG+1) = AMAX1(0.0,LNUMG-LNUMNEED)/LNUMG                                                       !EQN 368
        !    ENDIF
        ! ENDIF
        
        DO BR = 0, BRSTAGE                                                                                        !LPM 21MAR15
            DO LF = 1, LNUMSIMSTG(BR)                                                                            !LPM 23MAY2015 Modified to avoid high values of LAGETT 
                IF (DGLF(BR,LF).GE.LLIFGD.AND.LAGETT(BR,LF)-TTLFLIFE*EMRGFR.LT.LLIFATT+LLIFSTT) THEN             !LPM 21MAR15 DGLF will have a maximum value of 10 days
                    LAGETT(BR,LF) = LAGETT(BR,LF) + TTLFLIFE*EMRGFR                                              !EQN 358
                    ! Accelerated senescence at base of dense leaf canopy
                    IF (LAI.GT.LAIXX) THEN
                        IF (LF.EQ.TVI1.AND.BR.EQ.BROLDESTA) THEN
                            ! Increase age if deep shading at base of canopy
                            ! (Maximum accelerated ageing set in SPE file)
                            ! Accelerated ageing of lowermost active leaf
                            IF (LAGETT(BR,LF).LT.LLIFATT) THEN                                                  !LPM 28MAR15 LLIFGT was deleted 
                                LLIFXUNUSED = (LAGETT(BR,LF)+LLIFX)-LLIFATT                                                    !EQN 360a LPM 28MAR15 LLIFXUNUSED should be before of the estimation of LAGETT 
                                LAGETT(BR,LF) = AMIN1(LAGETT(BR,LF)+LLIFX,LLIFATT)                                             !EQN 359
                            ELSE
                                LLIFXUNUSED = LLIFX                                                                            !EQN 360b
                            ENDIF
                        ENDIF
                        ! If not all acceleration used up
                        IF (LF.EQ.TVI1+1) THEN
                            IF (LLIFXUNUSED.GT.0.0) THEN
                                IF (LAGETT(BR,LF).LT.LLIFATT) LAGETT(BR,LF) = AMIN1(LAGETT(BR,LF)+LLIFXUNUSED,LLIFATT)     !EQN 361
                            ENDIF
                        ENDIF
                    ENDIF
                !LAGEP(BR,LF) = LAGEP(BR,LF) + (TTLFLIFE*EMRGFR)/PHINT                                                              !EQN 362 !LPM21MAY2015 this variable is not used
                
                ! Days active
                    IF (LAGETT(BR,LF).LE.LLIFATT) THEN                                                  !LPM 28MAR15 LLIFGT was deleted 
                        IF (LNUMSOLDESTA.LT.0) THEN
                            LNUMSOLDESTA = LF
                            BROLDESTA = BR
                        ENDIF
                        DALF(BR,LF) = DALF(BR,LF) + 1.0                                                                        !EQN 364b
                    ELSE
                        IF (LAGETT(BR,LF)-TTLFLIFE*EMRGFR.LT.LLIFATT) THEN                     !LPM 28MAR15 LLIFGT was deleted 
                            TVR1 = (LLIFATT-(LAGETT(BR,LF)-TTLFLIFE*EMRGFR))/(TTLFLIFE*EMRGFR)
                            DALF(BR,LF) = DALF(BR,LF) + TVR1                                                                   !EQN 364c
                        ENDIF
                    ENDIF

                    ! Days senescing
                    IF (LAGETT(BR,LF).GT.LLIFATT) THEN
                        IF (LAGETT(BR,LF)-TTLFLIFE*EMRGFR.LT.LLIFATT) THEN
                            TVR1 = (LLIFATT-(LAGETT(BR,LF)-TTLFLIFE*EMRGFR))/(TTLFLIFE*EMRGFR)
                            DSLF(BR,LF) = DSLF(BR,LF) + (1.0-TVR1)                                                                     !EQN 365a
                        ELSE
                            IF (LAGETT(BR,LF).LE.LLIFATT+LLIFSTT) THEN
                                DSLF(BR,LF) = DSLF(BR,LF) + 1.0                                                                        !EQN 365b
                            ELSE
                                IF (LAGETT(BR,LF)-TTLFLIFE*EMRGFR.LT.LLIFATT+LLIFSTT) THEN
                                    TVR1 = ((LLIFATT+LLIFSTT)-(LAGETT(BR,LF)-TTLFLIFE*EMRGFR))/(TTLFLIFE*EMRGFR)
                                    DSLF(BR,LF) = DSLF(BR,LF) + TVR1                                                                   !EQN 365c
                                    LDEATHDAP(BR,LF) = DAP
                                ENDIF
                            ENDIF
                        ENDIF
                    ENDIF
                
                ELSE 
                    DGLF(BR,LF) = DGLF(BR,LF) + EMRGFR
                ENDIF

                IF (LNUMG.GT.0.0.AND.BR.EQ.BRSTAGE.AND.LF.EQ.LNUMSIMSTG(BR)) THEN                                            !LPM 28MAR15 Modified as part of the DO loop
                    IF (LNUMSG.LT.LNUMX) THEN
                        LAGETT(BR,LF+1) = LAGETT(BR,LF+1)+(TTLFLIFE*EMRGFR)*AMAX1(0.0,LNUMG-LNUMNEED)/LNUMG                  !EQN 366
                        !LAGEP(BR,LF+1)=LAGEP(BR,LF+1)+AMAX1(0.0,LNUMG-LNUMNEED)                                              !EQN 367 !LPM21MAY2015 this variable is not used
                        DGLF(BR,LF+1) = AMAX1(0.0,LNUMG-LNUMNEED)/LNUMG                                                       !EQN 368
                    ENDIF
                ENDIF 
            ENDDO
        ENDDO
        ! Leaves that emerged during day

        
        !-----------------------------------------------------------------------
        !         Update dry weights
        !-----------------------------------------------------------------------
        
        ! Dry weights
        ! LAH No growth/development on planting day
        IF (YEARDOY.GE.PLYEARDOY) THEN
            ! Assimilation and respiration
            CARBOC = CARBOC + AMAX1(0.0,(CARBOBEG+CARBOADJ))                                                           !EQN 424
            RESPRC = RESPRC + RTRESPADJ                                                                                !EQN 425
            RESPTC = 0.0  ! Respiration tops - not yet used
            RESPC = RESPRC + RESPTC                                                                                    !EQN 426
            LFWT = LFWT + GROLFADJ - SENLFG - SENLFGRS - LWPH                                                          !EQN 427
            LWPHC = LWPHC +  LWPH                                                                                      !EQN 428
            IF (LFWT.LT.-1.0E-8) THEN
                WRITE(Message(1),'(A35,F4.1,A14)') 'Leaf weight less than 0! Weight of ',lfwt,' reset to zero'
                CALL WARNING(1,'CSCGR',MESSAGE)
                LFWT = 0.0
            ENDIF
            RSWT = RSWT+GRORS-RSWPH-SRWTGRS+RSWTGLFADJ                                                                 !EQN 429
            RSWPHC = RSWPHC +  RSWPH                                                                                   !EQN 430
            ! Reserves distribution 
            ! Max concentration in leaves increases through life cycle.
            !IF (PSTART(MSTG).GT.0.0) LLRSWT = AMIN1(RSWT,LFWT*(1.0-LPEFR)*(RSCLX/100.0)*DSTAGE)                        !EQN 431 !LPM 04MAR15 MSTG TO PSX
            !IF (PSTART(MSTG).GT.0.0) LPERSWT = AMIN1(RSWT-LLRSWT,LFWT*LPEFR*(RSCLX/100.0)*DSTAGE)                      !EQN 432 !LPM 04MAR15 MSTG TO PSX
            
            !LPM 21MAY2015 The reserves distribution will not be included, it needs to be reviewed
            !IF (PSTART(PSX).GT.0.0) LLRSWT = AMIN1(RSWT,LFWT*(1.0-LPEFR)*(RSCLX/100.0)*DSTAGE)                        !EQN 431
            !IF (PSTART(PSX).GT.0.0) LPERSWT = AMIN1(RSWT-LLRSWT,LFWT*LPEFR*(RSCLX/100.0)*DSTAGE)                      !EQN 432
            !IF (STWT+CRWT.GT.0.0) THEN
            !    STRSWT = (RSWT-LLRSWT-LPERSWT)*STWT/(STWT+CRWT)                                                        !EQN 433a
            !    CRRSWT = (RSWT-LLRSWT-LPERSWT)*CRWT/(STWT+CRWT)                                                        !EQN 434a
            !ELSE
            !    STRSWT = (RSWT-LLRSWT-LPERSWT)                                                                         !EQN 433b
            !    CRRSWT = 0.0                                                                                           !EQN 434b
            !ENDIF
            IF (RSWT.LT.0.0) THEN
                IF (ABS(RSWT).GT.1.0E-6) THEN
                    WRITE(Message(1),'(A30,A11,F12.9)') 'Reserves weight reset to zero.', 'Weight was ',rswt
                    CALL WARNING(1,'CSCGR',MESSAGE)
                    RSWT = 0.0
                ENDIF
            ENDIF
            RSWTX = AMAX1(RSWTX,RSWT)
            STWT = STWT + GROSTADJ - SWPH
            IF (STWT.LT.1.0E-06) THEN
                IF (STWT.LT.0.0) WRITE(fnumwrk,*)'Stem weight less than 0! ',STWT
                STWT = 0.0
            ENDIF
            SWPHC = SWPHC +  SWPH                                                                                      !EQN 435
            CRWT = CRWT + GROCRADJ                                                                                     !EQN 436 
            
            SENTOPLITTER = SENTOPLITTER + SENTOPLITTERG                                                                !EQN 437
            SENCL(0) = SENCL(0) + SENTOPLITTERG*0.4                                                                    !EQN 438
            SENLL(0) = SENLL(0) + (SENLFG*LLIGP/100)*(SENFR)                                                           !EQN 439
            RTWT = 0.0
            DO L = 1, NLAYR
                RTWTL(L) = RTWTL(L) + RTWTGL(L) - RTWTSL(L) - RTWTUL(L)                                                !EQN 399
                SENWL(L) = SENWL(L) + RTWTSL(L)                                                                        !EQN 440
                SENCL(L) = SENCL(L) + RTWTSL(L) * 0.4                                                                  !EQN 441
                SENLL(L) = SENLL(L) + RTWTSL(L) * RLIGP/100.0                                                          !EQN 442
                ! Totals
                RTWT = RTWT + RTWTL(L)                                                                                 !EQN 443
                SENROOT = SENROOT + RTWTSL(L)                                                                          !EQN 444
                SENCS = SENCS + RTWTSL(L) * 0.4                                                                        !EQN 445
                SENLS = SENLS + RTWTSL(L) * RLIGP/100.0                                                                !EQN 446
            END DO
            SRWT = SRWT + GROSR + SRWTGRS + (RTWTG-RTWTGADJ+RTRESP-RTRESPADJ) ! Root N adjustment                      !EQN 447
        ENDIF
        
        SEEDRS = AMAX1(0.0,SEEDRS-GROLSSD-SEEDRSAVR)                                                                   !EQN 285
        IF (CFLSDRSMSG.NE.'Y'.AND.SEEDRS.LE.0.0.AND.LNUM.LT.4.0) THEN
            WRITE(Message(1),'(A44,F3.1)') 'Seed reserves all used but leaf number only ',lnum
            WRITE(Message(2),'(A58)') 'For good establishment seed reserves should last to leaf 4'
            WRITE(Message(3),'(A55)') 'Maybe stick too small or specific leaf area set too low'
            CALL WARNING(3,'CSCGR',MESSAGE)
            CFLSDRSMSG = 'Y'
        ENDIF
        SEEDUSE = SEEDUSE + GROLSSD+SEEDRSAVR                                                                          !EQN 448
        SEEDUSER = SEEDUSER + SEEDRSAVR                                                                                !EQN 449
        SEEDUSET = SEEDUSET + GROLSSD                                                                                  !EQN 450
        SEEDRSAV = SEEDRS
        IF (SRNOPD.GT.0.0) SRWUD = SRWT/SRNOPD                                                                         !EQN 292
        
        IF ((LFWT+STWT+CRWT+RSWT).GT.0.0) THEN
            HIAD = SRWT/(LFWT+STWT+CRWT+SRWT+RSWT)                                                                     !EQN 293
        ENDIF
        IF (RTWT.GT.0.0) SHRTD = (LFWT+STWT+CRWT+RSWT) / RTWT                                                          !EQN 294
    END SUBROUTINE CS_Integ_AgesWts
