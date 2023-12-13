!***************************************************************************************************************************
! This is the code from the section (DYNAMIC == INTEGR) lines 5534 - 5736 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine YCA_Integ_AgesWts calls subroutines to update the seasonal data with the data for the current day: ages and 
! dry weights. 
!***************************************************************************************************************************
    
    SUBROUTINE YCA_Integ_AgesWts ( &
        NLAYR       & 
        )

! 2023-01-25 chp removed unused variables
!       BRSTAGE        

        USE YCA_First_Trans_m
        USE YCA_Control_Leaf
        USE YCA_Control_Plant
    
        IMPLICIT NONE
        EXTERNAL WARNING
        
        INTEGER NLAYR
        INTEGER :: BR                      ! Index for branch number/cohorts#          ! (From SeasInit)  
        INTEGER :: LF                      ! Loop counter leaves            #          !LPM 21MAR15 to add a leaf counter
!       REAL BRSTAGE, TEMP
        
        !-----------------------------------------------------------------------
        !         Update ages
        !-----------------------------------------------------------------------
        IF (YEARDOY > PLYEARDOY) THEN
            DAP = DAP + 1
            IF (GYEARDOY > 0) then
                DAG = DAG + 1
            endif
            IF (EYEARDOY > 0) then
                DAE = DAE + 1
            endif
        ENDIF
        TVI1 = LNUMSOLDESTA
        LNUMSOLDESTA = -99
        ! Leaves that present at beginning of day
        DO BR = 0, BRSTAGEINT                                                                                        !LPM 21MAR15
            DO LF = 1, LNUMSIMSTG(BR)                                                                            !LPM 23MAY2015 Modified to avoid high values of LAGETT 
                IF (isLeafAlive(node(BR,LF))) THEN             !LPM 24APR2016 Leaf age in thermal time
                    call leafAge(node(BR,LF))
                    
                    ! Accelerated senescence at base of dense leaf canopy
                    IF (node(BR,LF)%LAIByCohort > LAIXX) THEN
                            ! Increase age if deep shading at base of canopy
                            ! (Maximum accelerated ageing set in SPE file)
                            ! Accelerated ageing of lowermost active leaf
                            IF (isLeafExpanding(node(BR,LF)) .OR. isLeafActive(node(BR,LF))) THEN                                                  !LPM 28MAR15 LLIFGT was deleted 
                                call setLeafAsSenescing(node(BR,LF))
                            ENDIF

                    ENDIF
                 ! Days active
                    IF (isLeafActive(node(BR,LF))) THEN                                                  !LPM 28MAR15 LLIFGT was deleted 
                        IF (LNUMSOLDESTA < 0) THEN
                            LNUMSOLDESTA = LF
                            BROLDESTA = BR
                        ENDIF
                        node(BR,LF)%DALF = node(BR,LF)%DALF + 1.0                                                                        !EQN 364b
                        !LPM 13DEC2016 To generate a restriction for leaf active duration which should not be greater than twice the chronological time at 24 C (TRDV3(2)) 
                        IF (node(BR,LF)%DALF>(2.0*LLIFATT/(TRDV3(2)-TRDV3(1)))) THEN 
                            call setLeafAsSenescing(node(BR,LF))
                        ENDIF
                    ELSE
                        IF (didLeafStartActiveToday(node(BR,LF))) THEN                     !LPM 28MAR15 LLIFGT was deleted 
                            TVR1 = (LLIFATT-(node(BR,LF)%LAGETT-dailyGrowth()))/(dailyGrowth())
                            node(BR,LF)%DALF = node(BR,LF)%DALF + TVR1                                                                   !EQN 364c
                        ENDIF
                    ENDIF
                    ! Days senescing
                    IF (isLeafSenescing(node(BR,LF))) THEN                                                                 ! DA  If leaf is senescing
                        IF (didLeafStartSenescingToday(node(BR,LF))) THEN                                             ! DA  (and) If leaf started senescing today
                            TVR1 = (LLIFGTT+LLIFATT-(node(BR,LF)%LAGETT-dailyGrowth()))/(dailyGrowth())
                            node(BR,LF)%DSLF = node(BR,LF)%DSLF + (1.0-TVR1)                                                            ! EQN 365a
                        ELSE                                                                                                  ! DA Else, if leaf didn't started senescing today
                            IF (isLeafAlive(node(BR,LF))) THEN                                                ! DA If the leaf is still alive
                                node(BR,LF)%DSLF = node(BR,LF)%DSLF + 1.0                                                               ! EQN 365b
                            ENDIF
                        ENDIF
                        !LPM 12DEC2016 To generate a restriction for leaf senescence duration which should not be greater than twice the chronological time at 24 C (TRDV3(2)) 
                        IF (node(BR,LF)%DSLF>(2.0*LLIFSTT/(TRDV3(2)-TRDV3(1)))) THEN 
                            call setLeafAsFall(node(BR,LF))
                        ENDIF
                    ENDIF
                    IF (didLeafFallToday(node(BR,LF))) THEN                             ! DA Or, if leaf died today
                        TVR1 = ((LLIFGTT+LLIFATT+LLIFSTT)-(node(BR,LF)%LAGETT-dailyGrowth()))/(dailyGrowth())
                        node(BR,LF)%DSLF = node(BR,LF)%DSLF + TVR1                                                          ! EQN 365c
                        node(BR,LF)%LDEATHDAP = DAP                                                                    ! DA establish decease date
                    ENDIF
                
                !ELSE 
                    IF (isLeafExpanding(node(BR,LF))) THEN
                        node(BR,LF)%DGLF = node(BR,LF)%DGLF + EMRGFR
                        !LPM 13DEC2016 To generate a restriction for leaf growing duration which should not be greater than twice the chronological time at 24 C (TRDV3(2)) 
                        IF (node(BR,LF)%DGLF>(2.0*LLIFGTT/(TRDV3(2)-TRDV3(1)))) THEN 
                            call setLeafAsActive(node(BR,LF))
                        ENDIF
                    ENDIF
                ENDIF

                IF (LNUMG > 0.0 .AND. BR == BRSTAGEINT .AND. LF == LNUMSIMSTG(BR)) THEN                                            !LPM 28MAR15 Modified as part of the DO loop
                    IF (LNUMSG < LNUMX) THEN
                        node(BR,LF+1)%LAGETT = node(BR,LF+1)%LAGETT + (dailyGrowth())*AMAX1(0.0,LNUMG-LNUMNEED)/LNUMG                  !EQN 366
                        !LAGEP(BR,LF+1)=LAGEP(BR,LF+1)+AMAX1(0.0,LNUMG-LNUMNEED)                                              !EQN 367 !LPM21MAY2015 this variable is not used
                        node(BR,LF+1)%DGLF = AMAX1(0.0,LNUMG-LNUMNEED)/LNUMG                                                       !EQN 368
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
        IF (YEARDOY >= PLYEARDOY) THEN
            ! Assimilation and respiration
            CARBOC = CARBOC + AMAX1(0.0,(CARBOBEG+CARBOADJ))                                                           !EQN 424
            RESPRC = RESPRC + RTRESPADJ                                                                                !EQN 425
            RESPTC = 0.0  ! Respiration tops - not yet used
            RESPC = RESPRC + RESPTC                                                                                    !EQN 426
            LFWT = LFWT + GROLFADJ - SENLFG - SENLFGRS - LWPH                                                          !EQN 427
            LWPHC = LWPHC +  LWPH                                                                                      !EQN 428
            IF (LFWT < -1.0E-8) THEN
                WRITE(Message(1),'(A35,F4.1,A14)') 'Leaf weight less than 0! Weight of ',lfwt,' reset to zero'
                CALL WARNING(1,'CSYCA',MESSAGE)
                LFWT = 0.0
            ENDIF
            !LPM 16FEB2021 Add reallocation of assimilates from the storage roots to the leaves and stems
            RSWT = SRWT*RSUSE                                                                                          !EQN 429

            
            RSWPHC = RSWPHC +  RSWPH                                                                                   !EQN 430
            ! Reserves distribution 
            ! Max concentration in leaves increases through life cycle.
            !IF (PSTART(MSTG) > 0.0) LLRSWT = AMIN1(RSWT,LFWT*(1.0-LPEFR)*(RSCLX/100.0)*DSTAGE)                        !EQN 431 !LPM 04MAR15 MSTG TO PSX
            !IF (PSTART(MSTG) > 0.0) LPERSWT = AMIN1(RSWT-LLRSWT,LFWT*LPEFR*(RSCLX/100.0)*DSTAGE)                      !EQN 432 !LPM 04MAR15 MSTG TO PSX
            
            !LPM 21MAY2015 The reserves distribution will not be included, it needs to be reviewed
            !IF (PSTART(PSX) > 0.0) LLRSWT = AMIN1(RSWT,LFWT*(1.0-LPEFR)*(RSCLX/100.0)*DSTAGE)                        !EQN 431
            !IF (PSTART(PSX) > 0.0) LPERSWT = AMIN1(RSWT-LLRSWT,LFWT*LPEFR*(RSCLX/100.0)*DSTAGE)                      !EQN 432
            !IF (woodyWeight() > 0.0) THEN
            !    STRSWT = (RSWT-LLRSWT-LPERSWT)*STWT/(woodyWeight())                                                        !EQN 433a
            !    CRRSWT = (RSWT-LLRSWT-LPERSWT)*CRWT/(woodyWeight())                                                        !EQN 434a
            !ELSE
            !    STRSWT = (RSWT-LLRSWT-LPERSWT)                                                                         !EQN 433b
            !    CRRSWT = 0.0                                                                                           !EQN 434b
            !ENDIF
            IF (RSWT < 0.0) THEN
                IF (ABS(RSWT) > 1.0E-6) THEN
                    WRITE(Message(1),'(A30,A11,F12.9)') 'Reserves weight reset to zero.', 'Weight was ',rswt
                    CALL WARNING(1,'CSYCA',MESSAGE)
                    RSWT = 0.0
                ENDIF
            ENDIF
            RSWTX = AMAX1(RSWTX,RSWT)
            STWT = STWT + GROSTADJ - SWPH
            IF (STWT < 1.0E-06) THEN
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
            !SRWT = SRWT + GROSR + SRWTGRS + (RTWTG-RTWTGADJ+RTRESP-RTRESPADJ) ! Root N adjustment                      !EQN 447 !LPM 05JUN2105 GROSR or basic growth of storage roots will not be used
            !LPM 16FEB2021 Reduce storage root weight when used as reserve for aboveground growth after release of water stress (GROLSRS)
            SRWTG = SRWTGRS + (RTWTG-RTWTGADJ+RTRESP-RTRESPADJ) - GROLSRS
            SRWT = SRWT + SRWTG ! Root N adjustment                              !EQN 447
            !LPM 13APR2021 Estimate fresh weight of storage roots
            !DMIC represents the maximum increase of dry matter due to low temperatures
            ! To reduce dry matter content for young plants we use 1.35E-3*(3000.- DAWWP)
            !This has an effect for plants younger than 6 months reducing up to 4.05%
            !Data from ACAI trials (multiple linear regression) converting the effect of chronological age to thermal time
            !0.023/17 = 1.35E-3 = DMRD
            IF (SRWTG > 0.0 .AND. HMPC > 0.0 .AND. Tfdmc >= 0.0) THEN
                IF (DAWWP < DMAG) THEN
                    SRWTF = SRWTF + (SRWTG * (100./(HMPC+(DMIC*(1.-Tfdmc))-((DMRD/1000.)*(DMAG - DAWWP))))) 
                ELSE
                    SRWTF = SRWTF + (SRWTG * (100./(HMPC+(DMIC*(1.-Tfdmc)))))
                ENDIF
            ELSE
                SRWTF = SRWTF
            ENDIF
            MSWT = SRWTF - SRWT
            
        ENDIF
        IF (DAGERM+TTGEM*WFGE >= PGERM) THEN !LPM 23MAR2016 To consider reserves of stake after germination
            !SEEDRS = AMAX1(0.0,SEEDRS-GROLSSD-SEEDRSAVR)                                                                   !EQN 285 !LPM 23MAR2016 SEEDRSAVR subtracted in CS_Growth_Init.f90
            SEEDRS = AMAX1(0.0,SEEDRS-GROLSSD)                                                                       !EQN 285
            IF (CFLSDRSMSG /= 'Y'.AND.SEEDRS <= 0.0.AND.LNUM < 4.0) THEN
                WRITE(Message(1),'(A44,F3.1)') 'Seed reserves all used but leaf number only ',lnum
                WRITE(Message(2),'(A58)') 'For good establishment seed reserves should last to leaf 4'
                WRITE(Message(3),'(A60)') 'Maybe stick too small verify PLWT and SPRL in the .CSX file'
                CALL WARNING(3,'CSYCA',MESSAGE)
                CFLSDRSMSG = 'Y'
            ENDIF
            !SEEDUSE = SEEDUSE + GROLSSD+SEEDRSAVR  !LPM 22DEC2016 Root growth based on top growth (deleted SEEDRSAVR)                                                                         !EQN 448
            SEEDUSE = SEEDUSE + GROLSSD
            !SEEDUSER = SEEDUSER + SEEDRSAVR                                                                                !EQN 449
            SEEDUSER = SEEDUSER
            SEEDUSET = SEEDUSET + GROLSSD                                                                                  !EQN 450
            SEEDRSAV = SEEDRS
        ENDIF
        ! IF (SRNOPD > 0.0) SRWUD = SRWT/SRNOPD                                                                         !EQN 292
        
        IF ((totalWeight()) > 0.0) THEN !LPM 02OCT2019 To consider the total weight for the harvest index
            HIAD = SRWT/(totalWeight()+SEEDRS+SDCOAT)                                                                     !EQN 293
        ENDIF
        IF (RTWT > 0.0) THEN
            SHRTD = (canopyWeight()) / RTWT                                                          !EQN 294
        ENDIF
    END SUBROUTINE YCA_Integ_AgesWts
