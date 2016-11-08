!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.INTEGR) lines 5737 - 5874 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module CS_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine CS_Integ_LA calculates the reserve concentration, shoot and total leaf area. It updates senesced, harvested, green 
! leaf area, stems and petioles, plant height, and root depth and length. 
!***************************************************************************************************************************
    
    SUBROUTINE CS_Integ_LA ( &
        CAID        , CANHT       , DEPMAX      , DLAYR       , NLAYR       , RLV        , BRSTAGE    & 
        )
        
        USE ModuleDefs
        USE CS_First_Trans_m
        
        IMPLICIT NONE
        
        INTEGER NLAYR
        
        REAL    CAID        , CANHT       , DEPMAX      , DLAYR(NL)   , RLV(NL)    , BRSTAGE     
       
        !-----------------------------------------------------------------------
        !         Calculate reserve concentration
        !-----------------------------------------------------------------------
        
        IF (LFWT+STWT+CRWT.GT.0.0) RSCD = RSWT/(LFWT+STWT+CRWT+RSWT)                                                   !EQN 451
        IF (RSCD.LT.0.0.AND.RSCD.GT.-1.0E-7) RSCD = 0.0
        RSCX = AMAX1(RSCX,RSCD)
        
        !-----------------------------------------------------------------------
        !         Update shoot leaf area (Must be done before PLA updated)
        !-----------------------------------------------------------------------
        
        ! First for leaf senescence
        DO L = 1,INT(SHNUM+1)
            IF (SHNUM-FLOAT(L-1).GT.0.0) THEN
                IF (PLA-SENLA.GT.0.0) SHLAS(L) = SHLAS(L) + PLAS*(SHLA(L)-SHLAS(L))/(PLA-SENLA)                        !EQN 452
            ENDIF
        ENDDO
        
        !-----------------------------------------------------------------------
        !         Update produced leaf area
        !-----------------------------------------------------------------------
        
        IF (TT*EMRGFR.GT.0.0) THEN
            PLA = PLA + PLAGSB4                                                                                        !EQN 453
            PLAX = AMAX1(PLAX,PLA)
            !LAP(LNUMSG) = LAP(LNUMSG) + PLAGSB4                                                                        !EQN 454
            LAP(BRSTAGE,LNUMSIMSTG(BRSTAGE)) = LAP(BRSTAGE,LNUMSIMSTG(BRSTAGE)) + PLAGSB4 
            
            DO L = 1,INT(SHNUM+1)
                IF (SHNUM.GE.1.0.OR.SHNUM-FLOAT(L-1).GT.0.0) THEN
                    SHLA(L) = SHLA(L) + SHLAGB4(L)                                                                    !LPM21MAR15 
                ENDIF
            ENDDO
            
            !IF (LCNUM.LT.LCNUMX) THEN    !LPM 28MAR15 This section is not necessary
            !    IF (PLAGSB4.GT.0.0) THEN
            !        LCNUM = LCNUM+1
            !        LCOA(LCNUM) = PLAGSB4                                                                              !EQN 455a
            !    ENDIF
            !ELSE
            !    LCOA(LCNUM) = LCOA(LCNUM) + PLAGSB4                                                                    !EQN 455b
            !ENDIF
            
        ENDIF
        
        !-----------------------------------------------------------------------
        !         Update senesced and harvested leaf area
        !-----------------------------------------------------------------------
        
        SENLA = SENLA + PLAS                                                                                           !EQN 456
        SENLALITTER = SENLALITTER + PLAS * SENFR                                                                       !EQN 457
        LAPHC = LAPHC + LAPH  ! Grazed leaf area                                                                       !EQN 458
        ! Distribute senesced leaf over leaf positions and cohorts
        PLASTMP = PLAS - PLASP
        IF (LNUMSG.GT.0 .AND. PLASTMP.GT.0) THEN
            !DO L = 1, LNUMSG                              !LPM 28MAR15 Change to introduce cohorts
            DO BR = 0, BRSTAGE                                                                                        !LPM 21MAR15
                DO LF = 1, LNUMSIMSTG(BR) 
                    IF (LAP(BR,LF)-LAPS(BR,LF).GT.PLASTMP) THEN
                        LAPS(BR,LF) = LAPS(BR,LF) + PLASTMP                                                                        !EQN 459a
                        PLASTMP = 0.0
                    ELSE
                        PLASTMP = PLASTMP - (LAP(BR,LF)-LAPS(BR,LF))
                        LAPS(BR,LF) = LAP(BR,LF)                                                                                   !EQN 459b
                    ENDIF
                    IF (PLASTMP.LE.0.0) EXIT
                ENDDO
            ENDDO
            !! Cohorts  !LPM 28MAR15 This section is not necessary
            !PLASTMP2 = AMAX1(0.0,PLAS)
            !DO L = 1, LCNUM
            !    IF (LCOA(L)-LCOAS(L).GT.PLASTMP2) THEN
            !        LCOAS(L) = LCOAS(L) + PLASTMP2                                                                     !EQN 460a
            !        PLASTMP2 = 0.0
            !    ELSE
            !        PLASTMP2 = PLASTMP2 - (LCOA(L)-LCOAS(L))
            !        LCOAS(L) = LCOA(L)                                                                                 !EQN 460b
            !    ENDIF
            !    IF (PLASTMP2.LE.0.0) EXIT
            !ENDDO
        ENDIF
        ! Distribute harvested leaf over leaf positions and cohorts
        ! Leaf positions
        IF (LNUMSG.GT.0 .AND. LAPH.GT.0) THEN
            !DO L = 1, LNUMSG
            !    IF (LAP(L)-LAPS(L).GT.0.0) LAPS(L) = LAPS(L) + (LAP(L)-LAPS(L)) * HAFR                                 !EQN 461
            !ENDDO
            DO BR = 0, BRSTAGE                                                                                        !LPM 28MAR15 Change to include cohorts
                DO LF = 1, LNUMSIMSTG(BR)
                    IF (LAP(BR,LF)-LAPS(BR,LF).GT.0.0) LAPS(BR,LF) = LAPS(BR,LF) + (LAP(BR,LF)-LAPS(BR,LF)) * HAFR     !EQN 461
                ENDDO
            ENDDO
            ! Cohorts
            !DO L = 1, LCNUM
            !    IF (LCOA(L)-LCOAS(L).GT.0.0) THEN
            !        LCOAS(L) = LCOAS(L) + (LCOA(L)-LCOAS(L)) * HAFR                                                    !EQN 462
            !    ENDIF
            !ENDDO
        ENDIF
        
        !-----------------------------------------------------------------------
        !         Update (green) leaf area                            
        !-----------------------------------------------------------------------            
        !LAPD = AMAX1(0.0,(PLA-SENLA-LAPHC))                                      ! DA LPM 02NOV2016 Removed                                      !EQN 463
        !LAI = AMAX1(0.0,(PLA-SENLA-LAPHC)*PLTPOP*0.0001)                         ! DA LPM 02NOV2016 Removed                                      !EQN 346
    
        ! DA LAI Calculation, this is not considering PLASI (injury) nor PLASS (stress) nor LAPHC (leaf harvest)

        !DO Bcount=0,BRSTAGE
        !    BR= BRSTAGE - Bcount
        !    DO Lcount=0,LNUMSIMSTG(BR)-1
        !        LF=LNUMSIMSTG(BR)-Lcount
        !        IF (LAGETT(BR,LF) < LLIFGTT+LLIFATT+LLIFSTT ) THEN
        !            LAI=AMAX1(LAI, LAIByCohort(BR,LF))                  ! DA LAI is the biggest LAIByCohort
        !        ENDIF
        !    ENDDO
        !ENDDO
        LAPD = LAI/(PLTPOP*0.0001)
        LAIX = AMAX1(LAIX,LAI)
        
        !-----------------------------------------------------------------------
        !         Update specific leaf area
        !-----------------------------------------------------------------------
        
        SLA = -99.0
        IF (LFWT.GT.1.0E-6) SLA=(PLA-SENLA-LAPHC) / (LFWT*(1.0-LPEFR))                                                 !EQN 465
        
        !-----------------------------------------------------------------------
        !         Update leaf petiole and stem area
        !-----------------------------------------------------------------------
        
        LPEAI = (LFWT*LPEFR*LPEAW)*PLTPOP*0.0001                                                                       !EQN 466
        STAI = STAI + STAIG - STAIS                                                                                    !EQN 467
        
        SAID = STAI+LPEAI                                                                                              !EQN 468
        CAID = LAI + SAID                                                                                              !EQN 469
        
        !-----------------------------------------------------------------------
        !         Update height
        !-----------------------------------------------------------------------
        
        CANHT = CANHT + CANHTG
        
        !-----------------------------------------------------------------------
        !         Update root depth and length
        !-----------------------------------------------------------------------
        
        IF (SDEPTH.GT.0.0 .AND.RTDEP.LE.0.0) RTDEP = AMAX1(0.0,SDEPTHU)   !LPM 26MAR2016 To consider root growth from the same depth of bud growth
        RTDEP = AMIN1 (RTDEP+RTDEPG,DEPMAX)                                                                            !EQN 390
        DO L = 1, NLAYR
            RLV(L)=RTWTL(L)*RLWR*PLTPOP/DLAYR(L)   ! cm/cm3                                                            !EQN 389
            IF (L.EQ.NLAYR.AND.RLV(L).GT.0.0)THEN
                IF (RTSLXDATE.LE.0.0) RTSLXDATE = YEARDOY
            ENDIF
        END DO
        
    END SUBROUTINE CS_Integ_LA
        
