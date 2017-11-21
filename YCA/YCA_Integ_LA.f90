!***************************************************************************************************************************
! This is the code from the section (DYNAMIC == INTEGR) lines 5737 - 5874 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine YCA_Integ_LA calculates the reserve concentration, shoot and total leaf area. It updates senesced, harvested, green 
! leaf area, stems and petioles, plant height, and root depth and length. 
!***************************************************************************************************************************
    
    SUBROUTINE YCA_Integ_LA ( &
        CAID        , CANHT       , DEPMAX      , DLAYR       , NLAYR       , RLV        , BRSTAGE    & 
        )
        
        USE ModuleDefs
        USE YCA_First_Trans_m
        
        IMPLICIT NONE
        
        INTEGER NLAYR
        
        REAL    CAID        , CANHT       , DEPMAX      , DLAYR(NL)   , RLV(NL)    , BRSTAGE     
       
        !-----------------------------------------------------------------------
        !         Calculate reserve concentration
        !-----------------------------------------------------------------------
        
        IF (LFWT+STWT+CRWT > 0.0) RSCD = RSWT/(LFWT+STWT+CRWT+RSWT)                                                   !EQN 451
        IF (RSCD < 0.0.AND.RSCD > -1.0E-7) THEN 
            RSCD = 0.0
        ENDIF
        RSCX = AMAX1(RSCX,RSCD)
        
        !-----------------------------------------------------------------------
        !         Update shoot leaf area (Must be done before PLA updated)
        !-----------------------------------------------------------------------
        
        ! First for leaf senescence
        DO L = 1,INT(SHNUM+1)
            IF (SHNUM-FLOAT(L-1) > 0.0) THEN
                IF (PLA-SENLA > 0.0) THEN
                    SHLAS(L) = SHLAS(L) + PLAS*(SHLA(L)-SHLAS(L))/(PLA-SENLA)                        !EQN 452
                ENDIF
            ENDIF
        ENDDO
        
        !-----------------------------------------------------------------------
        !         Update produced leaf area
        !-----------------------------------------------------------------------
        
        IF (TT*EMRGFR > 0.0) THEN
            PLA = PLA + PLAGSB4                                                                                        !EQN 453
            PLAX = AMAX1(PLAX,PLA)
            !LAP(LNUMSG) = LAP(LNUMSG) + PLAGSB4                                                                        !EQN 454
            !node(BRSTAGE,LNUMSIMSTG(BRSTAGE))%LAP = node(BRSTAGE,LNUMSIMSTG(BRSTAGE))%LAP + PLAGSB4  !LPM10JUL2017 Adjust leaf area at leaf position using LATL3 (actual leaf area by cohort)
            DO L = 1,INT(SHNUM+1)
                IF (SHNUM >= 1.0.OR.SHNUM-FLOAT(L-1) > 0.0) THEN
                    SHLA(L) = SHLA(L) + SHLAGB4(L)                                                                    !LPM21MAR15 
                ENDIF
            ENDDO
            
            !IF (LCNUM < LCNUMX) THEN    !LPM 28MAR15 This section is not necessary
            !    IF (PLAGSB4 > 0.0) THEN
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
        IF (LNUMSG > 0 .AND. PLASTMP > 0) THEN
            !DO L = 1, LNUMSG                              !LPM 28MAR15 Change to introduce cohorts
            DO BR = 0, BRSTAGE                                                                                        !LPM 21MAR15
                DO LF = 1, LNUMSIMSTG(BR) 
                    IF (node(BR,LF)%LATL3T-node(BR,LF)%LAPS > PLASTMP) THEN                                                                     ! DA If the leaf can senesce more
                        node(BR,LF)%LAPS = node(BR,LF)%LAPS + PLASTMP                                                                        !EQN 459a
                        PLASTMP = 0.0
                    ELSE
                        PLASTMP = PLASTMP - (node(BR,LF)%LATL3T-node(BR,LF)%LAPS)                                                               
                        node(BR,LF)%LAPS = node(BR,LF)%LATL3T                                                                                   !EQN 459b    ! DA The leaf area is totally senesced
                    ENDIF
                    IF (PLASTMP <= 0.0) EXIT
                ENDDO
            ENDDO
            !! Cohorts  !LPM 28MAR15 This section is not necessary
            !PLASTMP2 = AMAX1(0.0,PLAS)
            !DO L = 1, LCNUM
            !    IF (LCOA(L)-LCOAS(L) > PLASTMP2) THEN
            !        LCOAS(L) = LCOAS(L) + PLASTMP2                                                                     !EQN 460a
            !        PLASTMP2 = 0.0
            !    ELSE
            !        PLASTMP2 = PLASTMP2 - (LCOA(L)-LCOAS(L))
            !        LCOAS(L) = LCOA(L)                                                                                 !EQN 460b
            !    ENDIF
            !    IF (PLASTMP2 <= 0.0) EXIT
            !ENDDO
        ENDIF
        ! Distribute harvested leaf over leaf positions and cohorts
        ! Leaf positions
        IF (LNUMSG > 0 .AND. LAPH > 0) THEN
            !DO L = 1, LNUMSG
            !    IF (LAP(L)-LAPS(L) > 0.0) LAPS(L) = LAPS(L) + (LAP(L)-LAPS(L)) * HAFR                                 !EQN 461
            !ENDDO
            DO BR = 0, BRSTAGE                                                                                        !LPM 28MAR15 Change to include cohorts
                DO LF = 1, LNUMSIMSTG(BR)
                    IF (node(BR,LF)%LATL3T-node(BR,LF)%LAPS > 0.0) THEN
                        node(BR,LF)%LAPS = node(BR,LF)%LAPS + (node(BR,LF)%LATL3T - node(BR,LF)%LAPS) * HAFR     !EQN 461
                    ENDIF
                ENDDO
            ENDDO
            ! Cohorts
            !DO L = 1, LCNUM
            !    IF (LCOA(L)-LCOAS(L) > 0.0) THEN
            !        LCOAS(L) = LCOAS(L) + (LCOA(L)-LCOAS(L)) * HAFR                                                    !EQN 462
            !    ENDIF
            !ENDDO
        ENDIF
        
        LAPD = LAI/(PLTPOP*0.0001)
        LAIX = AMAX1(LAIX,LAI)
        
        !-----------------------------------------------------------------------
        !         Update specific leaf area
        !-----------------------------------------------------------------------
        
        SLA = -99.0
        IF (LFWT > ZERO) THEN
            SLA=(PLA-SENLA-LAPHC) / (LFWT*(1.0-LPEFR))                                                 !EQN 465
        ENDIF
        
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
        
        IF (SDEPTH > 0.0 .AND.RTDEP <= 0.0) RTDEP = AMAX1(0.0,SDEPTHU)   !LPM 26MAR2016 To consider root growth from the same depth of bud growth
        RTDEP = AMIN1 (RTDEP+RTDEPG,DEPMAX)                                                                            !EQN 390
        DO L = 1, NLAYR
            RLV(L)=RTWTL(L)*RLWR*PLTPOP/DLAYR(L)   ! cm/cm3                                                            !EQN 389
            IF (L == NLAYR.AND.RLV(L) > 0.0)THEN
                IF (RTSLXDATE <= 0.0) RTSLXDATE = YEARDOY
            ENDIF
        END DO
        
    END SUBROUTINE YCA_Integ_LA
        
