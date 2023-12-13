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
        LAI        , CANHT       , DEPMAX      , DLAYR       , NLAYR       , RLV        & 
        )

! 2023-01-25 chp removed unused variables
!       BRSTAGE        

        USE ModuleDefs
        USE YCA_First_Trans_m
        USE YCA_Control_Leaf
        USE YCA_Control_Plant
        
        IMPLICIT NONE
        
        INTEGER NLAYR
        INTEGER :: BR                      ! Index for branch number/cohorts#          ! (From SeasInit)  
        INTEGER :: LF                      ! Loop counter leaves            #          !LPM 21MAR15 to add a leaf counter
        
        REAL    LAI        , CANHT       , DEPMAX      , DLAYR(NL)   , RLV(NL)    !, BRSTAGE     
        REAL    :: leafAreaSenesced                 !PLASTMP Leaf area senesced,temporary   cm2/p      ! (From Integrate) 
       
        !-----------------------------------------------------------------------
        !         Calculate reserve concentration
        !-----------------------------------------------------------------------
        
        IF (LFWT + woodyWeight() > 0.0) RSCD = RSWT/(canopyWeight())                                                   !EQN 451
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
                IF (plantLeafAreaLeftToSenesce() > 0.0) THEN
                    SHLAS(L) = SHLAS(L) + PLAS*(SHLA(L)-SHLAS(L))/(plantLeafAreaLeftToSenesce())                        !EQN 452
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
            
            
        ENDIF
        
        !-----------------------------------------------------------------------
        !         Update senesced and harvested leaf area
        !-----------------------------------------------------------------------
        
        SENLA = SENLA + PLAS                                                                                           !EQN 456
        SENLALITTER = SENLALITTER + PLAS * SENFR                                                                       !EQN 457
        LAPHC = LAPHC + LAPH  ! Grazed leaf area                                                                       !EQN 458
        ! Distribute senesced leaf over leaf positions and cohorts
        leafAreaSenesced = PLAS - PLASP
        IF (LNUMSG > 0 .AND. leafAreaSenesced > 0) THEN
            !DO L = 1, LNUMSG                              !LPM 28MAR15 Change to introduce cohorts
            DO BR = 0, BRSTAGEINT                                                                                        !LPM 21MAR15
                DO LF = 1, LNUMSIMSTG(BR) 
                    IF (leafAreaLeftToSenesce(node(BR,LF)) > leafAreaSenesced) THEN                                                                     ! DA If the leaf can senesce more
                        node(BR,LF)%LAPS = node(BR,LF)%LAPS + leafAreaSenesced                                                                        !EQN 459a
                        leafAreaSenesced = 0.0
                    ELSE
                        leafAreaSenesced = leafAreaSenesced - leafAreaLeftToSenesce(node(BR,LF))                                                               
                        node(BR,LF)%LAPS = node(BR,LF)%LATL3T                                                                                   !EQN 459b    ! DA The leaf area is totally senesced
                    ENDIF
                    IF (leafAreaSenesced <= 0.0) EXIT
                ENDDO
            ENDDO
        ENDIF
        ! Distribute harvested leaf over leaf positions and cohorts
        ! Leaf positions
        IF (LNUMSG > 0 .AND. LAPH > 0) THEN
            !DO L = 1, LNUMSG
            !    IF (LAP(L)-LAPS(L) > 0.0) LAPS(L) = LAPS(L) + (LAP(L)-LAPS(L)) * HAFR                                 !EQN 461
            !ENDDO
            DO BR = 0, BRSTAGEINT                                                                                        !LPM 28MAR15 Change to include cohorts
                DO LF = 1, LNUMSIMSTG(BR)
                    IF (leafAreaLeftToSenesce(node(BR,LF)) > 0.0) THEN
                        node(BR,LF)%LAPS = node(BR,LF)%LAPS + (leafAreaLeftToSenesce(node(BR,LF))) * HAFR     !EQN 461
                    ENDIF
                ENDDO
            ENDDO
        ENDIF
        
        LAPD = LAI/(PLTPOP*0.0001)
        LAIX = AMAX1(LAIX,LAI)
        
        !-----------------------------------------------------------------------
        !         Update specific leaf area
        !-----------------------------------------------------------------------
        
        SLA = -99.0
        IF (LFWT > ZERO) THEN
            SLA=(plantGreenLeafArea()) / (LFWT*(1.0-LPEFR))                                                 !EQN 465
        ENDIF
        
        !-----------------------------------------------------------------------
        !         Update leaf petiole and stem area
        !-----------------------------------------------------------------------
        
        LPEAI = (LFWT*LPEFR*LPEAW)*PLTPOP*0.0001                                                                       !EQN 466
        STAI = STAI + STAIG - STAIS                                                                                    !EQN 467
        
        SAID = STAI+LPEAI                                                                                              !EQN 468
        CAID = LAI + SAID                                                                                                    !EQN 469
        
        !-----------------------------------------------------------------------
        !         Update height
        !-----------------------------------------------------------------------
        
        CANHT = CANHT + CANHTG
        
        !-----------------------------------------------------------------------
        !         Update root depth and length
        !-----------------------------------------------------------------------
        !LPM 26MAR2016 To consider root growth from the same depth of bud growth
        !LPM 10OCT2019 To modify the initial root depth as teh planting stick depth to avoid high water stress in the top layer
        IF (SDEPTH > 0.0 .AND.RTDEP <= 0.0) RTDEP = AMAX1(0.0,SDEPTH)   
        RTDEP = AMIN1 (RTDEP+RTDEPG,DEPMAX)                                                                            !EQN 390
        DO L = 1, NLAYR
            IF (WFG > 0.0 .AND. WFG < 0.5) THEN
                RLV(L)=RTWTL(L)*((RLWR/100.)/(WFG/0.5))*PLTPOP/DLAYR(L)
            ELSE
                RLV(L)=RTWTL(L)*(RLWR/100.)*PLTPOP/DLAYR(L)   ! cm/cm3                                                            !EQN 389
            ENDIF
            IF (L == NLAYR.AND.RLV(L) > 0.0)THEN
                IF (RTSLXDATE <= 0.0) RTSLXDATE = YEARDOY
            ENDIF
        END DO
        
    END SUBROUTINE YCA_Integ_LA
        
