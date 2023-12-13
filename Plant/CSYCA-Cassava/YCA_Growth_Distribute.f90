!***************************************************************************************************************************
! This is the code from the section (DYNAMIC == RATE) lines 5351 - 5530 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine YCA_Growth_Distribute calculates growth of reserves and plant height, and soil water.
!***************************************************************************************************************************
    
     SUBROUTINE YCA_Growth_Distribute ( &
        DLAYR       , ISWNIT      , ISWWAT      , LL          , NH4LEFT     , NLAYR       , NO3LEFT     , RLV         , &
        SENCALG     , SENLALG     , SENNALG     , SHF         , SW          , BRSTAGE     & 
        )
    
        USE ModuleDefs
        USE YCA_First_Trans_m
        USE YCA_Control_Plant
        USE YCA_Control_Leaf
    
        IMPLICIT NONE
        EXTERNAL CSIDLAYR
        
        CHARACTER(LEN=1) ISWNIT      , ISWWAT      
        INTEGER NLAYR       
        REAL    DLAYR(NL)   , LL(NL)      , NH4LEFT(NL) , NO3LEFT(NL) , RLV(NL)     , SENCALG(0:NL), BRSTAGE
        REAL    SENLALG(0:NL)             , SENNALG(0:NL)             , SHF(NL)     , SW(NL)  
        REAL    :: RTDEPTMP                ! Root depth,temporary value     cm/d       ! (From Growth)    

        INTEGER CSIDLAYR                                                                     ! Integer function call.
        INTEGER :: BR                      ! Index for branch number/cohorts#          ! (From SeasInit)  
        INTEGER :: LF                      ! Loop counter leaves            #          

        !-----------------------------------------------------------------------
        !           Reserves growth
        !-----------------------------------------------------------------------

        !GRORS = CARBOT+GROLSSD+GROLSRT+SENLFGRS-GROLFADJ-GROSTADJ-GROCRADJ-GROSR                                       !EQN 309 !LPM 05JUN2105 GROSR or basic growth of storage roots will not be used
        GRORS = CARBOT+GROLSSD+GROLSRT+SENLFGRS+GROLSRS-GROLFADJ-GROSTADJ-GROCRADJ                                      !LPM 05OCT2015 Added GROLSRS to avoid negative values of reserves
        IF(GRORS < 0.0.AND.GRORS > -1.0E-07) GRORS = 0.0

        ! Reserves to STORAGE ROOT if conc too great (overflow!)
        SRWTGRS = 0.0
        !LPM 14SEP2020 initialize temporary variable
        TVR1 = 0.0 
        ! Determine potential new concentration
        IF (LFWT+GROLFADJ+STWT+GROSTADJ+CRWT+GROCRADJ > 0.0) THEN
            TVR1 = GRORS/((LFWT+GROLFADJ-leafTotalSenescedWeight()) + (STWT+GROSTADJ+CRWT+GROCRADJ)+GRORS)  !EQN 310
        ENDIF
        IF(TVR1 < 0.0.AND.TVR1 > -1.0E-07) THEN
            TVR1 = 0.0
        END IF
        !LPM 15DEC2020 Keep SRWTGRS instead of SRWTGRSADJ to consider additional CHO going to the roots when N is the most 
        !restrictive factor (diluted N in storage roots)
        SRWTGRS = GRORS
        ! Determine FINAL new concentration
        IF (LFWT+GROLFADJ+STWT+CRWT+GROSTADJ+GROCRADJ > 0.0) TVR5 = (GRORS-SRWTGRS)/((LFWT+GROLFADJ-leafTotalSenescedWeight())+ &             !EQN 314
            (STWT+GROSTADJ+CRWT+GROCRADJ)+(GRORS-SRWTGRS))

        
                
        !-----------------------------------------------------------------------
        !           Height growth
        !-----------------------------------------------------------------------
        CANHTG = 0.0
        IF(GROSTP > 0.0) THEN
            !LPM06JUL2017 It is assumed a branching angle of 60 from the vertical line (cos(60)=0.5)
            !LPM 15JUL2020 Adjust canopy height to m instead of cm and use NODLT instead of SESR 
            DO BR = 0, BRSTAGEINT                                                                                        !LPM 21MAR15
                DO LF = 1, LNUMSIMSTG(BR)
                    IF (isLeafExpanding(node(BR,LF))) THEN
                        IF(BRSTAGE>=1.0) THEN
                            CANHTG = CANHTG + MAX(0.0,NODLT*(dailyGrowth()/LLIFGTT)*AMIN1(WFG,node(0,0)%NFLF2)*0.5)/100.0
                        ELSE
                            CANHTG = CANHTG + MAX(0.0,NODLT*(dailyGrowth()/LLIFGTT)*AMIN1(WFG,node(0,0)%NFLF2))/100.0
                        ENDIF
                    ENDIF
                ENDDO
            ENDDO
        ELSE
            CANHTG = 0.0
        !CANHTG = SERX*DU !LPM 06JUL2017 CANHTG modified to avoid fixed maximum height HTSTD                                                                                                !EQN 316
        ENDIF
        !-----------------------------------------------------------------------
        !           Root depth and length growth, and distribution
        !-----------------------------------------------------------------------

        RTWTGL = 0.0
        RTWTSL = 0.0
        RTWTUL = 0.0
        RTNSL = 0.0

        !IF (GERMFR > 0.0.OR.GESTAGE >= 0.5) THEN !LPM 21MAR2016 To separate germination and emergence
        IF (GERMFR > 0.0.OR.GESTAGE >= 1.0) THEN
    
            ! Establish water factor for root depth growth
            IF (ISWWAT /= 'N') THEN
                LRTIP = CSIDLAYR (NLAYR, DLAYR, RTDEP) ! Root tip layer
                IF (LRTIP > 1) THEN
                    SWPRTIP = SWP(LRTIP)                                                                               !EQN 392a
                ELSE
                    SWPRTIP = AMIN1(SWP(2),(SWP(2)-((DLAYR(1)-RTDEP)/DLAYR(1))*(SWP(2)-SWP(1))))                       !EQN 392b
                ENDIF
                WFRG = 1.0
                IF (WFRTG > 0.0)WFRG = AMAX1(0.0,AMIN1(1.0,(SWPRTIP/WFRTG)))                                          !EQN 393
            ELSE
                WFRG = 1.0
            ENDIF
    
            ! Root depth growth
            RTDEPG = 0.0
            IF (STDAY*GERMFR > 0) THEN
                IF (ISWWAT /='N') THEN
                    RTDEPG = TTGEM*RDGS/STDAY*GERMFR* SQRT(AMAX1(0.3,SHF(LRTIP))) * WFRG                                      !EQN 391a
                ELSE
                    RTDEPG = TTGEM*RDGS/STDAY*GERMFR                                                                          !EQN 391b
                ENDIF
            ENDIF
            
            !------------------------------!
            !     Root weight by layer     !
            !------------------------------!
            L = 0
            CUMDEP = 0.0
            RTDEPTMP = RTDEP+RTDEPG                                                                                    !EQN 402
            DO WHILE ((CUMDEP <= RTDEPTMP) .AND. (L < NLAYR))
                L = L + 1
                CUMDEP = CUMDEP + DLAYR(L)                                                                             !EQN 401
                ! LAH Limit on WFRG. 0 WFRG (when 1 layer) -> 0 TRLDF.
                IF (ISWWAT /='N'.AND. WFRTG > 0.0) THEN
                    WFRG = AMIN1(1.0,AMAX1(0.1,SWP(L)/WFRTG))                                                          !EQN 394
                ELSE
                    WFRG = 1.0
                ENDIF
                IF (ISWNIT/='N'.AND.NCRG > 0.0) THEN
                    NFRG = AMIN1(1.0,AMAX1(0.1,(NO3LEFT(L)+NH4LEFT(L))/NCRG))                                          !EQN 168
                ELSE
                    NFRG = 1.0
                ENDIF 
                ! LAH Tried to use AMAX1 here because layer may have 
                ! lots H20,no N,or inverse, and therefore need roots
                ! But with KSAS8101,AMAX1 lowered yield. Return to AMIN1
                RLDF(L) = AMIN1(WFRG,NFRG)*SHF(L)*DLAYR(L)                                                             !EQN 403
            END DO
            IF (L > 0 .AND. CUMDEP > RTDEPTMP .AND. DLAYR(L) > 0.0) THEN
                RLDF(L) = RLDF(L)*(1.0-((CUMDEP-RTDEPTMP)/DLAYR(L)))
            ENDIF
            NLAYRROOT = L
            
            ! Root senescence
            SENRTG = 0.0
            IF(STDAY /= 0) THEN
                DO L = 1, NLAYRROOT
                    RTWTSL(L) = RTWTL(L)*(RSEN/100.0)*TTGEM/STDAY                                                             !EQN 395
                    ! LAH Temperature effect above is not from soil temp
                    !LPM 19DEC2016 The model is considering now the soil temp (TTGEM)
                    IF (RTWT > 0.0) THEN
                        RTWTUL(L) = RTWTL(L)*GROLSRT/RTWT                                                     !EQN 396
                    ENDIF
                    SENRTG = SENRTG + RTWTSL(L)                                                                            !EQN 397
                    IF (ISWNIT /= 'N') THEN
                        RTNSL(L) = RTWTSL(L)*RANC                                                                          !EQN 398
                    ELSE
                        RTNSL(L) = 0.0
                    ENDIF  
                ENDDO
            ENDIF
    
            ! Root weight growth by layer
            TRLDF = 0.0
            DO  L = 1, NLAYRROOT
                TRLDF = TRLDF + RLDF(L)
            END DO
            IF (TRLDF > 0.0) THEN
                DO  L = 1, NLAYRROOT
                    RTWTGL(L) = (RLDF(L)/TRLDF)*(RTWTGADJ)                                                             !EQN 400
                END DO
            ENDIF
        ENDIF

        !-----------------------------------------------------------------------
        !           Water in profile and rootzone
        !-----------------------------------------------------------------------
            
        AH2OPROFILE = 0.0
        H2OPROFILE = 0.0
        AH2OROOTZONE = 0.0
        H2OROOTZONE = 0.0
        DO L = 1, NLAYR
            AH2OPROFILE = AH2OPROFILE+((SW(L)-LL(L))*DLAYR(L))*10.                                                     !EQN 404              
            H2OPROFILE = H2OPROFILE + SW(L)*DLAYR(L)*10.0                                                              !EQN 405
            IF (RLV(L) > 0.0) THEN
                AH2OROOTZONE=AH2OROOTZONE+((SW(L)-LL(L))*DLAYR(L))*10.
                H2OROOTZONE = H2OROOTZONE+SW(L)*DLAYR(L)*10.
            ENDIF
        END DO

        !-----------------------------------------------------------------------
        !           Rate variables expressed on an area basis
        !-----------------------------------------------------------------------

        ! C assimilation
        ! Senesced material added to litter or soil
        SENWALG = 0.0
        SENNALG = 0.0
        SENCALG = 0.0
        SENLALG = 0.0
        SENWAGS = 0.0
        SENCAGS = 0.0
        SENLAGS = 0.0
        SENNAGS = 0.0
        SENWALG(0) = SENTOPLITTERG * plantPopulation()                                                                       !EQN 406
        SENCALG(0) = SENWALG(0) * 0.4                                                                                  !EQN 407
        SENLALG(0) = (SENLFG*LLIGP/100) * plantPopulation()                                                                  !EQN 408
        SENNALG(0) = SENNLFG * SENFR * plantPopulation()                                                                     !EQN 409
        ! Root senescence
        DO L = 1, NLAYR
            SENWALG(L) = RTWTSL(L) * plantPopulation()                                                                       !EQN 410
            SENNALG(L) = RTNSL(L) * plantPopulation()                                                                        !EQN 411
            SENCALG(L) = SENWALG(L) * 0.4                                                                              !EQN 412
            SENLALG(L) = SENWALG(L) * RLIGP/100.0                                                                      !EQN 413
            SENWAGS = SENWAGS + SENWALG(L)
            SENCAGS = SENCAGS + SENCALG(L)
            SENLAGS = SENLAGS + SENLALG(L)
            SENNAGS = SENNAGS + SENNALG(L)
        ENDDO

        IF (ESTABLISHED /= 'Y'.AND.SHRTD > 2.0) ESTABLISHED = 'Y' 
        
    END SUBROUTINE YCA_Growth_Distribute