!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.RATE) lines 5351 - 5530 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module CS_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine CS_Growth_Distribute calculates growth of reserves and plant height, and soil water.
!***************************************************************************************************************************
    
     SUBROUTINE CS_Growth_Distribute ( &
        DLAYR       , ISWNIT      , ISWWAT      , LL          , NH4LEFT     , NLAYR       , NO3LEFT     , RLV         , &
        SENCALG     , SENLALG     , SENNALG     , SHF         , SW          & 
        )
    
        USE ModuleDefs
        USE CS_First_Trans_m
    
        IMPLICIT NONE
        
        CHARACTER(LEN=1) ISWNIT      , ISWWAT      
        INTEGER NLAYR       
        REAL    DLAYR(NL)   , LL(NL)      , NH4LEFT(NL) , NO3LEFT(NL) , RLV(NL)     , SENCALG(0:NL)
        REAL    SENLALG(0:NL)             , SENNALG(0:NL)             , SHF(NL)     , SW(NL)

        INTEGER CSIDLAYR                                                                     ! Integer function call.

        !-----------------------------------------------------------------------
        !           Reserves growth
        !-----------------------------------------------------------------------

        !GRORS = CARBOT+GROLSSD+GROLSRT+SENLFGRS-GROLFADJ-GROSTADJ-GROCRADJ-GROSR                                       !EQN 309 !LPM 05JUN2105 GROSR or basic growth of storage roots will not be used
        GRORS = CARBOT+GROLSSD+GROLSRT+SENLFGRS-GROLFADJ-GROSTADJ-GROCRADJ
        IF(GRORS.LT.0.0.AND.GRORS.GT.-1.0E-07) GRORS = 0.0

        ! Reserves to STORAGE ROOT if conc too great (overflow!)
        SRWTGRS = 0.0
        ! Determine potential new concentration
        IF (LFWT+GROLFADJ+STWT+CRWT+GROSTADJ+GROCRADJ.GT.0.0) TVR1 = (RSWT+GRORS)/((LFWT+GROLFADJ-SENLFG-SENLFGRS)+ &  !EQN 310
        (STWT+GROSTADJ+CRWT+GROCRADJ)+(RSWT+GRORS))
        IF(TVR1.LT.0.0.AND.TVR1.GT.-1.0E-07) TVR1 = 0.0
        IF (TVR1.GT.RSPCO/100.0) THEN   ! If potential>standard 
            TVR2 = RSWT+GRORS             ! What rswt could be                                                         !EQN 311
            TVR3 =   ((RSPCO/100.0)*(LFWT+GROLFADJ-SENLFG-SENLFGRS+STWT+CRWT+GROSTADJ+GROCRADJ))/(1.0-(RSPCO/100.0))! What rswt should be     !EQN 312
            SRWTGRS = (TVR2 - TVR3)                                                                                    !EQN 313
            ! Determine FINAL new concentration
            IF (LFWT+GROLFADJ+STWT+CRWT+GROSTADJ+GROCRADJ.GT.0.0) TVR5 = (RSWT+GRORS-SRWTGRS)/((LFWT+GROLFADJ-SENLFG-SENLFGRS)+ &             !EQN 314
                (STWT+GROSTADJ+CRWT+GROCRADJ)+(RSWT+GRORS-SRWTGRS))
        ENDIF
        
        IF(SRWTGRS.GT.0.0.AND.SRNOPD.LE.0.0) THEN                                  !LPM 05JUN2015 SRNOPD Defined when SRWT >0
            SRNOPD = INT(SRNOW*((LFWT+STWT+CRWT+RSWT)))                                                                !EQN 291
        ENDIF
                
        !-----------------------------------------------------------------------
        !           Height growth
        !-----------------------------------------------------------------------

        CANHTG = 0.0
        CANHTG = SERX*DU                                                                                               !EQN 316

        !-----------------------------------------------------------------------
        !           Root depth and length growth, and distribution
        !-----------------------------------------------------------------------

        RTWTGL = 0.0
        RTWTSL = 0.0
        RTWTUL = 0.0
        RTNSL = 0.0

        IF (GERMFR.GT.0.0.OR.GESTAGE.GE.0.5) THEN
    
            ! Establish water factor for root depth growth
            IF (ISWWAT.NE.'N') THEN
                LRTIP = CSIDLAYR (NLAYR, DLAYR, RTDEP) ! Root tip layer
                IF (LRTIP.GT.1) THEN
                    SWPRTIP = SWP(LRTIP)                                                                               !EQN 392a
                ELSE
                    SWPRTIP = AMIN1(SWP(2),(SWP(2)-((DLAYR(1)-RTDEP)/DLAYR(1))*(SWP(2)-SWP(1))))                       !EQN 392b
                ENDIF
                WFRG = 1.0
                IF (WFRTG.GT.0.0)WFRG = AMAX1(0.0,AMIN1(1.0,(SWPRTIP/WFRTG)))                                          !EQN 393
            ELSE
                WFRG = 1.0
            ENDIF
    
            ! Root depth growth
            RTDEPG = 0.0
            IF (ISWWAT.NE.'N') THEN
                ! LAH Note reduced effect of SHF, AND no acceleration
                RTDEPG = TT*RDGS/STDAY*GERMFR* SQRT(AMAX1(0.3,SHF(LRTIP))) * WFRG                                      !EQN 391a
            ELSE
                RTDEPG = TT*RDGS/STDAY*GERMFR                                                                          !EQN 391b
            ENDIF
            L = 0
            CUMDEP = 0.0
            RTDEPTMP = RTDEP+RTDEPG                                                                                    !EQN 402
            DO WHILE ((CUMDEP.LE.RTDEPTMP) .AND. (L.LT.NLAYR))
                L = L + 1
                CUMDEP = CUMDEP + DLAYR(L)                                                                             !EQN 401
                ! LAH Limit on WFRG. 0 WFRG (when 1 layer) -> 0 TRLDF.
                IF (ISWWAT.NE.'N'.AND.WFRTG.GT.0.0) THEN
                    WFRG = AMIN1(1.0,AMAX1(0.1,SWP(L)/WFRTG))                                                          !EQN 394
                ELSE
                    WFRG = 1.0
                ENDIF
                IF (ISWNIT.NE.'N'.AND.NCRG.GT.0.0) THEN
                    NFRG = AMIN1(1.0,AMAX1(0.1,(NO3LEFT(L)+NH4LEFT(L))/NCRG))                                          !EQN 168
                ELSE
                    NFRG = 1.0
                ENDIF 
                ! LAH Tried to use AMAX1 here because layer may have 
                ! lots H20,no N,or inverse, and therefore need roots
                ! But with KSAS8101,AMAX1 lowered yield. Return to AMIN1
                !RLDF(L) = AMAX1(WFRG,NFRG)*SHF(L)*DLAYR(L)
                RLDF(L) = AMIN1(WFRG,NFRG)*SHF(L)*DLAYR(L)                                                             !EQN 403
            END DO
            IF (L.GT.0.AND.CUMDEP.GT.RTDEPTMP) RLDF(L) = RLDF(L)*(1.0-((CUMDEP-RTDEPTMP)/DLAYR(L)))
            NLAYRROOT = L
            ! Root senescence
            SENRTG = 0.0
            DO L = 1, NLAYRROOT
                RTWTSL(L) = RTWTL(L)*(RSEN/100.0)*TT/STDAY                                                             !EQN 395
                ! LAH Temperature effect above is not from soil temp
                IF (RTWT.GT.0.0) RTWTUL(L) = RTWTL(L)*GROLSRT/RTWT                                                     !EQN 396
                SENRTG = SENRTG + RTWTSL(L)                                                                            !EQN 397
                IF (ISWNIT.NE.'N') THEN
                    RTNSL(L) = RTWTSL(L)*RANC                                                                          !EQN 398
                ELSE
                    RTNSL(L) = 0.0
                ENDIF  
            ENDDO
    
            ! Root weight growth by layer
            TRLDF = 0.0
            DO  L = 1, NLAYRROOT
                TRLDF = TRLDF + RLDF(L)
            END DO
            IF (TRLDF.GT.0.0) THEN
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
            IF (RLV(L).GT.0.0) THEN
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
        SENWALG(0) = SENTOPLITTERG * PLTPOP*10.0                                                                       !EQN 406
        SENCALG(0) = SENWALG(0) * 0.4                                                                                  !EQN 407
        SENLALG(0) = (SENLFG*LLIGP/100) * PLTPOP*10.0                                                                  !EQN 408
        SENNALG(0) = SENNLFG * SENFR * PLTPOP*10.0                                                                     !EQN 409
        ! Root senescence
        DO L = 1, NLAYR
            SENWALG(L) = RTWTSL(L) * PLTPOP*10.0                                                                       !EQN 410
            SENNALG(L) = RTNSL(L) * PLTPOP*10.0                                                                        !EQN 411
            SENCALG(L) = SENWALG(L) * 0.4                                                                              !EQN 412
            SENLALG(L) = SENWALG(L) * RLIGP/100.0                                                                      !EQN 413
            SENWAGS = SENWAGS + SENWALG(L)
            SENCAGS = SENCAGS + SENCALG(L)
            SENLAGS = SENLAGS + SENLALG(L)
            SENNAGS = SENNAGS + SENNALG(L)
        ENDDO

        IF (ESTABLISHED.NE.'Y'.AND.SHRTD.GT.2.0) ESTABLISHED = 'Y' 
        
    END SUBROUTINE CS_Growth_Distribute