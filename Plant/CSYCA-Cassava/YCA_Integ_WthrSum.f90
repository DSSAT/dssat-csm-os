!***************************************************************************************************************************
! This is the code from the section (DYNAMIC == INTEGR) lines 6419 - 6541 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine YCA_Integ_WthrSum calculates summaries of weather and soil variables and par utilization. 
!***************************************************************************************************************************
    
    SUBROUTINE YCA_Integ_WthrSum ( &
        CO2         , DOY         , DRAIN       , IRRAMT      , RAIN        , RUNOFF      , SRAD        , TLCHD       , &
        TMAX        , TMIN        , TNIMBSOM    , TNOXD       , TOMINFOM    , TOMINSOM    , TOMINSOM1   , TOMINSOM2   , &
        TOMINSOM3   , YEAR        , ISWWAT      & 
        )
        
        USE YCA_First_Trans_m
        USE YCA_Control_Plant
        
        IMPLICIT NONE
        EXTERNAL CALENDAR
        
        INTEGER DOY         , YEAR 
        
        REAL    CO2         , DRAIN       , IRRAMT      , RAIN        , RUNOFF      , SRAD        , TLCHD       , TMAX          
        REAL    TMIN        , TNIMBSOM    , TNOXD       , TOMINFOM    , TOMINSOM    , TOMINSOM1   , TOMINSOM2   , TOMINSOM3 
    
        CHARACTER(LEN=1) ISWWAT 
        !-----------------------------------------------------------------------
        !         Calculate weather and soil summary variables
        !-----------------------------------------------------------------------
                
        ! Cumulatives
        !LPM 24MAR2016 TTCUM estimated after germination, before is DAGERM and GEUCUM
        IF (GERMFR > 0.0) THEN
            IF (DAE > 0.0) THEN
                TTCUM = TTCUM + TT
            ELSE
                TTCUM = TTCUM + TTGEM
            ENDIF
        ELSE
            TTCUM = 0.0
        ENDIF
        
        
        IF (ISWWAT == 'Y') THEN
            DAWWP = DAWWP + (TT*WFG) !LPM 31JUL2015 Added to have a new clock with water stress
        ELSE
            DAWWP = TTCUM
        ENDIF
        IF (DAWWP > 900.0) THEN
            TTCUMLS = TTCUMLS + TTlfsize   ! LPM 12JUL2015 added to consider a different optimum temperature for potential leaf size
            IF (ISWWAT == 'Y') THEN
                DALS = DALS + (TTlfsize*WFG) !LPM 24APR2015 Added to have a new clock with water stress
            ELSE
                DALS = TTCUMLS
            ENDIF
        ENDIF
        RAINC = RAINC + RAIN
        DRAINC = DRAINC + DRAIN
        RUNOFFC = RUNOFFC + RUNOFF
        IRRAMTC = IRRAMTC + IRRAMT
        SRADC = SRADC + SRAD
        PARMJC = PARMJC + PARMJFAC*SRAD
        PARMJIC = PARMJIC + PARMJFAC*SRAD*PARI + PARMJIADJ
        TOMINC = TOMINC + TOMIN
        TOFIXC = TOFIXC + TNIMBSOM
        TOMINFOMC = TOMINFOMC + TOMINFOM
        TOMINSOMC = TOMINSOMC + TOMINSOM
        IF (TOMINSOM1 >= 0.0) THEN
            TOMINSOM1C = TOMINSOM1C + TOMINSOM1
            TOMINSOM2C = TOMINSOM2C + TOMINSOM2
            TOMINSOM3C = TOMINSOM3C + TOMINSOM3
        ELSE
            TOMINSOM1C = -99.0
            TOMINSOM2C = -99.0
            TOMINSOM3C = -99.0
        ENDIF
        TLCHC = TLCHC + TLCHD
        TNOXC = TNOXC + TNOXD
        ! Extremes
        TMAXX = AMAX1(TMAXX,TMAX)
        TMINN = AMIN1(TMINN,TMIN)
        CO2MAX = AMAX1(CO2MAX,CO2)
                
        ! Growing season means
                
        TMEANNUM = TMEANNUM + 1
        TMEANSUM = TMEANSUM + TMEAN
                
        ! 20-day means
        SRAD20S = 0.0
        TMEAN20S = 0.0
        STRESS20S = 0.0
        STRESS20NS = 0.0
        STRESS20WS = 0.0
        TT20S = 0.0
        DO L = 20,2,-1
            SRADD(L) = SRADD(L-1)
            SRAD20S = SRAD20S + SRADD(L)
            TMEAND(L) = TMEAND(L-1)
            TMEAN20S = TMEAN20S + TMEAND(L)
            STRESS(L) = STRESS(L-1)
            STRESSN(L) = STRESSN(L-1)
            STRESSW(L) = STRESSW(L-1)
            STRESS20S = STRESS20S + STRESS(L)
            STRESS20NS = STRESS20NS + STRESSN(L)
            STRESS20WS = STRESS20WS + STRESSW(L)
            TTD(L) = TTD(L-1)
            TT20S = TT20S + TTD(L)
            WUPRD(L) = WUPRD(L-1)
        ENDDO
        SRADD(1) = SRAD
        SRAD20S = SRAD20S + SRAD
        TMEAND(1) = TMEAN
        TMEAN20S = TMEAN20S + TMEAND(1)
        STRESS(1) = AMIN1(WFG,NFG)
        STRESSN(1) = NFG
        STRESSW(1) = WFG
        STRESS20S = STRESS20S + STRESS(1)
        STRESS20NS = STRESS20NS + STRESSN(1)
        STRESS20WS = STRESS20WS + STRESSW(1)
        TTD(1) = TT
        TT20S = TT20S + TTD(1)
        WUPRD(1) = AMAX1(0.0,AMIN1(10.0,WUPR))
        IF (TMEANNUM >= 20) THEN
            IF (TMEANNUM <= 20) TMEAN20P = TMEAN20S/20.0
            SRAD20 = SRAD20S/20.0
            TMEAN20 = TMEAN20S/20.0
            TT20 = TT20S/20.0
            STRESS20 = STRESS20S/20.0
            STRESS20N = STRESS20NS/20.0
            STRESS20W = STRESS20WS/20.0
        ELSE
            SRAD20 = 0.0
            IF (TMEANNUM <= 1) THEN       !CHP
              TT20 = TT20S                !CHP
            ELSE                          !CHP
              TT20 = TT20S/(TMEANNUM)     ! 
            ENDIF                         !CHP
            TMEAN20 = 0.0
            STRESS20 = 0.0
            STRESS20N = 0.0
            STRESS20W = 1.0
        ENDIF
        
        IF (STRESS20W < 0.5 .AND. (SUM(STRESSW(1:5))/5.0) > 0.5) THEN
            WFGREA = 1 + (SUM(STRESSW(1:5))/5.0)-(SUM(STRESSW(6:20))/15.0) 
        ELSE
            WFGREA = 1
        ENDIF
            
        ! Monthly means
        CALL Calendar (year,doy,dom,month)
        IF (DOM > 1) THEN
            TMAXSUM = TMAXSUM + TMAX
            TMINSUM = TMINSUM + TMIN
            DAYSUM = DAYSUM + 1.0
        ELSE
            IF (DAYSUM > 0) THEN
                IF (TMAXM < TMAXSUM/DAYSUM) TMAXM=TMAXSUM/DAYSUM
                IF (TMINM > TMINSUM/DAYSUM) TMINM=TMINSUM/DAYSUM
            ENDIF
            TMAXSUM = TMAX
            TMINSUM = TMIN
            DAYSUM =  1
        ENDIF
                
        !-----------------------------------------------------------------------
        !         Calculate PAR utilization efficiencies
        !-----------------------------------------------------------------------
                
        IF (PARMJC > 0.0) THEN
            PARUEC = AMAX1(0.0,(totalWeight()+SENTOPLITTER+SENROOT-SEEDUSE)* PLTPOP / PARMJC)
        ENDIF
        IF (PARMJIC > 0.0) THEN
            PARIUED = AMAX1(0.0,(totalWeight()+SENTOPLITTER+SENROOT-SEEDUSE)* PLTPOP / PARMJIC)
        ENDIF
        IF (CARBOBEG > 0.0 .AND. PARMJFAC > 0.0 .AND. SRAD > 0.0 .AND. PARI > 0.0) THEN
            PARIUE = (CARBOBEG*PLTPOP)/(PARMJFAC*SRAD*PARI)
        ENDIF
                
    END SUBROUTINE YCA_Integ_WthrSum          
        
