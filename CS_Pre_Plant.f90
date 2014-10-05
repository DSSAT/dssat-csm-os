!**********************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.RATE) lines 3938 - 4069 of the original CSCAS code.
! The actual and dummy arguments are only for those variables that are dummy arguments for CSCAS. The type of all
! other variables are declared in the MODULE Module_CSCAS_Vars. The type of only the dummy arguments are declared here.
! The variables and their units are defined in CSCAS.
!
! SUBROUTINE CS_Final_Init sets up the switches for establishment and determines whether today is a planting day.
!**********************************************************************************************************************
    
    
    SUBROUTINE CS_Pre_Plant( &
        BD          , CO2         , DLAYR       , DOY         , DUL         , LL          , NH4LEFT     , NLAYR       , &
        NO3LEFT     , RNMODE      , ST          , STGYEARDOY  , SW          , YEAR        , WEATHER     , &                               ! MF 15SE14 removed <TMAX        , TMIN        , > (In Module_CSCas_Vars_List) 
        YEARPLTCSM   &
        )
        
        USE ModuleDefs
        !USE CRSIMDEF                                                                MF 15SE14 Declared in ModuleDefs
        USE Module_CSCAS_Vars_List
        
        IMPLICIT NONE
        
        TYPE (WeatherType) WEATHER
        
        CHARACTER(LEN=1) RNMODE      
        INTEGER DOY         , NLAYR       , STGYEARDOY(20)            , YEAR        , YEARPLTCSM  
        REAL    BD(NL)      , CO2         , DLAYR(NL)   , DUL(NL)     , LL(NL)      , NH4LEFT(NL) , NO3LEFT(NL) , ST(0:NL)    
        REAL    SW(NL)                                                                                                      ! MF 15SE14 removed <, TMAX        , TMIN        > (In Module_CSCas_Vars_List) 
        
        !-----------------------------------------------------------------------
        !       Set 'establishment' switches
        !-----------------------------------------------------------------------
        
        ! EARLY? are parameters that allow for water and N stresses 
        ! to be switched off early in the life cycle. If they are set
        ! to -1.0, nothing is switched off. (NB. Not 0 because this 
        ! results in the stresses being switched off on the emergence
        ! day.)  
        ! NB. When N stress is switched off, the accumulation of N in 
        ! the plant still proceeds as it would otherwise have done,  
        ! and N stress may be more severe than it otherwise would 
        ! have been once N stress is switched back on.
        EARLYN = -1.0
        EARLYW = -1.0
        IF (LNUM.LE.EARLYN) THEN
            ISWNITEARLY = 'N'
        ELSE 
            ISWNITEARLY = 'Y'
        ENDIF  
        IF (LNUM.LE.EARLYW) THEN
            ISWWATEARLY = 'N'
        ELSE 
            ISWWATEARLY = 'Y'
        ENDIF  
        
        !-----------------------------------------------------------------------
        !       Set date and environmental equivalents
        !-----------------------------------------------------------------------
        
        YEARDOY = YEAR*1000 + DOY
        TMEAN = (TMAX+TMIN)/2.0
        TMEANSURF = TMEAN
        CO2AIR = 1.0E12*CO2*1.0E-6*44.0 / (8.314*1.0E7*((TMAX+TMIN)*0.5+273.0))  ! CO2 in g/m3
        
        !-----------------------------------------------------------------------
        !       Determine if today is planting day
        !-----------------------------------------------------------------------
        
        ! YEARPLTCSM established by CSM and brought across in argument.
        IF (FILEIOT.EQ.'DS4') THEN
            IF (IPLTI.EQ.'A' .OR. (INDEX('FQN',RNMODE) > 0)) THEN
                PLYEARDOYT = YEARPLTCSM
            ENDIF  
        ENDIF
        
        IF (PLYEARDOY.GT.9000000) THEN            ! If before planting
            IF(PLYEARDOYT.GT.0 .AND. PLYEARDOYT.LT.9000000)THEN
                ! Specified planting date
                IF(YEARDOY.EQ.PLYEARDOYT) THEN
                    PLYEARDOY = YEARDOY
                    PLYEAR = YEAR
                ENDIF
            ELSE
                IF (FILEIOT.NE.'DS4') THEN
                    ! Automatic planting
                    ! Check window for automatic planting,PWDINF<PLYEART<PWDINL
                    IF (YEARDOY.GE.PWDINF.AND.YEARDOY.LE.PWDINL) THEN
                        ! Within planting window.
                        ! Determine if soil temperature and soil moisture ok
                        ! Obtain soil temperature, TSDEP, at 10 cm depth
                        I = 1
                        TSDEP = 0.0
                        XDEP = 0.0
                        DO WHILE (XDEP .LT. 10.0)
                            XDEP = XDEP + DLAYR(I)
                            TSDEP = ST(I)
                            I = I + 1
                        END DO
                        ! Compute average soil moisture as percent, AVGSW
                        I = 1
                        AVGSW = 0.0
                        CUMSW = 0.0
                        XDEP = 0.0
                        DO WHILE (XDEP .LT. SWPLTD)
                            XDEPL = XDEP
                            XDEP = XDEP + DLAYR(I)
                            IF (DLAYR(I) .LE. 0.) THEN
                                !If soil depth is less than SWPLTD
                                XDEP = SWPLTD
                                CYCLE
                            ENDIF
                            DTRY = MIN(DLAYR(I),SWPLTD - XDEPL)
                            CUMSW = CUMSW + DTRY *(MAX(SW(I) - LL(I),0.0)) / (DUL(I) - LL(I))
                            I = I + 1
                        END DO
                        AVGSW = (CUMSW / SWPLTD) * 100.0
                        IF (TSDEP .GE. PTTN .AND. TSDEP .LE. PTX) THEN
                            IF (AVGSW .GE. SWPLTL .AND. AVGSW .LE. SWPLTH) THEN
                                PLYEARDOY = YEARDOY
                                PLYEAR = YEAR
                            ENDIF
                        ENDIF
                    ENDIF
                ELSE
                    IF (YEARDOY.GT.PWDINL) THEN
                        CFLFAIL = 'Y'
                        STGYEARDOY(12) = YEARDOY  ! Failure
                        STGYEARDOY(11) = YEARDOY  ! End Crop
                        Message(1) = 'Automatic planting failure '
                        CALL WARNING(1,'CSCAS',MESSAGE)
                    ENDIF
                ENDIF
            ENDIF
        ENDIF
        
        IF (YEARDOY.EQ.PLYEARDOY) THEN
            DAP = 0
            ! Initial soil N and H2O
            ISOILN = 0.0
            ISOILH2O = 0.0
            DO I = 1, NLAYR
                ISOILN = ISOILN + NO3LEFT(I)/(10.0/(BD(I)*(DLAYR(I))))+ NH4LEFT(I)/(10.0/(BD(I)*(DLAYR(I))))
                ISOILH2O = ISOILH2O + SW(I)*DLAYR(I)
            ENDDO
            ! Plant population as established; if no data,as planted
            IF (PLTPOPE.GT.0) THEN
                PLTPOP = PLTPOPE
            ELSE
                PLTPOP = PLTPOPP
            ENDIF  
            ! Shoot # set equal to plants per hill
            IF (PLPH.GT.0.0) THEN
                SHNUM = PLPH
            ELSE
                SHNUM = 1.0
            ENDIF
            SHNUML(1) = SHNUM
        ENDIF
    END SUBROUTINE CS_Pre_Plant        