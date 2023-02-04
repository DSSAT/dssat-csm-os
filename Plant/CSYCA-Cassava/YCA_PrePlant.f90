!**********************************************************************************************************************
! This is the code from the section (DYNAMIC == RATE) lines 3938 - 4069 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
! 
! Subroutine YCA_PrePlant sets up the switches for establishment and determines whether today is a planting day.
!**********************************************************************************************************************
    
    SUBROUTINE YCA_PrePlant( &  
        BD          , CO2         , DLAYR       , DOY         , DUL         , LL          , NH4LEFT     , NLAYR       , &
        NO3LEFT     , RNMODE      , ST          , STGYEARDOY  , SW          , TMAX        , TMIN        , YEAR        , &
        YEARPLTCSM  &                 ! WEATHER     ,      
        )
        
        USE ModuleDefs
        USE YCA_First_Trans_m
        
        IMPLICIT NONE
        EXTERNAL WARNING
        
        !TYPE (WeatherType) WEATHER    , WEATHER
        
        CHARACTER(LEN=1) RNMODE
        
        INTEGER DOY         , NLAYR       , STGYEARDOY(0:19)            , YEAR        , YEARPLTCSM
        
        REAL    BD(NL)      , CO2         , DLAYR(NL)   , DUL(NL)     , LL(NL)      , NH4LEFT(NL) , NO3LEFT(NL) , ST(NL)    
        REAL    SW(NL)      , TMAX        , TMIN        
        
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
        IF (LNUM <= EARLYN) THEN
            ISWNITEARLY = 'N'
        ELSE 
            ISWNITEARLY = 'Y'
        ENDIF  
        IF (LNUM <= EARLYW) THEN
            ISWWATEARLY = 'N'
        ELSE 
            ISWWATEARLY = 'Y'
        ENDIF  
        
        !-----------------------------------------------------------------------
        !       Set date and environmental equivalents
        !-----------------------------------------------------------------------
        
        YEARDOY = YEAR*1000 + DOY
        TMEAN = (TMAX+TMIN)/2.0                                                                                        !EQN 057
        TMEANSURF = TMEAN
        CO2AIR = 1.0E12*CO2*1.0E-6*44.0 / (8.314*1.0E7*((TMAX+TMIN)*0.5+273.0))  ! CO2 in g/m3                         !EQN 266
        
        !-----------------------------------------------------------------------
        !       Determine if today is planting day
        !-----------------------------------------------------------------------
        
        ! YEARPLTCSM established by CSM and brought across in argument.
        IF (FILEIOT == 'DS4') THEN
!           IF (IPLTI == 'A' .OR. (INDEX('FQN',RNMODE) > 0)) THEN
            IF (IPLTI == 'A' .OR. IPLTI =='F' .OR. (INDEX('FQN',RNMODE) > 0)) THEN
                PLYEARDOYT = YEARPLTCSM
            ENDIF  
        ENDIF
        
        IF (PLYEARDOY > 9000000) THEN            ! If before planting
            IF(PLYEARDOYT > 0 .AND. PLYEARDOYT < 9000000)THEN
                ! Specified planting date
                IF(YEARDOY == PLYEARDOYT) THEN
                    PLYEARDOY = YEARDOY
                    PLYEAR = YEAR
                ENDIF
            ELSE
                IF (FILEIOT /= 'DS4') THEN
                    ! Automatic planting
                    ! Check window for automatic planting,PWDINF<PLYEART<PWDINL
                    IF (YEARDOY >= PWDINF.AND.YEARDOY <= PWDINL) THEN
                        ! Within planting window.
                        ! Determine if soil temperature and soil moisture ok
                        ! Obtain soil temperature, TSDEP, at 10 cm depth
                        I = 1
                        TSDEP = 0.0
                        XDEP = 0.0
                        DO WHILE (XDEP  <  10.0)
                            XDEP = XDEP + DLAYR(I)
                            TSDEP = ST(I)
                            I = I + 1
                        END DO
                        ! Compute average soil moisture as percent, AVGSW
                        I = 1
                        AVGSW = 0.0
                        CUMSW = 0.0
                        XDEP = 0.0
                        DO WHILE (XDEP  <  SWPLTD)
                            XDEPL = XDEP
                            XDEP = XDEP + DLAYR(I)
                            IF (DLAYR(I)  <=  0.) THEN
                                !If soil depth is less than SWPLTD
                                XDEP = SWPLTD
                                CYCLE
                            ENDIF
                            DTRY = MIN(DLAYR(I),SWPLTD - XDEPL)
                            CUMSW = CUMSW + DTRY *(MAX(SW(I) - LL(I),0.0)) / (DUL(I) - LL(I))                          !EQN 032
                            I = I + 1
                        END DO
                        AVGSW = (CUMSW / SWPLTD) * 100.0                                                               !EQN 033
                        IF (TSDEP  >=  PTTN .AND. TSDEP  <=  PTX) THEN
                            IF (AVGSW  >=  SWPLTL .AND. AVGSW  <=  SWPLTH) THEN
                                PLYEARDOY = YEARDOY
                                PLYEAR = YEAR
                            ENDIF
                        ENDIF
                    ENDIF
                ELSE
                    IF (YEARDOY > PWDINL) THEN
                        CFLFAIL = 'Y'
                        STGYEARDOY(PSX+2) = YEARDOY  ! Failure
                        STGYEARDOY(PSX+1) = YEARDOY  ! End Crop
                        Message(1) = 'Automatic planting failure '
                        CALL WARNING(1,'CSYCA',MESSAGE)
                    ENDIF
                ENDIF
            ENDIF
        ENDIF
        
        IF (YEARDOY == PLYEARDOY) THEN
            DAP = 0
            ! Initial soil N and H2O
            ISOILN = 0.0
            ISOILH2O = 0.0
            DO I = 1, NLAYR
                ISOILN = ISOILN + NO3LEFT(I)/(10.0/(BD(I)*(DLAYR(I))))+ NH4LEFT(I)/(10.0/(BD(I)*(DLAYR(I))))           !EQN 030
                ISOILH2O = ISOILH2O + SW(I)*DLAYR(I)                                                                   !EQN 031
            ENDDO
            ! Plant population as established; if no data,as planted
            IF (PLTPOPE > 0) THEN
                PLTPOP = PPOE !LPM 06MAR2016 To have just one name for PPOE
            ELSE
                PLTPOP = PPOP !LPM 06MAR2016 To have just one name for PPOP
            ENDIF  
            ! Shoot # set equal to plants per hill
            IF (PLPH > 0.0) THEN
                SHNUM = PLPH
            ELSE
                SHNUM = 1.0
            ENDIF
            SHNUML(1) = SHNUM
        ENDIF
        
    END SUBROUTINE YCA_PrePlant        
