!***************************************************************************************************************************
! MF 15NO14 SUBROUTINE CS_Growth_VPD calculates the hourly response of the stomata of cassava to VPD. Once the stomata close 
! in response to VPD they remain closed for the remainder of the day. The routine also calculates a mean VPD response factor 
! for the day. The hourly and mean values of the VPD factor, VPDFPHR(24) and MNVPDFPHR, are available for use elsewhere 
! through the module Module_CSCAS_Vars_List.
! MF First (14SE14) put this code in the CSCAS subroutine CS_Growth_Photo, but had to extract it to CS_Growth_VPD so that 
! it could be called from SPAM because evapotranspiration is calculated in SPAM before CSCAS is called.
! For use with the DSSAT package, SPAM must USE Module_CSCAS_Vars_List else there will be undeclared variables in compiling
! the code. 
! MF 31DE14 No!!! You have to do this differently else you will have conflicts with variable declarations.
! MF 21JA16 Added code for Priestley-Taylor using P-T original formulation, see Suleiman and Hoogenboom (2007).
! Note that Priestley-Taylor as coded in CSCAS.FOR(6810), CSCRP.FOR(13007), PET.FOR(565) is not the original P-T formulation
! from their 1972 paper. See Ritchie in Tsuji et al. (eds.) (1998) for details of how it was coded using a weighted mean
! daily temperature. See also JWJ's comment at PET.FOR(599). This formulation was therefore not appropriate for hourly 
! estimation. Coefficients for the equations are from Allen et al. (1998). They differ from Meyer's (1993) coded
! in PETMEY, although I have not investigated why.
!***************************************************************************************************************************
    SUBROUTINE CS_Growth_VPD ( &
        TAIRHR      , WEATHER     , CONTROL   , SOILPROP   , EOP        &
        )
    
        USE ModuleDefs
        USE CS_First_Trans_m
        USE CS_VPD_m                                                                               ! MF 06JA16 To transfer hourly VPD factor to other routines (could be incorporated in ModuleDefs later).
        
        IMPLICIT NONE
        SAVE

        TYPE (ControlType) CONTROL    ! Defined in ModuleDefs
        TYPE (WeatherType) WEATHER    ! Defined in ModuleDefs
        TYPE (SoilType)    SOILPROP   ! Defined in ModuleDefs
        
        REAL    INTEGVPDFPHR                                                                       ! MF 17SE14 Added for VPD response of transpiration
        REAL    TAIRHR(TS)  , PARHR(TS)   , RADHR(TS)   , RHUMHR(TS)  , VPDHR(TS)                  ! MF 06JA16 These are all declared in ModuleDefs, but need to be declared here locally. Why?
        REAL    SNDN        , SNUP        , TDEW        , TMAX        , TMIN
        REAL    VPDFPPREV   , VPDMAXHRPREV
        REAL    CSVPSAT                                                                            ! REAL function
        REAL    AlphaPT     , DeltaVP     , GammaPS     , LambdaLH                                 ! MF 21JA16 Added for formulation of Priestley-Taylor
        REAL    MSALB       , SRAD        , DTEMP       , ETPT        , EOP                        ! MF 21JA16 Added for formulation of Priestley-Taylor
  
        INTEGER INT                                                                                ! INTEGER function
        INTEGER DYNAMIC  
        !INTEGER L                                                                                  ! Loop counter
        
        SAVE
        
!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
         
!*******************************************************************************************************************************
    IF (DYNAMIC.EQ.RATE) THEN                                              ! MF Only executed for actual growing conditions
!*******************************************************************************************************************************
        TDEW   = WEATHER % TDEW
        PARHR  = WEATHER % PARHR                                                                   ! MF 14SE14
        TMAX   = WEATHER % TMAX                                                                    ! MF 14SE14
        TMIN   = WEATHER % TMIN                                                                    ! MF 14SE14
        RADHR  = WEATHER % RADHR                                                                   ! MF 14SE14
        RHUMHR = WEATHER % RHUMHR                                                                  ! MF 14SE14
        TAIRHR = WEATHER % TAIRHR
        RADHR  = WEATHER % RADHR
        SNUP   = WEATHER % SNUP                                                                    ! MF 13NO14
        SNDN   = WEATHER % SNDN                                                                    ! MF 13NO14
        SRAD   = WEATHER % SRAD                                                                    ! MF 21JA16 
        MSALB  = SOILPROP% MSALB                                                                   ! MF 21JA16 
        
    
        ! MF 21JA16
        ! Reference ET calculated hourly using the Priestley-Taylor formulation based on the calculated hourly air temperature, TAIRHR
        AlphaPT = 1.26                                                               ! Priestley-Taylor advectivity constant, ?. 
                                                                                     ! Should be higher in semi-arid and arid environments.
        IF (LAI .LE. 0.0) THEN
          ALBEDO = MSALB
        ELSE
          !ALBEDO = 0.23-(0.23-MSALB)*EXP(-0.75*XHLAI)
          ALBEDO = 0.23-(0.23-MSALB)*EXP(-0.75*LAI)
        ENDIF
        
        ! Calculate the hourly variables, and reference transpiration using formulae and constants from http://agsys.cra-cin.it/tools
        ! Integrate the hourly data for the day.
        ET0DAY = 0.0
        DO L = 1,TS
            DeltaVP = 4098 * (0.6108 * EXP(17.27 * TAIRHR(L)/(TAIRHR(L) + 237.3)))/((TAIRHR(L) + 237.3)**2)
                                                                                     ! Slope of the saturation vapor pressure–temperature relation, delta (kPa/°C).
            LambdaLH = 2.501 - 0.002361 * TAIRHR(L)                                  ! Latent heat of vaporization of water, lambda (MJ/kg)
            GammaPS = 0.001013 * 101.325 / (0.622 * LambdaLH)                        ! Psychrometric constant, gamma (kPa/°C)
                                     ! The figure for the specific heat of moist air at constant pressure (Cp = 0.001013 MJ/kg/°C) is correct.
                                     ! GammaPS is affected by atmospheric pressure (101.325 kPa), so it should be adjusted to take account 
                                     ! of the site's elevation.
            ET0(L) = (AlphaPT/(LambdaLH)) * (DeltaVP * ((RADHR(L)*3.6/1000.) * &     ! RADHR is in J/m2/s. Multiply by 3600, divide by 10^6 for MJ/m2/hr.
                (1.0 - ALBEDO) - 0.0)) / (DeltaVP + GammaPS)
            ET0DAY = ET0DAY + ET0(L)
        END DO
        ! MF 21JA16 Calculate total ET the day based on weighted mean temperature.
        ! MF 21JA16 For comparison only with hourly calculations.
        DTEMP = 0.5 * TMAX + 0.5 * TMIN 
        DeltaVP = 4098 * (0.6108 * EXP(17.27 * DTEMP/(DTEMP + 237.3)))/((DTEMP + 237.3)**2.0)
        LambdaLH = 2.501 - 0.002361 * DTEMP
        GammaPS = 0.001013 * 101.325 / (0.622 * LambdaLH)
        ETPT = (AlphaPT/(LambdaLH)) * (DeltaVP * ((SRAD) * (1.0 - ALBEDO) - 0.0)) / (DeltaVP + GammaPS) 
        
        ! Vapour pressure                                                            ! Code for VPD reponse moved from CS_Growth and modified for hourly response.
        
        !Hourly loop for VPD stomatal response                                       ! MF 16SE14
        
        IF (TDEW.LE.-98.0) TDEW = TMIN                                               ! MF 14SE14
        DO L = 1, TS
            VPDHR(L) = (CSVPSAT(TAIRHR(L)) - CSVPSAT(TDEW))/1000.0                   ! MF 14SE14 VPDHR = VPD, hourly (kPa) 
        END DO 
        
        
        ! MF 14SE14 If VPD > PHTV, reduce VPFPHR by the amount it exceeds PHVT (the threshold), scaled by PHSV (the slope).
        VPDFPHR = 1.0                                                                ! MF 13NO14 VPDFPHR = VPD factor, hourly (#) 
        VPDFPPREV = 1.0                                                              ! MF 13NO14 VPDFPREV = VPD factor, hourly, previous hour (#)
        VPDStartHr = 0.0
        VPDMaxHr = 0.0
        DO L = 1, TS
            IF (PHTV.GT.0.0) THEN                                                    ! If PHTV has a value (.NE. -99)
                IF (VPDHR(L).GT.PHTV) THEN
                    VPDFPHR(L) = AMIN1(1.0,AMAX1(0.0,1.0+PHSV*(VPDHR(L)-PHTV)))
                    IF (L .GT. 1 .AND. VPDStartHr .EQ. 0.0) VPDStartHr = FLOAT(L)    ! Hour start of stomatal closure in response to VPD
                    VPDMaxHr = FLOAT(L)                                              ! Hour start of stomatal closure in response to VPD
                END IF
            ENDIF                                                                    ! PHTV is VPD response threshold, PHSV is the slope (negative) both set in CSCAS046.SPE. PHTV >= 5 shuts off the response.
            IF(VPDFPHR(L).GT.VPDFPPREV) THEN                                         ! If stomata close in response to VPD (VPDFPHR < 1.0), do not reopen (maintain VPDFPHR at its minimum value).
                VPDFPHR(L) = VPDFPPREV
                VPDMaxHr = VPDMaxHrPrev                                              ! Hour of maximum stomatal closure in response to VPD.
            ELSE
                VPDFPPREV = VPDFPHR(L)                                               ! Current hourly stomatal factor response to VPD
                VPDMaxHrPrev = FLOAT(L)                                              ! Current hour.
            ENDIF
        END DO
        
        ! MF 14SE14 Calculate mean VPD photosynthesis factor for the day 
        ! MF 07JA16 For comparison only with hourly calculations.
        INTEGVPDFPHR = 0.0
        DO L = 1, TS
            ! Adjust VPDFPHR for the fraction of hour at sunrise and sunset
            IF (L == INT(SNUP)) VPDFPHR(L) = VPDFPHR(L) * (REAL(L+1) - SNUP)
            IF (L == INT(SNDN)) VPDFPHR(L) = VPDFPHR(L) * (SNDN - REAL(L))
            INTEGVPDFPHR = INTEGVPDFPHR + VPDFPHR(L)
        END DO
        IF (DAP >= 1) MNVPDFPHR = AMIN1(1.0, AMAX1(0.0, (INTEGVPDFPHR / (SNDN - SNUP))))      ! MF 13NO14 MNVPDFPHR is the integral of the hourly VPDFPHR divided by the hours from sunrise to sunset. Range 0-1.
        CONTINUE
        
        ! MF 24JA16 Adjust PT estimate of hourly ET by the VPD factor. Note this is on the whole ET, not the transpiration component.
        EOP = 0.0
        DO L = 1, TS
             ET0(L) = ET0(L) * VPDFPHR(L)
             EOP = EOP + ET0(L)
        END DO
       
!*******************************************************************************************************************************
    END IF                                              ! MF DYNAMIC SECTION
!*******************************************************************************************************************************
    END SUBROUTINE CS_Growth_VPD
    
!-----------------------------------------------------------------------
!     VARIABLE DEFINITIONS: (updated 05JA16)
!-----------------------------------------------------------------------
! ALBEDO        Reflectance of soil-crop surface, fraction
! CSVPSAT       Calculates saturated vapor pressure of air (Tetens, 1930). (Real FUNCTION in CSUTS.FOR)
! ET0           Hourly reference transpiration (mm/m2/hr)  (Defined in CS_VPD_m)
! ET0DAY        Daily integral of ref transp (mm/m2/d)     (Defined in CS_VPD_m)
! INTEGVPDFPHR  Integral of the hourly VPDFPHR divided by the hours from sunrise to sunset. Range 0-1.
! MSALB         Soil albedo with mulch and soil water effects, fraction
! PARHR         Hourly PAR, MJ/m2                          (from WEATHER % PARHR  in ModuleDefs)
! PHTV          VPD response threshold, kPa                (set in CSCAS046.SPE. PHTV >= 5 shuts off the response. In CS_First_Trans_m)
! PHSV          Slope of VPD response, #/kPa               (negative, set in CSCAS046.SPE. In CS_First_Trans_m)
! RADHR         Solar radiation, hourly                    (from WEATHER % RADHR  in ModuleDefs)
! RHUMHR        Relative humidity, hourly, %               (from WEATHER % RHUMHR in ModuleDefs)
! SNUP          Sunrise, hours                             (from WEATHER % SNUP   in ModuleDefs)
! SNDN          Sunset, hours                              (from WEATHER % SNDN   in ModuleDefs)
! TAIRHR        Air temperature, hourly, °C                (from WEATHER % TAIRHR in ModuleDefs)
! TDEW          Dew point tempreature,°C                   (from WEATHER % TDEW   in ModuleDefs)
! TMAX          Maximum temperature, °C                    (from WEATHER % TMAX   in ModuleDefs)
! TMIN          Minimum temperature, °C                    (from WEATHER % TMIN   in ModuleDefs)
! VPDFPHR       VPD factor, hourly (#, 0-1)                (Defined in CS_VPD_m)
    
! MF 21JA16 Constants for formulation of Priestley-Taylor calculation of reference evapotranspiration
! AphaPT        Priestley-Taylor coefficient (1.26)
! DeltaVP       Slope of the saturation vapour pressure-temperature relationship (kPa/°C)
! GammaPS       Psychrometric constant(kPa/°C) 
! LambdaLH      Latent heat of vaporization of water (MJ/kg) 
 

