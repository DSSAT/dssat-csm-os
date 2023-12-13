
Module YCA_Growth_VPD
    USE ModuleDefs
    
    REAL, DIMENSION(TS)    :: VPDFPHR                ! Hourly VPD factor, 0-1
    REAL, DIMENSION(TS)    :: EOPH                   ! Hourly potential transpiration
    REAL    :: MNVPDFPHR                  ! Daily mean VPD factor, 0-1MNVPDFPHR
    REAL    :: VPDStartHr    , VPDMaxHr   ! VPD start hour, VPD max hour
    REAL    :: ET0DAY                     ! Daily integral of reference transpiration
    contains 
    

         
!****************************************************************************************
! Hourly VPD Factor for Photosynthesis function
!****************************************************************************************
    
    REAL function get_Growth_VPDFPHR (PHSV, PHTV, TDEW, TMIN, TAIRHR, targetHour)
    
    USE ModuleDefs
        
        IMPLICIT NONE
        EXTERNAL VPSAT
        SAVE
!       TYPE (WeatherType) WEATHER 

        INTEGER, intent (in) :: targetHour
        REAL, intent (in) :: PHSV
        REAL, intent (in) :: PHTV
        
        
        REAL, DIMENSION(TS) ::   TAIRHR, VPDHR ! PARHR   , RADHR  , RHUMHR  , These are all declared in ModuleDefs
!       REAL    INTEGVPDFPHR                                                                         ! Added for VPD response of transpiration
        REAL    TDEW        , TMIN  !SNDN        , SNUP        , TMAX        , 
        REAL    VPDFPPREV   , VPDMAXHRPREV
        REAL    VPSAT                                                                            ! REAL function
!       REAL    AlphaPT     , DeltaVP     , GammaPS     , LambdaLH                                 ! Added for formulation of Priestley-Taylor
!       REAL    MSALB       , SRAD        , DTEMP       , ETPT        , EOP                        ! Added for formulation of Priestley-Taylor
        INTEGER hour                                                                                  ! Loop counter
!       REAL ALBEDO
        REAL VPD_TRANSP  !VPDFP, INTEG_1, INTEG_2, 
        
        SAVE
        
!     Transfer values from constructed data types into local variables.

        
        
                
        ! Vapour pressure                                                            ! Code for VPD reponse moved from CS_Growth and modified for hourly response.
        
        !Hourly loop for VPD stomatal response
        
        IF (TDEW <= -98.0) THEN ! validating if it is -99
            TDEW = TMIN
        ENDIF
        VPD_TRANSP = 0.0
        DO hour =1, TS
            VPDHR(hour) = (VPSAT(TAIRHR(hour)) - VPSAT(TDEW))/1000.0                   ! VPDHR = VPD, hourly (kPa) 
            VPD_TRANSP = VPD_TRANSP + VPDHR(hour)
        END DO 
        VPD_TRANSP = VPD_TRANSP/TS
        
        ! If VPD > PHTV, reduce VPFPHR by the amount it exceeds PHVT (the threshold), scaled by PHSV (the slope).
        VPDFPHR = 1.0                                                                ! VPDFPHR = VPD factor, hourly (#) 
        VPDFPPREV = 1.0                                                              ! VPDFPREV = VPD factor, hourly, previous hour (#)
        VPDStartHr = 0.0
        VPDMaxHr = 0.0
        DO hour =1, TS
            IF (PHTV > 0.0) THEN                                                    ! If PHTV has a value (.NE. -99)
                IF (VPDHR(hour) > PHTV) THEN
                    VPDFPHR(hour) = AMIN1(1.0,AMAX1(0.0,1.0-PHSV*(VPDHR(hour)-PHTV)))
                    IF (hour > 1 .AND. VPDStartHr == 0.0) THEN
                        VPDStartHr = FLOAT(hour)    ! Hour start of stomatal closure in response to VPD
                    ENDIF
                    VPDMaxHr = FLOAT(hour)                                              ! Hour start of stomatal closure in response to VPD
                END IF
            ENDIF                                                                    ! PHTV is VPD response threshold, PHSV is the slope (negative) both set in CSCAS046.SPE. PHTV >= 5 shuts off the response.
            IF(VPDFPHR(hour) > VPDFPPREV) THEN                                         ! If stomata close in response to VPD (VPDFPHR < 1.0), do not reopen (maintain VPDFPHR at its minimum value).
                VPDFPHR(hour) = VPDFPPREV
                VPDMaxHr = VPDMaxHrPrev                                              ! Hour of maximum stomatal closure in response to VPD.
            ELSE
                VPDFPPREV = VPDFPHR(hour)                                               ! Current hourly stomatal factor response to VPD
                VPDMaxHrPrev = FLOAT(hour)                                              ! Current hour.
            ENDIF
        END DO
        

        get_Growth_VPDFPHR = VPDFPHR(targetHour)
        
    END function get_Growth_VPDFPHR
        
!****************************************************************************************
! VPD Factor for Photosynthesis function
!****************************************************************************************
    
!   REAL function get_Growth_VPDFP(DAP, LAI, PHSV, PHTV, WEATHER, CONTROL, SOILPROP)
    REAL function get_Growth_VPDFP(PHSV, PHTV, WEATHER, SOILPROP)
    
    USE ModuleDefs
        
        IMPLICIT NONE
        SAVE

!       INTEGER, intent (in) :: DAP
!       REAL, intent (in) :: LAI
        REAL, intent (in) :: PHSV
        REAL, intent (in) :: PHTV
!       TYPE (ControlType), intent (in) :: CONTROL    ! Defined in ModuleDefs
        TYPE (WeatherType), intent (in) :: WEATHER    ! Defined in ModuleDefs
        TYPE (SoilType), intent (in) ::   SOILPROP   ! Defined in ModuleDefs
        
        
        REAL, DIMENSION(TS) ::   TAIRHR, RADHR   ! PARHR   , RHUMHR  , VPDHR, These are all declared in ModuleDefs
        REAL    INTEGVPDFPHR                                                                         ! Added for VPD response of transpiration
        REAL    TDEW        , TMAX        , TMIN  !SNDN        , SNUP        , 
!       REAL    VPDFPPREV   , VPDMAXHRPREV
!       REAL    VPSAT                                                                            ! REAL function
!       REAL    AlphaPT     , DeltaVP     , GammaPS     , LambdaLH                                 ! Added for formulation of Priestley-Taylor
        REAL    MSALB       , SRAD      ! , DTEMP       , ETPT        , EOP                        ! Added for formulation of Priestley-Taylor
        INTEGER hour                                                                                  ! Loop counter
!       REAL ALBEDO
!       REAl VPDFP, INTEG_1, INTEG_2
        
        SAVE
        TDEW   = WEATHER % TDEW
        TMAX   = WEATHER % TMAX
        TMIN   = WEATHER % TMIN
        RADHR  = WEATHER % RADHR
        TAIRHR = WEATHER % TAIRHR
        SRAD   = WEATHER % SRAD
        MSALB  = SOILPROP% MSALB
        
        VPDFPHR(1) = get_Growth_VPDFPHR(PHSV, PHTV, TDEW, TMIN, TAIRHR, 1)
        ! Calculate mean VPD photosynthesis factor for the day 
        ! For comparison only with hourly calculations.
        INTEGVPDFPHR = 0.0
        DO hour =1, TS
            ! Adjust VPDFPHR for the fraction of hour at sunrise and sunset
            !IF (hour == INT(SNUP))THEN 
            !    VPDFPHR(hour) = VPDFPHR(hour) * (REAL(hour+1) - SNUP) 
            !ENDIF
            !IF (hour == INT(SNDN)) THEN !DA can i assume it's >=
            !    VPDFPHR(hour) = VPDFPHR(hour) * (SNDN - REAL(hour))
            !ENDIF
            INTEGVPDFPHR = INTEGVPDFPHR + VPDFPHR(hour)
        END DO

        
        get_Growth_VPDFP = INTEGVPDFPHR/TS
        
    END function get_Growth_VPDFP
    
    
  
    
   

!@author mylesjfisher 
! MF 15NO14 SUBROUTINE CS_Growth_VPD calculates the hourly response of the stomata of cassava to VPD.
! Once the stomata close in response to VPD they remain closed for the remainder of the day. 
! The routine also calculates a mean VPD response factor for the day. 
! The hourly and mean values of the VPD factor, VPDFPHR(24) and MNVPDFPHR, are available for use elsewhere through the module Module_CSCAS_Vars_List.

! MF First (14SE14) put this code in the CSCAS subroutine CS_Growth_Photo, but had to extract it to CS_Growth_VPD so that it could be called from SPAM
! because evapotranspiration is calculated in SPAM before CSCAS is called. The VPD response therefore is required before the crop submodel is called.
! For use with the DSSAT package, SPAM must USE Module_CSCAS_Vars_List else there will be undeclared variables in compiling the code. 
! MF 31DE14 No!! You should do this another way else you will have conflicts with variable declarations.

!MF 21JA16 Added code for Priestley-Taylor using Priestley and Taylor’s original formulation, see Suleiman and Hoogenboom (2007).

! Note that Priestley-Taylor as coded in CSCAS.FOR(6810), CSCRP.FOR(13007), and PET.FOR(565) is not the original Priestley-Taylor formulation from their 1972 paper. 
! See Ritchie in Tsuji et al. (eds.) (1998) for details of how it was coded using a weighted mean daily temperature. See also JWJ's comment at PET.FOR(599).
! This formulation was therefore not appropriate for hourly estimation. Coefficients for the equations are from Allen et al. (1998). 
! They differ from Meyer's (1993) coded in PETMEY, which is because Meyer (1993) is for inland SE Australia although I can see no a priori reason why. 
! I have not investigated the details further (I have asked Meyer for a copy of the document).
!
! References
! Meyer, W.S. (1993). Standard reference evaporation calculation for inland, south-east Australia. CSIRO Division of Water Resources, Griffith. Technical Memorandum.
! Priestley, C.H.B. and Taylor, R.J. (1972). On the assessment of surface heat flux and evaporation using large-scale parameters. Monthly Weather Review 100(2):81–92. 
! Ritchie, J.T. (1998). Soil water balance and plant water stress. In: G.Y. Tsuji, G. Hoogenboom and P.K. Thornton (eds.) Understanding Options for Agricultural Production. Dordrecht: Kluwer. Pp.41–54.
! Suleiman, A.A. and Hoogenboom, G. (2007). Comparison of Priestley-Taylor and FAO-56 Penman-Monteith for daily reference evapotranspiration estimation in Georgia. Journal of Irrigation and Drainage Engineering 133(2):175-182.
    
END Module YCA_Growth_VPD
    
!-----------------------------------------------------------------------
!     VARIABLE DEFINITIONS: (updated 05NO16)
!-----------------------------------------------------------------------
! ALBEDO        Reflectance of soil-crop surface, fraction
! CSVPSAT       Calculates saturated vapor pressure of air (Tetens, 1930). (Real FUNCTION in CSUTS.FOR)
! ET0           Hourly reference transpiration (mm/m2/hr)  (Defined in YCA_Model_VPD)
! ET0DAY        Daily integral of ref transp (mm/m2/d)     (Defined in YCA_Model_VPD)
! INTEGVPDFPHR  Integral of the hourly VPDFPHR divided by the hours from sunrise to sunset. Range 0-1.
! MSALB         Soil albedo with mulch and soil water effects, fraction
! PARHR         Hourly PAR, MJ/m2                          (from WEATHER % PARHR  in ModuleDefs)
! PHTV          VPD response threshold, kPa                (set in CSCAS046.SPE. PHTV >= 5 shuts off the response. In YCA_First_Trans_m)
! PHSV          Slope of VPD response, #/kPa               (negative, set in CSCAS046.SPE. In YCA_First_Trans_m)
! RADHR         Solar radiation, hourly                    (from WEATHER % RADHR  in ModuleDefs)
! RHUMHR        Relative humidity, hourly, %               (from WEATHER % RHUMHR in ModuleDefs)
! SNUP          Sunrise, hours                             (from WEATHER % SNUP   in ModuleDefs)
! SNDN          Sunset, hours                              (from WEATHER % SNDN   in ModuleDefs)
! TAIRHR        Air temperature, hourly, °C                (from WEATHER % TAIRHR in ModuleDefs)
! TDEW          Dew point tempreature,°C                   (from WEATHER % TDEW   in ModuleDefs)
! TMAX          Maximum temperature, °C                    (from WEATHER % TMAX   in ModuleDefs)
! TMIN          Minimum temperature, °C                    (from WEATHER % TMIN   in ModuleDefs)
! VPDFPHR       VPD factor, hourly (#, 0-1)                (Defined in CS_VPD_m)
    
! Constants for formulation of Priestley-Taylor calculation of reference evapotranspiration
! AphaPT        Priestley-Taylor coefficient (1.26)
! DeltaVP       Slope of the saturation vapour pressure-temperature relationship (kPa/°C)
! GammaPS       Psychrometric constant(kPa/°C) 
! LambdaLH      Latent heat of vaporization of water (MJ/kg) 
