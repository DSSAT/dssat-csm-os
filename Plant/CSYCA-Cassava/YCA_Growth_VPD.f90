
Module YCA_Growth_VPD
    USE ModuleDefs
    
    REAL, DIMENSION(TS)    :: VPDFPHR                ! Hourly VPD factor, 0-1
    REAL, DIMENSION(TS)    :: ET0                    ! Hourly reference transpiration
    REAL    :: MNVPDFPHR                  ! Daily mean VPD factor, 0-1MNVPDFPHR
    REAL    :: VPDStartHr    , VPDMaxHr   ! VPD start hour, VPD max hour
    REAL    :: ET0DAY                     ! Daily integral of reference transpiration
    contains 
    
!****************************************************************************************
! EO function
!****************************************************************************************

    
    REAL function get_Growth_EO (DAP, LAI, PHSV, PHTV, WEATHER, CONTROL, SOILPROP)
    
    USE ModuleDefs
        
        IMPLICIT NONE
        SAVE

        INTEGER, intent (in) :: DAP
        REAL, intent (in) :: LAI
        REAL, intent (in) :: PHSV
        REAL, intent (in) :: PHTV
        TYPE (ControlType), intent (in) :: CONTROL    ! Defined in ModuleDefs
        TYPE (WeatherType), intent (in) :: WEATHER    ! Defined in ModuleDefs
        TYPE (SoilType), intent (in) ::   SOILPROP   ! Defined in ModuleDefs
        
        
        REAL, DIMENSION(TS) ::   TAIRHR, PARHR   , RADHR  , RHUMHR  , VPDHR ! These are all declared in ModuleDefs
        REAL    INTEGVPDFPHR                                                                         ! Added for VPD response of transpiration
        REAL    SNDN        , SNUP        , TDEW        , TMAX        , TMIN
        REAL    VPDFPPREV   , VPDMAXHRPREV
        REAL    CSVPSAT                                                                            ! REAL function
        REAL    AlphaPT     , DeltaVP     , GammaPS     , LambdaLH                                 ! Added for formulation of Priestley-Taylor
        REAL    MSALB       , SRAD        , DTEMP       , ETPT        , EOP                        ! Added for formulation of Priestley-Taylor
        INTEGER hour                                                                                  ! Loop counter
        REAL ALBEDO
        REAl VPDFP, INTEG_1, INTEG_2
        
        SAVE
        
!     Transfer values from constructed data types into local variables.

        TDEW   = WEATHER % TDEW
        PARHR  = WEATHER % PARHR
        TMAX   = WEATHER % TMAX
        TMIN   = WEATHER % TMIN
        RADHR  = WEATHER % RADHR
        RHUMHR = WEATHER % RHUMHR
        TAIRHR = WEATHER % TAIRHR
        SNUP = WEATHER % SNUP
        SNDN   = WEATHER % SNDN
        SRAD   = WEATHER % SRAD
        MSALB  = SOILPROP% MSALB
        ! Reference ET calculated hourly using the Priestley-Taylor formulation based on the calculated hourly air temperature, TAIRHR
        AlphaPT = 1.26                                                               ! Priestley-Taylor advectivity constant, ?. 
                                                                                     ! Should be higher in semi-arid and arid environments.
        IF (LAI <= 0.0) THEN
          ALBEDO = MSALB
        ELSE
          ALBEDO = 0.23-(0.23-MSALB)*EXP(-0.75*LAI)
        ENDIF
        
        ! Calculate the hourly variables, and reference transpiration using formulae and constants from http://agsys.cra-cin.it/tools
        ! Integrate the hourly data for the day.
        ET0DAY = 0.0
        DO hour = 1,TS !
            DeltaVP = 4098 * (0.6108 * EXP(17.27 * TAIRHR(hour)/(TAIRHR(hour) + 237.3)))/((TAIRHR(hour) + 237.3)**2)
                                                                                     ! Slope of the saturation vapor pressure–temperature relation, delta (kPa/°C).
            LambdaLH = 2.501 - 0.002361 * TAIRHR(hour)                                  ! Latent heat of vaporization of water, lambda (MJ/kg)
            GammaPS = 0.001013 * 101.325 / (0.622 * LambdaLH)                        ! Psychrometric constant, gamma (kPa/°C)
                                     ! The figure for the specific heat of moist air at constant pressure (Cp = 0.001013 MJ/kg/°C) is correct.
                                     ! GammaPS is affected by atmospheric pressure (101.325 kPa), so it should be adjusted to take account 
                                     ! of the site's elevation.
            !ET0(hour) = (AlphaPT/(LambdaLH)) * (DeltaVP * ((RADHR(hour)*3.6/1000.) * (1.0 - EXP(-0.75*LAI)))) / (DeltaVP + GammaPS)    
            ! RADHR is in J/m2/s. Multiply by 3600, divide by 10^6 for MJ/m2/hr.
            
            
            ET0(hour) = (AlphaPT/(LambdaLH)) * (DeltaVP * ((RADHR(hour)*3.6/1000.) *   (1.0 - ALBEDO) - 0.0)) / (DeltaVP + GammaPS)
            ! RADHR is in J/m2/s. Multiply by 3600, divide by 10^6 for MJ/m2/hr.
              
                
            ET0DAY = ET0DAY + ET0(hour)
        END DO
                
        get_Growth_EO=ET0DAY
    
    END function get_Growth_EO

         
!****************************************************************************************
! Hourly VPD Factor for Photosynthesis function
!****************************************************************************************
    
    REAL function get_Growth_VPDFPHR (DAP, LAI, PHSV, PHTV, WEATHER, CONTROL, SOILPROP, targetHour)
    
    USE ModuleDefs
        
        IMPLICIT NONE
        SAVE

        INTEGER, intent (in) :: DAP
        INTEGER, intent (in) :: targetHour
        REAL, intent (in) :: LAI
        REAL, intent (in) :: PHSV
        REAL, intent (in) :: PHTV
        TYPE (ControlType), intent (in) :: CONTROL    ! Defined in ModuleDefs
        TYPE (WeatherType), intent (in) :: WEATHER    ! Defined in ModuleDefs
        TYPE (SoilType), intent (in) ::   SOILPROP   ! Defined in ModuleDefs
        
        
        REAL, DIMENSION(TS) ::   TAIRHR, PARHR   , RADHR  , RHUMHR  , VPDHR ! These are all declared in ModuleDefs
        REAL    INTEGVPDFPHR                                                                         ! Added for VPD response of transpiration
        REAL    SNDN        , SNUP        , TDEW        , TMAX        , TMIN
        REAL    VPDFPPREV   , VPDMAXHRPREV
        REAL    CSVPSAT                                                                            ! REAL function
        REAL    AlphaPT     , DeltaVP     , GammaPS     , LambdaLH                                 ! Added for formulation of Priestley-Taylor
        REAL    MSALB       , SRAD        , DTEMP       , ETPT        , EOP                        ! Added for formulation of Priestley-Taylor
        INTEGER hour                                                                                  ! Loop counter
        REAL ALBEDO
        REAl VPDFP, INTEG_1, INTEG_2
        
        SAVE
        
!     Transfer values from constructed data types into local variables.

        TDEW   = WEATHER % TDEW
        PARHR  = WEATHER % PARHR
        TMAX   = WEATHER % TMAX
        TMIN   = WEATHER % TMIN
        RADHR  = WEATHER % RADHR
        RHUMHR = WEATHER % RHUMHR
        TAIRHR = WEATHER % TAIRHR
        SNUP = WEATHER % SNUP
        SNDN   = WEATHER % SNDN
        SRAD   = WEATHER % SRAD
        MSALB  = SOILPROP% MSALB
        
        
                
        ! Vapour pressure                                                            ! Code for VPD reponse moved from CS_Growth and modified for hourly response.
        
        !Hourly loop for VPD stomatal response
        
        IF (TDEW <= -98.0) THEN ! validating if it is -99
            TDEW = TMIN
        ENDIF
        DO hour =1, TS
            VPDHR(hour) = (CSVPSAT(TAIRHR(hour)) - CSVPSAT(TDEW))/1000.0                   ! VPDHR = VPD, hourly (kPa) 
        END DO 
        
        
        ! If VPD > PHTV, reduce VPFPHR by the amount it exceeds PHVT (the threshold), scaled by PHSV (the slope).
        VPDFPHR = 1.0                                                                ! VPDFPHR = VPD factor, hourly (#) 
        VPDFPPREV = 1.0                                                              ! VPDFPREV = VPD factor, hourly, previous hour (#)
        VPDStartHr = 0.0
        VPDMaxHr = 0.0
        DO hour =1, TS
            IF (PHTV > 0.0) THEN                                                    ! If PHTV has a value (.NE. -99)
                IF (VPDHR(hour) > PHTV) THEN
                    VPDFPHR(hour) = AMIN1(1.0,AMAX1(0.0,1.0+PHSV*(VPDHR(hour)-PHTV)))
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
    
    REAL function get_Growth_VPDFP(DAP, LAI, PHSV, PHTV, WEATHER, CONTROL, SOILPROP)
    
    USE ModuleDefs
        
        IMPLICIT NONE
        SAVE

        INTEGER, intent (in) :: DAP
        REAL, intent (in) :: LAI
        REAL, intent (in) :: PHSV
        REAL, intent (in) :: PHTV
        TYPE (ControlType), intent (in) :: CONTROL    ! Defined in ModuleDefs
        TYPE (WeatherType), intent (in) :: WEATHER    ! Defined in ModuleDefs
        TYPE (SoilType), intent (in) ::   SOILPROP   ! Defined in ModuleDefs
        
        
        REAL, DIMENSION(TS) ::   TAIRHR, PARHR   , RADHR  , RHUMHR  , VPDHR ! These are all declared in ModuleDefs
        REAL    INTEGVPDFPHR                                                                         ! Added for VPD response of transpiration
        REAL    SNDN        , SNUP        , TDEW        , TMAX        , TMIN
        REAL    VPDFPPREV   , VPDMAXHRPREV
        REAL    CSVPSAT                                                                            ! REAL function
        REAL    AlphaPT     , DeltaVP     , GammaPS     , LambdaLH                                 ! Added for formulation of Priestley-Taylor
        REAL    MSALB       , SRAD        , DTEMP       , ETPT        , EOP                        ! Added for formulation of Priestley-Taylor
        INTEGER hour                                                                                  ! Loop counter
        REAL ALBEDO
        REAl VPDFP, INTEG_1, INTEG_2
        
        SAVE

        VPDFPHR(1) = get_Growth_VPDFPHR(DAP, LAI, PHSV, PHTV, WEATHER, CONTROL, SOILPROP,1)
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

        
        get_Growth_VPDFP = INTEGVPDFPHR
        
    END function get_Growth_VPDFP
    
    
!****************************************************************************************
! EOP function
!****************************************************************************************

    
    REAL function get_Growth_EOP(DAP, LAI, PHSV, PHTV, WEATHER, CONTROL, SOILPROP)
    
        USE ModuleDefs
        
        IMPLICIT NONE
        SAVE

        INTEGER, intent (in) :: DAP
        REAL, intent (in) :: LAI
        REAL, intent (in) :: PHSV
        REAL, intent (in) :: PHTV
        TYPE (ControlType), intent (in) :: CONTROL    ! Defined in ModuleDefs
        TYPE (WeatherType), intent (in) :: WEATHER    ! Defined in ModuleDefs
        TYPE (SoilType), intent (in) ::   SOILPROP   ! Defined in ModuleDefs
        
        
        REAL, DIMENSION(TS) ::   TAIRHR, PARHR   , RADHR  , RHUMHR  , VPDHR ! These are all declared in ModuleDefs
        REAL    INTEGVPDFPHR                                                                         ! Added for VPD response of transpiration
        REAL    SNDN        , SNUP        , TDEW        , TMAX        , TMIN
        REAL    VPDFPPREV   , VPDMAXHRPREV
        REAL    CSVPSAT                                                                            ! REAL function
        REAL    AlphaPT     , DeltaVP     , GammaPS     , LambdaLH                                 ! Added for formulation of Priestley-Taylor
        REAL    MSALB       , SRAD        , DTEMP       , ETPT        , EOP                        ! Added for formulation of Priestley-Taylor
        INTEGER hour                                                                                  ! Loop counter
        REAL ALBEDO
        REAl VPDFP, INTEG_1, INTEG_2
        
        SAVE
        
!     Transfer values from constructed data types into local variables.

        TDEW   = WEATHER % TDEW
        PARHR  = WEATHER % PARHR
        TMAX   = WEATHER % TMAX
        TMIN   = WEATHER % TMIN
        RADHR  = WEATHER % RADHR
        RHUMHR = WEATHER % RHUMHR
        TAIRHR = WEATHER % TAIRHR
        SNUP = WEATHER % SNUP
        SNDN   = WEATHER % SNDN
        SRAD   = WEATHER % SRAD
        MSALB  = SOILPROP% MSALB
        
        ET0DAY= get_Growth_EO(DAP, LAI, PHSV, PHTV, WEATHER, CONTROL, SOILPROP)
        MNVPDFPHR = get_Growth_VPDFP (DAP, LAI, PHSV, PHTV, WEATHER, CONTROL, SOILPROP)
 

        ! Adjust PT estimate of hourly ET by the VPD factor. 
        ! Note this is on the whole ET, not the transpiration component.
        EOP = 0.0
        DO hour =1, TS
             
             ET0(hour) = ET0(hour) * VPDFPHR(hour)
             EOP = EOP + ET0(hour)
        END DO
        
        get_Growth_EOP=EOP
        
        
    END function get_Growth_EOP
    
    
    
!****************************************************************************************
! MYLES ORIGINAL CODE
!****************************************************************************************

    !!! @deprecated
    !!! this is original Myles Fisher code before making it as several functions
    

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

       
     SUBROUTINE GROWTH_VPD (DAP, LAI, PHSV, PHTV, WEATHER, CONTROL, SOILPROP)
        
        USE ModuleDefs
       
        IMPLICIT NONE
        SAVE

        INTEGER, intent (in) :: DAP
        REAL, intent (in) :: LAI
        REAL, intent (in) :: PHSV
        REAL, intent (in) :: PHTV
        TYPE (ControlType), intent (in) :: CONTROL    ! Defined in ModuleDefs
        TYPE (WeatherType), intent (in) :: WEATHER    ! Defined in ModuleDefs
        TYPE (SoilType), intent (in) ::   SOILPROP   ! Defined in ModuleDefs
        REAL, DIMENSION(TS) ::   TAIRHR, PARHR   , RADHR  , RHUMHR  , VPDHR ! These are all declared in ModuleDefs
        REAL    INTEGVPDFPHR                                                                         ! Added for VPD response of transpiration
        REAL    SNDN        , SNUP        , TDEW        , TMAX        , TMIN
        REAL    VPDFPPREV   , VPDMAXHRPREV
        REAL    CSVPSAT                                                                            ! REAL function
        REAL    AlphaPT     , DeltaVP     , GammaPS     , LambdaLH                                 ! Added for formulation of Priestley-Taylor
        REAL    MSALB       , SRAD        , DTEMP       , ETPT        , EOP                        ! Added for formulation of Priestley-Taylor
        INTEGER hour                                                                                  ! Loop counter
        REAL ALBEDO
        
        SAVE
        
!     Transfer values from constructed data types into local variables.

         
!****************************************************************************************

!****************************************************************************************
        TDEW   = WEATHER % TDEW
        PARHR  = WEATHER % PARHR
        TMAX   = WEATHER % TMAX
        TMIN   = WEATHER % TMIN
        RADHR  = WEATHER % RADHR
        RHUMHR = WEATHER % RHUMHR
        TAIRHR = WEATHER % TAIRHR
        RADHR  = WEATHER % RADHR
        SNUP = WEATHER % SNUP
        SNDN   = WEATHER % SNDN
        SRAD   = WEATHER % SRAD
        MSALB  = SOILPROP% MSALB
        ! Reference ET calculated hourly using the Priestley-Taylor formulation based on the calculated hourly air temperature, TAIRHR
        AlphaPT = 1.26                                                               ! Priestley-Taylor advectivity constant, ?. 
                                                                                     ! Should be higher in semi-arid and arid environments.
        IF (LAI <= 0.0) THEN
          ALBEDO = MSALB
        ELSE
          ALBEDO = 0.23-(0.23-MSALB)*EXP(-0.75*LAI)
        ENDIF
        
        ! Calculate the hourly variables, and reference transpiration using formulae and constants from http://agsys.cra-cin.it/tools
        ! Integrate the hourly data for the day.
        ET0DAY = 0.0
        DO hour = 1,TS
            DeltaVP = 4098 * (0.6108 * EXP(17.27 * TAIRHR(hour)/(TAIRHR(hour) + 237.3)))/((TAIRHR(hour) + 237.3)**2)
                                                                                     ! Slope of the saturation vapor pressure–temperature relation, delta (kPa/°C).
            LambdaLH = 2.501 - 0.002361 * TAIRHR(hour)                                  ! Latent heat of vaporization of water, lambda (MJ/kg)
            GammaPS = 0.001013 * 101.325 / (0.622 * LambdaLH)                        ! Psychrometric constant, gamma (kPa/°C)
                                     ! The figure for the specific heat of moist air at constant pressure (Cp = 0.001013 MJ/kg/°C) is correct.
                                     ! GammaPS is affected by atmospheric pressure (101.325 kPa), so it should be adjusted to take account 
                                     ! of the site's elevation.
            ET0(hour) = (AlphaPT/(LambdaLH)) * (DeltaVP * ((RADHR(hour)*3.6/1000.) * &     ! RADHR is in J/m2/s. Multiply by 3600, divide by 10^6 for MJ/m2/hr.
                (1.0 - ALBEDO) - 0.0)) / (DeltaVP + GammaPS)
            ET0DAY = ET0DAY + ET0(hour)
        END DO
        ! Calculate total ET the day based on weighted mean temperature.
        ! For comparison only with hourly calculations.
        DTEMP = 0.5 * TMAX + 0.5 * TMIN 
        DeltaVP = 4098 * (0.6108 * EXP(17.27 * DTEMP/(DTEMP + 237.3)))/((DTEMP + 237.3)**2.0)
        LambdaLH = 2.501 - 0.002361 * DTEMP
        GammaPS = 0.001013 * 101.325 / (0.622 * LambdaLH)
        ETPT = (AlphaPT/(LambdaLH)) * (DeltaVP * ((SRAD) * (1.0 - ALBEDO) - 0.0)) / (DeltaVP + GammaPS) 
        
        ! Vapour pressure                                                            ! Code for VPD reponse moved from CS_Growth and modified for hourly response.
        
        !Hourly loop for VPD stomatal response
        
        IF (TDEW.LE.-98.0) TDEW = TMIN
        DO hour =1, TS
            VPDHR(hour) = (CSVPSAT(TAIRHR(hour)) - CSVPSAT(TDEW))/1000.0                   ! VPDHR = VPD, hourly (kPa) 
        END DO 
        
        
        ! If VPD > PHTV, reduce VPFPHR by the amount it exceeds PHVT (the threshold), scaled by PHSV (the slope).
        VPDFPHR = 1.0                                                                ! VPDFPHR = VPD factor, hourly (#) 
        VPDFPPREV = 1.0                                                              ! VPDFPREV = VPD factor, hourly, previous hour (#)
        VPDStartHr = 0.0
        VPDMaxHr = 0.0
        DO hour =1, TS
            IF (PHTV > 0.0) THEN                                                    ! If PHTV has a value (.NE. -99)
                IF (VPDHR(hour) > PHTV) THEN
                    VPDFPHR(hour) = AMIN1(1.0,AMAX1(0.0,1.0+PHSV*(VPDHR(hour)-PHTV)))
                    IF (hour > 1 .AND. VPDStartHr == 0.0) VPDStartHr = FLOAT(hour)    ! Hour start of stomatal closure in response to VPD
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
        
        ! Calculate mean VPD photosynthesis factor for the day 
        ! For comparison only with hourly calculations.
        INTEGVPDFPHR = 0.0
        DO hour =1, TS
            ! Adjust VPDFPHR for the fraction of hour at sunrise and sunset
            IF (hour == INT(SNUP))THEN 
                VPDFPHR(hour) = VPDFPHR(hour) * (REAL(hour+1) - SNUP) 
            ENDIF
            IF (hour == INT(SNDN)) THEN !DA can i assume it's >=
                VPDFPHR(hour) = VPDFPHR(hour) * (SNDN - REAL(hour))
            ENDIF
            INTEGVPDFPHR = INTEGVPDFPHR + VPDFPHR(hour)
        END DO
        IF (DAP >= 1) MNVPDFPHR = AMIN1(1.0, AMAX1(0.0, (INTEGVPDFPHR / (SNDN - SNUP))))      ! MNVPDFPHR is the integral of the hourly VPDFPHR divided by the hours from sunrise to sunset. Range 0-1.
        CONTINUE
        
        ! Adjust PT estimate of hourly ET by the VPD factor. Note this is on the whole ET, not the transpiration component.
        EOP = 0.0
        DO hour =1, TS
             ET0(hour) = ET0(hour) * VPDFPHR(hour)
             EOP = EOP + ET0(hour)
        END DO
        
        
        
    END  SUBROUTINE GROWTH_VPD
    
    !! END of Myles Fisher original code
    
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
