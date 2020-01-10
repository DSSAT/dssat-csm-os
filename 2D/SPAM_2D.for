C=======================================================================
C  COPYRIGHT 1998-2007 The University of Georgia, Griffin, Georgia
C                      University of Florida, Gainesville, Florida
C                      Iowa State University, Ames, Iowa
C                      International Center for Soil Fertility and 
C                       Agricultural Development, Muscle Shoals, Alabama
C                      University of Guelph, Guelph, Ontario
C  ALL RIGHTS RESERVED
C=======================================================================
C=======================================================================
C  SPAM_2D, Subroutine
C  Calculates soil-plant-atmosphere interface energy balance components.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  11/09/2001 WMB/CHP Split WATBAL into WATBAL and SPAM.
C  02/06/2003 KJB/CHP Added KEP, EORATIO inputs from plant routines.
C  06/19/2003 CHP Added KTRANS - used instead of KEP in TRANS.
C  04/01/2004 CHP/US Added Penman - Meyer routine for potential ET
!  10/24/2005 CHP Put weather variables in constructed variable. 
!  07/24/2006 CHP Use MSALB instead of SALB (includes mulch and soil 
!                 water effects on albedo)
!  08/25/2006 CHP Add SALUS soil evaporation routine, triggered by new
!                 FILEX parameter MESEV
!  12/09/2008 CHP Remove METMP
!  02/26/2009 CHP Modified and simplified for 2D
!=======================================================================

! Order of calculations for evapotranspiration:
!     RATE: 
!       1. SOIL - WATBAL2D (no ET calcs)
!       2. SPAM - Compute EOP, ES, TRUWP
!     INTEGRATION:
!       1. SOIL - Compute actual root water uptake and EP based on
!           detailed 2D, variable time step computations
!       2. SPAM - Summarize daily ET calculations

      SUBROUTINE SPAM_2D(CONTROL, ISWITCH,
     &    CELLS, CANHT, EORATIO, KSEVAP, KTRANS, PSTRES1, !Input
     &    PORMIN, RLV, RWUMX, SOILPROP, SoilProp_Furrow,  !Input
     &    SW, TRWU, WEATHER, XHLAI, XLAI,                 !Input
     &    FLOODWAT,                                       !I/O
     &    EO, EOP, EOS, EP, ES, SRFTEMP, ST,              !Output
     &    SWDELTX)                                        !Output

!-----------------------------------------------------------------------
      USE Cells_2D
      USE ModuleData
      USE FloodModule

      IMPLICIT NONE
      SAVE

      CHARACTER*1  IDETW, ISWWAT
      CHARACTER*1  MEEVP, MEPHO 
      CHARACTER*2  CROP
      CHARACTER*6, PARAMETER :: ERRKEY = "2DSPAM"

      INTEGER DYNAMIC

      REAL CANCOV, CANHT, CMSALB, CO2, MSALB
      REAL SRAD, TAVG, TMAX, TMIN, WINDSP, XHLAI, XLAI
      REAL CEF, CEM, CEO, CEP, CES, CET, EF, EM, EO, EP, ES, ET, EVAP,
     &    TRWU, TRWUP
      REAL EOS, EOP
      REAL XLAT, TAV, TAMP, SRFTEMP
      REAL EORATIO, KSEVAP, KTRANS

!!     Local variables in this routine.  Actual water stress variables 
!!     are calcualted in WatBal2D
!      REAL SWFAC, TURFAC

      REAL, DIMENSION(NL) :: ES_LYR, RLV, RWU, ST, SW, SWDELTX
      
      Type (CellType) CELLS(MaxRows,MaxCols)
      REAL, DIMENSION(MaxRows,MaxCols) :: ES_mm

!     Species-dependant variables imported from PLANT module:
      REAL PORMIN, RWUMX
      
!     P Stress on photosynthesis
      REAL PSTRES1

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SoilType)    SOILPROP, SoilProp_furrow
      TYPE (SwitchType)  ISWITCH
      TYPE (FloodWatType)FLOODWAT
      TYPE (WeatherType) WEATHER

!     Transfer values from constructed data types into local variables.
      CROP    = CONTROL % CROP
      DYNAMIC = CONTROL % DYNAMIC

      ISWWAT = ISWITCH % ISWWAT
      IDETW  = ISWITCH % IDETW
      MEEVP  = ISWITCH % MEEVP
      MEPHO  = ISWITCH % MEPHO

      CO2    = WEATHER % CO2
      SRAD   = WEATHER % SRAD  
      TAMP   = WEATHER % TAMP  
      TAV    = WEATHER % TAV   
      TAVG   = WEATHER % TAVG
      TMAX   = WEATHER % TMAX  
      TMIN   = WEATHER % TMIN  
      WINDSP = WEATHER % WINDSP
      XLAT   = WEATHER % XLAT  

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
!     We could use ETPHOT for photosynthesis (MEPHO = 'L', even if we don't 
!     use 'zonal' method for evaporation (MEEVP = 'Z'), so we need to keep  
!     this call to ETPHOT. SWFAC and TURFAC would only be computed by ETPHOT 
!     for the zonal method, which we can never have with the 2D model.  
!     So, the SWFAC and TURFAC values sent back from this routine should be
!     discarded.
      IF (MEPHO .EQ. 'L' .OR. MEEVP .EQ. 'Z') THEN
        CALL ETPHOT(CONTROL, ISWITCH,
     &    PORMIN, PSTRES1, RLV, RWUMX, SOILPROP, ST, SW,  !Input
     &    WEATHER, XLAI,                                 !Input
     &    EOP, EP, ES, RWU, TRWUP)                        !Output
      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      EF   = 0.0; CEF = 0.0
      EM   = 0.0; CEM = 0.0
      EO   = 0.0; CEO  = 0.0
      EP   = 0.0; CEP  = 0.0
      ES   = 0.0; CES  = 0.0
      ET   = 0.0; CET  = 0.0
      ES_LYR = 0.0
      SWDELTX = 0.0
      TRWU = 0.0
      EOP = 0.0
      EOS = 0.0

!     ---------------------------------------------------------
      CALL STEMP(CONTROL, ISWITCH,
     &    SOILPROP, SRAD, SW, TAVG, TMAX, XLAT, TAV, TAMP,!Input
     &    SRFTEMP, ST)                                    !Output

      CALL ESR_SoilEvap_2D(SEASINIT,
     &   CELLS, EOS, SOILPROP_FURROW,                     !Input
     &   ES, ES_mm)                                       !Output
     
      CALL TRANS(DYNAMIC, 
     &    CO2, CROP, EO, EVAP, KTRANS, TAVG, WINDSP, XHLAI, !Input
     &    EOP)                                            !Output

      IF (CROP .NE. 'FA') THEN
        IF (MEPHO .EQ. 'L') THEN
          CALL ETPHOT(CONTROL, ISWITCH,
     &    PORMIN, PSTRES1, RLV, RWUMX, SOILPROP, ST, SW,  !Input
     &    WEATHER, XLAI,                                  !Input
     &    EOP, EP, ES, RWU, TRWUP)                        !Output
        ENDIF
      ENDIF

      IF (IDETW .EQ. 'Y') THEN
        CALL OPSPAM(CONTROL, ISWITCH, FLOODWAT, TRWU,
     &    CEF, CEM, CEO, CEP, CES, CET, EF, EM, 
     &    EO, EOP, EOS, EP, ES, ET, TMAX, TMIN, TRWUP, SRAD,
     &    ES_LYR, SOILPROP)
      ENDIF

!     Transfer data to storage routine
      CALL PUT('SPAM', 'CEF', 0.0)
      CALL PUT('SPAM', 'CEM', 0.0)
      CALL PUT('SPAM', 'CEO', 0.0)
      CALL PUT('SPAM', 'CEP', 0.0)
      CALL PUT('SPAM', 'CES', 0.0)
      CALL PUT('SPAM', 'CET', 0.0)
      CALL PUT('SPAM', 'EF',  0.0)
      CALL PUT('SPAM', 'EM',  0.0)
      CALL PUT('SPAM', 'EO',  0.0)
      CALL PUT('SPAM', 'EP',  0.0)
      CALL PUT('SPAM', 'ES',  0.0)
      CALL PUT('SPAM', 'ET',  0.0)

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------

      SWDELTX = 0.0
      CALL STEMP(CONTROL, ISWITCH,
     &    SOILPROP, SRAD, SW, TAVG, TMAX, XLAT, TAV, TAMP,!Input
     &    SRFTEMP, ST)                                    !Output

!-----------------------------------------------------------------------
!     Potential evapotranspiration for combined bed and furrow
!     Albedo with canopy cover
      CANCOV = 1.0 - EXP(-KTRANS * XHLAI)
      MSALB = SOILPROP % MSALB
      CMSALB = CANCOV * 0.23 + (1.0 - CANCOV) * MSALB

      CALL PET(CONTROL, 
     &    CMSALB, XHLAI, MEEVP, WEATHER,  !Input for all
     &    EORATIO, !Needed by Penman-Monteith
     &    CANHT,   !Needed by dynamic Penman-Monteith
     &    EO)      !Output
     
!-----------------------------------------------------------------------
!     Potential soil evaporation for combined bed and furrow
      CALL PSE(EO, KSEVAP, XLAI, EOS)

!-----------------------------------------------------------------------
!     Switch off evaporation
      IF (ISWITCH % MESEV == 'N') THEN
        ES = 0.0
        ES_mm = 0.0
      ELSE
!     Actual soil evaporation 
        CALL ESR_SoilEvap_2D(RATE,
     &   CELLS, EOS, SOILPROP_FURROW,                     !Input
     &   ES, ES_mm)                                       !Output

      ENDIF
     
      CELLS%RATE%ES_Rate = ES_mm
      
!---------------------------------------------------------------------
!     Potential transpiration
      IF (XHLAI .GT. 0.0) THEN
        CALL TRANS(RATE, 
     &        CO2, CROP, EO, EVAP, KTRANS, TAVG, WINDSP, XHLAI, !Input
     &        EOP)                                            !Output
      ELSE
        EOP = 0.0
      ENDIF

!-----------------------------------------------------------------------
!     ALTERNATE CALL TO ENERGY BALANCE ROUTINES for PG
      IF (CROP .NE. 'FA') THEN
        IF (MEPHO .EQ. 'L' .AND. XHLAI .GT. 0.0) THEN
          CALL GET('SPAM','TRWUP',TRWUP)
          !ETPHOT called for photosynthesis only
          !    (MEPHO = 'L' and MEEVP <> 'Z')
          CALL ETPHOT(CONTROL, ISWITCH,
     &    PORMIN, PSTRES1, RLV, RWUMX, SOILPROP, ST, SW,  !Input
     &    WEATHER, XLAI,                                  !Input
     &    EOP, EP, ES, RWU, TRWUP)                        !Output
        ENDIF
      ENDIF

!-----------------------------------------------------------------------
!     Transfer data to storage routine
      CALL PUT('SPAM', 'EO',  EO)
      CALL PUT('SPAM', 'ES',  ES)
      
!***********************************************************************
!***********************************************************************
!     DAILY INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
      IF (ISWWAT .EQ. 'Y') THEN
        CALL GET('SPAM','TRWU',TRWU)
        CALL GET('SPAM','TRWUP',TRWUP)
        EP = TRWU * 10.
        ET  = ES  + EP
        CEO = CEO + EO
        CEP = CEP + EP
        CES = CES + ES
        CET = CET + ET
      ENDIF

      IF (IDETW .EQ. 'Y') THEN
        CALL OPSPAM(CONTROL, ISWITCH, FLOODWAT, TRWU,
     &    CEF, CEM, CEO, CEP, CES, CET, EF, EM, 
     &    EO, EOP, EOS, EP, ES, ET, TMAX, TMIN, TRWUP, SRAD,
     &    ES_LYR, SOILPROP)
      ENDIF

!     Transfer data to storage routine
      CALL PUT('SPAM', 'EP',  EP)
      CALL PUT('SPAM', 'CEO', CEO)
      CALL PUT('SPAM', 'CEP', CEP)
      CALL PUT('SPAM', 'CES', CES)
      CALL PUT('SPAM', 'CET', CET)
      CALL PUT('SPAM', 'ET',  ET)

!***********************************************************************
!***********************************************************************
!     OUTPUT - daily output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
!-----------------------------------------------------------------------
      CALL STEMP(CONTROL, ISWITCH,
     &    SOILPROP, SRAD, SW, TAVG, TMAX, XLAT, TAV, TAMP,!Input
     &    SRFTEMP, ST)                                    !Output

      CALL OPSPAM(CONTROL, ISWITCH, FLOODWAT, TRWU,
     &    CEF, CEM, CEO, CEP, CES, CET, EF, EM, 
     &    EO, EOP, EOS, EP, ES, ET, TMAX, TMIN, TRWUP, SRAD,
     &    ES_LYR, SOILPROP)

      IF (CROP .NE. 'FA' .AND. MEPHO .EQ. 'L') THEN
        CALL ETPHOT(CONTROL, ISWITCH,
     &    PORMIN, PSTRES1, RLV, RWUMX, SOILPROP, ST, SW,  !Input
     &    WEATHER, XLAI,                                  !Input
     &    EOP, EP, ES, RWU, TRWUP)                        !Output
      ENDIF

!***********************************************************************
!***********************************************************************
!     SEASEND - seasonal output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
      CALL OPSPAM(CONTROL, ISWITCH, FLOODWAT, TRWU,
     &    CEF, CEM, CEO, CEP, CES, CET, EF, EM, 
     &    EO, EOP, EOS, EP, ES, ET, TMAX, TMIN, TRWUP, SRAD,
     &    ES_LYR, SOILPROP)

      CALL STEMP(CONTROL, ISWITCH,
     &    SOILPROP, SRAD, SW, TAVG, TMAX, XLAT, TAV, TAMP,!Input
     &    SRFTEMP, ST)                                    !Output

      IF (MEPHO .EQ. 'L') THEN
        CALL ETPHOT(CONTROL, ISWITCH,
     &    PORMIN, PSTRES1, RLV, RWUMX, SOILPROP, ST, SW,  !Input
     &    WEATHER, XLAI,                                  !Input
     &    EOP, EP, ES, RWU, TRWUP)                        !Output
      ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE SPAM_2D

!-----------------------------------------------------------------------
!     VARIABLE DEFINITIONS: (updated 12 Feb 2004)
!-----------------------------------------------------------------------
! CANHT       Canopy height (m)
! CEF         Cumulative seasonal evaporation from floodwater surface (mm)
! CEM         Cumulative evaporation from surface mulch layer (mm)
! CEO         Cumulative potential evapotranspiration (mm)
! CEP         Cumulative transpiration (mm)
! CES         Cumulative evaporation (mm)
! CET         Cumulative evapotranspiration (mm)
! CLOUDS      Relative cloudiness factor (0-1) 
! CO2         Atmospheric carbon dioxide concentration
!              (�mol[CO2] / mol[air])
! CONTROL     Composite variable containing variables related to control 
!               and/or timing of simulation.    See Appendix A. 
! CROP        Crop identification code 
! DLAYR(L)    Thickness of soil layer L (cm)
! DUL(L)      Volumetric soil water content at Drained Upper Limit in soil 
!               layer L (cm3[water]/cm3[soil])
! EF          Evaporation rate from flood surface (mm / d)
! EM          Evaporation rate from surface mulch layer (mm / d)
! EO          Potential evapotranspiration rate (mm/d)
! EOP         Potential plant transpiration rate (mm/d)
! EORATIO     Ratio of increase in potential evapotranspiration with 
!               increase in LAI (up to LAI=6.0) for use with FAO-56 Penman 
!               reference potential evapotranspiration. 
! EOS         Potential rate of soil evaporation (mm/d)
! EP          Actual plant transpiration rate (mm/d)
! ES          Actual soil evaporation rate (mm/d)
! ET          Actual evapotranspiration rate (mm/d)
! FLOOD       Current depth of flooding (mm)
! FLOODWAT    Composite variable containing information related to bund 
!               management. Structure of variable is defined in 
!               ModuleDefs.for. 
! IDETW       Y=detailed water balance output, N=no detailed output 
! ISWITCH     Composite variable containing switches which control flow of 
!               execution for model.  The structure of the variable 
!               (SwitchType) is defined in ModuleDefs.for. 
! ISWWAT      Water simulation control switch (Y or N) 
! KSEVAP      Light extinction coefficient used for computation of soil 
!               evaporation 
! KTRANS      Light extinction coefficient used for computation of plant 
!               transpiration 
! LL(L)       Volumetric soil water content in soil layer L at lower limit
!              (cm3 [water] / cm3 [soil])
! MEEVP       Method of evapotranspiration ('P'=Penman, 
!               'R'=Priestly-Taylor, 'Z'=Zonal) 
! MEPHO       Method for photosynthesis computation ('C'=Canopy or daily, 
!               'L'=hedgerow or hourly) 
! NLAYR       Actual number of soil layers 
! RLV(L)      Root length density for soil layer L (cm[root] / cm3[soil])
! RWU(L)      Root water uptake from soil layer L (cm/d)
! MSALB       Soil albedo with mulch and soil water effects (fraction)
! SAT(L)      Volumetric soil water content in layer L at saturation
!              (cm3 [water] / cm3 [soil])
! SOILPROP    Composite variable containing soil properties including bulk 
!               density, drained upper limit, lower limit, pH, saturation 
!               water content.  Structure defined in ModuleDefs. 
! SRAD        Solar radiation (MJ/m2-d)
! SRFTEMP     Temperature of soil surface litter (�C)
! ST(L)       Soil temperature in soil layer L (�C)
! SUMES1      Cumulative soil evaporation in stage 1 (mm)
! SUMES2      Cumulative soil evaporation in stage 2 (mm)
! SW(L)       Volumetric soil water content in layer L
!              (cm3 [water] / cm3 [soil])
! SW_AVAIL(L) Soil water content in layer L available for evaporation, 
!               plant extraction, or movement through soil
!               (cm3 [water] / cm3 [soil])
! SWDELTS(L)  Change in soil water content due to drainage in layer L
!              (cm3 [water] / cm3 [soil])
! SWDELTU(L)  Change in soil water content due to evaporation and/or upward 
!               flow in layer L (cm3 [water] / cm3 [soil])
! SWDELTX(L)  Change in soil water content due to root water uptake in 
!               layer L (cm3 [water] / cm3 [soil])
! T           Number of days into Stage 2 evaporation (WATBAL); or time 
!               factor for hourly temperature calculations 
! TA          Daily normal temperature (�C)
! TAMP        Amplitude of temperature function used to calculate soil 
!               temperatures (�C)
! TAV         Average annual soil temperature, used with TAMP to calculate 
!               soil temperature. (�C)
! TAVG        Average daily temperature (�C)
! TDEW        Dew point temperature (�C)
! TGRO(I)     Hourly canopy temperature (�C)
! TGROAV      Average daily canopy temperature (�C)
! TMAX        Maximum daily temperature (�C)
! TMIN        Minimum daily temperature (�C)
! TRWU        Actual daily root water uptake over soil profile (mm/d)
! TRWUP       Potential daily root water uptake over soil profile (cm/d)
! U           Evaporation limit (cm)
! WINDSP      Wind speed at 2m (km/d)
! WINF        Water available for infiltration - rainfall minus runoff plus 
!               net irrigation (mm / d)
! XHLAI       Healthy leaf area index (m2[leaf] / m2[ground])
! XLAT        Latitude (deg.)
!-----------------------------------------------------------------------
!     END SUBROUTINE SPAM
!-----------------------------------------------------------------------

