C=======================================================================
C  COPYRIGHT 1998-2021 
C                      DSSAT Foundation
C                      University of Florida, Gainesville, Florida
C                      International Fertilizer Development Center
C                     
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
     &    CELLS, CANHT, EORATIO, KSEVAP, KTRANS, MULCH,   !Input
     &    PSTRES1, PORMIN, RLV, RWUMX, SOILPROP, SW,      !Input
     &    SoilProp_Furrow,                                !Input
     &    TRWU, WEATHER, XHLAI, XLAI,                     !Input
     &    FLOODWAT,                                       !I/O
     &    EO, EOP, EOS, EP, ES, SRFTEMP, ST,              !Output
     &    SWDELTX)                                        !Output

!-----------------------------------------------------------------------
      USE Cells_2D
      USE ModuleData
      USE FloodModule

      IMPLICIT NONE
      EXTERNAL ETPHOT, STEMP_EPIC, STEMP, ROOTWU, SOILEV, TRANS
      EXTERNAL MULCH_EVAP, OPSPAM, PET, PSE, FLOOD_EVAP, ESR_SOILEVAP
      EXTERNAL XTRACT, ROOTWU_HR, WATERSTRESS, ESR_SoilEvap_2D
      SAVE

      CHARACTER*1  IDETW, ISWWAT
      CHARACTER*1  MEEVP, MEINF, MEPHO, MESEV, METMP
      CHARACTER*2  CROP
      CHARACTER*6, PARAMETER :: ERRKEY = "2DSPAM"

      INTEGER DYNAMIC, NLAYR

      REAL CANCOV, CANHT, CMSALB, CO2, MSALB
      REAL SRAD, TAVG, TMAX, TMIN, WINDSP, XHLAI, XLAI
      REAL CEF, CEM, CEO, CEP, CES, CET, CEVAP 
      REAL EF, EM, EO, EP, ES, ET, EVAP 
      REAL TRWU, TRWUP, U
      REAL EOS, EOP, WINF
      REAL XLAT, TAV, TAMP, SRFTEMP
      REAL EORATIO, KSEVAP, KTRANS

!     Local variables in this routine.  Actual water stress variables 
!     are calcualted in WatBal2D
      REAL SWFAC, TURFAC

      REAL, DIMENSION(NL) :: ES_LYR, RLV, RWU, ST, SW, SWDELTX
      
      Type (CellType) CELLS(MaxRows,MaxCols)
      REAL, DIMENSION(MaxRows,MaxCols) :: ES_mm

      REAL DLAYR(NL), DUL(NL), LL(NL),   
     &    SAT(NL), SW_AVAIL(NL), UPFLOW(NL) 
!    &    SWAD(NL),SWDELTS(NL), SWDELTU(NL)

!     Root water uptake computed by some plant routines (optional)
!     REAL UH2O(NL)

!     Species-dependant variables imported from PLANT module:
      REAL PORMIN, RWUMX
!     Flood management variables:
      REAL EOS_SOIL
      
!     P Stress on photosynthesis
      REAL PSTRES1
!     Hourly transpiration for MEEVP=H      
      REAL, DIMENSION(TS)    :: ET0

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SoilType)    SOILPROP, SoilProp_furrow
      TYPE (SwitchType)  ISWITCH
      TYPE (FloodWatType)FLOODWAT
      TYPE (MulchType)   MULCH
      TYPE (WeatherType) WEATHER

!     Transfer values from constructed data types into local variables.
      CROP    = CONTROL % CROP
      DYNAMIC = CONTROL % DYNAMIC

      DLAYR  = SOILPROP % DLAYR
      DUL    = SOILPROP % DUL
      LL     = SOILPROP % LL
      MSALB  = SOILPROP % MSALB
      NLAYR  = SOILPROP % NLAYR
      SAT    = SOILPROP % SAT
      U      = SOILPROP % U

      ISWWAT = ISWITCH % ISWWAT
      IDETW  = ISWITCH % IDETW
      MEEVP  = ISWITCH % MEEVP
      MEINF  = ISWITCH % MEINF
      MEPHO  = ISWITCH % MEPHO
      METMP  = ISWITCH % METMP
      MESEV  = ISWITCH % MESEV

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
     &    WEATHER, XLAI,                                  !Input
     &    EOP, EP, ES, RWU, TRWUP)                        !Output
      ENDIF
      
      !Initialize ASCE dual KC ET variables (KRT)
      CALL PUT('SPAM', 'REFET', -99.0)
      CALL PUT('SPAM', 'KCB', -99.0)
      CALL PUT('SPAM', 'KE', -99.0)
      CALL PUT('SPAM', 'KC', -99.0)

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      EF   = 0.0; CEF  = 0.0
      EM   = 0.0; CEM  = 0.0
      EO   = 0.0; CEO  = 0.0
      EP   = 0.0; EOP  = 0.0; CEP  = 0.0
      ES   = 0.0; EOS  = 0.0; CES  = 0.0
      ET   = 0.0; CET  = 0.0
      EVAP = 0.0; CEVAP =0.0
      ES_LYR = 0.0
      SWDELTX = 0.0
      TRWU = 0.0
      XHLAI = 0.0
      ET0 = 0.0

!!     Soil water stress variables
!      SWFAC  = 1.0
!      TURFAC = 1.0
!
!      CALL GET('PLANT','RWUEP1',RWUEP1)

!     ---------------------------------------------------------
      IF (MEEVP .NE.'Z') THEN   !LPM 02dec14 use values from ETPHOT
        SELECT CASE (METMP)
        CASE ('E')    !EPIC soil temperature routine
          CALL STEMP_EPIC(CONTROL, ISWITCH,  
     &      SOILPROP, SW, TAVG, TMAX, TMIN, TAV, WEATHER,    !Input
     &      SRFTEMP, ST)                                     !Output
        CASE DEFAULT  !DSSAT soil temperature
          CALL STEMP(CONTROL, ISWITCH,
     &      SOILPROP, SRAD, SW, TAVG, TMAX, XLAT, TAV, TAMP, !Input
     &      SRFTEMP, ST)                                     !Output
        END SELECT
      ENDIF

!     ---------------------------------------------------------
          SELECT CASE(MESEV)
!         ------------------------
          CASE ('S')  ! Sulieman-Ritchie soil evaporation routine 
            CALL ESR_SoilEvap_2D(SEASINIT,
     &        CELLS, EOS_SOIL, SOILPROP_FURROW,      !Input
     &        ES, ES_mm)                        !Output
!         ------------------------
          CASE DEFAULT 
	  
!!!!!   NOTE CHP: NEED 2D SOILEV   !!!!!
!         CASE ('R')  !Ritchie soil evaporation routine
!           Calculate the availability of soil water for use in SOILEV.
!            DO L = 1, NLAYR
!              SW_AVAIL(L) = MAX(0.0, SW(L) + SWDELTS(L) + SWDELTU(L))
!            ENDDO
!            CALL SOILEV_2D(RATE,
!     &        CELLS, DLAYR, DUL, EOS_SOIL, LL, SW,      !Input
!     &        SW_AVAIL(1), U, WINF,                     !Input
!     &        ES, ES_mm)                                !Output
          END SELECT

!     ---------------------------------------------------------
      IF (MEEVP .NE. 'Z') THEN
        SELECT CASE (CONTROL % MESIC)
        CASE ('H')
          CALL ROOTWU_HR(SEASINIT,  
     &      DLAYR, EOP, ES_LYR, LL, NLAYR, RLV,           !Input
     &      SAT, SW, WEATHER,                             !Input
     &      EP, RWU, SWDELTX, SWFAC, TRWU, TRWUP, TURFAC) !Output
        CASE DEFAULT
          CALL ROOTWU(SEASINIT,
     &      DLAYR, LL, NLAYR, PORMIN, RLV, RWUMX, SAT, SW,!Input
     &      RWU, TRWUP)                                   !Output
        END SELECT

!       Initialize soil evaporation variables
        SELECT CASE (MESEV)
!     ----------------------------
        CASE ('R')  !Original soil evaporation routine
          CALL SOILEV(SEASINIT,
     &      DLAYR, DUL, EOS, LL, SW, SW_AVAIL(1),         !Input
     &      U, WINF,                                      !Input
     &      ES)                                           !Output
!     ----------------------------
        END SELECT

!       Initialize plant transpiration variables
        CALL TRANS(DYNAMIC, MEEVP, 
     &    CO2, CROP, EO, ET0, EVAP, KTRANS,               !Input
     &    WINDSP, XHLAI, WEATHER,                         !Input
     &    EOP)                                            !Output
      ENDIF

      CALL MULCH_EVAP(DYNAMIC, MULCH, EOS, EM)

!     ---------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
        IF (MEPHO .EQ. 'L' .OR. MEEVP .EQ. 'Z') THEN
          CALL ETPHOT(CONTROL, ISWITCH,
     &    PORMIN, PSTRES1, RLV, RWUMX, SOILPROP, ST, SW,  !Input
     &    WEATHER, XLAI,                                  !Input
     &    EOP, EP, ES, RWU, TRWUP)                        !Output
        ENDIF
      ENDIF

!     Call OPSPAM to open and write headers to output file
      IF (IDETW .EQ. 'Y') THEN
        CALL OPSPAM(CONTROL, ISWITCH, FLOODWAT, TRWU,
     &    CEF, CEM, CEO, CEP, CES, CET, CEVAP, EF, EM, 
     &    EO, EOP, EOS, EP, ES, ET, TMAX, TMIN, TRWUP, SRAD,
     &    ES_LYR, SOILPROP)
      ENDIF

!     Transfer data to storage routine
      CALL PUT('SPAM', 'CEF', CEF)
      CALL PUT('SPAM', 'CEM', CEM)
      CALL PUT('SPAM', 'CEO', CEO)
      CALL PUT('SPAM', 'CEP', CEP)
      CALL PUT('SPAM', 'CES', CES)
      CALL PUT('SPAM', 'CET', CET)
      CALL PUT('SPAM', 'CEVAP',CEVAP)
      CALL PUT('SPAM', 'EF',  EF)
      CALL PUT('SPAM', 'EM',  EM)
      CALL PUT('SPAM', 'EO',  EO)
      CALL PUT('SPAM', 'EP',  EP)
      CALL PUT('SPAM', 'ES',  ES)
      CALL PUT('SPAM', 'ET',  ET)

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------

      SWDELTX = 0.0
!     ---------------------------------------------------------
      IF (MEEVP .NE.'Z') THEN  !LPM 02dec14 use values from ETPHOT
        SELECT CASE (METMP)
        CASE ('E')    !EPIC soil temperature routine
          CALL STEMP_EPIC(CONTROL, ISWITCH,  
     &      SOILPROP, SW, TAVG, TMAX, TMIN, TAV, WEATHER,   !Input
     &      SRFTEMP, ST)                                    !Output
        CASE DEFAULT  
!       7/21/2016 - DSSAT method is default, per GH
!       CASE ('D')  !DSSAT soil temperature
          CALL STEMP(CONTROL, ISWITCH,
     &      SOILPROP, SRAD, SW, TAVG, TMAX, XLAT, TAV, TAMP,!Input
     &      SRFTEMP, ST)                                    !Output
        END SELECT
      ENDIF
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
     &    EO,      !Output
     &    ET0)     !Output hourly Priestly-Taylor with VPD effect
     
!-----------------------------------------------------------------------
!     Potential soil evaporation for combined bed and furrow
      CALL PSE(EO, KSEVAP, XLAI, EOS)

!-----------------------------------------------------------------------
!     ACTUAL SOIL, MULCH AND FLOOD EVAPORATION
!-----------------------------------------------------------------------
!     Initialize soil, mulch and flood evaporation
      ES = 0.; EM = 0.; EF = 0.; EVAP = 0.0
      UPFLOW = 0.0; ES_LYR = 0.0

!     Switch off evaporation
      IF (ISWITCH % MESEV == 'N') THEN
        ES = 0.0
        ES_mm = 0.0
      ELSE
!       Actual soil evaporation 
        EOS_SOIL = EOS
!       First meet evaporative demand from mulch
        IF (EOS_SOIL > 1.E-6 .AND. INDEX('RSM',MEINF) > 0) THEN
          CALL MULCH_EVAP(DYNAMIC, MULCH, EOS_SOIL, EM)
          IF (EOS_SOIL > EM) THEN
!           Some evaporative demand leftover for soil
            EOS_SOIL = EOS_SOIL - EM
          ELSE
            EOS_SOIL = 0.0
          ENDIF
        ENDIF

!       Soil evaporation after flood and mulch evaporation
        IF (EOS_SOIL > 1.E-6) THEN
          SELECT CASE(MESEV)
!         ------------------------
          CASE ('S')  ! Sulieman-Ritchie soil evaporation routine 
!           Note that this routine calculates UPFLOW, unlike the SOILEV.
            CALL ESR_SoilEvap_2D(RATE,
     &        CELLS, EOS_SOIL, SOILPROP_FURROW,      !Input
     &        ES, ES_mm)                        !Output
!         ------------------------
          CASE DEFAULT 
	  
!!!!!   NOTE CHP: NEED 2D SOILEV   !!!!!
!         CASE ('R')  !Ritchie soil evaporation routine
!           Calculate the availability of soil water for use in SOILEV.
!            DO L = 1, NLAYR
!              SW_AVAIL(L) = MAX(0.0, SW(L) + SWDELTS(L) + SWDELTU(L))
!            ENDDO
!            CALL SOILEV_2D(RATE,
!     &        CELLS, DLAYR, DUL, EOS_SOIL, LL, SW,      !Input
!     &        SW_AVAIL(1), U, WINF,                     !Input
!     &        ES, ES_mm)                                !Output
          END SELECT
!           ------------------------
        ENDIF
      ENDIF
     
      CELLS%RATE%ES_Rate = ES_mm
      
!     Total evaporation from soil, mulch, flood
      EVAP = ES + EM + EF

!---------------------------------------------------------------------
!     Potential transpiration
      IF (XHLAI > 1.E-6) THEN
        CALL TRANS(RATE, MEEVP, 
     &      CO2, CROP, EO, ET0, EVAP, KTRANS,           !Input
     &      WINDSP, XHLAI,                              !Input
     &      WEATHER,                                    !Input
     &      EOP)                                        !Output
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
          EVAP = ES  !CHP / BK 7/13/2017
        ENDIF
      ENDIF

!-----------------------------------------------------------------------
!     Transfer data to storage routine
      CALL PUT('SPAM', 'EF',  EF)
      CALL PUT('SPAM', 'EM',  EM)
      CALL PUT('SPAM', 'EO',  EO)
      CALL PUT('SPAM', 'EP',  EP)
      CALL PUT('SPAM', 'ES',  ES)
      CALL PUT('SPAM', 'EOP', EOP)
      CALL PUT('SPAM', 'EVAP',EVAP)
      CALL PUT('SPAM', 'UH2O',RWU)
      
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
        ET  = EVAP + EP
        CEF = CEF + EF
        CEM = CEM + EM
        CEO = CEO + EO
        CEP = CEP + EP
        CES = CES + ES
        CEVAP=CEVAP + EVAP
        CET = CET + ET
      ENDIF

      IF (IDETW .EQ. 'Y') THEN
        CALL OPSPAM(CONTROL, ISWITCH, FLOODWAT, TRWU,
     &    CEF, CEM, CEO, CEP, CES, CET, CEVAP, EF, EM, 
     &    EO, EOP, EOS, EP, ES, ET, TMAX, TMIN, TRWUP, SRAD,
     &    ES_LYR, SOILPROP)
      ENDIF

!     Transfer data to storage routine
      CALL PUT('SPAM', 'CEF', CEF)
      CALL PUT('SPAM', 'CEM', CEM)
      CALL PUT('SPAM', 'CEO', CEO)
      CALL PUT('SPAM', 'CEP', CEP)
      CALL PUT('SPAM', 'CES', CES)
      CALL PUT('SPAM', 'CET', CET)
      CALL PUT('SPAM', 'ET',  ET)
      CALL PUT('SPAM', 'CEVAP', CEVAP)

!***********************************************************************
!***********************************************************************
!     OUTPUT - daily output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
!-----------------------------------------------------------------------
!     ---------------------------------------------------------
      IF (MEEVP .NE.'Z') THEN  !LPM 02dec14 use values from ETPHOT
        SELECT CASE (METMP)
        CASE ('E')    !EPIC soil temperature routine
          CALL STEMP_EPIC(CONTROL, ISWITCH,  
     &      SOILPROP, SW, TAVG, TMAX, TMIN, TAV, WEATHER,   !Input
     &      SRFTEMP, ST)                                    !Output
        CASE DEFAULT  !DSSAT soilt temperature
          CALL STEMP(CONTROL, ISWITCH,
     &      SOILPROP, SRAD, SW, TAVG, TMAX, XLAT, TAV, TAMP,!Input
     &      SRFTEMP, ST)                                    !Output
        END SELECT
      ENDIF

      CALL OPSPAM(CONTROL, ISWITCH, FLOODWAT, TRWU,
     &    CEF, CEM, CEO, CEP, CES, CET, CEVAP, EF, EM, 
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
     &    CEF, CEM, CEO, CEP, CES, CET, CEVAP, EF, EM, 
     &    EO, EOP, EOS, EP, ES, ET, TMAX, TMIN, TRWUP, SRAD,
     &    ES_LYR, SOILPROP)

!     ---------------------------------------------------------
      IF (MEEVP .NE.'Z') THEN  !LPM 02dec14 use values from ETPHOT
        SELECT CASE (METMP)
        CASE ('E')    !EPIC soil temperature routine
          CALL STEMP_EPIC(CONTROL, ISWITCH,  
     &      SOILPROP, SW, TAVG, TMAX, TMIN, TAV, WEATHER,   !Input
     &      SRFTEMP, ST)                                    !Output
        CASE DEFAULT  !DSSAT soilt temperature
          CALL STEMP(CONTROL, ISWITCH,
     &      SOILPROP, SRAD, SW, TAVG, TMAX, XLAT, TAV, TAMP,!Input
     &      SRFTEMP, ST)                                    !Output
        END SELECT
      ENDIF

      IF (MEPHO .EQ. 'L') THEN
        CALL ETPHOT(CONTROL, ISWITCH,
     &    PORMIN, PSTRES1, RLV, RWUMX, SOILPROP, ST, SW,  !Input
     &    WEATHER, XLAI,                                  !Input
     &    EOP, EP, ES, RWU, TRWUP)                        !Output
      ENDIF

!     Transfer data to storage routine
      CALL PUT('SPAM', 'CEF', CEF)
      CALL PUT('SPAM', 'CEM', CEM)
      CALL PUT('SPAM', 'CEO', CEO)
      CALL PUT('SPAM', 'CEP', CEP)
      CALL PUT('SPAM', 'CES', CES)
      CALL PUT('SPAM', 'CET', CET)
      CALL PUT('SPAM', 'ET',  ET)
      CALL PUT('SPAM', 'CEVAP', CEVAP)

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
!              (µmol[CO2] / mol[air])
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

