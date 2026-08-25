C=======================================================================
C COPYRIGHT 1998-2025
C                     DSSAT Foundation
C                     University of Florida, Gainesville, Florida
C                     International Fertilizer Development Center
C                     
C ALL RIGHTS RESERVED
C=======================================================================
C=======================================================================
C  PLANT, Subroutine
C
C  This routine calls plant growth routines for all non-cropgro crops.
C
C  These plant growth routines are handled differently by the main
C     program than those of CROPGRO crops.
C
C     Standardized output values are initialized by this routine and
C     may or may not be computed by each individual plant growth module.
C
C     As additional crops are added, all will be called by this routine
C     rather than by the main program.
C-----------------------------------------------------------------------
C  Revision history
C
C  09/23/2001 CHP Written for modular potato model to be incorporated
C                   into CSM. (PT_SUBSTOR)
C  01/20/2002 WDB/CHP Added Ceres-Maize (MAIZE)
C  01/30/2002 LAH/CHP Added Wheat model (CSCERES)
C  04/17/2002 GH  Redefine module calls based on module name
C  07/15/2002 CHP Added rice module (US)
C  08/06/2002 WDB/CHP Added Ceres-Millet and Ceres-Sorghum
C  03/12/2003 CHP Changed senescence variable to composite (SENESCE)
C                   as defined in ModuleDefs.for
C  06/19/2003 CHP Added KTRANS (= KCAN + 0.15) for export to
C                   transpiration routine.
C  07/08/2003 CHP Export KSEVAP for use in soil evaporation routine.
C  10/08/2004 CHP Added GetPut_Iswitch call to push switch information
C                   into constructed variable which is accessible to
C                   all modules.
C  10/08/2004 CHP Removed some unused variables.
!  10/24/2005 CHP Put weather variables in constructed variable.
!  02/27/2006 CHP Changed Alt_Plant to PLANT and moved call to CROPGRO
!                 here.
!  07/13/2006 CHP Added P model to CROPGRO
!  02/06/2007 FSR added CASUPRO sugarcane
!  02/08/2007 LAH,CHP Added CSCRP (cassava)
!  07/15/2007 CHP, MJ added Canegro
!  09/10/2007 CHP, JIL added sweet corn
C  10/31/2007 US/RO/CHP Added TR_SUBSTOR (taro)
!  10/31/2007 CHP Added simple K model.
C  08/09/2012 GH  Added CSCAS model
!  04/16/2013 CHP/KAD Added SALUS model
!  05/09/2013 CHP/FR/JZW Added N-wheat module
!  06/03/2015 LPM Added CSYCA model (CIAT cassava)
!  05/10/2017 CHP removed SALUS model
!  12/01/2015 WDB added Sugarbeet
!  09/01/2018  MJ modified Canegro interface, IRRAMT added.
!  03/17/2020  WP Model TEFF from Mulugeta called on plant (added).
!  08/19/2021 FV Added OilcropSun
!  06/15/2022 CHP Added CropStatus
!  01/26/2023 CHP Reduce compile warnings: add EXTERNAL stmts, remove 
!                 unused variables, shorten lines. 
C=======================================================================

      SUBROUTINE PLANT(CONTROL, ISWITCH,
     &    EO, EOP, EOS, EP, ES, FLOODWAT, HARVFRAC,       !Input
     &    IRRAMT, NH4, NO3, SKi_Avail, SPi_AVAIL,         !Input
     &    SNOW, SOILPROP, SRFTEMP, ST, SW,                !Input
     &    TRWUP, WEATHER, YREND, YRPLT,                   !Input
     &    FLOODN,                                         !I/O
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,        !Output
     &    KUptake, MDATE, NSTRES, PSTRES1,                !Output
     &    PUptake, PORMIN, RLV, RWUMX, SENESCE,           !Output
     &    STGDOY, FracRts, UH2O, UNH4, UNO3, XHLAI, XLAI) !Output

!     2023-01-26 chp removed unused variables from argument list: 
!       TRWU, SomLitC, SomLitE, UPPM

C-----------------------------------------------------------------------
!     The following models are currently supported:
!         'CRGRO' - CROPGRO
!         'CSCER' - CERES Wheat, Barley
!         'CSCRP' - CropSim Wheat, Barley
!         'CSCAS' - CropSim/GumCAS Cassava
!         'CSYCA' - CIAT Cassava model
!         'MLCER' - CERES-Millet
!         'MZCER' - CERES-Maize
!         'PTSUB' - SUBSTOR-Potato
!         'RICER' - CERES-Rice
!         'SCCAN' - CANEGRO Sugarcane
!         'SCCSP' - CASUPRO Sugarcane
!         'SCSAM' - SAMUCA Sugarcane
!         'SGCER' - CERES-Sorghum
!         'SWCER' - CERES-Sweet corn
!         'MZIXM' - IXIM Maize
!         'TNARO' - Aroids - Tanier
!         'TRARO' - Aroids - Taro
!         'RIORZ' - IRRI ORYZA Rice model
!         'WHAPS' - APSIM N-wheat
!         'TFAPS' - APSIM Tef
!         'TFCER' - CERES Teff
!         'PRFRM' - Perennial forage model
!         'BSCER' - Sugarbeet
!         'SUOIL' - Sunflower (OilcropSun)
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
! Each plant module must compute SATFAC, SWFAC, and TURFAC
C-----------------------------------------------------------------------
      USE ModuleDefs
      USE ModuleData
      USE FloodModule

      IMPLICIT NONE
      EXTERNAL ALOHA_PINEAPPLE,BS_CERES,CROPGRO,CSCAS_INTERFACE,
     &  CSCERES_INTERFACE,CSCRP_INTERFACE,CSP_CASUPRO,CSYCA_INTERFACE,
     &  FIND,FORAGE,GETLUN,ML_CERES,MZ_CERES,PT_SUBSTOR,READ_ASCE_KT,
     &  RICE,SAMUCA,SC_CNGRO,SG_CERES,SU_CERES,SUMVALS,TEFF,TF_APSIM,
     &  TR_SUBSTOR,WARNING,WH_APSIM
      EXTERNAL INCDAT, ERROR

      SAVE

      CHARACTER*1  MEEVP, RNMODE
      CHARACTER*6  ERRKEY
      PARAMETER (ERRKEY = 'PLANTS')
      CHARACTER*8  MODEL
      CHARACTER*78 MESSAGE(10)    !Up to 10 lines of text to be output

      INTEGER DYNAMIC
      INTEGER RUN, CropStatus !, NVALP0
      INTEGER YREND, MDATE, YRPLT  !, YRSIM, YREMRG
      INTEGER STGDOY(20)

      REAL CANHT, CO2, DAYL, EO, EOP, EORATIO, EOS, EP, ES
      REAL KCAN, KEP, KSEVAP, KTRANS, LAI, NSTRES
      REAL PORMIN, RWUEP1, RWUMX, SRFTEMP, SNOW, IRRAMT
      REAL TMAX, TMIN !, TRWU
      REAL TRWUP, TWILEN, XLAI, XHLAI

      REAL, DIMENSION(2)  :: HARVFRAC
      REAL, DIMENSION(NL) :: NH4, NO3, RLV  !, RWU, UPPM
      REAL, DIMENSION(NL) :: ST, SW, UNO3, UNH4, UH2O

      LOGICAL FixCanht, BUNDED    !, CRGRO
c-----------------------------------------------------------------------
C         Variables needed to run ceres maize.....W.D.B. 12-20-01
      CHARACTER*2 CROP
      REAL    SRAD

!-----------------------------------------------------------------------
C         Variables to run CASUPRO from Alt_PLANT.  FSR 07-23-03
      REAL PAR, TAVG, TGROAV  !CHP 7/26/04 , TDAY
      REAL TGRO(TS)

	INTEGER, PARAMETER :: CanopyLayers=3
	REAL, DIMENSION(1:NumOfStalks,CanopyLayers) :: LFmntDEF
!     P model
      REAL, DIMENSION(NL) :: PUptake, SPi_AVAIL, FracRts
      REAL PSTRES1

!     K model
      REAL, DIMENSION(NL) :: KUptake, SKi_Avail

!     ORYZA Rice model
!      REAL, DIMENSION(0:NL) :: SomLitC
!      REAL, DIMENSION(0:NL,NELEM) :: SomLitE
!      LOGICAL, PARAMETER :: OR_OUTPUT = .FALSE.

!     Arrays which contain data for printing in SUMMARY.OUT file
      INTEGER, PARAMETER :: SUMNUM = 1
      CHARACTER*4, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

!-----------------------------------------------------------------------
!     Constructed variables are defined in ModuleDefs.
      TYPE (ControlType)  CONTROL
      TYPE (SwitchType)   ISWITCH
      TYPE (SoilType)     SOILPROP
      TYPE (ResidueType)  HARVRES
      TYPE (ResidueType)  SENESCE
      TYPE (FloodWatType) FLOODWAT
      TYPE (FloodNType)   FLOODN
      TYPE (WeatherType)  WEATHER

!     Transfer values from constructed data types into local variables.
      CROP    = CONTROL % CROP
      DYNAMIC = CONTROL % DYNAMIC
      MODEL   = CONTROL % MODEL
      RNMODE  = CONTROL % RNMODE
      RUN     = CONTROL % RUN

      MEEVP  = ISWITCH % MEEVP
      BUNDED = FLOODWAT % BUNDED
      CO2    = WEATHER % CO2
      DAYL   = WEATHER % DAYL
      PAR    = WEATHER % PAR
      SRAD   = WEATHER % SRAD
      TAVG   = WEATHER % TAVG
      TGRO   = WEATHER % TGRO
      TGROAV = WEATHER % TGROAV
      TMAX   = WEATHER % TMAX
      TMIN   = WEATHER % TMIN
      TWILEN = WEATHER % TWILEN

!***********************************************************************
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
!     Non-CROPGRO crops can not use MEPHO = 'L' or MEEVP = 'Z' at
!       this time.  If Species files are modified for these options
!       in the future, we need to make this check on a crop by crop basis.
!     The plant routines do not use these codes, but the SPAM module
!       does and it will bomb when species parameters are not found.
      IF (INDEX(MODEL,'CRGRO') <= 0 .and. index(model,'PRFRM') <= 0
     &  .AND. ISWITCH % MEPHO .EQ. 'L') THEN
        ISWITCH % MEPHO = 'C'
!       Put ISWITCH data where it can be retreived
!         by other modules as needed.
        CALL PUT(ISWITCH)

!       Write message to WARNING.OUT file
        WRITE(MESSAGE(1),110)
        WRITE(MESSAGE(2),120) CROP
        WRITE(MESSAGE(3),130)
        CALL WARNING(3, ERRKEY, MESSAGE)
      ENDIF

  110 FORMAT('You have specified use of the Leaf-level photosynthesis')
  120 FORMAT('option, which is not available for crop ', A2, '.')
  130 FORMAT('Canopy photosynthesis option will be used.')

      IF (INDEX(MODEL,'CRGRO') <= 0 .and. index(model,'PRFRM') <= 0
     &  .AND. ISWITCH % MEEVP .EQ. 'Z') THEN
!       Default to Priestly-Taylor potential evapotranspiration
        ISWITCH % MEEVP = 'R'
!       Put ISWITCH data where it can be retreived
!         by other modules as needed.
        CALL PUT(ISWITCH)

!       Write message to WARNING.OUT file
        WRITE(MESSAGE(1),210)
        WRITE(MESSAGE(2),220) CROP
        WRITE(MESSAGE(3),230)
        WRITE(MESSAGE(4),240)
        CALL WARNING(4, ERRKEY, MESSAGE)
      ENDIF

  210 FORMAT('You have specified use of the Zonal evapotranspiration')
  220 FORMAT('option, which is not available for crop ', A2, '.')
  230 FORMAT('The Priestly-Taylor potential evapo-transpiration ')
  240 FORMAT('option will be used.')

!---------------------------------------------------------------------
!     Print warning if "dynamic ET" routine is used - need canopy height
!     Need to modify this crop code list as canopy height routines are added.
      IF (MEEVP .EQ. 'D' .AND. INDEX('RIWHMLMZSGPTBA',CROP) .GT. 0) THEN
        WRITE(MESSAGE(1),310)
        WRITE(MESSAGE(2),320)
        WRITE(MESSAGE(3),330) CROP
        WRITE(MESSAGE(4),340)
        CALL WARNING(4, ERRKEY, MESSAGE)
        !Trigger to set canopy height upon emergence
        FixCanht = .TRUE.
      ENDIF

  310 FORMAT('You are using the Penman-Monteith potential evapo-')
  320 FORMAT('transpiration method, which requires canopy height.')
  330 FORMAT('The ',A2,' crop routine does not calculate this ')
  340 FORMAT('value.  A default value will be used.')

!     Initialize output variables.
!     Each plant routine may or may not re-compute these values.
      CANHT    = 0.0
      EORATIO  = 1.0
      KCAN     = 0.85
      KEP      = 1.0
      KSEVAP   = -99.
      KTRANS   = 1.0
      MDATE    = -99
      NSTRES   = 1.0
      !NVALP0   = 10000
      PORMIN   = 0.02
      RLV      = 0.0
      RWUEP1   = 1.5
      RWUMX    = 0.03
      !SENESCE % CumResWt= 0.0
      !SENESCE % CumResE = 0.0
      STGDOY   = 9999999
      XHLAI    = 0.0
      XLAI     = 0.0
      !YREMRG   = -99
      SENESCE % ResWt  = 0.0
      SENESCE % ResLig = 0.0
      SENESCE % ResE   = 0.0
      UNH4     = 0.0
      UNO3     = 0.0
      UH2O     = 0.0
      CropStatus = -99
      CONTROL % CropStatus = -99

      CALL READ_ASCE_KT(CONTROL, MEEVP)

!***********************************************************************
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!     If this is not a sequenced run, don't use any previously calculated
!       harvest residue.
!     This should be done by each plant routine, but in case not:
      IF (RUN .EQ. 1 .OR. INDEX('QF',RNMODE) .LE. 0) THEN
        HARVRES % RESWT  = 0.0
        HARVRES % RESLig = 0.0
        HARVRES % RESE   = 0.0
!        HARVRES % CumResWt= 0.0
!        HARVRES % CumResE = 0.0
      ENDIF

!     Initialize output variables.
!     Each plant routine may or may not re-compute these values.
      CANHT    = 0.0
!      EORATIO  = 1.0
      FracRts  = 0.0
!      KCAN     = 0.85
!      KEP      = 1.0
!      KSEVAP   = -99.
!      KTRANS   = 1.0
      KUptake = 0.0
      NSTRES   = 1.0
!      PORMIN   = 0.02
      PSTRES1  = 1.0
      PUPTAKE  = 0.0
      RLV      = 0.0
!      RWUEP1   = 1.5
!      RWUMX    = 0.03
      UH2O     = 0.0
      UNH4     = 0.0
      UNO3     = 0.0
      XHLAI    = 0.0
      XLAI     = 0.0
      SENESCE % ResWt  = 0.0
      SENESCE % ResLig = 0.0
      SENESCE % ResE   = 0.0
      CropStatus = -99
      CONTROL % CropStatus = -99

!***********************************************************************
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
      IF (CROP .NE. 'FA' .AND. 
     &    CONTROL % YRDOY .GE. YRPLT .AND. YRPLT .NE. -99) THEN

        SENESCE % ResWt  = 0.0
        SENESCE % ResLig = 0.0
        SENESCE % ResE   = 0.0

      ELSE
        CANHT = 0.0
        RLV   = 0.0
        XHLAI = 0.0
        XLAI  = 0.0
        RETURN
      ENDIF

!-----------------------------------------------------------------------
      ENDIF   !End of dynamic loop prior to calls to crop models
!-----------------------------------------------------------------------

!***********************************************************************
!***********************************************************************
!     Call crop models for all values of DYNAMIC:
      SELECT CASE (MODEL(1:5))
!-----------------------------------------------------------------------
!     CROPGRO model
      CASE('CRGRO')
        CALL CROPGRO(CONTROL, ISWITCH,
     &    EOP, HARVFRAC, NH4, NO3, SOILPROP, SPi_AVAIL,   !Input
     &    ST, SW, TRWUP, WEATHER, YREND, YRPLT,           !Input
     &    CANHT, CropStatus, EORATIO, HARVRES, KSEVAP,    !Output
     &    KTRANS, MDATE, NSTRES, PSTRES1,                 !Output
     &    PUptake, PORMIN, RLV, RWUMX, SENESCE,           !Output
     &    STGDOY, FracRts, UNH4, UNO3, XHLAI, XLAI)       !Output
!-----------------------------------------------------------------------
!     Forage model
      CASE('PRFRM')
        CALL FORAGE(CONTROL, ISWITCH,
     &    EOP, NH4, NO3, SOILPROP,                        !Input
     &    ST, SW, TRWUP, WEATHER, YREND, YRPLT,           !Input
     &    CANHT, EORATIO, HARVRES, MDATE,                 !Output
     &    NSTRES, PSTRES1, CropStatus,                    !Output
     &    PORMIN, RLV, RWUMX, SENESCE,                    !Output
     &    STGDOY, UNH4, UNO3, XHLAI, XLAI)                !Output

!     -------------------------------------------------
!     Wheat and Barley CSCER
      CASE('CSCER')
        CALL CSCERES_Interface (CONTROL, ISWITCH,          !Input
     &     EOP, YREND, NH4, NO3, SNOW, SOILPROP,           !Input
     &     SRFTEMP, ST, SW, TRWUP, WEATHER, YRPLT, HARVFRAC,!Input
     &     CANHT, HARVRES, KCAN, KEP, MDATE, NSTRES,PORMIN,!Output
     &     RLV, RWUMX, SENESCE, STGDOY, UNH4, UNO3, XLAI)  !Output

        IF (DYNAMIC .EQ. SEASINIT) THEN
          KTRANS = KEP
          KSEVAP = KEP
          XHLAI = XLAI
        ELSEIF (DYNAMIC .EQ. INTEGR) THEN
          XHLAI = XLAI
        ENDIF

!     -------------------------------------------------
!     Wheat and Barley CSCRP
      CASE('CSCRP')
        CALL CSCRP_Interface (CONTROL, ISWITCH,           !Input
     &    EOP, ES, NH4, NO3, SNOW, SOILPROP, SRFTEMP,     !Input
     &    ST, SW, TRWUP, WEATHER, YREND, YRPLT, HARVFRAC, !Input
     &    CANHT, HARVRES, KCAN, KEP, MDATE, NSTRES,       !Output
     &    PORMIN, RLV, RWUMX, SENESCE, STGDOY,            !Output
     &    UNH4, UNO3, XLAI)                               !Output
        IF (DYNAMIC .EQ. SEASINIT) THEN
          KTRANS = KEP
          KSEVAP = KEP
        ELSEIF (DYNAMIC .EQ. INTEGR) THEN
          XHLAI = XLAI
        ENDIF
!     -------------------------------------------------
!     Cassava CSCAS
      CASE('CSCAS')
        CALL CSCAS_Interface (CONTROL, ISWITCH,           !Input
     &    EOP, ES, NH4, NO3, SOILPROP, SRFTEMP,           !Input
     &    ST, SW, TRWUP, WEATHER, YREND, YRPLT, HARVFRAC, !Input
     &    CANHT, HARVRES, KCAN, KEP, MDATE, NSTRES,       !Output
     &    PORMIN, RLV, RWUMX, SENESCE, STGDOY,            !Output
     &    UNH4, UNO3, XLAI)                               !Output

        IF (DYNAMIC .EQ. SEASINIT) THEN
          KTRANS = KEP
          KSEVAP = KEP
        ELSEIF (DYNAMIC .EQ. INTEGR) THEN
          XHLAI = XLAI
        ENDIF
!     -------------------------------------------------
!     Cassava CSYCA (CIAT cassava model)
      CASE('CSYCA')
        CALL CSYCA_Interface (CONTROL, ISWITCH,           !Input
     &    EOP, NH4, NO3, SOILPROP, SRFTEMP,               !Input
     &    ST, SW, TRWUP, WEATHER, YREND, YRPLT, HARVFRAC, !Input
     &    CANHT, HARVRES, KCAN, KEP, MDATE, NSTRES,       !Output
     &    PORMIN, RLV, RWUMX, SENESCE, STGDOY,            !Output
     &    UNH4, UNO3, XLAI)                               !Output

        IF (DYNAMIC .EQ. SEASINIT) THEN
          KTRANS = KEP
          KSEVAP = KEP
        ELSEIF (DYNAMIC .EQ. INTEGR) THEN
          XHLAI = XLAI
        ENDIF

!     -------------------------------------------------
!     APSIM N-wheat WHAPS
      CASE('WHAPS')
        CALL WH_APSIM (CONTROL, ISWITCH,                  !Input
     &     EO, EOP, ES, HARVFRAC, NH4, NO3,               !Input
     &     SPi_AVAIL, SNOW,                               !Input
     &     SOILPROP, SW, TRWUP, WEATHER, YREND, YRPLT,    !Input
     &     CANHT, HARVRES, KCAN, KEP, KUptake, MDATE,     !Output
     &     NSTRES, PORMIN, PUptake, RLV,                  !Output
     &     RWUMX, SENESCE, STGDOY, FracRts,               !Output
     &     UNH4, UNO3, XLAI, XHLAI, UH2O, CropStatus)     !Output

        IF (DYNAMIC < RATE) THEN
!          KTRANS = KCAN + 0.15        !Or use KEP here??
          KTRANS = KEP        !KJB/WDB/CHP 10/22/2003
          KSEVAP = KEP
        ENDIF
!     -------------------------------------------------
!     APSIM Tef TFAPS
      CASE('TFAPS')
        CALL TF_APSIM (CONTROL, ISWITCH,                  !Input
     &     EO, EOP, ES, HARVFRAC, NH4, NO3,               !Input
     &     SPi_AVAIL, SNOW,                               !Input
     &     SOILPROP, SW, TRWUP, WEATHER, YREND, YRPLT,    !Input
     &     CANHT, HARVRES, KCAN, KEP, KUptake, MDATE,     !Output
     &     NSTRES, PORMIN, PUptake, RLV,                  !Output
     &     RWUMX, SENESCE, STGDOY, FracRts,               !Output
     &     UNH4, UNO3, XLAI, XHLAI, UH2O, CropStatus)     !Output

        IF (DYNAMIC < RATE) THEN
!          KTRANS = KCAN + 0.15        !Or use KEP here??
          KTRANS = KEP        !KJB/WDB/CHP 10/22/2003
          KSEVAP = KEP
        ENDIF

!     -------------------------------------------------
!     Millet
      CASE('MLCER')
        CALL ML_CERES (CONTROL, ISWITCH,
     &     CO2, DAYL, EOP, HARVFRAC, NH4, NO3,            !Input
     &     SNOW, SOILPROP, SRAD, SW, TMAX, TMIN,          !Input
     &     TRWUP, TWILEN, YREND, YRPLT,                   !Input
     &     CropStatus,                                    !Output
     $     CANHT, HARVRES, MDATE, NSTRES, PORMIN, RLV,    !Output
     &     RWUMX, SENESCE, STGDOY, UNO3, UNH4, XLAI,      !Output
     &     KCAN, KEP)                                     !Output

        IF (DYNAMIC .EQ. SEASINIT) THEN
!          KTRANS = KCAN + 0.15        !Or use KEP here??
          KTRANS = KEP        !KJB/WDB/CHP 10/22/2003
          KSEVAP = KEP
        ELSEIF (DYNAMIC .EQ. INTEGR) THEN
          XHLAI = XLAI
        ENDIF

!     -------------------------------------------------
!     Maize, Sweetcorn
      CASE('MZCER','MZIXM','SWCER')
        CALL MZ_CERES (CONTROL, ISWITCH,                  !Input
     &     EOP, HARVFRAC, NH4, NO3, SKi_Avail,            !Input
     &     SPi_AVAIL, SNOW,                               !Input
     &     SOILPROP, SW, TRWUP, WEATHER, YREND, YRPLT,    !Input
     &     CropStatus,                                    !Output
     &     CANHT, HARVRES, KCAN, KEP, KUptake, MDATE,     !Output
     &     NSTRES, PORMIN, PUptake, RLV, RWUMX, SENESCE,  !Output
     &     STGDOY, FracRts, UNH4, UNO3, XLAI, XHLAI)      !Output

        IF (DYNAMIC < RATE) THEN
!          KTRANS = KCAN + 0.15        !Or use KEP here??
          KTRANS = KEP        !KJB/WDB/CHP 10/22/2003
          KSEVAP = KEP
        ENDIF

!     -------------------------------------------------
!     Sugarbeet
      CASE('BSCER')
        CALL BS_CERES (CONTROL, ISWITCH,                  !Input
     &     EOP, HARVFRAC, NH4, NO3, SPi_AVAIL, SNOW,      !Input
     &     SOILPROP, SW, TRWUP, WEATHER, YREND, YRPLT,    !Input
     &     CANHT, CropStatus, HARVRES, KCAN, KEP, MDATE,  !Output
     &     NSTRES, PORMIN, PUptake, RLV, RWUMX, SENESCE,  !Output
     &     STGDOY, FracRts,XLAI, XHLAI)          !Output

        IF (DYNAMIC < RATE) THEN
          KTRANS = KEP        !KJB/WDB/CHP 10/22/2003
          KSEVAP = KEP
        ENDIF

!     -------------------------------------------------
!     Potato
      CASE('PTSUB')
        CALL PT_SUBSTOR(CONTROL, ISWITCH,
     &    CO2, EOP, HARVFRAC, NH4, NO3, SOILPROP, SRAD,   !Input
     &    ST, SW, TMAX, TMIN, TRWUP, TWILEN, YREND, YRPLT,!Input
     &    CANHT, HARVRES, MDATE, NSTRES, PORMIN, RLV,     !Output
     &    RWUMX, SENESCE, STGDOY, UNH4, UNO3, XLAI)       !Output

        IF (DYNAMIC .EQ. INTEGR) THEN
          XHLAI = XLAI
        ENDIF

!     -------------------------------------------------
!     Rice
      CASE('RICER')
        CALL RICE(CONTROL, ISWITCH,
     &    CO2, DAYL, EOP, FLOODWAT, HARVFRAC, NH4, NO3,   !Input
     &    SKi_Avail, SPi_AVAIL,                           !Input
     &    SOILPROP, SRAD, ST, SW, TMAX, TMIN, TRWUP,      !Input
     &    TWILEN, YRPLT,                                  !Input
     &    FLOODN,                                         !I/O
     &    CANHT, HARVRES, XLAI, KUptake, MDATE, NSTRES,   !Output
     &    PORMIN, PUptake, RWUEP1, RWUMX, CropStatus,     !Output
     &    RLV, SENESCE, STGDOY, FracRts, UNH4, UNO3)      !Output

        IF (DYNAMIC .EQ. INTEGR) THEN
          XHLAI = XLAI
        ENDIF

!     -------------------------------------------------
!     CERES-TEFF
      CASE('TFCER')
        CALL TEFF(CONTROL, ISWITCH,
     &    CO2, DAYL, EOP, FLOODWAT, HARVFRAC, NH4, NO3,   !Input
     &    SKi_Avail, SPi_AVAIL,                           !Input
     &    SOILPROP, SRAD, ST, SW, TMAX, TMIN, TRWUP,      !Input
     &    TWILEN, YRPLT,                                  !Input
     &    FLOODN,                                         !I/O
     &    CANHT, HARVRES, XLAI, KUptake, MDATE, NSTRES,   !Output
     &    PORMIN, PUptake, RWUEP1, RWUMX, CropStatus,     !Output
     &    RLV, SENESCE, STGDOY, FracRts, UNH4, UNO3)      !Output

        IF (DYNAMIC .EQ. INTEGR) THEN
          XHLAI = XLAI
        ENDIF

!!     -------------------------------------------------
!!     ORYZA2000 Rice
!      CASE('RIORZ')
!        CALL ORYZA_Interface (CONTROL, ISWITCH,                  !Input
!     &   EOP, FLOODWAT, HARVFRAC, NH4, NO3, SOILPROP,            !Input
!     &   SomLitC, SomLitE,                                       !Input
!     &   ST, SW, TRWUP, UPPM, WEATHER, YRPLT, YREND, OR_OUTPUT,  !Input
!     &   CANHT, HARVRES, KTRANS, KSEVAP, MDATE, NSTRES, PORMIN,  !Output
!     &   RLV, RWUMX, SENESCE, STGDOY, UNH4, UNO3, UH2O, XLAI)    !Output
!
!        IF (DYNAMIC .EQ. INTEGR) THEN
!          XHLAI = XLAI
!        ENDIF

!!     -------------------------------------------------
!!	Generic Salus crop model
!!	KD 09/14/2009
!	CASE('SALUS')
!	  CALL SALUS(CONTROL, ISWITCH, WEATHER, SOILPROP, ST,         !Input
!     &  HARVFRAC, YRPLT, EOP, SW, RWU, TRWUP, NH4, NO3, SPi_AVAIL,  !Input
!     &  KCAN, MDATE, RLV, XHLAI, UNO3, UNH4, PUptake)  	            !Output
!	  IF (DYNAMIC .EQ. INTEGR) THEN
!          XLAI = XHLAI
!        ENDIF

!     -------------------------------------------------
!     Sugarcane - CANEGRO
!     :::::::::::::::::::
!     Matthew Jones, 2006-09-20
!     :::::::::::::::::::::::::
      CASE('SCCAN')
      !  MJ Added IRRAMT July 2015
      !  MJ Added ES July 2015
      !  MJ added SATFAC Jan 2018
      !  HBD (Jan 2023) added RWU after MvdL 2011
        CALL SC_CNGRO (
     &    CONTROL, ISWITCH,                           !Input
     &    CO2, EOP, EP, EO, ES, HARVFRAC, NH4, NO3,   !Input
     &    SOILPROP, SW, TMAX, TMIN, TRWUP, EOS,       !Input
     &    RWUEP1, YREND, YRPLT, WEATHER, IRRAMT,      !Input
     &    CANHT, KCAN, KTRANS, MDATE, NSTRES,         !Output
     &    PORMIN, RLV, RWUMX,STGDOY, UNH4,            !Output
     &    UNO3, XLAI, XHLAI, EORATIO,SENESCE)         !Output

c     Added by MJ, 2007-04-04:
c     ::::::::::::::::::::::::
c     Total LAI must exceed or be equal to healthy LAI:
          XLAI = MAX(XLAI, XHLAI)
!     -------------------------------------------------
!     Sugarcane - SAMUCA
      CASE('SCSAM')
          call SAMUCA(
     &    CONTROL, ISWITCH,                               !Input
     &    CO2, DAYL, EOP,                                 !Input
     &    SOILPROP, ST, SRAD, TMAX, TMIN, TRWUP,          !Input
     &    RWUEP1, YREND, YRPLT, WEATHER,                  !Input
     $    CANHT, KCAN, KTRANS, MDATE, NSTRES,             !Output
     &    RLV, RWUMX, STGDOY,                             !Output
     &    XLAI, XHLAI, EORATIO)                           !Output
          
!     -------------------------------------------------
!     Sugarcane - CASUPRO
      CASE('SCCSP')
        CALL CSP_CASUPRO(CONTROL, ISWITCH,
     &    CO2, EOP, EOS, HARVFRAC, NH4, NO3, PAR,           !Input
     &    SOILPROP, SPi_AVAIL, SW, TAVG, TGRO,              !Input
     &    TGROAV, TMIN, TRWUP, WEATHER, YREND, YRPLT,       !Input
     &    CANHT, EORATIO, HARVRES, KTRANS, LFmntDEF, MDATE, !Output
     &    NSTRES, PUptake, PORMIN, RLV, RWUMX, SENESCE,     !Output
     &    STGDOY, FracRts, UNH4, UNO3, XHLAI, XLAI)         !Output

!     -------------------------------------------------
!     Sorghum
      CASE('SGCER')
        CALL SG_CERES (CONTROL, ISWITCH,
     &     CO2, DAYL, EOP, HARVFRAC, NH4, NO3,                  !Input
     &     SNOW, SOILPROP, SPi_AVAIL, SRAD, SW, TMAX, TMIN,     !Input
     &     TRWUP, TWILEN, YREND, YRPLT,                         !Input
     &     CANHT, HARVRES, MDATE, NSTRES, PORMIN, PUptake,      !Output
     &     RLV, RWUMX, SENESCE, STGDOY, UNO3, UNH4,             !Ouput
     &     XLAI, KCAN, KEP, FracRts, CropStatus)                !Output

        IF (DYNAMIC .EQ. SEASINIT) THEN
!          KTRANS = KCAN + 0.15        !Or use KEP here??
          KTRANS = KEP        !KJB/WDB/CHP 10/22/2003
          KSEVAP = KEP
        ELSEIF (DYNAMIC .EQ. INTEGR) THEN
          XHLAI = XLAI
        ENDIF

!     -------------------------------------------------
!     Sunflower
      CASE('SUOIL')
        CALL SU_CERES (CONTROL, ISWITCH,              !Input
     &     EOP, HARVFRAC, NH4, NO3, SKi_Avail,            !Input
     &     SPi_AVAIL,                                     !Input
     &     SOILPROP, SW, TRWUP, WEATHER, YREND, YRPLT,    !Input
     &     CANHT, HARVRES, KCAN, KEP,KUptake,  MDATE,     !Output
     &     NSTRES, PORMIN, PUptake, RLV, RWUMX, SENESCE,  !Output
     &     STGDOY, FracRts, UNH4, UNO3, XLAI, XHLAI)      !Output

        IF (DYNAMIC < RATE) THEN
          KTRANS = KEP        !KJB/WDB/CHP 10/22/2003
          KSEVAP = KEP
        ENDIF
        
!     -------------------------------------------------
!     Aroids-taro
      CASE('TRARO','TNARO')
        CALL TR_SUBSTOR(CONTROL, ISWITCH,
     &    CO2, DAYL, EOP, FLOODWAT, HARVFRAC, NH4, NO3,   !Input
     &    SOILPROP, SRAD, ST, SW, TMAX, TMIN, TRWUP,      !Input
     &    YRPLT,                                          !Input
     &    FLOODN,                                         !I/O
     &    CropStatus,                                     !Output
     &    CANHT, HARVRES, XLAI, MDATE, NSTRES, PORMIN,    !Output
     &    RWUEP1, RWUMX, RLV, SENESCE, STGDOY, UNH4, UNO3)!Output

        IF (DYNAMIC .EQ. INTEGR) THEN
          XHLAI = XLAI
        ENDIF

!     -------------------------------------------------
!     Pineapple - Aloha model
      CASE('PIALO')
        CALL Aloha_Pineapple(CONTROL, ISWITCH,
     &    EOP, HARVFRAC, NH4, NO3, SOILPROP, SW, TRWUP,   !Input
     &    WEATHER, YRPLT,                                 !Input
     &    LAI, MDATE, RLV, SENESCE, STGDOY, UNH4, UNO3)   !Output

        XLAI  = LAI
        XHLAI = LAI
!     -------------------------------------------------
      END SELECT

!     If the crop status (local value) from crop models was given a value, use it.
!     Crop status may come in from other routines such as auto-planting and auto-harvest.
!     Some crops may modify the CONTROL % CropStatus directly.
      IF (CropStatus > 0) THEN
        CONTROL % CropStatus = CropStatus
        CALL PUT(CONTROL)
      ENDIF

!***********************************************************************
!***********************************************************************
!     Processing after calls to crop models:
!-----------------------------------------------------------------------
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
! Zero the value of HARVRES composite variable here
!!!NOTE: At this time, the variable has already been used to
!     initialize soil properties for this season.
!  This should be done by each plant routine, but in case not:
        HARVRES % RESWT  = 0.0
        HARVRES % RESLig = 0.0
        HARVRES % RESE   = 0.0

!***********************************************************************
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
!     Set default canopy height upon emergence (or first day with
!       LAI.  Should actually set these defaults within each
!       crop routine.
        IF (FixCanht .AND. (XLAI .GT. 0.0 .OR. XHLAI .GT. 0.0)) THEN
          CANHT = 0.5
          FixCanht = .FALSE.
        ENDIF

!***********************************************************************
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
!     Store Summary.out labels and values in arrays to send to
!     OPSUM routines for printing.  Integers are temporarily 
!     saved as real numbers for placement in real array.
      LABEL(1)  = 'CRST'; VALUE(1)  = CONTROL % CropStatus

      !Send labels and values to OPSUM
      CALL SUMVALS (SUMNUM, LABEL, VALUE) 

! End of season crop status codes:
! CRST - Definition                               Status **
!    1 - crop matured normally                    NORMAL
!    2 - crop harvested on reported date          NORMAL
!    3 - crop harvested at reported growth stage  NORMAL
!    6 - auto-harvest within window               NORMAL
!   11 - failure to plant (automatic planting)    NO_SOW
!   12 - failure to germinate                     NOGERM
!   13 - failure to emerge                        NOEMRG
!   21 - crop mature due to slow grain filling    SLOGRN 
!   31 - crop died due to heat stress             HOT
!   32 - crop died due to cold stress             COLD
!   33 - crop died due to deficit water stress    DRY
!   34 - crop died due to excess water stress     WET
!   39 - crop died due to excess stress           STRESS
!   51 - crop died due to pest damage             PEST

!  100 – crop season length exceeded limits       SEASON
!  200 – weather data error                       WEATHER
!  999 – unspecified error condition              UNKNOWN

! ** Status text could be used in Summary.OUT if we want this to be more human readable.

!***********************************************************************
      ENDIF

!***********************************************************************
      RETURN
      END SUBROUTINE PLANT

!===========================================================================
! Variable listing for Alt_Plant - updated 08/18/2003
! --------------------------------------------------------------------------
! CANHT     Canopy height (m)
! CO2       Atmospheric carbon dioxide concentration (µmol[CO2] / mol[air])
! CONTROL   Composite variable containing variables related to control
!             and/or timing of simulation.  The structure of the variable
!             (ControlType) is defined in ModuleDefs.for.
! CROP      Crop identification code
! DAYL      Day length on day of simulation (from sunrise to sunset) (hr)
! EOP       Potential plant transpiration rate (mm/d)
! EORATIO   Ratio of increase in EO with increase in LAI (up to LAI=6.0)
!             for use with FAO-56 Penman reference EO.
! ERRKEY    Subroutine name for error file
! FIXCANHT  Logical variable, =TRUE if default canopy height is to be set
!             by Alt_Plant routine upon emergence
! FLOODN    Composite variable which contains flood nitrogen mass and
!             concentrations. Structure of variable is defined in
!             ModuleDefs.for. (var.)
! FLOODWAT  Composite variable containing information related to bund
!             management. Structure of variable is defined in ModuleDefs.for.
! HARVFRAC  Two-element array containing fractions of (1) yield harvested
!             and (2) by-product harvested (fraction)
! HARVRES   Composite variable containing harvest residue amounts for total 
!             dry matter, lignin, and N amounts.  Structure of variable is 
!             defined in ModuleDefs.for. 
! ISWITCH   Composite variable containing switches which control flow of 
!             execution for model.  The structure of the variable 
!             (SwitchType) is defined in ModuleDefs.for. 
! IRRAMT    Irrigation amount (mm)
! KCAN      Canopy light extinction coefficient for daily PAR, for 
!             equidistant plant spacing, modified when in-row and between 
!             row spacing are not equal 
! KEP       Energy extinction coefficient for partitioning EO to EP 
! KSEVAP    Light extinction coefficient used for computation of soil 
!             evaporation 
! KTRANS    Light extinction coefficient used for computation of plant 
!             transpiration 
! MDATE     Harvest maturity date (YYYYDDD)
! MEEVP     Method of evapotranspiration ('P'=Penman, 'R'=Priestly-Taylor,
!             'Z'=Zonal)
! MESSAGE   Text array containing information to be written to WARNING.OUT
!             file.
! MODEL     Name of CROPGRO executable file
! NH4(L)    Ammonium N in soil layer L (µg[N] / g[soil])
! NL        Maximum number of soil layers = 20
! NO3(L)    Nitrate in soil layer L (µg[N] / g[soil])
! NSTRES    Nitrogen stress factor (1=no stress, 0=max stress)
! NVALP0    Set to 100,000 in PHENOLOG, used for comparison of times of
!             plant stages (d)
! PORMIN    Minimum pore space required for supplying oxygen to roots for
!             optimal growth and function (cm3/cm3)
! RLV(L)    Root length density for soil layer L (cm[root] / cm3[soil])
! RNMODE    Simulation run mode (I=Interactive, A=All treatments,
!             B=Batch mode, E=Sensitivity, D=Debug, N=Seasonal, Q=Sequence)
! RUN       Change in date between two observations for linear
!             interpolation
! RWUEP1    Threshold for reducing leaf expansion compared w/ ratio of
!             TRWU/EP1 (total potential daily root water uptake/ actual
!             transpiration)
! RWUMX     Maximum water uptake per unit root length, constrained by soil
!             water (cm3[water] / cm [root])
! SENESCE   Composite variable containing data about daily senesced plant
!             matter. Structure of variable is defined in ModuleDefs.for
! SNOW      Snow accumulation (mm)
! SOILPROP  Composite variable containing soil properties including bulk
!             density, drained upper limit, lower limit, pH, saturation
!             water content.  Structure defined in ModuleDefs.
! SRAD      Solar radiation (MJ/m2-d)
! ST(L)     Soil temperature in soil layer L (°C)
! STGDOY(I) Day when plant stage I occurred (YYYYDDD)
! SW(L)     Volumetric soil water content in layer L
!            (cm3 [water] / cm3 [soil])
! TMAX      Maximum daily temperature (°C)
! TMIN      Minimum daily temperature (°C)
! TRWUP     Potential daily root water uptake over soil profile (cm/d)
! TWILEN    Daylength from twilight to twilight (h)
! UNH4(L)   Rate of root uptake of NH4, computed in NUPTAK
!            (kg [N] / ha - d)
! UNO3(L)   Rate of root uptake of NO3, computed in NUPTAK (kg [N] / ha -d)
! XHLAI     Healthy leaf area index (m2[leaf] / m2[ground])
! XLAI      Leaf area (one side) per unit of ground area
!            (m2[leaf] / m2[ground])
! YREMRG    Day of emergence (YYYYDDD)
! YREND     Date for end of season (usually harvest date) (YYYYDDD)
! YRPLT     Planting date (YYYYDDD)
!===========================================================================

!===========================================================================
      SUBROUTINE READ_ASCE_KT(CONTROL, MEEVP)
!     Generic routine to read evapotranspiration species parameters
!     KEP, EORATIO 
!     TSKC, TKCBmax ASCE tall ref (50 cm alfalfa)
!     SSKC, SKCBmax ASCE short ref (12 cm grass)

      USE ModuleData
      External IGNORE, WARNING, ERROR, GETLUN, FIND

      CHARACTER*1  BLANK, MEEVP
      PARAMETER (BLANK  = ' ')

      CHARACTER*2 CROP
      CHARACTER*6 SECTION
      CHARACTER*6  ERRKEY
      PARAMETER (ERRKEY = 'IPASCE')

      CHARACTER*12 FILEC
      CHARACTER*30 FILEIO
      CHARACTER*78 MSG(10)
      CHARACTER*80 PATHCR, CHAR
      CHARACTER*92 FILECC

      INTEGER LUNCRP, LUNIO, NMSG
      INTEGER PATHL, FOUND, ERR, LINC, LNUM, ISECT

!     Species-dependant variables exported to SPAM:
      REAL KEP, EORATIO, SSKC, SKCBMAX, TSKC, TKCBMAX

!     The variable "CONTROL" is of constructed type "ControlType" as 
!     defined in ModuleDefs.for, and contains the following variables.
!     The components are copied into local variables for use here.
      TYPE (ControlType) CONTROL

      IF (MEEVP .NE. 'S' .AND. MEEVP .NE. 'T') RETURN

      NMSG = 0

!-----------------------------------------------------------------------
!     Read file plus path for species file 
!-----------------------------------------------------------------------
      FILEIO = CONTROL % FILEIO
      LUNIO  = CONTROL % LUNIO
      CROP   = CONTROL % CROP
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
      READ(LUNIO,50,IOSTAT=ERR) FILEC, PATHCR ; LNUM = 7
   50 FORMAT(6(/),15X,A12,1X,A80)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
      CLOSE (LUNIO)

!-----------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
!       open species file
        LNUM = 0
        PATHL  = INDEX(PATHCR,BLANK)
        IF (PATHL .LE. 1) THEN
          FILECC = FILEC
        ELSE
          FILECC = PATHCR(1:(PATHL-1)) // FILEC
        ENDIF
        CALL GETLUN('FILEC', LUNCRP)
        OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)
        LNUM = 0

!       ***** READ ASCE EVAPOTRANSPIRATION PARAMETERS *****
        SECTION = '!*EVAP'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND .EQ. 0) THEN
          NMSG = NMSG + 1
          MSG(NMSG) = "*EVAP section missing from species file."
          GOTO 100
        ELSE
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          !KEP and EORATIO are not used in ASCE PET method,
          !but must be read in prior to ASCE parameters.
          READ(CHAR,'(2F6.0)',IOSTAT=ERR) KEP, EORATIO
          IF (ERR .NE. 0) THEN
            NMSG = NMSG + 1
            MSG(NMSG)="Error reading KEP and EORATIO."
          ENDIF
        ENDIF
        
!       Read short reference crop parameters
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        IF(ISECT .NE. 1) CALL ERROR (ERRKEY,1,FILECC,LNUM)
        READ(CHAR,'(2F6.0)',IOSTAT=ERR) SSKC, SKCBMAX
        IF (ERR .NE. 0) THEN
          NMSG = NMSG + 1
          MSG(NMSG)="Error reading SSKC, SKCBMAX for ASCE PET method."
        ENDIF

!       Read tall reference crop parameters
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        IF(ISECT .NE. 1) CALL ERROR (ERRKEY,1,FILECC,LNUM)
        READ(CHAR,'(2F6.0)',IOSTAT=ERR) TSKC, TKCBMAX
        IF (ERR .NE. 0) THEN
          NMSG = NMSG + 1
          MSG(NMSG)="Error reading TSKC, TKCBMAX for ASCE PET method."
        ENDIF

        CLOSE (LUNCRP)

!       Check for values with valid ranges.
        IF (MEEVP .EQ. 'S') THEN
          IF (SSKC .LT. 0.30 .OR. SSKC .GT. 1.0) THEN
            NMSG = NMSG + 1
            MSG(NMSG) = "SSKC for ASCE PET method is out of range."
          ENDIF
          IF (SKCBMAX .LT. 0.25 .OR. SKCBMAX .GT. 1.5) THEN
            NMSG = NMSG + 1
            MSG(NMSG) = "SKCBMAX for ASCE PET method is out of range."
          ENDIF
        ENDIF
        
        IF (MEEVP .EQ. 'T') THEN
          IF (TSKC .LT. 0.30 .OR. TSKC .GT. 1.0) THEN
            NMSG = NMSG + 1
            MSG(NMSG) = "TSKC for ASCE PET method is out of range."
          ENDIF
          IF (TKCBMAX .LT. 0.25 .OR. TKCBMAX .GT. 1.5) THEN
            NMSG = NMSG + 1
            MSG(NMSG) = "TKCBMAX for ASCE PET method is out of range."
          ENDIF
        ENDIF
        
      ELSE
!       If fallow, use minimum values
        SSKC    = 0.30
        SKCBMAX = 0.25
        TSKC    = 0.30
        TKCBMAX = 0.25
      ENDIF

 100  IF (NMSG > 0) THEN
        MSG(NMSG+1) ="ASCE PET method may not be valid for this crop."
        MSG(NMSG+2) = "Model will stop."
        CALL WARNING(NMSG+2, ERRKEY, MSG)
        CALL ERROR(ERRKEY,1,FILECC,LNUM)
      ENDIF

!     Store the values for retrieval in SPAM (actually in PET.for).
      IF (MEEVP.EQ.'S') THEN
        CALL PUT('SPAM', 'SKC', SSKC)
        CALL PUT('SPAM', 'KCBMAX', SKCBMAX)
      ELSEIF (MEEVP.EQ.'T') THEN
        CALL PUT('SPAM', 'SKC', TSKC)
        CALL PUT('SPAM', 'KCBMAX', TKCBMAX)
      ENDIF

      RETURN
      END SUBROUTINE READ_ASCE_KT
!===========================================================================
