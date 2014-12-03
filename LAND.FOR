C=======================================================================
C  COPYRIGHT 1998-2011 DSSAT Foundation
C                      University of Florida, Gainesville, Florida
C                      International Fertilizer Development Center
C                      University of Guelph, Guelph, Ontario
C                      Washington State University 
C  ALL RIGHTS RESERVED
C=======================================================================
C=======================================================================
C  LAND UNIT Module. G.Hoogenboom, J.W.Jones, C.Porter
C-----------------------------------------------------------------------
C  Land Unit Module.  Provides the interface between soil, weather
C  and crops.  Based on the original CROPGRO routine
C=======================================================================
C  REVISION       HISTORY
C  12/01/2001 CHP Written.
C  12/12/2001 GH  Rename to Land
!  10/24/2005 CHP Put weather variables in constructed variable. 
!  02/28/2006 CHP Rename Alt_Plant to Plant, move call to CROPGRO there
!  03/03/2006 CHP Added tillage (A.Andales & WDBatchelor).
!  03/21/2006 CHP Added mulch effects
!  10/31/2007 CHP Added simple K model.
C=======================================================================
C-----------------------------------------------------------------------
C    CROP should be modified when model versions change
C     or when a crop specific model is created.
C
C     CROP   = CR Generic CRopgro model     Version 3.90 (2002)
C     CROP   = BN for CROPGRO - DRY BEAN    Version 3.9  (2002)
C     CROP   = PN for CROPGRO - PEANUT      Version 3.9  (2002)
C     CROP   = SB for CROPGRO - SOYBEAN     Version 3.9  (2002)
C     CROP   = FA for CROPGRO - FALLOW      Version 3.9  (2002)
C     CROP   = TM for CROPGRO - TOMATO      Version 3.9  (2002)
C     CROP   = PR for CROPGRO - PEPPER      Version 3.9  (2002)
C     CROP   = PE for CROPGRO - PEA         Version 3.9  (2002)
C     CROP   = CH for CROPGRO - CHICKPEA    Version 3.9  (2002)
C     CROP   = PP for CROPGRO - PIGEONPEA   Version 3.9  (2002)
C     CROP   = C3 for CROPGRO - C4 CROPS    Version 3.9  (2002)
C     CROP   = C4 for CROPGRO - C3 CROPS    Version 3.9  (2002)
C     CROP   = G0 for CROPGRO - GRASS-0     Version 3.9  (2002)
C     CROP   = G1 for CROPGRO - GRASS-1     Version 3.9  (2002)
C     CROP   = G2 for CROPGRO - GRASS-2     Version 3.9  (2002)
C     CROP   = G3 for CROPGRO - GRASS-3     Version 3.9  (2002)
C     CROP   = G4 for CROPGRO - GRASS-4     Version 3.9  (2002)
C     CROP   = G5 for CROPGRO - GRASS-5     Version 3.9  (2002)
C     CROP   = G6 for CROPGRO - GRASS-6     Version 3.9  (2002)
C     CROP   = G7 for CROPGRO - GRASS-7     Version 3.9  (2002)
C     CROP   = G8 for CROPGRO - GRASS-8     Version 3.9  (2002)
C     CROP   = BR for CROPGRO - Brachiaria
C                               decumbens   Version 3.9  (2002)
C     CROP   = VB for CROPGRO - VELVETBEAN  Version 3.9  (2002)
C     CROP   = FB for CROPGRO - FABA BEAN   Version 3.9  (2002)
C     CROP   = NP for CROPGRO - Napier grassVersion 3.9  (2002)
C     CROP   = GB for CROPGRO - Green Bean  Version 3.9  (2002)
C     CROP   = PE for CROPGRO - Pea         Version 3.9  (2002)
C
C-GH  Need to add other DSSAT-CSM crops

C     CROP   = BA for CSCERES - BARLEY      Version 3.9  (2002)
C     CROP   = CB for CROPGRO - CABBAGE     Version 3.9  (2002)
C     CROP   = CS for CSSIM   - CASSAVA     Version 3.9  (2002)
C     CROP   = CO for CROPGRO - COTTON      Version 3.9  (2002)
C     CROP   = CP for CROPGRO - COWPEA      Version 3.9  (2002)
C     CROP   = CT for CROPGRO - CITRUS      Version 3.9  (2002)
C     CROP   = MZ for GECER   - MAIZE       Version 3.9  (2002)
C     CROP   = ML for GECER   - MILLET      Version 3.9  (2002)
C     CROP   = PI for PIAL    - PINEAPPLE   Version 3.9  (2002)
C     CROP   = PT for SUBSTOR - POTATO      Version 3.9  (2002)
C     CROP   = RI for RICER   - RICE        Version 3.9  (2002)
C     CROP   = SC for SCCAN   - SUGARCANE   Version 3.9  (2002)
C     CROP   = SG for GECER   - SORGHUM     Version 3.9  (2002)
C     CROP   = SU for SUOIL   - SUNFLOWER   Version 3.9  (2002)
C     CROP   = TM for CROPGRO - TOMATO      Version 3.9  (2002)
C     CROP   = TN for ARGRO   - TANIER      Version 3.9  (2002)
C     CROP   = TR for ARGRO   - TARO        Version 3.9  (2002)
C     CROP   = WH for CSCERES - WHEAT       Version 3.9  (2002)
C=======================================================================
      SUBROUTINE LAND(CONTROL, ISWITCH, 
     &                YRPLT, MDATE, YREND)
      
C-----------------------------------------------------------------------
      USE ModuleDefs      
      USE FloodModule    

      IMPLICIT NONE
      SAVE
C-----------------------------------------------------------------------
C     Crop, Experiment, Command line Variables
C-----------------------------------------------------------------------
      CHARACTER*2  CROP
      CHARACTER*6  ERRKEY
      PARAMETER   (ERRKEY = 'LAND  ')
      CHARACTER*8  MODEL
      CHARACTER*30 FILEIO
      
C-----------------------------------------------------------------------
C     Date / Timing / Sequencing Variables
C-----------------------------------------------------------------------
      INTEGER      DYNAMIC, YRSIM, YRDOY

C-----------------------------------------------------------------------
C     Input and Output Handling
C-----------------------------------------------------------------------
      CHARACTER*1  IDETS, IPLTI
      CHARACTER*78 MSG(2)

C-----------------------------------------------------------------------
C     Weather module Variables
C-----------------------------------------------------------------------
      TYPE (WeatherType)  WEATHER

C-----------------------------------------------------------------------
C     Soil Processes Module Variables 
C-----------------------------------------------------------------------
      REAL SNOW, WINF
      REAL, DIMENSION(NL) :: NH4, NO3, SPi_Avail, SKi_Avail, ST, UPPM
      REAL, DIMENSION(NL) :: SW, SWDELTS, UPFLOW
      TYPE (SoilType) SOILPROP    !type defined in ModuleDefs
      TYPE (FloodWatType) FLOODWAT
      TYPE (FloodNType)   FloodN
      TYPE (MulchType)    MULCH
!     Needed for ORYZA-Rice
      REAL, DIMENSION(0:NL) :: SomLitC
      REAL, DIMENSION(0:NL,NELEM) :: SomLitE

C-----------------------------------------------------------------------
C     Soil - Plant - Atmosphere Module Variables
C-----------------------------------------------------------------------
      REAL EO, EOP, ES, SRFTEMP, TRWUP
      REAL SWDELTU(NL), SWDELTX(NL) !, RWU(NL)
!     Needed for CaneGro_SA
      REAL EOS, EP, TRWU
!     Calculated by ORYZA-Rice
      REAL UH2O(NL)
!     Needed for SALUS
      REAL RWU(NL)

C-----------------------------------------------------------------------
C     PLANT Module Variables
C-----------------------------------------------------------------------
      INTEGER MDATE
      INTEGER STGDOY(20)
      REAL CANHT, EORATIO, NSTRES, PORMIN, PSTRES1, RWUMX
      REAL XHLAI, XLAI
      REAL KSEVAP, KTRANS
      REAL, Dimension(NL) :: PUptake, RLV, FracRts, UNH4, UNO3, KUptake
      Type (ResidueType) HARVRES  !type defined in ModuleDefs
      Type (ResidueType) SENESCE  
      
C-----------------------------------------------------------------------
C     Operations Management Module Variables 
C-----------------------------------------------------------------------
      TYPE (TillType) TILLVALS
      INTEGER YREND, YRPLT
      REAL IRRAMT
      REAL, DIMENSION(2) :: HARVFRAC   !Harvest & byproduct fractions
      TYPE (FertType) FERTDATA         !Fertilizer application
      TYPE (OrgMatAppType)OMAData      !Organic matter application

C-----------------------------------------------------------------------
!!     Temporary timer function
!!     Date / time variables
!      INTEGER DATE_TIME(8)
!!      date_time(1)  The 4-digit year  
!!      date_time(2)  The month of the year  
!!      date_time(3)  The day of the month  
!!      date_time(4)  The time difference with respect to Coordinated Universal Time (UTC) in minutes  
!!      date_time(5)  The hour of the day (range 0 to 23) - local time  
!!      date_time(6)  The minutes of the hour (range 0 to 59) - local time  
!!      date_time(7)  The seconds of the minute (range 0 to 59) - local time  
!!      date_time(8)  The milliseconds of the second (range 0 to 999) - local time  
!      REAL TIME0, TIME1, TIME_START DELTA_TIME
C-----------------------------------------------------------------------

C     Define constructed variable types based on definitions in
C     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH

C     Transfer values from constructed data types into local variables.
      CROP    = CONTROL % CROP
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      MODEL   = CONTROL % MODEL
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM

      IPLTI   = ISWITCH % IPLTI

C***********************************************************************
C***********************************************************************
C     Run Initialization - Called once per simulation
C***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
C-----------------------------------------------------------------------
!!     Temporary timer function
!      !Get initial time
!      CALL DATE_AND_TIME (VALUES=DATE_TIME)
!!     Convert time to seconds
!      TIME0 = DATE_TIME(7) 
!     &      + DATE_TIME(8) / 1000.  
!     &      + DATE_TIME(6) * 60.  
!     &      + DATE_TIME(5) * 3600.
!      TIME_START = TIME0
C-----------------------------------------------------------------------
C     Read switches from FILEIO
C-----------------------------------------------------------------------
      CALL IPIBS (CONTROL, ISWITCH, CROP, IDETS, MODEL)

C-----------------------------------------------------------------------
C     Read input parameters for weather routines
C-----------------------------------------------------------------------
      CALL WEATHR(CONTROL, ISWITCH, WEATHER, YREND)

C-----------------------------------------------------------------------
C     Read initial soil data 
C-----------------------------------------------------------------------
      CALL SOIL(CONTROL, ISWITCH, 
     &    ES, FERTDATA, HARVRES, IRRAMT, KTRANS,          !Input
     &    KUptake, OMAData, PUptake, SENESCE, SRFTEMP, ST,!Input
     &    FracRts, SWDELTX,TILLVALS, UNH4, UNO3, UPFLOW,  !Input
     &    WEATHER, XHLAI, FLOODN, FLOODWAT, MULCH,        !I/O
     &    NH4, NO3, SKi_AVAIL, SNOW, SPi_AVAIL, SOILPROP, !Output
     &    SomLitC, SomLitE,                               !Output
     &    SW, SWDELTS, SWDELTU, UPPM, WINF, YREND)        !Output

C-----------------------------------------------------------------------
C     Read initial soil-plant-atmosphere data
C-----------------------------------------------------------------------
      CALL SPAM(CONTROL, ISWITCH,
     &    CANHT, EORATIO, KSEVAP, KTRANS, MULCH,          !Input
     &    PSTRES1, PORMIN, RLV, RWUMX, SOILPROP, SW,      !Input
     &    SWDELTS, UH2O, WEATHER, WINF, XHLAI, XLAI,      !Input
     &    FLOODWAT, SWDELTU,                              !I/O
     &    EO, EOP, EOS, EP, ES, RWU, SRFTEMP, ST,         !Output
     &    SWDELTX, TRWU, TRWUP, UPFLOW)                   !Output

C-----------------------------------------------------------------------
C     Read initial plant module data
C-----------------------------------------------------------------------
      CALL PLANT(CONTROL, ISWITCH, 
     &    EO, EOP, EOS, EP, ES, FLOODWAT, HARVFRAC,       !Input
     &    NH4, NO3, RWU, SKi_Avail, SomLitC, SomLitE,     !Input
     &    SPi_AVAIL, SNOW, SOILPROP, SRFTEMP, ST, SW,     !Input
     &    TRWU, TRWUP, UPPM, WEATHER, YREND, YRPLT,       !Input
     &    FLOODN,                                         !I/O
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,        !Output
     &    KUptake, MDATE, NSTRES, PSTRES1,                !Output
     &    PUptake, PORMIN, RLV, RWUMX, SENESCE,           !Output
     &    STGDOY, FracRts, UNH4, UNO3, XHLAI, XLAI)       !Output

C-----------------------------------------------------------------------
C     Initialize summary.out information
C-----------------------------------------------------------------------
      CALL OPSUM (CONTROL, ISWITCH, YRPLT)

C*********************************************************************** 
C*********************************************************************** 
C     SEASONAL INITIALIZATION
C*********************************************************************** 
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
C     Call WEATHR for initialization - reads first day of weather
C     data for use in soil N and soil temp initialization.
C-----------------------------------------------------------------------
      CALL WEATHR(CONTROL, ISWITCH, WEATHER, YREND)

C-----------------------------------------------------------------------
C     Set planting date, adjust operations dates for seasonal or 
C     sequenced runs.
C-----------------------------------------------------------------------
      CALL MGMTOPS(CONTROL, ISWITCH, 
     &    FLOODWAT, HARVRES, NSTRES, SOILPROP, ST,        !Input 
     &    STGDOY, SW, WEATHER,                            !Input
     &    YREND, FERTDATA, HARVFRAC, IRRAMT,              !Output
     &    MDATE, OMADATA, TILLVALS, YRPLT)                !Output

C-----------------------------------------------------------------------
      IF (YRPLT < YRSIM .AND. CROP /= 'FA' .AND. IPLTI /= 'A') THEN
        CALL ERROR(ERRKEY,2,' ',0)
      ENDIF

C-----------------------------------------------------------------------
C     Seasonal initialization for soil-plant-atmosphere processes
!     chp moved this before SOIL, so soil temp is available 
C-----------------------------------------------------------------------
      CALL SPAM(CONTROL, ISWITCH,
     &    CANHT, EORATIO, KSEVAP, KTRANS, MULCH,          !Input
     &    PSTRES1, PORMIN, RLV, RWUMX, SOILPROP, SW,      !Input
     &    SWDELTS, UH2O, WEATHER, WINF, XHLAI, XLAI,      !Input
     &    FLOODWAT, SWDELTU,                              !I/O
     &    EO, EOP, EOS, EP, ES, RWU, SRFTEMP, ST,         !Output
     &    SWDELTX, TRWU, TRWUP, UPFLOW)                   !Output

C-----------------------------------------------------------------------
C     Seasonal initialization for soil processes
C-----------------------------------------------------------------------
      CALL SOIL(CONTROL, ISWITCH, 
     &    ES, FERTDATA, HARVRES, IRRAMT, KTRANS,          !Input
     &    KUptake, OMAData, PUptake, SENESCE, SRFTEMP, ST,!Input
     &    FracRts, SWDELTX,TILLVALS, UNH4, UNO3, UPFLOW,  !Input
     &    WEATHER, XHLAI, FLOODN, FLOODWAT, MULCH,        !I/O
     &    NH4, NO3, SKi_AVAIL, SNOW, SPi_AVAIL, SOILPROP, !Output
     &    SomLitC, SomLitE,                               !Output
     &    SW, SWDELTS, SWDELTU, UPPM, WINF, YREND)        !Output

C-----------------------------------------------------------------------
C     Initialize PLANT routines (including phenology and pest)
C-----------------------------------------------------------------------
      CALL PLANT(CONTROL, ISWITCH, 
     &    EO, EOP, EOS, EP, ES, FLOODWAT, HARVFRAC,       !Input
     &    NH4, NO3, RWU, SKi_Avail, SomLitC, SomLitE,     !Input
     &    SPi_AVAIL, SNOW, SOILPROP, SRFTEMP, ST, SW,     !Input
     &    TRWU, TRWUP, UPPM, WEATHER, YREND, YRPLT,       !Input
     &    FLOODN,                                         !I/O
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,        !Output
     &    KUptake, MDATE, NSTRES, PSTRES1,                !Output
     &    PUptake, PORMIN, RLV, RWUMX, SENESCE,           !Output
     &    STGDOY, FracRts, UNH4, UNO3, XHLAI, XLAI)       !Output
C-----------------------------------------------------------------------
C     Initialize summary output file - possible output from 
C     various modules.
C-----------------------------------------------------------------------
      IF (IDETS .EQ. 'Y' .OR. IDETS .EQ. 'A') THEN
        CALL OPSUM (CONTROL, ISWITCH, YRPLT)
      ENDIF

C***********************************************************************
C***********************************************************************
C     DAILY RATE CALCULATIONS
C***********************************************************************
      ELSE IF (DYNAMIC .EQ. RATE) THEN
C-----------------------------------------------------------------------
C     Call WEATHER Subroutine to input weather data and to
C     calculate hourly radiation and air temperature values
C     Note: First day of weather has already been read by 
C       initialization call to WEATHR.
C-----------------------------------------------------------------------
      CALL WEATHR(CONTROL, ISWITCH, WEATHER, YREND)

C-----------------------------------------------------------------------
C     Call Operations Management module to determine today's 
C     applications of irrigation, tillage, etc.
C-----------------------------------------------------------------------
      CALL MGMTOPS(CONTROL, ISWITCH, 
     &    FLOODWAT, HARVRES, NSTRES, SOILPROP, ST,        !Input 
     &    STGDOY, SW, WEATHER,                            !Input
     &    YREND, FERTDATA, HARVFRAC, IRRAMT,              !Output
     &    MDATE, OMADATA, TILLVALS, YRPLT)                !Output

C-----------------------------------------------------------------------
C     Call Soil processes module to determine today's rates of 
C     change of soil properties.
C-----------------------------------------------------------------------
      CALL SOIL(CONTROL, ISWITCH, 
     &    ES, FERTDATA, HARVRES, IRRAMT, KTRANS,          !Input
     &    KUptake, OMAData, PUptake, SENESCE, SRFTEMP, ST,!Input
     &    FracRts, SWDELTX,TILLVALS, UNH4, UNO3, UPFLOW,  !Input
     &    WEATHER, XHLAI, FLOODN, FLOODWAT, MULCH,        !I/O
     &    NH4, NO3, SKi_AVAIL, SNOW, SPi_AVAIL, SOILPROP, !Output
     &    SomLitC, SomLitE,                               !Output
     &    SW, SWDELTS, SWDELTU, UPPM, WINF, YREND)        !Output

C-----------------------------------------------------------------------
C     Call Soil-plant-atmosphere module to determine today's
C     rates of evapotranspiration.
C-----------------------------------------------------------------------
      CALL SPAM(CONTROL, ISWITCH,
     &    CANHT, EORATIO, KSEVAP, KTRANS, MULCH,          !Input
     &    PSTRES1, PORMIN, RLV, RWUMX, SOILPROP, SW,      !Input
     &    SWDELTS, UH2O, WEATHER, WINF, XHLAI, XLAI,      !Input
     &    FLOODWAT, SWDELTU,                              !I/O
     &    EO, EOP, EOS, EP, ES, RWU, SRFTEMP, ST,         !Output
     &    SWDELTX, TRWU, TRWUP, UPFLOW)                   !Output

C-----------------------------------------------------------------------
C     Call PLANT Subroutine to calculate crop growth and
C     development rates.
C     Skip plant growth and development routines for fallow runs
C-----------------------------------------------------------------------
      IF (CROP .NE. 'FA' .AND. 
     &    YRDOY .GE. YRPLT .AND. YRPLT .NE. -99) THEN
        CALL PLANT(CONTROL, ISWITCH, 
     &    EO, EOP, EOS, EP, ES, FLOODWAT, HARVFRAC,       !Input
     &    NH4, NO3, RWU, SKi_Avail, SomLitC, SomLitE,     !Input
     &    SPi_AVAIL, SNOW, SOILPROP, SRFTEMP, ST, SW,     !Input
     &    TRWU, TRWUP, UPPM, WEATHER, YREND, YRPLT,       !Input
     &    FLOODN,                                         !I/O
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,        !Output
     &    KUptake, MDATE, NSTRES, PSTRES1,                !Output
     &    PUptake, PORMIN, RLV, RWUMX, SENESCE,           !Output
     &    STGDOY, FracRts, UNH4, UNO3, XHLAI, XLAI)       !Output
      ENDIF

C***********************************************************************
C     DAILY INTEGRATION 
C***********************************************************************
      ELSE IF (DYNAMIC .EQ. INTEGR) THEN
C***********************************************************************
C     Integrate soil state variables
C-----------------------------------------------------------------------
      CALL SOIL(CONTROL, ISWITCH, 
     &    ES, FERTDATA, HARVRES, IRRAMT, KTRANS,          !Input
     &    KUptake, OMAData, PUptake, SENESCE, SRFTEMP, ST,!Input
     &    FracRts, SWDELTX,TILLVALS, UNH4, UNO3, UPFLOW,  !Input
     &    WEATHER, XHLAI, FLOODN, FLOODWAT, MULCH,        !I/O
     &    NH4, NO3, SKi_AVAIL, SNOW, SPi_AVAIL, SOILPROP, !Output
     &    SomLitC, SomLitE,                               !Output
     &    SW, SWDELTS, SWDELTU, UPPM, WINF, YREND)        !Output

C-----------------------------------------------------------------------
C     Compute cumulative totals for soil-plant-atmosphere processes
C-----------------------------------------------------------------------
      CALL SPAM(CONTROL, ISWITCH,
     &    CANHT, EORATIO, KSEVAP, KTRANS, MULCH,          !Input
     &    PSTRES1, PORMIN, RLV, RWUMX, SOILPROP, SW,      !Input
     &    SWDELTS, UH2O, WEATHER, WINF, XHLAI, XLAI,      !Input
     &    FLOODWAT, SWDELTU,                              !I/O
     &    EO, EOP, EOS, EP, ES, RWU, SRFTEMP, ST,         !Output
     &    SWDELTX, TRWU, TRWUP, UPFLOW)                   !Output

C-----------------------------------------------------------------------
C     Call Plant module to integrate daily plant processes and update
C     plant state variables.
C-----------------------------------------------------------------------
      IF (CROP .NE. 'FA' .AND. 
     &        YRDOY .GE. YRPLT .AND. YRPLT .NE. -99) THEN
        CALL PLANT(CONTROL, ISWITCH, 
     &    EO, EOP, EOS, EP, ES, FLOODWAT, HARVFRAC,       !Input
     &    NH4, NO3, RWU, SKi_Avail, SomLitC, SomLitE,     !Input
     &    SPi_AVAIL, SNOW, SOILPROP, SRFTEMP, ST, SW,     !Input
     &    TRWU, TRWUP, UPPM, WEATHER, YREND, YRPLT,       !Input
     &    FLOODN,                                         !I/O
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,        !Output
     &    KUptake, MDATE, NSTRES, PSTRES1,                !Output
     &    PUptake, PORMIN, RLV, RWUMX, SENESCE,           !Output
     &    STGDOY, FracRts, UNH4, UNO3, XHLAI, XLAI)       !Output
      ENDIF

C-----------------------------------------------------------------------
C     Call Operations Management module to check for harvest end, 
C     accumulate variables.
C-----------------------------------------------------------------------
      CALL MGMTOPS(CONTROL, ISWITCH, 
     &    FLOODWAT, HARVRES, NSTRES, SOILPROP, ST,        !Input 
     &    STGDOY, SW, WEATHER,                            !Input
     &    YREND, FERTDATA, HARVFRAC, IRRAMT,              !Output
     &    MDATE, OMADATA, TILLVALS, YRPLT)                !Output

C***********************************************************************
C***********************************************************************
C     Daily Output
C***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN

      CALL WEATHR(CONTROL, ISWITCH, WEATHER, YREND)

        CALL SOIL(CONTROL, ISWITCH, 
     &    ES, FERTDATA, HARVRES, IRRAMT, KTRANS,          !Input
     &    KUptake, OMAData, PUptake, SENESCE, SRFTEMP, ST,!Input
     &    FracRts, SWDELTX,TILLVALS, UNH4, UNO3, UPFLOW,  !Input
     &    WEATHER, XHLAI, FLOODN, FLOODWAT, MULCH,        !I/O
     &    NH4, NO3, SKi_AVAIL, SNOW, SPi_AVAIL, SOILPROP, !Output
     &    SomLitC, SomLitE,                               !Output
     &    SW, SWDELTS, SWDELTU, UPPM, WINF, YREND)        !Output

        CALL SPAM(CONTROL, ISWITCH,
     &    CANHT, EORATIO, KSEVAP, KTRANS, MULCH,          !Input
     &    PSTRES1, PORMIN, RLV, RWUMX, SOILPROP, SW,      !Input
     &    SWDELTS, UH2O, WEATHER, WINF, XHLAI, XLAI,      !Input
     &    FLOODWAT, SWDELTU,                              !I/O
     &    EO, EOP, EOS, EP, ES, RWU, SRFTEMP, ST,         !Output
     &    SWDELTX, TRWU, TRWUP, UPFLOW)                   !Output

C-----------------------------------------------------------------------
C     Call plant module for daily printout.
C-----------------------------------------------------------------------
        IF (CROP .NE. 'FA') THEN
          CALL PLANT(CONTROL, ISWITCH, 
     &    EO, EOP, EOS, EP, ES, FLOODWAT, HARVFRAC,       !Input
     &    NH4, NO3, RWU, SKi_Avail, SomLitC, SomLitE,     !Input
     &    SPi_AVAIL, SNOW, SOILPROP, SRFTEMP, ST, SW,     !Input
     &    TRWU, TRWUP, UPPM, WEATHER, YREND, YRPLT,       !Input
     &    FLOODN,                                         !I/O
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,        !Output
     &    KUptake, MDATE, NSTRES, PSTRES1,                !Output
     &    PUptake, PORMIN, RLV, RWUMX, SENESCE,           !Output
     &    STGDOY, FracRts, UNH4, UNO3, XHLAI, XLAI)       !Output
        ENDIF

        CALL MGMTOPS(CONTROL, ISWITCH, 
     &    FLOODWAT, HARVRES, NSTRES, SOILPROP, ST,        !Input 
     &    STGDOY, SW, WEATHER,                            !Input
     &    YREND, FERTDATA, HARVFRAC, IRRAMT,              !Output
     &    MDATE, OMADATA, TILLVALS, YRPLT)                !Output

C*********************************************************************** 
C***********************************************************************
C     Seasonal Output
C*********************************************************************** 
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN

C     Call WEATHER module to close current weather file 
      CALL WEATHR(CONTROL, ISWITCH, WEATHER, YREND)

C     Print seasonal summaries and close files.
      CALL SOIL(CONTROL, ISWITCH, 
     &    ES, FERTDATA, HARVRES, IRRAMT, KTRANS,          !Input
     &    KUptake, OMAData, PUptake, SENESCE, SRFTEMP, ST,!Input
     &    FracRts, SWDELTX,TILLVALS, UNH4, UNO3, UPFLOW,  !Input
     &    WEATHER, XHLAI, FLOODN, FLOODWAT, MULCH,        !I/O
     &    NH4, NO3, SKi_AVAIL, SNOW, SPi_AVAIL, SOILPROP, !Output
     &    SomLitC, SomLitE,                               !Output
     &    SW, SWDELTS, SWDELTU, UPPM, WINF, YREND)        !Output

      CALL SPAM(CONTROL, ISWITCH,
     &    CANHT, EORATIO, KSEVAP, KTRANS, MULCH,          !Input
     &    PSTRES1, PORMIN, RLV, RWUMX, SOILPROP, SW,      !Input
     &    SWDELTS, UH2O, WEATHER, WINF, XHLAI, XLAI,      !Input
     &    FLOODWAT, SWDELTU,                              !I/O
     &    EO, EOP, EOS, EP, ES, RWU, SRFTEMP, ST,         !Output
     &    SWDELTX, TRWU, TRWUP, UPFLOW)                   !Output

      CALL PLANT(CONTROL, ISWITCH, 
     &    EO, EOP, EOS, EP, ES, FLOODWAT, HARVFRAC,       !Input
     &    NH4, NO3, RWU, SKi_Avail, SomLitC, SomLitE,     !Input
     &    SPi_AVAIL, SNOW, SOILPROP, SRFTEMP, ST, SW,     !Input
     &    TRWU, TRWUP, UPPM, WEATHER, YREND, YRPLT,       !Input
     &    FLOODN,                                         !I/O
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,        !Output
     &    KUptake, MDATE, NSTRES, PSTRES1,                !Output
     &    PUptake, PORMIN, RLV, RWUMX, SENESCE,           !Output
     &    STGDOY, FracRts, UNH4, UNO3, XHLAI, XLAI)       !Output

!     Call management operations module for seasonal printout.
      CALL MGMTOPS(CONTROL, ISWITCH, 
     &    FLOODWAT, HARVRES, NSTRES, SOILPROP, ST,        !Input 
     &    STGDOY, SW, WEATHER,                            !Input
     &    YREND, FERTDATA, HARVFRAC, IRRAMT,              !Output
     &    MDATE, OMADATA, TILLVALS, YRPLT)                !Output

C-----------------------------------------------------------------------
C     Seasonal Output
C     Call end of season and summary output subroutines
C-----------------------------------------------------------------------
      CALL OPSUM (CONTROL, ISWITCH, YRPLT)

!!     Temporary timer function
!      CALL DATE_AND_TIME (VALUES=DATE_TIME)
!      
!!     Convert time to seconds
!      TIME1 = DATE_TIME(7) 
!     &      + DATE_TIME(8) / 1000.  
!     &      + DATE_TIME(6) * 60.  
!     &      + DATE_TIME(5) * 3600.
!      DELTA_TIME = TIME1 - TIME0
!      WRITE(200,'(1X,"RUN ",I3,3X,F10.3)') RUN, DELTA_TIME
!      TIME0 = TIME1

      IF (CONTROL % ERRCODE > 0) THEN
        WRITE(MSG(1),'(A,I8)') "End of run ", CONTROL % RUN
        WRITE(MSG(2),'("Simulation ended with error code ",I3)') 
     &      CONTROL % ERRCODE
        CALL WARNING(2,'ENDRUN',MSG)
        CALL INFO(2,'ENDRUN',MSG)
      ELSE
        WRITE(MSG(1),'(A,I8)') "Normal end of run ", CONTROL % RUN
        CALL WARNING(0,'ENDRUN',MSG)
        CALL INFO(1,'ENDRUN',MSG)
      ENDIF

C*********************************************************************** 
C***********************************************************************
C     End of Run
C*********************************************************************** 
      ELSE IF (DYNAMIC .EQ. ENDRUN) THEN
        CALL SOIL(CONTROL, ISWITCH, 
     &    ES, FERTDATA, HARVRES, IRRAMT, KTRANS,          !Input
     &    KUptake, OMAData, PUptake, SENESCE, SRFTEMP, ST,!Input
     &    FracRts, SWDELTX,TILLVALS, UNH4, UNO3, UPFLOW,  !Input
     &    WEATHER, XHLAI, FLOODN, FLOODWAT, MULCH,        !I/O
     &    NH4, NO3, SKi_AVAIL, SNOW, SPi_AVAIL, SOILPROP, !Output
     &    SomLitC, SomLitE,                               !Output
     &    SW, SWDELTS, SWDELTU, UPPM, WINF, YREND)        !Output

!!     Temporary timer function
!      CALL DATE_AND_TIME (VALUES=DATE_TIME)
!      
!!     Convert time to seconds
!      TIME1 = DATE_TIME(7) 
!     &      + DATE_TIME(8) / 1000.  
!     &      + DATE_TIME(6) * 60.  
!     &      + DATE_TIME(5) * 3600.
!      DELTA_TIME = TIME1 - TIME_START
!      WRITE(200,'(/," Total Time",F10.3)') RUN, DELTA_TIME


!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
      RETURN
      END SUBROUTINE LAND 
