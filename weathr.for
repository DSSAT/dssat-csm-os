C=======================================================================
C  COPYRIGHT 1998-2011 DSSAT Foundation
C                      University of Florida, Gainesville, Florida
C                      International Fertilizer Development Center
C                      Washington State University
C  ALL RIGHTS RESERVED
C=======================================================================
C=======================================================================
C  CROPGRO Weather Module consists of the following files:
C     WEATHR.FOR  - Main routine for weather module
C     HMET.FOR    - Generates hourly meteorological values from daily data
C     IPWTH.FOR   - Reads daily weather data from FILEW
C     SOLAR.FOR   - Computes day length (DAYLEN) and solar parameters (SOLAR)
C     WGEN.FOR    - Generates daily weather data
C     WTHMOD.FOR  - Modification of daily data based on user-supplied
C                     parameters
C
C=======================================================================
C  WEATHR, Subroutine, N.B. Pickering
C  Weather handling routine: input, weather modification and
C  generation of hourly values.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  08/30/1991 NBP Written
C  10/23/1993 NBP Separated DAYLEN function and created DECL to compute
C               declination from DAYL.  Used for sens. analysis.
C  04/04/1996 GH  Added RHUM
C  07/14/1996 GH  Initialize variables for WGEN
C  07/01/1999 CHP Modified for modular format
C  09/02/1999 GH  Incorporated into CROPGRO
C  10/01/2000 NBP Moved growth temperature here from ETPG
C  06/06/2002 GH  Modified for crop rotations
C  08/20/2002 GH  Modified for Y2K
C  06/02/2005 GH  Fixed call to WTHMOD in Seasinit section
!  10/24/2005 CHP Put weather variables in constructed variable. 
C  02/13/2006 JIL Export AMTRH (R/R0) for leaf rolling calculation
!  04/28/2008 CHP Added option to read CO2 from file 
!  07/25/2014 CHP Added daily CO2 read from weather file (DCO2)
C-----------------------------------------------------------------------
C  Called by: Main
c  Calls:     DAYLEN, ERROR, HMET, IPWTH, SOLAR, WGEN, WTHMDB, WTHMOD
C=======================================================================

      SUBROUTINE WEATHR (CONTROL, ISWITCH, WEATHER, YREND)

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      CHARACTER*1  MEWTH, RNMODE
      CHARACTER*6  ERRKEY
      CHARACTER*12 FILEW
      CHARACTER*78 MESSAGE(10)
      CHARACTER*80 PATHWT
      CHARACTER*92 FILEWW

      INTEGER DOY, MULTI, NEV, RUN, YEAR, YRDOY, YRSIM, YYDDD
      INTEGER RSEED1, RSEED(4), REPNO
      INTEGER DYNAMIC, YREND

      REAL
     &  CCO2, CLOUDS, CO2, DAYL, DCO2, DEC, ISINB, PAR, 
     &  RAIN, REFHT, RHUM, S0N, SNDN, SNUP, SRAD, 
     &  TA, TAMP, TAV, TAVG, TDAY, TDEW, TGROAV, TGRODY,
     &  TMAX, TMIN, TWILEN, VAPR, WINDHT, WINDSP, XELEV, XLAT, XLONG

      REAL, DIMENSION(TS) :: AMTRH, AZZON, BETA, FRDIFP, FRDIFR, PARHR
      REAL, DIMENSION(TS) :: RADHR, RHUMHR, TAIRHR, TGRO, WINDHR

!      PARAMETER (CO2BAS = 330.0)
      PARAMETER (ERRKEY = 'WEATHR')
!      PARAMETER (RAD=2.0*PI/365.0)

      INTERFACE 
        SUBROUTINE OPSTRESS(CONTROL, IDETO,   
     &    PlantStres, WEATHER, YIELD, BIOMAS)
          USE ModuleDefs 
          TYPE (ControlType), Intent(IN)           :: CONTROL
          CHARACTER*1,        Intent(IN), Optional :: IDETO
          TYPE (PlStresType), Intent(IN), Optional :: PlantStres
          TYPE (WeatherType), Intent(IN), Optional :: WEATHER
          INTEGER,            Intent(IN), Optional :: YIELD
          REAL,               Intent(IN), Optional :: BIOMAS
        END SUBROUTINE OPSTRESS
      END INTERFACE

!     The variable "CONTROL" is of constructed type "ControlType" as 
!     defined in ModuleDefs.for, and contains the following variables.
!     The components are copied into local variables for use here.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType) ISWITCH
      TYPE (WeatherType) WEATHER

      DYNAMIC = CONTROL % DYNAMIC 
      MULTI   = CONTROL % MULTI   
      RUN     = CONTROL % RUN    
      RNMODE  = CONTROL % RNMODE  
      REPNO   = CONTROL % REPNO  
      YRDOY   = CONTROL % YRDOY   
      YRSIM   = CONTROL % YRSIM   

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
      CALL IPWTH(CONTROL,
     &    CCO2, DCO2, FILEW, FILEWW, MEWTH, PAR, PATHWT,  !Output
     &    RAIN, REFHT, RHUM, RSEED1, SRAD,                !Output
     &    TAMP, TAV, TDEW, TMAX, TMIN, VAPR, WINDHT,      !Output
     &    WINDSP, XELEV, XLAT, XLONG, YREND,              !Output
     &    RUNINIT)

      CALL WTHMOD(RUNINIT,
     &    CONTROL, FILEWW, XLAT, YYDDD,                   !Input
     &    CO2, DAYL, PAR, RAIN, SRAD, TDEW,               !Input/Output
     &    TMAX, TMIN, TWILEN, WINDSP,                     !Input/Output
     &    DEC, NEV, SNUP, SNDN, YREND)                    !Output

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
        YYDDD = YRSIM
        CALL YR_DOY(YYDDD, YEAR, DOY)
!-----------------------------------------------------------------------
        IF (MEWTH .EQ. 'M' .OR. MEWTH .EQ. 'G') THEN
          CALL IPWTH(CONTROL,
     &      CCO2, DCO2, FILEW, FILEWW, MEWTH, PAR, PATHWT,!Output
     &      RAIN, REFHT, RHUM, RSEED1, SRAD,              !Output
     &      TAMP, TAV, TDEW, TMAX, TMIN, VAPR, WINDHT,    !Output
     &      WINDSP, XELEV, XLAT, XLONG, YREND,            !Output
     &      SEASINIT)
          IF (YREND == YRDOY) RETURN

        ELSEIF (MEWTH .EQ. 'S' .OR. MEWTH .EQ. 'W') THEN
C       Set default values FOR REFHT AND WINDHT
          REFHT  = 1.5
          WINDHT = 2.0
          CCO2   = -99.
          SRAD = -99.0
          TMAX = -99.0
          TMIN = -99.0
          RAIN = -99.0
          PAR  = -99.0
          RHUM = -99.0
          CALL WGEN (CONTROL,
     &      FILEW, MEWTH, MULTI, RUN, PATHWT, REPNO,      !Input
     &      RNMODE, RSEED1, YRDOY, YRSIM,                 !Input
     &      PAR, RAIN, RSEED, SRAD, TAMP, TAV, TDEW,      !Output
     &      TMAX, TMIN, WINDSP, XLAT, XLONG, YREND)       !Output
        ELSE
          CALL ERROR(ERRKEY,1,' ',0)
        ENDIF

      IF (INDEX('QFN',RNMODE) .LE. 0 .OR. 
     &            (RUN .EQ. 1 .AND. REPNO .EQ. 1)) THEN
C       Substitute default values if TAV or TAMP are missing.  Write a
C         message to the WARNING.OUT file.
!       10/27/2005 CHP The checks for TAV and TAMP were being done in the 
!         STEMP routine, overriding this check. STEMP used .LE. instead 
!         of .LT. and the results were very different for some experiments 
!         which do not supply TAV and TAMP (UFMA8301.PNX, CLMO8501.SBX, 
!         NCCL8801.SBX, GALN0201.COX, others).
!         So -- leave LE for now.
          IF (TAV  .LE. 0.0) THEN       
            TAV = 20.0
            WRITE(MESSAGE(1), 100)
            WRITE(MESSAGE(2), 120) TAV
            WRITE(MESSAGE(3), 130)
            CALL WARNING (3, ERRKEY, MESSAGE)
          ENDIF
          IF (TAMP .LE. 0.0) THEN
!         IF (TAMP .LT. 0.0) THEN
            TAMP = 5.0
            WRITE(MESSAGE(1), 110)
            WRITE(MESSAGE(2), 120) TAMP
            WRITE(MESSAGE(3), 130)
            CALL WARNING (3, ERRKEY, MESSAGE)
          ENDIF
        ENDIF

  100 FORMAT
     &   ('Value of TAV, average annual soil temperature, is missing.')
  110 FORMAT('Value of TAMP, amplitude of soil temperature function,',
     &            ' is missing.')
  120 FORMAT('A default value of', F5.1, 'ºC is being used for this',
     &            ' simulation,')
  130 FORMAT('which may produce undesirable results.')


C     Calculate day length, sunrise and sunset.
      CALL DAYLEN(
     &    DOY, XLAT,                                      !Input
     &    DAYL, DEC, SNDN, SNUP)                          !Output

!     Subroutine to determine daily CO2
      CALL CO2VAL(CONTROL, ISWITCH, CCO2, DCO2, CO2)

C     Adjust daily weather data, if weather modification requested.
C     Effective DEC calculated if DAYL is changed.
      IF (NEV .GT. 0) THEN
        CALL WTHMOD(SEASINIT, 
     &    CONTROL, FILEWW, XLAT, YYDDD,                   !Input
     &    CO2, DAYL, PAR, RAIN, SRAD, TDEW,               !Input/Output
     &    TMAX, TMIN, TWILEN, WINDSP,                     !Input/Output
     &    DEC, NEV, SNUP, SNDN, YREND)                    !Output
      ENDIF

C     Calculate daily solar parameters.
      CALL SOLAR(
     &    DAYL, DEC, SRAD, XLAT,                          !Input
     &    CLOUDS, ISINB, S0N)                             !Output

!     Windspeed adjustment and initialization moved ahead
!     of Call to HMET on 27MAR14 by BAK

C     Adjust wind speed from reference height to 2m height.
      IF (WINDSP > 0.0) THEN
!       WINDSP = WINDSP * (2.0 / WINDHT) ** 2.0
        WINDSP = WINDSP * (2.0 / WINDHT) ** 0.2   !chp 8/28/13
      ELSE
        WINDSP = 86.4   ! Equivalent to average of 1.0 m/s
      ENDIF

C     Calculate hourly weather data.
      CALL HMET(
     &    CLOUDS, DAYL, DEC, ISINB, PAR, REFHT,           !Input
     &    SNDN, SNUP, S0N, SRAD, TDEW, TMAX,              !Input
     &    TMIN, WINDHT, WINDSP, XLAT,                     !Input
     &    AMTRH, AZZON, BETA, FRDIFP, FRDIFR, PARHR,      !Output
     &    RADHR, RHUMHR, TAIRHR, TAVG, TDAY, TGRO,        !Output
     &    TGROAV, TGRODY, WINDHR)                         !Output

C     Compute daily normal temperature.
      TA = TAV - SIGN(1.0,XLAT) * TAMP * COS((DOY-20.0)*RAD)

      CALL OpWeath(CONTROL, ISWITCH, 
     &    CLOUDS, CO2, DAYL, PAR, RAIN, SRAD,         !Daily values
     &    TAVG, TDAY, TDEW, TGROAV, TGRODY, TMAX,     !Daily values
     &    TMIN, TWILEN, WINDSP, WEATHER)              !Daily values

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS - Read or generate daily weather data
!         (also run for initialization to get first day of weather data
!         for use by soil nitrogen and soil temperature initialization
!         routines.)
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
      YYDDD = YRDOY
      CALL YR_DOY(YYDDD, YEAR, DOY)
!-----------------------------------------------------------------------
C     Read new weather record.
      IF (MEWTH .EQ. 'M' .OR. MEWTH .EQ. 'G') THEN
        CALL IPWTH(CONTROL,
     &    CCO2, DCO2, FILEW, FILEWW, MEWTH, PAR, PATHWT,  !Output
     &    RAIN, REFHT, RHUM, RSEED1, SRAD,                !Output
     &    TAMP, TAV, TDEW, TMAX, TMIN, VAPR, WINDHT,      !Output
     &    WINDSP, XELEV, XLAT, XLONG, YREND,              !Output
     &    RATE)
        IF (YREND == YRDOY) RETURN

      ELSE IF (MEWTH .EQ. 'S' .OR. MEWTH .EQ. 'W') THEN
        SRAD = -99.0
        TMAX = -99.0
        TMIN = -99.0
        RAIN = -99.0
        PAR  = -99.0

        CALL WGEN (CONTROL,
     &    FILEW, MEWTH, MULTI, RUN, PATHWT, REPNO,        !Input
     &    RNMODE, RSEED1, YRDOY, YRSIM,                   !Input
     &    PAR, RAIN, RSEED, SRAD, TAMP, TAV, TDEW,        !Output
     &    TMAX, TMIN, WINDSP, XLAT, XLONG, YREND)         !Output
      ELSE
        CALL ERROR(ERRKEY,1,' ',0)
      ENDIF

C     Calculate day length, sunrise and sunset.
      CALL DAYLEN(
     &    DOY, XLAT,                                      !Input
     &    DAYL, DEC, SNDN, SNUP)                          !Output

C     Calculate twilight to twilight daylength for 
C        rice and maize routines.
      CALL TWILIGHT(DOY, XLAT, TWILEN) 

!     Subroutine to determine daily CO2
      CALL CO2VAL(CONTROL, ISWITCH, CCO2, DCO2, CO2)

C     Adjust daily weather data, if weather modification requested.
C     Effective DEC calculated if DAYL is changed.
      IF (NEV .GT. 0) THEN
        CALL WTHMOD(RATE, 
     &    CONTROL, FILEWW, XLAT, YYDDD,                   !Input
     &    CO2, DAYL, PAR, RAIN, SRAD, TDEW,               !Input/Output
     &    TMAX, TMIN, TWILEN, WINDSP,                     !Input/Output
     &    DEC, NEV, SNUP, SNDN, YREND)                    !Output
      ENDIF

C     Calculate daily solar parameters.
      CALL SOLAR(
     &    DAYL, DEC, SRAD, XLAT,                          !Input
     &    CLOUDS, ISINB, S0N)                             !Output

!     Wind speed adjustments and initialization moved
!     adhead of call to HMET on 27MAR14 by BAK

C     Adjust wind speed from reference height to 2m height.
      IF (WINDSP > 0.0) THEN
!       WINDSP = WINDSP * (2.0 / WINDHT) ** 2.0
        WINDSP = WINDSP * (2.0 / WINDHT) ** 0.2   !chp 8/28/13
      ELSE
        WINDSP = 86.4
      ENDIF

C     Calculate hourly weather data.
      CALL HMET(
     &    CLOUDS, DAYL, DEC, ISINB, PAR, REFHT,           !Input
     &    SNDN, SNUP, S0N, SRAD, TDEW, TMAX,              !Input
     &    TMIN, WINDHT, WINDSP, XLAT,                     !Input
     &    AMTRH, AZZON, BETA, FRDIFP, FRDIFR, PARHR,      !Output
     &    RADHR, RHUMHR, TAIRHR, TAVG, TDAY, TGRO,        !Output
     &    TGROAV, TGRODY, WINDHR)                         !Output

C     Compute daily normal temperature.
      TA = TAV - SIGN(1.0,XLAT) * TAMP * COS((DOY-20.0)*RAD)

!      CALL OPSTRESS(CONTROL, WEATHER=WEATHER)

!***********************************************************************
!***********************************************************************
!     Daily Output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
C-----------------------------------------------------------------------
      CALL OpWeath(CONTROL, ISWITCH, 
     &    CLOUDS, CO2, DAYL, PAR, RAIN, SRAD,         !Daily values
     &    TAVG, TDAY, TDEW, TGROAV, TGRODY, TMAX,     !Daily values
     &    TMIN, TWILEN, WINDSP, WEATHER)      !Daily values

!***********************************************************************
!***********************************************************************
!     SEASEND
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
      IF (MEWTH .EQ. 'M' .OR. MEWTH .EQ. 'G') THEN
        CALL IPWTH(CONTROL,
     &    CCO2, DCO2, FILEW, FILEWW, MEWTH, PAR, PATHWT,  !Output
     &    RAIN, REFHT, RHUM, RSEED1, SRAD,                !Output
     &    TAMP, TAV, TDEW, TMAX, TMIN, VAPR, WINDHT,      !Output
     &    WINDSP, XELEV, XLAT, XLONG, YREND,              !Output
     &    SEASEND)
      ENDIF

      CALL OpWeath(CONTROL, ISWITCH, 
     &    CLOUDS, CO2, DAYL, PAR, RAIN, SRAD,         !Daily values
     &    TAVG, TDAY, TDEW, TGROAV, TGRODY, TMAX,     !Daily values
     &    TMIN, TWILEN, WINDSP, WEATHER)              !Daily values

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
!     Weather station data
      WEATHER % REFHT  = REFHT
      WEATHER % WINDHT = WINDHT 
      WEATHER % XLAT   = XLAT  
      WEATHER % TAMP   = TAMP  
      WEATHER % TAV    = TAV   

!     Daily data
      WEATHER % AMTRH  = AMTRH
      WEATHER % CLOUDS = CLOUDS
      WEATHER % CO2    = CO2   
      WEATHER % DAYL   = DAYL  
      WEATHER % PAR    = PAR   
      WEATHER % RAIN   = RAIN  
      WEATHER % RHUM   = RHUM  
      WEATHER % SNDN   = SNDN  
      WEATHER % SNUP   = SNUP  
      WEATHER % SRAD   = SRAD  
      WEATHER % TA     = TA    
      WEATHER % TAVG   = TAVG  
      WEATHER % TDAY   = TDAY  
      WEATHER % TDEW   = TDEW  
      WEATHER % TGROAV = TGROAV
      WEATHER % TGRODY = TGRODY
      WEATHER % TMAX   = TMAX  
      WEATHER % TMIN   = TMIN  
      WEATHER % TWILEN = TWILEN
      WEATHER % WINDSP = WINDSP
      WEATHER % VAPR   = VAPR

!     Hourly data
      WEATHER % AZZON  = AZZON 
      WEATHER % BETA   = BETA  
      WEATHER % FRDIFP = FRDIFP
      WEATHER % FRDIFR = FRDIFR
      WEATHER % PARHR  = PARHR 
      WEATHER % RADHR  = RADHR 
      WEATHER % RHUMHR = RHUMHR
      WEATHER % TAIRHR = TAIRHR
      WEATHER % TGRO   = TGRO  
      WEATHER % WINDHR = WINDHR

      CALL OPSTRESS(CONTROL, WEATHER=WEATHER)

      RETURN
      END SUBROUTINE WEATHR

!***********************************************************************
!***********************************************************************
! WEATHR Variables
!-----------------------------------------------------------------------
! AZZON(TS)  Hourly solar azimuth (+/- from South) (deg.)
! BETA(TS)   Hourly solar elevation (+/- from horizontal) (deg.)
! CCO2       Atmospheric CO2 concentration read from input file (ppm)
! CLOUDS     Relative cloudiness factor (0-1) 
! CO2        Atmospheric carbon dioxide concentration (ppm)
! CO2BAS     Carbon dioxide base level from which adjustments are made
!              (ppm)
! DAYL       Day length on day of simulation (from sunrise to sunset) (hr)
! DEC        Solar declination or (90o - solar elevation at noon). 
!              Amplitude = +/- 23.45. Min = Dec. 21, Max = Jun 21/22 (deg.)
! DOY        Current day of simulation (d)
! ERRKEY     Subroutine name for error file 
! FILEW      Weather data file 
! FRDIFP(TS) Hourly fraction diffuse photon flux density after correcting 
!              for circumsolar radiation (Spitters, 1986) 
! FRDIFR(TS) Hourly fraction diffuse solar radiation after correcting for 
!              circumsolar radiation (Spitters, 1986) 
! ISINB      Integral in Spitter's Equation 6 
! MEWTH      Switch for method of obtaining weather data-- 'G' or 'M'- read 
!              weather data file 'S'- read SIMMETEO inputs and generate 
!              weather data 'W'- read WGEN inputs and generate weather data 
! MULTI      Current simulation year (=1 for first or single simulation, 
!              =NYRS for last seasonal simulation) 
! NEV        Number of environmental modification records 
! RUN       Report number for sequenced or multi-season runs 
! PAR        Daily photosynthetically active radiation or photon flux 
!              density (moles[quanta]/m2-d)
! PARHR(TS)  hourly PAR (J / m2 - s)
! PATHWT     Directory path for weather file 
! RADHR(TS)  Total hourly solar radiation (J/m2-s)
! RAIN       Precipitation depth for current day (mm)
! REFHT      Reference height for wind speed (m)
! RHUM       Relative humidity (%)
! RHUMHR(TS) Relative humidity hourly value (%)
! RSEED(4)   Random number generator seeds 
! RSEED1     Random number generator seed- user input 
! S0N        Normal extraterrestrial radiation (set equal to average solar 
!              constant; elliptical orbit ignored) (J/m2-s)
! SNDN       Time of sunset (hr)
! SNUP       Time of sunrise (hr)
! SRAD       Solar radiation (MJ/m2-d)
! TAIRHR(TS) Hourly air temperature (in some routines called TGRO) (°C)
! TAMP       Amplitude of temperature function used to calculate soil 
!              temperatures (°C)
! TAV        Average annual soil temperature, used with TAMP to calculate 
!              soil temperature. (°C)
! TAVG       Average daily temperature (°C)
! TDAY       Average temperature during daylight hours (°C)
! TDEW       Dewpoint temperature (°C)
! TGRO(I)    Hourly air temperature (°C)
! TGROAV     Average daily canopy temperature (°C)
! TGRODY     Average temperature during daylight hours (°C)
! TMAX       Maximum daily temperature (°C)
! TMIN       Minimum daily temperature (°C)
! TS         Number of intermediate time steps (=24) 
! WINDHR(TS) Hourly wind speed (m/s)
! WINDHT     Reference height for wind speed (m)
! WINDSP     Wind speed (km/d)
! XELEV      Field elevation (not used) (m)
! XLAT       Latitude (deg.)
! XLONG      Longitude (deg.)
! YEAR       Year of current date of simulation 
! YR_DOY     Function subroutoine converts date in YYDDD format to integer 
!              year (YY) and day (DDD). 
! YRDOY      Current day of simulation (YYDDD)
! YRSIM      Start of simulation date (YYDDD)
! YYDDD      Current date for weather module 
!***********************************************************************
!***********************************************************************


