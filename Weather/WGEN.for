C=======================================================================
C  WGEN.for contains:
C    WGEN   - Generates daily weather data for crop models.
C    WGENIN - Reads inputs from parameter file and creates two matrices:
C                       inputs and correction factors.
C    SIMMIN - Reads SIMMETEO inputs from parameter file and creates
C                       matrices
C    WGENPM - Estimates daily generator parameters
C    WGENGN - Generates daily weather.
C    RANDN  -   Generates a uniform random number (0-1)
c    LININT - Linearly interpolates between monthly values.
C    CFACT  -   Calculates correction factor
C    CIRC   -   Creates circular sequence from L to U eg. 1,2,3,1,2,3 etc.
C    AVG    -   Calculates average of array.
C    FOURCF - Computes coefficients for 1ST order Fourier eqn. using
C                       AMP and PHASE shift form.
C    FOURIN - Calculates a monthly value using 1st order Fourier eqn.
C    RADA   -   Computes extraterrestrial radiation.
C=======================================================================

C=======================================================================
C  WGEN, Subroutine, Richardson and Wright
C  Generates daily weather data for crop models.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1984     Written
C  09/02/1992 NBP Modified for DSSAT V3.  Internal generation of daily
C                 data and use of monthly means as input.
C  06/29/1994 JH  Added lower limit of 0.1 to calc. of rainfall in WGEN.
C  06/29/1994 JH  Modified to handle months with no rainfall in WGENIN.
C  07/29/1994 NBP Mod. JH's changes.  Added extra checks for rain probs.
C                 Changed defaults for TDEW, WINDSP.  Cleaned up.
C  07/14/1996 GH  Moved initialization section to WEATHR
C  02/25/1999 CHP Modified for modular format
C  09/02/1999 GH  Incorporated into CROPGRO
C  03/15/2000 GH  Change SC to 1368 to be consistent with SOLAR routine
!  06/23/2005 CHP Change variable "N" to "NM" to avoid future conflict 
!                 with global variable "N"
!  11/12/2005 CHP Added read for TAV, TAMP
!  09/01/2009 CHP Added call to error checking routine, including YREND
C-----------------------------------------------------------------------
c  Called by:   WEATHR
C  Calls:               WGENIN, SIMMIN, INCMTH, WGENPM, WGENGN
C-----------------------------------------------------------------------
C  Input : PATHWT,FILEW,MEWTH,RSEED1,YRDOY,YRSIM,XLAT,XLONG,
C          MESIC
C  In/Out: RSEED
C  Output: SRAD,TMAX,TMIN,RAIN,PAR,TDEW,WINDSP
C  Saved : Y,CF,MTH,MM,YMID,YMEND,WETPRV,XPRV
C  Local :
C  Notes : The WGEN weather estimator (Richarson and Wright, 1984) was
C          modified by IBSNAT for DSSAT V3.  The IBSNAT modification
C          uses standard deviations rather than coefficients of
C          variation which makes the estimator more stable than
C          original version.  The generator has been modified to permit
C          internal use in the DSSAT crop models by generating a day
C          at a time.  Monthly input parameters are now used for all
C          variables with linear interpolation computed internally.
C          Monthly climate parameters can now be accepted as input and
C          the generator uses a combination of the regression equations
C          in SIMMETEO (Shu Geng et al., 1986, 1987?) and Pickering
C          et al. (1988) to compute the standard deviations.  All
C          previous versions of this generator and the coefficient
C          files supplied by the IBSNAT must be replaced with this
C          new version.  This generator produces values of SRAD, TMAX,
C          TMIN, and RAIN.  TDEW is estimated from TMIN.  WINDSP is
C          set at 1 m/s.  Later versions will include the appropriate
C          WGEN/SIMMETEO code to generate these variables.
C=======================================================================

      SUBROUTINE WGEN(CONTROL,
     &    FILEW, MEWTH, MULTI, RUN, PATHWT, REPNO,        !Input
     &    RNMODE, RSEED1, YRDOY, YRSIM,                   !Input
     &    PAR, RAIN, RSEED, SRAD, TAMP, TAV,              !Output
     &    TMAX, TMIN, XLAT, XLONG, YREND)                 !Output
      
!     2023-01-26 chp removed unused variables in argument list:
!       TDEW, WINDSP

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL YR_DOY, ERROR, WGENIN, SIMMIN, YDOY, MTHMID, INCMTH, 
     &  MTHEND, WGENPM, WGENGN, DAILYWEATHERCHECK

      SAVE Y,CF,MTH,MM,YMID,YMEND,WETPRV,XPRV

      CHARACTER*1 MEWTH, RNMODE
      CHARACTER*6 ERRKEY
      CHARACTER*12 FILEW
      CHARACTER*80 PATHWT

      INTEGER DOY,I,M,NM,MTH,MM,YY,YR,YRDOY,YRSIM,YMID,YMEND,
     &  YDOY,RSEED1,RSEED(4),MTHMID,MTHEND,WETPRV
      INTEGER MULTI, RUN, REPNO, YREND
      INTEGER DYNAMIC

      PARAMETER (NM=12, M=14, ERRKEY = 'WGEN  ')

      REAL Y(NM,M),CF(NM,M),SRDMN,SRDSD,SRWMN,SRWSD,TXDMN,TXDSD,
     &  TXWMN,TXWSD,TNAMN,TNASD,PWW,PDW,ALPHA,BETA,PAR,SRAD, 
     &  TMAX,TMIN,RAIN,XLAT,XLONG,XPRV(3) !,TDEW, WINDSP
      REAL TAV, TAMP, RHUM

      TYPE (ControlType) CONTROL
      DYNAMIC = CONTROL % DYNAMIC

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------

C     Input and initialize if first day.
      RHUM = -99.

      IF ((INDEX('IABDNGSEC',RNMODE) .GT. 0 .AND. MULTI .EQ. 1) 
     &  .OR.(INDEX('QF',RNMODE).GT.0 .AND. RUN.EQ.1 .AND. REPNO.EQ.1) 
     &  .OR.(INDEX('Y',RNMODE).GT.0 .AND. CONTROL % ENDYRS .EQ. 1)) THEN
        IF (RSEED1 .GT. 0) THEN
          RSEED(1) = RSEED1
        ELSE
          RSEED(1) = 2510
        ENDIF
        RSEED(2) = 7692
        RSEED(3) = 2456
        RSEED(4) = 3765
      ENDIF

!-----------------------------------------------------------------------
!     Two methods of calculating Y, monthly input factors and CF, monthly
!     correction factors for weather generator
!-----------------------------------------------------------------------
        IF (MEWTH .EQ. 'W') THEN
          CALL WGENIN(
     &        FILEW, PATHWT,                              !Input
     &        CF, TAMP, TAV, XLAT, XLONG, Y)              !Output

        ELSE IF (MEWTH .EQ. 'S') THEN
          CALL SIMMIN (
     &        FILEW, PATHWT,                              !Input
     &        CF, TAMP, TAV, XLAT, XLONG, Y)              !Output

        ELSE
          CALL ERROR(ERRKEY,1,' ',0)
        ENDIF
        WETPRV = 0

        DO 10 I=1,3
          XPRV(I) = 0.0
   10   ENDDO

      ENDIF

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT .OR. DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
      CALL YR_DOY(YRDOY,YR,DOY)
      YY = YR

C     Reset month if mid-month boundary is crossed.

      IF (YRDOY .EQ. YRSIM .OR. YRDOY .GT. YMID) THEN
        IF (YRDOY .EQ. YRSIM) THEN
          MTH = 1
   20     IF (YRDOY .GT. YDOY(YY,MTHMID(YY,MTH))) THEN
            CALL INCMTH(YY,MTH,+1)
            GOTO 20
          ENDIF
        ELSE
          CALL INCMTH(YY,MTH,+1)
        ENDIF

        MM = 0
        YMID = YDOY(YY,MTHMID(YY,MTH))
        CALL INCMTH(YY,MTH,-1)
        YMEND = YDOY(YY,MTHEND(YY,MTH))
        CALL INCMTH(YY,MTH,+1)
      ENDIF

      IF (YRDOY .GT. YMEND) THEN
        MM = 1
        YMEND = YDOY(YY,MTHEND(YY,MTH))
      ENDIF

C     Calculate generator parameters for day.

      CALL WGENPM (
     &    CF, DOY, MM, MTH, Y, YR,                        !Input
     &    ALPHA, BETA, PDW, PWW, SRDMN,                   !Output
     &    SRDSD, SRWMN, SRWSD, TNAMN, TNASD,              !Output
     &    TXDMN, TXDSD, TXWMN, TXWSD)                     !Output

C     Generate daily weather data.

      CALL WGENGN (
     &    ALPHA, BETA, DOY, PDW, PWW, RSEED,              !Input
     &    SRDMN, SRDSD, SRWMN, SRWSD, TNAMN, TNASD,       !Input
     &    TXDMN, TXDSD, TXWMN, TXWSD, XLAT,               !Input
     &    PAR, RAIN, SRAD, TMAX, TMIN, WETPRV, XPRV)      !Input/Output

C     Set default values for dew point temp and windspeed

!      TDEW = TMIN + 2.0
!     CHP - based on Nigel Pickering's changes in IPWTH
!      TDEW = TMIN
!      WINDSP = 86.4

      CALL DailyWeatherCheck(CONTROL, 
     &    ERRKEY, FILEW, RAIN, 0,                         !Input
     &    SRAD, TMAX, TMIN, YRDOY,                        !Input
     &    YREND)                                          !Output

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE WGEN
!-----------------------------------------------------------------------
! WGEN Variables:
!-----------------------------------------------------------------------
! ALPHA     Gamma distribution skewness factor linearly interpolated for 
!             current date (0 - 1) Default value 0.44 
! BETA      Gamma distribution factor for daily rainfall depth (mm)
! CF(12,14) Array of monthly correction factors for weather generator (see 
!             Y) (varies)
! DOY       Current day of simulation (d)
! ERRKEY    Subroutine name for error file 
! FILEW     Weather data file 
! INCMTH    Function subroutine increases/decreases month and adjusts YR 
!             based on variable INC 
! M         Fertilizer type for current application; or for weather module, 
!             M=14, array counter size of weather generator values 
! MEWTH     Switch for method of obtaining weather data-- 'G' or 'M'- read 
!             weather data file 'S'- read SIMMETEO inputs and generate 
!             weather data 'W'- read WGEN inputs and generate weather data 
! MM        Midmonth boundary (da)
! MTH       Month 
! MTHEND    Integer function calculates day-of-year that is end of month
!             (d)
! MTHMID    Integer function calculates day-of-year that is midpoint of 
!             month (d)
! MULTI     Current simulation year (=1 for first or single simulation, 
!             =NYRS for last seasonal simulation) 
! NM         NM=12, Array size for monthly weather  generator values (mo)
! RUN      Report number for sequenced or multi-season runs 
! PAR       Daily photosynthetically active radiation or photon flux 
!             density (moles[quanta]/m2-d)
! PATHWT    Directory path for weather file 
! PDW       Probability that day i will be wet given that day i-1 is dry 
!             (interpolated for current day) 
! PWW       Probability that day i will be wet given that day i-1 is wet 
! RAIN      Precipitation depth for current day (mm)
! RNMODE    Simulation run mode (I=Interactive, A=All treatments, 
!             B=Batch mode, E=Sensitivity, D=Debug, N=Seasonal, Q=Sequence)
! RSEED(4)  Random number generator seeds 
! RSEED1    Random number generator seed- user input 
! SRAD      Solar radiation (MJ/m2-d)
! SRAMN(12) Mean monthly value of SRAD 
! SRDSD     SRAD standard deviation for dry day (MJ/m2-d)
! SRWMN     SRAD mean value for wet day (MJ/m2-d)
! SRWSD     SRAD standard deviation for wet day (MJ/m2-d)
! TDEW      Dewpoint temperature (°C)
! TMAX      Maximum daily temperature (°C)
! TMIN      Minimum daily temperature (°C)
! TNAMN(12) Minimum temperature mean value (°C)
! TNASD     Minimum temperature standard deviation (°C)
! TXDMN     Maximum temperature mean value for dry day (°C)
! TXDSD     Maximum temperature standard deviation for dry day (°C)
! WETPRV    Switch for previous day's rainfall 0=NONE 1=RAINFALL 
! WINDSP    Wind speed (km/d)
! XLAT      Latitude (deg.)
! XLONG     Longitude (deg.)
! XPRV      Previous day's values of X 
! Y(12,14)  Array of monthly input values for weather generator M= 1:SRDMN 
!             2:SRDSD 3:SRWMN 4:SRWSD 5:TXDMN 6:TXDSD 7:TXWMN 8:TXWSD 
!             9:TNAMN 10:TNASD 11:ALPHA 12:BETA 13:PDW 14:PWW 
! YDOY      Integer function converts YR and DOY to combined date, YRDOY
!             (YYDDD)
! YMEND     Last day of month (YYDDD)
! YMID      Midpoint of month (YYDDD)
! YR        Year portion of date 
! YR_DOY    Function subroutoine converts date in YYDDD format to integer 
!             year (YY) and day (DDD). 
! YRDOY     Current day of simulation (YYDDD)
! YRSIM     Start of simulation date (YYDDD)
! YY        Used to linearly interpolate the difference in partitioning to 
!             leaves between stages R1 and NDLEAF 
C=======================================================================

C=======================================================================
C  WGENIN,  Subroutine, N.B. Pickering
C  Reads WGEN inputs from parameter file and creates two matrices:
C  inputs and correction factors.
!  Called once per season
C  1=SRDMN, 2=SRDSD, 3=SRWMN, 4=SRWSD,  5=TXDMN, 6=TXDSD, 7=TXWMN,
C  8=TXWSD, 9=TNAMN, 10=TNASD, 11=ALPHA, 12=BETA, 13=PDW, 14=PWW
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/02/1992 NBP Written for DSSAT V3.
C  08/12/2003 CHP Added I/O error checking
!  06/23/2005 CHP Change variable "N" to "NM" to avoid future conflict 
!                 with global variable "N"
!  11/12/2005 CHP Added read for TAV, TAMP
!  02/04/2022 FO  Added code for LAT,LONG and ELEV output in Summary.OUT
!-----------------------------------------------------------------------
!  Called by: WGEN
!  Calls:     CFACT
!-----------------------------------------------------------------------
C  Input : PATHWT,FILEW,XLAT,XLONG,Y,CF
C  Output: Y,CF
C  Local :
C=======================================================================

      SUBROUTINE WGENIN(
     &    FILEW, PATHWT,                                  !Input
     &    CF, TAMP, TAV, XLAT, XLONG, Y)                  !Output

!-----------------------------------------------------------------------
      USE ModuleData
      USE SumModule
      IMPLICIT NONE
      EXTERNAL ERROR, GETLUN, FIND, IGNORE, SUMVALS, CFACT

      CHARACTER BLANK*1,FILEW*12,FILEWW*92,LINE*100
      CHARACTER*6 ERRKEY,FINDCH
      CHARACTER*8 WSTAT
      CHARACTER*9 CELEV
      CHARACTER*15 CXCRD, CYCRD
      CHARACTER*80 PATHWT

      INTEGER NM,M,I,J,MTH,FOUND,LNUM,NUMDAY(12),LUNCLI,ERRNUM
      INTEGER PATHL

      PARAMETER (NM=12, M=14, ERRKEY='WGENIN', BLANK = ' ')

      REAL Y(NM,M),CF(NM,M),PW,RNUM,RTOT,RWET,XLAT,XLONG,ALPHA,BETA,
     &  PDW,PWW, ELEV
      REAL TAV, TAMP
      
!     Arrays which contain data for printing in SUMMARY.OUT file
      INTEGER, PARAMETER :: SUMNUM = 3
      CHARACTER*4, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

!      DATA NUMDAY/31,28.25,31,30,31,30,31,31,30,31,30,31/
!     NUMDAY is INTEGER - change to REAL?? CHP
      DATA NUMDAY/31,28,31,30,31,30,31,31,30,31,30,31/
!-----------------------------------------------------------------------
      LNUM = 1
      PATHL  = INDEX(PATHWT,BLANK)

      IF (PATHL .LE. 1) THEN
          FILEWW = FILEW
      ELSE
         FILEWW = PATHWT(1:(PATHL-1)) // FILEW
      ENDIF

      CALL GETLUN('FILEW', LUNCLI)
      OPEN (LUNCLI,FILE=FILEWW,STATUS='OLD',IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEW,LNUM)
      WSTAT = FILEW(1:8)
      CALL PUT('WEATHER','WSTA',WSTAT)

      FINDCH = '*CLIMA'
      CALL FIND (LUNCLI,FINDCH,LNUM,FOUND)
      IF (FOUND .EQ. 1) THEN
        CALL IGNORE(LUNCLI,LNUM,FOUND,LINE)
        IF (FOUND .NE. 1) CALL ERROR(ERRKEY,-1,FILEW,LNUM)
!        READ (LINE,'(6X,2F9.0)',IOSTAT=ERRNUM) XLAT,XLONG
        READ (LINE,'(6X,2F9.0,F6.0,2F6.0)',IOSTAT=ERRNUM) 
     &            XLAT, XLONG, ELEV, TAV, TAMP
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEW,LNUM+1)
        REWIND(LUNCLI)
      ELSE
        !CALL ERROR(ERRKEY,-1,FILEW,LNUM)
        CALL ERROR(FINDCH, 42, FILEW,LNUM)
      ENDIF
      
!  05/28/2021 FO  Added code for LAT,LONG and ELEV in Summary.OUT
!     Check if LAT and LONG are correct in FileX
      IF(SUMDAT%YCRD .LE. -99.0 .OR. SUMDAT%XCRD .LE. -999.0) THEN
        
        WRITE(CYCRD,'(F9.4)') XLAT
        WRITE(CXCRD,'(F9.4)') XLONG
        WRITE(CELEV,'(F6.0)') ELEV
        
        IF(XLAT .GE. -90.0 .AND. XLAT .LE. 90.0 .AND.
     &     XLONG .GE.-180.0 .AND. XLONG .LE. 180.0 .AND.
     &   LEN_TRIM(CYCRD).GT.0.0 .AND. LEN_TRIM(CXCRD).GT.0.0
     &   .AND.
     &   (ABS(XLAT) .GT. 1.E-15 .OR. ABS(XLONG) .GT. 1.E-15))THEN
          !     Transfer data to the modules
         CALL PUT('FIELD','CYCRD',CYCRD)
         CALL PUT('FIELD','CXCRD',CXCRD)      
         LABEL(1) = 'YCRD'; VALUE(1) = XLAT 
         LABEL(2) = 'XCRD'; VALUE(2) = XLONG
        ELSE
          !     Transfer data to the modules
          CALL PUT('FIELD','CYCRD','            -99')
          CALL PUT('FIELD','CXCRD','            -99')
          LABEL(1) = 'YCRD'; VALUE(1) = -99.0 
          LABEL(2) = 'XCRD'; VALUE(2) = -999.0 
        ENDIF 
      ENDIF

!     Check if ELEV are correct in FileX      
      IF(SUMDAT%ELEV .LE. -99.0) THEN
        IF(ELEV .GT. -99.0 .AND. LEN_TRIM(CELEV) .GT. 0.0) THEN
          CALL PUT('FIELD','CELEV',CELEV)
          LABEL(3) = 'ELEV'; VALUE(3) = ELEV
        ELSE
          CALL PUT('FIELD','CELEV','      -99')
          LABEL(3) = 'ELEV'; VALUE(3) = -99.0
        ENDIF
      ENDIF
      
C     Send labels and values to OPSUM      
      CALL SUMVALS (SUMNUM, LABEL, VALUE)
            

      REWIND(LUNCLI)
      FINDCH = '*WGEN '
      CALL FIND (LUNCLI,FINDCH,LNUM,FOUND)
      IF (FOUND .EQ. 1) THEN
        DO 10 MTH=1,12
          CALL IGNORE(LUNCLI,LNUM,FOUND,LINE)
          IF (FOUND .NE. 1) CALL ERROR(ERRKEY,-1,FILEW,LNUM)
          READ (LINE,'(I6,14(1X,F5.0))',IOSTAT=ERRNUM)
     &      I,(Y(MTH,J),J=1,10),ALPHA,RTOT,PDW,RNUM
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEW,LNUM)

!         Check for missing data
          DO I = 1, 10
            IF (Y(MTH,I) < -90.) CALL ERROR(ERRKEY,4,FILEW,LNUM)
          ENDDO

!         Check for RNUM or RTOT missing
          IF (RNUM < -90. .OR. RTOT < -90.) THEN
            CALL ERROR(ERRKEY,4,FILEW,LNUM)

C         Compute PWW from PW and PDW and BETA [Y(MTH,M)] from RTOT
C         and ALPHA [Y(MTH,M-1)]. (ALPHA = 0.44 ==> skewness = 3.0)
          ELSEIF (ABS(RNUM) < 1.E-4 .OR. ABS(RTOT) < 1.E-4) THEN
            PW = 0.0
            PDW = 0.0
            PWW = 0.0
            ALPHA = 0.44
            BETA = 0.0

          ELSE
            PW = RNUM / NUMDAY(MTH)
            PWW = 1.0 - PDW*(1.0-PW)/PW
            PWW = MIN(MAX(PWW,0.0),1.0)
            ALPHA = MIN(MAX(ALPHA, 0.01),0.998)
            RWET  = RTOT / RNUM
            BETA  = MAX(RWET/ALPHA, 0.0)
          ENDIF

          Y(MTH,11)  = ALPHA
          Y(MTH,12)  = BETA
          Y(MTH,13)  = PDW
          Y(MTH,14)  = PWW

   10   ENDDO

      ELSE
        CALL ERROR(FINDCH, 42, FILEW,LNUM)
      ENDIF

      CALL CFACT(Y,CF)
      CLOSE(LUNCLI)

      RETURN
      END SUBROUTINE WGENIN

C=======================================================================
C  SIMMIN,  Subroutine, N.B. Pickering
C  Reads SIMMETEO inputs from parameter file and creates two matrices:
!  Called once per season
C  1=SRDMN, 2=SRDSD, 3=SRWMN, 4=SRWSD,  5=TXDMN, 6=TXDSD, 7=TXWMN,
C  8=TXWSD, 9=TNAMN, 10=TNASD, 11=ALPHA, 12=BETA, 13=PDW, 14=PWW
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/02/1992 NBP Written for DSSAT V3.
C  08/12/2003 CHP Added I/O error checking
!  06/23/2005 CHP Change variable "N" to "NM" to avoid future conflict 
!                 with global variable "N"
!  11/12/2005 CHP Added read for TAV, TAMP
!  02/04/2022 FO  Added code for LAT,LONG and ELEV output in Summary.OUT
!-----------------------------------------------------------------------
!  Called by: WGEN
!  Calls:     AVG, CFACT, FOURCF, FOURIN
!-----------------------------------------------------------------------
C  Input : PATHWT,FILEW,XLAT,XLONG,Y,CF
C  Output: Y,CF
C  Local :
C=======================================================================

      SUBROUTINE SIMMIN(
     &    FILEW, PATHWT,                                  !Input
     &    CF, TAMP, TAV, XLAT, XLONG, Y)                  !Output

!-----------------------------------------------------------------------
      USE ModuleData
      USE SumModule
      IMPLICIT NONE
      EXTERNAL ERROR, GETLUN, FIND, IGNORE, SUMVALS, CFACT, AVG, 
     &  FOURCF, FOURIN

      CHARACTER BLANK*1,FILEW*12,FILEWW*92,LINE*100
      CHARACTER*6 ERRKEY,FINDCH
      CHARACTER*8 WSTAT
      CHARACTER*9 CELEV
      CHARACTER*15 CXCRD, CYCRD
      CHARACTER*80 PATHWT

      INTEGER NM,M,I,MTH,FOUND,LNUM,NUMDAY(12),LUNCLI,ERRNUM, PATHL

      PARAMETER (NM=12, M=14, ERRKEY='SIMMIN', BLANK = ' ')

      REAL Y(NM,M),CF(NM,M),PW(12),PDW(12),RNUM,RTOT,RWET,XLAT,XLONG,
     &  AVG,PWMN,PDWMN,SRDMNF(3),SRDCVF(3),SRWMNF(3),SRWCVF(3),
     &  SRAMNF(3),TXDMNF(3),TXWMNF(3),TXAMNF(3),SAVG,SDIFF,TDIFF,
     &  FOURIN,SRAMN(12),SRDCV,SRDMN,SRDSD,SRWCV,SRWMN,SRWSD,TNAMN(12),
     &  TNASD,TXAMN(12),TXDMN,TXDSD,TXWMN,TXWSD,ALPHA,BETA,PWW
      REAL TAV, TAMP, ELEV
      
!     Arrays which contain data for printing in SUMMARY.OUT file
      INTEGER, PARAMETER :: SUMNUM = 3
      CHARACTER*4, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

!      DATA NUMDAY/31,28.25,31,30,31,30,31,31,30,31,30,31/
!     NUMDAY is INTEGER - change to REAL?? CHP
      DATA NUMDAY/31,28,31,30,31,30,31,31,30,31,30,31/

!-----------------------------------------------------------------------
      LNUM = 1
      PATHL  = INDEX(PATHWT,BLANK)
      IF (PATHL .LE. 1) THEN
          FILEWW = FILEW
      ELSE
         FILEWW = PATHWT(1:(PATHL-1)) // FILEW
      ENDIF

      CALL GETLUN('FILEW', LUNCLI)
      OPEN (LUNCLI,FILE=FILEWW,STATUS='OLD',IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEW,LNUM)
      WSTAT = FILEW(1:8)
      CALL PUT('WEATHER','WSTA',WSTAT)

      FINDCH = '*CLIMA'
      CALL FIND (LUNCLI,FINDCH,LNUM,FOUND)
      IF (FOUND .EQ. 1) THEN
        CALL IGNORE(LUNCLI,LNUM,FOUND,LINE)
        IF (FOUND .NE. 1) CALL ERROR(ERRKEY,-1,FILEW,LNUM)
!        READ (LINE,'(6X,2F9.0)',IOSTAT=ERRNUM) XLAT,XLONG
        READ (LINE,'(6X,2F9.0,F6.0,2F6.0)',IOSTAT=ERRNUM) 
     &            XLAT, XLONG, ELEV, TAV, TAMP
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEW,LNUM)
        REWIND(LUNCLI)
      ELSE
        CALL ERROR(FINDCH, 42, FILEW,LNUM)
      ENDIF

!  05/28/2021 FO  Added code for LAT,LONG and ELEV in Summary.OUT
!     Check if LAT and LONG are correct in FileX
      IF(SUMDAT%YCRD .LE. -99.0 .OR. SUMDAT%XCRD .LE. -999.0) THEN
        
        WRITE(CYCRD,'(F9.4)') XLAT
        WRITE(CXCRD,'(F9.4)') XLONG
        WRITE(CELEV,'(F6.0)') ELEV
        
        IF(XLAT .GE. -90.0 .AND. XLAT .LE. 90.0 .AND.
     &     XLONG .GE.-180.0 .AND. XLONG .LE. 180.0 .AND.
     &   LEN_TRIM(CYCRD).GT.0.0 .AND. LEN_TRIM(CXCRD).GT.0.0
     &   .AND.
     &   (ABS(XLAT) .GT. 1.E-15 .OR. ABS(XLONG) .GT. 1.E-15))THEN
          !     Transfer data to the modules
         CALL PUT('FIELD','CYCRD',CYCRD)
         CALL PUT('FIELD','CXCRD',CXCRD)      
         LABEL(1) = 'YCRD'; VALUE(1) = XLAT 
         LABEL(2) = 'XCRD'; VALUE(2) = XLONG
        ELSE
          !     Transfer data to the modules
          CALL PUT('FIELD','CYCRD','            -99')
          CALL PUT('FIELD','CXCRD','            -99')
          LABEL(1) = 'YCRD'; VALUE(1) = -99.0 
          LABEL(2) = 'XCRD'; VALUE(2) = -999.0 
        ENDIF 
      ENDIF

!     Check if ELEV are correct in FileX      
      IF(SUMDAT%ELEV .LE. -99.0) THEN
        IF(ELEV .GT. -99.0 .AND. LEN_TRIM(CELEV) .GT. 0.0) THEN
          CALL PUT('FIELD','CELEV',CELEV)
          LABEL(3) = 'ELEV'; VALUE(3) = ELEV
        ELSE
          CALL PUT('FIELD','CELEV','      -99')
          LABEL(3) = 'ELEV'; VALUE(3) = -99.0
        ENDIF
      ENDIF
      
C     Send labels and values to OPSUM      
      CALL SUMVALS (SUMNUM, LABEL, VALUE) 

      FINDCH = '*MONTH'
      CALL FIND (LUNCLI,FINDCH,LNUM,FOUND)
      IF (FOUND .EQ. 1) THEN
        DO 10 MTH=1,12
          CALL IGNORE(LUNCLI,LNUM,FOUND,LINE)
          IF (FOUND .NE. 1) CALL ERROR(ERRKEY,-1,FILEW,LNUM)
          READ (LINE,'(I6,5(1X,F5.0))',IOSTAT=ERRNUM)
     &      I,SRAMN(MTH),TXAMN(MTH),TNAMN(MTH),RTOT,RNUM
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEW,LNUM)

!         Check for missing data (For SRAD - check for negative data)
          IF (SRAMN(MTH) < 0.0)  CALL ERROR(ERRKEY,4,FILEW,LNUM)
          IF (TXAMN(MTH) < -90.) CALL ERROR(ERRKEY,4,FILEW,LNUM)
          IF (TNAMN(MTH) < -90.) CALL ERROR(ERRKEY,4,FILEW,LNUM)
          IF (RTOT < -90.) CALL ERROR(ERRKEY,4,FILEW,LNUM)
          IF (RNUM < -90.) CALL ERROR(ERRKEY,4,FILEW,LNUM)

C         Compute PDW [Y(MTH,M-3)] from PW (0.82*PW in Hershfield, 1970?),
C         PWW [Y(MTH,M-2)] from PW and PDW [Y(MTH,M-3)], and BETA [Y(MTH,M)]
C         from RTOT and ALPHA [Y(MTH,M-1)]. (ALPHA = 0.44 ==> skewness = 3.0)

          IF (ABS(RNUM) < 1.E-4 .OR. ABS(RTOT) < 1.E-4) THEN
            PW(MTH) = 0.0
            PDW(MTH) = 0.0
            PWW = 0.0
            ALPHA = 0.44
            BETA  = 0.0
          ELSE
            PW(MTH) = RNUM / NUMDAY(MTH)
            PDW(MTH) = 0.75*PW(MTH)
            PWW = 1. - PDW(MTH)*(1.-PW(MTH))/PW(MTH)
            PWW = MIN(MAX(PWW,0.),1.)
            RWET  = RTOT / RNUM
C-GH        BETA  = MAX (-2.16+1.83*RWET, 0.0)
            BETA  = MAX (-2.16+1.83*RWET, 0.01)
            ALPHA = MIN(MAX(RWET/BETA, 0.01),0.998)
          ENDIF
          Y(MTH,11) = ALPHA
          Y(MTH,12) = BETA
          Y(MTH,13) = PDW(MTH)
          Y(MTH,14) = PWW
   10   ENDDO
      ELSE
        CALL ERROR(FINDCH, 42, FILEW,LNUM)
      ENDIF

      PWMN = AVG(PW,12)
      PDWMN = AVG(PDW,12)

C     Calculate solar radiation parameters (wet and dry days).
C     Coefficient calculations in Langleys then converted to MJ/m2/d.

      CALL FOURCF (SRAMN, SRAMNF)
      SAVG = 23.87 * SRAMNF(1)
      SDIFF = ABS(410. - 3.21 * XLAT - 0.350 * SAVG)
      SRDMNF(1) = (SAVG+PWMN*SDIFF) * 0.04189
      SRDMNF(2) = SRAMNF(2)
      SRDMNF(3) = SRAMNF(3)
      SRWMNF(1) = (SAVG-(1.0-PWMN)*SDIFF) * 0.04189
      SRWMNF(2) = SRAMNF(2)
      SRWMNF(3) = SRAMNF(3)
      SRDCVF(1) = 0.24
      SRDCVF(2) = -0.08
      SRDCVF(3) = SRAMNF(3)
      SRWCVF(1) = 0.48
      SRWCVF(2) = -0.13
      SRWCVF(3) = SRAMNF(3)

C     Calculate mean maximum temperature parameters (wet and dry days).
C     TDIFF calculations in Langleys then converted to MJ/m2/d.

      CALL FOURCF (TXAMN, TXAMNF)
      TDIFF = ABS(9.67-27.4*PDWMN) * 5./9.
      TXDMNF(1) = TXAMNF(1) + PWMN*TDIFF
      TXDMNF(2) = TXAMNF(2)
      TXDMNF(3) = TXAMNF(3)
      TXWMNF(1) = TXAMNF(1) - (1.-PWMN)*TDIFF
      TXWMNF(2) = TXAMNF(2)
      TXWMNF(3) = TXAMNF(3)

C     Compute monthly SDEVs from mean and CV, then compute fourier coefficients.

      DO 20 MTH=1,12
        SRDMN = FOURIN (SRDMNF,MTH)
        SRWMN = FOURIN (SRWMNF,MTH)
        SRDCV = MAX (FOURIN (SRDCVF,MTH), 0.06)
        SRWCV = MAX (FOURIN (SRWCVF,MTH), 0.06)
        SRDSD = SRDMN * SRDCV
        SRWSD = SRWMN * SRWCV

        TXDMN = FOURIN (TXDMNF,MTH)
        TXWMN = FOURIN (TXWMNF,MTH)
        TXDSD = MAX (5.8-0.09*TXDMN, 0.5)
        TXWSD = MAX (5.8-0.09*TXWMN, 0.5)
        TNASD = MAX (5.2-0.13*TNAMN(MTH), 0.5)
        Y(MTH,1) = SRDMN
        Y(MTH,2) = SRDSD
        Y(MTH,3) = SRWMN
        Y(MTH,4) = SRWSD
        Y(MTH,5) = TXDMN
        Y(MTH,6) = TXDSD
        Y(MTH,7) = TXWMN
        Y(MTH,8) = TXWSD
        Y(MTH,9) = TNAMN(MTH)
        Y(MTH,10) = TNASD
   20 ENDDO

      CALL CFACT(Y,CF)
      CLOSE(LUNCLI)

      RETURN
      END SUBROUTINE SIMMIN

C=======================================================================
C  WGENPM, Subroutine, N.B. Pickering
C  Estimates daily generator parameters
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/02/92 NBP Written for DSSAT V3.
!  06/23/2005 CHP Change variable "N" to "NM" to avoid future conflict 
!                 with global variable "N"
!-----------------------------------------------------------------------
!  Called by: WGEN
!  Calls:     CIRC, LININT
C=======================================================================
      SUBROUTINE WGENPM (
     &    CF, DOY, MM, MTH, Y, YR,                        !Input
     &    ALPHA, BETA, PDW, PWW, SRDMN,                   !Output
     &    SRDSD, SRWMN, SRWSD, TNAMN, TNASD,              !Output
     &    TXDMN, TXDSD, TXWMN, TXWSD)                     !Output
      
!-----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL CIRC, ENDYR, LININT
      SAVE

      INTEGER CIRC,DOY,ENDYR,I,NM,M,MTH,MM,YR,CC
      PARAMETER (NM=12, M=14)
      REAL SRDMN,SRDSD,SRWMN,SRWSD,TXDMN,TXDSD,TXWMN,TXWSD,TNAMN,
     &  TNASD,PWW,PDW,ALPHA,BETA,Y(NM,M),CF(NM,M),XP,YP(M)

!-----------------------------------------------------------------------
      XP = DOY / REAL(ENDYR(YR)) * 12.

      CALL LININT(MTH,Y,XP,YP)

      CC = MTH + MM - 1
      I = CIRC(CC,1,NM)
      SRDMN = YP(1) + CF(I,1)
      SRDSD = YP(2) + CF(I,2)
      SRWMN = YP(3) + CF(I,3)
      SRWSD = YP(4) + CF(I,4)
      TXDMN = YP(5) + CF(I,5)
      TXDSD = YP(6) + CF(I,6)
      TXWMN = YP(7) + CF(I,7)
      TXWSD = YP(8) + CF(I,8)
      TNAMN = YP(9) + CF(I,9)
      TNASD = YP(10) + CF(I,10)
      ALPHA = MIN(YP(11)+CF(I,11),0.998)
      BETA  = MAX(YP(12)+CF(I,12),1.1)
      PDW = MIN(MAX(YP(13)+CF(I,13),0.),1.)
      PWW = MIN(MAX(YP(14)+CF(I,14),0.),1.)

      RETURN
      END SUBROUTINE WGENPM

C=======================================================================
C  WGENGN, Subroutine, Richardson and Wright
C  Generates daily weather.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  ??/??/84     Written.
C  09/02/92 NBP Modified for DSSAT V3.
C  06/29/94  JH Added lower limit of 0.1 to calc. of rainfall.
!-----------------------------------------------------------------------
!  Called by: WGEN
!  Calls:     RANDN, RADA
!-----------------------------------------------------------------------
C  Input : DOY,RSEED,XLAT,SRDMN,SRDSD,SRWMN,SRWSD,TXDMN,TXDSD,TXWMN,
C          TXWSD,TNAMN,TNASD,PDW,PWW,ALPHA,BETA,WETPRV,XPRV
C  Output: SRAD,TMAX,TMIN,RAIN,PAR
C  Local :
C=======================================================================

      SUBROUTINE WGENGN (
     &    ALPHA, BETA, DOY, PDW, PWW, RSEED,              !Input
     &    SRDMN, SRDSD, SRWMN, SRWSD, TNAMN, TNASD,       !Input
     &    TXDMN, TXDSD, TXWMN, TXWSD, XLAT,               !Input
     &    PAR, RAIN, SRAD, TMAX, TMIN, WETPRV, XPRV)      !Input/Output

!-----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL RADA, RANDN
      SAVE

      INTEGER DOY,RSEED(4),I,WET,WETPRV,J,K
      REAL SRDMN,SRDSD,SRWMN,SRWSD,TXDMN,TXDSD,TXWMN,TXWSD,TNAMN,TNASD,
     &  PWW,PDW,ALPHA,BETA,RADA,A(3,3),B(3,3),XPRV(3),E(3),R(3),X(3),
     &  RR(3),XLAT,AA,AB,RAIN,RC,SRMN,SRMIN,RN,RN1,RN2,RN3,SRSD,S1,S2,
     &  PAR,SRAD,TMAX,TMIN,TR1,TR2,TXXMN,TXXSD,V,Z,PI,PARFAC
      REAL HOLD

      PARAMETER (PI=3.14159, PARFAC=2.0)

      DATA A/0.567,0.253,-0.006,0.086,0.504,-0.039,-0.002,-0.050,0.244/
      DATA B/0.781,0.328,0.238,0.0,0.637,-0.341,0.0,0.0,0.873/

!-----------------------------------------------------------------------
C     Calculate maximum solar radiation for each day (langleys).

      RC = RADA(DOY,XLAT) * 0.8
      SRMIN = 0.2*RC

C     Determine wet or dry day using Markov chain model.

!      IF (RAIN .EQ. -99.) THEN
      IF (RAIN .LT. 0.) THEN
        CALL RANDN (RSEED,RN)
        IF ((WETPRV.EQ.0 .AND. RN.LT.PDW) .OR. (WETPRV.EQ.1 .AND.
     &    RN.LT.PWW)) THEN

C         Determine rainfall amount for wet days using Gamma distribution

          AA = 1. / ALPHA
          AB = 1. / (1.-ALPHA)
          TR1 = EXP(-18.42/AA)
          TR2 = EXP(-18.42/AB)
          S1 = 1.
          S2 = 1.
   10     IF (S1+S2 .GT. 1.) THEN
            CALL RANDN (RSEED,RN1)
            CALL RANDN (RSEED,RN2)

            IF (RN1 .LE. TR1) THEN
              S1 = 0.
            ELSE
              S1 = RN1**AA
            ENDIF

            IF (RN2 .LE. TR2) THEN
              S2 = 0.
            ELSE
              S2 = RN2**AB
            ENDIF

          GOTO 10
          ENDIF

          Z = S1 / (S1+S2)
          CALL RANDN (RSEED,RN3)
          RAIN = MAX(-Z*ALOG(RN3)*BETA,0.1)
        ELSE
          RAIN = 0.
        ENDIF
      ENDIF

!      IF (RAIN .EQ. 0.) THEN
      IF (RAIN .LT. 0.0001) THEN
        WET = 0
      ELSE
        WET = 1
      ENDIF

C     Generate TMAX, TMIN, and SRAD.

      IF (WET .EQ. 0) THEN
        SRMN = SRDMN
        SRSD = SRDSD
        TXXMN = TXDMN
        TXXSD = TXDSD
      ELSE
        SRMN = SRWMN
        SRSD = SRWSD
        TXXMN = TXWMN
        TXXSD = TXWSD
      ENDIF

      IF (TMAX .LT. -90. .OR. TMIN .LT. -90. .OR. SRAD .LT. -90.) THEN
        K = 1
   20   IF (K .LE. 3) THEN
          CALL RANDN (RSEED,RN1)
          CALL RANDN (RSEED,RN2)
          V = SQRT(-2.*ALOG(RN1))*COS(2.*PI*RN2)
          IF (ABS(V) .LE. 2.5) THEN
            E(K) = V
            K = K + 1
          ENDIF
        GOTO 20
        ENDIF

        DO 30 I = 1,3
          R(I) = 0.
          RR(I) = 0.
   30   ENDDO

        DO 50 I = 1,3
          DO 40 J = 1,3
            R(I) = R(I) + B(I,J)*E(J)
            RR(I) = RR(I) + A(I,J)*XPRV(J)
   40     ENDDO
   50   ENDDO

        DO 60 K = 1,3
          X(K) = R(K) + RR(K)
   60   ENDDO

      ENDIF

      IF (TMAX .LT. -90.) THEN
        TMAX = X(1)*TXXSD + TXXMN
      ELSE
        X(1) = (TMAX-TXXMN) / TXXSD
      ENDIF

      IF (TMIN .LT. -90.) THEN
        TMIN = X(2)*TNASD + TNAMN
      ELSE
        X(2) = (TMIN-TNAMN) / TNASD
      ENDIF

      IF (SRAD .LT. -90.) THEN
        SRAD  = X(3)*SRSD + SRMN
        SRAD = MIN(MAX(SRAD,SRMIN),RC)
      ELSE
        X(3) = (SRAD-SRMN) / SRSD
      ENDIF

      IF (PAR .LT. -90.) PAR = SRAD * PARFAC

!     Note: the weather generator can produce TMAX > TMIN.
!     Probably need to fix the weather generator.
      IF (TMIN > TMAX) THEN
        HOLD = TMAX
        TMAX = TMIN
        TMIN = HOLD
      ENDIF

!     TMAX and TMIN need to be at least 0.1C different
!     RR: the threshold is too small...; 
!     two numbers could be c as much as 0.1 apart and round to the same value (tenths place) 
!     also, there is no need to change both, just bump one of them up enough
!     IF (TMAX - TMIN < 0.05) THEN
      IF (TMAX - TMIN .LE. 0.1) THEN
        TMAX = TMAX + 0.1
!       TMIN = TMIN - 0.1
      ENDIF

C     Update values to be remembered.
      WETPRV = WET
      DO 70 K = 1,3
        XPRV(K) = X(K)
   70 ENDDO

      RETURN
      END SUBROUTINE WGENGN

C=======================================================================
C  RANDN, Subroutine, Richardson and Wright
C  Generates a uniform random number (0-1)
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  ??/??/84     Written.
C  09/02/92 NBP Modified for DSSAT V3.
!-----------------------------------------------------------------------
!  Called by: WGENGN
!  Calls:     None
!-----------------------------------------------------------------------
C  Input : RSEED
C  Output: RSEED, RN
C  Local :
C=======================================================================

      SUBROUTINE RANDN (RSEED,RN)

!-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,RSEED(4)
      REAL RN

!-----------------------------------------------------------------------
      RSEED(4) = 3*RSEED(4)+RSEED(2)
      RSEED(3) = 3*RSEED(3)+RSEED(1)
      RSEED(2) = 3*RSEED(2)
      RSEED(1) = 3*RSEED(1)
      I = RSEED(1)/1000
      RSEED(1) = RSEED(1)-I*1000
      RSEED(2) = RSEED(2) + I
      I = RSEED(2)/100
      RSEED(2) = RSEED(2)-100*I
      RSEED(3) = RSEED(3)+I
      I = RSEED(3)/1000
      RSEED(3) = RSEED(3)-I*1000
      RSEED(4) = RSEED(4)+I
      I = RSEED(4)/100
      RSEED(4) = RSEED(4)-100*I
      RN = (((RSEED(1)*.001+RSEED(2))*.01+RSEED(3))*.001+RSEED(4))*.01

      RETURN
      END SUBROUTINE RANDN

C=======================================================================
C  LININT, Subroutine, N.B. Pickering
C  Linearly interpolates between monthly values.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/02/92 NBP Written for DSSAT V3.
!  06/23/2005 CHP Change variable "N" to "NM" to avoid future conflict 
!                 with global variable "N"
!-----------------------------------------------------------------------
!  Called by: WGENPM
!  Calls:     CIRC
!-----------------------------------------------------------------------
C  Input : MTH,Y,XP
C  Output: YP
C  Local :
C=======================================================================

      SUBROUTINE LININT(MTH,Y,XP,YP)

!-----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL CIRC
      INTEGER J,NM,M,MTH,PRV,CIRC,CC
      PARAMETER (NM=12, M=14)
      REAL DIFX,X,Y(NM,M),XP,YP(M)

!-----------------------------------------------------------------------
      CC  = MTH - 1
      PRV = CIRC(CC,1,NM)
      X = MTH - 1.5
      DIFX = XP - X
      IF (DIFX .GT. FLOAT(NM)) DIFX = DIFX - FLOAT(NM)
      DO J = 1, M
        YP(J) = Y(PRV,J) + (Y(MTH,J)-Y(PRV,J))*DIFX
      END DO

      RETURN
      END SUBROUTINE LININT

C=======================================================================
C  CFACT, Subroutine, N.B. Pickering
C  Calculates correction factor for month based on three consecutive
C  monly values assuming daily values were linearly interpolated.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/02/92 NBP Written for DSSAT V3.
!  06/23/2005 CHP Change variable "N" to "NM" to avoid future conflict 
!                 with global variable "N"
!-----------------------------------------------------------------------
!  Called by: WGENIN, SIMMIN
!  Calls:     CIRC
!-----------------------------------------------------------------------
C  Input : Y
C  Output: CF
C  Local :
C=======================================================================

      SUBROUTINE CFACT(Y,CF)

!-----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL CIRC
      INTEGER I,J,NM,M,NXT,PRV,CIRC,CC
      PARAMETER (NM=12, M=14)
      REAL Y(NM,M),CF(NM,M)

!-----------------------------------------------------------------------
      DO J = 1, M
        DO I = 1, NM
          CC = I - 1
          PRV = CIRC(CC,1,NM)
          CC = I + 1
          NXT = CIRC(CC,1,NM)
          CF(I,J) = Y(I,J) - (0.25*(Y(PRV,J)+2.*Y(I,J)+Y(NXT,J)))
        END DO
      END DO

      RETURN
      END SUBROUTINE CFACT

C=======================================================================
C  CIRC, Integer Function, N.B. Pickering
C  Creates circular sequence from L to U eg. 1,2,3,1,2,3 etc.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/02/92 NBP Written for DSSAT V3.
!-----------------------------------------------------------------------
!  Called by: CFACT, LININT, WGENPM
!  Calls:     None
!-----------------------------------------------------------------------
C  Input : I, L, U
C  Output: CIRC
C  Local :
C=======================================================================

      INTEGER FUNCTION CIRC(I,L,U)

!-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,L,U

!-----------------------------------------------------------------------
      IF (I .LT. L) THEN
        I = I + (U - L + 1)
      ELSE IF (I .GT. U) THEN
        I = I - (U - L + 1)
      ENDIF
      CIRC = I

      RETURN
      END FUNCTION CIRC

C=======================================================================
C  AVG, Real Function, N. B. Pickering
C  Calculates average of array.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  10/11/90 NBP Written.
!-----------------------------------------------------------------------
!  Called by: SIMMIN
!  Calls:     None
!-----------------------------------------------------------------------
C  Input : X, N
C  Output: AVG
C  Local :
C=======================================================================

      REAL FUNCTION AVG(X,N)

!-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,N
      REAL X(N),SUM

!-----------------------------------------------------------------------
      SUM = 0.
      DO 10 I=1,N
        SUM = SUM + X(I)
10    ENDDO
      AVG = SUM / N

      RETURN
      END FUNCTION AVG

C=======================================================================
C  FOURCF, Subroutine, N. B. Pickering
C  Computes coefficients for 1ST order Fourier eqn. using AMP and
C  PHASE shift form.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/02/92 NBP Written for DSSAT V3.
!-----------------------------------------------------------------------
!  Called by: SIMMIN
!  Calls:     None
!-----------------------------------------------------------------------
C  Input : YDATA
C  Output: COEFF
C  Local :
C=======================================================================

      SUBROUTINE FOURCF(YDATA,COEFF)

!-----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER M
      REAL COEFF(3),A1,A2,T,PI,RAD,SUM0,SUM1,SUM2,X,YDATA(12)
      PARAMETER (PI=3.14159, RAD=2.*PI/12.)

!-----------------------------------------------------------------------
      SUM0 = 0.0
      SUM1 = 0.0
      SUM2 = 0.0
      DO 40 M = 1,12
        X = RAD * (M-0.5)
        SUM0 = SUM0 + YDATA(M)
        SUM1 = SUM1 + YDATA(M)*COS(X)
        SUM2 = SUM2 + YDATA(M)*SIN(X)
   40 CONTINUE
      A1  = SUM1 / 6.
      A2  = SUM2 / 6.
      T = ATAN2(A2,A1)
      IF (T .LT. 0.) T = T + 2.*PI
      COEFF(1) = SUM0 / 12.
      COEFF(2) = A1 / COS(T)
      COEFF(3) = T

      RETURN
      END SUBROUTINE FOURCF

C=======================================================================
C  FOURIN, Subroutine, N. B. Pickering
C  Calculates a monthly value using 1st order Fourier eqn.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/02/92 NBP Written for DSSAT V3.
!-----------------------------------------------------------------------
!  Called by: SIMMIN
!  Calls:     None
!-----------------------------------------------------------------------
C  Input : COEFF, M
C  Output: FOURIN
C  Local :
C=======================================================================

      REAL FUNCTION FOURIN(COEFF,M)

!-----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER M
      REAL COEFF(3),PI,RAD,X
      PARAMETER (PI=3.14159, RAD=2.*PI/12.)

!-----------------------------------------------------------------------
      X = RAD * (M-0.5)
      FOURIN = COEFF(1) + COEFF(2)*COS(X-COEFF(3))

      RETURN
      END FUNCTION FOURIN

C=======================================================================
C  RADA, Real Function, N.B. Pickering
C  Computes extraterrestrial radiation (Spitters, 1986).  Accuracy 0.3%
C  based on Tables in List (1984) adjusted for new SC value 1370 W/m2.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/02/92 NBP Written for DSSAT V3.
!-----------------------------------------------------------------------
!  Called by: WGENGN
!  Calls:     None
!-----------------------------------------------------------------------
C  Input : DOY, XLAT
C  Output: RADA
C  Local :
C=======================================================================

      REAL FUNCTION RADA(DOY,XLAT)

!-----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER DOY
      REAL C1,CCOS,DAYL,DEC,DSINB,PI,RAD,S0D,SC,SOC,SSIN,T,XLAT
      PARAMETER (PI = 3.14159, RAD = PI/180.)

!-----------------------------------------------------------------------
C     Initialization

      T = 2.*PI*(DOY+10.)/365.
      C1 = COS(T)

C     Calculation of declination of sun (Eqn. 16) and sun angles.
C     SOC limited for latitudes above polar circles.

      DEC = - 23.45 * C1
      SSIN = SIN(RAD*DEC) * SIN(RAD*XLAT)
      CCOS = COS(RAD*DEC) * COS(RAD*XLAT)
      SOC = SSIN / CCOS
      SOC = MIN(MAX(SOC,-1.),1.)

C     Calculate daylength (Eqn. 17), integral of SINB (s) (Eqn. 18),
C     solar constant (Eqn. 1).

      DAYL = 12. + 24.*ASIN(SOC)/PI
      DSINB = 3600. * (DAYL*SSIN + 24./PI*CCOS*SQRT(1.-SOC**2))
C-GH  SC = 1370. * (1. + 0.033*COS(2.*PI*DOY/365.))
      SC = 1368. * (1. + 0.033*COS(2.*PI*DOY/365.))

C     Calculate extra-terrestial radiation.

      S0D = SC * DSINB
      RADA = S0D / 1.E6

      RETURN
      END FUNCTION RADA
