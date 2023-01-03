C=====================================================================
C  AUTPLT, Subroutine
C-----------------------------------------------------------------------
C  Determines automatic planting date
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  03/01/1991 WTB Written
C  12/30/1996 GH  Revised
C  12/01/1999 CHP Modular format.
C  07/01/2000 GH  Incorporated in CROPGRO
C  04/20/2002 GH  Modified for sequence analysis
C  06/11/2002 GH  Modified for Y2K
C  08/01/2002 CHP Merged RUNINIT and SEASINIT into INIT section
C  08/20/2002 GH  Added YRDIF as a constructred variable
!  06/10/2005 CHP Fixed initialization for automatic planting
!  12/05/2014 DK  Forced planting for automatic planting ("F" option)
!  04/14/2021 CHP Added CONTROL % CropStatus
C-----------------------------------------------------------------------
C  INPUT : JUL,DLAYR,LL,DUL,SW,ST,PWDINF,PWDINL,SWPLTL,
C          SWPLTH,SWPLTD,PTX,PTTN,YRPLT
C
C  LOCAL : ANS,AVGSW,CUMSW,DTRY,I,TSDEP,XDEP,XDEPL
C
C  OUTPUT : YRPLT
C---------------------------------------------------------------------
C                         DEFINITIONS
C  PWDINF,PWDINL : First and last day of year for planting window
C  PTTN,PTX : Minimum and maximum soil temperature limits, centigrade
C  TSDEP : Computed soil temperature to 10 cm depth
C  SWPLTL,SWPLTH : Lower and upper limits on soil moisture, percent
C  SWPLTD : Depth (cm) to which average soil moisture is determined
C  CUMSW : Cumulative soil moisture
C  AVGSW : Average soil moisture percent to depth SWPLTD
C  YRPLT : Computed planting date
C=====================================================================

      SUBROUTINE AUTPLT (CONTROL, ISWWAT,
     &    DLAYR, DUL, FLOOD, IDETO, IPLTI, LL, ST, SW,    !Input
     &    MDATE, YRPLT)                                   !Output

      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL IPAPLT, GETLUN, YR_DOY, WARNING, ERROR, FIND
      SAVE

      CHARACTER*1  IDETO, RNMODE, IPLTI, ISWWAT
      CHARACTER*2  CROP
      CHARACTER*6, PARAMETER :: ERRKEY = 'AUTPLT'
      CHARACTER*78 MESSAGE(10)    !Up to 10 lines of text to be output

      INTEGER DOY, I, IPLT, MULTI, NOUTDO, PWDINF, PWDINL
      INTEGER YEAR, YR, YRDOY, MDATE, YRPLT, IDATE, YRSIM, YRDIF
      INTEGER DYNAMIC,YRO,ISIM, PWDPLT,PWDYR, MNUM

      REAL AVGSW, CUMSW, DTRY, PTTN, PTX
      REAL SWPLTD, SWPLTL, SWPLTH, TSDEP, XDEP, XDEPL
      REAL DLAYR(NL), DUL(NL), LL(NL), ST(NL), SW(NL)

      LOGICAL SOILWOK, SOILTOK

!     Added for RICE
      CHARACTER*1  PLME
      INTEGER PUDLTIME
      REAL FLOOD

!     The variable "CONTROL" is of constructed type "ControlType" as 
!     defined in ModuleDefs.for, and contains the following variables.
!     The components are copied into local variables for use here.
      TYPE (ControlType) CONTROL
      DYNAMIC = CONTROL % DYNAMIC
      RNMODE  = CONTROL % RNMODE
      MULTI   = CONTROL % MULTI
      YRDIF   = CONTROL % YRDIF
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM

C***********************************************************************
C***********************************************************************
C    Input and Initialization 
C***********************************************************************
      IF (DYNAMIC .EQ. INIT) THEN
C-----------------------------------------------------------------------

C***********************************************************************
C***********************************************************************
C    Run Initialization - Called once per simulation
C***********************************************************************
!      IF (DYNAMIC .EQ. RUNINIT) THEN
C-----------------------------------------------------------------------
      CALL IPAPLT (CONTROL, 
     &    CROP, PLME, PTX, PTTN, PWDINF, PWDINL,          !Output 
     &    SWPLTL, SWPLTH, SWPLTD, YRPLT)                  !Output

      IF (CROP .EQ. 'FA') THEN
         YRPLT = YRSIM
      ELSEIF (INDEX('AF',IPLTI) /= 0) THEN
         YRPLT = -9999999
      ENDIF

      CALL GETLUN('OUTO', NOUTDO)

C***********************************************************************
C***********************************************************************
C     Seasonal initialization - run once per season
C***********************************************************************
!      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
C     If second or subsequent run of seasonal run (MULTI>1) then re-read
C     PWDINF and PWDINL and modify values for this season.  
C-----------------------------------------------------------------------
!      IF (MULTI .GT. 1 .OR. RNMODE .EQ. 'Q') THEN
!        CALL IPAPLT (CONTROL, 
!     &    CROP, PTX, PTTN, PWDINF, PWDINL,                !Output 
!     &    SWPLTL, SWPLTH, SWPLTD, YRPLT)                  !Output

C-----------------------------------------------------------------------
C     From MRUN - adjust dates for seasonal runs.
C-----------------------------------------------------------------------
        IF (MULTI .GT. 1) THEN
          IF (YRPLT .GT. 0) THEN 
            CALL YR_DOY(YRPLT,YR,IPLT)
            YRPLT = (YR + MULTI - 1) * 1000 + IPLT
          ENDIF

          IF (PWDINF .GT. 1) THEN 
            CALL YR_DOY(PWDINF,YR,IDATE)
            PWDINF = (YR +  MULTI - 1) * 1000 + IDATE
          ENDIF

          IF (PWDINL .GT. 1) THEN 
            CALL YR_DOY(PWDINL,YR,IDATE)
            PWDINL = (YR +  MULTI - 1) * 1000 + IDATE
           ENDIF
        ENDIF

        IF (CROP .EQ. 'FA') THEN
          YRPLT = YRSIM
        ELSEIF (INDEX('AF', IPLTI) /= 0) THEN
          YRPLT = -9999999
        ENDIF

C-----------------------------------------------------------------------
C     From DATECS - modify PWDINF and PWDINL dates for sequenced runs
C-----------------------------------------------------------------------
        IF (RNMODE .EQ. 'Q') THEN
C-----------------------------------------------------------------------
C         YRDIF based on YRPLT or PWDINF
C-----------------------------------------------------------------------
          IF (YRPLT .GT. 0 .AND. CROP .NE. 'FA') THEN
            CALL YR_DOY(YRSIM, YR, ISIM)
            CALL YR_DOY(YRPLT, YRO, IPLT)
            YRDIF = YR - YRO
            YRPLT = (YRO + YRDIF) * 1000 + IPLT
            IF (YRPLT .LT. YRSIM) THEN
              YRDIF = YRDIF + 1
              YRPLT = (YRO + YRDIF) * 1000 + IPLT
            ENDIF
            IF (YRPLT .LT. YRSIM) THEN
              YRPLT = YRSIM
            ENDIF
            CONTROL % YRDIF   = YRDIF
          ENDIF
          IF (IPLTI .EQ. 'R' .OR.
     &       (INDEX('AF',IPLTI) > 0 .AND. YRPLT > 0)) THEN
            IF (PWDINF .LT. YRSIM) THEN
              CALL YR_DOY(PWDINF,YRO,IDATE)
              PWDINF = (YRO +  YRDIF) * 1000 + IDATE
            ENDIF
            IF (PWDINL .LT. YRSIM) THEN
              CALL YR_DOY(PWDINL,YRO,IDATE)
              PWDINL = (YRO +  YRDIF) * 1000 + IDATE
            ENDIF
          ELSE IF (INDEX('AF',IPLTI) > 0 .AND. YRPLT < 0) THEN  
             CALL YR_DOY(YRSIM, YR, ISIM)
             CALL YR_DOY(PWDINL,YRO,IDATE)
             YRDIF = YR - YRO
             PWDINL = (YRO +  YRDIF) * 1000 + IDATE
             IF (PWDINL .LT. YRSIM) THEN
               YRDIF = YRDIF + 1
               PWDINL = (YRO +  YRDIF) * 1000 + IDATE
             ENDIF
             CALL YR_DOY(PWDINF,YRO,IDATE)
             PWDINF = (YRO +  YRDIF) * 1000 + IDATE
            !YRPLT = PWDINF
           ENDIF
          ENDIF
      
C***********************************************************************
C***********************************************************************
C     Rate Calculations 
C***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
C-----------------------------------------------------------------------
C     Initialize soil temperature and soil water variables
C-----------------------------------------------------------------------
      TSDEP = 0.0
      CUMSW = 0.0
      AVGSW = 0.0
      YRPLT = 9999999
C-----------------------------------------------------------------------
C     Check window for automatic planting, PWDINF < YRPLT < PWDINL
C-----------------------------------------------------------------------
      CALL YR_DOY(YRDOY, YEAR, DOY)
      IF (PWDINF .LT. 1000) PWDINF = YEAR * 1000 + PWDINF
      IF (PWDINL .LT. 1000) PWDINL = YEAR * 1000 + PWDINL
C-----------------------------------------------------------------------
C     Too early.
      IF (YRDOY .LT. PWDINF) RETURN

!     Too late
      IF (YRDOY .GT. PWDINL) RETURN

C-----------------------------------------------------------------------
C     Within planting window.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     Obtain soil temperature, TSDEP, at 10 cm depth
C-----------------------------------------------------------------------
      I = 1
      XDEP = 0.0
      DO WHILE (XDEP .LT. 10.0)
        XDEP = XDEP + DLAYR(I)
        TSDEP = ST(I)
        I = I + 1
      END DO

      IF (ISWWAT .NE. 'N') THEN
C-----------------------------------------------------------------------
C     Compute average soil moisture as percent, AVGSW
C-----------------------------------------------------------------------
        I = 1
        XDEP = 0.0
        CUMSW = 0.0
        DO WHILE (XDEP .LT. SWPLTD)
          XDEPL = XDEP
          XDEP = XDEP + DLAYR(I)
          IF (DLAYR(I) .LE. 0.) THEN  
            !IF SOIL DEPTH IS LOWER THAN SWPLTD -US
            XDEP = SWPLTD
            CYCLE
          ENDIF

          DTRY = MIN(DLAYR(I),SWPLTD - XDEPL)
          CUMSW = CUMSW + DTRY *
     &          (MAX(SW(I) - LL(I),0.0)) / (DUL(I) - LL(I))
          I = I + 1
        END DO
        AVGSW = (CUMSW / SWPLTD) * 100.0
      ENDIF

C-----------------------------------------------------------------------
C     Determine YRPLT according to soil temperature and soil moisture
C-----------------------------------------------------------------------
!     Original (pre-rice) code:
!      IF (TSDEP .GE. PTTN .AND. TSDEP .LE. PTX) THEN
!        IF (AVGSW .GE. SWPLTL .AND. AVGSW .LE. SWPLTH) THEN
!          YRPLT = YRDOY
!        ENDIF
!      ENDIF

!     Modified RICE code:
      IF (TSDEP .GE. PTTN .AND. TSDEP .LE. PTX) THEN
        SOILTOK = .TRUE.
!       This should not be crop dependant -- how should we distinguish
!       between dry transplants and wet transplants? chp
!         should be dependant on PUDDLED (T or F) and PLME
!     Ultimately the days after puddling needs to come from FILEX (later). chp/gh
        IF (CROP .EQ. 'RI' .AND.(PLME .EQ. 'T' .OR. PLME. EQ. 'N')) THEN
          IF (FLOOD .GT. 25. .AND. PUDLTIME .EQ. 0) THEN
            !transplant 5 days after puddling
            PUDLTIME = 5 + YRDOY 
          ENDIF
          SOILWOK = .TRUE.
        ELSE
          IF ((AVGSW .GE. SWPLTL .AND. AVGSW .LE. SWPLTH) .OR. 
     &        (ISWWAT .EQ. 'N')) THEN
            SOILWOK = .TRUE.

!           Soil temperature and soil water are within acceptable ranges
!           Plant today
            YRPLT = YRDOY
            CALL YR_DOY(YRPLT,YR,IPLT)

          ELSE
            SOILWOK = .FALSE.
          ENDIF
        ENDIF
      ELSE
        SOILTOK = .FALSE.
      ENDIF

C-----------------------------------------------------------------------
C Forced planting mode. If conditions are not met, force planting on
C the last day of the window
C-----------------------------------------------------------------------
      IF (IPLTI .EQ. 'F') THEN
        IF (YRDOY .EQ. PWDINL) THEN
          YRPLT = YRDOY
        ENDIF
      ENDIF

C-----------------------------------------------------------------------
C     Too late.
C-----------------------------------------------------------------------
      IF (YRDOY .GE. PWDINL .AND. YRPLT .NE. YRDOY) THEN
        YRPLT = -99
        MDATE = YRDOY
        CONTROL % CropStatus = 11  !failure to plant
        CALL YR_DOY(PWDINF,PWDYR,PWDPLT)
        CALL YR_DOY(PWDINL,YR,IPLT)

        !Write to Warning.out file
        WRITE(MESSAGE(1),110)
        WRITE(MESSAGE(2),115) PWDYR,PWDPLT,YR,IPLT
  110   FORMAT('Conditions not met during defined window for planting')
  115   FORMAT('between DAY ',I4,1X,I3,' and DAY ',I4,1X,I3)
        MNUM = 2

        IF (.NOT. SOILTOK) THEN
          WRITE(MESSAGE(3),*) ' '
          WRITE(MESSAGE(4),120) TSDEP
          WRITE(MESSAGE(5),125) PTTN, PTX
  120     FORMAT('  Soil temperature in management depth: ', F5.1,' °C')
  125     FORMAT('  not within range of ',F5.1, ' and ',F5.1,' °C.')
          MNUM = 5
          
        ELSEIF (.NOT. SOILWOK) THEN
          WRITE(MESSAGE(3),*) ' '
          WRITE(MESSAGE(4),130) AVGSW
          WRITE(MESSAGE(5),135) SWPLTL, SWPLTH
  130     FORMAT('  Soil water in management depth: ', F6.2,' %ESW')
  135     FORMAT('  not within range of ',F6.2,' and ', F6.2,' %ESW')
          MNUM = 5
        ENDIF
        CALL WARNING(MNUM, ERRKEY, MESSAGE)

        !Write to screen
        WRITE (*,140) MESSAGE(1), MESSAGE(2)
 140    FORMAT(/,5X,A78,/,5X,A78,/)

        !Write to Overview.out file
        IF (IDETO .EQ. 'Y') THEN
          WRITE(NOUTDO,140) MESSAGE(1), MESSAGE(2)
        ENDIF

        RETURN
      ENDIF

!     For Rice, plant 5 days after puddling.
      IF (CROP .EQ. 'RI' .AND. YRDOY .EQ. PUDLTIME .AND. 
     &        (PLME .EQ. 'T' .OR. PLME. EQ. 'N')) THEN
        YRPLT = YRDOY
      ENDIF

C***********************************************************************
C***********************************************************************
C     END OF DYNAMIC IF CONSTRUCT
C***********************************************************************
      ENDIF
C***********************************************************************
      RETURN
      END !SUBROUTINE AUTPLT

C=======================================================================
C  IPAPLT, Subroutine, C. Porter
C-----------------------------------------------------------------------
C  Reads input variables from temporary data file for use by automatic
C      planting routine (AUTPLT)
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  11/23/1999 CHP Written for modular version of AUTPLT.
C  08/12/2003 CHP Added error checking
C========================================================================

      SUBROUTINE IPAPLT (CONTROL, 
     &    CROP, PLME, PTX, PTTN, PWDINF, PWDINL,          !Output 
     &    SWPLTL, SWPLTH, SWPLTD, YRPLT)                  !Output

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL ERROR, FIND

      CHARACTER*1 PLME
      CHARACTER*2 CROP
      CHARACTER*6 SECTION, ERRKEY 
      PARAMETER (ERRKEY = 'IPAPLT')
      CHARACTER*30 FILEIO 

      INTEGER ERRNUM, YRPLT
      INTEGER LINC, LNUM, FOUND
      INTEGER PWDINF, PWDINL
      INTEGER LUNIO

      REAL SWPLTL, SWPLTH, SWPLTD, PTX, PTTN

!     The variable "CONTROL" is of constructed type "ControlType" as 
!     defined in ModuleDefs.for, and contains the following variables.
!     The components are copied into local variables for use here.
      TYPE (ControlType) CONTROL
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO

C-----------------------------------------------------------------------
C    Open Temporary File
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)
      LNUM = 0

      !Read Automatic Management
      SECTION = '!AUTOM'
      CALL FIND(LUNIO, SECTION, LINC, FOUND); LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ(LUNIO, 1930, IOSTAT=ERRNUM) 
     &        PWDINF, PWDINL, SWPLTL, SWPLTH, SWPLTD, PTX, PTTN
 1930   FORMAT(14X,2(1X,I7),5(1X,F5.0))
        LNUM = LNUM + 1
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
      ENDIF

      !Read crop code from Cultivar Section
      SECTION = '*CULTI'
      CALL FIND(LUNIO, SECTION, LINC, FOUND); LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ (LUNIO,'(3X,A2)', IOSTAT=ERRNUM) CROP; LNUM = LNUM + 1
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
      ENDIF

      !Read Planting Details Section
      SECTION = '*PLANT'
      CALL FIND(LUNIO, SECTION, LINC, FOUND); LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ(LUNIO,'(3X,I7,25X,A1)', IOSTAT=ERRNUM) YRPLT, PLME
        LNUM = LNUM + 1
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
      ENDIF

      CLOSE (LUNIO)

      RETURN
      END SUBROUTINE IPAPLT

C=======================================================================
C Variable Definitions for AUTPLT and IPAPLT
C=======================================================================
! AVGSW     Average soil moisture as percent of depth SWPLTD (%)
! CUMSW     Cumulative soil moisture to depth of SWPLTD, calculated as 
!             fraction of moisture content between lower limit and drained 
!             upper limit multiplied by layer depth. (cm)
! DLAYR(L)  Soil thickness in layer L (cm)
! DOY       Current day of simulation (d)
! DS(L)     Cumulative depth in soil layer L (cm)
! DTRY      Effective depth of current soil layer (cm)
! DUL(L)    Volumetric soil water content at Drained Upper Limit in soil 
!             layer L (cm3[water]/cm3[soil])
! ERRKEY    Subroutine name for error file 
! ERRNUM    Error number for input 
! FILEIO    Filename for input file (e.g., IBSNAT35.INP) 
! FOUND     Indicator that good data was read from file by subroutine FIND 
!             (0 - End-of-file encountered, 1 - NAME was found) 
! IDATE     Day of irrigation or fertilizer application (d)
! IDETO     Switch for printing OVERVIEW.OUT file 
! IPLT      Day portion of planting date 
! IPLTI     Planting switch
! LNUM      Line number of input file 
! LL(L)     Volumetric soil water content in soil layer L at lower limit
!             (cm3 [water] / cm3 [soil])
! LUNIO     Logical unit number for FILEIO 
! MULTI     Current simulation year (=1 for first or single simulation, 
!             =NYRS for last seasonal simulation) 
! NL        Maximum number of soil layers = 20 
! NOUTDO    Logical unit for OVERVIEW.OUT file 
! PTTN      Minimum soil temperature limit for automatic planting (°C)
! PTX       Maximum soil temperature limit for automatic planting (°C)
! PWDINF    First day of year for planting window (YYDDD)
! PWDINL    Last day of year for planting window (YYDDD)
! SECTION   Section name in input file 
! ST(L)     Soil temperature in soil layer L (°C)
! SW(L)     Volumetric soil water content in layer L
!             (cm3 [water] / cm3 [soil])
! SWPLTD    Depth to which average soil moisture is determined for 
!             automatic planting and harvest conditions (cm)
! SWPLTH    Upper limit on soil moisture for automatic planting and harvest 
!             conditions (%)
! SWPLTL    Lower limit on soil moisture for automatic planting and harvest 
!             conditions (%)
! TSDEP     Average temperature in top 10 cm of soil. Used to modify 
!             emergence rate of development. (°C)
! XDEP      Depth to bottom of current soil layer (cm)
! XDEPL     Depth to top of current soil layer (cm)
! YEAR      Year of current date of simulation 
! YR        Year portion of date 
! YRDIF     Function subroutine which calculates number of days between two 
!             dates (da)
! YRDOY     Current day of simulation (YYDDD)
! MDATE     Date of harvest maturity (YYDDD) (YRNR8)
! YRPLT     Planting date (YYDDD)
! YRSIM     Start of simulation date (YYDDD)
!=======================================================================
