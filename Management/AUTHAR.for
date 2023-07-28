C=======================================================================
C  AUTHAR, Subroutine
C-----------------------------------------------------------------------
C  Determines when automatic harvest occurs
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/01/1991 WTB Written
C  12/31/1996 GH  Delete stop condition; set last window
C  12/01/1999 CHP Modular format.
C  07/01/2000 GH  Incorporated in CROPGRO
C  03/19/2001 GH  Corrected YREND initialization 
C  04/17/2002 GH  Modified for sequence analysis
C  08/01/2002 CHP Merged RUNINIT and SEASINIT into INIT section
C  08/20/2002 GH  Modified for Y2K
C  12/16/2004 CHP Added defaults for HPC and HBPC
C  04/01/2021 FO/VSH Update harvest array size for MultiHarvest
!  04/14/2021 CHP Added CONTROL % CropStatus
C=======================================================================

      SUBROUTINE AUTHAR(CONTROL, ISWWAT,
     &    DLAYR, DUL, IDETO, IHARI, LL, STGDOY,           !Input
     &    SW, MDATE, YRPLT,                               !Input
     &    YREND, HARVFRAC, HDATE, NHAR)                   !Output
      
!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      USE ModuleData
      
      IMPLICIT NONE
      EXTERNAL YR_DOY, ERROR, FIND, TIMDIF, GETLUN, WARNING, IPAHAR
      SAVE

      CHARACTER*1 IHARI, IDETO, ISWWAT, RNMODE
      CHARACTER*6, PARAMETER :: ERRKEY = 'AUTHAR'
      CHARACTER*78 MESSAGE(10)    !Up to 10 lines of text to be output

      INTEGER YREND, HDLAY, HEARLY, HLATE, I
      INTEGER NHAR, YR, IDATE, DEARLY, YREARLY
      INTEGER MULTI, TIMDIF, YRPLT, YRDIF, YRSIM
      INTEGER YRDOY, MDATE, DAP, NOUTDO
      INTEGER DYNAMIC, RUN
      INTEGER HDATE(NAPPL), HSTG(NAPPL) 
      INTEGER STGDOY(20), HARVF, NPHAR

      REAL AVGSW, CUMSW, DTRY, SWPLTD
      REAL SWPLTH, SWPLTL, XDEP, XDEPL
      REAL HPC(NAPPL), HBPC(NAPPL)
      REAL HARVFRAC(2)
      REAL DLAYR(NL), DUL(NL), LL(NL), SW(NL)

!     The variable "CONTROL" is of constructed type "ControlType" as 
!     defined in ModuleDefs.for, and contains the following variables.
!     The components are copied into local variables for use here.

      TYPE (ControlType) CONTROL

      DYNAMIC = CONTROL % DYNAMIC
      MULTI   = CONTROL % MULTI
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM
      YRDIF   = CONTROL % YRDIF
      RNMODE  = CONTROL % RNMODE
      RUN     = CONTROL % RUN

C***********************************************************************
C***********************************************************************
C    Input and Initialization 
C***********************************************************************
      IF (DYNAMIC .EQ. INIT) THEN
C-----------------------------------------------------------------------
      CALL IPAHAR(CONTROL,
     &    HPC, HBPC, HDATE, HDLAY, HLATE, HSTG,           !Output
     &    NHAR, SWPLTL, SWPLTH, SWPLTD)                   !Output

      CALL GETLUN('OUTO', NOUTDO)

      MDATE = -99

C-----------------------------------------------------------------------
C     Adjust for multi year runs
C-----------------------------------------------------------------------
      IF (MULTI .GT. 1) THEN
        IF (NHAR .GT. 0 .AND. IHARI .NE. 'D') THEN
          DO I = 1, NHAR
            CALL YR_DOY(HDATE(I), YR, IDATE)
            HDATE(I) = (YR + MULTI - 1) * 1000 + IDATE
          ENDDO
        ENDIF
        IF (IHARI .EQ. 'A') THEN
          CALL YR_DOY(HLATE, YR, IDATE)
          HLATE = (YR +  MULTI - 1) * 1000 + IDATE
        ENDIF
      ENDIF

C-----------------------------------------------------------------------
C     Adjust for crop rotations
C-----------------------------------------------------------------------
!      IF (RNMODE .EQ. 'Q') THEN
      IF (INDEX('FQ',RNMODE) > 0) THEN
        IF (NHAR .GT. 0 .AND. HDATE(1) .LT. YRSIM .AND. 
     &          IHARI .NE. 'D') THEN
          DO I = 1, NHAR
            CALL YR_DOY(HDATE(I), YR, IDATE)
            HDATE(I) = (YR + YRDIF) * 1000 + IDATE
          END DO
        ENDIF  
 
        IF (IHARI .EQ. 'A' .AND. HLATE .LT. YRSIM) THEN
          CALL YR_DOY(HLATE, YR, IDATE)
          HLATE = (YR +  YRDIF) * 1000 + IDATE
        ENDIF

      ENDIF

! 08/12/2003 chp moved to potato module
!      IF (INDEX('PT',CROP) > 0) THEN
!        MDATE = HDATE(1)
!      ELSE
!        MDATE = -99
!      ENDIF

      HARVFRAC(1) = HPC(1)  / 100.
      HARVFRAC(2) = HBPC(1) / 100.

!     08/15/2022 FO Multi-Harvest index to the reported harvests
      NPHAR = 1
C***********************************************************************
C***********************************************************************
C     Daily integration
C***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN

!     YREND = -99
      IF (YRDOY == YREND) RETURN

      !TF 05/04/2023 - stop simulation if crop failed to germinate
      IF(CONTROL % CropStatus .EQ. 12) THEN
        YREND = YRDOY
        RETURN
      ENDIF 

      !TF 05/04/2023 - stop simulation if crop failed to germinate
      IF(CONTROL % CropStatus .EQ. 13) THEN
        YREND = YRDOY
        RETURN
      ENDIF 
C-----------------------------------------------------------------------
C Harvest at maturity, NR8
C-----------------------------------------------------------------------
      IF (IHARI .EQ. 'M') THEN
        YREND     = MDATE
        CONTROL % CropStatus = 1    !harvest at maturity

C-----------------------------------------------------------------------
C Harvest on specified day of year, HDATE
C-----------------------------------------------------------------------
      ELSE IF (IHARI .EQ. 'R' .OR. IHARI .EQ. 'W' .OR.
     &   IHARI .EQ. 'X' .OR. IHARI .EQ. 'Y' .OR. IHARI .EQ. 'Z') THEN
        IF (YRDOY .GE. HDATE(NHAR)) THEN
          YREND = HDATE(NHAR)
          CONTROL % CropStatus = 2 !harvest on reported date
        ENDIF
        
        HARVF = 0
        IF (YRDOY .GE. HDATE(NPHAR)) THEN
         HARVF = 1
         IF (NPHAR < NHAR) THEN
            NPHAR = NPHAR + 1
         ENDIF
        ENDIF
        CALL PUT('MHARVEST','HARVF',HARVF)
C-----------------------------------------------------------------------
C Harvest on specified day after planting, HDATE
C-----------------------------------------------------------------------
      ELSE IF (IHARI .EQ. 'D') THEN
        DAP = MAX(0,TIMDIF(YRPLT,YRDOY))
        IF (DAP .GE. HDATE(1)) THEN
C-GH    IF (DAP .GE. HDATE(1) .OR. MDATE .EQ. YRDOY) THEN
           YREND     = YRDOY
           CONTROL % CropStatus = 2 !harvest on reported date
        ENDIF

C-----------------------------------------------------------------------
C Harvest at specified growth stage, HSTG
C-----------------------------------------------------------------------
      ELSE IF (IHARI .EQ. 'G') THEN
        DO I = 1, 13
          IF (HSTG(1) .EQ. I .AND. YRDOY .EQ. STGDOY(I)) THEN
            YREND     = YRDOY
            CONTROL % CropStatus = 3 !harvest at reported growth stage
            RETURN
          ENDIF
        END DO

C-----------------------------------------------------------------------
C Harvest within specified window if conditions are met
C-----------------------------------------------------------------------
      ELSE IF (IHARI .EQ. 'A') THEN

!       Havest maturity not reached yet.
        IF (YRDOY .LT. MDATE .OR. MDATE .EQ. -99) THEN
           RETURN

!       Harvest maturity reached today.  Set earliest harvest day.
        ELSE IF (YRDOY .EQ. MDATE) THEN
           HEARLY = YRDOY + HDLAY

!       Past harvest maturity.
        ELSE IF (YRDOY .GT. MDATE) THEN

C         Check window for automatic harvest, HEARLY < YRDOY < HLATE
          IF (YRDOY .LT. HEARLY) RETURN       !too early

!         Too late for harvest. Print error message to screen and to 
!             Overview.OUT and Warning.OUT files.
          IF (YRDOY .GT. HLATE) THEN
            CALL YR_DOY(HEARLY, YREARLY, DEARLY)
            CALL YR_DOY(HLATE, YR, IDATE)

            !Warning.out
            WRITE(MESSAGE(1), 110) 
            WRITE(MESSAGE(2), 115) YREARLY,DEARLY,YR,IDATE
            CALL WARNING(2, ERRKEY, MESSAGE)

            !Screen 
            WRITE (*,120) MESSAGE(1), MESSAGE(2)

            !Overview.out
            IF (IDETO .EQ. 'Y') THEN
              WRITE(NOUTDO,120) MESSAGE(1), MESSAGE(2)
            ENDIF

  110 FORMAT('Conditions not met during defined window for harvesting')
  115 FORMAT('between DAY ',I4,1X,I3,' and DAY ',I4,1X,I3)
  120 FORMAT(/,5X,A78,/,5X,A78,/)

C           Assume harvest to occur on the first day after the defined 
C           window to terminate the simulation. This needs to be changed 
C           to account for harvest loss of the crop;
            YREND     = YRDOY
!           Failure to plant (automatic planting)
            CONTROL % CropStatus = 11  
!           STGDOY(16) = YRDOY
            RETURN
          ENDIF

!         Today is within harvest window.

          IF (ISWWAT .EQ. 'Y') THEN
C           Compute average soil moisture as percent, AVGSW***
            I = 1
            XDEP = 0.0
            CUMSW = 0.0
  20        IF (XDEP .LT. SWPLTD) THEN
              XDEPL = XDEP
              XDEP = XDEP + DLAYR(I)
              DTRY = MIN(DLAYR(I),SWPLTD - XDEPL)
              CUMSW = CUMSW + DTRY *
     &                (MAX(SW(I) - LL(I),0.0)) / (DUL(I) - LL(I))
              I = I + 1
              GO TO 20
            ENDIF
            AVGSW = (CUMSW / SWPLTD) * 100.0
            IF (AVGSW .GE. SWPLTL .AND. AVGSW .LE. SWPLTH) THEN
               YREND = YRDOY
               CONTROL % CropStatus = 6 !auto-harvest within window
!              STGDOY(16) = YRDOY
            ENDIF
          ELSE
            !Soil water not being simulated - conditions assumed OK
            YREND = YRDOY
            CONTROL % CropStatus = 6 !auto-harvest within window
          ENDIF
        ENDIF

C-----------------------------------------------------------------------
C Error message if an incorrect code has been specified
C-----------------------------------------------------------------------
      ELSE
!       Invalid harvest code
        CALL ERROR(ERRKEY,10,' ',0)
      ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************

      RETURN
      END SUBROUTINE AUTHAR

C=======================================================================
C  IPAHAR, Subroutine, C. Porter
C-----------------------------------------------------------------------
C  Reads input variables from temporary data file for use by automatic
!      harvest routine (AUTHAR)
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  11/23/1999 CHP Written for modular version of AUTHAR.
C  08/20/2002 GH  Modified for Y2K
C  08/12/2003 CHP Added error checking
C  04/01/2021 FO/VSH Updated harvest section for MultiHarvest
C========================================================================

      SUBROUTINE IPAHAR(CONTROL,
     &    HPC, HBPC, HDATE, HDLAY, HLATE, HSTG,           !Output
     &    NHAR, SWPLTL, SWPLTH, SWPLTD)                   !Output

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      
      IMPLICIT NONE
      EXTERNAL ERROR, FIND

      CHARACTER*5 HCOM(NAPPL), HSIZ(NAPPL)
      CHARACTER*6 SECTION, ERRKEY 
      PARAMETER (ERRKEY = 'IPAHAR')
      CHARACTER*30 FILEIO 
      CHARACTER*90 CHAR

      INTEGER ERRNUM
      INTEGER HDLAY, HLATE, NHAR, HDATE(NAPPL), HSTG(NAPPL)
      INTEGER LINC, LNUM, FOUND
      INTEGER LUNIO

      REAL HPP, HRP, SWPLTL, SWPLTH, SWPLTD
      REAL HPC(NAPPL), HBPC(NAPPL)

!     The variable "CONTROL" is of constructed type "ControlType" as 
!     defined in ModuleDefs.for, and contains the following variables.
!     The components are copied into local variables for use here.
      TYPE (ControlType) CONTROL
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO

C-----------------------------------------------------------------------
C    Open Temporary File
C-----------------------------------------------------------------------
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)
      LNUM = 0
C-----------------------------------------------------------------------
C    Read Automatic Management
C-----------------------------------------------------------------------
      SECTION = '!AUTOM'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ(LUNIO,'(30X,3(1X,F5.0))',IOSTAT=ERRNUM) 
     &    SWPLTL, SWPLTH, SWPLTD
        LNUM = LNUM + 1
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
        READ (LUNIO,'(///,14X,2(1X,I7),5(1X,F5.0))', IOSTAT=ERRNUM)
     &    HDLAY, HLATE, HPP, HRP
        LNUM = LNUM + 4
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
      ENDIF

C-----------------------------------------------------------------------
C    Read Harvest Section
C-----------------------------------------------------------------------
      SECTION = '*HARVE'
      CALL FIND(LUNIO, SECTION, LINC, FOUND); LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        NHAR = 0
        DO
          READ(LUNIO,'(A90)',ERR=4102,END=4102) CHAR 
          LNUM = LNUM + 1
          IF(INDEX(CHAR, '*') .GT. 0) EXIT
          NHAR = NHAR + 1
          
          READ(CHAR,4100,IOSTAT=ERRNUM) HDATE(NHAR), HSTG(NHAR),
     &        HCOM(NHAR), HSIZ(NHAR), HPC(NHAR), HBPC(NHAR) 
 4100     FORMAT(3X,I7,4X,I2,2(1X,A5),2(1X,F5.0))
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)

        ENDDO              
 4102   CONTINUE
      ENDIF  !Section HARVEST

      CLOSE (LUNIO)

      !Harvest Initialization Section
      IF (NHAR .EQ. 0) THEN
        HPC(1)  = HPP
        HBPC(1) = HRP
      ENDIF

!     Default to 100% harvest of product, 0% harvest of by-product
      IF (HPC(1)  < 0.) HPC(1)  = 100.  !Percent product harvested
      IF (HBPC(1) < 0.) HBPC(1) = 0.    !Percent by-product harvested

      RETURN
      END !SUBROUTINE IPAHAR

!=======================================================================
! AUTHAR and IPAHAR Variable Definitions
!=======================================================================
! AVGSW     Average soil moisture as percent of depth SWPLTD; Also, in 
!             WDCONT, average volumetric soil water in top 30 cm.
!             (%; cm3/cm3 for WDCONT))
! CUMSW     Cumulative soil moisture to depth of SWPLTD, calculated as 
!             fraction of moisture content between lower limit and drained 
!             upper limit multiplied by layer depth. (cm)
! DAP       Number of days after planting (d)
! DLAYR(L)  Soil thickness in layer L (cm)
! DS(L)     Cumulative depth in soil layer L (cm)
! DTRY      Effective depth of current soil layer (cm)
! DUL(L)    Volumetric soil water content at Drained Upper Limit in soil 
!             layer L (cm3[water]/cm3[soil])
! ERRKEY    Subroutine name for error file 
! ERRNUM    Error number for input 
! FILEIO    Filename for input file (e.g., IBSNAT35.INP) 
! FOUND     Indicator that good data was read from file by subroutine FIND 
!             (0 - End-of-file encountered, 1 - NAME was found) 
! YREND    Computed harvest date (YYDDD)
! HDATE(I)  Harvest dates (normally only the first is used) (YYDDD)
! HDLAY     Earliest day after harvest maturity (R8) for harvest window
!             (da)
! HEARLY    First day of harvest window (YYDDD)
! HLATE     Latest day of the year to harvest; last day of window (YYDDD)
! HSTG      Growth stage which triggers automatic harvesting for IHARI='G' 
! IDATE     Day of irrigation or fertilizer application (d)
! LNUM      Line number of input file 
! LL(L)     Volumetric soil water content in soil layer L at lower limit
!             (cm3 [water] / cm3 [soil])
! LUNIO     Logical unit number for FILEIO 
! MULTI     Current simulation year (=1 for first or single simulation, 
!             =NYRS for last seasonal simulation) 
! NHAR      Number harvest dates read 
! NL        Maximum number of soil layers  
! NOUTDO    Logical unit for OVERVIEW.OUT file 
! SECTION   Section name in input file 
! STGDOY(I) Day when stage I occurred (YYDDD)
! SW(L)     Volumetric soil water content in layer L
!             (cm3 [water] / cm3 [soil])
! SWPLTD    Depth to which average soil moisture is determined for 
!             automatic planting and harvest conditions (cm)
! SWPLTH    Upper limit on soil moisture for automatic planting and harvest 
!             conditions (%)
! SWPLTL    Lower limit on soil moisture for automatic planting and harvest 
!             conditions (%)
! TIMDIF    Integer function which calculates the number of days between 
!             two Julian dates (da)
! XDEP      Depth to bottom of current soil layer (cm)
! XDEPL     Depth to top of current soil layer (cm)
! YR        Year portion of date 
! YR_DOY    Function subroutine converts date in YYDDD format to integer 
!             year (YY) and day (DDD). 
! YRDIF     Function subroutine which calculates number of days between two 
!             dates (da)
! YRDOY     Current day of simulation (YYDDD)
! YRNR8     Date of harvest maturity (YYDDD)
! MDATE     Date of harvest maturity (YYDDD)
! YRPLT     Planting date (YYDDD)
!=======================================================================
