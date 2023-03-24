C=======================================================================
C  FOR_IPPEST, Subroutine
C  Reads input data from FILEIO for pest module.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/23/1998 CHP Written.
C  01/12/1999 GH  Incorporated into CROPGRO
C-----------------------------------------------------------------------
C  Called by: PEST
C  Calls:     None
C=======================================================================
      SUBROUTINE FOR_IPPEST(
     &    FILEIO, LUNIO,                                  !Input
     &    FILEP, FILET, PATHPE, PHTHRS8, TRTNO)           !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
        ! which contain control information, soil
        ! parameters, hourly weather data.
!     NL defined in ModuleDefs.for

      IMPLICIT NONE
      EXTERNAL FIND, ERROR
      SAVE

      CHARACTER*2 CROP
      CHARACTER*6   SECTION,ERRKEY
      CHARACTER*12 FILEP, FILET
      CHARACTER*30 FILEIO
      CHARACTER*80  PATHPE

      INTEGER LUNIO
      INTEGER ERRNUM, FOUND, LNUM, TRTNO

      PARAMETER (ERRKEY = 'PEST  ')

      REAL PHTHRS8

!----------------------------------------------------------------------- 
!     Read in values from input file, which were previously input
!       in Subroutine IPIBS.  Echo input data to file SW.OUT
!-----------------------------------------------------------------------
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)

C-----------------------------------------------------------------------
C    Read FILE names and paths from IBSNAT file
C-----------------------------------------------------------------------
      READ(LUNIO,'(/////,15X,A12,////,15X,A12,1X,A80)') 
     &    FILET, FILEP, PATHPE

C-----------------------------------------------------------------------
C       Find and Read TREATMENTS Section
C-----------------------------------------------------------------------
        SECTION = '*TREAT'
        CALL FIND(LUNIO, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILEIO, LNUM)
        ELSE
        READ(LUNIO, '(I3)') TRTNO
        ENDIF

!-----------------------------------------------------------------------
!     Find and read Cultivar Section
!-----------------------------------------------------------------------
      SECTION = '*CULTI'
      CALL FIND(LUNIO, SECTION,LNUM,FOUND)
      IF (FOUND .EQ. 0) CALL ERROR (ERRKEY,4,FILEIO,LNUM)
      READ(LUNIO,140) CROP
140   FORMAT(3X,A2)

C-----------------------------------------------------------------------
C    Find and Read Cultivar Section
C-----------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
        SECTION = '*CULTI'
        CALL FIND(LUNIO, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) CALL ERROR (ERRKEY,4,FILEIO,LNUM)
        READ(LUNIO,320) PHTHRS8
320   FORMAT(54X,F6.0)
      ENDIF

      CLOSE(LUNIO)
C-----------------------------------------------------------------------
      RETURN
      END !SUBROUTINE IPPEST
C-----------------------------------------------------------------------

!----------------------------------------------------------------------- 
!     IPPEST variables:
!----------------------------------------------------------------------- 
! ASMDOT  Daily assimilative damage (g[CH2O] /m2 / d)
! CASM    Cumulative assimilate damage (g[CH2O] / m2)
! CLAI    Cumulative leaf area index destroyed (m2/m2)
! CLFM    Cumulative leaf mass destroyed  (g/m2)
! CPPLTD  Cumulative percent of plants destroyed (%)
! CRLF    Cumulative root length flux (cm root / cm2 ground)
! CRLV    Cumulative root length density (cm root / cm3 soil)
! CROP    Crop identification code 
! CROPD   Name of crop 
! CRTM    Cumulative root mass (g/m2)
! CSDM    Cumulative seed mass destroyed (g/m2)
! CSDN    Cumulative number of seeds destroyed (#/m2)
! CSHM    Cumulative shell mass destroyed (g/m2)
! CSHN    Cumulative number of shells destroyed (#/m2)
! CSTEM   Cumulative stem mass destroyed (g/m2)
! DISLA   Diseased leaf area (cm2[leaf]/m2[ground]/d)
! DISLAP  % Diseased leaf area (%/d)
! DYNAMIC Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, INTEGR, 
!           OUTPUT, or SEASEND 
! LAIDOT  Daily change in leaf area index due to pest damage (m2/m2/d)
! MODEL   Name of CROPGRO executable file 
! NOUTDD  Unit number for PEST.OUT file 
! NREP    Report number for sequenced or multi-season runs 
! OUTD    File name for pest damage output file (e.g., PEST.OUT) 
! PPLTD   Percent plants destroyed  (%/m2/d)
! RLFDOT  Daily root length flux damage (cm root / cm2 ground)
! RLVDOT  Daily root length damage (cm root / cm3 soil)
! SDIDOT  Number of seeds destroyed on the current day (#/m2/d)
! SHIDOT  Number of shells destroyed on current day (#/m2/d)
! SWIDOT  Daily seed mass damage (g/m2/day)
! WLIDOT  Daily pest or freeze damage to leaf mass (g/m2/day)
! WRIDOT  Daily pest damage to root mass (g/m2/day)
! WSHIDT  Weight of shell tissue consumed by pests today (g[shell]/m2-d)
! WSIDOT  Daily pest damage to stem mass (g/m2/day)
! YRDOY   Current day of simulation (YYDDD)
!----------------------------------------------------------------------- 
!     End Subroutine IPPEST
!-----------------------------------------------------------------------
