C=======================================================================
C  OPPEST, Subroutine
C  Pest and Disease OUTPUT File
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  01/01/1990 GH  Original code written as part of OPDAY
C  02/23/1998 CHP Modified based on Pest damage code in PLANT subroutine
C  01/12/1999 GH  Incorporated into CROPGRO
C  06/19/2001 GH  Modified output format
C  08/20/2002 GH  Modified for Y2K
C-----------------------------------------------------------------------
C  Called by: PEST
C  Calls:     None
C=======================================================================
      SUBROUTINE OPPEST(CONTROL, ISWITCH, 
     &    ASMDOT, CASM, CLAI, CLFM, CPPLTD, CRLF, CRLV,      
     &    CRTM, CSDM, CSDN, CSHM, CSHN, CSTEM, DISLA, DISLAP,   
     &    LAIDOT, PPLTD, RLFDOT, RLVDOT, SDIDOT, SHIDOT, 
     &    SWIDOT, WLIDOT, WRIDOT, WSIDOT, WSHIDT, YRPLT)     

C-------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, INCDAT, TIMDIF, YR_DOY
      SAVE

      CHARACTER*1 IDETD, ISWDIS, RNMODE
      CHARACTER* 6, PARAMETER :: ERRKEY = 'OPPEST'
      CHARACTER*12, PARAMETER :: OUTD = 'Pest.OUT'

      INTEGER DAP, DAS, DOY,  DYNAMIC, ERRNUM, FROP, LUNIO
      INTEGER NOUTDD, RUN, TIMDIF, YEAR, YRDOY, YRPLT
      INTEGER REPNO, YRDAY, DAY, INCDAT

      REAL DISLA,DISLAP,WLIDOT,WSIDOT,WRIDOT,SWIDOT,WSHIDT,SDIDOT
      REAL SHIDOT
      REAL RLVDOT,CRLV,RLFDOT,CRLF,CRTM
      REAL ASMDOT,CASM
      REAL PPLTD,CPPLTD
      REAL CLFM,CLAI,CSTEM,LAIDOT
      REAL CSDM,CSDN,CSHM,CSHN

      LOGICAL FEXIST, FIRST

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType) ISWITCH

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      DAS     = CONTROL % DAS
      FROP    = CONTROL % FROP
      LUNIO   = CONTROL % LUNIO
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE
      REPNO   = CONTROL % REPNO
      YRDOY   = CONTROL % YRDOY
      
      ISWDIS = ISWITCH % ISWDIS
      IDETD  = ISWITCH % IDETD
      IF (IDETD .NE. 'Y' .OR. ISWDIS .NE. 'Y') RETURN

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      !ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!       Get unit number for PEST.OUT
        CALL GETLUN('OUTD', NOUTDD)
        INQUIRE (FILE = OUTD, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTDD, FILE = OUTD, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
          FIRST = .FALSE.  
        ELSE
          OPEN (UNIT = NOUTDD, FILE = OUTD, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(NOUTDD,'("*PEST AND DISEASE DAMAGE OUTPUT FILE",/)')
          FIRST = .TRUE.  
        ENDIF

C-----------------------------------------------------------------------
C       Variable heading for PEST.OUT
C-----------------------------------------------------------------------
!     CHP 2/6/2006 Keep pest output consistent with PlantGro.OUT
!        IF (RNMODE .NE. 'Q' .OR. RUN .EQ. 1 .OR. FIRST) THEN

!          !For first run of a sequenced run, use replicate
!          ! number instead of run number in header.
!          IF (RNMODE .EQ. 'Q') THEN
!            CALL HEADER(SEASINIT, NOUTDD, REPNO)
!          ELSE
            CALL HEADER(SEASINIT, NOUTDD, RUN)
!          ENDIF

          WRITE (NOUTDD,2190)
 2190     FORMAT('@YEAR DOY   DAS   DAP    DLA   DLA%    DLAI',
     &       '    DLFM    DSTM   DSDM   DSD#   DSHM   DSH#   DRTM',
     &       '    DRLV  DRLF   DASM   DPO%   CLAI    CLFM    CSTM',
     $       '    CSDM   CSD#  CSHM   CSH#   CRTM   CRLV   CRLF',
     &       '    CASM   CPO% ')
!        ENDIF

!***********************************************************************
!***********************************************************************
!     Daily output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
C-----------------------------------------------------------------------
      IF (MOD(DAS,FROP) .EQ. 0) THEN
        DAP = MAX(0,TIMDIF(YRPLT,YRDOY))
        CALL YR_DOY(YRDOY, YEAR, DOY) 

!       Convert units from g/m2 to kg/ha for output
        WRITE (NOUTDD,300)YEAR, DOY, DAS, DAP,NINT(DISLA),DISLAP,
     &      LAIDOT,NINT(WLIDOT*10.),NINT(WSIDOT*10.),NINT(SWIDOT*10.),
     &      SDIDOT,WSHIDT*10.,SHIDOT,WRIDOT*10.,
     &      RLVDOT,RLFDOT,ASMDOT,PPLTD,
     &      CLAI,NINT(CLFM*10.),NINT(CSTEM*10.),NINT(CSDM*10.),
     &      NINT(CSDN),CSHM*10.,NINT(CSHN),CRTM*10.,CRLV,
     &      CRLF,CASM,CPPLTD
  300   FORMAT(1X,I4,1X,I3.3,2(1X,I5),1X,I6,1X,F6.1,1X,F7.1,2(1X,I7),
     &      1X, I6,4(1X,F6.1),
     &      4(1X,F6.2),1X,F6.1,2(1X,I7),2(1X,I6),1X,F6.0,1X,I6,1X,F6.0,
     &      2(1X,F6.1),1X,F7.1,1X,F6.1)
      ENDIF

!***********************************************************************
!***********************************************************************
!     SEASEND
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
      !For sequence runs, write final day with zeros to allow
      !  long term daily data to graph correctly.
      IF (RNMODE .EQ. 'Q') THEN
        YRDAY = INCDAT(YRDOY,1)
        CALL YR_DOY(YRDAY, YEAR, DAY)
        WRITE (NOUTDD,400)YEAR, DAY, 0, 0, 0, 0,
     &      0, 0, 0, 0, 0, 0, 0, 0,
     &      0, 0, 0, 0,
     &      0, 0, 0, 0, 0, 0, 0, 0, 0,
     &      0, 0, 0
      ENDIF
 400   FORMAT(1X,I4,1X,I3.3,3(1X,I5),1X,I5,1X,I6,2(1X,I6),1X,I5,
     &      4(1X,I5),
     &      4(1X,I5),1X,I5,2(1X,I6),2(1X,I5),1X,I5,1X,I5,1X,I5,
     &      2(1X,I5),1X,I6,1X,I5)
      CLOSE (NOUTDD)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END !SUBROUTINE OPPEST
!-----------------------------------------------------------------------
!     OPPEST Variable Definitions
!-----------------------------------------------------------------------
! ASMDOT  Daily assimilative damage (g CH2O/m2/d)
! CASM    Cumulative assimilate damage (g CH2O/m2)
! CLAI    Cumulative leaf area index destroyed (m2/m2)
! CLFM    Cumulative leaf mass destroyed (g/m2)
! CPPLTD  Cumulative percent of plants destroyed (%)
! CRLF    Cumulative root length flux (cm root/cm2 ground)
! CRLV    Cumulative root length density (cm root/cm3 soil)
! CRTM    Cumulative root mass (g/m2)
! CSDM    Cumulative seed mass destroyed (g/m2)
! CSDN    Cumulative number of seeds destroyed (#/m2)
! CSHM    Cumulative shell mass destroyed (g/m2)
! CSHN    Cumulative number of shells destroyed (#/m2)
! CSTEM   Cumulative stem mass destroyed (g/m2)
! DISLA   Diseased leaf area (cm2/m2/d)
! DISLAP  Percent diseased leaf area (%/d)
! ENAME   Description of Experiment
! EXPER   Experiment code (prefix of input files)
! LAIDOT  Daily change in leaf area index due to pest damage (m2/m2/d)
! MODEL   Name of CROPGRO executable file
! NOUTDD  Unit number for pest and damage output file
! OUTD    Output file name for pest output file
! PPLTD   Percent plants destroyed (%/m2/d)
! RLFDOT  Daily root length flux damage (cm root/cm2 ground)
! RLVDOT  Daily root length damage (cm root/cm3 soil)
! SDIDOT  Number of seeds destroyed on the current day (#/m2/d)
! SHIDOT  Number of shells destroyed on current day (#/m2/d)
! SWIDOT  Daily seed mass damage (g/m2/day)
! WLIDOT  Daily pest or freeze damage to leaf mass (g/m2/day)
! WRIDOT  Daily pest damage to root mass (g/m2/day)
! WSHIDT  Daily shell mass damage (g/m2/day)
! WSIDOT  Daily pest damage to stem mass (g/m2/day)
! YRDOY   Current day of simulation (YYDDD)
!-----------------------------------------------------------------------
!     End Subroutine OPPEST
!-----------------------------------------------------------------------
