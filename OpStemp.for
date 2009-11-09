C=======================================================================
C  OPSTEMP, Subroutine, C.H.Porter 
C  Generates output for daily soil temperature data
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  11/01/2001 CHP Written
C  06/07/2002 GH  Modified for crop rotations
C-----------------------------------------------------------------------
C  Called from:   STEMP
C  Calls:         None
C=======================================================================
      SUBROUTINE OPSTEMP(CONTROL, ISWITCH, DOY, SRFTEMP, ST)

!-----------------------------------------------------------------------
      USE ModuleDefs 
      USE ModuleData
      IMPLICIT NONE
      SAVE
!-----------------------------------------------------------------------
      CHARACTER*1  RNMODE
      CHARACTER*12 OUTT

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, L, N_LYR
      INTEGER NOUTDT, RUN, YEAR, YRDOY, REPNO
      REAL ST(NL), SRFTEMP

      LOGICAL FEXIST, DOPRINT

!-----------------------------------------------------------------------
!     The variable "CONTROL" is of constructed type "ControlType" as 
!     defined in ModuleDefs.for, and contains the following variables.
!     The components are copied into local variables for use here.
!-----------------------------------------------------------------------
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (SoilType)    SOILPROP

      IF (INDEX('N0',ISWITCH % IDETL) > 0) RETURN

      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FROP    = CONTROL % FROP
      YRDOY   = CONTROL % YRDOY

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
!      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      RNMODE  = CONTROL % RNMODE
      REPNO   = CONTROL % REPNO
      RUN     = CONTROL % RUN

      CALL GETLUN('OUTT',NOUTDT)
!     Open the output files
      OUTT = 'SoilTemp.OUT'
      INQUIRE (FILE = OUTT, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT=NOUTDT, FILE=OUTT, STATUS='OLD',
     &    IOSTAT = ERRNUM, POSITION='APPEND')
        !IF (RNMODE .NE. 'Q') THEN
        !ENDIF
      ELSE
        OPEN (UNIT=NOUTDT, FILE=OUTT, STATUS='NEW',
     &    IOSTAT = ERRNUM)
 !      Write headers info to daily output file
        WRITE(NOUTDT,'("*SOIL TEMPERATURE OUTPUT FILE (DAILY)")')
      ENDIF

C-----------------------------------------------------------------------
C     Variable heading for SoilTemp.OUT
C-----------------------------------------------------------------------
      IF (RNMODE .NE. 'Q' .OR. RUN .EQ. 1) THEN

        !For first run of a sequenced run, use replicate
        ! number instead of run number in header.
        IF (RNMODE .EQ. 'Q') THEN
          CALL HEADER(SEASINIT, NOUTDT, REPNO)
        ELSE
          CALL HEADER(SEASINIT, NOUTDT, RUN)
        ENDIF

        CALL GET(SOILPROP)
        N_LYR = MIN(10, MAX(4,SOILPROP%NLAYR))

        WRITE (NOUTDT, 
     &    '("!",T17,"Temperature (oC) by soil depth (cm):",
     &    /,"!",T17,"Surface",10A8)') (SoilProp%LayerText(L), L=1,N_LYR)
        IF (N_LYR < 10) THEN
          WRITE (NOUTDT,120) ("TS",L,"D",L=1,N_LYR)
  120     FORMAT('@YEAR DOY   DAS    TS0D',10("    ",A2,I1,A1))
!     &  '    TS1D    TS2D    TS3D    TS4D    TS5D',
!     &  '    TS6D    TS7D    TS8D    TS9D    TS10')
        ELSE
          WRITE (NOUTDT,122) ("TS",L,"D",L=1,9), "    TS10"
  122     FORMAT('@YEAR DOY   DAS    TS0D',9("    ",A2,I1,A1),A8)
        ENDIF
      ENDIF

      ENDIF !DYNAMIC

!***********************************************************************
!***********************************************************************
!     Daily Output
!***********************************************************************
      DOPRINT = .FALSE.
      SELECT CASE (DYNAMIC)
!      CASE (SEASINIT)
!        DOPRINT = .TRUE.
      CASE (OUTPUT)
        IF (MOD(DAS, FROP) == 0) THEN
          DOPRINT = .TRUE.
        ENDIF
      CASE (SEASEND)
        IF (MOD(DAS, FROP) /= 0) THEN
          DOPRINT = .TRUE.
        ENDIF
      END SELECT
      IF (DAS == 1) DOPRINT = .TRUE.

      IF (DOPRINT) THEN
        CALL YR_DOY(YRDOY, YEAR, DOY)
C       Generate output for file SoilTemp.OUT
        WRITE (NOUTDT,300) YEAR, DOY, DAS, SRFTEMP, (ST(L),L=1,N_LYR)
  300   FORMAT(1X,I4,1X,I3.3,1X,I5,11F8.1)
      ENDIF

!***********************************************************************
!***********************************************************************
!     SEASEND
!***********************************************************************
      IF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
        CLOSE (NOUTDT)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OPSTEMP
!***********************************************************************
