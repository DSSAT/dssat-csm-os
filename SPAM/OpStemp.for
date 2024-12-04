C=======================================================================
C  OPSTEMP, Subroutine, C.H.Porter
C  Generates output for daily soil temperature data
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  11/01/2001 CHP Written
C  06/07/2002 GH  Modified for crop rotations
C  11/24/2024 FO  Updated Verbose mode for SoilTemp.OUT
C-----------------------------------------------------------------------
C  Called from:   STEMP
C  Calls:         None
C=======================================================================
      SUBROUTINE OPSTEMP(CONTROL, ISWITCH, DOY, SRFTEMP, ST, TAV, TAMP)

!-----------------------------------------------------------------------
      USE ModuleDefs
      USE ModuleData
!     VSH
      USE CsvOutput
      USE Linklist
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, YR_DOY
      SAVE
!-----------------------------------------------------------------------
      CHARACTER*1  RNMODE, IDETL, IDETW, ISWWAT
      CHARACTER*12 OUTT

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, L, N_LYR
      INTEGER NOUTDT, RUN, YEAR, YRDOY, REPNO
      REAL ST(NL), SRFTEMP, TAV, TAMP

      LOGICAL FEXIST, DOPRINT

!-----------------------------------------------------------------------
!     The variable "CONTROL" is of constructed type "ControlType" as
!     defined in ModuleDefs.for, and contains the following variables.
!     The components are copied into local variables for use here.
!-----------------------------------------------------------------------
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (SoilType)    SOILPROP

      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FROP    = CONTROL % FROP
      YRDOY   = CONTROL % YRDOY
      
      FMOPT   = ISWITCH % FMOPT
      IDETL   = ISWITCH % IDETL
      IDETW   = ISWITCH % IDETW
      ISWWAT  = ISWITCH % ISWWAT

      IF(IDETW == 'N' .OR. IDETL == '0' .OR. ISWWAT == 'N') RETURN
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

      IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
        CALL GETLUN('OUTT',NOUTDT)
!       Open the output files
        OUTT = 'SoilTemp.OUT'
        INQUIRE (FILE = OUTT, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT=NOUTDT, FILE=OUTT, STATUS='OLD',
     &      IOSTAT = ERRNUM, POSITION='APPEND')
          !IF (RNMODE .NE. 'Q') THEN
          !ENDIF
        ELSE
          OPEN (UNIT=NOUTDT, FILE=OUTT, STATUS='NEW',
     &      IOSTAT = ERRNUM)
 !        Write headers info to daily output file
          WRITE(NOUTDT,'("*SOIL TEMPERATURE OUTPUT FILE (DAILY)")')
        ENDIF
      END IF   ! VSH
C-----------------------------------------------------------------------
C     Variable heading for SoilTemp.OUT
C-----------------------------------------------------------------------
      IF (RNMODE .NE. 'Q' .OR. RUN .EQ. 1) THEN

        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
          !For first run of a sequenced run, use replicate
          ! number instead of run number in header.
          IF (RNMODE .EQ. 'Q') THEN
            CALL HEADER(SEASINIT, NOUTDT, REPNO)
          ELSE
            CALL HEADER(SEASINIT, NOUTDT, RUN)
          ENDIF
        END IF   ! VSH

        CALL GET(SOILPROP)
        N_LYR = MIN(10, MAX(4,SOILPROP%NLAYR))

        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
          WRITE (NOUTDT, '("! TAV  =",F8.1,/,"! TAMP =",F8.1)') TAV,TAMP
          WRITE (NOUTDT,
     &      '("!",T17,"Temperature (oC) by soil depth (cm):",
     &      /,"!",T17,"Surface",10A8)')(SoilProp%LayerText(L),L=1,N_LYR)
          IF (N_LYR < 10) THEN
            WRITE (NOUTDT,120) ("TS",L,"D",L=1,N_LYR)
  120       FORMAT('@YEAR DOY   DAS    TS0D',10("    ",A2,I1,A1))
!     &    '    TS1D    TS2D    TS3D    TS4D    TS5D',
!     &    '    TS6D    TS7D    TS8D    TS9D    TS10')
          ELSE
            WRITE (NOUTDT,122) ("TS",L,"D",L=1,9), "    TS10"
  122       FORMAT('@YEAR DOY   DAS    TS0D',9("    ",A2,I1,A1),A8)
          ENDIF
        END IF   ! VSH
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
        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
!         Generate output for file SoilTemp.OUT
          WRITE (NOUTDT,300) YEAR, DOY, DAS, SRFTEMP, (ST(L),L=1,N_LYR)
  300     FORMAT(1X,I4,1X,I3.3,1X,I5,11F8.1)
        END IF   ! VSH

!       VSH CSV output corresponding to SoilTEMP.OUT
        IF (FMOPT == 'C') THEN ! VSH
          CALL CsvOutTemp_crgro(EXPNAME,CONTROL%RUN, CONTROL%TRTNUM,
     &CONTROL%ROTNUM,CONTROL%REPNO, YEAR, DOY, DAS, SRFTEMP,
     &N_LYR, ST, vCsvlineTemp, vpCsvlineTemp, vlngthTemp)

          CALL LinklstTemp(vCsvlineTemp)
        ENDIF

      ENDIF

!***********************************************************************
!***********************************************************************
!     SEASEND
!***********************************************************************
!      IF (DYNAMIC .EQ. SEASEND) THEN
      IF ((DYNAMIC == SEASEND)
     & .AND. (FMOPT == 'A'.OR.FMOPT == ' ')) THEN ! VSH
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
