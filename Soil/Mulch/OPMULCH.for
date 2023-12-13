C=======================================================================
C  OPMULCH, Subroutine, C.H.Porter
C  Mulch water balance output.
C  File 'MulWatBal.out' created only if IDETL = 'D'.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  10/28/2003 CHP Written
!  06/10/2008 CHP Add Mulch.OUT report
C=======================================================================

      SUBROUTINE OPMULCH(CONTROL, ISWITCH, MULCH)
!     &  MULWATADD, RESWATADD)

      USE ModuleDefs
!     VSH
      USE CsvOutput
      USE Linklist
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, YR_DOY, INCDAT
      SAVE

      CHARACTER*1 IDETL, IDETW, ISWWAT
      CHARACTER*13, PARAMETER :: MWBAL = 'MulWatBal.OUT'

      INTEGER DAS, DOY, DYNAMIC, FROP, INCDAT
      INTEGER DLUN, RUN, YRDOY,  YEAR
!     INTEGER DY1, DY2, ERRNUM, LUN, YR1, YR2,

!      REAL CUMWBAL, CUMMULADD, CUMMULEVAP, CUMRESWATADD
!     REAL RESWATADD
!      REAL MULWATADD, MWI, MWY, WBALAN

      LOGICAL FEXIST, PRINTDAY    !, PRINTBAL

      TYPE (ControlType)CONTROL
      TYPE (SwitchType) ISWITCH
      TYPE (MulchType)  MULCH

      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FROP    = CONTROL % FROP
      YRDOY   = CONTROL % YRDOY

      FMOPT   = ISWITCH % FMOPT   ! VSH

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      RUN = CONTROL % RUN
      IDETL  = ISWITCH % IDETL
      IDETW  = ISWITCH % IDETW
      ISWWAT = ISWITCH % ISWWAT
!      PRINTBAL = .TRUE.
      PRINTDAY = .TRUE.
!      IF (INDEX('AD',IDETL) < 1) PRINTBAL = .FALSE.
      IF (IDETW .NE. 'Y' .OR. IDETL == '0' .OR. ISWWAT .NE. 'Y') THEN
        PRINTDAY = .FALSE.
!        PRINTBAL = .FALSE.
      ENDIF

!-----------------------------------------------------------------------
      IF (PRINTDAY) THEN
        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
        call getlun("Mulch.OUT", DLUN)
        INQUIRE (FILE = "Mulch.OUT", EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = DLUN, FILE = "Mulch.OUT", POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = DLUN, FILE = "Mulch.OUT", STATUS = 'NEW')
          WRITE(DLUN,'("*Surface Mulch daily output file")')
        ENDIF

        IF (CONTROL%RNMODE .NE. 'Q' .OR. CONTROL%RUN == 1) THEN
          CALL HEADER(SEASINIT, DLUN, CONTROL%RUN)
          WRITE(DLUN,
     &      '("@YEAR DOY   DAS    MCFD   MDEPD    MWAD    MWTD")')
        ENDIF
        END IF ! VSH

        CALL YR_DOY(INCDAT(YRDOY,-1), YEAR, DOY)

        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
        WRITE(DLUN,200) YEAR, DOY, DAS, MULCH % MULCHCOVER,
     &      MULCH % MULCHTHICK, NINT(MULCH % MULCHMASS), MULCH %MULCHWAT
  200   FORMAT(1X,I4,1X,I3.3,1X,I5,F8.3,F8.2,I8,F8.2)

        END IF ! VSH

        !     VSH
      IF (FMOPT == 'C') THEN
         CALL CsvOutMulch(EXPNAME, CONTROL%RUN, CONTROL%TRTNUM,
     &CONTROL%ROTNUM, CONTROL%REPNO, YEAR, DOY, DAS,
     &MULCH % MULCHCOVER, MULCH % MULCHTHICK,
     &MULCH % MULCHMASS, MULCH %MULCHWAT,
     &vCsvlineMulch, vpCsvlineMulch, vlngthMulch)

         CALL LinklstMulch(vCsvlineMulch)
      END IF

      ENDIF
!-----------------------------------------------------------------------
!      IF (PRINTBAL) THEN
!!       Open output file
!        CALL GETLUN('MWBAL', LUN)
!        INQUIRE (FILE = MWBAL, EXIST = FEXIST)
!        IF (FEXIST) THEN
!          OPEN (UNIT = LUN, FILE = MWBAL, STATUS = 'OLD',
!     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
!        ELSE
!          OPEN (UNIT = LUN, FILE = MWBAL, STATUS = 'NEW',
!     &      IOSTAT = ERRNUM)
!          WRITE(LUN,'("*MULCH WATER BALANCE OUTPUT FILE")')
!        ENDIF
!
!        CALL HEADER(SEASINIT, LUN, RUN)
!
!!       Write header for daily balance output
!        WRITE (LUN,100)
!  100   FORMAT('@YEAR DOY   DAS',
!     &      ' MULWAT',                   !State vars
!     &      '  MULAD  RESAD',            !Inflows
!     &      '  MEVAP',                   !Outflows
!     &      '  MULBAL   CUMWBAL')
!
!        MWI   = MULCH%MULCHWAT
!        MWY   = MULCH%MULCHWAT
!
!        CUMWBAL      = 0.0
!        CUMMULADD    = 0.0
!        CUMRESWATADD = 0.0
!        CUMMULEVAP   = 0.0
!      ENDIF
!
!***********************************************************************
!***********************************************************************
!     Daily output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
!-----------------------------------------------------------------------
      IF (PRINTDAY .AND. MOD(DAS,FROP) == 0) THEN
!       Transfer data from constructed variable to local variables
        CALL YR_DOY(YRDOY, YEAR, DOY)

        CALL YR_DOY(YRDOY, YEAR, DOY)
        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
        WRITE(DLUN,200) YEAR, DOY, DAS, MULCH % MULCHCOVER,
     &      MULCH % MULCHTHICK, NINT(MULCH % MULCHMASS), MULCH %MULCHWAT
        END IF   ! VSH

!     VSH
        IF (FMOPT == 'C') THEN
         CALL CsvOutMulch(EXPNAME, CONTROL%RUN, CONTROL%TRTNUM,
     &     CONTROL%ROTNUM, CONTROL%REPNO, YEAR, DOY, DAS,
     &     MULCH % MULCHCOVER, MULCH % MULCHTHICK,
     &     MULCH % MULCHMASS, MULCH %MULCHWAT,
     &     vCsvlineMulch, vpCsvlineMulch, vlngthMulch)

         CALL LinklstMulch(vCsvlineMulch)
        END IF
      ENDIF
!-----------------------------------------------------------------------
!      IF (PRINTBAL) THEN
!
!!       Change in storage = Inflows - Outflows
!!       Balance = Inflows - Outflows - Change in storage
!        WBALAN = MULWATADD + RESWATADD        !Inflows
!     &         - MULCH%MULCHEVAP              !Outflows
!     &         - MULCH%MULCHWAT + MWY         !Change in mulch water
!
!        CUMWBAL = CUMWBAL + WBALAN
!
!        WRITE (LUN,300) YEAR, DOY, DAS,
!     &    MULCH%MULCHWAT,                   !State variables
!     &    MULWATADD, RESWATADD,             !Inflows
!     &    MULCH%MULCHEVAP,                  !Outflows
!     &    WBALAN, CUMWBAL                   !Balance
!  300   FORMAT(1X,I4,1X,I3.3,1X,I5,4F7.2, F8.2, F10.2)
!
!!       Save values for comparison tomorrow
!        MWY = MULCH%MULCHWAT
!
!        CUMMULADD    = CUMMULADD    + MULWATADD
!        CUMMULEVAP   = CUMMULEVAP   + MULCH%MULCHEVAP
!        CUMRESWATADD = CUMRESWATADD + RESWATADD
!      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
!      IF (PRINTBAL) THEN
!        CALL YR_DOY(YRSIM, YR1, DY1)
!        CALL YR_DOY(YRDOY, YR2, DY2)
!
!!       Put "!" in column 1 so that daily info can be plotted.
!        WRITE (LUN,350)
!  350   FORMAT(/,'!',5X,'MULCH WATER BALANCE PARAMETERS',
!     &         /,'!',5X,'========================',T48,'--mm--')
!        WRITE (LUN,400) YR1, DY1, MWI,
!     &                     YR2, DY2, MULCH%MULCHWAT,
!     &                     CUMMULADD, CUMRESWATADD, CUMMULEVAP
!  400   FORMAT(
!     &   /,'!',5X,'Mulch H20 (start) on Year/day',I5,'/',I3.3,T44,F10.2,
!     &   /,'!',5X,'Mulch H20 (final) on Year/day',I5,'/',I3.3,T44,F10.2,
!     &   /,'!',5X,'Rainfall added to mulch',      T44,F10.2,
!     &   /,'!',5X,'Water added with new residue', T44,F10.2,
!     &   /,'!',5X,'Mulch evaporation',            T44,F10.2)
!
!        WBALAN = MWI - MULCH%MULCHWAT       !Change in water content
!     &         + CUMMULADD + CUMRESWATADD   !Inflows
!     &         - CUMMULEVAP                 !Outflows
!
!        WRITE  (LUN,500) WBALAN
!  500   FORMAT(/,'!',5X,'Final Balance ',T42,F12.4,/)
!      ENDIF
!
!      CLOSE(LUN)
      IF (PRINTDAY .AND. MOD(DAS,FROP) /= 0) THEN
!     Transfer data from constructed variable to local variables
      CALL YR_DOY(YRDOY, YEAR, DOY)

        CALL YR_DOY(YRDOY, YEAR, DOY)
        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
        WRITE(DLUN,200) YEAR, DOY, DAS, MULCH % MULCHCOVER,
     &      MULCH % MULCHTHICK, NINT(MULCH % MULCHMASS), MULCH %MULCHWAT
        END IF   ! VSH
      ENDIF
      IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
      CLOSE(DLUN)
      END IF   ! VSH
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OPMULCH

