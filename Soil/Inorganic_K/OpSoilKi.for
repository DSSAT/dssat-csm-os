!=======================================================================
!  OpSoilKi, Subroutine, U. Singh and C.H.Porter
!-----------------------------------------------------------------------
!  Potassium
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  10/31/2007 CHP Written 
!-----------------------------------------------------------------------
!  Called: SoilKi
!  Calls: GETLUN, HEADER, YR_DOY, SUMVALS
!=====================================================================

      SUBROUTINE OpSoilKi(CONTROL, ISWITCH, 
     &    FertData, KUptake,    
     &    SOILPROP, Ki_AVAIL, SKiAvlProf, SkiTotProf) 

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, INCDAT, YR_DOY, SUMVALS
      SAVE

      CHARACTER*1  ISWPOT, RNMODE
      CHARACTER*8  LayerText(11)
      CHARACTER*10, PARAMETER :: OUTSK = 'SoilKi.OUT'

      INTEGER DAS, DOY, DOYi, DYNAMIC, ERRNUM, FROP, INCDAT
      INTEGER L, LUN, LUNK, NLAYR, REPNO, RUN, YEAR, YEARi, YRDOY  

      REAL SKiAvlProf, SkiTotProf, CumUptakeK, CumFertK
      REAL StartK, BALANCE
      REAL, DIMENSION(NL) :: KUptake
      REAL, DIMENSION(NL) :: Ki_AVAIL

!     Daily balance
      REAL KFertToday, KUptToday, KUpt_yest
      REAL K_yest, CumFert_yest, CumUpt_yest
      REAL DayBal, CumBal

      LOGICAL FEXIST, DOPRINT

!     Arrays which contain data for printing in SUMMARY.OUT file
      INTEGER, PARAMETER :: SUMNUM = 4
      CHARACTER*4, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType)  CONTROL
      TYPE (FertType)     FERTDATA
      TYPE (SwitchType)   ISWITCH
      TYPE (SoilType)     SOILPROP

!     Transfer values from constructed data types into local variables.
      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FROP    = CONTROL % FROP
      REPNO   = CONTROL % REPNO
      RNMODE  = CONTROL % RNMODE
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY

      CumFertK = FertData % AMTFER(Kel)

      NLAYR = SOILPROP % NLAYR

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      ISWPOT= ISWITCH % ISWPOT
      IF (ISWPOT .NE. 'Y' .OR. ISWITCH % IDETL == '0') THEN
        DOPRINT = .FALSE.
      ELSE
        DOPRINT = .TRUE.
      ENDIF

      CumUptakek = 0.0
      
      IF (.NOT. DOPRINT) RETURN
      CALL GETLUN(OUTSK, LUN)

      INQUIRE (FILE = OUTSK, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = LUN, FILE = OUTSK, STATUS = 'OLD',
     &    IOSTAT = ERRNUM, POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = LUN, FILE = OUTSK, STATUS = 'NEW',
     &    IOSTAT = ERRNUM)
        WRITE(LUN,'("*Soil Inorganic Potassium daily output file")')
      ENDIF

      IF (RUN .EQ. 1 .OR. INDEX('QF',RNMODE) .LE. 0) THEN
        !For sequenced run, use replicate
        ! number instead of run number in header.
        IF (RNMODE .EQ. 'Q') THEN
          CALL HEADER(SEASINIT, LUN, REPNO)
        ELSE
          CALL HEADER(SEASINIT, LUN, RUN)
        ENDIF

!       Text describing soil layer depths
        LayerText = SoilProp % LayerText
!       LayerText(11) is for layers 5 thru NLAYR
        IF (NLAYR > 4) THEN
          LayerText(5) = SOILPROP % LayerText(11)
        ENDIF

        WRITE (LUN, '("!",T58,"K avail by soil depth (cm):",
     &    T103,"K uptake by soil depth (cm):")')
        WRITE (LUN, '("!",T43,10(1X,A8))') 
     &    (LayerText(L),L=1,5),(LayerText(L),L=1,5)
        WRITE (LUN,120)
  120   FORMAT('@YEAR DOY   DAS',
     &  '    KTOTD    KAVLD     KAPC     KUPC',
     &  '    KAV1D    KAV2D    KAV3D    KAV4D    KAV5D',
     &  '    KUP1D    KUP2D    KUP3D    KUP4D    KUP5D')
        CALL YR_DOY(INCDAT(YRDOY,-1), YEARi, DOYi)
        WRITE (LUN,300) YEARi, DOYi, DAS, 
     &    NINT(SKiTotProf), NINT(SKiAvlProf), 0, 0,
     &    Ki_AVAIL(1:5), KUptake(1:5)
      ENDIF

!     --------------------------------------------------------
!     Seasonal balance
      CALL GETLUN ('SKBAL', LUNK)
      INQUIRE (FILE = 'SoilKBal.OUT', EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = LUNK, FILE = 'SoilKBal.OUT', STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = LUNK, FILE = 'SoilKBal.OUT', STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
        WRITE (LUNK,'("*Soil Potassium Balance")')
      ENDIF

      CALL HEADER(SEASINIT, LUNK, RUN)
      StartK = SKiTotProf

      IF (INDEX('DA',ISWITCH%IDETL) > 0) THEN
        WRITE(LUNK,'("@YEAR DOY   DAS",
     &     "     KTOTD      KUPD      KAPD      DAYBAL      CUMBAL")')
        CALL YR_DOY(INCDAT(YRDOY,-1), YEARi, DOYi)
        WRITE (LUNK,350) YEARi, DOYi, DAS, SKiTotProf
        K_yest = StartK
        CumFert_yest = 0.0
        CumUpt_yest  = 0.0
        CumBal = 0.0
        KUpt_yest = 0.0
      ENDIF

!***********************************************************************
!***********************************************************************
!     OUTPUT
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
!-----------------------------------------------------------------------
      DO L = 1, NLAYR
        CumUptakeK = CumUptakeK + KUptake(L)
      ENDDO

!     Daily printout
      IF (DOPRINT .AND. MOD(DAS, FROP) == 0) THEN
        CALL YR_DOY(YRDOY, YEAR, DOY)
        WRITE (LUN,300) YEAR, DOY, DAS, 
     &    NINT(SKiTotProf), NINT(SKiAvlProf), 
     &    NINT(CumFertK), NINT(CumUptakeK),
     &    Ki_AVAIL(1:5), KUptake(1:5)
  300   FORMAT(1X,I4,1X,I3.3,1X,I5,4I9,5F9.0,5F9.2)    
      ENDIF

      IF (INDEX('AD',ISWITCH%IDETL) > 0) THEN
        KFertToday = CumFertK - CumFert_yest
        KUptToday  = CumUptakeK - CumUpt_yest
        DayBal = SkiTotProf - K_yest - KFertToday + KUpt_yest
        CumBal = CumBal + DayBal
        CALL YR_DOY(YRDOY, YEAR, DOY)
        WRITE(LUNK,350) YEAR, DOY, DAS, 
     &    SKiTotProf, KFertToday, KUptToday, DayBal, CumBal 
  350   FORMAT(1X,I4,1X,I3.3,1X,I5,3F10.2,2F12.3)
        CumFert_yest = CumFertK
        CumUpt_yest  = CumUptakeK
        K_yest = SkiTotProf
        KUpt_yest = KUptToday
      ENDIF

!***********************************************************************
!***********************************************************************
!     SEASEND
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
!     Also print on last day if not already done.
      IF (DOPRINT .AND. MOD(DAS, FROP) .NE. 0) THEN
        CALL YR_DOY(YRDOY, YEAR, DOY) 
        WRITE (LUN,300) YEAR, DOY, DAS, 
     &    NINT(SKiTotProf), NINT(SKiAvlProf), 
     &    NINT(CumFertK), NINT(CumUptakeK),
     &    Ki_AVAIL(1:5), KUptake(1:5)
      ENDIF

!     Close daily output files.
      CLOSE (LUN)

!     --------------------------------------------------------
!     Seasonal K balance
      IF (DOPRINT) THEN
        WRITE(LUNK,400) NINT(StartK), YEARi, DOYi
      
        WRITE(LUNK,410) NINT(CumFertK)
        WRITE(LUNK,420) NINT(CumUptakeK)
        WRITE(LUNK,430) NINT(SKiTotProf), YEAR, DOY
        
        BALANCE = StartK + CumFertK - CumUptakeK - SKiTotProf
        WRITE(LUNK,440) BALANCE
      ENDIF

  400 FORMAT(/,"!  Starting K:    ",I10," kg/ha on ",I4,1X,I3.3) 
  410 FORMAT(  "!  Fertilizer K: +",I10," kg/ha") 
  420 FORMAT(  "!  K Uptake:     -",I10," kg/ha") 
  430 FORMAT(  "!  Ending K:     -",I10," kg/ha on ",I4,1X,I3.3) 
  440 FORMAT(  "!                  -------------",
     &       /,"!  K Balance:     ",F10.1," kg/ha",
     &      //,"!  -----------------------------------------------",/) 
    
!     --------------------------------------------------------
!     Store Summary.out labels and values in arrays to send to
!     OPSUM routines for printing.  Integers are temporarily 
!     saved as real numbers for placement in real array.

      LABEL(1) = 'KI#M'; VALUE(1) = FLOAT(FertData % NAPFER(Kel)) 
      LABEL(2) = 'KICM'; VALUE(2) = CumFertK      !P applied (kg/ha)
      LABEL(3) = 'KUPC'; VALUE(3) = CumUptakeK    !Cumul P uptake(kg/ha)
      LABEL(4) = 'SKAM'; VALUE(4) = SKiAvlProf    !Soil P at mat (kg/ha)

!     Send labels and values to OPSUM
      CALL SUMVALS (SUMNUM, LABEL, VALUE) 

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************

      RETURN
      END SUBROUTINE OpSoilKi

!=======================================================================
