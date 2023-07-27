!=======================================================================
!  Fert_Place, Subroutine
!  Determines fertilizer placement (previously FPLACE and FPLACE_C)
!-----------------------------------------------------------------------
!  Revision history
!-----------------------------------------------------------------------
!  08/08/1992 WTB Modified for version 3 input/output (matches LeGRO)  
!  02/18/1993 PWW Header revision and minor changes   
!  05/20/1999 CHP Modular format
!  09/16/1999 CHP Return changes to SNH4, SNO3, UREA as DLTSNH4, 
!                 DLTSNO3, DLTUREA  (integration performed in NTRANS)
!  01/01/2000 CHP/AJG Integrated the CENTURY-based and CERES-based SOM
!                 modules with CHP's modular format.
!  03/01/2000 AJG Added link between fertilizer incorporation and
!                 surface-residue incorporation. Renamed:
!                 IFTYPE to FERTYP, M to FERTYPE, FERCOD to FERMET.
!  03/16/2000 GH  Incorporated in CROPGRO
!  07/01/2000 GH  Renamed FERDEP to FERDEPTH, DFERT to FERDEP, 
!                 CD to CUMDEP
!  04/16/2002  GH Adjusted for crop rotations
!  08/17/2002  GH Modified for Y2K  
!  08/12/2003 CHP Added I/O error checking
!  09/29/2003 AJG Reorganized for incorporating the P module. Combined
!                 with subroutine CHEM_APP. Split off FERTILIZERTYPE
!                 as a separate subroutine.
!  12/17/2003 AJG Renamed all the CH_ and CHEM_ variables to P_ variables.
!  05/18/2004 AJG Renamed variables for P module
!                   NAPNIT to NAPFER(NELEM)
!                   AMTNIT to AMTFER(NELEM)
!  10/28/2004 CHP Fixed problem with multiple applications on same day.
!  01/18/2005 CHP Generic fertilizer routine
C  03/17/2005 CHP Moved fertilizer distribution routine to separate 
C                 subroutine to accomodate multiple applications on a
C                 single day with different fertilizer types and depths.
!  10/31/2007 CHP Added simple K model.
!  10/14/2008 CHP Added "F" option, treat same as "A" option
!-----------------------------------------------------------------------
!  Called : MgmtOps 
!  Calls  : Function IDLAYR
C=======================================================================

      SUBROUTINE Fert_Place (CONTROL, ISWITCH, 
     &  DLAYR, DS, FLOOD, NLAYR, YRPLT,           !Input
     &  FERTDATA)                                 !Output

!     ------------------------------------------------------------------
      USE FertType_mod
      USE ModuleData

      IMPLICIT  NONE
      EXTERNAL INCDAT, YR_DOY, ERROR, FIND, TIMDIF, WARNING, 
     &  FERTLAYERS, FERTAPPLY, IDLAYR
      SAVE
!     ------------------------------------------------------------------

      CHARACTER*1  IFERI, RNMODE
      CHARACTER*2  FERTYPEN
      CHARACTER*5  FERMET(NAPPL), FERTYPE_CDE(NAPPL)
      CHARACTER*6  SECTION
      CHARACTER*7  AppType
      CHARACTER*30 FILEIO 
      CHARACTER*78 MSG(10)
      CHARACTER*90 CHAR
      CHARACTER*6, PARAMETER :: ERRKEY = 'FPLACE'

      INTEGER DAP, DYNAMIC, ERRNUM, FERTDAY, FERTYPE,
     &  FOUND, FTYPEN, I, IDATE,    
     &  LINC, LNUM, LUNIO, MULTI, NAPFER(NELEM), 
     &  NFERT, NLAYR, TIMDIF
      INTEGER YR, YRDIF, YRDNIT, YRDOY, YRPLT, YRSIM
      INTEGER METFER
      INTEGER FDAY(NAPPL), FERTYP(NAPPL)

      REAL DSOILN , FERDEPTH,  !, FERMIXPERC,
     &  FERNIT, FERPHOS, FERPOT, SOILNC, SOILNX

      REAL FERMIXPERC

      REAL AMTFER(NELEM), ANFER(NAPPL), APFER(NAPPL), AKFER(NAPPL), 
     &  DLAYR(NL), DS(NL), ADDSNH4(NL), ADDSPi(NL), ADDSKi(NL),
     &  ADDSNO3(NL), ADDUREA(NL), FERDEP(NAPPL), AddBuffer(NL)

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (FertType)    FertData

      LOGICAL UNINCO

      REAL ADDFUREA, ADDFNO3, ADDFNH4
      REAL FLOOD, ADDOXU, ADDOXH4, ADDOXN3

!     Added with flexible fertilizer types
      LOGICAL FIRST
      LOGICAL HASN, HASP, HASK, FERTILIZE_TODAY
      LOGICAL HASUI, HASNI, HASCR

      REAL FERNO3, FERNH4, FERUREA
      INTEGER INCDAT, L, NSR

      REAL N0, NRL50, KN, AN
      REAL CumRelYesterday, CumRelToday, AmtRelToday
      REAL TIME, Tlinear, Y

      INTEGER KMAX
      REAL FMIXEFF
      REAL PROF(NL)

      DATA FIRST /.TRUE./

!-----------------------------------------------------------------------

      IFERI = ISWITCH % IFERI
!     IF (IFERI .EQ. 'N') RETURN

!      ===================================================
!      See fertilizer types in external file:
!      ..\DSSAT48\StandardData\FERCH048.SDA
!      ===================================================

      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC == SEASINIT) THEN
C-----------------------------------------------------------------------
!     Check for invalid fertilizer option.
!     CHP 10/14/2008 Added "F" option
      IF (INDEX('AFRDN',IFERI) .EQ. 0) THEN
        WRITE(MSG(1),300) IFERI
        WRITE(MSG(2),310) 
        CALL WARNING(2, ERRKEY, MSG)
        ISWITCH % IFERI = 'N'
      ENDIF

  300 FORMAT(
     &    'Warning: The fertilizer option, "',A1,'" is not currently ')
  310 FORMAT('supported.  No fertilizer applications will be added.')

      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO
      MULTI   = CONTROL % MULTI
      RNMODE  = CONTROL % RNMODE
      YRDIF   = CONTROL % YRDIF
      YRSIM   = CONTROL % YRSIM

!     Read fertilizer table into memory only once
      IF (FIRST) THEN
        CALL FertTypeRead(CONTROL)
        FIRST = .FALSE.
      ENDIF

!     Read FPLACE data from FILEIO.
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT = ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, 0)
      LNUM = 0

!     ------------------------------------------------------------------
!     Find AUTOMATIC MANAGEMENT Section
!     ------------------------------------------------------------------
      SECTION = '!AUTOM'
      CALL FIND (LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC

      IF (FOUND == 0) THEN
        CALL ERROR (SECTION, 42, FILEIO, LNUM)
      ELSE
        READ (LUNIO,'(//,14X,3F6.0,4X,A2)',IOSTAT=ERRNUM) 
     &        DSOILN,SOILNC,SOILNX, FERTYPEN
        LNUM = LNUM + 3
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, LNUM)
        READ(FERTYPEN,'(I2)',IOSTAT=ERRNUM) FTYPEN
        IF (ERRNUM .NE. 0) FTYPEN = 1
      ENDIF

!###AJG  Needs an automatic P fertilizer option in fileX ???

!     ------------------------------------------------------------------
!     Find FERTILIZER Section
!     ------------------------------------------------------------------
      SECTION = '*FERTI'
      CALL FIND (LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC

      IF (FOUND == 0) THEN
        CALL ERROR (SECTION, 42, FILEIO, LNUM)
      ELSE
        NFERT = 0

        DO I = 1, NAPPL
          READ (LUNIO, '(3X,I7,A90)', ERR = 90, END = 90) FDAY(I), CHAR
          LNUM = LNUM + 1

          READ(CHAR,'(1X,A5,1X,A5,4F6.0)',IOSTAT=ERRNUM) FERTYPE_CDE(I),
     &      FERMET(I), FERDEP(I), ANFER(I), APFER(I), AKFER(I)
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY, ERRNUM, FILEIO, LNUM)

          READ(FERTYPE_CDE(I),'(2X,I3)') FERTYP(I)
          IF (FertFile(FerTyp(I)) % Check .EQ. 0) THEN
            MSG(1) = "Invalid fertilizer code specified."
            WRITE(MSG(2),'(I7,A)') FDAY(I), CHAR(1:71)
            MSG(3) = "This fertilizer application will be ignored."
            CALL WARNING(3, ERRKEY, MSG)
            CYCLE
          ENDIF
!         The number of fertilizer applications to be done in this run.
          NFERT = NFERT + 1
        ENDDO
   90   CONTINUE
      ENDIF

      CLOSE (LUNIO)

!     Initialize to zero -- for all elements modeled.
      AMTFER = 0.
      NAPFER = 0

!-----------------------------------------------------------------------
!     Adjust for multi year runs
!-----------------------------------------------------------------------
      IF (MULTI > 1 .AND. NFERT > 0 .AND. IFERI .NE. 'D') THEN
!       Open the FILEIO input file.
        OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT = ERRNUM)
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, 0)

        SECTION = '*FERTI'
        CALL FIND (LUNIO, SECTION, LNUM, FOUND)

!       If the fertilizer section can't be found, call an error, or else
!       read the input data.
        IF (FOUND == 0) THEN
          CALL ERROR (SECTION, 42, FILEIO, LNUM)
        ELSE
          DO I = 1, NFERT
            READ (LUNIO, '(3X, I7)', IOSTAT = ERRNUM, ERR = 5010,
     &         END = 5010) FDAY(I)
          ENDDO
5010      CONTINUE
        ENDIF   !End of IF block on FOUND.

!       Close input file.
        CLOSE (LUNIO)

!       Adjust dates for seasonal runs.
        DO I = 1, NFERT
          CALL YR_DOY (FDAY(I), YR, IDATE)
          FDAY(I) = (YR + MULTI - 1) * 1000 + IDATE
        ENDDO
      ENDIF

!-----------------------------------------------------------------------
!     Adjust for crop rotations
!-----------------------------------------------------------------------
      IF (RNMODE == 'Q') THEN
        IF (NFERT > 0 .AND. FDAY(1) < YRSIM .AND. 
     &          IFERI .NE. 'D') THEN
          DO I = 1, NFERT
            CALL YR_DOY (FDAY(I), YR, IDATE)
            FDAY(I) = (YR + YRDIF) * 1000 + IDATE
          ENDDO
        ENDIF   !End of IF block on NFERT, FDAY and IFERI
      ENDIF   !End of IF block on RNMODE

!     ------------------------------------------------------------------
      YRDNIT = 0
      UNINCO = .FALSE.

      ADDFUREA = 0.0
      ADDFNH4  = 0.0
      ADDFNO3  = 0.0

      ADDOXU   = 0.0
      ADDOXH4  = 0.0
      ADDOXN3  = 0.0

      ADDSNH4  = 0.0
      ADDSNO3  = 0.0
      ADDUREA  = 0.0

      ADDSPi   = 0.0
      ADDSKi   = 0.0
      ADDBuffer= 0.0

      DO I = 1, NSlowRelN     !max # that can be applied
        SlowRelN(I) % ACTIVE = .FALSE.
      ENDDO
      NSR = 0                 !actual # applied in this simulation

      UIDATA % UIEND = 0
      NIDATA % NIEND = 0
      NActiveSR = 0

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS 
!***********************************************************************
      ELSEIF (DYNAMIC == RATE) THEN
!-----------------------------------------------------------------------
!     Initialize daily
      ADDFUREA = 0.0
      ADDFNH4  = 0.0
      ADDFNO3  = 0.0
      ADDOXU   = 0.0
      ADDOXH4  = 0.0
      ADDOXN3  = 0.0
      ADDSNH4  = 0.0
      ADDSNO3  = 0.0
      ADDUREA  = 0.0
      ADDSPi   = 0.0
      ADDSKi   = 0.0

!     Buffer for alternate electron acceptors (methane production)
      ADDBuffer = 0.0

      FERTDAY  = 0
      FERMIXPERC = 0.0
      FERDEPTH = 0.0

      FERNIT = 0.
      FERNO3 = 0.
      FERNH4 = 0.
      FERUREA = 0.
      FERPHOS = 0.
      FERPOT = 0.0

      IF (YRDOY .EQ. UIDATA % UIEND) THEN
        UIDATA % UIEFF = 0.0
        UIDATA % UIEND = YRDOY
      ENDIF

      IF (YRDOY .EQ. NIDATA % NIEND) THEN
        NIDATA % NIEFF = 0.0
        NIDATA % NIEND = YRDOY
      ENDIF

      FertLoop: DO I = 1, NFERT
        FERTILIZE_TODAY = .FALSE.
!       ------------------------------------------------------------------
!       Fertilize on specified dates (YYDDD format)
!       ------------------------------------------------------------------
        IF (NFERT > 0 .AND. IFERI == 'R') THEN
          IF (YRDOY == FDAY(I)) THEN
            FERTILIZE_TODAY = .TRUE.
          ELSEIF (FDAY(I) .GT. YRDOY) THEN
            EXIT FertLoop
          ENDIF
!       ------------------------------------------------------------------
!       Fertilize on specified days (DDD format)
!       ------------------------------------------------------------------
        ELSEIF (NFERT > 0 .AND. IFERI == 'D') THEN
          DAP = MAX (0, TIMDIF(YRPLT, YRDOY))
          IF ((FDAY(I) .NE. 0 .AND. DAP == FDAY(I)) .OR.
     &      (FDAY(I) == 0 .AND. YRDOY == YRPLT)) THEN
            FERTILIZE_TODAY = .TRUE.
          ELSEIF (FDAY(I) .GT. DAP) THEN
            EXIT FertLoop
          ENDIF
        ENDIF

        IF (.NOT. FERTILIZE_TODAY) CYCLE

        FERDEPTH = FERDEP(I)

C       Convert character codes for fertilizer method into integer
        READ (FERMET(I)(3:5),'(I3)') METFER
        FERTYPE  = FERTYP(I)

!       Go to FERTSECTION A of FERTILIZERTYPE to determine whether
!       the fertilizer contains N and/or P.
        CALL FERTILIZERTYPE (ISWITCH,
     &    ANFER(I), APFER(I), AKFER(I), FERTYPE, FERTYPE_CDE(I), !Input
     &    HASN, HASP, HASK, HASUI, HASNI, HASCR)                 !Output

        IF (HASN) THEN    !Do this only if NOT slow release
!         Set the amount of N to be applied and sum total amount of
!         N fertilizer
!         FERNIT    = FERNIT + ANFER(I) !CHP 2023-01-15
          FERNIT    = ANFER(I)
          FERNO3    = ANFER(I) * FertFile(FerType) % NO3_N_pct / 100.
          FERNH4    = ANFER(I) * FertFile(FerType) % NH4_N_pct / 100.
          FERUREA   = ANFER(I) * FertFile(FerType) % UREA_N_pct / 100.
          AMTFER(N) = AMTFER(N) + ANFER(I)
          NAPFER(N) = NAPFER(N) + 1
        ELSE 
          FERNIT  = 0.0
          FERNO3  = 0.0
          FERNH4  = 0.0
          FERUREA = 0.0

        ENDIF   !End of IF block on HASN.

!       For now P and K are NOT slow release
        IF (HASP) THEN
!         Set the amount of P to be applied and sum total amount of
!         P fertilizer
!         FERPHOS   = FERPHOS + APFER(I) !CHP 2023-01-15
          FERPHOS   = FERPHOS + APFER(I)
          AMTFER(P) = AMTFER(P) + APFER(I)
          NAPFER(P) = NAPFER(P) + 1
        ELSE 
          FERPHOS = 0.0
        ENDIF   !End of IF block on HASP.

        IF (HASK) THEN
!         Set the amount of K to be applied and sum total amount of
!         K fertilizer
!         FERPOT   = FERPOT + AKFER(I) !CHP 2023-01-15
          FERPOT   = FERPOT + AKFER(I)
          AMTFER(Kel) = AMTFER(Kel) + AKFER(I)
          NAPFER(Kel) = NAPFER(Kel) + 1
        ELSE 
          FERPOT = 0.0
        ENDIF   !End of IF block on HASP.

        IF (HASCR) THEN
          NSR = NSR + 1
          SlowRelN(NSR) % ACTIVE = .TRUE.
          SlowRelN(NSR) % CumRelYesterday = 0.0
          SlowRelN(NSR) % StartYRDOY = YRDOY
          SlowRelN(NSR) % N0 = ANFER(I)

!         How fertilizer is distributed between N components
          SlowRelN(NSR) % NO3_frac =FertFile(FerType) % NO3_N_pct/100.
          SlowRelN(NSR) % NH4_frac =FertFile(FerType) % NH4_N_pct/100.
          SlowRelN(NSR) % UREA_frac=FertFile(FerType) %UREA_N_pct/100.

!         Calculation of fertilizer release curve
          SlowRelN(NSR) % NRL50 = FertFile(FerType) % NRL50
          SlowRelN(NSR) % KN    = FertFile(FerType) % NSIGK
          SlowRelN(NSR) % AN = SlowRelN(NSR) % NRL50 *SlowRelN(NSR)%KN
        ENDIF

        IF (FERNIT > 1.E-3 .OR. FERPHOS > 1.E-3 .OR. FERPOT > 1.E-3)
     &        THEN
          FERTDAY = YRDOY

          CALL FertLayers(
     &      DLAYR, FERDEPTH, FERTYPE, METFER, NLAYR,            !Input
     &      AppType, FERMIXPERC, FMIXEFF, KMAX, PROF, UNINCO)   !Output

!         Set soil distribution for slow release fertilizers 
          IF (HASCR) THEN
            SlowRelN(NSR) % KMAX    = KMAX
            SlowRelN(NSR) % FMIXEFF = FMIXEFF
            SlowRelN(NSR) % PROF    = PROF

!           Don't add any N now for slow release fertilizers. 
            FERNH4 = 0.0
            FERNO3 = 0.0
            FERUREA = 0.0
          ENDIF

          CALL FertApply(
     &        FERNH4, FERNO3, FERUREA, FERPHOS, FERPOT,           !Input
     &        FLOOD, FMIXEFF, PROF, KMAX,                         !Input
     &        ADDFUREA, ADDFNH4, ADDFNO3, ADDOXU, ADDOXH4,        !I/O
     &        ADDOXN3, ADDSNH4, ADDSNO3, ADDUREA, ADDSPi, ADDSKi) !I/O
        ENDIF

!       chp 2023-01-24
!       Buffer capacity for alternate electron acceptors (methane)
        DO L = 1, NLayr
          SELECT CASE(FerType)
          CASE(2) !Ammonium sulfate, (NH4)2-SO4
!           For every mole of ammonium added, one electron is made available
!           Convert from kg[N] to kg[C] using atomic weights of each
            AddBuffer(L) = AddBuffer(L) + ADDSNH4(L) * 12./14.
          CASE (18) !Potassium sulfate, K2-SO4
!           For every mole of K added, one electron is made available
!           Convert from kg[K] to kg[C] using atomic weights of each
            AddBuffer(L) = AddBuffer(L) + ADDSKi(L) * 12./39.
          CASE DEFAULT
!           Need to add other fertilizers as appropriate
!           Mn(4+), NO3(-), Fe(3+), SO4(2-)
!           Use both stoichiometric ratios and number of electrons transferred.
!           For now do nothing more for other fertilizers
          END SELECT
        ENDDO

!----------------------------------------------------------------------
!       Check for inhibitors
!       This saves only the last UI application and does not consider
!         interactions of mulitple applications.
!       Urease inhibitor
        IF (FertFile(FerType) % UIEFF .GT. 1.E-6) THEN
          UIData % UIEFF = FertFile(FerType) % UIEFF
          UIData % UIEND = INCDAT(YRDOY,NINT(FertFile(FerType)%UIDUR))
          DO L = 1, NLAYR
            IF (DS(L) .GT. FERDEPTH) THEN
              UIData % UILYR = L
              EXIT
            ENDIF
          ENDDO
        ENDIF

!       Nitrification inhibitor
        IF (FertFile(FerType) % NIEFF .GT. 1.E-6) THEN
          NIData % NIEFF = FertFile(FerType) % NIEFF
          NIData % NIEND = INCDAT(YRDOY,NINT(FertFile(FerType)%NIDUR))
          DO L = 1, NLAYR
            IF (DS(L) .GT. FERDEPTH) THEN
              NIDATA % NILYR = L
              EXIT
            ENDIF
          ENDDO
        ENDIF
!----------------------------------------------------------------------

      ENDDO FertLoop

!----------------------------------------------------------------------
!     Look for slow release N today
      DO I = 1, NSR
        IF (.NOT. SlowRelN(I) % ACTIVE) CYCLE

        N0    = SlowRelN(I) % N0   
        NRL50 = SlowRelN(I) % NRL50
        KN    = SlowRelN(I) % KN   
        AN    = SlowRelN(I) % AN   
        CumRelYesterday = SlowRelN(I) % CumRelYesterday

        IF (N0 - CumRelYesterday .LT. 0.001) THEN
          SlowRelN(I) % ACTIVE = .FALSE.
          CYCLE
        ENDIF

        TIME = TIMDIF(SlowRelN(I) % StartYRDOY, YRDOY)
        CumRelToday = N0 / (1.0 + EXP(AN - KN * TIME))

!       Account for low K values with (relatively) long duration.
!       The sigmoidal curve starts before time zero in this case. 
!       Replace curve with linear for the first few days.
        IF (KN .LT. 0.3) THEN  !threshold k to trigger linear phase
          Tlinear = NRL50/4.   !duration of linear phase
          IF (TIME .EQ. 0.0) THEN
            CumRelToday = 0.0
          ELSEIF (TIME .LE. TLinear) THEN
            Y = TIME / Tlinear * N0 / (1.0 + EXP(AN - KN * Tlinear))
            CumRelToday = MIN(Y, CumRelToday)
          ENDIF
        ENDIF

!       If N is almost gone, go ahead and release it all.
        IF (CumRelToday > 0.999 * N0) THEN
          CumRelToday = N0
        ENDIF

        AmtRelToday = CumRelToday - CumRelYesterday

        FERNO3   = AmtRelToday * SlowRelN(I) % NO3_frac
        FERNH4   = AmtRelToday * SlowRelN(I) % NH4_frac
        FERUREA  = AmtRelToday * SlowRelN(I) % UREA_frac

!       For now, no P or K in slow release fertilizer
        FERPOT = 0.0
        FERPHOS = 0.0

        SlowRelN(I) % CumRelToday = CumRelToday
        SlowRelN(I) % CumRelYesterday = CumRelToday  !Save for tomorrow

        CALL FertApply(
     &    FERNH4, FERNO3, FERUREA, FERPHOS, FERPOT,           !Input
     &    FLOOD, FMIXEFF, PROF, KMAX,                         !Input
     &    ADDFUREA, ADDFNH4, ADDFNO3, ADDOXU, ADDOXH4,        !I/O
     &    ADDOXN3, ADDSNH4, ADDSNO3, ADDUREA, ADDSPi, ADDSKi) !I/O
      ENDDO

!!     ------------------------------------------------------------------
!!     Automatic N-fertilization routine
!!     ------------------------------------------------------------------
!      ELSEIF (IFERI == 'A' .OR. IFERI == 'F') THEN
!        IF ((1. - NSTRES)  * 100. > SOILNC
!  !  Add conditions for P stress also -- something like: 
!  !   &      .OR.  (1. - PSTRES1) * 100. > SOILPC)) 
!     &      .AND. YRDOY > (YRDNIT + 1)) THEN
!     &    
!!         Go to FERTSECTION A of FERTILIZERTYPE to determine whether
!!         the fertilizer contains N and/or P.
!          FERTYPE  = FTYPEN
!          FERDEPTH = DSOILN
!          METFER = 1
!          YRDNIT   = YRDOY
!          CALL FERTILIZERTYPE (ISWITCH,
!     &        SOILNX, 0.0, 0.0, FERTYPE, FERTYPE_CDE(I),
!     &        HASN, HASP, HASK)                       !Output
!
!          IF (HASN) THEN
!!           Set the amount of N to be applied and sum total amount of
!!           N fertilizer
!            FERNIT    = SOILNX
!            AMTFER(N) = AMTFER(N) + SOILNX
!            NAPFER(N) = NAPFER(N) + 1
!          ENDIF   !End of IF block on HASN.
!
!         ! IF (HASP) THEN
!!        !   Set the amount of P to be applied and sum total amount of
!!        !   P fertilizer
!         !   FERPHOS   = SOILPX
!         !   AMTFER(P) = AMTFER(P) + SOILPX
!         !   NAPFER(P) = NAPFER(P) + 1
!         ! ENDIF   !End of IF block on HASP.
!
!          IF (FERNIT > 1.E-3 .OR. FERPHOS > 1.E-3) THEN
!            CALL FertApply(
!     &        DLAYR, FERDEPTH, SOILNX, 0.0, 0.0,!Input
!     &        FERTYPE, FLOOD, METFER, NLAYR, YRDOY,       !Input
!     &        ADDFUREA, ADDFNH4, ADDFNO3, ADDOXU, ADDOXH4,!I/O
!     &        ADDOXN3, ADDSNH4, ADDSNO3, ADDUREA, ADDSPi, ADDSKi,!I/O
!     &        AppType, FERMIXPERC, FERTDAY, UNINCO)       !Output
!          ENDIF
!        ENDIF
!      ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
C-----------------------------------------------------------------------
      FertData % ADDFUREA= ADDFUREA
      FertData % ADDFNH4 = ADDFNH4
      FertData % ADDFNO3 = ADDFNO3

      FertData % ADDOXU  = ADDOXU
      FertData % ADDOXH4 = ADDOXH4
      FertData % ADDOXN3 = ADDOXN3

      FertData % ADDSNH4 = ADDSNH4
      FertData % ADDSNO3 = ADDSNO3
      FertData % ADDUREA = ADDUREA

      FertData % ADDSPi  = ADDSPi
      FertData % ADDSKi  = ADDSKi
      FertData % ADDBuffer = ADDBuffer

      FertData % AMTFER  = AMTFER
      FertData % AppType = AppType
      FertData % FERTDAY = FERTDAY
      FertData % FERDEPTH= FERDEPTH
      FertData % FERTYPE = FERTYPE
      FertData % NAPFER  = NAPFER
      FertData % UNINCO  = UNINCO
      FertData % FERMIXPERC = FERMIXPERC

      NActiveSR = 0
      DO I = 1, NSlowRelN
        IF (SlowRelN(I) % ACTIVE) NActiveSR = NActiveSR + 1
      ENDDO

!     Transfer data to ModuleData
      CALL PUT('MGMT','FERNIT',AMTFER(N))

      RETURN
      END SUBROUTINE Fert_Place


C=======================================================================
C  FertLayers, Subroutine
C
C  Distributes N fertilizer constituents to soil layers
C-----------------------------------------------------------------------
C  Revision history
C
C  03/17/2005 CHP pulled N fertilizer distribution from FPLACE 
!  10/29/2019 CHP separated FertApply into two subroutines:
!             FertLayers - determines the distribution in soil layers
!             FertApply - applies fertilizer amounts to soil layers
!     This is to facilitate slow release fertilizers that will add
!     a little bit every day to exactly the same layers. 
C=======================================================================
      SUBROUTINE FertLayers(
     &    DLAYR, FERDEPTH, FERTYPE, METFER, NLAYR,            !Input
     &    AppType, FERMIXPERC, FMIXEFF, KMAX, PROF, UNINCO)   !Output

!-----------------------------------------------------------------------
      USE ModuleDefs
      USE FloodModule
      IMPLICIT NONE
      EXTERNAL IDLAYR, WARNING
      SAVE

!     ------------------------------------------------------------------
      CHARACTER*78 MSG(10)
      CHARACTER*6, PARAMETER :: ERRKEY = 'FPLACE'
      CHARACTER*7  AppType

      INTEGER FERTYPE, I, IDLAYR, KMAX, L, NLAYR

      REAL CUMDEP, FERDEPTH
      REAL FMIXEFF, FERMIXPERC

      REAL DLAYR(NL), PROF(NL)

      LOGICAL UNINCO
      INTEGER KD, METFER
      REAL FME(10)    !Fertilizer mixing efficiency

!-----------------------------------------------------------------------
!Fertilizer methods -- METFER
! @CDE METFER FME DISTR   DESCRIPTION                                            
! AP001     1   0 Surface Broadcast, not incorporated                                          
! AP002     2 100 Layers  Broadcast, incorporated                                
! AP003     3   0 Surface Banded on surface                                      
! AP004     4 100 Layers  Banded beneath surface                                 
! AP005     5             Applied in irrigation water                            
! AP006     6             Foliar spray                                           
! AP007     7 100 Deep    Bottom of hole                                         
! AP008     8             On the seed                                            
! AP009     9 100 Deep    Injected                                               
! AP011    11   0 Surface Broadcast on flooded/saturated soil, none in soil      
! AP012    12  15 Layers  Broadcast on flooded/saturated soil, 15% in soil       
! AP013    13  30 Layers  Broadcast on flooded/saturated soil, 30% in soil           
! AP014    14  45 Layers  Broadcast on flooded/saturated soil, 45% in soil       
! AP015    15  60 Layers  Broadcast on flooded/saturated soil, 60% in soil       
! AP016    16  75 Layers  Broadcast on flooded/saturated soil, 75% in soil       
! AP017    17  90 Layers  Broadcast on flooded/saturated soil, 90% in soil       
! AP018    18  92 Layers  Band on saturated soil,2cm flood, 92% in soil          
! AP019    19  95 Deep    Deeply placed urea super granules/pellets, 95% in soil 
! AP020    20 100 Deep    Deeply placed urea super granules/pellets, 100% in soil

C           MIXING EFFICIENCY BASED ON BURESH ET AL.
!                1     2     3     4     5     6     7     8     9   10   
!       METFER  11    12    13    14    15    16    17    18    19   20
      DATA FME/0.0, 0.15, 0.30, 0.45, 0.60, 0.75, 0.90, 0.92, 0.95, 1.0/

C     Uses mixing efficiency (FME) input to determine where fertilizer goes.
C     If mixing efficiency is zero all fertilizer is in floodwater 
!     (or top soil layer).
C     If mixing efficiency is 1.0 all fertilizer retained in soil.
C     Function IDLAYR is used to identify layers for deep placement.
C     Routine assumes uniform incorporation within a layer for deep
C     point placed sources.
C
C     For all BI treatments, the routine will distribute the soil fraction
C     of the fertilizer over the layers encompassed by the incorporation
C     depth.
C
C     Need to make provision for USG as a source

!     Set fertilizer mixing efficiency based on method of fert.
      SELECT CASE (METFER)
        CASE (1, 3);  FMIXEFF = FME(1)           !0% incorporation
        CASE (2, 4:9);FMIXEFF = FME(10)          !100% incorporated
        CASE (11:20); FMIXEFF = FME(METFER - 10) !Range from 0 to 1
        CASE DEFAULT; FMIXEFF = FME(10)          !Default 100% inc.
      END SELECT

      IF (FMIXEFF < 0.98) THEN
        UNINCO = .TRUE.
      ENDIF

      KMAX = 1
      PROF = 0.

      SELECT CASE (METFER)
        CASE (1,3,11)
!         Surface placement
          KMAX = 1
          PROF(1) = 1.0

        CASE (2,4,12:18)  
!       These treatments are incorporated
!       Remainder of fertilizer (FME) distributed over layers
           KMAX = IDLAYR (NLAYR,DLAYR,FERDEPTH)
           CUMDEP   = 0.0
          IF (KMAX == 1) THEN
!           Incorporation shallow - only surface layer
            PROF(1) = 1.0
          ELSE
            CUMDEP = DLAYR(1)
            PROF(1)   = DLAYR(1) / FERDEPTH

            DO L = 2, KMAX
              CUMDEP = CUMDEP + DLAYR(L)
              IF (FERDEPTH <= CUMDEP) THEN
                PROF(L) = (FERDEPTH - (CUMDEP - DLAYR(L))) / FERDEPTH
              ELSE
                PROF(L) = DLAYR(L) / FERDEPTH
              ENDIF
            END DO
          ENDIF

        CASE (7,8,9,19,20)
!       This is deep placement
!       All fertilizer placed in layer KD with (PROF = 1.0)
          KD = IDLAYR (NLAYR,DLAYR,FERDEPTH)
          IF (KD == 1) THEN
            WRITE (MSG(1),1000) FERTYPE,  FERDEPTH
            WRITE (MSG(2),1001)
            WRITE (MSG(3),1002)
 1000       FORMAT('Fertilizer type ',I3,'; Depth ', F5.2)
 1001       FORMAT('Deep placement of fertilizer could not ',
     &          'be accomodated.')
 1002       FORMAT('Model is forcing deep placement into ',
     &          'second layer.')
            CALL WARNING(3, ERRKEY, MSG)
            KD = 2
          ENDIF
          PROF(KD) = 1.0
          KMAX     = KD
          
        CASE (5)
!       This is applying with irrigation placement
!       All fertilizer placed in layer KD with (PROF = 1.0)
          KD = IDLAYR (NLAYR,DLAYR,FERDEPTH)
          PROF(KD) = 1.0
          KMAX     = KD

        CASE DEFAULT
          MSG(1) = "Application method not currently active in CSM."
          MSG(2) = "No fertilizer added."
          WRITE(MSG(3),'(A,I3)') "Method: ", METFER
          CALL WARNING(3,ERRKEY,MSG)
      END SELECT

      IF (KMAX > 1 .AND. FMIXEFF < 0.95) THEN
        DO I = 1, KMAX
          SELECT CASE (I)
            CASE (1)
              IF (PROF(I)*1.2 < 1.0) THEN
                PROF(I) = PROF(I)*1.2
              ENDIF

            CASE (2)
              PROF(I)   = 1.0 - PROF(I-1)

            CASE (3)
              PROF(I)   = PROF(I-1) - PROF(I)
              PROF(I)   = AMAX1 (PROF(I),0.0)
              PROF(I-1) = PROF(I-1) - PROF(I)

            CASE DEFAULT
              PROF(I)   = 0.0
          END SELECT
        END DO
      ENDIF

!     Set the percentage of the surface residues that will be
!     incorporated with the fertilizer incorporation. Set to zero
!     if superficially applied or with irrigation water, and set
!     to 100 if incorporated or deeply placed.
      SELECT CASE (METFER)
        CASE (2,4,19,20); FERMIXPERC = 100. 
        CASE DEFAULT;     FERMIXPERC = 0.
      END SELECT

!       Set the percentage of fertilizer that is applied to the root zone
!       This is used in the soil inorganic phosphorus routine to compute
!       P available for uptake by roots.
        SELECT CASE (METFER)
          CASE (3,4,18); AppType = 'BANDED '
!         CASE (7,8,9) ; AppType = 'HILL   '
          CASE (7,8,9,19,20); AppType = 'POINT  '
          CASE DEFAULT ; AppType = 'UNIFORM'
        END SELECT

      RETURN
      END SUBROUTINE FertLayers
C=======================================================================

C=======================================================================
C  FertApply, Subroutine
C
C  Distributes N fertilizer constituents to soil layers
C-----------------------------------------------------------------------
C  Revision history
C
C  03/17/2005 CHP pulled N fertilizer distribution from FPLACE 
C=======================================================================
      SUBROUTINE FertApply(
     &    FERNH4, FERNO3, FERUREA, FERPHOS, FERPOT,           !Input
     &    FLOOD, FMIXEFF, PROF, KMAX,                         !Input
     &    ADDFUREA, ADDFNH4, ADDFNO3, ADDOXU, ADDOXH4,        !I/O
     &    ADDOXN3, ADDSNH4, ADDSNO3, ADDUREA, ADDSPi, ADDSKi) !I/O

!-----------------------------------------------------------------------
      USE ModuleDefs
      USE FloodModule
      IMPLICIT NONE
      SAVE

      CHARACTER*6, PARAMETER :: ERRKEY = 'FPLACE'
      INTEGER K, KMAX

      REAL FERNO3, FERNH4, FERUREA
      REAL FERPHOS, FERPOT
      REAL FMIXEFF

      REAL ADDSNH4(NL), ADDSPi(NL), ADDSKi(NL)
      REAL ADDSNO3(NL), ADDUREA(NL), PROF(NL)

      REAL ADDFUREA, ADDFNO3, ADDFNH4
      REAL FLOOD, ADDOXU, ADDOXH4, ADDOXN3

!       Add the fertilizer to the appropriate layer. 
        ADDFNO3   = ADDFNO3   + FERNO3  * (1.0 - FMIXEFF)
        ADDFNH4   = ADDFNH4   + FERNH4  * (1.0 - FMIXEFF)
        ADDFUREA  = ADDFUREA  + FERUREA * (1.0 - FMIXEFF)
        ADDSPi(1) = ADDSPi(1) + FERPHOS * (1.0 - FMIXEFF)
        ADDSKi(1) = ADDSKi(1) + FERPOT  * (1.0 - FMIXEFF)
        DO K = 1, KMAX
          ADDSNO3(K) = ADDSNO3(K) + FERNO3  * FMIXEFF * PROF(K)
          ADDSNH4(K) = ADDSNH4(K) + FERNH4  * FMIXEFF * PROF(K)
          ADDUREA(K) = ADDUREA(K) + FERUREA * FMIXEFF * PROF(K)
          ADDSPi(K)  = ADDSPi(K)  + FERPHOS * FMIXEFF * PROF(K)
          ADDSKi(K)  = ADDSKi(K)  + FERPOT  * FMIXEFF * PROF(K)
        END DO

!     -----------------------------------------------------
!       If no flood, N goes to oxidation layer and top soil layer
        IF (ABS(FLOOD) < 1.E-4) THEN
          ADDOXU  = ADDOXU  + ADDFUREA
          ADDOXH4 = ADDOXH4 + ADDFNH4
          ADDOXN3 = ADDOXN3 + ADDFNO3

          ADDUREA(1) = ADDUREA(1) + ADDFUREA
          ADDSNH4(1) = ADDSNH4(1) + ADDFNH4
          ADDSNO3(1) = ADDSNO3(1) + ADDFNO3
          ADDFUREA= 0.0
          ADDFNH4 = 0.0
          ADDFNO3 = 0.0
        ENDIF

      RETURN
      END SUBROUTINE FertApply
C=======================================================================

C=======================================================================
C  IDLAYR, Function
C
C  Determines layer where fertilizer is placed
C-----------------------------------------------------------------------
C  Revision history
C
C  02/08/93 PWW Written
C  02/08/93 PWW Header revision and minor changes 
C-----------------------------------------------------------------------
C  INPUT  : NLAYR FLAYR FDEPTH
C  LOCAL  : L
C OUTPUT : IDLAYR
C-----------------------------------------------------------------------
C  Called : FPLACE_C, FERTILIZER
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  NLAYR  : Number of layers in soil
C  DLAYR(): Thickness increment of soil layer L - cm
C  L      : Loop counter
C  FDEPTH : Fertilizer depth (cm)
C  DEPTH  : Depth to the bottom of a layer from the surface (cm)
C=======================================================================

      INTEGER FUNCTION IDLAYR (NLAYR,DLAYR,FDEPTH)

      IMPLICIT  NONE

      INTEGER   NLAYR,L
      DIMENSION DLAYR (NLAYR)
      REAL      FDEPTH,DLAYR,DEPTH

      DEPTH  = 0.0
      IDLAYR = 1
      DO L = 1, NLAYR
         DEPTH  = DEPTH + DLAYR (L)
         IF (FDEPTH <= DEPTH) THEN
            IDLAYR = L
            GO TO 10
         ENDIF
      END DO

      IDLAYR = NLAYR

   10 CONTINUE
      RETURN
      END FUNCTION IDLAYR

!=======================================================================
! FPLACE and IDLAYR Variables - updated 08/18/2003
!-----------------------------------------------------------------------
! AMTFER     Cumulative amount of N in fertilizer applications
!             (kg [N] / ha)
! ANFER(I)   Amount of nitrogen in fertilizer applied in Ith application
!             (kg [N] / ha)
! CHAR       Contains the contents of last record read 
! CONTROL    Composite variable containing variables related to control 
!              and/or timing of simulation.  The structure of the variable 
!              (ControlType) is defined in ModuleDefs.for. 
! CUMDEP     Cumulative depth of soil profile (cm)
! DAILY      Logical variable to determine whether daily or hourly flood 
!              chemistry or oxidation layer calculations are done. 
! DAP        Number of days after planting (d)
! DEPTH      Depth to the bottom of a layer from the surface (cm)
! DLAYR(L)   Thickness of soil layer L (cm)
! ADDFNH4    Rate of change of ammonium in floodwater (kg [N] / ha / d)
! ADDFNO3    Rate of change of nitrate in floodwater (kg [N] / ha / d)
! ADDFUREA   Rate of change of urea in floodwater (kg [N] / ha / d)
! ADDOXH4    Rate of change of ammonium in oxidation layer
!             (kg [N] / ha / d)
! ADDOXN3    Rate of change of nitrate in oxidation layer (kg [N] / ha / d)
! ADDOXU     Rate of change of urea in oxidation layer (kg [N] / ha / d)
! ADDSNH4(L) Rate of change of ammonium in soil layer L (kg [N] / ha / d)
! ADDSNO3(L) Rate of change of nitrate in soil layer L (kg [N] / ha / d)
! ADDUREA(L) Rate of change of urea content in soil layer L
!             (kg [N] / ha / d)
! AddBuffer(L) Buffer for alternate electron acceptors added with new
!             fertilizer (kg[C]/ha) (for suppression of methane production)
! DOY        Current day of simulation (d)
! DSOILN     Fertilizer depth for automatic fertilizer option (cm)
! ERRKEY     Subroutine name for error file 
! ERRNUM     Error number for input 
! FDAY(I)    Julian date for Ith fertilizer application (YYYYDDD)
! FDEPTH     Fertilizer depth (cm)
! FERDEP(I)  Fertilizer depth for application I (cm)
! FERDEPTH   Fertilizer depth on current day of simulation (cm)
! FERMET(I)  Fertilizer method for Ith application 
! FERNIT     Amount of nitrogen in fertilizer applied on current day of 
!              simulation (kg [N] / ha)
! FERTYP(I)  Type of fertilizer used for Ith application 
! FERTYPE    Fertilizer type for current application 
! FILEIO     Filename for input file (e.g., IBSNAT35.INP) 
! FLOOD      Current depth of flooding (mm)
! FME        Fertilizer mixing efficiency (fraction)
! FOUND      Indicator that good data was read from file by subroutine FIND 
!              (0 - End-of-file encountered, 1 - NAME was found) 
! FTYPEN     Fertilizer types 
! I          Loop counter 
! IDATE      Day of irrigation or fertilizer application (d)
! IDLAYR     Soil layer where fertilizer is placed 
! IFERI      Fertilizer switch (A,F= automatic, R= on specified dates 
!              (YYYYDDD format), D = on specified days after planting (DDD) 
!              format. 
! K          Loop counter/dummy variable 
! KD         Soil layer in which fertilizer is placed 
! KMAX       Maximum soil depth for fertilizer application (cm)
! LFD10      Date, 10 days after last fertilization.  Used to determine 
!              whether hourly flood chemistry computations will be done 
!              (see DAILY variable). (YYYYDDD)
! LINC       Line number of input file 
! LNUM       Current line number of input file 
! LUNIO      Logical unit number for FILEIO 
! MSG        Text array containing information to be written to WARNING.OUT 
!              file. 
! METFER     Numerical code for fertilizer method 
! MULTI      Current simulation year (=1 for first or single simulation, 
!              =NYRS for last seasonal simulation) 
! NAPFER     Current number of fertilizer applications applied. 
! NAPPL      Maximum number of applications (for fertilizer, irrigation, 
!              etc.) 
! NFERT      Total number of observed fertilizer applications 
! NL         Maximum number of soil layers = 20 
! NLAYR      Actual number of soil layers 
! NSTRES     Nitrogen stress factor (1=no stress, 0=max stress) 
! OXLAYR     Composite variable which contains data about oxidation layer.  
!              See ModuleDefs.for for structure. 
! PROF(L)    Proportion of soil destined fertilizer for layer L (fraction)
! RNMODE    Simulation run mode (I=Interactive, A=All treatments, 
!             B=Batch mode, E=Sensitivity, D=Debug, N=Seasonal, Q=Sequence)
! SECTION    Section name in input file 
! SOILNC     Critical threshold of soil nitrogen content to trigger 
!              automatic fertilization.  Measured as a percentage of N 
!              supply to N demand (%)
! SOILNX     Amount of N to be applied for automatic fertilization
!             (kg [N] / ha)
! UNINCO     Logical variable indicating whether fertilizer is fully 
!              incorporated; if true, ignore oxidation layer 
! YEAR       Year of current date of simulation 
! YRDIF      Increment in years which must be added to operations dates for 
!              seasonal or sequenced simulations (yr)
! YRDNIT     Date of last automatic fertilizer application (YYYYDDD)
! YRDOY      Current day of simulation (YYYYDDD)
! YRPLT      Planting date (YYYYDDD)
! YRSIM      Start of simulation date (YYYYDDD)
!======================================================================
