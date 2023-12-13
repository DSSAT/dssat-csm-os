!=======================================================================
C  MODULE FertType_mod
C  06/15/2014 CHP Written
!=======================================================================

      MODULE FertType_mod
!     Contains data definitions for flexible fertilizer inputs
      USE ModuleDefs

!     Data construct for control variables
      TYPE FertPropType
        REAL NO3_N_pct, NH4_N_pct, UREA_N_pct
        REAL UIEFF, UIDUR, NIEFF, NIDUR, NRL50, NSIGK
        CHARACTER*6 FertN, FertP, FertK, FertC, FertMg, FertS
        CHARACTER*3 NRFNC
        CHARACTER*35 FertName
        INTEGER Check 
      END TYPE FertPropType

      INTEGER, PARAMETER :: NFertTypes = 999
      Type (FertPropType), dimension(NFertTypes), protected :: FertFile
      INTEGER Number_of_Fertilizers

      TYPE SlowReleaseNType
        INTEGER StartYRDOY
        INTEGER KMAX
        REAL N0, NRL50, KN, AN, BN, NO3_frac, NH4_frac, UREA_frac
        REAL FMIXEFF, PROF(NL)
        REAL CumRelToday, CumRelYesterday
        LOGICAL ACTIVE
      END TYPE SlowReleaseNType

      INTEGER, PARAMETER :: NSlowRelN = 5
      Type (SlowReleaseNType), dimension(NSlowRelN) :: SlowRelN
      INTEGER NActiveSR

      TYPE NIType
        REAL NIEFF
        INTEGER NIEND, NILYR
      END TYPE NIType
      TYPE (NIType) NIData

      TYPE UIType
        REAL UIEFF
        INTEGER UIEND, UILYR
      END TYPE UIType
      TYPE (UIType) UIData

!=======================================================================
      CONTAINS

!=======================================================================
!  FertTypeRead
!  Reads fertilizer characteristics from FERTCH???.SDA file.
!-----------------------------------------------------------------------
!  REVISION       HISTORY
!  2019-06-04 CHP Written
!=======================================================================

      SUBROUTINE FertTypeRead(CONTROL) 
!-------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL PATH, GETLUN, FIND2, IGNORE
      SAVE

      TYPE (ControlType), INTENT(IN) :: CONTROL

      LOGICAL FEXIST
      INTEGER LUNF, LNUM, LINE, FOUND, ISECT, I
      INTEGER ERR, Ftype
      INTEGER PFLAG

      CHARACTER*8, PARAMETER :: ERRKEY = 'FertRead'
      CHARACTER*150 FILEF, CHARTEST

      CHARACTER*12 NAMEF
      CHARACTER*80 PATHSD

!-----------------------------------------------------------------------
!     Read from FERTCH???.SDA file
      FILEF = 'FERCH' // ModelVerTxt // '.SDA'
      LNUM = 0
      INQUIRE (FILE = FILEF, EXIST = FEXIST)

      IF (.NOT. FEXIST) THEN
        CALL PATH('STD',CONTROL%DSSATP,PATHSD,PFLAG,NAMEF)
        FILEF = TRIM(PATHSD) // FILEF
      ENDIF

      INQUIRE (FILE = FILEF, EXIST = FEXIST)
      IF (.NOT. FEXIST) THEN
        CALL Fert_ERROR(29, ERRKEY, FILEF, LNUM, LUNF)
        RETURN
      ENDIF

      CALL GETLUN('FILEF', LUNF)
      OPEN (LUNF,FILE=FILEF,STATUS='OLD',IOSTAT=ERR)
      IF (ERR > 0) THEN
        CALL Fert_ERROR(10, ERRKEY, FILEF, LNUM, LUNF)
        RETURN
      ENDIF

!     Set check variable to zero
      FertFile % Check = 0

!     Read characteristics for each fertilizer type 
      REWIND (LUNF)
      CALL FIND2(LUNF,'@CDE',LINE,FOUND); LNUM = LNUM + LINE
      IF (FOUND == 1) THEN
        I = 0
        DO WHILE (.TRUE.)
          CALL IGNORE(LUNF,LNUM,ISECT,CHARTEST)
          IF (ISECT == 1) THEN
            READ(CHARTEST,'(2X,I3)',IOSTAT=ERR) Ftype
            IF (ERR .GT. 0) CALL Fert_ERROR(64, ERRKEY, FILEF,LNUM,LUNF)
            IF (Ftype .GT. NFertTypes) 
     &        CALL Fert_ERROR(42, ERRKEY, FILEF, LNUM, LUNF)

            READ(CHARTEST,'(6X,A35,3X,8F6.0,3X,A3,F6.0,6A6)',IOSTAT=ERR)
     &        FertFile(Ftype) % FertName, 
     &        FertFile(Ftype) % NO3_N_pct, 
     &        FertFile(Ftype) % NH4_N_pct, 
     &        FertFile(Ftype) % UREA_N_pct,
     &        FertFile(Ftype) % UIEFF, 
     &        FertFile(Ftype) % UIDUR, 
     &        FertFile(Ftype) % NIEFF, 
     &        FertFile(Ftype) % NIDUR, 
     &        FertFile(Ftype) % NRL50, 
     &        FertFile(Ftype) % NRFNC, 
     &        FertFile(Ftype) % NSIGK,
     &        FertFile(Ftype) % FertN, 
     &        FertFile(Ftype) % FertP, 
     &        FertFile(Ftype) % FertK 

!           The remaining variables are included in the fertilizer 
!           lookup file, but are not currently used by the model.
!     &        FertFile(Ftype) % FertS, 
!     &        FertFile(Ftype) % FerMg, 
!     &        FertFile(Ftype) % FerCa, 
!     &        FertFile(Ftype) % FertB, 
!     &        FertFile(Ftype) % FerZn, 
!     &        FertFile(Ftype) % FerMn, 
!     &        FertFile(Ftype) % FerFe, 
!     &        FertFile(Ftype) % Form, 
!     &        FertFile(Ftype) % Notes

            IF (ERR .GT. 0) CALL Fert_ERROR(64, ERRKEY, FILEF,LNUM,LUNF)
            I = I + 1
            FertFile(Ftype) % Check = 1
          ELSE
            Number_of_Fertilizers = I
            EXIT
          ENDIF
        ENDDO

      ELSE
        CALL Fert_ERROR(64, ERRKEY, FILEF, LNUM, LUNF)
        RETURN
      ENDIF

      RETURN
      END SUBROUTINE FertTypeRead
!======================================================================

!=======================================================================
!  FertProps
!  Sends fertilizer properties to FertPlace as needed.
!-----------------------------------------------------------------------
!  REVISION       HISTORY
!  2019-06-04 CHP Written
!=======================================================================

      SUBROUTINE FertProps() 
!-------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
!-----------------------------------------------------------------------


      RETURN
      END SUBROUTINE FertProps
!======================================================================

!=======================================================================
!  FERTILIZERTYPE, Subroutine for fertilizer placement module.
!  Determines fertilizer type
!-----------------------------------------------------------------------
!  Revision history
!  09/26/2003 AJG Separated this code from FPLACE_C into a new 
!                 subroutine, restructured it and added P.
!  12/17/2003 AJG Renamed all the CH_ and CHEM_ variables to P_ variables.
!  05/06/2004 AJG Removed a few errors and the line in which FERTYPE 17
!                 was set back to 12.
!  01/20/2005 CHP Moved to generic fertilizer placement module, removed
!                 "_C" from name and removed fertilizer distribution.
!
!-----------------------------------------------------------------------
!  Called : FPLACE_C
!  Calls  : DISTRIB_SOILPi_C
!=======================================================================

      SUBROUTINE FERTILIZERTYPE (ISWITCH,
     &    ANFER, APFER, AKFER, FERTYPE, FERTYPE_CDE,      !Input
     &    HASN, HASP, HASK, HASUI, HASNI, HASCR)          !Output

!     ------------------------------------------------------------------
      IMPLICIT  NONE
      EXTERNAL READ_DETAIL, WARNING

      LOGICAL HASN, HASP, HASK  ! N, P, K
      LOGICAL HASUI             ! Urease inhibitor
      LOGICAL HASNI             ! Nitrification inhibitor
      LOGICAL HASCR             ! Controlled release
      INTEGER FERTYPE
      REAL ANFER, APFER, AKFER
      CHARACTER*5  FERTYPE_CDE
      CHARACTER*6, PARAMETER :: ERRKEY = 'FPLACE'
      CHARACTER*35 FERTYPE_TEXT
      CHARACTER*78 MSG(5)

      Type (SwitchType) ISWITCH

      HASN = .FALSE.
      HASP = .FALSE.
      HASK = .FALSE.
      HASUI= .FALSE.
      HASNI= .FALSE.
      HASCR= .FALSE.

!     ------------------------------------------------------------------
!     Does this fertilizer have N?
      CALL HASE_check(FertFile(Fertype) % FertN, HASN)
      IF (.NOT. HASN .AND. ANFER > 1.E-6 .AND. ISWITCH%ISWNIT == 'Y') 
     &          THEN
        CALL READ_DETAIL(5, 35, FERTYPE_CDE, '*Ferti', FERTYPE_TEXT)
        MSG(1) = "Invalid fertilizer data in experiment file."
        WRITE(MSG(2),'(A,A,A,A)') 
     &    "Fertilizer type: ",FERTYPE_CDE, " ",FERTYPE_TEXT
        WRITE(MSG(3),'(A,F8.1,A)')"Amount N specified: ",ANFER," kg/ha"
        MSG(4)="Check DETAIL.CDE file for valid fertilizer types."
        MSG(5)="No N applied on this date."
        CALL WARNING(5, ERRKEY, MSG)
      ENDIF

!     Does this fertilizer have P?
      CALL HASE_check(FertFile(Fertype) % FertP, HASP)
      IF (.NOT. HASP .AND. APFER > 1.E-6 .AND. ISWITCH % ISWPHO == 'Y') 
     &          THEN
        CALL READ_DETAIL(5, 35, FERTYPE_CDE, '*Ferti', FERTYPE_TEXT)
        MSG(1) = "Invalid fertilizer data in experiment file."
        WRITE(MSG(2),'(A,A,A,A)') 
     &    "Fertilizer type: ",FERTYPE_CDE, " ",FERTYPE_TEXT
        WRITE(MSG(3),'(A,F8.1,A)')"Amount P specified: ",APFER," kg/ha"
        MSG(4)="Check DETAIL.CDE file for valid fertilizer types."
        MSG(5)="No P applied on this date."
        CALL WARNING(5, ERRKEY, MSG)
      ENDIF

!     Does this fertilizer have K?
      CALL HASE_check(FertFile(Fertype) % FertK, HASK)
      IF (.NOT. HASK .AND. AKFER > 1.E-6 .AND. ISWITCH % ISWPOT == 'Y') 
     &          THEN
        CALL READ_DETAIL(5, 35, FERTYPE_CDE, '*Ferti', FERTYPE_TEXT)
        MSG(1) = "Invalid fertilizer data in experiment file."
        WRITE(MSG(2),'(A,A,A,A)') 
     &    "Fertilizer type: ",FERTYPE_CDE, " ",FERTYPE_TEXT
        WRITE(MSG(3),'(A,F8.1,A)')"Amount K specified: ",AKFER," kg/ha"
        MSG(4)="Check DETAIL.CDE file for valid fertilizer types."
        MSG(5)="No K applied on this date."
        CALL WARNING(5, ERRKEY, MSG)
      ENDIF

!     Does this fertilizer have urease inhibitor?
      IF (FertFile(Fertype) % UIEFF .GT. 1.E-6 .AND.
     &    FertFile(Fertype) % UIDUR .GT. 1.E-6) THEN
        HASUI = .TRUE.
      ELSE
        HASUI = .FALSE.
      ENDIF

!     Does this fertilizer have nitrification inhibitor?
      IF (FertFile(Fertype) % NIEFF .GT. 1.E-6 .AND.
     &    FertFile(Fertype) % NIDUR .GT. 1.E-6) THEN
        HASNI = .TRUE.
      ELSE
        HASNI = .FALSE.
      ENDIF

!     Does this fertilizer have controlled or slow release?
      IF (FertFile(Fertype) % NRL50 .GT. 1.E-6) THEN
        HASCR = .TRUE.

!       Set the sigma k value based on curve type
        IF (FertFile(Fertype) % NRFNC .EQ. "LIN") THEN
          FertFile(Fertype) % NSIGK = 0.2
        ELSEIF (FertFile(Fertype) % NRFNC .EQ. "STP") THEN
          FertFile(Fertype) % NSIGK = 2.0
        ELSE 
          IF (FertFile(Fertype) % NSIGK .LT. 0.2) THEN
            FertFile(Fertype) % NSIGK = 0.2
          ENDIF
          IF (FertFile(Fertype) % NSIGK .GT. 2.0) THEN
            FertFile(Fertype) % NSIGK = 2.0
          ENDIF
        ENDIF
      ELSE
        HASCR = .FALSE.
      ENDIF

      RETURN
      END SUBROUTINE FERTILIZERTYPE


!=======================================================================
!     Check if element (N, P, or K) is supposed to be found in fertilizer 
      Subroutine HASE_check(FertE, HasE)
      Implicit None

      CHARACTER*6 FertE
      LOGICAL HasE
      INTEGER ERR
      REAL Fert_content

      IF (FertE(4:6) .EQ. 'var' .OR. FertE(4:6) .EQ. 'VAR') THEN
        HASE = .TRUE. 
        RETURN
      ENDIF

      READ (FertE,'(F6.0)', IOSTAT=ERR) Fert_content

      IF (ERR > 0) THEN
        HASE = .FALSE.
        RETURN
      ENDIF 

      IF (Fert_content .GT. 1.E-6) THEN
        HASE = .TRUE.
        RETURN
      ENDIF

      End Subroutine HASE_check

!=======================================================================
      SUBROUTINE Fert_ERROR(IERROR, ERRKEY, FILEF, LNUM, LUNF)
!     Error handling routine for fertilizer characteristics input 

      IMPLICIT NONE
      EXTERNAL WARNING, ERROR

      CHARACTER* 6 ERRKEY
      CHARACTER*78 MSG(4)
      CHARACTER*150 FILEF
      INTEGER IERROR, LNUM, LUNF

      CLOSE (LUNF)

      MSG(2) = FILEF(1:78)
      WRITE(MSG(3),'(A,I4)') "Line number ", LNUM

      SELECT CASE(IERROR)

      CASE (10)
        MSG(1) ='Error opening fertilizer characteristics file.'
        WRITE(MSG(4),'(A)') "Program will stop."
        CALL WARNING(4,ERRKEY,MSG)
        CALL ERROR(ERRKEY,IERROR,FILEF,LNUM)

      CASE (29)
        MSG(1) = 'Fertilizer characteristics file does not exist.'
        WRITE(MSG(4),'(A)') "Program will stop."
        CALL WARNING(4,ERRKEY,MSG)
        CALL ERROR(ERRKEY,IERROR,FILEF,LNUM)

      CASE (42)
        MSG(1) ='Invalid fertilizer type in file.'
        WRITE(MSG(4),'(A)') "Program will stop."
        CALL WARNING(4,ERRKEY,MSG)
        CALL ERROR(ERRKEY,IERROR,FILEF,LNUM)

      CASE (64)
        MSG(1) ='Invalid format in fertilizer characteristics file.'
        WRITE(MSG(4),'(A)') "Program will stop."
        CALL WARNING(4,ERRKEY,MSG)
        CALL ERROR(ERRKEY,IERROR,FILEF,LNUM)

      END SELECT

      RETURN
      END SUBROUTINE Fert_ERROR
!======================================================================
      END MODULE FertType_mod
!======================================================================

