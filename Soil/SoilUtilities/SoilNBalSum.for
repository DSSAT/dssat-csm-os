!***********************************************************************
!  SoilNBalSum, Subroutine 
!
!  Purpose: Writes out a one-line seasonal soil N balance  
!
!  REVISION   HISTORY
!  07/11/2006 CHP Written
!***********************************************************************
!     HJ added CNTILEDR
      SUBROUTINE SoilNBalSum (CONTROL, 
     &    AMTFER, Balance, CUMIMMOB, CUMMINER, CUMRESN, 
     &    CumSenN, HARVRESN, LITE, SOM1E, CLeach, CNTILEDR, TLITN, 
     &    N_inorganic, TSOM1N, TSOM2N, TSOM3N, WTNUP,
     &    CUMFNRO, NGasLoss)

!***********************************************************************

!     ------------------------------------------------------------------
      USE GHG_mod 
      USE ModuleData
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER
      SAVE

      TYPE (ControlType), INTENT(IN) :: CONTROL
      REAL, INTENT(IN), OPTIONAL :: AMTFER, Balance, CUMIMMOB, 
     &    CUMFNRO, CUMMINER, CUMRESN, CumSenN, HARVRESN, CLeach,
     &    CNTILEDR, TLITN, N_inorganic, TSOM1N, TSOM2N, TSOM3N,	 
     &    WTNUP, NGasLoss
      REAL, DIMENSION(0:NL,3), INTENT(IN), OPTIONAL :: LITE, SOM1E

      CHARACTER(LEN=15), PARAMETER :: SNSUM = 'SoilNBalSum.OUT'
      INTEGER COUNT, ERRNUM, INDEX, LUNSNS, Num
      LOGICAL FEXIST, FIRST, PRINTN
      REAL State(6), Add(4), Sub(5), Bal(2), Miner(2) !HJ changed Sub(4)
      REAL State_init(6)
      REAL Bal_inorganic, Bal_organic

      Type (SwitchType)  ISWITCH

      DATA FIRST /.TRUE./
      DATA COUNT /0/

!     ------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.

!       Initialize output file
        CALL GETLUN(SNSUM, LUNSNS)
        INQUIRE (FILE = SNSUM, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = LUNSNS, FILE = SNSUM, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = LUNSNS, FILE = SNSUM, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
        ENDIF
        WRITE(LUNSNS,'(/,"*SOIL N BALANCE - CENTURY ROUTINES")')
        CALL HEADER(0, LUNSNS, CONTROL%RUN)

        WRITE(LUNSNS,5000) 
 5000   FORMAT(/,"!",T32,
     &   "|--------------- Initial N State Variables ----------------",
     &   "|----------------- Final N State Variables -----------------",
     &   "|------------- N Additions -------------",
     &   "|---------------- N Subtractions -----------------",
     &   "|--Mineralization---|----- Seasonal Balance -----|",
     &  /,"!",T32,
     &   "|----------------- Organic ----------------------|         ",
     &   "|------------------ Organic ----------------------|         ",
     &   "|  Harvest   Applied                    ",
     &   "|              Tile-     Plant     N gas     Flood",
     &   "|   Miner-    Immob-|                            |",
     &  /,"!",T32,
     &   "| Surface      SOM1      SOM2      SOM3    Litter     Inorg",
     &   "|  Surface      SOM1      SOM2      SOM3    Litter     Inorg",
     &   "|  Residue   Residue  Fertiliz   Senesed",
     &   "|  Leached   drained    Uptake    Losses    Losses",
     &   "|   alized    ilized|Inorganic   Organic   Overall",
     &  /,"@Run FILEX               TN CR",
     &   "     SN0Di    S1NTDi    S2NTDi    S3NTDi     LNTDi     NIADi",
     &   "      SN0D     S1NTD     S2NTD     S3NTD      LNTD      NIAD",
     &   "      HRNH     RESNC      NICM     SNNTC",
     &   "      NLCM      TDFC      NUCM     NGasC      RNRO",
     &   "      NMNC      NIMC    InNBal   OrgNBal     SNBAL")
      ENDIF

!     Organic
      IF (PRESENT(LITE) .AND. PRESENT(SOM1E)) THEN
        State(1) = LITE(0,N) + SOM1E(0,N)
        IF (PRESENT(Balance)) Bal(1) = Balance
      ENDIF
      IF (PRESENT(TSOM1N)) State(2) = TSOM1N
      IF (PRESENT(TSOM2N)) State(3) = TSOM2N
      IF (PRESENT(TSOM3N)) State(4) = TSOM3N
      IF (PRESENT(TLITN))  State(5) = TLITN

      IF (PRESENT(HARVRESN)) Add(1) = HARVRESN
      IF (PRESENT(CUMRESN))  Add(2) = CUMRESN
      IF (PRESENT(CumSenN))  Add(4) = CumSenN

!! CHP NOTE: Need to handle this. 
!!     N balance will be off by the amount of N senesced on the last day 
!!       of season because this amount has not been added to soil.  This is because
!!       soil processes are computed before plant processes.
!!     Need to subtract this amount from balance. It may be more complicated
!!      to correctly handle it, especially for crop rotations. Needs further investigation.
!! Question - does this amount ever get added to the soil? Is that why the imbalance?

!      IF (PRESENT(SENESCE)) THEN
!      TotLastSenes = 0.0
!      DO L = 0, NL
!        TotLastSenes = TotLastSenes + SENESCE % ResE(L,N)
!      ENDDO

!     Inorganic
      IF (PRESENT(N_inorganic)) THEN
        State(6) = N_inorganic
        IF (PRESENT(Balance)) Bal(2) = Balance
      ENDIF
      IF (PRESENT(AMTFER))   Add(3) = AMTFER
      IF (PRESENT(CLeach))   Sub(1) = CLeach
	IF (PRESENT(CNTILEDR)) Sub(2) = CNTILEDR     !HJ added
      IF (PRESENT(WTNUP))    Sub(3) = WTNUP
      IF (PRESENT(NGasLoss)) Sub(4) = NGasLoss
      IF (PRESENT(CUMFNRO))  Sub(5) = CUMFNRO

!     Mineralization/immobilization
      IF (PRESENT(CUMMINER)) Miner(1) = CUMMINER
      IF (PRESENT(CUMIMMOB)) Miner(2) = CUMIMMOB

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization 
!***********************************************************************
      IF (CONTROL % DYNAMIC .EQ. SEASINIT) THEN
!     ------------------------------------------------------------------
      CALL GET(ISWITCH)
      PRINTN = .FALSE.

!     Return if N output not requested.
      IF (ISWITCH % ISWNIT .EQ. 'N') RETURN !not simulating N
      IF (ISWITCH % IDETN .EQ. 'N') RETURN  !no N output
      IF (INDEX('0N', ISWITCH % IDETL) > 0) RETURN  !minimal output

      PRINTN = .TRUE.

!     ------------------------------------------------------------------
!      IF (CONTROL%RUN == 1) THEN
        COUNT = COUNT + 1
        IF (COUNT == 2) THEN
!          WRITE(LUNSNS,'(I4,1X,A12," INIT",F9.2,6F10.2)') 
!     &      CONTROL%RUN, CONTROL%FILEX, State
          State_init = State
          COUNT = 0
          State = 0.
        ENDIF
!      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal Output 
!***********************************************************************
      ELSEIF (CONTROL % DYNAMIC .EQ. SEASEND) THEN
!     ------------------------------------------------------------------
      IF (.NOT. PRINTN) RETURN

      COUNT = COUNT + 1

      IF (COUNT == 2) THEN

        IF (CONTROL % RNMODE == 'Q') THEN
          Num = CONTROL % ROTNUM
        ELSE
          Num = CONTROL % TRTNUM
        ENDIF

        Bal_inorganic = State(6) - State_init(6)          !Delta state
     &                - (Add(3) + Miner(1))               !Additions
     &                + (SUM(Sub) + Miner(2))             !Subtractions

        Bal_organic   = (SUM(State) - State(6))           !Final state
     &                - (SUM(State_init) - State_init(6)) !Initial state
     &                - (Add(1) + Add(2) + Add(4) + Miner(2)) !Additions
     &                + (Miner(1))                        !Subtractions

        WRITE(LUNSNS,'(I4,1X,A12,I10,1X,A2,30F10.2)')
     &    CONTROL%RUN, CONTROL%FILEX, Num, CONTROL%CROP,
     &    State_init, State, Add, Sub, Miner, 
     &    Bal_inorganic, Bal_organic, Bal(1)+Bal(2)

!       For crop rotations, remember the ending state to use as initial state next season.
        IF (CONTROL % RNMODE == 'Q') THEN
          State_init = State
        ENDIF

        COUNT = 0
        State = 0.
        State_init = 0.
        Add   = 0.
        Sub   = 0.
        Miner = 0.
        Bal   = 0.
      ENDIF

!***********************************************************************
!***********************************************************************
!     End of DYNAMIC IF construct
!***********************************************************************
      END IF

      RETURN
      END SUBROUTINE SoilNBalSum


!=======================================================================
      MODULE Interface_SoilNBalSum
!     Interface needed for optional arguments with SoilNBalSum
!     HJ added CNTILEDR following
      INTERFACE
        SUBROUTINE SoilNBalSum (CONTROL, 
     &    AMTFER, Balance, CUMIMMOB, CUMMINER, CUMRESN, 
     &    CumSenN, HARVRESN, LITE, SOM1E, CLeach, CNTILEDR, TLITN, 
     &    N_inorganic, TSOM1N, TSOM2N, TSOM3N, WTNUP,
     &    CUMFNRO, NGasLoss)
          USE ModuleDefs
          TYPE (ControlType), INTENT(IN) :: CONTROL
          REAL, INTENT(IN), OPTIONAL :: AMTFER, Balance, CUMIMMOB, 
     &      CUMFNRO, CUMMINER, CUMRESN, CumSenN, HARVRESN, CLeach, 
     &      CNTILEDR, TLITN, N_inorganic, TSOM1N, TSOM2N, TSOM3N,
     &      WTNUP, NGasLoss
          REAL, DIMENSION(0:NL,3), INTENT(IN), OPTIONAL :: LITE, SOM1E
        END SUBROUTINE
      END INTERFACE
      END MODULE Interface_SoilNBalSum

!=======================================================================
