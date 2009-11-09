!***********************************************************************
!  SoilNBalSum, Subroutine 
!
!  Purpose: Writes out a one-line seasonal soil N balance  
!
!  REVISION   HISTORY
!  07/11/2006 CHP Written
!***********************************************************************

      SUBROUTINE SoilNBalSum (CONTROL, 
     &    AMTFER, Balance, CUMIMMOB, CUMMINER, CUMRESN, 
     &    CumSenN, HARVRESN, LITE, SOM1E, TLCH, TLITN, 
     &    TNH4, TNO3, TNOX, TOTAML, TSOM1N, TSOM2N, TSOM3N, WTNUP)

!***********************************************************************

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      TYPE (ControlType), INTENT(IN) :: CONTROL
      REAL, INTENT(IN), OPTIONAL :: AMTFER, Balance, CUMIMMOB, 
     &    CUMMINER, CUMRESN, CumSenN, HARVRESN, TLCH, TLITN, 
     &    TNH4, TNO3, TNOX, TOTAML, TSOM1N, TSOM2N, TSOM3N, WTNUP
      REAL, DIMENSION(0:NL,3), INTENT(IN), OPTIONAL :: LITE, SOM1E

      CHARACTER(LEN=14), PARAMETER :: SNSUM = 'SolNBalSum.OUT'
      INTEGER COUNT, ERRNUM, LUNSNS
      LOGICAL FEXIST, FIRST
      REAL State(6), Add(4), Sub(4), Bal(2), Miner(2)

      DATA FIRST /.TRUE./
      DATA COUNT /0/

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
 5000   FORMAT(/,"!",T23,"|------------------- N State Variables ----",
     &    "---------------| |------------ N Additions ------------| |",
     &    "---------- N Subtractions -----------| |-Mineralization--|",
     &  /,"!",T85,"Harvest   Applied",T136,"Denit-             Ammonia",
     &    "    Miner-    Immob-  Seasonal",
     &  /,"!",T25,"Surface      SOM1      SOM2      SOM3    Litter    ",
     &    " Inorg   Residue   Residue  Fertiliz   Senesed   Leached   ",
     &    " rified    Uptake  Volatil.    alized    ilized   Balance",
     &  /,"@Run FILEX         TN      SN0D     S1NTD",
     &     "     S2NTD     S3NTD      LNTD      NIAD      HRNH",
     &     "     RESNC      NICM     SNNTC      NLCM      NDNC",
     &     "      NUCM      AMLS      NMNC      NIMC   SEASBAL")
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

!     Inorganic
      IF (PRESENT(TNO3) .AND. PRESENT(TNH4)) THEN
        State(6) = TNO3 + TNH4
        IF (PRESENT(Balance)) Bal(2) = Balance
      ENDIF
      IF (PRESENT(AMTFER)) Add(3) = AMTFER
      IF (PRESENT(TLCH))   Sub(1) = TLCH
      IF (PRESENT(TNOX))   Sub(2) = TNOX
      IF (PRESENT(WTNUP))  Sub(3) = WTNUP
      IF (PRESENT(WTNUP))  Sub(4) = TOTAML

!     Mineralization/immobilization
      IF (PRESENT(CUMMINER)) Miner(1) = CUMMINER
      IF (PRESENT(CUMIMMOB)) Miner(2) = CUMIMMOB

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization 
!***********************************************************************
      IF (CONTROL % DYNAMIC .EQ. SEASINIT) THEN
!     ------------------------------------------------------------------
      IF (CONTROL%RUN == 1) THEN
        COUNT = COUNT + 1
        IF (COUNT == 2) THEN
          WRITE(LUNSNS,'(I4,1X,A12," INIT",F9.2,6F10.2)') 
     &      CONTROL%RUN, CONTROL%FILEX, State
          COUNT = 0
          State = 0.
        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal Output 
!***********************************************************************
      ELSEIF (CONTROL % DYNAMIC .EQ. SEASEND) THEN
!     ------------------------------------------------------------------
      COUNT = COUNT + 1

      IF (COUNT == 2) THEN
        WRITE(LUNSNS,'(I4,1X,A12,I4,17F10.2)') 
     &    CONTROL%RUN, CONTROL%FILEX, CONTROL%TRTNUM, 
     &    State, Add, Sub, Miner, Bal(1)+Bal(2)
        COUNT = 0
        State = 0.
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
      INTERFACE
        SUBROUTINE SoilNBalSum (CONTROL, 
     &    AMTFER, Balance, CUMIMMOB, CUMMINER, CUMRESN, 
     &    CumSenN, HARVRESN, LITE, SOM1E, TLCH, TLITN, 
     &    TNH4, TNO3, TNOX, TOTAML, TSOM1N, TSOM2N, TSOM3N, WTNUP)
          USE ModuleDefs
          TYPE (ControlType), INTENT(IN) :: CONTROL
          REAL, INTENT(IN), OPTIONAL :: AMTFER, Balance, CUMIMMOB, 
     &        CUMMINER, CUMRESN, CumSenN, HARVRESN, TLCH, TLITN, 
     &        TNH4, TNO3, TNOX, TOTAML, TSOM1N, TSOM2N, TSOM3N, WTNUP
          REAL, DIMENSION(0:NL,3), INTENT(IN), OPTIONAL :: LITE, SOM1E
        END SUBROUTINE
      END INTERFACE
      END MODULE Interface_SoilNBalSum

!=======================================================================
