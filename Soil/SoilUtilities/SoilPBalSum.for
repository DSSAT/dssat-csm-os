!***********************************************************************
!  SoilPBalSum, Subroutine 
!
!  Purpose: Writes out a one-line seasonal soil P balance  
!
!  REVISION   HISTORY
!  07/14/2006 CHP Written
!***********************************************************************

      SUBROUTINE SoilPBalSum (CONTROL,
     &    AMTFER, Balance, CUMIMMOB, CUMMINER, CumPUptake, 
     &    CUMRESP, CumSenP, HARVRESP, LITE, SOM1E, 
     &    SPiActProf, SPiLabProf, SPiStaProf, TLITP, 
     &    TSOM1P, TSOM23P)

!     ------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER
      SAVE

      TYPE (ControlType), INTENT(IN) :: CONTROL
      REAL, INTENT(IN), OPTIONAL :: AMTFER, Balance, CUMIMMOB, 
     &    CUMMINER, CumPUptake, CUMRESP, CumSenP, HARVRESP, 
     &    SPiActProf, SPiLabProf, SPiStaProf, 
     &    TLITP, TSOM1P, TSOM23P
      REAL, DIMENSION(0:NL,3), INTENT(IN), OPTIONAL :: LITE, SOM1E

      CHARACTER(LEN=15), PARAMETER :: SPSUM = 'SoilPBalSum.OUT'
      INTEGER COUNT, ERRNUM, LUNSPS
      LOGICAL FEXIST, FIRST
      REAL State(5), Add(4), Sub(1), Bal(2), Miner(2)

      DATA FIRST /.TRUE./
      DATA COUNT /0/

      IF (FIRST) THEN
        FIRST = .FALSE.

!       Initialize output file
        CALL GETLUN(SPSUM, LUNSPS)
        INQUIRE (FILE = SPSUM, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = LUNSPS, FILE = SPSUM, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = LUNSPS, FILE = SPSUM, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
        ENDIF
        WRITE(LUNSPS,'(/,"*SOIL P BALANCE - CENTURY ROUTINES")')
        CALL HEADER(0, LUNSPS, CONTROL%RUN)

        WRITE(LUNSPS,5000) 
 5000   FORMAT(/,'!',21X,"|-------------- P State Variables -----",
     &    "----------| |------------ P Additions ------------|",
     &    "|Subtract| |-Mineralization--|",
     &  /,"!",T75,"Harvest   Applied",T126,"Miner-    Immob-  Seasonal",
     &  /,"!",T25,"Surface      SOM1     SOM23    Litter     Inorg   ",
     &    "Residue   Residue  Fertiliz   Senesed    Uptake    alized ",
     &    "   ilized   Balance",
     &  /,"@Run FILEX         TN      SP0D     S1PTD    S23PTD",
     &     "      LPTD      PIAD      HRPH     RESPC      PICM",
     &     "     SNPTC      PUPC      PMNC      PIMC   SEASBAL")
      ENDIF

!     Organic
      IF (PRESENT(LITE) .AND. PRESENT(SOM1E)) THEN
        State(1) = LITE(0,P) + SOM1E(0,P)
        IF (PRESENT(Balance)) Bal(1) = Balance
      ENDIF
      IF (PRESENT(TSOM1P)) State(2) = TSOM1P
      IF (PRESENT(TSOM23P))State(3) = TSOM23P
      IF (PRESENT(TLITP))  State(4) = TLITP

      IF (PRESENT(HARVRESP)) Add(1) = HARVRESP
      IF (PRESENT(CUMRESP))  Add(2) = CUMRESP
      IF (PRESENT(CumSenP))  Add(4) = CumSenP

!     Inorganic
      IF (PRESENT(SPiActProf)) THEN
        State(5) = SPiActProf + SPiLabProf + SPiStaProf
        IF (PRESENT(Balance)) Bal(2) = Balance
      ENDIF
      IF (PRESENT(AMTFER))      Add(3) = AMTFER
      IF (PRESENT(CumPUptake))  Sub(1) = CumPUptake

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
          WRITE(LUNSPS,'(I4,1X,A12," INIT",F9.3,5F10.3)') 
     &      CONTROL%RUN, CONTROL%FILEX, State
          COUNT = 0
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
        WRITE(LUNSPS,'(I4,1X,A12,I4,16F10.3)') 
     &    CONTROL%RUN, CONTROL%FILEX, CONTROL%TRTNUM, 
     &    State, Add, Sub, Miner, Bal(1)+Bal(2)
        COUNT = 0
      ENDIF

!***********************************************************************
!***********************************************************************
!     End of DYNAMIC IF construct
!***********************************************************************
      END IF

      RETURN
      END SUBROUTINE SoilPBalSum


!=======================================================================
      MODULE Interface_SoilPBalSum
!     Interface needed for optional arguments with SoilPBalSum
      INTERFACE
        SUBROUTINE SoilPBalSum (CONTROL, 
     &    AMTFER, Balance, CUMIMMOB, CUMMINER, CumPUptake, 
     &    CUMRESP, CumSenP, HARVRESP, LITE, SOM1E, 
     &    SPiActProf, SPiLabProf, SPiStaProf, TLITP, 
     &    TSOM1P, TSOM23P)
          USE ModuleDefs
          TYPE (ControlType), INTENT(IN) :: CONTROL
          REAL, INTENT(IN), OPTIONAL :: AMTFER, Balance, CUMIMMOB, 
     &        CUMMINER, CumPUptake, CUMRESP, CumSenP, HARVRESP, 
     &        SPiActProf, SPiLabProf, SPiStaProf, 
     &        TLITP, TSOM1P, TSOM23P
          REAL, DIMENSION(0:NL,3), INTENT(IN), OPTIONAL :: LITE, SOM1E
        END SUBROUTINE
      END INTERFACE
      END MODULE Interface_SoilPBalSum

!=======================================================================
