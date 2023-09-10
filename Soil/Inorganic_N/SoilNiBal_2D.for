!=======================================================================
C  SoilNiBal_2D, Subroutine
C 
C  Purpose: Provides seasonal inorganic soil N balance.  
C     Based on SoilNBal.for.
C
C  REVISION   HISTORY
C  03/04/2005 CHP wrote based on SoilNBal
!=======================================================================

      SUBROUTINE SoilNiBal_2D (CONTROL, ISWITCH, 
     &    FERTDATA, TNH4, TNO3, TUREA,   
     &    Cells, DENITRIF, IMM, MNR) 

!     ------------------------------------------------------------------
      USE Cells_2D
      USE GHG_mod
      USE FertType_mod
      USE Interface_SoilNBalSum
      IMPLICIT NONE
      EXTERNAL YR_DOY, INCDAT, GETLUN, HEADER
      SAVE
!     ------------------------------------------------------------------
      LOGICAL FEXIST

      CHARACTER*1  IDETN, IDETL, ISWNIT

      INTEGER DAS, DYNAMIC, INCDAT, YRDOY
!     INTEGER YRSIM, RUN
      INTEGER YR, DOY

      REAL TNO3, TNH4, TUREA
!!      REAL ALGFIX, ALGFIXI, AMTFER, TALLN, TALLNI, CLeach, TNH4, TNH4I,
!     &  TNO3, TNO3I,  TUREA, TUREAI, WTNUP, CNTILEDR   !HJ added
!      REAL TOTAML, CUMFNRO !, TOTFLOODN, TOTFLOODNI
!      REAL STATEN, BALANCE
!      REAL LCHTODAY, NTILEDRTODAY, IMMOBTODAY, MINERTODAY !HJ
!      REAL WTNUPTODAY, AMLTODAY, FNROTODAY, AMTFERTODAY
!      REAL N2Otoday, N2today, NOtoday
!      REAL CLeachY, TNTILEDRY, WTNUPY, CIMMOBY, CMINERY !HJ
!      REAL TOTAMLY, CUMFNROY, AMTFERY
!      REAL TOTSTATE, TOTADD, TOTSUB, DAYBAL, TOTSTATY, CUMBAL
!      REAL CIMMOBN, CMINERN, NGasLoss !, TNGSOILI
!
!      REAL N2OY, N2Y, NOY
!      REAL UnreleasedN  !Slow release fertilizer

!     ------------------------------------------------------------------
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (FertType)    FertData
!     TYPE (N2O_type)    N2O_DATA

!     ------------------------------------------------------------------
!     2D cell detail
      CHARACTER*20 SNiCellBAL
      CHARACTER*18, PARAMETER ::  SNiBAL2D = 'SoilNiBal2D.OUT'

      INTEGER Clunn, Clunn1D2D
      INTEGER detailRow, detailCol, PTFLG, MULTI
      INTEGER L, J
      INTEGER, DIMENSION(MaxRows,MaxCols) :: Cell_Type   

      REAL CelNtot, CelWD, CelHT, CelNBal, CelMNR, CelIMM
      REAL CelNtotI, CumNH4U, CumNO3U, FertAdd
      REAL CelTotAdd, CelTotRemove, FertFactor, BEDWD
      REAL TNO3_2D, TNH4_2D, TUREA_2D, Ntot, NtotI
      REAL, DIMENSION(0:NL,NELEM) :: IMM, MNR
      REAL, DIMENSION(MaxRows,MaxCols) :: YCelNtot,NO3UY,NH4UY,CCelNBal
      REAL, DIMENSION(MaxRows,MaxCols) :: ColFrac, DENITRIF

      TYPE (CellType) CELLS(MaxRows,MaxCols), CellDetail

!     ------------------------------------------------------------------
      IDETL   = ISWITCH % IDETL
      IDETN   = ISWITCH % IDETN
      ISWNIT  = ISWITCH % ISWNIT
      IF (IDETL  == 'N' .OR. 
     &    IDETL  == '0' .OR.    !zero
     &    IDETN  == 'N' .OR. 
     &    ISWNIT == 'N') RETURN

      ColFrac = BedDimension % ColFrac
      
!***********************************************************************
!***********************************************************************
!     Seasonal Initialization phase
!***********************************************************************
      IF (DYNAMIC == SEASINIT) THEN
!     ------------------------------------------------------------------
!!     Initialize output file
!      CALL GETLUN(SNiBAL, LUNSNC)
!      INQUIRE (FILE = SNiBAL, EXIST = FEXIST)
!      IF (FEXIST) THEN
!        OPEN (UNIT = LUNSNC, FILE = SNiBAL, STATUS = 'OLD',
!     &    POSITION = 'APPEND')
!        WRITE(LUNSNC,'(/,"!",79("="))') 
!      ELSE
!        OPEN (UNIT = LUNSNC, FILE = SNiBAL, STATUS = 'NEW')
!        WRITE(LUNSNC,'("*SOIL INORGANIC N BALANCE")')
!      ENDIF

!      CALL HEADER(SEASINIT, LUNSNC, RUN)

!!     Initial value for extractable N summed across all soil layers.
!      TNO3I  = TNO3
!      TNH4I  = TNH4
!      TUREAI = TUREA
!!     TNGSOILI = N2O_DATA % TNGSoil
!
!!     Sum the initial value of all abiotic N pools (soil, air)
!      TALLNI = TNO3I + TNH4I + TUREA  ! + TNGSOILI
!
!!     TOTFLOODNI = TOTFLOODN
!      ALGFIXI = ALGFIX
!
!!     If detailed printout requested, print daily soil N balance
!      IF (INDEX('AD',IDETL) > 0) THEN
!!       Cumulative values (yesterday)
!!       Save today's cumulative values for use tomorrow
!        AMTFERY  = 0.0
!        CMINERY  = 0.0
!        CIMMOBY  = 0.0
!        CLeachY  = 0.0
!        TNTILEDRY = 0.0
!        WTNUPY   = 0.0
!        CUMFNROY = 0.0
!        TOTAMLY  = 0.0
!        N2OY     = 0.0
!        N2Y      = 0.0
!        NOY      = 0.0
!
!        CUMBAL   = 0.0
!        DAYBAL = 0.0
        CALL YR_DOY(INCDAT(YRDOY,-1), YR, DOY)
!!       HJ added RLTD for N loss to tile        
!        WRITE(LUNSNC,10)
!   10     FORMAT('!',15X,
!     &   '|------------------- STATE VARIABLES -------------------|',
!     &   '---- ADDED ---|----------------------------- REMOVED ',
!     &   'TODAY --------------------------|    DAILY     CUMUL',/,
!     &   '@YEAR DOY  DAS     NO3     NH4   TUREA   TNGAS  FLOODN  ALG',
!     &   'FIX  UNFERT   AFERT  AMINER  RIMMOB    RLCH    RLTD    RNUP',
!     &   '    RNRO    RAML   N2OED    N2ED    NOED      DBAL      CBAL')
!        WRITE (LUNSNC,50) YR, DOY, 0, 
!!    &    TNO3, TNH4, TUREA, N2O_DATA % TNGSoil, TOTFLOODN, ALGFIX, 0.0,
!     &    TNO3, TNH4, TUREA, 0.0,                0.0,       0.0,    0.0,
!     &    0.0, 0.0, 
!     &    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
!      ENDIF

!!     TOTSTATY = TNO3I + TNH4I + TUREAI + ALGFIXI + TOTFLOODNI 
!      TOTSTATY = TNO3I + TNH4I + TUREAI + ALGFIXI + 0.0 
!!    &            + N2O_DATA % TNGSoil
!!     FLOODNY  = TOTFLOODNI
!
!!     CALL SoilNBalSum (CONTROL, N_inorganic=TOTSTATY)

!=========================================================================
!     Soil detail output
      CelNtotI = CelNtot
      CumNH4U = 0.
      CumNO3U = 0.
      NO3UY = 0
      NH4UY = 0

      detailRow = Cell_detail%Row
      detailCol = Cell_detail%Col
      PTFLG     = Cell_detail%NPTFLG
      MULTI     = Cell_detail%MULTI
      CellDetail = Cells(detailRow, detailCol)
      CelWD =  CellDetail % STRUC % WIDTH
      CelHT  = CellDetail % STRUC % THICK

      Cell_Type = CELLS % Struc % CellType
      BEDWD   = BedDimension % BEDWD

!     For Cell Detail N
      CCelNBal = 0.
      Ntot = 0
      IF (CONTROL%RUN < 10) THEN
        write (SNiCellBAL,'("CellDetailN_", I1,".OUT")')CONTROL%RUN
      ELSE IF (CONTROL%RUN < 100) THEN
        write(SNiCellBAL,'("CellDetailN_", I2, ".OUT")')CONTROL%RUN
      ELSE
        write(SNiCellBAL,'("CellDetailN_", I3, ".OUT")')CONTROL%RUN
      END IF

      CALL GETLUN(SNiCellBAL, CLunn)
      OPEN (UNIT = CLunn, FILE = SNiCellBAL, STATUS = 'REPLACE')
      WRITE(CLunn,'("*Nitrogen balance by cell")') !(",I2,",",I2,")")')
!    &    Cell_detail%row, Cell_detail%col
      CALL HEADER(SEASINIT, CLunn, CONTROL % RUN)
      WRITE (CLunn,1130)
 1130 FORMAT('!',26X,'STATE   ',
     & '------------------------ ADDED TODAY --------------------   ',
     & '--------------------- REMOVED TODAY ------------------------',
     & '-----------------  --- DAILY BAL ----',/,
     & '@YEAR DOY   DAS ROW COL',
     & '   TotalN     AFERT    CelNom',  
     & '    NFluxR    NFluxL    NFluxD    NFluxU',
     & '    NFluxR    NFluxL    NFluxD    NFluxU',
     & '  NH4UpTak  NO3UpTak       NOX    CelIMM  CellNBal   CumNBal') 

      DO L = 1, NRowsTot
        DO J = 1, NColsTot
          CellDetail = Cells(L, J)
          IF (CellDetail%STRUC%CellType > 5 .OR. 
     &        CellDetail%STRUC%CellType < 3) CYCLE

          CelNtot = (
     &      CellDetail % state % SNO3 +
     &      CellDetail % state % SNH4 +
     &      CellDetail % state % UREA )
!           if ((L .EQ. detailRow .AND. J .EQ. detailCol) .OR.
!     &        (L .LE. detailRow + MULTI .AND. L .GE. detailRow - MULTI
!     &         .AND. PTFLG .EQ. 1) .OR.
!     &        (J .LE. detailCol + MULTI .AND. J .GE. detailCol - MULTI
!     &         .AND. PTFLG .EQ. 2) .OR.
!     &        PTFLG .EQ. 3 ) THEN
          WRITE (CLunn,1325) YR, DOY, 0, L,J, 
     &        CelNtot,
    ! ADDED
     &          0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    ! REMOVED
     &          0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    ! BALANCE
     &          0.0, 0.0

          YCelNtot(L,J) = CelNtot
          Ntot = Ntot + CelNtot * ColFrac(L, J)
        enddo
      enddo
      NtotI = Ntot

!----------------------------------------------------------------------------
!     For Cell Detail N 1D-2D compare
      CCelNBal = 0.
        
!     Initialize output file
      CALL GETLUN(SNiBAL2D, Clunn1D2D)
      INQUIRE (FILE = SNiBAL2D, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = Clunn1D2D, FILE = SNiBAL2D, STATUS = 'OLD',
     &  POSITION = 'APPEND')
        WRITE(Clunn1D2D,'(/,"!",79("="))') 
      ELSE
        OPEN (UNIT = Clunn1D2D, FILE = SNiBAL2D, STATUS = 'NEW')
       WRITE(Clunn1D2D,'("Nitrogen 1D vs 2D comparison")')
      ENDIF

      CALL HEADER(SEASINIT, Clunn1D2D, CONTROL % RUN)
      WRITE (Clunn1D2D, 1131)
 1131 FORMAT('@YEAR DOY   DAS', 
     & '      NO3   NO32D     NH4   NH42D   TUREA TUREA2D')

      TNO3_2D = 0.0
      TNH4_2D = 0.0
      TUREA_2D = 0.0
      DO L = 1, NRowsTot
        DO J = 1, NColsTot
          CellDetail = Cells(L, J)
          IF (CellDetail%STRUC%CellType > 5 .OR. 
     &        CellDetail%STRUC%CellType < 3) CYCLE
          TNO3_2D = TNO3_2D + CellDetail % state % SNO3 * ColFrac(L,J)
          TNH4_2D = TNH4_2D + CellDetail % state % SNH4 * ColFrac(L,J)
          TUREA_2D= TUREA_2D+ CellDetail % state % UREA * ColFrac(L,J)
        enddo
      enddo
      WRITE (Clunn1D2D,1326) YR, DOY, 0,
     &          TNO3, TNO3_2D, TNH4, TNH4_2D, TUREA, TUREA_2D

!***********************************************************************
!***********************************************************************
!     Daily output
!***********************************************************************
      ELSEIF (DYNAMIC == OUTPUT) THEN
!     ------------------------------------------------------------------
!     Daily output only when detail switch is on
      IF (INDEX('AD',IDETL) .LE. 0) RETURN

!!     Compute daily rates from cumulative values
!!     Additions:
!      AMTFERTODAY = AMTFER  - AMTFERY
!      MINERTODAY  = CMINERN - CMINERY
!
!!     Subtractions:
!      IMMOBTODAY = CIMMOBN - CIMMOBY
!      LCHTODAY   = CLeach - CLeachY
!      NTILEDRTODAY = CNTILEDR - TNTILEDRY             !HJ added
!      WTNUPTODAY = (WTNUP - WTNUPY) * 10.  
!      FNROTODAY  = CUMFNRO - CUMFNROY
!      AMLTODAY   = TOTAML - TOTAMLY
!      N2Otoday   = 0.0  !N2O_data % CN2O_emitted - N2OY
!      N2today    = 0.0  !N2O_data % CN2_emitted  - N2Y 
!      NOtoday    = 0.0  !N2O_data % CNO_emitted  - NOY
!
!!     FLOODNTODAY = TOTFLOODN - FLOODNY
!
!!     For slow release fertilizers, keep track of unreleased N
!      UnreleasedN = 0.0
!      IF (NActiveSR .GT. 0) THEN
!        DO I = 1, NSlowRelN
!          IF (SlowRelN(I) % ACTIVE) THEN
!            UnreleasedN = UnreleasedN + 
!     &         SlowRelN(I) % N0 - SlowRelN(I) % CumRelToday
!          ENDIF
!        ENDDO
!      ENDIF
!
!!     TOTSTATE = TNO3 + TNH4 + TUREA + ALGFIX + TOTFLOODN 
!      TOTSTATE = TNO3 + TNH4 + TUREA + 0.0    + 0.0
!!    &         + N2O_DATA % TNGSoil + UnreleasedN
!     &         + 0.0                + UnreleasedN
!      TOTADD   = AMTFERTODAY + MINERTODAY
!      TOTSUB   = IMMOBTODAY + LCHTODAY + WTNUPTODAY + FNROTODAY 
!     &         + AMLTODAY + N2Otoday + N2today + NOtoday
!     &         + NTILEDRTODAY !HJ added
!
!      DAYBAL = TOTSTATE - TOTSTATY - TOTADD + TOTSUB
!      CUMBAL   = CUMBAL + DAYBAL
!
!      CALL YR_DOY(YRDOY, YR, DOY)
!!     Write daily output to SoilNiBal.OUT.
!      WRITE (LUNSNC,50) YR, DOY, DAS, 
!!    &  TNO3, TNH4, TUREA, N2O_DATA % TNGSoil, TOTFLOODN, ALGFIX, 
!     &  TNO3, TNH4, TUREA, 0.0,                0.0,       0.0, 
!     &  UnreleasedN,
!     &  AMTFERTODAY, MINERTODAY, 
!     &  IMMOBTODAY, LCHTODAY, NTILEDRTODAY, WTNUPTODAY, !HJ
!     &  FNROTODAY, AMLTODAY, N2Otoday, N2today, NOtoday,
!     &  DAYBAL, CUMBAL
!   50 FORMAT(I5, I4.3, I5, 7F8.3, F8.1, 10F8.4, 2F10.4)
!
!!     Save today's cumulative values for use tomorrow
!      AMTFERY  = AMTFER
!      CMINERY  = CMINERN
!      CIMMOBY  = CIMMOBN
!      CLeachY  = CLeach
!      TNTILEDRY = CNTILEDR            !HJ added
!      WTNUPY   = WTNUP
!      CUMFNROY = CUMFNRO
!      TOTAMLY  = TOTAML
!      N2OY     = 0.0  !N2O_data % CN2O_emitted
!      N2Y      = 0.0  !N2O_data % CN2_emitted 
!      NOY      = 0.0  !N2O_data % CNO_emitted 
!      TOTSTATY = TOTSTATE

!=========================================================================
!     Soil detail output
      Ntot = 0

      DO L = 1, NRowsTot
        DO J = 1, NColsTot
          CellDetail = Cells(L, J)
          IF (CellDetail%STRUC%CellType > 5 .OR. 
     &        CellDetail%STRUC%CellType < 3) CYCLE
      
          CelNtot =(
     &      CellDetail % state % SNO3 +
     &      CellDetail % state % SNH4 +
     &      CellDetail % state % UREA )

          SELECT CASE (TRIM(FERTDATA % AppType))
          CASE ('BANDED','POINT')
!           Banded or point application goes to cell L,1
            IF (J .EQ. 1) THEN
              FertAdd = (FERTDATA % ADDSNO3(L) +
     &          FERTDATA % ADDSNH4(L) + FERTDATA % ADDUREA(L))
     &          / ColFrac(L,1)
            ELSE
              FertAdd = 0.0
            END IF
           
          CASE ('DRIP')
!           Drip fertigation goes to cell DripRow,DripCol
            if (L == BedDimension%DripRow(FERTDATA%DrpRefIdx) .AND.
     &          J == BedDimension%DripCol(FERTDATA%DrpRefIdx)) then
              FertAdd = (FERTDATA % ADDSNO3(L) +
     &          FERTDATA % ADDSNH4(L) + FERTDATA % ADDUREA(L))
     &          / ColFrac(L,J)
            else
              FertAdd = 0.0
            endif

          CASE DEFAULT
            SELECT CASE (Cell_Type(L,J))
!           Within the bed, fertilizer is concentrated in bed cells
            CASE (3); 
              FertFactor = BedDimension%ROWSPC_cm /BEDWD
            CASE (4,5); FertFactor = 1.0
            CASE DEFAULT; CYCLE
            END SELECT

            FertAdd = FERTDATA % ADDSNO3(L) * FertFactor +
     &        FERTDATA % ADDSNH4(L) * FertFactor +
     &        FERTDATA % ADDUREA(L) * FertFactor
          END SELECT

          IF (Cell_Type(L, J) .NE. 0) THEN 
            CelMNR = MNR(L,N) 
            CelIMM = IMM(L,N) 
          else
            CelMNR = 0.0  
            CelIMM = 0.0
          Endif

          CelTotRemove =
     &      Cell_Ndetail % NFlux_L_out(L,J) +
     &      Cell_Ndetail % NFlux_D_out(L,J) +
     &      Cell_Ndetail % NFlux_U_out(L,J) +
     &      Cell_Ndetail % NFlux_R_out(L,J) +
     &      CelIMM +
     &      DENITRIF(L, J) +
     &      NO3UY(L, J) + 
     &      NH4UY(L, J)
          CelTotAdd =
     &      Cell_Ndetail % NFlux_L_in(L,J) +
     &      Cell_Ndetail % NFlux_D_in(L,J) +
     &      Cell_Ndetail % NFlux_U_in(L,J) +
     &      Cell_Ndetail % NFlux_R_in(L,J) +
     &      FertAdd + CelMNR
          CelNBal = CelNtot - YCelNtot(L,J) - CelTotAdd + CelTotRemove
          CCelNBal(L, J) = CCelNBal(L, J) + CelNBal

!         if (((CelNBal > 1.E-5 .OR. CelNBal < -1.E-5) .AND. 
!     &            PTFLG.EQ.4) .OR.
!     &          (L .EQ. detailRow .AND. J .EQ. detailCol .AND. 
!     &            PTFLG.EQ.0) .OR.
!     &          (L .LE. detailRow + MULTI .AND. 
!     &           L .GE. detailRow - MULTI .AND.
!     &           PTFLG .EQ. 1) .OR.
!     &          (J .LE. detailCol + MULTI .AND. 
!     &           J .GE. detailCol - MULTI .AND.
!     &           PTFLG .EQ. 2) .OR.
!     &          PTFLG .EQ. 3 ) THEN

          WRITE (CLunn,1325) 
     &      YR, DOY, DAS, L, J, CelNtot, FertAdd, CelMNR,
     &      Cell_Ndetail % NFlux_R_in(L, J),
     &      Cell_Ndetail % NFlux_L_in(L, J),
     &      Cell_Ndetail% NFlux_D_in(L, J),
     &      Cell_Ndetail%NFlux_U_in(L, J),
     &      Cell_Ndetail % NFlux_R_out(L, J),
     &      Cell_Ndetail % NFlux_L_out(L, J),
     &      Cell_Ndetail% NFlux_D_out(L, J),
     &      Cell_Ndetail%NFlux_U_out(L, J),
     &      NH4UY(L, J),
     &      NO3UY(L, J),
     &      DENITRIF(L, J), CelIMM,
     &      CelNBal, CCelNBal(L, J)
!         endif

 1325     FORMAT(I4,2X,I3.3,1X,I5,1X,I3,1X,I3,F9.2,31(1X,F9.5))

          YCelNtot(L,J) = CelNtot
          Ntot = Ntot + CelNtot * ColFrac(L, J)
          CumNH4U = CumNH4U + CellDetail % Rate % NH4Uptake*ColFrac(L,J)
          CumNO3U = CumNO3U + CellDetail % Rate % NO3Uptake*ColFrac(L,J)
          NO3UY(L,J) = CellDetail % Rate % NO3Uptake
          NH4UY(L,J) = CellDetail % Rate % NH4Uptake
        enddo
      enddo
      
      TNO3_2D = 0.0
      TNH4_2D = 0.0
      TUREA_2D = 0.0
      DO L = 1, NRowsTot
        DO J = 1, NColsTot
          CellDetail = Cells(L, J)
          IF (CellDetail%STRUC%CellType > 5 .OR. 
     &         CellDetail%STRUC%CellType < 3) CYCLE
          TNO3_2D = TNO3_2D + CellDetail % state % SNO3 * ColFrac(L, J)
          TNH4_2D = TNH4_2D + CellDetail % state % SNH4 * ColFrac(L, J)
          TUREA_2D= TUREA_2D+ CellDetail % state % UREA * ColFrac(L, J)
        enddo
      enddo

      WRITE (Clunn1D2D,1326) YR, DOY, 0,
     &  TNO3, TNO3_2D, TNH4, TNH4_2D, TUREA, TUREA_2D

 1326 FORMAT(I4,2X,I3.3,1X,I5,1X,6(1X,F7.3),31(1X,F9.5))

!***********************************************************************
!***********************************************************************
!     Seasonal Output
!***********************************************************************
      ELSEIF (DYNAMIC == SEASEND) THEN
!     ------------------------------------------------------------------
!!     N balance will be off by the amount of N uptake on the last day 
!!       of season because this amount has been added to the N uptake by
!!       plant, but has not been subtracted from soil.  This is because
!!       soil processes are computed before plant processes.
!!     May want to subtract this amount from balance?
!
!        CALL YR_DOY(YRSIM, YRI, DOYI)
!        CALL YR_DOY(YRDOY, YR, DOY)
!!       Add the fertilizer N to the initial N pool. Also add the N from
!!       organic residues applied during the growth period and sum this
!!       with the initial TALLNI to make the balance fit with the final
!!       TALLN. SEEDNI is not needed, because for the plant the NBAL
!!       only deals with N uptake from the soil.
!        TALLNI  = TALLNI + AMTFER + CMINERN !Initial + add
!
!!       For slow release fertilizers, keep track of unreleased N
!        UnreleasedN = 0.0
!        IF (NActiveSR .GT. 0) THEN
!          DO I = 1, NSlowRelN
!            IF (SlowRelN(I) % ACTIVE) THEN
!              UnreleasedN = UnreleasedN + 
!     &           SlowRelN(I) % N0 - SlowRelN(I) % CumRelToday
!            ENDIF
!          ENDDO
!        ENDIF
!
!!       Sum state N at end of season
!!       STATEN = TNO3 + TNH4 + TUREA + N2O_DATA % TNGSoil + UnreleasedN
!        STATEN = TNO3 + TNH4 + TUREA +  UnreleasedN
!
!!       Sum the initial value of all abiotic N pools (soil, air,
!!       fertilizer), SOM and N uptake (WTNUP multiplied by 10 to
!!       convert from g/m2 to kg/ha). Deduct the N in senesced material
!!       from the N removed by the plant, because senesced material has
!!       been returned to the soil.
!        TALLN = STATEN +                          !State end of day
!!    &          CLeach + CNOX + WTNUP * 10. +       !Losses
!!    &          TOTAML + CIMMOBN           !Losses
!!               HJ adeed CNTILEDR
!     &          CIMMOBN + CLeach + CNTILEDR + WTNUP * 10. + TOTAML !+       
!!     &          N2O_data % CN2O_emitted + N2O_data % CN2_emitted + 
!!     &          N2O_data % CNO_emitted     
!
!!       Write output to NBAL.OUT.
!        WRITE (LUNSNC,100) YRI, DOYI, YR, DOY
!
!        WRITE (LUNSNC, 200) TNO3I, TNO3, TNH4I, TNH4, TUREAI, TUREA
!!       WRITE (LUNSNC, 210) TNGSOILI, N2O_DATA % TNGSoil,
!        WRITE (LUNSNC, 210) 0.0, 0.0, !Temporarily disable N gas data
!     &        UnreleasedN
!
!!        IF (NBUND .GT. 0) THEN
!!          WRITE(LUNSNC,300) TOTFLOODNI, TOTFLOODN, ALGFIXI, ALGFIX
!!          TALLN = TALLN + TOTFLOODN + ALGFIX 
!!          STATEN = STATEN + TOTFLOODN + ALGFIX
!!        ENDIF
!
!!       HJ adeed CNTILEDR
!        WRITE (LUNSNC,600) AMTFER, CMINERN, 
!     &    CIMMOBN, CLeach, CNTILEDR, WTNUP * 10., TOTAML, 
!!    &    N2O_data % CN2O_emitted, N2O_data % CN2_emitted,
!!    &    N2O_data % CNO_emitted 
!     &    0.0, 0.0, 0.0 
!
!        IF (NBUND .GT. 0) THEN
!          TALLN = TALLN + CUMFNRO     !Factor in runoff over bund
!          WRITE(LUNSNC,700) CUMFNRO
!        ENDIF
!
!        WRITE(LUNSNC,800) TALLNI, TALLN
!        BALANCE = TALLN - TALLNI
!        WRITE(LUNSNC,900) BALANCE
!
!        CLOSE (UNIT = LUNSNC)
!
!100   FORMAT (//,'!SOIL INORGANIC N BALANCE',T51,'Initial',T73,'Final',
!     &  /,'!',T50,'Year/Doy', T70, 'Year/Doy', 
!     &  /,'!',T49, I5,'/',I3.3, T69,I5,'/',I3.3,
!     &  /,'!',T49,'(kg[N]/ha)          (kg[N]/ha)',
!     &  /,'! Soil N state variables:')
!
!200     FORMAT (
!     &       '!', 3X, 'Soil NO3',             T48, F10.2, T68, F10.2,
!     &     /,'!', 3X, 'Soil NH4',             T48, F10.2, T68, F10.2,
!     &     /,'!', 3X, 'Soil Urea',            T48, F10.2, T68, F10.2)
!
!210     FORMAT (
!     &       '!', 3X, 'Soil N gases',         T48, F10.2, T68, F10.2,
!     &     /,'!', 3x, 'Unreleased fert N',                T68, F10.2)
!
!300     FORMAT (
!     &       '!', 3X, 'Flood water N',        T48, F10.2, T68, F10.2,
!     &     /,'!', 3X, 'Algae N',              T48, F10.2, T68, F10.2)
!
!600     FORMAT (
!     &     /,'! N additions:'
!     &     /,'!', 3X, 'Fertilizer N',         T48, F10.2,
!     &     /,'!', 3X, 'Mineralized N',        T48, F10.2,
!	 
!!          HJ added N loss to tile drainage
!     &    //,'! N subtractions:',
!     &     /,'!', 3X, 'N immobilized',                    T68, F10.2,
!     &     /,'!', 3X, 'N leached',                        T68, F10.2,
!     &     /,'!', 3X, 'N loss to tile drainage',          T68, F10.2,
!     &     /,'!', 3X, 'N Uptake From Soil',               T68, F10.2,
!     &     /,'!', 3X, 'NH3 loss',                         T68, F10.2,
!     &     /,'!', 3X, 'N2O loss',                         T68, F10.2,
!     &     /,'!', 3X, 'N2 loss',                          T68, F10.2,
!     &     /,'!', 3X, 'NO loss',                          T68, F10.2)
!
!700     FORMAT ('!',3X,'N in runoff over bund',           T68, F10.2)
!
!800     FORMAT (/,'!',3X, 'Total N balance',     T48, F10.2, T68, F10.2)
!900     FORMAT ('!',3X,'Balance',                            T68, F10.3)
!
!      NGasLoss = TOTAML !+ N2O_data % CN2O_emitted +
!!    &    N2O_data % CN2_emitted + N2O_data % CNO_emitted
!
!!     HJ added CNTILEDR
!      CALL SoilNBalSum (CONTROL, 
!     &    AMTFER, Balance, 
!     &    CLeach=CLeach, CNTILEDR=CNTILEDR, N_inorganic=StateN, 
!     &    WTNUP=WTNUP*10., NGasLoss=NGasLoss, CUMFNRO=CUMFNRO)
     
      CLOSE (UNIT = Clunn1D2D)
      CLOSE (UNIT = Clunn)

!***********************************************************************
!***********************************************************************
!     End of DYNAMIC IF construct
!***********************************************************************
      END IF

      RETURN
      END SUBROUTINE SoilNiBal_2D

!=======================================================================
! SoilNiBal_2D VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! ALGFIX        N in algae (kg [N] / ha)
! AMTFER     Cumulative amount of N in fertilizer applications
! BD(L,j)   Bulk density, soil layer L (g [soil] / cm3 [soil])
! cellNtot  Nitrogen in soil cell (�g[N] / g[soil])
! CMINERN   Cumulative seasonal mineralization of N in soil profile (kg[N]/ha)
! CUMFNRO   Cumulative N lost in runoff over bund (kg [N] / ha)
! KG2PPM(L) Conversion factor to switch from kg [N] / ha to ug [N] / g 
!           KG2PPM(L) = 1.0/(BD*1.E-01*DLAYR(L))
! NFlux_D   Downward movement of nitrogen with the water flow. (kg [N] / ha / d)
! NFlux_U   upward movement of nitrogen with the water flow.
! NH4Uptake      in kg[N]/ha
! NO3Uptake      in kg[N]/ha
! SNO3(L,j) Total extractable nitrate N in soil layer L (kg [N] / ha) 
! TNOX      Season cumulative denitrification across the total soil profile adding to 
!                 the nitrous oxide (NOx) pool of the air (kg [N] / ha)  
! TNOXY     Yesterday's TNOX
! TOTAML         Cumulative ammonia volatilization (kg [N] / ha)
!-----------------------------------------------------------------------
! END SUBROUTINE SoilNiBal_2D
!=======================================================================

