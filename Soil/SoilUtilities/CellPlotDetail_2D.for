!=======================================================================
C  CellPlotDetail_2D, Subroutine
C 
C  Purpose: Provide daily plotting info for cell soil water and N data.
C
C  REVISION   HISTORY
C  03/04/2005 CHP wrote based on SoilNBal
!  07/25/2024 CHP re-wrote for Meng Zhang's spreadsheet animation.
!=======================================================================

      SUBROUTINE CellPlotDetail_2D (CONTROL, ISWITCH, CELLS, SOILPROP)

!     ------------------------------------------------------------------
      USE Cells_2D
      IMPLICIT NONE
      EXTERNAL YR_DOY, INCDAT, GETLUN, HEADER
      SAVE
!     ------------------------------------------------------------------

      CHARACTER*1  IDETN, IDETL
      INTEGER DAS, DYNAMIC, INCDAT, YRDOY
      INTEGER YR, DOY
!     ------------------------------------------------------------------
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (SoilType)    SOILPROP

!     ------------------------------------------------------------------
!     2D cell detail output file
      CHARACTER*22 CellOut

      INTEGER Clunn, row, col
      INTEGER, DIMENSION(MaxRows,MaxCols) :: Cell_Type

      REAL CelNtot, CelNConc, CelNUptake
      REAL CelNO3Conc, CelNH4Conc, CelUreaConc
      REAL BEDWD, ConvFactor
      REAL, DIMENSION(MaxRows,MaxCols) :: BedFrac, ColFrac

      TYPE (CellType) CELLS(MaxRows,MaxCols), CellDetail

!     ------------------------------------------------------------------
      IDETL   = ISWITCH % IDETL
      IDETN   = ISWITCH % IDETN
      IF (IDETL  == 'N' .OR. 
     &    IDETL  == '0' .OR.    !zero
     &    IDETN  == 'N') RETURN

      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY
      DAS     = CONTROL % DAS

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization phase
!***********************************************************************
      IF (DYNAMIC == SEASINIT) THEN
!     ------------------------------------------------------------------

!     Separate file names for each run
      IF (CONTROL%RUN < 10) THEN
        write (CellOut,'("CellPlotDetail_", I1,".OUT")') CONTROL%RUN
      ELSE IF (CONTROL%RUN < 100) THEN
        write(CellOut,'("CellPlotDetail_", I2, ".OUT")') CONTROL%RUN
      ELSE
        write(CellOut,'("CellPlotDetail_", I3, ".OUT")') CONTROL%RUN
      END IF

      CALL GETLUN(CellOut, CLunn)
      OPEN (UNIT = CLunn, FILE = CellOut, STATUS = 'REPLACE')
      WRITE(CLunn,'("*Daily Cell soil water and N output file")') 
      CALL HEADER(SEASINIT, CLunn, CONTROL % RUN)
      WRITE (CLunn,1130)
 1130 FORMAT(
     &'@YEAR DOY   DAS ROW COL',
     &'    TotalN      SNO3      SNH4      UREA',
     &'     NConc   NO3conc   NH4conc  UREAconc',
     &'     NFERT    NMiner   NUPtake      NGHG',
     &'    NFluxR    NFluxL    NFluxD    NFluxU',
     &'       RLV',
     &'       SWV        ES        EP   DripIrr      RAIN',
     &'    WFluxR    WFluxL    WFluxD    WFluxU')

      ENDIF

!***********************************************************************
!     Daily output
      SELECT CASE (DYNAMIC)
      CASE (SEASINIT)
        CALL YR_DOY(INCDAT(YRDOY,-1), YR, DOY)
        DAS = 0
      CASE (OUTPUT)
        CALL YR_DOY(YRDOY, YR, DOY)
      CASE DEFAULT; RETURN
      END SELECT

!***********************************************************************
!     Soil detail output
      Cell_Type = CELLS % Struc % Cell_Type
      BEDWD   = BedDimension % BEDWD
      ColFrac = BedDimension % ColFrac
      BedFrac = BedDimension % BedFrac

      DO row = 1, NRowsTot
        DO col = 1, NColsTot
          CellDetail = Cells(row,col)

          IF (CellDetail%STRUC%Cell_Type > 5 .OR. 
     &        CellDetail%STRUC%Cell_Type < 3) CYCLE

!         Cell N variables
          CelNtot =
     &      CellDetail % state % SNO3 +
     &      CellDetail % state % SNH4 +
     &      CellDetail % state % UREA
          
!         Cell conversion from kg/ha to ppm
          ConvFactor = SOILPROP % kg2ppm(row) / ColFrac(row,col) * 2.0
          
          CelNConc = CelNtot * ConvFactor
          CelNO3Conc = CellDetail % State % SNO3 * ConvFactor
          CelNH4Conc = CellDetail % State % SNH4 * ConvFactor
          CelUreaConc= CellDetail % State % UREA * ConvFactor

          CelNUptake = 
     &      CellDetail % rate % NO3Uptake + 
     &      CellDetail % rate % NH4Uptake

          WRITE (CLunn,1325) 
     &      YR, DOY, DAS, row, col, 

!           N Variables
     &      CelNtot, 
     &      CellDetail % State % SNO3, 
     &      CellDetail % State % SNH4, 
     &      CellDetail % State % Urea,
     &      CelNConc, 
     &      CelNO3conc, 
     &      CelNH4conc, 
     &      CelUREAconc,
     &      CellDetail % rate % CellFert, 
     &      CellDetail % rate % NMINER,
     &      CelNUptake,
     &      CellDetail % rate % GHG,
     &      CellDetail % rate % NFlux_R,
     &      CellDetail % rate % NFlux_L,
     &      CellDetail % rate % NFlux_D,
     &      CellDetail % rate % NFlux_U,

!           Root variables
     &      CellDetail % state % RLV, 

!           Water variables
     &      CellDetail % state % SWV,
     &      CellDetail % rate % ES_RATE,
     &      CellDetail % rate % EP_RATE,
     &      CellDetail % rate % DripIrr,
     &      CellDetail % rate % CellInf,
     &      CellDetail % rate % SWFlux_R, 
     &      CellDetail % rate % SWFlux_L, 
     &      CellDetail % rate % SWFlux_D, 
     &      CellDetail % rate % SWFlux_U

        enddo
      enddo

 1325     FORMAT(I4,2X,I3.3,1X,I5,1X,I3,1X,I3,40F10.5)

      IF (DYNAMIC .EQ. SEASEND) CLOSE (UNIT = Clunn)

      RETURN
      END SUBROUTINE CellPlotDetail_2D

!=======================================================================
! CellPlotDetail_2D VARIABLE DEFINITIONS:
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
! END SUBROUTINE CellPlotDetail_2D
!=======================================================================

