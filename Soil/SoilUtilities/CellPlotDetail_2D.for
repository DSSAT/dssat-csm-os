!=======================================================================
C  CellPlotDetail_2D, Subroutine
C  Purpose: Provide daily 2D plotting info by cell for soil water and N data.
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
!     2D cell detail output file name
      CHARACTER*22 CellOut

      INTEGER Clunn, row, col
      INTEGER, DIMENSION(MaxRows,MaxCols) :: Cell_Type

      REAL CelNtot, CelNConc, CelNUptake
      REAL CelNO3Conc, CelNH4Conc, CelUreaConc
      REAL Wtot, EPTot, ESTot
      REAL DeltaSWtot, DeltaNTot
      REAL ConvFactor
      REAL, DIMENSION(MaxRows,MaxCols) :: CelNtot_Y, Wtot_Y
      TYPE (CellType) CELLS(MaxRows,MaxCols), CellDetail

!     ------------------------------------------------------------------
      IDETL   = ISWITCH % IDETL
      IDETN   = ISWITCH % IDETN
      IF (IDETL  /= 'D' .OR. IDETN  == 'N') RETURN

      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY
      DAS     = CONTROL % DAS

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization phase
!***********************************************************************
      IF (DYNAMIC == SEASINIT) THEN
!     ------------------------------------------------------------------

!     Separate file names for each run for up to 999 simulations
      write(CellOut,'("CellPlotDetail_", I3.3, ".OUT")') CONTROL%RUN

      CALL GETLUN(CellOut, CLunn)
      OPEN (UNIT = CLunn, FILE = CellOut, STATUS = 'REPLACE')
      WRITE(CLunn,'("*Daily Cell soil water and N output file")') 
      CALL HEADER(SEASINIT, CLunn, CONTROL % RUN)
      WRITE (CLunn,1130)
 1130 FORMAT(
     &'@YEAR DOY   DAS ROW COL',
     &'    TotalN    DeltaN      SNO3      SNH4      UREA',
     &'     NConc   NO3conc   NH4conc  UREAconc',
     &'     NFERT    NMiner   NUPtake      NGHG',
     &'    NFluxR    NFluxL    NFluxD    NFluxU',
     &'       RLV',
     &'       SWV     SWTot   DeltaSW     ESTot     EPTot',
     &'        ES        EP   DripIrr      RAIN',
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

      DO row = 1, NRowsTot
        DO col = 1, NColsTot
          CellDetail = Cells(row,col)

          IF (CellDetail%STRUC%Cell_Type > 5 .OR. 
     &        CellDetail%STRUC%Cell_Type < 3) CYCLE

!         Cell N variables kg/ha
          CelNtot =
     &      CellDetail % state % SNO3 +
     &      CellDetail % state % SNH4 +
     &      CellDetail % state % UREA
          
!         Cell conversion from kg/ha to ppm
          ConvFactor = SOILPROP % kg2ppm(row) 
     &        / BedDimension % ColFrac(row,col) * 2.0
          
          CelNConc = CelNtot * ConvFactor
          CelNO3Conc = CellDetail % State % SNO3 * ConvFactor
          CelNH4Conc = CellDetail % State % SNH4 * ConvFactor
          CelUreaConc= CellDetail % State % UREA * ConvFactor

!         Plant N uptake kg/ha
          CelNUptake = 
     &      CellDetail % rate % NO3Uptake + 
     &      CellDetail % rate % NH4Uptake

!         Total water content 
          WTot = CellDetail % State % SWV * CellDetail % Struc %CellArea
!           cm3[water]         cm3[water]     cm3[soil]
!         -------------- =     ---------- * --------------
!         cm[row length]        cm3[soil]   cm[row length]

          DeltaSWTot = WTot - Wtot_Y(row,col)

!         Transpiration and Soil evaporation in mm/d
          EPTot = CellDetail % rate % EP_RATE / 10. 
     &       * CellDetail % Struc % Width
          ESTot = CellDetail % rate % ES_RATE / 10. 
     &       * CellDetail % Struc % Width

          IF (DAS == 0) THEN
            DeltaSWTot = 0.0
            DeltaNTot  = 0.0
          ELSE
            DeltaSWTot = WTot - Wtot_Y(row,col)
            DeltaNTot  = CelNtot - CelNtot_Y(row,col)
          ENDIF
          Wtot_Y(row,col) = WTot
          CelNtot_Y(row,col) = CelNtot

          WRITE (CLunn,1325) 
     &      YR, DOY, DAS, row, col, 

!           N Variables in kg/ha
     &      CelNtot, 
     &      DeltaNTot,
     &      CellDetail % State % SNO3, 
     &      CellDetail % State % SNH4, 
     &      CellDetail % State % Urea,

!           N variables in ppm
     &      CelNConc, 
     &      CelNO3conc, 
     &      CelNH4conc, 
     &      CelUREAconc,

!           N rates in kg/ha
     &      CellDetail % rate % CellFert, !kg[N]/ha/d
     &      CellDetail % rate % NMINER,   !kg[N]/ha/d
     &      CelNUptake,                   !kg[N]/ha/d
     &      CellDetail % rate % GHG,      !kg[N]/ha/d
     &      CellDetail % rate % NFlux_R,  !kg[N]/ha/d
     &      CellDetail % rate % NFlux_L,  !kg[N]/ha/d
     &      CellDetail % rate % NFlux_D,  !kg[N]/ha/d
     &      CellDetail % rate % NFlux_U,  !kg[N]/ha/d

!           Root variables
     &      CellDetail % state % RLV,     !cm/cm3 

!           Water variables
     &      CellDetail % state % SWV,     !mm3/mm3
     &      WTot,                         !cm3[water]/cm[row length]
     &      DeltaSWTot,                   !cm3[water]/cm[row length]
     &      ESTot,                        !mm
     &      EPTot,                        !mm
     &      CellDetail % rate % ES_RATE,  !mm
     &      CellDetail % rate % EP_RATE,  !mm
     &      CellDetail % rate % DripIrr,  !mm
     &      CellDetail % rate % CellInf,  !mm
     &      CellDetail % rate % SWFlux_R, !cm2/d
     &      CellDetail % rate % SWFlux_L, !cm2/d
     &      CellDetail % rate % SWFlux_D, !cm2/d
     &      CellDetail % rate % SWFlux_U  !cm2/d

        enddo
      enddo

 1325     FORMAT(I4,2X,I3.3,1X,I5,1X,I3,1X,I3,40F10.5)

      IF (DYNAMIC .EQ. SEASEND) CLOSE (UNIT = Clunn)

      RETURN
      END SUBROUTINE CellPlotDetail_2D

!=======================================================================
! CellPlotDetail_2D VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! CelNConc    Total N concentration in cell (ppm)
! CelNH4Conc  Ammonium in cell (ppm)
! CelNO3Conc  Nitrate in cell (ppm)
! celNtot     Total Nitrogen in soil cell (kg[N]/ha)
! CelNtot     Total N in cell (kg/ha)
! CelNtot_Y   Yesterday's value of CelNtot (kg/ha)
! CelNUptake  N uptake from cell (kg/ha)
! CelUreaConc Urea concentration in cell (ppm)
! ConvFactor  Conversion from kg/ha to ppm for this cell
! DeltaNTot   Change in N content from yesterday (kg/ha)
! DeltaSWtot  Change in water content from yesterday(cm3[water]/cm[row length])
! EPTot       Plant transpiration from cell today (mm)
! ESTot       Soil evaporation from cell today (mm)
! KG2PPM(L) Conversion factor to switch from kg [N] / ha to ug [N] / g 
!           KG2PPM(L) = 1.0/(BD*1.E-01*DLAYR(L))
! SNO3(L,j) Total extractable nitrate N in soil layer L (kg [N] / ha) 
! Wtot   Today's value of total water content (cm3[water]/cm[row length])
! Wtot_Y   Yesterday's value of total water content (cm3[water]/cm[row length])

!-----------------------------------------------------------------------
! CELLS(MaxRows,MaxCols) Contains cell state, rate, and structure info
!   see selected definitions below. See SoilCellUtils_2D.f90 for full list. 
! CellDetail contains same info as CELLS for one cell
!-----------------------------------------------------------------------
!  TYPE CellStateType variables
!    Real SWV                         !Soil water mm3/mm3
!    Real RLV                         !Root len dens cm/cm3 (PLANT)
!    REAL SNO3, SNH4, UREA            !Soil N (kg[N]/ha)
!    REAL KG2PPM                      !Conversion ppm to kg/ha
!  END TYPE CellStateType 
!
!  TYPE CellRateType variables
!    Sequence
!    REAL SWFlux_L, SWFlux_R   !Horiz soil water movement, cm2/d
!    REAL SWFlux_D, SWFlux_U   !Vert soil water movement, cm2/d
!    REAL ES_Rate              !Evaporation rates, mm/d
!    REAL EP_Rate              !Transpiration rates, mm/d
!    REAL DripIrr              !Drip irrig added directly to cell (mm/d)
!    REAL CellInf              !Rain + standard irrig (mm/d)
!    REAL NFlux_L, NFlux_R     !Horiz N movement (kg/ha)
!    REAL NFlux_D, NFlux_U     !Vert N movement (kg/ha)
!    REAL NO3Uptake, NH4Uptake !PLANT N uptake rates (kg/ha)
!    REAL CellFert             !Fertilizer (kg/ha)
!    REAL NMINER               !Net mineralization (kg/ha)
!    REAL NITRIF               !Nitrification (kg/ha)
!    REAL GHG                  !GHG N loss including denit (kg/ha)
!  END TYPE CellRateType
!-----------------------------------------------------------------------
! END SUBROUTINE CellPlotDetail_2D
!=======================================================================
