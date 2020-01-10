!==============================================================================
! Utility subroutines for 2-D cell data
!-----------------------------------------------------------
! 02/30/2011  
!              Increase MaxRows to 30 to match the change of NL
!              Replace RowFrac to ColFrac
  Module Cells_2D
  USE ModuleDefs
  SAVE
  
! Cell arrays -- fixed maximum dimensions for now
  INTEGER, PARAMETER :: MaxRows = 30, MaxCols = 25 
  INTEGER, PARAMETER :: MaxCells = 10
  REAL, DIMENSION(MaxRows,NL)     :: Layer_Cell_Dep
  REAL, DIMENSION(MaxRows,MaxCols):: Surf_Cell_Frac
  REAL, DIMENSION(MaxRows,MaxCols):: WidthFrac
  
  INTEGER NRowsTOT, NColsTOT
  INTEGER NDripLnTOT
! INTEGER NBedRows, NBedCols         !number of bed rows, cols
 
  TYPE BedDimType
    REAL BEDHT, BEDWD, DigDep, ROWSPC_cm  !cm
    REAL, DIMENSION(MaxRows,MaxCols) :: mm_2_vf  !conversion factor
    INTEGER FurCol1, FurRow1  !furrow location in grid
!   LIMIT_2D represents the lowest layer for which 2D modeling is done.
!   Below this depth, use 1D tipping bucket approach.
!   For now, compute as the highest layer with depth > 70cm.
!   Column width as fraction of total field surface area
    REAL, DIMENSION(MaxRows,MaxCols) :: ColFrac, BedFrac
    INTEGER LIMIT_2D
!    INTEGER DripLN(NDrpLn)      !Drip line number
    INTEGER DripCol(NDrpLn)     !Column recieving drip irrig
    INTEGER DripRow(NDrpLn)     !Row recieving drip irrig
    LOGICAL RaisedBed, PMCover 
  END TYPE
  TYPE (BedDimType) BedDimension

! TEMP CHP
  type Cell_detail_type
    integer :: row = 1, col = 9  !cm
    integer, DIMENSION(MaxCells) :: rows = (/ 2, 2, 3, 4, 5, 5, 6, 7, 8, 9 /) !cm
    integer, DIMENSION(MaxCells) :: cols = (/ 1, 2, 2, 2, 1, 2, 2, 2, 2, 2 /) !cm
    integer :: NPTFLG = 3 ! 0 for cell; 1 for whole row; 2 for whole column; 3 for all cells; 4 for warning
    integer :: WPTFLG = 0 ! 0 for cell; 1 for whole row; 2 for whole column; 3 for all cells; 4 for warning; 5 for multi-cells
    real    :: MINTS = 0.25! hour
    real    :: NEXTTS = 0.25
    integer :: MULTI = 0 ! for multiple row/col output
    integer :: start = 0, fin = 1000  !day
    Double Precision, Dimension(MaxRows,MaxCols) :: vdiff, vgrav   !cm2
    Double Precision, Dimension(MaxRows,MaxCols+1) :: H_in, H_out  !cm2
    Double Precision, Dimension(MaxRows+1,MaxCols) :: V_in, V_out  !cm2
    Double Precision, Dimension(MaxRows,MaxCols) :: IrrVol, InfVol !cm2
  end type
  type (Cell_detail_type) Cell_detail
  
  type Cell_Ndetail_type
    integer row, col  !cm
    real, DIMENSION(MaxRows,MaxCols) :: NFlux_L_out, NFlux_R_out
    real, DIMENSION(MaxRows,MaxCols) :: NFlux_D_out, NFlux_U_out
    real, DIMENSION(MaxRows,MaxCols) :: NFlux_L_in, NFlux_R_in
    real, DIMENSION(MaxRows,MaxCols) :: NFlux_D_in, NFlux_U_in
  end type
  type (Cell_Ndetail_type) Cell_Ndetail

! -----------------------------------------------------------------------------

      TYPE CellStrucType 
        Sequence
        Integer*4 CellType                
        ! 0 = no cell data (e.g., in furrow)      1,2     -99     1,2
        ! 1 = surface water                      ------|       |------
        ! 2 = surface litter                       3   |   0   |   3
        ! 3 = soil in bed                        _  _  |_______|  _  _
        ! 4 = soil below bed                                        
        ! 5 = soil below furrow                    4   |   5   |   4
        !-99= not simulated                       -99     -99     -99
        Real*4 Thick       !cm
        Real*4 Width       !cm
        Real*4 CellArea    !cm3[soil]/cm[row length]
      END TYPE CellStrucType

      TYPE CellStateType
        Sequence
        Real SWV                         !Soil water mm3/mm3
        Real RLV                         !Root len dens cm/cm3 (PLANT)
        REAL SNO3, SNH4, UREA            !Soil N (kg[N]/ha)
        REAL BD, DUL, LL, SAT, SWCN, WR  !Soil properties 
        ! WR(L)     Root hospitality factor, used to compute root distribution
!       REAL OC, SPi_Labile, SRP         !Not used currently
!       Real STemp                       !Soil temperature (SPAM)
      END TYPE CellStateType 

      TYPE CellRateType
        Sequence
        REAL SWFlux_L, SWFlux_R             !Horiz soil water movement, cm2/d
        REAL SWFlux_D, SWFlux_U             !Vert soil water movement, cm2/d
        REAL ES_Rate                        !Evaporation rates, mm/d
        REAL EP_Rate                        !Transpiration rates, mm/d
        REAL NO3Uptake, NH4Uptake           !PLANT N uptake rates
!       REAL OCAdd                          !Plant residue
      END TYPE CellRateType

      TYPE CellType 
        Sequence
        TYPE (CellStrucType) Struc
        TYPE (CellStateType) State
        TYPE (CellRateType)  Rate
      END TYPE CellType
      
!==============================================================================
  CONTAINS
!==============================================================================
!   Cell2Layer_2D, Subroutine 
!   Collapses 2D soil grid cell data into 1D DSSAT soil layer data.
!   Input data are in units of mass and are aggregated by adding.
!   Output data are in units of mass; surface value kept separately from soil array.
!   Assumes that thickness of each row is constant and width of each column is constant.
! -----------------------------------------------------------------------------
! 09/12/2006 CHP Written
! 04/16/2007 CHP Convert from 1D to 2D
! -----------------------------------------------------------------------------
  Subroutine Cell2Layer_2D(                             &
        CellArray, CellStruc, NLAYR,                    & !Input
        LayerArray, SurfaceVal)                           !Output
        
  IMPLICIT NONE

  TYPE (CellStrucType), DIMENSION(MaxRows,MaxCols), INTENT(IN) :: CellStruc
  REAL,                 DIMENSION(MaxRows,MaxCols), INTENT(IN) :: CellArray

  REAL, DIMENSION(NL), INTENT(OUT):: LayerArray
  REAL, OPTIONAL,      INTENT(OUT):: SurfaceVal

  INTEGER  NLAYR, L, Row, Col
  REAL Fraction

!----------------------------------------------------------------------
! Surface data
  IF (PRESENT(SurfaceVal)) THEN
    SurfaceVal = 0.0
    DO Row = 1, NRowsTot
      DO Col = 1, NColsTot
        IF (CellStruc(Row,Col)%CellType == 2) THEN
          SurfaceVal = SurfaceVal + CellArray(Row,Col)
        ENDIF
      ENDDO
    ENDDO
  ENDIF

! Use Cell/Layer association array to distribute mass from cells to soil layers
  LayerArray = 0.0
  LayerLoop: DO L = 1, NLAYR
    RowLoop: DO Row = 1, NL
      IF (CellStruc(Row,1) % Thick > 1.E-5) THEN
        Fraction = Layer_Cell_Dep(Row,L) / CellStruc(Row,1) % Thick
        IF (Fraction < 1.E-6) CYCLE !No contribution to soil layer "L" from Cell row "Row"
        ColLoop: DO Col = 1, NColsTot
          SELECT CASE(CellStruc(Row,Col) % CellType)
          CASE(3,4,5)
            LayerArray(L) = LayerArray(L) + Fraction * CellArray(Row,Col)
          END SELECT
        ENDDO ColLoop
      ENDIF
    ENDDO RowLoop
  ENDDO LayerLoop

!----------------------------------------------------------------------
  RETURN
  END SUBROUTINE Cell2Layer_2D
!==============================================================================
!
!==============================================================================
!   Layer2Cell_2D, Subroutine 
!   Expands soil layer data from DSSAT layers into 2D cell data
!   Input data, bedLayerArray, furLayerArray, are in units of mass 
!   Surface values stored separately in SurfaceVal scalar.
!   Output data, CellArray, are also in units of mass.  
!   Fixed dimension soil grid: 1-> NL=20 Rows allowed
! -----------------------------------------------------------------------------
! 09/12/2006 CHP Written
! 04/16/2007 CHP Convert from 1D to 2D
! -----------------------------------------------------------------------------
  Subroutine Layer2Cell_2D(                             &
       CellStruc, NLAYR, DLAYR, LayerArray, SurfaceVal, &  !Input
       CellArray)                                          !Output

  USE ModuleDefs
  IMPLICIT NONE
            
  TYPE (CellStrucType), DIMENSION(MaxRows,MaxCols), INTENT(IN) :: CellStruc
  REAL,                 DIMENSION(NL),              INTENT(IN) :: LayerArray 
  REAL,                                             INTENT(IN) :: SurfaceVal

  REAL,                 DIMENSION(MaxRows,MaxCols), INTENT(OUT):: CellArray

  INTEGER L, NLAYR, Row, Col
  REAL Fraction, RowAdd
  REAL, DIMENSION(NL) :: DLAYR

!----------------------------------------------------------------------
  CellArray = 0.0

! Surface cells
  DO Row = 1, NRowsTot
    DO Col = 1, NColsTot
      IF (CellStruc(Row,Col) % CellType == 2) THEN
        CellArray(Row,Col) = SurfaceVal * Surf_Cell_Frac(Row,Col)
      ENDIF
    ENDDO
  ENDDO

! Soil cells
  DO Row = 1, NRowsTot
    DO L = 1, NLAYR
      Fraction = Layer_Cell_Dep(Row,L) / DLAYR(L)
      IF (Fraction > 1.E-6) THEN
        RowAdd = LayerArray(L) * Fraction
        DO Col = 1, NColsTot
          CellArray(Row,Col) = CellArray(Row,Col) + RowAdd * WidthFrac(Row,Col)
        ENDDO
      ENDIF
    ENDDO
  ENDDO

!----------------------------------------------------------------------
  RETURN
  END SUBROUTINE Layer2Cell_2D
!==============================================================================

!==============================================================================
!   Interpolate2Layers_2D, Subroutine 
!   Collapses soil layer data from 2D cell data into 1D DSSAT layers
!   Values are cell width weighted average rather than accumulated as in the
!     Cell2Layer_2D subroutine.  For use by values which are not
!     additive, e.g., Soil water content or temperature
!   Input data, CellStruc, SurfaceVal, CellArray(MaxRows,MaxCols), 
!     Surface values stored separately in SurfaceVal scalar.
! -----------------------------------------------------------------------------
! 09/13/2006 CHP Written
! 04/16/2007 CHP Convert from 1D to 2D
! -----------------------------------------------------------------------------
  Subroutine Interpolate2Layers_2D(                    &
        CellArray, CellStruc, NLAYR,                   & !Input
        LayerArray, SurfaceVal)                          !Output

  USE ModuleDefs
  IMPLICIT NONE
          
  TYPE (CellStrucType), DIMENSION(MaxRows,MaxCols), INTENT(IN) :: CellStruc
  REAL,                 DIMENSION(MaxRows,MaxCols), INTENT(IN) :: CellArray
  REAL, OPTIONAL,      INTENT(OUT) :: SurfaceVal
  REAL, DIMENSION(NL), INTENT(OUT) :: LayerArray 

  INTEGER L, NLAYR, Row, Col
  REAL FRACTION
  
!----------------------------------------------------------------------
! Surface data
  IF (PRESENT(SurfaceVal)) THEN
    SurfaceVal = 0.0
    DO Row = 1, NL
      DO Col = 1, NColsTot
        IF (CellStruc(Row,Col)%CellType == 2) THEN
          SurfaceVal = SurfaceVal + CellArray(Row,Col) * Surf_Cell_Frac(Row,Col) 
        ENDIF
      ENDDO
    ENDDO
  ENDIF

! Use Cell/Layer association array to distribute mass from cells to soil layers
  LayerArray = 0.0
  LayerLoop: DO L = 1, NLAYR
    RowLoop: DO Row = 1, NRowsTot
      IF (CellStruc(Row,1) % Thick > 1.E-5) THEN
        Fraction = Layer_Cell_Dep(Row,L) / CellStruc(Row,1) % Thick
        IF (Fraction < 1.E-6) CYCLE !No contribution to soil layer "L" from Cell row "Row"
        ColLoop: DO Col = 1, NColsTot
          SELECT CASE(CellStruc(Row,Col) % CellType)
          CASE(3,4,5)
            LayerArray(L) = LayerArray(L) + CellArray(Row,Col) * Fraction * WidthFrac(Row,Col)
          END SELECT
        ENDDO ColLoop
      ENDIF
    ENDDO RowLoop
  ENDDO LayerLoop

  RETURN
  END SUBROUTINE Interpolate2Layers_2D
!==============================================================================

!==============================================================================
!   Interpolate2Cells_2D, Subroutine 
!   Expands 2D soil cell data from DSSAT layers
!   Values are not accumulated as in the Layer2Cell_2D subroutine.  
!     For use by values which are not additive, e.g., Soil water content
!   Input data, CellStruc, bedLayerArray(NL), furLayerArray(NL-NBedRows),
!     Surface values stored separately in SurfaceVal scalar.
!   Fixed dimension soil grid: 1-> NL=20 rows allowed
! -----------------------------------------------------------------------------
! 09/13/2006 CHP Written
! 04/16/2007 CHP Convert from 1D to 2D
! -----------------------------------------------------------------------------
  Subroutine Interpolate2Cells_2D(                    &
        CellStruc, SOILPROP, LayerArray, SurfaceVal,  &  !Input
        CellArray)                                       !Output

  USE ModuleDefs
  IMPLICIT NONE
          
  TYPE (CellStrucType), DIMENSION(MaxRows,MaxCols), INTENT(IN) :: CellStruc
  TYPE (SoilType),                                  INTENT(IN) :: SOILPROP
  REAL, DIMENSION(NL),                              INTENT(IN) :: LayerArray 
  REAL,                                             INTENT(IN) :: SurfaceVal

  REAL, DIMENSION(MaxRows,MaxCols),                 INTENT(OUT):: CellArray

  INTEGER L, NLAYR, Row, Col
  REAL Fraction

  NLAYR = SOILPROP % NLAYR

!----------------------------------------------------------------------
  CellArray = 0.0

  DO Row = 1, NRowsTot
    DO Col = 1, NColsTot
      SELECT CASE(CellStruc(Row,Col)%CellType)
!     Surface data
      CASE (2); CellArray(Row,Col) = SurfaceVal 

!     Soil data - weighted average
      CASE (3,4,5)
        DO L = 1, NLAYR
          Fraction = Layer_Cell_Dep(Row,L) / (CellStruc(Row,Col) % Thick)
          CellArray(Row,Col) = CellArray(Row,Col) + Fraction * LayerArray(L)
        ENDDO
      END SELECT   
    ENDDO   
  ENDDO

  RETURN
  END SUBROUTINE Interpolate2Cells_2D
!==============================================================================

!==============================================================================
!   Subroutine Layer_Cell_Assoc - used with 1-D and 2-D cells
!   Associates cell and layers by depth of soil (cm) that is common to both
!   Sends out 2-D array of soil depth values.  
!   Sends out 1-D array of WidthFrac, fraction of total width per column
!   Done once only -- array values saved for subsequent calls.

!   Cell data sent in as m, soil layers as cm -- this routine stores 
!       all data as cm.

! -----------------------------------------------------------------------------
! 09/13/2006 CHP Written
! 04/16/2007 CHP Convert from 1D to 2D
! -----------------------------------------------------------------------------
  Subroutine Layer_Cell_Assoc(CellStruc, SOILPROP) 

  USE ModuleDefs
  IMPLICIT NONE
  SAVE
                                        
  TYPE (CellStrucType), DIMENSION(MaxRows,MaxCols), INTENT(IN) :: CellStruc
  TYPE (SoilType) :: SOILPROP

  INTEGER NLAYR
  REAL, DIMENSION(NL) :: DS, DLAYR
  REAL, DIMENSION(MaxRows) :: TotWidth

  INTEGER L, Row, Col, NR, FirstSoilRow, LastSoilRow
  REAL CellDep, CellTop, Fraction, FracDep, FracTop
  REAL LayerDep, LayerTop, RowThick, TotSurfXSArea
  REAL CumCellDep
  REAL, DIMENSION(MaxRows,NL)     :: Temp_LCD
  REAL, DIMENSION(MaxRows,MaxCols):: Temp_WF

!----------------------------------------------------------------------
    DLAYR = SOILPROP % DLAYR
    DS    = SOILPROP % DS
    NLAYR = SOILPROP % NLAYR
    FirstSoilRow = 1

!   Find fraction of total surface per each surface cell
    Surf_Cell_Frac = 0.0
    TotSurfXSArea = 0.0
    DO Row = 1, NRowsTot
      DO Col = 1, NColsTot
        IF (CellStruc(Row,Col) % CellType == 2) THEN       !surface cell
          Surf_Cell_Frac(Row,Col) = CellStruc(Row,Col) % Thick * CellStruc(Row,Col) % Width
          TotSurfXSArea = TotSurfXSArea + Surf_Cell_Frac(Row,Col)
        ENDIF
      ENDDO
    ENDDO 
    
    IF (TotSurfXSArea > 0.) THEN
      Surf_Cell_Frac = Surf_Cell_Frac / TotSurfXSArea
    ELSE 
      Surf_Cell_Frac = 0.0
    ENDIF
        
!----------------------------------------------------------------------
!   Find fraction of total width per each column for soil cells
    WidthFrac = 0.0
    TotWidth = 0.0
    DO Row = 1, NRowsTot
      DO Col = 1, NColsTot
        SELECT CASE(CellStruc(Row,Col) % CellType)
        CASE(3,4,5)
          TotWidth(Row) = TotWidth(Row) + CellStruc(Row,Col) % Width
          WidthFrac(Row,Col) = CellStruc(Row,Col) % Width 
        CASE DEFAULT
          WidthFrac(Row,Col) = 0.0
        END SELECT
      ENDDO
      IF (TotWidth(Row) > 1.E-6) THEN
        DO Col = 1, NColsTot
          WidthFrac(Row,Col) = WidthFrac(Row,Col) / TotWidth(Row)
        ENDDO
      ENDIF
    ENDDO 

!----------------------------------------------------------------------
!   Already know number of soil layers = NLAYR
!   Maximum depth of soil profile = DS(NLAYR)

!   Find first row that is soil (for column 1)
    DO Row = 1, NRowsTot
      IF (CellStruc(Row,1) % CellType == 3) THEN       !soil cell
        FirstSoilRow = Row
        EXIT
      ENDIF
    ENDDO

!   Find maximum depth of cells and last cell soil row
    LastSoilRow = MaxRows
    CumCellDep = 0.0
!   DO Row = FirstSoilRow, MaxRows
    DO Row = 1, MaxRows
      IF (CellStruc(Row,1) % Thick > 1.E-6) THEN
        CumCellDep = CumCellDep + CellStruc(Row,1) % Thick
      ELSE
        LastSoilRow = Row - 1
        EXIT
      ENDIF
    ENDDO 

! Force soil layer data to match bottom specified by cell data.
! Skip surface cells
  IF (CumCellDep > DS(NLAYR)) THEN
!   Expand lowest layer to match profile depth for cells
    DLAYR(NLAYR) = DLAYR(NLAYR) + CumCellDep - DS(NLAYR)
    DS(NLAYR) = CumCellDep

  ELSE
!   Cut off soil layers to match cell depth
    DO L = 1, NLAYR
      IF (DS(L) < CumCellDep) THEN
        CYCLE
      ELSEIF (L > 1) then
        IF (DS(L-1) < CumCellDep) THEN
!         This is the lowest soil layer
          DS(L) = CumCellDep
          DLAYR(L) = DS(L) - DS(L-1)
          NLAYR = L
        ELSE
!         This is below the lowest soil layer
          DS(L) = 0.0
          DLAYR(L) = 0.0
        ENDIF
      ELSE
!       Only one soil layer, make a 2nd layer
        DS(2) = CumCellDep
        DLAYR(2) = CumCellDep - DLAYR(1)
        NLAYR = 2
      ENDIF
    ENDDO
  ENDIF

  SOILPROP % NLAYR = NLAYR
  SOILPROP % DLAYR = DLAYR
  SOILPROP % DS    = DS

!----------------------------------------------------------------------
!   Aggregate layers
    LayerDep = 0.0
    CellDep = 0.0
    NR = FirstSoilRow

!   Loop thru soil layers
    LayerLoop: DO L = 1, NLAYR
      LayerTop = LayerDep
      LayerDep = DS(L)
    
!     Aggregate a row of soil cells, starting at row Row (from surface loops) 
      CellRowLoop: DO Row = NR, LastSoilRow
        RowThick = CellStruc(Row,1) % Thick
        CellTop = CellDep
        CellDep = CellDep + RowThick
        FracTop = MAX(CellTop,LayerTop)
        FracDep = MIN(CellDep,LayerDep)
        Fraction = (FracTop - FracDep) / (LayerTop - LayerDep)
        Layer_Cell_Dep(Row,L) = Fraction * DLAYR(L)
    
        IF (LayerDep < CellDep) THEN
          NR = Row
          CellDep = CellTop   !Reset, so it calculates correctly in next loop
          EXIT CellRowLoop
        ELSEIF (ABS(LayerDep - CellDep) < 1.E-5) THEN !LayerDep == CellDep
          NR = Row + 1
          EXIT CellRowLoop
        ENDIF
    
      ENDDO CellRowLoop   
    ENDDO LayerLoop

    Temp_LCD = Layer_Cell_Dep
    Temp_WF  = WidthFrac

!----------------------------------------------------------------------
  RETURN
  END SUBROUTINE Layer_Cell_Assoc
!==============================================================================

!!==============================================================================
!  Subroutine Allocate_Var(Cell_Var, Var)
!  
!  Use ModuleDefs
!  Implicit None
!  SAVE
!  
!  Real Cell_Var(MaxRows,MaxCols)
!  Real, Allocatable :: Var(:,:)
!  Integer Row, Col
!  
!  DEALLOCATE (Var)
!  ALLOCATE (Var(NRowsTot, NColsTot))
!  
!  DO Row = 1, NRowsTot
!    DO Col = 1, NColsTot
!      Var(Row,Col) = Cell_Var(Row,Col)
!    ENDDO
!  ENDDO
!
!  RETURN
!  END Subroutine Allocate_Var
!!==============================================================================

!==============================================================================
  Subroutine BedLayerAdjust(SOILPROP, NLAYR0, DS0, Value, BEDHT)
! Takes soil layer data with pre-bed construction layer thicknesses and adjusts
!   data for bed soil layers.  Use for initial condition data from FILEIO.

    Implicit None
    INTEGER L, NLAYR0, NLAYRI
    REAL BEDHT, DigDep, BedValue
    REAL, DIMENSION(NL) :: DS0, Value, DSI, DLAYRI, DS_temp, Value_temp
    TYPE (SoilType) SOILPROP
    
    DigDep = BedDimension % DigDep
    BEDHT  = BedDimension % BEDHT
    
!   Mix properties within constructed bed
    CALL MixMassVol (Value, SOILPROP, DigDep, BedValue)
    
!   Create a new DS and value array that match the input values, but have an 
!   additional soil layer for the added bed height (BEDHT - DigDepth).  The 
!   reference points for the bed-constructed layers and the new layer should 
!   now match.  Use LMATCH to synch the values.  Replace values in the bed with
!   mixed values from MixMass subroutine.

    DS_temp(1) = BEDHT - DigDep
    DLAYRI(1) = BEDHT - DigDep
    Value_temp(1) = BedValue
    DO L = 1, NLAYR0
      DSI(L+1) = DS0(L) + BEDHT - DigDep
      DLAYRI(L+1) = DSI(L+1) - DSI(L)
      Value_temp(L+1) = Value(L)
    ENDDO
    NLAYRI = NLAYR0 + 1
    
    CALL LMATCH (NLAYRI, DSI, Value_temp, SOILPROP%NLAYR, SOILPROP%DS)
    Value = Value_temp

    Return
    End Subroutine BedLayerAdjust

!==============================================================================

  END MODULE
!==============================================================================
