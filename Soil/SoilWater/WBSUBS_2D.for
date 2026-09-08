C=======================================================================
C  WBSUM_2D, Subroutine, J.T. Ritchie
C  Performs daily summation of water balance variables.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1989 JTR Written
C  12/05/1993 NBP Made into subroutine
!  10/18/1997 CHP Modified for modular format.
!  03/19/2009 CHP Modified for 2D
!-----------------------------------------------------------------------
!  Called by: WATBAL2D
!  Calls:     None
C=======================================================================
      SUBROUTINE WBSUM_2D(DYNAMIC,
     &    CELLS, DRAIN_2D, HalfRow, RAIN, RUNOFF, SWV,    !Input
     &    netLatFlow,                                     !Input
     &    CRAIN, TDRAIN, TEP, TRUNOF, DayLatFlow,         !Output
     &    TSW, TSWINI)                                    !Output

!-----------------------------------------------------------------------
      USE Cells_2D
      USE ModuleData
      IMPLICIT NONE
      SAVE

      INTEGER DYNAMIC, i, j
      REAL CRAIN, DRAIN_2D, HalfRow, RAIN, RUNOFF
      REAL TDRAIN, TEP, TRUNOF, TSW, TSWINI, DayLatFlow, netLatFlow

      REAL, DIMENSION(MaxRows,MaxCols) :: CellArea
      REAL, DIMENSION(MaxRows,MaxCols) :: SWV
      
      TYPE (CellType), DIMENSION(MaxRows,MaxCols) ::  CELLS
      REAL, DIMENSION(MaxRows, MaxCols) :: ColFrac

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      CRAIN  = 0.
      TDRAIN = 0.
      TRUNOF = 0.
      TSW    = 0.
      CellArea = CELLS%STRUC%CellARea
      ColFrac = BedDimension % ColFrac

      DO i = 1, NRowsTot
        DO j = 1, NColsTot
          SELECT CASE(CELLS(i,j)%Struc%Cell_Type)
          CASE (3,4,5) 
            TSW = TSW + SWV(i,j) * CellArea(i,j) / HalfRow * 10.
!                       cm[water]     cm3[soil]      cm[row length]   mm
!           mm[water] = --------- * -------------- * -------------- * -- 
!                        cm[soil]   cm[row length]     cm2[soil]      cm
          END SELECT
        ENDDO
      ENDDO
      TSWINI = TSW

!***********************************************************************
!     DAILY INTEGRATION 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
      TSW = 0.0
      TEP = 0.0
      DO i = 1, NRowsTot
        DO j = 1, NColsTot
          SELECT CASE(CELLS(i,j)%Struc%Cell_Type)
          CASE (3,4,5) 
            TSW = TSW + SWV(i,j) * CellArea(i,j) / HalfRow * 10.
            TEP = TEP + CELLS(i,j)%Rate%EP_Rate * ColFrac(i,j)
          END SELECT
        ENDDO
      ENDDO

!-----------------------------------------------------------------------
C     Increment summation variables.
!-----------------------------------------------------------------------
      CRAIN  = CRAIN  + RAIN
      TRUNOF = TRUNOF + RUNOFF

      IF (BedDimension % LIMIT_2D .GE. NRowsTot) THEN 
!       No water table, drainage is from the bottom of the profile.
        TDRAIN = TDRAIN + DRAIN_2D
      ELSE
!       Where there is a water table, drainage is from lowest 2D layer
!         into water table and affects the net lateral flow, but
!         should not be reported as deep drainage. 
        DayLatFlow = netLatFlow - DRAIN_2D
      ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
!     Transfer data to storage routine
      CALL PUT('SPAM', 'EP',  TEP)

      RETURN
      END SUBROUTINE WBSUM_2D

!-----------------------------------------------------------------------
!     WBSUM_2D VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! CRAIN     Cumulative precipitation from 0:00 am to the end of this time step (mm)
! DRAIN_2D  Drainage this time step from bottom of profile or from bottom of 2D simulation to water table (mm)
! HalfRow   Half the width of a row (cm)
! RAIN      Precipitation depth for current day (mm)
! RUNOFF    Calculated runoff (mm/d)
! TDRAIN    Cumulative daily drainage from profile (mm)
! TEP       Daily plant transpirtation (mm)
! TRUNOF    Cumulative runoff (mm)
! TSW       Total soil water in profile (mm)
! TSWINI    Initial soil water content (mm)
! DayLatFlow Daily value of lateral flow, minus flow from 2D simulation layers to water table (mm)
! netLatFlow Net lateral flow into the system, computed daily to maintain a managed water table (mm)
! CellArea   Area of one cell (cm2)
! SWV        Volumetric soil water content in cell (cm3 [water] / cm3 [soil])
! ColFrac    Fraction of Half row width represented by each soil column
!-----------------------------------------------------------------------
!     END SUBROUTINE WBSUM_2D
C=======================================================================

!=====================================================================
!  Wbal_Sep, Subroutine, Cheryl Porter

!  Daily water balance for 2D drip irrigation model
!  Separate daily balances are kept for bed, under-bed and furrow
!  Not currently used.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  06/02/2009 CHP adapted WBAL for 2-D model 
!-----------------------------------------------------------------------
!  Called by: WATBAL_2D
!=====================================================================
      SUBROUTINE Wbal_Sep(CONTROL, ISWITCH, 
     &    BedLimit, Cells, DRAIN, DRNbed, DRNubd, 
     &    IRRAMT, MG_Drain, RAIN, RUNOFF)

!     ------------------------------------------------------------------
      USE Cells_2D
      USE ModuleData
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, YR_DOY
      SAVE

      TYPE (ControlType), INTENT(IN) :: CONTROL
      TYPE (SwitchType),  INTENT(IN) :: ISWITCH
      Type (CellType),    INTENT(IN) :: Cells(MaxRows,MaxCols)
      REAL, INTENT(IN) :: BedLimit, DRAIN, DRNbed, DRNubd
      REAL, INTENT(IN) :: IRRAMT, MG_Drain, RAIN, RUNOFF
      REAL totDripRate
      
      REAL EP_rate(MaxRows,MaxCols)
      
      CHARACTER*1 IDETL, IDETW, ISWWAT, MEINF
      CHARACTER*15, PARAMETER :: SWBAL2 = 'SoilWatBal2.OUT'
      INTEGER DAS, DYNAMIC, FurCol1, FurRow1, i, j, LUNWBL
      INTEGER YRSIM, YRDOY
      INTEGER YR2, DY2

      REAL DAYWBAL, CUMWBAL
      REAL HalfRow, MG_Drain_mm
      REAL DRNfur, ESfur
      REAL RWUbed, RWUubd, RWUfur
      REAL TSWbed,  TSWubd,  TSWfur
      REAL TSWYbed, TSWYubd, TSWYfur
      REAL WBALbed, WBALubd, WBALfur

      REAL Bed_drain, Bed_drain_y
      TYPE (DripIrrType) DripIrrig(NDrpLn)
      
      LOGICAL FEXIST

!     ------------------------------------------------------------------
      IDETW   = ISWITCH % IDETW
      IDETL   = ISWITCH % IDETL
      ISWWAT  = ISWITCH % ISWWAT
      IF (IDETW .EQ. 'N' .OR. ISWWAT .EQ. 'N' .OR. IDETL /= 'D') RETURN
!     ------------------------------------------------------------------
      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM
      DAS     = CONTROL % DAS
      IDETW   = ISWITCH % IDETW
      IDETL   = ISWITCH % IDETL
      ISWWAT  = ISWITCH % ISWWAT
      MEINF   = ISWITCH % MEINF

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!     Open output file
      CALL GETLUN('SWBAL2', LUNWBL)
      INQUIRE (FILE = SWBAL2, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = LUNWBL, FILE = SWBAL2, STATUS = 'OLD',
     &    POSITION = 'APPEND')
      ELSE
        
        OPEN (UNIT = LUNWBL, FILE = SWBAL2, STATUS = 'NEW')
        WRITE(LUNWBL,'("*WATER BALANCE OUTPUT FILE")')
      ENDIF

      CALL HEADER(SEASINIT, LUNWBL, CONTROL%RUN)

      IF (IDETL .EQ. 'D') THEN
        !Write header for daily output
        WRITE (LUNWBL,1120)
 1120   FORMAT('@YEAR DOY   DAS',
     &'   SWbed   SWubd   SWfur    PRED    IRRD',
     &'    RNOF  DRNbed  DRNubd  DRNfur',
     &'  RWUbed  RWUubd  RWUfur   ESfur',
     &'  BALbed  BALubd  BALfur  BALday  BALcum', !)
     &'     MG_Drain')
      ENDIF
      
      CUMWBAL = 0.0

      FurCol1 = BedDimension%FurCol1
      FurRow1 = BedDimension%FurRow1
      HalfRow = BedDimension%RowSpc_cm / 2.0

      WBALbed = 0.0
      WBALubd = 0.0
      WBALfur = 0.0

      Bed_drain = 0.0

!***********************************************************************
!***********************************************************************
      ENDIF
!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT 
!***********************************************************************
      IF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      DRNfur = DRAIN - DRNubd

      TSWbed = 0.0; RWUbed = 0.0
      TSWubd = 0.0; RWUubd = 0.0; 
      TSWfur = 0.0; RWUfur = 0.0; ESfur  = 0.0
      
      EP_rate = CELLS%RATE%EP_rate
                    
      DO i = 1, NRowsTot
        DO j = 1, NColsTot
          SELECT CASE(CELLS(i,j)%STRUC%Cell_Type)
          CASE(3)  !bed
            TSWbed = TSWbed + CELLS(i,j)%STATE%SWV * 
     &          CELLS(i,j)%STRUC%CellArea / HalfRow * 10.
            RWUbed = RWUbed + CELLS(i,j)%RATE%EP_rate 

          CASE(4)  !under bed
            TSWubd = TSWubd + CELLS(i,j)%STATE%SWV * 
     &          CELLS(i,j)%STRUC%CellArea / HalfRow * 10.
            RWUubd = RWUubd + CELLS(i,j)%RATE%EP_rate 

          CASE(5)  !under furrow
             TSWfur = TSWfur + CELLS(i,j)%STATE%SWV * 
     &          CELLS(i,j)%STRUC%CellArea / HalfRow * 10.
            RWUfur = RWUfur + CELLS(i,j)%RATE%EP_rate
            ESfur = ESfur + CELLS(i,j)%RATE%ES_rate 
          END SELECT
        ENDDO
      ENDDO

!     Amount of water which is removed from bed system, reduced
!     proportional to bed height : artificial bed system height.
      CALL GET(DripIrrig)
      DO I = 1, NDripLnTOT
        totDripRate = totDripRate + DripIrrig(I)%IrrRate
      END DO
      DO I = 1, NDripLnTOT
        MG_Drain_mm = MG_Drain /   !MG_Drain in cm3
     &            (BedDimension%RowSpc_cm * DripIrrig(I)%DripSpc)* 10.
!               mm  = cm3 / cm / cm * 10 mm/cm
     $            * DripIrrig(I)%IrrRate / totDripRate
      END DO
    
      Bed_drain_y = Bed_drain
      Bed_drain = MG_Drain_mm * BedDimension%BedHt / BedLimit
!        mm     =       mm    *         cm         /     cm
     
      IF (DYNAMIC == OUTPUT) THEN
!       Change in storage = Inflows - Outflows
!       Balance = Inflows - Outflows - Change in storage
!       Water balance in bed
        WBALbed = 
     &        - IRRAMT                 !Inflows
     &        + DRNbed + RWUbed        !Outflows
     &        + Bed_drain_y            !Outflows
     &        + (TSWbed - TSWYbed)     !Change in soil water 
        
!       Water balance under bed
        WBALubd = 
     &        - DRNbed                 !Inflows
     &        + DRNubd + RWUubd        !Outflows
     &        + (TSWubd - TSWYubd)     !Change in soil water 
        
!       Water balance under furrow
        WBALfur = 
     &        - RAIN                   !Inflows
     &        + DRNfur + RUNOFF        !Outflows
     &        + ESfur + RWUfur         !Outflows
     &        + (TSWfur - TSWYfur)     !Change in soil water 
      ENDIF

      DAYWBAL = WBALbed + WBALubd + WBALfur
      CUMWBAL = CUMWBAL + DAYWBAL
      
      CALL YR_DOY(YRDOY, YR2, DY2)
      WRITE (LUNWBL,1300) YR2, DY2, DAS, 
     &    TSWbed, TSWubd, TSWfur, RAIN, IRRAMT, 
     &    RUNOFF, DRNbed, DRNubd, DRNfur, 
     &    RWUbed, RWUubd, RWUfur, ESfur, 
     &    WBALbed, WBALubd, WBALfur, DAYWBAL, CUMWBAL  , Bed_drain  
 1300   FORMAT(1X,I4,1X,I3.3,1X,I5,18F8.3, 5X, F8.3)

!     Save values for comparison tomorrow
      TSWYbed   = TSWbed
      TSWYubd   = TSWubd
      TSWYfur   = TSWfur
      
!***********************************************************************
!***********************************************************************
!     SEASEND - Seasonal output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
      CLOSE(LUNWBL)   

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE Wbal_Sep
!=======================================================================
!     Wbal_Sep VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! Bed_drain   Water drain from bed
! Bed_drain_y Vertical water drain from bed
! ColFrac(MaxCols)  Cell column width divided half row width
! CUMWBAL  Comulative water balance
! DAYWBAL  Daily water balance
! DRAIN    Drain water including under-bed and under-furrow
! DRNbed   Drain water from bed
! DRNubd   Drain water from under bed
! DRNfur   Drain water from furrow
! DY2      Day of year
! ES       Actual soil evaporation rate (mm ?)
! ESfur    Actual soil evaporation rate from furrow (mm ?)
! EXPER    Experiment code (prefix of input files) 
! FIRST    Indicates first call to subroutine (true or false)
! IDETL    Switch for detailed printout (Y or N)
! IDETW    Y=detailed water balance output, N=no detailed output 
! ISWITCH  Composite variable containing switches which control flow of 
!            execution for model.  The structure of the variable 
!            (SwitchType) is defined in ModuleDefs.for. 
! ISWWAT   Water simulation control switch (Y or N) 
! IRRAMT   Irrigation amount (mm) 
! LUNWBL   Logical unit number for WBAL.OUT file 
! MEINF  Method of infiltration
! RAIN     Precipitation depth for current day (mm)
! RUNOFF   Calculated runoff (mm/d)
! RWUbed   Root uptake from bed
! RWUubd   Root uptake from underbed
! RWUfur   Root uptake from furrow
! SolProfDrain Drain from the bottom of soil profile. 
! SWEF     Soil water evaporation fraction; fraction of lower limit content 
!            to which evaporation can reduce soil water content in top layer
!            (fraction)
! TSW      Total soil water in profile (cm)
! TSWbed   Total soil water in bed in mm
! TSWfur   Total soil water in furrow in mm
! TSWubd   Total soil water under bed in mm
! TSWYbed  Yesterday's total soil water of bed in mm
! WBALbed  Water balance of bed 
! WBALubd  Water balance for under-bed
! WBALfur  Water balance of furrow
! YRDOY    Current day of simulation (YYDDD)
! YRSIM    Start of simulation date (YYDDD)
! YR2       Year
!-----------------------------------------------------------------------
!     END SUBROUTINE Wbal_Sep
!=======================================================================

