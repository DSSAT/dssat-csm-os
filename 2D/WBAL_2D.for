!=====================================================================
!  Wbal_2D, Subroutine, Gerrit Hoogenboom
!  Modified for 2-D drip irrigation model
!  Seasonally: Provides output Water balance.  Prints file SoilWatBal.OUT
!  Data is obtained from WATBAL, SPAM and IRRIG modules daily.  
!  Data from SPAM and IRRIG are sent via GETPUT routines.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  09/05/2008 CHP adapted WBAL for 2-D model 
!  08/15/2011  Add columns of LIMIT_2D and MgWTD for SoilWatBal.OUT
!              Fix bug of YRDOY
!              Clarify the output Drain as SolProfDrain to distiguish the drain from LIMIT_2D
!              StdIrrig may need replace by IRRAMT
!              Need to handle if both dripper and convension irrigtaion applied on same day 
!-----------------------------------------------------------------------
!  Called by: WATBAL
!=====================================================================
      SUBROUTINE Wbal_2D(CONTROL, ISWITCH, COUNT,
     &    CES, CEP, Drain_Limit2D, RUNOFF, IRRAMT, RAIN, 
     &    TES, TEP, CRAIN, TDRAIN, TRUNOF, TSW,
     &    LatFlow, StdIrrig, 
     &    ES, ES_DAY)

!     ------------------------------------------------------------------
      USE ModuleDefs 
      USE ModuleData
      USE Cells_2D
      IMPLICIT NONE
      SAVE

      CHARACTER*1 IDETL, IDETW, ISWWAT, MEINF
      CHARACTER*14, PARAMETER :: SWBAL = 'SoilWatBal.OUT'
      INTEGER DAS, DYNAMIC, LUNWBL, COUNT
!     INTEGER DOY, YEAR, L, NLAYR
      INTEGER YRSIM, YRDOY, INCDAT
      INTEGER YR1, DY1, YR2, DY2, LIMIT_2D

      REAL CEO, CES, CEP, TEP, TES, CRAIN, EFFIRR 
      REAL TDRAIN, TOTIR, TRUNOF, TSW, TSWINI
      REAL LatFlow, CumLatFlow, StdIrrig
      REAL WBALAN, MgmtWTD, SolProfDrain

!     Temporary daily balance
!     REAL, DIMENSION(NL) :: DLAYR, SWDELTS, SWDELTX
!     REAL SWDELTSTOT, SWDELTXTOT
      REAL IRRAMT, RAIN, RUNOFF, Drain_Limit2D
      REAL CUMWBAL, TOTEFFIRR, TSWY , ES

      DOUBLE PRECISION ES_DAY

      LOGICAL FEXIST

!     The variable "ISWITCH" is of type "SwitchType".
      TYPE (ControlType)  CONTROL
      TYPE (SwitchType)   ISWITCH

!     ------------------------------------------------------------------
      IDETW   = ISWITCH % IDETW
      IDETL   = ISWITCH % IDETL
      ISWWAT  = ISWITCH % ISWWAT
      IF (IDETW .EQ. 'N' .OR. ISWWAT .EQ. 'N' .OR. IDETL == '0') RETURN
!     ------------------------------------------------------------------
      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM
      DAS     = CONTROL % DAS
      IDETW   = ISWITCH % IDETW
      IDETL   = ISWITCH % IDETL
      ISWWAT  = ISWITCH % ISWWAT
      MEINF   = ISWITCH % MEINF
      
      CALL GET('MGMT','WATTAB',MgmtWTD)
      LIMIT_2D = BedDimension % LIMIT_2D
!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!     Open output file
      CALL GETLUN('SWBAL', LUNWBL)
      INQUIRE (FILE = SWBAL, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = LUNWBL, FILE = SWBAL, STATUS = 'OLD',
     &    POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = LUNWBL, FILE = SWBAL, STATUS = 'NEW')
        WRITE(LUNWBL,'("*WATER BALANCE OUTPUT FILE")')
      ENDIF

      CALL HEADER(SEASINIT, LUNWBL, CONTROL % RUN)

      IF (IDETL .EQ. 'D') THEN
        !Write header for daily output
        WRITE (LUNWBL,1120)
 1120   FORMAT('@YEAR DOY   DAS',
     & '      SWTD',                           !State vars
     & '     IRRD     PRED',                   !Inflows
     & '    LFLOD',                            !Inflows
     & '     DRND     ROFD     ESAD     EPAD', !Outflows
     & '     WBAL    CUMWBAL',                 !Balance
     & '    COUNT         ES     ES_DAY',      !Extras
     & ' LIMIT_2D MgWTD')                        
      ENDIF

      TSWINI = TSW
      TSWY   = TSW
      CUMWBAL = 0.0
      CumLatFlow  = 0.0
        
        CALL YR_DOY(INCDAT(YRDOY,-1),YR2,DY2)
      
        WRITE (LUNWBL,1300) YR2, DY2, DAS, 
     &    TSW,                                 !State variables
     &    0.0, 0.0, 0.0,                       !Inflows
     &    0.0, 0.0, 0.0, 0.0,                  !Outflows
     &    0.0, 0.0,                            !Balance
     &    COUNT, 0.0, 0.0,                     !Extras
     &    LIMIT_2D, MgmtWTD                    !LIMIT_2D, WaterTableDepth

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
!-----------------------------------------------------------------------
      IF (IDETL .EQ. 'D') THEN
      
!       Transfer data from constructed variable to local variables
        CALL Get('SPAM','CEO',CEO)

!       Change in storage = Inflows - Outflows
!       Balance = Inflows - Outflows - Change in storage
        if (BedDimension % LIMIT_2D .GE. NRowsTot) then 
          SolProfDrain = Drain_Limit2D
        else
          SolProfDrain =0
        !  LatFlow = LatFlow  + Drain_Limit2D It is already counted in Dainage_2D
        endif
          ! rain water from plastic cover run to furrow, thus use full rain data from weather 
          ! The following variables are in mm and are on teh average of the ROW space
          WBALAN = 
     &         + IRRAMT + RAIN !Inflows in mm. IRRAMT include the drip & spinkle
   ! &         + IRRAMT + StdIrrig + RAIN     !Inflows  JZW remove teh IRR amount
     &         + LatFlow                      !Lateral flow
     &         - SolProfDrain - RUNOFF - TES - TEP   !Outflows
     &         - (TSW - TSWY)                 !Change in soil water 
        !else
        ! SolProfDrain =0
          !! Drain_Limit2D is part of LatFlow
         ! WBALAN = 
   !  &         + IRRAMT + RAIN     !Inflows
   !! &         + IRRAMT + StdIrrig + RAIN     !Inflows
   !  &         + LatFlow                      !Lateral flow
   !  &         - RUNOFF - TES - TEP   !Outflows
   !  &         - (TSW - TSWY)   
   !     endif

        CUMWBAL = CUMWBAL + WBALAN

    
        CALL YR_DOY(YRDOY, YR2, DY2)
        WRITE (LUNWBL,1300) YR2, DY2, DAS 
     &    , TSW                                       !State variables
   
     &    , IRRAMT, RAIN                              !Inflows
   ! &    , IRRAMT+StdIrrig, RAIN                     !Inflows
     &    , LatFlow                                   !Lateral flow
     &    , SolProfDrain, RUNOFF, TES, TEP                   !Outflows
     &    , WBALAN, CUMWBAL                           !Balance
     &    , COUNT, ES, ES_DAY
     &    , LIMIT_2D, MgmtWTD
 1300   FORMAT(1X,I4,1X,I3.3,1X,I5
     &    , F10.4       !TSW
     &    , 3F9.4       !Inflows
     &    , 4F9.4       !Outflows
     &    , F9.4, F11.4 !Balances
     &    , I9, 2F11.4  !ES, ES_DAY
     &    , 5X, I3, 1X, F6.1)         !LIMIT_2D

        !Save values for comparison tomorrow
        TSWY   = TSW
      ENDIF
      
      CumLatFlow = CumLatFlow + LatFlow

!***********************************************************************
!***********************************************************************
!     SEASEND - Seasonal output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
      YRSIM   = CONTROL % YRSIM
      CALL YR_DOY(YRSIM, YR1, DY1)
      CALL YR_DOY(YRDOY, YR2, DY2)

      CALL Get('SPAM','CEO',CEO)
!      CALL Get('SPAM','CEP',CEP)
!      CALL Get('SPAM','CES',CES)
!      CALL Get('SPAM','EP', EP)
!      CALL Get('SPAM','ES', ES)
      CALL Get('MGMT','TOTIR', TOTIR)
      CALL Get('MGMT','EFFIRR',EFFIRR)

      IF (EFFIRR .GT. 0.0) THEN
        TOTEFFIRR = EFFIRR * TOTIR
      ELSE
        TOTEFFIRR = TOTIR
      ENDIF

      WRITE (LUNWBL,320)
  320 FORMAT(/,'!',5X,'WATER BALANCE PARAMETERS',
     &       /,'!',5X,'========================',T48,'--mm--')
      WRITE (LUNWBL,400)
     &                   YR1, DY1, TSWINI,
     &                   YR2, DY2, TSW, 
     &                   TOTEFFIRR,
     &                   CRAIN, 
     &                   CumLatFlow,
     &                   TDRAIN, TRUNOF,
     &                   CES, CEP, CEO
  400 FORMAT(
     &    /,'!',5X,'Soil H20 (start) on Year/day',I5,'/',I3.3,T44,F10.2,
     &    /,'!',5X,'Soil H20 (final) on Year/day',I5,'/',I3.3,T44,F10.2,
     &    /,'!',5X,'Effective Irrigation',         T44,F10.2,
     &    /,'!',5X,'Precipitation',                T44,F10.2,
     &    /,'!',5X,'Net lateral flow',             T44,F10.2,
     &    /,'!',5X,'Drainage',                     T44,F10.2,
     &    /,'!',5X,'Runoff',                       T44,F10.2,
     &    /,'!',5X,'Soil Evaporation',             T44,F10.2,
     &    /,'!',5X,'Transpiration',                T44,F10.2,
     &    /,'!',5X,'Potential ET',                 T44,F10.2)

      WBALAN = TSWINI - TSW    !Change in water content
     &       + TOTEFFIRR + CRAIN +            !Inflows
     &       + CumLatFlow                     !Lateral flow
     &       - TDRAIN - TRUNOF - CES - CEP    !Outflows

      WRITE  (LUNWBL,500) WBALAN
  500 FORMAT(/,'!',5X,'Final Balance ',T42,F12.3,/)

      CLOSE(LUNWBL)       

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE Wbal_2D
C=======================================================================


C=====================================================================
!     Wbal_2D VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! CEO      Cumulative potential evapotranspiration from 0:00am to the end of this time step(mm)
! CEP      Cumulative transpiration from 0:00am to the end of this time step (mm)
! CES      Cumulative evaporation from 0:00am to the end of this time step (mm)
! CRAIN    Cumulative precipitation from 0:00am to the end of this time step (mm)
! CUMWBAL  Cumulative water balance
! DEFICIT  Amount by which the allowable minimum soil water content in top 
!            layer exceeds the actual calculated soil water content (cm3/cm3)
! DLAYR(L) Soil thickness in layer L (cm)
! Drain_Limit2D Drain water in mm
! DY2      Day of year
! EFFIRR   Irrigation application efficiency (cm/cm)
! ES       Actual soil evaporation rate (mm/d)
! EXPER    Experiment code (prefix of input files) 
! FIRST    Indicates first call to subroutine (true or false)
! IDETL    Switch for detailed printout (Y or N)
! IDETW    Y=detailed water balance output, N=no detailed output 
! ISWITCH  Composite variable containing switches which control flow of 
!            execution for model.  The structure of the variable 
!            (SwitchType) is defined in ModuleDefs.for. 
! ISWWAT   Water simulation control switch (Y or N)
! IRRAMT   Irrigated water amount in mm. It is averaged on ROWSpace
! LatFlow  Daily total lat flow for all cells. Inward is positive. It is averaged on RowSpace
! LL(L)    Volumetric soil water content in soil layer L at lower limit
!            (cm3/cm3)
! LUNWARN  Logical unit number for Warning.OUT file 
! LUNWBL   Logical unit number for WBAL.OUT file 
! MEINF    Method of infiltration 
! NL       Maximum number of soil layers = 20 
! NLAYR    Actual number of soil layers 
! RAIN     Precipitation in mm. Considered that the rain on the plastic run to furrow. 
!          Thus the infiltration amount is WATAVL = RAIN * HalfRow / HalfFurrow      !mm
! RUNOFF   Run off water in mm
! RWUbed   Root uptake from bed
! RWUubd   Root uptake from under-bid
! RWUfur   Root uptake from furrow
! SAT(L)   Volumetric soil water content in layer L at saturation
!            (cm3 [water] / cm3 [soil])
! SW(L)    Volumetric soil water content in layer L
!            (cm3 [water] / cm3 [soil])
! SWEF     Soil water evaporation fraction; fraction of lower limit content 
!            to which evaporation can reduce soil water content in top layer
!            (fraction)
! TDRAIN   Cumulative daily drainage from profile (mm)
! TES      Total actual soil evaporation in current time step (mm)
! TEP      Total potential soil evaporation in current time step(mm)               
! TOTIR    Total seasonal irrigation (mm)
! TRUNOF   Cumulative runoff (mm)
! TSW      Total soil water in profile (mm)
! TSWY      Total soil water in profile Yesterday(mm)
! TSWINI   Initial soil water content (mm)
! TOTEFFIRR Total efficient irrigation water
! WBALAN   Seasonal water balance (should equal 0.0) (mm)
! YRDOY    Current day of simulation (YYDDD)
! YRSIM    Start of simulation date (YYDDD)
! YR2      Year
!-----------------------------------------------------------------------
!     END SUBROUTINE Wbal_2D
C=======================================================================

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
     &    CELLS, Drain_Limit2D, HalfRow, RAIN, RUNOFF, SWV,       !Input
     &    CES, CEP, CRAIN, TDRAIN, TES, TEP, TRUNOF,      !Output
     &    TSW, TSWINI)                                    !Output

!-----------------------------------------------------------------------
      USE Cells_2D
      IMPLICIT NONE
      SAVE

      INTEGER DYNAMIC, i, j
      REAL CRAIN, Drain_Limit2D, CES, CEP, HalfRow, RAIN, RUNOFF
      REAL TDRAIN, TEP, TES, TRUNOF, TSW, TSWINI
      REAL, DIMENSION(MaxRows,MaxCols) :: CellArea
      REAL, DIMENSION(MaxRows,MaxCols) :: SWV
      
      TYPE (CellType), DIMENSION(MaxRows,MaxCols) ::  CELLS
      
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
      CES    = 0.
      CEP    = 0.
      CellArea = CELLS%STRUC%CellARea
      ! Jin Wu New: we should use NLAYR instead of NRowTot?
      DO i = 1, NRowsTot
        DO j = 1, NColsTot
          SELECT CASE(CELLS(i,j)%Struc%CellType)
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
      TES = 0.0
      DO i = 1, NRowsTot
        DO j = 1, NColsTot
          SELECT CASE(CELLS(i,j)%Struc%CellType)
          CASE (3,4,5) 
            TSW = TSW + SWV(i,j) * CellArea(i,j) / HalfRow * 10.
            TEP = TEP + CELLS(i,j)%Rate%EP_Rate
            TES = TES + CELLS(i,j)%Rate%ES_Rate
          END SELECT
        ENDDO
      ENDDO

!-----------------------------------------------------------------------
C     Increment summation variables.
!-----------------------------------------------------------------------
      CRAIN  = CRAIN  + RAIN
      if (BedDimension % LIMIT_2D .GE. NRowsTot) 
     &    TDRAIN = TDRAIN + Drain_Limit2D
      TRUNOF = TRUNOF + RUNOFF
      CES    = CES    + TES
      CEP    = CEP    + TEP

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE WBSUM_2D

!-----------------------------------------------------------------------
!     WBSUM_2D VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! CEO      Cumulative potential evapotranspiration from 0:00 am to the end of this time step in mm
! CEP      Cumulative transpiration from simulation start day to current day (mm)
! CES      Cumulative evaporation from simulation start day to current day(mm)
! CRAIN     Cumulative precipitation from 0:00 am to the end of this time step (mm)
! DLAYR(L)  Soil thickness in layer L (cm)
! Drain_Limit2D Drain from LIMIT_2D  
!               Drainage rate from soil profile in current time step(mm/d) if there is no water table 
! NL        Maximum number of soil layers = 20 
! NLAYR     Actual number of soil layers 
! RAIN      Precipitation depth for current day (mm)
! RUNOFF    Calculated runoff (mm/d)
! SWV(Row, Col)     Volumetric soil water content in cell
!             (cm3 [water] / cm3 [soil])
! TDRAIN    Cumulative daily drainage from profile (mm)
! TRUNOF    Cumulative runoff (mm)
! TSW       Total soil water in profile (mm)
! TSWINI    Initial soil water content (mm)
!-----------------------------------------------------------------------
!     END SUBROUTINE WBSUM_2D
C=======================================================================

!=====================================================================
!  Wbal_Sep, Subroutine, Cheryl Porter

!  Daily water balance for 2-D drip irrigation model
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
          SELECT CASE(CELLS(i,j)%STRUC%CellType)
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

