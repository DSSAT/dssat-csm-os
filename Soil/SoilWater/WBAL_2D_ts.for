!=====================================================================
!  Wbal_2D_ts, Subroutine, Gerrit Hoogenboom
!  Modified for 2-D drip irrigation model
!  Seasonally: Provides output Water balance.  Prints file SoilWat_ts.OUT
!  Data is obtained from WATBAL, SPAM and IRRIG modules daily.  
!  Data from SPAM and IRRIG are sent via GETPUT routines.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  09/05/2008 CHP adapted WBAL for 2-D model 
!  08/25/2009 CHP modified for sub-daily time step
!  08/15/2011 Make detail available for cell(1,DripCol) and for cell(FurRow1,j), add INF_vol for cell detail
!             Add handling of LIMIT_2D for WBALAN
!             For 1st timestep, LatFlow include the portion which is calculated in WaterTable_2D
!-----------------------------------------------------------------------
!  Called by: WATBAL
!=====================================================================
      SUBROUTINE Wbal_2D_ts(CONTROL, ISWITCH, Time, TimeIncr,   !Input
     &    DRAIN, RUNOFF, IRRAMT, RAIN,                          !Input
     &    TES, TEP, TSW, CritCell, Diffus, Kunsat, LatFlow_ts,  !Input
     &    Count, LatFlow, SWV_D)                                !Input
!     ------------------------------------------------------------------
      USE Cells_2D

      USE ModuleData
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, YR_DOY
      SAVE

      CHARACTER*14, PARAMETER :: SWBAL = 'SoilWat_ts.OUT'
!      CHARACTER*19 SWCellBAL
      INTEGER DAS, DYNAMIC, LUNWBL, I, Count !, IDL, JJ
      INTEGER YRDOY
      INTEGER YR2, DY2, CritCell(2)

      REAL WBALAN, Time, TimeIncr, LatFlow_ts, LatFlow
      REAL NEXTTS  !TimeIncrCum, 
      REAL CUMWBAL, Diffus1, Kunsat1

      REAL, DIMENSION(MaxRows,MaxCols) :: Kunsat, Diffus

      LOGICAL FEXIST, DOPRINT

      integer detailRow, detailCol, PTFLG, MULTI !clun, 
      real MINTS
      integer, DIMENSION(MaxCells) :: rows, cols
      INTEGER DripCol(NDrpLn), DripRow(NDrpLn)

      Double Precision DRAIN, IRRAMT, RAIN, RUNOFF
      Double Precision TEP, TES, TSW, TSWY
      Double Precision, DIMENSION(MaxRows,MaxCols) :: SWV_D !, ep_vf, 
!     &          es_vf_ts, INF_vol_dtal
!      Double Precision IrrVol(NDrpLn)
      TYPE (ControlType)  CONTROL
      TYPE (SwitchType)   ISWITCH

!     ------------------------------------------------------------------
      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY
      DAS     = CONTROL % DAS
      DripCol = BedDimension % DripCol
      DripRow = BedDimension % DripRow
!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      DOPRINT=.TRUE.
      IF (ISWITCH % IDETW .EQ. 'N') THEN
        DOPRINT=.FALSE.
      ENDIF
      IF (ISWITCH % ISWWAT .EQ. 'N') THEN
        DOPRINT=.FALSE.
      ENDIF
      IF (ISWITCH % IDETL /= 'D') THEN
        DOPRINT=.FALSE.
      ENDIF
      IF (.NOT. DOPRINT) RETURN

!     Open output file SoilWat_ts.OUT
      CALL GETLUN(SWBAL, LUNWBL)
      INQUIRE (FILE = SWBAL, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = LUNWBL, FILE = SWBAL, STATUS = 'OLD',
     &    POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = LUNWBL, FILE = SWBAL, STATUS = 'NEW')
        WRITE(LUNWBL,'("*WATER BALANCE OUTPUT FILE")')
      ENDIF

      CALL HEADER(SEASINIT, LUNWBL, CONTROL % RUN)

!     Write header for daily output
      WRITE (LUNWBL,1120,ADVANCE='NO')
 1120 FORMAT('@YEAR DOY   DAS   TIME   INCR  Diffus  Kunsat  Row  Col',
     & '      SWTD',                               !State vars
     & '     IRRD     PRED     LAFD',              !Inflows
     & '     DRND     ROFD     ESAD     EPAD',     !Outflows
     & '     WBAL    CUMWBAL')                     !Balance

!     Soil water content for 1D simulations
      IF (NColsTot == 1) THEN
        WRITE(LUNWBL,1121) ("SW",I,"T",I=1,NRowsTot)
 1121   FORMAT(50(5X,A2,I2.2,A1))
      ELSE
        WRITE(LUNWBL,'(" ")')
      ENDIF

      TSWY   = TSW
      CUMWBAL = 0.0

      CALL YR_DOY(YRDOY, YR2, DY2)
      WRITE (LUNWBL,1300,ADVANCE='NO') YR2, DY2, DAS, Time, TimeIncr, 
     &    0.0, 0.0, 0, 0,
     &    TSW,                                     !State variables
     &    0.0, 0.0, 0.0,                           !Inflows
     &    0.0, 0.0, 0.0, 0.0,                      !Outflows
     &    0.0, CUMWBAL                             !Balance
      
!     Soil water content for 1D simulations
      IF (NColsTot == 1) THEN
          WRITE(LUNWBL,'(50F10.4)') (SWV_D(I,1),I=1,NRowsTot)
      ELSE
        WRITE(LUNWBL,'(" ")')
      ENDIF

!     ------------------------------------------------------------------
!     debug chp
!     water balance for single cell as defined in Cell_detail
      detailRow = Cell_detail%Row
      detailCol = Cell_detail%Col
      PTFLG     = Cell_detail%WPTFLG
      MULTI     = Cell_detail%MULTI
      MINTS     = Cell_detail%MINTS
      rows      = Cell_detail%rows
      cols      = Cell_detail%cols
      NEXTTS    = Cell_detail%NEXTTS

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
      IF (.NOT. DOPRINT) RETURN
!       Change in storage = Inflows - Outflows
!       Balance = Inflows - Outflows - Change in storage
        if (BedDimension % LIMIT_2D .GE. NRowsTot) then 
          WBALAN = 
     &         + IRRAMT + RAIN + LatFlow_ts      !Inflows
     &         - DRAIN - RUNOFF - TES - TEP   !Outflows
     &         - (TSW - TSWY)                 !Change in soil water 
        else ! Drain is part of LatFlow_ts
          WBALAN = 
     &         + IRRAMT + RAIN + LatFlow_ts      !Inflows
     &         - RUNOFF - TES - TEP   !Outflows
     &         - (TSW - TSWY)                 !Change in soil water 
        Endif
!       for 1st timestep, LatFlow include the portion which is calculated in WaterTable_2D
        If (Count .eq. 1)  WBALAN =  WBALAN - LatFlow_ts + LatFlow
        CUMWBAL = CUMWBAL + WBALAN

!       CritCell is the cell that controls the calculation of the minimum time 
!       step required to ensure stability.
        IF (CritCell(1) > 0 .and. CritCell(1) <= NRowsTot .and. 
     &      CritCell(2) > 0 .and. CritCell(2) <= NColsTot) THEN
          Diffus1 = Diffus(CritCell(1),CritCell(2))
          Kunsat1 = Kunsat(CritCell(1),CritCell(2))
        ELSE
          Diffus1 = Diffus(1,1)
          Kunsat1 = Kunsat(1,1)
        ENDIF

        CALL YR_DOY(YRDOY, YR2, DY2)
!        if ((CUMWBAL .LT. -1e-6 .OR. CUMWBAL .GT. 1e-6) .AND. CUMWBAL .EQ. 0) then
        if (Count .eq. 1) then 
          WRITE (LUNWBL,1300,ADVANCE='NO') YR2, DY2, DAS, Time,TimeIncr,
     &      Diffus1, Kunsat1, 
     &      CritCell(1), CritCell(2),
     &      TSW,                                        !State variables
     &      IRRAMT, RAIN, LatFlow,                      !Inflows
     &      DRAIN, RUNOFF, TES, TEP,                    !Outflows
     &      WBALAN, CUMWBAL                             !Balance
        else
          WRITE (LUNWBL,1300,ADVANCE='NO') YR2, DY2, DAS, Time,TimeIncr,
     &      Diffus1, Kunsat1, 
     &      CritCell(1), CritCell(2),
     &      TSW,                                        !State variables
     &      IRRAMT, RAIN, LatFlow_ts,                      !Inflows
     &      DRAIN, RUNOFF, TES, TEP,                    !Outflows
     &      WBALAN, CUMWBAL                             !Balance 
        endif
!        endif
 1300  FORMAT
     &    (1X,I4,1X,I3.3,1X,I5,2F7.3,   !Time
     &    2F8.1,                        !D, K
     &    2I5,                          !CritCells
     &    F10.4,                        !TSW
     &    3F9.4,                        !Inflows
     &    4F9.4,                        !Outflows
     &    F9.4,F11.4)                   !Balances, TSRadFrac

!       Soil water content for 1D simulations
        IF (NColsTot == 1) THEN
          WRITE(LUNWBL,'(50F10.4)') (SWV_D(I,1),I=1,NRowsTot)
        ELSE
          WRITE(LUNWBL,'(" ")')
        ENDIF

        !Save values for comparison tomorrow
        TSWY   = TSW

!***********************************************************************
!***********************************************************************
!     SEASEND - Seasonal output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
      IF (.NOT. DOPRINT) RETURN

      CLOSE(LUNWBL)    
!      close(clun)   

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE Wbal_2D_ts
C=======================================================================
C=====================================================================
!     Wbal_2D_ts VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! CritCell(2) The # of row and # of Col of cell which require smallest time step
! Diffus  Diffusivity
! DRAIN   Drain water from the bottom of simulation depth
! EP_vf(Row,Col) In a time step, cell actual plant transpiration rate volumetric fraction in mm/mm
! ES_vf_ts(Row,Col) In a time step, cell soil evaporation volumetric fraction in mm/mm
! H_in(Row,Col)    2D Horizontal water amount into cell in current time step in cm2
! H_out(Row,Col)   2D Horizontal water amount out of cell in current time step in cm2
! INF_vol_dtal INF_vol for cell detail
! IRRAMT  Irrigation amount (mm) 
! RAIN
! RUNOFF   Run off water in mm
! SWijcm2  Total cell water in cm2
! TEP     Total potential root uptake water in current time step
! TES     Total soil evaporation in current time step(mm)
! Time    Time of the day in hour
! TimeIncr length of time step
! TSW     Total soil water in profile (cm?)
!-----------------------------------------------------------------------
!     END SUBROUTINE Wbal_2D_ts
!=======================================================================
