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
     &    Count, LatFlow,                                       !Input
!         Temp chp
     &    CellArea, SWV_D, EP_vf, ES_vf_ts, IrrVol, INF_vol_dtal) !Input

!      SUBROUTINE Wbal_2D_ts(CONTROL, ISWITCH, Time, TimeIncr,   !Input  real, real
!     &    DRAIN, RUNOFF, IRRAMT, RAIN,                          !Input  dp, dp, dp, dp
!     &    TES, TEP, TSW, CritCell, Diffus, Kunsat, LatFlow_ts,  !Input  dp, dp, dp, int(2), real(r,c), real(r,c), real
!     &    Count, LatFlow,                                       !Input  int, real
!     &    CellArea, SWV_D, EP_vf, ES_vf_ts, IrrVol, INF_vol_dtal) !Input real(r,c), dp(r,c), dp(r,c), dp(r,c), dp(nd), dp(r,c)
!     ------------------------------------------------------------------
!      USE ModuleDefs 
      USE Cells_2D  !temp chp

      USE ModuleData
      IMPLICIT NONE
      SAVE

      CHARACTER*14, PARAMETER :: SWBAL = 'SoilWat_ts.OUT'
      CHARACTER*19 SWCellBAL
      INTEGER DAS, DYNAMIC, LUNWBL, I, Count, IDL, JJ
      INTEGER YRDOY
      INTEGER YR2, DY2, CritCell(2)

      REAL WBALAN, Time, TimeIncr, LatFlow_ts, LatFlow
      REAL TimeIncrCum, NEXTTS
      REAL CUMWBAL, Diffus1, Kunsat1

      Double Precision, DIMENSION(MaxRows,MaxCols) :: WCellCBal
      REAL, DIMENSION(MaxRows,MaxCols) :: Kunsat, Diffus

      LOGICAL FEXIST, DOPRINT

      integer clun, detailRow, detailCol, PTFLG, MULTI
      real MINTS
      integer, DIMENSION(MaxCells) :: rows, cols
      INTEGER DripCol(NDrpLn), DripRow(NDrpLn)
      Double Precision swij, swijcm2
      Double Precision rwuij, wbalij, esij
      REAL, DIMENSION(MaxRows,MaxCols) :: CellArea
      Double Precision, DIMENSION(MaxRows,MaxCols) :: swijcm2y,vOutijCum,hOutijCum,RWUijCum, ESijCum
      INTEGER L, J

      Double Precision DRAIN, IRRAMT, RAIN, RUNOFF
      Double Precision TEP, TES, TSW, TSWY
      Double Precision, DIMENSION(MaxRows,MaxCols) :: SWV_D, ep_vf, es_vf_ts, INF_vol_dtal
      Double Precision IrrVol(NDrpLn)
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
!     temp chp
!     water balance for single cell as defined in Cell_detail
      detailRow = Cell_detail%Row
      detailCol = Cell_detail%Col
      PTFLG     = Cell_detail%WPTFLG
      MULTI     = Cell_detail%MULTI
      MINTS     = Cell_detail%MINTS
      rows      = Cell_detail%rows
      cols      = Cell_detail%cols
      NEXTTS    = Cell_detail%NEXTTS

      
      IF (CONTROL%TRTNUM < 10) THEN
          write (SWCellBAL, '("CellDetailW_", I1, ".OUT")') CONTROL%TRTNUM
      ELSE IF (CONTROL%TRTNUM < 100) THEN
          write (SWCellBAL, '("CellDetailW_", I2, ".OUT")') CONTROL%TRTNUM
      ELSE
          write (SWCellBAL, '("CellDetailW_", I3, ".OUT")') CONTROL%TRTNUM
      END IF
      CALL GETLUN(SWCellBAL, CLun)
      OPEN (UNIT = CLun, FILE = SWCellBAL, STATUS = 'REPLACE')
      WRITE(CLun,'("*WATER BALANCE FOR CELL(",I2,",",I2,")")') 
     &      Cell_detail%row, Cell_detail%col
      CALL HEADER(SEASINIT, CLun, CONTROL % RUN)
      WRITE (CLun,1130)
 1130 FORMAT('@YEAR DOY   DAS   TIME   INCR ROW COL',
     & '       SWV',                               !State vars
!     & '      H_in      V_in',                     !Inflows
!     & '     H_out     V_out     RWUij      ESij', !Outflows
!     & '      WBAL   CUMWBAL',                     !Balance
!     & ' SW(i-1,j) SW(i,j-1)   SW(i,j) SW(i,j+1) SW(i+1,j)', !Extra info
     & '     H_cum     V_cum   RWU_cum    ES_cum') !Cumulative info
!     & '    Diffus',           !Extra info
!     & '    Kunsat     VoutD     VoutG') !Extra info
     
      JJ = 1
      DO L = 1, NRowsTot
        DO J = 1, NColsTot
          IF (L .LT. BedDimension%FurRow1 .AND. J .GE. BedDimension%FurCol1) CYCLE
          
          IF (PTFLG .EQ. 5) THEN
              IF (JJ .GT. MAXCELLS) CYCLE
              IF (L .NE. rows(jj) .OR. J .NE. cols(jj)) CYCLE
          END IF

          SWijcm2  = SWV_D(L, J)*CellArea(L, J)   !cm2
          SWij     = SWV_D(L, J)

          IF ((detailRow .EQ. L .AND. detailCol .EQ. J) .OR. 
     &         (L .LE. detailRow + MULTI .AND. L .GE. detailRow - MULTI .AND. PTFLG .EQ. 1) .OR.
     &         (J .LE. detailCol + MULTI .AND. J .GE. detailCol - MULTI .AND. PTFLG .EQ. 2) .OR.
     &         PTFLG .EQ. 3 .OR. PTFLG .EQ. 5) THEN
            WRITE (CLUN,1320) YR2, DY2, DAS, 0.0, 0.0, L, J,
     &            SWV_D(L, J),                !State variables
!     &            0.0, 0.0,                   !Inflows
!     &            0.0, 0.0, 0.0, 0.0,         !Outflows
!     &            0.0, 0.0,                   !Balance
     &            0.0, 0.0, 0.0, 0.0          !Cumulative
!     &            0.0, 0.0, 0.0, 0.0          !Extras
          END IF

          SWijcm2y(L, J) = SWijcm2
          JJ = JJ + 1

        END DO
      END DO
      WCellCBal = 0.0
      vOutijCum = 0
      hOutijCum = 0
      RWUijCum = 0
      ESijCum = 0
      TimeIncrCum = 0

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
        ! for 1st timestep, LatFlow include the portion which is calculated in WaterTable_2D
        If (Count .eq. 1)  WBALAN =  WBALAN - LatFlow_ts + LatFlow
        CUMWBAL = CUMWBAL + WBALAN

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

!     --------------------------------------------------------------------
!     temp chp
!     Water balance detail for one cell in cm2
      DO IDL = 1, NDripLnTOT
        L = DripRow(IDL)
        J = DripCol(IDL)
        Cell_detail%v_in(L, J) = Cell_detail%v_in(L, J) + IrrVol(IDL)
        Cell_detail%IrrVol(L, J) = Cell_detail%IrrVol(L, J) + IrrVol(IDL)
      END DO
      JJ = 1
      TimeIncrCum = TimeIncrCum + TimeIncr
      DO L = 1, NRowsTot
        DO J = 1, NColsTot
          IF (PTFLG .EQ. 5) THEN
              IF (JJ .GT. MAXCELLS) CYCLE
              IF (L .NE. rows(jj) .OR. J .NE. cols(jj)) CYCLE
          END IF
          IF (L .LT. BedDimension%FurRow1 .AND. J .GE. BedDimension%FurCol1) CYCLE

!         Within furrow, add infiltration to top cells
          Cell_detail%v_in(L, J) = Cell_detail%v_in(L, J) + INF_vol_dtal(L, J) * CellArea(L, J)
          Cell_detail%InfVol(L, J) = Cell_detail%InfVol(L, J) + INF_vol_dtal(L, J) * CellArea(L, J)
          !INF_vol = WINF_col(j) * 0.1 * DayIncr / Thick(FurRow1,j)
!           cm3[water]   mm[water]   cm         1           
!           ---------- = --------- * -- * d * --------
!            cm3[soil]       d       mm       cm[soil]   
        
          if (BedDimension % LIMIT_2D . LT. L) then
            Cell_detail%v_in(L, J) = -999999
          Endif

          SWijcm2  = SWV_D(L, J)*CellArea(L, J)   !cm2
          SWij     = SWV_D(L, J) !cm3/cm3

          RWUij = ep_vf(L, J) * CellArea(L, J)
          ESij = es_vf_ts(L, J) * CellArea(L, J)
          if (abs(Cell_detail%v_in(L, J) - 999999) .LT. 0.00001) then 
            WBALij = -999999
          elseif (abs(Cell_detail%v_out(L, J) - 999999) .LT. 0.00001) then 
            WBALij = -999999
          else 
            WBALij = 
     &         + Cell_detail%h_in(L, J) + Cell_detail%v_in(L, J)     !Inflows
     &         - Cell_detail%h_out(L, J) - Cell_detail%v_out(L, J)   !Outflows
     &         - RWUij - ESij                                        !Outflows
     &         - (SWijcm2 - SWijcm2y(L, J))                          !Change in soil water
            WCellCBal(L, J) = WCellCBal(L, J) + WBALij 
            vOutijCum(L, J) = vOutijCum(L, J) + Cell_detail%v_out(L, J)
            hOutijCum(L, J) = hOutijCum(L, J) + Cell_detail%h_out(L, J)
            RWUijCum(L, J) = RWUijCum(L, J) + RWUij
            ESijCum(L, J) = ESijCum(L, J) + ESij
          endif

          IF ((PTFLG .EQ. 4 .AND. (WBALij .LT. -1e-6 .OR. WBALij .GT. 1e-6)) .OR.
     &        (PTFLG .EQ. 0 .AND. detailRow .EQ. L .AND. detailCol .EQ. J) .OR. 
     &        (PTFLG .EQ. 1 .AND. L .LE. detailRow + MULTI .AND. L .GE. detailRow - MULTI) .OR.
     &        (PTFLG .EQ. 2 .AND. J .LE. detailCol + MULTI .AND. J .GE. detailCol - MULTI) .OR.
     &        (PTFLG .EQ. 3 .AND. DAS .GE. cell_detail%start .AND. DAS .LE. cell_detail%fin) .OR. 
     &        (PTFLG .EQ. 5 .AND. DAS .GE. cell_detail%start .AND. DAS .LE. cell_detail%fin)) THEN
            IF (MINTS .LE. 0) THEN
              WRITE (CLUN,1320) YR2, DY2, DAS, Time, TimeIncr, L, J,
     &            SWV_D(L, J),                                             !State vars
!     &            Cell_detail%h_in(L, J), Cell_detail%v_in(L, J),      !Inflows
!     &            Cell_detail%h_out(L, J), Cell_detail%v_out(L, J),    !Outflows
!     &            RWUij, ESij,                                         !Outflows
!     &            WBALij, WCellCBal(L, J),                             !Balance
     &            hOutijCum(L, J), vOutijCum(L, J),                    !Cumulative
     &            RWUijCum(L, J), ESijCum(L, J)                        !Cumulative
!     &            Diffus(L, J), Kunsat(L, J),                          !Extra
!     &            Cell_detail%vdiff(L, J), Cell_detail%vgrav(L, J)     !Extra
            ELSE IF((NEXTTS .NE. 24 .AND. ABS(Time - NEXTTS) .LE. 0.01) .OR.
     &              (Time .EQ. 24 .AND. NEXTTS .EQ. 24)) THEN
              WRITE (CLUN,1320) YR2, DY2, DAS, Time, TimeIncrCum, L, J,
     &            SWV_D(L, J),                                             !State vars
!     &            Cell_detail%h_in(L, J), Cell_detail%v_in(L, J),      !Inflows
!     &            Cell_detail%h_out(L, J), Cell_detail%v_out(L, J),    !Outflows
!     &            RWUij, ESij,                                         !Outflows
!     &            WBALij, WCellCBal(L, J),                             !Balance
     &            hOutijCum(L, J), vOutijCum(L, J),                    !Cumulative
     &            RWUijCum(L, J), ESijCum(L, J)                        !Cumulative
!     &            Diffus(L, J), Kunsat(L, J),                          !Extra
!     &            Cell_detail%vdiff(L, J), Cell_detail%vgrav(L, J)     !Extra
            ELSE IF(Time .GT. NEXTTS) THEN
              WRITE (CLUN,1320) YR2, DY2, DAS, Time, TimeIncrCum, L, J,
     &            SWV_D(L, J),                                             !State vars
!     &            Cell_detail%h_in(L, J), Cell_detail%v_in(L, J),      !Inflows
!     &            Cell_detail%h_out(L, J), Cell_detail%v_out(L, J),    !Outflows
!     &            RWUij, ESij,                                         !Outflows
!     &            WBALij, WCellCBal(L, J),                             !Balance
     &            hOutijCum(L, J), vOutijCum(L, J),                    !Cumulative
     &            RWUijCum(L, J), ESijCum(L, J)                        !Cumulative
!     &            Diffus(L, J), Kunsat(L, J),                          !Extra
!     &            Cell_detail%vdiff(L, J), Cell_detail%vgrav(L, J)     !Extra
            END IF
          ENDIF

          SWijcm2y(L, J) = SWijcm2
          JJ = JJ + 1
        END DO
      END DO
      IF((NEXTTS .NE. 24 .AND. ABS(Time - NEXTTS) .LE. 0.01) .OR.
     &   (Time .EQ. 24 .AND. NEXTTS .EQ. 24)) THEN
        NEXTTS = NEXTTS + MINTS
        Cell_detail%NEXTTS = NEXTTS
        vOutijCum = 0
        hOutijCum = 0
        RWUijCum = 0
        ESijCum = 0
        TimeIncrCum = 0
      ELSE IF(Time .GT. NEXTTS) THEN
        DO WHILE (Time .GT. NEXTTS)
          NEXTTS = NEXTTS + MINTS
        END DO
        Cell_detail%NEXTTS = NEXTTS
        vOutijCum = 0
        hOutijCum = 0
        RWUijCum = 0
        ESijCum = 0
        TimeIncrCum = 0
      END IF
      IF (NEXTTS .GE. 24) THEN
        NEXTTS = NEXTTS - 24
        Cell_detail%NEXTTS = NEXTTS
      END IF

!1320  FORMAT(I4,1X,1X,I3.3,1X,I5,1X,F6.2,1X,F6.1,1X,I3,1X,I3,9F10.6,F10.2,3F10.6)
1320  FORMAT(I4,1X,1X,I3.3,1X,I5,1X,F6.2,1X,F6.1,1X,I3,1X,I3,9F10.6,4(1X,F10.5))
!***********************************************************************
!***********************************************************************
!     SEASEND - Seasonal output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
      IF (.NOT. DOPRINT) RETURN

      CLOSE(LUNWBL)    
      close(clun)   

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
