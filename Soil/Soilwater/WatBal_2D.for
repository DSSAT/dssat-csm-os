!=======================================================================
!  WatBal2D, Subroutine
!  2D water balance for drip irrigation bed with
!     plastic mulch cover.  

!     Order of calculations:
!     -- accept daily irrigation inputs from management module
!     -- input potential daily root water extraction and hourly  
!           distribution
!     -- compute soil water dynamics,
!           including drip irrigation, redistribution, root water extraction, 
!           upflow and drainage on a variable time step. 
!     -- seasonal water balance computation:
!           change in overall water content = input - output

!-----------------------------------------------------------------------
!  REVISION HISTORY
!  08/25/2008 CHP Written
!  08/21/2009 CHP Change to sub-hourly, variable time step. Remove MGAR.
!  07/27/2010 CHP Drip irrigation emitter can be offset from centerline.
!                 Allow partial or full plastic cover on flat field.
!  03/01/2011 CHP and JZW, Add LIMIT_2D for water table handling
!                 If there is water table, daily LIMIT_2D will be defined
!                 the drainage below LIMIT_2D is 1D and Soil water are in equilibrum state.
!                 The root uptake and soil evaporation are 2D. Water table depth is in management level.               
!                 Horizental flow= drainage+Rootuptake+Horizental diffusion
!                 Set initial SWV as ThetaCap if there is water table
!                 Add ThetaCap output
!                 For conventional Irrigation, EFFIRR was counted repeatedly in StdIrrig
!                 For conventional Irrigation, add considering irrigaton water run from plastic cover to non-covered area              
!                 Rename RowFrac to ColFrac, UpFlow_2D to EvapFlow
!                 StdIrrig may need replace by IRRAMT
!                 Need to handle if both dripper and convension irrigtaion applied on same day 
!  02/01/2012 JZW Add the case: PMCover = false for potato calculation
!  03/13/2013 JZW, StrIrrig is for both dripper and non-dripper
!=======================================================================

      SUBROUTINE WatBal2D(CONTROL, ISWITCH, 
     &    EOP, IRRAMT, SOILPROP, SOILPROP_FURROW,   !Input
     &    WEATHER,                                  !Input
     &    Cells, SW, SWDELTS, TRWU, TRWUP)          !Output

!-----------------------------------------------------------------------
      USE Cells_2D
      USE ModuleData
      USE FloodModule 
      USE Interface_OPWBAL
      USE NFLUXts

      IMPLICIT NONE
      EXTERNAL WaterTable_2D, DRAINAGE_2D, ROOTWU_2D, 
     &  WBSUM_2D, CALC_SW_VOL, WBAL_2D_TS, 
     &  Rnoff_2D, INFO, K_UNSAT, DIFFUS_COEF, TIME_INTERVAL, 
     &  WATERSTRESS, WBAL, OpSW15min
      SAVE

      TYPE (ControlType), INTENT(IN) :: CONTROL
      TYPE (SwitchType) , INTENT(IN) :: ISWITCH
      REAL              , INTENT(IN) :: EOP, IRRAMT
!     TYPE (SoilType)   , INTENT(IN) :: SOILPROP, SOILPROP_furrow
      TYPE (SoilType)   , INTENT(INOUT) :: SOILPROP, SOILPROP_furrow
      TYPE (WeatherType), INTENT(IN) :: WEATHER
      REAL, DIMENSION(NL),INTENT(OUT):: SW, SWDELTS
      REAL              , INTENT(OUT):: TRWU, TRWUP
      Type (CellType) Cells(MaxRows,MaxCols)

      TYPE (DripIrrType) :: DripIrrig(NDrpLn)
      CHARACTER*8, PARAMETER :: ERRKEY = 'WATBAL2D'
      CHARACTER*12 TEXTURE(NL)
      CHARACTER*78 MSG(30)
      INTEGER DripCol,DripRow,DYNAMIC,FurRow1, FurCol1
      INTEGER i, j, jj,idl, row, col
     
      Integer DripNumTot, iHr
      INTEGER NLAYR, HR, IrrigIndex, LIMIT_2D
      INTEGER DripNumTotArr(NDrpLn), IrrIdxArr(NDrpLn)
!     INTEGER, DIMENSION(MaxRows,MaxCols) :: Cell_Type

!     debug chp
      INTEGER NextUpdate, Count, CritCell(2), DAYCOUNT

      REAL CN, BEDHT, BEDWD, CRAIN
      REAL DRAIN_2D, DRAIN_2D_Y, HalfRow, HalfFurrow, SolProfDrain
      REAL DripSpc(NDrpLn), DripOfset(NDrpLn), DripDep(NDrpLn)
      REAL RAIN, RUNOFF, Runoff_day
      REAL TEP, SRAD_TOT
      REAL TDRAIN, TRUNOF, TSW, TSWINI, TSW_cm
!     INTEGER, PARAMETER :: MaxNEvent = 20  !Max number of irrigation event
!     REAL, DIMENSION(MaxNEvent) :: DripStart, DripDur, DripInt, DripRate
      REAL IrrRate(NDrpLn), RWUEP1, Drainage_ts_col

      REAL TimeIncr, MinTimeIncr, ROWSPC_cm, DayIncr
      REAL StartTime, EndTime, DeltaT
      REAL CumRad, LastCumRad, TSRadFrac, SUM_TSRF

      REAL SWFAC,  SWFAC_ts,  SWFAC_day
      REAL TURFAC, TURFAC_ts, TURFAC_day
      REAL ActWTD, MgmtWTD, netLatFlow, LatFlow_ts !, MaxDif
      REAL StdIrrig, WidTot, DepTot, LatFlow, SurfaceVal, SumLatFlow
      REAL Excess_vf, Excess_mm
      
      REAL, DIMENSION(0:24) :: EOP_HR, CumFracRad
      REAL, DIMENSION(NL) :: BD, DLAYR, DS, DUL, Ksat, LL, SAT, WCr
      REAL, DIMENSION(NL) :: alphaVG, mVG, nVG
      REAL, DIMENSION(NL) :: RWU

      REAL, DIMENSION(0:MaxCols) :: PMFRACTION
      REAL, DIMENSION(MaxCols) :: WINF_col, Drain_col, Runoff_col
      REAL, DIMENSION(MaxRows,MaxCols) :: ES_mm
      REAL, DIMENSION(MaxRows,MaxCols) :: RLV_2D, mm_2_vf
      REAL, DIMENSION(MaxRows,MaxCols) :: CellInf, CellDrip
      REAL, DIMENSION(MaxRows,MaxCols) :: RWU_2D, RWU_2D_frac
      REAL, DIMENSION(MaxRows,MaxCols) :: SWFv_ts, SWFlux_L, SWFlux_R
      REAL, DIMENSION(MaxRows,MaxCols) :: SWFlux_D, SWFlux_U
      REAL, DIMENSION(MaxRows,MaxCols) :: SWFh_ts, RWUP_2D, Se
      REAL, DIMENSION(MaxRows,MaxCols) :: Thick, Width, Kunsat, Diffus
      REAL, DIMENSION(MaxRows,MaxCols) :: EvapFlow
      REAL, ALLOCATABLE :: IrrigSched(:,:,:), DripRate(:,:),DripInt(:,:)
!     REAL, ALLOCATABLE :: DripDep(:,:), DripStart(:,:), DripDur(:,:)

      Double Precision DRAIN_ts, EOP_ts, ES_avg, ES_day, ES_ts
      Double Precision INF_vol, IRR_ts, IrrVol(NDrpLn), Rain_ts
      Double Precision IrrVol_temp(NDrpLn), Runoff_ts  !chp
      Double Precision TRWU_ts,TRWUP_ts,SW_VOL_tot

      REAL, DIMENSION(MaxRows,MaxCols) :: SWV
      Double precision, DIMENSION(MaxRows,MaxCols) :: SWV_D, SWV_avail
      Double precision, DIMENSION(MaxRows,MaxCols) :: SWV_ts, RWU_2D_ts
      Double precision, DIMENSION(MaxRows,MaxCols) :: RWUP_2D_ts, EP_vf,
     &       ES_vf_ts, INF_vol_dtal
      Double precision, DIMENSION(MaxRows,MaxCols) :: INF_vol_dtal_temp
!     Double precision, DIMENSION(MaxRows,MaxCols) :: SWV_LAST
      Double Precision, DIMENSION(MaxRows,MaxCols,0:24) :: ES_Hr

!     Functions
      REAL Time_interval, k_unsat, diffus_coef

      LOGICAL IRRIG, IRRIGArr(NDrpLn)

!     Needed for generic water balance routine, but not actually used for 2D
      TYPE (FloodWatType) FLOODWAT
      TYPE (Mulchtype) MULCH
      REAL SNOW, TDFC, TDFD

!     Default time steps durring irrigation and drying !minutes
      REAL, PARAMETER :: TSI = 5.0, TSN = 30.0, Max_Time_Step=60.
!                         irrig        rain        default  

!     debug chp
      Cell_detail%row = 7 !FurRow1
      Cell_detail%col = 9 !FurCol1

      DYNAMIC = CONTROL % DYNAMIC
      RAIN    = WEATHER % RAIN
      IF (DYNAMIC .EQ. RUNINIT) THEN
!         call SW_SensorH(SOILPROP, CONTROL, Cells, SWV, 0)
!         call SW_SensorD(SOILPROP, CONTROL, Cells, SWV)

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      CN    = SOILPROP_furrow % CN
      DUL   = SOILPROP % DUL
      BD    = SOILPROP % BD
      DLAYR = SOILPROP % DLAYR
      DS    = SOILPROP % DS
      Ksat  = SOILPROP % SWCN   !cm/hr
      LL    = SOILPROP % LL
      NLAYR = SOILPROP % NLAYR
      SAT   = SOILPROP % SAT
      WCR   = SOILPROP % WCR
      alphaVG=SOILPROP % alphaVG
      mVG   = SOILPROP % mVG
      nVG   = SOILPROP % nVG
      TEXTURE = SOILPROP % TEXTURE
      SolProfDrain = 0.0

      CALL GET('PLANT', 'RWUEP1', RWUEP1)

!     PMFraction is the fraction of the soil covered by plastic mulch
!     PMFraction(0) is the entire row. PMFraction(J) is for each column of soil.
      CALL GET("SPAM", "PMFRACTION", PMFRACTION, MaxCols+1)

      BEDHT = BedDimension % BEDHT
      BEDWD = BedDimension % BEDWD
      ROWSPC_cm = BedDimension % ROWSPC_cm
      ! Jin Wu add in Feb. 2011
      BedDimension % LIMIT_2D = MaxRows + 10
      LIMIT_2D = BedDimension % LIMIT_2D
      FurRow1 = BedDimension % FurRow1
      FurCol1 = BedDimension % FurCol1
      mm_2_vf = BedDimension % mm_2_vf
      ColFrac = BedDimension % ColFrac
      HalfRow = ROWSPC_cm / 2. !half row width is modeled
      HalfFurrow = HalfRow - BEDWD / 2.

      CellArea = CELLS%STRUC%CellArea
      Thick    = CELLS%STRUC%Thick
      Width    = CELLS%STRUC%Width
      Cell_Type = CELLS % Struc % Cell_Type

!     Drip irrigation
      CALL GET(DripIrrig)
      DripOfset = DripIrrig % DripOfset
      DripDep   = DripIrrig % DripDep

!     Determine which column receives drip irrigation
      BedDimension % DripCol = -99
      BedDimension % DripRow = -99
      DO IDL = 1, NDripLnTOT
!       default drip emitter located at center of bed
        BedDimension % DripCol(IDL) = 1 
        BedDimension % DripRow(IDL) = 1
        WidTot = 0.
        DO j = 1, FurCol1-1
          WidTot = WidTot + Width(1,j)
          IF (WidTot > DripOfset(IDL)) THEN
            BedDimension % DripCol(IDL) = j
            EXIT
          ENDIF
        ENDDO
        DepTot = 0.
        DO i = 1, NRowsTot
          DepTot = DepTot + Thick(i,1)
          IF (DepTot > DripDep(IDL)) THEN
            BedDimension % DripRow(IDL) = i
            EXIT
          ENDIF
        ENDDO
      ENDDO

      SWV = CELLS % STATE % SWV_INIT

!     Set new water table level for today. This redefines Limit_2D,
!       the soil layer below which 1D saturated conditions exist.
      CALL WaterTable_2D(DYNAMIC, 
     &  CELLS, SOILPROP,                        !Input
     &  SW, SWV,                                !Input/Output
     &  ActWTD, netLatFlow, MgmtWTD, LIMIT_2D)  !Output

!     convert to double precision for time step loops
      SWV_D = DBLE(SWV)
      SWV_avail = SWV

      !CALL ArrayHandler(CELLS, CONTROL, SOILPROP, SWV, "SWV", 0.0, 0.5)

      CELLS%STATE%SWV = SWV
      CELLS%RATE%ES_Rate = 0.0
      CELLS%RATE%EP_Rate = 0.0

      CALL Drainage_2D(SEASINIT,  
     &    CELLS, Diffus, FurCol1, Kunsat,             !Input
     &    SOILPROP, SWV_D, TimeIncr, WCr,             !Input
     &    SWV_ts, SWFh_ts, SWFv_ts)                   !Output

      CALL ROOTWU_2D(SEASINIT, TimeIncr, 
     &    Cells, EOP_ts, SWV_avail,                       !Input 
     &    RWU_2D_ts, RWUP_2D_ts, TRWU_ts, TRWUP_ts)       !Output
     
!     Initialize summary variables
      CALL WBSUM_2D(SEASINIT,
     &    CELLS, DRAIN_2D, HalfRow, RAIN, RUNOFF, SWV,    !Input
     &    CRAIN, TDRAIN, TEP, TRUNOF,                     !Output
     &    TSW, TSWINI)                                    !Output

      CALL Interpolate2Layers_2D(                    
     &  CELLS%State%SWV, CELLS%Struc, SOILPROP%NLAYR,     !Input
     &  SW)                                               !Output

!     Call OPWBAL to write headers to output file
      CALL OPWBAL(CONTROL, ISWITCH, 
     &    CRAIN, DLAYR, IRRAMT,                       !Input
     &    netLatFlow, LL, NLAYR,                      !Input
     &    RUNOFF, SOILPROP, SW, TDRAIN, TRUNOF)       !Input

      SWDELTS= 0.0
      EOP_Hr = 0.0
      ES_Hr = 0.0
    
      IRRIGArr = .FALSE.

      Drain_ts = 0.0
      Runoff_ts = 0.0
      Irr_ts = 0.0
      Rain_ts = 0.0
      ES_ts = 0.0
      TRWU_ts = 0.0
      EP_VF = 0.0
      IrrVol = 0.d0
      LatFlow_ts = 0.0
      LatFlow = 0.0
      DRAIN_2D = 0.0
      DRAIN_2D_Y = 0.0

      Diffus = 0.0
      Kunsat = 0.0

      Call Calc_SW_Vol(
     &  CellArea, Cell_Type, HalfRow, SWV_D,                !Input
     &  SW_vol_tot)                                         !Output

!      call SW_SensorH(SOILPROP, CONTROL, Cells, SWV, 0)
!      call SW_SensorD(SOILPROP, CONTROL, Cells, SWV)

!     chp 2022-07-10
      INF_vol_dtal_temp = 0.0

!     In 2D model, TSW units are mm. 1D model uses cm.
      TSW_cm = TSW / 10.
      CALL Wbal(CONTROL, ISWITCH, 
     &    CRAIN, DRAIN_2D, FLOODWAT, netLatFlow,
     &    IRRAMT, MULCH, RAIN, RUNOFF, SNOW,  
     &    TDFC, TDFD, TDRAIN, TRUNOF, TSW_cm)

!     Output to SoilWat_ts.OUT and CellDetail.OUT
      CALL Wbal_2D_ts(CONTROL, ISWITCH, EndTime, TimeIncr, !Input
     &    DRAIN_ts, RUNOFF_ts, IRR_ts, RAIN_ts,              !Input
     &    ES_TS, TRWU_ts, SW_vol_tot, CritCell,              !Input
     &    Diffus, Kunsat, LatFlow_ts, Count, LatFlow,        !Input
     &    SWV_D)

      CALL OpSW15min(CONTROL, ISWITCH, 
     &    CELLS, EndTime, TimeIncr, SWV_D)  !Input

!     ------------------------------------------------------------------

      msg(1) = "Start 2D, variable time-step model"
      call info(1, ERRKEY, msg)

!     debug chp
      DAYCOUNT = 0

      !call SW_SensorH(SOILPROP, CONTROL, Cells, SWV, 0)
 !     call SW_SensorD(SOILPROP, CONTROL, Cells, SWV)

!     Needed for generic water balance routine, but not actually used for 2D
      SNOW = 0.0
      TDFC = 0.0
      TDFD = 0.0
      FLOODWAT % FLOOD = 0.0
      FLOODWAT % EF = 0.0
      FLOODWAT % CEF = 0.0

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
      IF (ISWITCH%ISWWAT == 'N') RETURN

      RLV_2D = CELLS % STATE % RLV

      DRAIN_2D_Y = DRAIN_2D
      DRAIN_2D = 0.0
      RWU_2D = 0.0
      RWUP_2D = 0.0
      SWFlux_L = 0.0
      SWFlux_R = 0.0
      SWFlux_D = 0.0
      SWFlux_U = 0.0
      TRWU = 0.0
      TRWUP = 0.0
      LatFlow_ts = 0.0
      LatFlow = 0.0
      SumLatFlow = 0.0

      SWFAC  = 0.0
      TURFAC = 0.0
      SUM_TSRF = 0.0
      CellInf = 0.0
      CellDrip = 0.0

      Cell_detail%IrrVol = 0
      Cell_detail%InfVol = 0

!     Compute water available at beginning of day
      SWV_avail = SWV_D

!     Convert soil evaporation to volumetric fraction units, 
      ES_mm = CELLS%Rate%ES_rate

!     Evaporation flux starts at bottom layer and accumulates up
      DO j = 1, NColsTot
        EvapFlow(NRowsTot,j) = ES_mm(NRowsTot,j)/10. * Width(NRowsTot,j)
!          cm2            =          cm        *    cm
        DO i = NRowsTot-1, 1, -1
          EvapFlow(i,j) = EvapFlow(i+1,j) + 
     &                         ES_mm(i,j) / 10. * Width(i,j)
        ENDDO
      ENDDO

!     Calculate a cumulative distribution function for solar radiation and apply to 
!     soil evaporation and plant transpiration.  Function is stored in CumFracRad and
!     goes from zero at the beginning of the day to 1.0 at the end of the day.
      SRAD_TOT = SUM(WEATHER%RADHR) 
      CumFracRad = 0.0
      DO i = 1, 24 ! Here the i refer to hour index in a day
        CumFracRad(i) = CumFracRad(i-1) + WEATHER%RADHR(i) / SRAD_TOT
      ENDDO

!     Set new water table level for today. This redefines Limit_2D,
!       the soil layer below which 1D saturated conditions exist.
      CALL WaterTable_2D(DYNAMIC, 
     &  CELLS, SOILPROP,                        !Input
     &  SW, SWV,                                !Input/Output
     &  ActWTD, netLatFlow, MgmtWTD, LIMIT_2D)  !Output

      SWV_avail = DBLE(SWV)
      SWV_D= SWV_avail

!     Compute daily runoff and water available for infiltration
!       for each column.
      CALL Rnoff_2D( 
     &  CN, ColFrac, FurCol1, FurRow1, HalfFurrow,        !Input
     &  HalfRow, LL, Rain, SAT, SWV_D,                    !Input
     &  RUNOFF, Runoff_col, WINF_col)                     !Output
!       The rain water from plastic cover run to furrow and infitration

!-----------------------------------------------------------------
!     Drip irrigation schedule for today
      CALL GET(DripIrrig)
 
!     DripDur   = DripIrrig % DripDur   !duration of ea. irrig (hr)
!     DripInt   = DripIrrig % DripInt   !interval between irrig (hr)
!     DripNum   = DripIrrig % DripNum   !# of irrigs for J'th entries today
!     DripRate  = DripIrrig % DripRate  !emitter rate (ml/s)* efficiency
!     DripStart = DripIrrig % DripStart !start time (hr)

      DripSpc   = DripIrrig % DripSpc   !emitter spacing (cm)
      IrrRate   = DripIrrig % IrrRate   !daily irrigation (mm)
      DripNumTotArr = 0                    !# of irrigs today
      DO IDL = 1, NDripLnTOT
        do J = 1, DripIrrig(IDL) % DripEvntEntr
          DripNumTotArr(IDL) = DripNumTotArr(IDL) + 
     &          DripIrrig(IDL) % DripNum(J)
        End do
      END DO
      IF (MAXVAL(DripNumTotArr) > 0 .AND. MAXVAL(IrrRate) > 1.E-6) THEN
        ALLOCATE (IrrigSched(NDripLnTOT,MAXVAL(DripNumTotArr),2))
        ALLOCATE (DripInt(NDripLnTOT,MAXVAL(DripNumTotArr)))
        ALLOCATE (DripRate(NDripLnTOT,MAXVAL(DripNumTotArr)))

        IrrigSched = 0.
        DripInt = 0.
        DripRate = 0.
        DO IDL = 1, NDripLnTOT
          JJ = 0
          do J = 1, DripIrrig(IDL) % DripEvntEntr
            DO i = 1, DripIrrig(IDL) % DripNum(J)
              JJ = JJ + 1
              IrrigSched(IDL,JJ,1) = DripIrrig(IDL) % DripStart(J) +
     &            (DripIrrig(IDL) % DripInt(J) + 
     &             DripIrrig(IDL) % DripDur(J)) * (i - 1)
              IrrigSched(IDL,JJ,2) = IrrigSched(IDL,JJ,1) +
     &            DripIrrig(IDL) % DripDur(J)
              DripInt(IDL,JJ) = DripIrrig(IDL) % DripInt(J)
              DripRate(IDL,JJ) = DripIrrig(IDL) % DripRate(J)
            ENDDO
            IF (J < DripIrrig(IDL) % DripEvntEntr) THEN
              DripInt(IDL,JJ) = DripIrrig(IDL) % DripStart(J+1) -
     &            IrrigSched(IDL,JJ,2)
            END IF
          Enddo
        END DO
      ELSE 
        ALLOCATE (DripInt(IDL,1))
        ALLOCATE (DripRate(IDL,1))
        DripNumTotArr = 0
        DripInt = 24.
      ENDIF

      DO IDL = 1, NDripLnTOT
        DripNumTot = DripNumTotArr(IDL)
!       Check if the schedule time is valid
        Do  jj = 1, DripNumTot
           if ((IrrigSched(IDL,JJ,1) .LT. 0.).or.
     &         (IrrigSched(IDL,JJ,1) .GT. 24.)) Then  
             WRITE(MSG(1),'(A)')
     &         "starting time is invalid"
             CALL INFO(1,ERRKEY,MSG)
           elseif ((IrrigSched(IDL,JJ,2) .LT. 0.).or.
     &             (IrrigSched(IDL,JJ,2) .GT. 24.)) Then 
             WRITE(MSG(1),'(A)')
     &         "ending time is invalid"
             CALL INFO(1,ERRKEY,MSG)
!          Check if the starting time of certain event is earlier than the end time of previous event
           elseif (JJ .GT. 1) then 
             if (IrrigSched(IDL,JJ,1) .LT. IrrigSched(IDL,JJ-1,2)) then
               WRITE(MSG(1),'(A)')
     &          "Check starting time with ending time of previous event"
               CALL INFO(1,ERRKEY,MSG)
             endif
           endif
        enddo
      END DO
      IRRIGArr = .FALSE.
      IrrIdxArr = 1

!     Can also use conventional irrigation, if no drip irrig entries
      IF (MAXVAL(DripNumTotArr) == 0) THEN 
        StdIrrig = IRRAMT ! for non-drip irrigation
        IF (StdIrrig < 1.E-6) THEN
          StdIrrig = 0.0
        ELSE
!         This assumes that plastic mulch does not affect irrigation amounts
          DO J = 1, NColsTot
            WINF_col(j) = WINF_col(j) + StdIrrig
          ENDDO
        ENDIF
      ELSE
        StdIrrig = 0.0 ! If there is drip irrigation, then no std irrig
      ENDIF

      CALL PUT('WATER', 'WINF_COL', WINF_col, MaxCols)

!-----------------------------------------------------------------
!     Time Loop
!-----------------------------------------------------------------
      StartTime = 0.0
      NextUPdate = 0
      Count = 0
      ES_day = 0.0
      LastCumRad = 0.0
      MinTimeIncr = 60.
      Runoff_day = 0.0

      TimeLoop: DO WHILE (StartTime < 24.0)
        Count = Count + 1

        Drain_ts = 0.0
        ES_ts = 0.0
        Irr_ts = 0.0
        Rain_ts = 0.0
        Runoff_ts = 0.0
        TRWU_ts = 0.0
        IrrVol = 0.d0
        INF_vol = 0.d0
        INF_vol_dtal= 0.d0

!       ---------------------------------------------------------------
!       ---------------------------------------------------------------
!       SET THE TIME STEP
!       First determine unsaturated hydraulic conductivity and diffusivity
!       for each cell based on soil water content at beginning of time step
!       Also compute optimum time increment for stability
        TimeIncr = Max_Time_Step  !minutes

        CritCell = 0 
!       DO i = 1, min(LIMIT_2D, NRowsTot)
        DO i = 1, NRowsTot
          DO j = 1, NColsTot
            SELECT CASE(CELLS(i,j)%STRUC%Cell_Type)
            CASE (3,4,5);CONTINUE
            CASE DEFAULT; CYCLE
            END SELECT
            Se(i,j) = (SWV_avail(i,j) - WCr(i))/(SAT(i) - WCr(i))
            Se(i,j) = MIN(1.0, Se(i,j))
            Se(i,j) = MAX(0.0, Se(i,j))

!            IF (i == NRowsTot) THEN
!              Kunsat(i,j) = Ksat(i)
!            ELSE
              Kunsat(i,j)= K_unsat(Ksat(i), mVG(i), Se(i,j))
!            ENDIF
            Diffus(i,j)= Diffus_Coef(Ksat(i), alphaVG(i), mVG(i),SAT(i),
     &        Se(i,j), WCr(i))

!           JZW 9/29/2009
            IF (Diffus(i,j) > 1.E-9 .AND. Kunsat(i,j) > 1.E-9 
     &                              .AND. i .LE. LIMIT_2D) THEN
              DeltaT = 1./(
     &          2.*Diffus(i,j)/(Width(i,j)*Width(i,j)) + 
     &          2.*Diffus(i,j)/(Thick(i,j)*Thick(i,j)) + 
!              calculation was not sensitive enough to K, multiply by 10.
     &          10.*Kunsat(i,j)/Thick(i,j)) * 60. !min 
                
              IF (DeltaT < TimeIncr) THEN            
                TimeIncr = DeltaT
                CritCell(1) = i; CritCell(2) = j
              ENDIF
            ENDIF
          ENDDO
        ENDDO

!       Need smaller time step during irrigation
        DO IDL = 1, NDripLnTOT
          IrrigIndex = IrrIdxArr(IDL)
          IRRIG      = IRRIGArr(IDL)
          DripNumTot = DripNumTotArr(IDL)
          IF (DripNumTot > 0 .AND. IrrigIndex <= DripNumTot) THEN
            IF (IRRIG) THEN
!             Currently in irrig cycle.  Check for end of irrig. 
              IF (StartTime - IrrigSched(IDL,IrrigIndex,2) > -0.5/60.)
     &                THEN
!               End of irrig cycle
!       NOTE: should change this logic. For very small time steps we should
!       continue to irrigate.  could add irrigation over a partial time step.
                IRRIG = .FALSE.
                IrrigIndex = IrrigIndex + 1
              ENDIF
            ELSE
!             Currently in drying cycle.  Check for start of next irrig. 
              IF (StartTime - IrrigSched(IDL,IrrigIndex,1) > -0.5/60.)
     &                THEN
!               Start new irrig cycle
                IRRIG = .TRUE.
              ENDIF
            ENDIF
          ELSE
            IRRIG = .FALSE.
          ENDIF

          IF (RAIN > 1.E-6) THEN  ! check if both irr and rain exist
!           Actually, under above if statement, the TimeIncr is overwrite by the next if (IRRIG) elseif 
!           IF (DripInt(J) > 1.E-6) THEN
!             if (IrrigIndex < DripNumTot) Then
!               DripIntNow =  ! Check if it  is correct ?????
!     &         IrrigSched(IrrigIndex+1,1) - IrrigSched(IrrigIndex,2)
!             else 
!               DripIntNow = 0
!             endif
!           IF ( DripIntNow > 1.E-6) THEN
!           IF ( DripInt(IrrigIndex) > 1.E-6) THEN
            IF (IrrigIndex .LT. DripNumTot) then 
              IF (DripInt(IDL,IrrigIndex) > 1.E-6) DeltaT =
     &                 Time_interval(DripInt(IDL,IrrigIndex), TSN)  
!             DeltaT = Time_interval(DripInt, TSN)  !minutes !TSN:Approximate time interval, min
            ELSE
              DeltaT = TSN
            ENDIF
            TimeIncr = MIN(TimeIncr, DeltaT)
          ENDIF

          IF (IRRIG) THEN
!           Irrigated time step
            !DeltaT = Time_interval(DripDur(J), TSI)  !minutes
            DeltaT = Time_interval(IrrigSched(IDL,IrrigIndex,2) 
     &              - IrrigSched(IDL,IrrigIndex,1), TSI)  !minutes
            TimeIncr = MIN(TimeIncr, DeltaT)
            IF (StartTime + TimeIncr/60. > IrrigSched(IDL,IrrigIndex,2))
     &                THEN
!             Don't let time step go beyond end of irrigation
              TimeIncr = (IrrigSched(IDL,IrrigIndex,2) - StartTime) *60.
            ENDIF
          ELSEIF (IrrigIndex <= DripNumTot) THEN
!           Non-irrigated time step and another irrigation coming up today
            IF (StartTime + TimeIncr/60. > IrrigSched(IDL,IrrigIndex,1))
     &         THEN
  !           Don't let time step go beyond start of next irrigation
              TimeIncr = (IrrigSched(IDL,IrrigIndex,1) - StartTime) *60.
            ENDIF
          ENDIF
          IrrIdxArr(IDL) = IrrigIndex
          IRRIGArr(IDL)  = IRRIG
        END DO
!       ---------------------------------------------------------------
        EndTime = StartTime + TimeIncr / 60.
        HR = INT(EndTime) + 1

!       If ending time is within 1/2 minute of either the next irrigation
!         event, or the next full hour, then use that as ending time.
!       NOTE: this will only work for time increments of > 1/2 minute
!       BE CAREFUL WITH TIME INCREMENTS < 1 MINUTE!
        DO IDL = 1, NDripLnTOT
          IrrigIndex = IrrIdxArr(IDL)
          IRRIG      = IRRIGArr(IDL)
          DripNumTot = DripNumTotArr(IDL)
          IF (DripNumTot > 0 .AND. IrrigIndex <= DripNumTot) THEN
            IF (.NOT. IRRIG .AND.
     &          ABS(EndTime - IrrigSched(IDL,IrrigIndex,1)) < 0.5/60.) 
     &                THEN
              EndTime = IrrigSched(IDL,IrrigIndex,1)
            ELSEIF (IRRIG .AND.
     &          ABS(EndTime - IrrigSched(IDL,IrrigIndex,2)) < 0.5/60.) 
     &                THEN
              EndTime = IrrigSched(IDL,IrrigIndex,2)
            ENDIF
          ENDIF
          IF (ABS(EndTime - HR) < 0.5/60.) THEN
            EndTime = HR
          ENDIF
        END DO
!        If (EndTime > 24.1) Then
!          Write(*,*) "irrigation schedule is beyond 24:00pm"
!          Write(*,*) " Program stop on ", CONTROL % YRDOY
!          Stop
!        Endif
        IF (EndTime > 24.) THEN
          EndTime = 24.
        ENDIF
        TimeIncr = (EndTime - StartTime) * 60.  !min
        DayIncr  = TimeIncr / 60. / 24.         !days
!        Print *, StartTime, TimeIncr

!       Minimum time increment today
        IF (TimeIncr < MinTimeIncr) MinTimeIncr = TimeIncr

!       DONE SETTING THE TIME STEP
!       ---------------------------------------------------------------
!       ---------------------------------------------------------------

!       Compute water available for uptake, drainage, lateral flow this time step
!       ---------------------------------------------------------------
!       ADDITION OF IRRIGATION AMOUNT
!       Add drip irrigation volume to cell(1,1), which is centerline of bed.
!       Can add to any cell in top layer. 7/26/2010
        DO IDL = 1, NDripLnTOT
          IrrigIndex = IrrIdxArr(IDL)
          IRRIG      = IRRIGArr(IDL)
          IF (IRRIG) THEN
            DripCol = BedDimension % DripCol(IDL)
            DripRow = BedDimension % DripRow(IDL)
!           Irrigation volume is half dripper rate because only half row is being modeled
            IrrVol(IDL) = (DripRate(IDL,IrrigIndex)/ 2.) / DripSpc(IDL) 
     &                * TimeIncr * 60.
!             cm3[water]     cm3[water]          1                s
!           ------------  =  ---------- * -------------- * min * ---
!           cm[row length]       s        cm[row length]         min

!           Apply all irrigation to DripRow, DripCol cell
            SWV_avail(DripRow,DripCol) = SWV_avail(DripRow,DripCol) + 
     &                           IrrVol(IDL)/ CellArea(DripRow,DripCol)
!          SWV_avail(1,DripCol) = SWV_avail(1,DripCol) + IrrVol(IDL)
!     &                                          / CellArea(1,DripCol)
            IRR_ts = IRR_ts + IrrVol(IDL) / HalfRow * 10.     !mm
            CellDrip(DripRow,DripCol) = CellDrip(DripRow,DripCol) 
     &                                   + IrrVol(IDL) / HalfRow * 10.
          ENDIF
        END DO

!       ---------------------------------------------------------------
        Runoff_ts = RUNOFF * DayIncr

!       ADDITION OF INFILTRATION AMOUNT
!       Add infiltration to top furrow cells evenly throughout day.
        IF (RAIN > 1.E-6 .OR. StdIrrig > 1.E-6) THEN
          DO j = 1, NColsTot
!           add infiltration to top cells
            IF (BedDimension % RaisedBed) THEN
              IF (j < FurCol1) THEN
                i = 1       !top of raised bed
              ELSE
                i = FurRow1 !top of furrow
              ENDIF
            ELSE
              i = 1         !flat surface
            ENDIF

            INF_vol = WINF_col(j) * 0.1 * DayIncr / Thick(i,j)
!           cm3[water]   mm[water]   cm         1           
!           ---------- = --------- * -- * d * --------
!            cm3[soil]       d       mm       cm[soil]   

            SWV_avail(i,j) = SWV_avail(i,j) + INF_vol

!           Check for soil water content above saturation after addition of 
!           rainfall and standard irrigation in this time step.
            IF (SWV_avail(i,j) > SAT(i)) THEN
              Excess_vf = SWV_avail(i,j) - SAT(i)
              Excess_mm = Excess_vf * 10. * Thick(i,j)
!                         cm3[water]  mm         
!             mm[water] = --------- * --  * cm[soil]
!                         cm3[soil]   cm  
  
              SWV_avail(i,j) = SAT(i)
              INF_vol = INF_vol - Excess_vf
              RUNOFF_col(j) = RUNOFF_col(j) + Excess_mm * ColFrac(i,j)
              Runoff_ts = Runoff_ts + Excess_mm * ColFrac(i,j)
              CALL PUT('WATER', 'WINF_COL', WINF_col, MaxCols)
            ENDIF

!           debug chp
            INF_vol_dtal(i,j) = INF_vol
!           Daily rainfall plus standard irrigation to cell i,j
            CellInf(i,j) = CellInf(i,j) + INF_vol_dtal(i,j)
          ENDDO
          Runoff_day = Runoff_day + Runoff_ts 
          Rain_ts = RAIN * DayIncr ! in mm
          IRR_ts = IRR_ts + StdIrrig * DayIncr
        ENDIF

!       Update lateral flow time step (for checking balance only)
        LatFlow_ts = netLatFlow * DayIncr  !mm
        SumLatFlow = SumLatFlow + LatFlow_ts

!       ===============================================================
!       Estimate cumulative fraction of daily solar radiation that will 
!         be reached by end of this time step.  
!       Used for average EOP and ES during this time interval.  
!       Also used to accumulate water stress.
        HR = int(EndTime)
        IF (HR >= 24) THEN
          CumRad = 1.0
        ELSE
          CumRad = CumFracRad(HR) + 
     &      (CumFracRad(HR+1) - CumFracRad(HR)) * (EndTime-HR)
        ENDIF
        TSRadFrac = CumRad - LastCumRad 
        LastCumRad = CumRad
!       ===============================================================

!       Soil Evaporation
!       ----------------
!       - Soil evaporation is assumed to be distributed over the day 
!         based on solar radiation.
!       - ES_mm(i,j) is  mm of evaporation from cell(i,j). 
!       - These values can be added up in a column of soil to get the 
!         total evaporation (mm) from that column. 
!       - To get the total evaporation across a row, column fractions are used.
        ES_ts = 0.0

        ColLoop: DO j = 1, NColsTot
          RowLoop: DO i = 1, NRowsTot
            SELECT CASE (CELLS(i,j)%STRUC%Cell_Type)
            CASE (3,4,5)
!             mm evap during this time interval in current cell
              ES_avg = TSRadFrac * ES_mm(i,j) 
!             mm/ts                  mm/d
              ES_ts = ES_ts + ES_avg * ColFrac(i,j)
!             Subtract from cell water by volume fraction
              SWV_avail(i,j) = SWV_avail(i,j) 
     &                       - ES_avg * mm_2_vf(i,j)

!             debug chp
              es_vf_ts(i,j) = ES_avg * mm_2_vf(i,j) 
            CASE DEFAULT; CYCLE
            END SELECT
          ENDDO RowLoop
        ENDDO ColLoop

        ES_day = ES_day + ES_ts

!       ---------------------------------------------------------------
!       ROOT WATER UPTAKE
!       -----------------
!       - RWU_2D_ts(i,j) is  mm of evaporation from cell(i,j) in one time step. 
!       - These values can be added up in a column of soil to get the 
!         total root uptake (mm) from that column. 
!       - To get the total uptake across a row, column weights are used. 
        EOP_ts = TSRadFrac * EOP

        CALL ROOTWU_2D(RATE, TimeIncr, 
     &    Cells, EOP_ts, SWV_avail,                       !Input 
     &    RWU_2D_ts, RWUP_2D_ts, TRWU_ts, TRWUP_ts)       !Output

        CALL WaterStress(SNGL(EOP_ts), RWUEP1, SNGL(TRWUP_ts)/10., 
     &      SWFAC_ts, TURFAC_ts)

        SWFAC  = SWFAC  + SWFAC_ts  * TSRadFrac
        TURFAC = TURFAC + TURFAC_ts * TSRadFrac
        SUM_TSRF = SUM_TSRF + TSRadFrac

!       Calculate SW available for drainage -- reduce by 
!       root water uptake.
        DO i = 1, NRowsTot
          DO j = 1, NColsTot
            SELECT CASE (CELLS(i,j)%STRUC%Cell_Type)
            CASE (3,4,5)
!             Potential and actual root water uptake (mm)
              RWU_2D(i,j) = RWU_2D(i,j) + RWU_2D_ts(i,j)
              RWUP_2D(i,j) = RWUP_2D(i,j) + RWUP_2D_ts(i,j)
              EP_vf(i,j) = RWU_2D_ts(i,j) * mm_2_vf(i,j)

              IF (i <= LIMIT_2D) THEN 
                SWV_avail(i,j) = SWV_avail(i,j) - EP_vf(i,j)
              ELSE  
                !TotRWU_WT = TotRWU_WT + EP_vf(i,j) * conversion
                SWV_avail(i,j) = SWV_avail(i,j) - EP_vf(i,j)
              ENDIF

            CASE DEFAULT; CYCLE
            END SELECT
          ENDDO
        ENDDO

        TRWU = TRWU + TRWU_ts
        IF (EOP_ts > 1.E-7) THEN
          TRWUP = TRWUP + TRWUP_ts
        ENDIF
        CELLS%Rate%EP_rate = RWU_2D

!       ---------------------------------------------------------------
!       HORIZONTAL AND VERTICAL WATER MOVEMENT
!       Drainage_2D computes both runoff and drainage for bed and furrow
        CALL Drainage_2D(RATE, 
     &    CELLS, Diffus, FurCol1, Kunsat,             !Input
     &    SOILPROP, SWV_avail, TimeIncr, WCr,         !Input
     &    SWV_ts, SWFh_ts, SWFv_ts)                   !Output

!       Here LatFlow_ts is due to the drainage of layer LIMIT_2D 
!       Drainage is from first layer to LIMIT_2D
        DRAIN_ts = 0.0
        DRAIN_col = 0.0
        DO col = 1, NColsTot
!         Drainage in this column in this time step:
          row = min(NLayr, LIMIT_2D)
          Drainage_ts_col = SWFv_ts(row,col) / HalfRow * 10.    !mm
          DRAIN_ts = DRAIN_ts + Drainage_ts_col                 !mm
          Drain_col(col) = Drain_col(col) + Drainage_ts_col     !mm
        ENDDO
        DRAIN_2D = DRAIN_2D + DRAIN_ts                          !mm

        SWV_avail = SWV_ts
        SWV_D = SWV_ts

!       Call NFLUX on sub-daily time step to computer N movement with water
        CALL NFLUXts_2D (
     &    CELLS, SWV_ts, SWFh_ts, SWFv_ts)       !Input

        Call Calc_SW_Vol(
     &    CellArea, Cell_Type, HalfRow, SWV_ts,             !Input
     &    SW_vol_tot)                                       !Output

        ! Output to SoilWat_ts.OUT and CellDetail.OUT
        Call Wbal_2D_ts(CONTROL, ISWITCH, EndTime, TimeIncr, !Input
     &    DRAIN_ts, RUNOFF_ts, IRR_ts, RAIN_ts,              !Input
     &    ES_TS, TRWU_ts, SW_vol_tot, CritCell,              !Input
     &    Diffus, Kunsat, LatFlow_ts, Count, LatFlow,        !Input
     &    SWV_D)

        CALL OpSW15min(CONTROL, ISWITCH, 
     &    CELLS, EndTime, TimeIncr, SWV_D)  !Input

!       ---------------------------------------------------------------
!       Update time for next iteration
!       if time step >1 hr, then start time = 0 will be missing
        StartTime = EndTime 
!       Output arrays only once per hour
        iHr = nint(StartTime-TimeIncr/120.0-0.0001) 
        if (((iHr < 23) .OR.
     &       (iHr >= 23 .AND. NextUpdate == 23.)) .AND. 
     &      (iHr . GE. float(NextUpdate)))    then
    ! &      StartTime > float(NextUpdate))    then
          NextUpdate = NextUpdate + 1
          if (NextUpdate > 23) NextUpdate = 0
          SWV = SNGL(SWV_ts)    !real
!          call SW_SensorH(SOILPROP, CONTROL, Cells, SWV, 
!     &       iHr)  
 !    &       NextUpdate)
          
         !CALL ArrayHandler(CELLS, CONTROL, SOILPROP, SNGL(SWV_ts), 
  !   &          "SWV", 0.0, 0.5)
        endif
    
!-----------------------------------------------------------------------
      ENDDO TimeLoop
!-----------------------------------------------------------------------

!       Add in daily upward flow from evaporation into flux for N movement
        DO i = 1, NRowsTot
          DO j = 1, NColsTot
            SELECT CASE (CELLS(i,j)%STRUC%Cell_Type)
            CASE (3,4,5)
!              Negative vertical flow = upward flow from (i+1,j)
              if (i .LT. NRowsTot)
     &            SWFlux_U(i+1,j) = SWFLUX_U(i+1,j) + EvapFlow(i,j)
            CASE DEFAULT; CYCLE
            END SELECT
          ENDDO
        ENDDO

      SWV_D = SWV_ts
      SWV = SNGL(SWV_D)

!     When a managed water table is present, 
!       adjust the lateral flow to include today's drainage
!     Drainage for systems with a water table is from the 2D top layers
!       to the 1D saturated layers and is "absorbed" by the lateral 
!       inflows or outflows which were calculated based on the depth 
!       to the watertable.
      IF (LIMIT_2D .LT. NLAYR) THEN
        NetLatFlow = NetLatFlow - DRAIN_2D
      ENDIF

!     Convert units from mm to cm for DSSAT plant routines.
      TRWUP = TRWUP / 10.           !cm
      TRWU  = TRWU  / 10.           !cm
      CALL PUT('SPAM','TRWUP', TRWUP)
      CALL PUT('SPAM','TRWU',  TRWU)

      DO i = 1, NRowsTot
        DO j = 1, NColsTot
          RWU_2D_frac(i,j) = RWU_2D(i,j) * ColFrac(i,j)
        ENDDO
      ENDDO

      CALL Cell2Layer_2D(
     &  RWU_2D_frac, CELLS%Struc, SOILPROP%NLAYR,  !Input
     &  RWU, SurfaceVal)                           !Output
      CALL PUT('SPAM','UH2O', RWU, NL)

!     Compare daily average with accumulated values. Should be the 
!       same for SWFAC.  Should be different for TURFAC
      SWFAC  = SWFAC  / SUM_TSRF
      TURFAC = TURFAC / SUM_TSRF
      CALL WaterStress(EOP, RWUEP1, TRWUP, SWFAC_day, TURFAC_day)

      IF (Allocated(IrrigSched)) DEALLOCATE (IrrigSched)
      IF (Allocated(DripInt)) DEALLOCATE (DripInt)
      IF (Allocated(DripRate)) DEALLOCATE (DripRate)

!     ---------------------------------------------------------------
!     Print message for small time increments
      IF (MinTimeIncr < 1.) THEN
        WRITE(MSG(1),'(A,F6.3,A)')
     &     "Minimum time increment < 1 minute: ",MinTimeIncr," min"
        CALL INFO(1,ERRKEY,MSG)
      ENDIF

      CELLS % Rate % SWFlux_L = SWFlux_L
      CELLS % Rate % SWFlux_R = SWFlux_R
      CELLS % Rate % SWFlux_D = SWFlux_D
      CELLS % Rate % SWFlux_U = SWFlux_U
      CELLS % Rate % CellInf  = CellInf

!***********************************************************************
!***********************************************************************
!     DAILY INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
      IF (ISWITCH%ISWWAT == 'N') RETURN

      CELLS % State % SWV = SWV

      RUNOFF = Runoff_day
      CALL WBSUM_2D(INTEGR,
     &    CELLS, DRAIN_2D, HalfRow, RAIN, RUNOFF, SWV,    !Input
     &    CRAIN, TDRAIN, TEP, TRUNOF,                     !Output
     &    TSW, TSWINI)                                    !Output

      CALL Interpolate2Layers_2D(                    
     &  CELLS%State%SWV, CELLS%Struc, SOILPROP%NLAYR,     !Input
     &  SW)                                               !Output

!***********************************************************************
!***********************************************************************
!     OUTPUT - Daily output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
C-----------------------------------------------------------------------
      IF (ISWITCH%ISWWAT == 'N') RETURN
      
      !CALL ArrayHandler(CELLS, CONTROL, SOILPROP, SWV,
    ! &          "SWV", 0.0, 0.5)

!     Output SoilWat.OUT
      CALL OPWBAL(CONTROL, ISWITCH, 
     &    CRAIN, DLAYR, IRRAMT,                       !Input
     &    netLatFlow, LL, NLAYR,                      !Input
     &    RUNOFF, SOILPROP, SW, TDRAIN, TRUNOF)       !Input

!     ---------------------------------------------------------
!     Daily water balance output to SoilWatBal.OUT 
      IF (LIMIT_2D .GE. NRowsTot) THEN 
        SolProfDrain = DRAIN_2D
      ELSE
        SolProfDrain = 0.0
      ENDIF

!     In 2D model, TSW units are mm. 1D model uses cm.
      TSW_cm = TSW / 10.
      CALL Wbal(CONTROL, ISWITCH, 
     &    CRAIN, SolProfDrain, FLOODWAT, netLatFlow,
     &    IRRAMT, MULCH, RAIN, RUNOFF, SNOW,  
     &    TDFC, TDFD, TDRAIN, TRUNOF, TSW_cm)

!-----------------------------------------------------------------
!          call SW_SensorD(SOILPROP, CONTROL, Cells, SWV)
!------------------------------------------------------------
!***********************************************************************
!***********************************************************************
!     SEASEND - Seasonal output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
      RLV_2D = CELLS % STATE % RLV
      !CALL ArrayHandler(CELLS, CONTROL, SOILPROP, SWV,
   !  &          "SWV", 0.0, 0.5)

      IF (ISWITCH%ISWWAT == 'N') RETURN
      
      CALL OPWBAL(CONTROL, ISWITCH, 
     &    CRAIN, DLAYR, IRRAMT,                       !Input
     &    netLatFlow, LL, NLAYR,                      !Input
     &    RUNOFF, SOILPROP, SW, TDRAIN, TRUNOF)       !Input

!     Seasonal water balance output 
!     In 2D model, TSW units are mm. 1D model uses cm.
      TSW_cm = TSW / 10.
      CALL Wbal(CONTROL, ISWITCH, 
     &    CRAIN, SolProfDrain, FLOODWAT, netLatFlow,
     &    IRRAMT, MULCH, RAIN, RUNOFF, SNOW,  
     &    TDFC, TDFD, TDRAIN, TRUNOF, TSW_cm)

!     chp 2022-07-10 can't use an array of zeros in the argument. 
!     I don't want to set the original variables to zero, so use a dummy argument here.
      IrrVol_temp = 0.0
      INF_vol_dtal_temp = 0.0
      Call Wbal_2D_ts(CONTROL, ISWITCH, 24.0, 0.0, 
     &    DRAIN_ts, RUNOFF_ts, IRR_ts, RAIN_ts, 
     &    ES_TS, TRWU_ts, SW_vol_tot, CritCell, 
     &    Diffus, Kunsat, LatFlow, 0, 0.0, SWV_D)

      CALL OpSW15min(CONTROL, ISWITCH, 
     &    CELLS, EndTime, TimeIncr, SWV_D)  !Input

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
!     Store root water uptake and potential root water uptake in global
!     variables.  
      CALL PUT('SPAM','TRWUP',TRWUP)
      CALL PUT('SPAM','TRWU',TRWU)

      RETURN
      END SUBROUTINE WatBal2D

C=====================================================================
!     WatBal2D VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! ActWTD     Actual water table depth
! BD(NL)     Bulk density
! CapilaryFlow(row, col) cell capillary rise by Joe Ritchie model in cm2/min
! CEP        Cumulative root uptake from simulation to current day (mm)
! CES        Cumulative evaporation from simulation to current day (mm)
! CN         Curve number an empirical parameter used in Ritchie hydrology for predicting direct runoff or infiltration from rainfall excess
! ColFrac    Cell column width divided half row width
! CRAIN      Cumulative precipitation (mm)
! CritCell(2)The # of row and # of Col of cell which require smallest time step. For output monitor
! CumFracRad Cumulative distribution function for solar radiation and apply to 
!            soil evaporation and plant transpiration.  Function is stored in CumFracRad and
!            goes from zero at the beginning of the day to 1.0 at the end of the day.
! DayIncr    It is TimeIncr in unit of day
! DeltaT     Time step criterial required for the cell ! Changed by Jin in Feb. 2011
! Diffus(Row,Col) Diffusivity in cm2/h
! DRN(L)     Drainage rate through soil layer L for current time step (cm)
! DRAIN_1D   Daily drain water from bottom of the soil profile
! DRAIN_2D   Daily drain water from bottom of the soil profile per unit length of soil in mm if there is no water table.
!            or refer to the drain from teh LLIMIT_2D if there is water table.
! Drain_ts   Drain water from bottom of the soil profile per unit length of soil in current time step in mm
! DripDur    Duration of ea. irrig (hr)
! DripInt    Interval between irrig (hr)
! DripNum(NDripEvnt) # of drip irrigatios for each event entry today
! DripNumTot Total # of drip irrigation today
! DripIrrig % DripEvntEntr   # of event entry today  
! DripRate   Emitter rate (ml/s)
! DripSpc    Emitter spacing (cm)
! DripStart  Start time (hr)
! EFFIRR     Irrigation application efficiency (cm/cm)
! EndTime    Irrigation end time in hour
! EP_vf(Row,Col) In a time step, cell actual plant transpiration rate volumetric fraction in mm/mm
! ES_vf_ts(Row,Col) In a time step, cell soil evaporation volumetric fraction in mm/mm
! EOP_avg     Time average potential plant transpiration rate of current time step in mm/hr
! EOP_Hr(0:24)Hourly values of potential plant transpiration rate in mm/hr
! ES          Actual soil evaporation rate up to this time step (mm)???
! ES_avg      Average soil evaporation rate in current time step(mm)
! ES_mm       Daily cell evaporation rates, mm/d
! ES_rate     Cell evaporation rates, mm/d
! ES_ts       (Cumulative?)Actual soil evaporation rate in this time step (mm)
! FurRow1     # of rows for top row of furrow counted from top of bed
! FurCol1     # of cols for top 1st col of furrow counted from center of bed
! Hr
! iHr         for SW at sensor depth hourly output 
! INF_vol     Infiltration to top furrow cells evenly throughout day in cm3[water]/cm3[soil] 
! INF_vol_dtal INF_vol for cell detail
! IRRAMT      Irrigation amount for today (mm). Including drip or sprinkle.
! IRRIG       LOGICAL variable. TRUE means drip irrigation is on, FALS means off 
! IRRIGIndex  Daily irrigation event index
! IrrigSched(DripNum,1) Irrigation start time in hr
! IrrigSched(DripNum,2) Irrigation end time in hr
! IrrRate     Daily irrigation (mm)
! IrrVol      Half of 2D irrigation volume of current time step in cm3[water]/ cm[row length]. Double precision
! ISWWAT      Soil water balance on/off switch (Y for yes, N for no)
! IRR_ts or irr_ts     1D irrigation volume of current time step in mm
! IRRIG       Irrigation simulation switch status
! Kunsat(Row,Col) Un-saturated hydraulic conductivity in cm/hr
! LastCumRad  Cumulative solar radiation until last time step
! LatFlow     Daily total lat flow for all cells. Inward is positive.
! LatFlow_ts  Total lat flow for all cells for current time step. Inward is positive.
!             It is in mm finally when pass to this subroutine
! LIMIT_2D    Represents the lowest layer for which 2D modeling is done.
! mm_2_vf(Row,Col)  Conversion from mm[water] to volumetric fraction for each cell  
!                     cm2[water]/cm3[soil]
! MgmtWTD     Managed water table depth, user input. Counted from top of bed or flat surface. 
! NDrpEvnt    Maximum # of dripper irrigation event entries per day 
! PORMIN      Minimum pore space required for supplying oxygen to roots for 
!                optimal growth and function (cm3/cm3)
! RADHR       Total hourly solar radiation (J/m2-s)
! Rain_ts     Rain water (mm) in current time step
! RLV_2D(Row,Col)    Root length density for cell (cm[root] / cm3[soil])
! RUNOFF      Daily runoff from furrows   
! Runoff_col(Col)  Daily runoff from furrows for each column in furrow in mm[water].
! Runoff_ts   Runoff in current time step
! RWUMX       The plant maximum uptake rate per unit length of root in cm3[water]/cm[root]) 
!                It is a crop specific parameter, read from the species file. It is constrained by soil     
! ROWSPC_CM   The plant distance between row (cm)
! RWU_2D(Row,Col) Daily cell root uptake water in mm
! RWU_2D_ts(Row,Col)   Cell root water uptake in current time step (mm[water])
! RWUP_2D(Row,Col)     Cell potential root water uptake upto current time step(mm[water])
! RWUP_2D_ts(Row,Col)  Cell potential root water uptake in current time step (mm[water])
! SRAD_TOT    Daily total RADHR
! StartTime   Irrigation start time in hour
! StdIrrig for dripper also    Standard one dimension effective irrigation amount(mm). Here for non-drip case
! SUM_TSRF    Sum of solar radiation fraction over each time step of the day
! SWA         Available soil water content in mm3/mm3 
! SWFAC       Effect of soil-water stress on photosynthesis, 1.0=no stress, 0.0=max stress
! SWDELTS     Change in soil water content due to drainage in layer L
! SW_vol_tot  Total soil water amount in the whole profile. Was in cm2, final in mm2
! SWFh_ts     Soil water flow oriental of current time step in cm2[water]
! SWFlux_D(i,j)Cell soil water flux towards down side in cm2[water]
! SWFlux_L(i,j)Cell soil water flux towards left side in cm2[water]
! SWFlux_R(i,j)Cell soil water flux toward right side in cm2[water]
! SWFlux_U(i,j)Cell soil water flux upward in cm2[water]
! SWFv_ts     Soil water flow vertical of current time step in cm2[water]
! SWV         Single precision cell soil water content in mm3/mm3  
! SWV_EOD     Soil water content at the end of the day in mm3/mm3   
! SWV_avail(i,j) Double precision cell soil water content at the beginning of time step in mm3/mm3
! SWV_start(1,1) Starting soil water content in mm3/mm3
! SWV_ts      Soil water content in current time step in mm3/mm3
! SWV_D       Double precision cell soil water content in mm3/mm3  
! TDRAIN      Total drain water in  current time step
! TEP         Total root uptake in current day (mm)
! TES         Total soil evaporation in current day(mm)
! ThetaCap    An array of volumetric soil water contents at the midpoint of each soil layer.
!             Calculated from the water characteristic curve at the height above the
!             water table. 
! Thick(Row,Col)Cell thickness in cm[soil]
! TimeIncr    Dynamic time step within a day in min. Minimum of DeltaT in min
! TotStdIrr   Standard one dimension irrigation amount(mm)
! TRUNOF      Total runoff in current time step
! TRWU        Total root uptake water up to current time step was in mm, final in cm
! TRWU_ts     Total root uptake water in current time step
! TRWUP       Total potential root uptake water up to current time step, was in mm, final in cm
! TRWUP_ts    Total potential root uptake water in current time step
! TSN         Approximate time interval, min
! TSRadFrac   Solar radiation fraction in current time step, TSRadFrac = CumRad - LastCumRad
! TSW         Total soil water in profile in current time step
! TURFAC      Water stress factor for expansion (0 - 1)
! EvapFlow   Upflow due to evaporation, needed for N movement, units are cm2 to match flux units
! WATAVL      Water input to the top of soil of current time step in mm
! Width(Row,Col)Cell width in cm[soil] in cm
! WINF_col(Col) Water available for furrow column infiltration in current time step in mm[water]
! JZW question WINF_col(COL) is for both bed and furrow?
! INF_vol    Infiltration in current time step. Double preciaion in cm3/cm3
!-----------------------------------------------------------------------
!     END SUBROUTINE WatBal2D
!=======================================================================

!=======================================================================
      Real Function Time_interval(Duration, ApproxInt)
!     Computes time interval given approximate interval and total duration    

      Implicit None
      Integer NInts  !Integer number of intervals
      Real Duration  !Time period to be split into intervals, hr
      Real ApproxInt !Approximate time interval, min
!     Real Time_interval !Actual time interval, min

      NInts = NINT(Duration * 60. / ApproxInt)
      NInts = MAX0(1, NInts)
      Time_interval = Duration * 60. / FLOAT(NInts) 

      End Function  Time_interval
!=======================================================================
C=====================================================================
!     Time_interval VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! ApproxInt Approximate time interval, min
! Duration  Time period to be split into intervals, hr
! NInts     Integer number of intervals
!-----------------------------------------------------------------------
!     END SUBROUTINE Time_interval
!=======================================================================

!=======================================================================
!     Calculates runoff column by column
      Subroutine Rnoff_2D( 
     &  CN, ColFrac, FurCol1, FurRow1, HalfFurrow,        !Input
     &  HalfRow, LL, Rain, SAT, SWV_D,                    !Input
     &  RUNOFF, Runoff_col, WINF_col)                     !Output

      Use Cells_2D
      USE ModuleData

      Implicit None
      INTEGER jj, j, FurCol1, FurRow1
      REAL CN, HalfFurrow, HalfRow, IABS, PB, RAIN, RUNOFF
      REAL SMX, SWABI, WATAVL
      REAL, DIMENSION(NL) :: LL, SAT
      REAL, DIMENSION(MaxRows,MaxCols) :: ColFrac
      REAL, DIMENSION(MaxCols) :: RUNOFF_col, WINF_col
      Double Precision, DIMENSION(MaxRows,MaxCols) :: SWV_D
      
      RUNOFF = 0.0
      Runoff_col = 0.0
      WINF_col = 0.0

      IF (RAIN > 1.E-6) THEN
        IF (BedDimension % PMCover .AND. FurCol1 > NColsTot) THEN 
          RUNOFF = RAIN 
          RETURN
        ENDIF

!       Rainfall over a row is concentrated in the furrow if there is plastic cover
        IF (BedDimension % PMCover) then
          WATAVL = RAIN * HalfRow / HalfFurrow      !mm
          jj = FurCol1
        else 
          WATAVL = RAIN
          jj = 1
        Endif

        DO j = jj, NColsTot
          SMX = 254.0 * (100.0/CN - 1.0)
!         Initial abstraction ratio
!         Runoff is related to the average soil water content of the top
!         two layers of soil
          SWABI = 0.15 * 0.5 * 
     &      (   ( SAT(FurRow1)   - SWV_D(FurRow1,j)   )
     &        / ( SAT(FurRow1)   -    LL(FurRow1)     )
     &      +   ( SAT(FurRow1+1) - SWV_D(FurRow1+1,j) )
     &        / ( SAT(FurRow1+1) -    LL(FurRow1+1)   ) )
          SWABI = MAX(0.0, SWABI)
          
!         No mulch effects on runoff
          IABS = SWABI
          PB = WATAVL - IABS * SMX
          
          IF (PB .GT. 0) THEN
            RUNOFF_col(j) = PB**2/(WATAVL + (1.0-IABS) * SMX) !mm/d
          END IF
          RUNOFF = RUNOFF + RUNOFF_col(j) * ColFrac(FurRow1, j)
          WINF_col(j) = WATAVL - Runoff_col(j)    !mm/d
        ENDDO
      ENDIF
 
      RETURN
      END Subroutine Rnoff_2D
!=======================================================================
C=====================================================================
!     Rnoff_2D VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! ColFrac(Col) Cell column width divided half row width
! IABS   Initial abstraction ratio, modified for surface mulch layer effects.
! PB     Determines threshold amount of rainfall that will occur before 
!            runoff starts (mm/d)
! RAIN    Precipitation depth for current day (mm)
! RUNOFF  Daily runoff from furrows (mm/d)?
! RUNOFF_col(Col) Column runoff water
! SMX    Soil storage available for surface water based on CN formula
!           (mm)
! SWABI  A soil water abstraction index, a unitless indicator of the soil 
!            water condition at the time of a rainfall event.  This affects 
!            the intercept of the runoff axis when runoff starts to 
!            occur--later when drier and sooner when wetter.
! SWV(Row, Col) Cell soil water content
! WATAVL    Water available for infiltration or runoff (rainfall plus 
!               irrigation) (mm)
! WINF_col(Col) Column infiltration water in mm/d
!-----------------------------------------------------------------------
!     END SUBROUTINE Rnoff_2D
!=======================================================================

!=======================================================================
      Subroutine Calc_SW_Vol(
     &  CellArea, Cell_Type, HalfRow, SWV_D,              !Input
     &  SW_vol_tot)                                       !Output

      Use Cells_2D
      Implicit None
      Integer i, j, Cell_Type(MaxRows,MaxCols)
      Real HalfRow
      Real, Dimension(MaxRows,MaxCols) :: CellArea
      DOUBLE PRECISION SW_vol_tot
      DOUBLE PRECISION, Dimension(MaxRows,MaxCols) :: SWV_D

      SW_vol_tot = 0.0
      DO i = 1, NRowsTot
        DO j = 1, NColsTot
          SELECT CASE (Cell_Type(i,j))
          CASE (3,4,5); CONTINUE
          CASE DEFAULT; CYCLE
          END SELECT
          SW_vol_tot = SW_vol_tot + SWV_D(i,j) * CellArea(i,j)  !cm2
        ENDDO
      ENDDO
      SW_vol_tot = SW_vol_tot / HalfRow * 10.   !mm

      Return
      End Subroutine Calc_SW_Vol
!=======================================================================
C=====================================================================
!     Calc_SW_Vol VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! HalfRow     Half row in cm
! SW_vol_tot  Total soil water amount in the whole profile. Was in cm2, final in mm
! SWV_D(Row,Col) Cell soil water content. e.g. SWV_ts in cm2/cm2 
!-----------------------------------------------------------------------
!     END SUBROUTINE Calc_SW_Vol
!=======================================================================

!=====================================================================
      Function Diffus_Coef(Ksat, alphaVG, mVG, SAT, Se, WCr)
!     Computes diffusivity coefficient
!       based on water content, water holding capacity
!     Parameters for diffusion coefficient from RETC code
!     Diffusivity coefficient in cm2/hr

!     ----------------------------------------------------------------
      Implicit None
      REAL Diffus_Coef, SAT, Ksat, WCr

!     RETC
      REAL Coef1, Exponent, Coef2, Coef3, Se
      REAL alphaVG, mVG

      REAL, PARAMETER :: DiffusCap = 417. !cm2/hr  !Hillel 1. m2/d 
      REAL, PARAMETER :: L = 0.5

!--------------------------------------------------------------
      IF (Se > 0.99) THEN
        Se = 0.99
      ENDIF

      IF (Se > 1.E-9) THEN
        Coef1 = (1-mVG)*Ksat / (alphaVG*mVG*(SAT-WCr))
        Coef2 = (1-Se**(1./mVG))
        Coef3 = Coef2**(-mVG) + Coef2**mVG - 2.
        Exponent = L - 1./mVG
        Diffus_Coef = (Coef1 * Se ** Exponent * Coef3)  !cm2/hr
      ELSE
        Diffus_Coef = 0.0
      ENDIF

!--------------------------------------------------------------
!     Upper limit on diffusion = DiffusCap
      IF (Diffus_Coef > DiffusCap) THEN
        Diffus_Coef = DiffusCap
      ENDIF

!     Lower limit = 0.
      IF (Diffus_Coef < 1.E-10) THEN
        Diffus_Coef = 0.0
      ENDIF

      RETURN
      END Function Diffus_Coef
!=====================================================================

!=======================================================================
!  K_unsat, function, 
!  Method 1.Based on paper "Soil Water Characteristic Estimates by Texture and Organic 
!  Matter for Hydrologic Solutions" by K. E. Saxton and W. J. Rawls, Aug. 2006
!  If soil structure is giving, use soil structure to calculate Ksat, otherwise
!  using LL, DUL and SAT to calculate Ksat
!  Method 2: RETC program: calculate hydraulic conductivity uses Mualem's model
!  m = 1- 1/n
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  
!-----------------------------------------------------------------------
!  Called by: Subroutine RETC_VG
!  Calls:     None
!=====================================================================
      Function K_unsat(Ksat, mVG, Se)
!     Computes unsaturated hydraulic conductivity 
!       based on Ksat, water content, water holding capacity
!       in cm/hr
!     ----------------------------------------------------------------
      Implicit None
      REAL K_unsat, Ksat
      REAL Se, mVG
!     L is a pore-connectivity parameter, be about 0.5 as an average for many soil
      REAL, PARAMETER :: L = 0.5

      IF (Se >= .9999) THEN
        K_unsat = Ksat
      ELSE
!        RETC program: calculate hydraulic conductivity uses Mualem's model
        ! Eq. 31 in RETC.pdf
        K_unsat = Ksat * (Se**L) * (1. - (1. - Se **(1./mVG) )**mVG)**2.
        ! in cm/h
        K_unsat = Max(0., K_unsat) 
        K_unsat = Min(Ksat, K_unsat) 
        IF (K_unsat < 1.E-10) THEN
          K_unsat = 0.0
        ENDIF
      ENDIF

      RETURN
      END Function K_unsat
!=====================================================================
!-----------------------------------------------------------------------
!     Kunsat VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! WCr      Residual water content
!-----------------------------------------------------------------------
!     END FUNCTION Kunsat
!=======================================================================

!=======================================================================
!=======================================================================
!     Subroutine WaterTable_2D is an interface to the more generic
!       WaterTable subroutine which is used by both 1D and 2D models.
!     This routine translates the 1D outputs of the WaterTable routine
!       into 2D arrays and updates the soil water content for changes in 
!       water table management at the beginning of the day.
!-----------------------------------------------------------------------

      Subroutine WaterTable_2D(DYNAMIC, 
     &  CELLS, SOILPROP,                        !Input
     &  SW, SWV,                                !Input/Output
     &  ActWTD, netLatFlow, MgmtWTD, LIMIT_2D)  !Output

      USE CELLS_2D
      Implicit none
      EXTERNAL WaterTable

      INTEGER, INTENT(IN) :: DYNAMIC
      Type (CellType), INTENT(IN) :: Cells(MaxRows,MaxCols)
      TYPE (SoilType), INTENT(IN) :: SOILPROP
      REAL, DIMENSION(NL), INTENT(INOUT) :: SW
      REAL, DIMENSION(MaxRows,MaxCols), INTENT(INOUT) :: SWV
      REAL, INTENT(OUT) :: ActWTD, netLatFlow, MgmtWTD
      INTEGER, INTENT(OUT) :: LIMIT_2D

      REAL MaxDepth
      REAL, DIMENSION(NL) :: SWDELTW
      REAL, DIMENSION(MaxRows,MaxCols) :: SWVDeltW, Thick, Colfrac
      INTEGER i,j

!-----------------------------------------------------------------------
      MaxDepth = SOILPROP % DS(SOILPROP % NLAYR)
      Thick = CELLS % Struc % Thick
      Colfrac = BedDimension % Colfrac
      SWVDeltW = 0.0

!     Water table initialization
      CALL WaterTable(DYNAMIC,          
     &  SOILPROP, SW,                           !Input
     &  ActWTD, netLatFlow, MgmtWTD, SWDELTW)   !Output

!     Convert the soil water flux due to water table into 2D variable 
      CALL Interpolate2Cells_2D(
     &  CELLS%STRUC, SOILPROP, SWDELTW, 0.0,              !Input
     &  SWVDeltW)                                         !Output

!     Recalculate netLatFlow for raised bed case to remove effect of moving water 
!       out of area above the furrow.
      IF (BedDimension % RaisedBed) THEN
        netLatFlow = 0.0
        DO i = 1, SOILPROP % NLAYR
          DO j = 1, NColsTot
            SELECT CASE(CELLS(i,j) % STRUC % Cell_Type)
            CASE (3,4,5)
              netLatFlow = netLatFlow + 
     &           SWVDeltW(i,j) * Thick(i,j) * ColFrac(i,j) * 10.
            END SELECT
          ENDDO
        ENDDO
      ENDIF

!     ------------------------------------------------------------------------
!     Set SWV based on today's water table  
!     Note chp 2025-04-02: I tried this SWV update as a time-step update in the 
!         main routine, but it caused a lot more drainage and a lot more loss
!         of N, enough to stress the plant and depress yields.
!     Capillary rise should come with an upflux of N which we are not modeling.
!     For now, just stick with this daily update to SWV and assume that
!       N movement upward due to capillary rise is approximately equal to 
!       N movement downward due to extra drainage. 
      DO i = 1, SOILPROP % NLAYR
        SW(i) = SW(i) + SWDELTW(i)
        DO j = 1, NColsTot
          SELECT CASE(CELLS(i,j) % STRUC % Cell_Type)
          CASE (3,4,5)
            SWV(i,j) = SWV(i,j) + SWVDeltW(i,j)
          END SELECT
        ENDDO
      ENDDO
!     ------------------------------------------------------------------------

!     The 2D model is not needed in the vicinity of the water table.
!     Calculate the limits of the 2D model. 
      IF (ActWTD > MaxDepth) THEN
!       Water table is below profile depth
        LIMIT_2D = NRowsTot  
      Else          
!       Set LIMIT_2D to be the layer above ThetaCap = .9 * SAT
        LIMIT_2D = SOILPROP % NLAYR
        DO i = SOILPROP % NLAYR, 1, -1
          IF ((SW(i) - SOILPROP % DUL(i)) > 
     &        (0.9 * (SOILPROP % SAT(i) - SOILPROP % DUL(i)))) then
            LIMIT_2D = i - 1
          ELSE 
            EXIT
          ENDIF
        ENDDO
      ENDIF 
      LIMIT_2D = MAX(LIMIT_2D, 1)
      BedDimension % LIMIT_2D = LIMIT_2D

      Return
      End Subroutine WaterTable_2D

!=======================================================================
!=======================================================================
