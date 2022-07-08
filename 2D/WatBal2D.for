!=======================================================================
!  WatBal2D, Subroutine
!  2D water balance for drip irrigation bed with
!     plastic mulch cover.  

!     Order of calculations:
!     -- accept daily irrigation inputs from management module
!     -- input potential daily root water extraction and hourly  
!		    distribution
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
     &    EOP, ES, IRRAMT, SOILPROP, SOILPROP_FURROW,     !Input 
     &    WEATHER,                                        !Input
     &    Cells, SW, SWDELTS, SWFAC, TURFAC, TRWU, TRWUP) !Output

!-----------------------------------------------------------------------
      USE Cells_2D
      USE ModuleData
      IMPLICIT NONE
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
      INTEGER DripCol,DripRow,DYNAMIC,FurRow1, FurCol1, i, j, jj,idl
     
      Integer DripNumTot, iHr
      INTEGER NLAYR, HR, IrrigIndex, LIMIT_2D
      INTEGER DripNumTotArr(NDrpLn), IrrIdxArr(NDrpLn)
      INTEGER, DIMENSION(MaxRows,MaxCols) :: Cell_Type

!     temp chp
      INTEGER NextUpdate, Count, CritCell(2), DAYCOUNT

      REAL CN, BEDHT, BEDWD, CEP, CES, CRAIN
      REAL DRAIN_2D, HalfRow, HalfFurrow
      REAL DripSpc(NDrpLn), DripOfset(NDrpLn), DripDep(NDrpLn)
      REAL RAIN, RUNOFF, ES
      REAL TEP, TES, SRAD_TOT
      REAL TDRAIN, TRUNOF, TSW, TSWINI
      !INTEGER, PARAMETER :: MaxNEvent = 20  !Max number of irrigation event
      !REAL, DIMENSION(MaxNEvent) :: DripStart, DripDur, DripInt, DripRate
      REAL IrrRate(NDrpLn), RWUEP1

      REAL TimeIncr, MinTimeIncr, ROWSPC_cm, DayIncr
      REAL StartTime, EndTime, DeltaT
      REAL CumRad, LastCumRad, TSRadFrac, SUM_TSRF

      REAL SWFAC,  SWFAC_ts,  SWFAC_day
      REAL TURFAC, TURFAC_ts, TURFAC_day
      REAL ActWTD, MgmtWTD, LatFlow, LatFlow_ts !, MaxDif
      REAL StdIrrig, WidTot, DepTot
      
      REAL, DIMENSION(0:24) :: EOP_HR, CumFracRad
      REAL, DIMENSION(NL) :: BD, DLAYR, DS, DUL, Ksat, LL, SAT, WCr
      REAL, DIMENSION(NL) :: alphaVG, mVG, nVG, ThetaCap
      REAL, DIMENSION(MaxCols) :: WINF_col
      REAL, DIMENSION(MaxRows,MaxCols) :: CellArea, ES_mm, ColFrac
      REAL, DIMENSION(MaxRows,MaxCols) :: mm_2_vf, RLV_2D, RWU_2D
      REAL, DIMENSION(MaxRows,MaxCols) :: SWFv_ts, SWFlux_L, SWFlux_R
      REAL, DIMENSION(MaxRows,MaxCols) :: SWFlux_D, SWFlux_U
      REAL, DIMENSION(MaxRows,MaxCols) :: SWFh_ts, RWUP_2D, Se
      REAL, DIMENSION(MaxRows,MaxCols) :: Thick, Width, Kunsat, Diffus
      REAL, DIMENSION(MaxRows,MaxCols) :: EvapFlow, SWA
      REAL, ALLOCATABLE :: IrrigSched(:,:,:), DripRate(:,:),DripInt(:,:)
!      REAL, ALLOCATABLE :: DripDep(:,:), DripStart(:,:), DripDur(:,:)
      
      Double Precision DRAIN_ts, EOP_ts, ES_avg, ES_day, ES_ts
      Double Precision INF_vol, IRR_ts, IrrVol(NDrpLn),Rain_ts,Runoff_ts
      Double Precision TRWU_ts,TRWUP_ts,SW_VOL_tot

      REAL, DIMENSION(MaxRows,MaxCols) :: SWV
      Double precision, DIMENSION(MaxRows,MaxCols) :: SWV_D, SWV_avail
      Double precision, DIMENSION(MaxRows,MaxCols) :: SWV_ts, RWU_2D_ts
      Double precision, DIMENSION(MaxRows,MaxCols) :: RWUP_2D_ts, EP_vf,
     &       ES_vf_ts, INF_vol_dtal
!      Double precision, DIMENSION(MaxRows,MaxCols) :: SWV_LAST
      Double Precision, DIMENSION(MaxRows,MaxCols,0:24) :: ES_Hr

!     Functions
      REAL Time_interval, k_unsat, diffus_coef

      LOGICAL IRRIG, IRRIGArr(NDrpLn)

!!     temp chp
!      integer lun2

!     Default time steps durring irrigation and drying !minutes
      REAL, PARAMETER :: TSI = 5.0, TSN = 30.0, Max_Time_Step=60.
!                         irrig        rain        default  

!      Cell_detail%row = 10
!      Cell_detail%col = 4


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

      CALL GET('PLANT', 'RWUEP1', RWUEP1)

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
      Cell_Type = CELLS % Struc % CellType
      
!     Drip irrigation
      CALL GET(DripIrrig)
      DripOfset = DripIrrig % DripOfset
      DripDep   = DripIrrig % DripDep

!     Determine which column receives drip irrigation
      BedDimension % DripCol = -99
      BedDimension % DripRow = -99
      DO IDL = 1, NDripLnTOT
        BedDimension % DripCol(IDL) = 1 !default drip emitter located at center of bed
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
      END DO
!     Initial soil water content in single precision
      IF (BedDimension % RaisedBed) THEN
        DO i = 1, NRowsTot
          DO j = 1, NColsTot
  !         Set all initial soil water contents to DUL (soil will be wet after
  !           construction of raised bed).
            SELECT CASE(CELLS(i,j)%Struc%CellType)
              CASE (3,4,5)
                SWV(i,j) = SOILPROP%DUL(i)
                SWA(i,j) = SOILPROP%DUL(i) - SOILPROP%LL(i)
              CASE DEFAULT
                SWV(i,j) = 0.0
                SWA(i,j) = 0.0
            END SELECT
          ENDDO
        ENDDO
      ELSE
        DO i = 1, NRowsTot
          DO j = 1, NColsTot
  !         Set all initial soil water contents to DUL (soil will be wet after
  !           construction of raised bed).
            SELECT CASE(CELLS(i,j)%Struc%CellType)
              CASE (3,4,5)
                SWV(i,j) = SW(i)
                IF (SW(i) < SOILPROP%LL(i)) THEN
                    SW(i) = SOILPROP%LL(i)
                END IF
                SWA(i,j) = SW(i) - SOILPROP%LL(i)
              CASE DEFAULT
                SWV(i,j) = 0.0
                SWA(i,j) = 0.0
            END SELECT
          ENDDO
        ENDDO
      END IF
      
      CALL WaterTable_2D(SEASINIT,                        
     &  CELLS, SOILPROP, SWV,                   !Input
     &  LatFlow, MgmtWTD, ThetaCap)             !Output
      ! If there is water table, set the initial condition of SWV as ThetaCap  
      IF (MgmtWTD .LT. 9999.) THEN
        Do i =1, NLAYR
          DO j = 1, NColsTot
            SELECT CASE(CELLS(i,j)%Struc%CellType)
              CASE (3,4,5)
                SWV(i,j) = ThetaCap(i)
                SWA(i,j) = ThetaCap(i) - SOILPROP%LL(i)
              CASE DEFAULT
                SWV(i,j) = 0.0
                SWA(i,j) = 0.0
            END SELECT
          Enddo
        enddo
        Endif
      Call ThetaCapOp(DYNAMIC,CONTROL, ISWITCH,MgmtWTD, SOILPROP)
!     convert to double precision for time step loops
      SWV_D = DBLE(SWV)

      !CALL ArrayHandler(CELLS, CONTROL, SOILPROP, SWV, "SWV", 0.0, 0.5)

      CELLS%STATE%SWV = SWV
      CELLS%RATE%ES_Rate = 0.0
      CELLS%RATE%EP_Rate = 0.0

      CALL Drainage_2D(SEASINIT,  
     &    CELLS, Diffus, FurCol1, FurRow1, Kunsat,        !Input
     &    MgmtWTD, SOILPROP, SWV_D, TimeIncr, WCr,        !Input
     &    ThetaCap, Ksat,                                 !Input
     &    LatFlow,                          !Output
     &    SWV_ts, SWFh_ts, SWFv_ts)          !Output

!!     -------------------------------------------------------------------
!!     If a water table is present, need to calculate stable initial water
!!     content based on upflow from water table.
!      IF (MgmtWTD < DS(NLAYR)) THEN
!!     Run through loop to initialize soil water in upper layers 
!
!!       Minimum difference in SWV
!        SWV_last = SWV_D
!        SWV_avail = SWV_D
!        MaxDif = 1000.
!        Count = 0
!      
!        DO WHILE (MaxDif > 1.E-4)
!          TimeIncr = Max_Time_Step  !minutes
!          MaxDif = 0.0
!          DO i = 1, NRowsTot
!            DO j = 1, NColsTot
!              SELECT CASE(CELLS(i,j)%STRUC%CellType)
!              CASE (3,4,5);CONTINUE
!              CASE DEFAULT; CYCLE
!              END SELECT
!              Se(i,j) = (SWV_avail(i,j) - WCr(i))/(SAT(i) - WCr(i))
!              Se(i,j) = MIN(1.0, Se(i,j))
!              Se(i,j) = MAX(0.0, Se(i,j))
!              Kunsat(i,j)= K_unsat(Ksat(i), mVG(i), Se(i,j))
!              Diffus(i,j)= Diffus_Coef(Ksat(i), alphaVG(i), mVG(i),
!     &          SAT(i), Se(i,j), WCr(i))
!              IF (Diffus(i,j) > 1.E-9 .AND. Kunsat(i,j) > 1.E-9) THEN
!                DeltaT = 1./(
!     &            2.*Diffus(i,j)/(Width(i,j)*Width(i,j)) + 
!     &            2.*Diffus(i,j)/(Thick(i,j)*Thick(i,j)) + 
!     &            10.*Kunsat(i,j)/Thick(i,j)) * 60. !min 
!              ENDIF
!            ENDDO
!          ENDDO
!          Count = Count + 1
!
!          CALL Drainage_2D(RATE, 
!     &      CELLS, Diffus, FurCol1, FurRow1, Kunsat,        !Input
!     &      MgmtWTD, SOILPROP, SWV_avail, DeltaT, WCr,      !Input
!     &      LatFlow_ts,                                     !Output
!     &      SWV_ts, SWFh_ts, SWFv_ts)                       !Output
!
!          DO i = 1, NRowsTot
!            DO j = 1, NColsTot
!              IF (ABS(SWV_ts(i,j) - SWV_last(i,j)) > MaxDif) THEN
!                MaxDif = ABS(SWV_ts(i,j) - SWV_last(i,j))
!              ENDIF
!            ENDDO
!          ENDDO
!          SWV_last = SWV_ts
!          SWV_avail = SWV_ts
!        ENDDO 
!      
!        SWV_D = SWV_ts
!      ENDIF
!!     End of water table upflow initialization
!!     -------------------------------------------------------------------

      CALL ROOTWU_2D(SEASINIT, TimeIncr, 
     &    Cells, EOP_ts, SWV_avail,                       !Input 
     &    RWU_2D_ts, RWUP_2D_ts, TRWU_ts, TRWUP_ts)       !Output
     
!     Initialize summary variables
      CALL WBSUM_2D(SEASINIT,
     &    CELLS, DRAIN_2D, HalfRow, RAIN, RUNOFF, SWV,    !Input
     &    CES, CEP, CRAIN, TDRAIN, TES, TEP, TRUNOF,      !Output
     &    TSW, TSWINI)                                    !Output

      CALL Interpolate2Layers_2D(                    
     &  CELLS%State%SWV, CELLS%Struc, SOILPROP%NLAYR,     !Input
     &  SW)                                               !Output

!     Water balance output initialization
      CALL Wbal_2D(CONTROL, ISWITCH, COUNT, 
     &    CES, CEP, DRAIN_2D, RUNOFF, IRRAMT, RAIN, 
     &    TES, TEP, CRAIN, TDRAIN, TRUNOF, TSW,
     &    LatFlow, StdIrrig, ES, ES_DAY)
     
!     Call OPWBAL to write headers to output file
      CALL OPWBAL_2D(CONTROL, ISWITCH, 
     &    CRAIN, DLAYR, IRRAMT, LL, NLAYR, RUNOFF,        !Input
     &    SOILPROP, StdIrrig, SW, TDRAIN, TRUNOF,         !Input
     &    ActWTD, LatFlow, MgmtWTD, ThetaCap)             !Input 

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

      Diffus = 0.0
      Kunsat = 0.0

      Call Calc_SW_Vol(
     &  CellArea, Cell_Type, HalfRow, SWV_D,                !Input
     &  SW_vol_tot)                                         !Output
     
           
!      call SW_SensorH(SOILPROP, CONTROL, Cells, SWV, 0)
!      call SW_SensorD(SOILPROP, CONTROL, Cells, SWV)

      Call Wbal_2D_ts(CONTROL, ISWITCH, 0.0, 0.0, 
     &    DRAIN_ts, RUNOFF_ts, IRR_ts, RAIN_ts, 
     &    ES_TS, TRWU_ts, SW_vol_tot, CritCell, Diffus, Kunsat, 0.0,
     &    0, 0.0,
!         Temp chp
     &    CellArea, SWV_D, EP_vf, ES_vf_ts, IrrVol, 0.d0)

      print *, " "
      print *, "Start 2D, variable time-step model"

!      CALL GETLUN('RWU_2D.CSV',LUN2)
!      OPEN (UNIT=LUN2, FILE='RWU_2D.CSV')
!      WRITE(LUN2,'(A)') "2D, variable time step root water uptake"
!      WRITE(LUN2,'(A)') "TIME(d), EOP, TRWUP, TRWU, SWFAC, TURFAC"

!     TEMP CHP
      DAYCOUNT = 0

      !call SW_SensorH(SOILPROP, CONTROL, Cells, SWV, 0)
 !     call SW_SensorD(SOILPROP, CONTROL, Cells, SWV)
      
!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
      
      IF (ISWITCH%ISWWAT == 'N') RETURN

      CALL GET(DripIrrig)
 
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

      SWFAC  = 0.0
      TURFAC = 0.0
      SUM_TSRF = 0.0

      Cell_detail%IrrVol = 0
      Cell_detail%InfVol = 0

!     Compute water available at beginning of day
      SWV_avail = SWV_D
      if (CONTROL % YRDOY .EQ. 2011090 ) then
        continue
      endif
      if (SWV_D(10,4) .gt. 0.5) then 
        continue
      endif


!     Convert soil evaporation to volumetric fraction units, 
      ES_mm = CELLS%Rate%ES_rate

!     Upflow needed for N movement, units are cm2 to match flux units
      EvapFlow = 0.0
      IF (BedDimension % PMCover) then
        jj = FurCol1 ! If there is plastic cover, the infiltration is in the furrow
      else
        jj = 1
      endif
      !DO j = FurCol1, NColsTot
      DO j = jj, NColsTot
        EvapFlow(NLAYR,j) = ES_mm(NLAYR,j)/10. * Width(FurRow1,j)
!          cm2               =            cm        *    cm
        DO i = NLAYR-1, 1, -1
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

!     Get depth to managed water table 
      CALL WaterTable_2D(RATE,                        
     &  CELLS, SOILPROP, SWV,                   !Input
     &  LatFlow, MgmtWTD, ThetaCap)             !Output

      ! SWV should be previous'days SWV and is input. It is used to calculate the LatFlow due to water table depth change
      ! Here LatFlow is due to daily change of MgmtWTD
        
      ! After call WaterTable_2D to get theLIMIT_2D, set the soil water content below LIMIT_2D as ThetaCap
      LIMIT_2D = BedDimension % LIMIT_2D
      if (LIMIT_2D .LT. NLAYR) then
        DO i = LIMIT_2D+1, NLAYR
          DO j = 1, NColsTot
            !SWV_avail is Double precision cell soil water content at the beginning of time step in mm3/mm3
            SWV_avail(i,j) = ThetaCap(i)
          !  SWV_D(i, j) = DBLE(SWV_avail(i.j))
          Enddo
        Enddo
        SWV_D= DBLE(SWV_avail)
      Endif
      Call ThetaCapOp(DYNAMIC,CONTROL,ISWITCH,MgmtWTD, SOILPROP)
      ! Output ThetaCap
      ! CALL YR_DOY(CONTROL % YRDOY, YR2, DY2)
!        WRITE (LUNThetaCap,1300) !YR2, DY2, CONTROL % DAS, &
 !        ThetaCap(1), ThetaCap(3), ThetaCap(5), ThetaCap(7), &
  !       ThetaCap(9), ThetaCap(11), ThetaCap(13), ThetaCap(15) ,    &
  !       ThetaCap(17), ThetaCap(19), ThetaCap(21), ThetaCap(23)
!     Compute daily runoff from furrows and water available for infiltration
!       for each column in furrow.
      CALL Rnoff_furrow( 
     &  CN, ColFrac, FurCol1, FurRow1, HalfFurrow,        !Input
     &  HalfRow, LL, Rain, SAT, SWV_D,                    !Input
     &  RUNOFF, WINF_col)                                 !Output
      ! The rain water from plastic cover run to furrow and infitration

!-----------------------------------------------------------------
!     Drip irrigation schedule for today
!      DripDur   = DripIrrig % DripDur   !duration of ea. irrig (hr)
!      DripInt   = DripIrrig % DripInt   !interval between irrig (hr)
!      DripNum   = DripIrrig % DripNum   !# of irrigs for J'th entries today
!      DripRate  = DripIrrig % DripRate  !emitter rate (ml/s)* efficiency
      DripSpc   = DripIrrig % DripSpc   !emitter spacing (cm)
!      DripStart = DripIrrig % DripStart !start time (hr)
      IrrRate   = DripIrrig % IrrRate   !daily irrigation (mm)
      DripNumTotArr = 0                    !# of irrigs today
      DO IDL = 1, NDripLnTOT
        do J = 1, DripIrrig(IDL) % DripEvntEntr
          DripNumTotArr(IDL) = DripNumTotArr(IDL) + 
     &          DripIrrig(IDL) % DripNum(J)
        End do
      END DO
      !DripIrrig % DripEvntEntr
      IF (MAXVAL(DripNumTotArr) > 0 .AND. MAXVAL(IrrRate) > 1.E-6) THEN
        ALLOCATE (IrrigSched(NDripLnTOT,MAXVAL(DripNumTotArr),2))
!        ALLOCATE (DripDur(NDripLnTOT,MAXVAL(DripNumTotArr)))
        ALLOCATE (DripInt(NDripLnTOT,MAXVAL(DripNumTotArr)))
        ALLOCATE (DripRate(NDripLnTOT,MAXVAL(DripNumTotArr)))
!        ALLOCATE (DripStart(NDripLnTOT,MAXVAL(DripNumTotArr)))
        
        IrrigSched = 0.
!        DripDur = 0.
        DripInt = 0.
        DripRate = 0.
!        DripStart = 0.
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
!        ALLOCATE (DripDur(IDL,1))
        ALLOCATE (DripInt(IDL,1))
        ALLOCATE (DripRate(IDL,1))
!        ALLOCATE (DripStart(IDL,1))
        DripNumTotArr = 0
!        DripDur = 0
        DripInt = 24.
      ENDIF

      DO IDL = 1, NDripLnTOT
        DripNumTot = DripNumTotArr(IDL)
        ! Chek if the schedule time is valid
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
           ! Chek if the starting time of certain event is earlier than the end time of previous event
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
!     IF (.NOT. BedDimension % RaisedBed .AND. DripNum == 0) THEN
      IF (MAXVAL(DripNumTotArr) == 0) THEN ! Removed by Jin Wu on March, 2013 , StdIrrig is for dripper and non-dripper
        StdIrrig = IRRAMT ! for non-drip irrigation
        IF (StdIrrig < 1.E-6) THEN
          StdIrrig = 0.0
        ELSE
          DO J = 1, NColsTot
          !JZW Should be handled by CALL Rnoff_furrow to count runoff
            IF (BedDimension % PMCover) then
              WINF_col(j) = WINF_col(j) + 
     &           StdIrrig * ROWSPC_cm/(ROWSPC_cm - BedDimension % BEDWD)
              !Standard one dimension effective irrigation amount(mm)
              !The irrigation water on plastic cover run to furrow area
              !Rnoff_furrow calculated the iffitration for rain, here add the iffitration of sprinkle
            else 
              WINF_col(j) = WINF_col(j) + StdIrrig 
            Endif
          ENDDO
        ENDIF
      ELSE
        StdIrrig = 0.0 ! If there is drip irrigation, then no sprinkle
      ENDIF

!!     TEMP CHP
!      PRINT *, CONTROL.DAS

!-----------------------------------------------------------------
!     Time Loop
!-----------------------------------------------------------------
      StartTime = 0.0
      NextUPdate = 0
      Count = 0
      ES_day = 0.0
      LastCumRad = 0.0
      MinTimeIncr = 60.

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
!       TIME STEP
!       First determine unsaturated hydraulic conductivity and diffusivity
!       for each cell based on soil water content at beginning of time step
!       Also compute optimum time increment for stability
        TimeIncr = Max_Time_Step  !minutes
       
        CritCell = 0 
        DO i = 1, min(LIMIT_2D, NRowsTot)
          DO j = 1, NColsTot
            SELECT CASE(CELLS(i,j)%STRUC%CellType)
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
            IF (Diffus(i,j) > 1.E-9 .AND. Kunsat(i,j) > 1.E-9) THEN
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

!!     temp chp
!      TimeIncr = 1.0
!
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
             ! Actually, under above if statement, the TimeIncr is overwrite by the next if (IRRIG) elseif 
             !IF (DripInt(J) > 1.E-6) THEN
!            if (IrrigIndex < DripNumTot) Then
!              DripIntNow =  ! Check if it  is correct ?????
!     &          IrrigSched(IrrigIndex+1,1) - IrrigSched(IrrigIndex,2)
!            else 
!               DripIntNow = 0
!            endif
!          IF ( DripIntNow > 1.E-6) THEN
           !IF ( DripInt(IrrigIndex) > 1.E-6) THEN
            IF (IrrigIndex .LT. DripNumTot) then 
              IF (DripInt(IDL,IrrigIndex) > 1.E-6) DeltaT =
     &                 Time_interval(DripInt(IDL,IrrigIndex), TSN)  !minutes !TSN:Approximate time interval, min
              ! DeltaT = Time_interval(DripInt, TSN)  !minutes !TSN:Approximate time interval, min
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

!           Apply all irrigation to top row, DripCol column
            SWV_avail(DripRow,DripCol) = SWV_avail(DripRow,DripCol) + 
     &                           IrrVol(IDL)/ CellArea(DripRow,DripCol)
!          SWV_avail(1,DripCol) = SWV_avail(1,DripCol) + IrrVol(IDL)
!     &                                          / CellArea(1,DripCol)
            IRR_ts = IRR_ts + IrrVol(IDL) / HalfRow * 10.     !mm
          ENDIF
        END DO

!       ---------------------------------------------------------------
!       ADDITION OF INFILTRATION AMOUNT
!       Add infiltration to top furrow cells evenly throughout day.
        IF (RAIN > 1.E-6 .OR. StdIrrig > 1.E-6) THEN
          IF (BedDimension % PMCover) then
            jj = FurCol1 ! If there is plastic cover, the infiltration is in the furrow
            i = FurRow1
          else
            jj = 1
            i = 1
          endif
          DO j = jj, NColsTot
            if ((j .GE. FurCol1) .and. (i .eq. 1) ) i = FurRow1
          !DO j = FurCol1, NColsTot
!           Within furrow, add infiltration to top cells
            !INF_vol = WINF_col(j) * 0.1 * DayIncr / Thick(FurRow1,j)
            INF_vol = WINF_col(j) * 0.1 * DayIncr / Thick(i,j)
!           cm3[water]   mm[water]   cm         1           
!           ---------- = --------- * -- * d * --------
!            cm3[soil]       d       mm       cm[soil]   
!JZW find bug here if rain is large SWV_avail >sat; Drainage_2D can not handle if SW>SAT
!infitration not to Row=1, but to FurRow1
            ! SWV_avail(FurRow1,j) = SWV_avail(FurRow1,j) + INF_vol
            SWV_avail(i,j) = SWV_avail(i,j) + INF_vol
            ! temp chp
            INF_vol_dtal(i,j) = INF_vol
          ENDDO
          Runoff_ts = RUNOFF * DayIncr
          Rain_ts = RAIN * DayIncr ! in mm
          IRR_ts = IRR_ts + StdIrrig * DayIncr
        ENDIF

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
        ES_ts = 0.0
        IF (BedDimension % PMCover) then
          jj = FurCol1 ! If there is plastic cover, the infiltration is in the furrow
        else
          jj = 1
        endif
        DO i = FurRow1, NRowsTot
          !DO j = FurCol1, NColsTot
          DO j = jj, NColsTot
            SELECT CASE (CELLS(i,j)%STRUC%CellType)
            CASE (3,4,5)
!             mm/hr                  mm/d
              ES_avg = TSRadFrac * ES_mm(i,j)
!             mm total during time interval
              ES_ts = ES_ts + ES_avg 
!             Subtract from cell water by volume fraction
              SWV_avail(i,j) = SWV_avail(i,j) - ES_avg * mm_2_vf(i,j)  
            
!             temp chp
              es_vf_ts(i,j) = ES_avg * mm_2_vf(i,j)
            CASE DEFAULT; CYCLE
            END SELECT
          ENDDO
        ENDDO
        ES_day = ES_day + ES_ts

!       ---------------------------------------------------------------
!       ROOT WATER UPTAKE
        EOP_ts = TSRadFrac * EOP

        CALL ROOTWU_2D(RATE, TimeIncr, 
     &    Cells, EOP_ts, SWV_avail,                       !Input 
     &    RWU_2D_ts, RWUP_2D_ts, TRWU_ts, TRWUP_ts)       !Output

        CALL WaterStress(SNGL(EOP_ts), RWUEP1, SNGL(TRWUP_ts)/10., 
     &      SWFAC_ts, TURFAC_ts)
        SWFAC  = SWFAC  + SWFAC_ts  * TSRadFrac
        TURFAC = TURFAC + TURFAC_ts * TSRadFrac
        SUM_TSRF = SUM_TSRF + TSRadFrac

!       TEMP CHP
!        WRITE(LUN2,'(F10.5,19(",",F10.4))') 
!     &   CONTROL%DAS+EndTime/24.,
!     &   EOP_ts, TRWUP_ts, TRWU_ts, SWFAC_ts, TURFAC_ts

!       Calculate SW available for drainage -- reduce by 
!       root water uptake.
        DO i = 1, NRowsTot
          DO j = 1, NColsTot
            SELECT CASE (CELLS(i,j)%STRUC%CellType)
            CASE (3,4,5)
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

!       ---------------------------------------------------------------
!       HORIZONTAL AND VERTICAL WATER MOVEMENT
!       Drainage_2D computes both runoff and drainage for bed and furrow
        CALL Drainage_2D(RATE, 
     &    CELLS, Diffus, FurCol1, FurRow1, Kunsat,        !Input
     &    MgmtWTD, SOILPROP, SWV_avail, TimeIncr, WCr,    !Input
     &    ThetaCap, Ksat,                                 !Input
     &    LatFlow_ts,                                     !Output
     &    SWV_ts, SWFh_ts, SWFv_ts)                       !Output
        ! Here LatFlow_ts is due to the drainage of layer LIMIT_2D 
        ! here the drainage is from first layer to LIMIT_2D
        DRAIN_ts = 0.0
        DO j = 1, NColsTot
          ! DRAIN_ts = DRAIN_ts + SWFv_ts(NLayr,j) / HalfRow * 10.   !mm 
          DRAIN_ts = 
     &    DRAIN_ts + SWFv_ts(min(NLayr, LIMIT_2D),j) / HalfRow * 10. !mm
          
        ENDDO
        DRAIN_2D = DRAIN_2D + DRAIN_ts    

        CELLS%Rate%EP_rate = RWU_2D
        SWV_avail = SWV_ts
        SWV_D = SWV_ts
        LatFlow = LatFlow + LatFlow_ts

!       ---------------------------------------------------------------
!       Update soil water process accumulators
        DO i = 1, NRowsTot
          DO j = 1, NColsTot
            SELECT CASE (CELLS(i,j)%STRUC%CellType)
            CASE (3,4,5)
!             Potential and actual root water uptake (mm)
              RWU_2D(i,j) = RWU_2D(i,j) + RWU_2D_ts(i,j)
              RWUP_2D(i,j) = RWUP_2D(i,j) + RWUP_2D_ts(i,j)

!             Horizontal flow (cm2) left and right.
!             SWFlux_L and SWFlux_R are both positive values and represent
!               the amount of water flowing thru the L and R cell boundaries.
              IF (SWFh_ts(i,j) < -1.E-10) THEN
!               Negative horizontal flow = flow to the left from (i,j+1)
                SWFlux_L(i,j+1) = SWFLUX_L(i,j+1) - SWFh_ts(i,j)
              ELSEIF (SWFh_ts(i,j) > 1.E-10) THEN
!               Positive horizontal flow = flow to the right from (i,j)
                SWFlux_R(i,j) = SWFLUX_R(i,j) + SWFh_ts(i,j)
              ENDIF

!@ Need to consider upflow from water table for N movement. - Do this after water balance complete.
!              ! Add by Jin Wu in Feb. 2011
!              if (i .EQ. LIMIT_2D) THEN
!                 EvapFlow(LIMIT_2D,j) =
!     &                EvapFlow(LIMIT_2D,j) +CapilaryFlow(J) *TimeIncr
!                 Do iRow= LIMIT_2D, NLAYR !NRowsTot
!                    EvapFlow(LIMIT_2D,j) = EvapFlow(LIMIT_2D,j)+
!     &                EvapFlow(iRow,j)
!                 Enddo
!              Endif
!             Vertical flow (cm2) includes upflow.
!             SWFlux_U and SWFlux_D are both positive values and represent
!               the amount of water flowing thru the upper and lower cell boundaries.
! CHECK UNITS FOR Upflow_2D(i,j).  THESE SHOULD BE PER TIME STEP, NOT PER DAY!
              IF (SWFv_ts(i,j) < -1.E-10) THEN
!               Negative vertical flow = upward flow from (i+1,j)
                SWFlux_U(i+1,j) = SWFLUX_U(i+1,j) - SWFv_ts(i,j) 
!     &                  + EvapFlow(i,j) 
      !JZW here may be double countine with the line 876? EvapFlow(i,j) is daily instead of time step?
              ELSEIF (SWFv_ts(i,j) > 1.E-10) THEN
!               Positive vertical flow = downward flow from (i,j)
!                SWFlux_D(i,j+1) = SWFLUX_D(i,j+1) + SWFv_ts(i,j)
                SWFlux_D(i,j) = SWFLUX_D(i,j) + SWFv_ts(i,j)
              ENDIF
            CASE DEFAULT; CYCLE
            END SELECT
          ENDDO
        ENDDO
        
        Call Calc_SW_Vol(
     &    CellArea, Cell_Type, HalfRow, SWV_ts,             !Input
     &    SW_vol_tot)                                       !Output
        ! Output to SoilWat_ts.OUT and CellDetail.OUT
        if (IRR_ts .gt. 0) then
          continue
        endif
        Call Wbal_2D_ts(CONTROL, ISWITCH, EndTime, TimeIncr, !Input
     &    DRAIN_ts, RUNOFF_ts, IRR_ts, RAIN_ts,              !Input
     &    ES_TS, TRWU_ts, SW_vol_tot, CritCell,              !Input
     &    Diffus, Kunsat, LatFlow_ts, Count, LatFlow,        !Input for the 1st timestep, should not be LatFlow_ts
!         Temp chp
     &    CellArea, SWV_ts, EP_vf, ES_vf_ts, IrrVol, INF_vol_dtal) !Input

!       ---------------------------------------------------------------
       !       Update time for next iteration
        StartTime = EndTime ! if time step >1 hr, then start time = 0 will be missing
        iHr = nint(StartTime-TimeIncr/120.0-0.0001) !       Output arrays only once per hour
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
            SELECT CASE (CELLS(i,j)%STRUC%CellType)
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

!     Convert units from mm to cm for DSSAT plant routines.
      TRWUP = TRWUP / 10.
      TRWU  = TRWU  / 10.

!     temp chp
!     Compare daily average with accumulated values. Should be the 
!       same for SWFAC.  Should be different for TURFAC
      SWFAC  = SWFAC  / SUM_TSRF
      TURFAC = TURFAC / SUM_TSRF
      CALL WaterStress(EOP, RWUEP1, TRWUP, SWFAC_day, TURFAC_day)

      IF (Allocated(IrrigSched)) DEALLOCATE (IrrigSched)
      
!      IF (Allocated(DripDur)) DEALLOCATE (DripDur)
      IF (Allocated(DripInt)) DEALLOCATE (DripInt)
      IF (Allocated(DripRate)) DEALLOCATE (DripRate)
!      IF (Allocated(DripStart)) DEALLOCATE (DripStart)

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

!!     TEMP CHP
!      DAYCOUNT = DAYCOUNT + 1
!!     IF (DAYCOUNT == 10) THEN
!      IF (DAYCOUNT == 1) THEN
!        PRINT *, CONTROL%YRDOY, Rain, IrrVol
!        DAYCOUNT = 0
!      ENDIF

!***********************************************************************
!***********************************************************************
!     DAILY INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
      IF (ISWITCH%ISWWAT == 'N') RETURN

      ! ThetaCap calculated in the morning of the day from CapFringe is SW. We assume that the water balance is reached imediately for the area between water table and Limit_2D 
      ! Calculation the distribution to each layer (the model of distribution used here should be improved later)
      !We still use 2D SWV below LIMIT_2D, thus we may use the code for WBSUM_2D and do not need to create WBSUM_1D 
 !       DO i = LIMIT_2D + 1 , NLAYR 
 !         DO j = 1, NColsTot 
  !          SWV(i,j) = SW(i) 
 !         end do
  !      enddo
     
      CELLS % State % SWV = SWV

      CALL WBSUM_2D(INTEGR,
     &    CELLS, DRAIN_2D, HalfRow, RAIN, RUNOFF, SWV,    !Input
     &    CES, CEP, CRAIN, TDRAIN, TES, TEP, TRUNOF,      !Output
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
      CALL OPWBAL_2D(CONTROL, ISWITCH, 
     &    CRAIN, DLAYR, IRRAMT, LL, NLAYR, RUNOFF,        !Input
     &    SOILPROP, StdIrrig, SW, TDRAIN, TRUNOF,         !Input
     &    ActWTD, LatFlow, MgmtWTD, ThetaCap)             !Input 

!     Daily water balance output to SoilWatBal.OUT 
      CALL Wbal_2D(CONTROL, ISWITCH, COUNT, 
     &    CES, CEP, DRAIN_2D, RUNOFF, IRRAMT, RAIN, 
     &    TES, TEP, CRAIN, TDRAIN, TRUNOF, TSW,
     &    LatFlow, StdIrrig, ES, ES_DAY)
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
      
      CALL OPWBAL_2D(CONTROL, ISWITCH, 
     &    CRAIN, DLAYR, IRRAMT, LL, NLAYR, RUNOFF,        !Input
     &    SOILPROP, StdIrrig, SW, TDRAIN, TRUNOF,         !Input
     &    ActWTD, LatFlow, MgmtWTD, ThetaCap)             !Input 

!     Seasonal water balance output 
      CALL Wbal_2D(CONTROL, ISWITCH, COUNT, 
     &    CES, CEP, DRAIN_2D, RUNOFF, IRRAMT, RAIN, 
     &    TES, TEP, CRAIN, TDRAIN, TRUNOF, TSW,
     &    LatFlow, StdIrrig, ES, ES_DAY)

      Call Wbal_2D_ts(CONTROL, ISWITCH, 24.0, 0.0, 
     &    DRAIN_ts, RUNOFF_ts, IRR_ts, RAIN_ts, 
     &    ES_TS, TRWU_ts, SW_vol_tot, CritCell, 
     &    Diffus, Kunsat, LatFlow, 0, 0.0,
!         Temp chp
     &    CellArea, SWV_D, EP_vf, ES_vf_ts, 0.d0, 0.d0)

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
! MgmtWTD inputed fixed management water table depth. Counted from top to down
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
      Subroutine Rnoff_furrow( 
     &  CN, ColFrac, FurCol1, FurRow1, HalfFurrow,        !Input
     &  HalfRow, LL, Rain, SAT, SWV_D,                    !Input
     &  RUNOFF, WINF_col)                                 !Output

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
        IF (BedDimension % PMCover .AND. FurCol1 > NColsTot) THEN ! JZ : no furrow?
          RUNOFF = RAIN ! CHP does not remember why this statement. Thus we may remove
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
        !JZW question: for flat bed, the rain will run to furrow??
        !DO j = FurCol1, NColsTot
        DO j = jj, NColsTot
          SMX = 254.0 * (100.0/CN - 1.0)
!         Initial abstraction ratio
!         Runoff is related to the average soil water content of the top
!         two layers of soil
          SWABI = 0.15 * ((SAT(FurRow1) - SWV_D(FurRow1,j)) / 
     &                    (SAT(FurRow1) - LL(FurRow1) * 0.5) +
     &                    (SAT(FurRow1+1) - SWV_D(FurRow1+1,j)) / 
     &                    (SAT(FurRow1+1) - LL(FurRow1+1) * 0.5))
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
      END Subroutine Rnoff_furrow
!=======================================================================
C=====================================================================
!     Rnoff_furrow VARIABLE DEFINITIONS:
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
!     END SUBROUTINE Rnoff_furrow
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

