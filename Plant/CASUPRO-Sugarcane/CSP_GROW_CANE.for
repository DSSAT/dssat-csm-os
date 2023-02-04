C=======================================================================
C  CSP_GROW_CANE, Growth subroutine for CASUPRO sugarcane model, based 
C  on GROW Subroutine, J.W. Jones, F.S. Royce, O.H. Daza. 
C-----------------------------------------------------------------------
C  Growth and Development of Sugarcane
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  06/15/2005 FSR Written.
C  10/07/2005 FSR Modified for stalk-based photosynthesis 
C  06/30/2010 FSR Added PLF2 variable for CASUPRO
C-----------------------------------------------------------------------
!  Called by: CASUPRO
!  Calls: 
C=======================================================================
      SUBROUTINE CSP_GROW_CANE(CONTROL, DYNAMIC,
     &  CAB, CropTypeCode, DeltaLeafArea,                 !Input 
     &  DeltaLeafNum, DTPI, EXCESS, FILEIO,               !Input
     &  FRSU, GAMMA, GRLF, GRRT, GRST,                    !Input
     &  GRSU, Kill, LeafNum, MAINTR, NLAYR, NVEG0,        !Input
     &  PgAvalPD, PGAVAL, PgRatio, PhenoStage,            !Input
     &  PLTPOP, PLF1, PLF2, PLWT, SLA,                    !Input
     &  SLAREF,                                           !Input 
     &  LSFAC, SENRT, SLDOT, Smax, SRDOT, StkHrNO, StalkState, !Input
     &  StkB, StkM, SumTTStalk,                           !Input
     &  TURFAC, XDAY, XFRRT,                              !Input
     &  XFRSU, XLAI, XLAIlost, XSTKRT, YFRRT,             !Input
     &  YFRSU, YRDOY, YRPLT, YSLA, YSTKRT,                !Input
     &  CDEM, FRRT, TOTWT, TOTALWT, XHLAI,                !Input/Output
     &  BRDMD, CountStalk, DeltaLeafArealost,                  !Output 
     &  LAIMX, LAIXD, LeafArea, LeafArealost, LeafAreaPlant,   !Output
     &  LFWT, LFWTHa, LSWTlost, LSWT, LSWTHa, LFWTHalost,      !Output 
     &  LFWTlost, LSWTHalost, StkH2OFac, STKmntDEF,            !Output
     &  RTDWP, RTWTHa, SENESCE, StalkPopul, STKWT, STKFWT,     !Output
     &  STKFWTHa, STKWTHa, STKWTHalost, STKWTP, SuDEF,         !Output
     &  SuH2OFac, SUWT, SUWTHa, TOPWT)                         !Output
!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
!     NL defined in ModuleDefs.for 
      USE ModuleData

      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, TABEX, TIMDIF, YR_DOY
      SAVE
!-----------------------------------------------------------------------
      CHARACTER*1  RNMODE  
      CHARACTER*4 StalkState(NumOfStalks,10)
      !CHARACTER(11) :: CropTypeName
      CHARACTER*30 FILEIO
!     CHARACTER*92 FILECC, FILEGC
!     CHARACTER*6 ECONO

      INTEGER CAB, CountStalk, CropTypeCode 
      INTEGER DAP, DAS, Day, DOY, DYNAMIC, I, L
      INTEGER LAIXD, NLAYR, NVEG0, OpenStatus  
      INTEGER PhenoStage, REPNO, RUN, Smax, Stalk, TIMDIF
      INTEGER XDAY(6), YEAR, YRDOY, YRPLT, YRSIM, CGLUN

      INTEGER, DIMENSION(1:NumOfStalks) :: Kill
      
      REAL BRDMD, CAVLF, CAVR, CDEF, DTPI, FRRT !CANHT, CANWH, 
      REAL FRSU, LAIMX !, FINREF
      REAL GAMMA, GRLF, GRLS, GRRT, GRST, GRSU, LAIMX_Previous !, KCAN
      REAL LSFAC, PgAvalPD, PLF1, PLF2, PLTPOP, PLWT, RFPSTKG 
      REAL RootFac, SeedCH2O, ShootCTotDem, ShootCTotSup !, ROWSPC
      REAL SLA !, SLAMIN, SLAMAX, SLAMX, SLAMN
      REAL Stk_H2O, StkHrNO, SRDOT !, SLAPAR
      REAL STK, StkB, STKDMC, STKDWtmp, StkH2OFac, StkM, SUDWtmp 
      REAL SumLfWt, SumLSWt, SumRtWt, SuH2OFac 
      REAL SumStkTiWt, SumStkSuWt, TURFAC !, TURFSL 
      REAL XHLAI, XLAI, XLAIlost !TURSLA, 
!      REAL xTEMP !PAR, 
!     REAL AGEFAC, ,PGAVL,  DTX

      REAL SLAREF, SLAVAR, StkEx, SuEx, TOPWT, TOTWT !, VSTAGE
      REAL WholeLeaf
      REAL XFRRT(4), XFRSU(4), YFRRT(4), YFRSU(4), YSLA(6)
      REAL XSTKRT(6), YSTKRT(6) !TGRO(TS), XSLATM(10), YSLATM(10), 
!     REAL XVSHT(10), YVSHT(10), ZVSDI(10)
      REAL TABEX  ! Function subroutine - Lookup utility
!     REAL SIZELF, SIZREF, , SLNDOT 
      REAL DAYS(6)

      REAL, DIMENSION(0:NumOfDays) ::       CDEM, 
     &                LeafAreaPlant, LeafAreaPlantlost, 
     &                LFDWHalost, LSDWHalost, LFWTHa, LFWTHalost, 
     &				LFWTP, LSWTP, 
     &                LSWTHa, LSWTHalost, LFWTPlost, LSWTPlost, RTDWP,
     &                RTWTHa, RTWTP, RTWTP2, SenesHa,      
     &                StalkPopul, STKDWHalost, STKFWTHa,
     &				STKWTHa, STKWTHalost,   
     &                STKWTPlost, STKFWTP, STKWTP, SUWTHa, SUWTP

      REAL, DIMENSION(1:NumOfStalks) :: EXCESS, GroDEF, 
     &                MAINTR, PGAVAL, PGLEFT, PgRatio, SenLfNat,  
     &                SenLfShd, SenLfWat, SLDOT, SuDEF, TOTALWT, temp    

      REAL, DIMENSION(0:NumOfDays, NumOfStalks) :: CAV, CDEMND, 
     &                DeltaLeafNum, DeltaLeafArea, DeltaLeafArealost,  
     &                LeafArea, LeafArealost, LeafNum, LFDPW, LSDPW, 
     &                LFDW, LSDW, LFDWlost, LSDWlost, LFWT, LSWT, 
     &                LFWTlost, LSWTlost, LFWTA,   
     &                RatioSDC, RTDPW, RTDW, RTWT, ShootCDEM, 
     &                ShootCSUP, STKDPW, STKDW,    ! StkCDEM, 
     &                STKmntDEF, STKFWT, STKWT, STKWTA, STKWTlost, 
     &                SUDPW, SUDW, SUWT, SumTTStalk       

!-----------------------------------------------------------------------
!     Surface and soil residue due to daily senescence of plant matter
      REAL SENRT(NL)
      REAL SenWt(0:NL)        !kg[dry matter]/ha
      !REAL SenE(0:NL,NELEM)   !kg[E]/ha (E=N, P, S,...)
!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL
      Type (ResidueType) SENESCE
      TYPE (SwitchType)  ISWITCH
!     Type (WeatherType) WEATHER

!     Transfer values from constructed data types into local variables.
      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      REPNO   = CONTROL % REPNO  ! FSR
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE ! FSR
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM

!-----------------------------------------------------------------------
      
! Days after planting
      DAP = MAX(0,TIMDIF(YRPLT,YRDOY))

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      CDEF     = 0.0
      CountStalk = 0
      FRRT     = 0.0  ! Brought from CSP_DEMAND.for
      LAIMX    = 0.0      
      TOPWT    = 0.0
      XHLAI    = 0.0
      XLAIlost = 0.0      
      SLAREF   = 0.0
      Stalk    = 0
      SLAVAR = 90  
      GRLS   = GRLF

      SenWt  = 0.0   
      SENESCE % ResWt  = 0.0
      SENESCE % ResE   = 0.0
!-----------------------------------------------------------------------
      IF (CropTypeCode < 2) THEN ! Plant crop 
!    From seed cane per ha weight [kg fresh weight], calculate 
!    CH2O available for early growth [grams]
!     g/plant = kg/ha * m2/plant * ha/m2 * gr/kg * CH2O/fresh cane 
         SeedCH2O = PLWT * 1/PLTPOP * 0.0001 * 1000 * .07
      ELSE ! Ratoon crop
         SeedCH2O = (PLWT * 1/PLTPOP * 0.0001 * 1000 * .07) * StkHrNO 
      END IF

!-----------------------------------------------------------------------
! Initialization of variables for stalk appearance and leaf area
      DO Day = 0, NumOfDays
          CDEM(Day) = 0.0
          LeafAreaPlant(Day) = 0.0
          LeafAreaPlantlost(Day) = 0.0
          LFDWHalost(Day) = 0.0
          LSDWHalost(Day) = 0.0
          LFWTHa(Day) = 0.0
          LSWTHa(Day) = 0.0
          LFWTHalost(Day) = 0.0
          LSWTHalost(Day) = 0.0
          LFWTP(Day)  = 0.0
          LSWTP(Day)  = 0.0
          LFWTPlost(Day) = 0.0
          LSWTPlost(Day) = 0.0 
          
          RTWTHa(Day) = 0.0
          RTWTP(Day)  = 0.0            !new
          RTWTP2(Day)  = 0.0           !new
          RTDWP(Day)  = 0.0     
          SenesHa(Day) = 0.0
          STKDWHalost(Day) = 0.0
          STKFWTHa(Day) = 0.0
          STKWTHa(Day)= 0.0
          STKWTHalost(Day) = 0.0
          STKFWTP(Day) = 0.0
          STKWTP(Day) = 0.0
          SUWTHa(Day) = 0.0
          STKWTPlost(Day) = 0.0
          SUWTP(Day)  = 0.0
          StalkPopul(Day) = 0.0
      END DO ! Day

      DO Stalk = 1, NumOfStalks
          EXCESS(Stalk)   = 0.0
          GroDEF(Stalk)   = 0.0
          MAINTR(Stalk)   = 0.0
          PGAVAL(Stalk)   = 0.0
          PGLEFT(Stalk)   = 0.0
          PgRatio(Stalk)   = 0.0
          SenLfNat(Stalk)   = 0.0
          SenLfShd(Stalk)   = 0.0
          SenLfWat(Stalk)   = 0.0
          SLDOT(Stalk)   = 0.0
          SuDEF(Stalk)   = 0.0
          TOTALWT(Stalk)   = 0.0
          temp(Stalk)   = 0.0
      END DO  ! Stalk

      DO Day = 0, NumOfDays
        DO Stalk = 1, NumOfStalks
          CAV(Day, Stalk)         = 0.0
          CDEMND(Day, Stalk)      = 0.0
          DeltaLeafArealost(Day, Stalk)= 0.0
          LeafArea(Day,Stalk)     = 0.0          
          LeafArealost(Day,Stalk) = 0.0     
          LFDPW(Day, Stalk)       = 0.0     
          LSDPW(Day, Stalk)       = 0.0          
          LFDW(Day, Stalk)        = 0.0          
          LSDW(Day, Stalk)        = 0.0
          LFWT(Day, Stalk)        = 0.0
          LSWT(Day, Stalk)        = 0.0     
          LFDWlost(Day, Stalk)    = 0.0    
          LSDWlost(Day, Stalk)    = 0.0
          LFWTlost(Day, Stalk)    = 0.0
          LSWTlost(Day, Stalk)    = 0.0
          LFWTA(Day, Stalk)       = 0.0
          RTDPW(DAY, Stalk)       = 0.0
          RTDW(DAY, Stalk)        = 0.0
          RTWT(DAY, Stalk)        = 0.0 
          ShootCDEM(Day,Stalk)    = 0.0
          ShootCSUP(Day,Stalk)    = 0.0
          STKDPW(Day,Stalk)       = 0.0
          STKDW(Day, Stalk)       = 0.0
          STKmntDEF(Day,Stalk)    = 0.0
          STKFWT(Day, Stalk)      = 0.0
          STKWT(Day, Stalk)       = 0.0
          STKWTA(DAY, Stalk)      = 0.0
          STKWTlost(Day,Stalk)    = 0.0
          SUDPW(DAS,Stalk)        = 0.0
          SUDW(Day, Stalk)        = 0.0
          SUWT(Day, Stalk)        = 0.0
        END DO  ! Stalk
      END DO ! Day

!---------------------------------------------------------------------   
      CALL GET(ISWITCH)
      IF (INDEX('Y'  ,ISWITCH%IDETG) > 0 .AND. 
     &    INDEX('YDA',ISWITCH%IDETL) > 0) THEN

! Open file to write results from CSP_Grow_Cane.for
        CALL GETLUN('CSP_GRO',CGLUN)
        OPEN(UNIT = CGLUN, FILE = "CSP_GrowCane.OUT", STATUS ="UNKNOWN",
     &    ACTION = "WRITE", POSITION = "APPEND", IOSTAT = OpenStatus)
        WRITE(CGLUN,'("*GROWTH ASPECTS OUTPUT FILE", 
     &       " - RESULTS FROM CSP_GROW_CANE.for")')

!Write headers
        CALL HEADER(SEASINIT, CGLUN, RUN)

!  Units:   ----------------------------------------------------------   
        WRITE(CGLUN,'("!"110X,"senesced")')
        WRITE(CGLUN,'("!"53X,"kg/ha   kg/ha  kg/ha   kg/ha   /m2
     &    m2/p  #/p  kg/p  kg/ha  kg/ha ")')
!---------------------------------------------------------------------   
        WRITE(CGLUN,'("@YEAR DOY DAS DAP PHSTG   LAI LAIlost   PGAVL
     &   SLA    LWAD    SWAD   RWAD    SUAD  SNAD    LAPD SNPD STWPD 
     &   Leaf  Stalk")',ADVANCE="NO")

        DO Stalk = 1, Smax
          WRITE(CGLUN,'(" SWD",I2.2)',ADVANCE="NO") Stalk
        END DO
        
        DO Stalk = 1, Smax
          WRITE(CGLUN,'(" SUW",I2.2)',ADVANCE="NO") Stalk
        END DO
        
        DO Stalk = 1, Smax
          WRITE(CGLUN,'(" LWD",I2.2)',ADVANCE="NO") Stalk
        END DO
              
        DO Stalk = 1, Smax
          WRITE(CGLUN,'(" LND",I2.2)',ADVANCE="NO") Stalk
        END DO
        
        DO Stalk = 1, Smax
          WRITE(CGLUN,'("  LAD",I2.2)',ADVANCE="NO") Stalk
        END DO
      ENDIF

C-----------------------------------------------------------------------
C     SET VARIETY SPECIFIC LEAF PARAMETERS
!     Scaling of leaf parameters of current cultivar with respect to 
!     the selected cultivar
C-----------------------------------------------------------------------
!!!        DUMFAC = SLAVAR / 105  ! was SLAREF from SPE file
!!!        F      = DUMFAC * FINREF
!!!        FVEG   = DUMFAC * SLAMAX
!!!        SLAMN  = DUMFAC * SLAMIN
!!!        SLAMX  = DUMFAC * SLAMAX
!***********************************************************************
!***********************************************************************
!  Daily Rate calculations
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. RATE) THEN

      CountStalk = 0
      ShootCTotDem  = 0.
      ShootCTotSup  = 0.
      StkEx      = 0.
      SuEx       = 0.
      SumStkTiWt = 0.
      SumStkSuWt = 0.
	SumLfWt    = 0.
	SumLSWt    = 0.
	SumRtWt    = 0.

      DO Stalk = 1, Smax
        SuDEF(Stalk) = 0.0
      End Do

!-----------------------------------------------------------------------
C     Compute F, specific leaf area for new leaf weight
C-----------------------------------------------------------------------
! TPHFAC    Reduction in specific leaf area due to daytime temperature 
!           being less than optimal (0-1) 

!!!     TPHFAC = 0.
!!!      DO I = 1,24
!!!        TPHFAC = TPHFAC + TABEX (YSLATM,XSLATM,TGRO(I),5)
!!!      ENDDO
!!!      TPHFAC = TPHFAC/24.
C-----------------------------------------------------------------------

!!!      PARSLA = (SLAMN + (SLAMX - SLAMN) * EXP(SLAPAR * PAR)) / SLAMX

!!!      TURFSL = MAX(0.1, (1.0 - (1.0 - TURFAC) * TURSLA))
!	 TURFSL = 1.0  ! temporary diagnosis FSR
C-----------------------------------------------------------------------
! Compute overall effect of TMP, PAR, water stress on SLA (F)

!  First for veg stages, then transition to rep stage from R1 to 
C  end leaf effect of PAR on SLA, COX PEANUT SCI. 5:27, 1978
C-----------------------------------------------------------------------
!!!      FFVEG = FVEG * TPHFAC * PARSLA * TURFSL
!!!      F = FFVEG
!      IF (XFRT*FRACDN .GE. 0.05) F = FFVEG * (1.0 - XFRT * FRACDN)
!-----------------------------------------------------------------------
! Compute SLAREF based on six observations: XDAY(6) days after planting
! and YSLA(6).  With sufficient data, may change DAP for thermal time.

      IF (DAP <= XDAY(1)) THEN
          SLAREF = YSLA(1)

        ELSEIF (DAP > XDAY(1) .AND. DAP < XDAY(6)) THEN
!         Call to TABEX requires 'REAL' arguments
          DO I = 1, 6
            DAYS(I) = FLOAT(XDAY(I))
          ENDDO
          SLAREF =  TABEX(YSLA, DAYS, FLOAT(DAP), 6) 
        ELSEIF  (DAP >= XDAY(6)) THEN
          SLAREF = YSLA(6)

      ENDIF
!-----------------------------------------------------------------------
! Potential Stalk Growth
!
! OHD code & comments from his CSP_DEMAND subroutine is as follows:
!
! This is a temporary function deduced from experiment Lote 14,
! used here as an approximation for the time being. OHD
!
! PGROSTK = (2.8929 * VSTAGE ** 2 + 9.3593 * VSTAGE) / SKPOP
!          
! The following function is the 1st derivative of the equation above OHD
! 
! RFPSTKG = (5.7858 * VSTAGE + 9.3593) / SKPOP * TURFAC
!
! RFPSTKG Rate Function of Potential STalK Growth (g [stalk]/m2 - leaf)
! (g [stalk] / stalk - leaf) = (g [stalk] / m2 - leaf) / (stalks/m2)
!
! FSR - This function may currently be a weak link in growth simulation.
! Since VSTAGE = LeafNum(DAS,1), the equation had to be modified to 
! simulate development of each (not just primary) stalk. Replaced VSTAGE
! with LeafNum(DAS,Stalk) and DLFN with DeltaLeafNum(DAS,Stalk).  
! 
! RFPSTKG = (5.7858 * LeafNum(DAS-1, Stalk) + 9.3593) / SKPOP * TURFAC
! Note: Use DAS-1 because LeafNum is the result of the (previous day's) 
! INTEGR, which is being used here in calculating a RATE.
!
!         DPSGRF  = NewStalk * RFPSTKG * DLFN  
!         DPSTKG  = DPSTK * PLTPOP
!
!    DLFN     DeltaLeafNum(DAS,1) (leaf?)
!    DPSGRF   Potential growth of stalk of reference cane variety
!              (g [stalk]/plant) = (stalks/plant)*(g [stalk]/stalk-leaf) * leaf
!    DPSTK    Potential growth of stalk of selected cane variety 
!              (g [stalk]/plant) = ratio * (g [stalk]/plant)
!    DPSTKG   Potential increase of stalk growth 
!              g [stalk]/m2 = (g [stalk]/plant) (plants/m2)

       DO Stalk = 1, Smax

! Section for stalk weight growth rate
!
!       Calculates the rate of increase of stalk weight for each stalk.
!       Original OHD constants were arbitrarily adjusted to generate more
!       reasonable estimates using per-stalk growth. 
!
C Note: Parameter CAB postpones demand for stalk tissue until after leaves 
C comprising "cabbage" have formed.  Note that there are fewer internodes than  
C leaves (or at least very short internodes due to stalkless "cabbage" leaf group) 
C and stalk does not become apparent until the "great growth" period begins. 
!
          IF ((LeafNum(DAS-1,Stalk) >= CAB) .AND.
     &        (StalkState(Stalk,1) .EQ. 'LIVE')) THEN

!!!           RFPSTKG = (StkM * LeafNum(DAS-1, Stalk) + StkB) / TURFAC
! The above formulation, dividing by TURFAC, does not make sense.  
! TURFAC is a type of multiplier, inhibiting growth as it decreases.
! Since TURFAC = 1.0 is no stress, I will simply change / to *.  

              RFPSTKG = (StkM * LeafNum(DAS-1, Stalk) + StkB) * TURFAC

              STKDPW(DAS,Stalk) = RFPSTKG * DeltaLeafNum(DAS,Stalk)

          ELSE
              STKDPW(DAS,Stalk) = 0
          END IF

! Where: (note: Some units in OHD comments were ambiguous; this attempts to clarify) 
!    DeltaLeafNum(DAS,Stalk) - increase in # of leaves / stalk / day
!    RFPSTKG  Rate function of potential stalk growth (not stalk-order specific)
!              (g [stalk] / stalk - leaf) = (g [stalk] / m2 - leaf) / (stalks/m2)       
!    STKDPW(i,j)   Potential increase in stalk weight; day i and stalk j (FSR)
!                   (g[stalk]/stalk) = (g[stalk]/stalk-leaf) * leaf  
!    StkB      Constant used in calculation the rate of increase of stalk weight
!               for each stalk. (OHD equation)
!    StkM      Coefficient used in calculation the rate of increase of stalk weight 
!               for each stalk. (OHD equation) 
!    TURFAC    Water stress factor for expansion (0 - 1) 
!               1 is no stress, 0 is full stress
!***********************************************************************
!**** New method for stalk (tiller) DM and sugar weight calculation ****
! We should replace the above algorithm with a node length and diameter
! function, using the Tabex trio in the ECO file:
! XVSHT   Node (v-stage or VS) number, reference stalk 
! YVSHT   Mature internode length, reference stalk (cm)
! ZVSDI   Mature internode diameter, reference stalk (cm)
! These parameters are already present in this sub-routine.
!***********************************************************************
! Potential Leaf and Leaf-Sheath Growth
!
!    LFDPW (i,j)   Potential increase in leaf weight; day i and stalk j (FSR)   
!                  (g[leaftissue]/stalk-d) = g[leaftissue]/cm2 * cm2/stalk-d 
!    LSDPW (i,j)   Potential increase in leaf sheath weight; day i and stalk j
!
! Section for leaf weight growth rate
! Calculates the potential rate of increase of leaf weight for each stalk

          IF (StalkState(Stalk,1) .EQ. 'LIVE') THEN 

              LFDPW(DAS,Stalk) =  (1/SLAREF) * DeltaLeafArea(DAS,Stalk) 
            LSDPW(DAS,Stalk) =  LSFAC * LFDPW(DAS,Stalk) ! sheath as a 
                                                         ! proportion of
          ELSE                                           ! leaf DM
              LFDPW(DAS,Stalk) = 0.
            LSDPW(DAS,Stalk) = 0. 
          END IF  
!-----------------------------------------------------------------------
!  Potential Root Growth
!
! RTDPW Potential increase of root growth (g [root] / stalk / day
! FRRT  Fraction of potential leaf and stalk growth that is used to  
!       determine potential root growth.
! OHD used the main stalk leafnumber (XFRRT) in a TABEX function, 
! to calculate FRRT (YFRRT). 
! Potential root growth is a fraction of the sum of rates of potential 
! leaf growth and potential stalk growth.
! When TURFAC=1 (No water stress) the right most term becomes 1 (OHD)
!      RTDPW(DAS) = FRRT * AGADPW(DAS) * (2 - TURFAC)
! FSR changed these to by-stalk functions, substituting 
! [LeafNum(DAS-1, Stalk)] for VSTAGE, and 
! [STKDPW(DAS,Stalk) + LFDPW(DAS,Stalk)] for AGADPW(DAS). 
! and also added RootFac, to push more of the root demand on the primary
! and other early stalks during early stool development (PhenoStage < 4).
!
      IF (CropTypeCode < 2) THEN ! Plant (not ratoon) crop

         IF (StalkState(Stalk,1) .EQ. 'LIVE') THEN     
            IF (PhenoStage < 4) THEN   

!              STK = Stalk ! convert Stalk value from int to real
              RootFac = TABEX(YSTKRT, XSTKRT, STK, 6) 

            ELSE
              
              RootFac = 1.0
              
            END IF  ! (NewStalk > 6)
            
              FRRT = TABEX(YFRRT, XFRRT, LeafNum(DAS-1, Stalk), 4)

              RTDPW(DAS,Stalk) = FRRT * RootFac
     &           * (STKDPW(DAS,Stalk)+LFDPW(DAS,Stalk)+LSDPW(DAS,Stalk))
     &           * (2 - TURFAC) 
          ELSE ! Stalk not LIVE
              RTDPW(DAS,Stalk) = 0.
              
          END IF  ! (StalkState(Stalk,1) .EQ. 'LIVE')

      ELSE  ! (i.e., CropTypeCode >= 2) 
            !  Ratoon crop version of root growth.  No RootFac. 

           IF (StalkState(Stalk,1) .EQ. 'LIVE') THEN     

              FRRT = TABEX(YFRRT, XFRRT, LeafNum(DAS-1, Stalk), 4)

              RTDPW(DAS,Stalk) = FRRT 
     &           * (STKDPW(DAS,Stalk)+LFDPW(DAS,Stalk)+LSDPW(DAS,Stalk))
     &           * (2 - TURFAC)
          ELSE
              RTDPW(DAS,Stalk) = 0.
              
          END IF  ! (StalkState(Stalk,1) .EQ. 'LIVE')

      END IF ! (CropTypeCode < 2) 

!-----------------------------------------------------------------------
!  Potential sucrose Production
!
!        FRSU = TABEX(YFRSU, XFRSU, VSTAGE, 4)
!
! Potential sucrose growth is a fraction of the sum of rates of potential 
! leaf growth and potential stalk growth
!
!        DPSUG = FRSU * (DPLFG + DPSTKG)
!
! FRSU        fraction of potential leaf and stalk growth that is 
!             depositioned as sucrose
! DPLFG       potential increase of leaf growth (g [leaf] / plant)
! DPSUG       potential increase of sucrose accumulation (g [sucrose] / plant)
! SUDPW(i,j)  Potential increase in sucrose weight; day i and stalk j
!             (g[sucrose]/stalk-d)    

C Note: Parameter CAB postpones demand for stalk tissue until after leaves 
C comprising "cabbage" have formed, therefore no sugars should be produced.  

  
          IF ((LeafNum(DAS-1,Stalk) >= CAB) .AND.
     &        (StalkState(Stalk,1) .EQ. 'LIVE')) THEN

              FRSU = TABEX(YFRSU, XFRSU, LeafNum(DAS-1, Stalk), 4)

              SUDPW(DAS,Stalk) = FRSU * (STKDPW(DAS,Stalk)
     &                         + LFDPW(DAS,Stalk)+LSDPW(DAS,Stalk))

          ELSE
              SUDPW(DAS,Stalk) = 0.

          END IF 

       END DO  !  (Stalk = 1, Smax)
!-----------------------------------------------------------------------

!  Calculate CH2O Demand based on Potential Growth

!      Daily potential CH2O demand for per-stalk leaf, stalk, root and 
!      sucrose growth.

!-------- Determine carbon demand of each tiller (not primary) SHOOT
!           Primary stalk receives early C from seed cane, not
!           from other stalks ShootCDEM   

      DO Stalk = 1, Smax

       IF (StalkState(Stalk,2) .EQ. 'TILR' .AND. 
     &     LeafNum(DAS-1,Stalk)<CAB) THEN
!
            ShootCDEM(DAS,Stalk)  = (STKDPW(DAS,Stalk) * GRST) 
     &                            + (LFDPW(DAS,Stalk)  * GRLF) 
     &                            + (LSDPW(DAS,Stalk)  * GRLF)
!                                                         GRLF for sheath
     &                            + (RTDPW(DAS,Stalk)  * GRRT)
     &                            + (SUDPW(DAS,Stalk)  * GRSU)
          
!  ShootCTotDem is demand for shoot CH2O that will not come from shoot itself.
            ShootCTotDem = ShootCTotDem 
     &                  + MAX(0.,(ShootCDEM(DAS,Stalk) - PGAVAL(Stalk)))
         ELSE
            ShootCDEM(DAS,Stalk) = 0.0
       ENDIF ! (StalkState(Stalk,2) .EQ. 'TILR' .AND.
        
      END DO 

!-------- Determine portion of shoot carbon for each stalk to supply

      DO  Stalk = 1, Smax

              ShootCSUP(DAS,Stalk) = ShootCTotDem * PgRatio(Stalk)
              !  This is last use of ShootCTotDem
      END DO 

!-------- Determine carbon demand for each STALK (and primary shoot)
!         Include C supplied to meet shoot demand

      DO Stalk = 1, Smax

        IF (LeafNum(DAS-1, Stalk) >= CAB .OR.
     &      StalkState(Stalk,2) .EQ. 'PRIM') THEN

            CDEMND(DAS,Stalk) = (STKDPW(DAS,Stalk) * GRST) 
     &                        + (LFDPW(DAS,Stalk)  * GRLF) 
     &                        + (LSDPW(DAS,Stalk)  * GRLF) 
     &                        + (RTDPW(DAS,Stalk)  * GRRT)
     &                        + (SUDPW(DAS,Stalk)  * GRSU)
     &                        +  ShootCSUP(DAS,Stalk)

          CDEMND(DAS,Stalk) = CDEMND(DAS,Stalk)  ! temp for debugging   
!       ShootCSUP, provides CH2O for (non-primary) shoots 
!       having < CAB leaves.  Outer stalks with higher light interception
!       supply CH2O to pool, even when small
         ELSE
            CDEMND(DAS,Stalk) = 0.0  ! for LeafNum(DAS-1, Stalk) < CAB
        ENDIF ! (LeafNum(DAS-1, Stalk) >= CAB)
      END DO 
      
!-----------------------------------------------------------------------
!  Negative PGAVAL indicates CH2O from PG is less than maintenance 
!  respiration need.  No CH2O from PG is available for growth, and stored 
!  sucrose may be used to make up CH2O deficit (except for leaf maintenance).
!  If insufficient stored sucrose is available, the deficit amount is 
!  recorded as STKmntDEF(DAS,Stalk) and will be used to drive senescence of
!  leaves and stalks.  FSR
!  
!  FSR - Stalks clearly loose leaves while full of sucrose, so leaf maintenance
!  was separated from stalk maintenance.  The question is to what extent stalks
!  will senesce while containing sucrose.  If the sucrose is delaying stalk
!  senescence  unrealistically, the amount of sucrose permitted to subsidize 
!  maintenance will be limited in the code below.
!  
!  If PGAVAL is positive, there will be PG for stalk growth; if
!  negative, either stored sucrose will meet maintenance needs, or a 
!  maintenance deficit will contribute to stalk senescence.

      DO Stalk = 1, Smax

        IF (LeafNum(DAS-1, Stalk) >= CAB .AND. PGAVAL(Stalk) < 0) THEN
          STKmntDEF(DAS,Stalk) = PGAVAL(Stalk) 
     &                            + (SUWT(DAS-1,Stalk) * GRSU)                  


          IF (STKmntDEF(DAS,Stalk) >= 0) THEN !Sucrose covers deficit

            SuDEF(Stalk) = PGAVAL(Stalk) / GRSU      
                                                
            SuDEF(Stalk) = MIN(SuDEF(Stalk), 0.) ! No deficit
      
            STKmntDEF(DAS,Stalk) = 0. 
     
          ELSE   ! Sucrose insufficient to cover deficit
         
!           deficit absorbed all sucrose
            SuDEF(Stalk) = - SUWT(DAS-1,Stalk) 
          ENDIF ! STKmntDEF(DAS,Stalk) >= 0
      
          PGAVAL(Stalk) = 0.0
          CAV(DAS,Stalk) = PGAVAL(Stalk)

        ELSE ! PGAVAL is sufficient to cover maintenance respiration
!                and plant growth can proceed.  CH2O available for each
!                stalk is saved as CAV(DAS,Stalk)
!                Or if [LeafNum(DAS-1, Stalk) < CAB]??  Is this correct (FSR)?

          CAV(DAS,Stalk) = PGAVAL(Stalk)
        END IF ! (LeafNum(DAS-1, Stalk) >= CAB .AND. PGAVAL(Stalk)

!       Ratio of Supply to Demand for Carbon, by stalk
        IF (CDEMND(DAS,Stalk) > 0) THEN ! Eliminates young shoots
          RatioSDC(DAS,Stalk) = MAX(0.,CAV(DAS,Stalk)/CDEMND(DAS,Stalk))

          RatioSDC(DAS,Stalk) = RatioSDC(DAS,Stalk)  ! temp for debuging

        ELSE
          RatioSDC(DAS,Stalk) = 0.0
        END IF ! (CDEMND(DAS,Stalk)) > 0)

      END DO ! Stalk = 1, Smax
!-----------------------------------------------------------------------
!     Calculate growth rates based on non-limiting CH2O 

      DO Stalk = 1, Smax  

        IF (StalkState(Stalk,1) .EQ. 'LIVE') THEN       

          IF (RatioSDC(DAS,Stalk) >= 1) THEN !CH2O is not limiting for 
!                                           this stalk on this day

            LFDW(DAS,Stalk)  = LFDPW(DAS,Stalk)
            LSDW(DAS,Stalk)  = LSDPW(DAS,Stalk)
            STKDW(DAS,Stalk) = STKDPW(DAS,Stalk)
            RTDW(DAS,Stalk)  = RTDPW(DAS,Stalk)
            SUDW(DAS,Stalk)  = SUDPW(DAS,Stalk)
            ! Include ShootCDEM here?  Since no distinction
            ! beween potential & actual, maybe not.

            CAV(DAS,Stalk) = CAV(DAS,Stalk)
     &                      - (LFDW(DAS,Stalk)  * GRLF)
     &                      - (LSDW(DAS,Stalk)  * GRLF)
     &                      - (STKDW(DAS,Stalk) * GRST)
     &                      - (RTDW(DAS,Stalk)  * GRRT)
     &                      - (SUDW(DAS,Stalk)  * GRSU)
     &                      - ShootCSUP(DAS,Stalk)

            ShootCTotSup = ShootCTotSup + ShootCSUP(DAS,Stalk)
!-----------------------------------------------------------------------
! Total amount of today's potential stalk, leaf, root and sucrose growth 
! for all stalks 
! 
!           Within these IF statements, remaining CH2O in each
!           stalk (CAV - CDEMND)is computed and converted into as 
!           much as another day’s demand (for that day) of sucrose 
!           or stalk DM. The GAMMA parameter determines the
!           partition for CH2O to stalk DM or sucrose.  

            IF (SUDW(DAS,Stalk) > 0) THEN  ! if sucrose exists, 
                                           ! then a stalk must exist
              SUDWtmp = ((1-GAMMA) * CAV(DAS,Stalk)) / GRSU
               
 !            Limits daily sucrose accumulation from excess CH2O
              SUDWtmp = MIN(SUDWtmp,SUDPW(DAS,Stalk)) 
 
              SUDW(DAS,Stalk)  = SUDW(DAS,Stalk)  + SUDWtmp 
 
              STKDWtmp =  (GAMMA * CAV(DAS,Stalk)) / GRST
               
 !            Limits daily stalk DM accumulation from excess CH2O
              STKDWtmp = MIN(STKDWtmp,STKDPW(DAS,Stalk)) 
               
              STKDW(DAS,Stalk) = STKDW(DAS,Stalk) + STKDWtmp
 
            ELSE  
 
              IF (STKDW(DAS,Stalk) > 0) THEN ! if stalk but no sucrose 
 
                STKDWtmp =  (GAMMA * CAV(DAS,Stalk)) / GRST
               
 !              Limits daily stalk DM accumulation from excess CH2O
                STKDWtmp = MIN(STKDWtmp,STKDPW(DAS,Stalk)) 
               
                STKDW(DAS,Stalk) = STKDW(DAS,Stalk) + STKDWtmp
               
              END IF          
            
              CAV(DAS,Stalk) = CAV(DAS,Stalk)
     &                     -((SUDWtmp*GRSU) + (STKDWtmp*GRST))

            END IF
            temp(Stalk) = CAV(DAS,Stalk)   ! debugging 

          SUDWtmp  = 0.0
          STKDWtmp = 0.0
!-----------------------------------------------------------------------
!  Calculate CH2O-limited growth rates
  
        ELSE ! RatioSDC(DAS,Stalk) < 1   CH2O is limiting on this day


        IF (StalkState(Stalk,2) .EQ. 'PRIM') THEN
            !Begin with primary stalk(s) 
        
          IF (LeafNum(DAS-1,Stalk) < CAB) THEN  ! primary SHOOTS only
                GroDEF(Stalk) = CDEMND(DAS,Stalk) - CAV(DAS,Stalk)
                GroDEF(Stalk) = MIN(GroDEF(Stalk), SEEDCH2O)
                CAV(DAS,Stalk) = CAV(DAS,Stalk) + GroDEF(Stalk)
                SEEDCH2O = SEEDCH2O - GroDEF(Stalk)
                SEEDCH2O = MAX(0., SEEDCH2O)  
          END IF ! (LeafNum(DAS-1,Stalk) < CAB)
!----------------------------------------------------------------------- 
!     Prioritize proportion PLF1 (early stalk development) 
!     or PLF2 (mature stalk) of limited CH2O to leaf growth

       IF(SumTTStalk(DAS-1, Stalk) .LT. DTPI) THEN   
          CAVLF = PLF1 * CAV(DAS,Stalk)    ! Primary only here
          CAVR  = (1 - PLF1) * CAV(DAS,Stalk)
       ELSE 
          CAVLF = PLF2 * CAV(DAS,Stalk)
          CAVR  = (1 - PLF2) * CAV(DAS,Stalk)
       END IF ! (LeafNum(DAS-1,Stalk) < CAB)
!-----------------------------------------------------------------------

!     CH2O-limited LEAF growth rate
          LFDW(DAS,Stalk) = LFDPW(DAS,Stalk)
          LSDW(DAS,Stalk) = LSDPW(DAS,Stalk)

!     Leaf PG reduced by leaf increment:
          CAVLF = CAVLF 
     &	      - ((LFDW(DAS,Stalk)*GRLF)+(LSDW(DAS,Stalk)*GRLF))  
                                                                 
!     Is there any leaf PG left for other component growth?
          IF (CAVLF <= 0) THEN 
!     Reduce from potential growth to limited growth

          WholeLeaf = LFDW(DAS,Stalk) + LSDW(DAS,Stalk)

          WholeLeaf = WholeLeaf + (CAVLF / GRLF)  

          WholeLeaf = MAX(0.0, WholeLeaf) 

          LFDW(DAS,Stalk) = (1/(1+LSFAC)) * WholeLeaf

          LSDW(DAS,Stalk) = WholeLeaf - LFDW(DAS,Stalk)            

            CAVLF = MAX(0., CAVLF)

          END IF ! CAVLF(DAS,Stalk) <= 0) Leaf

          CAVR  = CAVR + CAVLF

! Partition proportion of limited CH2O to TOPS growth
! Future section for partially developed leaves, stem (cabbage) 
!         TPDW(DAS,Stalk)  = (CAVR / GRTP) *  TPDPW(DAS,Stalk)
!    &    / (TPDPW(DAS,Stalk)+STKDPW(DAS,Stalk)+RTDPW(DAS,Stalk)+SUDPW(DAS,Stalk))
!--------------------------------------------------------------
! Partition proportion of limited CH2O to STALK growth 
          IF (STKDPW(DAS,Stalk) > 0) THEN

          STKDW(DAS,Stalk) = (CAVR / GRST) *  STKDPW(DAS,Stalk)
     &        / (STKDPW(DAS,Stalk)+ RTDPW(DAS,Stalk)+ SUDPW(DAS,Stalk)
     &           + ShootCSUP(DAS,Stalk)/GRST)

              ELSE
                      STKDW(DAS,Stalk) = 0    
          END IF 
!--------------------------------------------------------------       
! Partition proportion of limited CH2O to ROOT growth 
          IF (RTDPW(DAS,Stalk) > 0) THEN

          RTDW(DAS,Stalk)  = (CAVR / GRRT) *  RTDPW(DAS,Stalk)
     &        / (STKDPW(DAS,Stalk)+ RTDPW(DAS,Stalk)+ SUDPW(DAS,Stalk)
     &           + ShootCSUP(DAS,Stalk)/GRRT)

              ELSE
                      RTDW(DAS,Stalk) = 0     
          END IF 
!--------------------------------------------------------------
! Partition proportion of limited CH2O to SUCROSE  
          IF (SUDPW(DAS,Stalk) > 0) THEN

          SUDW(DAS,Stalk)  = (CAVR / GRSU) *  SUDPW(DAS,Stalk)
     &        / (STKDPW(DAS,Stalk)+ RTDPW(DAS,Stalk)+ SUDPW(DAS,Stalk)
     &           + ShootCSUP(DAS,Stalk)/GRSU)

              ELSE
                      SUDW(DAS,Stalk) = 0     
          END IF 
!--------------------------------------------------------------
! Partition proportion of limited CH2O to New Shoots   
          IF (ShootCSUP(DAS,Stalk) > 0) THEN

          ShootCSUP(DAS,Stalk)  = CAVR *  ShootCSUP(DAS,Stalk)
     &        / ((STKDPW(DAS,Stalk) * GRST)
     &          +(RTDPW(DAS,Stalk)  * GRRT)
     &          +(SUDPW(DAS,Stalk)  * GRSU)
     &          + ShootCSUP(DAS,Stalk))

              ELSE
                      ShootCSUP(DAS,Stalk) = 0    
          END IF 
!--------------------------------------------------------------
! Account for use of CAV      
          CAV(DAS,Stalk) = CAV(DAS,Stalk) 
     &        -(LFDW(DAS,Stalk) * GRLF)   
     &        -(LSDW(DAS,Stalk) * GRLF)       
     &        -(STKDW(DAS,Stalk)* GRST)     
     &        -(RTDW(DAS,Stalk) * GRRT)     
     &        -(SUDW(DAS,Stalk) * GRSU)
     &        - ShootCSUP(DAS,Stalk)
!    &        -(TPDW(DAS,Stalk) * GRTP)     Future use of TOPS component

          ShootCTotSup = ShootCSUP(DAS,Stalk)

              CAVLF = 0.0
              CAVR  = 0.0

!Note: this should go to ELSE ! not primary stalk
!---------------------------------------------------------------------
!      Use CH2O stored in seed cane to supplement shoot growth of 
!      the primary stalk, and CH2O from photosynthesizing stalks to
!      supply growth of the following shoots. 
           
           ELSE ! for non-primary shoots
             IF (LeafNum(DAS-1,Stalk) < CAB) THEN 
                GroDEF(Stalk) = ShootCDEM(DAS,Stalk) - CAV(DAS,Stalk)
                GroDEF(Stalk) = MIN(GroDEF(Stalk), ShootCTotSup)
                CAV(DAS,Stalk) = CAV(DAS,Stalk) + GroDEF(Stalk)
                ShootCTotSup = ShootCTotSup - GroDEF(Stalk)
                ShootCTotSup = MAX(0., ShootCTotSup)  

             END IF ! (LeafNum(DAS-1,Stalk) < CAB)
!-----------------------------------------------------------------------
!     Prioritize proportion PLF1 (early stalk development) 
!     or PLF2 (mature stalk) of limited CH2O to leaf growth

       IF(SumTTStalk(DAS-1, Stalk) .LT. DTPI) THEN   
          CAVLF = PLF1 * CAV(DAS,Stalk)
          CAVR  = (1 - PLF1) * CAV(DAS,Stalk)
       ELSE 
          CAVLF = PLF2 * CAV(DAS,Stalk)
          CAVR  = (1 - PLF2) * CAV(DAS,Stalk)
       END IF ! (LeafNum(DAS-1,Stalk) < CAB)
!-----------------------------------------------------------------------

!     CH2O-limited LEAF growth increments
          LFDW(DAS,Stalk) = LFDPW(DAS,Stalk)
          LSDW(DAS,Stalk) = LSDPW(DAS,Stalk)

!     Leaf PG reduced by leaf increment:
          CAVLF = CAVLF 
     &	      - ((LFDW(DAS,Stalk)*GRLF)+(LSDW(DAS,Stalk)*GRLF))   

!     Is there any leaf PG left for other component growth?
          IF (CAVLF <= 0) THEN 
!     Reduce from potential growth to limited growth

          WholeLeaf = LFDW(DAS,Stalk) + LSDW(DAS,Stalk)

          WholeLeaf = WholeLeaf + (CAVLF / GRLF)  

          WholeLeaf = MAX(0.0, WholeLeaf) 

          LFDW(DAS,Stalk) = (1/(1+LSFAC)) * WholeLeaf

          LSDW(DAS,Stalk) = WholeLeaf - LFDW(DAS,Stalk)            

!!!              LFDW(DAS,Stalk) = LFDW(DAS,Stalk) + (CAVLF / GRLF) 

              CAVLF = MAX(0., CAVLF)
          END IF ! CAVLF(DAS,Stalk) <= 0) Leaf

          CAVR  = CAVR + CAVLF

! Partition proportion of limited CH2O to TOPS growth
! Future section for partially developed leaves, stem (cabbage) 
!         TPDW(DAS,Stalk)  = (CAVR / GRTP) *  TPDPW(DAS,Stalk)
!    &    / (TPDPW(DAS,Stalk)+STKDPW(DAS,Stalk)+RTDPW(DAS,Stalk)+SUDPW(DAS,Stalk))
          
! Partition proportion of limited CH2O to STALK growth 
          IF (STKDPW(DAS,Stalk) > 0) THEN

          STKDW(DAS,Stalk) = (CAVR / GRST) *  STKDPW(DAS,Stalk)
     &        / (STKDPW(DAS,Stalk)+ RTDPW(DAS,Stalk)+ SUDPW(DAS,Stalk))

              ELSE
                      STKDW(DAS,Stalk) = 0    
          END IF 
!--------------------------------------------------------------
! Partition proportion of limited CH2O to ROOT growth 
          IF (RTDPW(DAS,Stalk) > 0) THEN

          RTDW(DAS,Stalk)  = (CAVR / GRRT) *  RTDPW(DAS,Stalk)
     &        / (STKDPW(DAS,Stalk)+ RTDPW(DAS,Stalk)+ SUDPW(DAS,Stalk))

              ELSE
                      RTDW(DAS,Stalk) = 0     
          END IF 
!--------------------------------------------------------------

! Partition proportion of limited CH2O to SUCROSE    
          IF (SUDPW(DAS,Stalk) > 0) THEN

          SUDW(DAS,Stalk)  = (CAVR / GRSU) *  SUDPW(DAS,Stalk)
     &        / (STKDPW(DAS,Stalk)+ RTDPW(DAS,Stalk)+ SUDPW(DAS,Stalk))

              ELSE
                      SUDW(DAS,Stalk) = 0     
          END IF 
!--------------------------------------------------------------

! Account for use of CAV      
          CAV(DAS,Stalk) = CAV(DAS,Stalk) 
     &        -(LFDW(DAS,Stalk) * GRLF)    
     &        -(LSDW(DAS,Stalk) * GRLF)    
     &        -(STKDW(DAS,Stalk)* GRST)     
     &        -(RTDW(DAS,Stalk) * GRRT)     
     &        -(SUDW(DAS,Stalk) * GRSU)     
!     &       -(TPDW(DAS,Stalk) * GRTP)     Future use of TOPS component

              CAVLF = 0.0
              CAVR  = 0.0
!             
        END IF ! (RatioSDC(DAS,Stalk) >= 1)       
        END IF ! StalkState(Stalk,2) .EQ. 'PRIM'      
       END IF !(StalkState(Stalk,1) .EQ. 'LIVE')

      END DO  ! Stalk = 1, Smax 
!------------------------------------------END--------------------------

!  Calculation of leaf and leaf sheath mass and leaf area reduction rate 
!  from leaf senescence. 
!  Loss from whole-stalk senescence is added in INTEGR section below
!  Stalk-level pest damage must still be incorporated - FSR

      DO Stalk = 1, Smax

        LFDWlost(DAS,Stalk) = LFWT(DAS-1,Stalk) * SLDOT(Stalk) ! Leaf
        LSDWlost(DAS,Stalk) = LSWT(DAS-1,Stalk) * SLDOT(Stalk) ! Sheath
															 

              IF (LFDWlost(DAS,Stalk) > 0) THEN

        DeltaLeafArealost(DAS,Stalk) =  LeafArea(DAS - 1, Stalk) 
     &                *  LFDWlost(DAS,Stalk) /  LFWT(DAS-1,Stalk)  
!        This area is based on average or static SLA; it may need to be 
!        modified to reflect maximum SLA (min thickness) of shaded leaves.
              ELSE
                      DeltaLeafArealost(DAS,Stalk) = 0 
                  
              END IF 

      END DO
!                
C-----------------------------------------------------------------------
!
!    CH2O-limited LEAF EXPANSION (senescence reduction not included here)  
!       
!!    IF (PhenoStage > 2) THEN  !! IF statement not needed  FSR

          DO Stalk = 1, Smax
              IF (LFDPW(DAS,Stalk) > 0) THEN

                  DeltaLeafArea(DAS,Stalk) = DeltaLeafArea(DAS,Stalk)
     &                    * (LFDW(DAS,Stalk) / LFDPW(DAS,Stalk))

              ELSE
                      DeltaLeafArea(DAS,Stalk) = 0 
                  
              END IF 
!       Limit leaf area based on actual-to-potential leaf weight ratio 
!       If PhenoStage <= 2, no leaves yet (pre-emergence of primary shoot)
!       Note: Use DAS, not DAS-1, because DeltaLeafArea is a daily rate,
!             not an accumulation.  

          END DO
!!    END IF 
C-----------------------------------------------------------------------
!
! PGLEFT - Excess PG after today's tissue growth (g [CH2O] / m2)
!      = MAX(0.0,PGAVL - ((WLDOTN + WSDOTN + WRDOTN + WSUDOTN) * AGRVG))   
!
!     The above calculation was simplified to the following since the
!     W*DOTN [growth rate of N but not C] variables were lost with the 
!     removal of the VEGGR module. FSR
!
      DO Stalk = 1, Smax
      
           IF (StalkState(Stalk,1) .EQ. 'LIVE') THEN 

             PGLEFT(Stalk) = MAX(CAV(DAS,Stalk), 0.0)

C-----------------------------------------------------------------------
! 
! EXCESS(j) Factor based on excess PG used to affect tomorrow's PG 
!            calculation for stalk j 
C
C     Scales to 1.0 if PGLEFT is small fraction, and to 0.2 if large
C     fraction.  Used 0.04, so minor PGLEFT has no effect.  Used square
C     root.  Creates almost no effect if PGLEFT/PG is small, but goes to
C     0.2 as PGLEFT/PG  approaches 1.0.  0.04 could be parameterized as
C     kickoff point.  Upper cutoff is the value 1.04.  Limit of 1.04 -
C     1.00 forces relationship to stop at 0.04, gives 0.2 of normal PG.
C     value 1.04 -0.04 also can not be greater than 1.0 or we get
C     stimulation of photosynthesis and the sq root works differently.

             IF (PGAVAL(Stalk) > 0.0001 .AND. PGLEFT(Stalk) > 0.00001) 
     &       THEN      
               EXCESS(Stalk) = (1.20 
     &                       - MIN(1.0, MAX(PGLEFT(Stalk)/PGAVAL(Stalk), 
     &                         0.20)) ) ** 0.5
             temp(Stalk) = EXCESS(Stalk)	! temporary debug
             ELSE
               EXCESS(Stalk) = 1.00  
             ENDIF
          
          ELSE
             PGLEFT(Stalk) = 0.0
             EXCESS(Stalk) = 0.0

          END IF  ! (Stalk <= NewStalk)

      END DO  ! Stalk = 1, Smax
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!     Write growth details by stalk to output file CSP_GROW_CANE.OUT 
!     prior to emergence (NVEG0)  FSR

      CALL YR_DOY(YRDOY, YEAR, DOY) 

      IF (DAS < NVEG0) THEN

      IF (INDEX('Y'  ,ISWITCH%IDETG) > 0 .AND. 
     &    INDEX('YDA',ISWITCH%IDETL) > 0) THEN


      WRITE(CGLUN,'(/1X,I4,1X,I3,1X,I3,1X,I3,4X,I2,1X,F5.2,3X,F5.2,2X,
     &      F6.2,2X,F5.0,1X,F6.0,1X,
     &      F7.0,1X,F6.0,1X,F7.0,1X,F5.1,3X,F5.2,2X,I3,1X,F5.2,
     &      1X,F6.0,1X,F6.0)',
     &      ADVANCE="NO"), 
     &      YEAR, DOY, DAS, DAP, 
     &      PhenoStage, XLAI, XLAIlost, PgAvalPD, SLA, 
     &      LFWTHa(DAS), STKWTHa(DAS), 
     &      RTWTHa(DAS), SUWTHa(DAS), StalkPopul(DAS),  
     &      LeafAreaPlant(DAS)/10000, CountStalk, STKWTP(DAS)/1000,
     &      LFWTHalost(DAS), STKWTHalost(DAS)

        DO Stalk = 1, Smax
          IF (Kill(Stalk) > -1) THEN 
          WRITE(CGLUN,'(1X,F5.0)', ADVANCE="NO") STKWT(DAS,Stalk)
          ELSE
            WRITE(CGLUN,'(6X)', ADVANCE="NO") 
          END IF
        END DO
!
        DO Stalk = 1, Smax
          IF (Kill(Stalk) > -1) THEN 
          WRITE(CGLUN,'(1X,F5.0)', ADVANCE="NO") SUWT(DAS,Stalk)
          ELSE
            WRITE(CGLUN,'(6X)', ADVANCE="NO") 
          END IF
        END DO
!
        DO Stalk = 1, Smax
          IF (Kill(Stalk) > -1) THEN 
          WRITE(CGLUN,'(1X,F5.1)', ADVANCE="NO") LFWT(DAS,Stalk)
          ELSE
            WRITE(CGLUN,'(6X)', ADVANCE="NO") 
          END IF
        END DO
!
        DO Stalk = 1, Smax
          IF (Kill(Stalk) > -1) THEN 
          WRITE(CGLUN,'(2X,F4.1)', ADVANCE="NO") LeafNum(DAS,Stalk)
          ELSE
            WRITE(CGLUN,'(6X)', ADVANCE="NO") 
          END IF
        END DO
!
        DO Stalk = 1, Smax
          IF (Kill(Stalk) > -1) THEN 
        WRITE(CGLUN,'(1X,F6.0)', ADVANCE="NO") LeafArea(DAS,Stalk)
          ELSE
          WRITE(CGLUN,'(7X)', ADVANCE="NO") 
          END IF
        END DO

      ENDIF
      ENDIF
!-----------------------------------------------------------------------
!***********************************************************************
!***********************************************************************
!     DAILY INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
C  Integrate today's Net Growth into the previous day's accumulated growth 
!  Note: The CASUPRO variable names do not necessarily coencide 
!        with the corresponding variable names in other models.
!
! All "lost" tissue components are accounted for.
!
      DO Stalk = 1, Smax
C-----------------------------------------------------------------------
!       Sum each stalk's contribution to daily root growth for supplying to
!       CSP_ROOTS module.  Placed before IF LIVE, since roots do not die
!       with stalks, and this collects root growth from all stalks  
!       including those that died today. 
!
	   RTDWP(DAS) = RTDWP(DAS) + RTDW(DAS,Stalk)
!        
!       RTDWP(DAS) - Root growth increment on day i for entire plant 
!                     (g[root tissue] / day)
C-----------------------------------------------------------------------

          IF (StalkState(Stalk,1) .EQ. 'LIVE') THEN 

            CountStalk = CountStalk + 1 ! tally live stalks

!-----------------------------------------------------------------------
!     Keeping track of senesced leaves and leaf sheaths
          LFWTlost(DAS,Stalk) = LFWTlost(DAS-1,Stalk)  
     &                             + LFDWlost(DAS,Stalk)

          LSWTlost(DAS,Stalk) = LSWTlost(DAS-1,Stalk)  
     &                             + LSDWlost(DAS,Stalk)
!-----------------------------------------------------------------------
          LFWT(DAS,Stalk)  = LFWT(DAS-1,Stalk)  + LFDW(DAS,Stalk)
     &                       - LFDWlost(DAS,Stalk) 
          LSWT(DAS,Stalk)  = LSWT(DAS-1,Stalk)  + LSDW(DAS,Stalk)
     &                       - LSDWlost(DAS,Stalk) 
          STKWT(DAS,Stalk) = STKWT(DAS-1,Stalk) + STKDW(DAS,Stalk)
          RTWT(DAS,Stalk)  = RTWT(DAS-1,Stalk) + RTDW(DAS,Stalk) 
          SUWT(DAS,Stalk)  = SUWT(DAS-1,Stalk)  + SUDW(DAS,Stalk)
     &                     + SuDEF(Stalk) ! SuDEF always <= 0

!  Sum all stalks components into total weight of stalk (g / stalk)
          TOTALWT(Stalk)  = LFWT(DAS,Stalk)  + LSWT(DAS,Stalk) 
     &	                + STKWT(DAS,Stalk) + RTWT(DAS,Stalk) 
     &                    + SUWT(DAS,Stalk)

!     Integrates leaf area in each day and each stalk
          LeafArea(DAS,Stalk) = 
     &        LeafArea(DAS - 1, Stalk) + DeltaLeafArea(DAS,Stalk)
     &                                 - DeltaLeafArealost(DAS,Stalk)

          LeafArealost(DAS,Stalk) = 
     &       LeafArealost(DAS - 1, Stalk) + DeltaLeafArealost(DAS,Stalk)

          ELSE IF (StalkState(Stalk,1) .EQ. 'DEAD') THEN
!     Transfers senesced stalk leaf mass and area to lost categories

          LFWTlost(DAS,Stalk) = LFWT(DAS-1,Stalk)
     &                        + LFWTlost(DAS-1,Stalk)  ! OK

          LSWTlost(DAS,Stalk) = LSWT(DAS-1,Stalk)
     &                        + LSWTlost(DAS-1,Stalk)  ! Leaf 
          LSWT(DAS,Stalk) = 0.                         ! Sheaths

          STKWTlost(DAS,Stalk) = STKWT(DAS-1,Stalk)
     &                        + STKWTlost(DAS-1,Stalk)
          STKWT(DAS,Stalk) = 0

          LeafArealost(DAS,Stalk) = LeafArealost(DAS-1, Stalk) 
     &                            + LeafArea(DAS-1,Stalk)  
          LeafArea(DAS,Stalk) = 0.

          END IF  ! (Stalk <= NewStalk)    
      END DO
!
C-----------------------------------------------------------------------
!     Estimate fresh cane weight by stalk
!       Using CaneGro (SC_CNGRO.for module) method for calculating 
!       stalk wet mass:  "CNB&AS May2001 START taking a shot at cane 
!       moisture content after Martines SASTA 2001"
!     
!     Eventually, this should be handled by a fresh cane weight function
!       that incorporates stalk internode age, as well as sucrose  
!       content and climate.         See Liu & Helyar 2003
!
!	Substituted variables StkH2OFac and SuH2OFac into Martines equation
!       to enable adjustment by variety. FSR 08/03/2009.  
!       Original values were 3.607 and 2.078 respectively.
!
      DO Stalk = 1, Smax

        Stk_H2O = (StkH2OFac * STKWT(DAS,Stalk)) 
     &          - ( SuH2OFac *  SUWT(DAS,Stalk))
        STKDMC = 0.3
        
        IF (STKWT(DAS,Stalk).GT.0.0) THEN
            STKDMC = STKWT(DAS,Stalk) / (Stk_H2O + STKWT(DAS,Stalk))
        ENDIF
        
        STKFWT(DAS,Stalk) = (STKWT(DAS,Stalk) /STKDMC) + SUWT(DAS,Stalk)

      END DO  ! Fresh stalk weight calculation

C-----------------------------------------------------------------------
!  Aggregate from Stalk [g / stalk / plant] to Plant [g / plant] units 
!  note: root growth is consolidated from stalks into entire plant, so  
!        roots are skipped in this section.
 
      DO Stalk = 1, Smax

          LFWTP(DAS)  = LFWTP(DAS)  + LFWT(DAS,Stalk)      
          LSWTP(DAS)  = LSWTP(DAS)  + LSWT(DAS,Stalk)      
          STKWTP(DAS) = STKWTP(DAS) + STKWT(DAS,Stalk)
          SUWTP(DAS)  = SUWTP(DAS)  + SUWT(DAS,Stalk)
          STKFWTP(DAS)= STKFWTP(DAS)+ STKFWT(DAS,Stalk) 
!                                         Fresh stalk weight  

!     Senesced leaf & stalk mass from whole plant (cumulative)
          LFWTPlost(DAS)  = LFWTPlost(DAS)  + LFWTlost(DAS,Stalk)
          LSWTPlost(DAS)  = LSWTPlost(DAS)  + LSWTlost(DAS,Stalk)
          STKWTPlost(DAS) = STKWTPlost(DAS) + STKWTlost(DAS,Stalk)

       END DO

C-----------------------------------------------------------------------
!  Calculation cumulative root weight for plant, minus senesced roots
 	RTWTP(DAS)  = RTWTP(DAS-1) + RTDWP(DAS) - (SRDOT * (1/PLTPOP)) 
C-----------------------------------------------------------------------
!  Calculate weight of above-ground portion of crop and 
!  total weight of crop (g[tissue] / m2).
!
      TOPWT  = (LFWTP(DAS) + LSWTP(DAS) + STKWTP(DAS) + SUWTP(DAS)) 
     &         * PLTPOP
      TOTWT  = (LFWTP(DAS) + LSWTP(DAS) + STKWTP(DAS) + SUWTP(DAS)
     &         + RTWTP(DAS)) * PLTPOP 
!-----------------------------------------------------------------------
!  Convert [stalks / plant] --> [stalks / m2]

      StalkPopul(DAS) = CountStalk * PLTPOP
C-----------------------------------------------------------------------
!  Convert [g / plant] --> [kg / ha] 
!
          LFWTHa(DAS)  = LFWTP(DAS)  * PLTPOP * 10      
          LSWTHa(DAS)  = LSWTP(DAS)  * PLTPOP * 10      
          STKWTHa(DAS) = STKWTP(DAS) * PLTPOP * 10  
          SUWTHa(DAS)  = SUWTP(DAS)  * PLTPOP * 10
          RTWTHa(DAS)  = RTWTP(DAS)  * PLTPOP * 10   !!!!!A+B
          STKFWTHa(DAS)= STKFWTP(DAS)*PLTPOP * 10 ! Fresh stalk weight
!   
!     Senesced leaf & stalk mass per hectare (cumulative)
          LFWTHalost(DAS)  = LFWTPlost(DAS)  * PLTPOP * 10
          LSWTHalost(DAS)  = LSWTPlost(DAS)  * PLTPOP * 10
          STKWTHalost(DAS) = STKWTPlost(DAS) * PLTPOP * 10
          SenesHa(DAS)  = LFWTHalost(DAS) + STKWTHalost(DAS) 

!     Daily per ha losses calculated for SENESCE % ResWt(0) values
          LFDWHalost(DAS) =  LFWTHalost(DAS) -  LFWTHalost(DAS-1)
          LSDWHalost(DAS) =  LSWTHalost(DAS) -  LSWTHalost(DAS-1)
          STKDWHalost(DAS) = STKWTHalost(DAS) - STKWTHalost(DAS-1)

C-----------------------------------------------------------------------
!    Senesced material into layers of SENESCE % ResWt
!!!	Surface material

      SenWt(0) = 
     &       AMAX1(LFDWHalost(DAS)+LSDWHalost(DAS)+STKDWHalost(DAS), 0.)

!     Senesced roots (kg/ha)

      DO L = 1, NLAYR  
        SenWt(L)  = SENRT(L)       !kg[dry matter]/ha
      ENDDO
      SENESCE % ResWt  = SenWt
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
! Leaf area by plant (stubble) on each day for all stalks
! note - Stalk daily amounts are cumulative, so plant total includes DAS-1

        DO Stalk = 1, Smax
   
           LeafAreaPlant(DAS) = LeafAreaPlant(DAS)  
     &                        + LeafArea(DAS,Stalk)   

!     Keeping track of senesced leaf area
           LeafAreaPlantlost(DAS) = LeafAreaPlantlost(DAS) 
     &                            + LeafArealost(DAS,Stalk)
        END DO

      XLAI = PLTPOP * LeafAreaPlant(DAS) / 10000 
      XHLAI = XLAI
C-----------------------------------------------------------------------
C     Remember XLAI
C-----------------------------------------------------------------------
      LAIMX_Previous = LAIMX

      LAIMX = MAX(XLAI,LAIMX)
      
      IF (LAIMX > LAIMX_Previous) THEN 

        LAIXD = DAP

      End IF
C-----------------------------------------------------------------------

!     Keeping track of senesced leaf area
      XLAIlost = PLTPOP * LeafAreaPlantlost(DAS) / 10000 
! ----------------------------------------------------------------------
! Calculate rate of biomass accumulation, BRDMD

      DO Stalk = 1, Smax

          SumLfWt    = SumLfWt + LFDW(DAS,Stalk)
          SumLSWt    = SumLSWt + LSDW(DAS,Stalk)      
          SumStkTiWt = SumStkTiWt + STKDW(DAS,Stalk)
          SumRtWt    = SumRtWt + RTDW(DAS,Stalk)          
          SumStkSuWt = SumStkSuWt + SUDW(DAS,Stalk)   

       END DO

!	Convert from g/plant to kg/ha to t/ha
      BRDMD = (SumLfWt + SumLSWt + SumStkTiWt + SumRtWt + SumStkSuWt)
     &       * PLTPOP * 10 / 1000
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!     Write growth details by stalk to output file CSP_GROW_CANE.OUT 
!     after emergence (NVEG0)  FSR
      IF (INDEX('Y'  ,ISWITCH%IDETG) > 0 .AND. !Growth output
     &    INDEX('YDA',ISWITCH%IDETL) > 0 .AND. !Detailed output
     &    (DAS .GE. NVEG0+1) .AND.             !after emergence
     &   (MOD(DAS,CONTROL%FROP) == 0      !Daily output every FROP days,
     &  .OR. YRDOY == YRPLT)) THEN        !and on planting date

        WRITE(CGLUN,'(/1X,I4,1X,I3,1X,I3,1X,I3,4X,I2,1X,F5.2,3X,F5.2,2X,
     &      F6.2,2X,F5.0,1X,F6.0,1X,
     &      F7.0,1X,F6.0,1X,F7.0,1X,F5.1,3X,F5.2,2X,I3,1X,F5.2,
     &      1X,F6.0,1X,F6.0)',
     &      ADVANCE="NO"), 
     &      YEAR, DOY, DAS, DAP, 
     &      PhenoStage, XLAI, XLAIlost, PgAvalPD, SLA, 
     &      LFWTHa(DAS), STKWTHa(DAS), 
     &      RTWTHa(DAS), SUWTHa(DAS), StalkPopul(DAS),  
     &      LeafAreaPlant(DAS)/10000, CountStalk, STKWTP(DAS)/1000,
     &      LFWTHalost(DAS), STKWTHalost(DAS)

        DO Stalk = 1, Smax
          IF (Kill(Stalk) > -1) THEN 
          WRITE(CGLUN,'(1X,F5.0)', ADVANCE="NO") STKWT(DAS,Stalk)
          ELSE
            WRITE(CGLUN,'(6X)', ADVANCE="NO") 
          END IF
        END DO

        DO Stalk = 1, Smax
          IF (Kill(Stalk) > -1) THEN 
          WRITE(CGLUN,'(1X,F5.0)', ADVANCE="NO") SUWT(DAS,Stalk)
          ELSE
            WRITE(CGLUN,'(6X)', ADVANCE="NO") 
          END IF
        END DO

        DO Stalk = 1, Smax
          IF (Kill(Stalk) > -1) THEN 
          WRITE(CGLUN,'(1X,F5.1)', ADVANCE="NO") LFWT(DAS,Stalk)
          ELSE
            WRITE(CGLUN,'(6X)', ADVANCE="NO") 
          END IF
        END DO

        DO Stalk = 1, Smax
          IF (Kill(Stalk) > -1) THEN 
          WRITE(CGLUN,'(2X,F4.1)', ADVANCE="NO") LeafNum(DAS,Stalk)
          ELSE
            WRITE(CGLUN,'(6X)', ADVANCE="NO") 
          END IF
        END DO

        DO Stalk = 1, Smax
          IF (Kill(Stalk) > -1) THEN 
        WRITE(CGLUN,'(1X,F6.0)', ADVANCE="NO") LeafArea(DAS,Stalk)
          ELSE
          WRITE(CGLUN,'(7X)', ADVANCE="NO") 
          END IF
        END DO

      ENDIF
!-----------------------------------------------------------------------
 
!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT 
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
C-----------------------------------------------------------------------
!!!      SENESCE % ResWt(0) = LFDWHalost(DAS) + STKDWHalost(DAS)
!!!      SENESCE % ResWt(0) = AMAX1(LFDWHalost(DAS)+STKDWHalost(DAS), 0.) 
! Assigning senesced above-ground biomass to ResWt should involve
! a delay and probably a proportion, possibly related to the delay.
! For now however, I will commit all senesced material to residue.

 
C-----------------------------------------------------------------------
C     Senescence of roots (kg/ha)
!!!      DO L = 1, NLAYR  
!!!        SenWt(L)  = SENRT(L)                        !kg[dry matter]/ha
!        SenLig(L) = SENRT(L) * PLIGRT                     !kg[lig]/ha
!        SenE(L,1) = (SENRT(L)* PRORTF) * 0.16 
!!!      ENDDO
!!!   SENESCE % ResWt  = SenWt
C-----------------------------------------------------------------------
C     Total Senescence, surface and soil (kg/ha)

      DO L = 0, NLAYR
!        SENESCE % ResE(L,N)  = SenE(L,N)
        SENESCE % CumResWt   = SENESCE % CumResWt + SenWt(L)
!        SENESCE % CumResE(N) = SENESCE % CumResE(N) + SenE(L,N)
      ENDDO
C-----------------------------------------------------------------------
!
!***********************************************************************
!***********************************************************************
!     SEASONAL OUTPUT 
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
!     CropTypeCode = CropTypeCode + 1

!-----------------------------------------------------------------------
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END ! SUBROUTINE CSP_GROW_CANE

!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!  CSP_GROW_CANE variable names:
!-----------------------------------------------------------------------
! BRDMD Rate of biomass accumulation (kg/ha/d)
! CAB  Number of leaves produced before stalk development begins.  Parameter.
! CAV(i,j) CH2O available on day i for stalk j for growth & growth respiration 
!          (g[CH2O] / m2 / day)
! CAVLF - Max C available to leaves under CH2O-limited growth.
! CAVR - C available after leaves to other plant components 
!        under CH2O-limited growth
! CDEF Daily difference between CH2O demanded and available for plant 
!      growth. Compensated from CH2O stored in seed cane. g[CH2O]/day  
! CDEM   Total CH2O demand for potential growth, including growth  
!        respiration costs  (g[CH2O] / m2 / day)
! DeltaLeafArea(i,j) increment of leaf area in period i and stalk j (cm2 / stalk)
! DeltaLeafArealost(i,j)  Leaf area lost on day i, stalk j  (cm2 / stalk) 
! DeltaLeafNum(DAS,Stalk) - increase in # of leaves / stalk / day
! DYNAMIC Module control variable; = RUNINIT, SEASINIT, RATE, EMERG,
!                                    INTEGR, OUTPUT, or FINAL
! EXCESS(j) Factor based on excess PG used to affect tomorrow's PG 
!            calculation for stalk j 
! F     Specific leaf area of new leaf tissue growth (cm2[leaf] / g[leaf])
! FVEG  Specific leaf area prior to computing effects of temperature, 
!       PAR, water stress (cm2[leaf] / g[leaf])
! FFVEG Specific leaf area of new leaf tissue growth (interim value)
! FRRT  Fraction of potential leaf and stalk growth that is used to  
!       determine potential root growth.
! FRSU  Fraction of potential leaf and stalk growth that is depositioned 
!       as sucrose
! GAMMA  Parameter governing partitioning of excess carbon between 
!        stalks [GAMMA * excess] and sucrose [(1-GAMMA) * excess]
! GroDEF(j) CH2O deficit for early stalk growth, compensated by SeedCH2O
!        (for primary stalk) or other stalks    (g[CH2O] / day) 
! GRLF   Growth respiration costs, g[CH2O] / g[leaf tissue]
! GRRT   Growth respiration costs, g[CH2O] / g[root tissue]
! GRST   Growth respiration costs, g[CH2O] / g[stalk tissue]
! GRSU   Growth respiration costs, g[CH2O] / g[sucrose]
! LeafArea(i,j)     Leaf area on day i and for stalk j       cm2 / stalk
! LeafAreaPlant(i)  Leaf area of the plant on day i           m2 / plant
! LFDWlost(i,j):  Amount senescence reduces leaf weight; day i and stalk j.
!                 g[leaf] / stalk / day
! LFDPW(i,j) Potential increase in leaf weight; day i and stalk j 
!            (g[leaftissue]/stalk-d)  
! LFDW(i,j)  CH2O-limited increase in leaf weight; day i and stalk j 
!               (g[leaf tissue]/stalk-d)  
! LFDWHalost(i) Daily leaf dry mass senescence, day i kg[leaf] / Ha / day   
! LFWT(i,j)  Weight, on day i, of leaves on stalk j:  g[leaf] / stalk / day
! LFWTHa(i)  Leaf weight per ground area on day i:     kg[leaf tissue] / ha
! LFWTHalost(i)  Cumulative senesced leaf dry weight thru day i: 
!                kg[leaf tissue] / ha
! LFWTP(i)   Leaf weight per plant   g[leaf tissue] / plant 
! STKmntDEF(i,j) CH2O deficit for maintenance respiration requirement, for
!             day i and stalk j. Used also for calculating senescence.
!             (g[CH2O] / stalk / day) 
! NumOfStalks   maximum size of the array for stalks
! PAR       Daily photosynthetically active radiation or photon flux 
!             density (moles [quanta]/m2-d)
! PARSLA    Effect of PAR on specific leaf area 
! PG         Daily gross photosynthesis (g[CH2O] / m2 / d)
! PGAVAL(j) Total daily CH2O available to stalk j for growth & respiration 
!           after expending PG on maintenance respiration 
!           (g[CH2O] / stalk j / d)
! PgAvalPD  Total daily CH2O available to the plant for growth & respiration after
!             expending PG on maintenance respiration (g[CH2O] / m2 / d)
!             (directly replaces PGAVL)
! PGAVL     Total daily CH2O available for growth & respiration
!             (g[CH2O] / m2 / d)
! PGLEFT(j)  Excess PG for stalk j after today's tissue growth (g [CH2O] / stalk)
! PLF1       Proportion of CH2O available to leaves under CH2O-limited conditions for young stalk
! PLF2       Proportion of CH2O available to leaves under CH2O-limited conditions for mature stalk 
! PLTPOP     Plant population (# plants / m2)
! PLWT       Fresh weight of seed cane planted in field [(kg/ ha)]
! RatioSDC(i,j) Ratio of available CH2O to demand for CH2O based on potential growth  
!               on day i of stalk j
! RTDPW(i,j) Potential root growth increment on day i from stalk j: g[root tissue] / stalk / day 
! RTDW(i,j)  Root growth increment on day i from stalk j: g[root tissue] / stalk / day 
! RTDWP(i)   Root growth increment on day i for entire plant (all stalks) g[root tissue] / day 
! RTWT(i,j)  Root cumulative weight per stalk on day i:  g[root tissue] / stalk / day 
! RTWTHa(i)  Root cumulative weight per ground area on day i:     kg[root tissue] / ha
! RTWTP(i)   Root cumulative weight per plant   g[root tissue] / plant 
! SeedCH2O   CH2O from seed cane available for early growth of primary stalk
!            g[CH2O] / seed
! ShootCDEM(i,j) Demand for CH2O by new shoots supplied by existing stalks (not 
!                applicable to primary shoot)   g [CH2O] / day
! ShootCSUP(i,j) Supplies ShootCDEM CH2O to shoots having < CAB leaves g [CH2O]/day
! SLA       Specific leaf area (cm2[leaf] / g[leaf]) (used instead of F for sharing among modules)
! SLAMAX    The maximum specific leaf area (SLA) for new leaves when grown 
!             under low (nearly zero) radiation but optimum water and 
!             temperature for the standard cultivar. (cm2 / g)
! SLAMIN    The minimum specific leaf area (SLA) for new leaves when grown 
!             under infinitely high radiation, optimum water and 
!             temperature for the standard cultivar. (cm2 / g)
! SLAMN     Minimum specific leaf area for new leaves when grown under high 
!             radiation and optimum water and temperature conditions (cm2 / g)
! SLAMX     Maximum specific leaf area for new leaves when grown under low 
!             radiation, but optimum water and temperature conditions
!             (cm2 / g)
! SLAPAR    Coefficient in exponential equation to reduce SLA as PAR 
!             increases (leaf curvature) 
! SLAREF     Specific leaf area (SLA) for new leaf growth for the ecotype (cm2/g)
! SLAVAR     Specific Leaf Area for leaves grown under optimum conditions. 
! Smax       Maximum number of stalks a variety can yield, # stalks stubble-1
! SRDOT      Daily root senescence (g / m2 / d)
! StalkPopul(i) Stalk number per ground area on day i: stalks / m2
! StalkState(j,k)  Condition k of stalk j.  Currently, conditions are:
!                  k = 1 (LIVE or DEAD); 2 (PRIM or TILR); 3-10 unused             
! STKDMC      Stalk dry matter content (proportion)
! STKDPW(i,j) Potential increase in stalk weight; day i and stalk j (FSR)
!               (g[stalk tissue]/stalk / day)  
! Stk_H2O     Stalk moisture content (proportion)
! STKDW(i,j)  CH2O-limited increase in stalk weight; day i and stalk j 
!               (g[stalk tissue]/stalk / day)
! STKDWHalost(i) Daily stalk dry mass senescence, day i kg[stalk] / Ha / day
! StkEx      Daily stalk tissue growth based on existence of excess PG. 
! StkHrNO    Number of stalks last harvested from existing stubble.
! STKWT(i,j) Weight, on day i, of stalk j:  g[stalk tissue] / stalk
! STKFWT(i,j) FRESH weight, on day i, of stalk j (as harvested: tissue, H2O, sugar)
! STKWTHa(i) Stalk weight per ground area on day i:   kg[stalk tissue] / ha
! STKWTHalost(i)  Cumulative senesced stalk dry weight (no sugars) thru day i: 
!                kg[stalk tissue] / ha
! STKFWTP(i) Stalk FRESH weight per plant   g[stalk tissue] / plant 
! STKWTP(i)  Stalk structure weight per plant  g[stalk tissue] / plant
! SuDEF(j)   sucrose used for maintenance respiration when PG is insufficient. 
!            by stalk j   (g[sucrose]/stalk / day)
! SUDPW(i,j) Potential increase in sucrose weight; day i and stalk j (FSR)
!            (g[sucrose]/stalk / day)    
! SUDW(i,j)  CH2O-limited increase in sucrose weight; day i and stalk j 
!            (g[sucrose]/stalk / day)
! SuEx       Daily sucrose growth based on existence of excess PG. 
! SumLfWt    Rate of leaf mass growth in all stalks.  g[tissue]/plant/d
! SumRtWt    Rate of root mass growth in all stalks.  g[tissue]/plant/d
! SumStkSuWt Rate of sucrose growth in all stalks.  g[sucrose]/plant/d
! SumStkTiWt Rate of stalk tissue growth in all stalks.  g[tissue]/plant/d
! SUWT(i,j)  Weight, on day i, of sucrose in stalk j:  g[sucrose]/stalk/day
! SUWTHa(i)  sucrose weight per ground area on day i:   kg[sucrose]/ha
! SUWTP(i)   sucrose weight per plant (stubble) on day i:   kg[sucrose]/plant 
! TMPFCS    Interim value of TMPFAC 
! TOPWT    Total weight of above-ground portion of crop (g[tissue]/m2)
! TOTALWT(j) Total weight of sugarcane stalk j, including a proportion of 
!            root tissue   (g[tissue] / stalk j) 
! TOTWT    Total weight of crop (g[tissue] / m2)
! TPHFAC    Reduction in specific leaf area due to daytime temperature 
!             being less than optimal (0-1) 
! TURFAC    Water stress factor for expansion (0 - 1) 
!            (When TURFAC=1 No water stress?)
! TURFSL    Factor which applies water stress to specific leaf area of new 
!             leaf tissue growth 
! TURSLA    Water stress effects on leaf area expansion 
! TURADD    Water stress factor (TURFAC) effect on reproductive growth and 
!             pod addition.  Stress is defined to INCREASE growth and 
!             addition. 
! XFRSU(i) Parameter inputs (i = 1-4) into TABEX governing proportion of CH2O
!          partitioned to potential sucrose production
! YFRSU(i) Parameter inputs (i = 1-4) into TABEX governing proportion of CH2O
!          partitioned to potential sucrose production
! XLAI     Leaf area (one side) per unit of ground area: (m2[leaf] / m2[ground])
! XSLATM(I) Temperature values for function that reduces specific leaf area 
!             (SLA) (°C)
! YSLATM(I) Array which describes the effect of temperature on specific 
!             leaf area 
! ZVSDI    Node diameter; to be used for tiller weight; not used yet (cm)
