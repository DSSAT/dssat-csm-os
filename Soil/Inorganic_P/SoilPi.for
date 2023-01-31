C=======================================================================
C  SoilPi, Subroutine, U. Singh and C.H.Porter
C-----------------------------------------------------------------------
C  Inorganic Phosphorus
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/24/2005 CHP Written
!  06/17/2006 AJG Renamed all root-zone and no-root-zone variables with
!                 Rts and noRts.
!  03/01/2007 US/CHP Added soluble P pool. Available P now defined as soluble P 
!                    soluble P in root zone. 
!  03/20/2007 CHP Modified banded and hill fertilizer applications
!  03/21/2007 CHP Keep FracRts as fraction of total layer depth, 
!                 rather than SVRts as absolute volume. This is for 
!                 conservation of mass with tillage and compaction.
!  01/26/2023 CHP Reduce compile warnings: add EXTERNAL stmts, remove 
!                 unused variables, shorten lines. 
C=====================================================================

      SUBROUTINE SoilPi(CONTROL, ISWITCH, FLOODWAT,
     &    FERTDATA, IMM, MNR, PUptake, SOILPROP,          !Input
     &    FracRts, SW, TillVals,                          !Input
     &    SPi_AVAIL, SPi_Labile, YREND)                   !Output

C-----------------------------------------------------------------------
      USE ModuleDefs
      USE ModSoilMix
      USE FloodModule
      IMPLICIT NONE
      EXTERNAL SoilPi_init, P_ROOTS_INIT, INFO, P_FERTINDEX, 
     &  PCHECK, OpSOILPi, SoilPiBal
      SAVE

      CHARACTER*1  ISWPHO, RNMODE
      CHARACTER*17 SOILLAYERTYPE(NL)

      INTEGER DYNAMIC, FERTDAY, L, NLAYR, RUN, YRDOY, NBUND, YREND
      REAL FLOOD
      REAL FerLabFracRts, FerLabFracNoRts
      REAL FracLabRts, FracActRts, FracLabNoRts, FracActNoRts
      REAL P_FERTINDEX, PERROR, PFracRts
      REAL PMINERAL, PAvailRts, PAvailNoRts, PTotAvail
      REAL SPiAvlProf, SPiLabProf, SPiActProf, SPiStaProf, SPiTotProf
      REAL SumRtVol, SPiSolProf
      REAL, DIMENSION(NL) :: TexFac, WatFac

      REAL SPiSolRtsProf, SPiSolNoRtsProf
      REAL SPiLabRtsProf, SPiLabNoRtsProf

      REAL ProfLab2Act,  ProfAct2Lab,  ProfAct2Sta,  ProfSta2Act
      REAL Rate_Lab2Act, Rate_Act2Lab, Rate_Act2Sta, Rate_Sta2Act
      REAL Amt_Lab2Act,  Amt_Act2Lab,  Amt_Act2Sta,  Amt_Sta2Act
      REAL MIXPCT, TDEP   !Tillage mixing percent and depth

      REAL TMINERP, TIMMOBP   !Daily mineralization, immobilization
      REAL CMINERP, CIMMOBP   !Cumul mineralization, immobilization

      REAL, DIMENSION(NL) :: CaCO3, CLAY, DLAYR, DS, KG2PPM, PH, TOTBAS
      REAL, DIMENSION(NL) :: DLTSPiActRts,  DLTSPiLabRts,  DLTSPiStaRts
      REAL, DIMENSION(NL) :: DLTSPiActNoRts, DLTSPiLabNoRts
      REAL, DIMENSION(NL) :: DLTSPiStaNoRts, DLTFracRts
      REAL, DIMENSION(NL) :: K_ACT2LAB, K_ACT2STA, K_LAB2ACT, K_STA2ACT
      REAL, DIMENSION(NL) :: FracRts, FracRtsY, FracNoRts, FracPSoln
      REAL, DIMENSION(NL) :: PPM2KG_Rts, PPM2KG_NoRts
      REAL, DIMENSION(NL) :: ADDSPi, PUptake, SW
      REAL, DIMENSION(0:NL,NELEM) :: IMM, MNR   !From SOM model

!     Labile P (kg/ha):
      REAL, DIMENSION(NL) :: SPi_Labile, SPiLabRts, SPiLabNoRts
!     Active P (kg/ha):
      REAL, DIMENSION(NL) :: SPi_Active, SPiActRts, SPiActNoRts
!     Stable P (kg/ha):
      REAL, DIMENSION(NL) :: SPi_Stable, SPiStaRts, SPiStaNoRts
!     Soluble P (kg/ha):
      REAL, DIMENSION(NL) :: SPi_Soluble, SPiSolRts, SPiSolNoRts
!     Plant available P (kg/ha):
      REAL, DIMENSION(NL) :: SPi_AVAIL, SPi_Total

!     Labile P (µg[P]/g[soil]):
      REAL, DIMENSION(NL) :: PiLabile, PiLabRts, PiLabNoRts
!     Active P (µg[P]/g[soil]):
      REAL, DIMENSION(NL) :: PiActive, PiActRts, PiActNoRts
!     Stable P (µg[P]/g[soil]):
      REAL, DIMENSION(NL) :: PiStable, PiStaRts, PiStaNoRts
!     Soluble P (µg[P]/g[soil]):
      REAL, DIMENSION(NL) :: PiSolRts, PiSolNoRts

      LOGICAL FIRST

!==========================================
      INTEGER NMSG
      REAL SUM_ADDSPi
      CHARACTER*78  MSG(22)
!==========================================

!-----------------------------------------------------------------------
!    03/20/2007 CHP Need to call RootSoilVol to initialize root volume
!     when fertilizer added in bands or hills prior to planting.
      INTERFACE 
        SUBROUTINE RootSoilVol(DYNAMIC, ISWPHO,  
     &    DLAYR, DS, NLAYR,           !Input from all routines
     &    PLTPOP, RLV, RTDEP, FILECC, !Input from plant routine
     &    FracRts,                    !Output
     &    LAYER, AppType)      !Input from soil module (for banded fert)
          USE ModuleDefs
          CHARACTER*1,         INTENT(IN)           :: ISWPHO
          INTEGER,             INTENT(IN)           :: DYNAMIC, NLAYR
          REAL, DIMENSION(NL), INTENT(IN)           :: DS, DLAYR
          REAL, DIMENSION(NL), INTENT(OUT)          :: FracRts
          REAL,                INTENT(IN), OPTIONAL :: PLTPOP, RTDEP
          REAL, DIMENSION(NL), INTENT(IN), OPTIONAL :: RLV
          CHARACTER*92,        INTENT(IN), OPTIONAL :: FILECC
          INTEGER,             INTENT(IN), OPTIONAL :: LAYER
          CHARACTER*7,         INTENT(IN), OPTIONAL :: AppType
        END SUBROUTINE RootSoilVol
      END INTERFACE
!-----------------------------------------------------------------------

!     Notes on soluble P as used in this model
!     SPi_Soluble 
!     SPiSolRts   Used as P available for uptake by roots
!     SPiSolNoRts
!     PiSolRts
!     PiSolNoRts
!     Soluble P determined each day based on fraction of P in solution
!       for each layer.  Actual uptake, leaching, etc. subtracted
!       from labile pool, then soluble P recalculated as fraction.
!     Could replace the fraction with a transformation calculted with
!       a rate constant.
!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (SoilType)    SOILPROP
      TYPE (FertType)    FERTDATA
      TYPE (TillType)    TILLVALS
      TYPE (FloodWatType) FLOODWAT
      NBUND = FLOODWAT % NBUND
      FLOOD = FLOODWAT % FLOOD

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      RNMODE  = CONTROL % RNMODE
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY

      ISWPHO  = ISWITCH % ISWPHO

      CaCO3   = SOILPROP % CACO3
      CLAY    = SOILPROP % CLAY  
      DLAYR   = SOILPROP % DLAYR
      DS      = SOILPROP % DS    
      KG2PPM  = SOILPROP % KG2PPM
      NLAYR   = SOILPROP % NLAYR
      PH      = SOILPROP % PH    
      TOTBAS  = SOILPROP % TOTBAS
      SOILLAYERTYPE = SOILPROP % SOILLAYERTYPE

      FERTDAY = FERTDATA % FERTDAY

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!     Keep fraction of soil volume in direct contact with roots (FracRts) 
!     separate from remaining soil volume in each layer.  
!     Initial soil root volume is assumed to be zero (good for annuals only!)
      FracNoRts = 1.0
      FracRts   = 0.0
      FracRtsY  = 0.0  !previous root volume fraction by layer
      FIRST   = .TRUE.

      SPiActRts = 0.0
      SPiLabRts = 0.0
      SPiStaRts = 0.0
      SPiSolRts = 0.0
      SPi_AVAIL = 0.0

      PiActRts = 0.0
      PiLabRts = 0.0
      PiStaRts = 0.0
      PiSolRts = 0.0

      PUptake = 0.0
      PERROR  = 0.0

      DLTSPiLabRts   = 0.0
      DLTSPiActRts   = 0.0
      DLTSPiStaRts   = 0.0
      DLTSPiLabNoRts = 0.0
      DLTSPiActNoRts = 0.0
      DLTSPiStaNoRts = 0.0

!     Today's values
      TMINERP  = 0.0  !mineralization
      TIMMOBP  = 0.0  !immobilization

!     Seasonal cumulative values
      CMINERP  = 0.0  !mineralization
      CIMMOBP  = 0.0  !immobilization

      IF (RUN .EQ. 1 .OR. INDEX('QF',RNMODE) .LE. 0) THEN
!       Non-sequenced runs
        IF (ISWPHO .EQ. 'N') THEN
          SPi_Active  = 0.0
          SPi_Labile  = 0.0
          SPi_Stable  = 0.0
          SPi_Soluble = 0.0
          SPi_AVAIL   = 0.0

          SPiActNoRts = 0.0
          SPiLabNoRts = 0.0
          SPiStaNoRts = 0.0
          SPiSolNoRts = 0.0

          RETURN
        ENDIF

        CALL SoilPi_init (CONTROL,       
     &    ISWPHO, SOILPROP,                               !Input
     &    K_ACT2LAB, K_ACT2STA, K_LAB2ACT, K_STA2ACT,     !Output
     &    PiActive, PiLabile, PiStable)                   !Output

!       Error in initialization
        IF (YREND == YRDOY) RETURN

        DO L = 1, NLAYR
!         03/01/2007 US/CHP added fraction of P in solution
          SELECT CASE(SoilLayerType(L))
            CASE('CALCAREOUS       '); FracPSoln(L) = 0.05
            CASE('HIGHLYWEATHERED  '); FracPSoln(L) = 0.015
            CASE('ANDISOL          '); FracPSoln(L) = 0.008
            CASE DEFAULT             ; FracPSoln(L) = 0.02
          END SELECT

          SPi_Active(L) = PiActive(L) / KG2PPM(L)
          SPi_Stable(L) = PiStable(L) / KG2PPM(L)
          SPi_Labile(L) = PiLabile(L) / KG2PPM(L)


          IF (NBUND .GT. 0 .OR. FLOOD .GT. 0) THEN
             TexFac = 3.85
	    ELSE
	       TexFac = 0.25 + 0.25 * (SOILPROP % SAT(L)/ 
     &                SOILPROP % LL(L))
          ENDIF
          WatFac(L) = AMAX1(SW(L) / SOILPROP % LL(L), 0.85)

          SPi_Soluble(L)= SPi_Labile(L)* AMIN1 (1.0, 
     &                    FracPSoln(L) * TexFac(L) * WatFac(L))
        ENDDO
      ENDIF

      SPiActNoRts = SPi_Active
      SPiLabNoRts = SPi_Labile
      SPiStaNoRts = SPi_Stable
      SPiSolNoRts = SPi_Soluble

!***********************************************************************
      ENDIF

!***********************************************************************
!***********************************************************************
!     Rate Calculations 
!***********************************************************************
      IF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
      IF (ISWPHO .EQ. 'N') RETURN

!     Set Dlt variables to zero every day
      DLTSPiLabRts   = 0.
      DLTSPiActRts   = 0.
      DLTSPiStaRts   = 0.
      DLTSPiLabNoRts = 0.
      DLTSPiActNoRts = 0.
      DLTSPiStaNoRts = 0.

      ProfLab2Act = 0.0
      ProfAct2Lab = 0.0
      ProfAct2Sta = 0.0
      ProfSta2Act = 0.0

!     Check for root growth (plant emergence)
      SumRtVol = 0.0
      DO L = 1, NLAYR
        SumRtVol = SumRtVol + FracRts(L) * DLAYR(L)
      ENDDO

      IF (SumRtVol > 1.E-6 .AND. FIRST) THEN
        CALL P_ROOTS_INIT(
     &    DLAYR, FracPSoln, FracRts, KG2PPM, NLAYR,       !Input
     &    SPi_Active, SPi_Labile, SPi_Stable, SW,         !Input
     &    FracNoRts, PiActNoRts, PiActRts, PiLabNoRts,    !Output
     &    PiLabRts, PiSolNoRts, PiSolRts, PiStaNoRts,     !Output
     &    PiStaRts, PPM2KG_Rts, PPM2KG_NoRts, SPi_Soluble,!Output
     &    SPiActNoRts, SPiActRts, SPiLabNoRts, SPiLabRts, !Output
     &    SPiSolNoRts, SPiSolRts, SPiStaNoRts, SPiStaRts) !Output
        FIRST = .FALSE.
        FracRtsY = FracRts
      ENDIF

!-----------------------------------------------------------------------
!     P Uptake, increase in root volume
      DO L = 1, NLAYR
        FracNoRts(L) = 1.0 - FracRts(L)

!       ----------------------------------------------------------------
        IF (PUptake(L) > 1.E-5) THEN
!       P uptake from yesterday, reduce combined labile+soluble pool.
!          SPiLabRts(L)  = SPiLabRts(L) - PUptake(L)
!          PPM2KG_Rts(L) = FracRts(L) / KG2PPM(L)
!          PiLabRts(L)   = SPiLabRts(L) / PPM2KG_Rts(L)

          IF (PUptake(L) < SPiSolRts(L)) THEN
!           All uptake is from soluble P, in contact with roots
            SPiLabRts(L)  = SPiLabRts(L) - PUptake(L)
          ELSEIF (PUptake(L) < SPiSolRts(L) + SPiSolNoRts(L)) THEN
!           All uptake is from soluble P, take 1st from root pool, then
!             remainder from non-root pool.
            SPiLabRts(L)  = SPiLabRts(L)   - SPiSolRts(L)
            SPiLabNoRts(L)= SPiLabNoRts(L) - (PUptake(L) - SPiSolRts(L))
          ELSE
!           Uptake from soluble P and labile P in contact with roots
            SPiLabNoRts(L)= SPiLabNoRts(L) - SPiSolNoRts(L)
            SPiLabRts(L)  = SPiLabRts(L) - (PUptake(L) - SPiSolNoRts(L))
          ENDIF

          PPM2KG_Rts(L) = FracRts(L) / KG2PPM(L)
          PiLabRts(L)   = SPiLabRts(L) / PPM2KG_Rts(L)

          IF (FracNoRts(L) > 1.E-6) THEN
            PPM2KG_NoRts(L) = FracNoRts(L) / KG2PPM(L)
            PiLabNoRts(L)   = SPiLabNoRts(L) / PPM2KG_NoRts(L)
          ELSE
            PPM2KG_NoRts(L) = 0.0
            PiLabNoRts(L)   = 0.0
          ENDIF

        ENDIF

!       Update soil volumes
        IF (1.0 - FracRtsY(L) > 1.E-6) THEN
          DLTFracRts(L) = (FracRts(L) - FracRtsY(L)) / (1.0-FracRtsY(L))
          DLTFracRts(L) = MAX(0.0, DLTFracRts(L))
          DLTFracRts(L) = MIN(1.0, DLTFracRts(L))
        ELSE
          DLTFracRts(L) = 0.0
        ENDIF

!       Addition of P with new root soil volume
        IF (DLTFracRts(L) > 1.E-6) THEN
          DLTSPiLabRts(L)   = SPiLabNoRts(L) * DLTFracRts(L)
          DLTSPiLabRts(L)   = AMIN1(DLTSPiLabRts(L), SPiLabNoRts(L))
          DLTSPiLabNoRts(L) = -DLTSPiLabRts(L)

          DLTSPiActRts(L)   = SPiActNoRts(L) * DLTFracRts(L)
          DLTSPiActRts(L)   = AMIN1(DLTSPiActRts(L), SPiActNoRts(L))
          DLTSPiActNoRts(L) = -DLTSPiActRts(L)

          DLTSPiStaRts(L)   = SPiStaNoRts(L) * DLTFracRts(L)
          DLTSPiStaRts(L)   = AMIN1(DLTSPiStaRts(L), SPiStaNoRts(L))
          DLTSPiStaNoRts(L) = -DLTSPiStaRts(L)
        ENDIF

        IF (FracNoRts(L) <= 0.) THEN
          DLTSPiLabRts(L)   = AMAX1(0.0, SPiLabNoRts(L))
          DLTSPiLabNoRts(L) = -DLTSPiLabRts(L)

          DLTSPiActRts(L)   = AMAX1(0.0, SPiActNoRts(L))
          DLTSPiActNoRts(L) = -DLTSPiActRts(L)

          DLTSPiStaRts(L)   = AMAX1(0.0, SPiStaNoRts(L))
          DLTSPiStaNoRts(L) = -DLTSPiStaRts(L)
        ENDIF
      ENDDO

!-----------------------------------------------------------------------
!     Fertilizer added today:
      IF (YRDOY == FERTDAY) THEN
        ADDSPi  = FERTDATA % ADDSPi

        SUM_ADDSPi = SUM(ADDSPi)
        IF (SUM_ADDSPi > 1.E-6) THEN
          WRITE(MSG(1),'(A)') "Layer    Fertilizer P (kg/ha)"
          NMSG = 1      !      ...I5....F8.1
        ENDIF

        DO L = 1, NLAYR
          IF (ADDSPi(L) > 1.E-6) THEN
!           Fraction of fertilizer to root zone

!           ------------------------------------------------------
            SELECT CASE (TRIM(FertData % AppType))
!           Fertilizer applied in bands or hills
            CASE ('BANDED','HILL','POINT','DRIP')
              IF (FracRts(L) < 1.E-5) THEN
!               Banded fertilizer is applied before root growth in this 
!               layer.  Set initial minimum root zone for application.
                CALL RootSoilVol (DYNAMIC, ISWPHO,   
     &              DLAYR, DS, NLAYR,                           !Input 
     &              LAYER=L, AppType=FertData%AppType,          !Input 
     &              FracRts=FracRts)                            !Output

!               Initialize root zone and non-root zone pools
                CALL P_ROOTS_INIT(
     &           DLAYR, FracPSoln, FracRts, KG2PPM, NLAYR,       !Input
     &           SPi_Active, SPi_Labile, SPi_Stable, SW,         !Input
     &           FracNoRts, PiActNoRts, PiActRts, PiLabNoRts,    !Output
     &           PiLabRts, PiSolNoRts, PiSolRts, PiStaNoRts,     !Output
     &           PiStaRts, PPM2KG_Rts, PPM2KG_NoRts, SPi_Soluble,!Output
     &           SPiActNoRts, SPiActRts, SPiLabNoRts, SPiLabRts, !Output
     &           SPiSolNoRts, SPiSolRts, SPiStaNoRts, SPiStaRts) !Output
                FIRST = .FALSE.
              ENDIF

!             Banded fertilizer is applied to root zone, split between
!             labile and active pools.
              FracLabRts = P_FERTINDEX (CaCO3(L), CLAY(L), PH(L), 
     &          PiLabRts(L), SOILLAYERTYPE(L), TOTBAS(L), NBUND, FLOOD)
              FracActRts  = 1. - FracLabRts

              WRITE(MSG(NMSG+1),'(I5,F8.1,A)') 
     &          L, ADDSPi(L)*FracLabRts, " Added to root zone labile P"
              WRITE(MSG(NMSG+2),'(I5,F8.1,A)') 
     &          L, ADDSPi(L)*FracActRts, " Added to root zone active P"
              NMSG = NMSG + 2

              DLTSPiLabRts(L) = DLTSPiLabRts(L)  + ADDSPi(L) *
     &          FracLabRts
              DLTSPiActRts(L) = DLTSPiActRts(L)  + ADDSPi(L) *
     &          FracActRts

!           ------------------------------------------------------
            CASE DEFAULT
!             Fertilizer applied evenly - not banded or hill
!             P is evenly applied over both root and no-root soil pools
!             Fraction of fertilizer to labile pool depends on soil 
!             parameters and labile P concentration.
              FerLabFracRts = P_FERTINDEX (CaCO3(L), CLAY(L), PH(L), 
     &         PiLabRts(L), SOILLAYERTYPE(L), TOTBAS(L), NBUND, FLOOD)
              FerLabFracNoRts = P_FERTINDEX (CaCO3(L), CLAY(L), PH(L), 
     &        PiLabNoRts(L), SOILLAYERTYPE(L), TOTBAS(L), NBUND,FLOOD)
!             Overall fraction of fertilizer to roots and no root zones, and
!             to labile and active pools.
              FracLabRts  = FerLabFracRts * FracRts(L)
              FracActRts  = (1. - FerLabFracRts) * FracRts(L)
              FracLabNoRts = FerLabFracNoRts * (1. - FracRts(L))
              FracActNoRts = (1. - FerLabFracNoRts) * FracNoRts(L)

              WRITE(MSG(NMSG+1),'(I5,F8.1,A)') L, 
     &         ADDSPi(L)*FracLabRts, " Added to root zone labile P"
              WRITE(MSG(NMSG+2),'(I5,F8.1,A)') L, 
     &         ADDSPi(L)*FracActRts, " Added to root zone active P"
              WRITE(MSG(NMSG+3),'(I5,F8.1,A)') L, 
     &         ADDSPi(L)*FracLabNoRts," Added to non-root zone labile P"
              WRITE(MSG(NMSG+4),'(I5,F8.1,A)') L, 
     &         ADDSPi(L)*FracActNoRts," Added to non-root zone active P"
              NMSG = NMSG + 4

!             Add fertilizer to labile and active pools, proportion to roots
!             and no-roots zones.
              DLTSPiLabRts(L) = DLTSPiLabRts(L) + ADDSPi(L) * FracLabRts
              DLTSPiActRts(L) = DLTSPiActRts(L) + ADDSPi(L) * FracActRts
              DLTSPiLabNoRts(L)=DLTSPiLabNoRts(L)+ADDSPi(L)*FracLabNoRts
              DLTSPiActNoRts(L)=DLTSPiActNoRts(L)+ADDSPi(L)*FracActNoRts
            END SELECT
          ENDIF
        ENDDO
        IF (SUM(ADDSPi) > 1.E-6) THEN
          NMSG = NMSG + 1
          WRITE(MSG(NMSG),'(A,F8.1)') "Total", SUM(ADDSPi)
          CALL INFO(NMSG,"SOILPI",MSG)
        ENDIF
      ENDIF

!     ------------------------------------------------------------------
!     Check if tillage occurred today -- if so, mix soil P within
!       tillage depth.  Do for non-root P species only.
      IF (TILLVALS % NTIL .GT. 0) THEN
        IF (YRDOY .EQ. TILLVALS % TILDATE) THEN
          !Call mixing routine for soil properties
          MIXPCT = TILLVALS % TILMIX
          TDEP = TILLVALS % TILDEP
          CALL SoilMix (SPiActNoRts, DLTSPiActNoRts, 1, DLAYR, MIXPCT, 
     &                                                      NLAYR, TDEP)
          CALL SoilMix (SPiLabNoRts, DLTSPiLabNoRts, 1, DLAYR, MIXPCT, 
     &                                                      NLAYR, TDEP)
          CALL SoilMix (SPiStaNoRts, DLTSPiStaNoRts, 1, DLAYR, MIXPCT, 
     &                                                      NLAYR, TDEP)
        ENDIF
      ENDIF

!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
      PMINERAL = 0.0

!     Daily P transformations
      DO L = 1, NLAYR
!       ----------------------------------------------------------------
!       PMINERAL is net mineralization in layer L
        IF (L == 1) THEN
!         First layer takes mineralization from surface also.
          PMINERAL = MNR(0,P) + MNR(1,P) - IMM(0,P) - IMM(1,P)
        ELSE
!         Add in residual from previous layer to preserve P balance
          PMINERAL = PMINERAL + MNR(L,P) - IMM(L,P)
        ENDIF

!       If the net P release from all SOM sources (PMINERAL) is positive,
!       add the mineralized p to the labile pool.
        IF (PMINERAL > 1.E-6) THEN
!         ------------------------------------------------------------
!         MINERALIZATION
!         Add mineralization to labile pool, proportion to soil pools
!         based on soil volume.
          DLTSPiLabRts(L)   = DLTSPiLabRts(L)   + PMINERAL *  FracRts(L)
          DLTSPiLabNoRts(L) = DLTSPiLabNoRts(L) + PMINERAL *FracNoRts(L)
          PMINERAL = 0.

        ELSEIF (PMINERAL < -1.E-6) THEN 
!         ------------------------------------------------------------
!         IMMOBILIZATION
!         Calculate P available for immobilization (pseudo-integration) 
!         for root zone and no-root zone.
          PAvailRts   = SPiLabRts(L)   + DLTSPiLabRts(L)
          PAvailNoRts = SPiLabNoRts(L) + DLTSPiLabNoRts(L)
          PTotAvail = PAvailRts + PAvailNoRts

!         Fraction of total available P which is in root zone.
          IF (PTotAvail > 1.E-6) THEN
            PFracRts = PAvailRts / PTotAvail
          ENDIF

!         PMINERAL is negative -- immobilization
          IF (ABS(PMINERAL) <= PTotAvail) THEN
!           Sufficient P to meet immobilization demand.  Proportion 
!           immobilization based on available P in each root zone.
!           Limit immobilization to what is there.
            DLTSPiLabRts(L)  = DLTSPiLabRts(L)  + PMINERAL * PFracRts
            DLTSPiLabNoRts(L)= DLTSPiLabNoRts(L)+ PMINERAL*(1.-PFracRts)
            PMINERAL = 0.

          ELSE    !(ABS(PMINERAL) > PTotAvail)
!           Insufficient P to meet immobilization demand. Proportion 
!           immobilization based on available P in each root zone and leave
!           a remainder.  
!           The remainder will be added to demand for next layer.
            DLTSPiLabRts(L)   = DLTSPiLabRts(L)   - PAvailRts
            DLTSPiLabNoRts(L) = DLTSPiLabNoRts(L) - PAvailNoRts
            PMINERAL = PMINERAL + PTotAvail
          ENDIF   !End of IF block on ABS(PMINERAL).
        ENDIF   !End of IF block on PMINERAL.

        IF (L == NLAYR .AND. ABS(PMINERAL) > 1.E-3) THEN
!         Due to checks in computation of immobilization, this
!         should never happen.
          PERROR = PERROR + PMINERAL
        ENDIF

!       ----------------------------------------------------------------
!       Inorganic soil transformations.

!       ------------------------------------
!       Root zone:
!       ------------------------------------
!       Compute rates of transformation
        IF (FracRts(L) > 1.E-5) THEN
          Rate_Lab2Act = K_Lab2Act(L) * PiLabRts(L)   !ppm
          Amt_Lab2Act  = Rate_Lab2Act * PPM2KG_Rts(L)  !kg/ha
        
          Rate_Act2Lab = K_Act2Lab(L) * PiActRts(L)
          Amt_Act2Lab  = Rate_Act2Lab * PPM2KG_Rts(L)
        
          Rate_Act2Sta = K_Act2Sta(L) * PiActRts(L)
          Amt_Act2Sta  = Rate_Act2Sta * PPM2KG_Rts(L)
        
          Rate_Sta2Act = K_Sta2Act(L) * PiStaRts(L)
          Amt_Sta2Act  = Rate_Sta2Act * PPM2KG_Rts(L)
        
!         Add transformations to Dlt variables 
          DLTSPiLabRts(L) = DLTSPiLabRts(L) + Amt_Act2Lab - Amt_Lab2Act
          DLTSPiActRts(L) = DLTSPiActRts(L) - Amt_Act2Lab + Amt_Lab2Act 
     &                                      + Amt_Sta2Act - Amt_Act2Sta 
          DLTSPiStaRts(L) = DLTSPiStaRts(L) - Amt_Sta2Act + Amt_Act2Sta
        
!         Sum transformations across profile
          ProfLab2Act = ProfLab2Act + Amt_Lab2Act
          ProfAct2Lab = ProfAct2Lab + Amt_Act2Lab
          ProfAct2Sta = ProfAct2Sta + Amt_Act2Sta
          ProfSta2Act = ProfSta2Act + Amt_Sta2Act
        ENDIF

!       ------------------------------------
!       No root zone:
!       ------------------------------------
!       Compute rates of transformation
        IF (FracNoRts(L) > 1.E-5) THEN
          PPM2KG_NoRts(L) = FracNoRts(L) / KG2PPM(L)
          Rate_Lab2Act = K_Lab2Act(L) * PiLabNoRts(L)
          Amt_Lab2Act  = Rate_Lab2Act * PPM2KG_NoRts(L)
        
          Rate_Act2Lab = K_Act2Lab(L) * PiActNoRts(L)
          Amt_Act2Lab  = Rate_Act2Lab * PPM2KG_NoRts(L)
        
          Rate_Act2Sta = K_Act2Sta(L) * PiActNoRts(L)
          Amt_Act2Sta  = Rate_Act2Sta * PPM2KG_NoRts(L)
        
          Rate_Sta2Act = K_Sta2Act(L) * PiStaNoRts(L)
          Amt_Sta2Act  = Rate_Sta2Act * PPM2KG_NoRts(L)
        
!         Add transformations to Dlt variables 
          DLTSPiLabNoRts(L) = DLTSPiLabNoRts(L) 
     &                                       + Amt_Act2Lab - Amt_Lab2Act
          DLTSPiActNoRts(L) = DLTSPiActNoRts(L)  
     &                                       - Amt_Act2Lab + Amt_Lab2Act
     &                                       + Amt_Sta2Act - Amt_Act2Sta
          DLTSPiStaNoRts(L) = DLTSPiStaNoRts(L)  
     &                                       - Amt_Sta2Act + Amt_Act2Sta
        
!         Sum transformations across profile
          ProfLab2Act = ProfLab2Act + Amt_Lab2Act
          ProfAct2Lab = ProfAct2Lab + Amt_Act2Lab
          ProfAct2Sta = ProfAct2Sta + Amt_Act2Sta
          ProfSta2Act = ProfSta2Act + Amt_Sta2Act
        ENDIF
      ENDDO

!***********************************************************************
      ENDIF

!***********************************************************************
!***********************************************************************
!     Daily integration
!***********************************************************************
      IF (DYNAMIC .EQ. INTEGR .OR. DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
      IF (ISWPHO .EQ. 'N') RETURN

      SPiAvlProf = 0.
      SPiLabProf = 0.
      SPiActProf = 0.
      SPiStaProf = 0.
      SPiSolProf = 0.
      SPiTotProf = 0.

      SPiLabRtsProf  = 0.0
      SPiLabNoRtsProf= 0.0
      SPiSolRtsProf  = 0.0
      SPiSolNoRtsProf= 0.0

      DO L = 1, NLAYR
!       Update soil P content
        SPiActRts(L) = SPiActRts(L) + DLTSPiActRts(L)
        SPiStaRts(L) = SPiStaRts(L) + DLTSPiStaRts(L)
        SPiLabRts(L) = SPiLabRts(L) + DLTSPiLabRts(L)

        SPiActNoRts(L) = SPiActNoRts(L) + DLTSPiActNoRts(L)
        SPiStaNoRts(L) = SPiStaNoRts(L) + DLTSPiStaNoRts(L)
        SPiLabNoRts(L) = SPiLabNoRts(L) + DLTSPiLabNoRts(L)
      ENDDO

      CALL PCHECK(NLAYR, YRDOY, 
     &    SPiLabRts, SPiActRts, SPiStaRts, 
     &    SPiLabNoRts, SPiActNoRts, SPiStaNoRts) 

      TMINERP = MNR(0,P)  !mineralization
      TIMMOBP = IMM(0,P)  !immobilization

      DO L = 1, NLAYR

!       Water factor for fraction of P in solution
        WatFac(L) = AMAX1(SW(L) / SOILPROP % LL(L), 0.85)
        IF (NBUND .GT. 0 .OR. FLOOD .GT. 0) THEN
           TexFac = 3.85
	  ELSE
	     TexFac = 0.25 + 0.25 * (SOILPROP % SAT(L)/ 
     &                SOILPROP % LL(L))
        ENDIF

 !       Calculate solution P in roots and no-roots zones
        SPiSolRts(L)  = SPiLabRts(L) * AMIN1 (1.0, 
     &                  FracPSoln(L) * TexFac(L) * WatFac(L))
        SPiSolNoRts(L)= SPiLabNoRts(L)* AMIN1 (1.0, 
     &                  FracPSoln(L) * TexFac(L) * WatFac(L))

!       --------------------------------------------------------------
!       03/01/2007 US/CHP 
        SPi_AVAIL(L) = SPiSolRts(L)
!       04/11/2007 CHP
!       Limiting plant available P to soluble P adjacent to roots is 
!         too restrictive.
!       Include part of labile P in contact w/ roots. 
!       SPi_AVAIL(L) = SPi_AVAIL(L) + FracPAvail * SPiLabRts(L)
!       Include soluble P in non-root zone (mobile) (only if roots are present)
!       This provided too much P to roots. -- remove.
!        IF (SPi_Avail(L) > 1.E-6) THEN
!          SPi_AVAIL(L) = SPi_AVAIL(L) + SPiSolNoRts(L) 
!        ENDIF

!       Layer total inorganic P
        SPi_Labile(L) = SPiLabRts(L) + SPiLabNoRts(L) 
        SPi_Active(L) = SPiActRts(L) + SPiActNoRts(L) 
        SPi_Stable(L) = SPiStaRts(L) + SPiStaNoRts(L) 
        SPi_Soluble(L)= SPiSolRts(L) + SPiSolNoRts(L) 
        SPi_Total(L)  = SPi_Labile(L) + SPi_Active(L) + SPi_Stable(L)

        SPiAvlProf = SPiAvlProf + SPi_AVAIL(L)
        SPiLabProf = SPiLabProf + SPi_Labile(L)
        SPiActProf = SPiActProf + SPi_Active(L)    
        SPiStaProf = SPiStaProf + SPi_Stable(L)  
        SPiSolProf = SPiSolProf + SPi_Soluble(L)  

        SPiSolRtsProf  = SPiSolRtsProf  + SPiSolRts(L)
        SPiSolNoRtsProf= SPiSolNoRtsProf+ SPiSolNoRts(L)
        SPiLabRtsProf  = SPiLabRtsProf  + SPiLabRts(L)
        SPiLabNoRtsProf= SPiLabNoRtsProf+ SPiLabNoRts(L)

!       Conversions
        IF (FracRts(L) > 1.E-5) THEN
          PPM2KG_Rts(L) = FracRts(L) / KG2PPM(L)
          PiLabRts(L) = SPiLabRts(L) / PPM2KG_Rts(L)
          PiActRts(L) = SPiActRts(L) / PPM2KG_Rts(L)
          PiStaRts(L) = SPiStaRts(L) / PPM2KG_Rts(L)
!         Soluble P in mg[P]/l[water]
          PiSolRts(L) = SPiSolRts(L) / SW(L) / DLAYR(L) / FracRts(L)*10.
        ELSE
          PiLabRts(L) = 0.
          PiActRts(L) = 0.
          PiStaRts(L) = 0.
          PiSolRts(L) = 0.
        ENDIF
       
        IF (FracNoRts(L) > 1.E-5) THEN
          PPM2KG_NoRts(L) = FracNoRts(L) / KG2PPM(L)
          PiLabNoRts(L) = SPiLabNoRts(L) / PPM2KG_NoRts(L)
          PiActNoRts(L) = SPiActNoRts(L) / PPM2KG_NoRts(L)
          PiStaNoRts(L) = SPiStaNoRts(L) / PPM2KG_NoRts(L)
!         Soluble P in mg[P]/l[water]
          PiSolNoRts(L) = SPiSolNoRts(L)/SW(L)/DLAYR(L)/FracNoRts(L)*10.
        ELSE
          PiLabNoRts(L) = 0.
          PiActNoRts(L) = 0.
          PiStaNoRts(L) = 0.
          PiSolNoRts(L) = 0.
        ENDIF

        PiLabile(L) = SPi_Labile(L) * KG2PPM(L) 
        PiActive(L) = SPi_Active(L) * KG2PPM(L) 
        PiStable(L) = SPi_Stable(L) * KG2PPM(L) 

!       Daily accumulation of mineralization and immobilization
        TMINERP = TMINERP + MNR(L,P)
        TIMMOBP = TIMMOBP + IMM(L,P)
      ENDDO

      SPiTotProf = SPiLabProf + SPiActProf + SPiStaProf
      CIMMOBP = CIMMOBP + TIMMOBP
      CMINERP = CMINERP + TMINERP

      DLTSPiLabRts = 0.
      DLTSPiActRts = 0.
      DLTSPiStaRts = 0.

      DLTSPiLabNoRts = 0.
      DLTSPiActNoRts = 0.
      DLTSPiStaNoRts = 0.

      FracRtsY   = FracRts

      IF (DYNAMIC .EQ. SEASINIT) THEN
        CALL OpSOILPi(CONTROL, ISWITCH, 
     &  CImmobP, CMinerP, FertData, PUptake,   
     &  SOILPROP, SPi_AVAIL, SPiAvlProf, PiLabile,
     &  SPiSolProf, SPiLabProf, SPiActProf, SPiStaProf, SPiTotProf)

        CALL SoilPiBal (CONTROL, ISWITCH,
     &    FertData, IMM, MNR, NLAYR, PUptake, 
     &    SPiActProf, SPiLabProf, SPiStaProf)
      ENDIF

!***********************************************************************
!***********************************************************************
!     OUTPUT
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
C-----------------------------------------------------------------------
      IF (ISWPHO .EQ. 'N') RETURN

      CALL OpSOILPi(CONTROL, ISWITCH, 
     &  CImmobP, CMinerP, FertData, PUptake,   
     &  SOILPROP, SPi_AVAIL, SPiAvlProf, PiLabile,
     &  SPiSolProf, SPiLabProf, SPiActProf, SPiStaProf, SPiTotProf)

      CALL SoilPiBal (CONTROL, ISWITCH,
     &    FertData, IMM, MNR, NLAYR, PUptake, 
     &    SPiActProf, SPiLabProf, SPiStaProf)

!***********************************************************************
!***********************************************************************
!     SEASEND
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
      IF (ISWPHO .EQ. 'N') RETURN

      CALL OpSOILPi(CONTROL, ISWITCH, 
     &  CImmobP, CMinerP, FertData, PUptake,   
     &  SOILPROP, SPi_AVAIL, SPiAvlProf, PiLabile,
     &  SPiSolProf, SPiLabProf, SPiActProf, SPiStaProf, SPiTotProf)

      CALL SoilPiBal (CONTROL, ISWITCH,
     &    FertData, IMM, MNR, NLAYR, PUptake, 
     &    SPiActProf, SPiLabProf, SPiStaProf)

C***********************************************************************
C***********************************************************************
C     END OF DYNAMIC IF CONSTRUCT
C***********************************************************************
      ENDIF
C***********************************************************************

      RETURN
      END SUBROUTINE SoilPi

C***********************************************************************
!=======================================================================
!  FUNCTION P_FERTINDEX
!
!  Calculates the fertilizer-P availability index of Jones et al. 1984
!  and Sharpley et al. 1984+1989.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  03/31/2004 AJG Written
!  03/23/2005 US Modified
!-----------------------------------------------------------------------
!  Called by: P_INI
!  Calls:     
!=======================================================================

      REAL FUNCTION P_FERTINDEX (
     & CaCO3, CLAY, PH, PiLabile, SOILLAYERTYPE, TOTBAS, NBUND, FLOOD)

!     ------------------------------------------------------------------
      IMPLICIT  NONE

      CHARACTER*17 SOILLAYERTYPE
      REAL CaCO3, CLAY, PH, PiLabile, TOTBAS, FLOOD
	INTEGER NBUND
!     ------------------------------------------------------------------

!     Default value:
      IF (PiLabile < 1.) THEN
!       Singh, 1985; Montage & Zapata, 2002 (IAEA)
        P_FERTINDEX = AMIN1(0.60 + 0.00020 ** (PiLabile), 0.90)
      ELSE
        P_FERTINDEX = 0.60
      ENDIF

      SELECT CASE (SOILLAYERTYPE)
!       Calcareous soils.
        CASE('CALCAREOUS       ')
          IF (CaCO3 > -1.E-6) THEN
!           Sharpley et al (1989) Table 4.
            P_FERTINDEX = -0.0042 * CaCO3 + 0.72
          ENDIF

!       Slightly weathered soils.
        CASE('SLIGHTLYWEATHERED')
          IF (TOTBAS > -1.E-6 .AND. PH > 0.) THEN
!           Sharpley et al (1984) Table 4. 
            P_FERTINDEX = AMIN1((0.0043 * TOTBAS + 0.0034 * PiLabile 
     &        + 0.11 * PH - 0.50),0.90)
          ENDIF

!       Highly weathered soils.
        CASE('HIGHLYWEATHERED  ')
!         Sharpley et al (1989) Table 4.
          IF (CLAY > 1.E-6) THEN   !Protection from LOG(0).
            P_FERTINDEX = -0.19 * ALOG10(CLAY) + 0.70  
          ENDIF

!       ANDISOLS - Volcanic ash soils (number of samples = 21).
!       CASE('ANDISOL          ')
! NEED TO SUPPLY AN EQUATION HERE
!         P_FERTINDEX = AMIN1(0.60 + 0.00020 ** (PiLabile), 0.90)

      END SELECT

!     For flooded soil, P availabilty is higher
        IF (NBUND. GT. 0. or. FLOOD .GT. 0.0) THEN
          P_FERTINDEX = AMAX1(P_FERTINDEX,1.0)
        ENDIF

      RETURN
      END FUNCTION P_FERTINDEX

!=======================================================================

!=======================================================================
!  SUBROUTINE PCHECK
!  Check for invalid soil inorganic P values
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  04/15/2005 CHP Written based on NCHECK routines
!=======================================================================
      SUBROUTINE PCHECK(NLAYR, YRDOY, 
     &    SPiLabRts, SPiActRts, SPiStaRts, 
     &    SPiLabNoRts, SPiActNoRts, SPiStaNoRts) 

!     ------------------------------------------------------------------
      USE ModuleDefs 
      IMPLICIT  NONE
      EXTERNAL WARNING
      SAVE
!     ------------------------------------------------------------------
      CHARACTER*78  MSG(10)
      INTEGER L, NLAYR, YRDOY
      REAL, PARAMETER :: TOL = -1.E-3
      REAL, DIMENSION(NL) :: SPiLabRts,   SPiActRts,   SPiStaRts, 
     &                       SPiLabNoRts, SPiActNoRts, SPiStaNoRts

!-----------------------------------------------------------------------
      WRITE(MSG(3),"('Value will be set to zero.')")
  100 FORMAT('Negative soil P value on day ',I7,' in layer ',I3)

!     Check for negative soil N values
      DO L = 1, NLAYR
        IF (SPiLabRts(L) .LT. TOL) THEN
          WRITE(MSG(1),100) YRDOY, L
          WRITE(MSG(2),"('SPiLabRts =',F10.3,' kg[P]/ha')") 
     &                    SPiLabRts(L)
          CALL WARNING(3, "PCHECK", MSG)
          SPiLabRts(L) = 0.0
        ENDIF

        IF (SPiActRts(L) .LT. TOL) THEN
          WRITE(MSG(1),100) YRDOY, L
          WRITE(MSG(2),"('SPiActRts =',F10.3,' kg[P]/ha')") 
     &                    SPiActRts(L) 
          CALL WARNING(3, "PCHECK", MSG)
          SPiActRts(L) = 0.0
        ENDIF

        IF (SPiStaRts(L) .LT. TOL) THEN
          WRITE(MSG(1),100) YRDOY, L
          WRITE(MSG(2),"('SPiStaRts =',F10.3,' kg[P]/ha')") 
     &                    SPiStaRts(L)
          CALL WARNING(3, "PCHECK", MSG)
          SPiStaRts(L) = 0.0
        ENDIF

        IF (SPiLabNoRts(L) .LT. TOL) THEN
          WRITE(MSG(1),100) YRDOY, L
          WRITE(MSG(2),"('SPiLabNoRts =',F10.3,' kg[P]/ha')") 
     &                    SPiLabNoRts(L)
          CALL WARNING(3, "PCHECK", MSG)
          SPiLabNoRts(L) = 0.0
        ENDIF

        IF (SPiActNoRts(L) .LT. TOL) THEN
          WRITE(MSG(1),100) YRDOY, L
          WRITE(MSG(2),"('SPiActNoRts =',F10.3,' kg[P]/ha')") 
     &                    SPiActNoRts(L)
          CALL WARNING(3, "PCHECK", MSG)
          SPiActNoRts(L) = 0.0
        ENDIF

        IF (SPiStaNoRts(L) .LT. TOL) THEN
          WRITE(MSG(1),100) YRDOY, L
          WRITE(MSG(2),"('SPiStaNoRts =',F10.3,' kg[P]/ha')") 
     &                    SPiStaNoRts(L)
          CALL WARNING(3, "PCHECK", MSG)
          SPiStaNoRts(L) = 0.0
        ENDIF
      ENDDO

      RETURN
      END SUBROUTINE PCHECK
!=======================================================================

!=======================================================================
!  SUBROUTINE P_ROOTS_INIT

!  Initialize inorganic P in root zone and non-root zone upon
!     emergence of plant or first hill or banded fertilzer
!     application.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  03/20/2007 CHP Moved out of SoilPi
!=======================================================================
      SUBROUTINE P_ROOTS_INIT(
     &  DLAYR, FracPSoln, FracRts, KG2PPM, NLAYR,         !Input
     &  SPi_Active, SPi_Labile, SPi_Stable, SW,           !Input
     &  FracNoRts, PiActNoRts, PiActRts, PiLabNoRts,      !Output
     &  PiLabRts, PiSolNoRts, PiSolRts, PiStaNoRts,       !Output
     &  PiStaRts, PPM2KG_Rts, PPM2KG_NoRts, SPi_Soluble,  !Output
     &  SPiActNoRts, SPiActRts, SPiLabNoRts, SPiLabRts,   !Output
     &  SPiSolNoRts, SPiSolRts, SPiStaNoRts, SPiStaRts)   !Output
     &                           
!-----------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE
      SAVE

      INTEGER L, NLAYR
      REAL, DIMENSION(NL) :: DLAYR, FracPSoln, FracRts, FracNoRts
      REAL, DIMENSION(NL) :: KG2PPM, PPM2KG_Rts, PPM2KG_NoRts
      REAL, DIMENSION(NL) :: PiLabRts, PiLabNoRts, PiSolRts, PiSolNoRts
      REAL, DIMENSION(NL) :: PiActRts, PiActNoRts, PiStaRts, PiStaNoRts
      REAL, DIMENSION(NL) :: SPi_Labile, SPiLabRts, SPiLabNoRts
      REAL, DIMENSION(NL) :: SPi_Active, SPiActRts, SPiActNoRts
      REAL, DIMENSION(NL) :: SPi_Stable, SPiStaRts, SPiStaNoRts
      REAL, DIMENSION(NL) :: SPi_Soluble, SPiSolRts, SPiSolNoRts, SW

!-----------------------------------------------------------------------
      DO L = 1, NLAYR
!       P in root zone:
        SPiLabRts(L) = SPi_Labile(L) * FracRts(L)
        SPiActRts(L) = SPi_Active(L) * FracRts(L)
        SPiStaRts(L) = SPi_Stable(L) * FracRts(L)

!       P in no-root zone:
        FracNoRts(L) = 1.0 - FracRts(L)
        SPiLabNoRts(L) = SPi_Labile(L) * FracNoRts(L)
        SPiActNoRts(L) = SPi_Active(L) * FracNoRts(L)
        SPiStaNoRts(L) = SPi_Stable(L) * FracNoRts(L)

!       Fraction of labile in solution
        SPiSolRts(L)   = SPiLabRts(L)   * FracPSoln(L)
        SPiSolNoRts(L) = SPiLabNoRts(L) * FracPSoln(L)
        SPi_Soluble(L) = SPiSolRts(L) + SPiSolNoRts(L)

!       Conversion from kg/ha to ppm:
        PPM2KG_Rts(L) = FracRts(L) / KG2PPM(L)
        IF (FracRts(L) > 1.E-6) THEN
          PiLabRts(L) = SPiLabRts(L) / PPM2KG_Rts(L)
          PiActRts(L) = SPiActRts(L) / PPM2KG_Rts(L)
          PiStaRts(L) = SPiStaRts(L) / PPM2KG_Rts(L)
!         Soluble P in mg[P]/l[water]
          PiSolRts(L) = SPiSolRts(L) / SW(L) / DLAYR(L) / FracRts(L)*10.
        ENDIF

        PPM2KG_NoRts(L) = FracNoRts(L) / KG2PPM(L)
        IF (FracNoRts(L) > 1.E-6) THEN
          PiLabNoRts(L) = SPiLabNoRts(L) / PPM2KG_NoRts(L)
          PiActNoRts(L) = SPiActNoRts(L) / PPM2KG_NoRts(L)
          PiStaNoRts(L) = SPiStaNoRts(L) / PPM2KG_NoRts(L)
!         Soluble P in mg[P]/l[water]
          PiSolNoRts(L) = SPiSolNoRts(L) / SW(L) / DLAYR(L) 
     &                / FracNoRts(L) * 10.
        ENDIF
      ENDDO

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE P_ROOTS_INIT
!=======================================================================


!=======================================================================
! SOILPI VARIABLE DEFINITIONS -- Updated 10/10/2005
!-----------------------------------------------------------------------
! ADDSPi(L)        Inorganic P fertilizer added today to soil layer L
!                   (kg[P]/ha)
! AddSPiActRts(L)  Storage variable for P fertilizer added to active P root 
!                    zone pool before growth of roots in soil layer L.
!                    (kg[P]/ha)
! AddSPiLabRts(L)  Storage variable for P fertilizer added to labile P root 
!                    zone pool before growth of roots in soil layer L
!                    (kg[P]/ha)
! Amt_Act2Lab      Quantity of transformation from active P to labile P
!                   (kg[P]/ha/d)
! Amt_Act2Sta      Quantity of transformation from active P to stable P
!                   (kg[P]/ha/d)
! Amt_Lab2Act      Quantity of transformation from labile P to active P
!                   (kg[P]/ha/d)
! Amt_Sta2Act      Quantity of transformation from stable P to active P
!                   (kg[P]/ha/d)
! BANDED           Logical variable to indicate whether current fertilizer 
!                    application is placed in bands or hills 
! CACO3(L)         Calcium carbonate in soil layer L (%)
! CIMMOBP          Seasonal cumulative P immobilized (kg[P]/ha)
! CLAY(L)          Percentage of clay in soil layer L 
! CMINERP          Seasonal cumulative P mineralized (kg[P]/ha)
! DLAYR(L)         Thickness of soil layer L (cm)
! DLTSPiActNoRts(L) Daily rate of increase of active P in non-root zone of 
!                    soil layer L (kg[P]/ha/d)
! DLTSPiActRts(L)   Daily rate of increase of active P in root zone of soil 
!                    layer L (kg[P]/ha/d)
! DLTSPiLabNoRts(L) Daily rate of increase of labile P in non-root zone of 
!                    soil layer L (kg[P]/ha/d)
! DLTSPiLabRts(L)   Daily rate of increase of labile P in root zone of soil 
!                    layer L (kg[P]/ha/d)
! DLTSPiStaNoRts(L) Daily rate of increase of stable P in non-root zone of 
!                    soil layer L (kg[P]/ha/d)
! DLTSPiStaRts(L)   Daily rate of increase of stable P in root zone of soil 
!                    layer L (kg[P]/ha/d)
! DLTSVRts(L)      Change in volume of soil in root zone (m3/m2)
! FERTDAY          Date of last fertilizer application (YYYYDDD)
! FIRST            Indicates first call to subroutine (true or false) 
! FerLabFracNoRts  Availability of fertilizer added to non-root zone, i.e., 
!                    fraction of fertilizer partitioned to labile P in 
!                    non-root zone
! FerLabFracRts    Availability of fertilizer added to non-root zone, i.e., 
!                    fraction of fertilizer partitioned to labile P in root 
!                    zone
! FracActNoRts     Fraction of fertilizer added to active pool of the 
!                    non-root zone
! FracActRts       Fraction of fertilizer added to active pool of the root 
!                    zone
! FracLabNoRts     Fraction of fertilizer added to labile pool of the 
!                    non-root zone
! FracLabRts       Fraction of fertilizer added to labile pool of the root 
!                    zone
! FracRts(L)       Fraction of soil volume that is in root zone in soil 
!                    layer L
! FracRtsY(L)      Fraction of volume of soil in direct contact with roots 
!                    in soil layer L yesterday
! FracNoRts(L)     Fraction of soil volume that is in no-root zone in soil 
!                    layer L
! IMM(L,IEL)       Immobilization of element IEL (1=N, 2=P) in soil layer L 
!                    for current day (kg[E]/ha/d)
! ISWPHO           Phosphorus simulation switch (Y or N) 
! K_ACT2LAB(L)     First order coefficient for transformation of active P 
!                    to labile P in soil layer L
! K_ACT2STA(L)     First order coefficient for transformation of active P 
!                    to stable P in soil layer L
! K_LAB2ACT(L)     First order coefficient for transformation of labile P 
!                    to active P in soil layer L
! K_STA2ACT(L)     First order coefficient for transformation of stable P 
!                    to active P in soil layer L
! KG2PPM(L)        Conversion factor to switch from kg [N] / ha to µg [N] / 
!                    g [soil] for soil layer L 
! MNR(L,IEL)       Mineralization of element IEL (1=N, 2=P) in soil layer L 
!                    for current day (kg[E]/ha/d)
! NLAYR            Actual number of soil layers 
! P_FERTINDEX      Function subroutine that calculates the fertilizer-P 
!                    availability index
! PAvailNoRts      Labile P in non-root soil zone which is available for 
!                    immobilization (kg[P]/ha)
! PAvailRts        Labile P in root soil zone which is available for 
!                    immobilization (kg[P]/ha)
! PERROR           Demand for P mineralization which cannot be met by 
!                    available P (kg[P]/ha)
! PFracRts         Fraction of labile P which is in the root zone (–)
! pH(L)            pH in soil layer L 
! PiActive(L)      Concentration of active P in soil layer L (ppm)
! PiActNoRts(L)    Concentration of active P in the non-root zone of soil 
!                    layer L (ppm)
! PiActRts(L)      Concentration of active P in the root growth zone of 
!                    soil layer L (ppm)
! PiLabile(L)      Concentration of labile P in soil layer L (ppm)
! PiLabNoRts(L)    Concentration of labile P in the non-root zone of soil 
!                    layer L (ppm)
! PiLabRts(L)      Concentration of labile P in the root growth zone of 
!                    soil layer L (ppm)
! PiStable(L)      Concentration of labile P in soil layer L (ppm)
! PiStaNoRts(L)    Concentration of labile P in the non-root zone of soil 
!                    layer L (ppm)
! PiStaRts(L)      Concentration of labile P in the root growth zone of 
!                    soil layer L (ppm)
! PMINERAL         Daily net P mineralization (mineralization minus 
!                    immobilization) (kg[P]/ha/d)
! PPM2KG_NoRts(L)  Conversion from concentration to mass in non-root soil 
!                    volume ((kg/ha)/ppm)
! PPM2KG_Rts(L)    Conversion from concentration to mass in root growth 
!                    soil volume ((kg/ha)/ppm)
! ProfAct2Lab      Transformation from active P to labile P over the soil 
!                    profile (kg[P]/ha/d)
! ProfAct2Sta      Transformation from active P to stable P over the soil 
!                    profile (kg[P]/ha/d)
! ProfLab2Act      Transformation from labile P to active P over the soil 
!                    profile (kg[P]/ha/d)
! ProfSta2Act      Transformation from stable P to active P over the soil 
!                    profile (kg[P]/ha/d)
! PTotAvail        Labile P which is available for immobilization
!                   (kg[P]/ha)
! PUPTAKE(L)       Plant uptake of P from soil layer L (kg[P]/ha/d)
! Rate_Act2Lab     Rate of transformation from active P to labile P (ppm/d)
! Rate_Act2Sta     Rate of transformation from active P to stable P (ppm/d)
! Rate_Lab2Act     Rate of transformation from labile P to active P (ppm/d)
! Rate_Sta2Act     Rate of transformation from stable P to active P (ppm/d)
! RNMODE           Simulation run mode (I=Interactive, A=All treatments, 
!                    B=Batch mode, E=Sensitivity, D=Debug, N=Seasonal, 
!                    Q=Sequence) 
! RUN              Change in date between two observations for linear 
!                    interpolation 
! SOILLAYERTYPE(L) General category of soil layer L as calcareous, slightly 
!                    weathered, highly weathered, or andisol
! SPi_Active(L)    Active P in soil layer L (kg[P]/ha)
! SPi_Avail(L)     Inorganic soil P which is available for plant uptake in 
!                    soil layer L (kg[P]/ha)
! SPi_Labile(L)    Labile soil P in soil layer L (kg[P]/ha)
! SPi_Stable(L)    Stable P in soil layer L (kg[P]/ha)
! SPiActNoRts(L)   Active P in non-root zone of soil layer L (kg[P]/ha)
! SPiActProf       Active P in soil profile (kg[P]/ha)
! SPiActRts(L)     Active P in root growth zone of soil layer L (kg[P]/ha)
! SPiAvlProf       P in soil profile which is available for uptake by roots
!                   (kg[P]/ha)
! SPiLabNoRts(L)   Labile P in non-root zone of soil layer L (kg[P]/ha)
! SPiLabProf       Labile P in soil profile (kg[P]/ha)
! SPiLabRts(L)     Labile P in root growth zone of soil layer L (kg[P]/ha)
! SPiStaNoRts(L)   Active P in non-root zone of soil layer L (kg[P]/ha)
! SPiStaProf       Active P in soil profile (kg[P]/ha)
! SPiStaRts(L)     Active P in root growth zone of soil layer L (kg[P]/ha)
! SPiTotProf       Total inorganic P in soil profile (kg[P]/ha)
! SumRtVol         Volume of soil in direct contact with roots in soil 
!                    profile (cm3/cm2)
! 
! TIMMOBP          Total immobilization of P on current day (kg[P]/ha/d)
! TMINERP          Total mineralization of P on current day (kg[P]/ha/d)
! TOTBAS(L)        Base saturation, soil layer L (cmol/kg)
! YRDOY            Current day of simulation (YYYYDDD)
!=======================================================================

