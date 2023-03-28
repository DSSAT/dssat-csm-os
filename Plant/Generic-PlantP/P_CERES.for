!=======================================================================
!  P_Ceres, Translates MZ_CERES variables into variables required by
!     the generic plant phosphorus routine, P_PLANT.
!
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  09/23/2004 CHP Written.
!  01/25/2005 CHP Added call to RootSoilVol
!  06/17/2006 AJG Renamed all root-zone and no-root-zone variables with
!                 Rts and noRts.
!-----------------------------------------------------------------------
!  Called by: MZ_CERES
!  Calls:     P_PLANT
!=======================================================================
      SUBROUTINE P_Ceres (DYNAMIC, ISWPHO,                !Input
     &    CumLeafSenes, DLAYR, DS, FILECC, MDATE, NLAYR,  !Input
     &    PCNVEG, PLTPOP, PODWT, RLV, RTDEP, RTWTO,       !Input
     &    SDWT, SWIDOT, SeedFrac, SPi_AVAIL, Stem2Ear,    !Input
     &    STMWTO, VegFrac, WLIDOT, WRIDOT, WSIDOT,        !Input
     &    WTLF, YRPLT,                                    !Input
     &    SENESCE,                                        !I/O
     &    PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, !Output
     &    PStres1, PStres2, PUptake, FracRts)             !Output

!     ------------------------------------------------------------------
!     Send DYNAMIC to accomodate emergence calculations
!       (DYNAMIC=EMERG) within the integration section of CERES (MA 19dec2013)
!     ------------------------------------------------------------------
      USE ModuleDefs
      USE ModuleData
      IMPLICIT  NONE
      EXTERNAL P_Plant
      SAVE
!     ------------------------------------------------------------------

      CHARACTER*1 ISWPHO
      CHARACTER*2 CROP 
      CHARACTER*6, PARAMETER :: ERRKEY = 'PCERES'
      CHARACTER*92 FILECC

      INTEGER DYNAMIC, L, MDATE, NLAYR, YRPLT
      INTEGER, PARAMETER :: SRFC = 0

      REAL Shut_kg, Leaf_kg, Stem_kg, Root_kg, Shel_kg, Seed_kg
      REAL PhFrac1, PhFrac2
      REAL PLTPOP, PODWT, MoveP
      REAL PStres1, PStres2
      REAL RTDEP, RTWTO, SDWT, SHELWT
      REAL SeedFrac, VegFrac, STMWTO, WTLF
      REAL CumSenSurfP, CumLeafSenes, CumLeafSenesY
      REAL SenSurf, SenSurfP, SenSoil, SenSoilP
      REAL PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, PCNVEG
      REAL PShut_kg, PRoot_kg, PShel_kg, PSeed_kg
      REAL Stem2Ear
      REAL SWIDOT, WLIDOT, WRIDOT, WSIDOT
      REAL PestShut, PestRoot, PestShel, PestSeed
      REAL ShutMob, RootMob, ShelMob

      REAL, DIMENSION(NL) :: DLAYR, DS, SPi_AVAIL   !, SPi_Labile
      REAL, DIMENSION(NL) :: PUptake, RLV, FracRts    !, RWU

      TYPE (ResidueType) SENESCE
      TYPE (ControlType) CONTROL

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
          IMPLICIT NONE
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

!***********************************************************************
!***********************************************************************
      IF (DYNAMIC == SEASINIT) THEN

      CALL GET(CONTROL)
      CROP = CONTROL % CROP

!       Soil phosphorus routine needs volume of soil adjacent to roots.
        CALL RootSoilVol(DYNAMIC, ISWPHO,    
     &    DLAYR, DS, NLAYR, PLTPOP, RLV, RTDEP, FILECC,   !Input
     &    FracRts)                                        !Output

        Shut_kg = 0.0        
        Leaf_kg = 0.0
        Stem_kg = 0.0
        Root_kg = 0.0
        Shel_kg = 0.0
        Seed_kg = 0.0

        SENESCE % ResE = 0.0
        SenSoilP = 0.0
        SenSurfP = 0.0
        CumSenSurfP = 0.0

        PestShut = 0.
        PestRoot = 0.
        PestShel = 0.
        PestSeed = 0. 

!       Mass of mobilized plant tissue -- not used in Ceres, set to zero.
        ShutMob = 0.0
        RootMob = 0.0
        ShelMob = 0.0

!       Yesterdays cumulative leaf senescence
        CumLeafSenesY = 0.0
  
      ELSE    !Not an initialization
        IF (ISWPHO == 'N') RETURN
      ENDIF

      IF (DYNAMIC < OUTPUT) THEN
!       Convert units for Generic Plant Phosphorus routine to kg/ha
        SHELWT = PODWT - SDWT               !g/m2

        Leaf_kg = WTLF * 10.
        Stem_kg = STMWTO * 10.
        Shut_kg = Leaf_kg + Stem_kg
        Root_kg = RTWTO * 10.
        Shel_kg = SHELWT * 10.
        Seed_kg = SDWT * 10.

        IF (Stem2Ear > 1.E-6) THEN
!         First growth of ears -- mass was transferred from stem
!         Need to transfer P
          MoveP = Stem2Ear * 10. * PConc_Shut   !kg[P]/ha
          PShut_kg = PShut_kg - MoveP
          PShel_kg = PShel_kg + MoveP * (Shel_kg/(Stem2Ear*10))
          PSeed_kg = PSeed_kg + MoveP * (Seed_kg/(Stem2Ear*10))
          Stem2Ear = 0.0
        ENDIF

      ENDIF

      IF (DYNAMIC == INTEGR) THEN
        CALL RootSoilVol(DYNAMIC, ISWPHO,    
     &    DLAYR, DS, NLAYR, PLTPOP, RLV, RTDEP, FILECC,   !Input
     &    FracRts)                                        !Output

!       Add in daily senesced leaf mass, which has not dropped from plant
!       and therefore was not added to SENESCE variable
        SenSurf = CumLeafSenes - CumLeafSenesY       !kg [dry matter]/ha
        SenSurfP = (CumLeafSenes - CumLeafSenesY) * PConc_Shut !kg[P]/ha
        CumSenSurfP = CumSenSurfP + SenSurfP

!       Save cumulative leaf senescence for use tomorrow
        CumLeafSenesY = CumLeafSenes 

!       Senesced matter to soil (kg[P]/ha) - already added to SENESCE
        SenSoil = 0.0
        SenSoilP = 0.0
        DO L = 1, NLAYR
          SenSoil = SenSoil + SENESCE % ResWt(L)
          SENESCE % ResE(L,P) = SENESCE % ResWt(L) * PConc_Root
          SenSoilP = SenSoilP + SENESCE % ResE(L,P)   !kg/ha
        ENDDO
        SENESCE % CUMRESE(P) = SENESCE % CUMRESE(P) + SenSoilP

!       Pest damage - convert from g/m2 to kg/ha
        PestShut = (WLIDOT + WSIDOT) * 10.
        PestRoot = WRIDOT * 10.
        PestShel = 0.0
        PestSeed = SWIDOT * 10. 
      ENDIF

!     PhFrac1_2 is the fraction of physiological time which has occurred between
!       emergence and first tassel.
!     PhFrac2_3 is the fraction of physiological time which has occurred between
!       first tassel and physiological maturity. 
      PhFrac1 = VegFrac
      PhFrac2 = SeedFrac
      
      CALL P_Plant (DYNAMIC, ISWPHO,                      !I Control
     &    CROP, FILECC, MDATE, YRPLT,                     !I Crop
     &    SPi_AVAIL,                                      !I Soils
     &    Leaf_kg, Stem_kg, Root_kg, Shel_kg, Seed_kg,    !I Mass
     &    PhFrac1, PhFrac2,                               !I Phase
     &    RLV,                           !RWU,            !I Roots
     &    SenSoilP, SenSurfP,                             !I Senescence
     &    PCNVEG,                                         !I N conc.
     &    PestShut, PestRoot, PestShel, PestSeed,         !I Pest damage
     &    ShutMob, RootMob, ShelMob,                      !I Mobilized 
     &    PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, !O P conc.
     &    PShut_kg, PRoot_kg, PShel_kg, PSeed_kg,         !O P amts.
     &    PStres1, PStres2,                               !O P stress
     &    PUptake)                                        !O P uptake

      IF (DYNAMIC == SEASEND) THEN
        SENESCE % ResE(SRFC,P) = CumSenSurfP
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE P_Ceres
C=======================================================================
