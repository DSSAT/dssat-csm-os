!=======================================================================
!  P_CGRO, Translates CROPGRO variables into variables required by
!     the generic plant phosphorus routine, P_PLANT.
!
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  09/20/2004 CHP Written.
!  01/25/2005 CHP Added RootSoilVol for P-uptake calcs.
!-----------------------------------------------------------------------
!  Called by: CROPGRO
!  Calls:     P_PLANT
!=======================================================================
      SUBROUTINE P_CGRO (DYNAMIC, ISWITCH, 
     &    CROP, FILECC, MDATE, PCNVEG, PLTPOP, RLV,       !Input
     &    RootMob, RTDEP, RTWT, SDWT, SeedFrac,           !Input
     &    ShelMob, SHELWT, ShutMob, SOILPROP,             !Input
     &    SPi_AVAIL, STMWT, SWIDOT, VegFrac, WLIDOT,      !Input
     &    WRIDOT, WSHIDT, WSIDOT, WTLF, YRPLT,            !Input
     &    SENESCE,                                        !I/O
     &    PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, !Output
     &    PStres1, PStres2, PUptake, FracRts)             !Output

!     ------------------------------------------------------------------
!     Send DYNAMIC to accomodate emergence calculations
!       (DYNAMIC=EMERG) within the integration section of CROPGRO.
!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
      IMPLICIT  NONE
      EXTERNAL P_PLANT
      SAVE
!     ------------------------------------------------------------------

      CHARACTER*1 ISWPHO
      CHARACTER*2 CROP
      CHARACTER*6, PARAMETER :: ERRKEY = 'P_CGRO'
      CHARACTER*92 FILECC

      INTEGER DYNAMIC, L, MDATE, NLAYR, YRPLT

      REAL PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed
      REAL PShut_kg, PRoot_kg, PShel_kg, PSeed_kg
      REAL PhFrac1, PhFrac2
      REAL PStres1, PStres2, SeedFrac, VegFrac
      REAL WTLF, STMWT, RTWT, SHELWT, SDWT
      REAL Leaf_kg, Stem_kg, Root_kg, Shel_kg, Seed_kg
      REAL ShutMob, ShelMob, RootMob
      REAL SenSurf, SenSurfP, SenSoilP
      REAL PCNVEG
      REAL PestShut, PestRoot, PestShel, PestSeed
      REAL WLIDOT, WSIDOT, WRIDOT, WSHIDT, SWIDOT

!     For RootSoilVol:
      REAL PLTPOP, RTDEP, FracRts(NL)

      REAL, DIMENSION(NL) :: DLAYR, DS, SPi_AVAIL
      REAL, DIMENSION(NL) :: PUptake, RLV     

      TYPE (SwitchType)  ISWITCH
      TYPE (SoilType)    SOILPROP
      TYPE (ResidueType) SENESCE

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
!-----------------------------------------------------------------------

      DLAYR  = SOILPROP % DLAYR
      DS     = SOILPROP % DS
      NLAYR  = SOILPROP % NLAYR

      ISWPHO = ISWITCH % ISWPHO

!***********************************************************************
      IF (DYNAMIC == SEASINIT) THEN
        IF (CROP /= 'FA') THEN
!         Soil phosphorus routine needs volume of soil adjacent to roots.
          CALL RootSoilVol(DYNAMIC, ISWPHO,    
     &    DLAYR, DS, NLAYR, PLTPOP, RLV, RTDEP, FILECC,   !Input
     &    FracRts)                                        !Output
        ELSE
          FracRts = 0.0
        ENDIF

        Leaf_kg = 0.0
        Stem_kg = 0.0
        Root_kg = 0.0
        Shel_kg = 0.0
        Seed_kg = 0.0

        SENESCE % ResE = 0.0
        SenSoilP = 0.0
        SenSurfP = 0.0
        SENESCE%CumResE(P) = 0.0

        PestShut = 0.
        PestRoot = 0.
        PestShel = 0.
        PestSeed = 0.   

      ELSE    !Not an initialization
        IF (ISWPHO == 'N') RETURN
      ENDIF

!***********************************************************************
      IF (CROP .NE. 'FA') THEN
!       Convert units for Generic Plant Phosphorus routine
!        Shut_kg = -99.
        Leaf_kg = WTLF * 10.
        Stem_kg = STMWT * 10.
        Root_kg = RTWT * 10.
        Shel_kg = SHELWT * 10.
        Seed_kg = SDWT * 10.

        IF (DYNAMIC .EQ. INTEGR) THEN
!         Calculate P senesced in shoots
!         This routine is called prior to senescence calcultion in GROW
!           subroutine.  Need to recompute yesterday's senescence from
!           the cumulative variable.
          SenSurf = SENESCE%ResWt(0)      !SenWt(0)
          SenSurfP = SenSurf * PConc_Shut
          SENESCE % ResE(0,2) = SenSurfP

!         Calculate P senesced in roots 
          SenSoilP = 0.0
          DO L = 1, NLAYR
            SENESCE % ResE(L,2) = SENESCE%ResWt(L) * PConc_Root
            SenSoilP = SenSoilP + SENESCE % ResE(L,2)   !kg/ha
          ENDDO
          SENESCE%CumResE(P) = SENESCE%CumResE(P) + SenSurfP + SenSoilP 

          CALL RootSoilVol(DYNAMIC, ISWPHO,      
     &      DLAYR, DS, NLAYR, PLTPOP, RLV, RTDEP, FILECC, !Input
     &      FracRts)                                      !Output

!         Pest damage - convert from g/m2 to kg/ha
!         PestShut includes leaf and stem pest damage
          PestShut = (WLIDOT + WSIDOT) * 10.
          PestRoot = WRIDOT * 10.
          PestShel = WSHIDT * 10.
          PestSeed = SWIDOT * 10.  
         ENDIF

!       PhFrac1_2 is the fraction of physiological time which has 
!         occurred between emergence and first seed.
!       PhFrac2_3 is the fraction of physiological time which has 
!         occurred between first seed and physiological maturity. 
        PhFrac1 = VegFrac
        PhFrac2 = SeedFrac
      ENDIF
      
      CALL P_PLANT(DYNAMIC, ISWPHO,                       !I Control
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

!***********************************************************************
      RETURN
      END SUBROUTINE P_CGRO
C=======================================================================

