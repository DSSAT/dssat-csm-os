!=======================================================================
!  P_CASUPRO, Based on P_Ceres by CHP.  Translates CSP_CASUPRO variables 
!     into variables required by the generic plant phosphorus routine, 
!     P_PLANT.
!
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  09/23/2004 CHP Wrote predecessor routine, P_Ceres.
!  01/25/2005 CHP Added call to RootSoilVol in P_Ceres.
!  09/23/2006 FSR Adapted to CASUPRO sugarcane model.
!-----------------------------------------------------------------------
!  Called by: CSP_CASUPRO
!  Calls:     P_PLANT
!=======================================================================
      SUBROUTINE P_CASUPRO (CONTROL, DYNAMIC, ISWITCH,    !Input
     &    CROP, FILECC, GrowFrac, LFWTHa, MDATE,          !Input
     &    PLTPOP, RipeFrac, RLV, RTDEP, RTWTHa,           !Input
     &    SOILPROP,SPi_AVAIL, STKWTHa,                    !Input
     &    WLIDOT, WRIDOT, WSIDOT, YRPLT,                  !Input
     &    SENESCE,                                        !I/O
     &    PConc_Shut, PConc_Root, PConc_Seed,             !Output
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
      CHARACTER*9, PARAMETER :: ERRKEY = 'P_CASUPRO'
      CHARACTER*92 FILECC

      INTEGER DAS, DYNAMIC, L, MDATE, NLAYR, YRPLT

      REAL Leaf_kg, Stem_kg, Root_kg, Shel_kg, Seed_kg
      REAL PhFrac1, PhFrac2
!      REAL GROEAR, GROGRN, GROLF
!      REAL GRORT, GROSHL, GROSTM
      REAL PLTPOP !, MoveP
      REAL PStres1, PStres2
      REAL RTDEP 
      REAL GrowFrac, RipeFrac 
      REAL SenSurf, SenSurfP, SenSoilP    !, SenSoil
      REAL PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, PCNVEG
      REAL PShut_kg, PRoot_kg, PShel_kg, PSeed_kg
      REAL WLIDOT, WRIDOT, WSIDOT
      REAL PestShut, PestRoot, PestShel, PestSeed
      REAL ShutMob, RootMob, ShelMob

      REAL, DIMENSION(NL) :: DLAYR, DS, SPi_AVAIL     !, SPi_Labile
      REAL, DIMENSION(NL) :: PUptake, RLV, FracRts     !, RWU
      REAL, DIMENSION(0:NumOfDays) :: STKWTHa, LFWTHa, RTWTHa

      TYPE (ControlType) CONTROL
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

      DAS    = CONTROL % DAS
	 
      DLAYR  = SOILPROP % DLAYR
      DS     = SOILPROP % DS
      NLAYR  = SOILPROP % NLAYR

      ISWPHO = ISWITCH % ISWPHO
!***********************************************************************
      IF (DYNAMIC == SEASINIT) THEN


!       Soil phosphorus routine needs volume of soil adjacent to roots.
        CALL RootSoilVol(DYNAMIC, ISWPHO,    
     &    DLAYR, DS, NLAYR, PLTPOP, RLV, RTDEP, FILECC,   !Input
     &    FracRts)                                        !Output

!        Shut_kg = 0.0        
        Leaf_kg = 0.0
        Stem_kg = 0.0
        Root_kg = 0.0
        Shel_kg = 0.0
        Seed_kg = 0.0
!       Mass of mobilized plant tissue not used in CASUPRO: set to zero.
        ShutMob = 0.0
        RootMob = 0.0
        ShelMob = 0.0

        SENESCE % ResE = 0.0
        SenSoilP = 0.0
        SenSurfP = 0.0
        SENESCE%CumResE(P) = 0.0

        PestShut = 0.
        PestRoot = 0.
        PestShel = 0.
        PestSeed = 0. 


  
        CALL RootSoilVol(DYNAMIC, ISWPHO,    
     &    DLAYR, DS, NLAYR, PLTPOP, RLV, RTDEP, FILECC,   !Input
     &    FracRts)                                        !Output

      ELSE    !Not an initialization
        IF (ISWPHO == 'N') RETURN
      ENDIF

!***********************************************************************
      IF (DYNAMIC < OUTPUT) THEN
!     Convert variables to those needed by Generic Plant Phosphorus routine
        Leaf_kg = LFWTHa(DAS)
        Stem_kg = STKWTHa(DAS)
!        Shut_kg = Leaf_kg + Stem_kg
        Root_kg = RTWTHa(DAS)
      ENDIF

        IF (DYNAMIC .EQ. INTEGR) THEN
        CALL RootSoilVol(DYNAMIC, ISWPHO,    
     &    DLAYR, DS, NLAYR, PLTPOP, RLV, RTDEP, FILECC,   !Input
     &    FracRts)                                        !Output

        SenSurf = SENESCE%ResWt(0)      !SenWt(0)
        SenSurfP = SenSurf * PConc_Shut
        SENESCE % ResE(0,2) = SenSurfP

!         Calculate P senesced in roots 
!!!        SenSoil = 0.0
        SenSoilP = 0.0
        DO L = 1, NLAYR
!!!          SenSoil = SenSoil + SENESCE % ResWt(L) ! probably not necessary
          SENESCE % ResE(L,P) = SENESCE % ResWt(L) * PConc_Root
          SenSoilP = SenSoilP + SENESCE % ResE(L,P)   !(kg[P]/ha)
        ENDDO

          SENESCE%CumResE(P) = SENESCE%CumResE(P) + SenSurfP + SenSoilP 

          CALL RootSoilVol(DYNAMIC, ISWPHO,      
     &      DLAYR, DS, NLAYR, PLTPOP, RLV, RTDEP, FILECC, !Input
     &      FracRts)                                      !Output
!       Pest damage - convert from g/m2 to kg/ha
!         PestShut includes leaf and stem pest damage
        PestShut = (WLIDOT + WSIDOT) * 10.
        PestRoot = WRIDOT * 10.
        PestShel = 0.0
        PestSeed = 0.0 
      ENDIF

!     PhFrac1_2 is the fraction of physiological time which has occurred between
!       emergence and rapid stalk growth.
!     PhFrac2_3 is the fraction of physiological time which has occurred between
!       rapid stalk growth and physiological maturity. 
      PhFrac1 = GrowFrac
      PhFrac2 = RipeFrac
      
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

!!!      IF (DYNAMIC == FINAL) THEN
!!!        SENESCE % ResE(0,P) = CumSenSurfP
!       close(lun)
!!!      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE P_CASUPRO
!=======================================================================
