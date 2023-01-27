!=======================================================================
!  P_Plant, Plant phosphorus model.
!
!  DETERMINES PLANT P TRANSFORMATIONS
!  Computes P concentration and total content for four plant 
!    components: shoot (leaf + stem), root, shell and seed.  
!  Optimum P concentration for each component is based on 3
!    stages:
!    (1) stage A (e.g., emergence)
!    (2) stage B (e.g., first flower)
!    (3) stage C (e.g., physiological maturity)
!  Optimum and minimum P concentrations for each plant component / 
!    stage are read from the species file.
!  Fraction of development between stages is sent from each plant
!    routine.  This routine linearly interpolates optimum and 
!    minimum P concentrations between stages.
!  Two options to read in P concentrations for either Shoot or for  
!    Leaf and Stem, as well as for root, shell and seed. Use
!    -99 for data option not used. Logical variable, UseShoots, is
!    set to TRUE when values of shoots are used, FALSE if leaf and stem
!    are used.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  08/../1997 AG  Written.
!  09/../1997 AG  Built into CERES 3.1.
!  09/../1998 AG  Built into CERES 3.5.
!  01/../1999 AG  Replaced ISTAGE with XSTAGE in wh.
!  06/../1999 AG  Added residue P carry-over.
!  08/../1999 AG  Added seasonal analysis initalization option.
!  09/19/2003 AJG Brought into modular format and linked to the 
!                 CENTURY-based SOM/residue module.
!  12/17/2003 AJG Renamed all the CH_ and CHEM_ variables to P_ variables.
!  02/12/2004 AJG Renamed the subroutine from CHEM_GEN to P_GEN.
!  03/23/2004 CHP Removed soil components
!  03/24/2004 CHP Added to CSM
!  09/17/2004 CHP Made generic for all plant routines to use a single routine.
!  01/26/2023 CHP Reduce compile warnings: add EXTERNAL stmts, remove 
!                 unused variables, shorten lines. 
!-----------------------------------------------------------------------
!  Called by: CROPGRO, MZ_CERES, RI_CERES...
!  Calls:     P_UPTAKE, OPPHOS
!=======================================================================
      SUBROUTINE P_Plant (DYNAMIC, ISWPHO,                !I Control
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

!     ------------------------------------------------------------------
!     Send DYNAMIC to accomodate emergence calculations
!       (DYNAMIC=EMERG) within the integration section of CROPGRO.
!     ------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT  NONE
      EXTERNAL P_IPPLNT, P_Demand, P_Uptake, OPPlantP, P_Partition, 
     &  PValue
      SAVE
!     ------------------------------------------------------------------
!     Interface variables
!     ------------------------------------------------------------------
!     INPUT
      INTEGER      DYNAMIC        !Processing control
      CHARACTER*1  ISWPHO         !P switch (Y or N)
      CHARACTER*2  CROP           !2-character crop code
      CHARACTER*92 FILECC         !Path and name of species file
      INTEGER      MDATE          !Maturity date
      INTEGER      YRPLT          !Planting date

      REAL         SPi_AVAIL(NL)  !P available for uptake (kg/ha)

      !State variables - mass (kg/ha) - includes new growth
      REAL         Leaf_kg        !Leaf mass (kg/ha)
      REAL         Stem_kg        !Stem mass (kg/ha)
      REAL         Root_kg        !Root mass (kg/ha)
      REAL         Shel_kg        !Shell mass (kg/ha)
      REAL         Seed_kg        !Seed mass (kg/ha)

      REAL         PhFrac1      !Fraction phys time betw St 1 & St 2
      REAL         PhFrac2      !Fraction phys time betw St 2 & St 3
      REAL         RLV(NL)        !Root length density
      REAL         SenSoilP       !P senesced from leaf + stem
      REAL         SenSurfP       !P senesced from roots
      REAL         PCNVEG         !N percentage in veg. tissue
      !Pest damage variables:
      REAL         PestShut       !Pest damage to leaf and stem
      REAL         PestRoot       !Pest damage to root
      REAL         PestShel       !Pest damage to shell
      REAL         PestSeed       !Pest damage to seed
      !Plant mass lost due to mobilization of N and C
      REAL         ShutMob        !Mobilization loss for leaf and stem
      REAL         RootMob        !Mobilization loss for roots
      REAL         ShelMob        !Mobilization loss for shells
    
!     OUTPUT
      REAL         PConc_Shut     !P conc in shoots (kg[P]/kg[shoots])
      REAL         PConc_Root     !P conc in roots (kg[P]/kg[roots])
      REAL         PConc_Veg      !P conc in veg tissue (kg[P]/kg[veg])
      REAL         PStres1        !P stress for partitioning
      REAL         PStres2        !P stress for photosynthesis
      REAL         PUptake(NL)    !P uptake by soil layer (kg/ha)

!     ------------------------------------------------------------------
      CHARACTER*6, PARAMETER :: ERRKEY = 'PPLANT'
      LOGICAL UseShoots
      INTEGER I

!     Shoot mass -- leaf + stem (kg/ha)
      REAL Shut_kg    !, ShutNew_kg

!     Change (increase or decrease) to P variable per day
      REAL DeltPShut, DeltPRoot, DeltPShel, DeltPSeed

!     Daily values of optimum and minimum and actual P concs.
      REAL PConc_Shut_opt, PConc_Root_opt, PConc_Shel_opt,PConc_Seed_opt
      REAL PConc_Shut_min, PConc_Root_min, PConc_Shel_min,PConc_Seed_min
      REAL PConc_Shel, PConc_Seed, PConc_Plant

!     P state variables by weight (kg/ha)
      REAL PShut_kg, PRoot_kg, PShel_kg, PSeed_kg, PPlant_kg

!     Misc.
      REAL PShutDem, PRootDem, PShelDem, PSeedDem, PTotDem
      REAL PSTRESS_RATIO
      REAL PUptakeProf
      REAL Plant_kg
      REAL N2P, N2P_max, N2P_min
      REAL PestShutP, PestRootP, PestShelP, PestSeedP
!      REAL PShutMob, PRootMob, PShelMob   !, PTotMob
!      REAL PShutMobPool, PRootMobPool, PShelMobPool 

!     From species file:
      REAL, DIMENSION(3) :: PCShutOpt, PCRootOpt, PCShelOpt, PCSeedOpt
      REAL, DIMENSION(3) :: PCLeafOpt, PCStemOpt
      REAL, DIMENSION(3) :: PCShutMin, PCRootMin, PCShelMin, PCSeedMin
      REAL, DIMENSION(3) :: PCLeafMin, PCStemMin
      REAL, DIMENSION(3) :: N2Pmin, N2Pmax
      REAL SRATPHOTO, SRATPART
      REAL FracPMobil, FracPUptake

      Real PValue !Function

!***********************************************************************
!***********************************************************************
!     SEASONAL INITIALIZATION: RUN ONCE PER SEASON.
!***********************************************************************
      IF (DYNAMIC == SEASINIT .OR. DYNAMIC .EQ. RUNINIT) THEN
!     ------------------------------------------------------------------
      IF (CROP .NE. 'FA' .AND. 
     &   (ISWPHO .EQ. 'Y' .OR. ISWPHO .EQ. 'H')) THEN
        CALL P_IPPLNT(FILECC,
     & N2Pmax, N2Pmin,
     & PCShutMin, PCLeafMin, PCStemMin, PCRootMin, PCShelMin, PCSeedMin,
     & PCShutOpt, PCLeafOpt, PCStemOpt, PCRootOpt, PCShelOpt, PCSeedOpt,
     & FracPMobil, FracPUptake, SRATPHOTO, SRATPART, UseShoots)
      ENDIF

!     ------------------------------------------------------------------
      Call P_Demand(DYNAMIC,
     &    PConc_Root, PConc_Root_min, PConc_Root_opt,     !Input
     &    PConc_Shel, PConc_Shel_min, PConc_Shel_opt,     !Input 
     &    PConc_Shut, PConc_Shut_min, PConc_Shut_opt,     !Input
     &    PConc_Seed_opt, PRoot_kg, PSeed_kg, PShel_kg,   !Input
     &    PShut_kg, Root_kg, RootMob, Seed_kg, Shel_kg,   !Input
     &    ShelMob, Shut_kg, ShutMob,                      !Input
     &    DeltPRoot, DeltPSeed, DeltPShel, DeltPShut,     !I/O
     &    PRootDem, PSeedDem, PShelDem, PShutDem,         !Output
     &    PTotDem)                                        !Output

!     ------------------------------------------------------------------
!     Initialize uptake variables - do this even if P not modelled.
      CALL P_Uptake (DYNAMIC,  
     &    N2P_min, PCNVeg, PConc_Veg, PTotDem,            !Input
     &    RLV, SPi_AVAIL,                                 !Input
     &    N2P, PUptake, PUptakeProf)                      !Output

!     Optimum P concentration
      PConc_Shut_opt = 0.0
      PConc_Root_opt = 0.0
      PConc_Shel_opt = 0.0    
      PConc_Seed_opt = 0.0

!     Minimum P concentration
      PConc_Shut_min = 0.0
      PConc_Root_min = 0.0
      PConc_Shel_min = 0.0    
      PConc_Seed_min = 0.0

!     P concentration variables (fraction of plant tissue)
      PConc_Shut  = 0.0
      PConc_Root  = 0.0
      PConc_Shel  = 0.0
      PConc_Seed  = 0.0
      PConc_Plant = 0.0

!     P[kg/ha] variables
      PShut_kg  = 0.0
      PRoot_kg  = 0.0
      PShel_kg  = 0.0
      PSeed_kg  = 0.0
      PPlant_kg = 0.0

!     Demand variables
      PShutDem = 0.0
      PRootDem = 0.0
      PShelDem = 0.0
      PSeedDem = 0.0
      PTotDem  = 0.0

      PStres1 = 1.0   
      PStres2 = 1.0   
      PSTRESS_RATIO = 1.0

!     P in senesced tissue
      SenSoilP = 0.0
      SenSurfP = 0.0

!     N:P ratio
      N2P = 0.0

!     Initial shoot mass 
      Shut_kg = 0.0

      IF (ISWPHO == 'N') RETURN

      CALL OPPlantP(DYNAMIC, MDATE, YRPLT, 
     &  PConc_Shut_opt, PConc_Root_opt, PConc_Shel_opt, PConc_Seed_opt,
     &  PConc_Shut_min, PConc_Root_min, PConc_Shel_min, PConc_Seed_min,
     &  PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, PConc_Plant,
     &  PShut_kg, PRoot_kg, PShel_kg, PSeed_kg, PPlant_kg,  
     &  Shut_kg, Root_kg, Shel_kg, Seed_kg, N2P, PTotDem,  
     &  SenSoilP, SenSurfP, PhFrac1, PhFrac2, 
     &  PStres1, PStres2, PSTRESS_RATIO, PUptakeProf,
     &  PestShutP, PestRootP, PestShelP, PestSeedP)

!***********************************************************************
!***********************************************************************
!     EMERGENCE
!***********************************************************************
      ELSEIF (DYNAMIC == EMERG) THEN
!-----------------------------------------------------------------------
      PTotDem = 0.0
      Shut_kg = Leaf_kg + Stem_kg

!     Optimum P concentration in vegetative matter at emergence (fraction)
      IF (UseShoots) THEN
        PConc_Shut_opt = PCShutOpt(1)
      ELSE
        PConc_Shut_opt = (PCLeafOpt(1)*Leaf_kg + PCStemOpt(1)*Stem_kg) /
     &                        Shut_kg
      ENDIF
      PConc_Root_opt = PCRootOpt(1)
      PConc_Shel_opt = PCShelOpt(1)    
      PConc_Seed_opt = PCSeedOpt(1)

!     Initial P concentration in vegetative matter at emergence (fraction)
      PConc_Shut = PConc_Shut_opt 
      PConc_Root = PConc_Root_opt 
      PConc_Shel = PConc_Shel_opt    
      PConc_Seed = PConc_Seed_opt 

!     Total plant weight
      Plant_kg = Shut_kg + Root_kg + Shel_kg + Seed_kg
!     What about nodules, tubers, etc.???

!     Plant P (kg/ha)
      PShut_kg = PConc_Shut * Shut_kg
      PRoot_kg = PConc_Root * Root_kg
      PShel_kg = PConc_Shel * Shel_kg
      PSeed_kg = PConc_Seed * Seed_kg
      PPlant_kg = PShut_kg + PRoot_kg + PShel_kg + PSeed_kg

      CALL OPPlantP(DYNAMIC, MDATE, YRPLT, 
     &  PConc_Shut_opt, PConc_Root_opt, PConc_Shel_opt, PConc_Seed_opt,
     &  PConc_Shut_min, PConc_Root_min, PConc_Shel_min, PConc_Seed_min,
     &  PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, PConc_Plant,
     &  PShut_kg, PRoot_kg, PShel_kg, PSeed_kg, PPlant_kg,  
     &  Shut_kg, Root_kg, Shel_kg, Seed_kg, N2P, PTotDem,  
     &  SenSoilP, SenSurfP, PhFrac1, PhFrac2, 
     &  PStres1, PStres2, PSTRESS_RATIO, PUptakeProf,
     &  PestShutP, PestRootP, PestShelP, PestSeedP)

!***********************************************************************
!***********************************************************************
!     DAILY INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC == INTEGR) THEN
!-----------------------------------------------------------------------
      Shut_kg = Leaf_kg + Stem_kg
      Plant_kg = Shut_kg + Root_kg + Shel_kg + Seed_kg

      IF (.NOT. UseShoots .AND. Shut_kg > 0) THEN
        DO I = 1, 3
          PCShutOpt(I) = (PCLeafOpt(I)*Leaf_kg + PCStemOpt(I)*Stem_kg) /
     &                                    Shut_kg
          PCShutMin(I) = (PCLeafMin(I)*Leaf_kg + PCStemMin(I)*Stem_kg) /
     &                                    Shut_kg
        ENDDO
      ENDIF

!-----------------------------------------------------------------------
!     Initialize delta variables
      DeltPShut = 0.0
      DeltPRoot = 0.0
      DeltPShel = 0.0
      DeltPSeed = 0.0

!-----------------------------------------------------------------------
!     Senesced plant matter - reduce P
      PShut_kg  = PShut_kg - SenSurfP  
      PRoot_kg  = PRoot_kg - SenSoilP   
      PPlant_kg = PPlant_kg - (SenSoilP + SenSurfP) 

!     Pest damage
      PestShutP = PestShut * PConc_Shut
      PestRootP = PestRoot * PConc_Root
      PestShelP = PestShel * PConc_Shel
      PestSeedP = PestSeed * PConc_Seed

      PShut_kg  = PShut_kg - PestShutP 
      PRoot_kg  = PRoot_kg - PestRootP  
      PShel_kg  = PShel_kg - PestShelP
      PSeed_kg  = PSeed_kg - PestSeedP

      PPlant_kg = PPlant_kg - 
     &        (PestShutP + PestRootP + PestShelP + PestSeedP)

!     ------------------------------------------------------------------
!     Calculate optimum and minimum P concentrations in plant tissue.
!     ------------------------------------------------------------------
      PConc_Shut_opt =  PValue(PhFrac1, PhFrac2, PCShutOpt)
      PConc_Root_opt =  PValue(PhFrac1, PhFrac2, PCRootOpt)
      PConc_Shel_opt =  PValue(PhFrac1, PhFrac2, PCShelOpt)
      PConc_Seed_opt =  PValue(PhFrac1, PhFrac2, PCSeedOpt)

      PConc_Shut_min =  PValue(PhFrac1, PhFrac2, PCShutMin)
      PConc_Root_min =  PValue(PhFrac1, PhFrac2, PCRootMin)
      PConc_Shel_min =  PValue(PhFrac1, PhFrac2, PCShelMin)
      PConc_Seed_min =  PValue(PhFrac1, PhFrac2, PCSeedMin)

      N2P_max =  PValue(PhFrac1, PhFrac2, N2Pmax)
      N2P_min =  PValue(PhFrac1, PhFrac2, N2Pmin)

!-----------------------------------------------------------------------
!     CALCULATE DEMANDS in kg/ha
      Call P_Demand(DYNAMIC,
     &    PConc_Root, PConc_Root_min, PConc_Root_opt,     !Input
     &    PConc_Shel, PConc_Shel_min, PConc_Shel_opt,     !Input 
     &    PConc_Shut, PConc_Shut_min, PConc_Shut_opt,     !Input
     &    PConc_Seed_opt, PRoot_kg, PSeed_kg, PShel_kg,   !Input
     &    PShut_kg, Root_kg, RootMob, Seed_kg, Shel_kg,   !Input
     &    ShelMob, Shut_kg, ShutMob,                      !Input
     &    DeltPRoot, DeltPSeed, DeltPShel, DeltPShut,     !I/O
     &    PRootDem, PSeedDem, PShelDem, PShutDem,         !Output
     &    PTotDem)                                        !Output

!-----------------------------------------------------------------------
!     P uptake
      CALL P_Uptake (DYNAMIC,   
     &    N2P_min, PCNVeg, PConc_Veg, PTotDem,            !Input
     &    RLV, SPi_AVAIL,                                 !Input
     &    N2P, PUptake, PUptakeProf)                      !Output

!-----------------------------------------------------------------------
      CALL P_Partition(
     &    FracPMobil, PConc_Root_min, PConc_Shel_min,     !Input
     &    PConc_Shut_min, PRootDem, PRoot_kg, PSeedDem,   !Input
     &    PShelDem, PShel_kg, PShutDem, PShut_kg,         !Input
     &    PUptakeProf, Root_kg, Shel_kg, Shut_kg,         !Input
     &    DeltPRoot, DeltPSeed, DeltPShel, DeltPShut)     !I/O

!------------------------------------------------------------------------
!     P mass in plant matter (kg/ha)
      PShut_kg  = PShut_kg + DeltPShut 
      PRoot_kg  = PRoot_kg + DeltPRoot 
      PShel_kg  = PShel_kg + DeltPShel   
      PSeed_kg  = PSeed_kg + DeltPSeed    
      PPlant_kg = PPlant_kg + PUptakeProf
      
!------------------------------------------------------------------------

C     CALCULATE P CONCENTRATIONS (fractions)
      IF (Seed_kg > 0.) THEN      
        PConc_Seed = PSeed_kg / Seed_kg 
      ELSE
        PConc_Seed = 0.
      ENDIF

      IF (Shel_kg > 0.) THEN
        PConc_Shel = PShel_kg / Shel_kg
      ELSE
        PConc_Shel = 0.
      ENDIF
C MA problem of transfer of shoot ( stems) to panicle at grain filling      
      IF (Shut_kg > 0.) THEN      
        PConc_Shut = PShut_kg / Shut_kg 
      ELSE
        PConc_Shut = 0.
      ENDIF

      IF (Root_kg > 0.) THEN
        PConc_Root = PRoot_kg / Root_kg
      ELSE
        PConc_Root = 0.
      ENDIF
      
      IF (Plant_kg > 0.) THEN
        PConc_Plant = PPlant_kg / Plant_kg
      ELSE
        PConc_Plant = 0.0
      ENDIF

!     Vegetative P concentration for N:P ratio
      IF (Shut_kg + Root_kg > 1.E-6) THEN
        PConc_Veg = (PConc_Shut * Shut_kg + PConc_Root * Root_kg) / 
     &         (Shut_kg + Root_kg) * 100.
      ENDIF

!-----------------------------------------------------------------------
!     From MB's P_Partition
C     Calculate PSTRESS_RATIO
        PSTRESS_RATIO = MIN(1.0, (PConc_Shut - PConc_Shut_Min) /  
     &                       (PConc_Shut_opt - PConc_Shut_Min))

C     Calculate PStres1 (Photosynthesis)
      IF (PSTRESS_RATIO .GE. SRATPHOTO) THEN
        PStres1 = 1.0
      ELSEIF (PSTRESS_RATIO < SRATPHOTO .AND. PSTRESS_RATIO > 1.E-6)THEN
        PStres1 = PSTRESS_RATIO / SRATPHOTO
      ELSE
        PStres1 = 0.0
      ENDIF   

C     Calculate PStres2 (Partitioning)
      IF (PSTRESS_RATIO .GE. SRATPART) THEN
        PStres2 = 1.0
      ELSEIF (PSTRESS_RATIO < SRATPART .AND. PSTRESS_RATIO > 1.E-6) THEN
        PStres2 = PSTRESS_RATIO / SRATPART
      ELSE
        PStres2 = 0.0
      ENDIF

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT AND SEASONAL SUMMARY
!***********************************************************************
      ELSEIF (DYNAMIC == OUTPUT .OR. DYNAMIC == SEASEND) THEN
!-----------------------------------------------------------------------
      CALL OPPlantP(DYNAMIC, MDATE, YRPLT, 
     &  PConc_Shut_opt, PConc_Root_opt, PConc_Shel_opt, PConc_Seed_opt,
     &  PConc_Shut_min, PConc_Root_min, PConc_Shel_min, PConc_Seed_min,
     &  PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, PConc_Plant,
     &  PShut_kg, PRoot_kg, PShel_kg, PSeed_kg, PPlant_kg,  
     &  Shut_kg, Root_kg, Shel_kg, Seed_kg, N2P, PTotDem,  
     &  SenSoilP, SenSurfP, PhFrac1, PhFrac2, 
     &  PStres1, PStres2, PSTRESS_RATIO, PUptakeProf,
     &  PestShutP, PestRootP, PestShelP, PestSeedP)

      SenSoilP   = 0.0
      SenSurfP   = 0.0
      PUptakeProf = 0.0

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------

      RETURN
      END SUBROUTINE P_Plant
C=======================================================================


!-----------------------------------------------------------------------
! Variable definitions - updated 09-29-2004
!-----------------------------------------------------------------------
! CHAR           Contains the contents of last record read 
! CROP           Two-character crop identification code 
! DeltPRoot      Portion of P uptake that is allocated to root (g[P]/m2)
! DeltPSeed      Portion of P uptake that is allocated to seed (g[P]/m2)
! DeltPShel      Portion of P uptake that is allocated to shell (g[P]/m2)
! DeltPShut      Portion of P uptake that is allocated to leaf and stem
!                 (g[P]/m2)
! ERR            Error code for file operation 
! ERRKEY         Subroutine name for error file 
! FILECC         Path plus filename for species file (*.spe) 
! FOUND          Indicator that good data was read from file by subroutine 
!                  FIND (0 - End-of-file encountered, 1 - NAME was found) 
! FracPMobil     Portion of P in vegetative tissue which is available for 
!                  mining to supply seed and shell P demand (fraction)
! ISECT          Indicator of completion of IGNORE routine: 0 - End of file 
!                  encountered, 1 - Found a good line to read, 2 - End of 
!                  Section in file encountered denoted by * in column 1. 
! ISWPHO         Phosphorus simulation switch (Y or N) 
! Leaf_kg        Mass of leaf tissue (kg[leaf]/ha)
! LeafNew_kg     Mass of today's new growth of leaves (kg[leaf]/ha)
! LNUM           Current line number of input file 
! LUNCRP         Logical unit number for FILEC (*.spe file) 
! MDATE          Harvest maturity date (YYYYDDD)
! MSG            Text array containing information to be written to 
!                  WARNING.OUT file. 
! NL             Maximum number of soil layers = 20 
! NLAYR          Actual number of soil layers 
! PCLeafMin(3)   Minimum P concentration in leaf defined at three critical 
!                  stages (g[P]/g[leaf])
! PCLeafOpt(3)   Optimum P concentration in leaf defined at three critical 
!                  stages (g[P]/g[leaf])
! PConc_Plant    P concentration in whole plant (g[P]/g[plant])
! PConc_Root     P concentration in root (g[P]/g[root])
! PConc_Root_Min Minimum P concentration in root (g[P]/g[root])
! PConc_Root_Opt Optimum P concentration in root (g[P]/g[root])
! PConc_Seed     Concentration of P in seed (g[P]/g[seed])
! PConc_Seed_Min Minimum P concentration in seed (g[P]/g[seed])
! PConc_Seed_Opt Optimum P concentration in seed (g[P]/g[seed])
! PConc_Shel     Concentration of P in shell (g[P]/g[shell])
! PConc_Shel_Min Minimum P concentration in shell (g[P]/g[shell])
! PConc_Shel_Opt Optimum P concentration in shell (g[P]/g[shell])
! PConc_Shut     P concentration in shoots (leaf and stem) (g[P]/g[shoot])
! PConc_Shut_Min Minimum P concentration in leaf and stem (g[P]/g[shoot])
! PConc_Shut_Opt Optimum P concentration in leaf and stem (g[P]/g[shoot])
! PCRootMin(3)   Minimum P concentration in root defined at three critical 
!                  stages (g[P]/g[root])
! PCRootOpt(3)   Optimum P concentration in root defined at three critical 
!                  stages (g[P]/g[root])
! PCSeedMin(3)   Minimum P concentration in seed defined at three critical 
!                  stages (g[P]/g[seed])
! PCSeedOpt(3)   Optimum P concentration in seed defined at three critical 
!                  stages (g[P]/g[seed])
! PCShelMin(3)   Minimum P concentration in shell defined at three critical 
!                  stages (g[P]/g[shell])
! PCShelOpt(3)   Optimum P concentration in shell defined at three critical 
!                  stages (g[P]/g[shell])
! PCShutMin(3)   Minimum P concentration in leaf and stem defined at three 
!                  critical stages (g[P]/g[shoot])
! PCShutOpt(3)   Optimum P concentration in leaf and stem defined at three 
!                  critical stages (g[P]/g[shoot])
! PCStemMin(3)   Minimum P concentration in stem defined at three critical 
!                  stages (g[P]/g[stem])
! PCStemOpt(3)   Optimum P concentration in stem defined at three critical 
!                  stages (g[P]/g[stem])
! PHFRAC1_2      Fraction of physiological time which has occurred between 
!                  first and second critical phases for computation of 
!                  optimum and minimum plant P concentrations 
! PHFRAC2_3      Fraction of physiological time which has occurred between 
!                  second and third critical phases for computation of 
!                  optimum and minimum plant P concentrations 
! Pi_Avail(L)    Soil P which is available for uptake by plants. (ppm)
! Plant_kg       Mass of plant (kg/ha)
! PlantNew_kg    Mass of today's new growth of plant tissue (kg/ha)
! PMined         P mined from leaf, stem and root for seed or shell demand
!                 (kg[P]/ha)
! PMinedSd       P mined from leaf, stem and root for seed demand
!                 (kg[P]/ha)
! PMinedSl       P mined from leaf, stem and root for shell demand
!                 (kg[P]/ha)
! PPlant_kg      P content in whole plant (kg[P]/ha)
! PRoot_kg       P in roots (kg[P]/ha)
! PRoot_Min      Minimum P for root growth (kg[P]/ha)
! PRootDem       Root demand for P (kg[P]/ha)
! PSeed_kg       P content in seed (kg[P]/ha)
! PSeedDem       P demand by seed (g[P]/m2)
! PShel_kg       P content in shell (kg[P]/ha)
! PShelDem       P demand by shell (g[P]/m2)
! PShut_kg       P in shoots (leaf and stem) (kg[P]/ha)
! ShutPMin       Minimum P for root growth (kg[P]/ha)
! PShutDem       Shoot demand for P (kg[P]/ha)
! PSTRES1        P stress which affects vegetative partitioning  (1=no 
!                  stress, 0=max stress) 
! PSTRES2        P stress factor for reducing photosynthate (1=no stress, 
!                  0=max stress) 
! PSTRESS_RATIO  Ratio of P in vegetative tissue to optimum P as a measure 
!                  of plant stress (g[P]/g[P])
! PSUPPLY        Supply of P from root uptake (kg[P]/ha)
! PTotDem        Total daily plant demand for P (kg[P]/ha)
! PUptake(L)     Plant uptake of P in soil layer L (kg[P]/ha/d)
! PUptakeProf    Plant uptake of P over whole soil profile (kg[P]/ha/d)
! PMine_Avail    P in vegetative tissue which is available for mining for 
!                  growth of seed and shell (kg[P]/ha)
! P_Mobil_max    Maximum amount of P in vegetative tissue which can be 
!                  mined today (kg[P]/ha)
! RLV(L)         Root length density for soil layer L
!                 (cm[root] / cm3[soil])
! Root_kg        Root mass (kg/ha)
! RootMineFrac   Fraction of mined P which is taken from roots 
! RootNew_kg     Mass of today's new growth of roots (kg/ha)
! RWU(L)         Root water uptake from soil layer L (cm/d)
! SECTION        Section name in input file 
! Seed_kg        Seed mass (kg/ha)
! SeedNew_kg     Mass of today's new growth of seed (kg/ha)
! SenSoilP       P in senesced root and nodule tissue (kg[P]/ha/d)
! SenSurfP       P in senesced canopy tissue (kg[P]/ha/d)
! Shel_kg        Shell mass (kg/ha)
! ShelNew_kg     Mass of today's new growth of shell (kg/ha)
! Shut_kg        Shoots mass (kg/ha)
! ShutMineFrac   Fraction of mined P which is taken from shoots 
! ShutNew_kg     Mass of today's new growth of shoots (kg/ha)
! SRATPART       Minimum value of the ratio of P in vegetative tissue to 
!                  the optimum P, below which vegetative partitioning will 
!                  be affected (g[P]/g[P])
! SRATPHOTO      Minimum value of the ratio of P in vegetative tissue to 
!                  the optimum P, below which reduced photosynthesis will 
!                  occur (g[P]/g[P])
! Stem_kg        Stem mass (kg/ha)
! StemNew_kg     Mass of today's new growth of stem (kg/ha)
! UseShoots      Logical variable (true or false), indicates whether input 
!                  from crop routine and species file will be for shoots 
!                  (true) or leaf and stem (false) 
! YRPLT          Planting date (YYYYDDD)
!-----------------------------------------------------------------------
!       END SUBROUTINE PLANT
!=======================================================================
    

