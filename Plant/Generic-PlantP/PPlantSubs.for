!=======================================================================
!  PPlantSubs, Contains subroutines for use by plant phosphorus model.
!=======================================================================

!=======================================================================
!  PValue, Linearly interpolates daily optimum and minimum P values 
!    based on growth stage fractions.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  05/20/2005 CHP Written.
!-----------------------------------------------------------------------
!  Called by: P_PLANT
!=======================================================================
      Function PValue(PhFrac1, PhFrac2, PArray)

!     ------------------------------------------------------------------
      Real PValue                   !Interpolated value returned
      Real, Intent(IN) :: PhFrac1   !Fraction complete growth stage 1
      Real, Intent(IN) :: PhFrac2   !Fraction complete growth stage 2
      Real, Intent(IN) :: PArray(3) !Array of values to be interpolated

!
!                |
!      PArray(1)>+   x
!      PArray(2)>+            x
!                | 
!                |
!      PArray(3)>+                     x
!                |___________________________
!                    |        |        |
!                Emergence  Stage1   Stage2            
!
!     ------------------------------------------------------------------
!     Calculate optimum and minimum P concentrations in plant tissue.
!     ------------------------------------------------------------------
      IF (PhFrac1 < 1.E-5) THEN
        !Prior to emergence
        PValue = PArray(1)

      ELSEIF (PhFrac1 < 1.0) THEN
        !First to second critical stage
        PValue = PArray(1) - (PArray(1) - PArray(2)) * PhFrac1

      ELSEIF (PhFrac2 < 1.0) THEN
        !Second to third critical stage
        PValue = PArray(2) - (PArray(2) - PArray(3)) * PhFrac2

      ELSE
        !Subsequent to third critical stage to harvest
        PValue = PArray(3)
      ENDIF

      PValue = MAX(0.0, PValue)

      RETURN
      End Function PValue
!=======================================================================

!=======================================================================
!  P_Demand
!     CALCULATE DEMANDS in kg/ha
!     and reduction in demands due to mobilized tissue.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  05/20/2005 CHP Written.
!-----------------------------------------------------------------------
!  Called by: P_PLANT
!=======================================================================
      Subroutine P_Demand(DYNAMIC,
     &    PConc_Root, PConc_Root_min, PConc_Root_opt,     !Input
     &    PConc_Shel, PConc_Shel_min, PConc_Shel_opt,     !Input 
     &    PConc_Shut, PConc_Shut_min, PConc_Shut_opt,     !Input
     &    PConc_Seed_opt, PRoot_kg, PSeed_kg, PShel_kg,   !Input
     &    PShut_kg, Root_kg, RootMob, Seed_kg, Shel_kg,   !Input
     &    ShelMob, Shut_kg, ShutMob,                      !Input
     &    DeltPRoot, DeltPSeed, DeltPShel, DeltPShut,     !I/O
     &    PRootDem, PSeedDem, PShelDem, PShutDem,         !Output
     &    PTotDem)                                        !Output

      USE ModuleDefs
      IMPLICIT NONE
      SAVE

      Integer DYNAMIC

      Real Shut_kg, Root_kg, Shel_kg, Seed_kg
      Real PShut_kg, PRoot_kg, PShel_kg, PSeed_kg
      Real ShutMob, RootMob, ShelMob
      REAL PConc_Shut_opt, PConc_Root_opt, PConc_Shel_opt,PConc_Seed_opt
      REAL PConc_Shut_min, PConc_Root_min, PConc_Shel_min
      Real DeltPShut, DeltPRoot, DeltPShel, DeltPSeed
      Real PShutDem, PRootDem, PShelDem, PSeedDem, PTotDem
      Real PShutMobPool, PRootMobPool, PShelMobPool, PSeedMobPool
      Real ShutPOpt, RootPOpt, ShelPOpt, SeedPOpt
      Real ShutPMin, RootPMin, ShelPMin
      REAL PConc_Shut, PConc_Root, PConc_Shel
      Real PShutMobToday, PRootMobToday, PShelMobToday

!***********************************************************************
!***********************************************************************
!     SEASONAL INITIALIZATION: RUN ONCE PER SEASON.
!***********************************************************************
      IF (DYNAMIC == SEASINIT .OR. DYNAMIC .EQ. RUNINIT) THEN
!     ------------------------------------------------------------------
      PShutMobPool = 0.0
      PRootMobPool = 0.0
      PShelMobPool = 0.0
      PSeedMobPool = 0.0

!***********************************************************************
!***********************************************************************
!     DAILY INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC == INTEGR) THEN
!-----------------------------------------------------------------------
!*** Need to take a mass balance approach.
!     Each day, compute max and min P allowable (in kg/ha).
!     Use these to check P mobilization amounts.  Don't move P if
!     concentrations are too low.
      ShutPMin = Shut_kg * PConc_Shut_min
      RootPMin = Root_kg * PConc_Root_min
      ShelPMin = Shel_kg * PConc_Shel_min

      ShutPOpt = Shut_kg * PConc_Shut_opt
      RootPOpt = Root_kg * PConc_Root_opt
      ShelPOpt = Shel_kg * PConc_Shel_opt
      SeedPOpt = Seed_kg * PConc_Seed_opt

!     ------------------------------------------------------------------
!     Compute P demand.  If demand is negative (due to high P content), 
!     P becomes available for mobilization.

      !Shoots
      PShutDem = ShutPOpt - PShut_kg
      IF (PShutDem < 0.0) THEN
        PShutMobPool = PShutMobPool - PShutDem    !(neg. val.)
        PShutDem = 0.0
      ENDIF

      !Roots
      PRootDem = RootPOpt - PRoot_kg 
      IF (PRootDem < 0.0) THEN
        PRootMobPool = PRootMobPool - PRootDem    !(neg. val.)
        PRootDem = 0.0
      ENDIF
      
      !Shell
      PShelDem = ShelPOpt - PShel_kg 
      IF (PShelDem < 0.0) THEN
        PShelMobPool = PShelMobPool - PShelDem    !(neg. val.)
        PShelDem = 0.0
      ENDIF
      
      !Seed - no mobilization pool
      PSeedDem = SeedPOpt - PSeed_kg 
      PSeedDem = MAX(0.0, PSeedDem)

!     ------------------------------------------------------------------
!     Mobilized P - reduce demand by amount which is mobilized.
!     Some P is forced to move with N & C mobilization.  Reduce total 
!     P demand to account for this.  If there is excess P mobilized, 
!     then save it for next time step.
!     ------------------------------------------------------------------
      !Shoots
      PShutMobPool = AMAX1(0.0, ShutMob * PConc_Shut) + PShutMobPool
!     Amount which can be mobilized should not reduce P below minimum
      PShutMobToday = AMIN1(PShutMobPool, PShut_kg - ShutPMin)
      IF (PShutDem >= PShutMobToday) THEN
        PShutMobPool = PShutMobPool - PShutMobToday
        PShutDem = PShutDem - PShutMobToday
        PShutMobToday = 0.0
      ELSE
        PShutMobPool = PShutMobPool - PShutDem
        PShutMobToday = PShutMobToday - PShutDem
        PShutDem = 0.0
      ENDIF

      !Roots
      PRootMobPool = AMAX1(0.0, RootMob * PConc_Root) + PRootMobPool
      PRootMobToday = AMIN1(PRootMobPool, PRoot_kg - RootPMin)
      IF (PRootDem >= PRootMobToday) THEN
        PRootMobPool = PRootMobPool - PRootMobToday
        PRootDem = PRootDem - PRootMobToday
        PRootMobToday = 0.0
      ELSE
        PRootMobPool = PRootMobPool - PRootDem
        PRootMobToday = PRootMobToday - PRootDem
        PRootDem = 0.0
      ENDIF

      !Shell
      PShelMobPool = AMAX1(0.0, ShelMob * PConc_Shel) + PShelMobPool
      PShelMobToday = AMIN1(PShelMobPool, PShel_kg - ShelPMin)
      IF (PShelDem >= PShelMobToday) THEN
        PShelMobPool = PShelMobPool - PShelMobToday
        PShelDem = PShelDem - PShelMobToday
        PShelMobToday = 0.0
      ELSE
        PShelMobPool = PShelMobPool - PShelDem
        PShelMobToday = PShelMobToday - PShelDem
        PShelDem = 0.0
      ENDIF

!     ------------------------------------------------------------------
!     If mobilization pools exist, they should be 
!     used first to supply demand to shells and seeds
!     Shoots -> Seeds
      IF (PShutMobToday > 1.E-5 .AND. PSeedDem > 1.E-5) THEN
        IF (PSeedDem >= PShutMobToday) THEN
          DeltPSeed = DeltPSeed + PShutMobToday
          DeltPShut = DeltPShut - PShutMobToday
          PSeedDem = PSeedDem - PShutMobToday
          PShutMobToday = 0.0
        ELSE
          DeltPSeed = DeltPSeed + PSeedDem
          DeltPShut = DeltPShut - PSeedDem
          PShutMobToday = PShutMobToday - PSeedDem
          PSeedDem = 0.0
        ENDIF
      ENDIF

!     Shoots -> Shells
      IF (PShutMobToday > 1.E-5 .AND. PShelDem > 1.E-5) THEN
        IF (PShelDem >= PShutMobToday) THEN
          DeltPShel = DeltPShel + PShutMobToday
          DeltPShut = DeltPShut - PShutMobToday
          PShelDem = PShelDem - PShutMobToday
          PShutMobToday = 0.0
        ELSE
          DeltPShel = DeltPShel + PShelDem
          DeltPShut = DeltPShut - PShelDem
          PShutMobToday = PShutMobToday - PShelDem
          PShelDem = 0.0
        ENDIF
      ENDIF

!     Roots -> Seeds
      IF (PRootMobToday > 1.E-5 .AND. PSeedDem > 1.E-5) THEN
        IF (PSeedDem >= PRootMobToday) THEN
          DeltPSeed = DeltPSeed + PRootMobToday
          DeltPRoot = DeltPRoot - PRootMobToday
          PSeedDem = PSeedDem - PRootMobToday
          PRootMobToday = 0.0
        ELSE
          DeltPSeed = DeltPSeed + PSeedDem
          DeltPRoot = DeltPRoot - PSeedDem
          PRootMobToday = PRootMobToday - PSeedDem
          PSeedDem = 0.0
        ENDIF
      ENDIF

!     Roots -> Shells
      IF (PRootMobToday > 1.E-5 .AND. PShelDem > 1.E-5) THEN
        IF (PShelDem >= PRootMobToday) THEN
          DeltPShel = DeltPShel + PRootMobToday
          DeltPRoot = DeltPRoot - PRootMobToday
          PShelDem = PShelDem - PRootMobToday
          PRootMobToday = 0.0
        ELSE
          DeltPShel = DeltPShel + PShelDem
          DeltPRoot = DeltPRoot - PShelDem
          PRootMobToday = PRootMobToday - PShelDem
          PShelDem = 0.0
        ENDIF
      ENDIF

!     Shells -> Seeds
      IF (PShelMobToday > 1.E-5 .AND. PSeedDem > 1.E-5) THEN
        IF (PSeedDem >= PShelMobToday) THEN
          DeltPSeed = DeltPSeed + PShelMobToday
          DeltPShel = DeltPShel - PShelMobToday
          PSeedDem = PSeedDem - PShelMobToday
          PShelMobToday = 0.0
        ELSE
          DeltPSeed = DeltPSeed + PSeedDem
          DeltPShel = DeltPShel - PSeedDem
          PShelMobToday = PShelMobToday - PSeedDem
          PSeedDem = 0.0
        ENDIF
      ENDIF

!     If leftover mobilizable tissue remains, add back to pool
      PShutMobPool = PShutMobPool + PShutMobToday
      PRootMobPool = PRootMobPool + PRootMobToday
      PShelMobPool = PShelMobPool + PShelMobToday

!     Recalculate demand
      PTotDem = PShutDem + PRootDem + PShelDem + PSeedDem

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      Return
      End Subroutine P_Demand

C=======================================================================


!=======================================================================
!  P_Partition
!     Partition P which has been mobilized and taken up from soil
!     to shoot, root, shell and seed.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  05/20/2005 CHP Written.
!-----------------------------------------------------------------------
!  Called by: P_PLANT
!=======================================================================
      Subroutine P_Partition(
     &    FracPMobil, PConc_Root_min, PConc_Shel_min,     !Input
     &    PConc_Shut_min, PRootDem, PRoot_kg, PSeedDem,   !Input
     &    PShelDem, PShel_kg, PShutDem, PShut_kg,         !Input
     &    PUptakeProf, Root_kg, Shel_kg, Shut_kg,         !Input
     &    DeltPRoot, DeltPSeed, DeltPShel, DeltPShut)     !I/O

!-----------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE
      SAVE

      REAL ADD, PConc_Shut_min, PConc_Root_min, PConc_Shel_min
      REAL Shut_kg, Root_kg, Shel_kg
      REAL PMine_Avail, FracPMobil, PMined
      REAL ShutMineFrac, RootMineFrac, ShelMineFrac
      REAL PUptakeProf, PSupply
      REAL PShutDem, PRootDem, PShelDem, PSeedDem
      REAL DeltPShut, DeltPRoot, DeltPShel, DeltPSeed
      REAL ShutPMin, PRootMin, PShelMin
      REAL PShut_kg, PRoot_kg, PShel_kg
      REAL P_Mobil_max

!-----------------------------------------------------------------------
!     Calculate minimum vegetative P (kg/ha)
      ShutPMin = PConc_Shut_min * Shut_kg 
      PRootMin = PConc_Root_min * Root_kg
      PShelMin = PConc_Shel_min * Shel_kg

!     P available for mining (kg/ha)   
!     This is in addition to the P mobilization which is tied to tissue
!     loss due to N and C mobilization.
      PMine_Avail = PShut_kg  + PRoot_kg  + PShel_kg      !Actual P
     &            - ShutPMin - PRootMin - PShelMin     !Minimum P

!     Maximum that can be mobilized in one day
      P_Mobil_max = MAX(0.0, FracPMobil * PMine_Avail)

!     Fraction of mined P that will come from roots, shoots, and shells
      IF (PMine_Avail > 0.0) THEN
        ShutMineFrac = (PShut_kg - ShutPMin) / PMine_Avail
        RootMineFrac = (PRoot_kg - PRootMin) / PMine_Avail
        ShelMineFrac = 1.0 - ShutMineFrac - RootMineFrac
      ENDIF

      PMined = 0.0

!------------------------------------------------------------------------
!     Partitioning of P 
!     First preference to seed, then to shell, then to veg.
!     All DeltP values in kg/ha
!------------------------------------------------------------------------
      PSupply = PUptakeProf   ! + PTotMob       !P to be partitioned

!     First meet demand for seeds
      IF (PSeedDem > 0. .AND. PSeedDem <= PSupply) THEN
        !Supply for seeds met with some left over
        DeltPSeed  = DeltPSeed + PSeedDem
        PSupply = PSupply - PSeedDem
      ELSEIF (PSeedDem > 0. .AND. PSeedDem > PSupply) THEN    
        !P mining from vegetative tissue to meet P demand for seeds
        PMined = MIN(PSeedDem - PSupply, P_Mobil_max)
        !P_Mobil_max = P_Mobil_max - PMined  
        DeltPSeed = DeltPSeed + PSupply + PMined
        PSupply = 0.0
      ELSE
        !No demand for seed
        DeltPSeed = DeltPSeed + 0.0
      ENDIF

!     Then meet demand for shells
      IF (PShelDem > 0. .AND. PSupply > 0.) THEN
        !Supply for shells met with some left over
        ADD = AMIN1(PShelDem, PSupply)
        DeltPShel = DeltPShel + ADD
        PSupply = PSupply - ADD
      ELSEIF (PMined > 0.) THEN
        !P mined from shells for seed growth
        DeltPShel = DeltPShel - PMined * ShelMineFrac
        PSupply = 0.0
      ELSE
        !No demand for shell
        DeltPShel = DeltPShel + 0.0
      ENDIF 

!     Then meet demand for vegetative tissue
      IF (PShutDem + PRootDem > 0. .AND. PSupply > 0.) THEN
        !Proportion remaining P to shoots and roots.
        DeltPShut = DeltPShut + PShutDem / (PShutDem + PRootDem)*PSupply
        DeltPRoot = DeltPRoot + PRootDem / (PShutDem + PRootDem)*PSupply
        PSupply = 0.0
      ELSEIF (PMined > 0.) THEN
        !Partition P mined between shoots and roots.
        DeltPShut = DeltPShut - PMined * ShutMineFrac
        DeltPRoot = DeltPRoot - PMined * RootMineFrac
      ELSE
        DeltPShut = DeltPShut + 0.0
        DeltPRoot = DeltPRoot + 0.0
      ENDIF


!-----------------------------------------------------------------------
      Return
      End Subroutine P_Partition

C=======================================================================
