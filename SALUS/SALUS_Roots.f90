!==============================================================================
! Subroutine to compute root growth
! "New" root growth to replace the one from DSSAT 3.5.
! Called by both the simple and the complex crop, therefore should exist
! in its own source file.  Based on the stand-alone program ROOTALON.
!-----------------------------------------------------------------------
!==============================================================================

subroutine SALUS_Roots(dynamic, soilprop, dBiomassRoot,   & !Input
   dtt, lai, laimax, rlwr, st, sw,plantpop,sowdepth,      & !Input
   RootDepth, RootLayerFrac, RootMassLayer, rlv)            !Output

USE ModuleDefs
implicit none 
save

real, parameter :: RockDensity=2.65, RtDepthIncPerDD=0.10, SHFScaleFac=0.10 
real, dimension(nl) :: aLayer, bd, dlayr, ds, ll, st, sw, rlv, shf, RootLayerFrac
real, dimension(nl) :: RootMassLayer, RootPresence, dRootLayerFrac, RootDepthFac, RootWtFac
real :: dBiomassRoot, DepthMax, dtt, lai, laimax, plantpop, rlwr, RootDepth, sowdepth
real :: ColdFac, DryFac, Porosity, RootDepthInc, SatFac, SumFactors, SumWeight, WatFillPor
integer :: dynamic, nlayr, RootFrontLayer, RootsGrowDown, layer

type(SoilType) soilprop
bd    = soilprop % bd
dlayr = soilprop % dlayr
ds    = soilprop % ds
ll    = soilprop % ll
nlayr = soilprop % nlayr
shf   = soilprop % wr

!------------------------------------------------------------------------------
! INITIALIZATION AND INPUT DATA
!------------------------------------------------------------------------------

! if(dynamic == runinit .or. dynamic == seasinit) then
if(dynamic == seasinit) then
! Initialize Variables
DepthMax = ds(nlayr)
aLayer(1) = 0.0
do layer = 2, nlayr
   aLayer(layer) = DS(layer-1)
end do
RootLayerFrac  = 0.0     
RootFrontLayer = 1 
RootDepth = sowdepth
      
! Following email with Bruno (07/27/2010)-
! Use exponential function from SALUS to compute RootDepthFac
! First compute depth to middle of layer      
!  DO layer = 1, NLAYR
!     midLayer(layer) = DS(layer) - (DLAYR(layer)/2)
!  ENDDO
  
! From VB codes
! Root_Depth_Fac(i) = 1 when ZLayr(i) <= 15
! Root_Depth_Fac(i) = exp(-(Z(i)-15.0)*0.03) when ZLayr(i) > 15
do layer = 1, nlayr
   if(ds(layer) <= 15.0) then
      RootDepthFac(layer) = 1.0
   else
      RootDepthFac(layer) = EXP(-(ds(layer)-15.0)*0.03)
   end if
end do
            
! Kofikuma to check Salus vs DSSAT soil layer thicknesses
! use exponential function instead
! Check with Bruno to see if we need these factors or if they are
! not the same as SHF
!  RootDepthFac     = 0.01 !all layers, to NL
!  RootDepthFac(1)  = 1.0
!  RootDepthFac(2)  = 1.0
!  RootDepthFac(3)  = 1.0
!  RootDepthFac(4)  = 0.85
!  RootDepthFac(5)  = 0.58
!  RootDepthFac(6)  = 0.37
!  RootDepthFac(7)  = 0.21
!  RootDepthFac(8)  = 0.11
!  RootDepthFac(9)  = 0.05
!  RootDepthFac(10) = 0.03
!  RootDepthFac(11) = 0.01
!  RootDepthFac(12) = 0.01

! do layer = 1, nlayr
!    rlv(layer) = (0.01*plantpop)*rlwr*1.E-4/dlayr(layer)
!!      (cm/cm3)   = (g/plt)*(plts/m2)*(cm/g)*(10^-4)/cm
! end do

!------------------------------------------------------------------------------
! RATE CALCULATIONS
!------------------------------------------------------------------------------
else if(dynamic == rate) then
!------------------------------------------------------------------------------
! First step:  grow roots downwards.  The new depth that the roots reached
! today is a function of the thermal time that drives growth, and potentially
! impeding factors: Soil Hospitality Factor, saturated soil, dry soil, and low temperature.
! Check conditions for root growth
if(lai <= laimax) then
    RootsGrowDown = 1        !Roughly until flowering
else
    RootsGrowDown = 0
end if

if(RootsGrowDown == 1) then

   ! Calculate impeding factors at the deepest root layer:
   ! Saturation stress as soon as water-filled porosity exceeds 80%:
   Porosity = 1.0 - BD(RootFrontLayer) / RockDensity
   WatFillPor = SW(RootFrontLayer) / Porosity
   SatFac = min(1.0, 5.0 * (1.0 - WatFillPor))
   
   ! Dry soil stress as soon as soil dries to lower limit plus 0.04 cm3 cm-3:
   DryFac = max(0.0, min(1.0, (sw(RootFrontLayer) - ll(RootFrontLayer)) * 1.0 / 0.04))
   
   ! Cold stress as soon as soil temperature drops to 4 C:
   ColdFac = max(0.0, min(1.0, st(RootFrontLayer) * 1.0 / 4.0))
   
   ! Root depth increment for the day (should be in cm):
   RootDepthInc = RtDepthIncPerDD * dtt * min(shf(RootFrontLayer), SatFac, DryFac, ColdFac)
   
   ! Add to existing root depth but do not exceed soil depth:
   RootDepth = min(RootDepth + RootDepthInc, DepthMax)
   
   ! Check if the roots have grown into the next layer:
   do while (RootDepth > ds(RootFrontLayer))
     RootFrontLayer = RootFrontLayer + 1
   end do

end if !Roots grow down
      
! Second step: Calculate the root dry matter for each layer
SumFactors = 0.0

do layer = 1, MIN(RootFrontLayer, nl) !Fraction of rtwt/layer
   !What is the fraction of root presence in each layer?
   if(layer < RootFrontLayer) then
     RootPresence(layer) = 1.0
   else
     RootPresence(layer) = (RootDepth - aLayer(RootFrontLayer))/dlayr(RootFrontLayer)
   end if

   !Saturation stress as soon as water-filled porosity exceeds 80%:
   Porosity = 1.0 - bd(layer) / RockDensity
   WatFillPor = sw(layer) / Porosity
   SatFac = min(1.0, 5.0 * (1.0 - WatFillPor))

   !Dry soil stress as soon as soil dries to lower limit plus 0.04 cm3 cm-3:
   DryFac = max(0.0, min(1.0, (SW(layer) - LL(layer))* 1.0 / 0.04))

   !Cold stress as soon as soil temperature drops to 4 C:
   ColdFac = max(0.0, min(1.0, ST(layer) * 1.0 / 4.0))
  
   RootWtFac(layer) = RootDepthFac(layer) * RootPresence(layer)*shf(layer)*min(SatFac, DryFac, ColdFac)
   SumFactors = SumFactors + RootWtFac(layer)
  
end do  !Fraction of root weight in each layer

! Compute root dry matter in each layer and convert to root length density      
SumWeight = 0.0
if(SumFactors > 0.0) then
   do layer = 1, min(RootFrontLayer, nl)
      dRootLayerFrac(layer) = dBiomassRoot*RootWtFac(layer)/ SumFactors
      RootMassLayer(layer) = RootMassLayer(layer) + dRootLayerFrac(layer)
      !!RootMassLayer(layer) = BiomassRoot * RootWtFac(layer)/ SumFactors
      SumWeight = SumWeight + RootMassLayer(layer)            
      !rlv(layer) = min(rlwr * RootMassLayer(layer) * 1.E-4/dlayr(layer), 4.0)
      !rlv(layer) = rlwr * RootMassLayer(layer) * 1.E-4/dlayr(layer)
      !             cm[root]    cm[root]    g[root]    m2          1
      !            --------- =  -------- *  ------- * ------- * -------
      !            cm3[soil]    g [root]    m2[soil]  10^4 cm2  cm[soil]
      rlv(layer) = rlv(layer) + rlwr*dRootLayerFrac(layer) * 1.E-4/dlayr(layer)
   end do
   if(SumWeight > 0.0) then
      do layer = 1, min(RootFrontLayer, nl)
         RootLayerFrac(layer) = RootMassLayer(layer)/ SumWeight
      end do
   end if
end if

! In case that on the first day of growth there is no root growth, RootLayerFrac
! must be initialized to something other than zero: - AG
if(sum(RootLayerFrac) <= 0.0 .AND. RootDepth > 0.0) then
   do layer = 1, RootFrontLayer
      RootLayerFrac(layer) = DLAYR(layer)*RootPresence(layer)/ RootDepth
   end do
end if

!------------------------------------------------------------------------------
end if
!------------------------------------------------------------------------------
return
end subroutine SALUS_Roots
      
      
!=======================================================================
! InitializeRoots Subroutine
! Initializes root variables at emergence.
! 12/22/2010 Adapted for SALUS from CROPGRO's inroot subroutine in ROOTS.for
!             
subroutine InitializeRoots(reltt, plantpop, sowdepth, dlayr, nlayr, rlwr, wpseed,  & !Inputs
   BiomassRoot, Biomass, RootDepth, rlv)                                             !Outputs
   
use ModuleDefs     
implicit none

integer :: layer, nlayr
real :: SeedWt, rootpartcoeff, RootWtEmg, AboveWtEmg, CumDepth, CumDepthProfile, RLVInit
real :: rlv(nl), dlayr(nl), reltt, plantpop, sowdepth, rlwr, wpseed, BiomassRoot, Biomass, RootDepth

! Compute dry matter weight at emergence
rootpartcoeff = 0.45 * exp(-1.904*reltt)
rootwtemg  = wpseed * plantpop * rootpartcoeff
abovewtemg = wpseed * plantpop * (1-rootpartcoeff)

! Initialize Biomass and BiomassRoot at emergence
BiomassRoot = RootWtEmg
Biomass     = AboveWtEmg
RootDepth   = 2*sowdepth   !Plant growing both parts- roots and leaves
!-----------------------------------------------------------------------
! DISTRIBUTE ROOT LENGTH EVENLY IN ALL LAYERS TO A DEPTH OF
! RTDEPTI (ROOT DEPTH AT EMERGENCE)
!-----------------------------------------------------------------------
CumDepthProfile = 0.0
do layer = 1, nlayr
   rlv(layer) = 0.0
end do

do layer = 1, nlayr
   CumDepth = min(RootDepth - CumDepthProfile, dlayr(layer))
   RLVInit  = BiomassRoot * rlwr * (CumDepth/RootDepth) * 1.E-4
   !  cm[root]      g[root]   cm[root]    m2
   !  -------- = ---------- * -------- * --------
   !cm2[ground]  m2[ground]    g[root]   10^4 cm2
   CumDepthProfile = CumDepthProfile + CumDepth
   rlv(layer) = RLVInit / dlayr(layer)
   if(CumDepthProfile >= RootDepth) exit
end do

return
end subroutine InitializeRoots
!=======================================================================

      
!------------------------------------------------------------------------------
! Variable definitions - updated 
!------------------------------------------------------------------------------
! aLayer: Depth to top of layer
! dBIOMASSROOT: Incremental root dry matter weight (g m-2 d-1)
! ColdFac: Cold soil temperature factor
! CumDepth: Cumulative soil depth (cm)
! CumDepthProfile: Cumulative depth of soil profile (cm)
! DepthMax: Maximum depth of soil
! DTT: Daily thermal time increment (degree-days)
! LAI: Canopy leaf area index (m2 m-2)
! LAIMAX: Maximum expected Leaf Area Index (m2 m-2)
! layer: Soil layer
! LL: Lower limit
! NL: Maximum number of soil layers       
! NLAYR: Number of soil layers    
! RLV: Root length density for soil layer "layer" (cm[root]/cm3[soil])
! RLVInit: Initial root density (cm[root]/cm2[ground])
! RLWR: Root Length to Weight Ratio (cm[root]/g[root]) 
! RootDepth: Root depth (cm)
! RootsGrowDown: Root growth flag; 1 to indicate that roots grow down
! RootFrontLayer: Root front layer
! RootLayerFrac: Fraction of live root mass in a given layer
! RootMassLayer: Root mass per layer (g m-2)
! RootPresence: Root presence in a layer (0 - 1)
! RtDepthIncPerDD: Root depth increment per degree day [cm]
! SHF: Soil hospitality factor
! SHFScaleFac: Scale-down factor for soil hospitality factor
! ST: Soil temperature
! SW: Soil water
! WatFillPor: Water filled porosity
!
! DSSAT          SALUS       Definition
! BD             BD          Bulk density
! DS             zLayer      Depth to bottom of layer
! DLAYR          dLayer      Depth of layer