!======================================================================
!  MZ_ROOTGR, Subroutine
!
!  Computes Daily Root Growth and Distribution
!----------------------------------------------------------------------
!  Revision history
!
!  1. Written
!  2  Modified by                           E. Alocilja & B. Baer 9-88
!  3  Modified by                           T. Jou                4-89
!  4. Header revision and minor changes             P.W.W.      2-8-93
!  5. Added switch block, etc.                      P.W.W.      2-8-93
!  6. Simplified the RLNEW calculation and slowed
!     the growth of roots in deeper soils.   J.T.R. & B.D.B.    6-20-94
!  7. Converted to modular routine                  W.D.B.      4-01-01
!  8. Further modular changes                       W.D.B      12-01-01
!  9. Adapted for AgMaize                           K.A.D.     07-18-13
!----------------------------------------------------------------------
!  Called by : MZ_AG_GLOBAL
!  Calls     : None
!
! TODO:
!-Soil water (sw) at the ll when water stress is turned off?
!----------------------------------------------------------------------

subroutine MZ_AG_RootGro(control, SoilProp, iswitch,          & !Control
       no3, nh4, sw,                                          & !MZ_GLOBAL
       growthStage, gstdyrdoySim, gddcer, dtt,                & !From Phenology
       grort,                                                 & !From Growth
       swfac,                                                 & !From WFactor
       cumdep, rtdep, rlv)                                      !Output
!----------------------------------------------------------------------
use ModuleDefs
use MZ_AG_ModuleDefs
implicit  none
save
!----------------------------------------------------------------------
character :: iswnit*1, files*12, pathsr*80
integer :: dynamic, nlayr, yrdoy, growthStage, gstdyrdoySim(20)
real :: gddcer, dtt, grort, swfac
real, dimension (nl) :: ds, dlayr, ll, dul, sat, sw, esw, nh4, no3, rldf, shf   

! Variables obtained locally
integer :: l, l1
real :: depmax, rlnew, rnfac, rnlf, rtexf, rtsurv, swdf, swexf, trldf
real :: pormin, rlwr, pltpop, sdepth

! Outputs
real :: cumdep, rtdep, rlv(nl)

! Constructed types
type (ControlType), intent(in) :: control
type (SoilType),    intent(in) :: SoilProp
type (SwitchType),  intent(in) :: iswitch
type(FileioType)  :: datafileio    
type(SpeciesType) :: dataspecies          

! Transfer values from constructed data types into local variables
dynamic = control % dynamic
yrdoy   = control % yrdoy
dlayr   = SoilProp % dlayr
nlayr   = SoilProp % nlayr
ll      = SoilProp % ll
dul     = SoilProp % dul
sat     = SoilProp % sat
ds      = SoilProp % ds
shf     = SoilProp % wr
iswnit  = iswitch % iswnit
!iswwat  = iswitch % iswwat


!----------------------------------------------------------------------
! Dynamic = runinit or seasinit
!----------------------------------------------------------------------
if(dynamic==runinit .OR. dynamic==seasinit) then

!Read all sections of fileio and transfer variables
call readfileio(control, 'ALLSEC', datafileio)
files  = datafileio % files
pathsr = datafileio % pathsr
sdepth = datafileio % sdepth 
pltpop = datafileio % pltpop

!Read species file section ROOT and transfer variables
call readspecies(files, pathsr, '*ROOT ', dataspecies)
pormin = dataspecies % pormin 
rlwr   = dataspecies % rlwr

depmax = ds(nlayr) 
esw   = (/ (dul(l)-ll(l), l=1,nl) /)
rldf  = 0.0
rlv   = 0.0
rtdep = 0.0
rnlf  = 0.0
rnfac = 0.0
rlnew = 0.0
swfac = 1.0
cumdep = 0.0

!----------------------------------------------------------------------
! Dynamic = integr
!----------------------------------------------------------------------
elseif(dynamic==integr) then

!---Before emergence set the root depth to the sowing depth
if(growthStage==1) rtdep = sdepth         

!---Initialize root depth and root length volume at emergence    
if(growthStage==2) rtdep = rtdep + 0.15*dtt
!Q. Replace this whole section with CROPGRO's approach
!KAD: For each layer with root presence at emergence, compute RLV. The last layer with
!root presence is probably incompletely explored so find the fraction of that layer
!actually explored by roots as 1 - (cumdep-rtdep)/dlayr and use that to reduce RLV
!for that layer. For any other layer below, initialize RLV to 0.
if(yrdoy==gstdyrdoySim(2)) then      !On the day of emergence
    cumdep = 0.0
    do l = 1, nlayr
       cumdep = cumdep + dlayr(l)
       rlv(l) = 0.20*pltpop/dlayr(l)
       if(cumdep > rtdep) exit
    end do

    rlv(l) = rlv(l)*(1.0-(cumdep-rtdep)/dlayr(l))
    l1 = l + 1
    if(l1 < nlayr) rlv = (/ (0.0, l=l1,nlayr) /)
end if

!---KAD: New root length
if(grort <= 1.E-4) return
!KAD: Q. Check- Should the program be allowed to proceed before emergence?
!KAD: Q. Need to add some conditions regarding water stress

!The small differences between root length/weight ratios used in earlier models were insignificant considering
!the uncertainty of the value and the uncertainty of loss of assimilate by exudation and respiration.
!A compromise value of 0.98 was choosen for all crops.
!Calculate new root length to be added to the total root system length, cm. root cm2 ground
rlnew = grort * pltpop * rlwr
! cm[roots]          g[roots]   plt   m2[ground]*cm[roots]
! ---------------- = -------- * --- * ---------------------
! cm2[ground]*day    plt*day    m2    cm2[ground]*g[roots]

!---KAD: Combined root length density weighing factor for each soil layer (rldf) accounting
!for the current root depth. The rldf combines the soil water deficit factor (swdf), a
!root nitrogen availability factor (rnfac) and a soil hospitality factor (shf) which is
!actually the soil root growth factor (srgf) from the soil file. Overall, rldf is close
!to 1 when conditions are optimal for root growth.
cumdep = 0.0
rnfac  = 1.0
l = 0
do while((cumdep < rtdep) .AND. (l < nlayr))
   l = l + 1
   cumdep = cumdep + dlayr(l)
   if(sw(l)-ll(l) < 0.25*esw(l)) then       !Same as if 4.0*(sw(l)-ll(l)) < esw(l)
      swdf = 4.0*(sw(l) - ll(l))/esw(l)     !Soil water deficit factor for Layer l
      if(swdf < 0.0) swdf = 0.0
   else
      swdf = 1.0
   endif

   !Made all crops so that RNFAC is constrained between 0.01 and 1.0;
   !on page 94 of Jones & Kiniry book the minimum is 0.01. - WTB
   if(iswnit /= 'N') then
      rnfac = 1.0 - (1.17*exp(-0.15*(no3(l) + nh4(l))))
      rnfac = amax1(rnfac,0.01)
   end if

   rldf(l) = amin1(swdf,rnfac)*shf(l)*dlayr(l)      
end do

l1 = l    !KAD: Current layer (determined by actual root depth)

!---KAD: Fraction of root survival on a given day depends on an excess water stress factor (swexf) and a fraction of
!root depth due to oxygen depleted soil (rtexf)
!The following changes were made to simplify the code and make the model more generic. It also takes into account 
!some newer data provided by Julio Dardenelli of Argentina. For the first time the ceres model restricts the rate 
!of downward movement of roots with the soil property -- root weighting factor -- to account for greater difficulty 
!in growing downward in hard soil. Changes made by JTR 6/16/94.
! WDB 10/22/03  
rtexf = 0.1      !Fraction root death per day under oxygen depleted soil 
swexf = 1.0      !Excess water stress factor for layer with deepest roots (0-1) 
if(sat(l) - sw(l) < pormin) then
   swexf = (sat(l) - sw(l)) / pormin
   swexf = min(swexf, 1.0)
end if

!Fraction survival of roots on a given day, taking into account death due to excess or deficit water conditions 
rtsurv = min(1.0,(1.-rtexf*(1.-swexf)))

!---KAD: Root front progression (cm); this is the actual root depth calculation accounting for water 
!stress factor (swfac and swdf) and soil hospitality factor. Potentially, root front progression (in cm)  
!is 10 to 20% of the daily thermal time accumulated by the plant limited by maximum depth of soil so, setting
!soil depth correctly is crucial to the simulation of water stress.
! WDB 10/22/03
if(gddcer < 275.0) then             ! JTR 6/17/94
   rtdep = rtdep + dtt*0.1*sqrt(shf(l)*amin1(swfac*2.0,swdf))
else
   rtdep = rtdep + dtt*0.2*sqrt(shf(l)*amin1(swfac*2.0,swdf))
endif
rtdep = amin1(rtdep, depmax)       
                     
!---A root length density factor used to calculate new root growth distribution (unitless)
!Sum up the root length density factor calculated earlier over the profile
rldf(l1) = rldf(l1)*(1.0-(cumdep-rtdep)/dlayr(l1))      
trldf = 0.0
do l = 1, l1
   trldf = trldf + rldf(l)
end do

!---KAD: Finally, compute root length volume in cm[roots]/cm3[soil]. Also apply the root survival factor here
if(trldf >= rlnew*1E-5) then
   !rnlf = rlnew/trldf
   do l = 1, l1
      rlv(l) = rlv(l) + (rldf(l)/trldf) * rlnew/dlayr(l) - 0.005*rlv(l)
      !Round off to nearest 1/1000th place
      rlv(l) = rlv(l) * rtsurv
  !    rlv(l) = real(int(rlv(l)*1000.))/1000.  
      rlv(l) = amax1(rlv(l), 0.0)
      rlv(l) = amin1(rlv(l), 4.0)
   end do
end if

!----------------------------------------------------------------------
! Dynamic = output
!----------------------------------------------------------------------
else if(dynamic == output) then

! {no procedures to date}

end if !dynamic loop


return
end subroutine MZ_AG_RootGro


!==============================================================================================================================
! Variable definitions                                                                                       Unit 
!------------------------------------------------------------------------------------------------------------------------------
! control          Constructed type for control variables
! cumdep           Cumulative depth, to bottom of soil layer                                                 cm
! datafileio       Constructed type for variables read from DSSAT's input/output file (DSSAT45.INP)
! dataspecies      Constructed type for variables read from species file
! depmax           Depth of soil                                                                             cm
! dlayr(l)         Soil thickness in layer l                                                                 cm
! ds(l)            Depth to bottom of layer l                                                                cm
! dtt              Growing degrees occurring today (Base 8C), C      
! dul(l)           Volumetric soil water content at Drained Upper Limit in soil layer l                      cm3/cm3
! dynamic          Modular control (runinit=1 for run initialization, seasinit=2 for seasonal initialization,
!                  rate=3 for rate calculations, integr=4 for integration of state variables, output=5 for
!                  writing daily outputs, seasend=6 for closing output files                                 
! esw(l)           Extractable water in soil layer l                                                         cm
! files            Species file name
! gddcer           Cumulative daily thermal time after planting (CERES method)                               degree-day                                                            
! grort            Root growth rate                                                                          g/plant/day
! growthStage      Integer value of growth stage
! gstdyrdoySim(i)  Simulated year and day of year for growth stage i                                         yyyyddd
! istage           Crop growth stage (1-9)
! iswnit           Switch indicating if soil nitrogen balance is on (Y/N)
! iswitch          Constructed type for control switches
! l                Loop counter
! l1               Loop counter
! ll(l)            Volumetric lower limit of soil water in soil layer l                                      cm3/cm3
! nh4(l)           Ammonium in soil layer l                                                                  ppm
! nlayr            Number of soil layers
! no3(l)           Nitrate in soil layer l                                                                   ppm
! pathsr           Path to species file
! pltpop           Plant density                                                                             plants/m2
! pormin           Minimum pore space volume required for supplying oxygen to roots for 
!                  optimum growth and function (0-100%)
! rldf(l)          A root length density factor for soil layer l used to calculate new root 
!                  growth distribution
! rlnew            New root length to be added to the total root system length                               cm[root]/cm2[ground]
! rlv(l)           Root length density of layer l                                                            cm[root]/cm3[soil]
! rlwr             Root length to weight ratio                                                               cm/g
! rnfac            Zero to unity factor describing mineral N availability effect on root growth in Layer l
! rnlf             Intermediate factor used to calculate distribution of new root growth in the soil, 
!                  value between 0 and 1
! rtdep            Rooting depth, initially set at emergence                                                 cm
! rtexf            Fraction root death per day under oxygen depleted soil 
! rtsurv(l)        Fraction survival of roots on a given day, taking into account death due to 
!                  excess or deficit water conditions 
! sat(l)           Volumetric soil water content at saturation in soil layer l                               cm3/cm3
! sdepth           Sowing depth                                                                              cm
! shf(l)           Relative root distribution in soil layer l (0-1)
! SoilProp         Constructed type for variables related to soil properties
! sw(l )           Volumetric soil water content of soil layer l                                             cm3/cm3
! swdf             Soil water deficit factor used to calculate root growth and water uptake,
!                  value between 0 and 1
! swexf            Excess water stress factor for layer with deepest roots (0-1) 
! swfac            Soil water stress effect on growth (0-1), 1 is no stress, 0 is full stress  
! trldf            An intermediate calculation used to calculate distribution of new root growth in soil
! yrdoy            Year and day of year                                                                      yyyyddd 
!==============================================================================================================================
