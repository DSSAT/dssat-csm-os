!==============================================================================
! SALUS Generic nutrient uptake subroutine for N and P!	
!==============================================================================
subroutine SALUS_NPuptake(dynamic, rwu, RootLayerFrac, dae,    & !Input
     Biomass, BiomassRoot, EConcOpt_par, ESupplyLayer_Kg, 	   & !Input
     reltt, relttemerge, soilprop,		                       & !Input
     EConcAct, EConcMin, EConcOpt, ESupplyLayerRed_Kg,  	   & !Output
     ESupplyTot_Kg, EDemand_Kg, EPlant_Kg, dEUptakeLayer_Kg,   & !Output
     SWaterFac, EStressFac)                                      !Output

Use ModuleDefs
implicit none
save 

integer :: layer, dae, dynamic, nlayr
real, parameter :: MinConcFrac = 0.6, gpm2_to_kgpha = 10.0, EMinAnyPool = 5.0
real, dimension(nl) :: dEUptakeLayer_Kg, ESupplyLayer_Kg, ESupplyLayerRed_Kg, SRedFac, RootLayerFrac, rwu, dlayr
real :: Biomass, BiomassRoot, dEUptake_Kg, EConcAct, EConcOpt, EConcMin, EConcOpt_par(3), EDemand_Kg, ESupplyTot_Kg
real :: EPlant_Kg, EStressFac, Plant_Kg, RELTT, RelTTEmerge, EUptake_Kg, SWaterFac, SAL_PValue 
Type(SoilType) soilprop

!------------------------------------------------------------------------------
! INITIALIZATION AND INPUT DATA
!------------------------------------------------------------------------------
if(dynamic==runinit .OR. dynamic==seasinit) then
!------------------------------------------------------------------------------
nlayr = soilprop % nlayr
dlayr = soilprop % dlayr 
ESupplyLayer_Kg = 0.0
ESupplyTot_Kg = 0.0
EStressFac = 1.0

!------------------------------------------------------------------------------
! INTEGRATION
!------------------------------------------------------------------------------
else if(dynamic == integr) then
!------------------------------------------------------------------------------

! Compute total plant mass
Plant_Kg = (Biomass + BiomassRoot) * gpm2_to_kgpha

! Nutrient Concentration interpolation (after emergence, integ)
EConcOpt =  SAL_PValue(RELTT,RelTTEmerge,EConcOpt_par)
EConcMin =  EConcOpt * MinConcFrac
   
! Water and Root Factor to Reduce Supply   
do layer = 1, nlayr
   SWaterFac = min(1.0, (1.0 / 0.005) * (RWU(layer)) / DLAYR(layer))
   SRedFac(layer) = min(RootLayerFrac(layer), SWaterFac)
   ESupplyLayerRed_Kg(layer) = SRedFac(layer) * ESupplyLayer_Kg(layer) 
end do  
ESupplyTot_Kg = ESupplyTot_Kg + sum(ESupplyLayerRed_Kg)

! The first day after emergence assume that live nutrient content eq. optimum (some nutrient comes from the seed):
if(dae <= 1) then
  EPlant_Kg = EConcOpt * Plant_Kg ! comes from seed
  EConcAct = EConcOpt
  return
end if

! Compute Demand
EDemand_Kg = EConcOpt * Plant_Kg - EPlant_Kg
	  
! Compute Uptake Rate
dEUptake_Kg = min(ESupplyTot_Kg, EDemand_Kg)

! Uptake per layer, kg ha-1:
do layer = 1, nlayr
   if(ESupplyTot_Kg > 0.0) then
      dEUptakeLayer_Kg(layer) = min((ESupplyLayer_Kg(layer) / ESupplyTot_Kg) * dEUptake_Kg, EMinAnyPool)
      EUptake_Kg = EUptake_Kg + dEUptakeLayer_Kg(layer)  !Add N from fixation
   end if
end do
 	
! Total Plant Nutrient Mass
EPlant_Kg = EPlant_Kg + dEUptake_Kg
 	
! Plant concentration of nutrient
if(Plant_Kg > 0.0) EConcAct = EPlant_Kg / Plant_Kg
 
! Compute stress factor	   
EStressFac = 1.0
!!EStressFac = 1 - (((EConcAct - EConcMin) / (EConcOpt - EConcMin)) - 1)**4
!!EStressFac = max(EStressFac, 0.0)   	
 
!------------------------------------------------------------------------------
! End of dynamic 'IF' construct
!------------------------------------------------------------------------------
end if
!------------------------------------------------------------------------------
 	
return
end subroutine SALUS_NPuptake	
	

!------------------------------------------------------------------------------
! Variable definitions - updated 
!------------------------------------------------------------------------------
!Biomass: Total plant dry matter weight (g m-2)
!BiomassRoot: Root dry matter weight (g m-2)	
!dEUptake_Kg: Actual total uptake rate (kg ha-1)
!dEUptakeLayer_Kg: Uptake per layer (kg ha-1)
!EConcAct: Actual whole plant nutrient concentration (fraction)
!EConcMin: Minimum whole plant nutrient concentration (fraction)
!EConcOpt: Optimum whole plant nutrient concentration (fraction)
!EConcOpt_par: Optimum whole plant nutrient concentration (fraction) at three stages during growth
!EDemand_Kg: Whole plant demand (kg ha-1)
!EMinAnyPool: Minimum quantity in any pool, to avoid negative labile nutrient (kg ha-1)
!ESupplyLayer_Kg: Mineral supply by layer (kg ha-1)
!ESupplyTot_Kg: Total mineral supply (kg ha-1)
!EPlant_Kg: Nutrient mass in total plant dry matter (kg ha-1)
!EStressFac: Stress factor
!gpm2_to_kgpha: g m-2 to kg ha-1 conversion factor
!MinConcFrac: Fraction of optimal whole plant nutrient concentraction that is considered to be minimum concentration at different growth stages
!NLAYR: Number of soil layers
!Plant_Kg: Total plant dry matter (aboveground and roots)
!RELTT: Relative thermal time during life of plant (0-1)
!RelTTEmerge: Relative thermal time to emergence
!SAL_PValue: Function to interpolate optimum and minimum nutrient concentrations
!------------------------------------------------------------------------------

