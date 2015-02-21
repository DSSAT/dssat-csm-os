!=========================================================================================================
! Various subroutines for the simple SALUS crop model
! Subroutine BiomassIncrement
! Computes biomass increment resulting from growth
! Need to add CO2 Effect on biomass increment

subroutine BiomassIncrement(plantpop, rowspacing, srad,  & !Input
  droughtfac, heatfac, coldfac, nfac, pfac, rue, xhlai,  & !Input
  kcan, biomassinc)                                        !Output

use ModuleDefs
implicit none 
save

real :: plantpop, rowspacing, srad, droughtfac, heatfac, coldfac, nfac, pfac, rue, xhlai, kcan, biomassinc
real, parameter::parfr = 0.5

! Row spacing must be in cm, biomassinc in g/m2/day
kcan = 1.5 - 0.768*(((rowspacing*0.01)**2)*plantpop)**0.1
biomassinc = rue*parfr*srad*(1.0 - exp(-kcan*xhlai))* min(droughtfac,heatfac,coldfac,nfac,pfac)

return
end subroutine BiomassIncrement
!=========================================================================================================


!=========================================================================================================
! Subroutine RadiationUseEfficiency
! Calculate Radiation Use Efficiency based on the simple two-part function in ALMANAC.

subroutine RadiationUseEfficiency(relttsn, ruemax, snparrue, reltt, droughtfac,   & !Input
   heatfac,coldfac,nfac,pfac, stresrue,                                           & !Input
   relrue, rue)                                                                     !Output

use ModuleDefs
implicit none 
save   

real :: relttsn, ruemax, snparrue, reltt, relrue, rue, droughtfac, heatfac
real :: coldfac, nfac, pfac, snparrues, stresrue
 
! First, compute relative RUE
if(reltt < relttsn) then
   relrue = 1.0
   rue = ruemax   
else !After senescence starts, RUE declines due to leaf senescence
   if(relttsn < 1.0) then
      !KAD Added 02/20/2011, similar modification as for LAI senescence
      snparrues = stresrue*snparrue + (snparrue-stresrue*snparrue)* min(droughtfac,heatfac,coldfac,nfac,pfac)
      relrue = ((1.001 - reltt)/(1.0 - relttsn))**snparrues
   else
      !print*,"RelTT at start of senescence must be less than 1"
   end if

   rue = min(rue, relrue*ruemax)
end if
      
!a*SNPARRUE_|x
!           |     x
! SNPARRUES |          x  
!           |               x                            
!  SNPARRUE_|_ _ _ _ _ _ _ _ _ _x
!           |___________________|
!           0     STRESS        1 
! LAI senesces a (a = STRESRUE) times faster at maximum stress reducing      
! the efficiency of use of radiation by the same factor.
      
return
end subroutine RadiationUseEfficiency
!=========================================================================================================


!=========================================================================================================
! Subroutine DailyThermalTime
! Calculates daily accumulated thermal time that drives plant phenology

subroutine DailyThermalTime(tmax, tmin, tbasedev, toptdev, ttmature,  & !Input
   teff, reltt, dtt, cumtt)                                             !Output

implicit none   
      
real :: tbasedev, toptdev, teff, dtt, ttmature, reltt, tmax, tmin, cumtt

if(tmax < tbasedev) then 
   dtt = 0. 
else     !If maximum temperature greater than base temperature
   if(tmin < tbasedev) then
      teff = (tbasedev + min(toptdev,tmax))/2
   else if(tmax > toptdev .AND. tmin < toptdev) then
      teff = (tmin + toptdev)/2
   else
      teff = (tmin + tmax)/2 
   end if
   dtt = teff - tbasedev
   cumtt = cumtt + dtt 
end if

if(ttmature <= 0.0) then
   reltt = reltt + 0.001
else
   reltt = reltt + dtt/ttmature
end if

return
end subroutine DailyThermalTime
!=========================================================================================================


!=========================================================================================================
! Subroutine Germination
! Uses thermal time to determine if germination has occurred

subroutine Germination(ttgerminate, ttmature, dtt, ttemerge,   & !Input
   ttaccumulator, killed, germinate)                             !Output

implicit none   
save
      
real :: ttgerminate, ttmature, dtt, ttaccumulator, ttemerge
logical :: killed, germinate

if(germinate) return
ttaccumulator = ttaccumulator + dtt

if(ttaccumulator >= ttgerminate) then 
   germinate = .true. 
   ttaccumulator = 0.0   !Set ttaccumulator back to zero after germination
   if((ttgerminate + ttemerge) > ttmature) then
      killed = .true.
   end if
end if      

return
end subroutine Germination
!=========================================================================================================


!=========================================================================================================
! Subroutine Emergence
! Uses thermal time to determine emergence status
!==============================================================================
subroutine Emergence(ttaccumulator, dtt, ttemerge, ttmature,      & !Input
   emerge)                                                          !Output
 
implicit none  
save 
      
real :: dtt, ttemerge, ttaccumulator, ttmature
logical :: emerge

if(emerge) return
ttaccumulator = ttaccumulator + dtt

if(ttaccumulator >= ttemerge) then 
   emerge = .true. 
   ttaccumulator = 0.0 !Set ttaccumulator back to zero after emergence
end if 

return
end subroutine Emergence
!=========================================================================================================


!=========================================================================================================
! Subroutine Maturity
! Uses thermal time to determine maturity status

subroutine Maturity(reltt, mature)

implicit none   
      
real :: reltt
integer mature
 
if(reltt >= 1.0) mature = 1 

return
end subroutine Maturity
!=========================================================================================================


!=========================================================================================================
! Subroutine LeafAreaIndex
! Computes canopy leaf area index
! Original Visual Basic Method (before senescence)
!          deltaLAI = RELLAI*LAIMAX - XHLAI
!          RELLAIS = (XHLAI + deltaLAI * MIN(DROUGHTFAC,HEATFAC,COLDFAC,NFAC,PFAC)) / LAIMAX
!          RELLAIMAX = RELLAIS
!          XHLAI = RELLAIS * LAIMAX
! Kofikuma Dzotsi 12/15/2010- cleaner implementation of original version
!          deltaLAI = (RELLAI*LAIMAX - XHLAI)* MIN(DROUGHTFAC,HEATFAC,COLDFAC,NFAC,PFAC)
! Original method after senescence
!                            This was the LAI just before senescence  
!                                       ______|_______   
!                                      |              |                     
!          XHLAI = MIN(XHLAI, RELLAI*LAIMAX*RELLAIMAX) 

subroutine LeafAreaIndex(laimax, laip1, laip2, relttsn, snparlai, droughtfac,   & !Input
   heatfac, coldfac, nfac, pfac, reltt, xhlai, relttsn2, streslai,              & !Input
   rellai, deltalai, rellais, rellaimax, rellaiyest)                              !Output
!-----------------------------------------------------------------------
IMPLICIT NONE         

real :: laimax, laip1, laip2, relttsn, snparlai, droughtfac, heatfac, coldfac, nfac, pfac
real :: reltt, rellai, xhlai, dlai, rellais, rellaimax, deltalai, rellaiyest
! 02/21/2011
real snparlais   !New senescence parameter
real streslai    !Factor by which senescence is increased at maximum water stress
real relttsn2    !Relative thermal time beyond which plant is no longer sensitive to water stress
 
if(reltt <= relttsn) then
   if((reltt + exp(laip1 - laip2*reltt)) > 0.0) then
      rellai = reltt/(reltt + exp(laip1 - laip2*reltt))

      !version2-Kofikuma 12/16/2010- Modification to original visual basic version:
      !Potential rate of LAI increase is given by (RELLAI-rellaiyest)*LAIMAX
      !Simply apply the stress factors to this rate. Avoid computing the rate
      !based on yesterday's LAI itself because always increasing rate after a stress.
      deltalai = (rellai-rellaiyest)*laimax * min(droughtfac,heatfac,coldfac,nfac,pfac)
   end if

   xhlai = xhlai + deltalai
   rellaimax = xhlai/laimax
                     
else !LAI declines 
   if((relttsn < 1.0) .and. (relttsn2 < 1.0)) then
      !version3-02/18/2011 Modification after discussion in Rm 201 with Bruno Basso and Joe Ritchie.
      !Have two SNPARLAI parameters: the one that determine the shape before RelTTSn2 = 0.70 and 
      !is influential by water stress; and another one that takes on the value from the crop
      !parameter file is not influenced by water stress. In other words the effect of water stress
      !on LAI, occurring beyond RelTTSn2 is not modeled.    
      if((reltt <= relttsn2) .and. (relttsn2 >= relttsn)) then
         snparlais = streslai*snparlai + (snparlai-streslai*snparlai)* min(droughtfac,heatfac,coldfac,nfac,pfac)
      else
         snparlais = snparlai
      end if
  
      rellai = ((1.001 - reltt)/(1.0 - relttsn))**snparlais 
   else
      !print*,"rel.tt at start of senescence must be less than 1"
   end if
  
   !version2-Kofikuma 12/16/2010 Modification to original version to account for stress
   !occuring during the phase of LAI decline
   !Smooth out curve at transition between LAI increase and LAI decline
   rellaiyest = max(rellaiyest, rellai)
   deltalai = (rellai-rellaiyest)*laimax*rellaimax

   !02/17/2011 - Prevent LAI from being negative during the decline phase
   if(xhlai+deltalai < 0.0) then
     xhlai = max(xhlai+deltalai, xhlai)
   else
     xhlai = xhlai + deltalai
   end if
           
end if

return
end subroutine LeafAreaIndex
!=========================================================================================================


!=========================================================================================================
! Subroutine Partitioning
! Partitions assimilates among organs, only tops and roots here
! This was the old function that was partitioning too much to the roots:
! ROOTPARTCOEFF = 1.5*(RELTT**2) - 1.9*RELTT + 0.8
! The new one (below, email with Bruno Basso, 05/10/2010) behaves better
! [Kofikuma Dzotsi, 05/10/2010]

subroutine partitioning(biomassinc, reltt,   & !Input
   rootpartcoeff, dbiomass, dbiomassroot)      !Output

implicit none   

real :: biomassinc, reltt, rootpartcoeff, dbiomass, dbiomassroot
real, parameter :: FRBforRT = 0.90      

! Swinnen et al., 1994
rootpartcoeff = 0.45 * exp(-1.904*reltt)
       
! The rest of new root C goes to soluble FOM:              
dbiomassroot = biomassinc * rootpartcoeff * FRBforRT
dbiomass = biomassinc * (1 - rootpartcoeff)
 
return
end subroutine Partitioning
!=========================================================================================================


!=========================================================================================================
! SUBROUTINE ENVIRONMENT
! Purpose: Evaluate the effect of extreme environmental conditions on
! crop growth. Stops crop model if any environmental extremes such as 
! frost, long-term cold, drought, or nutrient stress, etc. occurs.
! Variables
! CumSlowDev     = Cumulative slow development days (days) 
! CumSlowDevStop = Cumulative slow development days to stop crop model (days)
! dtt            = Daily thermal time increment (degree-days)
! killed         = True when crop is killed by adverse conditions
! reltt          = Relative thermal time during life of plant (0-1)
! relttsn        = Relative thermal time at beginning of senescence (0-1)
! tmin           = Daily minimum temperature (degree C)

subroutine Environment(reltt, relttsn, tmin, CumSlowDev, dtt, tfreeze,    & !Input
   CumSlowDevStop, killed, freezed)                                         !Output

implicit none 

integer CumSlowDev, CumSlowDevStop
real :: reltt, relttsn, tmin, dtt, tfreeze
logical killed, freezed

CumSlowDevStop = 20

! Stop simple crop model if frost after start of senescence (or max LAI):
! !IF (RELTT .GT. RELTTSN .AND. TMIN .LE. -10.0) THEN
! 8/25/2011 The above statement was the old condition. However, this condition was rarely met, esp.
! in cooler climate, which would let the crop grow for several months (more than a year!) and with
! continued artificial biomass accumulation. Therefore, a new parameter TFREEZE was introduced. Anytime
! during development when TMIN drops down to TFREEZE level, development stops, even before RELTTSN- KD
! Added new variable FREEZED to differentiate between freezing and slowed development
if(tmin <= tfreeze) then 
   freezed = .true.
   return
end if

! Check accumulation of an arbitrary number of slow development days (dTT < 0.1):
if(dtt <= 0.1) then
   CumSlowDev = CumSlowDev + 1
   if(CumSlowDev >= CumSlowDevStop) then
      killed = .TRUE.
      return
   end if
else
   CumSlowDev = 0
end if

return
end subroutine Environment
!=========================================================================================================


!=========================================================================================================
! Subroutine CONVERTB
! Convert above and below ground biomass from g m-2 to kg ha-1

subroutine convertb(biomass, biomassroot, grainyield,    & !Input
   biomassc, biomassrootc, grainyieldc)                    !Output
   
implicit none   

real :: biomass, biomassroot, grainyield, biomassc, biomassrootc, grainyieldc

biomassc = biomass * 10.0
biomassrootc = biomassroot * 10.0
grainyieldc = grainyield * 10.0 

return
end subroutine convertb
!=========================================================================================================


!=========================================================================================================
! Subroutine EstimateLaiPar
! Compute the potential LAI S-curve parameters
! In the original SALUS, this function is called Sigmoidal_Curve_Par

subroutine estimatelaipar(relttp1, relttp2, rellaip1, rellaip2,  & !Input
   laip1, laip2)                                                   !Output   

implicit none   
real :: relttp1, relttp2, rellaip1, rellaip2, laip1, laip2, xtemp

! Set RELTTP1 and RELTTP2 respectively to 0.15 and 0.50 so that
! LAIP1 and LAIP2 needed by the LAI subroutine can be calculated
! when RELLAIatP1 and RELLAIatP2 are read from the crop file.
! Kofikuma 07/23/2010
!  RELTTP1 = 0.15
!  RELTTP2 = 0.50
if(rellaip1 /= 0.0) then
   xtemp = log(relttp1 / rellaip1 - relttp1)
end if

if((rellaip2 /= 0.0) .AND. (relttp2 - relttp1 /= 0.0)) then
   laip2 = (xtemp - log(relttp2 / rellaip2 - relttp2)) / (relttp2 - relttp1)
end if

laip1 = xtemp + relttp1 * laip2

return
end subroutine EstimateLaiPar
!=========================================================================================================


!=========================================================================================================
!  Function SAL_PValue, Linearly interpolates daily optimum and minimum  
!  P values based on growth stage fractions.

Function SAL_PValue(RELTT, RelTTEmerge, PArray)

real :: SAL_PValue          !Interpolated value returned
Real PArray(3)              !Array of values to be interpolated
real :: RelTTEmerge, reltt
!
!
!                |
!      PArray(1)>+   x
!      PArray(2)>+            x
!                | 
!                |
!      PArray(3)>+                     x
!                |___________________________
!                    |        |        |
!                Emergence RelTT=0.5  Maturity            
!
!------------------------------------------------------------------
! Calculate optimum and minimum P concentrations in plant tissue.
!------------------------------------------------------------------
if(reltt == RelTTEmerge) then
   !Prior to emergence
   SAL_PValue = PArray(1)
else if (reltt < 0.5) then
   !First to second critical stage
   SAL_PValue = PArray(1) - (PArray(1) - PArray(2)) * (reltt - RelTTEmerge) * 2.0
else if (reltt < 1.0) then
   !Second to third critical stage
   SAL_PValue = PArray(2) - (PArray(2) - PArray(3)) * (reltt - 0.5) *2.
else
   !Subsequent to third critical stage to harvest
   SAL_PValue = PArray(3)
end if

SAL_PValue = MAX(0.0, SAL_PValue)

return
End Function SAL_PValue
!=========================================================================================================




!=========================================================================================================
! Variable definitions  
!=========================================================================================================
! BIOMASS           Total plant dry matter weight (g m-2)
! BIOMASSC          Total plant dry matter weight converted to kg ha-1 (kg ha-1)
! BIOMASSINC        Daily biomass increment (g m-2)
! BIOMASSROOT       Root dry matter weight (g m-2)
! BIOMASSROOTC      Root dry matter weight converted to kg ha-1 (kg ha-1)
! COLDFAC           Low temperature reduction factor (0-1)
! CONTROL           Composite variable containing variables related to control 
!                   and/or timing of simulation.  The structure of the variable 
!                   (ControlType) is defined in ModuleDefs.for. 
! CumSlowDev        Cumulative slow development days 
! CumSlowDevStop    Cumulative slow development days to stop crop model	
! DAE               Days after emergence
! DAP               Days after planting
! dBIOMASS          Incremental total plant dry matter weight (g m-2 d-1)
! dBIOMASSROOT      Incremental root dry matter weight (g m-2 d-1)
! dLAI              Daily increase in leaf area index (m2 m-2 d-1)
! DOY               Julian day
! DOYP              Date of planting (Julian day)
! DROUGHTFAC        Drought reduction factor (0-1)
! DTT               Daily thermal time increment (degree-days)
! EMERGE            Emergence flag; True when crop has emerged
! EMGINT            Intercept of emergence thermal time calculation
! EMGSLP            Slope of emergence thermal time calculation
! FRBforRT          Fraction of new root biomass for root growth
! FREEZED           Logical- True if TMIN <= TFREEZE     
! HEATFAC           High temperature reduction factor (0-1)
! ISWITCH           Composite variable containing switches which control flow of 
!                   execution for model.  The structure of the variable 
!                   (SwitchType) is defined in ModuleDefs.for. 
! KCAN              Canopy light extinction coefficient for daily PAR, for 
!                   equidistant plant spacing
! GERMINATE         Germination flag; True when crop has germinated
! KILLED            True when crop is killed by adverse conditions
! LAI               Canopy leaf area index (m2 m-2)
! LAIMAX            Maximum expected Leaf Area Index (m2 m-2)
! PLANTPOP          Plant population (m-2)
! MATURE            Maturity flag (0/1); 1 when crop has matured
! MDATE             Harvest maturity date (YYYYDDD)
! NFAC              Nitrogen deficiency factor (0-1)
! PARFR             Fraction of solar radiation useable by plants (PAR)
! PFAC              Phosphorus deficiency factor (0-1)
! RELLAI            Relative LAI (0-1)
! RELLAIP1          Relative LAI at point 1 on the potential LAI S-curve (0-1)
! RELLAIP2          Relative LAI at point 2 on the potential LAI S-curve (0-1)
! RELRUE            Relative RUE (0-1)
! RELTT             Relative thermal time during life of plant (0-1)
! RELTTP1           Relative thermal time at point 1 on the potential LAI S-curve (0-1)
! RELTTP2           Relative thermal time at point 2 on the potential LAI S-curve (0-1)
! RELTTSN           Relative thermal time at beginning of senescence (0-1)
! ROOTPARTCOEFF     Fraction of assimilate that goes to roots (0-1)
! ROWSPACING        Row spacing (cm)
! RUE               Radiation Use Efficiency (g MJ-1)
! RUEMAX            Maximum expected Radiation Use Efficiency (g MJ-1)
! SNPARLAI          Parameter for shape of potential LAI curve after beginning of senescence (0-1)
! SNPARRUE          Parameter for shape of potential RUE curve after beginning of senescence (0-1)
! SOWDEPTH          Sowing depth (cm)
! SRAD              Daily solar radiation (MJ m-2)
! TBASEDEV          Base temperature for development (degree C)
! TEFF              Daily effective temperature (degree C)
! TFREEZE           Freezing temperature in the sense that if TMIN <= TFREEZE, growth stops (deg C)
! TMAX              Daily maximum temperature (degree C)
! TMIN              Daily minimum temperature (degree C)
! TOPTDEV           Optimum temperature for development (degree C)
! TTGERMINATE       Thermal time planting to germination (degree-days)
! TTEMERGE          Thermal time germination to emergence (degree-days)	
! TTMATURE          Thermal time planting to maturity (degree-days)
! XHLAI             Healthy leaf area index (m2[leaf] / m2[ground])
