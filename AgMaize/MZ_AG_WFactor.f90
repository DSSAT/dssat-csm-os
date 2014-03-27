!=======================================================================
! Compute water stress factor for AgMaize
!-----------------------------------------------------------------------
! REVISION HISTORY
! 07/25/2013 KAD
!-----------------------------------------------------------------------
!  Called from: MZ_AG_Growth  
!  Calls:         
!=======================================================================
subroutine MZ_AG_WFactor(weather, iswitch, eop, trwup, cgr, rwuep1,   &   !Input 
                         swfac, turfac)                                   !Output
   
!-----------------------------------------------------------------------
use ModuleDefs
implicit none
save
!-----------------------------------------------------------------------
character(len=1) :: iswwat, meevp
real :: cgr, eop, ep1, trwup, esatTmax, tmax, rh, rhmin, vpdmax, vpdday, wdemand, wue, swfac, turfac, rwuep1

! Constructed types
type(WeatherType) :: weather
type(SwitchType)  :: iswitch
tmax   = weather % tmax
rh     = weather % rhum
iswwat = iswitch % iswwat
meevp  = iswitch % meevp
!-----------------------------------------------------------------------

! Calculate daily water stess factors 
swfac = 1.0
turfac = 1.0
if(iswwat /= 'N') then
   if(meevp /= 'Z') then
      if(meevp == 'R') then
         !Priestly-Taylor ('R') method for computing plant transpiration
         if(eop > 0.0) then
            ep1 = eop * 0.1
            if(ep1 >= trwup) swfac = trwup / ep1
            if(trwup/ep1 < rwuep1) turfac = (1.0/rwuep1) * trwup / ep1
         end if
         turfac = real(int(turfac*1000))/1000
      else
         !08/07/2013. Default method when neither 'Z' nor 'R' are selected (needs rhmin as inputs, add later)
         !Equations obtained from http://agsys.cra-cin.it/tools/evapotranspiration/help/Maximum_vapour_pressure_deficit.html
         esatTmax = 0.6108*exp(17.27*tmax/(tmax+237.3)) 
         if(rh > 0.0) then
            rhmin = rh
         else   
            rhmin = 50.0    !KAD: If rh missing (-99) assume 50%
         end if
         vpdmax = esatTmax*(1-rhmin/100.0)
         vpdday = 0.67*vpdmax              !From Stockle
         wue = 7.44 * vpdday**(-0.42)      !g/kg, Kremer et al. 2008
         !Conversion: 1 kg[water]/m2[ground] = 0.001 m3[water]/m2[ground] = 1 mm water height
         wdemand = cgr*0.1/wue       !Convert dry matter kg/ha to g/m2. Get wdemand in kg[water]/m2/day or mm/day
         wdemand = wdemand*0.1       !From mm to cm
         if(wdemand > 0.0 .AND. wdemand >= trwup) swfac = trwup/wdemand
      end if   
   end if
end if

return
end subroutine MZ_AG_WFactor



!==============================================================================================================================
! Variable definitions                                                                                  Unit 
!------------------------------------------------------------------------------------------------------------------------------
! eop       Potential plant transpiration                                                               mm/day
! ep1       Potential plant transpiration                                                               cm/day
! esatTmax  Saturation vapor pressure at maximum temperature                                            kPa
! iswitch   Constructed type for control switches
! iswwat    Water balance simulation switch (Y/N)
! meevp     Method of evapotranspiration (P=Penman, R=Priestly-Taylor, Z=Zonal) 
! rh        Minimum relative humidity                                                                   %
! rhmin     Minimum relative humidity                                                                   %
! rwuep1
! swfac     Soil water stress effect on growth (0-1), 1 is no stress, 0 is full 
! cgr       Crop growth rate (aboveground dry matter)                                                kg[dm]/ha/day
! tmax      Maximum daily temperature                                                                   degree C
! trwup     Total potential daily root water uptake                                                     cm/day
! turfac
! vpdmax    Maximum vapor pressure deficit                                                              kPa
! vpdday    Daytime vapor pressure deficit                                                              kPa
! wdemand   Water demand                                                                                Check, should be in cm
! weather   Constructed type for weather variables
! wue       Water use efficiency                                                                        g[dm]/kg[water]
!==============================================================================================================================
	  
