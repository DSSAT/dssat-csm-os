!=======================================================================
! Respiration subroutine for AgMaize by Thijs Tollenaar
! Calculates maintenance respiration
!-----------------------------------------------------------------------
! REVISION HISTORY
! Originally written by Thijs Tollenaar
! 06/17/2013 Translated into Fortran - Kofikuma Dzotsi
!-----------------------------------------------------------------------
!  Called from: MZ_AG_AGMAIZE   
!  Calls:       None   
!=======================================================================
subroutine MZ_AG_Respiration(control, weather,     & !DSSAT inputs
           dvs,                                    & !From Phenology
           wlvtop, wlvbot, wst, weolg, wrt, cgrw,  & !From Growth
           maint)                                    !Outputs
   
!-----------------------------------------------------------------------
use ModuleDefs
implicit none
save
!-----------------------------------------------------------------------
real, parameter :: qten=2.0
integer :: dynamic
real :: tmax, tmin, tmpa, dvs, wlvtop, wlvbot, wst, weolg, wrt, cgrw, teff, maint

! Constructed variables based on definitions in ModuleDefs.for   
type(ControlType), intent(in) :: control
type(WeatherType), intent(in) :: weather

! Transfer values from constructed data types into local variables
dynamic = control % dynamic
tmax    = weather % tmax
tmin    = weather % tmin

!-----------------------------------------------------------------------
! Dynamic = runinit
!-----------------------------------------------------------------------
if(dynamic == runinit .or. dynamic == seasinit) then
maint  = 0.0
wlvtop = 0.0
wlvbot = 0.0
wst    = 0.0
weolg  = 0.0
wrt    = 0.0
cgrw   = 0.0

!-----------------------------------------------------------------------
! Dynamic = integr
!-----------------------------------------------------------------------
else if(dynamic == integr) then   

! Effect of temperature on maintenance respiration
tmpa = (tmax + tmin)/2.0       !Daily average air temperature
teff = qten**(0.1*tmpa - 2.5)  !No temperature effect at 25 degrees C (i.e. teff = 1.0)

! Maintenance respiration
if(dvs > 2.0 .or. cgrw <= 0.0) then
   maint = 0.0
else
   maint = teff*(0.03*wlvtop + 0.015*(wlvbot+wst) + 0.01*weolg + 0.01*wrt)
end if
  
!-----------------------------------------------------------------------
! End of dynamic if structure
!-----------------------------------------------------------------------  
end if     

!-----------------------------------------------------------------------
! End of subroutine
!-----------------------------------------------------------------------    
return
end subroutine MZ_AG_Respiration

	  
!==============================================================================================================================
! Variable definitions                                                                                  Unit 
!------------------------------------------------------------------------------------------------------------------------------
! cgrw	    Mean crop growth rate during the previous week                                              kg[dm]/ha/day
! control   Constructed type for control variables
! dynamic   Modular control (runinit=1 for run initialization, seasinit=2 for seasonal initialization,
!           rate=3 for rate calculations, integr=4 for integration of state variables, output=5 for
!           writing daily outputs, seasend=6 for closing output files                                 
! qten      Factor accounting for increase in maintenance respiration for a 10-oC change in temperature 
! maint     Maintenance respiration	                                                                    kg[glucose]/ha/day 
! teff      Effect of temperature on maintenance respiration                                            fraction  
! tmax      Maximum air temperature for current day                                                     degree-C
! tmin      Minimum air temperature for current day                                                     degree-C
! tmpa      Mean air temperature for current day                                                        degree-C
! weather   Constructed type for weather variables
! weolg	    Weight of ears at onset of linear grain filling period                                      kg[dm]/ha             
! wlvbot    Weight of leaves below top layer (wlv - wlvtop)                                             kg[dm]/ha                
! wlvtop    Weight of leaves in top layer (lai <= 2)                                                    kg[dm]/ha
! wrt       Weight of roots                                                                             kg[dm]/ha                       
! wst       Weight of stems                                                                             kg[dm]/ha                
!==============================================================================================================================
 