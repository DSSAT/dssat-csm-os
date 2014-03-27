!=======================================================================
! Partitioning Coefficients subroutine for AgMaize by Thijs Tollenaar
! Calculates coefficients for partitioning of assimilates to 
! vegetative parts
!-----------------------------------------------------------------------
! REVISION HISTORY
! Originally written by Thijs Tollenaar
! 07/11/2013 Translated into Fortran - Kofikuma Dzotsi
!-----------------------------------------------------------------------
!  Called from:   
!  Calls:         
!=======================================================================
subroutine MZ_AG_PartCoefs(control, weather, yrplt,   & !Inputs
           silk, tlu, dvs, olg,                       & !Inputs from Phenology
           pstres,                                    & !Inputs from Growth
           fr, fsts, flv, fvear)                        !Outputs
   
!-----------------------------------------------------------------------
use ModuleDefs
implicit none
save
!-----------------------------------------------------------------------
integer, parameter :: tlag=14
integer, intent(in) :: yrplt
real, parameter,dimension(4) :: xdvsleaves  = [0.0, 0.6, 0.8, 1.0],   &
                                yfracleaves = [0.6, 0.5, 0.4, 0.0]
real :: tmax, tmin, mtp, silk, tlu, dvs, olg, pstres, fr4, fr8, fr12, fr, fsts, flv, fvear
real :: delayt(tlag+1), xtluroots(5), yfracroots(5), xdvsstems(3), yfracstems(3), xdvsears(3), yfracears(3)

! Other variables 
integer :: dynamic, yrdoy, i

! Functions
integer :: timdif
real    :: alin

! Constructed variables based on definitions in ModuleDefs.for   
type(ControlType), intent(in) :: control
type(WeatherType), intent(in) :: weather

! Transfer values from constructed data types into local variables
dynamic = control % dynamic
yrdoy   = control % yrdoy
tmax    = weather % tmax
tmin    = weather % tmin

!-----------------------------------------------------------------------
! Dynamic = runinit
!-----------------------------------------------------------------------
if(dynamic==runinit .or. dynamic==seasinit) then
delayt = 0.0
fr = 0.0
fsts = 0.0
flv = 0.0
fvear = 0.0
pstres = 0.0

!-----------------------------------------------------------------------
! Dynamic = rate
!-----------------------------------------------------------------------
else if(dynamic==rate) then   


!-----------------------------------------------------------------------
! Dynamic = integr
!-----------------------------------------------------------------------
else if(dynamic==integr) then

! Mean air temperature over the last 14 days
delayt(tlag+1) = (tmax + tmin)/2.   
delayt(1:tlag) = [(delayt(i+1), i=1,tlag)]
if(yrdoy > yrplt) then
   mtp = sum(delayt(1:tlag)) / min(timdif(yrplt,yrdoy),tlag)
else
   mtp = delayt(tlag)
end if   

! Effect of stage of development on partitioning to roots (fr)
fr4  = 0.6090 - 0.01112 * mtp + 0.0001875 * mtp**2
fr8  = 0.7076 - 0.02262 * mtp + 0.0003549 * mtp**2
fr12 = 0.6281 - 0.02765 * mtp + 0.0004766 * mtp**2
xtluroots(1:5)  = [4.0, 8.0, 12.0, silk, silk+6.0]          !This is Fortran 2003 syntax
yfracroots(1:5) = [fr4, fr8, fr12, 0.15, pstres]
fr = alin(xtluroots, yfracroots, size(xtluroots), tlu)      !There was an effect of leaf mutual shading (lapd) omitted
  
! Effect of stage of development on partitioning to leaves (flv)
flv = alin(xdvsleaves, yfracleaves, size(xdvsleaves), dvs)
  
! Effect of stage of development on partitioning to structural stem (fsts)
xdvsstems  = [0.85, 1.0, 1.3]
yfracstems = [1.00, 0.5, 0.0]
fsts = alin(xdvsstems, yfracstems, size(xdvsstems), dvs)
fsts = max(0.0, fsts)
  
! Effect of stage of development on partitioning to non grain ear (fvear)
xdvsears  = [0.75, 1.0, olg]
yfracears = [0.00, 0.4, 0.9]
fvear = alin(xdvsears, yfracears, size(xdvsears), dvs)
  
!-----------------------------------------------------------------------
! End of dynamic if structure
!-----------------------------------------------------------------------  
end if     

!-----------------------------------------------------------------------
! End of subroutine
!-----------------------------------------------------------------------    
return
end subroutine MZ_AG_PartCoefs


!==============================================================================================================================
! Variable definitions                                                                                  Unit 
!------------------------------------------------------------------------------------------------------------------------------
! alin        [Function] For performing linear interpolations of a y-variable given an x-variable      
! control     Constructed type for control variables
! delayt      Vector of mean air temperatures of the last 15 days                                       degree C
! dvs         Development stage (0 is planting, 1 is silking, and 2 is maturity) 
! dynamic     Modular control (runinit=1 for run initialization, seasinit=2 for seasonal initialization,
!             rate=3 for rate calculations, integr=4 for integration of state variables, output=5 for
!             writing daily outputs, seasend=6 for closing output files                                 
! flv         Proportion of assimilates partitioned to leaves                                           fraction
! fvear	      Effect of stage of development on partitioning to non-grain ear part                      fraction
! fr          Proportion of assimilates partitioned to roots                                            fraction
! fr4         Proportion of assimilates partitioned to the roots at 4-leaf stage                        fraction
! fr8         Proportion of assimilates partitioned to the roots at 8-leaf stage                        fraction
! fr12        Proportion of assimilates partitioned to the roots at 12-leaf stage                       fraction
! fsts        Proportion of assimilates partitioned to structural stems                                 fraction
! i           Integer loop counter
! mtp         Mean air temperature over the last 14 days                                                degree C
! olg         Onset linear dry matter accumulation of grain                                             dvs
! pstres      Fraction of stem reserves                                                                 fraction
! silk        Leaf stage at silking                                                                     tlu 
! timdif      [Function] For computing difference between two dates                                     returns number of days 
! tlag        Parameter defining a lag of 14 days
! tlu         Thermal leaf unit (cumulative rate of leaf tip appearance)                                tlu
! tmax        Maximum daily temperature                                                                 degree C
! tmin        Minimum daily temperature                                                                 degree C
! weather     Constructed type for weather variables
! xdvsears    Vector of dvs values used for interpolating ear partitioning coefficients                 dvs
! xdvsleaves  Vector of dvs values used for interpolating leaf partitioning coefficients                dvs
! xdvsstems   Vector of dvs values used for interpolating stem partitioning coefficients                dvs
! xtluroots   Vector of tlu values used for interpolating root partitioning coefficients                tlu
! yfracears   Vector of proportion of assimilates partitioned to ears                                   fraction
! yfracleaves Vector of proportion of assimilates partitioned to leaves                                 fraction
! yfracroots  Vector of proportion of assimilates partitioned to leaves                                 fraction
! yfracstems  Vector of proportion of assimilates partitioned to stems                                  fraction
! yrdoy       Year day-of-year                                                                          yyyyddd
! yrplt       Planting date                                                                             yyyyddd
!==============================================================================================================================
	  



