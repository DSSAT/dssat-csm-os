!=======================================================================
!  MZ_AG_Photsynt
!  Computes canopy daily gross photosynthesis (g biomass/m2 d) by 
!  integrating each leaf contribution over hourly time steps in the day.
!  Adapted from ETPHOT by N.B. Pickering
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  02/07/2003 JIL Written
!  09/12/2007 JIL Modified and adapted for IXIM model
!  03/08/2013 TT/SK/KAD Adapted for AgMaize, respiration part removed
!-----------------------------------------------------------------------
!  Called from: MZ_AG_AGMAIZE
!  Calls:       MZ_AG_Radabs, MZ_AG_Iphotsynt
!  Questions for Jon:
!  -What is cans (= 4.5) in the equation for canopy height? Equation here different from the one in paper
!  -Canopy width cannot be larger than row spacing?
!
!  Questions for CHP
!  -azir is azimuth relative to North while azzon is relative to South?
!
! TODO:
! -Error message for rowsp or pltpop = 0.0
!=======================================================================
subroutine MZ_AG_Photsynt(control, soilprop, weather, sw1,   &
           gddae, dvs,                                       &   !From Phenology
           lfn, greenla, lflon, lamaxpot, lapot,             &   !From Leaf area
           pgross, srad, parday, parsunday, parshday, radtotday) !Outputs                                             

use ModuleDefs
use MZ_AG_ModuleDefs
implicit none
save

!----------------------------------------------------------------------
! Define variables
!----------------------------------------------------------------------
integer, parameter :: lenla = 50  
real, parameter :: &
cans  = 4.5,       &
scvp  = 0.2,       &
tincr = 24.0/ts,   &
insp  = 4.514,     &
as    = 288.615,   &
cvx   = -75.774,   &
pi    = 3.14159,   &
rad   = pi/180.0,  &
pang  = 64.0,      &
cm2tom2 = 1.0E-4
integer, intent(in) :: lfn
real, intent(in)  :: gddae, dvs
real, intent(in), dimension(lenla)  :: lflon, lamaxpot    
real, intent(out) :: pgross
                        
! Constructed variables based on definitions in ModuleDefs.for   
type(ControlType), intent(in) :: control
type(WeatherType), intent(in) :: weather
type(SoilType),    intent(in) :: soilprop                    
type(FileioType)  :: datafileio
type(SpeciesType) :: dataspecies

! Variables from constructed types 
integer :: dynamic, yrdoy
real, dimension(ts) :: amtrh, tairhr, azzon, beta, frdifp, parhr
real :: salb, dul1, sw1, snup, sndn, azir, pltpop, rowspcm, rowspmeter
real :: asmax, canh, xc    !Photosynthesis parameters from species file

! KAD 09/24/2013 - For checking radiation subroutine
real,dimension(ts) :: parsunHour, parshHour, radtotHour
real :: srad, parday, parsunday, parshday, radtotday

! Other variables
character(len=12) :: files
character(len=80) :: pathsr
integer :: i, h, leafnum(lenla)
logical :: light, daytim
real :: lfln, lflp, canw, canwh, hs, turfac, mult, amplt, froll, palb, palbd
real :: pg, pghr, betn, canht, hlai, pgday, radtot
real,dimension(lenla) :: plaisl, plaish, parsh, parsun, lapot, greenla
real,dimension(lenla) :: arefhr, adifhr, addrhr, addfhr

! Transfer values from constructed data types into local variables
dynamic = control  % dynamic
yrdoy   = control % yrdoy
azzon   = weather  % azzon
beta    = weather  % beta
amtrh   = weather  % amtrh
tairhr  = weather  % tairhr
sndn    = weather  % sndn
snup    = weather  % snup
frdifp  = weather  % frdifp
parhr   = weather  % parhr     
dul1    = soilprop % dul(1)    !Fix this
salb    = soilprop % salb    

!----------------------------------------------------------------------
! Dynamic = runinit or dynamic = seasinit
!---------------------------------------------------------------------
if(dynamic==runinit .or. dynamic==seasinit) then
!open(unit=9000, file="PHOTAGMAIZE.OUT")
!Initialize variables
light = .true.
pghr = 0.0
betn = 0.0
canht = 0.0
hlai = 0.0
lfln = 0.0
lflp = 0.0
canw = 0.0
canwh = 0.0
daytim = .false.
hs = 0.0
turfac = 1.0
mult = 0.0
amplt = 0.0
froll = 0.0
palb = 0.0
palbd = 0.0
pg = 0.0
pgross = 0.0

!Read all sections of fileio and transfer variables
call readfileio(control, 'ALLSEC', datafileio)
files  = datafileio % files
pathsr = datafileio % pathsr
pltpop  = datafileio % pltpop 
rowspcm = datafileio % rowspc
rowspmeter = rowspcm / 100.0
azir    = datafileio % azir

!Read photosynthesis parameters from species file and transfer variables
call readspecies(files, pathsr, '*PHOTO', dataspecies)
asmax = dataspecies % asmax
xc    = dataspecies % xc 
canh  = dataspecies % canh

!CHP 2/23/2009 added array index for tairhr -- I know this is wrong, but
!what does it need to be?  Call subroutine in a loop from 1 to 24?
call MZ_AG_Iphotsynt(dynamic, asmax, gddae, greenla, plaisl, plaish,  &  !Input
     lapot, lflon, lfn, light, parsh, parsun, tairhr(1), lamaxpot,    &  !Input
     pghr)                                                               !Output
   
!----------------------------------------------------------------------
! Dynamic = rate 
!----------------------------------------------------------------------
else if(dynamic == rate) then  


!----------------------------------------------------------------------
! Dynamic = integrate 
!----------------------------------------------------------------------
else if(dynamic == integr) then

! Calculate between plant spacing (m)
if(rowspmeter > 0.0 .and. pltpop > 0.0) then
   betn = 1.0 / (rowspmeter*pltpop)
else
   betn = 0.0
end if

!Calculate canopy growth and update LAI using updated green leaf area
if(dvs <= 1.0) then
   canht = 1.85*canh/(1+EXP(-cans*(dvs-0.95)))
end if

hlai = 0.0
do i = 1, lfn
   if(greenla(i) < 0.0) greenla(i) = 0.0
   hlai = hlai + cm2tom2*greenla(i)*pltpop    
   if(lapot(i) >= 0.8*lamaxpot(i)) then
	  lfln = (((insp*greenla(i)+as) - ((insp*greenla(i)+as)**2.0 - (4.0*insp*greenla(i)*as*cvx))**0.5)/(2.0*cvx))
	  lflp = lfln * cos(pang*rad)
	  canw = lflp * 2.0/100.0
	  canwh = min(max(canwh,canw),rowspmeter)
   end if
end do
if(canht > 0.0  .and. canht < 0.01) canht = 0.01
if(canht > 0.01 .and. canwh < 0.01) canwh = 0.01

! Calculate plant albedo to PAR as a function of surface SW
palb = 0.6 * salb
if(sw1 < dul1) then
   palbd = palb * 1.25
   palb = palbd - (palbd-palb)/dul1 * sw1
end if

!***Begin hourly loop
pgday = 0.0
light = .true.
!open(unit=9000, file="RADABS.OUT")
!write(9000,'(9(1X,A6))') 'YRDOY', 'HOUR', 'LEAFNO', 'LAISL', 'LAISH', 'ADIF', 'ADDR', 'ADDF', 'AREF'
do h = 1, ts
   !Calculate effect of leaf rolling
   !mult  = 5.0 - 4.0*turfac
   !amplt = (25.0+(100.0-25.0)*exp(-3.5*(1.0-amtrh(h))))*mult
   !froll = amin1(turfac+(real(h)-14.0)**2.0/amplt,1.0)
   froll = 1.0	
	    
   !Calculate real and solar time
   hs = real(h) * tincr
   if(hs > snup .and. hs < sndn) then
      daytim = .true.
   else
      daytim = .false.
   end if

   !Calculate hourly radiation absorption by canopy/soil
   call MZ_AG_Radabs(azir, azzon(h), beta(h), betn, canht, canwh, daytim, frdifp(h),     &  !Input
        froll, greenla, h, lfn, palb, parhr(h), pltpop, rowspmeter, scvp, hlai, xc,      &  !Input
        parsh, parsun, plaish, plaisl, radtot, arefhr, adifhr, addrhr, addfhr, leafnum)     !Output
   
!   !write(9000, '(i7,1x,2(1x,i2),4(1x,f5.0))') (yrdoy, h, leafnum(i), adifhr(i), addrhr(i), addfhr(i), arefhr(i), i=1,25)
!   (write(9000, '(i7,1x,2(1x,i2),4(1x,f5.0))') yrdoy, h, leafnum(i), adifhr(i), addrhr(i), addfhr(i), arefhr(i), i=22,1,-1)
!   do i = 22,1,-1
!      write(9000, '(i7,2(1x,i6),2(1x,f6.3),4(1x,f6.1))') yrdoy, h, leafnum(i), plaisl(i), plaish(i), adifhr(i), addrhr(i), addfhr(i), arefhr(i)
!   end do
        
   !KAD - Save absorbed radiation values for each hour
   parsunHour(h) = sum(parsun*plaisl)*3600.
   parshHour(h) = sum(parsh*plaish)*3600.
   radtotHour(h) = radtot*3600.
   
   !Calculate instantaneous gross assimilation
   call MZ_AG_Iphotsynt(dynamic, asmax, gddae, greenla, plaisl, plaish,      &  !Input
        lapot, lflon, lfn, light, parsh, parsun, tairhr(h), lamaxpot,        &  !Input
        pghr)                                                                   !Output
     
   !Integrate instantaneous canopy photoynthesis (µmol[CO2]/m2/s) to get daily values (g[CO2]/m2/day)
   !1 mol[CO2] = 44 g[CO2] so 1umol[CO2] = 44.10-6g[CO2] and 1umol[CO2]/s = 44.10-6 x 3600 g[CO2]/hr
   pgday = pgday + tincr*pghr*44.0*0.0036
end do
!close(9000)
!***End hourly loop

! Daily gross photosynthesis (CH2O): g[CH2O]/m2/day
!This is actually glucose produced: 6 moles[CO2] (44 g/mole) would produce 1 mole[glucose] (180 g/mole) 
!also equivalent to 6 moles[CH2O] (30 g/mole)
pg = pgday*30.0/44.0
pgross = pg*10.0           !Convert pg to kg[CH2O]/ha/day same as kg[glucose]/ha/day

! write(9000,'(f6.2,1x,4(1x,f6.1),1x,i6,1x,f6.2,1x,f6.0,2(1x,f6.1))')  &
!     hlai,gddae,sum(greenla),lapot(10),lflon(10),lfn,dvs,lamaxpot(10),pgross  

! KAD 09/24/2013. Incoming and absorbed radiation
srad = weather % srad
parday = sum(parhr*3600.)*1E-6/4.6
parsunday = sum(parsunHour)*1E-6/4.6
parshday = sum(parshHour)*1E-6/4.6
radtotday = sum(radtotHour)*1E-6/4.6


!-----------------------------------------------------------------------
! End of dynamic if structure
!-----------------------------------------------------------------------
end if  

!----------------------------------------------------------------------
! End of subroutine
!----------------------------------------------------------------------
return
end subroutine MZ_AG_Photsynt


!==============================================================================================================================
! Variable definitions                                                                                  Unit 
!------------------------------------------------------------------------------------------------------------------------------
! amtrh        Hourly atmospheric transmission coefficient or ratio of solar:extraterrestrial radiation - 
! as           Asymptote of the leaf length vs. leaf area relationship                                  cm
! asmax        Maximum instantaneous assimilation at 30 degC                                            µmol[CO2]/m2/s
! azir         Row azimuth relative to North                                                            degrees
! azzon(h)     Hourly solar azimuth (+/- from South)                                                    degrees
! beta(h)      Hourly solar elevation (+/- from horizontal)                                             degrees
! betn         Spacing between plants along a row                                                       m/plant
! canh         Potential canopy height                                                                  m
! canht        Canopy height                                                                            m
! canw         Canopy width (general calculation)                                                       m
! canwh        Canopy width with boundaries defined (e.g. cannot exceed row spacing)                    m
! cm2tom2      Constant for converting leaf area from cm2 to m2                                         m2/cm2
! cvx          Curvature of the leaf length vs. leaf area relationship                                  -
! datafileio   Constructed variable containing all variables read from the DSSAT45.INP                  -
! dataspecies  Constructed variable containing all variables read from the species file                 -
! daytim       Logical variable that describes daytime or nighttime conditions                          -
! dul1         Volumetric drained upper limit of soil water holding capacity for layer 1                cm3[water]/cm3[soil]
! dvs          Development stage (0 is planting, 1 is silking, and 2 is maturity)                       Unitless
! dynamic      Main control variable to tell each module which section of code to run                   -
! files        Species file name                                                                        -
! froll        Leaf rolling factor associated with soil water stress affecting cell expansion           -
! frdifp(h)    Hourly fraction diffuse photon flux density after correcting                             -
!              for circumsolar radiation (Spitters, 1986) 
! gddae        Cumulative growing degree day after emergence                                            degree-days
! greenla(l)   Green leaf area for leaf l                                                               cm2
! h            Hourly iteration counter (1-24)                                                          -
! hlai         "Healthy" leaf area index (excluding senesced parts)                                     m2/m2
! hs           Hourly counter                                                                           hour
! i            Iteration counter of leaf number                                                              -
! insp         Initial slope of the leaf length vs. leaf area relationship                              cm/cm2
! integr       Program control variable to execute code to integrate daily rate variables (value=4)
! lamaxpot(l)  Maximum value of potential leaf area for leaf l (when fully expanded)                    cm2
! lapot(l)     Potential leaf area of leaf l from tip appearance to full expansion                      cm2
! lenla        Dimension (number of elements) of leaf area vector variables                             -
! lfln         Leaf length                                                                              cm
! lflon(l)     Longevity of leaf l                                                                      degree-days
! lfn          Total leaf number rounded up                                                             -
! lflp         Projection of leaves in the horizontal plane                                             cm
! light        Logical variable that differentiates daytime from nighttime
! palb         Plant albedo accounting for soil water in the first soil layer                           -
! palbd        Intermediary plant albedo calculation                                                    -
! pang         Average angle of leaves with the horizontal                                              degrees
! parhr(h)     Hourly photosynthetically active radiation (PAR)                                         µmol[quanta]/m2/s
! parsh(l)     Photosynthetically active radiation absorbed by leaf l in the shaded zone                µmol[quanta]/m2/s
! parsun(l)    Photosynthetically active radiation absorbed by leaf l in the sunlit zone                µmol[quanta]/m2/s
! pathsr       Directory containing (or path to ) the species file                                      -                 
! pg           Daily canopy gross assimilation (glucose equivalent)                                     g[glucose]/m2/day
! pgday        Daily canopy gross assimilation (CO2 uptake)                                             g[CO2]/m2/day
! pghr         Canopy instantaneous gross assimilation                                                  µmol[CO2]/m2/s
! pgross       Daily canopy gross assimilation (glucose equivalent in kg/ha/day)                        kg[glucose]/ha/day
! pi           Mathematical constant pi                                                                 -
! plaish(l)    Shaded leaf area index for leaf l                                                        m2/m2
! plaisl(l)    Shaded leaf area index for leaf l                                                        m2/m2
! pltpop       Plant density                                                                            plants/m2
! rad          Constant for converting angles from degrees to radians                                   radians/degree
! radtot       Sum of PAR components (energy balance check: should equal parhr)                         µmol[quanta]/m2/s
! rate         Program control variable to execute code to compute daily rate variables (value=3)
! rowspmeter   Row spacing in meter                                                                     m
! salb         Soil albedo                                                                              -
! scvp         Scattering coefficient used to calculate diffuse reflectance
! sndn         Time of sunset                                                                           hour
! snup         Time of sunrise                                                                          hour
! sw1          Volumetric soil water content of soil layer 1                                            cm3[water]/cm3[soil] 
! tairhr(h)    Hourly air temperature                                                                   degrees   
! tincr        Time increment                                                                           hour
! ts           Number of hourly time steps per day                                                      -
! xc           Parameter X for calculating black layer extinction coefficient                           -
!              according to Campbell (1986) 
!==============================================================================================================================

