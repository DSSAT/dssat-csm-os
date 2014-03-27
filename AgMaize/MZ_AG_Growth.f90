!=======================================================================
! Growth subroutine for AgMaize by Thijs Tollenaar
! Calculates dry matter partitioning to different plant parts
!-----------------------------------------------------------------------
! REVISION HISTORY
! Originally written by Thijs Tollenaar
! 07/11/2013 Translated into Fortran - Kofikuma Dzotsi
! TODO:
! -Should not have assimilate supply from photosynthesis before the 4th leaf stage.
!  During this period, assimilates come from the grain.
!-----------------------------------------------------------------------
!  Called from: MZ_AG_GLOBAL   
!  Calls:       MZ_AG_KernelNum, MZ_AG_WFactor    
!=======================================================================
subroutine MZ_AG_Growth(control, weather, iswitch, yrplt,                            & !DSSAT inputs
           eop, trwup,                                                               & !From SPAM      
           dvs, olg, gstdyrdoySim,                                                   & !From Phenology
           lai,                                                                      & !From LeafArea
           pgross,                                                                   & !From Photosynthesis
           maint,                                                                    & !From Respiration
           fr, fsts, flv, fvear,                                                     & !From Partitioning
           pstres, kn, grt, grort, gsh, glv, gst, egr, ggr, wrt, wlv, wlvtop,        & !Outputs
           wlvbot, wsts, wstr, wst, we, weolg, grain, kw, tadrw, cgr, cgrf, cgrw,    & !Outputs
           swfac, tadrwAnth, tadrwSilk, seedno, skerwt, stover, wtmain)                !Outputs
!-----------------------------------------------------------------------
use ModuleDefs
use ModuleData
use MZ_AG_ModuleDefs
implicit none
save
!-----------------------------------------------------------------------
real, parameter :: pvs=0.92, pvst=0.69, pvl=0.72, pvr=0.72, pvve=0.70, pvg=0.73
real, parameter,dimension(2) :: xpstres=[0.0, 0.2], yremobfrac=[0.0, 1.0]
real, intent(in) :: dvs, olg, lai, maint, fr, fsts, flv, fvear
real :: kn, gvp, grt, grort, gsh, glv, gst, egr, pggr, ggr, pgr, wrt, wlv, wlvtop, wlvbot, wsts, wstr, wst    
real :: we, weolg, grain, kw, kwyest, kgr, tadrw, itw, tdrw, cgrw, remobp, remob, alin, delayw(8), delaypg(8)
real :: pltpop, kmf, slf, kgf, kgffrac, asg, asgv, pstres, pvtst, pvveg, pgs, cgr, cgrf, tegg, gegf, milk, rwuep1
real :: tmpa, tmax, tmin, eop, trwup, swfac, turfac, tadrwAnth, tadrwSilk, seedno, skerwt, stover, wtmain
real :: delaypg_w1(8), delaypg_w2(15), delaytad_w1(9), delaytad_w2(16)
real :: pgross, cumpgross, pgross_w1, pgross_w2, cgr_d1, cgr_w1, cgr_w2
integer :: timdif, dynamic, doykset, yrplt, yrdoy, i, gstdyrdoySim(20)
character :: iswwat*1, files*12, pathsr*80

! Constructed variables based on definitions in modules   
type(ControlType), intent(in) :: control
type(WeatherType), intent(in) :: weather
type(SwitchType),  intent(in) :: iswitch
type(FileioType)  :: datafileio
type(SpeciesType) :: dataspecies

! Transfer values from constructed data types into local variables
dynamic = control % dynamic
yrdoy   = control % yrdoy
tmax    = weather % tmax
tmin    = weather % tmin
iswwat  = iswitch % iswwat

!-----------------------------------------------------------------------
! Dynamic = runinit
!-----------------------------------------------------------------------
if(dynamic==runinit .OR. dynamic==seasinit) then
asg       = 0.0
asgv      = 0.0
wstr      = 0.0
pggr      = 0.0
ggr       = 0.0
pstres    = 0.0
kgf       = 0.0
kgffrac   = 0.0
kmf       = 1.0
slf       = 1.0
wrt       = 0.0
wlv       = 0.0
wlvtop    = 0.0
wlvbot    = 0.0
wsts      = 0.0
wst       = 0.0
we        = 0.0
weolg     = 0.0
grain     = 0.0
wtmain    = 0.0
tadrw     = 0.0
tdrw      = 0.0
remobp    = 0.0 
remob     = 0.0 
kwyest    = 0.0
kw        = 0.0
cgr       = 0.0
cgrw      = 0.0
swfac     = 1.0
turfac    = 1.0
delayw    = 0.0
delaypg   = 0.0
kn        = 0.0
gsh       = 0.0
grt       = 0.0
grort     = 0.0
glv       = 0.0
gst       = 0.0
egr       = 0.0
tadrwAnth = 0.0
tadrwSilk = 0.0
seedno    = 0.0
skerwt    = 0.0
stover    = 0.0
cgrf      = 0.0
cumpgross = 0.0
delaypg_w1 = 0.0
delaypg_w2 = 0.0
delaytad_w1 = 0.0
delaytad_w2 = 0.0

 !Read all sections of fileio and transfer variables
 call readfileio(control, 'ALLSEC', datafileio)
 pltpop = datafileio % pltpop 
 files  = datafileio % files
 pathsr = datafileio % pathsr
 
 !Read phenology parameters from species file and transfer variables
 call readspecies(files, pathsr, '*PHENO', dataspecies)
 milk = dataspecies % mldvs
 
 !Read root parameters from species file and transfer variables
 call readspecies(files, pathsr, '*ROOT ', dataspecies)
 rwuep1 = dataspecies % rwuep1

!-----------------------------------------------------------------------
! Dynamic = rate
!-----------------------------------------------------------------------
else if(dynamic==rate) then


!-----------------------------------------------------------------------
! Dynamic = integr
!-----------------------------------------------------------------------
else if(dynamic==integr) then  

!---Estimation of kernel number 
if(dvs < 0.95) then
   doykset = yrdoy
   itw = tadrw         
   pgr = 0.0 
else if(dvs >= 0.9 .AND. dvs <= 1.05) then
   pgr = (tadrw - itw)/(timdif(doykset, yrdoy) * pltpop * 10.0)    
end if

call MZ_AG_KernelNum(pgr, kn)

!---Calculation of potential grain growth rate (pggr)
!Effect of temperature on grain growth rate
tmpa = (tmax + tmin) / 2.0          
tegg = 2.88 + 0.292*tmpa    
if(tmpa < 12.0) tegg = 0.912*(tmpa - 5.0)
if(tegg < 0.0) tegg = 0.0
 
!Potential grain sink (kg[dm]/ha/day)
pgs = kn * tegg/1000.0 * pltpop * 10.0     

!Grain exponential growth factor (for grain growth between silking and onset of linear 
!grain dry matter accumulation (OLG))  
if(dvs < 1.0) then
   gegf = 0.0
else if(dvs >= 1.0 .AND. dvs < olg) then
   gegf = exp(10.0 * (dvs - olg))
else if(dvs >= olg) then
    gegf = 1.0
end if

!Kernel maturity factor
if(dvs > milk) kmf = max(exp(10.0 * (milk - dvs)), 0.0)

!Grain sink limitation factor
if(kw > 310.0) slf = (350.0 - kw) / 50.0

!Kernel growth factor
if(kgr < 1.0 .AND. dvs > olg) kgf = kgf + 1.0      
kgffrac = min(10.0, kgf)/10.0
 
!Potential grain growth rate (kg/ha/day) 
pggr = pgs * gegf * slf * kmf * (1.0 - kgffrac) 

!Expected crop growth rate given today's pgross
cgr = cgrf * pgross    

!---Reducing today's assimilate supply using stress computed based on yesterday's partitioning
if(iswwat /= 'N') then
   call MZ_AG_WFactor(weather, iswitch, eop, trwup, cgr, rwuep1,   &   !Input 
                      swfac, turfac)                                   !Output
else
   swfac = 1.0                     !I know this is redundant [KAD]
end if   
 
!Adjusted assimilate
pgross = pgross * swfac  
                  
!---Assimilate supply for growth (kg[glucose]/ha/day)
asg = pgross - maint        
if(asg < 0.0 .AND. wstr >= abs(asg/pvs)) wstr = wstr - abs(asg/pvs)
asg = max(0.0, asg)

!---Production values for total stem (pvtst) and vegetative plant components (pvveg)
pvtst = pvst*fsts + pvs*(1 - fsts)  
pvveg = (flv*pvl + (1 - flv)*pvtst) * (1 - fr) + fr*pvr
 
!---Compute rate of assimilate supply for growth of vegetative components (asgv)
!Grain growth is source limited
if(dvs <= olg) then    
   egr = pvve * fvear * asg
   ggr = min(pggr, egr)
   asgv = asg - ggr/pvg - (egr - ggr)/pvve
else if(dvs > olg) then
   if(pggr <= asg*pvg) then
      ggr = pggr
      remob = 0.0
      asgv = asg - ggr/pvg      
   else if(pggr > asg*pvg) then
      !Potentially 90 % of difference between assimilate demand (by ear) and assimilate supply 
      !by daily canopy photosynthesis can be remobilized from stem reserves (wstr)
      !remob is the quantity of sucrose remobilized (kg[sucrose]/ha/day)
      remobp = 0.90 * (pggr/pvg - asg)
      remob = remobp * alin(xpstres, yremobfrac, size(xpstres), pstres)
      if(remob > wstr/pvs) remob = wstr/pvs            !Today's remobilization cannot exceed the reserves.
      ggr = (asg + remob) * pvg
      asgv = 0.0
   end if      
   egr = ggr
end if

!---Partitioning of vegetative components
! Growth rates (kg[dm]/ha/day) of vegetative components (gvp), roots (grt and grort), 
!vegetative components of shoots (gsh), leaves (glv), stem (gst)
gvp = pvveg * asgv             !Convert assimilate (glucose/ha/day) to dry matter/ha/day
grt = fr * gvp
grort = grt / (pltpop*10.0)
gsh = (1.0 - fr) * gvp
glv = flv * gsh
gst = (1.0 - flv) * gsh  

!---Integrate weights (kg[dm]/ha) of roots (wrt), total, top and bottom leaves (wlv, wlvtop and wlvbot),
!structural stems (wsts), total stems (wst), ear weight (we), total aboveground dry matter (tadrw),
!total dry matter (tdrw), grain (grain)
wrt = wrt + grt
wlv = wlv + glv
if(lai < 2.0) then
   wlvtop = wlv 
else 
   wlvtop = (2.0/lai)*wlv
end if
if(wlv > wlvtop) wlvbot = wlv - wlvtop
wsts = wsts + fsts * gst  
wstr = wstr + (1.0 - fsts)*gst*(pvs/pvtst) - remob   !Weight of stem reserves (kg[sucrose]/ha)
wst = wsts + wstr
pstres = wstr / wst                      
we = we + egr
if(abs(dvs-olg) <= 1E-3) weolg = we
tadrw = wlv + wst + we 
tdrw = tadrw + wrt
grain = grain + ggr
  
!---Kernel weight (mg/kernel) 
if(kn > 0.0) kw = grain / (kn * pltpop/100.0)      
kgr = kw - kwyest
kwyest = kw         !Save today's value of kernel weight

!---Mean crop growth rate during the previous week (kg/ha/day, used in respiration)
delayw(8) = tdrw       !In the original codes delayw(8) = tdrw + wdlv (where wdlv is weight of dead leaves)
delayw(1:7) = [(delayw(i+1), i=1,7)]
!cgrw = sum(delayw(1:7)) / min(timdif(yrplt,yrdoy), 7)
cgrw = (delayw(7)-delayw(1)) / min(timdif(yrplt,yrdoy), 7)

!---Cumulative and average pgross during the previous periods
!Previous week
delaypg_w1(8) = pgross       
delaypg_w1(1:7) = [(delaypg_w1(i+1), i=1,7)]
pgross_w1 = sum(delaypg_w1(1:7)) / min(timdif(yrplt,yrdoy), 7)

!Previous two weeks
delaypg_w2(15) = pgross       
delaypg_w2(1:14) = [(delaypg_w2(i+1), i=1,14)]
pgross_w2 = sum(delaypg_w2(1:14)) / min(timdif(yrplt,yrdoy), 14)

!Cumulative pgross
cumpgross = cumpgross + pgross

!---Average aboveground crop growth rate during the previous periods
!Previous week
delaytad_w1(9) = tadrw       
delaytad_w1(1:8) = [(delaytad_w1(i+1), i=1,8)]
cgr_w1 = (delaytad_w1(8)-delaytad_w1(1)) / min(timdif(yrplt,yrdoy), 7)

!One day
cgr_d1 = delaytad_w1(8) - delaytad_w1(7)

!Previous two weeks
delaytad_w2(16) = tadrw       
delaytad_w2(1:15) = [(delaytad_w2(i+1), i=1,15)]
cgr_w2 = (delaytad_w2(15)-delaytad_w2(1)) / min(timdif(yrplt,yrdoy), 14)

!---Crop growth rate (aboveground dry matter)
if(pgross /= 0.0) cgrf = cgr_w1/pgross_w1

!---Effective biomass demanding maintenance respiration (kg/ha), from Lizaso et al. 2005
wtmain = wrt + wlv + wst*0.8 + we - grain*0.84

!---Variables for output
if(yrdoy == gstdyrdoySim(6)) tadrwAnth = tadrw
if(yrdoy == gstdyrdoySim(7)) tadrwSilk = tadrw
seedno = kn*pltpop
skerwt = kw/1000.
stover = wlv + wst
call put('PLANT','BIOMAS', tadrw)           !For soil temperature routine

!-----------------------------------------------------------------------
! End of dynamic if structure
!-----------------------------------------------------------------------  
end if     

!-----------------------------------------------------------------------
! End of subroutine
!-----------------------------------------------------------------------    
return
end subroutine MZ_AG_Growth


!==============================================================================================================================
! Variable definitions                                                                                  Unit 
!------------------------------------------------------------------------------------------------------------------------------
! alin         [Function] For performing linear interpolations of a y-variable given an x-variable      
! asg	       Assimilate substrate for growth	                                                        kg[glucose]/ha/day
! asgv	       Assimilate supply for growth of vegetative components                                    kg[glucose]/ha/day
! cgr          Crop growth rate (aboveground dry matter)                                                kg[dm]/ha/day
! cgrw         Mean crop growth rate (aboveground dry matter) during the previous week                  kg[dm]/ha/day
! control      Constructed type for control variables
! datafileio   Constructed type for variables read from DSSAT's input/output file (DSSAT45.INP)
! delayw       Vector of total dry weight including roots during the previous week                      kg[dm]/ha
! doykset      Year-day of year on which the calculation of average daily plant growth rate 
!              for kernel number determination starts
! dvs          Development stage (0 is planting, 1 is silking, and 2 is maturity) 
! dynamic      Modular control (runinit=1 for run initialization, seasinit=2 for seasonal initialization,
!              rate=3 for rate calculations, integr=4 for integration of state variables, output=5 for
!              writing daily outputs, seasend=6 for closing output files                                 
! egr	       Ear growth rate	                                                                        kg[dm]/ha/day
! eop          Potential plant transpiration                                                            mm/day
! flv          Proportion of assimilates partitioned to leaves                                          fraction
! fvear	       Effect of stage of development on partitioning to non-grain ear part                     fraction
! fr           Proportion of assimilates partitioned to roots                                           fraction
! fsts         Proportion of assimilates partitioned to structural stems                                fraction
! gegf	       Grain exponential growth factor: for calculation of grain growth                         fraction
!              between silking and onset of linear grain dry matter accumulation  
! ggr	       Grain growth rate	                                                                    kg[dm]/ha/day
! glv	       Growth rate of leaves' dry matter	                                                    kg[dm]/ha/day
! grain	       Grain dry matter	                                                                        kg[dm]/ha
! grort        Root growth rate                                                                         g/plant/day
! grt	       Growth rate of root dry matter	                                                        kg[dm]/ha/day
! gsh	       Growth rate of vegetative components of shoot	                                        kg[dm]/ha/day
! gst	       Growth of stem dry matter	                                                            kg[dm]/ha/day
! gvp          Growth rate of vegetative components	                                                    kg[dm]/ha/day
! i            Integer loop counter
! iswitch      Constructed type for control switches
! iswwat       Water balance simulation switch (Y/N)
! itw	       Intermediate variable for the calculation of kernel number per plant                     kg[dm]/ha
! kgf(frac)    Kernel growth factor: quantification of decline in kernel growth                         fraction (kgffrac) and days (kgf) 
!              rate owing to absence of assimilate flow into kernel	
! kgr	       Kernel growth rate	                                                                    mg/kernel/day
! kmf          Kernel maturity factor                                                                   fraction
! kn           Kernel number                                                                            kernel/plant
! kw	       Kernel weight 	                                                                        mg/kernel
! kwyest       Previous day's kernel weight                                                             mg/kernel
! lai          Leaf area index                                                                          m2[leaves]/m2[ground]
! maint        Maintenance respiration	                                                                kg[glucose]/ha/day 
! milk         Half milk line                                                                           dvs
! olg          Onset linear dry matter accumulation of grain                                            dvs
! pggr	       Potential grain growth rate	                                                            kg[dm]/ha/day
! pgr	       Average daily plant growth rate used to determine kernel number                          g[dm]/plt/day
! pgross       Daily canopy gross assimilation (glucose equivalent in kg/ha/day)                        kg[glucose]/ha/day
! pgs	       Potential grain sink size	                                                            kg[dm]/ha/day
! pltpop       Plant density                                                                            plants/m2
! pstres       Fraction of stem reserves                                                                fraction
! pvl          Production values for leaves	                                                            g[leaves]/g[glucose]
! pvg	       Production value for grain	                                                            g[grain]/g[glucose]
! pvr          Production values for roots	                                                            g[roots]/g[glucose]
! pvs          Production value for sucrose 	                                                        g[sucrose]/g[glucose]
! pvst	       Production values for structural stem	                                                g[stem]/g[glucose]
! pvtst        Production value for total stem	                                                        g[stem]/g[glucose]
! pvve	       Production value for vegetative part of ear	                                            g[VE]/g[glucose]
! pvveg	       Production value for vegetative part components                                          g[VEG]/g[glucose]
! remob	       Stem reserves mobilised out of the stem (sucrose) or stem reserves                       kg[sucrose]/ha/day 
!              remobilised to the grain (glucose)  	
! remobp       Glucose equivalents that can be potentially remobilised from the stem                    kg[glucose]/ha/day
! seedno
! skerwt
! slf	       Grain sink limitation factor	                                                            fraction
! stover
! swfac        Soil water stress effect on growth (0-1), 1 is no stress, 0 is full 
! tadrw	       Total above ground dry weight	                                                        kg[dm]/ha
! tadrwAnth
! tadrwSilk
! tdrw         Total dry weight including roots                                                         kg[dm]/ha
! tegg	       Temperature effect on grain growth rate	                                                mg/kernel/day
! timdif       [Function] For computing difference between two dates                                    returns number of days 
! tmpa         Average air temperature of current day                                                   degree C
! tmax         Maximum air temperature of current day                                                   degree C
! tmin         Minimum air temperature of current day                                                   degree C
! trwup        Total potential daily root water uptake                                                  cm/day
! we	       Weight of ears	                                                                        kg[dm]/ha
! weather      Constructed type for weather variables
! weolg        Weight of ears at onset of linear grain filling                                          kg[dm]/ha
! wlv	       Total weight of leaves	                                                                kg[dm]/ha
! wlvbot       Weight of leaves below top layer (wlv - wlvtop)                                          kg[dm]/ha               
! wlvtop       Weight of leaves in top layer (lai <= 2)                                                 kg[dm]/ha
! wrt          Weight of roots                                                                          kg[dm]/ha
! wst	       Weight of stems	                                                                        kg[dm]/ha
! wstr	       Weight of (temporarily-stored) stem reserves                                             kg[sucrose]/ha
! wsts	       Weight of structural stem 	                                                            kg[dm]/ha
! wtmain
! xpstres      Vector of fraction stem reserves for estimating the fraction of remobilisation           fraction
! yremobfrac   Fraction of remobilisation                                                               fraction
! yrdoy        Year day-of-year                                                                         yyyyddd
! yrplt        Planting date                                                                            yyyyddd
!==============================================================================================================================
