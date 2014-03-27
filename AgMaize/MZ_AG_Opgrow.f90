!======================================================================================
! MZ_AG_Opgrow
! Produces AgMaize's daily outputs
! Write the following files:
!   AG_PlantGro.OUT  Daily growth output
!   AG_LaindGro.OUT  Daily leaf-level output
!----------------------------------------------------------------------
! Revision History
! 04/17/2012 CHP/JZW Written (based on CSM's template)
! 08/23/2012 KAD General revisions, added leaf area outputs
!----------------------------------------------------------------------
! Called from: Main program: MZ_AG_AGMAIZE
! Calls      : None
!======================================================================================
subroutine MZ_AG_Opgrow(control, iswitch, yrplt, mdate, growthStage, gti, tlu, lfnum,     & !Input
       tnleaf, anthes, silk, olg, dvs, gddcer, rla, pla, lai, latf, greenla, gddten,      & !Input
       gddapsim, lftips, lfcols, coltlu, elp, elt, maxlarf, lapot, gddladur, stLayer1,    & !Input
       rlaSoil, rlaAir, pgross, maint, fr, fsts, flv, fvear, kn, grt, gsh, glv, gst, egr, & !Input
       ggr, wrt, wlv, wlvtop, wlvbot, wsts, wstr, wst, we, grain, kw, tadrw, cgr, cgrf, cgrw,   & !Input
       eop, trwup, swfac, rtdep, rlv, srad, parday, parsunday, parshday, radtotday)                                                                         
      
use ModuleDefs 
use MZ_AG_ModuleDefs
implicit none
save

!----------------------------------------------------------------------
! Define variables
!----------------------------------------------------------------------
! Constructed variables    
type(ControlType), intent(in):: control
type(SwitchType),  intent(in):: iswitch
type(FileioType) :: datafileio

! Other variables 
character(len=15), parameter :: outaltmz='AG_PlantGro.OUT', outlai="AG_LaindGro.OUT"
integer,parameter :: lenla=50
character :: pathex*80
logical :: fexist, fexistlai
integer :: i, doy, dap, growthStage, noutmz, noutlai, errnum, errnumlai, year, yrplt, mdate, imax
real :: gti, gddcer, gddten, gddapsim, tlu, tnleaf, lftips, lfcols, elp, elt, anthes, silk, pgross, maint
real :: greenla(50), lapot(50), olg, dvs, rla, pla, lai, maxlarf, latf, coltlu, gddladur
real :: lfnum, stLayer1, rlaSoil, rlaAir
real :: fr, fsts, flv, fvear, kn, grt, gsh, glv, gst, egr, ggr, wrt, wlv
real :: wlvtop, wlvbot, wsts, wstr, wst, we, grain, kw, tadrw, cgr, cgrf, cgrw
real :: eop, trwup, swfac, rtdep, rlv(nl)
real :: cwad, gwad, lwad, swad
real :: srad, parday, parsunday, parshday, radtotday
character(len('LA')+2) :: paste(lenla), lfhead(lenla)
!real, dimension(50) :: lamax, gddlabeg, gddlaend

! Functions/ parameters
integer timdif
character :: correctyear*4, zero_blanks*30
!integer,parameter :: imax = nint(lfnum) + 4   !This is the maximum number of leaves for which output will be printed

! Constructed type variables
character(len=1) :: idetg
integer :: das, dynamic, run, yrdoy, frop

! Transfer values from constructed data types into local variables      
idetg   = iswitch % idetg
if(idetg=='N') return
das     = control % das
dynamic = control % dynamic
run     = control % run
yrdoy   = control % yrdoy
frop    = control % frop

! Create local variables that are compatible with DSSAT output names
cwad = tadrw
gwad = grain
lwad = wlv
swad = wst

!----------------------------------------------------------------------
! Dynamic = runinit
!----------------------------------------------------------------------
if(dynamic==runinit) then

! Get unit number for the output file
call getlun('outaltmz', noutmz)
call getlun('outlai', noutlai)

 ! Read fileio case 'FILENP' and transfer variables
 call readfileio(control, 'FILENP', datafileio)
 pathex = datafileio % pathex

!----------------------------------------------------------------------
! Dynamic = seasinit
!----------------------------------------------------------------------
else if(dynamic==seasinit) then
  !Read fileio case 'INPUTS' and transfer variables
  !call readfileio(control, 'INPUTS', datafileio)
  !lfnum  = datafileio % lfnum
  !!lfnum = 19.60
  imax = nint(lfnum) + 4   !This is the maximum number of leaves for which output will be printed
  !lfhead(1:lenla) = paste('LA', lenla, len('LA'))
  !lfhead = repeat('LA',lenla)
  do i = 1, lenla
     write(lfhead(i),'(I2)') i
     lfhead(i) = 'LA' // lfhead(i)
     lfhead(i) = zero_blanks(lfhead(i))
  end do
  
  !Indices of leaf numbers to display in output file !Or [(i, i=1,ceiling(lfnum))]
  !lfind = [1:size(lfind)];  lfind = lfind(1:(nint(lfnum)+4))           
  
  ! Open PlantGro.OUT as new or existing file    
  inquire(file=outaltmz, exist=fexist)
  if(fexist) then
     open(unit=noutmz, file=outaltmz, status='old', iostat=errnum, position='append')
     !open(unit=100, file="PlantGro.OUT")
     !write(100, FMT='(F6.3,1X,I6)') lfnum, imax
  else
     open(unit=noutmz, file=outaltmz, status='new', iostat=errnum)
     write(noutmz,'("*AgMAIZE DAILY OUTPUT FILE")')
  endif  
  
  ! Open LaindGro.OUT as new or existing file    
  inquire(file=outlai, exist=fexistlai)
  if(fexistlai) then
     open(unit=noutlai, file=outlai, status='old', iostat=errnumlai, position='append')
  else
     open(unit=noutlai, file=outlai, status='new', iostat=errnumlai)
     write(noutlai,'("*AgMAIZE DAILY LEAF-LEVEL OUTPUT FILE")')
  endif 

  ! Generate file heading
  call header(seasinit, noutmz, run)
  call header(seasinit, noutlai, run)

  ! Write variable headers        
  write(noutmz, '(A5, 1X,A3, 2(1X,A5), 1X,A4, 2(1X,A6), 12(1X,A6), 13(1X,A6), 30(1X,A6), 5(1X,A6))')    & 
 !               <imax>(1X,A6))') 
       '@YEAR', 'DOY', 'DAS', 'DAP', 'GSTD', 'GTI','GDDCER',    &
       'GDDTEN','GDDAPS','PGROSS','MAINRE','TLU','NTIP','NLIG','TNLEAF','ELP','ELT','ANTHES','SILK',           &
       'OLG','DVS','RLA','PLA','LAID','LADF','LATF', 'COLTLU', 'LLONG', 'GDLADU', 'RLASOL','RLAAIR','STLYR1',  &
       'FR','FSTS','FLV','FVEAR','KW','GRT','GSH','GLV','GST','EGR','GGR',                                     &
       'KN','WRT','LWAD','WLVTOP','WLVBOT','WSTS','WSTR','SWAD','WE','GWAD','CWAD','CGR','CGRW','CGRF','EOP','TRWUP', &
       'SWFAC','RTDEP','RLV2','SRAD','PARD','PARDSL','PARDSH','RADTOT'
                                                                                         
       !paste(repeat('lamx',imax),1:imax,imax,len('lamx'))
  write(noutlai, '(A5, 1X,A3, 2(1X,A5), <imax>(1X,A6))') '@YEAR', 'DOY', 'DAS', 'DAP', lfhead(1:imax)    
        
! write(noutmz, '('@YEAR', 1X, 'DOY', 1X, 'DAS', 1X, 'DAP', 1X, 'GSTD', 1X, 'GTI', 1X, 'GDDCER', 11(1X,A6), 13(1X,A6), <imax>(1X,A6))')         
       
!----------------------------------------------------------------------
! Dynamic = output
!----------------------------------------------------------------------
else if(dynamic==output) then
  dap = max(0, timdif(yrplt, yrdoy))
  if (dap > das) dap = 0

  ! Write output based on user specified frequency: 
  ! Use advance='no' to position file within the current record
  if((mod(das,frop) == 0)     &     !Daily output every FROP days,
     .OR. (yrdoy == yrplt)    &     !on planting date, and
     .OR. (yrdoy == mdate)) then    !at harvest maturity         
  
     call yr_doy(yrdoy, year, doy)     
     write(noutmz, '(1X,A4, 1X,I3, 2(1X,I5), 1X,I4, 2(1X,F6.1), 2(1X,F6.1), 2(1X,F6.1), 1X,F6.2, &
     2(1X,F6.2), 13(1X,F6.2), 2(1X,F6.0), 3(1X,F6.2), 5(1X,F6.2), 19(1X,F6.0), 6(1X,F6.2), 5(1X,F6.2))') &
     !<imax>(1X,F6.1), 
     !5(1X,F6.2), 5(1X,F6.2))')
     !To plot in GBuild, use correctyear(year) instead of year, and change formatting to character
                correctyear(year), doy, das,dap, growthStage, gti,gddcer, gddten,gddapsim,  &
                pgross,maint, tlu, lftips,lfcols, tnleaf,elp,elt,anthes,silk,      &
                olg,dvs,rla,pla,lai,maxlarf,latf,coltlu, lapot(5),gddladur, rlaSoil,rlaAir,stLayer1,  &
                fr,fsts,flv,fvear,kw, grt,gsh,glv,gst,egr,ggr,kn,wrt,lwad,wlvtop,wlvbot,wsts,wstr,    &      
                swad,we,gwad,cwad,cgr,cgrw, cgrf,eop,trwup,swfac,rtdep,rlv(2),    &
                srad,parday,parsunday,parshday,radtotday
     !lamax(1:imax), 
     !gddlabeg(1:5), gddlaend(1:5)
     write(noutlai, '(1X,A4, 1X,I3, 2(1X,I5), <imax>(1X,F6.1))') correctyear(year), doy, das, dap, greenla(1:imax)
  end if
        
!----------------------------------------------------------------------
! Dynamic = seasend
!----------------------------------------------------------------------
else if(dynamic==seasend) then      
  ! Close daily output file
  close(noutmz) 
  close(noutlai)
  
  ! Copy daily output file to filex's directory
  !call copy_file('*.OUT', trim(pathex))
  call copy_file(outaltmz, trim(pathex)//outaltmz)
  
       
!-----------------------------------------------------------------------
! End of dynamic if structure
!-----------------------------------------------------------------------
endif
return

!----------------------------------------------------------------------
! End of subroutine
!----------------------------------------------------------------------
end subroutine MZ_AG_Opgrow


  
!==============================================================================================================================
! Variable definitions                                                                                  Unit 
!------------------------------------------------------------------------------------------------------------------------------
! anthes       Leaf stage at anthesis                                                                   tlu   
! lfnum        Base leaf number                                                                         leaves
! cgrw         Mean crop growth rate (aboveground dry matter) during the previous week                  kg[dm]/ha/day
! coltlu       Thermal leaf units from leaf tip appearance to full leaf expansion                       tlu
! control      Constructed type for control variables
! cwad	       Total above ground dry weight	                                                        kg[dm]/ha
! dap          Day after planting                                                                       
! das          Days after start of simulation
! datafileio   Constructed type for variables read from DSSAT's input/output file (DSSAT45.INP)
! doy          Day of year
! dvs          Development stage (0 is planting, 1 is silking, and 2 is maturity) 
! dynamic      Modular control (runinit=1 for run initialization, seasinit=2 for seasonal initialization,
!              rate=3 for rate calculations, integr=4 for integration of state variables, output=5 for
!              writing daily outputs, seasend=6 for closing output files                                 
! egr	       Ear growth rate	                                                                        kg[dm]/ha/day
! elp          Change in leaf number due to photoperiod                                                 leaves 
! elt          Change in leaf number due to temperature                                                 leaves      
! errnum       Integer variable that informs on the data transfer status for the plant growth output file
! errnumlai    Integer variable that informs on the data transfer status for the detailed leaf area output file
! eop          Potential plant transpiration                                                            mm/day
! fexist       Logical variable for testing whether the plant growth output file exist
! fexistlai    Logical variable for testing whether the detailed leaf area output file exist
! flv          Proportion of assimilates partitioned to leaves                                          fraction
! fvear	       Effect of stage of development on partitioning to non-grain ear part                     fraction
! fr           Proportion of assimilates partitioned to roots                                           fraction
! frop         Frequency of daily output                                                                days
! fsts         Proportion of assimilates partitioned to structural stems                                fraction
! gddapsim     Cumulative daily thermal time after planting using APSIM's approach                      degree-day
! gddcer       Cumulative daily thermal time after planting (CERES method)                              degree-day                                                            
! gddladur     Duration of leaf expansion                                                               degree-day
! gddten       Cumulative daily thermal time after planting using GDD[10,30]                            degree-day
! ggr	       Grain growth rate	                                                                    kg[dm]/ha/day
! glv	       Growth rate of leaves' dry matter	                                                    kg[dm]/ha/day
! grain	       Grain dry matter	                                                                        kg[dm]/ha
! greenla(i)   Cumulative area of leaf i during expansion and senescence                                m2/leaf
! growthStage  Integer value of growth stage
! grt	       Growth rate of root dry matter	                                                        kg[dm]/ha/day
! gsh	       Growth rate of vegetative components of shoot	                                        kg[dm]/ha/day
! gst	       Growth of stem dry matter	                                                            kg[dm]/ha/day
! gti          General Thermal Index heat unit accumulation                                             gti (Q.what is the real unit of gti?)
! gwad	       Grain dry matter	                                                                        kg[dm]/ha
! imax         Maximum number of leaves for which output will be printed
! idetg        Growth output switch
! iswitch      Constructed type for control switches
! kn           Kernel number                                                                            kernel/plant
! kw	       Kernel weight 	                                                                        mg/kernel
! lai          Whole-plant leaf area index                                                              m2[leaf]/m2[ground]
! lapot(i)     Cumulative area of leaf i during expansion (tip to collar) under non-stressed conditions m2/leaf
! latf         Effect of temperature on leaf growth
! lfcols       Leaf node position of leaf that has completed expansion
! lwad	       Total weight of leaves	                                                                kg[dm]/ha
! maint        Maintenance respiration	                                                                kg[glucose]/ha/day 
! maxlarf      Reduction factor of maximum leaf area due to plant density
! mdate        Maturity date                                                                            yyyyddd
! noutmz       Integer unit number for plant growth output file
! noutlai      Integer unit number for detailed leaf area output file
! olg          Onset linear dry matter accumulation of grain                                            dvs
! outaltmz     Daily plant growth output file name
! outlai       Leaf-level daily leaf area output file name
! pgross       Daily canopy gross assimilation (glucose equivalent in kg/ha/day)                        kg[glucose]/ha/day
! pla          Plant leaf area                                                                          m2/plant
! rla          Rate of Leaf tip Appearence                                                              leaves/day
! rlaAir       Rate of leaf tip appearance using air temperatures                                       leaves/day
! rlaSoil      Rate of leaf tip appearance uing soil temperature of layer 1                             leaves/day
! rlv(l)       Root length density of layer l                                                           cm[root]/cm3[soil]
! rtdep        Rooting depth, initially set at emergence                                                cm
! silk         Leaf stage at silking                                                                    tlu 
! stLayer1     Soil temperature of layer 1                                                              degree C
! swad	       Weight of stems	                                                                        kg[dm]/ha
! swfac        Soil water stress effect on growth (0-1), 1 is no stress, 0 is full 
! tadrw	       Total above ground dry weight	                                                        kg[dm]/ha
! timdif       [Function] For computing difference between two dates                                    returns number of days 
! tlu          Thermal leaf unit (cumulative rate of leaf tip appearance)                               tlu
! tnleaf       Total number of initiated leaves                                                         leaves
! trwup        Total potential daily root water uptake                                                  cm/day
! we	       Weight of ears	                                                                        kg[dm]/ha
! wlv	       Total weight of leaves	                                                                kg[dm]/ha
! wlvbot       Weight of leaves below top layer (wlv - wlvtop)                                          kg[dm]/ha               
! wlvtop       Weight of leaves in top layer (lai <= 2)                                                 kg[dm]/ha
! wrt          Weight of roots                                                                          kg[dm]/ha
! wst	       Weight of stems	                                                                        kg[dm]/ha
! wstr	       Weight of (temporarily-stored) stem reserves                                             kg[sucrose]/ha
! wsts	       Weight of structural stem 	                                                            kg[dm]/ha
! year         Four-digit year                                                                          yyyy
! yrdoy        Year day-of-year                                                                         yyyyddd
! yrplt        Planting date                                                                            yyyyddd
!==============================================================================================================================
