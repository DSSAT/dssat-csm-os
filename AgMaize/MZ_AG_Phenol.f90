!======================================================================================
! MZ_AG_Phenol
! Calculates phenological development of crop based on thermal leaf unit (TLU)
!----------------------------------------------------------------------
! Revision History
! 09/04/2012 JW/KAD Translated from Basic (written by TT)
!----------------------------------------------------------------------
! Called from: Main program: MZ_AG_AGMAIZE
! Calls      : Function TIMDIF (in DATES.FOR)
! TO DO
! -The EPIC soil temperature was included AgMaize to improve predictions of the rate of 
! appearance. Normally there is a switch in DSSAT to activate this method BUT the 
! current implementation in AgMaize does not use this switch. I need to get the new
! version of DSSAT and formally call STEMP_EPIC so that its use can be controlled
! from the methods switch in the xfile. Also, check to see if one can always use
! the temperature from the top 5 cm layer only. KAD 03/14/2013
! TO CHECK:
! 1. Growth stages 5-6 when silk is increased more than tlu, the ratio dvs = tlu/silk
!    can decrease! e.g. Gainesville, FL dataset
! 2. Fix effect of pgr on asi once growth subroutine is running
!======================================================================================
subroutine MZ_AG_Phenol(control, iswitch, SoilProp, weather, yrplt, sw, st, snow, cgr,        & !Input
  gti, tlu, lfnum, tnleaf, anthes, silk, olg, dvs, dvsrate, growthStage, tmpa, rla, gddcer,   & !Output
  dtt, gddae, gddten, gddapsim, mdate, gstdyrdoySim, gstddoySim, gstdyearSim, growthtlu,      & !Output
  elp, elt, stLayer1, rlaSoil, rlaAir, lfnc, coltlu)                                            !Output
!----------------------------------------------------------------------  
use ModuleDefs
use MZ_AG_ModuleDefs
implicit none
save
!----------------------------------------------------------------------
real,    intent(in)  :: sw(nl), snow, cgr
integer, intent(in)  :: yrplt
integer, intent(out) :: growthStage, mdate, gstdyrdoySim(20), gstddoySim(20), gstdyearSim(20)
real,    intent(out) :: gti, tlu, tnleaf, anthes, silk, olg, dvs, dvsrate, growthtlu(10),   &
                        tmpa, rla, gddcer, gddae, dtt, gddten, gddapsim, elp, elt
                        
! Constructed variables 
type (ControlType), intent(in) :: control
type (SoilType),    intent(in) :: SoilProp
type (SwitchType),  intent(in) :: iswitch
type (WeatherType), intent(in) :: weather   
!type (MulchType),   intent(in) :: mulch   
type(FileioType)  :: datafileio     
type(EcotypeType) :: ecofileout             
type(SpeciesType) :: dataspecies  

! Variables from constructed types 
character(len=1) iswwat
integer :: dynamic, yrdoy, yrsim, nlayr
real :: tmax, tmin, srad, dayl, dlayr(nl), ll(nl)

! Input / parameters
character :: econo*6, filee*12, files*12, pather*80, pathsr*80
real :: sdepth, lfnum, rlamax, lsint, photp, lftop, pltpop, TCeiling, TOptRla, TMinRla, milk, blackl,  &
        xtemp(5), ygdd(5), swcg, tbase, topt, ropt

! Other variables/parameters
real, parameter :: tothirty = 30.0, tbten = 10.0
real :: meant, cumpdw, pgrasi, delayr(8), sumr, tsum, mrad, swsd 
real :: gtir, asi, extral, gfp, rlan, rlad, rlaf, cumdep, lfnc, coltlu, coltlu_2
integer :: doytnleaf, dcount, L, L0, i, year, doy, lfn
real :: st(nl), stLayer1, srftemp, rlaAir, rlaSoil       !EPIC's soil temperature calculation

! Functions
integer timdif
real         &     !Real functions
fnleafap,    &     !Function for computing rate of leaf appearance (see end of subroutine)
fnrla,       &     !New function for computing rate of leaf appearance (see end of subroutine)
fnrlaDaily,  &
dailyttapsim, tempthreehr,     &   !Functions for computing APSIM's thermal time accumulator
getgddcer, getgddten, getgti,  &   !Functions for computing GDD8, GDD[30,10] and GTI thermal accumulators
getcoltlu

! Transfer values from constructed data types into local variables
dynamic = control % dynamic
yrdoy   = control % yrdoy
yrsim   = control % yrsim
tmax    = weather % tmax
tmin    = weather % tmin
srad    = weather % srad 
dayl    = weather % dayl
dlayr   = SoilProp % dlayr
ll      = SoilProp % ll
nlayr   = SoilProp % nlayr
iswwat  = iswitch % iswwat

!----------------------------------------------------------------------
! Dynamic = runinit or dynamic = seasinit
!---------------------------------------------------------------------
if(dynamic==runinit .or. dynamic==seasinit) then
 !Read all sections of fileio and transfer variables
 call readfileio(control, 'ALLSEC', datafileio)
 filee = datafileio  % filee
 files  = datafileio % files
 pather = datafileio % pather 
 pathsr = datafileio % pathsr
 sdepth = datafileio % sdepth 
 lfnum  = datafileio % lfnum
 rlamax = datafileio % rlamx
 lsint  = datafileio % taint
 photp  = datafileio % photp
 lftop  = datafileio % lftop
 pltpop = datafileio % pltpop
 econo  = datafileio % econo
 
 !Read all sections of the species file and transfer variables
 call readspecies(files, pathsr, 'ALLSEC', dataspecies)
 !Transfer phenology parameters 
 TCeiling = dataspecies % tceil
 TOptRla  = dataspecies % torla
 TMinRla  = dataspecies % tbrla
 milk     = dataspecies % mldvs
 blackl   = dataspecies % bldvs
 
 !Transfer APSIM thermal time parameters 
 xtemp = dataspecies % xtemp
 ygdd  = dataspecies % ygdd
 
 !Transfer seed germination parameters 
 swcg = dataspecies % swcg
 
 !Read ecotype file and transfer variables
 call readecotype(pather, filee, econo, ecofileout)
 tbase = ecofileout % tbase
 topt  = ecofileout % topt
 ropt  = ecofileout % ropt
 
 !Initialize variables
 !GFP is duration of grain filling period (in gti) assuming photoperiod 15.1 h and mean temperature 
 !is 19oC during photoperiod and temperature sensitive period. One thermal leaf unit (tlu) = 28 gti 
 !during grain filling period.
 !OLG is onset of period of linear rate of grain filling (dvs)
 tnleaf = lfnum
 anthes = lfnum + lsint                                      
 silk = anthes
 !gfp = 0.95 * (lfnum + 0.2*4 + 3.1*photp + lsint) * 28.0     
 gfp = 0.95 * anthes * 28.0     
 olg = 1 + (5.5*28.0) / gfp                                 
 gti = 0.0
 tlu = 0.0
 dvsrate = 0.0
 dvs = 0.0
 growthStage = 14 
 tmpa = 0.0
 rla = 0.0
 gddcer = 0.0
 gddae = 0.0
 dtt = 0.0
 gddten = 0.0
 gddapsim = 0.0
 mdate = -99
 rlaf = 1.0
 gstddoySim = 0
 gstdyearSim = 0
 gstdyrdoySim = 0
 growthtlu = 0.0
 elp = 0.0
 elt = 0.0
 stLayer1 = 0.0
 rlaSoil = 0.0
 rlaAir = 0.0
 cumpdw = 0.0
 pgrasi = 0.0
 delayr = 0.0
 tsum = 0.0
 asi = 0.0
 lfnc = 0.0
 coltlu = 0.0
 dcount = 0
 meant = 0.0
 swsd = 0.0
 doytnleaf = 0
 
 !call MZ_GM_StempEpic(control, iswitch, soilprop, sw, weather, snow, mulch,   & !Inputs
 !                     srftemp, st)                                              !Output
!----------------------------------------------------------------------
! Dynamic = rate 
!----------------------------------------------------------------------
else if(dynamic == rate) then


!----------------------------------------------------------------------
! Dynamic = integrate 
!----------------------------------------------------------------------
else if(dynamic == integr) then 
!---Save preliminary stages
if(yrdoy == yrsim) then
   growthStage = 14
   gstdyrdoySim(growthStage) = yrdoy 
end if
if(yrdoy == yrplt) then
   growthStage = 13
   gstdyrdoySim(growthStage) = yrdoy 
end if
          
!---Check if crop is planted  
 !if(yrdoy <= yrplt) return
 !This was just for running the soil water balance several days prior to planting
! if(yrdoy <= yrplt) then
!    call MZ_GM_StempEpic(control, iswitch, soilprop, sw, weather, snow, mulch,   & !Inputs
!                         srftemp, st)                                              !Output
!    return
! end if
 
!---On the first day after planting check conditions for germination; do this only if growthStage is still at planting
 NOTGERMINATE: if(growthStage==13) then
    cumdep = 0.0
    do L = 1, nlayr
       cumdep = cumdep + dlayr(L)
       if(cumdep > sdepth) exit      !Could use >= instead of >
    end do
    L0 = L                           !L0 is layer that seed is in
 
    !If water stress is not simulated, crop germinates 1 dap; if water stress is simulated crop can still germinate 1 dap
    !but germination is controlled by soil moisture status
    if(iswwat /= 'N') then
       if(sw(L0) <= ll(L0)) then
          swsd = (sw(L0)-ll(L0))*0.65 + (sw(L0+1)-ll(L0+1))*0.35
          if(swsd >= swcg) growthStage = 1     !Germinate when soil water >= swcg; swcg = 0.02 cm3/cm3
       else
          growthStage = 1     
       end if
    else
       growthStage = 1
    end if
 end if NOTGERMINATE

!---On the day of germination, start computing the rate of leaf appearance and different thermal time accumulators 
 GERMINATE: if(growthStage >= 1) then 
 
!---Calculation of daily General Thermal Index heat unit accumulation
    tmpa = (tmax + tmin)/2 
    gtir = getgti(tmpa, tlu, silk)
    gti = gti + gtir        
 
!---Compute cumulative GDD8 for use in leaf area subroutine
    dtt = getgddcer(tmax, tmin, topt, ropt, tbase, tlu, anthes, snow, srad, dayl)
    gddcer = gddcer + dtt	
    
    !Get gddae for photosynthesis routine
    if(growthStage >= 2) gddae = gddae + dtt
  
!---Calculation of daily Growing Degree Day accumulation (called here GDD[TOPT,TBASE] i.e. GDD[30,10])
    gddten = gddten + getgddten(tmax, tmin, tothirty, tbten) 
      
!---Calculation of APSIM's thermal accumulator
    gddapsim = gddapsim + dailyttapsim(tmax, tmin, xtemp, ygdd)       

!---Rate of Leaf tip Appearence (RLA)     
    !Mean incident solar radiation during the last week     
    !BEG:SUGGESTED CODE IMPROVEMENT---
    delayr(8) = srad    
    !delayr(8) = srad   
    !delayr(1:7) = [(delayr(i+1), i=1,7)]
    !sumr = sum(delayr(1:7))    
    sumr = 0.0
    do i = 1, 7    !previous 7 days
       delayr(i) = delayr(i+1)
       sumr = sumr + delayr(i)
    end do
    !END:SUGGESTED CODE IMPROVEMENT---
    
    !BEG:SUGGESTED CODE IMPROVEMENT---
    !mrad = sumr / min(timdif(yrplt,yrdoy),7)
    if(timdif(yrplt,yrdoy) >= 7) then 
       mrad = sumr / 7 
    else 
       mrad = sumr / timdif(yrplt,yrdoy)
    end if
    !END:SUGGESTED CODE IMPROVEMENT---
  
    !Compute rate of leaf appearance factor (rlaf)   
    if(tlu <= 3.0) then 
       rlaf = 1 - (0.1*(3 - tlu) / 3)
    else if(tlu <= 12) then
       !BEG:SUGGESTED CODE IMPROVEMENT---
       !rlaf = 0.015*min(max(mrad,10.0),20.0) + 0.85
       if(mrad >= 20.0) then
          rlaf = 1.15
       else if(mrad >= 10.0) then
          rlaf = 1.0 + 0.15 * (mrad - 10.0) / 10.0 
       else 
          rlaf = 1.0
       end if
       !END:SUGGESTED CODE IMPROVEMENT---
       if(rlaf < 1.15 - 0.15 * (12 - tlu) / 9) rlaf = 1.15 - 0.15 * (12 - tlu) / (9*3)   
    else 
       rlaf = 1.15
    end if   

    !---03/18/2013 - Use EPIC's soil average soil temperature to compute rla if tlu < 8
    stLayer1 = st(1)      
    
    ! Define which RLA to use based on the leaf stage
    if(tlu < 8.0) then       
       !Use EPIC's soil average soil temperature to compute rla if tlu < 8
       stLayer1 = st(1)     
       rlaSoil = rlaf * fnrla(rlamax, TCeiling, TOptRla, TMinRla, stLayer1)                        
       rla = rlaSoil
    else
       !Compute RLA using max and min air temperatures
       rlaAir = rlaf * fnrlaDaily(tmax, tmin, rlamax, TCeiling, TOptRla, TMinRla)
       rla = rlaAir
    end if
    
    ! Integrate the thermal leaf unit
    tlu = tlu + rla
 end if GERMINATE
  
!---Effect of photoperiod and temperature on final leaf number at growthStage 3
 JUVENILE: if(growthStage == 3) then
   !Compute change in leaf number as a function of temperature (elt)
   dcount = dcount + 1	           !Number of days since the beginning of temperature and photoperiod sensitive phase
   tsum = tsum + tmpa              !Cumulative mean temperature since the beginning of growth stage 3
   meant = tsum / dcount           !Average of mean temperature since the beginning of growth stage 3
   if(meant > 15.0) then 
      elt = 0.2*(meant - 15.0)     !Grow 0.2 leaves for each degree C above 15 degrees C
   else 
      elt = 0.4*(15.0 - meant)     !Grow 0.4 leaves for each degree C below 15 degrees C
   end if
    
   !Compute change in leaf number as a function of photoperiod (elp)
   if(dayl <= 12.0) then
      elp = 0.0                      !No photoperiod sensitivity if day length is smaller or equal to 12 hours
   else if(dayl <= 16.0) then
      elp = (dayl - 12.0) * photp    !Grow photp more leaves for each hour of day length between 12 and 16 hours
   else
      elp = 4.0 * photp              !For day length > 16 hours, grow 4 times photp leaves
   end if
    
   !Compute number of extra leaves and add to total leaf number
   extral = elt + elp	           !Total number of extra leaves
   tnleaf = lfnum + extral         !Update total number of leaves
   anthes = tnleaf + lsint         !Leaf stage at anthesis (in tlu)
   silk = anthes

 !---Tassel initiation to end of anthesis
 else if(growthStage == 4) then JUVENILE
   silk = anthes
   doytnleaf = yrdoy

 else if(growthStage==5 .or. growthStage==6) then JUVENILE
   !If tassel initiation was ever skipped
   if(doytnleaf==0) doytnleaf = yrdoy  
  
   !Calculation of pgr for the estimation of asi
   cumpdw = cumpdw + cgr/(pltpop*10)
   pgrasi = cumpdw / timdif(doytnleaf, yrdoy)     
   if(pgrasi >= 3.0) then 
      asi = 0
   else
      asi = (2.0**(3.0-pgrasi) - 1.0) * 0.3    !asi in tlu (assuming mean rla is 0.3)
   end if
   silk = anthes + asi  
   gfp = 0.95 * anthes * 28.0
 end if JUVENILE
 
 !---Beginning of silking to maturity
 !Compute developmental stage (0 is planting, 1 is silking, and 2 is maturity)
 if(tlu <= silk) then
    !BEG:SUGGESTED CODE IMPROVEMENT
    dvs = tlu/silk
    !dvsrate = rla/silk
    !dvs = dvs + dvsrate
    !END:SUGGESTED CODE IMPROVEMENT
 else
    dvsrate = gtir/gfp      !Q.units? if gtir is gti/day and gfp is gti then dvs is days (rate of dvs is per day)???
    dvs = dvs + dvsrate
 end if
 
 !---Set growth stages: emergence to anthesis
 if(tlu >= 2.0 .and. tlu < 4.0) then
    growthStage = 2                    !growthStage 2 is plant emergence
 else if(tlu >= 4.0 .and. tlu <= (0.46*tnleaf - 1.0)) then
    growthStage = 3                    !growthStage 3 is end of juvenile phase 
 else if(tlu > (0.46*tnleaf - 1.0) .and. tlu < tnleaf) then
    growthStage = 4                    !growthStage 4 is tassel initiation
 else if(tlu >= tnleaf .and. tlu < anthes) then
    growthStage = 5                    !growthStage 5 is appearance of topmost leaf   
 else if(tlu >= anthes) then 
    growthStage = 6                    !growthStage 6 is anthesis
 end if                  
 
 !---Set growth stages: silking to black layer
 if(dvs >= 1.0)    growthStage = 7     !growthStage 7 is silking
 if(dvs >= olg)    growthStage = 8     !growthStage 8 is olg
 if(dvs >= milk)   growthStage = 9     !growthStage 9 is half milk line
 if(dvs >= blackl) growthStage = 10    !growthStage 10 is black layer
 
 !---Compute collar TLU
 lfnc = tnleaf - lftop                        !Node position of the largest leaf
 lfn = ceiling(tnleaf)                        !Total leaf number rounded up
 if(aint(tlu) < tnleaf) then                  !If current leaf tip number < tnleaf)
    coltlu = getcoltlu(tlu, lfnc)             !Continuous (for output)
    coltlu_2 = getcoltlu(aint(tlu), lfnc)     !Stepwise
 else 
    coltlu = coltlu_2 + tnleaf-real(lfn-1)
 end if           

 !---Save dates of current growth stages
 if(growthStage > 0) then
    if(gstddoySim(growthStage)==0) then
       call yr_doy(yrdoy, year, doy)
       gstdyrdoySim(growthStage) = yrdoy
       gstddoySim(growthStage) = doy
       gstdyearSim(growthStage) = year
       growthtlu(growthStage) = tlu
       !For growth stages between 2 (emergence) and 10 (black layer), set the occurrence date of the previous
       !stage to that of the current stage if both stages occur on the same date. An alternative way is to set
       !stage dates as they occur but that would be a lot of code repetitions OR use a function
       if(growthStage >= 2 .AND. growthStage <= 10) then
          if(gstdyrdoySim(growthStage-1) == 0) then
             gstdyrdoySim(growthStage-1) = gstdyrdoySim(growthStage)
             gstddoySim(growthStage-1) = gstddoySim(growthStage)
             gstdyearSim(growthStage-1) = gstdyearSim(growthStage)
             growthtlu(growthStage-1) = growthtlu(growthStage)
          end if
       end if
    end if   
 end if   

 !---Get maturity date and stop model
 if(growthStage == 10) then
    mdate = yrdoy
    return
 end if

 !---Output variables
 !yremrg = gstdyrdoySim(2)
 !isdate = gstdyrdoySim(7)

!----------------------------------------------------------------------
! Dynamic = Output 
!----------------------------------------------------------------------
else if(dynamic == output) then 

!----------------------------------------------------------------------
! Dynamic = Seasend 
!----------------------------------------------------------------------
else if(dynamic == seasend) then 

!-----------------------------------------------------------------------
! End of dynamic if structure
!-----------------------------------------------------------------------
end if  

!----------------------------------------------------------------------
! End of subroutine
!----------------------------------------------------------------------
return
end subroutine MZ_AG_Phenol
      
      
!==============================================================================================================================
! Functions used in this leaf area subroutine
!-----------------------------------------------------------------------
! Function for computing the rate of leaf appearance in phenology
!-----------------------------------------------------------------------
 pure real function fnleafap(t)
    real, intent(in) :: t
    fnleafap = 0.0997 - 0.036*t + 0.00362*t**2 - 0.0000639*t**3
 end function fnleafap      
 
!-----------------------------------------------------------------------
! New generic function for computing the rate of leaf appearance 05/20/2013
! Temperature (Temp) can be daily or hourly
!-----------------------------------------------------------------------
 real function fnrla(maxrla, TempCeil, TempOpt, TempMin, Temp)
    real, intent(in) :: maxrla, TempCeil, TempOpt, TempMin
    real Temp
    Temp = min(max(TempMin,Temp), TempCeil)
    fnrla = maxrla * ((TempCeil-Temp)/(TempCeil-TempOpt)) * (Temp/TempOpt)**(TempOpt/(TempCeil-TempOpt))
 end function fnrla    
 
!-----------------------------------------------------------------------
! Hourly calculation of the rate of leaf appearance (RLA)
! Same as fnrla but calculated RLA is divided by 24 (for 1 hour)
!-----------------------------------------------------------------------
 real function fnrlaHourly(maxrla, TempCeil, TempOpt, TempMin, tHour)
    real, intent(in) :: maxrla, TempCeil, TempOpt, TempMin, tHour
    real fnrla
    fnrlaHourly = fnrla(maxrla, TempCeil, TempOpt, TempMin, tHour)/24.0
 end function fnrlaHourly    
 
 
!-----------------------------------------------------------------------
! New function for computing the daily rate of leaf appearance based on
! hourly temperatures 05/29/2013
!-----------------------------------------------------------------------
 real function fnrlaDaily(tMax, tMin, maxrla, TempCeil, TempOpt, TempMin)
    real, intent(in) :: maxrla, TempCeil, TempOpt, TempMin
    real fnrlaHourly, rlaHourly, tMin, tMax, th
    real, parameter :: pi = 3.141592
    integer :: i
    
    fnrlaDaily = 0.0
    do i = 1, 24
       th = (tMax+tMin)/2.0 + (tMax-tMin)/2.0 * sin((pi/12.0)*real(i))
       !Estimate of hourly RLA, already divided by 24 and th already constrained between 0 and TCeiling
       rlaHourly = fnrlaHourly(maxrla, TempCeil, TempOpt, TempMin, th)       
       fnrlaDaily = fnrlaDaily + rlaHourly    
    end do   
 end function fnrlaDaily
   

!-----------------------------------------------------------------------      
! This function computes the daily thermal time corresponding to the maxT
! and minT inputs. Part of APSIM's thermal time accumulator.
! Steps:
! 1. Estimate eight 3-hourly temperatures based on minT and maxT
! 2. Estimate (interpolate) growing degree days corresponding to these
!    temperatures
! 3. Derive the daily thermal time as the average of the eight values of 
!    growing degree days
!-----------------------------------------------------------------------      
real function dailyttapsim(maxT, minT, xtemp, ygdd)
     real, intent(in) :: maxT, minT, xtemp(5), ygdd(5)
     integer maxPeriod, period
     real gdd3Hr, gdd3HrCum, tMean3Hr, alin
         
     gdd3HrCum = 0.0
     maxPeriod = 8
     do period = 1, maxPeriod
        tMean3Hr = tempthreehr(maxT, minT, real(period))                !Get a three-hour air temperature
        gdd3HrCum = gdd3HrCum + alin(xtemp,ygdd,size(xtemp),tMean3Hr)   !Estimate corresponding value of thermal time
     end do
     dailyttapsim = gdd3HrCum/real(maxPeriod)
end function dailyttapsim

 
!-----------------------------------------------------------------------      
! This function estimates a three-hourly air temperature given daily maximum
! and minimum air temperatures and a specific period. 
! Part of APSIM's daily thermal accumulator
!-----------------------------------------------------------------------      
real function tempthreehr(tMax, tMin, period) 
     real, intent(in) :: tMax, tMin, period
     real rangeFrac, diurnalRange, deviation
     rangeFrac = 0.92105 + 0.1140*period - 0.0703*period**2 + 0.0053*period**3
     diurnalRange = tMax - tMin
     deviation = rangeFrac * diurnalRange   
     tempthreehr = tMin + deviation
end function tempthreehr   

!-----------------------------------------------------------------------      
! Function to compute CERES-Maize's daily thermal time (GDD8)
! Taken from MZ_Phenol 
! tdsoil   Weighted average soil temperature, C
! tempcn = Crown temperature when snow is present and tmin < 0
! tempcx = Crown temp. for max. development rate, C
! tempcr = Crown temperature, C
! snow   = Snow depth, mm
! xs     = Temporary snow depth variable, mm
!-----------------------------------------------------------------------      
real function getgddcer(tMax, tMin, tOpt, rOpt, tBase, tlu, anthes, snow, srad, dayl)
     real, intent(in) :: tMax, tMin, tOpt, rOpt, tBase, tlu, anthes, snow, srad, dayl
     real th, dtt, xs, tempcn, tempcx, dopt, acoef, tdsoil, tmsoil, tnsoil 
     integer i
     
     ! Compute Crown Temperature under snow pack
     tempcn = tMin
     tempcx = tMax
     xs     = snow
     xs     = amin1(xs, 15.0)
     
     ! Calculate crown temperature based on temperature and snow cover. 
     ! Crown temperature is higher than tavg due to energy balance of snow pack.
     if(tMin < 0.0) then
        tempcn = 2.0 + tMin*(0.4+0.0018*(xs-15.0)**2)
     end if
     
     if(tMax < 0.0) then
        tempcx = 2.0 + tMax*(0.4+0.0018*(xs-15.0)**2)
     end if
     
     tempcr = (tempcx + tempcn)/2.0
  
     ! Compute thermal time based on new method developed by J.T.R at CYMMIT, 5/5/98.  
     ! DOPT, Devlopment optimum temperature, is set to TOPT during vegetative growth and to ROPT after anthesis
     dopt = tOpt
     if(tlu >= anthes) then
         dopt = rOpt
     end if

     ! Check basic temperature ranges and calculate DTT for development based on PC with JTR
     if(tMax < tBase) then
        dtt = 0.0
     else if(tMin > dopt) then
         
        !This statement replaces DTT = TOPT .. GoL and LAH, CIMMYT, 1999
        dtt = dopt - tBase
          
     !Now, modify TEMPCN, TEMPCX based on soil conditions or snow if corn and sorghum are before 10 leaves
     else if(tlu <= 10.0) then  
        !Check for snow  (should following be GT.0 or GT.15 ?).  
        !Based on snow cover, calculate DTT for the day
        if(xs > 0.0) then
           !Snow on the ground
           dtt = (tempcn + tempcx)/2.0 - tBase
        else
           !No snow, compute soil temperature
           acoef = 0.01061 * srad + 0.5902
           tdsoil = acoef * tMax + (1.0 - acoef) * tMin
           tnsoil = 0.36354 * tMax + 0.63646 * tMin
           if(tdsoil < tBase) then
              dtt = 0.0
           else
              if(tnsoil < tBase) tnsoil = tBase
              if(tdsoil > dopt)  tdsoil = dopt
              tmsoil = tdsoil * (dayl/24.) + tnsoil * ((24.-dayl)/24.)
              if(tmsoil < tBase) then
                 dtt = (tBase+tdsoil)/2.0 - tBase
              else
                 dtt = (tnsoil+tdsoil)/2.0 - tBase
              end if
              !Statement added ... GoL and LAH, CIMMYT, 1999
              dtt = amin1(dtt, dopt-tBase)
           end if   !tdsoil < tBase
        end if   !xs > 0.
          
     !Now, compute DTT for when Tmax or Tmin out of range
     else if(tMin < tBase .OR. tMax > dopt) then
        dtt = 0.0
        do i = 1, 24
           th = (tMax+tMin)/2. + (tMax-tMin)/2. * sin(3.14/12.*i)
           if(th < tBase) th = tBase
           if(th > dopt)  th = dopt
           dtt = dtt + (th-tBase)/24.0
        end do
     else
        dtt = (tmax+tMin)/2.0 - tBase
     end if
     
     getgddcer = amax1(dtt, 0.0)

end function getgddcer    

!-----------------------------------------------------------------------      
! Function to compute GDD[30,10]
!-----------------------------------------------------------------------      
real function getgddten(tMax, tMin, tOpt, tBase)
     real, intent(in) :: tMax, tMin, tOpt, tBase 
     real gddMax, gddMin
     if(tMax < tBase) then
        gddMax = tBase
     else
        gddMax = min(tOpt, tMax)
     end if
 
     if(tMin > tOpt) then
        gddMin = tOpt
     else
        gddMin = max(tBase, tMin) 
     end if       
 
     getgddten = (gddMax + gddMin)/2 - tBase
end function getgddten    

!-----------------------------------------------------------------------      
! Function to compute GTI (General Thermal Index)
!-----------------------------------------------------------------------      
real function getgti(tmpa, tlu, silk)
     real, intent(in) :: tmpa, tlu, silk
       
     if(tlu < silk) then
        getgti = 0.043177*tmpa**2 - 0.000894*tmpa**3
     else
        getgti = 5.3581 + 0.011178*tmpa**2
     end if
    
end function getgti    

!-----------------------------------------------------------------------
! Comparison of growth stages in MAIS and CERES
! growthStage Definitions (MAIS growth stages)    ISTAGEG (CERES)
!                                            7 - Sowing date
!   1 - Germination-9                        8 - Germination                                                 
!   2 - Emergence-1                          9 - Emergence
!   3 - End of juvenile phase-2              1 - End juvenile
!   4 - Tassel initiation-3                  2 - Pannicle initiation
!   5 - Appearance of topmost leaf-3         3 - End leaf growth
!   6 - Anthesis-3
!   7 - Silking-4                            4 - End pannicle growth
!   8 - Onset of linear grain filling-5      5 - Grain fill
!   9 - 50% milk line-6
!  10 - Black layer                          6 - Maturity
!-----------------------------------------------------------------------

!==============================================================================================================================
! Variable definitions                                                                                  Unit 
!------------------------------------------------------------------------------------------------------------------------------
! anthes     Leaf stage at anthesis                                                                     tlu   
! asi        Anthesis to silking interval                                                               tlu
! pgrasi     Average plant growth rate during growth stages 5 and 6                                     g/plant/day
! blackl     Physiological maturity or black layer                                                      dvs
! lfnum      Base leaf number                                                                           leaves
! cgr        Crop growth rate (aboveground dry matter)                                                  kg[dm]/ha/day
! coltlu     Cumulative thermal leaf units required for full leaf expansion                             tlu
! control    Constructed type for control variables
! cumdep     Cumulative depth of soil                                                                   cm
! cumpdw     Cumulative aboveground plant dry weight during growth stages 5 and 6                       g/plant
! dailyttapsim  [Function] Computes the daily thermal time using APSIM's method
! datafileio Constructed type for variables read from DSSAT's input/output file (DSSAT45.INP)
! dayl       Day length on day of simulation (from sunrise to sunset)                                   hour
! dcount     Day counter during temperature and photoperiod sensitive phase                             day
! delayr     Vector of daily mean solar radiation during the previous week                              MJ/m2/day
! dlayr(L)   Soil thickness in layer L                                                                  cm
! dopt       Optimum temperature (local variable)                                                       degree C
! doy        Three-digit day of year                                                                    ddd
! doytnleaf  Year-day of year during the tassel initiation phase (growth stage = 4) and used as
!            the date on which the topmost leaf appeared                                                yyyyddd
! dtt        Daily thermal time (for current day)                                                       degree-day
! dvs        Development stage (0 is planting, 1 is silking, and 2 is maturity) 
! dvsrate    Rate of development stage
! dynamic    Modular control (runinit=1 for run initialization, seasinit=2 for seasonal initialization,
!            rate=3 for rate calculations, integr=4 for integration of state variables, output=5 for
!            writing daily outputs, seasend=6 for closing output files                                 
! elp        Change in leaf number due to photoperiod                                                   leaves 
! elt        Change in leaf number due to temperature                                                   leaves                                                                                 
! extral     Change in leaf number due to temperature and photoperiod                                   leaves
! fnleafap   [Function] Computes the daily rate of leaf appearance (old method)
! fnrla      [Function] Computes the rate of leaf appearance based on hourly or daily temperatures
! fnrlaDaily       [Function] Calculates the daily rate of leaf appearance based on hourly RLA values
! fnrlaHourly      [Function] Calculates the hourly rate of leaf appearance
! gddae      Cumulative daily thermal time after emergence (CERES method)                               degree-day
! gddapsim   Cumulative daily thermal time after planting using APSIM's approach                        degree-day
! gddcer     Cumulative daily thermal time after planting (CERES method)                                degree-day                                                            
! gddten     Cumulative daily thermal time after planting using GDD[10,30]                              degree-day
! getgddcer        [Function] Computes daily thermal time using CERES-Maize's method
! getgddten        [Function] Computes daily thermal time using GDD[10,30] method
! getgti           [Function] Computes General Thermal Index (GTI)
! gfp              Duration of grain filling period                                                     gti
! growthtlu(i)     Leaf stage at which growth stage i occurred                                          tlu
! gstddoySim(i)    Component day of year of gstdyrdoySim                                                ddd
! gstdyearSim(i)   Component year of gstdyrdoySim                                                       yyyy
! gstdyrdoySim(i)  Simulated year and day of year for growth stage i                                    yyyyddd
! growthStage      Integer value of growth stage
! gti        General Thermal Index heat unit accumulation                                               gti (Q.what is the real unit of gti?)
! gtir       Rate of GTI accumulation                                                                   gti/day
! i          Integer loop counter
! isdate
! iswitch    Constructed type for control switches
! iswwat     Water balance simulation switch (Y/N)
! L          Loop counter
! L0         Temporary soil layer number
! lfnc          Node position of largest leaf 
! lftop         Number of leaves (or leaf nodes) above the largest (and most longevous) leaf            leaves
! ll(nl)     Volumetric soil water lower limit                                                          cm3/cm3
! lsint      Interval between time of emergence of topmost leaf and anthesis                            tlu
! mdate      Maturity date                                                                              yyyyddd
! meant      Mean air temperature during photoperiod- and temperature-sensitive period                  degree C
! milk       Half milk line                                                                             dvs
! mrad       Mean of daily incident solar radiation during the last week                                MJ/m2/day 
! mulch      Constructed type for variables describing mulch properties
! nlayr      Number of soil layers
! olg        Onset linear dry matter accumulation of grain                                              dvs
! pltpop     Plant density                                                                              plants/m2
! photp      Photoperiod sensitivity (leaves/hour above 12 hours)                                       leaves/hour
! rla        Rate of Leaf tip Appearence                                                                leaves/day
! rlaAir     Rate of leaf tip appearance using air temperatures                                         leaves/day
! rlaSoil    Rate of leaf tip appearance uing soil temperature of layer 1                               leaves/day
! rlad       Daytime rate of leaf tip appearance (based on TMAX)                                        tlu/day
! rlaf       Effect of solar radiation (low levels) on the rate of leaf appearance
! rlamax     Maximum rate of leaf appearance                                                            leaves/day
! rlan       Night-time rate of leaf tip appearance (based on TMIN)                                     tlu/day
! sdepth     Sowing depth                                                                               cm
! silk       Leaf stage at silking                                                                      tlu 
! snow       Snow depth                                                                                 mm
! SoilProp   Constructed type for variables related to soil properties
! srad       Incident solar radiation                                                                   MJ/m2/day
! srftemp    Temperature of soil surface litter                                                         degree C
! st(l)      Soil temperature of layer l                                                                degree C
! stLayer1   Soil temperature of layer 1                                                                degree C
! sumr       Accumulated solar radiation during past 7 days                                             MJ/m2
! sw(l)      Volumetric soil water content in layer l                                                   cm3/cm3 
! swsd       Modified soil water content for computing emergence (KD: or germination?)                  cm3/cm3
! swcg       Minimum soil water available required for germination to occur                             cm3/cm3
! tbase      Base temperature for daily thermal time calculation                                        degree C
! tbten      Base temperature for computing gdd[30,10]                                                  degree C
! TCeiling   Ceiling temperature for computing the rate of leaf appearance                              degree C
! tempthreehr  [Function] Estimates three-hour air temperatures based on daily max and min
! th         Hourly temperature                                                                         degree C
! timdif     [Function] For computing difference between two dates                                      returns number of days 
! tlu        Thermal leaf unit (cumulative rate of leaf tip appearance)                                 tlu
! tmax       Maximum daily temperature                                                                  degree C
! tmin       Minimum daily temperature                                                                  degree C
! TminRla
! tmpa       Daily mean air temperature                                                                 degree C
! tnleaf     Total number of initiated leaves                                                           leaves
! topt       Optimum temperature for daily thermal time calculation                                     degree C
! TOptRla    Optimum temperature for computing the rate of leaf appearance                              degree C
! tothirty   Optimum temperature for computing gdd[30,10]                                               degree C
! tsum       Sum of daily mean temperature during temperature- and photoperiod-sensitive phase          degree C
! weather    Constructed type for weather variables
! xtemp      Vector of temperatures used for interpolation thermal time for the APSIM thermal time
!            accumulator                                                                                degree C
! year       Four-digit year                                                                            yyyy
! ygdd       Vector of growing degree days used for interpolation thermal time for the APSIM 
!            thermal time accumulator                                                                   degree-day
! yrdoy      Year day-of-year                                                                           yyyyddd
! yremrg
! yrplt      Planting date                                                                              yyyyddd
!==============================================================================================================================
