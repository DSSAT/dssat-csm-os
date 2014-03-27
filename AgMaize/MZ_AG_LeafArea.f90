!======================================================================================
! MZ_GM_LeafArea
! Calculate leaf area expansion based on Thijs Tollenaar' model:
!    (1) Leaf area distribution by leaf number (Dwyer & Stewart, 1987)
!    (2) Rate and duration of expansion of individual leaves (Stewart & Dwyer, 1994)
!    (2) Effect of temperature on LAI from 4 to 12 leaf stage (Tollenaar, 1989)
!    (3) Effect of plant density on LAI (Grignon 1994 dataset)
!----------------------------------------------------------------------
! Revision History
! 08/15/2012 TT/SK/KAD Translated from MS Basic (originally written by Tollenaar et al.)
!----------------------------------------------------------------------
! Called from: Main program: MZ_AG_AGMAIZE
! Calls      : None
!----------------------------------------------------------------------
! TO DO: Influence of leaf mutual shading
!        Effect of water and nutrient stresses on leaf expansion and senescence
!        Check coltlu after tnleaf
! TO CHECK:
! Reduction in leaf max area due to plant density might be too strong?
! Reduction in rate of leaf expansion due to temperature might be too strong? Dr. Boote
! thinks temperature is already influencing the rate of leaf appearance OR we should
! use a linear function with 4 cardinal temperatures as in CROPGRO? 6/20/2013
!----------------------------------------------------------------------
!======================================================================================
subroutine MZ_AG_Leafarea(control, tlu, tnleaf, lfnc, tmpa, rla,                & !Input 
           gddcer, dtt, dvs, dvsrate, olg, swfac, gstdyrdoySim,                 & !Input 
           pla, lai, latf, lapot, greenla, greenlaSilk,                         & !Output
           lamaxpot, lftips, lfcols, lfn, maxlarf, lflon, gddladur)               !Output    

use ModuleDefs
use MZ_AG_ModuleDefs
implicit  none
save      

!----------------------------------------------------------------------
! Define Variables
!----------------------------------------------------------------------
integer, parameter :: d=5, lenla=50, m2tocm2=1e4                          
real, intent(in) :: tlu, tnleaf, tmpa, rla, gddcer, dtt, dvs, dvsrate, olg
type (ControlType), intent(in):: control                          
type(FileioType)  :: datafileio      
type(SpeciesType) :: dataspecies                             
real, intent(out) :: pla, lai, latf, lftips, lfcols, maxlarf, gddladur
real, dimension(lenla), intent(out) :: lapot, greenla, greenlaSilk, lamaxpot, lflon

! Constructed type variables
integer :: dynamic, yrdoy

! Other variables
character :: files*12, pathsr*80
integer :: idelt, iidelt, i, ii, icol, itip, lfn, gstdyrdoySim(20)
real :: pltpop, lftop, lalfx, ampli, asymp, nstress, swfac, tlud, lfnc, diftc, gddcery, lnl, wha
real :: tempCoef(3,3), decimal, coltlu
real, dimension(lenla) :: rlepot, rle, rlspot, rls, lamax, gddlabeg, gddlaend, collartlu, lfcollars

! Functions
real getcoltlu           !Function for computing collar tlu (see end of this subroutine)
real tempeffect          !Function for computing temperature effect on leaf expansion

! Transfer values from constructed data types into local variables
dynamic = control % dynamic
yrdoy   = control % yrdoy
     
!----------------------------------------------------------------------
! Dynamic = runinit or dynamic = seasinit
!----------------------------------------------------------------------
if(dynamic==runinit .OR. dynamic==seasinit) then
 
  !Initialize some variables
  latf = 1.0        
  pla = 0.0
  lai = 0.0
  greenla = 0.0
  greenlaSilk = 0.0
  lapot = 0.0
  lamaxpot = 0.0
  lamax = 0.0
  gddlabeg = 0.0
  gddlaend = 0.0
  lftips = 0.0
  lfcols = 0.0
  icol = 0
  itip = 0
  coltlu = 0.0
  maxlarf = 1.0
  lflon = 0.0
  gddladur = 0.0
  nstress = 1.0
  swfac = 1.0
  lfn = 0.0
  lfcollars = 0.0
  
  !Read all sections of fileio and transfer variables
  call readfileio(control, 'ALLSEC', datafileio)
  pltpop = datafileio % pltpop 
  lftop  = datafileio % lftop
  lalfx  = datafileio % lalfx * m2tocm2
  ampli  = datafileio % ampli
  asymp  = datafileio % asymp
  files  = datafileio % files
  pathsr = datafileio % pathsr
  
  !Read temperature coefficients parameters from species file and transfer variables
  call readspecies(files, pathsr, '*LEAF ', dataspecies)
  tempCoef = dataspecies % tempCoef
   
!----------------------------------------------------------------------
! Dynamic = rate 
!----------------------------------------------------------------------
else if(dynamic == rate) then


!-----------------------------------------------------------------------
! Dynamic = integr
!-----------------------------------------------------------------------
else if(dynamic == integr) then
      
  !---Temperature effect on leaf expansion
  latf = tempeffect(tlu, tmpa, tempCoef)
  if(latf <= 0.0) latf = 0.0    !To prevent negative latf when tmpa is not yet computed
        
  !---Preliminary calculations
  lfn = ceiling(tnleaf)       !This is equivalent to the Basic code fix(tnleaf+1) since tnleaf > 0
  
  !---Leaf senescence parameters (see Lisazo et al. 2003)
  lnl = lfnc
  wha = tnleaf/3               !Equation 12, Width of the relationship between leaf longevity and nodal position at half amplitude

  !Maximum leaf area reduction factor; based on Lizaso's Grignon dataset 1994; 2/01/2013
  !For low plant lai (< 2) reduction factor depends on plant density (plants sense each other)
  !If lai >= 2, reduction factor depends upon lai itself because of shading.
  if(lai < 2.0) then
     maxlarf = exp(-0.0085*(max(pltpop,4.0) - 4.0))     !Attn: this equation is again used later in the season when senescence brings LAI down to < 2
  else  
     maxlarf = min(1.1589 - 0.0873*lai, exp(-0.0085*(max(pltpop,4.0)-4.0)))
  end if  
 
  !---Leaf expansion loop
  tlud = tlu - rla               !tlu is cumulative rla so tlud is yesterday's tlu
  gddcery = gddcer - dtt         !gddcery is yesterday's gdd
  do idelt = 1, d                
     tlud = tlud + rla/d         !Increment tlud by (1/d)th in this loop
     gddcery = gddcery + dtt/d   !Add today's gdd to yestersday's, 1/dth at a time
    
     EXPANDLEAF: do i = 1, lfn                
        if(tlud < i) exit EXPANDLEAF       !Not enough tlu for tip of leaf i to appear
        
        if(gddlabeg(i) == 0.0) then    !Tip of leaf i has already appeared, save corresponding gdd
           gddlabeg(i) = gddcery
           !lftips = i                  
        end if   
       
        !Compute collar tlu for each expanding leaf depending on their position relative to the largest leaf (lfnc)
  	    !collar tlu beyond lfnc increases linearly to a maximum of tnleaf+3
  	    if(real(i) < tnleaf) then
  	       coltlu = getcoltlu(real(i), lfnc)
  	       diftc = coltlu - real(i)
  	       !diftc = coltlu - tlud ??
  	    else 
  	       coltlu = coltlu + tnleaf - real(i-1)
           !coltlu = coltlu + diftc*(real(i)-tnleaf)/3.0      
        end if
        collartlu(i) = coltlu
  	  
        EXPANDING: if(tlud <= coltlu) then         !Leaf is still expanding
           !Compute potential area of leaf i (Stewart and Dwyer 1994, equation 9)
           lamaxpot(i) = lalfx * exp(-0.0359*(i-lfnc)**2. + 0.00117*(i-lfnc)**3.)   !lfnc = 12.4, to be checked against data
              
           !Rate of leaf area expansion (unit area/day/leaf). Area of an expanding leaf between the date
           !of leaf-tip and leaf-collar appearance = lamaxpot(i) * (tlud - i) / (coltlu – i); adapt this to have a logistic type growth?
           rlepot(i) =  maxlarf * latf * lamaxpot(i) * (rla/real(d)) / (coltlu-real(i))    
           rle(i) = rlepot(i)*min(swfac, nstress)
           
  	       !itip = icol + 1
  	       !if(tlud <= collartlu(itip) .AND. i == itip) lfcols = lfcols + (rla/real(d)) / (collartlu(itip)-real(itip))
  	       
 	       !Integrate for each leaf (move to integrate); lapot(i) is cumulative leaf area for leaf i (m2/leaf)
  	       if(real(i) <= tnleaf) then 
              lapot(i) = lapot(i) + rlepot(i)
              greenla(i) = greenla(i) + rle(i)                         
           else 
           !Since lfn is ceiling(tnleaf), add growth for the decimal part of tnleaf
              lapot(i) = lapot(i) + rlepot(i)*decimal(tnleaf)        
              greenla(i) = greenla(i) + rle(i)*decimal(tnleaf)
           end if
                      
        else EXPANDING                            !Expansion is completed for current leaf
           if(gddlaend(i) == 0.0) then
              gddlaend(i) = gddcery
              lamax(i) = greenla(i)               !Maximum leaf area after accounting for stresses
              icol = i                            !Node number of collar that has appeared (or leaf that has completed expansion)
              lfcols = real(icol)
              gddladur = gddlaend(i) - gddlabeg(i)
           end if
           
        end if EXPANDING                  !Leaf is still epanding
  
     end do EXPANDLEAF    !Leaf expansion loop
  end do                  !tlud increment loop

  !---Begin leaf senescence loop 
  !Based on Lizaso et al. 2003 Field Crops Res. 80:1-17. For senescence, perhaps use unit increment 
  !of GDD as D like IXIM. Set timing of appearance of first and second leaves equal
  if(gddlabeg(2) /= 0.0) gddlabeg(1) = gddlabeg(2)

  gddcery = gddcer - dtt             !Restore yesterday's gdd
  do idelt = 1, d
     gddcery = gddcery + dtt/d       !Add today's gdd to yestersday's 1/dth at a time
  
     SENESCELEAF: do ii = 1, lfn
      ISEXPANDING: if(gddlabeg(ii) > 0.0) then    !If leaf has started expanding
        lflon(ii) = asymp + ampli*exp(-(ii-lnl)**2.0/(2.0*wha**2.0))   !Longevity of iith leaf
             
        !Or if(gddcery > (gddlabeg(ii)+lflon(ii))) then compute rate of leaf senescence
        if(gddcery <= (gddlabeg(ii)+lflon(ii))) then         !Senescence has not started yet
           rlspot(ii) = 0.0
           rls(ii) = 0.0                                     !Formerly cycle
        else if(gddcery > (gddlaend(ii)+lflon(ii))) then     !Leaf has fully senesced
           cycle SENESCELEAF
        else
           !Rate of leaf senescence (negative rate)
           rlspot(ii) = (-1) * lamax(ii) * (dtt/d) / (gddlaend(ii)-gddlabeg(ii))   
        end if
             
        !Integrate; greenla getting negative, senescence rate might be too high. 
        !We need to address this specifically instead of truncating at 0.0001
        if(ii <= tnleaf) then
           greenla(ii) = max(1e-4, greenla(ii) + rls(ii))           
        else
           !Don't over-senesce the last leaf
           greenla(ii) = max(1e-4, greenla(ii) + rls(ii)*decimal(tnleaf))              
        end if
      end if ISEXPANDING  
     end do SENESCELEAF
  end do
 
  !---Check upper bound: some problems related to photoperiod sensitivity:
  !lamaxpot dependency on a varying tnleaf
  !lapot(1:lfn) = (/ (min(lapot(i), lamaxpot(i)), i=1,lfn) /)
  !greenla(1:lfn) = (/ (min(greenla(i), lamaxpot(i)), i=1,lfn) /)
 
  !---Plant leaf area and LAI
  pla = sum(greenla)/m2tocm2    !in m2/plant
  lai = pla*pltpop              !in m2/m2
  lftips = min(tlu, tnleaf)
  if(yrdoy == gstdyrdoySim(7)) greenlaSilk = greenla
  
	   
!-----------------------------------------------------------------------
! End of dynamic if structure
!-----------------------------------------------------------------------
end if  

!----------------------------------------------------------------------
! End of subroutine
!----------------------------------------------------------------------
return      
end subroutine MZ_AG_LeafArea
 
!==============================================================================================================================
! Functions used in this leaf area subroutine
!-----------------------------------------------------------------------
! Function to compute collar TLU for each expanding leaf depending on their 
! position relative to the largest leaf, and an input TLU
! Collar TLU beyond largest leaf increases linearly to a maximum of TNLEAF+3
!-----------------------------------------------------------------------
!real function getcoltlu(leafTip, largeNode)
!  integer, intent(in) :: leafTip
!  real, intent(in) :: largeNode
!  real leafTipReal
!  leafTipReal = real(leafTip)
!  if(leafTipReal == 1.0) then         
!     getcoltlu = 3.0  
!  else if(leafTipReal > 1.0 .AND. leafTipReal <= largeNode) then
!     getcoltlu = 1.7*leafTipReal + 1 
!  else if(leafTipReal > largeNode) then 
!     getcoltlu = (1.7*largeNode + 1) + (leafTipReal-largeNode)
!  end if 
!end function getcoltlu    

!-----------------------------------------------------------------------      
! Function to compute temperature effect on leaf expansion
! Depends only on average daily temperature and replaces the previous
! data based interpolation procedure. 9/14/2012
!-----------------------------------------------------------------------    
function tempeffect(leafNum, tAvg, tCoef) result(latf)
   real, intent(in) :: leafNum, tavg, tCoef(3,3)
   real :: latf
   real :: latf1, latf2
   if(leafNum < 4.0) then
       latf = tCoef(1,1)*tAvg**2 +tCoef(1,2)*tAvg +tCoef(1,3)
   else if(leafNum >= 4.0 .and. leafNum < 8.0) then
       latf1 = tCoef(1,1)*tAvg**2 +tCoef(1,2)*tAvg +tCoef(1,3)
       latf2 = tCoef(2,1)*tAvg**2 +tCoef(2,2)*tavg +tCoef(2,3)
       latf = latf1 + (latf2-latf1)/4*(leafNum-4.0)
   else if(leafNum >= 8.0 .and. leafNum < 12.0) then
       latf1 = tCoef(2,1)*tAvg**2 +tCoef(2,2)*tAvg +tCoef(2,3)
       latf2 = tCoef(3,1)*tAvg**2 +tCoef(3,2)*tAvg +tCoef(3,3)
       latf = latf1 + (latf2-latf1)/4*(leafNum-8.0)
   else
       latf = tCoef(3,1)*tAvg**2 +tCoef(3,2)*tAvg +tCoef(3,3)
   end if
end function tempeffect                  

      
!==============================================================================================================================
! Variable definitions                                                                                     Unit 
!------------------------------------------------------------------------------------------------------------------------------
! lalfx         Area of the largest leaf on the plant under non-stressed conditions                        cm2
! ampli         Amplitude of the leaf longevity vs. leaf nodal position relationship                       degree-day
! asymp         Asymptote of the leaf longevity vs. leaf nodal position relationship                       degree-day
! coltlu        Thermal leaf units from leaf tip appearance to full leaf expansion                         tlu
! control       Constructed type for control variables
! datafileio    Constructed type for variables read from DSSAT's input/output file (DSSAT45.INP)
! d             Number of steps for calculation of leaf area expansion and senescence
! diftc         
! dtt           Today's thermal time                                                                       degree-day
! dvs           Development stage (0 is planting, 1 is silking, and 2 is maturity) 
! dvsrate       Rate of development stage
! dynamic       Modular control (runinit=1 for run initialization, seasinit=2 for seasonal initialization,
!               rate=3 for rate calculations, integr=4 for integration of state variables, output=5 for
!               writing daily outputs, seasend=6 for closing output files                                                                            
! gddcer        Cumulative daily thermal time (base 8) from planting (CERES method)                        degree-day
! gddcery       Cumulative daily thermal time (base 8) from planting until yesterday                       degree-day
! gddlabeg(i)   Cumulative daily thermal time (base 8) from planting to the appearance of leaf i's tip     degree-day
! gddladur      Duration of leaf expansion                                                                 degree-day
! gddlaend(i)   Cumulative daily thermal time (base 8) from planting to the appearance of leaf i's collar  degree-day
! getcoltlu     [Function] For computing thermal leaf unit at which leaf is fully expanded 
! greenla(i)    Cumulative area of leaf i during expansion and senescence                                  cm2/leaf
! greenlaSilk(i)
! i             Integer loop counter
! ii            Counter of leaf number during leaf senescence
! idelt         Counter of D steps during leaf expansion
! iidelt        Counter of D steps during leaf senescence
! lai           Whole-plant leaf area index                                                                m2[leaf]/m2[ground]
! lamax(i)      Maximum area actually reached by leaf i at full expansion                                  m2/leaf
! lamaxpot(i)   Maximum area of leaf i under non-stressed conditions                                       m2/leaf
! lapot(i)      Cumulative area of leaf i during expansion (tip to collar) under non-stressed conditions   m2/leaf
! latf          Effect of temperature on leaf growth
! lenla         Arbitrary definition of the length of leaf area vector
! lfcols        Leaf node position of leaf that has completed expansion
! lfn           Total leaf number rounded up
! lfnc          Node position of largest leaf 
! lflon(i)      Longevity of leaf i                                                                        degree-day
! lnl           Node position of leaf with the largest longevity
! maxlarf       Reduction factor of maximum leaf area due to plant density
! m2tocm2       Constant for converting areas from m2 to cm2
! nstress       Nitrogen stress factor
! olg           Onset linear dry matter accumulation of grain                                              dvs
! pla           Plant leaf area                                                                            m2/plant
! pltpop        Plant density                                                                              plants/m2
! rla           Mean daily rate of leaf appearance                                                         tlu/day
! rle(i)        Actual rate of area expansion of leaf i                                                    m2/leaf/day
! rlepot(i)     Rate of area expansion of leaf i under non-stressed conditions                             m2/leaf/day
! rls(i)        Actual rate of area senescence of leaf i                                                   m2/leaf/day
! rlspot(i)     Rate of area senescence of leaf i under non-stressed conditions                            m2/leaf/day
! tempCoef      Coefficient for estimating temperature effect on leaf expansion
! tempeffect    [Function] For calculating temperature effect on leaf expansion
! tlu           Cumulative thermal leaf unit                                                               tlu
! tlud          Accumulated thermal leaf unit until yesterday                                              tlu
! tmpa          Mean air temperature for current day                                                       degree C
! tnleaf        Total number of initiated leaves on the plant                                              leaves
! wha           Width of the leaf nodal position vs. leaf longevity relationship at half amplitude         leaves   
! swfac         Water stress factor
!==============================================================================================================================

