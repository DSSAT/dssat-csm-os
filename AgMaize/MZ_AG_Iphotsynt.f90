!=======================================================================
!  Iphotsynt, Subroutine, J.I. Lizaso
!  Calculates instantaneous plant photosynthesis (µmol CO2/m2 s) of 
!  shaded and sunlit leaf area on a per-leaf basis and integrates for
!  the whole canopy
!  Adapted from CANOPG by K.J. Boote, J.W. Jones, G. Hoogenboom
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  02/07/2003 JIL Written
!  09/12/2007 JIL Adapted for IXIM model
!  03/13/2013 KAD/TT/SK Adapted for AgMaize
!  08/28/2013 KAD Added definitions of variables
!-----------------------------------------------------------------------
!  Called from: MZ_AG_GLOBAL
!  Calls:       MZ_AG_PSParam, MZ_AG_PSLeaf (included here)
! 
! Questions for Jon:
! -What is pgsum? Looks like it is not being used?
! -Parameters for equations to estimate light response curve parameters in MZ_GM_PSParam? What is axx?
! -Data for the equations mentioned above
! -Temperature effect for curvature parameter in MZ_GM_PSParam tair**2 instead of tair*2
!=======================================================================
subroutine MZ_AG_Iphotsynt(dynamic,                      & !Control
     asmax, gddae, gla, plaisl, plaish, lap, lfl, lfn,   & !Input
     light, parsh, parsun, tairh, yx,                    & !Input
     pghr)                                                 !Output

use ModuleDefs
implicit none
save    

logical :: light
integer :: dynamic, i, lfn
real :: asmax, gddae, pghr, pgsum, pgsun, tairh
real,dimension(50) :: assat, cvxty, gla, intslp, lap, lfl, parsh, parsun, pgsh, pgsl, plaish, plaisl, yx 

!----------------------------------------------------------------------
! Dynamic = runinit
!----------------------------------------------------------------------
if(dynamic==runinit .or. dynamic==seasinit) then

! Initialize.
i     = 1
pghr  = 0.0
pgsum = 0.0
pgsun = 0.0
pgsh  = 0.0
pgsl  = 0.0

call MZ_AG_PSParam(dynamic,                                & !Control
     asmax, gddae, gla, lap, lfl, lfn, light, tairh, yx,   & !Input
     assat, cvxty, intslp)                                   !Output 

!-----------------------------------------------------------------------
! Dynamic = rate 
!-----------------------------------------------------------------------
else if(dynamic==rate) then


!-----------------------------------------------------------------------
! Dynamic = rate 
!-----------------------------------------------------------------------
else if(dynamic==integr) then

! JIL Calculate light response curve parameters per leaf as a function of leaf age and hourly air temperature
call MZ_AG_PSParam(dynamic, asmax, gddae, gla, lap, lfl, lfn, light, tairh, yx,    & !Input
                   assat, cvxty, intslp)                                             !Output 

! Calculate per-leaf assimilation
pghr = 0.0
do i = 1, lfn
   if(gla(i) > 0.05*yx(i)) then
      pgsum = 0.0

   !Compute photosynthesis for sunlit leaves
   call MZ_AG_PSLeaf(parsun(i), assat(i), intslp(i), cvxty(i),    & !Input
                     pgsun)                                         !Output
   pgsl(i) = pgsun

   !Compute photosynthesis for shaded leaves
   call MZ_AG_PSLeaf(parsh(i), assat(i), intslp(i), cvxty(i),    &  !Input
                     pgsh(i))                                       !Output
  
   !Compute instantaneous canopy gross photosynthesis (µmol[CO2]/m2/s)
   pghr = pghr + pgsl(i)*plaisl(i) + pgsh(i)*plaish(i)
   end if
end do

end if       !Endif for DYNAMIC LOOP

return

end subroutine MZ_AG_Iphotsynt


!==============================================================================================================================
! Variable definitions                                                                                  Unit 
!------------------------------------------------------------------------------------------------------------------------------
! assat(l)     Light-saturated assimilation rate (corresponds to the asymptote of the light             umol[CO2]/m2[leaf]/s
!              response curve) for leaf l
! cvxty(l)     Ratio of the diffusion resistance to the total resistance to CO2 (curvature              -
!              parameter for the light response curve) for leaf l
! dynamic      Main control variable to tell each module which section of code to run                   -
! asmax        Maximum instantaneous assimilation at 30 degC                                            µmol[CO2]/m2[leaf]/s
! gddae        Cumulative growing degree day after emergence                                            degree-days
! gla(l)       Green leaf area for leaf l                                                               cm2
! i            Iteration counter of leaf number                                                         -
! intslp(l)    Quantum efficiency of CO2 assimilation (initial slope of the light response              µmol[CO2]/µmol[quanta]
!              curve) for leaf l
! plaish(l)    Shaded leaf area index for leaf l                                                        m2/m2
! plaisl(l)    Shaded leaf area index for leaf l                                                        m2/m2
! lap(l)       Potential leaf area of leaf l from tip appearance to full expansion                      cm2
! lfl(l)       Longevity of leaf l                                                                      degree-days
! lfn          Total leaf number rounded up                                                             -
! light        Logical variable that differentiates daytime from nighttime
! parsh(l)     Photosynthetically active radiation absorbed by leaf l in the shaded zone                J/m2/s
! parsun(l)    Photosynthetically active radiation absorbed by leaf l in the sunlit zone                J/m2/s
! pghr         Canopy instantaneous gross assimilation                                                  µmol[CO2]/m2[ground]/s
! pgsh(l)      Shaded photosynthesis for leaf l                                                         µmol[CO2]/m2[leaf]/s
! pgsl(l)      Sunlit photosynthesis for leaf l                                                         µmol[CO2]/m2[leaf]/s 
! pgsun(l)     Sunlit photosynthesis for leaf l (intermediary variable)                                 µmol[CO2]/m2[leaf]/s 
! tairh(h)     Hourly air temperature                                                                   degrees   
! yx(l)        Maximum value of potential leaf area for leaf l (when fully expanded)                    cm2
!==============================================================================================================================


!=======================================================================
!  MZ_AG_PSParam, Subroutine, J.I. Lizaso
!  Calculates parameters of light response curve per leaf as affected by
!  leaf age and hourly air temperature
!  Adapted from PGLFEQ by K.J. Boote, N.B. Pickering
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  02/07/2003 JIL Written
!  09/12/2007 JIL Adapted for IXIM model
!-----------------------------------------------------------------------
!  Called from: Iphotsynt
!  Calls:       
!=======================================================================
subroutine MZ_AG_PSParam(dynamic, asmax, gddae, gla, lap, lfl, lfn, light, tairh, yx,    & !Input
                         assat, cvxty, intslp)                                             !Output 

use ModuleDefs
implicit none
save     

integer :: dynamic, i, lfn
logical :: light
real :: asf, ak, asmax, axx, axz, ayz, ck, cx, cxf, cxz, exponent, gddae, isf, tairh, xx
real,dimension(50) :: apk, asat, assat, cvty, cvxty, gla, insl, intslp, lap, lfl, yx

!----------------------------------------------------------------------
! Dynamic = runinit
!----------------------------------------------------------------------
if(dynamic==runinit .or. dynamic==seasinit) then

! Initialize
  asf    = 0.0
  ak     = 0.0
  axx    = 0.0
  axz    = 0.0
  ayz    = 0.0
  ck     = 0.0
  cx     = 0.0
  cxf    = 0.0
  cxz    = 0.0
  i      = 1
  isf    = 0.0
  xx     = 0.0
  apk    = 0.0
  asat   = 0.0
  assat  = 0.0
  cvty   = 0.0
  cvxty  = 0.0
  insl   = 0.0
  intslp = 0.0

! JIL Parameters are calculated once a day and updated hourly with changing hour temperature  

!-----------------------------------------------------------------------
! Dynamic = integr
!-----------------------------------------------------------------------
else if(dynamic==integr) then

! JIL Calculating light response curve parameters for each leaf
if(light) then
light = .false.
do i = 1, lfn
  asat(i) = 0.0
  cvty(i) = 0.0
  insl(i) = 0.0
  if(gla(i) > 0.0) then
    xx = lap(i)/yx(i)       !KAD: Age of leaf while it is expanding
    if(xx < 0.99) then      !Expanding leaf
      ayz = 0.66
      axx = 0.34
      ak = 10.0
      axz = 0.5
      cx = 0.95
      ck = 3.55
      cxz = 0.186
      insl(i) = 0.06         !KAD: No effect of leaf age on quantum efficiency during expansion
    else                     !Leaf maturing after complete expansion
      if(apk(i) < 1.0) then
        apk(i) = gddae
      end if
      ! CHP 11/27/2007 CHECK FOR LFL(I) = 0.
      if(lfl(i) > 1.E-10) then
         xx = (gddae-apk(i))/lfl(i)   !KAD: Age of leaf while it is senescing
      else
         xx = 1.0
      end if

      ayz = 0.18
      axx = 0.85
      ak = -7.0
      axz = 0.47
      cx = 0.95
      ck = -16.7
      cxz = 0.88
      !KAD: Effect of leaf age on quantum efficiency during leaf senescence
      exponent = 8.0*(xx - 0.75)
      if(exponent < -40.0) then
         insl(i) = 0.06
      else if(exponent > 40.0) then
         insl(i) = 0.04
      else
         insl(i) = 0.04+(0.02/(1.0 + exp(exponent)))
      end if
    end if
    !Leaf age determined. Continue to apply its effect on asat and cvty

    asat(i) = asmax*(ayz+(axx/(1.0+exp(-ak*(xx-axz)))))
    cvty(i) = cx/(1.0+exp(-ck*(xx-cxz)))
    if(apk(i) > 1.0 .and. xx < 0.25) then
       cx = 0.9
       ck = 0.2
       cvty(i) = cx + (ck*xx)
    end if
  end if    !gla(i) > 0.0 loop
end do
end if      !if(light) loop

! JIL Effect of T on light curve parameters curve parameters are defined at 30 C and scaled using hourly air temp
asf = 0.0886 - 0.00867*tairh + 0.002840*tairh**2.0 - 0.00005070*tairh**3.0
isf = 0.6783 + 0.02910*tairh - 0.000756*tairh**2.0 + 0.00000513*tairh**3.0
cxf = 1.0108 - 0.00050*tairh - 0.000010*tairh**2.0 + 0.00000050*tairh**3.0

do i = 1, lfn
   assat(i)  = asat(i)*asf*1.0
   intslp(i) = insl(i)*isf*1.0	
   cvxty(i)  = cvty(i)*cxf*1.0
end do

end if       !Endif for DYNAMIC LOOP

return

end	subroutine MZ_AG_PSParam     


!==============================================================================================================================
! Variable definitions                                                                                  Unit 
!------------------------------------------------------------------------------------------------------------------------------
! ak           Curvature parameter for the relationship between light-saturated assimilation rate
!              and leaf age
! apk(l)       Cumulative thermal time after emergence when expansion of leaf l was completed           degree-days
! asf          Relative effect of temperature on the light-saturated assimilation rate                  -
! asat(l)      Light-saturated assimilation rate for leaf l (no temperature effect applied)             umol[CO2]/m2[leaf]/s
! asmax        Maximum instantaneous assimilation at 30 degC                                            µmol[CO2]/m2[leaf]/s
! assat(l)     Light-saturated assimilation rate (corresponds to the asymptote of the light             µmol[CO2]/m2[leaf]/s
!              response curve) for leaf l
! axx          "Upper end" of the sigmoid relatiohsip between light-saturated assimilation rate
!              and leaf age
! axz          Relative leaf age at which 50% of axx is reached
! ayz          Vertical offset of the relationship between light-saturated assimilation rate
!              and leaf age
! ck           Parameter controlling the shape of the sigmoid relationship between the light response 
!              curve curvature parameter (cvxty) and leaf age
! cx           "Upper end" of the sigmoid relationship between the light response curve curvature
!              parameter (cvxty) and leaf age
! cxf          Relative effect of temperature on the curvature parameter of the light response curve    -
! cxz          Relative leaf age when 50% of cx is reached
! cvty(l)      Curvature parameter of light response curve for leaf l (no temperature effect applied)   -
! cvxty(l)     Ratio of the diffusion resistance to the total resistance to CO2 (curvature              -
!              parameter for the light response curve) for leaf l
! dynamic      Main control variable to tell each module which section of code to run                   -
! exponent     Intermediary variable
! gddae        Cumulative growing degree days after emergence                                           degree-days
! gla(l)       Green leaf area for leaf l                                                               cm2
! i            Iteration counter of leaf number                                                         -
! insl(l)      Quantum efficiency of CO2 assimilation for leaf l (no temperature effect applied)        µmol[CO2]/µmol[quanta]
! intslp(l)    Quantum efficiency of CO2 assimilation (initial slope of the light response              µmol[CO2]/µmol[quanta]
!              curve) for leaf l
! isf          Relative effect of temperature on the quantum efficiency of CO2 assimilation             -
! lap(l)       Potential leaf area of leaf l from tip appearance to full expansion                      cm2
! lfl(l)       Longevity of leaf l                                                                      degree-days
! lfn          Total leaf number rounded up                                                             -
! light        Logical variable that differentiates daytime from nighttime
! tairh(h)     Hourly air temperature                                                                   degrees   
! xx           Relative leaf age during expansion and senescence
! yx(l)        Maximum value of potential leaf area for leaf l (at full expansion)                      cm2
!==============================================================================================================================



!=======================================================================
!  MZ_AG_PSLeaf, Subroutine, J.I. Lizaso
!  Calculates gross photosynthesis (µmol CO2/m2 s) per unit leaf area as
!  a function of instantaneous PAR (µmol/m2 s)
!  Adapted from PGLEAF by K.J.Boote, J.W.Jones, G.Hoogenboom
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  02/07/03 JIL Written
!  09/12/2007 JIL Adapted for IXIM model
!  08/28/2013 KAD Added variable definitions
!-----------------------------------------------------------------------
!  Called from: Iphotsynt
!  Calls:       None    
!=======================================================================
subroutine MZ_AG_PSLeaf(parlf, ast, isp, cvt,                &  !Input
                        pglf)                                   !Output

implicit none
save 
real :: a, b, c, ast, cvt, isp, parlf, pglf

! Initialize
a    = 0.0
b    = 0.0
c    = 0.0
pglf = 0.0

! JIL Using a non-rectangular hyperbolae (Thornley and Johnson, 1990)
! PGLF is average leaf gross assimilation (umol/m2 s)
! KAD: for details, see equations 39 and 40 in Lizaso et al. 2005 Agron. J. 97
a = cvt
b = (isp*parlf) + ast
c = isp * parlf * ast

! JIL 09/17/09
  if(ast > 0.0 .and. a > 0.0) then
     pglf = (b - sqrt(b**2.0 - 4.0*a*c)) / (2.0*a)
   else
     pglf = 0.0
  endif
 
return

end subroutine MZ_AG_PSLeaf

!==============================================================================================================================
! Variable definitions                                                                                  Unit 
!------------------------------------------------------------------------------------------------------------------------------
! a, b and c   Parameters of a generalized non-rectangular hyperbolae                                       
! ast          Light-saturated assimilation rate (corresponds to the asymptote of the light             µmol[CO2]/m2[leaf]/s
!              response curve)
! cvt          Ratio of the diffusion resistance to the total resistance to CO2 (curvature              -
!              parameter for the light response curve)
! isp          Quantum efficiency of CO2 assimilation (initial slope of the light response curve)       µmol[CO2]/µmol[quanta]
! parlf        Photosynthetically active radiation absorbed by leaf                                     µmol[quanta]/m2/s
! pglf         Instantaneous CO2 assimilation                                                           µmol[CO2]/m2[leaf]/s 
!==============================================================================================================================
