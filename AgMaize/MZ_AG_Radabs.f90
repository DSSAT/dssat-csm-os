!=======================================================================
!  MZ_AG_Radabs, Subroutine, J.I. Lizaso
!  Calculates hourly canopy absorption of PAR (J/m2 s) by shaded and 
!  sunlit leaf area
!  Adapted from RADABS by N.B. Pickering
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  02/07/2003 JIL Written
!  09/12/2007 JIL Adapted for IXIM model
!  03/13/2013 Adapted for AgMaize
!-----------------------------------------------------------------------
!  Called from: MZ_AG_Photsynt
!  Calls:       MZ_AG_Shadow, MZ_AG_Lfextn, MZ_AG_Canabs
!  TO DO:
!  -Fix reflected radiation
!=======================================================================
subroutine MZ_AG_Radabs(azir, azzon, beta, betn, canht, canwh, daytim,               & !Input
           frdifp, froll, gla, h, lfn, palb, parh, pltpop, rowspc, scvp, hlai, xc,   & !Input
           parsh, parsun, plaish, plaisl, radtot,                                    & !Output
           arefhr, adifhr, addrhr, addfhr, leafnum)                                    !Output

implicit none
save   

logical :: daytim
integer :: h, i, lfn, leafnum(50)
real, dimension(50) :: gla, parsh, parsun, plaish, plaisl, arefhr, adifhr, addrhr, addfhr
real :: azir, azzon, beta, betn, canht, canwh, fracsh, frdifp, froll, frshv, hlai, kdifbl
real :: kdirbl, kdrblv, laish, laishv, laisl, laislv, palb, parh, parsl, parss, pcabsp, pcabsr
real :: pcintp, pcintr, pcrefp, pcrefr, pltpop, rowspc, scvp, xc, radtot

! Initialize.
if(h==1) then        !Daily initialization
   frshv  = 0.0
   laishv = 0.0
   laislv = 0.0
end if
pcabsp = 0.0         !Hourly initialization
pcintp = 0.0
pcrefp = 0.0
parsl = 0.0
parss = 0.0
pcabsr = 0.0
pcintr = 0.0
pcrefr = 0.0
parsun = 0.0
parsh = 0.0
radtot = 0.0

if(hlai > 0.000) then
   !Calculate fraction shaded and LAI's for vertical sun position
   if(h==1) then   
      call MZ_AG_Shadow(azir, azzon, 90.0, betn, canht, canwh, rowspc,    & !Input
                        frshv)                                              !Output
      call MZ_AG_Lfextn(90.0, froll, frshv, h, hlai, xc,                  & !Input
                        kdifbl, kdrblv, laishv, laislv)                     !Output
   end if

   if(daytim) then
      !Calculate fraction shaded, leaf extinction and LAI's
      call MZ_AG_Shadow(azir, azzon, beta, betn, canht, canwh, rowspc,    & !Input
                        fracsh)                                             !Output
      call MZ_AG_Lfextn(beta, froll, fracsh, h, hlai, xc,                 & !Input
                        kdifbl, kdirbl, laish, laisl)                       !Output

      !Calculate PAR absorbed by canopy during day
      call MZ_AG_Canabs(palb, beta, betn, canht, canwh, fracsh, froll,    & !Input
           gla, lfn, pltpop, frdifp, kdifbl, kdirbl, parh, rowspc, scvp,  & !Input
           pcabsp, pcintp, pcrefp, plaish, plaisl, parsh, parsun, radtot, & !Output
           arefhr, adifhr, addrhr, addfhr, leafnum)
   else
      !Night time with canopy           
      fracsh = frshv
      kdirbl = kdrblv
      laish = laishv
      laisl = laislv
   end if
   
else
   !Bare soil (day or night)
   pcabsp = (1.0-palb) * 100.0
   pcrefp = palb * 100.0
   parss = (1.0-palb) * parh
end if

return
end subroutine MZ_AG_Radabs



!==============================================================================================================================
! Variable definitions                                                                                  Unit 
!------------------------------------------------------------------------------------------------------------------------------
! azir         Row azimuth relative to North                                                            degrees
! azzon        Hourly solar azimuth (+/- from South)                                                    degrees
! beta         Hourly solar elevation (+/- from horizontal)                                             degrees
! betn         Spacing between plants along a row                                                       m/plant
! canht        Canopy height                                                                            m
! canwh        Canopy width                                                                             m
! daytim       Logical variable for test for daylight hours between sunrise and sunset                  -
! fracsh       Fraction of land shaded by the canopy 
! frdifp(h)    Hourly fraction diffuse photon flux density after correcting                             -
!              for circumsolar radiation (Spitters, 1986) 
! froll        Leaf rolling factor associated with soil water stress affecting cell expansion           -
! frshv        Fraction of land area shaded with vertical sun position
! gla(l)       Green leaf area for leaf l                                                               cm2
! h            Hourly iteration counter (1-24)                                                          -
! hlai         "Healthy" leaf area index (excluding senesced parts)                                     m2/m2
! kdifbl       Extinction coefficient of black leaves to diffuse radiation
! kdrblv       Extinction coefficient of black leaves to direct radiation for vertical sun position
! kdirbl       Extinction coefficient of black leaves to direct radiation 
! laish        Shaded leaf area index                                                                   m2[leaf]/m2[ground]
! laishv       Shaded leaf area index for vertical sun position                                         m2[leaf]/m2[ground]
! laisl        Sunlit leaf area index                                                                   m2[leaf]/m2[ground]
! laislv       Sunlit leaf area index for vertical sun position                                         m2[leaf]/m2[ground]
! lfn          Total leaf number rounded up                                                             -
! palb         Plant albedo accounting for soil water in the first soil layer                           -
! parh         Hourly photosynthetically active radiation (PAR)                                         µmol[quanta]/m2/s
! parsh(l)     Photosynthetically active radiation absorbed by leaf l in the shaded zone                µmol[quanta]/m2/s
! parsun(l)    Photosynthetically active radiation absorbed by leaf l in the sunlit zone                µmol[quanta]/m2/s
! pcabsp       Hourly percent absorbed photosynthetic radiation                                         %
! pcintp       Hourly percent intercepted photosynthetic radiation                                      %
! pcrefp       Hourly percent reflected photosynthetic radiation                                        %
! plaish(l)    Shaded leaf area index for leaf l                                                        m2/m2
! plaisl(l)    Sunlit leaf area index for leaf l                                                        m2/m2
! pltpop       Plant density                                                                            plants/m2
! radtot       Sum of PAR components (energy balance check: should equal parh)                          µmol[quanta]/m2/s
! rowspc       Row spacing                                                                              m
! scvp         Scattering coefficient used to calculate diffuse reflectance
! xc           Parameter X for calculating black layer extinction coefficient                           -
!              according to Campbell (1986) 
!==============================================================================================================================



!=======================================================================
!  SHADOW, Subroutine, N.B. Pickering, J.W. Jones
!  Calculates fraction shaded for sun and row geometry using ellipses.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  03/14/1991 NBP Written
!  11/15/1991 NBP Modified
!  11/23/1993 NBP Included more error checks and limits
!  09/12/2007 JIL Modified and adapted for IXIM model
!-----------------------------------------------------------------------
!  Called from: MZ_GM_Radabs
!  Calls:       
!=======================================================================
subroutine MZ_AG_Shadow(azir, azzon, beta, betn, canht, canwh, rowspc,    &  !Input
                        fracsh)                                              !Output

implicit none
save      

real, parameter :: pi=3.14159, rad=pi/180.0, zero=1.0E-6
real :: a, b, c1, c2, c3, c4, azimd, azir, azzon, beta, betn, canht, canwh, eta
real :: fracsh, gamma, rbeta, rowspc, shade, shlen, shperp, stouch

! Initialization
a       = 0.0
b       = 0.0
c1      = 0.0
c2      = 0.0
c3      = 0.0
c4      = 0.0
azimd   = 0.0
eta     = 0.0
fracsh  = 0.0
gamma   = 0.0
rbeta   = 0.0
shade   = 0.0
shlen   = 0.0
shperp  = 0.0
stouch  = 0.0
      
! Set fraction shaded to 0.0 for zero width or height
if(canwh <= zero .or. canht <= zero) then
   fracsh = 0.0
	
! Set fraction shaded to 1.0 for full cover
else if(canwh >= rowspc) then
   fracsh = 1.0

! Calculate fraction shaded
else
   !Adjust BETA if sun exactly overhead or at horizon.  Calculate acute positive angle between 
   !sun and row azimuths. Initialize other constants
   rbeta = min(max(beta*rad,1.0e-6), pi/2.0-1.0e-6)
   azimd = abs(azzon-azir)*rad
   if(azimd > pi/2.0) azimd = pi - azimd
   a = (canwh/canht)**2
   gamma = atan(a*tan(rbeta))
   c1 = a*(tan(rbeta))**2
   c2 = (a*tan(rbeta))**2
   
   ! Calculate shadow length assuming ellipsoidal shape for the plant
   shlen = canht * cos(rbeta-gamma) / sin(rbeta) * sqrt((1.0+c2)/(1.0+c1))
   b = (shlen/canwh)**2
   c3 = b*(tan(azimd))**2
   c4 = (b*tan(azimd))**2
   stouch = shlen / (cos(azimd) * sqrt(1.+c3))
   
   ! CALCULATE FRACTION SHADED
   ! Sun parallel to row.  Shadow limited to BETN
   if(azimd <= zero) then
      shlen = min(shlen, betn)
      shade = 0.25 * pi * shlen * canwh

   !Sun not parallel to row
   else
      !Calculate perpendicular shadow length
      azimd = max(azimd, 1.0e-6)
      eta = atan(1.0/(b*tan(azimd)))
      shperp = canwh*sin(azimd+eta)*sqrt((1.0+c4)/(1.0+c3))

      !Hedgerow (plant shadows overlap)
      if(stouch >= betn) then
         !Shadow length is perpendicular and limited to ROWSPC
         shlen = min(shperp, rowspc)
         shade = shlen * betn

      !Individual plants
      else
         !Limit shadow length to within one ROWSPC
         if(shperp > rowspc) shlen = shlen * rowspc/shperp
         shade = 0.25 * pi * shlen * canwh
      end if
   end if

   fracsh = min(shade/(rowspc*betn), 1.0)

end if

fracsh = min(max(fracsh,1.0e-6),1.0)

end subroutine MZ_AG_Shadow


!==============================================================================================================================
! Variable definitions                                                                                  Unit 
!------------------------------------------------------------------------------------------------------------------------------
! azir         Row azimuth relative to North                                                            degrees
! azzon        Hourly solar azimuth (+/- from South)                                                    degrees
! beta         Hourly solar elevation (+/- from horizontal)                                             degrees
! betn         Spacing between plants along a row                                                       m/plant
! canht        Canopy height                                                                            m
! canwh        Canopy width                                                                             m
! fracsh       Fraction of land shaded by the canopy 
! rowspc       Row spacing                                                                              m
! shade        Shadow area per plant                                                                    m2
! shlen        Shadow length per plant                                                                  m
! shperp       Length of the shadow perpendicular to the row                                            m
!==============================================================================================================================




!=======================================================================
!  LFEXTN, Subroutine, N.B. Pickering, K.J. Boote
!  Computes leaf extinction coefficients based on leaf angle distribution
!  (Goudriaan, 1988) and sunlit and shaded leaf area indices.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  ??/??/???? KJB Written
!  05/14/1991 NBP Removed COMMON and reorganized.
!  08/31/2004 JIL Adapted for Maize photosynthesis
!  09/12/2007 JIL Modified and adapted for IXIM model
!-----------------------------------------------------------------------
!  Called from: RADABS
!  Calls:      
!=======================================================================
 
subroutine MZ_AG_Lfextn(beta, froll, fracsh, h, hlai, xc,     & !Input
           kdifbl, kdirbl, laish, laisl)                        !Output
 
 implicit none
 save      
 
 integer :: i, h
 real, parameter :: pi = 3.14159, rad = pi/180.0, expbound=-40.0
 real :: beta, fracsh, kdirbl, kdifbl, laish, froll, laisl, hlai
 real :: bt, kdir, taudiff, taudir, xc, exponent
 
! Calculate black leaf extinction coefficients for diffuse and direct radiation
! Campbell (1986); Campbell (1990); Campbell and Norman (1998)
! Initialize
if(h==1) then     !Daily initialization
  bt       = 0.0
  kdifbl   = 0.0
  kdir     = 0.0
  taudiff  = 0.0
  taudir   = 0.0
end if

i        = 1           !Hourly initialization
kdirbl   = 0.0
laish    = 0.0
laisl    = 0.0
      
! Calculate KDIFBL once a day assuming Uniform OverCast sky (UOC)
if(h == 1) then
   taudiff = 0.0
   do i = 1, 89, 2
      bt = real(i)
      kdir = ((xc**2.+1./(tan(bt*rad))**2.)**0.5)/(xc+1.744*(xc+1.182)**(-0.733))

      !Underflows  chp 8/5/2009
      exponent = -kdir*hlai*froll
      if(exponent > expbound) then
         taudir = exp(-kdir*hlai*froll)
      else
         taudir = 0.0
      end if

      taudiff = taudiff + (taudir*sin(bt*rad)*cos(bt*rad))
   end do
      taudiff = 2.0*taudiff*2.0*rad
      kdifbl = -log(taudiff)/(hlai*froll)
end if

! Calculate KDIRBL hourly as a function of solar elevation 
kdirbl = ((xc**2.+1./(tan(beta*rad))**2.)**0.5)/(xc+1.744*(xc+1.182)**(-0.733))

! Calculate sunlit and shaded leaf area indices
exponent = -kdirbl*hlai*froll/fracsh
if(exponent > expbound) then
   laisl = (fracsh/kdirbl) * (1.0 - exp(-kdirbl*hlai*froll/fracsh))
else
   laisl = fracsh/kdirbl
end if
   
if(hlai*froll > 0.02) then
   laisl = max(laisl,0.02)
else
   laisl = hlai*froll
end if

laish = hlai*froll - laisl

end subroutine MZ_AG_Lfextn


!==============================================================================================================================
! Variable definitions                                                                                  Unit 
!------------------------------------------------------------------------------------------------------------------------------
! beta         Hourly solar elevation (+/- from horizontal)                                             degrees
! fracsh       Fraction of land shaded by the canopy 
! froll        Leaf rolling factor associated with soil water stress affecting cell expansion           -
! h            Hourly iteration counter (1-24)                                                          -
! hlai         "Healthy" leaf area index (excluding senesced parts)                                     m2/m2
! kdifbl       Extinction coefficient of black leaves to diffuse radiation
! kdirbl       Extinction coefficient of black leaves to direct radiation 
! laish        Shaded leaf area index                                                                   m2[leaf]/m2[ground]
! laisl        Sunlit leaf area index                                                                   m2[leaf]/m2[ground]
!==============================================================================================================================

           

!=======================================================================
!  CANABS, Subroutine, K.J. Boote, N.B. Pickering
!  Computes radiation absorbed by soil, sunlit and shaded leaves.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  ??/??/?? KJB Written
!  05/14/91 NBP Removed COMMON and reorganized.
!  08/31/04 JIL Adapted to photosynthesis in maize
!  09/12/2007 JIL Modified and adapted for IXIM model
! Q. Why not add addrsl to cumasun instead of (1.-scvr)*raddir*kdirbl*ylaisl
!-----------------------------------------------------------------------
!  Called from: RADABS
!  Calls:       
!=======================================================================

subroutine MZ_AG_Canabs(albedo, beta, betn, canht, canwh, fracsh, froll,   & !Input
           gla, lfn, pltpop, frdif, kdifbl, kdirbl, parh, rowspc, scvr,    & !Input
           pctabs, pctint, pctref, plaish, plaisl, radsh, radsun, radtot,  & !Output
           arefhr, adifhr, addrhr, addfhr, leafnum)

implicit none
save      

real, parameter :: pi=3.14159, rad=pi/180.0
integer :: l, lfn, leafnum(50)
real, dimension(50) :: gla, plaish, plaisl, radsh, radsun, arefhr, adifhr, addrhr, addfhr
real :: addf, addfsh, addfsl, addr, addrsl, adif, adifsh, adifsl, adir, adirsl, albedo
real :: aref, arefsh, arefsl, atot, beta, betn, canht, canwh, cumash, cumasun, delwp       
real :: delwr, difp, difpr, difr, fracsh, frdif, froll, incsoi, intcan, kdifbl, kdirbl        
real :: parh, pathp, pathr, pctabs, pctint, pctref, pltpop, raddif, raddir, radss, radtot     
real :: rdifsl, refdf, refdif, refdir, refdr, refsoi, reftot, rowspc, scvr, sinb      
real :: sqv, ucumash, ucumasun, uylaish, uylaisl, yhlai, ylaish, ylaisl
real :: yhlaican, ylaislcan, ylaishcan, adircan, adifcan, intcanall, incsoiall, refsoiall

! Initialization.
addf     = 0.0
addfsh   = 0.0 
addfsl   = 0.0 
addr     = 0.0 
addrsl   = 0.0 
adif     = 0.0 
adifsh   = 0.0 
adifsl   = 0.0 
adir     = 0.0 
adirsl   = 0.0 
aref     = 0.0 
arefsh   = 0.0 
arefsl   = 0.0 
atot     = 0.0 
cumash   = 0.0 
cumasun  = 0.0 
delwp    = 0.0 
delwr    = 0.0 
difp     = 0.0 
difpr    = 0.0 
difr     = 0.0 
incsoi   = 0.0 
intcan   = 0.0 
l        = 1
pathp    = 0.0 
pathr    = 0.0 
pctabs   = 0.0 
pctint   = 0.0 
pctref   = 0.0 
raddif   = 0.0 
raddir   = 0.0 
radss    = 0.0 
radtot   = 0.0 
rdifsl   = 0.0 
refdf    = 0.0 
refdif   = 0.0 
refdir   = 0.0 
refdr    = 0.0 
refsoi   = 0.0 
reftot   = 0.0 
ucumash  = 0.0 
ucumasun = 0.0 
uylaish  = 0.0 
uylaisl  = 0.0 
yhlai    = 0.0 
ylaish   = 0.0 
ylaisl   = 0.0 
plaish   = 0.0
plaisl   = 0.0
radsh    = 0.0
radsun   = 0.0
yhlaican = 0.0
adircan = 0.0
adifcan = 0.0
intcanall = 0.0
incsoiall = 0.0
refsoiall = 0.0
sqv = sqrt(1.0-scvr)
sinb = sin(beta*rad)
arefhr = 0.0
adifhr = 0.0
addrhr = 0.0
addfhr = 0.0
leafnum = 0.0

!-Compute reflection coefficients for direct and diffuse light
refdf = (1.0-sqv) / (1.0+sqv)
refdr = 2.0*kdirbl/(1.0+kdirbl) * refdf

!-Compute difpr, the fraction of sky "seen" by plants.
!Diffuse skylight is absorbed over an effective area equal to the
!canopy height plus width for an isolated row.  For interfering rows,
!Eqns. 2.130 and 2.128 from Goudriaan (1977) are used.  Concept
!extended for both between plants (P) and rows (R)
if(canwh < betn) then
   pathp = betn - canwh
   delwp = pathp + canht - sqrt(pathp**2+canht**2)
   difp = min((canwh+delwp)/betn,1.0)
else
   difp = 1.0
end if

if(canwh < rowspc) then
   pathr = rowspc - canwh
   delwr = pathr + canht - sqrt(pathr**2+canht**2)
   difr = min((canwh+delwr)/rowspc,1.0)
else
   difr = 1.0
end if
difpr = min(max(difp*difr,1.0e-6),1.0)

!-Partition total radiation into direct and diffuse components
raddif = frdif * parh
raddir = parh - raddif

!-Compute total soil-reflected radiation before entering the loop. The original codes
!recompute soil reflected light in each iteration and then calculate the plant absorption
!from it. In reality, for a given parh and canopy, soil reflected light is constant but it is
!the plant absorption from it that changes depending on the absorbing leaf layer.
!Correction added KAD 6/20/2013
yhlaican = sum(gla(1:lfn))*froll*pltpop*0.0001
ylaislcan = (fracsh/kdirbl) * (1.0-exp(-kdirbl*yhlaican/fracsh))
ylaishcan = yhlaican - ylaislcan
adircan  = fracsh * (1.0-refdr) * raddir * (1.0-exp(-kdirbl*sqv*yhlaican/fracsh))
adifcan  = difpr  * (1.0-refdf) * raddif * (1.0-exp(-kdifbl*sqv*yhlaican/difpr))
refdir = fracsh * refdr * raddir
refdif = difpr  * refdf * raddif
intcanall = refdir + refdif + adircan + adifcan
incsoiall = parh - intcanall                      
refsoiall = albedo * incsoiall                
      
!-Direct radiation absorbed in shaded zone by considering the direct
!(ADDR) and diffuse/scattered (ADDF) components of the direct beam.
! ** JIL Beginning per-leaf loop
do l = lfn, 1, -1
   if(gla(l) > 0.0) then
      !-Cumulative leaf area index (m2/m2) from top to bottom with
      !sunlit and shaded components separate
      yhlai = yhlai + gla(l)*froll*pltpop*0.0001
      ylaisl = (fracsh/kdirbl) * (1.0-exp(-kdirbl*yhlai/fracsh))
      ylaish = yhlai - ylaisl
      
      !-Absorbed from direct (Equations 17-23 in Lizaso et al. 2005)
      !First, compute total absorbed from direct PAR (adir) and its two components
      !(not scattered addr, and scattered addf)      
      adir = fracsh * (1.0-refdr) * raddir * (1.0-exp(-kdirbl*sqv*yhlai/fracsh))
      addr = fracsh * (1.0-scvr)  * raddir * (1.0-exp(-kdirbl*yhlai/fracsh))
      addf = adir - addr
      !From the previous three components, compute quantities absorbed by sunlit and shaded LAI.
      !Note that shaded LAI only absorbs the scattered component not absorbed by the sunlit LAI.
      adirsl = fracsh * (1.0-refdr) * raddir * (1.0-exp(-kdirbl*sqv*ylaisl/fracsh))           
      addrsl = fracsh * (1.0-scvr)  * raddir * (1.0-exp(-kdirbl*ylaisl/fracsh))
      addfsl = adirsl - addrsl
      addfsh = addf - addfsl      
      
      !-Absorbed from incident diffuse
      adif = difpr * (1.0-refdf) * raddif * (1.0-exp(-kdifbl*sqv*yhlai/difpr))
      adifsl = difpr * (1.0-refdf) * raddif * (1.0-exp(-kdifbl*sqv*ylaisl/difpr))
      adifsh = adif - adifsl
      
      !-Light reflected from the soil assumed to be isotropic and diffuse.
      !Absorption handled in the same manner as diffuse skylight
      !refdir = fracsh * refdr * raddir
      !refdif = difpr * refdf * raddif
      intcan = refdir + refdif + adir + adif       !Light intercepted by the canopy
      !incsoi = parh - intcan                      !Light intercepted by the soil
      !refsoi = albedo * incsoi                    !Light reflected by the soil
      
      !-Absorbed light by the canopy from the soil-reflected light
      !aref = difpr * (1.0-refdf) * refsoi * (1.0-exp(-kdifbl*sqv*yhlai/difpr))
      !arefsh = difpr * (1.0-refdf) * refsoi * (1.0-exp(-kdifbl*sqv*yhlaish/difpr))
      !Check and correct
      aref = difpr * (1.0-refdf) * refsoiall * (1.0-exp(-kdifbl*sqv*(yhlaican-yhlai)/difpr))
      arefsh = difpr * (1.0-refdf) * refsoiall * (1.0-exp(-kdifbl*sqv*(ylaishcan-ylaish)/difpr))
      arefsl = aref - arefsh      
         
      !-Total absorbed and total reflected light
      atot = adir + adif + aref
      reftot = refdir + refdif + refsoiall - aref

      !-Cumulative light absorbed down the canopy by sunlit and shaded LAI
      !radss = incsoi * (1.0-albedo)
      radss = incsoiall * (1.0-albedo)
      rdifsl = addfsl + adifsl + arefsl
      cumasun = rdifsl + (1.-scvr)*raddir*kdirbl*ylaisl
      cumash = addfsh + adifsh + arefsh

      !-Determine per-leaf sunlit and shaded LAI and sunlit and shaded PAR
	  plaisl(l) = ylaisl - uylaisl
	  plaish(l) = ylaish - uylaish

	  if(plaisl(l) < 1e-4) then
	     radsun(l) = 0.0
	  else
	     radsun(l) = (cumasun-ucumasun)/plaisl(l)
	  end if
	  
	  if(plaish(l) == 0.0) then
	     radsh(l) = 0.0
	  else
	     radsh(l) = (cumash-ucumash)/plaish(l)
	  end if
      
      !-Save current LAI and cumulative radiation values for next iteration
	  uylaish = ylaish
	  uylaisl = ylaisl
	  ucumasun = cumasun
	  ucumash = cumash
	  
	  !-KAD 09/24/2013 - Save different absorbed radiation components for checking
	  arefhr(l) = aref
	  adifhr(l) = adif
	  addrhr(l) = addr
	  addfhr(l) = addf
	  leafnum(l) = l
   end if
end do
! JIL End of per-leaf loop

! Set radiation array and calculate ratios of components
if(parh > 0.0) then
   pctint = 100.0 * intcan / parh
   pctabs = 100.0 * atot / parh
   pctref = 100.0 * reftot / parh
else
   pctint = 0.0
   pctabs = 0.0
   pctref = 0.0
end if

! Energy balance check (RADTOT=PARH)
radtot = atot + reftot + radss

end subroutine MZ_AG_Canabs
	  


!==============================================================================================================================
! Variable definitions                                                                                  Unit 
!------------------------------------------------------------------------------------------------------------------------------	  
! addf         Absorbed direct PAR that is scattered and becomes diffuse                                µmol[quanta]/m2/s
! addfsh       Absorbed direct PAR in the shaded LAI that is scattered and becomes diffuse              µmol[quanta]/m2/s
! addfsl       Absorbed direct PAR in the sunlit LAI that is scattered and becomes diffuse              µmol[quanta]/m2/s
! addr         Absorbed direct PAR that is not scattered                                                µmol[quanta]/m2/s
! addrsl       Absorbed direct PAR in the sunlit LAI that is not scattered                              µmol[quanta]/m2/s
! adif         Total absorbed diffuse PAR                                                               µmol[quanta]/m2/s
! adifsh       Diffuse PAR captured by shaded LAI                                                       µmol[quanta]/m2/s
! adifsl       Diffuse PAR captured by sunlit LAI                                                       µmol[quanta]/m2/s
! adir         Total absorbed direct PAR                                                                µmol[quanta]/m2/s
! adirsl       Direct PAR captured by sunlit LAI                                                        µmol[quanta]/m2/s
! albedo       Plant albedo accounting for soil water in the first soil layer                           -
! aref         Total canopy absorption from the soil-reflected PAR                                      µmol[quanta]/m2/s
! arefsh       Shaded LAI PAR absorption from the soil-reflected PAR                                    µmol[quanta]/m2/s
! arefsl       Sunlit LAI PAR absorption from the soil-reflected PAR                                    µmol[quanta]/m2/s
! atot         Total PAR absorbed by the canopy                                                         µmol[quanta]/m2/s
! beta         Hourly solar elevation (+/- from horizontal)                                             degrees
! betn         Spacing between plants along a row                                                       m
! canht        Canopy height                                                                            m
! canwh        Canopy width                                                                             m
! cumash       Cumulative PAR absorbed by shaded LAI (down to current leaf)                             µmol[quanta]/m2/s
! cumasun      Cumulative PAR absorbed by sunlit LAI (down to current leaf)                             µmol[quanta]/m2/s
! difpr        Fraction of sky "seen" by plants
! fracsh       Fraction of land shaded by the canopy (during daytime)
! frdif        Fraction diffuse photon flux density after correcting for circumsolar 
!              radiation (Spitters, 1986) 
! froll        Leaf rolling factor associated with soil water stress affecting cell expansion           -
! gla(l)       Green leaf area for leaf l                                                               cm2
! incsoi       Total PAR intercepted by the soil                                                        µmol[quanta]/m2/s
! intcan       Total PAR intercepted by the canopy                                                      µmol[quanta]/m2/s
! kdifbl       Extinction coefficient of black leaves to diffuse radiation
! kdirbl       Extinction coefficient of black leaves to direct radiation 
! l            Integer loop counter
! lfn          Total leaf number rounded up                                                             -
! parh         Hourly photosynthetically active radiation (PAR)                                         µmol[quanta]/m2/s
! pctabs       Percent of total PAR absorbed by the canopy                                              µmol[quanta]/m2/s
! pctint       Percent of total PAR intercepted by the canopy                                           µmol[quanta]/m2/s
! pctref       Percent of total PAR reflected by the canopy and the soil                                µmol[quanta]/m2/s
! plaish(l)    Shaded leaf area index for leaf l                                                        m2[leaf]/m2[ground]
! plaisl(l)    Sunlit leaf area index for leaf l                                                        m2[leaf]/m2[ground]
! pltpop       Plant density                                                                            plants/m2
! raddif       Hourly diffuse PAR                                                                       µmol[quanta]/m2/s
! raddir       Hourly direct PAR                                                                        µmol[quanta]/m2/s
! radsh(l)     Photosynthetically active radiation absorbed by leaf l in the shaded zone                µmol[quanta]/m2/s
! radss        Radiation on soil                                                                        µmol[quanta]/m2/s
! radsun(l)    Photosynthetically active radiation absorbed by leaf l in the sunlit zone                µmol[quanta]/m2/s
! radtot       Sum of PAR components (energy balance check: should equal parh)                          µmol[quanta]/m2/s
! rdifsl       Total diffuse PAR absorbed by sunlit LAI                                                 µmol[quanta]/m2/s
! refdif       Canopy-reflected diffuse PAR                                                             µmol[quanta]/m2/s
! refdir       Canopy-reflected direct PAR                                                              µmol[quanta]/m2/s
! refdr        Reflection coefficient for direct radiation
! refdf        Reflection coefficient for diffuse radiation
! refsoi       Diffuse PAR reflected from the soil                                                      µmol[quanta]/m2/s
! reftot       Total PAR reflected by the canopy and the soil                                           µmol[quanta]/m2/s
! rowspc       Row spacing                                                                              m
! scvr         Scattering coefficient used to calculate diffuse reflectance
! ucumash      Cumulative PAR absorbed by shaded LAI (down to previous leaf)                            µmol[quanta]/m2/s
! ucumasun     Cumulative PAR absorbed by sunlit LAI (down to previous leaf)                            µmol[quanta]/m2/s
! uylaish      Cumulative shaded LAI from top to bottom of canopy (down to previous leaf)               m2[leaf]/m2[ground]
! uylaisl      Cumulative sunlit LAI from top to bottom of canopy (down to previous leaf)               m2[leaf]/m2[ground]
! yhlai        Cumulative leaf area index from top to bottom of canopy                                  m2[leaf]/m2[ground]
! ylaish       Cumulative shaded LAI from top to bottom of canopy (down to current leaf)                m2[leaf]/m2[ground]
! ylaisl       Cumulative sunlit LAI from top to bottom of canopy (down to current leaf)                m2[leaf]/m2[ground]
!==============================================================================================================================
