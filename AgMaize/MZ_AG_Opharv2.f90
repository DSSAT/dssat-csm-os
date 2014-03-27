!=======================================================================
!  MZ_OPHARV, Subroutine C.H. Porter
!  Generates data for use by OPSUM and OVERVIEW for MAIZE.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  01/25/2002 CHP Written, based on OPHARV
!  03/03/2002 GH  Modified logic for calling of fileA
!  08/12/2003 CHP Added I/O error checking and changed call to READA
!  02/04/2005 CHP Added PODWT, LAI, HI to Summary.out output
!  08/11/2005 CHP Use BWAH as byproduct variable for Summary.OUT 
!                 (byproduct harvested), BWAM as byproduct variable 
!                 for Overview.OUT (byproduct produced).  Both variables
!                 now include senesced stover.
!  10/24/2005 CHP Added environmental & stress factors to Overview.OUT
!  07/13/2006 CHP Added P model
!  02/09/2007 GH  Add path for FileA
!  08/28/2009 CHP added EDAT, EDAP 
!  10/01/2013 KAD Adapted for AgMaize
!=======================================================================

subroutine MZ_AG_Opharv2(control, iswitch,                                      & !Control inputs
    yrplt, harvfrac,                                                            & !Inputs from MZ_GM_GLOBAL
    growthStage, mdate, gstdyrdoySim,                                           & !Inputs from Phenology
    lai, tlu, tnleaf, lftips, greenla, greenlaSilk,                             & !Inputs from Leaf Area
    tadrwAnth, tadrwSilk, kn, seedno, we, skerwt, stover, swfac, tadrw, grain,  & !Inputs from Growth
    bwah, sdwtah)                                                                 !Output

!-----------------------------------------------------------------------
Use ModuleDefs 
use MZ_AG_ModuleDefs    
implicit none
save
!-----------------------------------------------------------------------
!! Check units
!From Growth: canwaa=tadrwSilk, gpp=kn, gpsm=seedno, podwt=we, seedno=seedno, skerwt=skerwt, stover=stover, swfac=swfac, topwt=tadrw, yield=grain
!From LAI: xlai, xn=tnleaf
!From Phenology: isdate=endleafgrowth, istage=growthstage, mdate, stgdoy, yremrg=emerg
!From GLOBAL through plant.for: harvfrac, yrplt

character :: ideto*1, idets*1, iplti*1, rnmode*1, crop*2, stname*10(20), filea*12, pathex*80
integer :: demrg, dflr, dsilk, dmilk, dmat, ifpd, dfpd, ifsd, dfsd, dnr0, dnr1, dnr2, dnr3, dnr7, dynamic
integer :: iemrg, iflr, imat, isens, isilk, imilk, mdate, growthStage, run, timdif, lfmax, found
integer :: trtnum, yrnr1, yrnr2, yrnr3, yrnr5, yrdoy, yrsim, yrplt, trt_rot, gstdyrdoySim(20)
real, parameter :: cm2tom2=1E-4  
real :: agefac, aptnup, bwah, bwam, cannaa, tadrwAnth, tadrwSilk, gnup, kn, hi, StovSenes, maxlai, greenla(50) 
real :: greenlaSilk(50), nstres, we, psdwt, Pstres1, Pstres2, sdrate, sdwt, sdwtah, harvfrac(2)
real :: sdwtam, pltpop, laitop, seedno, skerwt, stover, swfac, tadrw, turfac, wtncan, wtnup, xgnp, lai, lftips
real :: grain, tlu, tnleaf

! Arrays which contain data for printing in SUMMARY.OUT file
integer, parameter :: sumnum = 17
character*4, dimension(sumnum) :: label
real, dimension(sumnum) :: value

! Arrays which contain Simulated and Measured data for printing in OVERVIEW.OUT and EVALUATE.OUT files (OPVIEW subroutine)
integer acount
character*6, dimension(EvaluateNum) :: olab, olap     !OLAP in dap
character*6 X(EvaluateNum)
character*8 Simulated(EvaluateNum), Measured(EvaluateNum)
character*50 descrip(EvaluateNum)

type(ControlType) control
type(SwitchType) iswitch
type(ResidueType) senesce
type(FileioType) :: datafileio

! Variables added for environmental and stress factors output
Type (PlStresType) PlantStres

dynamic = control % dynamic
crop    = control % crop
yrsim   = control % yrsim
rnmode  = control % rnmode
run     = control % run
iplti   = iswitch % iplti
ideto   = iswitch % ideto
idets   = iswitch % idets
senesce % ResWt = 0.0             !KAD: Not using yet
StovSenes = senesce % ResWt(0)

!-----------------------------------------------------------------------
acount = 26  !Number of FILEA headings.

! Headings in FILEA for Measured data
data olab /    & 
 'ADAT  ',     & !1  DFLR 
 'PD1T  ',     & !2  IFPD 
 'PDFT  ',     & !3  IFSD 
 'MDAT  ',     & !4  DMAT 
 'HWAM  ',     & !5  XGWT 
 'PWAM  ',     & !6  XPDW 
 'H#AM  ',     & !7  XNOGR
 'HWUM  ',     & !8  XGWU 
 'H#UM  ',     & !9  XNOGU
 'CWAM  ',     & !10 XCWT 

! 08/11/2005 CHP
! Change BWAH to BWAM -- by-product produced to maturity, but not necessarily removed from field
! 'BWAH',      & !11 XSWT 
 'BWAM  ',     & !11 XSWT 
 'LAIX  ',     & !12 XLAM 
 'HIAM  ',     & !13 XHIN 
 'THAM  ',     & !14 XTHR 
 'GNAM  ',     & !15 XNGR 
 'CNAM  ',     & !16 XNTP 
 'SNAM  ',     & !17 XNST 
 'GN%M  ',     & !18 XNPS 
 'CWAA  ',     & !19 XCWAA
 'CNAA  ',     & !20 XCNAA
 'L#SM  ',     & !21 XLFNO
 'LAIT  ',     & !22 LAI of top 7 leaves
 'EDAT  ',     & !23 Emergence date
 'LDAT  ',     & !24 Silking date
 'KDAT  ',     & !25 Milk line date
 'CWAS  ',     & !26 CWAS
  14*'      '/  
 
!-----------------------------------------------------------------------
data stname /     & !Stage
  'Germinate ',   & !1
  'Emergence ',   & !2
  'End Juv Ph',   & !3
  'Tassel Ini',   & !4
  'Topmost Lf',   & !5
  'Anthesis  ',   & !6
  'Silking   ',   & !7
  'Ons Lin GF',   & !8
  '50% Mk Lin',   & !9
  'Black Layr',   & !10
  '          ',   & !11
  '          ',   & !12
  'Planting  ',   & !13
  'Start Sim ',   & !14
  'End Sim   ',   & !15
  'Harvest   ',   & !16
  '          ',   & !17
  '          ',   & !18
  '          ',   & !19
  'Harvest   '/     !20

!***********************************************************************
!***********************************************************************
!     RUN INITIALIZATION
!***********************************************************************
if(dynamic == runinit) then
!-----------------------------------------------------------------------

! Variables not currently calculated
agefac = 1.0
aptnup = 0.0
cannaa = 0.0
gnup   = 0.0
nstres = 1.0
pstres1 = 1.0
pstres2 = 1.0
turfac  = 1.0
wtncan  = 0.0
wtnup   = 0.0
xgnp    = 0.0

!Read fileio case 'FILENP' and transfer variables
call readfileio(control, 'FILENP', datafileio)
filea  = datafileio % filea 
pathex = datafileio % pathex
   
!Read fileio case 'INPUTS' and transfer variables
call readfileio(control, 'INPUTS', datafileio)
trtnum = datafileio % trtnum 
pltpop = datafileio % pltpop

! Assign descriptions to Measured and Simulated data from DATA.CDE.
call getdesc(acount, olab, descrip)
olap = olab

call Opview(control, tadrw, acount, descrip, ideto, lftips,  & 
     measured, plantstres, simulated, gstdyrdoySim,          &
     stname, wtncan, lai, nint(grain), yrplt, growthStage)

!***********************************************************************
!***********************************************************************
!     SEASONAL INITIALIZATION
!***********************************************************************
elseif (dynamic == seasinit) then
!-----------------------------------------------------------------------
Simulated = ' '
Measured  = ' '

! Establish # and names of stages for environmental & stress summary
plantstres % active = .false.
plantstres % nstages = 5

PlantStres % StageName(0) = 'Planting to Harvest    '
PlantStres % StageName(1) = 'Emegence-Tassel Init   '
PlantStres % StageName(2) = 'Tassel Init-Topmost Lf '
PlantStres % StageName(3) = 'Topmost leaf-Silking   '
PlantStres % StageName(4) = 'Silking-Beg Lin Gr Fill'
PlantStres % StageName(5) = 'Beg Lin Gr Fill-Blk Lyr'

call Opview(control, tadrw, acount, descrip, ideto, lftips,   &
     measured, plantstres, simulated, gstdyrdoySim,           &
     stname, wtncan, lai, nint(grain), yrplt, growthStage)

maxlai = 0.0


!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
else if(dynamic == output) then
!-----------------------------------------------------------------------
maxlai = amax1(maxlai, lai)      !Maximum lai season

PlantStres % W_grow = turfac 
PlantStres % W_phot = swfac  
PlantStres % N_grow = agefac 
PlantStres % N_phot = nstres 
PlantStres % P_grow = pstres2
PlantStres % P_phot = pstres1
PlantStres % active = .false.

!if(growthStage > 0 .AND. growthStage < 6) then
!  Plantstres % active(growthStage) = .true.
!endif
!KAD 10/08/2013- Do not want to add more phases to PlantStres so, here I am manually combining
!AgMaize's phenological phases to match the maximum number of 5 stages shown in OVERVIEW.OUT
if(growthStage==2 .OR. growthStage==3) Plantstres % active(1) = .true.    !Emergence to Tassel Initiation
if(growthStage==4) Plantstres % active(2) = .true.                        !Tassel Initiation to Appearance of topmost leaf
if(growthStage==5 .OR. growthStage==6) Plantstres % active(3) = .true.    !Appearance of topmost leaf to Silking
if(growthStage==7) Plantstres % active(4) = .true.                        !Silking to Onset of Linear Grain Filling
if(growthStage==8 .OR. growthStage==9) Plantstres % active(5) = .true.    !Onset of Linear Grain Filling to Black Layer

yrdoy = control % yrdoy
if(yrdoy >= yrplt) then
   if(mdate < 0 .OR. (mdate > 0 .AND. yrdoy < mdate)) then
      PlantStres % active(0) = .true.
   endif
endif

! Send data to Overview.out data on days where stages occur
call Opview(control, tadrw, acount, descrip, ideto, lftips,   &
     measured, plantstres, simulated, gstdyrdoySim,           &
     stname, wtncan, lai, nint(grain), yrplt, growthStage)

!***********************************************************************
!***********************************************************************
!     Seasonal Output 
!***********************************************************************
else if(dynamic == seasend) then
!-----------------------------------------------------------------------
! Transfer dates for model stages.
yrnr1  = gstdyrdoySim(6)
!yrnr1  = isdate
!yrnr2  = gstdyrdoySim(7)
!yrnr3  = gstdyrdoySim(3)
!yrnr5  = gstdyrdoySim(5)
!wtnsd  = gnup  /10.0

!-----------------------------------------------------------------------
! Calculate variables for output
! update nitrogen and residue applications after routines have been
! modified to handle automatic management
!-----------------------------------------------------------------------
if(seedno > 0.0) then
   psdwt = grain/(seedno*10.)
else
   psdwt = 0.0
endif
if(tadrw > 0.0 .AND. grain >= 0.0) then
   hi = grain/tadrw
 else
   hi = 0.0
endif

!LAI for the top 7 leaves
lfmax = ceiling(tnleaf)                                           !Total number of actual leaves
laitop = sum( greenlaSilk((lfmax-6):lfmax) ) * cm2tom2 * pltpop   !in m2/m2

!-----------------------------------------------------------------------
! Actual yield harvested (default is 100 %)
!-----------------------------------------------------------------------
sdwt   = grain / 10.0
sdwtam = sdwt
sdwtah = sdwt * harvfrac(1)

!-----------------------------------------------------------------------
! Actual byproduct harvested (default is 0 %)
! Byproduct not harvested is incorporated
! 08/11/2005 Senesced leaf and stem stay on plant and are
! available for by-product harvest.
!-----------------------------------------------------------------------
bwam = stover + stovsenes
bwah = (stover + stovsenes) * harvfrac(2) 

!-----------------------------------------------------------------------
if((ideto == 'Y' .OR. index('IAEBCGDT',rnmode) > 0) .OR.   &
   (index('AY',idets) > 0 .AND. crop /= 'FA')) then
   if(index('FQ',rnmode) > 0) then
      trt_rot = control % rotnum
   else
      trt_rot = trtnum
   end if
   call reada(filea, pathex, olab, trt_rot, yrsim, x)
   
!-----------------------------------------------------------------------
! Convert from YRDOY format to DAP.  Change descriptions to match.
call reada_dates(x(1), yrsim, iflr)
if(iflr > 0 .AND. iplti == 'R' .AND. isens == 0) then
   dflr = timdif(yrplt, iflr)
else
  dflr  = -99
end if
olap(1) = 'ADAP  '
call getdesc(1, olap(1), descrip(1))

call reada_dates(x(2), yrsim, ifpd)
if(ifpd > 0 .AND. iplti == 'R' .AND. isens == 0) then
   dfpd = timdif(yrplt, ifpd)
else
   dfpd  = -99
end if
olap(2) = 'PD1P  '
call getdesc(1, olap(2), descrip(2))

call reada_dates(x(3), yrsim, ifsd)
if(ifsd > 0 .and. iplti == 'R' .AND. isens == 0) then
   dfsd = timdif(yrplt, ifsd)
else
   dfsd  = -99
end if
olap(3) = 'PDFP  '
call getdesc(1, olap(3), descrip(3))

call reada_dates(x(4), yrsim, imat)
if(imat > 0 .AND. iplti == 'R' .AND. isens == 0) then
   dmat = timdif(yrplt, imat)
else
   dmat  = -99
end if
olap(4) = 'MDAP  '
call getdesc(1,olap(4), descrip(4))

! 08/28/2009 CHP added EDAT, EDAP 
call reada_dates(x(23), yrsim, iemrg)
if(iemrg > 0 .AND. iplti == 'R' .AND. isens == 0) then
   demrg = timdif(yrplt, iemrg)
else
   demrg  = -99
end if
olap(23) = 'EDAP  '
call getdesc(1,olap(23), descrip(23))

call reada_dates(x(24), yrsim, isilk)
if(isilk > 0 .AND. iplti == 'R' .AND. isens == 0) then
   dsilk = timdif(yrplt, isilk)
else
  dsilk  = -99
end if
olap(24) = 'LDAP  '
call getdesc(1, olap(24), descrip(24))

call reada_dates(x(25), yrsim, imilk)
if(imilk > 0 .AND. iplti == 'R' .AND. isens == 0) then
   dmilk = timdif(yrplt, imilk)
else
  dmilk  = -99
end if
olap(25) = 'KDAP  '
call getdesc(1, olap(25), descrip(25))


if(yrplt > 0) then
   dnr1 = timdif(yrplt, gstdyrdoySim(6))
   if(dnr1 <= 0) then
      dnr1 = -99
   end if
else
   dnr1 = -99
end if

dnr2 = timdif(yrplt, gstdyrdoySim(7))
if(dnr2 <= 0 .OR. yrplt <= 0) then
   dnr2 = -99
end if

dnr3 = timdif(yrplt, gstdyrdoySim(9))
if(dnr3 <= 0 .OR. yrplt <= 0) then
   dnr3 = -99
end if

if(yrplt > 0) then
   dnr7 = timdif (yrplt, mdate)
   if(dnr7 <= 0)  then
      dnr7 = -99
   end if
else
   dnr7 = -99
end if

dnr0 = timdif(yrplt, gstdyrdoySim(2))
if(dnr0 <= 0 .OR. yrplt <= 0) then
   dnr0 = -99
end if

write(Simulated(1),'(i8)') dnr1;          write(measured(1),'(i8)') dflr    !ADAT
write(Simulated(2),'(i8)') -99 ;          write(measured(2),'(i8)') -99     !PD1T
write(Simulated(3),'(i8)') -99 ;          write(measured(3),'(i8)') -99     !PDFT
write(Simulated(4),'(i8)') dnr7;          write(measured(4),'(i8)') dmat    !MDAT
write(Simulated(5),'(i8)') nint(grain);   write(measured(5),'(a8)') x(5)    !HWAM
write(simulated(6),'(i8)') -99 ;          write(measured(6),'(i8)') -99     !PWAM
write(simulated(7),'(i8)') nint(seedno);  write(measured(7),'(a8)') x(7)    !H#AM
write(simulated(8),'(f8.4)') skerwt;      write(measured(8),'(a8)') x(8)    !HWUM 
write(simulated(9),'(f8.1)') kn;          write(measured(9),'(a8)') x(9)    !H#UM 
write(simulated(10),'(i8)') nint(tadrw);  write(measured(10),'(a8)') x(10)  !CWAM

! 08/11/2005 CHP changed from BWAH to BWAM, 
write(Simulated(11),'(i8)') nint(bwam);         write(measured(11),'(a8)') x(11)  !BWAM
write(Simulated(12),'(f8.2)') maxlai;           write(measured(12),'(a8)') x(12)  !LAIX
write(Simulated(13),'(f8.3)') hi;               write(measured(13),'(a8)') x(13)  !HIAM
write(Simulated(14),'(i8)') -99 ;               write(measured(14),'(i8)') -99    !THAM
write(Simulated(15),'(i8)') nint(gnup);         write(measured(15),'(a8)') x(15)  !GNAM
write(Simulated(16),'(i8)') nint(wtncan*10.);   write(measured(16),'(a8)') x(16)  !CNAM
write(Simulated(17),'(i8)') nint(aptnup);       write(measured(17),'(a8)') x(17)  !SNAM
write(Simulated(18),'(f8.1)') xgnp;             write(measured(18),'(a8)') x(18)  !GN%M
write(Simulated(19),'(i8)') nint(tadrwAnth);    write(measured(19),'(a8)') x(19)  !CWAA
write(Simulated(20),'(i8)') nint(cannaa*10);    write(measured(20),'(a8)') x(20)  !CNAA
write(Simulated(21),'(f8.2)') lftips;           write(measured(21),'(a8)') x(21)  !L#SM
write(Simulated(22),'(f8.2)') laitop;           write(measured(22),'(i8)') -99    !LAIT
write(Simulated(23),'(i8)') dnr0;               write(measured(23),'(i8)') demrg
write(Simulated(24),'(i8)') dnr2;               write(measured(24),'(i8)') dsilk  !LDAT
write(Simulated(25),'(i8)') dnr3;               write(measured(25),'(i8)') dmilk  !KDAT
write(Simulated(26),'(i8)') nint(tadrwSilk);    write(measured(26),'(i8)') -99    !CWAS

end if

call Opview(control, tadrw, acount, descrip, ideto, lftips,   &
     measured, plantstres, simulated, gstdyrdoySim,           &
     stname, wtncan, lai, nint(grain), yrplt, growthStage)

!-------------------------------------------------------------------
! Send information to OPSUM to generate SUMMARY.OUT file
!-------------------------------------------------------------------
 psdwt  = skerwt
 sdrate = -99.0

! Store Summary.out labels and values in arrays to send to
! OPSUM routines for printing.  Integers are temporarily 
! saved as real numbers for placement in real array.
label(1)  = 'ADAT'; value(1)  = float(yrnr1)
label(2)  = 'MDAT'; value(2)  = float(mdate)
label(3)  = 'DWAP'; value(3)  = sdrate
label(4)  = 'CWAM'; value(4)  = tadrw
label(5)  = 'HWAM'; value(5)  = sdwtam*10.
label(6)  = 'HWAH'; value(6)  = sdwtah*10.
! BWAH multiplied by 10.0 in OPSUM - divide by 10. here to preserve units.
label(7)  = 'BWAH'; value(7)  = bwah / 10. 
label(8)  = 'HWUM'; value(8)  = psdwt       !*1000.
label(9)  = 'H#AM'; value(9)  = seedno
label(10) = 'H#UM'; value(10) = kn
label(11) = 'NFXM'; value(11) = 0.0         !wtnfx*10.
label(12) = 'NUCM'; value(12) = wtnup*10.
label(13) = 'CNAM'; value(13) = wtncan*10.
label(14) = 'GNAM'; value(14) = gnup        !wtnsd*10.
label(15) = 'PWAM'; value(15) = we
label(16) = 'LAIX'; value(16) = maxlai
label(17) = 'HIAM'; value(17) = hi

! Send labels and values to OPSUM
call sumvals (sumnum, label, value) 

! Send Measured and Simulated datat to OPSUM
call evaluatedat (acount, measured, simulated, descrip, olap) 

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
end if
!***********************************************************************
return
end subroutine MZ_AG_Opharv2
!=======================================================================


!==============================================================================================================================
! Variable definitions                                                                                  Unit 
!------------------------------------------------------------------------------------------------------------------------------
! control       Constructed type for control variables
! agefac        Nitrogen stress factor affecting cell expansion 
! aptnup        Nitrogen in stover (above ground biomass), kg N/ha
! cannaa        Stover N at anthesis, g N/M2    
! tadrwAnth     Canopy weight at anthesis, kg/ha
! tadrwSilk     Canopy weight at silking, kg/ha
! gnup          Total grain N uptake, kg N/ha
! kn            Grain number per plant, grains/plant 
! seedno        Grain numbers, grains/m2 
! harvfrac      Two-element array containing fractions of (1) yield harvested and (2) by-product harvested (fraction)
! ideto         Switch for printing overview.out file
! idets         Code to generate several output files
! isdate        Year and day of year for end of leaf growth
! growthStage   Growth stage
! mdate         Maturity data, year and day of year
! nstres        Nitrogen stress factor affecting growth (0-1) 
! we            Pod (ear) weight, kg/ha
! seedno        Seed number, #/m2
! skerwt        Weight per kernel, g/kernel 
! gstdyrdoySim(20)    Array storing the dates that different growth stages occurred
! stover        Stover weight (leaf+stem), kg/ha
! swfac         Soil water stress effect on growth (0-1), 1 is no stress, 0 is full stress
! tadrw         Total above ground biomass, kg/ha
! turfac        Soil water stress effecting cell expansion
! wtncan        Weight of nitrogen in above ground biomass (stem, leaf, grain), kg N/ha    
! wtnup         Total nitrogen uptake, g/m2  
! xgnp          Nitrogen content of grain, %
! lai          Leaf area index, m2/m2
! lftips        Number of leaf tips    
! grain         Yield in kg/ha at 0% moisture content
! yremrg        Year and day of year of emergence 
! yrplt         Year and day of year of planting
!==============================================================================================================================
