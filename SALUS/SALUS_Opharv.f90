!=======================================================================
!  SALUS_Opharv, Subroutine 
!  Generates data for use by OPSUM for SALUS
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  04/01/2014 KAD Adapted MZ_OPHARV for SALUS	
!=======================================================================

subroutine SALUS_Opharv(control, iswitch, yrplt, harvfrac, gstage, mdate,   & !Input
      gyrdoySim, biomassc, bioatlaimax, grainyieldc, xhlai, droughtfac,     & !Input
      bwah, sdwtah)                                                           !Output
!-----------------------------------------------------------------------
Use ModuleDefs
implicit none
save
!-----------------------------------------------------------------------
real :: biomassc, bioatlaimax, grainyieldc, hi, xhlai, droughtfac, xn, wtncan, harvfrac(2)
real :: maxlai, turfac, agefac, nstres, pstres1, pstres2, seedno, skerwt, sdwt, sdwtam, adwtah
real :: bwam, bwah, gpp, gnup, xgnp, wtnup, aptnup, canwaa, cannaa, psdwt, sdrate, sdwtah, podwt
integer :: mdate, yrsim, yrplt, gstage, iemrg, iflr, imat, demrg, dflr, dmat, dnr1, dnr2, dnr3
integer :: gyrdoySim(20), lunio, errnum, isens, lnum, linc, found, trtnum, trt_rot, acount, dynamic
integer :: yrdoy, timdif, yrnr1, yrnr2

! Arrays which contain Simulated and Measured data for printing in OVERVIEW.OUT and EVALUATE.OUT files (OPVIEW subroutine)
character(len=17), parameter :: errkey='SALUS_OPHARV'
character :: ideto*1, idets*1, rnmode*1, iplti*1, crop*2, section*6, filea*12, fileio*30, pathex*80, stname*10(20)
character, dimension(EvaluateNum) :: olab*6, olap*6, x*6, Simulated*8, Measured*8, descrip*50     !OLAP in dap

! Arrays which contain data for printing in SUMMARY.OUT file
integer, parameter :: sumnum = 18
character*4, dimension(sumnum) :: label
real, dimension(sumnum) :: value

Type(ControlType) control
Type(SwitchType)  iswitch
Type(PlStresType) PlantStres

dynamic = control % dynamic
ideto   = iswitch % ideto
rnmode  = control % rnmode
yrsim   = control % yrsim
idets   = iswitch % idets
iplti   = iswitch % iplti
crop    = control % crop
fileio  = control % fileio

!-----------------------------------------------------------------------
acount = 22  !Number of FILEA headings

! Headings in FILEA for Measured data
data olab /   &
  'ADAT  ',   &  !1  DFLR 
  'PD1T  ',   &  !2  IFPD 
  'PDFT  ',   &  !3  IFSD 
  'MDAT  ',   &  !4  DMAT 
  'HWAM  ',   &  !5  XGWT 
  'PWAM  ',   &  !6  XPDW 
  'H#AM  ',   &  !7  XNOGR
  'HWUM  ',   &  !8  XGWU 
  'H#UM  ',   &  !9  XNOGU
  'CWAM  ',   &  !10 XCWT 
  'BWAM  ',   &  !11 XSWT 
  'LAIX  ',   &  !12 XLAM 
  'HIAM  ',   &  !13 XHIN 
  'THAM  ',   &  !14 XTHR 
  'GNAM  ',   &  !15 XNGR 
  'CNAM  ',   &  !16 XNTP 
  'SNAM  ',   &  !17 XNST 
  'GN%M  ',   &  !18 XNPS 
  'CWAA  ',   &  !19 XCWAA
  'CNAA  ',   &  !20 XCNAA
  'L#SM  ',   &  !21 XLFNO
  'EDAT  ',   &  !22 Emergence date
   18*'      '/   
!-----------------------------------------------------------------------
data stname /    &  !Stage
  'Germinate ',  &  !1
  'Emergence ',  &  !2
  'Beg Lf Sen',  &  !3
  'Maturity  ',  &  !4
  '          ',  &  !5
  '          ',  &  !6
  '          ',  &  !7
  '          ',  &  !8
  '          ',  &  !9
  '          ',  &  !10
  '          ',  &  !11
  '          ',  &  !12
  'Planting  ',  &  !13
  'Start Sim ',  &  !14
  'End Sim   ',  &  !15
  'Harvest   ',  &  !16
  '          ',  &  !17
  '          ',  &  !18
  '          ',  &  !19
  'Harvest   '/     !20

!***********************************************************************
!***********************************************************************
! Run Initialization
!***********************************************************************
if(dynamic == runinit) then
!-----------------------------------------------------------------------
! Read FILEIO
call getlun('fileio', lunio)
open (lunio, file=fileio, status='old', iostat=errnum)
if(errnum /= 0) call error(errkey, errnum, fileio, 0)

read (lunio, '(55x,i5)', iostat=errnum) isens; lnum = 1
if(errnum /= 0) call error(errkey, errnum, fileio, lnum)

read (lunio, '(3(/),15x,a12,1x,a80)', iostat=errnum) filea, pathex
lnum = lnum + 4  
if(errnum /= 0) call error(errkey, errnum, fileio, lnum)

section = '*TREAT'
call find(lunio, section, linc, found) ; lnum = lnum + linc
if(found == 0) then
  call error(section, 42, fileio, lnum)
else
  read(lunio, '(i3)', iostat=errnum) trtnum ; lnum = lnum + 1
  if (errnum /= 0) call error(errkey, errnum, fileio, lnum)
endif

close (lunio)

! Assign descriptions to Measured and Simulated data from DATA.CDE
call getdesc(acount, olab, descrip)
olap = olab

Pstres1 = 1.0
Pstres2 = 1.0

call opview(control, biomassc, acount, descrip, ideto, xn, measured, plantstres,  &
  simulated, gyrdoySim, stname, wtncan, xhlai, nint(grainyieldc), yrplt, gstage)

!***********************************************************************
!***********************************************************************
! Seasonal Initialization
!***********************************************************************
elseif(dynamic == seasinit) then
!-----------------------------------------------------------------------
! Variables not currently calculated
agefac = 1.0
aptnup = -99.
cannaa = -99.
canwaa = -99.
gnup   = -99.
nstres = 1.0
pstres1 = 1.0
pstres2 = 1.0
turfac  = 1.0
wtncan  = -9.9     !Will be multiplied by 10 in Opview to yield -99
wtnup   = -99.
xgnp    = -99.
xn      = -99.
seedno  = -99.
bwam    = -99.
bwah    = -99.
skerwt  = -99.
gpp     = -99.
podwt   = -99.
Simulated = ' '
Measured  = ' '

! Establish # and names of stages for environmental & stress summary
PlantStres % ACTIVE = .FALSE.
PlantStres % NSTAGES = 5

PlantStres % StageName(0) = 'Planting to Harvest    '
PlantStres % StageName(1) = 'Germinat. to Emergence '
PlantStres % StageName(2) = 'Emerg. to Beg. Lf Sen. '
PlantStres % StageName(3) = 'Beg. Lf Sen. to Matur. '
PlantStres % StageName(4) = '                       '
PlantStres % StageName(5) = '                       '

call opview(control, biomassc, acount, descrip, ideto, xn, measured, plantstres,  &
  simulated, gyrdoySim, stname, wtncan, xhlai, nint(grainyieldc), yrplt, gstage)
    
maxlai = 0.0

!***********************************************************************
!***********************************************************************
! Daily Output
!***********************************************************************
else if (dynamic == output) then
!-----------------------------------------------------------------------
maxlai = amax1 (maxlai, xhlai)      ! Maximum XLAI season

PlantStres % W_grow = turfac 
PlantStres % W_phot = droughtfac  
PlantStres % N_grow = agefac 
PlantStres % N_phot = nstres 
PlantStres % P_grow = pstres2
PlantStres % P_phot = pstres1
PlantStres % active = .FALSE.

if(gstage > 0 .AND. gstage < 4) then
  plantstres % active(gstage) = .true.
end if

yrdoy = control % yrdoy
if(yrdoy >= yrplt) then
  if(mdate < 0 .OR. (mdate > 0 .AND. yrdoy < mdate)) then
    plantstres % active(0) = .true.
  endif
endif

! Send data to Overview.out data on days where stages occur
call opview(control, biomassc, acount, descrip, ideto, xn, measured, plantstres,  &
  simulated, gyrdoySim, stname, wtncan, xhlai, nint(grainyieldc), yrplt, gstage)

!***********************************************************************
!***********************************************************************
! Seasonal Output 
!***********************************************************************
else if (dynamic == seasend) then
!-----------------------------------------------------------------------
! Transfer dates for model stages
yrnr1  = gyrdoySim(2)
yrnr2  = gyrdoySim(3)

if(biomassc > 0.0 .AND. grainyieldc >= 0.0) then
   hi = grainyieldc/biomassc
else
   hi = 0.0
end if

!-----------------------------------------------------------------------
! Actual yield harvested (default is 100 %)
!-----------------------------------------------------------------------
sdwt = grainyieldc / 10.0
sdwtam = sdwt
sdwtah = sdwt * harvfrac(1)

!-----------------------------------------------------------------------
if((ideto == 'Y' .OR. index('IAEBCGDT', rnmode) > 0) .OR.   &
   (index('AY',idets) > 0 .AND. crop /= 'FA')) then
   if(index('FQ',rnmode) > 0) then
      trt_rot = control % rotnum
   else
      trt_rot = trtnum
   end if
   call reada(filea, pathex, olab, trt_rot, yrsim, x)
   
   !-----------------------------------------------------------------------
   ! Convert from YRDOY format to DAP.  Change descriptions to match
   !Measured ADAT (see array index of olab)
   call reada_dates(x(1), yrsim, iflr)
   if(iflr > 0 .AND. iplti == 'R' .AND. isens == 0) then
      dflr = timdif(yrplt, iflr)
   else
     dflr  = -99
   end if
   olap(1) = 'ADAP  '
   call getdesc(1, olap(1), descrip(1))
   
   !Measured maturity date
   call reada_dates(x(4), yrsim, imat)
   if(imat > 0 .AND. iplti == 'R' .AND. isens == 0) then
      dmat = timdif(yrplt, imat)
   else
      dmat  = -99
   end if
   olap(4) = 'MDAP  '
   call getdesc(1,olap(4), descrip(4))
   
   !Measured emergence date 
   call reada_dates(x(22), yrsim, iemrg)
   if(iemrg > 0 .AND. iplti == 'R' .AND. isens == 0) then
      demrg = timdif(yrplt, iemrg)
   else
      demrg  = -99
   end if
   olap(23) = 'EDAP  '
   call getdesc(1, olap(22), descrip(22))
   
   !Simulated emergence date
   if(yrplt > 0) then
      dnr1 = timdif(yrplt, gyrdoySim(2))
      if(dnr1 <= 0) then
         dnr1 = -99
      end if
   else
      dnr1 = -99
   end if
   
   !Simulated date of beginning leaf senescence (ADAT)
   if(yrplt > 0) then
      dnr2 = timdif(yrplt, gyrdoySim(3))
      if(dnr2 <= 0) then
         dnr2 = -99
      end if
   else
      dnr2 = -99
   end if
   
   !Simulated maturity date
   if(yrplt > 0) then
      dnr3 = timdif(yrplt, mdate)
      if(dnr3 <= 0) then
         dnr3 = -99
      end if
   else
      dnr3 = -99
   end if
   
   write(Simulated(1),'(I8)') dnr2;              write(Measured(1),'(I8)') dflr    !ADAT
   write(Simulated(2),'(I8)') -99 ;              write(Measured(2),'(I8)') -99     !PD1T
   write(Simulated(3),'(I8)') -99 ;              write(Measured(3),'(I8)') -99     !PDFT
   write(Simulated(4),'(I8)') dnr3;              write(Measured(4),'(I8)') dmat    !MDAT
   write(Simulated(5),'(I8)') nint(grainyieldc); write(Measured(5),'(A8)') X(5)    !HWAM
   write(Simulated(6),'(I8)') -99 ;              write(Measured(6),'(I8)') -99     !PWAM
   write(Simulated(7),'(I8)') -99;               write(Measured(7),'(A8)') X(7)    !H#AM
   write(Simulated(8),'(F8.4)') -99.;            write(Measured(8),'(A8)') X(8)    !HWUM 
   write(Simulated(9),'(F8.1)') -99.;            write(Measured(9),'(A8)') X(9)    !H#UM 
   write(Simulated(10),'(I8)') nint(biomassc);   write(Measured(10),'(A8)') X(10)  !CWAM
   write(Simulated(11),'(I8)') -99;              write(Measured(11),'(A8)') X(11)  !BWAM
   write(Simulated(12),'(F8.2)') maxlai;         write(Measured(12),'(A8)') X(12)  !LAIX
   write(Simulated(13),'(F8.3)') hi;             write(Measured(13),'(A8)') X(13)  !HIAM
   write(Simulated(14),'(I8)') -99 ;             write(Measured(14),'(I8)') -99    !THAM
   write(Simulated(15),'(I8)') -99;              write(Measured(15),'(A8)') X(15)  !GNAM
   write(Simulated(16),'(I8)') -99;              write(Measured(16),'(A8)') X(16)  !CNAM
   write(Simulated(17),'(I8)') -99;              write(Measured(17),'(A8)') X(17)  !SNAM
   write(Simulated(18),'(F8.1)') -99.;           write(Measured(18),'(A8)') X(18)  !GN%M
   write(Simulated(19),'(I8)') -99;              write(Measured(19),'(A8)') X(19)  !CWAA
   write(Simulated(20),'(I8)') -99;              write(Measured(20),'(A8)') X(20)  !CNAA
   write(Simulated(21),'(F8.2)') -99.;           write(Measured(21),'(A8)') X(21)  !L#SM
   write(Simulated(22),'(I8)') dnr1;             write(Measured(22),'(I8)') demrg

end if

call opview(control, biomassc, acount, descrip, ideto, xn, measured, plantstres,  &
  simulated, gyrdoySim, stname, wtncan, xhlai, nint(grainyieldc), yrplt, gstage)

!-------------------------------------------------------------------
! Send information to OPSUM to generate SUMMARY.OUT file
!-------------------------------------------------------------------
psdwt  = skerwt
sdrate = -99.0

! Store Summary.out labels and values in arrays to send to OPSUM routines for printing.  Integers are temporarily 
! saved as real numbers for placement in real array.
label(1)  = 'ADAT'; value(1)  = float(yrnr2)
label(2)  = 'MDAT'; value(2)  = float(mdate)
label(3)  = 'DWAP'; value(3)  = sdrate
label(4)  = 'CWAM'; value(4)  = biomassc
label(5)  = 'HWAM'; value(5)  = sdwtam*10.
label(6)  = 'HWAH'; value(6)  = sdwtah*10.
! bwah multiplied by 10.0 in OPSUM - divide by 10. here to preserve units.
!label(7)  = 'BWAH'; value(7)  = bwah / 10.
label(7)  = 'BWAH'; value(7)  = bwah
label(8)  = 'HWUM'; value(8)  = psdwt       !*1000.
label(9)  = 'H#AM'; value(9)  = seedno
label(10) = 'H#UM'; value(10) = gpp
label(11) = 'NFXM'; value(11) = 0.0         !WTNFX*10.
!label(12) = 'NUCM'; value(12) = wtnup*10.
label(12) = 'NUCM'; value(12) = wtnup
!label(13) = 'CNAM'; value(13) = wtncan*10.
label(13) = 'CNAM'; value(13) = wtncan
label(14) = 'GNAM'; value(14) = gnup        !WTNSD*10.
!label(15) = 'PWAM'; value(15) = podwt*10.
label(15) = 'PWAM'; value(15) = podwt
label(16) = 'LAIX'; value(16) = xhlai
label(17) = 'HIAM'; value(17) = hi
label(18) = 'EDAT'; value(18) = float(yrnr1)

!Send labels and values to OPSUM
call sumvals(sumnum, label, value) 

!Send Measured and Simulated data to OPSUM
call evaluatedat(acount, Measured, Simulated, descrip, olap) 

!***********************************************************************
!***********************************************************************
! END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
end if
!***********************************************************************
return
end subroutine SALUS_Opharv
!=======================================================================



