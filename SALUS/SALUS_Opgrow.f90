!======================================================================
!  SA_OPGROW, Subroutine, G. Hoogenboom, J.W. Jones
!  Modified MZ_OPGROW by Kofikuma Dzotsi for Salus crop model
!----------------------------------------------------------------------
!  Generates output file for daily growth variables
!     This routine is used for maize, sorghum and millet.
!----------------------------------------------------------------------
!  REVISION HISTORY
!  01/01/1990     Written
!  09/21/1998 CHP Split off from OPDAY.FOR file
!  05/11/1999 GH  Incorporated in CROPGRO
!  12/18/2001 WDB Revised for modular CERES
!  08/20/2002 GH  Modified for Y2K
!  07/08/2003 CHP Changed senescence output.
!  05/31/2006 CHP Output DTT, growing degree days.
!  09/24/2009 KAD Modified for Salus
!----------------------------------------------------------------------
!  Called by: SALUS
!  Calls:     None
!======================================================================
subroutine SALUS_Opgrow(control, iswitch, dtt, mdate, biomassrootc, biomassc, xhlai, yrplt, droughtfac,  &
              cumtt, reltt, rue, rellai, hrvindstr, grainyieldc, rlv)
Use ModuleDefs 
Use ModuleData
implicit none
save
!----------------------------------------------------------------------
character :: idetg*1, outg*12
character*6, parameter :: errkey = 'SALUS_OPG'
integer :: i, dynamic, noutdg, errnum, run, l, yrplt, yrdoy, timdif, n_lyr
integer :: dap, das, mdate, frop, year, doy
logical :: fexist, first
real :: xhlai, biomassrootc, biomassc, grainyieldc, hrvindstr, droughtfac, dtt, cumtt
real :: reltt, rue, rellai, rlv(nl)

Type(ControlType) control
Type(SwitchType)  iswitch
Type(SoilType)    soilprop 
dynamic = control % dynamic
run     = control % run
yrdoy   = control % yrdoy
das     = control % das
frop    = control % frop
idetg   = iswitch % idetg
if(idetg == 'N') return

!-----------------------------------------------------------------------
! DYNAMIC = SEASINIT -  Seasonal initialization - run once per season
!-----------------------------------------------------------------------
if(dynamic == seasinit) then
   outg = 'PlantGro.OUT'
   call getlun('OUTG', noutdg)
   call get(soilprop)
   n_lyr = min(10, MAX(4,soilprop%nlayr))
         
   !-------------------------------------------------------------
   ! Open PlantGro.OUT as new or append
   !-------------------------------------------------------------
   inquire(file=outg, exist=fexist)
   if(fexist) then
      open(unit=noutdg, file=outg, status='old', iostat=errnum, position='append')
      first = .false.  
   else
      open(unit=noutdg, file=outg, status='new', iostat=errnum)
      write(noutdg, '("*GROWTH ASPECTS OUTPUT FILE")')
      first = .true.  
   endif

   !---------------------------------------------------------
   ! Generate variable heading for PlantGro.OUT
   !---------------------------------------------------------
   call header(seasinit, noutdg, run)
   write (noutdg, '("@YEAR DOY  DAS  DAP   LAID  RWAD  CWAD  GWAD   HIAD   WSPD  RELTT    RUE RELLAI")', advance='NO')
   do l = 1, n_lyr
      if(l < 10) then
         write(noutdg,'("    ",A2,I1,A1)',advance='NO') "RL",l,"D"
      else
         write(noutdg,'("   ",A2,I2,A1)', advance='NO') "RL",l,"D"
      end if
   end do
   write (noutdg, '("  DTTD  DTTC")')

!-----------------------------------------------------------------------
! DYNAMIC = OUTPUT
!-----------------------------------------------------------------------
else if(dynamic == output) then      
   if(yrdoy >= yrplt) then
      dap = max(0, timdif(yrplt, yrdoy))
      if(dap > das) dap = 0

      !-------------------------------------------------------------
      ! Write output based on user specified frequency 
      !-------------------------------------------------------------
      if((mod(das,frop) == 0) .OR. (yrdoy == yrplt) .OR. (yrdoy == mdate)) then  
         call yr_doy(yrdoy, year, doy)
		 write(noutdg, '(1x,i4,1x,i3.3,2(i5),1x,f6.2,3(1x,i5),5(1x,f6.2),10f8.2)', advance='NO')    &
         year, doy, das, dap, xhlai, nint(biomassrootc),nint(biomassc),nint(grainyieldc),hrvindstr, &
         (1-droughtfac), reltt, rue, rellai, (rlv(i),i=1,n_lyr)
          write(noutdg, '(f6.2, 1x, i5)') dtt, nint(cumtt)
      end if       
   end if

!-----------------------------------------------------------------------
! DYNAMIC = SEASEND
!-----------------------------------------------------------------------
else if(dynamic == seasend) then
   !Close daily output files.
   close(noutdg)

end if

!***********************************************************************
return
end subroutine SALUS_Opgrow
!=======================================================================


