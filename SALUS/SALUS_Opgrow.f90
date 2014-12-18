!======================================================================
!  SALUS_Opgrow
!  Modified MZ_OPGROW by KAD for SALUS crop model
!----------------------------------------------------------------------
!  Generates output file for daily growth variables
!----------------------------------------------------------------------
!  REVISION HISTORY
!  09/24/2009 KAD Modified MZ_OPGROW for SALUS
!  03/28/2014 KAD Removed SALUS.OUT and added more varibales to PlantGro.OUT
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
character*6, parameter :: errkey = 'SAL_OP'
character :: idetg*1, outg*12
logical :: fexist, first
integer :: i, dynamic, noutdg, errnum, run, l, yrplt, yrdoy, timdif, n_lyr, dap, das, mdate, frop, year, doy
real :: xhlai, biomassrootc, biomassc, grainyieldc, hrvindstr, droughtfac, dtt, cumtt, reltt, rue, rellai, rlv(nl)

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
if(dynamic == runinit) then
   outg = 'PlantGro.OUT'
   call getlun('OUTG', noutdg)
   
else if(dynamic == seasinit) then   
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


