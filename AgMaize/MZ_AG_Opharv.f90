!======================================================================================
! MZ_AG_Opharv
! Produces AgMaize's end-of-season outputs
! Write the following files:
!   AG_GroStage.OUT
!----------------------------------------------------------------------
! Revision History
! 03/05/2013 KAD Written (based on MZ_Opharv)
!----------------------------------------------------------------------
! Called from: Main program: MZ_AG_GLOBAL
! Calls      : None
! Utility subroutines called: reada (in reads.for), yr_doy
!======================================================================================
subroutine MZ_AG_Opharv(control, iswitch, gstddoySim, gstdyearSim, growthtlu)    
      
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

! Variables to write to GM_GroStages.OUT
character, dimension(10) :: gstddes*30, gstddoyObs*3
integer, dimension(10)   :: gstddoySim, gstdyearSim, gstdnum
real, dimension(10)      :: growthtlu

! Other variables
character(len=16), parameter :: gstdfile='AG_GroStage.OUT'
character :: filea*12, pathex*80
character(len=6), dimension(EvaluateNum) :: olab, X
logical :: gstdfexist
integer :: i, exptrt, trtnum, year, doy, gstdunit

! Arrays with simulated data to be printed in a summary file and to the console
integer, parameter :: sumnum = 17
character(len=4), dimension(sumnum) :: label
real, dimension(sumnum) :: SimValues

! Constructed type variables
character :: ideto*1, idets*1, rnmode*1, crop*2, fileio*30
integer :: dynamic, run, yrsim, yrdoy

! Transfer values from constructed data types into local variables      
dynamic = control % dynamic
crop    = control % crop
run     = control % run
rnmode  = control % rnmode
fileio  = control % fileio
yrsim   = control % yrsim
yrdoy   = control % yrdoy
ideto   = iswitch % ideto
idets   = iswitch % idets

! Define growth stages
gstddes(1:10) = ['Germination','Emergence','End of Juvenile Phase','Tassel Initiation','Appearance of Topmost Leaf',  &
                 'Anthesis','Silking','Onset of Linear Grain Filling','50% Milk Line','Black Layer']  
gstdnum(1:10) = [(i, i=1,10, 1)]  

! Define olab: vector of headers in FILEA; 1.ADAT(anthesis date); 4.MDAT(Maturity date); 22.EDAT(Emergence date)
! LDAT (silking date), KDAT (milk line date), GDAT (onset gr. filling date)
olab(1:25) = ['ADAT  ','PD1T  ','PDFT  ','MDAT  ','HWAM  ','PWAM  ','H#AM  ','HWUM  ','H#UM  ','CWAM  ',     &
              'BWAM  ','LAIX  ','HIAM  ','THAM  ','GNAM  ','CNAM  ','SNAM  ','GN%M  ','CWAA  ','CNAA  ',     &
              'L#SM  ','EDAT  ','LDAT  ','KDAT  ','GDAT  ']

!----------------------------------------------------------------------
! Dynamic = runinit
!----------------------------------------------------------------------
if(dynamic == runinit) then

   !Read fileio case 'FILENP' and transfer variables
   call readfileio(control, 'FILENP', datafileio)
   filea  = datafileio % filea 
   pathex = datafileio % pathex
   
   !Read fileio case 'INPUTS' and transfer variables
   call readfileio(control, 'INPUTS', datafileio)
   trtnum = datafileio % trtnum   

   gstddoyObs = '-99'
       
!----------------------------------------------------------------------
! Dynamic = seasinit
!----------------------------------------------------------------------
else if(dynamic==seasinit) then

!-------------------------------------------------------------
! Open GM_GroStages.OUT as new or existing file
!-------------------------------------------------------------
  if(crop /= 'FA') then
     call getlun('gstdfile', gstdunit)
     inquire(file=gstdfile, exist=gstdfexist)
     if(gstdfexist) then
        open(unit=gstdunit, file=gstdfile, status='old', position='append')
     else
        open(unit=gstdunit, file=gstdfile, status='new')
        write(gstdunit,'("GROWTH STAGE OCCURRENCE DATES (Day Of Year)")')   
     endif 
     call header(seasinit, gstdunit, run)     
     write(gstdunit, '("*GROWTH STAGES",19X,"GSTD",3X,"TLU",2X,"YEAR",2X,"SIMU",2X,"MEAS")')
  end if

!----------------------------------------------------------------------
! Dynamic = output
!----------------------------------------------------------------------
else if(dynamic==output) then

        
!----------------------------------------------------------------------
! Dynamic = seasend
!----------------------------------------------------------------------
else if(dynamic==seasend) then

 !---------------------------------------------------------
 ! Get observed growth stage information
 !---------------------------------------------------------
  if((ideto == 'Y' .or. index('IAEBCGDT',rnmode) > 0) .or. (index('AY',idets) > 0 .and. crop /= 'FA')) then
      if(index('FQ',rnmode) > 0) then
         exptrt = control % rotnum
      else
         exptrt = trtnum
      end if
      call reada(filea, pathex, olab, exptrt, yrsim, X)
      !Save measured growth stage values for output 
      gstddoyObs(2)  = adjustl(X(22))   ! 2 Emergence (doy)
      gstddoyObs(6)  = adjustl(X(1))    ! 6 Anthesis (doy)
      gstddoyObs(7)  = adjustl(X(23))   ! 7 Silking (doy)
      gstddoyObs(8)  = adjustl(X(25))   ! 8 Onset grain filling (doy)
      gstddoyObs(9)  = adjustl(X(24))   ! 9 Milk line (doy)
      gstddoyObs(10) = adjustl(X(4))    !10 Black layer(maturity, doy)
  end if  
      
 !---------------------------------------------------------
 ! Write growth stages information and close file
 !---------------------------------------------------------
 call yr_doy(yrdoy, year, doy)
 do i = 1, size(gstddes)
    write(gstdunit, '(1X,A30,2X,I4,2X,F4.1,2X,I4,2X,I4,2X,A4)') gstddes(i), gstdnum(i), growthtlu(i), gstdyearSim(i), gstddoySim(i), gstddoyObs(i)
 end do 
 close(gstdunit)
 
!-----------------------------------------------------------------------
! End of dynamic if structure
!-----------------------------------------------------------------------
endif
return

!----------------------------------------------------------------------
! End of subroutine
!----------------------------------------------------------------------
end subroutine MZ_AG_Opharv


      
!==============================================================================================================================
! Variable definitions                                                                                  Unit 
!------------------------------------------------------------------------------------------------------------------------------
!
!==============================================================================================================================


