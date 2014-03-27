!=======================================================================
!  RESPIR, Subroutine, K.J.Boote, J.W.Jones, G. Hoogenboom
!  Calculates daily maintainence and growth respiration.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  01/01/1989 KJB Written.
!  10/15/1992 NBP Made into subroutine.
!  09/15/1998 CHP Modified for modular format
!  05/11/1999 GH  Incorporated in CROPGRO
!  03/20/2003 JIL Modified and added growth respiration
!  09/12/2007 JIL Adapted for IXIM model
!-----------------------------------------------------------------------
!  Called from:   PHOTSYNT
!  Calls:         
!=======================================================================
subroutine MZ_AG_RespirIXIM(control, weather,                   & !Input
           pgross, wtmain, glv, grt, gst, egr, ggr,             & !Input
           maint, cvf)                                            !Output
!-----------------------------------------------------------------------
use ModuleDefs
use MZ_AG_ModuleDefs
implicit none
save
!-----------------------------------------------------------------------
integer :: dynamic, h
real, dimension(24) :: tairhr
real, intent(out) :: maint, cvf
real :: pcarlf, pcarst, pcarrt, pcarea, pcarsd   !Carbohydrate fractions in leaves, stems, roots, reproductive organs and grain
real :: pprolf, pprost, pprort, pproea, pprosd   !Protein fractions in leaves, stems, roots, reproductive organs and grain
real :: pliplf, plipst, pliprt, plipea, plipsd   !Lipid fractions in leaves, stems, roots, reproductive organs and grain
real :: pliglf, pligst, pligrt, pligea, pligsd   !Lignin fractions in leaves, stems, roots, reproductive organs and grain
real :: poalf, poast, poart, poaea, poasd        !Organic acid fractions in leaves, stems, roots, reproductive organs and grain
real :: pminlf, pminst, pminrt, pminea, pminsd   !Mineral fractions in leaves, stems, roots, reproductive organs and grain
real :: r30c2, res30c
real :: glv, grt, gst, egr, ggr          !Growth rates of organs
real :: wtmain, pgross
real :: rcea, rcgr, rclf, rcrt, rcst, ro, rp, trsfac
character(len=12) :: files
character(len=80) :: pathsr
type(ControlType) :: control
type(WeatherType) :: weather
type(FileioType)  :: datafileio
type(SpeciesType) :: dataspecies
tairhr  = weather  % tairhr
dynamic = control  % dynamic

!----------------------------------------------------------------------
! Dynamic = runinit
!----------------------------------------------------------------------
if(dynamic == runinit .or. dynamic == seasinit) then
cvf    = 0.0  
h      = 1
maint  = 0.0 
rcea   = 0.0  
rcgr   = 0.0  
rclf   = 0.0  
rcrt   = 0.0  
rcst   = 0.0  
ro     = 0.0  
rp     = 0.0  
trsfac = 0.0  
wtmain = 0.0
glv    = 0.0
grt    = 0.0
gst    = 0.0
egr    = 0.0
ggr    = 0.0

!Read fileio case 'FILENP' and transfer variables
call readfileio(control, 'FILENP', datafileio)
files  = datafileio % files
pathsr = datafileio % pathsr

!Read respiration parameters from species file and transfer variables
call readspecies(files, pathsr, '*RESPI', dataspecies)
res30c = dataspecies % res30c 
r30c2  = dataspecies % r30c2 

!Read plant composition parameters from species file and transfer variables
call readspecies(files, pathsr, '*PLANT', dataspecies)  
!Get carbohydrate fractions
pcarlf = dataspecies % pcarlf    
pcarst = dataspecies % pcarst     
pcarrt = dataspecies % pcarrt   
pcarea = dataspecies % pcarea   
pcarsd = dataspecies % pcarsd   

!Get protein fractions
pprolf = dataspecies % pprolf    
pprost = dataspecies % pprost     
pprort = dataspecies % pprort   
pproea = dataspecies % pproea   
pprosd = dataspecies % pprosd   

!Get lipid fractions
pliplf = dataspecies % pliplf    
plipst = dataspecies % plipst     
pliprt = dataspecies % pliprt   
plipea = dataspecies % plipea   
plipsd = dataspecies % plipsd 

!Get lignin fractions
pliglf = dataspecies % pliglf    
pligst = dataspecies % pligst     
pligrt = dataspecies % pligrt   
pligea = dataspecies % pligea   
pligsd = dataspecies % pligsd

!Get organic acid fractions
poalf = dataspecies % poalf    
poast = dataspecies % poast     
poart = dataspecies % poart   
poaea = dataspecies % poaea   
poasd = dataspecies % poasd

!Get mineral fractions
pminlf = dataspecies % pminlf    
pminst = dataspecies % pminst     
pminrt = dataspecies % pminrt   
pminea = dataspecies % pminea   
pminsd = dataspecies % pminsd

!-----------------------------------------------------------------------
! Dynamic = integr
!-----------------------------------------------------------------------
else if(dynamic == integr) then   

!-----------------------------------------------------------------------
! Temperature effect on maintenance respiration (McCree, 1974)
!-----------------------------------------------------------------------
trsfac = 0.0
do h = 1,24
   trsfac = trsfac + 0.044+0.0019*tairhr(h)+0.001*tairhr(h)**2
end do

!trsfac = sum((0.044+0.0019*tairhr(h)+0.001*tairhr(h)**2, h=1,24))

!-----------------------------------------------------------------------
! Convert maintenance respiration to actual temperature. RES30C is
! the g CH2O/g DW hr used in maintenance respiration at 30 C.
! R30C2 is the g of CH2O/g CH2O hr at 30 C. MAINR is g CH2O/m2 d 
!-----------------------------------------------------------------------
ro = res30c * trsfac
rp = r30c2 * trsfac
maint = ro*wtmain + rp*pgross

!-----------------------------------------------------------------------
! Calculate growth respiration according to Penning de Vries and van
! Laar (1982) and Boote et al (1998)
!-----------------------------------------------------------------------
! Calculate growth respiratory cost per organ (g[CH2O]/g[DM tissue])
! according to Table 11, p 61 (Penning de Vries et al, 1989)
rclf = pliplf*3.189 + pliglf*2.231 + poalf*0.954 + pminlf*0.12 + pcarlf*1.275 + pprolf*1.887
rcst = plipst*3.189 + pligst*2.231 + poast*0.954 + pminst*0.12 + pcarst*1.275 + pprost*1.887
rcrt = pliprt*3.189 + pligrt*2.231 + poart*0.954 + pminrt*0.12 + pcarrt*1.275 + pprort*1.887
rcea = plipea*3.189 + pligea*2.231 + poaea*0.954 + pminea*0.12 + pcarea*1.275 + pproea*1.887
rcgr = plipsd*3.189 + pligsd*2.231 + poasd*0.954 + pminsd*0.12 + pcarsd*1.275 + pprosd*1.887

! Calculate weighted conversion factor for the whole plant cvf (g[CH2O]/g[DM])
if((glv + gst + grt + egr + ggr) == 0.0) then
    cvf = 0.0
else
    cvf = (glv*rclf + gst*rcst + grt*rcrt + egr*rcea + ggr*rcgr) / (glv + gst + grt + egr + ggr)
end if
  
!-----------------------------------------------------------------------
! End of dynamic if structure
!-----------------------------------------------------------------------  
end if     

!-----------------------------------------------------------------------
! End of subroutine
!-----------------------------------------------------------------------    
return
end subroutine MZ_AG_RespirIXIM


	  
!-----------------------------------------------------------------------
! Respir variables:
!-----------------------------------------------------------------------
! mainr   Maintenance respiration (g[CH2O] / m2 / d)
! pg      Daily gross photosynthesis (g[CH2O] / m2 / d)
! r30c2   Respiration coefficient that depends on total plant mass, value 
!           at 30C (g[CH2O] used / g[CH2O] fixed / hr)
! res30c  Respiration coefficient that depends on gross photosynthesis, 
!           value at 30C (g CH2O/g DW/hr)
! ro      Respiration coefficient that depends on total plant mass
!           (g[CH2O] / g[tissue])
! rp      proportion of the day's photosynthesis which is respired in the 
!           maintenance process 
! tgro(i) Hourly air temperature (°C)
! trsfac  Temperature effect on maintenance respiration 
! ts      Number of intermediate time steps (=24) 
! wtmain  Mass of tissue assumed to require maintenance (g[tissue] / m2)

