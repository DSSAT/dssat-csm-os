!=======================================================================
! Kernel Number subroutine for AgMaize by Thijs Tollenaar
! Calculates kernel number as a function of plant growth rate
! See Tollenaar et al.(1992): Crop Sci. 32: 432-438, Table 4
! New kernel function see Echarte et al. 2004: Crop Science 44: 1654-1661
!-----------------------------------------------------------------------
! REVISION HISTORY
! Originally written by Thijs Tollenaar
! 07/11/2013 Translated into Fortran - Kofikuma Dzotsi
! 03/05/2014 New kernel number function (Echarte et al. 2004)
!-----------------------------------------------------------------------
!  Called from: MZ_AG_Growth   
!  Calls:       None  
!=======================================================================
subroutine MZ_AG_KernelNum(pgrorate,       & !Input from Growth
                           kernelnumber)     !Output
   
!-----------------------------------------------------------------------
implicit none
save
!-----------------------------------------------------------------------
real, parameter :: xzero=0.42, potkn=493.0    !Cultivar parameters xzero: 0.52; potkn: 852.0
real, parameter :: curpar=1.52                !curpar=1.52
real, intent(in) :: pgrorate
real, intent(out) :: kernelnumber

if(pgrorate < xzero) then
   kernelnumber = 0.0
else
   kernelnumber = max(0.0, potkn * (1 - exp(-(pgrorate-xzero)/curpar)) )
end if   
   

!real, parameter :: a=276.0, b=0.244, xzero=0.7, xint=6.5
!real, intent(in) :: pgrorate
!real, intent(out) :: kernelnumber
!real :: yint, yr, xf
!if(pgrorate <= xzero) then
!   kernelnumber = 0.0
!else if(pgrorate < xint) then
!   kernelnumber = a * (pgrorate - xzero)/(1.0 + (b*(pgrorate - xzero)))
!else if(pgrorate >= xint) then
!   yint = a * (xint - xzero) / (1.0 + (b * (xint - xzero)))
!   yr = a * (6.0 - xzero) / (1.0 + (b * (6.0 - xzero)))
!   xf = pgrorate - (a * xint - (1.0 + b * xint) * (yint - yr)) / (a - b * (yint - yr))
!   kernelnumber = yr + (a * xf) / (1.0 + b * xf)
!end if

return
end subroutine MZ_AG_KernelNum


	  
!==============================================================================================================================
! Variable definitions                                                                                  Unit 
!------------------------------------------------------------------------------------------------------------------------------
! xzero Kernel number threshold, g/plt/day
! potkn Potential kernel number per plant/ear
! curpar Degree of curvilinearity of kernel number vs. plant growth rate relationship
! pgrorate        Average daily plant growth rate used to determine kernel number                       g[dm]/plt/day
! kernelnumber    Kernel number                                                                         kernel/plant
!==============================================================================================================================


