!=======================================================================
!  DayCent_diffusivity, Subroutine
!
!     dD0_fc = f(dul, bd, wfps_fc)
!
!-----------------------------------------------------------------------
!  Revision history
!  06/12/2014 PG / CHP Written
!-----------------------------------------------------------------------
!  Called : SOIL
!  Calls  : Fert_Place, IPSOIL, NCHECK, NFLUX, RPLACE,
!           SOILNI, YR_DOY, FLOOD_CHEM, OXLAYER
!=======================================================================
      subroutine DayCent_diffusivity(dD0, sw, soilprop)      
!      the variable dDO_fc is manually calculated for the moment

      use ModuleDefs
      IMPLICIT NONE

      real, dimension(NL), intent(out):: dD0
      real, dimension(NL), intent(IN) :: SW
      integer L, nlayr
      type (SoilType), intent(in) :: SOILPROP
      
      real, dimension(NL) :: dul, bd, poros
      real, dimension(NL) :: dD0_fc, dD0_DayCent, ratio
      real POROSer
      REAL PFC, VFRAC, THETA_V, THETA_P, THETA_A, S_WAT, SW_P
      REAL TP1, TP2, TP3, TP4, TP5, TP6, TP7, TP8

      BD    = SOILPROP % BD
      DUL   = SOILPROP % DUL
      POROS = SOILPROP % POROS
      NLAYR = SOILPROP % NLAYR

! From Iurii Shcherbak 6 July 2014
! Linear model
! Dfc = 0.741804 - 0.287059 BD - 0.755057 FC
! Adj R^2 = 0.999412
!  
! Quadratic
! Dfc = 0.906277 - 0.461712 BD + 0.045703 BD^2 - 1.19827 FC +  0.225558 BD FC + 0.292491 FC^2
! Adj R^2 = 0.999997

      do L = 1, Nlayr
!       Linear
!       dD0_fc(L) = 0.741804 - 0.287059 * BD(L) - 0.755057 * FC(L)

!       Quadratic
        dD0_fc(L) = 0.906277 - 0.461712 * BD(L) 
     &                       + 0.045703 * BD(L) * BD(L) 
     &                       - 1.19827  * DUL(L) 
     &                       + 0.225558 * BD(L) * DUL(L) 
     &                       + 0.292491 * DUL(L) * DUL(L)
      enddo

!     Temporary function for diffusivity - set to constant for now
!      dD0_fc = 0.10    ! 0.10 for KT, fixed value since diffusivity routine is not working
!      dD0_fc = 0.16    ! 0.16 for KY, fixed value since diffusivity routine is not working
!      dD0_fc = 0.11    ! 0.11 for Canada (use KT), fixed value since diffusivity routine is not working
!      dD0_fc = 0.21    ! 0.21 for India, fixed value since diffusivity routine is not working
!      dD0_fc = 0.09    ! 0.09 for France (use KT), fixed value since diffusivity routine is not working

!*****************************************************************************
!*****************************************************************************
!!     Diffusivity from DayCent:
!!     chp 4/18/2017
!
!*              Copyright 1993 Colorado State University                    *
!*                      All Rights Reserved                                 *
! 
!*****************************************************************************
!**
!**  FILE:      diffusiv.c
!**
!**  FUNCTION:  float diffusiv()
!**
!**  PURPOSE:   For estimating normalized diffusivity in soils.  Method of
!**             Millington and Shearer (1971) Soil Science Literature Source:
!**             Davidson, E.A. and S.E. Trumbore (1995).
!**             Contributed by Chris Potter, NASA Ames
!**  
!**  INPUTS:
!**    A       - is the fraction of soil bed volume occuppied by field capacity
!**              (intra-aggregate pore space), units 0-1
!**    bulkden - bulk density (g/cm3)
!**    wfps    - water-filled pore space (fraction 0.0-1.0)
!**              (fraction of a porespace that is filled with water)
!**              volumetric water / porosity
!**
!**  GLOBAL VARIABLES:
!**    None
!**
!**  LOCAL VARIABLES:
!**    debug      - flag to set debugging mode, 0 = off, 1 = on
!**    my_theta_V - testing alternate way of computing the volumetric water
!**                 content of the soil bed volume
!**    PARTDENS   - particle density (g/cm3)
!**    pfc        - water content of soil expressed as a percent of field
!**                 capacity (%)
!**    porosity   - the fraction of soil bed volume occupied by pore space (P)
!**                 (A + inter-aggregate pore space), units 0-1
!**                  = 1 - bulkden/particle density
!**    sw_p       - the fractional liquid saturation of the P component of
!**                 total pore volume
!**    s_wat      - the fractional liquid saturation of the A component of
!**                 total pore volume
!**    theta_A    - the volume of water per unit bed volume contained in
!**                 intra-aggregate pore space
!**    theta_P    - the volume of water per unit bed volume contained in
!**                 inter-aggregate pore space
!**    theta_V    - the volumetric water content of the soil bed volume
!**    tp1 .. tp8 - intermediate variables
!**    vfrac      - volumetric fraction    
!**
!**  OUTPUT:
!**    dDO - the normalized diffusivity in aggregate soil media, units 0-1
!
      do L = 1, Nlayr
!       Inter-aggregate porosity
        POROSer = POROS(L) - DUL(L)
        pfc = SW(L) / DUL(L) * 100.   !water content as percent of DUL
        if (pfc >= 100.) then
          VFRAC = (SW(L) - DUL(L)) / POROSer
          THETA_V = DUL(L) + MIN(VFRAC,1.0) * (POROS(L) - DUL(L))
        else
          VFRAC = 0.0
          THETA_V = SW(L)
        endif 

        THETA_P = MAX(0.0, (THETA_V - DUL(L)))
        THETA_A = MIN(DUL(L), THETA_V)

        S_WAT = MIN(1.0, SW(L)/DUL(L))
        SW_P = MIN(1.0, THETA_P/POROSer)

        TP1 = (1.0 - S_WAT) ** 2.0
        TP2 = (DUL(L) - THETA_A) / (1.0 - POROSer)
        TP3 = TP2 ** (0.5*TP2 + 1.16)
        TP4 = 1.0 - POROSer ** (0.5*POROSer + 1.16)
        TP5 = POROSer - THETA_P
        TP6 = MAX(0.0, TP5 ** (0.5*TP5 + 1.16))
        TP7 = (1.0 - SW_P) ** 2.0
        TP8 = MAX(0.0, (TP1 * TP3 * TP4 * (TP5-TP6)) / 
     &   (1.E-6 + (TP1 * TP3 * TP4) + TP5 - TP6) * 1.E7)

        dD0_DayCent(L) = MAX(0.0, (TP8/1.E7 + TP7 * TP6))

!*****************************************************************************
!       compare with Shcherbak method
        ratio(L) = dD0_DayCent(L) / dD0_fc(L)
      enddo

      dD0 = dD0_DayCent

      return
      end subroutine DayCent_diffusivity
!=======================================================================

