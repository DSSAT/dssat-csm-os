!***********************************************************************
!  DECRAT_C, subroutine for CENTURY-based SOM/residue module of DSSAT.
!
!  Purpose: This subroutine calculates the effect of soil temperature
!           and soil water content on the decomposition rate
!           parameter.
!
!  Revision history:
!  ........ Parton et al.  Written for CENTURY model; the soil water
!                          section was written in the C language for
!                          the daily version of CENTURY.
!  01/01/1999 AJG  Revised and linked to DSSAT.
!  25/05/1999 AJG  Converted the soil water section to FORTRAN and
!                linked it to DSSAT.
!  01/01/2000 CHP/AJG Integrated the CENTURY-based and CERES-based SOM
!               modules with CHP's modular format.
!  07/24/2008 CHP  Replaced temperature function
!
!  Called: CENTURY
!  Calls : Function ATANF_C
!***********************************************************************

      SUBROUTINE DECRAT_C (ISWITCH,
     &    DLAYR, DUL, L, LL, SAT, ST, SW,                 !Input
     &    DEFAC)                                          !Output

!     ------------------------------------------------------------------
      USE ModuleDefs  
!     USE ModuleData  
                      
      IMPLICIT NONE
      SAVE
!     ------------------------------------------------------------------
      INTEGER L, SRFC
      PARAMETER (SRFC = 0)

!     REAL ATANF_C, NORMALIZER,
      REAL TFSOM, WFSOM, SWEF, XL

      REAL AD, 
     &  DEFAC(0:NL), DLAYR(NL), LL(NL), ST(NL), SW(NL),
     &  DUL(NL), SAT(NL)

      REAL SWL, DULL, SATL
      TYPE (SwitchType) ISWITCH

!     ------------------------------------------------------------------
!       Soil temperature factor.
!       ----------------------------------------------------------------
!!       Calculate the effect of temperature on the decomposition rate
!!       parameter. The normalizer is the value of the numerator at 30
!!       degrees Celsius.
!        NORMALIZER = ATANF_C (30.)
!        IF (L == SRFC) THEN
!          TFSOM = ATANF_C (ST(1)) / NORMALIZER
!        ELSE
!          TFSOM = ATANF_C (ST(L)) / NORMALIZER
!        ENDIF
!
!!       Limit TFSOM between >= 0.01 and =< 1.
!        TFSOM = AMAX1 (TFSOM, 0.01)
!        TFSOM = AMIN1 (TFSOM, 1.)

!-------------------------------------------------------------------------
!       Lloyd & Taylor, 1994 equation 11
!       On the Temperature Dependence of Soil Respiration
!       Functional Ecology, Vol. 8, No. 3, (Jun., 1994), pp. 315-323
!       TFSOM = 0.32 * EXP(308.56 * (1/56.02 - 1/(TK-227.13)))
        IF (L < 2) THEN
          TFSOM = 0.32 * EXP(5.51 - 308.56 / (ST(1) + 46.0))
        ELSE
          TFSOM = 0.32 * EXP(5.51 - 308.56 / (ST(L) + 46.0))
        ENDIF

!       Limit TFSOM between >= 0.01 and =< 2.
        TFSOM = AMAX1 (TFSOM, 0.01)
        TFSOM = AMIN1 (TFSOM, 2.)

!-------------------------------------------------------------------------
        SELECT CASE(ISWITCH % ISWWAT)
        CASE ('Y')    !Compute water factor
!         ----------------------------------------------------------------
!         Soil water factor.
!         ----------------------------------------------------------------
          IF (L < 2) THEN   !surface
            SWL = SW(1)
            DULL = DUL(1)
            SATL = SAT(1)
            SWEF = 0.9 - 0.00038 * (DLAYR(1) - 30.) ** 2
            AD = LL(1) * SWEF
          ELSE   !Soil layers
            SWL = SW(L)
            DULL = DUL(L)
            SATL = SAT(L)
            AD  = LL(L)
          ENDIF

          WFSOM  = (SWL - AD) / (DULL - AD)

          IF (SWL > DULL) THEN
            XL = (SWL - DULL) / (SATL - DULL)
            WFSOM = 1.0 - 0.5 * XL
          ENDIF

!         For the SRFC layer, WFSOM cannot go below 0.03226 (if
!         DBLVAL=0). Use that also for the soil layers as minimum, or
!         the strange situation may occur that the minimum WFSOM for the
!         SRFC layer is higher than for layer 1).
          WFSOM = AMAX1 (WFSOM, 0.03226)
          WFSOM = AMIN1 (WFSOM, 1.)

        CASE DEFAULT  !Water not simulated
          WFSOM = 1.0
        END SELECT

!==============================================================================
!       Combine the effects of temperature, moisture and anaerobic
!       conditions.
        DEFAC(L) = TFSOM * WFSOM

!       Limit DEFAC to >= 0 and =< 1.
        DEFAC(L) = AMAX1 (DEFAC(L), 0.)
        DEFAC(L) = AMIN1 (DEFAC(L), 2.)

!     ------------------------------------------------------------------
      RETURN
      END SUBROUTINE DECRAT_C


!!**********************************************************************
!!  ATANF_C, function for CENTURY-based SOM/residue module of DSSAT.
!!
!!  Purpose: This routine is functionally equivalent to the routine of
!!           the same name, described in the publication: "Some graphs
!!           and their functional forms." by William Parton and Georgo.
!!           Innis 1972 - Technical Report no. 153, Natural Resource
!!           Ecology Lab., Colorado State University, Fort Collins, Co.
!!
!!  Revision history:
!!  ........ Parton et al.  Written for CENTURY model.
!!  26/05/99 AJG  Linked to DSSAT.
!!
!!  Called: DECRAT_C
!!  Calls : --
!!**********************************************************************
!
!      REAL FUNCTION ATANF_C (TEMPERATURE)
!
!      REAL PI, TEMPERATURE
!      REAL TEFF(4)
!
!      DATA TEFF /15.7, 11.4, 29.7, 0.0309/
!
!      PI = 3.1415927
!      ATANF_C = TEFF(2) + (TEFF(3) / PI) * ATAN (PI * TEFF(4) *
!     &  (TEMPERATURE - TEFF(1)))
!
!      RETURN
!      END   !Function ATANF_C

!***********************************************************************
! DECRAT_C variables:
!
! A, B        Intermediate parameter (--)
! BASE1       Parameter used for soil-water factor for SOM/residue decomposition (-)
! BASE2       Parameter used for soil-water factor for SOM/residue decomposition (-)
! BD(L)       Bulk density (g[soil] / cm3[soil])
! C, D        Intermediate parameter (-)
! DBLVAL      Parameter used for soil-water factor for SOM/residue decomposition (-)
! DEFAC(L)    Decomposition rate factor (temperature and soil-water effect) (-)
! DLAYR(L)  Soil thickness in layer L (cm)
! E1, E2      Intermediate parameter (-)
! EO          Potential evapotranspiration (mm)
! EOY         Yesterday’s value of the potential evapotranspiration (mm)
! LL(L)       Volumetric soil water content in layer L at lower limit
!               (cm3[water] / cm3[soil])
! NORMALIZER  Temperature factor for SOM/residue decomposition at 30 degrees
!               Celsius (reference value). (-)
! PI          The number Pi (3.1415927) (-)
! SRFC        Identifier for the litter surface layer on top of the soil (SRFC=0)  (-)
! ST(L)       Soil temperature by soil layer (oC)
! SW(L)       Today's Volumetric Soil Water Content in layer L (cm3/cm3)
! SWY(L)      Yesterday's value of the soil water content (cm3[water] / cm3[soil])
! TEFF(I)     Parameters in the calculation of the effect of soil temperature on
!               SOM decomposition (oC)
! TEMPERATURE Soil temperature (oC)
! TFSOM       Temperature factor for SOM/residue decomposition (-)
! WATERFILLED Water-filled porosity. (cm3[water] / cm3[soil pores])
! WFSOM       Water factor for SOM/residue decomposition (-)
! WINF        The amount of water that infiltrates (mm)
! WINFY       Yesterday’s value of the amount of water that infiltrates (mm)
!***********************************************************************
