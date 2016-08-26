!----------------------------------------------------------------------*
! SUBROUTINE SRDPRF                                                    *
! Authors: Daniel van Kraalingen                                       *
! Date   : 15-Sept-1994, Version: 1.0                                  *
! Purpose: This subroutine calculates the absorbed flux of radiation   *
!          for shaded leaves, the direct flux absorbed by leaves and   *
!          the fraction of sunlit leaf area.                           *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning                                     units  class *
! ----   ---- -------                                     -----  ----- *
! GAID    R4  Green area index above selected height    ha leaf/       *
!                                                     ha ground I      *
! CSLV    R4  Scattering coefficient of leaves for PAR       -      I  *
! SINB    R4  Sine of solar height                           -      I  *
! ECPDF   R4  Extinction coefficient for          ha ground/ha leaf I  *
!             diffuse light                                            *
! RDPDR   R4  Instantaneous flux of direct photo-          W/m2     I  *
!             synthetically active radiation (PAR)                     *
! RDPDF   R4  Instantaneous flux of diffuse photo-         W/m2     I  *
!             synthetically active irradiation (PAR)                   *
! RAPSHL  R4  Absorbed flux for shaded leaves           W/m2 leaf   O  *
! RAPPPL  R4  Direct flux absorbed by leaves            W/m2 leaf   O  *
!             perpendicular on direct beam                             *
! FSLLA   R4  Fraction of leaf area that is sunlit           -      O  *
!                                                                      *
! Fatal error checks: none                                             *
! Warnings          : none                                             *
! Subprograms called: none                                             *
! File usage        : none                                             *
!----------------------------------------------------------------------*

      SUBROUTINE SRDPRF (GAID  , CSLV  , SINB, ECPDF, RDPDR, RDPDF, &
                         RAPSHL, RAPPPL, FSLLA)
      IMPLICIT NONE

!     Formal parameters
      REAL GAID  , CSLV  , SINB , ECPDF, RDPDR, RDPDF
      REAL RAPSHL, RAPPPL, FSLLA

!     Local parameters
      REAL TMPR1,RFLH,RFLS,ECPBL,ECPTD,RAPDFL,RAPTDL,RAPDDL,CLUSTF
      SAVE         ! TAOLI

!     Reflection of horizontal and spherical leaf angle distribution
      TMPR1 = SQRT (1. - CSLV)
      RFLH  = (1. - TMPR1) / (1. + TMPR1)
      RFLS  = RFLH * 2. / (1. + 2. * SINB)

!     Extinction coefficient for direct radiation and total direct flux
      CLUSTF = ECPDF / (0.8*TMPR1)
      ECPBL  = (0.5/SINB) * CLUSTF
      ECPTD  = ECPBL * TMPR1

!     Absorbed fluxes per unit leaf area: diffuse flux, total direct
!     flux, direct component of direct flux
      RAPDFL = (1.-RFLH) * RDPDF * ECPDF * EXP (-ECPDF * GAID)
      RAPTDL = (1.-RFLS) * RDPDR * ECPTD * EXP (-ECPTD * GAID)
      RAPDDL = (1.-CSLV) * RDPDR * ECPBL * EXP (-ECPBL * GAID)

!     Absorbed flux (J/m2 leaf/s) for shaded leaves
      RAPSHL = RAPDFL + RAPTDL - RAPDDL

!     Direct flux absorbed by leaves perpendicular on direct beam
      RAPPPL = (1.-CSLV) * RDPDR / SINB

!     Fraction sunlit leaf area (FSLLA)
      FSLLA = CLUSTF * EXP (-ECPBL*GAID)

      RETURN
      END
