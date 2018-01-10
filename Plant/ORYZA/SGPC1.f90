!----------------------------------------------------------------------*
! SUBROUTINE SGPC1                                                     *
! Authors: Daniel van Kraalingen                                       *
! Date   : 15-Dec-1993, Version: 1.0                                   *
! Purpose: This subroutine performs a Gaussian integration over        *
!          depth of canopy by selecting three different GAI's and      *
!          computing assimilation at these GAI levels. The             *
!          integrated variable is GPC. Also absorbed PAR is            *
!          integrated in RAPC                                          *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning                                     units  class *
! ----   ---- -------                                     -----  ----- *
! CSLV    R4  Scattering coefficient of leaves for           -      I  *
!             visible radiation (PAR)                                  *
! AMAX    R4  Assimilation rate at light saturation       kg CO2/   I  *
!                                                        ha leaf/h     *
! EFF     R4  Initial light use efficiency               kg CO2/J/  I  *
!                                                        ha/h m2 s     *
! ECPDF   R4  Extinction coefficient for          ha ground/ha leaf I  *
!             diffuse light                                            *
! GAI     R4  Green area index                             ha/ha    I  *
! SINB    R4  Sine of solar height                           -      I  *
! RDPDR   R4  Instantaneous flux of direct photo-          W/m2     I  *
!             synthetically active radiation (PAR)                     *
! RDPDF   R4  Instantaneous flux of diffuse photo-         W/m2     I  *
!             synthetically active irradiation (PAR)                   *
! GPC     R4  Instantaneous assimilation rate of          kg CO2/   O  *
!             whole canopy                               ha soil/h     *
! RAPC    R4  Absorbed PAR                                 W/m2     O  *
!                                                                      *
! Fatal error checks: none                                             *
! Warnings          : none                                             *
! Subprograms called: SGPL                                             *
! File usage        : none                                             *
!----------------------------------------------------------------------*

      SUBROUTINE SGPC1 (CSLV , AMAX , EFF, ECPDF, GAI, SINB, &
                        RDPDR, RDPDF, GPC, RAPC)
      IMPLICIT NONE

!     Formal parameters
      REAL CSLV, AMAX, EFF, ECPDF, GAI, SINB, RDPDR, RDPDF, GPC, RAPC

!     Local parameters

!     Gauss parameters
      INTEGER IGSN
      PARAMETER (IGSN=3)
      REAL GSX(IGSN), GSW(IGSN)

!     Miscellaneous
      REAL GAID, GPL, RAPL
      INTEGER I1

      SAVE         ! TAOLI

!     Gauss weights for three point Gauss
      DATA GSX /0.112702, 0.500000, 0.887298/
      DATA GSW /0.277778, 0.444444, 0.277778/

!     Selection of depth of canopy, canopy assimilation is set to zero
      GPC  = 0.
      RAPC = 0.

      DO I1=1,IGSN
         GAID = GAI * GSX(I1)

         CALL SGPL (CSLV , AMAX , EFF , ECPDF, GAI, GAID, SINB, &
                    RDPDR, RDPDF, GPL , RAPL)

!        Integration of local assimilation rate to canopy
!        assimilation (GPC) and absorption of PAR by canopy (RAPC)
         GPC  = GPC  + GPL  * GSW(I1)
         RAPC = RAPC + RAPL * GSW(I1)

      END DO

      GPC  = GPC  * GAI
      RAPC = RAPC * GAI

      RETURN
      END
