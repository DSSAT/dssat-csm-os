!----------------------------------------------------------------------*
! SUBROUTINE SGPC2                                                     *
! Authors: Daniel van Kraalingen                                       *
! Date   : 15-Dec-1993, Version: 1.1                                   *
! Purpose: Iterative procedure to calculate whole canopy assimilation  *
!          using internal error control. Routine can be used in place  *
!          of SGPC1 for greater accuracy. The method used is a combi-  *
!          nation of the TRAPZD and QSIMP routines from Numerical      *
!          Recipes (Press et al., 1990, page 111-113)                  *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning                                     units  class *
! ----   ---- -------                                     -----  ----- *
! CSLV    R4  Scattering coefficient of leaves for          -       I  *
!             visible radiation (PAR)                                  *
! AMAX    R4  Assimilation rate at light saturation      kg CO2/    I  *
!                                                       ha leaf/h      *
! EFF     R4  Initial light use efficiency            kg CO2/J/ha/  I  *
!                                                       (h/m2/s)       *
! ECPDF   R4  Extinction coefficient for          ha ground/ha leaf I  *
!             diffuse light                                            *
! GAI     R4  Green area index                            ha/ha     I  *
! SINB    R4  Sine of solar height                          -       I  *
! RDPDR   R4  Instantaneous flux of direct photo-          W/m2     I  *
!             synthetically active radiation (PAR)                     *
! RDPDF   R4  Instantaneous flux of diffuse photo-         W/m2     I  *
!             synthetically active irradiation (PAR)                   *
! GPC     R4  Instantaneous assimilation rate of         kg CO2/    O  *
!             whole canopy                              ha soil/h      *
! RAPC    R4  Absorbed radiation                          W/m2      O  *
!                                                                      *
! Fatal error checks: N > NMAX (6)                                     *
! Warnings          : none                                             *
! Subprograms called: SGPL, FATALERR                                   *
! File usage        : none                                             *
!----------------------------------------------------------------------*

      SUBROUTINE SGPC2 (CSLV, AMAX, EFF, ECPDF, GAI, SINB, RDPDR, RDPDF, &
                        GPC , RAPC)
      IMPLICIT NONE

!     Formal parameters
      REAL CSLV, AMAX, EFF, ECPDF, GAI, SINB, RDPDR, RDPDF, GPC, RAPC

!     Local variables

      REAL EPS, GPCTO, GPCT, RAPCT, RAPCTO, SUM1, SUM2, GPCO, RAPCO
      REAL DEL, TNM, X, GPL1, GPL2, RAPL1, RAPL2
      INTEGER NMAX, N, J, IT

      PARAMETER (EPS=0.10, NMAX=7)

      SAVE         ! TAOLI

      GPC   = 0.
      GPCO  = 1.
      GPCT  = 0.

      RAPC  = 0.
      RAPCO = 1.
      RAPCT = 0.

      N = 1

      DO WHILE ((ABS (GPC-GPCO).GT.EPS*ABS (GPCO).OR.&
           ABS (RAPC-RAPCO).GT.EPS*ABS (RAPCO)))

         GPCO   = GPC
         GPCTO  = GPCT

         RAPCO  = RAPC
         RAPCTO = RAPCT

         IF (N.EQ.1) THEN
            CALL SGPL (CSLV, AMAX, EFF,ECPDF, GAI, 0., SINB, &
                       RDPDR, RDPDF, GPL1 , RAPL1)
            CALL SGPL (CSLV, AMAX, EFF,ECPDF, GAI, GAI, SINB, &
                       RDPDR, RDPDF, GPL2 , RAPL2)

!           First approximation of total gross photosynthesis of canopy
            GPCT  = 0.5*GAI*(GPL1 +GPL2)

!           First approximation of absorption of PAR by canopy
            RAPCT = 0.5*GAI*(RAPL1+RAPL2)

         ELSE
            IT   = 2**(N-2)
            TNM  = IT
            DEL  = GAI/TNM
            X    = 0.5*DEL
            SUM1 = 0.
            SUM2 = 0.
            DO J=1,IT
              CALL SGPL (CSLV, AMAX, EFF,ECPDF, GAI, X, SINB, &
                         RDPDR, RDPDF, GPL2 , RAPL2)
              SUM1 = SUM1+GPL2
              SUM2 = SUM2+RAPL2
              X    = X+DEL
            END DO

!           Higher order approximation of total gross photosynthesis
!           of canopy
            GPCT  = 0.5*(GPCT +GAI*SUM1/TNM)

!           Higher order approximation of absorption of PAR by
!           canopy
            RAPCT = 0.5*(RAPCT+GAI*SUM2/TNM)

         END IF

         GPC  = (4.*GPCT -GPCTO)/3.
         RAPC = (4.*RAPCT-RAPCTO)/3.

         N = N+1
         IF (N.GT.NMAX) CALL FATALERR ('SGPC2','too many steps')
      END DO

      RETURN
      END
