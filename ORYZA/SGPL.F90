!----------------------------------------------------------------------*
! SUBROUTINE SGPL                                                      *
! Authors: Daniel van Kraalingen                                       *
! Date   : 15-Sept-1994, Version: 1.0                                  *
! Purpose: This subroutine calculates assimilation at a single depth   *
!          in the canopy                                               *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning                                     units  class *
! ----   ---- -------                                     -----  ----- *
! CSLV    R4  Scattering coefficient of leaves for visible   -      I  *
!             radiation (PAR)                                          *
! AMAX1   R4  Assimilation rate at light saturation       kg CO2/   I  *
!                                                        ha leaf/h     *
! EFF1    R4  Initial light use efficiency               kg CO2/J/  I  *
!                                                       (ha/h/m2/s)    *
! ECPDF   R4  Extinction coefficient for diffuse light              I  *
! GAI     R4  Total green area                             ha/ha    I  *
! GAID    R4  Green area index above selected height       ha/ha    I  *
! SINB    R4  Sine of solar height                           -      I  *
! RDPDR   R4  Instantaneous flux of direct photo-          W/m2     O  *
!             synthetically active radiation (PAR)                     *
! RDPDF   R4  Instantaneous flux of diffuse photo-         W/m2     O  *
!             synthetically active irradiation (PAR)                   *
! GPL     R4  Instantaneous assimilation rate of          kg CO2/   O  *
!             leaves at depth GAI                        ha leaf/h     *
! RAPL    R4  Absorbed radiation at depth GAI            W/m2 leaf  O  *
!                                                                      *
! Fatal error checks: none                                             *
! Warnings          : none                                             *
! Subprograms called: SRDPRF                                           *
! File usage        : none                                             *
!----------------------------------------------------------------------*

      SUBROUTINE SGPL (CSLV , AMAX1, EFF1 ,ECPDF, GAI, GAID, SINB, &
                       RDPDR, RDPDF, GPL  , RAPL)
      IMPLICIT NONE

!     Formal parameters

      REAL CSLV,AMAX1,EFF1,ECPDF,GAI,GAID,SINB,RDPDR,RDPDF

!     Local parameters
      REAL AMAX2, EFF2

!     Gauss parameters
      INTEGER IGSN
      PARAMETER (IGSN=3)
      REAL GSX(IGSN), GSW(IGSN)

!     Miscellaneous
      REAL RAPSHL, RAPPPL, RAPSLL, FSLLA
      REAL GPSHL , GPSLL , TMPR1 , GPL  ,RAPL
      INTEGER I1

      SAVE         ! TAOLI

!     Gauss weights for three point Gauss
      DATA GSX /0.112702, 0.500000, 0.887298/
      DATA GSW /0.277778, 0.444444, 0.277778/

!     Selection of depth of canopy, canopy assimilation is set to zero

      CALL SRDPRF (GAID,CSLV,SINB,ECPDF,RDPDR,RDPDF, &
                   RAPSHL,RAPPPL,FSLLA)

!     Get photosynthesis parameters from user routine
      CALL GPPARGET (GAI,GAID,AMAX1,EFF1,AMAX2,EFF2)

!     Assimilation of shaded leaf area
      IF (AMAX2.GT.0.) THEN
         GPSHL = AMAX2 * (1.-EXP (-RAPSHL*EFF2/AMAX2))
      ELSE
         GPSHL = 0.
      END IF

!     Assimilation of sunlit leaf area

      GPSLL  = 0.
      RAPSLL = 0.
      DO I1=1,IGSN
         TMPR1 = RAPSHL + RAPPPL * GSX(I1)
         IF (AMAX2.GT.0.) THEN
            GPSLL = GPSLL + AMAX2 * (1.-EXP(-TMPR1*EFF2/AMAX2)) * GSW(I1)
         ELSE
            GPSLL = 0.
         END IF
         RAPSLL = RAPSLL + TMPR1 * GSW(I1)
      END DO

!     Local assimilation rate (GPL) and rate of
!     absorption of PAR by canopy (RAPL)
      GPL  = FSLLA * GPSLL  + (1.-FSLLA) * GPSHL
      RAPL = FSLLA * RAPSLL + (1.-FSLLA) * RAPSHL

      RETURN
      END
