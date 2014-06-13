!----------------------------------------------------------------------*
! SUBROUTINE SSKYC                                                     *
! Authors: Daniel van Kraalingen                                       *
! Date   : 2-Jun-1993, Version 1.0                                     *
! Purpose: This subroutine estimates solar inclination and fluxes of   *
!          diffuse and direct irradiation at a particular time of      *
!          the day.                                                    *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning                                     units  class *
! ----   ---- -------                                     -----  ----- *
! HOUR    R4  Hour for which calculations should be done     h      I  *
! SOLCON  R4  Solar constant                               W/m2     I  *
! FRPAR   R4  Fraction of total shortwave irradiation        -      I  *
!             that is photosynthetically active (PAR)                  *
! DSINBE  R4  Daily integral of sine of solar height         s      I  *
!             corrected for lower transmission at low                  *
!             elevation                                                *
! SINLD   R4  Intermediate variable from SASTRO              -      I  *
! COSLD   R4  Intermediate variable from SASTRO              -      I  *
! RDD     R4  Daily shortwave radiation                   J/m2/d    I  *
! SINB    R4  Sine of solar inclination at HOUR              -      O  *
! RDPDR   R4  Instantaneous flux of direct photo-          W/m2     O  *
!             synthetically active radiation (PAR)                     *
! RDPDF   R4  Instantaneous flux of diffuse photo-         W/m2     O  *
!             synthetically active irradiation (PAR)                   *
!                                                                      *
! Fatal error checks: RDD <= 0                                         *
! Warnings          : ATMTR > 0.9                                      *
! Subprograms called: FATALERR                                         *
! File usage        : none                                             *
!----------------------------------------------------------------------*

      SUBROUTINE SSKYC (HOUR, SOLCON, FRPAR, DSINBE, SINLD, COSLD, RDD, &
                        SINB, RDPDR , RDPDF)
      IMPLICIT NONE

!     Formal parameters
      REAL HOUR, SOLCON, FRPAR, DSINBE, SINLD, COSLD, RDD
      REAL SINB, RDPDR, RDPDF

!     Local parameters
      REAL ATMTR, FRDIF, SOLHM, TMPR1

!     Hour on day that solar height is at maximum
      PARAMETER (SOLHM=12.)
      SAVE         ! TAOLI

      IF (RDD.LE.0.) CALL FATALERR &
         ('SSKYC','total shortwave irradiation <= zero')

      ATMTR  = 0.
      FRDIF  = 0.
      RDPDF = 0.
      RDPDR = 0.

!     Sine of solar inclination, 0.2617993 is 15 degrees in radians
      SINB = SINLD+COSLD*COS ((HOUR-SOLHM)*0.2617993)

      IF (SINB.GT.0.) THEN
!        Sun is above the horizon

         TMPR1 = RDD*SINB*(1.+0.4*SINB)/DSINBE
         ATMTR = TMPR1/(SOLCON*SINB)

         IF (ATMTR.GT.0.9) WRITE (*,'(A,G12.5,A)') &
             ' WARNING from SSKYC: ATMTR =',ATMTR,', value very large'

         IF (ATMTR.LE.0.22) THEN
            FRDIF = 1.
         ELSE IF (ATMTR.GT.0.22 .AND. ATMTR.LE.0.35) THEN
            FRDIF = 1.-6.4*(ATMTR-0.22)**2
         ELSE
            FRDIF = 1.47-1.66*ATMTR
         END IF

!        Apply lower limit to fraction diffuse
         FRDIF = MAX (FRDIF, 0.15+0.85*(1.-EXP (-0.1/SINB)))

!        Diffuse and direct PAR
         RDPDF = TMPR1*FRPAR*FRDIF
         RDPDR = TMPR1*FRPAR*(1.-FRDIF)

      END IF

      RETURN
      END
