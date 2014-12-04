!----------------------------------------------------------------------*
! SUBROUTINE SASTRO                                                    *
! Authors: Daniel van Kraalingen                                       *
! Date   : 12-June-1996, Version: 1.1                                  *
! Purpose: This subroutine calculates solar constant, daily            *
!          extraterrestrial radiation, daylength and some intermediate *
!          variables required by other routines. The routine has been  *
!          written such that latitudes from pole to pole can be used.  *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning                                     units  class *
! ----   ---- -------                                     -----  ----- *
! IDOY    I4  Day of year (Jan 1st = 1)                      d      I  *
! LAT     R4  Latitude of the site                       degrees    I  *
! SOLCON  R4  Solar constant at day=IDOY                   W/m2     O  *
! ANGOT   R4  Daily extraterrestrial radiation            J/m2/d    O  *
! DAYL    R4  Astronomical daylength (base = 0 degrees)      h      O  *
! DAYLP   R4  Photoperiodic daylength (base = -4 degrees)    h      O  *
! DSINB   R4  Daily total of sine of solar height            s      O  *
! DSINBE  R4  Daily integral of sine of solar height         s      O  *
!             corrected for lower transmission at low                  *
!             elevation                                                *
! SINLD   R4  Intermediate variable for subroutine SSKYC     -      O  *
! COSLD   R4  Intermediate variable for subroutine SSKYC     -      O  *
!                                                                      *
! Fatal error checks: LAT > 90, LAT < -90                              *
! Warnings          : LAT above polar circle, LAT within polar circle  *
! Subprograms called: FATALERR                                         *
! File usage        : none                                             *
!----------------------------------------------------------------------*

      SUBROUTINE SASTRO (IDOY  , LAT   , &
                         SOLCON, ANGOT , DAYL , DAYLP, &
                         DSINB , DSINBE, SINLD, COSLD)

      IMPLICIT NONE

!     Formal parameters
      REAL LAT, SOLCON, ANGOT, DAYL, DAYLP, DSINB, DSINBE, SINLD, COSLD
      INTEGER IDOY

!     Local parameters
      REAL AOB, PI, DEGTRAD, DOY, DEC, ZZCOS, ZZSIN, ZZA
      SAVE         ! TAOLI

!     PI and conversion factor from degrees to radians
      PARAMETER (PI=3.1415927, DEGTRAD=0.017453292)

!     Error check and conversion of day number
      IF (ABS (LAT).GT.90.) CALL FATALERR &
         ('SASTRO','LAT > 90 or LAT < -90')
      DOY = REAL (IDOY)

!     Declination of the sun as a function of daynumber,
!     calculation of daylength from intermediate variables
!     SINLD, COSLD and AOB

      DEC   = -ASIN (SIN (23.45*DEGTRAD)*COS (2.*PI*(DOY+10.)/365.))
      SINLD = SIN (DEGTRAD*LAT)*SIN (DEC)
      COSLD = COS (DEGTRAD*LAT)*COS (DEC)
      AOB   = SINLD/COSLD

      IF (AOB.LT.-1.) THEN
         WRITE (*,'(2A)') ' WARNING from SASTRO: ', &
                'latitude above polar circle, daylength=0 hours'
         DAYL  = 0.
         ZZCOS = 0.
         ZZSIN = 1.
      ELSE IF (AOB.GT.1.) THEN
         WRITE (*,'(2A)') ' WARNING from SASTRO: ', &
                'latitude within polar circle, daylength=24 hours'
         DAYL  = 24.
         ZZCOS =  0.
         ZZSIN = -1.
      ELSE
         DAYL  = 12.*(1.+2.*ASIN (AOB)/PI)
         DAYLP = 12.0*(1.+2.*ASIN ((-SIN(-4.*DEGTRAD)+SINLD)/COSLD)/PI)
         ZZA   = PI*(12.+DAYL)/24.
         ZZCOS = COS (ZZA)
         ZZSIN = SIN (ZZA)
      END IF

!     Daily integral of sine of solar height (DSINB) with a
!     correction for lower atmospheric transmission at lower solar
!     elevations (DSINBE)

      DSINB  = 2.*3600.*(DAYL*0.5*SINLD-12.*COSLD*ZZCOS/PI)
      DSINBE = 2.*3600.*(DAYL*(0.5*SINLD+0.2*SINLD**2+0.1*COSLD**2)- &
               (12.*COSLD*ZZCOS+9.6*SINLD*COSLD*ZZCOS+ &
               2.4*COSLD**2*ZZCOS*ZZSIN)/PI)

!     Solar constant and daily extraterrestrial radiation
      SOLCON = 1370.*(1.+0.033*COS (2.*PI*DOY/365.))
      ANGOT  = SOLCON*DSINB

      RETURN
      END
