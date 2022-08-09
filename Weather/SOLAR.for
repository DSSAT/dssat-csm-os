C=======================================================================
C  SOLAR.for contains:
C     SOLAR  - Computes declination, solar time of sunrise and sunset,
C                daily solar constant and cloudiness factor.
C     DAYLEN - Computes solar day length
C     DECL   - Computes solar declination from day length
C=======================================================================

C=======================================================================
C  SOLAR, Subroutine, N.B. Pickering
C  Computes daily solar constant (Spitters, 1986) and cloudiness factor.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  12/02/1990 NBP Written
C  09/23/1993 NBP Separated DAYLEN function and created DECL to compute
C                  declination from DAYL.  Removed DOY effect on S0N.
C  03/15/2000 GH  Incorporated into modular CROPGRO
C-----------------------------------------------------------------------
C  Called by: WEATHR
C  Calls:     None
C=======================================================================

      SUBROUTINE SOLAR(
     &    DAYL, DEC, SRAD, XLAT,                          !Input
     &    CLOUDS, ISINB, S0N)                             !Output

!-----------------------------------------------------------------------
      IMPLICIT NONE
      REAL AMTRCS,AMTRD,CCOS,CLOUDS,DAYL,DEC,DSINB,ISINB,
     &  PI,RAD,SCLEAR,S0D,S0N,SC,SOC,SRAD,SRADJ,SSIN,XLAT
      PARAMETER (AMTRCS=0.77, PI = 3.14159, RAD = PI/180.0, SC=1368.0)

!-----------------------------------------------------------------------
C     Initialization
      SRADJ = SRAD * 1.0E6

C     Sun angles.  SOC limited for latitudes above polar circles.
      SSIN = SIN(RAD*DEC) * SIN(RAD*XLAT)
      CCOS = COS(RAD*DEC) * COS(RAD*XLAT)
      SOC = SSIN / CCOS
      SOC = MIN(MAX(SOC,-1.0),1.0)

C     Integral of SINB over day (sec) (Eqn. 18).
      DSINB = 3600.0 * (DAYL*SSIN + 24.0/PI*CCOS*SQRT(1.0-SOC**2))

C     Set normal (perp.) extra-terrestial radiation equal to
C     average solar constant (elliptical orbit ignored).
      S0N = SC

C     Calculate daily atmospheric transmission from global
C     and normal extra-terrestial radiation.
      S0D = S0N * DSINB
C FO 06/05/2022 Added protection in S0D for division by zero.      
      IF(S0D .GT. 0.0) THEN
        AMTRD = SRADJ / S0D     
      ELSE
        AMTRD = 0.0
      ENDIF

C     Calculate clear sky radiation (0.77*S0D) in MJ.
      SCLEAR = AMTRCS * S0D * 1.E-6

C     Calculate daily cloudiness factor (range = 0-1).
C FO 06/05/2022 Added protection in SCLEAR for division by zero.
      IF(SCLEAR .GT. 0) THEN
        CLOUDS = MIN(MAX(1.0-SRAD/SCLEAR,0.0),1.0)
      ELSE
        CLOUDS = 0.0
      ENDIF

C     Integral in Eqn. 6 (used in HMET)
      ISINB = 3600.0 * (DAYL*(SSIN+0.4*(SSIN**2+0.5*CCOS**2)) +
     &  24.0/PI*CCOS*(1.0+1.5*0.4*SSIN)*SQRT(1.0-SOC**2))

      RETURN
      END SUBROUTINE SOLAR
C=======================================================================
!   SOLAR Variables
!-----------------------------------------------------------------------
! AMTRCS Fraction of radiation which reaches surface under clear sky 
!          conditions 
! AMTRD  Actual fraction of radiation which reaches surface 
! CCOS   Product of cosines of declination and latitude 
! CLOUDS Relative cloudiness factor (0-1) 
! DAYL   Day length on day of simulation (from sunrise to sunset) (hr)
! DEC    Solar declination or (90o - solar elevation at noon). Amplitude = 
!          +/- 23.45. Min = Dec. 21, Max = Jun 21/22 (deg.)
! DSINB  Integral of SINB over day (s)
! ISINB  Integral in Spitter's Equation 6 
! PI     PI=3.14159 (rad)
! RAD    RAD=PI/180. (rad./deg.)
! S0D    Daily extraterrestrial irradiance (MJ/m2-d)
! S0N    Normal extraterrestrial radiation (set equal to average solar 
!          constant; elliptical orbit ignored) (J/m2-s)
! SC     Solar constant (J / m2 - s)
! SCLEAR Clear sky radiation (MJ / m2 - d)
! SOC    Sine over cosine (intermediate calculation) 
! SRAD   Solar radiation (MJ/m2-d)
! SRADJ  Solar Radiation (J/m2-d)
! SSIN   Product of sines of declination and latitude 
! XLAT   Latitude (deg.)
C=======================================================================


C=======================================================================
C  DAYLEN, Real Function, N.B. Pickering, 09/23/1993
C  Computes solar day length (Spitters, 1986).
!-----------------------------------------------------------------------
!  Called by: WEATHR
!  Calls:     None
C=======================================================================

      SUBROUTINE DAYLEN(
     &    DOY, XLAT,                                      !Input
     &    DAYL, DEC, SNDN, SNUP)                          !Output

!-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER DOY
      REAL DEC,DAYL,PI,RAD,SOC,SNDN,SNUP,XLAT
      PARAMETER (PI=3.14159, RAD=PI/180.0)

!-----------------------------------------------------------------------
C     Calculation of declination of sun (Eqn. 16). Amplitude= +/-23.45
C     deg. Minimum = DOY 355 (DEC 21), maximum = DOY 172.5 (JUN 21/22).
      DEC = -23.45 * COS(2.0*PI*(DOY+10.0)/365.0)

C     Sun angles.  SOC limited for latitudes above polar circles.
      SOC = TAN(RAD*DEC) * TAN(RAD*XLAT)
      SOC = MIN(MAX(SOC,-1.0),1.0)

C     Calculate daylength, sunrise and sunset (Eqn. 17)
      DAYL = 12.0 + 24.0*ASIN(SOC)/PI
C FO 06/05/2022 Added protection to avoid day length less than 0.0
C               and greater than 24.0 hours.      
      DAYL = MIN(MAX(DAYL,0.0),24.0)
      SNUP = 12.0 - DAYL/2.0
      SNDN = 12.0 + DAYL/2.0

      RETURN
      END SUBROUTINE DAYLEN

C=======================================================================
! DAYLEN Variables
!-----------------------------------------------------------------------
! DAYL  Day length on day of simulation (from sunrise to sunset)  (hr)
! DEC   Solar declination or (90o - solar elevation at noon) (deg.)
! DOY   Day of year (d)
! PI    PI=3.14159 (rad)
! RAD   RAD=PI/180. (rad./deg.)
! SNDN  Time of sunset (hr)
! SNUP  Time of sunrise (hr)
! SOC   Sine over cosine (intermediate calculation)
! XLAT  Latitude (deg.)
C=======================================================================


C=======================================================================
C  DECL, Real Function, N.B. Pickering, 09/23/1993
C  Computes solar declination from day length (Spitters, 1986).
!-----------------------------------------------------------------------
!  Called by: WTHMOD
!  Calls:     None
!-----------------------------------------------------------------------
C  Input : DAYL,XLAT
C  Output: DECL
C  Local : DL,PI,RAD,SC,XLAT1
C=======================================================================

      REAL FUNCTION DECL(DAYL,XLAT)

!-----------------------------------------------------------------------
      IMPLICIT NONE
      REAL DAYL,DL,PI,RAD,SC,XLAT1,XLAT
      PARAMETER (PI=3.14159, RAD=PI/180.0)

!-----------------------------------------------------------------------
C     Limit value of XLAT and DL to prevent zeros.

      IF (XLAT .GE. 0.0 .AND. XLAT .LT. 0.1) THEN
        XLAT1 = 0.1
        DL = 12.00573
      ELSE IF (XLAT .LT. 0.0 .AND. XLAT .GT. -0.1) THEN
        XLAT1 = -0.1
        DL = 11.99427
      ELSE
        DL = DAYL
        XLAT1 = XLAT
      ENDIF

C     Calculation of declination of sun.

      SC = SIN((DL-12.0)/24.0*PI)
      XLAT1 = TAN(RAD*XLAT1)
      DECL = ATAN(SC/XLAT1) / RAD

      RETURN
      END FUNCTION DECL

C=======================================================================
! DECL Variables
!-----------------------------------------------------------------------
! DAYL   Day length (from sunrise to sunset) (hr)
! DECL   Solar declination function subroutine (deg.)
! DL     Day length (hr)
! PI    PI=3.14159 (rad)
! RAD   RAD=PI/180. (rad./deg.)
! SC     Solar constant (J / m2 - s)
! XLAT1  Latitude (deg.)
! XLAT   Latitude (deg.)
C=======================================================================


C=======================================================================
C  TWILIGHT, Subroutine - from daylength calculations used in Maize and 
C     Rice routines.
C  Computes twilight to twilight day length
C  Civil twilight: the geometric centre of the sun is relative to the horizon, -6 degrees
!-----------------------------------------------------------------------
!  Called by: WEATHR
!  Calls:     None
C=======================================================================

      SUBROUTINE TWILIGHT(DOY, XLAT, TWILEN) 

!-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER DOY
      REAL C1, DEC, DLV, PI, RAD, S1, TWILEN, XLAT
      PARAMETER (PI=3.14159, RAD=PI/180.0)

!-----------------------------------------------------------------------
      !TWILEN is Twilight to Twilight daylength 

      S1 = SIN(XLAT*0.01745)
      C1 = COS(XLAT*0.01745)
      DEC    = 0.4093*SIN(0.0172*(DOY-82.2))
      DLV    = ((-S1*SIN(DEC)-0.1047)/(C1*COS(DEC)))
      DLV    = MIN(MAX(DLV,-0.87), 1.0)
      TWILEN = 7.639*ACOS(DLV)

      RETURN
      END SUBROUTINE TWILIGHT
C=======================================================================
