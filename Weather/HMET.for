C=================================================================
C  HMET.for contains:
C     HMET  - Calculates hourly values of temperature, PAR, solar
C             radiation, fraction diffuse radiation, and relative
C             humidity.
C     HANG  - Computes solar elevation and azimuth
C     HTEMP - Computes temperature for hour-of-day
C     HRAD  - Computes total hourly solar radiation
C     FRACD - Computes hourly fraction diffuse total radiation and PAR.
C     HPAR  - Computes hourly PAR
C     VPSAT - Calculates saturated vapor pressure of air
C     VPSLOP- Calculates slope of saturated vapor pressure versus
C             temperature curve
C=================================================================

C=================================================================
C  HMET, Subroutine, N.B. Pickering
C  Calculates hourly values of temperature, PAR, solar radiation,
C  fraction diffuse radiation, and relative humidity,
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  12/02/1990 NBP Written
C  05/28/1993 NBP Modified temperature and radiation functions
C  10/23/1993 NBP Modified FRACD function
C  11/23/1993 NBP Limit on BETA to 1 deg, if positive
C  09/04/1996 GH  Fixed ASIN problems
C  09/02/1999 GH  Incorporated into CROPGRO
C  01/10/2000 NBP Moved growth temperature here from ETPG
C  01/29/2000 NBP Added HWIND 
C  03/15/2000 GH  Modular version revisited
C  02/10/2006 JIL New calculation of fraction of diffuse radiation
C  02/13/2006 JIL Export AMTRH (R/R0) for leaf rolling calculation
C  10/02/2007 JIL New calculation of PARHR
C-----------------------------------------------------------------------
c  Called by: WEATHR
C  Calls:     HANG, HTEMP, HRAD, FRACD, HPAR
C=======================================================================

      SUBROUTINE HMET(
     &    CLOUDS, DAYL, DEC, ISINB, PAR, REFHT,           !Input
     &    SNDN, SNUP, S0N, SRAD, TDEW, TMAX,              !Input
     &    TMIN, WINDHT, WINDSP, XLAT,                     !Input
     &    AMTRH, AZZON, BETA, FRDIFP, FRDIFR, PARHR,      !Output
     &    RADHR, RHUMHR, TAIRHR, TAVG, TDAY, TGRO,        !Output
     &    TGROAV, TGRODY, WINDHR)                         !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
!     TS defined in ModuleDefs.for
      IMPLICIT NONE
      EXTERNAL HANG, HTEMP, VPSAT, HWIND, HRAD, FRACD, HPAR
      INTEGER H,NDAY

      REAL, DIMENSION(TS) :: AMTRH, AZZON, BETA, FRDIFP, FRDIFR, PARHR
      REAL, DIMENSION(TS) :: RADHR, RHUMHR, TAIRHR, TGRO, WINDHR

      REAL CLOUDS, DAYL, DEC,
     &  HS,ISINB,PAR,REFHT,S0N,SRAD,SNDN,SNUP,
     &  TAVG,TDAY,TDEW,TGROAV,TGRODY,TINCR,TMAX,TMIN,
     &  RH,VPSAT,WINDAV,WINDHT,WINDSP,
     &  XLAT
      PARAMETER (TINCR=24./TS)

!-----------------------------------------------------------------------
C     Initialize
      TAVG = 0.0
      TDAY = 0.0
      NDAY = 0
      WINDAV = WINDSP / 86.4 * (REFHT/WINDHT)**0.2

C     Loop to compute hourly weather data.
      DO H = 1,TS

C       Calculate real and solar time.
        HS = REAL(H) * TINCR

C       Calculate sun angles and hourly weather variables.
        CALL HANG(
     &    DEC, HS, XLAT,                                  !Input
     &    AZZON(H), BETA(H))                              !Output

        CALL HTEMP(
     &    DAYL, HS, SNDN, SNUP, TMAX, TMIN,               !Input
     &    TAIRHR(H))                                      !Output

        RH = VPSAT(TDEW) / VPSAT(TAIRHR(H)) * 100.0
        RHUMHR(H) = MIN(RH,100.0)

        CALL HWIND(
     &    DAYL, HS, SNDN, SNUP, WINDAV,                   !Input
     &    WINDHR(H))                                      !Output

        CALL HRAD(
     &    BETA(H), HS, ISINB, SNDN, SNUP, SRAD,           !Input
     &    RADHR(H))                                       !Output

        CALL FRACD(
     &    BETA(H), CLOUDS, HS, RADHR(H), S0N, SNDN, SNUP, !Input
     &    AMTRH(H), FRDIFP(H), FRDIFR(H))                 !Output

        CALL HPAR(
     &    HS, PAR, RADHR(H), SNDN, SNUP, SRAD,            !Input
     &    PARHR(H))                                       !Output

        TAVG = TAVG + TAIRHR(H)

        IF (H .GE. SNUP .AND. H .LE. SNDN) THEN
          TDAY = TDAY + TAIRHR(H)
          NDAY = NDAY + 1
        ENDIF

      ENDDO
      
      TAVG = TAVG / REAL(TS)
      TDAY = TDAY / REAL(NDAY)

      TGRODY = TDAY
      TGROAV = TAVG
      DO H=1,TS
        TGRO(H) = TAIRHR(H)
      ENDDO

      IF (PAR .LE. 0.) THEN
        PAR = 2.0 * SRAD
      ENDIF

      RETURN
      END SUBROUTINE HMET

C=======================================================================
!     HMET variable definitions
!-----------------------------------------------------------------------
! AZZON(TS)  Hourly solar azimuth (+/- from South) (deg.)
! BETA(TS)   Hourly solar elevation (+/- from horizontal) (deg.)
! CLOUDS     Relative cloudiness factor (0-1) 
! DAYL       Day length on day of simulation (from sunrise to sunset) (hr)
! DEC        Solar declination or (90o - solar elevation at noon). 
!              Amplitude = +/- 23.45. Min = Dec. 21, Max = Jun 21/22 (deg.)
! FRDIFP(TS) Hourly fraction diffuse photon flux density after correcting 
!              for circumsolar radiation (Spitters, 1986) 
! FRDIFR(TS) Hourly fraction diffuse solar radiation after correcting for 
!              circumsolar radiation (Spitters, 1986) 
! H          Internal hourly counter (hr)
! HS         Internal hourly counter (hour)
! ISINB      Integral in Spitter's Equation 6 
! NDAY       Number of daylight hours (hr)
! PAR        Daily photosynthetically active radiation or photon flux 
!              density (moles[quanta]/m2-d)
! PARHR(TS)  hourly PAR (J / m2 - s)
! RADHR(TS)  Total hourly solar radiation (J/m2-s)
! REFHT      Reference height for wind speed (m)
! RH         Relative humidity (%)
! RHUMHR(TS) Relative humidity hourly value (%)
! S0N        Normal extraterrestrial radiation (set equal to average solar 
!              constant; elliptical orbit ignored) (J/m2-s)
! SNDN       Time of sunset (hr)
! SNUP       Time of sunrise (hr)
! SRAD       Solar radiation (MJ/m2-d)
! TAIRHR(TS) Hourly air temperature (in some routines called TGRO) (°C)
! TAVG       Average daily temperature (°C)
! TDAY       Average temperature during daylight hours (°C)
! TDEW       Dewpoint temperature (°C)
! TGRO(I)    Hourly air temperature (°C)
! TGROAV     Average daily canopy temperature (°C)
! TGRODY     Average temperature during daylight hours (°C)
! TINCR      Time increment (hr)
! TMAX       Maximum daily temperature (°C)
! TMIN       Minimum daily temperature (°C)
! TS         Number of intermediate time steps (=24) 
! VPSAT      Saturated vapor pressure of air (Pa)
! WINDAV     Average wind speed (m/s)
! WINDHR(TS) Hourly wind speed (m/s)
! WINDHT     Reference height for wind speed (m)
! WINDSP     Wind speed (km/d)
! XLAT       Latitude (deg.)
C=======================================================================



C=======================================================================
C  HANG, Subroutine, N.B. Pickering, 12/02/90
C  Computes solar elevation and azimuth for time-of-day (deg).
C-----------------------------------------------------------------------
C  12/02/90 NBP Written
C  08/16/96 GH  Put limits on BETAS to avoid ASIN problems
!  01/29/99 chp modified for modular input
!-----------------------------------------------------------------------
!  Called by: HMET
!  Calls:     None
!-----------------------------------------------------------------------
C  Input : HS,DEC,XLAT
C  Output: AZZON,BETA
C  Local : PI,RAD,SINAS,SINB,SOLTIM
C=======================================================================

      SUBROUTINE HANG(
     &    DEC, HS, XLAT,                                  !Input
     &    AZZON, BETA)                                    !Output

!-----------------------------------------------------------------------
      IMPLICIT NONE

      REAL AZZON,BETA,CCOS,HANGL,HS,DEC,PI,RAD,SINAS,SSIN,XLAT,BETAS
      PARAMETER (PI=3.14159, RAD=PI/180.)

!-----------------------------------------------------------------------
C     Sines and cosines and their products.

      SSIN = SIN(RAD*DEC) * SIN(RAD*XLAT)
      CCOS = COS(RAD*DEC) * COS(RAD*XLAT)
      HANGL = (HS-12.0)*PI/12.0

C     Calculate solar elevation and azimuth.
      BETAS = MIN(MAX(SSIN + CCOS*COS(HANGL),-1.0),1.0)
      BETA  = ASIN(BETAS)
      SINAS = COS(RAD*DEC) * SIN(HANGL) / COS(BETA)
      SINAS = MIN(MAX(SINAS,-1.0),1.0)
      AZZON = ASIN(SINAS)
      AZZON = AZZON / RAD
      BETA = BETA / RAD

      IF (BETA .GT. 1.E-4) THEN
        BETA = MAX(BETA, 1.0)
      ENDIF

      RETURN
      END !SUBROUTINE HANG

C=======================================================================
! Subroutine HANG Variables
!-----------------------------------------------------------------------
! AZZON(TS) Hourly solar azimuth (+/- from South) (deg.)
! BETA(TS)  Hourly solar elevation (+/- from horizontal) (deg.)
! BETAS     Sine of hourly solar elevation (Equation 15, Spitters) 
! CCOS      Product of cosines of declination and latitude 
! DEC       Solar declination or (90o - solar elevation at noon). Amplitude 
!             = +/- 23.45. Min = Dec. 21, Max = Jun 21/22 (deg.)
! HANGL     Hour angle or hourly time step as a proportion of day (*PI)
!             (rad.)
! HS        Internal hourly counter (hour)
! SINAS     Sine of AZZON 
! SSIN      Product of sines of declination and latitude 
! XLAT      Latitude (deg.)
C=======================================================================



C=======================================================================
C  HTEMP, Subroutine, N.B. Pickering, 09/16/91.
C  Computes temperature for hour-of-day (Parton and Logan, 1981).
!-----------------------------------------------------------------------
!  Called by: HMET
!  Calls:     None
!-----------------------------------------------------------------------
C  Notes : Only today's TMAX,TMIN,SNUP,SNDN are used.
C          A=0.0,B=1.0e-6,C=2.0 gives the same as WCALC.
C=======================================================================

      SUBROUTINE HTEMP(
     &    DAYL, HS, SNDN, SNUP, TMAX, TMIN,               !Input
     &    TAIRHR)                                         !Output

!-----------------------------------------------------------------------
      IMPLICIT NONE
      REAL A,ARG,B,C,DAYL,HDECAY,HS,MAX,MIN,PI,SNDN,SNUP,T,
     &  TMAX,TMIN,TMINI,TAIRHR,TSNDN
      PARAMETER (A=2.0, B=2.2, C=1.0, PI=3.14159)    ! Parton and Logan

!-----------------------------------------------------------------------
C     Calculate day length, time of maximum temperature,time of
C     minimum temperature, hours of exponential decay, temperature
C     at sunset, and temperature at infinite time.

      MIN = SNUP + C
      MAX = MIN + DAYL/2.0 + A
      T = 0.5 * PI * (SNDN-MIN) / (MAX-MIN)
      TSNDN = TMIN + (TMAX-TMIN)*SIN(T)
      TMINI = (TMIN-TSNDN*EXP(-B)) / (1.0-EXP(-B))
      HDECAY = 24.0 + C - DAYL

C     Daylight hours--sine curve.  Minimum temperature at (SNUP+C) and
C     maximum temperature at (noon+A+C) hours.

      IF (HS .GE. SNUP+C .AND. HS .LE. SNDN) THEN
        T = 0.5 * PI * (HS-MIN) / (MAX-MIN)
        TAIRHR = TMIN + (TMAX-TMIN)*SIN(T)

C     Nighttime--exponential decrease from sunset temperature (TSNDN)
C     to minimum (TMINI) at infinite time and TMIN at (SNUP+C).

      ELSE
        IF (HS .LT. SNUP+C) THEN
          T = 24.0 + HS - SNDN
        ELSE IF (HS .GT. SNDN) THEN
          T = HS - SNDN
        ENDIF
        ARG = -B * T / HDECAY
        TAIRHR = TMINI + (TSNDN-TMINI)*EXP(ARG)
      ENDIF

      RETURN
      END SUBROUTINE HTEMP
C=======================================================================
! HTEMP Variables
!-----------------------------------------------------------------------
! A          Lag time for calculation time of maximum temperature (Parton 
!              and Logan) (hr)
! ARG        Intermediate variable in calculation of hourly temperature 
! B          Exponential decay factor (Parton and Logan) 
! C          Lag time between time of sunrise and time of minimum 
!              temperature (Parton and Logan) (hr)
! DAYL       Day length on day of simulation (from sunrise to sunset) (hr)
! HDECAY     Time for hourly temperature exponential decay from sunset to 
!              sunrise + C (hr)
! HS         Internal hourly counter (hour)
! MAX        Time of maximum temperature (hr)
! MIN        Time of minimum temperature (hr)
! SNDN       Time of sunset (hr)
! SNUP       Time of sunrise (hr)
! T          Time factor for hourly calculations 
! TAIRHR(TS) Hourly air temperature (in some routines called TGRO) (°C)
! TMAX       Maximum daily temperature (°C)
! TMIN       Minimum daily temperature (°C)
! TMINI      Theoretical temperature at infinite time used for exponential 
!              temperature decay after sunset (°C)
! TSNDN      Temperature at sundown (°C)
C=======================================================================



C=======================================================================
C  HRAD, Subroutine, N.B. Pickering, 03/19/91
C  Computes total hourly solar radiation (Spitters, 1986).
!-----------------------------------------------------------------------
!  Called by: HMET
!  Calls:     None
C=======================================================================

      SUBROUTINE HRAD(
     &    BETA, HS, ISINB, SNDN, SNUP, SRAD,              !Input
     &    RADHR)                                          !Output

!-----------------------------------------------------------------------
      IMPLICIT NONE
      REAL BETA,HS,ISINB,PI,RAD,RADHR,SINB,SNDN,SNUP,SRAD
      PARAMETER (PI=3.14159, RAD=PI/180.0)

!-----------------------------------------------------------------------
C     Daylight hour calculations
      IF (HS .GT. SNUP .AND. HS .LT. SNDN) THEN

C       Initialization
        SINB = SIN(RAD*BETA)

C       Compute instantaneous SRAD values with Spitters' Eqn. 6.
        RADHR = SINB * (1.0+0.4*SINB) * SRAD*1.0E6 / ISINB

      ELSE
        RADHR = 0.0
      ENDIF

      RETURN
      END SUBROUTINE HRAD
C=======================================================================
! HRAD Variables
!-----------------------------------------------------------------------
! BETA(TS)  Hourly solar elevation (+/- from horizontal) (deg.)
! HS        Internal hourly counter (hour)
! ISINB     Integral in Spitter's Equation 6 
! PI        PI=3.14159 (rad)
! RAD       RAD=PI/180. (rad./deg.)
! RADHR(TS) Total hourly solar radiation (J/m2-s)
! SINB      Sine of Beta (hourly solar elevation) 
! SNDN      Time of sunset (hr)
! SNUP      Time of sunrise (hr)
! SRAD      Solar radiation (MJ/m2-d)
C=======================================================================



C=======================================================================
C  FRACD, Subroutine, N.B. Pickering, 03/19/91
C  Computes hourly fraction diffuse total radiation and PAR.
!-----------------------------------------------------------------------
!  Called by: HMET
!  Calls:     None
C=======================================================================

      SUBROUTINE FRACD(
     &    BETA, CLOUDS, HS, RADHR, S0N, SNDN, SNUP,       !Input
     &    AMTRH, FRDIFP, FRDIFR)                          !Output

!-----------------------------------------------------------------------
      IMPLICIT NONE
      REAL AMTRH,BETA,CLOUDS,CORR,COS90B,COSB,FRDFH,FRDIFP,
     &  FRDIFR,HS,PI,RAD,RADHR,S0,S0N,SDF,SINB,SNDN,SNUP
      PARAMETER (PI = 3.14159, RAD = PI/180.0)

!-----------------------------------------------------------------------
C     Daylight hour calculations
      IF (HS .GT. SNUP .AND. HS .LT. SNDN) THEN

C       Initialization
        SINB = SIN(RAD*BETA)
        COSB = COS(RAD*BETA)
        COS90B = COS(RAD*(90.0-BETA))

C       Calculate instantaneous atmospheric transmission from global
C       and extra-terrestial radiation.

        S0 = S0N * SINB
        AMTRH = RADHR / S0

C       Calculate hourly fraction diffuse from hourly atmospheric
C       transmission (after Erbs, Klein and Duffie, 1982; De Jong,
C       1980).  Lower end adjusted to give smooth transition.

!        IF (AMTRH .LE. 0.22) THEN
!          FRDFH = 1.0
!        ELSE IF (AMTRH .GT. 0.22 .AND. AMTRH .LE. 0.5) THEN
!          FRDFH = 1.0 - 4.3*(AMTRH-0.22)**2
!        ELSE
!          FRDFH = 0.1 + 0.563*EXP(-5.5*(AMTRH-0.5))
!        ENDIF

! JIL 09/11/2007 Using single logistic equation instead of discontinous function
! FO  06/05/2022 Added protection for overflow in the EXP() function.
!                Overflow happens when AMTRH is greater than 64.
!                This is possible for extreme latitudes that has
!                low day length periods. IF AMTRH is greater than 5.0
!                FRDFH will be always equal to 0.156. AMTRH lower than
!                5.0, it will reach a maximum of 1.016.
          IF(AMTRH .LT. 5.0) THEN
            FRDFH = 0.156 + (0.86/(1.0+EXP(11.1*(AMTRH - 0.53))))
            FRDFH = AMIN1 (FRDFH,1.0)
          ELSE
            FRDFH = 0.156
          ENDIF

C       Calculate instantaneous diffuse global irradiance.
        SDF = RADHR * FRDFH

C       Decrease sdf by circumsolar part of diffuse radiation.
!         Spitters Eqn. 9
        CORR =  (1.0-CLOUDS) * COS90B**2 * COSB**3
        SDF = SDF / (1.0 + CORR)

C       Calculate intantaneous fraction diffuse of global and par.
        IF (RADHR .GT. 0.0) THEN
          FRDIFR = SDF / RADHR
        ELSE
          FRDIFR = 0.0
        ENDIF

!     Spitters Eqn. 10.
        FRDIFP = (1.0+0.3*(1.0-CLOUDS)) * FRDIFR
        FRDIFR = MIN(MAX(FRDIFR,0.0),1.0)
        FRDIFP = MIN(MAX(FRDIFP,0.0),1.0)

      ELSE
        FRDIFP = 1.0
        FRDIFR = 1.0
      ENDIF

      RETURN
      END SUBROUTINE FRACD
C=======================================================================
! FRACD Variables
!-----------------------------------------------------------------------
! AMTRH      Hourly atmospheric transmission coefficient or ratio of solar: 
!              extraterrestrial radiation 
! BETA(TS)   Hourly solar elevation (+/- from horizontal) (deg.)
! CLOUDS     Relative cloudiness factor (0-1) 
! CORR       Correction to decrease diffuse solar radiation by the 
!              circumsolar component, since circumsolar diffuse radiation 
!              acts like direct radiation (Spitters, 1986) 
! COS90B     Cosine of (90° - BETA) 
! COSB       Cosine of BETA 
! FRDFH      Hourly fraction diffuse radiation before correcting for 
!              circumsolar radiation (Spitters, 1986) 
! FRDIFP(TS) Hourly fraction diffuse photon flux density after correcting 
!              for circumsolar radiation (Spitters, 1986) 
! FRDIFR(TS) Hourly fraction diffuse solar radiation after correcting for 
!              circumsolar radiation (Spitters, 1986) 
! HS         Internal hourly counter (hour)
! PI         PI=3.14159 (rad)
! RAD        RAD=PI/180. (rad./deg.)
! RADHR(TS)  Total hourly solar radiation (J/m2-s)
! S0         Extraterrestrial irradiance on a plant parallel to earth 
!              surface (J / m2-s)
! S0N        Normal extraterrestrial radiation (set equal to average solar 
!              constant; elliptical orbit ignored) (J/m2-s)
! SDF        Instantaneous diffuse global irradiance (J / m2 - s)
! SINB       Sine of Beta (hourly solar elevation) 
! SNDN       Time of sunset (hr)
! SNUP       Time of sunrise (hr)
C=======================================================================



C=======================================================================
C  HPAR, Subroutine, N.B. Pickering, 03/19/91
C  Computes hourly PAR using Spitters (1986) or Efimova (1967)
C  in Pearcy et al (1991).
!-----------------------------------------------------------------------
!  Called by: HMET
!  Calls:     None
!-----------------------------------------------------------------------
C  Input : BETA,FRDIFR,HS,ISINB,PAR,SNDN,SNUP,RADHR
C  Output: PARHR
C  Local : CONST,DAYL,PARFAC,PARQC,PI,RAD,SINB,SOLTIM
C=======================================================================

      SUBROUTINE HPAR(
     &    HS, PAR, RADHR, SNDN, SNUP, SRAD,               !Input
     &    PARHR)                                          !Output

!-----------------------------------------------------------------------
      IMPLICIT NONE
      REAL HS,PAR,PARFAC,PARHR,PARQC,RADHR,SNDN,SNUP,SRAD
      PARAMETER (PARQC=4.6)
!-----------------------------------------------------------------------
C     Daylight hour calculations
      IF (HS .GT. SNUP .AND. HS .LT. SNDN) THEN

C       Compute instantaneous PAR using same equation as HRAD.  If PAR not
C       available, compute from RADHR and FRDIFR.  Uses PARQC=4.6 photon/J
C       and Eqn. 6.2 in Pearcy et al (1991).  PARFAC=2.1-2.6 (sunny-cloudy).
C  10/02/2007 JIL     For this to work units should be:
C             SRAD (MJ/m2 d)
C             PAR (mol/m2 d)
C             RADHR (J/m2 s)
C             PARHR (umol/m2 s)
C             PARQC = 4.6 umol/J

        IF (PAR .GT. 1.E-4) THEN
          PARHR = RADHR * PAR/SRAD
        ELSE
C         PARFAC = (0.43*(1.0-FRDIFR)+0.57*FRDIFR) * PARQC
C          PARFAC = 2.0

C  10/02/2007 JIL   According to Lizaso et al. (2003)
          PARFAC = (0.43 + 0.12*EXP(-SRAD/2.8)) * PARQC
          PARHR = RADHR * PARFAC
        ENDIF

C     Night time.
      ELSE
        PARHR = 0.0
      ENDIF

      RETURN
      END SUBROUTINE HPAR

C=======================================================================
! HPAR Variables
!-----------------------------------------------------------------------
! HS        Internal hourly counter (hour)
! PAR       Daily photosynthetically active radiation or photon flux 
!           density, calculated as half the solar radiation 
!           (moles[quanta]/m2-d)
! PARFAC    Factor used to compute hourly PAR from hourly solar radiation
! PARHR(TS) hourly PAR (J / m2 - s) Units should be umol/m2 s ....JIL
! RADHR(TS) Total hourly solar radiation (J/m2-s)
! SNDN      Time of sunset (hr)
! SNUP      Time of sunrise  (hr)
! SRAD      Solar Radiation  (MJ/m2-d)
C=======================================================================


C=======================================================================
C  HWIND, Subroutine, N.B. Pickering, 01/27/00.
C  Computes wind speed for hour-of-day using speed at night
C  (10% of max.) plus full sine in the day (90% of max.).
!-----------------------------------------------------------------------
!  Called by: HMET
!  Calls:     None
C=======================================================================

      SUBROUTINE HWIND(
     &    DAYL, HS, SNDN, SNUP, WINDAV,                   !Input
     &    WINDHR)                                         !Output

!-----------------------------------------------------------------------
      IMPLICIT NONE
      REAL DAYL,HS,PI,SNDN,SNUP,T,WNIGHT,WINDAV,WINDHR,WMAX
      PARAMETER (PI = 3.14159 )
!-----------------------------------------------------------------------

C     Set base wind speed as proportion of maximum daily wind speed then
C     compute the maximum wind speed from the average daily wind speed.

      WNIGHT = 0.3
      WMAX = WINDAV / (WNIGHT+(1.0-WNIGHT)*DAYL/48.0)

C     Day time.
      IF (HS.GT.SNUP .AND. HS.LT.SNDN) THEN
        T = 2.0*PI*(HS-SNUP)/(SNDN-SNUP) + 1.5*PI
        WINDHR = WMAX*WNIGHT + (1.0-WNIGHT) * WMAX/2.0 * (1.0+SIN(T))

C     Night time.
      ELSE
        WINDHR = WMAX*WNIGHT
      ENDIF

      RETURN
      END SUBROUTINE HWIND
C=======================================================================
! HWIND Variables
!-----------------------------------------------------------------------
! DAYL   Day length on day of simulation (from sunrise to sunset) (hr)
! HS     Internal hourly counter (hour)
! SNDN   Time of sunset (hr)
! SNUP   Time of sunrise  (hr)
! T      Time factor for hourly calculations
! WNIGHT Night time wind speed set at 10% of WMAX (m/s)
! WMAX   Maximum wind speed (m/s)
! WINDAV Average wind speed (m/s)
! WINDHR Hourly wind speed (m/s)
! WINDHT Reference height for windspeed (m)
! WINDSP Windspeed (km/d)
C=======================================================================


C=======================================================================
C  VPSAT, Real Function, N.B. Pickering, 4/1/90
C  Calculates saturated vapor pressure of air (Tetens, 1930).
!-----------------------------------------------------------------------
!  Called by: CANPET, HMET, VPSLOP, PETPEN
!  Calls:     None
!-----------------------------------------------------------------------
C  Input : T (C)
C  Output: VPSAT (Pa)
C=======================================================================

      REAL FUNCTION VPSAT(T)

      IMPLICIT NONE
      REAL T

      VPSAT = 610.78 * EXP(17.269*T/(T+237.30))

      RETURN
      END FUNCTION VPSAT
C=======================================================================
! VPSAT Variables
!-----------------------------------------------------------------------
! T     Air temperature (oC)
! VPSAT Saturated vapor pressure of air (Pa)
C=======================================================================



C=======================================================================
C  VPSLOP, Real Function, N.B. Pickering, 4/1/90
C  Calculates slope of saturated vapor pressure versus temperature curve
C  using Classius-Clapeyron equation (see Brutsaert, 1982 p. 41)
!-----------------------------------------------------------------------
!  Called by: ETSOLV, PETPEN, TRATIO
!  Calls:     VPSAT
!-----------------------------------------------------------------------
C  Input : T (C)
C  Output: VPSLOP 
C=======================================================================

      REAL FUNCTION VPSLOP(T)

      IMPLICIT NONE
      EXTERNAL VPSAT

      REAL T,VPSAT

C     dEsat/dTempKel = MolWeightH2O * LatHeatH2O * Esat / (Rgas * TempKel^2)

      VPSLOP = 18.0 * (2501.0-2.373*T) * VPSAT(T) / (8.314*(T+273.0)**2)

      RETURN
      END FUNCTION VPSLOP
C=======================================================================
! VPSLOP variables
!-----------------------------------------------------------------------
! T      Air temperature (oC)
! VPSAT  Saturated vapor pressure of air (Pa)
! VPSLOP Slope of saturated vapor pressure versus temperature curve
C=======================================================================


C=======================================================================
C  TDEW, Real Function, Matthew Jones, 2007-04-05
C  Calculates dew point temperature
!-----------------------------------------------------------------------
!  Called by: IPWEATHR
!-----------------------------------------------------------------------
C  Input : Tmin, Tmax (C), RHUM (%)
C  Output: TDEW
C=======================================================================

      REAL FUNCTION CALC_TDEW(TMIN, RHUM)

      IMPLICIT NONE

c     Interface variables
      REAL TMIN, RHUM
c     Local variables
      REAL A, B, C, Td, ea, es


c     Campbell and Norman method:
c     Taken from Campbell, G and Norman, J (1998), 'An Introduction to 
c     Environmental Physics', Springer-Verlag New York, pp42-44
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     Assuming that RHUM is maximum humidity, which occurs at the same
c     time as minimum temperature was recorded (TMIN)

c     C & N:
c      a = 0.611
c     From DSSAT / CANEGRO
      a = 0.61078

c     C & N:
c      b = 17.502
c     From DSSAT / CANEGRO
      b = 17.269

c     C & N:
c      c = 240.97
c     From DSSAT / CANEGRO
      c = 237.3

      es = a * EXP(b * TMIN / (TMIN + c))
      ea = RHUM/100. * es
      Td = (c * LOG(ea/a)) / (b - LOG(ea/a))
      CALC_TDEW = Td

c     Wikipedia method (amounts to the same thing):
C       [from http://en.wikipedia.org/wiki/Dew_point]
c     These parameter values taken from VPSAT calc.
c      A    = 17.269
c      B    = 237.3


c     CALC_TDEW = (B * ((A * TMIN / (B + TMIN)) + LOG(RHUM/100.) ) ) / 
c     &            (A - ((A * TMIN / (B + TMIN)) + LOG(RHUM/100.) ) )

      RETURN
      END FUNCTION CALC_TDEW
C=======================================================================
! TDEW variables
!-----------------------------------------------------------------------
! TMIN        Min daily temperature (oC)
! TMAX        Max daily temperature (oC)
! CALC_TDEW   Dew point temperature (oC)
! RHUM        Relative humidity (%)
! ea          Ambient vapour pressure (kPa)
! es          Saturated vapour pressure (kPa)
! a           Air pressure (kPa)
! b           ??? some sort of empirical constant
! c           ??? (oC)
C=======================================================================
