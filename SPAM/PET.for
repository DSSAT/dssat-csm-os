!***********************************************************************
!  POTENTIAL EVAPOTRANSPIRATION
!     File PET.for
!***********************************************************************
!  Includes subroutines:

!  MEEVP Routine Description
!   S  PETASCE ASCE Standardized Reference Evapotranspiration Equation
!                for the short reference crop (12-cm grass) with dual
!                FAO-56 crop coefficient method (potential E and T
!                calculated independently).
!   T  PETASCE ASCE Standardized Reference Evapotranspiration Equation
!                for the tall reference crop (50-cm alfalfa) with dual
!                FAO-56 crop coefficient method (potential E and T
!                calculated independently).
!   R  PETPT   Calculates Priestley-Taylor potential evapotranspiration
!                (default method with potential E and T partitioned as a
!                function of LAI).
!   F  PETPEN  FAO Penman-Monteith (FAO-56) reference evapotranspiration
!                with EORATIO adjustment for CROPGRO models and KC = 1.0
!                for non-CROPGRO models (potential E and T partioned as
!                a function of LAI).
!   D  PETDYN  Dynamic Penman-Monteith, pot. evapotranspiration, with
!                dynamic input of LAI, crop height effects on Ra and Rs
!   P  PETPNO  FAO Penman (FAO-24) potential evapotranspiration
!   M  PETMEY  "Standard reference evaporation calculation for inland
!                south eastern Australia" By Wayne Meyer 1993
!   H  PETPTH Calculates Priestly-Taylor potential evapotranspiration
!             using hourly temperature and radiation. Also includes a VPD
!             effect to the transpiration

!  Also includes these subroutines:
!      PSE        Potential soil evaporation
!      FLOOD_EVAP Evaporation from water surface
C=======================================================================


C=======================================================================
!     SUBROUTINE PET
!     Calls appropriate potential evapotranspiration routine

      SUBROUTINE PET(CONTROL,
     &      ET_ALB, XHLAI, MEEVP, WEATHER,  !Input for all
     &      EORATIO, !Needed by Penman-Monteith
     &      CANHT,   !Needed by dynamic Penman-Monteith
     &      EO,      !Output
     &      ET0)     !Output hourly Priestly-Taylor with VPD effect

      USE ModuleDefs
      IMPLICIT NONE
      EXTERNAL YR_DOY, PETPT, PETPEN, PETASCE, PETDYN, PETPNO, PETMEY, 
     &  PETPTH, WARNING, ERROR
      SAVE

      TYPE (WeatherType) WEATHER
      TYPE (ControlType) CONTROL
      CHARACTER*1 MEEVP
      INTEGER YRDOY, YEAR, DOY
      REAL CANHT, CLOUDS, EO, EORATIO, ET_ALB, RHUM, SRAD, TAVG
      REAL TDEW, TMAX, TMIN, VAPR, WINDHT, WINDSP, XHLAI
      REAL WINDRUN, XLAT, XELEV
      REAL, DIMENSION(TS)    ::RADHR, TAIRHR, ET0
      LOGICAL NOTDEW, NOWIND
      CHARACTER*78  MSG(2)
      CHARACTER*12 FILEX
      CHARACTER*6, PARAMETER :: ERRKEY = "PET   "

      CLOUDS = WEATHER % CLOUDS
      SRAD   = WEATHER % SRAD
      NOTDEW = WEATHER % NOTDEW
      NOWIND = WEATHER % NOWIND
      RHUM   = WEATHER % RHUM
      TAVG   = WEATHER % TAVG
      TDEW   = WEATHER % TDEW
      TMAX   = WEATHER % TMAX
      TMIN   = WEATHER % TMIN
      VAPR   = WEATHER % VAPR
      WINDHT = WEATHER % WINDHT
      WINDSP = WEATHER % WINDSP
      WINDRUN= WEATHER % WINDRUN
      XLAT   = WEATHER % XLAT
      XELEV  = WEATHER % XELEV
      RADHR  = WEATHER % RADHR
      TAIRHR = WEATHER % TAIRHR

      YRDOY = CONTROL % YRDOY
      FILEX = CONTROL % FILEX
      CALL YR_DOY(YRDOY, YEAR, DOY)

      SELECT CASE (MEEVP)
!         ------------------------
          !Priestley-Taylor potential evapotranspiration
          CASE ('R')
            CALL PETPT(
     &        ET_ALB, SRAD, TMAX, TMIN, XHLAI,          !Input
     &        EO)                                       !Output
!         ------------------------
          !FAO Penman-Monteith (FAO-56) potential evapotranspiration,
!             with KC = 1.0
          CASE ('F')
            CALL PETPEN(
     &        CLOUDS, EORATIO, ET_ALB, SRAD, TAVG, TDEW,  !Input
     &        TMAX, TMIN, VAPR, WINDSP, WINDHT, XHLAI,    !Input
     &        EO)                                         !Output
!         ------------------------
          !ASCE Standardized Reference Evapotranspiration Equation
          !for the short reference crop (12-cm grass, "S") or the
          !tall reference crop (50-cm grass, "T") with dual
          !FAO-56 crop coefficient method.
          CASE ('S','T')
            CALL PETASCE(
     &        CANHT, DOY, ET_ALB, MEEVP, NOTDEW, NOWIND,  !Input
     &        RHUM, SRAD, TDEW, TMAX, TMIN, WINDHT,       !Input
     &        WINDRUN, VAPR, XHLAI, XLAT, XELEV,          !Input
     &        EO)                                         !Output
!         ------------------------
          !Dynamic Penman-Monteith, pot. evapotranspiration, with
!             dynamic input of LAI, crop height effects on Ra and Rs
          CASE ('D')
            CALL PETDYN(
     &        CANHT, CLOUDS, ET_ALB, SRAD, TAVG, TDEW,    !Input
     &        TMAX, TMIN, WINDSP, XHLAI,                  !Input
     &        EO)                                         !Output
!         ------------------------
          !FAO Penman (FAO-24) potential evapotranspiration
          CASE ('P')
            CALL PETPNO(
     &        CLOUDS, ET_ALB, SRAD, TAVG, TDEW,           !Input
     &        TMAX, TMIN, WINDSP, XHLAI,                  !Input
     &        EO)                                         !Output
!         ------------------------
          !Penman - Meyer routine for estimation of Et in Southern NSW
          CASE ('M')
            CALL PETMEY(CONTROL,
     &        TAVG, WINDSP, SRAD, TDEW, XHLAI, ET_ALB,    !Input
     &        EO)                                         !Output
!         ------------------------
          !Observed Potential ET from Weather file (Future)
          !CASE ('O')
          !    EO = EOMEAS
!         ------------------------
          !Priestley-Taylor potential evapotranspiration hourly
          !including a VPD effect on transpiration
          CASE ('H')
              CALL PETPTH(
     &        ET_ALB, TMAX, XHLAI, RADHR, TAIRHR,       !Input
     &        EO, ET0)                                  !Output
!         ------------------------
          CASE DEFAULT
              MSG(1) = "Undefined EVAPO parameter in FileX."
              MSG(2) = "Unknown MEEVP in PET.for."
              CALL WARNING(2,ERRKEY,MSG)
              CALL ERROR(ERRKEY,1,FILEX,0)
!         ------------------------
      END SELECT

      RETURN
      END SUBROUTINE PET

C=======================================================================

C=======================================================================
C  PETASCE, Subroutine, K. R. Thorp
C  Calculates reference evapotranspiration for the short or tall
C  reference crops using the ASCE Standardized Reference
C  Evapotranspiration Equation.
C  Adjusts reference evapotranspiration to potential soil water
C  evaporation and potential transpiration using FAO-56 dual crop
C  coefficients, following FAO-56 (Allen et al., 1998) and the
C  ASCE (2005) standardized reference ET algorithm.
C  DeJonge K. C., Thorp, K. R., 2017. Implementing standardized
C  reference evapotranspiration and dual crop coefficient approach
C  in the DSSAT Cropping System Model. Transactions of the ASABE.
C  60(6):1965-1981.
C  ASCE Task Committee on Standardization of Reference
C  Evapotranspiration (Walter, I. A., Allen, R. G., Elliott, R.,
C  Itenfisu, D., Brown, P., Jensen, M. E., Mecham, B., Howell, T. A.,
C  Snyder, R., Eching, S., Spofford, T., Hattendorf, M., Martin, D.,
C  Cuenca, R. H., Wright, J. L.), 2005. The ASCE Standardized Reference
C  Evapotranspiration Equation. American Society of Civil Engineers,
C  Reston, VA.
C  Allen, R. G., Pereira, L. S., Raes, D., Smith, M., 1998.  FAO
C  Irrigation and Drainage Paper No. 56. Crop Evapotranspiration:
C  Guidelines for Computing Crop Water Requirements. Food and
C  Agriculture Organization of the United Nations, Rome Italy.
!-----------------------------------------------------------------------
C  REVISION HISTORY
C  08/19/2013 KRT Added the ASCE Standardize Reference ET approach
C  01/26/2015 KRT Added the dual crop coefficient (Kc) approach
C  01/18/2018 KRT Merged ASCE dual Kc ET method into develop branch
C  07/23/2020 KRT Changed flags to S and T for short and tall references
!-----------------------------------------------------------------------
!  Called from:   PET
!  Calls:         None
C=======================================================================
      SUBROUTINE PETASCE(
     &        CANHT, DOY, MSALB, MEEVP, NOTDEW, NOWIND,   !Input
     &        RHUM, SRAD, TDEW, TMAX, TMIN, WINDHT,       !Input
     &        WINDRUN, VAPR, XHLAI, XLAT, XELEV,          !Input
     &        EO)                                         !Output
!-----------------------------------------------------------------------
      USE ModuleDefs
      USE ModuleData
      IMPLICIT NONE
      SAVE
!-----------------------------------------------------------------------
!     INPUT VARIABLES:
      REAL CANHT, MSALB, RHUM, SRAD, TDEW, TMAX, TMIN
      REAL VAPR, WINDHT, WINDRUN
      REAL XHLAI, XLAT, XELEV
      INTEGER DOY
      LOGICAL NOTDEW, NOWIND
      CHARACTER*1 MEEVP
!-----------------------------------------------------------------------
!     OUTPUT VARIABLES:
      REAL EO
!-----------------------------------------------------------------------
!     LOCAL VARIABLES:
      REAL TAVG, PATM, PSYCON, UDELTA, EMAX, EMIN, ES, EA, FC, FEW, FW
      REAL ALBEDO, RNS, PIE, DR, LDELTA, WS, RA1, RA2, RA, RSO, RATIO
      REAL FCD, TK4, RNL, RN, G, WINDSP, WIND2m, Cn, Cd, KCMAX, RHMIN
      REAL WND, CHT
      REAL REFET, SKC, KCBMIN, KCBMAX, KCB, KE, KC
!-----------------------------------------------------------------------

!     ASCE Standardized Reference Evapotranspiration
!     Average temperature, ASCE (2005) Eq. 2
      TAVG = (TMAX + TMIN) / 2.0 !deg C

!     Atmospheric pressure, ASCE (2005) Eq. 3
      PATM = 101.3 * ((293.0 - 0.0065 * XELEV)/293.0) ** 5.26 !kPa

!     Psychrometric constant, ASCE (2005) Eq. 4
      PSYCON = 0.000665 * PATM !kPa/deg C

!     Slope of the saturation vapor pressure-temperature curve
!     ASCE (2005) Eq. 5                                    !kPa/degC
      UDELTA = 2503.0*EXP(17.27*TAVG/(TAVG+237.3))/(TAVG+237.3)**2.0

!     Saturation vapor pressure, ASCE (2005) Eqs. 6 and 7
      EMAX = 0.6108*EXP((17.27*TMAX)/(TMAX+237.3)) !kPa
      EMIN = 0.6108*EXP((17.27*TMIN)/(TMIN+237.3)) !kPa
      ES = (EMAX + EMIN) / 2.0                     !kPa

!     Actual vapor pressure, ASCE (2005) Table 3
      IF (VAPR.GT.1.E-6) THEN
        EA = VAPR !kPa
      ELSEIF (.NOT.NOTDEW) THEN
!       ASCE (2005) Eq. 8
        EA = 0.6108*EXP((17.27*TDEW)/(TDEW+237.3)) !kPa
      ELSEIF (RHUM.GT.1.E-6) THEN
!       RHUM is relative humidity at TMIN (or max rel. hum) (%)
!       ASCE (2005) Eq. 12
        EA = EMIN * RHUM / 100. !kPa
      ELSE
!       ASCE (2005) Appendix E, assume TDEW=TMIN-2.0
        EA = 0.6108*EXP((17.27*(TMIN-2.0))/((TMIN-2.0)+237.3)) !kPa
      ENDIF

!     RHmin, ASCE (2005) Eq. 13, RHmin limits from FAO-56 Eq. 70
      RHMIN = MAX(20.0, MIN(80.0, EA/EMAX*100.0))

!     Net shortwave radiation, ASCE (2005) Eq. 16
      IF (XHLAI .LE. 0.0) THEN
        ALBEDO = MSALB
      ELSE
        ALBEDO = 0.23
      ENDIF
      RNS = (1.0-ALBEDO)*SRAD !MJ/m2/d

!     Extraterrestrial radiation, ASCE (2005) Eqs. 21,23,24,27
      PIE = 3.14159265359
      DR = 1.0+0.033*COS(2.0*PIE/365.0*DOY) !Eq. 23
      LDELTA = 0.409*SIN(2.0*PIE/365.0*DOY-1.39) !Eq. 24
      WS = ACOS(-1.0*TAN(XLAT*PIE/180.0)*TAN(LDELTA)) !Eq. 27
      RA1 = WS*SIN(XLAT*PIE/180.0)*SIN(LDELTA) !Eq. 21
      RA2 = COS(XLAT*PIE/180.0)*COS(LDELTA)*SIN(WS) !Eq. 21
      RA = 24.0/PIE*4.92*DR*(RA1+RA2) !MJ/m2/d Eq. 21

!     Clear sky solar radiation, ASCE (2005) Eq. 19
      RSO = (0.75+2E-5*XELEV)*RA !MJ/m2/d

!     Net longwave radiation, ASCE (2005) Eqs. 17 and 18
      RATIO = SRAD/RSO
      IF (RATIO .LT. 0.3) THEN
        RATIO = 0.3
      ELSEIF (RATIO .GT. 1.0) THEN
        RATIO = 1.0
      END IF
      FCD = 1.35*RATIO-0.35 !Eq 18
      TK4 = ((TMAX+273.16)**4.0+(TMIN+273.16)**4.0)/2.0 !Eq. 17
      RNL = 4.901E-9*FCD*(0.34-0.14*SQRT(EA))*TK4 !MJ/m2/d Eq. 17

!     Net radiation, ASCE (2005) Eq. 15
      RN = RNS - RNL !MJ/m2/d

!     Soil heat flux, ASCE (2005) Eq. 30
      G = 0.0 !MJ/m2/d

!     Wind speed, ASCE (2005) Eq. 33 and Appendix E
      IF (NOWIND) THEN
        WIND2m = 2.0 !m/s
      ELSE
        WINDSP = WINDRUN * 1000.0 / 24.0 / 60.0 / 60.0 !m/s
        WIND2m = WINDSP * (4.87/LOG(67.8*WINDHT-5.42))
      ENDIF

!     Aerodynamic roughness and surface resistance daily timestep constants
!     ASCE (2005) Table 1
      SELECT CASE(MEEVP) !
        CASE('S') !Short reference crop (12-cm grass)
          Cn = 900.0 !K mm s^3 Mg^-1 d^-1
          Cd = 0.34 !s m^-1
        CASE('T') !Tall reference crop (50-cm alfalfa)
          Cn = 1600.0 !K mm s^3 Mg^-1 d^-1
          Cd = 0.38 !s m^-1
      END SELECT

!     Standardized reference evapotranspiration, ASCE (2005) Eq. 1
      REFET =0.408*UDELTA*(RN-G)+PSYCON*(Cn/(TAVG+273.0))*WIND2m*(ES-EA)
      REFET = REFET/(UDELTA+PSYCON*(1.0+Cd*WIND2m)) !mm/d
      REFET = MAX(0.0001, REFET)

!     FAO-56 dual crop coefficient approach
!     First step is to obtain crop coefficient parameters.
      CALL GET('SPAM', 'SKC', SKC)
      KCBMIN = 0.0
      CALL GET('SPAM', 'KCBMAX', KCBMAX)

!     Basal crop coefficient (Kcb)
!     Also similar to FAO-56 Eq. 97
!     KCB is zero when LAI is zero by hard coding KCBMIN = 0.0.
      IF (XHLAI .LE. 0.0) THEN
         KCB = 0.0
      ELSE
         !Equation from DeJonge et al. (2012) Agricultural Water
         !Management 115, 92-103 and revised in DeJonge and Thorp (2017)
         KCB = MAX(0.0,KCBMIN+(KCBMAX-KCBMIN)*(1.0-EXP(-1.0*SKC*XHLAI)))
      ENDIF

      !Maximum crop coefficient (Kcmax) (FAO-56 Eq. 72)
      WND = MAX(1.0,MIN(WIND2m,6.0))
      CHT = MAX(0.001,CANHT)
      SELECT CASE(MEEVP)
        CASE('S') !Short reference crop (12-cm grass)
            KCMAX = MAX((1.2+(0.04*(WND-2.0)-0.004*(RHMIN-45.0))
     &                      *(CHT/3.0)**(0.3)),KCB+0.05)
        CASE('T') !Tall reference crop (50-cm alfalfa)
            KCMAX = MAX(1.0,KCB+0.05)
      END SELECT

      !Effective canopy cover (fc) (FAO-56 Eq. 76)
      IF (KCB .LE. KCBMIN) THEN
         FC = 0.0
      ELSE
         FC = ((KCB-KCBMIN)/(KCMAX-KCBMIN))**(1.0+0.5*CANHT)
      ENDIF

      !Exposed and wetted soil fraction (FAO-56 Eq. 75)
      !Wetted soil fraction (FW) is hard-coded to 1.0.
      !FW should not be used to adjust demand.
      !Rather wetting fraction should be addressed on supply side.
      !Difficult to do with a 1-D soil water model, but 2-D models
      !offer opportunity for this.
      FW = 1.0
      FEW = MIN(1.0-FC,FW)

      !Potential evaporation coefficient (Ke) (Based on FAO-56 Eq. 71)
      !Kr = 1.0 since this is for potential E. Other model routines
      !handle reductions from potential.
      KE = MAX(0.0, MIN(1.0*(KCMAX-KCB), FEW*KCMAX))

      !Potential crop coefficient (Kc) (FAO-56 Eqs. 58 & 69)
      KC = KCB + KE

      !Potential evapotranspiration (FAO-56 Eq. 69)
      EO = (KCB + KE) * REFET

      EO = MAX(EO,0.0001)

      CALL PUT('SPAM', 'REFET', REFET)
      CALL PUT('SPAM', 'KCB', KCB)
      CALL PUT('SPAM', 'KE', KE)
      CALL PUT('SPAM', 'KC', KC)

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE PETASCE
!-------------------------------------------------------------------

C=======================================================================

C=======================================================================
C  PETPEN, Subroutine, N.B. Pickering
C  Calculates FAO-56 Penman-Monteith potential evapotranspiration, exactly
C  grass reference, with place for optional Kc, need this Kc in species.
!-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/19/1992 NBP Written
C  11/04/1993 NBP Renamed routine PET to PETPEN.  Changed XLAI to XHLAI
C               Added XHLAI .LE. 0.0 statement.
C  05/13/1994 NBP Converted all vapor pressures to Pa.  Rearranged.
C  09/16/1994 NBP Added limits to prevent EO and ES (PE) < 0.
C  10/17/1997 CHP Updated for modular format.
C  09/01/1999 GH  Incorporated into CROPGRO
C  05/06/2002 WMB Fixed Stefan-Boltzmann constant
C  01/15/2003 KJB discarded old Penman FAO-24 (it is much too stressful)
C     replaced with Penman FAO-56, exactly grass reference, with explicit
C     LAI 2.88, height 0.12 m, Rs = 100/(0.5*2.88)
C  01/15/2003 KJB:  THREE ISSUES TO ADDRESS:
C  1) USING FIXED ALBEDO, BECAUSE THAT IS HOW REFERENCE IS DEFINED
C  2)  PRESENTLY USING A LOCKED-IN VALUE OF 1.1 TO GIVE KC OF 1.1
C  I WOULD LIKE TO SEE OPTION OF SPECIES INPUT OF KC=1.1 TO 1.3
C  3) WINDHT WAS IN OLD, APPARENTLY 2.0, NO LONGER HERE.  ???
C  02/06/2003 KJB/CHP Added EORATIO as input from plant routines.
!  07/24/2006 CHP Use MSALB instead of SALB (includes mulch and soil
!                 water effects on albedo)
!  09/19/2006 SSJ Fixed error in REFHT calc as noted below.
!  08/25/2011 CHP Use measured vapor pressure (VAPR), if available
!-----------------------------------------------------------------------
!  Called from:   PET
!  Calls:         None
C=======================================================================
      SUBROUTINE PETPEN(
     &    CLOUDS, EORATIO, MSALB, SRAD, TAVG, TDEW,       !Input
     &    TMAX, TMIN, VAPR, WINDSP, WINDHT, XHLAI,        !Input
     &    EO)                                             !Output
!-----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL VPSAT, VPSLOP
      SAVE
!-----------------------------------------------------------------------
!     INPUT VARIABLES:
      REAL CLOUDS, EORATIO, MSALB, SRAD, TAVG, TDEW, TMAX, TMIN,
     &        WINDSP, XHLAI, WINDSP_M
!-----------------------------------------------------------------------
!     OUTPUT VARIABLES:
      REAL EO
!-----------------------------------------------------------------------
!     LOCAL VARIABLES:
      REAL ALBEDO, EAIR, ESAT, G, LHVAP, PSYCON, RADB,
     &  RNET, RNETMG, S, TK4,
     &  VHCAIR, VPD, DAIR, RT, ET0, KC, WINDHT, VAPR
      REAL SHAIR, PATM, SBZCON
      REAL k, d, REFHT, Zom, Zoh, ra, rl, rs    !added for PenMon
!      REAL alt_RADB, Tprev
!      INTEGER THREEDAYCOUNT
!      REAL    THREEDAYAVG(3)

!     PARAMETER (WINDHT = 2.0)
C     PARAMETER (SHAIR = 1005.0)
      PARAMETER (SHAIR = 0.001005)  !changed for PenMon to MJ/kg/K
      PARAMETER (PATM = 101300.0)
!      PARAMETER (SBZCON=4.093E-9)  !(MJ/m2/d)
      PARAMETER (SBZCON=4.903E-9)   !(MJ/K4/m2/d) fixed constant 5/6/02
!-----------------------------------------------------------------------
!     FUNCTION SUBROUTINES:
      REAL VPSLOP, VPSAT      !Found in file HMET.for

C-----------------------------------------------------------------------
C     Compute air properties.
      LHVAP = (2501.0-2.373*TAVG) * 1000.0                ! J/kg
C     PSYCON = SHAIR * PATM / (0.622*LHVAP)               ! Pa/K
      PSYCON = SHAIR * PATM / (0.622*LHVAP) * 1000000     ! Pa/K

!     Previous code:
      ESAT = (VPSAT(TMAX)+VPSAT(TMIN)) / 2.0              ! Pa
      EAIR = VPSAT(TDEW)                                  ! Pa

!     If actual vapor pressure is available, use it.
      IF (VAPR > 1.E-6) THEN
        EAIR = VAPR * 1000.
      ENDIF

      VPD = MAX(0.0, ESAT - EAIR)                         ! Pa
      S = (VPSLOP(TMAX)+VPSLOP(TMIN)) / 2.0               ! Pa/K
      RT = 8.314 * (TAVG + 273.0)                         ! N.m/mol
      DAIR = 0.028966*(PATM-0.387*EAIR)/RT                ! kg/m3
C BAD DAIR = 0.1 * 18.0 / RT * ((PATM  -EAIR)/0.622 + EAIR)   ! kg/m3
      VHCAIR = DAIR * SHAIR    !not used                  ! J/m3

C     Convert windspeed to 2 m reference height.
!     Do this conversion in WEATHR and send out 2m windspeed
!     CHP 11/26/01
!      WIND2 = WINDSP * (2.0/WINDHT)**0.2

C       Calculate aerodynamic resistance (ra).
C       ra (d/m) = {ln[zm-d/zom]*ln[zh-d/zoh]}/(k^2*uz)
C       zm = ht.wind measurement (m), zh = ht.humidity measurement (m),
C       zom,zoh=rooughness length of momentum, heat and vapor x-fer (m)
C       k=von Karman's constant 0.41, uz=WINDSP @ z m/d,
C       d = zero plane displacement height (m)

        REFHT = 0.12                 !arbitrary for testing PenMon
        WINDSP_M = WINDSP*(1000.)     !Converts km/d to m/d
        k = 0.41                     !von Karman's constant

!       was 2/3, which (for integers) results in zero!!
!       SSJ 9/19/2006 added the decimals
        !d = (2/3)*REFHT
        d = (2./3.)*REFHT

        Zom = 0.123*REFHT
        Zoh = 0.1*Zom
        ra = (LOG((WINDHT-d)/Zom)*LOG((WINDHT-d)/Zoh))/((k**2)*WINDSP_M)

C       Calculate surface resistance (rs).
C       rs = rl/LAIactive       rs (s m^-1),
C       rl = bulk stomatal resistance of the well-illuminated leaf (s m^-1)

        rl = 100           !value assummed from FAO grass reference
        rs = rl/(0.5*2.88) !0.5*XHLAI assumes half of LA is contributing
C                          !  to heat/vapor transfer
        rs = rs/86400      !converts (s m^-1 to d/m)

C     Calculate net radiation (MJ/m2/d).  By FAO method 1990. EAIR is divided
C       by 1000 to convert Pa to KPa.

c     MJ, 2007-04-11
c     --------------
c     There appears to be no support for soil heat flux (G), apart
c     from the variable already existing; it is just always set to
c     0, for some reason.
c     Here is the (improved) CANEGRO method for calculating G
c     (Allen, R.G. et al 1989,
c     'Operational Estimates of Reference Evapotranspiration',
c     Agronomy Journal Vol. 81, No. 4),
c     http://www.kimberly.uidaho.edu/water/papers/evapotranspiration/
c                   Allen_Operational_Estimates_Reference_ET_1989.pdf
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c         3-day sum of average temperature:
c          IF (THREEDAYCOUNT .LT. 1) THEN
c             Initialise
c              THREEDAYCOUNT = 1
c              THREEDAYAVG   = Tavg
c          ELSE IF (THREEDAYCOUNT .GE. 3) THEN
c              THREEDAYCOUNT = 1
c          ELSE
c              THREEDAYCOUNT = THREEDAYCOUNT + 1
c          ENDIF
c          THREEDAYAVG(THREEDAYCOUNT) = Tavg
c          Tprev = SUM(THREEDAYAVG)/3.
c          G = (Tavg-Tprev) * 0.38
c     --------------
c     MJ, 2007-04-12:
c     :::::::::::::::
c     FAO suggests that G be set to 0.  Oh well.
c     ------------------------------------------

      G = 0.0
      IF (XHLAI .LE. 0.0) THEN
        ALBEDO = MSALB
      ELSE
C  KJB NOTE THAT REFERENCE IS ALWAYS ALBEDO FIXED TO 0.23,  OLD PEN VARIED
C  THE ALBEDO WITH LAI.  WHAT DO WE WANT?  IS THIS PART OF THE REASON THAT
C  KC IS NEEDED WITH THE REFERENCE FORMULATION?
C       ALBEDO = 0.23-(0.23-SALB)*EXP(-0.75*XHLAI)
        ALBEDO = 0.23
      ENDIF

      TK4 = ((TMAX+273.)**4+(TMIN+273.)**4) / 2.0
C
C     BELOW WAS THE OLD PENMAN, DIFFERENT CLOUDS METHOD, EAIR CHG IS GOOD
C     RADB = SBZCON * TK4 * (0.4 - 0.005 * SQRT(EAIR)) *
C    &        (1.1 * (1. - CLOUDS) - 0.1)
C

      RADB = SBZCON * TK4 * (0.34 - 0.14 * SQRT(EAIR/1000)) *
     &        (1.35 * (1. - CLOUDS) - 0.35)

      RNET= (1.0-ALBEDO)*SRAD - RADB

C     Compute EO using Penman-Montieth

      RNETMG = (RNET-G)
C     !MJ/m2/d
        ET0 = ((S*RNETMG + (DAIR*SHAIR*VPD)/ra)/(S+PSYCON*(1+rs/ra)))
C     !Converts MJ/m2/d to mm/d
        ET0 = ET0/ (LHVAP / 1000000.)
        IF (XHLAI .LE. 6.0) THEN
        XHLAI = XHLAI
        ELSE
        XHLAI = 6.0
        ENDIF
C   KJB LATER, NEED TO PUT VARIABLE IN PLACE OF 1.1
!      KC=1.0+(1.1-1.0)*XHLAI/6.0
      KC=1.0+(EORATIO-1.0)*XHLAI/6.0
      EO=ET0*KC
C     EO=ET0
        EO = MAX(EO,0.0)
!###  EO = MAX(EO,0.0)   !gives error in DECRAT_C
      EO = MAX(EO,0.0001)

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE PETPEN

!-----------------------------------------------------------------------
!     PETPEN VARIABLES:
!-----------------------------------------------------------------------
! ALBEDO  Reflectance of soil-crop surface (fraction)
! CLOUDS  Relative cloudiness factor (0-1)
! DAIR
! EAIR    Vapor pressure at dewpoint (Pa)
! EO      Potential evapotranspiration rate (mm/d)
! ESAT    Vapor pressure of air (Pa)
! G       Soil heat flux density term (MJ/m2/d)
! LHVAP   Latent head of water vaporization (J/kg)
! PATM     = 101300.0
! PSYCON  Psychrometric constant (Pa/K)
! RADB    Net outgoing thermal radiation (MJ/m2/d)
! RNET    Net radiation (MJ/m2/d)
! RNETMG  Radiant energy portion of Penman equation (mm/d)
! RT
! S       Rate of change of saturated vapor pressure of air with
!           temperature (Pa/K)
! MSALB   Soil albedo with mulch and soil water effects (fraction)
! SBZCON   Stefan Boltzmann constant = 4.903E-9 (MJ/m2/d)
! SHAIR    = 1005.0
! SRAD    Solar radiation (MJ/m2-d)
! TAVG    Average daily temperature (°C)
! TDEW    Dewpoint temperature (°C)
! TK4     Temperature to 4th power ((oK)**4)
! TMAX    Maximum daily temperature (°C)
! TMIN    Minimum daily temperature (°C)
! Tprev   3-day sum of average temperature:
! VHCAIR
! VPD     Vapor pressure deficit (Pa)
! VPSAT   Saturated vapor pressure of air (Pa)
! VPSLOP  Calculates slope of saturated vapor pressure versus
!           temperature curve (Pa/K)
! WFNFAO  FAO 24 hour wind function
! WIND2   Windspeed at 2m reference height. (km/d)
! WINDSP  Wind speed at 2m (km/d)
! XHLAI   Leaf area index (m2[leaf] / m2[ground])
!-----------------------------------------------------------------------
!     END SUBROUTINE PETPEN
!-----------------------------------------------------------------------

C=======================================================================
C  PETDYN Subroutine, K. J. BOOTE, F. SAU, M. BOSTIC
C  Calculates PENMAN-MONTEITH potential evapotranspiration
C  using dynamic CANHT, LAI, along with wind effects on Ra, Rs
C  Steiner approach for Ra recommended, but FAO and Lhomme commented out
C  Sunlit LAI effect on Rs is recommended, but rl/(0.5*LAI) would work
C  Weighting used for Ra and Rs between soil and crop.  Need to changee
C  two constants (HTS and zos) if you change from sunlit LAI to other.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  11/19/01 TO 1/15/02  Written By Boote, Sau, McNair
C  01/15/03 Moved from V3.5 trial to V4.0  by K. J. Boote
!  07/24/2006 CHP Use MSALB instead of SALB (includes mulch and soil
!                 water effects on albedo)

!  Called from:   PET
!  Calls:         None
C=======================================================================
      SUBROUTINE PETDYN(
     &    CANHT, CLOUDS, MSALB, SRAD, TAVG, TDEW,         !Input
     &    TMAX, TMIN, WINDSP, XHLAI,                      !Input
     &    EO)                                             !Output
C  Calculates Penman-Monteith evapotranspiration
!-----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL VPSAT, VPSLOP
      SAVE
!-----------------------------------------------------------------------
!     INPUT VARIABLES:
      REAL CLOUDS, MSALB, SRAD, TAVG, TDEW, TMAX, TMIN,
     &        WINDSP, XHLAI, WINDSP_M
!-----------------------------------------------------------------------
!     OUTPUT VARIABLES:
      REAL EO
!-----------------------------------------------------------------------
!     LOCAL VARIABLES:
      REAL ALBEDO, EAIR, ESAT, G, LHVAP, PSYCON, RADB,
     &  RNET, RNETMG, S, TK4,
     &  VHCAIR, VPD, DAIR, RT
      REAL SHAIR, PATM, SBZCON
      REAL k,DFAO, CANHT, ZOMF, ZOHF, ra, rl, rs, RAERO !add for PenDyn
      REAL ZCROP,DCROP,ZOMC,ZOVC,WIND2C,RASOIL,HTS,DLH,ZOLH
      REAL MAXHT, rb, AC, AS, zos, RTOT                  !add for PenDyn
C     PARAMETER (SHAIR = 1005.0)
      PARAMETER (SHAIR = 0.001005)  !changed for PenDyn to MJ/kg/K
      PARAMETER (PATM = 101300.0)
!      PARAMETER (SBZCON=4.093E-9)  !(MJ/m2/d)
      PARAMETER (SBZCON=4.903E-9)   !(MJ/K4/m2/d) fixed constant 5/6/02
!-----------------------------------------------------------------------
!     FUNCTION SUBROUTINES:
      REAL VPSLOP, VPSAT      !Found in file HMET.for

C-----------------------------------------------------------------------
C     Compute air properties.
      LHVAP = (2501.0-2.373*TAVG) * 1000.0                 ! J/kg
C     PSYCON = SHAIR * PATM / (0.622*LHVAP)                ! Pa/K
      PSYCON = SHAIR * PATM / (0.622*LHVAP) * 1000000     ! Pa/K
      ESAT = (VPSAT(TMAX)+VPSAT(TMIN)) / 2.0               ! Pa
      EAIR = VPSAT(TDEW)                                   ! Pa
      VPD = ESAT - EAIR                                    ! Pa
      S = (VPSLOP(TMAX)+VPSLOP(TMIN)) / 2.0                ! Pa/K
      RT = 8.314 * (TAVG + 273.0)                             ! N.m/mol
      DAIR = 0.028966*(PATM-0.387*EAIR)/RT                    ! kg/m3
C BAD DAIR = 0.1 * 18.0 / RT * ((PATM  -EAIR)/0.622 + EAIR)   ! kg/m3
      VHCAIR = DAIR * SHAIR    !not used                      ! J/m3

C     Convert windspeed to 2 m reference height.
!     Do this conversion in WEATHR and send out 2m windspeed
!     CHP 11/26/01
!      WIND2 = WINDSP * (2.0/WINDHT)**0.2

C       Calculate aerodynamic resistance (ra).
C       ra (d/m) = {ln[zm-d/zom]*ln[zh-d/zoh]}/(k^2*uz)
C       zm = ht.wind measurement (m), zh = ht.humidity measurement (m),
C       zom,zoh=rooughness length of momentum, heat and vapor x-fer (m)
C       k=von Karman's constant 0.41, uz=WINDSP @ z m/d,
C       d = zero plane displacement height (m)

      WINDSP_M = WINDSP*(1000.)/86400.          !Converts km/d to m/s
      k = 0.41                                  !von Karman's constant

      IF (CANHT .LE. 0.10) THEN
        ZCROP = 2.0 + 0.10
C       Next 3 are Steiner et al. coefficients, used for Steiner Ra
        DCROP = 0.75 * 0.10
        ZOMC = 0.25 * (0.10 - DCROP)
        ZOVC = 0.1 * ZOMC
        DFAO = 2. * 0.10 / 3.0
        ZOMF = 0.123*0.10
        ZOHF = 0.1*ZOMF

      ELSE
        ZCROP = 2.0 + CANHT
        DCROP = 0.75 * CANHT
        ZOMC = 0.25 * (CANHT - DCROP)
        ZOVC = 0.1 * ZOMC
        DFAO= 2.0 * CANHT / 3.0
        ZOMF = 0.123*CANHT
        ZOHF = 0.1*ZOMF
      ENDIF

C     LHOMME ET AL. AG & FOR. MET. 104:119.  2000.
C     Combined effects of LAI and crop height on Ra
C     cd = 0.2 (in eq below), where X=0.2*LAI
C     Zolh up to X<0.2 (or LAI=1), then X 0.2 to 1.5 (LAI=7.5)
C     Actually should have a cap at LAI 7.5 or less.

      DLH = 1.1*MAX(0.10,CANHT)*LOG(1.+(0.2*XHLAI)**0.25)

      IF (XHLAI .LT. 1.0) THEN
        ZOLH = 0.01+0.3*MAX(0.10,CANHT)*(0.2*XHLAI)**0.5
      ELSE
C        ELSEIF (XHLAI .LT. 7.5)
        ZOLH = 0.3*MAX(0.10,CANHT)*(1.0-DLH/MAX(0.10,CANHT))
      ENDIF

C  Concept of Ra, always for 2 m above crop height, from Steiner et al
C       Agron. J. 83:240.  1991.  Also, needs wind speed adjusted, up to
C       10 m, then back down to exactly 2 m above crop height.
C       Needs z defined at 2 m above crop, or z = 2.0 + CANHT
C       Grass assumed 0.10 m, its d is 0.075, its Zom is 0.00625

      WIND2C = WINDSP_M * LOG((10.-0.075)/0.00625) *
     &                      LOG((ZCROP-DCROP)/ZOMC) /
     &         (LOG((10.-DCROP)/ZOMC)*LOG((2.-0.075)/0.00625))

C       Steiner Ra
      ra = ( (LOG((ZCROP-DCROP)/ZOMC)*LOG((ZCROP-DCROP)/ZOVC))
     &       /((k**2)*WIND2C) )  /86400

C       Standard FAO Ra
C       ra = ( (LOG((ZCROP-DFAO)/ZOMF)*LOG((ZCROP-DFAO)/ZOHF))
C    &       /((k**2)*WIND2C) )  /86400

C       Lhomme Ra
C       ra = ( (LOG((ZCROP-DLH)/ZOLH)*LOG((ZCROP-DLH)/(0.1*ZOLH)))
C    &       /((k**2)*WIND2C) )  /86400

C      NOW, COMPUTING Ra for bare soil and Rs for bare soil
C      For bare soil Ra, with an effective height of 0.40 m
C      Uses standard FAO eq, windsp for 2 m height.  Not for soil Rs
C      HTS = 0.13, for SUNLIT LAI FORM.  HTS = 0.25 for 0.5*LAI FORM.

      HTS = 0.13
C      HTS = 0.25
      RASOIL = (  (LOG((2.0-2*HTS/3.)/(0.123*HTS))
     &    *LOG((2.0-2*HTS/3.)/(0.1*0.123*HTS)))/((k**2)*WINDSP_M))/86400

CWMB    BOUNDARY LAYER RESISTANCE (rb) FOR BARE SOIL FROM (JAGTAP AND JONES, 1989)
C       zos = roughness ht. of soil (m), MAXHT = maximum plant height (m)
C       MAXHT is a dummy argument to get right Rs from soil.  Not real
C       This is wet surface resistance, Rs-soil, to match up with Rs-crop
C       Do not want WIND2C, as this one acts most for bare soil, no crop
C
C       For Sunlit LAI for Rc, use zos = 0.01
C       For 0.5*LAI for Rc, need zos = 0.03
      zos = 0.01
C       zos = 0.03
      MAXHT = 1.0
      rb=((log(2.0/zos)*log((0.83*MAXHT)/zos))/
     &            ((k**2)*WINDSP_M))/86400
C

C       Using K = 0.5 everywhere possible
        AC = 1-exp(-0.50*XHLAI)
        AS = 1 - AC

      RAERO = AC*RA + AS*RASOIL
C     Calculate surface resistance (rs).
C     rs = rl/LAIactive       rs (s m^-1),
C     rl = bulk stomatal resistance of the well-illuminated leaf (s m^-1)

      rl = 100                !value assummed from FAO grass reference
      IF (XHLAI .GE. 0.1) THEN
C          rs = rl/(0.5*XHLAI)
        rs = rl/((1/0.5)*(1.0-EXP(-0.5*XHLAI)))       !SUNLIT LAI form
      ELSE
        rs = rl/(0.5*0.1)
      ENDIF

      rs = rs/86400           !converts (s m^-1 to d/m)

      RTOT = AC*rs + AS*rb

C     Calculate net radiation (MJ/m2/d).  By FAO method 1990. EAIR is divided
C       by 1000 to convert Pa to KPa.

      G = 0.0
      IF (XHLAI .LE. 0.0) THEN
        ALBEDO = MSALB
      ELSE
C     I THINK THIS K VALUE SHOULD BE 0.5, NEAR THEORETICAL OF 0.5 KJB
        ALBEDO = 0.23-(0.23-MSALB)*EXP(-0.75*XHLAI)
      ENDIF

      TK4 = ((TMAX+273.)**4+(TMIN+273.)**4) / 2.0
      RADB = SBZCON * TK4 * (0.34 - 0.14 * SQRT(EAIR/1000)) *
     &        (1.35 * (1. - CLOUDS) - 0.35)
      RNET= (1.0-ALBEDO)*SRAD - RADB

C     Compute EO using Penman-Montieth

      RNETMG = (RNET-G)
C     !MJ/m2/d
      EO=((S*RNETMG + (DAIR*SHAIR*VPD)/RAERO)/(S+PSYCON*(1+RTOT/RAERO)))
C     !Converts MJ/m2/d to mm/d
        EO = EO/ (LHVAP / 1000000.)
!###  EO = MAX(EO,0.0)   !gives error in DECRAT_C
      EO = MAX(EO,0.0001)

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE PETDYN

!     PETPEN VARIABLES:  Nearly same as PETPEN above

C=======================================================================
C  PETPT, Subroutine, J.T. Ritchie
C  Calculates Priestly-Taylor potential evapotranspiration
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  ??/??/19?? JR  Written
C  11/04/1993 NBP Modified
C  10/17/1997 CHP Updated for modular format.
C  09/01/1999 GH  Incorporated into CROPGRO
!  07/24/2006 CHP Use MSALB instead of SALB (includes mulch and soil
!                 water effects on albedo)
!-----------------------------------------------------------------------
!  Called by:   WATBAL
!  Calls:       None
C=======================================================================
      SUBROUTINE PETPT(
     &    MSALB, SRAD, TMAX, TMIN, XHLAI,                 !Input
     &    EO)                                             !Output

!-----------------------------------------------------------------------
      IMPLICIT NONE

!-----------------------------------------------------------------------
!     INPUT VARIABLES:
      REAL MSALB, SRAD, TMAX, TMIN, XHLAI
!-----------------------------------------------------------------------
!     OUTPUT VARIABLES:
      REAL EO
!-----------------------------------------------------------------------
!     LOCAL VARIABLES:
      REAL ALBEDO, EEQ, SLANG, TD

!-----------------------------------------------------------------------
!     Should use TAVG here -- we have it from WEATHER variable!
!     SSJ 9/18/2006
!     TD = TAVG
!     JWJ 2/15/2007 - Can't use TAVG unless coefficients in EEQ
!         equation are recalibrated.  Keep TD calc as it was
!         developed.
      TD = 0.60*TMAX+0.40*TMIN

      IF (XHLAI .LE. 0.0) THEN
        ALBEDO = MSALB
      ELSE
        ALBEDO = 0.23-(0.23-MSALB)*EXP(-0.75*XHLAI)
      ENDIF

      SLANG = SRAD*23.923
      EEQ = SLANG*(2.04E-4-1.83E-4*ALBEDO)*(TD+29.0)
      EO = EEQ*1.1

      IF (TMAX .GT. 35.0) THEN
        EO = EEQ*((TMAX-35.0)*0.05+1.1)
      ELSE IF (TMAX .LT. 5.0) THEN
        EO = EEQ*0.01*EXP(0.18*(TMAX+20.0))
      ENDIF

!###  EO = MAX(EO,0.0)   !gives error in DECRAT_C
      EO = MAX(EO,0.0001)

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE PETPT
!-----------------------------------------------------------------------
!     PETPT VARIABLES:
!-----------------------------------------------------------------------
! ALBEDO  Reflectance of soil-crop surface (fraction)
! EEQ     Equilibrium evaporation (mm/d)
! EO      Potential evapotranspiration rate (mm/d)
! MSALB   Soil albedo with mulch and soil water effects (fraction)
! SLANG   Solar radiation
! SRAD    Solar radiation (MJ/m2-d)
! TD      Approximation of average daily temperature (ºC)
! TMAX    Maximum daily temperature (°C)
! TMIN    Minimum daily temperature (°C)
! XHLAI   Leaf area index (m2[leaf] / m2[ground])
!-----------------------------------------------------------------------
!     END SUBROUTINE PETPT
C=======================================================================


C=======================================================================
C  PETPNO, Subroutine, N.B. Pickering
C  Calculates FAO-24 Penman potential evapotranspiration (without
C  correction)--grass reference.
!-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/19/1992 NBP Written
C  11/04/1993 NBP Renamed routine PET to PETPEN.  Changed XLAI to XHLAI
C               Added XHLAI .LE. 0.0 statement.
C  05/13/1994 NBP Converted all vapor pressures to Pa.  Rearranged.
C  09/16/1994 NBP Added limits to prevent EO and ES (PE) < 0.
C  10/17/1997 CHP Updated for modular format.
C  09/01/1999 GH  Incorporated into CROPGRO
C  05/06/2002 WMB Fixed Stefan-Boltzmann constant
!  07/24/2006 CHP Use MSALB instead of SALB (includes mulch and soil
!                 water effects on albedo)
!-----------------------------------------------------------------------
!  Called from:   PET
!  Calls:         None
C=======================================================================
      SUBROUTINE PETPNO(
     &    CLOUDS, MSALB, SRAD, TAVG, TDEW,                !Input
     &    TMAX, TMIN, WINDSP, XHLAI,                      !Input
     &    EO)                                             !Output
!-----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL VPSAT, VPSLOP
!-----------------------------------------------------------------------
!     INPUT VARIABLES:
      REAL CLOUDS, MSALB, SRAD, TAVG, TDEW, TMAX, TMIN,
     &        WINDSP, XHLAI
!-----------------------------------------------------------------------
!     OUTPUT VARIABLES:
      REAL EO
!-----------------------------------------------------------------------
!     LOCAL VARIABLES:
      REAL ALBEDO, EAIR, ESAT, G, LHVAP, PSYCON, RADB,
     &  RNET, RNETMG, S, TK4,
     &  VHCAIR, VPD, WFNFAO, DAIR, RT
      REAL SHAIR, PATM, SBZCON

      PARAMETER (SHAIR = 1005.0)
      PARAMETER (PATM = 101300.0)
!      PARAMETER (SBZCON=4.093E-9)   !(MJ/m2/d)
      PARAMETER (SBZCON=4.903E-9)   !(MJ/K4/m2/d) fixed constant 5/6/02
!-----------------------------------------------------------------------
!     FUNCTION SUBROUTINES:
      REAL VPSLOP, VPSAT      !Found in file HMET.for

C-----------------------------------------------------------------------
C     Compute air properties.
      LHVAP = (2501.0-2.373*TAVG) * 1000.0                 ! J/kg
      PSYCON = SHAIR * PATM / (0.622*LHVAP)                ! Pa/K
      ESAT = (VPSAT(TMAX)+VPSAT(TMIN)) / 2.0               ! Pa
      EAIR = VPSAT(TDEW)                                   ! Pa
      VPD = ESAT - EAIR                                    ! Pa
      S = (VPSLOP(TMAX)+VPSLOP(TMIN)) / 2.0                ! Pa/K
      RT = 8.314 * (TAVG + 273.0)                             ! N.m/mol
      DAIR = 0.1 * 18.0 / RT * ((PATM  -EAIR)/0.622 + EAIR)   ! kg/m3
      VHCAIR = DAIR * SHAIR    !not used                      ! J/m3

C     Convert windspeed to 2 m reference height.
!     Do this conversion in WEATHR and send out 2m windspeed
!     CHP 11/26/01
!      WIND2 = WINDSP * (2.0/WINDHT)**0.2

C     Calculate net radiation (MJ/m2/d).  Constants for RADB from
C     Jensen et al (1989) for semi-humid conditions.  The value 0.005
C     converts the value 0.158 from kPa to Pa.

      G = 0.0
      IF (XHLAI .LE. 0.0) THEN
        ALBEDO = MSALB
      ELSE
        ALBEDO = 0.23-(0.23-MSALB)*EXP(-0.75*XHLAI)
      ENDIF

      TK4 = ((TMAX+273.)**4+(TMIN+273.)**4) / 2.0
      RADB = SBZCON * TK4 * (0.4 - 0.005 * SQRT(EAIR)) *
     &        (1.1 * (1. - CLOUDS) - 0.1)
      RNET= (1.0-ALBEDO)*SRAD - RADB

C     Compute ETP using the FAO wind function.  The multipliers for WNDFAO
C     are 1000 times smaller than in Jensen et al (1979) to convert VPD in
C     Pa to kPa. Equation for RNETMG converts from MJ/m2/d to mm/day.

!      WFNFAO = 0.0027 * (1.0+0.01*WIND2)
      WFNFAO = 0.0027 * (1.0+0.01*WINDSP)
      RNETMG = (RNET-G) / LHVAP * 1.0E6
      EO = (S*RNETMG + PSYCON*WFNFAO*VPD) / (S+PSYCON)
!###  EO = MAX(EO,0.0)   !gives error in DECRAT_C
      EO = MAX(EO,0.0001)

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE PETPNO

!-----------------------------------------------------------------------
!     PETPEN VARIABLES:
!-----------------------------------------------------------------------
! ALBEDO  Reflectance of soil-crop surface (fraction)
! CLOUDS  Relative cloudiness factor (0-1)
! DAIR
! EAIR    Vapor pressure at dewpoint (Pa)
! EO      Potential evapotranspiration rate (mm/d)
! ESAT    Vapor pressure of air (Pa)
! G       Soil heat flux density term (MJ/m2/d)
! LHVAP   Latent head of water vaporization (J/kg)
! PATM     = 101300.0
! PSYCON  Psychrometric constant (Pa/K)
! RADB    Net outgoing thermal radiation (MJ/m2/d)
! RNET    Net radiation (MJ/m2/d)
! RNETMG  Radiant energy portion of Penman equation (mm/d)
! RT
! S       Rate of change of saturated vapor pressure of air with
!           temperature (Pa/K)
! MSALB   Soil albedo with mulch and soil water effects (fraction)
! SBZCON   Stefan Boltzmann constant = 4.093E-9 (MJ/m2/d)
! SHAIR    = 1005.0
! SRAD    Solar radiation (MJ/m2-d)
! TAVG    Average daily temperature (°C)
! TDEW    Dewpoint temperature (°C)
! TK4     Temperature to 4th power ((oK)**4)
! TMAX    Maximum daily temperature (°C)
! TMIN    Minimum daily temperature (°C)
! VHCAIR
! VPD     Vapor pressure deficit (Pa)
! VPSAT   Saturated vapor pressure of air (Pa)
! VPSLOP  Calculates slope of saturated vapor pressure versus
!           temperature curve (Pa/K)
! WFNFAO  FAO 24 hour wind function
! WIND2   Windspeed at 2m reference height. (km/d)
! WINDSP  Wind speed at 2m (km/d)
! XHLAI   Leaf area index (m2[leaf] / m2[ground])
!-----------------------------------------------------------------------
!     END SUBROUTINE PETPNO
!-----------------------------------------------------------------------


C=======================================================================
C=======================================================================
C  PETMEY, Subroutine
!  Copyright(c) CSIRO 2000C  Calculates soil-plant-atmosphere interface
!  energy balance components.
C-----------------------------------------------------------------------
C  REVISION       HISTORY

C-----------------------------------------------------------------------
C  Called by: Main

C=============================================================================
!  Routine to calculate Potential Evapotranspiration (ET) from daily         !
!  weather data.                                                             !
!                                                                            !
!  Uses coefficients and methods as per                                      !
!  "Standard reference evaporation calculation for inland south eastern      !
!   Australia" By Wayne Meyer 1993                                           !
!                                                                            !
!  Written in Microsoft Fortran V5.1 by Bob White March 1994                 !
!                                                                            !
!  Long variable names (up to 31 characters) have been used. This may cause  !
!  some problems if this routine is complied with early versions of fortran  !
!                                                                            !
!  Function returns calculated value for Potential ET (0.0 if error)         !
!                                                                            !
!  Modified to account for changing albedo from bare soil (salb) to crop     !
!  (0.23) as canopy (LAI) increases. RJGW -> 05-04-95                        !
!                                                                            !
!  04/01/2004 CHP adapted for CSM
!  07/24/2006 CHP Use MSALB instead of SALB (includes mulch and soil
!                 water effects on albedo)
C=============================================================================


! VARIABLES INPUT TO ROUTINE
C=============================================================================
!! Name             !                                                        !
C=============================================================================
!! MeanTemp         ! Mean daily temperature in degres C                     !
C=============================================================================
!! Prev3dayMean     ! Mean of the previous 3 days mean temperature           !
C=============================================================================
!! DailyWindRun     ! Daily wind run in Km / day                             !
C=============================================================================
!! SolarIrradiance  ! Solar irradiance (MJ / m**2 / day)                     !
C=============================================================================
!! MeanDewPt        ! Mean daily dew point temperature (Deg C)               !
C=============================================================================
!! Jday             ! Julian day of the year                                 !
C=============================================================================
!!VARIABLES LOCAL TO THIS ROUTINE
C=============================================================================
!!                  ! Description                                            !
C=============================================================================
!! Albedo           !                                                        !
C=============================================================================
!! Coeff_A          ! Empirical coefficient used in Ro calculation           !
C=============================================================================
!! Coeff_B          !    "          "        "    "  "      "                !
C=============================================================================
!! Coeff_C          ! Empirical coefficient used in Net emissivity calc.     !
C=============================================================================
!! Coeff_D          !    "           "        "   "  "       "      "        !
C=============================================================================
!! Coeff_WindA      ! Empirical coefficient used in wind function calc.      !
C=============================================================================
!! Coeff_WindB      !    "           "        "   "  "       "     "         !
C=============================================================================
!! Delta            ! Used in proportioning term see equ (7)                 !
C=============================================================================
!! DODPG            ! Proportioning term                                     !
C=============================================================================
!! ETpot            ! Calculated potential evapotranspiration                !
C=============================================================================
!! Fac1             ! Intermediate term in long wave radiation calc.         !
C=============================================================================
!! Gflux            ! Ground heat flux                                       !
C=============================================================================
!! LatHeapVap       ! Latent heat of vaporisation                            !
C=============================================================================
!! MaxIrradiance    ! Max clear day irradiance                               !
C=============================================================================
!! NetEmissivity    ! Net emissivity                                         !
C=============================================================================
!! NetRad           ! Net radiation                                          !
C=============================================================================
!! PI               !                                                        !
C=============================================================================
!! Radj             ! Julian day of year converted to radians                !
C=============================================================================
!! RalLon           ! Long wave radiation                                    !
C=============================================================================
!! StefBoltz        ! Stefan-Boltzman constant                               !
C=============================================================================
!! VPdew            ! Actual daily vapour pressure                           !
C=============================================================================
!! VPD              ! Vapour pressure deficit                                !
C=============================================================================
!! VPsat            ! Saturation vapour pressure                             !
C=============================================================================
!! WindFunc         ! Wind function (see equ 24)                             !
C=============================================================================
C=============================================================================


      Subroutine Petmey(CONTROL,
     &    MeanTemp, DailyWindRun, SolarIrradiance,        !Input
     &    MeanDewPt, Xhlai, MSALB,                        !Input
     &    EO)                                             !Output

      Use ModuleDefs
      Implicit none
      EXTERNAL Yr_Doy
      SAVE

      INTENT(IN) :: CONTROL,
     &    MeanTemp, DailyWindRun, SolarIrradiance,
     &    MeanDewPt, Xhlai, MSALB
      INTENT(OUT) :: EO

       Integer Jday,Year,Yrdoy,yRSIM
       Real Albedo, Coeff_WindA, Coeff_WindB
       Real Coeff_A,Coeff_B,Coeff_C,Coeff_D
       Real DailyWindRun,Eo,Xhlai
       Real Delta, dodpg, Fac1, Gflux, LatHeatVap
       Real MaxIrradiance,MSALB
       Real MeanDewPt, MeanTemp
       Real NetEmissivity,NetRad  !,PI
!       Real Prev3dayMean,Radj, RadLon, SolarIrradiance, StefBoltz
       Real Radj, RadLon, SolarIrradiance, StefBoltz
       Real VPdew, VPD, VPsat, WindFunc
       Real TAVt,Tavy2,Tavy1,T3day,Tav
!       Character*2 Crop
C
        TYPE (ControlType) CONTROL
        YRDOY = CONTROL % YRDOY
        YRSIM = CONTROL % YRSIM

!       write(*,10)MeanTemp,Prev3dayMean,DailyWindRun,
!     &            SolarIrradiance, MeanDewPt,Albedo,Jday
!10     format(1x,'Variables passed to ETpot',/,
!     &        1x,'Mean temp = ',F6.1,/,
!     &        1x,'Prev 3 day mean temp = ',F6.1,/,
!     &        1x,'Daily wind run = ',F6.1,/,
!     &        1x,'Solar irradiance = ',F6.1,/,
!     &        1x,'Mean dew point = ',F6.1,/,
!     &        1x,'Albedo = ',F6.3,/,
!     &        1x,'Day of year = ',I3,/)

!  EMPiRICAL COEFFICIENTS (as defined by Meyer Tech memo 1993)
c
c get day of year for max irradiance calculation
c

        CALL Yr_Doy(Yrdoy,Year,Jday)
c
c    compute moving average 3 day temperature - use average for first 3 days
c
      Tav=MeanTemp
      If(YrDoy.le.Yrsim+3)Then
         Tavt=Tav
         Tavy2=Tav
         Tavy1=Tav
      Else
         Tavy2=Tavy1
         Tavy1=Tavt
         Tavt=Tav
      Endif
      T3Day=(Tavy2+Tavy1+tav)/3.0

c
c   calculate albedo
c
       IF (cONTROL%Crop.eq.'RI') THEN
          Albedo=0.23-(0.23-0.05)*exp(-0.75*xhlai)
       Else
          Albedo =0.23 -(0.23 -MSALB)*exp(-0.75*Xhlai)
       Endif

        Coeff_A     = 0.92
        Coeff_B     = 0.08
        Coeff_C     = 0.34
        Coeff_D     = -0.139
        Coeff_WindA = 17.8636
        Coeff_WindB = 0.0440
        !Albedo      = 0.23 This is being passed in
        StefBoltz   = 4.896e-09
!        PI          = 22.0 / 7.0

!  CALCULATE LATENT HEAT OF VAPORIZATION, HTVAP (MJ/m2/mm)

        LatHeatVap = 2.50025 - 0.002365 * MeanTemp

!  CALCULATE WEIGHING FACTOR, DODPG (-)

!     Slope of saturation curve (kPa/Deg C)
        delta = 0.1 * exp(21.255 - (5304 / (MeanTemp+273.1)))
     &          * (5304 / (MeanTemp+273.1)**2)

!     PROPORTIONING TERM
        dodpg = delta/(delta+0.066)

!  Calculated incoming solar irradiance on a clear day, (MJ/m**2/day)
!  Maximum daily irradiance from fitted data (for SE Aust.)
!  Use a general equation or lookup table for other areas

        RadJ = (REAL(Jday) / 365.25) * PI * 2.0

        MaxIrradiance = 22.357 + (11.0947 * cos(RadJ)) -
     &                  (2.3594 * sin(RadJ))

!        if (SolarIrradiance .gt. MaxIrradiance)
!     &      SolarIrradiance = MaxIrradiance

        VPsat = 0.611*exp((17.27*MeanTemp)/(MeanTemp + 237.3))
        VPdew = 0.611*exp((17.27*MeanDewPt)/(MeanDewPt + 237.3))
        VPdew = AMIN1(VPdew,VPsat)
        VPD = VPsat-VPdew


!  CALCULATE OUTGOING LONGWAVE RADIATION, RADLON [Ro] (MJ/m**2/day)

        NetEmissivity = Coeff_C+(Coeff_D*sqrt(VPdew))

        Fac1 = (Coeff_A*(SolarIrradiance/MaxIrradiance)) + Coeff_B
        Radlon = Fac1 * NetEmissivity * StefBoltz * (MeanTemp+273.)**4.

!  CALCULATE NET RADIATION, RADNET (MJ/m2)

        NetRad = ((1. - Albedo) * SolarIrradiance) - Radlon

!  CALCULATE SOIL HEAT FLUX, SHFLUX (MJ/m2)

        Gflux = (MeanTemp - T3Day) * 0.12

!  CALCULATE COMPONENTS OF AERODYNAMIC TERM

        WindFunc = Coeff_WindA + (Coeff_WindB * DailyWindRun)

!  POTENTIAL EVAPOTRANSPIRATION BY COMBINATION METHOD, ETPOT (mm/day)

!        Gflux= AMIN1(NetRad,Gflux)

        Eo = ( dodpg * (NetRad-Gflux)
     &          + (1.- dodpg) * WindFunc * VPD )/LatHeatVap

      if (eo.lt.0.0) eo = 0.0
      end Subroutine Petmey
!=======================================================================

C=======================================================================
C  PETPTH, Subroutine, based on J.T. Ritchie
C  Calculates Priestly-Taylor potential evapotranspiration
C  using hourly data and adding a VPD effect on transpiration
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  ??/??/19?? JR  Written
C  11/04/1993 NBP Modified
C  10/17/1997 CHP Updated for modular format.
C  09/01/1999 GH  Incorporated into CROPGRO
!  07/24/2006 CHP Use MSALB instead of SALB (includes mulch and soil
!                 water effects on albedo)
!  09/01/2020 LPM  Modified PETPT to use hourly variables
!-----------------------------------------------------------------------
!  Called by:   WATBAL
!  Calls:       None
C=======================================================================
      SUBROUTINE PETPTH(
     &    MSALB, TMAX, XHLAI, RADHR, TAIRHR,              !Input
     &    EO,ET0)                                         !Output

!-----------------------------------------------------------------------
      USE ModuleDefs
      USE ModuleData
      IMPLICIT NONE

!-----------------------------------------------------------------------
!     INPUT VARIABLES:
      REAL MSALB, TMAX, XHLAI
      REAL, DIMENSION(TS)    ::RADHR, TAIRHR
!-----------------------------------------------------------------------
!     OUTPUT VARIABLES:
      REAL EO
      REAL, DIMENSION(TS)    :: ET0
!-----------------------------------------------------------------------
!     LOCAL VARIABLES:
      REAL ALBEDO, EEQ, SLANG
      INTEGER hour
      REAL EOP
!-----------------------------------------------------------------------


      IF (XHLAI .LE. 0.0) THEN
        ALBEDO = MSALB
      ELSE
        ALBEDO = 0.23-(0.23-MSALB)*EXP(-0.75*XHLAI)
      ENDIF

      EO = 0.0
      EOP = 0.0
      DO hour = 1,TS
          SLANG = (RADHR(hour)*3.6/1000.)*23.923
          EEQ = SLANG*(2.04E-4-1.83E-4*ALBEDO)*(TAIRHR(hour)+29.0)
          ET0(hour) = EEQ*1.1
          IF (TMAX .GT. 35.0) THEN
            ET0(hour) = EEQ*((TMAX-35.0)*0.05+1.1)
          ELSE IF (TMAX .LT. 5.0) THEN
            ET0(hour) = EEQ*0.01*EXP(0.18*(TMAX+20.0))
          ENDIF
          EO = EO + ET0(hour)
      ENDDO



!###  EO = MAX(EO,0.0)   !gives error in DECRAT_C
      EO = MAX(EO,0.0001)

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE PETPTH
!-----------------------------------------------------------------------
!     PETPTH VARIABLES:
!-----------------------------------------------------------------------
! ALBEDO  Reflectance of soil-crop surface (fraction)
! EEQ     Equilibrium evaporation (mm/d)
! EO      Potential evapotranspiration rate (mm/d)
! EOPH    Hourly potential transpiration (mm/h)
! EOP     Potential transpiration (mm/h)
! EOS     Potential evaporation (mm/h)
! ET0     Hourly reference transpiration (mm/m2/hr)
! MSALB   Soil albedo with mulch and soil water effects (fraction)
! PHTV          VPD response threshold, kPa                (set in CSYCA047.SPE. PHTV >= 5 shuts off the response)
! PHSV          Slope of VPD response, #/kPa               (negative, set in CSYCA047.SPE)
! RADHR         Solar radiation, hourly                    (from WEATHER % RADHR  in ModuleDefs)
! SLANG   Solar radiation
! TAIRHR        Air temperature, hourly, °C                (from WEATHER % TAIRHR in ModuleDefs)
! TDEW          Dew point tempreature,°C                   (from WEATHER % TDEW   in ModuleDefs)
! TMAX    Maximum daily temperature (°C)
! TMIN    Minimum daily temperature (°C)
! XHLAI   Leaf area index (m2[leaf] / m2[ground])
! VPDFPHR       VPD factor, hourly (#, 0-1)
!-----------------------------------------------------------------------
!     END SUBROUTINE PETPTH
C=======================================================================


!=======================================================================
!  PSE, Subroutine, J.T. Ritchie
!  Calculates soil potential evaporation from total PET and LAI.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  ??/??/19?? JR  Written
!  11/04/1993 NBP Modified
!  07/21/1998 CHP changed PE Function to PSE Subroutine
!  01/15/2003 KJB CHANGED TO USE SAME K EXTINCTION AS USED FOR TRANSP
!    VILLALOBOS MEASURED K=0.52 FOR BOTH SOIL EVAPORATION AND TRANSP
!    THEORY OF GOUDRIAAN INDICATES A VALUE NEAR 0.5 BECAUSE OF BOTH
!    NEAR-INFARED PLUS VISIBLE COMBINE TO GIVE YOU THIS COMBINATION K
!  07/08/2003 CHP/LAH added use of KSEVAP which comes from crop routines
!  03/10/2006 CHP Reduce potential soil evaporation if mulch cover.
!                 From A. Andeles tillage routine.
!  05/26/2007 CHP/MJ Use XLAI instead of XHLAI
!                 This was important for Canegro and affects CROPGRO crops
!                 only very slightly (max 0.5% yield diff for one peanut
!                 experiment).  No difference to other crop models.
!  06/25/2008 CHP Move mulch evaporationto MULCHEVAP subroutine
!-----------------------------------------------------------------------
!  Input : EO,XLAI
!  Output: PE
!=======================================================================

      SUBROUTINE PSE(EO, KSEVAP, XLAI, EOS)

!-----------------------------------------------------------------------
      USE ModuleDefs
      USE ModuleData
      IMPLICIT NONE
      SAVE

!     CHARACTER*1 MEINF
      REAL EO, XLAI, EOS
      REAL KSEVAP
      REAL KE, REFET

      CALL GET('SPAM', 'KE', KE)
      CALL GET('SPAM', 'REFET', REFET)
!-----------------------------------------------------------------------
!     Potential soil evaporation based on leaf area index and potential
!         evapotranspiration.

! LAH JULY 2, 2003
      IF (KE .GE. 0.0) THEN
        EOS = KE * REFET !KRT added for ASCE dual Kc ET approach
      ELSEIF (KSEVAP .LE. 0.0) THEN

!       Old computation:
        IF (XLAI .LE. 1.0) THEN   !<-------!
!         EOS = EO*(1.0 - 0.43*XLAI)       !
!         get rid of discontinuity:
          EOS = EO*(1.0 - 0.39*XLAI)       !
        ELSE                               !-> old code
          EOS = EO/1.1*EXP(-0.4*XLAI)      !
        ENDIF    !<------------------------!

      ELSE
        EOS = EO*EXP(-KSEVAP*XLAI) !<------- Tony's new code 07/02/2003
      ENDIF


!     Ken's new computation: 01/03/2003
!      EOS = EO*EXP(-0.50*XLAI)

!     Note from Tony:  this is not the same as the old computation and
!       may cause different results.  We need to re-evaluate.
!     Probably should use KEP here.

      EOS = MAX(EOS,0.0)

      RETURN
      END SUBROUTINE PSE

!-----------------------------------------------------------------------
!     PSE VARIABLES:
!-----------------------------------------------------------------------
! EO     Potential evapotranspiration rate (mm/d)
! EOS    Potential rate of soil evaporation (mm/d)
! KE     Evaporation coefficient for ASCE dual Kc ET method
! REFET  ASCE Standardized Reference Evapotranspiration (alfalfa or grass)
! XLAI   Leaf area index (m2[leaf] / m2[ground])
!-----------------------------------------------------------------------
!     END SUBROUTINE PSE
C=======================================================================


C=======================================================================
C  FLOOD_EVAP, Subroutine, U. Singh
C  Determines floodwater evaporation
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  ??/??/??   US  Written.
C  02/27/2002 CHP modified for modular format.
!  12/07/2005 CHP added discontinuity prevention
!-----------------------------------------------------------------------
!  Called by:   SPAM
!  Calls:       None
C=======================================================================
      SUBROUTINE FLOOD_EVAP(XLAI, EO, EF)

!-----------------------------------------------------------------------
      IMPLICIT NONE

      REAL EO, EF, XLAI  !, EF85

!-----------------------------------------------------------------------
!     EF = EO*(1.0-0.45*XLAI)
      EF = EO*(1.0-0.53*XLAI)  !prevents discontinuity
      IF (XLAI .GT. 0.85) THEN
!       EF85 = EO * 0.62  !EF at XLAI=0.85, prevents discontinuity
!       EF = AMIN1(EF85, EO/1.1*EXP(-0.60*XLAI))
        EF = EO/1.1*EXP(-0.60*XLAI)
      ENDIF

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE FLOOD_EVAP
C=======================================================================


