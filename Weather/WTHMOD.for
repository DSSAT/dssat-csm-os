C=======================================================================
!  WTHMOD.for contains:   
!     WTHMOD - Input weather modification array, Adjust weather variables
!     WTHSET - Update WTHADJ array.
C=======================================================================

C=======================================================================
C  WTHMOD, Subroutine, N.B. Pickering
C  Adjust weather variables according to selections made in WTHMDI
C  and stored in WTHADJ.  Variables are adjusted by an offset an
C  multiplier.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/04/1991 NBP Written
C  01/30/1998 GH  Added WTHMDB
C  02/25/1999 CHP Modified for modular format
C  04/20/1999 CHP Combined WTHMDB and WTHMOD so fewer variables need to be
C               passed to main
C  09/02/1999 GH  Incorporated into CROPGRO
C  03/15/2000 GH  Modular version revisited
C  08/20/2002 GH  Modified for Y2K
C  08/20/2002 GH  Modified for Seasonal and Sequence runs
C  08/12/2003 CHP Added I/O error checking
C  09/01/2009 CHP Added call to error checking routine, including YREND
C  03/25/2022 GH  Fix environmental modification for crop rotations
C-----------------------------------------------------------------------
C  Called by: WEATHR
C  Calls:     DECL, WTHSET
C=======================================================================

      SUBROUTINE WTHMOD(DYNAMIC,
     &    CONTROL, FILEWW, XLAT, YYDDD,                   !Input
     &    CO2, DAYL, PAR, RAIN, SRAD, TDEW,               !Input/Output
     &    TMAX, TMIN, TWILEN, WINDSP,                     !Input/Output
     &    DEC, NEV, SNUP, SNDN, YREND)                    !Output
     
!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL YR_DOY, ERROR, DAILYWEATHERCHECK, FIND, WTHSET, DECL
      SAVE

      INTEGER, PARAMETER :: NMODS = 100
      CHARACTER*1 CO2FAC(NMODS), DAYFAC(NMODS), DPTFAC(NMODS)
      CHARACTER*1 PRCFAC(NMODS), TMFAC(NMODS), TXFAC(NMODS)
      CHARACTER*1 RADFAC(NMODS), WNDFAC(NMODS)
      CHARACTER*1  WMODB, RNMODE
      CHARACTER*6  SECTION, ERRKEY
      CHARACTER*30 FILEIO
      CHARACTER*90 CHAR
      CHARACTER*92 FILEWW

      INTEGER LINWTH
      PARAMETER (LINWTH=0)
      INTEGER I, NEV, WMDATE(NMODS), YYDDD, YRDIF
      INTEGER ISIM, YRS, WYRDIF
      INTEGER LNUM, LUNIO, FOUND, ERR
      INTEGER DYNAMIC,MULTI,YRSIM,YR,WDATE, YREND

      REAL CO2,DEC,DECL,DAYL,HOLD,PAR,RAIN,SNUP,SNDN,SRAD,
     &  TDEW, TMAX, TMIN, TWILEN, WINDSP, XLAT
      REAL, DIMENSION(NMODS) ::
     &  CO2ADJ, DAYADJ, DPTADJ, PRCADJ, TMADJ,
     &  TXADJ, RADADJ, WNDADJ(NMODS)
      REAL WTHADJ(2,8), RHUM

!!     PARAMETER (CO2BAS = 330.0)
      PARAMETER (ERRKEY = 'WTHMOD')

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

!     Transfer values from constructed data types into local variables.
C     DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO
      RNMODE  = CONTROL % RNMODE
      MULTI   = CONTROL % MULTI
      YRDIF   = CONTROL % YRDIF
      YRSIM   = CONTROL % YRSIM

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT .OR. DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
!-----------------------------------------------------------------------
C    Read Environmental Modification Section
C-----------------------------------------------------------------------
      SECTION = '*ENVIR'
      CALL FIND(LUNIO, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION,42,FILEIO,LNUM)
      ELSE
        NEV = 0
        DO I = 1,NMODS
!          READ(LUNIO,120,IOSTAT=ERR,ERR=130)
!     &        WMDATE(I),DAYFAC(I),DAYADJ(I),RADFAC(I),
!     &        RADADJ(I),TXFAC(I),TXADJ(I),TMFAC(I),TMADJ(I),
!     &        PRCFAC(I),PRCADJ(I),CO2FAC(I),CO2ADJ(I),
!     &        DPTFAC(I),DPTADJ(I),WNDFAC(I),WNDADJ(I)
!  120     FORMAT(3X,I7,5(1X,A1,F4.1),1X,A1,F4.0,2(1X,A1,F4.1))
          READ(LUNIO,'(3X,I7,A90)',ERR=130) WMDATE(I), CHAR
          READ(CHAR,120,IOSTAT=ERR) 
     &        DAYFAC(I),DAYADJ(I),RADFAC(I),
     &        RADADJ(I),TXFAC(I),TXADJ(I),TMFAC(I),TMADJ(I),
     &        PRCFAC(I),PRCADJ(I),CO2FAC(I),CO2ADJ(I),
     &        DPTFAC(I),DPTADJ(I),WNDFAC(I),WNDADJ(I)
  120     FORMAT(5(1X,A1,F4.1),1X,A1,F4.0,2(1X,A1,F4.1))
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
          NEV = NEV + 1
        ENDDO
  130   CONTINUE
      ENDIF
      
      CLOSE (LUNIO)

C-----------------------------------------------------------------------
C     Adjust for multi year runs
C-----------------------------------------------------------------------
      IF (MULTI .GT. 1) THEN
        IF (WMDATE(1) .LT. YRSIM .AND. NEV .GT. 0) THEN
          DO I = 1, NEV
            CALL YR_DOY(WMDATE(I),YR,WDATE)
            WMDATE(I) = (YR + MULTI - 1) * 1000 + WDATE
          ENDDO
        ENDIF
      ENDIF

C-----------------------------------------------------------------------
C     Adjust for crop rotations
C-----------------------------------------------------------------------
      IF (RNMODE .EQ. 'Q') THEN
        IF (WMDATE(1) .LT. YRSIM .AND. NEV .GT. 0) THEN
          CALL YR_DOY(YRSIM, YRS, ISIM)
          CALL YR_DOY(WMDATE(1), YR, WDATE)
          WYRDIF  =  YRS - YR
          DO I = 1, NEV
            CALL YR_DOY(WMDATE(I),YR,WDATE)
            WMDATE(I) = (YR + WYRDIF) * 1000 + WDATE
          END DO
        ENDIF
      ENDIF

      WMODB = 'N'
      RHUM = -99.

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
      WMODB = 'N'
C     Repeat until a weather modification (WM) date preceeds today's date.

      I = 1
   20 IF (YYDDD .GE. WMDATE(I) .AND. I .LE. NEV) THEN
        I = I + 1
        GO TO 20
      ENDIF

      I = I - 1
      IF (I .GE. 1 .AND. I .LE. NEV) THEN
        CALL WTHSET(DAYADJ(I),DAYFAC(I),1,WTHADJ,WMODB)
        CALL WTHSET(RADADJ(I),RADFAC(I),2,WTHADJ,WMODB)
        CALL WTHSET( TXADJ(I), TXFAC(I),3,WTHADJ,WMODB)
        CALL WTHSET( TMADJ(I), TMFAC(I),4,WTHADJ,WMODB)
        CALL WTHSET(PRCADJ(I),PRCFAC(I),5,WTHADJ,WMODB)
        CALL WTHSET(CO2ADJ(I),CO2FAC(I),6,WTHADJ,WMODB)
        CALL WTHSET(DPTADJ(I),DPTFAC(I),7,WTHADJ,WMODB)
        CALL WTHSET(WNDADJ(I),WNDFAC(I),8,WTHADJ,WMODB)

  !      DO I = 1, 8
  !        IF (ABS(WTHADJ(1,I)) .GT. 0.001 .OR. 
  !   &          ABS(WTHADJ(2,I) - 1.) .GT. 0.0001) THEN
  !          WMODB = 'Y'
  !        ENDIF
  !      ENDDO
      ENDIF

!-----------------------------------------------------------------------
      IF (WMODB .EQ. 'Y') THEN

C       Adjustment of DAY LENGTH.  Effective DOY is calculated so calcs.
C       of SNUP, SNDN, and FRACD in SOLAR and HMET are correct.
!       Use to adjust TWILEN also. chp 05-08-02
        IF (ABS(WTHADJ(1,1)) .GT. 0.001 .OR. 
     &          ABS(WTHADJ(2,1) - 1.) .GT. 0.0001) THEN
          DAYL = WTHADJ(1,1) + DAYL*WTHADJ(2,1)
          SNUP = 12.0 - DAYL/2.0
          SNDN = 12.0 + DAYL/2.0
          DEC = DECL(DAYL,XLAT)
          TWILEN = WTHADJ(1,1) + TWILEN*WTHADJ(2,1)
        ENDIF

C       Adjustment of radiation.  PAR and SRAD are automatically
C       adjusted together.
        IF (ABS(WTHADJ(1,2)) .GT. 0.001 .OR. 
     &        ABS(WTHADJ(2,2) - 1.) .GT. 0.0001) THEN
          HOLD = WTHADJ(1,2) + SRAD*WTHADJ(2,2)
          HOLD = AMAX1 (0.05,HOLD)
          IF (PAR .GT. 0.) PAR = PAR * HOLD/SRAD
          SRAD = HOLD
        ENDIF
  
C       Adjustment of maximum and minimum temperature.
        IF (ABS(WTHADJ(1,3)) .GT. 0.001 .OR. 
     &          ABS(WTHADJ(2,3) - 1.) .GT. 0.0001) THEN
          TMAX = WTHADJ(1,3) + TMAX*WTHADJ(2,3)
        ENDIF
    
        IF (ABS(WTHADJ(1,4)) .GT. 0.001 .OR. 
     &          ABS(WTHADJ(2,4) - 1.) .GT. 0.0001) THEN
          TMIN = WTHADJ(1,4) + TMIN*WTHADJ(2,4)
        ENDIF
  
!        IF (TMAX .LT. TMIN) THEN
!          HOLD = TMAX
!          TMAX = TMIN
!          TMIN = HOLD
!        ENDIF
  
C       Adjustment of precipitation.
        IF (ABS(WTHADJ(1,5)) .GT. 0.001 .OR. 
     &          ABS(WTHADJ(2,5) - 1.) .GT. 0.0001) THEN
          RAIN = WTHADJ(1,5) + RAIN*WTHADJ(2,5)
          RAIN = AMAX1(RAIN, 0.0)
        ENDIF

C       Adjustment of CO2.
        IF (ABS(WTHADJ(1,6)) .GT. 0.001 .OR. 
     &          ABS(WTHADJ(2,6) - 1.) .GT. 0.0001) THEN
!         CO2 = WTHADJ(1,6) + CO2BAS*WTHADJ(2,6)
          CO2 = WTHADJ(1,6) + CO2*WTHADJ(2,6)
        ENDIF
  
C       Adjustment of humidity (dew point).
        IF ((ABS(WTHADJ(1,7)) .GT. 0.001) .OR. 
     &          (ABS(WTHADJ(2,7) - 1.) .GT. 0.0001)) THEN
          TDEW = WTHADJ(1,7) + TDEW*WTHADJ(2,7)
        ENDIF
  
C       Adjustment of wind speed.
        IF (ABS(WTHADJ(1,8)) .GT. 0.001 .OR. 
     &          ABS(WTHADJ(2,8) - 1.) .GT. 0.0001) THEN
          WINDSP = WTHADJ(1,8) + WINDSP*WTHADJ(2,8)
        ENDIF
  
!        IF (CO2 .LE. 0. .OR. DAYL .LT. 0. .OR. 
!     &      (TMAX - TMIN) .LT. 0.005) THEN
!          CALL ERROR(ERRKEY,10,FILEW,LINWTH)
!        ENDIF

        CALL DailyWeatherCheck(CONTROL,
     &    ERRKEY, FILEWW, RAIN, 0,                        !Input 
     &    SRAD, TMAX, TMIN, YYDDD,                        !Input
     &    YREND)                                          !Output

      ENDIF

!***********************************************************************
!***********************************************************************
!     End of DYNAMIC IF-construct
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE WTHMOD

C=======================================================================
! WTHMOD Variables
!-----------------------------------------------------------------------
! CO2         Atmospheric carbon dioxide concentration (ppm)
! CO2ADJ(10)  Carbon dioxide adjustment amount (ppm)
! CO2BAS      Carbon dioxide base level from which adjustments are made
!               (ppm)
! CO2FAC(10)  C02 adjustment switch 
! DAYADJ(10)  Day length adjustment amount (d)
! DAYFAC(10)  Day length adjustment switch 
! DAYL        Day length on day of simulation (from sunrise to sunset) (hr)
! DEC         Solar declination or (90o - solar elevation at noon). 
!               Amplitude = +/- 23.45. Min = Dec. 21, Max = Jun 21/22 (deg.)
! DECL        Solar declination function subroutine (deg.)
! DPTADJ(10)  Dewpoint temperature adjustment amount (°C)
! DPTFAC(10)  Dewpoint adjustment switch ('A', 'S', 'M', or 'R') 
! ERR         Error code for file operation 
! ERRKEY      Subroutine name for error file 
! FILEIO      Filename for input file (e.g., IBSNAT35.INP) 
! FOUND       Indicator that good data was read from file by subroutine 
!               FIND (0 - End-of-file encountered, 1 - NAME was found) 
! HOLD        Amount of water a soil layer will hold above it's present 
!               level, used to calculate downward flow; also, temporary 
!               variable/ intermediate calculation (cm)
! LINWTH      Current line read from weather file 
! LNUM        Current line number of input file 
! LUNIO       Logical unit number for FILEIO 
! NEV         Number of environmental modification records 
! PAR         Daily photosynthetically active radiation or photon flux 
!               density (moles[quanta]/m2-d)
! PRCADJ(10)  Precipitation adjustment amount (mm)
! PRCFAC(10)  Precipitation adjustment switch 
! RADADJ(10)  Radiation adjustment amount (MJ/m2-d)
! RADFAC(10)  Radiation adjustment switch 
! RAIN        Precipitation depth for current day (mm)
! SECTION     Section name in input file 
! SNDN        Time of sunset (hr)
! SNUP        Time of sunrise (hr)
! SRAD        Solar radiation (MJ/m2-d)
! TDEW        Dewpoint temperature (°C)
! TMADJ(10)   Minimum temperature adjustment amount (°C)
! TMAX        Maximum daily temperature (°C)
! TMFAC(10)   Minimum temperature adjustment switch 
! TMIN        Minimum daily temperature (°C)
! TXADJ(10)   Maximum temperature adjustment amount (°C)
! TXFAC(10)   Maximum temperature adjustment switch 
! WINDSP      Wind speed (km/d)
! WMDATE(10)  Weather modification date (YYDDD)
! WMODB       Weather modification code= 'Y' or 'N' 
! WNDADJ(10)  Wind speed adjustment amount (km/dc)
! WNDFAC(10)  Wind speed adjustment switch 
! WTHADJ(2,8) Weather modification array containing offset and multiplier 
!               for each variable (1=DAYL, 2=SRAD, 3=TMAX, 4=TMIN, 5=RAIN, 
!               6=RH, 7=WIND, 8=CO2) 
! XLAT        Latitude (deg.)
! YRDOY       Current day of simulation (YYDDD)
C=======================================================================

C=======================================================================
C  WTHSET, Subroutine
C  Update WTHADJ array
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/04/91 NBP Written
C-----------------------------------------------------------------------
C  Called by: WTHMDB
C  Calls:     None
!-----------------------------------------------------------------------
C  Input : AMOUNT,TYPE,VNUM,WTHADJ
C  Output: WTHADJ
C=======================================================================

      SUBROUTINE WTHSET(AMOUNT, TYPEC, VNUM, WTHADJ, WMODB)

!-----------------------------------------------------------------------
      IMPLICIT NONE

      CHARACTER*1 TYPEC, WMODB

      INTEGER VNUM

      REAL AMOUNT,WTHADJ(2,8)

!-----------------------------------------------------------------------

C     Change: additive <> 0, multiplicative <> 1, constant > 0.

      IF (TYPEC .EQ. 'A' .AND. ABS(AMOUNT) .GT. 1.E-5) THEN
!     Add AMOUNT to weather value
        WTHADJ(1,VNUM) = AMOUNT
        WTHADJ(2,VNUM) = 1.
        WMODB = 'Y'

      ELSEIF (TYPEC .EQ. 'S' .AND. ABS(AMOUNT) .GT. 1.E-5) THEN
!     Subtract AMOUNT from weather value
        WTHADJ(1,VNUM) = -AMOUNT
        WTHADJ(2,VNUM) = 1.
        WMODB = 'Y'

      ELSE IF (TYPEC .EQ. 'M' .AND. ABS(AMOUNT - 1.0) .GT. 1.E-5) THEN
!     Multiply weather value by AMOUNT
        WTHADJ(1,VNUM) = 0.
        WTHADJ(2,VNUM) = AMOUNT
        WMODB = 'Y'

      ELSE IF (TYPEC .EQ. 'R') THEN
!     Replace weather value by AMOUNT
        WTHADJ(1,VNUM) = AMOUNT
        WTHADJ(2,VNUM) = 0.
        WMODB = 'Y'
          
      ELSE
!     No change to weather value
        WTHADJ(1,VNUM) = 0.
        WTHADJ(2,VNUM) = 1.
      ENDIF

      RETURN
      END SUBROUTINE WTHSET

C=======================================================================
! WTHSET Variables
!-----------------------------------------------------------------------
! AMOUNT        Amount to change variable in weather modification
! TYPEC         Type of modification code: 'A'=add, 'S'=subtract, 'M'=multiply,
!                 'R'=replace
! VNUM          Array location for variable being adjusted
! WTHADJ(2,8)   Weather modification array containing offset and multiplier for 
!                 each variable (1=DAYL, 2=SRAD, 3=TMAX, 4=TMIN, 5=RAIN, 6=RH, 
!                 7=WIND, 8=CO2)
C=======================================================================

