C=======================================================================
C  FLOODI, Subroutine, U. Singh
C  Determines floodwater initialization - called first day of new
C     flood event.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C             US  Written
C  03/29/2002 CHP modular format
C=======================================================================

      SUBROUTINE FLOODI (
     &    BD1, FLOODWAT, OXMIN3, OXMIN4, SNH4,            !Input  
     &    SNO3, SOILPROP, SW1, UREA, YRDOY, YRDRY,        !Input
     &    DailyCalc, DLTUREA,  DLTSNH4, DLTSNO3, DLTFUREA,!I/O
     &    DLTFNO3, DLTFNH4, IBP,                          !I/O
     &    ALGACT, AKILL, ALGFIX, DLTOXU, DLTOXH4, DLTOXN3,!Output
     &    FNI, IMMOBIL, OXLT)                             !Output

!-----------------------------------------------------------------------
      USE ModuleDefs  
      USE FloodModule
      IMPLICIT  NONE
      EXTERNAL EQUIL2
      SAVE

      REAL    BD1, FAC1
      REAL    FLOOD, SW1
      REAL    ALGACT
      REAL    FNI, PERC
      REAL    OXFAC, OXLT, OXMIN3, OXMIN4
      REAL    ALGFIX
      INTEGER IHDAY, IBP
      INTEGER YRDOY, YRDRY
      LOGICAL AKILL, DailyCalc, IMMOBIL

      REAL, DIMENSION(NL) :: DLAYR, OC, SNH4, SNO3, UREA
      REAL FOCI, SURCEC
      REAL TMPFUREA, TMPFNO3, TMPFNH4
      REAL DLTUREA(NL),  DLTSNO3(NL), DLTSNH4(NL)
      REAL DLTFUREA, DLTFNO3, DLTFNH4
      REAL TMPUREA,  TMPNO3,  TMPNH4
      REAL TMPOXU, TMPOXH4, TMPOXN3
      REAL DLTOXU, DLTOXH4, DLTOXN3

      TYPE (SoilType)     SOILPROP
      TYPE (FloodWatType) FLOODWAT

      SURCEC= SOILPROP % CEC(1)
      DLAYR = SOILPROP % DLAYR
      OC    = SOILPROP % OC

      PERC  = FLOODWAT % PERC
      FLOOD = FLOODWAT % FLOOD

!-----------------------------------------------------------------------
      FAC1 = 1.0 / (BD1 * 1.E-01 * DLAYR(1))

      OXLT   = 0.078+(0.94*PERC)-0.03*OC(1)-0.14*PERC*OC(1)
      IF (OXLT .LE. 0.0) THEN
        OXLT = 0.01
      ENDIF

      OXFAC    = 1.0/(BD1*OXLT*1.0E-01)

      IMMOBIL  = .FALSE.
      AKILL  = .FALSE.
      ALGFIX = 0.0

      ! Defaults to daily output
      DailyCalc = .TRUE.

      ! Initialize Algal Activity
      FOCI   = OC(1) / 3.0
      FOCI   = AMIN1 (FOCI, 1.0)
      ALGACT = AMIN1 (0.2, FOCI)
      FNI    = 0.1

      ! Initial Buffer Power
      IBP    = 0

!     Flood variables (kg/ha) 
      TMPFUREA = DLTFUREA
      TMPFNO3  = DLTFNO3
      TMPFNH4  = DLTFNH4

!     Layer 1 variables (kg/ha)
      TMPUREA = UREA(1) + DLTUREA(1)
      TMPNO3  = SNO3(1) + DLTSNO3(1)
      TMPNH4  = SNH4(1) + DLTSNH4(1)

!     Oxidation layer variables (kg/ha)
!     check this - don't these have a value under dry conditions?  CHP
      TMPOXU  = TMPUREA * FAC1 / OXFAC
      TMPOXH4 = TMPNO3  * FAC1 / OXFAC
      TMPOXN3 = TMPNH4  * FAC1 / OXFAC

      IF (DailyCalc) THEN
         IHDAY = 1
       ELSE
         IHDAY = 12
      ENDIF

      CALL EQUIL2 (
     &    BD1, SURCEC, DLAYR, FLOOD, IHDAY, 1,            !Input
     &    OXLT, OXMIN3, OXMIN4, SW1, YRDOY, YRDRY,        !Input
     &    IBP, TMPFUREA, TMPOXU, TMPUREA)                 !I/O

      CALL EQUIL2 (
     &    BD1, SURCEC, DLAYR, FLOOD, IHDAY, 2,            !Input
     &    OXLT, OXMIN3, OXMIN4, SW1, YRDOY, YRDRY,        !Input
     &    IBP, TMPFNO3, TMPOXN3, TMPNO3)                  !I/O
      CALL EQUIL2 (
     &    BD1, SURCEC, DLAYR, FLOOD, IHDAY, 3,            !Input
     &    OXLT, OXMIN3, OXMIN4, SW1, YRDOY, YRDRY,        !Input
     &    IBP, TMPFNH4, TMPOXH4, TMPNH4)                  !I/O

!     Changes to urea, NO3 and NH4 due to flood initialization processes
      DLTUREA(1) = MAX(0.0, TMPUREA) - UREA(1)
      DLTSNO3(1) = MAX(0.0, TMPNO3)  - SNO3(1)
      DLTSNH4(1) = MAX(0.0, TMPNH4)  - SNH4(1)

      DLTFUREA = MAX(0.0, TMPFUREA)
      DLTFNO3  = MAX(0.0, TMPFNO3)
      DLTFNH4  = MAX(0.0, TMPFNH4)

      DLTOXU  = MAX(0.0, TMPOXU)
      DLTOXH4 = MAX(0.0, TMPOXH4)
      DLTOXN3 = MAX(0.0, TMPOXN3)

      RETURN
      END SUBROUTINE FLOODI

