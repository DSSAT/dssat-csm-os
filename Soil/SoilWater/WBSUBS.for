C=======================================================================
C  SNOWFALL, Subroutine
C  Determines snow melt.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  12/05/1993 NBP Written
C  01/07/1992 AG  Written
C  01/07/1993 PWW Header revision and minor changes
C  02/10/1993 PWW Took out common blocks -- pass paramaters
C  07/10/1996 GH  Modified to add snow to rain
C  10/20/1997 CHP Modified for modular format
C  09/01/1999 GH  Incorporated into CROPGRO
C  01/13/2000 NBP Made SWFACS initialize--called from WATBAL or ETPHOT
C  06/11/2002 GH  Modified for Y2K
!-----------------------------------------------------------------------
C  Called by: WATBAL
C  Calls    : None
C=======================================================================
      SUBROUTINE SNOWFALL (DYNAMIC,
     &    TMAX, RAIN,                                     !Input
     &    SNOW, WATAVL)                                   !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      INTEGER DYNAMIC
      REAL TMAX, RAIN, SNOW, WATAVL, SNOMLT

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      SNOW = 0.0
!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
      SNOMLT = 0.0
      IF (TMAX  .GT. 1.0) THEN
         SNOMLT = TMAX + RAIN*0.4
         IF (SNOMLT .GT. SNOW) THEN
             SNOMLT = SNOW
         ENDIF

         SNOW   = SNOW - SNOMLT
         WATAVL = RAIN + SNOMLT
      ELSE
         SNOW   = SNOW   + RAIN
         WATAVL = 0.0
      ENDIF

      if(snow.lt.0.001) snow = 0

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE SNOWFALL
!-----------------------------------------------------------------------
!     SNOWFALL VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! RAIN    Precipitation depth for current day (mm)
! SNOMLT  Daily Snowmelt (mm/d)
! SNOW    Snow accumulation (mm)
! TMAX    Maximum daily temperature (°C)
! WATAVL  Water available for infiltration or runoff (rainfall plus 
!           irrigation) (mm/d)
!-----------------------------------------------------------------------
!     END SNOWFALL Subroutine
C=======================================================================


C=======================================================================
C  IPWBAL, Subroutine, C. H. Porter
C-----------------------------------------------------------------------
C  Reads input variables for soil-water routine.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  10/01/1997 CHP Written.
C  08/12/2003 CHP Added I/O error checking
!  6/14/2005  CHP Use RNMODE as indicator of sequence mode.
!-----------------------------------------------------------------------
!  Called by: WATBAL
!  Calls    : ERROR, FIND
!=======================================================================
      SUBROUTINE IPWBAL (CONTROL, LL, NLAYR,              !Input
     &    SW, ActWTD)                                     !Output

!     2023-01-26 chp removed unused variables in argument list:
!       DLAYR, SAT,

!-----------------------------------------------------------------------
      USE ModuleData
      IMPLICIT NONE
      EXTERNAL FIND, ERROR
      SAVE

!     REAL, DIMENSION(NL), INTENT(IN) :: DLAYR, LL, SAT
      REAL, DIMENSION(NL), INTENT(IN) :: LL
      INTEGER, INTENT(IN) :: NLAYR
      REAL, DIMENSION(NL), INTENT(OUT) :: SW
      REAL, INTENT(OUT) :: ActWTD

      INTEGER DYNAMIC
      INTEGER LUNIO
      CHARACTER*6 ERRKEY
      PARAMETER (ERRKEY = 'IPWBAL')

      CHARACTER*1  RNMODE !, MESIC
      CHARACTER*6  SECTION
      CHARACTER*30 FILEIO

      INTEGER ERRNUM, FOUND, L, LINC, LNUM, RUN

      REAL, DIMENSION(NL) :: SW_INIT
      REAL ICWD, ICWD_INIT, SWAD  !, SWEF

!     The variable "CONTROL" is of constructed type "ControlType" as 
!     defined in ModuleDefs.for, and contains the following variables.
!     The components are copied into local variables for use here.
      TYPE (ControlType) CONTROL
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO
!      MESIC   = CONTROL % MESIC
      RNMODE  = CONTROL % RNMODE
      RUN     = CONTROL % RUN

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
C     Find and Read Initial Conditions Section
      IF (INDEX('FQ',RNMODE) .LE. 0 .OR. RUN == 1) THEN

        OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)
        LNUM = 0

        SECTION = '*INITI'
        CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILEIO, LNUM)
        ELSE
          READ(LUNIO,'(40X,F6.0)',IOSTAT=ERRNUM) ICWD ; LNUM = LNUM + 1
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
          IF (ICWD .LT. 0.0) THEN
            ICWD = 9999.
          ENDIF
          ActWTD = ICWD

          DO L = 1, NLAYR
            READ(LUNIO,'(9X,F5.3)',IOSTAT=ERRNUM) SW(L)
            LNUM = LNUM + 1
            IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)

            IF (SW(L) .LT. LL(L)) THEN
              IF (L == 1) THEN
!               Layer 1 - check for SW < air dry
                SWAD = 0.30 * LL(L)
                IF (SW(L) < SWAD) THEN
                  SW(L) = SWAD
                ENDIF
              ELSE
!               Layers 2 thru NLAYR
                SW(L) = LL(L)
              ENDIF
            ENDIF
          ENDDO

        ENDIF
      ENDIF

      SW_INIT   = SW
      ICWD_INIT = ICWD
      CALL PUT('MGMT','ICWD',ICWD)

      CLOSE (LUNIO)

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
!      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)
!      SECTION = '*INITI'
!      CALL FIND(LUNIO, SECTION, LNUM, FOUND)
!      IF (FOUND .EQ. 0) THEN
!        CALL ERROR(SECTION, 42, FILEIO, LNUM)
!      ELSE
!        READ(LUNIO,'(40X,F6.0)', IOSTAT=ERRNUM) ICWD ; LNUM = LNUM + 1
!        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
!        ActWTD = ICWD
!
!        DO L = 1, NLAYR
!          READ(LUNIO,'(9X,F5.3)',IOSTAT=ERRNUM) SW(L)
!          LNUM = LNUM + 1
!          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
!        ENDDO
!      ENDIF
!      CLOSE (LUNIO)

!!     Limit initial water content to wilting point
!      DO L = 2, NLAYR
!        IF (SW(L) .LT. LL(L)) SW(L) = LL(L)
!      ENDDO

!     CHP - don't restrict SW in top layer.
!!     Limit top soil layer to air dry water content
!      SWEF = 0.9-0.00038*(DLAYR(1)-30.)**2
!      IF (SW(1) .LT. SWEF * LL(1)) SW(1) = SWEF * LL(1)

      SW   = SW_INIT   
      ICWD = ICWD_INIT  
      ActWTD = ICWD
      CALL PUT('MGMT','ICWD',ICWD)
      CALL PUT('MGMT','WATTAB',ActWTD)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE IPWBAL

!-----------------------------------------------------------------------
!     IPWBAL VARIABLE DEFINITIONS:   updated 2/19/2004
!-----------------------------------------------------------------------
! CROP    Crop identification code 
! ERRNUM  Error number for input 
! FILEIO  Filename for input file (e.g., IBSNAT35.INP) 
! FOUND   Indicator that good data was read from file by subroutine FIND (0 
!           - End-of-file encountered, 1 - NAME was found) 
! ICWD    Initial water table depth (cm)
! LINC    Line number of input file 
! LL(L)   Volumetric soil water content in soil layer L at lower limit
!          (cm3 [water] / cm3 [soil])
! LNUM    Current line number of input file 
! LUNIO   Logical unit number for FILEIO 
! NLAYR   Actual number of soil layers 
! SECTION Section name in input file 
! SW(L)   Volumetric soil water content in layer L
!          (cm3 [water] / cm3 [soil])
! ActWTD  Depth to water table (cm)
!-----------------------------------------------------------------------
!     END IPWBAL Subroutine
C=======================================================================


C=======================================================================
C  UP_FLOW, Subroutine, J.T. Ritchie
C  Calculates upward movement of water due to evaporation.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1989 JR  Written
C  12/05/1993 NBP Made into subroutine.
C  04/29/1995 NBP Changed UPFLOW to array in UP_FLOW.  Passed back to NFLUX.
C  10/13/1997 CHP Modified for modular format
C  09/01/1999 GH  Incorporated into CROPGRO
!  06/21/2005 CHP Changed variable FLOW to UPFLOW, subroutine name from
!                 UPFLOW to UP_FLOW.
!-----------------------------------------------------------------------
!  Called by: WATBAL
!  Calls:     None
C=======================================================================
      SUBROUTINE UP_FLOW(
     &    NLAYR, DLAYR, DUL, LL, SAT, SW, SW_AVAIL,       !Input
     &    UPFLOW, SWDELTU)                                !Output

!     ------------------------------------------------------------------
      USE ModuleDefs   
      IMPLICIT NONE
      SAVE

      INTEGER NLAYR
      INTEGER IST, L, M
      REAL DBAR, GRAD, FLOWFIX, SWOLD, THET1, THET2
      REAL DLAYR(NL), DUL(NL), ESW(NL), UPFLOW(NL), LL(NL)
      REAL SAT(NL), SW(NL), SWDELTU(NL), SWTEMP(NL)
      REAL SW_AVAIL(NL), SW_INF(NL)

!-----------------------------------------------------------------------
      DO L = 1, NLAYR
        UPFLOW(L) = 0.0
        SWDELTU(L) = 0.0
        SWTEMP(L) = SW(L)
!       Calculated flow will be limited by SW_INF and SW_AVAIL
        SW_INF(L) = SW_AVAIL(L)    !Includes saturated flow
        SW_AVAIL(L) = MAX(0.0, SW_AVAIL(L) - LL(L))
        ESW(L) = DUL(L) - LL(L)
      ENDDO

      IF (DLAYR(1) .GE. 5.0) THEN
        IST = 1
      ELSE
        IST = 2
      ENDIF

!     Subtract evaporation from top soil layer.
!     This has already been done in integration section yesterday.
!      SWTEMP(1) = SWTEMP(1) - 0.1 * ES / DLAYR(1)
!      SW_AVAIL(1) = SW_AVAIL(1) - 0.1 * ES / DLAYR(1)
!      SW_AVAIL(1) = MAX(0.0, SW_AVAIL(1))
!      SW_INF(1) = SW_AVAIL(1)

      DO L = IST,NLAYR-1
        M = L + 1
        SWOLD = SWTEMP(L)

        THET1 = MIN(SWTEMP(L) - LL(L), ESW(L))
        THET1 = MAX(0.0,THET1)

        THET2 = MIN(SWTEMP(M) - LL(M), ESW(M))
        THET2 = MAX(0.0, THET2)

        DBAR = 0.88 * EXP(35.4*((THET1*DLAYR(L) + THET2*DLAYR(M))/
     &    (DLAYR(L) + DLAYR(M)))*0.5)
        DBAR = MIN(DBAR, 100.0)

!     Need to calculate ESW where used. CHP 10/15/01
        GRAD = (THET2/ESW(M) - THET1/ESW(L)) * (ESW(M) * DLAYR(M) +
     &    ESW(L)*DLAYR(L)) / (DLAYR(M) + DLAYR(L))

        UPFLOW(L) = DBAR * GRAD / ((DLAYR(L) + DLAYR(M)) * 0.5)

!-----------------------------------------------------------------------
!      Upward flow from layer M to layer L
        IF (UPFLOW(L) .GT. 0.0) THEN
          IF (SWTEMP(L) .LE. DUL(L)) THEN
            SWTEMP(L) = SWTEMP(L) + UPFLOW(L) / DLAYR(L)
            SW_INF(L) = SW_INF(L) + UPFLOW(L) / DLAYR(L)
            IF (SWTEMP(L) .GT. DUL(L) .OR. SW_INF(L) .GT. SAT(L)) THEN
              FLOWFIX = MAX(0.0, (SWTEMP(L) - DUL(L)) * DLAYR(L), 
     &                           (SW_INF(L) - SAT(L)) * DLAYR(L))
              FLOWFIX = MIN(UPFLOW(L), FLOWFIX)
              UPFLOW(L) = UPFLOW(L) - FLOWFIX
              SWTEMP(L) = SWOLD + UPFLOW(L) / DLAYR(L)
            ENDIF
          ELSE    !No upward flow if SWTEMP > DUL
            UPFLOW(L) = 0.
          ENDIF

          IF (UPFLOW(L) / DLAYR(M) .GT. SW_AVAIL(M)) THEN
            UPFLOW(L) = SW_AVAIL(M) * DLAYR(M)
            SWTEMP(L) = SWOLD + UPFLOW(L) / DLAYR(L)
          ENDIF
          SWTEMP(M) = SWTEMP(M) - UPFLOW(L) / DLAYR(M)

!-----------------------------------------------------------------------
!       Downward flow from layer L to layer M
        ELSE IF (UPFLOW(L) .LT. 0.0) THEN
          IF (SWTEMP(L) .GE. LL(L)) THEN
            IF (ABS(UPFLOW(L) / DLAYR(L)) .GT. SW_AVAIL(L)) THEN
              UPFLOW(L) = - SW_AVAIL(L) * DLAYR(L)
            ENDIF
              
            SWTEMP(L) = SWTEMP(L) + UPFLOW(L) / DLAYR(L)
            SWTEMP(M) = SWTEMP(M) - UPFLOW(L) / DLAYR(M)
            SW_INF(M) = SW_INF(M) - UPFLOW(L) / DLAYR(M)

            IF (SW_INF(M) .GT. SAT(M)) THEN
              FLOWFIX = MIN(ABS(UPFLOW(L)), 
     &                     (SW_INF(M) - SAT(M)) * DLAYR(M))
              UPFLOW(L) = UPFLOW(L) + FLOWFIX
              SWTEMP(L) = SWOLD + UPFLOW(L) / DLAYR(L)
              SWTEMP(M) = SWTEMP(M) - FLOWFIX / DLAYR(M)
            ENDIF
          ELSE    !No downward flow if SWTEMP < LL
            UPFLOW(L) = 0.
          ENDIF
        ENDIF
      ENDDO

      DO L = 1, NLAYR
        SWDELTU(L) = SWTEMP(L) - SW(L)
      ENDDO

      RETURN
      END SUBROUTINE UP_FLOW
!-----------------------------------------------------------------------
!     UP_FLOW VARIABLE DEFINITIONS: updated 2/19/2004
!-----------------------------------------------------------------------
! DBAR         
! DLAYR(L)    Thickness of soil layer L (cm)
! DUL(L)      Volumetric soil water content at Drained Upper Limit in soil 
!               layer L (cm3[water]/cm3[soil])
! ESW(L)      Plant extractable soil water by layer (= DUL - LL)
!              (cm3[water]/cm3[soil])
! FLOWFIX     Adjustment amount for upward flow calculations to prevent a 
!               soil layer from exceeding the saturation content (cm3/cm3)
! GRAD         
! IST         Beginning soil layer for upward flow calculations (=1 for 
!               layers 1 through 5, =2 for lower layers) 
! LL(L)       Volumetric soil water content in soil layer L at lower limit
!              (cm3 [water] / cm3 [soil])
! NLAYR       Actual number of soil layers 
! SAT(L)      Volumetric soil water content in layer L at saturation
!              (cm3 [water] / cm3 [soil])
! SW(L)       Volumetric soil water content in layer L
!              (cm3 [water] / cm3 [soil])
! SW_AVAIL(L) Soil water content in layer L available for evaporation, 
!               plant extraction, or movement through soil
!               (cm3 [water] / cm3 [soil])
! SW_INF(L)   Soil water content in layer L including computed upward flow
!              (cm3 [water] / cm3 [soil])
! SWDELTU(L)  Change in soil water content due to evaporation and/or upward 
!               flow in layer L (cm3 [water] / cm3 [soil])
! SWOLD       Previous soil water content prior to capillary flow from 
!               layer (cm3/cm3)
! SWTEMP(L)   Soil water content in layer L (temporary value to be modified 
!               based on drainage, root uptake and upward flow through soil 
!               layers). (cm3/cm3)
! THET1       Soil water content above the lower limit (LL) for an upper 
!               layer of soil for water flow from a lower layer (cm/cm)
! THET2       Soil water content above the lower limit (LL) for a lower 
!               layer of soil for water flow into an upper layer (cm/cm)
! UPFLOW(L)   Movement of water between unsaturated soil layers due to soil 
!               evaporation: + = upward, -- = downward (cm/d)
!-----------------------------------------------------------------------
!     END SUBROUTINE UP_FLOW
C=======================================================================


C=======================================================================
C  WBSUM, Subroutine, J.T. Ritchie
C  Performs daily summation of water balance variables.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/89 JR  Written
C  12/05/93 NBP Made into subroutine
!  10/18/97 CHP Modified for modular format.
!-----------------------------------------------------------------------
!  Called by: WATBAL
!  Calls:     None
C=======================================================================
      SUBROUTINE WBSUM(DYNAMIC,
     &    NLAYR, DRAIN, RAIN, RUNOFF, DLAYR, SW,          !Input
     &    CRAIN, TDRAIN, TRUNOF, TSW, TSWINI)             !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      INTEGER DYNAMIC, L, NLAYR
      REAL CRAIN, DRAIN, RAIN, RUNOFF
      REAL TDRAIN, TRUNOF, TSW, TSWINI
      REAL DLAYR(NL), SW(NL)

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      CRAIN  = 0.
      TDRAIN = 0.
      TRUNOF = 0.
      TSWINI = 0.

      DO L = 1,NLAYR
        TSWINI = TSWINI + SW(L) * DLAYR(L)
      ENDDO
      TSW = TSWINI

!***********************************************************************
!     DAILY INTEGRATION 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
      TSW = 0.0
      DO L = 1, NLAYR
        TSW = TSW + SW(L) * DLAYR(L)
      ENDDO

!-----------------------------------------------------------------------
C     Increment summation variables.
!-----------------------------------------------------------------------
      CRAIN  = CRAIN  + RAIN
      TDRAIN = TDRAIN + DRAIN
      TRUNOF = TRUNOF + RUNOFF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE WBSUM

!-----------------------------------------------------------------------
!     WBSUM VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! CRAIN     Cumulative precipitation (mm)
! DLAYR(L)  Soil thickness in layer L (cm)
! DRAIN     Drainage rate from soil profile (mm/d)
! NL        Maximum number of soil layers = 20 
! NLAYR     Actual number of soil layers 
! RAIN      Precipitation depth for current day (mm)
! RUNOFF    Calculated runoff (mm/d)
! SW(L)     Volumetric soil water content in layer L
!             (cm3 [water] / cm3 [soil])
! TDRAIN    Cumulative daily drainage from profile (mm)
! TRUNOF    Cumulative runoff (mm)
! TSW       Total soil water in profile (cm)
! TSWINI    Initial soil water content (cm)
!-----------------------------------------------------------------------
!     END SUBROUTINE WBSUM
C=======================================================================


C=======================================================================
C  WTDEPT, Subroutine
C  Determines water table depth
!  Allows perched water table (uses highest free water surface in profile)
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/06/1997 GH  Written
!  10/20/1997 CHP Modified for modular format.
!  03/17/2001 CHP Allows water table between top and bottom of layer
!                 (no longer a step function).  Uses highest free water
!                 surface.
!  05/23/2007 CHP Start at bottom of profile and determine 1st unsaturated
!                   layer.  Add factor to decrease step function appearance.
C-----------------------------------------------------------------------
C  Called by: WATBAL
C  Calls    : None
C=======================================================================
      SUBROUTINE WTDEPT(
     &    NLAYR, DLAYR, DS, DUL, SAT, SW,                 !Input
     &    WTDEP)                                          !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.

      IMPLICIT NONE

      INTEGER L, NLAYR
      REAL  WTDEP             !Depth to Water table (cm)
      REAL SATFRAC(NL), FACTOR
      REAL DLAYR(NL), DS(NL), DUL(NL), SAT(NL), SW(NL)
      REAL, PARAMETER :: TOL = 0.95

!-----------------------------------------------------------------------
      DO L = NLAYR, 1, -1
        SATFRAC(L) = (SW(L) - DUL(L)) / (SAT(L) - DUL(L))
        SATFRAC(L) = MIN(MAX(0.0, SATFRAC(L)), 1.0)
        IF (SATFRAC(L) > TOL) THEN
!         Layer is saturated, continue up to next layer
          CYCLE
        ELSEIF (L == NLAYR) THEN
!         Bottom layer is unsaturated
          WTDEP = DS(NLAYR) - DLAYR(NLAYR) * SATFRAC(NLAYR)
          EXIT
        ELSE
!         Layer is unsaturated.  Interpolate water table depth.
!         FACTOR prevents a step function when transitioning between
!           layers.
          FACTOR = MIN(MAX(0.0, (SATFRAC(L+1) - TOL) / (1.0 - TOL)),1.0)
          WTDEP = DS(L) - DLAYR(L) * SATFRAC(L) * FACTOR
          EXIT
        ENDIF
      ENDDO

!      IF (L > 0 .AND. L <= NLAYR) THEN
!        WRITE(6500,'(I4,9F8.3,3F8.2)') L, SW(L), SW(L+1), SAT(L), 
!     &          SAT(L+1), DUL(L), DUL(L+1), SATFRAC(L), SATFRAC(L+1), 
!     &          FACTOR, DLAYR(L), DS(L), WTDEP
!      ELSE
!        WRITE(6500,'(I4,9F8.3,3F8.2)') L, SW(NLAYR), 0.0, SAT(NLAYR), 
!     &          0.0, DUL(NLAYR), 0.0, 0.0, 0.0, 
!     &          0.0, DLAYR(NLAYR), DS(NLAYR), WTDEP
!      ENDIF

      RETURN
      END SUBROUTINE WTDEPT

!-----------------------------------------------------------------------
!     WTDEPT VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! DLAYR(L) Soil thickness in layer L (cm)
! DS(L)    Cumulative depth in soil layer L (cm)
! NL       Maximum number of soil layers = 20 
! NLAYR    Actual number of soil layers 
! SAT(L)   Volumetric soil water content in layer L at saturation
!            (cm3 [water] / cm3 [soil])
! SW(L)    Volumetric soil water content in layer L (cm3[water]/cm3[soil])
! WTDEP    Water table depth  (cm)
! SATFRAC   Fraction of layer L which is saturated
!-----------------------------------------------------------------------
!     END SUBROUTINE WTDEPT
C=======================================================================


C=======================================================================
!     END WATBAL MODULE
!=======================================================================

