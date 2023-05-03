C=======================================================================
C  FLOOD_IRRIG, Subroutine
C
C  Determines irrigation - works with paddy flooding
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2. Header revision and minor changes           P.W.W.      8-7-93
C  05/29/2002 CHP Rewrote for modular CSM model
C  09/05/2003 CHP Added option for reported irrigation in DAP
!  01/22/2010 CHP remove water table management from here - not just
!                  for flooded fields.
C=======================================================================

      SUBROUTINE FLOOD_IRRIG (DYNAMIC, 
     &    BUND, COND, CONDAT, IBDAT, IIRRCV, IIRRI,       !Input
     &    IPDAT, IPERC, NBUND, NCOND, NPERC,              !Input
     &    PUDDLED, RAIN, SOILPROP, SW, YRDOY, YRPLT,      !Input
     &    FLOODWAT,                                       !I/O
     &    DEPIR)                                          !Output

      USE ModuleDefs
      USE FloodModule
      IMPLICIT NONE
      EXTERNAL INCDAT, SW_DEF, WARNING, ERROR
      SAVE

      CHARACTER*1 IIRRI
      CHARACTER*6, PARAMETER :: ERRKEY = "FIRRIG"
      CHARACTER*78, DIMENSION(3) :: MSG
      INTEGER  DYNAMIC, J,YRDOY, YRPLT
      REAL     DEPIR,TDSW,RAIN

      INTEGER INCDAT, NBUND, NCOND, NLAYR, NPERC, NPUD        !, NTBL
      INTEGER IBDAT(NAPPL), BUNDDAT(NAPPL), IIRRCV(NAPPL)
      INTEGER CONDAT(NAPPL), IRRDAT(NAPPL), PUDDAT(NAPPL), PUDAT(NAPPL)
!     INTEGER JULWTB(NAPPL), WTDAT(NAPPL)
      INTEGER IPDAT(NAPPL), PERCDAT(NAPPL)
      REAL ABUND, APWAT, EF
      REAL FLOOD, INFILT, PERC, PERMW, PUDPERC
      REAL BUND(NAPPL), COND(NAPPL), IPERC(NAPPL)
      REAL, DIMENSION(NL) :: DLAYR, SAT, SW
      LOGICAL BUNDED, PUDDLED, CONVERTED !, PERMF

      TYPE (FloodWatType) FLOODWAT
      TYPE (SOILTYPE) SOILPROP
      SAT   = SOILPROP % SAT
      DLAYR = SOILPROP % DLAYR
      NLAYR = SOILPROP % NLAYR

      INFILT= FLOODWAT % INFILT
      EF    = FLOODWAT % EF
      FLOOD = FLOODWAT % FLOOD
 
!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
      BUNDED  = .FALSE.
      APWAT   = 0.0
      ABUND   = 0.0
      PERC    = 0.0
      PERMW   = 0.0
      PUDPERC = 0.0
      CONVERTED = .FALSE.

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
C-----------------------------------------------------------------------
!     If irrigation events are reported as DAP, convert to YYYYDDD
!     Only need to do once after YRPLT has been set.
      IF (.NOT. CONVERTED) THEN
        IF (YRPLT .GT. 0 .AND. YRPLT .LT. 9999999) THEN

          !Convert percolation dates
          DO J = 1, NPERC
            IF (IIRRI .EQ. 'D') THEN
              PERCDAT(J) = INCDAT(YRPLT,IPDAT(J))
            ELSE
              PERCDAT(J) = IPDAT(J)
            ENDIF
          ENDDO

          !Convert bund height dates
          DO J = 1, NBUND
            IF (IIRRI .EQ. 'D') THEN
              BUNDDAT(J) = INCDAT(YRPLT, IBDAT(J))
            ELSE
              BUNDDAT(J) = IBDAT(J)
            ENDIF
          ENDDO

          !Convert puddling dates
          DO J = 1, NPUD
            IF (IIRRI .EQ. 'D') THEN
              PUDAT(J) = INCDAT(YRPLT, PUDDAT(J))
            ELSE
              PUDAT(J) = PUDDAT(J)
            ENDIF
          ENDDO

          !Convert irrigation dates
          DO J = 1, NCOND
            IF (IIRRI .EQ. 'D') THEN    
              IRRDAT(J) = INCDAT(YRPLT, CONDAT(J))
            ELSE
              IRRDAT(J) = CONDAT(J)
            ENDIF
          ENDDO

!          Convert water table dates
!          DO K = 1, NTBL
!            IF (IIRRI .EQ. 'D') THEN
!              WTDAT(J) = INCDAT(YRPLT, JULWTB(K))
!            ELSE
!              WTDAT(J) = JULWTB(K)
!            ENDIF
!          ENDDO

          CONVERTED = .TRUE.
        ENDIF
      ENDIF

!     Puddling
      DO J = 1, NPUD
        IF (YRDOY .EQ. PUDAT(J)) THEN
          PUDDLED = .TRUE.
        ENDIF
      ENDDO

      !Get daily percolation rate
      DO J = 1, NPERC
        IF (YRDOY .EQ. PERCDAT(J)) THEN
          PERC = IPERC(J)
        ENDIF
      END DO

      IF (FLOOD .GT. 0.0) THEN
        PUDPERC = PERC*10.0
        IF (FLOOD .LT. PUDPERC) THEN
          PUDPERC = FLOOD
        ENDIF
      ELSE
        PUDPERC = 0.0
      ENDIF

      ! Identify the irrigation code (IIRR) & bund height (ABUND)
      DO J = 1, NBUND
        IF (YRDOY .EQ. BUNDDAT(J)) THEN
          PERMW  = 0.0
          ABUND  = BUND(J)      !mm
          IF (ABUND .GT. 0.0) THEN
            BUNDED = .TRUE.
          ELSE
            BUNDED = .FALSE.
          ENDIF
        ENDIF
      END DO

      IF (NCOND .GT. 0) THEN
      ! Identify irrigation events for today
        DO J = 1, NCOND
          IF (YRDOY .EQ. IRRDAT(J)) THEN

            PERMW  = 0.0

      !--------------------------------------------------
            SELECT CASE (IIRRCV(J))

           !--------------------------------------------------
            CASE (6)
              !Single irrigation to specified flood depth (mm) 
              !COND(J) represents final flood depth at end of day
              CALL SW_DEF(DLAYR, NLAYR, SW, SAT, TDSW)
              DEPIR = COND(J) + TDSW - FLOOD - RAIN

              !Add drainage
              IF (PUDDLED) THEN
                DEPIR = DEPIR + PUDPERC     !today's
              ELSE
                DEPIR = DEPIR + INFILT      !yesterday's
              ENDIF

              !Add in yesterday's values of flood evaporation
              DEPIR = DEPIR + EF

           !--------------------------------------------------
            CASE (1:5)
              ! Field schedule for irrigation
                DEPIR = COND(J)

           !--------------------------------------------------
            CASE (11)
              !Irrigation to permanent or constant flood depth
              IF (COND(J) .GT. 0.0) THEN
                !Set permanent flood depth to COND(J)
                PERMW  = COND(J)
              ELSE 
                PERMW  = 0.0
              ENDIF

            END SELECT
      !--------------------------------------------------
          ENDIF
        END DO
      ENDIF

!     Every day check for permanent water level
      IF (PERMW. GT. 0.0) THEN
        IF (.NOT. PUDDLED) THEN
          MSG(1)=
     &      "Must set puddling and bund height to maintain a " // 
     &      "permanent flood pool."
          MSG(2)="IROP 11 must also include IROP 9 & 10."
          MSG(3)="Model will stop."
          CALL WARNING(3, ERRKEY, MSG)
          CALL ERROR(ERRKEY,1,"",0)
        ENDIF
        CALL SW_DEF(DLAYR, NLAYR, SW, SAT, TDSW)
        DEPIR = PERMW + TDSW - FLOOD - RAIN

        !Add drainage
        IF (PUDDLED) THEN
          DEPIR = DEPIR + PUDPERC     !today's
        ELSE
          DEPIR = DEPIR + INFILT      !yesterday's
        ENDIF

        !Add in yesterday's values of flood evaporation
        DEPIR = DEPIR + EF
      END IF

!     1/23/2010 CHP move water table entries to IRRIG
!      IF (NTBL .GT. 0) THEN
!        DO K = 1, NTBL
!          IF (YRDOY .EQ. WTDAT(J)) THEN
!            APWAT = PWAT(K)
!          ENDIF
!        END DO
!
!!       Don't change SW here - calculate an irrigation depth to add.
!        IF (APWAT .GT. 0.0) THEN
!          CUMDEP = 0.0
!          DO L = 1, NLAYR
!            IF (CUMDEP .GE. APWAT) THEN
!              DEPIR = DEPIR + SAT(L) - SW(L)  !CHP - CHECK!
!!              SW(L) = SAT(L)
!            ENDIF
!            CUMDEP = CUMDEP + DLAYR(L)
!          END DO
!        ENDIF
!      ENDIF

      DEPIR = MAX(0.0, DEPIR)

!***********************************************************************
      ENDIF
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************

      FLOODWAT % BUNDED  = BUNDED
      FLOODWAT % ABUND   = ABUND    !mm
      FLOODWAT % PUDPERC = PUDPERC
      FLOODWAT % PERC    = PERC
      
      RETURN
      END SUBROUTINE FLOOD_IRRIG


C=======================================================================
C  SW_DEF, Subroutine
C
C  Calculates soil water deficit.
C-----------------------------------------------------------------------
C  Revision history
C  04/18/2002 CHP Written.
C=======================================================================

      SUBROUTINE SW_DEF(DLAYR, NLAYR, SW, SAT, TDSW)

      Use ModuleDefs
      Implicit None
      
      INTEGER L, NLAYR
      REAL TDSW
      REAL, DIMENSION(NL) :: DLAYR, SAT, SW

      TDSW = 0.0
      DO L = 1, NLAYR
        TDSW = TDSW + (SAT(L) - SW(L)) * DLAYR(L) * 10.  !(in mm)
      END DO

      RETURN
      END SUBROUTINE SW_DEF

C=======================================================================
