C=======================================================================
C  CALSHK, Subroutine
C
C  Determines rice shock period
C-----------------------------------------------------------------------
C  Revision history
C
C  04/01/1996 MUS Written
C  08/29/2002 CHP/MUS Converted to modular format for inclusion in CSM.
C  02/19/2003 CHP Converted dates to YRDOY format
C=======================================================================

      SUBROUTINE CALCSHK (DYNAMIC, 
     &    DTT, ISTAGE, ISWWAT, ITRANS, LTRANS,            !Input
     &    MODELVER, P1, P1T, SHOCKFAC, TAGE, TMAX,        !Input
     &    TMIN, YRDOY, YRSOW,                             !Input
     &    CARBO, CUMDTT,                                  !I/O
     &    TSHOCK)                                         !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT  NONE
      EXTERNAL INCDAT, TIMDIF
      SAVE

      REAL      TSHOCK,TSHOCK1,TMAX,TMIN,TAGE,DTT,CUMDTT,CARBO
      REAL      P1, P1T
      REAL      SHOCKAGE, SHOCK, SHOCKFAC, SHOCKD, SHOCKLAI, DSHOCK
      INTEGER   DYNAMIC, ISTAGE, ITRANS
      INTEGER   MODELVER
      INTEGER   INCDAT, TIMDIF, LYRDOY, YRDOY, YRSOW
      LOGICAL   LTRANS, SHKINIT
      CHARACTER ISWWAT*1

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      !
      ! Any further initializtion of shock should go here.  The shock
      ! submodel and value of SHOCKFAC is specified in the RICER960.SPE
      ! species file

      IF (ITRANS .EQ. 2 .OR. ITRANS .EQ. 3) THEN
        SHKINIT = .TRUE.
      ELSE
        SHKINIT = .FALSE.
      ENDIF

      TSHOCK = 1.0

!***********************************************************************
!***********************************************************************
!     Daily rate / integration calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
      IF (ISWWAT .NE. 'Y') THEN
        TSHOCK = 1.0
        RETURN
      ENDIF

      SELECT CASE (MODELVER)
        CASE (1)
         IF (ISTAGE .NE. 5) THEN
           !
           ! Allow 7-14 day sliding scale effect on partitioning
           ! for transplanting
           !
           TSHOCK  = 1.0
           TSHOCK1 = 1.0

           IF (LTRANS) THEN
              DSHOCK = 7.0*(P1/P1T)
!              LDATE  = ISOW + DSHOCK
              LYRDOY  = INCDAT(YRSOW, INT(DSHOCK))
!              IF (DOY .GE. ISOW .AND. DOY .LE. LDATE) THEN
              IF (YRDOY .GE. YRSOW .AND. YRDOY .LE. LYRDOY) THEN
!                 TSHOCK = 1.0-(LDATE-DOY)/DSHOCK
                 TSHOCK = 1.0 - TIMDIF(YRDOY, LYRDOY) / DSHOCK
                 IF (TMAX .GE. 32.0) THEN
                    TSHOCK  = TSHOCK*0.05*(45.0-TMAX)
                 ENDIF
                 IF (TMIN .GE. 28.0) THEN
                    TSHOCK1 = TSHOCK*0.05*(40.0-TMIN)
                 ENDIF
                 TSHOCK = AMIN1 (TSHOCK,TSHOCK1)
                 TSHOCK = AMAX1 (TSHOCK,0.0)
              ENDIF
           ENDIF
        END IF

        IF (TAGE .LE. 10.0) THEN
           TSHOCK = 1.0
        ENDIF

       CASE (2)
         !
         ! MUS shock calculations
         !
         IF (SHKINIT) THEN
           !
           ! Calculate shock period in DTT
           !
           ! Function derived from unpublished data of MUS
           !
           ! Calculate CUMDTT from XFILE data and estimate DTT
           ! accumulated during nursery development (for ITRANS = 2).
           ! For ITRANS = 3, model simulation of nursery growth, DTTSUM
           ! is accumulated from actual weather
           !
           SHOCKAGE = -0.62695 + 0.105389*CUMDTT + 0.000263*CUMDTT**2-
     &                 0.00000015*CUMDTT**3
           !
           ! Shock calculated as a function of SHOCKAGE and SHOCKFAC
           !
           ! SHOCKFAC = 0.00, no damage on seedlings
           !            1.00, moderate damage on seedlings
           !            1.41, severe damage on seedlings
           !
           SHOCK    = SHOCKAGE*SHOCKFAC
           !
           ! Decrement shock by the DTT for today (todays heatsum)
           !
           SHOCK    = SHOCK - DTT
           !
           ! Flag inidcating that shock has been initialized
           ! Only initialize once, on day of transplanting
           !
           SHKINIT  = .FALSE.
           !
           ! Calculate approximate days of shock based on today
           !
           SHOCKD   = SHOCK/(((TMAX+TMIN)/2)-8.0)
           !
           ! Estimate the Last DATE of shock effect
           !
!           LDATE    = ISOW + INT (SHOCKD + 0.5)
           LYRDOY    = INCDAT(YRSOW, INT(SHOCKD + 0.5))
           !
! Determining the stopage period (SHOCKLAID) of LAI during TS period
           !
           SHOCKLAI = 0.25*SHOCKAGE
C          SHOCKLAID = SHOCKLAI/(((TMAX+TMIN)/2)-8.0)

           TSHOCK  = 1.0
           CUMDTT  = CUMDTT - SHOCK
         ELSE
           !
           ! Decrement shock value by todays DTT
           !
           SHOCK    = AMAX1 (SHOCK - DTT, 0.0)
           TSHOCK   = 1.0
           !
           ! Decrement LAI shock value by todays DTT
           !
           SHOCKLAI  = AMAX1 (SHOCKLAI - DTT, 0.0)
         ENDIF
         IF (SHOCK .GT. 0.0) THEN
             TSHOCK  = 1.0
             TSHOCK1 = 1.0
             !
             ! Calculate TSHOCK factor for today, on a sliding scale
             !
!             TSHOCK = 1.0-(LDATE-DOY)/SHOCKD
             TSHOCK = 1.0 - TIMDIF(YRDOY, LYRDOY) / SHOCKD
             IF (TMAX .GE. 32.0) THEN
                 TSHOCK  = TSHOCK*0.05*(45.0-TMAX)
             ENDIF
             IF (TMIN .GE. 28.0) THEN
                 TSHOCK1 = TSHOCK*0.05*(40.0-TMIN)
             ENDIF
             TSHOCK = AMIN1 (TSHOCK,TSHOCK1)
             TSHOCK = AMAX1 (TSHOCK,0.0)
         ENDIF
         !
         ! If during LAI shock period, set CARBO for today = 0.0
         !
         IF (SHOCKLAI .GT. 0.0) THEN
            CARBO = 0.0
         ENDIF
      END SELECT

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE CALCSHK

