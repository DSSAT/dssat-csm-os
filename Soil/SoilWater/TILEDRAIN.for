C=======================================================================
C  TILEDRAIN, Subroutine
C  Calculates tile drainage.
!-----------------------------------------------------------------------
C  REVISION HISTORY
C  03/21/2001 CHP Written based on WDB code.
C  10/17/2003 CHP Added to current model.
!-----------------------------------------------------------------------
!  Called by: WATBAL
!  Calls:     None
C=======================================================================

      SUBROUTINE TILEDRAIN(CONTROL, 
     &    DLAYR, DUL, ETDR, NLAYR, SAT, SW, SWDELTS,      !Input
     &    DRN, SWDELTT, TDFC, TDFD, TDLNO)                !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     
      IMPLICIT NONE
      EXTERNAL FIND, ERROR
      SAVE

      CHARACTER*6 ERRKEY, SECTION
      PARAMETER (ERRKEY = 'TILDR')
      CHARACTER*30 FILEIO 

      INTEGER DYNAMIC
      INTEGER LUNIO
      INTEGER L, NLAYR
      INTEGER ERRNUM, FOUND, LINC, LNUM, TOPSAT

      REAL CUMDEP, ETDR, HEAD, KTILE
      REAL TDFD, TDFC, FLDD, SFDRN
      REAL TDF_AVAIL, DRNTOT, EXCESS
      REAL TOTSWDELTT

      INTEGER TDLNO
      REAL DLAYR(NL), DRN(NL), DUL(NL), SAT(NL), SW(NL), 
     &        SWDELTS(NL), SWDELTT(NL)

      TYPE (ControlType) CONTROL
      DYNAMIC = CONTROL % DYNAMIC

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
C-----------------------------------------------------------------------
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO

C     Read FILEIO
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)
      LNUM = 0

C     Find and Read Fields Section.
      SECTION = '*FIELD'

      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ(LUNIO,'(38X,2F6.0)') FLDD, SFDRN; LNUM = LNUM + 1
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
      ENDIF

      CLOSE (LUNIO)

C     Skip tile drain section if depth of tile is < 0 in file X
      IF (FLDD .LE. 0.0) THEN
        TDLNO = -99

C     Find layer number for tile
      ELSE
        CUMDEP = DLAYR(1)
        DO L = 2, NLAYR
          CUMDEP = CUMDEP + DLAYR(L)
          IF (CUMDEP .GE. FLDD .AND. CUMDEP - DLAYR(L) .LT. FLDD) THEN
             TDLNO = L
          ENDIF
        ENDDO
      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      TDFC = 0.

!***********************************************************************
!***********************************************************************
!     Daily rate calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!----------------------------------------------------------------------
      SWDELTT = 0.0

C     Compute hydraulic head above drain defined as top-most saturated 
C     layer above the drain. All saturated layers above the drain must be 
C     continuous.  Compute soil water available to drain (water in
C     saturated zone in exceedance of drained upper limit).
      HEAD = 0.0
      TDF_AVAIL = 0.0
      DO L = TDLNO, 1, -1
        IF (SW(L) .GE. 0.98 * SAT(L)) THEN
          HEAD = HEAD + DLAYR(L)
          TDF_AVAIL = TDF_AVAIL + (SW(L) + SWDELTS(L) - DUL(L))*DLAYR(L)
          TOPSAT = L
        ELSE
!chp      Total head includes saturated portion of first unsaturated layer.
          HEAD = HEAD + (SW(L) - DUL(L)) / (SAT(L) - DUL(L)) * DLAYR(L)
          HEAD = MAX(HEAD, 0.)
          TDF_AVAIL = TDF_AVAIL + (SW(L) + SWDELTS(L) - DUL(L))*DLAYR(L)
          TOPSAT = L
          EXIT
        ENDIF
      ENDDO

!      KTILE = SWCN(TDLNO) * HEAD / (SFDRN * 100.)
      KTILE = ETDR * HEAD
      TDFD = 0.0

C     Drain water from tile layer if layer is saturated
      IF (HEAD .GT. 0.0) THEN
        TDFD = MIN(KTILE, TDF_AVAIL)

C       Redistribute water from upper layers
C       Assume that water is limited by user-specified tile drainage rate
C         rather than by each layer's Ksat.
        DRNTOT = 0.0
        TOTSWDELTT = 0.0
        DO L = TOPSAT, TDLNO  !top saturated layer down to tiledrain 
          IF (DRNTOT .LT. TDFD) THEN
            !Reduce soil water from top saturated layers until tile
            !  drainage capacity is met.
            SWDELTT(L) = -(SW(L) + SWDELTS(L) - DUL(L)) 
            DRNTOT = DRNTOT - SWDELTT(L) * DLAYR(L)
            DRN(L) = DRN(L) + DRNTOT
            IF (DRNTOT .GT. TDFD) THEN
              EXCESS = DRNTOT - TDFD
              SWDELTT(L) = SWDELTT(L) + EXCESS / DLAYR(L)
              DRN(L) = DRN(L) - EXCESS
            ENDIF
          ELSE
            !Lower soil layers will remain at saturation
            SWDELTT(L) = 0.0
            DRN(L) = DRN(L) + TDFD
          ENDIF

          TOTSWDELTT = TOTSWDELTT + SWDELTT(L) * DLAYR(L) 
        ENDDO 

        TDFC = TDFC + TDFD    !Cumulative
      ELSE
        TDFD = 0.0
      ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END !SUBROUTINE TILEDRAIN

C** WDB  Add tile drain flow
C** WDB  PTDFL - POTENTIAL TILE DRAIN FLOW, cm/day
C** WDB  KTILE  - Conductivity of tile layer, mm/day
C** WDB  TDFD - Cumulative Tile drain flow, cm/day
C** WDB  ATDFL - actual Tile drain flow, cm/day
C** WDB  FLDD = depth of tile
C** WDB  KTILE = conductivity of tile drain, cm/day
C** WDB  TDFC = Sum of tile drain flow from beginning of model run, cm
C** WDB  TDLNO = Layer number containing tile drain
C** WDB  FLDD = Depth of tile, from file X, cm
!     SWTILE = Soil water that drains to tile field today (mm/d)
