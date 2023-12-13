C=======================================================================
C  MULCHWATER, Subroutine, A. A. Andales
C  Calculates effect of surface residue on rainfall interception and
!     soil evaporation
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  08/27/1998 AAA (A. Andales) Wrote TILLRAIN.
C  03/08/2001 CHP moved TILLRAIN to WATBAL module.
C  05/22/2003 CHP Added mulch routine to current model.
C  12/11/2021 FO  Initialized local variables for MULCHWAT Integr.
!  01/26/2023 CHP Reduce compile warnings: add EXTERNAL stmts, remove 
!                 unused variables, shorten lines. 
C=======================================================================

      SUBROUTINE MULCHWATER(CONTROL, ISWITCH,
     &    WATAVL, MULCH)

C-----------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE
      EXTERNAL OPMULCH
      SAVE

      CHARACTER*1 RNMODE, MEINF
      INTEGER RUN, DYNAMIC

      REAL CNRAIN, NRAIN, WATAVL
      REAL DEFICIT
      REAL MULCHCOVER, MULCHEVAP, MULCHMASS, MULCHSAT, MULCHWAT
      REAL MULWATADD, MULWATAVL, MULCHMASS_Y, RESWATADD, WATFAC

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (MulchType)   MULCH    !Surface mulch characteristics

      DYNAMIC = CONTROL % DYNAMIC
      MEINF = ISWITCH % MEINF

!     Mulch variables computed elsewhere
      MULCHMASS  = MULCH % MULCHMASS
      MULCHCOVER = MULCH % MULCHCOVER
      MULCHEVAP  = MULCH % MULCHEVAP
      WATFAC     = MULCH % MUL_WATFAC

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      CNRAIN    = 0.0
      NRAIN     = 0.0
      MULCHWAT  = 0.0
      MULWATADD = 0.0
      RESWATADD = 0.0
      MULCHEVAP = 0.0

      !Mulch water balance not simulated
!     IF (INDEX('RSN',MEINF) .GT. 0) THEN
      IF (INDEX('RSM',MEINF) <= 0) THEN
        MULCH % MULCHWAT = 0.0
        MULCH % MULCHSAT = 0.0
        RETURN

      ELSE
!       Initialize MULCHWAT.
        RUN    = CONTROL % RUN
        RNMODE = CONTROL % RNMODE
        IF (RUN .EQ. 1 .OR. INDEX('QF',RNMODE) .LE. 0) THEN
          IF (MULCHMASS .GT. 0.) THEN
!           MULCHSAT = 3.8E-5 * MULCHMASS * 10.0
            MULCHSAT = WATFAC * 1.E-4 * MULCHMASS
!           Soil.MULCHSPONGE = 0.0000385 * Soil.MULCHDW
            MULCHWAT = MULCHSAT / 2.0
!           Soil.MULCHWAT = 0.1 * Soil.MULCHSPONGE
          ELSE
            MULCHWAT = 0.0
            MULCHSAT = 0.0
          ENDIF
        ENDIF

        MULCHMASS_Y = MULCHMASS

!       Mulch variables computed here.
        MULCH % MULCHWAT = MULCHWAT
        MULCH % MULCHSAT = MULCHSAT
        MULCH % NEWMULCH = 0.
        MULCH % NEWMULCHWAT = 0.

        CALL OPMULCH(CONTROL, ISWITCH, MULCH)
!     &  MULWATADD, RESWATADD)
      ENDIF

!***********************************************************************
!***********************************************************************
!     Daily rate calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
C  Calculate rainfall interception by surface residue
C-----------------------------------------------------------------------
!     IF (INDEX('RSN',MEINF) .GT. 0) THEN
      IF (INDEX('RSM',MEINF) <= 0) RETURN

!     MULCHSAT = 3.8E-5 * MULCHMASS * 10.0
      MULCHSAT = WATFAC * 1.E-4 * MULCHMASS  !mm H2O

!     Volumetric content of new residue is half of saturation content
      IF (MULCH % NEWMULCH > 1.E-6) THEN
        RESWATADD = 0.5 * WATFAC * 1.E-4 * MULCH % NEWMULCH
        MULCH % NEWMULCH = 0.0
        MULCH % NEWMULCHWAT = RESWATADD
      ELSE
        RESWATADD = 0.0
        MULCH % NEWMULCH = 0.
        MULCH % NEWMULCHWAT = 0.
      ENDIF

      IF (MULCHMASS .GT. 0.01 .AND. WATAVL .GT. 0.0) THEN
!     This estimates today's mulch evaporation as 85% of the mulchwater
!       or no more than yesterday's evaporation.  This allows mulch water
!       to exceed saturation for short periods of time, but reduces
!       instabilities.
!
!        added       sat.    ( exist.    new mulch
!        water  =   water  -   water   +   water  )
        DEFICIT = MULCHSAT - (MULCHWAT + RESWATADD)
!                         +        anticipated evap.
     &                    + MIN(MULCHWAT*0.85, MULCHEVAP)

!       This one is more stable, but allows MULCHWAT to stay above
!         MULCHSAT for longer periods:
!        DEFICIT = MULCHSAT - MULCHWAT + MIN(MULCHWAT,MULCHSAT)*0.85
!       Same for this one:
!        DEFICIT = MULCHSAT - MULCHWAT + (MULCHWAT*0.85 + MULCHEVAP)/2.

!       Fraction of water available for infiltration will go to mulch
        MULWATAVL = WATAVL * MULCHCOVER
        MULWATADD = MAX(MIN(DEFICIT, MULWATAVL),0.)

!       Remaining water goes to soil.
        NRAIN = MAX(WATAVL - MULWATADD, 0.0)
        WATAVL = NRAIN

      ELSE
        MULWATADD = 0.
        MULWATAVL = 0.
        NRAIN = WATAVL
      ENDIF

      MULCH % MULCHSAT = MULCHSAT

!***********************************************************************
!***********************************************************************
!     Daily integration
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
!     IF (INDEX('RSN',MEINF) .GT. 0) THEN
      IF (INDEX('RSM',MEINF) <= 0) RETURN

!     Update mulch water content
      MULCHWAT = MULCHWAT + MULWATADD + RESWATADD - MULCHEVAP
      IF (MULCHWAT < 1.E-4) MULCHWAT = 0.
      CNRAIN = CNRAIN + NRAIN

      MULCH % MULCHWAT = MULCHWAT

!***********************************************************************
!***********************************************************************
!     Daily and seasonal output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
!     IF (INDEX('RSN',MEINF) .GT. 0) THEN
      IF (INDEX('RSM',MEINF) <= 0) RETURN

      CALL OPMULCH(CONTROL, ISWITCH, MULCH)
!    &  MULWATADD, RESWATADD)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE MULCHWATER

C-----------------------------------------------------------------------
! MULCHWATER variables:
!--------------------
! CNRAIN    Cumulative net rainfall (rainfall which is available for runoff
!             and infiltration after absorbtion by mulch) (mm)
! DEFICIT   Amount by which the allowable minimum soil water content in top
!             layer exceeds the actual calculated soil water content (cm3/cm3)
! MULCHEVAP Water content subtracted from surface residue layer on current
!             day due to evaporation (mm/d)
! MULCHMASS  Residue material which is left on top of soil and not
!             incorporated (kg[residue]/ha)
! MULCHMASS_Y is yesterday's amount of mulch (kg/ha)
! MULCHSAT  Maximum water retention by surface residue (mm)
! MULCHWAT   Amount of water held by surface residue (mm)
! MULWATADD Water content added to surface residue layer on current day due
!             to rainfall (mm/d)
! NRAIN     Net rainfall after interception by surface residue (mm)
! WATAVL    Water available for infiltration or runoff (rainfall plus
!             irrigation) (mm/d)
C=======================================================================










