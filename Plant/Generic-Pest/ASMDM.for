C=======================================================================
C  ASMDM, Subroutine
C  Calculates assimilative damage due to pests.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  03/20/98 CHP Written based on assimilative damage calculations in
C               PLANT subroutine.
C  01/13/98 GH  Incorporated into CROPGRO
C-----------------------------------------------------------------------
C  Called by: PEST
C  Calls:     None
C=======================================================================
      SUBROUTINE ASMDM(
     &    PGAVL, PPSR, TPSR,                              !Input
     &    ASMDOT, CASM,                                   !Output
     &    DYNAMIC)                                        !Control

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      INTEGER DYNAMIC

      REAL ASMDOT, CASM
      REAL PGAVL, PPSR, TPSR

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
C      Initialize cumulative assimilative damage variable.
C-----------------------------------------------------------------------
      CASM = 0.0
      ASMDOT = 0.0

!***********************************************************************
!***********************************************************************
!     Daily integration
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!***********************************************************************
      ASMDOT = 0.0

      IF (TPSR .GT. PGAVL) THEN
        TPSR = MAX(0.,PGAVL)
      ELSE
        TPSR = MAX(0.,TPSR)
      ENDIF

      IF (PPSR .GT. 100.) THEN
        PPSR = 100.
      ELSE
        PPSR = MAX(0.,PPSR)
      ENDIF

      TPSR = TPSR + PPSR*PGAVL/100.
      IF (TPSR .GT. PGAVL) THEN
        TPSR = MAX(0.,PGAVL)
      ELSE
        TPSR = MAX(0.,TPSR)
      ENDIF

      ASMDOT = ASMDOT + TPSR
      CASM = CASM + TPSR

!***********************************************************************
      ENDIF
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      RETURN
      END   ! SUBROUTINE ASMDM

C-----------------------------------------------------------------------
C     Variable definitions
C-----------------------------------------------------------------------
! ASMDOT  Daily assimilative damage (g[CH2O] /m2 / d)
! CASM    Cumulative assimilate damage (g[CH2O] / m2)
! PGAVL   Total available CH2O available for growth & respiration
!           (g[CH2O] / m2)
! PPSR    Assimilate damage in daily percent photosynthesis reduction (%/d)
! TPSR    Daily absolute assimilate damage (g[CH2O]/m2/d)
C-----------------------------------------------------------------------
C     End Subroutine ASMDM
C-----------------------------------------------------------------------


