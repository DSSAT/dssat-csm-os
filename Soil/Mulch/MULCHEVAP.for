C=======================================================================
C  MULCH_EVAP, Subroutine, C.H.Porter
C  Potential and actual evaporation from mulch layer
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  06/25/2008 CHP Written
C=======================================================================

      SUBROUTINE MULCH_EVAP(DYNAMIC, MULCH, EOS, EM)

      USE ModuleDefs; USE MODULEDATA
      IMPLICIT NONE
      SAVE

      INTEGER DYNAMIC
      REAL AM, EM, EOM, EOS, EXTFAC, MAI
!     REAL EMULFAC1, EMULFAC2
      REAL MULCHCOVER, MULCHMASS, MULCHTHICK, MULCHWAT
      REAL EM1, EM2, EOS1, EOS2, EOS3
      TYPE (MulchType)  MULCH

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      EM = 0.0

!***********************************************************************
!***********************************************************************
!     Daily rate calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
!     Reduce potential soil evaporation if mulch cover.
!     From A. Andeles tillage routine.  Added by CHP 03/16/01.
!---------------------------------------------------------------------
      MULCHMASS  = MULCH % MULCHMASS

      IF (MULCHMASS .GT. 0.1) THEN
        MULCHCOVER = MULCH % MULCHCOVER
        MULCHTHICK = MULCH % MULCHTHICK
        MULCHWAT   = MULCH % MULCHWAT
        AM         = MULCH % MULCH_AM
        EXTFAC     = MULCH % MUL_EXTFAC

!!-----------------------------------------------------------------------
!!       Previous method:
!!-----------------------------------------------------------------------
!!       Potential evaporation met with mulch water content.
!!       12/16/2005 CHP Only allow fraction of existing mulch water to evaporate
!!                       in a day.
!        EM1 = MIN(EOS*MULCHCOVER, MULCHWAT*0.85)
!        EM1 = MAX(EM1, 0.0)
!        EOS1 = EOS - EM1
!
!!       Calculate evaporation reduction factor.
!!       EMULFAC = MIN(EXP(-0.5*MULCHTHICK),1.0 - 0.807*MULCHCOVER)
!        EMULFAC1 = EXP(-0.5 * MULCHTHICK)
!        EMULFAC2 = 1.0 - 0.807 * MULCHCOVER
!
!        EOS1 = EOS1 * MIN(EMULFAC1, EMULFAC2)

!-----------------------------------------------------------------------
!       6/24/2008 Use method in Scopel, et al. 2004
!-----------------------------------------------------------------------
!       Mulch area index over area covered by mulch
        IF (MULCHCOVER > 1.E-6) THEN
          MAI = (AM * 1.E-5 * MULCHMASS) / MULCHCOVER
        ELSE
          MAI = 0.0
        ENDIF

!       Potential mulch evaporation (mulched areas only)
        EOM = EOS * (1. - EXP(-EXTFAC * MAI))

!       Actual mulch evaporation (mulched areas only)
        EM2 = MIN(EOM, MULCHWAT * 0.85)

!       Adjust to whole field
        EM2 = MAX(EM2, 0.0) * MULCHCOVER

!       Adjust potential soil evaporation
        EOS2 = EOS - EM2
        EOS3 = EOS * EXP(-EXTFAC * MAI) * MULCHCOVER +
     &         EOS * (1.0 - MULCHCOVER)
        EOS3 = MIN(EOS2, EOS3)

        EM = EM2
        EOS = EOS3

      ELSE
        EM = 0.
        EM1=0.; EM2=0.; EOS1=0.; EOS2=0.; EOS3=0.
      ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************

      MULCH % MULCHEVAP  = EM
      RETURN
      END SUBROUTINE MULCH_EVAP

!=======================================================================
