!***********************************************************************
!  MULCHLAYER, Subroutine
!
!  Purpose: computes properties of mulch layer.
!
!  Revision       History
!  05/18/2004 CHP Written.
!-----------------------------------------------------------------------
!  Called : Century, SoilOrg (formerly NTRANS)
C=======================================================================

      SUBROUTINE MULCHLAYER (MULCH)

!-----------------------------------------------------------------------
      USE ModuleDefs
      USE ModuleData
      USE Interface_IpSoil
      IMPLICIT  NONE
      SAVE

      REAL AM, ATHICK, SURF, XMASS
      REAL MULCHALB, MULCHCOVER, MULCHMASS, MULCHTHICK

!     Constructed variables are defined in ModuleDefs.
      TYPE (MulchType)   MULCH

!     ----------------------------------------------------------------
!     Transfer mulch amount to local variable
      MULCHMASS = MULCH % MULCHMASS
      AM        = MULCH % MULCH_AM

!     Calculate fraction residue surface coverage
      MULCHCOVER = 1.0 - EXP(-AM * 1.E-5 * MULCHMASS)

!     Estimate mulch thickness for use in evaporation reduction.
      MULCHTHICK = 0.0
      ATHICK = 1.5
      XMASS = MULCHMASS
      SURF = MULCHCOVER
      DO WHILE (SURF .GT. 0.01)
        XMASS = XMASS - SURF / (AM * 1.E-5)
        SURF = SURF * (1.0 - EXP(-AM * 1.E-5 * XMASS / SURF))
        MULCHTHICK = MULCHTHICK + SURF * ATHICK
      END DO

!     Set mulch albedo here
      MULCHALB = 0.45

!     Transfer local variables to MULCH composite variable for export.
      MULCH % MULCHALB   = MULCHALB
      MULCH % MULCHCOVER = MULCHCOVER
      MULCH % MULCHTHICK = MULCHTHICK

      CALL PUT('ORGC','MULCHMASS',MULCH % MULCHMASS)
!     ----------------------------------------------------------------
      RETURN
      END SUBROUTINE MULCHLAYER

!=======================================================================

