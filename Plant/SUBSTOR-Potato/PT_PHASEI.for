C======================================================================
C  PT_PHASEI, Subroutine
C
C  Determines phase initialization
C-----------------------------------------------------------------------
C  Revision history
C
C                 Written
C  02/07/1993 PWW Header revision and minor changes       
C  02/07/1993 PWW Added switch common block, restructured +
C  08/28/2001 CHP Moved GROSUB and ROOTGR calculations to those routines.
C=======================================================================

      SUBROUTINE PT_PHASEI (
     &    ISTAGE, CUMDTT, XPLANT, SPRLAP,                 !I/O
     &    CTII, CUMSTT, MAXLAI, SENLA, TSPRWT, XDTT)      !Output

C-----------------------------------------------------------------------
      IMPLICIT  NONE
      SAVE

      INTEGER ISTAGE

      REAL CTII, CUMDTT, CUMSTT, MAXLAI
      REAL SENLA, TPSM, TSPRWT
      REAL PLSC(25), SPRLAP, XDTT, XPLANT

C-----------------------------------------------------------------------
      SELECT CASE (ISTAGE)
        CASE (1)                        ! Begin tuber growth
          ISTAGE = 2
          XDTT    = CUMDTT
                                          
C-----------------------------------------------------------------------
        CASE (2)                        ! Maturity
          CTII    = 0.0
          ISTAGE  = 5
          TSPRWT  = 0.0
          TPSM    = 0.0

C-----------------------------------------------------------------------
        CASE (5)                        ! Planting - germination
          ISTAGE  = 6
          CUMDTT  = 0.0
          CUMSTT  = 0.0
          IF (SPRLAP .GT. 0.0) THEN
             ISTAGE = 7
          END IF

C-----------------------------------------------------------------------
        CASE (6)                        ! Germination
          ISTAGE  = 7
          CUMDTT  = 0.0                 ! Thermal time for haulm

C-----------------------------------------------------------------------
        CASE (7)                        ! Emergence
          ISTAGE  = 1
          CTII    = 0.0
          CUMDTT  = 0.0
          CUMSTT  = 0.0
          MAXLAI  = 0.0
          PLSC(1) = 0.0
          SENLA   = 0.0
          TPSM    = XPLANT
        CASE DEFAULT
          STOP 'Illegal value for ISTAGE in PHASEI module'
      END SELECT

      RETURN
      END SUBROUTINE PT_PHASEI
