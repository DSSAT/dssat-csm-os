C=======================================================================
C  GNURSE, Subroutine
C
C  Determines day when crop leaves nursery
C-----------------------------------------------------------------------
C  Revision history
C
C  08/07/1993 PWW Header revision and minor changes
C  08/29/2002 CHP/MUS Converted to modular format for inclusion in CSM.
C  02/19/2003 CHP Converted dates to YRDOY format
C=======================================================================

      SUBROUTINE GNURSE (ITRANS, TAGE, YRPLT, YRSIM,      !Input
     &                   YRSOW, YRNURSE)                  !Output

      IMPLICIT NONE
      EXTERNAL TIMDIF

      INTEGER  ITRANS, TIMDIF, YRNURSE, YRPLT, YRSIM, YRSOW
      REAL     TAGE

      YRNURSE = YRSIM

      IF (ITRANS .EQ. 2 .OR. ITRANS .EQ. 4) THEN
        IF (TIMDIF(YRSOW, YRPLT) .EQ. IFIX(TAGE)) THEN
          IF (YRSIM .LE. YRPLT) THEN
            YRNURSE = MIN0 (YRSIM, YRSOW)
          ENDIF
        ELSE
          YRNURSE = MIN0 (YRSIM, YRSOW)
          IF (YRSIM .LT. YRPLT) THEN
             YRNURSE = YRSOW
          ENDIF
        ENDIF                                                                  
      ENDIF

      RETURN
      END SUBROUTINE GNURSE
