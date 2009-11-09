C=======================================================================
C  PT_THTIME, Subroutine
C
C  Determines thermal time calculations
C-----------------------------------------------------------------------
C  Revision history
C
C               Written
C  02/28/1993 PWW Header revision and minor changes
C  08/28/2001 CHP Modified for modular format.
C=======================================================================

      SUBROUTINE PT_THTIME (
     &    ISTAGE, L0, ST, TMAX, TMIN,                     !Input
     &    DTT, STT)                                       !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
!     NL defined in ModuleDefs.for

      IMPLICIT NONE

      INTEGER  ISTAGE, L0

      REAL DTT, STT, TMIN, TMAX, XTEMP
      REAL ST(NL)

C-----------------------------------------------------------------------
      DTT = 0.0
      STT = 0.0

      XTEMP = (TMAX + TMIN)/2.0
      !
      ! Thermal time leaf expansion
      !
      IF (ISTAGE .LE. 4) THEN
         IF (XTEMP .LE. 2.0) THEN
            DTT = 0.0
          ELSE IF (XTEMP .GT.  2.0 .AND. XTEMP .LE. 17.0) THEN
            DTT = 0.0667*(XTEMP - 2.0)
          ELSE IF (XTEMP .GT. 17.0 .AND. XTEMP .LE. 24.0) THEN
            DTT = 1.0
          ELSE IF (XTEMP .GT. 24.0 .AND. XTEMP .LE. 35.0) THEN
            DTT = 1.0 -0.0909*(XTEMP - 24.0)
          ELSE
            DTT = 0.0
         END IF
      END IF
      !
      ! Soil thermal time (tuber)
      !
      IF (ISTAGE .GE. 6 .OR. ISTAGE .LE. 2) THEN
         IF (ST(L0) .GE. 2.0 .AND. ST(L0) .LT. 15.0) THEN
            STT = 0.0769*(ST(L0)-2.0)
          ELSE IF (ST(L0) .GE. 15.0 .AND. ST(L0) .LT. 23.0) THEN
            STT = 1.0
          ELSE IF (ST(L0) .GE. 23.0 .AND. ST(L0) .LT. 33.0) THEN
            STT = 1.0 - 0.1*(ST(L0) - 23.0)
          ELSE
            STT = 0.0
         ENDIF
      END IF

      RETURN
      END SUBROUTINE PT_THTIME
