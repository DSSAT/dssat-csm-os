      SUBROUTINE DTFSEDP (IYEAR,IDOY,DOY,DPDTTM)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER IYEAR,IDOY
      REAL DOY
      DOUBLE PRECISION DPDTTM

**    Local variables
      DOUBLE PRECISION TMP_D
      INTEGER TMP_I(6)
      REAL FSEC
      SAVE

      TMP_I(1) = IYEAR
      TMP_I(2) = 1
      TMP_I(3) = 1
      TMP_I(4) = 0
      TMP_I(5) = 0
      TMP_I(6) = 0
      FSEC     = 0.

      CALL DTARDP (TMP_I,FSEC,TMP_D)
      DPDTTM = TMP_D+DBLE (IDOY-1)+DBLE (MAX (DOY-REAL (IDOY),0.))

      RETURN
      END
