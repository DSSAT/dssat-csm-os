      LOGICAL FUNCTION RDINQR3 (VAR_NAME)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER*(*) VAR_NAME

**    machine constants and buffer length
      INCLUDE 'rdmachin.inc'
      INCLUDE 'rddata.inc'

*     Local variables
      INTEGER I1,IFINDC
      CHARACTER*31 VAR_NAME_UC
      SAVE

      VAR_NAME_UC = VAR_NAME
      CALL UPPERC (VAR_NAME_UC)

      I1 = 0

      IF (INFREP.GT.0) THEN
*        return existence from rerun file if variable occurs on it
         I1 = IFINDC (REPLIS,ILNREP,1,INFREP,VAR_NAME_UC)
      END IF

      RDINQR3 = I1.GT.0

      RETURN
      END
