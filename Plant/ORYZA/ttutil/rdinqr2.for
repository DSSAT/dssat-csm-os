      LOGICAL FUNCTION RDINQR2 (VAR_NAME)
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

      IF (ISCOM.GT.0) THEN
*        return existence from rerun file if variable occurs on it

         IF (INFREP.EQ.0) CALL FATALERR
     &      ('RDINQR2','no rerun file active')

         I1 = IFINDC (REPLIS,ILNREP,1,INFREP,VAR_NAME_UC)
      END IF

      IF (I1.EQ.0) THEN
*        return existence from active data file

         IF (INFDAT.EQ.0) CALL FATALERR
     &      ('RDINQR2','no data file active')

         I1 = IFINDC (DATLIS,ILNDAT,1,INFDAT,VAR_NAME_UC)
      END IF

      RDINQR2 = I1.GT.0

      RETURN
      END
