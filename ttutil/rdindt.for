      SUBROUTINE RDINDT (VAR_NAME,DATA_TYPE)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER*(*) VAR_NAME, DATA_TYPE

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
*        return data type from rerun file if variable occurs on it

         IF (INFREP.EQ.0) CALL FATALERR
     &      ('RDINDT','no rerun file active')

         I1 = IFINDC (REPLIS,ILNREP,1,INFREP,VAR_NAME_UC)
         IF (I1.GT.0) DATA_TYPE = REPTYP(I1)
      END IF

      IF (I1.EQ.0) THEN
*        return data type from active data file

         IF (INFDAT.EQ.0) CALL FATALERR ('RDINDT','no data file active')

         I1 = IFINDC (DATLIS,ILNDAT,1,INFDAT,VAR_NAME_UC)
         IF (I1.EQ.0) CALL FATALERR ('RDINDT','variable not found')
         DATA_TYPE = DATTYP(I1)
      END IF

      RETURN
      END
