      LOGICAL FUNCTION RDINAR (VAR_NAME)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER*(*) VAR_NAME

**    machine constants and buffer length
      INCLUDE 'rdmachin.inc'
      INCLUDE 'rddata.inc'

*     Local variables
      INTEGER I1,IFINDC
      CHARACTER*1  SCAR
      CHARACTER*31 VAR_NAME_UC
      SAVE

      VAR_NAME_UC = VAR_NAME
      CALL UPPERC (VAR_NAME_UC)

      I1   = 0
      SCAR = ' '

      IF (ISCOM.GT.0) THEN
*        return arr value from rerun file if variable occurs on it

         IF (INFREP.EQ.0) CALL FATALERR
     &      ('RDINAR','no rerun file active')

         I1 = IFINDC (REPLIS,ILNREP,1,INFREP,VAR_NAME_UC)
         IF (I1.GT.0) SCAR = REPARR(I1)
      END IF

      IF (I1.EQ.0) THEN
*        return arr value from active data file

         IF (INFDAT.EQ.0) CALL FATALERR ('RDINAR','no data file active')

         I1 = IFINDC (DATLIS,ILNDAT,1,INFDAT,VAR_NAME_UC)
         IF (I1.EQ.0) CALL FATALERR ('RDINAR','variable not found')
         SCAR = DATARR(I1)
      END IF

      RDINAR = SCAR .EQ. 'a'

      RETURN
      END
