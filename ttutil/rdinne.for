      SUBROUTINE RDINNE (VAR_NAME,NO_EL)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER NO_EL
      CHARACTER*(*) VAR_NAME

**    machine constants and buffer length
      INCLUDE 'rdmachin.inc'
      INCLUDE 'rddata.inc'

*     Local variables
      INTEGER I1,IFINDC,IP
      CHARACTER*31 VAR_NAME_UC
      SAVE

      VAR_NAME_UC = VAR_NAME
      CALL UPPERC (VAR_NAME_UC)

      I1 = 0

      IF (ISCOM.GT.0) THEN
*        return number of elements from rerun file if variable occurs on it

         IF (INFREP.EQ.0) CALL FATALERR
     &      ('RDINNE','no rerun file active')

         I1 = IFINDC (REPLIS,ILNREP,1,INFREP,VAR_NAME_UC)
         IF (I1.GT.0) THEN
*           variable in rerun list, get number of elements from rerun
*           file
            IF (REPARR(I1).EQ.'s') CALL FATALERR ('RDINNE',
     $       'variable on rerun file is scalar')

            IP = INFREP*(ISCOM-1)+I1
            IF (IP.LE.ILPREP) THEN
               NO_EL = IARREP(IP)
            ELSE
               CALL SWPI4 (2,'RD\IARREP',-99,ILPREP,IP,NO_EL)
            END IF
         END IF
      END IF

      IF (I1.EQ.0) THEN
*        return number of elements from active data file

         IF (INFDAT.EQ.0) CALL FATALERR ('RDINNE','no data file active')

         I1 = IFINDC (DATLIS,ILNDAT,1,INFDAT,VAR_NAME_UC)
         IF (I1.EQ.0) CALL FATALERR ('RDINNE','variable not found')
         IF (DATARR(I1).EQ.'s') CALL FATALERR ('RDINNE',
     $    'variable on data file is scalar')

         NO_EL = IARDAT(I1)
      END IF

      RETURN
      END
