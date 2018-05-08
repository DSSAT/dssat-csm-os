      SUBROUTINE AMBUSY (ITASK,MODULE,ICODE)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER ITASK,ICODE
      CHARACTER*(*) MODULE

**    local variables and function types
      INTEGER IFINDC,IFND,ILMAX,IN,ISTORE
      CHARACTER*6 MODLIS,LMODNM
      PARAMETER (ILMAX=10)
      DIMENSION ISTORE(0:ILMAX),MODLIS(ILMAX)

      SAVE
      DATA ISTORE/0,ILMAX*0/,IN/0/

*     local module name
      CALL UPPERC (MODULE)
      LMODNM = MODULE
      IFND   = IFINDC (MODLIS,ILMAX,1,IN,LMODNM)

      IF (ITASK.EQ.1) THEN
*        enter input code
         IF (IFND.EQ.0) THEN
*           add module to list
            IN = IN + 1
            IF (IN.GT.ILMAX) CALL FATALERR ('AMBUSY','too many names')
            MODLIS(IN) = LMODNM
            IFND = IN
         END IF

*        write input code in list
         ISTORE(IFND) = ICODE

      ELSE IF (ITASK.EQ.2) THEN
*        return input code or zero for unknown name
         ICODE = ISTORE (IFND)

      ELSE
*        comment
         CALL FATALERR ('AMBUSY','illegal ITASK value')
      END IF

      RETURN
      END
