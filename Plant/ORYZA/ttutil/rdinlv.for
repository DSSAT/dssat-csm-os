      SUBROUTINE RDINLV (SETFLAG,VARLIS,VARLIS_MN,VARLIS_AN)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      LOGICAL SETFLAG
      INTEGER VARLIS_MN,VARLIS_AN
      CHARACTER*(*) VARLIS(VARLIS_MN)

**    machine constants and buffer length
      INCLUDE 'rdmachin.inc'
      INCLUDE 'rddata.inc'

*     Local variables
      INTEGER I1,I2,I3
      SAVE

      IF (.NOT.SETFLAG.OR.ISCOM.EQ.0) THEN

*        return list of names from active datafile

         IF (INFDAT.EQ.0) CALL FATALERR ('RDINLV','no data file active')
         IF (INFDAT.GT.VARLIS_MN) CALL FATALERR
     &      ('RDINLV','external list does not have enough elements')

         I3 = LEN (VARLIS(1))

         DO I1=1,INFDAT
            I2 = LEN_TRIM (DATLIS (I1))
            IF (I2.LE.I3) THEN
*              name in internal table not longer than external string
               VARLIS(I1) = DATLIS(I1)(1:I2)
            ELSE
               CALL FATALERR ('RDINLV',
     &         'variable name to long for external list')
            END IF
         END DO

         VARLIS_AN = INFDAT

      ELSE IF (SETFLAG) THEN

*        return list of names from rerun file

         IF (INFREP.EQ.0) CALL FATALERR
     &      ('RDINLV','no rerun file active')
         IF (INFREP.GT.VARLIS_MN) CALL FATALERR
     &      ('RDINLV','external list does not have enough elements')

         I3 = LEN (VARLIS(1))

         DO I1=1,INFREP
            I2 = LEN_TRIM (REPLIS (I1))
            IF (I2.LE.I3) THEN
*              name in internal table not longer than external string
               VARLIS(I1) = REPLIS(I1)(1:I2)
            ELSE
               CALL FATALERR ('RDINLV',
     &         'variable name to long for external list')
            END IF
         END DO

         VARLIS_AN = INFREP

      END IF

      RETURN
      END
