      SUBROUTINE STR_COPY (SOURCE_S,TARGET_S,OK)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER*(*) SOURCE_S,TARGET_S
      LOGICAL OK

**    Local variables
      INTEGER ILS,ILT
      SAVE

      OK  = .TRUE.
      ILS = MAX (1,LEN_TRIM (SOURCE_S))
      ILT = LEN (TARGET_S)

      IF (ILS.GT.ILT) THEN
         OK  = .FALSE.
         ILS = ILT
      END IF

      TARGET_S = SOURCE_S(1:ILS)

      RETURN
      END
