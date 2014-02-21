      SUBROUTINE SFINDG (NAMLIS,ILDEC,IST,IEND,NAME,ISTYPE,
     &                   IFINDG,IMATCH)

      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER ILDEC, IST, IEND, ISTYPE, IFINDG, IMATCH
      CHARACTER*(*) NAMLIS(ILDEC), NAME

**    local variables
      INTEGER IN, IM, IL1, IL3, INCR
      SAVE

      IL1 = LEN (NAMLIS(1))
      IL3 = LEN_TRIM (NAME)

*     error check
      IF (IL3.GT.IL1) CALL FATALERR ('SFINDG','search string too long')

      IF (IEND.GT.IST) THEN
*        search from low to high elements
         IF (IST.LT.1.OR.IEND.GT.ILDEC)
     &      CALL FATALERR ('SFINDG','search outside array bounds')
         INCR = +1
      ELSE IF (IEND.LT.IST) THEN
*        search from high to low elements
         IF (IEND.LT.1.OR.IST.GT.ILDEC)
     &      CALL FATALERR ('SFINDG','search outside array bounds')
         INCR = -1
      END IF

      IFINDG = 0
      DO IN=IST,IEND,INCR
         IL1 = LEN_TRIM(NAMLIS(IN))

*        check length of strings
         IF (IL3.LE.IL1) THEN
*           string that is searched is not longer than string
*           to search in
            IM  = INDEX (NAMLIS(IN)(1:IL1),NAME(1:IL3))
         ELSE
*           string that is searched is longer than string
*           to search in, a match cannot occur and also
*           is is risky to call INDEX in that situation
            IM = 0
         END IF

         IF (IM.GT.0) THEN
            IF (ISTYPE.EQ.1) THEN
*              string should match exactly
               IF (IM.EQ.1.AND.IL3.EQ.IL1) THEN
                  IFINDG = IN
                  GOTO 30
               END IF
            ELSE IF (ISTYPE.EQ.2) THEN
*              beginning should match
               IF (IM.EQ.1) THEN
                  IFINDG = IN
                  GOTO 30
               END IF
            ELSE IF (ISTYPE.EQ.3) THEN
*              end should match
               IF (IM.EQ.IL1-IL3+1) THEN
                  IFINDG = IN
                  GOTO 30
               END IF
            ELSE IF (ISTYPE.EQ.4) THEN
*              match position not specified
               IFINDG = IN
               GOTO 30
            END IF
         END IF
      END DO

      IM = 0

30    CONTINUE

      IMATCH = IM

      RETURN
      END
