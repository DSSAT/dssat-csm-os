      SUBROUTINE EXTENS (FILEIN,NEWEXT,ICHECK,FILEOU)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER ICHECK
      CHARACTER*(*) FILEIN,FILEOU,NEWEXT

**    local variables + function called
      INTEGER I,ILEXT1,ILEXT2,ILFIL,IP,LOUT,IS
      CHARACTER*1 OLD,NEW,CHR
      CHARACTER*132 FILBUF
      LOGICAL CHECK, HAAK, FOUND
      SAVE

*     remove point from new extension
      IF (NEWEXT(1:1).NE.'.') THEN
         IS = 1
      ELSE
         IS = 2
      END IF

*     length of input filename and new extension
      ILFIL  = LEN_TRIM (FILEIN)
      ILEXT2 = LEN_TRIM (NEWEXT) - IS + 1
      FILBUF = NEWEXT
      LOUT   = LEN (FILEOU)
      IF (ILFIL.EQ.0)  CALL FATALERR ('EXTENS','no filename supplied')

*     initialize search for "no extension"
      IP     = ILFIL + 1
      ILEXT1 = 0
      HAAK   = .FALSE.
      FOUND  = .FALSE.

*     search for point from the end on
      I = ILFIL
10    IF (I.GT.0 .AND..NOT.(FOUND.OR.HAAK)) THEN
*        check single character
         CHR  = FILEIN(I:I)
*        end of directory string on UNIX, Macintosh, MS-DOS and VAX
         HAAK = CHR.EQ.CHAR(47) .OR. CHR.EQ.CHAR(58) .OR.
     $          CHR.EQ.CHAR(92) .OR. CHR.EQ.CHAR(93)

         IF (CHR.EQ.'.') THEN
*           a (valid) point is found
            IP = I
            ILEXT1 = ILFIL - IP
            FOUND  = .TRUE.
         END IF

         I = I - 1
      GOTO 10
      END IF

*     check and set output name in uppercase
      IF (IP.GT.132) THEN
         CALL MESSWRT ('Input file name',FILEIN)
         CALL FATALERR ('EXTENS','input filename too long')
      END IF
      FILBUF = ' '
      IF (IP.GE.2) FILBUF = FILEIN(1:IP-1)
      CALL UPPERC (FILBUF)
*     set point
      IF (ILEXT2.GT.0) FILBUF(IP:IP) = '.'

*     if old and new extension are equally long ; check !!
      CHECK = (ILEXT1.EQ.ILEXT2) .AND. (ICHECK.NE.0)

      DO 20 I=IP+1,IP+ILEXT2
         IF (I.GT.132) THEN
            CALL MESSWRT ('Input file name',FILEIN)
            CALL FATALERR ('EXTENS','new filename too long')
         END IF

*        get character new extension and put into output
         NEW = NEWEXT(I-IP+IS-1:I-IP+IS-1)
         CALL UPPERC (NEW)
         FILBUF(I:I) = NEW

         IF (CHECK) THEN
*           get character old extension for comparison
            OLD = FILEIN(I:I)
            CALL UPPERC (OLD)
            IF (OLD.NE.NEW) CHECK=.FALSE.
         END IF
20    CONTINUE

      IF (CHECK) THEN
         CALL MESSWRT ('Input file name',FILEIN)
         CALL FATALERR ('EXTENS','old and new extension equal')
      END IF

      IF (LOUT.LT.IP+ILEXT2) THEN
         CALL MESSWRT ('Input file name',FILEIN)
         CALL FATALERR ('EXTENS','output string too short for new name')
      END IF

      FILEOU = FILBUF(1:IP+ILEXT2)
      RETURN
      END
