      SUBROUTINE RDSCTB (EOF)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      LOGICAL EOF

**    include files
      INCLUDE 'rdrecinf.inc'
      INCLUDE 'rdtblinf.inc'
      INCLUDE 'rdndec.gin'
      INCLUDE 'rddecinf.inc'
      INCLUDE 'lextokin.inc'
      INCLUDE 'rdstainf.inc'

*     lexical analysis
      INTEGER TNEXT

*     state machine
*     Syntax check state machine
*     --------------------------
*    -1 - error in name part
*     0 - error in value part
*     1 - looking for table ; expecting identifier
*     2 - first identifier found
*     3 - first identifier + EOL
*     4 - name separator found
*     5 - next identifier found
*     6 - next identifier + EOL
*     7 - equal sign found
*     8 - integer found
*     9 - multiplier * found
*     A - string found
*     B - concatenator // found
*     C - other value or missing value or value after * found
*     D - value separator found, there must be another value
*     E - any value + space
*     F - value + EOL, on next line no comma, can be new table
*     G - End_Of_Table
*
*        STATUS              + - 1 2 3 4 5 6 7 8 9 A B C D E F G
*      WORD                  -----------------------------------
*     0 - unknown            + - + + + + + + - - - - - - - - - -
*     1 - TEOI   - EOF       G G G + + + + + - G - G - G - G G -
*     2 - TEOL   - EOL       6 F 1 3 3 4 6 6 7 F 9 F B F D F F -
*     3 - TTIME  - time      + C + + + + + C C - C - - - C C C -
*     4 - TINTEG - integer   + 8 + + + + + 8 8 - C - - - 8 8 8 -
*     5 - TFLOAT - real      + C + + + + + C C - C - - - C C C -
*     6 - TSPACE - space     + - 1 2 3 4 5 6 7 E 9 E B E D E F -
*     7 - TCOMMA - comma     4 D + 4 + + 4 + - D - D - D - D - -
*     8 - TEQUAL - equal =   7 - + 7 + + 7 + - - - - - - - - - -
*     9 - TSEMIC - semicol   G G + + + + + + - G - G - G - G G -
*    10 - TMULT  - *         + - + + + + + + - 9 - - - - - - - -
*    11 - TLPAR  - (         + - + + + + + + - - - - - - - - - -
*    12 - TRPAR  - )         + - + + + + + + - - - - - - - - - -
*    13 - TSCONC - //        + - + + + + + + - - - B - - - - - -
*    14 - TCMMNT - !         + - 1 2 3 4 5 6 7 8 9 A B C D E F -
*    15 - TIDENT - name      5 G 2 5 5 5 5 5 - - - - - - - - G -
*    16 - TSTRNG - string    + A + + + + + A A - A - A - A A A -
*    17 - TLOGIC - logical   + C + + + + + C C - C - - - C C C -
*    18 - TMISS  - missing   + C + + + + + C C - C - - - C C C -

*     IS     - status of parser
*     ISOLD  - previous parser status
*     EOTAB  - End_Of_Table status
*     JUMPTB - jumptable
*     NINPUT - number of parser input codes
*     NSTATS - number of parser states
      INTEGER NINPUT, NSTATS, EOTAB
      PARAMETER (NSTATS=16, NINPUT=18, EOTAB=16)
      INTEGER IS,ISOLD,JUMPTB
      DIMENSION JUMPTB(-1:NSTATS,0:NINPUT)

*     error messages
      CHARACTER MESSAG*35
      DIMENSION MESSAG(NSTATS-1)

*     others
*     IDUM   - dummy integer
*     CDUM   - dummy string
*     IREP   - repeat value
*     COLNUM - current column number
*     TBCOMP - flags a complete table
*     NEWREC - Whether new record should be read from input file
*     IWAR   - Warning whether record that was read was too long
*              for input buffer
      INTEGER IDUM, IREP, COLNUM, IWAR
      CHARACTER CDUM*1,CTRLZ*1
      LOGICAL TBCOMP, ON, OFF, NEWREC
      PARAMETER (ON=.TRUE., OFF=.FALSE.)

      SAVE
      INCLUDE 'rdndat.gin'

      DATA JUMPTB/
     $ -1, 0,-1,-1,-1,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     $ 16,16,16,-1,-1,-1,-1,-1, 0,16, 0,16, 0,16, 0,16,16, 0,
     $  6,15, 1, 3, 3, 4, 6, 6, 7,15, 9,15,11,15,13,15,15, 0,
     $ -1,12,-1,-1,-1,-1,-1,12,12, 0,12, 0, 0, 0,12,12,12, 0,
     $ -1, 8,-1,-1,-1,-1,-1, 8, 8, 0,12, 0, 0, 0, 8, 8, 8, 0,
     $ -1,12,-1,-1,-1,-1,-1,12,12, 0,12, 0, 0, 0,12,12,12, 0,
     $ -1, 0, 1, 2, 3, 4, 5, 6, 7,14, 9,14,11,14,13,14,15, 0,
     $  4,13,-1, 4,-1,-1, 4,-1, 0,13, 0,13, 0,13, 0,13, 0, 0,
     $  7, 0,-1, 7,-1,-1, 7,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     $ 16,16,-1,-1,-1,-1,-1,-1, 0,16, 0,16, 0,16, 0,16,16, 0,
     $ -1, 0,-1,-1,-1,-1,-1,-1, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0,
     $ -1, 0,-1,-1,-1,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     $ -1, 0,-1,-1,-1,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     $ -1, 0,-1,-1,-1,-1,-1,-1, 0, 0, 0,11, 0, 0, 0, 0, 0, 0,
     $ -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15, 0,
     $  5,16, 2, 5, 5, 5, 5, 5, 0, 0, 0, 0, 0, 0, 0, 0,16, 0,
     $ -1,10,-1,-1,-1,-1,-1,10,10, 0,10, 0,10, 0,10,10,10, 0,
     $ -1,12,-1,-1,-1,-1,-1,12,12, 0,12, 0, 0, 0,12,12,12, 0,
     $ -1,12,-1,-1,-1,-1,-1,12,12, 0,12, 0, 0, 0,12,12,12, 0/

      DATA MESSAG/
     $ 'Variable name expected             ',
     $ 'Next variable name or = expected   ',
     $ 'Without =, a next name is expected ',
     $ 'Next variable name expected        ',
     $ 'Next name, = or value on next line ',
     $ 'Next name or first value expected  ',
     $ 'Value expected                     ',
     $ 'Illegal after INTEGER value        ',
     $ 'After * a value is expected        ',
     $ 'Illegal after CHARACTER string     ',
     $ 'After // next string expected      ',
     $ 'No proper value separation         ',
     $ 'There must be another value        ',
     $ 'After a value this is illegal      ',
     $ 'New variable or next value expected'/

*     initialize output parameter
      EOF = OFF

*     initialize table info common variables
      TBLERR = OFF
      NAMERR = OFF
      TBARAY = 's'
      INAMES = 0
      NCOL   = 0
      NVAL   = 0

*     initialize local variables for table parsing
      TBCOMP = ON
      IREP   = 1
      IS     = 1
      IDUM   = 0
      CTRLZ  = CHAR (26)

*     new table
      CALL RDTMP1 (2,IDUM,CDUM)

10    IF (IS.NE.EOTAB) THEN
*        save current status ; next TOKEN ; jump
         NEWREC = TOKEN.EQ.TEOL.OR.TOKEN.EQ.-1
20       IF (NEWREC) THEN
            CALL RECREAD (STBUF,RECLEN-1,STBLEN,EOF,IWAR)
            IF (.NOT.EOF) EOF = STBUF(1:1).EQ.CTRLZ
            IF (.NOT.EOF) THEN
               RECNO  = RECNO+1
               NEWREC = STBUF2(1).EQ.'*'.OR.
     &                  STBUF2(1).EQ.'!'.OR.
     &                  STBLEN.EQ.0

               IF (.NOT.NEWREC) THEN
*                 check for truncation of records
                  IF (IWAR.EQ.1) THEN
                     ISP = STBLEN-1
                     IP  = STBLEN-1
                     CALL RDERR (2,'Input record truncated')
                  END IF

                  CALL RDLEX (2)
               END IF
            ELSE
               NEWREC = .FALSE.
            END IF
         GOTO 20
         END IF

         IF (.NOT.EOF) THEN
            CALL RDLEX (3)
         ELSE
            CALL RECREAD_TERM
            RECNO = 0
            STBUF  = ' '
            STBLEN = 0
            ISP    = 1
            IP     = 1
            TOKEN  = TEOI
         END IF

         ISOLD = IS
         IS    = JUMPTB (ISOLD,TOKEN)

c         write (30,'(a,i2,A8,a,i2)')
c     $    ' rdsctb: ',token, tokenn(token),' -> state ',is

         IF (ISOLD.GT.0 .AND. IS.LE.0) THEN
*           syntax error
            TBLERR = ON
            IF (IS.EQ.-1) NAMERR = ON
            IF (IS.EQ. 0) IREP = 1

*           state machine error message
            IF (TOKEN.NE.TEOI) CALL RDERR (2,MESSAG(ISOLD))
            IF (TOKEN.EQ.TEOI) CALL RDERR (2,'Unexpected End_Of_File')
         END IF

         IF (TOKEN.EQ.TIDENT .AND. IS.NE.EOTAB) THEN

            IF (NCOL.GE.NCOLMX) THEN
*              too many variables in table ; stop
               CALL RDERR (2,'Too many columns in table')
               IF (TOSCR) WRITE (*,'(/,A,I3,2A,/,2A)')
     $          ' Table has more than',NCOLMX,' columns.',
     $          ' Increase the value',' of NCOLMX in the',
     $          ' subroutines RDSCTB and RDTMP1 (both!!).'
               IF (TOLOG) WRITE (IULOG,'(/,A,I3,2A,/,2A)')
     $          ' Table has more than',NCOLMX,' columns.',
     $          ' Increase the value',' of NCOLMX in the',
     $          ' subroutines RDSCTB and RDTMP1 (both!!).'
               CALL FATALERR ('RDSCTB','too many columns')
            ELSE IF (IP-ISP+1.GT.INLMX) THEN
*              name too long
               CALL RDERR (2,'Variable name too long')
               NAMERR = ON
               TBLERR = ON
            ELSE
*              accept new variable name
               NCOL = NCOL + 1
               IF (.NOT.NAMERR) INAMES = INAMES + 1

*              set name, line number and type
               TBLNAM(NCOL) = STBUF(ISP:IP)
               CALL UPPERC (TBLNAM(NCOL))
               TBLLIN(NCOL) = RECNO
               TBLTYP(NCOL) = '-'

*              get field pointer
               CALL RDTMP1 (4,TBLPNT(NCOL),CDUM)
            END IF

         ELSE IF (TOKEN.EQ.TSTRNG .OR. TOKEN.EQ.TINTEG .OR.
     $            TOKEN.EQ.TFLOAT .OR. TOKEN.EQ.TTIME  .OR.
     $            TOKEN.EQ.TLOGIC .OR. TOKEN.EQ.TMISS) THEN

            IF (TOKEN.EQ.TINTEG) THEN
*              check for repeat value
               CALL RDLEX (3)
               TNEXT = TOKEN
               CALL RDLEX (4)
            END IF

            IF (TOKEN.EQ.TINTEG .AND. TNEXT.EQ.TMULT) THEN
*              get integer value as repeat value
               IREP = VINT

               IF (IS.GT.0 .AND. NCOL.GT.1) THEN
*                 repeater in table is illegal
                  CALL RDERR (2,'Repeat value not allowed in table')
                  TBLERR = ON
               ELSE IF (IREP.LE.0) THEN
*                 illegal repeat value
                  CALL RDERR (2,'Repeat value should be positive')
                  TBLERR = ON
               ELSE
*                 variable is an array, also if IREP=1
                  TBARAY = 'a'
               END IF

            ELSE IF (ISOLD.EQ.11) THEN
*              string concatenates with previous string ?
               CALL RDTMP1 (7,IDUM,CDUM)

            ELSE
*              all other cases, write value to file buffer
               CALL RDTMP1 (6,IREP,CDUM)

*              check data type of written value ; get column number
               NVAL   = NVAL + IREP
               COLNUM = 1 + MOD (NVAL-1,MAX(1,NCOL))

               IF (TOKEN.NE.TMISS) THEN
*                 type checking
                  IF (TBLTYP(COLNUM).EQ.'-') THEN
*                    set column data type
                     TBLTYP(COLNUM) = VTYPE

                  ELSE IF (
     $             VTYPE.NE.TBLTYP(COLNUM).AND..NOT.NAMERR) THEN
*                    inconsistent type
                     IF (NCOL.LE.1) THEN
*                       simple message
                        CALL RDERR (2,'Inconsistent variable type')
                        TBLERR = ON
                     ELSE IF (.NOT.TBLERR) THEN
*                       no syntax errors but types inconsistent
                        CALL RDERR (2,'Column type inconsistent ')
                        TBLERR = ON
                     END IF
                  END IF
               END IF

*              reset repeat value
               IREP = 1

*              table complete ; last column ?
               TBCOMP = COLNUM.EQ.NCOL
            END IF
         END IF
      GOTO 10
      END IF

*     check completeness of table
      IF (.NOT.TBCOMP .AND. .NOT.TBLERR) THEN
         CALL RDERR (2,'Previous table is incomplete')
         TBLERR = ON
      END IF

      IF (TOKEN.EQ.TIDENT) THEN
*        end of table detected by begin of next ; step backward LEX
         CALL RDLEX (4)
      ELSE IF (TOKEN.EQ.TEOI) THEN
*        set End_Of_File
         EOF = ON
      END IF

*     if NVAL > 1, there may be an array of values, there may
*     also be a table with 1 or more rows. In all cases the
*     variable(s) are classified as array variables, also if there
*     is just a single row. during the parsing above there the use of
*     a repeat value was already detected.
      IF (NVAL.GT.1) TBARAY = 'a'

*     terminate table
      CALL RDTMP1 (8,IDUM,CDUM)

      RETURN
      END
