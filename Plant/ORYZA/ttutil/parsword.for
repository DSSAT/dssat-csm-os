*     ---------------------------------------------
*     ---------------------------------------------
      SUBROUTINE PARSWORD (XSTRING,TTYPE,IB,IE,NERR,NWAR)

*     Finds and decodes 1 word with leading and/or trailing spaces
*     decoded values are passed to calling program via  RDDECINF.INC
*     If there is more than a single word an error message is given
*
*     XSTRING - character string                               I
*     TTYPE   - returned token type according to RDNDEC.GIN    O
*               if only spaces are parsed then TTYPE -> TSPACE
*     IB      - begin of word pointer                          O
*     IE      - end of word pointer                            O
*     NERR    - >0 there is an error                           O
*     NWAR    - >0 there is a warning                          O

      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER*(*) XSTRING
      INTEGER TTYPE, IB, IE, NERR, NWAR

*     buffer for RDLEX routine
      INCLUDE 'rdrecinf.inc'

*     token information
      INCLUDE 'lextokin.inc'

*     integer codes for tokens (TSPACE is used below)
      INCLUDE 'rdndec.gin'

*     error messages
      INCLUDE 'rderrinf.inc'

*     local variables
      INTEGER ILS
      SAVE

*     write string in buffer under line number 0
      ILS    = LEN_TRIM (XSTRING)
      STBUF  = XSTRING
      STBLEN = LEN(XSTRING)
      RECNO  = 0

*     parse
      CALL RDLEX (1)
      CALL RDLEX (2)
      CALL RDLEX (3)

      IF (TOKEN.EQ.TSPACE .AND. IP.LT.ILS) THEN
*        there are leading spaces ; another time
         CALL RDLEX (3)
      END IF

*     returned
      TTYPE = TOKEN
      IB    = ISP
      IE    = IP

*     error check
      IF (IP.LT.ILS) THEN
         ISP = IP+1
         IP  = ILS
         CALL RDERR (2,'This is not a single item')
      END IF

      NERR  = INERR
      NWAR  = INWAR

      RETURN
      END
