      SUBROUTINE RDLEX (ITASK)
      IMPLICIT NONE

*     Formal parameters
      INTEGER ITASK

**    define long buffer to hold byte information
      INCLUDE 'lextokin.inc'
      INCLUDE 'rdrecinf.inc'

*     Syntax table declarations
      INCLUDE 'rdjdec.gin'
      INCLUDE 'rdndec.gin'

*     local variables
      INTEGER CODES(RECLEN)
      INTEGER I1, I2, ISTATE, TMPP
      INTEGER MNSTCK,NSTCK,ISTCK
      PARAMETER (MNSTCK=500)
      INTEGER BPSTCK(MNSTCK),EPSTCK(MNSTCK),TKSTCK(MNSTCK)

*     values
      INCLUDE 'rddecinf.inc'
      INTEGER INSTCK(MNSTCK)
      DOUBLE PRECISION RESTCK(MNSTCK),TISTCK(MNSTCK)
      LOGICAL LOSTCK(MNSTCK)
      CHARACTER*1 VTSTCK(MNSTCK)

      SAVE

*     Jump table data
      INCLUDE 'rdjdat.gin'
      INCLUDE 'rdndat.gin'

      IF (ITASK.EQ.1) THEN
*        initialize
*        formal parameters
         ISP    = 0
         IP     = 0
         TOKEN  = -1

*        internal variables
         ISTCK  = 0
         NSTCK  = 0
         RETURN

      END IF

      IF (ITASK.EQ.2) THEN
*        prepare record for parsing

*        add carriage return code for end_of_line character to the
*        right of rightmost character
         STBLEN         = STBLEN+1
         STBUF2(STBLEN) = CHAR (13)

*        significant record should be available, convert every
*        character in record to corresponding code

         DO I2=1,STBLEN
            ASCII(I2) = ICHAR (STBUF2(I2))
            CODES(I2) = ITYPE(ASCII(I2))
         END DO

*        common variables
         ISP    = 0
         IP     = 0
         TOKEN  = 0

*        internal variables
         ISTCK  = 0
         NSTCK  = 0

      ELSE IF (ITASK.EQ.3.AND.ISTCK.EQ.NSTCK) THEN

*        lexical analysis must move forward and this part of the buffer
*        has not yet been analysed

*        increase number of stack elements, anticipating on the
*        token to be found
         NSTCK  = NSTCK+1
         IF (NSTCK.GT.MNSTCK) CALL FATALERR
     &      ('LEXPARSE','Internal error, too many stack elements')
         ISP = IP+1
         IF (ISP.LT.STBLEN) THEN
*           pointer has not progressed until end_of_line
            TOKEN = 0
            TMPP  = ISP
            DO I1=1,NJMP
               ISTATE = JMPPTR(I1)
               IP     = ISP
70             IF (ISTATE.GT.0) THEN
                  ISTATE = LEXJTB(ISTATE,CODES(IP))
                  IP     = IP+1
               GOTO 70
               END IF

               IF (ISTATE.LT.0) THEN
*                 jumptable jumping has stopped at valid parser
                  IP = IP-2
                  IF (IP.GE.TMPP) THEN
*                    parsing has progressed further than any previous
*                    parser
                     TMPP  = IP
                     TOKEN = ABS (ISTATE)
                  END IF
               END IF

            END DO

            IP = TMPP

*           decoding section
            IF (TOKEN.EQ.TFLOAT) THEN
               CALL RDDECD (ISP,IP)
               RESTCK(NSTCK) = VFLOAT
               VTSTCK(NSTCK) = 'F'
               VTYPE         = 'F'
            ELSE IF (TOKEN.EQ.TINTEG) THEN
               CALL RDDECI (ISP,IP)
               INSTCK(NSTCK) = VINT
               VTSTCK(NSTCK) = 'I'
               VTYPE         = 'I'
            ELSE IF (TOKEN.EQ.TLOGIC) THEN
               CALL RDDECL (ISP,IP)
               LOSTCK(NSTCK) = VLOGIC
               VTSTCK(NSTCK) = 'L'
               VTYPE         = 'L'
            ELSE IF (TOKEN.EQ.TTIME) THEN
               CALL RDDECT (ISP,IP)
               TISTCK(NSTCK) = VTIME
               VTSTCK(NSTCK) = 'T'
               VTYPE         = 'T'
            ELSE IF (TOKEN.EQ.TSTRNG) THEN
               VTSTCK(NSTCK) = 'C'
               VTYPE         = 'C'
            ELSE IF (TOKEN.EQ.TMISS) THEN
               VTYPE         = '-'
            ELSE
               VTSTCK(NSTCK) = ' '
               VTYPE         = ' '
            END IF

         ELSE
            TOKEN = TEOL
            IP    = ISP
         END IF

         BPSTCK(NSTCK) = ISP
         EPSTCK(NSTCK) = IP
         TKSTCK(NSTCK) = TOKEN
         ISTCK         = NSTCK

      ELSE IF (ITASK.EQ.3.AND.ISTCK.LT.NSTCK) THEN

*        lexical analysis can move forward but token has already been
*        found prior
         ISTCK  = ISTCK+1

*        (this list is identical to the list in the next section)
         ISP    = BPSTCK(ISTCK)
         IP     = EPSTCK(ISTCK)
         TOKEN  = TKSTCK(ISTCK)

         VINT   = INSTCK(ISTCK)
         VFLOAT = RESTCK(ISTCK)
         VLOGIC = LOSTCK(ISTCK)
         VTIME  = TISTCK(ISTCK)
         VTYPE  = VTSTCK(ISTCK)

      ELSE IF (ITASK.EQ.4) THEN

*        lexical analysis must move back, but then
*        part of buffer has already been analysed

         ISTCK = ISTCK-1
         IF (ISTCK.GE.1) THEN
*           backwarding yields a token

*           (this list is identical to the previous list)
            ISP    = BPSTCK(ISTCK)
            IP     = EPSTCK(ISTCK)
            TOKEN  = TKSTCK(ISTCK)

            VINT   = INSTCK(ISTCK)
            VFLOAT = RESTCK(ISTCK)
            VLOGIC = LOSTCK(ISTCK)
            VTIME  = TISTCK(ISTCK)
            VTYPE  = VTSTCK(ISTCK)

         ELSE IF (ISTCK.EQ.0) THEN
*           parsing has arrived at the point prior to the first
*           character flag that beginning of line is reached
            ISP    = 0
            IP     = 0
            TOKEN  = 0
         ELSE
*           backwarding goes too far back
            CALL FATALERR ('RDLEX   ','Internal error, empty stack')
         END IF
      ELSE IF (ITASK.EQ.5) THEN
*        set backup pointer to beginning of record
         ISTCK  = 0
         ISP    = 0
         IP     = 0
         TOKEN  = 0
      END IF

      RETURN
      END

      SUBROUTINE RDDECD (IB,IE)

*     PURPOSE:
*           SUBROUTINE to decode reals. Reals are decoded into a double
*     precision variable ranges are: exponent +/- 38, up to 15
*     digits (whether or not significant)

*     Double precision
*     ================
*     VAX (D_Floating package, default)
*     appr. 0.29d-38 to 1.7d+38, 16 digits accuracy

*     MS-Fortran (IEEE)
*     2.225073858507201d-308 to 1.797693134862316d+308,
*     15 digits accuracy

*     LS-Fortran (Mac)
*     2.3d-308 to 1.7d+308, 15 digits accuracy

*     Single precision
*     ================
*     VAX (D_Floating package, default)
*     appr. 0.29e-38 to 1.7e+38, 7 digits accuracy

*     MS-Fortran (IEEE)
*     1.1754944e-38 to 3.4028235e+38, 6 digits accuracy

*     LS-Fortran
*     1.2e-38 to 3.4e38, appr. 7 digits accuracy

*     INPUT_ARGUMENTS:
*     IB -
*     IE -

*     AUTHOR(S)   : Daniel van Kraalingen
*     E-MAIL      : d.w.g.vankraalingen@ab.dlo.nl
*     VERSION     : 1.0
*     DATE        : 30-September-1997
*     ORGANISATION:
*     Research Institute for Agrobiology and Soil Fertility (AB-DLO).
*     P.O. Box 14, 6700 AA, Wageningen, The Netherlands.

      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER IB, IE

**    Include file
      INCLUDE 'rdrecinf.inc'
      INCLUDE 'rddecinf.inc'

*     maximum number of significant digits
      INTEGER SIGDGM
      PARAMETER (SIGDGM=15)

*     maximum absolute value of exponent
      INTEGER EXPOM
      PARAMETER (EXPOM=99)

*     Local variables
      INTEGER I2,SIGN,SIGNE,ITMP,IDOT,ICHR,EXPO,IDIG(48:57),DIST,SIGDIG
      LOGICAL EXPFND,SGDFND
      INTEGER MULLB, MULUB
      PARAMETER (MULLB=-15, MULUB=15)
      DOUBLE PRECISION MULTIA(MULLB:MULUB),DDIG(48:57), MULTI

      SAVE

      DATA DDIG /0.D0,1.D0,2.D0,3.D0,4.D0,5.D0,6.D0,7.D0,8.D0,9.D0/
      DATA IDIG /0,1,2,3,4,5,6,7,8,9/
      DATA MULTIA /1.D-15, 1.D-14, 1.D-13, 1.D-12, 1.D-11,
     &             1.D-10, 1.D-9 , 1.D-8 , 1.D-7 , 1.D-6 ,
     &             1.D-5 , 1.D-4 , 1.D-3 , 1.D-2 , 1.D-1 ,
     &             0.D+0 ,
     &             1.D+0 , 1.D+1 , 1.D+2 , 1.D+3 , 1.D+4 ,
     &             1.D+5 , 1.D+6 , 1.D+7 , 1.D+8 , 1.D+9 ,
     &             1.D+10, 1.D+11, 1.D+12, 1.D+13, 1.D+14/

      IF (STBUF2(IB).EQ.'-') THEN
*        minus sign found, shift start of decoding one to the right
         SIGN = -1
         ITMP = IB+1
      ELSE IF (STBUF2(IB).EQ.'+') THEN
*        plus sign found, also shift start of decoding one to the right
         SIGN = +1
         ITMP = IB+1
      ELSE
*        no sign found, default sign is positive
         SIGN = +1
         ITMP = IB
      END IF

*     search for dot, if not found, search subsequently for E,e,D and d
*     to enable e.g. 3e4 to be decoded as a real
      IDOT = INDEX (STBUF(IB:IE),'.')
      IF (IDOT.EQ.0) THEN
         IDOT = INDEX (STBUF(IB:IE),'E')
         IF (IDOT.EQ.0) THEN
            IDOT = INDEX (STBUF(IB:IE),'e')
            IF (IDOT.EQ.0) THEN
               IDOT = INDEX (STBUF(IB:IE),'D')
               IF (IDOT.EQ.0) THEN
                  IDOT = INDEX (STBUF(IB:IE),'d')
                  IF (IDOT.EQ.0) THEN
C                     write (*,*) stbuf(ib:ie)
                     CALL FATALERR ('RDDECD','internal error')
                  END IF
               END IF
            END IF
         END IF
      END IF
      IDOT = IB+IDOT-1

*     set exponent_found and significant_digit_found
      EXPFND = .FALSE.
      SGDFND = .FALSE.
      SIGDIG = 0
      VFLOAT = 0.D0

*     go to end of string, if exponent character is found, jump out
*     before the end of the string

      DO 10 I2=ITMP,IE

         ICHR = ASCII(I2)

         IF (ICHR.GE.49.AND.ICHR.LE.57) THEN
*           digit found

*           start counting digits on the first non zero
            IF (SGDFND) THEN
               SIGDIG = SIGDIG+1
            ELSE
               SGDFND = .TRUE.
               SIGDIG = 1
            END IF

            DIST = IDOT-I2
            IF (DIST.GE.MULLB.AND.DIST.LE.MULUB) THEN
*              digit number not outside range
               VFLOAT = VFLOAT+MULTIA(DIST)*DDIG(ICHR)
            ELSE IF (DIST.GT.MULUB) THEN
               MULTI  = 10.D0**(DIST-1)
               VFLOAT = VFLOAT+MULTI*DDIG(ICHR)
            ELSE IF (DIST.LT.MULLB) THEN
               MULTI  = 10.D0**DIST
               VFLOAT = VFLOAT+MULTI*DDIG(ICHR)
            END IF
         ELSE IF (ICHR.EQ.48) THEN
            IF (SGDFND) SIGDIG = SIGDIG+1
         ELSE IF (STBUF2(I2).EQ.'.') THEN
*           dot found, do nothing
            CONTINUE
         ELSE IF (INDEX ('EeDd',STBUF2(I2)).NE.0) THEN
*           exponent character found, shift start of exponent decoding
*           one to the right
            ITMP   = I2+1
            EXPFND = .TRUE.
            GOTO 20
         END IF
10    CONTINUE

20    CONTINUE

      IF (EXPFND) THEN
*        exponent found, process sign and value
         IF (STBUF2(ITMP).EQ.'-') THEN
            SIGNE = -1
            ITMP  = ITMP+1
         ELSE IF (STBUF2(ITMP).EQ.'+') THEN
            SIGNE = +1
            ITMP  = ITMP+1
         ELSE
            SIGNE = +1
         END IF
         EXPO = 0
         DO 30 I2=ITMP,IE
            EXPO = EXPO*10+IDIG(ASCII(I2))
30       CONTINUE
      END IF

*     check whether sign before number was found
      IF (SIGN.EQ.-1) VFLOAT = -VFLOAT

      IF (EXPFND) THEN
*        exponent found, see if it within the valid range
         IF (EXPO.LE.EXPOM) THEN
            VFLOAT = VFLOAT*10.D0**(SIGNE*EXPO)
         ELSE
            CALL RDERR (2,'Exponent outside limits')
         END IF
      END IF

      IF (SIGDIG.GT.SIGDGM)
     &   CALL RDERR (3,
     & 'Many digits, possible loss of accuracy, even with RD*DOU calls')

      RETURN
      END

      SUBROUTINE RDDECI (IB,IE)

*     PURPOSE:
*           SUBROUTINE to decode integers, range:
*     -2147483647 < value < +2147483647

*     INPUT_ARGUMENTS:
*     IB -
*     IE -

*     AUTHOR(S)   : Daniel van Kraalingen
*     E-MAIL      : d.w.g.vankraalingen@ab.dlo.nl
*     VERSION     : 1.0
*     DATE        : 30-September-1997
*     ORGANISATION:
*     Research Institute for Agrobiology and Soil Fertility (AB-DLO).
*     P.O. Box 14, 6700 AA, Wageningen, The Netherlands.

      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER IB,IE

**    Include file
      INCLUDE 'rdrecinf.inc'
      INCLUDE 'rddecinf.inc'

*     Local variables
      INTEGER ISPTMP,I2, SIGN, NDIG
      INTEGER DIGITS(48:57), IVAL
      LOGICAL OVERFL
      SAVE

      DATA DIGITS /0,1,2,3,4,5,6,7,8,9/

*     decode an integer, it must be a valid integer !! (is
*     already check by RDLEX)

*     find out if digits begin at the first character or not
      IF (STBUF2(IB).EQ.'-') THEN
         ISPTMP = IB+1
         SIGN   = -1
      ELSE IF (STBUF2(IB).EQ.'+') THEN
         ISPTMP = IB+1
         SIGN   = 1
      ELSE
         ISPTMP = IB
         SIGN   = 1
      END IF

*     calculate number of digits
      NDIG  = IE-ISPTMP+1

*     check for overflow
      IF (NDIG.LT.10) THEN
         OVERFL = .FALSE.
      ELSE IF (NDIG.EQ.10) THEN
         OVERFL = LGT (STBUF(ISPTMP:IE),'2147483647')
      ELSE
         OVERFL = .TRUE.
      END IF

      IF (.NOT.OVERFL) THEN
         IVAL = DIGITS(ASCII(ISPTMP))
         DO 10 I2=ISPTMP+1,IE
            IVAL = IVAL*10+DIGITS(ASCII(I2))
10       CONTINUE
         VINT = IVAL*SIGN
      ELSE
*        write overflow message
         VINT = 99
         CALL RDERR (2,'Integer value too large')
      END IF

      RETURN
      END

**    USAGE:

      SUBROUTINE RDDECL (IB,IE)

*     PURPOSE:
*           SUBROUTINE to decode logicals

*     INPUT_ARGUMENTS:
*     IB -
*     IE -

*     AUTHOR(S)   : Daniel van Kraalingen
*     E-MAIL      : d.w.g.vankraalingen@ab.dlo.nl
*     VERSION     : 1.0
*     DATE        : 30-September-1997
*     ORGANISATION:
*     Research Institute for Agrobiology and Soil Fertility (AB-DLO).
*     P.O. Box 14, 6700 AA, Wageningen, The Netherlands.

      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER IB, IE

**    Include files
      INCLUDE 'rdrecinf.inc'
      INCLUDE 'rddecinf.inc'

*     Local variables
      CHARACTER LOGSTR*7
      SAVE

      IF (IE-IB+1.LE.7) THEN
         LOGSTR = STBUF(IB:IE)
         CALL UPPERC (LOGSTR)
         IF (LOGSTR.EQ.'.TRUE.') THEN
            VLOGIC = .TRUE.
         ELSE IF (LOGSTR.EQ.'.FALSE.') THEN
            VLOGIC = .FALSE.
         ELSE
            CALL RDERR (2,'not a logical value')
         END IF
      ELSE
         CALL RDERR (2,'not a logical value')
      END IF

      RETURN
      END

**    USAGE:

      SUBROUTINE RDDECT (IB,IE)

*     PURPOSE:
*           SUBROUTINE to decode date/time

*     INPUT_ARGUMENTS:
*     IB -
*     IE -

*     AUTHOR(S)   : Daniel van Kraalingen
*     E-MAIL      : d.w.g.vankraalingen@ab.dlo.nl
*     VERSION     : 1.0
*     DATE        : 30-September-1997
*     ORGANISATION:
*     Research Institute for Agrobiology and Soil Fertility (AB-DLO).
*     P.O. Box 14, 6700 AA, Wageningen, The Netherlands.

      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER IB, IE

**    Include files
      INCLUDE 'rdrecinf.inc'
      INCLUDE 'rddecinf.inc'

*     Local variables
      INTEGER INSEP,MNSEP
      PARAMETER (MNSEP=7)
      INTEGER I1, I2, IPSEP(0:MNSEP), DATEA(6),IFINDC
      INTEGER YRSEQ, DAYSEQ, HRSEQ
      CHARACTER MONN1(12)*3, MONN2(12)*9, MONTMP*9, MESSAG*80
      LOGICAL MONNUM,ERR
      REAL FSEC
      SAVE

      DATA MONN1 /'JAN','FEB','MAR','APR','MAY','JUN',
     &            'JUL','AUG','SEP','OCT','NOV','DEC'/
      DATA MONN2 /'JANUARY','FEBRUARY','MARCH','APRIL',
     &            'MAY','JUNE','JULY','AUGUST',
     &            'SEPTEMBER','OCTOBER','NOVEMBER','DECEMBER'/

*     SYNTAX CHECK NOG VIA VLAG AAN KUNNEN ZETTEN

*     initialize local storage
      DATEA(1) = 1900
      DATEA(2) = 1
      DATEA(3) = 1
      DATEA(4) = 0
      DATEA(5) = 0
      DATEA(6) = 0
      FSEC     = 0.

*     determine position of separators
      I1       = IB
      IPSEP(0) = IB-1
      INSEP    = 0

10    IF (I1.LE.IE.AND.INSEP.LT.MNSEP-1) THEN
         IF (INDEX ('-/:_.',STBUF2(I1)).NE.0) THEN
*           separator found
            INSEP = INSEP+1
            IPSEP(INSEP) = I1
         END IF
         I1 = I1+1
      GOTO 10
      END IF

*     add one separator at the end of the string
      IPSEP(INSEP+1) = IE+1

*     determine if a date is present

      IF (INDEX ('/-',STBUF2(IPSEP(1))).NE.0) THEN

*        date should be present
*        determine whether it is 1994-12-31 or 31-dec-1994 format by che
*        if there are digits between the 1st and 2nd separator
         MONNUM = .TRUE.
         DO 20 I1=IPSEP(1)+1,IPSEP(2)-1
            I2 = ICHAR(STBUF2(I1))
            IF (MONNUM.AND.(I2.LT.48.OR.I2.GT.57)) MONNUM = .FALSE.
20       CONTINUE
         IF (MONNUM) THEN
*           numeric month, expecting yy-mm-dd format
            YRSEQ  = 1
            DAYSEQ = 3
            CALL RDDECI(IPSEP(1)+1,IPSEP(2)-1)
            DATEA(2) = VINT
         ELSE
*           nonnumeric month, expecting dd-mm-yy format
            MONTMP = STBUF(IPSEP(1)+1:IPSEP(2)-1)
            CALL UPPERC (MONTMP)
            IF (IPSEP(2)-IPSEP(1)-1.EQ.3) THEN
*              expecting 3 letter month name
               I1 = IFINDC (MONN1,12,1,12,MONTMP(1:3))
            ELSE IF (IPSEP(2)-IPSEP(1)-1.GT.3) THEN
*              expecting full month name
               I1 = IFINDC (MONN2,12,1,12,MONTMP)
            ELSE
*              name has illegal length
               CALL RDERR (2,'length of month not correct')
            END IF

            IF (I1.GT.0) THEN
               DATEA(2) = I1
            ELSE
               CALL RDERR (2,'illegal month name')
            END IF

*           year should be in first position, day in last
            YRSEQ  = 3
            DAYSEQ = 1

         END IF

*        process year
         CALL RDDECI (IPSEP(YRSEQ-1)+1,IPSEP(YRSEQ)-1)
         DATEA(1) = VINT

*        process day number
         CALL RDDECI (IPSEP(DAYSEQ-1)+1,IPSEP(DAYSEQ)-1)
         DATEA(3) = VINT

      END IF

*     determine if a time is present and if so where it is present

      IF (STBUF2(IPSEP(1)).EQ.':') THEN
*        no date part before time part
         HRSEQ    = 1
         DATEA(1) = 0
         DATEA(2) = 0
         DATEA(3) = 0
      ELSE IF (INSEP.GE.3) THEN
*        date part precedes time part
         HRSEQ = 4
      ELSE
*        no time part at all
         HRSEQ = 0
      END IF

      IF (HRSEQ.GT.0) THEN

*        time is present, hour and minute should always be there

*        process hour
         CALL RDDECI (IPSEP(HRSEQ-1)+1,IPSEP(HRSEQ)-1)
         DATEA(4) = VINT

*        process minute
         CALL RDDECI (IPSEP(HRSEQ)+1,IPSEP(HRSEQ+1)-1)
         DATEA(5) = VINT

         IF (INSEP.GE.HRSEQ+1) THEN

*           integer part of seconds should also be there
            CALL RDDECI(IPSEP(HRSEQ+1)+1,IPSEP(HRSEQ+2)-1)
            DATEA(6) = VINT

            IF (INSEP.GE.HRSEQ+2.AND.
     &          IPSEP(HRSEQ+3)-IPSEP(HRSEQ+2).GT.1) THEN
*              fractional part of seconds should also be there
*              include decimal point !
               CALL RDDECD(IPSEP(HRSEQ+2),IPSEP(HRSEQ+3)-1)
               FSEC = REAL (VFLOAT)
            END IF

         END IF
      END IF

*     decode date to double precision real
      CALL DTSYS (1,DATEA,FSEC,VTIME,ERR,MESSAG)
      IF (ERR) THEN
         VTIME = 0.D0
         CALL RDERR (2,MESSAG)
      END IF

      RETURN
      END
