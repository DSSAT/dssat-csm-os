      SUBROUTINE RDTMP1 (ITASK,INT1,STRG)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER ITASK,INT1
      CHARACTER*(*) STRG

**    machine constants and buffer length
      INCLUDE 'rdmachin.inc'

*     includes
      INCLUDE 'rdrecinf.inc'
      INCLUDE 'rderrinf.inc'
      INCLUDE 'lextokin.inc'
      INCLUDE 'rddecinf.inc'

*     temporary file
*     --------------
*     IUN    - unit number used to open random access file for I/O
*     TOPREC - the highest record number in use
      INTEGER IUN, TOPREC

*     table variables in this subroutine
*     ----------------------------------
*     NCOLMX - maximum number of columns in table
*     TBLWID - number of columns in table
*     TBLREC - current writing record for TBLWID columns
*     TBLFBT - points to free byte for TBLWID columns
*     ICOL   - column number for current value
*
*     IBUF   - buffer array with INTEGER values
*     RBUF   - buffer array with REAL values, equivalenced with IBUF !!
*     LBUF   - buffer array with LOGICAL's, equivalenced with IBUF !!
*
*     IXL    - number of bytes used per item (4,8 or 1 for R,I and C)
*     IWN    - word number in buffer, calculated from pointer and IXL
*
      INTEGER NCOLMX, TBLWID, TBLREC, TBLFBT, ICOL
      PARAMETER (NCOLMX=100)
      DIMENSION TBLFBT(NCOLMX), TBLREC(NCOLMX)

      INTEGER IXL, IWN, NELMT
      DOUBLE PRECISION  RBUF(0:ILBUF/IRL-1,NCOLMX)

*     other
      INTEGER I,J,K,ITOLD,NUL,ILRTOP,IRECL,IBYTE,IREP
      INTEGER IPC, IXLMAX
      LOGICAL INIT,CONCAT,REPEAT,WRTDAT
      DIMENSION IPC(0:IIL-1)

      SAVE
      DATA INIT /.FALSE./, NUL/0/, ITOLD /0/


      IF (.NOT.INIT) THEN
*        initialize ; calculate values of array IPC
         IPC(IIL-1) = 1
         DO 10 I=IIL-2,0,-1
            IPC(I) = 256 * IPC(I+1)
10       CONTINUE

*        calculate IXLMAX as the maximum type length
         IXLMAX = MAX(IIL,IRL,ILL)

*        the number of the last double precision number on a record
         ILRTOP = ILBUF/IRL-1

         INIT  = .TRUE.
      END IF


      IF (ITASK.EQ.1) THEN
*        initialize TMP file writing ; open direct access file
         IRECL = ILBUF + IIL
         IUN   = INT1
         CALL FOPENG (IUN,STRG,'NEW','UD',IRECL,'DEL')

*        first free record
         TOPREC    = 2
*        initialize first value_record
         TBLREC(1) = TOPREC
         TBLFBT(1) = 0


      ELSE IF (ITASK.EQ.2) THEN
*        new table
         IF (.NOT.(ITOLD.EQ.1 .OR. ITOLD.EQ.8))
     $    CALL FATALERR ('RDTMP1','Internal_1')

*        reset table width
         TBLWID = 0


      ELSE IF (ITASK.EQ.4) THEN
*        initialize field for next column ; return pointer
         IF (INERR.EQ.0) THEN
*           increase table width and column number
            TBLWID = TBLWID + 1
            ICOL   = TBLWID

            IF (TBLWID.GT.NCOLMX) CALL FATALERR ('RDTMP1','Internal_2')

*           set record pointer
            IF (TBLWID.EQ.1) THEN
*              set pointer at multiple of IXLMAX
               TBLFBT(1) = IXLMAX * ((IXLMAX-1 + TBLFBT(1))/IXLMAX)
               IF (TBLFBT(1).EQ.ILBUF) THEN
*                 write full buffer to file
                  WRITE (IUN,REC=TBLREC(1)) (RBUF(K,1),K=0,ILRTOP),NUL
*                 get number of free record
                  TOPREC = TOPREC + 1
*                 set column buffer number and buffer pointer
                  TBLREC(1) = TOPREC
                  TBLFBT(1) = 0
               END IF
            ELSE
*              set pointer to start of new record
               TOPREC = TOPREC + 1
               TBLREC(TBLWID) = TOPREC
               TBLFBT(TBLWID) = 0
            END IF

*           pointer belonging to field
            INT1 = ILBUF * (TBLREC(TBLWID)-1) + TBLFBT(TBLWID)
         END IF


      ELSE IF (ITASK.EQ.6 .OR. ITASK.EQ.7) THEN
*        write value buffer to record buffer ; get input
         CONCAT = ITASK.EQ.7
         IREP   = INT1

*        data type output
         STRG = ' '

*        check for string type
         IF (CONCAT.AND.VTYPE.NE.'C')
     $      CALL FATALERR ('RDTMP1','Internal_3')

         IF (INERR.EQ.0) THEN
            IF (CONCAT) THEN
*              column number unchanged ; settings for string
c      write (30,'(a,i5)') ' concatenate on column:', icol
               NELMT = IP - ISP
               IXL   = IRL
*              decrease pointer to overwrote trailing CHAR(0)
               TBLFBT(ICOL) = TBLFBT(ICOL) - IRL
*              do not write repeat value
               REPEAT = .FALSE.
            ELSE
*              column number
               ICOL = 1 + MOD (ICOL,TBLWID)
c      write (30,'(a,i3)') 'rdtmp1 writes column',ICOL

*              number of elements NELMT and their length IXL
               IF (VTYPE.EQ.'I') THEN
*                 integer
                  NELMT = 1
                  IXL   = IRL
               ELSE IF (VTYPE.EQ.'F') THEN
*                 real
                  NELMT = 1
                  IXL   = IRL
               ELSE IF (VTYPE.EQ.'C') THEN
*                 character
                  NELMT = IP - ISP
                  IXL   = IRL
               ELSE IF (VTYPE.EQ.'L') THEN
*                 logical
                  NELMT = 1
                  IXL   = IRL
               ELSE IF (VTYPE.EQ.'T') THEN
*                 date_time
                  NELMT = 1
                  IXL   = IRL
               ELSE IF (VTYPE.EQ.'-') THEN
*                 missing value ; mirror IREP
                  NELMT = 1
                  IXL   = IRL
                  IREP  = -1 * IREP
               ELSE
*                 error
                  CALL FATALERR ('RDTMP1','illegal variable type')
               END IF

*              initialize pointer to multiple of IXLMAX bytes
               TBLFBT(ICOL) = IXLMAX* ((IXLMAX-1 + TBLFBT(ICOL))/IXLMAX)
*              write repeat value first
               REPEAT = .TRUE.
            END IF

*           write repeat value and NELMT data elements
            J = 1
            WRTDAT = .TRUE.
80          IF (WRTDAT) THEN
*              record filled ?
               IF (TBLFBT(ICOL).GE.ILBUF) THEN
*                 get number of free record
                  TOPREC = TOPREC + 1
c      write (30,'(a,i3,a,i3)') ' ----', TBLREC(ICOL),' to',TOPREC
*                 write buffer to file with pointer to new
                  WRITE (IUN,REC=TBLREC(ICOL))
     $             (RBUF(K,ICOL),K=0,ILRTOP),(TOPREC-1)*ILBUF
*                 reset column buffer number
                  TBLREC(ICOL) = TOPREC
*                 reset record pointer
                  TBLFBT(ICOL) = 0
               END IF

               IF (REPEAT) THEN
*                 write repeat value as real
                  RBUF(TBLFBT(ICOL)/IRL,ICOL) = DBLE(IREP)
c       write (30,'(i3,4(A,I6))')
c     $  ICOL,': repeat value ',irep,
c     $ ' TBLFBT=',TBLFBT(icol),' record=',tblrec(icol),
c     $ ' word=',tblfbt(icol) / iil
*                 use at least IXLMAX bytes for this integer
                  IF (IREP.GT.0) THEN
*                    use IRL bytes
                     TBLFBT(ICOL) = TBLFBT(ICOL) + IRL
*                    disable repeat reading
                     REPEAT = .FALSE.
                  ELSE
*                    missing value, use IRL bytes, don't write data
                     TBLFBT(ICOL) = TBLFBT(ICOL) + IRL
                     WRTDAT = .FALSE.
                  END IF
               GOTO 80
               END IF

*              word number
               IWN  = TBLFBT(ICOL) / IXL

c        write (30,'(i3,4(A,I6))')
c     $  ICOL,': toprec=',toprec,
c     $ ' TBLFBT=',TBLFBT(ICOL),' record=',TBLREC(ICOL),
c     $ ' word=',iwn
*              write data item in buffer
               IF (VTYPE.EQ.'F') THEN
*                 floating point
                  RBUF(IWN,ICOL) = VFLOAT
               ELSE IF (VTYPE.EQ.'I') THEN
*                 integer
                  RBUF(IWN,ICOL) = DBLE(VINT)
               ELSE IF (VTYPE.EQ.'L') THEN
*                 logical
                  if (VLOGIC) then
                     RBUF(IWN,ICOL) = +1.0d0
                  else
                     RBUF(IWN,ICOL) = -1.0d0
                  end if
               ELSE IF (VTYPE.EQ.'T') THEN
*                 date_time
                  RBUF(IWN,ICOL) = VTIME
               ELSE IF (VTYPE.EQ.'C') THEN
*                 string code to real ; byte number and integer
                  IF (J.LT.NELMT) THEN
*                    take character from line buffer
                     IBYTE = ASCII(ISP+J)
                  ELSE
*                    last byte is zero
                     IBYTE = 0
                  END IF
                  RBUF(IWN,ICOL) = DBLE(IBYTE)
               END IF

*              increase buffer pointer
               TBLFBT(ICOL) = TBLFBT(ICOL) + IXL

*              next data item
               J = J + 1
               WRTDAT = J.LE.NELMT
c      write (30,'(2a,i6)') '--- after writing:',
c     $ 'TBLFBT(icol) = ',TBLFBT(icol)
            GOTO 80
            END IF
         END IF


      ELSE IF (ITASK.EQ.8) THEN
*        terminate table ; flush column buffers
         IF (INERR.EQ.0) THEN
            DO 50 I=1,TBLWID
               IF (TBLREC(I).LT.TOPREC) THEN
*                 write buffer which will not be used
c                  write (30,'(a,i5)')
c     $              ' eot: write record',TBLREC(I)
                  WRITE (IUN,REC=TBLREC(I)) (RBUF(K,I),K=0,ILRTOP),NUL
               ELSE IF (I.GT.1) THEN
*                 buffer 1 becomes the top record
                  DO 40 K=0,ILRTOP
                     RBUF(K,1) = RBUF(K,I)
40                CONTINUE
                  TBLREC(1) = TOPREC
                  TBLFBT(1) = TBLFBT(I)
               END IF
50          CONTINUE
         END IF


      ELSE IF (ITASK.EQ.9) THEN
*        terminal call ; delete TMP file ?
         IF (ITOLD.NE.8) CALL FATALERR ('RDTMP1','Internal_4')
         IF (INERR.EQ.0) THEN
*           flush buffer 1
c            write (30,'(a,i5)') ' terminal: dump record',TBLREC(1)
            WRITE (IUN,REC=TBLREC(1)) (RBUF(K,1),K=0,ILRTOP),NUL

*           write info for RDTMP2 to first record
            WRITE (IUN,REC=1) TOPREC,ILBUF,IIL

         ELSE
*           delete TMP file
            CLOSE (IUN,STATUS='DELETE')
         END IF
      ELSE
*        unknown task
         CALL FATALERR ('RDTMP1','Internal_5')
      END IF

      ITOLD = ITASK

      RETURN
      END
