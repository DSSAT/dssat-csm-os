      SUBROUTINE RDINDX (IERR,IUNIT,SETS,TOSCRX,TOLOGX,IUL,DATFIL,
     $   TMPFIL,NAMLIS,VARTYP,VARRAY,ILIND,
     $   IARLEN,INDPNT,ILPNT,INFND,INSETS)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER IERR,IUNIT,IUL,ILIND,IARLEN,INDPNT,ILPNT,INFND,INSETS
      CHARACTER DATFIL*(*),TMPFIL*(*),NAMLIS*(*),VARTYP*1,VARRAY*1
      DIMENSION NAMLIS(ILIND),VARTYP(ILIND),VARRAY(ILIND)
      DIMENSION IARLEN(ILPNT),INDPNT(ILPNT)
      LOGICAL SETS,TOSCRX,TOLOGX

**    include files
      INCLUDE 'rderrinf.inc'
      INCLUDE 'rdtblinf.inc'
      INCLUDE 'rdfilinf.inc'
      INCLUDE 'rdrecinf.inc'

*     local variables
      INTEGER IDUM, NROW, INFLD, IVOLD, LNAME
      CHARACTER CDUM*1
      LOGICAL EOF, STCOMP

*     functions called
      INTEGER IFINDC

      SAVE

      IDUM = 0

*     maximum variable name length
      INLMX = LEN (NAMLIS(1))
      IF (INLMX.GT.31) CALL FATALERR
     &   ('RDINDX','variable names too long')

*     file handling to common, initialize error system
      IUDF   = IUNIT + 1
      CALL RDERRI (DATFIL,IUDF,IUL,TOSCRX,TOLOGX)
      ILFILE = LEN_TRIM (DATFIL)

*     initialize record read system
      CALL RECREAD_INIT (IUDF,DATFIL)
      RECNO = 0

*     initialize Lexical analysis system
      CALL RDLEX (1)

      CALL RDTMP1 (1,IUNIT,TMPFIL)

*     initialize loop over tables
      INFLD  = 0
      INFND  = 0
      INSETS = 0
      EOF    = .FALSE.
      STCOMP = .TRUE.


10    IF (.NOT.EOF) THEN
*        analyze next table
         CALL RDSCTB (EOF)

*        number of rows
         NROW = NVAL / MAX (1,NCOL)

*        check names and types
         DO 20 ICOL=1,INAMES
*           increase output field counter
            INFLD = INFLD + 1

*           name previously used ?
            LNAME = IFINDC
     $       (NAMLIS,ILIND,1,INFND,TBLNAM(ICOL)(1:INLMX))

            IF (LNAME.EQ.0 .AND. INSETS.LE.1) THEN
*              accept new name in index
               INFND = INFND + 1
               IF (INSETS.EQ.0) INSETS = 1
               IF (INFND.GT.ILIND) THEN
*                 too many different names on file
                  CALL RDERR (4,'exceeded maximum number of names')
                  IF (SETS) THEN
                    IF (TOSCRX) WRITE (*,'(/,2A,/,A)')
     $            ' Too many names occur in the sets of rerun file ',
     $              DATFIL(1:ILFILE),
     $            ' Increase the value of ILNREP in RDMACHIN.INC.'
                    IF (TOLOGX) WRITE (IUL,'(/,2A,/,A)')
     $            ' Too many names occur in the sets of rerun file ',
     $              DATFIL(1:ILFILE),
     $            ' Increase the value of ILNREP in RDMACHIN.INC.'
                  END IF

                  IF (.NOT.SETS) THEN
                    IF (TOSCRX) WRITE (*,'(/,2A,/,A)')
     $            ' Too many names occur in the data file ',
     $              DATFIL(1:ILFILE),
     $            ' Increase the value of ILNDAT in RDMACHIN.INC.'
                    IF (TOLOGX) WRITE (IUL,'(/,2A,/,A)')
     $            ' Too many names occur in the data file ',
     $              DATFIL(1:ILFILE),
     $            ' Increase the value of ILNDAT in RDMACHIN.INC.'
                  END IF
                  CALL FATALERR (' ',' ')
               END IF
               NAMLIS(INFND) = TBLNAM(ICOL)
               VARTYP(INFND) = ' '
               VARRAY(INFND) = ' '
               IVOLD = INFND
               LNAME = INFND

            ELSE IF (LNAME.GT.0 .AND. SETS) THEN
*              variable name was previously found
               IF (LNAME.EQ.1 .AND. STCOMP) THEN
*                 start of new set
                  INSETS = INSETS + 1
               ELSE IF (LNAME.NE.IVOLD+1) THEN
*                 error in variable name order
                  CALL RDERR (4,'wrong name order in set')
               END IF
               STCOMP = LNAME.EQ.INFND
               IVOLD  = LNAME
            ELSE IF (LNAME.GT.0) THEN
*              old name illegal (no sets)
               CALL RDERR (4,'name already used')
            ELSE
*              new name illegal (in set)
               CALL RDERR (4,'name does not occur in previous set(s)')
            END IF

*           array length
            IF (.NOT.SETS) THEN
               IARLEN(INFLD) = NROW
               INDPNT(INFLD) = TBLPNT(ICOL)
            ELSE
               IF (INFLD.LE.ILPNT) THEN
                  IARLEN(INFLD) = NROW
                  INDPNT(INFLD) = TBLPNT(ICOL)
               ELSE
                  CALL SWPI4 (1,'RD\IARREP',IUNIT+2,
     &                        ILPNT,INFLD,NROW)
                  CALL SWPI4 (1,'RD\IPTREP',IUNIT+3,
     &                        ILPNT,INFLD,TBLPNT(ICOL))
               END IF
            END IF

            IF (VARTYP(LNAME) .EQ. ' ') THEN
*              variable not yet typed ; accept column type and array flag
               VARTYP(LNAME) = TBLTYP(ICOL)
               VARRAY(LNAME) = TBARAY
            ELSE IF (TBLTYP(ICOL).NE.' ') THEN
*              variable and column are both typed ; check type and array flag
               IF (TBLTYP(ICOL).NE.VARTYP(LNAME))
     $          CALL RDERR (4,'data type is not consistent')
               IF (VARRAY(LNAME).NE.TBARAY)
     $          CALL RDERR (4,'scalar / array inconsistency')
            END IF
20       CONTINUE
      GOTO 10
      END IF

*     final call's to RDTMP1 (delete TMP in case of errors)
      CALL RDTMP1 (9,IDUM,CDUM)

*     set number of errors
      IERR = INERR

      RETURN
      END
