C=======================================================================
C  FIND, Subroutine, J.W.Jones, 01/03/91
C  Finds appropriate SECTION in a file of logical unit number LUNUM by
C  searching for a 6-character NAME at beginning of each line.
C-----------------------------------------------------------------------
C  INPUT : LUNUM  - logical unit number of the file to read
C          NAME  - 6-character variable name of section to find
C  OUTPUT: LNUM  - Line number of the file currently being read
C          FOUND - Indicator of completion of find routine
C                    0 - End-of-file encountered i.e. name not found
C                    1 - NAME was found
C  LOCAL :
C  IFILE : LUNUM
C  NOTES : Modified N.B. Pickering, 08/27/91
C=======================================================================

      SUBROUTINE FIND(LUNUM,NAME,LNUM,FOUND)

      IMPLICIT NONE
      EXTERNAL UPCASE
      INTEGER FOUND,I,LNUM,LUNUM
      CHARACTER SECTION*6,NAME*6,UPCASE*1
C
C     Initialization.
C
      FOUND = 0
      LNUM  = 1
      DO I = 1, LEN(NAME)
         NAME(I:I) = UPCASE(NAME(I:I))
      END DO
C
C     Loop to read through data file.
C
   10 IF (.TRUE.) THEN
         READ(LUNUM,'(A)',END=20) SECTION
         DO I = 1,LEN(SECTION)
            SECTION(I:I) = UPCASE(SECTION(I:I))
         END DO
C
C        String found, set FOUND to 1, and exit loop.
C
         IF (NAME .EQ. SECTION) then
            FOUND = 1
            GOTO 20
C
C           String not found, set FOUND to 0.
C
          ELSE
            FOUND = 0
         ENDIF

         LNUM = LNUM + 1
         GOTO 10
      ENDIF

   20 RETURN
      END

C=======================================================================
C  FIND2, Subroutine, J.W.Jones, 01/03/91
C  Finds appropriate SECTION in a file of logical unit number LUNUM by
C  searching for a *-character NAME at beginning of each line.
!  Same as FIND, but no UPCASE function and NAME can be up 
!    to 50 characters long
C-----------------------------------------------------------------------
C  INPUT : LUNUM  - logical unit number of the file to read
C          NAME  - *-character variable name of section to find
C  OUTPUT: LNUM  - Line number of the file currently being read
C          FOUND - Indicator of completion of find routine
C                    0 - End-of-file encountered i.e. name not found
C                    1 - NAME was found
C  LOCAL :
C  IFILE : LUNUM
C  NOTES : Modified N.B. Pickering, 08/27/91
C=======================================================================

      SUBROUTINE FIND2(LUNUM,NAME,LNUM,FOUND)

      IMPLICIT NONE
      INTEGER FOUND,LNUM,LUNUM, LENGTH
      CHARACTER SECTION*50
      CHARACTER NAME*(*)
C
C     Initialization.
C
      FOUND = 0
      LNUM  = 1
      LENGTH = LEN(NAME)
C
C     Loop to read through data file.
C
   10 IF (.TRUE.) THEN
         READ(LUNUM,'(A)',END=20) SECTION
C
C        String found, set FOUND to 1, and exit loop.
C
         IF (NAME .EQ. SECTION(1:LENGTH)) THEN
            FOUND = 1
            GOTO 20
C
C           String not found, set FOUND to 0.
C
          ELSE
            FOUND = 0
         ENDIF

         LNUM = LNUM + 1
         GOTO 10
      ENDIF

   20 RETURN
      END

!=======================================================================
!  FIND_IN_FILE, Function, M.Jones, 2006
!  Finds NEEDLE in a file of logical unit number HAYSTACK by
!  searching first * characters in each LINE.
!  Same as FIND, but no UPCASE function and NEEDLE is variable length
!-----------------------------------------------------------------------
!  INPUT : HAYSTACK  - logical unit number of the file to read
!          NEEDLE    - 6-character variable name of section to find
!  OUTPUT: FIND_IN_FILE - Indicator of completion of find routine
!             0 - End-of-file encountered i.e. NEEDLE not found
!             1 - NEEDLE was found
!=======================================================================

      INTEGER FUNCTION FIND_IN_FILE(NEEDLE, HAYSTACK)
          IMPLICIT NONE

!         Function returns success (1) or failure (0) code
c         File unit number of file in which to search
          INTEGER HAYSTACK
c         Text to search for:
          CHARACTER*(*) NEEDLE
c         length of chars to search for: 
          INTEGER STRLEN
c         Line to read from file:
          CHARACTER*1024 LINE

c         ====================================
          FIND_IN_FILE = 0
          STRLEN = LEN_TRIM(NEEDLE)

c                 Search file for line starting with needle
                  DO WHILE (.TRUE.)
c                     Read a line
                      READ(HAYSTACK, '(A)', END=50) LINE
c                     Check if the first chars match:
                      IF (LINE(1:STRLEN) .EQ. TRIM(NEEDLE)) THEN 
                          FIND_IN_FILE = 1        
                          RETURN
                      ENDIF
                  ENDDO

   50             RETURN

      END FUNCTION FIND_IN_FILE



C=======================================================================
C  IGNORE, Subroutine, J.W.Jones, 01/03/91
C----------------------------------------------------------------------------
C  PURPOSE: To read lines as an n-character variable and check it
C           for a blank line or for a comment line denoted by ! in col 1.
C  INPUTS:  LUN - Logical unit number of the file to be read
C           LINEXP - Starting line number at which this routine begins to
C                    read the file
C  OUTPUTS: LINEXP - Line number last read by the routine
C           ISECT - Indicator of completion of IGNORE routine
C                   0 - End of file encountered
C                   1 - Found a good line to read
C                   2 - End of Section in file encountered, denoted by *
C                       in column 1
C           CHARTEST - n-character variable containing the contents of
C                      the last line read by the IGNORE routine
C----------------------------------------------------------------------------
C
      SUBROUTINE IGNORE(LUN,LINEXP,ISECT,CHARTEST)

      CHARACTER BLANK*(80),CHARTEST*(*)
      INTEGER   LENGTH, LUN,LINEXP,ISECT
      DATA BLANK/'                                                    '/

      LENGTH = LEN(CHARTEST)

      ISECT = 1
 30   READ(LUN,'(A)',ERR=70, END=70)CHARTEST
      LINEXP = LINEXP + 1

!     CHP 5/1/08
      IF (CHARTEST(1:1) == CHAR(26)) THEN
        GO TO 70
      ENDIF

C     Check to see if all of this section has been read
      IF(CHARTEST(1:1) .EQ. '*'  .OR. CHARTEST(1:1) .EQ. '$') THEN
C        End of section encountered
         ISECT = 2
         RETURN
      ENDIF
C
C     Check for blank lines and comments (denoted by ! in column 1)
      IF(CHARTEST(1:1).NE.'!' .AND. CHARTEST(1:1).NE.'@') THEN
!         IF(CHARTEST(1:80).NE.BLANK)THEN
         IF(CHARTEST(1:LENGTH).NE.BLANK)THEN
C           FOUND A GOOD LINE TO READ
            RETURN
         ENDIF
      ENDIF

      GO TO 30
C     To read the next line

 70   ISECT = 0
      RETURN
      END SUBROUTINE IGNORE

C=======================================================================
C  IGNORE2, Subroutine, J.W.Jones, 01/03/91
C----------------------------------------------------------------------------
C  PURPOSE: To read lines as an n-character variable and check it
C           for a blank line or for a comment line denoted by ! in col 1.
C           Also check for second tier of data as notated by @ in the first
C           column.
C----------------------------------------------------------------------------
! Revision history
! 06/14/2005 CHP Return w/ ISECT=2, when '*' in column 1 found (previously 
!                looked for next data line.
C----------------------------------------------------------------------------
C INPUTS:  LUN - Logical unit number of the file to be read
C          LINEXP - Starting line number at which this routine begins to
C                   read the file
C OUTPUTS: LINEXP - Line number last read by the routine
C          ISECT - Indicator of completion of IGNORE2 routine
C                  0 - End of file encountered
C                  1 - Found a good line to read
C                  2 - End of Section in file encountered ("*") found
C                  3 - Second tier headers found ("@" found)
C          CHARTEST - 80-character variable containing the contents of
C                     the last line read by the IGNORE2 routine
C=======================================================================
      SUBROUTINE IGNORE2(LUN,LINEXP,ISECT,CHARTEST)
      CHARACTER CHARTEST*(*)
      INTEGER LUN,LINEXP,ISECT, Length
!     CHARACTER BLANK*80
!     DATA BLANK/'                                                    '/
C----------------------------------------------------------------------------
      ISECT = 1
 30   READ(LUN,'(A)',ERR=70,END=70)CHARTEST
      LINEXP = LINEXP + 1
C     CHECK TO SEE IF ALL OF THIS SECTION HAS BEEN READ

      IF(CHARTEST(1:1) .EQ. '*' )THEN
C       INTERMEDIATE HEADER FOUND.  
        ISECT = 2
!        GOTO 30
        RETURN
      ENDIF

      IF(CHARTEST(1:1) .EQ.'@') THEN
C       NEXT TIER ENCOUNTERED
        ISECT = 3
        RETURN
      ENDIF
C
C     CHECK FOR BLANK LINES AND COMMENTS (DENOTED BY ! IN COLUMN 1)
      IF(CHARTEST(1:1).NE.'!' .AND. CHARTEST(1:1).NE.'@') THEN
!       IF(CHARTEST.NE.BLANK)THEN
        Length = Len_Trim(CHARTEST)
        IF (Length > 0) THEN
C         FOUND A GOOD LINE TO READ
          RETURN
        ENDIF
      ENDIF

      GO TO 30
C       TO READ THE NEXT LINE
 70   ISECT = 0

      RETURN
      END SUBROUTINE IGNORE2

C=======================================================================
C  HFIND, Subroutine  GPF 7/95
C  Finds appropriate HEADER in a file of logical unit number LUNUM
C  by searching for a 5-character NAME following the '@' at the
C  beginning of a header line
C-----------------------------------------------------------------------
C  INPUT  : LUNUM  logical unit of file to read
C           NAME   variable name of header section to find (5-char)
C  OUTPUT : LNUM   line number of file currently read
C           ISECT  return status of find routine
C                  0  - EOF, name not found
C                  1  - NAME found
C                  2  - End of section encountered, denoted by *
C=======================================================================
      SUBROUTINE HFIND(LUNUM,NAME,LNUM,ISECT)

      IMPLICIT NONE
      EXTERNAL UPCASE
      INTEGER ISECT,I,LNUM,LUNUM
      CHARACTER HEADER*5,NAME*(*),UPCASE*1,LINE*128
C
C     Initialization, save initial line
C
      ISECT = 1
      DO I = 1, LEN(NAME)
         NAME(I:I) = UPCASE(NAME(I:I))
      END DO

C     Loop to read through data file.

   10  IF (.TRUE.) THEN
         READ(LUNUM,'(A)',ERR=20,END=20) LINE
         LNUM = LNUM + 1

C     End of section

         IF (LINE(1:1) .EQ. '*') THEN
            ISECT = 2
            RETURN
         ENDIF

C     Header line

         IF (LINE(1:1) .EQ. '@') THEN
            HEADER='     '
            DO I=2,LEN(LINE)
               IF (LINE(I:I) .NE. ' ') THEN
                  LINE(I:I) = UPCASE(LINE(I:I))
               ENDIF
            ENDDO
            DO I=2,(LEN(LINE)-LEN(NAME)+1)
               HEADER(1:LEN(NAME)) = LINE(I:(I+LEN(NAME)-1))
               IF (HEADER(1:LEN(NAME)) .EQ. NAME) THEN
                 ISECT = 1
                 RETURN
               ENDIF
            ENDDO
         ENDIF
         GOTO 10
      ENDIF
   20 ISECT = 0
      RETURN
      END
C=======================================================================


C=======================================================================
C  READ_DETAIL, Subroutine, C. H. Porter
C-----------------------------------------------------------------------
C  Reads DETAIL.CDE file, searches for CODE within SECTION, returns TEXT
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  10/15/2007 CHP Written.
C========================================================================

      SUBROUTINE READ_DETAIL(LENCDE, LENTXT, CODE, SECTION, TEXT)
C-----------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE
      EXTERNAL ERROR, FIND_IN_FILE, get_dir, GETLUN

      INTEGER,          INTENT(IN)  :: LENCDE, LENTXT
      CHARACTER*(LENCDE), INTENT(IN)  :: CODE
      CHARACTER*(*),    INTENT(IN)  :: SECTION
      CHARACTER*(LENTXT), INTENT(OUT) :: TEXT

      CHARACTER*(LENCDE) FCODE
      CHARACTER*(LENTXT) FTEXT
      CHARACTER*10 FILECDE
      CHARACTER*120 DATAX
      CHARACTER*120 PATHX

      CHARACTER*6, PARAMETER :: ERRKEY = 'DETAIL'
      INTEGER FOUND, FIND_IN_FILE, ERR, LNUM, LUN
!      INTEGER IPX

      LOGICAL FEXIST    !, EOF

      DATA FILECDE /'DETAIL.CDE'/

C-----------------------------------------------------------------------
      TEXT = ''
      LNUM = 0
      DATAX = FILECDE
      INQUIRE (FILE = DATAX, EXIST = FEXIST)

      IF (.NOT. FEXIST) THEN
!       File does not exist in data directory, check directory
!         with executable.
        CALL GETARG(0,PATHX)
!        call path_adj(pathx)
        call get_dir(pathx,datax)
        datax = trim(datax)//filecde
!        IPX = LEN_TRIM(PATHX)
!        DATAX = PATHX(1:(IPX-12)) // FILECDE
D       DATAX = STDPATH // FILECDE
        INQUIRE (FILE = DATAX, EXIST = FEXIST)
      ENDIF        

      IF (.NOT. FEXIST) THEN
!       Last, check for file in C:\DSSAT45 directory
        DATAX = trim(STDPATH) // FILECDE
        INQUIRE (FILE = DATAX, EXIST = FEXIST)
      ENDIF

      IF (FEXIST) THEN
        CALL GETLUN('DTACDE',LUN)
        OPEN (LUN, FILE=DATAX, STATUS = 'OLD',IOSTAT=ERR)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY, ERR, DATAX, 0)
        FOUND = FIND_IN_FILE(SECTION,LUN)
        IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, DATAX, LNUM)
        DO WHILE (.TRUE.)    !.NOT. EOF(LUN)
          READ(LUN,'(A,T10,A)',END=200,IOSTAT=ERR) FCODE, FTEXT
          LNUM = LNUM + 1
          IF (ERR .NE. 0) CALL ERROR(ERRKEY, ERR, DATAX, LNUM)
          IF (CODE .EQ. FCODE) THEN
            TEXT = FTEXT
            EXIT
          ENDIF
        ENDDO
        CLOSE(LUN)
      ELSE
!       Detail.CDE file is missing -- stop program with message.
        CALL ERROR(ERRKEY, 29, FILECDE, 0)
      ENDIF
  200 CONTINUE
      RETURN
      END SUBROUTINE READ_DETAIL
C=======================================================================

!=======================================================================
!  Subroutine READA
!   Reads measured development and final harvest data from FILEA 
!   and maps measured data to appropriate headers for output to 
!   OVERVEIW.OUT
!-----------------------------------------------------------------------
!  Revision history:
!  08/12/2005 CHP Modified to read "alias" headers for some variables
C  02/09/2007 GH  Add path for fileA
!=======================================================================
      SUBROUTINE READA(FILEA, PATHEX, OLAB, TRTNUM, YRSIM, X)

!-----------------------------------------------------------------------
!     READ DEVELOPMENT AND FINAL HARVEST DATA FROM  FILEA
!-----------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE
      EXTERNAL GETLUN, IGNORE, INFO, YR_DOY

      INTEGER TRTNUM,ERRNUM,LUNA,LINEXP,ISECT,NTR,I, J
      INTEGER YRSIM,YR,ISIM
      INTEGER COUNT
!     Headers with aliases -- save column
      INTEGER HWAM, HWAH, BWAM, BWAH, PDFT, R5AT  

      REAL TESTVAL

      CHARACTER*6   OLAB(EvaluateNum), HD
      CHARACTER*6   HEAD(EvaluateNum)
      CHARACTER*6   DAT(EvaluateNum), X(EvaluateNum)  !, ERRKEY
      CHARACTER*12  FILEA
      CHARACTER*78  MSG(10)
	CHARACTER*80  PATHEX
	CHARACTER*92  FILEA_P
      CHARACTER*255 C255

      LOGICAL FEXIST

      FILEA_P = TRIM(PATHEX)//FILEA

C-----------------------------------------------------------------------
C     Initialize measured values to -99 before reading values
C
      X = '   -99'

      CALL GETLUN('FILEA', LUNA)
      LINEXP = 0

      INQUIRE (FILE = FILEA_P, EXIST = FEXIST)

      IF (FEXIST) THEN
        OPEN (LUNA,FILE = FILEA_P,STATUS = 'OLD',IOSTAT=ERRNUM)
        IF (ERRNUM .NE. 0) GOTO 5000
        CALL YR_DOY(YRSIM,YR,ISIM)

C       FIND THE HEADER LINE, DESIGNATED BY @TRNO
        DO WHILE (.TRUE.)
          READ(LUNA,'(A)',END=5000) C255
          LINEXP = LINEXP + 1
          IF (C255(1:1) .EQ. '@') EXIT    
        ENDDO

C       FOUND HEADER LINE, SAVE IT IN HEAD AND SEARCH FOR TREATMENT
        DO I = 1,EvaluateNum
          READ(C255,'(1X,A5)') HEAD(I)
          IF (HEAD(I) .EQ. '     ') THEN
            COUNT = I - 1
            EXIT
          ENDIF
          C255 = C255(7:255)
        ENDDO

C       FIND THE RIGHT TREATMENT LINE OF DATA
        DO I = 1,1000
          CALL IGNORE(LUNA,LINEXP,ISECT,C255)
C
C    Return if no matching treatment is found in file FILA
C    No field measured data are necessary to be able to run the
C    model

          IF (ISECT .EQ. 0) GO TO 100
          READ(C255(1:6),'(2X,I4)',IOSTAT=ERRNUM) NTR
          IF (ERRNUM .NE. 0) GOTO 5000
          IF(NTR .EQ. TRTNUM) GO TO 60
        ENDDO
  
  60    CONTINUE
  
C       READ DATA LINE
        DO I = 1,COUNT
          READ(C255,'(A6)',IOSTAT=ERRNUM) DAT(I)
          IF (ERRNUM .NE. 0) GOTO 5000

          !Test for numeric value -- set non-numeric values to -99
          READ(C255,'(F6.0)',IOSTAT=ERRNUM) TESTVAL
          IF (ERRNUM .NE. 0 .AND. 
     &        TRIM(ADJUSTL(HEAD(I))) .NE. 'TNAM') THEN
            DAT(I) = '   -99'
          ENDIF 

          C255 = C255(7:255)
        ENDDO
  
!       Search for location within array of headers which can
!       contain the same data.  Store index for later use.
!       Pairs of headers:  
!       'HWAM' or 'HWAH' 
!       'BWAM' or 'BWAH' 
!       'PDFT' or 'R5AT' 
        HWAM = -99; HWAH = -99
        BWAM = -99; BWAH = -99
        PDFT = -99; R5AT = -99
        DO J = 1, EvaluateNum
          SELECT CASE (OLAB(J))
            CASE('HWAM  '); HWAM = J 
            CASE('HWAH  '); HWAH = J 
            CASE('BWAM  '); BWAM = J 
            CASE('BWAH  '); BWAH = J 
            CASE('PDFT  '); PDFT = J 
            CASE('R5AT  '); R5AT = J 
          END SELECT
        ENDDO

C       MATCH HEADER WITH DATA
        DO I = 2, COUNT   !Loop thru FILEA headers
          HD = ADJUSTL(HEAD(I))

!         For "alias" headers already know columns to store data
          SELECT CASE(HD)

          CASE ('HWAM')
            IF (HWAM > 0) THEN
              !store HWAM with HWAM header
              X(HWAM) = DAT(I)    
            ELSEIF (HWAH > 0) THEN
              !store HWAM with HWAH header 
              IF (X(HWAH) == '   -99') X(HWAH) = DAT(I)  
            ENDIF

          CASE ('HWAH')
            IF (HWAH > 0) THEN
              !store HWAH with HWAH header
              X(HWAH) = DAT(I)
            ELSEIF (HWAM > 0) THEN
              !store HWAH with HWAM header
              IF (X(HWAM) == '   -99') X(HWAM) = DAT(I) 
            ENDIF

          CASE ('BWAM')
            IF (BWAM > 0) THEN
              !store BWAM with BWAM header
              X(BWAM) = DAT(I)
            ELSEIF (BWAH > 0) THEN
              !store BWAM with BWAH header
              IF (X(BWAH) == '   -99') X(BWAH) = DAT(I)
            ENDIF

          CASE ('BWAH')
            IF (BWAH > 0) THEN
              !store BWAH with BWAH header
              X(BWAH) = DAT(I)
            ELSEIF (BWAM > 0) THEN
              !store BWAH with BWAM header
              IF (X(BWAM) == '   -99') X(BWAM) = DAT(I)
            ENDIF

          CASE ('PDFT')  
            IF (PDFT > 0) THEN
              !store PDFT with PDFT header
              X(PDFT) = DAT(I)
            ELSEIF (R5AT > 0) THEN
              !store PDFT with R5AT header
              IF (X(R5AT) == '   -99') X(R5AT) = DAT(I)
            ENDIF

          CASE ('R5AT')  
            IF (R5AT > 0) THEN
              !store R5AT with R5AT header
              X(R5AT) = DAT(I)
            ELSEIF (PDFT > 0) THEN
              !store R5AT with PDFT header
              IF (X(PDFT) == '   -99') X(PDFT) = DAT(I)
            ENDIF

!         No aliases for the rest
          CASE DEFAULT
            DO J = 1, EvaluateNum    !Loop thru crop-specific headers
              IF (OLAB(J) == HD) THEN
                X(J) = DAT(I)
                EXIT
              ENDIF
            ENDDO

          END SELECT
        ENDDO
  
 100    CONTINUE
        CLOSE(LUNA)
        RETURN

!       Error handling
 5000   CONTINUE
        X = '   -99'
        WRITE (MSG(1),'(" Error in FILEA - Measured data not used")')
        CALL INFO(1, "READA ", MSG)
      ENDIF

      CLOSE (LUNA)
      RETURN
      END SUBROUTINE READA

C=======================================================================
C  GETDESC, Subroutine C.H. Porter
C  Reads DATA.CDE to get descriptions for Measured and predicted data.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/01/2002 CHP Written.
C  12/17/2004 CHP Increased length of PATHX (path for executable) to 120.
C=======================================================================

      SUBROUTINE GETDESC(COUNT, OLAB, DESCRIP)

C-----------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE
      EXTERNAL GETLUN, GET_DIR, ERROR, INFO

      CHARACTER*6 CODE, OLAB(*)
      CHARACTER*6, PARAMETER :: ERRKEY = 'GETDSC'
      CHARACTER*8  FILECDE
      CHARACTER*50 DESCRIP(*), LONGTEXT
      CHARACTER*120 DATAX
      CHARACTER*78 MSG(3)
      CHARACTER*120 PATHX

      INTEGER COUNT, ERR, I, LUN, LNUM

      LOGICAL FEXIST    !, EOF

      DATA FILECDE /'DATA.CDE'/
C-----------------------------------------------------------------------

      DATAX = FILECDE
      INQUIRE (FILE = DATAX, EXIST = FEXIST)

      IF (.NOT. FEXIST) THEN
!       File does not exist in data directory, check directory
!         with executable.
        CALL GETARG(0,PATHX)
!        call path_adj(pathx)
        call get_dir(pathx,datax)
        datax = trim(datax)//filecde
!        IPX = LEN_TRIM(PATHX)
!        DATAX = PATHX(1:(IPX-12)) // FILECDE
        INQUIRE (FILE = DATAX, EXIST = FEXIST)
      ENDIF        

      IF (.NOT. FEXIST) THEN
!       Last, check for file in C:\DSSAT45 directory
        DATAX = trim(STDPATH) // FILECDE
        INQUIRE (FILE = DATAX, EXIST = FEXIST)
      ENDIF

      IF (FEXIST) THEN
        CALL GETLUN('DTACDE',LUN)
        OPEN (LUN, FILE=DATAX, STATUS = 'OLD', IOSTAT=ERR)
        IF (ERR /= 0) GOTO 100
        DO I = 1, COUNT
          DESCRIP(I) = ' '
          REWIND (LUN)
          LNUM = 0
          DO WHILE (.TRUE.)
            LNUM = LNUM + 1
            READ(LUN,'(A6,17X,A50)',END=20,ERR=20,IOSTAT=ERR)
     &          CODE,LONGTEXT
            IF (CODE .EQ. OLAB(I) .AND. ERR .EQ. 0) THEN
              DESCRIP(I) = LONGTEXT
              EXIT
            ENDIF
          ENDDO 
          !print *, i, " of ", count, olab(i), " ", descrip(i)
   20     IF (DESCRIP(I) .EQ. ' ') THEN
            DESCRIP(I) = OLAB(I)
            IF (ERR < 0) EXIT
          ENDIF
        ENDDO
        CLOSE(LUN)
        RETURN
      ELSE
!       Data.CDE file is missing -- stop program with message.
        CALL ERROR(ERRKEY, 29, FILECDE, 0)
      ENDIF

!     Data.cde file can not be found.  Just use OLAB (four character
!       code to fill description array.
  100   DO I = 1, COUNT
          DESCRIP(I) = OLAB(I)
        ENDDO

        WRITE(MSG(1),11) FILECDE
        WRITE(MSG(2),12) 
        WRITE(MSG(3),13) 
   11   FORMAT(' ',A8,' can not be found.')
   12   FORMAT(' Overview file will display variable labels ')
   13   FORMAT(' for simulated vs. measured data.')

        CALL INFO(3, ERRKEY, MSG)

C-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE GETDESC
C=======================================================================

C=======================================================================
C  CHANGE_DESC, Subroutine C.H. Porter
C  Change units from 'YrDoy' to 'DAP' in descriptions for which date
!     conversion has been done.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/01/2002 CHP Written.
C=======================================================================

      SUBROUTINE CHANGE_DESC(Descrip)

C-----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL UPCASE

      INTEGER StartCol, I, LENGTH
      CHARACTER*1 CHAR, UPCASE
      CHARACTER(50) Descrip, NewDesc
C-----------------------------------------------------------------------
!     Convert string to uppercase - save as NewDesc
      LENGTH = LEN(TRIM(Descrip))
      NewDesc = ""
      DO I = 1, LENGTH
        CHAR = Descrip(I:I)
        CHAR = UPCASE(CHAR)
        NewDesc = NewDesc(1:I-1) // CHAR
      ENDDO

!     Find occurance of '(YRDOY)' in description string
      StartCol = INDEX(NewDesc,'(YRDOY)')
      IF (StartCol .GT. 0) THEN
        Descrip = 
     &    Descrip(1:StartCol-1) // '(dap)  ' // Descrip(StartCol+7:50)
      ENDIF

C-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE CHANGE_DESC
C=======================================================================

C=======================================================================
C  READA_Dates, Subroutine C.H. Porter
C  Convert dates from READA (text) to ouptut format (integer)
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  05-14-2002 CHP Written.
C  05/07/2020 FO  Added new Y4K subroutine call to convert YRDOY
C=======================================================================
      SUBROUTINE READA_Dates(XDAT, YRSIM, IDAT)

      IMPLICIT NONE
      EXTERNAL ERROR, Y4K_DOY, YR_DOY

      CHARACTER*12 XDAT,ERRKEY
      REAL        RDAT
      INTEGER     ERRNUM, IDAT, ISIM, YR, YRSIM
      
      PARAMETER (ERRKEY = 'RADATE')

      CALL YR_DOY(YRSIM, YR, ISIM)            
      READ(XDAT(1:12),1000,IOSTAT=ERRNUM) RDAT
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,2,'FILEA',0)
 1000 FORMAT(F12.0)
      IDAT = INT(RDAT)

      IF (IDAT .GT. 0 .AND. IDAT .LT. 1000) THEN
        IF (IDAT .GT. ISIM) THEN
          IDAT = YR*1000 + IDAT
        ELSE
          IDAT = (YR+1)*1000 + IDAT
        ENDIF

      ELSEIF (IDAT .GT. 0 .AND. IDAT .GE. 1000) THEN
C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY
        !CALL Y2K_DOY(IDAT)
        CALL Y4K_DOY(IDAT,'READA ',0,ERRKEY,1)
        
        !CALL FullYear (IDAT, YR, DOY)
        !IDAT = YR*1000 + DOY
      ENDIF

      RETURN
      END SUBROUTINE READA_Dates
C=======================================================================

C=======================================================================
C  PARSE_HEADERS, Subroutine C.H. Porter
C  Reads a line of headers and determines column widths for 
C     corresponding data.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  11/05/2003 CHP Written.
C=======================================================================
      SUBROUTINE PARSE_HEADERS(CHAR, MAXCOL, HEADER, COUNT, COL)

      IMPLICIT NONE
      CHARACTER*(*) CHAR
      INTEGER MAXCOL
!     Up to MAXCOL headers per line, up to 10 characters long each
      CHARACTER*15  HEADER(MAXCOL), TEXT
      INTEGER COUNT, COL(MAXCOL,2), I, J, LENGTH
      LOGICAL SPACES

!     Initialize
      HEADER = '          '
      COUNT = 0
      COL = 0
      LENGTH = LEN(TRIM(CHAR))    !Don't use first character
      IF (LENGTH .LE. 1) RETURN   !No headers to parse, go back empty

      COUNT = 1
      COL(COUNT,1) = 1
      SPACES = .TRUE. !Allows for multiple spaces between headers

!     Look for spaces between headers
      DO I = 2, LENGTH       
!       A "!" signifies -- do not read the rest of the record 
        IF (CHAR(I:I) .EQ. '!') THEN
          LENGTH = I-1
          EXIT
        
        ELSEIF (CHAR(I:I) .EQ. ' ') THEN
          IF (SPACES) THEN
            CYCLE         !Treat multiple blanks as one
          ELSE
            COL(COUNT,2) = I - 1
            HEADER(COUNT) = ADJUSTL(CHAR(COL(COUNT,1):COL(COUNT,2)))
            COUNT = COUNT + 1
            COL(COUNT,1) = I + 1
            SPACES = .TRUE.
          ENDIF
        ELSE
          SPACES = .FALSE.
          CYCLE
        ENDIF
      ENDDO
      COL(COUNT,2) = LENGTH
      HEADER(COUNT) = ADJUSTL(CHAR(COL(COUNT,1):COL(COUNT,2)))
      HEADER(1) = ADJUSTL(CHAR(2:COL(1,2)))

!     Take out trailing periods from header names
      DO I = 1, COUNT
        TEXT = HEADER(I)
        LENGTH = LEN(TRIM(TEXT))
        DO J = LENGTH, 1, -1
          IF (TEXT(J:J) .EQ. '.' .OR. TEXT(J:J) .EQ. ' ') THEN
            LENGTH = LENGTH - 1
          ELSE
            EXIT
          ENDIF
        ENDDO
        HEADER(I) = TEXT(1:LENGTH)
      ENDDO
!--------------------------------------------------------------------
!     Need to adjust starting and ending columns for left-justified 
!         column headers
      DO I = 1, COUNT
        IF (TRIM(HEADER(I)) .EQ. 'SOIL_NAME') THEN
          !For SOIL_NAME, start in column 2 and read 50 characters
          COL(COUNT,1) = 2
          COL(COUNT,2) = 51
        ENDIF

        IF (TRIM(HEADER(I)) .EQ. 'SOIL_SCS') THEN
          !For SOIL_SCS, start 2 columns over from previous column 
          !  and read 50 characters
          IF (COUNT .GT. 1) THEN
            COL(COUNT,1) = COL(COUNT-1,2) + 2
            COL(COUNT,2) = COL(COUNT,1) + 49
          ELSE
            COL(COUNT,1) = 2
            COL(COUNT,2) = 51
          ENDIF
        ENDIF
      ENDDO

      RETURN
      END SUBROUTINE PARSE_HEADERS
C=======================================================================


C=======================================================================
C  ReadCropModels, Subroutine, C. H. Porter
C-----------------------------------------------------------------------
C  Reads SIMULATION.CDE file, reads Crop Models section
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  10/30/2007 CHP Written.
C========================================================================

      SUBROUTINE ReadCropModels(MaxNum, ModelName, CropName)
C-----------------------------------------------------------------------
      USE Moduledefs
      IMPLICIT NONE
      EXTERNAL ERROR, FIND_IN_FILE, get_dir, GETLUN, IGNORE

      INTEGER MaxNum
      CHARACTER*2   CropName(MaxNum)
      CHARACTER*5   ModelName(MaxNum)
      CHARACTER*14  FILECDE
      CHARACTER*23  SECTION
      CHARACTER*80  CHAR
      CHARACTER*120 DATAX
      CHARACTER*120 PATHX

      CHARACTER*6, PARAMETER :: ERRKEY = 'SIMCDE'
      INTEGER FOUND, ERR, LNUM, LUN
      INTEGER I, ISECT, FIND_IN_FILE

      LOGICAL FEXIST    !, EOF

      DATA FILECDE /'SIMULATION.CDE'/

C-----------------------------------------------------------------------
      DATAX = FILECDE
      INQUIRE (FILE = DATAX, EXIST = FEXIST)

      IF (.NOT. FEXIST) THEN
!       File does not exist in data directory, check directory
!         with executable.
        CALL GETARG(0,PATHX)
!        call path_adj(pathx)
        call get_dir(pathx,datax)
        datax = trim(datax)//filecde
!        IPX = LEN_TRIM(PATHX)
!        DATAX = PATHX(1:(IPX-12)) // FILECDE
D       DATAX = STDPATH // FILECDE
        INQUIRE (FILE = DATAX, EXIST = FEXIST)
      ENDIF        

      IF (.NOT. FEXIST) THEN
!       Last, check for file in C:\DSSAT45 directory
        DATAX = trim(STDPATH) // FILECDE
        INQUIRE (FILE = DATAX, EXIST = FEXIST)
      ENDIF

      IF (FEXIST) THEN
        CALL GETLUN('SIMCDE',LUN)
        OPEN (LUN, FILE=DATAX, STATUS = 'OLD',IOSTAT=ERR)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY, ERR, DATAX, 0)
        SECTION = '*Simulation/Crop Models'
        FOUND = FIND_IN_FILE(SECTION, LUN)
        IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, DATAX, LNUM)
        I = 0
        DO WHILE (.TRUE.)   !.NOT. EOF(LUN)
          CALL IGNORE(LUN,LNUM,ISECT,CHAR)
          IF (ISECT == 1) THEN
            I = I + 1
            READ(CHAR,*) ModelName(I), CropName(I)
            IF (I == MaxNum) EXIT
          ELSE
            EXIT
          ENDIF
        ENDDO
        CLOSE(LUN)
      ELSE
!       Simulation.CDE file is missing -- stop program with message.
        CALL ERROR(ERRKEY, 29, FILECDE, 0)
      ENDIF

      RETURN
      END SUBROUTINE ReadCropModels
C=======================================================================

C=======================================================================
C  READ_FILEA, Subroutine T. B. Ferreira
C  Reads FileA data.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/28/2024 CHP Written.
C=======================================================================

      SUBROUTINE READA_Y4K(FILEA, PATHEX, OLAB, TRTNUM, YRSIM, X)
        
        USE ModuleDefs
        IMPLICIT NONE
        
        EXTERNAL UPCASE, GETLUN, PARSE_HEADERS, IGNORE, INFO
      
        CHARACTER*1   UPCASE
        CHARACTER*6   OLAB(EvaluateNum) !HD
!        CHARACTER*6   HEAD(EvaluateNum)
        CHARACTER*12  X(EvaluateNum)  !, ERRKEY
        CHARACTER*12  FILEA
        CHARACTER*78  MSG(2)
	      CHARACTER*80  PATHEX
	      CHARACTER*92  FILEA_P
        CHARACTER*255 C255
        CHARACTER*20  DAT
        CHARACTER*6, PARAMETER :: ERRKEY = 'FILEA'
!       Headers with aliases -- save column
!        INTEGER HWAM, HWAH, BWAM, BWAH, PDFT, R5AT, NTR, YRPST 
        
        INTEGER TRTNUM,ERRNUM,LUNA,LINEXP,COLN, J
        INTEGER YRSIM, ACN, ECN, TR, I
        
        INTEGER, PARAMETER :: MAXCOLN = 40
        CHARACTER*15  HEADER(MAXCOLN), HTXT, FAHEADERS(MAXCOLN)
        INTEGER COL(MAXCOLN,2), ICOUNT, C1, C2, ISECT
        INTEGER C1D(MAXCOLN), C2D(MAXCOLN)

        LOGICAL FEXIST

        FILEA_P = TRIM(PATHEX)//FILEA
        ACN = 0
        X = '   -99'
        CALL GETLUN('FILEA', LUNA)
        
        INQUIRE(FILE=FILEA_P,EXIST=FEXIST)
        IF (FEXIST) THEN
          OPEN (LUNA,FILE = FILEA_P,STATUS = 'OLD',IOSTAT=ERRNUM)
          
C         FIND THE HEADER LINE, DESIGNATED BY @TRNO
          DO WHILE (.TRUE.)
            READ(LUNA,'(A)',END=5010) C255
            LINEXP = LINEXP + 1
            IF (C255(1:1) .EQ. '@') EXIT    
          ENDDO
        ENDIF
        
        CALL PARSE_HEADERS(C255, MAXCOLN, HEADER, ICOUNT, COL)
        DO COLN = 1, ICOUNT
          HTXT = HEADER(COLN)
          C1 = COL(COLN,1)
          C2 = COL(COLN,2)
!         uppercase every word in each column
          DO J = 1, LEN(TRIM(HTXT))
              HTXT(J:J) = UPCASE(HTXT(J:J))
          END DO
          HEADER(COLN) = HTXT
          
          IF(INDEX(HEADER(COLN),TRIM(HTXT)) .GT. 0) THEN
            ACN = ACN + 1
            C1D(ACN) = C1 !beginning
            C2D(ACN) = C2 !ending            
          
            FAHEADERS(ACN) = HEADER(COLN)

            IF(TRIM(HEADER(COLN)) .EQ. "R5AT") FAHEADERS(ACN) = "PDFT"
            IF(TRIM(HEADER(COLN)) .EQ. "PDFD") FAHEADERS(ACN) = "PDFT"
            IF(TRIM(HEADER(COLN)) .EQ. "BWAH") FAHEADERS(ACN) = "BWAM"
            IF(TRIM(HEADER(COLN)) .EQ. "HWAH") FAHEADERS(ACN) = "HWAM"
            IF(TRIM(HEADER(COLN)) .EQ. "CWAH") FAHEADERS(ACN) = "CWAM"
            IF(TRIM(HEADER(COLN)) .EQ. "HDAT") FAHEADERS(ACN) = "R8AT"
            
          ENDIF
        ENDDO

       
C       FIND THE RIGHT TREATMENT LINE OF DATA
        DO I = 1,1000
          CALL IGNORE(LUNA,LINEXP,ISECT,C255)
          
          IF (ISECT .EQ. 0) THEN
            WRITE (MSG(1),'(" Error in FILEA-Measured data not used")')
            CALL INFO(1, "READA ", MSG)
            CLOSE(LUNA)
            RETURN
          ENDIF

!         Search TRNO header        
          DO J = 1, ACN
            C1 = COL(J,1)
            C2 = COL(J,2)
          
            IF(TRIM(FAHEADERS(J)).EQ. 'TRNO') THEN            
                READ(C255(C1:C2+1),'(2X,I4)',IOSTAT=ERRNUM) TR

                IF(TR .EQ. TRTNUM) GO TO 65
            ENDIF
          ENDDO
        ENDDO      
  65    CONTINUE
    
        DO ECN = 1, EvaluateNum
          DO J = 1, ACN

!         TF - if statement to handle cases where the value has more 
!         line columns than the variable name
          IF(J .EQ. 1) THEN
            C1 = C1D(J)
            C2 = C1D(J+1)
          ELSEIF(J .EQ. ACN) THEN
            C1 = C2D(J-1)
            C2 = C2D(J)+1
          ELSE
            C1 = C2D(J-1)
            C2 = C1D(J+1)
          ENDIF
          IF(TRIM(FAHEADERS(J)) .EQ. TRIM(OLAB(ECN))) THEN

!           Check if the column before was TRNO since it ocuppies one
!           extract character after the last line column of the header
            IF(TRIM(FAHEADERS(J-1)) .EQ. 'TRNO') THEN
              READ(C255(C1+2:C2-1),'(A12)',IOSTAT=ERRNUM) DAT
              X(ECN) = TRIM(DAT)
            ELSE
              READ(C255(C1+1:C2-1),'(A12)',IOSTAT=ERRNUM) DAT
              X(ECN) = TRIM(DAT)
            ENDIF
          ENDIF
          ENDDO
        ENDDO  

      CLOSE (LUNA)
      RETURN
   
!       Error handling
 5010   CONTINUE
        X = '   -99'
        WRITE (MSG(1),'(" Error in FILEA - Measured data not used")')
        WRITE (MSG(1),'(I7)') YRSIM
        CALL INFO(1, "READA ", MSG)

      CLOSE (LUNA)
      RETURN
      
      END SUBROUTINE READA_Y4K

