c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     These subroutines are used by the CANEGRO Plant Module (sugarcane) for 
c     acquiring cultivar and species coefficients from file (or wherever).
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     Matthew Jones, October 2006
c     Gainesville, Florida
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::





c     Get a cultivar coefficient:
c     :::::::::::::::::::::::::::
c     This subroutine is perhaps theoretically best as a function.  The idea
c     is that it presents a first layer of abstraction; from here, the 
c     inputs can come from anywhere (file / internet / database / ?).  Usage
c     was intended as: 
c     x = Get_Cultivar_Coeff('x')
c     Creating a module that is also a function, however, is confusing, at 
c     the very least.  It might also corrupt the idea of a DSSAT module.  
c     For this reason, it is implemented as a subroutine:
c     CALL Get_Cultivar_Coeff(x, 'x', Control)
c     As a module, it can initialise itself, and is replaceable with a 
c     future that does the same thing, but perhaps gets the parameters in a 
c     different way (e.g. from a database).

!     2019-04-07 CHP Replace console output with messages to INFO, WARNING, or ERROR.OUT

c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     ========================================================================
c     SUBROUTINE GET_CULTIVAR_COEFF(COEFF, COEFF_NAME, CONTROL, ERROR)
c     ========================================================================
      SUBROUTINE GET_CULTIVAR_COEFF(
c                        [IO]: the coefficient
     -                   COEFF,
c                        [I]: name of coefficient
     -                   COEFF_NAME,
c                        [I]: DSSAT control variable
     -                   CONTROL,
c                        [I]: DSSAT simulation switch control
c     -                   ISWITCH,
c                        [O]: was there an error?
     -                   EERROR)

c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     ========================================================================
c     HEADER: Variables and declarations
c     ========================================================================
c     Use DSSAT modules:
          USE ModuleDefs
          USE CNG_ModuleDefs
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     Declare, and save, all variables:
          IMPLICIT NONE
          EXTERNAL GETLUN, WARNING, FIND, INFO, FINDNEXT, ERROR
          EXTERNAL FIND_IN_FILE
          SAVE
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     Declare variables:
c     ::::::::::::::::::
c     Subroutine parameters:
c     ::::::::::::::::::::::
c         The coefficient retrieved is stored in this variable
          REAL COEFF

c         The name of the coefficient to be searched for
c           Note: DSSAT cultivar coefficients are generally limited to
c           4 or 5 characters.  20 are provided by way of futureproofing.
          CHARACTER*(*) COEFF_NAME

c         The DSSAT Control variable (tells the module when to init, etc)
          Type(ControlType) CONTROL
c         The DSSAT simulation swtch data type:
c          TYPE (SWITCHTYPE) ISWITCH

c         An error variable: if the calling module determines that was
c         an error, it can decide what to do (use a default value, 
c         terminate excecution, etc)
          LOGICAL EERROR
c         ::::::::::::::::::::::::::
c         Specify intention:
c         ::::::::::::::::::
          INTENT(IN)  CONTROL, COEFF_NAME
c                     , ISWITCH
          INTENT(OUT) EERROR
          INTENT(INOUT) COEFF 
c     :::::::::::::::::::::::::
c     End of subroutine params
c     :::::::::::::::::::::::::

c     Local variables:
c     ::::::::::::::::

c         Array of coefficient names:
          CHARACTER*20 CF_NAMES(120)

c         Array of coefficient values:
          REAL CF_VALUES(120)

c         Array of logical values indicating whether or not a value
c         was actually read from file:
          LOGICAL CF_OK(120)

c         The name of the cultivar file
          CHARACTER*256 FILENAME

!c         The name of the ecotype file
!          CHARACTER*256 ECO_FNAME

!         Name of INH file (temporary input, like INP)
          CHARACTER*30 FILEIOH

c         The Code (IB***) of the cultivar (internal var) and eco type:
          CHARACTER*6 CULT_CODE, ECOTYPE

c         The cultivar and ecotype file IO unit numbers:
          INTEGER ECOFILE !CFILE, 

c         The Filex and fileio number:
!          INTEGER FILEX
          INTEGER FIO

c         A line of the cultivar file (to be read) from file, then
c         parsed:
          CHARACTER*2048 LINE, LINE2, ELINE, ELINE2

c         Has the file been opened?
          LOGICAL OPENED, FIO_OPENED

c         Temp cultivar number read from file:
!          INTEGER CULTNUM

c         Temp cultivar/ecotype names:
          CHARACTER*6 CULTNAME    !, ECO_NAME

c         Temp values:
          INTEGER BEGI, ENDI, I, PREVE, LINELEN

c         Temp. variable name:
          CHARACTER*20 VARNAME, CHTEMP

!CHP      Message text array to INFO.OUT
          CHARACTER*78 MSG(120)

C-KRT *****************************************************************
C-KRT Modified declaration statements for compatability with g95.
C-KRT     CHARACTER BLANK*1,ERRKEY*6,FILEC*12,FILECC*92,FILEIO*30,
c                   Eco file name from INP
C-KRT&              FILEE*12, FILEEE*92,
C-KRT&    PATHCR*80,SECTION*6, ERRMSG*78(1)          
	      CHARACTER BLANK*1,ERRKEY*6,FILEIO*30 !,FILEC*12,FILECC*92
	      CHARACTER FILEE*12, FILEEE*92
	      CHARACTER PATHER*80, SECTION*6  !PATHCR*80, 
	      CHARACTER*78 ERRMSG(1)
C-KRT *****************************************************************
          
          INTEGER LUNIO, ERRNUM, LNUM, FOUND, PATHL, FIND_IN_FILE
          INTEGER NParams

          PARAMETER (BLANK=' ')

c     ::::::::::::::::
c     ========================================================================
c     CODE
c     ========================================================================
c         Set these values to 0 (or equiv) for each subroutine call
c          COEFF = 0.
          LINE = ''
c     ========================================================================
c     RUNINIT: Initialisation for each run
c     ========================================================================
          IF (CONTROL%DYNAMIC .EQ. RUNINIT) THEN

c             The file must not be opened, so that the first call to
c             initialise a variable will open the input file and
c             initialise the coefficient arrays
              OPENED = .FALSE.
              FIO_OPENED = .FALSE.

c             Init arrays with default values:





c     ========================================================================
c     SEASINIT: Initialisation for each season
c     Note: all cultivar coefficients will be requested in the init section
c     anyway, so care must be taken not to reopen the cultivar file each time.
c     ========================================================================
          ELSEIF (CONTROL%DYNAMIC .EQ. SEASINIT) THEN

c              CF_OK = .FALSE.

c             GET the cultivar code from the FILEX.  Note, this might be easier
c             to get from the DSSAT45.INP file instead.
c             :::::::::::::::::::::::::::::::::::::::::
c             If the file is not already open:
c             ::::::::::::::::::::::::::::::::
c              IF (.NOT.(OPENED)) THEN
c                 Open the FileX to retrieve the cultivar code:
c                  CALL GETLUN('FILEX', FILEX)
c                  OPEN(UNIT=FILEX, FILE=CONTROL%FILEX, 
c     -                 ACCESS='SEQUENTIAL', ACTION='READ')
c                  OPENED = .TRUE.

c                 Search file for line '*CULTIVARS'
c                  CALL FIND_IN_FILE('*CULTIVARS', FILEX)
c                 Having found a match, 

c                 1.) skip a line
c                  READ(FILEX, '(A)', END=50) LINE
c                 2.) read each line checking that the
c                 read cultivar number matches stored cultivar number
c                  DO WHILE(.TRUE.)
c                      READ(FILEX, '(I2,4X,A6,X)', 
c     -                END=50) CULTNUM, CULTNAME
c                     ******* Check ******:
c                      IF (CULTNUM .EQ. 1) THEN
c                          CULT_CODE = CULTNAME
c                          EXIT
c                      ENDIF
c                  ENDDO



c             :::::::::::::::::::::::::::::::::::::::::::::::::::
              IF (.NOT.(FIO_OPENED)) THEN
c                 Get file handle number for FILEIO (INP file)
                  CALL GETLUN('FILEIO', FIO)
c                 Open the file:
                  OPEN(UNIT=FIO, FILE=CONTROL%FILEIO, 
     -                 ACCESS='SEQUENTIAL', ACTION='READ')
c                 Mark opened:
                  FIO_OPENED = .TRUE.
c                 Find the *CULTIVARS section of the file:
                  FOUND = FIND_IN_FILE('*CULTIVARS', FIO)

c                 Loop until cultivar is found:
                  DO WHILE (.TRUE.)
c                     Read a line (if all else fails, the loop exits by
c                     GOTO-ing to 50):
                      READ(FIO, '(A)', END=50) LINE
                         
c                     If the line is not blank:
                      IF (LEN(TRIM(LINE)) .GT. 0) THEN
c                         If the line is not a comment
                          IF (LINE(1:1) .NE. '!') THEN
c                             It really ought to be in the correct format in this section:
                              READ(LINE, '(6X, A6)') CULTNAME
                              IF (LEN(TRIM(CULTNAME)) .GT.  0) THEN
c                                 If the read-in CULTNAME seems ok, then set as CULT_CODE
                                  CULT_CODE = CULTNAME
                                  EXIT
                              ENDIF
                          ENDIF
                      ENDIF
                          
                      CONTINUE
                  ENDDO
c             ENDIF

c                 Close the file:
                  CLOSE(FIO)


c                 Some error handling...
c                 ::::::::::::::::::::::::::::::
c                 Very difficult to do this without GOTO statements.  Yuck.
   40             GOTO 60

   50             CONTINUE
                  MSG(1) = 
     &             "Could not read cultivar number from FILEX / FILEIO!"
                  MSG(2) = "Model will stop."
                  CALL WARNING (2,ERRKEY,MSG)
!                  WRITE(*,*) 'Could not read cultivar', 
!     -                       ' number from FILEX / FILEIO!'
!                  PAUSE

   60             CONTINUE
c                 ::: end of error handling ::::
c                 ::::::::::::::::::::::::::::::


c                 And close the file
c                  CLOSE(FILEX)
c                 Cultivar code is now known.
c                 :::::::::::::::::::::::::::::::

c                 :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c                 Now, open the cultivar file, and read the heading line, creating
c                 an array of headings; those headings will have corresponding
c                 values, read from DSSAT45.INP, because this will contain 
c                 possibly modified values from interactive mode.
c                 :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c                 Get fileIO unit number:
c                 Note: no specific file number is allocated to the cultivar file.
c                 DSSAT will select an unused one (hopefully) and return that.
!                  CALL GETLUN('CULT', CFILE)
                  CALL GETLUN('ECO', ECOFILE)

!c                 Open the file:
!c                 ::::::::::::::
!c                 Set filename (comes from INP file, but this is the default):
!                  FILENAME = STDPATH//'Genotype'//SLASH//'SCCAN045.CUL'
!
c                 Code copied from ETPHOT.FOR:
c                 ::::::::::::::::::::::::::::
c                 Get the cultivar/eco filenames from the INP file:
c                 :::::::::::::::::::::::::::::::::::::::
                  CALL GETLUN('FILEIO', LUNIO)
                  OPEN(LUNIO,FILE=Control%FILEIO,STATUS='OLD',
     &                 IOSTAT=ERRNUM)
                  IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)

                  SECTION = '*FILES'
                  CALL FIND(LUNIO,SECTION,LNUM,FOUND)
                  IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEIO,LNUM)

c                 Read ecotype file name
                  READ(LUNIO,'(/////,15X,A,1X,A)',IOSTAT=ERRNUM) 
     &                                              FILEE, PATHER
                  LNUM = LNUM + 6
                  IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,
     &                                          LNUM)

!c                 Read cultivar file name
!                  READ(LUNIO,'(15X,A,1X,A)',IOSTAT=ERRNUM) 
!     &                                              FILEC, PATHCR
!
!                  LNUM = LNUM + 1
!                  IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,
!     &                                          LNUM)

c                 Close the file
                  CLOSE(LUNIO)

!!                 08/04/09 CHP Separated FILECC from FILEEE, to allow separate pathnames
!                  PATHL  = INDEX(PATHCR,BLANK)
!                  IF (PATHL .LE. 1) THEN
!                    FILECC = FILEC
!                  ELSE
!                    FILECC = PATHCR(1:(PATHL-1)) // FILEC
!                  ENDIF
!                  FILENAME = FILECC
!
!c                 :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!c                 End of cultivar file path acquisition
!c                 Open:
!                  OPEN(UNIT=CFILE, FILE=FILENAME, ACCESS='SEQUENTIAL', 
!     -                 ACTION='READ')
!
!c                 Now, read the file until the line started '@var' is
!c                 encountered.  'var' no doubt stands for 'variety'
!c                 I would like to read this line, and then read the line
!c                 containing the actual cultivar.  Then the string of 
!c                 headings can be parsed into an array, along with the 
!c                 values (column widths defined by headings).
!
!c     ****   MATT: check that this will not be an infinite loop if the @var tokenc
!c     does not exist ******
!c     Ok, I think it will crash (EOF) if it cannot find @var
!                  DO WHILE(.TRUE.)
!c                     Clear line:
!                      DO I=1, 1024
!                           LINE(I:I) = ' '
!                           LINE2(I:I) = ' '
!                      ENDDO
!c                     Read a line:
!                      READ(CFILE, '(A)') LINE
!c                     Ignore comments:
!                      IF ((LINE(1:1)      .NE. '!') .AND.
!     -                    (LEN_TRIM(LINE) .GT.  0)) THEN
!c                         If the first bit is '@VAR#', we have found the headings
!                          IF (LINE(1:5) .EQ. '@VAR#') THEN
!!                             chp write to INFO.OUT
!                              !WRITE(*, '(A)') LINE
!                              MSG(1) = LINE(1:78)
!                              CALL INFO(1,'SCCAN_CULT',MSG)
!c                             We have discovered the heading line!
!c                             Search for cultivar
!                              DO WHILE(.TRUE.)
!                                  READ(CFILE, '(A)') LINE2
!!                                 chp write to INFO.OUT
!                                  !WRITE(*, '(A)') LINE2
!                                  MSG(1) = LINE2(1:78)
!                                  CALL INFO(1,'SCCAN_CULT',MSG)
!                                  IF (LINE2(1:1) .NE. '!') THEN
!c                                     If the line begins with the cultivar name, 
!c                                     we have found the cultivar
!                                      IF (LINE2(1:6) .EQ. CULT_CODE) 
!     &                                THEN
!c                                         Exit from loop, because LINE2 contains the
!c                                         cultivar info
!                                          EXIT
!                                      ENDIF
!                                  ENDIF
!                              ENDDO
!c                             Exit from loop, because LINE contains headings
!                              EXIT
!                          ENDIF
!                      ENDIF
!                  ENDDO
!c                 Close the cultivar file
!                  CLOSE(CFILE)

c                 LINE contains headings.
c                 LINE2 contains cultivar values.
c                 These are still in a single string, and need to be parsed.

!                 CHP 4/1/2010 Read cultivar info directly from INH file
                  I = LEN(TRIM(CONTROL % FILEIO))
                  FILEIOH = CONTROL % FILEIO
                  WRITE(FILEIOH(I:I),'(A1)') 'H'

                  OPEN(LUNIO,FILE=FILEIOH,STATUS='OLD',IOSTAT=ERRNUM)
                  IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIOH,0)

                  SECTION = '*CULTIVARS'
                  CALL FIND(LUNIO,SECTION,LNUM,FOUND)
                  IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEIO,LNUM)

c                 Read cultivar header info
                  READ(LUNIO,'(A)',IOSTAT=ERRNUM) LINE
                  READ(LUNIO,'(A)',IOSTAT=ERRNUM) LINE2
                  LNUM = LNUM + 2

c                 :::::::::::::::::::::::::::::::::::::::::::::::::::
c                 MJ, Mar 2008:
c                 ECOTYPE file
c                 :::::::::::::::::::::::::::::::::::::::::::::::::::
c                 Eco parameters are to be processed as if they were
c                 cultivar params.  The eco file reference must be
c                 read from the cultivar line; then the eco file is
c                 opened, and the line of headings and values from 
c                 the appropriate row in the file are appended to 
c                 LINE and LINE2.  The code following this should 
c                 then be able to parse these without further
c                 modification.

!                 08/04/09 CHP added separate pathname for ECO file
                  PATHL  = INDEX(PATHER,BLANK)
                  IF (PATHL .LE. 1) THEN
                    FILEEE = FILEE
                  ELSE
                    FILEEE = PATHER(1:(PATHL-1)) // FILEE
                  ENDIF
                  FILENAME = FILEEE

c                 Open ECO file:
                  OPEN(UNIT=ECOFILE, FILE=FILEEE, ACCESS='SEQUENTIAL', 
     -                 ACTION='READ') 

c                 Read ecotype code from cultivar file:
!chp              READ(LINE2, '(26X, A6)') ECOTYPE
                  READ(LINE2, '(23X, A6)') ECOTYPE
                  ! WRITE(*, '(A)') 'Ecotype is ', ECOTYPE


c                 Loop through file, until ECOTYPE is found:
                  DO WHILE(.TRUE.)
c                     Clear strings
                      DO I=1, 1024
                           ELINE(I:I) = ' '
                           ELINE2(I:I) = ' '
                      ENDDO
c                     Read a ELINE:
                      READ(ECOFILE, '(A)') ELINE
c                     Ignore comments:
                      IF ((ELINE(1:1)      .NE. '!') .AND.
     -                    (LEN_TRIM(ELINE) .GT.  0)) THEN
c                         If the first bit is '@ECO#', we have found the headings
                          IF (ELINE(1:5) .EQ. '@ECO#') THEN
!                             chp write to INFO.OUT
                              !WRITE(*, '(A)') ELINE
                              MSG(1) = ELINE(1:78)
                              CALL INFO(1,'SCCAN_ECO',MSG)
c                             We have discovered the heading line!
c                             Search for ecotype
                              DO WHILE(.TRUE.)
                                  READ(ECOFILE, '(A)',END=1000) ELINE2
!                                 chp write to INFO.OUT
                                  !WRITE(*, '(A)') ELINE2
                                  MSG(1) = ELINE2(1:78)
                                  CALL INFO(1,'SCCAN_ECO',MSG)
                                  IF (ELINE2(1:1) .NE. '!') THEN
c                                     If the line begins with the ecotype name, 
c                                     we have found the ecotype defn
                                      IF (TRIM(ELINE2(1:6)) .EQ.
     &                                    TRIM(ECOTYPE)) 
     &                                THEN
c                                         Exit from loop, because ELINE2 contains the
c                                         ecotype info
                                          EXIT
                                      ENDIF
                                  ENDIF
                              ENDDO
c                             Exit from loop, because ELINE contains headings
                              EXIT
                          ENDIF
                      ENDIF

                  ENDDO 


c                 Close ECO file:
                  CLOSE(ECOFILE)

c                 * Now, join the cul and eco column name strings and column value strings *
c                 * excluding the initial text (25 characters) *
                  LINE  = TRIM(LINE) // ' ' // ELINE(26:)
                  LINE2 = TRIM(LINE2) // ' ' // ELINE2(26:)
                  
!                  WRITE(*, '(A)') LINE
!                  WRITE(*, '(A)') LINE2


c                 :::::::::::::::::::::::::::::::::::::::::::::::::::

c                 Read the cultivar name from the string read from file:
                  READ(LINE2, '(7X, A16)') CULTIVAR

c                 Parsing of variable name strings:
c                 :::::::::::::::::::::::::::::::::
                  BEGI = 1
                  ENDI = 1
                  PREVE = 0
                  VARNAME = '                    '
c                 Skip first three tokens:  Skip 5 - chp (from INH file now)
                  CALL FINDNEXT(BEGI, ENDI, 2, VARNAME, LINE)    !C
                  CALL FINDNEXT(BEGI, ENDI, ENDI, VARNAME, LINE) !CR
                  CALL FINDNEXT(BEGI, ENDI, ENDI, VARNAME, LINE) !INGENO
                  CALL FINDNEXT(BEGI, ENDI, ENDI, VARNAME, LINE) !CNAME
                  CALL FINDNEXT(BEGI, ENDI, ENDI, VARNAME, LINE) !ECO#

c                 Assign End index of current token to end index of 
c                 'previous' token
                  PREVE = ENDI

c                 Init counter
                  I = 1

                  DO WHILE (.TRUE.)
                      VARNAME = '                    '
c                      WRITE(*, '(A, I6)') 'I is ', I
c                     Find a token:
                      LINELEN = LEN_TRIM(LINE)
                      CALL FINDNEXT(BEGI, ENDI, ENDI, VARNAME, LINE)
                      IF (BEGI .GT. LINELEN) EXIT
                      IF (ENDI .GT. LINELEN) EXIT
c                     WRITE(*, '(A10, 2(I10))') VARNAME, BEGI, ENDI
                      
                      WRITE(CF_NAMES(I), '(A20)') VARNAME
c                      CF_NAMES(I) = VARNAME
                      CF_OK(I) = .FALSE.

c                     Now, find corresponding value in LINE2:
c                     :::::::::::::::::::::::::::::::::::::::
                          CHTEMP = '                    '
c                         Copy, as a string, the value indicated by the
c                         string indices, into a temporary, known-length
c                         string
                          READ(LINE2(PREVE:ENDI), '(A)') CHTEMP
c                         Now, parse this as a real value into the array
c                         If the string is empty, the default value should be used, so
c                         ignore a string that is just spaces
                          IF (LEN_TRIM(CHTEMP) .GT. 0)  THEN
c                              WRITE(*, '(2A)') 'Reading from ', CHTEMP
                              READ(CHTEMP, '(F20.0)') CF_VALUES(I)
                              CF_OK(I) = .TRUE.
                          ENDIF

c                     Assign End index of current token to end index of 
c                     'previous' token
                      PREVE = ENDI
                      I = I+1
                  ENDDO
                  NParams = I-1
c                 END of parsing of variable name strings:
c                 ::::::::::::::::::::::::::::::::::::::::

c                 Now, print array neatly:
c                 ::::::::::::::::::::::::
                  DO I=1,NParams
!                     CHP - write to INFO.out
                      WRITE(MSG(I), '(2H| ,A20,2H| ,F20.10, 2H| )') 
     -                              CF_NAMES(I), CF_VALUES(I)
                  ENDDO
                  CALL INFO(NParams,'SCCAN_CULT',MSG)
                  CONTINUE
c                 ::::::::::::::
c             End of 'is the file opened?' condition statement
              ENDIF
c             ::::::::::::::::::::::::::::::::


c             SEARCH for a particular coefficient:
c             ::::::::::::::::::::::::::::::::::::
c             The program has just asked for a coefficient by name.  This is
c             the COEFF_NAME parameter to the subroutine.  The corresponding
c             variable will be looked up in the CF_NAMES array, and if a 
c             match is found, the values will be retrieved from the CF_VALUES
c             array and returned.  The parsing of values, etc above only 
c             happens for the first parameter that is requested.
c             ::::::::::::::::::::::::::::::::::::
c              WRITE(*, '(3A)') 'Searching for cult coeff.: ',CHAR(9), 
c     &                         COEFF_NAME
c             Assume the worst:
              EERROR = .TRUE.
              ERRMSG(1) = 'Could not find cultivar param: '
     &                    // COEFF_NAME
              DO I=1,120
c                 Compare strings
c                 WRITE(*, '(1H#,A,1H#)') CF_NAMES(I)
c                 WRITE(*, '(1H#,A,1H#)') COEFF_NAME
                  IF (TRIM(CF_NAMES(I)) .EQ. TRIM(COEFF_NAME)) THEN
c                     Assign value in case of positive match, and if the
c                     value was read ok from file.  Return an ERROR if not.
                      IF (CF_OK(I)) THEN
                          COEFF = CF_VALUES(I)
c                         Set error value to false
                          EERROR = .FALSE.
                      ELSE
                          ERRMSG(1) = 'Problem with cultivar param: '
     &                                // COEFF_NAME
                      ENDIF
                      EXIT
                  ENDIF
              ENDDO
              CONTINUE

c             If there was an error, write warning to WARNINGS.OUT
              IF (EERROR) THEN
                  CALL WARNING(1,'CULT COEFF', ERRMSG)
d                  WRITE(*, '(A)') ERRMSG(1)
              ENDIF



c     ========================================================================
c     FINAL: Finalisation of run/season?
c     ========================================================================
          ELSEIF (CONTROL%DYNAMIC .EQ. SEASEND) THEN

          OPENED = .FALSE.
          FIO_OPENED = .FALSE.

c         End of CONTROL%DYNAMIC condition
          ENDIF
c     ========================================================================
c     END of subroutine
c     ========================================================================
          WRITE(MSG(1),'(A15," = ",F15.5)') TRIM(COEFF_NAME), COEFF
          CALL INFO(1,"SC_COEFFS",MSG)
          RETURN

!     Error handling - EOF Ecotype file
 1000 CONTINUE
      CALL ERROR(ERRKEY,1,FILEEE,1)
      RETURN

      END SUBROUTINE GET_CULTIVAR_COEFF
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     ========================================================================





c     A subroutine to tokenise (sort of)
c     Finds the indices of the beginning and end of a header variable string
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE FINDNEXT(BEGINI, ENDI, FROM, VARNAME, LINE)
          IMPLICIT NONE
          SAVE
          CHARACTER*(*) LINE, VARNAME
          INTEGER BEGINI, ENDI, STRLEN, FROM

          STRLEN = LEN_TRIM(LINE)
          BEGINI = FROM


c         Find start of token
          DO WHILE(.TRUE.)
              IF (BEGINI .GT. STRLEN) EXIT
              IF (LINE(BEGINI:BEGINI) .EQ. ' ') THEN
                  BEGINI = BEGINI + 1
              ELSE
                  EXIT
              ENDIF
          ENDDO

c         Start of token is found (1st non-space char)
c         Start search for end from start of token + 1
          ENDI = BEGINI + 1

c         Find end of token
          DO WHILE(.TRUE.)
              IF (ENDI .GT. STRLEN) EXIT
              IF (LINE(ENDI:ENDI) .NE. ' ') THEN
                  ENDI = ENDI + 1
              ELSE
                  EXIT
              ENDIF
          ENDDO

c         Check bounds:
          IF (ENDI .GT. STRLEN) ENDI = STRLEN

c         Send back token:
          VARNAME = LINE(BEGINI:ENDI)

c         Erase from original string:
          LINE(BEGINI:ENDI) = ' '

          RETURN

      END


!CHP moved to READS.FOR as LOGICAL FUNCTION FIND_IN_FILE
!      SUBROUTINE FIND_IN_FILE(NEEDLE, HAYSTACK)
!          IMPLICIT NONE
!c         File unit number of file in which to search
!          INTEGER HAYSTACK
!c         Text to search for:
!          CHARACTER*(*) NEEDLE
!c         length of chars to search for: 
!          INTEGER STRLEN
!c         Line to read from file:
!          CHARACTER*1024 LINE
!
!c         ====================================
!
!          STRLEN = LEN_TRIM(NEEDLE)
!
!c                 Search file for line starting with needle
!                  DO WHILE (.TRUE.)
!c                     Read a line
!                      READ(HAYSTACK, '(A)', END=50) LINE
!c                     Check if the first chars match:
!                      IF (LINE(1:STRLEN) .EQ. TRIM(NEEDLE)) THEN          
!                          RETURN
!                      ENDIF
!                  ENDDO
!
!                  RETURN
!
!   50             CONTINUE
!d                  WRITE(*,*) 'Could not read: FIND_IN_FILE', NEEDLE
!c                  PAUSE
!
!          RETURN
!c         ====================================          
!      END





c     A subroutine to read the species file, save the values in an
c     internal array, and return them when requested.
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GET_SPECIES_COEFF(
c                        [IO]: the coefficient
     -                   COEFF,
c                        [I]: name of coefficient
     -                   COEFF_NAME,
c                        [I]: DSSAT control variable
     -                   CONTROL,
c                        [O]: was there an error?
     -                   EERROR)

c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     ========================================================================
c     HEADER: Variables and declarations
c     ========================================================================
c     Use DSSAT modules:
          USE ModuleDefs
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     Declare, and save, all variables:
          IMPLICIT NONE
          EXTERNAL ERROR, GETLUN, FIND, WARNING
          SAVE
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     Declare variables:
c     ::::::::::::::::::
c     Subroutine parameters:
c     ::::::::::::::::::::::
c         The coefficient retrieved is stored in this variable
          REAL COEFF

c         The name of the coefficient to be searched for
c           Note: DSSAT cultivar coefficients are generally limited to
c           4 or 5 characters.  30 are provided by way of futureproofing.
          CHARACTER*(*) COEFF_NAME

c         The DSSAT Control variable (tells the module when to init, etc)
          Type(ControlType) CONTROL

c         An error variable: if the calling module determines that was
c         an error, it can decide what to do (use a default value, 
c         terminate excecution, etc)
          LOGICAL EERROR
c         ::::::::::::::::::::::::::
c         Specify intention:
c         ::::::::::::::::::
          INTENT(IN)  CONTROL, COEFF_NAME
          INTENT(OUT) EERROR
          INTENT(INOUT) COEFF 
c     :::::::::::::::::::::::::
c     End of subroutine params
c     :::::::::::::::::::::::::

c     Local variables:
c     ::::::::::::::::

c         Array of coefficient names:
          CHARACTER*20 CF_NAMES(50), CF_NAME_XX, TMP_STR

c         Array of coefficient values:
          REAL CF_VALUES(50)

c         Array of logical values indicating whether or not a value
c         was actually read from file:
          LOGICAL CF_OK(50)

c         The name of the species file
          CHARACTER*256 FILENAME

c         The cultivar file IO unit number:
          INTEGER CFILE

c         The Filex number:
!          INTEGER FILEX

c         A line of the species file (to be read) from file, then
c         parsed:
          CHARACTER*1024 LINE !, LINE2

c         Has the file been opened?
          LOGICAL OPENED

c         Temp species number read from file:
c          INTEGER CULTNUM

c         Temp cultivar name:
c          CHARACTER*6 CULTNAME

c         Temp values:
          INTEGER I, EQ_IND, K, X
          LOGICAL INVALID
          REAL REAL_VAL
!          INTEGER BEGI, ENDI, PREVE, LINELEN, 

c         Temp. variable name:
!          CHARACTER*20 VARNAME, CHTEMP
C-KRT *****************************************************************
C-KRT Modified declaration to make compatable with g95.
C-KRT     CHARACTER BLANK*1,ERRKEY*6,FILEC*12,FILECC*92,FILEIO*30,
C-KRT&    PATHCR*80,SECTION*6 , ERRMSG*78(1)
          CHARACTER BLANK*1,ERRKEY*6,FILEC*12,FILECC*92,FILEIO*30
          CHARACTER PATHCR*80,SECTION*6
          CHARACTER*78 ERRMSG(1)
C-KRT *****************************************************************
          INTEGER LUNIO, ERRNUM, LNUM, FOUND, PATHL

          PARAMETER (BLANK=' ')

c     ::::::::::::::::
c     ========================================================================
c     CODE
c     ========================================================================
c         Set these values to 0 (or equiv) for each subroutine call
c          COEFF = 0.
          LINE = ''
c     ========================================================================
c     RUNINIT: Initialisation for each run
c     ========================================================================
          IF (CONTROL%DYNAMIC .EQ. RUNINIT) THEN

c             The file must not be opened, so that the first call to
c             initialise a variable will open the input file and
c             initialise the coefficient arrays
              OPENED = .FALSE.
c             As yet, none of the parameters have been read correctly:
              CF_OK = .FALSE.

c     ========================================================================
c     SEASINIT: Initialisation for each season
c     Note: all coefficients will be requested in the init section
c     anyway, so care must be taken not to reopen the species file each time.
c     ========================================================================
          ELSEIF (CONTROL%DYNAMIC .EQ. SEASINIT) THEN



c             GET the cultivar code from the FILEX.  Note, this might be easier
c             to get from the DSSAT45.INP file instead.
c             :::::::::::::::::::::::::::::::::::::::::
c             If the file is not already open:
c             ::::::::::::::::::::::::::::::::
              IF (.NOT.(OPENED)) THEN
                  OPENED = .TRUE.
c                 :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c                 Open the species file, and read values into array
c                 :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c                 Get fileIO unit number:
c                 Note: no specific file number is allocated to the species file.
c                 DSSAT will select an unused one (hopefully) and return that.
                  CALL GETLUN('SPEC', CFILE)

c                 Code copied from ETPHOT.FOR:
c                 ::::::::::::::::::::::::::::
c                 Get the species filename from the INP file:
c                 :::::::::::::::::::::::::::::::::::::::
                  CALL GETLUN('FILEIO', LUNIO)
                  OPEN(LUNIO,FILE=Control%FILEIO,STATUS='OLD',
     &                 IOSTAT=ERRNUM)
                  IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)

                  SECTION = '*FILES'
                  CALL FIND(LUNIO,SECTION,LNUM,FOUND)
                  IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEIO,LNUM)

                  READ(LUNIO,'(////,15X,A,1X,A)',IOSTAT=ERRNUM) FILEC,
     &                                                          PATHCR
                  LNUM = LNUM + 5
                  IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,
     &                                          LNUM)

                  PATHL  = INDEX(PATHCR,BLANK)
                  IF (PATHL .LE. 1) THEN
                    FILECC = FILEC
                  ELSE
                    FILECC = PATHCR(1:(PATHL-1)) // FILEC
                  ENDIF

D                 WRITE(*, '(2A)') 'Species file path is ', FILECC

c                 Close the file
                  CLOSE(LUNIO)
c                 :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c                 End of species file path acquisition

c                 Open the file:
c                 ::::::::::::::
c *               Set filename (comes from INP file any day now):
c                  WRITE(*, '(2A)') 'Species file: ', FILECC
                  FILENAME = FILECC

c                 Open:
                  OPEN(UNIT=CFILE, FILE=FILENAME, ACCESS='SEQUENTIAL', 
     -                 ACTION='READ')

c                 Read the file, ignoring blank lines, or lines starting with
c                 a '*' or '!':
                  X = 0
                  DO WHILE(.TRUE.)
                      READ(CFILE, '(A)', END=99) LINE
                      LINE = TRIM(LINE)
c                     If the line is not empty...
                      IF (LEN_TRIM(LINE) .GT. 0) THEN
c                         If the line does not start with a '!' or '*':
                          IF (.NOT.(((LINE(1:1)) .EQ. '!' 
     -                        .OR. (LINE(1:1)) .EQ. '*'))) 
     -                    THEN
c                             Echo the line read:
c                              WRITE(*,'(A1,A40,A1)') '"',LINE, '"'
c                             Get part before '=' sign; I could depend on
c                             fixed widths but would rather make it more 
c                             robust, hence:
c                             Search for '='
                              EQ_IND = INDEX(LINE, '=')
c                             If the length of the string before the '=' sign
c                             is greater than 0, attempt to parse value:
                              IF (EQ_IND-1 .GT. 0) THEN
c                               Detect invalid characters:
                                DO K=EQ_IND+1, LEN(LINE)
                                  IF (INDEX('1234567890. -+',LINE(K:K))
     &                             .LT. 1)
     &                            THEN
D                                   WRITE(*,*) 'Invalid! ', K, LINE(K:K)
                                    INVALID = .TRUE.
                                    EXIT
                                  ENDIF
                                ENDDO
                                IF (.NOT.(INVALID)) THEN
c                                 Increment counter
                                  X = X+1
c                                 Convert to REAL:
                                  READ(LINE(EQ_IND+1:LEN(LINE)), 
     &                                    '(F10.0)') REAL_VAL
c                                 Echo value:
c                                  WRITE(*, '(A, F10.5)') LINE(1:EQ_IND),
c     &                                                   REAL_VAL 
c                                 Assign to array(s):
c                                  TMP_STR = TRIM(LINE(1:EQ_IND))
                                  TMP_STR = LINE(1:EQ_IND-1)
D                                 WRITE(*, '(A, I3)') TMP_STR, EQ_IND
                                  WRITE(CF_NAMES(X), '(A20)') 
     &                                                TRIM(TMP_STR)
                                  CF_VALUES(X) = REAL_VAL
                                  CF_OK(X)     = .TRUE.
                                ENDIF
                              ENDIF
                          ENDIF
c          ********************
                      ENDIF
                      
                  ENDDO
   99             CLOSE(CFILE)

c             PAUSE
c             Print out arrays:
!              DO I=1, 50
!                  IF(CF_OK(I)) WRITE(*, '(A20,A,F10.5)') 
!     &               CF_NAMES(I), ' = ', CF_VALUES(I)
!              ENDDO

c                 ::::::::::::::
c             End of 'is the file opened?' condition statement
              ENDIF
c             ::::::::::::::::::::::::::::::::



c             SEARCH for a particular coefficient:
c             ::::::::::::::::::::::::::::::::::::
c             The program has just asked for a coefficient by name.  This is
c             the COEFF_NAME parameter to the subroutine.  The corresponding
c             variable will be looked up in the CF_NAMES array, and if a 
c             match is found, the values will be retrieved from the CF_VALUES
c             array and returned.  The parsing of values, etc above only 
c             happens for the first parameter that is requested.
c             ::::::::::::::::::::::::::::::::::::
c             Assume the worst:
              EERROR = .TRUE.

              ERRMSG(1) = 'Could not find species coeff: ' 
     &                    // COEFF_NAME


c             Write COEFF_NAME to a 20-element string:
              WRITE(CF_NAME_XX, '(A20)') COEFF_NAME
              

              DO I=1,50
c                 Compare strings
c                 WRITE(*, '(1H#,A,1H#)') 'Array:'//CF_NAMES(I)
c                 WRITE(*, '(1H#,A,1H#)') 'Coeff:'//CF_NAME_XX
                  IF (TRIM(CF_NAMES(I)) .EQ. TRIM(CF_NAME_XX)) THEN
c                     Assign value in case of positive match, and if the
c                     value was read ok from file.  Return an ERROR if not.
                      IF (CF_OK(I)) THEN
                          COEFF = CF_VALUES(I)
c                         Set error value to false
                          EERROR = .FALSE.
                      ELSE
                          ERRMSG(1) = 'Problem reading species coeff: '
     &                                // CF_NAME_XX         
                      ENDIF
                      EXIT
                  ENDIF
              ENDDO
c              WRITE(*,*) '---'
              CONTINUE

c             Write an error statement if the coeff could not be found:
              IF (EERROR) THEN
!d                WRITE(*,'(3A)') 'ERROR: could not read species ',
!d     &                     'coefficient - ', CF_NAME_XX
c             Write a warning:
                CALL WARNING(1, 'SPECIES FILE READ', ERRMSG)
              ENDIF



c     ========================================================================
c     FINAL: Finalisation of run/season?
c     ========================================================================
          ELSEIF (CONTROL%DYNAMIC .EQ. SEASEND) THEN

          OPENED = .FALSE.
      
c         End of CONTROL%DYNAMIC condition
          ENDIF
c     ========================================================================
c     END of subroutine
c     ========================================================================
          RETURN
      END SUBROUTINE GET_SPECIES_COEFF
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     ========================================================================
