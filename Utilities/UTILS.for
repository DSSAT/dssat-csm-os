!=======================================================================
C  UTILS, File, G. Hoogenboom, P.W. Wilkens, C.H. Porter
C  General utility functions
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/30/1998 GH  Combine UTILS based on UPCASE, VERIFY, TABEX, and CLEAR
C  07/01/2000 GH  Added SWAP
C=======================================================================

C=======================================================================
C  UPCASE, Function
C
C  Function to return the upper case of a lower case letter.  Otherwise
C  returns the same character
C-----------------------------------------------------------------------
C  Revision history
C
C  05/15/1992 BDB Written
C  05/28/1993 PWW Header revision and minor changes   
C-----------------------------------------------------------------------
C  INPUT  : INCHAR
C
C  LOCAL  :
C
C  OUTPUT : INCHAR
C-----------------------------------------------------------------------
C  Called : INPUT READS IPEXP JULIAN
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  INCHAR :
C  CHAVAL :
C=======================================================================

      CHARACTER*1 FUNCTION UPCASE (INCHAR)

      IMPLICIT  NONE

      CHARACTER INCHAR*1
      INTEGER   CHAVAL

      CHAVAL = ICHAR(INCHAR)

      IF ((CHAVAL .LE. 122) .AND. (CHAVAL .GE. 97)) THEN
         UPCASE = CHAR(CHAVAL-32)
       ELSE
         UPCASE = INCHAR
      ENDIF

      END FUNCTION UPCASE

C=======================================================================
C  VERIFY, Subroutine
C
C  I/O utility routine
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      5-28-93
C-----------------------------------------------------------------------
C  INPUT  : LINE VALUE FLAG
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : IPECO IPSOIL IPVAR SECLI  SECROP SEFERT SEFREQ SEHARV SEINIT
C           SEIRR SEPLT  SERES SESOIL SEVAR  SEWTH  IPEXP  SETIME
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C=======================================================================

      SUBROUTINE VERIFY (LINE,VALUE,FLAG)

      IMPLICIT    NONE

      CHARACTER*1 DIGITS(13),LINE(80)
      INTEGER     JKNEW,JSIGNR,JTIME,JVALUE,NC,NDECML,I,J,JSIGN
      REAL        TFRAC,VALUE,FLAG

      DATA DIGITS/'0','1','2','3','4','5','6','7','8','9',
     &            '+','-','.'/

      FLAG   = -1.0
      JKNEW  =    0
      JSIGNR =    1
      JTIME  =    0
      JVALUE =    0
      NC     =    0
      NDECML =   -1
      TFRAC  =  0.0
      VALUE  =  0.0
C
C     Check for CR/LF
C
      DO I = 1, 80
         IF (LINE(I) .NE. ' ') GO TO 100
      END DO
      !
      ! Nothing entered .. set FLAG to 1.0 and exit
      !
      FLAG = 1.0
      GO TO 1300

  100 NC     = NC + 1
      IF (NC .GT. 80) GO TO 1300
      IF (LINE(NC) .EQ. ' ') GO TO 100
      DO I = 1, 13
         J = I
         IF (LINE(NC) .EQ. DIGITS(I)) GO TO 300
      END DO
      GO TO 1200
  300 IF (J .LE. 10) GO TO 600
      IF (J .EQ. 13) GO TO 500
      JSIGN  = 1
      IF (J .EQ. 12) JSIGN = -1

C-----------------------------------------------------------------------
C***    IF SIGN IS REPEATED
C-----------------------------------------------------------------------

      IF (JKNEW .GT. 0) GO TO 1200
      JKNEW = 1
C-----------------------------------------------------------------------
C
C***    SIGN APPEARS AFTER THE DECIMAL POINT
C
C-----------------------------------------------------------------------
      IF (NDECML) 400,1200,900
  400 JSIGNR = JSIGN
      GO TO 900
  500 JKNEW  = 1
C-----------------------------------------------------------------------
C***    DECIMAL REPEATED
C-----------------------------------------------------------------------

      IF (NDECML .GE. 0) GO TO 1200
      NDECML = 0
      GO TO 900
  600 J = J - 1
      JKNEW = 1
      IF (NDECML) 700,800,900
  700 JVALUE = JVALUE*10 + J
      GO TO 900
  800 JTIME = JTIME + 1
      TFRAC = TFRAC + FLOAT(J)/(10.**JTIME)
  900 VALUE = FLOAT(JSIGNR*JVALUE)+FLOAT(JSIGNR)*TFRAC
 1000 NC    = NC + 1
      IF (NC .GT. 80) GO TO 1300
      IF (LINE(NC) .EQ. ' ') GO TO 1000
      DO I = 1, 13
         J = I
         IF (LINE(NC) .EQ. DIGITS(I)) GO TO 300
      END DO
 1200 FLAG = 1.0

 1300 DO I = 1, 80
         LINE(I) = ' '
      END DO

      END SUBROUTINE VERIFY

C=======================================================================
C  TABEX, Function
C
C  Look up utility routine
C-----------------------------------------------------------------------
      FUNCTION TABEX(VAL,ARG,DUMMY,K)

      IMPLICIT NONE

      INTEGER K,J

      REAL VAL(K),ARG(K),DUMMY,TABEX

           DO 100  J = 2,K
           IF (DUMMY .GT. ARG(J)) GO TO 100
           GO TO 200
  100      CONTINUE
      J = K
  200 TABEX = (DUMMY-ARG(J-1))*(VAL(J)-VAL(J-1))/(ARG(J)-ARG(J-1))+VAL
     &     (J-1)

      END FUNCTION TABEX

C=======================================================================
C  CLEAR, Subroutine
C
C  Clears the screen using ANSI codes, sets color to white on blue
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2. Header revision and minor changes           P.W.W.      5-28-93
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  : ESC
C
C  OUTPUT : None
C-----------------------------------------------------------------------
C  Called : SESOIL SESIM SERES SEPLT SENSDM SENS SEFREQ SEWTH SEFERT SECLI
C           SEVAR SETIME IPVAR IPSOIL WTHMDI OPHARV OPDAY IPEXP SEIRR SEINIT
C           SEHARV SECROP INVAR INPUT
C
C  Calls  : None
C-----------------------------------------------------------------------

C                         DEFINITIONS
C
C  ESC    : ESC key codes
C=======================================================================

      SUBROUTINE CLEAR

      IMPLICIT  NONE

      !CHARACTER ESC
      !ESC = CHAR(27)
C      WRITE  (*,100) ESC
      WRITE  (*,100) 
C 100   FORMAT (1X,A1,'[2J',//////)
100   FORMAT (20/)

      END SUBROUTINE CLEAR

C=======================================================================
C  HOME, Subroutine
C
C  Moves cursor to 0,0 ANSI and clears the screen
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                     P.W.W.      2- 9-93
C  2. Header revision and minor changes           P.W.W.      5-28-93
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  : ESC
C
C  OUTPUT : None
C-----------------------------------------------------------------------
C  Called : ERROR AUTPLT AUTHAR INPUT
C
C  Calls  : CLEAR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  ESC    : ESC key codes
C=======================================================================

      SUBROUTINE HOME

      IMPLICIT  NONE

      CHARACTER ESC

      ESC = CHAR(27)

      WRITE (*,100) ESC
      WRITE (*,200) ESC

100   FORMAT (1X,A1,'[2J')
200   FORMAT (1X,A1,'[0;0H')

      END SUBROUTINE HOME

C=======================================================================
C  CURPOS, Subroutine
C
C  Moves cursor to specified row
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                     P.W.W.      2- 9-93
C  2. Header revision and minor changes           P.W.W.      5-28-93
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  : ESC
C
C  OUTPUT : None
C-----------------------------------------------------------------------
C  Called : INTRO
C
C  Calls  : NONE
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  ESC    : ESC key codes
C=======================================================================

      SUBROUTINE CURPOS (LINE)

      IMPLICIT  NONE

      CHARACTER ESC,LINE*2

      ESC = CHAR(27)

      WRITE (*,200) ESC,LINE

200   FORMAT (1X,A1,'[',A2,';0H')

      END SUBROUTINE CURPOS

C=======================================================================
C  CURV, Function
C
C  Function to interpolate between four data points.
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1990 JWJ Written
C  01/12/1999 GH  Added to UTILS routine
C  10/08/2004 CHP Changed criteria for XM from .GT. to .GE. 
C-----------------------------------------------------------------------
C  INPUT  :
C
C  LOCAL  :
C
C  OUTPUT : INCHAR
C-----------------------------------------------------------------------
C  Called :
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  CTYPE  :
C  CURV   :
C  XB     :
C  XM     :
C=======================================================================
      FUNCTION CURV(CTYPE,XB,X1,X2,XM,X)

      IMPLICIT NONE

      CHARACTER*3 CTYPE
      REAL CURV,XB,X1,X2,XM,X

      CURV = 1.0
      IF (CTYPE .EQ. 'NON' .OR. CTYPE .EQ. 'non') RETURN

C-------------------------------------------------------------------------------
      IF(CTYPE .EQ. 'LIN' .OR. CTYPE .EQ. 'lin') THEN
        CURV = 0.
        IF(X .GT. XB .AND. X .LT. X1)CURV = (X-XB)/(X1-XB)
        IF(X .GE. X1 .AND. X .LE. X2)CURV = 1.
        IF(X .GT. X2 .AND. X .LT. XM)CURV = 1.0 - (X-X2)/(XM-X2)
        CURV = MAX(CURV,0.0)
        CURV = MIN(CURV,1.0)
      ENDIF
C-------------------------------------------------------------------------------

C-------------------------------------------------------------------------------
      IF(CTYPE .EQ. 'QDR' .OR. CTYPE .EQ. 'qdr') THEN
        CURV = 0.
        IF(X .GT. XB .AND. X .LT. X1)CURV = 1. -((X1-X)/(X1-XB))**2
        IF(X .GE. X1 .AND. X .LE. X2)CURV = 1.
        IF(X .GT. X2 .AND. X .LT. XM)CURV = 1. - ((X-X2)/(XM-X2))**2
        CURV = MAX(CURV,0.0)
        CURV = MIN(CURV,1.0)
      ENDIF
C-------------------------------------------------------------------------------

C-------------------------------------------------------------------------------
C     Curve type INL is the inverse linear with a minimum for use in photoperiod
C     In this case, XM is the lowest relative rate, X1 and X2 are critical dayl
C-------------------------------------------------------------------------------
      IF(CTYPE .EQ. 'INL' .OR. CTYPE .EQ. 'inl') THEN
        CURV = 1.0
        IF(X .GT. X1 .AND. X .LT. X2)CURV = 1.-(1.-XM)*((X-X1)/(X2-X1))
!        IF(X .GT. X2) CURV = XM
        IF(X .GE. X2) CURV = XM       !CHP per Stu Rymph 10/8/2004
        CURV = MAX(CURV,XM)
        CURV = MIN(CURV,1.0)
      ENDIF
C-------------------------------------------------------------------------------
C     Curve type SHO for use with short day plants.
C     The curve is the inverse linear with a minimum for use in photoperiod
C     In this case, XM is the lowest relative rate, X1 and X2 are critical dayl
C-------------------------------------------------------------------------------
      IF(CTYPE .EQ. 'SHO' .OR. CTYPE .EQ. 'sho') THEN
        IF (X .LE. X1) THEN
           CURV = 1.0
        ELSE IF ((X .GT. X1) .AND. (X .LT. X2)) THEN
           CURV = 1.-(1.-XM)*((X-X1)/(X2-X1))
        ELSE IF (X .GE. X2) THEN
          CURV = XM
        ENDIF
        CURV = MAX(CURV,XM)
        CURV = MIN(CURV,1.0)
      ENDIF
C-------------------------------------------------------------------------------
C     Curve type LON for use with long day plants.
C     The curve is the inverse linear with a minimum for use in photoperiod
C     In this case, XM is the lowest relative rate, X1 and X2 are critical dayl
C-------------------------------------------------------------------------------
      IF(CTYPE .EQ. 'LON' .OR. CTYPE .EQ. 'lon') THEN
        IF (X .LT. X2) THEN
           CURV = XM
        ELSE IF ((X .GE. X2) .AND. (X .LT. X1)) THEN
           CURV = 1.-(1.-XM)*((X1-X)/(X1-X2))
        ELSE
           CURV = 1.0
        ENDIF
        CURV = MAX(CURV,XM)
        CURV = MIN(CURV,1.0)
      ENDIF
C-------------------------------------------------------------------------------
C
C-------------------------------------------------------------------------------
      IF(CTYPE .EQ. 'SIN' .OR. CTYPE .EQ. 'sin') THEN
        CURV = 0.
        IF(X .GT. XB .AND. X .LT. X1)
     &   CURV = 0.5*(1.+COS(2.*22./7.*(X-X1)/(2.*(X1-XB))))
        IF(X .GE. X1 .AND. X .LE. X2)CURV = 1.
        IF(X .GT. X2 .AND. X .LT. XM)
     &   CURV = 0.5*(1.+COS(2.*22./7.*(X2-X)/(2.*(XM-X2))))
        CURV = MAX(CURV,0.0)
        CURV = MIN(CURV,1.0)
      ENDIF
C-------------------------------------------------------------------------------
C-------------------------------------------------------------------------------
C-------------------------------------------------------------------------------
C     Curve type REV - Reversible process - used for cold hardening
C	Rate of cold hardening increases as TMIN decreases from X1 to XB
C	Cold hardening reverses at an increasing rate as TMIN increases from X1 to X2
C     Process at maximum rate at or below XB
C	Rate decreases linearly to 0 at X1
C	Process reverses at a linear rate from X1 to X2
C	XM is the maximum absolute rate
C-------------------------------------------------------------------------------
      IF(CTYPE .EQ. 'REV' .OR. CTYPE .EQ. 'rev') THEN
        CURV = 1.
        IF(X .GT. XB .AND. X .LT. X1)CURV = 1.0-((X-XB)/(X1-XB))
        IF(X .GE. X1 .AND. X .LE. X2)CURV = 0.0-((X-X1)/(X2-X1))
        IF(X .GT. X2 )CURV = -1.0 
        CURV = MAX(CURV,-1.0)
        CURV = MIN(CURV,1.0)
	  CURV = CURV * XM
      ENDIF
C-------------------------------------------------------------------------------
C     Curve type DHD - used for cold dehardening in spring
C	No cold dehardening below XB (rate=0)
C	Rate of cold dehardening increases as TMIN increases from XB to X1
C     Process at maximum rate at or above X1
C	X2 is not used
C	XM is the maximum absolute rate
C-------------------------------------------------------------------------------
      IF(CTYPE .EQ. 'DHD' .OR. CTYPE .EQ. 'dhd') THEN
        CURV = 0.
        IF(X .GT. XB .AND. X .LT. X1)CURV = (X-XB)/(X1-XB)
        IF(X .GE. X1 .AND. X .LE. X2)CURV = 1
        IF(X .GT. X2 )CURV = 1
        CURV = MAX(CURV,0.0)
        CURV = MIN(CURV,1.0)
	  CURV = CURV * XM
      ENDIF

C-------------------------------------------------------------------------------
C     Curve type DRD - used for reducing rates of processes as dormancy advances
C	Multiply rates by this factor to reduce them on short days, 
C	no effect on long days
C	XM is the maximum reduction factor at full dormancy (daylength=XB)
C	Less reduction as daylength gets longer
C     Process at maximum rate at or above X1
C	X2 is not used
C-------------------------------------------------------------------------------
      IF(CTYPE .EQ. 'DRD' .OR. CTYPE .EQ. 'drd') THEN
        CURV = X2
        IF(X .GT. XB .AND. X .LT. X1)
     &	  CURV = X2+(XM-X2)*(X-XB)/(X1-XB)
        IF(X .GE. X1 )CURV = XM
        CURV = MAX(CURV,X2)
        CURV = MIN(CURV,XM)
      ENDIF

C-------------------------------------------------------------------------------
C    Curve type CDD - used for reducing rates of processes as dormancy advances
C	Multiply rates by this factor to reduce them on short days, 
C	Long day effect depends on value of XM
C	X2 is the maximum reduction factor at full dormancy (daylength=XB)
C	Less reduction as daylength gets longer
C    Process at maximum rate at or above X1
C	Curvilinear version of DRD
C-------------------------------------------------------------------------------

      IF(CTYPE .EQ. 'CDD' .OR. CTYPE .EQ. 'cdd') THEN
        CURV = X2
        IF(X .GT. XB .AND. X .LT. X1)
     &	  CURV = XM-((XM-X2)*((X1-X)/(X1-XB))**2)
        IF(X .GE. X1)CURV = XM
        CURV = MAX(CURV,X2)
        CURV = MIN(CURV,XM)
      ENDIF

C-------------------------------------------------------------------------------
C	Curve type EXK - generic exponential function with "k"
C	XB sets the amplitude of the curve (max Y value)
C	X1/XM sets the amount of curvature (k) and shape of the curve (+ or -)
C	X2 shifts the curve left (- X2) or right (+X2) on the X axis 
C	If X1/XM is positive, X2 is the X-intercept 
C-------------------------------------------------------------------------------

      IF(CTYPE .EQ. 'EXK' .OR. CTYPE .EQ. 'exk') THEN

        CURV = XB - EXP(X1*(X-X2)/XM)
      ENDIF
C-------------------------------------------------------------------------------
C-------------------------------------------------------------------------------
C     Curve type VOP - Variable Order Polynomial
C	Polynomial order (quadratic, cubic, etc.) is continuously variable 
C	(not discete steps)
C	XB=T0, the lower temperature where the function scales to 0
C	XM=T0', the upper temperature where the function scales to 0
C	X1=Tref, reference temperature at which the functio scales to 1.0
C	X2=qft, variable that sets the order of the polynomial
C	Set X2=1 the function is quadratic, X2=2 cubic, X2=3 quartic, etc. 
C     X2 does not have to be an integer
C	Function scales to 0 below XB and above XM
C	Minimum CURV value =0.0, maximum can exceed 1.0
C	Can use mft, a multiplier, to set the scale of the function
C	Read mft in from file and apply in main section of code (ex. mft*CURV)
C-------------------------------------------------------------------------------
      IF(CTYPE .EQ. 'VOP' .OR. CTYPE .EQ. 'vop') THEN
        CURV=0.0
	  IF(X .GT. XB .AND. X .LT. XM)
     &	  CURV = (((X-XB)**X2)*(XM-X))/(((X1-XB)**X2)*(XM-X1))
        IF(X .GE. XM ) CURV = 0.0
        CURV = MAX(CURV,0.0)
      ENDIF

C-------------------------------------------------------------------------------
C-------------------------------------------------------------------------------
C     Curve type Q10 - basic Q10 function
C	XB=Tref, reference temperature
C	X1=k, te response at Tref
C	X2= Q10 increase in the response for every 10°K increase in temperature
C	XM is not used
C-------------------------------------------------------------------------------
      IF(CTYPE .EQ. 'Q10' .OR. CTYPE .EQ. 'q10') THEN
	  CURV=X1*(X2**((X-XB)/10))
      ENDIF

C-------------------------------------------------------------------------------
C-------------------------------------------------------------------------------
C     Curve type PWR - basic function raising X to some power with scaling
C	XB=multiplier for main function
C	X1=power to raise X to
C	X2= scaling multiplier to scale reults to a given range
C	XM is not used
C	Added condition for negative values of X - was generating NaN with
C	negative values of X and fractional vlaues of X1 
C	(ex. Temp=-1.8C and X1=1.5905).  Now uses value for 0.0 when X<0.0
C-------------------------------------------------------------------------------
      IF(CTYPE .EQ. 'PWR' .OR. CTYPE .EQ. 'pwr') THEN
		IF (X .LT. 0.0) THEN
		CURV=X2*XB*(0**X1)
		ELSE
		CURV=X2*XB*(X**X1)
		ENDIF
      ENDIF

C-------------------------------------------------------------------------------

      END FUNCTION CURV

C=======================================================================
C  SWAP
C
C  Subroutine to swap values among different variables
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/90 JWH Written
C  07/01/00 GH  Added to UTILS routine
C-----------------------------------------------------------------------

      SUBROUTINE SWAP(A, B, C, D, E, F)

      IMPLICIT NONE

      INTEGER A, B, C, D, E, F, I

      I = A
      A = B
      B = I
      I = C
      C = D
      D = I
      I = E
      E = F
      F = I
      RETURN
      END SUBROUTINE SWAP
C-----------------------------------------------------------------------


C=======================================================================
C  GETLUN, Subroutine, C. H. Porter
C-----------------------------------------------------------------------
C  Assigns unique output file unit numbers to input and output files
C     based on file variable name.  If valid file variable name is not
C     specified, unit numbers are assigned incrementally starting with
C     unit 500.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  10/17/2001 CHP Written.
C-----------------------------------------------------------------------
! Called by: IRRIG, OPWBAL, OPGROW, . . . 
! Calls: None
C========================================================================

      SUBROUTINE GETLUN(FileVarName, LUN)

!-----------------------------------------------------------------------
      USE ModuleDefs
      USE ModuleData
      IMPLICIT NONE
      EXTERNAL LenString
      SAVE

      CHARACTER*(*), INTENT(IN) :: FileVarName
      INTEGER, INTENT(OUT) :: LUN

      LOGICAL FEXIST, FIRST, FPRINT(2*MaxFiles)
      INTEGER Counter, Len2, Length, I, OUTLUN, StartLun
      INTEGER Len1, LenString
      CHARACTER*10 ALIAS 
      CHARACTER*16 SaveName(2*MaxFiles), FileName

      TYPE (OutputType) FileData

      DATA OUTLUN /30/    !LUN.LST - list of unit assignments
      DATA FIRST /.TRUE./

      INTEGER       DATE_TIME(8)
!      date_time(1)  The 4-digit year  
!      date_time(2)  The month of the year  
!      date_time(3)  The day of the month  
!      date_time(4)  The time difference with respect to Coordinated Universal Time (UTC) in minutes  
!      date_time(5)  The hour of the day (range 0 to 23) - local time  
!      date_time(6)  The minutes of the hour (range 0 to 59) - local time  
!      date_time(7)  The seconds of the minute (range 0 to 59) - local time  
!      date_time(8)  The milliseconds of the second (range 0 to 999) - local time  

!      CHARACTER*3   MON(12)
!      DATA MonthTxt /'Jan','Feb','Mar','Apr','May','Jun','Jul'
!     &         ,'Aug','Sep','Oct','Nov','Dec'/

!-----------------------------------------------------------------------
!     Get list of output files
      IF (FIRST) THEN
        CALL GET(FileData)
        FPRINT = .FALSE.
        StartLun = MaxFiles + 1
        Counter = 0
        FIRST = .FALSE.

!       On first call to subroutine, open new file to record
!       input and output file information.
        INQUIRE (FILE = 'LUN.LST', EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = OUTLUN, FILE = 'LUN.LST', STATUS = 'REPLACE',
     &      POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = OUTLUN, FILE = 'LUN.LST', STATUS = 'NEW')
          WRITE(OUTLUN,10)
   10     FORMAT('*Summary of files opened during simulation',
     &        //,'Unit  File',/'Num.  Variable Name')
        ENDIF

        CALL DATE_AND_TIME (VALUES=DATE_TIME)
      
!       Version information stored in ModuleDefs.for
        WRITE (OUTLUN,100) Version, VBranch, MonthTxt(DATE_TIME(2)),
     &    DATE_TIME(3), DATE_TIME(1), DATE_TIME(5), 
     &    DATE_TIME(6), DATE_TIME(7)
  100   FORMAT ("*DSSAT Cropping System Model Ver. ",I1,".",I1,".",I1,
     &    ".",I3.3,1X,A10,4X,
     &    A3," ",I2.2,", ",I4,"; ",I2.2,":",I2.2,":",I2.2,
     &        //," Unit  Filename")
      ENDIF

!-----------------------------------------------------------------------
      LUN = 0
      Length = LenString(FileVarName)

!     Assign input file names:
      SELECT CASE (FileVarName(1:Length))

!     Input Files (Units 8 through 29):
      CASE ('FILEA');   LUN = 8   !observed time series data
      CASE ('FILEC', 'FILEE', 'FINPUT');  LUN = 10          
                  !*.spe, *.eco, miscellaneous input files

      CASE ('FILEW');   LUN = 11  !*.wth - weather files
      CASE ('FILEP');   LUN = 12  !*.pst - pest files
      CASE ('FILESS');  LUN = 13  !RESCH???.SDA (was SOILN980.SOL)
      CASE ('BATCH');   LUN = 14  !Batch run input file
      CASE ('ERRORX');  LUN = 15  !Model.err
      CASE ('FILETL');  LUN = 16  !TILOP???.SDA
      CASE ('PFILE');   LUN = 17  !Phosphorus input files
      CASE ('FILEIO');  LUN = 21  !temp. input file; dssat45.inp
      CASE ('DTACDE');  LUN = 22  !DATA.CDE
      CASE ('FILETMP'); LUN = 23  !Tony Hunt temp file
      CASE ('SIMCNTL'); LUN = 24  !Simulation Control file
      CASE ('DSPRO');   LUN = 25  !DSSATPRO file
      CASE ('FILEWC');  LUN = 26  !*.cli - climate summary files
      CASE ('FILEWG');  LUN = 27  !*.wtg - generated weather files

!     Currently 30 is the highest number for reserved logical units
!     Change value in subroutine OUTFILES if necessary

!     Reserve unit #30 for OUTLUN
      CASE ('LUN.LST'); LUN = OUTLUN  !List of output files
      CASE DEFAULT;     LUN = 0
      END SELECT

      IF (LUN == 0) THEN
!       Assign output file names:
        DO I = 1, FileData % NumFiles
          FileName = FileData % FileName(I)
          Len1 = LenString(FileName)
          ALIAS = FileData % ALIAS(I)
          Len2 = LenString(ALIAS)
          IF (FileVarName(1:Length) == FileName(1:Len1) .OR.
     &        FileVarName(1:Length) == ALIAS(1:Len2)) THEN
            LUN = FileData % LUN(I)
            EXIT
          ENDIF
        ENDDO
      ENDIF

!     Files not covered above will be assigned numbers
!     incrementally starting with unit number 110.
      IF (LUN == 0) THEN
        !First check to see if a unit number has already been
        !assigned to this FileVarName.  If so, assign same LUN.
        DO I = StartLun, StartLun + Counter
          FileName = SaveName(I)
          Len1 = LenString(FileName)
          IF (FileVarName(1:Length) == FileName(1:Len1)) THEN
            LUN = I
            EXIT 
          ENDIF
        ENDDO

        !Assign a unique unit number to this FileVarName
        IF (I .GT. StartLun + Counter) THEN
          LUN = StartLun + Counter
          Counter = Counter + 1
          SaveName(LUN) = FileVarName
        ENDIF
      ENDIF

!     Print to 'LUN.LST' file each file assigned a unit number
!     (only print the first time a unit is assigned)
!       OUTPUT.LST - ICASA format headers, etc.
!     Save FileVarName in case it is used again.
      IF (.NOT. FPRINT(LUN)) THEN
        INQUIRE (UNIT = OUTLUN, OPENED = FEXIST)
        IF (.NOT. FEXIST) THEN
          OPEN (UNIT = OUTLUN, FILE = 'LUN.LST', STATUS = 'OLD',
     &      POSITION = 'APPEND')
        ENDIF
        WRITE(OUTLUN,'(I5,2X,A)') LUN, FileVarName
        FPRINT(LUN) = .TRUE.
        SaveName(LUN) = FileVarName
      ENDIF

!      CLOSE(OUTLUN)

      RETURN
      END SUBROUTINE GETLUN
C=======================================================================


!=======================================================================
!  HEADER, Subroutine, C. H. Porter
!-----------------------------------------------------------------------
!  Writes simulation header info to file.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!========================================================================
!  09/25/2007 CHP  Moved HEADER to OPHEAD.for
!=======================================================================


C=======================================================================
C  GET_CROPD, Subroutine, C. H. Porter
C-----------------------------------------------------------------------
C  Assigns text description of crop based on 2 letter crop code.

C-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/19/2002 CHP Written.
C  08/22/2003 CHP revised to read crop descriptions from DETAIL.CDE
C  11/23/2004 CHP Increased length of PATHX (path for executable) to 120.
C========================================================================

      SUBROUTINE GET_CROPD(CROP, CROPD)
C-----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL READ_DETAIL, WARNING

      CHARACTER*2  CROP
      CHARACTER*6  ERRKEY
!      CHARACTER*10 FILECDE
      CHARACTER*16 CROPD
      CHARACTER*78 MSG(3)
      PARAMETER (ERRKEY = 'CROPD')

C-----------------------------------------------------------------------
      CROPD = '          '

      CALL READ_DETAIL(2, 16, CROP, "*Crop and Weed Species", CROPD)

      IF (CROPD .EQ. '          ') THEN
        WRITE(MSG(1),11) CROP , "DETAIL.CDE"
   11   FORMAT('Crop code ',A2, ' could not be found in file: ',A)
        CALL WARNING(1, ERRKEY, MSG)
      ENDIF

!Previous code:
!      SELECT CASE (CROP)
!      CASE ('BA'); CROPD = 'BARLEY    '
!      CASE ('BN'); CROPD = 'DRY BEAN  '
!      CASE ('BR'); CROPD = 'BRACHIARIA'
!      CASE ('C3'); CROPD = 'C3-CROPS  '
!      CASE ('C4'); CROPD = 'C4-CROPS  '
!      CASE ('CB'); CROPD = 'CABBAGE   '
!      CASE ('CS'); CROPD = 'CASSAVA   '
!      CASE ('CH'); CROPD = 'CHICKPEA  '
!      CASE ('CO'); CROPD = 'COTTON    '
!      CASE ('CP'); CROPD = 'COWPEA    '
!      CASE ('CT'); CROPD = 'CITRUS    '
!      CASE ('FA'); CROPD = 'FALLOW    '
!      CASE ('FB'); CROPD = 'FABA BEAN '
!      CASE ('G0'); CROPD = 'BAHIA     '
!      CASE ('G1'); CROPD = 'GRASS-1   '
!      CASE ('G2'); CROPD = 'GRASS-2   '
!      CASE ('G3'); CROPD = 'GRASS-3   '
!      CASE ('G4'); CROPD = 'GRASS-4   '
!      CASE ('G5'); CROPD = 'GRASS-5   '
!      CASE ('G6'); CROPD = 'GRASS-6   '
!      CASE ('G7'); CROPD = 'GRASS-7   '
!      CASE ('G8'); CROPD = 'GRASS-8   '
!      CASE ('MZ'); CROPD = 'MAIZE     '
!      CASE ('ML'); CROPD = 'MILLET    '
!      CASE ('PE'); CROPD = 'PEA       '
!      CASE ('PI'); CROPD = 'PINEAPPLE '
!      CASE ('PN'); CROPD = 'PEANUT    '
!      CASE ('PP'); CROPD = 'PIGEONPEA '
!      CASE ('PR'); CROPD = 'PEPPER    '
!      CASE ('PT'); CROPD = 'POTATO    '
!      CASE ('RI'); CROPD = 'RICE      '
!      CASE ('SB'); CROPD = 'SOYBEAN   '
!      CASE ('SC'); CROPD = 'SUGARCANE '
!      CASE ('SG'); CROPD = 'SORGHUM   '
!      CASE ('SU'); CROPD = 'SUNFLOWER '
!      CASE ('TM'); CROPD = 'TOMATO    '
!      CASE ('TN'); CROPD = 'TANIER    '
!      CASE ('TR'); CROPD = 'TARO      '
!      CASE ('VB'); CROPD = 'VELVETBEAN'
!      CASE ('WH'); CROPD = 'WHEAT     '
!      END SELECT

      RETURN
      END SUBROUTINE GET_CROPD
C=======================================================================



C=======================================================================
C  INTERPOLATE, Subroutine
C
C  Interpolates and coagulates across/between layers

! CHP 2019-04-04 This is an accumulation rather than an interpolation.
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P. Wilkens  2-8-93
C-----------------------------------------------------------------------
C  INPUT  : NOUTDM,DAP,YRDOY,PCINPD,PG,GROWTH,MAINR,GRWRES,CADLF,CADST,
C           CMINEA,THUMC,CUMRES,TOTWT,RHOL,RHOS,PGNOON,PCINPN
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : OPDAY
C
C  Calls  :
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      REAL FUNCTION INTERPOLATE (ARRVAR, DEPTH, DS)
      USE MODULEDEFS
      IMPLICIT  NONE

      REAL      ARRVAR(NL),X(NL),DS(NL), DEPTH,TOTALVAL,TOTDEPTH,DIFF
      INTEGER   I

   !   DATA X   /5.,10.,15.,15.,15.,30.,30.,30.,30.,30./
      X = DS

      IF (DEPTH .LE. X(1)) THEN
         !
         ! Depth is <= to 5 cm
         !
         INTERPOLATE = ARRVAR(1)
         RETURN
      ENDIF

      TOTDEPTH = 0.0
      TOTALVAL = 0.0

      DO I = 1, 10
         IF (TOTDEPTH + X(I) .LE. DEPTH) THEN
           !
           ! We have not yet reached the queried depth
           !
           TOTDEPTH = TOTDEPTH + X(I)
           TOTALVAL = TOTALVAL + ARRVAR(I)*X(I)
!           IF (TOTDEPTH .EQ. DEPTH) THEN
           IF (ABS(TOTDEPTH - DEPTH) .LT. 0.0005) THEN
              INTERPOLATE = TOTALVAL / TOTDEPTH
              EXIT
           ENDIF
         ELSE
           !
           ! Ended up in the middle of a layer .. mix it
           !
           DIFF     = DEPTH - TOTDEPTH
           TOTDEPTH = DEPTH
           TOTALVAL = TOTALVAL + ARRVAR(I)*DIFF
           INTERPOLATE = TOTALVAL / TOTDEPTH
           EXIT
         ENDIF
      END DO

      END FUNCTION INTERPOLATE
C=======================================================================

C=======================================================================
C  OPCLEAR, Subroutine C.H. Porter
C  Delete *.OUT files 
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  07-22-2002 CHP Written.
C  03/07/2005 CHP Read previous output file names from OUTPUT.LST 
C                 rather than delete all *.OUT files.
!  11/02/2006 CHP If OUTPUT.CDE not available, save *.OUT files in temp
!                 directory.
C=======================================================================
      SUBROUTINE OPCLEAR
      USE ModuleDefs
!cDEC$ IF (COMPILER == 0) 
!        USE DFPORT
!cDEC$ ELSE
C        USE IFPORT
!cDEC$ ENDIF
      IMPLICIT NONE
      EXTERNAL GETLUN, OUTFILES, WARNING
!     Can't list routine SYSTEM as external because it generates an
!       error with some compilers.

      Type (OutputType) FileData
      CHARACTER*16, DIMENSION(MaxFiles) :: FileName
      CHARACTER*78 MSG(2)
      INTEGER ERR, I, LUN, LUNTMP, SYS, SYSTEM
      LOGICAL FEXIST

!     Read in list of possible output file names
      CALL OUTFILES(FileData)
      FileName = FileData % FileName

      IF (FileData % NumFiles > 0) THEN
!       File data was read from OUTPUT.CDE
!       Find available logical unit for temporary file open commands.
        CALL GETLUN("OPCLR", LUN)

!       Loop thru files, if it exists, delete it.
        DO I = 1, FileData % NumFiles
          OPEN (FILE=trim(FileName(I)),UNIT=LUN,STATUS='OLD',IOSTAT=ERR)
          IF (ERR == 0) CLOSE(LUN,STATUS='DELETE',ERR=50)
  50    CONTINUE
        ENDDO

      ELSE
!       File data was not succesfully read from OUTPUT.CDE
!       Copy *.OUT files to *.BAK files, then delete

        INQUIRE (FILE="HEADER.OUT", EXIST=FEXIST)
        IF (FEXIST) THEN

!         Open temporary batch file
          CALL GETLUN("OUTBAT", LUNTMP)
          OPEN (LUNTMP, FILE="TEMP.BAT", STATUS = 'REPLACE')
          WRITE(LUNTMP,'(A)') "ECHO OFF"
          WRITE(LUNTMP,'(A)') "COPY /Y *.OUT *.BAK"
          WRITE(LUNTMP,'(A)') "ERASE *.OUT"
          CLOSE (LUNTMP)

          SYS = SYSTEM("TEMP.BAT >TEMP.BAK")
          
!         Delete TEMP.BAT file
          OPEN (LUNTMP, FILE = "TEMP.BAT", STATUS = 'UNKNOWN')
          CLOSE (LUNTMP, STATUS = 'DELETE')
  
!         Delete TEMP.BAK file
          OPEN (LUNTMP, FILE = "TEMP.BAK", STATUS = 'UNKNOWN')
          CLOSE (LUNTMP, STATUS = 'DELETE')

!         Output.CDE file is missing
          WRITE(MSG(1),'(A,A)') "OUTPUT.CDE file not found."
          MSG(2)=
     &          "Previous *.OUT files will be saved as *.BAK files."
          CALL WARNING(2,'CSM',MSG)
        ENDIF
      ENDIF

      RETURN
      END SUBROUTINE OPCLEAR
C=======================================================================


C=======================================================================
C  OPNAMES, Subroutine C.H. Porter
C  Use alternate output filenames if FNAME is set.
C  File commands are written to a DOS batch file so the screen output 
C    can be controlled.
C  To expand the list of files: Increase MaxFiles, Add filename and 
C    character code to list.
C  Write names of output files to OUTPUT.LST file
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  07/22/2002 CHP Written.
!  01/11/2005 CHP Added a few more output files to the list
!  07/27/2006 CHP Get filenames from FileData, rather than hardwire
C=======================================================================
      SUBROUTINE OPNAMES(FNAME)
      USE ModuleDefs
      USE ModuleData
!!!!cDEC$ IF (COMPILER == 0) 
!        USE DFPORT
!!!!cDEC$ ELSE
C        USE IFPORT
!!!!cDEC$ ENDIF
      IMPLICIT NONE
      EXTERNAL GETLUN

      SAVE
      INTEGER i, COUNT, LUNLST, LUNTMP, SYS, SYSTEM
      INTEGER FNUM

      CHARACTER*8  FNAME
      CHARACTER*12  TempName
      CHARACTER*16, DIMENSION(MaxFiles) :: FileName
      CHARACTER*2,  DIMENSION(MaxFiles) :: OPCODE
      CHARACTER*50, DIMENSION(MaxFiles) :: Comment
      CHARACTER*80 BatchCommand

      LOGICAL FOPEN, FEXIST

      TYPE (SwitchType) ISWITCH

      Type (OutputType) FileData

!-----------------------------------------------------------------------
!     If list of names is absent, can't rename files...
      CALL GET(FileData)
      FileName = FileData % FileName
      OPCODE   = FileData % OPCODE
      Comment  = FileData % Description

      IF (FileData % NumFiles == 0) RETURN

!-----------------------------------------------------------------------
!     If FNAME = 'OVERVIEW', then keep default names
!     If FNAME not equal to 'OVERVIEW': 
!       Assign output file names based on FNAME.

!     The following files will be copied to a file of the form 
!     FNAME.Occ, 
!     where FNAME is read from FILEIO and sent from main program, 
!           cc  = Alternate filename character code (see below).

!-----------------------------------------------------------------------
!     Open OUTPUT.LST file - list of new output files
      CALL GETLUN("OUTO", LUNLST)
      OPEN (LUNLST, FILE="OUTPUT.LST", STATUS = 'REPLACE')

      WRITE(LUNLST,10)
   10 FORMAT('*Output file list',
     &    //,'@FILENAME          DESCRIPTION')

      IF (FNAME .EQ. 'OVERVIEW') THEN
!       Keep filenames, just write to OUTPUT.LST file
        DO i = 1, FileData % NumFiles
          !Check if FileName(i) exists, if not, go on to the next file
          INQUIRE (FILE = Trim(FileName(i)), EXIST = FEXIST)
          IF (.NOT. FEXIST) CYCLE   
          WRITE(LUNLST,'(A16,3X,A50)') FileName(i), Comment(i)
        Enddo

      ELSE
!       Re-name output files based on FNAME
!       Open temporary batch file
        CALL GETLUN("OUTBAT", LUNTMP)
        OPEN (LUNTMP, FILE="TEMP.BAT", STATUS = 'REPLACE')
        COUNT = 0

        DO i = 1, FileData % NumFiles
          !Check if FileName(i) exists, if not, go on to the next file
          INQUIRE (FILE = Trim(FileName(i)), EXIST = FEXIST)
          IF (.NOT. FEXIST) CYCLE   

          !Determine new file name and store as TempName
          IF (OPCODE(i) /= '  ') THEN
            TempName = FNAME // '.' // 'O' // OPCODE(i) 
            WRITE(LUNLST,'(A12,5X,A50)') TempName, Comment(i)

            !Check if TempName exists, if so, delete it.
            INQUIRE (FILE = TempName, EXIST = FEXIST)
            IF (FEXIST) THEN   
              BatchCommand = 'ERASE ' // TempName
              WRITE(LUNTMP, '(A50)') BatchCommand
              COUNT = COUNT + 1
            ENDIF

            !Copy from default filename into new filename 
            BatchCommand = 'COPY ' // FileName(i) // ' '  
     &                             // TempName // ' /Y'
            WRITE(LUNTMP,'(A50)') BatchCommand

            !Delete old file
            BatchCommand = 'ERASE ' // FileName(i)
            WRITE(LUNTMP,'(A50)') BatchCommand
            WRITE(LUNTMP, '(" ")')
            COUNT = COUNT + 2

            !If file was left open, close it now.
            INQUIRE(FILE=FILENAME(I), OPENED=FOPEN)
            IF (FOPEN) THEN
              INQUIRE(FILE=FILENAME(I), NUMBER=FNUM)
              CLOSE(FNUM)
            ENDIF

          ELSE
!           Don't rename
            !Check if FileName(i) exists, if not, go on to the next file
            INQUIRE (FILE = Trim(FileName(i)), EXIST = FEXIST)
            IF (FEXIST) THEN
              WRITE(LUNLST,'(A16,3X,A50)') FileName(i), Comment(i)
            ENDIF
          ENDIF 
        Enddo

        CLOSE (LUNTMP)

        IF (COUNT > 0) THEN
!         Run batch file - direct output to TEMP.BAK file
          BatchCommand = "TEMP.BAT >TEMP.BAK"
C-KRT February 2, 2024
C-KRT This file renaming strategy is not compatible with Linux systems.
C-KRT The Linux operating system will fail to run the following BatchCommand.
C-KRT Linux OS prints "sh: 1: TEMP.BAT: not found" because it can't
C-KRT find a system command called TEMP.BAT. Anyway, the commands
C-KRT given in the batch file are for Windows systems.
C-KRT Model will continue to run; however, the output files are not
C-KRT copied to the new output file names as directed in BatchCommand.
C-KRT Need to rework this strategy for OS compatibility.
          SYS = SYSTEM(BatchCommand)
  
!         Delete TEMP.BAT file
          OPEN (LUNTMP, FILE = "TEMP.BAT", STATUS = 'UNKNOWN')
          CLOSE (LUNTMP, STATUS = 'DELETE')
  
!         Delete TEMP.BAK file
          OPEN (LUNTMP, FILE = "TEMP.BAK", STATUS = 'UNKNOWN')
          CLOSE (LUNTMP, STATUS = 'DELETE')
        ELSE
!         Close empty batch file
          CLOSE (LUNTMP, STATUS = 'DELETE')
        ENDIF

      ENDIF

      CALL GET (ISWITCH)
      IF (INDEX('0N',ISWITCH % IDETL) < 1) THEN
        CLOSE (LUNLST)
      ELSE
        CLOSE (LUNLST, STATUS = 'DELETE')
      ENDIF

      RETURN
      END SUBROUTINE OPNAMES
C=======================================================================

C=======================================================================
C  ALIN, Function
C
C  Linear intepolation routine
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2. Header revision and minor changes           P.W.W.      8-7-93
C-----------------------------------------------------------------------
C  INPUT  : TABX,TABY,N
C
C  LOCAL  : I
C
C  OUTPUT : XVAL
C-----------------------------------------------------------------------
C  Called : NFACTO
C
C  Calls  : None
C=======================================================================

      REAL FUNCTION ALIN (TABX,TABY,N,XVAL)

      IMPLICIT  NONE

      INTEGER   N,I
      REAL      TABX,TABY,XVAL

      DIMENSION TABX(N),TABY(N)

      IF (XVAL .LE. TABX(1)) THEN
         ALIN = TABY(1)
         RETURN
      ENDIF
      IF (XVAL .GE. TABX(N)) THEN
         ALIN = TABY(N)
         RETURN
      ENDIF

      DO I = 2, N
        IF (XVAL .LE. TABX(I)) EXIT
      END DO

      ALIN = (XVAL-TABX(I-1))*(TABY(I)-TABY(I-1))/
     &            (TABX(I)-TABX(I-1))+TABY(I-1)

      RETURN    
      END FUNCTION ALIN 
C=======================================================================


!=======================================================================
!  OUTFILES, Subroutine C.H. Porter
!  Reads available output files from OUTPUT.CDE
!  08/16/2005 CHP These alternate names must be coordinated with the 
!                 DSSAT shell. Do not change arbitrarily.
!  07/27/2006 CHP Read list of files from OUTPUT.CDE rather than
!                 hardwire.  Assign unit numbers here.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  03/09/2005 CHP Written.
!  01/11/2007 CHP Changed GETPUT calls to GET and PUT
!  01/07/2022 FO  Find *.OUT files for MacOS and Linux systems
!=======================================================================
      SUBROUTINE OUTFILES(FileData)
      USE ModuleDefs 
      USE ModuleData
      IMPLICIT NONE
      EXTERNAL get_dir, IGNORE, WARNING
      SAVE

      CHARACTER*6  ERRKEY
      CHARACTER*10 FILECDE
      CHARACTER*78 MSG(2)
      CHARACTER*80 CHARTEST
      CHARACTER*120 DATAX, PATHX
      CHARACTER(len=255) :: DSSAT_HOME

      INTEGER ERR, I, ISECT, LNUM, LUN
      LOGICAL FEXIST      !EOF, 
      TYPE (OutputType) FileData

      DATA FILECDE /'OUTPUT.CDE'/
      PARAMETER (ERRKEY = 'OUTFLE')

!-----------------------------------------------------------------------
!     Initialize
      FileData % FileName    = ''
      FileData % OpCode      = ''
      FileData % Description = ''
      FileData % ALIAS       = ''
      FileData % LUN         = 0
      FileData % NumFiles    = 0

!     Does file exist in data directory?
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
!       Check for file in C:\DSSAT48 directory
        DATAX = trim(STDPATH) // FILECDE
        INQUIRE (FILE = DATAX, EXIST = FEXIST)
      ENDIF
      
! FO - 01/07/2022 - Update for MacOS and Linux systems.
!     This is check at DSSAT_HOME is neede to remove old *.OUT
!     files for new runs.
      IF (.NOT. FEXIST) THEN
        CALL get_environment_variable("DSSAT_HOME", DSSAT_HOME)
        IF(TRIM(DSSAT_HOME) .NE. '') THEN
            STDPATH = TRIM(DSSAT_HOME)
        ENDIF
        DATAX = trim(STDPATH) // FILECDE
        INQUIRE (FILE = DATAX,EXIST = FEXIST)
      ENDIF

      IF (.NOT. FEXIST) THEN
        MSG(1) = "OUTPUT.CDE file not found"
        MSG(2) = "Error in subroutine OUTFILES"
        CALL WARNING(2, ERRKEY, MSG)
!        CALL ERROR (ERRKEY,2,DATAX,0)
      ENDIF

      IF (FEXIST) THEN
        LUN = 22
        OPEN (LUN, FILE=DATAX, STATUS = 'OLD', IOSTAT=ERR)
        IF (ERR /= 0) THEN
          FEXIST = .FALSE.
        ELSE
          I = 0
          DO WHILE (I <= MaxFiles)   !.NOT. EOF(LUN) .AND. 
            CALL IGNORE(LUN,LNUM,ISECT,CHARTEST)
            IF (ISECT == 0) EXIT
            IF (ISECT /= 1) CYCLE
            I = I + 1
            READ (CHARTEST,'(A16,A2,1X,A50,1X,A10)',IOSTAT=ERR)
     &        FileData % FileName(I), 
     &        FileData % OpCode(I), 
     &        FileData % Description(I), 
     &        FileData % ALIAS(I)
            FileData % LUN(I) = I + 30
          ENDDO
          FileData % NumFiles = I
          CLOSE(LUN)
        ENDIF
      ENDIF

      CALL PUT(FileData)

      RETURN
      END SUBROUTINE OUTFILES
C=======================================================================

!=======================================================================
!  LenString, Function
!
!  Function to return the length of a character string, excluding 
!     trailing blanks.  This is the same as the fortran function
!     LEN.  When the string is read from an ASCII file, the LEN
!     function does not recognize the blanks as blanks.  Same for 
!     functions ADJUSTL, ADJUSTR, LEN_TRIM.
!-----------------------------------------------------------------------
!  Revision history
!
!  10/24/2005 CHP Written
!=======================================================================

      INTEGER FUNCTION LenString (STRING)

      IMPLICIT  NONE

      CHARACTER(len=*) STRING
      CHARACTER(len=1) CHAR
      INTEGER   I, Length, CHARVAL
      
      LenString = 0

      Length = LEN(STRING)
      DO I = Length, 1, -1
        CHAR = STRING(I:I)
        CHARVAL = ICHAR(CHAR)
        IF (CHARVAL < 33 .OR. CHARVAL > 126) THEN
          CYCLE
        ELSE
          LenString = I
          EXIT
        ENDIF
      ENDDO

      RETURN
      END FUNCTION LenString

!=======================================================================


!=======================================================================
!  StartString, Function
!
!  Function to return the location of the first non-blank character
!     in a string.  Works with strings read from ASCII file, where 
!     blanks may not be recognized.
!-----------------------------------------------------------------------
!  Revision history
!
!  11/30/2007 CHP Written
!=======================================================================

      INTEGER FUNCTION StartString (STRING)

      IMPLICIT  NONE
      EXTERNAL LenString

      CHARACTER(len=*) STRING
      CHARACTER(len=1) CHAR
      INTEGER   CHARVAL, I, LenString
      
      StartString = 0
      DO I = 1, LenString (STRING)
        CHAR = STRING(I:I)
        CHARVAL = ICHAR(CHAR)
        IF (CHARVAL < 33 .OR. CHARVAL > 126) THEN
          CYCLE
        ELSE
          StartString = I
          EXIT
        ENDIF
      ENDDO
      RETURN
      END FUNCTION StartString

!=======================================================================

!=======================================================================
!  Join_Trim, Subroutine
!
!  Subroutine concatenates two strings and ignores leading or trailing 
!     blanks.
!-----------------------------------------------------------------------
!  Revision history
!
!  11/30/2007 CHP Written
!=======================================================================

      Subroutine Join_Trim (STRING1, STRING2, JoinString)

      IMPLICIT NONE
      EXTERNAL LenString, StartString

      CHARACTER(len=*) STRING1, STRING2, JoinString
      INTEGER EndLen1, EndLen2, LenString
      INTEGER StartLen1, StartLen2, StartString
      
      EndLen1 = LenString(STRING1)
      IF (EndLen1 > 0) THEN
        StartLen1 = StartString(STRING1)
      ENDIF
      
      EndLen2 = LenString(STRING2)
      IF (EndLen2 > 0) THEN
        StartLen2 = StartString(STRING2)
      ENDIF

      IF (EndLen1 * EndLen2 > 0) THEN
        JoinString = STRING1(StartLen1:EndLen1) // 
     &               STRING2(StartLen2:EndLen2)
      ELSEIF (EndLen1 > 0) THEN
        JoinString = STRING1(StartLen1:EndLen1)
      ELSEIF (EndLen2 > 0) THEN
        JoinString = STRING2(StartLen2:EndLen2)
      ELSE
        JoinString = ""
      ENDIF

      RETURN
      END Subroutine Join_Trim

!=======================================================================



!!=======================================================================
!!  path_adj, Subroutine
!!
!!  Subroutine adjusts path for indications of relative path such as up one 
!!    directory '..'
!!-----------------------------------------------------------------------
!!  Revision history
!!
!!  3/26/2014 PDA Written
!!=======================================================================
!
!      subroutine path_adj(path)
!
!        use moduledefs
!
!        implicit none
!
!        character(len=*),intent(inout) :: path
!
!        integer           :: p1,p2,p3
!
!          p1=index(path,'.')
!          p2=index(path,'..')
!
!          if(p1>1)then
!             p3=len_trim(path)
!             p1=index(path(1:p1),slash,back=.true.)
!             if(p2>0)then
!                p2=p2+3
!                p1=index(path(1:(p1-1)),slash,back=.true.)
!             else
!                p2 = p1 + 2
!             end if
!                path = path(1:p1)//path(p2:p3)
!          end if
!
!      end subroutine path_adj
!
!  3/26/2014 PDA Written
!=======================================================================

      subroutine path_adj(path)

        use moduledefs

        implicit none

        character(len=*),intent(inout) :: path

        integer           :: p1,p2,p3
        
           p1 = index(path,slash//'..'//slash)

           do while(p1>1)
              p2 = p1 + 4
              p1=index(path(1:(p1-1)),slash,back=.true.)
              p3=len_trim(path)
              if(p3<p1+4)then
                 path = path(1:p1)
              else
                 path = path(1:p1)//path(p2:p3)
              end if
              p1 = index(path,slash//'..'//slash)
           end do

      end subroutine path_adj

!=======================================================================
!  skipspc, Subroutine
!
!  Subroutine to skip spaces in a string
!-----------------------------------------------------------------------
!  Revision history
!
!  3/26/2014 PDA Added to CSM
!=======================================================================

      SUBROUTINE SKIPSPC(STRING,POS,DIRECTION)
    
        IMPLICIT NONE

        INTEGER         :: POS

        CHARACTER(LEN=*):: STRING
        CHARACTER(LEN=*):: DIRECTION
        
      
        SELECT CASE(DIRECTION)
            CASE('R','r')
                DO WHILE(STRING(POS:POS)==' ')
                    IF (POS < LEN(STRING)) THEN
                        POS = POS + 1
                    ELSE
                        EXIT
                    ENDIF
                ENDDO
            CASE('L','l')
                DO WHILE(STRING(POS:POS)==' ')
                    IF (POS > 1 ) THEN
                        POS = POS - 1
                    ELSE
                        EXIT
                    ENDIF
                ENDDO
        END SELECT
        
      END SUBROUTINE SKIPSPC


!=======================================================================
!  get_next_string, Subroutine
!
!  Subroutine to get next non-space substring from a larger string
!-----------------------------------------------------------------------
!  Revision history
!
!  3/26/2014 PDA Added to CSM
!=======================================================================

      subroutine get_next_string(full_string,start,next_string)

        implicit none
        external skipspc

        character(len=*),intent(in)  :: full_string
        character(len=*),intent(out) :: next_string
        integer,intent(in)           :: start
        integer                      :: pos

        pos = start + index(full_string(start:len(full_string)),' ') - 1

        call skipspc(full_string,pos,'R')

        read(full_string(pos:len(full_string)),'(a)') next_string

      end subroutine get_next_string

!=======================================================================
!  get_dir, Subroutine
!
!  Subroutine to strip file name from full path and return only the directory
!-----------------------------------------------------------------------
!  Revision history
!
!  3/26/2014 PDA Written
!=======================================================================

      subroutine get_dir(full_path,only_dir)

        use moduledefs

        implicit none

        character(len=*),intent(in)  :: full_path
        character(len=*),intent(out) :: only_dir
        integer pos

        pos = index(full_path,slash,back=.true.)

        only_dir = full_path(1:pos)        

      end subroutine get_dir
!=======================================================================
!  ROUND, Subroutine
!  Subroutine to round decimal part of a number in Fortran
!-----------------------------------------------------------------------
!  Revision history
!  07/11/2024 FO Added round a number function
!=======================================================================
      REAL FUNCTION ROUND(VAL, N)
        IMPLICIT NONE
        REAL VAL
        INTEGER N
        ROUND = ANINT(VAL*10.0**N)/10.0**N
        RETURN
      END FUNCTION ROUND
!=======================================================================