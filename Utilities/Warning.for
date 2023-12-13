C=======================================================================
C  WARNING, Subroutine, C.H.PORTER
C  Writes warning messages to Warning.OUT file
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  03/21/2002 CHP Written
C  09/03/2004 CHP Modified call to GETPUT_CONTROL 
C  03/22/2005 CHP Added option to suppress Warning.OUT messages with 
C                 IDETL = 0 (zero).
!  05/04/2005 CHP Added date to warning message.
!  01/11/2007 CHP Changed GETPUT calls to GET and PUT
C=======================================================================

      SUBROUTINE WARNING (ICOUNT, ERRKEY, MESSAGE)

!     FILEIO and RUN needed to generate header for WARNING.OUT file

      USE ModuleDefs
      USE ModuleData
      USE HeaderMod
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, YR_DOY
      SAVE

      CHARACTER*(*) ERRKEY
      CHARACTER*11, PARAMETER :: WarnOut = 'WARNING.OUT'
      CHARACTER*30  FILEIO
      CHARACTER*(*)  MESSAGE(*)

      INTEGER ICOUNT, DOY, I, LUN, OLDRUN, RUN, YEAR, YRDOY, ErrCode
      LOGICAL FIRST, FEXIST, FOPEN

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH

      DATA FIRST /.TRUE./
      DATA OLDRUN /0/

!-----------------------------------------------------------------------
!     Suppress Warning.OUT if IDETL = '0' (zero)
      CALL GET(ISWITCH)
!     IF (ISWITCH % IDETL == '0') RETURN

      CALL GET(CONTROL)
      FILEIO = CONTROL % FILEIO
      RUN    = CONTROL % RUN
      YRDOY  = CONTROL % YRDOY
      ErrCode = CONTROL % ErrCode

      IF (INDEX(ERRKEY,'ENDRUN') <= 0) THEN
!       First time routine is called to print, open file.
!       File will remain open until program execution is stopped.
        IF (FIRST) THEN

!         Check for IDETL = '0' (zero) --> suppress output
          CALL GET(ISWITCH)
          IF (ISWITCH % IDETL == '0' .AND. ErrCode <=0) RETURN

          CALL GETLUN(WarnOut, LUN)
          INQUIRE (FILE = WarnOut, EXIST = FEXIST)
          IF (FEXIST) THEN
            INQUIRE (FILE = WarnOut, OPENED = FOPEN)
            IF (.NOT. FOPEN) THEN
              OPEN (UNIT=LUN, FILE=WarnOut, STATUS='OLD',
     &            POSITION='APPEND')
            ENDIF
          ELSE
            OPEN (UNIT=LUN, FILE=WarnOut, STATUS='NEW')
            WRITE(LUN,'("*WARNING DETAIL FILE")')
          ENDIF

          WRITE(LUN,'(/,78("*"))')
          IF (CONTROL % MULTI > 1) CALL MULTIRUN(RUN,0)
          IF (Headers % RUN == RUN) THEN
            CALL HEADER(SEASINIT, LUN, RUN)
            FIRST = .FALSE.
            OLDRUN = RUN
          ENDIF
        ENDIF
!         VSH
          CALL GETLUN('OUTWARN', LUN)
          INQUIRE (FILE = WarnOut, OPENED = FOPEN)
          IF (.NOT. FOPEN) THEN
             OPEN (UNIT=LUN, FILE=WarnOut, STATUS='OLD',
     &             POSITION='APPEND')
          ENDIF          
      ENDIF

      IF (ICOUNT > 0) THEN
        !Print header if this is a new run.
        IF (OLDRUN .NE. RUN .AND. RUN .NE. 0 .AND. FILEIO .NE. "")THEN
          IF (Headers % RUN == RUN) THEN
            CALL HEADER(SEASINIT,LUN,RUN)
            OLDRUN = RUN
          ENDIF
        ENDIF

!       Print the warning.  Message is sent from calling routine as text.
        CALL YR_DOY(YRDOY, YEAR, DOY)
        WRITE(LUN,'(/,1X,A,"  YEAR DOY = ",I4,1X,I3)')ERRKEY,YEAR,DOY
        DO I = 1, ICOUNT
          WRITE(LUN,'(1X,A78)') MESSAGE(I)
        ENDDO
      ENDIF

!     ERRKEY = 'ENDRUN' -> End of season
      IF (INDEX(ERRKEY,'ENDRUN') > 0) THEN    
        FIRST = .TRUE.
        CLOSE(LUN)
      ENDIF

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE WARNING
