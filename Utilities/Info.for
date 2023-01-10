C=======================================================================
C  INFO, Subroutine, C.H.PORTER
C  Writes informational messages to INFO.OUT file
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  07/27/2006 CHP Written
!  01/11/2007 CHP Changed GETPUT calls to GET and PUT
C=======================================================================

      SUBROUTINE INFO (ICOUNT, ERRKEY, MESSAGE)

!     FILEIO and RUN needed to generate header for INFO.OUT file

      USE ModuleDefs
      USE ModuleData
      USE HeaderMod
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, YR_DOY
      SAVE

      CHARACTER*(*) ERRKEY
      CHARACTER*11, PARAMETER :: InfoOut = 'INFO.OUT'
      CHARACTER*30  FILEIO
      CHARACTER*(*) MESSAGE(*)

      INTEGER ICOUNT, DOY, I, LUN, OLDRUN, RUN, YEAR, YRDOY
      LOGICAL FIRST, FEXIST, FOPEN

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH

      DATA FIRST /.TRUE./
      DATA OLDRUN /0/

!-----------------------------------------------------------------------
!     Suppress INFO.OUT if IDETL = '0' (zero) or 'N' 
      CALL GET(ISWITCH)
      IF (INDEX('0N',ISWITCH % IDETL) > 0) RETURN

      CALL GET(CONTROL)
      FILEIO = CONTROL % FILEIO
      RUN    = CONTROL % RUN
      YRDOY  = CONTROL % YRDOY
      
!      IF (INDEX(ERRKEY,'ENDRUN') <= 0) THEN
!       First time routine is called to print, open file.
!       File will remain open until program execution is stopped.
        IF (FIRST) THEN

!         Check for IDETL = '0' (zero) --> suppress output
          CALL GET(ISWITCH)
          IF (ISWITCH % IDETL == '0') RETURN

          CALL GETLUN('OUTINFO', LUN)
          INQUIRE (FILE = InfoOut, EXIST = FEXIST)
          IF (FEXIST) THEN
            INQUIRE (FILE = InfoOut, OPENED = FOPEN)
            IF (.NOT. FOPEN) THEN
              OPEN (UNIT=LUN, FILE=InfoOut, STATUS='OLD',
     &            POSITION='APPEND')
            ENDIF
          ELSE
            OPEN (UNIT=LUN, FILE=InfoOut, STATUS='NEW')
            WRITE(LUN,'("*INFO DETAIL FILE")')
          ENDIF

          WRITE(LUN,'(/,78("*"))')
!          IF (CONTROL % MULTI > 1) CALL MULTIRUN(RUN, 0)
          IF (Headers % RUN == RUN) THEN
            CALL HEADER(SEASINIT, LUN, RUN)
            FIRST = .FALSE.
            OLDRUN = RUN
          ENDIF 
		ELSE
!         VSH
          CALL GETLUN('OUTINFO', LUN)
          INQUIRE (FILE = InfoOut, OPENED = FOPEN)
          IF (.NOT. FOPEN) THEN
             OPEN (UNIT=LUN, FILE=InfoOut, STATUS='OLD',
     &             POSITION='APPEND')
          ENDIF               
        ENDIF   ! for FIRST

        IF (ICOUNT > 0) THEN
          !Print header if this is a new run.
          IF (OLDRUN .NE. RUN .AND. RUN .NE. 0 .AND. FILEIO .NE. "")THEN
            IF (Headers % RUN == RUN) THEN
              CALL HEADER(SEASINIT,LUN,RUN)
              OLDRUN = RUN
            ENDIF
          ENDIF

!         Print the INFO.  Message is sent from calling routine as text.
          CALL YR_DOY(YRDOY, YEAR, DOY)
          WRITE(LUN,'(/,1X,A,"  YEAR DOY = ",I4,1X,I3)')ERRKEY,YEAR,DOY
          DO I = 1, ICOUNT
            WRITE(LUN,'(1X,A)') MESSAGE(I)
          ENDDO
        ENDIF

!      ELSE    !ERRKEY = 'ENDRUN' -> End of season
      IF (INDEX(ERRKEY,'ENDRUN') > 0) THEN
        FIRST = .TRUE.
        CLOSE(LUN)
      ENDIF

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE INFO
