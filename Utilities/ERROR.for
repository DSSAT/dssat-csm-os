C=======================================================================
C  ERROR, Subroutine, N.B. Pickering
C  Outputs error messages to screen from file ERROR.DAT
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  08/30/1991 NBP Written
C  10/31/1994 GH  Add option to read MODEL.ERR from model EXE path
C  12/04/2001 CHP Added call to GETLUN for unit number.
!  11/05/2002 AJG Increased the size of PATHX.
C  07/21/2003 CHP Added call to WARNING.out for error messages.  
C                 Added generic messages for open and read errors.
C  11/23/2004 CHP Increased length of PATHX (path for executable) to 120.
C-----------------------------------------------------------------------
C  Input : ERRKEY,ERRNUM,FILE,LNUM)
C  Output: message to screen
C  Local :
C  Ifile : MODEL.ERR
C  FN/SUB: FIND
C=======================================================================

      SUBROUTINE ERROR (ERRKEY,ERRNUM,FILE,LNUM)

      USE ModuleDefs
      USE ModuleData
      IMPLICIT      NONE
      EXTERNAL GENERIC_MSG, GET_DIR, GETLUN, HEADER, WARNING

      CHARACTER*(*) ERRKEY,FILE
      CHARACTER*9   EFILE
      CHARACTER     AKEY*6,BLANK*80,KEY*6,LINE*80
      CHARACTER*78  MSG(10)
      CHARACTER*100 ERRORX, SAVE_ERRORX
      CHARACTER*120 PATHX

      INTEGER       ANUM,ERRNUM,LNUM, LUN, I , ELUN
      INTEGER       IMSG

      LOGICAL       FEXIST, FOUND  !, EOF


      PARAMETER     (BLANK = ' ')

      TYPE (ControlType) CONTROL
      CALL GET(CONTROL)

      IMSG = 1
      EFILE = 'ERROR.OUT'
      CALL GETLUN('ERRORO', ELUN)

      INQUIRE (FILE = EFILE, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = ELUN, FILE = EFILE, STATUS = 'OLD', 
     &    POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = ELUN, FILE = EFILE, STATUS = 'NEW')
        WRITE(ELUN,'("*RUN-TIME ERRORS OUTPUT FILE",//)')
      ENDIF

      CALL HEADER(SEASINIT, ELUN, CONTROL%RUN)
      WRITE(ELUN,'(A,", Trt",I5)') CONTROL%FILEX, CONTROL%TRTNUM

      CALL GETARG(0,PATHX)
!      call path_adj(pathx)
      call get_dir(pathx,errorx)
      errorx = trim(errorx)//'MODEL.ERR'

!     If ERRORX file is not in executable directory, try std. location
      INQUIRE (FILE = ERRORX, EXIST = FEXIST)
      IF (.NOT. FEXIST) THEN
        SAVE_ERRORX = ERRORX
        ERRORX = trim(STDPATH) // 'MODEL.ERR'
      ENDIF

      INQUIRE (FILE = ERRORX,EXIST = FEXIST)
      IF (FEXIST) THEN

         CALL GETLUN('ERRORX', LUN)
         OPEN (LUN,FILE=ERRORX,STATUS='OLD')
C
C        Initialization
C
         FOUND = .FALSE.
         IF (ERRNUM .GT. 6000 .OR. ERRNUM .LT. 0) THEN
            KEY = 'MISC  '
         ELSE
            KEY = ERRKEY
         ENDIF
C
C        Loop to search for error message in file MODEL.ERR.
C
   10    DO WHILE(.TRUE.)
           READ (LUN,'(A)',END=20) LINE
           AKEY = LINE(1:6)
           IF (AKEY .EQ. KEY) THEN
              READ (LINE,'(6X,I5)') ANUM
              IF (ANUM .EQ. ERRNUM) THEN
                 FOUND = .TRUE.
                 GOTO 20
              ENDIF
            ELSE
              FOUND = .FALSE.
           ENDIF
         ENDDO

   20    IF (FOUND) THEN
            WRITE (*,*)
            WRITE (ELUN,*)
   30       READ  (LUN,'(A)',END=40) LINE
            IF (LINE .NE. BLANK) THEN
               WRITE (*,*) LINE
               WRITE (ELUN,*) LINE
               WRITE(MSG(IMSG),'(A77)') LINE  ; IMSG = IMSG+1
               GOTO 30
            ENDIF
          ELSEIF (KEY .NE. 'GENERI') THEN
          !Check for generic message numbers
             KEY = 'GENERI'
             REWIND (LUN)
             GO TO 10

!        As an alternate, could have generic messages generated in code.
!            CALL GENERIC_MSG(ERRNUM, LINE)
!            WRITE (*,'(/,A78,/)') LINE
!            WRITE (ELUN,'(/,A78,/)') LINE
!            WRITE (MSG(IMSG),'(A78)') LINE
!            IMSG = IMSG + 1

          ELSE
!           Could not find error message in file
            WRITE (MSG(IMSG),'(A,A,I5)') 'Unknown ERROR. ',
     &           'Error number: ',ERRNUM
            WRITE (ELUN,'(/,A78,/)') MSG(IMSG)
            WRITE (*,'(/,A78)') MSG(IMSG)
            IMSG = IMSG + 1
          ENDIF

   40    IF (FILE .EQ. ' ') THEN
            WRITE (*,'(2A/)') 'Error key: ',ERRKEY
            WRITE (ELUN,'(2A/)') 'Error key: ',ERRKEY
            WRITE (MSG(IMSG),'(2A)') 'Error key: ',ERRKEY
            IMSG = IMSG + 1
          ELSE
            I = MIN(LEN(TRIM(FILE)),37)
            WRITE (*,'(3A,I5,2A/)')
     &    'File: ',FILE(1:I),'   Line: ',LNUM,'   Error key: ',ERRKEY
            WRITE (ELUN,'(3A,I5,2A/)')
     &    'File: ',FILE(1:I),'   Line: ',LNUM,'   Error key: ',ERRKEY
            WRITE (MSG(IMSG),'(2A)') 'File: ',TRIM(FILE(1:I))
            WRITE (MSG(IMSG+1),'(A,I5)') '   Line: ',LNUM
            WRITE (MSG(IMSG+2),'(2A)') '   Error key: ',ERRKEY
            IMSG = IMSG + 3
         ENDIF
         CLOSE (LUN)
      ELSE
C                                                                !BDB
C        Tell user that error file can not be found and give a   !BDB
C        generic error message.                                  !BDB
C                                                                !BDB
         ERRORX = SAVE_ERRORX
         WRITE (*,50) TRIM(ERRORX)
         WRITE (ELUN,50) TRIM(ERRORX)
         WRITE (MSG(IMSG),51) TRIM(ERRORX)
         IMSG = IMSG + 1
   50    FORMAT('Could not locate error file: ',A,/)
   51    FORMAT('Could not locate error file: ',A48)

         !Check for generic message numbers
         CALL GENERIC_MSG(ERRNUM, LINE)
         WRITE (*,'(/,A78,/)') LINE(1:78)
         WRITE (ELUN,'(/,A78,/)') LINE(1:78)
         WRITE (MSG(IMSG),'(A78)') LINE(1:78)
         IMSG = IMSG + 1

         WRITE (*,60)  FILE, LNUM, ERRKEY   
         WRITE (ELUN,60)  FILE, LNUM, ERRKEY   
         WRITE (MSG(IMSG),60) FILE, LNUM, ERRKEY
         IMSG = IMSG + 1
   60    FORMAT('File: ',A12,'   Line: ',I5,' Error key: ',A)
      ENDIF

      WRITE(ELUN,70)
   70 FORMAT("Additional information may be available ",
     &            "in WARNING.OUT file.")
      WRITE (*,70) 
      WRITE (*, *) CHAR(7)
      WRITE (*,260)
260   FORMAT (/,1X,'Please press < ENTER > key to continue ',2X,$)
C-GH      READ  (*, *)

      CLOSE (ELUN)

      WRITE(MSG(IMSG),'(A)') "Simulations terminated."
      CALL WARNING(IMSG, ERRKEY, MSG)

!      INQUIRE (FILE = "LUN.LST", EXIST = FEXIST)
!      IF (FEXIST) THEN
!        CALL GETLUN('LUN.LST', LUN)
!        INQUIRE (UNIT = LUN, OPENED = FOPEN) 
!        IF (.NOT. FOPEN) THEN
!          OPEN (FILE="LUN.LST", UNIT=LUN, ERR=99, STATUS='OLD')
!        ELSE
!          REWIND(LUN)
!        ENDIF
!
!        !Skip over first 3 lines in LUN.LST file
!        DO I=1,3
!          READ(LUN,'(A80)') LINE
!        ENDDO
!
!        !Read list of unit numbers that have been opened and close each
!!       EOF not portable
!!       DO WHILE (.NOT. EOF(LUN))
!        ERR = 0
!        DO WHILE (ERR == 0)
!          READ(LUN, '(A)', IOSTAT=ERRNUM, ERR=99, END=99) LINE
!          READ(LINE,'(I5)',IOSTAT=ERRNUM, ERR=99) LUNIT
!          IF (ERRNUM /= 0) EXIT
!          ERR = ERRNUM
!          IF (LUNIT .NE. LUN) THEN
!            CLOSE (LUNIT)
!          ENDIF
!        ENDDO
!        CLOSE (LUN)
!      ENDIF
!
   99 STOP 99
      END SUBROUTINE ERROR

!=========================================================================
      SUBROUTINE GENERIC_MSG(ERRNUM, MESSAGE)
!     If error messages cannot be found in MODEL.ERR file, or if MODEL.ERR
!     file cannot be found, check for generic message type.

      IMPLICIT NONE
      INTEGER ERRNUM
      CHARACTER*(*) MESSAGE

      !Check for generic message numbers
      SELECT CASE(ERRNUM)
        CASE(29)
          WRITE(MESSAGE,35) 'File not found. Please check ',
     &      'file name or create file. Error number: ', ERRNUM 
        CASE(33)
          WRITE(MESSAGE,35) 'End of file encountered. ',
     &      'Error number: ',ERRNUM
        CASE(59)
          WRITE(MESSAGE,35) 'Syntax error. ',
     &      'Error number: ',ERRNUM
        CASE(64)
          WRITE(MESSAGE,35) 'Invalid format in file. ',
     &      'Error number: ', ERRNUM
        CASE DEFAULT 
          WRITE(MESSAGE,35) 'Unknown ERROR. ',
     &      'Error number: ',ERRNUM
      END SELECT

   35 FORMAT(A,A,I5)

      END SUBROUTINE GENERIC_MSG
!=========================================================================

!=======================================================================
! ErrorCode, Subroutine, C.H. Porter, 02/09/2010
! Ends a run for errors by setting YREND variable.  Continue with next
!     simulation in batch.  Stops sequence simulation.

!-----------------------------------------------------------------------
! REVISION HISTORY
! 02/09/2010 CHP Written
!-----------------------------------------------------------------------
      SUBROUTINE ErrorCode(CONTROL, ErrCode, ERRKEY, YREND)

      USE ModuleDefs
      USE ModuleData
      IMPLICIT NONE
      EXTERNAL ERROR, WARNING

      CHARACTER(*) ERRKEY 
      CHARACTER*78 MSG(4)
      INTEGER ErrCode, YREND
      TYPE (ControlType) CONTROL

!-----------------------------------------------------------------------
      YREND = CONTROL%YRDOY
      CONTROL % ErrCode = ErrCode
      CALL PUT(CONTROL)

!     For sequence runs, stop run with any error
      IF(INDEX('FQ',CONTROL%RNMODE) > 0)CALL ERROR(ERRKEY,ErrCode,' ',0)

      WRITE(MSG(1),'(A,I8,A)') "Run",CONTROL%RUN, " will be terminated."
      CALL WARNING(1,ERRKEY,MSG)

      RETURN
      END SUBROUTINE ErrorCode
!=======================================================================
! Current error codes:

! Daily weather data
!  1 Header section not found in weather file.
!  2 Solar radiation data error
!  3 Precipitation data error
!  4 Tmax and Tmin are both set to 0
!  5 Tmax and Tmin have identical values
!  6 Tmax is less than Tmin
!  8 Non-sequential data in file
! 10 Weather record not found
! 29 Weather file not found
! 30 Error opening weather file
! 59 Invalid format in weather file
! 64 Syntax error.
!
! Weather modification
! 72 Solar radiation data error
! 73 Precipitation data error
! 74 Tmax and Tmin are both set to 0
! 75 Tmax and Tmin have identical values
! 76 Tmax is less than Tmin
!
! Generated weather data
! 82 Solar radiation data error
! 83 Precipitation data error
! 84 Tmax and Tmin are both set to 0
! 85 Tmax and Tmin have identical values
! 86 Tmax is less than Tmin

!100 Number of cohorts exceeds maximum.
