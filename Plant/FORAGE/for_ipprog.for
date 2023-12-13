C======================================================================
C FOR_IPPROG, Subroutine
C This subroutine reads pest observations from the time series file T
C for the experiment being simulated.
C----------------------------------------------------------------------
C REVISION HISTORY
C 01/01/1990 WDB Written
C 02/25/1998 CHP Modified for PEST Module
C 01/12/1999 GH  Incorporated into CROPGRO
C 04/15/2002 GH  Modified number of pests to 100
C 10/28/2002 CHP Modified for Y2K
C 11/14/2002 CHP Added date correction for sequenced runs
C 05/09/2003 CHP Increased number of pest observations to 200
C 03/16/2004 SJR replaced older version of this file with this one
C                  from newer release (3/04) but needed to retain
C                  FILEIO and RUN for older WARNING routine used 
C                  throughout the rest of this version
C 05/07/2020 FO  Add new Y4K subroutine call to convert YRDOY
C-----------------------------------------------------------------------
C  Called by: PEST
C  Calls:     None
C=======================================================================
      SUBROUTINE FOR_IPPROG(CONTROL, 
     &    FILET, NPEST, PID, YRPLT, TRTNO,                !Input
     &    IDAP, PCN, PNO, POBS, PSTHD, YPL)               !Output
C----------------------------------------------------------------------
C This subroutine reads pest observations from the time series file T
C for the experiment being simulated.
C----------------------------------------------------------------------
      USE MODULEDEFS
      IMPLICIT NONE
      EXTERNAL GETLUN, IGNORE2, Y2K_DOY, Y4K_DOY, YR_DOY, WARNING, 
     &  ERROR, TIMDIF
      SAVE
C-----------------------------------------------------------------------
      CHARACTER*6 ERRKEY
      PARAMETER (ERRKEY = 'IPPROG')
      CHARACTER*78 MESSAGE(10)    !Up to 10 lines of text to be output

      CHARACTER*1   RNMODE
      CHARACTER*6   HEAD
      CHARACTER     FILET*12, FILEIO*30
      CHARACTER*200 C200, AMPSAVE, CSAVE
      CHARACTER*5   PSTHD(6)
      CHARACTER*5   PID(200)

      INTEGER COUNT,COLNUM,ERRNUM
      INTEGER ISECT, I, J, L
      INTEGER LNUM, LUN, PCN
      INTEGER TR, TRTNO
      INTEGER MULTI, RUN
      INTEGER YRPLT, YRPST, TIMDIF, YRDIF, DAPP
      INTEGER YR, IPST
      INTEGER NPEST
      INTEGER PCOL(6), PNO(6), POBS(6)
      INTEGER IDAP(6,200)

      REAL CDATA
      REAL REP(6)
      REAL YPL(6,200)

      TYPE (ControlType) CONTROL
      MULTI  = CONTROL % MULTI
      RNMODE = CONTROL % RNMODE
      YRDIF  = CONTROL % YRDIF 
C-----------------------------------------------------------------------
C     SJR 3/16/04
C      Include FILEIO and RUN to allow use of older WARNING routine
C-----------------------------------------------------------------------
      FILEIO = CONTROL % FILEIO
      RUN    = CONTROL % RUN

C-----------------------------------------------------------------------
C     Initialize variables
C-----------------------------------------------------------------------
      IDAP = 0
      YPL  = 0.0

      PCOL = 0
      POBS = 0
      REP  = 1.
      PSTHD = '     '

      COUNT = 1
      LNUM = 0
C-----------------------------------------------------------------------
C     Find header line
C-----------------------------------------------------------------------
      CALL GETLUN('FINPUT', LUN)
!      INQUIRE (FILE=FILET, EXIST=STATUS)
!      INQUIRE (UNIT=LUN, OPENED=STATUS)

      OPEN(LUN, FILE=FILET, STATUS='OLD', IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILET,0)
! add check for FILET exists.  If not, set NPEST=0. chp

   10 READ(LUN,'(A)',IOSTAT = ERRNUM) C200
      LNUM = LNUM + 1
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILET,LNUM)
      IF (C200(1:1) .NE. '@') GO TO 10
C-----------------------------------------------------------------------
C     Read headers from file T and compare them to pest headers in pest
C     coefficient file. If a header matches a header in the coefficient
C     file, then record the data column number in the PCOL array.
C-----------------------------------------------------------------------
200   CONTINUE
      AMPSAVE = C200

      DO COLNUM = 1, 30
        READ(C200,220) HEAD
220   FORMAT(A6)
        IF (HEAD.EQ. '      ') GOTO 400
        HEAD = ADJUSTL(HEAD)  !Removes leading blanks in header
        DO J = 1, NPEST
        IF (INDEX(PID(J),TRIM(HEAD)) .GT. 0) THEN
        !DO K = 1,3
        !  IF(HEAD(K:K+3) .EQ. PID(J)) THEN
        PCOL(COUNT) = COLNUM
        PSTHD(COUNT) = HEAD
        PNO(COUNT) = J
        COUNT = COUNT + 1
        GO TO 300
        ENDIF
        !ENDDO
        ENDDO
300   C200 = C200(7:200)
      ENDDO
C-----------------------------------------------------------------------
C     Read in data for each column
C-----------------------------------------------------------------------
400   CONTINUE
      PCN = COUNT - 1

500   CONTINUE
      IF (PCN .GT. 0) THEN
      !Pest/disease headers are present
        CALL IGNORE2(LUN,LNUM,ISECT,C200)
        IF (ISECT.EQ.0) THEN
        GOTO 600
        ELSE IF (ISECT .EQ. 1) THEN       !Begin ISECT=1 if construct
C-----------------------------------------------------------------------
C    ISECT=1 Good line found
C-----------------------------------------------------------------------
        READ(C200,510,END=600,IOSTAT=ERRNUM) TR,YRPST
510     FORMAT(1X,I5,1X,I5)
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILET,LNUM)
        IF (TR .NE. TRTNO) GOTO 500
C-----------------------------------------------------------------
C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY
        !CALL Y2K_DOY(YRPST)
        CALL Y4K_DOY(YRPST,FILET,LNUM,ERRKEY,1)
        CALL YR_DOY(YRPST,YR,IPST)               !GH - Fix to handle
        YRPST = (YR + MULTI - 1) * 1000 + IPST   !GH - multiple years.

C-----------------------------------------------------------------------
C     Adjust for crop rotations
C-----------------------------------------------------------------------
        IF (RNMODE .EQ. 'Q') THEN
        YRPST = (YR + YRDIF) * 1000 + IPST
        ENDIF

C-----------------------------------------------------------------
        DAPP = MAX(0,TIMDIF(YRPLT,YRPST))
        IF (DAPP .EQ. 0) GOTO 500

        CSAVE = C200
        DO I = 1,PCN                  !Begin Pest do loop
        C200 = CSAVE
        L = (PCOL(I)-3)*6 + 13      !CHP - instead of loop, force
        C200 = C200(L:200)          !       C200 to PCOL
        READ(C200,'(1X,F5.0)',IOSTAT=ERRNUM) CDATA
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILET,LNUM)
        IF (CDATA .GE. 0.0) THEN    !Begin 'good data' if-constr.
        POBS(I) = POBS(I) + 1
!-----------------------------------------------------------------
!          Check for replicated values and average the values
!          with the previous data if there are replicated values
!-----------------------------------------------------------------
        IF (POBS(I) .GT. 1 .AND.
     &    DAPP .EQ. IDAP(I,POBS(I) - 1)) THEN
        REP(I) = REP(I) + 1.
        POBS(I) = POBS(I) - 1
        YPL(I,POBS(I)) =
     &    (YPL(I,POBS(I)) * (REP(I) - 1.) + CDATA) / REP(I)
C---------------------------------------------------------------
        ELSE
        YPL(I,POBS(I)) = CDATA
        IDAP(I,POBS(I)) = DAPP
        REP(I) = 1.
        ENDIF
        ENDIF                       !End good data if construct
        ENDDO                         !End PEST do loop

        ELSEIF (ISECT .EQ. 3 .AND. C200 .NE. AMPSAVE) THEN
        GOTO 200
        ENDIF                             !End ISECT=1 if construct

        GOTO 500
600   CONTINUE

      ELSE
      !Pest/disease headers are NOT present -- print warning
        WRITE(MESSAGE(1), 110) FILET 
110   FORMAT(' No valid pest types found in ', A12, '.')
C-----------------------------------------------------------------------
C     SJR 3/16/04 use older WARNING routine syntax
C      CHP changed routine in newer release versions
C-----------------------------------------------------------------------

        CALL WARNING(1, ERRKEY, MESSAGE)

      ENDIF

      CLOSE(LUN)
      RETURN
      END ! SUBROUTINE IPPROG

!----------------------------------------------------------------------
!     Variable Definitions
!-----------------------------------------------------------------------
! AMPSAVE   Contents of data record beginning with '@' 
! C200      Data record from input file 
! CDATA     Input value of pest damage 
! COLNUM    Counter for column number 
! COUNT     Number of pest columns found in FILET 
! DAPP      Date of observed pest damage (days after planting)
! ERRKEY    Subroutine name for error file 
! ERRNUM    Error number for input 
! FILET     Pest time series file 
! HEAD      Heading read from input file 
! IDAP(I,J) Day of pest damage for pest I, observation J
!             (days after planting)
! IPST      Day portion of YRPST date 
! ISECT     Indicator of completion of IGNORE routine: 0 - End of file 
!             encountered, 1 - Found a good line to read, 2 - End of 
!             Section in file encountered denoted by * in column 1.  
! LNUM      Current line number of input file 
! LUN       Logical unit for pest information 
! MULTI     Current simulation year (=1 for first or single simulation, 
!             =NYRS for last seasonal simulation) 
! NPEST     Number of pest or damage types in FILEP 
! PCN       Number of pests in FILET 
! PCOL(I)   Pest column 
! PID(I)    Pest identification header from FILEP (max 40) 
! PNO(I)    Pest number from File P for pest I 
! POBS(I)   Number of observations for pest I 
! PSTHD(I)  Column heading for each pest progress curve in FILET (max 6) 
! REP(I)    Count for replicated records for pest I 
! TIMDIF    Integer function which calculates the number of days between 
!             two Julian dates (da)
! TR        Treatment number 
! TRTNO     Treatment number being simulated (from FILEX) 
! YPL(I,J)  Array for storage of pest data for pest or damage type I, 
!             observation J 
! YR        Year portion of date 
! YR_DOY    Function subroutine converts date in YYDDD format to integer 
!             year (YY) and day (DDD). 
! YRPLT     Planting date (YYDDD)
! YRPST     Date of pest observation (YYDDD)
!----------------------------------------------------------------------
!     End Subroutine IPPROG
!-----------------------------------------------------------------------
