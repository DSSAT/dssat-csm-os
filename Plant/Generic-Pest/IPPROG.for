C======================================================================
C IPPROG, Subroutine
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
C 09/28/2004 CHP Fixed problem with reading multiple tiers of pest
C                  data from FILET.
C 05/07/2020 FO  Add new Y4K subroutine call to convert YRDOY
C-----------------------------------------------------------------------
C  Called by: PEST
C  Calls:     None
C=======================================================================
      SUBROUTINE IPPROG(CONTROL, 
     &    FILET, PATHEX, PID, YRPLT, TRTNUM,              !Input
     &    IDAP, PCN, PNO, POBS, PSTHD, YPL)               !Output
C----------------------------------------------------------------------
C This subroutine reads pest observations from the time series file T
C for the experiment being simulated.
C----------------------------------------------------------------------
      USE MODULEDEFS
      IMPLICIT NONE
      EXTERNAL ERROR, GETLUN, IGNORE2, LenString, TIMDIF, Y2K_DOY, 
     &  Y4K_DOY, YR_DOY, WARNING, PARSE_HEADERS
C-----------------------------------------------------------------------
      
      CHARACTER*6 ERRKEY
      PARAMETER (ERRKEY = 'IPPROG')
      CHARACTER*78 MSG(10)    !Up to 10 lines of text to be output
      CHARACTER*80  PATHEX
      CHARACTER*92 T_PATH_FILE

      CHARACTER*1   RNMODE
      CHARACTER*6   HEAD
      CHARACTER     FILET*12
      CHARACTER*200 C200, AMPSAVE, CSAVE
      CHARACTER*5   PSTHD(6)
      CHARACTER*5   PID(MAXPEST)

      INTEGER COUNT,COLNUM,ERRNUM
      INTEGER ISECT, I, J, LenString
      INTEGER LNUM, LUN, PCN
      INTEGER TR, TRTNUM
      INTEGER MULTI
      INTEGER YRPLT, YRPST, TIMDIF, YRDIF, DAPP
      INTEGER YR, IPST
      INTEGER PCOL(6), PNO(6), POBS(6)
      INTEGER IDAP(6,MAXPEST)

      REAL CDATA
      REAL REP(6)
      REAL YPL(6,MAXPEST)

      INTEGER, PARAMETER :: MAXCOL = 50
      CHARACTER*15  HEADER(MAXCOL)
      INTEGER COL(MAXCOL,2), C1, C2, COUNT_F, C1D(6), C2D(6), COLN
      TYPE (ControlType) CONTROL
      MULTI  = CONTROL % MULTI
      RNMODE = CONTROL % RNMODE
      YRDIF  = CONTROL % YRDIF 

      C200 = ' '
C-----------------------------------------------------------------------
C     Initialize variables
C-----------------------------------------------------------------------
      IDAP = 0
      YPL  = 0.0

      PCOL = 0
      POBS = 0
      REP  = 1.
      PSTHD = '     '

      LNUM = 0
      PCN = 0
C-----------------------------------------------------------------------
C     Find header line
C-----------------------------------------------------------------------
      CALL GETLUN('FINPUT', LUN)
      IF (LenString(PATHEX) < 1) THEN
        T_PATH_FILE = FILET
      ELSE
!       T_PATH_FILE = TRIM(PATHEX) // SLASH // TRIM(FILET)
        T_PATH_FILE = TRIM(PATHEX) // TRIM(FILET)
      ENDIF
      OPEN(LUN, FILE=T_PATH_FILE, STATUS='OLD', IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,T_PATH_FILE,0)

!     Find a header line
  100 DO WHILE (C200(1:1) .NE. '@')
        READ(LUN,'(A)',IOSTAT = ERRNUM, END=1000) C200
        LNUM = LNUM + 1
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,T_PATH_FILE,LNUM)
      ENDDO
C-----------------------------------------------------------------------
C     Read headers from file T and compare them to pest headers in pest
C     coefficient file. If a header matches a header in the coefficient
C     file, then record the data column number in the PCOL array.
C-----------------------------------------------------------------------
  200 CONTINUE
      AMPSAVE = C200
      COUNT = 0

!     TF 05/22/2023 - Updated fileT read method to handle  
!      dates in YYDDD and YYYYDDD format
      CALL PARSE_HEADERS(C200, MAXCOL, HEADER, COUNT_F, COL)
!     Max 30 columns per tier of data
      DO COLNUM = 1, COUNT_F
        C1 = COL(COLNUM,1)
        C2 = COL(COLNUM,2)
        HEAD = TRIM(HEADER(COLNUM))  !Removes leading blanks in header
        DO J = 1, MAXPEST
          IF (INDEX(PID(J),TRIM(HEAD)) .GT. 0) THEN
            PCN = PCN + 1         !Total pest count
            COUNT = COUNT + 1     !Pest count this tier
            PCOL(COUNT) = COLNUM  !Column # this tier 
            PSTHD(PCN) = HEAD     !pest header
            PNO(PCN) = J          !pest number from pest file
!           save the position of the pest-related columns
            C1D(PCN) = C1 !beginning
            C2D(PCN) = C2 !ending
            EXIT
          ENDIF
        ENDDO
        C200 = C200(7:200)
      ENDDO
C-----------------------------------------------------------------------
C     Read in data for each column
C-----------------------------------------------------------------------
      CONTINUE

  500 CONTINUE
      IF (COUNT .GT. 0) THEN
!       Pest/disease headers are present in this tier of data
        CALL IGNORE2(LUN,LNUM,ISECT,C200)

C-----------------------------------------------------------------------
        IF (ISECT .EQ. 0) THEN
C         ISECT=0 End of file
          GO TO 1000
C-----------------------------------------------------------------------

        ELSE IF (ISECT .EQ. 1) THEN       
C-----------------------------------------------------------------------
C    ISECT=1 Good line found
C-----------------------------------------------------------------------
          DO COLN = 1, COUNT_F
            C1 = COL(COLN,1)
            C2 = COL(COLN,2)
            SELECT CASE (TRIM(HEADER(COLN)))
              CASE('TRNO');READ(C200(C1:C2+1),*,IOSTAT=ERRNUM) TR
              CASE('DATE');READ(C200(C1:C2),*,IOSTAT=ERRNUM) YRPST
            END SELECT
          END DO 
!          READ(C200,510,END=1000,IOSTAT=ERRNUM) TR,YRPST
!  510     FORMAT(1X,I5,1X,I5)
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,T_PATH_FILE,LNUM)
          IF (TR .NE. TRTNUM) GOTO 500
C-----------------------------------------------------------------
C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY
          !CALL Y2K_DOY(YRPST)
          CALL Y4K_DOY(YRPST,T_PATH_FILE,LNUM,ERRKEY,1)
          CALL YR_DOY(YRPST,YR,IPST)               !GH - Fix to handle
          YRPST = (YR + MULTI - 1) * 1000 + IPST   !GH - multiple yrs

C-----------------------------------------------------------------------
C     Adjust for crop rotations
C-----------------------------------------------------------------------
          IF (RNMODE .EQ. 'Q') THEN
            YRPST = (YR + YRDIF) * 1000 + IPST
          ENDIF

C---------------------------------------------------------------
          DAPP = MAX(0,TIMDIF(YRPLT,YRPST))
          IF (DAPP .EQ. 0) GOTO 500

          CSAVE = C200
          DO J = 1, COUNT               !Begin Pest do loop, this tier
            I = PCN - COUNT + J         !counter for this tier
            C200 = CSAVE
            C200 = C200(C1D(J)-1:C2D(J))          !       C200 to PCOL
            READ(C200,'(1X,F5.0)',IOSTAT=ERRNUM) CDATA
            IF (ERRNUM .NE. 0)CALL ERROR(ERRKEY,ERRNUM,T_PATH_FILE,LNUM)
            IF (CDATA .GE. 0.0) THEN    !Begin 'good data' if-constr.
              POBS(I) = POBS(I) + 1
!-----------------------------------------------------------------
!          Check for replicated values and average the values
!          with the previous data if there are replicated values
!-----------------------------------------------------------------
! This code re-written for Intel compiler -- can't handle double
!     IF clause.
!              IF (POBS(I) .GT. 1 .AND.
!     &              DAPP .EQ. IDAP(I,POBS(I) - 1)) THEN
!                REP(I) = REP(I) + 1.
!                POBS(I) = POBS(I) - 1
!                YPL(I,POBS(I)) =
!     &            (YPL(I,POBS(I)) * (REP(I) - 1.) + CDATA) / REP(I)
!              ELSE
!                YPL(I,POBS(I)) = CDATA
!                IDAP(I,POBS(I)) = DAPP
!                REP(I) = 1.
!              ENDIF
!C---------------------------------------------------------------
              IF (POBS(I) .GT. 1) THEN
                IF (DAPP .EQ. IDAP(I,POBS(I) - 1)) THEN
                REP(I) = REP(I) + 1.
                POBS(I) = POBS(I) - 1
                YPL(I,POBS(I)) =
     &            (YPL(I,POBS(I)) * (REP(I) - 1.) + CDATA) / REP(I)
                ELSE
                  YPL(I,POBS(I)) = CDATA
                  IDAP(I,POBS(I)) = DAPP
                  REP(I) = 1.
                ENDIF
C---------------------------------------------------------------
              ELSE
                YPL(I,POBS(I)) = CDATA
                IDAP(I,POBS(I)) = DAPP
                REP(I) = 1.
              ENDIF
            ENDIF                       !End good data if construct
          ENDDO                         !End PEST do loop
          GOTO 500

C-----------------------------------------------------------------------
        ELSEIF (ISECT .EQ. 3 .AND. C200 .NE. AMPSAVE) THEN
C         ISECT=3 New data tier found and it is not a repeated header
          GOTO 200
C-----------------------------------------------------------------------
        ENDIF                             !End ISECT=1 if construct

      ELSE
!       No pests in this tier of data, check for another tier of data
        GOTO 100
      ENDIF

!     End of file, check that a pest header was found
 1000 CONTINUE
      IF (PCN .LE. 0) THEN
        !No Pest/disease headers found -- print warning
        MSG(1)="No valid pest types found in T file:"
        WRITE(MSG(2), '(A)') TRIM(T_PATH_FILE) 
        CALL WARNING(2, ERRKEY, MSG)
      ENDIF

      CLOSE(LUN)
      RETURN
      END SUBROUTINE IPPROG

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
! TRTNUM    Treatment number being simulated (from FILEX) 
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
