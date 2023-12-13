C=======================================================================
C  FOR_IPPARM, Subroutine
C----------------------------------------------------------------------
C This subroutine reads the pest damage coefficient datafile
C passed in by the fileP variable string.
C----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1994  WDB Written
C  02/25/1998  CHP Modified for PEST Module
C  01/12/1999  GH  Incorporated into CROPGRO
C  04/15/2002  GH  Modified number of pests to 100
C  05/09/2003  CHP Modified number of pests to 200
C-----------------------------------------------------------------------
C  Called by: PEST
C  Calls:     None
C=======================================================================
      SUBROUTINE FOR_IPPARM(
     &    FILEP, PATHPE,                                  !Input
     &    NPEST, PCPID, PCTID, PDCF1, PID)                !Output
C----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL GETLUN, IGNORE, ERROR
      SAVE

      CHARACTER*1   BLANK
      CHARACTER*2   NEWLIN
      CHARACTER*5   PID(200),PCPID(200,6)
      CHARACTER*6   ERRKEY
      CHARACTER*12  FILEP
      CHARACTER*20  PNAME(200)
      CHARACTER*80  PATHPE
      CHARACTER*92  FILEPP
      CHARACTER*200 C200

      INTEGER ERRNUM,PATHL
      INTEGER LN,LNUM,ISECT
      INTEGER CPN, LUN, NPEST
      INTEGER I, J
      INTEGER PCTID(200)

      REAL PDCF1(200,6)

      PARAMETER  (BLANK  = ' ')
      PARAMETER  (ERRKEY = 'IPPARM')

      I = INDEX(FILEP, '.PST')
      IF (I .LE. 0) RETURN

C----------------------------------------------------------------------
C     Initialize variables
C----------------------------------------------------------------------
      NPEST = 0
      LUN = 38
      LNUM = 0
      LN=0

      DO I = 1, 200
        PCTID(I) = 0
        DO J = 1, 6
        PCPID(I,J) = 'xxxxx'
        PDCF1(I,J) = 0.0
        ENDDO
      ENDDO
C----------------------------------------------------------------------
C     Open pest coefficient file
C----------------------------------------------------------------------
      PATHL  = INDEX(PATHPE,BLANK)
      IF (PATHL .LE. 1) THEN
        FILEPP = FILEP
      ELSE
        FILEPP = PATHPE(1:(PATHL-1)) // FILEP
      ENDIF

      CALL GETLUN('FILEP', LUN)
      OPEN (LUN, FILE = FILEPP, STATUS = 'OLD', IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEP,0)
C----------------------------------------------------------------------
C     Read each line of coefficient file, checking for comments
C     or blank lines using the subroutine IGNORE.
C----------------------------------------------------------------------
   10 CONTINUE
      CALL IGNORE(LUN,LNUM,ISECT,C200)
      IF (ISECT .EQ. 0) THEN
        CLOSE(LUN)
        RETURN
      ELSEIF (ISECT .EQ. 1 ) THEN
        READ(C200,40,IOSTAT=ERRNUM) NEWLIN
   40   FORMAT(A2)
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,1,FILEP,LNUM)
C----------------------------------------------------------------------
C     Read in first line of data for new pest
C----------------------------------------------------------------------
        IF (NEWLIN .NE.'  ') THEN
        READ(C200,50,IOSTAT=ERRNUM) LN,PID(LN),PNAME(LN),
     &    PCTID(LN),PCPID(LN,1),PDCF1(LN,1)
   50   FORMAT(I2,1X,A5,1X,A20,1X,I1,1X,A5,1X,F10.5)
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,1,FILEP,LNUM)
        NPEST = LN
        CPN = 1
C----------------------------------------------------------------------
C     Read in subsequent coupling point data
C----------------------------------------------------------------------
        ELSE
        CPN = CPN + 1
        READ(C200,60,IOSTAT=ERRNUM) PCPID(LN,CPN),PDCF1(LN,CPN)
   60     FORMAT(32X,A5,1X,F10.5)
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,1,FILEP,LNUM)
        ENDIF
      ENDIF
      GO TO 10
C----------------------------------------------------------------------
C     Completed reading pest coefficient file
C----------------------------------------------------------------------
      CLOSE(LUN)

      RETURN
      END  !SUBROUTINE IPPARM

C----------------------------------------------------------------------
C     Variable definitions
C----------------------------------------------------------------------
! BLANK      Blank character 
! C200       Data record from input file 
! CPN        Number of coupling points for current pest 
! ERRKEY     Subroutine name for error file 
! ERRNUM     Error number for input 
! FILEP      Filename for pest coefficient file 
! FILEPP     Path plus file name for pest information file 
! ISECT      Indicator of completion of IGNORE routine: 0 - End of file 
!              encountered, 1 - Found a good line to read, 2 - End of 
!              Section in file encountered denoted by * in column 1.  
! LN         Pest number 
! LNUM       Current line number of input file 
! LUN        Logical unit for pest information 
! NEWLIN     First two characters of current record 
! NPEST      Number of pest or damage types in FILEP 
! PATHL      Number of characters in path name (path plus filename for 
!              FILEC) 
! PATHPE     Path name for pest information file 
! PCPID(I,J) Pest coupling ponts identification code for pest I, coupling 
!              point J 
! PCTID(I)   Pest damage characterization method for pest I: (1) absolute 
!              daily damage rate, (2) percent observed damage, (3) daily 
!              percent damage rate, (4) absolute daily damage rate w/ 
!              preference and competition. 
! PDCF1(I,J) Pest damage coefficient asscociated with pest I, coupling 
!              point J 
! PID(I)     Pest identification header from FILEP 
! PNAME      Pest name 
C----------------------------------------------------------------------
C     End Subroutine IPPARM
C----------------------------------------------------------------------
