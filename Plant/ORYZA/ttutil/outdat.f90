module Module_OutDat
implicit none

private

integer, parameter :: NAMES_MXN      = 500
integer, parameter :: VARL_M         = 36
integer, parameter :: NAMES_IB_MXN_4 = 8 
integer, parameter :: NAMES_IB_MXN_5 = 100 
integer, parameter :: NAMES_IB_MXN_7 = 8 
integer, parameter :: NAMES_IB_MXN_8 = NAMES_IB_MXN_5 
integer, parameter :: NAMES_IB_MXN_9 = NAMES_IB_MXN_5 

type TMemBin
  character*1        :: RunType
  integer            :: InfoID
  character*(VARL_M) :: VarName
  real               :: VarValue
end type TMemBin

type(TMemBin), allocatable, dimension(:) :: MemBin
integer                                  :: MemBinHigh = 0

logical :: OutDat_UseMemory = .FALSE.

!make only public what is needed
public :: OutDat_UseMemory
public :: OUTDAT
public :: OUTARR

contains

subroutine MemWrite(RecNo, RunType, InfoID, VarName, VarValue)
  implicit none

! formal parameters
  integer, intent(in)            :: RecNo
  integer, intent(in)            :: InfoID
  character*1, intent(in)        :: RunType
  character*(VARL_M), intent(in) :: VarName
  real, intent(in)               :: VarValue

! local variables
  integer, parameter :: cAllocAmount = 50000 ! amount by which MemBin is increased when it is too small

  type(TMemBin), allocatable, dimension(:) :: MemBinSave

  save

! see if memory needs to be increased
  do while (Recno > MemBinHigh)
    MembinHigh = MemBinHigh + cAllocAmount

    if (Size(MemBin) > 0) then 
      ! something already in the array, increase but retain contents
      allocate(MemBinSave(MemBinHigh))
      MemBinSave(1:(MemBinHigh - cAllocAmount)) = MemBin
      call move_alloc(MemBinSave, MemBin)
    else
      allocate(MemBin(MemBinHigh))
    end if
  end do

  MemBin(Recno)%RunType  = RunType
  MemBin(Recno)%InfoID   = InfoID
  MemBin(Recno)%VarName  = VarName
  MemBin(Recno)%VarValue = VarValue 
end subroutine

subroutine MemRead(RecNo, RunType, InfoID, VarName, VarValue, EOM)
  implicit none

! formal parameters
  integer, intent(in)             :: RecNo
  integer, intent(out)            :: InfoID
  character*1, intent(out)        :: RunType
  character*(VARL_M), intent(out) :: VarName
  real, intent(inout)             :: VarValue
  integer, intent(out)            :: EOM ! ' end of memory :-)'

  save

  if ((RecNo >= 1) .and. (RecNo <= MemBinHigh)) then
    RunType  = MemBin(Recno)%RunType   
    InfoID   = MemBin(Recno)%InfoID
    VarName  = MemBin(Recno)%VarName
    VarValue = MemBin(Recno)%VarValue
    EOM      = 0
  else
    EOM      = -1
  end if
end subroutine

subroutine MemDeallocate
  Deallocate(MemBin)
end subroutine

logical function MemAllocated()
  MemAllocated = Allocated(MemBin)
end function

SUBROUTINE OUTDAT (ITASK, IUNIT, RN, R)
  IMPLICIT NONE

! FORMAL_PARAMETERS:
  INTEGER, intent(in)           :: ITASK, IUNIT
  REAL, intent(in)              :: R
  CHARACTER(LEN=*), intent(in)  :: RN

! local variables
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!                                                                     *
!     A D J U S T A B L E   P A R A M E T E R S                       *
!     =========================================                       *
!     IMNC1  = maximum number of columns of dependent variables in an *
!              output block, if increased, also increase LINE*x, x    *
!              should be at least 14+IMNC1*13,                        *
!     NAMES_MXN = maximum number of names of dependent variables,     *
!                 can be increased without problems,                  *
!     VARL_M = maximum length of a variable, remainder is removed     *
!     Warning: do not change the maximum length of names of variables,*
!              currently set to 11 !!                                 *
!                                                                     *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

! AVN    - Array of variable names
! IAVNL  - Array of lengths of AVN
! AVV    - Array with values of AVN
! ASELN  - Array with selected variable names
! SEL    - Array with flags whether variables from AVN should be
!          printed
! FND    - Array with counts how many times a variable is seen at
!          itask=2
! MGIVEN - Array of flags whether messages about repeated input
!          of a variable is given
! BLK    - Assigned block number, 0 if not assigned
! COL_WIDTH_MNN
!        - maximum width of a Y column, excluding spaces
! SEQ    - SEQ(1) = element of AVN that is first in
!                   current block,
!          SEQ(2) = element of AVN that is second in
!                   current block etc.
! EXTRA_SP - Extra width of columns
! EXTRA_SPX- Extra width for X columns
! NAMES_IB_MXN_4 - Maximum number of names in block in itask=4
!                  (determined by paper width)
! NAMES_IB_MXN_5 - Maximum number of names in block in itask=5
! NAMES_IB_MXN_7 - Maximum number of names in block in itask=7
! NAMES_IB_MXN_8 - Maximum number of names in block in itask=8
! NAMES_IB_MXN_9 - Maximum number of names in block in itask=9

! SEQ2     - Array with numbers that link to selected arrays

! Maximum length of a variable name is 36 characters (31 for name,
! 5 for possible index e.g. '(134)')

  CHARACTER*(VARL_M) AVN(NAMES_MXN)
  INTEGER            IAVNL(NAMES_MXN)
  CHARACTER*(VARL_M) ASELN(NAMES_MXN)
  INTEGER            BLK(NAMES_MXN)
  LOGICAL            MGIVEN(NAMES_MXN)
  INTEGER            SEQ2(NAMES_MXN)
  INTEGER            FND(NAMES_MXN)
  CHARACTER*(VARL_M) LXN, LN
  REAL               AVV(NAMES_IB_MXN_5)
  LOGICAL            FNDA(NAMES_IB_MXN_5)
  INTEGER            SEQ(NAMES_IB_MXN_5)
  INTEGER            EXTRA_SP(NAMES_IB_MXN_5),EXTRA_SPX

  INTEGER COL_WIDTH_MNN,LINE_LEN,CENTRE
  INTEGER LINE_LEN_4
  INTEGER LINE_LEN_5
  INTEGER LINE_LEN_7
  INTEGER LINE_LEN_8
  INTEGER LINE_LEN_9

  PARAMETER (COL_WIDTH_MNN=12,CENTRE=8)
  PARAMETER (LINE_LEN_4=COL_WIDTH_MNN+2+NAMES_IB_MXN_4*(COL_WIDTH_MNN+1))
  PARAMETER (LINE_LEN_5=COL_WIDTH_MNN+2+NAMES_IB_MXN_5*(COL_WIDTH_MNN+1))
  PARAMETER (LINE_LEN_7=COL_WIDTH_MNN+2+NAMES_IB_MXN_7*(COL_WIDTH_MNN+1))
  PARAMETER (LINE_LEN_8=LINE_LEN_5)
  PARAMETER (LINE_LEN_9=LINE_LEN_5)

! DECLARATION OF LONGEST LINE, OTHERS SHOULD FIT INTO THIS ONE
  CHARACTER*(LINE_LEN_5) LINE

! NUMBER OF DIFFERENT VARIABLE NAMES THAT HAVE BEEN FOUND IN A
! PARTICULAR RUN
  INTEGER IFND

! IFND2 IS THE TRUE NUMBER OF VARIABLES TO BE PRINTED IN A
! PARTICULAR BLOCK
  INTEGER IFND2

! FLAG WHETHER BLOCK IS FULL WITH COLUMNS
  LOGICAL FULL_BLOCK

! LENGTH OF LINE
  INTEGER LINE_L

! FLAG WHETHER EXTRA SPACES BETWEEN COLUMNS ARE NECESSARY
  LOGICAL EXTRA_F

! OTHER LOCAL PARAMETERS
  INTEGER ILU1, ILU2, ICHECK, IOS, INSEL, ISEL, ISAVE
  INTEGER IRUN1, IRUN2, IRUN3, IRUN4, IRUN5
  INTEGER ITOLD, ILTASK, INAME, IBLOK, ILXN, IR
  INTEGER ISREC, IREC, IEREC, IB, I1, I2, I3, I4, I5, I6, IFINDC
  REAL LV, LVO
  CHARACTER CHR*1, RUNTYP*1, RUNDUM*1, TEXT(4:9)*18, COMMCHR*1
  LOGICAL OPEND, OK, YFND, RECOVR, SELECTED, FIRST8, FIRST9
  INTEGER UNLOG
  LOGICAL TOSCR, TOLOG
  CHARACTER*80 SPACE

  CHARACTER*(VARL_M) :: DumVarName
  REAL               :: DumVarValue
  INTEGER            :: DumIOSValue

! UNCOMMENT FOR MAC ABSOFT FORTRAN COMPILATION
! CHARACTER EOFCHR*1

  SAVE

! UNCOMMENT FOR MAC ABSOFT FORTRAN COMPILATION
! DATA EOFCHR /'Z'/

  DATA TEXT /'TABLE OUTPUT','SPREADSHEET OUTPUT', &
             '2 COLUMN OUTPUT','ICASA OUTPUT',    &
             'END OF RUN OUTPUT','GREENERY OUTPUT'/

  DATA ITOLD /-1/, IRUN1 /0/, IRUN2 /0/, INSEL /0/, RECOVR /.FALSE./
  DATA FIRST8 /.TRUE./
  DATA FIRST9 /.TRUE./

  IF (ITASK == 1) THEN

!    UNIT NUMBER AND STATUS CHECK:
!    =============================
!    UNIT NUMBER MUST BE > 0 AT FIRST CALL, MAY BE ZERO OR EQUAL
!    TO VALUE AT FIRST CALL

     IF (ITOLD == -1) THEN

!       DESIRED MESSAGE OUTPUT
        CALL MESSINQ (TOSCR, TOLOG, UNLOG)

!       OUTDAT WRITES MESSAGES TO ITS FORMATTED OUTPUT FILE
!       AND NO ADDITIONAL LOGFILE OUTPUT IS GENERATED.
!       SO MESSAGES TO SCREEN REQUIRE TOSCR TO TBE TRUE
!       TOLOG AND UNLOG ARE IGNORED.

        IF (IUNIT == 0) CALL FATALERR ('OUTDAT','NO UNIT NUMBER SUPPLIED')
        ILU2 = IUNIT

     ELSE

        IF (IUNIT /= 0.AND.IUNIT /= ILU2) CALL FATALERR &
           ('OUTDAT','CHANGE OF UNIT NUMBER NOT ALLOWED')

        IF (ITOLD == 1) THEN

!          REPEATED INITIALIZATION IS TAKING PLACE

           IF (TOSCR) WRITE (*,'(A)') &
             ' WARNING FROM OUTDAT: IGNORING REPEATED INITIALIZATION'
           WRITE (ILU2,'(A)') &
             ' WARNING FROM OUTDAT: IGNORING REPEATED INITIALIZATION'
           RETURN

        ELSE IF (ITOLD == 2) THEN

           CONTINUE

        ELSE IF (ITOLD == 3) THEN

!          DURING A PREVIOUS CALL, ONE OR MORE VARIABLES WERE
!          SELECTED, THIS SELECTION IS DISCARDED AFTER
!          INITIALIZATION

           IF (TOSCR) WRITE (*,'(A)') &
             ' WARNING FROM OUTDAT: SELECTED VARIABLES DISCARDED'
           WRITE (ILU2,'(A)') &
             ' WARNING FROM OUTDAT: SELECTED VARIABLES DISCARDED'

        ELSE IF (ITOLD >= 4) THEN

           IF (RECOVR) CALL FATALERR ('OUTDAT', &
              'NORMAL OUTDAT CALL IMPOSSIBLE AFTER RECOVERY')

        END IF
     END IF

!    MAKE UNIT NUMBER FOR TEMPORARY FILE LOCAL AND MAKE UNIT NUMBER
!    AVAILABLE ALSO FOR OUTPLT
     ILU1 = ILU2+1
     CALL AMBUSY (1, 'OUTDAT', ILU2)

!    OPEN FILE CHECK:
!    SEE IF UNITS ILU2 AND ILU1 ARE OPEN. IF NOT OPEN HERE

     INQUIRE (UNIT=ILU2, OPENED=OPEND)
     IF (.NOT.OPEND) CALL FOPENG (ILU2, 'RES.DAT', 'NEW', 'SF', 0, 'DEL')

!    SEE IF UNIT ILU1 IS OPEN, IF OPEN USE IT, IF NOT, OPEN USING
!    DEFAULT OUTPUT FILE NAME
     if (.NOT. OutDat_UseMemory) then
        INQUIRE (UNIT=ILU1, OPENED=OPEND)
        IF (.NOT.OPEND) THEN
!          FOR NORMAL COMPILERS
           CALL FOPENG (ILU1, 'RES.BIN', 'NEW', 'UD', 48, 'DEL')

!          RESET NUMBER OF RUNS IN RES.BIN FILE, FIRST RECORD TO
!          WRITE TO AND START RECORD OF SET
           IRUN4 = 0
           IREC  = 1
           ISREC = 0
        ELSE
!          TEMPORARY FILE IS OPEN
           IF (ITOLD == -1) CALL FATALERR ('OUTDAT', &
           'TEMPORARY FILE MAY NOT BE OPENED OUTSIDE OUTDAT')
        END IF
     else
        OPEND = MemAllocated()

        IF (.NOT.OPEND) THEN
!          RESET NUMBER OF RUNS IN RES.BIN FILE, FIRST RECORD TO
!          WRITE TO AND START RECORD OF SET
           IRUN4 = 0
           IREC  = 1
           ISREC = 0
        ELSE
           IF (ITOLD == -1) CALL FATALERR ('OUTDAT', &
           'TEMPORARY FILE MAY NOT BE OPENED OUTSIDE OUTDAT')
        END IF
     end if

!    INITIALIZE ROUTINE THAT WRITES COMMENT LINES TO OUTPUT FILE
     CALL OUTCOM ('<INIT$$$>')

!    RESET ARRAYS WITH NAMES
     DO I1=1,NAMES_MXN
        AVN(I1)    = ' '
        FND(I1)    = 0
        MGIVEN(I1) = .FALSE.
     END DO

!    FIND OUT IF INITIALIZATION IF GENERATED BY RERUNS
     CALL AMBUSY (2, 'RDFROM', IRUN2)
     IF (IRUN2 == 0) THEN
!       RUN NUMBER NOT OBTAINED FROM RDFROM OR FIRST RUN
        IRUN3  = IRUN1
        RUNTYP = 'N'
     ELSE
!       RUN NUMBER OBTAINED FROM RDFROM
        IRUN3  = IRUN2
        RUNTYP = 'R'
     END IF

!    INCREASE RUN NUMBER AND NUMBER OF RUNS IN FILE
     IRUN1 = IRUN1+1
     IRUN4 = IRUN4+1

!    WRITE TOTAL NUMBER OF RUNS IN RES.BIN FILE TO RES.BIN FILE
     if (.NOT. OutDat_UseMemory) then
        WRITE (ILU1,REC=1) '-', IRUN4, '....................................', 0.
     else
        call MemWrite(1, '-', IRUN4, '....................................', 0.)
     end if

!    UPDATE POINTER RECORD FROM PREVIOUS SET ONLY IF IT IS NOT
!    THE FIRST INITIALIZATION TO THE SAME RES.BIN FILE

     IF (IRUN4 >= 2) then
        if (.NOT. OutDat_UseMemory) then
           WRITE (ILU1,REC=ISREC) ' ', IREC, '....................................', 0.
        else
           call MemWrite(ISREC, ' ', IREC, '....................................', 0.)
        end if
     END IF
     IREC  = IREC+1
     ISREC = IREC

!    CHECK ON LENGTH OF NAME
     IF (LEN (RN) <= VARL_M) THEN
!       LENGTH OF NAME IS OK
        LXN = RN
     ELSE
!       LENGTH OF NAME IS NOT OK
        I1 = LEN_TRIM (RN)
        IF (TOSCR) WRITE (*,'(2A,/,2A)') &
          ' WARNING FROM OUTDAT: VARIABLE ',RN(1:I1), &
          ' IS TOO LONG FOR OUTDAT, IT IS TRUNCATED TO: ', &
            RN(1:VARL_M)
        WRITE (ILU2,'(2A,/,2A)') &
          ' WARNING FROM OUTDAT: VARIABLE ',RN(1:I1), &
          ' IS TOO LONG FOR OUTDAT, IT IS TRUNCATED TO: ', &
            RN(1:VARL_M)
        LXN = RN(1:VARL_M)
     END IF

     CALL UPPERC (LXN)
     ILXN  = LEN_TRIM (LXN)

     if (.NOT. OutDat_UseMemory) then
        WRITE (ILU1, REC=IREC) ' ', -99, '....................................', 0.
     else
        call MemWrite(IREC, ' ', -99, '....................................', 0.)
     end if

     IREC  = IREC+1

     if (.NOT. OutDat_UseMemory) then
        WRITE (ILU1, REC=IREC) RUNTYP, IRUN3, LXN, R
     else
        call MemWrite(IREC, RUNTYP, IRUN3, LXN, R)
     end if

!    UNCOMMENT FOR MAC ABSOFT FORTRAN COMPILATION
!    WRITE (ILU1, REC=IREC+1) EOFCHR, 0, '....................................', 0.

     IFND  = 0
     INSEL = 0
     ISAVE = 0
     SPACE = ' '

  ELSE IF (ITASK == 2) THEN

!    DUMP VARIABLE TO FILE

!    CHECK STATUS FIRST
     IF (ITOLD >= 3) CALL FATALERR ('OUTDAT', 'INITIALIZATION NOT DONE')

!    CHECK ON LENGTH OF NAME
     IF (LEN (RN) <= VARL_M) THEN
!       LENGTH OF NAME IS OK
        LN = RN
     ELSE
!       LENGTH OF NAME IS NOT OK
        I1 = LEN_TRIM (RN)
        IF (TOSCR) WRITE (*,'(2A,/,2A)') &
          ' WARNING FROM OUTDAT: VARIABLE ',RN(1:I1), &
          ' IS TOO LONG FOR OUTDAT, IT IS TRUNCATED TO: ', &
            RN(1:VARL_M)
        WRITE (ILU2,'(2A,/,2A)') &
          ' WARNING FROM OUTDAT: VARIABLE ',RN(1:I1), &
          ' IS TOO LONG FOR OUTDAT, IT IS TRUNCATED TO: ', &
            RN(1:VARL_M)
        LN = RN(1:VARL_M)
     END IF

!    VALUES AND NAMES ARE WRITTEN TO FILE

     CALL UPPERC (LN)

     OK    = .TRUE.
     INAME = 0

     IF (LN == LXN) THEN
        DO I1=1,IFND
           FND(I1) = 0
        END DO
        ISAVE = 0
     ELSE
!       VARIABLE IS NOT THE INDEPENDENT VARIABLE, LOOK IN LIST

!       INCREASE NEW TRY POINTER VALUE, IF AT END OF LIST RESET
        I1 = ISAVE+1
        IF (I1 > IFND) I1 = 1

        IF (I1 > ISAVE) THEN
!          SEARCH TO END OF LIST FROM POSITION OF PREVIOUS CALL
           DO WHILE (I1 <= IFND.AND.INAME == 0)
              IF (LN == AVN(I1)) INAME = I1
              I1 = I1+1
           END DO
        END IF

        IF (INAME == 0) THEN
!          MATCH NOT FOUND TO END OF LIST, TRY BEGINNING
           I1 = 1
           DO WHILE (I1 <= ISAVE.AND.INAME == 0)
              IF (LN == AVN(I1)) INAME = I1
              I1 = I1+1
           END DO
        END IF

        IF (INAME == 0) THEN
!          NAME NOT FOUND IN LIST, ADD TO LIST
           IFND = IFND+1
           IF (IFND > NAMES_MXN) CALL FATALERR ('OUTDAT', 'TOO MANY VARIABLES FOR OUTPUT')
           AVN(IFND)  = LN
           INAME      = IFND
           FND(INAME) = 1
        ELSE
!          NAME FOUND IN LIST
           FND(INAME) = FND(INAME)+1
           IF (FND(INAME) == 2) THEN
!             VARIABLE SUPPLIED TWICE, PREVENT WRITING TO FILE
              OK = .FALSE.
              IF (.NOT.MGIVEN(INAME)) THEN
                 MGIVEN(INAME) = .TRUE.
!                GIVE WARNING THE SECOND TIME A VARIABLE IS ENTERED
!                DO NOT GIVE WARNINGS ON THIRD, FOURTH ETC.
                 I1 = LEN_TRIM (LN)
                 IF (TOSCR) WRITE (*,'(4A)') &
                   ' WARNING FROM OUTDAT: VARIABLE ',LN(1:I1), &
                   ' SUPPLIED WITHOUT NEW INDEPENDENT ', &
                     LXN(1:ILXN)
                 WRITE (ILU2,'(4A)') &
                   ' WARNING FROM OUTDAT: VARIABLE ',LN(1:I1), &
                   ' SUPPLIED WITHOUT NEW INDEPENDENT ', &
                     LXN(1:ILXN)
              END IF
           END IF
        END IF
     END IF

     IF (OK) THEN
        IREC = IREC+1

        if (.NOT. OutDat_UseMemory) then
           WRITE (ILU1,REC=IREC) ' ', INAME, LN, R
        else
           call MemWrite(IREC, ' ', INAME, LN, R)
        end if

!       UNCOMMENT FOR MAC ABSOFT FORTRAN COMPILATION
!       WRITE (ILU1,REC=IREC+1) EOFCHR, INAME, LN, R
     END IF

     ISAVE = INAME

  ELSE IF (ITASK == 3) THEN

!    ---------------------------------------
!    SELECTION OF OUTPUT VARIABLES FOR TABLE
!    ---------------------------------------

     IF (ITOLD == 1.OR.ITOLD == 99) THEN
        IF (TOSCR) WRITE (*,'(A)') &
 &        ' WARNING FROM OUTDAT: WRONG TASK PRECEEDS SELECTION OF OUTPUT'
        WRITE (ILU2,'(A)') &
          ' WARNING FROM OUTDAT: WRONG TASK PRECEEDS SELECTION OF OUTPUT'
        RETURN
     ELSE IF (ITOLD == 2.OR.(ITOLD >= 4.AND.ITOLD <= 16)) THEN
!       FIRST TIME
        INSEL = 0
     END IF

     LN = RN
     CALL UPPERC (LN)

!    LOOKUP NAME IN LIST
     IF (INSEL == 0) THEN
        I1 = 0
     ELSE
        I1 = IFINDC (ASELN,INSEL,1,INSEL,LN)
     END IF

     IF (I1 == 0) THEN
!       NAME NOT FOUND IN LIST, ADD
        INSEL = INSEL+1
        IF (INSEL > NAMES_MXN) CALL FATALERR &
           ('OUTDAT', 'TOO MANY VARIABLES SELECTED')
        ASELN(INSEL) = LN
     END IF

  ELSE IF ((ITASK >= 4 .AND.ITASK <= 9).OR. &
           (ITASK >= 14.AND.ITASK <= 19)) THEN

!    --------------------------
!    WRITING OF OUTPUT TABLE(S)
!    --------------------------

!    ERROR CHECKS ON OLD TASK
     IF (ITOLD == -1) THEN
!       RECOVERY OF OUTPUT FILE, OPEN RES.BIN FILE
        TOSCR  = .TRUE.
        RECOVR = .TRUE.
        IF (IUNIT <= 10) CALL FATALERR ('OUTDAT', 'ILLEGAL UNIT NUMBER')
        ILU2 = IUNIT
        ILU1 = ILU2+1
        if (.NOT. OutDat_UseMemory) then
           CALL FOPENG (ILU1, 'RES.BIN', 'OLD', 'UD', 48, ' ')
        else
           CALL FATALERR('OUTDAT', 'RECOVERY IMPOSSIBLE WITH MEMORY STORAGE')
        end if

!       CHECK IF OUTPUT FILE IS OPEN, MAKE NEW ONE IF IT IS NOT

        INQUIRE (UNIT=ILU2, OPENED=OPEND)
        IF (.NOT.OPEND) CALL FOPENG (ILU2,'RECOVER.DAT','NEW','SF',0,'DEL')

!       FIND OUT IF END RECORD OF LAST SET IS IN THE FILE
        if (.NOT. OutDat_UseMemory) then
           READ (ILU1, REC=1) RUNTYP, IRUN5
        else
           CALL FATALERR('OUTDAT', 'RECOVERY IMPOSSIBLE WITH MEMORY STORAGE')
        end if

        IF (RUNTYP == '-') THEN
!          WRITE END RECORD OF LAST SET IN THE FILE

!          GO TO START OF LAST SET FIRST
           IEREC = 1
           DO I1=1,IRUN5-1
              ISREC = IEREC+1
              if (.NOT. OutDat_UseMemory) then
                 READ (ILU1, REC=ISREC) RUNDUM, IEREC
              else
                 CALL FATALERR('OUTDAT', 'RECOVERY IMPOSSIBLE WITH MEMORY STORAGE')
              end if
           END DO

!          READ REST UNTIL END_OF_FILE
           IOS = 0
           IR = IEREC+2
           DO WHILE (IOS == 0)
              if (.NOT. OutDat_UseMemory) then
                 READ (ILU1, REC=IR, IOSTAT=IOS) RUNDUM
              else
                 CALL FATALERR('OUTDAT', 'RECOVERY IMPOSSIBLE WITH MEMORY STORAGE')
              end if

!             UNCOMMENT FOR MAC ABSOFT FORTRAN COMPILATION
!             IF (RUNDUM == EOFCHR) IOS = -1

              IF (IOS == 0) IR = IR+1

           END DO

           if (.NOT. OutDat_UseMemory) then
              WRITE (ILU1, REC=IEREC+1) ' ', IR-1 , '....................................', 0.
              WRITE (ILU1, REC=1)       '+', IRUN5, '....................................', 0.
           else
              CALL FATALERR('OUTDAT', 'RECOVERY IMPOSSIBLE WITH MEMORY STORAGE')
           end if
        END IF

     ELSE IF (ITOLD == ITASK) THEN

        IF (TOSCR) WRITE (*,'(A)') &
          ' WARNING FROM OUTDAT: NEW OUTPUT AND PREVIOUS OUTPUT HAVE SAME FORMAT'
        WRITE (ILU2,'(A)') &
          ' WARNING FROM OUTDAT: NEW OUTPUT AND PREVIOUS OUTPUT HAVE SAME FORMAT'

     ELSE IF (ITOLD == 99) THEN

        IF (TOSCR) WRITE (*,'(A)') &
          ' WARNING FROM OUTDAT: OUTPUT TABLE CANNOT BE CREATED, NO RES.BIN FILE'
        WRITE (ILU2,'(A)') &
          ' WARNING FROM OUTDAT: OUTPUT TABLE CANNOT BE CREATED, NO RES.BIN FILE'
        RETURN

     ELSE IF (ITOLD == 3.OR.ITOLD == 2.OR.ITOLD == 1) THEN

!       PREVIOUS CALL WAS WITH ITASK=3, 2, OR 1
        IEREC = IREC
        if (.NOT. OutDat_UseMemory) then
           WRITE (ILU1, REC=ISREC) ' ', IREC , '....................................', 0.
           READ  (ILU1, REC=1)     RUNDUM,IRUN5
           WRITE (ILU1, REC=1)     '+', IRUN5, '....................................', 0.
        else
           call MemWrite(ISREC, ' ', IREC, '....................................', 0.)
           call MemRead(1, RUNDUM, IRUN5, DumVarName, DumVarValue, DumIOSValue)
           call MemWrite(1,     '+', IRUN5, '....................................', 0.)
        end if
     END IF

     ILTASK = MOD (ITASK,10)

     IF (ITASK >= 14) THEN
!       WHOLE RES.BIN FILE SHOULD BE CONVERTED
        I6 = 1
     ELSE
!       ONLY LAST SET IN RES.BIN FILE SHOULD BE CONVERTED
        I6 = IRUN5
     END IF

     DO I5=I6,IRUN5

     IF (ITASK >= 14.OR.ITOLD == -1) THEN
!       READ NAMES FROM FILE INTO AVN ARRAY OF THE SET WHICH IS
!       CONVERTED INTO AN OUTPUT TABLE. THIS IS NOT NECESSARY IF
!       ONLY THE LAST SET HAS TO BE CONVERTED AND THIS LAST SET
!       WAS JUST FINISHED WITH AN ITASK=2 CALL (NAME OF INDEPENDENT
!       VARIABLE AND ARRAY AVN ARE ALREADY KNOWN !!!).
!       SEARCH FOR FIRST RECORD OF SET FIRST
        IEREC = 1
        DO I1=1,I5
           ISREC = IEREC+1
           if (.NOT. OutDat_UseMemory) then
              READ (ILU1, REC=ISREC) RUNDUM, IEREC
           else
              call MemRead(ISREC, RUNDUM, IEREC, DumVarName, DumVarValue, DumIOSValue)
           end if
        END DO

!       READ NAMES AND DETERMINE TOTAL NUMBER OF NAMES
!       READ NAME OF INDEPENDENT VARIABLE
!       DETERMINE LENGTH OF NAME OF INDEPENDENT VARIABLE

        if (.NOT. OutDat_UseMemory) then
           READ (ILU1, REC=ISREC+1) RUNTYP, IRUN3, LXN
        else
           call MemRead((ISREC+1), RUNTYP, IRUN3, LXN, DumVarValue, DumIOSValue)
        end if

        ILXN  = LEN_TRIM (LXN)
        IFND  = 0

        DO IR=ISREC+2,IEREC
           if (.NOT. OutDat_UseMemory) then
              READ (ILU1, REC=IR) RUNDUM, INAME, LN
           else
              call MemRead(IR, RUNDUM, INAME, LN, DumVarValue, DumIOSValue)
           end if
           IFND = MAX (INAME, IFND)
           IF (INAME > 0) AVN(INAME) = LN
        END DO
     END IF

!    ASSIGN EACH SELECTED VARIABLE A BLOCK NUMBER, THIS IS BASED
!    ON THE LENGTH OF THE NAME OF THE SELECTED VARIABLE, WHEN
!    A NEW NAME DOES NOT FIT ANYMORE ON THE RECORD, A NEW BLOCK IS
!    GENERATED
     FULL_BLOCK = .TRUE.
     IBLOK      = 0
     I2         = 1 + MAX (COL_WIDTH_MNN+1,ILXN)

     IF (ILTASK == 4) THEN
        LINE_LEN = LINE_LEN_4
     ELSE IF (ILTASK == 5) THEN
        LINE_LEN = LINE_LEN_5
     ELSE IF (ILTASK == 6) THEN
        LINE_LEN = LINE_LEN_4
     ELSE IF (ILTASK == 7) THEN
        LINE_LEN = LINE_LEN_7
     ELSE IF (ILTASK == 8) THEN
        LINE_LEN = LINE_LEN_8
     ELSE IF (ILTASK == 9) THEN
        LINE_LEN = LINE_LEN_9
     END IF

!    ASSIGN BLOCK NUMBERS TO VARIABLES
     DO I1=1,IFND

        IF (INSEL == 0) THEN
!          NO VARIABLES WERE SELECTED, SELECT EACH ONE
           SELECTED = .TRUE.
        ELSE
           ISEL = IFINDC (ASELN,INSEL,1,INSEL,AVN(I1))
           IF (ISEL > 0) THEN
!             VARIABLE FOUND IN LIST OF SELECTED VARIABLES
              SELECTED = .TRUE.
           ELSE
!             VARIABLE NOT FOUND IN LIST OF SELECTED VARIABLES
              SELECTED = .FALSE.
           END IF
        END IF

        IF (SELECTED) THEN

           IF (FULL_BLOCK) THEN
!             PREVIOUS BLOCK FULL
              IBLOK      = IBLOK+1
              FULL_BLOCK = .FALSE.
           END IF

!          VARIABLE SELECTED
           IAVNL(I1) = LEN_TRIM (AVN(I1)(1:35))
           IF (IAVNL(I1) == 0) THEN
              IF (TOSCR) WRITE (*,'(A)') &
                ' WARNING FROM OUTDAT: ZERO LENGTH VARIABLE NAME'
              WRITE (ILU2,'(A)') &
                ' WARNING FROM OUTDAT: ZERO LENGTH VARIABLE NAME'
              IAVNL(I1) = 1
           END IF

           IF (ILTASK == 6) THEN
              FULL_BLOCK   = .TRUE.
           ELSE
              I3 = 1+MAX (COL_WIDTH_MNN,IAVNL(I1))
              IF (I2+I3 > LINE_LEN) THEN
!                NEW VARIABLE EXCEEDS OUTPUT FILE RECORD,
!                INCREASE BLOCK NUMBER ETC.
                 IBLOK = IBLOK+1
                 I2    = 1+MAX (COL_WIDTH_MNN+1,ILXN)
              END IF
              I2 = I2+I3
              FULL_BLOCK = .FALSE.
           END IF
           BLK(I1) = IBLOK
        ELSE
           BLK(I1) = 0
        END IF
     END DO

!    CHECK NUMBER OF FOUND BLOCKS
     IF (IBLOK == 0) THEN
        IF (TOSCR) WRITE (*,'(A)') &
          ' WARNING FROM OUTDAT: NO OUTPUT VALUES GIVEN'
        WRITE (ILU2,'(/,A)') &
          ' WARNING FROM OUTDAT: NO OUTPUT VALUES GIVEN'
     ELSE IF (IBLOK > 1.AND.ILTASK == 8) THEN
        CALL FATALERR ('OUTDAT', &
        'MORE THAN ONE BLOCK WITH END OF RUN OPTION')
     END IF

!    WRITE COMMENT HEADER STUFF
!    --------------------------

     IF (ILTASK == 4.OR.ILTASK == 5.OR.ILTASK == 7) THEN

!       DISPLAY COMMENT HEADER FOR NORMAL, TAB-DELIMITED OUTPUT
!       AND ICASA OUTPUT

        COMMCHR = '*'
        IF (ILTASK == 7) COMMCHR = '!'

!       WRITE LINE TO MARK START OF NEW RUN
        WRITE (ILU2,'(A,76A1)') COMMCHR,('-',I1=1,76)

!       WRITE HEADER TO OUTPUT FILE AND POSSIBLE EXTRA
!       COMMENT LINES

        IF (IRUN3 == 0) THEN
           WRITE (ILU2, '(2A)') COMMCHR, ' OUTPUT TABLE NUMBER  :  0 (=FIRST OUTPUT TABLE)'
        ELSE IF (RUNTYP == 'R') THEN
           WRITE (ILU2, '(2A,I5)') COMMCHR, ' OUTPUT FROM RERUN SET:', IRUN3
        ELSE IF (RUNTYP == 'N') THEN
           WRITE (ILU2, '(2A,I5)') COMMCHR,' OUTPUT TABLE NUMBER  :', IRUN3
        ELSE
           CALL FATALERR ('OUTDAT','UNKNOWN RUN TYPE')
        END IF

        WRITE (ILU2,'(3A,/,3A)') &
          COMMCHR,' OUTPUT TABLE FORMAT  : ',TEXT(ILTASK), &
          COMMCHR,' ',RN

        IF (ILTASK /= 7) THEN
!          INSTRUCT OUTCOM TO WRITE COMMENT LINES TO OUTPUT FILE
           CALL OUTCOM ('<PRINT$$$>')
        END IF

!       DO NOT WRITE OUTCOM LINES WITH OTHER OUTPUT FORMATS,
!       BECAUSE THEY WILL BE WRITTEN WITH AN ASTERISK INSTEAD
!       OF WITH AN EXCLAMATION MARK

     ELSE IF (ILTASK == 8) THEN

!       WRITE COMMENT HEADER FOR END OF RUN OUTPUT ONLY ONCE
        IF (FIRST8) THEN
!          WRITE LINE TO MARK START OF RUN
           WRITE (ILU2,'(A,76A1)') '*',('-',I1=1,76)

           WRITE (ILU2,'(2A)') '* OUTPUT TABLE FORMAT  : ', TEXT(ILTASK)
!          INSTRUCT OUTCOM TO WRITE COMMENT LINES TO OUTPUT FILE
           CALL OUTCOM ('<PRINT$$$>')
        END IF
     ELSE IF (ILTASK == 9) THEN
!       DO NOT WRITE A COMMENT HEADER FOR GREENERY OUTPUT
        CONTINUE
     END IF

     DO IB=1,IBLOK

!       SEARCH STUFF FOR BLOCK NUMBER IB
        IFND2 = 0
        DO I1=1,IFND
           IF (BLK(I1) == IB) THEN
!             VARIABLE IS IN CURRENT BLOCK, ADD TO LIST
!             PUT POINTER IN LONG LIST
              IFND2      = IFND2+1
              SEQ(IFND2) = I1
              SEQ2(I1)   = IFND2
           END IF
        END DO

!       HEADER WITH VARIABLE NAMES IS WRITTEN DEPENDENT ON ITASK
        IF (ILTASK == 4.OR.ILTASK == 7) THEN

           WRITE (ILU2,'(A)') ' '
           CHR = ' '

!          WRITE NAME OF INDEPENDENT VARIABLE
!          ----------------------------------

!          INITIALIZE AND ADD LEADING SPACE
           LINE    = ' '
           LINE_L  = 1

           IF (ILTASK == 4) THEN
!             NORMAL TABLE FORMAT
              IF (ILXN <= CENTRE+1) THEN
!                VARIABLE NAME FITS LEFT OF CENTRE POINT
                 I3          = LINE_L + 1 + CENTRE + 1-ILXN
                 I4          = LINE_L + CENTRE + 1
                 LINE(I3:I4) = LXN(1:ILXN)
                 LINE_L      = LINE_L + COL_WIDTH_MNN + 1
                 EXTRA_SPX   = 0
              ELSE
!                VARIABLE NAME EXTENDS BEYOND THE CENTRE POINT
                 LINE(LINE_L + 1:LINE_L + ILXN) = LXN(1:ILXN)
                 EXTRA_SPX                      = MAX (ILXN - COL_WIDTH_MNN + 1,0)
                 LINE_L                         = LINE_L + COL_WIDTH_MNN + 1 + EXTRA_SPX
              END IF
           ELSE IF (ILTASK == 7) THEN
!             ICASA FORMAT
              LINE   = '@'
              LINE_L = 1
              CALL ADDSTR (LINE, LINE_L, LXN)
              CALL ADDSTR (LINE, LINE_L, '>')
              LINE_L    = 14
              EXTRA_SPX = 0
           END IF

!          SET FLAG WHETHER EXTRA SPACE IS REQUIRED WHILE WRITING
           EXTRA_F = (EXTRA_SPX > 0)

!          WRITE NAMES OF DEPENDENT VARIABLES CENTERED ABOVE COLUMN
           DO I1=1,IFND2
              I2 = SEQ(I1)

!             ADD SEPARATING SPACE BETWEEN VARIABLE NAMES
              LINE_L = LINE_L+1

!             VARIABLE SHOULD APPEAR IN THIS BLOCK
              IF (IAVNL(I2) <= CENTRE) THEN
!                VARIABLE NAME FITS TO THE LEFT OF CENTRE POINT
                 I3 = LINE_L + 1 + CENTRE - IAVNL(I2)
                 I4 = LINE_L + CENTRE

                 LINE(I3:I4)  = AVN(I2)(1:IAVNL(I2))
                 LINE_L       = LINE_L + COL_WIDTH_MNN
                 EXTRA_SP(I1) = 0
              ELSE
!                VARIABLE NAME EXTENDS BEYOND CENTRE POINT
                 LINE(LINE_L + 1:LINE_L + IAVNL(I2)) = AVN(I2)(1:IAVNL(I2))
                 EXTRA_SP(I1)                        = MAX (IAVNL(I2)-COL_WIDTH_MNN,0)
                 LINE_L                              = LINE_L+COL_WIDTH_MNN+EXTRA_SP(I1)
              END IF

              IF (EXTRA_SP(I1) > 0) EXTRA_F = .TRUE.

           END DO

!          WRITE LINE TO FILE
           WRITE (ILU2,'(A)') LINE(1:LINE_L)
           WRITE (ILU2,'(A)') ' '

!          ADD A SINGLE SPACE TO ALL COLUMNS
           EXTRA_SPX = EXTRA_SPX+1
           DO I1=1,IFND2
              EXTRA_SP(I1) = EXTRA_SP(I1)+1
           END DO

        ELSE IF (ILTASK == 5.OR.(ILTASK == 8.AND.FIRST8)) THEN

!          WRITE TAB DELIMITED VARIABLE HEADER
           CHR = CHAR (9)

!          WRITE TABS BETWEEN THE NAMES, ALLOW NO SPACES !!
           LINE   = ' '
           LINE_L = 0
           CALL ADDSTR (LINE,LINE_L,LXN)
           DO I1=1,IFND2
              CALL ADDSTR (LINE, LINE_L, CHR)
              CALL ADDSTR (LINE, LINE_L, AVN(SEQ(I1))(1:IAVNL(SEQ(I1))))
           END DO

           EXTRA_F = .FALSE.

!          WRITE LINE TO FILE
           WRITE (ILU2,'(A)') ' '
           WRITE (ILU2,'(A)') LINE(1:LINE_L)
           WRITE (ILU2,'(A)') ' '

           IF (ILTASK == 8.AND.FIRST8) FIRST8 = .FALSE.

        ELSE IF (ILTASK == 6) THEN

!          TWO COLUMN OUTPUT

           CHR = ' '
           WRITE (ILU2,'(4A,/,A,/,1X,4A)') &
             '* ', LXN(1:ILXN), CHR,AVN(SEQ(1))(1:IAVNL(SEQ(1))), &
             ' 1 1 1', &
             AVN(SEQ(1))(1:IAVNL(SEQ(1))),'(',LXN(1:ILXN),')'

           EXTRA_F = .FALSE.

        ELSE IF (ILTASK == 9.AND.FIRST9) THEN

!          WRITE TAB DELIMITED VARIABLE HEADER
           CHR = ','

!          WRITE TABS BETWEEN THE NAMES, ALLOW NO SPACES !!
           LINE   = ' '
           LINE_L = 0
           CALL ADDSTR (LINE,LINE_L,LXN)
           DO I1=1,IFND2
              CALL ADDSTR (LINE, LINE_L, CHR)
              CALL ADDSTR (LINE, LINE_L, AVN(SEQ(I1))(1:IAVNL(SEQ(I1))))
           END DO

           EXTRA_F = .FALSE.

!          WRITE LINE TO FILE
           WRITE (ILU2,'(A)') LINE(1:LINE_L)

           IF (ILTASK == 9.AND.FIRST9) FIRST9 = .FALSE.

        END IF

!       INITIALIZE OUTPUT
        YFND = .FALSE.

!       ONLY END OF RUN VALUES, SEARCH FOR LAST OUTPUT SET
!       (THIS PART WAS TAKEN FROM CRETTP ROUTINE OF TTSELECT)

        IF (ILTASK == 8) THEN
           LN = ' '
           IR = IEREC
           DO WHILE (LN /= LXN.AND.IREC > ISREC)
              IR = IR-1
              if (.NOT. OutDat_UseMemory) then
                 READ (ILU1,REC=IR) RUNDUM, INAME, LN
              else
                 call MemRead(IR, RUNDUM, INAME, LN, DumVarValue, DumIOSValue)
              end if
           END DO
           ISREC = IR-1
        END IF

        DO IR=ISREC+1,IEREC
!          READ NEXT RECORD
           if (.NOT. OutDat_UseMemory) then
              READ (ILU1,REC=IR) RUNDUM, INAME, LN, LV
           else
              call MemRead(IR, RUNDUM, INAME, LN, LV, DumIOSValue)
           end if
!          SEE IF VARIABLE NAME IS THE INDEPENDENT VARIABLE
           IF (LN == LXN) THEN
              IF (YFND) THEN
                 IF (ICHECK == IFND2) THEN
!                   NO MISSING VARIABLES, WRITE LINE
!                   DIRECTLY TO FILE
                    IF (ILTASK >= 4.AND.ILTASK <= 8) THEN
                       IF (.NOT.EXTRA_F) THEN
!                         NO EXTRA SPACES REQUIRED,
!                         NO MISSING VARIABLES
                          WRITE (ILU2,'(1X,1P,G13.6,255(A,G12.5))') &
                                 LVO,(CHR,AVV(I1),I1=1,IFND2)
                       ELSE
!                         EXTRA SPACES REQUIRED
                          WRITE (ILU2,'(A,1P,G13.6,255(A,G12.5))') &
                             SPACE(1:EXTRA_SPX),LVO, &
                            (SPACE(1:EXTRA_SP(I1)),AVV(I1), &
                            I1=1,IFND2)
                       END IF
                    ELSE IF (ILTASK == 9) THEN
                       LINE   = ' '
                       LINE_L = 0
                       CALL ADDINT (LINE, LINE_L, INT (LVO))
                       CALL ADDSTR (LINE, LINE_L, ',')
                       CALL ADDINT (LINE, LINE_L, INT (AVV(1)))
                       CALL ADDSTR (LINE, LINE_L, ',')
                       CALL ADDINT (LINE, LINE_L, INT (AVV(2)))
                       CALL ADDSTR (LINE, LINE_L, ',')
                       CALL ADDINT (LINE, LINE_L, INT (AVV(3)))
                       CALL ADDSTR (LINE, LINE_L, ',')
                       DO I1=4,IFND2
                          CALL ADDREA (LINE, LINE_L, AVV(I1), '1P,G12.5')
                          CALL ADDSTR (LINE, LINE_L, ',')
                       END DO
                       WRITE (ILU2,'(A)') LINE(1:LINE_L)
                    ELSE
                       CALL FATALERR ('OUTDAT', 'INTERNAL ERROR')
                    END IF
                 ELSE
!                   ONE OR MORE MISSING VALUES FOUND WRITE TO STRING
!                   LINE FIRST
                    IF (ILTASK >= 4.AND.ILTASK <= 8) THEN
! NO NEW INDENT LEVEL !
                    IF (.NOT.EXTRA_F) THEN
!                      NO EXTRA SPACES REQUIRED BETWEEN COLUMNS
                       LINE   = ' '
                       LINE_L = 1
                       CALL ADDREF (LINE, LINE_L,LVO, '1P,G13.6')
                       DO I1=1,IFND2
                          CALL ADDSTF (LINE,LINE_L,CHR)
                          IF (FNDA(I1)) THEN
!                            VALUE WAS FOUND
                             CALL ADDREF (LINE, LINE_L, AVV(I1), '1P,G12.5')
                          ELSE
!                            VALUE WAS NOT FOUND
                             LINE(LINE_L + CENTRE:LINE_L + CENTRE) = '-'
                             LINE_L                                = LINE_L + COL_WIDTH_MNN
                          END IF
                       END DO
                    ELSE
!                      EXTRA SPACES REQUIRED BETWEEN COLUMNS
                       LINE   = ' '
                       LINE_L = EXTRA_SPX
                       CALL ADDREF (LINE, LINE_L, LVO, '1P,G13.6')
                       DO I1=1,IFND2
                          LINE_L = LINE_L+EXTRA_SP(I1)
                          IF (FNDA(I1)) THEN
!                            VALUE WAS FOUND
                             CALL ADDREF (LINE, LINE_L, AVV(I1), '1P,G12.5')
                          ELSE
!                            VALUE WAS NOT FOUND
                             LINE(LINE_L + CENTRE:LINE_L + CENTRE) = '-'
                             LINE_L                                = LINE_L + COL_WIDTH_MNN
                          END IF
                       END DO
                    END IF
! END OF NO NEW INDENT LEVEL !
                    ELSE IF (ILTASK == 9) THEN
                       LINE   = ' '
                       LINE_L = 0
                       CALL ADDINT (LINE, LINE_L, INT (LVO))
                       CALL ADDSTR (LINE, LINE_L, ',')
                       CALL ADDINT (LINE, LINE_L, INT (AVV(1)))
                       CALL ADDSTR (LINE, LINE_L, ',')
                       CALL ADDINT (LINE, LINE_L, INT (AVV(2)))
                       CALL ADDSTR (LINE, LINE_L, ',')
                       CALL ADDINT (LINE, LINE_L, INT (AVV(3)))
                       DO I1=4,IFND2
                          CALL ADDSTR (LINE, LINE_L, ',')
                          IF (FNDA(I1)) THEN
!                            VALUE WAS FOUND
                             CALL ADDREA (LINE, LINE_L, AVV(I1), '1P,G12.5')
                          ELSE
!                            VALUE WAS NOT FOUND
                             CALL ADDSTR (LINE, LINE_L, '-')
                          END IF
                       END DO
                    ELSE
                       CALL FATALERR ('OUTDAT','INTERNAL ERROR')
                    END IF

                    WRITE (ILU2,'(A)') LINE(1:LINE_L)
                 END IF
!                INITIALIZE SEARCH FOR 'Y' VALUES
                 YFND = .FALSE.
              END IF

!             REINITIALIZE THINGS
              DO I1=1,IFND2
                 FNDA(I1) = .FALSE.
                 AVV(I1)  = -99.
              END DO
              ICHECK = 0
              LVO = LV
           ELSE
!             RECORD CONTAINS 'Y' ; CHECK NAMES I2...I3
              IF (BLK(INAME) == IB) THEN
                 AVV(SEQ2(INAME))  = LV
                 YFND              = .TRUE.
                 ICHECK            = ICHECK+1
                 FNDA(SEQ2(INAME)) = .TRUE.
              END IF
           END IF
        END DO

!       WRITE LAST LINE AT E_O_F
!       UNFORTUNATELY THIS SECTION MUST BE IDENTICAL TO THE
!       PREVIOUS ONE
        IF (YFND) THEN
           IF (ICHECK == IFND2) THEN
!             NO MISSING VARIABLES, WRITE LINE
!             DIRECTLY TO FILE
              IF (ILTASK >= 4.AND.ILTASK <= 8) THEN
                 IF (.NOT.EXTRA_F) THEN
!                   NO EXTRA SPACES REQUIRED,
!                   NO MISSING VARIABLES
                    WRITE (ILU2,'(1X,1P,G13.6,255(A,G12.5))') &
                           LVO,(CHR,AVV(I1),I1=1,IFND2)
                 ELSE
!                   EXTRA SPACES REQUIRED
                    WRITE (ILU2,'(A,1P,G13.6,255(A,G12.5))') &
                           SPACE(1:EXTRA_SPX),LVO, &
                          (SPACE(1:EXTRA_SP(I1)),AVV(I1), &
                           I1=1,IFND2)
                 END IF
              ELSE IF (ILTASK == 9) THEN
                 LINE   = ' '
                 LINE_L = 0
                 CALL ADDINT (LINE, LINE_L, INT (LVO))
                 CALL ADDSTR (LINE, LINE_L, ',')
                 CALL ADDINT (LINE, LINE_L, INT (AVV(1)))
                 CALL ADDSTR (LINE, LINE_L, ',')
                 CALL ADDINT (LINE, LINE_L, INT (AVV(2)))
                 CALL ADDSTR (LINE, LINE_L, ',')
                 CALL ADDINT (LINE, LINE_L, INT (AVV(3)))
                 CALL ADDSTR (LINE, LINE_L, ',')
                 DO I1=4,IFND2
                    CALL ADDREA (LINE, LINE_L, AVV(I1), '1P,G12.5')
                    CALL ADDSTR (LINE, LINE_L, ',')
                 END DO
                 WRITE (ILU2,'(A)') LINE(1:LINE_L)
              ELSE
                 CALL FATALERR ('OUTDAT','INTERNAL ERROR')
              END IF
           ELSE
!             ONE OR MORE MISSING VALUES FOUND WRITE TO STRING
!             LINE FIRST
              IF (ILTASK >= 4.AND.ILTASK <= 8) THEN
! NO NEW INDENT LEVEL !
              IF (.NOT.EXTRA_F) THEN
!                NO EXTRA SPACES REQUIRED BETWEEN COLUMNS
                 LINE   = ' '
                 LINE_L = 1
                 CALL ADDREF (LINE, LINE_L, LVO, '1P,G13.6')
                 DO I1=1,IFND2
                    CALL ADDSTF (LINE, LINE_L,CHR)
                    IF (FNDA(I1)) THEN
!                      VALUE WAS FOUND
                       CALL ADDREF (LINE, LINE_L, AVV(I1), '1P,G12.5')
                    ELSE
!                      VALUE WAS NOT FOUND
                       LINE(LINE_L + CENTRE:LINE_L + CENTRE) = '-'
                       LINE_L                                = LINE_L + COL_WIDTH_MNN
                    END IF
                 END DO
              ELSE
!                EXTRA SPACES REQUIRED BETWEEN COLUMNS
                 LINE   = ' '
                 LINE_L = EXTRA_SPX
                 CALL ADDREF (LINE, LINE_L, LVO, '1P,G13.6')
                 DO I1=1,IFND2
                    LINE_L = LINE_L + EXTRA_SP(I1)
                    IF (FNDA(I1)) THEN
!                      VALUE WAS FOUND
                       CALL ADDREF (LINE, LINE_L, AVV(I1), '1P,G12.5')
                    ELSE
!                      VALUE WAS NOT FOUND
                       LINE(LINE_L + CENTRE:LINE_L + CENTRE) = '-'
                       LINE_L                                = LINE_L + COL_WIDTH_MNN
                    END IF
                 END DO
              END IF
! END OF NO NEW INDENT LEVEL !
              ELSE IF (ILTASK == 9) THEN
                 LINE   = ' '
                 LINE_L = 0
                 CALL ADDINT (LINE, LINE_L, INT (LVO))
                 CALL ADDSTR (LINE, LINE_L, ',')
                 CALL ADDINT (LINE, LINE_L, INT (AVV(1)))
                 CALL ADDSTR (LINE, LINE_L, ',')
                 CALL ADDINT (LINE, LINE_L, INT (AVV(2)))
                 CALL ADDSTR (LINE, LINE_L, ',')
                 CALL ADDINT (LINE, LINE_L, INT (AVV(3)))
                 DO I1=4,IFND2
                    CALL ADDSTR (LINE,LINE_L,',')
                    IF (FNDA(I1)) THEN
!                      VALUE WAS FOUND
                       CALL ADDREA (LINE, LINE_L, AVV(I1), '1P,G12.5')
                    ELSE
!                      VALUE WAS NOT FOUND
                       CALL ADDSTR (LINE, LINE_L, '-')
                    END IF
                 END DO
              ELSE
                 CALL FATALERR ('OUTDAT', 'INTERNAL ERROR')
              END IF

              WRITE (ILU2,'(A)') LINE(1:LINE_L)
           END IF
        END IF
     END DO

!    ADD SOME BLANK LINES IF TABLE OR SPREADSHEET OUTPUT WAS
!    CHOOSEN

     IF (ILTASK == 4.OR.ILTASK == 5) WRITE (ILU2,'(/,/,/,1X)')

     END DO

  ELSE IF (ITASK == 99) THEN

!    THIS OPTION DELETES THE TEMPORARY FILE
     IF (ITOLD == 99) CALL FATALERR ('OUTDAT', 'TEMPORARY FILE ALREADY DELETED')

     if (.NOT. OutDat_UseMemory) then
        CLOSE (ILU1, STATUS='DELETE')
     else
        call MemDeallocate
     end if

  ELSE IF (ITASK == 0) THEN
!    ALLOW ZERO TASK, (OCCURS WITH IPFORM=0 IN RKDRIV AND
!    EUDRIV DRIVERS)
     CONTINUE
  ELSE
     CALL FATALERR ('OUTDAT','WRONG ITASK')
  END IF

! DO NOT SAVE TASK WHEN IT WAS A DUMMY TASK
  IF (ITASK /= 0) ITOLD = ITASK

  END SUBROUTINE OUTDAT

      SUBROUTINE OUTARR (ITASK, IUNIT, RN, R, N)
      IMPLICIT NONE
      INTEGER, intent(in) :: ITASK, IUNIT, N
      CHARACTER*(*), intent(in)      :: RN
      REAL, intent(in),dimension(10) :: R
      end subroutine

end Module Module_OUTDAT
