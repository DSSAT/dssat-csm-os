SUBROUTINE FOPENG (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV)
  IMPLICIT NONE
! formal
  INTEGER IUNIT,IRECL
  CHARACTER(LEN=*) FILNAM, STATUS, PRIV, TYPE

! local
  INTEGER ILF,ILFIL,ITMP,IWL,IOS, UNLOG
  CHARACTER(LEN=1)   CHOICE
  CHARACTER(LEN=2)   LTYPE
  CHARACTER(LEN=3)   EXT, LPRIV, LSTAT
  CHARACTER(LEN=11)  FFF
  CHARACTER(LEN=132) LFNAME
  LOGICAL DELOLD,KNPRIV,OPENU,OPENF,THERE,OK
  LOGICAL SEQ, DIR, STR
  LOGICAL TOSCR, TOLOG
  SAVE

! initialize error variables
  IOS = 0

! desired output type (used in case of 'Unknown' privilege)
  call MESSINQ (TOSCR, TOLOG, UNLOG)

! make filename local and prepare for operating system
  call STR_COPY (FILNAM,LFNAME,OK)
  if (.not.OK) then
     call MESSWRT ('ERROR in FOPENG','File name too long for internal buffer')
     call FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
  end if

  call FLNAME (LFNAME)
  ILFIL = LEN_TRIM (LFNAME)

! check unit number
! -----------------
! get local copies of status, type and privilege
! unit number has proper value and is free ?
  if (IUNIT < 10 .or. IUNIT > 999) then
     call MESSWRT ('ERROR in FOPENG','Unit number is < 10 or > 999 !')
     call FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
  end if

  INQUIRE (UNIT=IUNIT,OPENED=OPENU)
  if (OPENU) then
!    unit number is in use, get the connected filename
     INQUIRE (UNIT=IUNIT, NAME=LFNAME)
     ILF = LEN_TRIM (LFNAME)
     call MESSWRT ('ERROR in FOPENG: Unit number is already in use for file',LFNAME(1:ILF))
     call FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
  end if

! check status
! ------------
  LSTAT  = STATUS
  call UPPERC (LSTAT)

! Old is always readonly
  if (LSTAT=='RDO') LSTAT = 'OLD'

! simple value check
  if (LEN_TRIM (STATUS) /= 3 .or. (LSTAT /= 'OLD'.and.LSTAT /= 'NEW')) then
     call MESSWRT ('ERROR in FOPENG','illegal file status')
     call FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
  end if

! check type
! ----------
  LTYPE  = TYPE
  call UPPERC (LTYPE)

  if (LEN_TRIM (TYPE) /= 2) then
!    TYPE should be a two character code
     call MESSWRT ('ERROR in FOPENG','Illegal TYPE description')
     call FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)

  else if (INDEX ('FU',LTYPE(1:1))==0 .and. INDEX ('FU',LTYPE(2:2))==0) then
     call MESSWRT ('ERROR in FOPENG','No FORM specified')
     call FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)

  else if (INDEX ('SDB',LTYPE(1:1))==0 .and. INDEX ('SDB',LTYPE(2:2))==0) then
     call MESSWRT ('ERROR in FOPENG','No ACCESS specified')
     call FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
  end if

! get form and check
! ------------------
  SEQ = INDEX(LTYPE,'S') > 0  ! sequential
  DIR = INDEX(LTYPE,'D') > 0  ! direct
  STR = INDEX(LTYPE,'B') > 0  ! stream

  if (INDEX (LTYPE,'F') > 0) then
!    formatted file
     FFF = 'FORMATTED'
     if (STR) then
        call MESSWRT ('ERROR in FOPENG','Formatted stream (type BF) not supported')
        call FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
     end if

  else if (INDEX (LTYPE,'U') > 0) then
!    unformatted file
     FFF = 'UNFORMATTED'
  end if

! check record length
! -------------------
  if (DIR .and. IRECL <= 0) then
     call MESSWRT ('ERROR in FOPENG','Non-positive record length for direct access')
     call FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
  else if (IRECL /= 0 .and. (SEQ .or. STR)) then
     call MESSWRT ('ERROR in FOPENG','Nonzero record length for sequential or stream access')
     call FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
  end if

! check privilege
! ---------------
  LPRIV  = PRIV
  call UPPERC (LPRIV)

  if (LSTAT=='OLD'.and.LPRIV /= ' ') then
     call WARNING_OR ('FOPENG', 'status OLD does not require a privilege')
  else if (LSTAT=='NEW') then
     KNPRIV = LPRIV=='DEL'   .or.   LPRIV=='NOD'  .or.  LPRIV=='UNK'

     if (LEN_TRIM (PRIV) /= 3 .or. .not.KNPRIV) then
        call MESSWRT ('ERROR in FOPENG','illegal privilege')
        call FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
     end if
  end if

! check filename and file status
! ------------------------------
  THERE  = .FALSE.
  OPENF  = .FALSE.
  if (ILFIL==0) then
     call MESSWRT ('ERROR in FOPENG','zero length file name')
     call FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
  else
     INQUIRE (FILE=LFNAME(1:ILFIL),EXIST=THERE,IOSTAT=IOS)

     if (IOS /= 0) then
!       error from system, probably from file name
        call MESSWRT ('ERROR in FOPENG','INQUIRE for file existence not successfull, check IOS value')
        call FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
     end if

     if (THERE) INQUIRE (FILE=LFNAME(1:ILFIL),OPENED=OPENF)

     if (THERE .and. OPENF) then
        call MESSWRT ('ERROR in FOPENG','File is already open')
        call FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
     end if
  end if

  if (LSTAT=='OLD' .and. .not.THERE) then
     call MESSWRT ('ERROR in FOPENG','File does not exist')
     call FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
  end if

  if (LSTAT=='NEW' .and. THERE) then
!    action depends on privilege
     DELOLD = .FALSE.

     if (LPRIV=='UNK') then
!       interactive choice
        ITMP = 0
10      if (THERE) then
           if (TOSCR) WRITE (*    ,'(3A)') ' File ',LFNAME(1:ILFIL),' already exists'
           if (TOLOG) WRITE (UNLOG,'(3A)') ' File ',LFNAME(1:ILFIL),' already exists'
20         CONTINUE
           if (TOSCR) then
              call ENTDCH ('Overwrite (Y/N)','N',CHOICE)
              call UPPERC (CHOICE)
           else
              call MESSWRT ('FOPENG','Screen i/o disabled ; cannot ask overwrite permission')
              call FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
           end if

           if (CHOICE=='Y') then
!             delete old file
              DELOLD = .TRUE.
           else if (CHOICE=='N') then
!             old file not deleted, suggest new file name
              ITMP = ITMP + 1
              WRITE (EXT,'(I3.3)') ITMP
              call EXTENS (FILNAM,EXT,0,LFNAME)
              call ENTDCH ('Enter new file name',LFNAME,LFNAME)
              ILFIL = LEN_TRIM (LFNAME)
              THERE = .FALSE.
              INQUIRE (FILE=LFNAME(1:ILFIL),EXIST=THERE,IOSTAT=IOS)
              if (IOS /= 0) then
!                error from system, probably from file name
                 call MESSWRT ('ERROR in FOPENG','INQUIRE for file existence not successfull, check IOS value')
                 call FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
              end if
              GOTO 10
           else
!             illegal choice
              GOTO 20
           end if
        end if

     else if (LPRIV=='DEL') then
!       delete old file
        DELOLD  = .TRUE.

     else if (LPRIV=='NOD') then
        call MESSWRT ('ERROR in FOPENG', 'Existing file cannot be deleted')
        call FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
     end if

     if (DELOLD) then
!       old file should be deleted
!       delete file as sequential unformatted file ; this does not
!       lead to an error message for direct access files on the Vax
        OPEN (IUNIT,FILE=LFNAME(1:ILFIL),STATUS='OLD',ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
        CLOSE (IUNIT,STATUS='DELETE',IOSTAT=IOS)

        if (IOS /= 0) then
!          error from system
           call MESSWRT ('ERROR in FOPENG', 'Existing file cannot be deleted')
           call FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
        end if
     end if
  end if

! no errors, open file, !!! MACHINE DEPENDENT !!!
  ILF = LEN_TRIM (FFF)
  call LOWERC (LFNAME)

! record length
  IWL = IRECL

! =====================================
! ========== Standard Fortran =========
! =====================================
  if (STR) OPEN (IUNIT, FILE=LFNAME(1:ILFIL), STATUS=LSTAT, ACCESS='STREAM'    , FORM=FFF(1:ILF), IOSTAT=IOS)
  if (SEQ) OPEN (IUNIT, FILE=LFNAME(1:ILFIL), STATUS=LSTAT, ACCESS='SEQUENTIAL', FORM=FFF(1:ILF), IOSTAT=IOS)
  if (DIR) OPEN (IUNIT, FILE=LFNAME(1:ILFIL), STATUS=LSTAT, ACCESS='DIRECT'    , FORM=FFF(1:ILF), IOSTAT=IOS, RECL=IWL)

! =====================================
! ======= Compaq Visual Fortran =======
! =====================================
!  if (STR) OPEN (IUNIT, FILE=LFNAME(1:ILFIL), STATUS=LSTAT, ACCESS='SEQUENTIAL', FORM='Binary'  , IOSTAT=IOS)
!  if (SEQ) OPEN (IUNIT, FILE=LFNAME(1:ILFIL), STATUS=LSTAT, ACCESS='SEQUENTIAL', FORM=FFF(1:ILF), IOSTAT=IOS)
!  if (DIR) OPEN (IUNIT, FILE=LFNAME(1:ILFIL), STATUS=LSTAT, ACCESS='DIRECT'    , FORM=FFF(1:ILF), IOSTAT=IOS, RECL=IWL)

  if (IOS /= 0) then
!    error from system
     call MESSWRT ('ERROR in FOPENG','system error while trying to open file')
     call FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
  end if
Return
END SUBROUTINE FOPENG

SUBROUTINE FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
! error routine of fopeng
  IMPLICIT NONE
! formal
  INTEGER IUNIT,IRECL,IOS
  CHARACTER(LEN=*) FILNAM, STATUS, PRIV, TYPE

! local
  INTEGER ILFIL, UNLOG
  LOGICAL TOSCR, TOLOG
  SAVE

! desired output type
  call MESSINQ (TOSCR, TOLOG, UNLOG)

! supply info on arguments
  ILFIL = MAX (LEN_TRIM (FILNAM),1)
  if (TOSCR) WRITE (*,'(/,A,/,A,I5,3(/,2A),/,A,I4,/,2A)')     &
   ' Arguments of the call to FOPENG leading to this error:', &
   '   Unit             = ',IUNIT,                            &
   '   File name        =    ',FILNAM(1:ILFIL),               &
   '   File status      =    ',STATUS,                        &
   '   File type        =    ',TYPE,                          &
   '   Record length    = ',IRECL,                            &
   '   Delete privilege =    ',PRIV
  if (TOLOG) WRITE (UNLOG,'(/,A,/,A,I5,3(/,2A),/,A,I4,/,2A)') &
   ' Arguments of the call to FOPENG leading to this error:', &
   '   Unit             = ',IUNIT,                            &
   '   File name        =    ',FILNAM(1:ILFIL),               &
   '   File status      =    ',STATUS,                        &
   '   File type        =    ',TYPE,                          &
   '   Record length    = ',IRECL,                            &
   '   Delete privilege =    ',PRIV

  if (IOS /= 0) then
     if (TOSCR) WRITE (*    ,'(A,I6,A)') '   IOSTAT           = ',IOS, ' <-- system I/O status code'
     if (TOLOG) WRITE (UNLOG,'(A,I6,A)') '   IOSTAT           = ',IOS, ' <-- system I/O status code'
   end if

   call FATALERR (' ',' ')
Return
END SUBROUTINE FOPERR
