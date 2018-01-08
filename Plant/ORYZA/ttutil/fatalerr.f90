SUBROUTINE FatalERR (MODULE,MESSAG)
  USE ttutilPrefs, ONLY: FatalErrorMode, FatalErrorDefault, FatalErrorInternal, FatalErrorERRfile, WriteExternalMessage

  IMPLICIT NONE
! formal
  CHARACTER(LEN=*) :: MODULE, MESSAG

! local variables
  INTEGER          :: il1,il2,UNLOG, ErrUnit
  LOGICAL          :: TOSCR, TOLOG
  CHARACTER(LEN=1) :: DUMMY

! function
  INTEGER :: GETUN

  il1 = LEN_TRIM (MODULE)
  il2 = LEN_TRIM (MESSAG)

! desired output type
  call MESSINQ (TOSCR, TOLOG, UNLOG)

! fudge construction to fool the compiler about the return statement
  if (FatalErrorMode == FatalErrorDefault .or. FatalErrorMode == FatalErrorInternal) then
!    special message ?
     if (FatalErrorMode == FatalErrorInternal) call WriteExternalMessage

     if (il1==0 .and. il2==0) then
        if (TOSCR) write (*,'(A)') ' Fatal execution error, press <Enter>'
        if (TOLOG) write (UNLOG,'(A)') ' Fatal execution error'
     else if (il1>0 .and. il2==0) then
        if (TOSCR) write (*,'(3A)') ' Fatal execution error in ',MODULE(1:il1),', press <Enter>'
        if (TOLOG) write (UNLOG,'(2A)') ' Fatal execution error in ',MODULE(1:il1)
     else
        if (TOSCR) write (*,'(4A,/,A)') ' ERROR in ',MODULE(1:il1),': ',MESSAG(1:il2),' Press <Enter>'
        if (TOLOG) write (UNLOG,'(4A)') ' ERROR in ',MODULE(1:il1),': ',MESSAG(1:il2)
     end if
     if (TOSCR) READ (*,'(A)') DUMMY

!    ================================================
!    replace by any other EXIT procedure if necessary
!    ================================================
     STOP

  else if (FatalErrorMode == FatalErrorERRfile) then
!    special error file to be created
     ErrUnit = GETUN (10,99)
     call FOPENS (ErrUnit,'MODEL_ERRORS.TXT','NEW','DEL')

     if (TOSCR) write (*,'(A)') ' Fatal execution error, see file model_errors.txt'
     if (TOLOG) write (UNLOG,'(A)') ' Fatal execution error, see file model_errors.txt'

     if (il1==0 .and. il2==0) then
        write (ErrUnit,'(A)') ' Fatal execution error'
     else if (il1>0 .and. il2==0) then
        write (ErrUnit,'(2A)') ' Fatal execution error in ',MODULE(1:il1)
     else
        write (ErrUnit,'(4A)') ' ERROR in ',MODULE(1:il1),': ',MESSAG(1:il2)
     end if
     close (ErrUnit)

!    ================================================
!    replace by any other EXIT procedure if necessary
!    ================================================
     STOP
  end if
Return
END SUBROUTINE FatalERR
