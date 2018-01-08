MODULE ttutilPrefs
   PRIVATE

!  defaults
   LOGICAL, PUBLIC, PARAMETER :: TOSCR_default = .true.
   LOGICAL, PUBLIC, PARAMETER :: TOLOG_default = .false.
   INTEGER, PUBLIC, PARAMETER :: UNLOG_default = 0

!  message settings
   LOGICAL, PUBLIC, SAVE :: TOSCR = TOSCR_default
   LOGICAL, PUBLIC, SAVE :: TOLOG = TOLOG_default
   INTEGER, PUBLIC, SAVE :: UNLOG = UNLOG_default

!  status
   LOGICAL, PUBLIC, SAVE :: MessagePrefsSet = .false.

!  error message modes
   INTEGER, PUBLIC, PARAMETER :: FatalErrorDefault  = 0
   INTEGER, PUBLIC, PARAMETER :: FatalErrorERRfile  = 1
   INTEGER, PUBLIC, PARAMETER :: FatalErrorInternal = 2

!  actual error mode
   INTEGER, PUBLIC, SAVE :: FatalErrorMode = FatalErrorDefault

!  external message
   INTEGER, SAVE      :: MessLineCnt = 0
   INTEGER, SAVE      :: MessActualLEN
   INTEGER, PARAMETER :: MessLEN = 80
   CHARACTER(LEN=MessLEN), DIMENSION(:), ALLOCATABLE, SAVE :: ExternalMessage

   PUBLIC FillExternalMessage, WriteExternalMessage
CONTAINS
! =================================================================
SUBROUTINE FillExternalMessage (CharArray)
! =================================================================
!  allocates and fills special external error message with input text
   IMPLICIT NONE
!  administration
   CHARACTER (LEN=*), PARAMETER :: SubProgram = 'FillExternalMessage'
!  formal
   CHARACTER(LEN=*), DIMENSION(:) :: CharArray
!  local
   INTEGER :: iw

   MessActualLEN = len(CharArray)
   if (MessActualLEN > MessLEN) then
      FatalErrorMode = 0
      call FatalERR ('FillMessage','Message lines too long')
   end if

   MessLineCnt = size(CharArray) ; iw = 0
   if (allocated(ExternalMessage))  deallocate (ExternalMessage,  stat=iw)
   if (iw/=0) call FatalERR (SubProgram,'cannot de-allocate ExternalMessage')
   allocate (ExternalMessage(MessLineCnt),  stat=iw)
   if (iw/=0) call FatalERR (SubProgram,'cannot allocate ExternalMessage')

   ExternalMessage = CharArray
Return
END SUBROUTINE FillExternalMessage
! =================================================================
SUBROUTINE WriteExternalMessage
! =================================================================
!  writes special external error message
   IMPLICIT NONE
!  local
   INTEGER :: i

   if (TOSCR .and. MessLineCnt>0) then
      write (*,'(1x,a)') Repeat('-',MessActualLEN)
      do i=1,MessLineCnt
         write (*,'(1x,a)') TRIM(ExternalMessage(i))
      end do
      write (*,'(1x,a,/)') Repeat('-',MessActualLEN)
   end if

   if (TOLOG .and. MessLineCnt>0) then
      write (*,'(1x,a)') Repeat('-',MessActualLEN)
      do i=1,MessLineCnt
         write (UNLOG,'(1x,a)') TRIM(ExternalMessage(i))
      end do
      write (*,'(1x,a,/)') Repeat('-',MessActualLEN)
   end if
Return
END SUBROUTINE WriteExternalMessage
END MODULE ttutilPrefs
