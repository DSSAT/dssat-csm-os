SUBROUTINE UPPERC (STRING)
  IMPLICIT NONE
! formal
  CHARACTER(LEN=*) :: STRING

! local
  INTEGER :: i,ic,L
  CHARACTER(LEN=1), DIMENSION(97:122), PARAMETER :: U = &
   (/'A','B','C','D','E','F','G','H','I','J', &
     'K','L','M','N','O','P','Q','R','S','T', &
     'U','V','W','X','Y','Z'/)
  LOGICAL, DIMENSION(0:255), PARAMETER :: Mask = &
   (/(.false.,i=0,96), (.true.,i=97,122), (.false.,i=123,255)/)

  L = len_trim (STRING)
  do i=1,L
!    convert lowercase letters
     ic = ichar(STRING(i:i))
     if (Mask(ic)) STRING(i:i) = U(ic)
  end do
Return
END SUBROUTINE UPPERC
