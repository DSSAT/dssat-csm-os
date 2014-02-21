SUBROUTINE DTNOW (DATEA)
! returns date and time as integer array
! calls the standard Fortran-90 DATE_AND_TIME subroutine
  IMPLICIT NONE

! formal parameters
  INTEGER, DIMENSION(6) :: DATEA

! local variables
  INTEGER, DIMENSION(8) :: VALUES

  call DATE_AND_TIME (VALUES=VALUES)

  DATEA(1) = VALUES(1)
  DATEA(2) = VALUES(2)
  DATEA(3) = VALUES(3)
  DATEA(4) = VALUES(5)
  DATEA(5) = VALUES(6)
  DATEA(6) = VALUES(7)
Return
END SUBROUTINE DTNOW
