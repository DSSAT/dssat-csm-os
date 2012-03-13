SUBROUTINE RECREAD_INIT (UNIT,INPUT_FILE)
! input file is closed automatically when end of file is reached
! call recread_term is not strictly necessary then (closes only when the file is still open)
  IMPLICIT NONE

! Formal parameters
  CHARACTER(LEN=*) INPUT_FILE
  INTEGER UNIT

! common block
  INCLUDE 'recread.inc'
  SAVE

  L_UNIT = UNIT
  call FOPENG (L_UNIT, trim(INPUT_FILE),'old', 'bu', 0, ' ')

  F_BUF       = ' '
  F_BUF_P     = F_BUF_DEC_LEN
  F_BUF_LEN   = F_BUF_DEC_LEN
  LAST_BUF    = .FALSE.
  L_EOF       = .FALSE.
  CR          = CHAR (13)
  LF          = CHAR (10)
  FILE_CLOSED = .FALSE.
  INIT        = .TRUE.
Return
END SUBROUTINE RECREAD_INIT

BLOCK DATA RECREAD_DATA
IMPLICIT NONE
INCLUDE 'recread.inc'
SAVE
DATA INIT /.FALSE./
END
