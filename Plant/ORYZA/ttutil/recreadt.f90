      SUBROUTINE RECREAD_TERM
      IMPLICIT NONE

!     formal

!     common block
      INCLUDE 'recread.inc'

      SAVE

      IF (.NOT.FILE_CLOSED) THEN
         CLOSE (L_UNIT)
         FILE_CLOSED = .TRUE.
      END IF

      RETURN
      END
