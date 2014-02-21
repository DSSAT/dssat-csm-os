      SUBROUTINE FOPENS (IUNIT,FILNAM,STATUS,PRIV)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER IUNIT
      CHARACTER*(*) FILNAM, STATUS, PRIV

**    local variables
      INTEGER IRECL
      CHARACTER*2 TYPE
      SAVE

*     call FOPENG for a formatted, sequential file
      TYPE  = 'FS'
      IRECL = 0
      CALL FOPENG (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV)

      RETURN
      END
