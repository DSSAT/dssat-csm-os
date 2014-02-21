      REAL FUNCTION GETOBS (FILEIN,VARNAM)

!     Returns the estimated (through interpolation) observation from
!     datafile FILEIN for variable VARNAM.
!     Can be used to get observations to the output file.

!     FILEIN - File name in which VARNAM should occur                I
!     VARNAM - Name of variable for which observations are requested I

!     GETOBS - Returned interpolated observation is returned

      IMPLICIT NONE

!     formal parameters
      CHARACTER (*) FILEIN, VARNAM

!     local parameters
      INTEGER ITASK
      LOGICAL FRCREQ, THERE, FRC, OBS_DAY
      REAL VARVAL
      SAVE

      ITASK  = 2
      FRCREQ = .FALSE.

      CALL OBSSYS (ITASK, FILEIN, VARNAM , FRCREQ, &
                   THERE, FRC   , OBS_DAY, VARVAL)

      IF (.NOT.THERE) CALL FATALERR ('GETOBS','data not available')
      GETOBS = VARVAL

      RETURN
      END
