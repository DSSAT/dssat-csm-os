      REAL FUNCTION OBSTRG (VARIN,FILEIN,VARNAM)

!     Returns the estimated (through interpolation) observation from
!     datafile FILEIN for variable VARNAM if forcing was enabled.
!     If no observations could be found or forcing for the current
!     simulation time was disabled, the value VARIN is returned. Can
!     be used to force rate variables.

!     VARIN  - Value to be returned when no triggering is found      I
!     FILEIN - File name in which VARNAM should occur                I
!     VARNAM - Name of variable for which triggering is requested    I

!     OBSTRG - Returned value, = VARIN when no triggering is found,  O
!              if triggered the interpolated observation is returned

      IMPLICIT NONE

!     formal parameters
      CHARACTER (*) FILEIN, VARNAM
      REAL          VARIN

!     local parameters
      INTEGER ITASK
      LOGICAL FRCREQ, THERE, FRC, OBS_DAY
      REAL    VARVAL
      SAVE

      ITASK  = 2
      FRCREQ = .TRUE.

      CALL OBSSYS (ITASK, FILEIN, VARNAM , FRCREQ, &
                   THERE, FRC   , OBS_DAY, VARVAL)

      IF (THERE.AND.FRC) THEN
         OBSTRG = VARVAL
      ELSE
         OBSTRG = VARIN
      END IF

      RETURN
      END
