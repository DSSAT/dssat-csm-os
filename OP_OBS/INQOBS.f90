      LOGICAL FUNCTION INQOBS (FILEIN,VARNAM)

!     Inquires the observation system about the presence of
!     observation data on the simulation time.

!     Returns .true. or .false.

!     FILEIN - File name in which VARNAM should occur                I
!     VARNAM - Name of variable for which observations are requested I

!     INQOBS - Returns .true. when observation is found, else .false.

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

      INQOBS = OBS_DAY

      RETURN
      END
