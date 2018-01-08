      REAL FUNCTION INTGR2 (STATE,RATE,DELT,FILEIN,STATNM)

!     Integrates similar to the INTGRL function when no observations
!     and/or forcing was given. When forcing was enabled, the
!     estimated observation is returned

!     STATE  - Old value of state                                    I
!     RATE   - Rate as calculated by model                           I
!     DELT   - Time step to be applied when no forcing takes place   I
!     FILEIN - Name of datafile in which STATNM is possibly forced   I
!     STATNM - Name of state variable                                I

!     INTGR2 - Function result, integrated or forced                 I

      IMPLICIT NONE
!     formal parameters
      REAL          STATE, RATE, DELT
      CHARACTER (*) STATNM, FILEIN

!     local variables
      LOGICAL FRCREQ, FRC, THERE, OBS_DAY
      REAL FRCSTA
      INTEGER ITASK
      SAVE

      ITASK  = 2
      FRCREQ = .TRUE.

      CALL OBSSYS (ITASK, FILEIN, STATNM , FRCREQ, &
                   THERE, FRC   , OBS_DAY, FRCSTA)

      IF (.NOT.FRC) THEN
         INTGR2 = STATE+RATE*DELT
      ELSE
         INTGR2 = FRCSTA
      END IF

      RETURN
      END
