SUBROUTINE MESSINI (xTOSCR, xTOLOG, xUNLOG)
  USE ttutilPrefs

  IMPLICIT NONE
! formal parameters
  LOGICAL :: xTOSCR, xTOLOG
  INTEGER :: xUNLOG

! local
  LOGICAL :: LOGOPEN

  if (xTOLOG .AND. xUNLOG==0) then
!    reset default i/o behaviour (as if MESSINI was never called)
     MessagePrefsSet = .false.

!    reset defaults
     TOSCR = TOSCR_default
     TOLOG = TOLOG_default
     UNLOG = UNLOG_default

     call MESSWRT ('MESSINI','TTUTIL messages to default')

  else
!    activate message direction
     MessagePrefsSet = .true.

!    store settings
     TOSCR = xTOSCR
     TOLOG = xTOLOG
     UNLOG = ABS(xUNLOG)

     if (TOLOG) then
!       verify logfile status
        INQUIRE (UNIT=UNLOG, OPENED=LOGOPEN)
        IF (.NOT.LOGOPEN) THEN
           TOLOG = .FALSE.
           call FOPENS (UNLOG, 'TTUTIL.LOG', 'NEW', 'DEL')
           TOLOG = .TRUE.
           call WARNING_OR ('MESSINI','logfile unit not open')
           call MESSWRT ('MESSINI','logfile TTUTIL.LOG created')
        end if
     end if
  end if

Return
END SUBROUTINE MESSINI
