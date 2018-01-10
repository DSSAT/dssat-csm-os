SUBROUTINE MESSINQ (xTOSCR, xTOLOG, xUNLOG)
  USE ttutilPrefs

  IMPLICIT NONE
! formal
  LOGICAL :: xTOSCR, xTOLOG
  INTEGER :: xUNLOG

  if (MessagePrefsSet) then
!    pass preferences set
     xTOSCR = TOSCR
     xTOLOG = TOLOG
     xUNLOG = ABS(UNLOG)
  else
!    default i/o
     xTOSCR = TOSCR_default
     xTOLOG = TOLOG_default
     xUNLOG = UNLOG_default
  end if

Return
END SUBROUTINE MESSINQ
