SUBROUTINE OPENLOGF (xTOSCR, NAME, PROGNAM, VRSSTR, AUTHOR, TTNOTE)
  USE ttutilPrefs

  IMPLICIT NONE
! formal
  LOGICAL          :: xTOSCR, TTNOTE
  CHARACTER(LEN=*) :: NAME, PROGNAM, VRSSTR, AUTHOR

! local
  INTEGER               :: IULOG
  INTEGER, DIMENSION(6) :: DTARR
  DOUBLE PRECISION      :: DPDTTM
  CHARACTER(LEN=80)     :: FILNAM, DTSTR, MESSAG
  CHARACTER(LEN=132)    :: LINE
  REAL                  :: TTUVRS

! functions
  INTEGER :: GETUN

  if (MessagePrefsSet) call FatalERR ('OPENLOGF','cannot be called twice without resetting MESSINI')

! construct logfile name
  call EXTENS (NAME, 'LOG', 0, FILNAM)
  call UPPERC (FILNAM)

! get unit number and open file
  IULOG = GETUN(41,99)
  call FOPENS (IULOG,FILNAM,'new','del')

! current time
  call DTNOW (DTARR)
  call DTARDP (DTARR,0.0,DPDTTM)
  call DTDPST ('day-monthlt-year hour:min:sec',DPDTTM,DTSTR)

! logfile header
  write (IULOG,'(1X,5A)') 'This file ',TRIM(FILNAM),' was created at ',TRIM(DTSTR),' by'
  LINE = REPEAT('=',29+LEN_TRIM(FILNAM)+LEN_TRIM(DTSTR))
  write (IULOG,'(1X,A)') TRIM(LINE)

! initialize TTUTIL message system
  call MESSINI (xTOSCR,.TRUE.,IULOG)

! program / version / author
  call MESSWRT ('program',PROGNAM)
  call MESSWRT ('version',VRSSTR)
  call MESSWRT (' author',AUTHOR)

  if (TTNOTE) then
     call TTUVER (0.0, TTUVRS)
     write (MESSAG,'(A,F5.2)') 'linked with TTUTIL',TTUVRS
     call MESSWRT ('   note',MESSAG)
  end if

! end file and screen header
  write (IULOG,'(1X,A,/,A)') TRIM(LINE),' '
  if (xTOSCR) write (*,'(A)') ' '
Return
END SUBROUTINE OPENLOGF
