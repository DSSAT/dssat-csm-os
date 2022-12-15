C=======================================================================
C  Chemical, Subroutine, C.H. Porter
C  Dummy chemical application routine to read data from FILEIO.
C  Will be replaced by a functioning module in upcoming versions.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  10/16/2001 CHP  Written
C  04/20/2002 GH   Adjust for crop rotations
C  08/01/2002 CHP  Merged RUNINIT and SEASINIT into INIT section
C  08/20/2002 GH   Modified for Y2K
C  08/12/2003 CHP  Added I/O error checking
C-----------------------------------------------------------------------
C  Called : MGMTOPS
C  Calls  : 
C=======================================================================

      SUBROUTINE Chemical(CONTROL, ISWITCH, NCHEM)

C-----------------------------------------------------------------------

      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL ERROR, FIND, YR_DOY
      SAVE

      CHARACTER*1 ISWCHE, RNMODE
      CHARACTER*5 CHCOD(10), CHMET(10), CHT(10)
      CHARACTER*6 ERRKEY, SECTION
      PARAMETER (ERRKEY = 'Chemic')
      CHARACTER*30 FILEIO
      CHARACTER*90 CHAR

      INTEGER DYNAMIC, ERRNUM, FOUND, I, IDATE, LNUM, LUNIO
      INTEGER MULTI, NCHEM, YR, YRDIF, YRSIM
      INTEGER CDATE(10)

      REAL CHAMT(10),CHDEP(10)

!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

!     The variable "ISWITCH" is of type "SwitchType".
      TYPE (SwitchType) ISWITCH

      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO
      MULTI   = CONTROL % MULTI
      YRSIM   = CONTROL % YRSIM
      YRDIF   = CONTROL % YRDIF
      RNMODE  = CONTROL % RNMODE

      ISWCHE  = ISWITCH % ISWCHE
!      IDETH   = ISWITCH % IDETH  - will be used to determine output
!         for Chemical.OUT.

C***********************************************************************
C***********************************************************************
C    Input and Initialization 
C***********************************************************************
      IF (DYNAMIC .EQ. INIT) THEN
!-----------------------------------------------------------------------
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)

      !Read Chemical Section
      SECTION = '*CHEMI'
      CALL FIND(LUNIO, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        NCHEM = 0
        DO I = 1,10
!          READ(LUNIO,3096,IOSTAT=ERRNUM,ERR=3097,END=3097) 
!     &      CDATE(I),CHCOD(I),CHAMT(I),CHMET(I),CHDEP(I),CHT(I)  
! 3096     FORMAT(3X,I7,1X,A5,1X,F5.0,1X,A5,1X,F5.0,1X,A5)
          READ(LUNIO,'(3X,I7,1X,A90)',ERR=3097,END=3097) CDATE(I), CHAR
          LNUM = LNUM + 1
          READ(CHAR,3096,IOSTAT=ERRNUM) 
     &      CHCOD(I),CHAMT(I),CHMET(I),CHDEP(I),CHT(I)  
 3096     FORMAT(A5,1X,F5.0,1X,A5,1X,F5.0,1X,A5)
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
          NCHEM = NCHEM + 1
        ENDDO
 3097   CONTINUE
      ENDIF

      CLOSE (LUNIO)

C-----------------------------------------------------------------------
C     Adjust for crop rotations
C-----------------------------------------------------------------------
      IF (RNMODE .EQ. 'Q') THEN
        DO I = 1, NCHEM
          CALL YR_DOY(CDATE(I), YR, IDATE)
          CDATE(I) = (YR + YRDIF) * 1000 + IDATE
        END DO
      ENDIF

C-----------------------------------------------------------------------
C     Adjust for multi year runs
C-----------------------------------------------------------------------
      IF (MULTI .GT. 1) THEN
        DO I = 1, NCHEM
          CALL YR_DOY(CDATE(I),YR,IDATE)
          CDATE(I) = (YR + MULTI - 1) * 1000 + IDATE
        END DO
      ENDIF

C***********************************************************************
C***********************************************************************
C     END OF DYNAMIC IF CONSTRUCT
C***********************************************************************
      ENDIF
C***********************************************************************
      RETURN

      END SUBROUTINE Chemical

