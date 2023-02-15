C=======================================================================
C  RI_IPCROP, Subroutine
C
C  Read crop parameters from RICER970.SPE
C-----------------------------------------------------------------------
C  Revision history
C
C  06/15/1994 PWW Written
C  02/07/1993 PWW Header revision and minor changes
C  02/07/1993 PWW Added switch common block, restructured
C  08/29/2002 CHP/MUS Converted to modular format for inclusion in CSM.
C  08/12/2003 CHP Added I/O error checking
C  02/25/2012 JZW PHINT from CUL file (remove from SPE)
!  07/01/2012 CHP remove IPCREATE
C=======================================================================

      SUBROUTINE RI_IPCROP (FILEC, PATHCR, !CROP, 
     &    CO2X, CO2Y, MODELVER, PORMIN, 
       !&    CO2X, CO2Y, MODELVER, PHINT, PORMIN, 
     &    RLWR, RWUEP1, RWUMX, SHOCKFAC)

      IMPLICIT    NONE
      EXTERNAL GETLUN, ERROR
      SAVE

!     CHARACTER*2 CROP
      CHARACTER*4 ACRO(9)
      CHARACTER*6, PARAMETER :: ERRKEY = 'RI_IPC'
      CHARACTER   BLANK*1,FILEC*12,PATHCR*80,CHAR*180,FILECC*92

      INTEGER     I,J,PATHL,LUNCRP,ERR,LNUM
      INTEGER MODELVER

      REAL PORMIN, RLWR, RWUEP1, RWUMX, SHOCKFAC
      ! REAL PHINT, PORMIN, RLWR, RWUEP1, RWUMX, SHOCKFAC
      REAL, DIMENSION(10) :: CO2X, CO2Y   !, CO2X1, CO2Y1

!     LOGICAL EOF
      
      PARAMETER (BLANK  = ' ')

      DATA ACRO /'SHME','SHFC','PHIN','CO2X','CO2Y',
     &           'RWEP','PORM','RWMX','RLWR'/
!      !
!      ! Default CO2 response of Rice
!      !
!      DATA CO2X1 /   0, 220, 330, 440, 550, 660, 770, 880, 990,9999/
!      DATA CO2Y1 /0.00,0.71,1.00,1.08,1.17,1.25,1.32,1.38,1.43,1.50/
      !
      ! Default values in the species file
      !
!      MODELVER =    1
!      SHOCKFAC =  1.0
!      !PHINT    = 83.0 !PHENOL  ! JZZW this default was used to create *.spe if *.spe does not exist
!      RWUEP1   = 1.50 !RICE
!      PORMIN   = 0.00              ! Minimum pore space
!      RWUMX    = 0.03              ! Max root water uptake
!      RLWR     = 1.05 !ROOTS
!      CO2X     = CO2X1
!      CO2Y     = CO2Y1

      CALL GETLUN('FILEC', LUNCRP)
      PATHL  = INDEX (PATHCR,BLANK)
      IF (PATHL .LE. 1) THEN
         FILECC = FILEC
       ELSE
         FILECC = PATHCR(1:(PATHL-1)) // FILEC
      ENDIF
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      !
      ! If there is no file, create it in the default directory
      !
      IF (ERR .NE. 0) THEN
         CALL ERROR(ERRKEY,ERR,FILEC,0)
!         CALL RI_IPCREATE (
!     &    ACRO, CO2X, CO2Y, CROP, FILECC, LUNCRP,  MODELVER, 
!     &    PORMIN, RLWR, RWUEP1, RWUMX, SHOCKFAC)
!      ! &    PHINT, PORMIN, RLWR, RWUEP1, RWUMX, SHOCKFAC)
!         RETURN
      ENDIF

C-----------------------------------------------------------------------
C     Read Crop Parameters from FILEC
C-----------------------------------------------------------------------
      LNUM = 0
!     EOF not portable. CHP 7/24/2007
!     DO WHILE (.NOT. EOF (LUNCRP))
      DO WHILE (ERR == 0)
        LNUM = LNUM + 1
        READ (LUNCRP,'(A180)',IOSTAT=ERR, END = 200) CHAR
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEC,LNUM)
        DO J = 1, 9
          IF (CHAR(10:13) .EQ. ACRO(J)) THEN
            SELECT CASE (J)
            CASE ( 1); READ (CHAR(16:20),'(  I5  )',IOSTAT=ERR) MODELVER
            CASE ( 2); READ (CHAR(16:20),'(  F5.0)',IOSTAT=ERR) SHOCKFAC
            !CASE ( 3); READ (CHAR(16:20),'(  F5.0)',IOSTAT=ERR) PHINT
            CASE ( 3); Continue
            CASE ( 4)
              READ (CHAR(16:66),'(10F5.0)',IOSTAT=ERR)(CO2X(I),I=1,10)
            CASE ( 5)
              READ (CHAR(16:66),'(10F5.2)',IOSTAT=ERR)(CO2Y(I),I=1,10)
            CASE ( 6); READ (CHAR(16:21),'(  F5.2)',IOSTAT=ERR) RWUEP1
            CASE ( 7); READ (CHAR(16:21),'(  F5.2)',IOSTAT=ERR) PORMIN
            CASE ( 8); READ (CHAR(16:21),'(  F5.2)',IOSTAT=ERR) RWUMX
            CASE ( 9); READ (CHAR(16:21),'(  F5.2)',IOSTAT=ERR) RLWR
            END SELECT
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEC,LNUM)
          ENDIF
        END DO
      END DO

  200 CLOSE (LUNCRP)

      RETURN
      END SUBROUTINE RI_IPCROP

!C=======================================================================
!C  RI_IPCREATE, Subroutine
!C
!C  Make crop parameter file RICER970.SPE
!C-----------------------------------------------------------------------
!C  Revision history
!C
!C  1. Written                                     P.W.W.      6-15-94
!C  2. Header revision and minor changes           P.W.W.      2- 7-93
!C  2. Added switch common block, restructured     P.W.W.      2- 7-93
!C-----------------------------------------------------------------------
!C  INPUT  :
!C
!C  LOCAL  :
!C
!C  OUTPUT :
!C-----------------------------------------------------------------------
!C  Called : PHENOL
!C
!C  Calls  : None
!C-----------------------------------------------------------------------
!C                         DEFINITIONS
!C
!C  PP3    :
!C=======================================================================
!
!      SUBROUTINE RI_IPCREATE (
!     &    ACRO, CO2X, CO2Y, CROP, FILECC, LUNCRP,  MODELVER, 
!     &    PORMIN, RLWR, RWUEP1, RWUMX, SHOCKFAC)
!      !&    PHINT, PORMIN, RLWR, RWUEP1, RWUMX, SHOCKFAC)
!
!      IMPLICIT    NONE
!
!      CHARACTER*2 CROP
!      CHARACTER*4 ACRO(9)
!      CHARACTER   FILECC*92
!      INTEGER     I,LUNCRP,ERR
!      INTEGER MODELVER
!      REAL PHINT, PORMIN, RLWR, RWUEP1, RWUMX, SHOCKFAC
!      REAL CO2X(10), CO2Y(10)
!
!!      LUNCRP = 10
!      OPEN (LUNCRP,FILE = FILECC, STATUS = 'NEW',IOSTAT=ERR)
!
!      WRITE (LUNCRP,100) CROP
!      WRITE (LUNCRP,110)
!      WRITE (LUNCRP,120) CROP, ACRO( 1), MODELVER
!      WRITE (LUNCRP,130) CROP, ACRO( 2), SHOCKFAC
!      !WRITE (LUNCRP,130) CROP, ACRO( 3), PHINT
!      WRITE (LUNCRP,140) CROP, ACRO( 4), (INT(CO2X(I)),I=1,10)
!      WRITE (LUNCRP,150) CROP, ACRO( 5), (CO2Y(I),I=1,10)
!      WRITE (LUNCRP,160) CROP, ACRO( 6), RWUEP1
!      WRITE (LUNCRP,160) CROP, ACRO( 7), PORMIN
!      WRITE (LUNCRP,160) CROP, ACRO( 8), RWUMX
!      WRITE (LUNCRP,160) CROP, ACRO( 9), RLWR
!
!      CLOSE (LUNCRP)
!
!      RETURN
!
!C-----------------------------------------------------------------------
!C     Format Strings
!C-----------------------------------------------------------------------
!
!  100 FORMAT ('*SPECIES DATA: ',A2,/)
!  110 FORMAT ('*CHARACTERISTICS',/,'@C X(EN) Y(CH)  YSTD')
!  120 FORMAT (A2,1X,'OPT   ',A4,2X,I5)
!  130 FORMAT (A2,1X,'OPT   ',A4,2X,F5.1)
!  140 FORMAT (A2,1X,'OPT   ',A4,2X,10(I5))
!  150 FORMAT (A2,1X,'OPT   ',A4,2X,10(F5.2))
!  160 FORMAT (A2,1X,'OPT   ',A4,2X,F5.2)
!
!      END SUBROUTINE RI_IPCREATE
