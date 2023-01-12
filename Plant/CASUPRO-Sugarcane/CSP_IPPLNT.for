C== CSP_IPPLNT.for ======================================================
C
C  Based on IPPLNT, Subroutine, C.H. Porter
C-----------------------------------------------------------------------
C  Reads variables from crop or species specific data file
C
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/21/1998 CHP Split IPCROP into IPCROP and IPPLNT
C  05/10/1999 GH  Incoporated in CROPGRO
C  03/16/2000 GH  Further adaptation for modular system
C  09/21/2001 CHP Read KCAN, PORMIN, RWUEP1 and RWUMX here for export
C                   to WATBAL.
C  11/06/2001 O.H. Daza modified to read parameters for the sugarcane 
C                 model
C  08/12/2003 CHP Added I/O error checking
C  08/19/2003 FSR Modified for DSSAT 4.0 CASUPRO sugarcane 
C  07/26/2004 CHP Removed variables which were not being used
C  09/02/2004 CHP Added warning for use of default ecotype.
C                 Added KC_SLOPE to SPE file and KC_ECO to ECO file.
C  06/30/2010 FSR Added PLF2 variable for CASUPRO
C-----------------------------------------------------------------------
!  Called by:   CASUPRO (at RUNINIT)
!  Calls:       FIND, ERROR, IGNORE
C=======================================================================
      SUBROUTINE CSP_IPPLNT(CONTROL,
     &  CAB, CanLmtFac, CROP, ECONO, EORATIO, FILECC,          !Output  
     &  FILEGC, FINREF,                                        !Output
     &  GAMMA, GRLF, GRRT, GRST, GRSU, KCAN,                   !Output
     &  KEP, LfShdFac, LSFAC, NOUTDO, PCARSU, PCH2O, PLF1,     !Output 
     &  PLF2, PLIGSU, PLIPSU, PLWT, PMINSU, POASU, PORMIN,     !Output
     &  PROLFI, PRORTI, PROSTI, PROSUI, R30C2, RCH2O, RES30C,  !Output
     &  RLF30C, RLIG, RLIP, RMIN, ROA, RWUEP1, RWUMX, SLAMAX,  !Output
     &  SLAMIN, SLAPAR,                                        !Output
     &  SIZELF, SIZREF, StkB, StkM, StkH2OFac, SuH2OFac,       !Output
     &  TURSLA, XDAY,                                          !Output
     &  XFRRT, XFRSU, XSLATM, XSTKRT, XVSHT, YFRRT,            !Output
     &  YFRSU, YSLATM, YSLA, YSTKRT, YVSHT, ZVSDI)             !Output
!---------------------------------------------------------------------

      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL FIND, ERROR, GETLUN, IGNORE

!-----------------------------------------------------------------------
      CHARACTER*1  BLANK  ! , UPCASE, DETACH
      PARAMETER (BLANK  = ' ')

      CHARACTER*2 CROP
      CHARACTER*6 SECTION, ECONO, ECOTYP
      CHARACTER*6  ERRKEY
      PARAMETER (ERRKEY = 'IPPLNT')

      CHARACTER*12 FILEC, FILEE
      CHARACTER*30 FILEIO
      !CHARACTER*78 MSG(2)
      CHARACTER*80 PATHCR, CHAR, PATHEC
      CHARACTER*92 FILECC, FILEGC
      !CHARACTER*255 C255

      INTEGER CAB, LUNCRP, LUNECO, LUNIO, NOUTDO
      INTEGER PATHL, FOUND, ERR, LINC, LNUM, ISECT
      INTEGER II, XDAY(6)

      REAL
     &  CanLmtFac, FINREF, !, FREEZ1, FREEZ2,
     &  LfShdFac, PCH2O, PROLFI, PRORTI, PROSTI, 
     &  R30C2, RCH2O, RES30C, RLF30C, RLIG, RLIP, 
     &  RMIN, ROA, SIZELF, SIZREF,    !, SLAREF
     &  SLAMAX, SLAMIN, SLAPAR, TURSLA

!      REAL CMOBMX, 

      REAL XSLATM(10), YSLATM(10)

!     Species-dependant variables exported to SPAM or WATBAL::
      REAL EORATIO, KCAN, KEP, PORMIN, RWUEP1, RWUMX 
      REAL LSFAC, KCAN_ECO   !KC_SLOPE, LMF, 

!     List of variables added for the CASUPRO sugarcane model
      REAL empty, GAMMA, GRLF, GRRT, GRST, GRSU
	REAL PCARSU, PLF1, PLF2, PLIGSU, PLIPSU, PMINSU, PLWT 
	REAL POASU, PROSUI, StkB, StkM, StkH2OFac, SuH2OFac 
	REAL XFRRT(4), XFRSU(4), YFRRT(4), YFRSU(4)
	REAL XSTKRT(6), YSLA(6), YSTKRT(6) 
	REAL XVSHT(10), YVSHT(10), ZVSDI(10)

!     The variable "CONTROL" is of constructed type "ControlType" as 
!     defined in ModuleDefs.for, and contains the following variables.
!     The components are copied into local variables for use here.
      TYPE (ControlType) CONTROL
      FILEIO = CONTROL % FILEIO
      LUNIO  = CONTROL % LUNIO

!-----------------------------------------------------------------------
!       Read data from FILEIO (.INP) for use in CASUPRO module
!-----------------------------------------------------------------------
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
!-----------------------------------------------------------------------
      READ(LUNIO,50,IOSTAT=ERR) FILEC, PATHCR ; LNUM = 7
   50 FORMAT(6(/),15X,A12,1X,A80)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

      READ(LUNIO,51,IOSTAT=ERR) FILEE, PATHEC; LNUM = LNUM + 1
   51 FORMAT(15X,A12,1X,A80)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

!-----------------------------------------------------------------------
!    Read Experiment Details, Treatments, and Cultivars Sections
!-----------------------------------------------------------------------
      SECTION = '*EXP.D'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ(LUNIO,'(////,3X,A2)',IOSTAT=ERR) CROP; LNUM = LNUM + 5
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
      ENDIF

      SECTION = '*PLANT'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
         READ(LUNIO,'(61X,F4.0)',IOSTAT=ERR) 
     &	     PLWT   ; LNUM = LNUM + 1
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
      ENDIF

!-----------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
!-----------------------------------------------------------------------
!    Read Cultivar Section
!-----------------------------------------------------------------------
        SECTION = '*CULTI'
        CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILEIO, LNUM)
        ELSE

          READ(LUNIO,'(24X,A6,12X,8F6.3,6X,F6.3,60X,F6.3,96X,3F6.3)'
     &	     ,IOSTAT=ERR)ECONO, StkH2OFac, SuH2OFac, empty, PLF1 
     &         ,PLF2, GAMMA ,StkB, StkM, SIZELF 
     &         ,LSFAC, RES30C, RLF30C, R30C2; LNUM = LNUM + 1

          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
        ENDIF

      ENDIF
      
      CLOSE (LUNIO)

!-----------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
!-----------------------------------------------------------------------
! READ CROP PARAMETERS FROM FILEC (.SPE file)
!-----------------------------------------------------------------------
        LNUM = 0
        PATHL  = INDEX(PATHCR,BLANK)
        IF (PATHL .LE. 1) THEN
          FILECC = FILEC
        ELSE
          FILECC = PATHCR(1:(PATHL-1)) // FILEC
        ENDIF
        CALL GETLUN('FILEC', LUNCRP)
        OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)
        LNUM = 0
C-----------------------------------------------------------------------
C READ PHOTOSYNTHESIS PARAMETERS *******************
C-----------------------------------------------------------------------
	  SECTION = '*#PHOT'
	  CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
		CALL ERROR(SECTION, 42, FILECC, LNUM)
	  ELSE
		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(12X,F6.3)',IOSTAT=ERR) KCAN
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDIF

C-----------------------------------------------------------------------
C READ RESPIRATION PARAMETERS **********************
C-----------------------------------------------------------------------
        SECTION = '*#RESP'
        CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
		CALL ERROR(SECTION, 42, FILECC, LNUM)
	  ELSE		

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(13X,F6.2)',IOSTAT=ERR) GRLF
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(13X,F6.2)',IOSTAT=ERR) GRRT
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(13X,F6.2)',IOSTAT=ERR) GRST
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(13X,F6.2)',IOSTAT=ERR) GRSU
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(34X,F6.3)',IOSTAT=ERR) RCH2O
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(13X,F6.3)',IOSTAT=ERR) RLIP 
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(13X,F6.3)',IOSTAT=ERR) RLIG   
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(13X,F6.4)',IOSTAT=ERR) ROA
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(13X,F6.2)',IOSTAT=ERR) RMIN
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(13X,F6.2)',IOSTAT=ERR)  PCH2O
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        ENDIF

C-----------------------------------------------------------------------
C READ PLANT COMPOSITION PARAMETERS
C-----------------------------------------------------------------------
        SECTION = '*#PLAN'
        CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
		CALL ERROR(SECTION, 42, FILECC, LNUM)
	  ELSE		

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.3)',IOSTAT=ERR) PCARSU 
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.3)',IOSTAT=ERR) PLIGSU 
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.3)',IOSTAT=ERR) PLIPSU 
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.3)',IOSTAT=ERR) PMINSU 
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.2)',IOSTAT=ERR) POASU 
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.2)',IOSTAT=ERR) PROLFI 
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.2)',IOSTAT=ERR) PRORTI 
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.2)',IOSTAT=ERR) PROSTI 
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.2)',IOSTAT=ERR) PROSUI 
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
 		
        ENDIF

C-----------------------------------------------------------------------
C READ CARBON AND NITROGEN MINING PARAMETERS
C-----------------------------------------------------------------------
!        SECTION = '!*CARB'
!        CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
!        IF (FOUND .EQ. 0) THEN
!          CALL ERROR(SECTION, 42, FILECC, LNUM)
!        ELSE
!          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
!          READ(CHAR,'(F6.0)',IOSTAT=ERR) CMOBMX
!          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!        ENDIF

C-----------------------------------------------------------------------
C READ LEAF GROWTH PARAMETERS
C-----------------------------------------------------------------------        
        SECTION = '*#LEAF'
        CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
		CALL ERROR(SECTION, 42, FILECC, LNUM)
	  ELSE		

		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.2)',IOSTAT=ERR) FINREF 
 		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
	
 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR) ! SLAREF moved to 
										    ! ECO as YSLA(6)
		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.2)',IOSTAT=ERR) SIZREF
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.2)',IOSTAT=ERR) SLAMAX
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.1)',IOSTAT=ERR) SLAMIN
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.3)',IOSTAT=ERR) SLAPAR
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.3)',IOSTAT=ERR) TURSLA
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,5F6.1)',IOSTAT=ERR) (XSLATM(II),II = 1,5)
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,5F6.2)',IOSTAT=ERR) (YSLATM(II),II = 1,5)
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        
	  ENDIF

!-----------------------------------------------------------------------
C Read ROOT parameters
!-----------------------------------------------------------------------
        SECTION = '*#ROOT'
        CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
		CALL ERROR(SECTION, 42, FILECC, LNUM)
	  ELSE		
		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)

		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(13X,F6.2)',IOSTAT=ERR) RWUEP1 
 		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(13X,F6.2)',IOSTAT=ERR) RWUMX
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)

		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(13X,F6.2)',IOSTAT=ERR) PORMIN 
 		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)

		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(13X,4F6.0)',IOSTAT=ERR) (XFRRT(II),II = 1,4) 
 		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(13X,4F6.2)',IOSTAT=ERR) (YFRRT(II),II = 1,4)
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(13X,6F6.0)',IOSTAT=ERR) (XSTKRT(II),II = 1,6) 
 		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(13X,6F6.2)',IOSTAT=ERR) (YSTKRT(II),II = 1,6)
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        ENDIF
C-----------------------------------------------------------------------
        SECTION = '*#EVAP'
        CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
		CALL ERROR(SECTION, 42, FILECC, LNUM)
	  ELSE		

		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(12X,F6.2)',IOSTAT=ERR) KEP 
 		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(12X,F6.2)',IOSTAT=ERR) EORATIO
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        ENDIF

        CLOSE (LUNCRP)
C-----------------------------------------------------------------------
C    Read Ecotype Parameter File - check for use of default
C-----------------------------------------------------------------------
!    Set file plus pathname for ecotype parameter file
!-----------------------------------------------------------------------
        PATHL  = INDEX(PATHEC,BLANK)
        IF (PATHL .LE. 1) THEN
          FILEGC = FILEE
        ELSE
          FILEGC = PATHEC(1:(PATHL-1)) // FILEE
        ENDIF

        CALL GETLUN('FILEE', LUNECO)
        OPEN (LUNECO,FILE = FILEGC,STATUS = 'OLD',IOSTAT=ERR)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,0)
        ECOTYP = '      '
        LNUM = 0

C------ Find particular ECOTYPE ----------------------------------------

	  SECTION = ECONO 
	  CALL FIND(LUNECO, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
		CALL ERROR(SECTION, 42, FILECC, LNUM)
	  ELSE		
		
		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.3)',IOSTAT=ERR) KCAN_ECO 
 		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)

		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR) ! SMAX

		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,I6)',IOSTAT=ERR) CAB
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)

		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.3)',IOSTAT=ERR) CanLmtFac 
 		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)

		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.3)',IOSTAT=ERR) LfShdFac 
 		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)

		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)  !MINSHD
		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)  !StkSenFrac
                CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)  !SENDAY

		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,6I6)',IOSTAT=ERR) (XDAY(II),II = 1,6) 
  		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)

		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,6F6.0)',IOSTAT=ERR) (YSLA(II),II = 1,6)
  		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)

		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)  !XLI(6)
		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)  !YVTR(6)

		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,10F6.0)',IOSTAT=ERR) (XVSHT(II),II=1,10)
  		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)

		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,10F6.2)',IOSTAT=ERR) (YVSHT(II),II=1,10)
  		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)

		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,10F6.1)',IOSTAT=ERR) (ZVSDI(II),II=1,10)
  		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)

		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)  !YVSWH
		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)  !XLFNUM 
		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)  !YLFSZ
		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)  !XStkNum
		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)	 !YLfFac

		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,4F6.0)',IOSTAT=ERR) (XFRSU(II),II = 1,4) 
  		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)

		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,4F6.3)',IOSTAT=ERR) (YFRSU(II),II = 1,4)
  		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)

	  ENDIF
		
        CLOSE (LUNECO)

        IF (KCAN_ECO .GT. 1.E-6) THEN
          KCAN = KCAN_ECO
        ENDIF

!-----------------------------------------------------------------------
      ENDIF
!-----------------------------------------------------------------------
!     Assign unit number for overview.out file
      CALL GETLUN('OUTO', NOUTDO)
!-----------------------------------------------------------------------
! Write results of input  If this is re-activated, must incorporate
!                         file CSP_IPPLNT_OUT.for, which is not fully
!                         converted from DSSAT 3.5 format.
!-----------------------------------------------------------------------
!     CALL CSP_IPPLNT_OUT(CONTROL,
!    &  CMOBMX, CROP, ECONO, EORATIO, FILECC, FILEGC, FREEZ1,  !Input
!    &  FREEZ2, KCAN, KEP, NOUTDO, PCARSU, PCH2O,              !Input
!    &  PLIGSU, PLIPSU, PMINSU, POASU, PORMIN, PROLFI,         !Input
!    &  PRORTI, PROSTI, PROSUI, R30C2, RCH2O, RES30C, RLIG,    !Input
!    &  RLIP, RMIN, ROA, RWUEP1, RWUMX)          !Input
!-----------------------------------------------------------------------
      RETURN
      
      END ! SUBROUTINE CSP_IPPLNT
!=======================================================================

!-----------------------------------------------------------------------
! Variable definitions
!-----------------------------------------------------------------------
! CMOBMX   Maximum C pool mobilization rate (g[CH2O] / m2 / d)
! CROP     Crop identification code 
! DLAYR(L) Soil Depth in layer L (cm)
! DS(L)    Cumulative depth in soil layer L (cm)
! DUL(L)   Volumetric soil water content at Drained Upper Limit in soil 
!            layer L (cm3 [H2O] /cm3 [soil])
! ECONO    Ecotype code - used to match ECOTYP in .ECO file 
! ENAME    Experiment description 
! EO       Potential evapotranspiration rate (mm/d)
! EORATIO  Ratio of increase in EO with increase in LAI (up to LAI=6.0) 
!            for use with FAO-56 Penman reference EO. (--)
! EP       Actual plant transpiration rate (mm/d)
! ERR      Error code for file operation 
! EXPER    Experiment code (prefix of input files) 
! FILEC    Filename for SPE file (e.g., SBGRO980.SPE) 
! FILECC   Path plus filename for species file (*.spe) 
! FILEE    Filename for ECO file (e.g., SBGRO980.ECO) 
! FILEGC   Pathname plus filename for ECO file 
! FILEIO   Filename for INP file (e.g., IBSNAT35.INP) 
! FOUND    Indicator that good data was read from file by subroutine FIND 
!            (0 - End-of-file encountered, 1 - NAME was found) 
! FREEZ1   Temperature below which plant loses all leaves, but development 
!            continues (°C)
! FREEZ2   Temperature below which plant growth stops completely. (°C)
! IDETG    Flag to print growth output file
! IDETL    Switch for detailed printout (0,N,Y,D,A) 
! IDETO    Switch for printing OVERVIEW.OUT file 
! ISECT    Data record code (0 - End of file encountered, 1 - Found a good 
!            line to read, 2 - End of Section in file encountered, denoted 
!            by * in column 1
! ISWDIS   Pest damage simulation switch (Y or N) 
! ISWNIT   Nitrogen simulation switch (Y or N) 
! ISWWAT   Water simulation control switch (Y or N)
! KCAN     Canopy light extinction coefficient for daily PAR, for 
!            equidistant plant spacing, modified when in-row and between row 
!            spacings are not equal 
! KEP      Energy extinction coefficient for partitioning EO to EP (--)
! LL(L)    Volumetric soil water content in soil layer L at lower limit
!            ( cm3/cm3)
! LUNCRP   Logical unit number for FILEC (*.spe file) 
! MEEVP    Method of evapotranspiration (P=Penman, R=Priestly-Taylor, 
!            Z=Zonal) 
! MODEL    Name of CROPGRO executable file 
! NL       maximum number of soil layers = 20 
! NLAYR    Number of soil layers 
! NOUTDO   Logical unit for OVERVIEW.OUT file 
! PATHCR   Pathname for SPE file or FILEE. 
! PATHEC   Pathname for FILEC 
! PATHL    Number of characters in path name (path plus filename for FILEC)
! PCARSU   Proportion of sugars that is carbohydrate (fraction)
! PCH2O    Respiration loss due to storage/mobilization of CH2O
!            (g[CH2O] / g[CH2O])
! PLIGSD   Proportion of seed tissue that is lignin (fraction)
! PLIGSH   Proportion of shell tissue that is lignin (fraction)
! PLIGSU   Proportion of sugars that is lignin (fraction)
! PLIPSH   Proportion of shell tissue that is lipid (fraction)
! PLIPSU   Proportion of sugars that is lipid (fraction)
! PMINSD   Proportion of seed tissue that is mineral (fraction)
! PMINSH   Proportion of shell tissue that is mineral (fraction)
! PMINSU   Proportion of sugars that is mineral (fraction)
! POASD    Proportion of seed tissue that is organic acid (fraction)
! POASH    Proportion of shell tissue that is organic acid (fraction)
! POASU    Proportion of sugars that is organic acid (fraction)
! PORMIN   Minimum pore space required for supplying oxygen to roots for 
!            optimal growth and function (cm3/cm3)
! PROLFI   Maximum protein composition in leaves during growth with 
!            luxurious supply of N (g[protein] / g[leaf tissue])
! PRORTI   Maximum protein composition in roots during growth with 
!            luxurious supply of N (g[protein] / g[root])
! PROSHI   Maximum protein composition in shells during growth with 
!            luxurious supply of N ( g[protein] / g[shell tissue])
! PROSTI   Maximum protein composition in stems during growth with 
!            luxurious supply of N (g[protein] / g[stem])
! PROSUF   Minimum sugars protein composition after N mining
!            (g [protein] / g [sugars])
! PROSUI   Maximum protein composition in sugars during growth with 
!            luxurious supply of N (g [protein] / g [sugars])
! R30C2    Respiration coefficient that depends on total plant mass, value 
!            at 30C (g[CH2O] used / g[CH2O] fixed / hr)
! RCH2O    Respiration required for synthesizing CH2O structure
!            (g[CH2O] / g[tissue])
! RES30C   Respiration coefficient that depends on gross photosynthesis, 
!            value at 30C (g CH2O/g DW/hr)
! RFIXN    CH2O required for biological N fixation (g[CH2O] / g[protein])
! RLIG     Respiration required for synthesizing lignin structure
!            (g[CH2O] / g[lignin])
! RLIP     Respiration required for synthesizing lipid structure
!            (g[CH2O] / g[lipid])
! RMIN     Respiration required for synthesizing mineral structure
!            (g[CH2O] / g[mineral])
! ROA      Respiration required for synthesizing organic acids
!            (g[CH2O] / g[product])
! RWUEP1   Threshold for reducing leaf expansion compared w/ ratio of 
!            TRWU/EP1 (total potenial daily root water uptake/ actual 
!            transpiration)
! RWUMX    Maximum water uptake per unit root length, constrained by soil 
!            water (cm3[water] / cm [root])
! SAT(L)   Volumetric soil water content in layer L at saturation
!            (cm3 [water] / cm3 [soil])
! TITLET   Description of treatment for this simulation 
! TRTNO    Treatment number 
! TTFIX    Physiological days delay in nodule initiation
!            (photo-thermal days / day)
! XVSHT   Node (v-stage or VS) number, reference stalk 
! YVSHT   Mature internode length, reference stalk (cm)
! ZVSDI   Mature internode diameter, reference stalk (cm)
!-----------------------------------------------------------------------
!      END SUBROUTINE CSP_IPPLNT
!=======================================================================

