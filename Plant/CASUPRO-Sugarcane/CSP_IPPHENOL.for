C=======================================================================
C  CSP_IPPHENOL Subroutine based on IPPHENOL_SC Subroutine by O.H. Daza
C   adapted from IPPHENOL, which was created from INPHEN and IPECO Subroutines
C  (and portions of IPIBS, INPLNT, IPCROP)
C  Reads and initializes phenological parameters. (Called once per
C  simulation)
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  03/31/1991 JWW Separated old INPHEN into INPHEN, INVEG, INCOMP
C  04/01/1991 GH  Adapted for CROPGRO
C  08/../1997 CHP Modified to modularize Phenology sections
C  07/30/1998 CHP Changed name to IPPHENOL - input section of PHENOL module
C  01/12/1999 GH  Incorporated into CROPGRO
C  06/11/2002 GH  Modified for Y2K
C  09/26/2001 OHD adapted for the sugarcane model (CASUPRO)
C  08/12/2003 CHP Added I/O error checking
C  08/22/2003 FSR updated to DSSAT 4.0
C  07/26/2004 CHP Removed variables which were not being used
C  06/30/2010 FSR Added PLF2 variable for CASUPRO
C-----------------------------------------------------------------------
C  Called by: CSP_PHENOL
C  Calls    : ERROR, FIND, IGNORE
C=========================================================================

      SUBROUTINE CSP_IPPHENOL(CONTROL, FILECC,
     &           CROP,  DTPI, ISIMI,                      !Output
     &           LI1, MinGr, Ph1P, Ph1R, Ph2, Ph3,        !Output 
     &           Ph4, PI1, PI2, PLANTS, PLME, PLTPOP,     !Output
     &           RTNFAC, ROWSPC, SDEPTH, Smax,            !Output
     &           StkHrNO, TB, TELOM, TM, TO1,             !Output
     &           TO2, XLFNUM, XLI, XStkNum, YLfFac,       !Output
     &           YLFSZ, YVTR)                             !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.

      IMPLICIT NONE
      EXTERNAL ERROR, FIND, GETLUN, IGNORE

!-------------------------------------------------------------------------
      CHARACTER*1   ISIMI, PLME, BLANK
      CHARACTER*2   CROP
      CHARACTER*6   SECTION, ECOTYP, ECONO, ERRKEY
      CHARACTER*12  FILEC, FILEE
      !CHARACTER*16  ECONAM
      CHARACTER*30  FILEIO
      CHARACTER*80  CHAR, PATHCR, PATHEC
      CHARACTER*92  FILECC, FILEGC
      !CHARACTER*255 C255

      CHARACTER*6 VARNO
      CHARACTER*12 VARNAME

      INTEGER NPHS
      INTEGER LUNIO, LUNECO, ISECT, PATHL, Smax !LUNCRP, 
      INTEGER II !, WLUN
      INTEGER ERR, LINC, LNUM, FOUND
!     INTEGER M, 

      PARAMETER (BLANK = ' ')
      PARAMETER (ERRKEY = 'IPPHEN')
      PARAMETER (NPHS = 4)          ! Number of plant phases = 4

      REAL AZIR, SDEPTH, PLANTS, PLTPOP, RTNFAC, ROWSPC
      REAL PI1, PI2, DTPI, MinGr !Go, Gmax, So, 
      REAL Ph1P, Ph1R, Ph2, Ph3, Ph4, PHTMAX

      REAL TB(5), TO1(5), TO2(5), TM(5), XLFNUM(7), YLFSZ(7) 
      REAL XStkNum(9), YLfFac(9) 
      REAL XLI(7), YVTR(7)
	REAL empty, Gamma, LFMAX, LI1, LIsun, LIshd, PLF1, PLF2 
	REAL R30C2, RES30C, RLF30C, SIZLF, LSFAC 
	REAL StkB, StkM, StkHrNO, StkH2OFac,SuH2OFac, TELOM
!      REAL NSENP(20), PSENP(20), WSENP(20), 
!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

!     Transfer values from constructed data types into local variables.
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO

!-----------------------------------------------------------------------
!     Read in values from temporary file, which were previously input
!     in Subroutine IPIBS. Open file DSSAT45.INP
!-----------------------------------------------------------------------
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)

      READ (LUNIO,100,IOSTAT=ERR) FILEC, PATHCR; LNUM = 7
  100 FORMAT(//////,15X,A12,1X,A80)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

      READ (LUNIO,105,IOSTAT=ERR) FILEE, PATHEC; LNUM = LNUM + 1
  105 FORMAT(15X,A12,1X,A80)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
!-----------------------------------------------------------------------
!     Subroutine FIND finds appropriate SECTION in a file by
!     searching for the specified 6-character string at beginning
!     of each line.
!-----------------------------------------------------------------------
      SECTION = '*SIMUL'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) CALL ERROR (SECTION, 42, FILEIO,LNUM)
      READ(LUNIO,'(31X,A1)',IOSTAT=ERR) ISIMI; LNUM = LNUM + 1
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

!-----------------------------------------------------------------------
!     Find and read Cultivars Section
!-----------------------------------------------------------------------
      SECTION = '*CULTI'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) CALL ERROR (SECTION, 42, FILEIO,LNUM)
      READ(LUNIO,'(3X,A2)',IOSTAT=ERR) CROP; LNUM = LNUM + 1
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

!-----------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
!-----------------------------------------------------------------------
!     Find and Read Planting Details Section
!-----------------------------------------------------------------------
        SECTION = '*PLANT'
        CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND .EQ. 0) CALL ERROR (SECTION, 42, FILEIO,LNUM)
        READ(LUNIO,140,IOSTAT=ERR) PLANTS, PLTPOP, PLME, ROWSPC, AZIR, 
     &  SDEPTH
  140   FORMAT(18X,2(1X,F5.1),5X,A1,6X,2(1X,F5.0),1X,F5.1)
!!        READ(LUNIO,140,IOSTAT=ERR) PLANTS, PLTPOP, PLME, ROWSPC, SDEPTH
!!  140 FORMAT(18X,2(1X,F5.1),5X,A1,6X,1X,F5.0,6X,1X,F5.1)
        LNUM = LNUM + 1
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

! PLANTS Plant population at seeding, plants m^-2
! PLTPOP Plant population at emergence, plants m^-2
! PLME   Planting method: transplant (T), seed (S), 
!                         pregerminated seed (P) or nursery (N),
!                         ratooned (R) (latest option incorporated in 
!                         DSSAT) (personal comunication from Dr. G. Kiker) 
! ROWSPC Row spacing, cm
! SDEPTH Planting depth, cm

!-----------------------------------------------------------------------
!     Find and read entire line in Cultivar Section of DSSAT45.INP
!-----------------------------------------------------------------------
        SECTION = '*CULTI'
        CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND .EQ. 0) CALL ERROR (SECTION, 42, FILEIO,LNUM)
        READ(LUNIO,165,IOSTAT=ERR) VARNO, VARNAME, ECONO,
     &          LFMAX,PHTMAX,StkH2OFac,SuH2OFac,empty,PLF1,
     &          PLF2,Gamma,StkB,StkM,empty,
     &          SIZLF,LIsun,LIshd,empty,TB(1),TO1(1),TO2(1),TM(1),
     &          PI1,PI2,DTPI,LSFAC,empty,LI1,TELOM,TB(2),TO1(2),
     &          TO2(2),TM(2),Ph1P,Ph1R,Ph2,Ph3,Ph4,StkHrNO,RTNFAC,
     &          MinGr,empty,RES30C,RLF30C,R30C2,empty,empty 

  165   FORMAT(A6,1X,A16,1X,A6,44F6.0)                   !02/10/2009 
!!!  165   FORMAT(A6,1X,A16,1X,A6,7(1X,F5.0))
        LNUM = LNUM + 1
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
      ENDIF

      CLOSE (LUNIO)

C-----------------------------------------------------------------------
C     Open FILEE Ecotype coefficients (.ECO File)
C-----------------------------------------------------------------------
        LNUM = 0
        PATHL  = INDEX(PATHEC,BLANK)
        IF (PATHL .LE. 1) THEN
          FILEGC = FILEE
        ELSE
          FILEGC = PATHEC(1:(PATHL-1)) // FILEE
        ENDIF

C-----------------------------------------------------------------------
C    Read Ecotype Parameter File
C-----------------------------------------------------------------------
! ECONAM  Name of the ecotype, which is referenced from *.CUL file
! ECOTYP  Ecotype code for this simulation 
C-----------------------------------------------------------------------
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
				
		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)  !
		
		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)  
		READ(CHAR,'(14X,I6)',IOSTAT=ERR) SMAX 
 		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)

		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)  !CAB
		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)	
		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)	
		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR) !YSLA(6)

		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
		READ(CHAR,'(11X,6F9.0)',IOSTAT=ERR) (XLI(II),II=1,6)
  		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)

		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
		READ(CHAR,'(11X,6F9.5)',IOSTAT=ERR) (YVTR(II),II=1,6)
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)

		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)	!XVSHT
		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR) !YVSWH

		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,6F6.0)',IOSTAT=ERR) (XLFNUM(II),II=1,6)
  		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)

		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,6F6.0)',IOSTAT=ERR) (YLFSZ(II),II=1,6)
  		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)

		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
		READ(CHAR,'(15X,8F5.0)',IOSTAT=ERR) (XStkNum(II),II=1,8)
  		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)

  		CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
		READ(CHAR,'(15X,8F5.2)',IOSTAT=ERR) (YLfFac(II),II=1,8)
  		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)

	  ENDIF

        CLOSE (LUNECO)
	


!        DO WHILE (ECOTYP .NE. ECONO)
!          CALL IGNORE(LUNECO, LNUM, ISECT, C255)
!          IF (ISECT .EQ. 1 .AND. C255(1:1) .NE. ' ' .AND.
!     &          C255(1:1) .NE. '*') THEN
!            READ (C255,3100,IOSTAT=ERR) ECOTYP, ECONAM, Ph1P, Ph1R, 
!     &            Ph2, Ph3, Ph4
!     
! 3100       FORMAT (A6,1X,A17,2(1X,F5.0),1X,F5.1,2(1X,F5.0))
!            
!            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)
!            IF (ECOTYP .EQ. ECONO) THEN
!              EXIT
!            ENDIF
            
!          ELSE IF (ISECT .EQ. 0) THEN
!            IF (ECONO .EQ. 'DFAULT') CALL ERROR(ERRKEY,35,FILEGC,LNUM)
!            ECONO = 'DFAULT'
!            REWIND(LUNECO)
!            LNUM = 0
!          ENDIF
!       ENDDO

!        CLOSE (LUNECO)

C-----------------------------------------------------------------------
C     End of FILEE input
C-----------------------------------------------------------------------

!      ENDIF

      RETURN

      END    !SUBROUTINE CSP_IPPHENOL

!-------------------------------------------------------------------------------
!    CSP_IPPHENOL LOCAL VARIABLES: (Other variables defined in PHENOL_SC)
!-------------------------------------------------------------------------------
! VARIABLE  DEFINITION, UNITS
!
! CHAR      Contains the contents of last record read
! CROP      Crop identification code
! DLAYR(L)  Soil Depth in layer L, cm
! DS(L)     Cumulative depth in soil layer L, cm
! DTPI	  Thermal time threshold (Tb = 9 °C) corresponding to a 
!             given leaf number at which phyllochron interval changes 
!             from Ph1 to Ph2, °C-day
! DUL(L)    Volumetric soil water content at Drained Upper Limit in 
!             soil layer L, cm3 [H2O]/cm3 [soil]
! ECONAM    Name of the ecotype, which is referenced from *.CUL file
! ECONO     Ecotype code - used to match ECOTYP in .ECO file
! ECOTYP    Ecotype code for this simulation
! ERR       Error code for file operation
! ERRKEY    Subroutine name for error file
! ERRNUM    Error number for input
! FILEC     Filename for SPE file (e.g., SBGRO980.SPE)
! FILECC    Path plus filename for species file (*.spe)
! FILEE     Filename for ECO file (e.g., SBGRO980.ECO)
! FILEGC    Pathname plus filename for ECO file
! FILEIO    Filename for input file (e.g., IBSNAT35.INP)
! FOUND     Indicator that good data was read from file by subroutine 
!             FIND (0 - End-of-file encountered, 1 - NAME was found)
! Gmax	  Threshold thermal time at which the maximum stalk number 
!             is reached,	°C-day
! Go	      Threshold thermal time at which the stable stalk number 
!             is set,	°C-day
! ISECT     Data record code (0 - End of file encountered, 1 - Found a 
!             good line to read, 2 - End of Section in file encountered, 
!             denoted by * in column 1)
! ISIMI     Start of simulation code: E = On reported emergence day, 
!             I = When initial conditions measured, 
!             P = On reported planting date, 
!             S = On specified date
! ISWWAT	  Water simulation control switch (Y or N)
! LL(L)     Volumetric soil water content in soil layer L at lower 
!             limit, cm3 cm-3
! LNUM      Current line number of input file
! LUNCRP    Logical unit number for FILEC (*.spe file)
! LUNECO    Logical unit number for FILEE (*.eco file)
! LUNIO     Logical unit number for FILEIO
! NL        Maximum number of soil layers = 20
! NLAYR     Actual number of soil layers
! NPHS      Number of plant phases = 13
! NSENP(I)  Sensitivity of phase I to Nitrogen stress. Varies from -1 
!             (slows dev) to +1 (hastens dev)
! PATHCR    Pathname for SPE file or FILEE
! PATHEC    Pathname for FILEC
! PATHL     Number of characters in path name (path plus filename for FILEC)
! Ph1P      Threshold to sprouting (Tb = 9 °C) - Plant cane, °C-day
! Ph1R	  Threshold to sprouting (Tb = 9 °C) - Ratoon cane, °C-day
! Ph2       Threshold to initiation of growth of primary stalk (emergence for 
!             plant cane), mm (°C-day)-1
! Ph3       Threshold growth of the first tillers and leaves above the 
!             ground surface, °C-day
! Ph4       Threshold for tillering and establishment of the foliage (°C-day)
! PI1       Phyllochron interval 1 (Tb = 9 °C), °C-day
! PI2       Phyllochron interval 2 (Tb = 9 °C), °C-day
! PLANTS    Plant population at seeding, plants m-2
! PLME      Planting method: transplant (T), seed (S), pregerminated seed (P) 
!             or nursery (N)
! PLTPOP    Plant population at emergence, plants m-2
! PSENP	  Sensitivity of phase I to phosphorus stress (not yet used)
! RTNFAC    Number of primary shoots to develop from each mature stalk 
!           at previous harvest.
! ROWSPC    Row spacing, cm
! SAT(L)    Volumetric soil water content in layer L at saturation, 
!             cm3 [water] cm-3 [soil]
! SDEPTH    Planting depth, cm
! SECTION   Section name in input file
! Smax      Maximum number of stalks a variety can yield, # stalks stubble-1
! So        Maximum stable stalk number in a stubble, # stalks stubble-1
! TB        Base temperature, °C
! TM        Maximum Temperature, °C
! TO1       Lower optimum temperature, °C
! TO2       Upper optimum temperature, °C
! VARNAME   Name of cultivar
! VARNO     Identification code or number for a specific cultivar
! WSENP(I)  Sensitivity of phase I to water stress, varying from -1 (slows dev)
!             to 1 (hastens dev)
! YVTR(7)   Variable tiller rate (tiller/°C-day) 
!-------------------------------------------------------------------------------
!     END SUBROUTINE CSP_IPPHENOL
!-------------------------------------------------------------------------------
