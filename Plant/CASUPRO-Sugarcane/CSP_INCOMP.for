C========================================================================
C  CSP_INCOMP  Subroutine
C     This subroutine initializes parameters for composition of tissues
C     which vary with genotype at the beginning of each run.
C----------------------------------------------------------------------
C  REVISION HISTORY
C  03/31/1991 JWW Separated old INPHEN into INPHEN, INVEG, INCOMP
C  04/01/1991 GH  Adapted for CROPGRO
C  09/18/1998 CHP Moved to PLANT module and added input statements
C  05/10/1999 GH  Incorporated in CROPGRO
C  11/09/2001 O.H. Daza adapted to sugarcane model
C  08/12/2003 CHP Added I/O error checking
C  08/25/2003 F.S. Royce updated to DSSAT 4.0 version of CASUPRO
C  07/26/2004 CHP Removed variables which were not being used
C-----------------------------------------------------------------------
C  Called from: CASUPRO
C  Calls  :     ERROR, FIND, IGNORE, CSP_INCOMP_OUT
C=======================================================================

      SUBROUTINE CSP_INCOMP(DYNAMIC,
     &    FILECC, FRLF,  FRRT,  FRSTM,  FRSU,             !Input
     &    AGRLF, AGRRT,  AGRSTM, AGRSU, AGRVG, AGRVG2)    !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL GETLUN, ERROR, FIND, IGNORE, CSP_INCOMP_OUT
      SAVE
!-----------------------------------------------------------------------

      CHARACTER*6 ERRKEY
      PARAMETER (ERRKEY = 'INCOMP')
      CHARACTER*6 SECTION ! , ECONO, ECOTYP
      CHARACTER*80 CHAR
      CHARACTER*92 FILECC ! , FILEGC
!       CHARACTER*255 C255

      INTEGER LUNCRP  ! , LUNECO
      INTEGER DYNAMIC, ERR, LNUM, FOUND, ISECT

      REAL  AGRLF , AGRRT , AGRSTM, AGRVG , AGRVG2,
     &      FRLF  , FRRT  , FRSTM, 
     &      PCARLF, PCARRT, PCARST, PCARSU,
     &      PLIGLF, PLIGRT, PLIGST, PLIGSU,
     &      PLIPLF, PLIPRT, PLIPST, PLIPSU,
     &      PMINLF, PMINRT, PMINST, PMINSU,
     &      POALF , POART , POAST , POASU ,
     &      PROLFI, PRORTI, PROSTI, PROSUI,
     &      RCH2O , RLIG  , RLIP  , RMIN  , RNO3C , ROA

! LIST OF NEW VARIABLES ADDED TO ENABLE SIMULATION OF SUGARS IN THE 
! SUGARCANE MODEL

      REAL AGRSU, FRSU !, CADSU, CRUSSU, FNINSU, NADSU, NGRSU, NGRSUG
!	REAL PROSUG, PROSUT, WCRSU, WSUDOTN
!      REAL PCNSU
!	REAL NRUSSU, NSUDOT, SUWT
!      REAL WNRSU 

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
!     Open and read in values from FILEC (.SPE) input file.
!-----------------------------------------------------------------------
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)
      LNUM = 0
!-----------------------------------------------------------------------
!    Find and Read Respiration Section
!-----------------------------------------------------------------------
!     Subroutine FIND finds appropriate SECTION in a file by
!     searching for the specified 6-character string at beginning
!     of each line.
!-----------------------------------------------------------------------
        SECTION = '*#RESP'
        CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
		CALL ERROR(SECTION, 42, FILECC, LNUM)
	  ELSE		
 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR) !GRLF
 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR) !GRSU

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(13X,F6.3)',IOSTAT=ERR) RCH2O
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
        ENDIF
!-----------------------------------------------------------------------
        SECTION = '*#PLAN'
        CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
		CALL ERROR(SECTION, 42, FILECC, LNUM)
	  ELSE		

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.3)',IOSTAT=ERR) PCARLF
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.3)',IOSTAT=ERR) PCARRT 
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.3)',IOSTAT=ERR) PCARST
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.3)',IOSTAT=ERR) PCARSU 
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.3)',IOSTAT=ERR) PLIGLF 
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.3)',IOSTAT=ERR) PLIGRT 
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.3)',IOSTAT=ERR) PLIGST 
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.3)',IOSTAT=ERR) PLIGSU 
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.3)',IOSTAT=ERR) PLIPLF 
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.3)',IOSTAT=ERR) PLIPRT
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.3)',IOSTAT=ERR) PLIPST 
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.3)',IOSTAT=ERR) PLIPSU    
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.4)',IOSTAT=ERR) PMINLF
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.2)',IOSTAT=ERR) PMINRT 
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.3)',IOSTAT=ERR) PMINST
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.3)',IOSTAT=ERR) PMINSU 
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.3)',IOSTAT=ERR) POALF   
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.4)',IOSTAT=ERR) POART
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

 		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(14X,F6.2)',IOSTAT=ERR) POAST
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

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

      CLOSE (LUNCRP)
!-----------------------------------------------------------------------

! Echoes input data

      CALL CSP_INCOMP_OUT( 
     &  FILECC, RNO3C, RCH2O,	RLIP, RLIG,	ROA, RMIN, PROLFI,    !Input
     &  PROSTI, PRORTI, PROSUI, PCARLF, PCARST, PCARRT, PCARSU, !Input
     &  PLIPLF, PLIPST, PLIPRT, PLIPSU, PLIGLF, PLIGST, PLIGRT, !Input
     &  PLIGSU, POALF, POAST, POART, POASU, PMINLF, PMINST,     !Input
     &  PMINRT, PMINSU)                                         !Input

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN


      AGRVG  = AGRLF * FRLF + AGRRT * FRRT + AGRSTM * FRSTM +
!Sugars
     &         AGRSU * FRSU

      AGRVG2 = AGRVG + (FRLF * PROLFI + FRRT * PRORTI + FRSTM * PROSTI +
!Sugars
     &                  FRSU * PROSUI) * RNO3C

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END ! SUBROUTINE INCOMP
!=======================================================================

!-----------------------------------------------------------------------
!       Variable definitions
!-----------------------------------------------------------------------
! AGRLF   Mass of CH2O required for new leaf growth 
! AGRRT   Mass of CH2O required for new root growth 
! AGRSTM  Mass of CH2O required for new stem growth 
! AGRVG   Mass of CH2O required for vegetative tissue growth including 
!           stoichiometry and respiration 
! AGRVG2  Total mass of CH2O required for vegetative tissue growth 
! DYNAMIC Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, INTEGR, 
!           OUTPUT, or FINAL 
! ECONO   Ecotype code - used to match ECOTYP in .ECO file
!           ","IPDMND, IPGROW, IPIBS, IPPHENOL, PODS, IPPLNT
! ECOTYP  Ecotype code for this simulation "
! ERR     Error code for file operation 
! FILECC  Path plus filename for species file (*.spe) 
! FILEGC  Pathname plus filename for ECO file "
! FOUND   Indicator that good data was read from file by subroutine FIND (0 
!           - End-of-file encountered, 1 - NAME was found) 
! FRLF    Fraction of vegetative tissue growth that goes to leaves on a day
!           
! FRRT    Fraction of vegetative tissue growth that goes to roots on a day 
! FRSTM   Fraction of vegetative tissue growth that goes to stems on a day 
! ISECT   Data record code (0 - End of file encountered, 1 - Found a good 
!           line to read, 2 - End of Section in file encountered, denoted 
!           by * in column 1
! LNUM    Current line number of input file 
! LUNCRP  Logical unit number for FILEC (*.spe file) 
! LUNECO  Logical unit number for FILEE (*.eco file) "
! PCARLF  Proportion of leaf tissue that is carbohydrate
!           (fraction)","IPGROW, INCOMP
! PCARRT  Proportion of root tissue that is carbohydrate
!           (fraction)","IPGROW, INCOMP
! PCARST  Proportion of stem tissue that is carbohydrate
!           (fraction)","IPGROW, INCOMP
! PLIGLF  Proportion of leaf tissue that is lignin
!           (fraction)","IPGROW, INCOMP
! PLIGRT  Proportion of root tissue that is lignin
!           (fraction)","IPGROW, INCOMP
! PLIGST  Proportion of stem tissue that is lignin
!           (fraction)","IPGROW, INCOMP
! PLIPLF  Proportion of leaf tissue that is lipid
!           (fraction)","IPGROW, INCOMP
! PLIPRT  Proportion of root tissue that is lipid
!           (fraction)","IPGROW, INCOMP
! PLIPST  Proportion of stem tissue that is lipid
!           (fraction)","IPGROW, INCOMP
! PMINLF  Proportion of leaf tissue that is mineral
!           (fraction)","IPGROW, INCOMP
! PMINRT  Proportion of root tissue that is mineral
!           (fraction)","IPGROW, INCOMP
! PMINST  Proportion of stem tissue that is mineral
!           (fraction)","IPGROW, INCOMP
! POALF   Proportion of leaf tissue that is organic acid
!           (fraction)","IPGROW, INCOMP
! POART   Proportion of root tissue that is organic acid
!           (fraction)","IPGROW, INCOMP
! POAST   Proportion of stem tissue that is organic acid
!           (fraction)","IPGROW, INCOMP
! PROLFF  Minimum leaf protein composition after N mining
!           ( g[protein] / g[leaf])
! PROLFI  Maximum protein composition in leaves during growth with 
!           luxurious supply of N
!           (g[protein] / g[leaf tissue])","IPPLNT, IPDMND, I
! PRORTI  Maximum protein composition in roots during growth with luxurious 
!           supply of N (g[protein] / g[root])","IPPLNT, IPDMND, IPGROW, 
! PROSTF  Minimum stem protein composition after N mining
!           (g[protein] / g[stem])","IPGROW, INCOMP, IPDMND
! PROSTI  Maximum protein composition in stems during growth with luxurious 
!           supply of N (g[protein] / g[stem])","IPPLNT, IPDMND, IPGROW, 
! RCH2O   Respiration required for synthesizing CH2O structure
!           (g[CH2O] / g[tissue])","IPDMND, PODCOMP, IPPLNT, 
! RLIG    Respiration required for synthesizing lignin structure
!           (g[CH2O] / g[lignin])","IPDMND, PODCOMP, IPPLNT, 
! RLIP    Respiration required for synthesizing lipid structure
!           (g[CH2O] / g[lipid])","IPDMND, PODCOMP, IPPLNT, I
! RMIN    Respiration required for synthesizing mineral structure
!           (g[CH2O] / g[mineral])","IPPLNT, IPDMND, PODCOMP
! RNO3C   Respiration required for reducing NO3 to protein
!           (g[CH2O] / g[protein])","IPDMND, IPPLNT, INCOMP
! ROA     Respiration required for synthesizing organic acids
!           (g[CH2O] / g[product])","IPDMND, PODCOMP, IPPLNT
!-----------------------------------------------------------------------
!      END SUBROUTINE INCOMP_SC
!=======================================================================
