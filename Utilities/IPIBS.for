C=======================================================================
C  IPIBS, Subroutine, G. Hoogenboom
C-----------------------------------------------------------------------
C  Reads input variables for temporary data file for transfer of
C  information from the INPUT module to CSM.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  05/01/1989 GH  Written.
C  01/08/1997 GH  Modified for Chickpea
C  12/02/1997 CHP Modified for modularization of soil water routines.
C  01/21/1997 GH  Added MESOM, correction for residue code
C  02/25/1998 CHP Modified for modularization of pest routines.
C  11/30/1998 CHP Added RUNINIT and SEASINIT sections.  Data is read from files only
C                 once per simulation, but IPIBS is called for initialization
C                 calculations once per season
C  03/17/2000 GH  Incorporated in CROPGRO
C  06/12/2001 CHP Added check for MESOM
C  11/05/2001 GH  Removed reading of time variables for simulation control
C  08/12/2003 CHP Added I/O error checking
C  10/08/2004 CHP Added GetPut_Iswitch call to push switch information
C                   into constructed variable which is accessible to
C                   all modules.
C  06/14/2005 CHP Added read for MESIC variable for sequenced runs.
!  08/28/2006 CHP Added MESEV - option for new soil evaporation routine 
!                 from SALUS
!  01/11/2007 CHP Changed GETPUT calls to GET and PUT
!  02/05/2007 CHP Reverse location of MESEV and METMP in FILEX
!  04/28/2008 CHP Added switch for CO2 from file (ICO2)
!  05/20/2008 CHP Changed method codes to trigger experimental routines:
!                 MEINF = 'R', Ritchie runoff, with mulch effects
!                 MEINF = 'S', SCS - same as Ritchie method
!                 MEINF = 'N', SCS - no much effects modeled
!                 MEINF = 'M', Mulch effects modelled.
!  12/09/2008 CHP Remove METMP
!  10/02/2009 CHP Removed some checks that are also done in IPEXP
C========================================================================

      SUBROUTINE IPIBS (CONTROL, ISWITCH, 
     &                  CROP, IDETS, MODEL)                 !Output

C-----------------------------------------------------------------------
C
C***  EXPERIMENT AND TREATMENT SELECTION.
C
C-----------------------------------------------------------------------
      USE ModuleDefs 
      USE ModuleData
      IMPLICIT NONE
      EXTERNAL ERROR, FIND, GETLUN, UPCASE
      SAVE

      CHARACTER*1  IDETC, IDETD, IDETG, IDETH, IDETL, IDETN, IDETO
      CHARACTER*1  IDETP, IDETR, IDETS, IDETW
      CHARACTER*1  IFERI, IRESI, IHARI
      CHARACTER*1  IIRRI, IOX, IPLTI
      CHARACTER*1  ISIMI
      CHARACTER*1  ISWWAT, ISWNIT, ISWCHE, ISWTIL, ICO2
      CHARACTER*1  ISWSYM, ISWPHO, ISWPOT, ISWDIS
      CHARACTER*1  MEEVP, MEHYD, MEINF, MEPHO, MESIC
      CHARACTER*1  MESOL, MESOM, MESEV, METMP, MEGHG
      CHARACTER*1  UPCASE, RNMODE
      CHARACTER*2  CROP
      CHARACTER*6  ERRKEY, SECTION
      PARAMETER    (ERRKEY = 'IPIBS ')
      CHARACTER*8  FNAME, MODEL
      CHARACTER*12 FILEX, FILEA, FILEC
      CHARACTER*30 FILEIO
!     CHARACTER*78 MESSAGE(10)
      CHARACTER*80 PATHCR,PATHEX

      INTEGER FROP, ISENS, NSWI
      INTEGER LUNIO, LINC, LNUM, FOUND
      INTEGER ERRNUM, RUN, N_ELEMS

C     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

C     The variable "ISWITCH" is of type "SwitchType".
      TYPE (SwitchType) ISWITCH

C     Transfer values from constructed data types into local variables.
      FILEIO  = CONTROL % FILEIO
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE

C-----------------------------------------------------------------------
C    Open Temporary File
C-----------------------------------------------------------------------
      CALL GETLUN('FILEIO', LUNIO)
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)
C-----------------------------------------------------------------------
C    Read FILE names and paths
C-----------------------------------------------------------------------
      READ (LUNIO,'(55X,I5)', IOSTAT=ERRNUM) ISENS 
      LNUM = 1
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)

      READ (LUNIO,'(/,15X,A8)', IOSTAT=ERRNUM) MODEL
      LNUM = LNUM + 2 
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)

!     FILEX used to determine output file names when FNAME <> 'OVERVIEW'
      READ (LUNIO,'(15X,A12)', IOSTAT=ERRNUM) FILEX
      LNUM = LNUM + 1 
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)

      READ (LUNIO,'(15X,A12,1X,A80)', IOSTAT=ERRNUM) FILEA,PATHEX
      LNUM = LNUM + 1 
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)

      READ (LUNIO,'(/,15X,A12,1X,A80)', IOSTAT=ERRNUM) FILEC, PATHCR  
      LNUM = LNUM + 2 
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)

!-------------------------------------------------------------------------
!     Can by-pass this section unless in debug model
      IF (INDEX(RNMODE, 'D') > 0) THEN

!       For sequenced runs, only read values for RUN 1
        IF (INDEX('QF',RNMODE) .LE. 0 .OR. RUN .EQ. 1) THEN
          READ (LUNIO,'(5(/),15X,A8)', IOSTAT=ERRNUM) FNAME
          LNUM = LNUM + 6 
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)

C-----------------------------------------------------------------------
C    Read Simulation Control
C-----------------------------------------------------------------------
          READ (LUNIO,'(/,31X,A1,41X,A5)',IOSTAT=ERRNUM) ISIMI  
          READ (LUNIO,'(14X,9(5X,A1),2I6)', IOSTAT=ERRNUM) 
     &      ISWWAT, ISWNIT, ISWSYM, ISWPHO, ISWPOT, ISWDIS, ISWCHE, 
     &      ISWTIL, ICO2
          LNUM = LNUM + 3 
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)

          ISWWAT = UPCASE(ISWWAT)
          ISWNIT = UPCASE(ISWNIT)
          ISWSYM = UPCASE(ISWSYM)
          ISWPHO = UPCASE(ISWPHO)
          ISWPOT = UPCASE(ISWPOT)
          ISWDIS = UPCASE(ISWDIS)
          ISWCHE = UPCASE(ISWCHE)
          ISWTIL = UPCASE(ISWTIL)
          ICO2   = UPCASE(ICO2)

          READ (LUNIO,200, IOSTAT=ERRNUM) MESIC, MEEVP, MEINF, MEPHO, 
     &        MEHYD, NSWI, MESOM, MESEV, MESOL, METMP, MEGHG
  200     FORMAT(25X,A1,11X,A1,3(5X,A1),5X,I1,5(5X,A1))
          LNUM = LNUM + 1 
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)

          READ (LUNIO,'(14X,5(5X,A1))', IOSTAT=ERRNUM)  
     &                          IPLTI, IIRRI, IFERI, IRESI, IHARI
          LNUM = LNUM + 1 
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)

          READ (LUNIO,250,IOSTAT=ERRNUM) 
     &       IOX, IDETO, IDETS, FROP, IDETG, IDETC, IDETW,
     &       IDETN, IDETP, IDETD, IDETL, IDETH, IDETR
  250     FORMAT(14X,3(5X,A1),4X,I2,9(5X,A1))
          LNUM = LNUM + 1 
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)

          IF (FROP .LE. 0) FROP = 1

        ELSE        !For sequenced runs, read only selected variables
          READ (LUNIO,'(8(/),31X,A1,17X,A1)',IOSTAT=ERRNUM)ISWSYM,ISWDIS
          LNUM = LNUM + 9 
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)

          ISWSYM = UPCASE(ISWSYM)
          ISWDIS = UPCASE(ISWDIS)

          READ (LUNIO,'(25X,A1,23X, A1)', IOSTAT=ERRNUM) MESIC,MEPHO
          LNUM = LNUM + 1 
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)

          READ (LUNIO,'(14X,5(5X,A1))', IOSTAT=ERRNUM) 
     &      IPLTI, IIRRI, IFERI, IRESI, IHARI
          LNUM = LNUM + 1 
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
        ENDIF
      ELSE
        MEEVP  = ISWITCH % MEEVP
        MEPHO  = ISWITCH % MEPHO
        ISWWAT = ISWITCH % ISWWAT
        ISWNIT = ISWITCH % ISWNIT
        ISWPHO = ISWITCH % ISWPHO
        NSWI   = ISWITCH % NSWI
        IDETS  = ISWITCH % IDETS
      ENDIF
!-------------------------------------------------------------------------

C-----------------------------------------------------------------------
C    Read Cultivar Section
C-----------------------------------------------------------------------
      !Read crop code from Cultivar Section
      SECTION = '*CULTI'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ (LUNIO,'(3X,A2)', IOSTAT=ERRNUM) CROP ; LNUM = LNUM + 1
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
      ENDIF

C-----------------------------------------------------------------------
      CLOSE (LUNIO)

!-------------------------------------------------------------------
!     Determine how many elements are to be simulated (i.e., N, P, K)
      IF (ISWWAT == 'Y') THEN
        IF (ISWNIT == 'Y') THEN
          IF (ISWPHO == 'Y' .OR. ISWPHO == 'H') THEN
!           Water, N & P will be modelled. 
            N_ELEMS = 2
          ELSE
!           Water & N modelled, P not modelled.
            N_ELEMS = 1
          ENDIF
        ELSE
!         Water modelled, but neither N nor P.
          NSWI = 0
          ISWPHO = 'N'
          N_ELEMS = 0
        ENDIF
      ELSE
!       No simulation of water, N or P.
        NSWI = 0
        ISWNIT = 'N'
        ISWPHO = 'N'    
        N_ELEMS = 0
      ENDIF

      IF (ISWWAT .EQ. 'N' .OR. ISWNIT .EQ. 'N') THEN
        NSWI = 0
      ENDIF

!-------------------------------------------------------------------
!     Values read or modified here.
      CONTROL % CROP  = CROP
      CONTROL % DAS   = 0
      CONTROL % LUNIO = LUNIO
      CONTROL % N_ELEMS = N_ELEMS

      ISWITCH % MEEVP  = MEEVP
      ISWITCH % MEPHO  = MEPHO
      ISWITCH % ISWWAT = ISWWAT
      ISWITCH % ISWNIT = ISWNIT
      ISWITCH % ISWPHO = ISWPHO
      ISWITCH % NSWI   = NSWI

!-------------------------------------------------------------------------
!     Can by-pass this section unless in debug model
      IF (INDEX(RNMODE, 'D') > 0) THEN
!       Transfer values from local variables into constructed data types.
        CONTROL % FROP  = FROP
        CONTROL % MESIC = MESIC
        CONTROL % MODEL = MODEL

        ISWITCH % IDETC  = IDETC
        ISWITCH % IDETD  = IDETD
        ISWITCH % IDETG  = IDETG
        ISWITCH % IDETL  = IDETL
        ISWITCH % IDETN  = IDETN
        ISWITCH % IDETO  = IDETO
        ISWITCH % IDETS  = IDETS
        ISWITCH % IDETW  = IDETW
        ISWITCH % IHARI  = IHARI
        ISWITCH % ISIMI  = ISIMI

        ISWITCH % ISWDIS = ISWDIS
        ISWITCH % ISWSYM = ISWSYM
        ISWITCH % ISWPOT = ISWPOT
        ISWITCH % ISWCHE = ISWCHE
        ISWITCH % ISWTIL = ISWTIL
        ISWITCH % ICO2   = ICO2

        ISWITCH % MEHYD  = MEHYD
        ISWITCH % MEINF  = MEINF
        ISWITCH % MESOL  = MESOL
        ISWITCH % MESOM  = MESOM
        ISWITCH % METMP  = METMP
        ISWITCH % MESEV  = MESEV
        ISWITCH % MEGHG  = MEGHG

        ISWITCH % IPLTI  = IPLTI
        ISWITCH % IIRRI  = IIRRI
        ISWITCH % IFERI  = IFERI
        ISWITCH % IRESI  = IRESI
        ISWITCH % IDETP  = IDETP
        ISWITCH % IDETH  = IDETH
        ISWITCH % IDETR  = IDETR
      ENDIF
!-------------------------------------------------------------------------

      CALL PUT(CONTROL)
      CALL PUT(ISWITCH)

      RETURN
      END SUBROUTINE IPIBS

!=======================================================================
! Variable definitions for IPIBS
!=======================================================================
! CROP     Crop identification code 
! ERRKEY   Subroutine name for error file 
! ERRNUM   Error number for input 
! FILEA    Input file which contains observed data for comparison with 
!            simulated results, corresponding to FILEX (e.g., UFGA7801.SBA) 
! FILEC    Filename for SPE file (e.g., SBGRO980.SPE) 
! FILEIO   Filename for input file (e.g., IBSNAT35.INP) 
! FILEX    Experiment file, e.g., UFGA7801.SBX 
! FNAME    Output file name, usually 'OVERVIEW' 
! FOUND    Indicator that good data was read from file by subroutine FIND 
!            (0 - End-of-file encountered, 1 - NAME was found) 
! FROP     Frequency of output days
! IDETC    Code to generate OUTC file (e.g., SoilC.OUT), Y or N 
! IDETH    Code to generate OUTH file (e.g., CHEMICAL.OUT), Y or N 
! IDETL    Switch for detailed printout (Y or N) 
! IDETN    Code to generate OUTN file (e.g., SoilN.OUT), Y or N 
! IDETO    Switch for printing OVERVIEW.OUT file 
! IDETP    Code to generate OUTP file (e.g., SoilP.OUT), Y or N 
! IDETR    Code to generate OUTR file (e.g., OPERAT.OUT), Y or N 
! IDETS    Code to generate OUTR, OUTS and OUTE files (OPERAT.OUT, 
!            SUMMARY.OUT and ENVIRON.OUT) 
! IHARI    Harvest type code: M=at harvest maturity, R=on specified day of 
!            year (HDATE), D= on specified day after planting (HDATE), G= 
!            at specified growth stage (HSTG), A= within specified window 
!            when SW conditions are met 
! IIRRI    Irrigation switch R=on reported dates, D=as reported, days after 
!            planting, A=automatic, when required., F=automatic w/ fixed 
!            amt, P=as reported thru last reported day then automatic, W=as 
!            reported thru last reported day then fixed amount, N=not 
!            irrigated 
! ISENS     
! ISWCHE   Switch for observed chemical applications 
! ISWNIT   Nitrogen simulation switch (Y or N) 
! ISWTIL   Switch for observed tillage application 
! ISWWAT   Water simulation control switch (Y or N) 
! LNUM     Line number of input file 
! LUNIO    Logical unit number for FILEIO 
! MEEVP    Method of evapotranspiration (P=Penman, R=Priestly-Taylor, 
!            Z=Zonal) 
! MEPHO    Method for photosynthesis computation ('C'=Canopy or daily, 
!            'L'=hedgerow or hourly) 
! MESIC    Code for sequenced runs: 'S'=second or subsequent run in 
!            sequenced simulation, 'M'=single run or first run of sequenced 
!            simulation. 
! MESOM    Method for soil N computations ('G'=Godwin or Ceres-based, 
!            'P'=Parton or Century-based (future)) 
! MODEL    Name of CROPGRO executable file 
! NCHEM    Number of chemical applications 
! NHAR     Number harvest dates read 
! NREP     Report number for sequenced or multi-season runs 
! NTIL     Number tillage applications read 
! PATHCR   Pathname for SPE file or FILEE. 
! PLME     Planting method; T = transplant, S = seed, P = pre-germinated 
!            seed, N = nursery 
! SECTION  Section name in input file 
! TDATE(I) Observed date for Ith tillage application 
! TDEP(I)  Depth for Ith tillage application 
! TIMPL(I) Method for Ith tillage application 
! VARNO    Variety number 
! VRNAME   Variety name 
! YEAR     Year of current date of simulation 
! YR       Year portion of date 
! YR_DOY   Function subroutine converts date in YYDDD format to integer 
!            year (YY) and day (DDD). 
! YRPLT    Planting date (YYDDD)
! YRSIM    Start of simulation date (YYDDD)
!=======================================================================

