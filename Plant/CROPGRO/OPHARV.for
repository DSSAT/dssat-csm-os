C=======================================================================
C  OPHARV, Subroutine G. Hoogenboom, J. W. Jones
C  Generates output for seasonal data.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1990 GH  Written
C  11/02/1999 CHP Changed TSOC to THUMC, TSIN to THUMN, AMTRES to CUMRES 
C  07/01/2000 GH  Eliminated common block statements
C  03/03/2002 GH  Modified logic for reading fileA
C  06/11/2002 GH  Modified for Y2K
C  08/12/2003 CHP Added I/O error checking and changed call to READA
!  05/03/2004 CHP Added P stresses to OPVIEW call 
!  12/16/2004 KJB, LAH, CHP Change BWAH to BWAM, compute as 
!                   TOPWT - SDWT, instead of STMWT
!  02/03/2005 CHP Change HWAM to mean dry weight at harvest maturity 
!                   (not physiological maturity).
!  02/04/2005 CHP Added PODWT to SUMVALS array for output to Summary.out
!  08/11/2005 CHP/GH BWAH = TOPWT - PODWT (was TOPWT - SDWT)
!  10/24/2005 CHP Added environmental & stress factors to Overview.OUT
C  02/09/2007 GH  Add path for FileA
!  08/28/2009 CHP added EDAT, EDAP 
!  09/17/2019 CHP remove crop CT
C  07/08/2022 GH  Add CU for Cucumber
C  05/01/2023 GH  Add GY for Guar; SR for Strawberry
!  06/20/2024 FO  Added Economic Yield for Evaluate and Summary
!  06/27/2024 FO  Added Lint Percentage for Evaluate
!  07/11/2024 FO  Added Economic standard output format
!  10/11/2024 GH  Add AM for Amaranth
C=======================================================================

      SUBROUTINE OPHARV(CONTROL, ISWITCH, 
     &    AGEFAC, CANHT, CANNAA, CANWAA, CROP,            !Input
     &    HARVFRAC, LAIMX, MDATE, NSTRES, PCLSD, PCNSD,   !Input
     &    PODNO, PODWT, PStres1, PStres2, SDRATE, SDWT,   !Input
     &    SEEDNO, STGDOY, SWFAC, TOPWT, TURFAC,           !Input
     &    VSTAGE, WTNCAN, WTNFX, WTNSD, WTNST, WTNUP,     !Input
     &    XLAI, RSTAGE, YREMRG, YRNR1, YRNR3, YRNR5,      !Input
     &    YRNR7, YRPLT, LINTW, LINTP,                     !Input
     &    SDWTAH)                                         !Output

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL FIND, ERROR, STNAMES, OPVIEW, READA_Dates, ROUND,
     &  CHANGE_DESC, GetDesc, SUMVALS, EvaluateDat, TIMDIF, READA_Y4K
      SAVE

      CHARACTER*1  RNMODE,IDETO,IPLTI, PLME
      CHARACTER*2  CROP
      CHARACTER*6  SECTION
      CHARACTER*6, PARAMETER :: ERRKEY = 'OPHARV'
      CHARACTER*10 STNAME(20)
      CHARACTER*12 FILEA, FMT
      CHARACTER*30 FILEIO
	    CHARACTER*80 PATHEX

      INTEGER ACOUNT, DFLR, DEMRG, DFPD, DFSD, DHRV
      INTEGER DNR8,DMAT,DNR0, DNR1,DNR3,DNR5,DNR7
      INTEGER DYNAMIC, ERRNUM, FOUND
      INTEGER IFLR, IEMRG, IFPD, IFSD, IHRV, IMAT, ISENS
      INTEGER LINC, LNUM, LUNIO, RUN, TIMDIF, TRTNUM, YIELD, YREMRG
      INTEGER YRNR1,YRNR3,YRNR5,YRNR7,MDATE,YRDOY, YRPLT,YRSIM
      INTEGER RSTAGE, iEYLDH
      INTEGER TRT_ROT
      INTEGER STGDOY(20)

      REAL BIOMAS, BWAH, CANHT, CANNAA, CANWAA, HI, HWAH, HWAM
      REAL LAIMX, PCLSD, PCNSD, PODWT, PODNO, PSDWT, PSPP
      REAL SDRATE, SDWT, SDWTAH, SEEDNO, EYLDH, ROUND, LINTW, LINTP
      REAL THRES, TOPWT, VSTAGE
      REAL WTNCAN, WTNFX, WTNSD, WTNST, WTNUP, XLAI
      REAL, DIMENSION(2) :: HARVFRAC

!     Arrays which contain data for printing in SUMMARY.OUT file
!       (OPSUM subroutine)
      INTEGER, PARAMETER :: SUMNUM = 19
      CHARACTER*5, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

!     Arrays which contain Simulated and Measured data for printing
!       in OVERVIEW.OUT and EVALUATE.OUT files (OPVIEW subroutine)
      CHARACTER*6, DIMENSION(EvaluateNum) :: OLAB, OLAP !OLAP in dap
      CHARACTER*12 X(EvaluateNum)
      CHARACTER*8 Simulated(EvaluateNum), Measured(EvaluateNum)
      CHARACTER*50 DESCRIP(EvaluateNum)

!     P module
      REAL PStres1, PStres2

!     Variables added for environmental and stress factors output
      REAL AGEFAC, NSTRES, SWFAC, TURFAC
      TYPE (PlStresType) PlantStres

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType) ISWITCH

!     Transfer values from constructed data types into local variables.
      DYNAMIC= CONTROL % DYNAMIC
      FILEIO = CONTROL % FILEIO
      LUNIO  = CONTROL % LUNIO
      RUN    = CONTROL % RUN
      RNMODE = CONTROL % RNMODE
      YRDOY  = CONTROL % YRDOY
      YRSIM  = CONTROL % YRSIM

      IDETO = ISWITCH % IDETO
      IPLTI = ISWITCH % IPLTI

      ACOUNT = 27  !Number of possible FILEA headers for this crop

!CHP 12/16/2004 Need to be able to read FILEA headers of either
!     'BWAM' or 'BWAH' and interpret data as 'BWAM'

!     Define headings for observed data file (FILEA)
      DATA OLAB / !Pred.             Obs.   Definition
                  !------------      -----  -----------
     & 'EDAT  ',  ! 1 EDAT                  Emergence date
     & 'ADAT  ',  ! 2 DNR1           DFLR   Anthesis date
     & 'PD1T  ',  ! 3 DNR3           DFPD   First Pod        
     & 'PDFT  ',  ! 4 DNR5           DFSD   First Seed       
     & 'MDAT  ',  ! 5 DNR7           DMAT   Physiological Maturity
     & 'R8AT  ',  ! 6 DNR8           DHRV   Harvest Maturity (dap)
     & 'HWAM  ',  ! 7 NINT(SDWT*10)  XGWT   Seed Yield (kg/ha;dry)
     & 'EYLDH ',  ! 8 EYLDH          EYLDH  Economic Yield
     & 'LINTP ',  ! 9 LINTP          LINTP  Percent Lint (%)
     & 'PWAM  ',  !10 NINT(PODWT*10) XPDW   Pod Yield (kg/ha;dry) 
     & 'CWAA  ',  !11 NINT(CANWAA*10)XCWAA  Biomass (kg/ha) at Anth
     & 'CWAM  ',  !12 NINT(TOPWT*10) XCWT   Biomass (kg/ha) Harv Mat
     & 'BWAM  ',  !13 (TOPWT-PODWT)*10 XSWT Tops - seed (kg/ha) @Mat
     & 'H#AM  ',  !14 NINT(SEEDNO)   XNOGR  Seed Number (Seed/m2)
     & 'HWUM  ',  !15 PSDWT          XGWU   Weight Per Seed (g;dry)
     & 'H#UM  ',  !16 PSPP           XNOGU  Seeds/Pod
     & 'HIAM  ',  !17 HI             XHIN   Harvest Index (kg/kg)
     & 'THAM  ',  !18 THRES          XTHR   Shelling Percentage (%)
     & 'LAIX  ',  !19 LAIMX          XLAM   Maximum LAI (m2/m2)
     & 'L#SM  ',  !20 VSTAGE         XLFNO  Final Leaf # Main Stem
     & 'CHTA  ',  !21 CANHT          XCNHT  Canopy Height (m)
     & 'CNAA  ',  !22                XCNAA  Biomass N @ anth (kg/ha)
     & 'CNAM  ',  !23 NINT(WTNCAN*10)XNTP   Biomass N (kg N/ha)
     & 'SNAM  ',  !24 NINT(WTNST*10) XNST   Stalk N (kg N/ha)
     & 'GNAM  ',  !25 NINT(WTNSD*10) XNGR   Seed N (kg N/ha)
     & 'GN%M  ',  !26 PCNSD          XNPS   Seed N (%)
     & 'GL%M  ',  !27 PCLSD          XLPS   Seed Lipid (%)
     & 13*'      '/
!     GWAH    !Grain weight at harvest (kg/ha)
!     CWAH    !Canopy weight at harvest (kg/ha)
!     FWAH    !Fruit weight at harvest (kg/ha)

!  9/14/2004 GH, JWW, CHP
!  Problem with PDFT header and corresponding variables DNR5 and DFSD.
!  The simulated variable, DNR5, corresponds to first seed.
!  The observed variable, DFSD, also seems to correspond to first seed.
!  But the label in DATA.CDE for header PDFT is full pod (which 
!     corresponds to R4 stage, not R5). 
!  The apparent fix is to change all PDFT headers in FILEA to R5AT.
!  The OLAB array element 3 would have to be changed also to R5AT.

!***********************************************************************
!***********************************************************************
!     RUN INITIALIZATION
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
C-----------------------------------------------------------------------
!     Read FILEIO
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)

      READ (LUNIO,'(55X,I5)',IOSTAT=ERRNUM) ISENS; LNUM = 1   
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
      READ (LUNIO,'(3(/),15X,A12,1X,A80)',IOSTAT=ERRNUM) FILEA,
     &     PATHEX
      LNUM = LNUM + 4
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)
  
      SECTION = '*TREAT'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ(LUNIO, '(I3)',IOSTAT=ERRNUM) TRTNUM ; LNUM = LNUM + 1
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)
      ENDIF

!     Find and Read Planting Details Section
      SECTION = '*PLANT'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) CALL ERROR (SECTION, 42, FILEIO,LNUM)
      READ(LUNIO,'(35X,A1)',IOSTAT=ERRNUM) PLME ; LNUM = LNUM + 1
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)

      CLOSE (LUNIO)

!     Assign names to stages based on crop.
      CALL STNAMES(CROP, PLME, STNAME)
      
!     Assign descriptions to Measured and Simulated data 
!         from DATA.CDE.
      CALL GETDESC(ACOUNT, OLAB, DESCRIP)
      OLAP = OLAB

      BIOMAS = -99.
      VSTAGE = -99.
      WTNCAN = -99.
      XLAI   = -99.
      YIELD  = -99.
      RSTAGE = -99.
      EYLDH  = -99.

      CALL OPVIEW(CONTROL, 
     &    BIOMAS, ACOUNT, DESCRIP, IDETO, VSTAGE, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, XLAI, YIELD, YRPLT, RSTAGE)

C***********************************************************************
C***********************************************************************
C     SEASONAL INITIALIZATION
C***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
      Simulated = ' '
      Measured  = ' '
      YIELD  = 0
      BIOMAS = 0.0

!     Establish #, names of stages for environmental & stress summary
      PlantStres % ACTIVE = .FALSE.
      PlantStres % StageName = '                       '
      SELECT CASE (CROP)
      CASE ('AM','BC','BG','BN','CH','CI','CN','CO','CP',
     &      'CU','FB','GB','GY','LT','PE','PN','PP',
     &      'PR','QU','SB','SF','SR','SU','TM','VB')
        PlantStres % NSTAGES = 4
        PlantStres % StageName(1)  = 'Emergence -First Flower'
        PlantStres % StageName(2)  = 'First Flower-First Seed'
        PlantStres % StageName(3)  = 'First Seed - Phys. Mat.'
        PlantStres % StageName(4)  = 'Emergence  - Phys. Mat.'

      CASE ('CB')
        PlantStres % NSTAGES = 0

      CASE ('BM','BH','BR','C3','C4','NP')
        PlantStres % NSTAGES = 1
        PlantStres % StageName(1)  = 'Emergence  - Phys. Mat.'

      CASE DEFAULT
        PlantStres % NSTAGES = 0
      END SELECT

      PlantStres % StageName(0) = 'Planting to Harvest    '

      CALL OPVIEW(CONTROL, 
     &    BIOMAS, ACOUNT, DESCRIP, IDETO, VSTAGE, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, XLAI, YIELD, YRPLT, RSTAGE)

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
C-----------------------------------------------------------------------
      BIOMAS = TOPWT*10.

      PlantStres % W_grow = TURFAC
      PlantStres % W_phot = SWFAC 
      PlantStres % N_grow = NSTRES
      PlantStres % N_phot = AGEFAC
      PlantStres % P_grow = PSTRES2
      PlantStres % P_phot = PSTRES1
      PlantStres % ACTIVE = .FALSE.

!     Set ACTIVE variable to indicate that current phase is active
      SELECT CASE (CROP)
      CASE ('AM','BC','BG','BN','CH','CI','CN','CO','CP',
     &      'CU','FB','GB','GY','LT','PE','PN','PP',
     &      'PR','QU','SB','SF','SR','SU','TM','VB')
        IF (YRDOY > STGDOY(1) .AND. YRDOY <= STGDOY(5)) THEN
          PlantStres % ACTIVE(1) = .TRUE.
        ENDIF

        IF (YRDOY > STGDOY(5) .AND. YRDOY <= STGDOY(8)) THEN
          PlantStres % ACTIVE(2) = .TRUE.
        ENDIF

        IF (YRDOY > STGDOY(8) .AND. YRDOY <= STGDOY(10)) THEN
          PlantStres % ACTIVE(3) = .TRUE.
        ENDIF

        IF (YRDOY > STGDOY(1) .AND. YRDOY <= STGDOY(10)) THEN
          PlantStres % ACTIVE(4) = .TRUE.
        ENDIF

      CASE ('CB')
        IF (YRDOY > STGDOY(15) .AND. YRDOY <= STGDOY(16)) THEN
          PlantStres % ACTIVE(1) = .TRUE.
        ENDIF

      CASE ('BM','BH','BR','NP')
        IF (YRDOY > STGDOY(1) .AND. YRDOY <= STGDOY(16)) THEN
          PlantStres % ACTIVE(1) = .TRUE.
        ENDIF
      END SELECT

      IF (YRDOY >= STGDOY(15) .AND. YRDOY <= STGDOY(16) 
     &      .AND. STGDOY(15) > -99) THEN
        PlantStres % ACTIVE(0) = .TRUE.
      ENDIF

!     Send data to Overview.out data on days where stages occur
      CALL OPVIEW(CONTROL, 
     &    BIOMAS, ACOUNT, DESCRIP, IDETO, VSTAGE, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, XLAI, YIELD, YRPLT, RSTAGE)

!***********************************************************************
!***********************************************************************
!     Seasonal Output 
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
!     Compute values to be sent to Overview, Summary and Evaluate files.
      IF (SEEDNO .GT. 1.E-4) THEN
         PSDWT = SDWT/SEEDNO
      ELSE
         PSDWT = 0.0
      ENDIF

      IF (PODNO .GT. 1.E-4) THEN
         PSPP = SEEDNO/PODNO
      ELSE
         PSPP  = 0.
      ENDIF

      IF (PODWT .GT. 0.1) THEN
         THRES = SDWT*100./PODWT
      ELSE
         THRES = 0.0
      ENDIF
      THRES = MIN(THRES,99.99)

      IF (TOPWT .GT. 1.E-4 .AND. SDWT .GE. 1.E-4) THEN
         HI = SDWT/TOPWT
      ELSE
         HI = 0.
      ENDIF

      IF (CROP .EQ. 'FA') YRPLT = -99
      
      ! 2024-06-20 FO - Economic Yield for Cotton.
      IF(CROP .EQ. 'CO') THEN
        ! Units from g/m2 to ton/ha
        EYLDH = LINTW / 100
      ENDIF
      
      ! 2024-07-11 FO - Economic standard output format
      IF    (EYLDH < 0.999) THEN; FMT = '(F8.3)'
      ELSEIF(EYLDH < 10.0)  THEN; FMT = '(F8.2)'
      ELSEIF(EYLDH < 100.0) THEN; FMT = '(F8.1)'
      ELSEIF(EYLDH < 1000.0)THEN
        iEYLDH = INT(EYLDH)
        FMT = '(I8)'
      ELSEIF(EYLDH < 10000.0)THEN
        EYLDH = ROUND(EYLDH, -1)
        iEYLDH = INT(EYLDH)
        FMT = '(I8)'
      ELSEIF(EYLDH < 100000.0)THEN
        EYLDH = ROUND(EYLDH, -2)
        iEYLDH = INT(EYLDH)
        FMT = '(I8)'
      ELSE
        EYLDH = ROUND(EYLDH, -2)
        iEYLDH = INT(EYLDH)
        FMT = '(I8)'
      ENDIF
!-----------------------------------------------------------------------
!     Read Measured (measured) data from FILEA
!-----------------------------------------------------------------------
      IF ((INDEX('YE',IDETO) > 0 .OR. INDEX('IAEBCGDT',RNMODE) .GT. 0) 
     &  .OR. (INDEX('AY',ISWITCH%IDETS) .GT. 0 .AND. CROP .NE.'FA'))THEN
         IF (INDEX('FQ',RNMODE) > 0) THEN
           TRT_ROT = CONTROL % ROTNUM
         ELSE
           TRT_ROT = TRTNUM
         ENDIF
         !CALL READA (FILEA, PATHEX,OLAB, TRT_ROT, YRSIM, X)
         CALL READA_Y4K(FILEA, PATHEX,OLAB, TRT_ROT, YRSIM, X)

!     Convert from YRDOY format to DAP.  Change descriptions to match.
!       Anthesis
        CALL READA_Dates(X(2), YRSIM, IFLR)  !X is text, IFLR is integer
        IF (IFLR .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DFLR = TIMDIF(YRPLT,IFLR)   !dap
        ELSE
          DFLR  = -99
        ENDIF
        !CALL CHANGE_DESC(DESCRIP(1)) 
        OLAP(2) = 'ADAP  '
        CALL GetDesc(1,OLAP(2), DESCRIP(2))

!       First pod
        CALL READA_Dates(X(3), YRSIM, IFPD)
        IF (IFPD .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DFPD = TIMDIF(YRPLT,IFPD)
        ELSE
          DFPD  = -99
        ENDIF
        !CALL CHANGE_DESC(DESCRIP(2)) 
        OLAP(3) = 'PD1P  '
        CALL GetDesc(1,OLAP(3), DESCRIP(3))

!       First seed
        CALL READA_Dates(X(4), YRSIM, IFSD)
        IF (IFSD .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DFSD = TIMDIF(YRPLT,IFSD)
        ELSE
          DFSD  = -99
        ENDIF
        !CALL CHANGE_DESC(DESCRIP(3))
        OLAP(4) = 'PDFP  '
        CALL GetDesc(1,OLAP(4), DESCRIP(4))

!       Physiological maturity
        CALL READA_Dates(X(5), YRSIM, IMAT)
        IF (IMAT .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DMAT = TIMDIF(YRPLT,IMAT)
        ELSE
          DMAT  = -99
        ENDIF
        !CALL CHANGE_DESC(DESCRIP(4)) 
        OLAP(5) = 'MDAP  '
        CALL GetDesc(1,OLAP(5), DESCRIP(5))

!       Harvest maturity
        CALL READA_Dates(X(6), YRSIM, IHRV)
        IF (IHRV .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DHRV = TIMDIF(YRPLT,IHRV)
        ELSE
          DHRV  = -99
        ENDIF
        !CALL CHANGE_DESC(DESCRIP(24)) 
        OLAP(6) = 'R8AP  '
        CALL GetDesc(1,OLAP(6), DESCRIP(6))

!       Emergence date
!       08/28/2009 CHP added EDAT, EDAP 
        CALL READA_Dates(X(1), YRSIM, IEMRG)  
        IF (IEMRG .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DEMRG = TIMDIF(YRPLT,IEMRG)   !dap
        ELSE
          DEMRG  = -99
        ENDIF
        OLAP(1) = 'EDAP  '
        CALL GetDesc(1,OLAP(1), DESCRIP(1))

        DNR1 = TIMDIF(YRPLT,YRNR1)
        IF (DNR1 .LE. 0) THEN
          DNR1 = -99
          YRNR1 = -99
        ENDIF

        DNR3 = TIMDIF(YRPLT,YRNR3)
        IF (DNR3 .LE. 0) THEN
          DNR3 = -99
          YRNR3 = -99
        ENDIF

        DNR5 = TIMDIF(YRPLT,YRNR5)
        IF (DNR5 .LE. 0) THEN
          DNR5 = -99
          YRNR5 = -99
        ENDIF

        DNR7 = TIMDIF(YRPLT,YRNR7)
        IF (DNR7 .LE. 0) THEN
          DNR7 = -99
          YRNR7 = -99
        ENDIF

        DNR8 = TIMDIF(YRPLT,MDATE)
        IF (DNR8 .LE. 0 .OR. YRPLT .LE. 0) THEN
          DNR8 = -99
          MDATE = -99
        ENDIF

        DNR0 = TIMDIF(YRPLT,YREMRG)
        IF (DNR0 .LE. 0 .OR. YRPLT .LE. 0) THEN
          DNR0 = -99
          YREMRG = -99
        ENDIF

!-----------------------------------------------------------------------
!     Store Simulated and Measured data for this season.
      WRITE(Simulated(1),' (I8)') DNR0;  WRITE(Measured(1),'(I8)') DEMRG
      WRITE(Simulated(2),' (I8)') DNR1;  WRITE(Measured(2),'(I8)') DFLR
      WRITE(Simulated(3),' (I8)') DNR3;  WRITE(Measured(3),'(I8)') DFPD
      WRITE(Simulated(4),' (I8)') DNR5;  WRITE(Measured(4),'(I8)') DFSD
      WRITE(Simulated(5),' (I8)') DNR7;  WRITE(Measured(5),'(I8)') DMAT
      WRITE(Simulated(6),' (I8)') DNR8;  WRITE(Measured(6),'(I8)') DHRV
      WRITE(Simulated(7),' (I8)') NINT(SDWT*10);  
                                  WRITE(Measured(7),'(A8)') TRIM(X(7))
      IF(EYLDH < 100.0) THEN
        WRITE(Simulated(8),FMT) EYLDH; 
                                  WRITE(Measured(8),'(A8)')TRIM(X(8))
      ELSE
        WRITE(Simulated(8),FMT) iEYLDH; 
                                  WRITE(Measured(8),'(A8)')TRIM(X(8))
      ENDIF
      IF(CROP .EQ. 'CO') THEN
        WRITE(Simulated(9),'(F8.1)') LINTP; 
                                  WRITE(Measured(9),'(A8)')TRIM(X(9))
      ENDIF
      WRITE(Simulated(10),' (I8)') NINT(PODWT*10); 
                                  WRITE(Measured(10),'(A8)') TRIM(X(10))
      WRITE(Simulated(11), '(I8)') NINT(CANWAA*10);
                                  WRITE(Measured(11),'(A8)')TRIM(X(11))
      WRITE(Simulated(12),'(I8)') NINT(TOPWT*10); 
                                  WRITE(Measured(12),'(A8)')TRIM(X(12))
      WRITE(Simulated(13),'(I8)') NINT(TOPWT-SDWT)*10; 
                                  WRITE(Measured(13),'(A8)')TRIM(X(13))
      WRITE(Simulated(14),'(I8)') NINT(SEEDNO);   
                                  WRITE(Measured(14),'(A8)')TRIM(X(14))
      WRITE(Simulated(15),'(F8.4)')PSDWT;
                                  WRITE(Measured(15),'(A8)')TRIM(X(15))
      WRITE(Simulated(16),'(F8.2)')PSPP; 
                                  WRITE(Measured(16),'(A8)')TRIM(X(16))
      WRITE(Simulated(17),'(F8.3)')HI;   
                                  WRITE(Measured(17),'(A8)')TRIM(X(17))
      WRITE(Simulated(18),'(F8.2)')THRES;
                                  WRITE(Measured(18),'(A8)')TRIM(X(18))
      WRITE(Simulated(19),'(F8.2)')LAIMX;
                                  WRITE(Measured(19),'(A8)')TRIM(X(19))
      WRITE(Simulated(20),'(F8.2)')VSTAGE;
                                  WRITE(Measured(20),'(A8)')TRIM(X(20))
      WRITE(Simulated(21),'(F8.2)')CANHT;
                                  WRITE(Measured(21),'(A8)')TRIM(X(21))
      WRITE(Simulated(22),'(I8)') NINT(CANNAA*10);
                                  WRITE(Measured(22),'(A8)')TRIM(X(22))
      WRITE(Simulated(23),'(I8)') NINT(WTNCAN*10);
                                  WRITE(Measured(23),'(A8)')TRIM(X(23))
      WRITE(Simulated(24),'(I8)') NINT(WTNST*10); 
                                  WRITE(Measured(24),'(A8)')TRIM(X(24))
      WRITE(Simulated(25),'(I8)') NINT(WTNSD*10); 
                                  WRITE(Measured(25),'(A8)')TRIM(X(25))
      WRITE(Simulated(26),'(F8.2)')PCNSD;
                                  WRITE(Measured(26),'(A8)')TRIM(X(26))
      WRITE(Simulated(27),'(F8.2)')PCLSD;
                                  WRITE(Measured(27),'(A8)')TRIM(X(27))
      ENDIF  

      IF (CONTROL % ERRCODE > 0) THEN
!       Simulation terminated abnormally, computed values are meaningless
        SIMULATED = '     -99'
      ENDIF

!-----------------------------------------------------------------------
!     Send data to OPSUM for SUMMARY.OUT file.
!-----------------------------------------------------------------------
!     Compute values to be sent to OPSUM for SUMMARY.OUT file.

C     Actual yield harvested (default is 100 %)
      SDWTAH = SDWT * HARVFRAC(1)
!      SDWTAH = SDWT * HPC(1)/100.
!     Let OPSUM multiply by HPC - not available here.
!      SDWTAH = SDWT

C     Actual byproduct harvested (default is 0 %)
C     Byproduct not harvested is incorporated
!      BWAH   = (TOPWT - SDWT)
!      BWAH   = (TOPWT - SDWT) * HBPC(1)/100.
!08/11/2005 GH/CHP
!      BWAH   = (TOPWT - SDWT) * HARVFRAC(2)
!     By-product, for use in Summary.OUT is the by-product harvested 
!     for its economic value -- generally the stalks.
      BWAH   = (TOPWT - PODWT) * HARVFRAC(2)

      IF (SEEDNO .GT. 1.E-4) THEN
         PSDWT = SDWT/SEEDNO
      ELSE
         PSDWT = 0.0
      ENDIF

      IF (PODNO .GT. 1.E-4) THEN
         PSPP = SEEDNO/PODNO
      ELSE
         PSPP  = 0.
      ENDIF

      IF ((CROP .EQ. 'TM') .OR. (CROP .EQ. 'PR') .OR. 
     &    (CROP .EQ. 'CU') .OR. (CROP .EQ. 'SR')) THEN
        HWAM = PODWT * 10.
        HWAH = PODWT * 10.
      ELSE
!chp 2/3/05 per GH        HWAM = SDWTAM * 10.
        HWAM = SDWT * 10.
        HWAH = SDWTAH * 10.
      ENDIF

!      IF (IDETS .EQ. 'Y' .OR. IDETS .EQ. 'A') THEN
!       Store Summary.out labels and values in arrays to send to
!       OPSUM routines for printing.  Integers are temporarily 
!       saved as real numbers for placement in real array.
        LABEL(1)  = 'ADAT'; VALUE(1)  = FLOAT(YRNR1)
        LABEL(2)  = 'MDAT'; VALUE(2)  = FLOAT(YRNR7)
        LABEL(3)  = 'DWAP'; VALUE(3)  = SDRATE
        LABEL(4)  = 'CWAM'; VALUE(4)  = TOPWT*10.
        LABEL(5)  = 'HWAM'; VALUE(5)  = HWAM
        LABEL(6)  = 'HWAH'; VALUE(6)  = HWAH
        LABEL(7)  = 'BWAH'; VALUE(7)  = BWAH
        LABEL(8)  = 'HWUM'; VALUE(8)  = PSDWT     !*1000.
        LABEL(9)  = 'H#AM'; VALUE(9)  = SEEDNO
        LABEL(10) = 'H#UM'; VALUE(10) = PSPP
        LABEL(11) = 'NFXM'; VALUE(11) = WTNFX*10.
        LABEL(12) = 'NUCM'; VALUE(12) = WTNUP*10.
        LABEL(13) = 'CNAM'; VALUE(13) = WTNCAN*10.
        LABEL(14) = 'GNAM'; VALUE(14) = WTNSD*10.
        LABEL(15) = 'PWAM'; VALUE(15) = PODWT * 10.
        LABEL(16) = 'LAIX'; VALUE(16) = LAIMX
        LABEL(17) = 'HIAM'; VALUE(17) = HI
        LABEL(18) = 'EDAT'; VALUE(18) = FLOAT(YREMRG)
        LABEL(19) = 'EYLDH'; VALUE(19) = EYLDH

        !Send labels and values to OPSUM
        CALL SUMVALS (SUMNUM, LABEL, VALUE) 
!      ENDIF

!-----------------------------------------------------------------------
!     Call Overview.out routine
      BIOMAS = TOPWT*10.
!      YIELD  = NINT(HWAM)
      YIELD  = NINT(HWAH)     !12/10/2003
      
      CALL OPVIEW(CONTROL, 
     &    BIOMAS, ACOUNT, DESCRIP, IDETO, VSTAGE, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, XLAI, YIELD, YRPLT, RSTAGE)

!-----------------------------------------------------------------------
      !Send Measured and Simulated datat to OPSUM
      IF(INDEX('YE',IDETO) > 0 .OR. INDEX('IAEBCGD',RNMODE) .GT. 0) THEN
        CALL EvaluateDat (ACOUNT, Measured, Simulated, DESCRIP, OLAP) 
      ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OPHARV
C=======================================================================



C=======================================================================
C  STNAMES, Subroutine C.H.Porter
C  Assigns STNAME for various CROPGRO crops.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/01/2001 CHP Written
C  08/28/2020 GH Added quinoa
C=======================================================================

      SUBROUTINE STNAMES(CROP, PLME, STNAME)

C-----------------------------------------------------------------------
      IMPLICIT NONE

      CHARACTER*1  PLME
      CHARACTER*2  CROP 
      CHARACTER*10 STNAME(20)
      INTEGER I

C-----------------------------------------------------------------------
C     Define names of reproductive phases
C-----------------------------------------------------------------------
      DO I = 1,20
         STNAME(I) = '          '
      ENDDO

      SELECT CASE (CROP)
      CASE ('AM','BC','BN','CH','CI','CN','CP','CU','FB','GB','GY',
     &      'PE','PP','PR','SB','SR','TM','VB','LT')
!     For stage-dependant irrigation - send GSTAGE back to irrig routine
        STNAME(1) = 'Emergence '    !; GSTAGE(1) = "GS001"
        STNAME(2) = 'Unifoliate'
        STNAME(3) = 'End Juven.'
        STNAME(4) = 'Flower Ind'
        STNAME(5) = 'First Flwr'
        STNAME(6) = 'First Pod '
        STNAME(7) = 'First Pod '
        STNAME(8) = 'First Seed'
        STNAME(9) = 'End Pod   '
        STNAME(10)= 'Phys. Mat '
        STNAME(11)= 'Harv. Mat '
        STNAME(12)= 'End Msnode'
        STNAME(13)= 'End Leaf  '
        STNAME(14)= 'Start Sim '
        STNAME(15)= 'Sowing    '
        STNAME(16)= 'Harvest   '

!KJB - 5/4/2017 adding stages for sunflower and safflower (maybe CROPGRO wheat later)
      CASE ('SU','SF')
!     For stage-dependant irrigation - send GSTAGE back to irrig routine
        STNAME(1) = 'Emergence '    !; GSTAGE(1) = "GS001"
        STNAME(2) = 'Unifoliate'
        STNAME(3) = 'End Juven.'
        STNAME(4) = 'Flower Ind'
        STNAME(5) = 'Star burst'
        STNAME(6) = '1st thalam'
        STNAME(7) = '1st thalam'
        STNAME(8) = '1st Flw/Sd'
        STNAME(9) = 'End Sd-add'
        STNAME(10)= 'Phys. Mat '
        STNAME(11)= 'Harv. Mat '
        STNAME(12)= 'End Msnode'
        STNAME(13)= 'End Leaf  '
        STNAME(14)= 'Start Sim '
        STNAME(15)= 'Sowing    '
        STNAME(16)= 'Harvest   '

! GH
! KJB - 7/27/2020 adding stages for quinoa
! KJB very similar to sunflower, but do not like "thalam"
      CASE ('QU')
!     For stage-dependant irrigation - send GSTAGE back to irrig routine
        STNAME(1) = 'Emergence '    !; GSTAGE(1) = "GS001"
        STNAME(2) = 'Unifoliate'
        STNAME(3) = 'End Juven.'
        STNAME(4) = 'Flower Ind'
        STNAME(5) = 'Repr. bud'
        STNAME(6) = '1st inflor'
        STNAME(7) = '1st inflor'
        STNAME(8) = '1st Flw/Sd'
        STNAME(9) = 'End Sd-add'
        STNAME(10)= 'Phys. Mat '
        STNAME(11)= 'Harv. Mat '
        STNAME(12)= 'End Msnode'
        STNAME(13)= 'End Leaf  '
        STNAME(14)= 'Start Sim '
        STNAME(15)= 'Sowing    '
        STNAME(16)= 'Harvest   '
        
! KJB
      CASE ('BG','PN')
        STNAME(1) = 'Emergence '
        STNAME(2) = 'Unifoliate'
        STNAME(3) = 'End Juven.'
        STNAME(4) = 'Flower Ind'
        STNAME(5) = 'First Flwr'
        STNAME(6) = 'First Peg '
        STNAME(7) = 'First Pod '
        STNAME(8) = 'First Seed'
        STNAME(9) = 'End Pod   '
        STNAME(10)= 'Phys. Mat '
        STNAME(11)= 'Harv. Mat '
        STNAME(12)= 'End Msnode'
        STNAME(13)= 'End Leaf  '
        STNAME(14)= 'Start Sim '
        STNAME(15)= 'Sowing    '
        STNAME(16)= 'Harvest   '

      CASE ('CB')
        STNAME( 1) = 'Emergence '
        STNAME( 2) = 'Unifoliate'
        STNAME( 3) = 'End Juven.'
        STNAME( 4) = '          '
        STNAME( 5) = '          '
        STNAME( 6) = 'First Head'
        STNAME( 7) = 'Full Head '
        STNAME( 8) = '          '
        STNAME( 9) = '          '
        STNAME(10) = 'Phys. Mat '
        STNAME(11) = 'Harv. Mat '
        STNAME(12) = 'End Msnode'
        STNAME(13) = 'End Leaf  '
        STNAME(14) = 'Start Sim '
        STNAME(15) = 'Sowing    '
        STNAME(16) = 'Harvest   '

      CASE ('BM','BH','BR','NP')
         STNAME( 1) = 'Emergence '
         STNAME( 2) = 'First Leaf'
         STNAME( 3) = 'End Juven.'
         STNAME( 4) = 'Flower Ind'
         STNAME( 5) = 'Flowering '
         STNAME(10) = 'Phys. Mat '
         STNAME(11) = 'Harv. Mat '
         STNAME(12) = 'End Msnode'
         STNAME(13) = 'End Leaf  '
         STNAME(14) = 'Start Sim '
         STNAME(15) = 'Sowing    '
         STNAME(16) = 'Harvest   '

      CASE ('CO') 
        STNAME( 1) = 'Emergence '
        STNAME( 2) = 'First Leaf'
        STNAME( 3) = 'End Juven.'
        STNAME( 4) = 'Flower Ind'
        STNAME( 5) = 'Flowering '
        STNAME( 6) = 'Boll > 6mm'
        STNAME( 7) = 'End Flower'
        STNAME( 8) = 'First Seed'
        STNAME( 9) = 'Bolls>.5sz'
        STNAME(10) = 'Cracked Bl'
        STNAME(11) = '90%Open Bl'
        STNAME(12) = 'End Msnode'
        STNAME(13) = 'End Leaf  '
        STNAME(14) = 'Start Sim '
        STNAME(15) = 'Sowing    '
        STNAME(16) = 'Harvest   '                

      CASE ('FA')
        STNAME(14)= 'Start Sim '
        STNAME(16)= 'End Sim   '

      END SELECT

      IF (PLME .EQ. 'T') THEN
        STNAME(15) = 'Transplant'
      ENDIF

      RETURN
      END SUBROUTINE STNAMES
C=======================================================================

