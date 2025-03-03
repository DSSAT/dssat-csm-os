C=======================================================================
C  PT_OPHARV, Subroutine G. Hoogenboom, J. W. Jones
C  Generates output for seasonal data for potato.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1990 GH  Written
C  11/02/1999 CHP Changed TSOC to THUMC, TSIN to THUMN, AMTRES to CUMRES 
C  07/01/2000 GH  Eliminated common block statements
C  02/11/2002 CHP Modified for modular potato model.
C  03/03/2002 GH  Modified logic for reading of fileA
C  08/12/2003 CHP Added I/O error checking and changed call to READA
!  05/03/2004 CHP Added P stresses to OPVIEW call 
!  08/11/2005 CHP Use BWAH as byproduct variable for Summary.OUT 
!                 (byproduct harvested), BWAM as byproduct variable 
!                 for Overview.OUT (byproduct produced to maturity)
!  10/24/2005 CHP Added environmental & stress factors to Overview.OUT
C  02/09/2007 GH  Added path for FileA
!  08/28/2009 CHP added EDAT, EDAP 
C=======================================================================

      SUBROUTINE PT_OPHARV(CONTROL, ISWITCH, 
     &    AGEFAC, APTNUP, BIOMAS, GNUP, HARVFRAC, ISDATE, !Input
     &    ISTAGE, MAXLAI, MDATE, NSTRES, PLTPOP, SDWT,    !Input
     &    SDWTPL, SEEDNO, STGDOY, STOVWT, SWFAC, TOTNUP,  !Input
     &    TUBN, TUBWT, TURFAC, WTNCAN, WTNUP, XLAI,       !Input
     &    YIELD, YRPLT,                                   !Input
     &    BWAH, SDWTAH, WTNSD)                            !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL FIND, ERROR, OPVIEW, READA, READA_Dates, GetDesc, 
     &  SUMVALS, EvaluateDat, TIMDIF, READA_Y4K
      SAVE

      CHARACTER*1  IDETO, IPLTI, RNMODE
      CHARACTER*2  CROP
      CHARACTER*6  SECTION
!      CHARACTER*6  XPDW,XGWT,XTHR,XGWU,XNOGR,XNOGU,
!     &             XLAM,XCWT,XSWT,XHIN,XNPS,XNTP,XNST,XNGR,XCWAA,XCNAA,
!     &             XLFNO, XCNHT, XLPS
      CHARACTER*6, PARAMETER :: ERRKEY = 'OPHARV'
      CHARACTER*10 STNAME(20)
      CHARACTER*12 FILEA
      CHARACTER*30 FILEIO
      CHARACTER*80 PATHEX

      INTEGER ACOUNT, DFLR, DMAT
      INTEGER DEMRG, DNR0, DNR1, DNR7, DYNAMIC, ERRNUM, FOUND
!      INTEGER IFLR, IFSD, IFPD, IHRV, IMAT
      INTEGER IEMRG, IFLR, IMAT
      INTEGER ISDATE, ISENS, ISTAGE, LINC, LNUM, LUNIO, MDATE, RUN
      INTEGER TIMDIF, TRTNUM, YRNR1, YRNR2, YRNR3
      INTEGER YRDOY, YREMRG, YRNR5, YRNR7, YRSIM, YRPLT
      INTEGER TRT_ROT
      INTEGER STGDOY(20)
      
      REAL AGEFAC, APTNUP, BIOMAS, BWAH, CTPP, GNUP, GPP
      REAL HAULM, HI
      REAL LeafNo, MAXLAI, NSTRES, PBIOMS, PLTPOP, PSDWT, PTUBNP
      REAL Pstres1, Pstres2   
      REAL SDWT, SDWTAH, SDWTAM, SDWTPL, SEEDNO, STOVER, STOVWT
      REAL SWFAC, TOTNUP, TUBN, TUBNUP, TUBSM, TUBWT, TURFAC
      REAL WTNCAN, WTNFX, WTNSD, WTNUP, XLAI
      REAL YIELD, YIELDB, YLDFR 

      REAL, DIMENSION(2) :: HARVFRAC

!     Arrays which contain data for printing in SUMMARY.OUT file
!       (OPSUM subroutine)
      INTEGER, PARAMETER :: SUMNUM = 18
      CHARACTER*4, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

!     Arrays which contain predicted and Measured data for printing
!       in OVERVIEW.OUT and EVALUATE.OUT files (OPVIEW subroutine)
      CHARACTER*6, DIMENSION(EvaluateNum) :: OLAB, OLAP !OLAP in dap
      CHARACTER*12 X(EvaluateNum)
      CHARACTER*8 Simulated(EvaluateNum), Measured(EvaluateNum)
      CHARACTER*50 DESCRIP(EvaluateNum)

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL
      TYPE (SwitchType) ISWITCH

!     Variables added for environmental and stress factors output
      TYPE (PlStresType) PlantStres

!     Transfer values from constructed data types into local variables.
      CROP   = CONTROL % CROP 
      DYNAMIC= CONTROL % DYNAMIC
      FILEIO = CONTROL % FILEIO
      LUNIO  = CONTROL % LUNIO
      RUN    = CONTROL % RUN
      RNMODE = CONTROL % RNMODE
      YRDOY  = CONTROL % YRDOY
      YRSIM  = CONTROL % YRSIM

      IDETO = ISWITCH % IDETO
      IPLTI = ISWITCH % IPLTI

      ACOUNT = 22  !Number of possible FILEA headers for this crop
      DATA OLAB /
     &    'TDAT  ',     ! 1 BEGIN TUBER GROWTH (dap)
     &    'PD1T  ',     ! 2
     &    'PDFT  ',     ! 3
     &    'HDAT  ',     ! 4 PHYSIOL. MATURITY (dap)
     &    'UWAH  ',     ! 5 TUBER DRY YIELD (kg/ha)
     &    'PWAM  ',     ! 6
     &    'H#AM  ',     ! 7
     &    'UYAH  ',     ! 8 TUBER FRESH YIELD (Mg/ha)
     &    'H#UM  ',     ! 9
     &    'TWAH  ',     !10 TUBER+TOP (kg/ha) HARVEST

!        08/11/2005 Change BWAH to BWAM - byproduct produced to maturity
!     &    'BWAH',     !11 TOP WT. (kg/ha) AT HARVEST 
     &    'BWAM  ',     !11 TOP WT. (kg/ha) AT HARVEST 

     &    'LAIX  ',     !12 MAXIMUM LAI (m2/m2)
     &    'HIAM  ',     !13
     &    'THAM  ',     !14
     &    'UNAM  ',     !15 TUBER N UPTAKE (kg N/ha)
     &    'TNAH  ',     !16 TOTAL N UPTAKE (kg N/ha)
     &    'CNAM  ',     !17 TOPS N UPTAKE (kg N/ha)
     &    'UN%H  ',     !18 TUBER N (%) AT HARVEST
     &    'CWAA  ',     !19
     &    'CNAA  ',     !20
     &    'L#SM  ',     !21
     &    'EDAT  ',     !22 Emergence date
     &    18*'      '/

      DATA STNAME/
     &    'Beg Tuber ',   !Stage 1
     &    'Maturity  ',   !Stage 2
     &    '          ',   !Stage 3
     1    '          ',   !Stage 4
     &    'Sowing    ',   !Stage 5
     &    'Germinat. ',   !Stage 6
     2    'Emergence ',   !Stage 7
     &    '          ',   !Stage 8
     &    '          ',   !Stage 9
     3    '          ',   !Stage 10
     &    '          ',   !Stage 11
     &    '          ',   !Stage 12
     4    '          ',   !Stage 13
     &    'Start Sim ',   !Stage 14
     &    '          ',   !Stage 15
     5    '          ',   !Stage 16
     &    '          ',   !Stage 17
     &    '          ',   !Stage 18
     6    '          ',   !Stage 19
     &    'Harvest   '/   !Stage 20

!***********************************************************************
!***********************************************************************
!     RUN INITIALIZATION
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
C-----------------------------------------------------------------------
!       Read FILEIO
        OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)

        READ (LUNIO,'(55X,I5)', IOSTAT=ERRNUM) ISENS; LNUM = 1
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)

        READ (LUNIO,'(3(/),15X,A12,1X,A80)', IOSTAT=ERRNUM) FILEA,
     &        PATHEX
        LNUM = LNUM + 4  
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
  
        SECTION = '*TREAT'
        CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILEIO, LNUM)
        ELSE
          READ(LUNIO, '(I3)', IOSTAT=ERRNUM) TRTNUM ; LNUM = LNUM + 1
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
        ENDIF

        CLOSE (LUNIO)

!     Assign descriptions to Measured and Simulated data 
!         from DATA.CDE.
      CALL GETDESC(ACOUNT, OLAB, DESCRIP)
      OLAP = OLAB

      Pstres1 = 1.0
      Pstres2 = 1.0
      
      CALL OPVIEW(CONTROL, 
     &    BIOMAS*10., ACOUNT, DESCRIP, IDETO, LeafNo, 
     &    Measured, PlantStres, Simulated, STGDOY, STNAME, 
     &    WTNCAN*10., XLAI, NINT(YIELD), YRPLT, ISTAGE)

C***********************************************************************
C***********************************************************************
C     SEASONAL INITIALIZATION
C***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
      Simulated = ' '
      Measured  = ' '
      YIELD  = 0.0
      BIOMAS = 0.0
      LeafNo = 0.0
      
!     Establish # and names of stages for environmental stress summary
      PlantStres % ACTIVE = .FALSE.
      PlantStres % StageName = '                       '
      PlantStres % NSTAGES = 2
      PlantStres % StageName(0) = 'Planting to Harvest    '
      PlantStres % StageName(1) = 'Emergence-Begin Tuber  '
      PlantStres % StageName(2) = 'Begin Tuber-Maturity   '

      CALL OPVIEW(CONTROL, 
     &    BIOMAS*10., ACOUNT, DESCRIP, IDETO, LeafNo, 
     &    Measured, PlantStres, Simulated, STGDOY, STNAME, 
     &    WTNCAN*10., XLAI, NINT(YIELD), YRPLT, ISTAGE)

C***********************************************************************
C***********************************************************************
C     DAILY OUTPUT
C***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
C-----------------------------------------------------------------------
      PlantStres % W_grow = TURFAC
      PlantStres % W_phot = SWFAC 
      PlantStres % N_grow = AGEFAC
      PlantStres % N_phot = NSTRES
      PlantStres % P_grow = PSTRES2
      PlantStres % P_phot = PSTRES1
      PlantStres % ACTIVE = .FALSE.

      IF (ISTAGE > 0 .AND. ISTAGE < 3) THEN
        PlantStres % ACTIVE(ISTAGE) = .TRUE.
      ENDIF

      IF (YRDOY >= YRPLT) THEN
        IF (MDATE < 0 .OR.
     &     (MDATE > 0 .AND. YRDOY < MDATE)) THEN
          PlantStres % ACTIVE(0) = .TRUE.
        ENDIF
      ENDIF

!     Send data to Overview.out data on days where stages occur
      CALL OPVIEW(CONTROL, 
     &    BIOMAS*10., ACOUNT, DESCRIP, IDETO, LeafNo, 
     &    Measured, PlantStres, Simulated, STGDOY, STNAME, 
     &    WTNCAN*10., XLAI, NINT(YIELD), YRPLT, ISTAGE)

C***********************************************************************
C***********************************************************************
C     Seasonal Output 
C***********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
!     Transfer dates for potato model stages.
      YRNR1  = ISDATE
      YRNR2  = STGDOY(2)
      YRNR3  = STGDOY(3)
      YRNR5  = STGDOY(5)
      IF (YRPLT .GT. 0) THEN
        YRNR7  = MDATE
      ELSE
        YRNR7  = -99
      ENDIF

C-----------------------------------------------------------------------
C     Calculate variables for output
C     update nitrogen and residue applications after routines have been
C     modified to handle automatic management
C-----------------------------------------------------------------------
      IF (SEEDNO .GT. 0.0) THEN
         PSDWT = SDWT/SEEDNO
      ELSE
         PSDWT = 0.0
      ENDIF

C-----------------------------------------------------------------------
C    Actual byproduct harvested (default is 0 %)
C    Byproduct not harvested is incorporated
C-----------------------------------------------------------------------
      STOVER = STOVWT*100.0

      BWAH   = STOVER * HARVFRAC(2)

      WTNSD  = GNUP  /10.0
!      TOTNUP = WTNUP
      IF (BIOMAS .GT. 0.0 .AND. YIELD .GE. 0.0) THEN
        HI = YIELD/(BIOMAS*10.0)
      ELSE
        HI = 0.0
      ENDIF

!      SDWT   = YIELD / 10.0
      SDWTAM = YIELD / 10.0

C-----------------------------------------------------------------------
C    Actual yield harvested (default is 100 %)
C-----------------------------------------------------------------------
!     SDWTAH - multiply by HPC in OPSUM - not available here.      
!     SDWTAH = SDWT * HPC(1)/100.0
      SDWTAH = YIELD / 10.0 * HARVFRAC(1)
!      SDWTAH = YIELD / 10.0 * HPC(1)/100.0

      PBIOMS = BIOMAS * 10.0

      YLDFR  = (YIELD/1000.)/0.2
!      HAULM  = BIOMAS*10. * PLANTS
      HAULM  = BIOMAS*10. * PLTPOP    !CHP

      IF (TUBWT .GT. 0.0) THEN
        PTUBNP = TUBN*100. / TUBWT
      ELSE
        PTUBNP = 0.0
      ENDIF

!      TUBNUP = TUBN*10.0 * PLANTS
      TUBNUP = TUBN*10.0 * PLTPOP     !CHP
!      TOTNUP = TUBNUP    + APTNUP
      TUBSM  = 0.0
      CTPP   = 0.0

C-----------------------------------------------------------------------
      IF ((INDEX('YE',IDETO) > 0 .OR. INDEX('IAEBCGDT',RNMODE) .GT. 0) 
     &  .OR. (INDEX('AY',ISWITCH%IDETS) .GT. 0 .AND. CROP .NE.'FA'))THEN
         IF (INDEX('FQ',RNMODE) > 0) THEN
           TRT_ROT = CONTROL % ROTNUM
         ELSE
           TRT_ROT = TRTNUM
         ENDIF
         !CALL READA (FILEA, PATHEX,OLAB, TRT_ROT, YRSIM, X)
         CALL READA_Y4K(FILEA, PATHEX,OLAB, TRT_ROT, YRSIM, X)

!       Convert from YRDOY format to DAP.  Change descriptions to match.
        CALL READA_Dates(X(1), YRSIM, IFLR)
        IF (IFLR .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DFLR = TIMDIF (YRPLT,IFLR)
        ELSE
          DFLR  = -99
        ENDIF
        OLAP(1) = 'TDAP  '
        CALL GetDesc(1,OLAP(1), DESCRIP(1))

        CALL READA_Dates(X(4), YRSIM, IMAT)
        IF (IMAT .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DMAT = TIMDIF (YRPLT,IMAT)
        ELSE
          DMAT  = -99
        ENDIF
        OLAP(4) = 'MDAP  '     !2/11/09 CHP changed from HDAP to MDAP
        CALL GetDesc(1,OLAP(4), DESCRIP(4))

        CALL READA_Dates(X(22), YRSIM, IEMRG)
        IF (IEMRG .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DEMRG = TIMDIF(YRPLT,IEMRG)
        ELSE
          DEMRG  = -99
        ENDIF
        OLAP(22) = 'EDAP  '
        CALL GetDesc(1,OLAP(22), DESCRIP(22))

        DNR1 = TIMDIF (YRPLT,ISDATE)
        IF (DNR1 .LE. 0) THEN
          DNR1 = -99
        ENDIF

        DNR7 = TIMDIF (YRPLT,MDATE)
        IF (DNR7 .LE. 0 .OR. YRPLT .LT. 0) THEN
          DNR7 = -99
        ENDIF

        YREMRG = STGDOY(7)
        IF (YRPLT .GT. 0) THEN
          DNR0 = TIMDIF (YRPLT,YREMRG)
          IF (DNR0 .LE. 0)  THEN
            DNR0 = -99
          ENDIF
        ELSE
          DNR0 = -99
        ENDIF

        YIELDB = (YIELD/1000.)/0.2                  ! Fresh yield

        WRITE(Simulated(1),'(I8)') DNR1;  WRITE(Measured(1),'(I8)') DFLR
        WRITE(Simulated(2),'(I8)') -99;   WRITE(Measured(2),'(I8)') -99
        WRITE(Simulated(3),'(I8)') -99;   WRITE(Measured(3),'(I8)') -99
        WRITE(Simulated(4),'(I8)') DNR7;  WRITE(Measured(4),'(I8)') DMAT
        WRITE(Simulated(5),'(I8)') NINT(YIELD)
                              WRITE(Measured(5),'(A8)') TRIM(X(5))
        WRITE(Simulated(6),'(I8)') -99;   WRITE(Measured(6),'(I8)') -99
        WRITE(Simulated(7),'(I8)') -99;   WRITE(Measured(7),'(I8)') -99
        WRITE(Simulated(8),'(F8.2)')YLDFR;
                              WRITE(Measured(8),'(A8)') TRIM(X(8))
        WRITE(Simulated(9),'(I8)') -99;   WRITE(Measured(9),'(I8)') -99
        WRITE(Simulated(10),'(I8)') NINT(PBIOMS)
                               WRITE(Measured(10),'(A8)') TRIM(X(10))

!     08/11/2005 CHP changed from BWAH to BWAM, value remains the same (HAULM)
        WRITE(Simulated(11),'(I8)') NINT(HAULM) 
                              WRITE(Measured(11),'(A8)') TRIM(X(11))

        WRITE(Simulated(12),'(F8.2)')MAXLAI
                              WRITE(Measured(12),'(A8)') TRIM(X(12))
        WRITE(Simulated(13),'(I8)') -99; WRITE(Measured(13),'(I8)') -99
        WRITE(Simulated(14),'(I8)') -99; WRITE(Measured(14),'(I8)') -99
        WRITE(Simulated(15),'(F8.2)')TUBNUP
                              WRITE(Measured(15),'(A8)') TRIM(X(15))
        WRITE(Simulated(16),'(F8.2)')TOTNUP
                              WRITE(Measured(16),'(A8)') TRIM(X(16))
        WRITE(Simulated(17),'(F8.2)')APTNUP
                              WRITE(Measured(17),'(A8)') TRIM(X(17))
        WRITE(Simulated(18),'(F8.3)')PTUBNP
                              WRITE(Measured(18),'(A8)') TRIM(X(18))
        WRITE(Simulated(19),'(I8)') -99; WRITE(Measured(19),'(I8)') -99
        WRITE(Simulated(20),'(I8)') -99; WRITE(Measured(20),'(I8)') -99
        WRITE(Simulated(21),'(I8)') -99; WRITE(Measured(21),'(I8)') -99
        WRITE(Simulated(22),'(I8)') DNR0;WRITE(Measured(22),'(I8)')DEMRG
      ENDIF
!-------------------------------------------------------------------
!         Compute values to be sent to OPSUM for SUMMARY.OUT file.
          GPP = 0.0
          WTNFX  = 0.0
          PSDWT  = 0.0

!         Store Summary.out labels and values in arrays to send to
!         OPSUM routines for printing.  Integers are temporarily 
!         saved aS real numbers for placement in real array.
          LABEL(1)  = 'ADAT'; VALUE(1)  = FLOAT(YRNR1)
          LABEL(2)  = 'MDAT'; VALUE(2)  = FLOAT(YRNR7)
          LABEL(3)  = 'DWAP'; VALUE(3)  = SDWTPL
          LABEL(4)  = 'CWAM'; VALUE(4)  = BIOMAS*10.
          LABEL(5)  = 'HWAM'; VALUE(5)  = YIELD
          LABEL(6)  = 'HWAH'; VALUE(6)  = SDWTAH * 10.
          LABEL(7)  = 'BWAH'; VALUE(7)  = BWAH
          LABEL(8)  = 'HWUM'; VALUE(8)  = PSDWT   !*1000.
          LABEL(9)  = 'H#AM'; VALUE(9)  = SEEDNO
          LABEL(10) = 'H#UM'; VALUE(10) = GPP
          LABEL(11) = 'NFXM'; VALUE(11) = WTNFX*10.
          LABEL(12) = 'NUCM'; VALUE(12) = WTNUP   !WTNUP in kg/ha
          LABEL(13) = 'CNAM'; VALUE(13) = WTNCAN*10.
          LABEL(14) = 'GNAM'; VALUE(14) = WTNSD*10.
          LABEL(15) = 'PWAM'; VALUE(15) = 0.0
          LABEL(16) = 'LAIX'; VALUE(16) = MAXLAI
          LABEL(17) = 'HIAM'; VALUE(17) = HI
          LABEL(18)  ='EDAT'; VALUE(18) = FLOAT(YREMRG)

          !Send labels and values to OPSUM
          CALL SUMVALS (SUMNUM, LABEL, VALUE) 

      CALL OPVIEW(CONTROL, 
     &    BIOMAS*10., ACOUNT, DESCRIP, IDETO, LeafNo, 
     &    Measured, PlantStres, Simulated, STGDOY, STNAME, 
     &    WTNCAN*10., XLAI, NINT(YIELD), YRPLT, ISTAGE)

      !Send Observed and Simulated datat to OPSUM
      CALL EvaluateDat (ACOUNT, Measured, Simulated, DESCRIP, OLAP) 

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE PT_OPHARV
