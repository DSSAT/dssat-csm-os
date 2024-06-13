C=======================================================================
C  FOR_OPHARV, Subroutine G. Hoogenboom, J. W. Jones
C  Generates output for seasonal data.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1990 GH  Written
C  11/02/1999 CHP Changed TSOC to THUMC, TSIN to THUMN, AMTRES to CUMRES 
C  07/01/2000 GH  Eliminated common block statements
C  03/03/2002 GH  Modified logic for reading fileA
C  06/11/2002 GH  Modified for Y2K
C=======================================================================
C      CALLED BY: CROPGRO
C=======================================================================
      SUBROUTINE FOR_OPHARV(CONTROL, ISWITCH, 
     &    CANHT, CANNAA, CANWAA, CROP, LAIMX, HARVFRAC,   !Input
     &    MDATE, NSTRES, PCLSD, PCNSD, PODNO, PODWT,      !Input
     &    SDRATE, SDWT, SDWTAM, SEEDNO, STGDOY, STMWT,    !Input
     &    TOPWT, TURFAC, VSTAGE, WTNCAN, WTNFX, WTNSD,    !Input
     &    WTNST, WTNUP, XLAI,                             !Input
     &    YRNR1, YRNR3, YRNR5, YRNR7, YRPLT,              !Input
     &    BWAH, SDWTAH)                                   !Output

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
        ! which contain control information, soil
        ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL FIND, ERROR, FOR_STNAMES, FOR_OPVIEW, READA, 
     &  READA_Dates, CHANGE_DESC, GetDesc, SUMVALS, EvaluateDat,
     &  TIMDIF, READA_Y4K
      SAVE

      CHARACTER*1  RNMODE,IDETO,IDETS,IPLTI, PLME
      CHARACTER*2  CROP
      CHARACTER*6  SECTION
!      CHARACTER*6  XGWT,XNOGR,XGWU,XNOGU, SECTION
!      CHARACTER*6  XCWT,XSWT,XLAM,XHIN,XNGR,XTHR,XPDW,XNTP
!      CHARACTER*6  XNST,XNPS,XCWAA,XCNAA,XLFNO,XLPS,XCNHT
      CHARACTER*6, PARAMETER :: ERRKEY = 'OPHARV'
      CHARACTER*10 STNAME(20)
      CHARACTER*12 FILEA
      CHARACTER*30 FILEIO
      character*80 pathex

      INTEGER DFLR,DFPD,DFSD,DHRV,DNR8,DMAT,DNR1,DNR3,DNR5,DNR7
      INTEGER DYNAMIC, ERRNUM, FOUND
      INTEGER IFLR, IFPD, IFSD, IHRV, IMAT, ISENS
      INTEGER LNUM, LUNIO, RUN, TIMDIF, TRTNO, YIELD
      INTEGER YRNR1,YRNR3,YRNR5,YRNR7,MDATE,YRDOY, YRPLT,YRSIM
      INTEGER STGDOY(20)

      REAL BIOMAS, BWAH, CANHT, CANNAA, CANWAA, HI, HWAH, HWAM
      REAL LAIMX, NSTRES, PCLSD, PCNSD, PODWT, PODNO, PSDWT, PSPP
      REAL SDRATE, SDWT, SDWTAH, SDWTAM, SEEDNO, STMWT
      REAL THRES, TOPWT, TURFAC, VSTAGE
      REAL WTNCAN, WTNFX, WTNSD, WTNST, WTNUP, XLAI
      REAL, DIMENSION(2) :: HARVFRAC

!     Arrays which contain data for printing in SUMMARY.OUT file
!       (OPSUM subroutine)
      INTEGER, PARAMETER :: ACOUNT = 24
      INTEGER, PARAMETER :: SUMNUM = 14
      CHARACTER*4, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

!     Arrays which contain predicted and observed data for printing
!       in OVERVIEW.OUT and EVALUATE.OUT files (FOR_OPVIEW subroutine)
!         OLAP modified for dap
      CHARACTER*6, DIMENSION(EvaluateNum) :: OLAB, OLAP  
      CHARACTER*12  X(EvaluateNum)
      CHARACTER*8 PREDICTED(EvaluateNum), OBSERVED(EvaluateNum)
      CHARACTER*50 DESCRIP(EvaluateNum)

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

!     The variable "ISWITCH" is of type "SwitchType".
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
      IDETS = ISWITCH % IDETS
      IPLTI = ISWITCH % IPLTI

!      ACOUNT = 24  !Number of possible FILEA headers for this crop

!     Define headings for observed data file (FILEA)
      DATA OLAB / !Pred.          Obs.   Definition
        !------------   -----  -----------
     & 'ADAT  ', ! 1 DNR1           DFLR   Anthesis date
     & 'PD1T  ', ! 2 DNR3           DFPD   First Pod        
     & 'PDFT  ', ! 3 DNR5           DFSD   First Seed       
     & 'MDAT', ! 4 DNR7           DMAT   Physiological Maturity
     & 'HWAM', ! 5 NINT(SDWT*10)  XGWT   Seed Yield (kg/ha;dry)
     & 'PWAM', ! 6 NINT(PODWT*10) XPDW   Pod Yield (kg/ha;dry) 
     & 'H#AM', ! 7 NINT(SEEDNO)   XNOGR  Seed Number (Seed/m2)
     & 'HWUM', ! 8 PSDWT          XGWU   Weight Per Seed (g;dry)
     & 'H#UM', ! 9 PSPP           XNOGU  Seeds/Pod
     & 'CWAM', !10 NINT(TOPWT*10) XCWT   Biomass (kg/ha) at Harvest Mat.
     & 'BWAH', !11 NINT(STMWT*10) XSWT   Stalk (kg/ha) at Harvest Mat.
     & 'LAIX', !12 LAIMX          XLAM   Maximum LAI (m2/m2)
     & 'HIAM', !13 HI             XHIN   Harvest Index (kg/kg)
     & 'THAM', !14 THRES          XTHR   Shelling Percentage (%)
     & 'GNAM', !15 NINT(WTNSD*10) XNGR   Seed N (kg N/ha)
     & 'CNAM', !16 NINT(WTNCAN*10)XNTP   Biomass N (kg N/ha)
     & 'SNAM', !17 NINT(WTNST*10) XNST   Stalk N (kg N/ha)
     & 'GN%M', !18 PCNSD          XNPS   Seed N (%)
     & 'CWAA', !19 NINT(CANWAA*10)XCWAA  Biomass (kg/ha) at Anthesis
     & 'CNAA', !20                XCNAA 
     & 'L#SM', !21 VSTAGE         XLFNO  Final Leaf Number (Main Stem)
     & 'GL%M', !22 PCLSD          XLPS   Seed Lipid (%)
     & 'CHTA', !23 CANHT          XCNHT  Canopy Height (m)
     & 'R8AT', !24 DNR8           DHRV   Harvest Maturity (dap)
     & 16*'    '/

!     GWAH    !Grain weight at harvest (kg/ha)
!     CWAH    !Canopy weight at harvest (kg/ha)
!     FWAH    !Fruit weight at harvest (kg/ha)
!***********************************************************************
!***********************************************************************
!     RUN INITIALIZATION
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
C-----------------------------------------------------------------------
!     Read FILEIO
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)

      READ (LUNIO,'(55X,I5)') ISENS     !used only in OPHARV
      READ (LUNIO,'(3(/),15X,A12,1X,A80)',IOSTAT=ERRNUM) FILEA,
     &     PATHEX

      SECTION = '*TREAT'
      CALL FIND(LUNIO, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILEIO, LNUM)
      ELSE
        READ(LUNIO, '(I3)') TRTNO
      ENDIF

!     Find and Read Planting Details Section
      SECTION = '*PLANT'
      CALL FIND(LUNIO, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) CALL ERROR (ERRKEY,3,FILEIO,LNUM)
      READ(LUNIO,'(35X,A1)') PLME

      CLOSE (LUNIO)

!     Assign names to stages based on crop.
      CALL FOR_STNAMES(CROP, PLME, STNAME)
      
!     Assign descriptions to observed and predicted data 
!         from DATA.CDE.
      CALL GETDESC(ACOUNT, OLAB, DESCRIP)
      OLAP = OLAB

      CALL FOR_OPVIEW(CONTROL, 
     &    BIOMAS, ACOUNT, DESCRIP, IDETO, VSTAGE, NSTRES, 
     &    OBSERVED, PREDICTED, STGDOY, STNAME, TURFAC, 
     &    WTNCAN, XLAI, YIELD, YRPLT)

C***********************************************************************
C***********************************************************************
C     SEASONAL INITIALIZATION
C***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
      PREDICTED = ' '
      OBSERVED  = ' '
      YIELD  = 0.0
      BIOMAS = 0.0
      
      CALL FOR_OPVIEW(CONTROL, 
     &    BIOMAS, ACOUNT, DESCRIP, IDETO, VSTAGE, NSTRES, 
     &    OBSERVED, PREDICTED, STGDOY, STNAME, TURFAC, 
     &    WTNCAN, XLAI, YIELD, YRPLT)

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
C-----------------------------------------------------------------------
      BIOMAS = TOPWT*10.
!     Send data to Overview.out data on days where stages occur
      CALL FOR_OPVIEW(CONTROL, 
     &    BIOMAS, ACOUNT, DESCRIP, IDETO, VSTAGE, NSTRES, 
     &    OBSERVED, PREDICTED, STGDOY, STNAME, TURFAC, 
     &    WTNCAN, XLAI, YIELD, YRPLT)

!***********************************************************************
!***********************************************************************
!     Seasonal Output 
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
!     Compute values to be sent to Overview, Summary and Evaluate files.
      IF (SEEDNO .GT. 0.0) THEN
        PSDWT = SDWT/SEEDNO
      ELSE
        PSDWT = 0.0
      ENDIF

      IF (PODNO .GT. 0.) THEN
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

      IF (TOPWT .GT. 0. .AND. SDWT .GE. 0.) THEN
        HI = SDWT/TOPWT
      ELSE
        HI = 0.
      ENDIF

      IF (CROP .EQ. 'FA') YRPLT = -99

!-----------------------------------------------------------------------
!     Read observed (measured) data from FILEA
!-----------------------------------------------------------------------
      IF(IDETO .EQ. 'Y' .OR. INDEX('IAEBCGD',RNMODE) .GT. 0) THEN
        !CALL READA(FILEA, PATHEX, OLAB, TRTNO, YRSIM, X)
        CALL READA_Y4K(FILEA, PATHEX,OLAB, TRTNO, YRSIM, X)
!     &    FILEA, OLAB, TRTNO, YRSIM,                      !Input
!     &    IFLR, IFPD, IFSD, IHRV, IMAT, XCNAA, XCNHT,     !Output
!     &    XCWAA, XCWT, XGWT, XGWU, XHIN, XLAM, XLFNO,     !Output
!     &    XLPS, XNGR, XNOGR, XNOGU, XNPS, XNTP, XNST,     !Output
!     &    XPDW, XSWT, XTHR)                               !Output

!     Convert from YRDOY format to DAP.  Change descriptions to match.
        CALL READA_Dates(X(1), YRSIM, IFLR)
        IF (IFLR .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
        DFLR = TIMDIF(YRPLT,IFLR)
        ELSE
        DFLR  = -99
        ENDIF
        !CALL CHANGE_DESC(DESCRIP(1)) 
        OLAP(1) = 'ADAP  '
        CALL GetDesc(1,'ADAP  ', DESCRIP(1))

        CALL READA_Dates(X(2), YRSIM, IFPD)
        IF (IFPD .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
        DFPD = TIMDIF(YRPLT,IFPD)
        ELSE
        DFPD  = -99
        ENDIF
        !CALL CHANGE_DESC(DESCRIP(2)) 
        OLAP(2) = 'PD1P  '
        CALL GetDesc(1,'PD1P  ', DESCRIP(2))

        CALL READA_Dates(X(3), YRSIM, IFSD)
        IF (IFSD .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
        DFSD = TIMDIF(YRPLT,IFSD)
        ELSE
        DFSD  = -99
        ENDIF
        !CALL CHANGE_DESC(DESCRIP(3))
        OLAP(3) = 'PDFP  '
        CALL GetDesc(1,'PDFP  ', DESCRIP(3))

        CALL READA_Dates(X(4), YRSIM, IMAT)
        IF (IMAT .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
        DMAT = TIMDIF(YRPLT,IMAT)
        ELSE
        DMAT  = -99
        ENDIF
        !CALL CHANGE_DESC(DESCRIP(4)) 
        OLAP(4) = 'MDAP  '
        CALL GetDesc(1,'MDAP  ', DESCRIP(4))

        CALL READA_Dates(X(24), YRSIM, IHRV)
        IF (IHRV .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
        DHRV = TIMDIF(YRPLT,IHRV)
        ELSE
        DHRV  = -99
        ENDIF
        !CALL CHANGE_DESC(DESCRIP(24)) 
        OLAP(24) = 'R8AP  '
        CALL GetDesc(1,'R8AP  ', DESCRIP(24))

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
        IF (DNR8 .LE. 0) THEN
        DNR8 = -99
        MDATE = -99
        ENDIF

!-----------------------------------------------------------------------
!     Store predicted and observed data for this season.
      WRITE(PREDICTED(1),' (I8)') DNR1;  WRITE(OBSERVED(1),'(I8)') DFLR
      WRITE(PREDICTED(2),' (I8)') DNR3;  WRITE(OBSERVED(2),'(I8)') DFPD
      WRITE(PREDICTED(3),' (I8)') DNR5;  WRITE(OBSERVED(3),'(I8)') DFSD
      WRITE(PREDICTED(4),' (I8)') DNR7;  WRITE(OBSERVED(4),'(I8)') DMAT
      WRITE(PREDICTED(5),' (I8)') NINT(SDWT*10);  
        WRITE(OBSERVED(5),'(A8)') TRIM(X(5))
      WRITE(PREDICTED(6),' (I8)') NINT(PODWT*10); 
        WRITE(OBSERVED(6),'(A8)') TRIM(X(6))
      WRITE(PREDICTED(7),' (I8)') NINT(SEEDNO);   
        WRITE(OBSERVED(7),'(A8)') TRIM(X(7))
      WRITE(PREDICTED(8),'(F8.3)')PSDWT; 
                                WRITE(OBSERVED(8),'(A8)') TRIM(X(8))
      WRITE(PREDICTED(9),'(F8.2)')PSPP;  
                                WRITE(OBSERVED(9),'(A8)') TRIM(X(9))
      WRITE(PREDICTED(10),'(I8)') NINT(TOPWT*10); 
        WRITE(OBSERVED(10),'(A8)') TRIM(X(10))
      WRITE(PREDICTED(11),'(I8)') NINT(STMWT*10); 
        WRITE(OBSERVED(11),'(A8)') TRIM(X(11))
      WRITE(PREDICTED(12),'(F8.2)')LAIMX;
                                WRITE(OBSERVED(12),'(A8)') TRIM(X(12))
      WRITE(PREDICTED(13),'(F8.3)')HI;   
                                WRITE(OBSERVED(13),'(A8)') TRIM(X(13))
      WRITE(PREDICTED(14),'(F8.2)')THRES;
                                WRITE(OBSERVED(14),'(A8)') TRIM(X(14))
      WRITE(PREDICTED(15),'(I8)') NINT(WTNSD*10); 
        WRITE(OBSERVED(15),'(A8)') TRIM(X(15))
      WRITE(PREDICTED(16),'(I8)') NINT(WTNCAN*10);
        WRITE(OBSERVED(16),'(A8)') TRIM(X(16))
      WRITE(PREDICTED(17),'(I8)') NINT(WTNST*10); 
        WRITE(OBSERVED(17),'(A8)') TRIM(X(17))
      WRITE(PREDICTED(18),'(F8.2)')PCNSD;
                                WRITE(OBSERVED(18),'(A8)') TRIM(X(18))
      WRITE(PREDICTED(19),'(I8)') NINT(CANWAA*10);
        WRITE(OBSERVED(19),'(A8)') TRIM(X(19))
      WRITE(PREDICTED(20),'(I8)') NINT(CANNAA*10);
        WRITE(OBSERVED(20),'(A8)') TRIM(X(20))
      WRITE(PREDICTED(21),'(F8.2)')VSTAGE
        WRITE(OBSERVED(21),'(A8)') TRIM(X(21))
      WRITE(PREDICTED(22),'(F8.2)')PCLSD;
                                WRITE(OBSERVED(22),'(A8)') TRIM(X(22))
      WRITE(PREDICTED(23),'(F8.2)')CANHT;
                                WRITE(OBSERVED(23),'(A8)') TRIM(X(23))
      WRITE(PREDICTED(24),'(I8)')  DNR8; 
                                WRITE(OBSERVED(24),'(I8)')DHRV
     
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
      BWAH   = (TOPWT - SDWT) * HARVFRAC(2)
!      BWAH   = (TOPWT - SDWT) * HBPC(1)/100.
!      BWAH   = (TOPWT - SDWT)

      IF (SEEDNO .GT. 0.0) THEN
        PSDWT = SDWT/SEEDNO
      ELSE
        PSDWT = 0.0
      ENDIF

      IF (PODNO .GT. 0.) THEN
        PSPP = SEEDNO/PODNO
      ELSE
        PSPP  = 0.
      ENDIF

      IF ((CROP .EQ. 'TM') .OR. (CROP .EQ. 'PR')) THEN
        HWAM = PODWT * 10
        HWAH = PODWT * 10
      ELSE
        HWAM = SDWTAM * 10
        HWAH = SDWTAH * 10
      END IF

!      IF (IDETS .EQ. 'Y' .OR. IDETS .EQ. 'A') THEN
!       Store Summary.out labels and values in arrays to send to
!       OPSUM routines for printing.  Integers are temporarily 
!       saved aS real numbers for placement in real array.
        LABEL(1)  = 'ADAT'; VALUE(1)  = FLOAT(YRNR1)
        LABEL(2)  = 'MDAT'; VALUE(2)  = FLOAT(YRNR7)
        LABEL(3)  = 'DWAP'; VALUE(3)  = SDRATE
        LABEL(4)  = 'CWAM'; VALUE(4)  = TOPWT*10.
        LABEL(5)  = 'HWAM'; VALUE(5)  = HWAM
        LABEL(6)  = 'HWAH'; VALUE(6)  = HWAH
        LABEL(7)  = 'BWAH'; VALUE(7)  = BWAH
        LABEL(8)  = 'HWUM'; VALUE(8)  = PSDWT*1000.
        LABEL(9)  = 'H#AM'; VALUE(9)  = SEEDNO
        LABEL(10) = 'H#UM'; VALUE(10) = PSPP
        LABEL(11) = 'NFXM'; VALUE(11) = WTNFX*10.
        LABEL(12) = 'NUCM'; VALUE(12) = WTNUP*10.
        LABEL(13) = 'CNAM'; VALUE(13) = WTNCAN*10.
        LABEL(14) = 'GNAM'; VALUE(14) = WTNSD*10.

        !Send labels and values to OPSUM
        CALL SUMVALS (SUMNUM, LABEL, VALUE) 
!      ENDIF

!-----------------------------------------------------------------------
!     Call Overview.out routine
      BIOMAS = TOPWT*10.
      YIELD  = NINT(HWAM)
      
      CALL FOR_OPVIEW(CONTROL, 
     &    BIOMAS, ACOUNT, DESCRIP, IDETO, VSTAGE, NSTRES, 
     &    OBSERVED, PREDICTED, STGDOY, STNAME, TURFAC, 
     &    WTNCAN, XLAI, YIELD, YRPLT)

!-----------------------------------------------------------------------
      !Send Observed and Predicted datat to OPSUM
      IF(IDETO .EQ. 'Y' .OR. INDEX('IAEBCGD',RNMODE) .GT. 0) THEN
        CALL EvaluateDat (ACOUNT, Observed, Predicted, DESCRIP, OLAP) 
      ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE FOR_OPHARV
C=======================================================================



C=======================================================================
C  STNAMES, Subroutine C.H.Porter
C  Assigns STNAME for various CROPGRO crops.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/01/2001 CHP Written
C=======================================================================

      SUBROUTINE FOR_STNAMES(CROP, PLME, STNAME)

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
      CASE ('BN')
        STNAME(1) = 'Emergence '
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

      CASE ('C3','C4','G0','G1','G2','G3','G4','G5','G6','G7','G8','BR')
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

      CASE ('CH')
        STNAME(1) = 'Emergence '
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

      CASE ('CP')
        STNAME(1) = 'Emergence '
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

      CASE ('CT')
        STNAME( 1) = 'Emergence '
        STNAME( 2) = 'Unifoliate'
        STNAME( 3) = 'End Juven.'
        STNAME( 4) = 'Flower Ind'
        STNAME( 5) = 'Flowering '
        STNAME( 6) = 'First Pod '
        STNAME( 7) = 'Full Pod  '
        STNAME( 8) = 'First Seed'
        STNAME( 9) = 'End Pod   '
        STNAME(10) = 'Phys. Mat '
        STNAME(11) = 'Harv. Mat '
        STNAME(12) = 'End Msnode'
        STNAME(13) = 'End Leaf  '
        STNAME(14) = 'Start Sim '
        STNAME(15) = 'Sowing    '
        STNAME(16) = 'Harvest   '       

      CASE ('FA')
        STNAME(14)= 'Start Sim '
        STNAME(16)= 'End Sim   '

      CASE ('FB')
        STNAME(1) = 'Emergence '
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

      CASE ('PE')
        STNAME(1) = 'Emergence '
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

      CASE ('PN')
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

      CASE ('PP')
        STNAME(1) = 'Emergence '
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

      CASE ('PR')
        STNAME(1) = 'Emergence '
        STNAME(2) = 'Unifoliate'
        STNAME(3) = 'End Juven.'
        STNAME(4) = 'Flower Ind'
        STNAME(5) = 'First Flwr'
        STNAME(6) = 'Frst Fruit'
        STNAME(7) = 'Frst Fruit'
        STNAME(8) = 'First Seed'
        STNAME(9) = 'End Fruit '
        STNAME(10)= 'Phys. Mat '
        STNAME(11)= 'Harv. Mat '
        STNAME(12)= 'End Msnode'
        STNAME(13)= 'End Leaf  '
        STNAME(14)= 'Start Sim '
        STNAME(15)= 'Sowing    '
        STNAME(16)= 'Harvest   '

      CASE ('SB')
        STNAME(1) = 'Emergence '
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

      CASE ('TM')
        STNAME(1) = 'Emergence '
        STNAME(2) = 'Unifoliate'
        STNAME(3) = 'End Juven.'
        STNAME(4) = 'Flower Ind'
        STNAME(5) = 'First Flwr'
        STNAME(6) = 'Frst Fruit'
        STNAME(7) = 'Frst Fruit'
        STNAME(8) = 'First Seed'
        STNAME(9) = 'End Fruit '
        STNAME(10)= 'Phys. Mat '
        STNAME(11)= 'Harv. Mat '
        STNAME(12)= 'End Msnode'
        STNAME(13)= 'End Leaf  '
        STNAME(14)= 'Start Sim '
        STNAME(15)= 'Sowing    '
        STNAME(16)= 'Harvest   '

      CASE ('VB')
        STNAME(1) = 'Emergence '
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
      
      END SELECT

      IF (PLME .EQ. 'T') THEN
        STNAME(15) = 'Transplant'
      ENDIF

      RETURN
      END SUBROUTINE FOR_STNAMES
