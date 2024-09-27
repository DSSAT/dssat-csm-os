C=======================================================================
C  TEFF_OPHARV, Subroutine
C
C  Write the harvest report
C-----------------------------------------------------------------------
C  Revision history
C
C  02/07/1993 PWW Header revision and minor changes
C  02/07/1993 PWW Added switch block, etc.
C  08/29/2002 CHP/MUS Converted to modular format for inclusion in CSM.
C  08/12/2003 CHP Added I/O error checking and changed call to READA
!  05/03/2004 CHP Added P stresses to OPVIEW call 
!  07/21/2005 CHP Changed PODWT to g/m2 to be consistent with other
!                 plant modules.
!                 Added PWAM, LAIX, HIAM to summary.out 
!  08/11/2005 CHP Use BWAH as byproduct variable for Summary.OUT 
!                 (byproduct harvested), BWAM as byproduct variable 
!                 for Overview.OUT (byproduct produced)
!  10/24/2005 CHP Added environmental & stress factors to Overview.OUT
C  02/09/2007 GH  Add path for FileA
!  04/02/2008 US/CHP Added P model
!  08/28/2009 CHP added EDAT, EDAP 
C  12/12/2019 MB/US Copyed from Rice model and modified for Teff 
C  03/29/2021 MB/WP Addapted to Teff based on CERES-Rice
C=======================================================================

      SUBROUTINE TEFF_OPHARV (CONTROL, ISWITCH, 
     &    AGEFAC, APTNUP, BIOMAS, CANNAA, CANWAA,         !Input
     &    DYIELD, G2, GNUP, GPP, GPSM, GRAINN, GRNWT,     !Input
     &    HARVFRAC, ISDATE, ISTAGE, LAI, LEAFNO, MAXLAI,  !Input
     &    MDATE, NSTRES, PANWT, PBIOMS, PLANTS, PLTPOP,   !Input
     &    PSTRES1, PSTRES2,                               !Input
     &    SKERWT, STGDOY, STNAME, STOVER, STOVN, SWFAC,   !Input
     &    TILNO, TOTNUP, TURFAC, XGNP, YRPLT,             !Input
     &    BWAH, PODWT, SDWT, SDWTAH, TOPWT, WTNSD)        !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT     NONE
      EXTERNAL FIND, ERROR, OPVIEW, READA, READA_Dates, 
     &  GetDesc, SUMVALS, EvaluateDat, TIMDIF, READA_Y4K
      SAVE

      CHARACTER*1  RNMODE, IDETO, IPLTI
      CHARACTER*6  SECTION
!      CHARACTER*6  XPDW,XGWT,XTHR,XGWU,XNOGR,XNOGU,XLAM,XCWT,XSWT,XHIN,
!     &             XNPS,XNTP,XNST,XNGR,XCWAA,XCNAA,XLFNO,XPNO
      CHARACTER*6, PARAMETER :: ERRKEY = 'OPHARV'
      CHARACTER*10 STNAME(20)
      CHARACTER*12 FILEA
      CHARACTER*30 FILEIO
	CHARACTER*80 PATHEX

      INTEGER TRTNUM
      INTEGER TRT_ROT
      INTEGER IEMRG, ISDATE,ISENS, YREMRG
      INTEGER YRDOY,YRPLT,YRSIM,YRNR1,YRNR2,YRNR3,YRNR5,YRNR7
      INTEGER DEMRG, D_emerge, DNR1,DNR7,MDATE,STGDOY(20)
      INTEGER DYNAMIC, LUNIO, RUN, ACOUNT, ERRNUM, LINC, LNUM, FOUND
      INTEGER DFLR, DMAT, LEAFNO, YIELD, ISTAGE
      INTEGER DNR0, DPIN, IFLR, TIMDIF, IMAT, IPIN

      REAL MAXLAI,ACREFC,TOTNUP,PSDWT,PSPP,HI, LAI
      REAL WTNCAN,PLANTS, PANWT, PODWT
      REAL WTNSD,WTNUP,PLTPOP,PBIOMS,SDWTAM,APTNUP
      REAL SKERWT,STOVER,STOVN, SEEDNO,SDWT,G2
      REAL TOPWT,GPP,GPSM,GNUP,GRAINN, BIOMAS,XGNP
      REAL VSTAGE, NSTRES, TURFAC ,XLAI
      REAL SDRATE, DYIELD, AGEFAC, SWFAC
      REAL CANWAA,CANNAA,GRNWT,TILNO,BWAH,SDWTAH
      REAL Pstres1, Pstres2   

      REAL, DIMENSION(2) :: HARVFRAC

      PARAMETER (ACREFC = 2.47)

!     Arrays which contain data for printing in SUMMARY.OUT file
!       (OPSUM subroutine)
      INTEGER, PARAMETER :: SUMNUM = 18
      CHARACTER*4, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

!     Arrays which contain simulated and measured data for printing
!       in OVERVIEW.OUT and EVALUATE.SUM files (OPVIEW subroutine)
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
      DYNAMIC= CONTROL % DYNAMIC
      FILEIO = CONTROL % FILEIO
      LUNIO  = CONTROL % LUNIO
      RUN    = CONTROL % RUN
      RNMODE = CONTROL % RNMODE
      YRDOY  = CONTROL % YRDOY
      YRSIM  = CONTROL % YRSIM

      IDETO = ISWITCH % IDETO
      IPLTI = ISWITCH % IPLTI

      ACOUNT = 19  !Number of possible FILEA headers for this crop

!     Define headings for observed data file (FILEA)
      DATA OLAB / ! 
                !   
     &  'IDAT  ', ! 1 Panicle Initiation date (YrDoy)
     &  'ADAT  ', ! 2 Anthesis date (YrDoy)              
     &  'MDAT  ', ! 3 Physiological maturity date (YrDoy)        
     &  'HWAM  ', ! 4 Yield at maturity (kg dm/ha)
     &  'HWUM  ', ! 5 Unit wt at maturity (mg dm/unit)
     &  'H#AM  ', ! 6 Number at maturity (no/m2)
     &  'P#AM  ', ! 7 PANICLE NUMBER (PANICLE/m2) 
     &  'LAIX  ', ! 8 Maximum Leaf area index 
     &  'CWAA  ', ! 9 Tops weight at anthesis (kg dm/ha)
     &  'CNAA  ', !10 Tops N at anthesis (kg/ha) 
     &  'CWAM  ', !11 Tops weight at maturity (kg dm/ha)

!         08/11/2005 CHP
!         Change BWAH to BWAM -- by-product produced to maturity, but
!           not necessarily removed from field
!     &  'BWAH', !12 By-product harvest (kg dm/ha)
     &  'BWAM  ', !12 By-product produced to maturity (kg[dm]/ha)
               
     &  'HIAM  ', !13 Harvest index at maturity
     &  'L#SM  ', !14 Leaf number per stem,maturity
     &  'GNAM  ', !15 Grain N at maturity (kg/ha) 
     &  'CNAM  ', !16 Tops N at maturity (kg/ha) 
     &  'SNAM  ', !17 Stem N at maturity (kg/ha)
     &  'GN%M  ', !18 Grain N at maturity (%)
     &  'EDAT  ', !19 emergence date
!     &  'PD1T', !19 
!     &  'PDFT', !20       
!     &  'PWAM', !21 
!     &  'THAM', !22 
     &  21*'      '/

!***********************************************************************
!***********************************************************************
!     RUN INITIALIZATION
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
C-----------------------------------------------------------------------
!     Read FILEIO
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)

      READ (LUNIO,'(55X,I5)', IOSTAT=ERRNUM) ISENS ; LNUM = 1
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
      READ (LUNIO,'(3(/),15X,A12,1X,A80)', IOSTAT=ERRNUM) FILEA,
     &      PATHEX
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
     &    PBIOMS, ACOUNT, DESCRIP, IDETO, VSTAGE, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, XLAI, YIELD, YRPLT, ISTAGE)

C***********************************************************************
C***********************************************************************
C     SEASONAL INITIALIZATION
C***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
      Simulated = ' '
      Measured  = ' '
      YIELD     = 0
      PBIOMS    = 0.0
      WTNCAN    = 0.0
      
!     Establish #, names of stages for environmental & stress summary
      PlantStres % ACTIVE = .FALSE.
      PlantStres % NSTAGES = 5
      PlantStres % StageName(0) = 'Planting to Harvest    '
      PlantStres % StageName(1) = 'Emergence-End Juvenile '
      PlantStres % StageName(2) = 'End Juvenil-Panicl Init'
      PlantStres % StageName(3) = 'Panicl Init-End Lf Grow'
      PlantStres % StageName(4) = 'End Lf Grth-Beg Grn Fil'
      PlantStres % StageName(5) = 'Grain Filling Phase    '

      CALL OPVIEW(CONTROL, 
     &    PBIOMS, ACOUNT, DESCRIP, IDETO, VSTAGE, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, XLAI, YIELD, YRPLT, ISTAGE)

      MAXLAI = 0.0

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
C-----------------------------------------------------------------------
      VSTAGE = REAL(LEAFNO)
      !WTNCAN = TOTNUP/10.0
      WTNCAN = (STOVN + GRAINN)*PLANTS
      XLAI = LAI
      MAXLAI = AMAX1 (MAXLAI,XLAI)      ! Maximum XLAI season

      PlantStres % W_grow = TURFAC 
      PlantStres % W_phot = SWFAC  
      PlantStres % N_grow = AGEFAC 
      PlantStres % N_phot = NSTRES 
      PlantStres % P_grow = PSTRES2
      PlantStres % P_phot = PSTRES1
      PlantStres % ACTIVE = .FALSE.

      IF (ISTAGE > 0 .AND. ISTAGE < 6) THEN
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
     &    PBIOMS, ACOUNT, DESCRIP, IDETO, VSTAGE, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, XLAI, YIELD, YRPLT, ISTAGE)

!***********************************************************************
!***********************************************************************
!     Seasonal Output 
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
C     Actual yield harvested (default is 100 %)
C-----------------------------------------------------------------------
      SDWT   = DYIELD / 10.0
      SDWTAM = SDWT
      SDWTAH = SDWT * HARVFRAC(1) !chp 02/03/2005
!      SDWTAH = GRNWT * PLANTS * HARVFRAC(1)   !CHP 12/19/2003
      TOPWT  = BIOMAS*PLANTS                  !Was BIOMAS

      YRNR1  = ISDATE
      YRNR2  = STGDOY(2)
      YRNR3  = STGDOY(3)
      YRNR5  = STGDOY(5)
      IF (YRPLT .GT. 0) THEN
        YRNR7  = MDATE
      ELSE
        YRNR7  = -99
      ENDIF

!      WTNUP  = TOTNUP/10.0
!      WTNCAN = TOTNUP/10.0
!      WTNSD  = GNUP  /10.0
      WTNUP  = (GRAINN + STOVN) * PLANTS      !CHP 12/19/2003
      WTNCAN = WTNUP                          !CHP 12/19/2003
      WTNSD  = GRAINN * PLANTS                !CHP 12/19/2003

      PSDWT = 0.0
      IF (SEEDNO .GT. 0.0 .AND. SDWT .GE. 0.0) THEN
         PSDWT = SDWT/SEEDNO
      ENDIF

      IF (BIOMAS .GT. 0.0 .AND. GRNWT .GE. 0.0) THEN
         HI = GRNWT / BIOMAS
       ELSE
         HI = 0.0
      ENDIF

C-----------------------------------------------------------------------
C     Actual byproduct harvested (default is 0 %)
C     Byproduct not harvested is incorporated
C-----------------------------------------------------------------------
      BWAH   = STOVER * HARVFRAC(2)

!-----------------------------------------------------------------------
!     Read Measured (measured) data from FILEA
!-----------------------------------------------------------------------
      IF ((INDEX('YE',IDETO) > 0 .OR. INDEX('IAEBCGDT',RNMODE) > 0) .OR.
     &    (INDEX('AY',ISWITCH%IDETS) > 0 .AND. CONTROL%CROP .NE. 'FA')) 
     &  THEN
         IF (INDEX('FQ',RNMODE) > 0) THEN
           TRT_ROT = CONTROL % ROTNUM
         ELSE
           TRT_ROT = TRTNUM
         ENDIF
         !CALL READA (FILEA, PATHEX,OLAB, TRT_ROT, YRSIM, X)
         CALL READA_Y4K(FILEA, PATHEX,OLAB, TRT_ROT, YRSIM, X)

!       Convert from YRDOY format to DAP.  Change descriptions to match.
!       CALL READA_Dates(X(2), YRSIM, IFLR)
        CALL READA_Dates(X(2), YRPLT, IFLR)
        IF (IFLR .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DFLR = TIMDIF(YRPLT,IFLR)
        ELSE
          DFLR  = -99
        ENDIF
        OLAP(2) = 'ADAP  '
        CALL GetDesc(1,OLAP(2), DESCRIP(2))

        CALL READA_Dates(X(3), YRSIM, IMAT)
        IF (IMAT .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DMAT = TIMDIF(YRPLT,IMAT)
        ELSE
          DMAT = -99
        ENDIF
        OLAP(3) = 'MDAP  '
        CALL GetDesc(1,OLAP(3), DESCRIP(3))

        CALL READA_Dates(X(1), YRSIM, IPIN)
        IF (IPIN .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DPIN = TIMDIF (YRPLT,IPIN)
        ELSE
          DPIN = -99
        ENDIF
        OLAP(1) = 'IDAP  '
        CALL GetDesc(1,OLAP(1), DESCRIP(1))

        CALL READA_Dates(X(19), YRSIM, IEMRG)
        IF (IEMRG .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DEMRG = TIMDIF(YRPLT,IEMRG)
        ELSE
          DEMRG  = -99
        ENDIF
        OLAP(19) = 'EDAP  '
        CALL GetDesc(1,OLAP(19), DESCRIP(19))

        DNR1 = TIMDIF (YRPLT,ISDATE)
        IF (DNR1 .LE. 0) THEN
           DNR1 = -99
        ENDIF

        DNR7 = TIMDIF (YRPLT,MDATE)
        IF (DNR7 .LE. 0 .OR. YRPLT .LT. 0) THEN
           DNR7 = -99
        ENDIF

        DNR0 = TIMDIF (YRPLT,YRNR2)
        IF (DNR0 .LE. 0) THEN
           DNR0 = -99
        ENDIF

        IF (STGDOY(9) < 9999999) THEN
          YREMRG = STGDOY(9)    !emergence  
        ELSE
          YREMRG = STGDOY(11)   !transplant
        ENDIF
        IF (YRPLT .GT. 0) THEN
          D_emerge = TIMDIF (YRPLT,YREMRG)
          IF (D_emerge .LE. 0)  THEN
            D_emerge = -99
          ENDIF
        ELSE
          D_emerge = -99
        ENDIF

      YIELD  = NINT(DYIELD)
      PLTPOP = PLANTS

!     Write values to Simulated and Measured arrays
      WRITE(Simulated(1), '(I8)') DNR0;   WRITE(Measured(1),'(I8)') DPIN
      WRITE(Simulated(2), '(I8)') DNR1;   WRITE(Measured(2),'(I8)') DFLR
      WRITE(Simulated(3), '(I8)') DNR7;   WRITE(Measured(3),'(I8)') DMAT
      WRITE(Simulated(4), '(I8)') YIELD;  
                              WRITE(Measured(4),'(A8)') TRIM(X(4))
      WRITE(Simulated(5),'(F8.4)')SKERWT; 
                              WRITE(Measured(5),'(A8)') TRIM(X(5))
      WRITE(Simulated(6), '(I8)') NINT(GPSM)                            
                              WRITE(Measured(6),'(A8)') TRIM(X(6))
      WRITE(Simulated(7),'(F8.1)') (TILNO+1.)*PLTPOP                    
                              WRITE(Measured(7),'(A8)') TRIM(X(7))
      WRITE(Simulated(8),'(F8.1)')MAXLAI; 
                              WRITE(Measured(8),'(A8)') TRIM(X(8))
      WRITE(Simulated(9),'(I8)') NINT(CANWAA*10)                        
                              WRITE(Measured(9),'(A8)') TRIM(X(9))
      WRITE(Simulated(10),'(I8)') NINT(CANNAA*10)                       
                              WRITE(Measured(10),'(A8)') TRIM(X(10))
      WRITE(Simulated(11),'(I8)') NINT(PBIOMS)                          
                              WRITE(Measured(11),'(A8)') TRIM(X(11))

!     08/11/2005 CHP changed from BWAH to BWAM, value remains the same (STOVER)
      WRITE(Simulated(12),'(I8)') NINT(STOVER) 
                              WRITE(Measured(12),'(A8)') TRIM(X(12))

      WRITE(Simulated(13),'(F8.3)') HI;  
                              WRITE(Measured(13),'(A8)') TRIM(X(13))
      WRITE(Simulated(14),'(I8)') LEAFNO;
                              WRITE(Measured(14),'(A8)') TRIM(X(14))
      WRITE(Simulated(15),'(I8)') NINT(GNUP)                            
                              WRITE(Measured(15),'(A8)') TRIM(X(15))
      WRITE(Simulated(16),'(I8)') NINT(TOTNUP)                          
                              WRITE(Measured(16),'(A8)') TRIM(X(16))
      WRITE(Simulated(17),'(I8)') NINT(APTNUP)
                              WRITE(Measured(17),'(A8)') TRIM(X(17))
      WRITE(Simulated(18),'(F8.2)') XGNP;
                              WRITE(Measured(18),'(A8)') TRIM(X(18))
      WRITE(Simulated(19),'(I8)')D_emerge
                              WRITE(Measured(19),'(I8)') DEMRG
      ENDIF  

!-----------------------------------------------------------------------
!     Send data to OPSUM for SUMMARY.OUT file.
!-----------------------------------------------------------------------
!      IF (IDETS .EQ. 'Y' .OR. IDETS .EQ. 'A') THEN
!     Compute values to be sent to OPSUM for SUMMARY.OUT file.
C-------------------------------------------------------------------
!      SDRATE  = PSDWT*PLTPOP/0.8*10

!      PSDWT   = SKERWT
C     SDRATE  = PSDWT * PLTPOP * 10.0
      SDRATE  = G2 * AMAX1(PLANTS,PLTPOP) * 10.0
      PSDWT   = SKERWT
      SEEDNO  = GPSM
      PSPP    = GPP

      SDRATE  = PSDWT*PLTPOP/0.8*10

      PODWT = PANWT * PLTPOP  !g/m2 CHP 7/21/2005 was * 10.
      BWAH = BWAH/10.         !Converts to g/m2
!       Store Summary.out labels and values in arrays to send to
!       OPSUM routines for printing.  Integers are temporarily 
!       saved as real numbers for placement in real array.
        LABEL(1)  = 'ADAT'; VALUE(1)  = FLOAT(YRNR1)
        LABEL(2)  = 'MDAT'; VALUE(2)  = FLOAT(YRNR7)
        LABEL(3)  = 'DWAP'; VALUE(3)  = SDRATE
        LABEL(4)  = 'CWAM'; VALUE(4)  = TOPWT*10.
        LABEL(5)  = 'HWAM'; VALUE(5)  = SDWTAM*10.
        LABEL(6)  = 'HWAH'; VALUE(6)  = SDWTAH*10.
        LABEL(7)  = 'BWAH'; VALUE(7)  = BWAH
        LABEL(8)  = 'HWUM'; VALUE(8)  = PSDWT !*1000.
        LABEL(9)  = 'H#AM'; VALUE(9)  = SEEDNO
        LABEL(10) = 'H#UM'; VALUE(10) = PSPP
        LABEL(11) = 'NFXM'; VALUE(11) = 0.0
        LABEL(12) = 'NUCM'; VALUE(12) = WTNUP*10.
        LABEL(13) = 'CNAM'; VALUE(13) = WTNCAN*10.
        LABEL(14) = 'GNAM'; VALUE(14) = WTNSD*10.
        LABEL(15) = 'PWAM'; VALUE(15) = PODWT*10. !chp 7/21/05 added *10
        LABEL(16) = 'LAIX'; VALUE(16) = MAXLAI
        LABEL(17) = 'HIAM'; VALUE(17) = HI
        LABEL(18) = 'EDAT'; VALUE(18) = FLOAT(YREMRG)
        
        !Send labels and values to OPSUM
        CALL SUMVALS (SUMNUM, LABEL, VALUE) 
!      ENDIF

      CALL OPVIEW(CONTROL, 
     &    PBIOMS, ACOUNT, DESCRIP, IDETO, VSTAGE, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, XLAI, YIELD, YRPLT, ISTAGE)

      !Send Measured and Simulated datat to OPSUM
      CALL EvaluateDat (ACOUNT, Measured, Simulated, DESCRIP, OLAP) 

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE TEFF_OPHARV

