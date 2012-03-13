!=======================================================================
!  OR_OPHARV, Subroutine
!
!  Write the harvest report
!-----------------------------------------------------------------------
!  Revision history
!  05/26/2011 CHP adapted for ORYZA
!=======================================================================

      SUBROUTINE OR_OPHARV (CONTROL, ISWITCH, 
     &   HARVFRAC, ISDATE, ISTAGE, LAI, LESTRS, MDATE,    !Input
     &   NGR, NSLLV, PCEW, STGDOY, STNAME,                !Input
     &   WAGT, WLVD, WLVG, WRR, WSO, WST, YRPLT)          !Input

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT     NONE
      SAVE

      CHARACTER*1  RNMODE, IDETO, IPLTI
      CHARACTER*6  SECTION
      CHARACTER*6, PARAMETER :: ERRKEY = 'OPHARV'
      CHARACTER*10 STNAME(20)
      CHARACTER*12 FILEA
      CHARACTER*30 FILEIO
	CHARACTER*80 PATHEX

      INTEGER IEMRG, ISENS, TRTNUM, TRT_ROT
      INTEGER ISDATE, YREMRG
      INTEGER YRDOY, YRPLT, YRSIM
      INTEGER YRNR1, YRNR2, YRNR3, YRNR5, YRNR7
      INTEGER DEMRG, D_emerge, DNR1,DNR7,MDATE,STGDOY(20)
      INTEGER DYNAMIC, LUNIO, RUN, ACOUNT, ERRNUM, LINC, LNUM, FOUND
      INTEGER DFLR, DMAT, YIELD, ISTAGE
      INTEGER DNR0, DPIN, IFLR, TIMDIF, IMAT, IPIN

      REAL MAXLAI, NGR, HIAM, LAI
      REAL WTNCAN, WTNSD, SDWTAM, SEEDNO
      REAL VSTAGE, NSTRES, XLAI
      REAL WRR, NSLLV
      REAL BWAH, SDWTAH
      REAL Pstres1, Pstres2   

      REAL WAGT, WST, WLVG, WLVD, WSO
      REAL PCEW, HWUM, LESTRS

      REAL, DIMENSION(2) :: HARVFRAC

!     Arrays which contain data for printing in SUMMARY.OUT file
!       (OPSUM subroutine)
      INTEGER, PARAMETER :: SUMNUM = 18
      CHARACTER*4, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

!     Arrays which contain simulated and measured data for printing
!       in OVERVIEW.OUT and EVALUATE.SUM files (OPVIEW subroutine)
      CHARACTER*6, DIMENSION(EvaluateNum) :: OLAB, OLAP !OLAP in dap
      CHARACTER*6 X(EvaluateNum)
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

      ACOUNT = 10  !Number of possible FILEA headers for this crop

!     Define headings for observed data file (FILEA)
      DATA OLAB / ! 
                  !new was    
     &  'EDAT  ', ! 1 !19 emergence date
     &  'ADAT  ', ! 2 ! 2 Anthesis date (YrDoy)              
     &  'IDAT  ', ! 3 ! 1 Panicle Initiation date (YrDoy)
     &  'MDAT  ', ! 4 ! 3 Physiological maturity date (YrDoy)        
     &  'CWAM  ', ! 5 !11 Tops weight at maturity (kg dm/ha)
     &  'HWAM  ', ! 6 ! 4 Yield at maturity (kg dm/ha)
     &  'HWUM  ', ! 7 ! 5 Unit wt at maturity (mg dm/unit)
     &  'H#AM  ', ! 8 ! 6 Number at maturity (no/m2)
     &  'HIAM  ', ! 9 !13 Harvest index at maturity
     &  'LAIX  ', !10 ! 8 Maximum Leaf area index 

!     From CERES-Rice:
!     &  'P#AM  ', ! 7 Panicle number (panicle/m2) 
!     &  'CWAA  ', ! 9 Tops weight at anthesis (kg dm/ha)
!     &  'CNAA  ', !10 Tops N at anthesis (kg/ha) 
!     &  'BWAM  ', !12 By-product produced to maturity (kg[dm]/ha)
!     &  'L#SM  ', !14 Leaf number per stem,maturity
!     &  'GNAM  ', !15 Grain N at maturity (kg/ha) 
!     &  'CNAM  ', !16 Tops N at maturity (kg/ha) 
!     &  'SNAM  ', !17 Stem N at maturity (kg/ha)
!     &  'GN%M  ', !18 Grain N at maturity (%)
!     &  'PD1T', !19 
!     &  'PDFT', !20       
!     &  'PWAM', !21 
!     &  'THAM', !22 

     &  30*'      '/

!***********************************************************************
!***********************************************************************
!     RUN INITIALIZATION
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
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

      NSTRES  = 1.0
      Pstres1 = 1.0
      Pstres2 = 1.0
      
      CALL OPVIEW(CONTROL, 
     &    WAGT, ACOUNT, DESCRIP, IDETO, VSTAGE, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, XLAI, YIELD, YRPLT, ISTAGE)

!***********************************************************************
!***********************************************************************
!     SEASONAL INITIALIZATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      Simulated = ' '
      Measured  = ' '
      YIELD     = 0
      WTNCAN    = 0.
      WTNSD     = 0.
      MAXLAI = 0.0
      
!     Establish #, names of stages for environmental & stress summary
      PlantStres % ACTIVE = .FALSE.
      PlantStres % NSTAGES = 0
      PlantStres % StageName(0) = 'Planting to Harvest    '
!      PlantStres % StageName(1) = 'Emergence-End Juvenile '
!      PlantStres % StageName(2) = 'End Juvenil-Panicl Init'
!      PlantStres % StageName(3) = 'Panicl Init-End Lf Grow'
!      PlantStres % StageName(4) = 'End Lf Grth-Beg Grn Fil'
!      PlantStres % StageName(5) = 'Grain Filling Phase    '

      CALL OPVIEW(CONTROL, 
     &    WAGT, ACOUNT, DESCRIP, IDETO, VSTAGE, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, XLAI, YIELD, YRPLT, ISTAGE)

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
!-----------------------------------------------------------------------
      MAXLAI = AMAX1 (MAXLAI, LAI)      ! Maximum XLAI season
      XLAI = LAI

      PlantStres % W_grow = 1.0 - LESTRS 
      PlantStres % W_phot = 1.0 - PCEW  
      PlantStres % N_grow = NSLLV 
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
     &    WAGT, ACOUNT, DESCRIP, IDETO, VSTAGE, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, XLAI, YIELD, YRPLT, ISTAGE)

!***********************************************************************
!***********************************************************************
!     Seasonal Output 
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
!     Actual yield harvested (default is 100 %)
!-----------------------------------------------------------------------
      SDWTAM = WRR
      SDWTAH = WRR * HARVFRAC(1) !chp 02/03/2005
      SEEDNO  = NGR / 1.E4    !#/m2 

!     Actual byproduct harvested (default is 0 %)
!     Byproduct not harvested is incorporated
      BWAH   = (WST + WLVG + WLVD) * HARVFRAC(2)

      IF (NGR > 0.0) THEN
        HWUM = WRR / NGR * 1.E3  !g/grain
      ELSE
        HWUM = 0.0
      ENDIF

      IF (WAGT > 0.0 .AND. WRR > 0.0) THEN
        HIAM = WRR / WAGT
      ELSE
        HIAM = 0.0
      ENDIF 

      YRNR1  = ISDATE
      YRNR2  = STGDOY(2)
      YRNR3  = STGDOY(3)
      YRNR5  = STGDOY(5)

      IF (YRPLT .GT. 0) THEN
        YRNR7  = MDATE
      ELSE
        YRNR7  = -99
      ENDIF

!-----------------------------------------------------------------------
!     Read Measured data from FILEA
!-----------------------------------------------------------------------
      IF ((INDEX('YE',IDETO) > 0 .OR. INDEX('IAEBCGDT',RNMODE) > 0) .OR.
     &    (INDEX('AY',ISWITCH%IDETS) > 0 .AND. CONTROL%CROP .NE. 'FA')) 
     &  THEN
         IF (INDEX('FQ',RNMODE) > 0) THEN
           TRT_ROT = CONTROL % ROTNUM
         ELSE
           TRT_ROT = TRTNUM
         ENDIF
         CALL READA (FILEA, PATHEX,OLAB, TRT_ROT, YRSIM, X)

!       Convert from YRDOY format to DAP.  Change descriptions to match.
!       Emergence - observed
        CALL READA_Dates(X(1), YRSIM, IEMRG)
        IF (IEMRG .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DEMRG = TIMDIF(YRPLT,IEMRG)
        ELSE
          DEMRG  = -99
        ENDIF
        OLAP(1) = 'EDAP  '
        CALL GetDesc(1,OLAP(1), DESCRIP(1))

!       Emergence - simulated
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

!       Anthesis, flowering, heading - observed
        CALL READA_Dates(X(2), YRSIM, IFLR)
        IF (IFLR .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DFLR = TIMDIF(YRPLT,IFLR)
        ELSE
          DFLR  = -99
        ENDIF
        OLAP(2) = 'ADAP  '
        CALL GetDesc(1,OLAP(2), DESCRIP(2))

!       Anthesis, flowering, heading - simulated
        DNR1 = TIMDIF (YRPLT,ISDATE)
        IF (DNR1 .LE. 0) THEN
           DNR1 = -99
        ENDIF

!       Panicle initiation - observed
        CALL READA_Dates(X(3), YRSIM, IPIN)
        IF (IPIN .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DPIN = TIMDIF (YRPLT,IPIN)
        ELSE
          DPIN = -99
        ENDIF
        OLAP(3) = 'IDAP  '
        CALL GetDesc(1,OLAP(3), DESCRIP(3))

!       Panicle initiation - simulated
        DNR0 = TIMDIF (YRPLT,YRNR2)
        IF (DNR0 .LE. 0) THEN
           DNR0 = -99
        ENDIF

!       Maturity - observed
        CALL READA_Dates(X(4), YRSIM, IMAT)
        IF (IMAT .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DMAT = TIMDIF(YRPLT,IMAT)
        ELSE
          DMAT = -99
        ENDIF
        OLAP(4) = 'MDAP  '
        CALL GetDesc(1,OLAP(4), DESCRIP(4))

!       Maturity - simulated
        DNR7 = TIMDIF (YRPLT,MDATE)
        IF (DNR7 .LE. 0 .OR. YRPLT .LT. 0) THEN
           DNR7 = -99
        ENDIF

      YIELD  = NINT(WRR)

!     &  'EDAT  ', ! 1 !19 emergence date
!     &  'ADAT  ', ! 2 ! 2 Anthesis date (YrDoy)              
!     &  'IDAT  ', ! 3 ! 1 Panicle Initiation date (YrDoy)
!     &  'MDAT  ', ! 4 ! 3 Physiological maturity date (YrDoy)        
!     &  'CWAM  ', ! 5 !11 Tops weight at maturity (kg dm/ha)
!     &  'HWAM  ', ! 6 ! 4 Yield at maturity (kg dm/ha)
!     &  'HWUM  ', ! 7 ! 5 Unit wt at maturity (mg dm/unit)
!     &  'H#AM  ', ! 8 ! 6 Number at maturity (no/m2)
!     &  'HIAM  ', ! 9 !13 Harvest index at maturity
!     &  'LAIX  ', !10 ! 8 Maximum Leaf area index 

!     Write values to Simulated and Measured arrays
      WRITE(Simulated(1),'(I8)')D_emerge; WRITE(Measured(1),'(I8)')DEMRG
      WRITE(Simulated(2),'(I8)') DNR1;    WRITE(Measured(2),'(I8)') DFLR
      WRITE(Simulated(3),'(I8)') DNR0;    WRITE(Measured(3),'(I8)') DPIN
      WRITE(Simulated(4),'(I8)') DNR7;    WRITE(Measured(4),'(I8)') DMAT
      WRITE(Simulated(5),'(I8)') NINT(WAGT)                          
                                          WRITE(Measured(5),'(A8)') X(5)
      WRITE(Simulated(6),'(I8)') NINT(SDWTAM)
                                          WRITE(Measured(6),'(A8)') X(6)
      WRITE(Simulated(7),'(F8.4)')HWUM;   WRITE(Measured(7),'(A8)') X(7)
      WRITE(Simulated(8), '(I8)') NINT(SEEDNO)                          
                                          WRITE(Measured(8),'(A8)') X(8)
      WRITE(Simulated(9),'(F8.3)') HIAM;  WRITE(Measured(9),'(A8)') X(9)
      WRITE(Simulated(10),'(F8.2)')MAXLAI
                                         WRITE(Measured(10),'(A8)')X(10)

!      WRITE(Simulated(7),'(F8.2)') (TILNO+1.)*PLTPOP                    
!                                          WRITE(Measured(7),'(A8)') X(7)
!      WRITE(Simulated(9),'(I8)') NINT(CANWAA*10)                        
!                                          WRITE(Measured(9),'(A8)') X(9)
!      WRITE(Simulated(10),'(I8)') NINT(CANNAA*10)                       
!                                         WRITE(Measured(10),'(A8)')X(10)
!!     08/11/2005 CHP changed from BWAH to BWAM, value remains the same (STOVER)
!      WRITE(Simulated(12),'(I8)') NINT(STOVER) 
!                                         WRITE(Measured(12),'(A8)')X(12)
!
!      WRITE(Simulated(14),'(I8)') LEAFNO;WRITE(Measured(14),'(A8)')X(14)
!      WRITE(Simulated(15),'(I8)') NINT(GNUP)                            
!                                         WRITE(Measured(15),'(A8)')X(15)
!      WRITE(Simulated(16),'(I8)') NINT(TOTNUP)                          
!                                         WRITE(Measured(16),'(A8)')X(16)
!      WRITE(Simulated(17),'(I8)') NINT(APTNUP)
!                                         WRITE(Measured(17),'(A8)')X(17)
!      WRITE(Simulated(18),'(F8.2)') XGNP;WRITE(Measured(18),'(A8)')X(18)
      ENDIF  

!-----------------------------------------------------------------------
!     Send data to OPSUM for SUMMARY.OUT file.
!-----------------------------------------------------------------------
!       Store Summary.out labels and values in arrays to send to
!       OPSUM routines for printing.  Integers are temporarily 
!       saved as real numbers for placement in real array.
        LABEL(1)  = 'ADAT'; VALUE(1)  = FLOAT(YRNR1)
        LABEL(2)  = 'MDAT'; VALUE(2)  = FLOAT(YRNR7)
        LABEL(3)  = 'DWAP'; VALUE(3)  = -99.  !SDRATE
        LABEL(4)  = 'CWAM'; VALUE(4)  = WAGT
        LABEL(5)  = 'HWAM'; VALUE(5)  = SDWTAM
        LABEL(6)  = 'HWAH'; VALUE(6)  = SDWTAH
        LABEL(7)  = 'BWAH'; VALUE(7)  = BWAH
        LABEL(8)  = 'HWUM'; VALUE(8)  = HWUM
        LABEL(9)  = 'H#AM'; VALUE(9)  = SEEDNO
        LABEL(10) = 'H#UM'; VALUE(10) = -99. !=seeds/pod
        LABEL(11) = 'NFXM'; VALUE(11) = 0.0
        LABEL(12) = 'NUCM'; VALUE(12) = -99.
        LABEL(13) = 'CNAM'; VALUE(13) = WTNCAN
        LABEL(14) = 'GNAM'; VALUE(14) = WTNSD
        LABEL(15) = 'PWAM'; VALUE(15) = WSO
        LABEL(16) = 'LAIX'; VALUE(16) = MAXLAI
        LABEL(17) = 'HIAM'; VALUE(17) = HIAM
        LABEL(18) = 'EDAT'; VALUE(18) = -99.

        !Send labels and values to OPSUM
        CALL SUMVALS (SUMNUM, LABEL, VALUE) 
!      ENDIF

      CALL OPVIEW(CONTROL, 
     &    WAGT, ACOUNT, DESCRIP, IDETO, VSTAGE, 
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
      END SUBROUTINE OR_OPHARV

!=======================================================================
!=======================================================================
