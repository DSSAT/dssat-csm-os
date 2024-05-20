C=======================================================================
C  TR_OPHARV, Subroutine
C
C  Write the harvest report
C-----------------------------------------------------------------------
C  Revision history
C
C  12/13/2004 MUS/RO Converted to modular format for inclusion in CSM.
C=======================================================================

      SUBROUTINE TR_OPHARV (CONTROL, ISWITCH, 
     &    AGEFAC, APTNUP, BIOMAS, CANNAA, CANWAA,         !Input
     &    CORMLNO, CORMN, CORMNUP, CORMWT, DYIELD, G2,    !Input
     &    HARVFRAC, ISDATE, ISTAGE, LAI, LEAFNO, MAXLAI,  !Input
     &    MDATE, NSTRES, PBIOMS, PCORMN, PLANTS, PLTPOP,  !Input
     &    SKERWT, STGDOY, STNAME, STOVER, STOVN, SWFAC,   !Input
     &    TOTNUP, TURFAC, YRPLT,                          !Input
     &    BWAH, SDWT, SDWTAH, TOPWT, WTNSD,               !Output
     &    MCORMWT)    !, TCORMWT ) !additions RMO
!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL FIND, ERROR, GETDESC, OPVIEW, READA, READA_Dates, 
     &  SUMVALS, EvaluateDat, TIMDIF, READA_Y4K
      SAVE

      CHARACTER*1  IDETO, IPLTI, RNMODE
      CHARACTER*6  SECTION
      CHARACTER*6, PARAMETER :: ERRKEY = 'OPHARV'
      CHARACTER*10 STNAME(20)
      CHARACTER*12 FILEA
      CHARACTER*30 FILEIO
      CHARACTER*80 PATHEX

      INTEGER TRTNUM
      INTEGER TRT_ROT
      INTEGER ISDATE,ISENS
      INTEGER YRDOY, YRPLT,YRSIM,YRNR1,YRNR2,YRNR7  !,YRNR3,YRNR5
      INTEGER DNR1,DNR7,MDATE,STGDOY(20)
      INTEGER DYNAMIC, LUNIO, ACOUNT, ERRNUM, LINC, LNUM, FOUND
      INTEGER DFLR, DMAT, LEAFNO, YIELD, ISTAGE
      INTEGER DNR0, IFLR, TIMDIF, IMAT    !, IPIN 

      REAL MAXLAI,TOTNUP,PSDWT,PSPP,HI, LAI !,ACREFC
      REAL WTNCAN,PLANTS
      REAL WTNSD,WTNUP,PLTPOP,PBIOMS,SDWTAM,APTNUP
      REAL SKERWT,STOVER,STOVN, SEEDNO,SDWT,G2
      REAL TOPWT,CORMLNO,CORMNUP,CORMN, BIOMAS,PCORMN
      REAL VSTAGE, NSTRES, TURFAC ,XLAI
      REAL SDRATE, DYIELD, PODWT
      REAL CMOIST, CANWAA,CANNAA,CORMWT,BWAH,SDWTAH
      REAL Pstres1, Pstres2   
      REAL AGEFAC, SWFAC
      REAL MCORMWT  !, TCORMWT   !additions RMO

      REAL, DIMENSION(2) :: HARVFRAC

!      PARAMETER (ACREFC = 2.47)

!     Arrays which contain data for printing in SUMMARY.OUT file
!       (OPSUM subroutine)
      INTEGER, PARAMETER :: SUMNUM = 17
      CHARACTER*4, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

!     Arrays which contain predicted and Measured data for printing
!       in OVERVIEW.OUT and EVALUATE.OUT files (OPVIEW subroutine)
      CHARACTER*6 OLAB(40), OLAP(40)  !OLAP modified for dap
      CHARACTER*12 X(40)
      CHARACTER*8 Simulated(40), Measured(40)
      CHARACTER*50 DESCRIP(40)

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
      RNMODE = CONTROL % RNMODE
      YRSIM  = CONTROL % YRSIM

      IDETO = ISWITCH % IDETO
      IPLTI = ISWITCH % IPLTI

      ACOUNT = 17  !Number of possible FILEA headers for this crop

!     Define headings for observed data file (FILEA)
      DATA OLAB / ! 
                !   
       !&  'IDAT', ! 1 End of establishment phase (YrDoy)
     &  'ADAT  ', ! 1 Maximum vegetative growth (YrDoy)              
     &  'MDAT  ', ! 2 Harvest maturity date (YrDoy)        
     &  'UYAH  ', ! 3 Fresh Corm Yield at maturity (kg/ha)
c
     &  'UWAH  ', ! 4 TUBER Dry Yield (kg/ha)
c     &  'GW%M  ', ! 4 Product Moisture Content (%)
     &  'HWUM  ', ! 5 Unit corm wt at maturity (g dm/unit)
     &  'H#AM  ', ! 6 Number cormels at maturity (no/m2)
       !&  'P#AM', ! 7 PANICLE NUMBER (PANICLE/m2) 
     &  'LAIX  ', ! 7 Maximum Leaf area index 
     &  'CWAA  ', ! 8 Tot DM wt at max veg growth (kg dm/ha)
     &  'CNAA  ', ! 9 Tops N at at max veg growth (kg/ha) 
     &  'TWAH  ', !10 Tot DM wt at maturity (kg dm/ha)
     &  'BWAM  ', !11 By-product harvest (kg dm/ha)
     &  'HIAM  ', !12 Harvest index at maturity
     &  'L#SX  ', !13 Leaf number per stem,maximum
     &  'UNAM  ', !14 Corm N at maturity (kg/ha) 
     &  'TNAH  ', !15 TOTAL N at maturity (kg/ha) 
     &  'CNAM  ', !16 Tops N at maturity (kg/ha)
     &  'UN%H  ', !17 Corm N at maturity (%)
!     &  'PD1T', !19 
!     &  'PDFT', !20       
!     &  'PWAM', !21 
!     &  'THAM', !22 
     &  23*'      '/

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
      READ (LUNIO,'(3(/),15X,A12,1X,A80)',IOSTAT=ERRNUM) FILEA,
     &     PATHEX
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
      
!     Establish # and names of stages for environmental stress summary
      PlantStres % ACTIVE = .FALSE.
      PlantStres % NSTAGES = 5
      PlantStres % StageName(1) = 'Establishment phase '
      PlantStres % StageName(2) = 'Initial growth phase'
      PlantStres % StageName(3) = 'Maximum growth phase'
      PlantStres % StageName(4) = 'Corm growth'
      PlantStres % StageName(5) = 'Cormel growth    '

      CALL OPVIEW(CONTROL, 
     &    PBIOMS, ACOUNT, DESCRIP, IDETO, VSTAGE, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, XLAI, YIELD, YRPLT, ISTAGE)

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
C-----------------------------------------------------------------------
      VSTAGE = REAL(LEAFNO)
      !WTNCAN = TOTNUP/10.0
      WTNCAN = (STOVN + CORMN)*PLANTS
      XLAI = LAI

      PlantStres % W_grow = TURFAC
      PlantStres % W_phot = SWFAC 
      PlantStres % N_phot = NSTRES
      PlantStres % N_grow = AGEFAC
      PlantStres % P_phot = PSTRES1
      PlantStres % P_grow = PSTRES2
      PlantStres % ACTIVE = .FALSE.

      IF (ISTAGE > 0 .AND. ISTAGE < 6) THEN
        PlantStres % ACTIVE(ISTAGE) = .TRUE.
      ENDIF

      YRDOY = CONTROL % YRDOY
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
      SDWTAM = DYIELD / 10.0
!      SDWTAH = SDWT * HARVFRAC(1)
      SDWTAH = CORMWT * PLANTS * HARVFRAC(1) ! total corm dry wt (kg/ha)
      TOPWT  = BIOMAS*PLANTS                  !Was BIOMAS

      YRNR1  = ISDATE
      YRNR2  = STGDOY(2)
      !YRNR3  = STGDOY(3)
      !YRNR5  = STGDOY(5)
      IF (YRPLT .GT. 0) THEN
        YRNR7  = MDATE
      ELSE
        YRNR7  = -99
      ENDIF

      WTNUP  = (CORMN + STOVN) * PLANTS   !Total N uptake
      WTNCAN = WTNUP                   
      WTNSD  = CORMN * PLANTS  !(corm N uptake kg N/ha)       

      PSDWT = 0.0

      IF (BIOMAS .GT. 0.0 .AND. CORMWT .GE. 0.0) THEN
         HI = CORMWT / BIOMAS
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
      IF(IDETO .EQ. 'Y' .OR. INDEX('IAEBCGD',RNMODE) .GT. 0) THEN
         IF (INDEX('FQ',RNMODE) > 0) THEN
           TRT_ROT = CONTROL % ROTNUM
         ELSE
           TRT_ROT = TRTNUM
         ENDIF
        !CALL READA(FILEA, PATHEX, OLAB, TRT_ROT, YRSIM, X)
        CALL READA_Y4K(FILEA, PATHEX,OLAB, TRT_ROT, YRSIM, X)

!       Convert from YRDOY format to DAP.  Change descriptions to match.
        CALL READA_Dates(X(1), YRSIM, IFLR)
        IF (IFLR .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DFLR = TIMDIF(YRPLT,IFLR)
        ELSE
          DFLR  = -99
        ENDIF
        OLAP(1) = 'ADAP  '
        CALL GetDesc(1,OLAP(1), DESCRIP(1))

        CALL READA_Dates(X(2), YRSIM, IMAT)
        IF (IMAT .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DMAT = TIMDIF(YRPLT,IMAT)
        ELSE
          DMAT = -99
        ENDIF
        OLAP(2) = 'MDAP  '
        CALL GetDesc(1,OLAP(2), DESCRIP(2))

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
!     CHP 1/11/2005 Need to supply this value from somewhere!
      CMOIST = 200.0    ! set moisture to 200% RMO

!     fresh yield calc. RMO
      YIELD  = NINT(DYIELD) + NINT(CMOIST*DYIELD/100.0) 
      PLTPOP = PLANTS

C
!     Write values to Simulated and Measured arrays
!      WRITE(Simulated(1), '(I8)') DNR0;   WRITE(Measured(1),'(I8)') DPIN 
      WRITE(Simulated(1), '(I8)') DNR1;   
                                    WRITE(Measured(1),'(A8)') TRIM(X(1)) 
      WRITE(Simulated(2), '(I8)') DNR7;   
                                    WRITE(Measured(2),'(I8)') DMAT
      WRITE(Simulated(3), '(I8)') YIELD/1000;  
                                    WRITE(Measured(3),'(A8)') TRIM(X(3))
      WRITE(Simulated(4),'(F8.1)')DYIELD; 
                                    WRITE(Measured(4),'(A8)') TRIM(X(4)) 
      WRITE(Simulated(5),'(F8.2)')MCORMWT;
                                    WRITE(Measured(5),'(A8)') TRIM(X(5)) 
      WRITE(Simulated(6), '(I8)') NINT(CORMLNO)                             
                                    WRITE(Measured(6),'(A8)') TRIM(X(6)) 
      WRITE(Simulated(7),'(F8.2)')MAXLAI; 
                                    WRITE(Measured(7),'(A8)') TRIM(X(7)) 
      WRITE(Simulated(8),'(I8)') NINT(CANWAA*10)                         
                                    WRITE(Measured(8),'(A8)') TRIM(X(8)) 
      WRITE(Simulated(9),'(I8)') NINT(CANNAA*10)                        
                                    WRITE(Measured(9),'(A8)')TRIM(X(9)) 
      WRITE(Simulated(10),'(I8)') NINT(PBIOMS)                           
                                   WRITE(Measured(10),'(A8)')TRIM(X(10)) 
      WRITE(Simulated(11),'(I8)') NINT(STOVER)                           
                                   WRITE(Measured(11),'(A8)')TRIM(X(11))
      WRITE(Simulated(12),'(F8.3)') HI;  
                                   WRITE(Measured(12),'(A8)')TRIM(X(12)) 
      WRITE(Simulated(13),'(I8)') LEAFNO;
                                   WRITE(Measured(13),'(A8)')TRIM(X(13)) 
      WRITE(Simulated(14),'(I8)') NINT(CORMNUP)                             
                                   WRITE(Measured(14),'(A8)')TRIM(X(14)) 
      WRITE(Simulated(15),'(I8)') NINT(TOTNUP)                           
                                   WRITE(Measured(15),'(A8)')TRIM(X(15))
      WRITE(Simulated(16),'(I8)') NINT(APTNUP)
                                   WRITE(Measured(16),'(A8)')TRIM(X(16))
      WRITE(Simulated(17),'(F8.2)') PCORMN
	                             WRITE(Measured(17),'(A8)')TRIM(X(17))
      ENDIF  


!-----------------------------------------------------------------------
!     Send data to OPSUM for SUMMARY.OUT file.
!-----------------------------------------------------------------------
!      IF (IDETS .EQ. 'Y' .OR. IDETS .EQ. 'A') THEN
!     Compute values to be sent to OPSUM for SUMMARY.OUT file.
C-------------------------------------------------------------------

      SDRATE  = G2 * AMAX1(PLANTS,PLTPOP) * 10.0
      PSDWT   = SKERWT
      PSPP    = 0.0   !CHP 1/11/2005
      SEEDNO  = 0.0   !CHP 1/11/2005

      SDRATE  = PSDWT*PLTPOP/0.8*10

      PODWT = CORMWT * PLANTS   !?? chp 8/18/2005

!       Store Summary.out labels and values in arrays to send to
!       OPSUM routines for printing.  Integers are temporarily 
!       saved aS real numbers for placement in real array.
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
      END SUBROUTINE TR_OPHARV

