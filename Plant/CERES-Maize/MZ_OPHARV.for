!=======================================================================
!  MZ_OPHARV, Subroutine C.H. Porter
!  Generates data for use by OPSUM and OVERVIEW for MAIZE.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  01/25/2002 CHP Written, based on OPHARV
!  03/03/2002 GH  Modified logic for calling of fileA
!  08/12/2003 CHP Added I/O error checking and changed call to READA
!  02/04/2005 CHP Added PODWT, LAI, HI to Summary.out output
!  08/11/2005 CHP Use BWAH as byproduct variable for Summary.OUT 
!                 (byproduct harvested), BWAM as byproduct variable 
!                 for Overview.OUT (byproduct produced).  Both variables
!                 now include senesced stover.
!  10/24/2005 CHP Added environmental & stress factors to Overview.OUT
!  07/13/2006 CHP Added P model
C  02/09/2007 GH  Add path for FileA
!  08/28/2009 CHP added EDAT, EDAP 
!=======================================================================

      SUBROUTINE MZ_OPHARV(CONTROL, 
     &    AGEFAC, APTNUP, CANNAA, CANWAA, GNUP, GPP,      !Input
     &    GPSM,HARVFRAC, IDETO, IDETS, IPLTI, ISDATE,     !Input
     &    ISTAGE, MDATE, NSTRES, PODWT, PSTRES1, PSTRES2, !Input
     &    SEEDNO, SENESCE, SKERWT, STGDOY, STOVER, SWFAC, !Input
     &    TOPWT, TURFAC,WTNCAN, WTNUP, XGNP, XLAI, XN,    !Input
     &    YIELD, YREMRG, YRPLT,                           !Input
     &    BWAH, SDWTAH)                                   !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, ERROR, GETDESC, OPVIEW, READA, 
     &  READA_Dates, SUMVALS, TIMDIF, EvaluateDat, READA_Y4K
      SAVE

      CHARACTER*1  IDETO, IDETS, IPLTI, RNMODE
      CHARACTER*2  CROP
      CHARACTER*6  SECTION
      CHARACTER*6, PARAMETER :: ERRKEY = 'OPHARV'
      CHARACTER*10 STNAME(20)
      CHARACTER*12 FILEA
      CHARACTER*30 FILEIO
	CHARACTER*80 PATHEX

      INTEGER DEMRG, DFLR, DMAT, IFPD, DFPD, IFSD, DFSD
      INTEGER DNR0, DNR1, DNR7, DYNAMIC, ERRNUM, FOUND
      INTEGER IEMRG, IFLR, IMAT
      INTEGER ISDATE, ISENS, LINC, LNUM, LUNIO, MDATE, ISTAGE, RUN
      INTEGER TIMDIF, TRTNUM, YRNR1, YRNR2, YRNR3
      INTEGER YRDOY, YREMRG, YRNR5, YRSIM, YRPLT
      INTEGER TRT_ROT
      INTEGER STGDOY(20)
      
      REAL AGEFAC, APTNUP, BWAH, BWAM, CANNAA, CANWAA
      REAL GNUP, GPP, GPSM, HI, StovSenes
      REAL MAXLAI, NSTRES, PBIOMS, PODWT, PSDWT
      REAL Pstres1, Pstres2   
      REAL SDRATE
      REAL SDWT, SDWTAH, SDWTAM, SEEDNO, SKERWT, STOVER
      REAL SWFAC, TOPWT, TURFAC
      REAL WTNCAN, WTNUP, XGNP, XLAI, XN
      REAL YIELD 

      REAL, DIMENSION(2) :: HARVFRAC

!     Arrays which contain data for printing in SUMMARY.OUT file
      INTEGER, PARAMETER :: SUMNUM = 18
      CHARACTER*4, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

!     Arrays which contain Simulated and Measured data for printing
!       in OVERVIEW.OUT and EVALUATE.OUT files (OPVIEW subroutine)
      INTEGER ACOUNT
      CHARACTER*6, DIMENSION(EvaluateNum) :: OLAB, OLAP !OLAP in dap
      CHARACTER*12 X(EvaluateNum)
      CHARACTER*8 Simulated(EvaluateNum), Measured(EvaluateNum)
      CHARACTER*50 DESCRIP(EvaluateNum)

      TYPE (ControlType) CONTROL
      TYPE (ResidueType) SENESCE

!     Variables added for environmental and stress factors output
      TYPE (PlStresType) PlantStres

      DYNAMIC = CONTROL % DYNAMIC
      CROP    = CONTROL % CROP
      YRSIM   = CONTROL % YRSIM
      RNMODE  = CONTROL % RNMODE
      FILEIO  = CONTROL % FILEIO
      RUN     = CONTROL % RUN

      StovSenes = SENESCE % ResWt(0)

!-----------------------------------------------------------------------
      ACOUNT = 22  !Number of FILEA headings.
!     Headings in FILEA for Measured data
      DATA OLAB /
     &    'ADAT  ',     !1  DFLR 
     &    'PD1T  ',     !2  IFPD 
     &    'PDFT  ',     !3  IFSD 
     &    'MDAT  ',     !4  DMAT 
     &    'HWAM  ',     !5  XGWT 
     &    'PWAM  ',     !6  XPDW 
     &    'H#AM  ',     !7  XNOGR
     &    'HWUM  ',     !8  XGWU 
     &    'H#UM  ',     !9  XNOGU
     &    'CWAM  ',     !10 XCWT 

!         08/11/2005 CHP
!         Change BWAH to BWAM -- by-product produced to maturity, but
!           not necessarily removed from field
!     &    'BWAH',     !11 XSWT 
     &    'BWAM  ',     !11 XSWT 

     &    'LAIX  ',     !12 XLAM 
     &    'HIAM  ',     !13 XHIN 
     &    'THAM  ',     !14 XTHR 
     &    'GNAM  ',     !15 XNGR 
     &    'CNAM  ',     !16 XNTP 
     &    'SNAM  ',     !17 XNST 
     &    'GN%M  ',     !18 XNPS 
     &    'CWAA  ',     !19 XCWAA
     &    'CNAA  ',     !20 XCNAA
     &    'L#SM  ',     !21 XLFNO
     &    'EDAT  ',     !22 Emergence date
     &    18*'      '/  
 
!-----------------------------------------------------------------------
      DATA STNAME /   !Stage
     &  'End Juveni',   !1
     &  'Floral Ini',   !2
     &  '75% Silkin',   !3
     &  'Beg Gr Fil',   !4
     &  'End Gr Fil',   !5
     &  'Maturity  ',   !6
     &  'Sowing    ',   !7
     &  'Germinate ',   !8
     &  'Emergence ',   !9
     &  '          ',   !10
     &  '          ',   !11
     &  '          ',   !12
     &  '          ',   !13
     &  'Start Sim ',   !14
     &  'End Sim   ',   !15
     &  'Harvest   ',   !16
     &  '          ',   !17
     &  '          ',   !18
     &  '          ',   !19
     &  'Harvest   '/   !20

!***********************************************************************
!***********************************************************************
!     RUN INITIALIZATION
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
!       Read FILEIO
        CALL GETLUN('FILEIO', LUNIO)
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
     &    PBIOMS, ACOUNT, DESCRIP, IDETO, XN, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, XLAI, NINT(YIELD), YRPLT, ISTAGE)

!***********************************************************************
!***********************************************************************
!     SEASONAL INITIALIZATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      Simulated = ' '
      Measured  = ' '

!     Establish # and names of stages for environmental & stress summary
      PlantStres % ACTIVE = .FALSE.
      PlantStres % NSTAGES = 5

      PlantStres % StageName(0) = 'Planting to Harvest    '
      PlantStres % StageName(1) = 'Emergence-End Juvenile '
      PlantStres % StageName(2) = 'End Juvenil-Floral Init'
      PlantStres % StageName(3) = 'Floral Init-End Lf Grow'
      PlantStres % StageName(4) = 'End Lf Grth-Beg Grn Fil'
      PlantStres % StageName(5) = 'Grain Filling Phase    '

      APTNUP = 0.0
      CANWAA = 0.0
      CANNAA = 0.0
      MAXLAI = 0.0
      SDWT   = 0.0
      SKERWT = 0.0
      STOVER = 0.0
      StovSenes = 0.0
      XGNP   = 0.0
      XLAI   = 0.0
      XN     = 0.0
      YIELD  = 0.0

      YREMRG = -99
      YRNR1  = -99
      YRNR2  = -99
      YRNR3  = -99
      YRNR5  = -99
      ISDATE = -99

      CALL OPVIEW(CONTROL, 
     &    PBIOMS, ACOUNT, DESCRIP, IDETO, XN, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, XLAI, NINT(YIELD), YRPLT, ISTAGE)

      MAXLAI = 0.0

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
!-----------------------------------------------------------------------
      PBIOMS = TOPWT * 10.0
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

      YRDOY = CONTROL % YRDOY
      IF (YRDOY >= YRPLT) THEN
!        IF (MDATE < 0 .OR.
!     &     (MDATE > 0 .AND. YRDOY < MDATE)) THEN
!          PlantStres % ACTIVE(0) = .TRUE.
!        ENDIF
        IF (STGDOY(16) < 0 .OR.
     &     (STGDOY(16) > 0 .AND. YRDOY < STGDOY(16))) THEN
          PlantStres % ACTIVE(0) = .TRUE.
        ELSE
          PlantStres % Active(0) = .FALSE.
        ENDIF
      ENDIF

!     Send data to Overview.out data on days where stages occur
      CALL OPVIEW(CONTROL, 
     &    PBIOMS, ACOUNT, DESCRIP, IDETO, XN, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, XLAI, NINT(YIELD), YRPLT, ISTAGE)

!***********************************************************************
!***********************************************************************
!     Seasonal Output 
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
!     Transfer dates for model stages.
      YRNR1  = ISDATE
      YRNR2  = STGDOY(2)
      YRNR3  = STGDOY(3)
      YRNR5  = STGDOY(5)
      !WTNSD  = GNUP  /10.0

!-----------------------------------------------------------------------
!     Calculate variables for output
!     update nitrogen and residue applications after routines have been
!     modified to handle automatic management
!-----------------------------------------------------------------------
      IF (SEEDNO .GT. 0.0) THEN
         PSDWT = YIELD/(SEEDNO*10.)
      ELSE
         PSDWT = 0.0
      ENDIF
      IF (TOPWT .GT. 0.0 .AND. YIELD .GE. 0.0) THEN
         HI = YIELD/(TOPWT*10.0)
       ELSE
         HI = 0.0
      ENDIF

!-----------------------------------------------------------------------
!     Actual yield harvested (default is 100 %)
!-----------------------------------------------------------------------

      SDWT   = YIELD  / 10.0
      SDWTAM = SDWT
      SDWTAH = SDWT * HARVFRAC(1)

!-----------------------------------------------------------------------
!     Actual byproduct harvested (default is 0 %)
!     Byproduct not harvested is incorporated
! 08/11/2005 Senesced leaf and stem stay on plant and are
!         available for by-product harvest.
!-----------------------------------------------------------------------
      BWAM = STOVER + StovSenes

      BWAH   = (STOVER + StovSenes) * HARVFRAC(2) 
      PBIOMS = TOPWT * 10.0

!-----------------------------------------------------------------------
      IF ((INDEX('YE',IDETO) > 0 .OR. INDEX('IAEBCGDT',RNMODE) .GT. 0) 
     &  .OR. (INDEX('AY',IDETS) .GT. 0 .AND. CROP .NE. 'FA')) THEN
         IF (INDEX('FQ',RNMODE) > 0) THEN
           TRT_ROT = CONTROL % ROTNUM
         ELSE
           TRT_ROT = TRTNUM
         ENDIF
         !CALL READA (FILEA, PATHEX,OLAB, TRT_ROT, YRSIM, X)
         CALL READA_Y4K(FILEA, PATHEX,OLAB, TRT_ROT, YRSIM, X)

!-----------------------------------------------------------------------
!     Convert from YRDOY format to DAP.  Change descriptions to match.
        CALL READA_Dates(X(1), YRSIM, IFLR)
        IF (IFLR .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DFLR = TIMDIF(YRPLT,IFLR)
        ELSE
          DFLR  = -99
        ENDIF
        OLAP(1) = 'ADAP  '
        CALL GetDesc(1,OLAP(1), DESCRIP(1))

        CALL READA_Dates(X(2), YRSIM, IFPD)
        IF (IFPD .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DFPD = TIMDIF(YRPLT,IFPD)
        ELSE
          DFPD  = -99
        ENDIF
        OLAP(2) = 'PD1P  '
        CALL GetDesc(1,OLAP(2), DESCRIP(2))

        CALL READA_Dates(X(3), YRSIM, IFSD)
        IF (IFSD .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DFSD = TIMDIF(YRPLT,IFSD)
        ELSE
          DFSD  = -99
        ENDIF
        OLAP(3) = 'PDFP  '
        CALL GetDesc(1,OLAP(3), DESCRIP(3))

        CALL READA_Dates(X(4), YRSIM, IMAT)
        IF (IMAT .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DMAT = TIMDIF(YRPLT,IMAT)
        ELSE
          DMAT  = -99
        ENDIF
        OLAP(4) = 'MDAP  '
        CALL GetDesc(1,OLAP(4), DESCRIP(4))

!       08/28/2009 CHP added EDAT, EDAP 
        CALL READA_Dates(X(22), YRSIM, IEMRG)
        IF (IEMRG .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DEMRG = TIMDIF(YRPLT,IEMRG)
        ELSE
          DEMRG  = -99
        ENDIF
        OLAP(22) = 'EDAP  '
        CALL GetDesc(1,OLAP(22), DESCRIP(22))

        IF (YRPLT .GT. 0) THEN
          DNR1 = TIMDIF (YRPLT,ISDATE)
          IF (DNR1 .LE. 0) THEN
            DNR1 = -99
          ENDIF
        ELSE
          DNR1 = -99
        ENDIF
  
        IF (YRPLT .GT. 0) THEN
          DNR7 = TIMDIF (YRPLT,MDATE)
          IF (DNR7 .LE. 0)  THEN
            DNR7 = -99
          ENDIF
        ELSE
          DNR7 = -99
        ENDIF

        DNR0 = TIMDIF(YRPLT,YREMRG)
        IF (DNR0 .LE. 0 .OR. YRPLT .LE. 0) THEN
          DNR0 = -99
          YREMRG = -99
        ENDIF

      WRITE(Simulated(1),'(I8)') DNR1;  WRITE(Measured(1),'(I8)') DFLR    !ADAT
      WRITE(Simulated(2),'(I8)') -99 ;  WRITE(Measured(2),'(I8)') -99     !PD1T
      WRITE(Simulated(3),'(I8)') -99 ;  WRITE(Measured(3),'(I8)') -99     !PDFT
      WRITE(Simulated(4),'(I8)') DNR7;  WRITE(Measured(4),'(I8)') DMAT    !MDAT
      WRITE(Simulated(5),'(I8)') NINT(YIELD)
                              WRITE(Measured(5),'(A8)') TRIM(X(5))        !HWAM
      WRITE(Simulated(6),'(I8)') -99 ;  WRITE(Measured(6),'(I8)') -99     !PWAM
      WRITE(Simulated(7),'(I8)') NINT(GPSM)
                              WRITE(Measured(7),'(A8)') TRIM(X(7))        !H#AM
      WRITE(Simulated(8),'(F8.4)') SKERWT
                              WRITE(Measured(8),'(A8)') TRIM(X(8))        !HWUM 
      WRITE(Simulated(9),'(F8.1)') GPP; 
                              WRITE(Measured(9),'(A8)') TRIM(X(9))        !H#UM 
      WRITE(Simulated(10),'(I8)') NINT(PBIOMS)
                              WRITE(Measured(10),'(A8)') TRIM(X(10))      !CWAM

!     08/11/2005 CHP changed from BWAH to BWAM, 
      WRITE(Simulated(11),'(I8)') NINT(BWAM)  
                              WRITE(Measured(11),'(A8)') TRIM(X(11))      !BWAM

      WRITE(Simulated(12),'(F8.2)') MAXLAI
                              WRITE(Measured(12),'(A8)') TRIM(X(12))      !LAIX
      WRITE(Simulated(13),'(F8.3)') HI; 
                              WRITE(Measured(13),'(A8)') TRIM(X(13))      !HIAM
      WRITE(Simulated(14),'(I8)') -99 ; WRITE(Measured(14),'(I8)') -99    !THAM
      WRITE(Simulated(15),'(I8)') NINT(GNUP)
                              WRITE(Measured(15),'(A8)') TRIM(X(15))      !GNAM
      WRITE(Simulated(16),'(I8)') NINT(WTNCAN*10.)
                              WRITE(Measured(16),'(A8)') TRIM(X(16))      !CNAM
      WRITE(Simulated(17),'(I8)') NINT(APTNUP)
                              WRITE(Measured(17),'(A8)') TRIM(X(17))      !SNAM
      WRITE(Simulated(18),'(F8.1)')XGNP;
                              WRITE(Measured(18),'(A8)') TRIM(X(18))      !GN%M
      WRITE(Simulated(19),'(I8)') NINT(CANWAA*10)
                              WRITE(Measured(19),'(A8)') TRIM(X(19))      !CWAA
      WRITE(Simulated(20),'(I8)') NINT(CANNAA*10)
                              WRITE(Measured(20),'(A8)') TRIM(X(20))      !CNAA
      WRITE(Simulated(21),'(F8.2)') XN; 
                              WRITE(Measured(21),'(A8)') TRIM(X(21))      !L#SM
      WRITE(Simulated(22),'(I8)') DNR0; WRITE(Measured(22),'(I8)') DEMRG

      ENDIF

!-------------------------------------------------------------------
!     Send information to OPSUM to generate SUMMARY.OUT file
!-------------------------------------------------------------------
      PSDWT  = SKERWT
      SDRATE = -99.0

!     Store Summary.out labels and values in arrays to send to
!     OPSUM routines for printing.  Integers are temporarily 
!     saved as real numbers for placement in real array.
      LABEL(1)  = 'ADAT'; VALUE(1)  = FLOAT(YRNR1)
      LABEL(2)  = 'MDAT'; VALUE(2)  = FLOAT(MDATE)
      LABEL(3)  = 'DWAP'; VALUE(3)  = SDRATE
      LABEL(4)  = 'CWAM'; VALUE(4)  = TOPWT*10.
      LABEL(5)  = 'HWAM'; VALUE(5)  = SDWTAM*10.
      LABEL(6)  = 'HWAH'; VALUE(6)  = SDWTAH*10.
! BWAH multiplied by 10.0 in OPSUM - divide by 10. here to preserve units.
      LABEL(7)  = 'BWAH'; VALUE(7)  = BWAH / 10. 
      LABEL(8)  = 'HWUM'; VALUE(8)  = PSDWT       !*1000.
      LABEL(9)  = 'H#AM'; VALUE(9)  = SEEDNO
      LABEL(10) = 'H#UM'; VALUE(10) = GPP
      LABEL(11) = 'NFXM'; VALUE(11) = 0.0         !WTNFX*10.
      LABEL(12) = 'NUCM'; VALUE(12) = WTNUP*10.
      LABEL(13) = 'CNAM'; VALUE(13) = WTNCAN*10.
      LABEL(14) = 'GNAM'; VALUE(14) = GNUP        !WTNSD*10.
      LABEL(15) = 'PWAM'; VALUE(15) = PODWT*10.
      LABEL(16) = 'LAIX'; VALUE(16) = MAXLAI
      LABEL(17) = 'HIAM'; VALUE(17) = HI
      LABEL(18) = 'EDAT'; VALUE(18) = FLOAT(YREMRG)

      !Send labels and values to OPSUM
      CALL SUMVALS (SUMNUM, LABEL, VALUE) 

      CALL OPVIEW(CONTROL, 
     &    PBIOMS, ACOUNT, DESCRIP, IDETO, XN, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, XLAI, NINT(YIELD), YRPLT, ISTAGE)

      !Send Measured and Simulated datat to OPSUM
      CALL EvaluateDat (ACOUNT, Measured, Simulated, DESCRIP, OLAP) 

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE MZ_OPHARV
!=======================================================================



