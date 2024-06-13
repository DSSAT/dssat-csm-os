!=======================================================================
!  SG_OPHARV, Subroutine C.H. Porter
!  Generates data for use by OPSUM and OVERVIEW for MAIZE.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  01/25/2002 CHP Written, based on OPHARV
!  03/03/2002 GH  Modified logic for calling of fileA
!  08/12/2003 CHP Added I/O error checking and changed call to READA
!  05/03/2004 CHP Added P stresses to OPVIEW call 
!  02/04/2005 CHP Added PODWT, LAIX and HI to Summary.out output
!  03/31/2005 CHP Multiply WTNCAN by 10. for output to Overview.out 
!  08/11/2005 CHP Use BWAH as byproduct variable for Summary.OUT 
!                 (byproduct harvested), BWAM as byproduct variable 
!                 for Overview.OUT (byproduct produced).  Both variables
!                 now include senesced stover.
!  10/24/2005 CHP Added environmental & stress factors to Overview.OUT
C  02/09/2007 GH  Added path for FileA
C  05/11/2007 GH  Added IDAT as output & renumber output variables
!  08/28/2009 CHP added EDAT, EDAP 
!=======================================================================

      SUBROUTINE SG_OPHARV(CONTROL, 
     &    AGEFAC, APTNUP, BIOMAS, CANNAA, CANWAA, GNUP,   !Input
     &    GPP, GPSM, HARVFRAC, IDETO, IDETS, IPLTI,       !Input
     &    ISDATE, ISTAGE, MAXLAI, MDATE, NSTRES, PODWT,   !Input
     &    SDWT, SEEDNO, SENESCE, SKERWT, STGDOY, STOVER,  !Input
     &    SWFAC, TOPWT, TURFAC, WTNCAN, WTNUP, XGNP,      !Input
     &    XLAI, XN, YIELD, YRPLT,                         !Input
     &    BWAH, SDWTAH)                                   !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, ERROR, OPVIEW, READA, READA_Dates, 
     &  GetDesc, SUMVALS, EvaluateDat, TIMDIF, READA_Y4K
      SAVE

      CHARACTER*1  IDETO, IDETS, IPLTI, RNMODE
      CHARACTER*2  CROP
      CHARACTER*6  SECTION
      CHARACTER*6, PARAMETER :: ERRKEY = 'OPHARV'
      CHARACTER*10 STNAME(20)
      CHARACTER*12 FILEA
      CHARACTER*30 FILEIO
      CHARACTER*80 PATHEX

      INTEGER DEMRG, DFLR, DMAT, DPIN
      INTEGER DNR1, DNR2, DNR7, DNR9, DYNAMIC, ERRNUM, FOUND
      INTEGER IEMRG, IFLR, IMAT, IPIN
      INTEGER ISDATE, ISENS, LINC, LNUM, LUNIO, MDATE, ISTAGE, RUN
      INTEGER TIMDIF, TRTNUM, YRNR1, YRNR2, YRNR3
      INTEGER YRDOY, YREMRG, YRNR5, YRSIM, YRPLT
      INTEGER TRT_ROT
      INTEGER STGDOY(20)
      
      REAL AGEFAC, APTNUP, BIOMAS, BWAH, BWAM, CANNAA, CANWAA
      REAL GNUP, GPP, GPSM, HI, StovSenes
      REAL MAXLAI, NSTRES, PBIOMS, PODWT, PSDWT
      REAL Pstres1, Pstres2   
      REAL SDRATE
      REAL SDWT, SDWTAH, SDWTAM, SEEDNO, SKERWT, STOVER
      REAL SWFAC, TOPWT, TURFAC
      REAL WTNCAN, WTNSD, WTNUP, XGNP, XLAI, XN
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
C-GH      ACOUNT = 21  !Number of FILEA headings.
	     ACOUNT = 23  !Number of FILEA headings.
!     Headings in FILEA for Measured data
      DATA OLAB /
     &    'IDAT  ',     !1  Panicle Initiation
     &    'ADAT  ',     !2  DFLR 
     &    'PD1T  ',     !3  IFPD 
     &    'PDFT  ',     !4  IFSD 
     &    'MDAT  ',     !5  DMAT 
     &    'HWAM  ',     !6  XGWT 
     &    'PWAM  ',     !7  XPDW 
     &    'H#AM  ',     !8  XNOGR
     &    'HWUM  ',     !9  XGWU 
     &    'H#UM  ',     !10 XNOGU
     &    'CWAM  ',     !11 XCWT 
                 
!         08/11/2005 CHP
!         Change BWAH to BWAM -- by-product produced to maturity, but
!           not necessarily removed from field
!     &    'BWAH',     !11 XSWT 
     &    'BWAM  ',     !12 XSWT 
                 
     &    'LAIX  ',     !13 XLAM 
     &    'HIAM  ',     !14 XHIN 
     &    'THAM  ',     !15 XTHR 
     &    'GNAM  ',     !16 XNGR 
     &    'CNAM  ',     !17 XNTP 
     &    'SNAM  ',     !18 XNST 
     &    'GN%M  ',     !19 XNPS 
     &    'CWAA  ',     !20 XCWAA
     &    'CNAA  ',     !21 XCNAA
     &    'L#SM  ',     !22 XLFNO
     &    'EDAT  ',     !23 Emergence date
     &    17*'      '/  
 
!-----------------------------------------------------------------------
      DATA STNAME /   !Stage
     &  'End Juveni',   !1
     &  'Panicle In',   !2
     &  'End Lf Gro',   !3
     &  'End Pan Gr',   !4
     &  'End Mn Fil',   !5
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
     &  'Anthesis  ',   !16
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

!     Establish #, names of stages for environmental & stress summary
      PlantStres % ACTIVE = .FALSE.
      PlantStres % NSTAGES = 5

      PlantStres % StageName(0) = 'Planting to Harvest    '
      PlantStres % StageName(1) = 'Emergence-End Juvenile '
      PlantStres % StageName(2) = 'End Juvenil-Panicle Ini'
      PlantStres % StageName(3) = 'Panicle Ini-End Lf Grow'
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

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
!-----------------------------------------------------------------------
      PBIOMS = BIOMAS * 10.0
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
        IF (MDATE < 0 .OR.
     &     (MDATE > 0 .AND. YRDOY <= MDATE)) THEN
          PlantStres % ACTIVE(0) = .TRUE.
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
      WTNSD  = GNUP  /10.0

!-----------------------------------------------------------------------
!     Calculate variables for output
!     update nitrogen and residue applications after routines have been
!     modified to handle automatic management
!-----------------------------------------------------------------------
      IF (SEEDNO .GT. 0.0) THEN
         PSDWT = SDWT/SEEDNO
      ELSE
         PSDWT = 0.0
      ENDIF
      IF (BIOMAS .GT. 0.0 .AND. YIELD .GE. 0.0) THEN
         HI = YIELD/(BIOMAS*10.0)
       ELSE
         HI = 0.0
      ENDIF

!-----------------------------------------------------------------------
!     Actual yield harvested (default is 100 %)
!-----------------------------------------------------------------------

      SDWT   = YIELD  / 10.0
      SDWTAM = SDWT
      SDWTAH = SDWT * HARVFRAC(1)
      TOPWT  = BIOMAS

!-----------------------------------------------------------------------
!     Actual byproduct harvested (default is 0 %)
!     Byproduct not harvested is incorporated
! 08/11/2005 Senesced leaf and stem stay on plant and are
!         available for by-product harvest.
!-----------------------------------------------------------------------
      BWAM = STOVER + StovSenes

      BWAH   = (STOVER + StovSenes) * HARVFRAC(2) 
      PBIOMS = BIOMAS * 10.0

!-----------------------------------------------------------------------
      IF ((INDEX('YE',IDETO) > 0 .OR. INDEX('IAEBCGD',RNMODE) .GT. 0) 
     &  .OR. (INDEX('AY',IDETS) .GT. 0 .AND. CROP .NE. 'FA')) THEN
         IF (INDEX('FQ',RNMODE) > 0) THEN
           TRT_ROT = CONTROL % ROTNUM
         ELSE
           TRT_ROT = TRTNUM
         ENDIF
        !CALL READA(FILEA, PATHEX, OLAB, TRT_ROT, YRSIM, X)
        CALL READA_Y4K(FILEA, PATHEX,OLAB, TRT_ROT, YRSIM, X)

!-----------------------------------------------------------------------
!     Convert from YRDOY format to DAP.  Change descriptions to match.

!       -----------------------------------------------------------
!       Panicle initiation - measured
        CALL READA_Dates(X(1), YRSIM, IPIN)
        IF (IPIN .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DPIN = TIMDIF (YRPLT,IPIN)
        ELSE
          DPIN = -99
        ENDIF
        OLAP(1) = 'IDAP  '
        CALL GetDesc(1,OLAP(1), DESCRIP(1))

!       Panicle initiation - simulated
        DNR2 = TIMDIF (YRPLT,YRNR2)
        IF (DNR2 .LE. 0 .OR. YRPLT .LE. 0) THEN
           DNR2 = -99
        ENDIF

!       -----------------------------------------------------------
!       Anthesis - measured
        CALL READA_Dates(X(2), YRSIM, IFLR)
        IF (IFLR .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DFLR = TIMDIF(YRPLT,IFLR)
        ELSE
          DFLR  = -99
        ENDIF
        OLAP(2) = 'ADAP  '
        CALL GetDesc(1,OLAP(2), DESCRIP(2))

!       Anthesis - simulated
        IF (YRPLT .GT. 0) THEN
          DNR1 = TIMDIF (YRPLT,ISDATE)
          IF (DNR1 .LE. 0) THEN
            DNR1 = -99
          ENDIF
        ELSE
          DNR1 = -99
        ENDIF
  
!       -----------------------------------------------------------
!       Maturity - measured
        CALL READA_Dates(X(5), YRSIM, IMAT)
        IF (IMAT .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DMAT = TIMDIF(YRPLT,IMAT)
        ELSE
          DMAT  = -99
        ENDIF
        OLAP(5) = 'MDAP  '
        CALL GetDesc(1,OLAP(5), DESCRIP(5))

!       Maturity - simulated
        IF (YRPLT .GT. 0) THEN
          DNR7 = TIMDIF (YRPLT,MDATE)
          IF (DNR7 .LE. 0)  THEN
            DNR7 = -99
          ENDIF
        ELSE
          DNR7 = -99
        ENDIF

!       -----------------------------------------------------------
!       Emergence - measured
        CALL READA_Dates(X(23), YRSIM, IEMRG)
        IF (IEMRG .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DEMRG = TIMDIF(YRPLT,IEMRG)
        ELSE
          DEMRG  = -99
        ENDIF
        OLAP(23) = 'EDAP  '
        CALL GetDesc(1,OLAP(23), DESCRIP(23))

!       Emergence - simulated
        YREMRG = STGDOY(9)
        IF (YRPLT .GT. 0) THEN
          DNR9 = TIMDIF (YRPLT,YREMRG)
          IF (DNR9 .LE. 0)  THEN
            DNR9 = -99
          ENDIF
        ELSE
          DNR9 = -99
        ENDIF

!       -----------------------------------------------------------
      WRITE(Simulated(1),'(I8)')DNR2;
                          WRITE(Measured(1),'(I8)')DPIN !IDAT
      WRITE(Simulated(2),'(I8)')DNR1;
                          WRITE(Measured(2),'(I8)')DFLR !ADAT
      WRITE(Simulated(3),'(I8)')-99 ;
                          WRITE(Measured(3),'(I8)')-99  !PD1T
      WRITE(Simulated(4),'(I8)')-99 ;
                          WRITE(Measured(4),'(I8)')-99  !PDFT
      WRITE(Simulated(5),'(I8)')DNR7;
                          WRITE(Measured(5),'(I8)')DMAT !MDAT
      WRITE(Simulated(6),'(I8)')NINT(YIELD)
                          WRITE(Measured(6),'(A8)')TRIM(X(6)) !HWAM
      WRITE(Simulated(7),'(I8)')-99 ;
                          WRITE(Measured(7),'(I8)')-99  !PWAM
      WRITE(Simulated(8),'(I8)')NINT(GPSM)
                          WRITE(Measured(8),'(A8)')TRIM(X(8)) !H#AM
      WRITE(Simulated(9),'(F8.4)') SKERWT
                          WRITE(Measured(9),'(A8)')TRIM(X(9)) !HWUM
      WRITE(Simulated(10),'(F8.1)') GPP
                          WRITE(Measured(10),'(A8)')TRIM(X(10))!H#UM
      WRITE(Simulated(11),'(I8)') NINT(PBIOMS)
                          WRITE(Measured(11),'(A8)')TRIM(X(11))!CWAM

!     08/11/2005 CHP changed from BWAH to BWAM, 
      WRITE(Simulated(12),'(I8)') NINT(BWAM)  
                          WRITE(Measured(12),'(A8)')TRIM(X(12))!BWAM

      WRITE(Simulated(13),'(F8.2)') MAXLAI
                          WRITE(Measured(13),'(A8)')TRIM(X(13))!LAIX
      WRITE(Simulated(14),'(F8.3)')HI
                          WRITE(Measured(14),'(A8)')TRIM(X(14))!HIAM
      WRITE(Simulated(15),'(I8)')-99;
                          WRITE(Measured(15),'(I8)')-99 !THAM
      WRITE(Simulated(16),'(I8)') NINT(GNUP)
                          WRITE(Measured(16),'(A8)')TRIM(X(16))!GNAM
      WRITE(Simulated(17),'(I8)') NINT(WTNCAN*10.)
                          WRITE(Measured(17),'(A8)')TRIM(X(17))!CNAM
      WRITE(Simulated(18),'(I8)') NINT(APTNUP)
                          WRITE(Measured(18),'(A8)')TRIM(X(18))!SNAM
      WRITE(Simulated(19),'(F8.1)')XGNP
                          WRITE(Measured(19),'(A8)')TRIM(X(19))!GN%M
      WRITE(Simulated(20),'(I8)') NINT(CANWAA*10)
                          WRITE(Measured(20),'(A8)')TRIM(X(20))!CWAA
      WRITE(Simulated(21),'(I8)') NINT(CANNAA*10)
                          WRITE(Measured(21),'(A8)')TRIM(X(21))!CNAA
      WRITE(Simulated(22),'(F8.2)')XN
                          WRITE(Measured(22),'(A8)')TRIM(X(22))!L#SM
      WRITE(Simulated(23),'(I8)') DNR9; 
                          WRITE(Measured(23),'(I8)') DEMRG

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
      LABEL(14) = 'GNAM'; VALUE(14) = WTNSD*10.
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
      END SUBROUTINE SG_OPHARV
!=======================================================================



