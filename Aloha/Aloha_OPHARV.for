!=======================================================================
!  Aloha_OPHARV, Subroutine C.H. Porter
!  Generates data for use by OPSUM and OVERVIEW for Pineapple.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  06/24/2017 CHP Written, based on MZ_OPHARV
!=======================================================================

      SUBROUTINE Aloha_OPHARV(CONTROL, ISWITCH,
     &   BIOMAS, CRWNWT, GPSM, GPP, HARVFRAC, ISDATE,     !Input
     &   LAI, LN, MDATE, PMDATE, STGDOY, STOVER,          !Input
     &   WTINITIAL, YIELD, YRDOY, YRPLT)                  !Input

!-----------------------------------------------------------------------
      USE Aloha_mod
      IMPLICIT NONE
      SAVE

      CHARACTER*1  IDETO, IDETS, IPLTI, RNMODE
      CHARACTER*2  CROP
      CHARACTER*6  SECTION
      CHARACTER*6, PARAMETER :: ERRKEY = 'OPHARV'
      CHARACTER*12 FILEA
      CHARACTER*30 FILEIO
	CHARACTER*80 PATHEX

      INTEGER DEMRG, DFLR, DMAT, IFPD, DFPD, IFSD, DFSD
      INTEGER DNR0, DNR1, DNR7, DYNAMIC, ERRNUM, FOUND
!      INTEGER FHDATE
      INTEGER IEMRG, IFLR, IMAT
      INTEGER ISDATE, ISENS, LINC, LNUM, LUNIO, MDATE, ISTAGE, RUN
      INTEGER TIMDIF, TRTNUM, YRNR1, YRNR2, YRNR3
      INTEGER YRDOY, YREMRG, YRNR5, YRSIM, YRPLT
      INTEGER PMAT, DNR3, PMDATE
      INTEGER TRT_ROT
      INTEGER STGDOY(20)
      
      REAL AGEFAC, APTNUP, BWAH, BWAM, CANNAA, CANWAA
      REAL CRWNWT, GNUP, GPP, GPSM, HI, StovSenes
      REAL MAXLAI, NSTRES, PODWT, PSDWT
      REAL Pstres1, Pstres2   
      REAL SDRATE
      REAL SDWT, SDWTAH, SDWTAM, SEEDNO, SKERWT, STOVER
      REAL SWFAC, BIOMAS, Biomass_kg_ha, TURFAC
      REAL WTINITIAL, WTNCAN, WTNUP, XGNP, LAI, LN
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
      CHARACTER*6 X(EvaluateNum)
      CHARACTER*8 Simulated(EvaluateNum), Measured(EvaluateNum)
      CHARACTER*50 DESCRIP(EvaluateNum)

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
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
      ACOUNT = 21  !Number of FILEA headings.
!     Headings in FILEA for Measured data
      DATA OLAB /
     &    'FDAT',    !1  
     &    'HDAT',    !2  
     &    'PDFT',    !3  
     &    'MDAT',    !4  
     &    'FWAH',    !5  
     &    'PWAM',    !6  
     &    'E#AM',    !7  
     &    'EWUM',    !8  
     &    'E#UM',    !9  
     &    'CWAM',    !10 
     &    'BWAH',    !11
     &    'LAIX',    !12
     &    'HIAM',    !13
     &    'THAM',    !14
     &    'GNAM',    !15
     &    'CNAM',    !16
     &    'SNAM',    !17
     &    'GN%M',    !18
     &    'CWAA',    !19
     &    'CNAA',    !20
     &    'L#SM',    !21
     &    19*'    '/ 

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

      IDETO = ISWITCH % IDETO

      Biomass_kg_ha = BIOMAS * 10. !Convert from g/m2 to kg/ha
      
      CALL OPVIEW(CONTROL, 
     &    Biomass_kg_ha, ACOUNT, DESCRIP, IDETO, LN, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, LAI, NINT(YIELD), YRPLT, ISTAGE)

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
      PlantStres % StageName(1) = 'Emergence - Zero Stem  '
      PlantStres % StageName(2) = 'Zero Stem - Forcing    '
      PlantStres % StageName(3) = 'Forcing - SCY          '
      PlantStres % StageName(4) = 'SCY - Early Flwr       '
      PlantStres % StageName(5) = 'Early Flwr - Fruit Harv'

      Biomass_kg_ha = BIOMAS * 10. !Convert from g/m2 to kg/ha

      CALL OPVIEW(CONTROL, 
     &    Biomass_kg_ha, ACOUNT, DESCRIP, IDETO, LN, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, LAI, NINT(YIELD), YRPLT, ISTAGE)

      MAXLAI = 0.0

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
!-----------------------------------------------------------------------
      MAXLAI = AMAX1 (MAXLAI,LAI)      ! Maximum LAI season

      PlantStres % W_grow = TURFAC 
      PlantStres % W_phot = SWFAC  
      PlantStres % N_grow = AGEFAC 
      PlantStres % N_phot = NSTRES 
      PlantStres % P_grow = PSTRES2
      PlantStres % P_phot = PSTRES1

!                                                         
!STG - DEFINITION                                         
!  7 - Preplanting                                        
!  8 - Planting to root initiation                        
!  9 - Root initiation to first new leaf emergence
!  1 - First new leaf emergence to net zero root growth
!  2 - Net zero stem growth to forcing
!  3 - Forcing to sepals closed on youngest flowers
!  4 - SCY to first open flower
!  5 - Fruit growth
!  6 - Physiological maturity

!     PlantStres % StageName     Aloha Stages active
! 0  'Planting to Harvest    '    8,9,1,2,3,4,5,6
! 1  'Emergence - Zero Stem  '    1
! 2  'Zero Stem - Forcing    '    2
! 3  'Forcing - SCY          '    3
! 4  'SCY - Early Flwr       '    4
! 5  'Early Flwr - Fruit Harv'    5,6

      PlantStres % ACTIVE = .FALSE.
      SELECT CASE(ISTAGE)
      CASE(1,2,3,4,5)
        PlantStres % ACTIVE(ISTAGE) = .TRUE.
      CASE(6)
        PlantStres % ACTIVE(5) = .TRUE.
      END SELECT

      YRDOY = CONTROL % YRDOY
      IF (YRDOY >= YRPLT) THEN
        IF (MDATE < 0 .OR.
     &     (MDATE > 0 .AND. YRDOY < MDATE)) THEN
          PlantStres % ACTIVE(0) = .TRUE.
        ELSE
          PlantStres % Active(0) = .FALSE.
        ENDIF
      ENDIF

      Biomass_kg_ha = BIOMAS * 10. !Convert from g/m2 to kg/ha

!     Send data to Overview.out data on days where stages occur
      CALL OPVIEW(CONTROL, 
     &    Biomass_kg_ha, ACOUNT, DESCRIP, IDETO, LN, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, LAI, NINT(YIELD), YRPLT, ISTAGE)

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
!      YRNR7  = MDATE
!      WTNUP  = TOTNUP/10.0
!      WTNCAN = TOTNUP/10.0
!      WTNSD  = GNUP  /10.0

C-----------------------------------------------------------------------
C     Adjust dates since Pineapple grows after harvest
C-----------------------------------------------------------------------

!      MDATE = FHDATE

!-----------------------------------------------------------------------
!     Actual yield harvested (default is 100 %)
!-----------------------------------------------------------------------

      SDWT   = YIELD  / 10.0
      SDWTAM = YIELD / 10.0
      SDWTAH = SDWT * HARVFRAC(1)
!      TOPWT  = BIOMAS

!-----------------------------------------------------------------------
!     Calculate variables for output
!     update nitrogen and residue applications after routines have been
!     modified to handle automatic management
!-----------------------------------------------------------------------
!      IF (INDEX ('PI',CROP) .EQ. 0) THEN
!      TOTNUP = WTNUP
!      ENDIF
      PSDWT = 0.0

      SDRATE = WTINITIAL
      IF (GPSM .GT. 0.0 .AND. SDWT  .GE. 0.0) THEN
         PSDWT = SDWT/GPSM
      ENDIF

      IF (BIOMAS .GT. 0.0 .AND. YIELD .GE. 0.0) THEN
         HI = YIELD/(BIOMAS*10.0)
       ELSE
         HI = 0.0
      ENDIF

!-----------------------------------------------------------------------
!     Actual byproduct harvested (default is 0 %)
!     Byproduct not harvested is incorporated
!-----------------------------------------------------------------------

      BWAH   = STOVER * HARVFRAC(2)/100.0

      IF ((INDEX('YE',IDETO) > 0 .OR. INDEX('IAEBCGDT',RNMODE) .GT. 0) 
     &  .OR. (INDEX('AY',IDETS) .GT. 0 .AND. CROP .NE. 'FA')) THEN
         IF (INDEX('FQ',RNMODE) > 0) THEN
           TRT_ROT = CONTROL % ROTNUM
         ELSE
           TRT_ROT = TRTNUM
         ENDIF
         CALL READA (FILEA, PATHEX,OLAB, TRT_ROT, YRSIM, X)

!-----------------------------------------------------------------------
!     Convert from YRDOY format to DAP.  Change descriptions to match.
C
C     Initialize observed values to -99 before reading values
C
      !IFLR   =     -99
      !IFPD   =     -99
      !IFSD   =     -99
      !IMAT   =     -99
      !XGWT   = '   -99'
      !XPDW   = '   -99'
      !XNOGR  = '   -99'
      !XGWU   = '   -99'
      !XNOGU  = '   -99'
      !XCWT   = '   -99'
      !XSWT   = '   -99'
      !XLAM   = '   -99'
      !XHIN   = '   -99'
      !XNGR   = '   -99'
      !XNST   = '   -99'
      !XNTP   = '   -99'
      !XNPS   = '   -99'
      !XTHR   = '   -99'
      !XCWAA  = '   -99'
      !XCNAA  = '   -99'
      !XLFNO  = '   -99'

      !FBTONS = FBIOM*0.01
      !PBIOMS = (BIOMAS*10.0)/1000.0
      !FYIELD = YIELD/1000.0
      !PVEGWT = STOVER/1000.0

      Biomass_kg_ha = BIOMAS * 10. !Convert from g/m2 to kg/ha


        CALL READA_Dates(X(1), YRSIM, IFLR)
        IF (IFLR .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DFLR = TIMDIF(YRPLT,IFLR)
        ELSE
          DFLR  = -99
        ENDIF
        OLAP(1) = 'ADAP  '
        CALL GetDesc(1,OLAP(1), DESCRIP(1))

        CALL READA_Dates(X(4), YRSIM, IMAT)
        IF (IMAT .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DMAT = TIMDIF(YRPLT,IMAT)
        ELSE
          DMAT  = -99
        ENDIF
        OLAP(4) = 'MDAP  '
        CALL GetDesc(1,OLAP(4), DESCRIP(4))

        IF (IFPD .GT. 0 .AND. IPLTI .EQ. 'R') THEN
          PMAT = TIMDIF (YRPLT,IFPD)
        ELSE
          PMAT = -99
        ENDIF

        IF (YRPLT .GT. 0) THEN
          DNR1 = TIMDIF (YRPLT,ISDATE)
          IF (DNR1 .LE. 0) THEN
            DNR1 = -99
          ENDIF
        ELSE
          DNR1 = -99
        ENDIF
  
        IF (YRPLT .GT. 0) THEN
          DNR3 = TIMDIF (YRPLT,MDATE)
          IF (DNR3 .LE. 0) THEN
            DNR3 = -99
        ELSE
          DNR3 = -99
        ENDIF
      
        IF (YRPLT .GT. 0) THEN
          DNR7 = TIMDIF (YRPLT,PMDATE)
          IF (DNR7 .LE. 0)  THEN
            DNR7 = -99
          ENDIF
        ELSE
          DNR7 = -99
        ENDIF
      ENDIF

      WRITE(Simulated(1),'(I8)') DNR1;  WRITE(Measured(1),'(I8)') DFLR    !ADAT
      WRITE(Simulated(2),'(I8)') -99 ;  WRITE(Measured(2),'(I8)') -99     !PD1T
      WRITE(Simulated(3),'(I8)') -99 ;  WRITE(Measured(3),'(I8)') -99     !PDFT
      WRITE(Simulated(4),'(I8)') DNR7;  WRITE(Measured(4),'(I8)') DMAT    !MDAT
      WRITE(Simulated(5),'(I8)') NINT(YIELD)
                                        WRITE(Measured(5),'(A8)') X(5)    !HWAM
      WRITE(Simulated(6),'(I8)') -99 ;  WRITE(Measured(6),'(I8)') -99     !PWAM
      WRITE(Simulated(7),'(I8)') NINT(GPSM)
                                        WRITE(Measured(7),'(A8)') X(7)    !H#AM
      WRITE(Simulated(8),'(F8.4)') SKERWT
                                        WRITE(Measured(8),'(A8)') X(8)    !HWUM 
      WRITE(Simulated(9),'(F8.1)') GPP; WRITE(Measured(9),'(A8)') X(9)    !H#UM 
      WRITE(Simulated(10),'(I8)') NINT(BIOMAS)
                                        WRITE(Measured(10),'(A8)') X(10)  !CWAM

!     08/11/2005 CHP changed from BWAH to BWAM, 
      WRITE(Simulated(11),'(I8)') NINT(BWAM)  
                                        WRITE(Measured(11),'(A8)') X(11)  !BWAM

      WRITE(Simulated(12),'(F8.2)') MAXLAI
                                        WRITE(Measured(12),'(A8)') X(12)  !LAIX
      WRITE(Simulated(13),'(F8.3)') HI; WRITE(Measured(13),'(A8)') X(13)  !HIAM
      WRITE(Simulated(14),'(I8)') -99 ; WRITE(Measured(14),'(I8)') -99    !THAM
      WRITE(Simulated(15),'(I8)') NINT(GNUP)
                                        WRITE(Measured(15),'(A8)') X(15)  !GNAM
      WRITE(Simulated(16),'(I8)') NINT(WTNCAN*10.)
                                        WRITE(Measured(16),'(A8)') X(16)  !CNAM
      WRITE(Simulated(17),'(I8)') NINT(APTNUP)
                                        WRITE(Measured(17),'(A8)') X(17)  !SNAM
      WRITE(Simulated(18),'(F8.1)')XGNP;WRITE(Measured(18),'(A8)') X(18)  !GN%M
      WRITE(Simulated(19),'(I8)') NINT(CANWAA*10)
                                        WRITE(Measured(19),'(A8)') X(19)  !CWAA
      WRITE(Simulated(20),'(I8)') NINT(CANNAA*10)
                                        WRITE(Measured(20),'(A8)') X(20)  !CNAA
      WRITE(Simulated(21),'(F8.2)') LN; WRITE(Measured(21),'(A8)') X(21)  !L#SM
      WRITE(Simulated(22),'(I8)') DNR0; WRITE(Measured(22),'(I8)') DEMRG

      ENDIF

!-------------------------------------------------------------------
!     Send information to OPSUM to generate SUMMARY.OUT file
!-------------------------------------------------------------------
!      PSDWT  = SKERWT
!      SDRATE = -99.0

!     Store Summary.out labels and values in arrays to send to
!     OPSUM routines for printing.  Integers are temporarily 
!     saved as real numbers for placement in real array.
      LABEL(1)  = 'ADAT'; VALUE(1)  = FLOAT(YRNR1)
      LABEL(2)  = 'MDAT'; VALUE(2)  = FLOAT(MDATE)
      LABEL(3)  = 'DWAP'; VALUE(3)  = SDRATE
      LABEL(4)  = 'CWAM'; VALUE(4)  = BIOMAS*10.
      LABEL(5)  = 'HWAM'; VALUE(5)  = SDWTAM*10.
      LABEL(6)  = 'HWAH'; VALUE(6)  = SDWTAH*10.
! BWAH multiplied by 10.0 in OPSUM - divide by 10. here to preserve units. (?????)
      LABEL(7)  = 'BWAH'; VALUE(7)  = BWAH  
      LABEL(8)  = 'HWUM'; VALUE(8)  = PSDWT       !*1000.
      LABEL(9)  = 'H#AM'; VALUE(9)  = GPSM
      LABEL(10) = 'H#UM'; VALUE(10) = GPP
      LABEL(11) = 'NFXM'; VALUE(11) = 0.0         !WTNFX*10.
      LABEL(12) = 'NUCM'; VALUE(12) = WTNUP*10.
      LABEL(13) = 'CNAM'; VALUE(13) = WTNCAN*10.
      LABEL(14) = 'GNAM'; VALUE(14) = 0.0 ! GNUP = WTNSD*10.
      LABEL(15) = 'PWAM'; VALUE(15) = CRWNWT*10.
      LABEL(16) = 'LAIX'; VALUE(16) = MAXLAI
      LABEL(17) = 'HIAM'; VALUE(17) = HI
      LABEL(18) = 'EDAT'; VALUE(18) = FLOAT(YREMRG)

      !Send labels and values to OPSUM
      CALL SUMVALS (SUMNUM, LABEL, VALUE) 

      CALL OPVIEW(CONTROL, 
     &    Biomass_kg_ha, ACOUNT, DESCRIP, IDETO, LN, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, LAI, NINT(YIELD), YRPLT, ISTAGE)

      !Send Measured and Simulated datat to OPSUM
      CALL EvaluateDat (ACOUNT, Measured, Simulated, DESCRIP, OLAP) 

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE Aloha_OPHARV
!=======================================================================



