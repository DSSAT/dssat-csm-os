!=======================================================================
!  Aloha_OPHARV, Subroutine C.H. Porter
!  Generates data for use by OPSUM and OVERVIEW for Pineapple.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  06/24/2017 CHP Written, based on MZ_OPHARV
!=======================================================================

      SUBROUTINE Aloha_OPHARV(CONTROL, ISWITCH,
     &   AGEFAC, BIOMAS, CNAM, CRWNWT, EYEWT, FBIOM,      !Input
     &   FRTWT, FRUITS, GPSM, GPP, HARVFRAC, ISDATE,      !Input
     &   ISTAGE, LAI, LN, MDATE, NSTRES, PLTPOP, PMDATE,  !Input
     &   PSTRES1, PSTRES2, STGDOY, STOVER, SWFAC,         !Input
     &   TURFAC, VWATM, WTINITIAL, WTNCAN, WTNGRN,        !Input
     &   WTNUP, YIELD, YRDOY, YRPLT)                      !Input

!-----------------------------------------------------------------------
      USE Aloha_mod
      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, ERROR, GETDESC, OPVIEW, READA, 
     &  READA_Dates, SUMVALS, EvaluateDat, TIMDIF, READA_Y4K
      SAVE

      CHARACTER*1  IDETO, IDETS, IPLTI, RNMODE
      CHARACTER*2  CROP
      CHARACTER*6  SECTION
      CHARACTER*6, PARAMETER :: ERRKEY = 'OPHARV'
      CHARACTER*12 FILEA
      CHARACTER*30 FILEIO
	CHARACTER*80 PATHEX

      INTEGER DMAT 
      INTEGER DNR7, DYNAMIC, ERRNUM, FOUND
      INTEGER IMAT, IFORC, IHARV, DFR1, DFORC, DHARV, HDAP  
      INTEGER ISDATE, ISENS, LINC, LNUM, LUNIO, MDATE, ISTAGE, RUN
      INTEGER TIMDIF, TRTNUM, YRNR1, YRNR2, YRNR3
      INTEGER YRDOY, YREMRG, YRNR5, YRSIM, YRPLT
      INTEGER DNR3, PMDATE
      INTEGER TRT_ROT
      INTEGER STGDOY(20)
      
      REAL AGEFAC, BWAH
      REAL CRWNWT, EYEWT, FBIOM, FBTONS, FRTWT, FRUITS
      REAL GPP, GPSM, HI
      REAL MAXLAI, NSTRES, PBIOMS, PEYEWT, PSDWT, PLTPOP 
      REAL Pstres1, Pstres2   
      REAL SDRATE
      REAL SDWT, SDWTAH, SDWTAM, STOVER   !, StovSenes  
      REAL SWFAC, BIOMAS, Biomass_kg_ha, TURFAC
      REAL WTINITIAL, WTNCAN, WTNGRN, WTNUP, LAI, LN
      REAL YIELD, YIELDB, YieldFresh
      REAL VWATM, CNAM, BWAM, HWAH, StovSenes

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
      TYPE (SwitchType)  ISWITCH
      TYPE (ResidueType) SENESCE

!     Variables added for environmental and stress factors output
      TYPE (PlStresType) PlantStres

      DYNAMIC = CONTROL % DYNAMIC

!-----------------------------------------------------------------------
      ACOUNT = 15  !Number of FILEA headings.
!     Headings in FILEA for Measured data
      DATA OLAB /
     &    'FDAT',    !1  forcing date
     &    'MDAT',    !2  maturity date (was 4) 
     &    'HDAT',    !3  harvest date (was 2)
     &    'FWAH',    !4  yield fresh weight, t/ha (was 5) 
     &    'YDWAH',   !5  yield dry weight, t/ha (new) 
     &    'BADMF',   !6  biomass at forcing, t/ha (was 10)
     &    'BADMH',   !7  biomass at harvest, t/ha (was 11)
     &    'VWATM',   !8  veg dry matter @ mat, t/ha (was 6)
     &    'LAIX',    !9  max LAI (was 12) 
     &    'L#SM',    !10 Leaf # (was 19)
     &    'HIAM',    !11 HI (was 13)
     &    'E#AM',    !12 eye number/m2 (was 7)
     &    'E#UM',    !13 eye number/fruit (was 9)
     &    'EWUM',    !14 eye unit weight (was 8)
     &    'CNAM',    !15 tops N, kg/ha (was 16)

!    &    'PDFT',    !not used (was 3)
!    &    'THAM',    !not used (was 14)
!    &    'FRNAM',   !not used (was 15)
!    &    'FRN%M',   !not used (was 18)
!    &    'VNAM',    !not used - same as CNAM (was 17)
     &    25*'    '/ 
!                                                                                       Old    New 
! ORIGINAL ALOHA MODEL OUTPUT:                                           Simul   Meas   FileA  FileA
! 400 FORMAT (6X, 'FORCING DATE (DAP)             ',2X,I6,7X,I6,/,  !  1 DNR1,   DFLR   FDAT - OK
!    &        6X, 'HARVEST DATE (DAP)             ',2X,I6,7X,I6,/,  ! 2? DNR3,   PMAT   HDAT - OK
!    &        6X, 'PHYSIO. MATURITY DATE          ',2X,I6,7X,I6,/,  ! 4? DNR7,   DMAT   MDAT - OK
!    &        6X, 'FRESH FRUIT YIELD (T/HA)       ',F8.2, 7X,A6,/,  !  5 FYIELD, XGWT   FWAH - OK
!    &        6X, 'EYE WEIGHT (G)                 ',F8.3, 7X,A6,/,  !  8 EYEWT,  XGWU   EWUM - OK
!    &        6X, 'EYE PER SQ METER               ',F8.0, 7X,A6,/,  !  7 GPSM,   XNOGR  E#AM - OK
!    &        6X, 'EYE PER FRUIT                  ',F8.2, 7X,A6,/,  !  9 GPP,    XNOGU  E#UM - OK
!    &        6X, 'MAX. LAI                       ',F8.2, 7X,A6,/,  ! 12 MAXLAI, XLAM   LAIX - OK
!    &        6X, 'DRY BIOM @ FORC. (T/HA)        ',F8.2, 7X,A6,/,  ! 10 FBTONS, XCWT   CWAM - BADMF, aerial biomass at forcing, t/ha
!    &        6X, 'DRY BIOMASS (T/HA)             ',F8.2, 7X,A6,/,  ! 11 PBIOMS, XSWT   BWAM - BADMH, aerial biomass at harvest, t/ha
!    &        6X, 'DRY VEG. WT. (T/HA)            ',F8.2, 7X,A6,/,  !  6 PVEGWT, XPDW   PWAM - VWATM, veg wt @ maturity, t/ha 
!    &        6X, 'FRUIT N%                       ',F8.2, 7X,A6,/,  ! 18 XGNP,   XNPS   GN%M - FRN%M, fruit N concentration at maturity, %
!    &        6X, 'TOT N UPTAKE (KG N/HA)         ',F8.1, 7X,A6,/,  ! 16 TOTNUP, XNTP   CNAM - OK
!    &        6X, 'VEGETATIVE N UPTAKE            ',F8.1, 7X,A6,/,  ! 17 APTNUP, XNST   SNAM - VNAM, veg N at maturity, kg/ha
!    &        6X, 'FRUIT N UPTAKE                 ',F8.1, 7X,A6)    ! 15 GNUP,   XNGR   GNAM - FRNAM, fruit N at maturity, kg/ha

!***********************************************************************
!***********************************************************************
!     RUN INITIALIZATION
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
      CROP    = CONTROL % CROP
      YRSIM   = CONTROL % YRSIM
      RNMODE  = CONTROL % RNMODE
      FILEIO  = CONTROL % FILEIO
      RUN     = CONTROL % RUN
      IPLTI   = ISWITCH % IPLTI

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
      CROP    = CONTROL % CROP
      YRSIM   = CONTROL % YRSIM
      RNMODE  = CONTROL % RNMODE
      FILEIO  = CONTROL % FILEIO
      RUN     = CONTROL % RUN

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

!     For now the senesced leaf mass is not tracked, so this will be zero
      StovSenes = SENESCE % ResWt(0)
C-----------------------------------------------------------------------
C     Adjust dates since Pineapple grows after harvest
C-----------------------------------------------------------------------

!      MDATE = FHDATE

!-----------------------------------------------------------------------
!     Actual yield harvested (default is 100 %)
!     YIELD variable is in kg/ha
!-----------------------------------------------------------------------
      IF (PLTPOP .GE. 0.0) THEN
         IF (GPP .GT. 0.0) THEN           ! GPP = eyes per fruit
            EYEWT = FRTWT/GPP             ! Eye weight (g/eye)
            PEYEWT = EYEWT * 1000.0       ! Eye weight (mg/eye)
         ENDIF
         GPSM   = GPP*FRUITS              ! # eyes/m2
         ! Total plant weight except fruit(g/m2)
         STOVER = BIOMAS*10.0-YIELD
         ! Fresh fruit yield (kg/ha)       
         YIELDFresh  = YIELD / Species % FDMC 
         ! Fresh fruit yield (lb/acre)
         YIELDB = YIELDFresh / 0.8914         
      ENDIF

      SDWT   = YIELD  / 10.0      !g/m2
      SDWTAM = YIELD / 10.0       !g/m2
      SDWTAH = SDWT * HARVFRAC(1) !g/m2

      SDRATE = WTINITIAL

!     CHP 10/17/2017
!     Oddly, PSDWT = SDWT/GPSM is exactly equal to EYEWT = FRTWT/GPP
!       Actually, not so odd:
!       PSDWT = SDWT/GPSM = FRTWT*FRUITS/GPSM = (FRTWT*FRUITS)/(GPP*FRUITS) 
!       = FRTWT/GPP = EYEWT
      PSDWT = 0.0
      IF (GPSM .GT. 0.0 .AND. SDWT  .GE. 0.0) THEN
         ! Unit weight of eye g/unit = (g/m2) / (#/m2)
         PSDWT = SDWT/GPSM           
      ENDIF

      IF (BIOMAS .GT. 0.0 .AND. YIELD .GE. 0.0) THEN
         HI = YIELD/(BIOMAS*10.0)     !Harvest index
       ELSE
         HI = 0.0
      ENDIF

!-----------------------------------------------------------------------
!     Actual yield harvested (default is 100 %)
      HWAH = YIELD * HARVFRAC(1)

!     Actual byproduct harvested (default is 0 %)
!     Byproduct not harvested is incorporated
      BWAM = BIOMAS*10. - YIELD 
      BWAH = (BWAM + StovSenes) * HARVFRAC(2) 
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
      FBTONS = FBIOM*0.01             !Biomass at forcing, t/ha
      PBIOMS = (BIOMAS*10.0)/1000.0   !Biomass at maturity, t/ha
!     FYIELD = YIELD/1000.0
!     PVEGWT = STOVER/1000.0
      Biomass_kg_ha = BIOMAS * 10.    !Convert from g/m2 to kg/ha

!     Change observed (FileA) dates to DAP
!       Forcing date to DAP
        CALL READA_Dates(X(1), YRSIM, IFORC)
        IF (IFORC .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DFORC = TIMDIF(YRPLT,IFORC)
        ELSE
          DFORC  = -99
        ENDIF
        OLAP(1) = 'FDAP  '
        CALL GetDesc(1,OLAP(1), DESCRIP(1))

!       Maturity date to DAP
        CALL READA_Dates(X(2), YRSIM, IMAT)
        IF (IMAT .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DMAT = TIMDIF(YRPLT,IMAT)
        ELSE
          DMAT  = -99
        ENDIF
        OLAP(2) = 'MDAP  '
        CALL GetDesc(1,OLAP(2), DESCRIP(2))

!       Harvest date to DAP
        CALL READA_Dates(X(3), YRSIM, IHARV)
        IF (IHARV .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DHARV = TIMDIF(YRPLT,IHARV)
        ELSE
          DHARV  = -99
        ENDIF
        OLAP(3) = 'HDAP  '
        CALL GetDesc(1,OLAP(3), DESCRIP(3))

!       Change simulated dates to DAP
!       isdate is forcing date
        IF (YRPLT .GT. 0) THEN
          DFR1 = TIMDIF (YRPLT,ISDATE)
          IF (DFR1 .LE. 0) THEN
            DFR1 = -99
          ENDIF
        ELSE
          DFR1 = -99
        ENDIF
  
        IF (YRPLT .GT. 0) THEN
          DNR3 = TIMDIF (YRPLT,MDATE)
          IF (DNR3 .LE. 0) THEN
            DNR3 = -99
        ELSE
          DNR3 = -99
        ENDIF
      
        IF (YRPLT .GT. 0) THEN
          HDAP = TIMDIF (YRPLT,YRDOY)
          IF (HDAP .LE. 0) THEN
            HDAP = -99
          ENDIF
        ELSE
          HDAP = -99
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

      WRITE(Simulated(1),'(I8)') DFR1;  WRITE(Measured(1),'(I8)') DFORC   !FDAT 
      WRITE(Simulated(2),'(I8)') DNR7;  WRITE(Measured(2),'(I8)') DMAT    !MDAT  
      WRITE(Simulated(3),'(I8)') HDAP;  WRITE(Measured(3),'(I8)') DHARV   !HDAT   
      WRITE(Simulated(4),'(F8.2)') YIELDFresh/1000.                                     
                                  WRITE(Measured(4),'(A8)')TRIM(X(4))     !FWAH 
      WRITE(Simulated(5),'(F8.2)') YIELD/1000.                                     
                                  WRITE(Measured(5),'(A8)')TRIM(X(5))     !YDWAH 
      WRITE(Simulated(6),'(F8.1)') FBTONS                                      
                                  WRITE(Measured(6),'(A8)')TRIM(X(6))     !BADMF     
      WRITE(Simulated(7),'(F8.1)') PBIOMS                                        
                                  WRITE(Measured(7),'(A8)')TRIM(X(7))     !BADMH    
      WRITE(Simulated(8),'(F8.2)')VWATM;
                                  WRITE(Measured(8),'(A8)')TRIM(X(8))     !VWATM  
      WRITE(Simulated(9),'(F8.2)') MAXLAI                                          
                                  WRITE(Measured(9),'(A8)')TRIM(X(9))     !LAIX     
      WRITE(Simulated(10),'(F8.1)') LN; 
                                  WRITE(Measured(10),'(A8)')TRIM(X(10))   !L#SM 
      WRITE(Simulated(11),'(F8.3)') HI; 
                                  WRITE(Measured(11),'(A8)')TRIM(X(11))   !HIAM
      WRITE(Simulated(12),'(I8)') NINT(GPSM)                                      
                                  WRITE(Measured(12),'(A8)')TRIM(X(12))   !E#AM 
      WRITE(Simulated(13),'(F8.1)') GPP;
                                  WRITE(Measured(13),'(A8)')TRIM(X(13))   !E#UM     
      WRITE(Simulated(14),'(F8.3)') EYEWT                                          
                                  WRITE(Measured(14),'(A8)')TRIM(X(14))   !EWUM    
      WRITE(Simulated(15),'(F8.1)')CNAM;
                                  WRITE(Measured(15),'(A8)')TRIM(X(15))   !CNAM

! These aren't calculated - remove from Overview output
!     WRITE(Simulated(3),'(I8)') -99 ;  WRITE(Measured(3),'(I8)') -99     !PDFT - not used
!     WRITE(Simulated(14),'(I8)') -99 ; WRITE(Measured(14),'(I8)') -99    !THAM
!     WRITE(Simulated(15),'(F8.1)') FRNAM
!                                       WRITE(Measured(15),'(A8)') X(15)  !FRNAM
!     WRITE(Simulated(18),'(F8.1)')FRNpctM
!                                       WRITE(Measured(18),'(A8)') X(18)  !FRN%M
!     WRITE(Simulated(16),'(F8.1)')VNAM;WRITE(Measured(16),'(A8)')X(16)   !VNAM

      ENDIF

!-------------------------------------------------------------------
!     Send information to OPSUM to generate SUMMARY.OUT file
!-------------------------------------------------------------------
!     Store Summary.out labels and values in arrays to send to
!     OPSUM routines for printing.  Integers are temporarily 
!     saved as real numbers for placement in real array.
      LABEL(1)  = 'ADAT'; VALUE(1)  = FLOAT(ISDATE)
      LABEL(2)  = 'MDAT'; VALUE(2)  = FLOAT(MDATE)
      LABEL(3)  = 'DWAP'; VALUE(3)  = SDRATE
      LABEL(4)  = 'CWAM'; VALUE(4)  = BIOMAS*10.
      LABEL(5)  = 'HWAM'; VALUE(5)  = YIELD
      LABEL(6)  = 'HWAH'; VALUE(6)  = HWAH
!     BWAH multiplied by 10.0 in OPSUM - divide by 10. here to preserve units. (?????)
      LABEL(7)  = 'BWAH'; VALUE(7)  = BWAH / 10. 
      LABEL(8)  = 'HWUM'; VALUE(8)  = EYEWT  !unit eye weight g/unit
      LABEL(9)  = 'H#AM'; VALUE(9)  = GPSM   !# eyes/m2 at maturity
      LABEL(10) = 'H#UM'; VALUE(10) = GPP    !# eyes/fruit at maturity
      LABEL(11) = 'NFXM'; VALUE(11) = 0.0    !WTNFX*10.
      LABEL(12) = 'NUCM'; VALUE(12) = WTNUP*10.
      LABEL(13) = 'CNAM'; VALUE(13) = WTNCAN*10.
      LABEL(14) = 'GNAM'; VALUE(14) = WTNGRN*10.
      LABEL(15) = 'PWAM'; VALUE(15) = CRWNWT*10.
      LABEL(16) = 'LAIX'; VALUE(16) = MAXLAI
      LABEL(17) = 'HIAM'; VALUE(17) = HI
      LABEL(18) = 'EDAT'; VALUE(18) = FLOAT(YREMRG)

!     Send labels and values to OPSUM
      CALL SUMVALS (SUMNUM, LABEL, VALUE) 

      CALL OPVIEW(CONTROL, 
     &    Biomass_kg_ha, ACOUNT, DESCRIP, IDETO, LN, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, LAI, NINT(YIELD), YRPLT, ISTAGE)

!     Send Measured and Simulated datat to OPSUM
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



