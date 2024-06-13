!=======================================================================
!  SC_OPHARV, Subroutine 
!  Generates output for seasonal data.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  02/13/2007 CHP Added framework for CaneGro
!=======================================================================

      SUBROUTINE SC_OPHARV(CONTROL, ISWITCH, 
     &    CaneCrop, EMERGED, Growth, Part, Out, MAXLAI,    !Input
!     &    AGEFAC, HARVFRAC, NSTRES, 
     &    PStres1, PStres2,     !Input
!     &    PhenoStage, 
     &    STGDOY, 
c         WFAC, TOPWT, TURFAC,       !Input
     &    XLAI, YRPLT,                                    !Input
     &    HARVFRAC, TOP_N)  !Input  ! HBD (Jan 2023) after MvdL 2011

!-----------------------------------------------------------------------
      USE ModuleDefs 
c     Define CANEGRO composite variables:
      USE CNG_ModuleDefs

      IMPLICIT NONE
      EXTERNAL GETDESC, OPVIEW, READA, SUMVALS, EvaluateDat, 
     &  ERROR, TIMDIF, READA_Y4K
      SAVE

      CHARACTER*1  RNMODE,IDETO,IPLTI
      CHARACTER*6, PARAMETER :: ERRKEY = 'OPHARV'
      CHARACTER*10 STNAME(20)
      CHARACTER*12 FILEA
      CHARACTER*30 FILEIO
	CHARACTER*80 PATHEX

      INTEGER ACOUNT, DAS
      INTEGER DYNAMIC, ERRNUM   
      INTEGER LUNIO, RUN, YIELD
      INTEGER YRDOY, YREMRG, YRPLT,YRSIM
      INTEGER DNR_EMRG, TIMDIF
      INTEGER PhenoStage
      INTEGER TRT_ROT
      INTEGER STGDOY(20)

      REAL BIOMAS, TRSH, AELH, HIAM, SUCH
      REAL MAXLAI, LFNUM, STKH
      REAL WTNCAN, XLAI, GLAI, L_SH
!      REAL, DIMENSION(2) :: HARVFRAC

      REAL PStres1, PStres2

      ! HBD (Jan 2023) after MvdL 2011
      !     MvdL
       !REAL HARVFRAC(2)
      Type (ResidueType) HARVRES
!     Harvest residue variables 0 = surface
      INTEGER, PARAMETER :: SRFC = 0
      REAL HResWt(0:NL)   !Residue mass (kg[dry matter]/ha)
      REAL HResLig(0:NL)  !Residue lignin (kg[lignin]/ha)
      REAL HResE(0:NL,NELEM)  !Residue element components (kg[E]/ha)
      REAL HARVFRAC(2) ! HBD (Jan 2023) after MvdL 2011
!      REAL PLIGLF
      REAL TOP_N

!     Arrays which contain data for printing in SUMMARY.OUT file
!       (OPSUM subroutine)
      INTEGER, PARAMETER :: SUMNUM = 7
      CHARACTER*4, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

!     Arrays which contain Simulated and Measured data for printing
!       in OVERVIEW.OUT and EVALUATE.OUT files (OPVIEW subroutine)
      CHARACTER*6, DIMENSION(EvaluateNum) :: OLAB, OLAP !OLAP in dap
      CHARACTER*12 X(EvaluateNum)
      CHARACTER*8 Simulated(EvaluateNum), Measured(EvaluateNum)
      CHARACTER*50 DESCRIP(EvaluateNum)

      LOGICAL EMERGED

!     P module
!      REAL PStres1, PStres2

!     Variables added for environmental and stress factors output
!      REAL AGEFAC, NSTRES, SWFAC, TURFAC
      TYPE (PlStresType) PlantStres
c     The Partitioning object
      TYPE (PartType) Part
c     The CaneCrop 'object', an input parameter
      TYPE (CaneCropType) CaneCrop
c     The Canegro output type
      TYPE (OutType) Out 
      TYPE(GrothType)    Growth

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType) ISWITCH

!     Transfer values from constructed data types into local variables.
      DAS    = CONTROL % DAS
      DYNAMIC= CONTROL % DYNAMIC
      FILEIO = CONTROL % FILEIO
      LUNIO  = CONTROL % LUNIO
      RUN    = CONTROL % RUN
      RNMODE = CONTROL % RNMODE
      YRDOY  = CONTROL % YRDOY
      YRSIM  = CONTROL % YRSIM

      IDETO = ISWITCH % IDETO
      IPLTI = ISWITCH % IPLTI

      ACOUNT = 13  !Number of possible FILEA headers for this crop

!     Define headings for observed data file (FILEA)
      DATA OLAB / !  Definition
     & 'SUCH  ', ! 1   Yield (kg/ha;dry)
     & 'AELH  ', ! 2   Biomass (kg/ha) at Harvest Mat.
     & 'STKH  ', ! 3   Stem wt (kg/ha)
     & 'TRSH  ', ! 4   Byproduct 
     & 'LAIGH ', ! 5   LAI at harvest 
     & 'LAIX  ', ! 6   Maximum LAI (m2/m2)
     & 'CHTA  ', ! 7   Canopy Height (m)
     & 'HIAM  ', ! 8   Harvest index               
     & 'L#SM  ', ! 9   Green leaf number at harvest
     & 'EDAP  ', !10   Emergence dap
     & 'CWAM  ', !11   Aboveground biomass at maturity (kg/ha)
     & 'SDAH  ', !12   Dead stem weight at harvest (kg/ha)
c       MJ, Feb 2012 for AgMIP sequences:
     & 'CFAH  ', ! 13  Stalk fresh mass at harvest (t/ha NOT kg/ha!)
     & 27*'      '/  !27 labels of 40 not used for sugarcane

!     chp 4/7/2009 added stage names so that developmental phases
!       will be printed to Overview.OUT
      DATA STNAME /
     &  '          ', !stage  1
     &  '          ', !stage  2
     &  '          ', !stage  3
     &  'Stlk emerg', !stage  4 Stalk emergence
     &  '          ', !stage  5
     &  '          ', !stage  6
     &  '          ', !stage  7
     &  'Planting  ', !stage  8 Planting or ratoon date
     &  '          ', !stage  9
     &  'Emergence ', !stage 10 Emergence
     &  'Peak popul', !stage 11 Peak population
     &  '          ', !stage 12
     &  '          ', !stage 13
     &  'Start sim ', !stage 14 Start of simulation
     &  '          ', !stage 15
     &  'Harvest   ', !stage 16
     &  '          ', !stage 17
     &  '          ', !stage 18
     &  '          ', !stage 19
     &  '          '/ !stage 20

!***********************************************************************
!***********************************************************************
!     RUN INITIALIZATION
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
C-----------------------------------------------------------------------
!     Read FILEIO
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)
      READ (LUNIO,'(4(/),15X,A12,1X,A80)',IOSTAT=ERRNUM) FILEA,PATHEX
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,5)
      CLOSE (LUNIO)

!     Assign descriptions to Measured and Simulated data 
!         from DATA.CDE.
      CALL GETDESC(ACOUNT, OLAB, DESCRIP)
      OLAP = OLAB

      CALL OPVIEW(CONTROL, 
     &    BIOMAS, ACOUNT, DESCRIP, IDETO, LFNUM, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, XLAI, YIELD, YRPLT, PhenoStage)

      WTNCAN = 0.0 !No N at this time.

C***********************************************************************
C***********************************************************************
C     SEASONAL INITIALIZATION
C***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
      Simulated = '        '
      Measured  = '        '
      YIELD  = 0.0
      BIOMAS = 0.0
      
!     Establish # and names of stages for environmental stress summary
      PlantStres % ACTIVE = .FALSE.
      PlantStres % StageName = '                       '
      PlantStres % NSTAGES = 4
      PlantStres % ACTIVE = .FALSE.
c      PlantStres % StageName(0) = 'Planting to Harvest    '
c      PlantStres % StageName(1) = 'Emergence -Harvest     '
c      PlantStres % StageName(2) = 'Emergence -stalk emerg.'
c      PlantStres % StageName(3) = 'Stalk emerg. -Harvest  '

      PlantStres % StageName(1) = 'Planting to Emergence  '
      PlantStres % StageName(2) = 'Emergence->Stk Elongatn'
      PlantStres % StageName(3) = 'Stk Elong to Peak Popn.'
      PlantStres % StageName(4) = 'Peak Popn. to Harvest  '
      PlantStres % StageName(0) = 'Emergence to Harvest   '


      CALL OPVIEW(CONTROL, 
     &    BIOMAS, ACOUNT, DESCRIP, IDETO, LFNUM, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, XLAI, YIELD, YRPLT, PhenoStage)

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
C-----------------------------------------------------------------------
      BIOMAS = Part%AERLDM*1000.
c      MJ, Mar 2008: replaced the line below with
c      PlantStres % W_grow = 1.0  !TURFAC
c     with: (SWDF2)
      PlantStres % W_grow = PStres2
c      and (SWDF1):
c      PlantStres % W_phot = 1.0  !SWFAC 
      PlantStres % W_phot = PStres1 

c     There is no N or P stress (yet!)
      PlantStres % N_grow = 1.0  !NSTRES
      PlantStres % N_phot = 1.0  !AGEFAC
      PlantStres % P_grow = 1.0  !PSTRES2
      PlantStres % P_phot = 1.0  !PSTRES1
      PlantStres % ACTIVE = .FALSE.

      IF (YRDOY >= YRPLT 
     &    .AND. .NOT.(EMERGED)) THEN
        PlantStres % ACTIVE(1) = .TRUE.
      ELSE
        PlantStres % ACTIVE(1) = .FALSE.
      ENDIF

      IF (YRDOY >= YRPLT .AND. EMERGED) THEN
        PlantStres % ACTIVE(0) = .TRUE.
      ENDIF

c     Condition for Emergence -stalk emerg.
c     Stalk emerg event is STGDOY(4)
c     While STGDOY(4) is 0 (date not yet assigned),
c     and the plant is emerged, and this is after planting
      IF (YRDOY >= YRPLT 
     -    .AND. 
     -     EMERGED 
     -    .AND. 
     -     STGDOY(4) .EQ. 0) THEN
        PlantStres % ACTIVE(2) = .TRUE.
      ELSE
        PlantStres % ACTIVE(2) = .FALSE.  
      ENDIF

c     Condition for peak population - harvest
c     While STGDOY(4) is NOT 0 (a date has been assigned),
c     and STGDOY(11) is NOT 0 (peak popn)
c     and the plant is emerged, and this is after STGDOY(4)
      IF (YRDOY >= YRPLT 
     -    .AND. 
     -     EMERGED 
     -    .AND. 
     -     YRDOY >= STGDOY(4)
     -    .AND.    
     -    STGDOY(4) .NE. 0
     -    .AND. 
     -    STGDOY(11) .NE. 0 
     _    .AND. YRDOY .GE. STGDOY(11))
     -THEN
        PlantStres % ACTIVE(4) = .TRUE.
      ELSE
        PlantStres % ACTIVE(4) = .FALSE.  
      ENDIF

c     Condition for Stalk emerg. - harvest
c     While STGDOY(4) is NOT 0 (a date has been assigned),
c     and the plant is emerged, and this is after STGDOY(4)
c     and STGDOY(11) = peak popn is 0, indicating that
c     peak pop has NOT yet been reached.
      IF (YRDOY >= YRPLT 
     -    .AND. 
     -     EMERGED 
     -    .AND. 
     -     YRDOY >= STGDOY(4)
     -    .AND.    
     -    STGDOY(4) .NE. 0 
     -    .AND. STGDOY(11) .EQ. 0) 
     -THEN
        PlantStres % ACTIVE(3) = .TRUE.
      ELSE
        PlantStres % ACTIVE(3) = .FALSE.  
      ENDIF

!     Send data to Overview.out data on days where stages occur
      CALL OPVIEW(CONTROL, 
     &    BIOMAS, ACOUNT, DESCRIP, IDETO, LFNUM, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, XLAI, YIELD, YRPLT, PhenoStage)

!***********************************************************************
!***********************************************************************
!     Seasonal Output 
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
!     Compute values to be sent to Overview, Summary and Evaluate files.

!     Sucrose mass
      SUCH = Part%SUCMAS 

!     Actual yield harvested (default is 100 %)
!     Should use HARVFRAC(1) from FILEX, but for now, assume 100%
c      SUCH = SUCH   

!     Canopy weight -- stalk + leaf + sucrose
      AELH = Part%AERLDM

!     Byproduct harvested is leaf weight
!     Should use HARVFRAC(2) from FILEX, but for now, assume 0%
c      TRSH = Part%TOPDM*1000. * 0.0   !HARVFRAC(2)
c      TRSH = Part%TOPDM*1000. + Out%TRASDM*1000.

      TRSH = Out%TRASDM
c     Green leaf number at harvest
      L_SH = CaneCrop%TMEANLF - CaneCrop%TMEANDEDLF

c     Green Leaf area index at harvest
      GLAI = Growth%LAI

      STKH = Part%STKDM
!     chp added 7/30/07
      IF (STKH .GT. 0.0 .AND. SUCH .GE. 0.0) THEN
         HIAM = SUCH/STKH
       ELSE
         HIAM = 0.0
      ENDIF

      IF (STGDOY(10) < 9999999) THEN
        YREMRG = STGDOY(10)
      ELSE
        YREMRG = YRPLT
      ENDIF
      IF (YRPLT .GT. 0) THEN
        DNR_EMRG = TIMDIF (YRPLT,YREMRG)
        IF (DNR_EMRG .LE. 0)  THEN
          DNR_EMRG = -99
        ENDIF
      ELSE
        DNR_EMRG = -99
      ENDIF

      ! HBD (Jan 2023) after MvdL 2011
      !   Added by MvdL: 
      ! for C modelling purposes, * 1000 to get from t/ha to kg/ha

      HResWt  = 0.0     !MvdL: Consider moving this to a later stage 
      HResLig = 0.0
      HResE   = 0.0
      
      HResWt(SRFC)  = (TRSH*1000*(1. - HARVFRAC(2))) 
     &  + (Part%TOPDM*1000*(1. - HARVFRAC(2)))
      HResLig(SRFC) = HResWt(SRFC)*0.07                          
      !MvdL: This represents the min N conc.
      HResE(SRFC, N) = (TRSH*1000*0.004*(1.-HARVFRAC(2)))        
     & + (1.-HARVFRAC(2))*TOP_N                       

      HARVRES % ResWt  = HResWt
      HARVRES % ResLig = HResLig
      HARVRES % ResE   = HResE 

!-----------------------------------------------------------------------
!     Read Measured (measured) data from FILEA
!-----------------------------------------------------------------------
      IF ((INDEX('YE',IDETO) > 0 .OR. INDEX('IAEBCGDT',RNMODE) .GT. 0) 
     &  .OR. (INDEX('AY',ISWITCH%IDETS) .GT. 0 .AND. 
     &      CONTROL%CROP .NE. 'FA')) THEN
         IF (INDEX('FQ',RNMODE) > 0) THEN
           TRT_ROT = CONTROL % ROTNUM
         ELSE
           TRT_ROT = CONTROL % TRTNUM
         ENDIF
         !CALL READA (FILEA, PATHEX,OLAB, TRT_ROT, YRSIM, X)
         CALL READA_Y4K(FILEA, PATHEX,OLAB, TRT_ROT, YRSIM, X)

!     Store Simulated and Measured data for this season.
        WRITE(Simulated(1),'(F8.2)') SUCH; 
                        WRITE(Measured(1),'(A8)')TRIM(X(1))
        WRITE(Simulated(2),'(F8.2)') AELH; 
                                    WRITE(Measured(2),'(A8)')TRIM(X(2))
        WRITE(Simulated(3),'(F8.2)') STKH; 
                                    WRITE(Measured(3),'(A8)')TRIM(X(3))
        WRITE(Simulated(4),'(F8.2)') TRSH; 
                                    WRITE(Measured(4),'(A8)')TRIM(X(4))
        WRITE(Simulated(5),'(F8.2)') GLAI; 
                                    WRITE(Measured(5),'(A8)')TRIM(X(5))
        WRITE(Simulated(6),'(F8.2)')MAXLAI;
                                    WRITE(Measured(6),'(A8)')TRIM(X(6))
        WRITE(Simulated(7),'(F8.2)') CaneCrop%CANHEIGHT
                                    WRITE(Measured(7),'(A8)')TRIM(X(7))
        WRITE(Simulated(8),'(F8.2)') HIAM; 
                                    WRITE(Measured(8),'(A8)')TRIM(X(8))
        WRITE(Simulated(9),'(F8.2)') L_SH; 
                                    WRITE(Measured(9),'(A8)')TRIM(X(9))
        WRITE(Simulated(10),'(I8)')  DNR_EMRG
                                   WRITE(Measured(10),'(A8)')TRIM(X(10))
c       MJ, Feb 2012: add fresh stalk mass at harvest to output file
        WRITE(Simulated(13),'(F8.1)')  Part%STKWM
                                   WRITE(Measured(10),'(A8)')TRIM(X(11))
      ENDIF  


!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
!     Send data to OPSUM for SUMMARY.OUT file.
!-----------------------------------------------------------------------
!     Store Summary.out labels and values in arrays to send to
!     OPSUM routines for printing.  Integers are temporarily 
!     saved as real numbers for placement in real array.
c      LABEL(1) = 'AELH'; VALUE(1) = AELH
c      LABEL(2) = 'SUCH'; VALUE(2) = SUCH
c      LABEL(3) = 'SUCH'; VALUE(3) = SUCH
c      LABEL(4) = 'TRSH'; VALUE(4) = TRSH
c      LABEL(5) = 'LAIX'; VALUE(5) = MAXLAI
c      LABEL(6) = 'HIAM'; VALUE(6) = HIAM

      LABEL(1) = 'CWAM'; VALUE(1) = AELH * 1000.
      LABEL(2) = 'HWAH'; VALUE(2) = SUCH * 1000.
      LABEL(3) = 'SUCH'; VALUE(3) = SUCH
      LABEL(4) = 'TRSH'; VALUE(4) = TRSH
      LABEL(5) = 'LAIX'; VALUE(5) = MAXLAI
      LABEL(6) = 'HIAM'; VALUE(6) = HIAM
      LABEL(7) = 'EDAT'; VALUE(7) = FLOAT(YREMRG)

      !Send labels and values to OPSUM
c     MJ, Mar 08: I believe this line is used to pass information
c     via the label/value arrays to the screen output when the model
c     is running.  ** (CHP) and to Summary.OUT
      CALL SUMVALS (SUMNUM, LABEL, VALUE) 

!-----------------------------------------------------------------------
!     Call Overview.out routine
      BIOMAS = Part%AERLDM*1000.
      YIELD  = NINT(SUCH*1000.)
      
      CALL OPVIEW(CONTROL, 
     &    BIOMAS, ACOUNT, DESCRIP, IDETO, LFNUM, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, XLAI, YIELD, YRPLT, PhenoStage)

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
      END SUBROUTINE SC_OPHARV
C=======================================================================

