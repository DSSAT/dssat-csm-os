!=======================================================================
!  SC_OPHARV, Subroutine 
!  Generates output for seasonal data.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  02/13/2007 CHP Added framework for CaneGro
!  03/31/2020 MV  Adapted code from CANEGRO to SAMUCA
!=======================================================================

    SUBROUTINE SC_OPHARV_SAM(CONTROL, ISWITCH, &
        CaneCrop, EMERGED, MAXLAI,     &
        PStres1, PStres2,              &
        STGDOY,                        &
        XLAI, YRPLT)

    USE ModuleDefs
    USE SAM_ModuleDefs

    IMPLICIT NONE
    EXTERNAL ERROR, EvaluateDat, GETDESC, OPVIEW, READA, SUMVALS, TIMDIF, &
             READA_Y4K
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
    REAL WTNCAN, XLAI, GLAI, L_SH, DWAP, BWAH, HWAHF

    REAL PStres1, PStres2

    !--- Arrays which contain data for printing in SUMMARY.OUT file (OPSUM subroutine)
    INTEGER, PARAMETER :: SUMNUM = 10
    CHARACTER*5, DIMENSION(SUMNUM) :: LABEL
    REAL, DIMENSION(SUMNUM) :: VALUE

    !--- Arrays which contain Simulated and Measured data for printing
    !--- in OVERVIEW.OUT and EVALUATE.OUT files (OPVIEW subroutine)
    CHARACTER*6, DIMENSION(EvaluateNum) :: OLAB, OLAP !OLAP in dap
    CHARACTER*12 X(EvaluateNum)
    CHARACTER*8 Simulated(EvaluateNum), Measured(EvaluateNum)
    CHARACTER*50 DESCRIP(EvaluateNum)

    LOGICAL EMERGED
    
    !--- Define constructed variable types based on definitions in ModuleDefs.for.
    TYPE (ControlType)    CONTROL
    TYPE (SwitchType)     ISWITCH
    TYPE (PlStresType)    PlantStres
    
    !--- SAMUCA composite variable
    type (CaneSamuca)   CaneCrop
    
    real    CFAH
    real    CHTA

    !--- Transfer values from constructed data types into local variables.
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

    !--- Define headings for observed data file (FILEA)
    DATA OLAB /& !  Definition
    'SUCH  ', & ! 1   Sugar Yield (kg/ha)
    'AELH  ', & ! 2   Biomass (kg/ha) at Harvest Mat.
    'STKH  ', & ! 3   Stem wt (kg/ha)
    'TRSH  ', & ! 4   Byproduct
    'LAIGH ', & ! 5   LAI at harvest
    'LAIX  ', & ! 6   Maximum LAI (m2/m2)
    'CHTA  ', & ! 7   Canopy Height (m)
    'HIAM  ', & ! 8   Harvest index
    'L#SM  ', & ! 9   Green leaf number at harvest
    'EDAP  ', & !10   Emergence dap
    'CWAM  ', & !11   Aboveground biomass at maturity (kg/ha)
    'SDAH  ', & !12   Dead stem weight at harvest (kg/ha)
    'CFAH  ', & !13   Stalk fresh mass at harvest (t/ha NOT kg/ha!)
    27*'      '/!27   labels of 40 not used for sugarcane

    !     chp 4/7/2009 added stage names so that developmental phases will be printed to Overview.OUT
    DATA STNAME /   &
    '          ', & !stage  1
    '          ', & !stage  2
    '          ', & !stage  3
    'Stlk emerg', & !stage  4 Stalk emergence
    '          ', & !stage  5
    '          ', & !stage  6
    '          ', & !stage  7
    'Planting  ', & !stage  8 Planting or ratoon date
    '          ', & !stage  9
    'Emergence ', & !stage 10 Emergence
    'Peak popul', & !stage 11 Peak population
    '          ', & !stage 12
    '          ', & !stage 13
    'Start sim ', & !stage 14 Start of simulation
    '          ', & !stage 15
    'Harvest   ', & !stage 16
    '          ', & !stage 17
    '          ', & !stage 18
    '          ', & !stage 19
    '          '/   !stage 20

    !***********************************************************************
    !***********************************************************************
    !     RUN INITIALIZATION
    !***********************************************************************
    IF (DYNAMIC .EQ. RUNINIT) THEN
        
        !--- Read FILEIO
        OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)
        READ (LUNIO,'(4(/),15X,A12,1X,A80)',IOSTAT=ERRNUM) FILEA,PATHEX
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,5)
        CLOSE (LUNIO)

        !--- Assign descriptions to Measured and Simulated data from DATA.CDE.
        CALL GETDESC(ACOUNT, OLAB, DESCRIP)
        OLAP = OLAB

        CALL OPVIEW(CONTROL, &
            BIOMAS, ACOUNT, DESCRIP, IDETO, LFNUM,      &
            Measured, PlantStres, Simulated, STGDOY,	&
            STNAME, WTNCAN, XLAI, YIELD, YRPLT, PhenoStage)

        WTNCAN = 0.0 !No N at this time.

    !***********************************************************************
    !***********************************************************************
    !     SEASONAL INITIALIZATION
    !***********************************************************************
    ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
        
        Simulated   = '        '
        Measured    = '        '
        YIELD       = 0.0
        BIOMAS      = 0.0

        !--- Establish # and names of stages for environmental stress summary
        PlantStres % ACTIVE         = .FALSE.
        PlantStres % StageName      = '                       '
        PlantStres % NSTAGES        = 4
        PlantStres % ACTIVE         = .FALSE.        
        PlantStres % StageName(1)   = 'Planting to Emergence  '
        PlantStres % StageName(2)   = 'Emergence->Stk Elongatn'
        PlantStres % StageName(3)   = 'Stk Elong to Peak Popn.'
        PlantStres % StageName(4)   = 'Peak Popn. to Harvest  '
        PlantStres % StageName(0)   = 'Emergence to Harvest   '
        
        CALL OPVIEW(CONTROL, &
            BIOMAS, ACOUNT, DESCRIP, IDETO, LFNUM,      &
            Measured, PlantStres, Simulated, STGDOY,	&
            STNAME, WTNCAN, XLAI, YIELD, YRPLT, PhenoStage)

    !***********************************************************************
    !***********************************************************************
    !     DAILY OUTPUT
    !***********************************************************************
    ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
        
        BIOMAS              = CaneCrop % dw_aerial
        PlantStres % W_grow = PStres2
        PlantStres % W_phot = PStres1

        !---There is no N or P stress (yet)
        PlantStres % N_grow = 1.0  
        PlantStres % N_phot = 1.0  
        PlantStres % P_grow = 1.0  
        PlantStres % P_phot = 1.0  
        PlantStres % ACTIVE = .FALSE.

        IF (YRDOY >= YRPLT .AND. .NOT.(EMERGED)) THEN
            PlantStres % ACTIVE(1) = .TRUE.
        ELSE
            PlantStres % ACTIVE(1) = .FALSE.
        ENDIF

        IF (YRDOY >= YRPLT .AND. EMERGED) THEN
            PlantStres % ACTIVE(0) = .TRUE.
        ENDIF

        !     Condition for Emergence -stalk emerg.
        !     Stalk emerg event is STGDOY(4)
        !     While STGDOY(4) is 0 (date not yet assigned),
        !     and the plant is emerged, and this is after planting
        IF ((YRDOY >= YRPLT) .AND. (EMERGED) .AND. (STGDOY(4) .EQ. 0)) THEN
            PlantStres % ACTIVE(2) = .TRUE.
        ELSE
            PlantStres % ACTIVE(2) = .FALSE.
        ENDIF

        !     Condition for peak population - harvest
        !     While STGDOY(4) is NOT 0 (a date has been assigned),
        !     and STGDOY(11) is NOT 0 (peak popn)
        !     and the plant is emerged, and this is after STGDOY(4)
        IF ((YRDOY >= YRPLT) .AND. (EMERGED) .AND. (YRDOY >= STGDOY(4)) .AND. &
            (STGDOY(4) .NE. 0) .AND. (STGDOY(11) .NE. 0) .AND. (YRDOY .GE. STGDOY(11)))THEN
        PlantStres % ACTIVE(4) = .TRUE.
        ELSE
            PlantStres % ACTIVE(4) = .FALSE.
        ENDIF

        !     Condition for Stalk emerg. - harvest
        !     While STGDOY(4) is NOT 0 (a date has been assigned),
        !     and the plant is emerged, and this is after STGDOY(4)
        !     and STGDOY(11) = peak popn is 0, indicating that
        !     peak pop has NOT yet been reached.
        IF ((YRDOY >= YRPLT) .AND. (EMERGED) .AND. (YRDOY >= STGDOY(4)) .AND. &
            (STGDOY(4) .NE. 0) .AND. (STGDOY(11) .EQ. 0))THEN
            PlantStres % ACTIVE(3) = .TRUE.
        ELSE
            PlantStres % ACTIVE(3) = .FALSE.
        ENDIF

        !     Send data to Overview.out data on days where stages occur
        CALL OPVIEW(CONTROL, &
            BIOMAS, ACOUNT, DESCRIP, IDETO, LFNUM,        &
            Measured, PlantStres, Simulated, STGDOY,      &
            STNAME, WTNCAN, XLAI, YIELD, YRPLT, PhenoStage)

    !***********************************************************************
    !***********************************************************************
    !     Seasonal Output
    !***********************************************************************
    ELSE IF (DYNAMIC .EQ. SEASEND) THEN
        
        !--- Compute values to be sent to Overview, Summary and Evaluate files

        !--- Sucrose mass (t/ha)
        SUCH = CaneCrop % suc_it_AG
                
        !--- Canopy mass (t/ha)
        AELH = CaneCrop % dw_aerial
        
        !--- Trash mass (t/ha)        
        TRSH = CaneCrop % dw_lf

        !--- Total green leaf number formed per stem at harvest
        L_SH = CaneCrop % n_lf_AG_dewlap

        !--- Green Leaf area index at harvest
        GLAI = CaneCrop % lai

        !--- Stalk dry mass (t/ha)
        STKH = CaneCrop % dw_it_AG
        
        !--- Stalk fresh mass (t/ha)
        CFAH = CaneCrop % fw_it_AG
        
        !--- Harvest Index for sugarcane as POL% [Fraction of sucrose in fresh stalk mass]
        HIAM = CaneCrop % pol / 100.d0
        
        !--- Canopy Height (m)
        CHTA = CaneCrop % stk_h
        
        !--- Planting Material Weight (kg/ha)
        DWAP = CaneCrop % dw_total * 1000.d0
        
        !--- By-product (kg/ha) [residues]
        BWAH = ((CaneCrop % dw_it_dead_AG) + (CaneCrop % dw_lf_dead)) * 1000.d0
        
        !--- Harvested yield (fresh weight) (kg/ha)
        HWAHF = CaneCrop % fw_it_AG * 1000.d0        
        
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

        !-----------------------------------------------------------------------
        !     Read Measured (measured) data from FILEA
        !-----------------------------------------------------------------------
        IF ((INDEX('YE',IDETO) > 0 .OR. INDEX('IAEBCGDT',RNMODE) .GT. 0) &
            .OR. (INDEX('AY',ISWITCH%IDETS) .GT. 0 .AND. &
            CONTROL%CROP .NE. 'FA')) THEN
        IF (INDEX('FQ',RNMODE) > 0) THEN
            TRT_ROT = CONTROL % ROTNUM
        ELSE
            TRT_ROT = CONTROL % TRTNUM
        ENDIF
        !CALL READA (FILEA, PATHEX,OLAB, TRT_ROT, YRSIM, X)
        CALL READA_Y4K(FILEA, PATHEX,OLAB, TRT_ROT, YRSIM, X)

        !--- Store Simulated and Measured data for this season.
        WRITE(Simulated(1),'(F8.2)')    SUCH;               
                                WRITE(Measured(1),'(A8)')   TRIM(X(1))
        WRITE(Simulated(2),'(F8.2)')    AELH;               
                                WRITE(Measured(2),'(A8)')   TRIM(X(2))
        WRITE(Simulated(3),'(F8.2)')    STKH;               
                                WRITE(Measured(3),'(A8)')   TRIM(X(3))
        WRITE(Simulated(4),'(F8.2)')    TRSH;               
                                WRITE(Measured(4),'(A8)')   TRIM(X(4))
        WRITE(Simulated(5),'(F8.2)')    GLAI;               
                                WRITE(Measured(5),'(A8)')   TRIM(X(5))
        WRITE(Simulated(6),'(F8.2)')    MAXLAI;				
                                WRITE(Measured(6),'(A8)')   TRIM(X(6))
        WRITE(Simulated(7),'(F8.2)')    CHTA;               
                                WRITE(Measured(7),'(A8)')   TRIM(X(7))
        WRITE(Simulated(8),'(F8.2)')    HIAM;               
                                WRITE(Measured(8),'(A8)')   TRIM(X(8))
        WRITE(Simulated(9),'(F8.2)')    L_SH;               
                                WRITE(Measured(9),'(A8)')   TRIM(X(9))
        WRITE(Simulated(10),'(I8)')     DNR_EMRG;           
                                WRITE(Measured(10),'(A8)')  TRIM(X(10))
        WRITE(Simulated(13),'(F8.1)')   CFAH;               
                                WRITE(Measured(10),'(A8)')  TRIM(X(11)) ! MV: Why Simulated jumps from 10 to 13? Why Measured 10 is replaced by X(11)?
        ENDIF
                        
        !-----------------------------------------------------------------------
        !     Send data to OPSUM for SUMMARY.OUT file.
        !-----------------------------------------------------------------------
        !     Store Summary.out labels and values in arrays to send to
        !     OPSUM routines for printing.  Integers are temporarily
        !     saved as real numbers for placement in real array.
        LABEL(1) = 'CWAM';      VALUE(1) = AELH * 1000.d0
        LABEL(2) = 'HWAH';      VALUE(2) = STKH * 1000.d0
        LABEL(3) = 'SUCH';      VALUE(3) = SUCH * 1000.d0
        LABEL(4) = 'TRSH';      VALUE(4) = TRSH * 1000.d0
        LABEL(5) = 'LAIX';      VALUE(5) = MAXLAI
        LABEL(6) = 'HIAM';      VALUE(6) = HIAM
        LABEL(7) = 'EDAT';      VALUE(7) = FLOAT(YREMRG)        
        LABEL(8)  = 'DWAP';     VALUE(8)    = DWAP  !Planting Material Weight (kg/ha)
        LABEL(9)  = 'BWAH';     VALUE(9)    = BWAH/10.d0  !By-product (kg/ha) [residues] Divided by 10 because output is addidng zero at end...
        LABEL(10) = 'HWAHF';    VALUE(10)   = HWAHF !Harvested yield (fresh weight) (kg/ha)

        !Send labels and values to OPSUM
        !     MJ, Mar 08: I believe this line is used to pass information
        !     via the label/value arrays to the screen output when the model
        !     is running.  ** (CHP) and to Summary.OUT
        CALL SUMVALS (SUMNUM, LABEL, VALUE)

        !-----------------------------------------------------------------------
        !     Call Overview.out routine
        BIOMAS = CaneCrop % dw_aerial * 1000.d0 ! Aerial dry biomass [kg ha-1]
        YIELD  = SUCH * 1000.d0                 ! Sucrose as sugarcane yield [kg ha-1] 

        CALL OPVIEW(CONTROL, &
            BIOMAS, ACOUNT, DESCRIP, IDETO, LFNUM,    &
            Measured, PlantStres, Simulated, STGDOY,  &
            STNAME, WTNCAN, XLAI, YIELD, YRPLT, PhenoStage)

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
    
    RETURN
    
    END SUBROUTINE SC_OPHARV_SAM
    !=======================================================================

