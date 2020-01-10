! 08/15/2011  Move the NH4I and NO3I reading and handling from SoilNi_init_2D to this subroutine
!                because the initia condition in SoilCNPinit_C is called by Century and
!                SoilNi_init_2D is after the call of Century
!             Add the handling of alphVG, mVG and nVG as well as TEXTURE, CACO3, DMOD, EXTP
!                SASC, TOTBAS, TOTP, ORGP, EXCA, EXNA, EXK
!             Set the error for the case of full plastic cover
!             Rename RowFrac to ColFrac
!==============================================================================
! Cell initialization subroutines for 2-D cell data
! To be called when (RUN = 1 OR INDEX('QF',RNMODE)<0)
! There are five options:

! CASE 1: Flat surface, no plastic mulch cover
!   CASE 1a: Bed Width=0, Furrow width = ROWSPC_CM
!   CASE 1b: Bed Width>0, Furrow width >0,  Bed Width+Furrow width=ROWSPC_CM
!   CASE 1c: Bed Width=ROWSPC_CM, Furrow width = 0
! CASE 2: Flat surface, partial plastic mulch cover
! CASE 3: Flat surface, full plastic mulch cover (not handled yet)
! CASE 4: Raised bed, plastic mulch cover over bed
! CASE 5: Raised bed, no plastic mulch 
! Case Table is as bellow
!------------------------------------------------------------------------------------------
!                 |                      Bed Width (cm)                                   |
!                 |-----------------------------------------------------------------------|
!                 |    BW < 2    |    2 < BW < ROWSPC - 2     |    BW > ROWSPC - 2        |
!-----------------|--------------|----------------------------|---------------------------|
!                 |              | PMALB > 0   |  PMALB <= 0  | PMALB > 0   | PMALB <= 0  |
!-----------------|--------------|----------------------------|-------------|-------------|
!  Bed  | BH < 2  |    CASE 1a   |     CASE 2  |    CASE 1b   |    CASE 3   |    CASE 1c  |
! Height|---------|--------------|----------------------------|-------------|-------------|
!  (cm) | BH > 2  |    CASE 1a   |     CASE 4  |    CASE 5    |    CASE 3   |    CASE 1c  |
!                 !set RaisedBed !    set RaisedBed = .TRUE.  !  set RaisedBed=  .FALSE.  !        
!                 != .FALSE.     !                            !                           !
!------------------------------------------------------------------------------------------

!==============================================================================
!   Subroutine CellInit_2D
!   Initialization for cell structure and initial conditions
    Subroutine CellInit_2D(SOILPROP, CELLS, &        !input
                NH4I, NO3I,  &                        !input & output
                SoilProp_Bed, SoilProp_Furrow)        !output
!   ---------------------------------------------------------
!  Revision history
!  09/02/2008 CHP Written
!  07/27/2010 CHP Added option for flat surface with partial or full plastic mulch cove
!                   (cases 2 & 3).
!  08/04/2010 CHP Changed MixMass subroutines.
!  ---------------------------------------------------------
!   Key to array indices:
!   L = original soil layers as input from soil profile data, 1 to NLAYR
!   M = modified soil layers from bottom of furrow to bottom of soil profile, 1 to SoilProp_Furrow%NLAYR
!   Row = modified soil layers from top of bed to bottom of soil profile, 1 to NRowsTot
	USE Cells_2D
    USE ModuleData
    Implicit NONE

    Type (CellType) Cells(MaxRows,MaxCols)
    Type (SoilType) SOILPROP, SoilProp_Bed, SoilProp_Furrow
!    Type (DripIrrType) DripIrrig

    CHARACTER*6 SECTION
    CHARACTER*8, PARAMETER :: ERRKEY = 'CellInit'
    CHARACTER*12 bedTexture
    CHARACTER*14 FMT
    ! CHARACTER*17 bedSOILLAYERTYPE 
    CHARACTER*125 MSG(50)
    CHARACTER*180 CHAR
    INTEGER L, M, NLayr, Row, Col
    INTEGER ERR, FOUND, LNUM, LUNIO
    INTEGER N_Bed_Cols, N_Fur_Cols, N_Bed_Rows  !, NRowsTOT, NColsTOT
    INTEGER FurRow1, FurCol1, BedCase
    REAL BEDHT, BEDWD, ROWSPC_CM, DigDep, DEPTH, Incr, HalfRow !, Bed_KG2PPM
    REAL Bed_Col_Width, Bed_Row_Thick, Fur_Col_Width, PREV_DEPTH 
    REAL PMALB, BEDFRACTION, MSALB
    LOGICAL WITHIN_BED

    REAL, DIMENSION(NL) :: DS, DLAYR, NH4I, NO3I
    REAL, DIMENSION(NL) :: FurrowNH4I, FurrowNO3I, NewPropNH4I, NewPropNO3I
    REAL Furrow_DEP
    REAL, DIMENSION(NL) :: DS_shift, DLAYR_shift

    REAL Depth_old_layer, Prev_dep
 
    INTEGER, DIMENSION(MaxRows,MaxCols) :: Cell_type
    REAL, DIMENSION(MaxRows,MaxCols) :: Thick, Width, CellArea, mm_2_vf
    REAL, DIMENSION(MaxRows,MaxCols) :: ColFrac, BedFrac
    REAL Bed_BD, Bed_CEC, Bed_Clay, Bed_DUL, Bed_LL, Bed_OC, Bed_PH, Bed_NH4, Bed_NO3
    REAL Bed_Sand, Bed_SAT, Bed_Silt, Bed_SWCN, Bed_ADCOEF, Bed_TOTN, Bed_WR, Bed_TotOrgN
    REAL Bed_WCR, Bed_alphaVG, Bed_mVG, Bed_nVG, Bed_CACO3, Bed_DMOD
    Real Bed_SASC, Bed_TOTBAS, Bed_EXCA, Bed_EXNA, Bed_EXK, Bed_EXTP, Bed_TOTP, Bed_ORGP
    REAL LayerThick

    Double Precision :: Bed_hb, Bed_lambda, Furrow_HB, Furrow_lambda

    LOGICAL COARSE, RaisedBed, PMCover
    
    TYPE (ControlType) CONTROL
    CALL GET(CONTROL)

!   ---------------------------------------------------------
    DS    = SOILPROP % DS
    DLAYR = SOILPROP % DLAYR
    NLAYR = SOILPROP % NLAYR
!   ---------------------------------------------------------
!   Get bed dimensions and row spacing
    CALL GETLUN('FILEIO', LUNIO)
    OPEN (LUNIO, FILE = CONTROL%FILEIO,STATUS = 'OLD',IOSTAT=ERR)
    IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,CONTROL%FILEIO,0)
    LNUM = 0

!-----------------------------------------------------------------------
    PMALB = -99.

!   Read plastic mulch albedo from FIELDS section
    SECTION = '*FIELD'
    CALL FIND(LUNIO, SECTION, LNUM, FOUND)
    IF (FOUND /= 0)  THEN
      READ(LUNIO,'(79X,2(1x,F5.1), F6.2)',IOSTAT=ERR)BEDWD, BEDHT, PMALB
     ! READ(LUNIO,'(/,A)', IOSTAT=ERR) CHAR
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,CONTROL%FILEIO,LNUM)
      IF ((ERR == 0) .AND. (PMALB .eq. 0.)) THEN
        !READ(CHAR,'(92X,F6.2)',IOSTAT=ERR) PMALB
        !(ERR /= 0) 
        PMALB = -99.
      ENDIF
    ENDIF

!   Read Planting Details Section
    SECTION = '*PLANT'
    CALL FIND(LUNIO, SECTION, LNUM, FOUND) 
    IF (FOUND == 0) CALL ERROR(SECTION, 42, CONTROL%FILEIO, LNUM)
    READ(LUNIO,'(42X,F6.0,42X,2F6.0)',IOSTAT=ERR) ROWSPC_CM !, BEDWD, BEDHT
    LNUM = LNUM + 1
    IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,CONTROL%FILEIO,LNUM)
    
    CLOSE(LUNIO)
 
!---------------------------------------------------------------------------
!   This case is beyond the case table above
    IF (BEDWD > ROWSPC_CM) Then
      MSG(1) = "Bed width > Row Spacing"
      MSG(2) = "Check planting data in experiment file."
      write(msg(3),'(A)') "Program still continue."
      call warning(3,errkey,msg)
     ! IF (BEDWD >ROWSPC_CM) 
      CALL ERROR(ERRKEY,ERR,CONTROL%FILEIO,LNUM)
      ! If we do not exit program, we need to have handling: 
      !  FurrowColumn=0, totBedRows#=0, totBedCol#=0; then line 746FMT="I2,F8.2,0I5", crash
    ENDIF
!--------------------------------------------------------------------------
!   Column widths in bed and furrow for the cases in case table above
!---------------------------------------------------------------------------  
    IF (BEDWD < 2.) THEN ! BH<2 or BH >2
    !zero bed width, Case 1a (1st case column) in case table above
      IF (PMALB > 1.E-2 .AND. BEDWD < 0) THEN
        BedCase = 3  !case 3A, Flat surface, full plastic mulch cover
        MSG(1)= "Flat surface with no plastic mulch cover."
        BEDWD = ROWSPC_CM
        BEDHT = 0.
        RaisedBed = .FALSE.
        PMCover   = .TRUE.
        MSG(1)= "Missing BEDWD, set BEDWD = ROWSPC_CM。"
        MSG(2)= "Flat surface with full plastic cover by default."
        call warning(2,errkey,msg)
      ELSEIF (BEDWD .GE. 0) THEN
        BedCase = 3  !Flat surface, full plastic mulch cover
        PMCover   = .TRUE.
        BEDWD = ROWSPC_CM
        BEDHT = 0.
        RaisedBed = .FALSE.  
        PMCover   = .TRUE.
        MSG(1)= "Flat surface with plastic cover."
        MSG(2) = "BEDWD is too small to simulate in experiment file."
        write(msg(3),'(A)') "Program will stop."
        call warning(3,errkey,msg)
        CALL ERROR(ERRKEY,0,"",0)
      ELSE
        BedCase = 1 !case 1C, Flat surface, no plastic mulch cover
        BEDWD = ROWSPC_CM
        BEDHT = 0.
        MSG(1)= "Flat surface no plastic mulch cover."
        MSG(2) = "ROWSPC <= BEDWD in experiment file."
        call warning(2,errkey,msg)
        RaisedBed = .FALSE.
        PMCover   = .FALSE.
      ENDIF

    ELSEIF (ROWSPC_CM - BEDWD < 2.) THEN ! BH<2 or BH>2
    !  Bed width=ROWSPC, Case 3 or 1C, last case colmn in case table above
    ! 03/29/2019 Meng remove the limitation and start to use case 3
      IF (PMALB > 1.E-2) THEN
        BedCase = 3  !Flat surface, full plastic mulch cover
        PMCover   = .TRUE.
        BEDWD = ROWSPC_CM
        BEDHT = 0.
        RaisedBed = .FALSE.  
        PMCover   = .TRUE.
        MSG(1)= "Flat surface with full plastic cover."
        MSG(2) = "ROWSPC = BEDWD in experiment file."
        call warning(2,errkey,msg)
!        write(msg(3),'(A)') "Program will stop."
!        call warning(3,errkey,msg)
!        CALL ERROR(ERRKEY,0,"",0)
      ELSE
        BedCase = 1 !case 1C, Flat surface, no plastic mulch cover
        BEDWD = ROWSPC_CM
        MSG(1)= "Flat surface no plastic mulch cover."
        MSG(2) = "ROWSPC <= BEDWD in experiment file."
        call warning(2,errkey,msg)
        RaisedBed = .FALSE.
        PMCover   = .FALSE.
      ENDIF
    ! the following cases are finate bed width
    ELSEIF (BEDHT < 2.) THEN ! 2 < BW < ROWSPC -2
    ! Case 2 or 1b middle case colmn, 1t case row in case table above
      IF (PMALB > 1.E-2) THEN
        BedCase = 2  !Flat surface, partial plastic mulch cover
        MSG(1)= "Flat surface, partial plastic cover."
        BEDWD = BEDWD
        BEDHT = 0.
        RaisedBed = .FALSE.
        PMCover   = .TRUE.
      Else
        BedCase = 1 !Case 1b, Flat surface, no plastic mulch cover
        MSG(1)= "Flat surface with no plastic mulch cover."
        ! BEDWD = 0. 
        BEDWD = BEDWD !jin changed in Mar, 2014
        BEDHT = 0.
        RaisedBed = .FALSE.
        PMCover   = .FALSE.
      Endif

    ELSE ! 2 < BW < ROWSPC -2
    ! case 4 or 5
      IF (PMALB > 1.E-2) THEN
        BedCase = 4 ! Raised bed, plastic mulch cover over bed
        MSG(1)= "Raised bed with plastic mulch cover."
        BEDWD = BEDWD
        BEDHT = BEDHT
        RaisedBed = .TRUE.
        PMCover   = .TRUE.
      ELSE
        BedCase = 5 ! Raised bed, no plastic mulch 
        MSG(1)= "Raised bed without plastic mulch cover."
        BEDWD = BEDWD
        BEDHT = BEDHT
        RaisedBed = .TRUE.
        PMCover   = .FALSE.
      Endif
    ENDIF
    
    CALL WARNING(1,ERRKEY,MSG)
    Call PUT('PLANT', 'BEDHT',  BEDHT)
    Call PUT('PLANT', 'BEDWD',  BEDWD)

    IF (PMCover .AND. PMALB < 1.E-2) THEN ! JZW These should be removed. We set PMCover=true only if PMALB>1.E-2
      PMALB = 0.05
      WRITE(MSG(1),'(A,F5.2,A)')'Default value of ',PMALB,' used for albedo of plastic mulch.'
      MSG(2) ='Actual value can be input in FIELDS section of experiment file.'
      CALL WARNING(2,ERRKEY,MSG)
    ENDIF

! ---------------------------------------------------------------------------
!   Define column dimensions
!   Half of the bed is simulated area because of the symmetry vertically
!    IF (( RaisedBed ) .or. ((BedCase .EQ. 1) .AND. (BEDWD > 2.) )) Then
    ! 03/29/2019 Meng remove the extra condition to always set correct 
    ! bed/farrow based on input data
    IF (BEDWD > 2.) Then
   ! IF (PMCover) THEN
!     Plastic mulch, Need to establish "bed" and "furrow" columns, even if no raised bed
       N_Bed_Cols = nint(BEDWD/10.)  ! half of the bed is simulated, ~5cm columns
!       N_Bed_Cols = nint(BEDWD/7.5)  ! JZW Potato project
      N_Bed_Cols = MIN(N_Bed_Cols, MaxCols - 1) !Reserve at least one column for furrow
      Bed_Col_Width = BEDWD / N_Bed_Cols / 2.
    ELSE
!     No plastic mulch - CASE 1 !JZW BedCase 1,2,3 Flat bed
      N_Bed_Cols = 0
      Bed_Col_Width = 0.
    ENDIF

    N_Fur_Cols = nint((ROWSPC_CM - BEDWD) / 2. / 10.)  !~10cm columns
    N_Fur_Cols = MIN(N_Fur_Cols, MaxCols - N_Bed_Cols)
    IF (N_Fur_Cols > 0) THEN
      Fur_Col_Width = (ROWSPC_CM - BEDWD) / 2. / N_Fur_Cols
    ELSE
      Fur_Col_Width = 0.0
    ENDIF
    
!   Number of columns from planting bed centerline to furrow centerline
    NColsTOT = N_Bed_Cols + N_Fur_Cols

! ---------------------------------------------------------------------------
!   Define row dimensions
    IF (RaisedBed) THEN
!     Calculate bed row thickness
      !N_Bed_Rows = nint(BEDHT/4.0) !JZW For Potato project
      N_Bed_Rows = nint(BEDHT/5.0)
      Bed_Row_Thick = BEDHT / N_Bed_Rows    !cm
!     Depth of native soil excavated and blended to create beds
      DigDep = BEDWD / ROWSPC_CM * BEDHT
    ELSE
      N_Bed_Rows = 0
      Bed_Row_Thick = 0.0    !cm
      DigDep = 0.0 
    ENDIF

!   JZW: we may set FurCol1 = N_Bed_Cols + 1 and FurRow1 = N_Bed_Rows + 1 to replace the selected case 
    SELECT CASE(BedCase)
    CASE (1) !Flat surface, no plastic
      FurRow1 = 1
      If (N_Bed_Cols .GE. 0) then
        FurCol1 = N_Bed_Cols + 1
      else
        FurCol1 = 1
      endif
    CASE (2,3) !Flat surface, partial or full plastic
      FurRow1 = 1
      FurCol1 = N_Bed_Cols + 1
    CASE (4) !Raised bed, partial plastic
      FurRow1 = N_Bed_Rows + 1
      FurCol1 = N_Bed_Cols + 1
    CASE (5) ! Raised bed, no plastic mulch  
      FurRow1 = N_Bed_Rows + 1
      FurCol1 = N_Bed_Cols + 1
    END SELECT

!   Store these values for use by conversion routines
    BedDimension % BEDHT  = BEDHT
    BedDimension % BEDWD  = BEDWD
    BedDimension % DigDep = DigDep
    BedDimension % ROWSPC_CM = ROWSPC_CM
    BedDimension % PMCover = PMCover
    BedDimension % RaisedBed = RaisedBed
    BedDimension % FurRow1 = FurRow1
    BedDimension % FurCol1 = FurCol1
    
    HalfRow = ROWSPC_cm / 2.

! ---------------------------------------------------------------------
!   Properties of soils within bed
! ---------------------------------------------------------------------
    FurrowNH4I = 0
    FurrowNO3I = 0
    NewPropNH4I = 0
    NewPropNO3I = 0
    IF (RaisedBed) THEN
!     These variables are in units of mass per unit soil volume or 
!        volume per unit soil volume - weighed average by soil depth.
      CALL MixMassVol (SOILPROP%BD,    SOILPROP, DigDep, Bed_BD)      !g/cm3
      CALL MixMassVol (SOILPROP%DUL,   SOILPROP, DigDep, Bed_DUL)     !mm3/mm3
      CALL MixMassVol (SOILPROP%LL,    SOILPROP, DigDep, Bed_LL)      !mm3/mm3
      CALL MixMassVol (SOILPROP%PH,    SOILPROP, DigDep, Bed_PH)      !
      CALL MixMassVol (SOILPROP%SAT,   SOILPROP, DigDep, Bed_SAT)     !mm3/mm3
      CALL MixMassVol (SOILPROP%SWCN,  SOILPROP, DigDep, Bed_SWCN)    !cm/hr
      CALL MixMassVol (SOILPROP%ADCOEF,SOILPROP, DigDep, Bed_ADCOEF)  !cm3[H2O]/g[soil]

!     These variables are in units of mass per unit soil mass
!        Weighted average by soil mass.
      Call MixMassMass(SOILPROP%CEC,   SOILPROP, DigDep, Bed_CEC)     !cmol/kg
      Call MixMassMass(SOILPROP%CLAY,  SOILPROP, DigDep, Bed_Clay)    !%
      Call MixMassMass(SOILPROP%OC,    SOILPROP, DigDep, Bed_OC)      !%
      Call MixMassMass(SOILPROP%SILT,  SOILPROP, DigDep, Bed_Silt)    !%
      Call MixMassMass(SOILPROP%TOTN,  SOILPROP, DigDep, Bed_TOTN)    !%
      Call MixMassMass(NH4I, SOILPROP, DigDep, Bed_NH4)
      Call MixMassMass(NO3I, SOILPROP, DigDep, Bed_NO3)
      Call MixMassMass(SOILPROP%SASC,   SOILPROP, DigDep, Bed_SASC)     !%
      Call MixMassMass(SOILPROP%TOTBAS,   SOILPROP, DigDep, Bed_TOTBAS) !cmol/kg
      Call MixMassMass(SOILPROP%EXCA,   SOILPROP, DigDep, Bed_EXCA)     !cmol/kg
      Call MixMassMass(SOILPROP%EXNA,   SOILPROP, DigDep, Bed_EXNA)     !cmol/kg
      Call MixMassMass(SOILPROP%EXK,   SOILPROP, DigDep, Bed_EXK)       !cmol/kg
      Call MixMassMass(SOILPROP%EXTP,   SOILPROP, DigDep, Bed_EXTP)     !mg/kg
      Call MixMassMass(SOILPROP%TOTP,   SOILPROP, DigDep, Bed_TOTP)     !mg/kg
      Call MixMassMass(SOILPROP%ORGP,   SOILPROP, DigDep, Bed_ORGP)     !mg/kg
      Call MixMassMass(SOILPROP%CACO3,   SOILPROP, DigDep, Bed_CACO3)   !g/kg
     ! Call MixMassMass(SOILPROP%DMOD,   SOILPROP, DigDep, Bed_DMOD)   !DMOD is not array. 0-1 scale factor use MixMassTot????
!     These variables are in units of kg/ha.  Sum of total within bed.
      CALL MixMassTot (SOILPROP%TotOrgN,SOILPROP,DigDep, Bed_TotOrgN) !kg[N]/ha
      
      !bedSOILLAYERTYPE = SOILPROP%SOILLAYERTYPE(1)
      Bed_Sand = 100. - Bed_Clay - Bed_Silt
      Bed_WR = 1.0

!-----------------------------------------------------------------------
!     Compute vanGenuchten parameters which describe water retention curve
      CALL TEXTURECLASS (Bed_Clay, Bed_Sand, Bed_silt,    &  !Input
         BedTEXTURE, COARSE)                               !Output

	  call calBrokCryPara(BedTEXTURE, Bed_SAT, Bed_LL,     & !Input
          Bed_DUL,                                       & !Input
          Bed_WCR, Bed_HB, Bed_lambda)                     !output

!     van Genuchten parameters fit from known soil properties based on RETC code
      CALL RETC_VG(BedTEXTURE, Bed_SWCN, Bed_LL,           & !Input
          Bed_DUL, Bed_SAT, Bed_WCR, Bed_Hb, Bed_lambda, & !Input
          Bed_alphaVG, Bed_mVG, Bed_nVG)                   !Output

! ---------------------------------------------------------------------
!     Properties of soil in furrow:
!       (starts below excavated soil)
! ---------------------------------------------------------------------
!     Determine layer thicknesses in furrow
!     M = temporary index for soils below furrow, indexed to bottom of bed elev.
!     DS_shift is referenced to bottom of bed elev.
!     L = old soil layer index, prior to construction of bed
      M = 0
      Prev_dep = DigDep
      DO L = 1, NLAYR
!       Depth below furrow
        IF (DS(L) - DigDep > 0.0) THEN
          M = M + 1
          DS_shift(M) = DS(L) - DigDep
          DLAYR_shift(M) = DS(L) - Prev_dep
          Prev_dep = DS(L)
    
          SoilProp_Furrow%BD(M)    = Soilprop%BD(L)
          SoilProp_Furrow%CEC(M)   = Soilprop%CEC(L)
          SoilProp_Furrow%CLAY(M)  = Soilprop%CLAY(L)
          SoilProp_Furrow%DUL(M)   = Soilprop%DUL(L)
          SoilProp_Furrow%LL(M)    = Soilprop%LL(L)    
          SoilProp_Furrow%OC(M)    = Soilprop%OC(L)    
          SoilProp_Furrow%PH(M)    = Soilprop%PH(L)    
          SoilProp_Furrow%SAND(M)  = Soilprop%SAND(L)  
          SoilProp_Furrow%SAT(M)   = Soilprop%SAT(L)   
          SoilProp_Furrow%SILT(M)  = Soilprop%SILT(L)  
          SoilProp_Furrow%SWCN(M)  = Soilprop%SWCN(L)  
          SoilProp_Furrow%ADCOEF(M)= Soilprop%ADCOEF(L)
          SoilProp_Furrow%TOTN(M)  = Soilprop%TOTN(L)  
          SoilProp_Furrow%WR(M)    = Soilprop%WR(L)   
          SoilProp_Furrow%TEXTURE(M)=Soilprop%TEXTURE(L)
          SoilProp_Furrow%COARSE(M)=Soilprop%COARSE(L)
          SoilProp_Furrow%SOILLAYERTYPE(M)=Soilprop%SOILLAYERTYPE(L)
          SoilProp_Furrow%WCR(M)   = Soilprop%WCR(L)
          SoilProp_Furrow%alphaVG(M)=Soilprop%alphaVG(L)
          SoilProp_Furrow%mVG(M)   = Soilprop%mVG(L)
          SoilProp_Furrow%nVG(M)   = Soilprop%nVG(L)
          FurrowNH4I(M)            = NH4I(L)
          FurrowNO3I(M)            = NO3I(L)
          SoilProp_Furrow%SASC(M)  = SOILPROP%SASC(L)
          SoilProp_Furrow%TOTBAS(M)= SOILPROP%TOTBAS(L)
          SoilProp_Furrow%EXCA(M)  = SOILPROP%EXCA(L)
          SoilProp_Furrow%EXNA(M)  = SOILPROP%EXNA(L)
          SoilProp_Furrow%EXK(M)   = SOILPROP%EXK(L)
          SoilProp_Furrow%EXTP(M)  = SOILPROP%EXTP(L)
          SoilProp_Furrow%TOTP(M)  = SOILPROP%TOTP(L)
          SoilProp_Furrow%ORGP(M)  = SOILPROP%ORGP(L)
          SoilProp_Furrow%CACO3(M) = SOILPROP%CACO3(L)
!         kg/ha - possibly need to split for first furrow row
          IF (M == 1) THEN
            SoilProp_Furrow%TotOrgN(M)=Soilprop%TotOrgN(L) * DLAYR_shift(M) / DLAYR(L)
          ELSE
            SoilProp_Furrow%TotOrgN(M)=Soilprop%TotOrgN(L)
          ENDIF
        ENDIF
      ENDDO

!     LYRSET2 resets the layer thicknesses below the furrow 
      CALL LYRSET4 (M, DS_shift,                &         !Input
                  SoilProp_Furrow%DS, SoilProp_Furrow%NLAYR, SoilProp_Furrow%DLAYR, Furrow_DEP)  !Output

!     Adjust all soil properties for the revised layer division
      CALL LMATCH (M, DS_shift, SoilProp_Furrow%BD    ,SoilProp_Furrow%NLAYR, SoilProp_Furrow%DS)
      CALL LMATCH (M, DS_shift, SoilProp_Furrow%CEC   ,SoilProp_Furrow%NLAYR, SoilProp_Furrow%DS)
      CALL LMATCH (M, DS_shift, SoilProp_Furrow%CLAY  ,SoilProp_Furrow%NLAYR, SoilProp_Furrow%DS)
      CALL LMATCH (M, DS_shift, SoilProp_Furrow%DUL   ,SoilProp_Furrow%NLAYR, SoilProp_Furrow%DS)
      CALL LMATCH (M, DS_shift, SoilProp_Furrow%LL    ,SoilProp_Furrow%NLAYR, SoilProp_Furrow%DS)
      CALL LMATCH (M, DS_shift, SoilProp_Furrow%OC    ,SoilProp_Furrow%NLAYR, SoilProp_Furrow%DS)
      CALL LMATCH (M, DS_shift, SoilProp_Furrow%PH    ,SoilProp_Furrow%NLAYR, SoilProp_Furrow%DS)
      CALL LMATCH (M, DS_shift, SoilProp_Furrow%SAT   ,SoilProp_Furrow%NLAYR, SoilProp_Furrow%DS)
      CALL LMATCH (M, DS_shift, SoilProp_Furrow%SILT  ,SoilProp_Furrow%NLAYR, SoilProp_Furrow%DS)
      CALL LMATCH (M, DS_shift, SoilProp_Furrow%SWCN  ,SoilProp_Furrow%NLAYR, SoilProp_Furrow%DS)
      CALL LMATCH (M, DS_shift, SoilProp_Furrow%ADCOEF,SoilProp_Furrow%NLAYR, SoilProp_Furrow%DS)
      CALL LMATCH (M, DS_shift, SoilProp_Furrow%TOTN  ,SoilProp_Furrow%NLAYR, SoilProp_Furrow%DS)
      CALL LMATCH (M, DS_shift, SoilProp_Furrow%WR    ,SoilProp_Furrow%NLAYR, SoilProp_Furrow%DS)
      CALL LMATCH (M, DS_shift, SoilProp_Furrow%WCR    ,SoilProp_Furrow%NLAYR, SoilProp_Furrow%DS)
      !We do not want to use LMATCH for alphaVG, mVG, and nVG?. 
      CALL LMATCH (M, DS_shift, FurrowNH4I             ,SoilProp_Furrow%NLAYR, SoilProp_Furrow%DS)
      CALL LMATCH (M, DS_shift, FurrowNO3I             ,SoilProp_Furrow%NLAYR, SoilProp_Furrow%DS)
      CALL LMATCH (M, DS_shift, SoilProp_Furrow%SASC   ,SoilProp_Furrow%NLAYR, SoilProp_Furrow%DS)
      CALL LMATCH (M, DS_shift, SoilProp_Furrow%TOTBAS ,SoilProp_Furrow%NLAYR, SoilProp_Furrow%DS)
      CALL LMATCH (M, DS_shift, SoilProp_Furrow%EXCA   ,SoilProp_Furrow%NLAYR, SoilProp_Furrow%DS)
      CALL LMATCH (M, DS_shift, SoilProp_Furrow%EXNA   ,SoilProp_Furrow%NLAYR, SoilProp_Furrow%DS)
      CALL LMATCH (M, DS_shift, SoilProp_Furrow%EXK    ,SoilProp_Furrow%NLAYR, SoilProp_Furrow%DS)
      CALL LMATCH (M, DS_shift, SoilProp_Furrow%EXTP   ,SoilProp_Furrow%NLAYR, SoilProp_Furrow%DS)
      CALL LMATCH (M, DS_shift, SoilProp_Furrow%TOTP   ,SoilProp_Furrow%NLAYR, SoilProp_Furrow%DS)
      CALL LMATCH (M, DS_shift, SoilProp_Furrow%ORGP   ,SoilProp_Furrow%NLAYR, SoilProp_Furrow%DS)
      CALL LMATCH (M, DS_shift, SoilProp_Furrow%CACO3  ,SoilProp_Furrow%NLAYR, SoilProp_Furrow%DS)

      CALL LMATCH_MASS(M, DS_shift, SoilProp_Furrow%TotOrgN,SoilProp_Furrow%NLAYR,SoilProp_Furrow%DS)
!-----------------------------------------------------------------------
      DO M = 1, SoilProp_Furrow%NLAYR
        IF (M == 1) THEN
          LayerThick = SoilProp_Furrow%DS(M)
        ELSE
          LayerThick = SoilProp_Furrow%DS(M) - SoilProp_Furrow%DS(M-1)
        ENDIF
!       Conversion from kg/ha to ppm (or mg/l).  
        SoilProp_Furrow%KG2PPM(M) = 10.0 / (SoilProp_Furrow%BD(M) * LayerThick)
        SoilProp_Furrow%SAND(M) = 100. - SoilProp_Furrow%CLAY(M) - SoilProp_Furrow%SILT(M)
        CALL TEXTURECLASS (SoilProp_Furrow%CLAY(M), SoilProp_Furrow%SAND(M),   &    !Input
          SoilProp_Furrow%SILT(M),                                             &    !Input
          SoilProp_Furrow%TEXTURE(M), SoilProp_Furrow%COARSE(M))                    !Output
        call calBrokCryPara(SoilProp_Furrow%TEXTURE(M), SoilProp_Furrow%SAT(M),      & !Input
          SoilProp_Furrow%LL(M), SoilProp_Furrow%DUL(M),                                       & !Input
          SoilProp_Furrow%WCR(M), Furrow_HB, Furrow_lambda)                     !output

!     van Genuchten parameters fit from known soil properties based on RETC code
        CALL RETC_VG(SoilProp_Furrow%TEXTURE(M), SoilProp_Furrow%SWCN(M),          & !Input
           SoilProp_Furrow%LL(M), SoilProp_Furrow%DUL(M), SoilProp_Furrow%SAT(M),   & !Input
           SoilProp_Furrow%WCR(M), Furrow_Hb, Furrow_lambda,                  & !Input                                   & ! input
           SoilProp_Furrow%alphaVG(M),                                        & ! Output
           SoilProp_Furrow%mVG(M), SoilProp_Furrow%nVG(M))                          !Output

      ENDDO

!-----------------------------------------------------------------------
!     Non-bedded system
!-----------------------------------------------------------------------
    ELSE
      SOILPROP_BED = SOILPROP
      SOILPROP_FURROW = SOILPROP
      FurrowNH4I = NH4I
      FurrowNO3I = NO3I
    ENDIF
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!     Bedded and non-bedded systems:
!-----------------------------------------------------------------------
    Cell_type = -99
    IF (RaisedBed) THEN
      WITHIN_BED = .TRUE.
    ELSE
      WITHIN_BED = .FALSE.
    ENDIF

    PREV_DEPTH = 0.  !DEPTH is measured from top of bed    !cm 
    L = 1            !Index of original Soil layer before digging
    M = 0            !Index for furrow layers, referenced to bottom of bed
!   Row is indexed to top of bed and is used for both
!     cell row depths and SoilProp_Bed layer depths

    DO Row = 1, MaxRows     !Cell row index
      IF (WITHIN_BED) THEN
!       Increment depths within bed
        Incr = Bed_Row_Thick
!       Depth of new layering system
        DEPTH = MIN(PREV_DEPTH + Incr, BEDHT)

!       Depth of old layers relative to new top of bed elevation
        Depth_old_layer = DS(L) + BEDHT - DigDep
        DO WHILE (Depth_old_layer < DEPTH)
          L = L + 1
          Depth_old_layer = DS(L) + BEDHT - DigDep
        ENDDO

        SoilProp_Bed%BD(Row)     = Bed_BD
        SoilProp_Bed%CEC(Row)    = Bed_CEC
        SoilProp_Bed%Clay(Row)   = Bed_Clay
        SoilProp_Bed%DUL(Row)    = Bed_DUL
        SoilProp_Bed%LL(Row)     = Bed_LL
        SoilProp_Bed%OC(Row)     = Bed_OC
        SoilProp_Bed%PH(Row)     = Bed_PH
        SoilProp_Bed%Sand(Row)   = Bed_Sand
        SoilProp_Bed%SAT(Row)    = Bed_SAT
        SoilProp_Bed%Silt(Row)   = Bed_Silt
        SoilProp_Bed%SWCN(Row)   = Bed_SWCN
        SoilProp_Bed%ADCOEF(Row) = Bed_ADCOEF
        SoilProp_Bed%TOTN(Row)   = Bed_TOTN
        SoilProp_Bed%TotOrgN(Row)= Bed_TotOrgN * Incr / BEDHT
        SoilProp_Bed%WR(Row)     = Bed_WR
        SoilProp_Bed%WCR(Row)    = Bed_WCR
        SoilProp_Bed%alphaVG(Row)= Bed_alphaVG
        SoilProp_Bed%mVG(Row)    = Bed_mVG   
        SoilProp_Bed%nVG(Row)    = Bed_nVG
        SoilProp_Bed%TEXTURE(Row)= BedTEXTURE
        SoilProp_Bed%COARSE(Row) = COARSE
        SoilProp_Bed%KG2PPM(Row) = 10.0 / (Bed_BD * Bed_Row_Thick) 
        NewPropNH4I(Row)         = Bed_NH4
        NewPropNO3I(Row)         = Bed_NO3
        SoilProp_Bed%SASC(Row)   = Bed_SASC
        SoilProp_Bed%TOTBAS(Row) = Bed_TOTBAS   
        SoilProp_Bed%EXCA(Row)   = Bed_EXCA
        SoilProp_Bed%EXNA(Row)   = Bed_EXNA
        SoilProp_Bed%EXK(Row)    = Bed_EXK   
        SoilProp_Bed%EXTP(Row)   = Bed_EXTP 
        SoilProp_Bed%TOTP(Row)   = Bed_TOTP
        SoilProp_Bed%ORGP(Row)   = Bed_ORGP   
        SoilProp_Bed%CACO3(Row)  = Bed_CACO3 

        IF (ABS(DEPTH - BEDHT) < 0.1) THEN
          WITHIN_BED = .FALSE.
        ENDIF

      ELSE
!       Use furrow layer depths below bed
        M = M + 1
        IF (M > SoilProp_Furrow%NLAYR) EXIT
        DEPTH = SoilProp_Furrow%DS(M) + BEDHT

        SoilProp_Bed%BD(Row)     = SoilProp_Furrow%BD(M)
        SoilProp_Bed%CEC(Row)    = SoilProp_Furrow%CEC(M)
        SoilProp_Bed%Clay(Row)   = SoilProp_Furrow%Clay(M)
        SoilProp_Bed%DUL(Row)    = SoilProp_Furrow%DUL(M)
        SoilProp_Bed%LL(Row)     = SoilProp_Furrow%LL(M)
        SoilProp_Bed%OC(Row)     = SoilProp_Furrow%OC(M)
        SoilProp_Bed%PH(Row)     = SoilProp_Furrow%PH(M)
        SoilProp_Bed%Sand(Row)   = SoilProp_Furrow%Sand(M)
        SoilProp_Bed%SAT(Row)    = SoilProp_Furrow%SAT(M)
        SoilProp_Bed%Silt(Row)   = SoilProp_Furrow%Silt(M)
        SoilProp_Bed%SWCN(Row)   = SoilProp_Furrow%SWCN(M)
        SoilProp_Bed%ADCOEF(Row) = SoilProp_Furrow%ADCOEF(M)
        SoilProp_Bed%TOTN(Row)   = SoilProp_Furrow%TOTN(M)
        SoilProp_Bed%WR(Row)     = SoilProp_Furrow%WR(M)
        SoilProp_Bed%WCR(Row)    = SoilProp_Furrow%WCR(M)
        SoilProp_Bed%alphaVG(Row)= SoilProp_Furrow%alphaVG(M)
        SoilProp_Bed%mVG(Row)    = SoilProp_Furrow%mVG(M)   
        SoilProp_Bed%nVG(Row)    = SoilProp_Furrow%nVG(M)
        SoilProp_Bed%KG2PPM(Row) = SoilProp_Furrow%KG2PPM(M) 
        SoilProp_Bed%TEXTURE(Row)= SoilProp_Furrow%TEXTURE(M) 
        SoilProp_Bed%COARSE(Row)= SoilProp_Furrow%COARSE(M) 
        SoilProp_Bed%TotOrgN(Row)= SoilProp_Furrow%TotOrgN(M)
        NewPropNH4I(Row)         = FurrowNH4I(M)
        NewPropNO3I(Row)         = FurrowNO3I(M)
        SoilProp_Bed%SASC(Row)   = SoilProp_Furrow%SASC(M)
        SoilProp_Bed%TOTBAS(Row) = SoilProp_Furrow%TOTBAS(M)   
        SoilProp_Bed%EXCA(Row)   = SoilProp_Furrow%EXCA(M)
        SoilProp_Bed%EXNA(Row)   = SoilProp_Furrow%EXNA(M)
        SoilProp_Bed%EXK(Row)    = SoilProp_Furrow%EXK(M)   
        SoilProp_Bed%EXTP(Row)   = SoilProp_Furrow%EXTP(M) 
        SoilProp_Bed%TOTP(Row)   = SoilProp_Furrow%TOTP(M)
        SoilProp_Bed%ORGP(Row)   = SoilProp_Furrow%ORGP(M)   
        SoilProp_Bed%CACO3(Row)  = SoilProp_Furrow%CACO3(M) 
      ENDIF

      SoilProp_Bed%DLAYR(Row) = DEPTH - PREV_DEPTH
      SoilProp_Bed%DS(Row)    = DEPTH

!     Assign cell types and properties
      DO Col = 1, NColsTot
        Thick(Row,Col) = DEPTH - PREV_DEPTH
        IF (Col <= N_Bed_Cols) THEN
          Width(Row,Col) = Bed_Col_Width
!          ColFrac(Col) = Width(Row,Col) / HalfRow
!          BedFrac(Col) = Width(Row,Col) / BEDWD / 2.0
          IF (DEPTH < BEDHT + 0.01) THEN
            ColFrac(Row, Col) = Width(Row,Col) / HalfRow
            BedFrac(Row, Col) = HalfRow / (BEDWD / 2.0)
            Cell_type(Row,Col) = 3    !within bed
          ELSE
            ColFrac(Row, Col) = Width(Row,Col) / HalfRow
            BedFrac(Row, Col) = 1
            Cell_type(Row,Col) = 4    !below bed
          ENDIF
        ELSE
          Width(Row,Col) = Fur_Col_Width
!          ColFrac(Col) = Width(Row,Col) / HalfRow
!          BedFrac(Col) = 0.0
          IF (DEPTH < BEDHT + 0.01) THEN
            ColFrac(Row, Col) = 0.0
            BedFrac(Row, Col) = 0.0
            Cell_type(Row,Col) = 0    !in furrow (no soil)
          ELSE
            ColFrac(Row, Col) = Width(Row,Col) / HalfRow
            BedFrac(Row, Col) = 1
            Cell_type(Row,Col) = 5    !soil below furrow
          ENDIF
        ENDIF
        CellArea(Row,Col) = Thick(Row,Col) * Width(Row,Col)
        
        SELECT CASE(Cell_type(Row,Col))
        CASE (3)
          Cells(Row,Col)%State%BD    = Bed_BD
          Cells(Row,Col)%State%DUL   = Bed_DUL
          Cells(Row,Col)%State%LL    = Bed_LL
          Cells(Row,Col)%State%SAT   = Bed_SAT
          Cells(Row,Col)%State%WR    = Bed_WR
          Cells(Row,Col)%State%SWCN  = bED_SWCN      
        CASE (4,5)
          Cells(Row,Col)%State%BD    = SoilProp_Furrow%BD(M)
          Cells(Row,Col)%State%DUL   = SoilProp_Furrow%DUL(M)
          Cells(Row,Col)%State%LL    = SoilProp_Furrow%LL(M)
          Cells(Row,Col)%State%SAT   = SoilProp_Furrow%SAT(M)
          Cells(Row,Col)%State%WR    = SoilProp_Furrow%WR(M)
          Cells(Row,Col)%State%SWCN  = SoilProp_Furrow%SWCN(M)
        CASE DEFAULT
          Cells(Row,Col)%State%BD    = -99.
          Cells(Row,Col)%State%DUL   = -99.
          Cells(Row,Col)%State%LL    = -99.
          Cells(Row,Col)%State%SAT   = -99.
          Cells(Row,Col)%State%WR    = -99.
          Cells(Row,Col)%State%SWCN  = -99.      
          Cells(Row,Col)%State%WR    = 0.0
        END SELECT
        Cells(Row,Col)%State%SWV     = 0
        Cells(Row,Col)%State%RLV     = 0
        Cells(Row,Col)%State%SNO3    = 0
        Cells(Row,Col)%State%SNH4    = 0
        Cells(Row,Col)%State%UREA    = 0
        Cells(Row,Col)%Rate%SWFlux_L = 0
        Cells(Row,Col)%Rate%SWFlux_R = 0
        Cells(Row,Col)%Rate%SWFlux_D = 0
        Cells(Row,Col)%Rate%SWFlux_U = 0
        Cells(Row,Col)%Rate%ES_Rate  = 0
        Cells(Row,Col)%Rate%EP_Rate  = 0
        Cells(Row,Col)%Rate%NO3Uptake = 0
        Cells(Row,Col)%Rate%NH4Uptake = 0
      ENDDO

      PREV_DEPTH = DEPTH
    ENDDO

    NRowsTot = Row - 1
    Cells%Struc%Width = Width
    Cells%Struc%Thick = Thick
    CELLS%Struc%CellType = Cell_type
    Cells%Struc%CellArea = CellArea
  IF (RaisedBed) Then
  ! IF (PMCover) THEN
!   Combined albedo for bed and furrow
    SoilProp_Bed % SALB   = PMALB  
    BEDFRACTION = BEDWD / ROWSPC_CM
    MSALB = PMALB * BEDFRACTION + SOILPROP % SALB * (1. - BEDFRACTION)
    SoilProp_Bed % MSALB  = MSALB
    SoilProp_Bed % CMSALB = MSALB
    SoilProp_Furrow % SALB   = SOILPROP%SALB  
    SoilProp_Furrow % MSALB  = MSALB
    SoilProp_Furrow % CMSALB = MSALB

    SoilProp_Bed%NLAYR = NRowsTot
    SoilProp_Furrow%NLAYR = NRowsTot - FurRow1 + 1

!   Need to re-think SALB and CN.  Values from the soil profile are not appropriate.
    SoilProp_Bed % SLDESC = "Constructed bed using " // SOILPROP % SLDESC(1:28)
    SoilProp_Bed % CN     = 98.      
    SoilProp_Bed % U      = SOILPROP % U     
    SoilProp_Bed % SLPF   = SOILPROP % SLPF  
    SoilProp_Bed % SWCON  = SOILPROP % SWCON
    SoilProp_Bed % DMOD   = SOILPROP % DMOD
         
    SoilProp_Furrow % SLDESC = "Constructed furrow using " // SOILPROP % SLDESC(1:25)
    SoilProp_Furrow % CN     = SOILPROP % CN    
    SoilProp_Furrow % U      = SOILPROP % U     
    SoilProp_Furrow % SLPF   = SOILPROP % SLPF  
    SoilProp_Furrow % SWCON  = SOILPROP % SWCON
    SoilProp_Furrow % DMOD   = SOILPROP % DMOD
  ENDIF

! Variables which are not being modified currently, but may need later    
!      SoilProp_Bed % SOILLAYERTYPE = SOILLAYERTYPE (Char)
!      SoilProp_Bed % TEXTURE= TEXTURE              (CHAR)
!      SOILPROP % CACO3  = CACO3  (CACO3 g/kg)   done
!      SOILPROP % DMOD   = DMOD  (SLNF 0 to 1 scale)  done    
!      SOILPROP % EXTP   = EXTP  (SLPX mg/kg) done       
!      SOILPROP % SASC   = SASC  (in IPSLAN stable organic C (%)done
!      SOILPROP % TOTBAS = TOTBAS  (SLBS cmol/kg) done
!      SOILPROP % TOTP   = TOTP  (SLPT mg/kg) done
!      SOILPROP % ORGP   = ORGP (SLPO mg/kg) done
!      SOILPROP % EXCA   = EXCA (SLCA cmol/kg) done
!      SOILPROP % EXNA   = EXNA (SLNA cmol/kg) done
!      SOILPROP % EXK    = EXK  (SLKE cmol/kg) done

! -----------------------------------------------------
!   LIMIT_2D represents the lowest layer for which 2D modeling is done.
!   Below this depth, use 1D tipping bucket approach.
!   For now, compute as the highest layer with depth > 70cm.
!    BedDimension % LIMIT_2D = NRowsTot
!    DO L = 1, NRowsTot
!!      IF (SoilProp_Bed%DS(L) < 75.) CYCLE
!      IF (SoilProp_Bed%DS(L) < 39.) CYCLE
!      BedDimension % LIMIT_2D = L
!      EXIT
!    ENDDO
!   temp chp
    BedDimension % LIMIT_2D = NRowsTot
    BedDimension % ColFrac  = ColFrac
    BedDimension % BedFrac  = BedFrac

    DO Row = 1, NRowsTot
      IF (CELLS(Row,1)%Struc%CellType > 3) THEN
        FurRow1 = Row
        DO Col = 1, NColsTot
          IF (CELLS(Row,Col)%Struc%CellType == 5) THEN
            FurCol1 = Col
            EXIT
          ENDIF
        ENDDO
        EXIT
      ENDIF 
    ENDDO
    BedDimension % FurRow1 = FurRow1
    BedDimension % FurCol1 = FurCol1

    DO Row = 1, NRowsTot
      DO Col = 1, NColsTot
!       conversion from mm[water] to volumetric fraction for each cell
        mm_2_vf(Row,Col) = 1.0 / 10. * HalfRow / CellArea(Row,Col)  
!                       1.0    /    10.    *    HalfRow     / (thick*width) 

!       cm3[water]               cm[water]     cm2[soil]      cm[row length] 
!       ---------- = mm[water] * --------- * -------------- * --------------
!        cm3[soil]               mm[water]   cm[row length]      cm3[soil]     
      ENDDO
    ENDDO
    BedDimension % mm_2_vf = mm_2_vf

! -----------------------------------------------------
! ---------------------------------------------------------------------
! write output
! ---------------------------------------------------------------------  
!---------------- Output Cell Structures ---------------------------------
    Write(msg(1), '("Bed height (cm)  =",F8.2)') BEDHT
    Write(msg(2), '("Bed width (cm)   =",F8.2)') BEDWD
    Write(msg(3), '("Row spacing (cm) =",F8.2)') ROWSPC_CM
    Write(msg(4), '("# Bed Rows       =",I3)') N_Bed_Rows
    Write(msg(5), '("Total # Rows     =",I3)') NRowsTOT
    Write(msg(6), '("# Bed columns    =",I3)') N_Bed_Cols
    Write(msg(7), '("# furrow columns =",I3)') N_Fur_Cols
    Write(msg(8), '("Bed col wid (cm) =",F8.3)') Bed_Col_Width
    Write(msg(9), '("Furrow wid (cm)  =",F8.3)') Fur_Col_Width 
    Write(msg(10),'("Dig Depth (cm)   =",F8.3)') DigDep 
    call info(10,errkey,msg)

    Write(msg(1),'(" Column ->",20I5)') (Col, Col=1, NColsTOT)
    Write(msg(2),'("Wid(cm) ->",20F5.1)') (Cells(1,Col)%Struc%width,Col = 1, NColsTOT)
    Write(msg(3),'("     Thick")') 
    Write(msg(4),'("Lyr   (cm)   Cell type:")')
    WRITE(FMT,'("(I2,F8.2,",I2,"I5)")') NColsTot 
    DO Row = 1, NRowsTot  
      Write(msg(4+Row),FMT)    &
      Row, Cells(Row,1)%Struc%Thick, (Cells(Row,Col)%Struc%CellType,Col = 1, NColsTOT) 
    Enddo 
    call info(NRowsTot+4,errkey,msg)  
!------------------------------
    NH4I = NewPropNH4I 
    NO3I = NewPropNO3I 
    Return
    End Subroutine CellInit_2D
!==============================================================================
! CellInit_2D VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! Bed_Col_Width       Column width in bed. Note, the first colimn can be adjusted to half width
! Bed_Row_Thick       Row thickness in bed, Note, the first row can be adjusted to half thickness
! BEDHT
! bedTexture          Scalar variable of bed texture
! BEDWD               Bed width
! MaxCols             Maximum number of columns in soil cell array
! DigDep              The depthe of soil to removed from the surface of furrow
! DS(NL)			  Cumulative depth in soil layer L (refer to the buttom of layer)
! Fur_Col_Width	      Column width in furrow
! Layer_Cell_Dep(J, L) The cell depth for Jth Row in Lth layer 
! N_Bed_Cols		  Actual total number of bed columns which has to be odd number
! N_Bed_Rows          Actual total number of bed rows 
! N_Fur_Cols		  Actual total number of furrow columns 
! NColsTOT            Actual total number of columns for the whole bed-furrow system
! NRowsTOT            Actual the maximum # of rows includuing bed and under bed
! NL                  The array length of soil layer related variables
! NLAYR				  Actual number of soil layers variable
! ColFrac(col)        Width of each column as a fraction of the total row width
! ROWSPC_CM			  The plant distance between row (cm)
! SOILPROP            Input data for soil water content which represent the SW of bed soil layers
! SoilProp_bed        Soil property of bed soil
! SoilProp_furrow     Soil property of furrow
! TEXTURE(NL)         Texture for each row
!----------------------------------------------------------------------------------
! Array index ranges for each arreas 
! -------------------------------------------------------------------------
! The Cell type 3 is within Bed, 
!                   the cell's column are from 1 to N_Bed_Cols
!					the cell's row are from 1 to N_Bed_Row
! The Cell type 4 is under Bed,
!                   the cell's column are from 1 to N_Bed_Cols
!					the cell's row are from N_Bed_Rows+1 to NRowsTOT
! The Cell type 5 is furrow,
!                   the cell's column are from (N_Bed_Cols+1) to NColsTOT
!					the cell's row are from (N_Bed_Rows+1) to NRowsTOT
! The Cell type 99 is non-simulated area, but within the array index. Its array index are:
!                   (Row<N_Bed_Row).and. (Col > N_Bed_Col)
!                   (Row>N_Bed_Row).and.(Row<NRowsTOT).and.(Col > NColsTOT)
!                   (Row>NRowsTOT)
!=======================================================================

!=======================================================================
!   Subroutine MixMassVol
!   Mixes variable in units of mass per unit soil volume
!     or units of volume per unit soil volume
!     for blended value in homogeneous planting bed

    Subroutine MixMassVol(Value_in, SOILPROP, DigDep, BedProp)

    Use ModuleDefs
    Implicit None

    INTEGER L
    REAL DigDep, BedProp, MixedAmount, Depth, PrevDep, Thick
    REAL, DIMENSION(NL) :: Value_in
    TYPE (SoilType) SOILPROP

    MixedAmount = 0.0
    PrevDep = 0.0
    DO L = 1, SOILPROP%NLAYR
      Depth = MIN(SOILPROP%DS(L), DigDep)
      Thick = Depth - PrevDep
      MixedAmount = MixedAmount + Value_in(L) * Thick
      IF (DigDep - Depth < 0.01) EXIT
      PrevDep = Depth
    ENDDO

    BedProp = MixedAmount / DigDep

    RETURN
    End Subroutine MixMassVol
!=======================================================================

!=======================================================================
!   Subroutine MixMassMass
!   Mixes variable in units of mass per unit soil mass for blended value in 
!     homogeneous planting bed

    Subroutine MixMassMass(Value_in, SOILPROP, DigDep, BedProp)

    Use ModuleDefs
    Implicit None

    INTEGER L
    REAL DigDep, BedProp, MixedAmount, Depth, PrevDep, Thick, TotMassSoil, MassSoil
    REAL, DIMENSION(NL) :: Value_in
    TYPE (SoilType) SOILPROP

    MixedAmount = 0.0
    TotMassSoil = 0.0
    PrevDep = 0.0
    DO L = 1, SOILPROP%NLAYR
      Depth = MIN(SOILPROP%DS(L), DigDep)
      Thick = Depth - PrevDep
      MassSoil = SOILPROP%BD(L) * Thick * 10.  !kg/ha
      TotMassSoil = TotMassSoil + MassSoil
      MixedAmount = MixedAmount + Value_in(L) * MassSoil
      IF (DigDep - Depth < 0.01) EXIT
      PrevDep = Depth
    ENDDO

    BedProp = MixedAmount / TotMassSoil

    RETURN
    End Subroutine MixMassMass

!=======================================================================

!=======================================================================
!   Subroutine MixMassTot
!   Mixes variable in units of mass per unit soil area for blended value in 
!     homogeneous planting bed

    Subroutine MixMassTot(Value_in, SOILPROP, DigDep, BedProp)

    Use ModuleDefs
    Implicit None

    INTEGER L
    REAL DigDep, BedProp, MixedAmount, Depth, PrevDep, Thick
    REAL, DIMENSION(NL) :: Value_in
    TYPE (SoilType) SOILPROP

    MixedAmount = 0.0
    PrevDep = 0.0
    DO L = 1, SOILPROP%NLAYR
      Depth = MIN(SOILPROP%DS(L), DigDep)
      Thick = Depth - PrevDep
      MixedAmount = MixedAmount + Value_in(L) * Thick / SoilProp%Dlayr(L) 
      IF (DigDep - Depth < 0.01) EXIT
      PrevDep = Depth
    ENDDO

    BedProp = MixedAmount !Total is just sum of amounts in each layer

    RETURN
    End Subroutine MixMassTot

!=======================================================================
