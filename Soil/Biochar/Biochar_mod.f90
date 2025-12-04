MODULE Biochar_mod
  USE ModuleDefs
  USE ModuleData
  IMPLICIT NONE
  SAVE

  ! Biochar application type
  TYPE BiocharAppType
     INTEGER :: AppDate
     REAL :: Amount    ! kg/ha
     REAL :: Depth     ! cm
     REAL :: Ash       ! %
     REAL :: C_pct     ! %
     REAL :: pH        ! pH
     REAL :: CEC       ! cmol/kg
     CHARACTER(LEN=6) :: Type ! WOOD, MANURE
  END TYPE BiocharAppType

  TYPE BiocharStateType
     REAL, DIMENSION(NL) :: Amount    ! kg/ha in each layer
     REAL, DIMENSION(NL) :: C_pool    ! kg C/ha
     REAL, DIMENSION(NL) :: Age       ! Days
     REAL, DIMENSION(NL) :: CEC_added ! cmol/kg soil
  END TYPE BiocharStateType

  TYPE(BiocharAppType), DIMENSION(100) :: Applications
  INTEGER :: NumApps = 0
  TYPE(BiocharStateType) :: BCState
  
  LOGICAL :: Initialized = .FALSE.

CONTAINS

!=======================================================================
  SUBROUTINE Biochar_Init(CONTROL)
    USE ModuleDefs
    IMPLICIT NONE
    TYPE (ControlType), INTENT(IN) :: CONTROL
    
    INTEGER :: LUN, ERR, LNUM, FOUND
    CHARACTER(LEN=120) :: LINE
    CHARACTER(LEN=12) :: FILE_BG
    
    IF (Initialized) RETURN
    
    ! Initialize
    NumApps = 0
    BCState % Amount = 0.0
    BCState % C_pool = 0.0
    BCState % Age = 0.0
    BCState % CEC_added = 0.0
    
    FILE_BG = 'BIOCHAR.INP'
    CALL GETLUN('BIOCHAR', LUN)
    OPEN(UNIT=LUN, FILE=FILE_BG, STATUS='OLD', IOSTAT=ERR)
    
    IF (ERR .NE. 0) THEN
       ! No biochar file, assume no biochar
       RETURN
    ENDIF
    
    ! Read Applications
    NumApps = 0
    DO WHILE (.TRUE.)
       READ(LUN, '(A)', IOSTAT=ERR) LINE
       IF (ERR .NE. 0) EXIT
       IF (LINE(1:1) .EQ. '@') THEN
          ! Header found, read data
          DO WHILE (.TRUE.)
             READ(LUN, *, IOSTAT=ERR) &
                  Applications(NumApps+1)%AppDate, &
                  Applications(NumApps+1)%Amount, &
                  Applications(NumApps+1)%Depth, &
                  Applications(NumApps+1)%Ash, &
                  Applications(NumApps+1)%pH, &
                  Applications(NumApps+1)%CEC, &
                  Applications(NumApps+1)%C_pct, &
                  Applications(NumApps+1)%Type
             
             IF (ERR .NE. 0) EXIT
             NumApps = NumApps + 1
             IF (NumApps >= 100) EXIT
          END DO
          EXIT
       ENDIF
    END DO
    
    CLOSE(LUN)
    Initialized = .TRUE.
    
  END SUBROUTINE Biochar_Init

!=======================================================================
  SUBROUTINE Biochar_Daily(CONTROL, SOILPROP, NH4, NO3, SPi_AVAIL, SKi_AVAIL)
    USE ModuleDefs
    IMPLICIT NONE
    
    TYPE (ControlType), INTENT(IN) :: CONTROL
    TYPE (SoilType), INTENT(INOUT) :: SOILPROP
    REAL, DIMENSION(NL), INTENT(INOUT) :: NH4, NO3, SPi_AVAIL, SKi_AVAIL
    
    INTEGER :: I, L, YRDOY
    REAL :: AppAmount, AppDepth, Frac, SoilMass
    REAL :: BD_char, Porosity_char, DUL_char, LL_char
    REAL :: AgingRate, MaxCEC_Increase
    
    YRDOY = CONTROL % YRDOY
    
    ! 1. Check for applications
    DO I = 1, NumApps
       print *, "I=", I
       print *, "AppDate=", Applications(I)%AppDate
       print *, "YRDOY=", YRDOY
       IF (Applications(I)%AppDate .EQ. YRDOY) THEN
          AppAmount = Applications(I)%Amount
          AppDepth = Applications(I)%Depth
          
          ! Distribute into layers
          print *, "NLAYR=", SOILPROP%NLAYR
          DO L = 1, SOILPROP%NLAYR
             print *, "   L=", L, " DS(L)", SOILPROP%DS(L), "DLAYR(L)=", SOILPROP%DLAYR(L), "AppDepth=", AppDepth
             IF (SOILPROP%DS(L) <= AppDepth) THEN
                ! Full layer
                ! Calculate fraction of application in this layer
                ! Simplified: Uniform distribution in top AppDepth
                ! Mass in layer L = AppAmount * (DLAYR(L) / AppDepth)
                Frac = SOILPROP%DLAYR(L) / AppDepth
                BCState%Amount(L) = BCState%Amount(L) + AppAmount * Frac
                BCState%C_pool(L) = BCState%C_pool(L) + AppAmount * Frac * (Applications(I)%C_pct / 100.0)
                
                ! Immediate Nutrient Release (Ash)
                ! Assume 1% K, 0.1% P, 0.1% N in Ash? 
                ! Report says Ash contains K, P, Ca, Mg.
                ! Let's assume Ash is 50% carbonates/oxides and 10% K, 1% P.
                ! Simplified: Release K and P
                SKi_AVAIL(L) = SKi_AVAIL(L) + AppAmount * Frac * (Applications(I)%Ash/100.0) * 0.10 ! 10% of Ash is K
                SPi_AVAIL(L) = SPi_AVAIL(L) + AppAmount * Frac * (Applications(I)%Ash/100.0) * 0.01 ! 1% of Ash is P
                
                ! pH Effect (Liming)
                ! Increase pH based on Ash content and difference
                ! Simple buffer approach: pH_new = pH_old + delta
                ! Delta depends on Ash amount and buffering capacity
                ! Assume 1 ton/ha ash raises pH by 0.1 (very rough)
                ! AppAmount * Ash / 100 is Ash mass (kg/ha)
                ! 1000 kg/ha -> 0.1 pH
                SOILPROP%PH(L) = SOILPROP%PH(L) + (AppAmount * Frac * Applications(I)%Ash/100.0) * 0.0001
                IF (SOILPROP%PH(L) > Applications(I)%pH) SOILPROP%PH(L) = Applications(I)%pH ! Cap at biochar pH
                print *, "    L=", L, "PH(L)=", SOILPROP%PH(L)
                
             ELSEIF (SOILPROP%DS(L) > AppDepth .AND. SOILPROP%DS(L)-SOILPROP%DLAYR(L) < AppDepth) THEN
                ! Partial layer
                Frac = (AppDepth - (SOILPROP%DS(L)-SOILPROP%DLAYR(L))) / AppDepth
                BCState%Amount(L) = BCState%Amount(L) + AppAmount * Frac
                print *, "    L=", L, "Amount(L)=", BCState%Amount(L)
                ! ... (same logic)
             ENDIF
          END DO
       ENDIF
    END DO
    
    ! 2. Update Properties (Daily or just when changed)
    ! Biochar properties (assumed)
    BD_char = 0.4 ! g/cm3
    Porosity_char = 0.7 ! cm3/cm3
    DUL_char = 0.5 ! cm3/cm3
    LL_char = 0.2 ! cm3/cm3
    
    DO L = 1, SOILPROP%NLAYR
       IF (BCState%Amount(L) > 0.0) THEN
          ! Calculate mass fraction of biochar
          ! Soil Mass (kg/ha) = BD * DLAYR * 10000 * 1000 / 1000000 ?
          ! BD (g/cm3) = Mg/m3. 
          ! Mass (kg/ha) = BD (Mg/m3) * DLAYR (cm) * 100 (m/cm?) * 10000 (m2/ha) * 1000 (kg/Mg) / 100 (cm/m)
          ! Mass (kg/ha) = BD * DLAYR * 100,000
          SoilMass = SOILPROP%BD(L) * SOILPROP%DLAYR(L) * 100000.0
          
          Frac = BCState%Amount(L) / (SoilMass + BCState%Amount(L))
          
          ! Update BD
          ! 1/BD_mix = f/BD_char + (1-f)/BD_soil
          SOILPROP%BD(L) = 1.0 / (Frac/BD_char + (1.0-Frac)/SOILPROP%BD(L))
          
          ! Update Porosity
          SOILPROP%POROS(L) = 1.0 - SOILPROP%BD(L) / 2.65 ! Assuming particle density stays similar or adjust
          
          ! Update DUL, LL, SAT
          ! Simple weighted average for now, though interaction is complex
          SOILPROP%DUL(L) = SOILPROP%DUL(L) * (1.0-Frac) + DUL_char * Frac
          SOILPROP%LL(L) = SOILPROP%LL(L) * (1.0-Frac) + LL_char * Frac
          SOILPROP%SAT(L) = SOILPROP%SAT(L) * (1.0-Frac) + Porosity_char * Frac
          
          ! Aging and CEC
          ! Oxidation increases CEC
          BCState%Age(L) = BCState%Age(L) + 1.0
          AgingRate = 0.001 ! per day
          MaxCEC_Increase = 50.0 ! cmol/kg biochar
          
          ! Current CEC of biochar = Initial + Max * (1 - exp(-k*t))
          ! We need to know initial CEC from application. Assume average if multiple.
          ! Simplified: Add CEC to soil
          ! CEC_soil_new = CEC_soil_old + CEC_biochar * Frac
          ! CEC_biochar = 20 (base) + 50 * (1 - exp(-0.001 * Age))
          
          SOILPROP%CEC(L) = SOILPROP%CEC(L) + (20.0 + 50.0 * (1.0 - EXP(-0.001 * BCState%Age(L)))) * Frac
          
          ! Nitrate Retention (Physical entrapment)
          ! Reduce leaching by increasing WHC (already done via DUL/LL)
          ! Report says "parking nitrate". 
          ! Maybe reduce NO3 mobility?
          ! DSSAT uses `SWCON` for drainage. We could reduce `SWCON`.
          ! SOILPROP%SWCON = SOILPROP%SWCON * (1.0 - 0.5 * Frac)
          
       ENDIF
    END DO
    
  END SUBROUTINE Biochar_Daily

END MODULE Biochar_mod
