C=======================================================================
C  CSCAS_Interface, Subroutine
C  DSSAT interface for CSYCA CIAT Cassava growth routine.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  04/16/2002 LAH/CHP Written.
C  08/09/2003 CHP Changed senescence variable to composite (SENESCE)
C                   as defined in ModuleDefs.for
C  08/09/2012 GH  Updated for cassava
C  02/03/2014 MF/PM SNOW variable deleted.
C  20/01/2015 MF Added module YCA_Albedo_Check_m to allow checks in WORK.OUT.
C  20/01/2015 MF Updated some variables to be consistent with CSCAS.
!  01/26/2023 CHP Reduce compile warnings: add EXTERNAL stmts, remove 
!                 unused variables, shorten lines. 
C=======================================================================
      SUBROUTINE CSYCA_Interface (CONTROL, ISWITCH,       !Input
     &    EOP, NH4, NO3,SOILPROP, SRFTEMP,                !Input
     &    ST, SW, TRWUP, WEATHER, YREND, YRPLT, HARVFRAC, !Input
     &    CANHT, HARVRES, KCAN, KEP, MDATE, NSTRES,       !Output
!         MF 20JA15 REPLACED PORMIN with RWUMP
     &    RWUPM, RLV, RWUMX, SENESCE, STGDOY,             !Output       
     &    UNH4, UNO3, XLAI)                               !Output

!     2023-01-26 chp removed unused variables in argument list:
!       ES

      USE ModuleDefs
      USE ModuleData
      USE YCA_Albedo_Check_m                                             ! MF 18JA15 For WORK.OUT
      USE YCA_First_Trans_m

      IMPLICIT NONE
      EXTERNAL CSYCA, YR_DOY
      SAVE

      CHARACTER*1   IDETG, IDETL, IDETO, IDETS
      CHARACTER*1   ISWDIS, ISWWAT, ISWNIT, MESOM, RNMODE                                            
      CHARACTER (LEN=120) FILEIOIN      ! Name of input file

      INTEGER DYNAMIC, RUN, TN, RUNI, RN, ON
      INTEGER REP, STEP, CN, YRHAR, YREND, YRDOY
      INTEGER MDATE, NLAYR
      INTEGER MULTI, FROP, SN, YEAR, DOY
      INTEGER STGYEARDOY(0:19), STGDOY(0:19), YRPLT
!     INTEGER YEARPLTCSM    ! MF 26OC14 to run CSCAS from ORIGINAL_CSCAS

! MF 26OC14 REPLACED ALBEDO WITH ALBEDOS 
! MF 26OC14 REPLACED PORMIN WITH RWUMP
! MF 26OC14 REPLACED GSTAGE WITH BRSTAGE
      REAL CLOUDS, WUPT, EOP, TRWUP, SRAD, TMAX, TMIN, CO2  !ES, 
      REAL KCAN, KEP, DEPMAX, DAYLT, DEWDUR
      REAL NSTRES, XLAI, NFP, MSALB, ALBEDOS                            
      REAL DAYL, RWUPM, RAIN, RWUMX, SRFTEMP, TWILEN                    
      REAL CANHT, EO, WINDSP, PARIP, PARIPA   
      REAL BRSTAGE, LAI         
      REAL TAIRHR(TS), TDEW, SLPF
!      REAL LAIL, LAILA, TWILEN

      REAL, DIMENSION(2)  :: HARVFRAC

      REAL, DIMENSION(NL) :: BD, DLAYR, DS, DUL, LL
      REAL, DIMENSION(NL) :: NH4, NO3, RLV, SAT, SHF, NO3LEFT, NH4LEFT
      REAL, DIMENSION(NL) :: ST, SW, UNO3, UNH4, UH2O
      REAL, DIMENSION(0:NL) :: SENCALG, SENNALG, SENLALG
      REAL, DIMENSION(0:NL) :: RESCALG, RESNALG, RESLGALG
      REAL, DIMENSION(0:NL) :: SOILTEMP

!     Not used here, but keep for Tony's interface
      REAL          LAIL(30)      ! Leaf area index by layer       m2/m2
      REAL          LAILA(30)     ! Leaf area index,active,by layr m2/m2

      REAL DRAIN, RUNOFF, IRRAMT, FERNIT
      REAL TNOXD, TLCHD, TOMINFOM, TOMINSOM
      REAL TOMINSOM1, TOMINSOM2, TOMINSOM3, TNIMBSOM
      REAL          EP            ! Transpiration daily            mm/d 
      REAL          ET            ! Evapotranspiration daily       mm/d 

      TYPE (ControlType) CONTROL
      TYPE (SoilType)    SOILPROP
      TYPE (SwitchType)  ISWITCH
      Type (ResidueType) HARVRES
      Type (ResidueType) SENESCE
      TYPE (WeatherType) WEATHER

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      CROP    = CONTROL % CROP
      FROP    = CONTROL % FROP
      MULTI   = CONTROL % MULTI
      RNMODE  = CONTROL % RNMODE
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY

      ISWWAT  = ISWITCH % ISWWAT
      ISWNIT  = ISWITCH % ISWNIT
      ISWDIS  = ISWITCH % ISWDIS
      IDETG   = ISWITCH % IDETG
      IDETL   = ISWITCH % IDETL
      IDETO   = ISWITCH % IDETO
      IDETS   = ISWITCH % IDETS
      MESOM   = ISWITCH % MESOM

      BD     = SOILPROP % BD     
      DLAYR  = SOILPROP % DLAYR  
      DS     = SOILPROP % DS    
      DUL    = SOILPROP % DUL    
      LL     = SOILPROP % LL     
      MSALB  = SOILPROP % MSALB  
      NLAYR  = SOILPROP % NLAYR  
      SAT    = SOILPROP % SAT    
      SHF    = SOILPROP % WR
      SLPF   = SOILPROP % SLPF

      YRHAR = YREND
      WUPT  = TRWUP

      DAYLT = DAYL
      DEWDUR = 12.
      NH4Left = NH4
      NO3Left = NO3
      PARIP  = -99.   !Not used w/ DSSAT
      PARIPA = -99.   !Not used w/ DSSAT
      ALBEDOS = MSALB                                                   ! MF 26OC14 REPLACED ALBEDO WITH ALBEDOS
      ALBEDOS_Interface = MSALB                                         ! MF 18JA15 For WORK.OUT

      DEPMAX = DS(NLAYR)

      FILEIOIN(1:30) = CONTROL % FILEIO

      CALL YR_DOY(YRDOY, YEAR, DOY)


      IF (DYNAMIC  ==  RUNINIT .OR. DYNAMIC  ==  SEASINIT) THEN
        TN = 0
        RN = 0
        SN = 0
        ON = 0
        CN = 0  !Crop component
        REP = 1
        STEP = 1
        RUNI = 1

        FERNIT = 0.0
        IRRAMT = 0.0
        DRAIN  = 0.0 
        RUNOFF = 0.0
        TNOXD  = 0.0 
        TLCHD  = 0.0 
        EO     = 0.0
        UH2O   = 0.0

        TOMINFOM  = 0.0 
        TOMINSOM  = 0.0 
        TOMINSOM1 = 0.0 
        TOMINSOM2 = 0.0 
        TOMINSOM3 = 0.0 
        TNIMBSOM  = 0.0 

      ELSEIF (DYNAMIC == RATE) THEN
!       Get data from ModuleData
        CALL GET('MGMT','FERNIT', FERNIT)     !N from fert today (kg/ha)
        CALL GET('MGMT','IRRAMT', IRRAMT)     !Irrigation today (mm) 
        CALL GET('WATER','DRAIN', DRAIN)      !Drainage today (mm)
        CALL GET('WATER','RUNOFF',RUNOFF)     !Runoff today (mm)
        CALL GET('NITR', 'TNOXD', TNOXD)      !Nitrif today (kg/ha)
        CALL GET('NITR', 'TLCHD', TLCHD)      !Leached N today (kg/ha)
        CALL GET('SPAM','EO',EO) 
        CALL GET('SPAM','UH2O',UH2O)          !Root water uptake (cm/d)
        CALL GET('ORGC','TOMINFOM' ,TOMINFOM) !Miner from FOM (kg/ha)
        CALL GET('ORGC','TOMINSOM' ,TOMINSOM) !Miner from SOM (kg/ha)
        CALL GET('ORGC','TOMINSOM1',TOMINSOM1)!Miner from SOM1 (kg/ha)
        CALL GET('ORGC','TOMINSOM2',TOMINSOM2)!Miner from SOM2 (kg/ha)
        CALL GET('ORGC','TOMINSOM3',TOMINSOM3)!Miner from SOM3 (kg/ha)
        CALL GET('ORGC','TNIMBSOM', TNIMBSOM) !Immob (kg/ha)
        CALL GET('SPAM','EP',  EP)
      ELSEIF (DYNAMIC == INTEGR) THEN
        CALL GET('SPAM','ET',  ET)
      ENDIF

      SOILTEMP(0) = SRFTEMP
      DO L = 1, NLAYR
        SOILTEMP(L) = ST(L)
      ENDDO

      CLOUDS = WEATHER % CLOUDS
      CO2    = WEATHER % CO2
      DAYL   = WEATHER % DAYL 
      TWILEN = WEATHER % TWILEN
      RAIN   = WEATHER % RAIN
      SRAD   = WEATHER % SRAD
      TAIRHR = WEATHER % TAIRHR
      TDEW   = WEATHER % TDEW
      TMAX   = WEATHER % TMAX
      TMIN   = WEATHER % TMIN
      WINDSP = WEATHER % WINDSP
C-----------------------------------------------------------------------
      CALL CSYCA (FILEIOIN, RUN, TN, RN, RNMODE,           !Command line
     & ISWWAT, ISWNIT, ISWDIS, MESOM,                      !Contols      
     & IDETS, IDETO, IDETG, IDETL, FROP,                   !Controls
     & SN, ON, RUNI, REP, YEAR, DOY, STEP, CN,             !Run+loop
     & SRAD, TMAX, TMIN, TAIRHR, RAIN, CO2, TDEW,          !Weather 
     & DRAIN, RUNOFF, IRRAMT,                              !Water   
     & DAYL, DEWDUR, CLOUDS, ST, EO,                       !Weather 
     & NLAYR, DLAYR, DEPMAX, LL, DUL, SAT, BD, SHF, SLPF,  !Soil states 
     & SW, NO3LEFT, NH4LEFT, FERNIT,                       !H2o,N states
     & TLCHD, TNIMBSOM, TNOXD,                             !N components
     & TOMINFOM, TOMINSOM, TOMINSOM1, TOMINSOM2, TOMINSOM3,!N components
     & YRPLT, HARVFRAC,                                    !Pl.date         
!     LPM 06MAR2016 Use YRPLT instead of YEARPLTCSM 
     & PARIP, PARIPA, EOP, EP, ET, TRWUP, ALBEDOS,         !Resources       
!     MF 26OC14 REPLACED ALBEDO WITH ALBEDOS
     & LAI, KCAN, KEP,                                    !States           
!     LPM 14AUG20 use LAI instead of CAID
     & RLV, NFP, RWUPM, RWUMX, CANHT, LAIL, LAILA,         !States          
!     MF 26OC14 REPLACED PORMIN WITH RWUPM
     & UNO3, UNH4, UH2O,                                   !Uptake       
     & SENCALG, SENNALG, SENLALG,                          !Senescence   
     & RESCALG, RESNALG, RESLGALG,                         !Residues     
     & STGYEARDOY, BRSTAGE,                                !Stage dates  
     & WEATHER     , SOILPROP    , CONTROL     , 
     & DYNAMIC) !, WEATHER)                                !Control         
!     MF 10JA15 WEATHER IS NEEDED FOR HOURLY EVALUATIONS

!       MF 26OC14 There are 92 actual variables in the call to CSCAS. The only variables that need to be passed are the dummy variables of
!          CSCAS_Interface of which there are only 31. The others can be passed in a Module (20JA15 still to do).

      XLAI   = LAI
      NSTRES = NFP

      STGDOY = STGYEARDOY

      IF (STGDOY(19) == YRDOY) THEN
        MDATE = YRDOY
        YREND = YRDOY
      ENDIF 

      if (dynamic .eq. integr) then
        DO L=0, NLAYR
          SENESCE % ResWt(L)  = (SENCALG(L) + RESCALG(L)) / 0.40
          SENESCE % ResLig(L) = SENLALG(L) + RESLGALG(L)
          SENESCE % ResE(L,1) = SENNALG(L) + RESNALG(L)
        ENDDO
      endif
                      
      IF (YREND == YRDOY .AND. DYNAMIC == INTEGR) THEN 
        !Transfer harvest residue from senescence variable to 
        !harvest residue variable on day of harvest.
        HARVRES = SENESCE
        SENESCE % ResWt  = 0.0
        SENESCE % ResLig = 0.0
        SENESCE % ResE   = 0.0
      ELSE
        MDATE = -99
      ENDIF

      RETURN
      END SUBROUTINE CSYCA_Interface
