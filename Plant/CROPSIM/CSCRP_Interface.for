C=======================================================================
C  CSCERES_Interface, Subroutine
C  DSSAT interface for CSCERES Wheat growth routine.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  04/16/2002 LAH/CHP Written.
C  03-12-2003 CHP Changed senescence variable to composite (SENESCE)
C                   as defined in ModuleDefs.for
C=======================================================================
      SUBROUTINE CSCRP_Interface (CONTROL, ISWITCH,       !Input
     &    EOP, ES, NH4, NO3, SNOW, SOILPROP, SRFTEMP,     !Input
     &    ST, SW, TRWUP, WEATHER, YREND, YRPLT, HARVFRAC, !Input
     &    CANHT, HARVRES, KCAN, KEP, MDATE, NSTRES,       !Output
     &    PORMIN, RLV, RWUMX, SENESCE, STGDOY,            !Output
     &    UNH4, UNO3, XLAI)                               !Output

      USE ModuleDefs
      USE ModuleData
      USE CRP_First_Trans_m

      IMPLICIT NONE
      EXTERNAL YR_DOY, WARNING, CSCRP
      SAVE

      CHARACTER*1   IDETG, IDETL, IDETO, IDETS
      CHARACTER*1   ISWDIS, ISWWAT, ISWNIT, MESOM, RNMODE
      !CHARACTER*78  MESSAGE(10)
      CHARACTER (LEN=250) FILEIOIN      ! Name of input file

      INTEGER DYNAMIC, RUN, TN, RUNI, RN, ON
      INTEGER REP, STEP, CN, YRHAR, YREND, YRDOY
      INTEGER MDATE, NLAYR
      INTEGER MULTI, FROP, SN, YEAR, DOY
      INTEGER STGYEARDOY(20), STGDOY(20), YRPLT

      REAL CLOUDS, ES, WUPT, EOP, TRWUP, SRAD, TMAX, TMIN, CO2
      REAL SNOW 
      REAL KCAN, KEP, DEPMAX, DAYLT, DEWDUR
      REAL NSTRES, XLAI, NFP, MSALB !, ALBEDOS
      REAL DAYL, PORMIN, RAIN, RWUMX, SRFTEMP, TWILEN
      REAL CANHT, EO, WINDSP, PARIP, PARIPA
      REAL GSTAGE, LAI !, RWUPM
      REAL TAIRHR(TS), TDEW, SLPF
!     REAL LAIL, LAILA, TWILEN

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
      NH4LEFT = NH4
      NO3LEFT = NO3
      PARIP  = -99.   !Not used w/ DSSAT
      PARIPA = -99.   !Not used w/ DSSAT
      ALBEDO = MSALB

      DEPMAX = DS(NLAYR)

      FILEIOIN(1:30) = CONTROL % FILEIO

      CALL YR_DOY(YRDOY, YEAR, DOY)

!     Print warning if Century soil N routine used
      IF (DYNAMIC .EQ. RUNINIT .AND. ISWITCH % MESOM .EQ. 'P') THEN
        WRITE(MESSAGE(1),100) 
        WRITE(MESSAGE(2),110)  
        WRITE(MESSAGE(3),120) 
        CALL WARNING(3, "WHCER ", MESSAGE)
      ENDIF

  100 FORMAT('You have selected the Century soil nutrient model. ')
  110 FORMAT('The CSCER (wheat/barley) routines have not been ')
  120 FORMAT('calibrated for use with this model.' )

      IF (DYNAMIC .EQ. RUNINIT .OR. DYNAMIC .EQ. SEASINIT) THEN
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
      CALL CSCRP(FILEIOIN, RUN, TN, RN, RNMODE,             
     &  ISWWAT, ISWNIT, ISWDIS, MESOM,    
     &  IDETS, IDETO, IDETG, IDETL, FROP,                   
     &  SN, ON, RUNI, REP, YEAR, DOY, STEP, CN,             
     &  SRAD, TMAX, TMIN, TAIRHR, RAIN, CO2, TDEW,          
     &  DRAIN, RUNOFF, IRRAMT,                           
     &  TWILEN, WINDSP, DEWDUR, CLOUDS, SoilTemp, EO, ES,   
     &  NLAYR, DLAYR, DEPMAX, LL, DUL, SAT, BD, SHF, SLPF,  
     &  SNOW, SW, NO3LEFT, NH4LEFT, FERNIT,                 
     &  TLCHD, TNIMBSOM, TNOXD, 
     &  TOMINFOM, TOMINSOM, TOMINSOM1, TOMINSOM2, TOMINSOM3,
     &  YRPLT, HARVFRAC,                                    
     &  PARIP, PARIPA, EOP, EP, ET, TRWUP, ALBEDO,
     &  LAI, KCAN, KEP, 
     &  RLV, NFP, PORMIN, RWUMX, CANHT, LAIL, LAILA,        
     &  UNO3, UNH4, UH2O,                                   
     &  SENCALG, SENNALG, SENLALG,                          
     &  RESCALG, RESNALG, RESLGALG,                         
     &  STGYEARDOY, GSTAGE,                                 
!    &  WEATHER, SOILPROP, CONTROL,                         
     &  DYNAMIC) !, WEATHER

      XLAI   = CAID
      NSTRES = NFP

      STGDOY = STGYEARDOY

      IF (STGDOY(11).EQ.YRDOY) THEN
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
                      
      IF (YREND.EQ.YRDOY .AND. DYNAMIC.EQ.INTEGR) THEN 
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
      END SUBROUTINE CSCRP_Interface
