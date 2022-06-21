!=======================================================================
!  Aloha_GROSUB, Subroutine
!
!  Maize growth routine
!-----------------------------------------------------------------------
!  Revision history
!  02/07/1993 PWW Header revision and minor changes   
!  02/07/1993 PWW Switch block added, etc
!  10/17/2017 CHP Adpated for CSM v4.7
!  09/05/2020 JVJ Stages changes for inclusion in Overview   
!-----------------------------------------------------------------------
!  INPUT  : NOUTDO,ISWNIT
!
!  LOCAL  : NSINK,NPOOL1,NPOOL2,NPOOL,NSDR,I,ICOLD,PCARB,PRFT,PC,
!           GRF,GROEAR,RGFILL,TTMP,GROGRN,SFAC,TFAC,RMNC,XNF,TNLAB,
!           RNLAB,RNOUT,SLFW,SLFN,SLFC,SLFT,PLAS,TI
!
!  OUTPUT :
!-----------------------------------------------------------------------
!  Called : PINE
!
!  Calls  : NFACTO NUPTAK
!-----------------------------------------------------------------------
!                         DEFINITIONS
!
!  GRF    :
!  GROGRN : Daily growth of the grain - g
!  I      : Loop counter
!  ICOLD  :
!  NOUTDO : File handle
!  NPOOL  : Total plant N available for translocation to grain (g/plant)
!  NPOOL1 : Tops N available for translocation to grain (g/plant)
!  NPOOL2 : Root N available for translocation to grain (g/plant)
!  NSDR   : Plant N supply/demand ratio used to modify grain N content
!  NSINK  : Demand for N associated with grain filling (g/plant/day)
!  PAR    : Daily photosynthetically active radiation, calculated as half
!           the solar radiation - MJ/square metre
!  PC     :
!  PCARB  : Daily amount of carbon fixed - g
!  PLAS   : The rate of senescence of leaf area on one plant - sq. cm/day
!  PRFT   : Photosynthetic reduction factor for low and high temperatures
!  RGFILL : Rate of grain fill - mg/day
!  RMNC   : Root minimum nitrogen concentration (g N/g root dry weight)
!  TI     : Fraction of a phyllochron interval which occurred as a fraction
!           of today's daily thermal time
!  TNLAB  :
!  TTMP   :
!=======================================================================

      SUBROUTINE Aloha_GROSUB (CONTROL, ISWITCH, 
     &    DTT, ISTAGE, NH4, NO3, SOILPROP, SW, SWFAC,!Input
     &    SUMDTT, TBASE, TURFAC, WEATHER, XSTAGE,             !Input
     &    AGEFAC, BASLFWT, BIOMAS, CRWNWT, EYEWT, FBIOM,      !Output
     &    FLRWT, FRTWT, FRUITS, GPP, GPSM, GRAINN, GRORT, SUMSRADGRO, SUMSRAD, SRADGRO, PARGRO, SUMPARGRO, SUMPAR,    !Output
     &    LAI, LFWT, LN, NSTRES, RLV, ROOTN, RTWT, SUMDTTGRO, SUMTMAXGRO, !Output
     &    SENESCE, SKWT, STMWT, STOVN, STOVWT,  TEMPM, SUMTMAX, TMAXGRO, GDDFR, !Output
     &    UNH4, UNO3, WTNUP, WTINITIAL, XGNP, YIELD)!Output QUITAR SUMDTTGRO DE AQUI Y YA SALE TODO

      USE Aloha_mod
      USE Interface_SenLig_Ceres
      IMPLICIT  NONE
      SAVE

      INTEGER   ICOLD
      REAL      PCARB,PRFT,PC,TI,GRF,RGFILL,
     &          SLFW,SLFN,SLFC,SLFT,PLAS
      REAL      TABEX,PCO2,Y1

      CHARACTER ISWNIT*1
      REAL    GROGRN,SFAC,TFAC,RMNC,XNF,TNLAB,RNLAB
      REAL    NSINK,NPOOL1,NPOOL2,NPOOL,NSDR,RNOUT
      INTEGER ICSDUR
      REAL    SEEDNI, ROOTN, STOVN, GRAINN, SEEDN, XANC
      REAL    APTNUP, RANC, GNP, NFAC, RCNP 
      REAL    TANC, VANC, VMNC, TMNC

      CHARACTER*6, PARAMETER :: ERRKEY ='GROSUB'
      CHARACTER*78 MSG(2)
      INTEGER I, ISTAGE, ISTAGE_old, IDURP, YRDOY
      INTEGER STGDOY(20), DAP1, DAP2, DAP3, DAP4, DAP5, DAP6, DAP7, DAP8, DAP9, DAP10, DAP11, DAP12, DAP13, DAP14, DAP15, DAP16, DAP17, DAP18, DAP19, YRPLT
      INTEGER DYNAMIC
      REAL    PLA, LAI, BIOMAS, LFWT, BASLFWT, STMWT, STOVWT, WTINITIAL, WTNEWCASE12
      REAL    RLAE13, RLDW13, RBWTDW13, RSTMWT13, LAI13, GDDFR13, PLA12, PLA13, LFWT12, LFWT13, BASLFWT12, BASLFWT13, STMWT12, STMWT13 !CASE 13
      REAL    RLAE1, RLDW1, RBWTDW1, RSTMWT1, LAI1, GDDFR1, PLA1, LFWT1, BASLFWT1, STMWT1 !CASE 1
      REAL    PLAG, RTWT, FLRWT, GROSTM, SENLA, SLAN, GRORT, GDDFR, TMAXGRO, PLACASE12
      REAL    GROBSL, GROLF, CUMPH, LN, CUMDEP, SUMP, PLAMX, GROFLR, BASLFWTCASE12
      REAL    GROCRWN, GROFRT, FRTWT, CRWNWT, SKWT, GROSK, PTF, EYEWT, STMWTCASE12
      REAL    SWMAX, SWMIN, NDEF3, NSTRES, AGEFAC, LIFAC, SRADGRO, PARGRO
      REAL    PAR, CC, TRF2, CARBO, SWFAC, TEMPM, LFWTCASE12  !,TRNU, 
      REAL    DTT, TURFAC, XN, CMF, TOTPLTWT, SUMDTT, GPP, SUMTMAX, SUMDTTGRO, SUMTMAXGRO
      REAL    PDWI, PGRORT, DM, FBIOM, MAXLAI, PHOTOSYNEYE, FRUITS, SUMSRADGRO, SUMSRAD, SUMPARGRO, SUMPAR
      REAL    YIELD, GPSM, XSTAGE  !, FDMC

      REAL    CO2, SRAD, TMIN, TMAX
      REAL    PLTPOP, SDWTPL, PLANTSIZE
      REAL    G2, G3, P8, PHINT, TBASE      !G2, G3, P4, PHINT, TBASE G2, G3, P7, PHINT, TBASE
      INTEGER PMTYPE, NFORCING
      REAL    GRNWT, SDWTAH, SDWTAM, WTNUP, BWAH
      REAL    WTNLF, WTNST, WTNSH, WTNRT, WTNLO
      REAL    NDEF4, ANFAC, ATANC, TCNP, XGNP, GNUP, TOTNUP
      REAL    CUMDTT, CANNAA, CANWAA
      REAL    PLIGLF, PLIGRT

      REAL, DIMENSION(10) :: CO2X, CO2Y
      REAL, DIMENSION(NL) :: RLV, NO3, NH4, SW, UNH4, UNO3

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (WeatherType) WEATHER
      TYPE (SoilType) SOILPROP
      TYPE (ResidueType) SENESCE

      DYNAMIC = CONTROL % DYNAMIC
      CO2  = WEATHER % CO2
      SRAD = WEATHER % SRAD
      TMIN = WEATHER % TMIN
      TMAX = WEATHER % TMAX

!=======================================================================
      SELECT CASE (DYNAMIC)
!=======================================================================
      CASE (RUNINIT)
!=======================================================================
      ISWNIT     = ISWITCH % ISWNIT
      PLA        = 0.0
      PLACASE12  = 0.0
      LAI        = 0.0
      BIOMAS     = 0.0
      LFWT       = 0.0
      BASLFWT    = 0.0
      STMWT      = 0.0
      STOVWT     = 0.0
      SWFAC      = 1.0
      TURFAC     = 1.0
      LN     = 0.0
      SUMDTTGRO = 0.0
      SUMTMAXGRO = 0.0
      SUMTMAX = 0.0
      SUMSRADGRO = 0.0
      SUMSRAD = 0.0
      SUMPARGRO = 0.0
      SUMPAR = 0.0
      FLRWT  = 0.0
      FRTWT  = 0.0
      CRWNWT = 0.0
      SKWT   = 0.0
      GROSK  = 0.0
      YIELD  = 0.0
      SENLA  = 0.0
      SLAN   = 0.0
      CARBO  = 0.0
      GRNWT  = 0.0  !Not ever given a value, but used to compute SDWT
      RTWT   = 0.0
      SDWTAH = 0.0  !Only used for output in OPHarv
      SDWTAM = 0.0
      BWAH   = 0.0
      WTNLF  = 0.0
      WTNST  = 0.0
      WTNSH  = 0.0
      WTNRT  = 0.0
      WTNLO  = 0.0
      GPSM   = 0.0
      GPP    = 0.0
      PTF    = 0.0
      FRUITS = 0.0

      DO I = 1, NL
         RLV(I) = 0.0
      END DO

      BIOMAS = 0.0
      LAI    = 0.0
      XN     = 0.0
      SWFAC  = 1.0
      TURFAC = 1.0
      NDEF4  = 1.0
      ANFAC  = 0.0
      ATANC  = 0.0
      VANC   = 0.0
      VMNC   = 0.0
      SEEDNI = 0.0
      GRAINN = 0.0
      GNP    = 0.0
      XGNP   = 0.0
      APTNUP = 0.0
      GNUP   = 0.0
      TOTNUP = 0.0
      CUMDTT = 0.0
      SUMDTT = 0.0
      DTT    = 0.0
      CANNAA = 0.05
      CANWAA = 0.0
      SUMDTTGRO = 0.0
      SUMTMAXGRO = 0.0
      SUMTMAX = 0.0
      SUMSRADGRO = 0.0
      SUMSRAD = 0.0
      SUMPARGRO = 0.0
      SUMPAR = 0.0

      PLAG    = 0.0   ! PLAG (cm^2) is daily green leaf area growth
      GROSTM  = 0.0   ! GROSTM (g/plant/day) is daily stem growth
      GRORT   = 0.0   ! GRORT (g/plant/day) is daily root growth
      GROBSL  = 0.0   ! GROBSL (g/plant/day) is daily basal leaf growth
      GROLF   = 0.0   ! GROLF (g/plant/day) is daily green leaf growth
      GROFLR  = 0.0
      GROCRWN = 0.0
      GROFRT  = 0.0

      CALL Aloha_NFACTO (DYNAMIC, 
     &    ISTAGE, TANC, XSTAGE,                           !Input
     &    AGEFAC, NDEF3, NFAC, NSTRES, RCNP, TCNP, TMNC)  !Output

      CALL Aloha_NUPTAK(CONTROL, ISWITCH, 
     &    ISTAGE, NO3, NH4, PDWI, PGRORT, PLIGRT,         !Input
     &    PLTPOP, PTF, RANC, RCNP, RLV, RTWT, SOILPROP,   !Input
     &    STOVWT, SW, TCNP, XSTAGE,                       !Input
     &    ROOTN, SENESCE, STOVN, TANC, UNH4, UNO3, WTNUP) !Output

!=======================================================================
      CASE (SEASINIT)
!=======================================================================
      SDWTPL    = PLANTING % SDWTPL
      PLTPOP    = PLANTING % PLTPOP
      PMTYPE    = PLANTING % PMTYPE
      NFORCING  = PLANTING % NFORCING
      PLANTSIZE = PLANTING % PLANTSIZE

      CO2X = SPECIES % CO2X
      CO2Y = SPECIES % CO2Y
      CC   = SPECIES % CONV      !modifica PCARB este dato esta en el archivo especies (CONV) y podria ajustar la biomasa con una ecuacion de regresion aqui. Zhang dijo que no tenia datos de uso radiacion use eficience para la piña yo si los tengo. En maiz usan 5g/Mj en piña Zhang uso 2.5 (este es el valor CONV). Estos cambio se darian a partir del ciclo 1.
      CMF  = Species % CMFC
      LIFAC= Species % LIFAC

      G2  = CULTIVAR % G2
      G3  = CULTIVAR % G3
      P8  = CULTIVAR % P8                  ! P4  = CULTIVAR % P4   P7  = CULTIVAR % P7 
      PHINT = CULTIVAR % PHINT

      PLA        = 0.0
      LAI        = 0.0
      BIOMAS     = 0.0
      LFWT       = 0.0
      BASLFWT    = 0.0
      STMWT      = 0.0
      STOVWT     = 0.0
      LN     = 0.0

!     Calculate initial SEED N
      SEEDNI = (ROOTN+STOVN+GRAINN+SEEDN)*PLTPOP

      ISTAGE_OLD = 0

!     Initialize senescence variables
      CALL SenLig_Ceres(PLIGLF=PLIGLF, PLIGRT=PLIGRT)

      CALL Aloha_NFACTO (DYNAMIC, 
     &    ISTAGE, TANC, XSTAGE,                           !Input
     &    AGEFAC, NDEF3, NFAC, NSTRES, RCNP, TCNP, TMNC)  !Output

      CALL Aloha_NUPTAK(CONTROL, ISWITCH, 
     &    ISTAGE, NO3, NH4, PDWI, PGRORT, PLIGRT,         !Input
     &    PLTPOP, PTF, RANC, RCNP, RLV, RTWT, SOILPROP,   !Input
     &    STOVWT, SW, TCNP, XSTAGE,                       !Input
     &    ROOTN, SENESCE, STOVN, TANC, UNH4, UNO3, WTNUP) !Output

!=======================================================================
      CASE (RATE)
!=======================================================================
      TEMPM = (WEATHER % TMAX + WEATHER % TMIN) / 2.

      IF (ISWNIT .NE. 'N') THEN
!       Top actual N concentration (g N/g Dry weight)
        XANC   = TANC*100.0               
        APTNUP = STOVN*10.0*PLTPOP

        IF (ISTAGE .LT. 10) THEN                      !IF (ISTAGE .LT. 7) THEN     JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
          CALL Aloha_NFACTO (DYNAMIC, 
     &      ISTAGE, TANC, XSTAGE,                           !Input
     &      AGEFAC, NDEF3, NFAC, NSTRES, RCNP, TCNP, TMNC)  !Output
        ENDIF
      ENDIF

      IF (ISTAGE .GT. 8) RETURN                     ! IF (ISTAGE .GT. 5) RETURN   JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included

!-----------------------------------------------------------------

      PAR   = 0.5*SRAD
      Y1    = EXP(-LIFAC*LAI)                       ! Beer's law
      PCARB = CC*PAR/PLTPOP*(1.0-Y1)                ! on per plant basis
      !
      ! Calculate Photosynthetic Response to CO2
      !
      PCO2  = TABEX (CO2Y,CO2X,CO2,10)
      PCARB = PCARB*PCO2

      TEMPM = 0.6*TMIN + 0.4*TMAX
      SELECT CASE (ISTAGE)
        CASE (1,2,3,4,5,10,11,12,13)    !CASE (1,2,3,4,5,6,10,11,12)                      ! CASE (1,2,3,7,8,9)  JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
          IF (TEMPM .LE. 25.0) THEN
             PRFT = 1.0-0.001*(TEMPM-25.0)**2              !  PRFT   : Photosynthetic reduction factor for low and high temperatures
           ELSEIF (TEMPM .LT. 29.0) THEN
             PRFT = 1.0-0.056*(TEMPM-25.0)**2
           ELSE
             PRFT = 0.1                                !OTRA OPORTUNIDAD DE MODIFICAR LO DE CLIMA CALIENTE, PERO HAY QUE VER EL IMPACTO SOBRE LA BIOMASA DE PRFT
          ENDIF
        CASE (6,7,8,9)                                   !CASE (4,5,6) JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
          PRFT = 1.0-0.005*((0.4*TMIN+0.6*TMAX)-26.)**2
          PRFT = AMAX1 (PRFT,0.0)
      END SELECT
!-----------------------------------------------------------------
      !
      ! Temperature factor
      !
      IF (TEMPM .LT. 15.0) THEN                               ! 
         TRF2 = 0.45                                          ! 
       ELSEIF (TEMPM .GE. 15.0 .AND. TEMPM .LT. 30.0) THEN    ! Con el el OUTPUT Plant Gro.out puedo ir cuadrando las variables del archivo T.
         TRF2 = 0.082*EXP(0.1*TEMPM)
       ELSE
         TRF2 = 1.65                                          ! TRF2 = 1.65 SER� QUE ESTE VALOR MODIFICA EL PESO DE LA ZONA CALIENTE?
      ENDIF

      IF (ISTAGE .GE. 6 .AND. ISTAGE .LT. 10) THEN                 !IF (ISTAGE .GE. 4 .AND. ISTAGE .LT. 7) THEN    JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
         CARBO = PCARB*AMIN1(PRFT,0.55+0.45*SWFAC,NSTRES)          !IF (ISTAGE .GE. 8 .AND. ISTAGE .LT. 10) THEN ojo la linea anterior estaba as� el 25/03/2021
       ELSE
         CARBO = PCARB*AMIN1(PRFT,SWFAC,NSTRES)
      ENDIF
      DTT = AMAX1 (DTT,0.0)
      
!----------------------------------------------------------------- OJO AQUI LOGRO MODIFICAR EL NUMERO DE HOJAS
      IF (ISTAGE .LE. 4) THEN                                    ! Creo que si aqu� en lugar de LE coloco LT deja de salir m�s hojas o modificarse LAI (no recuerdo que es) despu�s de forza.
!                                                                ! IF (ISTAGE .LE. 3) THEN  debe ser 4 para que deje de producir hojas a partir de forza.
!        Calculate leaf emergence                                ! Originalmente lo hab�a modificado como 5 pero era 4. Sucer� lo mismo m�s abajo?
!        The first 5 leaves grow faster than other leaves, used for maize
!         
         PC = 1.0                                             ! PC Used to compute fraction of phyllochron interval occurring today
         IF (CUMPH .LE. 13.0) THEN                            ! Condicion para detonar la ecuacion para cuando hay 13 o menos de 13 hojas. 
            PC = 0.85 - 0.25/13*CUMPH                         ! leaves from first foliar cycle are slower than second cicle and third cycle.
          ELSEIF (CUMPH .GE. 13.0 .AND. CUMPH.LE. 26.0) THEN  ! Condicion para detonar la ecuacion para cuando hay mas de 13 y menos de 26 hojas.
         PC = 1.25 - 0.25/7.3*CUMPH      !0.4                 ! 
          ELSE                                                ! Condicion que se detona cuando no se cumple ninguna de las anteriores (mas de 26 hojas).
         PC = 1.25 - 0.25/8.35*CUMPH     !0.5                 ! SI BAJO SUBO Prueba cuando tenga datos de mas de 26 hojas tengo que revisar que numero debe ser este 10.8.
      ENDIF
!         
!        TI is the fraction of leaf emerged for a day.  It is calculated from
!        the following equations
!        
!        Correcting water stress effect and effect due to shading.
!                                                            OJO AQUI LOGRO BAJAR EL LAI
         IF (ISTAGE .LE. 4) THEN                             !   
            IF (TEMPM .GT. TBASE .AND. SRAD .LT. 24) THEN    ! Ojo para Typic        
               IF ((LN*PLTPOP) .LE. (13*PLTPOP)) THEN        ! Condicion para detonar la ecuacion para cuando hay 13 o menos de 13 hojas.               
                  TI = TURFAC*(1.0/PHINT)*(DTT-2.0)/PC       !
               ELSEIF ((LN*PLTPOP) .GT. (13*PLTPOP) .AND.    ! Condicion para detonar la ecuacion para cuando hay mas de 13 y menos de 26 hojas.
     &          (LN*PLTPOP) .LE. (26*PLTPOP) ) THEN          !               
                  TI = TURFAC*(1.0/PHINT)*1.01*(DTT-2.0)/PC 
                   
                  ELSEIF ((LN*PLTPOP) .LE. (39*PLTPOP)) THEN ! Condicion para detonar la ecuacion para cuando hay 39 o menos de 39 hojas. 
               TI = TURFAC*(1.0/PHINT)*(DTT-2.0)/PC !  Condicion para detonar la ecuacion para cuando hay mas de 26 y menos de 39 hojas.
     
                        
                ELSE
                  TI = TURFAC*(1.0/PHINT)*1.05*(DTT-2.0)/PC  !Esta es la ecuacion que se detona cuando no se cumple ninguna de las anteriores.
               ENDIF
            ELSE
                IF (TEMPM .GT. TBASE .AND. SRAD .GE. 24) THEN!Ojo para Warm
               IF ((LN*PLTPOP) .LE. (13*PLTPOP)) THEN        !  Condicion para detonar la ecuacion para cuando hay 13 o menos de 13 hojas.              
                  TI = TURFAC*(1.05/PHINT)*DTT/PC
                
                 ELSEIF ((LN*PLTPOP) .GT. (13*PLTPOP)) THEN  !  Condicion para detonar la ecuacion para cuando hay mas de 13 y menos de 26 hojas.             
                  TI = TURFAC*(1.0/PHINT)*0.5*DTT/PC   
                  
                  
                  ELSEIF ((LN*PLTPOP).LE.(26*PLTPOP)) THEN
                  TI = TURFAC*(1.0/PHINT)*(-0.001*LN*PLTPOP+1.05)* !  Condicion para detonar la ecuacion para cuando hay mas de 26 y menos de 39 hojas.
     &                 DTT/PC
                ELSE
                  TI = TURFAC*(1.0/PHINT)*1.05*DTT/PC        ! Esta es la ecuacion que se detona cuando no se cumple ninguna de las anteriores.
               ENDIF
             ELSE
!              If mean air temperature is less than Tbase, no leaf emerges
               TI = 0.0     
            ENDIF
                
            ENDIF         
          IF (ISTAGE .EQ. 4) THEN     !Esta etapa es forzamiento por lo tanto no se producen más hojas.
           TI = 0.0    
           ENDIF
         ENDIF

!        CUMPH is number of expanded leaves. It is updated daily
         CUMPH = CUMPH + TI 
!        XN is leaf number of the oldest expanding leaf
         XN    = CUMPH +  1 
         LN    = XN         ! LN is leaf number
      ENDIF  ! CIERRA IF (ISTAGE .LE. 4) THEN    

!-----------------------------------------------------------------
!  ISTAGE Definition
!     7 - Preplanting                                                 10         11 Start simulation to planting
!     8 - Planting to root initiation                                 11         12 Planting to Root Initiation
!     9 - Root initiation to first new leaf emergence                 12         13 Root Initiation to First New Leaf
!     1 - First new leaf emergence to net zero root growth            1           1 First new leaf emergence to foliar cycle 1
!     2 - Net zero stem growth to forcing                             2,3,4   2,3,4 Foliar cycle 1 to foliar cycle 2,3 and forcing 
!     3 - Forcing to sepals closed on youngest flowers                5,6         5 Forcing to Open Heart
!     4 - SCY to first open flower                                    7           6 Open Heart to EarlyAnthesis
!     5 - Fruit growth                                                8           7 Early Anthesis to Last Anthesis
!     6 - Physiological maturity                                      9           8 Last Anthesis to Physiological maturity
!-----------------------------------------------------------------                9 Physiology to Harvest
      SELECT CASE (ISTAGE)                                            !           10 Harvest
!-----------------------------------------------------------------
      CASE (1)     !CASE (1)
        !
        ! First new leaf emergence to net zero root growth
        !
        
            IF ((LN*PLTPOP) .GT. (0*PLTPOP)) THEN                   
               PLAG = CMF*0.77*(174.0+16.0*XN)*0.5*TI*TURFAC
               
             ELSEIF ((LN*PLTPOP) .GE. (1*PLTPOP)) THEN
               PLAG = CMF*0.77*(174.0+16.0*XN)*TI*TURFAC
     1                *(-0.001*LN*PLTPOP+1.05)
             ELSE
               PLAG = CMF*0.77*(174.0+16.0*XN)*TI*TURFAC
            ENDIF
        
!        
!       Green leaf weight is calculated from leaf area
!        
        GROLF  = PLAG*(1.0/((85.0*EXP(-XN*0.012))*TRF2))
        GROBSL = 0.42*GROLF                                                           ! GROBSL = 0.42*GROLF
!        
!       Daily root growth is calculated from carbo and daily leaf weight
!       
!       If GRORT is less than 15% of carbo then set to 15% of carbo. A growth reducing
!       factor (GRF) is calculated and GROLF, GROBSL are reduced by GRF. And PLA is
!       recalculated.
!       
        GRORT = CARBO - GROLF - GROBSL
        IF (GRORT .LT. 0.15*CARBO) THEN
           IF (GROLF .GT. 0.0 .OR. GROBSL .GT. 0.0) THEN
              GRF   = CARBO*0.85/(GROLF+GROBSL)
              GRORT = CARBO*0.15
            ELSE
              GRF   = 1.0
           ENDIF
           GROLF  = GROLF  * GRF
           GROBSL = GROBSL * GRF
           PLAG   = GROLF  * ((85*EXP(-XN*0.012))*TRF2)
        ENDIF

C       PLA     = (LFWT+GROLF)**0.87*96.0
        LFWT    = LFWT+GROLF      !Update green leaf weight
        BASLFWT = BASLFWT+GROBSL  !Update basal leaf weight
        PLA     = PLA+PLAG        !Update total leaf area.         
                                                                  
        IF (GROLF .GT. 0.0) THEN
           SLAN = PLA/1000.       !assumed 0.001 leaf area senescence
        ENDIF
        LFWT = LFWT-SLAN/600.0    !recalculate green leaf weight  QUE SIGNIFICA EL 600 LFWT = LFWT-SLAN/600.0 SI LO PONGO EN 100 BAJA LWAD EN ARCHIVO T
                                  ! Deduciendolo de otras ecuaciones (que podr�an no tener relaci�n) el 600 es el peso total de la planta TOTPLTWT en gramos
                                  !Cuando tenga datos del primer ciclo foliar debo ver qu� es lo que calcula para la BIOMASA y de ah� ver como ajusto ese dato
!-----------------------------------------------------------------
      CASE (2,3,4)    !CASE (2,3,4)                     ! AQUI ES DONDE TENGO QUE HACER MODIFICACIONES PARA CALIBRAR DE CICLO 1 A CICLO 3.
        !
        ! Net zero stem growth to forcing               ! AQUI ES DONDE HAY QUE HACER LOS CALCULOS PARA AJUSTAR O DIFERENCIAR LAS ZONAS EN LOS PESOS
        !                                               ! DESPUES DE LAS ETAPAS 11, 12 Y 1
       
            IF ((LN*PLTPOP) .GE. (13*PLTPOP)) THEN                     ! OJO SI ESTO ES AS� ENTONCES SI LOGRO MODIFICAR EL INTERVALO DEL FILOCRON PARA LA ZONA TIPICA Y CALIENTE
               PLAG = CMF*1.75*(33.0+7.5*XN)*0.5*TI*TURFAC             ! PODR�A ENTONCES ESTO HACER QUE SE DIFERENCIEN EN PESO,
             ELSEIF ((LN*PLTPOP) .GE. (26*PLTPOP)) THEN
               PLAG = CMF*1.75*(33.0+7.5*XN)*TI*TURFAC
     1               *(-0.001*LN*PLTPOP+1.05)
             ELSE
               PLAG = CMF*1.75*(33.0+7.5*XN)*TI*TURFAC
           ENDIF
        

        GROLF  = PLAG*(1/((85*EXP(-XN*0.012))*TRF2))
        GROBSL = 0.42*GROLF
        !
        ! Calculation of daily stem growth
        !
        GROSTM = 0.52*GROBSL
        GROSTM = AMIN1 (GROSTM,GROBSL)
        !
        ! Check the balance of supply and demand
        !
        GRORT = CARBO - GROLF - GROBSL - GROSTM
        IF (GRORT .LT. 0.15*CARBO) THEN
           IF (GROLF .GT. 0.0 .OR. GROBSL .GT. 0.0 .OR.
     &         GROSTM .GT. 0.0) THEN
              GRF   = CARBO*0.9/(GROLF+GROBSL+GROSTM)
              GRORT = CARBO*0.1
            ELSE
              GRF = 1.0
           ENDIF

           GROLF  = GROLF  * GRF
           GROBSL = GROBSL * GRF
           GROSTM = GROSTM * GRF

           PLAG   = GROLF*((85.0*EXP(-XN*0.012))*TRF2)
        ENDIF

        LFWT    = LFWT    + GROLF
        BASLFWT = BASLFWT + GROBSL
        STMWT   = STMWT   + GROSTM
        PLA     = PLA     + PLAG

        IF (GROLF .GT. 0.0) THEN
           SLAN = PLA/1000.0
        ENDIF
        LFWT = LFWT-SLAN/600.0

!-----------------------------------------------------------------

       
        
      CASE (5,6)      ! CASE (5,6)                                      ! CASE (3)
        !
        ! Forcing to sepals closed on youngest flowers
        !
          IF ((LN*PLTPOP) .GT. (26*PLTPOP)) THEN               
               PLAG = CMF*2.*(33.0+7.5*XN)*TI*TURFAC
             ELSEIF ((LN*PLTPOP) .GE. (39*PLTPOP)) THEN
               PLAG = CMF*2.*(33.0+7.5*XN)*0.5*TI*TURFAC          !Cambios aqu� repercuten de Open Heart en adelante.
     &               *(-0.001*LN*PLTPOP+1.05)
             ELSE
               PLAG = CMF*2.*(33.0+7.5*XN)*TI*TURFAC
            ENDIF
          

        GROLF  = PLAG*(1.0/((85*EXP(-XN*0.012))*TRF2))
        GROBSL = 0.42 * GROLF
        GRORT  = 0.05 * GROLF
        GRORT  = AMAX1 (GRORT,0.0)
        GROFLR = (1.26-0.17*PLTPOP+0.0075*PLTPOP**2)*DTT/20.5
     &           *AMIN1(AGEFAC,TURFAC)
        GROFLR = AMAX1 (GROFLR,0.0)
        GROSTM = CARBO - GROLF - GROBSL - GRORT - GROFLR
        IF (GROSTM .LT. 0.16*CARBO) THEN
           IF (GROLF .GT. 0.0 .OR. GROBSL .GT. 0.0 .OR. GRORT .GT. 0.0
     &         .OR. GROFLR .GT. 0.0) THEN
              GRF    = CARBO*0.84/(GROLF+GROBSL+GRORT+GROFLR)
              GROSTM = CARBO*0.16
            ELSE
              GRF    = 1.0
           ENDIF
           GROLF  = GROLF  * GRF
           GROBSL = GROBSL * GRF
           GRORT  = GRORT  * GRF
           GROFLR = GROFLR * GRF
           PLAG   = GROLF*((85.0*EXP(-XN*0.012))*TRF2)/2.0
        ENDIF

        LFWT    = LFWT    + GROLF
        PLA     = PLA     + PLAG
        BASLFWT = BASLFWT + GROBSL
        STMWT   = STMWT   + GROSTM
        FLRWT   = FLRWT   + GROFLR
!     chp added FRTWT and CRWNWT because these are in daily output
!     this allows mass balance to work.
!       FRTWT  = FLRWT*0.7      !CHP 10/14/2017
!       CRWNWT = FLRWT*0.3      !CHP 10/14/2017

        IF (GROLF .GT. 0.0) THEN
           SLAN = PLA/1000.0
        ENDIF
        LFWT  = LFWT  - SLAN/600.0
        SUMP  = SUMP  + CARBO !Total biomass cumulated during the stage
        IDURP = IDURP + 1     !Duration of the stage        
!-----------------------------------------------------------------
 
      CASE (7)        !CASE (7)        !CASE (4)
        !
        ! SCY to first open flower
        !
        GROFLR = (1.26-0.17*PLTPOP+0.0075*PLTPOP**2)*DTT/20.5*
     &           AMIN1(AGEFAC,TURFAC)
        GROFLR = AMAX1 (GROFLR,0.0)
        GRORT  = 0.05*GROFLR

        IF (TOTPLTWT .LE. 600.0) THEN                                   
           GROSTM = CARBO - GRORT - GROFLR
           IF (GROSTM .LT. 0.16*CARBO) THEN
              IF (GRORT .GT. 0.0 .OR. GROFLR .GT. 0.0) THEN
                 GRF    = CARBO*0.84/(GRORT+GROFLR)
                 GROSTM = CARBO*0.16
               ELSE
                 GRF    = 1.0
              ENDIF
              GRORT  = GRORT  * GRF
              GROFLR = GROFLR * GRF
              GROSK  = GROSK  * GRF
           ENDIF
         ELSE
           GROSK  = CARBO*0.15
           GROSTM = CARBO - GROSK - GRORT - GROFLR   !Sucker initiation
           IF (GROSTM .LE. 0.16*CARBO) THEN
              IF (GROSK .GT. 0.0 .OR. GROFLR .GT. 0.0 .OR.
     &            GRORT .GT. 0.0) THEN
                 GRF = CARBO*0.84/(GRORT+GROFLR+GROSK)
               ELSE
                 GRF = 1.0
              ENDIF
              GRORT  = GRORT  * GRF
              GROFLR = GROFLR * GRF
              GROSK  = GROSK  * GRF
           ENDIF
        ENDIF

        IF (GROSTM .GT. 0.2*CARBO) THEN
           GROSTM = 0.2*CARBO
        ENDIF
        STMWT = STMWT + GROSTM
        FLRWT = FLRWT + GROFLR
!       FRTWT  = FLRWT*0.7      !CHP 10/14/2017
!       CRWNWT = FLRWT*0.3      !CHP 10/14/2017
        SKWT  = SKWT  + GROSK
        
!-----------------------------------------------------------------
      CASE (8)         ! CASE (8)                ! CASE (5)
        !
        ! Fruit growth
        !

        IF (SWMAX .LE. 0.0) THEN
           IF (XSTAGE .GE. 10.0) THEN
              SWMAX = STMWT
              SWMIN = 0.65*SWMAX
           ENDIF
        ENDIF

        IF (SUMDTT .GT. P8) THEN           !IF (SUMDTT .GT. P4) THEN   IF (SUMDTT .GT. P7) THEN 
           GROSK = CARBO*0.1
           STMWT = STMWT
           SKWT  = SKWT + GROSK
           GO TO 2400
        ENDIF

        SLAN    = SUMDTT*PLA/10000.0
        LFWT    = LFWT*(1.0-1/1000.0)
        BASLFWT = BASLFWT*(1.0-1.0/2000.0)
        RGFILL  = 1-0.0025*(TEMPM-26.)**2

        IF (PMTYPE .GE. 1) THEN
           IF (SUMDTT .GT. 0.5*P8) THEN    !IF (SUMDTT .GT. 0.5*P4) THEN  IF (SUMDTT .GT. 0.5*P7) THEN
              IF (SRAD .LT. 12.0) THEN     
                 GROFRT = GPP*G3*0.001*(0.7+0.3*SWFAC)*(SRAD/12.)
               ELSEIF (SRAD .LT. 36.0) THEN !DEBERIA USAR AQUI 22 Y VER QUE SUCEDE ENTRE LA TIPICA Y LA CALIENTE.
                 GROFRT = GPP*G3*0.001*(0.7+0.3*SWFAC)*(1.5-SRAD/24.)
               ELSE
                 GROFRT = 0.0
              ENDIF
            ELSE
              IF (SRAD .LT. 12.0) THEN
                 GROFRT = GPP*G3*0.001*(0.7+0.3*SWFAC)*(0.6+0.4*
     &                    SRAD/12.)
               ELSEIF (SRAD .LT. 36.0) THEN
                 GROFRT = GPP*G3*0.001*(0.7+0.3*SWFAC)*(0.6+0.4*
     &                    (1.5-SRAD/24.))
               ELSE
                 GROFRT = 0.0
              ENDIF
           ENDIF
         ELSE
           GROFRT = RGFILL*GPP*G3*0.001*(0.7+0.3*SWFAC)
        ENDIF

        GROCRWN = 0.125*GROFRT
        GRORT   = CARBO*0.05

        IF (TOTPLTWT .GT. 600.0) then
           GROSK  = CARBO*0.15
           GROSK  = AMAX1 (GROSK,0.0)
           GROSTM = CARBO - GROFRT - GROCRWN - GRORT - GROSK
           IF (GROSTM.LT.0.0) GO TO 1700
           IF (GROSTM .GT. 0.15*CARBO) THEN
              GROSTM = 0.15*CARBO
           ENDIF
           SKWT   = SKWT   + GROSK
           STMWT  = STMWT  + GROSTM
           CRWNWT = CRWNWT + GROCRWN
           GO TO 1900
        ENDIF

        GROSTM = CARBO - GROFRT - GROCRWN - GRORT
        IF (GROSTM.LT.0.0) GO TO 1700
        IF (GROSTM .GT. 0.15*CARBO) THEN
           GROSTM = 0.15*CARBO
        ENDIF
        CRWNWT = CRWNWT + GROCRWN
        STMWT  = STMWT  + GROSTM
        GO TO 1900

1700    SELECT CASE (PMTYPE)
          CASE (1:13)                         ! Tengo que revisar que dec�a el original aqu�.
            IF (SUMDTT .LT. 0.8*P8) THEN      ! IF (SUMDTT .LT. 0.8*P4) THEN   IF (SUMDTT .LT. 0.8*P7) THEN 
               IF (SRAD .LT. 6.0) THEN
                  GROSTM  = CARBO
                ELSEIF (SRAD .LT. 13.0) THEN
                  GROSTM  = CARBO*((13.-SRAD)/7.)
                  GROFRT  = (CARBO-GROSTM)*0.889
                  GROCRWN = (CARBO-GROSTM)*0.111
                ELSE
                  GROSTM  = 0.0
                  GROFRT  = 0.889*CARBO
                  GROCRWN = 0.111*CARBO
               ENDIF
               STMWT  = STMWT  + GROSTM
               CRWNWT = CRWNWT + GROCRWN
             ELSE
               IF (SRAD .LT. 6.0) THEN
                  GROSTM  = CARBO
                ELSEIF (SRAD .LT. 13.0) THEN
                  GROSTM  = CARBO*((13.0-SRAD)/7.0)
                  GROFRT  = (CARBO - GROSTM)*0.889
                  GROCRWN = (CARBO - GROSTM)*0.111
                  STMWT   = STMWT  + GROSTM
                  CRWNWT  = CRWNWT + GROCRWN
                ELSE
                  STMWT   = STMWT + CARBO - GROFRT - GROCRWN - GRORT
                  IF (STMWT .LT. SWMIN) THEN
                     STMWT   = SWMIN
                     GROFRT  = 0.889  * CARBO
                     GROCRWN = 0.111  * CARBO
                     CRWNWT  = CRWNWT + GROCRWN
                  ENDIF
               ENDIF
            ENDIF
          CASE (0)
            GROSTM  = 0.0
            GROFRT  = 0.889  * CARBO
            GROCRWN = 0.111  * CARBO
            STMWT   = STMWT  + GROSTM
            CRWNWT  = CRWNWT + GROCRWN
        END SELECT

 1900   IF (ISWNIT .EQ. 'Y') THEN
           !
           ! Grain N allowed to vary between .01 and .018.
           ! High temp., low soil water, and high N increase grain N
           !
           SFAC  = 1.125 - 0.1250*TURFAC
           TFAC  = 0.690 + 0.0125*TEMPM
           GNP   = (0.004+0.013*NFAC)*AMAX1(SFAC,TFAC)
           NSINK = GROGRN*GNP   
 
           IF (NSINK .GT. 0.0) THEN
              RMNC   = 0.75*RCNP
              RANC   = AMAX1  (RANC,RMNC)
              VANC   = STOVN / STOVWT
              VANC   = AMAX1  (VANC,VMNC)
              NPOOL1 = STOVWT*(VANC-VMNC)
              NPOOL2 = RTWT  *(RANC-RMNC)
              XNF    = 0.15  + 0.25*NFAC
              TNLAB  = XNF   * NPOOL1
              RNLAB  = XNF   * NPOOL2
              NPOOL  = TNLAB + RNLAB
              IF (ICSDUR .EQ. 1) THEN
                 GPP = AMIN1(GPP*NDEF3,(NPOOL/(0.062*.0095)))
              ENDIF
              NSDR = NPOOL/NSINK
              IF (NSDR .LT. 1.0) THEN
                 NSINK = NSINK*NSDR
              ENDIF
              IF (NSINK .GT. TNLAB) THEN
                 STOVN = STOVN - TNLAB
                 RNOUT = NSINK - TNLAB
                 ROOTN = ROOTN - RNOUT
                 RANC  = ROOTN / RTWT
               ELSE
                 STOVN = STOVN - NSINK
                 VANC  = STOVN / STOVWT
              ENDIF
           ENDIF
 
           GRAINN = GRAINN + NSINK
        ENDIF
        !
        ! Update fruit weight
        !
        FRTWT = FRTWT + GROFRT
        FLRWT = FLRWT + GROFRT + GROCRWN
        IF (SUMDTT .GT. 0.8*P8) THEN       !IF (SUMDTT .GT. 0.8*P4) THEN   IF (SUMDTT .GT. 0.8*P7) THEN  
           STMWT = AMIN1 (STMWT,SWMAX)
        ENDIF

!-----------------------------------------------------------------
      CASE (9)      !CASE (9)              !     CASE (6)
!        
!       Physiological maturity
!        
        RETURN


      END SELECT
!-----------------------------------------------------------------

      IF (CARBO .EQ. 0.0) THEN
         CARBO = 0.001                 ! Make sure that carbo is not 0.
      ENDIF
!     PDWI (g/plant/day) is potential shoot growth
      PDWI   = PCARB*(1.0-GRORT/CARBO) 
!     Pgrort is potential root growth
      PGRORT = PCARB*GRORT/CARBO       
!    
!     Calculation of zero-to-unity factors for leaf senescence due to drought
!     stress (SLFW), competition for light (SLFC), and low temperature (SLFT).
!    
 2400 SLFW = 1.0
      SLFN = 0.95+0.05*AGEFAC
      SLFC = 1.0
      IF (ISTAGE .GT. 2 .AND. ISTAGE .LT. 10) THEN                    !  IF (ISTAGE .GT. 2 .AND. ISTAGE .LT. 7) THEN JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
         IF (LAI .GT. 6.0) THEN
            SLFC = 1.0-0.0005*(LAI-6.0)
         ENDIF
      ENDIF
      
           
      

      SLFT = 1.0
      IF (TEMPM .LE. 4.0) THEN
         SLFT = 1.0-(4.0-TEMPM)/4.0
      ENDIF

      IF (TMIN .GT. 0.0) THEN
         ICOLD = 0
       ELSE
         SLFT  = 0.0
         ICOLD = ICOLD + 1
      ENDIF
!      
!     Leaf area senescence on a day (PLAS) and LAI is calculated for stage 1 to 5
!      
      SLFT  = AMAX1 (SLFT,0.0)
      PLAS  = (PLA-SENLA)*(1.0-AMIN1(SLFW,SLFC,SLFT))
      SENLA = SENLA + PLAS
      SENLA = AMAX1 (SENLA,SLAN)
      SENLA = AMIN1 (SENLA,PLA)
      LAI   = (PLA-SENLA)*PLTPOP*0.0001     ! ESTA LINEA GOBIERNA LAI A PARTIR DE ISTAGE 1 LAS OTRAS VER: CASE (11) Y CASE (12)
                                            ! AQUI ES DONDE PUEDO HACER QUE LAI SEA DIFERENTE EN CLIMA CALIENTE Y CLIMA TIPICO
      
        
      

      IF (LN .GT. 3 .AND .LAI .LE. 0.0 .AND. ISTAGE .LE. 6) THEN             !  IF (LN .GT. 3 .AND .LAI .LE. 0.0 .AND. ISTAGE .LE. 3) THEN JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
         WRITE (MSG(1),   2800)
         CALL WARNING(1, ERRKEY, MSG)
!         IF (IDETO .EQ. 'Y') THEN
!            WRITE (NOUTDO,2800)
!         ENDIF
         ISTAGE = 7                                       ! ISTAGE = 4 JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
       ELSE
         IF (ICOLD .GE. 7) THEN
            WRITE (MSG(1),   2800)
            CALL WARNING(1, ERRKEY, MSG)
!            IF (IDETO .EQ. 'Y') THEN
!               WRITE (NOUTDO,2800)
!            ENDIF
           ISTAGE = 8                                     !ISTAGE = 5 JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
         ENDIF
      ENDIF
!      
!     Half GRORT is used for respiration and 0.5% of root is lost due to senescence
!      
      RTWT = RTWT + 0.45*GRORT - 0.0025*RTWT
!      
!     Finally, total biomass per unit area (BIOMAS g/m2), total plant weight,
!     Total plant dry weight per hectare (DM kg/ha) and Plant top fraction
!     (PTF) are calculated
!      
!     When fruit development starts, the fruit population (FRUITS) is
!       less than the plant population (PLTPOP). Need to differentiate
!       for consistency with daily and seasonal outputs.
      SELECT CASE(ISTAGE)
      CASE(8,9,10)                                             ! CASE(5,6)  JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
!       In this case FLRWT is fruit + crown
        BIOMAS   = (LFWT + STMWT + BASLFWT + SKWT)*PLTPOP 
     &                + (FLRWT * FRUITS) 
      CASE DEFAULT
        BIOMAS   = (LFWT + STMWT + FLRWT + BASLFWT + SKWT)*PLTPOP !SUPONGO QUE AQUI SE CALCULA BIOMASA DE CASE 2 EN ADELANTE, LAS OTRAS ESTAN EN CASE 11 Y 12
      END SELECT 

      TOTPLTWT =  LFWT + STMWT + FLRWT + BASLFWT + SKWT
      DM       = BIOMAS*10.0
      STOVWT   = LFWT + STMWT
      PTF      = (LFWT+BASLFWT+STMWT+FLRWT+SKWT) /
     &           (LFWT+BASLFWT+STMWT+FLRWT+SKWT+RTWT)
      
      IF (ISWNIT .NE. 'N') THEN
        CALL Aloha_NUPTAK(CONTROL, ISWITCH, 
     &    ISTAGE, NO3, NH4, PDWI, PGRORT, PLIGRT,         !Input
     &    PLTPOP, PTF, RANC, RCNP, RLV, RTWT, SOILPROP,   !Input
     &    STOVWT, SW, TCNP, XSTAGE,                       !Input
     &    ROOTN, SENESCE, STOVN, TANC, UNH4, UNO3, WTNUP) !Output
      ENDIF

!-----------------------------------------------------------------
      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 2800 FORMAT (2X,'Crop failure growth program terminated ')

!=======================================================================
!     Integration
!-----------------------------------------------------------------------
      CASE (INTEGR)
!=======================================================================
!     This code used to be in PhaseI subroutine. Put here to make timing match 
!     with old code.
!     Some of the code was removed to other subroutines.
      IF (ISTAGE /= ISTAGE_old) THEN
        ISTAGE_OLD = ISTAGE

!       New stage initialization
        SELECT CASE (ISTAGE)
        CASE (1)      ! CASE (1)
          PLAG    = 0.0

          PLA     = PLA13
          LFWT    = LFWT13
          BASLFWT = BASLFWT13
          STMWT   = STMWT13


          RTWT    = 0.20                !Es el peso de la raíz         
          FLRWT   = 0.0
          FLRWT   = 0.0
          GROSTM  = 0.0                 
          SENLA   = 0.0                 
          SLAN    = 0.0                 
          GRORT   = 0.0                 
          GROBSL  = 0.0                 
          GROLF   = 0.0                 
          CUMPH   = 0.514               
          LN      = 1                   
          CUMDEP  = 0.0
          
         YRDOY   = CONTROL % YRDOY   ! Esto hala la fecha del día que se cumple la etapa primera hoja en emerger
         DAP2     = YRDOY-YRPLT      ! Estos son los días desde la siembra hasta que se cumple la etapa que hala de Phenol según su ubicación
         DAP3     = DAP2 - DAP1      !Resta los días pasados de la siembra a la etapa que hala de Phenol a los días que habían pasado de la etapa anterior
         GDDFR   = (SUMDTTGRO-DTT)/(DAP3)  !Estos es promedio de GDD que se acumulan desde la iniciación de la raíz hasta la emergencia de la primera hoja.
         GDDFR1  = GDDFR
         TMAXGRO = (SUMTMAXGRO-TMAX)/(DAP3)
         SRADGRO = (SUMSRADGRO-SRAD)/(DAP3)
         PARGRO  = (SUMPARGRO-PAR)/(DAP3)
        
         RLAE1    = (LOG(PLA)-LOG(PLA13))/((GDDFR1*TMAXGRO)-(0))
         PLA1     = PLA*EXP((RLAE1*((GDDFR1*TMAXGRO)-(0))))      
         
         RLDW1    = (LOG(LFWT)-LOG(LFWT13))/((GDDFR1*TMAXGRO)-(0)) !AHORA TENGO QUE DARLE UN NOMBRE A GDDFR1*TMAXGRO PARA COLOCARLO EN LUGAR DE TODO ESO
         LFWT1    = LFWT13*EXP(((RLDW1)*((GDDFR1*TMAXGRO)-(0))))
         
         RBWTDW1  = (LOG(BASLFWT)-LOG(BASLFWT13))/((GDDFR1*TMAXGRO)-(0))
         BASLFWT1 = BASLFWT13*EXP(((RBWTDW1)*((GDDFR1*TMAXGRO)-(0))))
         
         RSTMWT1  = (LOG(STMWT)-LOG(STMWT13))/((GDDFR1*TMAXGRO)-(0))
         STMWT1   = STMWT13*EXP(((RSTMWT1)*((GDDFR1*TMAXGRO)-(0))))
         
         
         LAI1    = PLTPOP*PLA1*0.0001

          LAI         = LAI1                     
          LFWT        = LFWT1                    
          BASLFWT     = BASLFWT1                 
          STMWT       = STMWT1                   
          STOVWT= STMWT
          BIOMAS   = DAP3!(LFWT + STMWT + BASLFWT)*PLTPOP                     
     
    
      CASE (2)       !CASE (2)
          GROSTM = 0.0  ! Daily stem growth (g/plant/day) 
          YRDOY   = CONTROL % YRDOY
          DAP4    = YRDOY-YRPLT
          DAP5    = DAP4-DAP3
          GDDFR   = SUMDTTGRO/DAP5  !cuando intente hacer esto en CASE (3) debo recordar que debo poner la variable SUMDTTGRO en Phenol Case(3)
          TMAXGRO = SUMTMAXGRO/DAP5
          SRADGRO = SUMSRADGRO/(DAP5)
          PARGRO  = SUMPARGRO/(DAP5)

   !   LAI=(PLA-SENLA)*PLTPOP*0.0001 *        !   y = -0.0946x2 + 1.1978x - 1.9054
   !  &    (-0.0946*GDDFR**2+1.1978*GDDFR-1.9054)   
   !   BIOMAS = WTINITIAL*PLTPOP * !Quiero quitar que esta formula de aqui en adelante considere WTINITIAL me parece incorrecto sino que considere PLA          
   ! &         (0.0034*GDDFR**2-0.0392*GDDFR+1.0369) ! CUANDO TENGA LOS PESOS SECOS DEBO AJUSTAR ESTA ECUACI�N
       BIOMAS = DAP5
       
        CASE (3)      !CASE (3)
!          IF (NFORCING .GE. 2) THEN
             ! Forcing by number of days after planting
             PLANTSIZE = TOTPLTWT

             !          ELSE    
!             PLANTSIZE = PLANTING % PLANTSIZE
!          ENDIF
          
          YRDOY   = CONTROL % YRDOY
          DAP6    = YRDOY-YRPLT
          DAP7    = DAP6-DAP4
          GDDFR   = SUMDTTGRO/DAP7  
          TMAXGRO = SUMTMAXGRO/DAP7
          SRADGRO = SUMSRADGRO/(DAP7)
          PARGRO  = SUMPARGRO/(DAP7)
          
          FBIOM  = BIOMAS               
          SUMP   = 0.0                  
          IDURP  = 0                    
          PLAMX  = PLA                  
          GROFLR = 0.0
          GROCRWN= 0.0
          GROFRT = 0.0
          FLRWT  = 0.0
          FRTWT  = 0.0
          CRWNWT = 0.0


!-------------------------------------------------------- NEW  JVJ  Case duplicated because 2 stages in vegetative phase were included 
         CASE (4)          !CASE (4)
!          IF (NFORCING .GE. 2) THEN
             ! Forcing by number of days after planting
             PLANTSIZE = TOTPLTWT
!          ELSE    
!             PLANTSIZE = PLANTING % PLANTSIZE
!          ENDIF
          
          YRDOY   = CONTROL % YRDOY
          DAP8    = YRDOY-YRPLT
          DAP9    = DAP8-DAP6
          GDDFR   = SUMDTTGRO/DAP9  
          TMAXGRO = SUMTMAXGRO/DAP9
          SRADGRO = SUMSRADGRO/(DAP9)
          PARGRO  = SUMPARGRO/(DAP9)
          
          
          FBIOM  = BIOMAS               
          SUMP   = 0.0                  
          IDURP  = 0                    
          PLAMX  = PLA                  
          GROFLR = 0.0
          GROCRWN= 0.0
          GROFRT = 0.0
          FLRWT  = 0.0
          FRTWT  = 0.0
          CRWNWT = 0.0
  
         CASE (5)      !CASE (5)                                                          !  JVJ  Case duplicated because 2 stages in vegetative phase were included 
!          IF (NFORCING .GE. 2) THEN
             ! Forcing by number of days after planting
             PLANTSIZE = TOTPLTWT
!          ELSE    
!             PLANTSIZE = PLANTING % PLANTSIZE
!          ENDIF
          
          YRDOY   = CONTROL % YRDOY
          DAP10    = YRDOY-YRPLT
          DAP11    = DAP10-DAP8
          GDDFR   = SUMDTTGRO/DAP11  
          TMAXGRO = SUMTMAXGRO/DAP11
          SRADGRO = SUMSRADGRO/(DAP11)
          PARGRO  = SUMPARGRO/(DAP11)
          
                  
          FBIOM  = BIOMAS               
          SUMP   = 0.0                  
          IDURP  = 0                    
          PLAMX  = PLA                  
          GROFLR = 0.0
          GROCRWN= 0.0
          GROFRT = 0.0
          FLRWT  = 0.0
          FRTWT  = 0.0
          CRWNWT = 0.0
            
          
          
!---------------------------------------------------------NEW END JVJ  Case duplicated because 2 stages in vegetative phase were included  
        CASE (6)        !CASE (6)                               !     CASE (4)
          
          YRDOY   = CONTROL % YRDOY
          DAP12    = YRDOY-YRPLT
          DAP13    = DAP12-DAP10
          GDDFR   = SUMDTTGRO/DAP13  
          TMAXGRO = SUMTMAXGRO/DAP13
          SRADGRO = SUMSRADGRO/(DAP13)
          PARGRO  = SUMPARGRO/(DAP13)

          
!         MaxLAI = LAI at the end of the stage
          MAXLAI      = LAI               
!         Above biomass per square meter (g/m^2)
C         ABIOMS      = BIOMAS            
!         Average photosysnthesis rate of fruit eye
          PHOTOSYNEYE = SUMP*1000./IDURP  

!         G2 is genetic coefficient for potential eye number
          GPP    = G2*(PHOTOSYNEYE/12000+0.43)*
     &             (0.7+0.3*PLANTSIZE/550.)
          GPP    = AMIN1 (GPP,G2)                
          GPP    = AMAX1 (GPP,0.0)

!         Move from Istage 4 because no actual fruits until stage 5
!         FRUITS = PLTPOP*(1.-0.10*PLTPOP/14.0)  ! number of fruits=PLTPOP/m2*FRUITING%

!CHP 10/14/2017          FLRWT  =  0.1*STMWT           ! FLRWT stands for the weight ofwhole inflorescence STMWT is stem weight.  Both are in gram/plant.
          SKWT   =  0.0
          GROSK  =  0.0
          PTF    =  1.0                 
          EYEWT  =  0.0                 
          VANC   = TANC                 
          VMNC   = TMNC                 

        CASE (7)       ! CASE (7)                  !   CASE (5)

          YRDOY   = CONTROL % YRDOY
          DAP14    = YRDOY-YRPLT
          DAP15    = DAP14-DAP12
          GDDFR   = SUMDTTGRO/DAP15  
          TMAXGRO = SUMTMAXGRO/DAP15
          SRADGRO = SUMSRADGRO/(DAP15)
          PARGRO  = SUMPARGRO/(DAP15)

          
!         Move from Istage 4 because no actual fruits until stage 5
          FRUITS = PLTPOP*(1.-0.10*PLTPOP/14.0)  
!         There will be some loss of mass when going from flower mass
!           to fruit + crown because FRUITS (#/m2) < PLTPOP (#/m2)

! FRTWT (g/plant) is fruit weight.  It is assumed to be 50% of inflorescence at begining of the stage
! CRWNWT (g/plant) is crown weight which is assumed to be 20% of inflorescence at the begining of the stage
!         FRTWT  = FLRWT*0.5            
!         CRWNWT = FLRWT*0.2            
! 10/14/2017 CHP 50% to fruit and 20% to crown causes 30% of flower mass to be lost. 
!     change ratios to add up to 1, maintaining approximately the same ratio.
          FRTWT  = FLRWT*0.7 
          CRWNWT = FLRWT*0.3 
          SWMAX  = 0.0
          SWMIN  = 0.0

        CASE (8)    !CASE (8)                    !   CASE (5)  JVJ  Case duplicated because 1 stage in reproductive phase were included 
          YRDOY   = CONTROL % YRDOY
          DAP16    = YRDOY-YRPLT
          DAP17    = DAP16-DAP14
          GDDFR   = SUMDTTGRO/DAP17  
          TMAXGRO = SUMTMAXGRO/DAP17
          SRADGRO = SUMSRADGRO/(DAP17)
          PARGRO  = SUMPARGRO/(DAP17)

          
!         Move from Istage 4 because no actual fruits until stage 5
          FRUITS = PLTPOP*(1.-0.10*PLTPOP/14.0)  
!         There will be some loss of mass when going from flower mass
!           to fruit + crown because FRUITS (#/m2) < PLTPOP (#/m2)

! FRTWT (g/plant) is fruit weight.  It is assumed to be 50% of inflorescence at begining of the stage
! CRWNWT (g/plant) is crown weight which is assumed to be 20% of inflorescence at the begining of the stage
!         FRTWT  = FLRWT*0.5            
!         CRWNWT = FLRWT*0.2            
! 10/14/2017 CHP 50% to fruit and 20% to crown causes 30% of flower mass to be lost. 
!     change ratios to add up to 1, maintaining approximately the same ratio.
          FRTWT  = FLRWT*0.7 
          CRWNWT = FLRWT*0.3 
          SWMAX  = 0.0
          SWMIN  = 0.0
          
        CASE (9)       !CASE (9)                            ! CASE (6)
          
          YRDOY   = CONTROL % YRDOY
          DAP18    = YRDOY-YRPLT
          DAP19    = DAP18-DAP16
          GDDFR   = SUMDTTGRO/DAP19  
          TMAXGRO = SUMTMAXGRO/DAP19
          SRADGRO = SUMSRADGRO/(DAP19)
          PARGRO = SUMPARGRO/(DAP19)
         
          
          YIELD = FRTWT*10.0*FRUITS        
!         IF (PLTPOP .GE. 0.0) THEN
!            IF (GPP .GT. 0.0) THEN
!               EYEWT = FRTWT/GPP
!            ENDIF
!            PEYEWT = EYEWT*1000.0            
!            GPSM   = GPP*FRUITS              
!            STOVER = BIOMAS*10.0-YIELD       
!            YIELD  = YIELD / Species % FDMC  
!            YIELDB = YIELD/0.8914            
            STGDOY (ISTAGE) = YRDOY
!         ENDIF
!         HBIOM  = BIOMAS                 ! Record biomass at fruit harvest date


       CASE (10)       !CASE (9)                            ! CASE (6)
          
          YRDOY   = CONTROL % YRDOY
          DAP18    = YRDOY-YRPLT
          DAP19    = DAP18-DAP16
          GDDFR   = SUMDTTGRO/DAP19  
          TMAXGRO = SUMTMAXGRO/DAP19
          SRADGRO = SUMSRADGRO/(DAP19)
          PARGRO  = SUMPARGRO/(DAP19) 
         
          
          YIELD = FRTWT*10.0*FRUITS        
!         IF (PLTPOP .GE. 0.0) THEN
!            IF (GPP .GT. 0.0) THEN
!               EYEWT = FRTWT/GPP
!            ENDIF
!            PEYEWT = EYEWT*1000.0            
!            GPSM   = GPP*FRUITS              
!            STOVER = BIOMAS*10.0-YIELD       
!            YIELD  = YIELD / Species % FDMC  
!            YIELDB = YIELD/0.8914            
            STGDOY (ISTAGE) = YRDOY
!         ENDIF
!         HBIOM  = BIOMAS                 ! Record biomass at fruit harvest date

        CASE (11) 
        YRDOY   = CONTROL % YRDOY
        YRPLT   = YRDOY
        STGDOY (ISTAGE) = YRDOY

        CASE (12)          !CASE (11)                  !  CASE (8)
          WTINITIAL = SDWTPL/(PLTPOP*10.0)        ! kg/ha  --> g/plt

          PLA        = WTINITIAL*41.1048     ! 41.1048 es un promedio (desvest= 3.09) de la relación porcentual entre el peso seco de la semilla Sucker al momento de la siembra en tres momentos diferentes del año y el area del tejido verde de la hoja.
          PLA12      = PLA
          LAI        = PLTPOP*PLA*0.0001     !         
          BIOMAS     = WTINITIAL*PLTPOP      !
          LFWT       = WTINITIAL*0.6909      ! 0.6909 es un promedio (desvest= 0.03) de la relación porcentual entre el peso seco de la semilla Sucker al momento de la siembra en tres momentos diferentes del año y el peso seco del tejido verde de la hoja.
          LFWT12     =LFWT 
          BASLFWT    = LFWT*0.1927           ! 0.1927 es un promedio (desvest= 0.04) de la relación porcentual entre el peso seco de la semilla Sucker al momento de la siembra en tres momentos diferentes y el peso seco del tejido blanco basal.
          BASLFWT12  = BASLFWT 
          STMWT      = WTINITIAL*0.1164      ! 0.1122 es un promedio (desvest= 0.01) de la relación porcentual entre el peso seco de la semilla Sucker al momento de la siembra en tres momentos diferentes y el peso seco del tallo.
          STMWT12    = STMWT
          STOVWT     = WTINITIAL             ! 
          YRDOY   = CONTROL % YRDOY
          YRPLT   = YRDOY
          
         
      
      CASE (13)        !CASE (12)                   
          YRDOY   = CONTROL % YRDOY   ! Esto hala la fecha del dia que se cumple la etapa iniciacion de la raiz
          DAP1     = YRDOY-YRPLT      ! Estos son los dias desde la siembra hasta que se cumple la etapa iniciacion de la raiz
          GDDFR   = SUMDTTGRO/(DAP1)  ! Esto es el promedio de GDD que se acumulan desde la siembra hasta que Phenol detona el evento iniciacion de la raiz.
          GDDFR13 = GDDFR
          TMAXGRO = SUMTMAXGRO/(DAP1)  ! Este esta ok asi porque el dia de la siembra no acumula GDD pero en la etapa siguiente se le debe restar el dia presente de lo contrario la temperatura maxima promedio es mayor a lo correcto
          SRADGRO = SUMSRADGRO/(DAP1)  ! Este esta ok asi porque el dia de la siembra no acumula GDD pero en la etapa siguiente se le debe restar el dia presente de lo contrario la radiacion promedio es mayor a lo correcto
          PARGRO = SUMPARGRO/(DAP1)    ! Este esta ok asi porque el dia de la siembra no acumula GDD pero en la etapa siguiente se le debe restar el dia presente de lo contrario PAR promedio es mayor a lo correcto
      
         ! Mi idea es crear una regresión para cada etapa de los 
         ! valores de tasa relativa de expansion y crecimiento de 
         ! cada tejido para colocarla en el exponente "e" y en lugar
         ! del promedio de unidade de crecimeinto usar el total de
         ! unidades de crecimeinto por etapa multiplicado por TMAX
         ! como la forma de crear la separacion entre las zonas

         RLAE13    = 0.0019 * (LOG(GDDFR13/TMAXGRO)) + 0.0018               !r2= 0.8994  Listo variable calibrada en CASE (13)
         PLA13     = PLA*EXP((RLAE13*((GDDFR13*TMAXGRO)-0)))                  
         
         RLDW13    = 3.79 * (LOG(GDDFR13/TMAXGRO)) + 3.3437                 !r2= 0.9540 Listo variable calibrada en CASE (13) 
         LFWT13    = LFWT12*EXP(((RLDW13/1000)*((GDDFR13*TMAXGRO)-0)))        
         
         RBWTDW13   = -3.89 * (LOG(GDDFR13/TMAXGRO)) - 3.0861               !r2= 0.9008 Listo variable calibrada en CASE (13)
         BASLFWT13 = BASLFWT12*EXP(((RBWTDW13/1000)*((GDDFR13*TMAXGRO)-0)))
         
         RSTMWT13  = 1.3531 * (LOG(GDDFR13/TMAXGRO)) + 1.6061                !r2= 0.9306 Listo variable calibrada en CASE (13)
         STMWT13   = STMWT12*EXP(((RSTMWT13/1000)*((GDDFR13*TMAXGRO)-0)))
         
         
         LAI13    = PLTPOP*PLA13*0.0001

          LAI         = LAI13                     
          LFWT        = LFWT13                    
          BASLFWT     = BASLFWT13                 
          STMWT       = STMWT13                   
          STOVWT= STMWT
          BIOMAS= DAP1!(LFWT + STMWT + BASLFWT)*PLTPOP                     
     
          
!          NSTRES     = 1.0

          IF (ISWNIT .NE. 'N') THEN
             IF (FRTWT .GT. 0.0) THEN
                XGNP = (GRAINN/FRTWT)*100.0
C               XPTN = XGNP*6.25
                GNUP = GRAINN*FRUITS*10.0
             ENDIF
             TOTNUP = GNUP + APTNUP
          ENDIF

!        CASE (9)
!          NDEF3  =  1.0
!          NSTRES =  1.0
!          AGEFAC =  1.0

        END SELECT
      ENDIF

      CALL Aloha_NUPTAK (CONTROL, ISWITCH, 
     &    ISTAGE, NO3, NH4, PDWI, PGRORT, PLIGRT,         !Input
     &    PLTPOP, PTF, RANC, RCNP, RLV, RTWT, SOILPROP,   !Input
     &    STOVWT, SW, TCNP, XSTAGE,                       !Input
     &    ROOTN, SENESCE, STOVN, TANC, UNH4, UNO3, WTNUP) !Output

!=======================================================================
      END SELECT
!=======================================================================
      RETURN
      END SUBROUTINE Aloha_GROSUB

! PLAG (cm^2) is daily green leaf area growth    ! O sea debo modificar PLAG para hacer que aumentar el area foliar 
! leaf area index (m2 leaf/m2 ground)
! LFWT (g/plant) is green leaf weight which is assumed to be 53% of initial crown weight
! RTWT (g/plant) is root weight
! STMWT is 115% of initial crown weight
! Basal white leaf weight is 66% of green leaf weight
! Inflorescence weight is set to 0.0
! STOVWT (g/plant) is stover weight
! GROSTM (g/plant/day) is daily stem growth
! SENLA (cm2/plant) is area of leaf senesces due to stress on a given day
! SLAN (cm2/plant) is total normal leaf senescence since emergence.
! GRORT (g/plant/day) is daily root growth
! GROBSL (g/plant/day) is daily basal leaf growth
! GROLF (g/plant/day) is daily green leaf growth
! CUMPH (leaves/plant) is number of leaves emerged
! LN (leaves/plant) is leaf number
!PEYEWT =  Eye weight (mg/eye)
!GPSM   =  Number of eyes per square meter
!STOVER =  Total plant weight except fruit
!YIELD  =  Dry fruit yield (kg/ha)
!YIELDB =  Fresh fruit yield (lb/acre)
!LAI     = leaf area index (m2 leaf/m2 ground)
 
!LFWT    = LFWT (g/plant) is green leaf weight which is assumed to be 53% of initial crown weight
!BASLFWT = Basal white leaf weight is 66% of green leaf weight
!STMWT   = STMWT is 115% of initial crown weight
!STOVWT  = STOVWT (g/plant) is stover weight
!FBIOM  =  Record biomass at forcing
!SUMP   =  SUMP is the total weight of biomass cumulated in Istage 4.
!IDURP  =  Duration of stage 3 (days)
!PLAMX  =  PLAMX (cm2/plant) is maximal green leaf area.  PLA is total green leaf area.
! PTF is plant top fraction in gram/plant.
! EYEWT (G/eye) is weight of the eye
!FRUITS =  number of fruits=PLTPOP/m2*FRUITING%
!YIELD = fruit dry weight yield (kg/ha)
! TURFAC      !Soil water stress effect on expansion (0-1), 1 is no stress, 0 is full stress
! PC          !Used to compute fraction of phyllochron interval occurring today      


