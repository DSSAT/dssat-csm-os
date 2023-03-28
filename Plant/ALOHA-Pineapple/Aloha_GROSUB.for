!=======================================================================
!  Aloha_GROSUB, Subroutine
!
!  Maize growth routine
!-----------------------------------------------------------------------
!  Revision history
!  02/07/1993 PWW Header revision and minor changes   
!  02/07/1993 PWW Switch block added, etc
!  10/17/2017 CHP Adpated for CSM v4.7
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
     &    DTT, ISTAGE, NH4, NO3, SOILPROP, SW, SWFAC,         !Input
     &    SUMDTT, TBASE, TURFAC, WEATHER, XSTAGE,             !Input
     &    AGEFAC, BASLFWT, BIOMAS, CRWNWT, EYEWT, FBIOM,      !Output
     &    FLRWT, FRTWT, FRUITS, GPP, GPSM, GRAINN, GRORT,     !Output
     &    LAI, LFWT, LN, NSTRES, RLV, ROOTN, RTWT,            !Output
     &    SKWT, STMWT, STOVN, STOVWT,  TEMPM,                 !Output
     &    UNH4, UNO3, WTNUP, WTINITIAL, XGNP, YIELD)          !Output

      USE Aloha_mod
      USE Interface_SenLig_Ceres
      IMPLICIT  NONE
      EXTERNAL Aloha_NFACTO, Aloha_NUPTAK, TABEX, WARNING
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
      INTEGER STGDOY(20)
      INTEGER DYNAMIC
      REAL    PLA, LAI, BIOMAS, LFWT, BASLFWT, STMWT, STOVWT, WTINITIAL
      REAL    PLAG, RTWT, FLRWT, GROSTM, SENLA, SLAN, GRORT
      REAL    GROBSL, GROLF, CUMPH, LN, CUMDEP, SUMP, PLAMX, GROFLR
      REAL    GROCRWN, GROFRT, FRTWT, CRWNWT, SKWT, GROSK, PTF, EYEWT
      REAL    SWMAX, SWMIN, NDEF3, NSTRES, AGEFAC, LIFAC
      REAL    PAR, CC, TRF2, CARBO, SWFAC, TEMPM  !,TRNU
      REAL    DTT, TURFAC, XN, CMF, TOTPLTWT, SUMDTT, GPP
      REAL    PDWI, PGRORT, DM, FBIOM, MAXLAI, PHOTOSYNEYE, FRUITS
      REAL    YIELD, GPSM, XSTAGE  !, FDMC

      REAL    CO2, SRAD, TMIN, TMAX
      REAL    PLTPOP, SDWTPL, PLANTSIZE
      REAL    G2, G3, P4, PHINT, TBASE
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
!      TYPE (ResidueType) SENESCE

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
      LAI        = 0.0
      BIOMAS     = 0.0
      LFWT       = 0.0
      BASLFWT    = 0.0
      STMWT      = 0.0
      STOVWT     = 0.0
      SWFAC      = 1.0
      TURFAC     = 1.0
      LN     = 0.0

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
     &    ISTAGE, NO3, NH4, PDWI, PGRORT,                 !Input
     &    PLTPOP, PTF, RANC, RCNP, RLV, RTWT, SOILPROP,   !Input
     &    STOVWT, SW, TCNP, XSTAGE,                       !Input
     &    ROOTN, STOVN, TANC, UNH4, UNO3, WTNUP)          !Output

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
      CC   = SPECIES % CONV
      CMF  = Species % CMFC
      LIFAC= Species % LIFAC

      G2  = CULTIVAR % G2
      G3  = CULTIVAR % G3
      P4  = CULTIVAR % P4
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
     &    ISTAGE, NO3, NH4, PDWI, PGRORT,                 !Input
     &    PLTPOP, PTF, RANC, RCNP, RLV, RTWT, SOILPROP,   !Input
     &    STOVWT, SW, TCNP, XSTAGE,                       !Input
     &    ROOTN, STOVN, TANC, UNH4, UNO3, WTNUP)          !Output

!=======================================================================
      CASE (RATE)
!=======================================================================
      TEMPM = (WEATHER % TMAX + WEATHER % TMIN) / 2.

      IF (ISWNIT .NE. 'N') THEN
!       Top actual N concentration (g N/g Dry weight)
        XANC   = TANC*100.0               
        APTNUP = STOVN*10.0*PLTPOP

        IF (ISTAGE .LT. 7) THEN
          CALL Aloha_NFACTO (DYNAMIC, 
     &      ISTAGE, TANC, XSTAGE,                           !Input
     &      AGEFAC, NDEF3, NFAC, NSTRES, RCNP, TCNP, TMNC)  !Output
        ENDIF
      ENDIF

      IF (ISTAGE .GT. 5) RETURN

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
        CASE (1,2,3,7,8,9)
          IF (TEMPM .LE. 25.0) THEN
             PRFT = 1.0-0.001*(TEMPM-25.0)**2
           ELSEIF (TEMPM .LT. 29.0) THEN
             PRFT = 1.0-0.056*(TEMPM-25.0)**2
           ELSE
             PRFT = 0.1
          ENDIF
        CASE (4,5,6)
          PRFT = 1.0-0.005*((0.4*TMIN+0.6*TMAX)-26.)**2
          PRFT = AMAX1 (PRFT,0.0)
      END SELECT
!-----------------------------------------------------------------
      !
      ! Temperature factor
      !
      IF (TEMPM .LT. 15.0) THEN
         TRF2 = 0.45
       ELSEIF (TEMPM .GE. 15.0 .AND. TEMPM .LT. 30.0) THEN
         TRF2 = 0.082*EXP(0.1*TEMPM)
       ELSE
         TRF2 = 1.65
      ENDIF

      IF (ISTAGE .GE. 4 .AND. ISTAGE .LT. 7) THEN
         CARBO = PCARB*AMIN1(PRFT,0.55+0.45*SWFAC,NSTRES)
       ELSE
         CARBO = PCARB*AMIN1(PRFT,SWFAC,NSTRES)
      ENDIF
      DTT = AMAX1 (DTT,0.0)

!-----------------------------------------------------------------
      IF (ISTAGE .LE. 3) THEN
!         
!        Calculate leaf emergence
!        The first 5 leaves grow faster than other leaves, used for maize
!         
         PC = 1.0                       ! Calculate leaf emergence
         IF (CUMPH .LE. 15.0) THEN
            PC = 1.25 - 0.25/15.0*CUMPH
         ENDIF
!         
!        TI is the fraction of leaf emerged for a day.  It is calculated from
!        the following equations
!        
!        Correcting water stress effect and effect due to shading.
!        
         IF (ISTAGE .EQ. 3) THEN
            IF (TEMPM .GT. TBASE) THEN
               IF ((LN*PLTPOP) .GT. 550.0) THEN
                  TI = TURFAC*(1.0/PHINT)*0.5*(DTT-2.0)/PC
                ELSEIF ((LN*PLTPOP) .GE. 50.0) THEN
                  TI = TURFAC*(1.0/PHINT)*(-0.001*LN*PLTPOP+1.05)*
     &                 (DTT-2.0)/PC
                ELSE
                  TI = TURFAC*(1.0/PHINT)*(DTT-2.0)/PC
               ENDIF
             ELSE
               TI = 0.0   ! If mean air temperature is less than Tbase,
            ENDIF         ! no leaf emerges
          ELSE
            IF (TEMPM .GT. TBASE) THEN
               IF ((LN*PLTPOP) .GT. 550.0) THEN
                  TI = TURFAC*(1.0/PHINT)*0.5*DTT/PC
                ELSEIF ((LN*PLTPOP).GE.50.0) THEN
                  TI = TURFAC*(1.0/PHINT)*(-0.001*LN*PLTPOP+1.05)*
     &                 DTT/PC
                ELSE
                  TI = TURFAC*(1.0/PHINT)*DTT/PC
               ENDIF
             ELSE
!              If mean air temperature is less than Tbase, no leaf emerges
               TI = 0.0     
            ENDIF
         ENDIF

!        CUMPH is number of expanded leaves. It is updated daily
         CUMPH = CUMPH + TI 
!        XN is leaf number of the oldest expanding leaf
         XN    = CUMPH +  1 
         LN    = XN         ! LN is leaf number
      ENDIF

!-----------------------------------------------------------------
!  ISTAGE Definition
!     7 - Preplanting
!     8 - Planting to root initiation
!     9 - Root initiation to first new leaf emergence
!     1 - First new leaf emergence to net zero root growth
!     2 - Net zero stem growth to forcing
!     3 - Forcing to sepals closed on youngest flowers
!     4 - SCY to first open flower
!     5 - Fruit growth
!     6 - Physiological maturity
!-----------------------------------------------------------------
      SELECT CASE (ISTAGE)
!-----------------------------------------------------------------
      CASE (1)
        !
        ! First new leaf emergence to net zero root growth
        !
        SELECT CASE (PMTYPE)
          CASE (0)
            IF ((LN*PLTPOP) .GT. 550.0) THEN
               PLAG = CMF*1.75*(33.0+7.5*XN)*0.5*TI*TURFAC
             ELSEIF ((LN*PLTPOP) .GE. 50.0) THEN
               PLAG = CMF*1.75*(33.0+7.5*XN)*TI*TURFAC
     1                *(-0.001*LN*PLTPOP+1.05)
             ELSE
               PLAG = CMF*1.75*(33.0+7.5*XN)*TI*TURFAC
           ENDIF
          CASE (1)
            IF ((LN*PLTPOP) .GT. 550.0) THEN
               PLAG = CMF*0.77*(174.0+16.0*XN)*0.5*TI*TURFAC
             ELSEIF ((LN*PLTPOP) .GE. 50.0) THEN
               PLAG = CMF*0.77*(174.0+16.0*XN)*TI*TURFAC
     1                *(-0.001*LN*PLTPOP+1.05)
             ELSE
               PLAG = CMF*0.77*(174.0+16.0*XN)*TI*TURFAC
            ENDIF
        END SELECT
!        
!       Green leaf weight is calculated from leaf area
!        
        GROLF  = PLAG*(1.0/((85.0*EXP(-XN*0.012))*TRF2))
        GROBSL = 0.42*GROLF
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
        LFWT = LFWT-SLAN/600.0    !recalculate green leaf weight

!-----------------------------------------------------------------
      CASE (2)
        !
        ! Net zero stem growth to forcing
        !
        SELECT CASE (PMTYPE)
          CASE (0)
            IF ((LN*PLTPOP) .GT. 550.0) THEN
               PLAG = CMF*1.75*(33.0+7.5*XN)*0.5*TI*TURFAC
             ELSEIF ((LN*PLTPOP) .GE. 50.0) THEN
               PLAG = CMF*1.75*(33.0+7.5*XN)*TI*TURFAC
     1               *(-0.001*LN*PLTPOP+1.05)
             ELSE
               PLAG = CMF*1.75*(33.0+7.5*XN)*TI*TURFAC
           ENDIF
          CASE (1)
            IF ((LN*PLTPOP) .GT. 550.0) THEN
               PLAG = CMF*0.77*(174.0+16.0*XN)*0.5*TI*TURFAC
             ELSEIF ((LN*PLTPOP) .GE. 50.0) THEN
               PLAG = CMF*0.77*(174.0+16.0*XN)*TI*TURFAC
     1               *(-0.001*LN*PLTPOP+1.05)
             ELSE
               PLAG = CMF*0.77*(174.0+16.0*XN)*TI*TURFAC
            ENDIF
        END SELECT

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
      CASE (3)
        !
        ! Forcing to sepals closed on youngest flowers
        !
        SELECT CASE (PMTYPE)
          CASE (0)
            IF ((LN*PLTPOP) .GT. 550.0) THEN
               PLAG = CMF*2.*(33.0+7.5*XN)*0.5*TI*TURFAC
             ELSEIF ((LN*PLTPOP) .GE. 50.0) THEN
               PLAG = CMF*2.*(33.0+7.5*XN)*TI*TURFAC
     1               *(-0.001*LN*PLTPOP+1.05)
             ELSE
               PLAG = CMF*2.*(33.0+7.5*XN)*TI*TURFAC
            ENDIF
          CASE (1)
            IF ((LN*PLTPOP) .GT. 550.0) THEN
               PLAG = CMF*2.5*(174.0+16.0*XN)*0.5*TI*TURFAC
             ELSEIF ((LN*PLTPOP) .GE. 50.0) THEN
               PLAG = CMF*2.5*(174.0+16.0*XN)*TI*TURFAC
     1               *(-0.001*LN*PLTPOP+1.05)
             ELSE
               PLAG = CMF*2.5*(174.0+16.0*XN)*TI*TURFAC
            ENDIF
        END SELECT

        GROLF  = PLAG*(1.0/((85*EXP(-XN*0.012))*TRF2))
        GROBSL = 0.42 * GROLF
        GRORT  = 0.05 * GROLF
        GRORT  = AMAX1 (GRORT,0.0)
        GROFLR = (1.26-0.17*PLTPOP+0.0075*PLTPOP**2)*DTT/20.5
     1           *AMIN1(AGEFAC,TURFAC)
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
      CASE (4)
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
      CASE (5)
        !
        ! Fruit growth
        !

        IF (SWMAX .LE. 0.0) THEN
           IF (XSTAGE .GE. 10.0) THEN
              SWMAX = STMWT
              SWMIN = 0.65*SWMAX
           ENDIF
        ENDIF

        IF (SUMDTT .GT. P4) THEN
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
           IF (SUMDTT .GT. 0.5*P4) THEN
              IF (SRAD .LT. 12.0) THEN
                 GROFRT = GPP*G3*0.001*(0.7+0.3*SWFAC)*(SRAD/12.)
               ELSEIF (SRAD .LT. 36.0) THEN
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
          CASE (1:10)
            IF (SUMDTT .LT. 0.8*P4) THEN
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
        IF (SUMDTT .GT. 0.8*P4) THEN
           STMWT = AMIN1 (STMWT,SWMAX)
        ENDIF

!-----------------------------------------------------------------
      CASE (6)
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
      IF (ISTAGE .GT. 2 .AND. ISTAGE .LT. 7) THEN
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
      LAI   = (PLA-SENLA)*PLTPOP*0.0001

      IF (LN .GT. 3 .AND .LAI .LE. 0.0 .AND. ISTAGE .LE. 3) THEN
         WRITE (MSG(1),   2800)
         CALL WARNING(1, ERRKEY, MSG)
!         IF (IDETO .EQ. 'Y') THEN
!            WRITE (NOUTDO,2800)
!         ENDIF
         ISTAGE = 4
       ELSE
         IF (ICOLD .GE. 7) THEN
            WRITE (MSG(1),   2800)
            CALL WARNING(1, ERRKEY, MSG)
!            IF (IDETO .EQ. 'Y') THEN
!               WRITE (NOUTDO,2800)
!            ENDIF
           ISTAGE = 5
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
      CASE(5,6)
!       In this case FLRWT is fruit + crown
        BIOMAS   = (LFWT + STMWT + BASLFWT + SKWT)*PLTPOP 
     &                + (FLRWT * FRUITS) 
      CASE DEFAULT
        BIOMAS   = (LFWT + STMWT + FLRWT + BASLFWT + SKWT)*PLTPOP
      END SELECT 

      TOTPLTWT =  LFWT + STMWT + FLRWT + BASLFWT + SKWT
      DM       = BIOMAS*10.0
      STOVWT   = LFWT + STMWT
      PTF      = (LFWT+BASLFWT+STMWT+FLRWT+SKWT) /
     &           (LFWT+BASLFWT+STMWT+FLRWT+SKWT+RTWT)
      
      IF (ISWNIT .NE. 'N') THEN
        CALL Aloha_NUPTAK(CONTROL, ISWITCH, 
     &    ISTAGE, NO3, NH4, PDWI, PGRORT,                 !Input
     &    PLTPOP, PTF, RANC, RCNP, RLV, RTWT, SOILPROP,   !Input
     &    STOVWT, SW, TCNP, XSTAGE,                       !Input
     &    ROOTN, STOVN, TANC, UNH4, UNO3, WTNUP)          !Output
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
        CASE (1)
          PLAG    = 0.0                 
!          LAI     = PLTPOP*PLA*0.0001  
!          LFWT    = WTINITIAL*0.53     
          RTWT    = 0.20                
!          STMWT   = WTINITIAL*0.115    
!          BASLFWT = LFWT*0.66          
          FLRWT   = 0.0                 
!          STOVWT  = WTINITIAL          
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

        CASE (2)
          GROSTM = 0.0  ! Daily stem growth (g/plant/day)

        CASE (3)
!          IF (NFORCING .GE. 2) THEN
             ! Forcing by number of days after planting
             PLANTSIZE = TOTPLTWT
!          ELSE    
!             PLANTSIZE = PLANTING % PLANTSIZE
!          ENDIF
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

        CASE (4)
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

        CASE (5)

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

        CASE (6)
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

        CASE (8)
          WTINITIAL = SDWTPL/(PLTPOP*10.0)        ! kg/ha  --> g/plt

          PLA        = WTINITIAL*0.6*63.0  
          LAI        = PLTPOP*PLA*0.0001   
          BIOMAS     = WTINITIAL*PLTPOP    
          LFWT       = WTINITIAL*0.53      
          BASLFWT    = LFWT*0.66           
          STMWT      = WTINITIAL*0.115     
          STOVWT     = WTINITIAL              

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
     &    ISTAGE, NO3, NH4, PDWI, PGRORT,                 !Input
     &    PLTPOP, PTF, RANC, RCNP, RLV, RTWT, SOILPROP,   !Input
     &    STOVWT, SW, TCNP, XSTAGE,                       !Input
     &    ROOTN, STOVN, TANC, UNH4, UNO3, WTNUP)          !Output

!=======================================================================
      END SELECT
!=======================================================================
      RETURN
      END SUBROUTINE Aloha_GROSUB

! PLAG (cm^2) is daily green leaf area growth
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



