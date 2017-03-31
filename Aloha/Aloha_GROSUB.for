C=======================================================================
C  Aloha_GROSUB, Subroutine
C
C  Maize growth routine
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2. Header revision and minor changes           P.W.W.      2-7-93
C  3. Switch block added, etc                     P.W.W.      2-7-93
C  4. Updated PCARB calculation          J.T.R. & B.D.B. 21-Jun-1994
C-----------------------------------------------------------------------
C  INPUT  : NOUTDO,ISWNIT
C
C  LOCAL  : NSINK,NPOOL1,NPOOL2,NPOOL,NSDR,I,ICOLD,PCARB,PRFT,PC,
C           GRF,GROEAR,RGFILL,TTMP,GROGRN,SFAC,TFAC,RMNC,XNF,TNLAB,
C           RNLAB,RNOUT,SLFW,SLFN,SLFC,SLFT,PLAS,TI
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : PINE
C
C  Calls  : NFACTO NUPTAK
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  GRF    :
C  GROGRN : Daily growth of the grain - g
C  I      : Loop counter
C  ICOLD  :
C  NOUTDO : File handle
C  NPOOL  : Total plant N available for translocation to grain (g/plant)
C  NPOOL1 : Tops N available for translocation to grain (g/plant)
C  NPOOL2 : Root N available for translocation to grain (g/plant)
C  NSDR   : Plant N supply/demand ratio used to modify grain N content
C  NSINK  : Demand for N associated with grain filling (g/plant/day)
C  PAR    : Daily photosynthetically active radiation, calculated as half
C           the solar radiation - MJ/square metre
C  PC     :
C  PCARB  : Daily amount of carbon fixed - g
C  PLAS   : The rate of senescence of leaf area on one plant - sq. cm/day
C  PRFT   : Photosynthetic reduction factor for low and high temperatures
C  RGFILL : Rate of grain fill - mg/day
C  RMNC   : Root minimum nitrogen concentration (g N/g root dry weight)
C  TI     : Fraction of a phyllochron interval which occurred as a fraction
C           of today's daily thermal time
C  TNLAB  :
C  TTMP   :
C=======================================================================

      SUBROUTINE Aloha_GROSUB (NOUTDO,ISWNIT,IDETO)

      IMPLICIT  NONE

      INCLUDE  'GEN1.BLK'
      INCLUDE  'GEN2.BLK'
      INCLUDE  'GEN3.BLK'
      INCLUDE  'GEN4.BLK'
      INCLUDE  'NTRC1.BLK'

      CHARACTER ISWNIT*1,IDETO*1
      INTEGER   NOUTDO,ICOLD
      REAL      PCARB,PRFT,PC,TI,GRF,RGFILL,GROGRN,SFAC,TFAC,RMNC,XNF,
     &          TNLAB,RNLAB,RNOUT,SLFW,SLFN,SLFC,SLFT,PLAS
      REAL      NSINK,NPOOL1,NPOOL2,NPOOL,NSDR
      REAL      TABEX,PCO2,Y1
      SAVE

!=======================================================================
      SELECT CASE (DYNAMIC)
!=======================================================================
      CASE (RUNINIT)
!=======================================================================
      PLA        = 0.0
      LAI        = 0.0
      BIOMAS     = 0.0
      LFWT       = 0.0
      BASLFWT    = 0.0
      STMWT      = 0.0
      STOVWT     = 0.0

!=======================================================================
      CASE (SEASINIT)
!=======================================================================
      PLA        = WTINITIAL*0.6*63.0
      LAI        = PLTPOP*PLA*0.0001
      BIOMAS     = WTINITIAL*PLTPOP
      LFWT       = WTINITIAL*0.53
      BASLFWT    = LFWT*0.66
      STMWT      = WTINITIAL*0.115
      STOVWT     = WTINITIAL

      ! Calculate initial SEED N
      !
      SEEDNI = (ROOTN+STOVN+GRAINN+SEEDN)*PLTPOP


!=======================================================================
      CASE (RATE)
!=======================================================================
      XANC   = TANC*100.0               ! Top actual N concentration (g N/g Dry weight)
      APTNUP = STOVN*10.0*PLTPOP

      IF (ISWNIT .NE. 'N' .AND. ISTAGE .LT. 7) THEN
         CALL NFACTO
      ENDIF

!-----------------------------------------------------------------
!     From PHENOL and PHASEI
      SELECT CASE (ISTAGE)
        CASE (8)
          PLA    = WTINITIAL*0.6*63.0
          LAI    = PLA*PLTPOP*0.0001
          BIOMAS = WTINITIAL*PLTPOP

        CASE (6)

        CASE (9)
          NDEF3  =  1.0
          NSTRES =  1.0
          AGEFAC =  1.0

        CASE (1)
          PLAG    = 0.0                 ! PLAG (cm^2) is daily green leaf area growth
          LAI     = PLTPOP*PLA*0.0001   ! leaf area index (m2 leaf/m2 ground)
          LFWT    = WTINITIAL*0.53      ! LFWT (g/plant) is green leaf weight which is assumed to be 53% of initial crown weight
          RTWT    = 0.20                ! RTWT (g/plant) is root weight
          STMWT   = WTINITIAL*0.115     ! STMWT is 115% of initial crown weight
          BASLFWT = LFWT*0.66           ! Basal white leaf weight is 66% of green leaf weight
          FLRWT   = 0.0                 ! Inflorescence weight is set to 0.0
          STOVWT  = WTINITIAL           ! STOVWT (g/plant) is stover weight
          FLRWT   = 0.0
          GROSTM  = 0.0                 ! GROSTM (g/plant/day) is daily stem growth
          SENLA   = 0.0                 ! SENLA (cm2/plant) is area of leaf senesces due to stress on a given day
          SLAN    = 0.0                 ! SLAN (cm2/plant) is total normal leaf senescence since emergence.
          GRORT   = 0.0                 ! GRORT (g/plant/day) is daily root growth
          GROBSL  = 0.0                 ! GROBSL (g/plant/day) is daily basal leaf growth
          GROLF   = 0.0                 ! GROLF (g/plant/day) is daily green leaf growth
          CUMPH   = 0.514               ! CUMPH (leaves/plant) is number of leaves emerged
          LN      = 1                   ! LN (leaves/plant) is leaf number
          CUMDEP  = 0.0                 ! variable used in CERES-MAIZE

        CASE (2)
          GROSTM = 0.0                   ! Daily stem growth (g/plant/day).

        CASE (3)
          SUMP   = 0.0                  ! SUMP is the total weight of biomass cumulated in Istage 4.
          IDURP  = 0                    ! Duration of stage 3 (days)
          PLAMX  = PLA                  ! PLAMX (cm2/plant) is maximal green leaf area.  PLA is total green leaf area.
          GROFLR = 0.0
          GROCRWN= 0.0
          GROFRT = 0.0
          FLRWT  = 0.0
          FRTWT  = 0.0
          CRWNWT = 0.0

        CASE (4)
          FLRWT  =  0.1*STMWT           ! FLRWT stands for the weight ofwhole inflorescence STMWT is stem weight.  Both are in gram/plant.
          SKWT   =  0.0
          GROSK  =  0.0
          PTF    =  1.0                 ! PTF is plant top fraction in gram/plant.
          EYEWT  =  0.0                 ! EYEWT (G/eye) is weight of the eye
          VANC   = TANC                 ! Variable used in nitrogen balance
          VMNC   = TMNC                 ! .....


      END SELECT
!-----------------------------------------------------------------
      IF (ISWNIT .NE. 'N') THEN
         RANC  = 0.022
         TANC  = 0.044
         ROOTN = RANC*RTWT
         STOVN = STOVWT*TANC
      ENDIF

      PAR   = 0.5*SRAD
!     Y1    = EXP(-0.52*LAI)                        ! Beer's law
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
         ! Calculate leaf emergence
         ! The first 5 leaves grow faster than other leaves, used for maize
         !
         PC = 1.0                       ! Calculate leaf emergence
         IF (CUMPH .LE. 15.0) THEN
            PC = 1.25 - 0.25/15.0*CUMPH
         ENDIF
         !
         ! TI is the fraction of leaf emerged for a day.  It is calculated from
         ! the following equations
         !
         ! Correcting water stress effect and effect due to shading.
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
               TI = 0.0        ! If mean air temperature is less than Tbase,
            ENDIF              ! no leaf emerges
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
               TI = 0.0                 ! If mean air temperature is less than Tbase, no leaf emerges
            ENDIF
         ENDIF

         CUMPH = CUMPH + TI             ! CUMPH is number of expanded leaves. It is updated daily
         XN    = CUMPH +  1             ! XN is leaf number of the oldest expanding leaf
         LN    = XN                     ! LN is leaf number
      ENDIF

C     7 - Preplanting
C     8 - Planting to root initiation
C     9 - Root initiation to first new leaf emergence
C     1 - First new leaf emergence to net zero root growth
C     2 - Net zero stem growth to forcing
C     3 - Forcing to sepals closed on youngest flowers
C     4 - SCY to first open flower
C     5 - Fruit growth
C     6 - Physiological maturity

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
        ! Green leaf weight is calculated from leaf area
        !
        GROLF  = PLAG*(1.0/((85.0*EXP(-XN*0.012))*TRF2))
        GROBSL = 0.42*GROLF
        !
        ! Daily root growth is calculated from carbo and daily leaf weight
        !
        ! If GRORT is less than 15% of carbo then set to 15% of carbo. A growth reducing
        ! factor (GRF) is calculated and GROLF, GROBSL are reduced by GRF. And PLA is
        ! recalculated.
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
        LFWT    = LFWT+GROLF                   ! Update green leaf weight
        BASLFWT = BASLFWT+GROBSL               ! Update basal leaf weight
        PLA     = PLA+PLAG                     ! Update total leaf area.

        IF (GROLF .GT. 0.0) THEN
           SLAN = PLA/1000.                     ! assumed 1 from 1000 leaf area senescence
        ENDIF
        LFWT = LFWT-SLAN/600.0                  ! recalculate green leaf weight

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

        IF (GROLF .GT. 0.0) THEN
           SLAN = PLA/1000.0
        ENDIF
        LFWT  = LFWT  - SLAN/600.0
        SUMP  = SUMP  + CARBO            ! Total biomass cumulated during the stage
        IDURP = IDURP + 1                ! Duration of the stage

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
           GROSTM = CARBO - GROSK - GRORT - GROFLR     ! Sucker initiation
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
        SKWT  = SKWT  + GROSK

!-----------------------------------------------------------------
      CASE (5)
        !
        ! Fruit growth
        !
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
           NSINK = GROGRN*GNP                !!!!!!!!!!!! GROGRN*GNP

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
        ! Physiological maturity
        !
        RETURN
      END SELECT
!-----------------------------------------------------------------

      IF (CARBO .EQ. 0.0) THEN
         CARBO = 0.001                 ! Make sure that carbo is not 0.
      ENDIF
      PDWI   = PCARB*(1.0-GRORT/CARBO) ! PDWI (g/plant/day) is potential shoot growth
      PGRORT = PCARB*GRORT/CARBO       ! Pgrort is potential root growth
      !
      ! Calculation of zero-to-unity factors for leaf senescence due to drought
      ! stress (SLFW), competition for light (SLFC), and low temperature (SLFT).
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
      ! Leaf area senescence on a day (PLAS) and LAI is calculated for stage 1 to 5
      !
      SLFT  = AMAX1 (SLFT,0.0)
      PLAS  = (PLA-SENLA)*(1.0-AMIN1(SLFW,SLFC,SLFT))
      SENLA = SENLA + PLAS
      SENLA = AMAX1 (SENLA,SLAN)
      SENLA = AMIN1 (SENLA,PLA)
      LAI   = (PLA-SENLA)*PLTPOP*0.0001

      IF (LN .GT. 3 .AND .LAI .LE. 0.0 .AND. ISTAGE .LE. 3) THEN
         WRITE (*,   2800)
         IF (IDETO .EQ. 'Y') THEN
            WRITE (NOUTDO,2800)
         ENDIF
         ISTAGE = 4
       ELSE
         IF (ICOLD .GE. 7) THEN
            WRITE (*,   2800)
            IF (IDETO .EQ. 'Y') THEN
               WRITE (NOUTDO,2800)
            ENDIF
           ISTAGE = 5
         ENDIF
      ENDIF
      !
      ! Half GRORT is used for respiration and 0.5% of root is lost due to senescence
      !
      RTWT = RTWT + 0.45*GRORT - 0.0025*RTWT
      !
      ! Finally, total biomass per unit area (BIOMAS g/m2), total plant weight,
      ! Total plant dry weight per hactare (DM kg/ha) and Plant top fraction
      ! (PTF) are calculated
      !
      BIOMAS   = (LFWT + STMWT + FLRWT + BASLFWT + SKWT)*PLTPOP
      TOTPLTWT =  LFWT + STMWT + FLRWT + BASLFWT + SKWT
      DM       = BIOMAS*10.0
      STOVWT   = LFWT + STMWT
      PTF      = (LFWT+BASLFWT+STMWT+FLRWT+SKWT)/(RTWT+LFWT+
     &            BASLFWT+SKWT+STMWT+FLRWT)
      
      IF (ISWNIT .NE. 'N') THEN
         CALL NUPTAK
      ENDIF

!-----------------------------------------------------------------
      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 2800 FORMAT (2X,'Crop failure growth program terminated ')
!=======================================================================
      END SELECT
!=======================================================================
      RETURN
      END SUBROUTINE Aloha_GROSUB
