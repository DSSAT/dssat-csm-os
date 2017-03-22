C======================================================================
C  Aloha_PHASEI, Subroutine
C
C  Determines phase initialization
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2. Header revision and minor changes           P.W.W.      2-7-93
C  3. Added switch common block, restructured     P.W.W.      2-7-93
C  4. Removed Y1 and Y2 variables                 B.D.B. 21-Jun-1994
C-----------------------------------------------------------------------
C  INPUT  : ISWWAT,ISWNIT
C
C  LOCAL  : PP3,SKERWT,AG2,RATIO,CUMRAT,GROPAN,PANDW,L,L1
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : PHENOL
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  AG2    :
C  CUMRAT :
C  PP3    :
C  RATIO  :
C  SKERWT : Weight of a single kernel - g
C=======================================================================

      SUBROUTINE Aloha_PHASEI (ISWWAT,ISWNIT)

      IMPLICIT  NONE

      INCLUDE  'GEN1.BLK'
      INCLUDE  'GEN2.BLK'
      INCLUDE  'GEN3.BLK'
      INCLUDE  'GEN4.BLK'
      INCLUDE  'NTRC1.BLK'

      CHARACTER ISWWAT*1,ISWNIT*1
      INTEGER   L,L1

      SAVE

      CNSD1  = 0.0
      CNSD2  = 0.0
      CSD1   = 0.0
      CSD2   = 0.0
      ICSDUR = 0

C     7 - Preplanting
C     8 - Planting to root initiation
C     9 - Root initiation to first new leaf emergence
C     1 - First new leaf emergence to net zero root growth
C     2 - Net zero stem growth to forcing
C     3 - Forcing to sepals closed on youngest flowers
C     4 - SCY to first open flower
C     5 - Fruit growth
C     6 - Physiological maturity

      SELECT CASE (ISTAGE)
        CASE (1)
          !
          ! ISTAGE = 1 >> First new leaf emergence to net zero root growth
          !
          ISTAGE = 2
          GROSTM = 0.0                   ! Daily stem growth (g/plant/day).
          RETURN
        CASE (2)
          !
          ! ISTAGE = 2 >> Net zero stem growth to forcing
          !
          ISTAGE = 3
          TBASE  = 10.00                ! Base temperature of 6.25 is used during forcing to sepals closed on youngest flowers
          SUMDTT = 0.0                  ! Cumulative GDD set to 0.0
          SUMP   = 0.0                  ! SUMP is the total weight of biomass cumulated in Istage 4.
          IDURP  = 0                    ! Duration of stage 3 (days)
          PLAMX  = PLA                  ! PLAMX (cm2/plant) is maximal green leaf area.  PLA is total green leaf area.
          GROFLR = 0.0
          GROCRWN= 0.0
          GROFRT = 0.0
          FLRWT  = 0.0
          FRTWT  = 0.0
          CRWNWT = 0.0
          RETURN
        CASE (3)
          !
          ! ISTAGE = 3 >> SCY to first open flower
          !
          ISTAGE = 4
          TBASE  = 10.0                 ! TBASE of 10.0 is used in this stage
          SUMDTT =  0.0                 ! Cumulative growing degree days set to 0.0
          FLRWT  =  0.1*STMWT           ! FLRWT stands for the weight ofwhole inflorescence STMWT is stem weight.  Both are in gram/plant.
          SKWT   =  0.0
          GROSK  =  0.0
          PTF    =  1.0                 ! PTF is plant top fraction in gram/plant.
          EYEWT  =  0.0                 ! EYEWT (G/eye) is weight of the eye
          VANC   = TANC                 ! Variable used in nitrogen balance
          VMNC   = TMNC                 ! .....
          RETURN
        CASE (4)
          !
          ! ISTAGE = 4 >> Fruit growth
          !
          ISTAGE = 5
          FRTWT  = FLRWT*0.5            ! FRTWT (g/plant) is fruit weight.  It is assumed to be 50% of inflorescence at begining of the stage
          CRWNWT = FLRWT*0.2            ! CRWNWT (g/plant) is crown weight which is assumed to be 20% of inflorescence at the begining of the stage
          SWMAX  = 0.0
          SWMIN  = 0.0
          TBASE  = 4.0                  ! Tbase of 4.0 is used in the stage
          SUMDTT = 0.0                  ! Cumulative growing degree days set to 0.0
          RETURN
        CASE (5)
          !
          ! ISTAGE = 5 >> Physiological maturity
          !
          ISTAGE = 6
          TBASE  = 12.0
          RETURN
        CASE (6)
          !
          ! ISTAGE = 6 >> Preplanting
          !
          ISTAGE = 7
          PLA    = WTINITIAL*0.6*63.0
          LAI    = PLA*PLTPOP*0.0001
          BIOMAS = WTINITIAL*PLTPOP
          RETURN
        CASE (7)
          !
          ! ISTAGE = 7 >> Planting to root initiation
          !
          ISTAGE = 8
          RTDEP  = SDEPTH               ! Rooting depth = seeding depth (cm)
          SUMDTT = 0.0                  ! Cumulative growing degree days set to 0.0
          PLA    = WTINITIAL*0.6*63.0
          LAI    = PLA*PLTPOP*0.0001
          BIOMAS = WTINITIAL*PLTPOP
          RETURN
        CASE (8)
          !
          ! ISTAGE = 8 >> Root initiation to first new leaf emergence
          !
          ISTAGE =  9
          NDEF3  =  1.0
          SUMDTT =  0.0                 ! Cumulative growing degree days set to 0.0
          TBASE  = 12.0                 ! Tbase of 12.0 is used
          RTWT   =  0.0
          NSTRES =  1.0
          AGEFAC =  1.0
          RETURN
        CASE (9)
          !
          ! ISTAGE = 9 >> First new leaf emergence to net zero root growth
          !
          ISTAGE  = 1
          TBASE   = TBASE1              ! Tbase1 used for calibration
          SUMDTT  = 0.0                 ! Cumulative growing degree days set to 0.0
          CUMDTT  = 0.0                 ! CUMDTT is also cumulative growing degree days but it is set to 0.0 only at root initiation crown weight when planting
          PLAY    = PLA                 ! PLAY used in CERES-MAIZE
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
      END SELECT

      CUMDEP = 0.0
      IF (ISWWAT .EQ. 'N') RETURN

      DO L = 1, NLAYR
         CUMDEP = CUMDEP + DLAYR(L)
         RLV(L) = 0.20*PLTPOP/DLAYR (L)
         IF (CUMDEP .GT. RTDEP) EXIT
      END DO

      RLV(L) = RLV(L)*(1.0-(CUMDEP-RTDEP)/DLAYR(L))
      L1 = L + 1
      IF (L1 .LT. NLAYR) THEN
         DO L = L1, NLAYR
            RLV(L) = 0.0
         END DO
      ENDIF

      DO L = 1, NLAYR
         RWU(L) = 0.0
      END DO

      IF (ISWNIT .NE. 'N') THEN
         RANC  = 0.022
         TANC  = 0.044
         ROOTN = RANC*RTWT
         STOVN = STOVWT*TANC
      ENDIF

      RETURN
      END SUBROUTINE Aloha_PHASEI
