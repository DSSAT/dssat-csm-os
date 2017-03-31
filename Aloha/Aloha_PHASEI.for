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

      CHARACTER ISWWAT*1,ISWNIT*1
      INTEGER   L,L1
      SAVE

!     Stress factors for phases now done in OPSTRESS
!      CNSD1  = 0.0
!      CNSD2  = 0.0
!      CSD1   = 0.0
!      CSD2   = 0.0
!      ICSDUR = 0

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
!moved to phenol          ISTAGE = 2
!moved to grosub          GROSTM = 0.0                   ! Daily stem growth (g/plant/day).
          RETURN

        CASE (2)
          !
          ! ISTAGE = 2 >> Net zero stem growth to forcing
          !
!moved to phenol          ISTAGE = 3
!moved to phenol          TBASE  = 10.00                ! Base temperature of 6.25 is used during forcing to sepals closed on youngest flowers
!moved to phenol          SUMDTT = 0.0                  ! Cumulative GDD set to 0.0
!moved to grosub          SUMP   = 0.0                  ! SUMP is the total weight of biomass cumulated in Istage 4.
!moved to grosub          IDURP  = 0                    ! Duration of stage 3 (days)
!moved to grosub          PLAMX  = PLA                  ! PLAMX (cm2/plant) is maximal green leaf area.  PLA is total green leaf area.
!moved to grosub          GROFLR = 0.0
!moved to grosub          GROCRWN= 0.0
!moved to grosub          GROFRT = 0.0
!moved to grosub          FLRWT  = 0.0
!moved to grosub          FRTWT  = 0.0
!moved to grosub          CRWNWT = 0.0
          RETURN
        CASE (3)
          !
          ! ISTAGE = 3 >> SCY to first open flower
          !
!moved to phenol          ISTAGE = 4
!moved to phenol          TBASE  = 10.0                 ! TBASE of 10.0 is used in this stage
!moved to phenol          SUMDTT =  0.0                 ! Cumulative growing degree days set to 0.0
!moved to grosub          FLRWT  =  0.1*STMWT           ! FLRWT stands for the weight ofwhole inflorescence STMWT is stem weight.  Both are in gram/plant.
!moved to grosub          SKWT   =  0.0
!moved to grosub          GROSK  =  0.0
!moved to grosub          PTF    =  1.0                 ! PTF is plant top fraction in gram/plant.
!moved to grosub          EYEWT  =  0.0                 ! EYEWT (G/eye) is weight of the eye
!moved to grosub          VANC   = TANC                 ! Variable used in nitrogen balance
!moved to grosub          VMNC   = TMNC                 ! .....
          RETURN

        CASE (4)
          !
          ! ISTAGE = 4 >> Fruit growth
          !
!moved to phenol          ISTAGE = 5
!moved to phenol          TBASE  = 4.0                  ! Tbase of 4.0 is used in the stage
!moved to phenol          SUMDTT = 0.0                  ! Cumulative growing degree days set to 0.0
!moved to grosub          FRTWT  = FLRWT*0.5            ! FRTWT (g/plant) is fruit weight.  It is assumed to be 50% of inflorescence at begining of the stage
!moved to grosub          CRWNWT = FLRWT*0.2            ! CRWNWT (g/plant) is crown weight which is assumed to be 20% of inflorescence at the begining of the stage
!moved to grosub          SWMAX  = 0.0
!moved to grosub          SWMIN  = 0.0
          RETURN

        CASE (5)
          !
          ! ISTAGE = 5 >> Physiological maturity
          !
!moved to phenol          ISTAGE = 6
!moved to phenol          TBASE  = 12.0
          RETURN
        CASE (6)
          !
          ! ISTAGE = 6 >> Preplanting
          !
!moved to phenol          ISTAGE = 7
!moved to grosub          PLA    = WTINITIAL*0.6*63.0
!moved to grosub          LAI    = PLA*PLTPOP*0.0001
!moved to grosub          BIOMAS = WTINITIAL*PLTPOP
          RETURN
        CASE (7)
          !
          ! ISTAGE = 7 >> Planting to root initiation
          !
! moved to phenol          ISTAGE = 8
! moved to rootgr          RTDEP  = SDEPTH               ! Rooting depth = seeding depth (cm)
! moved to phenol          SUMDTT = 0.0                  ! Cumulative growing degree days set to 0.0
! moved to grosub          PLA    = WTINITIAL*0.6*63.0
! moved to grosub          LAI    = PLA*PLTPOP*0.0001
! moved to grosub          BIOMAS = WTINITIAL*PLTPOP
          RETURN

        CASE (8)
          !
          ! ISTAGE = 8 >> Root initiation to first new leaf emergence
          !
!moved to phenol          ISTAGE =  9
!moved to grosub          NDEF3  =  1.0
!moved to phenol          SUMDTT =  0.0                 ! Cumulative growing degree days set to 0.0
!moved to phenol          TBASE  = 12.0                 ! Tbase of 12.0 is used
!moved to rootgr          RTWT   =  0.0
!moved to grosub          NSTRES =  1.0
!moved to grosub          AGEFAC =  1.0
          RETURN

        CASE (9)
          !
          ! ISTAGE = 9 >> First new leaf emergence to net zero root growth
          !
!moved to phenol          ISTAGE  = 1
!moved to phenol          TBASE   = TBASE1              ! Tbase1 used for calibration
!moved to phenol          SUMDTT  = 0.0                 ! Cumulative growing degree days set to 0.0
!moved to phenol          CUMDTT  = 0.0                 ! CUMDTT is also cumulative growing degree days but it is set to 0.0 only at root initiation crown weight when planting
!not used                 PLAY    = PLA                 ! PLAY used in CERES-MAIZE
!moved to grosub          PLAG    = 0.0                 ! PLAG (cm^2) is daily green leaf area growth
!moved to grosub          LAI     = PLTPOP*PLA*0.0001   ! leaf area index (m2 leaf/m2 ground)
!moved to grosub          LFWT    = WTINITIAL*0.53      ! LFWT (g/plant) is green leaf weight which is assumed to be 53% of initial crown weight
!moved to grosub          RTWT    = 0.20                ! RTWT (g/plant) is root weight
!moved to grosub          STMWT   = WTINITIAL*0.115     ! STMWT is 115% of initial crown weight
!moved to grosub          BASLFWT = LFWT*0.66           ! Basal white leaf weight is 66% of green leaf weight
!moved to grosub          FLRWT   = 0.0                 ! Inflorescence weight is set to 0.0
!moved to grosub          STOVWT  = WTINITIAL           ! STOVWT (g/plant) is stover weight
!moved to grosub          FLRWT   = 0.0
!moved to grosub          GROSTM  = 0.0                 ! GROSTM (g/plant/day) is daily stem growth
!moved to grosub          SENLA   = 0.0                 ! SENLA (cm2/plant) is area of leaf senesces due to stress on a given day
!moved to grosub          SLAN    = 0.0                 ! SLAN (cm2/plant) is total normal leaf senescence since emergence.
!moved to grosub          GRORT   = 0.0                 ! GRORT (g/plant/day) is daily root growth
!moved to grosub          GROBSL  = 0.0                 ! GROBSL (g/plant/day) is daily basal leaf growth
!moved to grosub          GROLF   = 0.0                 ! GROLF (g/plant/day) is daily green leaf growth
!moved to grosub          CUMPH   = 0.514               ! CUMPH (leaves/plant) is number of leaves emerged
!moved to grosub          LN      = 1                   ! LN (leaves/plant) is leaf number
!moved to grosub          CUMDEP  = 0.0                 ! variable used in CERES-MAIZE
      END SELECT

!moved to rootgr      IF (ISWWAT .EQ. 'N') RETURN
!moved to rootgr      CUMDEP = 0.0
!moved to rootgr      DO L = 1, NLAYR
!moved to rootgr         CUMDEP = CUMDEP + DLAYR(L)
!moved to rootgr         RLV(L) = 0.20*PLTPOP/DLAYR (L)
!moved to rootgr         IF (CUMDEP .GT. RTDEP) EXIT
!moved to rootgr      END DO
!moved to rootgr
!moved to rootgr      RLV(L) = RLV(L)*(1.0-(CUMDEP-RTDEP)/DLAYR(L))
!moved to rootgr      L1 = L + 1
!moved to rootgr      IF (L1 .LT. NLAYR) THEN
!moved to rootgr         DO L = L1, NLAYR
!moved to rootgr            RLV(L) = 0.0
!moved to rootgr         END DO
!moved to rootgr      ENDIF

!done in SPAM         DO L = 1, NLAYR
!done in SPAM            RWU(L) = 0.0
!done in SPAM         END DO

!moved to grosub      IF (ISWNIT .NE. 'N') THEN
!moved to grosub         RANC  = 0.022
!moved to grosub         TANC  = 0.044
!moved to grosub         ROOTN = RANC*RTWT
!moved to grosub         STOVN = STOVWT*TANC
!moved to grosub      ENDIF

      RETURN
      END SUBROUTINE Aloha_PHASEI
