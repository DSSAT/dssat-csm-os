C=======================================================================
! combination of previous PHENOL and INPHEN subroutines
!  SUBROUTINE Aloha_PHENOL
C  Determines phenological stage
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2. Header revision and minor changes           P.W.W.      2-7-93
C  3. Added switch block, code cleanup            P.W.W.      2-7-93
C  4. Modified TT calculations to reduce line #'s P.W.W.      2-7-93
C  5. Modified for MILLET model                   W.T.B.      MAY 94
C=======================================================================

      SUBROUTINE Aloha_PHENOL (CONTROL, ISWITCH,
     &    SW, WEATHER, SOILPROP,          !Input
     &    ISTAGE, MDATE, STGDOY)                 !Output

      USE Aloha_mod
      IMPLICIT    NONE
      SAVE

      INTEGER     STGDOY(20),YRDOY,I,NDAS,L,L0
      REAL        TTMP,SWSD,YIELDB,XLAT,PHOTOSYNEYE,PEYEWT

      INTEGER      DYNAMIC, MDATE,YRSIM,HAREND
      REAL         XSTAGE
      REAL         GRAINN

      CHARACTER*1 ISWWAT, IDETO, ISWNIT
      INTEGER     ISTAGE, NLAYR, NOUTDO, ISDATE, IDURP, FHDATE, PMDATE
      INTEGER     ICSDUR
      REAL        TBASE, LAI, BIOMAS
      REAL        STMWT, APTNUP, DTT, TEMPM
!      REAL        TBASV, TOPTV, TTOPV, TBASR, TOPTR, TTOPR
      REAL        TMFAC1(8)
      REAL        TMIN, TMAX, TEMPFMX, SUMDTT, CUMDEP, GPP
      REAL        FRTWT, RTDEP, TEMPFM, TOTPLTWT, MAXLAI, SUMP
      REAL        FRUITS, SWMAX, SWMIN,  YIELD, EYEWT, GPSM, STOVER
      REAL        FDMC, HBIOM, XGNP, GNUP, TOTNUP
      REAL        CSD1, CSD2, CNSD1, CNSD2
      REAL        P1, P2, P3, P4, P5, P6, TBASE1
      REAL        G2
      REAL, DIMENSION(NL) :: SW, LL, DLAYR, CUMDTT, FBIOM
      REAL, DIMENSION(20) :: SI1, SI2, SI3, SI4

        REAL PLTPOP, SDEPTH, PLANTSIZE
        INTEGER NFORCING, NDOF


      TYPE (CONTROLTYPE) CONTROL
      TYPE (SwitchType) ISWITCH
      TYPE (SOILTYPE) SOILPROP
      TYPE (WEATHERTYPE) WEATHER

      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY

      TMIN = WEATHER % TMIN
      TMAX = WEATHER % TMAX

      LL   = SOILPROP % LL

C     7 - Preplanting
C     8 - Planting to root initiation
C     9 - Root initiation to first new leaf emergence
C     1 - First new leaf emergence to net zero root growth
C     2 - Net zero stem growth to forcing
C     3 - Forcing to sepals closed on youngest flowers
C     4 - SCY to first open flower
C     5 - Fruit growth
C     6 - Physiological maturity

!=================================================================
      SELECT CASE(DYNAMIC)
!=================================================================
      CASE (RUNINIT, SEASINIT)
!-----------------------------------------------------------------

      XLAT = WEATHER % XLAT
      ISWWAT = ISWITCH % ISWWAT
      ISWNIT = ISWITCH % ISWNIT

      ISTAGE = 7
      XSTAGE = 0.1

      STGDOY(14) = YRSIM
      MDATE      = -99
      HAREND     = -99

      TBASE      = 12.0
      !TBASV = SPECIES % TBASV
      !TOPTV = SPECIES % TOPTV
      !TTOPV = SPECIES % TTOPV
      !TBASR = SPECIES % TBASR
      !TOPTR = SPECIES % TOPTR
      !TTOPR = SPECIES % TTOPR

      !LAI        = PLTPOP*PLA*0.0001
      !BIOMAS     = WTINITIAL*PLTPOP
      !PLA        = WTINITIAL*0.6*63.0
      !LFWT       = WTINITIAL*0.53
      !BASLFWT    = LFWT*0.66
      !STMWT      = WTINITIAL*0.115
      !STOVWT     = WTINITIAL

      !IF (ISWNIT .NE. 'Y') THEN
      !   TANC = 0.0
      !ENDIF
      !
      !! Calculate initial SEED N
      !!
      !SEEDNI = (ROOTN+STOVN+GRAINN+SEEDN)*PLTPOP

      DO I = 1, 8
         TMFAC1(I) = 0.931 + 0.114*I-0.0703*I**2+0.0053*I**3
      END DO

      SDEPTH   = Planting % SDEPTH
      NFORCING = Planting % NFORCING
      NDOF     = Planting % NDOF
      PLTPOP   = Planting % PLTPOP

      P1 = Cultivar % P1
      P2 = Cultivar % P2
      P3 = Cultivar % P3
      P4 = Cultivar % P4
      P5 = Cultivar % P5
      P6 = Cultivar % P6
      G2 = Cultivar % G2
      TBASE1  = 12. !????

!=================================================================
      CASE (RATE)
!-----------------------------------------------------------------

!moved to grosub      XANC   = TANC*100.0               ! Top actual N concentration (g N/g Dry weight)
!moved to grosub      APTNUP = STOVN*10.0*PLTPOP
!move to below        DTT    = TEMPM - TBASE
!from FileX           SDEPTH = 5.0

      SELECT CASE (ISTAGE)
        CASE (1,2,3,7,8,9)
          IF (TMIN .GT. TBASE .AND. TMAX .LT. 35.0) THEN
             IF (XLAT .LT. 21.0 .and. XLAT .GT. -21.0) THEN
                TEMPM = 0.6*TMIN+0.4*TMAX
              ELSE
                TEMPM = (TMAX+TMIN)/2
             ENDIF
             DTT = TEMPM - TBASE
           ELSEIF (TMIN .LE. TBASE .OR. TMAX .GE. 35.0) THEN
             IF (TMAX .LT. TBASE) THEN
                DTT = 0.0
             ENDIF
             IF (DTT .NE. 0.0) THEN
                DTT = 0.0
                DO I = 1, 8
                   TTMP = TMIN + TMFAC1(I)*(TMAX-TMIN)
                   IF (TTMP .GT. TBASE .AND. TTMP .LE. 35.0) THEN
                      DTT = DTT + (TTMP-TBASE)/8.0
                   ENDIF
                   IF (TTMP .GT. 35.0 .AND. TTMP .LT. 45.0) THEN
                      DTT = DTT + (35.0-TBASE)*(1.0-(TTMP-35.0)/10.0)/8.
                   ENDIF
                END DO
             ENDIF
          ENDIF
!-----------------------------------------------------------------
!       Reproductive Phase
        CASE (4,5,6)
          IF (TMAX .LT. TBASE) THEN
             DTT = 0.0
          ENDIF
          IF (DTT .GT. 0.0) THEN
             
!            Correcting fruit temperature and higher temperature effect
             IF (TMAX .GT. 18.0 .AND. TMAX .LT. 33.0) THEN
                TEMPFMX = 4.32*EXP(0.078*TMAX)
              ELSEIF (TMAX .GE. 33.0 .AND. TMAX .LT. 50.0) THEN
                TEMPFMX = TMAX*(1.715-(TMAX-33.0)/35.0)
              ELSEIF (TMAX .GE. 50.0) THEN
                TEMPFMX = 62.0
              ELSE
                TEMPFMX = TMAX
             ENDIF
             IF (TMIN .GT. TBASE .AND. TEMPFMX .LT. 42.0) THEN
                IF (XLAT .LT. 21.0 .AND. XLAT .GT. -21.0) THEN
                   TEMPFM = 0.6*TMIN+0.4*TEMPFMX
                 ELSE
                   TEMPFM = (TEMPFMX+TMIN)/2
                ENDIF
                DTT = TEMPFM-TBASE
                GO TO 20
             ENDIF

             IF (TEMPFMX .LT. TBASE) THEN
                DTT = 0.0
             ENDIF

             IF (DTT .GT. 0.0) THEN
                DTT = 0.0
                DO I = 1, 8
                   TTMP = TMIN + TMFAC1(I)*(TEMPFMX-TMIN)
                   IF (TTMP .GT. TBASE .AND. TTMP .LE. 42.0) THEN
                      DTT = DTT + (TTMP-TBASE)/8.0
                    ELSEIF (TTMP .GT. 42.0 .AND. TTMP .LT. 62.0) THEN
                      DTT = DTT + (42.0-TBASE)*(1.0-((TTMP-42.0)/
     &                      (62.0-42.0)))/8.0
                    ELSE
                      DTT = DTT
                   ENDIF
                END DO
             ENDIF
          ENDIF
      END SELECT

   20 SUMDTT  = SUMDTT  + DTT

!-----------------------------------------------------------------
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
!-----------------------------------------------------------------
        CASE (7)
          !
          ! Stage 7 >> Preplanting
          !
          STGDOY(ISTAGE) = YRDOY
          NDAS           = 0
 !        CALL PHASEI (ISWWAT,ISWNIT)
          ISTAGE = 8
          SUMDTT = 0.0       ! Cumulative growing degree days set to 0.0

          IF (ISWWAT .EQ. 'N') RETURN
          CUMDEP = 0.0
          DO L = 1, NLAYR
             CUMDEP = CUMDEP + DLAYR(L)
             IF (SDEPTH .LT. CUMDEP) EXIT
          END DO
          L0 = L
          RETURN

!-----------------------------------------------------------------
        CASE (8)
          !
          ! Stage 8 >> Planting to root initiation
          !

!         Check for soil too dry for rooting
          IF (ISWWAT .NE. 'N') THEN
             IF (SW(L0) .LE. LL(L0)) THEN
                 SWSD = (SW(L0)-LL(L0))*0.65 + (SW(L0+1)-LL(L0+1))*0.35
                 NDAS = NDAS + 1
                 IF (SWSD .LT. 0.02) RETURN
             ENDIF
          ENDIF

!         After 140 days, give up
          IF (NDAS .GT. 140) THEN  !<-- genotype parameter?
             ISTAGE = 6       !"maturity"
             PLTPOP = 0.0
             GPP    = 1.0
             FRTWT  = 0.0
             WRITE (     *,1399)
             IF (IDETO .EQ. 'Y') THEN
                WRITE (NOUTDO,1399)
             ENDIF
            RETURN
          ENDIF

          STGDOY(ISTAGE) = YRDOY
  !        CALL PHASEI (ISWWAT,ISWNIT)
          ISTAGE =  9
          SUMDTT =  0.0                 ! Cumulative growing degree days set to 0.0
          TBASE  = 12.0                 ! Tbase of 12.0 is used
          RETURN

!-----------------------------------------------------------------
        CASE (9)
          !
          ! Stage 9 >> Root initiation to first new leaf emergence
          !
          NDAS   = NDAS + 1
          RTDEP  = RTDEP + 0.01*DTT     ! Depth of root (f) DTT
          IF (NDAS .LT. P6) THEN
             RETURN                     ! P6: NDAS from root initiation to first leaf emerged
          ENDIF
          STGDOY(ISTAGE) = YRDOY

  !        CALL PHASEI (ISWWAT,ISWNIT)
          ISTAGE  = 1
          TBASE   = TBASE1              ! Tbase1 used for calibration
          SUMDTT  = 0.0                 ! Cumulative growing degree days set to 0.0
          CUMDTT  = 0.0                 ! CUMDTT is also cumulative growing degree days but it is set to 0.0 only at root initiation crown weight when planting
          RETURN

!-----------------------------------------------------------------
        CASE (1)
          !
          ! Stage 1 >> First new leaf emergence to net zero root growth
          !
          NDAS   = NDAS + 1
          XSTAGE = SUMDTT / P1
          IF (NDAS .LT. (P1+P6)) THEN
             RETURN                     ! P1: NDAS from leaf emerged to end stem growth
          ENDIF
          STGDOY(ISTAGE) = YRDOY
  !        CALL PHASEI (ISWWAT,ISWNIT)
          ISTAGE = 2
          RETURN

!-----------------------------------------------------------------
        CASE (2)
          !
          ! Stage 2 >> Net zero stem growth to forcing
          !
          NDAS   = NDAS + 1
          IF (NFORCING .GE. 2) THEN
             !
             ! Forcing by number of days after planting
             !
             IF (NDAS .LT. NDOF) THEN
                RETURN
             ENDIF
             PLANTSIZE = TOTPLTWT
           ELSE
              !
              ! Forcing by Plant Size (200 to 350 grams usually)
              !
              IF (TOTPLTWT .LT. PLANTSIZE) THEN
                 RETURN
              ENDIF
          ENDIF

          ISDATE = YRDOY                ! Record forcing date.
          FBIOM  = BIOMAS               ! Record biomass at forcing

!         Ready for next stage
          STGDOY(ISTAGE) = YRDOY
          ISTAGE = 3
          TBASE  = 10.00                ! Base temperature of 6.25 is used during forcing to sepals closed on youngest flowers
          SUMDTT = 0.0                  ! Cumulative GDD set to 0.0

!-----------------------------------------------------------------
        CASE (3)
          !
          ! Stage 3 >> Forcing to sepals closed on youngest flowers
          !
          IF (SUMDTT .LT. P2) THEN
             RETURN                       ! P2: GDD needed to complete this stage
          ENDIF

          MAXLAI      = LAI               ! MaxLAI = LAI at the end of the stage
C         ABIOMS      = BIOMAS            ! Above biomass per square meter (g/m^2)
          PHOTOSYNEYE = SUMP*1000./IDURP  ! Average photosysnthesis rate of fruit eye

          GPP    = G2*(PHOTOSYNEYE/12000+0.43)*
     &             (0.7+0.3*PLANTSIZE/550.)
          GPP    = AMIN1 (GPP,G2)                ! G2 is genetic coefficient for potential eye number
          GPP    = AMAX1 (GPP,0.0)
          FRUITS = PLTPOP*(1.-0.10*PLTPOP/14.0)  ! number of fruits=PLTPOP/m2*FRUITING%

!         Ready for next stage
          STGDOY(ISTAGE) = YRDOY
          ISTAGE = 4
          TBASE  = 10.0                 ! TBASE of 10.0 is used in this stage
          SUMDTT =  0.0                 ! Cumulative growing degree days set to 0.0

!-----------------------------------------------------------------
        CASE (4)
          !
          ! Stage 4 >> SCY to first open flower
          !
          XSTAGE = 1.5+3.0*SUMDTT/P3      ! Used by CERES-MAIZE
          IF (SUMDTT .LT. P3) THEN
             RETURN                       ! P3: GDD needed to complete this stage
          ENDIF

!         Ready for next stage
          STGDOY(ISTAGE) = YRDOY
          ISTAGE = 5
          TBASE  = 4.0                  ! Tbase of 4.0 is used in the stage
          SUMDTT = 0.0                  ! Cumulative growing degree days set to 0.0

!-----------------------------------------------------------------
        CASE (5)
          !
          ! Stage 5 >> Fruit growth
          !
          XSTAGE = 4.5+5.5*SUMDTT/(P4*.8)
          IF (SWMAX .LE. 0.0) THEN
             IF (XSTAGE .GE. 10.0) THEN
                SWMAX = STMWT
                SWMIN = 0.65*SWMAX
             ENDIF
          ENDIF
          IF (SUMDTT .LT. (P4+(PLTPOP-8.0)*2.4*16.95)) THEN
             RETURN                        ! P4: GDD needed to complete this stage
          ENDIF
          FHDATE = YRDOY                   ! Fruit harvest date

          YIELD = FRTWT*10.0*FRUITS        ! fruit dry weight yield (kg/ha)
          IF (PLTPOP .GE. 0.0) THEN
             IF (GPP .GT. 0.0) THEN
                EYEWT = FRTWT/GPP
             ENDIF
             PEYEWT = EYEWT*1000.0         ! Eye weight (mg/eye)
             GPSM   = GPP*FRUITS           ! Number of eyes per square meter
             STOVER = BIOMAS*10.0-YIELD    ! Total plant weight except fruit
             YIELD  = YIELD/FDMC           ! Fresh fruit yield (kg/ha)
             YIELDB = YIELD/0.8914         ! Fresh fruit yield (lb/acre)
             STGDOY (ISTAGE) = YRDOY
          ENDIF

!         Ready for next stage
          ISTAGE = 6
          TBASE  = 12.0

!-----------------------------------------------------------------
        CASE (6)
          !
          ! Stage 6 >> Physiological maturity
          !
          XSTAGE = 4.5+5.5*SUMDTT/P5
          IF (SUMDTT .LT. (P5+P4)) THEN
             RETURN
          ENDIF

          HBIOM  = BIOMAS                 ! Record biomass at fruit harvest date

          IF (ISWNIT .NE. 'N') THEN
             IF (FRTWT .GT. 0.0) THEN
                XGNP = (GRAINN/FRTWT)*100.0
C               XPTN = XGNP*6.25
                GNUP = GRAINN*FRUITS*10.0
             ENDIF
             TOTNUP = GNUP + APTNUP
          ENDIF

          PMDATE = YRDOY                  ! physiological maturity date
          MDATE  = YRDOY                  ! Set MDATE to stop model
          STGDOY(ISTAGE) = YRDOY

!         Ready for next stage
          ISTAGE = 7

      END SELECT
!-----------------------------------------------------------------

      IF (ISWWAT .NE. 'N') THEN
         SI1(ISTAGE) = CSD1  / ICSDUR
         SI2(ISTAGE) = CSD2  / ICSDUR
         SI3(ISTAGE) = CNSD1 / ICSDUR
         SI4(ISTAGE) = CNSD2 / ICSDUR
      ENDIF

      IF (ISTAGE .NE. 6) THEN
  !       CALL PHASEI (ISWWAT,ISWNIT)
         RETURN
      ENDIF

!=================================================================
      END SELECT
!=================================================================


      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

1399  FORMAT ('Crop failure because of lack of root initiation',
     1       ' within 140 days of planting')
2380  FORMAT ('Crop failure - Growth program terminated')
3600  FORMAT (1X,'Crop failure because of lack of germination ',
     1           'within 40 days of sowing')

      END SUBROUTINE Aloha_PHENOL
!=================================================================
