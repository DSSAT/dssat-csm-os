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
     &    SW, WEATHER, SOILPROP, YRPLT,                       !Input
     &    DTT, EDATE, ISDATE, ISTAGE, MDATE, PMDATE,          !Output
     &    STGDOY, SUMDTT, TBASE, TEMPM, XSTAGE)               !Output

      USE Aloha_mod
      IMPLICIT    NONE
      EXTERNAL TIMDIF
      SAVE

      INTEGER     STGDOY(20),YRDOY,I,NDAS,L,L0, TIMDIF, YRPLT
      REAL        TTMP,SWSD,XLAT

!     REAL        YIELDB,PHOTOSYNEYE,PEYEWT,LAI, BIOMAS, MAXLAI, SUMP
!     INTEGER     IDURP, ICSDUR
!     REAL        STMWT, APTNUP, RTDEP, 
!     REAL        FRUITS, SWMAX, SWMIN, YIELD, EYEWT, GPSM, STOVER
!     REAL        FDMC, HBIOM, XGNP, GNUP, TOTNUP
!     REAL        CSD1, CSD2, CNSD1, CNSD2
!     REAL, DIMENSION(NL) :: FBIOM
!     REAL, DIMENSION(20) :: SI1, SI2, SI3, SI4

      INTEGER      DYNAMIC, EDATE, MDATE,HAREND
      REAL         XSTAGE

      CHARACTER*1 ISWWAT, IDETO, ISWNIT
      INTEGER     ISTAGE, NLAYR, NOUTDO, ISDATE, FHDATE, PMDATE
      REAL        TBASE
      REAL        DTT, TEMPM
!      REAL        TBASV, TOPTV, TTOPV, TBASR, TOPTR, TTOPR
      REAL        TMFAC1(8)
      REAL        TMIN, TMAX, TEMPFMX, SUMDTT, CUMDEP, GPP
      REAL        FRTWT, TEMPFM, TOTPLTWT
      REAL        P1, P2, P3, P4, P5, P6, TBASE1
      REAL        CUMDTT
      REAL, DIMENSION(NL) :: SW, LL, DLAYR

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

      LL    = SOILPROP % LL
      NLAYR = SOILPROP % NLAYR
      DLAYR = SOILPROP % DLAYR

!     7 - Preplanting
!     8 - Planting to root initiation
!     9 - Root initiation to first new leaf emergence
!     1 - First new leaf emergence to net zero root growth
!     2 - Net zero stem growth to forcing
!     3 - Forcing to sepals closed on youngest flowers
!     4 - SCY to first open flower
!     5 - Fruit growth
!     6 - Physiological maturity

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

      STGDOY(14) = CONTROL%YRSIM
      MDATE      = -99
      HAREND     = -99
      EDATE      = 9999999

      TBASE      = 12.0
      !TBASV = SPECIES % TBASV
      !TOPTV = SPECIES % TOPTV
      !TTOPV = SPECIES % TTOPV
      !TBASR = SPECIES % TBASR
      !TOPTR = SPECIES % TOPTR
      !TTOPR = SPECIES % TTOPR

!TEMP
!      IF (ISWNIT .NE. 'Y') THEN
!         TANC = 0.0
!      ENDIF
!      
!      ! Calculate initial SEED N
!      !
!      SEEDNI = (ROOTN+STOVN+GRAINN+SEEDN)*PLTPOP

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
      TBASE1  = 16. !????

!=================================================================
      CASE (RATE)
!-----------------------------------------------------------------

!moved to grosub      XANC   = TANC*100.0               ! Top actual N concentration (g N/g Dry weight)
!moved to grosub      APTNUP = STOVN*10.0*PLTPOP
!from FileX           SDEPTH = 5.0

      DTT    = TEMPM - TBASE
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

          IF (PLANTING % NFORCING .GE. 2) THEN
            NDOF = TIMDIF(YRPLT, PLANTING % ForcingYRDOY)
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
!         Cumulative growing degree days set to 0.0
          SUMDTT =  0.0
!         Tbase of 12.0 is used                 
          TBASE  = 12.0
          RETURN

!-----------------------------------------------------------------
        CASE (9)
          !
          ! Stage 9 >> Root initiation to first new leaf emergence
          !
          NDAS   = NDAS + 1
!MOVE TO GROSUB          RTDEP  = RTDEP + 0.01*DTT     ! Depth of root (f) DTT
          IF (NDAS .LT. P6) THEN
             ! P6: NDAS from root initiation to first leaf emerged
             RETURN               
          ENDIF
          STGDOY(ISTAGE) = YRDOY
          EDATE = YRDOY

  !        CALL PHASEI (ISWWAT,ISWNIT)
          ISTAGE  = 1
          ! Tbase1 used for calibration
          TBASE   = TBASE1
          ! Cumulative growing degree days set to 0.0
          SUMDTT  = 0.0        
          ! CUMDTT is also cumulative growing degree days but 
          ! it is set to 0.0 only at root initiation crown weight 
          ! when planting
          CUMDTT  = 0.0
          RETURN

!-----------------------------------------------------------------
        CASE (1)
          !
          ! Stage 1 >> First new leaf emergence to net zero root growth
          !
          NDAS   = NDAS + 1
          XSTAGE = SUMDTT / P1
          IF (NDAS .LT. (P1+P6)) THEN
             ! P1: NDAS from leaf emerged to end stem growth
             RETURN                     
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
!moved to grosub             PLANTSIZE = TOTPLTWT
           ELSE
              !
              ! Forcing by Plant Size (200 to 350 grams usually)
              !
              IF (TOTPLTWT .LT. PLANTSIZE) THEN
                 RETURN
              ENDIF
          ENDIF

          ISDATE = YRDOY                ! Record forcing date.

!         Ready for next stage
          STGDOY(ISTAGE) = YRDOY
          ISTAGE = 3
          ! Base temperature of 6.25 is used during forcing to sepals 
          ! closed on youngest flowers
          TBASE  = 10.00            
          ! Cumulative GDD set to 0.0
          SUMDTT = 0.0                

!-----------------------------------------------------------------
        CASE (3)
          !
          ! Stage 3 >> Forcing to sepals closed on youngest flowers
          !
          IF (SUMDTT .LT. P2) THEN
             ! P2: GDD needed to complete this stage
             RETURN                      
          ENDIF

!         Ready for next stage
          STGDOY(ISTAGE) = YRDOY
          ISTAGE = 4
          ! TBASE of 10.0 is used in this stage
          TBASE  = 10.0                
          ! Cumulative growing degree days set to 0.0 
          SUMDTT =  0.0                

!-----------------------------------------------------------------
        CASE (4)
          !
          ! Stage 4 >> SCY to first open flower
          !
          XSTAGE = 1.5+3.0*SUMDTT/P3      ! Used by CERES-MAIZE
          IF (SUMDTT .LT. P3) THEN
             ! P3: GDD needed to complete this stage
             RETURN                       
          ENDIF

!         Ready for next stage
          STGDOY(ISTAGE) = YRDOY
          ISTAGE = 5
          ! Tbase of 4.0 is used in the stage
          TBASE  = 4.0                  
          ! Cumulative growing degree days set to 0.0
          SUMDTT = 0.0                  

!-----------------------------------------------------------------
        CASE (5)
          !
          ! Stage 5 >> Fruit growth
          !
          XSTAGE = 4.5+5.5*SUMDTT/(P4*.8)
          IF (SUMDTT .LT. (P4+(PLTPOP-8.0)*2.4*16.95)) THEN
             ! P4: GDD needed to complete this stage
             RETURN                        
          ENDIF
          FHDATE = YRDOY                   ! Fruit harvest date
          STGDOY(ISTAGE) = YRDOY

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

!MOVE TO GROSUB          HBIOM  = BIOMAS  ! Record biomass at fruit harvest date

          PMDATE = YRDOY                  ! physiological maturity date
          MDATE  = YRDOY                  ! Set MDATE to stop model
          CONTROL % CropStatus = 1
          STGDOY(ISTAGE) = YRDOY

!         Ready for next stage
!          ISTAGE = 7

      END SELECT
!-----------------------------------------------------------------

  !    IF (ISTAGE .NE. 6) THEN
  !!       CALL PHASEI (ISWWAT,ISWNIT)
  !       RETURN
  !    ENDIF

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
