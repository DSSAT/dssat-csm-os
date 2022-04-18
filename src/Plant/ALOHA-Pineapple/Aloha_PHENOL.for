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
C  6. Stages changes for inclusion in Overview    J.V.J.      9-5-20      
C=======================================================================

      SUBROUTINE Aloha_PHENOL (CONTROL, ISWITCH,
     &    SW, WEATHER, SOILPROP, YRPLT,                       !Input
     &    DTT, EDATE, ISDATE, ISTAGE, MDATE, PMDATE,          !Output
     &    STGDOY, SUMDTT, TBASE, TEMPM, XSTAGE)               !Output

      USE Aloha_mod
      IMPLICIT    NONE
      SAVE

      INTEGER     STGDOY(20),YRDOY,I,NDAS,L,L0, TIMDIF, YRPLT
      REAL        TTMP,SWSD,XLAT,ROOTINGTIME   !ELIMINAR ROOTINGTIME SINO FUNCIONA LA IDEA

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
!TEMP      REAL         GRAINN

      CHARACTER*1 ISWWAT, IDETO, ISWNIT
      INTEGER     ISTAGE, NLAYR, NOUTDO, ISDATE, FHDATE, PMDATE
      REAL        TBASE
      REAL        DTT, TEMPM
!      REAL        TBASV, TOPTV, TTOPV, TBASR, TOPTR, TTOPR
      REAL        TMFAC1(8)
      REAL        TMIN, TMAX, TEMPFMX, SUMDTT, CUMDEP, GPP
      REAL        FRTWT, TEMPFM, TOTPLTWT
      REAL        TC, P1, P2, P3, P4, P5, P6, P7, P8, G1, TBASE1
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

      ISTAGE = 10                                   ! ISTAGE = 7 JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
      XSTAGE = 0.1

      STGDOY(14) = CONTROL%YRSIM                    !REVISAR AQUI
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
      
      
      TC = Cultivar % TC
      P1 = Cultivar % P1
      P2 = Cultivar % P2
      P3 = Cultivar % P3
      P4 = Cultivar % P4
      P5 = Cultivar % P5
      P6 = Cultivar % P6
      P7 = Cultivar % P7
      P8 = Cultivar % P8
      G1 = Cultivar % G1
      TBASE1  = 16. !????

!=================================================================
      CASE (RATE)
!-----------------------------------------------------------------

!moved to grosub      XANC   = TANC*100.0               ! Top actual N concentration (g N/g Dry weight)
!moved to grosub      APTNUP = STOVN*10.0*PLTPOP
!from FileX           SDEPTH = 5.0

      DTT    = TEMPM - TBASE
      SELECT CASE (ISTAGE)
        CASE (1,2,3,4,5,10,11,12)                ! CASE (1,2,3,7,8,9) JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
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
        CASE (6,7,8,9)                  !CASE (4,5,6) JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
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
        CASE (10)               !JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
          !
          ! Stage 7 >> Preplanting
          !
          STGDOY(ISTAGE) = YRDOY
          NDAS           = 0
 !        CALL PHASEI (ISWWAT,ISWNIT)
          ISTAGE = 11         ! ISTAGE = 8
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
        CASE (11)    !JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
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
          IF (NDAS .GT. 140) THEN  !<-- genotype parameter?  IF (NDAS .GT. 140) THEN
             ISTAGE = 12       !"maturity" ISTAGE = 6       !"maturity"  JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
             PLTPOP = 0.0                                  ! Ojo en la fila de arriba decía ISTAGE=9 y funcionaba pero creo que es ISTAGE=12
             GPP    = 1.0
             FRTWT  = 0.0
             WRITE (     *,1399)
             IF (IDETO .EQ. 'Y') THEN
                WRITE (NOUTDO,1399)
             ENDIF
            RETURN
          ENDIF
           
          IF (SUMDTT .LT. (TC)) THEN      ! IF (SUMDTT .LT. (P6)) THEN
             
              RETURN                       ! 
          ENDIF          
          ROOTINGTIME = SUMDTT / TBASE      
     
          STGDOY(ISTAGE) = YRDOY
          !        CALL PHASEI (ISWWAT,ISWNIT)
          ISTAGE =  12                   !ISTAGE =  9  JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
          SUMDTT =  0.0                 ! Cumulative growing degree days set to 0.0
          TBASE  = 12.0                 ! Tbase of 12.0 is used
          RETURN

!-----------------------------------------------------------------
        CASE (12) !! JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
          !
          ! Stage 9 >> Root initiation to first new leaf emergence
          !
          NDAS   = NDAS + 1               !JVJ Necesario para que la fecha de forza salga correctamente
 !
          IF (SUMDTT .LT. (P1)) THEN   !IF (SUMDTT .LT. (P7)) THEN+30 porque la primera hoja sale con la aparicion de puntas de raices blancas más 30 GDD.
             RETURN                       
           ENDIF          
    
          !         Ready for next stage         
          STGDOY(ISTAGE) = YRDOY          ! Esto se traduce: despues de hacer la ecuación inmediata anterior la fecha de cumplimiento de la etapa  
          EDATE = YRDOY                   ! es el valor del dia del año resultante de ese calculo.

  !        CALL PHASEI (ISWWAT,ISWNIT)
          ISTAGE  = 1
          TBASE   = TBASE1              ! Tbase1 used for calibration
          SUMDTT  = 0.0                 ! Cumulative growing degree days set to 0.0
          CUMDTT  = 0.0                 ! CUMDTT is also cumulative growing degree days but it is set to 0.0 only at root initiation crown weight when planting
          RETURN

!-----------------------------------------------------------------
      CASE (1)           !JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
          !
    ! Stage 1 >> First new leaf emergence to net zero root growth
          !
          NDAS   = NDAS + 1               !JVJ Necesario para que la fecha de forza salga correctamente
          IF (SUMDTT .LT. (P2)) THEN  !(SUMDTT .LT. (P1))   IF (SUMDTT .LT. (P1).AND. LN .LT. 13) THEN 
             RETURN                   ! First new leaf emergence to Cicle 1               
          ENDIF                       !                                                                                    
                                      !                                      
!         Ready for next stage                                               

          STGDOY(ISTAGE) = YRDOY
          ISTAGE = 2                    !JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
          TBASE  = TBASE1                 ! TBASE of 10.0 is used in this stage
          SUMDTT =  0.0                 ! Cumulative growing degree days set to 0.0
          
!-----------------------------------------------------------------
!-----------------------------------------------------------------
        CASE (2) !JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
          !
          !
          ! Stage 2 >> Net zero stem growth to forcing
          NDAS   = NDAS + 1               !JVJ Necesario para que la fecha de forza salga correctamente
          IF (SUMDTT .LT. (P2+P3) ) THEN   !(SUMDTT .LT. (P1+35)) IF (SUMDTT .LT. (P1+P8) .AND. LN .LT. 26) THEN
             RETURN                       ! Cicle 1 to Cicle 2
          ENDIF

!         Ready for next stage
 
          STGDOY(ISTAGE) = YRDOY
          ISTAGE = 3                    !JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
          TBASE  = TBASE1                 ! TBASE of 10.0 is used in this stage
          SUMDTT =  0.0                 ! Cumulative growing degree days set to 0.0 
 

          CASE (3) !JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
          !
         ! Stage 3 >> Forcing to sepals closed on youngest flowers
          !
          NDAS   = NDAS + 1               !JVJ Necesario para que la fecha de forza salga correctamente
          IF (SUMDTT .LT. (P2+P3+P4) ) THEN   !(SUMDTT .LT. (P1+35+65)) IF (SUMDTT .LT. (P1+P8+TC) .AND. LN .LT. 39) THEN
             RETURN                       ! Cicle 2 to Cicle 3
          ENDIF

!         Ready for next stage
          STGDOY(ISTAGE) = YRDOY
          ISTAGE = 4                    !JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
          TBASE  = TBASE1                 ! TBASE of 10.0 is used in this stage
          SUMDTT =  0.0                 ! Cumulative growing degree days set to 0.0 
                 
 !----------------------------------------------------------------- 
          
 
        CASE (4)                        ! CASE (2) JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
          !
          ! Stage 2 >> Net zero stem growth to forcing  ! JVJ Stage 4 >> Cicle 3 growth to forcing
          !
          NDAS   = NDAS + 1
         
                
          IF (PLANTING % NFORCING .GE. 2) THEN  !
          NDOF = TIMDIF(YRPLT, PLANTING % ForcingYRDOY) -ROOTINGTIME   !NDOF es el tiempo desde la siembra hasta el forzamiento (aplicación química)
                                                                       !pero por alguna razón suma el tiempo que se lleva en puntas de raíces blancas
                                                                       !por esa razón en CASE(11) cree una variable que se llama ROOTINGTIME que simplemente
                                                                       !calcula los días que se demoró en producir puntas de raíces blancas para poder restar
                                                                       !esos días aquí, y que la fecha de forza reportada coincida con la fecha de aplicación química real de la forza.
            
        ENDIF
          
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
          ISTAGE = 5                    ! JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
          TBASE  = 10.00                ! Base temperature of 6.25 is used during forcing to sepals closed on youngest flowers
          SUMDTT = 0.0                  ! Cumulative GDD set to 0.0

!-----------------------------------------------------------------
      CASE (5)                          !JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
          !
          ! Stage 3 >> Forcing to sepals closed on youngest flowers ! JVJ Stage 6 >> Forcing to Open Heart 
          !
          IF (SUMDTT .LT. (P5)) THEN
             RETURN                       ! P2: GDD needed to complete this stage
          ENDIF

!         Ready for next stage
          STGDOY(ISTAGE) = YRDOY
          ISTAGE = 6                    !JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
          TBASE  = 10.0                 ! TBASE of 10.0 is used in this stage
          SUMDTT =  0.0                 ! Cumulative growing degree days set to 0.0      
          
          
        CASE (6)                        !JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
          !
          ! Stage 3 >> Forcing to sepals closed on youngest flowers  ! JVJ Stage 6 >> Open Heart to EarlyAnthesis 
          !
          IF (SUMDTT .LT. P6) THEN        !IF (SUMDTT .LT. P2) THEN
             RETURN                       ! P2: GDD needed to complete this stage
          ENDIF

!         Ready for next stage
          STGDOY(ISTAGE) = YRDOY
          ISTAGE = 7                    !JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
          TBASE  = 10.0                 ! TBASE of 10.0 is used in this stage
          SUMDTT =  0.0                 ! Cumulative growing degree days set to 0.0

     
!-----------------------------------------------------------------
        CASE (7)                        !JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
          !
          ! Stage 4 >> SCY to first open flower   ! JVJ Stage 7 >> Early Anthesis to Last Anthesis 
          !
          XSTAGE = 1.5+3.0*SUMDTT/P7      ! Used by CERES-MAIZE   XSTAGE = 1.5+3.0*SUMDTT/P3
          IF (SUMDTT .LT. P7) THEN        ! IF (SUMDTT .LT. P3) THEN
             RETURN                       ! P3: GDD needed to complete this stage
          ENDIF

!         Ready for next stage
          STGDOY(ISTAGE) = YRDOY
          ISTAGE = 8                    !JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
          TBASE  = 4.0                  ! Tbase of 4.0 is used in the stage
          SUMDTT = 0.0                  ! Cumulative growing degree days set to 0.0

!-----------------------------------------------------------------
        CASE (8)                        !JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
          !
          ! Stage 5 >> Fruit growth
          !
          XSTAGE = 4.5+5.5*SUMDTT/(P8*.8)      ! XSTAGE = 4.5+5.5*SUMDTT/(P4*.8)
          IF (SUMDTT .LT. (P8+(PLTPOP-8.0)*2.4*16.95)) THEN   ! IF (SUMDTT .LT. (P4+(PLTPOP-8.0)*2.4*16.95)) THEN
             RETURN                        ! P4: GDD needed to complete this stage
          ENDIF
          PMDATE = YRDOY                   ! Fruit harvest date FHDATE = YRDOY
          STGDOY(ISTAGE) = YRDOY

!         Ready for next stage
          ISTAGE = 9                  !JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
          TBASE  = 12.0

!-----------------------------------------------------------------
        CASE (9)                     !JVJ Value changed because 2 stages in vegetative phase and one stage in reproductive phase were included
          !
          ! Stage 6 >> Physiological maturity
          !
          XSTAGE = 4.5+5.5*SUMDTT/G1       ! XSTAGE = 4.5+5.5*SUMDTT/P5
          IF (SUMDTT .LT. (G1+P8)) THEN    !  IF (SUMDTT .LT. (P5+P4)) THEN   G1+P8
             RETURN
          ENDIF

!MOVE TO GROSUB          HBIOM  = BIOMAS                 ! Record biomass at fruit harvest date

          FHDATE = YRDOY                  ! physiological maturity date PMDATE = YRDOY 
          MDATE  = YRDOY                  ! Set MDATE to stop model
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
