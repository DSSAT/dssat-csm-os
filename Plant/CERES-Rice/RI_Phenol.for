C=======================================================================
C  RI_PHENOL, Subroutine
C
C  Determines phenological stage
C-----------------------------------------------------------------------
C  Revision history
C
C                 Written
C  08-07-1993 PWW Header revision and minor changes
C  08/29/2002 CHP/MUS Converted to modular format for inclusion in CSM.
C  02/19/2003 CHP Converted dates to YRDOY format
!  02/20/2012 CHP/US Modify temperature response
!  12/06/2016 CHP/US Add check for small LAI during grainfilling - triggers maturity
C=======================================================================

      SUBROUTINE RI_PHENOL (CONTROL, ISWITCH, 
     &    AGEFAC, BIOMAS, DAYL, LEAFNO, NSTRES, PHEFAC,   !Input
     &    PHINT, SDEPTH, SOILPROP, SRAD, SW, SWFAC,       !Input
     &    TGROGRN, TILNO, TMAX, TMIN, TWILEN, TURFAC,     !Input
     &    YRPLT,FLOODWAT, LAI,                            !Input
     &    CUMDTT, EMAT, ISDATE, PLANTS, RTDEP, YRSOW,     !I/O
     &    CDTT_TP, DTT, FERTILE, FIELD, ISTAGE,           !Output
     &    ITRANS, LTRANS, MDATE, NDAT, NEW_PHASE, P1, P1T,!Output
     &    P3, P4, SDTT_TP, SEEDNI, SI3, STGDOY, STNAME,   !Output
     &    STRCOLD, STRESSW, STRHEAT, SUMDTT, TAGE,        !Output
     &    TBASE, TF_GRO, TSGRWT, WSTRES, XSTAGE, XST_TP,  !Output
     &    SeedFrac, VegFrac)                              !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
      USE FloodModule    ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT  NONE 
      SAVE
      
      CHARACTER ISWWAT*1,ISWNIT*1,IDETO*1,PLME*1
      CHARACTER*10 STNAME(20)
      CHARACTER*78 MSG(10)

      INTEGER   L,I,L0,YRDOY,STGDOY(20),NOUTDO
      INTEGER DOY, DYNAMIC, EMAT, ICSDUR, IDUR1, ISIM, ISTAGE
      INTEGER ISDATE, ISM, ITDATE, ITRANS
      INTEGER LEAFNO, MDATE, NDAS, NDAT, NLAYR
      INTEGER YR, YRPLT, YRSIM, YRSOW

      REAL TOPTMX, TOPTD, TOPTDX, TDELAYMX, TMPNPI, TMPNFL, TMPXFL   ! NEW VARIABLE TO BE READ IN SPP FILE
      REAL AGEFAC, ATEMP, BIOMAS, CDTT_TP, CNSD1, CNSD2, CSD1, CSD2
      REAL CUMDEP, CUMDTT, CUMTMP, DTT, FERTILE
      REAL G4, G5, NSTRES, LAI
      REAL P1, P1T, P3, P4, P5, P8, P9, P2O, P2R
      REAL PHEFAC, PHINT, PLANTS, RTDEP
      REAL SDAGE, SDEPTH, SDTT_TP, SEEDNI, SIND, SRAD
      REAL STRESSW, SUMDTT, SUMDTT_2, SUMDTT_3, SUMDTT_4, SWFAC
      REAL TAGE, TBASE, TEMPM, TFERT, TGROGRN
      REAL TILNO, TMAX, TMIN, TMPDTT
      REAL TURFAC, WSTRES, XNTI, XSTAGE, XST_TP

      REAL SI1(6), SI2(6), SI3(6), SI4(6)
      REAL DLAYR(NL), LL(NL), SW(NL)

      LOGICAL FIELD, LTRANS, PI_TF, PRESOW, TF_GRO, NEW_PHASE, BUNDED

      REAL CUMHEAT
      REAL TWILEN,PDTT,RATEIN,STRCOLD,STRHEAT,TCANOPY,TD,THEAD
      REAL HARVMAT,TN,TSGRWT,TMPI
      REAL SWSD,TOPT,TNSOIL,TDSOIL         ! TOPT IN SPP FILE MAR17
      REAL TMSOIL,ACOEF,DAYL,TH,SUMHDTT

      REAL LAIX   !LOCAL VARIABLE

!     CHP/US added for P model
      REAL SeedFrac, VegFrac
       
!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

!     The variable "ISWITCH" is of type "SwitchType".
      TYPE (SwitchType) ISWITCH

!     The variable "SOILPROP" is of type "SoilType".
      TYPE (SoilType) SOILPROP
      
      TYPE (FloodWatType) FLOODWAT

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM
      BUNDED  = FLOODWAT  % BUNDED
      ISWWAT = ISWITCH % ISWWAT
      ISWNIT = ISWITCH % ISWNIT
      IDETO  = ISWITCH % IDETO

      LL    = SOILPROP % LL
      DLAYR = SOILPROP % DLAYR
      NLAYR = SOILPROP % NLAYR
      
      CALL YR_DOY (YRDOY,YR,DOY)

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      CALL YR_DOY (YRSIM,YR,ISIM)
      CALL StnameFill(STNAME)

      CALL RI_IPPHEN (CONTROL,                            !Input
     &    ATEMP, G4, G5, P1, P2O, P2R, P5, PLME, SDAGE)       !Output

      CALL PhaseInit(CNSD1, CNSD2, CSD1, CSD2, 
     &    CUMTMP, ICSDUR, IDUR1, NEW_PHASE)

      XSTAGE     = 0.1      
      STGDOY     = 9999999   
      STGDOY(14) = YRSIM    
      MDATE      = -99      
      NDAT       = 0        
      BUNDED  = .FALSE.    !ASSUMES START AS UPLAND FIELD
      SEEDNI     = 0.0      
      CUMDTT   = 0.0        
      SUMDTT   = 0.0        
      DTT      = 0.0        
      WSTRES = 1.0
      RATEIN = 0.0
      TOPT = 28.0  !/G4      ! IN SPP FILE MAR17
      TOPTMX = 36.0  !/G4    !IN SPP FILE MAR17
      TMPNPI = 15.0    !IN SPP FILE MAR17
      TMPNFL = 15.0
      TMPXFL = 28.0
      TOPTDX = 40.0
      TDELAYMX = 46.0

      LAIX = 0.0

      ! Initialze stress indices - FROM INPLNT
      DO I = 1, 6
         SI1(I) = 0.0
         SI2(I) = 0.0
         SI3(I) = 0.0
         SI4(I) = 0.0
      END DO

      P1T   = P1      
      TBASE = 9.0     !IN SPP FILE MAR17
      TAGE  = SDAGE   

      LTRANS = .FALSE.
      PRESOW = .TRUE. 
      CALL RiceInit(
     &    PLME, TAGE, YRDOY, YRPLT, YRSIM, YRSOW,         !Input
     &    FIELD, ITRANS, PRESOW, TF_GRO)                  !Output

      IF (IDETO .EQ. 'Y') THEN
        CALL GETLUN('OUTO', NOUTDO)
      ENDIF
     
      IF (ITRANS .EQ. 3) THEN
        ISTAGE = 1
      ELSE
        ISTAGE = 7
      ENDIF

      NEW_PHASE = .FALSE.

!! temp chp
!      write(3000,'(A)') 
!     & "   yrdoy  xstage icsdur     dtt    sind     tn    tmpi" //
!     &  "  idur1     lai"


!***********************************************************************
!***********************************************************************
!     Daily rate / integration calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
      NEW_PHASE = .FALSE.

      CNSD1  = CNSD1  + 1.0 - NSTRES
      CNSD2  = CNSD2  + 1.0 - AGEFAC
      CSD1   = CSD1   + 1.0 - SWFAC
      CSD2   = CSD2   + 1.0 - TURFAC
      ICSDUR = ICSDUR + 1
      TEMPM = (TMAX + TMIN)*0.5
!     CHP/US 4/03/2008  P model
      SeedFrac = 0.0
      VegFrac  = 0.0

      IF (LAI > LAIX) LAIX = LAI

!-----------------------------------------------------------------------
!     Transplant date
!      IF (DOY .EQ. ITDATE) THEN
      IF (YRDOY .EQ. YRPLT) THEN
        FIELD = .TRUE.
        SELECT CASE (ITRANS)
          CASE (1);    STGDOY(07) = YRDOY 
          CASE (4)
            STGDOY(10) = YRDOY
            ISTAGE     = 8        !GERMINATION
            CALL PhaseInit(CNSD1, CNSD2, CSD1, CSD2, 
     &        CUMTMP, ICSDUR, IDUR1, NEW_PHASE)

            !FROM PHASEI
            ISTAGE = 9            !EMERGENCE
            P9 = 10.0*SDEPTH + 20.0
            SUMDTT = SUMDTT - P8
            !END PHASEI STUFF

           CASE (2,3)
             CALL TRNSPL_PHENOL ( 
     &        ATEMP, ITRANS, P1T, SDEPTH, TAGE, TBASE,        !Input
     &        CDTT_TP, CUMDTT, ISTAGE, P1, P8, P9, SDTT_TP,   !Output
     &        SUMDTT, XSTAGE, XST_TP)                         !Output

             STGDOY(11) = YRDOY
             CALL YR_DOY(YRPLT, YR, ITDATE)
             IF (ITDATE .GT. 200) THEN        
               TAGE = TAGE + (ITDATE - 200)   
             ENDIF                            
             LTRANS     = .TRUE. 
             TF_GRO     = .TRUE.
             NDAT       = 0
             IF (ITRANS .EQ. 3 .AND. SUMDTT .GT. P1) THEN
               CALL PhaseInit(CNSD1, CNSD2, CSD1, CSD2, 
     &           CUMTMP, ICSDUR, IDUR1, NEW_PHASE)

               !FROM PHASEI
               ISTAGE = 2         !PAN INIT
               SIND   = 0.0
               PI_TF  = .FALSE.
               !END PHASEI STUFF

             ENDIF
         END SELECT
      ENDIF

!-----------------------------------------------------------------------
      IF (TF_GRO) THEN
         !TOPT = TOPT            !  MADE TOPT -- CROP SPECIFIC G4 > 1 FOR HEAT SENSITIVE CUL Mar17
         !TOPTMX = TOPTMX
         !tmax = 55.0   ! high temp test mar17
          !TMIN = 32.0 !
         IF (TMAX .LT. TBASE) THEN
           DTT = 0.0
         !ELSEIF (TMIN .GT. TOPT) THEN
         !  DTT = TOPT - TBASE
           !
           ! Now, modify TMIN, TMAX based on soil conditions
           !
         ELSEIF (LEAFNO .LE. 10) THEN
           !
           ACOEF  = 0.01061 * SRAD + 0.5902
           TDSOIL = ACOEF * TMAX + (1.0 - ACOEF) * TMIN
           TNSOIL = 0.36354 * TMAX + 0.63646 * TMIN
           IF (TDSOIL .LT. TBASE) THEN
             DTT = 0.0
           ELSE
             IF (TNSOIL .GT. TOPT) THEN
                 TNSOIL = TOPT - (TNSOIL - TOPT)
             ENDIF
             IF (TDSOIL .LE. 40.0) THEN
               IF (TDSOIL .GT. TOPT) THEN
                 TDSOIL = TOPT
               ENDIF
             ELSE
                 TDSOIL = TOPT - (TDSOIL - 40.0)  
                 ! Delay in development (Snyder & Gesch) 
             ENDIF
             TMSOIL = TDSOIL*(DAYL/24.)+TNSOIL*((24.-DAYL)/24.)
             IF (TMSOIL .LT. TBASE) THEN
                 DTT = (TBASE+TDSOIL)/2.0 - TBASE
             ELSEIF (TMSOIL .GT. TDELAYMX) THEN
                 DTT = 0.0
             ELSEIF (TMSOIL .GT. TOPT .AND. TMSOIL .LE. TOPTMX) THEN
                 TMSOIL = TOPT 
                 DTT = TMSOIL - TBASE
             ELSEIF (TMSOIL .GT. TOPTMX .AND. TMSOIL .LE. TDELAYMX) THEN
                 TMSOIL = TOPT - (TMSOIL - TOPT)
                 DTT = TMSOIL - TBASE
             ELSEIF (TMSOIL .GE. TBASE .AND. TMSOIL .LE. TOPT) THEN     
                 DTT = TMSOIL - TBASE
             ENDIF
               !
               ! Statement added ... GoL and LAH, CIMMYT, 1999
               !
             DTT = AMIN1 (DTT,TOPTMX-TBASE)
           ENDIF

         !Now, compute DTT for when Tmax or Tmin out of range
         ELSEIF (TMIN .LT. TBASE .OR. TMAX .GT. TOPT) THEN
           DTT = 0.0
           DO I = 1, 24
             TH = (TMAX+TMIN)/2. + (TMAX-TMIN)/2. * SIN(3.14/12.*I)
             IF (TH .LT. TBASE .OR. TH .GT. TDELAYMX) THEN
             TH = TBASE
             ENDIF
             IF (TH .GT. TOPTMX) THEN
                TH = TOPT - (TH - TOPT)  !Development delay VARIABLE FOR SPP FILE MAR17
             ELSEIF (TH .GT. TOPT .AND. TH .LE. TOPTMX) THEN
                TH = TOPT
             ENDIF
             DTT = DTT + (TH-TBASE)/24.0
           END DO
         ELSE
           DTT = (TMAX+TMIN)/2.0 - TBASE
         ENDIF
         ! DROUGHT STRESS APPLIES ONLY TO LOWLAND RICE  MAR17????
         ! BUNDED IS USED AS SURROGATE FOR UPLAND - ECOTYPE FILE
         IF (PHEFAC .LT. 1.0 .AND. BUNDED) THEN   ! MAR17
             TMPDTT = DTT
            IF (ISTAGE .EQ. 2 .OR. ISTAGE .EQ. 1) THEN
                DTT    = AMIN1 ((DTT * PHEFAC),TMPDTT)
            ELSEIF(ISTAGE .EQ. 3) THEN
                DTT    = AMIN1 ((DTT * PHEFAC**1.33),TMPDTT)
            ELSEIF (ISTAGE .GT. 3 .AND. ISTAGE .LT. 6) THEN
                DTT = DTT * AMIN1 (1.5,(1.5 - PHEFAC))   !need to check mar17
                DTT = AMAX1 (DTT, TMPDTT)
            ENDIF
         ENDIF
         SUMDTT = SUMDTT + DTT
         CUMDTT = CUMDTT + DTT
         IF (ICSDUR .EQ. 0) THEN
             ICSDUR = 1
         ENDIF
      ENDIF

!-----------------------------------------------------------------------
!     Determine if new stage is reached today
      SELECT CASE (ISTAGE)
!-----------------------------------------------------------------------
        CASE (7)  !SOWING
          ! Determine sowing date
          STGDOY(ISTAGE) = YRDOY
          IF (ITRANS .EQ. 2 .OR. ITRANS .EQ. 4) THEN
             TF_GRO = .TRUE.
          ENDIF

          CALL PhaseInit(CNSD1, CNSD2, CSD1, CSD2, 
     &        CUMTMP, ICSDUR, IDUR1, NEW_PHASE)

          !FROM PHASEI
          ISTAGE = 8      !GERMINATION
          P8     = 150.0*EXP(-0.055*TEMPM)
          P8     = AMIN1 (P8,85.0)
          P8     = AMAX1 (P8,28.0)
          SUMDTT = 0.0
          CUMDTT = 0.0
          NDAS   = 0
          !END OF PHASEI STUFF

          L0 = 1
          IF (ISWWAT .EQ. 'Y') THEN
            CUMDEP = 0.0
            DO L = 1, NLAYR
              CUMDEP = CUMDEP + DLAYR(L)
              IF (SDEPTH .LT. CUMDEP) EXIT
            END DO
            L0 = L
          ENDIF
          RETURN

!-----------------------------------------------------------------------
        CASE (8)  !GERMINATION
          ! Determine germination date
          SWSD = 1.0                                 ! Default (PW)
          IF (ISWWAT .NE. 'N') THEN
             IF (ISWWAT .EQ. 'Y' .OR. SW(L0) .LE. LL(L0)) THEN
                SWSD = (SW(L0)-LL(L0))*0.65+(SW(L0+1)-LL(L0+1))*0.35
             ENDIF
          ENDIF

          NDAS = NDAS + 1
          
          IF (NDAS .LT. 40) THEN
             IF (ITRANS .EQ. 2) THEN
                SWSD = 1.0
             ENDIF
             IF (SWSD .LT. 0.02) THEN
                 SUMDTT = 0.               
           ! Heat unit for germination accumulated only when soil 
           !   is moist  - US Feb04
                RETURN
             ENDIF
      !   
      !     Temperature extremes removed
      !        IF (TEMPM .LT. 15.0 .OR. TEMPM .GT. 42.0) THEN
      !           RETURN
      !        ENDIF
             IF (SUMDTT .LT. P8) THEN
                RETURN
             ENDIF
             STGDOY(ISTAGE) = YRDOY
           ELSE
             !FAILURE TO GERMINATE
             ISTAGE  = 5      !END MN FIL
             PLANTS  = 0.0
             HARVMAT = SUMDTT
          ENDIF
           CALL PhaseInit(CNSD1, CNSD2, CSD1, CSD2, 
     &        CUMTMP, ICSDUR, IDUR1, NEW_PHASE)

          SELECT CASE (ISTAGE)
          CASE (5)        !FAILURE TO GERMINATE
            ISTAGE = 6    !MATURITY

          CASE (8)        !GERMINATION
            !FROM PHASEI
            ISTAGE = 9    !EMERGENCE
            P9     = 10.0*SDEPTH + 20.0
            SUMDTT = SUMDTT - P8
            !END OF PHASEI STUFF
          END SELECT
          RETURN

!-----------------------------------------------------------------------
        CASE (9)          !EMERGENCE
          ! Determine seedling emergence date
          RTDEP = RTDEP + 0.15*DTT    !ROOTGR 
          IF (SUMDTT .LT. P9) THEN
             RETURN
          ENDIF
          STGDOY(ISTAGE) = YRDOY

           CALL PhaseInit(CNSD1, CNSD2, CSD1, CSD2, 
     &        CUMTMP, ICSDUR, IDUR1, NEW_PHASE)

          !FROM PHASEI
          ISTAGE = 1          !END JUV. PH.
          SUMDTT = SUMDTT - P9
          TBASE  = 9.0    !PHENOL
          !RWID   = 0.023 !not used
          !END OF PHASEI STUFF

          IF (P9 .GT. 150.0) THEN
            IF (PRESOW) THEN
              MSG(1)='Seed ran out of metabolite due to deep planting.'
              CALL WARNING(1, "RIPHEN", MSG)
              IF (IDETO .EQ. 'Y') THEN
                WRITE (NOUTDO,'(A78)') MSG(1)
              ENDIF
            ENDIF
          ENDIF
          RETURN

!-----------------------------------------------------------------------
        CASE (1)      !END JUV
          ! Determine end of juvenile stage
          IF (LTRANS .AND. TF_GRO) THEN
             NDAT = NDAT + 1
          ENDIF
          XSTAGE = SUMDTT/P1
          IDUR1  = IDUR1 + 1
          
		!VegFrac = SUMDTT / (P1 + 7. * (TOPT - TBASE) + P3)
          VegFrac = xstage/4.5
   !   write(98,*) 'day',yrdoy,'xst=',xstage/10,'new',xstage/4.5
          IF (SUMDTT .LT. P1) THEN
             RETURN
          ENDIF
          STGDOY(ISTAGE) = YRDOY

          CALL RI_Stress (ISTAGE, ISWWAT, ISWNIT,
     &      CNSD1, CNSD2, CSD1, CSD2, ICSDUR,  
     &      SI1, SI2, SI3, SI4)

          !FROM PHASEI
          ISTAGE = 2          !PAN INIT
          SIND   = 0.0
          PI_TF  = .FALSE.
          !END OF PHASEI STUFF

!-----------------------------------------------------------------------
        CASE (2)      !PAN INIT
          ! Determine date of panicle initiation
          IF (LTRANS .AND. TF_GRO) THEN
             NDAT = NDAT + 1
          ENDIF
          XSTAGE = MAX(xstage, 1.0 + 0.5*SIND)
          PDTT   = DTT
          !IF (ISWWAT .NE. 'Y') THEN
          !   ICSDUR = ICSDUR + 1        
          !ENDIF
          IF (ICSDUR .EQ. 1 .AND. SUMDTT .GT. P1) THEN
             PDTT = SUMDTT - P1
          ENDIF
          TN     = (0.25*TMAX)+(0.75*TMIN)
          IF (LEAFNO .LE. 10) THEN
             TN = TNSOIL
          ENDIF
          CUMTMP = CUMTMP + TN
          IF (TWILEN .GT. P2O) THEN
          !   RATEIN = 1.0/(136.0+P2R*(HRLT-P2O))
             RATEIN = 1.0/(136.0+P2R*(TWILEN-P2O))
          ELSE
             RATEIN = 1.0 / 136.0
          ENDIF

!          IF (ITRANS .NE. 1 .AND. DOY .EQ. ITDATE) THEN
          IF (ITRANS .NE. 1 .AND. YRDOY .EQ. YRPLT) THEN
             SIND = (SUMDTT-P1)*RATEIN
             IF (SIND .GT. 1.0) THEN
                MSG(1) = 'Seedlings were transplanted too late.'
                CALL WARNING(1, "RIPHEN", MSG)
                IF (IDETO .EQ. 'Y') THEN
                   WRITE (NOUTDO,'(A78)') MSG(1)
                ENDIF
             ENDIF
           ELSE
             SIND = SIND + RATEIN*PDTT
          ENDIF
          ! P Model
          !VegFrac = MAX(VegFrac, SUMDTT/(P1+7.*(TOPT - TBASE)+P3))
          VegFrac = MAX(VegFrac, XSTAGE/4.5)    
		IF (SIND .LT. 1.0) THEN
             RETURN
          ENDIF
          TMPI = CUMTMP/(ICSDUR)   !*G5
          ! Check if night temp (NT) was below 15 C during this stage
          !
          IF (.NOT. PI_TF) THEN
             IF ((TN .GT. TMPNPI*G5 .AND. IDUR1 .GE. 1) 
!             Additional criteria of SIND > 3 added 1/11/2017
!               by CHP and RMO - needs to blessed by Upendra!!
!           Slow rice progression with continuous addition of biomass
!              under cool, but not freezing conditions. This causes 
!              development to progress after a long vegetative phase.
     &            .OR. SIND > 2.0) THEN
                PI_TF    = .TRUE.
              ELSE
                PI_TF    = .FALSE.
                IDUR1 = 0
             ENDIF
             IF (TMPI .LT. TMPNPI*G5) THEN  ! AVERAGE TN DURING PI < 15 (MAR17 TMPNPI IN SPP FILE)
                IF (TN .GT. TMPNPI*G5) THEN
                   IDUR1 = IDUR1 + 1  !CHECKING TO SEE IF 2 CONSEC DAYS WITH TN >TMPNPI (15Oc)
                ENDIF

!! temp chp
!      write(3000,3000) yrdoy, xstage, icsdur, dtt,sind,tn,tmpi,idur1,lai
! 3000 format(i8,f8.3,i7,2f8.3,f7.2,f8.3,I7,f8.3)

                RETURN
             ENDIF

             ! NOTE: If night temp > 15 for 2 successive days 
             ! after daylength requirement for PI_TF had been met
             ! then allow plant to reaach PI_TF
          ENDIF

          STGDOY(ISTAGE) = YRDOY
          CUMHEAT = 0.0
          STRCOLD = 1.0
          STRHEAT = 1.0
          SUMHDTT = 0.

          CALL RI_Stress (ISTAGE, ISWWAT, ISWNIT,
     &      CNSD1, CNSD2, CSD1, CSD2, ICSDUR,  
     &      SI1, SI2, SI3, SI4)

          !FROM PHASEI
          ISTAGE = 3          !HEADING
         ! P3     = 374.0 
          P4     = 120.0                        ! 120.0 MAKE THIS SPP MAR17
          XNTI   = SUMDTT/PHINT
          P3     = 5.5*PHINT+0.135*SUMDTT      !SPP FILE
          SUMDTT_2 = SUMDTT 
          !VegFrac = MAX(VegFrac,SUMDTT_2 / (SUMDTT_2 + P3))
          VegFrac = MAX(VegFrac, XSTAGE/4.5)   
    !  write(98,*) 'day',yrdoy,'xst=',xstage/10,'new',xstage/4.5
          SUMDTT = 0.0       
		! Need a check to ensure sumdtt is close to P1 + (1/ratein)
          !END OF PHASEI STUFF

!-----------------------------------------------------------------------
        CASE (3)          !HEADING
          ! Determine heading and end of leaf growth
          IF (LTRANS .AND. TF_GRO) THEN
             NDAT = NDAT + 1
          ENDIF

          XSTAGE = MAX(XSTAGE, 1.5 + 3.0*SUMDTT/P3)

          IF (SUMDTT .GT. 0.65*P3) THEN
             TD      = 0.75*TMAX+0.25*TMIN
             CUMHEAT = CUMHEAT + TD
             IDUR1   = IDUR1   + 1
             !
             ! HEENAN & LEWIS,'81 J.AUST.INST.AG.SC.47:118
             !
             TCANOPY = 0.87 + 0.92*TMIN
       !WRITE(*,*)'TD H TC CT 15G5',TD,CUMHEAT,TCANOPY,CUMTMP,TMPNFL*G5
   
             IF (TCANOPY .LE. TMPNFL*G5) THEN    !TMPMNFL CRITICAL LOW TEMP FOR STERILITY SPP FILE MAR17
                CUMTMP = CUMTMP + (TMPNFL*G5-TCANOPY)
             ENDIF
          ENDIF

!          IF (ITRANS .EQ. 2 .AND. DOY .EQ. ITDATE) THEN
          IF (ITRANS .EQ. 2 .AND. YRDOY .EQ. YRPLT) THEN
             P3 = P3 + (P1-P1T)
             MSG(1) = 'Seedlings were transplanted too late.'
             CALL WARNING(1, "RIPHEN", MSG)
             IF (IDETO .EQ. 'Y') THEN
                WRITE (NOUTDO,'(A78)') MSG(1)
             ENDIF
          ENDIF
          !  P model

          !VegFrac = MAX(VegFrac,(SUMDTT+SUMDTT_2) / (SUMDTT_2+P3))
          VegFrac = MAX(VegFrac, XSTAGE/4.5)   
   !   write(98,*) 'day',yrdoy,'xst=',xstage/10,'new',xstage/4.5


          IF (SUMDTT .LT. P3) THEN
             RETURN
          ENDIF

          ISDATE = YRDOY
          !
          ! Effect of extreme temperature on spikelet number
          !
          STRCOLD = 1.0-(0.01*CUMTMP)**1.667
          THEAD   = (CUMHEAT/IDUR1)                      
          IF (THEAD .GE. TMPXFL/G4) THEN          ! MAKE SPP FILE
             STRHEAT = 1.0 - 0.1*(THEAD-TMPXFL/G4)  !0.85 - 0.1
             !WRITE(*,*)'IN TH>28',THEAD,TMPXFL/G4,STRHEAT
             !PAUSE
          ENDIF
          !WRITE(*,*)'STRC THD STRH',STRCOLD,THEAD,STRHEAT,TMPXFL/G4
          !WRITE(*,*)'MEAN TEMP',(TMAX+TMIN)/2.0
          !PAUSE
          STRHEAT = AMIN1 (STRHEAT,1.0)
          STRHEAT = AMAX1 (STRHEAT,0.0)
          STRCOLD = AMIN1 (STRCOLD,1.0)
          STRCOLD = AMAX1 (STRCOLD,0.0)

          STGDOY(ISTAGE) = YRDOY

          IF (BIOMAS*PLANTS .LE. 1.0) THEN
             !PLANT FAILURE
             ISTAGE = 5       !END MN FIL
             PLANTS = 0.0

             IF (PRESOW) THEN
                CALL YR_DOY(YRDOY, YR, DOY)
                WRITE(MSG(1),'("Day ",I4,1X,I3,":")') YR, DOY 
                MSG(2) = 'Crop failure because of severe stress.'
                CALL WARNING(2, "RIPHEN", MSG)
                IF (IDETO .EQ. 'Y') THEN
                   WRITE (NOUTDO,'(/,A78,/,A78,/)') MSG(1), MSG(2)
                ENDIF
             ENDIF

            CALL RI_Stress (ISTAGE, ISWWAT, ISWNIT,
     &        CNSD1, CNSD2, CSD1, CSD2, ICSDUR,  
     &        SI1, SI2, SI3, SI4)

            !FROM PHASEI
             ISTAGE = 6      !MATURITY
            !END OF PHASEI STUFF

          ELSE

            CALL RI_Stress (ISTAGE, ISWWAT, ISWNIT,
     &        CNSD1, CNSD2, CSD1, CSD2, ICSDUR,  
     &        SI1, SI2, SI3, SI4)

          !FROM PHASEI
          ISTAGE = 4      !BEGIN GRAIN FILLING
	    SUMDTT_3 = SUMDTT + SUMDTT_2
          !VegFrac = MAX(VegFrac,(SUMDTT_3) / (SUMDTT_2+P3+P4))
          VegFrac = 1.0
          SUMDTT = SUMDTT - P3
          !END OF PHASEI STUFF

          ENDIF

!-----------------------------------------------------------------------
        CASE (4)      !BEGIN GRAIN FILLING
          ! Determine beginning of effective grain filling period
          XSTAGE = 4.5 + 1.5*SUMDTT/(P5*0.90)
          IF (LTRANS .AND. TF_GRO) THEN
             NDAT = NDAT + 1
          ENDIF
          IDUR1  = IDUR1  + 1
          CUMTMP = CUMTMP + TEMPM
	    ! Using VegFrac of 1.0
          !VegFrac = MAX(VegFrac,(SUMDTT + SUMDTT_3) / (SUMDTT_3+P4))
          SeedFrac = SUMDTT / P5
	    VegFrac = 1.0
          IF (SUMDTT .LT. P4) THEN       !   P4=170
             RETURN
          ENDIF

          !TFERT  = (CUMTMP/IDUR1)*G4
          !IF (TFERT .GT. TMPNFL*G5 .AND. TFERT .LT. TMPXFL/G4) THEN
             FERTILE = 1.0
           !ELSEIF (TFERT .GE. TMPXFL/G4) THEN
             !FERTILE = 0.85 - 0.1*(TFERT-TMPXFL/G4)   !MAR17
           !ELSEIF (TFERT .LE. TMPNFL*G5) THEN
            ! FERTILE = 0.85 - 0.1*(TMPNFL*G5-TFERT)
          !ENDIF

          FERTILE = AMAX1 (FERTILE,0.0)
          !!
          !! Modify grain number as function of temp
          !!
          WSTRES = 1.0
          IF (ISWWAT .EQ. 'Y') THEN
             WSTRES = 1.0 - (CSD1/ICSDUR)
          ENDIF
          WSTRES = AMAX1 (WSTRES,0.0)

          STGDOY(ISTAGE) = YRDOY

          CALL RI_Stress (ISTAGE, ISWWAT, ISWNIT,
     &      CNSD1, CNSD2, CSD1, CSD2, ICSDUR,  
     &      SI1, SI2, SI3, SI4)

          !FROM PHASEI
          ISTAGE = 5      !END MN FIL
	    SUMDTT_4 = SUMDTT
          SUMDTT   = SUMDTT - P4

          STRESSW = AMAX1 (SI2(3),SI2(4))
!          
          EMAT   = 0
          ISM    = 0
!          !END OF PHASEI STUFF

!-----------------------------------------------------------------------
        CASE (5)      !END MN FIL
          ! Determine end of main culm grain filling
          XSTAGE = 6.0 + 4.0*SUMDTT/P5
          IF (LTRANS .AND. TF_GRO) THEN
             NDAT = NDAT + 1
          ENDIF
          IDUR1  = IDUR1  + 1
          CUMTMP = CUMTMP + TEMPM
          ! P MODEL
		SeedFrac = AMIN1 (1.0, (SUMDTT + SUMDTT_4) / P5)
	    VegFrac = 1.0

!         Check for LAI > small number, continue season, otherwise maturity triggered
          IF (LAI > 1.E-4) THEN
            IF (SUMDTT .LT. 0.90*P5) THEN
               RETURN
            ENDIF
          ENDIF

		SeedFrac = AMIN1 (1.0, (SUMDTT + SUMDTT_4) / P5)
          VegFrac = 1.0
          IF (ISM .LE. 0) THEN
             STGDOY (ISTAGE) = YRDOY
             HARVMAT = P5 + 40.0
             IF (ITRANS .EQ. 1 .OR. ITRANS .EQ. 4) THEN
                HARVMAT = SUMDTT
             ENDIF
             ISM = 1
             RETURN
          ENDIF
          !
          ! Determine end of tiller grain filling
          !
          IF (DTT .LT. 0.001 .OR. TILNO .LE. 1.0) THEN
             HARVMAT = SUMDTT
          ENDIF
          IF (LTRANS .AND. TF_GRO) THEN
             NDAT = NDAT + 1
          ENDIF
          IF (SUMDTT .LT. HARVMAT) THEN
             IF (TGROGRN .GT. 0.12) THEN
                RETURN
             ENDIF
             EMAT = EMAT + 1
             IF (EMAT .LT. 2) THEN
                RETURN
             ENDIF
          ENDIF
          STGDOY(12) = YRDOY
          TSGRWT = (CUMTMP/IDUR1)   ! NO CULTIVAR EFFECT

          CALL RI_Stress (ISTAGE, ISWWAT, ISWNIT,
     &      CNSD1, CNSD2, CSD1, CSD2, ICSDUR,  
     &      SI1, SI2, SI3, SI4)

!          !FROM PHASEI
           ISTAGE   =  6      !MATURITY
	     SeedFrac = 1.0
!          !END OF PHASEI STUFF

!-----------------------------------------------------------------------
        CASE (6)      !MATURITY
          ! Determine physiological maturity
          STGDOY (ISTAGE) = YRDOY
          MDATE  = YRDOY

          CALL RI_Stress (ISTAGE, ISWWAT, ISWNIT,
     &      CNSD1, CNSD2, CSD1, CSD2, ICSDUR,  
     &      SI1, SI2, SI3, SI4)

          !FROM PHASEI
          ISTAGE = 20      !HARVEST
          CUMDTT = 0.0
          DTT    = 0.0
          !END OF PHASEI STUFF
          STGDOY(ISTAGE) = YRDOY

      END SELECT

!-----------------------------------------------------------------------

      CALL PhaseInit(CNSD1, CNSD2, CSD1, CSD2, 
     &        CUMTMP, ICSDUR, IDUR1, NEW_PHASE)

      IF (ISTAGE .EQ. 6 .AND .ITRANS .EQ. 3) THEN
        TF_GRO = .FALSE.
      ENDIF

C-----------------------------------------------------------------------
C        Define dates for water balance calculations
C
C        YRSIM = Start of Simulation Date
C        YRPLT = Planting Date
C        EDATE = Emergence Date
C        MDATE = Maturity Date
C        YRDOY = Year - Day of Year (Dynamic Variable)
C-----------------------------------------------------------------------

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

      END SUBROUTINE RI_PHENOL
C=======================================================================


C=======================================================================
C  RI_Stress, Subroutine
C
C  Initialization at the beginning of every stage
C-----------------------------------------------------------------------
C  Revision history
C
C  05/07/2002 CHP Written
C-----------------------------------------------------------------------
C  Called : RI_PHENOL
C=======================================================================

      SUBROUTINE RI_Stress (ISTAGE, ISWWAT, ISWNIT,
     &    CNSD1, CNSD2, CSD1, CSD2, ICSDUR,  
     &    SI1, SI2, SI3, SI4)

      IMPLICIT NONE
      CHARACTER*1 ISWNIT, ISWWAT
      INTEGER ICSDUR, ISTAGE
      REAL CNSD1, CNSD2, CSD1, CSD2
      REAL SI1(6), SI2(6), SI3(6), SI4(6)

!-----------------------------------------------------------------------
!     Initialize at the beginning of each phase

        IF (ISWWAT .EQ. 'Y') THEN
           SI1(ISTAGE) = CSD1/ICSDUR
           SI2(ISTAGE) = CSD2/ICSDUR
        ELSE
           SI1(ISTAGE) = 0.0
           SI2(ISTAGE) = 0.0
        ENDIF

        IF (ISWNIT .EQ. 'Y') THEN
           SI3(ISTAGE) = CNSD1/ICSDUR
           SI4(ISTAGE) = CNSD2/ICSDUR
        ELSE
           SI3(ISTAGE) = 0.0
           SI4(ISTAGE) = 0.0
        ENDIF

      RETURN
      END SUBROUTINE RI_Stress
C=======================================================================


C=======================================================================
C  PhaseInit, Subroutine
C
C  Initialization at the beginning of every stage
C-----------------------------------------------------------------------
C  Revision history
C
C  05/07/2002 CHP Written
C-----------------------------------------------------------------------
C  Called : RI_PHENOL
C=======================================================================

      SUBROUTINE PhaseInit (CNSD1, CNSD2, CSD1, CSD2, 
     &    CUMTMP, ICSDUR, IDUR1, NEW_PHASE)

      IMPLICIT NONE
      INTEGER ICSDUR, IDUR1
      REAL CNSD1, CNSD2, CSD1, CSD2, CUMTMP
      LOGICAL NEW_PHASE

!-----------------------------------------------------------------------
!     Initialize at the beginning of each phase

      CNSD1   = 0.0
      CNSD2   = 0.0
      CSD1    = 0.0
      CSD2    = 0.0
      CUMTMP  = 0.0
      ICSDUR  = 0
      IDUR1   = 0
      NEW_PHASE = .TRUE.

      RETURN
      END SUBROUTINE PhaseInit
C=======================================================================


C=======================================================================
C  RiceInit, Subroutine
C
C  Seasonal initialization
C-----------------------------------------------------------------------
C  Revision history
C
C  05/07/2002 CHP Written
C  02/19/2003 CHP Converted dates to YRDOY format
C-----------------------------------------------------------------------
C  Called : RI_PHENOL
C=======================================================================
      SUBROUTINE RiceInit(
     &    PLME, TAGE, YRDOY, YRPLT, YRSIM, YRSOW,         !Input
     &    FIELD, ITRANS, PRESOW, TF_GRO)                  !Output

      IMPLICIT NONE

      CHARACTER*1 PLME
      INTEGER ITRANS, YRPLT
      INTEGER YRDOY, YRSIM, YRSOW, INCDAT
      REAL TAGE 

      LOGICAL FIELD, TF_GRO, PRESOW
      
!     Is this correct? chp - this was done in IPFLOD
      IF (PLME .EQ. 'S' .AND. TAGE .GT. 0.0 .AND. TAGE .LT. 5.0) THEN
        PLME = 'P'
      ENDIF

!     NOTE: LTRANS REMOVED FROM INITIALIZATION HERE BECAUSE LOGIC IS 
!         HANDLED IN PHENOL MAIN ROUTINE.  LTRANS INITIALIZED TO FALSE
!         FOR ALL CASES.  SET TO TRUE ON TRANSPLANT DATE FOR 
!         ITRANS = 2 OR 3.

      SELECT CASE(PLME)
      CASE ('S')    
        ITRANS = 1                        !FROM IRRVAL
        !ITDATE = ISOW                     !FROM IPFLOD
        TF_GRO = .TRUE.                   !FROM IPFLOD
        FIELD  = .TRUE.                   !FROM IPFLOD

      CASE ('N')
        ITRANS = 2                        !FROM IRRVAL
        IF (YRDOY .NE. YRSIM) THEN      !CHP
          ! Allow N stress during nursery development
          PRESOW  = .FALSE.               !FROM INGROW
        ENDIF

!       No planting date specified in FILEX - set here
!       This needs to go in AUTPLT (?) chp
        IF (YRPLT .LE. 0) THEN
           IF (YRSOW .GT. 0) THEN
              YRPLT = INCDAT(YRSOW, IFIX(TAGE))
           ENDIF
        ELSE
           YRSOW  = INCDAT(YRPLT, -IFIX(TAGE))
        ENDIF

        TF_GRO = .TRUE.                   !FROM IPFLOD
        FIELD  = .FALSE.                  !FROM INGROW

      CASE ('T')
        ITRANS = 3                        !FROM IRRVAL
        FIELD  = .TRUE.                   !FROM IPFLOD

!       No planting date specified in FILEX - set here
!       This needs to go in AUTPLT (?) chp
        IF (YRPLT .LE. 0) THEN
           IF (YRSOW .GT. 0) THEN
              YRPLT = INCDAT(YRSOW, IFIX(TAGE))
           ENDIF
        ENDIF

        YRSOW   = YRPLT
        TF_GRO = .FALSE.                  !FROM IPFLOD

      CASE ('P')
        ITRANS = 4                        !FROM IRRVAL
        TF_GRO = .TRUE.                   !FROM IPFLOD
!       No planting date specified in FILEX - set here
!       This needs to go in AUTPLT (?) chp
        IF (YRPLT .LE. 0) THEN
           FIELD = .TRUE.                 !FROM IPFLOD
           IF (YRSOW .GT. 0) THEN
              YRPLT = INCDAT(YRSOW, IFIX(TAGE))
           ENDIF
        ELSE
           FIELD = .FALSE.                !FROM IPFLOD
           YRSOW  = INCDAT(YRPLT, -IFIX(TAGE))
        ENDIF

      CASE DEFAULT
        ITRANS = 3                        !FROM IRRVAL
        FIELD  = .TRUE.                   !FROM IPFLOD

!       No planting date specified in FILEX - set here
!       This needs to go in AUTPLT (?) chp
        IF (YRPLT .LE. 0) THEN
           IF (YRSOW .GT. 0) THEN
              YRPLT = INCDAT(YRSOW, IFIX(TAGE))
           ENDIF
        ENDIF

        YRSOW   = YRPLT
        TF_GRO = .FALSE.                  !FROM IPFLOD

      END SELECT

      RETURN
      END SUBROUTINE RiceInit

C=======================================================================

C=======================================================================
C  StnameFill, Subroutine
C
C  Initialization at the beginning of every stage
C-----------------------------------------------------------------------
C  Revision history
C
C  05/07/2002 CHP Written
C-----------------------------------------------------------------------
C  Called : RI_PHENOL
C=======================================================================

      SUBROUTINE StnameFill(STNAME)

      IMPLICIT NONE
      CHARACTER*10 STNAME(20)

      STNAME(1)  = 'End Juveni'
      STNAME(2)  = 'Pan Init  '
      STNAME(3)  = 'Heading   '
      STNAME(4)  = 'Beg Gr Fil'
      STNAME(5)  = 'End Mn Fil'
      STNAME(6)  = 'Maturity  '
      STNAME(7)  = 'Sowing    '
      STNAME(8)  = 'Germinate '
      STNAME(9)  = 'Emergence '
      STNAME(10) = 'Prgerm Sow'
      STNAME(11) = 'Transplant'
      STNAME(12) = 'End Ti Fil'
      STNAME(13) = '          '
      STNAME(14) = 'Start Sim '
      STNAME(15) = '          '
      STNAME(16) = '          '
      STNAME(17) = '          '
      STNAME(18) = '          '
      STNAME(19) = '          '
      STNAME(20) = 'Harvest   '

      RETURN
      END SUBROUTINE StnameFill

C=======================================================================
C  RI_IPPHEN, Subroutine
C
C  Reads FILEIO for RICE routine
C  05/07/2002 CHP Written
C  08/12/2003 CHP Added I/O error checking
C=======================================================================

      SUBROUTINE RI_IPPHEN (CONTROL,                      !Input
     &    ATEMP, G4, G5, P1, P2O, P2R, P5, PLME, SDAGE)       !Output

      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT     NONE

      CHARACTER*1  PLME
      CHARACTER*6  ERRKEY, SECTION
      PARAMETER (ERRKEY = 'IPRICE')
      CHARACTER*30 FILEIO
      INTEGER LINC, LNUM, LUNIO, ERR, FOUND
      REAL ATEMP, G4, G5, P1, P2R, P5, P2O, SDAGE

C     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL
      FILEIO = CONTROL % FILEIO
      LUNIO  = CONTROL % LUNIO

!-----------------------------------------------------------------------
!       Read data from FILEIO for use in PLANT module
!-----------------------------------------------------------------------
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
      LNUM = 0
C-----------------------------------------------------------------------
C    Read Planting Details Section
C-----------------------------------------------------------------------
      SECTION = '*PLANT'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEIO, LNUM)
      READ (LUNIO,'(35X,A1,30X,2(1X,F5.0))', IOSTAT=ERR) 
     &                PLME, SDAGE, ATEMP ; LNUM = LNUM + 1
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

      IF (PLME .EQ. 'T' .AND. ATEMP .LT. 0.0) THEN
         CALL ERROR (ERRKEY,10,FILEIO,LNUM)
      ENDIF

C-----------------------------------------------------------------------
C     Read crop genetic information
C-----------------------------------------------------------------------
      SECTION = '*CULTI'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEIO, LNUM)
      READ (LUNIO,100, IOSTAT=ERR) P1, P2R, P5, P2O, G4, G5; 
     & LNUM = LNUM + 1
!CHP  100 FORMAT (30X,4(F6.1),18X,F6.2)
  100 FORMAT (30X,4(F6.0),18X,1(F6.0),6X,F6.0)
      !write(*,*)'P1 P2R P5 P2O G4 G5',P1, P2R, P5, P2O, G4!, G5
      !pause
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

      CLOSE (LUNIO)
      RETURN
      END SUBROUTINE RI_IPPHEN
C=======================================================================

