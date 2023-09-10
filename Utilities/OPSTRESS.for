!=======================================================================
!  OpStress, Subroutine
!
!  Tracks weather parameters as a function of critical growth stages.
!     CONTROL is a required variable
!     Other variables are optional.
!     ISWITCH, PlantStres are sent from OPHARV routines
!     WEATHER sent from weather module
!     ET can be sent from SPAM routine, but it is not currently printed.
!-----------------------------------------------------------------------
!  Revision history
!  10/20/2005 CHP Written based on CROPGRO v3.5 routines SUMWTH and OPSTRS
!  01/07/2010 CHP Add irrigation water productivity to Overview and Summary
!  01/26/2010 CHP Add N productivity
!  02/22/2011 CHP Add environmental factors during growth season, incl. CO2
!  10/16/2020 CHP Cumulative "soil" evaporation includes mulch and flood evap
!-----------------------------------------------------------------------

      SUBROUTINE OPSTRESS(CONTROL, IDETO,   
     &    PlantStres, WEATHER, YIELD, BIOMAS)

      USE SumModule
      USE ModuleData
      IMPLICIT  NONE
      EXTERNAL GETLUN, SUMVALS
      SAVE

!-----------------------------------------------------------------------
      INTEGER, PARAMETER :: MaxStag = 5 !max # of stages output

!     Interface variables
      TYPE (ControlType), Intent(IN)           :: CONTROL
      CHARACTER*1,        Intent(IN), Optional :: IDETO
      TYPE (PlStresType), Intent(IN), Optional :: PlantStres
      TYPE (WeatherType), Intent(IN), Optional :: WEATHER
      INTEGER,            Intent(IN), Optional :: YIELD
      REAL,               Intent(IN), Optional :: BIOMAS

!     Local variables
      CHARACTER*1 IDETO_SAVE, RNMODE
      CHARACTER*2 CROP
      CHARACTER*12, PARAMETER :: OUTO = 'OVERVIEW.OUT'
      CHARACTER*23 STAG(0:MaxStag)

      INTEGER DYNAMIC, I, NOUTDO, NYRS, STTOT, NDCH

      REAL TMAX, TMIN, RAIN, DAYL, SRAD, CO2
      REAL N_phot, N_grow, W_phot, W_grow
      REAL P_grow, P_phot
      REAL ET_L, EP_L, EO_L, EVAP_L, TOTIR    !, ES_L
      REAL DMP_Rain, GrP_Rain, DMP_ET, GrP_ET, DMP_EP, GrP_EP 
      REAL DEPIR, DMP_Irr, GrP_Irr
      REAL DMP_NApp, GrP_NApp, DMP_NUpt, GrP_NUpt
      REAL CumNit
      REAL TMINA, TMAXA, SRADA, DAYLA, CO2A, PRCP, ETCP, ESCP, EPCP

      REAL, DIMENSION(0:MaxStag) :: N_photR, CETR, CEPR, DAYLR, CEVAPR    !, CESR 
      REAL, DIMENSION(0:MaxStag) :: N_growR, P_growR, P_photR
      REAL, DIMENSION(0:MaxStag) :: RAINR, RADR, CO2R
      REAL, DIMENSION(0:MaxStag) :: W_photR, TMAXR, TMINR, W_growR
      REAL, DIMENSION(0:MaxStag) :: TMEANR, CEOR
      INTEGER, DIMENSION(0:MaxStag) :: NNR, Ndays_RAIN
      INTEGER, DIMENSION(0:MaxStag) :: Ndays_LT0, Ndays_LT2, Ndays_GT30
      INTEGER, DIMENSION(0:MaxStag) :: Ndays_GT32, Ndays_GT34

!     Arrays which contain data for printing in SUMMARY.OUT file
      INTEGER, PARAMETER :: SUMNUM = 22
      CHARACTER*5, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

!     Need switch settings for water productivity section
      TYPE (SwitchType) ISWITCH

!-----------------------------------------------------------------------
      LOGICAL FIRST 
      DATA FIRST /.TRUE./

      IF (FIRST) THEN
        IDETO_SAVE = 'N'
        CALL GETLUN('OUTO', NOUTDO)
        FIRST = .FALSE.
      ENDIF

      DYNAMIC = CONTROL % DYNAMIC

!***********************************************************************
!***********************************************************************
!     SEASONAL INITIALIZATION
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      CROP   = CONTROL % CROP
      NYRS   = CONTROL % NYRS
      RNMODE = CONTROL % RNMODE

      IF (PRESENT(IDETO)) THEN
        IDETO_SAVE = IDETO
      ELSE
        IDETO_SAVE = 'N'
      ENDIF

      IF (PRESENT(PlantStres)) THEN
        STTOT = PlantStres % NSTAGES
        STAG  = PlantStres % StageName
      ELSE
        STTOT = 0
        STAG = '                       '
      ENDIF

!     Zero averages in arrays
      TMAXR = 0.0
      TMINR = 0.0
      TMEANR= 0.0
      RAINR = 0.0
      DAYLR = 0.0
      RADR  = 0.0
      CO2R  = 0.0
      CETR  = 0.0; CEPR = 0.0; CEOR = 0.0
      CEVAPR=0.0  !; CESR = 0.0
      W_growR = 0.0
      W_photR = 0.0
      N_growR = 0.0
      N_photR = 0.0
      P_growR = 0.0
      P_photR = 0.0

      NNR   = 0
      Ndays_RAIN = 0
      Ndays_LT0  = 0
      Ndays_LT2  = 0
      Ndays_GT30 = 0
      Ndays_GT32 = 0
      Ndays_GT34 = 0

      ET_L   = 0.0
      EP_L   = 0.0
!     ES_L   = 0.0
      EO_L   = 0.0
      EVAP_L   = 0.0

      W_grow = 1.0 
      W_phot = 1.0  
      N_grow = 1.0 
      N_phot = 1.0

      DMP_Rain = -99.
      GrP_Rain = -99.
      DMP_ET = -99.
      GrP_ET = -99.
      DMP_EP = -99.
      GrP_EP = -99.
      DMP_Irr = -99.    
      GrP_Irr = -99.

      DMP_NApp = -99.
      GrP_NApp = -99.
      DMP_NUpt = -99.
      GrP_NUpt = -99.

      NDCH  = -99 
      TMINA = -99. 
      TMAXA = -99. 
      SRADA = -99. 
      DAYLA = -99. 
      CO2A  = -99. 
      PRCP  = -99. 
      ETCP  = -99. 
      ESCP  = -99.
      EPCP  = -99.

!***********************************************************************
!***********************************************************************
!     RATE
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
!     If weather data is sent, routine was called by weather module and
!     data need to be accumulated.
      IF (PRESENT(WEATHER)) THEN
        TMAX = WEATHER % TMAX
        TMIN = WEATHER % TMIN
        RAIN = WEATHER % RAIN
        DAYL = WEATHER % DAYL
        SRAD = WEATHER % SRAD 
        CO2  = WEATHER % CO2
      ENDIF

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
!-----------------------------------------------------------------------
      CALL GET('SPAM','ET',ET_L)
      CALL GET('SPAM','EP',EP_L)
!     CALL GET('SPAM','ES',ES_L)
      CALL GET('SPAM','EO',EO_L)
      call GET('SPAM','EVAP',EVAP_L)

      IF (PRESENT(PlantStres)) THEN
        W_grow = PlantStres % W_grow  !was TURFAC
        W_phot = PlantStres % W_phot  !was SWFAC
        N_grow = PlantStres % N_grow  !was NSTRES
        N_phot = PlantStres % N_phot  !was AGEFAC
        P_grow = PlantStres % P_grow  !was PStres2
        P_phot = PlantStres % P_phot  !was PStres1

        CALL GET('MGMT', 'DEPIR', DEPIR)

        DO I = 0, PlantStres % NSTAGES
          IF (PlantStres % ACTIVE(I)) THEN
            NNR(I) = NNR(I) + 1

            TMAXR(I) = TMAXR(I) + TMAX
            TMINR(I) = TMINR(I) + TMIN
            TMEANR(I)= TMEANR(I)+ (TMAX + TMIN) / 2.0
            RAINR(I) = RAINR(I) + RAIN
!           CumIrr(I)= CumIrr(I)+ DEPIR
            DAYLR(I) = DAYLR(I) + DAYL
            RADR (I) = RADR (I) + SRAD 
            CO2R (I) = CO2R (I) + CO2 

            CETR(I)  = CETR(I)  + ET_L
            CEPR(I)  = CEPR(I)  + EP_L
!           CESR(I)  = CESR(I)  + ES_L
            CEVAPR(I)= CEVAPR(I)+ EVAP_L
            CEOR(I)  = CEOR(I)  + EO_L

            W_growR(I) = W_growR(I) + W_grow 
            W_photR(I) = W_photR(I) + W_phot  
            N_growR(I) = N_growR(I) + N_grow 
            N_photR(I) = N_photR(I) + N_phot
            P_growR(I) = P_growR(I) + P_grow
            P_photR(I) = P_photR(I) + P_phot

            IF (RAIN > 0.0) Ndays_RAIN(I) = Ndays_RAIN(I) + 1
            IF (TMIN < 0.0) Ndays_LT0(I)  = Ndays_LT0(I)  + 1
            IF (TMIN < 2.0) Ndays_LT2(I)  = Ndays_LT2(I)  + 1
            IF (TMAX > 30.) Ndays_GT30(I) = Ndays_GT30(I) + 1
            IF (TMAX > 32.) Ndays_GT32(I) = Ndays_GT32(I) + 1
            IF (TMAX > 34.) Ndays_GT34(I) = Ndays_GT34(I) + 1

          ENDIF
        ENDDO
      ENDIF

!***********************************************************************
!***********************************************************************
!     SEASONAL OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
!     Only process end of season data if sent from OPVIEW routine.
      IF (.NOT. PRESENT(YIELD)) RETURN

      IF (IDETO_SAVE .EQ. 'Y' .AND. STTOT > 0 .AND.
     &      CONTROL % ErrCode == 0) THEN
        WRITE(NOUTDO,500)
 500    FORMAT(//,'*ENVIRONMENTAL AND STRESS FACTORS',//,1X, 
     &'|-----Development Phase------|',38('-'),'Environment',38('-'),
     &'|',17('-'),'Stress',17('-'),'|',
     &/,30X,'|',15('-'),'Average',14('-'),'|-----Cumulative-----',
     &'|--------Count of days--------|',10X,'(0=Min, 1=Max Stress)',9X,
     &'|',
     &/,25X,'Time  Temp  Temp  Temp Solar Photop',16X,'Evapo    Pot',
     &' TMIN TMIN TMAX TMAX TMAX RAIN|----Water----|---Nitrogen--|',
     &'-Phosphorus-|',
     &/,25X,'Span   Max   Min  Mean   Rad  [day]    CO2   Rain  Trans ',
     &'    ET   <    <    >    >    >    >   Photo         Photo      ',
     &'   Photo',
     &/,25X,'days     C     C     C MJ/m2     hr    ppm     mm     mm ',
     &'   mm   0 C  2 C 30 C 32 C 34 C  0mm  synth Growth  synth ',
     &'Growth  synth Growth',/,160('-'))
      ENDIF

      IF (RNMODE .EQ. 'I' .AND. NYRS .LE. 1) THEN
        WRITE (*,500)
      ENDIF

      DO I = 0, STTOT
        IF (NNR(I) > 0) THEN
          TMAXR(I) = TMAXR(I)/ NNR(I)
          TMINR(I) = TMINR(I)/ NNR(I)
          TMEANR(I)= TMEANR(I)/NNR(I)
          DAYLR(I) = DAYLR(I)/ NNR(I)
          !RADTR(I) = RADR(I)
          RADR (I) = RADR(I) / NNR(I)
          CO2R (I) = CO2R(I) / NNR(I)

          W_growR(I) = 1. - (W_growR(I) / NNR(I))
          W_photR(I) = 1. - (W_photR(I) / NNR(I))
          N_growR(I) = 1. - (N_growR(I) / NNR(I))
          N_photR(I) = 1. - AMIN1(1.0, (N_photR(I) / NNR(I)))
          P_growR(I) = 1. - (P_growR(I) / NNR(I))
          P_photR(I) = 1. - (P_photR(I) / NNR(I))
        ENDIF

        IF (I == 0 .OR. NNR(I) == 0) CYCLE

        IF (IDETO_SAVE .EQ. 'Y' .AND. STTOT > 0 .AND.
     &      CONTROL % ErrCode == 0) THEN
          IF (RAINR(I) < 1000.) THEN
            WRITE(NOUTDO,600) STAG(I), NNR(I), TMAXR(I), TMINR(I), 
     &        TMEANR(I),
     &        RADR(I), DAYLR(I), CO2R(I), RAINR(I), CETR(I), CEOR(I), 
     &        Ndays_LT0(I), Ndays_LT2(I), Ndays_GT30(I), Ndays_GT32(I),
     &        Ndays_GT34(I), Ndays_RAIN(I), W_photR(I),
     &        W_growR(I), N_photR(I), N_growR(I), P_photR(I), P_growR(I)
! 600       FORMAT(1X,A23,I5,3F6.1,F7.2,3F7.1,6F7.3)
  600       FORMAT(1X,A23,I5,4F6.1,F7.2,4F7.1,6I5,6F7.3)
          ELSE
            WRITE(NOUTDO,610) STAG(I), NNR(I), TMAXR(I), TMINR(I), 
     &        TMEANR(I),
     &        RADR(I), DAYLR(I), CO2R(I), NINT(RAINR(I)), NINT(CETR(I)),
     &        NINT(CEOR(I)), 
     &        Ndays_LT0(I), Ndays_LT2(I), Ndays_GT30(I), Ndays_GT32(I),
     &        Ndays_GT34(I), Ndays_RAIN(I), W_photR(I),
     &        W_growR(I), N_photR(I), N_growR(I), P_photR(I), P_growR(I)
! 610       FORMAT(1X,A23,I5,3F6.1,F7.2,F7.1,2I7,6F7.3)
  610       FORMAT(1X,A23,I5,4F6.1,F7.2,F7.1,3I7,6I5,6F7.3)
          ENDIF
        ENDIF

        IF (RNMODE .EQ. 'I' .AND. NYRS .LE. 1) THEN
          IF (RAINR(I) < 1000.) THEN
            WRITE(*,600) STAG(I), NNR(I), TMAXR(I), TMINR(I), 
     &        RADR(I), DAYLR(I), RAINR(I), CETR(I), W_photR(I), 
     &        W_growR(I), N_photR(I), N_growR(I), P_photR(I), P_growR(I)
          ELSE
            WRITE(*,610) STAG(I), NNR(I), TMAXR(I), TMINR(I), 
     &        RADR(I), DAYLR(I), NINT(RAINR(I)), NINT(CETR(I)),
     &        W_photR(I), W_growR(I), N_photR(I), N_growR(I), 
     &        P_photR(I), P_growR(I)
          ENDIF
        ENDIF
      ENDDO

!     Print seasonal environment & stress summary from planting to harvest
      IF (IDETO_SAVE .EQ. 'Y' .AND. STTOT > 0 .AND.
     &      CONTROL % ErrCode == 0) THEN
        I = 0
        WRITE(NOUTDO,'(A)') "   "
        IF (RAINR(I) < 1000.) THEN
          WRITE(NOUTDO,600) STAG(I), NNR(I), TMAXR(I), TMINR(I), 
     &      TMEANR(I),
     &      RADR(I), DAYLR(I), CO2R(I), RAINR(I), CETR(I), CEOR(I), 
     &      Ndays_LT0(I), Ndays_LT2(I), Ndays_GT30(I), Ndays_GT32(I),
     &      Ndays_GT34(I), Ndays_RAIN(I), W_photR(I),
     &      W_growR(I), N_photR(I), N_growR(I), P_photR(I), P_growR(I)
        ELSE
          WRITE(NOUTDO,610) STAG(I), NNR(I), TMAXR(I), TMINR(I), 
     &      TMEANR(I),
     &      RADR(I), DAYLR(I), CO2R(I), NINT(RAINR(I)), NINT(CETR(I)),
     &      NINT(CEOR(I)), 
     &      Ndays_LT0(I), Ndays_LT2(I), Ndays_GT30(I), Ndays_GT32(I),
     &      Ndays_GT34(I), Ndays_RAIN(I), W_photR(I),
     &      W_growR(I), N_photR(I), N_growR(I), P_photR(I), P_growR(I)
        ENDIF
        WRITE(NOUTDO,"(160('-'))")
      ENDIF

!     Resource productivity calculations
      IF (CONTROL % ErrCode == 0) THEN
        IF (PRESENT(YIELD) .AND. PRESENT(BIOMAS) .AND. NNR(0) > 0) THEN
          IF (CONTROL % CROP /= 'FA') THEN
            IF (RAINR(0) > 1.E-3) THEN
              DMP_Rain = Biomas / RAINR(0) 
              GrP_Rain = YIELD  / RAINR(0)
            ENDIF
          
            CALL GET('MGMT','TOTIR', TOTIR)   !Tot applied irrig (mm)

            IF (TOTIR > 1.E-3) THEN
              DMP_Irr = Biomas / TOTIR 
              GrP_Irr = YIELD  / TOTIR
            ENDIF
          
            IF (CETR(0) > 1.E-3) THEN
              DMP_ET = Biomas / CETR(0) 
              GrP_ET = YIELD  / CETR(0) 
            ENDIF
          
            CALL GET('SPAM','CEP',CEPR(0))
            IF (CEPR(0) > 1.E-3) THEN
              DMP_EP = Biomas / CEPR(0) 
              GrP_EP = YIELD  / CEPR(0) 
            ENDIF

            NDCH  = NNR(0)
            TMINA = TMINR(0)
            TMAXA = TMAXR(0)
            SRADA = RADR(0)
            DAYLA = DAYLR(0)
            CO2A  = CO2R(0)
            PRCP  = RAINR(0)
            ETCP  = CETR(0)
!           ESCP  = CESR(0)
            ESCP  = CEVAPR(0)
            EPCP  = CEPR(0)
          ENDIF
          
          CALL GET(ISWITCH)
          IF (IDETO_SAVE == 'Y' .AND. ISWITCH % ISWWAT == 'Y') THEN
            WRITE (NOUTDO, 1200) NNR(0), 
     &      RAINR(0), DMP_Rain*0.1, DMP_Rain, GrP_Rain*0.1, GrP_Rain, 
     &      CETR(0),  DMP_ET*0.1,   DMP_ET,   GrP_ET*0.1,   GrP_ET, 
     &      CEPR(0),  DMP_EP*0.1,   DMP_EP,   GrP_EP*0.1,   GrP_EP

            IF (TOTIR > 1.E-3) THEN
              WRITE(NOUTDO, 1210) 
     &          TOTIR, DMP_Irr*0.1, DMP_Irr, GrP_Irr*0.1, GrP_Irr
            ENDIF
          ENDIF

          IF (ISWITCH % ISWNIT == 'Y') THEN
            CALL GET('MGMT', 'FERNIT', CumNit)
            IF (CumNit > 1.E-3) THEN
              DMP_NApp = Biomas / CumNit
              GrP_NApp = YIELD  / CumNit
              IF (IDETO_SAVE == 'Y') THEN
                WRITE(NOUTDO, 1220) CumNit, DMP_NApp, GrP_NApp 
              ENDIF
            ENDIF
          
            IF (SUMDAT % NUCM > 1.E-3) THEN
              DMP_NUpt = Biomas / float(SUMDAT % NUCM)
              GrP_NUpt = YIELD  / float(SUMDAT % NUCM)
              IF (IDETO_SAVE == 'Y') THEN
                WRITE(NOUTDO, 1230) SUMDAT % NUCM, DMP_NUpt,GrP_NUpt
              ENDIF
            ENDIF
          ENDIF !ISWNIT == 'Y'

!         Reset arrays for next run.
          TMAXR = 0.0
          TMINR = 0.0
          RAINR = 0.0
          DAYLR = 0.0
          RADR  = 0.0
          CO2R  = 0.0
          CETR  = 0.0
!         CESR  = 0.0
          CEVAPR  = 0.0
          CEPR  = 0.0
          W_growR = 0.0
          W_photR = 0.0
          N_growR = 0.0
          N_photR = 0.0
          P_growR = 0.0
          P_photR = 0.0
          NNR   = 0

        ENDIF
      ENDIF

 1200 FORMAT(
     &//,'*Resource Productivity',
     & /,' Growing season length:', I4,' days ',
     &//,' Precipitation during growing season',T42,F7.1,' mm[rain]',
     & /,'   Dry Matter Productivity',T42,F7.2,' kg[DM]/m3[rain]',
     &                       T75,'=',F7.1,' kg[DM]/ha per mm[rain]',
     & /,'   Yield Productivity',T42,F7.2,' kg[grain yield]/m3[rain]',
     &                       T75,'=',F7.1,' kg[yield]/ha per mm[rain]',
     &//,' Evapotranspiration during growing season',T42,F7.1,' mm[ET]',
     & /,'   Dry Matter Productivity',T42,F7.2,' kg[DM]/m3[ET]',
     &                       T75,'=',F7.1,' kg[DM]/ha per mm[ET]',
     & /,'   Yield Productivity',T42,F7.2,' kg[grain yield]/m3[ET]',
     &                       T75,'=',F7.1,' kg[yield]/ha per mm[ET]',
     &//,' Transpiration during growing season',T42,F7.1,' mm[EP]',
     & /,'   Dry Matter Productivity',T42,F7.2,' kg[DM]/m3[EP]',
     &                       T75,'=',F7.1,' kg[DM]/ha per mm[EP]',
     & /,'   Yield Productivity',T42,F7.2,' kg[grain yield]/m3[EP]',
     &                       T75,'=',F7.1,' kg[yield]/ha per mm[EP]')
 1210 FORMAT(
     & /,' Irrigation during growing season',T42,F7.1,' mm[irrig]',
     & /,'   Dry Matter Productivity',T42,F7.2,' kg[DM]/m3[irrig]',
     &                       T75,'=',F7.1,' kg[DM]/ha per mm[irrig]',
     & /,'   Yield Productivity',T42,F7.2,' kg[grain yield]/m3[irrig]',
     &                       T75,'=',F7.1,' kg[yield]/ha per mm[irrig]')
 1220 FORMAT(
     & /,' N applied during growing season',T42,F7.0,' kg[N applied]/ha'
     & /,'   Dry Matter Productivity',T42,F7.1,' kg[DM]/kg[N applied]',
     & /,'   Yield Productivity',T42,F7.1,' kg[yield]/kg[N applied]')
 1230 FORMAT(
     & /,' N uptake during growing season',T42,I7,' kg[N uptake]/ha'
     & /,'   Dry Matter Productivity',T42,F7.1,' kg[DM]/kg[N uptake]',
     & /,'   Yield Productivity',T42,F7.1,' kg[yield]/kg[N uptake]')

!-----------------------------------------------------------------------
!     Store Summary.out labels and values in arrays to send to
!     OPSUM routines for printing.  
      LABEL(1)  = 'DMPPM'; VALUE(1)  = DMP_Rain
      LABEL(2)  = 'YPPM '; VALUE(2)  = GrP_Rain 
                                    
      LABEL(3)  = 'DMPEM'; VALUE(3)  = DMP_ET
      LABEL(4)  = 'YPEM '; VALUE(4)  = GrP_ET
                                    
      LABEL(5)  = 'DMPTM'; VALUE(5)  = DMP_EP 
      LABEL(6)  = 'YPTM '; VALUE(6)  = GrP_EP
                                    
      LABEL(7)  = 'DMPIM'; VALUE(7)  = DMP_Irr
      LABEL(8)  = 'YPIM '; VALUE(8)  = GrP_Irr
                                    
      LABEL(9)  = 'DPNAM'; VALUE(9)  = DMP_NApp
      LABEL(10) = 'YPNAM'; VALUE(10) = GrP_NApp
                                    
      LABEL(11) = 'DPNUM'; VALUE(11) = DMP_NUpt
      LABEL(12) = 'YPNUM'; VALUE(12) = GrP_NUpt

      LABEL(13) = 'NDCH' ; VALUE(13) = FLOAT(NDCH)
      LABEL(14) = 'TMINA'; VALUE(14) = TMINA
      LABEL(15) = 'TMAXA'; VALUE(15) = TMAXA
      LABEL(16) = 'SRADA'; VALUE(16) = SRADA
      LABEL(17) = 'DAYLA'; VALUE(17) = DAYLA
      LABEL(18) = 'CO2A' ; VALUE(18) = CO2A
      LABEL(19) = 'PRCP' ; VALUE(19) = PRCP
      LABEL(20) = 'ETCP' ; VALUE(20) = ETCP
      LABEL(21) = 'ESCP' ; VALUE(21) = ESCP
      LABEL(22) = 'EPCP' ; VALUE(22) = EPCP
      
!     Send labels and values to OPSUM
      CALL SUMVALS (SUMNUM, LABEL, VALUE) 

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************

      RETURN

      END SUBROUTINE OPSTRESS
!=======================================================================


