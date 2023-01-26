c     GTP Canegro
c     Matthew Jones May 2012
c     Calculate water stress factors:
c     (1) SWDF1 and SWDF2 (photosynthesis and expansive growth stress)
c     (2) SWDF30 (tillering water stress)
c     (3) CWSI
c     (4) STDAYC (cumulative days since severe stress)
c   Modified by MJ, Sept 2013, to implement Singels' suggestions
c   for improving water stress modelling.

!   2023-01-26 chp removed unused variables from argument list: TRWUP

c ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SC_UM_WSTRESS(
     &  Control, ISWITCH, SoilProp, Weather,  ! DSSAT CSM inputs
     &  SW, EO, GLAI,                         ! Plant & environ. inputs
     &  SWDF1, SWDF2, SWDF30, CWSI, STDAYC,   ! Water stress outputs
     &  RLV)
c ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

c     DSSAT module definitions
      USE ModuleDefs
c     This module exposes access to functions for getting and
c     setting certain CSM variables
      USE ModuleData

      IMPLICIT NONE
      EXTERNAL GET_SPECIES_COEFF, GET_CULTIVAR_COEFF, AQ_WSTRESS
      SAVE

c     Number of soil layers
      INTEGER NLAYR
c     Stress factor affecting photosynthesis
      REAL, INTENT(OUT) :: SWDF1
c     Stress factor affecting expansive growth
      REAL, INTENT(OUT) :: SWDF2
c     Stress factor affecting tiller development
      REAL, INTENT(OUT) :: SWDF30
c     Water stress factor affecting photosynthesis (including long-term effects from severe stress)
      REAL, INTENT(OUT) :: CWSI
c     Cumulative stressed days for leaf senescence (can be reset with enough 
c     precipitation) [d]
      REAL, INTENT(OUT) :: STDAYC
c     Daily weather inputs from the CSM:
      Type (WeatherType), INTENT(IN) :: WEATHER

c     Daily change in stressed days
      REAL D_STDAYC

c     Precipitation required to cancel out accumulated
c     water stress.
      REAL RESET     
           


c     Soil layer thicknesses representing how much of each layer is within 30 cm of the top of the soil profile[cm]
      REAL SWDF30_MASK(NL)
c     The same as SWDF30_MASK, but normalised to add up to 1
      REAL SWDF30_NORM_MASK(NL)
c     Sum of SWDF30_MASK values (for normalisation)
      REAL SWDF30_MCSUM
c     Cumulative depth of soil layers[cm]
      REAL CDEP1
!c     Potential plant transpiration [cm/d]
!      REAL EP1
c     Daily change in thermal time for photosynthesis recovery [°Cd]
      REAL DTT_WSREC
c     Thermal time accumulated since the last severe water stress event [°Cd]
      REAL RECTIM
c     SWDF1 value at last severe water stress event
      REAL SWDFMN

c     Available soil water content fraction [0-1; cm3/cm3/cm3/cm3]
      REAL FX(NL)

c     DSSAT CSM simulation control variable
      TYPE (ControlType), INTENT(IN) :: Control
c     DSSAT CSM input switches
      TYPE (SwitchType), INTENT(IN) :: ISWITCH
c     DSSAT CSM soil properties variable
      TYPE(SoilType), INTENT(IN) :: SoilProp

c     Green leaf area index[m²/m²]
      REAL, INTENT(IN) :: GLAI
c     Potential evaporation [mm/d]
      REAL, INTENT(IN) :: EO
c     Potential daily root water uptake over soil profile[cm/d]
!     REAL, INTENT(IN) :: TRWUP
c     Volumetric soil water content of soil layer, cm3 water/cm3 soil    [cm³/cm³]
      REAL, INTENT(IN) :: SW(NL)
c     Volumetric soil water content of soil layer, cm3 water/cm3 soil    [cm³/cm³]
      REAL, INTENT(IN) :: RLV(NL)  
      
      REAL SWDF1ARR(NL), SWDF2ARR(NL)    
      
c     Mean and max daily temperature [°C]
      REAL :: TMEAN, TMAX
c     Daily rainfall (mm)
      REAL :: RAIN


c     Genetic trait (species file) parameters
c     :::::::::::::::::::::::::::::::::::
c     Root water uptake factor 1 (photosynthesis)
c     Corresponds with SWDF1
      REAL RWUEP1
c     Root water uptake factor 2 (expansive growth)
c     Corresponds with SWDF2
      REAL RWUEP2
c     Critical soil water deficit value below which water stress has long-term impacts
      REAL CRITSW
c     Thermal time required to for photosynthesis to recover from water stress [°Cd]
      REAL HuRecover
c     Base temperature for photosynthesis thermal recovery time [°C]
      REAL TBASE_WSREC

c     Effective (infiltrated) irrigation each day (mm):
      REAL IRRAMT
c     Precipitation - rain and irrigation (mm)
      REAL PRECIP

c     Function for AquaCrop-style water stress
      ! REAL AQ_WSTRESS
c     And its parameters:
c     P_UP5: soil water deficit threshold at which water stress starts, when ET0 = 5 mm/d
c     P_LOW5: soil water deficit threshold at which water stress is maximised (plant process rate is zero), when ET0 = 5 mm/d
c     FSHAPE: exponential shape parameter
      REAL P_UP5_1, P_LOW5_1, FSHAPE_1, P_UP5_2, P_LOW5_2, FSHAPE_2

c     Loop counter
      INTEGER I
c     Error variable for species coefficient read
      LOGICAL SPC_ERROR
c     Error status of cultivar/species file reads
      LOGICAL CF_ERR

c ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     Copy number of active layers to local variable for clearer
c     code
      NLAYR = SoilProp%NLAYR
      TMEAN = Weather%TAVG
      RAIN = Weather%RAIN
      TMAX = Weather%TMAX

c     ===============================================================
c     SEASONAL INITIALISATION
c     ===============================================================
c     Code to be called at the beginning of each treatment
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF (Control%DYNAMIC .EQ. SEASINIT) THEN
c       Read coefficients for SWDF1, SWDF2 from the species file
        RWUEP1         = 1.
        RWUEP2         = 2.
        CALL GET_SPECIES_COEFF(RWUEP1,'RWUEP1', CONTROL, SPC_ERROR)
        CALL GET_SPECIES_COEFF(RWUEP2,'RWUEP2', CONTROL, SPC_ERROR)

c       Read critical SWDF value from species file:
        CRITSW = 0.2
        CALL GET_SPECIES_COEFF(CRITSW,'CRITSW', CONTROL, SPC_ERROR) 

c       Thermal time for photosythesis to recover from moisture stress
        HuRecover = 150.
        CALL GET_SPECIES_COEFF(HuRecover, 'HuRecover', 
     &    CONTROL, SPC_ERROR)

        RESET    = 5.
c       Species coeffs
        CALL GET_SPECIES_COEFF(RESET,  'RESET',  Control, SPC_ERROR)


c       AquaCrop water stress parameters
c       ::::::::::::::::::::::::::::::::
c       For calculating SWDF1 (stress affecting photosynthesis and transpiration)
        P_UP5_1 = 0.55
        P_LOW5_1 = 0.9
        FShape_1 = 2.0
c       For calculating SWDF2 (stress affecting plant expansive growth)
        P_UP5_2 = 0.20
        P_LOW5_2 = 0.70
        FShape_2 = 2.0
c       Read parameters from genotype files:
        CALL GET_CULTIVAR_COEFF(P_UP5_1, 'AQP_UP5', CONTROL, CF_ERR)
        ! CALL GET_CULTIVAR_COEFF(P_UP5_2, 'P_UP5_1', CONTROL, CF_ERR)
c       Calculate P_LOW_5_1 as 0.35 + P_UP5
c       AS, MJ June 2015
c       AS, MJ Oct 2016: P_LOW5 = P_LO for photos and transpiration
        P_LOW5_1 = MIN(1.0, P_UP5_1 + 0.35)
c       AS, MJ Oct 2016:
c       P_LOW5 = P_LO for Expansive growth 
        P_LOW5_2 = MIN(1.0, P_UP5_2 + 0.50)

D        WRITE(*, '(A, 2F8.2)') 'Photos, transp.:', P_UP5_1, P_LOW5_1
D        WRITE(*, '(A, 2F8.2)') 'Expansive grth.:', P_UP5_2, P_LOW5_2
        

c       Base temperature for photosynthesis thermal recovery time [°C]
c       ## read from species file!
        TBASE_WSREC = 10.
        DTT_WSREC = 0.

c       Reset water stress variables:
c       :::::::::::::::::::::::::::::
c       Soil water deficit factors for photosynthesis and growth:
        SWDF1 = 1.0
        SWDF2 = 1.0
        CWSI = 1.0
c       Soil water deficit factor for tillering (soil water content
c       of top 30 cm of soil):
        SWDF30 = 1.0
c       Cumulative stress days
        STDAYC = 0.0
        D_STDAYC = 0.0
        RECTIM = 0.0

c       Local variables for calculating SWDF30
        CDEP1 = 0.
        SWDF30_MCSUM = 0.
        DO I=1, NL
          SWDF30_MASK(I) = 0.
          SWDF30_NORM_MASK(I) = 0.
          FX(I) = 1.
        ENDDO

c       WATER STRESS INIT:
c       :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::    
c       MJ, December 2010.  Calculate which layers fall completely (1),
c       partially (2) and not at all (3) within the 30 cm depth threshold
c       for tillering water stress.
c       SWDF30_MASK:
c       Contains DLAYR(I) representing (1), 0 representing (3),
c       and (possibly) a single layer thickness fraction value representing (2).
c       SWDF30_NORM_MASK:
c       This is then normalised (so that all coefficients add up to 1)
c       The dot product of this is then used to simplify and shorten the 
c       SWDF30 calculation in daily rates.  It is also reduces the 
c       computation time by several nanoseconds...

c       Find soil layers within 30 cm of soil surface
c       :::::::::::::::::::::::::::::::::::::::::::::
        DO I=1, NLAYR
c         Calc soil depth so far
          CDEP1 = CDEP1 + SoilProp%DLAYR(I)          
          IF (CDEP1 .GT. 30.) THEN
c           Find the fraction of the soil layer that reaches 30 cm depth
c           for the layer that spans the 30 cm threshold (2)
            SWDF30_MASK(I) = MAX(0., SoilProp%DLAYR(I) - (CDEP1 - 30.))
          ELSE
c         This layer is fully within the 30 cm limit (1):
          SWDF30_MASK(I) = SoilProp%DLAYR(I)
          ENDIF
        ENDDO
c       Normalise SWDF30_MASK:
c       ::::::::::::::::::::::
c       Add up all elements from 1 to NLAYR
        SWDF30_MCSUM = SUM(SWDF30_MASK(1:NLAYR))
c       Error checking (swdf30 layers should add up to within 0.01 cm of 30 cm!):
        IF (ABS(30. - SWDF30_MCSUM) .GT. 0.001) THEN
          WRITE(0, '(A, F10.4)') 'Total soil depth (should be 30!) '// 
     &      ' in SC_UM_WSTRESS seasonal init. is: ',SWDF30_MCSUM
        ENDIF
        DO I=1, NLAYR
c         Normalise:
          SWDF30_NORM_MASK(I) = SWDF30_MASK(I) / SWDF30_MCSUM 
        ENDDO  

!c       Output DUL and LL per layer
!        DO I=1, SoilProp%NLAYR
!          WRITE(*, '(4F10.5)') SoilProp%DLAYR(I), SoilProp%LL(I), 
!     &      SoilProp%DUL(I), SoilProp%SAT(I)
!        ENDDO


c       :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::     

c     =================================================================
c     RATE CALCULATIONS
c     =================================================================
      ELSEIF (Control%DYNAMIC .EQ. RATE) THEN
c       MJ, October 2006: use the switch object to determine whether or not
c       to simulate water stresses (i.e. ISWWAT determines whether or not
c       a water balance is simulated)
c       :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        DTT_WSREC = 0.

c       Get effective irrig amt today (mm)
        CALL GET('MGMT','IRRAMT',IRRAMT)  
c       Get total precipitation for today:
        PRECIP = RAIN+IRRAMT

c       SWDF30 is a stress factor, weighted by layer 
c       thickness, which applies to the top x layers that
c       occupy the top 30 cm of the soil profile
c       ::::::::::::::::::::::::::::::
        SWDF30 = 1.
        IF (ISWITCH%ISWWAT .EQ. 'Y') THEN
          DO I=1, NLAYR
!c           19/6/2003 - AS and SB changed
!c           strpop was negative and sw was less than ll
            FX(I) = MAX(0., MIN(1., 
     &        3.*(SW(I) - SoilProp%LL(I)) 
     &        / (SoilProp%DUL(I) - SoilProp%LL(I)) ))
          ENDDO
          SWDF30 = MIN(1., 
     &      DOT_PRODUCT(FX(1:NLAYR), SWDF30_NORM_MASK(1:NLAYR)))
        ENDIF


c       MJ, May 2012.   Calculate SWDF1 and SWDF2
c       ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c       Default: no stress:
        SWDF1 = 1.
        SWDF2 = 1.
        DO I=1, NL
          SWDF1ARR(I) = 1.0
          SWDF2ARR(I) = 1.0
        ENDDO

c       LAI > 1.E-4, calculate stresses:
c       [as result of a threshold value for LAI
c       that limits the calculation of EP - see SPAM.for, 331]
        IF (   (GLAI .GT. 0.0001)
     &   .AND. (ISWITCH%ISWWAT .EQ. 'Y') 
     &   .AND. (EO .GT. 0.00001) ) 
     &  THEN 
!          EP1 = EO * 0.1
!         Was as follows
!          SWDF1 = MIN(1.0, (1.0/RWUEP1* TRWUP/EP1))
c         :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c         Changed to the following (ASA 2013 / 08RE14 UKZN ACRU Hybrid model)
          ! WRITE(*, '(A, F5.2)') 'SC_UM_WSTRESS: Photos, EO = ', EO
          CALL AQ_WSTRESS(EO, P_UP5_1, P_LOW5_1, Fshape_1, RLV(1:NLAYR),
     &       SW(1:NLAYR), SoilProp%DLAYR(1:NLAYR), NLAYR, 
     &       SoilProp%LL(1:NLAYR), SoilProp%DUL(1:NLAYR), 
     &       SoilProp%SAT(1:NLAYR), SWDF1, SWDF1ARR(1:NLAYR))

!          SWDF2 = MIN(1.0, (1.0/RWUEP2 * TRWUP/EP1))
          ! WRITE(*, '(A, F5.2)') 'SC_UM_WSTRESS: Growth, EO = ', EO
          CALL AQ_WSTRESS(EO, P_UP5_2, P_LOW5_2, Fshape_2, RLV(1:NLAYR),
     &       SW(1:NLAYR), SoilProp%DLAYR(1:NLAYR), NLAYR, 
     &       SoilProp%LL(1:NLAYR), SoilProp%DUL(1:NLAYR), 
     &       SoilProp%SAT(1:NLAYR), SWDF2, SWDF2ARR(1:NLAYR))

        ENDIF

c       Calculate longer-term impacts of severe stress on 
c       photosynthesis:
c       :::::::::::::::::::::::::::::::::::::::::::::::::
        IF (GLAI .GT. 0.001) THEN
          IF (ISWITCH%ISWWAT .EQ. 'Y') 
     &    THEN
c           If water balance is used:
            DTT_WSREC = MAX(0., TMEAN - TBASE_WSREC)
            
c           When water stress is severe and leaf area index
c           low, set time since last severe stress event
c           to 0.
            IF ((SWDF1 .LE. CRITSW) .AND. (GLAI .GT. 0.5))
     &      THEN
              RECTIM = 0.
              SWDFMN = SWDF1
            ENDIF

c           CWSI is a photosynthesis reduction factor
            CWSI = MIN(SWDFMN + RECTIM/HuRecover, SWDF1)
          ELSE
            CWSI = 1.
          ENDIF
        ENDIF


c       Calculate cumulative water stress.  This
c       affects leaf senescence (accelerates it).
c       ::::::::::::::::::::::::::::::::::::::::::::::::
c       MJ, June 2012: I do not like this, but it is 
c       probably not critical.  GIB originally suggested
c       calculating leaf senescence based on the net
c       C-balance (leaf stays alive while photosynthesis
c       rate > respiration rate).
c       For now, let's stick with krusty approach:
C Stress days for leaf size and number are accumulated STDAYG as in MAIN 
C but are reset when soil  profile is half full. The stress increment 
C is doubled every 5 deg C above Tmax = 25C. This is to cope with the 
c damage done to a canopy which can remain green in a dry soil but will 
c lose leaves rapidly under very hot conditions.
        D_STDAYC = 0.0
        IF (ISWITCH%ISWWAT .EQ. 'Y') THEN
c         If water stress is included, accumulate stress
          D_STDAYC = (1.0-SWDF2)*MAX(1.0,TMAX/5.0-4.0)
c         Reset the counter to 0 when precipiation exceeds the
c         threshold.
          IF(PRECIP .GT. RESET) THEN
            D_STDAYC = STDAYC * -1.0
          ENDIF
c          WRITE(*,*) 'STDAYC is ', STDAYC
        ELSE
c         If water stress excluded:
          STDAYC=0.0
        ENDIF        


c     ===============================================================
c     INTEGRATION CALCULATIONS
c     ===============================================================
      ELSEIF(Control%DYNAMIC.EQ.INTEGR) THEN   
c       Integrate longer-term impacts of severe stress on 
c       photosynthesis:
c       :::::::::::::::::::::::::::::::::::::::::::::::::
        IF ((GLAI .GT. 0.001) .AND. (ISWITCH%ISWWAT .EQ. 'Y')) THEN
c         Integrate total thermal time value
          RECTIM = RECTIM + DTT_WSREC
c         When water stress is severe and leaf area index
c         low, set time since last severe stress event
c         to 0.
          IF ((SWDF1 .LE. CRITSW) .AND. (GLAI .GT. 0.5))
     &    THEN
            RECTIM = 0.
            SWDFMN = SWDF1
          ENDIF
        ENDIF

c       Water stress for leaf senescence
        STDAYC = D_STDAYC + STDAYC


c     ===============================================================
c     OUTPUT
c     ===============================================================
c     Produce output each day after rate and integration calcs
c     are completed.  This output relates only to this module.
c     ::::::::::::::::::::::::::::::::::::
      ELSEIF(Control%DYNAMIC.EQ.OUTPUT) THEN  
c     ===============================================================
      ENDIF

c     End of subroutine      
      END     
c     ***************************************************************      


c      New water stress approach (ASA 2013 / 08RE14 UKZN ACRU Hybrid model)
c      Algorithm prepared by Abraham Singels
c      Implemented by M. Jones, Sept 2013
c      CHANGES
c      2016-10-24 MJ: modified the lower threshold value to be insensitive
c                     to atmospheric demand.  P_LO is set to P_LOW5
c      :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c      Uses AquaCrop approach with soil water content (expressed as
c      a soil water deficit) thresholds which vary according to reference
c      evaporation (EO here).
c      Reference: Steduto, et al. (2010)?? 
c      :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
       SUBROUTINE AQ_WSTRESS(EO, P_UP5, P_LOW5, Fshape, RLV, 
     &   SW, DLAYR, NLAYR, LL, DUL, SAT, SWDFN, NSTLAYR)
         IMPLICIT NONE

         INTEGER, INTENT(IN) :: NLAYR
         REAL, INTENT(IN) :: EO, P_UP5, P_LOW5, Fshape
         REAL, INTENT(IN), DIMENSION (NLAYR) ::  DLAYR, LL, DUL,
     &     SW, RLV, SAT
c        Output: soil water deficit factor 'n'
         REAL SWDFN
         INTEGER I
         REAL C_UP, P_UP, P_LOW , DREL_RAW  !, C_LOW
         REAL, DIMENSION (NLAYR) ::  RSWD, DREL, AQWS_RLZ, AQWSTRESS,
c          Normalised stress values per layer:
     &     NSTLAYR

c        Aeration stress variables
c        Soil water content, DUL, SAT over the rooted profile (cm)
         REAL SWC_RTD, DUL_RTD, SAT_RTD
c        ...and per layer
         REAL SWC_RTDi, DUL_RTDi, SAT_RTDi
c        Rooted-profile aeration stress         
         REAL AERSTp
c        Number of rooted layers
         INTEGER NROOTD
c        Aeration stress per layer
         REAL AERST(NLAYR)
c        Waterlogging stress index, today and 7-day avg (not yet implemented)
         REAL WTRLG, WTRLG_7

         INTENT (OUT) :: SWDFN, NSTLAYR

c        Soil deficit stress
c        :::::::::::::::::::::::::::::::::::::::::::::
c         Upper threshold (relative soil water deficit (RSWD) where stress for a plant process starts)
          ! P_UP5 = 0.55
          C_UP = 0.4186*(EXP(4.8622 * P_UP5))
          P_UP = C_UP/(EO + C_UP)
          
          DREL_RAW = 0.0

c         Lower threshold (RSWD threshold where stress is maximum --> plant process rate is 0)
          ! P_LOW5 = 1.0
          ! C_LOW = 0.4186*(EXP(4.8622 * P_LOW5))
          ! P_LOW = C_LOW/(EO + C_LOW)
c         MJ, Oct 2016: 
c           P_LOW5 is typically P_UP5 + 0.35 for transpiration and photosynthesis,
c           P_LOW5 is typically P_UP5 + 0.50 for expansive growth.
          P_LOW = P_LOW5

          DO I=1, NLAYR
c           Relative soil water deficit, per layer:
            RSWD(I) = 1.0 - ((SW(I) - LL(I))/(DUL(I)-LL(I)))
c           Soil water depletion as a fraction of the range between P_UP and P_LOW
!           Conditional statement added by MJ, Oct 2016:
!           A discontinuity in DREL is evident when EO is very low and
!           P_UP = P_LOW.  DREL goes from 1 to 0 very rapidly as EO
!           increases from 0.4 to 0.5 mm/d for P_UP5 = 0.6 and P_LOW = 0.95
            DREL_RAW = MIN(1.0, MAX(0.0, (RSWD(I)-P_UP)/(P_LOW-P_UP)))
            IF (P_UP .GE. P_LOW) THEN
              DREL(I) = 0.0
            ELSE
              DREL(I) = DREL_RAW 
            ENDIF
            DREL_RAW = 0.0
c           Water stress factor per layer:
            AQWSTRESS(I) = 1.0-(EXP(DREL(I)*Fshape)-1.0)/
     &       (EXP(Fshape)-1.0)
!            WRITE(*, ('(10F10.3)')) EO,P_UP5,C_UP,P_UP,P_LOW,SW(I),LL(I)
!     &        ,DUL(I),DREL(I), AQWSTRESS(I) 
          ENDDO

c         Multiply each stress value by the thickness of the layer,
c         and the volume of roots in that layer, to get a stress value
c         weighted by root length: 
          DO I=1, NLAYR
            AQWS_RLZ(I) = AQWSTRESS(I) * RLV(I) * DLAYR(I)
          ENDDO
c         then divide by the product of thickness and rootlength density
c         to get a weighted-average SWDF:
          IF ((SUM(RLV) .GT. 0.0001) .AND. (SUM(DLAYR) .GT. 0.0001)) 
     &    THEN
            SWDFN = SUM(AQWS_RLZ) / 
     &        DOT_PRODUCT(RLV(1:NLAYR), DLAYR(1:NLAYR))
            !AQ_WSTRESS = SUM(AQWS_RLZ) / (1.0 * NLAYR)
          ELSE
            SWDFN = 1.0
          ENDIF 

          DO I=1, NLAYR
            NSTLAYR(I) = AQWS_RLZ(I) / 
     &        DOT_PRODUCT(RLV(1:NLAYR), DLAYR(1:NLAYR))
          ENDDO

c        Soil excess water stress
c        'Aeration stress' (Steduto, et al)
c        :::::::::::::::::::::::::::::::::::::::::::::
         SWC_RTD = 0.0
         DUL_RTD = 0.0
         SAT_RTD = 0.0
         AERSTp = 0.0
c        Get total SWC, SAT for the whole rooted profile
         DO I=1, NLAYR
           IF (RLV(I) .GT. 0.0001) THEN
           ! this layer is rooted
           NROOTD = I  
c          Rooted profile soil water content (cm)
           SWC_RTDi = SW(I) * DLAYR(I)
c          Rooted profile DUL (anything above is waterlogging)
           DUL_RTDi = DUL(I) * DLAYR(I)
c          Rooted profile SAT (anything above is runoff)
           SAT_RTDi = SAT(I) * DLAYR(I)

c          Aeration stress per layer (used to distribute whole-profile
c          aeration stress only):
c          # possibility to have a lower denominator for
c          # reducing expansive growth (e.g. ((SAT_RTDi - DUL_RTDi)/2.0) )
c          # more than photosynthesis
           AERST(I) = MAX(0.0, SWC_RTDi - DUL_RTDi) /
     &       (SAT_RTDi - DUL_RTDi)

c          Totals:
           SWC_RTD = SWC_RTD + SWC_RTDi
           DUL_RTD = DUL_RTD + DUL_RTDi
           SAT_RTD = SAT_RTD + SAT_RTDi
           AERSTp = AERSTp + AERST(I)
           
           ENDIF
         ENDDO     

c        Extent of aeration stress over the rooted profile
         ! WTRLG = MAX(0.0, SWC_RTD - DUL_RTD) / (SAT_RTD - DUL_RTD)
c        Use the lowest per-layer value to determine per-profile aeration stress:
         WTRLG = MINVAL(AERST)
c        Now calculate 7-day running average aeration stress:
         ! #todo
         WTRLG_7 = WTRLG
c        If there is aeration stress, distribute by layer in order
c        to limit transpiration according to per-layer relative 
c        aeration stress:
         IF (WTRLG_7 .GT. 0.00001) THEN
c        Consider rooted layers only
         DO I=1, NROOTD
c          This layer's SWDF1 'aeration stress' is weighted by the 
c          previously-calculated relative aeration stress and scaled 
c          to per-profile stress.
            NSTLAYR(I) = (AERST(I) / AERSTp) * (1.0 - WTRLG_7)
         ENDDO
c        And update the whole-profile water stress factor:
         SWDFN = (1.0 - WTRLG_7)
!         WRITE(*, '(7F10.4)') SWC_RTD, DUL_RTD, SAT_RTD, 
!     &     MAX(0.0, SAT_RTD-SWC_RTD), SAT_RTD-DUL_RTD, AERSTp, WTRLG_7
         ENDIF
       END



