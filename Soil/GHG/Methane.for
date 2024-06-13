C=======================================================================
C SUBROUTINE MethaneDynamics
C
C Subroutine to act as interface between CERES-Rice and Arah methane model.
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written     R.B.M. March 1998.
! 2021-06-30 CHP and US adapt for DSSAT-CSM v4.8
! 2023-01-24 chp added SAEA to soil analysis in FileX
! 2023-05-14 EHFMS changed the starting condition for methane production
C=======================================================================
      SUBROUTINE MethaneDynamics(CONTROL, ISWITCH, SOILPROP,  !Input
     &    FERTDATA, FLOODWAT, SW, RLV, newCO2, DRAIN,         !Input
     &    CH4_data)                                           !Output

      USE GHG_mod
      USE FloodModule
      USE MethaneConstants
      USE MethaneVariables
      IMPLICIT NONE
      EXTERNAL OpMethane, SteadyState, setup
      SAVE

      INTEGER n1,NLAYR,i,j, DYNAMIC
      REAL dlayr(NL),SW(NL),DLL(NL),RLV(NL),CSubstrate(NL),BD(NL),
     &     Buffer(NL,2),afp(NL), SAEA(NL), SAT(NL), DUL(NL)
      REAL, DIMENSION(0:NL) :: newCO2
      REAL drain,flood,x,CH4Emission,buffconc,rCO2,
     &     rCH4,TCH4Substrate,rbuff,afpmax,
     &     ProductionFrac,ConsumptionFrac,EmissionFrac,PlantFrac,
     &     EbullitionFrac,DiffusionFrac,LeachingFrac,
     &     CH4Production,CH4Consumption,CH4PlantFlux,CH4Ebullition,
     &     CH4Diffusion,CH4Leaching,CH4Stored
      REAL CumCH4Consumpt, CumCH4Leaching, newCO2Tot, CH4_balance
      REAL CumCH4Emission, CumCO2Emission, CO2emission, CumNewCO2
      REAL StorageFlux, Cum_CH4_bal, CH4Stored_Y
!     REAL CH4_correction !, ReductFact

      REAL TCO2, TCH4, FloodCH4

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (SoilType)    SOILPROP
      TYPE (FloodWatType)FLOODWAT
      TYPE (FertType)    FERTDATA
      TYPE (CH4_type)    CH4_data

!-----------------------------------------------------------------------
      REAL, PARAMETER :: spd = 24.*3600.   ! seconds per day
!     Reference height for the Arah model to be the top of the bund
      REAL, PARAMETER :: RefHeight = 100. ! mm
      !1/d EHFMS changed, before was 0.06
      REAL, PARAMETER :: BufferRegenRate = 0.070
      !EHFMS created this parameter 
      REAL, PARAMETER :: frac_afpmax = 0.30
      DYNAMIC = CONTROL % DYNAMIC
      DLAYR = SOILPROP % DLAYR
      DLL   = SOILPROP % LL
      BD    = SOILPROP % BD
      NLAYR = SOILPROP % NLAYR
      
C***********************************************************************
C***********************************************************************
C    Input and Initialization 
C***********************************************************************
      IF (DYNAMIC .EQ. INIT) THEN
C-----------------------------------------------------------------------
      FirstTime = .TRUE.

      TCO2 = 0.0
      TCH4 = 0.0
      newCO2Tot = 0.0
      CO2emission    = 0.0
      CH4Emission    = 0.0
      CH4Consumption = 0.0
      CH4Leaching    = 0.0
      CH4Stored      = 0.0
      CumCO2Emission = 0.0
      CumCH4Emission = 0.0
      CumCH4Consumpt = 0.0
      CumCH4Leaching = 0.0                    
      CumNewCO2     = 0.0

      CH4_data % CO2emission    = 0.0
      CH4_data % CH4Emission    = 0.0
      CH4_data % CH4Consumption = 0.0
      CH4_data % CH4Leaching    = 0.0
      CH4_data % CumCO2Emission = 0.0
      CH4_data % CumCH4Emission = 0.0
      CH4_data % CumCH4Consumpt = 0.0
      CH4_data % CumCH4Leaching = 0.0                    

      IF (CONTROL % RUN .EQ. 1 .OR. 
     &    INDEX('QF',CONTROL % RNMODE) .LE. 0) THEN
        CH4Stored     = 0.0
        CH4Stored_Y   = 0.0
        CH4_data % CH4Stored = 0.0

!     SAEA = Soil Alternative Electron Acceptors (mol Ceq/m3)
!     SAEA = 26.5  
      SAEA = SOILPROP % SAEA

      FloodCH4 = 0.0
      DO i=1,NLAYR
!       Convert the alternate electron acceptors in each layer 
!       from mol Ceq/m3 to kgC/ha
!       Buffer(i,1) = Buffer(i,1) * 12.*(dlayr(i)/100.)*10. ! kg Ceq/ha
        Buffer(i,1) = SAEA(i) * 12.*(dlayr(i)/100.)*10. ! kg Ceq/ha
        Buffer(i,2) = 0.0
      ENDDO

!     proportionality constant for root transmissivity and RLV	
!     (0.00015 m air/(m root))
!     lamda_rho = lamdarho  ! 0.00015
      lamda_rho = 0.00015

      ENDIF

      CALL OpMethane(CONTROL, ISWITCH,  
     &  newCO2Tot, CO2emission, TCH4Substrate, StorageFlux, CH4Stored,  
     &  CH4Production, CH4Consumption, CH4Leaching, CH4Emission,
     &  CH4PlantFlux, CH4Ebullition, CH4Diffusion, CH4_balance, 
     &  CumNewCO2, CumCO2Emission, CumCH4Emission, CumCH4Consumpt, 
     &  CumCH4Leaching, Cum_CH4_bal)

C***********************************************************************
C***********************************************************************
C     Rate Calculations 
C***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
C-----------------------------------------------------------------------
      CO2emission = 0.0
      CH4emission = 0.0
      CH4Consumption = 0.0
      CH4Leaching    = 0.0

      CH4Production  = 0.0
      CH4PlantFlux   = 0.0
      CH4Ebullition  = 0.0
      CH4Diffusion   = 0.0

!     Calculate total CO2 coming in from decomposition 
!     of organic matter, newCO2Tot
!     Transfer newCO2 from soil organic matter modules 
!     to CSubstrate variable
      CSubstrate = 0.0
      newCO2Tot = 0.0
      DO i = 1, SOILPROP % NLAYR
        IF (i == 1) THEN
          newCO2Tot = newCO2(0) + newCO2(1)
          CSubstrate(1) = newCO2Tot
        ELSE
          CSubstrate(i) = newCO2(i)
          newCO2Tot = newCO2Tot + newCO2(i)
        ENDIF
      ENDDO

      FLOOD = FLOODWAT % FLOOD

!     Arah model parameters for flood-water layer
      n1 = 2
      z(1) = (RefHeight - flood)/1000. ! mm-->m
      z(n1) = RefHeight/1000.	   
      DO i =1,n1
        theta(i) = 1.0            ! water content 
        epsilon(i) = 0.0          ! air-filled porosity
        lamda(i) = 0.0            ! root transmissivity
        VV(om,i) = 0.0            ! maximum methanogenesis rate
        VV(o2,i) = 0.0            ! maximum aerobic respiration rate
        VV(ch4,i) = 1.5e-5        ! maximum CH4 oxidation rate
      ENDDO

!     Parameters for soil layers
!     CO2_Cflux = 0.0
      TCH4Substrate = 0.0
      CO2emission   = 0.0

      DO i=1,NLAYR
!       calculate air-filled porosity (v/v)
        IF (FLOOD.GT.0.0) THEN 
          afp(i) = 0.0
        ELSE 
            afp(i) = max(0.0,1.0 - BD(i)/2.65 - SW(i))          
      ENDIF
         afpmax = 1.0 - BD(i)/2.65

!       Update buffer from new fertilizer
        Buffer(i,1) = Buffer(i,1) + FERTDATA % AddBuffer(i)

!       calculate buffer concentration (molCeq/m3)
        buffconc = Buffer(i,1)/10./12./(dlayr(i)/100.)  

!       calculate reoxidisation of buffer if soil is aerated
!         IF (afp(i).GT.0.0) THEN !     
! EHFMS: Methane Prod. under conditions of partially saturated soil
      IF (afp(i).GT.frac_afpmax*afpmax) THEN 
         rCH4 = 0.0              ! no CH4 production
         rCO2 = CSubstrate(i)    ! aerobic respiration
         rbuff = -MIN(BufferRegenRate * afp(i) / afpmax * Buffer(i,2),
     &                Buffer(i,2))
      ELSE
      ! calculate methane production
      IF (buffconc > 0.0) THEN
         rCH4 = 0.3 * (1.0 - buffconc/24.0)    ! mol C m3/d  was 0.2
         rCH4 = rCH4 * dlayr(i)/100. * 12. * 10.   ! kgC/ha/d
      ELSE  
        rCH4 = CSubstrate(i) / 2.0            ! kgC/ha/d
      ENDIF
       rCH4 = MAX(0.0, MIN(rCH4, CSubstrate(i)/2.0))
       rCO2 = CSubstrate(i) - (2.0 * rCH4)
      IF (rCO2 > Buffer(i,1)) THEN
         rCO2 = Buffer(i,1)
         rCH4 = (CSubstrate(i) - rCO2) / 2.0
      ENDIF
      rbuff = rCO2  
      ENDIF
      Buffer(i,1) = Buffer(i,1) - rbuff       ! oxidized buffer pool
      Buffer(i,2) = Buffer(i,2) + rbuff       ! reduced buffer pool                       

!       Total CH4 substrate (kgC/ha)
        TCH4Substrate = TCH4Substrate + rCH4  

!       Calculate soil profile parameters for Arah model
        j = i + n1
        z(j) = z(j-1) + dlayr(i)/100.     ! depth of each layer
        theta(j) = SW(i)                  ! soil water content (v/v)
        epsilon(j) = afp(i)               ! air-filled porosity (v/v)
        lamda(j) = RLV(i) * lamda_rho     ! root transmissivity
!       maximum rate of methanogenesis (Vm, mol CH4/m3/s)
!       (assume all is consumed in a day)
!       (i.e. convert kgC/ha/d -->moleCH2O/m3/s)
        VV(om,j) = rCH4/10./12./(dlayr(i)/100.)/spd
!       maximum rate of aerobic respiration (Vr, mol CO2/m3/s)
!       (convert kgC/ha/d -->moleCH2O/m3/s)
        VV(o2,j) = rCH4/10./12./(dlayr(i)/100.)/spd 
!       maximum rate of methane oxidation   (Vo, mol CH4/m3/s)
        VV(ch4,j) = 1.5e-5
      ENDDO

!     Leaching rate
      Lz = drain * 1.e-3/spd    ! mm/d --> m3/m2/s

!     Call the steady-state routine of the Arah model
!     TSubstrate comes out of these routines, 
!     ~10% of TCH4Substrate for IRLB9701.RIX, trt 4
      CALL setup(NLAYR+n1)
      CALL SteadyState

!     Calculate fractions of C in each methane flux
      IF(TSubstrate.GT.0.0) THEN
        ProductionFrac  = meth%Production /TSubstrate
        ConsumptionFrac = meth%Consumption/TSubstrate
        PlantFrac       = meth%RootFluxOut/TSubstrate
        EbullitionFrac  = meth%Ebullition /TSubstrate
        LeachingFrac    = meth%Leaching   /TSubstrate
      ELSE
        ProductionFrac  = 0.0
        ConsumptionFrac = 0.0
        PlantFrac       = 0.0
        EbullitionFrac  = 0.0
        LeachingFrac    = 0.0
      ENDIF

!!     Limit leaching fraction + consumption fraction 
!      to no more than production
!      IF (ConsumptionFrac + LeachingFrac .GT. ProductionFrac) THEN
!        ReductFact = ProductionFrac / (ConsumptionFrac + LeachingFrac)
!        ConsumptionFrac = ConsumptionFrac * ReductFact
!        LeachingFrac = LeachingFrac * ReductFact
!      ENDIF

      EmissionFrac = ProductionFrac - ConsumptionFrac - LeachingFrac
      DiffusionFrac = EmissionFrac - (PlantFrac + EbullitionFrac)

!     Calculate actual flux rates (kgC/ha/d) based on 
!     CERES-Rice substrate calculations
      CH4Production  = TCH4Substrate * ProductionFrac
      CH4Consumption = TCH4Substrate * ConsumptionFrac
      CH4PlantFlux   = TCH4Substrate * PlantFrac
      CH4Ebullition  = TCH4Substrate * EbullitionFrac
      CH4Diffusion   = TCH4Substrate * DiffusionFrac
      CH4Leaching    = TCH4Substrate * LeachingFrac
      CH4Emission    = TCH4Substrate * EmissionFrac

C***********************************************************************
C***********************************************************************
C     Daily integration
C***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
C-----------------------------------------------------------------------
!     Calculate emissions from dissolved CH4 on draining
      if (FLOOD.gt.0.0) then
        CH4Stored = meth%Storage !chp * 12. * 10.	! kgC/ha
      else
        x = CH4Stored * 0.5
        CH4Emission = CH4Emission + x
        CH4Stored =	CH4Stored - x
      endif

      StorageFlux = CH4Stored - CH4Stored_Y
      CH4Stored_Y = CH4Stored

!!     chp 2022-03-23 prevent negative CH4 emissions
!      IF (CH4Emission < -1.E-6) THEN
!        CH4_correction = CH4Emission  ! value is negative
!        CH4Emission = 0.0
!        CH4Diffusion = CH4Diffusion - CH4_correction
!        CH4Production = CH4Production - CH4_correction
!      ENDIF

      CO2Emission    = newCO2Tot - CH4Production - StorageFlux
      CO2Emission = AMIN1(CO2Emission, newCO2Tot)

      CumNewCO2        = CumNewCO2        + newCO2Tot
      CumCH4Emission   = CumCH4Emission   + CH4Emission
      CumCH4Consumpt   = CumCH4Consumpt   + CH4Consumption
      CumCH4Leaching   = CumCH4Leaching   + CH4Leaching
      CumCO2Emission   = CumCO2Emission   + CO2emission

      CH4_balance = newCO2Tot - (CO2emission + CH4Emission + 
     &                       StorageFlux + CH4Leaching + CH4Consumption)

      Cum_CH4_bal = CumNewCO2 - (CumCO2Emission + CumCH4Emission + 
     &                      CH4stored + CumCH4Leaching + CumCH4Consumpt)

!***********************************************************************
!***********************************************************************
!     OUTPUT or SEASEND
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------
      CALL OpMethane(CONTROL, ISWITCH, 
     &  newCO2Tot, CO2emission, TCH4Substrate, StorageFlux, CH4Stored,  
     &  CH4Production, CH4Consumption, CH4Leaching, CH4Emission,
     &  CH4PlantFlux, CH4Ebullition, CH4Diffusion, CH4_balance, 
     &  CumNewCO2, CumCO2Emission, CumCH4Emission, CumCH4Consumpt, 
     &  CumCH4Leaching, Cum_CH4_bal)

C***********************************************************************
C***********************************************************************
C     END OF DYNAMIC IF CONSTRUCT
C***********************************************************************
      ENDIF
C***********************************************************************

      CH4_data % CO2emission = CO2Emission
      CH4_data % CH4Emission = CH4Emission
      CH4_data % CH4Consumption=CH4Consumption
      CH4_data % CH4Leaching = CH4Leaching
      CH4_data % CH4Stored   = CH4Stored

      CH4_data % CumCO2Emission = CumCO2Emission
      CH4_data % CumCH4Emission = CumCH4Emission
      CH4_data % CumCH4Consumpt = CumCH4Consumpt
      CH4_data % CumCH4Leaching = CumCH4Leaching


      RETURN
      END

C=======================================================================
C  OpMethane, Subroutine, C.H.Porter, P. Grace
C  Generates daily output for methane emissions
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  07/02/2021 CHP Written
!=======================================================================

      SUBROUTINE OpMethane(CONTROL, ISWITCH,  
     &  newCO2Tot, CO2emission, TCH4Substrate, StorageFlux, CH4Stored,  
     &  CH4Production, CH4Consumption, CH4Leaching, CH4Emission,
     &  CH4PlantFlux, CH4Ebullition, CH4Diffusion, CH4_balance, 
     &  CumNewCO2, CumCO2Emission, CumCH4Emission, CumCH4Consumpt, 
     &  CumCH4Leaching, Cum_CH4_bal)

!-------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, SUMVALS, YR_DOY
      SAVE

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH

      CHARACTER*1  IDETL, IDETN, ISWNIT, ISWWAT, RNMODE
      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, REPNO
      INTEGER LUN, RUN, YEAR, YRDOY
      LOGICAL FEXIST

      REAL 
     &  newCO2Tot, CO2emission, TCH4Substrate, StorageFlux, CH4Stored,  
     &  CH4Production, CH4Consumption, CH4Leaching, CH4Emission,
     &  CH4PlantFlux, CH4Ebullition, CH4Diffusion, CH4_balance, 
     &  CumNewCO2, CumCO2Emission, CumCH4Emission, CumCH4Consumpt, 
     &  CumCH4Leaching, Cum_CH4_bal

!     Arrays which contain data for printing in SUMMARY.OUT file
      INTEGER, PARAMETER :: SUMNUM = 1  !CO2EM now from GHG module
      CHARACTER*5, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

!-----------------------------------------------------------------------
!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC

      IDETL  = ISWITCH % IDETL
      IDETN  = ISWITCH % IDETN
      ISWWAT = ISWITCH % ISWWAT
      ISWNIT = ISWITCH % ISWNIT
      DAS    = CONTROL % DAS
      YRDOY  = CONTROL % YRDOY
      CALL YR_DOY(YRDOY, YEAR, DOY) 

!***********************************************************************
!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!     Variable heading for N2O.OUT
!-----------------------------------------------------------------------
      IF (ISWWAT == 'N' .OR. ISWNIT == 'N' .OR. IDETL .NE. 'D') RETURN

      IF (IDETN .EQ. 'Y') THEN

        FROP    = CONTROL % FROP
        RNMODE  = CONTROL % RNMODE
        REPNO   = CONTROL % REPNO
        RUN     = CONTROL % RUN

!       Initialize daily growth output file
        CALL GETLUN('Methane.OUT',LUN)
        INQUIRE (FILE = 'Methane.OUT', EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = LUN, FILE = 'Methane.OUT', STATUS = 'OLD',
     &      POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = LUN, FILE = 'Methane.OUT', STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(LUN,'("*Methane Daily output file",/,
     &    "  All values are kg/ha")')
        ENDIF

        IF (RNMODE .NE. 'Q' .OR. RUN .EQ. 1) THEN
          !For first run of a sequenced run, use replicate
          ! number instead of run number in header.
          IF (RNMODE .EQ. 'Q') THEN
            CALL HEADER(SEASINIT, LUN, REPNO)
          ELSE
            CALL HEADER(SEASINIT, LUN, RUN)
          ENDIF

          write(LUN,'(A,//,5A,/,5A,/,5A,/,5A)') 
     & "*** All values are kg[C]/ha. ***",

     & "!              ",
     & "   -------------------------------------------------- Daily ",
     & "kg[C]/ha ---------------------------------------------------",
     & "----------   ------------------  Cumul  kg[C]/ha -----------",
     & "----------",

     & "!              ",
     & "       New   Emitted Substrate   Storage       CH4       CH4",
     & "       CH4       CH4       CH4     Plant       CH4       CH4",
     & "       CH4       New   Emitted       CH4       CH4       CH4",
     & "       CH4",

     & "!              ",
     & "       CO2       CO2   for CH4      Flux    Stored  Produced",
     & "  Consumed   Leached   Emitted      Flux     Ebull    Diffus",
     & "   Balance       CO2       CO2   Emitted  Consumed   Leached",
     & "   Balance",

     & "@YEAR DOY   DAS",
     & "     CO2TD     CO2ED    CH4SBD    CH4SFD    CH4STD    CH4PRD",
     & "    CH4COD    CH4LCD     CH4ED    CH4PLD    CH4EBD    CH4DID",
     & "    CH4BLD     CO2TC     CO2EC     CH4EC    CH4COC    CH4LCC",
     & "    CH4BLC" 
        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      IF (ISWWAT == 'N' .OR. ISWNIT == 'N' .OR. IDETL .NE. 'D') RETURN
      IF (IDETN .EQ. 'Y') THEN

        write(LUN,100)
     &    YEAR, DOY, DAS,
     &  newCO2Tot, CO2emission, TCH4Substrate, StorageFlux, CH4Stored,  
     &  CH4Production, CH4Consumption, CH4Leaching, CH4Emission,
     &  CH4PlantFlux, CH4Ebullition, CH4Diffusion, CH4_balance, 
     &  CumNewCO2, CumCO2Emission, CumCH4Emission, CumCH4Consumpt, 
     &  CumCH4Leaching, Cum_CH4_bal

  100   FORMAT(1X,I4,1X,I3.3,I6,20F10.4)

      ENDIF
!***********************************************************************
!***********************************************************************
!     SEASEND
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
!     Store Summary.out labels and values in arrays to send to
!     OPSUM routines for printing.  Integers are temporarily 
!     saved as real numbers for placement in real array.
      LABEL(1)  = 'CH4EM'; VALUE(1)  = CumCH4Emission  
!     LABEL(2)  = 'CO2EM'; VALUE(2)  = CumCO2emission

!     Send labels and values to OPSUM
      CALL SUMVALS (SUMNUM, LABEL, VALUE) 

      !Close daily output files.
      CLOSE (LUN)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OpMethane
C=======================================================================
! Output Variable        Definition
! CO2EC  Cum CO2 emitted Cumulative CO2 emissions from soil (kg[C]/ha)            .
! CO2ED  CO2emission     Daily CO2 emission (kg/ha)                               .
! CO2TC  CumNewCO2       Cumul. CO2 from surface + soil OM decomp. (kg[C]/ha)     .
! CO2TD  SoilCO2(kg/ha)  Daily CO2 from surface + soil OM decomp. (kg[C]/ha)      .
! CH4BLC Cum_CH4_bal     Cumulative CH4 balance (kg[C]/ha)                        .
! CH4BLD CH4_balance     Daily CH4 Balance (kg[C]/ha)                             .
! CH4COC CumCH4Consumpt  Cumulative CH4 consumption (kg[C]/ha)                    .
! CH4COD CH4Consumption  Daily CH4 Consumption (kg[C]/ha)                         .
! CH4DID CH4Diffusion    Daily CH4 Diffusion (kg[C]/ha)                           .
! CH4EBD CH4Ebullition   Daily CH4 Ebullition (kg[C]/ha)                          .
! CH4EC  Cum CH4 emitted Cumulative methane emitted kg[C]/ha                      .
! CH4ED  CH4Emission     Daily CH4 Emission (kg[C]/ha)                            .
! CH4LCC CumCH4Leaching  Cumulative CH4 leaching (kg[C]/ha)                       .
! CH4LCD CH4Leaching     Daily CH4 Leaching (kg[C]/ha)                            .
! CH4PLD CH4PlantFlux    Daily CH4 PlantFlux (kg[C]/ha)                           .
! CH4PRD CH4Production   Daily CH4 Production (kg[C]/ha)                          .
! CH4SBD TCH4Substrate   Daily portion of new CO2 proportioned to CH4 (kg[C]/ha)  .
! CH4SFD StorageFlux     Daily CH4 Storage flux (kg[C]/ha)                        .
! CH4STD CH4Stored       CH4 stored in soil and floodwater (kg[C]/ha)             .
