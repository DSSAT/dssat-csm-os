C=======================================================================
C SUBROUTINE MethaneDynamics
C
C Subroutine to act as interface between CERES-Rice and Arah methane model.
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written										     R.B.M. March 1998.
C-----------------------------------------------------------------------
C Inputs:
C   DAP        : days after planting
C   dlayr      : width of each soil layer (cm)
C   flood      : depth of flood water (mm)
C   SW		 : soil water content (m3 water/m3 soil)
C   RLV         : root length density (cm root/cm3 soil)
C   BD         : bulk density of each soil layer (g soil cm-3 soil)
C   Csubstrate : available CH2O for methanogenesis (kgCH2O/ha per soil layer.
C   drain      : percolation rate (mm/d)
C Output:
C   CH4flux    : total CH4 emission (kgCH4 ha-1 d-1)
C=======================================================================
      SUBROUTINE MethaneDynamics(CONTROL, ISWITCH, SOILPROP,  !Input
     &    FLOOD, SW, RLV, CSubstrate, DRAIN)                  !Input

      USE ModuleDefs
     	USE MethaneConstants
	USE MethaneVariables
	IMPLICIT NONE
      SAVE

	INTEGER n1,NLAYR,i,j, DYNAMIC
	REAL dlayr(NL),SW(NL),DLL(NL),RLV(NL),CSubstrate(NL),BD(NL),
     &     Buffer(NL,2),afp(NL)
	REAL drain,flood,x,CO2_Cflux,CH4Emission,spd,buffconc,rCO2,
     &     rCH4,TCH4Substrate,rbuff,afpmax,BufferRegenRate,RefHeight,
     &     ProductionFrac,ConsumptionFrac,EmissionFrac,PlantFrac,
     &     EbullitionFrac,DiffusionFrac,LeachingFrac,
     &     CH4Production,CH4Consumption,CH4PlantFlux,CH4Ebullition,
     &     CH4Diffusion,CH4Leaching,CH4Stored
      REAL CumProduction, CumEmission, StorageFrac, CumTCH4Sub
      REAL CumConsumpt, CumLeaching, CH4Stored_ORIG

      REAL TCO2, TCH4, FloodCH4

      TYPE (ControlType) CONTROL
      TYPE (SwitchType) ISWITCH
      TYPE (SoilType) SOILPROP

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
	TCO2 = 0.0
	TCH4 = 0.0
      CumProduction = 0.0
      CumEmission   = 0.0
      CumTCH4Sub    = 0.0
      CH4Stored     = 0.0
      CumConsumpt   = 0.0
      CumLeaching   = 0.0

	spd = 24.*3600.   ! seconds per day

! Define reference height for the Arah model to be the top of the bund
	RefHeight = 100. ! mm

! Soil buffer regeneration rate after drainage
	BufferRegenRate = 0.06 ! 1/d	  0.02

! proportionality constant for root transmissivity and RLV	(0.00015 m air/(m root))
!	lamda_rho = lamdarho  ! 0.00015
	lamda_rho = 0.00015

! Convert the alternate electron acceptors in each layer from mol Ceq/m3 to kgC/ha
!     Temporarily hard-wire Buffer(NL,1) to 26.5 until we read initial values from soil file.
	FloodCH4 = 0.0
	DO i=1,NLAYR
!       Buffer(i,1) = Buffer(i,1) * 12.*(dlayr(i)/100.)*10. ! kg Ceq/ha
!       Temporarily set SAEA (new soil input - Soil Alternative Electron Acceptors) values to 26.5
!       Need to introduce new soil input parameter, or find a way to estimate from other inputs?
        Buffer(i,1) = 26.5 * 12.*(dlayr(i)/100.)*10. ! kg Ceq/ha
	  Buffer(i,2) = 0.0
	ENDDO

! Sample soil profile used in MERES example from IRRI with SAEA values.
!*IBRI910025  IRRI-UNDP   -99      50  
!@SITE        COUNTRY          LAT     LONG SCS FAMILY
! IRRI-UNDP   Philippines    14.18   121.25 Aquandic epiqualf
!@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
!   -99  0.13  12.0  0.60  67.0  1.00  1.00 IB001 IB001 IB001
!@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SAEA
!    10   -99 0.280 0.397 0.412 1.000  -9.0  1.00  1.20  0.43  0.53  0.04  0.13   6.6   -99   -99  26.5
!    20   -99 0.280 0.397 0.412 1.000  -9.0  1.00  1.20  0.43  0.53  0.04  0.13   6.6   -99   -99  26.5
!    30   -99 0.280 0.397 0.412 0.200  -9.0  1.00  1.20  0.43  0.53  0.04  0.13   6.6   -99   -99  26.5
!    40   -99 0.280 0.397 0.412 0.200  -9.0  1.00  1.20  0.43  0.53  0.04  0.13   6.6   -99   -99  26.5
!    50   -99 0.280 0.397 0.412 0.100  -9.0  1.00  1.20  0.43  0.53  0.04  0.13   6.6   -99   -99  26.5

      CALL OpMethane(CONTROL, ISWITCH,  
     &  ProductionFrac, ConsumptionFrac,
     &  PlantFrac, EbullitionFrac, DiffusionFrac, LeachingFrac,
     &  meth%StorageFlux, TSubstrate,
     &  CH4Production,CH4Consumption,CH4Emission,CH4PlantFlux,
     &  CH4Ebullition,CH4Diffusion,CH4Leaching,
     &  iterations1,difference1,
     &  CumProduction, CumEmission, CH4Stored, StorageFrac,
     &  TCH4Substrate, CH4Stored_ORIG)

C***********************************************************************
C***********************************************************************
C     Rate Calculations 
C***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
C-----------------------------------------------------------------------
! Arah model parameters for flood-water layer
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

! Parameters for soil layers
      CO2_Cflux = 0.0
      TCH4Substrate = 0.0

      DO i=1,NLAYR

        ! calculate air-filled porosity (v/v)
        IF (FLOOD.GT.0.0) THEN			 
          afp(i) = 0.0
        ELSE 				 
          afp(i) = max(0.0,1.0 - BD(i)/2.65 - SW(i))
        ENDIF
        afpmax = 1.0 - BD(i)/2.65 - DLL(i)

        ! calculate buffer concentration (molCeq/m3)
        buffconc = Buffer(i,1)/10./12./(dlayr(i)/100.)  

c	if(i.eq.1) write(29,'(i6,3f10.6)') dap,flood,afp(1),buffconc

        ! calculate reoxidisation of buffer if soil is aerated
        IF (afp(i).GT.0.0) THEN
          rCH4 = 0.0              ! no CH4 production
          rCO2 = CSubstrate(i)    ! aerobic respiration
          rbuff = -MIN(BufferRegenRate*afp(i)/afpmax*Buffer(i,2),
     &                     Buffer(i,2))
        ELSE
!         calculate methane production
          if (buffconc.gt.0.0) then
            rCH4 = 0.2 * (1.0 - buffconc/24.0)    ! mol C m3/d	 was 0.2
            rCH4 = rCH4 * dlayr(i)/100.*12.*10.   ! kgC/ha/d
          else  
            rCH4 = CSubstrate(i) / 2.0            ! kgC/ha/d
          endif
          rCH4 = max(0.0,min(rCH4,CSubstrate(i)/2.0))
          rCO2 = CSubstrate(i) - (2.0 * rCH4)
          if (rCO2.gt.Buffer(i,1)) then
            rCO2 = Buffer(i,1)
            rCH4 = (CSubstrate(i) - rCO2)/2.0
          endif
          rbuff = rCO2  
        ENDIF
        Buffer(i,1) = Buffer(i,1) - rbuff       ! oxidised buffer pool
        Buffer(i,2) = Buffer(i,2) + rbuff       ! reduced buffer pool

        CO2_Cflux = CO2_Cflux + (rCO2 + rCH4)	! total CO2-C flux
! should this subtract rCH4?	    CO2_Cflux = CO2_Cflux + (rCO2 + rCH4)	! total CO2-C flux

!       Total CH4 substrate (kgC/ha)
        TCH4Substrate = TCH4Substrate + rCH4  

!       TEMP CHP
        WRITE(5555, '(i7,i4,3f10.4)') 
     &    control.yrdoy, i, csubstrate(i), rch4, rco2

!       Calculate soil profile parameters for Arah model
        j = i + n1
        z(j) = z(j-1) + dlayr(i)/100.     ! depth of each layer
        theta(j) = SW(i)                  ! soil water content (v/v)
        epsilon(j) = afp(i)               ! air-filled porosity (v/v)
        lamda(j) = RLV(i) * lamda_rho     ! root transmissivity
        ! maximum rate of methanogenesis (Vm, mol CH4/m3/s)
        ! (assume all is consumed in a day)
        ! (i.e. convert kgC/ha/d -->moleCH2O/m3/s)
        VV(om,j) = rCH4/10./12./(dlayr(i)/100.)/spd
        ! maximum rate of aerobic respiration (Vr, mol CO2/m3/s)
        ! (convert kgC/ha/d -->moleCH2O/m3/s)
        VV(o2,j) = rCH4/10./12./(dlayr(i)/100.)/spd 
        ! maximum rate of methane oxidation   (Vo, mol CH4/m3/s)
        VV(ch4,j) = 1.5e-5
      ENDDO

!     Leaching rate
      Lz = drain * 1.e-3/spd    ! mm/d --> m3/m2/s

C Call the steady-state routine of the Arah model
!     TSubstrate comes out of these routines, ~10% of TCH4Substrate for IRLB9701.RIX, trt 4
      CALL setup(NLAYR+n1)
      CALL SteadyState

c     IF(DAP.eq.20) CALL Report(29)

C Calculate fractions of C in each methane flux
      IF(TSubstrate.GT.0.0) THEN
        ProductionFrac  = meth%Production /TSubstrate
        ConsumptionFrac = meth%Consumption/TSubstrate
        PlantFrac       = meth%RootFluxOut/TSubstrate
        EbullitionFrac  = meth%Ebullition /TSubstrate
        LeachingFrac    = meth%Leaching   /TSubstrate
      ELSE
        ProductionFrac = 0.0
        ConsumptionFrac = 0.0
        PlantFrac = 0.0
        EbullitionFrac = 0.0
        LeachingFrac = 0.0
      ENDIF
      EmissionFrac = ProductionFrac - ConsumptionFrac - LeachingFrac
      DiffusionFrac = EmissionFrac - (PlantFrac + EbullitionFrac)

c Calculate actual flux rates (kgC/ha/d) based on CERES-Rice substrate calculations
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
c Calculate emissions from dissolved CH4 on draining
      if (FLOOD.gt.0.0) then
        CH4Stored = meth%Storage !chp* 12. * 10.	! kgC/ha
      else
        x = CH4Stored * 0.5
        CH4Emission = CH4Emission + x
        CH4Stored =	CH4Stored - x
!        CH4Stored_ORIG =	CH4Stored_ORIG - x
      endif

      CumTCH4Sub    = CumTCH4Sub + TCH4Substrate
      CumProduction = CumProduction + CH4Production
      CumEmission   =  CumEmission  + CH4Emission
      CumConsumpt   = CumConsumpt + CH4Consumption
      CumLeaching   = CumLeaching + CH4Leaching

!      CH4Stored = CumTCH4Sub - (CumEmission + CumConsumpt + CumLeaching)

!     CO2_Cflux = CO2_Cflux + (TCH4Substrate * (1.0 - EmissionFrac))
      CO2_Cflux = CO2_Cflux + (TCH4Substrate * (1.0 - ProductionFrac))

!***********************************************************************
!***********************************************************************
!     OUTPUT or SEASEND
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------
      CALL OpMethane(CONTROL, ISWITCH,  
     &  ProductionFrac, ConsumptionFrac,
     &  PlantFrac, EbullitionFrac, DiffusionFrac, LeachingFrac,
     &  meth%StorageFlux, TSubstrate,
     &  CH4Production,CH4Consumption,CH4Emission,CH4PlantFlux,
     &  CH4Ebullition,CH4Diffusion,CH4Leaching,
     &  iterations1,difference1,
     &  CumProduction, CumEmission, CH4Stored, StorageFrac,
     &  TCH4Substrate, CH4Stored_ORIG)

C***********************************************************************
C***********************************************************************
C     END OF DYNAMIC IF CONSTRUCT
C***********************************************************************
      ENDIF
C***********************************************************************

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
     &  ProductionFrac, ConsumptionFrac,
     &  PlantFrac, EbullitionFrac, DiffusionFrac, LeachingFrac,
     &  StorageFlux, TSubstrate,
     &  CH4Production,CH4Consumption,CH4Emission,CH4PlantFlux,
     &  CH4Ebullition,CH4Diffusion,CH4Leaching,
     &  iterations1,difference1,
     &  CumProduction, CumEmission, CH4Stored, StorageFrac,
     &  TCH4Substrate, CH4Stored_ORIG)
!-------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE
      SAVE
!-----------------------------------------------------------------------

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH

      CHARACTER*1  IDETN, ISWNIT, ISWWAT, RNMODE
      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, REPNO
      INTEGER LUN, RUN, YEAR, YRDOY
      LOGICAL FEXIST

      REAL ProductionFrac, ConsumptionFrac,
     &  PlantFrac, EbullitionFrac, DiffusionFrac, LeachingFrac,
     &  StorageFlux, TSubstrate,
     &  CH4Production,CH4Consumption,CH4Emission,CH4PlantFlux,
     &  CH4Ebullition,CH4Diffusion,CH4Leaching,difference1
      REAL CumProduction, CumEmission, CH4Stored
      REAL StorFlux, StorageFrac, TCH4Substrate, CH4Stored_ORIG
      INTEGER iterations1

!     Arrays which contain data for printing in SUMMARY.OUT file
      INTEGER, PARAMETER :: SUMNUM = 2
      CHARACTER*5, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

!-----------------------------------------------------------------------
!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC

      IDETN  = ISWITCH % IDETN
      ISWWAT = ISWITCH % ISWWAT
      ISWNIT = ISWITCH % ISWNIT

      IF (ISWWAT == 'N' .OR. ISWNIT == 'N') RETURN

      DAS     = CONTROL % DAS
      YRDOY   = CONTROL % YRDOY
      CALL YR_DOY(YRDOY, YEAR, DOY) 

!***********************************************************************
!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
C     Variable heading for N2O.OUT
C-----------------------------------------------------------------------
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
          WRITE(LUN,'("*Methane Daily output file")')
        ENDIF

        IF (RNMODE .NE. 'Q' .OR. RUN .EQ. 1) THEN
          !For first run of a sequenced run, use replicate
          ! number instead of run number in header.
          IF (RNMODE .EQ. 'Q') THEN
            CALL HEADER(SEASINIT, LUN, REPNO)
          ELSE
            CALL HEADER(SEASINIT, LUN, RUN)
          ENDIF

          write(LUN,'(6a)') 
     & "@YEAR DOY   DAS",
     &  "    CH4PRF    CHPCOF    CH4PLF    CH4EBF    CH4DIF    CH4LCF",
     &  "      CH4SFL    CH4SUB    CH4PRD    CH4COD    CH4EMD",
     &  "    CH4PLD    CH4EBD    CH4DID    CH4LCD    CH4SFD", 
     &  "    CH4PRC    CH4EMC   CH4STOR    CH4SFR   TCH4SUB",
     &  "    STORIG CH4ITR          CH4DI"

        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
      IF (IDETN .EQ. 'Y') THEN

        IF (ABS(StorageFlux) .LT. 1.E-8) THEN
          StorFlux = 0.0
        ELSE
          StorFlux = StorageFlux
        ENDIF

        write(LUN,100)
     &    YEAR, DOY, DAS,
     &    ProductionFrac, ConsumptionFrac,
     &    PlantFrac, EbullitionFrac, DiffusionFrac, LeachingFrac,
     &    StorFlux*86400.*12.*10., TSubstrate*1.e6,
     &    CH4Production,CH4Consumption,CH4Emission,CH4PlantFlux,
     &    CH4Ebullition,CH4Diffusion,CH4Leaching,
     &    StorFlux*12.*10.,
     &    CumProduction, CumEmission, CH4Stored, StorageFrac,
     &    TCH4Substrate, CH4Stored_ORIG,
     &    iterations1,difference1

  100   FORMAT(1X,I4,1X,I3.3,I6,6f10.4,f12.3,f10.3,14f10.5,i7,f15.10)

      ENDIF
!***********************************************************************
!***********************************************************************
!     SEASEND
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
!      IF (INDEX('AD',IDETL) == 0) RETURN
      !Close daily output files.
      CLOSE (LUN)

!     Store Summary.out labels and values in arrays to send to
!     OPSUM routines for printing.  Integers are temporarily 
!     saved as real numbers for placement in real array.
      LABEL(1)  = 'CH4EC'; VALUE(1)  = CH4Emission  

!     Send labels and values to OPSUM
      CALL SUMVALS (SUMNUM, LABEL, VALUE) 

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
! CH4PRF ProductionFrac  Daily CH4 Production fraction      
! CH4COF ConsumptionFrac Daily CH4 Consumption fraction
! CH4PLF PlantFrac       Daily CH4 Plant fraction      
! CH4EBF EbullitionFrac  Daily CH4 Ebullition fraction 
! CH4DIF DiffusionFrac   Daily CH4 Diffusion fraction  
! CH4LCF LeachingFrac    Daily CH4 Leaching fraction  
! CH4SFL StorFlux        Daily CH4 Storage flux
! CH4SUB TSubstrate      Daily CH4 TSubstrate
! CH4PRD CH4Production   Daily CH4 Production  
! CH4COD CH4Consumption  Daily CH4 Consumption 
! CH4EMD CH4Emission     Daily CH4 Emission    
! CH4PLD CH4PlantFlux    Daily CH4 PlantFlux   
! CH4EBD CH4Ebullition   Daily CH4 Ebullition  
! CH4DID CH4Diffusion    Daily CH4 Diffusion   
! CH4LCD CH4Leaching     Daily CH4 Leaching    
! CH4SFD StorFlux        Daily CH4 Storage flux

! CH4PRC CumProduction   Cumulative CH4 production
! CH4EMC CumEmission     Cumulative CH4 emissions


! CH4ITR iterations1     Daily iterations1    
! CH4DI  difference1     Daily difference1    
