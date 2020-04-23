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
      SUBROUTINE MethaneDynamics(CONTROL, SOILPROP,       !Input
     &    FLOOD, SW, RLV, CSubstrate, DRAIN)              !Input

      USE ModuleDefs
     	USE MethaneConstants
	USE MethaneVariables
	IMPLICIT NONE
      SAVE

	INTEGER n1,NLAYR,i,j, DYNAMIC, DAS, LUN
	REAL dlayr(NL),SW(NL),DLL(NL),RLV(NL),CSubstrate(NL),BD(NL),
     &     Buffer(NL,2),afp(NL)
	REAL drain,flood,x,CO2_Cflux,CH4Emission,spd,buffconc,rCO2,
     &     rCH4,TCH4Substrate,rbuff,afpmax,BufferRegenRate,RefHeight,
     &     ProductionFrac,ConsumptionFrac,EmissionFrac,PlantFrac,
     &     EbullitionFrac,DiffusionFrac,LeachingFrac,
     &     CH4Production,CH4Consumption,CH4PlantFlux,CH4Ebullition,
     &     CH4Diffusion,CH4Leaching,CH4Stored

      REAL TCO2, TCH4, FloodCH4
      LOGICAL FEXIST

      TYPE (ControlType) CONTROL
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


!     Initialize daily growth output file
        CALL GETLUN('Methane.OUT',LUN)
!       CLOSE (LUN)

        INQUIRE (FILE = 'Methane.OUT', EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = LUN, FILE = 'Methane.OUT', STATUS = 'OLD',
     &      POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = LUN, FILE = 'Methane.OUT', STATUS = 'NEW')
!         OPEN (UNIT = LUN, FILE = 'Methane.OUT')
          WRITE(LUN,'("*Methane Daily output file")')
        ENDIF

        !Write headers
        CALL HEADER(SEASINIT, LUN, CONTROL%RUN)

      write(LUN,'(5a)') "   DAS",
     &  "      Prod   Consump     Plant     Ebull    Diffus     Leach",
     &  "     Storage Substrate   CH4prod   CH4cons  CH4emis",
     &  " CH4plflux   CH4ebul   CH4diff  CH4leach  StorFlux", 
     &  " itr           Diff"

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
        theta(i) = 1.0		   ! water content 
        epsilon(i) = 0.0         ! air-filled porosity
	  lamda(i) = 0.0		   ! root transmissivity
	  VV(om,i) = 0.0		   ! maximum methanogenesis rate
	  VV(o2,i) = 0.0		   ! maximum aerobic respiration rate
	  VV(ch4,i) = 1.5e-5	   ! maximum CH4 oxidation rate
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
	      rCH4 = 0.0		      ! no CH4 production
	      rCO2 = CSubstrate(i)	  ! aerobic respiration
	      rbuff = -MIN(BufferRegenRate*afp(i)/afpmax*Buffer(i,2),
     &                     Buffer(i,2))	 
	    ELSE
	    ! calculate methane production
            if (buffconc.gt.0.0) then
	        rCH4 = 0.2 * (1.0 - buffconc/24.0)   ! mol C m3/d	 was 0.2
		    rCH4 = rCH4 * dlayr(i)/100.*12.*10.	 ! kgC/ha/d
		  else  
		    rCH4 = CSubstrate(i) / 2.0		 ! kgC/ha/d
	      endif								 
	      rCH4 = max(0.0,min(rCH4,CSubstrate(i)/2.0))
	      rCO2 = CSubstrate(i) - (2.0 * rCH4)
	      if (rCO2.gt.Buffer(i,1)) then
	        rCO2 = Buffer(i,1)
	        rCH4 = (CSubstrate(i) - rCO2)/2.0
	      endif
	      rbuff	= rCO2  
	    ENDIF
	    Buffer(i,1) = Buffer(i,1) - rbuff       ! oxidised buffer pool
	    Buffer(i,2) = Buffer(i,2) + rbuff       ! reduced buffer pool
	    CO2_Cflux = CO2_Cflux + (rCO2 + rCH4)	! total CO2-C flux
! should this subtract rCH4?	    CO2_Cflux = CO2_Cflux + (rCO2 + rCH4)	! total CO2-C flux
	    TCH4Substrate = TCH4Substrate + rCH4	! total CH4 substrate (kgC/ha)

          ! Calculate soil profile parameters for Arah model
!!! n needs a value - takes N=1 value from ModuleDefs. Change name of variable .!!!
!chp 7/11/2017
	    j = i + n
	    z(j) = z(j-1) + dlayr(i)/100.	 ! depth of each layer
	    theta(j) = SW(i)	             ! soil water content (v/v)
	    epsilon(j) = afp(i)				 ! air-filled porosity (v/v)
	    lamda(j) = RLV(i) * lamda_rho	 ! root transmissivity
	    ! maximum rate of methanogenesis (Vm, mol CH4/m3/s)
	    ! (assume all is consumed in a day)
	    ! (i.e. convert kgC/ha/d -->moleCH2O/m3/s)
	    VV(om,j) = rCH4/10./12./(dlayr(i)/100.)/spd
		! maximum rate of aerobic respiration (Vr, mol CO2/m3/s)
	    ! (convert kgC/ha/d -->moleCH2O/m3/s)
		VV(o2,j) = rCH4/10./12./(dlayr(i)/100.)/spd 
	    ! maximum rate of methane oxidation	(Vo, mol CH4/m3/s)
	    VV(ch4,j) = 1.5e-5
		
	  ENDDO

	! Leaching rate
	  Lz = drain * 1.e-3/spd    ! mm/d --> m3/m2/s

C Call the steady-state routine of the Arah model
	  CALL setup(NLAYR+n)
	  CALL SteadyState

c	IF(DAP.eq.20) CALL Report(29)

C Calculate fractions of C in each methane flux
	  IF(TSubstrate.GT.0.0) THEN
		ProductionFrac = meth.Production/TSubstrate
		ConsumptionFrac = meth.Consumption/TSubstrate
	    PlantFrac = meth.RootFluxOut/TSubstrate
	    EbullitionFrac = meth.Ebullition/TSubstrate
	    LeachingFrac = meth.Leaching/TSubstrate
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
	  CH4Production = TCH4Substrate * ProductionFrac
	  CH4Consumption = TCH4Substrate * ConsumptionFrac
	  CH4PlantFlux = TCH4Substrate * PlantFrac
	  CH4Ebullition = TCH4Substrate * EbullitionFrac
	  CH4Diffusion = TCH4Substrate * DiffusionFrac
	  CH4Leaching = TCH4Substrate * LeachingFrac
	  CH4Emission = TCH4Substrate * EmissionFrac		
        CO2_Cflux = CO2_Cflux + (TCH4Substrate * (1.0 - EmissionFrac))

C***********************************************************************
C***********************************************************************
C     Daily integration
C***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
C-----------------------------------------------------------------------
c Calculate emissions from dissolved CH4 on draining
        if (FLOOD.gt.0.0) then
	    CH4Stored = meth.Storage * 12. * 10.	! kgC/ha
	  else
	    x = CH4Stored * 0.5
	    CH4Emission = CH4Emission + x
	    CH4Stored =	CH4Stored - x
	  endif


!***********************************************************************
!***********************************************************************
!     OUTPUT
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
!-----------------------------------------------

      write(LUN,'(i6,6f10.4,f12.3,f10.3,8f10.5,i4,f15.10)') CONTROL.DAS,
     &  ProductionFrac, ConsumptionFrac,
     &  PlantFrac, EbullitionFrac, DiffusionFrac, LeachingFrac,
     &  meth.StorageFlux*86400.*12.*10., TSubstrate*1.e6,
     &    CH4Production,CH4Consumption,CH4Emission,CH4PlantFlux,
     &    CH4Ebullition,CH4Diffusion,CH4Leaching,
     &    meth.StorageFlux*12.*10.,iterations1,difference1

!***********************************************************************
!***********************************************************************
!     SEASEND - seasonal output and close files
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
      CLOSE (LUN)

C***********************************************************************
C***********************************************************************
C     END OF DYNAMIC IF CONSTRUCT
C***********************************************************************
      ENDIF
C***********************************************************************

	RETURN
	END

