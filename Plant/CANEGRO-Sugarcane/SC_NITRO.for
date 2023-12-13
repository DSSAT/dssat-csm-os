c     ---------------------------------------------------------------
c                                 SC_NITROGEN
c     ---------------------------------------------------------------
c     A subroutine for calculating Nitrogen demand, stress, etc
c     for the SASRI Canegro Plant Module in the DSSAT CSM
c
c     The Nitrogen follows the mass allocations in the model - for 
c     example, if the plant is stressed and more energy is invested
c     in the root system, the root mass increases and with it, the
c     demand for N (as N-concentration might fall below the critical 
c     level).
c     ---------------------------------------------------------------
c     July 2008
c     Michael van der Laan (michael.vanderlaan@sugar.org.za)
c     Matthew Jones,  (matthew.jones@sugar.org.za)
c     
c     
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     (c) South African Sugarcane Research Institute
c     Mount Edgecombe, 4300
c     ---------------------------------------------------------------

      SUBROUTINE SC_NITRO(ISWITCH, 
     &    TOPS_MASS, ROOTS_MASS, STALKS_MASS, 
     &    DEADLF_MASS, NO3, NH4,UNO3, UNH4, RLV,
     &    SW, N_STRESS, SoilProp, Control, 
     &    CaneCrop, SENESCE, ROOTNCONC, TOP_N, YRPLT)

!     2023-04-08 FO Commented it out variables not used: RWU, XHLAI, 
!                   SENESCE, SOIL, N_STORAGE, MAXNO3U, MAXNH4U,
!                   TUNO3, TUNH4, SUNO3, SUNH4, IUNO3, IUNH4,
!                   NUF, RESMIN, NO3_UPTAKE, NH4_UPTAKE, NH4_DEMAND,
!                   NO3_SUPPLY, NH4_SUPPLY, PSV_UPTAKE, N_DEMAND,
!                   N_UPTAKE, TOT_DEMAND, N_ALLOC_WEIGHTS, 
!                   N_ALC_TOTAL, N_FUNC, POT_CUM_MASS, RT_ACT_FAC,
!                   BUFF_POW, NH4_CONC, DLF_NAVAIL, SENESF,
!                   STDAYC, BEST, GLFMX1, CWSI, DLF_NCONC, GLFMX2, 
!                   INO3_SUPPLY, INH4_SUPPLY, NO3_AVAIL, NH4_AVAIL, 
!                   RT_ACT_MASS, LL, SAT, WR
c     Define DSSAT composite variables:
c     [Taken from MZ_CERES.for]

      USE ModuleDefs

c     Define CANEGRO composite variables:
      USE CNG_ModuleDefs

c     Use the composite variable defn:
      USE N_TYPES
      
c     All variables to be declared:
      IMPLICIT NONE
c     and saved between subroutine calls:
      EXTERNAL SC_OPNIT

      SAVE


c     Variable declarations:
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

c         This belongs in the parameter section, but I
c         am not sure how to initialise composite
c         variables as PARAMETER...:

c         N-stress response to N-concentration parameters:
c         X: N-concentration (kg/kg/ha)
c         Y: N-stress factor (0-1) 
c            [0 = maximum stress; 1 = no stress]
          TYPE(N_PARAM_TYPE) N_PARAMS(NUM_COMPS)
          TYPE (SOILTYPE) SoilProp
!          TYPE(CNG_SoilType) Soil
          TYPE (SwitchType) ISWITCH
          TYPE(CANECROPTYPE) CaneCrop
          TYPE(ResidueType)  SENESCE     !MvdL

c     ARGUMENT VARIABLES
c     ::::::::::::::::::
c         These statments give the intention for use of these
c         variables.  The actual variables are declared
c         elsewhere.
          INTENT(IN) ::   TOPS_MASS, ROOTS_MASS, STALKS_MASS, 
     &                    DEADLF_MASS, NO3, NH4, CONTROL,
     &                    SoilProp, SW
          INTENT(OUT) ::  N_STRESS, UNO3, UNH4

c     STATE VARIABLES
c     :::::::::::::::

c         DSSAT composite variables:
c         [Taken from MZ_CERES.for]
c         ::::::::::::::::::::::::::
          TYPE (ControlType) CONTROL
          INTEGER	 YRPLT

!          REAL XHLAI

c         N_POOL: An array of Nitrogen masses for each plant
c         component.  This is updated in the integration
c         section each day.
c         Units:  kg/ha
          REAL N_POOL(NUM_COMPS)
          REAL TOP_N

c         N_CONC: An array of N-concentration for each plant
c         component.
c         Units:  kg/kg(/ha)
          REAL N_CONC(NUM_COMPS)

c         N_STRESS: An array of N-stresses for each plant 
c         component.  These are used to reduce
c         various processes (photosynthesis, mainly)
c         Units:  None
          REAL N_STRESS       
!         (NUM_COMPS) MvdL: lets just go with one for now

c         TOPS_MASS: Dry mass of tops (t/ha), input
          REAL TOPS_MASS
c         ROOTS_MASS: Dry mass of roots (t/ha), input
          REAL ROOTS_MASS
c         STALKS_MASS: Dry mass of stalks (t/ha), input
          REAL STALKS_MASS
c         TRASH_MASS: Dry mass of trash (t/ha), input
          REAL DEADLF_MASS

c         MASSES: Dry masses of each plant component, 
c         stored in array form to facilitate
c         processing in loops
c         Units:  t/ha
          REAL MASSES(NUM_COMPS)

c         YEST_DMDEAD: dry matter of trash, yesterday (used
c         to calculate increase in trash today, to determine
c         remobilisation of N from senesced leaves:
          REAL YEST_DMDEAD

c         SENESCE_DM: dry mass of material senesced since
c         yesterday:
          REAL SENESCE_DM

c         SW - soil water content for each layer
          REAL SW(NL)

c		N Storage pool for the remobilization of N, 
c		can use up to 10% of it per day. And it will consist 
c		of all 'excess' N, (any N over and above Crit_N)
!		REAL N_STORAGE

c	    REAL RESMIN
!	    REAL MAXNO3U
!	    REAL MAXNH4U
!	    REAL TUNO3, TUNH4

          REAL ACC_UPTAKE
          REAL TOT_N_POOL

c     RATE VARIABLES
c     ::::::::::::::
c         Amount of N in nitrate (NO3) and ammonium (NH4)
c         forms, g [N]/kg soil/ha, per layer
          REAL NO3(NL), NH4(NL)
!          REAL SUNO3(NL), SUNH4(NL)
!	    REAL IUNO3(NL), IUNH4(NL)
		
!	    REAL NUF
!	    REAL RESMIN(NL)

c         Mass of Nitrate taken up (kg N/ha/day):
!          REAL NO3_UPTAKE
!          REAL NH4_UPTAKE

c         Mass of Ammonium taken up
!          REAL NH4_DEMAND

!          REAL INO3_SUPPLY
!          REAL NO3_SUPPLY
!          REAL NH4_SUPPLY

c         Passive N-uptake (mass flow and remobilised N)
!          REAL PSV_UPTAKE

c         Amount of N in nitrate (NO3) and ammonium (NH4)
c         forms, kg[N]/ha, TAKEN UP BY THE PLANT
c         NL is the number of layers, defined in module
c         definitions for DSSAT CSM
c         This is a model output
          REAL UNO3(NL), UNH4(NL)

c         Root water uptake, per layer per day:
!          REAL RWU(NL)
	    
		  REAL RLV(NL)

c         Allocation of N to each plant component today (kg/day)
          REAL N_ALLOC(NUM_COMPS)
	    REAL TOT_N_ALLOC

c         N_SENESCE: mass of Nitrogen remobilised from senesced
c         leaves (trash).  This is distributed together with N_UPTAKE
c		(Going to add this to the storage pool)
          REAL N_SENESCE

c         N_DEMAND: mass of Nitrogen demanded by the crop, as
c         determine by N_stress
c         UNITS: kg/ha
!          REAL N_DEMAND(NUM_COMPS)

c         TOT_N_DEMAND: Total N-demand, kg/ha/day:
          REAL TOT_N_DEMAND


c     LOCAL VARIABLES
c     :::::::::::::::

c         N_UPTAKE: Nitrogen uptake by the plant as a whole
c         (something like 'available' N), 
c         UNITS:    kg/ha/day
!          REAL N_UPTAKE

c         Weightings of N-demand by stress
          REAL N_DEMAND_WEIGHTS(NUM_COMPS)

c         Total N-demand, total dry mass of plant components
!          REAL TOT_DEMAND
          REAL TOT_MASS

c         Weightings of N-demand by mass
          REAL N_MASS_WEIGHTS(NUM_COMPS)

c         Allocation weights (combining mass and demand)
!          REAL N_ALLOC_WEIGHTS(NUM_COMPS)

c         Total allocation (for normalising)
!          REAL N_ALC_TOTAL

c         Counter
          INTEGER I

c         Return value from N_FUNC function
!          REAL N_FUNC

c         Run mode:
          INTEGER DYNAMIC

c         Output unit number for CSV file:
!          INTEGER N_OUT

c		Added by MvdL
!           Potential cumulative biomass (above and below ground)
!		REAL POT_CUM_MASS        
!           Factor that accounts for enhanced N uptake during 
!           early growth stages
!		REAL RT_ACT_FAC          
!           Relates to soil NH4 adsorption
!		REAL BUFF_POW            
!           Soluble ammonium concentration
!		REAL NH4_CONC(NL)         
!           Stress index used to calculate stress factor
		REAL TOP_N_STRESS_INDEX  
!           Potential NO3 and NH4 uptake
		REAL POT_N_SUPPLY        
         
!		REAL DLF_NAVAIL
		REAL UPTK_RATIO
!		REAL SENESF
!		REAL STDAYC
!		REAL BEST
!		REAL GLFMX1
		REAL NITROGEN_CANOPY_EXPANSION_FACTOR
!		REAL CWSI
!		REAL DLF_NCONC
!		REAL GLFMX2
		REAL TOP_N_CONC
		REAL ROOTNCONC

		INTEGER L

!		REAL INO3_SUPPLY,INH4_SUPPLY
!		REAL NO3_AVAIL(NL)
!		REAL NH4_AVAIL(NL)
		REAL DEM_SUP_RATIO
		
		REAL ABVGRND_N_MASS
	

c        FUTURE USER INPUTS (will be hard-coded for now)
!        Crop mass below which increased root activity occurs
!         REAL RT_ACT_MASS         
         
c      New uptake approach variables
      REAL SMDFR    
      REAL RFAC     
      REAL FNH4     
      REAL SHF (NL)
      REAL FNO3   
      REAL RNH4U(NL)
      REAL RNO3U(NL)           
      REAL TRNU     
!      REAL LL(NL)
!      REAL SAT(NL)
!      REAL WR(NL)

c     ---------------------------------------------------------------
c                         CODE
c     ---------------------------------------------------------------

      DYNAMIC = CONTROL%DYNAMIC


c     ===============================================================
      IF (DYNAMIC.EQ.RUNINIT) THEN
c     INITIALISE
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

          
		ACC_UPTAKE = 0.0
        ABVGRND_N_MASS = 0.0
        ROOTNCONC = 0.01
        
c         ------------  TOPS  ---------------------------------   
                                                                  
          N_PARAMS(TOPS)%MIN_CONC = 0.004        
          N_PARAMS(TOPS)%INIT_CONC = 0.03                           

c         ------------  STALKS  -------------------------------

          N_PARAMS(STALKS)%MIN_CONC = 0.003
          N_PARAMS(STALKS)%INIT_CONC = 0.03                         

c         ------------  ROOTS  -------------------------------

          N_PARAMS(ROOTS)%MIN_CONC = 0.003
          N_PARAMS(ROOTS)%INIT_CONC = 0.012                          
        
c         ------------  TRASH  -------------------------------

          N_PARAMS(DEADLF)%MIN_CONC = 0.004                         

c         ----------------------------------------------------

! HBD Apr 2023 included these initiations in runinit because values started high in any simulation
!           N_CONC(TOPS) = 0
!           N_CONC(ROOTS) = 0
!           N_CONC(STALKS) = 0
!           N_CONC(DEADLF) = 0
!
      CALL SC_OPNIT(CONTROL, ISWITCH,
     &    YRPLT, SoilProp%NLAYR, SENESCE,
     &    MASSES,
     &    N_POOL, ! ok
!     &    N_ALLOC,! ok
     &    N_STRESS, ! ok?
!     &    PSV_UPTAKE, FO has commented it in SC_NITRO
!     &    N_SENESCE, ! ok TODO check if this is = to SENESCE % ResE(n,1) (HBD)
     &    N_CONC, ! ok
!     &    TOT_N_DEMAND, POT_N_SUPPLY, !ok TODO is it necessary to be exported? (HBD)
!     &    TUNO3, ! FO has commented it in SC_NITRO; TODO check necessity here (HBD)
     &    ABVGRND_N_MASS, 
!     &    TOT_N_POOL, !ok,ok
     &    ACC_UPTAKE)
     
      ELSE IF (DYNAMIC .EQ. SEASINIT) THEN

          N_STRESS = 1

c         For all components:
          DO I=1,NUM_COMPS 
c             Set N-stress to 1 (no stress)  
c             Set concentration to non-stressed level:
              N_CONC(I) = N_PARAMS(I)%INIT_CONC   
          ENDDO
           
! HBD Apr 2023 tests included these initiations in at planting/ratooning because values started high in any simulation
! - I let the roots in this way currently, but ratoon carry-over effects must be accomodated in future
           !N_CONC(TOPS) = 0
           !N_CONC(STALKS) = 0
           !N_CONC(DEADLF) = 0

c          ## Set N_POOL sizes to something acceptable?
! HBD Apr 2023:
! - N_POOL for leaves and stalks, unless it is a pre-sprouted cane or 'seed' (future need?)
! - A sett (stalk piece) should be connected with the mass and its [N] for the planting material
! - I let the roots in this way currently, but ratoon carry-over effects must be accomodated in future
           N_POOL(TOPS) = N_PARAMS(TOPS)%INIT_CONC * 0.0000002849 * 1000
           N_POOL(ROOTS) = N_PARAMS(ROOTS)%INIT_CONC * 0.00001111 * 1000
           N_POOL(STALKS) = N_PARAMS(STALKS)%INIT_CONC * 0.000001 * 1000
           
c         Set yesterday's trash amount to 0
          YEST_DMDEAD = 0.

c         Open output file and write headings:
c         ------------------------------------
!          CALL GETLUN('SCNITR', N_OUT)
!          OPEN(FILE='N_OUT.CSV', UNIT=N_OUT) !HBD: THIS WAS A LOCAL THING
!
!                  WRITE(N_OUT, '(29(A12,2H, ))') 
!     &           'YRDOY',
!     &           'TOP_MASS','STK_MASS','ROOT_MASS','TRASH_MASS',
!     &           'TOP_POOL','STK_POOL','ROOT_POOL','TRASH_POOL',
!     &           'TOP_ALLOC','STK_ALLOC','ROOT_ALLOC','TRASH_ALLOC',
!     &           'TOP_STRESS',
!     &           'PSV_UPTAKE','N_SENESCE', 'N_C_TOPS', 'N_C_STK',
!     &           'N_C_ROOT', 'N_C_TRASH', 'TOT_N_DEMAND',
!     &           'POT_N_SUPPLY', 'TUNO3', 'ABVGRND_N_MASS', 
!     &           'TOT_N_POOL', 'ACC_UPTAKE'
c         ------------------------------------

      CALL SC_OPNIT(CONTROL, ISWITCH,
     &    YRPLT, SoilProp%NLAYR, SENESCE,
     &    MASSES,
     &    N_POOL, ! ok
!     &    N_ALLOC,! ok
     &    N_STRESS, ! ok?
!     &    PSV_UPTAKE, FO has commented it in SC_NITRO
!     &    N_SENESCE, ! ok TODO check if this is = to SENESCE % ResE(n,1) (HBD)
     &    N_CONC, ! ok
!     &    TOT_N_DEMAND, POT_N_SUPPLY, !ok TODO is it necessary to be exported? (HBD)
!     &    TUNO3, ! FO has commented it in SC_NITRO; TODO check necessity here (HBD)
     &    ABVGRND_N_MASS, 
!     &    TOT_N_POOL, !ok,ok
     &    ACC_UPTAKE)

      ELSE IF (DYNAMIC .EQ. RATE) THEN
c     ===============================================================
c     RATES
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c         Copy input mass values to array for ease of calc:
          MASSES(TOPS)   = TOPS_MASS         !tons/ha
          MASSES(STALKS) = STALKS_MASS
          MASSES(ROOTS)  = ROOTS_MASS
          MASSES(DEADLF) = DEADLF_MASS


c         Calculate N-concentrations for each component:
c         ::::::::::::::::::::::::::::::::::::::::::::::
c         Conc = N_pool/mass, if mass > 0
            IF (MASSES(TOPS) > 0.00001) THEN
                N_CONC(TOPS) = N_POOL(TOPS)/(MASSES(TOPS) * 1000)   
		ENDIF
		IF (MASSES(STALKS) > 0.00001) THEN
                N_CONC(STALKS) = N_POOL(STALKS)/(MASSES(STALKS) * 1000)
		ENDIF
		IF (MASSES(ROOTS) > 0.00001) THEN
                N_CONC(ROOTS) = N_POOL(ROOTS)/(MASSES(ROOTS) * 1000)  
		ENDIF
 
!           MvdL this is to send to Roots to calculate root N senescence   
            ROOTNCONC = N_CONC(ROOTS)    
c      ***********POTENTIAL N UPTAKE*******************
         
c		Calculate potential per layer extraction:
C-----------------------------------------------------------------------
C     Calculate potential N supply in soil layers with roots (TRNU)
C-----------------------------------------------------------------------

      TRNU = 0
      POT_N_SUPPLY = 0
      
      DO L=1, SoilProp%NLAYR
         IF (RLV(L) .NE. 0.0) THEN
c            L1 = L
            SHF(L) = SoilProp%WR(L)   !MvdL SHF must = SoilProp%WR
C
C        The following code was written by JTR to simplify the code for the
C        generic model and to make the functions more like the water uptake
C        functions.  Done on June 28, 1994.
C
C        New water content dependent factor for uptake and new uptake
C        methods that work like the soil water uptake  -- JTR 6/94
C
            SMDFR    = 1.5-6.0*((SW(L)-SoilProp%LL(L))/
     &                 (SoilProp%SAT(L)-SoilProp%LL(L))-0.5)**2
            SMDFR    = AMAX1 (SMDFR,0.0)
            SMDFR    = AMIN1 (SMDFR,1.0)
            RFAC     = 1.0-EXP(-8.0*RLV(L))
!           SHF = relative distrubution in soil Canegro's WR??
            FNH4     = SHF(L)*0.075             
            FNO3     = SHF(L)*0.075
            RNH4U(L) = RFAC*SMDFR*FNH4*(NH4(L)-0.5)*SoilProp%DLAYR(L)
!           Potential nitrate uptake
            RNO3U(L) = RFAC*SMDFR*FNO3*NO3(L)*SoilProp%DLAYR(L)      
            RNH4U(L) = MAX(RNH4U(L),0.0)
            RNO3U(L) = MAX(RNO3U(L),0.0) 
            TRNU     = TRNU + RNO3U(L) + RNH4U(L)
         ENDIF
      END DO

        POT_N_SUPPLY = TRNU
        
C       MvdL: A new approach to deal with senesced leaves:
C       Leaves die at min conc - the rest is assumed to be translocated to
C       living leaves
c TODO: are these phases the same between v4.5 and v4.8? (HBD?)
        SENESCE_DM = MAX(0., DEADLF_MASS - YEST_DMDEAD)
        
        IF (SENESCE_DM.GT.0) THEN
        N_SENESCE = SENESCE_DM * N_PARAMS(TOPS)%MIN_CONC
        N_POOL(TOPS) = N_POOL(TOPS) - N_SENESCE  
        ENDIF

c      ************************N DEMAND*******************
        IF (CaneCrop%GROPHASE.EQ.1) THEN  !Emerging
        N_PARAMS(TOPS)%CRIT_CONC = 0.016
        N_PARAMS(TOPS)%OPT_CONC = 0.018
        
        !N_PARAMS(STALKS)%CRIT_CONC = 0.005  
        N_PARAMS(STALKS)%OPT_CONC = 0.005
        
        !N_PARAMS(ROOTS)%CRIT_CONC = 0.005
        N_PARAMS(ROOTS)%OPT_CONC = 0.005
        
        ELSEIF (CaneCrop%GROPHASE.EQ.2) THEN  !Tillering
        N_PARAMS(TOPS)%CRIT_CONC = 0.011
        N_PARAMS(TOPS)%OPT_CONC = 0.013
        
        N_PARAMS(STALKS)%OPT_CONC = 0.003
        
        N_PARAMS(ROOTS)%OPT_CONC = 0.005
        
        ELSEIF (CaneCrop%GROPHASE.EQ.3) THEN   !Stalk emergence
        N_PARAMS(TOPS)%CRIT_CONC = 0.009
        N_PARAMS(TOPS)%OPT_CONC = 0.01
        
        N_PARAMS(STALKS)%OPT_CONC = 0.003
        
        N_PARAMS(ROOTS)%OPT_CONC = 0.003
        
        ELSEIF (CaneCrop%GROPHASE.EQ.4) THEN  !Peak stalk population
        N_PARAMS(TOPS)%CRIT_CONC = 0.007
        N_PARAMS(TOPS)%OPT_CONC = 0.008
        
        N_PARAMS(STALKS)%OPT_CONC = 0.002
        
        N_PARAMS(ROOTS)%OPT_CONC = 0.002
        ENDIF
        
        IF (MASSES(TOPS).GT.0) THEN
        
       TOT_N_DEMAND = 0
        
		DO I=1, NUM_COMPS
			N_MASS_WEIGHTS(I) = 0.
              IF (N_CONC(I) .LT. N_PARAMS(I)%OPT_CONC) THEN
				N_MASS_WEIGHTS(I) = (MASSES(I) * 1000) *
     &				(N_PARAMS(I)%OPT_CONC - N_CONC(I))
                  TOT_N_DEMAND = TOT_N_DEMAND + N_MASS_WEIGHTS(I)
	        ENDIF
          ENDDO

c		Total mass of all components
		TOT_MASS   = 0.

c		Calc total mass:
		DO I=1, NUM_COMPS
			TOT_MASS = TOT_MASS + MASSES(I)
		ENDDO
c	    (Total Mass shouldn't include dead matter, only living.)
		TOT_MASS = TOT_MASS - MASSES(DEADLF)
              
c		Determining weighting for demand...
		DO I=1, NUM_COMPS
			N_DEMAND_WEIGHTS(I) = 0.
              IF ((TOT_MASS  .GT. 0.000001) .AND.
     &			(TOT_N_DEMAND .GT. 0.000001)) THEN
					N_DEMAND_WEIGHTS(I) = (N_MASS_WEIGHTS(I)
     &					/ TOT_N_DEMAND)
              ENDIF
          ENDDO
              
c      ********************N STRESS FACTOR***************

		TOP_N_CONC = N_POOL(TOPS) / (MASSES(TOPS)*1000)
		
		
c      Top_Nitrogen_Stress_Factor = 1  '1: no stress; 0: full stress
c      Top_Nitrogen_Stress_Index = 0 '0: no stress; 1: full stress

		TOP_N_STRESS_INDEX = 0
		N_STRESS = 1
       
        IF (TOP_N_CONC.LT.N_PARAMS(TOPS)%CRIT_CONC) THEN
       
		TOP_N_STRESS_INDEX = 1. - (TOP_N_CONC - 
     &		N_PARAMS(TOPS)%MIN_CONC) / 
     &		(N_PARAMS(TOPS)%CRIT_CONC - N_PARAMS(TOPS)%MIN_CONC)
		IF (TOP_N_STRESS_INDEX .LT. 0.) THEN
			TOP_N_STRESS_INDEX = 0.
		ELSEIF (TOP_N_STRESS_INDEX .GT. 1.) THEN
     	 		TOP_N_STRESS_INDEX = 1.
		ENDIF
      
		N_STRESS = 1. - (TOP_N_STRESS_INDEX ** 2.)   
		
		ENDIF




c     *************ACTUAL N UPTAKE***********************
      
c		MvdL: apportion N to respective plant parts and remove from soil
	!	N_DEMAND_WEIGHTS(I) = 0.
      
c		MvdL: This next bit is borrowed from APSIM
!           avoid taking it all up as it can
!           cause rounding errors to take
!           NO3 below zero.

!         2023-07-28 HBD, FO - Needs re-check what is the impact 
!         of this floating point protection below
          IF(POT_N_SUPPLY .GT. 0.0) THEN
		     DEM_SUP_RATIO = TOT_N_DEMAND/POT_N_SUPPLY
          ELSE
               DEM_SUP_RATIO = 0.0
          ENDIF
		
		IF(TOT_N_DEMAND.GT.POT_N_SUPPLY) THEN
			UPTK_RATIO = 0.99999                   
		ELSE
			UPTK_RATIO = MAX(DEM_SUP_RATIO, 0.)
		ENDIF
         
        ACC_UPTAKE = 0
         
        DO I=1, SoilProp%NLAYR               
		UNO3(I) = RNO3U(I) * UPTK_RATIO
		UNH4(I) = RNH4U(I) * UPTK_RATIO
		ACC_UPTAKE = ACC_UPTAKE + UNO3(I) + UNH4(I)
        ENDDO
	

c         Today's senesced dry matter becomes yesterday's:
c         (value is stored for tomorrow by SAVE directive)
       YEST_DMDEAD = DEADLF_MASS


c     INTEGRATION
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

		TOT_N_ALLOC = 0.
		N_ALLOC = 0.

C		MvdL: I think this is a good place to allocate to the roots first!!!!
		
			IF (POT_N_SUPPLY .LT. N_DEMAND_WEIGHTS(ROOTS)) THEN
				N_ALLOC(ROOTS) = POT_N_SUPPLY
				POT_N_SUPPLY = 0.
			ELSE 
			 DO I=1, NUM_COMPS    
				N_ALLOC(I) = POT_N_SUPPLY * UPTK_RATIO 
     &				* N_DEMAND_WEIGHTS(I)
				TOT_N_ALLOC = TOT_N_ALLOC + N_ALLOC(I)
			 ENDDO
			ENDIF
		
              
c         Use the allocations to update the quantities of N:
          DO I=1, NUM_COMPS
c             Recalc N-pools
              N_POOL(I) = N_POOL(I) + N_ALLOC(I)
          ENDDO

		TOT_N_POOL = SUM(N_POOL)
		ABVGRND_N_MASS = N_POOL(TOPS) + N_POOL(STALKS) + N_POOL(DEADLF)
		TOP_N = N_POOL(TOPS)   !MvdL: this is to be used in the OPHARV module
		

      
      ENDIF !MASSES(TOPS).GT.0
       
c      *********EFFECT OF N STRESS ON CROP GROWTH/DEVELOPMENT***********
c	    GAS: Add in a condition so that this will only be used if Nitrogen is being simulated
		
c		CWSI = CWSI * N_STRESS      !Already in SC-CNGRO,where CWSI is a photosynthesis reduction factor
       
       !CropSyst's factor to account for N stress on LAI
       !'Determine nitrogen stress effect on specific leaf area
       !'Using overall top nitrogen stress factor for this calculation


		IF (N_STRESS.GE. 0.3) THEN
			Nitrogen_Canopy_Expansion_Factor = 0.
		ELSE
			Nitrogen_Canopy_Expansion_Factor = 1. - (1. - N_STRESS)
     &			/ 0.7
		ENDIF
       
c		Determine combined stress effect on specific leaf area
c		Take the minimum of temperature, water, and nitrogen limiting factors


c		MvdL: this stress factor should then probably be built into the following algortihm from SC_Canop3.for, but will need to discuss first
!		GLFMX1=max(BEST-(SENESF*STDAYC/100.0),0.0)
!		GLFMX2=99.9

c		MvdL: water stress and N stress combined - shouldn't we use the minimum of the two??

c		NEED TO ADD SENESCED LEAF MATERIAL ON SURFACE TO SoilOrg.FOR}
c		What about N stress on other processes? e.g. Stalk numbers (tillering) increase with higher soil N
!          WRITE(N_OUT, '(
!     &                I8,2H,  
!     &                ,4(F10.6, 1H,)
!     &                ,4(F10.4, 1H,)
!     &                ,4(F10.4, 1H,)
!     &                ,1(F10.4, 1H,)
!     &                ,2(F10.4, 1H,)
!     &                ,4(F10.4, 1H,)
!     &                ,2(F10.6, 1H,)
!     &                ,5(F15.7, 1H,)
!     &                ,1(F15.7, 1H,))
!     &              )') 
!     &                    Control%YRDOY,
!     &                    MASSES,
!     &                    N_POOL, 
!     &                    N_ALLOC,
!     &                    N_STRESS,
!     &                    PSV_UPTAKE, N_SENESCE,
!     &                    N_CONC,
!     &                    TOT_N_DEMAND, POT_N_SUPPLY,
!     &                    TUNO3, ABVGRND_N_MASS, TOT_N_POOL, 
!     &                    ACC_UPTAKE

!HBD Apr 2023:
!- senesced (attached), senesced (dropped) and root senesced mass, N conc and N mass need further development
!- should we recalculate [N] f(N mass, DM) for each component? ([N] are being exported with values early in the crop season)

      CALL SC_OPNIT(CONTROL, ISWITCH,
     &    YRPLT, SoilProp%NLAYR, SENESCE,
     &    MASSES,
     &    N_POOL, ! ok
!     &    N_ALLOC,! ok
     &    N_STRESS, ! ok?
!     &    PSV_UPTAKE, FO has commented it in SC_NITRO
!     &    N_SENESCE, ! ok TODO check if this is = to SENESCE % ResE(n,1) (HBD)
     &    N_CONC, ! ok
!     &    TOT_N_DEMAND, POT_N_SUPPLY, !ok TODO is it necessary to be exported? (HBD)
!     &    TUNO3, ! FO has commented it in SC_NITRO; TODO check necessity here (HBD)
     &    ABVGRND_N_MASS, 
!     &    TOT_N_POOL, !ok,ok
     &    ACC_UPTAKE)

      ELSE IF (DYNAMIC.EQ.OUTPUT) THEN
c     OUTPUT
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL SC_OPNIT(CONTROL, ISWITCH, 
     &    YRPLT, SoilProp%NLAYR, SENESCE,
     &    MASSES,
     &    N_POOL, ! ok
!     &    N_ALLOC,! ok
     &    N_STRESS, ! ok?
!     &    PSV_UPTAKE, FO has commented it in SC_NITRO
!     &    N_SENESCE, ! ok TODO check if this is = to SENESCE % ResE(n,1) (HBD)
     &    N_CONC, ! ok
!     &    TOT_N_DEMAND, POT_N_SUPPLY, !ok TODO is it necessary to be exported? (HBD)
!     &    TUNO3, ! FO has commented it in SC_NITRO; TODO check necessity here (HBD)
     &    ABVGRND_N_MASS, 
!     &    TOT_N_POOL, !ok,ok
     &    ACC_UPTAKE)

c     Otherwise, close files at end of season:
      ELSE IF (DYNAMIC.EQ.SEASEND) THEN
!          CLOSE(N_OUT)
      CALL SC_OPNIT(CONTROL, ISWITCH, 
     &    YRPLT, SoilProp%NLAYR, SENESCE,
     &    MASSES,
     &    N_POOL, ! ok
!     &    N_ALLOC,! ok
     &    N_STRESS, ! ok?
!     &    PSV_UPTAKE, FO has commented it in SC_NITRO
!     &    N_SENESCE, ! ok TODO check if this is = to SENESCE % ResE(n,1) (HBD)
     &    N_CONC, ! ok
!     &    TOT_N_DEMAND, POT_N_SUPPLY, !ok TODO is it necessary to be exported? (HBD)
!     &    TUNO3, ! FO has commented it in SC_NITRO; TODO check necessity here (HBD)
     &    ABVGRND_N_MASS, 
!     &    TOT_N_POOL, !ok,ok
     &    ACC_UPTAKE)
      ENDIF

      END
