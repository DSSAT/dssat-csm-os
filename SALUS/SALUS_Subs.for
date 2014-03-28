!==============================================================================
!     Various subroutines for the simple SALUS crop model
!==============================================================================
!     06/24/2009  Translated from Visual Basic (Kofikuma Dzotsi)
!     05/10/2010  Added new equation for root partitioning coefficient (KD)
!     07/26/2010  Added environment extremes (KD)
!
!==============================================================================
!     Subroutine BiomassIncrement
!     Computes biomass increment resulting from growth
!	Need to add CO2 Effect on biomass increment
!==============================================================================
      SUBROUTINE BiomassIncrement(PLANTPOP, ROWSPACING, SRAD, 	
     &	DROUGHTFAC, HEATFAC, COLDFAC, NFAC, PFAC, RUE, XHLAI,				  
     &	KCAN, BIOMASSINC)  								
!------------------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE 
      SAVE
!------------------------------------------------------------------------------         
      REAL PLANTPOP, ROWSPACING, SRAD
	REAL DROUGHTFAC, HEATFAC, COLDFAC, NFAC, PFAC
	REAL RUE, XHLAI, KCAN, BIOMASSINC	
	REAL, PARAMETER::PARFR = 0.5
!------------------------------------------------------------------------------
!	Row spacing must be in cm
	KCAN = 1.5 - 0.768*(((ROWSPACING*0.01)**2)*PLANTPOP)**0.1
	BIOMASSINC = RUE*PARFR*SRAD*(1.0 - EXP(-KCAN*XHLAI))*
     &			   MIN(DROUGHTFAC,HEATFAC,COLDFAC,NFAC,PFAC)			!in g/m2 
!------------------------------------------------------------------------------  
      RETURN
      END SUBROUTINE BiomassIncrement
!==============================================================================


!==============================================================================
!     Subroutine RadiationUseEfficiency
!     Calculate Radiation Use Efficiency based on the simple two-part 
!	function in ALMANAC.
!==============================================================================
      SUBROUTINE RadiationUseEfficiency(RELTTSN, RUEMAX,        !Input
     &	SNPARRUE, RELTT,DROUGHTFAC,HEATFAC,COLDFAC,NFAC,PFAC,   !Input
     &  STRESRUE,
     &	RELRUE,RUE)								                !Output
!------------------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE 
      SAVE   
!------------------------------------------------------------------------------   
      REAL RELTTSN, RUEMAX, SNPARRUE, RELTT, RELRUE, RUE
      REAL DROUGHTFAC,HEATFAC,COLDFAC,NFAC,PFAC
      REAL SNPARRUES   !New senescence parameter
	REAL STRESRUE
!------------------------------------------------------------------------------ 
 
!     First, compute relative RUE
      IF(RELTT .LT. RELTTSN) THEN
	   RELRUE = 1.0
	   RUE = RUEMAX	   
	ELSE !After senescence starts, RUE declines due to leaf senescence,
	     !See Kiniry et al., 1992, Trans. of the ASAE
		IF(RELTTSN .LT. 1.0) THEN
! KD Added 02/20/2011, similar modification as for LAI senescence
		   SNPARRUES = STRESRUE*SNPARRUE + (SNPARRUE-STRESRUE*SNPARRUE)*
     &                 MIN(DROUGHTFAC,HEATFAC,COLDFAC,NFAC,PFAC)
		   RELRUE = ((1.001 - RELTT)/(1.0 - RELTTSN))**SNPARRUES
		ELSE
		   PRINT*,"RelTT at start of senescence must be less than 1"
		ENDIF
		
		RUE = MIN(RUE, RELRUE*RUEMAX)
		
	ENDIF	      
      
!a*SNPARRUE_|x
!           |     x
! SNPARRUES |          x  
!           |               x                            
!  SNPARRUE_|_ _ _ _ _ _ _ _ _ _x
!           |___________________|
!           0     STRESS        1 
! LAI senesces a (a = STRESRUE) times faster at maximum stress reducing      
! the efficiency of use of radiation by the same factor.
      
!------------------------------------------------------------------------------  
      RETURN
      END SUBROUTINE RadiationUseEfficiency
!==============================================================================


	
!==============================================================================
!     Subroutine DailyThermalTime
!     Calculates daily accumulated thermal time that drives plant phenology
!==============================================================================
      SUBROUTINE DailyThermalTime(TMAX, TMIN, TBASEDEV,	   !Input
     &	TOPTDEV, TTMATURE,					               !Input
     &	TEFF, RELTT, DTT, CUMTT)						   !Output
!-----------------------------------------------------------------------
      IMPLICIT NONE   
      
      REAL TBASEDEV, TOPTDEV, TEFF, DTT, TTMATURE, RELTT
	REAL TMAX, TMIN, CUMTT
!-----------------------------------------------------------------------  
		IF (TMAX .LT. TBASEDEV) THEN 
           DTT = 0 
		ELSE ! If maximum temperature greater than base temperature

			IF (TMIN .LT. TBASEDEV) THEN
			TEFF = (TBASEDEV + MIN(TOPTDEV,TMAX))/2

			  ELSEIF (TMAX .GT. TOPTDEV .AND. TMIN .LT. TOPTDEV) THEN
			  TEFF = (TMIN + TOPTDEV)/2

			  ELSE
			  TEFF = (TMIN + TMAX)/2 
			  ENDIF

		    DTT = TEFF - TBASEDEV
		    CUMTT = CUMTT + DTT 
		    ENDIF

		IF (TTMATURE .LE. 0.0) THEN
		RELTT = RELTT + 0.001
		ELSE
		RELTT = RELTT + DTT/TTMATURE
		ENDIF
!-----------------------------------------------------------------------  
      RETURN
      END SUBROUTINE DailyThermalTime
!==============================================================================



!==============================================================================
!     Subroutine Germination
!     Uses thermal time to determine if germination has occurred
!==============================================================================
      SUBROUTINE Germination(TTACCUMULATOR1, TTGERMINATE,
     &	TTMATURE, DTT, TTEMERGE,
     &	KILLED,	GERMINATE)		
!-----------------------------------------------------------------------
      IMPLICIT NONE   
      
      REAL TTGERMINATE, TTMATURE, DTT
	REAL TTACCUMULATOR1, TTEMERGE
	LOGICAL KILLED, GERMINATE
!-----------------------------------------------------------------------  
		IF (GERMINATE) THEN
		RETURN
		ENDIF
		
		TTACCUMULATOR1 = TTACCUMULATOR1 + DTT

		IF (TTACCUMULATOR1 .GE. TTGERMINATE) THEN 
            GERMINATE = .TRUE. 
		TTACCUMULATOR1 = 0.0 !Set TTACCUMULATOR back to zero after germination

			IF ((TTGERMINATE + TTEMERGE) .GT. TTMATURE) THEN
			KILLED = .TRUE.
			ENDIF
		ENDIF      
!-----------------------------------------------------------------------  
      RETURN
      END SUBROUTINE Germination
!==============================================================================



!==============================================================================
!     Subroutine Emergence
!     Uses thermal time to determine emergence status
!==============================================================================
      SUBROUTINE Emergence(TTACCUMULATOR, DTT, TTEMERGE, TTMATURE,
     &					   EMERGE)	     
 
!-----------------------------------------------------------------------
       IMPLICIT NONE   
      
       REAL DTT
       REAL TTEMERGE, TTACCUMULATOR, TTMATURE
	 LOGICAL EMERGE
!-----------------------------------------------------------------------  
		IF (EMERGE) THEN
		RETURN
		ENDIF
		
		TTACCUMULATOR = TTACCUMULATOR + DTT
	
		IF (TTACCUMULATOR .GE. TTEMERGE) THEN 
            EMERGE = .TRUE. 
		TTACCUMULATOR = 0.0 !Set TTACCUMULATOR back to zero after emergence
		ENDIF 
!-----------------------------------------------------------------------  
      RETURN
      END SUBROUTINE Emergence
!==============================================================================



!==============================================================================
!     Subroutine Maturity
!     Uses thermal time to determine maturity status
!==============================================================================
      SUBROUTINE Maturity(RELTT, MATURE)	  			
!-----------------------------------------------------------------------
      IMPLICIT NONE   
      
      REAL RELTT
	INTEGER MATURE
!-----------------------------------------------------------------------  
		IF (RELTT .GE. 1.0) THEN 
	      MATURE = 1 
		ENDIF     
!-----------------------------------------------------------------------  
      RETURN
      END SUBROUTINE Maturity
!==============================================================================



!==============================================================================
! Subroutine LeafAreaIndex
! Computes canopy leaf area index
! Original Visual Basic Method (before senescence)
!		   deltaLAI = RELLAI*LAIMAX - XHLAI
!		   RELLAIS = (XHLAI + deltaLAI * 
!     &			      MIN(DROUGHTFAC,HEATFAC,COLDFAC,NFAC,PFAC)) / LAIMAX
!		   RELLAIMAX = RELLAIS
!          XHLAI = RELLAIS * LAIMAX
! Kofikuma Dzotsi 12/15/2010- cleaner implementation of original version
!          deltaLAI = (RELLAI*LAIMAX - XHLAI)*
!     &                 MIN(DROUGHTFAC,HEATFAC,COLDFAC,NFAC,PFAC)
! Original method after senescence
!                            This was the LAI just before senescence  
!                                       ______|_______   		
!                                      |              |                     
!	       XHLAI = MIN(XHLAI, RELLAI*LAIMAX*RELLAIMAX) 
!==============================================================================
      SUBROUTINE LeafAreaIndex(LAIMAX, LAIP1, LAIP2,          !Input
     &	RELTTSN, SNPARLAI, DROUGHTFAC, HEATFAC, COLDFAC,	  !Input		
     &  NFAC, PFAC,	RELTT, XHLAI, RELTTSN2,STRESLAI,          !Input
     &	RELLAI, deltaLAI, RELLAIS, RELLAIMAX,rellaiyest)	  !Output		
!-----------------------------------------------------------------------
      IMPLICIT NONE         
      REAL LAIMAX, LAIP1, LAIP2, RELTTSN, SNPARLAI
	REAL DROUGHTFAC, HEATFAC, COLDFAC, NFAC, PFAC
	REAL RELTT, RELLAI, XHLAI, dLAI
	REAL RELLAIS, RELLAIMAX, deltaLAI	
	REAL rellaiyest	
	! 02/21/2011
	REAL SNPARLAIS   !New senescence parameter
	REAL STRESLAI    !Factor by which sen. is increased at max water stress
	REAL RELTTSN2    !Relative thermal time beyond which 
	                 !plant is no longer sensitive to water stress
!-----------------------------------------------------------------------  
       IF (RELTT .LE. RELTTSN) THEN
		   IF ((RELTT + EXP(LAIP1 - LAIP2*RELTT)) .GT. 0.0) THEN
		   RELLAI = RELTT/(RELTT + EXP(LAIP1 - LAIP2*RELTT))

! version2-Kofikuma 12/16/2010- Modification to original visual basic version:
! Potential rate of LAI increase is given by (RELLAI-rellaiyest)*LAIMAX
! Simply apply the stress factors to this rate. Avoid computing the rate
! based on yesterday's LAI itself because always increasing rate after a stress.
		   deltaLAI = (RELLAI-rellaiyest)*LAIMAX *  
     &                 MIN(DROUGHTFAC,HEATFAC,COLDFAC,NFAC,PFAC)
 		   ENDIF

         XHLAI = XHLAI + deltaLAI
         RELLAIMAX = XHLAI/LAIMAX
                     
	 ELSE !LAI declines	 
	 
		 IF((RELTTSN .LT. 1.0) .AND. (RELTTSN2 .LT. 1.0)) THEN
! version3-02/18/2011 Modification after discussion in Rm 201 with Bruno Basso and Joe Ritchie.
! Have two SNPARLAI parameters: the one that determine the shape before RelTTSn2 = 0.70 and 
! is influential by water stress; and another one that takes on the value from the crop
! parameter file is not inflenced by water stress. In other words the effect of water stress
! on LAI, occurring beyond RelTTSn2 is not modeled.		    
		   IF((RELTT .LE. RELTTSN2) .AND. (RELTTSN2 .GE. RELTTSN)) THEN	      
          SNPARLAIS = STRESLAI*SNPARLAI + (SNPARLAI-STRESLAI*SNPARLAI)*
     &                 MIN(DROUGHTFAC,HEATFAC,COLDFAC,NFAC,PFAC)
		   ELSE
		      SNPARLAIS = SNPARLAI
		   ENDIF
		    
		   RELLAI = ((1.001 - RELTT)/(1.0 - RELTTSN))**SNPARLAIS		    
    		 ELSE
		    PRINT*,"Rel.TT at start of senescence must be less than 1"
		 ENDIF
		  
! version2-Kofikuma 12/16/2010 Modification to original version to account for stress
! occuring during the phase of LAI decline
!        Smooth out curve at transition between LAI increase and LAI decline
		 rellaiyest = MAX(rellaiyest, RELLAI)
         deltaLAI = (RELLAI-rellaiyest)*LAIMAX*RELLAIMAX

!        02/17/2011        
!        Prevent LAI from being negative during the decline phase
         IF(XHLAI+deltaLAI .LT. 0.0) THEN
           XHLAI = MAX(XHLAI+deltaLAI, XHLAI)
         ELSE
           XHLAI = XHLAI + deltaLAI
         ENDIF
           
	 ENDIF
!-----------------------------------------------------------------------  
      RETURN
      END SUBROUTINE LeafAreaIndex
!==============================================================================
		


!==============================================================================
!     Subroutine Partitioning
!     Partitions assimilates among organs, only tops and roots here
!     This was the old function that was partitioning too much to the roots:
!     ROOTPARTCOEFF = 1.5*(RELTT**2) - 1.9*RELTT + 0.8
!     The new one (below, email with Bruno Basso, 05/10/2010) behaves better
!     [Kofikuma Dzotsi, 05/10/2010]
!==============================================================================
      SUBROUTINE Partitioning(BIOMASSINC, RELTT,	        !Input			
     &	ROOTPARTCOEFF, dBIOMASS, dBIOMASSROOT)				!Output
!-----------------------------------------------------------------------
      IMPLICIT NONE   
      
      REAL BIOMASSINC, RELTT, ROOTPARTCOEFF
      REAL dBIOMASS, dBIOMASSROOT
      REAL,PARAMETER::FRBforRT = 0.90      
!-----------------------------------------------------------------------
!     Swinnen et al., 1994
      ROOTPARTCOEFF = 0.45 * EXP(-1.904*RELTT)
       
!     The rest of new root C goes to soluble FOM:              
	dBIOMASSROOT = BIOMASSINC * ROOTPARTCOEFF * FRBforRT
	dBIOMASS = BIOMASSINC * (1 - ROOTPARTCOEFF)
!-----------------------------------------------------------------------  
      RETURN
      END SUBROUTINE Partitioning
!==============================================================================


*************************************************************************
*************************************************************************
*     SUBROUTINE ENVIRONMENT
*************************************************************************
*	Purpose: Evaluate the effect of extreme environmental conditions on
*	crop growth. Stops crop model if any environmental extremes such as 
*	frost, long-term cold, drought, or nutrient stress, etc. occurs.
*************************************************************************
*	07/27/2009 Kofikuma Dzotsi. Translated from Visual Basic as part of
*				the Salus crop model.
**************************************************************************
*     LIST OF VARIABLES
*	CumSlowDev	 = Cumulative slow development days (days) 
*	CumSlowDevStop = Cumulative slow development days to stop crop model (days)
*	DTT			 = Daily thermal time increment (degree-days)
*	KILLED		 = True when crop is killed by adverse conditions
*	RELTT			 = Relative thermal time during life of plant (0-1)
*	RELTTSN		 = Relative thermal time at beginning of senescence (0-1)
*	TMIN			 = Daily minimum temperature (degree C)
***********************************************************************
      SUBROUTINE ENVIRONMENT(
     &  RELTT,RELTTSN,TMIN,CumSlowDev,DTT,CumSlowDevStop,TFREEZE,		
     &	KILLED, FREEZED)												

!-----------------------------------------------------------------------  
      IMPLICIT NONE 

      REAL RELTT,RELTTSN,TMIN,DTT,TFREEZE
      INTEGER CumSlowDev, CumSlowDevStop
      LOGICAL KILLED, FREEZED
!-----------------------------------------------------------------------
	CumSlowDevStop = 20
!-----------------------------------------------------------------------      

!	Stop simple crop model if frost after start of senescence (or max LAI):
!!	IF (RELTT .GT. RELTTSN .AND. TMIN .LE. -10.0) THEN
!     8/25/2011 The above statement was the old condition. However, this condition was rarely met, esp.
!     in cooler climate, which would let the crop grow for several months (more than a year!) and with
!     continued artificial biomass accumulation. Therefore, a new parameter TFREEZE was introduced. Anytime
!     during development when TMIN drops down to TFREEZE level, development stops, even before RELTTSN- KD
!     Added new variable FREEZED to differentiate between freezing and slowed development
		IF (TMIN .LE. TFREEZE) THEN 
		FREEZED = .TRUE.
		RETURN
		ENDIF

!	Check accumulation of an arbitrary number of slow development days (dTT < 0.1):
		IF (DTT .LE. 0.1) THEN
			CumSlowDev = CumSlowDev + 1
			IF (CumSlowDev .GE. CumSlowDevStop) THEN
			KILLED = .TRUE.
			RETURN
			ENDIF
		ELSE
			CumSlowDev = 0
		ENDIF

      RETURN
      END SUBROUTINE ENVIRONMENT
************************************************************************
************************************************************************
	
	
!==============================================================================
!     Subroutine CONVERTB
!     Convert above and below ground biomass from g m-2 to kg ha-1
!==============================================================================
      SUBROUTINE CONVERTB(BIOMASS, BIOMASSROOT, GrainYield,
     &           BIOMASSC, BIOMASSROOTC, GrainYieldC)    		
!-----------------------------------------------------------------------
      IMPLICIT NONE   
      REAL BIOMASS, BIOMASSROOT, GrainYield
      REAL BIOMASSC, BIOMASSROOTC, GrainYieldC	
!-----------------------------------------------------------------------  
      BIOMASSC = BIOMASS * 10.0
	BIOMASSROOTC = BIOMASSROOT * 10.0
	GrainYieldC = GrainYield * 10.0 
!-----------------------------------------------------------------------  
      RETURN
      END SUBROUTINE CONVERTB
!==============================================================================



!==============================================================================
!     Subroutine EstimateLaiPar
!     Compute the potential LAI S-curve parameters
!     In the original SALUS, this function is called Sigmoidal_Curve_Par
!==============================================================================
      SUBROUTINE EstimateLaiPar(RELTTP1, RELTTP2,    !Input
     &           RELLAIP1, RELLAIP2,			 !Input
     &	         LAIP1, LAIP2)				 !Output    		
!-----------------------------------------------------------------------
      IMPLICIT NONE   
      REAL RELTTP1, RELTTP2, RELLAIP1, RELLAIP2
	REAL LAIP1, LAIP2, XTEMP	
!-----------------------------------------------------------------------  
!     Set RELTTP1 and RELTTP2 respectively to 0.15 and 0.50 so that
!     LAIP1 and LAIP2 needed by the LAI subroutine can be calculated
!     when RELLAIatP1 and RELLAIatP2 are read from the crop file.
!     Kofikuma 07/23/2010
!      RELTTP1 = 0.15
!      RELTTP2 = 0.50
!-----------------------------------------------------------------------  
		IF (RELLAIP1 .NE. 0.0) THEN
		XTEMP = LOG(RELTTP1 / RELLAIP1 - RELTTP1)
		ENDIF

		IF ((RELLAIP2 .NE. 0.0) .AND. (RELTTP2 - RELTTP1 .NE. 0.0)) 
     &	THEN
		LAIP2 = (XTEMP - LOG(RELTTP2 / RELLAIP2 - RELTTP2)) / 
     &		       (RELTTP2 - RELTTP1)
		ENDIF

		LAIP1 = XTEMP + RELTTP1 * LAIP2
!-----------------------------------------------------------------------  
      RETURN
      END SUBROUTINE EstimateLaiPar
!==============================================================================



!==============================================================================
!  Function SAL_PValue, Linearly interpolates daily optimum and minimum  
!  P values based on growth stage fractions.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  05/20/2005 CHP Written.
!  07/01/2009 KAD Slightly adapted for Salus crop model
!-----------------------------------------------------------------------
!  Called by: 
!==============================================================================
      Function SAL_PValue(RELTT, RelTTEmerge, PArray)

!     ------------------------------------------------------------------
      Real SAL_PValue                   !Interpolated value returned
	Real RelTTEmerge
      Real RELTT					   
      Real PArray(3)				   !Array of values to be interpolated
!
!
!                |
!      PArray(1)>+   x
!      PArray(2)>+            x
!                | 
!                |
!      PArray(3)>+                     x
!                |___________________________
!                    |        |        |
!                Emergence RelTT=0.5  Maturity            
!
!     ------------------------------------------------------------------
!     Calculate optimum and minimum P concentrations in plant tissue.
!     ------------------------------------------------------------------
      IF (RELTT == RelTTEmerge) THEN
        !Prior to emergence
        SAL_PValue = PArray(1)

      ELSEIF (RELTT .LT. 0.5) THEN
        !First to second critical stage
        SAL_PValue = PArray(1) - (PArray(1) - PArray(2)) 
     &      * (RELTT - RelTTEmerge) * 2.0

      ELSEIF (RELTT .LT. 1.0) THEN
        !Second to third critical stage
        SAL_PValue = PArray(2) - (PArray(2) - PArray(3)) 
     &                * (RELTT - 0.5) *2.

      ELSE
        !Subsequent to third critical stage to harvest
        SAL_PValue = PArray(3)
      ENDIF

      SAL_PValue = MAX(0.0, SAL_PValue)

      RETURN
      End Function SAL_PValue
!==============================================================================


!------------------------------------------------------------------------------
! Variable definitions  
!------------------------------------------------------------------------------
!     BIOMASS			Total plant dry matter weight (g m-2)
!     BIOMASSC		    Total plant dry matter weight converted to kg ha-1 (kg ha-1)
!	BIOMASSINC		Daily biomass increment (g m-2)
!     BIOMASSROOT		Root dry matter weight (g m-2)
!     BIOMASSROOTC	    Root dry matter weight converted to kg ha-1 (kg ha-1)
!     COLDFAC		    Low temperature reduction factor (0-1)
!	CONTROL           Composite variable containing variables related to control 
!                       and/or timing of simulation.  The structure of the variable 
!                       (ControlType) is defined in ModuleDefs.for. 
!	CumSlowDev		Cumulative slow development days 
!	CumSlowDevStop	Cumulative slow development days to stop crop model	
!	DAE				Days after emergence
!	DAP				Days after planting
!     dBIOMASS		    Incremental total plant dry matter weight (g m-2 d-1)
!     dBIOMASSROOT	    Incremental root dry matter weight (g m-2 d-1)
!	dLAI			    Daily increase in leaf area index (m2 m-2 d-1)
!	DOY				Julian day
!     DOYP		        Date of planting (Julian day)
!     DROUGHTFAC	    Drought reduction factor (0-1)
!	DTT				Daily thermal time increment (degree-days)
!	DYN				Dynamic control variable
!	EMERGE			Emergence flag; True when crop has emerged
!	EMGINT		    Intercept of emergence thermal time calculation
!	EMGSLP		    Slope of emergence thermal time calculation
!	endsim			Code signifying physiological maturity (end of simulation)
!     FRBforRT	        Fraction of new root biomass for root growth
!     FREEZED           Logical- True if TMIN <= TFREEZE     
!     HEATFAC		    High temperature reduction factor (0-1)
!	ISWITCH           Composite variable containing switches which control flow of 
!                       execution for model.  The structure of the variable 
!                       (SwitchType) is defined in ModuleDefs.for. 
!	KCAN              Canopy light extinction coefficient for daily PAR, for 
!                       equidistant plant spacing
!	GERMINATE		    Germination flag; True when crop has germinated
!	KILLED			True when crop is killed by adverse conditions
!	LAI				Canopy leaf area index (m2 m-2)
!     LAIMAX		    Maximum expected Leaf Area Index (m2 m-2)
!     PLANTPOP	        Plant population (m-2)
!	MATURE			Maturity flag (0/1); 1 when crop has matured
!     MDATE             Harvest maturity date (YYYYDDD)
!     NFAC		        Nitrogen deficiency factor (0-1)
!     PARFR		        Fraction of solar radiation useable by plants (PAR)
!     PFAC		        Phosphorus deficiency factor (0-1)
!	RELLAI			Relative LAI (0-1)
!	RELLAIP1	        Relative LAI at point 1 on the potential LAI S-curve (0-1)
!	RELLAIP2	        Relative LAI at point 2 on the potential LAI S-curve (0-1)
!     RELRUE			Relative RUE (0-1)
!     RELTT			    Relative thermal time during life of plant (0-1)
!	RELTTP1           Relative thermal time at point 1 on the potential LAI S-curve (0-1)
!	RELTTP2           Relative thermal time at point 2 on the potential LAI S-curve (0-1)
!	RELTTSN		    Relative thermal time at beginning of senescence (0-1)
!	ROOTPARTCOEFF	    Fraction of assimilate that goes to roots (0-1)
!     ROWSPACING	    Row spacing (cm)
!	RUE				Radiation Use Efficiency (g MJ-1)
!     RUEMAX		    Maximum expected Radiation Use Efficiency (g MJ-1)
!	SNPARLAI	        Parameter for shape of potential LAI curve after beginning of senescence (0-1)
!	SNPARRUE	        Parameter for shape of potential RUE curve after beginning of senescence (0-1)
!     SOWDEPTH	        Sowing depth (cm)
!     SRAD		        Daily solar radiation (MJ m-2)
!	TBASEDEV	        Base temperature for development (degree C)
!	TEFF			    Daily effective temperature (degree C)
!     TFREEZE           Freezing temperature in the sense that if TMIN <= TFREEZE, growth stops (deg C)
!	TMAX		        Daily maximum temperature (degree C)
!	TMIN		        Daily minimum temperature (degree C)
!	TOPTDEV		    Optimum temperature for development (degree C)
!	TTGERMINATE	    Thermal time planting to germination (degree-days)
!	TTEMERGE		    Thermal time germination to emergence (degree-days)	
!	TTMATURE	        Thermal time planting to maturity (degree-days)
!     XHLAI             Healthy leaf area index (m2[leaf] / m2[ground])
