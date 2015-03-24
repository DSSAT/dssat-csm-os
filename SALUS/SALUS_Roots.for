!==============================================================================
!	Subroutine to compute root growth
!	+-----------------------------------------------------------------------
!	| "New" root growth to replace the one from DSSAT 3.5.
!	| Called by both the simple and the complex crop, therefore should exist
!	| in its own source file.  Based on the stand-alone program ROOTALON.
!	+-----------------------------------------------------------------------
!==============================================================================

      SUBROUTINE SALUS_ROOTS(DYNAMIC, SOILPROP, dBiomassRoot,	   !Input
     &  dTT, LAI, LAIMAX, RLWR, ST, SW,PLANTPOP,SOWDEPTH,      !Input
     &  RootDepth, RootLayerFrac, RootMassLayer, RLV)          !Output		

!------------------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE 
      SAVE
!------------------------------------------------------------------------------  

      REAL aLayer(NL)			
      REAL BD(NL)				
      REAL dBiomassRoot		
      REAL DepthMax			
      REAL DLAYR(NL)
      REAL DS(NL)			
      REAL dTT
      INTEGER DYNAMIC				
      REAL LAI				
      REAL LAIMAX			
      REAL LL(NL)
!      REAL midLayer(NL)
      INTEGER NLAYR
      REAL PLANTPOP				
      REAL RLV(NL)					
      REAL RLWR				
      REAL RootDepth
      INTEGER RootFrontLayer				
      REAL RootLayerFrac(NL)		
      REAL RootMassLayer(NL)		
      REAL RootPresence(NL)
      INTEGER RootsGrowDown				
      REAL SHF(NL)
      REAL SOWDEPTH				
      REAL ST(NL)				
      REAL SW(NL)
    
!	Local variables:
      REAL ColdFac
      REAL dRootLayerFrac(NL)
      REAL DryFac
      INTEGER layer
      REAL Porosity
      REAL RootDepthFac(NL)		
      REAL RootDepthInc 
      REAL RootWtFac(NL) 
      REAL SatFac	
      REAL SumFactors
      REAL SumWeight
      REAL WatFillPor 
      
!     ------------------------------------------------------------------
!     Constants
      REAL, PARAMETER :: RockDensity = 2.65
      REAL, PARAMETER :: RtDepthIncPerDD = 0.10
      REAL, PARAMETER :: SHFScaleFac = 0.10 
!     ------------------------------------------------------------------
!     Constructed variable types based on definitions in ModuleDefs.for
      TYPE (SoilType) SOILPROP
      
!     Transfer values from constructed data types into local variables
      BD    = SOILPROP % BD
      DLAYR = SOILPROP % DLAYR
      DS    = SOILPROP % DS
      LL    = SOILPROP % LL
      NLAYR = SOILPROP % NLAYR
      SHF   = SOILPROP % WR

!------------------------------------------------------------------------------
!     INITIALIZATION AND INPUT DATA
!------------------------------------------------------------------------------

!     IF(DYNAMIC.EQ.RUNINIT .OR. DYNAMIC.EQ.SEASINIT) THEN
      IF(DYNAMIC.EQ.SEASINIT) THEN
!------------------------------------------------------------------------------

!     -------------------------------------------------------------------------
!     Initialize Variables
!     -------------------------------------------------------------------------
      DepthMax = DS(NLAYR)
      aLayer(1) = 0.0
      DO layer = 2, NLAYR
         aLayer(layer) = DS(layer-1)
      ENDDO
      
      RootLayerFrac  = 0.0     
      RootFrontLayer = 1 
      RootDepth = SOWDEPTH
      
!     Following email with Bruno (07/27/2010)-
!     Use exponential function from SALUS to compute RootDepthFac
!     First compute depth to middle of layer      
!      DO layer = 1, NLAYR
!         midLayer(layer) = DS(layer) - (DLAYR(layer)/2)
!      ENDDO
      
!     From VB codes
!     Root_Depth_Fac(i) = 1 when ZLayr(i) <= 15
!     Root_Depth_Fac(i) = exp(-(Z(i)-15.0)*0.03) when ZLayr(i) > 15
      DO layer = 1, NLAYR
        IF (DS(layer) .LE. 15.0) THEN
            RootDepthFac(layer) = 1.0
            ELSE
            RootDepthFac(layer) = EXP(-(DS(layer)-15.0)*0.03)
        ENDIF
      ENDDO
            
!     Kofikuma to check Salus vs DSSAT soil layer thicknesses
!     use exponential function instead
!     Check with Bruno to see if we need these factors or if they are
!     not the same as SHF
!      RootDepthFac     = 0.01 !all layers, to NL
!      RootDepthFac(1)  = 1.0
!      RootDepthFac(2)  = 1.0
!      RootDepthFac(3)  = 1.0
!      RootDepthFac(4)  = 0.85
!      RootDepthFac(5)  = 0.58
!      RootDepthFac(6)  = 0.37
!      RootDepthFac(7)  = 0.21
!      RootDepthFac(8)  = 0.11
!      RootDepthFac(9)  = 0.05
!      RootDepthFac(10) = 0.03
!      RootDepthFac(11) = 0.01
!      RootDepthFac(12) = 0.01

!	DO layer = 1, NLAYR
!	   RLV(layer) = (0.01*PLANTPOP)*RLWR*1.E-4/DLAYR(layer)
!        (cm/cm3)   = (g/plt)*(plts/m2)*(cm/g)*(10^-4)/cm
!      ENDDO					

!------------------------------------------------------------------------------
!     RATE CALCULATIONS
!------------------------------------------------------------------------------
      ELSEIF(DYNAMIC.EQ.RATE) THEN
!------------------------------------------------------------------------------
!	First step:  grow roots downwards.  The new depth that the roots reached
!	today is a function of the thermal time that drives growth, and potentially
!	impeding factors: Soil Hospitality Factor, saturated soil, dry soil,
!	and low temperature.
!
!     Check conditions for root growth
      IF (LAI .LE. LAIMAX) THEN
          RootsGrowDown = 1        !Roughly until flowering
          ELSE
          RootsGrowDown = 0
      ENDIF

      IF (RootsGrowDown .EQ. 1) THEN

!	Calculate impeding factors at the deepest root layer:
!     Saturation stress as soon as water-filled porosity exceeds 80%:
      Porosity = 1.0 - BD(RootFrontLayer) / RockDensity
      WatFillPor = SW(RootFrontLayer) / Porosity
      SatFac = MIN(1.0, 5.0 * (1.0 - WatFillPor))

!	Dry soil stress as soon as soil dries to lower limit plus 0.04 cm3 cm-3:
      DryFac = MAX(0.0, MIN(1.0, (SW(RootFrontLayer) - 
     &                            LL(RootFrontLayer)) * 1.0 / 0.04))

!	Cold stress as soon as soil temperature drops to 4 C:
      ColdFac = MAX(0.0, MIN(1.0, ST(RootFrontLayer) * 1.0 / 4.0))

!	Root depth increment for the day (should be in cm):
      RootDepthInc = RtDepthIncPerDD * dTT * MIN(SHF(RootFrontLayer), 
     &	                                     SatFac, DryFac, ColdFac)

!	Add to existing root depth but do not exceed soil depth:
      RootDepth = MIN(RootDepth + RootDepthInc, DepthMax)

!	Check if the roots have grown into the next layer:
        DO WHILE (RootDepth .GT. DS(RootFrontLayer))
          RootFrontLayer = RootFrontLayer + 1
        END DO

      ENDIF !Roots grow down
      
!	Second step:  Calculate the root dry matter for each layer
      SumFactors = 0.0

      DO layer = 1, MIN(RootFrontLayer, NL) !Fraction of rtwt/layer

!	What is the fraction of root presence in each layer?
        IF (layer .LT. RootFrontLayer) THEN
          RootPresence(layer) = 1.0
        ELSE
          RootPresence(layer) = (RootDepth - aLayer(RootFrontLayer)) / 
     &		                     DLAYR(RootFrontLayer)
        ENDIF

!	Saturation stress as soon as water-filled porosity exceeds 80%:
        Porosity = 1.0 - BD(layer) / RockDensity
        WatFillPor = SW(layer) / Porosity
        SatFac = MIN(1.0, 5.0 * (1.0 - WatFillPor))

!	Dry soil stress as soon as soil dries to lower limit plus 0.04 cm3 cm-3:
        DryFac = MAX(0.0, MIN(1.0, (SW(layer) - LL(layer))* 
     &		                        1.0 / 0.04))

!	Cold stress as soon as soil temperature drops to 4 C:
        ColdFac = MAX(0.0, MIN(1.0, ST(layer) * 1.0 / 4.0))
  
        RootWtFac(layer) = RootDepthFac(layer) * RootPresence(layer) * 
     &                     SHF(layer) * MIN(SatFac, DryFac, ColdFac)

        SumFactors = SumFactors + RootWtFac(layer)
  
      END DO  !Fraction of root weight in each layer

 !     Compute root dry matter in each layer and convert to root length density      
       SumWeight = 0.0
       IF (SumFactors .GT. 0.0) THEN
          DO layer = 1, MIN(RootFrontLayer, NL)
             dRootLayerFrac(layer) = dBiomassRoot*RootWtFac(layer)/ 
     &			                     SumFactors
              RootMassLayer(layer) = RootMassLayer(layer) + 
     &			                    dRootLayerFrac(layer)
 !!           RootMassLayer(layer) = BiomassRoot * RootWtFac(layer)/ 
 !!    &			                   SumFactors
             SumWeight = SumWeight + RootMassLayer(layer)
            
!           RLV(layer) = MIN(RLWR * RootMassLayer(layer) * 
!     &                       1.E-4/DLAYR(layer), 4.0)
           
!	      RLV(layer) = RLWR * RootMassLayer(layer) * 1.E-4/DLAYR(layer)
!             cm[root]    cm[root]    g[root]    m2          1
!            --------- =  -------- *  ------- * ------- * -------
!            cm3[soil]    g [root]    m2[soil]  10^4 cm2  cm[soil]
            
             RLV(layer) = RLV(layer) + RLWR*dRootLayerFrac(layer) * 
     &                                 1.E-4/DLAYR(layer)
          END DO
            
            IF (SumWeight .GT. 0.0) THEN
                DO layer = 1, MIN(RootFrontLayer, NL)
                   RootLayerFrac(layer) = RootMassLayer(layer)/ 
     &				                      SumWeight
                END DO
            END IF
       END IF

!	In case that on the first day of growth there is no root growth, RootLayerFrac
!	must be initialized to something other than zero: - AG

        IF (SUM(RootLayerFrac) .LE. 0.0 .AND. RootDepth .GT. 0.0) THEN
            DO layer = 1, RootFrontLayer
               RootLayerFrac(layer) = DLAYR(layer)*RootPresence(layer)/
     &			                      RootDepth
            END DO
        END IF

!------------------------------------------------------------------------------
      ENDIF
!------------------------------------------------------------------------------
      RETURN
      END SUBROUTINE SALUS_ROOTS
      
      
!=======================================================================
!  InitializeRoots Subroutine
!  Initializes root variables at emergence.
!----------------------------------------------------------------------
!  12/22/2010 From CROPGRO's inroot subroutine in ROOTS.for, 
!             Adapted for SALUS
!=======================================================================
      SUBROUTINE InitializeRoots(
     &  RELTT, PLANTPOP, SOWDEPTH, DLAYR, NLAYR, RLWR, WPSEED,  !Inputs
     &  BiomassRoot, Biomass, RootDepth, RLV)                   !Outputs
!=======================================================================
      USE ModuleDefs     
      IMPLICIT NONE

      INTEGER layer, NLAYR
      REAL SeedWt, ROOTPARTCOEFF, RootWtEmg, AboveWtEmg, CumDepth
      REAL CumDepthProfile, RLVInit 
      REAL RLV(NL), DLAYR(NL)
      REAL RELTT, PLANTPOP, SOWDEPTH, RLWR, WPSEED
      REAL BiomassRoot, Biomass, RootDepth
!=======================================================================
! Compute dry matter weight at emergence
       ROOTPARTCOEFF = 0.45 * EXP(-1.904*RELTT)
	 RootWtEmg  = WPSEED * PLANTPOP * ROOTPARTCOEFF
	 AboveWtEmg = WPSEED * PLANTPOP * (1-ROOTPARTCOEFF)
	 
! Initialize Biomass and BiomassRoot at emergence
	 BiomassRoot = RootWtEmg
	 Biomass     = AboveWtEmg
	 RootDepth   = 2*SOWDEPTH !Plant growing both parts- roots and leaves
!-----------------------------------------------------------------------
!     DISTRIBUTE ROOT LENGTH EVENLY IN ALL LAYERS TO A DEPTH OF
!     RTDEPTI (ROOT DEPTH AT EMERGENCE)
!-----------------------------------------------------------------------
      CumDepthProfile = 0.0

      DO layer = 1, NLAYR
         RLV(layer) = 0.0
      ENDDO

      DO layer = 1, NLAYR
           CumDepth = MIN(RootDepth - CumDepthProfile, DLAYR(layer))
           RLVInit  = BiomassRoot * RLWR * (CumDepth/RootDepth) * 1.E-4
!          cm[root]      g[root]   cm[root]    m2
!          -------- = ---------- * -------- * --------
!        cm2[ground]  m2[ground]    g[root]   10^4 cm2

           CumDepthProfile = CumDepthProfile + CumDepth
           RLV(layer) = RLVInit / DLAYR(layer)
           IF (CumDepthProfile .GE. RootDepth) GO TO 300
      ENDDO

  300 CONTINUE
!***********************************************************************
      RETURN
      END SUBROUTINE InitializeRoots
!=======================================================================

      
!------------------------------------------------------------------------------
!     Variable definitions - updated 
!------------------------------------------------------------------------------
!	aLayer: Depth to top of layer
!     dBIOMASSROOT: Incremental root dry matter weight (g m-2 d-1)
!	ColdFac: Cold soil temperature factor
!     CumDepth: Cumulative soil depth (cm)
!     CumDepthProfile: Cumulative depth of soil profile (cm)
!	DepthMax: Maximum depth of soil	
!	DTT: Daily thermal time increment (degree-days)
!	LAI: Canopy leaf area index (m2 m-2)
!	LAIMAX: Maximum expected Leaf Area Index (m2 m-2)
!	layer: Soil layer
!	LL: Lower limit
!     NL: Maximum number of soil layers       
!     NLAYR: Number of soil layers    
!	RLV: Root length density for soil layer "layer" (cm[root]/cm3[soil])
!     RLVInit: Initial root density (cm[root]/cm2[ground])
!	RLWR: Root Length to Weight Ratio (cm[root]/g[root]) 
!	RootDepth: Root depth (cm)
!	RootsGrowDown: Root growth flag; 1 to indicate that roots grow down
!	RootFrontLayer: Root front layer
!	RootLayerFrac: Fraction of live root mass in a given layer
!	RootMassLayer: Root mass per layer (g m-2)
!	RootPresence: Root presence in a layer (0 - 1)
!	RtDepthIncPerDD: Root depth increment per degree day [cm]
!	SHF: Soil hospitality factor
!	SHFScaleFac: Scale-down factor for soil hospitality factor
!	ST: Soil temperature
!	SW: Soil water
!	WatFillPor: Water filled porosity
!
!     DSSAT          SALUS       Definition
!     BD             BD          Bulk density
!     DS             zLayer      Depth to bottom of layer
!     DLAYR          dLayer      Depth of layer