!=======================================================================
!  GrN_Ptl, Subroutine
!
!       Calculates N available for transfer to grain (g/plant) from each
!       plant part.  By definition, available grain N is set to 0.
!
!       Notes
!           N available for translocation to the grain is the sum of N
!              available in the stover and in the roots.
!           N available in stover is the difference of its N content and
!              the minimum it's allowed to fall to.
!           N available in roots is the product of root weight and the
!              fraction of root N available for translocation.
!-----------------------------------------------------------------------
!  Revision history
!  12-21-2011 FSR Created from NWheat subroutine "nwheats_gnptl"
!  01/11/2018 KEP converted WH_ sub-routines to TF_.
!-----------------------------------------------------------------------
!  Called by: TF_GroSub
!=======================================================================
*      SUBROUTINE GrN_Ptl (CONTROL, ISWITCH,
*     &   mnc, nfact, nitmn, npot, optfr, part, pl_la, pl_nit,     !INPUT
*     &   plantwt, sen_la,                                         !INPUT
*     &   navl)                                                   !OUTPUT
*!-----------------------------------------------------------------------
*      USE ModuleDefs
*      USE TF_module
*      IMPLICIT NONE
*      SAVE
*!-----------------------------------------------------------------------
*!                             Define Variables
*!-----------------------------------------------------------------------
*      INTEGER    DYNAMIC
*      INTEGER    part   ! plant part number
*      REAL       mnc(mxpart)  ! Minimum N concentration, by plant part
*      REAL       navl(mxpart) ! N in each plant part available to grain?
*      REAL       nfact(4)
*      REAL       nitmn  ! nitrogen minimum level              (g/part)
*      REAL       npot   ! potential N available for transfer  (g/part)
*      REAL       optfmn ! optimum fraction minimum (0-1)
*      REAL       optfmx ! optimum fraction maximum (0-1)
*      REAL       optfr  ! optimum fraction due to stress      (0-1)
*      REAL       pl_la  ! Plant leaf area mm2
*      REAL       pl_nit(mxpart) !plant part nitrogen (g/plant)
*      REAL       plantwt(mxpart) !plant part weights (g/plant)
*      REAL       sen_la  ! Senesced leaf area
*
*      PARAMETER (optfmn = 0.15) !*! should be a (species?) input
*      PARAMETER (optfmx = 0.35) !*! should be a (species?) input
*
*      REAL       g_gn_optfr ! this is a temp variable; remove ASAP
*
*!     The variable "CONTROL" is of type "ControlType".
*      TYPE (ControlType) CONTROL
*      TYPE (ResidueType) SENESCE
*      TYPE (SwitchType)  ISWITCH
*
*      DYNAMIC = CONTROL % DYNAMIC
*!-----------------------------------------------------------------------
*
*!*! following moved to ModuleDefs.for
*!*!   integer    mxpart !*! maximum number of plant parts
*!*!   parameter (mxpart = 6) !*! counted root, stem, leaf sheath,
*!*!             leaf, grain and seed: could not find any reference
*
**+  Local Variables	 *******OK*******
*! -------------- get grain N potential (supply) ----------
*!                get the fraction of optimum conditions
*
*      optfr  =  optfmn + (optfmx - optfmn)*nfact(2)
*
*cnh added for watch purposes
*      g_gn_optfr = optfr
*
*      ! Now find the available N of each part.
*      DO 1000 part = 1, mxpart
*         IF (pl_la - sen_la .gt. 0.0) THEN
*      ! Calculate N available for transfer to grain
*            nitmn = mnc(part) * plantwt(part)
*            npot = MAX(pl_nit(part) - nitmn, 0.0)
*            navl(part)  =  optfr * npot
*         ELSE  ! No green leaf area. No further N transfer to grain
*      navl(part) = 0.0
*         ENDIF
*
*1000  continue
*!*!      ENDDO
*
*      navl(grain_part) = 0.0
*      navl(seed_part) = 0.0
*
*      RETURN
*      END SUBROUTINE GrN_Ptl
!-----------------------------------------------------------------------
C--------------------------------------------------------------------------------------------------
C                         Define Variables
C--------------------------------------------------------------------------------------------------

! ANDEM       !Crop N demand (kg N/ha)
! ANO3        !Total extractable nitrate N in soil profile (kg N/ha)
! ANH4        !Total extractable ammonium N in soil profile (kg N/ha)
! DLAYR(L)    Soil thickness in layer L (cm)
! DNG         !N demand of potential new growth of tops (g N/plant)
! DROOTN      !Daily change in plant root nitrogen content (g N/plant)
! DSTOVN      !Daily change in plant stover nitrogen content (g N/plant)
! FACTOR      !Ratio of root N uptake to plant N demand
! FNH4        !Unitless soil ammonium supply index
! FNO3        !Unitless soil nitrate supply index
! FON(20)     !Fresh organic nitrogen in soil layer L due to root senescence, kg N/ha
! KG2PPM(20)  !Factor that convergs mg elemental N/kg soil to kg N/ha for soil layer L
! L           !Index counter
! L1          !Lowest soil layer with roots
! LL(20)      !Lower limit of plant extractable water for soil layer L, cm3/cm3
! mnc(mxpart) ! Minimum N concentration, by plant part
! NDEM        !Plant nitrogen demand, g/plant
! NH4(20)     !Ammonium in soil layer L, mg elemental N/kg soil
! NLAYR       !Number of soil layer
! NO3(20)     !Nitrate in soil layer L (mg elemental N/kg soil)
! NUF         !Plant N supply to demand used to modify N uptake
! PDWI        !Potential increment in new shoot growth, g/plant
! PGRORT      !Potential increment in new root growth, g/plant
! pl_la       !Plant leaf area mm2
! PLTPOP      !Plant population, plants/m2
! PTF         !Ratio of above ground biomass to total biomass
! RANC        !Root actual N concentration, g N/g root
! RCNP        !Root critical nitrogen concentration, g N/g root dry weight
! RFAC        !Interim variable describing the effects of root length density on potential N uptake from a layer
! RLV(20)     !Root length density for soil layer L, cm root/cm2 soil
! RNDEM       !Plant root demand for nitrogen (g/plant)
! RNH4U(20)   !Potential ammonia uptake from layer L, kg N/ha
! RNLOSS      !Loss of N from the plant via root exudation in one layer (g N/m2)
! RNO3U(20)   !Potential nitrate uptake from layer L, kg N/ha
! ROOTN       !Root nitrogen content, g N/plant
! RTWT        !Root weight, g/plant
! sen_la      ! Senesced leaf area
! SHF(20)     !Relative root distribution in soil layer L (0-1)
! SMDFR       !Soil moisture deficit factor affecting N uptake
! SNH4(20)    !Ammonium nitrogen in soil layer L, kg N/ha
! SNO3(20)    !Nitrate content in soil layer L, kg N/ha
! STOVN       !Nitrogen content in stover, g N/plant
! STOVWT      !Stover weight (Stem + leaf+lfsheath), g/plant
! SAT(20)     !Saturated water holding capacity for soil layer L, cm3/cm3
! SW(20)      !Soil water content in layer L, cm3 water/cm3 soil
! TANC        !Nitrogen content in above ground biomass, decimal
! TCNP        !Critical nitrogen concentration in tops, g N/g dry weight
! TNDEM       !Plant tops demand for nitrogen (g N/plant)
! TRLV        !Total root length density, cm root/cm2 soil
! TRNLOS      !Total plant N lost by root exudation (g N/m2)
! TRNU        !Total potential root nitrogen uptake, kg N/ha
! UNH4(20)    !Plant uptake of ammonium from layer (kg N/ha/day)
! UNO3(20)    !Plant uptake of nitrate from a layer (kg N/ha/day)
! XMIN        !
! XNDEM       !Temporary variable
! xstag_nw    !Non-integer growth stage indicator (NWheats)
! XSTAGE      !Non-integer growth stage indicator

