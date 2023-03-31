!=======================================================================
!  Aloha_NUPTAK, Subroutine
!
!  Determines N uptake
!-----------------------------------------------------------------------
!  Revision history
!  02/08/1993 PWW Header revision and minor changes   
!  06/--/1993 WTB Modifications
!  06/28/1994 JTR & BDB Changed water content dependent factor 
!  10/17/2017 CHP Adpated for CSM v4.7
!-----------------------------------------------------------------------
!  INPUT  : None
!
!  LOCAL  : NUF,NDEM,L,L1,THUMN,RNH4U,RNO3U,TRNLOS,TRLV,TOTN,DNG,
!           TNDEM,RNDEM,ANDEM,DROOTN,DSTOVN,FNH4,FNO3,SMDFR,RFAC,UNO3,UNH4,
!           XMIN,RNLOSS,XNDEM,FACTOR
!
!  OUTPUT : None
!-----------------------------------------------------------------------
!  Called : GROSUB SGROSUB MGROSUB WGROSUB
!
!  Calls  : None
!-----------------------------------------------------------------------
!=======================================================================

      SUBROUTINE Aloha_NUPTAK(CONTROL, ISWITCH,
     &    ISTAGE, NO3, NH4, PDWI, PGRORT,                 !Input
     &    PLTPOP, PTF, RANC, RCNP, RLV, RTWT, SOILPROP,   !Input
     &    STOVWT, SW, TCNP, XSTAGE,                       !Input
     &    ROOTN, STOVN, TANC, UNH4, UNO3, WTNUP)          !Output
      USE ModuleDefs
      IMPLICIT  NONE
      SAVE

      REAL, DIMENSION(NL) :: ANO3, ANH4, DLAYR, KG2PPM, LL, NH4, NO3, 
     &     RLV, RNO3U, RNH4U, SAT, SHF, SNH4, SNO3, SW, UNH4, UNO3
      INTEGER DYNAMIC

      REAL        ANDEM              
      REAL        DNG         
      REAL        DROOTN      
      REAL        DSTOVN      
      REAL        FACTOR      
      REAL        FNH4        
      REAL        FNO3        
      INTEGER     L, L1           
      INTEGER     ISTAGE          
      REAL        NDEM        
      REAL        NUF
      INTEGER     NLAYR       
      REAL        PDWI        
      REAL        PGRORT 
!      REAL        PLIGRT    
      REAL        PLTPOP      
      REAL        PTF         
      REAL        RANC        
      REAL        RCNP        
      REAL        RFAC
      REAL        RNDEM       
      REAL        RNLOSS      
      REAL        ROOTN       
      REAL        RTWT
      REAL        SMDFR       
      REAL        STOVN       
      REAL        STOVWT      
      REAL        TANC        
      REAL        TCNP        
      REAL        TNDEM       
      REAL        TRLV        
      REAL        TRNLOS      
      REAL        TRNU  
      REAL        WTNUP      
      REAL        XMIN        
      REAL        XNDEM       
      REAL        XSTAGE   

      LOGICAL     FIRST   

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
!      TYPE (ResidueType) SENESCE
      TYPE (SoilType) SOILPROP

      DYNAMIC = CONTROL % DYNAMIC
      DLAYR   = SOILPROP % DLAYR
      KG2PPM  = SOILPROP % KG2PPM
      LL      = SOILPROP % LL
      SAT     = SOILPROP % SAT
      SHF     = SOILPROP % WR

!=======================================================================
      SELECT CASE (DYNAMIC)
!=======================================================================
      CASE (RUNINIT)
!=======================================================================
      TRNLOS = 0.0
      RANC   = 0.0
      TANC   = 0.0
      TRLV   = 0.0
      DROOTN = 0.0
      DSTOVN = 0.0
      TRNU   = 0.0
      NUF    = 0.0
      XNDEM = 0.0
      XMIN = 0.0
      TNDEM = 0.0
      RNLOSS = 0.0
      RNDEM = 0.0
      RFAC = 0.0
      NDEM = 0.0
      FNO3 = 0.0
      FNH4 = 0.0
      FACTOR = 0.0
      DNG = 0.0
      ANDEM = 0.0
      L1 = 0
      UNH4 = 0.0
      UNO3 = 0.0
      ROOTN = 0.0
      STOVN  = 0.0
      WTNUP  = 0.0

      NLAYR = SOILPROP % NLAYR

!=======================================================================
      CASE (SEASINIT)
!=======================================================================
      RANC  = 0.022
      TANC   = 0.044
      WTNUP  = 0.0
      FIRST = .TRUE.

!=======================================================================
      CASE (RATE)
!=======================================================================
      IF (ISWITCH % ISWNIT .NE. 'Y') RETURN

      TRNLOS = 0.0
      RANC   = ROOTN / RTWT
      TANC   = STOVN / STOVWT
      TRLV   = 0.0
      ANO3   = 0.0
      ANH4   = 0.0
      DROOTN = 0.0
      DSTOVN = 0.0
      TRNU   = 0.0
      NUF    = 0.0
      XNDEM = 0.0
      XMIN = 0.0
      TNDEM = 0.0
      RNLOSS = 0.0
      RNDEM = 0.0
      RFAC = 0.0
      NDEM = 0.0
      FNO3 = 0.0
      FNH4 = 0.0
      FACTOR = 0.0
      DNG = 0.0
      ANDEM = 0.0
      L1 = 0
      UNH4 = 0.0
      UNO3 = 0.0

      DO L = 1, NLAYR
         RNO3U(L) = 0.0
         RNH4U(L) = 0.0
!     THIS NEEDS TO BE FIXED LATER:
         TRLV     = TRLV    + RLV(L) !* DLAYR(L)
!        NO3(L)   = SNO3(L) * FAC(L)
!        NH4(L)   = SNH4(L) * FAC(L)
         SNO3(L) = NO3(L) / KG2PPM(L) 
         SNH4(L) = NH4(L) / KG2PPM(L)
      END DO

C-----------------------------------------------------------------------
C     Calculate N demand (DNG=g N/PLT) for new growth (PDWI=g/plt)
C-----------------------------------------------------------------------

      IF (PDWI .EQ. 0.0) THEN
         PDWI = 1.0
      ENDIF
      DNG = PDWI*TCNP
      IF (XSTAGE .LE. 1.2) THEN
         DNG = 0.0
      ENDIF

C-----------------------------------------------------------------------
C     Calculate total N demand (NDEM) of tops (TNDEM) and roots (RNDEM),
C     all expressed in g N/plt Convert total N demand to kg N/ha (ANDEM)
C-----------------------------------------------------------------------

      TNDEM  = STOVWT * (TCNP-TANC) + DNG
      RNDEM  = RTWT   * (RCNP-RANC) + PGRORT*RCNP
      NDEM   = MAX(0.0, TNDEM  + RNDEM)

C-----------------------------------------------------------------------
C     Convert total N demand from g N/plt to kg N/ha (ANDEM)
C-----------------------------------------------------------------------

      ANDEM  = NDEM   * PLTPOP*10.0

C-----------------------------------------------------------------------
C     Calculate potential N supply in soil layers with roots (TRNU)
C-----------------------------------------------------------------------

      DO L = 1, NLAYR
         IF (RLV(L) > 0.0001) THEN
            L1 = L
C
C           The following code was written by JTR to simplify the code for the
C           generic model and to make the functions more like the water uptake
C           functions.  Done on June 28, 1994.
C
C           New water content dependent factor for uptake and new uptake
C           methods that work like the soil water uptake  -- JTR 6/94
C
            SMDFR    = 1.5-6.0*((SW(L)-LL(L))/(SAT(L)-LL(L))-0.5)**2
            SMDFR    = AMAX1 (SMDFR,0.0)
            SMDFR    = AMIN1 (SMDFR,1.0)
            RFAC     = 1.0-EXP(-8.0*RLV(L))
            FNH4     = SHF(L)*0.075
            FNO3     = SHF(L)*0.075
            RNH4U(L) = RFAC*SMDFR*FNH4*(NH4(L)-0.5)*DLAYR(L)
            RNO3U(L) = RFAC*SMDFR*FNO3*NO3(L)*DLAYR(L)
            RNH4U(L) = MAX(RNH4U(L),0.0)
            RNO3U(L) = MAX(RNO3U(L),0.0) 
            TRNU     = TRNU + RNO3U(L) + RNH4U(L)
         ENDIF
      END DO

C-----------------------------------------------------------------------
C     Calculate factor (NUF) to reduce N uptake to level of demand
C-----------------------------------------------------------------------

      IF (ANDEM .LE. 0.0) THEN
         TRNU  = 0.0
         NUF   = 0.0
       ELSE
         ANDEM = AMIN1 (ANDEM,TRNU)
         IF (TRNU .EQ. 0.0) RETURN
         NUF   = ANDEM/TRNU
         TRNU  = 0.0
      ENDIF

C-------------------------------------------------------------------------
C     Calculate N uptake in soil layers with roots based on demand (kg/ha)
C-------------------------------------------------------------------------

      DO L = 1, L1
         UNO3(L) = RNO3U(L)*NUF
         XMIN    = 0.25/KG2PPM(L)
         UNO3(L) = MIN (UNO3(L),SNO3(L) - XMIN)
         UNO3(L) = MAX(UNO3(L),0.0)  
!        SNO3(L) = MAX (SNO3(L) - UNO3(L), 0.0)
!        NO3(L)  = SNO3(L) * FAC(L)

         UNH4(L) = RNH4U(L)*NUF
         XMIN    = 0.5/KG2PPM(L)
         UNH4(L) = MIN (UNH4(L),SNH4(L) - XMIN)
         UNH4(L) = MAX(UNH4(L),0.0) 
!        SNH4(L) = MAX (SNH4(L) - UNH4(L), 0.0)
!        NH4(L)  = SNH4(L) * FAC(L)

         TRNU    = TRNU + UNO3(L) + UNH4(L)   !kg[N]/ha
      END DO

!     Why take it back to per plant basis?
      IF (PLTPOP .GT. 0.0) THEN
        TRNU = TRNU/(PLTPOP*10.0)               !g[N]/plant
      ELSE
        TRNU = 0.0
      ENDIF
C-----------------------------------------------------------------------
C     Update stover and root N
C-----------------------------------------------------------------------

      IF (NDEM .GT. TRNU) THEN
          XNDEM  = TRNU
          FACTOR = XNDEM / NDEM
          NDEM   = XNDEM
          TNDEM  = TNDEM * FACTOR
          RNDEM  = RNDEM * FACTOR
      ENDIF

      IF (NDEM .LE. 0.0 .OR. TRNU .LE. 0.0) THEN
         DSTOVN = 0.0
         DROOTN = 0.0
       ELSE
         !
         ! Calculate root exudation losses (if any)
         !
         DO L = 1, L1
            RNLOSS = 0.0
            IF (TANC .GT. TCNP) THEN
               RNLOSS = RANC*RTWT*0.05*PLTPOP*RLV(L)/TRLV
            ENDIF
            TRNLOS  = TRNLOS + RNLOSS
            !FON(L)  = FON(L) + RNLOSS     !FON is local variable here
         END DO

         ! Adjust DSTOVN and DROOTN to compensate for N lost to FON
         IF(NDEM.GT.0.0.AND.PLTPOP.GT.0.0) THEN
         DSTOVN = TNDEM / NDEM*TRNU-      PTF*TRNLOS/(PLTPOP*10.0)
         DROOTN = RNDEM / NDEM*TRNU-(1.0-PTF)*TRNLOS/(PLTPOP*10.0)
         ENDIF
      ENDIF

!=======================================================================
!     Integration
!-----------------------------------------------------------------------
      CASE (INTEGR)
!=======================================================================
      IF (ISWITCH % ISWNIT .NE. 'Y') RETURN

!     Initialize RLV at stage 1 - from PHASEI
      IF (ISTAGE == 1 .AND. FIRST) THEN
        FIRST = .FALSE.
!       RANC  = 0.022
!        TANC  = 0.044
        ROOTN = RANC*RTWT
        STOVN = STOVWT*TANC

!        IF (RTWT .GT. 0.0)   RANC   = ROOTN / RTWT
!        IF (STOVWT .GT. 0.0) TANC   = STOVN / STOVWT
      ENDIF

      IF (ISTAGE .LT. 7) THEN
        STOVN = STOVN + DSTOVN
        IF(STOVWT.GT.0.001) TANC  = STOVN / STOVWT
        ROOTN = ROOTN + DROOTN
        IF(RTWT.GT.0.001) RANC  = ROOTN / (RTWT-0.01*RTWT)
      ENDIF

!     Moved from OPGROW
      WTNUP = WTNUP + TRNU * PLTPOP
            
!=======================================================================
      END SELECT
!=======================================================================

      RETURN
      END SUBROUTINE Aloha_NUPTAK


C--------------------------------------------------------------------------------------------------
C                         Define Variables
C--------------------------------------------------------------------------------------------------

! ANDEM       !Crop N demand (kg N/ha)
! ANH4        !Total extractable ammonium N in soil profile (kg N/ha)
! ANO3        !Total extractable nitrate N in soil profile (kg N/ha)
! DLAYR(L)    Soil thickness in layer L (cm)
! DNG         !N demand of potential new growth of tops (g N/plant)
! DROOTN      !Daily change in plant root nitrogen content (g N/plant)
! DSTOVN      !Daily change in plant stover nitrogen content (g N/plant)
! FACTOR      !Ratio of root N uptake to plant N demand
C  FACTOR : Relative weighting to distribute crop root residues at the beginning
C           of a simulation
! FNH4        !Unitless soil ammonium supply index
! FNO3        !Unitless soil nitrate supply index
! FON(20)     !Fresh organic nitrogen in soil layer L due to root senescence, kg N/ha
! KG2PPM(20)  !Factor that convergs mg elemental N/kg soil to kg N/ha for soil layer L
! L           !Index counter
! L1          !Lowest soil layer with roots
! LL(20)      !Lower limit of plant extractable water for soil layer L, cm3/cm3
! NDEM        !Plant nitrogen demand, g/plant
! NH4(20)     !Ammonium in soil layer L, mg elemental N/kg soil
! NLAYR       !Number of soil layer
! NO3(20)     !Nitrate in soil layer L (mg elemental N/kg soil)
! NUF         !Plant N supply to demand ratio used to modify N uptake
! PDWI        !Potential increment in new shoot growth, g/plant
! PGRORT      !Potential increment in new root growth, g/plant
! PLTPOP      !Plant population, plants/m2
! PTF         !Ratio of above ground biomass to total biomass
! RANC        !Root actual N concentration, g N/g root
! RCNP        !Root critical nitrogen concentration, g N/g root dry weight
!  RFAC       !Interim variable describing the effects of root length density
!              on potential N uptake from a layer
! RLV(20)     !Root length density for soil layer L, cm root/cm2 soil
! RNDEM       !Plant root demand for nitrogen (g/plant)
! RNH4U(20)   !Potential ammonia uptake from layer L, kg N/ha
! RNLOSS      !Loss of N from the plant via root exudation in one layer (g N/m2)
! RNO3U(20)   !Potential nitrate uptake from layer L, kg N/ha
! ROOTN       !Root nitrogen content, g N/plant
! RTWT        !Root weight, g/plant
! SHF(20)     !Relative root distribution in soil layer L (0-1)
! SMDFR       !Soil moisture deficit factor affecting N uptake
! SNH4(20)    !Ammonium nitrogen in soil layer L, kg N/ha
! SNO3(20)    !Nitrate content in soil layer L, kg N/ha
! STOVN       !Nitrogen content in stover, g N/plant
! STOVWT      !Stover weight (Stem + leaf), g/plant
! SAT(20)     !Saturated water holding capacity for soil layer L, cm3/cm3
! SW(20)      !Soil water content in layer L, cm3 water/cm3 soil
! TANC        !Nitrogen content in above ground biomass, decimal
! TCNP        !Critical nitrogen concentration in tops, g N/g dry weight
! TNDEM       !Plant tops demand for nitrogen (g N/plant)
C  TOTN   : Total mineral N in a layer (kg N/ha)
! TRLV        !Total root length density, cm root/cm2 soil
! TRNLOS      !Total plant N lost by root exudation (g N/m2)
! TRNU        !Total potential root nitrogen uptake, kg N/ha
! UNH4(20)    !Plant uptake of ammonium from layer (kg N/ha/day)
! UNO3(20)    !Plant uptake of nitrate from a layer (kg N/ha/day)
! XMIN        !
! XNDEM       !Temporary variable
! XSTAGE      !Non-integer growth stage indicator
C  XNDEM  :
