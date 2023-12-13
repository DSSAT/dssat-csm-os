C======================================================================
C  MZ_IX_NUPTAK, Subroutine
C
C  Determines N uptake
C----------------------------------------------------------------------
C  Revision history
C
C                 Written
C  02/08/1993 PWW Header revision and minor changes    
C  06/01/1994 WTB Modified 
C  06/28/1994 JR/BDB Changed water content dependent factor 
C  03/29/2001 WBD Converted to modular format      
C  12/01/2001 WDB Major revision for 2002 release   
C  03/12/2003 CHP Changed senescence variable to composite (SENESCE)
C                   as defined in ModuleDefs.for
!  03/02/2005 CHP/JIL Add check for negative RNH4U & RNO3U.
!  11/07/2005 CHP Replaced FAC with SOILPROP variable KG2PPM
!  01/31/2006 CHP/JIL Fixed computation of RNLOSS to match root loss
!                     calculation in MZ_GROSUB (from 5% to .5%)
!  03/30/2006 CHP Added composition of senesced matter for SOM modules
!  10/09/2007 JIL Adapted and modified for IXIM model
C----------------------------------------------------------------------
C  Called : MZ_IX_GROSUB 
C
C  Calls  : None
C----------------------------------------------------------------------
      SUBROUTINE MZ_IX_NUPTAK (DYNAMIC,                         !Control
     &  CARBO,DLAYR,EAROWT,ECNP,KG2PPM,                         !Input
     &  LCNP,LFWT,LL,NH4,NLAYR,NO3,NX,PDWI,PGROEA,PGROLF,       !Input
     &  PGRORT,PGROST,PLIGRT,PLTPOP,PTF,RCNP,RLV,RTWT,SAT,SCNP, !Input
     &  SENESCE,SHF,STMWT,STOVWT,SW,TCNP,XSTAGE,                !Input
     &  EARN,LEAFN,RANC,ROOTN,STEMN,STOVN,TANC,TRNU,UNH4,UNO3)  !Output

      USE ModuleDefs
      IMPLICIT  NONE
      SAVE
C----------------------------------------------------------------------
C                         Define Variables
C----------------------------------------------------------------------

      REAL        ANDEM              
      REAL        CARBO 
      REAL        DLAYR(NL)   
      REAL        DLEAFN
      REAL        DEARN
      REAL        DNG         
      REAL        DROOTN
      REAL        DSTEMN      
      REAL        DSTOVN      
      INTEGER     DYNAMIC
      REAL        EANC
      REAL        EARN
      REAL        EAROWT
      REAL        ECNP
      REAL        ENDEM
      REAL        KG2PPM(NL)     
      REAL        FACTOR      
      REAL        FNH4        
      REAL        FNO3        
      INTEGER     I
      INTEGER     L           
      INTEGER     L1          
      REAL        LANC
      REAL        LCNP
      REAL        LEAFGN
      REAL        LEAFN
      REAL        LEAFNG(50)
      REAL        LEAFNS(50)
      REAL        LEAFSN
      REAL        LEAFWTGY(50)
      REAL        LFAN(50)
      REAL        LFNRT
      REAL        LFWT
      REAL        LL(NL)      
      REAL        LNDEM
      REAL        NDEM        
      REAL        NH4(NL)     
      INTEGER     NLAYR       
      REAL        NO3(NL)     
      REAL        NUF         
      REAL        NRF
      REAL        NX
      REAL        ONDEM
      REAL        PDWI        
      REAL        PGROEA
      REAL        PGROLF
      REAL        PGRORT      
      REAL        PGROST
      REAL        PLTPOP      
      REAL        PTF         
      REAL        RANC        
      REAL        RCNP        
      REAL        RFAC        
      REAL        RLV(NL)     
      REAL        RNDEM       
      REAL        RNH4U(NL)   
      REAL        RNLOSS      
      REAL        RNO3U(NL)   
      REAL        ROOTN       
      REAL        RTWT
      REAL        SANC
      REAL        SHF(NL)     
      REAL        SMDFR       
      REAL        SNAV
      REAL        SNDEM
      REAL        SNH4(NL)    
      REAL        SNO3(NL)    
      REAL        STEMN
      REAL        STOVN       
      REAL        STOVWT      
      REAL        SAT(NL)     
      REAL        SCNP
      REAL        STMWT
      REAL        SW(NL)      
      REAL        TANC        
      REAL        TCNP        
      REAL        TNDEM       
      REAL        TOTNA
      REAL        TRLV        
      REAL        TRNLOS      
      REAL        TRNU        
      REAL        UNH4(NL)    
      REAL        UNO3(NL)    
      REAL        XMIN        
      REAL        XNDEM       
      REAL        XNU
      REAL        XSTAGE      


      TYPE (ResidueType) SENESCE

!----------------------------------------------------------------------
!     CHP 3/30/2006
!     Proportion of lignin in roots
      REAL PLIGRT

C----------------------------------------------------------------------
C     Initialize variables
C----------------------------------------------------------------------
!----------------------------------------------------------------------
!                     DYNAMIC = RUNINIT
!----------------------------------------------------------------------

      IF(DYNAMIC.EQ.RUNINIT.OR.DYNAMIC.EQ.SEASINIT) THEN

      ANDEM    = 0.0
      DLEAFN   = 0.0
      DEARN    = 0.0
      DNG      = 0.0
      DROOTN   = 0.0
      DSTEMN   = 0.0
      DSTOVN   = 0.0
      EANC     = 0.0
      EARN     = 0.0
      ENDEM    = 0.0
      FACTOR   = 0.0
      FNH4     = 0.0
      FNO3     = 0.0
      I        = 0
      L        = 0
      L1       = 0
      LANC     = 0.0
      LEAFGN   = 0.0
      LEAFN    = 0.0
      LEAFSN   = 0.0
      LFNRT    = 0.0
      LNDEM    = 0.0
      NDEM     = 0.0
      NRF      = 0.0
      NUF      = 0.0
      ONDEM    = 0.0
      RFAC     = 0.0
      RNDEM    = 0.0
      RNLOSS   = 0.0
      ROOTN    = 0.0
      SANC     = 0.0
      SMDFR    = 0.0
      SNAV     = 0.0
      SNDEM    = 0.0
      STEMN    = 0.0
      STOVN    = 0.0
      TNDEM    = 0.0
      TOTNA    = 0.0
      TRLV     = 0.0
      TRNLOS   = 0.0
      TRNU     = 0.0
      XMIN     = 0.0
      XNDEM    = 0.0
      XNU      = 0.0
      
	DO I=1,50
        LEAFNG(I)   = 0.0
	  LEAFNS(I)   = 0.0
	  LEAFWTGY(I) = 0.0
	  LFAN(I)     = 0.0
      END DO
        RNH4U = 0.0
        RNO3U = 0.0
        SNH4  = 0.0
        SNO3  = 0.0
        UNH4  = 0.0
        UNO3  = 0.0

!-----------------------------------------------------------------------
!                     DYNAMIC = INTEGR
!-----------------------------------------------------------------------
      ELSEIF(DYNAMIC.EQ.INTEGR) THEN   

      IF (RTWT.GT.0.0) RANC   = ROOTN / RTWT
      IF (STOVWT.GT.0.0) TANC   = STOVN / STOVWT
      
      IF (STMWT.GT.0.0) SANC  = STEMN / STMWT
      IF (LFWT .GT.0.0) LANC  = LEAFN / LFWT
      IF (EAROWT.GT.0.0) THEN
         EANC = EARN / EAROWT
      ELSE
         EANC = 0.0
      ENDIF

      DO L = 1, NLAYR
         RNO3U(L) = 0.0
         RNH4U(L) = 0.0
!         TRLV     = TRLV    + RLV(L)
         TRLV     = TRLV    + RLV(L) * DLAYR(L)
         !KG2PPM(L) = 10.0/(BD(L)*DLAYR(L))
         SNO3(L) = NO3(L) / KG2PPM(L)  ! Converts (mg N/kg) to (kg N/ha)
         SNH4(L) = NH4(L) / KG2PPM(L)  ! JIL 
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
C     Grain N is mobilized before in MZ_GROSUB from shoot and/or root, 
C     thus, TNDEM is calculated from STOVWT(Leaf+Stem)      JIL
C-----------------------------------------------------------------------

      TNDEM  = STOVWT * (TCNP-TANC) + DNG
      RNDEM  = RTWT   * (RCNP-RANC) + PGRORT*RCNP

	SNDEM  = AMAX1(STMWT  * (SCNP-SANC) + PGROST*SCNP, 0.0)
	ENDEM  = AMAX1(EAROWT  * (ECNP-EANC) + PGROEA*ECNP, 0.0)
	LNDEM  = AMAX1(LFWT   * (LCNP-LANC) + PGROLF*LCNP, 0.0)

      ONDEM  = LNDEM + SNDEM + ENDEM     !Organ N demand
      NDEM   = TNDEM  + RNDEM

C-----------------------------------------------------------------------
C     Convert total N demand from g N/plt to kg N/ha (ANDEM)
C-----------------------------------------------------------------------

      ANDEM  = NDEM   * PLTPOP*10.0

C-----------------------------------------------------------------------
C     Calculate potential N supply in soil layers with roots (TRNU)
C     based on soil supply  JIL
C-----------------------------------------------------------------------

      DO L = 1, NLAYR
         IF (RLV(L) .NE. 0.0) THEN
            L1 = L
C
C        The following code was written by JTR to simplify the code for the
C        generic model and to make the functions more like the water uptake
C        functions.  Done on June 28, 1994.
C
C        New water content dependent factor for uptake and new uptake
C        methods that work like the soil water uptake  -- JTR 6/94
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
            TRNU     = TRNU + RNO3U(L) + RNH4U(L)     !kg N/ha
         ENDIF
      END DO

C-----------------------------------------------------------------------
C     Setting a maximum N uptake based on the capacity of the system JIL
C-----------------------------------------------------------------------

	XNU = NX *(1.0-EXP(-0.8*CARBO))*PLTPOP*10.0    !kg N/ha

	IF (TRNU .GT. XNU) THEN
	  NRF = XNU/TRNU

        TRNU = 0.0
	  DO L = 1, L1
	    RNO3U(L) = RNO3U(L) * NRF
	    RNH4U(L) = RNH4U(L) * NRF
          TRNU     = TRNU + RNO3U(L) + RNH4U(L)       !kg N/ha
	  END DO
	ENDIF

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
         UNH4(L) = RNH4U(L)*NUF
         XMIN    = 0.25/KG2PPM(L)  !At least 0.25 mg N/kg soil of NO3, 
	                             !         0.50 mg N/kg soil of NH4 
	                             !must remain in soil layer. 
							       !Original CERES-Maize values 0.1 and 2.0  JIL

         UNO3(L) = MIN (UNO3(L),SNO3(L) - XMIN)
         UNO3(L) = MAX(UNO3(L),0.0)  
         XMIN    = 0.5/KG2PPM(L)
         UNH4(L) = MIN (UNH4(L),SNH4(L) - XMIN)
         UNH4(L) = MAX(UNH4(L),0.0) 
         TRNU    = TRNU + UNO3(L) + UNH4(L)   !kg[N]/ha
      END DO

      IF (PLTPOP .GT. 0.0) THEN
        TRNU = TRNU/(PLTPOP*10.0)             !g[N]/plant
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
         DLEAFN = 0.0
         DSTEMN = 0.0
         DEARN  = 0.0

      IF (NDEM .LE. 0.0 .OR. TRNU .LE. 0.0) THEN
         DSTOVN = 0.0
         DROOTN = 0.0
       ELSE
         !
         ! Calculate root senescence losses @ 0.5%/day
         !
         TRNLOS = 0.0
         DO L = 1, L1
!CHP/JIL 2/1/06 
            !RNLOSS = 0.0
            !IF (TANC .GT. TCNP) THEN
              !RNLOSS = RANC * RTWT * 0.05 * PLTPOP * RLV(L) / TRLV
               RNLOSS = RANC * RTWT * 0.005 * PLTPOP * 
     &                                RLV(L)*DLAYR(L) / TRLV
          !   g[N]/m2 = g[N]/g[root]       * plants/m2
          !                  * g[root]/plant        * fraction
            !ENDIF

            !Calculate N in senesced roots (kg/ha)
            SENESCE % ResE(L,1) = RNLOSS * 10.0
!                      kg[N]/ha =  g/m2  * 10.

            !Back calculate senesced root mass from N senesced.
            IF (RANC .GT. 0.0) THEN
              SENESCE % ResWt(L) = SENESCE % ResE(L,1) / RANC   
            ELSE                               !kg[dry matter]/ha
              SENESCE % ResWt(L) = SENESCE % ResE(L,1) * 10.0 / 0.40   
!              kg[dry matter]/ha =       kg[N]/ha * kg[C]/kg[N]         
!                                                 / kg[C]/kg[dry matter]
            ENDIF
            !Compute lignin, cellulose and carbohydrate portions
            SENESCE % ResLig(L) = SENESCE % ResWt(L) * PLIGRT

            TRNLOS  = TRNLOS + RNLOSS
            !FON(L)  = FON(L) + RNLOSS     !FON is local variable here
         END DO

         ! Adjust DSTOVN and DROOTN to compensate for N lost to FON
         IF(NDEM.GT.0.0.AND.PLTPOP.GT.0.0) THEN
!           DSTOVN = TNDEM / NDEM*TRNU-      PTF*TRNLOS/(PLTPOP*10.0)
!           DROOTN = RNDEM / NDEM*TRNU-(1.0-PTF)*TRNLOS/(PLTPOP*10.0)
!     JIL 06/25/2007
           DSTOVN = TNDEM / NDEM*TRNU-      PTF*TRNLOS/(PLTPOP)
           DROOTN = RNDEM / NDEM*TRNU-(1.0-PTF)*TRNLOS/(PLTPOP)
!          g N/pl =   fraction  *g N/pl- fraction*g N/m2/(pl/m2) 
           IF (DSTOVN .GT. 0.0.AND.ONDEM .GT. 0.0) THEN
               DLEAFN = LNDEM / ONDEM*DSTOVN
               DSTEMN = SNDEM / ONDEM*DSTOVN
               DEARN  = ENDEM / ONDEM*DSTOVN
             ELSE
               DSTEMN = DSTOVN
           ENDIF
         ENDIF
      ENDIF

      STOVN = STOVN + DSTOVN                   !g N/pl
      ROOTN = ROOTN + DROOTN                   !g N/pl
      
      LEAFN = LEAFN + DLEAFN                   !g N/pl
      STEMN = STEMN + DSTEMN                   !g N/pl
      EARN  = EARN  + DEARN                    !g N/pl

      IF(STOVWT.GT.0.0) TANC = STOVN / STOVWT
      IF(LFWT .GT. 0.0) LANC = LEAFN / LFWT
      IF(STMWT.GT. 0.0) SANC = STEMN / STMWT
      IF(EAROWT.GT.0.0) THEN
        EANC = EARN / EAROWT
      ELSE
        EANC = 0.0
      ENDIF

      IF(RTWT.GT.0.1*RTWT.AND.RTWT.GT.0.0) 
     &   RANC  = ROOTN / (RTWT-0.01*RTWT)

      ENDIF       !Endif for DYNAMIC LOOP

      RETURN
      END SUBROUTINE MZ_IX_NUPTAK


C--------------------------------------------------------------------------------------------------
C                         Define Variables
C--------------------------------------------------------------------------------------------------

! ANDEM       !Crop N demand (kg N/ha)
! ANO3        !Total extractable nitrate N in soil profile (kg N/ha)
! ANH4        !Total extractable ammonium N in soil profile (kg N/ha)
! DLAYR(L)    !Soil thickness in layer L (cm)
! DNG         !N demand of potential new growth of tops (g N/plant)
! DROOTN      !Daily change in plant root nitrogen content (g N/plant)
! DSTOVN      !Daily change in plant stover nitrogen content (g N/plant)
! FACTOR      !Ratio of root N uptake to plant N demand
! FNH4        !Unitless soil ammonium supply index
! FNO3        !Unitless soil nitrate supply index
! FON(20)     !Fresh organic nitrogen in soil layer L due to root senescence, kg N/ha
! KG2PPM(20)  !Factor that converts mg elemental N/kg soil to kg N/ha for soil layer L
! L           !Index counter
! L1          !Lowest soil layer with roots
! LL(20)      !Lower limit of plant extractable water for soil layer L, cm3/cm3
! NDEM        !Plant nitrogen demand, g N/plant
! NH4(20)     !Ammonium in soil layer L, mg elemental N/kg soil
! NLAYR       !Number of soil layer
! NO3(20)     !Nitrate in soil layer L (mg elemental N/kg soil)
! NUF         !Plant N supply to demand used to modify N uptake
! PDWI        !Potential increment in new shoot growth, g/plant
! PGRORT      !Potential increment in new root growth, g/plant
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
! TRLV        !Total root length density, cm root/cm2 soil
! TRNLOS      !Total plant N lost by root exudation (g N/m2)
! TRNU        !Total potential root nitrogen uptake, kg N/ha converted to g N/pl
! UNH4(20)    !Plant uptake of ammonium from layer (kg N/ha/day)
! UNO3(20)    !Plant uptake of nitrate from a layer (kg N/ha/day)
! XMIN        !Minimum amount of NO3, NH4 remaining in soil layer (kg N/ha)
! XNDEM       !Temporary variable
! XNU         !Maximum allowable daily N uptake (kg N/ha)
! XSTAGE      !Non-integer growth stage indicator

