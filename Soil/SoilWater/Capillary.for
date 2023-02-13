!  Capillary.for 
!
!  FUNCTIONS:
!  Capillary - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: Capillary which is from Paddy.for of ORYZA2000 for lowland rice soil.
!     Modified for DSSAT use by Jin Wu   02/28/2010
!
!****************************************************************************

      SUBROUTINE Capillary(DYNAMIC,
     &      ActWTD, SOILPROP, SW,          !Input
     &      Capri)                         !Output

!   4/8/2010 note: If water table depth < max root depth, then need to 
!   handle. Caplillary rise mehtod may not be valid.  CHECK!!!

      USE ModuleDefs
      implicit none
      EXTERNAL OPGENERIC, SUWCMS2, SUBSL2
      SAVE

      INTEGER, INTENT(IN) :: DYNAMIC
      REAL, INTENT(IN) :: ActWTD
      TYPE (SoilType), INTENT(IN) :: SOILPROP
      REAL, DIMENSION(NL), INTENT(IN) :: SW !, SWDELTU, RWU
      REAL, DIMENSION(NL), INTENT(OUT) :: CAPRI

 !    Local variables:
      Integer I, NLAYR !, SWITKH, SWITPF
      REAL FLOW, WL(NL), WLAD(NL), WLFC(NL), SAT(NL),MS(NL) !, WCL(NL)
      REAL FLOWUP(NL) 
      Real DUL(NL)   !, FACT, CAPTOT, LL(NL)
      REAL Thick(NL), WLST(NL) !,WLFL(NL+1)
      REAL Top(NL), Bottom(NL), DELT !, CAPRI_Lr(NL)
!     REAL EVSWS(NL), TRWL(NL)
      REAL  VGA(NL), VGL(NL), VGN(NL), WCR(NL)
!      COMMON /NUCHT/VGA, VGL, VGN, VGR
      REAL KST(NL), SWAD(NL), WCSTRP(NL)
!      COMMON /HYDCON/KST, WCAD, WCSTRP
      REAL newSW

!     TEMP CHP
!     output using generic output routine
      INTEGER NVars, Width
      CHARACTER*80  FormatTxt
      CHARACTER*120 HeaderTxt
      REAL CAPR12

!     Curtis Result: Ksat=11.4, LL=0.05, DUL=0.096, SAT=0.46, 
!     VG parameter: alpha=	0.079245615/cm, m=	0.613605717, n=	2.588030008
!     Upflow software use the following data: sand (Param)
!     6.46
!     water content at residual=0.053 
!     Water content at saturation=0.375
!     alpha=0.035
!     n=3.18
!     Saturation 0.38, FC: 0.168, FC equilibrium 0.219, water content : 0.115, ET = 5.0 mm/day

!***********************************************************************
!***********************************************************************
! Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      NLAYR = SOILPROP % NLAYR
      Thick = SOILPROP % DLAYR * 10.   !Thickness of soil layer (mm)
!     LL = SOILPROP % LL
      SAT = SOILPROP % SAT
      WCSTRP = SAT
      SWAD = SOILPROP % WCR       !vol water content air dry (cm3/cm3)
      WCR  = SOILPROP % WCR       !van Genuchten residual water content
      DUL = SOILPROP % DUL
      VGA = SOILPROP % alphaVG
      VGL = 0.5 !1.0??
      VGN = SOILPROP % nVG
!     VGM = SOILPROP % mVG        !Why is VanGenuchten "m" not used?
      KST = SOILPROP % SWCN * 24. !Sat. hydraulic conductivity (cm/d)

      DO I = 1, NLAYR
!         WCSTRP(I) =WCST(I) !Array saturated volumetric water content ripened soil per soil compartment (cm3 cm-3)  
!         VGR =WCAD(I) ! 0.02    
!         VGA(I) =0.026 ! 0.01458 !0.0727 ! 0.0079245615 ! van Genuchten alpha parameter in 1/cm
!         VGL(I) = 0.5 !6.2 !0.001  ! van Genuchten lambda parameter, consider VGL= l (read as el) of RETC
!         VGN(I) = 1.45 !1.3559 !2.588030008 ! van Genuchten n parameter
!         KST(I) =38. ! cm/d 127 !10.0 !120.0   ! Saturated hydraulic conductivity in cm/day
!
         WLAD(I) = SWAD(I) * Thick(I) !WaterAmount at air dry (mm)
         WLFC(I) = DUL(I)  * Thick(I) !The amount of water at FC (mm)
         WLST(I) = SAT(I)  * Thick(I) !The amount of water at Sat (mm)

         if (I .EQ. 1) then 
           Top(I)  = 0.    ! Depth of top of soil compartment(in cm)
         else  
           Top(I)= SOILPROP % DS(I-1)
         Endif
         Bottom(I) = SOILPROP % DS(I)
      END DO

      CAPRI = 0.0
      FLOWUP = 0.0

!     --------------------------------------------------------------------
!     TEMP CHP
!     output using generic output routine
      NVars = 12
      Width = 96
      HeaderTxt = "  CAPRI1  CAPRI2  CAPRI3  CAPRI4  CAPRI5  CAPRI6" // 
     &            "  CAPRI7  CAPRI8  CAPRI9  CAPR10  CAPR11  CAPR12"
      FormatTxt = "(12F8.3)"
      CALL OPGENERIC ( 
     &    NVars, Width, HeaderTxt, FormatTxt,  
     &    CAPRI(1), CAPRI(2), CAPRI(3), CAPRI(4), CAPRI(5), CAPRI(6),
     &    CAPRI(7), CAPRI(8), CAPRI(9), CAPRI(10), CAPRI(11), CAPRI(12))

!***********************************************************************
!***********************************************************************
! DAILY RATE CALCULATIONS
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
      CAPRI = 0.0
!     ZW    = ActWTD
      IF (ActWTD > SOILPROP % DS(NLAYR)) RETURN

!     WCL   = SW
!     WLFL  = 0. ! The fluxes in Ith laye.

      DELT = 1.0 ! Time step in day
      DO I = 1, NLAYR
         WL(I)    = SW(I)  * Thick(I)      !soil water mm
!        EVSWS(I) = -SWDELTU(I) * Thick(I)  !soil evap mm/d
!        TRWL(I)  = RWU(I) * 10.         !root uptake mm/d
      END DO

!      SWITKH = 1  ! HC characteristics are given as parameters of the vG function, 
!      SWITPF = 1
            
      FLOWUP = 0.0
      DO I = NLAYR, 1, -1
        FLOW = 0.
!       If no van Genuchten parameters are available, the soil-water tension (FACT; pF) 
!       is calculated from linear interpolation between the user-supplied characteristic 
!       points of the pF curve (water content at saturation, WCST, at field capacity, WCFC,  
!       at wilting point, WCWP, and at air dryness, WCAD). The soil-water tension in  
!       pF value (FACT) is then transformed into the soil-water tension in mbar (MS).  
!       If van Genuchten parameters are available, the subroutine SUWCMS2 is used to  
!       calcuate MS(I) (Section 6.2.4). 
           
!       IF (WL(I).GT.WLAD(I).AND.WL(I).LT.WLFC(I).AND.ZW.GT.ZL(I) 
!       JZWU change LT to LE
        IF (WL(I) > WLAD(I) .AND.         !SW > air dry
     &      WL(I) <= WLFC(I) .AND.        !SW < DUL
     &      ActWTD >= Bottom(I)) THEN     !water table below this layer

!          if K(theta) are available &  ¦×(theta) i.e. Psi(theta) use van Genuchten function 
!          IF ((SWITKH.NE.0).AND.(SWITPF.EQ.1)) ! VG parameters are given	

!          MS is the upper limit of the integral	
!          SUWCMS2 calculates suction(MS) from SW(WCL) and SAT(WCST). 
           CALL SUWCMS2(I,1,SAT(I),SW(I),MS(I),
     &          VGA, VGN, WCR, SWAD, WCSTRP)

	     !IF (MS(I).GT.33.) ! field capacity (defined as 100 mbar (pF 2))	
	     !Jin Change 100 to 33	
	     IF (MS(I) > 100.) ! field capacity (defined as 100 mbar (pF 2))+
!    &        CALL SUBSL2(LOG10(MS(I)),ZW-ZL(I)+0.5*TKL(I)/10.,I, 	
     &        CALL SUBSL2(LOG10(MS(I)),ActWTD-Top(I)+0.0*Thick(I)/10.,I,
     &           SAT(I),FLOW,     ! FLow at top of layer boundary
     &           VGA, VGL, VGN, WCR, KST, SWAD, WCSTRP)
                ! second argument of SUBSL2 is cm.
!             SUBSL2(PF value soil layer,Distance to ground table in cm,Ith layer,SAT, Capillary rise)

!          If flow negative (percolation), then reset at zero	
           IF (FLOW < 1.E-10) FLOW = 0.	
           FLOWUP(I) = FLOW
          ENDIF
        ENDDO

        DO I = 1, NLAYR
!         New SW moving up from layer below.
          IF (I == 1) THEN
            newSW = MAX(0.0, FLOWUP(I))
          ELSE
            newSW = MAX(0.0, FLOWUP(I) - FLOWUP(I-1))
          ENDIF

!         Don't allow newSW to result in SW > DUL
          CAPRI(I) = MIN(newSW, WLFC(I) - WL(I))
          CAPRI(I) = MAX(0.0, CAPRI(I))

!          Flow will not make the soil water content above DUL
!           Write(*,*)"UpQ for layer at ",I,"th layer is ", FLOW
!     &        ,"mm/d, MS(I)=", MS(I) , ",WCL=",WCL(I)
!           IF (I.EQ.1) THEN	 
!              CAPRI(I) = CAPRI(I-1) + MIN(
!     &          FLOWUP(I) - CAPRI(I-1), 
!     &          (WLST(I) - WL(I)) / DELT + 
!     &          EVSWS(I) + TRWL(I) + FLOWUP(I+1) - FLOWUP(I))
!           ELSE	
!  
!              CAPRI(I) = MIN(FLOW, (WLST(I)-WL(I))/DELT	 + EVSWS(I)
!     &                   + TRWL(I)+  WLFL(I+1) - WLFL(I))
!           END IF
      ENDDO	

!!     --------------------------------------------------------------------
!!     Calculate change in water content in each layer
!      DeltaSW(1) = CAPRI(1)
!      DO I = 2, NLAYR 
!        DeltaSW(I) = CAPRI(I) - CAPRI(I-1)

!     --------------------------------------------------------------------
!     TEMP CHP
!     output using generic output routine
      CAPR12 = 0.0
      IF (NLAYR > 12) THEN
        DO I = 12, NLAYR
          CAPR12 = CAPR12 + CAPRI(I)
        ENDDO
      ELSE
        CAPR12 = CAPRI(12)
      ENDIF
      CALL OPGENERIC ( 
     &    NVars, Width, HeaderTxt, FormatTxt,  
     &    CAPRI(1), CAPRI(2), CAPRI(3), CAPRI(4), CAPRI(5), CAPRI(6),
     &    CAPRI(7), CAPRI(8), CAPRI(9), CAPRI(10), CAPRI(11), CAPR12)
!
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE Capillary


C=====================================================================
!     Capillary VARIABLE DEFINITIONS: (updated Dec 2009)
!-----------------------------------------------------------------------
! CAPRI()    Water flow to Ith soil layer (mm/d)
! CAPTOT     Total capillary rise from the groundwater table (mm/d)
! DELT       Time step in day
! EVSWS      The extraction by evaporation. Actual evaporation rate of top soil layer (mm/d)
! FACT       Intermediate factor
! Flow       Capillary rise calculated by subroutine SUBSL2 (mm/d)
! KST()      Array of saturated hydraulic conductivity, per soil layer (cm/d)
! MS()       The soil-water tension of Ith layer in (mbar= 1.0197g/cm2=1.097cm of water)
!            Array of soil-water tension (suction), per soil layer (cmH2O)
! NL         Defines the total number of soil layers (puddled and nonpuddled) and has a maximum value of 10 (including NLPUD). 
!                For example, NL and NLPUD can be set at 8 and 3, respectively, that is, a soil profile with three 
!                puddled soil layers (of which the third represents the plow sole) and five layers in the nonpuddled subsoil.
! PN         Power function parameter of hydraulic conductivity for each soil layer (see ORYZA book page 130,182)
!            KMS = KST(I)*(MS**PN(I))
! SWITKH     Hydraulic conductivity switch. 
!            SWITKH = 0, no hydraulic conductivity characteristics are available
!            SWITKH = 1  HC characteristics are given as parameters of the van Genuchten function, 
!            SWITKH = 2  HC characteristics are given as parameters of the power function. 
!            The van Genuchten and power functions are explained in the subroutine SATFLX (in “Dynamic calculation of percolation rate?in Section 6.2.2).
! SWITPF     Water retention curve switch. whether water retention characteristics (per soil layer) 
!            are given as water content values at specific water tensions (saturation, field capacity, wilting point, and when air-dried) (SWITPF = 0), 
!            or as parameter values of the van Genuchten function (SWITPF = 1).
! TKL(I)     Thickness of water compartment in m originally. 
!            But line 328 of Paddy.for converts TKL from m into mm; calculate water contents in mm 
! Thick(I)   chp changed TKL to Thick 2023-02-13
! TRWL()     The extraction of water by transpiration 
!            Water extraction rate by roots per compartment mm d-l
!            Array of actual water withdrawal by transpiration, per soil layer
! VGA        van Genuchten alpha parameter in cm-1 
! VGL        van Genuchten lambda parameter. Consider VGL= l (read as el) of RETC
! VGN        van Genuchten n parameter
! VGR        van Genuchten residual water content
! WCR()      chp changed VGR to WCR 2023-02-13
! WCAD()     Volumetric water content at air dry(m3/m3)
! SWAD()     chp changed WCAD to SWAD 2023-02-13
! WCFC()     Array of soil water content at field capacity, per soil layer(m3/m3)
! DUL()      chp changed WCFC to DUL 2023-02-13
! WCL()      Array of actual soil water content, per soil layer (m3/m3)
! WCST()     Array of soil water content at saturation, per soil layer (m3/m3)'
! SAT        chp changed WCST to SAT 2023-02-13
! WCSTRP()   Array saturated volumetric water content ripened soil per soil compartment (m3 m-3)  (previously puddled)
! WL(I)      The actual amount of water in the soil layer (mm) 
! WLAD(1)    Amount of water at air dryness in the soil layer (mm)
! WLFC()     The amount of water at FC (mm) 
! WLFL(I)    The fluxes at boundaries of Ith laye. (mm/d)
!              e.g. WLFL(2) is the flow rate between soil layer (1) and soil layer (2), 
! FLOWUP(I)  chp - is this the same as WLFL? I'm assuming so since it seems to be the flux between layers. 
!            FLOWUP(I) = flow from layer (I+1) to layer I (mm/d)
! WLST()     The amount of water at Saturation (mm)
! ZL()       Depth of top of soil compartment(cm)
! Top()      chp changed ZL to Top 2023-02-13
! ZW         Depth of groundwater table below soil surface(cm)
! ActWTD     chp changed ZW to ActWTD 2023-02-13
!-----------------------------------------------------------------------
!     END SUBROUTINE Capillary
!=======================================================================
