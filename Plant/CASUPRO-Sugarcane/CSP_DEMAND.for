C=======================================================================
C  CSP_DEMAND, Subroutine, from DEMAND subroutine 
C  by J.W. Jones and G. Hoogenboom.
C-----------------------------------------------------------------------
C  Calculates potential demand for C and N based upon new growth and
C  existing N deficiency in old tissue.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  01/01/1990 JWJ Written.
C  02/01/1993 GH  Revised.
C  04/24/1994 NBP Changed TAIRHR to TGRO.
C  08/22/1995 GH  Added seed composition routine from KJB & ELPiper
C  04/02/1996 JWJ Modified partitioning during early growth
C  01/10/1997 GH  Added TURFAC effect on seed growth and pod addition
C  09/15/1998 CHP Modified for modular format
C  05/10/1999 GH  Incorporated in CROPGRO
!  11/08/2001 O.H. Daza modified for the sugarcane model
C  08/19/2003 FSR Modified for DSSAT 4.0 CASUPRO sugarcane 
C  07/26/2004 CHP Removed variables which were not being used
C-----------------------------------------------------------------------
C  Called by:  CASUPRO
C  Calls:      CSP_IPDMND
C=======================================================================

      SUBROUTINE CSP_DEMAND(DYNAMIC,
     &    AGRLF, AGRRT, AGRSTM, AGRSU, CROP, DLFN, DPLARF,    !Input
     &    DRPP, FILECC, FILEIO, NSTRES, NVEG0, PAR,           !Input
     &    PCNL, PCNRT, PCNST, PCNSU, PGAVL, PLTPOP, RPROAV,   !Input
     &    RTWT, SetStalk, STMWT, SUWT, SWFAC, TAVG, TDUMX,    !Input
     &    TURFAC, VSTAGE, WCRLF, WCRRT, WCRST, WCRSU, WNRLF,  !Input
     &    WNRRT, WNRST, WNRSU, WTLF, YRDOY, YRSIM,            !Input
     &    AGRVG, AGRVG2, F, FNINL, FNINR, FNINS, FRLF,        !Ouput
     &    FRRT, FRSTM, FRSU, NDMNEW, NDMOLD, NDMTOT,          !Ouput 
     &    NDMVEG, NMINEP, NMOBR)                              !Ouput 

!-----------------------------------------------------------------------
      USE ModuleDefs
      USE ModuleData

      IMPLICIT NONE
      EXTERNAL CSP_IPDMND, PRNT_DEMAND_SC, TABEX
      SAVE

      CHARACTER*2 CROP
!      CHARACTER*3 TYPSDT
      CHARACTER*30 FILEIO
      CHARACTER*92 FILECC !, FILEGC

!    INTEGER RUNINIT, SEASINIT, EMERG, INTEGR
!     PARAMETER (RUNINIT = 1, SEASINIT = 2, EMERG = 3, INTEGR = 4)
!     INTEGER TS
!     PARAMETER (TS = 24)
      INTEGER DYNAMIC   !, TIMDIF
      INTEGER DAS, YRSIM  !NPP, I, 
      INTEGER YRDOY, NVEG0

!      REAL FRSTMM, NVSTL,NVSTS,NVSTR
      REAL TABEX  !,CURV

      REAL TPHFAC,PARSLA,FFVEG, SLAVAR, SLAREF, !GROYES,GAINNW,GAINWT, 
     &  FINREF, SLAMAX, SLAMIN, AGRRT, AGRSTM, FRLFF, FRSTMF

      REAL AGRLF, AGRVG, AGRVG2, CDMTOT, CDMVEG, CNOLD, DRPP, DUMFAC, F,
     &  FNINL, FNINR, FNINS, FRLF, FRLFMX, FRRT, FRSTM, FVEG, !GROMAX, 
     &  NDMNEW, NDMOLD, NDMTOT, NDMVEG, NMINEP, NMOBMX, NMOBR, NSTRES, 
     &  NVSMOB, PAR, PCNL, PCNRT, PCNST, PGAVL, PLTPOP, PROLFF,
     &  PROLFI, PRORTF, PRORTI, PROSTF, PROSTI, RCH2O, RLIG, RLIP, 
     &  RMIN, RNO3C, RPRO, ROA, RPROAV, RTWT, SIZELF, SIZREF, SLAMN, 
     &  SLAMX, SLAPAR, STMWT, SWFAC, TAVG, TDUMX, SIZRAT, TURFAC,!SRMAX,
     &  TURSLA, VSSINK, VSTAGE, WCRLF, WCRRT, WCRST, WNRLF, WNRRT, 
     &  WNRST, WTLF

!      REAL TEMXFR,CAVTOT,GDMSDO

      REAL XVGROW(6), YVREF(6)    !, YVGROW(6)
      REAL XSLATM(10), YSLATM(10)
      REAL XLEAF(25), YLEAF(25), YSTEM(25)
      REAL TURFSL

!Sugars
      REAL AGRSU, FNINSU, FRSU, PROSUF, PROSUI, WNRSU !, NVSTSU
	REAL SUWT, WCRSU, PCNSU

!New
      REAL ACDMVEG, DPSTK, STKRAT, STKVAR, STKREF
      REAL DLFN, DPLFA, DPLFG, DPLFAG, DPLARF, DPSGRF
	REAL PGROSTK    !GROSTKYES, 
	REAL DPSTKG, DPRTG, DPSUG
	REAL FRRTK, FRSUK, XFRRT(4), YFRRT(4), XFRSU(4), YFRSU(4)
	REAL RFPSTKG, RSDC, EXCAR   !GRORTYES, PGRORT, 
	REAL WLFPLUS, WSTKPLUS, WRTPLUS, WSUPLUS, GAMMA, WPLUS

!	REAL X1, X2, Y1, Y2 

! SKPOP Temporal variable to estimate stalk growth rate in funtion 
! estimated from experiment
	INTEGER SetStalk, SKPOP

!	INTEGER OpenStatus
      
	LOGICAL SUBTITLE
      TYPE (ControlType) CONTROL
      CALL GET(Control) 

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------

      CALL CSP_IPDMND(FILECC, FILEIO,                     !Input
     &  FINREF, FRLFF, FRLFMX, FRSTMF, NMOBMX, NVSMOB, PROLFF,  !Output
     &  PROLFI, PRORTF, PROSUF, PROSUI, PRORTI, PROSTF, PROSTI, !Output
     &  RCH2O, RLIG, RLIP, RMIN, RNO3C, RPRO, ROA, SIZELF,      !Output
     &  SIZREF, SLAMAX, SLAMIN, SLAPAR, SLAREF, SLAVAR, TURSLA, !Output
     &  VSSINK, XLEAF, XSLATM, XVGROW, YLEAF, YSLATM, YSTEM,    !Output
     &  YVREF, XFRRT, YFRRT, XFRSU, YFRSU, GAMMA)               !Output

      STKVAR = 1 ! g/cm
	STKREF = 1 ! g/cm

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
	NDMNEW = 0.0
	NDMVEG = 0.0
      NDMTOT = 0.0
	NMINEP = 0.0
	NMOBR  = 0.0
	FNINL  = 0.0
      FNINS  = 0.0
      FNINR  = 0.0
	FNINSU = 0.0  !Sugars

      SKPOP = 1  ! Temporary

!	X1 = 14
!	Y1 = 0.15
!	X2 = 30
!	Y2 = 0.05
      
C-----------------------------------------------------------------------
C     SET VARIETY SPECIFIC LEAF PARAMETERS
!     Scaling of leaf parameters of current cultivar with respect to 
!     the selected cultivar
C-----------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
        DUMFAC = SLAVAR / SLAREF
        F      = DUMFAC * FINREF
        FVEG   = DUMFAC * SLAMAX
        SLAMN  = DUMFAC * SLAMIN
        SLAMX  = DUMFAC * SLAMAX
!        GROMAX = 0.0
        SIZRAT = SIZELF / SIZREF
!NEW
	  STKRAT = STKVAR / STKREF  !Ratio of stalk weights
        DPLFA = 0  !Former GROMAX

! SC Area values are calculated in PHENOL_SC

!        DO I = 1,6
!          YVGROW(I) = SIZRAT * YVREF(I)  ! look up table in .SPE file
!        ENDDO

C-----------------------------------------------------------------------
C     INITIALIZE PARTITIONING PARAMETERS
C-----------------------------------------------------------------------
!       look up table in .SPE file
!        FRLF = TABEX(YLEAF,XLEAF,0.0,8)  !leaf
        
!	  FRSTM = TABEX(YSTEM,XLEAF,0.0,8) !stalk

!        FRSU = 0.0  !Sugars

!        FRRT = 1.0 - FRLF - FRSTM - FRSU !root

! SC Initialization
	  FRLF  = 0.7
	  FRSTM = 0.05
	  FRRT  = 0.1
	  FRSU  = 1.0 - FRLF - FRSTM - FRRT

	  PGROSTK = 0.0  !Growth of stalks

        DPSTKG  = 0.0  !Delta stalks
	  DPRTG   = 0.0  !Delta roots
	  DPSUG   = 0.0  !Delta sugars
      ENDIF

!***********************************************************************
!***********************************************************************
!     EMERGENCE CALCULATIONS - Performed once per season upon emergence
!       or transplanting of plants
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. EMERG) THEN
!-----------------------------------------------------------------------
! Converts protein content in plant components to nitrogen
! g [N]/g [leaf tissue] = g [Protein]/g [leaf tissue] x 0.16 g [N]/g [Protein] 

! PROLFI    Maximum protein composition in leaves during growth with 
!             luxurious supply of N (g [protein] / g [leaf tissue])
! PRORTI    Maximum protein composition in roots during growth with 
!             luxurious supply of N (g [protein] / g [root])
! PROSTI    Maximum protein composition in stalks during growth with 
!             luxurious supply of N (g [protein] / g [stem])
! PROSUI    Maximum protein composition in sugars during growth with 
!             luxurious supply of N (g [protein] / g [sugars])

        FNINL  = PROLFI * 0.16  !leaf
        FNINS  = PROSTI * 0.16  !stalk
        FNINR  = PRORTI * 0.16  !root
! SC
        FNINSU = PROSUI * 0.16  !Sugars

C***********************************************************************
C***********************************************************************
C     DAILY RATE/INTEGRATION
C***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
C-----------------------------------------------------------------------
!     DAS = MAX(0,TIMDIF(YRSIM,YRDOY))
      DAS = CONTROL % DAS

C-----------------------------------------------------------------------
C     Compute max N mining, NMINEP, based on stage-dependent mining
C     rate, NMOBR
C-----------------------------------------------------------------------
C     Assume that a Maximum Fraction (NMOBMX) of N can be Mobilized per Day

! The part below (*) may not be applicable to sugarcane. 
! The decision for dropping it is postponed until we get a better 
! understanding for SC.
! NVSMOB is the relative N mobil rate in veg stage, rel to reprod. stage
C-----------------------------------------------------------------------
C     9/27/95 ACCELERATE N MOBILIZATION AFTER R5, FUNCTION OF (1-SWFAC)
C     ALLOWS ACCELERATING BY 50% IF MAX DEFICIT.
C     2/6/96 SOMETIMES SEEDS FILL, XPOD IS LOW, THEN N MOBILIZATION SLOWS
C     I DON'T REALLY WANT THAT, LATE IN CYCLE.  KJB
C     NOW, DXR57 HITS CLOSE TO 1 AT MATURITY AND PREVENTS THAT
C-----------------------------------------------------------------------

! NMOBMX    Maximum fraction of N which can be mobilized in a day 
! NMOBR     Stage-dependent potential N mining rate expressed as a 
!             fraction of the maximum rate (NMOBMX)
! NVSMOB    Relative rate of N mining during vegetative stage to that in 
!             reproductive stage 
! TDUMX     Photo-thermal time that occurs in a real day based on early 
!             reproductive development temperature function
!             (photo-thermal days / day)

      TDUMX = 1.0

      NMOBR = NVSMOB * NMOBMX * TDUMX

! (*)
!      IF (DAS .GT. NR5) THEN
!        NMOBR = NMOBMX * TDUMX2 * (1.0 + 0.5*(1.0 - SWFAC))
!     &      * (1.0 + 0.3*(1.0 - NSTRES)) * (NVSMOB + (1. - NVSMOB)
!     &      * MAX(XPOD,DXR57**2.))
!      ENDIF

! NMINEP    Potential N mobilization from storage (g [N] / m2 - d)
! WNRLF     N available for mobilization from leaves above lower limit of 
!             mining (g [N] / m2)
! WNRRT     N available for mobilization from roots above lower limit of 
!             mining (g [N] / m2)
! WNRST     N available for mobilization from stalks above lower limit of 
!             mining (g [N] / m2)
! WNRSU     N available for mobilization from sugars above lower limit of 
!             mining (g [N] / m2)

      NMINEP = NMOBR * (WNRLF + WNRST + WNRRT + WNRSU)

C-----------------------------------------------------------------------
! Several lines from DEMAND for seed and shell (growth and N) were 
! deleted here
C-----------------------------------------------------------------------

C     Vegetative partitioning factors and demand for C and N for new
C     growth before VSSINK, assume leaf expansion is fixed, compute
C     SLA based on function of light, temp, etc, then compute
C     FRLF (leaf partitioning), then FRRT, FRSTM
C-----------------------------------------------------------------------
C     Check to see if new vegetative tissue can be grown, using PGAVL
C-----------------------------------------------------------------------
!      CDMVEG = MAX(0.0,(1.0 - XFRT) * PGAVL)

! CDMVEG    Carbon demand for vegetative growth (g [CH2O] / m2 - d)
! NDMVEG    N required for vegetative growth if all PGAVL is used as 
!             computed (g [N] / m2 - d)
! PGAVL     Total available CH2O available for growth & respiration
!             (g [CH2O] / m2)

      NDMVEG = 0.0

!      CDMVEG = (PGAVL * XFRT - CDMREP) + CDMVEG

C-----------------------------------------------------------------------
C     Calculate Pattern of Vegetative Partitioning, a function of V-STAGE
C-----------------------------------------------------------------------
! VSTAGE = LeafNumber from PHENOL_SC. This is not needed any longer as it 
! will be passed from PHENOL_SC

!      FRLF  = TABEX(YLEAF,XLEAF,VSTAGE,8)  
!      FRSTM = TABEX(YSTEM,XLEAF,VSTAGE,8)
!      FRSU = 0
!      FRRT = 1. - FRLF - FRSTM - FRSU

!-----------------------------------------------------------------------
C     Compute F, specific leaf area for new leaf weight
C-----------------------------------------------------------------------
! TPHFAC    Reduction in specific leaf area due to daytime temperature 
!             being less than optimal (0-1) 

!      TPHFAC = 0.
!      DO I = 1,24
!        TPHFAC = TPHFAC + TABEX (YSLATM,XSLATM,TGRO(I),5)
!      ENDDO
!      TPHFAC = TPHFAC/24.

! SC Not used but code above is kept intact for future reference
      TPHFAC = 1.0  

C-----------------------------------------------------------------------

      PARSLA = (SLAMN + (SLAMX - SLAMN) * EXP(SLAPAR * PAR)) / SLAMX

      TURFSL = MAX(0.1, (1.0 - (1.0 - TURFAC) * TURSLA))

C-----------------------------------------------------------------------
C     Compute overall effect of TMP, PAR, water stress on SLA (F)

! First for veg stages, then transition to rep stage from R1 to end leaf
C   effect of PAR on SLA, COX PEANUT SCI. 5:27, 1978
C-----------------------------------------------------------------------

      FFVEG = FVEG * TPHFAC * PARSLA * TURFSL

      F = FFVEG

!      IF (XFRT*FRACDN .GE. 0.05) F = FFVEG * (1.0 - XFRT * FRACDN)
C-----------------------------------------------------------------------
C     For determinate plants (XFRUIT=1.) leaf expansion stops at NDLEAF
C-----------------------------------------------------------------------
!      IF (XFRUIT .GT. 0.9999 .AND. DAS .GE. NDLEAF) F = 0.0

C-----------------------------------------------------------------------
C     During early vegetative growth, leaf area expansion depends on
C     VSTAGE (Prior to VSSINK). This sets FRLF, partitioning of d.m.
C     to leaves. FRRT and FRSTM are then computed by left over C. When
C     an upper limit of d.m. goes to leaves, leaf area expansion is
C     restricted so that F is maintained as computed and minimal amounts
C     of C is partitioned to FRSTM and FRRT  (JWJ 4/1/96)
C-----------------------------------------------------------------------
!  Potential growth of leaves
!-----------------------------------------------------------------------
! Sugarcane is not limited by VSSINK. The following lines will be 
! commneted out to include calculations for the sugarcane model. 
! Values are passed from PHENOL_SC

! GAINNW    Leaf area added (prior to VSSINK) (cm2 [leaf] / m2 [ground])
! GAINWT    Leaf weight added (prior to VSSINK and after NDLEAF)
!             (g [leaf] / m2 [ground])
! GROMAX    Maximum leaf area which can be added per plant between 
!             emergence and day of simulation as a function of V-stage on 
!             day of simulation (cm2 [leaf] / plant)
! GROYES    Maximum leaf area which could have been added per plant between 
!             emergence and yesterday as a function of V-stage
!             (cm2 [leaf] / plant)

!      IF (VSTAGE .LT. VSSINK) THEN
!        GROYES = GROMAX

! SC This value is calculated for each day of simulation in PHENOL_SC
! DPLFA    DeltaPotLeafArea

! DPLARF    Potential rate of increase of leaf area per plant of 
!            reference cane variety (cm2 / plant - d)
! DPLFA    Potential rate of increase of leaf area per plant of 
!            selected cane variety (cm2 / plant - d)
! DPLFAG   Potential rate of increase of leaf area per ground unit 
!            (cm2 [leaf] / m2)
! TURFAC   Water stress factor for expansion (0 - 1)

        DPLFA = SIZRAT * DPLARF * TURFAC

!        GROMAX = TABEX(YVGROW,XVGROW,VSTAGE,6) * SIZELF / SIZREF

! SC It is required to have the leaf area of the whole plant 
! (sum of leaf area of each stalk: See PHENOL_SC)
!        GROMAX = DPLFAG
!        GAINNW = (GROMAX - GROYES) * PLTPOP

! (cm2 [leaf] / m2-d) = (cm2 [leaf] / plant-d) (plants / m2)
        DPLFAG = DPLFA * PLTPOP

C-----------------------------------------------------------------------
C     CALCULATE MINIMUM WEIGHT NEEDED TO ADD GAINNW LEAF AREA/M2,
C     AND AMOUNT OF LEAF WEIGHT WHICH CAN BE GROWN WITH PG AVAILABLE
C-----------------------------------------------------------------------
!        IF (F .GT. 0.0) THEN
!          GAINWT = GAINNW / F
!        ELSE
!          GAINWT = 0.0
!        ENDIF

! DPLFG    potential increase of leaf growth (g [leaf]/m2 [ground] - d )
! g [leaf]/m2 [ground] - d = (cm2 [leaf]/m2 [ground]-d) / (cm2 [leaf]/g [leaf])

        IF (F .GT. 0.0) THEN
          DPLFG = DPLFAG / F
        ELSE
          DPLFG = 0.0
        ENDIF

C-----------------------------------------------------------------------
C Include the gains in weight of stalks, roots and sugars
C-----------------------------------------------------------------------
! Potential growth of stalks
C-----------------------------------------------------------------------
!DelPotLfGrow = 
!DelPotStlkGrow =
!DelPotRtGrow =
!DelPotSuGrow =

! DPRTG    potential increase of root growth (g [root] / plant)
! DPSUG    potential increase of sugars accumulation (g [sugars] / plant)

! GROSTKYES  Growth of stalks yesterday (g [stalk] / plant)
! PGROSTK    Potential growth of stalks today (g [stalk] / plant)

!        GROSTKYES = PGROSTK

! This a temporary function deduced from experiment Lote 14.
! Used here as an approximation for the time being.
! g [stalk] / stalk = (g [stalk] / m2) / (stalks / m2)

! It will be important to have the functions in the same notation.
! Use either accumulation or rates in all functions as a standard.
! PGROSTK = (2.8929 * VSTAGE ** 2 + 9.3593 * VSTAGE) / SKPOP

! The function below is the first derivative of the equation above
! RFPSTKG Rate function of potential stalk growth (g [stalk]/m2 - leaf)
! (g [stalk] / stalk - leaf) = (g [stalk] / m2 - leaf) / (stalks/m2)

        RFPSTKG = (5.7858 * VSTAGE + 9.3593) / SKPOP * TURFAC

! DPSGRF Potential growth of stalk of reference cane variety (g [stalk] / m2)
! (g [stalk]/plant) = (stalks/plant) (g [stalk]/stalk-leaf) * leaf
! DPSTK  Potential growth of stalk of selected cane variety 

        DPSGRF = SetStalk * RFPSTKG * DLFN

	  DPSTK = STKRAT * DPSGRF

! DPSTKG   potential increase of stalk growth (g [stalk] / m2)
! g [stalk]/m2 = (g [stalk]/plant) (plants/m2)

        DPSTKG = DPSTK * PLTPOP

C-----------------------------------------------------------------------
! Potential growth of roots
C-----------------------------------------------------------------------
! FRRTK     fraction of potential leaf and stalk growth that is used for 
!             growth of roots

! Use TABEX  function instead
!        IF (VSTAGE <= X1) THEN
!	    FRRTK = Y1
!        ELSE IF (VSTAGE > X1 .AND. VSTAGE <= X2) THEN   
!          FRRTK = (Y2 - Y1) / (X2 - X1) * (VSTAGE - X1) + Y1
!        ELSE
!	    FRRTK = Y2
!        END IF

        FRRTK = TABEX(YFRRT, XFRRT, VSTAGE, 4)
	
! Potential root growth is a fraction of the sum of rates of potential 
! leaf growth and potential stalk growth
! When TURFAC=1 (No water stress) the right most term becomes 1
	  DPRTG = FRRTK * (DPLFG + DPSTKG) * (2 - TURFAC)

!CHECK THIS


C-----------------------------------------------------------------------
! Potential growth of sugars
C-----------------------------------------------------------------------
! FRSUK     fraction of potential leaf and stalk growth that is 
!             depositioned as sugars

        FRSUK = TABEX(YFRSU, XFRSU, VSTAGE, 4)

! Potential sugar growth is a fraction of the sum of rates of potential 
! leaf growth and potential stalk growth

        DPSUG = FRSUK * (DPLFG + DPSTKG)

C-----------------------------------------------------------------------
C     Compute fraction of C partitioned to leaves, based on F, VSSINK
C     Limit leaf pertitioning to FRLFMX (i.e., FRLFMX = 0.7)
C-----------------------------------------------------------------------
! g [leaf]/g [veg] = (g [CH2O]/g [leaf]) * (g [leaf]/m2 [ground])/(g [CH2O]/m2-d)
!        FRLF = (AGRLF * GAINWT) / (CDMVEG + 0.0001)

!        IF (FRLF .GT. FRLFMX) THEN
!          GAINWT = (CDMVEG / AGRLF) * FRLFMX
!          GAINNW = GAINWT * F
!          FRLF = FRLFMX
!        ENDIF
!
!-----------------------------------------------------------------------
! NEW Carbon demand for potential growth
!-----------------------------------------------------------------------
! AGRLF     Mass of CH2O required for new leaf growth (g [CH2O] / g [leaf])
! AGRRT     Mass of CH2O required for new root growth (g [CH2O] / g [root])
! AGRSTM    Mass of CH2O required for new stalk growth (g [CH2O] / g [stalk])
! AGRSU     Mass of CH2O required for new sugar growth (g [CH2O] / g [sugar])
! CDMVEG    Carbon demand for vegetative growth (g [CH2O] / m2 - d)
! ACDMVEG   Actual carbon demand for vegetative growth (g [CH2O] / m2 - d)
! RSDC      Ratio of supply demand for carbon

! Better replace this statement with (YRDOY > YREMRG)      
	IF (VSTAGE > 0) THEN
!      IF YRDOY > YREMRG  
        CDMVEG = AGRLF * DPLFG + AGRSTM * DPSTKG + AGRRT * DPRTG +
     &	       AGRSU * DPSUG
        IF (PGAVL > 0) THEN
          RSDC = PGAVL / CDMVEG
          IF (RSDC >= 1.0) THEN  !Excess of carbon
            EXCAR    = PGAVL - CDMVEG
	      WLFPLUS  = AGRLF * DPLFG
!Make GAMMA a function of leaf number
            WSTKPLUS = AGRSTM * DPSTKG + GAMMA * EXCAR !CHECK UNITS
!            WSTKPLUS = DPSTKG
	      WRTPLUS  = AGRRT * DPRTG
	      WSUPLUS  = AGRSU * DPSUG + (1 - GAMMA) * EXCAR  !CHECK UNITS
!	      WSUPLUS  = DPSUG
	      ACDMVEG  = CDMVEG  !Updates carbon actually used
          ELSE  !Deficit of carbon
	      WLFPLUS  = RSDC * AGRLF * DPLFG
            WSTKPLUS = RSDC * AGRSTM * DPSTKG
	      WRTPLUS  = RSDC * AGRRT * DPRTG
	      WSUPLUS  = RSDC * AGRSU * DPSUG
	      ACDMVEG  = PGAVL  !Updates carbon actually used
          END IF

!-----------------------------------------------------------------------
C    Recompute FRSTM and FRRT based on FRLF
! SC has the sugar component. It is included in the relations to 
! account for it.
C-----------------------------------------------------------------------
! Compute actual fractions FRLF, FRSTM, FRRT and FRSU allocated to each 
! plant component on each day, based on carbon availability
! FRLF      Fraction of vegetative tissue growth that goes to leaves on a 
!             day (g [leaf] / g [veg])
! FRRT      Fraction of vegetative tissue growth that goes to roots on a 
!             day (g [root] / g [veg])
! FRSTM     Fraction of vegetative tissue growth that goes to stems on a 
!             day (g [stem] / g [veg])
! FRSU      Fraction of vegetative tissue growth that goes to sugars on a 
!             day (g [root] / g [veg])

          WPLUS = WLFPLUS + WSTKPLUS + WRTPLUS + WSUPLUS

!         FRLF  = AGRLF * WLFPLUS / ACDMVEG
!	    FRSTM = AGRSTM * WSTKPLUS / ACDMVEG
!	    FRRT  = AGRRT * WRTPLUS / ACDMVEG
!         FRSU  = AGRRT * WSUPLUS / ACDMVEG

!  	    FRSU  = 1.0 - FRLF - FRSTM - FRRT

!!!!!  	    FRSU  = AGRSU * WSUPLUS / ACDMVEG

          FRLF  = WLFPLUS / WPLUS
	    FRSTM = WSTKPLUS / WPLUS
	    FRRT  = WRTPLUS / WPLUS
  	    FRSU  = WSUPLUS / WPLUS
!  	    FRSU  = 1.0 - FRLF - FRSTM - FRRT

        END IF  

!        FRSTM = (1.0 - FRLF) * FRSTM / (FRSTM + FRRT)
! SC
!        FRSTM = (1.0 - FRLF) * FRSTM / (FRSTM + FRRT + FRSU)
!        FRRT = (1.0 - FRLF) * FRRT / (FRSTM + FRRT + FRSU)

! SC Sugar is allocated after leaf, stalk and root
!        FRSU  = 1.0 - FRLF - FRSTM - FRRT
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C     Compute CH2O cost per g of tissue, excluding cost for protein (AGRVG)
C     and total CH2O cost per g of veg tissue (AGRVG2)
C-----------------------------------------------------------------------
! AGRVG     Mass of CH2O required for vegetative tissue growth including 
!             stoichiometry and respiration (g [CH2O] / g [tissue])
! AGRVG2    Total mass of CH2O required for vegetative tissue growth
!             (g [CH2O] / g [tissue])
! RPROAV    Respiration required for protein synthesis, average based on 
!             sources of N (g [CH2O] / g [protein])

! Updates AGRVG and AGRVG2

      AGRVG = AGRLF * FRLF + AGRRT * FRRT + AGRSTM * FRSTM + 
     &        AGRSU * FRSU

      AGRVG2 = AGRVG + (FRLF * PROLFI + FRRT * PRORTI + FRSTM * PROSTI +
     &                  FRSU * PROSUI) * RPROAV

C-----------------------------------------------------------------------
C    Compute N Demand for new tissue, including reproductive and vegetative

C SC Compute N Demand for new vegetative tissue for sugarcane
C-----------------------------------------------------------------------
! NDMNEW    Total N demand for new growth (g [N] / m2 - d)
! NDMOLD    N demand for old tissue (g [N] / m2 - d)
! NDMTOT    Total N demand (g [N] / m2 - d)
! NDMVEG    N required for vegetative growth if all PGAVL is used as 
!             computed (g [N] / m2 / d)
! SC    
      NDMVEG = (ACDMVEG / AGRVG2) * (FRLF * FNINL + FRSTM * FNINS +
     &          FRRT * FNINR + FRSU * FNINSU)

!      NDMNEW = NDMREP + NDMVEG
! SC
      NDMNEW = NDMVEG

C-----------------------------------------------------------------------
C    Check to see if any C is left after reproductive growth for
C    reducing N to re-fill old tissue, if N can be taken up by roots
C-----------------------------------------------------------------------
!      CNOLD = MAX(0.0,PGAVL-CDMREP)

! CNOLD Available CH2O after reproductive growth (g [CH2O]/m2-d)

! SC does not have reproductive partitioning. Therefore XFRT=0 and CDMREP=0
      CNOLD = 0
      NDMOLD = 0.0

C-----------------------------------------------------------------------
C    Nitrogen demand for old tissue
C-----------------------------------------------------------------------
!      IF (DAS .GT. NVEG0 .AND. DAS .LT. NR7 .AND.
!     &          CNOLD .GT. 0.0) THEN
! SC if CNOLD = 0 this IF condition will never be met. To account for 
! N demand for old tissue explicitly, this IF and the inner IF are 
! removed.
!      IF (DAS .GT. NVEG0 .AND. CNOLD .GT. 0.0) THEN

!        NVSTL = FNINL
!        NVSTS = FNINS
!        NVSTR = FNINR
! SC
!        NVSTSU = FNINSU  !Sugars

!        IF (DXR57 .GT.0.0) THEN
!           FRNLFT = (NRCVR + (1. - NRCVR) * (1. - DXR57**2))
!           NVSTL = PROLFF*0.16 + (FNINL-PROLFF*0.16) * FRNLFT
!           NVSTS = PROSTF*0.16 + (FNINS-PROSTF*0.16) * FRNLFT
!           NVSTR = PRORTF*0.16 + (FNINR-PRORTF*0.16) * FRNLFT
!        ENDIF

! Last line for sugars 
!        NDMOLD = (WTLF  - WCRLF) * MAX(0.0,(NVSTL  - PCNL  / 100.))
!     &         + (STMWT - WCRST) * MAX(0.0,(NVSTS  - PCNST / 100.))
!     &         + (RTWT  - WCRRT) * MAX(0.0,(NVSTR  - PCNRT / 100.))
!     &         + (SUWT  - WCRSU) * MAX(0.0,(NVSTSU - PCNSU / 100.))

!        IF (NDMOLD .GT. (CNOLD / RNO3C * 0.16)) THEN
!          NDMOLD = CNOLD / RNO3C * 0.16
!        ENDIF
!      ENDIF
C-----------------------------------------------------------------------
C    Total N demand
C-----------------------------------------------------------------------
!      NDMTOT = NDMREP + NDMVEG + NDMOLD

      NDMTOT = NDMVEG + NDMOLD  ! Sugarcane

C-----------------------------------------------------------------------
C    Compute total demand for C, and max. C that could be mined
!     CDMTOT not used - chp
C-----------------------------------------------------------------------
! RNO3C     Respiration required for reducing NO3 to protein
!             (g [CH2O] / g [protein])

!      CDMTOT = CDMREP + CDMVEG + NDMOLD*RNO3C/0.16

      CDMTOT = CDMVEG + NDMOLD * RNO3C / 0.16 ! Sugarcane 
      
!	END IF
	 
!      GDMSD = GDMSDO

      ENDIF !end VSTAGE


      IF (DYNAMIC == 4) THEN
        SUBTITLE = .TRUE.
      END IF

C-----------------------------------------------------------------------
C    At this point, PGAVL will be used entirely, assuming that N can be
C    made available in the ratio described.
C     Growth Demands : 
C     N-Demands      : NDMVEG, NDMOLD, NDMTOT, NDMNEW
C     C-Demands      : CDMVEG,, CDMVEG, CDMTOT, CNOLD

C***********************************************************************
C***********************************************************************
C     END OF DYNAMIC IF CONSTRUCT
C***********************************************************************
      ENDIF
C-----------------------------------------------------------------------

      SELECT CASE (DYNAMIC)
        CASE (1)
	    SUBTITLE = .TRUE.

	  CASE (2)
          SUBTITLE = .TRUE.
	
        CASE (3)	  
          SUBTITLE = .TRUE.

	  CASE (4)
	    IF (SUBTITLE) THEN        !portability
              SUBTITLE = .FALSE.
          ELSE
            SUBTITLE = .TRUE. 
          END IF
      END SELECT


      CALL PRNT_DEMAND_SC(
     &  DYNAMIC, SUBTITLE, YRDOY, YRSIM,
     &  AGRLF, AGRRT, AGRSTM, AGRSU, 
     &  DLFN, DPLARF, DRPP, 
     &  NSTRES, NVEG0, 
     &  PAR, PCNL, PCNRT, PCNST, PCNSU, PGAVL, PLTPOP, 
     &  RPROAV, 
     &  WTLF, RTWT, STMWT, SUWT, 
     &  SetStalk, SWFAC, 
     &  TAVG, TDUMX, TURFAC, 
     &  VSTAGE, 
     &  WCRLF, WCRRT, WCRST, WCRSU, 
     &  WNRLF, WNRRT, WNRST, WNRSU, 

     &  AGRVG, AGRVG2, 
     &  F, FNINL, FNINR, FNINS, 
     &  FRLF, FRRT, FRSTM, FRSU, 
     &  NDMNEW, NDMOLD, NDMTOT, NDMVEG, NMINEP, NMOBR)

      SUBTITLE = .FALSE.

      RETURN

      END SUBROUTINE CSP_DEMAND

C=======================================================================
C  CSP_IPDMND Subroutine, from IPDMND subroutine, C.H. Porter
C-----------------------------------------------------------------------
C  Reads input data for CSP_DEMAND subroutine
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  07/04/1998 CHP Written.
C  08/12/2003 CHP Added I/O error checking
C  08/19/2003 FSR Changed from Daza's IPDMND_SC for CASUPRO sugarcane 
C-----------------------------------------------------------------------
C  Called by:  CSP_DEMAND
C  Calls:      FIND, ERROR, IGNORE
C=======================================================================

      SUBROUTINE CSP_IPDMND(FILECC, FILEIO,                     !Input
     &  FINREF, FRLFF, FRLFMX, FRSTMF, NMOBMX, NVSMOB, PROLFF,  !Output
     &  PROLFI, PRORTF, PROSUF, PROSUI, PRORTI, PROSTF, PROSTI, !Output
     &  RCH2O, RLIG, RLIP, RMIN, RNO3C, RPRO, ROA, SIZELF,      !Output
     &  SIZREF, SLAMAX, SLAMIN, SLAPAR, SLAREF, SLAVAR, TURSLA, !Output
     &  VSSINK, XLEAF, XSLATM, XVGROW, YLEAF, YSLATM, YSTEM,    !Output
     &  YVREF, XFRRT, YFRRT, XFRSU, YFRSU, GAMMA)               !Output
      
!-----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, ERROR, IGNORE
!-----------------------------------------------------------------------
!      CHARACTER*3   TYPSDT
      CHARACTER*6   ERRKEY
      PARAMETER (ERRKEY = 'DEMAND')
      CHARACTER*6   SECTION
      CHARACTER*6   ECONO !ECOTYP, 
      CHARACTER*30  FILEIO
      CHARACTER*80  C80
      CHARACTER*92  FILECC    !, FILEGC
!      CHARACTER*255 C255

      INTEGER LUNCRP, LUNIO, ERR, LINC, LNUM, FOUND, ISECT    !, LUNECO
      INTEGER I, II

      REAL FINREF, FRLFF, FRLFMX, FRSTMF, NMOBMX, NVSMOB, 
     &  PROLFF, PROLFI, PRORTF, PRORTI, PROSTF, PROSTI, PROSUF, PROSUI, 
     &  RPRO, RCH2O, RLIG, RLIP, RMIN, RNO3C, ROA, SIZELF, SIZREF, 
     &  SLAMAX, SLAMIN, SLAPAR, SLAREF, SLAVAR, TURSLA, VSSINK  !,SLOSUM

      REAL XVGROW(6), YVREF(6)
      REAL XSLATM(10), YSLATM(10)
      REAL XLEAF(25), YLEAF(25), YSTEM(25)

      REAL GAMMA, XFRRT(4), YFRRT(4), XFRSU(4), YFRSU(4) !CASUPRO 

!-----------------------------------------------------------------------
      CALL GETLUN('FILEIO', LUNIO)
      OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
      LNUM = 0
!-----------------------------------------------------------------------
C    Find and Read Field Section from FILEIO - previously read in IPIBS
!       Look for the second section header beginning with '*CULTI'
C-----------------------------------------------------------------------
      SECTION = '*CULTI'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ENDIF
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ(LUNIO,'(24X,A6,66X,2F6.0)',IOSTAT=ERR) 
     &      ECONO, SLAVAR, SIZELF   ;    LNUM = LNUM + 1
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
      ENDIF

      CLOSE (LUNIO)

!-----------------------------------------------------------------------
!     Read in values from input file, which were previously input
!       in Subroutine IPCROP.
!-----------------------------------------------------------------------
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      LNUM = 0
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!-----------------------------------------------------------------------
!    Find and Read Respiration Section
!-----------------------------------------------------------------------
      SECTION = '!*RESP'
      CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(F6.0,6X,F6.0)',IOSTAT=ERR) RNO3C, RPRO
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(5F6.0)',IOSTAT=ERR)RCH2O,RLIP,RLIG,ROA,RMIN
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF

!-----------------------------------------------------------------------
!    Find and Read Plant Composition Section
!-----------------------------------------------------------------------
      SECTION = '!*PLAN'
      CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(F6.0,6X,2F6.0,6X,F6.0)', IOSTAT=ERR)
     &          PROLFI, PROLFF, PROSTI, PROSTF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(F6.0,6X,2F6.0,6X,F6.0)', IOSTAT=ERR) 
     &             PRORTI, PRORTF, PROSUI, PROSUF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

      ENDIF

!-----------------------------------------------------------------------
!    Find and Read Seed Composition Section
!                  Removed for CASUPRO sugarcane model (FSR)
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!    Find and Read Carbon and Nitrogen Mining Section
!-----------------------------------------------------------------------
      SECTION = '!*CARB'
      CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(18X,2F6.0)',IOSTAT=ERR) NMOBMX, NVSMOB
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF

!-----------------------------------------------------------------------
!    Find and Read Vegetative Partitioning Section
!-----------------------------------------------------------------------
      SECTION = '!*VEGE'
      CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(8F6.0)',IOSTAT=ERR)(XLEAF(II),II=1,8)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(8F6.0)',IOSTAT=ERR)(YLEAF(II),II=1,8)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(8F6.0)',IOSTAT=ERR)(YSTEM(II),II=1,8)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(12X,2F6.0)',IOSTAT=ERR) FRSTMF, FRLFF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(F6.0)',IOSTAT=ERR) FRLFMX
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF

!-----------------------------------------------------------------------
!    Find and Read Leaf Growth Section
!-----------------------------------------------------------------------
      SECTION = '!*LEAF'
      CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4F6.0)',IOSTAT=ERR) FINREF, SLAREF, SIZREF, VSSINK
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4F6.0)',IOSTAT=ERR) SLAMAX, SLAMIN, SLAPAR, TURSLA
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(6F6.0)',IOSTAT=ERR)(XVGROW(II),II=1,6)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(6F6.0)',IOSTAT=ERR)(YVREF(II),II=1,6)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(5F6.0)',IOSTAT=ERR)(XSLATM(II),II = 1,5)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(5F6.0)',IOSTAT=ERR)(YSLATM(II),II = 1,5)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF

! New
!-----------------------------------------------------------------------
!    Find and Read Root Section
!-----------------------------------------------------------------------
      SECTION = '!*ROOT'
      CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE

        DO I=1,3
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        ENDDO

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4F6.0)',IOSTAT=ERR) (XFRRT(II),II = 1,4)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4F6.0)',IOSTAT=ERR) (YFRRT(II),II = 1,4)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      END IF 

! New
!-----------------------------------------------------------------------
!    Find and Read Sugars Section
!-----------------------------------------------------------------------
      SECTION = '!*SUGA'
      CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
      
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4F6.0)',IOSTAT=ERR) (XFRSU(II),II = 1,4)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4F6.0)',IOSTAT=ERR) (YFRSU(II),II = 1,4)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(F6.0)',IOSTAT=ERR) GAMMA
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      END IF

!-----------------------------------------------------------------------
!    Find and Read Seed and Shell Growth Section
!                  Removed for CASUPRO sugarcane model (FSR)
!-----------------------------------------------------------------------
      CLOSE(LUNCRP)

C-----------------------------------------------------------------------
C    Read Ecotype Parameter File
!                  Removed for CASUPRO sugarcane model (FSR)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!FSR - The de-bugging routines below were not included in this build.  
!      If they are needed, they will need to be adjusted to the DSSAT
!      ver 4.0 variables.  Other files needed will be CSP_IPDMND_OUT.for
!      and CSP_IPPLNT_OUT.for.  
!-----------------------------------------------------------------------

! Echoes input data

!    CALL IPDMND_SC_OUT(
!    &  ECONO, FILECC, FILEGC, FILEIO,                          !Input
!    &  FINREF, FRLFF, FRLFMX, FRSTMF, NMOBMX, NVSMOB, PROLFF,  !Input
!    &  PROLFI, PRORTF, PRORTI, PROSTF, PROSTI, PROSUF, PROSUI, !Input 
!    &  RCH2O, RLIG, RLIP, RMIN, RNO3C, RPRO, ROA, SLAMAX,      !Input
!    &  SLAMIN, SLAPAR, SLAREF, SLAVAR, SIZELF, SIZREF, TURSLA, !Input
!    &  VSSINK, XLEAF, XSLATM, XVGROW, YLEAF, YSLATM, YSTEM,    !Input
!    &  YVREF, XFRRT, YFRRT, XFRSU, YFRSU, GAMMA)               !Input

!-----------------------------------------------------------------------
      RETURN
!-----------------------------------------------------------------------
      END  ! SUBROUTINE CSP_IPDMND
!-----------------------------------------------------------------------


       SUBROUTINE PRNT_DEMAND_SC(
     &  DYNAMIC, SUBTITLE, YRDOY, YRSIM,
     &  AGRLF, AGRRT, AGRSTM, AGRSU, 
     &  DLFN, DPLARF, DRPP, 
     &  NSTRES, NVEG0, 
     &  PAR, PCNL, PCNRT, PCNST, PCNSU, PGAVL, PLTPOP, 
     &  RPROAV, 
     &  WTLF, RTWT, STMWT, SUWT, 
     &  SetStalk,  SWFAC, 
     &  TAVG, TDUMX, TURFAC, 
     &  VSTAGE, 
     &  WCRLF, WCRRT, WCRST, WCRSU, 
     &  WNRLF, WNRRT, WNRST, WNRSU, 

     &  AGRVG, AGRVG2, 
     &  F, FNINL, FNINR, FNINS, 
     &  FRLF, FRRT, FRSTM, FRSU, 
     &  NDMNEW, NDMOLD, NDMTOT, NDMVEG, NMINEP, NMOBR)

      IMPLICIT NONE

      INTEGER DYNAMIC, RUNINIT, SEASINIT, EMERG, INTEGR
      INTEGER OpenStatus
      INTEGER YRDOY, YRSIM, NVEG0, SetStalk!
      PARAMETER (RUNINIT = 1, SEASINIT = 2, EMERG = 3, INTEGR = 4)

      REAL AGRLF, AGRRT, AGRSTM, AGRSU
      REAL DLFN, DPLARF, DRPP
      REAL NSTRES
	
      REAL PAR, PCNL, PCNRT, PCNST, PCNSU, PGAVL, PLTPOP
      REAL RPROAV
      REAL WTLF, RTWT, STMWT, SUWT
      REAL SWFAC
      REAL TAVG, TDUMX, TURFAC
      REAL VSTAGE
      REAL WCRLF, WCRRT, WCRST, WCRSU
      REAL WNRLF, WNRRT, WNRST, WNRSU

      REAL AGRVG, AGRVG2
      REAL F, FNINL, FNINR, FNINS
      REAL FRLF, FRRT, FRSTM, FRSU
      REAL NDMNEW, NDMOLD, NDMTOT, NDMVEG, NMINEP, NMOBR

      LOGICAL SUBTITLE

      SELECT CASE (DYNAMIC)
        CASE (1)

      OPEN(UNIT = 400, FILE = "TestDemand_SC.out", STATUS = "UNKNOWN", 
     & ACTION = "WRITE", POSITION = "REWIND", IOSTAT = OpenStatus)

      WRITE(400,'(1X,"RESULTS FROM DEMAND_SC.for")')
      WRITE(400,'(1X, " ")')    !chp for portability
      WRITE(400,'(1X,"  YRDOY   YRSIM")', ADVANCE="NO") 
      WRITE(400,'(1X,"AGRLF AGRRT AGRSTM AGRSU")', ADVANCE="NO") 
      WRITE(400,'(1X," DLFN   DPLARF  DRPP")', ADVANCE="NO") 
      WRITE(400,'(1X,"NSTRES NVEG0")', ADVANCE="NO") 
      WRITE(400,'(1X,"  PAR  PCNL PCNRT PCNST PCNSU  PGAVL PLTPOP")', 
     &  ADVANCE="NO") 
      WRITE(400,'(1X,"RPROAV")', ADVANCE="NO") 
      WRITE(400,'(1X,"   WTLF    RTWT   STMWT    SUWT")', ADVANCE="NO") 
      WRITE(400,'(1X,"SetStalk SWFAC")', ADVANCE="NO") 
      WRITE(400,'(1X," TAVG TDUMX TURFAC")', ADVANCE="NO") 
      WRITE(400,'(1X,"VSTAGE")', ADVANCE="NO") 
      WRITE(400,'(1X,"WCRLF WCRRT WCRST WCRSU")', ADVANCE="NO") 
      WRITE(400,'(1X,"WNRLF WNRRT WNRST WNRSU")', ADVANCE="NO") 
      WRITE(400,'(1X,"AGRVG AGRVG2")', ADVANCE="NO") 
      WRITE(400,'(1X,"     F FNINL FNINR FNINS")', ADVANCE="NO") 
      WRITE(400,'(1X," FRLF  FRRT FRSTM  FRSU")', ADVANCE="NO") 
      WRITE(400,'(1X,"NDMNEW NDMOLD NDMTOT NDMVEG NMINEP NMOBR")') 
      WRITE(400,'(1X,"RUNINIT")')
	
        CASE (2)
          WRITE(400,'(1X,"SEASINIT")')
	
        CASE (3)	  
          WRITE(400,'(1X,"EMERG")')

        CASE (4)
      IF (SUBTITLE) THEN   !chp for portability
	  WRITE(400,'(1X,"INTEGR")')
             SUBTITLE = .FALSE.
          ELSE
            SUBTITLE = .TRUE. 
          END IF
      END SELECT

      IF (SUBTITLE) THEN
      WRITE(400,'(2(1X,I7))', ADVANCE = "NO") YRDOY, YRSIM
      WRITE(400,'(2(1X,F5.2),1X,F6.2,1X,F5.2)', ADVANCE = "NO") 
     &  AGRLF, AGRRT, AGRSTM, AGRSU
      WRITE(400,'(1X,F5.2,1X,F8.2,1X,F5.2)', ADVANCE = "NO") 
     &  DLFN, DPLARF, DRPP
      WRITE(400,'(1X,F6.2,1X,I5)', ADVANCE = "NO") 
     &  NSTRES, NVEG0
      WRITE(400,'(5(1X,F5.2),1X,F6.2,1X,F6.2)', ADVANCE = "NO") 
     &  PAR, PCNL, PCNRT, PCNST, PCNSU, PGAVL, PLTPOP
      WRITE(400,'(1X,F6.2)', ADVANCE = "NO") 
     &  RPROAV
      WRITE(400,'(4(1X,F7.2))', ADVANCE = "NO") 
     &  WTLF, RTWT, STMWT, SUWT
      WRITE(400,'(1X,I8,1X,F5.2)', ADVANCE = "NO") 
     &  SetStalk, SWFAC
      WRITE(400,'(2(1X,F5.2),1X,F6.2)', ADVANCE = "NO") 
     &  TAVG, TDUMX, TURFAC
      WRITE(400,'(1X,F6.2)', ADVANCE = "NO") 
     &  VSTAGE
      WRITE(400,'(4(1X,F5.2))', ADVANCE = "NO") 
     &  WCRLF, WCRRT, WCRST, WCRSU
      WRITE(400,'(4(1X,F5.2))', ADVANCE = "NO") 
     &  WNRLF, WNRRT, WNRST, WNRSU
      WRITE(400,'(1X,F5.2,1X,F6.2)', ADVANCE = "NO") 
     &  AGRVG, AGRVG2
      WRITE(400,'(1X,F6.2,3(1X,F5.2))', ADVANCE = "NO") 
     &  F, FNINL, FNINR, FNINS
      WRITE(400,'(4(1X,F5.2))', ADVANCE = "NO") 
     &  FRLF, FRRT, FRSTM, FRSU
      WRITE(400,'(5(1X,F6.2),1X,F5.2)') 
     &  NDMNEW, NDMOLD, NDMTOT, NDMVEG, NMINEP, NMOBR
      
      END IF

      RETURN

      END  ! SUBROUTINE PRNT_DEMAND_SC


! Open file to write results from DEMAND_SC.for

!FORMAT

!YRDOY YRSIM  2(1X,I5)
!AGRLF AGRRT AGRSTM AGRSU 2(1X,F5.2),1X,F6.2,1X,F5.2
! DLFN DPLARF DRPP 1X,F5.2,1X,F6.2,1X,F5.2
!NSTRES NVEG0 1X,F6.2,1X,F5.2
!  PAR  PCNL PCNRT PCNST PCNSU PGAVL PLTPOP 6(1X,F5.2),1X,F6.2
!RPROAV 1X,F6.2
! WTLF  RTWT STMWT  SUWT  4(1X,F5.2)
!SetStalk SWFAC  1X,I9,1X,F5.2
! TAVG TDUMX TURFAC 2(1X,F5.2),1X,F6.2,
!VSTAGE  1X,F5.2
!WCRLF WCRRT WCRST WCRSU 4(1X,F5.2)
!WNRLF WNRRT WNRST WNRSU 4(1X,F5.2)

!AGRVG AGRVG2 1X,F5.2,1X,F6.2
!    F FNINL FNINR FNINS 4(1X,F5.2)
! FRLF  FRRT FRSTM  FRSU 4(1X,F5.2)
!NDMNEW NDMOLD NDMTOT NDMVEG NMINEP NMOBR 6(1X,F6.2),1X,F5.2


!YRDOY, YRSIM,
!AGRLF, AGRRT, AGRSTM, AGRSU, 
!DLFN, DPLARF, DRPP, 
!NSTRES, NVEG0, 
!PAR, PCNL, PCNRT, PCNST, PCNSU, PGAVL, PLTPOP, 
!RPROAV, 
!WTLF, RTWT, STMWT, SUWT, 
!SetStalk,  SWFAC, 
!TAVG, TDUMX, TURFAC, 
!VSTAGE, 
!WCRLF, WCRRT, WCRST, WCRSU, 
!WNRLF, WNRRT, WNRST, WNRSU, 

!AGRVG, AGRVG2, 
!F, FNINL, FNINR, FNINS, 
!FRLF, FRRT, FRSTM, FRSU, 
!NDMNEW, NDMOLD, NDMTOT, NDMVEG, NMINEP,  


!=======================================================================
!       Variable definitions for CSP_DEMAND and CSP_IPDMND
!-----------------------------------------------------------------------
! AGRLF     Mass of CH2O required for new leaf growth (g [CH2O] / g [leaf])
! AGRRT     Mass of CH2O required for new root growth (g [CH2O] / g [root])
! AGRSTM    Mass of CH2O required for new stem growth (g [CH2O] / g [stem])
! AGRVG     Mass of CH2O required for vegetative tissue growth including 
!             stoichiometry and respiration (g [CH2O] / g [tissue])
! AGRVG2    Total mass of CH2O required for vegetative tissue growth
!             (g [CH2O] / g [tissue])
! CDMTOT    Total CH2O demand (g [CH2O] / m2 / d)
! CDMVEG    Carbon demand for vegetative growth (g [CH2O] / m2 / d)
! CNOLD     Available CH2O after reproductive growth (g [CH2O] / m2 / d)
! CROP      Crop identification code 
! DAS       Days after start of simulation (days)
! DRPP      Photoperiod days which occur in a real day
!             (photoperiod days / day)
! ECONO     Ecotype code - used to match ECOTYP in .ECO file 
! ECOTYP    Ecotype code for this simulation 
! F         Specific leaf area of new leaf tissue growth, including N
!             (cm2 [leaf] / g [leaf])
! FFVEG     Specific leaf area of new leaf tissue growth (interim value)
!             (cm2 [leaf] / g [leaf])
! FILECC    Path plus filename for species file (*.spe) 
! FILEGC    Pathname plus filename for ECO file 
! FILEIO    Filename for INP file (e.g., IBSNAT35.INP) 
! FINREF    Specific leaf area (SLA) of leaves of standard crop cultivar 
!             when plants emerge (cm2 [leaf] / g [leaf])
! FNINL     Maximum fraction of N for growing leaf tissue (g [N] / g [leaf])
! FNINR     Maximum fraction of N for growing root tissue (g [N] / g [root])
! FNINS     Maximum fraction of N for growing stem tissue (g [N] / g [stem])
! FRLF      Fraction of vegetative tissue growth that goes to leaves on a 
!             day (g [leaf] / g [veg])
! FRLFF     Fraction of daily increase in vegetative weight which goes to 
!             leaves after the day on which the maximum number of V-stages 
!             occurs (NDVSTG). (g [leaf] / g [veg])
! FRLFMX    Maximum leaf partitioning (g [leaf] / g [veg])
! FRRT      Fraction of vegetative tissue growth that goes to roots on a 
!             day (g [root] / g [veg])
! FRSTM     Fraction of vegetative tissue growth that goes to stems on a 
!             day (g [stem] / g [veg])
! FRSTMF    Fraction of daily dry weight increase in vegetative plant parts 
!             which goes to stems after the day on which the maximum number 
!             of V-stages occurs (NDVSTG). (g [stem] / g [veg])
! FVEG      Specific leaf area prior to computing effects of temperature, 
!             PAR, water stress (cm2 [leaf] / g [leaf])
! GAINNW    Leaf area added (prior to VSSINK) (cm2 [leaf] / m2 [ground])
! GAINWT    Leaf weight added (prior to VSSINK and after NDLEAF)
!             (g [leaf] / m2 [ground])
! GROMAX    Maximum leaf area which can be added per plant between 
!             emergence and day of simulation as a function of V-stage on 
!             day of simulation (cm2 [leaf] / plant)
! GROYES    Maximum leaf area which could have been added per plant between 
!             emergence and yesterday as a function of V-stage
!             (cm2 [leaf] / plant)
! LUNCRP    Logical unit number for FILEC (*.spe file) 
! LUNECO    Logical unit number for FILEE (*.eco file) 
! LUNIO     Logical unit number for FILEIO 
! NDMNEW    Total N demand for new growth (g [N] / m2 / d)
! NDMOLD    N demand for old tissue (g [N] / m2 / d)
! NDMTOT    Total N demand (g [N] / m2 / d)
! NDMVEG    N required for vegetative growth if all PGAVL is used as 
!             computed (g [N] / m2 / d)
! NMINEP    Potential N mobilization from storage (g [N] / m2 / d)
! NMOBMX    Maximum fraction of N which can be mobilized in a day 
! NMOBR     Stage-dependent potential N mining rate expressed as a 
!             fraction of the maximum rate (NMOBMX)
! NSTRES    Nitrogen stress factor (1=no stress, 0=max stress) 
! NVEG0     Day of emergence (days)
! NVSMOB    Relative rate of N mining during vegetative stage to that in 
!             reproductive stage 
! NVSTL     N content in leaves (fraction)
! NVSTR     N content in roots (fraction)
! NVSTS     N content in stems (fraction)
! PAR       Daily photosynthetically active radiation or photon flux 
!             density (moles [quanta]/m2-d)
! PARSLA    Effect of PAR on specific leaf area 
! PCNL      Percentage of N in leaf tissue (100 g [N] / g [leaf])
! PCNRT     Percent N in root tissue (100 g [N] / g [root])
! PCNST     Percent N in stem tissue (100 g [N] / g [stem])
! PGAVL     Total available CH2O available for growth & respiration
!             (g [CH2O] / m2)
! PLTPOP    Plant population (# plants / m2)
! PROLFF    Minimum leaf protein composition after N mining
!             ( g [protein] / g [leaf])
! PROLFI    Maximum protein composition in leaves during growth with 
!             luxurious supply of N (g [protein] / g [leaf tissue])
! PRORTF    Minimum root protein composition after N mining
!             ( g [protein] / g [root])
! PRORTI    Maximum protein composition in roots during growth with 
!             luxurious supply of N (g [protein] / g [root])
! PROSTF    Minimum stem protein composition after N mining
!             (g [protein] / g [stem])
! PROSTI    Maximum protein composition in stems during growth with 
!             luxurious supply of N (g [protein] / g [stem])
! RCH2O     Respiration required for synthesizing CH2O structure
!             (g [CH2O] / g [tissue])
! RLIG      Respiration required for synthesizing lignin structure
!             (g [CH2O] / g [lignin])
! RLIP      Respiration required for synthesizing lipid structure
!             (g [CH2O] / g [lipid])
! RMIN      Respiration required for synthesizing mineral structure
!             (g [CH2O] / g [mineral])
! RNO3C     Respiration required for reducing NO3 to protein
!             (g [CH2O] / g [protein])
! ROA       Respiration required for synthesizing organic acids
!             (g [CH2O] / g [product])
! RPRO      Respiration required for re-synthesizing protein from mobilized 
!             N (g[CH2O] / g[protein])
! RPROAV    Respiration required for protein synthesis, average based on 
!             sources of N (g [CH2O] / g [protein])
! RTWT      Dry mass of root tissue, including C and N
!             (g [root] / m2 [ground])
! SIZELF    The size of a normal upper node leaf (nodes 8 - 10) used to 
!             adjust leaf area expansion during sink-limited phase of 
!             vegetative growth, i.e., prior to VSSINK nodes on the main stem
!             (cm2/leaf)
! SIZRAT    Ratio of upper node normal leaf size for given variety to that 
!             for standard cultivar, used to adjust table of maximum leaf 
!             area vs. V-stage 
! SIZREF    The size of a normal upper node  leaf (nodes 8 - 10) of 
!             standard cultivar. (cm2 / leaf)
! SLAMAX    The maximum specific leaf area (SLA) for new leaves when grown 
!             under low (nearly zero) radiation but optimum water and 
!             temperature for the standard cultivar. (cm2 / g)
! SLAMIN    The minimum specific leaf area (SLA) for new leaves when grown 
!             under infinitely high radiation, optimum water and 
!             temperature for the standard cultivar. (cm2 / g)
! SLAMN     Minimum specific leaf area for new leaves when grown under high 
!             radiation and optimum water and temperature conditions (cm2 / g)
! SLAMX     Maximum specific leaf area for new leaves when grown under low 
!             radiation, but optimum water and temperature conditions
!             (cm2 / g)
! SLAPAR    Coefficient in exponential equation to reduce SLA as PAR 
!             increases (leaf curvature) 
! SLAREF    Specific leaf area (SLA) for new leaves during peak vegetative 
!             growth for the standard cultivar. (cm2/g)
! SLAVAR    Specific leaf area (SLA) for new leaves during peak vegetative 
!             growth for cultivar I, modified by environmental factor (cm2/g)
! STMWT     Dry mass of stem tissue, including C and N
!             (g [stem] / m2 [ground)
! SUMTEM    Factor which affects protein composition based on average 
!             temperature. 
! SWFAC     Effect of soil-water stress on photosynthesis, 1.0=no stress, 
!             0.0=max stress 
! TAVG      Average daily temperature (°C)
! TDUMX     Photo-thermal time that occurs in a real day based on early 
!             reproductive development temperature function
!             (photo-thermal days / day)
! TPHFAC    Reduction in specific leaf area due to daytime temperature 
!             being less than optimal (0-1) 
! TURADD    Water stress factor (TURFAC) effect on reproductive growth and 
!             pod addition.  Stress is defined to INCREASE growth and 
!             addition. 
! TURFAC    Water stress factor for expansion (0 - 1) 
! TURFSL    Factor which applies water stress to specific leaf area of new 
!             leaf tissue growth 
! TURSLA    Water stress effects on leaf area expansion 
! VSSINK    Vegetative stage beyond which sink-limited leaf area expansion 
!             can no longer limit photosynthesis or leaf area growth. 
! VSTAGE    Number of nodes on main stem of plant 
! WCRLF     Mass of CH2O reserves in leaves (g [leaf CH2O] / m2 [ground])
! WCRRT     Mass of CH2O reserves in roots (g [root CH2O] / m2 [ground])
! WCRST     Mass of CH2O reserves in stems (g [stem CH2O] / m2 [ground])
! WNRLF     N available for mobilization from leaves above lower limit of 
!             mining (g [N] / m2)
! WNRRT     N available for mobilization from roots above lower limit of 
!             mining (g [N] / m2)
! WNRST     N available for mobilization from stems above lower limit of 
!             mining (g [N] / m2)
! WTLF      Dry mass of leaf tissue including C and N
!             (g [leaf] / m2 [ground])
! XLEAF(I)  V-stage at which partitioning to leaves is YLEAF(I).
!             (leaf nodes)
! XSLATM(I) Temperature values for function that reduces specific leaf area 
!             (SLA) (°C)
! XVGROW(I) V-stage at which maximum leaf area growth per plant since 
!             emergence is YVGROW(I). (# leaf nodes)
! YLEAF(I)  Partitioning fraction to leaves at V-stage XLEAF(I)
!             ( g [leaf] / g [veg. plant])
! YRDOY     Current day of simulation (YYDDD)
! YRSIM     Start of simulation date (YYDDD)
! YSLATM(I) Array which describes the effect of temperature on specific 
!             leaf area 
! YSTEM(I)  Partitioning factor for stem growth at V-stage XSTEM(I)
!             (g [stem] / g [veg. plant])
! YVGROW(I) Maximum leaf area grown per plant at V-stage XVGROW(I)
!             (cm2 / plant)
! YVREF(I)  Maximum leaf area grown per plant at V-stage XVGROW(I), for 
!             reference cultivar. (cm2 / plant)

! LIST OF NEW VARIABLES ADDED TO ENABLE SIMULATION OF SUGARS IN THE 
! SUGARCANE MODEL

! AGRSU    Mass of CH2O required for new sugar growth (g [CH2O] / g [sugar])
! ALPHSU   Fraction of new sugars growth that is mobile C (fraction)
! CPFSU    Respiration requirement for net sugars growth
!            (g [CH20] / g [tissue]) +++
! CRUSSU   C mobilized from sugars in a day (g [CH2O] / m2 - d)
! CSUW     Cumulative leaf growth (g [leaf] / m2)
! FNINSU   Maximum fraction of N for growing sugars (g [N] / g [sugar])
! FRSU     Fraction of vegetative tissue growth that goes to sugars on a 
!            day (g [root] / g [veg])
! GAMMA    Partitioning of excess carbon between stalks and sugars
! NADSU    N added to sugars N reserves (g [N] / m2 - d)
! NGRSU    Maximum N demand for sugars growth (g [sugars N] / m2 [ground] - d)
! NRUSSU   N actually mobilized from sugars in a day (g [N] / m2 - d)
! NSUDOT   Net N addition for sugars (g [N] / m2 [ground] - d)
! NSUOFF   N loss from sugars in a day (g [N]/m2 - d)
! NSUALL   N added to sugars today (g [N] / m2 - d)
! NVSTSU   N content in sugars (fraction)
! PCNSU    Percentage of N in sugars (100 g [N] / g [sugars]) +++
! PCARSU   Proportion of sugars that is carbohydrate (fraction)
! PLIGSU   Proportion of sugars that is lignin (fraction)
! PLIPSU   Proportion of sugars that is lipid (fraction)
! PMINSU   Proportion of sugars that is mineral (fraction)
! POASU    Proportion of sugars that is organic acid (fraction)
! PROSUF   Minimum sugars protein composition after N mining
!            (g [protein] / g [sugars])
! PROSUI    Maximum protein composition in sugars during growth with 
!             luxurious supply of N (g [protein] / g [sugars])
! RHOSU     Fraction of sugars which is carbohydrate (g [CH20] / g[sugars])
! SUWT      Dry mass of sugars, including C and N
!             (g [sugars] / m2 [ground])
! WCRSU     Mass of CH2O reserves in sugars (g [sugars CH2O] / m2 [ground])
! WNRSU     N available for mobilization from sugars above lower limit of 
!             mining (g [N] / m2)
! WRCSUDT   Net C addition for sugars (g [CH2O] / m2 - d)
! WSUDOT    Net sugars growth rate (g [sugars] / m2 - d)
! WSUDOTN   Dry weight growth rate of new sugars including N but not C 
!             reserves (g [sugars] / m2 [ground] - d)
! WSUI      Initial weight of sugars (g [leaf] / m2)
! WTNSU     Mass of N in sugars (g [sugars N] / m2 [ground])
! WTNSUA    Cumulative N added to sugars (g [N] / m2 - d)
! WTNSUO    Cumulative N loss from sugars (g [N] / m2)
! WTSUO     Cumulative sugar losses (g [sugars] / m2)
!-----------------------------------------------------------------------
!       END SUBROUTINE CSP_DEMAND
!=======================================================================
