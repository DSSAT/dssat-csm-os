C=======================================================================
C  RI_GROSUB, Subroutine
C
C  Determines rice growth
C-----------------------------------------------------------------------
C  Revision history
C
C  08/07/1993 PWW Header revision and minor changes
C                 Water stress effect on leaf appearance
C  08/29/2002 CHP/MUS Converted to modular format for inclusion in CSM.
C  02/19/2003 CHP Converted dates to YRDOY format
C  04/02/2008 US/CHP Added P and K models
C  02/25/2012 JZW PHINT from CUL file (remove from SPE)
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  FIELD  : Switch to indicate field simulation of all processes but
C           when false then growth without stress as in seedbed
C  LTRANS : True if transplanted (itrans=2 or 3)
C=======================================================================

      SUBROUTINE RI_GROSUB (CONTROL, ISWITCH,
     &    CO2, CDTT_TP, CUMDTT, DTT, FERTILE, FIELD,      !Input
     &    FLOOD, FracRts, ISTAGE, ITRANS, LTRANS,         !Input
     &    NEW_PHASE, NH4, NO3, P1, P1T, P3, P4, PHEFAC,   !Input     
     &    RLV, RTDEP, SDEPTH, SDTT_TP, SeedFrac,          !Input
     &    SI3, SOILPROP, SKi_AVAIL, SPi_AVAIL, SRAD, ST,  !Input
     &    STRCOLD, STRESSW, STRHEAT, SUMDTT, SW, SWFAC,   !Input
     &    TAGE, TBASE, TF_GRO, TMAX, TMIN, TSGRWT,        !Input
     &    TURFAC, VegFrac, WSTRES, XSTAGE, XST_TP, YRPLT, !Input
     &    YRSOW,                                          !Input
     &    EMAT, FLOODN, PLANTS, RTWT,                     !I/O
     &    AGEFAC, APTNUP, BIOMAS, CANNAA, CANWAA, DYIELD, !Output
     &    GNUP, GPP, GPSM, GRAINN, GRNWT, GRORT,          !Output
     &    KUptake, KSTRES, LAI,                           !Output
     &    LEAFNO, LFWT, MAXLAI, NSTRES, PANWT, PBIOMS,    !Output
     &    PHINT, PLTPOP, PConc_Root, PConc_Seed,          !Output
     &    PConc_Shel, PConc_Shut, PORMIN, PSTRES1,        !Output
     &    PSTRES2, PUptake, RLWR, ROOTN, RTWTO, RWUEP1,   !Output
     &    RWUMX, SEEDNI, SEEDRV, SENESCE,                 !Output
     &    SKERWT, STMWT, STMWTO,                          !Output
     &    STOVER, STOVN, TANC, TGROGRN, TILNO, TOTNUP,    !Output
     &    CumNUptake, UNH4, UNO3, WTLF, XGNP)             !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
      USE FloodModule    ! parameters, hourly weather data.

      USE Interface_SenLig_Ceres

      IMPLICIT  NONE
      EXTERNAL YR_DOY, RI_IPGROSUB, RI_IPCROP, TABEX, ERROR,
     &  RI_NFACTO, CALCSHK, RI_TILLSUB, RI_NUPTAK, RI_KUPTAK, 
     &  P_Ceres, PlantInit, TRNSPL_GROSUB, MZ_KUPTAK
      SAVE

      CHARACTER*1 ISWWAT, ISWNIT, ISWPHO, ISWPOT
      CHARACTER*2 CROP
      CHARACTER*12 FILEC      
	CHARACTER*92 FILECC

      CHARACTER*6     ERRKEY          
      PARAMETER       (ERRKEY='RI_GRO')   

      CHARACTER*80 PATHCR
      INTEGER   I,YEAR

      REAL      SWFAC,TURFAC,PHEFAC
      REAL      NSINK,NPOOL1,NPOOL2,NPOOL,NSDR,NSINKT,TEMF
      REAL      TL,EPLANTS,RUEA,RUEX,TTMP,PC,TI,BF,XTN,A
      REAL      SLFW,SLFT,SLFN,XPLAG1,GRF,TCARB1,TILCAR,ADDTIL,SHORAT
      REAL      TPART,CHK,GROGRN,GNO,GROSTM,GROLF,STMGF,RGNFIL,RMNC
      REAL      XNF,TNLAB,RNLAB,RNOUT,GNC,SLFC,PLAS,BIOMAX,YLFWT
      REAL      TABEX,PCO2,Y1,Y2,PLSC,YSTOVWT,YRTWT

      INTEGER DOY, DYNAMIC, EMAT, MDATE
      INTEGER ISTAGE, ITRANS
      INTEGER L, LEAFNO, MODELVER, NLAYR, YRDOY, YRPLT, YRSOW

      REAL ABIOMS, AGEFAC, ANTSTM, APTNUP, ATEMP, BIOMAS
      REAL CANNAA, CANWAA, CARBO, CO2, CDTT_TP, CUMDEP, CUMDTT, CUMPH
      REAL DTT, DYIELD, FERTILE, FLOOD
      REAL G1, G1FAC, G2, G3, GNP, GNUP, GPP, GPSM, GRAIN
      REAL GRAINN, GRNWT, GRORT, GSIZE, LAI, LFWT
      REAL MAXLAI, MFILL, MGPP, MGROLF, MGROSTM, MLFWT, MPLA, MPLAG
      REAL MSTMWT, NDEF3, NFAC, NPPH, NSTRES, P1, P1T, P3, P4, P5
      REAL PANFAC, PANIWT, PANWT, PAR, PBIOMS, PCARB, PDWI, PGRORT
      REAL PHINT, PLA, PLANTS, PLPH, PLTPOP, PORMIN, PPANIWT, PRFT, PTF
      REAL RANC, RCNP, RESERVE, RGFILL, RLWR
      REAL ROOTN, ROWSPC, RTDEP, RTR, RTWT, RTWTO, RWUEP1, RWUMX
      REAL SDEPTH, SDTT_TP, SDWTPL, SEEDNI  
      REAL SEEDRV, SENLA, SHOCKFAC, SKERWT, SLAN, SNLFWT 
      REAL SPACE, SPIKE, SRAD, STMWT, STMWTO, STOVER, STOVN 
      REAL STOVNMIN, STOVWT, STRCOLD, STRESSW, STRHEAT, SUMDTT, SWMIN
      REAL TAGE, TANC, TBASE, TCARBO, TCNP, TEMPM, TFILL, TGPP
      REAL TGROGRN, TGROLF, TGROSTM, TGRNWT, TPANIWT, TPLA
      REAL TILNO, TILRAT, TLFWT, TMAX, TMIN, TMNC, TMPFIL, TOTNUP
      REAL TPLAG, TPLANTS, TRLOS, TRNLOS, TSGRWT, TSHOCK, TSTMWT
      REAL UNFILL, VANC, VMNC, WSTRES, WTLF, PCNVEG, CumNUptake
      REAL XANC, XGNP, XN, XSTAGE, XST_TP
!     REAL HARVFRAC(2)

      REAL, DIMENSION(6) :: SI3
      REAL TMFAC1(8)
      REAL  CO2X(10), CO2Y(10)
      REAL, DIMENSION(NL) :: DLAYR, DS, RLV, RNLOSS, NH4, NO3
      REAL, DIMENSION(NL) :: ST, SW, UNH4, UNO3
      REAL, DIMENSION(NL) :: FracRts, SPi_AVAIL, PUptake
      REAL, DIMENSION(NL) :: SKi_AVAIL, Kuptake
	REAL  Kstres, FSLFK, SLFK
      REAL  FSLFP, PStres1
      REAL  PStres2, SeedFrac, VegFrac, SLFP      
      REAL  PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed
      REAL        CumLeafSenes    !today's cumul. leaf senescence
      REAL        CumLeafSenesY   !yesterday's cumul. leaf senescence
      REAL        CumLfNSenes     !cumul. N loss in senesced leaves
      REAL  PODWT, SDWT, SWIDOT, STEM2EAR, WLIDOT, WRIDOT, WSIDOT
!     Proportion of lignin in STOVER and Roots
      REAL PLIGLF, PLIGRT
      REAL SLPF

      LOGICAL FIELD, LTRANS, NEW_PHASE, TF_GRO, FIRST

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL
 
	TYPE (ResidueType) SENESCE 

!     The variable "ISWITCH" is of type "SwitchType".
      TYPE (SwitchType) ISWITCH

!     The variable "SOILPROP" is of type "SoilType".
      TYPE (SoilType) SOILPROP
      TYPE (FloodNType) FLOODN

!     Transfer values from constructed data types into local variables.
      CROP    = CONTROL % CROP
      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY

      ISWWAT = ISWITCH % ISWWAT
      ISWNIT = ISWITCH % ISWNIT
      ISWPHO = ISWITCH % ISWPHO
      ISWPOT = ISWITCH % ISWPOT

      DLAYR = SOILPROP % DLAYR
      NLAYR = SOILPROP % NLAYR
      SLPF  = SOILPROP % SLPF

      CALL YR_DOY(YRDOY, YEAR, DOY)

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      CALL RI_IPGROSUB (CONTROL, 
     &    ATEMP, CROP, FILEC, G1, G2, G3, P5, PHINT, PATHCR, 
     &    PLANTS, PLPH, PLTPOP, ROWSPC, SDWTPL)

      CALL RI_IPCROP (FILEC, PATHCR, !CROP, 
     &    CO2X, CO2Y, MODELVER, PORMIN, 
        !&    CO2X, CO2Y, MODELVER, PHINT, PORMIN,  
     &    RLWR, RWUEP1, RWUMX, SHOCKFAC)

      FILECC   = TRIM(PATHCR) // FILEC
      FSLFP    = 0.050  
	! FRACTION OF LEAF SENESECED DUE TO 100% P STRESS /DAY
      FSLFK    = 0.050   
	! FRACTION OF LEAF SENESECED DUE TO 100% K STRESS /DAY

      STMWT    = 0.001
      LEAFNO   = 0
      SEEDRV   = G2
      RANC     = 0.022
      SEEDNI   = 0.0
      GPP    = 1.0
      GRNWT  = 0.0
      GSIZE  = 1.0

!      LAI      = 0.01
      LAI      = 0.0  !chp 8/12/2003 - prevents early N stress
      MAXLAI   = 0.0
      PTF      = 0.0
      PLA      = 0.5 
      MPLA     = 0.5
      MPLAG    = 0.0
      TPLAG    = 0.0
      SLAN     = 0.0    
      SNLFWT   = 0.0    
      LFWT     = 0.001  
      MLFWT    = 0.001  
      TLFWT    = 0.0    
      RTWT     = 0.001  
      STMWT    = 0.001  
      MSTMWT   = 0.001  
      TSTMWT   = 0.001  
      PANWT    = 0.0    
      PANIWT   = 0.0    
      TGRNWT   = 0.0
      STOVWT   = LFWT + STMWT + PANWT 
      BIOMAS   = STOVWT 
      MGROSTM  = 0.0 
      TGROSTM  = 0.0 
      TGROLF   = 0.0 
      MGROLF   = 0.0 
      SENLA    = 0.0 
      GRORT    = 0.0 
      TGROGRN  = 0.0     
      TCARBO   = 0.0     
      CUMPH    = 0.514   
      GRAINN   = 0.0 

      DYIELD   = 0.0 
      STOVER   = 0.0 
      SEEDNI   = 0.0 
      CANNAA   = 0.0 
      CANWAA   = 0.0 
      TOTNUP   = 0.0 
      GNUP     = 0.0 
      APTNUP   = 0.0 
      XGNP     = 0.0 

      GPSM     = 0.0
      TCNP     = 0.0
      NSINKT   = 0.0

      WTLF = LFWT * PLTPOP      !Leaf weight, g/m2
      STMWTO = STMWT * PLTPOP   !Stem weight, g/m2
      RTWTO = RTWT * PLTPOP     !Root weight, g/m2

      !** Initialize variables

      CALL SenLig_Ceres(PLIGLF=PLIGLF, PLIGRT=PLIGRT)
 
      CumLeafSenes = 0.0
      CumLeafSenesY = 0.0
      CumLfNSenes = 0.0
      SENESCE % ResWt  = 0.0
      SENESCE % ResLig = 0.0
      SENESCE % ResE   = 0.0


      CALL RI_NFACTO(DYNAMIC, FIELD, XSTAGE, 
     &    AGEFAC, NDEF3, NFAC, NSTRES, RCNP, TANC, TCNP, TMNC)  !Output

      IF (ISWNIT .EQ. 'Y') THEN
        TANC  = 0.044
        ROOTN = RANC * RTWT
        STOVN = STOVWT * TANC
      ELSE
        TANC  = 0.0
        STOVN = 0.0
        ROOTN = 0.0
        XANC  = 0.0
      ENDIF


      DO I = 1, 8
         TMFAC1(I) = 0.931+0.114*I-0.0703*I**2+0.0053*I**3
      END DO

      CALL CALCSHK (DYNAMIC, 
     &    DTT, ISTAGE, ISWWAT, ITRANS, LTRANS,            !Input
     &    MODELVER, P1, P1T, SHOCKFAC, TAGE, TMAX,        !Input
     &    TMIN, YRDOY, YRSOW,                             !Input
     &    CARBO, CUMDTT,                                  !I/O
     &    TSHOCK)                                         !Output
      CALL RI_TILLSUB (DYNAMIC,
     &    AGEFAC, DTT, FLOOD, G2, G3, GPP, GRNWT, ISTAGE, !Input
     &    LAI, MGPP, MGROLF, MPLAG, NSTRES, P3, P4, P5,   !Input
     &    RGFILL, RTR, SLFN, SLFT, SLFW, SUMDTT, TCARBO,  !Input
     &    TEMF, TFILL, TGPP, TMPFIL, TSHOCK, TURFAC, XN,  !Input
     &    PSTRES1, PSTRES2, KSTRES,                       !Input
     &    TGROGRN, TGROLF, TGROSTM, TPLAG,                !I/O
     &    TILNO, TLFWT, TSTMWT, TPLA)                     !Output
      CALL RI_NUPTAK (DYNAMIC, 
     &    FLOOD, NH4, NO3, PDWI, PGRORT, PLANTS, PTF,     !Input
     &    RCNP, RLV, RTWT, SOILPROP, ST, STOVWT, SW, TCNP,!Input
     &    FLOODN, STOVN, RANC, ROOTN, TANC,               !I/O
     &    RNLOSS, SENESCE, TRNLOS, UNH4, UNO3, PLIGRT,    !Output
     &    CumNUptake)                                     !Output

      CALL RI_KUPTAK(
     &       ISWPOT, NLAYR, SKi_Avail, UNH4, UNO3,        !Input
     &       KUPTAKE, KSTRES)                             !Output
      CALL P_Ceres (DYNAMIC, ISWPHO,                        !Input
     &      CumLeafSenes, DLAYR, DS, FILECC, MDATE, NLAYR,  !Input
     &      PCNVEG, PLTPOP, PODWT, RLV, RTDEP, RTWTO,       !Input
     &      SDWT, SWIDOT, SeedFrac, SPi_AVAIL, Stem2Ear,    !Input
     &      STMWTO, VegFrac, WLIDOT, WRIDOT, WSIDOT,        !Input
     &      WTLF, YRPLT,                                    !Input
     &      SENESCE,                                        !I/O
     &      PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, !Output
     &      PStres1, PStres2, PUptake, FracRts)             !Output


      FIRST = .TRUE.

!***********************************************************************
!***********************************************************************
!     Daily rate / integration calculations
!***********************************************************************

      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------

          SENESCE % ResWt  = 0.0
          SENESCE % ResLig = 0.0
          SENESCE % ResE   = 0.0

!         Keep yesterdays cumulative senescence 
          CumLeafSenesY = CumLeafSenes

      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL PlantInit(
     &    GRAINN, NPPH, PLPH, ROOTN, STOVN,               !Input
     &    PLANTS, PLTPOP,                                 !I/O
     &    ITRANS, ROWSPC, SEEDNI, SPACE, TPLANTS)         !Output
      ENDIF

!     MAKE SURE THAT GROSUB IS CALLED UNDER THESE CONDITIONS! - CHP
      IF (YRDOY .EQ. YRPLT .AND.(ITRANS .EQ. 2 .OR. ITRANS .EQ. 3)) THEN
        CALL TRNSPL_GROSUB (DYNAMIC,  
     &    ATEMP, CDTT_TP, FIELD, ISWNIT, ITRANS, NPPH,    !Input
     &    P1T, PANWT, PHINT, PLANTS, SDTT_TP, SDWTPL,     !Input
     &    TAGE, TBASE, TMAX, TPLANTS, XST_TP,             !Input
     &    AGEFAC, BIOMAS, CUMPH, ISTAGE, LAI, LFWT, MLFWT,!Output
     &    MPLA, MSTMWT, NDEF3, NFAC, NSTRES, RCNP, ROOTN, !Output
     &    RTWT, SEEDRV, STMWT, STOVN, STOVWT, TANC,       !Output
     &    TCNP, TLFWT, TMNC, TPLA, TRLOS)                 !Output

!     UPDATE WEIGHTS FOR P MODEL      
	WTLF = LFWT * PLTPOP      !Leaf weight, g/m2
      STMWTO = STMWT * PLTPOP   !Stem weight, g/m2
      RTWTO = RTWT * PLTPOP     !Root weight, g/m2
      PODWT = PANWT * PLTPOP    !Panicle weight, g/m2
      SDWT = GRNWT * PLTPOP     !seed weight, g/m2

        IF (ISWPHO .NE. 'N') THEN
          CALL P_Ceres (EMERG, ISWPHO,                           !Input
     &      CumLeafSenes, DLAYR, DS, FILECC, MDATE, NLAYR,  !Input
     &      PCNVEG, PLTPOP, PODWT, RLV, RTDEP, RTWTO,       !Input
     &      SDWT, SWIDOT, SeedFrac, SPi_AVAIL, Stem2Ear,    !Input
     &      STMWTO, VegFrac, WLIDOT, WRIDOT, WSIDOT,        !Input
     &      WTLF, YRPLT,                                    !Input
     &      SENESCE,                                        !I/O
     &      PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, !Output
     &      PStres1, PStres2, PUptake, FracRts)             !Output
        ENDIF

        !LAI = 0.01
        IF (ISWNIT .EQ. 'Y') THEN
          APTNUP     = STOVN*10.0*PLANTS   
          XANC       = STOVN/STOVWT*100.0  
        ENDIF
      ENDIF

      !Initialize for new stage set today
      IF (NEW_PHASE) THEN
        SELECT CASE (ISTAGE)
        CASE (1)
          LEAFNO = 1      !GROSUB

          IF (ISWWAT .EQ. 'Y') THEN
            DO L = 1, NLAYR
              RLV(L) = 0.0 
!              RWU(L) = 0.0   ROOTWU, not here.
            END DO

            CUMDEP = 0.0
            DO L = 1, NLAYR
              CUMDEP = CUMDEP + DLAYR(L)
              RLV(L) = AMIN1(0.20*PLANTS/DLAYR(L),0.5) 
              IF (CUMDEP .GT. RTDEP) then
                EXIT
              ELSE
                RLV(L) = RLV(L)*(1.0-(CUMDEP-RTDEP)/DLAYR(L))  
              ENDIF          
              IF (L .EQ. NLAYR) THEN
                IF (CUMDEP .LE. RTDEP) THEN
                  RTDEP = CUMDEP  !ROOTGR, TRANSPL
                  EXIT
                ENDIF
              ENDIF
            END DO
          ENDIF

        CASE (4)
          ANTSTM = STMWT
          ABIOMS = BIOMAS*PLANTS
          CANWAA = ABIOMS
          CANNAA = ABIOMS*TANC
          SPIKE   = G1*ANTSTM
          GPP     = SPIKE*AMIN1 (STRCOLD,STRHEAT)      !HSNSTRES)
          MGPP    = G1*MSTMWT*AMIN1 (STRCOLD,STRHEAT)  !,HSNSTRES)
          PTF    = 1.0    
          TILNO  = AMIN1 (TILNO,SPACE)   
          IF (MGPP .GT. 0.0 .AND. GPP/MGPP .LT. 2.0) THEN
             GPP    = MGPP*2.0
             TGPP   = GPP - MGPP
          ENDIF

        CASE (5)
          GPP    = GPP*FERTILE*WSTRES
          MGPP   = MGPP*FERTILE*WSTRES
          TGPP   = GPP - MGPP
          GPP    = AMAX1 (GPP,0.0)
C
C         Calculate single grain-filling rate: g/degree C
C
          RGFILL = G2/(P5 *0.95)     ! LINEAR GRAIN FILL
C
C         Lower rate for varieties with lower grain number
C
          IF (G1 .LT. 50.0) THEN
             RGFILL = G2/P5*(G1/50.0)
          ENDIF
          
          MFILL   = 0.0
          TFILL   = 0.0
          VANC    = TANC
	!PCNVEG = TANC * 100     ! FOR P_CERES
          VMNC    = TMNC
!          STRESSN = AMAX1 (SI4(3),SI4(4))    !not used
          !
          IF (MSTMWT .GT. 0.0) THEN
             TILRAT = STMWT/(0.75*G3*MSTMWT)
           ELSE
             TILRAT = 1.0
          ENDIF

          TILNO   = AMIN1 (TILNO,TILRAT)
          SWMIN   = 35.0*G2

          IF (STMWT .LT. SWMIN) THEN
             SWMIN = STMWT          
          ENDIF                     

          PANIWT  = 0.0
          G1FAC   = AMIN1 (1.0,G1/50.0)
          PANFAC  = 0.65*G1FAC    
          IF (STRHEAT .LT. 1.0) THEN
             PANFAC = PANFAC * STRHEAT
             !WRITE(*,*)'PNF STRH', PANFAC,STRHEAT,G1
             !PAUSE
          ENDIF
          !STRESSW = AMAX1 (SI2(3),SI2(4))
          PANIWT  = (MSTMWT*0.4+MLFWT*0.20)*G1FAC*(1.0-0.5*STRESSW) 

          IF ((MSTMWT-PANIWT) .LT. ((1.0-PANFAC)*MSTMWT)) THEN
             PANIWT = PANFAC*MSTMWT
          ENDIF

          TPANIWT = PANIWT*(TSTMWT/(MSTMWT*G3))
          TPANIWT = AMAX1 (TPANIWT,0.0)

          IF (STMWT-(PANIWT+TPANIWT) .LT. SWMIN) THEN
             MSTMWT  = AMIN1(SWMIN*(PANIWT/TPANIWT),SWMIN)
             TSTMWT  = SWMIN-MSTMWT
             PPANIWT = STMWT-SWMIN
             PANIWT  = AMIN1 (PPANIWT,PPANIWT*(PANIWT/TPANIWT))
             TPANIWT = PPANIWT-PANIWT
             STMWT   = MSTMWT+TSTMWT
          ELSE
             MSTMWT  = MSTMWT - PANIWT
             TSTMWT  = TSTMWT - TPANIWT
             STMWT   = MSTMWT + TSTMWT
          ENDIF

          PANIWT  = PANIWT + TPANIWT
          RESERVE = PANFAC * PANIWT                  ! 0.40
          RESERVE = AMIN1 (RESERVE,STMWT)

          IF (GPP .GT. 0.0) THEN
             GRNWT  = PANFAC*PANIWT                  ! 0.35
             !
             ! GRAINN content is 2X > pan N content
             !
             !GRAINN    = GRNWT * 0.011
             GRAINN    = PANFAC*PANIWT*TCNP*1.15*(1.0-SI3(4))   !1.7
             STOVNMIN  = TMNC * (STOVWT - GRNWT)
             
             IF (STOVN-GRAINN .LT. STOVNMIN) THEN
                GRAINN = MAX(0.0, STOVN - STOVNMIN)
                STOVN  = STOVNMIN
              ELSE
                STOVN  = STOVN - GRAINN
             ENDIF
           
             IF (ISWNIT .EQ. 'Y' .AND. GRNWT .GT. 0.0) THEN
                GNC = GRAINN/GRNWT
                IF (GNC .LT. 0.011) THEN
                   GRNWT = GRAINN/0.011
                ENDIF
                IF (GRNWT > 0.0) THEN
                  GNP    = GRAINN / GRNWT*100.0
                ENDIF
             ENDIF
             PANIWT    = PANIWT - GRNWT
             STOVWT = STOVWT - GRNWT
             TANC   = STOVN  / STOVWT
           ELSE
             GRNWT  = 0.0
             GRAINN = 0.0
          ENDIF
           
!          EMAT   = 0
!          ISM    = 0
          TGROLF = 0.0

        CASE (6)
          IF (TSGRWT .GE. 35.0) THEN
             GSIZE = 1.0 - 0.05*(TSGRWT-35.0)
           ELSEIF (TSGRWT .LE. 15.0) THEN
             GSIZE = 1.0 + 0.05*(15.0-TSGRWT)
          ENDIF

          GSIZE = AMIN1 (GSIZE,1.15)
          GSIZE = AMAX1 (GSIZE,0.85)
CCCCC-PW
          SKERWT = G2*GSIZE
          GRAIN  = GRNWT/(G2*GSIZE)
          IF (GRAIN .GT. GPP .AND. GPP .GT. 0.0) THEN
             SKERWT = AMIN1 (GRNWT/GPP,G2*1.05)
          ENDIF
          GPSM   = GRNWT / SKERWT*PLANTS

        CASE(8)       !germination
          RTDEP  = SDEPTH     !FROM PHENOL

        CASE (20)
          DYIELD = GRNWT*10.0*PLANTS
          SKERWT = G2*GSIZE
          STOVER = (BIOMAS*PLANTS*10.0)  - DYIELD

!       chp 2018-07-19 - default to zero removal of by-product from field
!       This default behavior is already handled in the input module.
!          IF (HARVFRAC(2) .LE. 0.0) THEN 
!              HARVFRAC(2) = 1.0
!          ENDIF

!          IF (STOVER .EQ. 0.0) THEN
!             GSRATIO = 0.0       !NOT USED
!           ELSE
!             GSRATIO = DYIELD/STOVER 
!          ENDIF

!          PANNO  = (TILNO+1.0)*PLANTS    !Not used
          GRAIN  = GRNWT/(G2*GSIZE)
          IF (GRAIN .GT. GPP .AND. GPP .GT. 0.0) THEN
             SKERWT = AMIN1 (GRNWT/GPP,G2*1.05)
          ENDIF
          SPIKE  = AMAX1 (SPIKE,GPP)
          UNFILL = SPIKE - GRNWT/SKERWT
          GPSM   = GRNWT / SKERWT*PLANTS
          IF (ISWNIT .EQ. 'Y' .AND. GRNWT .GT. 0.0) THEN
             XGNP   = (GRAINN/GRNWT)*100.0
             GNUP   = GRAINN*10.0*PLANTS
             TOTNUP = GNUP + APTNUP
          ENDIF
          TOTNUP = (GRAINN+STOVN)*10.0*PLANTS

        END SELECT
      ENDIF

      IF (ISWNIT .EQ. 'Y') THEN
         XANC   = TANC*100.0           
         APTNUP = STOVN*10.0*PLANTS    
      ENDIF

      IF (PLANTS .LE. 0.0 .OR. ISTAGE .GE. 6 .OR. .NOT. TF_GRO) THEN
        AGEFAC = 1.0
        NSTRES = 1.0
        UNO3 = 0.0
        UNH4 = 0.0
        RETURN
      ENDIF

      IF (ISWNIT .EQ. 'Y' .AND. ISTAGE .LT. 7) THEN
        CALL RI_NFACTO(DYNAMIC, FIELD, XSTAGE, 
     &    AGEFAC, NDEF3, NFAC, NSTRES, RCNP, TANC, TCNP, TMNC)  !Output
      ENDIF

      PAR     = 0.5*SRAD     !PAR local variable
     
      PLSC    = AMIN1((0.5+0.001*PLANTS),1.0)
      TL      = EXP(-0.85*ROWSPC*PLSC)               ! .65
      Y1      = EXP(-0.625*LAI)                      ! .625
      Y2      = EXP(-TL*LAI)
      EPLANTS = PLANTS**0.975
      RUEA    = 5.85   !6.85      UPS UH 03/21/03
      RUEX    = 0.65
      PCARB   = RUEA*PAR**RUEX/EPLANTS*(1.0-AMIN1(Y1,Y2))
      !
      ! PCARB2  = 2.95*PAR/EPLANTS*(1-AMAX1(Y1,Y2))
      !
      ! Calculate Photosynthetic Response to CO2
      !
      PCO2  = TABEX (CO2Y,CO2X,CO2,10)
      PCARB = PCARB*PCO2
!     need to lower so temp is 10 for prft=0 mar17
      PRFT  = 1.0-0.0025*((0.25*TMIN+0.75*TMAX)-26.0)**2   
      PRFT  = AMAX1 (PRFT,0.0)
      IF (PRFT .GT. 1.0) THEN
          PRFT = 1.0                                
      ENDIF

      !
      ! Calculate Transplanting Shock
      !
      CALL CALCSHK (DYNAMIC, 
     &    DTT, ISTAGE, ISWWAT, ITRANS, LTRANS,            !Input
     &    MODELVER, P1, P1T, SHOCKFAC, TAGE, TMAX,        !Input
     &    TMIN, YRDOY, YRSOW,                             !Input
     &    CARBO, CUMDTT,                                  !I/O
     &    TSHOCK)                                         !Output

      CARBO = PCARB*AMIN1(PRFT,SWFAC,NSTRES,TSHOCK,PStres1,KSTRES)
     &       * SLPF

      TEMF  = 1.0
      TEMPM = (TMAX + TMIN)*0.5

      IF (TMIN .LT. 14.0 .OR. TMAX .GT. 32.0) THEN
         IF (TMAX .LT. TBASE) THEN
            TEMF = 0.0
         ENDIF
         IF (TEMF .NE. 0.0) THEN
            TEMF = 0.0
            DO I = 1, 8
               TTMP = TMIN + TMFAC1(I)*(TMAX-TMIN)
               IF (TTMP .GT. 14.0  .AND. TTMP .LE. 32.0) THEN
                   TEMF = TEMF + 1.0/8.0
               ENDIF
               IF (TTMP .GE. TBASE .AND. TTMP .LT. 14.0) THEN
                   TEMF = TEMF + 0.0210*(TTMP-TBASE)
               ENDIF
               IF (TTMP .GT. 32.0  .AND. TTMP .LT. 42.0) THEN
                   TEMF = TEMF + 0.0125*(42.0-TTMP)
               ENDIF
            END DO
         ENDIF
      ENDIF

!     Calculate radiation to temperature ratio for tillering and senescence
      IF (DTT .GT. 0) THEN
        RTR = SRAD / DTT
      ELSE
        RTR = SRAD
      ENDIF


      PC    =  1.0
      IF     (CUMPH .LT. 5.0) THEN
!        PC = 0.56 + 0.068*CUMPH  ! J.Padilla
         PC = 0.48 + 0.068*CUMPH  ! J.Padilla  0.56 v4  UPS UH 03/21/03
      ELSEIF (CUMPH .GT. 9.0) THEN
         PC = 0.10 + 0.100*CUMPH        !0.25
      ENDIF
      IF (ISTAGE .LT. 3) THEN
          TI    = DTT/(PHINT/PHEFAC*PC)
      ELSEIF ((ISTAGE .EQ. 3) .AND. (SUMDTT .LT. (P3-20.-PHINT))) THEN 
          TI    = DTT/(PHINT/PHEFAC*PC)
      ELSEIF ((ISTAGE .EQ. 3) .AND. (SUMDTT .LE. (P3-20.-PHINT))) THEN 
          TI    = DTT/(PHINT*PC)
      ELSEIF (ISTAGE .GE. 4) THEN
          TI    = DTT/(PHINT*PC)
      ENDIF
      CUMPH = CUMPH + DTT/(PHINT*PC)
      XN    = CUMPH
      !    IF (SUMDTT .LT.(P3-20.-PHINT)) THEN
      !    
      !   ELSE
      !
      !       IF (SUMDTT .GE.(P3-20.-PHINT)) THEN 
       !  TI    = DTT/(PHINT*PC)
       ! CUMPH = CUMPH + DTT/(PHINT*PC)
        ! XN    = CUMPH
      IF (ISTAGE .LT. 4) THEN
         LEAFNO = AMAX1 (CUMPH,1.0)
      ENDIF
      BF    = 0.12                                   ! 0.12
      XTN   = -9.678*EXP(-BF*CUMPH)
      A     = 3800.0       !3800

      SELECT CASE (ISTAGE)
        CASE (1)  ! Juvenile stage
          MPLAG = A/CUMPH*(-BF)*XTN*EXP(XTN)*TI*TSHOCK*
     &            AMIN1(TURFAC,TEMF,AGEFAC,PSTRES2,KSTRES)
          !
          ! Need to incorporate shock effect
          !
          MGROLF = MPLAG*0.0055
          GRORT  = CARBO - MGROLF
          GRORT  = AMAX1 (GRORT,CARBO*0.35)
          IF (SEEDRV .GT. 0.0) THEN
             SEEDRV = SEEDRV + CARBO - MGROLF - GRORT
          ENDIF
          IF (SEEDRV .LT. 0.0) THEN
             SEEDRV = 0.0
             MGROLF = CARBO*0.65
             GRORT  = CARBO - MGROLF
             MPLAG  = MGROLF/0.0055*TSHOCK*
     &		        AMIN1(TURFAC,TEMF,AGEFAC,PSTRES2,KSTRES)
          ENDIF
          IF (CUMPH .LE. 5.0/G3 .OR. TSHOCK .LT. 1.0) THEN
             MPLA  = MPLA  + MPLAG
             PLA   = MPLA
             MLFWT = MLFWT + MGROLF
             LFWT  = MLFWT
             STMWT = MSTMWT
          ENDIF
          IF (CUMPH .GT. 5.0/G3 .AND. TSHOCK .GE. 1.0) THEN
             IF (MGROLF .GT. 0.60*CARBO) THEN
                MGROLF = 0.60*CARBO
                MPLAG  = MGROLF/0.0060*
     &			       AMIN1 (TURFAC,TEMF,AGEFAC,PSTRES2,KSTRES)
             END IF
             GRORT = 0.25*CARBO
             IF (MGROLF .LT. (0.30*CARBO*TSHOCK)) THEN
                MGROLF = 0.35*TSHOCK*CARBO
                MPLAG  = MGROLF/0.0060*
     &			       AMIN1 (TURFAC,TEMF,AGEFAC,PSTRES2,KSTRES)
             ENDIF
             MGROSTM = MGROLF*0.85   !0.15
             TCARBO  = CARBO - MGROLF - GRORT - MGROSTM
             IF (TCARBO .GT. 0.0) THEN
                GRORT  = GRORT + TCARBO*(1.0-TSHOCK)
                TCARBO = TCARBO*TSHOCK
                CALL RI_TILLSUB (DYNAMIC,
     &            AGEFAC, DTT, FLOOD, G2, G3, GPP, GRNWT, ISTAGE,!Input
     &            LAI, MGPP, MGROLF, MPLAG, NSTRES, P3, P4, P5,  !Input
     &            RGFILL, RTR, SLFN, SLFT, SLFW, SUMDTT, TCARBO, !Input
     &            TEMF, TFILL, TGPP, TMPFIL, TSHOCK, TURFAC, XN, !Input
     &            PSTRES1, PSTRES2, KSTRES,                      !Input 
     &            TGROGRN, TGROLF, TGROSTM, TPLAG,               !I/O
     &            TILNO, TLFWT, TSTMWT, TPLA)                    !Output
              ELSE
                TCARBO = 0.0
                TGROLF = 0.0
                TPLAG  = 0.0
             ENDIF
             MPLA  = MPLA  + MPLAG
             MLFWT = MLFWT + MGROLF
             PLA   = MPLA  + TPLA
             LFWT  = MLFWT + TLFWT
             STMWT = MSTMWT
          ENDIF

        CASE (2)  ! Panicle initiation
          MPLAG  = A/CUMPH*(-BF)*XTN*EXP(XTN)*TI*
     &             AMIN1(TURFAC,TEMF,AGEFAC,PSTRES2,KSTRES)*TSHOCK
          XPLAG1 = MPLAG
          MGROLF = MPLAG*0.0060
          IF (CUMPH .LE. 6.0) THEN
             IF (MGROLF .GT. CARBO*0.60) THEN
                MGROLF = CARBO*0.60
                MPLAG  = MGROLF/0.0060*TSHOCK*
     &		           AMIN1(TURFAC,TEMF,AGEFAC,PSTRES2,KSTRES)
             ENDIF
             MGROSTM = MGROLF * 0.15
             GRORT   = CARBO  - MGROLF - MGROSTM
             MPLA    = MPLA   + MPLAG
             MLFWT   = MLFWT  + MGROLF
             MSTMWT  = MSTMWT + MGROSTM
             PLA     = MPLA
             LFWT    = MLFWT
             STMWT   = MSTMWT
           ELSE
             GRORT   = CARBO*0.20
             IF (MGROLF .GT. 0.45*CARBO) THEN
                MGROLF = 0.45*CARBO
                MPLAG  = MGROLF/0.0060*TSHOCK*
     &                   AMIN1(TURFAC,TEMF,AGEFAC,PSTRES2,KSTRES)
             ENDIF
             MGROSTM = MGROLF*0.85   !0.15
             TCARBO  = CARBO - MGROLF - MGROSTM - GRORT
             GRF     = 0.25*CARBO*G3*AGEFAC
             IF (TCARBO .LT. GRF) THEN
                TCARBO  = TCARBO  + GRF
                MGROLF  = MGROLF  - (0.5*GRF)
                MGROSTM = MGROSTM - (0.5*GRF)
                MPLAG   = MGROLF/0.0055*TSHOCK*
     &			        AMIN1(TURFAC,TEMF,AGEFAC,PSTRES2,KSTRES)
             END IF
             IF (TCARBO .GT. 0.0) THEN
                GRORT  = GRORT + TCARBO*(1.0-TSHOCK)
                TCARBO = TCARBO* TSHOCK
                CALL RI_TILLSUB (DYNAMIC,
     &            AGEFAC, DTT, FLOOD, G2, G3, GPP, GRNWT, ISTAGE,!Input
     &            LAI, MGPP, MGROLF, MPLAG, NSTRES, P3, P4, P5,  !Input
     &            RGFILL, RTR, SLFN, SLFT, SLFW, SUMDTT, TCARBO, !Input
     &            TEMF, TFILL, TGPP, TMPFIL, TSHOCK, TURFAC, XN, !Input
     &            PSTRES1, PSTRES2, KSTRES,                      !Input 
     &            TGROGRN, TGROLF, TGROSTM, TPLAG,               !I/O
     &            TILNO, TLFWT, TSTMWT, TPLA)                    !Output
              ELSE
                TCARBO = 0.0
                TGROLF = 0.0
                TPLAG  = 0.0
             ENDIF
             MPLA   = MPLA   + MPLAG
             MLFWT  = MLFWT  + MGROLF
             MSTMWT = MSTMWT + MGROSTM
             PLA    = MPLA   + TPLA
             LFWT   = MLFWT  + TLFWT
             STMWT  = MSTMWT + TSTMWT
          END IF
          TCARB1 = 0.0

        CASE (3)  ! Heading and end of leaf growth
          MPLAG   = A/CUMPH*(-BF)*XTN*EXP(XTN)*TI*
     &              AMIN1(TURFAC,TEMF,AGEFAC,PSTRES2,KSTRES) +
     &              (0.65*TCARB1)/0.0055
          GRORT   = CARBO*0.15
          MGROLF  = MPLAG*0.0060
          MGROSTM = 0.0015*DTT*AMIN1(TURFAC,TEMF)+0.35*TCARB1
          MGROSTM = AMIN1 (MGROSTM,0.65*CARBO)
          IF (MGROLF .GT. 0.35*CARBO .AND. SUMDTT .GT. 249.0) THEN
             MGROLF = 0.35*CARBO
             MPLAG  = MGROLF/0.0060*
     &        		AMIN1(TURFAC,TEMF,AGEFAC,PSTRES2,KSTRES)
          END IF
          TILCAR = CARBO - MGROLF - MGROSTM - GRORT
          IF (TILCAR .LE. 0.0) THEN
             TILCAR  = (MGROSTM*0.5+MGROLF*0.3)*G3    !US
             MGROLF  = (MGROLF*0.7)/G3
             MGROSTM = CARBO - GRORT - MGROLF - TILCAR
             MPLAG   = MGROLF/0.0060*
     &		         AMIN1(TURFAC,TEMF,AGEFAC,PSTRES2,KSTRES)
          END IF
          TCARBO = TILCAR
          GRF    = TILCAR     !US
          IF (GRF .GT. CARBO-GRORT) THEN
             GRF     = CARBO - GRORT
             MGROLF  = 0.0
             MGROSTM = 0.0
             MPLAG   = 0.0
             TCARBO  = GRF
          ENDIF
          IF (TCARBO .NE. GRF) THEN
             ADDTIL  = GRF     - TCARBO
             SHORAT  = MGROLF  / (MGROLF+MGROSTM)
             TPART   = SHORAT  * ADDTIL
             MGROLF  = MGROLF  - TPART
             MGROSTM = MGROSTM - (ADDTIL-TPART)
             TCARBO  = GRF
             MPLAG   = MGROLF/0.0060*
     &                 AMIN1(TURFAC,TEMF,AGEFAC,PSTRES2,KSTRES)
          ENDIF
          MLFWT  = MLFWT  + MGROLF
          MSTMWT = MSTMWT + MGROSTM
          MPLA   = MPLA   + MPLAG
          IF (TCARBO .GT. 0.0) THEN
            CALL RI_TILLSUB (DYNAMIC,
     &        AGEFAC, DTT, FLOOD, G2, G3, GPP, GRNWT, ISTAGE, !Input
     &        LAI, MGPP, MGROLF, MPLAG, NSTRES, P3, P4, P5,   !Input
     &        RGFILL, RTR, SLFN, SLFT, SLFW, SUMDTT, TCARBO,  !Input
     &        TEMF, TFILL, TGPP, TMPFIL, TSHOCK, TURFAC, XN,  !Input
     &        PSTRES1, PSTRES2, KSTRES,                       !Input
     &        TGROGRN, TGROLF, TGROSTM, TPLAG,                !I/O
     &        TILNO, TLFWT, TSTMWT, TPLA)                     !Output
           ELSE
             TCARBO  = 0.0
             TGROLF  = 0.0
             TGROSTM = 0.0
             TPLAG   = 0.0
          ENDIF
          TCARB1 = TILCAR - TCARBO
          TCARB1 = AMAX1 (TCARB1,0.0)
          LFWT   = MLFWT  + TLFWT
          STMWT  = MSTMWT + TSTMWT
          PLA    = MPLA   + TPLA
!          SLAN   = PLA*(0.005*(SUMDTT/P3)**2)       
          SLAN   = (PLA-SENLA)*(0.005*(SUMDTT/P3)**2) !US
          CHK    = TCARBO + MGROLF + MGROSTM + GRORT

        CASE (4)  ! Beginning of effective grain filling period
          CARBO   = CARBO*(1.0-0.25*(SUMDTT/(P4+P5)))
          MGROSTM = 0.001*DTT*AMIN1(SWFAC,TEMF)
          MGROLF  = 0.0
          GRORT   = CARBO*0.15
          TCARBO  = CARBO - MGROSTM - GRORT
          IF (TCARBO .LT. 0.0) THEN
             TCARBO  = 0.0
             MGROSTM = CARBO - GRORT
          END IF
          GRF = 0.1*CARBO*G3*AMIN1(AGEFAC,PSTRES2,KSTRES)
          IF (TCARBO .LT. GRF) THEN
             TCARBO  = TCARBO  + GRF
             MGROSTM = MGROSTM - GRF
          END IF
          IF (TCARBO .GT. 0.0) THEN
            CALL RI_TILLSUB (DYNAMIC,
     &        AGEFAC, DTT, FLOOD, G2, G3, GPP, GRNWT, ISTAGE, !Input
     &        LAI, MGPP, MGROLF, MPLAG, NSTRES, P3, P4, P5,   !Input
     &        RGFILL, RTR, SLFN, SLFT, SLFW, SUMDTT, TCARBO,  !Input
     &        TEMF, TFILL, TGPP, TMPFIL, TSHOCK, TURFAC, XN,  !Input
     &        PSTRES1, PSTRES2, KSTRES,                       !Input
     &        TGROGRN, TGROLF, TGROSTM, TPLAG,                !I/O
     &        TILNO, TLFWT, TSTMWT, TPLA)                     !Output
           ELSE
             TCARBO  = 0.0
             TGROLF  = 0.0
             TGROSTM = 0.0
             TPLAG   = 0.0
          ENDIF
          PLA    = PLA    + TPLAG
          LFWT   = MLFWT  + TLFWT
          MSTMWT = MSTMWT + MGROSTM
          STMWT  = MSTMWT + TSTMWT
!          SLAN   = PLA*(0.015+0.95*(SUMDTT/(P4+P5))**2)      
          SLAN   = (PLA-SENLA)*(0.005+0.01*(SUMDTT/(P4+P5))**2)   !US

        CASE (5)  ! End of main culm grain filling
          IF (PLANTS .LE. 0.01) RETURN
          TCARB1  = CARBO
          CARBO  = CARBO*(1.0-0.45*(SUMDTT/P5))
!          SLAN   = PLA*(0.015+0.85*((SUMDTT+P4)/(P4+P5))**2)
          !0.85--> 0.25   US 
          SLAN   = (PLA-SENLA)*(0.005+0.01*((SUMDTT+P4)/(P4+P5))**2)   
          TMPFIL = 1.0

          IF (TEMPM .LT. 20.0) THEN
             TMPFIL = (TEMPM-7.0)/13.0
           ELSEIF (TEMPM .GT. 32.0) THEN
             TMPFIL = 1.0 - ((TEMPM-32.0)/40.0)
          ENDIF

          TMPFIL = AMAX1 (TMPFIL,0.0)
          IF (SUMDTT .LT. P5*0.90) THEN
             !
             ! Depends on pot'l sink size, variety, temp and
             ! minimum of water or N
             !
             IF (SUMDTT .LT. 75 .AND. MGPP .GT. MFILL) THEN
                GROGRN = (MGPP-MFILL)*RGFILL*DTT*TMPFIL*SWFAC
              ELSE
                GROGRN = (MGPP+TGPP-MFILL)*RGFILL*DTT*TMPFIL*SWFAC
             ENDIF
             GNO     = GRNWT/(G2*1.05)               ! V2.1 - 1.05
             IF (GNO .GT. GPP) THEN
                GROGRN = 0.0
             ENDIF
             GRORT   = CARBO*(0.2-0.1*SWFAC)
             GROSTM  = CARBO - GROGRN - GRORT
             TGROGRN = 0.0
             GROLF   = 0.0
             IF (GROSTM .LT. 0.0) THEN
                IF (RESERVE .GT. 0.0) THEN
                   RESERVE = RESERVE + GROSTM
                   GROLF   = 0.15*GROSTM
                   GROSTM  = 0.85*GROSTM
                   IF (RESERVE .LT. 0.0) THEN
                      GROGRN  = GROGRN + RESERVE
                      RESERVE = 0.0
                   ENDIF
                 ELSE
                  GROGRN = CARBO - GRORT
                  GROSTM = 0.0
                ENDIF
              ELSE
                STMGF   = AMIN1 (1.0,(0.5+0.0025*SUMDTT))
                RESERVE = RESERVE + GROSTM*STMGF
             ENDIF
             MLFWT = MLFWT + GROLF
             LFWT  = MLFWT + TLFWT
             STMWT = STMWT + GROSTM
             GRNWT = GRNWT + GROGRN
             MFILL = GRNWT / G2*0.65                 ! 0.75
             TFILL = 0.0
           ELSE
             !
             ! Further reduced at this late stage
             TCARBO   = CARBO*0.9
             !
             TGROGRN = 0.0
             TGROSTM = 0.0
             TGROLF  = 0.0
             IF (TCARBO .GT. 0.0) THEN
               CALL RI_TILLSUB (DYNAMIC,
     &           AGEFAC, DTT, FLOOD, G2, G3, GPP, GRNWT, ISTAGE, !Input
     &           LAI, MGPP, MGROLF, MPLAG, NSTRES, P3, P4, P5,   !Input
     &           RGFILL, RTR, SLFN, SLFT, SLFW, SUMDTT, TCARBO,  !Input
     &           TEMF, TFILL, TGPP, TMPFIL, TSHOCK, TURFAC, XN,  !Input
     &           PSTRES1, PSTRES2, KSTRES,                       !Input 
     &           TGROGRN, TGROLF, TGROSTM, TPLAG,                !I/O
     &           TILNO, TLFWT, TSTMWT, TPLA)                     !Output
             ENDIF
             STMWT   = STMWT  + TGROSTM
             TGRNWT  = TGRNWT + TGROGRN
             GROGRN  = TGROGRN
             GRNWT   = GRNWT  + GROGRN
             TFILL   = MFILL  + TGRNWT/G2*0.55       ! 0.65
          ENDIF

          IF (GROGRN .NE. 0.0 .AND. ISWNIT .EQ. 'Y') THEN
              !
              ! Need to confirm this relationship
              !
              RGNFIL = 8.929666-3.90*DTT+0.75*(TMAX-TMIN)+5.3067*TEMPM
c              RGNFIL = 4.929666-3.90*DTT+0.55*(TMAX-TMIN)+5.0067*TEMPM  
              NSINK  = RGNFIL*(MGPP+TGPP)*1.E-7
              NSINK  = AMAX1 (NSINK,0.0)
c             NSINK  = NSINK*AMIN1(NDEF3,SWFAC)
c US,MB,WP    Change from .LT. 0.001 to .LE. 0 to avoid large variability in Grain N content
              IF (NSINK .LE. 0.0) GO TO 1200
      
              NSINK  = AMIN1 (NSINK,0.015*GROGRN)
              NSINKT = TGROGRN/GROGRN*NSINK
              RMNC   = RCNP*0.8
              TANC   = STOVN / STOVWT
              TANC   = AMAX1 (TANC,TMNC)
              NPOOL1 = STOVWT* (TANC - TMNC)
              NPOOL2 = RTWT  * (RANC - RMNC)
              NPOOL2 = AMAX1 (NPOOL2,0.0)
              XNF    = 0.15 + 0.25*NFAC
C
C             XNF    = 0.25 + 0.25*NFAC
C
              TNLAB  = XNF*NPOOL1
              RNLAB  = XNF*NPOOL2
              NPOOL  = TNLAB + RNLAB    !1.75
              !
              ! Check supply demand balance for main stem first
              !
              IF (SUMDTT .GE. 0.90*P5 .AND. TGROGRN .GT. 0.0) THEN
                 !
                 ! Only come here for last bit of tiller panicle filling
                 !
                 NSDR = 0.0
                 IF (NSINKT .GT. 0.0) THEN
                    NSDR = NPOOL/NSINKT
                 ENDIF
                 IF (NSDR .LT. 1.0) THEN
                    NSINKT = NSINKT * NSDR
                    TGRNWT = TGRNWT - TGROGRN*(1.0-NSDR)
                    GRNWT  = GRNWT  - TGROGRN*(1.0-NSDR)
                    TFILL  = MFILL  + TGRNWT/G2*0.75
                 ENDIF
                 IF (NSINKT .GT. TNLAB) THEN
                    STOVN = STOVN - TNLAB
                    RNOUT = NSINKT- TNLAB
                    RNLAB = RNLAB - RNOUT
                    TNLAB = 0.0
                    ROOTN = ROOTN - RNOUT
                    RANC  = ROOTN / RTWT
                  ELSE
                    STOVN = STOVN - NSINKT
                    TANC  = STOVN / STOVWT
                 ENDIF
                 IF (NFAC .LT. 0.001) THEN
                    EMAT = EMAT + 1
                 ENDIF
                 NSINK = 0.0
                 GO TO 1200
              ENDIF

              NSDR   = NPOOL/NSINK
              NSINKT = 0.0
      
              IF (NSDR .LT. 1.0) THEN
                 !
                 ! Not enough N reduce grain filling
                 NSINK = NSINK*NSDR
                 IF (NSDR .LT. 0.5) THEN
                    GRNWT = GRNWT - GROGRN*(1.0-NSDR)
                    MFILL = GRNWT/G2*0.75
                 ENDIF
              ENDIF
              !
              ! Now if enough N in shoots supply panicles
              ! IF not enough get it from roots
              !
              IF (NSINK .GT. TNLAB) THEN
                 STOVN = STOVN - TNLAB
                 RNOUT = NSINK - TNLAB
                 RNLAB = RNLAB - RNOUT
                 TNLAB = 0.0
                 ROOTN = ROOTN - RNOUT
                 RANC  = ROOTN / RTWT
               ELSE
                 TNLAB = TNLAB - NSINK
                 STOVN = STOVN - NSINK
                 TANC  = STOVN / STOVWT
                 !
                 ! Here there is enough in shoot pool - just getting
                 ! enough to satisfy panicle demand
                 !
              ENDIF
              !
              ! Conversion efficiency
              !
1200          GRAINN = GRAINN + (NSINK + NSINKT)
              IF (GROGRN .GT. 0.0) THEN
                 GNC = (NSINK + NSINKT)/GROGRN*100.0
              ENDIF
              IF (GRNWT .GT. 0.0) THEN
                 GNP = GRAINN/GRNWT*100.0
              ENDIF
          ENDIF
      END SELECT

      
      DYIELD = GRNWT*10.0*PLANTS
      SKERWT = G2*GSIZE
      GPSM   = GRNWT / SKERWT*PLANTS
      STOVER = (BIOMAS*PLANTS*10.0)  - DYIELD

      
      CARBO  = AMAX1 (CARBO,0.0001)
      PDWI   = PCARB*(1.0-GRORT/CARBO)
      PGRORT = PCARB - PDWI
      YRTWT  = RTWT
      YSTOVWT= STOVWT
      PTF    =(LFWT + STMWT + PANIWT + GRNWT)/(LFWT + STMWT + PANIWT +
     &         GRNWT + (RTWT + 0.5*GRORT - 0.005*RTWT))
      ! TODAY'S PTF FOR NUPTAKE CALCULATION
      SLFW   = 0.950 + 0.050*SWFAC !MAR17

!      SLFN   = 0.995 + 0.005*NSTRES
!      IF (ISTAGE .GE. 4 .AND. LAI .GE. 2.0) THEN    
!         SLFN = 0.925 + 0.075*AGEFAC      

      SLFN   = 0.85 + 0.15*AGEFAC   !   .98  .02 NSTRES
      IF (ISTAGE .GE. 4 ) THEN  
         SLFN = 0.9750 + 0.0250*AGEFAC 
      ENDIF
      !
      ! SLFC
      !
      SLFC   = 1.0

      IF (LAI .GT. 5.0 .AND. RTR. LT. 1.5) THEN
          SLFC = AMAX1 (0.995,1.0-0.015*(LAI-5.0))
      ENDIF
      IF (ISTAGE .GE. 4 .AND. LAI .GT. 2.0) THEN
          SLFC = AMAX1 (0.825,1.0-0.02*LAI)
      ENDIF
      !
      ! SLFT
      !
      SLFT = 1.0
      IF (TEMPM .LE. 6.0) THEN
         SLFT = 1.0-(6.0-TEMPM)/6.0
      ENDIF
      IF (TMIN .LE. 0.0) THEN
         SLFT = 0.0
      ENDIF
      SLFT   = AMAX1 (SLFT,0.0)

!     Senescence due to phosphorus and potassium stress

      SLFP   = (1-FSLFP) + FSLFP*PSTRES1 
      SLFK   = (1-FSLFP) + FSLFK*KSTRES 

      !
      ! Senescence
      !
      PLAS   = (PLA-SENLA)*(1.0-AMIN1(SLFW,SLFC,SLFT,SLFN,SLFP,SLFK))
      IF  (PLAS .GT. 0.025*PLA) THEN
          PLAS =  PLA*0.015
      ENDIF

!     SENLA  = SENLA + PLAS
      SENLA  = SENLA + AMAX1(PLAS,SLAN)   !US
      !SENLA  = AMAX1 (SENLA,SLAN)
      SENLA  = AMIN1 (SENLA,PLA)

      LAI    = (PLA-SENLA)*PLANTS*0.0001
      SNLFWT = SENLA*0.00025                ! V2.1 - .00045->0.00045
!
      CumLeafSenes = SENLA * 0.00025 * PLTPOP * 10.
!         kg/ha     =  g/plant * plants/m2 * (kg/ha)/(g/m2)

      YLFWT  = LFWT
      BIOMAX = BIOMAS

      IF (LFWT .GT. SNLFWT) THEN
         LFWT = LFWT - SNLFWT
      ENDIF
      RTWT   = RTWT   + 0.5*GRORT - 0.005*RTWT
      PANWT  = PANIWT + GRNWT
      BIOMAS = LFWT   + STMWT + PANWT

      IF (BIOMAX .GT. BIOMAS) THEN
         BIOMAS = BIOMAX
         LFWT   = YLFWT
         SNLFWT = 0.0
      ENDIF

       
	WTLF = LFWT * PLTPOP      !Leaf weight, g/m2
      STMWTO = STMWT * PLTPOP   !Stem weight, g/m2
      RTWTO = RTWT * PLTPOP     !Root weight, g/m2
      PODWT = PANWT * PLTPOP    !Panicle weight, g/m2
      SDWT = GRNWT * PLTPOP     !seed weight, g/m2


!      IF (ISWNIT .EQ. 'Y' .AND. XSTAGE .LT. 10.0 .AND. FIELD) THEN
      IF (XSTAGE .LT. 10.0 .AND. FIELD) THEN
         IF (ISWNIT .EQ. 'Y' ) THEN
         CALL RI_NUPTAK (DYNAMIC, 
     &    FLOOD, NH4, NO3, PDWI, PGRORT, PLANTS, PTF,     !Input
     &    RCNP, RLV, RTWT, SOILPROP, ST, STOVWT, SW, TCNP,!Input
     &    FLOODN, STOVN, RANC, ROOTN, TANC,               !I/O
     &    RNLOSS, SENESCE, TRNLOS, UNH4, UNO3, PLIGRT,    !Output
     &    CumNUptake)                                     !Output
         ENDIF
	   ! Switches for P and K
         CALL MZ_KUPTAK(
     &       ISWPOT, NLAYR, SKi_Avail, UNH4, UNO3,        !Input
     &       KUPTAKE, KSTRES)                             !Output

         IF (ISWPHO .NE. 'N') THEN
           CALL P_Ceres (DYNAMIC, ISWPHO,                   !Input
     &      CumLeafSenes, DLAYR, DS, FILECC, MDATE, NLAYR,  !Input
     &      PCNVEG, PLTPOP, PODWT, RLV, RTDEP, RTWTO,       !Input
     &      SDWT, SWIDOT, SeedFrac, SPi_AVAIL, Stem2Ear,    !Input
     &      STMWTO, VegFrac, WLIDOT, WRIDOT, WSIDOT,        !Input
     &      WTLF, YRPLT,                                    !Input
     &      SENESCE,                                        !I/O
     &      PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, !Output
     &      PStres1, PStres2, PUptake, FracRts)             !Output
         ENDIF

      ENDIF
      RTWT   = RTWT   + 0.5*GRORT - 0.005*RTWT
      PANWT  = PANIWT + GRNWT
      BIOMAS = LFWT   + STMWT + PANWT

      IF (BIOMAX .GT. BIOMAS) THEN
         BIOMAS = BIOMAX
         LFWT   = YLFWT
         SNLFWT = 0.0
      ENDIF

      STOVWT = LFWT + STMWT + PANWT
      TANC = STOVN/STOVWT
      RANC = ROOTN/RTWT

!         Compute N lost in leaf senescence
!         N in senesced leaves - calculate based on today's N content
      CumLfNSenes = CumLfNSenes + 
     &             (CumLeafSenes - CumLeafSenesY) * STOVN / STOVWT

!       Senesced leaves do not fall to the ground and so are added to
!         surface litter only at harvest.
        SENESCE % ResWt(0)  = CumLeafSenes
        SENESCE % ResLig(0) = CumLeafSenes * 0.4 * PLIGLF
!       N in senesced leaves
        SENESCE % ResE(0,1) = CumLfNSenes


      MAXLAI = AMAX1 (LAI,MAXLAI)

      PBIOMS  = BIOMAS*PLANTS*10.0    !from phenol

!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
!     UPDATED WEIGHTS  
	WTLF = LFWT * PLTPOP      !Leaf weight, g/m2
      STMWTO = STMWT * PLTPOP   !Stem weight, g/m2
      RTWTO = RTWT * PLTPOP     !Root weight, g/m2
      PODWT = PANWT * PLTPOP    !Panicle weight, g/m2
      SDWT = GRNWT * PLTPOP     !seed weight, g/m2

      IF (ISWPHO .NE. 'N') THEN
       CALL P_Ceres (DYNAMIC, ISWPHO,                       !Input
     &      CumLeafSenes, DLAYR, DS, FILECC, MDATE, NLAYR,  !Input
     &      PCNVEG, PLTPOP, PODWT, RLV, RTDEP, RTWTO,       !Input
     &      SDWT, SWIDOT, SeedFrac, SPi_AVAIL, Stem2Ear,    !Input
     &      STMWTO, VegFrac, WLIDOT, WRIDOT, WSIDOT,        !Input
     &      WTLF, YRPLT,                                    !Input
     &      SENESCE,                                        !I/O
     &      PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, !Output
     &      PStres1, PStres2, PUptake, FracRts)             !Output
      ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
      RETURN
      END SUBROUTINE RI_GROSUB
C=======================================================================

C=======================================================================
C  PlantInit, Subroutine
C
C  Initialization at the beginning of planting
C-----------------------------------------------------------------------
C  Revision history
C
C  05/07/2002 CHP Written
C-----------------------------------------------------------------------
C  Called : RI_GROSUB
C=======================================================================
      SUBROUTINE PlantInit(
     &    GRAINN, NPPH, PLPH, ROOTN, STOVN,               !Input
     &    PLANTS, PLTPOP,                                 !I/O
     &    ITRANS, ROWSPC, SEEDNI, SPACE, TPLANTS)         !Output

      IMPLICIT NONE

      INTEGER ITRANS
      REAL GRAINN, NPPH, PDIST, PLANTN, PLANTS, PLPH, PLTPOP, ROOTN
      REAL ROWSPC, SEEDNI, SPACE, STOVN, TPLANTS, WINROW, XPLANT

      TPLANTS = PLTPOP                            !FROM IPFLOD

C     Set up hill population from plant population
      NPPH = PLPH
      IF (NPPH .LT. 0.001) THEN  
         NPPH = 3.0                               !FROM IPFLOD
      ENDIF                    

      SELECT CASE(ITRANS)
      CASE (1)    
        IF (PLANTS .LE. 0.0) THEN
          PLANTS = TPLANTS*NPPH                   !FROM IPFLOD
          PLTPOP = PLANTS                         !FROM IPFLOD
        ENDIF
        XPLANT = PLANTS
        PLANTN = (ROOTN + STOVN + GRAINN) / 10.0  !FROM INPHEN
        SEEDNI = PLANTN*PLANTS*10.0               !FROM INPHEN

      CASE (2)
        PLANTS = TPLANTS * NPPH                   !FROM IPFLOD
        PLTPOP = PLANTS                           !FROM IPFLOD
        XPLANT = PLANTS
        !PLANTS = 300.0                            !FROM INGROW
        PLANTN = 6.6E-6                           !FROM INPHEN
        SEEDNI = PLANTN*10.0*TPLANTS*NPPH         !FROM INPHEN

      CASE (3)
        PLANTS = TPLANTS * NPPH                   !FROM IPFLOD
        PLTPOP = PLANTS                           !FROM IPFLOD
        XPLANT = PLANTS
        PLANTN = (ROOTN + STOVN + GRAINN) / 10.0  !FROM INPHEN
        SEEDNI = PLANTN*PLANTS*10.0               !FROM INPHEN

      CASE (4)
        IF (PLANTS .LE. 0.0) THEN
          PLANTS = TPLANTS*NPPH                   !FROM IPFLOD
          PLTPOP = PLANTS                         !FROM IPFLOD
        ENDIF
        XPLANT = PLANTS
        PLANTN = (ROOTN + STOVN + GRAINN) / 10.0  !FROM INPHEN
        SEEDNI = PLANTN*PLANTS*10.0               !FROM INPHEN

      END SELECT

      IF (PLANTS .GT. 0.0) THEN
!        XPLANT = PLANTS                          !FROM INGROW
        SPACE  = 1000.0/XPLANT                    !FROM INGROW
      ELSE
        SPACE = 0.0
      ENDIF

      IF (ROWSPC*XPLANT .GT. 0.0) THEN
        WINROW = 1.0/(ROWSPC*XPLANT)              !FROM INGROW
      ELSE
        WINROW = 0.0                              !FROM INGROW
      ENDIF

      IF (WINROW .LT. ROWSPC) THEN
        IF (WINROW .LT. 0.1*ROWSPC) THEN
          SPACE = SPACE*WINROW/(0.1*ROWSPC)       !FROM INGROW
        ENDIF
      ELSE
        PDIST  = ROWSPC                           !FROM INGROW
        ROWSPC = WINROW                           !FROM INGROW
        IF (PDIST .LT. 0.1*ROWSPC) THEN
          SPACE = SPACE*PDIST/(0.1*ROWSPC)        !FROM INGROW
        ENDIF
      ENDIF

!     CHP added this in 7/31/2003 as per email from UPS
      PLTPOP = PLANTS

      RETURN
      END SUBROUTINE PlantInit

C=======================================================================

C=======================================================================
C  RI_IPGROSUB, Subroutine
C
C  Reads FILEIO for GROSUB routine
C  05/07/2002 CHP Written
C  08/12/2003 CHP Added I/O error checking
C=======================================================================
      SUBROUTINE RI_IPGROSUB (CONTROL, 
     &    ATEMP, CROP, FILEC, G1, G2, G3, P5, PHINT, PATHCR, 
     &    PLANTS, PLPH, PLTPOP, ROWSPC, SDWTPL)

      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT     NONE
      EXTERNAL ERROR, FIND

      CHARACTER*2 CROP
      CHARACTER*6  ERRKEY, SECTION
      PARAMETER (ERRKEY = 'IPRICE')
      CHARACTER*12 FILEC
      CHARACTER*30 FILEIO
      CHARACTER*80 PATHCR

      INTEGER LINC, LNUM, LUNIO, ERR, FOUND

      REAL ATEMP, G1, G2, G3, P5, PHINT
      REAL PLANTS, PLPH, PLTPOP, ROWSPC, SDWTPL

C     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL
      FILEIO = CONTROL % FILEIO
      LUNIO  = CONTROL % LUNIO

!-----------------------------------------------------------------------
!       Read data from FILEIO for use in PLANT module
!-----------------------------------------------------------------------
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)

C-----------------------------------------------------------------------
C     Read FILE names and paths
C-----------------------------------------------------------------------
      READ(LUNIO,'(6(/),15X,A12,1X,A80)', IOSTAT=ERR) FILEC, PATHCR
      LNUM = 7
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

!-----------------------------------------------------------------------
!    Read Experiment Details, Treatments, and Cultivars Sections
!-----------------------------------------------------------------------
      SECTION = '*EXP.D'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEIO, LNUM)
      READ(LUNIO,'(////,3X,A2)', IOSTAT=ERR) CROP ; LNUM = LNUM + 5
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

C-----------------------------------------------------------------------
C    Read Planting Details Section
C-----------------------------------------------------------------------
      SECTION = '*PLANT'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEIO, LNUM)
      READ (LUNIO,70, IOSTAT=ERR) 
     &        PLANTS, PLTPOP, ROWSPC, SDWTPL, ATEMP, PLPH
C   70 FORMAT (18X,2(1X,F5.1),13X,F5.0,12X,1X,F5.0,6X,2(1X,F5.0))
   70 FORMAT (18X,2(F6.0),13X,F5.0,12X,1X,F5.0,6X,2(1X,F5.0))
      LNUM = LNUM + 1
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

      ROWSPC = ROWSPC / 100.0

C-----------------------------------------------------------------------
C     Read crop genetic information
C-----------------------------------------------------------------------
      SECTION = '*CULTI'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEIO, LNUM)
      READ (LUNIO,100, IOSTAT=ERR) P5, G1, G2, G3, PHINT
! 100 FORMAT (42X,F6.0,6X,3F6.0,6X,F6.0)
  100 FORMAT (42X,F6.0,6X,4F6.0)
      LNUM = LNUM + 1
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

      CLOSE (LUNIO)
      RETURN
      END SUBROUTINE RI_IPGROSUB
C=======================================================================
