C=======================================================================
C  COPYRIGHT 1998-2010 The University of Georgia, Griffin, Georgia
C                      University of Florida, Gainesville, Florida
C                      Iowa State University, Ames, Iowa
C                      International Center for Soil Fertility and 
C                       Agricultural Development, Muscle Shoals, Alabama
C                      University of Guelph, Guelph, Ontario
C  ALL RIGHTS RESERVED
C=======================================================================
C=======================================================================
C  CSP_CASUPRO Sugarcane subroutine, based on the
C  CROPGRO, Subroutine, G. Hoogenboom, J.W. Jones, K.J. Boote and 
C  sugarcane-specific modifications by Oscar Daza
C-----------------------------------------------------------------------
C  Based on CROPGRO template plant growth subroutine.
C  Computes plant development and growth.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  01/01/1990 GH  Written.
C  04/24/1994 NBP Changed TAIRHR to TGRO.
C  07/14/1995 JWH Modified to reserve some C for nodule growth
C  09/17/2001 CHP Removed CANNAA, RO and RP from output list.
C  09/21/2001 CHP Added KCAN, PORMIN, RWUEP1, RWUMX to output list.
C  12/03/2001 GH  Changed name to CROPGRO to reflect template approach.
C  02/03/2003 CHP EORATIO and KEP now exported for ET routines.
C  06/19/2003 CHP Added KTRANS for export to transpiration routines instead of KEP.
C  07/08/2003 CHP Added KSEVAP for export to soil evaporation routines.
C  08/10/2003 FSR Modified for Oscar H. Daza sugarcane model CASUPRO
C  07/26/2004 CHP Removed variables which were not being used
C  03/24/2005 CHP Put weather variables in constructed variable. 
C  05/16/2005 FSR Removed calls to NFIX and CSP_MOBIL
C  06/30/2006 FSR Incorporated into CSM v.5
!  01/26/2023 CHP Reduce compile warnings: add EXTERNAL stmts, remove 
!                 unused variables, shorten lines. 
C=======================================================================

      SUBROUTINE CSP_CASUPRO(CONTROL, ISWITCH, 
     &    CO2, EOP, EOS, HARVFRAC, NH4, NO3, PAR,           !Input
     &    SOILPROP, SPi_AVAIL, SW, TAVG, TGRO,              !Input
     &    TGROAV, TMIN, TRWUP, WEATHER, YREND, YRPLT,       !Input
     &    CANHT, EORATIO, HARVRES, KTRANS, LFmntDEF, MDATE, !Output 
     &    NSTRES, PUptake, PORMIN, RLV, RWUMX, SENESCE,     !Output
     &    STGDOY, FracRts, UNH4, UNO3, XHLAI, XLAI)         !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
      USE ModuleData

      IMPLICIT NONE
      EXTERNAL CSP_IPPLNT, CSP_PHENOL, CSP_PHOTO, PEST, CSP_INCOMP, 
     &  CSP_GROW_CANE, CSP_NUPTAK, CSP_SENES, CSP_ROOTS, CSP_OPGROW, 
     &  CSP_OPHARV, P_CASUPRO, CSP_RESPIR, PlantNBal, CSP_HRes
      SAVE
!-----------------------------------------------------------------------
      CHARACTER*1 IDETO, ISWNIT, ISWPHO, ISWSYM, 
     &    ISWWAT, ISWDIS, MEPHO, RNMODE
      CHARACTER*2 CROP
      CHARACTER*4 StalkState(NumOfStalks,10)
      CHARACTER*6 ECONO
      CHARACTER*30 FILEIO
      CHARACTER*10 STNAME(20)
      CHARACTER*92 FILECC, FILEGC
       
      INTEGER, DIMENSION(1:NumOfStalks) :: Kill
      INTEGER, PARAMETER :: NumOfStages = 5, NumOfPhases = 4
      INTEGER, PARAMETER :: CanopyLayers=3

      INTEGER CAB, CountStalk, CropTypeCode, DAS, DYNAMIC 
      INTEGER LAIXD, RUN, ZONE, NLAYR, NOUTDO, NVEG0    
      INTEGER Smax, YREND
      INTEGER YREMRG, YRDOY, MDATE, YRPLT, YRSIM 
      INTEGER STGDOY(20), XDAY(6)

      REAL b, AGEFAC 
      REAL AGRLF, AGRRT, AGRSTM  
      REAL AGRVG, AGRVG2  
      REAL CNODMN, CO2  
      REAL CTONOD, CAVVEG 
      REAL CMINEP 
      REAL CUMDEP
      REAL CNOD,      
     &     CADLF, CADST, CANHT, CANWH, CMINEA 
      REAL DAYL, DWNOD 
      REAL DRPP, DTPI, DTX     
      REAL EOP, EOS, EP1
      REAL FINREF, FRACSH, FRSHV, FRLF, FRRT, FRSTM 
!     REAL FREEZ1, FREEZ2
      REAL GrowFrac, RipeFrac, GROWTH, GRWRES 
      REAL HARVFRAC(2)
      REAL LAIMX, LSFAC, LI, LITOTAL !, LAIX, LITOT, LMF
      REAL MAINR
      REAL NSTRES, NDMNEW,
     &     NGRLF, NGRRT,  NGRST,    
     &    !NADLF,  NMINEA,
     &     NDMTOT, ! NDMSDR, NDMOLD, NDMVEG, 
     &     NFIXN  !NMINEP, NMOBR, NDTH, NODGR  
      REAL NAVL, NAVLV   
      !REAL NRUSLF, NRUSRT, NRUSST
      REAL  
     &    PAR, PCNL, PCNRT, PCNST, PConc_Seed, Ph1P  
      REAL PLIGLF, PLIGNO, PLIGRT, PLIGST, PLWT
      REAL PG, PGT, PCLSD, PCCSD, PCNSD, PCNSH  
      REAL PgAvalPD, PLTPOP, PCH2O,  
     &     PLIGSD, PLF1, PLF2, PLIGSH,         
     &     PROLFI, PRORTI, PROSTI, PROVEG   
      REAL R30C2, RCH2O, RES30C, RLF30C, ROWSPC, RPROAV,
     &     RLIG, RLIP, RMIN, RNITP,    
     &     ROA !, RNH4C, RNO3C, RPRO
      REAL RATTP, RHOL, RO, RP, RTDEP, RHOS   !, ROWSPC
      REAL RSPNO3, RSPNH4   
      REAL SDNPL, SIZELF, TDAY !, SLAVAR

!     Senescence variables computed for soil N routines.
      REAL SENRT(NL)      

      REAL SATFAC, SWFAC, SDWT, SHELWT,  
     &     SLA, SLAMAX, STMWT, SEEDNI, SIZREF
      REAL SLAMIN, SLAPAR, SLAREF, SLPF
      REAL SRDOT, SLAAD, SLNDOT, SSDOT, SSNDOT, StkB, StkM
      REAL TAVG, TDUMX, TGROAV, TMIN, TNLEAK, TOPWT, TOTWT, !TRLV,   
     &     TRNH4U, TRNO3U, TRNU, TRWUP, TURFAC, TURSLA 
      REAL VSTAGE
      REAL WSIDOT, WRIDOT
      REAL WTNCAN, WTNFX, WTNLA, WTNLO, WTNNA 
      REAL WTNNO, WTNNOD, WTNRA, WTNRO, WTNSA, WTNSDA
      REAL WTNSDO, WTNSHA, WTNSHO, WTNSO, WTNUP   
      REAL WLDOTN, WRDOTN,  WSDOTN, WTNRT, WTNSD, WTNSH,   
     &     WTNST, WTNEW, WTNLF, WTLF, WLIDOT  
      REAL XHLAI, XLAI, XLAIlost   

      REAL BD(NL), DLAYR(NL), DS(NL), DUL(NL), ESW(NL), 
     &     KG2PPM(NL), LL(NL), PUptake(NL), SAT(NL), 
     &     SPi_AVAIL(NL), FracRts(NL), 
     &     SW(NL), RLV(NL), WR(NL)            
      REAL NH4(NL), NO3(NL), UNH4(NL), UNO3(NL)
      REAL XSLATM(10), YSLATM(10), PHTHRS(NumOfPhases)
      REAL TGRO(TS)

!     Species-dependant variables exported to SPAM module:
      REAL EORATIO, KCAN, KEP,  KTRANS  
      REAL PORMIN, RWUMX, RWUEP1

!FSR - LIST OF NEW VARIABLES ADDED FOR THE CASUPRO SUGARCANE MODEL
      INTEGER PhenoStage, Stalk, TillerCount
       
      REAL AGRSU, BADMD, BRDMD, CanLmtFac, DLFN, DPLARF     
      REAL FRSU, GAMMA, GRLF, GRRT, GRST, GRSU, LfShdFac, M   
      REAL MinGr, PCARSU, PConc_Shut, PConc_Root, PI1, PI2, PLIGSU 
      REAL PLIPSU, PMINSU, POASU, PROSUI, PSTRES1, PSTRES2  
      REAL StkH2OFac, StkHrNO, SumTTD, SuH2OFac, SumTTG !PHTMAX, 
      REAL XFRRT(4), XFRSU(4), YFRRT(4), YFRSU(4) 
      REAL XLFNUM(7), XSTKRT(6), YLFSZ(7), XStkNum(9) 
      REAL YLfFac(9), YSLA(6), YSTKRT(6), XVSHT(10), YVSHT(10)
      REAL ZVSDI(10)


      REAL, DIMENSION(0:NumOfDays) :: CDEM, LeafAreaPlant, LFWTHa,
     &                LFWTHalost, LSWTHalost, LSWTHa, RTDWP, RTWTHa,  
     &                StalkPopul, STKFWTHa, STKWTHa, 
     &				STKWTHalost, STKWTP, SUWTHa
      REAL, DIMENSION(0:NumOfDays, NumOfStalks) :: DeltaLeafNum,   
     &                DeltaLeafArea, DeltaLeafArealost, LeafArea, 
     &                LeafArealost, LeafNum, LFWT, LFWTlost, 
     &                LSWTlost, LSWT, 
     &                STKmntDEF, STKFWT, STKWT, SumTTStalk, SUWT
      REAL, DIMENSION(1:NumOfStalks) :: EXCESS, MAINTR, PGAVAL, 
     &               PgRatio, StkHt, SuDEF, SLDOT, TOTALWT  
      REAL, DIMENSION(1:NumOfStalks,CanopyLayers) :: LeafDist,LeafMaint,
     &                LFmntDEF,StkPG
!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SoilType) SOILPROP
      TYPE (SwitchType) ISWITCH
      Type (ResidueType) HARVRES
      Type (ResidueType) SENESCE
      Type (WeatherType) WEATHER

!     Transfer values from constructed data types into local variables.
      CROP    = CONTROL % CROP
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      YRDOY   = CONTROL % YRDOY
      DAS     = CONTROL % DAS
      YRSIM   = CONTROL % YRSIM
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE

      BD     = SOILPROP % BD     
      DLAYR  = SOILPROP % DLAYR  
      DS     = SOILPROP % DS     
      DUL    = SOILPROP % DUL    
      KG2PPM = SOILPROP % KG2PPM    
      LL     = SOILPROP % LL     
      NLAYR  = SOILPROP % NLAYR  
      SAT    = SOILPROP % SAT    
      SLPF   = SOILPROP % SLPF  
      WR     = SOILPROP % WR     

      IDETO  = ISWITCH % IDETO
      ISWDIS = ISWITCH % ISWDIS
      ISWNIT = ISWITCH % ISWNIT
      ISWPHO = ISWITCH % ISWPHO
      ISWSYM = ISWITCH % ISWSYM
      ISWWAT = ISWITCH % ISWWAT
      MEPHO  = ISWITCH % MEPHO

      CO2    = WEATHER % CO2   
      DAYL   = WEATHER % DAYL  
      PAR    = WEATHER % PAR   
      TAVG   = WEATHER % TAVG  
      TDAY   = WEATHER % TDAY  
      TGRO   = WEATHER % TGRO  
      TGROAV = WEATHER % TGROAV
      TMIN   = WEATHER % TMIN  

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
!     Call input routine for CASUPRO model parameters
!-----------------------------------------------------------------------
      CALL CSP_IPPLNT(CONTROL,
     &  CAB, CanLmtFac, CROP, ECONO, EORATIO, FILECC,          !Output  
     &  FILEGC, FINREF,                                        !Output
     &  GAMMA, GRLF, GRRT, GRST, GRSU, KCAN,                   !Output
     &  KEP, LfShdFac, LSFAC, NOUTDO, PCARSU, PCH2O, PLF1,     !Output 
     &  PLF2, PLIGSU, PLIPSU, PLWT, PMINSU, POASU, PORMIN,     !Output
     &  PROLFI, PRORTI, PROSTI, PROSUI, R30C2, RCH2O, RES30C,  !Output
     &  RLF30C, RLIG, RLIP, RMIN, ROA, RWUEP1, RWUMX, SLAMAX,  !Output
     &  SLAMIN, SLAPAR,                                        !Output
     &  SIZELF, SIZREF, StkB, StkM, StkH2OFac, SuH2OFac,       !Output
     &  TURSLA, XDAY,                                          !Output
     &  XFRRT, XFRSU, XSLATM, XSTKRT, XVSHT, YFRRT,            !Output
     &  YFRSU, YSLATM, YSLA, YSTKRT, YVSHT, ZVSDI)             !Output

      KTRANS = KEP
!!      KSEVAP = -99.   !Defaults to old method of light
                      !  extinction calculation for soil evap.
!-----------------------------------------------------------------------
      CALL CSP_PHENOL(CONTROL, ISWITCH, FILECC,
     &    DTPI, Kill, LI, M, SOILPROP, TGROAV,               !Input
     &    TURFAC, XLFNUM, XStkNum, YLfFac, YLFSZ, YRDOY,     !Input
     &    YRPLT, YRSIM,                                      !Input
     &    CropTypeCode, DeltaLeafArea, DeltaLeafNum,         !Output
     &    DLFN, DPLARF, DRPP, DTX, TillerCount,              !Output 
     &    LeafNum, MinGr, MDATE,                             !Output
     &    NVEG0, PhenoStage, PHTHRS, Ph1P, PI1, PI2, ROWSPC, !Output
     &    Smax, StalkState, STNAME, STGDOY, StkHrNO,         !Output
     &    SumTTD, SumTTG, SumTTStalk, TDUMX, VSTAGE,         !Output
     &    XLAI, YREMRG)                                      !Output
!-----------------------------------------------------------------------
      IF (CROP .NE. 'FA' .AND. MEPHO .EQ. 'C') THEN
        CALL CSP_PHOTO(CONTROL,FILEIO, 
     &    CANHT, CanLmtFac, CANWH, CO2, ECONO, EXCESS,    !Input
     &    DeltaLeafArea, DeltaLeafNum, FILECC,            !Input 
     &    DeltaLeafArealost, FILEGC, FRACSH, FRSHV,       !Input
     &    KCAN, LeafAreaPlant, LeafNum, LfShdFac, PAR,    !Input 
     &    PStres2, RNITP, SLAREF, SLAAD,                  !Input
     &    SLPF, Smax, StalkState, StkHt, SWFAC, TAVG,     !Input 
     &    TURFAC, WEATHER, XHLAI,                         !Input
     &    AGEFAC, LeafDist, LI, LITOTAL, PGT, PgRatio,    !Output
     &    PLTPOP, StkPG)                                  !Output
      ENDIF
!-----------------------------------------------------------------------
  !    IF (ISWDIS .EQ. 'Y') THEN
  !      CALL PEST(CONTROL, ISWITCH, 
  !   &    AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, PGAVL,    !Input
  !   &    PHTIM, PLTPOP, RTWT, SLA, SLDOT, SOILPROP,      !Input
  !   &    SSDOT, STMWT, TOPWT, WLFDOT, WTLF, YRPLT,       !Input
  !   &    RLV, SDNO, SHELN, SWIDOT,                       !Input/Output
  !   &    VSTAGE, WSHIDT, WTSD, WTSHE,                    !Input/Output
  !   &    ASMDOT, DISLA, NPLTD, PPLTD,                    !Output
  !   &    SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT)             !Output
  !    ENDIF

!-----------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
!        CALL CSP_DEMAND(RUNINIT, 
!     &    AGRLF, AGRRT, AGRSTM, AGRSU, CROP, DLFN, DPLARF,    !Input
!     &    DRPP, FILECC, FILEIO, NSTRES, NVEG0, PAR,           !Input
!     &    PCNL, PCNRT, PCNST, PCNSU, PGAVL, PLTPOP, RPROAV,   !Input
!     &    RTWT, STMWT, SUWT, SWFAC, TAVG, TDUMX,              !Input
!     &    TURFAC, VSTAGE, WCRLF, WCRRT, WCRST, WCRSU, WNRLF,  !Input
!     &    WNRRT, WNRST, WNRSU, WTLF, YRDOY, YRSIM,            !Input
!     &    AGRVG, AGRVG2, F, FNINL, FNINR, FNINS, FRLF,        !Ouput
!     &    FRRT, FRSTM, FRSU, NDMNEW, NDMOLD, NDMTOT,          !Ouput 
!     &    NDMVEG, NMINEP, NMOBR)                              !Ouput 

!-----------------------------------------------------------------------
C    Call plant COMPosition INitialization (for data input)
C-----------------------------------------------------------------------
        CALL CSP_INCOMP(RUNINIT, 
     &    FILECC, FRLF,  FRRT,  FRSTM,  FRSU,             !Input
     &    AGRLF, AGRRT,  AGRSTM, AGRSU, AGRVG, AGRVG2)    !Output

!-----------------------------------------------------------------------
      CALL CSP_GROW_CANE(CONTROL, DYNAMIC,
     &  CAB, CropTypeCode, DeltaLeafArea,                 !Input 
     &  DeltaLeafNum, DTPI, EXCESS, FILEIO,               !Input
     &  FRSU, GAMMA, GRLF, GRRT, GRST,                    !Input
     &  GRSU, Kill, LeafNum, MAINTR, NLAYR, NVEG0,        !Input
     &  PgAvalPD, PGAVAL, PgRatio, PhenoStage,            !Input
     &  PLTPOP, PLF1, PLF2, PLWT, SLA,                    !Input
     &  SLAREF,                                           !Input 
     &  LSFAC, SENRT, SLDOT, Smax, SRDOT, StkHrNO, StalkState, !Input
     &  StkB, StkM, SumTTStalk,                           !Input
     &  TURFAC, XDAY, XFRRT,                              !Input
     &  XFRSU, XLAI, XLAIlost, XSTKRT, YFRRT,             !Input
     &  YFRSU, YRDOY, YRPLT, YSLA, YSTKRT,                !Input
     &  CDEM, FRRT, TOTWT, TOTALWT, XHLAI,                !Input/Output
     &  BRDMD, CountStalk, DeltaLeafArealost,                  !Output 
     &  LAIMX, LAIXD, LeafArea, LeafArealost, LeafAreaPlant,   !Output
     &  LFWT, LFWTHa, LSWTlost, LSWT, LSWTHa, LFWTHalost,      !Output 
     &  LFWTlost, LSWTHalost, StkH2OFac, STKmntDEF,            !Output
     &  RTDWP, RTWTHa, SENESCE, StalkPopul, STKWT, STKFWT,     !Output
     &  STKFWTHa, STKWTHa, STKWTHalost, STKWTP, SuDEF,         !Output
     &  SuH2OFac, SUWT, SUWTHa, TOPWT)                         !Output
!-----------------------------------------------------------------------
        !IF (ISWNIT .EQ. 'Y') THEN
          CALL CSP_NUPTAK(RUNINIT,
     &      DLAYR, DUL, KG2PPM, FILECC, LL, NDMTOT,       !Input
     &      NH4, NLAYR, NO3, RLV, SAT, SW,                !Input
     &      TRNH4U, TRNO3U, TRNU, UNH4, UNO3)             !Output

!-----------------------------------------------------------------------
C        CALL CSP_VEGGR(RUNINIT,
C     &    AGRLF, AGRRT, AGRSTM, AGRSU, CMINEP, CSAVEV,    !Input
C     &    DTX, FILECC, FNINL, FNINR, FNINS, FNINSU, NAVL, !Input
C     &    NDMNEW, NDMOLD, NMINEA, PAR, PCH2O, PG, PGAVL,  !Input
C     &    ROWSPC, STMWT, TRNU, TURFAC, VSTAGE, WCRLF,     !Input
C     &    WCRRT, WCRST, WCRSU, WTLF, XLAI, YRDOY, YRSIM,  !Input
C     &    AGRVG, FRLF, FRRT, FRSTM, FRSU,                 !Input/Output
C     &    CADLF, CADST, CADSU, CMINEA, CRUSLF, CRUSRT,    !Output
C     &    CRUSST, CRUSSU, EXCESS, NADLF, NADRT, NADST,    !Output
C     &    NADSU, NGRLF, NGRRT, NGRST, NGRSU, NSTRES,      !Output
C     &    TNLEAK, WLDOTN, WRDOTN, WSDOTN, WSUDOTN)        !Output

!-----------------------------------------------------------------------
C     Call leaf senescence routine for initialization
C-----------------------------------------------------------------------
      CALL CSP_SENES(CONTROL, DYNAMIC,
     &    CAB, ECONO, FILECC, FILEGC,                 !Input
     &    LeafMaint, LeafNum, LFmntDEF,               !Input
     &    MinGr, Ph1P, PhenoStage,                    !Input
     &    PI1, RATTP, Smax,                           !Input
     &    StalkState, STKmntDEF, STKWT, STKWTP,       !Input
     &    SumTTStalk, SWFAC, TMIN, XLAI,              !Input
     &    YRDOY, YRPLT,                               !Input
     &    Kill, SLDOT, SLNDOT, SSDOT, SSNDOT)         !Output
C-----------------------------------------------------------------------
C     Call to root growth and rooting depth routine
C-----------------------------------------------------------------------
        CALL CSP_ROOTS(CONTROL, DYNAMIC,
     &  AGRRT, CROP, DLAYR, DS, DTX, DUL, ESW, FILECC,       !Input
     &  FRRT, ISWWAT, LL, NLAYR, PGT, PLTPOP, PLWT, RO, RP,  !Input
     &  RTDWP, RTWTHa, SAT, SW, SWFAC,                       !Input
     &  VSTAGE, WR, WRDOTN, WTNEW,                           !Input
     &  CUMDEP, RLV, RTDEP, SATFAC, SENRT, SRDOT)            !Output

        ENDIF
!-----------------------------------------------------------------------
      CALL CSP_OPGROW(CONTROL, ISWITCH, 
     &  BRDMD, CADLF, CADST, CANHT, CANWH, CMINEA,                !Input
     &  CountStalk, DLAYR, EOS, GROWTH, GRWRES,                   !Input
     &  Kill, LeafArea, LeafArealost,LeafNum, LFWTHa,             !Input
     &  LFWT, LFWTHalost, LSWTlost, LSWT, LSWTHa,                 !Input
     &  LSWTHalost, LFWTlost, LITOTAL,                            !Input
     &  MAINR, MDATE, NFIXN, NLAYR, NSTRES, PSTRES1, PSTRES2,     !Input
     &  PCLSD,PCCSD, PCNL, PCNRT, PCNSD, PCNSH, PCNST,            !Input
     &  PGT, RHOL, RHOS, RLV, RTDEP,                              !Input
     &  RTWTHa, SATFAC, SENESCE, SLAREF, Smax, StalkPopul,        !Input
     &  STKFWTHa, STKFWT, STKWT, STKWTHa,                         !Input
     &  STKWTHalost, SumTTD, SumTTG, SWFAC, SUWT, SUWTHa,         !Input
     &  TGRO, TGROAV, TOTWT, TURFAC, WTNCAN, WTNLF, WTNST,        !Input
     &  WTNSD, WTNUP, WTNFX, XLAI, XLAIlost, YRPLT, PhenoStage,   !Input
     &  BADMD)                                                   !Output
!-----------------------------------------------------------------------
!     Initialize Overview.out file.
      PSTRES1 = 1.0
      PSTRES2 = 1.0
      PConc_Shut = 0.0
      PConc_Root = 0.0

      CALL CSP_OPHARV(CONTROL, ISWITCH, 
     &    AGEFAC, BADMD, TillerCount, LAIMX,              !Input
     &    LAIXD, LeafNum, LFWTHa, LSWTHa,                 !Input
     &    HARVFRAC,NSTRES, PLTPOP, PLWT, PStres1,         !Input
     &    PStres2, PhenoStage, StalkPopul, StalkState,    !Input 
     &    STGDOY, STKFWTHa, StkHt, STKWTHa,               !Input
     &    STNAME, SUWTHa, SWFAC, TOPWT, TURFAC,           !Input
     &    WTNCAN, WTNUP, XLAI, YRPLT)                     !Input

!     If this is not a sequenced run, don't use any previously calculated
!       harvest residue.
      IF (RUN .EQ. 1 .OR. INDEX('QF',RNMODE) .LE. 0) THEN
        HARVRES % RESWT  = 0.0
        HARVRES % RESLIG = 0.0
        HARVRES % RESE   = 0.0
      ENDIF

!     Previously uninitialized (from VEGGR -- do we really need these?)
!     chp 2/6/2007
      WSDOTN = 0.0
      WLDOTN = 0.0
      NGRST  = 0.0
      NGRLF  = 0.0
      NGRRT  = 0.0
      NDMNEW = 0.0
      
!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!***********************************************************************
!     The following statements came from INPLNT
!-----------------------------------------------------------------------
      CMINEP = 0.0
      CNOD   = 0.0
      CNODMN = 0.0
      CTONOD = 0.0
      NAVL   = 0.0
      PgAvalPD = 0.0	

      RO     = 0.0
      RP     = 0.0
      RPROAV = 0.0
!     from SWFACS
      TURFAC = 1.0
      SWFAC  = 1.0

      RSPNO3 = 0.0   !Added by CHP 10-17-2003
      RSPNH4 = 0.0   !

      PSTRES1 = 1.0
      PSTRES2 = 1.0

      STGDOY = 9999999

      DO Stalk = 1, NumOfStalks
         DO Zone = 1, 3
            LFmntDEF(Stalk,Zone) =  0.    ! Initialize 			 
         END DO
      END DO

!-----------------------------------------------------------------------
      CALL CSP_PHENOL(CONTROL, ISWITCH, FILECC,
     &    DTPI, Kill, LI, M, SOILPROP, TGROAV,               !Input
     &    TURFAC, XLFNUM, XStkNum, YLfFac, YLFSZ, YRDOY,     !Input
     &    YRPLT, YRSIM,                                      !Input
     &    CropTypeCode, DeltaLeafArea, DeltaLeafNum,         !Output
     &    DLFN, DPLARF, DRPP, DTX, TillerCount,              !Output 
     &    LeafNum, MinGr, MDATE,                             !Output
     &    NVEG0, PhenoStage, PHTHRS, Ph1P, PI1, PI2, ROWSPC, !Output
     &    Smax, StalkState, STNAME, STGDOY, StkHrNO,         !Output
     &    SumTTD, SumTTG, SumTTStalk, TDUMX, VSTAGE,         !Output
     &    XLAI, YREMRG)                                      !Output
!-----------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
        IF (MEPHO .EQ. 'L') THEN
          !Retrieve AGEFAC and PG from ETPHOT routine.
          CALL GET('SPAM', 'AGEFAC', AGEFAC)
          CALL GET('SPAM', 'PG'    , PG)
        ELSEIF (MEPHO .EQ. 'C') THEN
          CALL CSP_PHOTO(CONTROL,FILEIO, 
     &    CANHT, CanLmtFac, CANWH, CO2, ECONO, EXCESS,    !Input
     &    DeltaLeafArea, DeltaLeafNum, FILECC,            !Input 
     &    DeltaLeafArealost, FILEGC, FRACSH, FRSHV,       !Input
     &    KCAN, LeafAreaPlant, LeafNum, LfShdFac, PAR,    !Input 
     &    PStres2, RNITP, SLAREF, SLAAD,                  !Input
     &    SLPF, Smax, StalkState, StkHt, SWFAC, TAVG,     !Input 
     &    TURFAC, WEATHER, XHLAI,                         !Input
     &    AGEFAC, LeafDist, LI, LITOTAL, PGT, PgRatio,    !Output
     &    PLTPOP, StkPG)                                  !Output
       ENDIF
      ENDIF
!-----------------------------------------------------------------------
C     Initialize pest coupling point and damage variables
!     Need to initialize even if pests are not being modelled this
!     season - zero out values from last simulation.
!-----------------------------------------------------------------------
   !     CALL PEST(CONTROL, ISWITCH, 
   !  &    AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, PGAVL,    !Input
   !  &    PHTIM, PLTPOP, RTWT, SLA, SLDOT, SOILPROP,      !Input
   !  &    SSDOT, STMWT, TOPWT, WLFDOT, WTLF, YRPLT,       !Input
   !  &    RLV, SDNO, SHELN, SWIDOT,                       !Input/Output
   !  &    VSTAGE, WSHIDT, WTSD, WTSHE,                    !Input/Output
   !  &    ASMDOT, DISLA, NPLTD, PPLTD,                    !Output
   !  &    SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT)             !Output
!-----------------------------------------------------------------------
!     Call plant COMPosition INitialization
!     This call must preceed initialization call to GROW (need to
!         initialize value of SDPROR for use in that routine) chp 9/22/98
!-----------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
        CALL CSP_INCOMP(SEASINIT,
     &    FILECC, FRLF,  FRRT,  FRSTM,  FRSU,             !Input
     &    AGRLF, AGRRT,  AGRSTM, AGRSU, AGRVG, AGRVG2)    !Output
      ENDIF
 
!-----------------------------------------------------------------------
      CALL CSP_GROW_CANE(CONTROL, DYNAMIC,
     &  CAB, CropTypeCode, DeltaLeafArea,                 !Input 
     &  DeltaLeafNum, DTPI, EXCESS, FILEIO,               !Input
     &  FRSU, GAMMA, GRLF, GRRT, GRST,                    !Input
     &  GRSU, Kill, LeafNum, MAINTR, NLAYR, NVEG0,        !Input
     &  PgAvalPD, PGAVAL, PgRatio, PhenoStage,            !Input
     &  PLTPOP, PLF1, PLF2, PLWT, SLA,                    !Input
     &  SLAREF,                                           !Input 
     &  LSFAC, SENRT, SLDOT, Smax, SRDOT, StkHrNO, StalkState, !Input
     &  StkB, StkM, SumTTStalk,                           !Input
     &  TURFAC, XDAY, XFRRT,                              !Input
     &  XFRSU, XLAI, XLAIlost, XSTKRT, YFRRT,             !Input
     &  YFRSU, YRDOY, YRPLT, YSLA, YSTKRT,                !Input
     &  CDEM, FRRT, TOTWT, TOTALWT, XHLAI,                !Input/Output
     &  BRDMD, CountStalk, DeltaLeafArealost,                  !Output 
     &  LAIMX, LAIXD, LeafArea, LeafArealost, LeafAreaPlant,   !Output
     &  LFWT, LFWTHa, LSWTlost, LSWT, LSWTHa, LFWTHalost,      !Output 
     &  LFWTlost, LSWTHalost, StkH2OFac, STKmntDEF,            !Output
     &  RTDWP, RTWTHa, SENESCE, StalkPopul, STKWT, STKFWT,     !Output
     &  STKFWTHa, STKWTHa, STKWTHalost, STKWTP, SuDEF,         !Output
     &  SuH2OFac, SUWT, SUWTHa, TOPWT)                         !Output
!-----------------------------------------------------------------------
        CALL CSP_NUPTAK(SEASINIT,
     &    DLAYR, DUL, KG2PPM, FILECC, LL, NDMTOT,         !Input
     &    NH4, NLAYR, NO3, RLV, SAT, SW,                  !Input
     &    TRNH4U, TRNO3U, TRNU, UNH4, UNO3)               !Output

!-----------------------------------------------------------------------
C     Call leaf senescence routine for initialization
C-----------------------------------------------------------------------
      CALL CSP_SENES(CONTROL, DYNAMIC,
     &    CAB, ECONO, FILECC, FILEGC,                 !Input
     &    LeafMaint, LeafNum, LFmntDEF,               !Input
     &    MinGr, Ph1P, PhenoStage,                    !Input
     &    PI1, RATTP, Smax,                           !Input
     &    StalkState, STKmntDEF, STKWT, STKWTP,       !Input
     &    SumTTStalk, SWFAC, TMIN, XLAI,              !Input
     &    YRDOY, YRPLT,                               !Input
     &    Kill, SLDOT, SLNDOT, SSDOT, SSNDOT)         !Output
C-----------------------------------------------------------------------
C     Call to root growth and rooting depth routine
C-----------------------------------------------------------------------
      CALL CSP_ROOTS(CONTROL, DYNAMIC,
     &  AGRRT, CROP, DLAYR, DS, DTX, DUL, ESW, FILECC,       !Input
     &  FRRT, ISWWAT, LL, NLAYR, PGT, PLTPOP, PLWT, RO, RP,  !Input
     &  RTDWP, RTWTHa, SAT, SW, SWFAC,                       !Input
     &  VSTAGE, WR, WRDOTN, WTNEW,                           !Input
     &  CUMDEP, RLV, RTDEP, SATFAC, SENRT, SRDOT)            !Output

C-----------------------------------------------------------------------
C     Plant phosphorus module initialization
C-----------------------------------------------------------------------
      CALL P_CASUPRO (CONTROL, DYNAMIC, ISWITCH,    !Input
     &    CROP, FILECC, GrowFrac, LFWTHa, MDATE,          !Input
     &    PLTPOP, RipeFrac, RLV, RTDEP, RTWTHa,           !Input
     &    SOILPROP,SPi_AVAIL, STKWTHa,                    !Input
     &    WLIDOT, WRIDOT, WSIDOT, YRPLT,                  !Input
     &    SENESCE,                                        !I/O
     &    PConc_Shut, PConc_Root, PConc_Seed,             !Output
     &    PStres1, PStres2, PUptake, FracRts)             !Output

!-----------------------------------------------------------------------
!     Write headings to output file GROWTH.OUT
!-----------------------------------------------------------------------
      CALL CSP_OPGROW(CONTROL, ISWITCH, 
     &  BRDMD, CADLF, CADST, CANHT, CANWH, CMINEA,                !Input
     &  CountStalk, DLAYR, EOS, GROWTH, GRWRES,                   !Input
     &  Kill, LeafArea, LeafArealost,LeafNum, LFWTHa,             !Input
     &  LFWT, LFWTHalost, LSWTlost, LSWT, LSWTHa,                 !Input
     &  LSWTHalost, LFWTlost, LITOTAL,                            !Input
     &  MAINR, MDATE, NFIXN, NLAYR, NSTRES, PSTRES1, PSTRES2,     !Input
     &  PCLSD,PCCSD, PCNL, PCNRT, PCNSD, PCNSH, PCNST,            !Input
     &  PGT, RHOL, RHOS, RLV, RTDEP,                              !Input
     &  RTWTHa, SATFAC, SENESCE, SLAREF, Smax, StalkPopul,        !Input
     &  STKFWTHa, STKFWT, STKWT, STKWTHa,                         !Input
     &  STKWTHalost, SumTTD, SumTTG, SWFAC, SUWT, SUWTHa,         !Input
     &  TGRO, TGROAV, TOTWT, TURFAC, WTNCAN, WTNLF, WTNST,        !Input
     &  WTNSD, WTNUP, WTNFX, XLAI, XLAIlost, YRPLT, PhenoStage,   !Input
     &  BADMD)                                                   !Output
!-----------------------------------------------------------------------    

      CALL CSP_OPHARV(CONTROL, ISWITCH, 
     &    AGEFAC, BADMD, TillerCount, LAIMX,              !Input
     &    LAIXD, LeafNum, LFWTHa, LSWTHa,                 !Input
     &    HARVFRAC,NSTRES, PLTPOP, PLWT, PStres1,         !Input
     &    PStres2, PhenoStage, StalkPopul, StalkState,    !Input 
     &    STGDOY, STKFWTHa, StkHt, STKWTHa,               !Input
     &    STNAME, SUWTHa, SWFAC, TOPWT, TURFAC,           !Input
     &    WTNCAN, WTNUP, XLAI, YRPLT)                     !Input

! Zero the value of HARVRES composite variable here 
! NOTE: At this time, the variable has already been used to 
!     initialize soil properties for this season.
        HARVRES % RESWT  = 0.0
        HARVRES % RESLIG = 0.0
        HARVRES % RESE   = 0.0

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. RATE) THEN
!***********************************************************************

      IF (YRDOY .GT. YRPLT .AND. YRPLT .NE. -99) THEN 

         IF (YRDOY .GT. YREMRG .AND. YREMRG .GT. 0 
     &                    .AND. ISWWAT .EQ. 'Y') THEN
!          Calculate daily water stress factors
           SWFAC  = 1.0
           TURFAC = 1.0

            IF (EOP .GT. 0.001) THEN
                EP1 = EOP * 0.1  ! convert mm/d to cm/d
                IF (TRWUP / EP1 .LT. RWUEP1) THEN
                  TURFAC = (1./RWUEP1) * TRWUP / EP1
                ENDIF
                IF (EP1 .GE. TRWUP) THEN
                  SWFAC = TRWUP / EP1
                ENDIF
            ENDIF
         ENDIF
! EOP      Potential plant transpiration rate (mm/d)
! RWUEP1   Threshold for reducing leaf expansion compared w/ ratio of 
!            TRWU/EP1 (total potenial daily root water uptake/ actual 
!            transpiration)
! TRWUP    Potential daily root water uptake over soil profile (cm/d)

!-----------------------------------------------------------------------
!     CALL vegetative and reproductive development subroutine
!----------------------------------------------------------------------
  !    IF (ISWDIS .EQ. 'Y') THEN
  !      CALL PEST(CONTROL, ISWITCH, 
  !   &    AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, PGAVL,    !Input
  !   &    PHTIM, PLTPOP, RTWT, SLA, SLDOT, SOILPROP,      !Input
  !   &    SSDOT, STMWT, TOPWT, WLFDOT, WTLF, YRPLT,       !Input
  !   &    RLV, SDNO, SHELN, SWIDOT,                       !Input/Output
  !   &    VSTAGE, WSHIDT, WTSD, WTSHE,                    !Input/Output
  !   &    ASMDOT, DISLA, NPLTD, PPLTD,                    !Output
  !   &    SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT)             !Output
  !    ENDIF
!-----------------------------------------------------------------------
      CALL CSP_PHENOL(CONTROL, ISWITCH, FILECC,
     &    DTPI, Kill, LI, M, SOILPROP, TGROAV,               !Input
     &    TURFAC, XLFNUM, XStkNum, YLfFac, YLFSZ, YRDOY,     !Input
     &    YRPLT, YRSIM,                                      !Input
     &    CropTypeCode, DeltaLeafArea, DeltaLeafNum,         !Output
     &    DLFN, DPLARF, DRPP, DTX, TillerCount,              !Output 
     &    LeafNum, MinGr, MDATE,                             !Output
     &    NVEG0, PhenoStage, PHTHRS, Ph1P, PI1, PI2, ROWSPC, !Output
     &    Smax, StalkState, STNAME, STGDOY, StkHrNO,         !Output
     &    SumTTD, SumTTG, SumTTStalk, TDUMX, VSTAGE,         !Output
     &    XLAI, YREMRG)                                      !Output
!----------------------------------------------------------------------
      IF (CROP .NE. 'FA' .AND. DAS .GT. NVEG0) THEN
        IF (MEPHO .EQ. 'L') THEN
          !Retrieve AGEFAC and PG from ETPHOT routine.
          CALL GET('SPAM', 'AGEFAC', AGEFAC)
          CALL GET('SPAM', 'PG'    , PG)
        ELSEIF (MEPHO .EQ. 'C') THEN
          CALL CSP_PHOTO(CONTROL,FILEIO, 
     &    CANHT, CanLmtFac, CANWH, CO2, ECONO, EXCESS,    !Input
     &    DeltaLeafArea, DeltaLeafNum, FILECC,            !Input 
     &    DeltaLeafArealost, FILEGC, FRACSH, FRSHV,       !Input
     &    KCAN, LeafAreaPlant, LeafNum, LfShdFac, PAR,    !Input 
     &    PStres2, RNITP, SLAREF, SLAAD,                  !Input
     &    SLPF, Smax, StalkState, StkHt, SWFAC, TAVG,     !Input 
     &    TURFAC, WEATHER, XHLAI,                         !Input
     &    AGEFAC, LeafDist, LI, LITOTAL, PGT, PgRatio,    !Output
     &    PLTPOP, StkPG)                                  !Output
        ENDIF
      ENDIF
!-----------------------------------------------------------------------
      CALL CSP_GROW_CANE(CONTROL, DYNAMIC,
     &  CAB, CropTypeCode, DeltaLeafArea,                 !Input 
     &  DeltaLeafNum, DTPI, EXCESS, FILEIO,               !Input
     &  FRSU, GAMMA, GRLF, GRRT, GRST,                    !Input
     &  GRSU, Kill, LeafNum, MAINTR, NLAYR, NVEG0,        !Input
     &  PgAvalPD, PGAVAL, PgRatio, PhenoStage,            !Input
     &  PLTPOP, PLF1, PLF2, PLWT, SLA,                    !Input
     &  SLAREF,                                           !Input 
     &  LSFAC, SENRT, SLDOT, Smax, SRDOT, StkHrNO, StalkState, !Input
     &  StkB, StkM, SumTTStalk,                           !Input
     &  TURFAC, XDAY, XFRRT,                              !Input
     &  XFRSU, XLAI, XLAIlost, XSTKRT, YFRRT,             !Input
     &  YFRSU, YRDOY, YRPLT, YSLA, YSTKRT,                !Input
     &  CDEM, FRRT, TOTWT, TOTALWT, XHLAI,                !Input/Output
     &  BRDMD, CountStalk, DeltaLeafArealost,                  !Output 
     &  LAIMX, LAIXD, LeafArea, LeafArealost, LeafAreaPlant,   !Output
     &  LFWT, LFWTHa, LSWTlost, LSWT, LSWTHa, LFWTHalost,      !Output 
     &  LFWTlost, LSWTHalost, StkH2OFac, STKmntDEF,            !Output
     &  RTDWP, RTWTHa, SENESCE, StalkPopul, STKWT, STKFWT,     !Output
     &  STKFWTHa, STKWTHa, STKWTHalost, STKWTP, SuDEF,         !Output
     &  SuH2OFac, SUWT, SUWTHa, TOPWT)                         !Output
!----------------------------------------------------------------------
      ENDIF
!***********************************************************************
!***********************************************************************
!     DAILY INTEGRATION 
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. INTEGR .AND. CROP .NE. 'FA') THEN
!***********************************************************************
!-----------------------------------------------------------------------
!     Move CSP_PHENOL integration up here.
!     Need to set NVEG0 before test for DAS = NVEG0, otherwise,
!     initialization on day of emergence will never occur.
!-----------------------------------------------------------------------
      CALL CSP_PHENOL(CONTROL, ISWITCH, FILECC,
     &    DTPI, Kill, LI, M, SOILPROP, TGROAV,               !Input
     &    TURFAC, XLFNUM, XStkNum, YLfFac, YLFSZ, YRDOY,     !Input
     &    YRPLT, YRSIM,                                      !Input
     &    CropTypeCode, DeltaLeafArea, DeltaLeafNum,         !Output
     &    DLFN, DPLARF, DRPP, DTX, TillerCount,              !Output 
     &    LeafNum, MinGr, MDATE,                             !Output
     &    NVEG0, PhenoStage, PHTHRS, Ph1P, PI1, PI2, ROWSPC, !Output
     &    Smax, StalkState, STNAME, STGDOY, StkHrNO,         !Output
     &    SumTTD, SumTTG, SumTTStalk, TDUMX, VSTAGE,         !Output
     &    XLAI, YREMRG)                                      !Output
!-----------------------------------------------------------------------
      IF (DAS .EQ. NVEG0+1) THEN
!-----------------------------------------------------------------------
!     On day of emergence, initialize:
!-----------------------------------------------------------------------
!!!      CALL CSP_GROW_CANE(CONTROL, DYNAMIC,
!!!     &  AGEFAC,  CAB, CropTypeCode, DeltaLeafArea,             !Input
!!!     &  DeltaLeafNum, DTX, EXCESS, FILECC, FILEGC, FILEIO,     !Input
!!!     &  FINREF, FRSU, GAMMA, GRLF, GRRT, GRST, GRSU, Kill,     !Input
!!!     &  LeafNum, NVEG0, PAR, PgAvalPD, PGAVL,                  !Input
!!!     &  PGAVAL, PgRatio, PhenoStage, PLTPOP, PLF, PLWT,        !Input
!!!     &  SIZELF, SLA, SLAMIN, SIZREF, SLAMAX, SLAPAR, SLAREF,   !Input
!!!     &  SLAVAR, SLDOT, Smax, StkHrNO, StalkState,              !Input
!!!     &  StkB, StkM, TGRO, TURFAC, TURSLA, XFRRT,               !Input
!!!     &  XFRSU, XLAI, XLAIlost, XSLATM, XSTKRT, YFRRT,          !Input
!!!     &  YFRSU, YRDOY, YRPLT, YSTKRT,                           !Input
!!!     &  CDEM, FRRT, TOTWT, TOTALWT, XHLAI, YSLATM,       !Input/Output
!!!     &  CountStalk, DeltaLeafArealost, LAIMX, LeafArea,        !Output 
!!!     &  LeafArealost, LeafAreaPlant, LFWT, LFWTHa, LFWTHalost, !Output
!!!     &  LFWTlost, MntDEF, RTDWP, RTWTHa, SENESCE, StalkPopul,  !Output
!!!     &  STKWT, STKWTHa, STKWTHalost, STKWTP, SUWT, SUWTHa,     !Output
!!!     &  TOPWT)                                                 !Output
!-----------------------------------------------------------------------
C     Call to root growth and rooting depth routine
C-----------------------------------------------------------------------
          CALL CSP_ROOTS(CONTROL, EMERG,
     &  AGRRT, CROP, DLAYR, DS, DTX, DUL, ESW, FILECC,       !Input
     &  FRRT, ISWWAT, LL, NLAYR, PGT, PLTPOP, PLWT, RO, RP,  !Input
     &  RTDWP, RTWTHa, SAT, SW, SWFAC,                       !Input
     &  VSTAGE, WR, WRDOTN, WTNEW,                           !Input
     &  CUMDEP, RLV, RTDEP, SATFAC, SENRT, SRDOT)            !Output
!-----------------------------------------------------------------------
      ENDIF
!----------------------------------------------------------------------
!***********************************************************************
C     Skip growth processes and N and C balances before plants emerge
C-----------------------------------------------------------------------
      IF (DAS .GT. NVEG0) THEN
!-----------------------------------------------------------------------
!     Initialize available N and C for beginning of daily calcs.
!-----------------------------------------------------------------------
      NAVL = 0.0
!!      PGAVL = 0.0
      PgAvalPD = 0.0
C-----------------------------------------------------------------------
C    Initialize variables that represent N and C availability during a day
C    Assume that Fraction CMOBMX of CH2O can be Mobilized per Day
C    PGAVL is the total available CH2O available for growth & respiration
C
C    8/26/97 KJB  DTX IN PLACE OF 1 TO SLOW IT DOWN A BIT AT ALL TIMES
C    AND TO BE SENSITIVE TO TEMPERATURE PRIOR TO R5 STAGE, BUT
C    STILL WANT THE SPEED-UP CAUSED BY THE "+ DXR57" FEATURE AFTER R5.
 
!CMINEP Potential CH2O mobilization from storage removed for CASUPRO by FSR  
!     CMINEP = CMOBMX * DTX * (WCRLF + WCRST + WCRRT + WCRSU)

!!      PGAVL = PG !+ CMINEP
C-----------------------------------------------------------------------
      IF (MEPHO .EQ. 'C') THEN
        CALL CSP_PHOTO(CONTROL,FILEIO, 
     &    CANHT, CanLmtFac, CANWH, CO2, ECONO, EXCESS,    !Input
     &    DeltaLeafArea, DeltaLeafNum, FILECC,            !Input 
     &    DeltaLeafArealost, FILEGC, FRACSH, FRSHV,       !Input
     &    KCAN, LeafAreaPlant, LeafNum, LfShdFac, PAR,    !Input 
     &    PStres2, RNITP, SLAREF, SLAAD,                  !Input
     &    SLPF, Smax, StalkState, StkHt, SWFAC, TAVG,     !Input 
     &    TURFAC, WEATHER, XHLAI,                         !Input
     &    AGEFAC, LeafDist, LI, LITOTAL, PGT, PgRatio,    !Output
     &    PLTPOP, StkPG)                                  !Output
      ENDIF
C-----------------------------------------------------------------------
C       Compute maintenance respiration and subtract from available CH2O
C-----------------------------------------------------------------------
      CALL CSP_RESPIR(CONTROL,
     &    LeafArea, LeafDist, LFWT, R30C2,            !Input
     &	RES30C, RLF30C, SLAREF, StalkState,         !Input
     &    StkPG, SUWT, TGRO, TOTALWT,                 !Input
     &    RO, RP,                                     !Input/Output
     &    LeafMaint, MAINTR)                          !Output
C-----------------------------------------------------------------------
      DO Stalk = 1, NumOfStalks

         IF (StalkState(Stalk,1) .EQ. 'LIVE') THEN 

! Remove leaf area maintenance respiration costs from photosynthate 
! production for each canopy zone on each stalk.  If PG remains, add 
! to PGAVAL for the stalk; if there is insufficient PG for leaf maint,
! the deficit is recorded to drive leaf senescence, and PGAVAL is left
! at zero. 

          PGAVAL(Stalk) = 0.

          DO Zone = 1, 3

            b = StkPG(Stalk,Zone) - LeafMaint(Stalk,Zone)
             
            IF (b >= 0.) THEN                           

               PGAVAL(Stalk) = PGAVAL(Stalk) + b

                 LFmntDEF(Stalk,Zone) = 0.          

            ELSE                                      

               LFmntDEF(Stalk,Zone) = b

            END IF  
             
          END DO

! PGAVAL is reduced by stalk (without leaves) maintenance respiration 
! costs.  Result can be positive or negative.
 
            PGAVAL(Stalk) = PGAVAL(Stalk) - MAINTR(Stalk) 


         ELSE ! Stalk not alive 

            PGAVAL(Stalk) = 0.0 

         END IF  ! (StalkState(Stalk,1) .EQ. 'LIVE')

        PgAvalPD = PgAvalPD + (PGAVAL(Stalk) * PLTPOP) 
        PgAvalPD = PgAvalPD   ! temporary for de-bug - FSR

      END DO  

!-----------------------------------------------------------------------
!     Reduce PGAVL if pest damage occurs to C assimilation
!     Moved to PEST module - chp
!-----------------------------------------------------------------------
!     Call PEST Module for INTEGRATION calculations
!-----------------------------------------------------------------------
  !    IF (ISWDIS.EQ.'Y') THEN
  !      CALL PEST(CONTROL, ISWITCH, 
  !   &    AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, PGAVL,    !Input
  !   &    PHTIM, PLTPOP, RTWT, SLA, SLDOT, SOILPROP,      !Input
  !   &    SSDOT, STMWT, TOPWT, WLFDOT, WTLF, YRPLT,       !Input
  !   &    RLV, SDNO, SHELN, SWIDOT,                       !Input/Output
  !   &    VSTAGE, WSHIDT, WTSD, WTSHE,                    !Input/Output
  !   &    ASMDOT, DISLA, NPLTD, PPLTD,                    !Output
  !   &    SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT)             !Output
  !
  !      IF (ASMDOT .GT. 1.E-4) THEN
  !          PgAvalPD = PgAvalPD - ASMDOT
  !          PgAvalPD = MAX(0.0, PgAvalPD) 
  !      ENDIF
  !
  !    ENDIF

C-----------------------------------------------------------------------
C    Compute N Available From Seed, During Early Growth
!     chp - this takes much longer than 7 ptd to deplete seed N.
C-----------------------------------------------------------------------
        NAVL = MAX(SDNPL * DTX / 7. , 0.0)
        SDNPL = SDNPL - NAVL
        
C-----------------------------------------------------------------------
C    If ISWNIT = Y - Call soil N routines. Balance Available C and N
C    If ISWNIT = N - Do not call soil N routines, N assumed to be limited by C
C-----------------------------------------------------------------------
      IF (ISWNIT .EQ. 'Y') THEN
        CALL CSP_NUPTAK(INTEGR,
     &    DLAYR, DUL, KG2PPM, FILECC, LL, NDMTOT,         !Input
     &    NH4, NLAYR, NO3, RLV, SAT, SW,                  !Input
     &    TRNH4U, TRNO3U, TRNU, UNH4, UNO3)               !Output

C-----------------------------------------------------------------------
C    Account for C Used to reduce N Uptake to protein
C-----------------------------------------------------------------------
!        RSPNO3 = TRNO3U/0.16 * RNO3C
!        RSPNH4 = TRNH4U/0.16 * RNH4C

        IF (PgAvalPD .LT. (RSPNO3+RSPNH4)) THEN
          PgAvalPD = 0.0
        ELSE
          PgAvalPD = PgAvalPD - (RSPNO3 + RSPNH4)
        ENDIF

C-----------------------------------------------------------------------
C       Accumulate nitrogen for today's growth, NAVL
C-----------------------------------------------------------------------
        NAVL = NAVL + TRNU
      ELSE
!OHD - Provide all the N demanded (Potential Production)
          NAVL = NAVL + NDMNEW
      ENDIF
C-----------------------------------------------------------------------
C    CALL Nitrogen mobilization subroutine
C    to compute availability of N from other tissue (NMINEA)
C-----------------------------------------------------------------------
C    Accumulate NAVL for growth, reduce PGAVL by protein re-synthesis cost
C    Followning 6 lines Removed by FSR (May-18-2005)
C-----------------------------------------------------------------------
!      IF (PGAVL .GT. NMINEA/0.16*RPRO) THEN  
!         PGAVL = PGAVL - NMINEA/0.16*RPRO
!      ELSE
!         PGAVL = 0.0
!      ENDIF
!      NAVL   = NAVL + NMINEA
C-----------------------------------------------------------------------
C     NAVLV = N available for veg growth from uptake
C     CAVVEG = C available for veg growth
C     NDMVEG = N required for veg growth if all PGAVL is used as computed
C     PROVEG = average protein composition of growing tissue today
C-----------------------------------------------------------------------
      CAVVEG = MAX(0.,PgAvalPD)             ! Changes for CASUPRO (FSR)
      NAVLV = MAX(0.,NAVL)                  ! Changes for CASUPRO (FSR)
      IF(CAVVEG .GT. 0.0) THEN
        PROVEG = PROLFI * FRLF + PRORTI * FRRT + PROSTI * FRSTM + 
     &           PROSUI * FRSU              ! Changes for CASUPRO (FSR)
      ENDIF

C-----------------------------------------------------------------------
C     Call routine to compute actual vegetative growth, C to mine or add
C-----------------------------------------------------------------------
C      CALL CSP_VEGGR(INTEGR,
C     &    AGRLF, AGRRT, AGRSTM, AGRSU, CMINEP, CSAVEV,    !Input
C     &    DTX, FILECC, FNINL, FNINR, FNINS, FNINSU, NAVL, !Input
C     &    NDMNEW, NDMOLD, NMINEA, PAR, PCH2O, PG, PGAVL,  !Input
C     &    ROWSPC, STMWT, TRNU, TURFAC, VSTAGE, WCRLF,     !Input
C     &    WCRRT, WCRST, WCRSU, WTLF, XLAI, YRDOY, YRSIM,  !Input
C     &    AGRVG, FRLF, FRRT, FRSTM, FRSU,                 !Input/Output
C     &    CADLF, CADST, CADSU, CMINEA, CRUSLF, CRUSRT,    !Output
C     &    CRUSST, CRUSSU, EXCESS, NADLF, NADRT, NADST,    !Output
C     &    NADSU, NGRLF, NGRRT, NGRST, NGRSU, NSTRES,      !Output
C     &    TNLEAK, WLDOTN, WRDOTN, WSDOTN, WSUDOTN)        !Output
C-----------------------------------------------------------------------
C     Compute C required for LF, ST, and RT growth, and remaining C and N
C-----------------------------------------------------------------------
      PgAvalPD = PgAvalPD - AGRVG * (WLDOTN + WSDOTN + WRDOTN)
      NAVL = NAVL - (NGRLF + NGRST + NGRRT)
      PgAvalPD = PgAvalPD - (CADST + CADLF) * PCH2O
C-----------------------------------------------------------------------
C  Since Veggr was called here, and CSP_GROW_CANE replaced Veggr, it is
C  now called here.  
C-----------------------------------------------------------------------
!!      IF (MEPHO .EQ. 'C') THEN
!!        CALL CSP_PHOTO(CONTROL, 
!!     &    CO2, EXCESS, KCAN, LeafArea, DeltaLeafArea,     !Input
!!     &    DeltaLeafArealost, LeafAreaPlant, LeafNum,      !Input
!!     &    SLPF, PAR, PStres2, RNITP, SLA, SLAAD,          !Input
!!     &    StalkState, Smax, SWFAC, TAVG, XHLAI,           !Input
!!     &    AGEFAC, LI, PG, PgRatio, PLTPOP, StkPG)         !Output
!!      ENDIF
C-----------------------------------------------------------------------
      CALL CSP_GROW_CANE(CONTROL, DYNAMIC,
     &  CAB, CropTypeCode, DeltaLeafArea,                 !Input 
     &  DeltaLeafNum, DTPI, EXCESS, FILEIO,               !Input
     &  FRSU, GAMMA, GRLF, GRRT, GRST,                    !Input
     &  GRSU, Kill, LeafNum, MAINTR, NLAYR, NVEG0,        !Input
     &  PgAvalPD, PGAVAL, PgRatio, PhenoStage,            !Input
     &  PLTPOP, PLF1, PLF2, PLWT, SLA,                    !Input
     &  SLAREF,                                           !Input 
     &  LSFAC, SENRT, SLDOT, Smax, SRDOT, StkHrNO, StalkState, !Input
     &  StkB, StkM, SumTTStalk,                           !Input
     &  TURFAC, XDAY, XFRRT,                              !Input
     &  XFRSU, XLAI, XLAIlost, XSTKRT, YFRRT,             !Input
     &  YFRSU, YRDOY, YRPLT, YSLA, YSTKRT,                !Input
     &  CDEM, FRRT, TOTWT, TOTALWT, XHLAI,                !Input/Output
     &  BRDMD, CountStalk, DeltaLeafArealost,                  !Output 
     &  LAIMX, LAIXD, LeafArea, LeafArealost, LeafAreaPlant,   !Output
     &  LFWT, LFWTHa, LSWTlost, LSWT, LSWTHa, LFWTHalost,      !Output 
     &  LFWTlost, LSWTHalost, StkH2OFac, STKmntDEF,            !Output
     &  RTDWP, RTWTHa, SENESCE, StalkPopul, STKWT, STKFWT,     !Output
     &  STKFWTHa, STKWTHa, STKWTHalost, STKWTP, SuDEF,         !Output
     &  SuH2OFac, SUWT, SUWTHa, TOPWT)                         !Output
!-----------------------------------------------------------------------

C     Call leaf senescence routine to compute leaf loss variables
C-----------------------------------------------------------------------
      CALL CSP_SENES(CONTROL, DYNAMIC,
     &    CAB, ECONO, FILECC, FILEGC,                 !Input
     &    LeafMaint, LeafNum, LFmntDEF,               !Input
     &    MinGr, Ph1P, PhenoStage,                    !Input
     &    PI1, RATTP, Smax,                           !Input
     &    StalkState, STKmntDEF, STKWT, STKWTP,       !Input
     &    SumTTStalk, SWFAC, TMIN, XLAI,              !Input
     &    YRDOY, YRPLT,                               !Input
     &    Kill, SLDOT, SLNDOT, SSDOT, SSNDOT)         !Output
C-----------------------------------------------------------------------
C     Call frost damage routine if TMIN is less than FREEZ1 deg C
C-----------------------------------------------------------------------
! Note: This routine was eliminated in favor of a section within 
!       CSP_SENES.
!!!      IF (TMIN .LT. FREEZ1) THEN
!!!        CALL CSP_FROST(
!!!     &    FREEZ1, FREEZ2, IDETO, NOUTDO, NRUSLF, SLDOT,   !Input
!!!     &    TMIN, WTLF, YRDOY,  YRPLT,                      !Input
!!!     &    MDATE,                                          !Input/Output
!!!     &    SenLfFrz)                                       !Output
!!!      ELSE
!!!          SenLfFrz = 0.0
!!!      ENDIF
C-----------------------------------------------------------------------
C     Call to root growth and rooting depth routine
!-----------------------------------------------------------------------
!!!      IF (ISWWAT .EQ. 'Y') THEN
        CALL CSP_ROOTS(CONTROL, DYNAMIC,
!!!        CALL CSP_ROOTS(CONTROL, EMERG,
     &  AGRRT, CROP, DLAYR, DS, DTX, DUL, ESW, FILECC,       !Input
     &  FRRT, ISWWAT, LL, NLAYR, PGT, PLTPOP, PLWT, RO, RP,  !Input
     &  RTDWP, RTWTHa, SAT, SW, SWFAC,                       !Input
     &  VSTAGE, WR, WRDOTN, WTNEW,                           !Input
     &  CUMDEP, RLV, RTDEP, SATFAC, SENRT, SRDOT)            !Output
!!!      ENDIF
       
C-----------------------------------------------------------------------	 
C     Plant phosphorus module call
C-----------------------------------------------------------------------	 
       IF (ISWPHO .EQ. 'Y' .OR. ISWPHO .EQ. 'H') THEN

       CALL P_CASUPRO (CONTROL, DYNAMIC, ISWITCH,         !Input
     &    CROP, FILECC, GrowFrac, LFWTHa, MDATE,          !Input
     &    PLTPOP, RipeFrac, RLV, RTDEP, RTWTHa,           !Input
     &    SOILPROP,SPi_AVAIL, STKWTHa,                    !Input
     &    WLIDOT, WRIDOT, WSIDOT, YRPLT,                  !Input
     &    SENESCE,                                        !I/O
     &    PConc_Shut, PConc_Root, PConc_Seed,             !Output
     &    PStres1, PStres2, PUptake, FracRts)             !Output

       ENDIF
C-----------------------------------------------------------------------
C     Compute total C cost for growing vegetative tissue
C     for tomorrow's potential growth calculations
C-----------------------------------------------------------------------
C       Calculate the respiration required for veg tissue
C       depending on the source of N uptake
C-----------------------------------------------------------------------
!FSR - Changes for CASUPRO sugarcane model 
C     Following 6 lines of code removed by FSR (May-18-2005)
!      IF ((TRNU + NMINEA) .GT. 0.0) THEN
!         RPROAV = ((RSPNO3 + RSPNH4) * 0.16 + NMINEA * RPRO) 
!     &   / (TRNU + NMINEA)
!      ELSE
!         RPROAV = (RNO3C + RNH4C) / 2.   
!      ENDIF
C-----------------------------------------------------------------------
!FSR - Changes for CASUPRO sugarcane model 

!      AGRVG2 = AGRVG + (FRLF * PROLFI + FRRT * PRORTI + FRSTM * PROSTI
!     &         +  FRSU * PROSUI) * RPROAV

C-----------------------------------------------------------------------
C     Call routine to integrate growth and damage 
C-----------------------------------------------------------------------
!!!	  IF (DAS .GT. NVEG0) THEN
!!!
!!!	  CALL CSP_GROW_CANE(CONTROL, DYNAMIC,
!!!   &  AGEFAC,  CAB, CropTypeCode, DeltaLeafArea,             !Input
!!!     &  DeltaLeafNum, DTX, EXCESS, FILECC, FILEGC, FILEIO,     !Input
!!!     &  FINREF, FRSU, GAMMA, GRLF, GRRT, GRST, GRSU, Kill,     !Input
!!!     &  LeafNum, NVEG0, PAR, PgAvalPD, PGAVL,                  !Input
!!!     &  PGAVAL, PgRatio, PhenoStage, PLTPOP, PLF, PLWT,        !Input
!!!     &  SIZELF, SLA, SLAMIN, SIZREF, SLAMAX, SLAPAR, SLAREF,   !Input
!!!     &  SLAVAR, SLDOT, Smax, StkHrNO, StalkState,              !Input
!!!     &  StkB, StkM, TGRO, TURFAC, TURSLA, XFRRT,               !Input
!!!     &  XFRSU, XLAI, XLAIlost, XSLATM, XSTKRT, YFRRT,          !Input
!!!     &  YFRSU, YRDOY, YRPLT, YSTKRT,                           !Input
!!!     &  CDEM, FRRT, TOTWT, TOTALWT, XHLAI, YSLATM,       !Input/Output
!!!     &  CountStalk, DeltaLeafArealost, LAIMX, LeafArea,        !Output 
!!!     &  LeafArealost, LeafAreaPlant, LFWT, LFWTHa, LFWTHalost, !Output
!!!     &  LFWTlost, MntDEF, RTDWP, RTWTHa, SENESCE, StalkPopul,  !Output
!!!     &  STKWT, STKWTHa, STKWTHalost, STKWTP, SUWT, SUWTHa,     !Output
!!!     &  TOPWT)                                                 !Output
!-----------------------------------------------------------------------
!!!        ENDIF
      ENDIF !(DYNAMIC .EQ. INTEGR .AND. CROP .NE. 'FA')
!-----------------------------------------------------------------------

!***********************************************************************
!***********************************************************************
!-----------------------------------------------------------------------
!     OUTPUT / FINAL section
!-----------------------------------------------------------------------
      ELSE IF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
        IF (YRDOY .EQ. YREND .AND. DYNAMIC .EQ. OUTPUT) THEN
          STGDOY(16) = YREND
        ENDIF

        !IF (ISWDIS.EQ.'Y') THEN
        !  CALL PEST(CONTROL, ISWITCH, 
     &  !    AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, PGAVL,    !Input
     &  !    PHTIM, PLTPOP, RTWT, SLA, SLDOT, SOILPROP,      !Input
     &  !    SSDOT, STMWT, TOPWT, WLFDOT, WTLF, YRPLT,       !Input
     &  !    RLV, SDNO, SHELN, SWIDOT,                       !I/O
     &  !    VSTAGE, WSHIDT, WTSD, WTSHE,                    !I/O
     &  !    ASMDOT, DISLA, NPLTD, PPLTD,                    !Output
     &  !    SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT)             !Output
        !ENDIF
!-----------------------------------------------------------------------
C     Call leaf senescence routine for output
C-----------------------------------------------------------------------
      CALL CSP_SENES(CONTROL, DYNAMIC,
     &    CAB, ECONO, FILECC, FILEGC,                 !Input
     &    LeafMaint, LeafNum, LFmntDEF,               !Input
     &    MinGr, Ph1P, PhenoStage,                    !Input
     &    PI1, RATTP, Smax,                           !Input
     &    StalkState, STKmntDEF, STKWT, STKWTP,       !Input
     &    SumTTStalk, SWFAC, TMIN, XLAI,              !Input
     &    YRDOY, YRPLT,                               !Input
     &    Kill, SLDOT, SLNDOT, SSDOT, SSNDOT)         !Output
!-----------------------------------------------------------------------
C     Call sugarcane growth routine to update senescence 
C-----------------------------------------------------------------------
      CALL CSP_GROW_CANE(CONTROL, DYNAMIC,
     &  CAB, CropTypeCode, DeltaLeafArea,                 !Input 
     &  DeltaLeafNum, DTPI, EXCESS, FILEIO,               !Input
     &  FRSU, GAMMA, GRLF, GRRT, GRST,                    !Input
     &  GRSU, Kill, LeafNum, MAINTR, NLAYR, NVEG0,        !Input
     &  PgAvalPD, PGAVAL, PgRatio, PhenoStage,            !Input
     &  PLTPOP, PLF1, PLF2, PLWT, SLA,                    !Input
     &  SLAREF,                                           !Input 
     &  LSFAC, SENRT, SLDOT, Smax, SRDOT, StkHrNO, StalkState, !Input
     &  StkB, StkM, SumTTStalk,                           !Input
     &  TURFAC, XDAY, XFRRT,                              !Input
     &  XFRSU, XLAI, XLAIlost, XSTKRT, YFRRT,             !Input
     &  YFRSU, YRDOY, YRPLT, YSLA, YSTKRT,                !Input
     &  CDEM, FRRT, TOTWT, TOTALWT, XHLAI,                !Input/Output
     &  BRDMD, CountStalk, DeltaLeafArealost,                  !Output 
     &  LAIMX, LAIXD, LeafArea, LeafArealost, LeafAreaPlant,   !Output
     &  LFWT, LFWTHa, LSWTlost, LSWT, LSWTHa, LFWTHalost,      !Output 
     &  LFWTlost, LSWTHalost, StkH2OFac, STKmntDEF,            !Output
     &  RTDWP, RTWTHa, SENESCE, StalkPopul, STKWT, STKFWT,     !Output
     &  STKFWTHa, STKWTHa, STKWTHalost, STKWTP, SuDEF,         !Output
     &  SuH2OFac, SUWT, SUWTHa, TOPWT)                         !Output
C-----------------------------------------------------------------------
      CALL CSP_OPGROW(CONTROL, ISWITCH, 
     &  BRDMD, CADLF, CADST, CANHT, CANWH, CMINEA,                !Input
     &  CountStalk, DLAYR, EOS, GROWTH, GRWRES,                   !Input
     &  Kill, LeafArea, LeafArealost,LeafNum, LFWTHa,             !Input
     &  LFWT, LFWTHalost, LSWTlost, LSWT, LSWTHa,                 !Input
     &  LSWTHalost, LFWTlost, LITOTAL,                            !Input
     &  MAINR, MDATE, NFIXN, NLAYR, NSTRES, PSTRES1, PSTRES2,     !Input
     &  PCLSD,PCCSD, PCNL, PCNRT, PCNSD, PCNSH, PCNST,            !Input
     &  PGT, RHOL, RHOS, RLV, RTDEP,                              !Input
     &  RTWTHa, SATFAC, SENESCE, SLAREF, Smax, StalkPopul,        !Input
     &  STKFWTHa, STKFWT, STKWT, STKWTHa,                         !Input
     &  STKWTHalost, SumTTD, SumTTG, SWFAC, SUWT, SUWTHa,         !Input
     &  TGRO, TGROAV, TOTWT, TURFAC, WTNCAN, WTNLF, WTNST,        !Input
     &  WTNSD, WTNUP, WTNFX, XLAI, XLAIlost, YRPLT, PhenoStage,   !Input
     &  BADMD)                                                   !Output
C-----------------------------------------------------------------------    
C     Plant phosphorus module call
C-----------------------------------------------------------------------	 
       IF (ISWPHO .EQ. 'Y' .OR. ISWPHO .EQ. 'H') THEN

       CALL P_CASUPRO (CONTROL, DYNAMIC, ISWITCH,    !Input
     &    CROP, FILECC, GrowFrac, LFWTHa, MDATE,          !Input
     &    PLTPOP, RipeFrac, RLV, RTDEP, RTWTHa,           !Input
     &    SOILPROP,SPi_AVAIL, STKWTHa,                    !Input
     &    WLIDOT, WRIDOT, WSIDOT, YRPLT,                  !Input
     &    SENESCE,                                        !I/O
     &    PConc_Shut, PConc_Root, PConc_Seed,             !Output
     &    PStres1, PStres2, PUptake, FracRts)             !Output

       ENDIF
C-----------------------------------------------------------------------

        ! Write to Overview.out and summary.out files.
        CALL CSP_OPHARV(CONTROL, ISWITCH, 
     &    AGEFAC, BADMD, TillerCount, LAIMX,              !Input
     &    LAIXD, LeafNum, LFWTHa, LSWTHa,                 !Input
     &    HARVFRAC,NSTRES, PLTPOP, PLWT, PStres1,         !Input
     &    PStres2, PhenoStage, StalkPopul, StalkState,    !Input 
     &    STGDOY, STKFWTHa, StkHt, STKWTHa,               !Input
     &    STNAME, SUWTHa, SWFAC, TOPWT, TURFAC,           !Input
     &    WTNCAN, WTNUP, XLAI, YRPLT)                     !Input
      ENDIF

!       Call PlantNBal only for seasonal output.
      IF (DYNAMIC .EQ. SEASEND) THEN
        IF (CROP .NE. 'FA') THEN
          CALL PlantNBal (CONTROL, ISWITCH, 
     &      SEEDNI, TNLEAK, WTNFX, WTNLA, WTNLF, WTNLO,     !Input
     &      WTNNA, WTNNO, WTNNOD, WTNRA, WTNRO, WTNRT,      !Input
     &      WTNSA, WTNSD, WTNSDA, WTNSDO, WTNSH, WTNSHA,    !Input
     &      WTNSHO, WTNSO, WTNST, WTNUP)                    !Input
        ENDIF
!-----------------------------------------------------------------------
!     Calculate harvest residue left in field
        CALL CSP_HRes(CONTROL,
     &    CROP, DLAYR, DWNOD, HARVFRAC, NLAYR,            !Input
     &    PConc_Root, PConc_Shut, PLIGLF, PLIGNO, PLIGRT, !Input
     &    PLIGSD, PLIGSH, PLIGST, RLV, RTWTHa,            !Input
     &    SDWT, SENESCE, SHELWT, STMWT, WTLF,             !Input
     &    WTNLF,WTNNOD, WTNRT, WTNSD, WTNSH, WTNST,       !Input
     &    HARVRES)                                        !Output

        SENESCE % ResWt  = 0.0
        SENESCE % ResLig = 0.0
        SENESCE % ResE   = 0.0
      ENDIF

!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************

      RETURN
      END SUBROUTINE CSP_CASUPRO
!=======================================================================

!***********************************************************************
!     Variable listing
!***********************************************************************
! AGEFAC    Reduction in photosynthesis, correlated to a decrease in 
!             percentage of Nitrogen in leaves (0-1) (fraction)
! AGRLF     Mass of CH2O required for new leaf growth (g[CH2O] / g[leaf])
! AGRRT     Mass of CH2O required for new root growth (g[CH2O] / g[root])
! AGRSTM    Mass of CH2O required for new stem growth (g[CH2O] / g[stem])
! AGRVG     Mass of CH2O required for vegetative tissue growth including 
!             stoichiometry and respiration (g[CH2O] / g[tissue])
! AGRVG2    Total mass of CH2O required for vegetative tissue growth
!             (g[CH2O] / g[tissue])
! AREALF    Area of leaves (one side) per unit ground area
!             (cm2[leaf] / m2[ground])
! ASMDOT    Daily assimilative damage (g[CH2O] /m2 / d)
! BADMD     Aerial dry biomass: stalk + leaf(live & dead) + sucrose (t/ha)
! BETN      Spacing between plants in a row (m / plant)
! BRDMD     Rate of biomass accumulation (kg/ha/d)
! CADLF     Mass of CH2O added to leaf reserves after growth
!             (g[CH2O] / m2 / d)
! CADST     Mass of CH2O added to stems (g[CH2O] / m2 / d)
! CANHT     Canopy height (m)
! CANNAA    Weight of N in total plant at flowering (g[N] / m2)
! CANWAA    Canopy weight at flowering stage (g[plant] / m2)
! CANWH     Canopy width normal to row (m)
! CAVVEG    C available for vegetative tissue growth (g[CH2O] / m2 / d)
! CDMREP    Total CH2O needed for potential reproductive growth
!             (g[CH2O] / m2 / d)
! CLW       Cumulative leaf growth (g[leaf]/m2)
! CMINEA    Actual carbon mined from vegetative tissue (g[CH2O] / m2 / d)
! CMINEP    Potential CH2O mobilization from storage (g[CH2O] / m2 / d)
! CMOBMX    Maximum C pool mobilization rate (g[CH2O] / m2 / d)
! CNDFX     Carbon needed to fix N needed but not supplied by uptake or 
!             mining (g[CH2O] / m2 / d)
! CROP      Crop identification code 
! CROPD     Name of crop 
! CRUSLF    C mobilized from leaf tissue in a day (g[CH2O] / m2 / d)
! CRUSRT    C mobilized from root tissue in a day (g[CH2O] / m2 / d)
! CRUSST    C mobilized from stem tissue in a day (g[CH2O] / m2 / d)
! CSAVEV    Fraction of PG for VEG that is stored as CH2O 
! CSW       Cumulative stem growth (g[stem]/m2)
! CUMDEP    Cumulative depth of soil profile (cm)
! DAS       Days after start of simulation (days)
! DAYL      Day length on day of simulation (from sunrise to sunset) (hr)
! DISLA     Diseased leaf area (cm2/m2/d)
! DLAYR(L)  Soil thickness in layer L (cm)
! DRPP      Photoperiod days which occur in a real day
!             (photoperiod days / day)
! DTX       Thermal time that occurs in a real day based on vegetative 
!             development temperature function (thermal days / day)
! DUL(L)    Volumetric soil water content at Drained Upper Limit in soil 
!             layer L (cm3 [H2O] /cm3 [soil])
! DYNAMIC   Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
!             INTEGR, OUTPUT, or FINAL 
! ECONO     Ecotype code - used to match ECOTYP in .ECO file 
! ENAME     Experiment description 
! EP1       Potential plant transpiration, cm/d
! EXCESS    Factor based on excess PG used to affect tomorrow's PG 
!             calculation 
! EXPER     Experiment code (prefix of input files) 
! F         Specific leaf area of new leaf tissue growth, including N
!             (cm2[leaf] / g[leaf])
! FILECC    Path plus filename for species file (*.spe) 
! FILEGC    Pathname plus filename for ECO file 
! FILEIO    Filename for INP file (e.g., DSSAT45.INP) 
! FLWN(J)   Number of flowers added for cohort J (# / m2)
! FNINL     Maximum fraction of N for growing leaf tissue (g[N] / g[leaf])
! FNINR     Maximum fraction of N for growing root tissue (g[N] / g[root])
! FNINS     Maximum fraction of N for growing stem tissue (g[N] / g[stem])
! FRACDN    Relative time between flowering (NR1) and last leaf appearance 
!             (NDLEAF) 
! FREEZ1    Temperature below which plant loses all leaves, but development 
!             continues (�C)
! FREEZ2    Temperature below which plant growth stops completely. (�C)
! FRLF      Fraction of vegetative tissue growth that goes to leaves on a 
!             day (g[leaf] / g[veg])
! FRRT      Fraction of vegetative tissue growth that goes to roots on a 
!             day (g[root] / g[veg])
! FRSTM     Fraction of vegetative tissue growth that goes to stems on a 
!             day (g[stem] / g[veg])
! GROWTH    Total new growth of plant tissue on a day (g[tissue] / m2 / d)
! GRWRES    Growth respiration (g[CH2O]/m2-d)
! IDETO     Switch for printing OVERVIEW.OUT file 
! ISWDIS    Pest damage simulation switch (Y or N) 
! ISWITCH   Composite variable containing switches which control flow of 
!             execution for model.  See Appendix A. 
! ISWNIT    Nitrogen simulation switch (Y or N) 
! ISWWAT    Water simulation control switch (Y or N) 
! LAIMX     Maximum leaf area index this season (m2[leaf] / m2[ground])
! LL(L)     Volumetric soil water content in soil layer L at lower limit
!             ( cm3/cm3)
! MAINR     Maintenance respiration (g[CH2O] / m2 / d)
! MAINTR(j) Maintenance respiration by stalk (g[CH2O] / stalk j / d)
! MEEVP     Method of evapotranspiration (P=Penman, R=Priestly-Taylor, 
!             Z=Zonal) 
! MEPHO    Method for photosynthesis computation ('C'=Canopy or daily, 
!            'L'=hedgerow or hourly) 
! MODEL     Name of CROPGRO executable file 
! NADLF     N added to leaf N reserves (g[N] / m2 / d)
! NADRT     N added to root N reserves (g[N] / m2 / d)
! NADST     N added to stem N reserves (g[N] / m2 / d)
! NAVL      Total mass of nitrogen available for growth (g[N] / m2 / d)
! NAVLV     N available for vegetative growth (g[N] / m2 / d)
! NDLEAF    Day when leaf expansion ceased (days)
! NDMNEW    Total N demand for new growth (g[N] / m2 / d)
! NDMOLD    N demand for old tissue (g[N] / m2 / d)
! NDMTOT    Total N demand (g[N] / m2 / d)
! NDMVEG    N required for vegetative growth if all PGAVL is used as 
!             computed (g[N] / m2 / d)
! NGRLF     Maximum N demand for leaf growth (g[leaf N] / m2[ground] / d)
! NGRRT     Maximum N demand for root growth (g[root N] / m2[ground] / d)
! NGRST     Maximum N demand for stem growth (g[stem N] / m2[ground] / d)
! NH4(L)    Ammonium N in soil layer L (�g[N] / g[soil])
! NL        Maximum number of soil layers = 20 
! NLAYR     Number of soil layers 
! NMINEA    Actual Nitrogen mined from existing tissue (g[N] / m2 / d)
! NMINEP    Potential N mobilization from storage (g[N] / m2 / d)
! NMOBR     Stage dependent N mining rate 
! NO3(L)    Nitrate in soil layer L (�g[N] / g[soil])
! NOUTDO    Logical unit for OVERVIEW.OUT file 
! NPLTD     Number of plants destroyed (#/m2/d)
! NR1       Day when 50% of plants have at least one flower (days)
! NRUSLF    N actually mobilized from leaves in a day (g[N]/m2-d)
! NRUSRT    N actually mobilized from roots in a day (g[N]/m2-d)
! NSTRES    Nitrogen stress factor (1=no stress, 0=max stress) 
! NVEG0     Day of emergence (days)
! OUTD      File name for pest damage output file 
! OUTG      Growth output file name (typically 'GROWTH.OUT') 
! PAR       Daily photosynthetically active radiation or photon flux 
!             density (moles[quanta]/m2-d)
! PCH2O     Respiration loss due to storage/mobilization of CH2O
!             (g[CH2O] / g[CH2O])
! PCNL      Percentage of N in leaf tissue (100 g[N] / g[leaf])
! PCNRT     Percent N in root tissue (100 g[N] / g[root])
! PCNST     Percent N in stem tissue (100 g[N] / g[stem])
! PG        Daily gross photosynthesis (g[CH2O] / m2 / d)
! PGAVAL(j) Total daily CH2O available to stalk j for growth & respiration 
!           after expending PG on maintenance respiration 
!           (g[CH2O] / stalk j / d)
! PgAvalPD  Total daily CH2O available for growth & respiration after
!           expending PG on maintenance respiration (g[CH2O] / m2 / d)
!           (directly replaces PGAVL)
! PGAVL     Total daily CH2O available for growth & respiration
!           (g[CH2O] / m2 / d)
! PHTHRS(I) Threshold time that must accumulate in phase I for the next 
!             stage to occur  (thermal or photothermal days)
! PLTPOP    Plant population (# plants / m2)
! PNTIM(I)  Photothermal days from first flower when flowers in age group I 
!             formed (p-t-d)
! PPLTD     Percent plants destroyed (%/m2/d)
! PROLFI    Maximum protein composition in leaves during growth with 
!             luxurious supply of N (g[protein] / g[leaf tissue])
! PRORTI    Maximum protein composition in roots during growth with 
!             luxurious supply of N (g[protein] / g[root])
! PROSTI    Maximum protein composition in stems during growth with 
!             luxurious supply of N (g[protein] / g[stem])
! PROVEG    Average protein composition of growing tissue today
!             (g[protein] / g[veget. tissue])
! R30C2     Respiration coefficient that depends on gross photosynthesis, 
!           value at 30C (g[CH2O] used / g[CH2O] fixed / hr)
! RES30C    Respiration coefficient that depends on total plant mass,
!           value at 30C (g CH2O/g DW/hr)
! RFIXN     CH2O required for biological N fixation (g[CH2O] / g[protein])
! RHOL      Fraction of leaf which is carbohydrate (g [CH20] / g[leaf])
! RHOS      Fraction of stem which is carbohydrate (g [CH2O] / g[stem])
! RLIG      Respiration required for synthesizing lignin structure
!             (g[CH2O] / g[lignin])
! RLIP      Respiration required for synthesizing lipid structure
!             (g[CH2O] / g[lipid])
! RLV(L)    Root length density for soil layer L ((cm root / cm3 soil))
! RMIN      Respiration required for synthesizing mineral structure
!             (g[CH2O] / g[mineral])
! RNH4C     CH2O required for protein synthesis when source of N is 
!             ammonium uptake (g[CH2O] / g[protein])
! RNITP     True nitrogen concentration in leaf tissue for photosynthesis 
!             reduction. (%)
! RNO3C     Respiration required for reducing NO3 to protein
!             (g[CH2O] / g[protein])
! RO        Respiration coefficient that depends on total plant mass
!             (g[CH2O] / g[tissue])
! ROA       Respiration required for synthesizing organic acids
!             (g[CH2O] / g[product])
! ROWSPC    Row spacing (m)
! RP        proportion of the day's photosynthesis which is respired in the 
!             maintenance process 
! RPRO      Respiration required for re-synthesizing protein from mobilized 
!             N (g[CH2O] / g[protein])
! RPROAV    Respiration required for protein synthesis, average based on 
!             sources of N (g[CH2O] / g[protein])
! RSPNH4    Respiration required for reducing NH4 to protein
!             (g[CH2O] / m2 / d)
! RSPNO3    Respiration required for reducing NO3 to protein
!             (g[CH2O] / m2 / d)
! PhenoStage    Number of RSTAGES which have occurred. 
! RTDEP     Root depth (cm)
! RTWT      Dry mass of root tissue, including C and N
!             (g[root] / m2[ground])
! RVSTGE    Rate of VSTAGE change (nodes/day)
! SAT(L)    Volumetric soil water content in layer L at saturation
!             (cm3 [water] / cm3 [soil])
! SATFAC    Root length weighted soil water excess stress factor ( 0 = no 
!             stress; 1 = saturated stress ) 
! SENSOILC  Daily contribution of senesced plant matter to soil carbon
!             (g[C] / m2 / day)
! SENSOILL  Daily contribution of senesced plant matter to soil lignin
!             (g[lignin] / m2 / day)
! SENSOILN  Daily contribution of senesced plant matter to soil nitrogen
!             (g[N] / m2 / day)
! SENSURFC  Daily contribution of senesced and frozen plant matter to 
!             surface carbon  (g[C] / m2 / day)
! SENSURFL  Daily contribution of senesced and frozen plant matter to 
!             surface lignin (g[lignin] / m2 / day)
! SENSURFN  Daily contribution of senesced and frozen plant matter to 
!             surface nitrogen (g[N] / m2 / day)
! SLA       Specific leaf area (cm2[leaf] / g[leaf])
! SLAAD     Specific leaf area, excluding weight of C stored in leaves
!             (cm2[leaf] / g[leaf])
! SLDOT     Defoliation due to daily leaf senescence (g/m2/day)
! SLNDOT    Leaf senescence due to water stress (g/m2/day)
! SNH4(L)   Total extractable ammonium N in soil layer L (kg [N] / ha)
! SNO3(L)   Total extractable nitrate N in soil layer L (kg [N] / ha)
! SRDOT     Daily root senescence (g / m2 / d)
! SSDOT     Daily senescence of petioles (g / m2 / d)
! SSNDOT    Petiole senescence due to water stress (g/m2/day)
! ST(L)     Soil temperature in soil layer L (�C)
! STGDOY(I) Day when stage I occurred (YYDDD)
! StkHrNO   Number of stalks last harvested from existing stubble.
! StkPG(j)  Daily gross photosynthesis by stalk j (g[CH2O] / stalk / d)
! STMWT     Dry mass of stem tissue, including C and N
!             (g[stem] / m2[ground)
! STNAME(I) Output headings for specified crops 
! SW(L)     Volumetric soil water content in layer L
!             (cm3 [water] / cm3 [soil])
! SWFAC     Effect of soil-water stress on photosynthesis, 1.0=no stress, 
!             0.0=max stress 
! TAVG      Average daily temperature (�C)
! TDUMX     Photo-thermal time that occurs in a real day based on early 
!             reproductive development temperature function
!             (photo-thermal days / day)
! TDUMX2    Photo-thermal time that occurs in a real day based on late 
!             reproductive development temperature function
!             (photo-thermal days / day)
! TGRO(I)   Hourly air temperature (�C)
! TGROAV    Average daily air temperature (�C)
! TITLET    Description of treatment for this simulation 
! TMIN      Minimum daily temperature (�C)
! TNLEAK    Total nitrogen leak (g[N] / m2 / d)
! TOPWT     Total weight of above-ground portion of crop. (g[tissue] / m2)
! TOTALWT(j) Total weight of sugarcane stalk j, including a proportion of 
!           root tissue   (g[tissue] / stalk j)
! TOTWT     Total weight of crop (g[tissue] / m2)
! TRNH4U    Total N uptake in ammonium form in a day (g[N] / m2 / d)
! TRNO3U    Total N uptake in nitrate form in a day (g[N] / m2 / d)
! TRNU      Total N uptake in a day (g[N] / m2 / d)
! TS        Number of intermediate time steps (=24) 
! TURFAC    Water stress factor for expansion (0 - 1) 
! UNH4      Uptake of NH4 from soil (interim value) (kg N/ha)
! UNO3      Uptake of NO3 from soil (interim value) (kg N/ha)
! VSTAGE    Number of nodes on main stem of plant 
! WCRLF     Mass of CH2O reserves in leaves (g[leaf CH2O] / m2[ground])
! WCRRT     Mass of CH2O reserves in roots (g[root CH2O] / m2[ground])
! WCRST     Mass of CH2O reserves in stems (g[stem CH2O] / m2[ground])
! WLDOTN    Dry weight growth rate of new leaf tissue including N but not C 
!             reserves (g[leaf] / m2[ground]-d)
! SenLfFrz    Leaf weight losses due to freezing (g[leaf]/m2-d)
! WLIDOT    Daily pest or freeze damage to leaf mass (g/m2/day)
! WNRLF     N available for mobilization from leaves above lower limit of 
!             mining (g[N] / m2)
! WNRRT     N available for mobilization from roots above lower limit of 
!             mining (g[N] / m2)
! WRDOTN    Dry weight growth rate of new root tissue including N but not C 
!             reserves (g[root] / m2[ground]-d)
! WRIDOT    Daily pest damage to root mass ( g/m2/day)
! WSDOTN    Dry weight growth rate of new stem tissue including N but not C 
!             reserves (g[stem] / m2[ground]-d)
! WSIDOT    Daily pest damage to stem mass (g/m2/day)
! WTCO      Cumulative losses of plant tissue (g[tissue] / m2)
! WTLF      Dry mass of leaf tissue including C and N
!             (g[leaf] / m2[ground])
! WTLO      Cumulative leaf losses (g[leaf] / m2)
! WTMAIN    Mass of tissue assumed to require maintenance (g[tissue] / m2)
! WTNCAN    Mass of N in canopy (g[N] / m2[ground])
! WTNEW     Initial mass of seedling (g / plant)
! WTNFX     Cumulative weight of N fixed (g[N] / m2)
! WTNLA     Cumulative N added to leaves (g[N]/m2-d)
! WTNLF     Mass of N in leaves (g[leaf N] / m2[ground])
! WTNLO     Cumulative N loss from leaves (g[N] / m2)
! WTNRA     Cumulative N added to roots (g[N]/m2-d)
! WTNRO     Cumulative N loss from roots (g[N] / m2)
! WTNRT     Mass of N in roots (g[root N] / m2[ground])
! WTNSA     Cumulative N added to stems (g[N]/m2-d)
! WTNSO     Cumulative N loss from stems (g[N] / m2)
! WTNST     Mass of N in stems (g[stem N] / m2[ground])
! WTNUP     Cumulative N uptake (g[N] / m2)
! WTRO      Cumulative root losses (g[root] / m2)
! WTSHMT     Cohorts that reach THRESH today (g/m2)
! WTSO      Cumulative stem losses (g[stem] / m2)
! XFRT      Current day's partitioning to reproductive growth (0-1)
!             (g[fruit] / g[plant])
! XHLAI     Leaf area index (m2[leaf] / m2[ground])
! XLAI      Leaf area (one side) per unit of ground area
!             (m2[leaf] / m2[ground])
! XSLATM(I) Temperature values for function that reduces specific leaf area 
!             (SLA) (�C)
! YRDOY     Current day of simulation (YYDDD)
! YREMRG    Day of emergence (YYDDD)
! MDATE     Harvest maturity (YYDDD)
! YRPLT     Planting date (YYDDD)
! YRSIM     Start of simulation date (YYDDD)


! LIST OF NEW VARIABLES ADDED TO ENABLE SIMULATION OF SUGARS IN THE 
! OHD SUGARCANE MODEL FSR

! AGRSU    Mass of CH2O required for new sugars growth (g [CH2O] / g[sugar])
! ALPHSU   Fraction of new sugars growth that is mobile C (fraction)
! CPFSU    Respiration requirement for net sugars growth
!            (g [CH20] / g [tissue]) +++
! CRUSSU   C mobilized from sugars in a day (g [CH2O] / m2 - d)
! CSUW     Cumulative leaf growth (g [leaf] / m2)
! NADSU    N added to sugars N reserves (g [N] / m2 - d)
! NGRSU    Maximum N demand for sugars growth (g [sugars N] / m2 [ground] - d)
! NRUSSU   N actually mobilized from sugars in a day (g [N] / m2 - d)
! NSUDOT   Net N addition for sugars (g [N] / m2 [ground] - d)
! NSUOFF   N loss from sugars in a day (g [N]/m2 - d)
! NSUALL   N added to sugars today (g [N] / m2 - d)
! PCNSU    Percentage of N in sugars (100 g [N] / g [sugars]) +++
! PCARSU   Proportion of sugars that is carbohydrate (fraction)
! PLIGSU   Proportion of sugars that is lignin (fraction)
! PLIPSU   Proportion of sugars that is lipid (fraction)
! PMINSU   Proportion of sugars that is mineral (fraction)
! POASU    Proportion of sugars that is organic acid (fraction)
! PROSUF   Minimum sugars protein composition after N mining
!            (g [protein] / g [sugars])
! PROSUG   Normal growth protein composition in sugars during growth
!            (g [protein] / g [sugar])
! PROSUI   Maximum protein composition in sugars during growth with 
!            luxurious supply of N (g [protein] / g [sugars])
! PROSUT   Protein fraction of new sugars growth (g [protein] / g [sugar])
! RHOSU    Fraction of sugars which is carbohydrate (g [CH20] / g[sugars])
! StalkState(j,k)  Condition k of stalk j.  Currently, conditions are:
!                  k = 1 (LIVE or DEAD); 2 (PRIM or TILR); 3-10 unused          
! SUWT     Dry mass of sugars, including C and N
!            (g [sugars] / m2 [ground])
! WCRSU    Mass of CH2O reserves in sugars (g [sugars CH2O] / m2 [ground])
! WNRSU    N available for mobilization from sugars above lower limit of 
!            mining (g [N] / m2)
! WRCSUDT  Net C addition for sugars (g [CH2O] / m2 - d)
! WSUDOT   Net sugars growth rate (g [sugars] / m2 - d)
! WSUDOTN  Dry weight growth rate of new sugars including N but not C 
!            reserves (g [sugars] / m2 [ground] - d)
! WSUI     Initial weight of sugars (g [leaf] / m2)
! WTNSU    Mass of N in sugars (g [sugars N] / m2 [ground])
! WTNSUA   Cumulative N added to sugars (g [N] / m2 - d)
! WTNSUO   Cumulative N loss from sugars (g [N] / m2)
! WTSUO    Cumulative sugar losses (g [sugars] / m2)

!***********************************************************************
!      END SUBROUTINE CASUPRO
!=======================================================================



