C=======================================================================
C  GROW, Subroutine, G. Hoogenboom, J.W. Jones, and K.J.Boote
C-----------------------------------------------------------------------
C  Governing Model Equations
C  Integrates all growth related variables
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  01/09/1989 GH  Written.
C  01/04/1996 GH  Added seed composition routine.
C  01/19/1996 KJB NMOBR
C  04/02/1996 KJB Modified
C  05/30/1998 CHP moved diseased leaf area calculations to PEST module.
C  07/16/1998 CHP Modified for modular format
C  05/11/1999 GH  Incorporated in CROPGRO
C  03/29/2000 CHP Added cumulative senescence variables
C  06/19/2001 GH  Add checks for negative state variables
C  06/11/2002 GH  Modified for Y2K
C  03/12/2003 CHP Changed senescence variable to composite (SENESCE)
C                   as defined in ModuleDefs.for
C  03/24/2004 CHP Added P component of senesced matter
C  01/19/2006 CHP N in senesced roots lost at actual N%, not minimum.
C  04/01/2021 VSH/AH Added MultiHarvest code changes.
!  06/15/2022 CHP Added CropStatus
!  11/08/2023  FO Added lint growth for cotton.
C-----------------------------------------------------------------------
C  Called by:  PLANT
C  Calls:      IPGROW, STRESS
C              ERROR
C=======================================================================
      SUBROUTINE GROW(CONTROL, ISWITCH, DYNAMIC, SOILPROP, 
     &  AGEFAC, CADLF, CADST, CRUSLF, CRUSRT, CRUSSH,     !Input
     &  CRUSST, DISLA, F, FILECC, FILEGC, FRLF, FRSTM,    !Input
     &  NADLF, NADRT, NADST, NDTH, NFIXN, NGRLF, NGRRT,   !Input
     &  NGRSD, NGRSH, NGRST, NMINEA, NODGR, NOUTDO,       !Input
     &  NPLTD, NRUSLF, NRUSRT, NRUSSH, NRUSST, ECONO,     !Input
     &  POTCAR, POTLIP, PPLTD, SDIDOT, SDPROR,            !Input
     &  SENNOD, SENRT, SLDOT, SLNDOT, SRDOT, SSDOT,       !Input
     &  SSNDOT, TRNH4U, TRNO3U, TRNU, TAVG, NSTRES,       !Input
     &  TURFAC, WLDOTN, WLIDOT, WRDOTN, WRIDOT, WSDDTN,   !Input
     &  WSDOTN, WSHDTN, WSIDOT, WTABRT, WTSHMT, YRNR1,    !Input
     &  MDATE, YRPLT,                                     !Input

     &  SWIDOT, WLFDOT, WSHIDT, WTNFX, XHLAI,             !Input/Output

     &  AREALF, BETN, CANNAA, CANWAA, CLW, CropStatus,    !Output
     &  CSW, DWNOD, DWNODA, GROWTH, GRWRES, LAIMX, PCCSD, !Output
     &  PCLSD, PCNL, PCNRT, PCNSD, PCNSH, PCNST, PLTPOP,  !Output
     &  PLIGLF, PLIGNO, PLIGRT, PLIGSD, PLIGSH, PLIGST,   !Output
     &  PODWT, PUNCSD, PUNCTR, RHOL, RHOS, RNITP,         !Output
     &  ROWSPC, RTWT, SDNPL, SDRATE, SDWT, LINTW,         !Output
     &  SEEDNI, SEEDNO, SENESCE, SHELWT, SLA,             !Output
     &  SLAAD, STMWT, TOPWT, TOTWT, WCRLF, WCRRT, WCRSH,  !Output
     &  WCRST, WNRLF, WNRRT, WNRSH, WNRST, WTCO,          !Output
     &  WTLF, WTLO, WTMAIN, WTNCAN, WTNEW, WTNLA, WTNLF,  !Output
     &  WTNLO, WTNNA, WTNNAG, WTNNO, WTNNOD, WTNOO,       !Output
     &  WTNRA, WTNRO, WTNRT, WTNSA, WTNSD, WTNSDA,        !Output
     &  WTNSDO, WTNSH, WTNSHA, WTNSHO, WTNSO, WTNST,      !Output
     &  WTNUP, WTRO, WTSDO, WTSHO, WTSO, XLAI, XPOD,      !Output
     &  ShutMob, RootMob, ShelMob,                        !Output
     &  TOSHMINE,TOCHMINE,HPODWT,HSDWT,HSHELWT)           !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      
      IMPLICIT NONE
      EXTERNAL IPGROW, ERROR, STRESS, LTGROW
      SAVE
!-----------------------------------------------------------------------

      CHARACTER*1  ISWSYM, ISWNIT, IDETO, IHARI, PLME
      CHARACTER*2  XPODF, CROP
      CHARACTER*6  ERRKEY, ECONO
      PARAMETER (ERRKEY = 'GROW  ')
      CHARACTER*30 FILEIO
      CHARACTER*92 FILECC, FILEGC

      INTEGER DYNAMIC, NOUTDO, L, NLAYR
      INTEGER YRDOY, YRNR1, MDATE
      INTEGER YRPLT, CropStatus

      REAL WTNUP,WTNFX,WTNMOB,WTNCAN,TGROW
      REAL WRCSHD,DISLA,WSDMAN
      REAL WLFDOT, PPLTD,NPLTD,SDWTAM,CANNAA,CANWAA
      REAL DWNODA,WTNNAG,AGEFAC
      REAL POTLIP,POTCAR
      REAL CPFLF, CPFSTM, CPFRT, CPFNOD, CPFSH1, CPFSD1
      REAL WRIDOT,WSIDOT,WTNNOD
      REAL WNDOT,  WPDOT,  WDOT,WRCLDT,WRCSDT,WRCRDT

      REAL NLOFF,  NSOFF,  NROFF,  NSHOFF, NSDOFF, NNOFF
      REAL WTLO,   WTSO,   WTRO,   WTSHO,  WTSDO,  WTNOO
      REAL WTCO
      REAL NLALL,  NSALL,  NRALL,  NSHALL, NSDALL
      REAL WTNLA,  WTNSA,  WTNRA,  WTNSHA, WTNSDA, WTNNA
      REAL WTNLO,  WTNSO,  WTNRO,  WTNSHO, WTNSDO, WTNNO
      REAL WLDOT,  WSDOT,  WRDOT, LTDOT, LINTW

      REAL WLDOTN, WSDOTN, WRDOTN,         WSDDTN, WSHDTN
      REAL NGRLF,  NGRST,  NGRRT,  NGRSH,  NGRSD
      REAL WTLF,   STMWT,  RTWT,   SHELWT, SDWT
      REAL PROLFF, PROSTF, PRORTF, PROSHF
      REAL PCNL,   PCNST,  PCNRT,  PCNSH,  PCNSD
      REAL WTNLF,  WTNST,  WTNRT,  WTNSH,  WTNSD
      REAL NLDOT,  NSDOT,  NRDOT,  NSHDOT, NSDDOT
      REAL NRUSLF, NRUSST, NRUSRT, NRUSSH
      REAL CRUSLF, CRUSST, CRUSRT, CRUSSH
      REAL ALPHL,  ALPHS,  ALPHR,  ALPHSH
      REAL RHOL,   RHOS,   RHOR,   RHOSH
      REAL WCRLF,  WCRST,  WCRRT,  WCRSH
      REAL NADLF,  NADST,  NADRT
      REAL WNRLF,  WNRST,  WNRRT,  WNRSH
      REAL CADLF,  CADST

      REAL SLDOT,  SSDOT,  SRDOT, SWIDOT
      REAL SLNDOT, SSNDOT, SDIDOT
      REAL WLIDOT, WSHIDT, WSHDOT, WSDDOT
      REAL SDPROR, GRWRES, TRNO3U, TRNH4U, NFIXN
      REAL WTABRT
      REAL WTLSD, WTCSD, NTOVR, NDTH, XPOD
      REAL TOTWT, TOPWT, PODWT, DWNOD
      REAL WTMAIN
      REAL WTSHMT
      REAL WTNTOT, NMINEA, TRNU
      REAL PCLSD, PCCSD, RNITP, PCNMIN
      REAL ALFDOT,  F, SLA, AREALF, XLAI, SLAAD
      REAL LAIMX, AREAH
      REAL XHLAI, SEEDNO, PLTPOP, ROWSPC, BETN
      REAL TURFAC, TAVG, NSTRES
      REAL GROWTH, NODGR

      REAL FRLF, FRSTM
      REAL SDNPL, SEEDNI, SDRATE
      REAL WTNEW

      REAL PCARLF, PCARNO, PCARRT, PCARSD, PCARSH, PCARST,
     &  PLIGLF, PLIGNO, PLIGRT, PLIGSD, PLIGSH, PLIGST,
     &  PLIPLF, PLIPNO, PLIPRT, PLIPSH, PLIPST,
     &  PMINLF, PMINNO, PMINRT, PMINSD, PMINSH, PMINST,
     &  POALF,  POANO,  POART,  POASD,  POASH,  POAST,
     &  PROLFI, PRONOD, PRORTI, PROSTI,
     &  SDPRO, WTFSD, WTPSD, SDWTPL
      REAL RMIN, SDLIP, WLFI, WSTI, WRTI
      REAL CLW, CSW

!     Surface and soil residue due to daily senescence of plant matter
      REAL SENRT(NL), SENNOD(NL)
      REAL SenWt(0:NL)        !kg[dry matter]/ha
      REAL SenLig(0:NL)       !kg[lignin]/ha
      REAL SenE(0:NL,NELEM)   !kg[E]/ha (E=N, P, S,...)
      REAL NLPEST

!CHP - puncture variables, not functional
      REAL PUNCSD, PUNCTR, PUNDOT, SDPDOT 

!     P module
      REAL ADD
      REAL ShutMob, RootMob, ShelMob

      REAL TOSHMINE, TOCHMINE
      REAL HPODWT,HSDWT,HSHELWT
      REAL NHSHWT, NHSDWT
!-----------------------------------------------------------------------
!     Constructed variable types defined in ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (SoilType)    SOILPROP
      TYPE (ResidueType) SENESCE

!     Transfer values from constructed data types into local variables.
      !Don't get DYNAMIC from CONTROL variable because it will not
      ! have EMERG value (set only in CROPGRO).
      !DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      YRDOY   = CONTROL % YRDOY

      IDETO  = ISWITCH % IDETO
      IHARI  = ISWITCH % IHARI
      ISWNIT = ISWITCH % ISWNIT
      ISWSYM = ISWITCH % ISWSYM

      NLAYR  = SOILPROP % NLAYR

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC == RUNINIT) THEN
!-----------------------------------------------------------------------
      CALL IPGROW(
     &  FILEIO, FILECC,  CROP,                                  !Input
     &  ALPHL,  ALPHR,  ALPHS,  ALPHSH,                         !Output
     &  PCARLF, PCARST, PCARRT, PCARSH, PCARSD, PCARNO,         !Output
     &  PLIGLF, PLIGST, PLIGRT, PLIGSH, PLIGSD, PLIGNO,         !Output
     &  PLIPLF, PLIPST, PLIPRT, PLIPSH,                 PLIPNO, !Output
     &  PMINLF, PMINST, PMINRT, PMINSH, PMINSD, PMINNO,         !Output
     &  POALF,  POAST,  POART,  POASH,  POASD,  POANO,          !Output
     &  PROLFF, PROSTF, PRORTF, PROSHF,                 PRONOD, !Output
     &  PROLFI, PROSTI, PRORTI,                                 !Output
     &  PLTPOP, ROWSPC, RMIN,   PLME,   SDWTPL,                 !Output
     &  SDLIP,  SDPRO,  WTFSD,  WTPSD,   XPODF)                 !Output

      IF (CROP .NE. 'FA') THEN
!-----------------------------------------------------------------------
!       Copied from IPIBS
!-----------------------------------------------------------------------
        ROWSPC = ROWSPC / 100.
        IF (ROWSPC * PLTPOP > 1.E-4) THEN
          BETN = 1./(ROWSPC*PLTPOP)
        ELSE
          BETN = 0.
        ENDIF

C-----------------------------------------------------------------------
C     Newest version of CROPGRO has variable seed N, oil, CH2O,
C     which may make this tougher
C-----------------------------------------------------------------------
        CPFLF  = 30./44.*(PLIPLF*1.720 + PLIGLF*0.659 + POALF*(-0.011)
     &         + PMINLF*RMIN + PCARLF*0.170)
        CPFSTM = 30./44.*(PLIPST*1.720 + PLIGST*0.659 + POAST*(-0.011)
     &         + PMINST*RMIN + PCARST*0.170)
        CPFRT  = 30./44.*(PLIPRT*1.720 + PLIGRT*0.659 + POART*(-0.011)
     &         + PMINRT*RMIN + PCARRT*0.170)
        CPFNOD =  30./44.*(PLIPNO*1.720 + PLIGNO*0.659 + POANO*(-0.011)
     &         + PMINNO*RMIN + PCARNO*0.170)
        CPFSH1 =  30./44.*(PLIPSH*1.720 + PLIGSH*0.659 + POASH*(-0.011)
     &         + PMINSH*0.073 + PCARSH*0.170)
        CPFSD1 = 30./44.*(PMINSD*0.073 + PLIGSD*0.659 + POASD*(-0.011)
     &         + (SDLIP*1.720 + PCARSD*0.170)*(1. - SDPROR))
C-----------------------------------------------------------------------
!     CALCULATE PERCENT NITROGEN IN LEAVES AT END OF SEASON
        PCNMIN = PROLFF * 16.0            !Moved from INCOMP
!-----------------------------------------------------------------------
      ENDIF
C-----------------------------------------------------------------------
C     Net Lint growth rate
C-----------------------------------------------------------------------
      IF(CROP .EQ. 'CO') THEN
        CALL LTGROW(DYNAMIC, FILECC, FILEGC, ECONO,              !Input
     &    WSDDOT, TAVG, TURFAC, NSTRES,                          !Input
     &    LTDOT)                                                 !Output
      ENDIF
!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC == SEASINIT) THEN
!-----------------------------------------------------------------------
      ALFDOT = 0.0
      AREALF = 0.0
      AREAH  = 0.0
      CANWAA = 0.0
      CANNAA = 0.0
      GROWTH = 0.0
      GRWRES = 0.0
      LAIMX  = 0.0
      LINTW  = 0.0
      LTDOT  = 0.0
      NLDOT  = 0.0
      NSDOT  = 0.0
      NRDOT  = 0.0
      NSDDOT = 0.0
      NSHDOT = 0.0
      NTOVR  = 0.0
      PCCSD  = 0.0
      PCLSD  = 0.0
      PCNL   = 0.0
      PCNRT  = 0.0
      PCNSD  = 0.0
      PCNSH  = 0.0
      PCNST  = 0.0
      PODWT  = 0.0
      RHOL   = 0.0
      RHOR   = 0.0
      RHOS   = 0.0
      RHOSH  = 0.0
      RTWT   = 0.0
      SDRATE = 0.0
      SDWT   = 0.0
      SDWTAM = 0.0
      SEEDNI = 0.0
      SEEDNO = 0.0

      SenWt  = 0.0
      SenLig = 0.0
      SenE   = 0.0
      SENESCE % ResWt  = 0.0
      SENESCE % ResLig = 0.0
      SENESCE % ResE   = 0.0
      SENESCE % CumResE(N) = 0.0

      SHELWT = 0.0
      SLAAD  = 0.0
      STMWT  = 0.0
      TGROW  = 0.0
      TOPWT  = 0.0
      TOTWT  = 0.0
      WCRLF  = 0.0
      WCRRT  = 0.0
      WCRSH  = 0.0
      WCRST  = 0.0
      WNRLF  = 0.0
      WNRRT  = 0.0
      WNRSH  = 0.0
      WNRST  = 0.0
      WSDDOT = 0.0
      WSHDOT = 0.0
      WTCO   = 0.0
      WTCSD  = 0.0
      WTLF   = 0.0
      WTLO   = 0.0
      WTLSD  = 0.0
      WTMAIN = 0.0
      WTNCAN = 0.0
      WTNLA  = 0.0
      WTNLF  = 0.0
      WTNLO  = 0.0
      WTNMOB = 0.0
      WTNNA  = 0.0
      WTNNAG = 0.0
      WTNNO  = 0.0
      WTNNOD = 0.0
      WTNOO  = 0.0
      WTNRA  = 0.0
      WTNRO  = 0.0
      WTNRT  = 0.0
      WTNSA  = 0.0
      WTNSD  = 0.0
      WTNSDA = 0.0
      WTNSDO = 0.0
      WTNSH  = 0.0
      WTNSHA = 0.0
      WTNSHO = 0.0
      WTNSO  = 0.0
      WTNST  = 0.0
      WTNTOT = 0.0
      WTNUP  = 0.0
      WTRO   = 0.0
      WTSDO  = 0.0
      WTSHO  = 0.0
      WTSO   = 0.0
      XLAI   = 0.0
      XHLAI  = 0.0
      XPOD   = 0.0

      CLW = 0.0   !Cumulative leaf growth
      CSW = 0.0   !Cumulative stem growth

      ShutMob = 0.0
      RootMob = 0.0
      ShelMob = 0.0

      SDPDOT = 0.0    !CHP - not used
      PUNDOT = 0.0    !CHP - not used

      NHSHWT = 0.0    !CHP - N loss due to Multi-Harvest for shell wt
      NHSDWT = 0.0    !CHP - N loss due to Multi-Harvest for seed wt
      NLPEST = 0.0    !CHP - N loss due to pest damage

!-----------------------------------------------------------------------
! KJB 02/12/03 Set SLA to zero until emergence
! JWJ 09/16/03 Set SLA to -99 until emergence
! CHP 09/17/03 Do this for output only - SLA is used in calculations!
      SLA = 0.0
!     Need to get initial value of F from DEMAND first  chp 9/14/98
!      IF (CROP .NE. 'FA') THEN
!        SLA    = F                 
!      ENDIF
C-----------------------------------------------------------------------
C     Net Lint growth rate
C-----------------------------------------------------------------------
      IF(CROP .EQ. 'CO') THEN
        CALL LTGROW(DYNAMIC, FILECC, FILEGC, ECONO,              !Input
     &    WSDDOT, TAVG, TURFAC, NSTRES,                          !Input
     &    LTDOT)                                                 !Output
      ENDIF
!***********************************************************************
!***********************************************************************
!     EMERGENCE CALCULATIONS - Performed once per season upon emergence
!         or transplanting of plants
!***********************************************************************
      ELSEIF (DYNAMIC == EMERG) THEN
!-----------------------------------------------------------------------
!     Net growth rates
      WLDOT  = 0.0
      WSDOT  = 0.0
      WRDOT  = 0.0
      WNDOT  = 0.0
      WPDOT  = 0.0
      WLFDOT = 0.0

C     Initial seedling or transplant weight
      SDRATE  = WTPSD * PLTPOP / 0.8 * 10

      IF (PLME .NE. 'T') THEN
        WTNEW  = WTFSD * WTPSD
        TOTWT  = WTNEW * PLTPOP
      ELSE
        TOTWT = SDWTPL / 10.
        WTNEW = TOTWT / PLTPOP
      ENDIF

!     Initial mass of plant components
      WTLF  = FRLF * TOTWT
      STMWT = FRSTM * TOTWT
      TOPWT = WTLF + STMWT
      RTWT  = TOTWT - TOPWT

      WLFI   = WTLF
      WSTI   = STMWT
      WRTI   = RTWT

!     Initialize cumulative variables
      CLW   = WTLF
      CSW   = STMWT

!     CH2O reserves
      WCRLF = ALPHL * WTLF
      WCRST = ALPHS * STMWT
      WCRRT = ALPHR * RTWT

!!     Initialize cumulative C variables
!      WCRLA = WCRLF
!      WCRSA = WCRST
!      WCRRA = WCRRT

!     Carbohydrate composition of plant components (fraction)
      RHOL   = ALPHL
      RHOR   = ALPHR
      RHOS   = ALPHS
      RHOSH  = ALPHSH

!     Net carbon addition variables
      WRCLDT = 0.0
      WRCSDT = 0.0
      WRCRDT = 0.0
      WRCSHD = 0.0

!     Compute N in plant components
      WTNLF  = WLFI * PROLFI * 0.16
      WTNST  = WSTI * PROSTI * 0.16
      WTNRT  = WRTI * PRORTI * 0.16
      WTNSH  = 0.0
      WTNSD  = 0.0
      WTNTOT = WTNLF + WTNST + WTNRT + WTNSH + WTNSD

!     Seed or transplant N at planting
      SDNPL  = WTPSD * SDPRO * 0.16 * 0.75 * PLTPOP -
     &  (WTNLF + WTNST + WTNRT)
      SDNPL  = MAX(SDNPL,0.0)
      SEEDNI = WTNLF + WTNST + WTNRT + SDNPL

!     Initialize cumulative N variables
      WTNLA  = WTNLF
      WTNSA  = WTNST
      WTNRA  = WTNRT

!     Percent N in plant components
      PCNL   = WTNLF / WLFI * 100.
      RNITP  = PCNL
      PCNST  = WTNST / WSTI * 100.
      PCNRT  = WTNRT / WRTI * 100.

!     Intialize leaf area.  Value of F initialized in DEMAND.
      AREALF = WTLF * F
      AREAH  = AREALF
      SLA    = AREALF / WTLF
      SLAAD  = AREALF / (WTLF - WCRLF)
      XLAI   = AREALF / 10000.
      XHLAI  = XLAI

C***********************************************************************
C***********************************************************************
C     Daily integration
C***********************************************************************
      ELSEIF (DYNAMIC == INTEGR) THEN
C-----------------------------------------------------------------------
      NLPEST = 0.0    !CHP - N loss due to pest damage

      GROWTH = WLDOTN + WSDOTN + WRDOTN + WSHDTN + WSDDTN + NODGR
  
      GRWRES = WLDOTN*CPFLF + WSDOTN*CPFSTM + WRDOTN*CPFRT +
     &         NODGR*CPFNOD + WSHDTN*CPFSH1 + WSDDTN*CPFSD1 +
     &         30./44*(TRNO3U/0.16 * 1.798 + TRNH4U/0.16 * 0.462 +
     &         NFIXN/0.16 * 1.798)

C-----------------------------------------------------------------------
C    Use 1982 Penning de Vries book, p. 125, composition of tissue,
C    fraction C for six categories, convert by 30/12 to CH20.
C-----------------------------------------------------------------------
C     Account for N added to existing plant tissue, e.g., NADLF, that
C     is damaged by insects, freezing, or senesced.  Otherwise, could
C     get increase in tissue N composition when tissue is aborted.  Need
C     to account for mass, N and C lost this way in sections below
C-----------------------------------------------------------------------
C       WLDOT = Net leaf growth rate
C-----------------------------------------------------------------------
      WLDOT = WLDOTN - SLDOT - WLIDOT - WLFDOT - NRUSLF/0.16 - CRUSLF

!     ShutMob is amount of leaf mass lost due to N and C mobilization
!     A positive value represents leaf mass lost. (kg/ha)
      ShutMob = (NRUSLF/0.16 + CRUSLF) * 10.      !kg/ha

      IF (WTLF > 1.E-4) THEN
        WLDOT = WLDOT + (CADLF+NADLF/0.16) *
     &    (1. - MIN(1.0,(SLDOT+WLIDOT+WLFDOT)/WTLF))
        ADD = (CADLF+NADLF/0.16) *
     &    (1. - MIN(1.0,(SLDOT+WLIDOT+WLFDOT)/WTLF))
        ShutMob = ShutMob - ADD * 10.             !kg/ha
      ELSE
        ADD = 0.0
      ENDIF
      IF (WLDOT < 0.0) THEN
        WLDOT = MAX(WLDOT, -WTLF)
      ENDIF
      
C-----------------------------------------------------------------------
C       WSDOT = Net stem growth rate
C-----------------------------------------------------------------------
      WSDOT = WSDOTN - SSDOT - WSIDOT - NRUSST / 0.16 - CRUSST
      ShutMob = ShutMob + (NRUSST / 0.16 + CRUSST) * 10.      !kg/ha

      IF (STMWT > 1.E-4) THEN
         WSDOT = WSDOT + (CADST+NADST/0.16) *
     &   (1. - MIN(1.0,(SSDOT+WSIDOT)/STMWT))
        ADD = (CADST+NADST/0.16) *
     &    (1. - MIN(1.0,(SSDOT+WSIDOT)/STMWT))
        ShutMob = ShutMob - ADD * 10.                         !kg/ha
      ELSE
         ADD = 0.
      ENDIF
      IF (WSDOT < 0.0) THEN
        WSDOT = MAX(WSDOT, -STMWT)
      ENDIF
C-----------------------------------------------------------------------
C     Net root growth rate
C-----------------------------------------------------------------------
      WRDOT = WRDOTN - SRDOT - NRUSRT/0.16 - CRUSRT - WRIDOT
      RootMob = (NRUSRT/0.16 + CRUSRT) * 10.      !kg/ha

      IF (RTWT > 1.E-4) THEN
         WRDOT = WRDOT + (NADRT/0.16) *
     &   (1. - MIN(1.0,(SRDOT+WRIDOT)/RTWT))
        ADD = (NADRT/0.16) *
     &    (1. - MIN(1.0,(SRDOT+WRIDOT)/RTWT))
        RootMob = RootMob - ADD * 10.             !kg/ha
      ELSE
        ADD = 0.0
      ENDIF

      IF (WRDOT < 0.0) THEN
        WRDOT = MAX(WRDOT, -RTWT)
      ENDIF
C-----------------------------------------------------------------------
C     Net shell growth rate
C-----------------------------------------------------------------------
      WSHIDT = MIN(WSHIDT,SHELWT)     ! pest damage to shells      
      WSHDOT = WSHDTN - WSHIDT - WTABRT - TOSHMINE - TOCHMINE - HSHELWT
      IF (WSHDOT .LT. 0.0) THEN
        WSHDOT = MAX(WSHDOT, -SHELWT)
      ENDIF      
      ShelMob = (TOSHMINE + TOCHMINE) * 10.    !kg/ha
C-----------------------------------------------------------------------
C     Net seed growth rate
C-----------------------------------------------------------------------
      SWIDOT = MIN(SWIDOT,SDWT)       ! pest damage to seeds
      WSDDOT = WSDDTN - SWIDOT - HSDWT
      IF (WSDDOT .LT. 0.0) THEN
        WSDDOT = MAX(WSDDOT, -SDWT)
      ENDIF
            
      WTLSD  = WTLSD + WSDDOT * POTLIP    !lipids in seed
      WTCSD  = WTCSD + WSDDOT * POTCAR    !carbohydrates in seed
C-----------------------------------------------------------------------
C     Net Lint growth rate
C-----------------------------------------------------------------------
      IF(CROP .EQ. 'CO') THEN
        CALL LTGROW(DYNAMIC, FILECC, FILEGC, ECONO,              !Input
     &    WSDDOT, TAVG, TURFAC, NSTRES,                          !Input
     &    LTDOT)                                                 !Output

        IF (LTDOT .LT. 0.0) THEN
            LTDOT = MAX(LTDOT, -LINTW)
        ENDIF
      ENDIF
      
C-----------------------------------------------------------------------
C     Net nodule growth rate
C-----------------------------------------------------------------------
      WNDOT  = NODGR - NDTH            

C-----------------------------------------------------------------------
C     Net pod growth rate
C-----------------------------------------------------------------------
      WPDOT = WSHDOT + WSDDOT - HPODWT
      IF (WPDOT .LT. 0.0) THEN
        WPDOT = MAX(WPDOT, -PODWT)
      ENDIF        
C-----------------------------------------------------------------------
C     Total Net plant growth rate
C-----------------------------------------------------------------------
      WDOT = WLDOT + WSDOT + WRDOT + WPDOT + WNDOT 

C-----------------------------------------------------------------------
      IF (GROWTH > 1.E-4) THEN
         IF (XPODF == 'PD') THEN
           XPOD = 0.17 * (WSDDTN+WSHDTN)/GROWTH + 0.83 * XPOD
         ELSE IF (XPODF == 'SD') THEN
           XPOD = 0.17 * (WSDDTN)/GROWTH + 0.83 * XPOD
         ELSE
           CALL ERROR(ERRKEY,1,'      ',0)
         ENDIF
      ENDIF
C-----------------------------------------------------------------------
C    Integration, Add Today's Net Growth to Existing Weights
C-----------------------------------------------------------------------
      TOTWT  = TOTWT  + WDOT
      TOPWT  = TOPWT  + WSDOT + WLDOT + WPDOT
      WTLF   = WTLF   + WLDOT
      STMWT  = STMWT  + WSDOT
      SDWT   = SDWT   + WSDDOT
      SHELWT = SHELWT + WSHDOT
      RTWT   = RTWT   + WRDOT
      PODWT  = PODWT  + WPDOT
      DWNOD  = DWNOD  + WNDOT
      TGROW  = TGROW  + GROWTH
      LINTW  = LINTW  + LTDOT

C-----------------------------------------------------------------------
C     Cumulative leaf and stem growth
C-----------------------------------------------------------------------
      CLW = CLW + WLDOTN      
      CSW = CSW + WSDOTN      

C-----------------------------------------------------------------------
C     Store values of TOPWT at flowering and SDWT at maturity
C-----------------------------------------------------------------------
      IF (YRDOY == YRNR1) THEN
         CANWAA = TOPWT
      ELSEIF (YRDOY == MDATE) THEN
         SDWTAM = SDWT
      ENDIF
C---------------------------------------------------------------------
C     Compute Seed Weight for Which there is Maintenance Costs.
C-----------------------------------------------------------------------
      WSDMAN = MIN(SDWT,SHELWT)
      WTMAIN = TOTWT - SDWT + WSDMAN

C-----------------------------------------------------------------------
C     Carbon Reserves:  Net Growth Rates for Mobile Carbohydrates
C-----------------------------------------------------------------------
C     Account for N added to existing plant tissue, e.g., NADLF, that
C     is damaged by insects, freezing, or senesced.  Otherwise, could
C     get increase in tissue N composition when tissue is aborted.  Need
C     to account for mass, N and C lost this way in sections below
C-----------------------------------------------------------------------
      WRCLDT = ALPHL * WLDOTN - CRUSLF - RHOL*(SLNDOT+WLIDOT+WLFDOT)
      IF (WTLF > 1.E-4) THEN
         WRCLDT = WRCLDT + CADLF *
     &     (1. - MIN(1.0,(SLDOT+WLIDOT+WLFDOT)/WTLF))
      ENDIF

      WRCSDT = ALPHS * WSDOT - CRUSST - RHOS * SSNDOT
      IF (STMWT > 1.E-4) THEN
         WRCSDT = WRCSDT + CADST *
     &     (1. - MIN(1.0,(SSDOT+WSIDOT)/STMWT))
      ENDIF

      WRCRDT = ALPHR * WRDOT - CRUSRT - RHOR * SRDOT
      WRCSHD = ALPHSH * WSHDOT - CRUSSH - RHOSH*(WTABRT+WTSHMT+WSHIDT)
C-----------------------------------------------------------------------
C     Update C Storage, Concentrations in Leaves, Stems, Roots, and Shells
C-----------------------------------------------------------------------
      WCRLF = WCRLF + WRCLDT
      WCRST = WCRST + WRCSDT
      WCRRT = WCRRT + WRCRDT
      WCRSH = WCRSH + WRCSHD

      IF (WCRLF .LE. 1.0E-30) WCRLF = 0.0
      IF (WCRST .LE. 1.0E-30) WCRST = 0.0
      IF (WCRRT .LE. 1.0E-30) WCRRT = 0.0
      IF (WCRSH .LE. 1.0E-30) WCRSH = 0.0

C-----------------------------------------------------------------------
C     Compute Cumulative C loss During Season
C-----------------------------------------------------------------------
      WTLO  = WTLO  + SLDOT  + WLIDOT + WLFDOT
      WTSO  = WTSO  + SSDOT  + WSIDOT
      WTRO  = WTRO  + SRDOT  + WRIDOT
      WTSHO = WTSHO + WTABRT + WSHIDT
      WTSDO = WTSDO + SWIDOT
      WTNOO = WTNOO + NDTH
      WTCO  = WTLO  + WTSO   + WTSHO  + WTSDO

C-----------------------------------------------------------------------
C     Compute CH20 fractions
C-----------------------------------------------------------------------
      IF (WTLF > 0.0001) THEN
         RHOL =  WCRLF/WTLF
      ELSE
         RHOL = 0.0
      ENDIF

      IF (STMWT > 0.0001) THEN
         RHOS =  WCRST/STMWT
      ELSE
         RHOS = 0.0
      ENDIF

      IF (RTWT > 0.0001) THEN
         RHOR =  WCRRT/RTWT
      ELSE
         RHOR = 0.0
      ENDIF

      IF (SHELWT > 0.0001) THEN
         RHOSH =  WCRSH/SHELWT
      ELSE
         RHOSH = 0.0
      ENDIF

C=======================================================================
C    Nitrogen Balance:  Net Growth Rates for Nitrogen Components
C=======================================================================
C     Account for N added to existing plant tissue, e.g., NADLF, that
C     is damaged by insects, freezing, or senesced.  Otherwise, could
C     get increase in tissue N composition when tissue is aborted.  Need
C     to account for mass, N and C lost this way in sections below
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C     Leaf nitrogen senescence and pest damage loss
C-----------------------------------------------------------------------
!              senesc. + pest   + freeze  
      NLOFF  = (SLNDOT + WLIDOT + WLFDOT) * (PCNL/100.) +
     &         (SLDOT-SLNDOT) * PROLFF * 0.16
      NLPEST = NLPEST + WLIDOT * PCNL/100.
      IF (NLOFF. LT. 0.0) THEN
        NLOFF = 0.0
      ENDIF

C-----------------------------------------------------------------------
C     Net growth rate of nitrogen in the leaves
C-----------------------------------------------------------------------
      NLDOT = NGRLF - NLOFF - NRUSLF
      IF (WTLF > 1.E-4) THEN
         NLDOT = NLDOT + NADLF *
     &                  (1. - MIN(1.0,(SLDOT + WLIDOT + WLFDOT) / WTLF))
      ENDIF

C-----------------------------------------------------------------------
C     Stem nitrogen senescence and pest damage loss
C-----------------------------------------------------------------------
!              senesce + pest
      NSOFF  = (SSNDOT + WSIDOT) * (PCNST/100.) + (SSDOT - SSNDOT)
     &         * PROSTF * 0.16
      NLPEST = NLPEST + WSIDOT * PCNST/100.
      IF (NSOFF < 0.0) THEN
        NSOFF = 0.0
      ENDIF

C-----------------------------------------------------------------------
C     Net growth rate of nitrogen in the stems
C-----------------------------------------------------------------------
      NSDOT = NGRST - NSOFF - NRUSST
      IF (STMWT > 1.E-4) THEN
         NSDOT = NSDOT + NADST * 
     &                  (1. - MIN(1.0,(SSDOT + WSIDOT) / STMWT))
      ENDIF

C-----------------------------------------------------------------------
C     Root nitrogen senescence and pest damage loss
C-----------------------------------------------------------------------
      NROFF  = (SRDOT+WRIDOT) * (PCNRT/100.)
      IF (NROFF < 0.0) THEN
         NROFF = 0.0
      ENDIF

C-----------------------------------------------------------------------
C     Net growth rate of nitrogen in the roots
C-----------------------------------------------------------------------
      NRDOT = NGRRT - NROFF - NRUSRT
      IF (RTWT > 1.E-4) THEN
         NRDOT = NRDOT + NADRT * 
     &                  (1. - MIN(1.0,(SRDOT + WRIDOT) / RTWT))
      ENDIF

C-----------------------------------------------------------------------
C     Shell nitrogen senescence, abortion and pest damage loss
C-----------------------------------------------------------------------
      NSHOFF = (WTABRT+WSHIDT+HSHELWT) * (PCNSH/100.)
      NHSHWT = NHSHWT + HSHELWT * PCNSH/100.
      NLPEST = NLPEST + WSHIDT * PCNSH/100.
      IF (NSHOFF < 0.0) THEN
        NSHOFF = 0.0
      ENDIF

C-----------------------------------------------------------------------
C     Net growth rate of nitrogen in shells
C-----------------------------------------------------------------------
      NSHDOT = NGRSH - NSHOFF - NRUSSH

C-----------------------------------------------------------------------
C     Seed nitrogen senescence, abortion and pest damage loss
C-----------------------------------------------------------------------
      NSDOFF = (SWIDOT+HSDWT) * (PCNSD/100.)
      NHSDWT = NHSDWT + HSDWT * PCNSH/100.
      IF (NSDOFF < 0.0) THEN
         NSDOFF = 0.0
      ENDIF

C-----------------------------------------------------------------------
C     Net growth rate of nitrogen in seeds
C-----------------------------------------------------------------------
      NSDDOT = NGRSD - NSDOFF

C-----------------------------------------------------------------------
C   Integration, Update N in Each Component by Adding Today's Growth
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C     Total nitrogen in the leaves
C-----------------------------------------------------------------------
      IF ((NLDOT < 0.0) .AND. (ABS(NLDOT) > WTNLF)) THEN
         NLDOT = - WTNLF
      ENDIF
      WTNLF = WTNLF + NLDOT

C-----------------------------------------------------------------------
C     Total nitrogen in the stems
C-----------------------------------------------------------------------     
      IF ((NSDOT < 0.0) .AND. (ABS(NSDOT) > WTNST)) THEN
         NSDOT = - WTNST
      ENDIF
      WTNST = WTNST + NSDOT

C-----------------------------------------------------------------------
C     Total nitrogen in the roots
C-----------------------------------------------------------------------
      IF ((NRDOT < 0.0) .AND. (ABS(NRDOT) > WTNRT)) THEN
         NRDOT = - WTNRT
      ENDIF
      WTNRT = WTNRT + NRDOT

C-----------------------------------------------------------------------
C     Total nitrogen in the nodules
C-----------------------------------------------------------------------
      WTNNOD = DWNOD * 0.16 * PRONOD

C-----------------------------------------------------------------------
C     Total nitrogen in the shells
C-----------------------------------------------------------------------
      IF ((NSHDOT < 0.0) .AND. (ABS(NSHDOT) > WTNSH)) THEN
         NSHDOT = - WTNSH
      ENDIF
      WTNSH = WTNSH + NSHDOT

      IF ((NSDDOT < 0.0) .AND. (ABS(NSDDOT) > WTNSD)) THEN
         NSDDOT = - WTNSD
      ENDIF

C-----------------------------------------------------------------------
C     Total nitrogen in the seed
C-----------------------------------------------------------------------
      WTNSD = WTNSD + NSDDOT

C-----------------------------------------------------------------------
C     Total nitrogen in the plant
C-----------------------------------------------------------------------
      WTNTOT = WTNLF + WTNST + WTNRT + WTNSH + WTNSD + WTNNOD

C-----------------------------------------------------------------------
C     Total nitrogen in the canopy (all above ground components)
C-----------------------------------------------------------------------
      WTNCAN = WTNLF + WTNST + WTNSH + WTNSD

C-----------------------------------------------------------------------
C     Save total nitrogen in the canopy at the start of flowering
C-----------------------------------------------------------------------
      IF (YRDOY == YRNR1) THEN
         CANNAA = WTNCAN
      ENDIF

C-----------------------------------------------------------------------
C     Compute Cumulative N added during season
C-----------------------------------------------------------------------
!   CHP - working on Plant N balance:
!   Add adjustment factors for N addition.  
!     These factors are used in computation of net addition to 
!     tissue, but previously not here.  Adding these statements
!     forces cumulative N additions to equal 'current N' plus 
!     'senesced N'.  

!   However . . .  by making this change, cumulative tissue N no longer
!     matches 'Seed N' plus 'N2 fixed' plus 'N uptake'.  Why???

!     Leaf
      NLALL = NGRLF - NRUSLF 
      IF (WTLF > 1.E-4) THEN
        NLALL = NLALL + NADLF * 
     &                (1. - MIN(1.0,(SLDOT + WLIDOT + WLFDOT) / WTLF))
      ENDIF

!     Stem
      NSALL = NGRST - NRUSST 
      IF (STMWT > 1.E-4) THEN
        NSALL = NSALL + NADST * 
     &                  (1. - MIN(1.0,(SSDOT + WSIDOT) / STMWT))
      ENDIF

!     Root
      NRALL = NGRRT - NRUSRT
      IF (RTWT > 1.E-4) THEN
        NRALL = NRALL + NADRT * 
     &                  (1. - MIN(1.0,(SRDOT + WRIDOT) / RTWT))
      ENDIF

!     Shell and seed
      NSHALL = NGRSH - NRUSSH
      NSDALL = NGRSD

      WTNLA = WTNLA + NLALL
      WTNSA = WTNSA + NSALL
      WTNRA = WTNRA + NRALL
      WTNSHA = WTNSHA + NSHALL
      WTNSDA = WTNSDA + NSDALL
C-----------------------------------------------------------------------
C    Nodules
C-----------------------------------------------------------------------
      IF (ISWNIT == 'Y' .AND. ISWSYM == 'Y') THEN
         DWNODA = DWNODA + NODGR
         WTNFX = WTNFX + NFIXN + (NODGR * 0.16 * PRONOD) - NTOVR
         WTNNAG = DWNODA * 0.16 * PRONOD
      ENDIF
C-----------------------------------------------------------------------
C     Compute Cumulative N loss During Season
C-----------------------------------------------------------------------
      WTNLO  = WTNLO  + NLOFF
      WTNSO  = WTNSO  + NSOFF
      WTNRO  = WTNRO  + NROFF
      WTNSHO = WTNSHO + NSHOFF
      WTNSDO = WTNSDO + NSDOFF
      IF (ISWNIT == 'Y' .AND. ISWSYM == 'Y') THEN
         NNOFF  = NDTH * 0.16 * PRONOD
         WTNNO  = WTNNO  + NNOFF
      ENDIF
C-----------------------------------------------------------------------
C     N Balance Components for Mobilized, Uptake, and Fixed N
C-----------------------------------------------------------------------
      WTNMOB = WTNMOB + NMINEA
      WTNUP = WTNUP + TRNU
      WTNNA = WTNNOD + WTNNO
C-----------------------------------------------------------------------
C     Compute Percentage N in each Plant Component
C-----------------------------------------------------------------------
      IF ((WTLF > 1.E-4) .AND. (WTLF > WTNLF) .AND. 
     &   (WTNLF > 1.E-5)) THEN
         PCNL = WTNLF / WTLF * 100.0
      ELSE
         PCNL = 0.0
      ENDIF

      IF ((STMWT > 1.E-4) .AND. (WTNST > 1.E-5)) THEN
         PCNST = WTNST / STMWT * 100.0
      ELSE
         PCNST = 0.0
      ENDIF

      IF ((RTWT > 1.E-4) .AND. (WTNRT > 1.E-5)) THEN
         PCNRT = WTNRT / RTWT * 100.0
      ELSE
         PCNRT = 0.0
      ENDIF

      IF ((SHELWT > 1.E-4) .AND. (WTNSH > 1.E-5)) THEN
         PCNSH = WTNSH / SHELWT * 100.0
      ELSE
         PCNSH = 0.0
      ENDIF
C-----------------------------------------------------------------------
C     Compute Percentage Seed Composition
C-----------------------------------------------------------------------
      IF (SDWT > 0.01) THEN
         PCNSD = WTNSD / SDWT * 100.0
         PCLSD = WTLSD / SDWT * 100.0
         PCCSD = WTCSD / SDWT * 100.0
      ELSE
         PCNSD = 0.0
         PCLSD = 0.0
         PCCSD = 0.0
      ENDIF
C-----------------------------------------------------------------------
C    Calculate true nitrogen concentration in leaf tissue for
C     photosynthesis reduction.
C-----------------------------------------------------------------------
      IF ((WTLF - WCRLF) > 1.E-4) THEN
         RNITP = 100.*WTNLF/(WTLF-WCRLF)
      ELSE
         RNITP = PCNMIN
      ENDIF
C-----------------------------------------------------------------------
C     Calculate Remaining N in Shells, Leaves, Stems, and Roots
C     That can be Mined (Plant N-Balance).
C-----------------------------------------------------------------------
      IF ((WTLF - WCRLF) > 1.E-4) THEN
        WNRLF = MAX (WTNLF - PROLFF * 0.16 * (WTLF-WCRLF), 0.0)
      ELSE
        WNRLF = 0.0
      ENDIF

      IF ((STMWT - WCRST) > 1.E-4) THEN
        WNRST = MAX (WTNST - PROSTF * 0.16 * (STMWT-WCRST), 0.0)
      ELSE
        WNRST = 0.0
      ENDIF

      IF (RTWT > 1.E-4) THEN
        WNRRT = MAX (WTNRT - PRORTF * 0.16 * RTWT, 0.0)
      ELSE
        WNRRT = 0.0
      ENDIF

      IF (SHELWT > 1.E-4) THEN
        WNRSH = MAX (WTNSH - PROSHF * 0.16 * SHELWT, 0.0)
      ELSE
        WNRSH = 0.0
      ENDIF

C-----------------------------------------------------------------------
C     This section added to provide senescence parameters to the 
C     Soil N routines (senescence and freeze values are added to soil
C     residue; pest damage components are lost from the system)
C-----------------------------------------------------------------------
C     Surface carbon includes all senescence and freeze variables for
C      leaf (SLDOT, WLFDOT), stem (SSDOT), and shell (WTABRT)
C       At this time, the model does not senesce seed.   
C     Convert from biomass to C with a factor 0.40.   
C-----------------------------------------------------------------------
      SenWt(0) = (SLDOT + WLFDOT + SSDOT + WTABRT)
C     Convert from g/m2 to kg/ha with a factor of 10.
      SenWt(0) = AMAX1(SenWt(0), 0.) * 10.0    !kg[dry matter]/ha
      
C-----------------------------------------------------------------------
C     Surface nitrogen includes nitrogen losses computed above minus the
C       pest damage components. 
C-----------------------------------------------------------------------
      SenE(0,1) = NLOFF + NSOFF + NSHOFF - NLPEST  
C     Convert from g/m2 to kg/ha with a factor of 10.
      SenE(0,1) = AMAX1 (SenE(0,1),0.) * 10.0             !kg[N]/ha

C-----------------------------------------------------------------------
C     Surface phosphorus senescence.  - do this is P routine
C-----------------------------------------------------------------------
!      SenE(0,2) = SenWt(0) * PConc_Shut             !kg[N]/ha

C-----------------------------------------------------------------------
C     Contribution of lignin to surface litter from senesced and frozen 
C       plant matter
C-----------------------------------------------------------------------
      SenLig(0) = (SLDOT + WLFDOT) * PLIGLF + SSDOT * PLIGST +
     &    WTABRT * PLIGSH
C     Convert from g/m2 to kg/ha with a factor of 10.
      SenLig(0) = AMAX1 (SenLig(0),0.) * 10.0             !kg[lig]/ha

C-----------------------------------------------------------------------
C     Senescence of roots and nodules (kg/ha)
C-----------------------------------------------------------------------
      DO L = 1, NLAYR 
        SenWt(L)  = SENRT(L) + SENNOD(L)            !kg[dry matter]/ha
        SenLig(L) = SENRT(L) * PLIGRT + SENNOD(L) * PLIGNO   !kg[lig]/ha
        !SenE(L,1) = (SENRT(L)* PRORTF + SENNOD(L) * PRONOD) * 0.16 
!       01/19/2006 CHP Root senescence at current N% (per KJB and JWJ)
        SenE(L,1) = SENRT(L) * PCNRT / 100. + SENNOD(L) * PRONOD * 0.16 
      ENDDO

C-----------------------------------------------------------------------
C     Leaf Area, Specific Leaf Area Calculations
C     Calculate Area Growth rate and Update Leaf Area (AREALF, XLAI)
C-----------------------------------------------------------------------
      ALFDOT = WLDOTN*F - (SLDOT+WLIDOT+WLFDOT+NRUSLF/0.16)*SLA
      IF (WTLF > 1.E-4) THEN
         ALFDOT = ALFDOT + SLA * (NADLF/0.16) *
     &   (1. - MIN(1.0,(SLDOT+WLIDOT+WLFDOT)/WTLF))
      ENDIF

      AREALF = AREALF + ALFDOT
      XLAI   = AREALF / 10000.
      IF (AREALF > 0.00001 .AND. WTLF > 1.E-4) THEN
         SLA    = AREALF / WTLF
         SLAAD  = AREALF / (WTLF - WCRLF)
         IF (SLA > 999.) SLA = 0.0
         IF (SLAAD > 999.) SLAAD = -99.
      ENDIF
C-----------------------------------------------------------------------
C     Remember XLAI
C-----------------------------------------------------------------------
      LAIMX = MAX(XLAI,LAIMX)
C-----------------------------------------------------------------------
C     Calculate "Healthy" or Non-Diseased Leaf Area Index
C-----------------------------------------------------------------------
!     AREAH  = AREALF - 2. * DISLA
!     KJB Remove 2. factor
      AREAH  = AREALF - DISLA
      AREAH  = MAX(0.,AREAH)
      XHLAI  = AREAH / 10000.
C-----------------------------------------------------------------------
C     Integrate Pest Damage to Seeds
C-----------------------------------------------------------------------
      PUNCSD = PUNCSD + SDPDOT           !chp not used
      SEEDNO = SEEDNO - SDIDOT
      PUNCTR = PUNCTR + PUNDOT           !chp not used
C-----------------------------------------------------------------------
C     Loss in Plants due to Pests, Adjust Plant Spacing Also
C-----------------------------------------------------------------------
      IF (NPLTD > 1.E-5) THEN
         PLTPOP = PLTPOP - NPLTD
         PLTPOP = MAX(0.,PLTPOP)
         IF (PLTPOP*ROWSPC > 1.E-4) THEN
            BETN = 1.0/(ROWSPC*PLTPOP)
         ENDIF
      ENDIF

      IF (PPLTD > 1.E-4) THEN
         PLTPOP = PLTPOP - PPLTD*PLTPOP/100.
         PLTPOP = MAX(0.,PLTPOP)
         IF(PLTPOP*ROWSPC > 1.E-4) THEN
            BETN = 1.0/(ROWSPC*PLTPOP)
         ENDIF
      ENDIF

C-----------------------------------------------------------------------
C     Terminate growth if stress causes extremely low plant weights
C-----------------------------------------------------------------------
      IF (TOPWT < 0.00001 .OR. STMWT < 0.00001) THEN
        CALL STRESS(
     &    AGEFAC, DWNOD, IDETO, IHARI, NOUTDO, PODWT,     !Input
     &    RTWT, SDWT, SHELWT, STMWT, TOPWT,               !Input
     &    TOTWT, TURFAC, WTLF, YRDOY, YRPLT,              !Input
     &    MDATE, CropStatus)                              !Output
        RETURN
      ENDIF

      IF (IHARI .NE. 'R' .AND. IHARI .NE. 'D') THEN
        IF (RTWT  < 0.00001 .OR. WTLF  < 0.00001) THEN
          CALL STRESS( 
     &      AGEFAC, DWNOD, IDETO, IHARI, NOUTDO, PODWT,   !Input
     &      RTWT, SDWT, SHELWT, STMWT, TOPWT,             !Input
     &      TOTWT, TURFAC, WTLF, YRDOY, YRPLT,            !Input
     &      MDATE, CropStatus)                            !Output
          RETURN
        ENDIF
      ENDIF

      SENESCE % ResWt  = SenWt
      SENESCE % ResLig = SenLig
      DO L = 0, NLAYR
        SENESCE % ResE(L,N)  = SenE(L,N)
!        SENESCE % ResE(L,P)  = SenE(L,P)

!        This is being done in OpSoilOrg:
!        SENESCE % CumResWt   = SENESCE % CumResWt + SenWt(L)
!        SENESCE % CumResE(N) = SENESCE % CumResE(N) + SenE(L,N)

!        Do this in P module:
!        SENESCE % CumResE(P) = SENESCE % CumResE(P) + SenE(L,P)
      ENDDO

C***********************************************************************
C***********************************************************************
C     END OF DYNAMIC IF CONSTRUCT
C***********************************************************************
      ENDIF
C-----------------------------------------------------------------------
      RETURN
      END  ! SUBROUTINE GROW
C=======================================================================

C=======================================================================
C  STRESS, Subroutine, from code in GROW subroutine
C-----------------------------------------------------------------------
C  Plant death due to stress
C-----------------------------------------------------------------------
C  REVISION        HISTORY
C  09/18/1998 CHP  Written based on code in GROW subroutine
!  06/15/2022 CHP Added CropStatus
!-----------------------------------------------------------------------
!  Called by:  GROW
!  Calls:      None
C=======================================================================
      SUBROUTINE STRESS(
     &  AGEFAC, DWNOD, IDETO, IHARI, NOUTDO, PODWT,       !Input
     &  RTWT, SDWT, SHELWT, STMWT, TOPWT,                 !Input
     &  TOTWT, TURFAC, WTLF, YRDOY, YRPLT,                !Input
     &  MDATE, CropStatus)                                !Output
!-----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL YR_DOY, WARNING, TIMDIF
!-----------------------------------------------------------------------
      CHARACTER*1  IDETO, IHARI
      CHARACTER*78 MESSAGE(10)
      INTEGER NOUTDO, YRDOY, YRPLT, MDATE, DAP, TIMDIF
      INTEGER YR, DOY, CropStatus
      REAL AGEFAC, DWNOD, PODWT, RTWT, SDWT,
     &  SHELWT, STMWT, TOPWT, TOTWT, TURFAC, WTLF

!-----------------------------------------------------------------------
!     Set Ending Plant Weights, Dates of Stages if Plants Died
!-----------------------------------------------------------------------
      TOTWT  = MAX(0.,TOTWT)
      TOPWT  = MAX(0.,TOPWT)
      WTLF   = MAX(0.,WTLF)
      STMWT  = MAX(0.,STMWT)
      SDWT   = MAX(0.,SDWT)
      SHELWT = MAX(0.,SHELWT)
      RTWT   = MAX(0.,RTWT)
      PODWT  = MAX(0.,PODWT)
      DWNOD  = MAX(0.,DWNOD)

      IF (MDATE < 0) THEN
!        NR8   = MAX(0,TIMDIF(YRSIM,YRDOY))
        MDATE = YRDOY
        CropStatus = 39
      ENDIF
C-----------------------------------------------------------------------
      IF (IHARI == 'M') THEN
        DAP   = MAX(0,TIMDIF(YRPLT,YRDOY))

        !Message to WARNING.OUT
        CALL YR_DOY(YRDOY, YR, DOY)
        WRITE(MESSAGE(1), 100) DAP
        WRITE(MESSAGE(2), 110) YR, DOY, TURFAC, AGEFAC
        CALL WARNING(2,'CRPGRO', MESSAGE)
  100 FORMAT('Plant died due to extreme stress at ',I3,
     &         ' days after planting.')
  110 FORMAT('(DAY : ',I4,1X,I3,'; TURFAC : ',F5.2,'; AGEFAC : ',
     &            F5.2,'  )')

!       Message to screen
!       WRITE (*,275) MESSAGE(1), MESSAGE(2)
!       Message to Overview.out
        IF (IDETO == 'Y') THEN
          WRITE (NOUTDO,275) MESSAGE(1), MESSAGE(2)
        ENDIF
  275   FORMAT(/,2X,A78,/,2X,A78)
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END  ! SUBROUTINE STRESS
C=======================================================================

C=======================================================================
C  IPGROW, Subroutine, C. H. Porter
C-----------------------------------------------------------------------
C  Reads input data for GROW routines.
C----------------------------------------------------------------------
C  REVISION HISTORY
C  09/15/1998 CHP Written
C  08/12/2003 CHP Added I/O error checking
!  11/26/2007 CHP THRESH, SDPRO, SDLIP moved from eco to cul file
C-----------------------------------------------------------------------
C  Called : GROW
C  Calls  : FIND, ERROR, IGNORE
C=======================================================================
      SUBROUTINE IPGROW(
     &  FILEIO, FILECC,  CROP,                            !Input
     &  ALPHL,  ALPHR,  ALPHS,  ALPHSH,                   !Output
     &  PCARLF, PCARST, PCARRT, PCARSH, PCARSD, PCARNO,   !Output
     &  PLIGLF, PLIGST, PLIGRT, PLIGSH, PLIGSD, PLIGNO,   !Output
     &  PLIPLF, PLIPST, PLIPRT, PLIPSH,         PLIPNO,   !Output
     &  PMINLF, PMINST, PMINRT, PMINSH, PMINSD, PMINNO,   !Output
     &  POALF,  POAST,  POART,  POASH,  POASD,  POANO,    !Output
     &  PROLFF, PROSTF, PRORTF, PROSHF,         PRONOD,   !Output
     &  PROLFI, PROSTI, PRORTI,                           !Output
     &  PLTPOP, ROWSPC, RMIN,   PLME,   SDWTPL,           !Output
     &  SDLIP,  SDPRO,  WTFSD,  WTPSD,   XPODF)           !Output

!-----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, ERROR, IGNORE, TIMDIF, UPCASE
!-----------------------------------------------------------------------
      CHARACTER*1 PLME, UPCASE
      CHARACTER*2 XPODF, CROP
      CHARACTER*6 ERRKEY
      PARAMETER (ERRKEY = 'IPGROW')
      CHARACTER*6 SECTION, ECONO  !, ECOTYP
      CHARACTER*30 FILEIO
      CHARACTER*80 C80
      CHARACTER*92 FILECC
!      CHARACTER*255 C255

      INTEGER LUNCRP, LUNIO, I    !, LUNECO
      INTEGER ERR, LINC, LNUM, FOUND, ISECT

      REAL ROWSPC, RMIN, PLTPOP, WTFSD, WTPSD,
     &  SDPRO, SDLIP, SDWTPL,
     &  ALPHL,  ALPHS,  ALPHR,  ALPHSH,
     &  PROLFF, PROSTF, PRORTF, PROSHF,                 PRONOD,
     &  PROLFI, PROSTI, PRORTI,
     &  PCARLF, PCARST, PCARRT, PCARSH, PCARSD, PCARNO,
     &  PLIPLF, PLIPST, PLIPRT, PLIPSH,                 PLIPNO,
     &  PLIGLF, PLIGST, PLIGRT, PLIGSH, PLIGSD, PLIGNO,
     &  POALF,  POAST,  POART,  POASH,  POASD,  POANO,
     &  PMINLF, PMINST, PMINRT, PMINSH, PMINSD, PMINNO

!-----------------------------------------------------------------------
      CALL GETLUN('FILEIO', LUNIO)
      OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
      LNUM = 0
!-----------------------------------------------------------------------
!    Find and read Cultivar Section
!-----------------------------------------------------------------------
      SECTION = '*CULTI'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND == 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ (LUNIO,'(3X,A2)',IOSTAT=ERR) CROP; LNUM = LNUM + 1
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
      ENDIF

      IF (CROP .NE. 'FA') THEN
!-----------------------------------------------------------------------
!    Read Planting Details Section
!-----------------------------------------------------------------------
        SECTION = '*PLANT'
        CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND == 0) THEN
          CALL ERROR(SECTION, 42, FILEIO, LNUM)
        ELSE
C-GH      READ(LUNIO,'(24X,F6.1,5X,A1,6X,F6.0,12X,F6.0)',IOSTAT=ERR)
          READ(LUNIO,'(24X,F6.0,5X,A1,6X,F6.0,12X,F6.0)',IOSTAT=ERR)
     &            PLTPOP, PLME, ROWSPC, SDWTPL ; LNUM = LNUM + 1
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
        ENDIF

!-----------------------------------------------------------------------
!    Read Cultivar Section
!-----------------------------------------------------------------------
        SECTION = '*CULTI'
        CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND == 0) THEN
          CALL ERROR(SECTION, 42, FILEIO, LNUM)
        ELSE
          READ(LUNIO,'(24X,A6,66X,F6.0,24X,2F6.0)',IOSTAT=ERR) 
     &        ECONO, WTPSD, SDPRO, SDLIP
          LNUM = LNUM + 1
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
        ENDIF

      ENDIF
  
      CLOSE (LUNIO)

      IF (CROP .NE. 'FA') THEN
!-----------------------------------------------------------------------
!     Read in values from input file, which were previously input
!       in Subroutine IPCROP.
!-----------------------------------------------------------------------
        CALL GETLUN('FILEC', LUNCRP)
        OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)
        LNUM = 0
!-----------------------------------------------------------------------
!    Find and Read Respiration Section
!-----------------------------------------------------------------------
!     Subroutine FIND finds appropriate SECTION in a file by
!     searching for the specified 6-character string at beginning
!     of each line.
!-----------------------------------------------------------------------
        SECTION = '!*RESP'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND == 0) THEN
          CALL ERROR(SECTION, 42, FILECC, LNUM)
        ELSE
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(24X,F6.0)',IOSTAT=ERR) RMIN
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDIF
  
        SECTION = '!*PLAN'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND == 0) THEN
          CALL ERROR(SECTION, 42, FILECC, LNUM)
        ELSE
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(F6.0,6X,2F6.0,6X,F6.0)',IOSTAT=ERR)
     &                    PROLFI, PROLFF, PROSTI, PROSTF
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(F6.0,6X,F6.0,12X,F6.0)',IOSTAT=ERR)
     &                    PRORTI, PRORTF, PROSHF
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(12X,F6.0)',IOSTAT=ERR) PRONOD
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(6F6.0)',IOSTAT=ERR)
     &            PCARLF, PCARST, PCARRT, PCARSH, PCARSD, PCARNO
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(5F6.0)',IOSTAT=ERR)
     &            PLIPLF, PLIPST, PLIPRT, PLIPSH, PLIPNO
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(6F6.0)',IOSTAT=ERR)
     &            PLIGLF, PLIGST, PLIGRT, PLIGSH, PLIGSD, PLIGNO
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(6F6.0)',IOSTAT=ERR)
     &          POALF, POAST, POART, POASH, POASD, POANO
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(6F6.0)',IOSTAT=ERR)
     &            PMINLF, PMINST, PMINRT, PMINSH, PMINSD, PMINNO
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDIF

        SECTION = '!*CARB'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND == 0) THEN
          CALL ERROR(SECTION, 42, FILECC, LNUM)
        ELSE
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(4X,A2)',IOSTAT=ERR) XPODF
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(4F6.0)',IOSTAT=ERR) ALPHL, ALPHS, ALPHR, ALPHSH
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDIF

        DO I = 1,2
          XPODF(I:I) = UPCASE(XPODF(I:I))
        ENDDO

!-----------------------------------------------------------------------
        SECTION = '!*VEGE'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND == 0) THEN
          CALL ERROR(SECTION, 42, FILECC, LNUM)
        ELSE
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(F6.0)',IOSTAT=ERR) WTFSD
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDIF
  
        CLOSE (LUNCRP)

!C-----------------------------------------------------------------------
!C    Read Ecotype Parameter File
!C-----------------------------------------------------------------------
!        CALL GETLUN('FILEE', LUNECO)
!        OPEN (LUNECO,FILE = FILEGC,STATUS = 'OLD',IOSTAT=ERR)
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,0)
!
!        ECOTYP = '      '
!        LNUM = 0
!        DO WHILE (ECOTYP .NE. ECONO)
!          CALL IGNORE(LUNECO, LNUM, ISECT, C255)
!        IF (ISECT == 1 .AND. C255(1:1) .NE. ' ' .AND.
!     &          C255(1:1) .NE. '*') THEN
!            READ (C255,'(A6,108X,2F6.0)',IOSTAT=ERR)
!     &        ECOTYP  !, SDPRO, SDLIP
!            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)
!            IF (ECOTYP == ECONO) THEN
!              EXIT
!            ENDIF
!
!          ELSE IF (ISECT == 0) THEN
!            IF (ECONO == 'DFAULT') CALL ERROR(ERRKEY,35,FILEGC,LNUM)
!            ECONO = 'DFAULT'
!            REWIND(LUNECO)
!          LNUM = 0
!          ENDIF
!        ENDDO
! 
!        CLOSE (LUNECO)

      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END  ! SUBROUTINE IPGROW
C=======================================================================

!-----------------------------------------------------------------------
!       Variable definitions: updated 27 Feb 04
!-----------------------------------------------------------------------
! AGEFAC   Relative effect of current leaf N on canopy photosynthesis (0-1)
!           (fraction)
! ALFDOT   Rate of leaf expansion (cm2[leaf] / m2[ground] / d)
! ALPHL    Fraction of new leaf growth that is mobile C (fraction)
! ALPHR    Fraction of new root growth that is mobile C (fraction)
! ALPHS    Fraction of new seed growth that is mobile C (fraction)
! ALPHSH   Fraction of new shell growth that is mobile C (fraction)
! AREAH    Area of healthy leaves (cm2[leaf] / m2[ground])
! AREALF   Area of leaves (one side) per unit ground area
!           (cm2[leaf] / m2[ground])
! BETN     Spacing between plants along a row (m / plant)
! C255     255-character record read from file 
! C80      80-character record read from file 
! CADLF    Mass of CH2O added to leaf reserves after growth
!           (g[CH2O] / m2 / d)
! CADST    Mass of CH2O added to stems (g[CH2O] / m2 / d)
! CANNAA   Weight of N in total plant at flowering (g[N] / m2)
! CANWAA   Canopy weight at flowering stage (g[plant] / m2)
! CLW      Cumulative leaf growth (g[leaf]/m2)
! CONTROL  Composite variable containing variables related to control 
!            and/or timing of simulation.    See Appendix A. 
! CPFLF    Respiration requirement for net leaf growth
!           (g[CH20] / g[tissue])
! CPFNOD   Respiration requirement for net nodule growth
!           (g[CH20] / g[tissue])
! CPFRT    Respiration requirement for net root growth
!           (g[CH20] / g[tissue])
! CPFSD1   Respiration requirement for net seed growth
!           (g[CH20] / g[tissue])
! CPFSH1   Respiration requirement for net leaf growth
!           (g[CH20] / g[tissue])
! CPFSTM   Respiration requirement for net stem growth
!           (g[CH20] / g[tissue])
! CROP     Crop identification code 
! CRUSLF   C mobilized from leaf tissue in a day (g[CH2O] / m2 / d)
! CRUSRT   C mobilized from root tissue in a day (g[CH2O] / m2 / d)
! CRUSSH   C mobilized from shell tissue in a day (g[CH2O] / m2 / d)
! CRUSST   C mobilized from stem tissue in a day (g[CH2O] / m2 / d)
! CSW      Cumulative stem growth (g[stem]/m2)
! DAP      Number of days after planting (d)
! DISLA    Diseased leaf area (cm2[leaf]/m2[ground]/d)
! DOY      Current day of simulation (d)
! DWNOD    Current nodule mass (g[nodule] / m2)
! DWNODA   Cumulative nodule growth (g[nodule] / m2)
! ECONO    Ecotype code - used to match ECOTYP in .ECO file 
! ECOTYP   Ecotype code for this simulation 
! ERR      Error code for file operation 
! F        Specific leaf area of new leaf tissue growth, including N
!           (cm2[leaf] / g[leaf])
! FILECC   Path plus filename for species file (*.spe) 
! FILEGC   Pathname plus filename for ECO file 
! FILEIO   Filename for input file (e.g., IBSNAT35.INP) 
! FOUND    Indicator that good data was read from file by subroutine FIND 
!            (0 - End-of-file encountered, 1 - NAME was found) 
! FRLF     Fraction of vegetative tissue growth that goes to leaves on a 
!            day (g[leaf] / g[veg])
! FRSTM    Fraction of vegetative tissue growth that goes to stems on a day
!           (g[stem] / g[veg])
! GROWTH   Total new growth of plant tissue on a day (g[tissue] / m2 / d)
! GRWRES   Growth respiration (g[CH2O]/m2-d)
! IDETO    Switch for printing OVERVIEW.OUT file 
! IHARI    Harvest type code: M=at harvest maturity, R=on specified day of 
!            year (HDATE), D= on specified day after planting (HDATE), G= 
!            at specified growth stage (HSTG), A= within specified window 
!            when SW conditions are met 
! ISECT    Indicator of completion of IGNORE routine: 0 - End of file 
!            encountered, 1 - Found a good line to read, 2 - End of Section 
!            in file encountered denoted by * in column 1. 
! ISWITCH  Composite variable containing switches which control flow of 
!            execution for model.  The structure of the variable 
!            (SwitchType) is defined in ModuleDefs.for. 
! ISWNIT   Nitrogen simulation switch (Y or N) 
! ISWSYM   Nitrogen fixation simulation switch (Y = simulate nodule growth, 
!            N = no nodule growth, U = N-fixation occurs at a rate that 
!            carbon will allow, and nodules are not grown explicitly) 
! LAIMX    Maximum leaf area index this season (m2[leaf] / m2[ground])
! LINC     Line number of input file 
! LNUM     Current line number of input file
! LTDOT    Net Lint growth rate (g/m2/d)
! LUNCRP   Logical unit number for FILEC (*.spe file) 
! LUNECO   Logical unit number for FILEE (*.eco file) 
! LUNIO    Logical unit number for FILEIO 
! MDATE    Harvest maturity date (YYYYDDD)
! MESSAGE  Text array containing information to be written to WARNING.OUT 
!            file. 
! NADLF    N added to leaf N reserves (g[N] / m2 / d)
! NADRT    N added to root N reserves (g[N] / m2 / d)
! NADST    N added to stem N reserves (g[N] / m2 / d)
! NDTH     Nodule death rate (g[nodule] / m2 / d)
! NFIXN    Amount of N fixed during the day (g[N] / m2 / d)
! NGRLF    Maximum N demand for leaf growth (g[leaf N] / m2[ground] / d)
! NGRRT    Maximum N demand for root growth (g[root N] / m2[ground] / d)
! NGRSD    Rate of N accumulation in new seeds (g[N] / m2 / d)
! NGRSH    Rate of N accumulation in new shells (g[N] / m2 / d)
! NGRST    Maximum N demand for stem growth (g[stem N] / m2[ground] / d)
! NLALL    N added to leaves today (g[N]/m2-d)
! NLAYR    Actual number of soil layers 
! NLDOT    Net N addition for leaves (g[leaf N] / m2[ground] / d)
! NLOFF    N loss from leaves in a day (g[N]/m2-d)
! NMINEA   Actual Nitrogen mined from existing tissue (g[N] / m2 / d)
! NNOFF    N loss from nodules in a day (g[N]/m2-d)
! NODGR    New nodule growth (g[nod] / m2 / d)
! NOUTDO   Logical unit for OVERVIEW.OUT file 
! NPLTD    Number of plants destroyed (#/m2/d)
! NRALL    N added to roots today (g[N]/m2-d)
! NRDOT    Net N addition for roots (g[N] / m2[ground] / d)
! NROFF    N loss from roots in a day (g[N]/m2-d)
! NRUSLF   N actually mobilized from leaves in a day (g[N]/m2-d)
! NRUSRT   N actually mobilized from roots in a day (g[N]/m2-d)
! NRUSSH   N actually mobilized from shells in a day (g[N]/m2-d)
! NRUSST   N actually mobilized from stems in a day (g[N]/m2-d)
! NSALL    N added to stems today (g[N]/m2-d)
! NSDALL   N added to seeds today (g[N]/m2-d)
! NSDDOT   Net N addition for seeds (g[N] / m2[ground] / d)
! NSDOFF   N loss from seeds in a day (g[N]/m2-d)
! NSDOT    Net N addition for stems (g[N] / m2[ground] / d)
! NSHALL   N added to shells today (g[N]/m2-d)
! NSHDOT   Net N addition for shells (g[N] / m2[ground] / d)
! NSHOFF   N loss from shells in a day (g[N]/m2-d)
! NSOFF    N loss from stems in a day (g[N]/m2-d)
! NTOVR    (Not used) 
! PCARLF   Proportion of leaf tissue that is carbohydrate (fraction)
! PCARNO   Proportion of nodule tissue that is carbohydrate (fraction)
! PCARRT   Proportion of root tissue that is carbohydrate (fraction)
! PCARSD   Proportion of seed tissue that is carbohydrate
!           (g[CH2O] / g[seed])
! PCARSH   Proportion of shell tissue that is carbohydrate (fraction)
! PCARST   Proportion of stem tissue that is carbohydrate (fraction)
! PCCSD    Percentage of carbohydrate in seed tissue (100 g[C] / g[seed])
! PCLSD    Percentage of lipid in seed tissue (100 g[lipid] / g[seed])
! PCNL     Percentage of N in leaf tissue (100 g[N] / g[leaf])
! PCNMIN   Minimum percentage of leaf N composition after N mining
!           (100 g[N] / g[leaf])
! PCNRT    Percent N in root tissue (100 g[N] / g[root])
! PCNSD    Percentage of N in seed tissue (100 g[N] / g[seed])
! PCNSH    Percentage of N in shell tissue (100 g[N] / g[shell])
! PCNST    Percent N in stem tissue (100 g[N] / g[stem])
! PLIGLF   Proportion of leaf tissue that is lignin (fraction)
! PLIGNO   Proportion of nodule tissue that is lignin (fraction)
! PLIGRT   Proportion of root tissue that is lignin (fraction)
! PLIGSD   Proportion of seed tissue that is lignin (fraction)
! PLIGSH   Proportion of shell tissue that is lignin (fraction)
! PLIGST   Proportion of stem tissue that is lignin (fraction)
! PLIPLF   Proportion of leaf tissue that is lipid (fraction)
! PLIPNO   Proportion of nodule tissue that is lipid (fraction)
! PLIPRT   Proportion of root tissue that is lipid (fraction)
! PLIPSH   Proportion of shell tissue that is lipid (fraction)
! PLIPST   Proportion of stem tissue that is lipid (fraction)
! PLME     Planting method; T = transplant, S = seed, P = pre-germinated 
!            seed, N = nursery 
! PLTPOP   Plant population (# plants / m2)
! PMINLF   Proportion of leaf tissue that is mineral (fraction)
! PMINNO   Proportion of nodule tissue that is mineral (fraction)
! PMINRT   Proportion of root tissue that is mineral (fraction)
! PMINSD   Proportion of seed tissue that is mineral (fraction)
! PMINSH   Proportion of shell tissue that is mineral (fraction)
! PMINST   Proportion of stem tissue that is mineral (fraction)
! POALF    Proportion of leaf tissue that is organic acid (fraction)
! POANO    Proportion of nodule tissue that is organic acid (fraction)
! POART    Proportion of root tissue that is organic acid (fraction)
! POASD    Proportion of seed tissue that is organic acid (fraction)
! POASH    Proportion of shell tissue that is organic acid (fraction)
! POAST    Proportion of stem tissue that is organic acid (fraction)
! PODWT    Dry mass of seeds plus shells, including C and N
!           (g[pods] / m2[ground])
! POTCAR   Potential carbohydrate composition of seed based on temperature
!           (fraction)
! POTLIP   Potential lipid composition of seed based on temperature
!           (fraction)
! PPLTD    Percent plants destroyed (%/m2/d)
! PROLFF   Minimum leaf protein composition after N mining
!           (g[protein] / g[leaf])
! PROLFI   Maximum protein composition in leaves during growth with 
!            luxurious supply of N (g[protein] / g[leaf tissue])
! PRONOD   Protein composition in nodules (g[protein] / g[nodule])
! PRORTF   Minimum root protein composition after N mining
!           (g[protein] / g[root])
! PRORTI   Maximum protein composition in roots during growth with 
!            luxurious supply of N (g[protein] / g[root])
! PROSHF   Minimum shell protein composition after N mining
!           (g[protein] / g[shell])
! PROSTF   Minimum stem protein composition after N mining
!           (g[protein] / g[stem])
! PROSTI   Maximum protein composition in stems during growth with 
!            luxurious supply of N (g[protein] / g[stem])
! PUNCSD   Cumulative puncture damage to seed (not yet implemented) 
! PUNCTR   Cumulative puncture damage (not yet implemented) 
! PUNDOT   Daily puncture damage (not yet implemented) 
! RHOL     Fraction of leaf which is carbohydrate (g [CH20] / g[leaf])
! RHOR     Fraction of root which is carbohydrate (g [CH2O] / g[root])
! RHOS     Fraction of stem which is carbohydrate (g [CH2O] / g[stem])
! RHOSH    Fraction of shell which is carbohydrate (g [CH2O] / g[shell])
! RMIN     Respiration required for synthesizing mineral structure
!           (g[CH2O] / g[mineral])
! RNITP    True nitrogen concentration in leaf tissue for photosynthesis 
!            reduction. (%)
! ROWSPC   Row spacing (m)
! RTWT     Dry mass of root tissue, including C and N
!           (g[root] / m2[ground])
! SDIDOT   Number of seeds destroyed on the current day (#/m2/d)
! SDLIP    Maximum lipid composition in seed (fraction)
! SDNPL    Seed N (g[N] / m2)
! SDPDOT   Daily seed puncture damage (not yet implemented) 
! SDPRO    Seed protein fraction at 25�C (g[protein] / g[seed])
! SDPROR   Ratio to adjust lipid and carbohydrate proportions when seed 
!            protein differs from protein composition of standard cultivar 
!            (SDPROS) 
! SDRATE   Seeding rate, mass of seed sown (g[seed] / m2[ground])
! SDWT     Dry mass of seed tissue, including C and N
!           (g[seed] / m2[ground])
! SDWTAM   Seed weight at maturity (g / m2)
! SDWTPL   Initial planting material dry weight (kg / ha)
! SECTION  Section name in input file 
! SEEDNI   Seed or transplant N at planting (g[N] / m2)
! SEEDNO   Total number of seeds (#/m2)
! SENESCE  Composite variable containing data about daily senesced plant 
!            matter. Structure of variable is defined in ModuleDefs.for 
! SENWT    Leaf senescence due to N mobilization (g[leaf] / m2[ground])
! SHELWT   Total mass of all shells (g / m2)
! SLA      Specific leaf area (cm2[leaf] / m2[ground])
! SLAAD    Specific leaf area, excluding weight of C stored in leaves
!           (cm2[leaf] / g[leaf])
! SLDOT    Defoliation due to daily leaf senescence (g/m2/day)
! SLNDOT   Leaf senescence due to water stress (g/m2/day)
! SOILPROP Composite variable containing soil properties including bulk 
!            density, drained upper limit, lower limit, pH, saturation 
!            water content.  Structure defined in ModuleDefs. 
! SRDOT    Daily root senescence (g / m2 / d)
! SSDOT    Daily senescence of petioles (g / m2 / d)
! SSNDOT   Petiole senescence due to water stress (g/m2/day)
! STMWT    Dry mass of stem tissue, including C and N (g[stem] / m2[ground)
! SWIDOT   Daily seed mass damage (g/m2/day)
! TGROW    Cumulative growth of plant tissue (g[tissue] / m2)
! TOPWT    Total weight of above-ground portion of crop, including pods
!           (g[tissue] / m2)
! TOTWT    Total weight of crop (g[tissue] / m2)
! TRNH4U   Total N uptake in ammonium form in a day (kg[N] / ha / d)
! TRNO3U   Total N uptake in nitrate form in a day (kg[N] / ha / d)
! TRNU     Total N uptake in a day (kg[N] / ha / d)
! TURFAC   Water stress factor for expansion (0 - 1) 
! WCRLF    Mass of CH2O reserves in leaves (g[leaf CH2O] / m2[ground])
! WCRRT    Mass of CH2O reserves in roots (g[root CH2O] / m2[ground])
! WCRSH    Mass of CH2O reserves in shells (g[shell CH2O] / m2[ground])
! WCRST    Mass of CH2O reserves in stems (g[stem CH2O] / m2[ground])
! WDOT     Net growth rate for the entire crop (g[tissue] / m2 / d)
! WLDOT    Net leaf growth rate (g[leaf] / m2 / d)
! WLDOTN   Dry weight growth rate of new leaf tissue including N but not C 
!            reserves (g[leaf] / m2[ground]-d)
! WLFDOT   Leaf weight losses due to freezing (g[leaf]/m2-d)
! WLFI     Initial weight of leaves (g[leaf] / m2)
! WLIDOT   Daily pest or freeze damage to leaf mass (g/m2/day)
! WNDOT    Net nodule growth rate (g[nodule] / m2 / d)
! WNRLF    N available for mobilization from leaves above lower limit of 
!            mining (g[N] / m2)
! WNRRT    N available for mobilization from roots above lower limit of 
!            mining (g[N] / m2)
! WNRSH    N available for mobilization from shells above lower limit of 
!            mining (g[N] / m2)
! WNRST    N available for mobilization from stems above lower limit of 
!            mining (g[N] / m2)
! WPDOT    Net pod growth rate (g[pod] / m2 / d)
! WRCLDT   Net C addition for leaves (g[CH2O] / m2 /d)
! WRCRDT   Net C addition for roots (g[CH2O] / m2 /d)
! WRCSDT   Net C addition for stems (g[CH2O] / m2 /d)
! WRCSHD   Net C addition for shells (g[CH2O] / m2 /d)
! WRDOT    Net root growth rate (g[root] / m2 / d)
! WRDOTN   Dry weight growth rate of new root tissue including N but not C 
!            reserves (g[root] / m2[ground]-d)
! WRIDOT   Daily pest damage to root mass (g/m2/day)
! WRTI     Initial weight of roots (g[root] / m2)
! WSDDOT   Net seed growth rate (g[seed] / m2 / d)
! WSDDTN   New seed growth today (g[seed] / m2 / d)
! WSDMAN   Mass of seed requiring maintenance (g[seed] / m2)
! WSDOT    Net stem growth rate (g[stem] / m2 / d)
! WSDOTN   Dry weight growth rate of new stem tissue including N but not C 
!            reserves (g[stem] / m2[ground]-d)
! WSHDOT   Net shell growth rate (g[shell] / m2 / d)
! WSHDTN   New shell growth today (g[shell] / m2 / d)
! WSHIDT   Weight of shell tissue consumed by pests today (g[shell]/m2-d)
! WSIDOT   Daily pest damage to stem mass (g/m2/day)
! WSTI     Initial weight of stems (g[stem] / m2)
! WTABRT   Weight of shells aborted on a day (g[shell] / m2 / d)
! WTCO     Cumulative losses of plant tissue (g[tissue] / m2)
! WTCSD    Cumulative carbohydrate added to seeds (g[C] / m2 / d)
! WTFSD    Relative weight of seed compared to maximum (fraction)
! WTLF     Dry mass of leaf tissue including C and N (g[leaf] / m2[ground])
! WTLO     Cumulative leaf losses (g[leaf] / m2)
! WTLSD    Cumulative lipids added to seeds (g[lipid] / m2 / d)
! WTMAIN   Mass of tissue assumed to require maintenance (g[tissue] / m2)
! WTNCAN   Mass of N in canopy (g[N] / m2[ground])
! WTNEW    Initial mass of seedling or seed (g / plant)
! WTNFX    Cumulative weight of N fixed (g[N] / m2)
! WTNLA    Cumulative N added to leaves (g[N]/m2-d)
! WTNLF    Mass of N in leaves (g[leaf N] / m2[ground])
! WTNLO    Cumulative N loss from leaves (g[N] / m2)
! WTNMOB   Cumulative mobilized N (g[N] / m2)
! WTNNA    Cumulative N added to nodules (g[N]/m2-d)
! WTNNAG   Total accumulated N used in nodule growth (g[N] / m2)
! WTNNO    Cumulative N loss from nodules (g[N] / m2)
! WTNNOD   Mass of N in nodules (g[N] / m2[ground])
! WTNOO    Cumulative nodule losses (g[nodule] / m2)
! WTNRA    Cumulative N added to roots (g[N]/m2-d)
! WTNRO    Cumulative N loss from roots (g[N] / m2)
! WTNRT    Mass of N in roots (g[root N] / m2[ground])
! WTNSA    Cumulative N added to stems (g[N]/m2-d)
! WTNSD    Mass of N in seeds (g[N] / m2[ground])
! WTNSDA   Cumulative N added to seeds (g[N]/m2-d)
! WTNSDO   Cumulative N loss from seeds (g[N] / m2)
! WTNSH    Mass of N in shells (g[N] / m2[ground])
! WTNSHA   Cumulative N added to shells (g[N]/m2-d)
! WTNSHO   Cumulative N loss from shells (g[N] / m2)
! WTNSO    Cumulative N loss from stems (g[N] / m2)
! WTNST    Mass of N in stems (g[stem N] / m2[ground])
! WTNTOT   Total plant N content (g[N] / m2[ground])
! WTNUP    Cumulative N uptake (g[N] / m2)
! WTPSD    Maximum weight per seed under non-limiting substrate (g / seed)
! WTRO     Cumulative root losses (g[root] / m2)
! WTSDO    Cumulative seed losses (g[seed] / m2)
! WTSHMT   Cohorts that reach THRESH today (g/m2)
! WTSHO    Cumulative shell losses (g[shell] / m2)
! WTSO     Cumulative stem losses (g[stem] / m2)
! XHLAI    Healthy leaf area index (m2[leaf] / m2[ground])
! XLAI     Leaf area (one side) per unit of ground area
!           (m2[leaf] / m2[ground])
! XPOD     Growth partitioning to pods which slows node appearance
!           (fraction)
! XPODF    Input parameter which determines the method of calculating XPOD 
! YR       Year portion of date 
! YRDOY    Current day of simulation (YYYYDDD)
! YRNR1    Day when 50% of plants have at least one flower (YYYYDDD)
! YRPLT    Planting date (YYYYDDD)
C=======================================================================
!       END SUBROUTINES GROW and IPGROW
C=======================================================================
