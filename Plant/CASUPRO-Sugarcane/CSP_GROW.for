C=======================================================================
C  CSP_GROW, Subroutine, O. H. Daza, based on
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
C  03-12-2003 CHP Changed senescence variable to composite (SENESCE)
C                   as defined in ModuleDefs.for
C  08/14/2003 FSR  Changed from GROW_SC for CASUPRO in DSSAT 4.0
C  07/26/2004 CHP Removed variables which were not being used
C-----------------------------------------------------------------------
C  Called by:  CASUPRO
C  Calls:      CSP_IPGROW, CSP_STRESS
C              ERROR
C=======================================================================
      SUBROUTINE CSP_GROW(CONTROL, DYNAMIC, ISWITCH, 
     &    AGEFAC, CADLF, CADST, CRUSLF, CRUSRT, CRUSST,   !Input
     &    DISLA, F, FILECC, FILEGC, FRLF, FRSTM, NADLF,   !Input
     &    NADRT, NADST, NGRLF, NGRRT, NGRST, NMINEA,      !Input
     &    NOUTDO, NPLTD, NRUSLF, NRUSRT, NRUSST, PPLTD,   !Input
     &    SENRT, SLDOT, SLNDOT, SOILPROP, SRDOT, SSDOT,   !Input
     &    SSNDOT, TRNH4U, TRNO3U, TRNU, TURFAC, WLDOTN,   !Input
     &    WLIDOT, WRDOTN, WRIDOT, WSDOTN, WSIDOT, YRNR1,  !Input
     &    MDATE, YRPLT,                                   !Input
     &    CADSU, CRUSSU, FRSU, NADSU, NGRSU, NRUSSU,  !CSR Input (FSR)
     &    WSUDOTN,                                    !CSR Input (FSR)

     &    WLFDOT, XHLAI,                                  !Input/Output

     &    AREALF, BETN, CANNAA, CANWAA, CLW, CSW, GROWTH, !Output
     &    GRWRES, LAIMX, PCNL, PCNRT, PCNST, PLTPOP,      !Output
     &    PLIGLF, PLIGRT, PLIGST, RHOL, RHOS, RNITP,      !Output
     &    ROWSPC, RTWT, SDNPL, SDRATE, SEEDNI, SENESCE,   !Output
     &    SLA, SLAAD, STMWT, TOPWT, TOTWT, WCRLF, WCRRT,  !Output
     &    WCRST, WNRLF, WNRRT, WNRST, WTCO, WTLF, WTLO,   !Output
     &    WTMAIN, WTNCAN, WTNEW, WTNLA, WTNLF, WTNLO,     !Output
     &    WTNRA, WTNRO, WTNRT, WTNSA, WTNSO, WTNST,       !Output
     &    WTNUP, WTRO, WTSO, XLAI,                        !Output
     &    CSUW, PCNSU, SUWT, WCRSU, WNRSU, WTNSU,     !CSP Output (FSR)
     &    WTNSUA)                                     !CSP Output (FSR)

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE
!-----------------------------------------------------------------------

      CHARACTER*1  ISWSYM, ISWNIT, IDETO, IHARI, PLME
      CHARACTER*2  CROP   !, XPODF
      CHARACTER*6  ERRKEY
      PARAMETER (ERRKEY = 'GROW  ')
      CHARACTER*30 FILEIO
      CHARACTER*92 FILECC, FILEGC

      INTEGER DYNAMIC, NOUTDO, L, NLAYR
      INTEGER YRDOY, YRNR1, MDATE
      INTEGER YRPLT

      REAL WTNUP,WTNMOB,WTNCAN,TGROW  !,WTNFX
      REAL DISLA   !WRCSHD,WSDMAN
      REAL WLFDOT, PPLTD,NPLTD,CANNAA,CANWAA  !,SDWTAM
      REAL AGEFAC  !DWNODA,WTNNAG,
!      REAL POTLIP,POTCAR
      REAL CPFLF, CPFSTM, CPFRT   !, CPFNOD, CPFSH1, CPFSD1
      REAL WRIDOT,WSIDOT  !,WTNNOD
      REAL WDOT,WRCLDT,WRCSDT,WRCRDT  !WNDOT,WPDOT,    

      REAL NLOFF,  NSOFF,  NROFF  !,  NSHOFF, NSDOFF
      REAL WTLO,   WTSO,   WTRO   !,  WTSHO,  WTSDO,  WTNOO
      REAL WTCO
      REAL NLALL,  NSALL,  NRALL  !,  NSHALL, NSDALL
      REAL WTNLA,  WTNSA,  WTNRA  !,  WTNSHA, WTNSDA, WTNNA
      REAL WTNLO,  WTNSO,  WTNRO  !,  WTNSHO, WTNSDO, WTNNO
      REAL WLDOT,  WSDOT,  WRDOT

      REAL WLDOTN, WSDOTN, WRDOTN !,  WSDDTN, WSHDTN
      REAL NGRLF,  NGRST,  NGRRT
      REAL WTLF,   STMWT,  RTWT   !,  SDWT
      REAL PROLFF, PROSTF, PRORTF !,  PROSHF
      REAL PCNL,   PCNST,  PCNRT
      REAL WTNLF,  WTNST,  WTNRT
      REAL NLDOT,  NSDOT,  NRDOT  !,  NSHDOT, NSDDOT
      REAL NRUSLF, NRUSST, NRUSRT
      REAL CRUSLF, CRUSST, CRUSRT
      REAL ALPHL,  ALPHS,  ALPHR
      REAL RHOL,   RHOS,   RHOR
      REAL WCRLF,  WCRST,  WCRRT
      REAL NADLF,  NADST,  NADRT
      REAL WNRLF,  WNRST,  WNRRT
      REAL CADLF,  CADST

      REAL SLDOT,  SSDOT,  SRDOT  !, SWIDOT
      REAL SLNDOT, SSNDOT         !, SDIDOT
      REAL WLIDOT, WSDDOT         !, WSHIDT, WSHDOT
      REAL GRWRES, TRNO3U, TRNH4U !SDPROR, 
!      REAL WTABRT
!      REAL WTLSD, WTCSD, NTOVR, XPOD
      REAL TOTWT, TOPWT           !, PODWT, DWNOD
      REAL WTMAIN
!      REAL WTSHMT
      REAL WTNTOT, NMINEA, TRNU
      REAL RNITP, PCNMIN          !PCLSD, PCCSD, 
      REAL ALFDOT,  F, SLA, AREALF, XLAI, SLAAD
      REAL LAIMX, AREAH
      REAL XHLAI, PLTPOP, ROWSPC, BETN    !, SEEDNO
      REAL TURFAC
      REAL GROWTH

      REAL FRLF, FRSTM
      REAL SDNPL, SEEDNI, SDRATE
      REAL WTNEW

      REAL PCARLF, PCARRT, PCARST,    !PCARNO, PCARSD, PCARSH, 
     &  PLIGLF, PLIGRT, PLIGST,   !PLIGNO, PLIGSD, PLIGSH, 
     &  PLIPLF, PLIPRT, PLIPST,   !PLIPNO, PLIPSH, 
     &  PMINLF, PMINRT, PMINST,   !PMINNO, PMINSD, PMINSH, 
     &  POALF,  POART,  POAST,    !POANO,  POASD,  POASH,  
     &  PROLFI, PRORTI, PROSTI,   !PRONOD, 
     &  SDPRO, WTFSD, WTPSD       !, SDWTPL
      REAL RMIN, WLFI, WSTI, WRTI !, SDLIP
      REAL CLW, CSW


!FSR -  LIST OF VARIABLES ADDED TO ENABLE SIMULATION OF SUGARS IN THE 
!       SUGARCANE MODEL
      REAL ALPHSU, CPFSU, CRUSSU, CSUW, FRSU
      REAL CADSU, NADSU, NGRSU, NRUSSU, NSUDOT
      REAL NSUALL, PCNSU, PCARSU, PLIGSU, PLIPSU, PMINSU  !NSUOFF, 
      REAL POASU, PROSUF, PROSUI, RHOSU, SUWT, WCRSU, WNRSU, WRCSUDT  
      REAL WSUDOT, WSUDOTN, WSUI, WTNSU, WTNSUA, WTNSUO, WTSUO

!     Surface and soil residue due to daily senescence of plant matter
      REAL SENRT(NL)
      REAL SenWt(0:NL)        !kg[dry matter]/ha
      REAL SenLig(0:NL)       !kg[lignin]/ha
      REAL SenE(0:NL,NELEM)   !kg[E]/ha (E=N, P, S,...)

!CHP - puncture variables, not functional
      REAL PUNDOT, SDPDOT !,PUNCSD,  PUNCTR

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
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
      CALL CSP_IPGROW(
     &    FILEIO, FILECC, FILEGC, CROP,                   !Input
     &    ALPHL,  ALPHR,  ALPHS,  ALPHSU, PCARLF, PCARST, !Output
     &    PCARRT, PCARSU, PLIGLF, PLIGST, PLIGRT, PLIGSU, !Output
     &    PLIPLF, PLIPST, PLIPRT, PLIPSU, PLME, PLTPOP,   !Output
     &    PMINLF, PMINST, PMINRT, PMINSU, POALF, POAST,   !Output
     &    POART, POASU,  PROLFF, PROSTF, PRORTF, PROSUF,  !Output
     &    PROLFI, PROSTI, PRORTI, PROSUI, ROWSPC, RMIN,   !Output
     &    SDPRO,  WTFSD,  WTPSD)                          !Output

                                          !CASUPRO Output follows (FSR)
!    &  ALPHSU, PCARSU, PLIGSU, PLIPSU, PMINSU, POASU)
      IF (CROP .NE. 'FA') THEN
!-----------------------------------------------------------------------
!       Copied from IPIBS
!-----------------------------------------------------------------------
        ROWSPC = ROWSPC / 100.
        IF (ROWSPC .GT. 0.0 .AND. PLTPOP .GT. 0.0) THEN
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

        CPFSU  = 30./44. * (PLIPSU * 1.720 + PLIGSU * 0.659 
     &         + POASU * (-0.011) + PMINSU * RMIN + PCARSU * 0.170)

! FSR- ABOVE added CPFSU for CASUPRO Sugar model.  
!      Removed CPFNOD & CPFSD1	 
!       CPFNOD =  30./44.*(PLIPNO*1.720 + PLIGNO*0.659 + POANO*(-0.011)
!    &         + PMINNO*RMIN + PCARNO*0.170)
!       CPFSH1 =  30./44.*(PLIPSH*1.720 + PLIGSH*0.659 + POASH*(-0.011)
!    &         + PMINSH*0.073 + PCARSH*0.170)
!       CPFSD1 = 30./44.*(PMINSD*0.073 + PLIGSD*0.659 + POASD*(-0.011)
!    &         + (SDLIP*1.720 + PCARSD*0.170)*(1. - SDPROR))
C-----------------------------------------------------------------------
!     CALCULATE PERCENT NITROGEN IN LEAVES AT END OF SEASON
        PCNMIN = PROLFF * 16.0            !Moved from INCOMP
!-----------------------------------------------------------------------
      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      ALFDOT = 0.0
      AREALF = 0.0
      AREAH  = 0.0
      CANWAA = 0.0
      CANNAA = 0.0
      GROWTH = 0.0
      GRWRES = 0.0
      LAIMX  = 0.0
      NLDOT  = 0.0
      NSDOT  = 0.0
      NRDOT  = 0.0
	NSUDOT = 0.0  !Sugar
      PCNL   = 0.0
      PCNRT  = 0.0
	PCNSU  = 0.0  !Sugars
      RHOL   = 0.0
      RHOR   = 0.0
      RHOS   = 0.0
	RHOSU  = 0.0  !Sugars
      RTWT   = 0.0
 
      SEEDNI = 0.0

      SenWt  = 0.0
      SenLig = 0.0
      SenE   = 0.0
      SENESCE % ResWt  = SenWt
      SENESCE % ResLig = SenLig
      SENESCE % ResE   = SenE

      SLAAD  = 0.0
      STMWT  = 0.0
      TGROW  = 0.0
      TOPWT  = 0.0
      TOTWT  = 0.0
      WCRLF  = 0.0
      WCRRT  = 0.0
      WCRST  = 0.0
      WCRSU  = 0.0  !Sugars
      WNRLF  = 0.0
      WNRRT  = 0.0
      WNRST  = 0.0
	WNRSU  = 0.0  !Sugars
      WSDDOT = 0.0
      WTCO   = 0.0
      WTLF   = 0.0
      WTLO   = 0.0
      WTMAIN = 0.0
      WTNCAN = 0.0
      WTNLA  = 0.0
      WTNLF  = 0.0
      WTNLO  = 0.0
      WTNMOB = 0.0
      WTNRA  = 0.0
      WTNRO  = 0.0
      WTNRT  = 0.0
	WTNSU  = 0.0  !Sugars
      WTNSA  = 0.0
	WTNSUA = 0.0  !Sugars
      WTNSO  = 0.0
	WTNSUO = 0.0  !Sugars
      WTNST  = 0.0
      WTNTOT = 0.0
      WTNUP  = 0.0
      WTRO   = 0.0
      WTSO   = 0.0
	WTSUO  = 0.0  !Sugars
      XLAI   = 0.0
      XHLAI  = 0.0

      CLW = 0.0   !Cumulative leaf growth
      CSW = 0.0   !Cumulative stem growth
      CSUW = 0.0  !Cumulative sugars accumulation

      SDPDOT = 0.0    !CHP - not used
      PUNDOT = 0.0    !CHP - not used
!-----------------------------------------------------------------------
! KJB 02/12/03 Set SLA to zero until emergence
! JWJ 09/16/03 Set SLA to -99 until emergence
! CHP 09/17/03 Do this for output only - SLA is used in calculations!
      SLA = 0.0
!     Need to get initial value of F from DEMAND first  chp 9/14/98
!      IF (CROP .NE. 'FA') THEN
!        SLA    = F                 
!      ENDIF

!***********************************************************************
!***********************************************************************
!     EMERGENCE CALCULATIONS - Performed once per season upon emergence
!         or transplanting of plants
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. EMERG) THEN
!-----------------------------------------------------------------------
!     Net growth rates
      WLDOT  = 0.0
      WSDOT  = 0.0
      WRDOT  = 0.0
      WSUDOT = 0.0  !Sugars

      WLFDOT = 0.0

C     Initial seedling or transplant weight
      SDRATE = WTPSD * PLTPOP * 10  ! kg / ha (CASUPRO sugarcane)

      IF (PLME .NE. 'T') THEN
        WTNEW  = WTFSD * WTPSD
        TOTWT  = WTNEW * PLTPOP
!     ELSE
!       TOTWT = SDWTPL / 10.
!       WTNEW = TOTWT / PLTPOP
      ENDIF

!     Initial mass of plant components
      WTLF  = FRLF * TOTWT
      STMWT = FRSTM * TOTWT
      SUWT  = FRSU  * TOTWT  !Sugars
      TOPWT = WTLF + STMWT + SUWT !Sugars
      RTWT  = TOTWT - TOPWT

      WLFI   = WTLF
      WSTI   = STMWT
      WRTI   = RTWT
	WSUI   = SUWT  !Sugars

!     Initialize cumulative variables
! CLW      Cumulative leaf growth (g [leaf] / m2)
      CLW   = WTLF  !Leaf
      CSW   = STMWT !Stalk
	CSUW  = SUWT  !Sugars 

!     CH2O reserves
      WCRLF = ALPHL * WTLF
      WCRST = ALPHS * STMWT
      WCRRT = ALPHR * RTWT
	WCRSU = ALPHSU * SUWT  !Sugars

!!     Initialize cumulative C variables
!      WCRLA = WCRLF
!      WCRSA = WCRST
!      WCRRA = WCRRT

!     Carbohydrate composition of plant components (fraction)
      RHOL   = ALPHL
      RHOR   = ALPHR
      RHOS   = ALPHS
	RHOSU = ALPHSU  !Sugars

!     Net carbon addition variables
      WRCLDT = 0.0
      WRCSDT = 0.0
      WRCRDT = 0.0
      WRCSUDT = 0.0  !Sugars

!     Compute N in plant components
      WTNLF  = WLFI * PROLFI * 0.16
      WTNST  = WSTI * PROSTI * 0.16
      WTNRT  = WRTI * PRORTI * 0.16
      WTNSU  = WSUI * PROSUI * 0.16  !Sugars

      WTNTOT = WTNLF + WTNST + WTNRT + WTNSU

!     Seed or transplant N at planting
      SDNPL  = WTPSD * SDPRO * 0.16 * PLTPOP - 
     &         (WTNLF + WTNST + WTNRT + WTNSU)
      SDNPL  = MAX(SDNPL,0.0)

      SEEDNI = WTNLF + WTNST + WTNRT + SDNPL + WTNSU !Sugars

!     Initialize cumulative N variables
      WTNLA  = WTNLF
      WTNSA  = WTNST
      WTNRA  = WTNRT
      WTNSUA = WTNSU  !Sugars

!     Percent N in plant components
      PCNL   = WTNLF / WLFI * 100.
      RNITP  = PCNL
      PCNST  = WTNST / WSTI * 100.
      PCNRT  = WTNRT / WRTI * 100.
      PCNSU  = WTNSU / WSUI * 100.  !Sugars

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
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
C-----------------------------------------------------------------------
!     GROWTH = WLDOTN + WSDOTN + WRDOTN + WSHDTN + WSDDTN + NODGR
! 
!     GRWRES = WLDOTN*CPFLF + WSDOTN*CPFSTM + WRDOTN*CPFRT +
!    &         NODGR*CPFNOD + WSHDTN*CPFSH1 + WSDDTN*CPFSD1 +
!    &         30./44*(TRNO3U/0.16 * 1.798 + TRNH4U/0.16 * 0.462 +
!    &         NFIXN/0.16 * 1.798)
!
! FSR-Substituted OHD code (COSUPRO sugarcane model)
      GROWTH = WLDOTN + WSDOTN + WRDOTN + WSUDOTN
  
      GRWRES = WLDOTN * CPFLF + WSDOTN * CPFSTM + WRDOTN * CPFRT +
     &         WSUDOTN * CPFSU +
     &         30./44 * (TRNO3U / 0.16 * 1.798 + TRNH4U / 0.16 * 0.462)

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

      IF (WTLF .GT. 0.0) THEN
         WLDOT = WLDOT + (CADLF+NADLF/0.16) *
     &   (1. - MIN(1.0,(SLDOT+WLIDOT+WLFDOT)/WTLF))
      ENDIF
      IF (WLDOT .LT. 0.0) THEN
        WLDOT = MAX(WLDOT, -WTLF)
      ENDIF
      
C-----------------------------------------------------------------------
C       WSDOT = Net stalk growth rate
C-----------------------------------------------------------------------
      WSDOT = WSDOTN - SSDOT - NRUSST / 0.16 - CRUSST - WSIDOT
      IF (STMWT .GT. 0.0) THEN
         WSDOT = WSDOT + (CADST + NADST / 0.16) *
     &   (1. - MIN(1.0,(SSDOT + WSIDOT) / STMWT))
      ENDIF
      IF (WSDOT .LT. 0.0) THEN
        WSDOT = MAX(WSDOT, -STMWT)
      ENDIF
C-----------------------------------------------------------------------
C     Net root growth rate
C-----------------------------------------------------------------------
      WRDOT = WRDOTN - SRDOT - NRUSRT/0.16 - CRUSRT - WRIDOT
      IF (RTWT .GT. 0.0) THEN
         WRDOT = WRDOT + (NADRT / 0.16) *
     &   (1. - MIN(1.0,(SRDOT + WRIDOT) / RTWT))
      ENDIF
      IF (WRDOT .LT. 0.0) THEN
        WRDOT = MAX(WRDOT, -RTWT)
      ENDIF
C-----------------------------------------------------------------------
C     Net sugars growth rate  (Added for CASUPRO FSR)
C-----------------------------------------------------------------------
      WSUDOT = WSUDOTN - NRUSSU / 0.16 - CRUSSU
      IF (SUWT .GT. 0.0) THEN
         WSUDOT = WSUDOT + (CADSU + NADSU / 0.16)
      ENDIF

C-----------------------------------------------------------------------
C     Net shell growth rate (Removed fof CASUPRO FSR)
C-----------------------------------------------------------------------
C     Net seed growth rate (Removed for CASUPRO FSR)
C-----------------------------------------------------------------------
C     Net nodule growth rate (Removed for CASUPRO FSR)
C-----------------------------------------------------------------------
C     Net pod growth rate (Removed for CASUPRO FSR)
C-----------------------------------------------------------------------
!
C     Total Net plant growth rate
C-----------------------------------------------------------------------
      WDOT = WLDOT + WSDOT + WRDOT + WSUDOT  

C-----------------------------------------------------------------------
C    Integration, Add Today's Net Growth to Existing Weights
C-----------------------------------------------------------------------
      TOTWT  = TOTWT  + WDOT
      TOPWT  = TOPWT  + WSDOT + WLDOT + WSUDOT
      WTLF   = WTLF   + WLDOT
      STMWT  = STMWT  + WSDOT
      RTWT   = RTWT   + WRDOT
      SUWT   = SUWT   + WSUDOT  !Sugars FSR
      TGROW  = TGROW  + GROWTH

C-----------------------------------------------------------------------
C     Cumulative leaf, stalk and sugars growth
C-----------------------------------------------------------------------
      CLW  = CLW  + WLDOTN      
      CSW  = CSW  + WSDOTN        
      CSUW = CSUW + WSUDOTN  !Sugars   

!OHD - Include roots later 

C-----------------------------------------------------------------------
C     Compute Seed Weight for Which there is Maintenance Costs.
C-----------------------------------------------------------------------

      WTMAIN = TOTWT 

C-----------------------------------------------------------------------
C     Carbon Reserves:  Net Growth Rates for Mobile Carbohydrates
C-----------------------------------------------------------------------
C     Account for N added to existing plant tissue, e.g., NADLF, that
C     is damaged by insects, freezing, or senesced.  Otherwise, could
C     get increase in tissue N composition when tissue is aborted.  Need
C     to account for mass, N and C lost this way in sections below
C-----------------------------------------------------------------------
!Leaves
      WRCLDT = ALPHL * WLDOTN - CRUSLF - RHOL * (SLNDOT+WLIDOT+WLFDOT) 
      IF (WTLF .GT. 0.0) THEN
         WRCLDT = WRCLDT + CADLF *
     &     (1. - MIN(1.0,(SLDOT + WLIDOT + WLFDOT) / WTLF))
      ENDIF

!Stalks
      WRCSDT = ALPHS * WSDOT - CRUSST - RHOS * SSNDOT  
      IF (STMWT .GT. 0.0) THEN
         WRCSDT = WRCSDT + CADST *
     &     (1. - MIN(1.0,(SSDOT + WSIDOT) / STMWT))
      ENDIF

!Roots
      WRCRDT = ALPHR * WRDOT - CRUSRT - RHOR * SRDOT   

!Sugars
      WRCSUDT = ALPHSU * WSUDOT - CRUSSU  !Sugars (FSR)

C-----------------------------------------------------------------------
C     Update C Storage, Concentrations in Leaves, Stalks, Roots and Sugars
C-----------------------------------------------------------------------
      WCRLF = WCRLF + WRCLDT
      WCRST = WCRST + WRCSDT
      WCRRT = WCRRT + WRCRDT
      WCRSU = WCRSU + WRCSUDT  !Sugars

      IF (WCRLF .LE. 1.0E-30) WCRLF = 0.0
      IF (WCRST .LE. 1.0E-30) WCRST = 0.0
      IF (WCRRT .LE. 1.0E-30) WCRRT = 0.0
	IF (WCRSU .LE. 1.0E-30) WCRSU = 0.0  !Sugars (FSR)


C-----------------------------------------------------------------------
C     Compute Cumulative C loss During Season
C-----------------------------------------------------------------------
      WTLO  = WTLO  + SLDOT  + WLIDOT + WLFDOT
      WTSO  = WTSO  + SSDOT  + WSIDOT
      WTRO  = WTRO  + SRDOT  + WRIDOT
!OHD - Does not include sugars

      WTCO  = WTLO  + WTSO  

C-----------------------------------------------------------------------
C     Compute CH20 fractions
C-----------------------------------------------------------------------
      IF (WTLF .GT. 0.0001) THEN
         RHOL =  WCRLF/WTLF
      ELSE
         RHOL = 0.0
      ENDIF

      IF (STMWT .GT. 0.0001) THEN
         RHOS =  WCRST/STMWT
      ELSE
         RHOS = 0.0
      ENDIF

      IF (RTWT .GT. 0.0001) THEN
         RHOR =  WCRRT/RTWT
      ELSE
         RHOR = 0.0
      ENDIF

      IF (SUWT .GT. 0.0001) THEN  !Sugars (FSR)
         RHOSU = WCRSU / SUWT
      ELSE
         RHOSU = 0.0
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
      IF (NLOFF. LT. 0.0) THEN
        NLOFF = 0.0
      ENDIF

C-----------------------------------------------------------------------
C     Net growth rate of nitrogen in the leaves
C-----------------------------------------------------------------------
      NLDOT = NGRLF - NLOFF - NRUSLF
      IF (WTLF .GT. 0.0) THEN
         NLDOT = NLDOT + NADLF *
     &                  (1. - MIN(1.0,(SLDOT + WLIDOT + WLFDOT) / WTLF))
      ENDIF

C-----------------------------------------------------------------------
C     Stalk nitrogen senescence and pest damage loss
C-----------------------------------------------------------------------
!              senesce + pest
      NSOFF  = (SSNDOT + WSIDOT) * (PCNST/100.) + (SSDOT - SSNDOT)
     &         * PROSTF * 0.16
      IF (NSOFF .LT. 0.0) THEN
        NSOFF = 0.0
      ENDIF

C-----------------------------------------------------------------------
C     Net growth rate of nitrogen in the stems
C-----------------------------------------------------------------------
      NSDOT = NGRST - NSOFF - NRUSST
      IF (STMWT .GT. 0.0) THEN
         NSDOT = NSDOT + NADST * 
     &                  (1. - MIN(1.0,(SSDOT + WSIDOT) / STMWT))
      ENDIF

C-----------------------------------------------------------------------
C     Root nitrogen senescence and pest damage loss
C-----------------------------------------------------------------------
      NROFF  = (SRDOT+WRIDOT) * (PCNRT/100.)
      IF (NROFF .LT. 0.0) THEN
         NROFF = 0.0
      ENDIF

C-----------------------------------------------------------------------
C     Net growth rate of nitrogen in the roots
C-----------------------------------------------------------------------
      NRDOT = NGRRT - NROFF - NRUSRT
      IF (RTWT .GT. 0.0) THEN
         NRDOT = NRDOT + NADRT * 
     &                  (1. - MIN(1.0,(SRDOT + WRIDOT) / RTWT))
      ENDIF
C-----------------------------------------------------------------------
C     Net growth rate of nitrogen in sugars (NEW, CHECK)
C-----------------------------------------------------------------------
! NGRSU    Maximum N demand for sugars growth (g [sugars N] / m2 [ground] - d)
! NRUSSU   N actually mobilized from sugars in a day (g [N] / m2 - d)
! NSUDOT   Net N addition for sugars (g [N] / m2 [ground] - d)

      NSUDOT = NGRSU - NRUSSU   ! Added for CASUPRO (FSR)

C-----------------------------------------------------------------------
C     Shell nitrogen senescence, abortion and pest damage loss
!                   Removed for CASUPRO (FSR)
C-----------------------------------------------------------------------
C     Net growth rate of nitrogen in shells
!                   Removed for CASUPRO (FSR)
C-----------------------------------------------------------------------
C     Seed nitrogen senescence, abortion and pest damage loss
!                   Removed for CASUPRO (FSR)
C-----------------------------------------------------------------------
C     Net growth rate of nitrogen in seeds  
!                   Removed for CASUPRO (FSR)
!
C-----------------------------------------------------------------------
C   Integration, Update N in Each Component by Adding Today's Growth
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C     Total nitrogen in the leaves
C-----------------------------------------------------------------------
      IF ((NLDOT .LT. 0.0) .AND. (ABS(NLDOT) .GT. WTNLF)) THEN
         NLDOT = - WTNLF
      ENDIF
      WTNLF = WTNLF + NLDOT

C-----------------------------------------------------------------------
C     Total nitrogen in the stalks
C-----------------------------------------------------------------------     
      IF ((NSDOT .LT. 0.0) .AND. (ABS(NSDOT) .GT. WTNST)) THEN
         NSDOT = - WTNST
      ENDIF
      WTNST = WTNST + NSDOT

C-----------------------------------------------------------------------
C     Total nitrogen in the roots
C-----------------------------------------------------------------------
      IF ((NRDOT .LT. 0.0) .AND. (ABS(NRDOT) .GT. WTNRT)) THEN
         NRDOT = - WTNRT
      ENDIF
      WTNRT = WTNRT + NRDOT

C-----------------------------------------------------------------------
C     Total nitrogen in sugars  Added for CASUPRO (FSR)
C-----------------------------------------------------------------------
      IF ((NSUDOT .LT. 0.0) .AND. (ABS(NSUDOT) .GT. WTNSU)) THEN
	   NSUDOT = - WTNSU
	ENDIF
      WTNSU = WTNSU + NSUDOT

C-----------------------------------------------------------------------
C     Total nitrogen in the nodules 
!                   Removed for CASUPRO (FSR)
C-----------------------------------------------------------------------
C     Total nitrogen in the shells  
!                   Removed for CASUPRO (FSR)
C-----------------------------------------------------------------------
C     Total nitrogen in the seed  
!                   Removed for CASUPRO (FSR)
C-----------------------------------------------------------------------
C     Total nitrogen in the plant  
C-----------------------------------------------------------------------
      WTNTOT = WTNLF + WTNST + WTNRT + WTNSU 

C-----------------------------------------------------------------------
C     Total nitrogen in the canopy (all above ground components)
C-----------------------------------------------------------------------
      WTNCAN = WTNLF + WTNST + WTNSU

C-----------------------------------------------------------------------
C     Save total nitrogen in the canopy at the start of flowering
C-----------------------------------------------------------------------
      IF (YRDOY .EQ. YRNR1) THEN
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
      IF (WTLF .GT. 0.0) THEN
        NLALL = NLALL + NADLF * 
     &                (1. - MIN(1.0,(SLDOT + WLIDOT + WLFDOT) / WTLF))
      ENDIF

!     Stem
      NSALL = NGRST - NRUSST 
      IF (STMWT .GT. 0.0) THEN
        NSALL = NSALL + NADST * 
     &                  (1. - MIN(1.0,(SSDOT + WSIDOT) / STMWT))
      ENDIF

!     Root
      NRALL = NGRRT - NRUSRT
      IF (RTWT .GT. 0.0) THEN
        NRALL = NRALL + NADRT * 
     &                  (1. - MIN(1.0,(SRDOT + WRIDOT) / RTWT))
      ENDIF

!     Add a Sugar section similar to above?
      NSUALL = NGRSU - NRUSSU + NADSU  !Sugars

      WTNLA = WTNLA + NLALL
      WTNSA = WTNSA + NSALL
      WTNRA = WTNRA + NRALL
      WTNSUA = WTNSUA + NSUALL  !Sugars

C-----------------------------------------------------------------------
C    Nodules       
!                   Removed for CASUPRO (FSR)
C-----------------------------------------------------------------------
C     Compute Cumulative N loss During Season
C-----------------------------------------------------------------------
      WTNLO  = WTNLO  + NLOFF
      WTNSO  = WTNSO  + NSOFF
      WTNRO  = WTNRO  + NROFF

!OHD - Does not include sugars
!     WTNSUO = WTNSUO + NSUOFF  !Sugars

C-----------------------------------------------------------------------
C     N Balance Components for Mobilized, Uptake, and Fixed N
C-----------------------------------------------------------------------
      WTNMOB = WTNMOB + NMINEA
      WTNUP = WTNUP + TRNU
C-----------------------------------------------------------------------
C     Compute Percentage N in each Plant Component
C-----------------------------------------------------------------------
      IF ((WTLF .GT. 0.0) .AND. (WTLF .GT. WTNLF) .AND. 
     &   (WTNLF .GT. 0.0)) THEN
         PCNL = WTNLF / WTLF * 100.0
      ELSE
         PCNL = 0.0
      ENDIF

      IF ((STMWT .GT. 0.0) .AND. (WTNST .GT. 0.0)) THEN
         PCNST = WTNST / STMWT * 100.0
      ELSE
         PCNST = 0.0
      ENDIF

      IF ((RTWT .GT. 0.0) .AND. (WTNRT .GT. 0.0)) THEN
         PCNRT = WTNRT / RTWT * 100.0
      ELSE
         PCNRT = 0.0
      ENDIF

!Sugars
      IF ((SUWT .GT. 0.0) .AND. (WTNSU .GT. 0.0)) THEN
         PCNSU = WTNSU / SUWT * 100.0
      ELSE
         PCNSU = 0.0
      ENDIF

C-----------------------------------------------------------------------
C     Compute Percentage Seed Composition    
!                   Removed for CASUPRO (FSR)
C-----------------------------------------------------------------------
C    Calculate true nitrogen concentration in leaf tissue for
C     photosynthesis reduction.
C-----------------------------------------------------------------------
      IF ((WTLF - WCRLF) .GT. 0.0) THEN
         RNITP = 100. * WTNLF / (WTLF - WCRLF)
      ELSE
         RNITP = PCNMIN
      ENDIF
C-----------------------------------------------------------------------
C     Calculate Remaining N in Shells, Leaves, Stalks and Roots
C     That can be Mined (Plant N-Balance).
C-----------------------------------------------------------------------
      IF ((WTLF - WCRLF) .GT. 0.0) THEN
        WNRLF = MAX (WTNLF - PROLFF * 0.16 * (WTLF-WCRLF), 0.0)
      ELSE
        WNRLF = 0.0
      ENDIF

      IF ((STMWT - WCRST) .GT. 0.0) THEN
        WNRST = MAX (WTNST - PROSTF * 0.16 * (STMWT-WCRST), 0.0)
      ELSE
        WNRST = 0.0
      ENDIF

      IF (RTWT .GT. 0.) THEN
        WNRRT = MAX (WTNRT - PRORTF * 0.16 * RTWT, 0.0)
      ELSE
        WNRRT = 0.0
      ENDIF
!Sugars
      IF (SUWT .GT. 0.) THEN
        WNRSU = MAX (WTNSU - PROSUF * 0.16 * SUWT, 0.0)
      ELSE
        WNRSU = 0.0
      ENDIF

C-----------------------------------------------------------------------
C     This section added to provide senescence parameters to the 
C     Soil N routines (senescence and freeze values are added to soil
C     residue; pest damage components are lost from the system)
C-----------------------------------------------------------------------
C     Surface carbon includes all senescence and freeze variables for
C      leaf (SLDOT, WLFDOT), and stalk (SSDOT)
C         
C     Convert from biomass to C with a factor 0.40.   
C-----------------------------------------------------------------------
      SenWt(0) = (SLDOT + WLFDOT + SSDOT)
C     Convert from g/m2 to kg/ha with a factor of 10.
      SenWt(0) = AMAX1(SenWt(0), 0.) * 10.0    !kg[dry matter]/ha
      
C-----------------------------------------------------------------------
C     Surface nitrogen includes nitrogen losses computed above minus the
C       pest damage components. 
C-----------------------------------------------------------------------
      SenE(0,1) = (NLOFF - WLIDOT * PCNL  / 100.) +       !Leaf
     &            (NSOFF - WSIDOT * PCNST / 100.)         !Stem

C     Convert from g/m2 to kg/ha with a factor of 10.
      SenE(0,1) = AMAX1 (SenE(0,1),0.) * 10.0             !kg[N]/ha

C-----------------------------------------------------------------------
C     Contribution of lignin to surface litter from senesced and frozen 
C       plant matter
C-----------------------------------------------------------------------
      SenLig(0) = (SLDOT + WLFDOT) * PLIGLF + SSDOT * PLIGST  
!    &    WTABRT * PLIGSH
C     Convert from g/m2 to kg/ha with a factor of 10.
      SenLig(0) = AMAX1 (SenLig(0),0.) * 10.0             !kg[lig]/ha

C-----------------------------------------------------------------------
C     Senescence of roots and nodules (kg/ha)
C-----------------------------------------------------------------------
      DO L = 1, NLAYR 
        SenWt(L)  = (SENRT(L))                        !kg[dry matter]/ha
        SenLig(L) = SENRT(L) * PLIGRT                        !kg[lig]/ha
        SenE(L,1) = (SENRT(L)* PRORTF) * 0.16 
      ENDDO
C-----------------------------------------------------------------------
C     Leaf Area, Specific Leaf Area Calculations
C     Calculate Area Growth rate and Update Leaf Area (AREALF, XLAI)
C-----------------------------------------------------------------------
      ALFDOT = WLDOTN*F - (SLDOT+WLIDOT+WLFDOT+NRUSLF/0.16)*SLA
      IF (WTLF .GT. 0.0) THEN
         ALFDOT = ALFDOT + SLA * (NADLF/0.16) *
     &   (1. - MIN(1.0,(SLDOT+WLIDOT+WLFDOT)/WTLF))
      ENDIF

      AREALF = AREALF + ALFDOT
      XLAI   = AREALF / 10000.
      IF (AREALF .GT. 0.00001 .AND. WTLF .GT. 0.0) THEN
         SLA    = AREALF / WTLF
         SLAAD  = AREALF / (WTLF - WCRLF)
         IF (SLA .GT. 999.) SLA = 0.0
         IF (SLAAD .GT. 999.) SLAAD = -99.
      ENDIF
C-----------------------------------------------------------------------
C     Remember XLAI
C-----------------------------------------------------------------------
      LAIMX = MAX(XLAI,LAIMX)
C-----------------------------------------------------------------------
C     Calculate "Healthy" or Non-Diseased Leaf Area Index
C-----------------------------------------------------------------------
      AREAH  = AREALF - 2. * DISLA
      AREAH  = MAX(0.,AREAH)
      XHLAI  = AREAH / 10000.
C-----------------------------------------------------------------------
C     Integrate Pest Damage to Seeds        Removed for CASUPRO (FSR)
C-----------------------------------------------------------------------
C     Loss in Plants due to Pests, Adjust Plant Spacing Also
C-----------------------------------------------------------------------

      IF (NPLTD .GT. 0.0) THEN
         PLTPOP = PLTPOP - NPLTD
         PLTPOP = MAX(0.,PLTPOP)
         IF (PLTPOP*ROWSPC .GT. 0.0) THEN
            BETN = 1.0/(ROWSPC*PLTPOP)
         ENDIF
      ENDIF

      IF (PPLTD .GT. 0.0) THEN
         PLTPOP = PLTPOP - PPLTD*PLTPOP/100.
         PLTPOP = MAX(0.,PLTPOP)
         IF(PLTPOP*ROWSPC .GT. 0.0) THEN
            BETN = 1.0/(ROWSPC*PLTPOP)
         ENDIF
      ENDIF

C-----------------------------------------------------------------------
C     Terminate growth if stress causes extremely low plant weights
C-----------------------------------------------------------------------
      IF (TOPWT .LT. 0.00001 .OR. STMWT .LT. 0.00001) THEN
        CALL CSP_STRESS(
     &    AGEFAC, IDETO, IHARI, NOUTDO,                   !Input
     &    RTWT, SUWT, STMWT, TOPWT,                       !Input
     &    TOTWT, TURFAC, WTLF, YRDOY, YRPLT,              !Input
     &    MDATE)                                          !Output
        RETURN
      ENDIF

      IF (IHARI .NE. 'R' .AND. IHARI .NE. 'D') THEN
        IF (RTWT  .LT. 0.00001 .OR. WTLF  .LT. 0.00001) THEN
          CALL CSP_STRESS( 
     &      AGEFAC, IDETO, IHARI, NOUTDO,                 !Input
     &      RTWT, SUWT, STMWT, TOPWT,                     !Input
     &      TOTWT, TURFAC, WTLF, YRDOY, YRPLT,            !Input
     &      MDATE)                                        !Output
          RETURN
        ENDIF
      ENDIF

      SENESCE % ResWt  = SenWt
      SENESCE % ResLig = SenLig
      SENESCE % ResE   = SenE

C***********************************************************************
C***********************************************************************
C     END OF DYNAMIC IF CONSTRUCT
C***********************************************************************
      ENDIF
C-----------------------------------------------------------------------
      RETURN
      END  ! SUBROUTINE CSP_GROW
C=======================================================================

C=======================================================================
C  CSP_STRESS, Subroutine, modified from STRESS, for CASUPRO sugarcane 
C-----------------------------------------------------------------------
C  Plant death due to stress
C-----------------------------------------------------------------------
C  REVISION        HISTORY
C  09/18/1998 CHP  Written based on code in GROW subroutine
C  08/14/2003 FSR  Changed from STRESS for O.H. Daza's CASUPRO sugarcane 
!-----------------------------------------------------------------------
!  Called by:  CSP_GROW
!  Calls:      None
C=======================================================================
      SUBROUTINE CSP_STRESS(
     &  AGEFAC, IDETO, IHARI, NOUTDO,                     !Input
     &  RTWT, SUWT, STMWT, TOPWT,                         !Input
     &  TOTWT, TURFAC, WTLF, YRDOY, YRPLT,                !Input
     &  MDATE)                                            !Output
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
      CHARACTER*1  IDETO, IHARI
      CHARACTER*78 MESSAGE(10)
      INTEGER NOUTDO, YRDOY, YRPLT, MDATE, DAP, TIMDIF
      INTEGER YR, DOY
      REAL AGEFAC, RTWT,
     &  STMWT, SUWT, TOPWT, TOTWT, TURFAC, WTLF

!-----------------------------------------------------------------------
!     Set Ending Plant Weights, Dates of Stages if Plants Died
!-----------------------------------------------------------------------
      TOTWT  = MAX(0.,TOTWT)
      TOPWT  = MAX(0.,TOPWT)
      WTLF   = MAX(0.,WTLF)
      STMWT  = MAX(0.,STMWT)
      RTWT   = MAX(0.,RTWT)
      SUWT   = MAX(0.,SUWT)  !Sugar

      IF (MDATE .LT. 0) THEN
        MDATE = YRDOY
      ENDIF
C-----------------------------------------------------------------------
      IF (IHARI .EQ. 'M') THEN
        DAP   = MAX(0,TIMDIF(YRPLT,YRDOY))

        !Message to WARNING.OUT
        CALL YR_DOY(YRDOY, YR, DOY)
        WRITE(MESSAGE(1), 100) DAP
        WRITE(MESSAGE(2), 110) YR, DOY, TURFAC, AGEFAC
        CALL WARNING(2,'CRPGRO', MESSAGE)
  100 FORMAT(' Plant died due to extreme stress at ',I3,
     &         ' days after planting.')
  110 FORMAT(' (DAY : ',I4,1X,I3,'; TURFAC : ',F5.2,'; AGEFAC : ',
     &            F5.2,'  )')

!       Message to screen
!       WRITE (*,275) MESSAGE(1), MESSAGE(2)
!       Message to Overview.out
        IF (IDETO .EQ. 'Y') THEN
          WRITE (NOUTDO,275) MESSAGE(1), MESSAGE(2)
        ENDIF
  275   FORMAT(/,10X,A78,/,10X,A78,/)
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END  ! SUBROUTINE CSP_STRESS
C=======================================================================

C=======================================================================
C  CSP_IPGROW based on IPGROW Subroutine by C. H. Porter (FSR)
C-----------------------------------------------------------------------
C  Reads input data for CSP_GROW routines.
C----------------------------------------------------------------------
C  REVISION HISTORY
C  09/15/1998 CHP Written
C  08/12/2003 CHP Added I/O error checking
C  08/14/2003 FSR modified for CASUPRO sugarcane 
C-----------------------------------------------------------------------
C  Called : CSP_GROW
C  Calls  : FIND, ERROR, IGNORE
C=======================================================================
      SUBROUTINE CSP_IPGROW(
     &    FILEIO, FILECC, FILEGC, CROP,                   !Input
     &    ALPHL,  ALPHR,  ALPHS,  ALPHSU, PCARLF, PCARST, !Output
     &    PCARRT, PCARSU, PLIGLF, PLIGST, PLIGRT, PLIGSU, !Output
     &    PLIPLF, PLIPST, PLIPRT, PLIPSU, PLME, PLTPOP,   !Output
     &    PMINLF, PMINST, PMINRT, PMINSU, POALF, POAST,   !Output
     &    POART, POASU,  PROLFF, PROSTF, PRORTF, PROSUF,  !Output
     &    PROLFI, PROSTI, PRORTI, PROSUI, ROWSPC, RMIN,   !Output
     &    SDPRO,  WTFSD,  WTPSD)                          !Output

!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
      CHARACTER*1 PLME    !, UPCASE
      CHARACTER*2 CROP
      CHARACTER*6 ERRKEY
      PARAMETER (ERRKEY = 'IPGROW')
      CHARACTER*6 SECTION, ECONO, ECOTYP
      CHARACTER*30 FILEIO
      CHARACTER*80 C80
      CHARACTER*92 FILECC, FILEGC
      CHARACTER*255 C255

      INTEGER LUNCRP, LUNECO, LUNIO   !, I
      INTEGER ERR, LINC, LNUM, FOUND, ISECT

      REAL ROWSPC, RMIN, PLTPOP, WTFSD, WTPSD,
     &  SDPRO, !,  SDWTPL,
     &  ALPHL,  ALPHS,  ALPHR,
     &  PROLFF, PROSTF, PRORTF,
     &  PROLFI, PROSTI, PRORTI,
     &  PCARLF, PCARST, PCARRT,
     &  PLIPLF, PLIPST, PLIPRT,
     &  PLIGLF, PLIGST, PLIGRT,
     &  POALF,  POAST,  POART,
     &  PMINLF, PMINST, PMINRT

!-----------------------------------------------------------------------
! LIST OF NEW VARIABLES ADDED TO ENABLE SIMULATION OF SUGARS IN THE 
! SUGARCANE MODEL  Added for CASUPRO (FSR)
      
	REAL ALPHSU, PCARSU, PLIGSU, PLIPSU, PMINSU, POASU, PROSUF, PROSUI

! Open file to write results from IPGROW_SC.     Added for CASUPRO (FSR)

!      OPEN(UNIT = 2000, FILE = "CSP01TEST.OUT", STATUS = "UNKNOWN",
!     &   ACTION = "WRITE", POSITION = "APPEND", IOSTAT = OpenStatus)
!
!      WRITE(2000,'')
!      WRITE(2000,'')
!      WRITE(2000,'(1X,"RESULTS FROM CSP_IPGROW.for")')
!      WRITE(2000,'(1X,"--------------------------")')
!      WRITE(2000,'')
!-----------------------------------------------------------------------
      CALL GETLUN('FILEIO', LUNIO)
      OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
      LNUM = 0
!C-----------------------------------------------------------------------
!C    Find and read Cultivar Section
!C-----------------------------------------------------------------------
      SECTION = '*CULTI'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
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
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILEIO, LNUM)
        ELSE
          READ(LUNIO,'(24X,F6.1,5X,A1,6X,F6.0)',IOSTAT=ERR)
     &            PLTPOP, PLME, ROWSPC; LNUM = LNUM + 1
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
        ENDIF

!-----------------------------------------------------------------------
!    Read Cultivar Section
!-----------------------------------------------------------------------
        SECTION = '*CULTI'
        CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILEIO, LNUM)
        ELSE
          READ(LUNIO,'(24X,A6,61X,F5.2)',IOSTAT=ERR) ECONO, WTPSD
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
        CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(ERRKEY, 1, FILECC, LNUM)
        ELSE
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(24X,F6.0)',IOSTAT=ERR) RMIN
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDIF
  
        SECTION = '!*PLAN'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILECC, LNUM)
        ELSE
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(F6.0,6X,2F6.0,6X,F6.0)',IOSTAT=ERR)
     &                    PROLFI, PROLFF, PROSTI, PROSTF
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(F6.0,6X,2F6.0,6X,F6.0)',IOSTAT=ERR)
     &                    PRORTI, PRORTF, PROSUI, PROSUF
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!         READ(C80,'(12X,F6.0)',IOSTAT=ERR) PRONOD    !Out for CASUPRO       
!         IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(4F6.0)',IOSTAT=ERR)
     &            PCARLF, PCARST, PCARRT, PCARSU
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(4F6.0)',IOSTAT=ERR)
     &            PLIPLF, PLIPST, PLIPRT, PLIPSU
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(4F6.0)',IOSTAT=ERR)
     &            PLIGLF, PLIGST, PLIGRT, PLIGSU
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(4F6.0)',IOSTAT=ERR)
     &          POALF, POAST, POART, POASU
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(4F6.0)',IOSTAT=ERR)
     &            PMINLF, PMINST, PMINRT, PMINSU
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDIF

        SECTION = '!*CARB'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILECC, LNUM)
        ELSE
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!         READ(C80,'(4X,A2)',IOSTAT=ERR) XPODF          !Out for CASUPRO
!         IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(4F6.0)',IOSTAT=ERR) ALPHL, ALPHS, ALPHR, ALPHSU
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDIF

!        DO I = 1,2                                    !Out for CASUPRO
!         XPODF(I:I) = UPCASE(XPODF(I:I))
!        ENDDO

!-----------------------------------------------------------------------
        SECTION = '!*VEGE'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND .EQ. 0) THEN
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

!-----------------------------------------------------------------------
C    Read Ecotype Parameter File
C-----------------------------------------------------------------------
        CALL GETLUN('FILEE', LUNECO)
        OPEN (LUNECO,FILE = FILEGC,STATUS = 'OLD',IOSTAT=ERR)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,0)

        ECOTYP = '      '
        LNUM = 0
        DO WHILE (ECOTYP .NE. ECONO)
          CALL IGNORE(LUNECO, LNUM, ISECT, C255)
        IF (ISECT .EQ. 1 .AND. C255(1:1) .NE. ' ' .AND.
     &          C255(1:1) .NE. '*') THEN
            READ (C255,'(A6,48X,F5.2)',IOSTAT=ERR)
     &        ECOTYP, SDPRO
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)
            IF (ECOTYP .EQ. ECONO) THEN
              EXIT
            ENDIF

          ELSE IF (ISECT .EQ. 0) THEN
            IF (ECONO .EQ. 'DFAULT') CALL ERROR(ERRKEY,35,FILEGC,LNUM)
            ECONO = 'DFAULT'
            REWIND(LUNECO)
          LNUM = 0
          ENDIF
        ENDDO
  
        CLOSE (LUNECO)

      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END  ! SUBROUTINE CSP_IPGROW
C=======================================================================

!-----------------------------------------------------------------------
!       Variable definitions:
!-----------------------------------------------------------------------
! AGEFAC   Reduction in photosynthesis, correlated to a decrease in 
!            percentage of Nitrogen in leaves (0-1) (fraction)
! ALFDOT   Rate of leaf expansion (cm2[leaf] / m2[ground] / d)
! ALPHL    Fraction of new leaf growth that is mobile C (fraction)
! ALPHR    Fraction of new root growth that is mobile C (fraction)
! ALPHS    Fraction of new seed growth that is mobile C (fraction)
! ALPHSH   Fraction of new shell growth that is mobile C (fraction)
! ALPHSU   Fraction of new sugars growth that is mobile C (fraction)
! AREAH    Area of healthy leaves (cm2[leaf] / m2[ground])
! AREALF   Area of leaves (one side) per unit ground area
!            (cm2[leaf] / m2[ground])
! BETN     Spacing between plants in a row (m / plant)
! CADLF    Mass of CH2O added to leaf reserves after growth
!            (g[CH2O] / m2 / d)
! CADST    Mass of CH2O added to stems (g[CH2O] / m2 / d)
! CANNAA   Weight of N in total plant at flowering (g[N] / m2)
! CANWAA   Canopy weight at flowering stage (g[plant] / m2)
! CLW      Cumulative leaf growth (g[leaf]/m2)
! CPFLF    Respiration requirement for net leaf growth
!            (g[CH20] / g[tissue])
! CPFNOD   Respiration requirement for net nodule growth
!            (g[CH20] / g[tissue])
! CPFRT    Respiration requirement for net root growth
!            (g[CH20] / g[tissue])
! CPFSD1   Respiration requirement for net seed growth
!            (g[CH20] / g[tissue])
! CPFSH1   Respiration requirement for net leaf growth
!            (g[CH20] / g[tissue])
! CPFSTM   Respiration requirement for net stem growth
!            (g[CH20] / g[tissue])
! CPFSU    Respiration requirement for net sugars growth
!            (g [CH20] / g [tissue]) +++
! CROP     Crop identification code 
! CRUSLF   C mobilized from leaf tissue in a day (g[CH2O] / m2 / d)
! CRUSRT   C mobilized from root tissue in a day (g[CH2O] / m2 / d)
! CRUSSH   C mobilized from shell tissue in a day (g[CH2O] / m2 / d)
! CRUSST   C mobilized from stem tissue in a day (g[CH2O] / m2 / d)
! CRUSSU   C mobilized from sugars in a day (g [CH2O] / m2 - d)
! CSUW     Cumulative sugar accumulation growth (g [sugar] / m2)
! DAP      Number of days after planting (d)
! DISLA    Diseased leaf area (cm2/m2/d)
! DWNOD    Current nodule mass (g[nodule] / m2)
! DWNODA   Cumulative nodule growth (g[nodule] / m2)
! DYNAMIC  Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
!            INTEGR, OUTPUT, or FINAL 
! ECONO    Ecotype code - used to match ECOTYP in .ECO file 
! ECOTYP   Ecotype code for this simulation 
! F        Specific leaf area of new leaf tissue growth, including N
!            (cm2[leaf] / g[leaf])
! FILECC   Path plus filename for species file (*.spe) 
! FILEGC   Pathname plus filename for ECO file 
! FILEIO   Filename for INP file (e.g., IBSNAT35.INP) 
! FRLF     Fraction of vegetative tissue growth that goes to leaves on a 
!            day (g[leaf] / g[veg])
! FRSTM    Fraction of vegetative tissue growth that goes to stems on a day
!            (g[stem] / g[veg])
! GROWTH   Total new growth of plant tissue on a day (g[tissue] / m2 / d)
! GRWRES   Growth respiration (g[CH2O]/m2-d)
! IDETO    Switch for printing OVERVIEW.OUT file 
! IHARI    Harvest type code: M=at harvest maturity, R=on specified day of 
!            year (HDATE), D= on specified day after planting (HDATE), G= 
!            at specified growth stage (HSTG), A= within specified window 
!            when SW conditions are met 
! ISWNIT   Nitrogen simulation switch (Y or N) 
! ISWSYM   Nitrogen fixation simulation switch (Y = simulate nodule growth, 
!            N = no nodule growth, U = N-fixation occurs at a rate that 
!            carbon will allow, and nodules are not grown explicitly) 
! LAIMX    Maximum leaf area index this season (m2[leaf] / m2[ground])
! LUNCRP   Logical unit number for FILEC (*.spe file) 
! LUNECO   Logical unit number for FILEE (*.eco file) 
! LUNIO    Logical unit number for FILEIO 
! NADLF    N added to leaf N reserves (g[N] / m2 / d)
! NADRT    N added to root N reserves (g[N] / m2 / d)
! NADST    N added to stem N reserves (g[N] / m2 / d)
! NADSU    N added to sugars N reserves (g [N] / m2 - d)
! NDTH     Nodule death rate (g[nodule] / m2 / d)
! NFIXN    Amount of N fixed during the day (g[N] / m2 / d)
! NGRLF    Maximum N demand for leaf growth (g[leaf N] / m2[ground] / d)
! NGRRT    Maximum N demand for root growth (g[root N] / m2[ground] / d)
! NGRSD    Rate of N accumulation in new seeds (g[N] / m2 / d)
! NGRSH    Rate of N accumulation in new shells (g[N] / m2 / d)
! NGRST    Maximum N demand for stem growth (g[stem N] / m2[ground] / d)
! NGRSU    Maximum N demand for sugars growth (g [sugars N] / m2 [ground] - d)
! NLALL    N added to leaves today (g[N]/m2-d)
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
! NRUSSU   N actually mobilized from sugars in a day (g [N] / m2 - d)
! NSALL    N added to stems today (g[N]/m2-d)
! NSDALL   N added to seeds today (g[N]/m2-d)
! NSDDOT   Net N addition for seeds (g[N] / m2[ground] / d)
! NSDOFF   N loss from seeds in a day (g[N]/m2-d)
! NSDOT    Net N addition for stems (g[N] / m2[ground] / d)
! NSHALL   N added to shells today (g[N]/m2-d)
! NSHDOT   Net N addition for shells (g[N] / m2[ground] / d)
! NSHOFF   N loss from shells in a day (g[N]/m2-d)
! NSOFF    N loss from stems in a day (g[N]/m2-d)
! NSUALL   N added to sugars today (g [N] / m2 - d)
! NSUDOT   Net N addition for sugars (g [N] / m2 [ground] - d)
! NSUOFF   N loss from sugars in a day (g [N]/m2 - d)
! NTOVR    (Not used) 
! PCARLF   Proportion of leaf tissue that is carbohydrate (fraction)
! PCARNO   Proportion of nodule tissue that is carbohydrate (fraction)
! PCARRT   Proportion of root tissue that is carbohydrate (fraction)
! PCARSD   Proportion of seed tissue that is carbohydrate
!            (g[CH2O] / g[seed])
! PCARSH   Proportion of shell tissue that is carbohydrate (fraction)
! PCARST   Proportion of stem tissue that is carbohydrate (fraction)
! PCARSU   Proportion of sugars that is carbohydrate (fraction)
! PCCSD    Percentage of carbohydrate in seed tissue (100 g[C] / g[seed])
! PCLSD    Percentage of lipid in seed tissue (100 g[lipid] / g[seed])
! PCNL     Percentage of N in leaf tissue (100 g[N] / g[leaf])
! PCNMIN   Minimum percentage of leaf N composition after N mining
!            ( 100 g[N] / g[leaf])
! PCNRT    Percent N in root tissue (100 g[N] / g[root])
! PCNSD    Percentage of N in seed tissue (100 g[N] / g[seed])
! PCNSH    Percentage of N in shell tissue (100 g[N] / g[shell])
! PCNST    Percent N in stem tissue (100 g[N] / g[stem])
! PCNSU    Percentage of N in sugars (100 g [N] / g [sugars]) +++
! PLIGLF   Proportion of leaf tissue that is lignin (fraction)
! PLIGNO   Proportion of nodule tissue that is lignin (fraction)
! PLIGRT   Proportion of root tissue that is lignin (fraction)
! PLIGSD   Proportion of seed tissue that is lignin (fraction)
! PLIGSH   Proportion of shell tissue that is lignin (fraction)
! PLIGST   Proportion of stem tissue that is lignin (fraction)
! PLIGSU   Proportion of sugars that is lignin (fraction)
! PLIPLF   Proportion of leaf tissue that is lipid (fraction)
! PLIPNO   Proportion of nodule tissue that is lipid (fraction)
! PLIPRT   Proportion of root tissue that is lipid (fraction)
! PLIPSH   Proportion of shell tissue that is lipid (fraction)
! PLIPST   Proportion of stem tissue that is lipid (fraction)
! PLIPSU   Proportion of sugars that is lipid (fraction)
! PLME     Planting method
! PLTPOP   Plant population (# plants / m2)
! PMINLF   Proportion of leaf tissue that is mineral (fraction)
! PMINNO   Proportion of nodule tissue that is mineral (fraction)
! PMINRT   Proportion of root tissue that is mineral (fraction)
! PMINSD   Proportion of seed tissue that is mineral (fraction)
! PMINSH   Proportion of shell tissue that is mineral (fraction)
! PMINST   Proportion of stem tissue that is mineral (fraction)
! PMINSU   Proportion of sugars that is mineral (fraction)
! POALF    Proportion of leaf tissue that is organic acid (fraction)
! POANO    Proportion of nodule tissue that is organic acid (fraction)
! POART    Proportion of root tissue that is organic acid (fraction)
! POASD    Proportion of seed tissue that is organic acid (fraction)
! POASH    Proportion of shell tissue that is organic acid (fraction)
! POAST    Proportion of stem tissue that is organic acid (fraction)
! POASU    Proportion of sugars that is organic acid (fraction)
! PODWT    Dry mass of seeds plus shells, including C and N
!            (g[pods] / m2[ground])
! POTCAR   Potential carbohydrate composition of seed based on temperature
!            (fraction)
! POTLIP   Potential lipid composition of seed based on temperature
!            (fraction)
! PPLTD    Percent plants destroyed (%/m2/d)
! PROLFF   Minimum leaf protein composition after N mining
!            ( g[protein] / g[leaf])
! PROLFI   Maximum protein composition in leaves during growth with 
!            luxurious supply of N (g[protein] / g[leaf tissue])
! PRONOD   Protein composition in nodules (g[protein] / g[nodule])
! PRORTF   Minimum root protein composition after N mining
!            ( g[protein] / g[root])
! PRORTI   Maximum protein composition in roots during growth with 
!            luxurious supply of N (g[protein] / g[root])
! PROSHF   Minimum shell protein composition after N mining
!            (g[protein] / g[shell])
! PROSTF   Minimum stem protein composition after N mining
!            (g[protein] / g[stem])
! PROSTI   Maximum protein composition in stems during growth with 
!            luxurious supply of N (g[protein] / g[stem])
! PROSUF   Minimum sugars protein composition after N mining
!            (g [protein] / g [sugars])
! PROSUI   Maximum protein composition in sugars during growth with 
!            luxurious supply of N (g [protein] / g [sugars])
! PUNCSD   Cumulative puncture damage to seed (not yet implemented) 
! PUNCTR   Cumulative puncture damage (not yet implemented) 
! PUNDOT   Daily puncture damage (not yet implemented) 
! RHOL     Fraction of leaf which is carbohydrate (g [CH20] / g[leaf])
! RHOR     Fraction of root which is carbohydrate (g [CH2O] / g[root])
! RHOS     Fraction of stem which is carbohydrate (g [CH2O] / g[stem])
! RHOSH    Fraction of shell which is carbohydrate (g [CH2O] / g[shell])
! RHOSU    Fraction of sugars which is carbohydrate (g [CH20] / g [sugars])
! RMIN     Respiration required for synthesizing mineral structure
!            (g[CH2O] / g[mineral])
! RNITP    True nitrogen concentration in leaf tissue for photosynthesis 
!            reduction. (%)
! ROWSPC   Row spacing (m)
! RTWT     Dry mass of root tissue, including C and N
!            (g[root] / m2[ground])
! SDIDOT   Number of seeds destroyed on the current day (#/m2/d)
! SDLIP    Maximum lipid composition in seed (fraction)
! SDNPL    Seed N (g[N] / m2)
! SDPDOT   Daily seed puncture damage (not yet implemented) 
! SDPRO    Seed protein fraction at 25oC (g[protein] / g[seed])
! SDPROR   Ratio to adjust lipid and carbohydrate proportions when seed 
!            protein differs from protein composition of standard cultivar 
!            (SDPROS) 
! SDRATE   Seeding rate, mass of seed sown (g[seed] / m2[ground])
! SDWT     Dry mass of seed tissue, including C and N
!            (g[seed] / m2[ground])
! SDWTAM   Seed weight at maturity (g / m2)
! SDWTPL   Initial planting material dry weight (kg / ha)
! SEEDNI   Seed or transplant N at planting (g[N] / m2)
! SEEDNO   Total number of seeds (#/m2)
! SENSOILC Daily contribution of senesced plant matter to soil carbon
!            (g[C] / m2 / day)
! SENSOILL Daily contribution of senesced plant matter to soil lignin
!            (g[lignin] / m2 / day)
! SENSOILN Daily contribution of senesced plant matter to soil nitrogen
!            (g[N] / m2 / day)
! SENSURFC Daily contribution of senesced and frozen plant matter to 
!            surface carbon  (g[C] / m2 / day)
! SENSURFL Daily contribution of senesced and frozen plant matter to 
!            surface lignin (g[lignin] / m2 / day)
! SENSURFN Daily contribution of senesced and frozen plant matter to 
!            surface nitrogen (g[N] / m2 / day)
! SHELWT   Total mass of all shells (g / m2)
! SLA      Specific leaf area (cm2[leaf] / m2[ground])
! SLAAD    Specific leaf area, excluding weight of C stored in leaves
!            (cm2[leaf] / g[leaf])
! SLDOT    Defoliation due to daily leaf senescence (g/m2/day)
! SLNDOT   Leaf senescence due to water stress (g/m2/day)
! SRDOT    Daily root senescence (g / m2 / d)
! SSDOT    Daily senescence of petioles (g / m2 / d)
! SSNDOT   Petiole senescence due to water stress (g/m2/day)
! STMWT    Dry mass of stem tissue, including C and N (g[stem] / m2[ground)
! SUWT     Dry mass of sugars, including C and N
!            (g [sugars] / m2 [ground])
! SWIDOT   Daily seed mass damage (g/m2/day)
! TGROW    Cumulative growth of plant tissue (g[tissue] / m2)
! TOPWT    Total weight of above-ground portion of crop, including pods
!            (g[tissue] / m2)
! TOTWT    Total weight of crop (g[tissue] / m2)
! TRNH4U   Total N uptake in ammonium form in a day (g[N] / m2 / d)
! TRNO3U   Total N uptake in nitrate form in a day (g[N] / m2 / d)
! TRNU     Total N uptake in a day (g[N] / m2 / d)
! TURFAC   Water stress factor for expansion (0 - 1) 
! WCRLF    Mass of CH2O reserves in leaves (g[leaf CH2O] / m2[ground])
! WCRRT    Mass of CH2O reserves in roots (g[root CH2O] / m2[ground])
! WCRSH    Mass of CH2O reserves in shells (g[shell CH2O] / m2[ground])
! WCRST    Mass of CH2O reserves in stems (g[stem CH2O] / m2[ground])
! WCRSU    Mass of CH2O reserves in sugars (g [sugars CH2O] / m2 [ground])
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
! WNRSU    N available for mobilization from sugars above lower limit of 
!            mining (g [N] / m2)
! WPDOT    Net pod growth rate (g[pod] / m2 / d)
! WRCLDT   Net C addition for leaves (g[CH2O] / m2 /d)
! WRCRDT   Net C addition for roots (g[CH2O] / m2 /d)
! WRCSDT   Net C addition for stems (g[CH2O] / m2 /d)
! WRCSHD   Net C addition for shells (g[CH2O] / m2 /d)
! WRCSUDT  Net C addition for sugars (g [CH2O] / m2 - d)
! WRDOT    Net root growth rate (g[root] / m2 / d)
! WRDOTN   Dry weight growth rate of new root tissue including N but not C 
!            reserves (g[root] / m2[ground]-d)
! WRIDOT   Daily pest damage to root mass ( g/m2/day)
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
! WSUDOT   Net sugars growth rate (g [sugars] / m2 - d)
! WSUDOTN  Dry weight growth rate of new sugars including N but not C 
!            reserves (g [sugars] / m2 [ground] - d)
! WSUI     Initial weight of sugars (g [leaf] / m2)
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
! WTNSU    Mass of N in sugars (g [sugars N] / m2 [ground])
! WTNSUA   Cumulative N added to sugars (g [N] / m2 - d)
! WTNSUO   Cumulative N loss from sugars (g [N] / m2)! WTNTOT   Total plant N content (g[N] / m2[ground])
! WTNUP    Cumulative N uptake (g[N] / m2)
! WTPSD    Maximum weight per seed under non-limiting substrate (g / seed)
! WTRO     Cumulative root losses (g[root] / m2)
! WTSDO    Cumulative seed losses (g[seed] / m2)
! WTSHMT    Cohorts that reach THRESH today (g/m2)
! WTSHO    Cumulative shell losses (g[shell] / m2)
! WTSO     Cumulative stem losses (g[stem] / m2)
! WTSUO    Cumulative sugar losses (g [sugars] / m2)
! XHLAI    Leaf area index (m2[leaf] / m2[ground])
! XLAI     Leaf area (one side) per unit of ground area
!            (m2[leaf] / m2[ground])
! XPOD     Growth partitioning to pods which slows node appearance
!            (fraction)
! XPODF    Input parameter which determines the method of calculating XPOD 
! YRDOY    Current day of simulation (YYDDD)
! YRNR1    Day when 50% of plants have at least one flower (YYDDD)
! MDATE    Harvest maturity (YYDDD)
! YRPLT    Planting date (YYDDD)
C=======================================================================
!       END SUBROUTINES GROW and IPGROW
C=======================================================================
