C=======================================================================
C  FOR_GROW, Subroutine, G. Hoogenboom, J.W. Jones, and K.J.Boote
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
C  07-02-2003 SJR Added storage organ variables for forage model
C  07-06/2003 SJR Added STRSRFL, STRLYR! parameters to allow user to 
C                        specify distribution of storage organ dry mass
C                        between soil surface layer and soil layer 1
C                        used to distribute senesced material for CENTURY model
!  06/15/2022 CHP Added CropStatus
C-----------------------------------------------------------------------
C  Called by:  CROPGRO
C  Calls:      FOR_IPGROW, FOR_STRESS
C              ERROR
C=======================================================================
      SUBROUTINE FOR_GROW(CONTROL, ISWITCH, DYNAMIC, SOILPROP, 
     &  AGEFAC, CADLF, CADST, CRUSLF, CRUSRT, CRUSSH,     !Input
     &  CRUSST, DISLA, F, FILECC, FILEGC, FRLF, FRSTM,    !Input
     &  LFSCMOB, LFSENWT, LFSNMOB, LTSEN, NADLF, NADRT,   !Input
     &  NADST, NDTH, NFIXN, NGRLF, NGRRT, NGRSD, NGRSH,   !Input
     &  NGRST, NMINEA, NODGR, NOUTDO, NPLTD, NRUSLF,      !Input
     &  NRUSRT, NRUSSH, NRUSST, POTCAR, POTLIP,           !Input
     &  PPLTD, RTSCMOB, RTSNMOB, SDIDOT, SDPROR,          !Input
     &  SENNOD, SENRT, SLDOT, SLMDOT, SLNDOT,             !Input
     &  SLNADDOT, SRDOT, SRMDOT, SRNADDOT,                !Input
     &  SRNDOT, SRSCMOB, SRSNMOB, SSDOT, SSMDOT,          !Input
     &  SSNADDOT, SSNDOT, SSRMDOT, SSRNADDOT,             !Input
     &  STSCMOB, STLTSEN, STSENWT, STSNMOB, TRNH4U,       !Input
     &  TRNO3U, TRNU, TURFAC, WLDOTN, WLIDOT, WRDOTN,     !Input
     &  WRIDOT, WSDDTN, WSDOTN, WSHDTN, WSIDOT, WTABRT,   !Input
     &  WTSHMT, YRNR1, MDATE, YRPLT,                      !Input
     &  FHLEAF,FHSTEM,FHVSTG,                             !Input

     &  SWIDOT, WLFDOT, WSHIDT, WTNFX, XHLAI,             !Input/Output

     &  AREALF, BETN, CANNAA, CANWAA, CLW, CropStatus,    !Output
     &  CSW, DWNOD, DWNODA, GROWTH, GRWRES, LAIMX, PCCSD, !Output
     &  PCLSD, PCNL, PCNRT, PCNSD, PCNSH, PCNST, PLTPOP,  !Output
     &  PLIGLF, PLIGNO, PLIGRT, PLIGSD, PLIGSH, PLIGST,   !Output
     &  PODWT, PUNCSD, PUNCTR, RHOL, RHOS, RNITP,         !Output
     &  ROWSPC, RTWT, SDNPL, SDRATE, SDWT, SDWTAM,        !Output
     &  SEEDNI, SEEDNO, SENESCE, SHELWT, SLA,             !Output
     &  SLAAD, STMWT, TOPWT, TOTWT, WCRLF, WCRRT, WCRSH,  !Output
     &  WCRST, WNRLF, WNRRT, WNRSH, WNRST, WTCO,          !Output
     &  WTLF, WTLO, WTMAIN, WTNCAN, WTNEW, WTNLA, WTNLF,  !Output
     &  WTNLO, WTNNA, WTNNAG, WTNNO, WTNNOD, WTNOO,       !Output
     &  WTNRA, WTNRO, WTNRT, WTNSA, WTNSD, WTNSDA,        !Output
     &  WTNSDO, WTNSH, WTNSHA, WTNSHO, WTNSO, WTNST,      !Output
     &  WTNUP, WTRO, WTSDO, WTSHO, WTSO, XLAI, XPOD,      !Output

     &  CADRT, CADSH, NADSH,                              !Input
     &  CADSR, CRUSSR, FRSTR, NADSR, NGRSR, NRUSSR,       !Input
     &  PSRLYRD, PSRSRFD, PSRSRFL, PSRLYR1, SSRDOT,       !Input
     &  SSRNDOT, STRSRFL, STRLYR1, WSRDOTN, WSRIDOT,      !Input
     &  WSFDOT, WSRFDOT,                                  !Input/Output
     &  CSRW, PCNSR, PLIGSR, RHOSR, STRWT, WCRSR,         !Output
     &  WNRSR, WTNSR, WTNSRA, WTNSRO, WTSRO,              !Output
     
     &  ALPHL, ALPHR, ALPHS, ALPHSH, ALPHSR, PCARSR,      !Output
     &  PLIPSR, PMINSR, POASR, PROSRF,CPFSTR, NSRALL,     !Output
     &  NSRDOT, NSROFF, TPSRLYR1,TPSRSRFL, WRCSRDT,       !Output
     &  WSRDOT, WSRI,                                     !Output

     &  NLALL, NRALL, NSALL,                              !Output
     &  PCHOLFF, PCHORTF, PCHOSRF, PCHOSTF,               !Output
     &  RHOR, WLDOT, WRCLDT, WRCRDT, WRCSDT, WRCSHD,      !Output
     &  WRDOT, WSDOT,                                     !Output

     &  VSTAGE, DWTCO, DWTLO, DWTSO,
     &  PWTCO, PWTLO, PWTSO)                         !Input/Output

!     2023-01-26 chp removed unused variables in argument list:
!       PORPT, TSCMOB, SLCADDOT, SRCADDOT, SSCADDOT

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
        ! which contain control information, soil
        ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL FOR_IPGROW, ERROR, FOR_STRESS
      SAVE
!-----------------------------------------------------------------------

      CHARACTER*1  ISWSYM, ISWNIT, IDETO, IHARI, PLME
      CHARACTER*2  XPODF, CROP
      CHARACTER*6  ERRKEY
      PARAMETER (ERRKEY = 'FRGROW')
      CHARACTER*30 FILEIO
      CHARACTER*92 FILECC, FILEGC
!     CHARACTER*78 MSG(2)
!     CHARACTER*6   SECTION
!     CHARACTER(len=6) trtchar
      character(len=60) ename
!     CHARACTER*78 MESSAGE(2)

      INTEGER DYNAMIC, NOUTDO, L, NLAYR
      INTEGER YRDOY, YRNR1, MDATE
      INTEGER YRPLT, RUN, CropStatus
!     INTEGER MOWLUN,ISECT,ERR, I
!     INTEGER,ALLOCATABLE,DIMENSION(:) :: TRNO,DATE
!     INTEGER LUNCRP,fhlun, LNUM, FOUND
!     INTEGER MOWCOUNT,j
      INTEGER TRTNO
      

      REAL WTNUP,WTNFX,WTNMOB,WTNCAN,TGROW
      REAL WRCSHD,DISLA,WSDMAN
      REAL WLFDOT, PPLTD,NPLTD,SDWTAM,CANNAA,CANWAA
      REAL DWNODA,WTNNAG,AGEFAC
      REAL POTLIP,POTCAR      !PORPT, 
      REAL CPFLF, CPFSTM, CPFRT, CPFNOD, CPFSH1, CPFSD1
      REAL WRIDOT,WSIDOT,WTNNOD
      REAL WNDOT, WPDOT, WDOT,WRCLDT,WRCSDT,WRCRDT

      REAL NLOFF, NSOFF, NROFF, NSHOFF, NSDOFF, NNOFF
      REAL WTLO, WTSO, WTRO, WTSHO, WTSDO, WTNOO
      REAL WTCO, DWTCO, DWTLO, DWTSO, PWTCO, PWTLO, PWTSO
      REAL NLALL, NSALL, NRALL, NSHALL, NSDALL
      REAL WTNLA, WTNSA, WTNRA, WTNSHA, WTNSDA, WTNNA
      REAL WTNLO, WTNSO, WTNRO, WTNSHO, WTNSDO, WTNNO
      REAL WLDOT, WSDOT, WRDOT

      REAL WLDOTN, WSDOTN, WRDOTN, WSDDTN, WSHDTN
      REAL NGRLF, NGRST, NGRRT, NGRSH, NGRSD
      REAL WTLF, STMWT, RTWT, SHELWT, SDWT
      REAL PROLFF, PROSTF, PRORTF, PROSHF
      REAL PCNL, PCNST, PCNRT, PCNSH, PCNSD
      REAL WTNLF, WTNST, WTNRT, WTNSH, WTNSD
      REAL NLDOT, NSDOT, NRDOT, NSHDOT, NSDDOT
      REAL NRUSLF, NRUSST, NRUSRT, NRUSSH
      REAL CRUSLF, CRUSST, CRUSRT, CRUSSH
      REAL ALPHL, ALPHS, ALPHR, ALPHSH, TNCFAC
      REAL RHOL, RHOS, RHOR, RHOSH
      REAL WCRLF, WCRST, WCRRT, WCRSH
      REAL NADLF, NADST, NADRT
      REAL WNRLF, WNRST, WNRRT, WNRSH
      REAL CADLF, CADST

      REAL LTSEN, SLDOT, SSDOT, SRDOT, SWIDOT

      REAL EODLFC, EODLFN, EODSTC, EODSTN
      REAL EODRTC, EODRTN, EODSRC, EODSRN
      REAL LFCADDM, LFNADDM, RTCADDM, RTNADDM
      REAL SRCADDM, SRNADDM, STCADDM, STNADDM
!     REAL LFNADFDM, STNADFDM, SRNADFDM
      REAL SLNADDOT, SRNADDOT       !SLCADDOT, SRCADDOT, 
      REAL SSNADDOT, SSRNADDOT !SSCADDOT, SSRCADDOT, 
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
      REAL ALFDOT, F, SLA, AREALF, XLAI, SLAAD
      REAL LAIMX, AREAH
      REAL XHLAI, SEEDNO, PLTPOP, ROWSPC, BETN
      REAL TURFAC
      REAL GROWTH, NODGR
!     REAL,ALLOCATABLE,DIMENSION(:) :: MOW,RSPLF,MVS,rsht

      REAL FRLF, FRSTM
      REAL SDNPL, SEEDNI, SDRATE
      REAL WTNEW

      REAL PCARLF, PCARNO, PCARRT, PCARSD, PCARSH, PCARST,
     &  PLIGLF, PLIGNO, PLIGRT, PLIGSD, PLIGSH, PLIGST,
     &  PLIPLF, PLIPNO, PLIPRT, PLIPSH, PLIPST,
     &  PMINLF, PMINNO, PMINRT, PMINSD, PMINSH, PMINST,
     &  POALF, POANO, POART, POASD, POASH, POAST,
     &  PROLFI, PRONOD, PRORTI, PROSTI,
     &  SDPRO, WTFSD, WTPSD, SDWTPL

      REAL RMIN, SDLIP, WLFI, WSTI, WRTI
      REAL CLW, CSW

!     Surface and soil residue due to daily senescence of plant matter
!      REAL SENCLN(0:NL, 3), SENRT(NL), SENNOD(NL)
      REAL SENRT(NL), SENNOD(NL)
      REAL SenWt(0:NL)        !kg[dry matter]/ha
      REAL SenLig(0:NL)       !kg[lignin]/ha
      REAL SenE(0:NL,NELEM)   !kg[E]/ha (E=N, P, S,...)

!CHP - puncture variables, not functional
      REAL PUNCSD, PUNCTR, PUNDOT, SDPDOT 

C-----------------------------------------------------------------------
C      New shell and root variables for N accounting
C-----------------------------------------------------------------------
      REAL CADRT, CADSH, NADSH
C-----------------------------------------------------------------------
C      New storage variables for forage model 
C-----------------------------------------------------------------------
      REAL CADSR, CRUSSR, FRSTR, NADSR, NGRSR,  
     &  NRUSSR, SSRDOT, SSRNDOT, WSRDOTN, WSRIDOT,  
     &  WSRDOT, WSRFDOT,                                     
     &  CSRW, PCNSR, PLIGSR, RHOSR, STRWT, WCRSR,   
     &  WNRSR, WTNSR, WTNSRA, WTNSRO, WTSRO,        
     &  CPFSTR, NSRDOT, ALPHSR, 
     &  PCARSR, PLIPSR, PMINSR, 
     &  POASR, PROSRI, PROSRF, WSRI, WRCSRDT, NSROFF, NSRALL,
     &  PSRLYRD, PSRSRFD, PSRSRFL, PSRLYR1, STRSRFL, 
     &  STRLYR1, TPSRSRFL, TPSRLYR1

      REAL CLOFF, CROFF, CSOFF, CSROFF, 
     &   PCHOLFF, PCHORTF, PCHOSRF, PCHOSTF

!     REAL PSTCLSMOB, PSTCLSTRES

      REAL LFSCMOB, LFSENWT, LFSNMOB, RTSCMOB, RTSNMOB, SRSCMOB,  
     &   SRSNMOB, STSCMOB, STSNMOB        !, TSCMOB
      REAL SLMDOT, SRMDOT, SRNDOT, SSMDOT, SSRMDOT, STLTSEN, STSENWT 
C-----------------------------------------------------------------------
C      New stem freeze damage variables for forage model 
C-----------------------------------------------------------------------
      REAL WSFDOT  

C------------------------------------------------------------
C  PDA 5/6/2010 VARIABLES ADDED FOR FORAGE HARVEST SUBROUTINE
C------------------------------------------------------------
      REAL FHLEAF,FHSTEM,FHVSTG
C-----------------------------------------------------------------------
C      VSTAGE for adjustment due to senescence and freeze damage 
C-----------------------------------------------------------------------
      REAL VSTAGE  

C-----------------------------------------------------------------------
C      New parameters for variable N  and CHO at senescence 
C-----------------------------------------------------------------------
      REAL SENCLV, SENCRV, SENCSV, SENCSRV
      REAL SENNLV, SENNRV, SENNSV, SENNSRV

!     Constructed variable types defined in ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (SoilType)    SOILPROP
      TYPE (ResidueType) SENESCE
      
      YRDOY  = CONTROL % YRDOY
      crop   = control % crop
      trtno  = control % trtnum
      run    = control % run
      ename  = control % ename
      
!     Transfer values from constructed data types into local variables.
      !Don't get DYNAMIC from CONTROL variable because it will not
      ! have EMERG value (set only in CROPGRO).
      !DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      RUN     = CONTROL % RUN
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
      CALL FOR_IPGROW(
     &  FILEIO, FILECC, FILEGC, CROP,                           !Input
     &  ALPHL, ALPHR, ALPHS, ALPHSH, TNCFAC,                    !Output
     &  PCARLF, PCARST, PCARRT, PCARSH, PCARSD, PCARNO,         !Output
     &  PLIGLF, PLIGST, PLIGRT, PLIGSH, PLIGSD, PLIGNO,         !Output
     &  PLIPLF, PLIPST, PLIPRT, PLIPSH, PLIPNO, !Output
     &  PMINLF, PMINST, PMINRT, PMINSH, PMINSD, PMINNO,         !Output
     &  POALF, POAST, POART, POASH, POASD, POANO,          !Output
     &  PROLFF, PROSTF, PRORTF, PROSHF, PRONOD, !Output
     &  PROLFI, PROSTI, PRORTI,                                 !Output
     &  PLTPOP, ROWSPC, RMIN, PLME, SDWTPL,                !Output
     &  SENCLV, SENCRV, SENCSV, SENCSRV,                   !Output
     &  SENNLV, SENNRV, SENNSV, SENNSRV,                   !Output           
     &  SDLIP, SDPRO, WTFSD, WTPSD, XPODF,                 !Output
     &  ALPHSR, PCARSR, PCHOLFF, PCHORTF, PCHOSRF, PCHOSTF,!Output
     &  PLIGSR, PLIPSR, PMINSR, POASR, PROSRF, PROSRI)     !Output
 

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
     &    + PMINLF*RMIN + PCARLF*0.170)
        CPFSTM = 30./44.*(PLIPST*1.720 + PLIGST*0.659 + POAST*(-0.011)
     &    + PMINST*RMIN + PCARST*0.170)
        CPFSTR = 30./44.*(PLIPSR*1.720 + PLIGSR*0.659 + POASR*(-0.011)
     &    + PMINSR*RMIN + PCARSR*0.170)
        CPFRT  = 30./44.*(PLIPRT*1.720 + PLIGRT*0.659 + POART*(-0.011)
     &    + PMINRT*RMIN + PCARRT*0.170)
        CPFNOD =  30./44.*(PLIPNO*1.720 + PLIGNO*0.659 + POANO*(-0.011)
     &    + PMINNO*RMIN + PCARNO*0.170)
        CPFSH1 =  30./44.*(PLIPSH*1.720 + PLIGSH*0.659 + POASH*(-0.011)
     &    + PMINSH*0.073 + PCARSH*0.170)
        CPFSD1 = 30./44.*(PMINSD*0.073 + PLIGSD*0.659 + POASD*(-0.011)
     &    + (SDLIP*1.720 + PCARSD*0.170)*(1. - SDPROR))
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
      SDWT   = 0.0
      SDWTAM = 0.0
      SEEDNI = 0.0
      SEEDNO = 0.0

!      SENCLN = 0.0
      SenWt  = 0.0
      SenLig = 0.0
      SenE   = 0.0
      SENESCE % ResWt  = SenWt
      SENESCE % ResLig = SenLig
      SENESCE % ResE   = SenE

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
      DWTCO  = 0.0
      DWTLO  = 0.0
      DWTSO  = 0.0
      PWTCO  = 0.0
      PWTLO  = 0.0
      PWTSO  = 0.0
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
      CSRW= 0.0      !Cumulative storage organ growth

      SDPDOT = 0.0    !CHP - not used
      PUNDOT = 0.0    !CHP - not used

      NSRDOT = 0.0
      PCNSR  = 0.0
      RHOSR  = 0.0
      STRWT  = 0.0
      WCRSR  = 0.0
      WNRSR  = 0.0
      WTNSRA = 0.0
      WTSRO  = 0.0
      WTNSR  = 0.0
      WTNSRO = 0.0



!-----------------------------------------------------------------------
! KGB 02/12/03 Set SLA to zero until emergence
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
      WNDOT  = 0.0
      WPDOT  = 0.0
      WLFDOT = 0.0
      WSFDOT = 0.0
      WSRDOT = 0.0
      WSRFDOT = 0.0
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
      STRWT = FRSTR * TOTWT
      RTWT  = TOTWT - TOPWT - STRWT

      WLFI   = WTLF
      WSTI   = STMWT
      WRTI   = RTWT
      WSRI  = STRWT

!     Initialize cumulative variables
      CLW   = WTLF
      CSW   = STMWT
      CSRW  = STRWT

!     CH2O reserves
      WCRLF = ALPHL * WTLF
      WCRST = ALPHS * STMWT
      WCRRT = ALPHR * RTWT
      WCRSR = ALPHSR * STRWT

!!     Initialize cumulative C variables
!      WCRLA = WCRLF
!      WCRSA = WCRST
!      WCRRA = WCRRT

!     Carbohydrate composition of plant components (fraction)
      RHOL   = ALPHL
      RHOR   = ALPHR
      RHOS   = ALPHS
      RHOSH  = ALPHSH
      RHOSR  = ALPHSR

!     Net carbon addition variables
      WRCLDT = 0.0
      WRCSDT = 0.0
      WRCRDT = 0.0
      WRCSHD = 0.0
      WRCSRDT= 0.0

!     Compute N in plant components
      WTNLF  = WLFI * PROLFI * 0.16
      WTNST  = WSTI * PROSTI * 0.16
      WTNRT  = WRTI * PRORTI * 0.16
      WTNSR  = WSRI * PROSRI * 0.16
      WTNSH  = 0.0
      WTNSD  = 0.0
      WTNTOT = WTNLF + WTNST + WTNRT + WTNSH + WTNSD + WTNSR

!     Seed or transplant N at planting
      SDNPL  = WTPSD * SDPRO * 0.16 * 0.75 * PLTPOP -
     &  (WTNLF + WTNST + WTNRT + WTNSR)
      SDNPL  = MAX(SDNPL,0.0)
      SEEDNI = WTNLF + WTNST + WTNRT + SDNPL + WTNSR

!     Initialize cumulative N variables
      WTNLA  = WTNLF
      WTNSA  = WTNST
      WTNRA  = WTNRT
      WTNSRA = WTNSR

!     Percent N in plant components
      PCNL   = WTNLF / WLFI * 100.
      RNITP  = PCNL
      PCNST  = WTNST / WSTI * 100.
      PCNRT  = WTNRT / WRTI * 100.
      PCNSR  = WTNSR / WSRI * 100

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
      GROWTH = WLDOTN + WSDOTN + WRDOTN + WSHDTN + WSDDTN + NODGR
     &    + WSRDOTN
  
      GRWRES = WLDOTN*CPFLF + WSDOTN*CPFSTM + WRDOTN*CPFRT +
     &    NODGR*CPFNOD + WSHDTN*CPFSH1 + WSDDTN*CPFSD1 +
     &    WSRDOTN*CPFSTR +
     &    30./44*(TRNO3U/0.16 * 1.798 + TRNH4U/0.16 * 0.462 +
     &    NFIXN/0.16 * 1.798)



C-----------------------------------------------------------------------
C      8/2/05 SJR create new variables representing N refill and CH2O 
C      refill lost to pest and freeze damage - previously was calculated 
C      in several equations.  NADLF and CADLF lost with senesced tissues
C      was calculated and included as part of SxDOT in the SENES and 
C      ROOTS subroutines
C      10/06/05 SJR Subtracted SxmDOT from organ mass because no longer 
C      add C of N to today's naturally senesced tissues.
C-----------------------------------------------------------------------
      IF (WTLF-SLDOT.GT.0) THEN
        LFCADDM = CADLF * (MIN(1.0,(WLIDOT+WLFDOT)/(WTLF - SLDOT)))
        LFNADDM = NADLF * (MIN(1.0,(WLIDOT+WLFDOT)/(WTLF - SLDOT)))
      ELSE
        LFCADDM = 0
        LFNADDM = 0
      ENDIF
      IF (RTWT-SRDOT.GT.0)THEN
        RTCADDM = CADRT * (MIN(1.0,WRIDOT/(RTWT - SRDOT)))
        RTNADDM = NADRT * (MIN(1.0,WRIDOT/(RTWT - SRDOT)))
      ELSE
        RTCADDM = 0
        RTNADDM = 0
      ENDIF
      IF (STRWT-SSRDOT.GT.0)THEN
        SRCADDM = CADSR * (MIN(1.0,(WSRIDOT+WSRFDOT)/(STRWT-SSRDOT)))
        SRNADDM = NADSR * (MIN(1.0,(WSRIDOT+WSRFDOT)/(STRWT-SSRDOT)))
      ELSE
        SRCADDM = 0
        SRNADDM = 0
      ENDIF
      IF (STMWT-SSDOT.GT.0)THEN
        STCADDM = CADST * (MIN(1.0,(WSIDOT+WSFDOT)/(STMWT - SSDOT)))
        STNADDM = NADST * (MIN(1.0,(WSIDOT+WSFDOT)/(STMWT - SSDOT)))
      ELSE
        STCADDM = 0
        STNADDM = 0
      ENDIF

C-----------------------------------------------------------------------
C      8/2/05 SJR create new variables representing End-of-Day N and CH2O
C      concentrations.  Differs from PCNx as this includes NADxx and 
C      CADxx which are added prior to senescing tissues.  Simplifies 
C      several calculations.
C-----------------------------------------------------------------------
C      10/11/05 SJR Modified equations to accomodate new mobilization
C      and senescence scheme.  "Natural senescence" is calculated at the
C      beginning of the day and no C or N refill is added.  These tissues 
C      remain at yesterday's C and N concentrations.  The rest of the
C      tissues may have C annd N refill and, thus, have a different C
C      and N concentration.
C-----------------------------------------------------------------------
      if((WTLF + NADLF / 0.16 + CADLF - SLMDOT) .gt. 0.0)then
!     EODLFC = (WCRLF + CADLF) / (WTLF + NADLF / 0.16 + CADLF)
         EODLFC = (WCRLF + CADLF - LFSCMOB) / (WTLF + NADLF / 0.16 +
     &        CADLF - SLMDOT)
!     EODLFN = (WTNLF + NADLF) / (WTLF + NADLF / 0.16 + CADLF)
         EODLFN = (WTNLF + NADLF - LFSNMOB) / (WTLF + NADLF / 0.16 +
     &        CADLF - SLMDOT)
      else
         EODLFC = 0.0
         EODLFN = 0.0
      end if

      if((STMWT + NADST / 0.16 + CADST) .gt. 0.0)then
!      EODSTC = (WCRST + CADST) / (STMWT + NADST / 0.16 + CADST)
         EODSTC = (WCRST + CADST - STSCMOB) / (STMWT + NADST / 0.16 +
     &        CADST - SSMDOT)
!      EODSTN = (WTNST + NADST) / (STMWT + NADST / 0.16 + CADST)
         EODSTN = (WTNST + NADST - STSNMOB) / (STMWT + NADST / 0.16 + 
     &        CADST - SSMDOT)
      else
         EODSTC = 0.0
         EODSTN = 0.0
      end if

      if((RTWT + NADRT / 0.16 + CADRT) .gt. 0.0)then
!      EODRTC = (WCRRT + CADRT) / (RTWT + NADRT / 0.16 + CADRT)
         EODRTC = (WCRRT + CADRT - RTSCMOB) / (RTWT + NADRT / 0.16 + 
     &        CADRT - SRMDOT)
!      EODRTN = (WTNRT + NADRT) / (RTWT + NADRT / 0.16 + CADRT)
         EODRTN = (WTNRT + NADRT - RTSNMOB) / (RTWT + NADRT / 0.16 + 
     &        CADRT - SRMDOT)
      else
         EODRTC = 0.0
         EODRTN = 0.0
      end if

      if((STRWT + NADSR / 0.16 + CADSR) .gt. 0.0)then
!      EODSRC = (WCRSR + CADSR) / (STRWT + NADSR / 0.16 + CADSR)
         EODSRC = (WCRSR + CADSR - SRSCMOB) / (STRWT + NADSR / 0.16 +
     &        CADSR - SSRMDOT)
!      EODSRN = (WTNSR + NADSR) / (STRWT + NADSR / 0.16 + CADSR)
         EODSRN = (WTNSR + NADSR - SRSNMOB) / (STRWT + NADSR / 0.16 +
     &        CADSR - SSRMDOT)
      else
         EODSRC = 0.0
         EODSRN = 0.0
      end if

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
!      WLDOT = WLDOTN - (SLDOT - SLCADDOT - SLNADDOT / 0.16) - WLIDOT - 
!     &        WLFDOT - NRUSLF/0.16 - CRUSLF
C-----------------------------------------------------------------------
C     01/01/06 SJR Revised calculation of WxDOT - was missing corrections
C                        for LFSENWT, reorganized to make it easier to follow.
C-----------------------------------------------------------------------
!      WLDOT = WLDOTN - (SLDOT - SLCADDOT - SLNADDOT / 0.16) - WLIDOT - 
!     &        WLFDOT - (NRUSLF - LFSNMOB) / 0.16 - (CRUSLF - LFSCMOB)
      

!      IF (WTLF .GT. 0.0) THEN
!         WLDOT = WLDOT + CADLF - SLCADDOT - LFCADDM + 
!     &        NADLF / 0.16 - SLNADDOT / 0.16 - LFNADDM / 0.16
!      ENDIF
C-----------------------------------------------------------------------
C      Correct WLDOTN for mobilization (xRUSLF)
C      Subtract senesced DM but add back that part which was mobilized from natural senescence
C      prior to abscission as that was included in xRUSLF
C      Add back the part that was mobilized from senescence due to N-mobilization prior to abscission
C      Subtract losses to pest and freeze damage
C-----------------------------------------------------------------------
      WLDOT = WLDOTN - CRUSLF - NRUSLF/0.16-
     &    SLDOT - WLIDOT - WLFDOT
C--------------------------------------------
C PDA 5/6/2010  ADDED CODE FOR FORAGE HARVEST 
C--------------------------------------------
      IF (FHLEAF .GT. 0) THEN
        WLDOT=WLDOT-FHLEAF
      ENDIF
!      IF (FHLEAF .GT. 0) THEN
!        IF (WTLF .GT. 0) THEN
!          WLDOT = WLDOTN - SLDOT - WLIDOT - WLFDOT
!     &    - (FHLEAF - CADLF + LFCADDM - (NADLF-LFNADDM)/0.16)
!        ELSE
!          WLDOT = WLDOTN - SLDOT - WLIDOT - WLFDOT - FHLEAF
!        ENDIF
!      ELSE
!        WLDOT = WLDOTN - CRUSLF - NRUSLF/0.16
!     &        - SLDOT - WLIDOT - WLFDOT
!      ENDIF
C--------------------------------------------

C-----------------------------------------------------------------------
C      If started the day with some leaf mass, add CADLF and NADLF, corrected for parts lost with 
C      pest and freeze-damaged tissue and also mobilization (from other than natural senescence 
C      and low-light senescence.
C-----------------------------------------------------------------------
      IF (WTLF .GT. 0.0 .AND. FHLEAF.EQ.0) THEN
        WLDOT = WLDOT + CADLF - LFCADDM +
     &    (NADLF - LFNADDM)/0.16
      ENDIF

      IF (WLDOT .LT. 0.0) THEN
        WLDOT = MAX(WLDOT, -WTLF)
      ENDIF
      
C-----------------------------------------------------------------------
C       WSDOT = Net stem growth rate
C-----------------------------------------------------------------------
!      WSDOT = WSDOTN - (SSDOT - SSCADDOT - SSNADDOT / 0.16) - WSIDOT - 
!     &        WSFDOT - NRUSST / 0.16 - CRUSST 

!      WSDOT = WSDOTN - (SSDOT - SSCADDOT - SSNADDOT / 0.16) - WSIDOT - 
!     &        WSFDOT - (NRUSST - STSNMOB) / 0.16 - (CRUSST - STSCMOB)


!      IF (STMWT .GT. 0.0) THEN
!           WSDOT = WSDOT + CADST - SSCADDOT - STCADDM + 
!     &              NADST/0.16 - SSNADDOT / 0.16 - STNADDM /0.16
!      ENDIF

      WSDOT = WSDOTN - CRUSST - NRUSST/0.16-
     &    SSDOT  - WSIDOT - WSFDOT

C--------------------------------------------
C PDA 5/6/2010  ADDED CODE FOR FORAGE HARVEST 
C--------------------------------------------
      IF (FHSTEM.GT.0)THEN
        WSDOT=WSDOT-FHSTEM
      ENDIF
!      IF (FHSTEM .GT. 0) THEN
!        IF (STMWT .GT. 0) THEN
!          WSDOT = WSDOTN - SSDOT - WSIDOT - WSFDOT
!     &    - (FHSTEM - CADST + STCADDM - (NADST-STNADDM)/0.16)
!        ELSE
!          WSDOT = WSDOTN - SSDOT - WSIDOT - WSFDOT - FHSTEM
!        ENDIF
!      ELSE
!        WSDOT = WSDOTN - CRUSST - NRUSST/0.16
!     &        - SSDOT - WSIDOT - WSFDOT
!      ENDIF
C--------------------------------------------

      IF (STMWT .GT. 0.0 .AND. FHSTEM .EQ. 0) THEN
        WSDOT = WSDOT + CADST - STCADDM +(NADST - STNADDM)/0.16
      ENDIF

C--------------------------------------------

      IF (WSDOT .LT. 0.0) THEN
        WSDOT = MAX(WSDOT, -STMWT)
      ENDIF
C-----------------------------------------------------------------------
C       WSRDOT = Net storage tissue growth rate
C-----------------------------------------------------------------------
!      WSRDOT = WSRDOTN - (SSRDOT - SSRCADDOT - SSRNADDOT / 0.16) - 
!     &              WSRIDOT - WSRFDOT - NRUSSR / 0.16 - CRUSSR 

!      WSRDOT = WSRDOTN - (SSRDOT - SSRCADDOT - SSRNADDOT / 0.16) - 
!     &              WSRIDOT - WSRFDOT - (NRUSSR - SRSNMOB) / 0.16 - 
!     &              (CRUSSR - SRSCMOB) 


!      IF (STRWT .GT. 0.0) THEN
!         WSRDOT = WSRDOT + CADSR - SSRCADDOT - SRCADDM + 
!     &              NADSR/0.16 - SSRNADDOT / 0.16 - SRNADDM / 0.16
!      ENDIF

      WSRDOT = WSRDOTN - CRUSSR - NRUSSR/0.16-
     &    SSRDOT - WSRIDOT - WSRFDOT 

      IF (STRWT .GT. 0.0) THEN
        WSRDOT = WSRDOT + CADSR - SRCADDM +
     &    (NADSR - SRNADDM)/0.16
      ENDIF

      IF (WSRDOT .LT. 0.0) THEN
        WSRDOT = MAX(WSRDOT, -STRWT)
      ENDIF

C-----------------------------------------------------------------------
C     Allocate net growth to above and below ground storage organ tissue
C      using initial partitioning strategy from species file.
C      Allocate freeze losses according to the proportions calculated in FREEZE
C      Allocate all othe losses using current "actual" proportions.
C      If losses for one portion are greater than available mass
C      the excess loss is taken from the other portion
C      NOTE: using TPSRLYR1 and TPSRSRFL to hold intermediate values for 
C      themselves before calculating actual new proportions.
C-----------------------------------------------------------------------
      TPSRLYR1 = (WSRDOTN * STRLYR1) + (WSRDOT - WSRDOTN + WSRFDOT)
     &    * PSRLYR1 - WSRFDOT * PSRLYRD

      TPSRSRFL = (WSRDOTN * STRSRFL) + (WSRDOT - WSRDOTN + WSRFDOT) 
     &    * PSRSRFL - WSRFDOT * PSRSRFD

!      TPSRLYR1 = (WSRDOTN * STRLYR1) - ((SSRDOT - SSRCADDOT - 
!     &              SSRNADDOT / 0.16) + WSRIDOT + NRUSSR / 0.16 + CRUSSR)
!     &               * PSRLYR1 - WSRFDOT * PSRLYRD

!      TPSRSRFL = (WSRDOTN * STRSRFL) - ((SSRDOT - SSRCADDOT -  
!     &              SSRNADDOT / 0.16) + WSRIDOT + NRUSSR / 0.16 + CRUSSR)
!     &               * PSRSRFL - WSRFDOT * PSRSRFD



      IF (STRWT .GT. 0.0) THEN
!      TPSRLYR1 = TPSRLYR1 + CADSR * STRLYR1 - (SSRCADDOT + SRCADDM) * 
!     &              PSRLYRD + NADSR/0.16 * STRLYR1 - (SSRNADDOT / 0.16 +
!     &              SRNADDM / 0.16) * PSRLYR1 

!      TPSRSRFL = TPSRSRFL + CADSR * STRSRFL - (SSRCADDOT + SRCADDM) * 
!     &              PSRSRFD + NADSR/0.16 * STRSRFL - (SSRNADDOT / 0.16 +
!     &              SRNADDM / 0.16) * PSRSRFL
C-----------------------------------------------------------------------
C     Check to make sure not losing more mass from above or below ground
C      portions than exist in each.  If so, allocate excess loss to the
C      other portion.  If total loss is more than storage organ mass,
C      limit loss to actual mass of storage organ present
C      NOTE: must be done even when net gain is positive because freeze 
C      damage could result in a net loss of above ground tissue and a net 
C      gain of total and below ground tissue.
C-----------------------------------------------------------------------


        IF (-TPSRSRFL .GT. STRWT*PSRSRFL) THEN
        TPSRLYR1 = TPSRLYR1 + (TPSRSRFL+(STRWT*PSRSRFL))
        TPSRSRFL = -STRWT * PSRSRFL
        IF (-TPSRLYR1 .GE. STRWT*PSRLYR1) THEN
        TPSRLYR1 = -STRWT * PSRLYR1
        TPSRSRFL = -STRWT * PSRSRFL
        ENDIF
        ENDIF

        IF (-TPSRLYR1 .GT. STRWT*PSRLYR1) THEN
        TPSRSRFL = TPSRSRFL + (TPSRLYR1+(STRWT*PSRLYR1))
        TPSRLYR1 = -STRWT * PSRLYR1
        IF (-TPSRSRFL .GE. STRWT*PSRSRFL) THEN
        TPSRLYR1 = -STRWT * PSRLYR1
        TPSRSRFL = -STRWT * PSRSRFL
        ENDIF
        ENDIF

      ENDIF






C-----------------------------------------------------------------------
C     Calculate proportions of below and above ground storage organ 
C      tissue at end of day - with new growth and losses.
C      Still need current days proportions for allocating senesced
C      nutrients to appropriate soil layers for CENTURY
C-----------------------------------------------------------------------
! 2019-05-30 CHECK FOR ZERO DIVIDE CHP
      IF (STRWT + WSRDOT .GT. .001) THEN
        TPSRLYR1 = (TPSRLYR1 + (STRWT*PSRLYR1))/(STRWT+ WSRDOT)
        TPSRSRFL = (TPSRSRFL + (STRWT*PSRSRFL))/(STRWT+ WSRDOT)
      ELSE
        TPSRLYR1 = (TPSRLYR1 + (STRWT*PSRLYR1))/(.001)
        TPSRSRFL = (TPSRSRFL + (STRWT*PSRSRFL))/(.001)
      ENDIF

C-----------------------------------------------------------------------
C     Net root growth rate
C-----------------------------------------------------------------------
!      WRDOT = WRDOTN - (SRDOT - SRCADDOT - SRNADDOT / 0.16) - WRIDOT - 
!     &        NRUSRT/0.16 - CRUSRT 

!      WRDOT = WRDOTN - (SRDOT - SRCADDOT - SRNADDOT / 0.16) - WRIDOT - 
!     &        (NRUSRT - RTSNMOB) / 0.16 - (CRUSRT - RTSCMOB) 


!      IF (RTWT .GT. 0.0) THEN
!         WRDOT = WRDOT + CADRT - SRCADDOT - RTCADDM + 
!     &        NADRT/0.16 - SRNADDOT / 0.16 - RTNADDM / 0.16
!      ENDIF

      WRDOT = WRDOTN - CRUSRT - NRUSRT/0.16-
     &    SRDOT -      WRIDOT  

      IF (RTWT .GT. 0.0) THEN
        WRDOT = WRDOT + CADRT - RTCADDM +
     &    (NADRT - RTNADDM)/0.16
      ENDIF

      IF (WRDOT .LT. 0.0) THEN
        WRDOT = MAX(WRDOT, -RTWT)
      ENDIF
C-----------------------------------------------------------------------
C     Net shell growth rate
C-----------------------------------------------------------------------
      WSHIDT = MIN(WSHIDT,SHELWT)     ! pest damage to shells
      WSHDOT = WSHDTN - WSHIDT - WTABRT - NRUSSH / 0.16 - CRUSSH
      IF (SHELWT .GT. 0.0) THEN
        WSHDOT = WSHDOT + (CADSH + NADSH/0.16) *
     &   (1. - MIN(1.0,WTABRT/SHELWT))
      ENDIF


C-----------------------------------------------------------------------
C     Net seed growth rate
C-----------------------------------------------------------------------
      SWIDOT = MIN(SWIDOT,SDWT)       ! pest damage to seeds
      WSDDOT = WSDDTN - SWIDOT        
      WTLSD  = WTLSD + WSDDOT * POTLIP    !lipids in seed
      WTCSD  = WTCSD + WSDDOT * POTCAR    !carbohydrates in seed

C-----------------------------------------------------------------------
C     Net nodule growth rate
C-----------------------------------------------------------------------
      WNDOT  = NODGR - NDTH            

C-----------------------------------------------------------------------
C     Net pod growth rate
C-----------------------------------------------------------------------
      WPDOT = WSHDOT + WSDDOT          

C-----------------------------------------------------------------------
C     Total Net plant growth rate
C-----------------------------------------------------------------------
      WDOT = WLDOT + WSDOT + WRDOT + WPDOT + WNDOT + WSRDOT

C-----------------------------------------------------------------------
      IF (GROWTH .GT. 0.0) THEN
        IF (XPODF .EQ. 'PD') THEN
        XPOD = 0.17 * (WSDDTN+WSHDTN)/GROWTH + 0.83 * XPOD
        ELSE IF (XPODF .EQ. 'SD') THEN
        XPOD = 0.17 * (WSDDTN)/GROWTH + 0.83 * XPOD
        ELSE
        CALL ERROR(ERRKEY,1,'      ',0)
        ENDIF
      ENDIF
C-----------------------------------------------------------------------
C    Integration, Add Today's Net Growth to Existing Weights
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C      SJR 5/12/04 reduce VSTAGE for forage model
C    Lower VSTAGE for leaf lost to freezing
C     PDA 5/6/2010  Added FHLEAF for forage harvest 
C-----------------------------------------------------------------------
      IF ((SLDOT + WLDOT) .GE. WTLF) THEN
        VSTAGE=0.0
      ELSE
C       SJR 5/19/04 - drop leaf loss due to senescence - per discussion
C       with KJB - decided that it was a bad idea
!            VSTAGE = ((WTLF-(SLDOT+WLFDOT))/WTLF) * VSTAGE
        VSTAGE = ((WTLF-WLFDOT)/WTLF) * VSTAGE
C--------------------------------------------
C PDA 5/6/2010  ADDED CODE FOR FORAGE HARVEST 
C--------------------------------------------
        IF (FHLEAF.GT.0) VSTAGE=FHVSTG
C--------------------------------------------
      ENDIF
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
      STRWT  = STRWT + WSRDOT


C-----------------------------------------------------------------------
C     Cumulative leaf and stem growth
C-----------------------------------------------------------------------
      CLW = CLW + WLDOTN      
      CSW = CSW + WSDOTN      
      CSRW= CSRW + WSRDOTN

C-----------------------------------------------------------------------
C     Store values of TOPWT at flowering and SDWT at maturity
C-----------------------------------------------------------------------
      IF (YRDOY .EQ. YRNR1) THEN
        CANWAA = TOPWT
      ELSEIF (YRDOY .EQ. MDATE) THEN
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
!      WRCLDT = ALPHL * WLDOTN - CRUSLF - RHOL*(SLNDOT+WLIDOT+WLFDOT)
C      Changed to
!      WRCLDT = ALPHL * WLDOTN - CRUSLF - RHOL*(SLDOT+WLIDOT+WLFDOT)
C      SJR - 6/5/05 When added minimum CH2O concentration after 
C      abscission, replaced the last line listed above with
!      CLOFF = (SLNDOT + WLIDOT +WLFDOT) * RHOL + 
!     &        PCHOLFF*(SLDOT - SLNDOT)
C-----------------------------------------------------------------------
C     7/27/05 SJR Revised to be compatible with NLOFF equation
C      RHOL is yesterday's CH2O concentration - no allowance for CADLF 
C      or NADLF.  
C      CH2O lost = CH2O lost from low-light senescence, water stress, 
C      pest and freeze damage at "current" CH2O concentration (adjusted 
C      for CADLF and NADLF) + Remainder of senesced leaf exclusing NADLF
C      lost at "final" CH2O concentration.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C      8/2/05 SJR - Simplified and revised equation using new variables.
C      Easier to read and understand. 
C-----------------------------------------------------------------------
C      10/06/05 SJR - Revised CxOFF equations for because not refilling 
C      SxMDOT tissues any more. 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C      02/01/06 SJR      Rearrange WRCLDT calculation so easier to follow.
C      Subtract CH2O lost with abscission of natural senescence and tissue 
C      loss due to N mobilization at "final" CH2O concentration.
C      Subtract CH2O lost to low-light and H2O stress at "current" concentration
C      Subtract CH2O lost to pest and freeze damage at "current" concentration
C      Add CH2O refill corrected for the amount lost with senesced tissues other than 
C      those lost to natural senescence and the amount of C refill lost 
C      with pest and freeze damaged tissue.
C-----------------------------------------------------------------------
      CLOFF = (SLMDOT + LTSEN + LFSENWT) *  
     &    (SENCLV * (RHOL - PCHOLFF) + PCHOLFF) 
     &    + (SLNDOT + WLIDOT + WLFDOT) * RHOL
C--------------------------------------------
C PDA 5/6/2010  ADDED CODE FOR FORAGE HARVEST 
C--------------------------------------------
      IF (FHLEAF.GT.0)THEN
        IF (WLDOTN.GT.0)THEN
        CLOFF=CLOFF+(FHLEAF-WLDOTN)*RHOL+WLDOTN*ALPHL
        ELSE
        CLOFF=CLOFF+FHLEAF*RHOL
        ENDIF
!        IF (WTLF .GT. 0)THEN
!          CLOFF=CLOFF
!     &  +(FHLEAF-WLDOTN-CADLF+LFCADDM-(NADLF-LFNADDM)/0.16)*RHOL
!        ELSE
!          CLOFF=CLOFF+(FHLEAF-WLDOTN)*RHOL
!        ENDIF
!        IF (WLDOTN.GT.0)THEN
!          CLOFF = CLOFF+WLDOTN*ALPHL
!        ENDIF
      ENDIF
C--------------------------------------------

      IF (CLOFF. LT. 0.0) THEN
        CLOFF = 0.0
      ENDIF

C--------------------------------------------
C PDA 5/6/2010  ADDED CODE FOR FORAGE HARVEST 
C--------------------------------------------
      IF (FHLEAF.GT.0)THEN
        WRCLDT=-CRUSLF-(CLOFF-WLDOTN*ALPHL)
      ELSE
        WRCLDT=WLDOTN*ALPHL-CRUSLF-CLOFF
      ENDIF

!      IF (WTLF .GT. 0.0) THEN
!        IF (FHLEAF.GT.0)THEN
!          IF (WLDOTN.GT.0)WRCLDT=-CRUSLF-CLOFF
!          CLOFF  = CLOFF+CADLF-LFCADDM
!          WRCLDT = WLDOTN * ALPHL - CRUSLF - CLOFF
!        ELSE
!          WRCLDT = WLDOTN * ALPHL - CRUSLF - CLOFF
!     &     + CADLF - LFCADDM
!        ENDIF
!      ENDIF
C--------------------------------------------

      IF (WTLF .GT. 0.0.AND.FHLEAF.EQ.0) THEN
        WRCLDT = WRCLDT + CADLF - LFCADDM
      ENDIF

!      CLOFF = (SLMDOT + LFSENWT) * PCHOLFF +
!     &        (LTSEN + SLNDOT) * RHOL + 
!     &        (WLIDOT + WLFDOT) * RHOL 
!
!      IF (CLOFF. LT. 0.0) THEN
!        CLOFF = 0.0
!      ENDIF
!
!      WRCLDT = WLDOTN * ALPHL - CRUSLF - CLOFF
!      IF (WTLF .GT. 0.0) THEN
!         WRCLDT = WRCLDT + CADLF - SLCADDOT - LFCADDM
!      ENDIF


C-----------------------------------------------------------------------
C      Original code commented out
C      Appears to be double accounting for C in new C reserves in stem
C      Changed WSDOT to WSDOTN to fix this/be consistent with leaf code
C      E-mailed Dr. Boote, waiting for comment
C-----------------------------------------------------------------------
!      WRCSDT = ALPHS * WSDOT - CRUSST - RHOS * SSNDOT
!      WRCSDT = ALPHS * WSDOTN - CRUSST - RHOS * (SSDOT+WSFDOT+WSIDOT)
!      IF (STMWT .GT. 0.0) THEN
!         WRCSDT = WRCSDT + CADST *
!     &     (1. - MIN(1.0,(SSDOT+WSIDOT+WSFDOT)/STMWT))
!      ENDIF

      CSOFF = (SSMDOT + STLTSEN + STSENWT) * 
     &    (SENCSV * (RHOS - PCHOSTF) + PCHOSTF)
     &    + (SSNDOT + WSIDOT + WSFDOT) * RHOS  

C--------------------------------------------
C PDA 5/6/2010  ADDED CODE FOR FORAGE HARVEST 
C--------------------------------------------
      IF (FHSTEM.GT.0)THEN
        IF (WSDOTN.GT.0)THEN
        CSOFF=CSOFF+(FHSTEM-WSDOTN)*RHOS+WSDOTN*ALPHS
        ELSE
        CSOFF=CSOFF+FHSTEM*RHOS
        ENDIF
      ENDIF

!      IF (FHSTEM.GT.0)THEN
!        IF (STMWT .GT. 0)THEN
!          CSOFF=CSOFF
!     &  +(FHSTEM-WSDOTN-CADST+STCADDM-(NADST-STNADDM)/0.16)*RHOS
!        ELSE
!          CSOFF=CSOFF+(FHSTEM-WSDOTN)*RHOS
!        ENDIF
!        IF (WSDOTN.GT.0)THEN
!          CSOFF = CSOFF+WSDOTN*ALPHS
!        ENDIF
!      ENDIF

C--------------------------------------------

      IF (CSOFF. LT. 0.0) THEN
        CSOFF = 0.0
      ENDIF

C--------------------------------------------
C PDA 5/6/2010  ADDED CODE FOR FORAGE HARVEST 
C--------------------------------------------
      IF (FHSTEM.GT.0)THEN
        WRCSDT=-CRUSST-(CSOFF-WSDOTN*ALPHS)
      ELSE
        WRCSDT=WSDOTN*ALPHS-CRUSST-CSOFF
      ENDIF

!      IF (STMWT .GT. 0.0) THEN
!        IF (FHSTEM.GT.0)THEN
!          CSOFF  = CSOFF+CADST-STCADDM
!          WRCSDT = WSDOTN * ALPHS - CRUSST - CSOFF
!        ELSE
!          WRCSDT = WSDOTN * ALPHS - CRUSST - CSOFF
!     &     + CADST - STCADDM
!        ENDIF
!      ENDIF
C--------------------------------------------

!      WRCSDT = WSDOTN * ALPHS - CRUSST - CSOFF
      IF (STMWT .GT. 0.0.AND.FHSTEM.EQ.0) THEN
        WRCSDT = WRCSDT + CADST - STCADDM
      ENDIF


!      WRCSRDT = ALPHSR * WSRDOT - CRUSSR - RHOSR * (SSRNDOT
!     &              +WSRIDOT + WSRFDOT)

!      WRCSRDT = ALPHSR * WSRDOTN - CRUSSR - RHOSR * (SSRDOT
!     &              +WSRIDOT + WSRFDOT)
!      IF (STRWT .GT. 0.0) THEN
!         WRCSRDT = WRCSRDT + CADSR *
!     &     (1. - MIN(1.0,(SSRDOT+WSRIDOT+WSRFDOT)/STRWT))
!      ENDIF

      CSROFF = SSRMDOT *  
     &    (SENCSRV * (RHOSR - PCHOSRF) + PCHOSRF)
     &    + (SSRNDOT + WSRIDOT) * RHOSR

      IF (CSROFF. LT. 0.0) THEN
        CSROFF = 0.0
      ENDIF

      WRCSRDT = WSRDOTN * ALPHSR - CRUSSR - CSROFF
      IF (STRWT .GT. 0.0) THEN
        WRCSRDT = WRCSRDT + CADSR - SRCADDM
      ENDIF


C-----------------------------------------------------------------------
C      Original code commented out
C      Appears to be double accounting for C in new C reserves in stem
C      Changed WSDOT to WSDOTN to fix this/be consistent with leaf code
C      E-mailed Dr. Boote, waiting for comment
C-----------------------------------------------------------------------
!      WRCRDT = ALPHR * WRDOT - CRUSRT - RHOR * SRDOT
!      WRCRDT = ALPHR * WRDOTN - CRUSRT - RHOR * (SRDOT + WRIDOT)
      CROFF = (SRMDOT + SRNDOT) *  
     &    (SENCRV * (RHOR - PCHORTF) + PCHORTF)
     &    + (SRNDOT + WRIDOT) * RHOR 

      IF (CROFF. LT. 0.0) THEN
        CROFF = 0.0
      ENDIF

      WRCRDT = WRDOTN * ALPHR - CRUSRT - CROFF
      IF (RTWT .GT. 0.0) THEN
        WRCRDT = WRCRDT + CADRT - RTCADDM
      ENDIF
      
      
!      WRCSHD = ALPHSH * WSHDOT - CRUSSH - RHOSH*(WTABRT+WTSHMT+WSHIDT)
      WRCSHD = ALPHSH * WSHDTN - CRUSSH - RHOSH*(WTABRT+WTSHMT+WSHIDT)

C-----------------------------------------------------------------------
C     Update C Storage, Concentrations in Leaves, Stems, Roots, and Shells
C-----------------------------------------------------------------------
      WCRLF = WCRLF + WRCLDT
      WCRST = WCRST + WRCSDT
      WCRRT = WCRRT + WRCRDT
      WCRSH = WCRSH + WRCSHD
      WCRSR = WCRSR + WRCSRDT

      IF (WCRLF .LE. 1.0E-30) WCRLF = 0.0
      IF (WCRST .LE. 1.0E-30) WCRST = 0.0
      IF (WCRRT .LE. 1.0E-30) WCRRT = 0.0
      IF (WCRSH .LE. 1.0E-30) WCRSH = 0.0
      IF (WCRSR .LE. 1.0E-30) WCRSR = 0.0
C-----------------------------------------------------------------------
C     Compute Cumulative C loss During Season
C-----------------------------------------------------------------------
      WTLO  = WTLO  + SLDOT  + WLIDOT + WLFDOT
      WTSO  = WTSO  + SSDOT  + WSIDOT + WSFDOT
      WTRO  = WTRO  + SRDOT  + WRIDOT
      WTSHO = WTSHO + WTABRT + WSHIDT
      WTSDO = WTSDO + SWIDOT
      WTNOO = WTNOO + NDTH
      WTCO  = WTLO  + WTSO   + WTSHO  + WTSDO
      WTSRO  = WTSRO  + SSRDOT  + WSRIDOT + WSRFDOT

!      WRITE(5000,'(I10)') date


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

      IF (STRWT .GT. 0.0001) THEN
        RHOSR =  WCRSR/STRWT
      ELSE
        RHOSR = 0.0
      ENDIF

      IF (RTWT .GT. 0.0001) THEN
        RHOR =  WCRRT/RTWT
      ELSE
        RHOR = 0.0
      ENDIF
!      WRITE(8000,'(2F6.3)') RHOR,WCRRT
      
      IF (SHELWT .GT. 0.0001) THEN
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
C      SJR 7/7/05 some controversy over what N concentration tissues are 
C      senesced at depending on the reason for senescence.  Quick or 
C      stress-related losses should be lost at current N concentration but 
C      slow, normal senescence should be lost at final N concentration.  
C      Existing code uses PCNL to calculate N lost to quick senescence.  
C      This does not account for the increase in N content and concentration 
C      from NADLF added today.  Fixed by calculating a temporary N 
C      concentration from WTNLF, NADLF, and WTLF at the start of the day.
C-----------------------------------------------------------------------
!      NLOFF  = (LTSEN + SLNDOT + WLIDOT + WLFDOT) * (PCNL/100.) +
!     &     (SLDOT-(SLNDOT + LTSEN)) * PROLFF * 0.16
C-----------------------------------------------------------------------
C      02/01/06 SJR      Rearrange NLDOT calculation so easier to follow.
C      Subtract CH2O lost with abscission of natural senescence and tissue 
C      loss due to N mobilization at "final" N concentration.
C      Subtract N lost to low-light and H2O stress at "current" concentration
C      Subtract N lost to pest and freeze damage at "current" concentration
C      Add N refill corrected for the amount lost with senesced tissues other than 
C      those lost to natural senescence and the amount of N refill lost 
C      with pest and freeze damaged tissue.
C-----------------------------------------------------------------------
!      NLOFF  = (SLMDOT + LFSENWT) * PROLFF * 0.16 +
!     &        (LTSEN + SLNDOT) * PCNL/100 + 
!     &        (WLIDOT + WLFDOT) * PCNL/100  
!      
!      IF (NLOFF. LT. 0.0) THEN
!        NLOFF = 0.0
!      ENDIF


C-----------------------------------------------------------------------
C     Net growth rate of nitrogen in the leaves
C-----------------------------------------------------------------------
!      NLDOT = NGRLF - NRUSLF - NLOFF
!      IF (WTLF .GT. 0.0) THEN
!         NLDOT = NLDOT + NADLF - SLNADDOT - LFNADDM
!      ENDIF



      NLOFF  = SLMDOT * 
     &    (SENNLV * (PCNL/100 - PROLFF * 0.16) + PROLFF * 0.16) 
     &    + (LTSEN + LFSENWT) * PROLFF *0.16
     &    + (SLNDOT + WLIDOT + WLFDOT) * PCNL/100  
      
C--------------------------------------------
C PDA 5/6/2010  ADDED CODE FOR FORAGE HARVEST 
C--------------------------------------------
      IF (FHLEAF.GT.0)THEN
        IF (WLDOTN.GT.0)THEN
        NLOFF=NLOFF+(FHLEAF-WLDOTN)*PCNL/100+NGRLF
        ELSE
        NLOFF=NLOFF+FHLEAF*PCNL/100
        ENDIF
      ENDIF
!      IF (FHLEAF.GT.0)THEN
!        IF (WTLF .GT. 0)THEN
!          NLOFF=NLOFF
!     &     +(FHLEAF-WLDOTN-CADLF+LFCADDM-(NADLF-LFNADDM)/0.16)*PCNL/100
!        ELSE
!          NLOFF=NLOFF+(FHLEAF-WLDOTN)*PCNL/100
!        ENDIF
!        IF (WLDOTN.GT.0)THEN
!          NLOFF = NLOFF+NGRLF
!        ENDIF
!      ENDIF
C--------------------------------------------

      IF (NLOFF. LT. 0.0) THEN
        NLOFF = 0.0
      ENDIF


C-----------------------------------------------------------------------
C     Net growth rate of nitrogen in the leaves
C-----------------------------------------------------------------------
!      NLDOT = NGRLF - NRUSLF - NLOFF
C--------------------------------------------
C PDA 5/6/2010  ADDED CODE FOR FORAGE HARVEST 
C--------------------------------------------
      IF (FHLEAF.GT.0)THEN
        NLDOT=-NRUSLF-(NLOFF-NGRLF)
      ELSE
        NLDOT=NGRLF-NRUSLF-NLOFF
      ENDIF
!      IF (WTLF .GT. 0.0) THEN
!        IF (FHLEAF.GT.0)THEN
!          NLOFF = NLOFF + NADLF - LFNADDM
!          NLDOT = NGRLF - NRUSLF - NLOFF
!        ELSE
!          NLDOT = NGRLF - NRUSLF - NLOFF + NADLF - LFNADDM
!        ENDIF
!      ENDIF
C--------------------------------------------
      IF (WTLF .GT. 0.0.AND.FHLEAF.EQ.0) THEN
        NLDOT = NLDOT + NADLF - LFNADDM
      ENDIF



C-----------------------------------------------------------------------
C     Stem nitrogen senescence and pest damage loss
C-----------------------------------------------------------------------
 !     NSOFF  = (LTSEN * PORPT + SSNDOT + WSIDOT + WSFDOT) * EODSTN +
 !    &   (SSMDOT + - SSNDOT - LTSEN * PORPT - SSNADDOT / 0.16) * 
 !    &     PROSTF * 0.16 

      NSOFF  = SSMDOT * 
     &    (SENNSV * (PCNST/100 - PROSTF * 0.16) + PROSTF * 0.16) 
     &    + (STLTSEN + STSENWT) * PROSTF *0.16
     &    + (SSNDOT + WSIDOT + WSFDOT) * PCNST/100  
      

      IF (NSOFF. LT. 0.0) THEN
        NSOFF = 0.0
      ENDIF


C-----------------------------------------------------------------------
C     Net growth rate of nitrogen in the stem
C-----------------------------------------------------------------------
!      NSDOT = NGRST - NRUSST - NSOFF
C--------------------------------------------
C PDA 5/6/2010  ADDED CODE FOR FORAGE HARVEST 
C--------------------------------------------
      IF (FHSTEM.GT.0)THEN
        NSDOT=-NRUSST-(NSOFF-NGRST)
      ELSE
        NSDOT=NGRST-NRUSST-NSOFF
      ENDIF
!      IF (FHSTEM.GT.0)THEN
!        IF (STMWT .GT. 0)THEN
!          NSOFF=NSOFF
!     &     +(FHSTEM-WSDOTN-CADST+STCADDM-(NADST-STNADDM)/0.16)*PCNST/100
!        ELSE
!          NSOFF=NSOFF+(FHSTEM-WSDOTN)*PCNST/100
!        ENDIF
!        IF (WSDOTN.GT.0)THEN
!          NSOFF = NSOFF+NGRST
!        ENDIF
!      ENDIF

C--------------------------------------------

      IF (STMWT .GT. 0.0.AND.FHSTEM.EQ.0) THEN
        NSDOT = NSDOT + NADST - STNADDM
      ENDIF

C--------------------------------------------
C PDA 5/6/2010  ADDED CODE FOR FORAGE HARVEST 
C--------------------------------------------
!      IF (STMWT .GT. 0.0) THEN
!        IF (FHSTEM.GT.0)THEN
!         NSOFF = NSOFF + NADST - STNADDM
!         NSDOT = NGRST - NRUSST - NSOFF
!        ELSE
!         NSDOT = NGRST - NRUSST - NSOFF + NADST - STNADDM
!        ENDIF
!      ENDIF
C--------------------------------------------

C-----------------------------------------------------------------------
C     Storage tissue nitrogen senescence and pest damage loss
C      Using different strategy - Stolon not abscised like leaf 
C      therefore, are assuming senesced tissue lost at average 
C      N concentration
C-----------------------------------------------------------------------
!      NSROFF  = (SSRNDOT + WSRIDOT + WSRFDOT) * (PCNSR/100.) +
!     &     (SSRDOT-SSRNDOT) * PROSRF * 0.16


      NSROFF  = SSRMDOT  * 
     &  (SENNSRV * (PCNSR/100 - PROSRF * 0.16) + PROSRF * 0.16) 
     &  + (SSRNDOT + WSRIDOT + WSRFDOT) * PCNSR/100  
      
      IF (NSROFF. LT. 0.0) THEN
        NSROFF = 0.0
      ENDIF


C-----------------------------------------------------------------------
C     Net growth rate of nitrogen in the STOR
C-----------------------------------------------------------------------
      NSRDOT = NGRSR - NRUSSR - NSROFF
      IF (STRWT .GT. 0.0) THEN
        NSRDOT = NSRDOT + NADSR - SRNADDM
      ENDIF




C-----------------------------------------------------------------------
C     Root nitrogen senescence and pest damage loss
C-----------------------------------------------------------------------
      NROFF  = SRMDOT  * 
     &    (SENNRV * (PCNRT/100 - PRORTF * 0.16) + PRORTF * 0.16) 
     &    + (SRNDOT + WRIDOT) * PCNRT/100  
      
      IF (NROFF. LT. 0.0) THEN
        NROFF = 0.0
      ENDIF


C-----------------------------------------------------------------------
C     Net growth rate of nitrogen in the roots
C-----------------------------------------------------------------------
      NRDOT = NGRRT - NRUSRT - NROFF
      IF (RTWT .GT. 0.0) THEN
        NRDOT = NRDOT + NADRT - RTNADDM
      ENDIF



C-----------------------------------------------------------------------
C     Shell nitrogen senescence, abortion and pest damage loss
C-----------------------------------------------------------------------
      NSHOFF = (WTABRT+WSHIDT) * (PCNSH/100.)
      IF (NSHOFF .LT. 0.0) THEN
        NSHOFF = 0.0
      ENDIF

C-----------------------------------------------------------------------
C     Net growth rate of nitrogen in shells
C-----------------------------------------------------------------------
      NSHDOT = NGRSH - NSHOFF - NRUSSH
      IF (SHELWT .GT. 0.0) THEN
        NSHDOT = NSHDOT + NADSH * 
     &    (1. - MIN(1.0,(WTABRT+WSHIDT) / SHELWT))
      ENDIF


C-----------------------------------------------------------------------
C     Seed nitrogen senescence, abortion and pest damage loss
C-----------------------------------------------------------------------
      NSDOFF = SWIDOT * PCNSD/100.
      IF (NSDOFF .LT. 0.0) THEN
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
      IF ((NLDOT .LT. 0.0) .AND. (ABS(NLDOT) .GT. WTNLF)) THEN
        NLDOT = - WTNLF
      ENDIF
      WTNLF = WTNLF + NLDOT

C-----------------------------------------------------------------------
C     Total nitrogen in the stems
C-----------------------------------------------------------------------     
      IF ((NSDOT .LT. 0.0) .AND. (ABS(NSDOT) .GT. WTNST)) THEN
        NSDOT = - WTNST
      ENDIF
      WTNST = WTNST + NSDOT

C-----------------------------------------------------------------------
C     Total nitrogen in the storage tissues
C-----------------------------------------------------------------------
      IF ((NSRDOT .LT. 0.0) .AND. (ABS(NSRDOT) .GT. WTNSR)) THEN
        NSRDOT = - WTNSR
      ENDIF
      WTNSR = WTNSR + NSRDOT


C-----------------------------------------------------------------------
C     Total nitrogen in the roots
C-----------------------------------------------------------------------
      IF ((NRDOT .LT. 0.0) .AND. (ABS(NRDOT) .GT. WTNRT)) THEN
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
      IF ((NSHDOT .LT. 0.0) .AND. (ABS(NSHDOT) .GT. WTNSH)) THEN
        NSHDOT = - WTNSH
      ENDIF
      WTNSH = WTNSH + NSHDOT

      IF ((NSDDOT .LT. 0.0) .AND. (ABS(NSDDOT) .GT. WTNSD)) THEN
        NSDDOT = - WTNSD
      ENDIF

C-----------------------------------------------------------------------
C     Total nitrogen in the seed
C-----------------------------------------------------------------------
      WTNSD = WTNSD + NSDDOT

C-----------------------------------------------------------------------
C     Total nitrogen in the plant
C-----------------------------------------------------------------------
      WTNTOT = WTNLF + WTNST + WTNSR + WTNRT + WTNSH + WTNSD + WTNNOD

C-----------------------------------------------------------------------
C     Total nitrogen in the canopy (all above ground components)
C-----------------------------------------------------------------------
      WTNCAN = WTNLF + WTNST + WTNSH + WTNSD

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
        NLALL = NLALL + NADLF - SLNADDOT - LFNADDM
      ENDIF

!     Stem
      NSALL = NGRST - NRUSST 
      IF (STMWT .GT. 0.0) THEN
        NSALL = NSALL + NADST - SSNADDOT - STNADDM
      ENDIF

!     Storage tissues
      NSRALL = NGRSR - NRUSSR 
      IF (STRWT .GT. 0.0) THEN
        NSRALL = NSRALL + NADSR - SSRNADDOT - SRNADDM
      ENDIF

!     Root
      NRALL = NGRRT - NRUSRT
      IF (RTWT .GT. 0.0) THEN
        NRALL = NRALL + NADRT - SRNADDOT - RTNADDM
      ENDIF

!     Shell and seed
      NSHALL = NGRSH - NRUSSH
C-----------------------------------------------------------------------
C      Added these lines after adding NADSH variable
C-----------------------------------------------------------------------

      IF (SHELWT .GT. 0.0) THEN
        NSHALL = NSHALL + NADSH * 
     &    (1. - MIN(1.0,(WTABRT+WSHIDT) / SHELWT))
      ENDIF

      NSDALL = NGRSD

      WTNLA = WTNLA + NLALL
      WTNSA = WTNSA + NSALL
      WTNRA = WTNRA + NRALL
      WTNSHA = WTNSHA + NSHALL
      WTNSDA = WTNSDA + NSDALL
      WTNSRA = WTNSRA + NSRALL
C-----------------------------------------------------------------------
C    Nodules
C-----------------------------------------------------------------------
      IF (ISWNIT .EQ. 'Y' .AND. ISWSYM .EQ. 'Y') THEN
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
      WTNSRO = WTNSRO + NSROFF
      IF (ISWNIT .EQ. 'Y' .AND. ISWSYM .EQ. 'Y') THEN
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


      IF ((STRWT .GT. 0.0) .AND. (STRWT .GT. WTNSR) .AND. 
     &   (WTNSR .GT. 0.0)) THEN
        PCNSR = WTNSR / STRWT * 100.0
      ELSE
        PCNSR = 0.0
      ENDIF



      IF ((RTWT .GT. 0.0) .AND. (WTNRT .GT. 0.0)) THEN
        PCNRT = WTNRT / RTWT * 100.0
      ELSE
        PCNRT = 0.0
      ENDIF

      IF ((SHELWT .GT. 0.0) .AND. (WTNSH .GT. 0.0)) THEN
        PCNSH = WTNSH / SHELWT * 100.0
      ELSE
        PCNSH = 0.0
      ENDIF
C-----------------------------------------------------------------------
C     Compute Percentage Seed Composition
C-----------------------------------------------------------------------
      IF (SDWT .GT. 0.01) THEN
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
      IF ((WTLF - WCRLF) .GT. 0.0) THEN
        RNITP = 100.*WTNLF/(WTLF-TNCFAC*WCRLF)
!-----------------------------------------------------------------------
!  PDA 5/26/2010  ADDED CODE TO ALLOW LEAF CH2O TO AFFECT PHOTOSYNTHESIS
!      IF (WTLF .GT. 0.0.AND.(WTLF - WCRLF) .GT. 0.0) THEN
!         IF (WCRLF/WTLF.GT.ALPHL) THEN
!           RNITP = 100*WTNLF/(WTLF-ALPHL/(1-ALPHL)*(WTLF-WCRLF))      
!         ELSE
!           RNITP = 100*WTNLF/(WTLF-WCRLF)
!         ENDIF
!-----------------------------------------------------------------------
      ELSE
        RNITP = PCNMIN
      ENDIF
C-----------------------------------------------------------------------
C     Calculate Remaining N in Shells, Leaves, Stems, and Roots
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

      IF ((STRWT - WCRSR) .GT. 0.0) THEN
        WNRSR = MAX (WTNSR - PROSRF * 0.16 * (STRWT-WCRSR), 0.0)
      ELSE
        WNRSR = 0.0
      ENDIF


      IF (RTWT .GT. 0.) THEN
        WNRRT = MAX (WTNRT - PRORTF * 0.16 * RTWT, 0.0)
      ELSE
        WNRRT = 0.0
      ENDIF

      IF (SHELWT .GT. 0.0) THEN
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
C      leaf (SLDOT, WLFDOT), stem (SSDOT), storage organ (SSRDOT, WSRFDOT)
C       and shell (WTABRT). At this time, the model does not senesce seed.   
C     Convert from biomass to C with a factor 0.40.   
C      NOTE storage organ senescence is apportioned to both surface layer 
C      and soil layer1 according to proportions PSRSRFD and PSRLYRD from FREEZE
C      Also replaced PCNx with EODxxN.  PCNx is yesterday's ending N 
C      concentration and doesn't include NADLF and CADLF from today.
C-----------------------------------------------------------------------
!        LFNADFDM = LFNADDM * WLFDOT / (WLIDOT + WLFDOT)
!        STNADFDM = STNADDM * WSFDOT / (WSIDOT + WSFDOT) 
!        SRNADFDM = SRNADDM * WSRFDOT / (WSRIDOT + WSRFDOT)
!      SENCLN(0,1) = (SLDOT + WLFDOT + SSDOT + WTABRT) * 0.40
      SenWt(0) = (SLDOT + WLFDOT + SSDOT + WSFDOT + WTABRT + 
     &    SSRDOT *PSRSRFL) + WSRFDOT * PSRSRFD
C
C     Convert from g/m2 to kg/ha with a factor of 10.
!      SENCLN(0,1) = AMAX1(SENCLN(0,1), 0.) * 10.0     !kg[C]/ha
      SenWt(0) = AMAX1(SenWt(0), 0.) * 10.0    !kg[dry matter]/ha
      
C-----------------------------------------------------------------------
C     Surface nitrogen includes nitrogen losses computed above minus the
C       pest damage components. 
C-----------------------------------------------------------------------
!      SENCLN(0,3) = (NLOFF - WLIDOT * PCNL / 100.) +     
!      SenE(0,1) = (NLOFF - WLIDOT * PCNL  / 100.) +                  !Leaf
!     &        (NSOFF - WSIDOT * PCNST / 100.) +                  !Stem
!     &        (NSHOFF- WSHIDT * PCNSH / 100.) +                  !Shell
!     &              (NSROFF- (WSRIDOT + WSRFDOT) * PCNSR / 100.*PSRSRFL)
!     &              + WSRFDOT * PCNSR / 100 * PSRSRFD                  !Storage

      SenE(0,1) = (NLOFF - WLIDOT * EODLFN) +                  !Leaf
     &    (NSOFF - WSIDOT * EODSTN) +                  !Stem
     &    (NSHOFF- WSHIDT * PCNSH / 100.) +                  !Shell
     &    (NSROFF- (WSRIDOT + WSRFDOT) * EODSRN * PSRSRFL)
     &    + WSRFDOT * EODSRN * PSRSRFD                  !Storage

C     Convert from g/m2 to kg/ha with a factor of 10.
!      SENCLN(0,3) = AMAX1 (SENCLN(0,3),0.) * 10.0        !kg[N]/ha
      SenE(0,1) = AMAX1 (SenE(0,1),0.) * 10.0             !kg[N]/ha

C-----------------------------------------------------------------------
C     Contribution of lignin to surface litter from senesced and frozen 
C       plant matter
C-----------------------------------------------------------------------
!      SENCLN(0,2) = (SLDOT + WLFDOT) * PLIGLF + SSDOT * PLIGST +
      SenLig(0) = (SLDOT + WLFDOT) * PLIGLF + (SSDOT+WSFDOT) * PLIGST +
     &    WTABRT * PLIGSH + (SSRDOT*PSRSRFL + WSRFDOT*PSRSRFD)* PLIGSR 
C     Convert from g/m2 to kg/ha with a factor of 10.
!      SENCLN(0,2) = AMAX1 (SENCLN(0,2),0.) * 10.0        !kg[lig]/ha
      SenLig(0) = AMAX1 (SenLig(0),0.) * 10.0             !kg[lig]/ha

C-----------------------------------------------------------------------
C     Senescence of roots, nodules, and subsurface storage organs (kg/ha)
C-----------------------------------------------------------------------
      DO L = 1, NLAYR 
      IF (L .EQ. 1) THEN 
      !       SENCLN(L,1) = (SENRT(L) + SENNOD(L)) * 0.40   !kg[C]/ha
        SenWt(L)  = SENRT(L) + SENNOD(L)+ 
     &    (SSRDOT*PSRLYR1 + WSRFDOT*PSRLYRD)    !kg[dry matter]/ha
!       SENCLN(L,2) = SENRT(L) * PLIGRT + SENNOD(L) * PLIGNO !kg[lig]/ha
        SenLig(L) = SENRT(L) * PLIGRT + SENNOD(L) * PLIGNO +
     &    (SSRDOT*PSRLYR1 + WSRFDOT*PSRLYRD)* PLIGSR       !kg[lig]/ha
!       SENCLN(L,3) = (SENRT(L)* PRORTF + SENNOD(L) * PRONOD) * 0.16 
        SenE(L,1) = (SENRT(L)* PRORTF + SENNOD(L) * PRONOD) * 0.16 +
     &    (NSROFF- (WSRIDOT + WSRFDOT) * EODSRN * PSRLYR1)
     &    + WSRFDOT * EODSRN * PSRLYRD
      ELSE
!       SENCLN(L,1) = (SENRT(L) + SENNOD(L)) * 0.40   !kg[C]/ha
        SenWt(L)  = (SENRT(L) + SENNOD(L))            !kg[dry matter]/ha
!       SENCLN(L,2) = SENRT(L) * PLIGRT + SENNOD(L) * PLIGNO !kg[lig]/ha
        SenLig(L) = SENRT(L) * PLIGRT + SENNOD(L) * PLIGNO   !kg[lig]/ha
!       SENCLN(L,3) = (SENRT(L)* PRORTF + SENNOD(L) * PRONOD) * 0.16 
        SenE(L,1) = (SENRT(L)* PRORTF + SENNOD(L) * PRONOD) * 0.16 
      ENDIF
      ENDDO

C-----------------------------------------------------------------------
C     Update distribution of storage tissue above and below soil surface
C-----------------------------------------------------------------------
      PSRLYR1 = TPSRLYR1
      PSRSRFL = TPSRSRFL 

C-----------------------------------------------------------------------
C     Soil nitrogen includes nitrogen losses computed above minus the
C       pest damage components of root and nodule. 
C-----------------------------------------------------------------------
      !SENCLN(1,3) = NROFF - WRIDOT * EODRTN. + NNOFF
      !SENCLN(1,3) = AMAX1 (SENCLN(1,3), 0.) * 10.0    !kg[N]/ha
C-----------------------------------------------------------------------
C     Contribution of lignin to soil from senesced roots and nodules
C-----------------------------------------------------------------------
      !SENCLN(1,2) = (SRDOT * PLIGRT + NDTH * PLIGNO) * 10.0
        !kg[lig]/ha
C-----------------------------------------------------------------------
C     Leaf Area, Specific Leaf Area Calculations
C     Calculate Area Growth rate and Update Leaf Area (AREALF, XLAI)
C-----------------------------------------------------------------------
C      SJR - 6/17/05 Per e-mails with KJB - the amount of protein lost to 
C            leaf N mobilization and refill should not affect leaf area.  
C            Fix by not adding NRUSLF/0.16 to senescence and pest losses.  
C            This prevents N mobilization from affecting SLA. Next, eliminate  
C            the second line adding NADLF.  This still allows SENWT (skeleton 
C            lost with mobilized N) to reduce leaf area.
C      Original Code
!      ALFDOT = WLDOTN*F - (SLDOT+WLIDOT+WLFDOT+NRUSLF/0.16)*SLA
!      IF (WTLF .GT. 0.0) THEN
!         ALFDOT = ALFDOT + SLA * (NADLF/0.16) *
!     &   (1. - MIN(1.0,(SLDOT+WLIDOT+WLFDOT)/WTLF))
!      ENDIF
C      Revised into one line
C-----------------------------------------------------------------------

!      ALFDOT = WLDOTN * F - (SLDOT - SLNADDOT + WLIDOT + WLFDOT) * SLA

C--------------------------------------------
C PDA 5/6/2010  ADDED CODE FOR FORAGE HARVEST 
C--------------------------------------------
      IF (FHLEAF.GT.0)THEN
        ALFDOT = - (SLDOT + WLIDOT + WLFDOT) * SLA
     &  -(FHLEAF-WLDOTN)*SLA
      ELSE
        ALFDOT = WLDOTN * F - (SLDOT - SLNADDOT + WLIDOT + WLFDOT) * SLA
      ENDIF
C--------------------------------------------

      AREALF = AREALF + ALFDOT

C-----------------------------------------------------------------------
C     New line for Forage model to prevent negative LAI's in winter
C-----------------------------------------------------------------------

      AREALF = MAX(AREALF, 0.0)

      XLAI   = AREALF / 10000.
      IF (AREALF .GT. 0.00001 .AND. WTLF .GT. 0.0) THEN
        SLA    = AREALF / WTLF
        SLAAD  = AREALF / (WTLF - WCRLF)
        IF (SLA .GT. 999.) SLA = -99.
        IF (SLAAD .GT. 999.) SLAAD = -99.
      ENDIF
C-----------------------------------------------------------------------
C     Remember XLAI
C-----------------------------------------------------------------------
      LAIMX = MAX(XLAI,LAIMX)
C-----------------------------------------------------------------------
C     Calculate "Healthy" or Non-Diseased Leaf Area Index
C-----------------------------------------------------------------------
!      AREAH  = AREALF - 2. * DISLA
      AREAH  = AREALF - DISLA !KJB correction beta factor covers this
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
      IF (STRWT .LT. 0.00001) THEN
       IF (TOPWT .LT. 0.00001 .OR. STMWT .LT. 0.00001) THEN
        CALL FOR_STRESS(
     &    AGEFAC, DWNOD, IDETO, IHARI, NOUTDO, PODWT,     !Input
     &    RTWT, SDWT, SHELWT, STMWT, TOPWT,               !Input
     &    TOTWT, TURFAC, WTLF, YRDOY, YRPLT,              !Input
     &    MDATE, CropStatus,                              !Output
     &  STRWT)                                                                  !Input
        RETURN
       ENDIF

       IF (IHARI .NE. 'R' .AND. IHARI .NE. 'D') THEN
        IF (RTWT  .LT. 0.00001 .OR. WTLF  .LT. 0.00001) THEN
        CALL FOR_STRESS( 
     &  AGEFAC, DWNOD, IDETO, IHARI, NOUTDO, PODWT,   !Input
     &  RTWT, SDWT, SHELWT, STMWT, TOPWT,             !Input
     &  TOTWT, TURFAC, WTLF, YRDOY, YRPLT,            !Input
     &  MDATE, CropStatus,                            !Output
     &  STRWT)                                                                  !Input
        RETURN
        ENDIF
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
      END  ! SUBROUTINE FOR_GROW
C=======================================================================

C=======================================================================
C  FOR_STRESS, Subroutine, from code in FOR_GROW subroutine
C-----------------------------------------------------------------------
C  Plant death due to stress
C-----------------------------------------------------------------------
C  REVISION        HISTORY
C  09/18/1998 CHP  Written based on code in FOR_GROW subroutine
!  06/15/2022 CHP Added CropStatus
!-----------------------------------------------------------------------
!  Called by:  FOR_GROW
!  Calls:      None
C=======================================================================
      SUBROUTINE FOR_STRESS(
     &  AGEFAC, DWNOD, IDETO, IHARI, NOUTDO, PODWT,       !Input
     &  RTWT, SDWT, SHELWT, STMWT, TOPWT,                 !Input
     &  TOTWT, TURFAC, WTLF, YRDOY, YRPLT,                !Input
     &  MDATE, CropStatus,                                !Output
     &  STRWT)                                            !Input

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
      REAL STRWT
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
      STRWT  = MAX(0.,STRWT)

      IF (MDATE .LT. 0) THEN
!        NR8   = MAX(0,TIMDIF(YRSIM,YRDOY))
        MDATE = YRDOY
        CropStatus = 39
      ENDIF
C-----------------------------------------------------------------------
      IF (IHARI .EQ. 'M') THEN
        DAP   = MAX(0,TIMDIF(YRPLT,YRDOY))

        !Message to WARNING.OUT
        CALL YR_DOY(YRDOY, YR, DOY)
        WRITE(MESSAGE(1), 100) DAP
        WRITE(MESSAGE(2), 110) YR, DOY, TURFAC, AGEFAC
        CALL WARNING(2,'CRPGRO', MESSAGE)
100   FORMAT(' Plant died due to extreme stress at ',I3,
     &    ' days after planting.')
110   FORMAT(' (DAY : ',I4,1X,I3,'; TURFAC : ',F5.2,'; AGEFAC : ',
     &    F5.2,'  )')

        !Message to screen
!        WRITE (*,275) MESSAGE(1), MESSAGE(2)
275   FORMAT(/,10X,A78,/,10X,A78,/)

        !Message to Overview.out
        IF (IDETO .EQ. 'Y') THEN
        WRITE (NOUTDO,275) MESSAGE(1), MESSAGE(2)
        ENDIF
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END  ! SUBROUTINE FOR_STRESS
C=======================================================================

C=======================================================================
C  FOR_IPGROW, Subroutine, C. H. Porter
C-----------------------------------------------------------------------
C  Reads input data for FOR_GROW routines.
C----------------------------------------------------------------------
C  REVISION HISTORY
C  09/15/98 CHP Written
C-----------------------------------------------------------------------
C  Called : FOR_GROW
C  Calls  : FIND, ERROR, IGNORE
C=======================================================================
      SUBROUTINE FOR_IPGROW(
     &  FILEIO, FILECC, FILEGC, CROP,                           !Input
     &  ALPHL, ALPHR, ALPHS, ALPHSH, TNCFAC,                    !Output
     &  PCARLF, PCARST, PCARRT, PCARSH, PCARSD, PCARNO,         !Output
     &  PLIGLF, PLIGST, PLIGRT, PLIGSH, PLIGSD, PLIGNO,         !Output
     &  PLIPLF, PLIPST, PLIPRT, PLIPSH, PLIPNO, !Output
     &  PMINLF, PMINST, PMINRT, PMINSH, PMINSD, PMINNO,         !Output
     &  POALF, POAST, POART, POASH, POASD, POANO,          !Output
     &  PROLFF, PROSTF, PRORTF, PROSHF, PRONOD, !Output
     &  PROLFI, PROSTI, PRORTI,                                 !Output
     &  PLTPOP, ROWSPC, RMIN, PLME, SDWTPL,                 !Output
     &  SENCLV, SENCRV, SENCSV, SENCSRV,                   !Output
     &  SENNLV, SENNRV, SENNSV, SENNSRV,                   !Output           
     &  SDLIP, SDPRO, WTFSD, WTPSD, XPODF,                 !Output
     &  ALPHSR, PCARSR, PCHOLFF, PCHORTF, PCHOSRF, PCHOSTF,!Output
     &  PLIGSR, PLIPSR, PMINSR, POASR, PROSRF, PROSRI)     !Output
!-----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, ERROR, IGNORE, UPCASE
!-----------------------------------------------------------------------
      CHARACTER*1 PLME, UPCASE
      CHARACTER*2 XPODF, CROP
      CHARACTER*6 ERRKEY
      PARAMETER (ERRKEY = 'IPGROW')
      CHARACTER*6 SECTION, ECONO, ECOTYP
      CHARACTER*30 FILEIO
      CHARACTER*80 C80
      CHARACTER*92 FILECC, FILEGC
      CHARACTER*255 C255

      INTEGER LUNCRP, LUNECO, LUNIO, I
      INTEGER ERR, LNUM, FOUND, ISECT

      REAL ROWSPC, RMIN, PLTPOP, WTFSD, WTPSD,
     &  SDPRO, SDLIP, SDWTPL,
     &  ALPHL, ALPHS, ALPHR, ALPHSH, TNCFAC,
     &  PROLFF, PROSTF, PRORTF, PROSHF, PRONOD,
     &  PROLFI, PROSTI, PRORTI,
     &  PCARLF, PCARST, PCARRT, PCARSH, PCARSD, PCARNO,
     &  PLIPLF, PLIPST, PLIPRT, PLIPSH, PLIPNO,
     &  PLIGLF, PLIGST, PLIGRT, PLIGSH, PLIGSD, PLIGNO,
     &  POALF, POAST, POART, POASH, POASD, POANO,
     &  PMINLF, PMINST, PMINRT, PMINSH, PMINSD, PMINNO

      REAL ALPHSR, PCARSR, PCHOLFF, PCHORTF, PCHOSRF, PCHOSTF,             
     &     PLIGSR, PLIPSR, PMINSR, POASR, PROSRF, PROSRI

C-----------------------------------------------------------------------
C      New parameters for variable N  and CHO at senescence 
C-----------------------------------------------------------------------
      REAL SENCLV, SENCRV, SENCSV, SENCSRV
      REAL SENNLV, SENNRV, SENNSV, SENNSRV
      
!-----------------------------------------------------------------------
      CALL GETLUN('FILEIO', LUNIO)
      OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)

!C-----------------------------------------------------------------------
!C    Find and read Cultivar Section
!C-----------------------------------------------------------------------
      SECTION = '*CULTI'
      CALL FIND(LUNIO, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILEIO, LNUM)
      ELSE
        READ (LUNIO,'(3X,A2)') CROP
      ENDIF

      IF (CROP .NE. 'FA') THEN
!-----------------------------------------------------------------------
!    Read Planting Details Section
!-----------------------------------------------------------------------
        SECTION = '*PLANT'
        CALL FIND(LUNIO, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(ERRKEY, 1, FILEIO, LNUM)
        ELSE
C-GH      READ(LUNIO,'(24X,F6.1,5X,A1,6X,F6.0,12X,F6.0)')
          READ(LUNIO,'(24X,F6.0,5X,A1,6X,F6.0,12X,F6.0)')
     &    PLTPOP, PLME, ROWSPC, SDWTPL
        ENDIF

!-----------------------------------------------------------------------
!    Read Cultivar Section
!-----------------------------------------------------------------------
        SECTION = '*CULTI'
        CALL FIND(LUNIO, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILEIO, LNUM)
        ELSE
        READ(LUNIO,'(24X,A6,66X,F6.0)') ECONO, WTPSD
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
!-----------------------------------------------------------------------
!    Find and Read Respiration Section
!-----------------------------------------------------------------------
!     Subroutine FIND finds appropriate SECTION in a file by
!     searching for the specified 6-character string at beginning
!     of each line.
!-----------------------------------------------------------------------
        LNUM = 1
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
        CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
        ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(F6.0,6X,2F6.0,6X,F6.0)',IOSTAT=ERR)
     &    PROLFI, PROLFF, PROSTI, PROSTF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(F6.0,6X,F6.0,12X,F6.0)',IOSTAT=ERR)
     &    PRORTI, PRORTF, PROSHF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(12X,F6.0)',IOSTAT=ERR) PRONOD
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(6F6.0)',IOSTAT=ERR)
     &    PCARLF, PCARST, PCARRT, PCARSH, PCARSD, PCARNO
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(5F6.0)',IOSTAT=ERR)
     &    PLIPLF, PLIPST, PLIPRT, PLIPSH, PLIPNO
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(6F6.0)',IOSTAT=ERR)
     &    PLIGLF, PLIGST, PLIGRT, PLIGSH, PLIGSD, PLIGNO
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(6F6.0)',IOSTAT=ERR)
     &    POALF, POAST, POART, POASH, POASD, POANO
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(6F6.0)',IOSTAT=ERR)
     &    PMINLF, PMINST, PMINRT, PMINSH, PMINSD, PMINNO
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(F6.0, 6X, F6.0)',IOSTAT=ERR)
     &    PROSRI, PROSRF 
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(5F6.0)',IOSTAT=ERR)
     &    PCARSR, PLIPSR, PLIGSR, POASR, PMINSR
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4F6.0)',IOSTAT=ERR)
     &    PCHOLFF, PCHOSTF, PCHORTF, PCHOSRF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)


        ENDIF

        SECTION = '!*CARB'
        CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
        ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4X,A2)',IOSTAT=ERR) XPODF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(6F6.0)',IOSTAT=ERR) 
     &    ALPHL, ALPHS, ALPHR, ALPHSH, ALPHSR, TNCFAC
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4F6.0)',IOSTAT=ERR) 
     &    SENNLV, SENCLV, SENNSV, SENCSV
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4F6.0)',IOSTAT=ERR) 
     &    SENNRV, SENCRV, SENNSRV, SENCSRV 
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDIF

        DO I = 1,2
        XPODF(I:I) = UPCASE(XPODF(I:I))
        ENDDO

!-----------------------------------------------------------------------
        SECTION = '!*VEGE'
        CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
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

        ISECT = 2
        DO I=1,200
        CALL IGNORE(LUNECO, LNUM, ISECT, C255)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,0)
        IF ((ISECT .EQ. 1) .AND. (C255(1:1) .NE. ' ') .AND.
     &    (C255(1:1) .NE. '*')) THEN
        READ (C255,'(A6,108X,2F6.0)',IOSTAT=ERR)
     &    ECOTYP, SDPRO, SDLIP
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)
        IF (ECOTYP .EQ. ECONO) THEN
        EXIT
        ENDIF
        ELSE IF (ISECT .EQ. 0) THEN
        IF (ECONO .EQ. 'DFAULT') CALL ERROR(ERRKEY,3,FILEGC,LNUM)
        ECONO = 'DFAULT'
        REWIND(LUNECO)
        ENDIF
        ENDDO
  
        CLOSE (LUNECO)
  
      ENDIF


!-----------------------------------------------------------------------
      RETURN
      END  ! SUBROUTINE FOR_IPGROW
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
! ALPHSR   Fraction of new storage organ growth that is mobile C (fraction)
! AREAH    Area of healthy leaves (cm2[leaf] / m2[ground])
! AREALF   Area of leaves (one side) per unit ground area
!            (cm2[leaf] / m2[ground])
! BETN     Spacing between plants in a row (m / plant)
! CADLF    Mass of CH2O added to leaf reserves after growth
!            (g[CH2O] / m2 / d)
! CADRT      Mass of CH2O added back to roots for CH2O cost of excess mobilized N 
!            (never really mobilized) (g[CH2O] / m2 / d)
! CADSH      Mass of CH2O added back to shells for CH2O cost of excess mobilized N 
!            (never really mobilized) (g[CH2O] / m2 / d)
! CADSR    Mass of CH2O added to storage organs (g[CH2O] / m2 / d)
! CADST    Mass of CH2O added to stems (g[CH2O] / m2 / d)
! CANNAA   Weight of N in total plant at flowering (g[N] / m2)
! CANWAA   Canopy weight at flowering stage (g[plant] / m2)
! CLOFF    CH2O loss from leaves in a day (g[CH2O]/m2-d)
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
! CPFSTR   Respiration requirement for net storage organ growth
!            (g[CH20] / g[tissue])
! CPFSTM   Respiration requirement for net stem growth
!            (g[CH20] / g[tissue])
! CROFF    CH2O loss from roots in a day (g[CH2O]/m2-d)
! CROP     Crop identification code 
! CRUSLF   C mobilized from leaf tissue in a day (g[CH2O] / m2 / d)
! CRUSRT   C mobilized from root tissue in a day (g[CH2O] / m2 / d)
! CRUSSH   C mobilized from shell tissue in a day (g[CH2O] / m2 / d)
! CRUSSR   C mobilized from storage organs in a day (g[CH2O] / m2 / d)
! CRUSST   C mobilized from stem tissue in a day (g[CH2O] / m2 / d)
! CSOFF    CH2O loss from stems in a day (g[CH2O]/m2-d)
! CSROFF   CH2O loss from STOR in a day (g[CH2O]/m2-d)
! CSRW      Cumulative storage growth (g[storage]/m2)
! CSW      Cumulative stem growth (g[stem]/m2)
! DAP      Number of days after planting (d)
! DISLA    Diseased leaf area (cm2/m2/d)
! DWNOD    Current nodule mass (g[nodule] / m2)
! DWNODA   Cumulative nodule growth (g[nodule] / m2)
! DYNAMIC  Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
!            INTEGR, OUTPUT, or SEASEND 
! ECONO    Ecotype code - used to match ECOTYP in .ECO file 
! ECOTYP   Ecotype code for this simulation 
! EODLFC       "End-of-Day" leaf CH2O concentration, after adding CADLF
!             but prior to senescence (proportion).
! EODLFN       "End-of-Day" leaf N concentration, after adding NADLF
!             but prior to senescence (proportion).
! EODRTC       "End-of-Day" root CH2O concentration, after adding CADRT
!             but prior to senescence (proportion).
! EODRTN       "End-of-Day" root N concentration, after adding NADRT
!             but prior to senescence (proportion).
! EODSRC       "End-of-Day" STOR CH2O concentration, after adding CADSR
!             but prior to senescence (proportion).
! EODSRN       "End-of-Day" STOR N concentration, after adding NADSR
!             but prior to senescence (proportion).
! EODSTC       "End-of-Day" stem CH2O concentration, after adding CADST
!             but prior to senescence (proportion).
! EODSTN       "End-of-Day" stem N concentration, after adding NADST
!             but prior to senescence (proportion).
! F        Specific leaf area of new leaf tissue growth, including N
!            (cm2[leaf] / g[leaf])
! FILECC   Path plus filename for species file (*.spe) 
! FILEGC   Pathname plus filename for ECO file 
! FILEIO   Filename for INP file (e.g., IBSNAT35.INP) 
! FRLF     Fraction of vegetative tissue growth that goes to leaves on a 
!            day (g[leaf] / g[veg])
! FRSTR    Fraction of vegetative tissue growth that goes to storage organs 
!            on a day (g[storage] / g[veg])
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
! LFCADDM       Leaf CH2O refill (CADLF) lost in pest + freeze damage (g [CH2O] m-2 d-1
! LFNADDM       Leaf N refill (NADLF) lost in pest + freeze damage (g [N] m-2 d-1
! LFNADFDM Leaf N refill (NADLF) lost in freeze damage (g [N] m-2 d-1
! LFSCMOB   Mass of leaf CH2O mobilized from tissue lost to natural 
!              senescence (g [CH2O] m-2 d-1)
! LFSENWT   Leaf senescence due to N mobilization (g[leaf] / m2[ground])
! LFSNMOB   Mass of leaf N mobilized from tissue lost to natural  
!              senescence (g [N] m-2 d-1)
! LUNCRP   Logical unit number for FILEC (*.spe file) 
! LUNECO   Logical unit number for FILEE (*.eco file) 
! LUNIO    Logical unit number for FILEIO 
! NADLF    N added to leaf N reserves (g[N] / m2 / d)
! NADRT    N added to root N reserves (g[N] / m2 / d)
! NADSH    N added to shell N reserves (g[N] / m2 / d)
! NADSR    N added to storage organ N reserves (g[N] / m2 / d)
! NADST    N added to stem N reserves (g[N] / m2 / d)
! NDTH     Nodule death rate (g[nodule] / m2 / d)
! NFIXN    Amount of N fixed during the day (g[N] / m2 / d)
! NGRLF    Maximum N demand for leaf growth (g[leaf N] / m2[ground] / d)
! NGRRT    Maximum N demand for root growth (g[root N] / m2[ground] / d)
! NGRSD    Rate of N accumulation in new seeds (g[N] / m2 / d)
! NGRSH    Rate of N accumulation in new shells (g[N] / m2 / d)
! NGRSR    Maximum N demand for storage organ growth 
!                  (g[stem N] / m2[ground] / d)
! NGRST    Maximum N demand for stem growth (g[stem N] / m2[ground] / d)
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
! NRUSSR   N actually mobilized from storage organs in a day (g[N]/m2-d)
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
! NSRALL   N added to storage organ today (g[N]/m2-d)
! NSRDOT   Net N addition for storage organ (g[N] / m2[ground] / d)
! NSROFF   N loss from storage organ in a day (g[N]/m2-d)
! NTOVR    (Not used) 
! PCARLF   Proportion of leaf tissue that is carbohydrate (fraction)
! PCARNO   Proportion of nodule tissue that is carbohydrate (fraction)
! PCARRT   Proportion of root tissue that is carbohydrate (fraction)
! PCARSD   Proportion of seed tissue that is carbohydrate
!            (g[CH2O] / g[seed])
! PCARSH   Proportion of shell tissue that is carbohydrate (fraction)
! PCARSR   Proportion of storage tissue that is carbohydrate (fraction)
! PCARST   Proportion of stem tissue that is carbohydrate (fraction)
! PCCSD    Percentage of carbohydrate in seed tissue (100 g[C] / g[seed])
! PCHOLFF  Proportion of mobile CH2O remaining in senesced leaf tissue
!             g [CH2O] / g [senesced leaf DM]
! PCHORTF  Proportion of mobile CH2O remaining in senesced root tissue
!             g [CH2O] / g [senesced root DM]
! PCHOSRF  Proportion of mobile CH2O remaining in senesced STOR tissue
!             g [CH2O] / g [senesced STOR DM]
! PCHOSTF  Proportion of mobile CH2O remaining in senesced stem tissue
!             g [CH2O] / g [senesced leaf DM]
! PCLSD    Percentage of lipid in seed tissue (100 g[lipid] / g[seed])
! PCNL     Percentage of N in leaf tissue (100 g[N] / g[leaf])
! PCNMIN   Minimum percentage of leaf N composition after N mining
!            ( 100 g[N] / g[leaf])
! PCNRT    Percent N in root tissue (100 g[N] / g[root])
! PCNSD    Percentage of N in seed tissue (100 g[N] / g[seed])
! PCNSH    Percentage of N in shell tissue (100 g[N] / g[shell])
! PCNSR    Percent N in storage organ (100 g[N] / g[storage])
! PCNST    Percent N in stem tissue (100 g[N] / g[stem])
! PLIGLF   Proportion of leaf tissue that is lignin (fraction)
! PLIGNO   Proportion of nodule tissue that is lignin (fraction)
! PLIGRT   Proportion of root tissue that is lignin (fraction)
! PLIGSD   Proportion of seed tissue that is lignin (fraction)
! PLIGSH   Proportion of shell tissue that is lignin (fraction)
! PLIGSR   Proportion of storageorgan that is lignin (fraction)
! PLIGST   Proportion of stem tissue that is lignin (fraction)
! PLIPLF   Proportion of leaf tissue that is lipid (fraction)
! PLIPNO   Proportion of nodule tissue that is lipid (fraction)
! PLIPRT   Proportion of root tissue that is lipid (fraction)
! PLIPSH   Proportion of shell tissue that is lipid (fraction)
! PLIPSR   Proportion of storage tissue that is lipid (fraction)
! PLIPST   Proportion of stem tissue that is lipid (fraction)
! PLME     Planting method; T = transplant, S = seed, P = pre-germinated 
!            seed, N = nursery 
! PLTPOP   Plant population (# plants / m2)
! PMINLF   Proportion of leaf tissue that is mineral (fraction)
! PMINNO   Proportion of nodule tissue that is mineral (fraction)
! PMINRT   Proportion of root tissue that is mineral (fraction)
! PMINSD   Proportion of seed tissue that is mineral (fraction)
! PMINSH   Proportion of shell tissue that is mineral (fraction)
! PMINSR   Proportion of storage tissue that is mineral (fraction)
! PMINST   Proportion of stem tissue that is mineral (fraction)
! POALF    Proportion of leaf tissue that is organic acid (fraction)
! POANO    Proportion of nodule tissue that is organic acid (fraction)
! POART    Proportion of root tissue that is organic acid (fraction)
! POASD    Proportion of seed tissue that is organic acid (fraction)
! POASH    Proportion of shell tissue that is organic acid (fraction)
! POASR    Proportion of storage tissue that is organic acid (fraction)
! POAST    Proportion of stem tissue that is organic acid (fraction)
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
! PROSRF   Minimum storage organ protein composition after N mining
!            (g[protein] / g[storage])
! PROSRI   Maximum protein composition in storage organ during growth 
!            with luxurious supply of N (g[protein] / g[storage])
! PROSTF   Minimum stem protein composition after N mining
!            (g[protein] / g[stem])
! PROSTI   Maximum protein composition in stems during growth with 
!            luxurious supply of N (g[protein] / g[stem])
! PSRLYRD        Proportion of total storage organ tissue loss from below ground tissue
! PSRLYR1   Proportion of storage organ tissue in soil layer 1 (below soil surface)
! PSRSRFD   Proportion of total storage organ tissue loss from above ground tissue
! PSRSRFL   Proportion of storage organ tissue on/above soil surface
! PSTCLMOB      Proportion of stem CH2O in senesced tissues lost to N mobilization
! PSTCLSTRES      Proportion of stem CH2O in senesced tissues lost to H2O & light stress
! PUNCSD   Cumulative puncture damage to seed (not yet implemented) 
! PUNCTR   Cumulative puncture damage (not yet implemented) 
! PUNDOT   Daily puncture damage (not yet implemented) 
! RHOL     Fraction of leaf which is carbohydrate (g [CH20] / g[leaf])
! RHOR     Fraction of root which is carbohydrate (g [CH2O] / g[root])
! RHOS     Fraction of stem which is carbohydrate (g [CH2O] / g[stem])
! RHOSH    Fraction of shell which is carbohydrate (g [CH2O] / g[shell])
! RHOSR    Fraction of storage tissue which is carbohydrate 
!                  (g [CH2O] / g[storage])
! RMIN     Respiration required for synthesizing mineral structure
!            (g[CH2O] / g[mineral])
! RNITP    True nitrogen concentration in leaf tissue for photosynthesis 
!            reduction. (%)
! ROWSPC   Row spacing (m)
! RTCADDM       Root CH2O refill (CADRT) lost in pest + freeze damage (g [CH2O] m-2 d-1
! RTNADDM       Root N refill (NADRT) lost in pest + freeze damage (g [N] m-2 d-1
! RTSCMOB   Mass of root CH2O mobilized from tissue lost to natural senescence 
!              (g [CH2O] m-2 d-1)
! RTSNMOB   Mass of root N mobilized from tissue lost to natural senescence 
!              (g [N] m-2 d-1)
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
! SLCADDOT  Today's CADLF lost with senescing leaf tissue (g [CH2O]/m2/d)
! SLDOT    Defoliation due to daily leaf senescence (g/m2/day)
! SLMDOT    Defoliation due to daily leaf senescence that is lost at PROLFF, 
!              hence, some fraction of the N content is subject to mobilization (g/m2/day)
! SLNDOT   Leaf senescence due to water stress (g/m2/day)
! SLNADDOT  Today's NADLF lost with senescing leaf tissue (g [N]/m2/d)
! SRCADDM       STOR CH2O refill (CADSR) lost in pest + freeze damage (g [CH2O] m-2 d-1
! SRDOT    Daily root senescence (g / m2 / d)
! SRMDOT    Daily root senescence that is lost at PRORTF, 
!              hence, some fraction of the N content is subject to mobilization (g/m2/day)
! SRNADDM       STOR N refill (NADSR) lost in pest + freeze damage (g [N] m-2 d-1
! SRNADDOT  Today's NADRT lost with senescing root tissue (g [CP]/m2/d)
! SRNADFDM STOR N refill (NADSR) lost in freeze damage (g [N] m-2 d-1
! SRNDOT    Daily root senescence that is lost to water -stress (lost at current N%) 
! SRSCMOB   Mass of STOR CH2O mobilized from tissue lost to natural senescence 
!              (g [CH2O] m-2 d-1)
! SRSNMOB   Mass of STOR N mobilized from tissue lost to natural senescence 
!              (g [N] m-2 d-1)
! SSMDOT    Daily petiole senescence that is lost at PROSTF, 
!              hence, some fraction of the N content is subject to mobilization (g/m2/day)
! SSRMDOT   Daily STOR senescence that is lost at PROSRF, 
!              hence, some fraction of the N content is subject to mobilization (g/m2/day)
! SSCADDOT  Today's CADST lost with senescing stem tissue (g [CH2O]/m2/d)
! SSDOT    Daily senescence of petioles (g / m2 / d)
! SSMDOT    Daily petiole senescence that is lost at PROSTF, 
!              hence, some fraction of the N content is subject to mobilization (g/m2/day)
! SSRMDOT   Daily STOR senescence that is lost at PROSRF, 
!              hence, some fraction of the N content is subject to mobilization (g/m2/day)
! SSNADDOT  Today's NADST lost with senescing stem tissue (g [N]/m2/d)
! SSNDOT   Petiole senescence due to water stress (g/m2/day)
! SSRCADDOT  Today's CADSTR lost with senescing STOR tissue (g [CH2O]/m2/d)
! SSRDOT   Daily senescence of storage organ tissue (g / m2 / d)
! SSRNADDOT  Today's NASTR lost with senescing STOR tissue (g [N]/m2/d)
! SSRNDOT  Daily senescence of storage organ due to water stress (g/m2/day)
! STCADDM       Stem CH2O refill (CADST) lost in pest + freeze damage (g [CH2O] m-2 d-1
! STLTSEN   Stem senescence corresponding to LTSEN
! STMWT    Dry mass of stem tissue, including C and N (g[stem] / m2[ground)
! STNADDM       Stem N refill (NADST) lost in pest + freeze damage (g [N] m-2 d-1
! STNADFDM Stem N refill (NADST) lost in freeze damage (g [N] m-2 d-1
! STRLYR1  Initial proportion of storage organ dry mass in soil layer 1
! STRSRFL  Initial proportion of storage organ dry mass on soil surface
! STRWT    Dry mass of storage organ, including C and N (g[storag] / m2[ground)
! STSCMOB   Mass of petiole CH2O mobilized from tissue lost to natural senescence 
!              (g [CH2O] m-2 d-1)
! STSENWT   Stem senescence corresponding to SENWT
! STSNMOB   Mass of petiole N mobilized from tissue lost to natural senescence 
!              (g [N] m-2 d-1)
! SWIDOT   Daily seed mass damage (g/m2/day)
! TGROW    Cumulative growth of plant tissue (g[tissue] / m2)
! TNCFAC   Degree of TNC influence on RNITP, 0 TNC reduces photo and RNITP, 
!          1 increases RNITP                      
! TOPWT    Total weight of above-ground portion of crop, including pods
!            (g[tissue] / m2)
! TOTWT    Total weight of crop (g[tissue] / m2)
! TPSRLYR1 Intermediate value used in updating value of PSRLYR1
! TPSRSRFL Intermediate value used in updating value of PSRSRFL
! TRNH4U   Total N uptake in ammonium form in a day (g[N] / m2 / d)
! TRNO3U   Total N uptake in nitrate form in a day (g[N] / m2 / d)
! TRNU     Total N uptake in a day (g[N] / m2 / d)
! TSCMOB    Total plant CH2O mobilized from tissue lost to natural and 
!              low-light senescence (g [CH2O] m-2 d-1)
! TURFAC   Water stress factor for expansion (0 - 1) 
! VSTAGE    Number of nodes on main stem of plant 
! WCRLF    Mass of CH2O reserves in leaves (g[leaf CH2O] / m2[ground])
! WCRRT    Mass of CH2O reserves in roots (g[root CH2O] / m2[ground])
! WCRSH    Mass of CH2O reserves in shells (g[shell CH2O] / m2[ground])
! WCRSR    Mass of CH2O reserves in storage organ (g[storage CH2O] / m2[ground])
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
! WNRSR    N available for mobilization from storage organ above lower 
!            limit of mining (g[N] / m2)
! WNRST    N available for mobilization from stems above lower limit of 
!            mining (g[N] / m2)
! WPDOT    Net pod growth rate (g[pod] / m2 / d)
! WRCLDT   Net C addition for leaves (g[CH2O] / m2 /d)
! WRCRDT   Net C addition for roots (g[CH2O] / m2 /d)
! WRCSDT   Net C addition for stems (g[CH2O] / m2 /d)
! WRCSHD   Net C addition for shells (g[CH2O] / m2 /d)
! WRCSRDT  Net C addition for storage organ (g[CH2O] / m2 /d)
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
! WSFDOT   Stem weight losses due to freezing (g[stem]/m2-d)
! WSHDOT   Net shell growth rate (g[shell] / m2 / d)
! WSHDTN   New shell growth today (g[shell] / m2 / d)
! WSHIDT   Weight of shell tissue consumed by pests today (g[shell]/m2-d)
! WSRI     Initial weight of storage organ (g[storage] / m2)
! WSRDOT   Net storage organ growth rate (g[storage] / m2 / d)
! WSRDOTN  New storage tissue growth today (g[storage] / m2 / d)
! WSRFDOT       Storage organ weight losses due to freezing (g[storage]/m2-d)
! WSRIDOT  Weight of storage organ consumed by pests today (g[storage]/m2-d)
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
! WTNSR    Mass of N in storage organ (g[storage N] / m2[ground])
! WTNSRA   Cumulative N added to shtorage organ (g[N]/m2-d)
! WTNSRO   Cumulative N loss from storage organ (g[N] / m2)
! WTNST    Mass of N in stems (g[stem N] / m2[ground])
! WTNTOT   Total plant N content (g[N] / m2[ground])
! WTNUP    Cumulative N uptake (g[N] / m2)
! WTPSD    Maximum weight per seed under non-limiting substrate (g / seed)
! WTRO     Cumulative root losses (g[root] / m2)
! WTSDO    Cumulative seed losses (g[seed] / m2)
! WTSHMT    Cohorts that reach THRESH today (g/m2)
! WTSHO    Cumulative shell losses (g[shell] / m2)
! WTSO     Cumulative stem losses (g[stem] / m2)
! WTSRO    Cumulative storage organ losses (g[storage] / m2)
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
!       END SUBROUTINES FOR_GROW and FOR_IPGROW
C=======================================================================

  
