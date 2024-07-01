C=======================================================================
C COPYRIGHT 1998-2024
C                     DSSAT Foundation
C                     University of Florida, Gainesville, Florida
C                     International Fertilizer Development Center
C 
C ALL RIGHTS RESERVED
C=======================================================================
C=======================================================================
C  Perennial Forage Model, Subroutine, K.J. Boote, G. Hoogenboom
C-----------------------------------------------------------------------
C  FORAGE template plant growth subroutine.
C  Computes plant development and growth.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  01/01/1990 GH  Written.
C  04/24/1994 NBP Changed TAIRHR to TGRO.
C  07/14/1995 JWH Modified to reserve some C for nodule growth
C  08/21/1995 GH  Added seed composition.
C  01/13/1999 CHP Adapted for modular system
C  05/11/1999 GH  Further adaptation for modular system
C  03/16/2000 GH  Further adaptation for modular system
C  09/17/2001 CHP Removed CANNAA, RO and RP from output list.
C  09/21/2001 CHP Added KCAN, PORMIN, RWUEP1, RWUMX to output list.
C  12/03/2001 GH  Changed name to CROPGRO to reflect template approach.
C  02/03/2003 CHP EORATIO and KEP now exported for ET routines.
C  06/22/2003 SJR Added calls to DORMANT subroutine.
C  05/31/2005 SJR Added CH2OREF to senesce organs at minimum CH2O conc.
C  09/28/2005 SJR Added SENMOB to senesce organs earlier in the day.
!  01/26/2023 CHP Reduce compile warnings: add EXTERNAL stmts, remove 
!                 unused variables, shorten lines. 
C=======================================================================

      subroutine FORAGE(CONTROL, ISWITCH, 
     &    EOP, NH4, NO3, SOILPROP,                        !Input
     &    ST, SW, TRWUP, WEATHER, YREND, YRPLT,           !Input
     &    CANHT, EORATIO, HARVRES, MDATE,                 !Output
     &    NSTRES, PSTRES1, CropStatus,                    !Output
     &    PORMIN, RLV, RWUMX, SENESCE,                    !Output
     &    STGDOY, UNH4, UNO3, XHLAI, XLAI)                !Output

!     Variables not used:
!     KSEVAP, KTRANS, HARVFRAC, 
!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
        ! which contain control information, soil
        ! parameters, hourly weather data.
      USE ModuleData
      IMPLICIT NONE
      EXTERNAL FOR_IPPLNT, FOR_PHOTO, FOR_PHENOL, FOR_DORMANCY,
     &  FOR_PEST, FOR_DEMAND, FOR_INCOMP, FOR_SENMOB, FOR_NUPTAK,
     &  FOR_NFIX, FOR_PODS, FOR_PODDET, FOR_CH2OREF, FOR_VEGGR,
     &  FOR_ROOTS, FOR_GROW, for_opgrow, for_mobil,
     &  for_opmob, FOR_RESPIR, FOR_FREEZE, FORAGE_HARVEST,
     &  SUMVALS, FOR_PLANTNBAL, FOR_HRES_CGRO
      SAVE
!-----------------------------------------------------------------------
      CHARACTER*1 DETACH, IDETO, ISWNIT, ISWSYM,
     &    ISWWAT, ISWDIS, MEPHO, RNMODE
      CHARACTER*2 CROP
      CHARACTER*6 ECONO
      CHARACTER*30 FILEIO
      CHARACTER*92 FILECC, FILEGC

      INTEGER DAS, DYNAMIC, RUN
      INTEGER NDLEAF, NDSET, NLAYR, NOUTDO,
     &    NR1, NR2, NR5, NR7, NVEG0
      INTEGER RSTAGE, YREND, CropStatus
      INTEGER YREMRG, YRDOY, YRNR1, YRNR2,
     &    YRNR3, YRNR5, YRNR7, MDATE, YRPLT, YRSIM
      INTEGER STGDOY(20)

      REAL AGEFAC, AGRSD3, AREALF, ASMDOT
      REAL AGRLF, AGRRT, AGRSH2, AGRSTM
      REAL AGRSD1, AGRSD2, AGRVG, AGRVG2
      REAL AGRNOD, AGRSH1
      REAL BETN   !, BWAH
      REAL CANNAA, CANWAA, CDMREP, CGRSH, CGRSD, CNODMN, CO2, CSAVEV
      REAL CNDFX, CTONODR, CTONOD, CAVVEG
      REAL CADPR1, CMOBMX, CMINEP
      REAL CLW, CSW, CUMDEP
      REAL CNOD, CRUSLF, CRUSRT, CRUSST,
     &    CRUSSH, CADLF, CADST, CANHT, CANWH, CMINEA
      REAL DAYL, DWNOD
      REAL DWNODA, DISLA, DRPP, DTX, DXR57
      REAL EOP, EP1, EXCESS
      REAL FNINSH, FRACDN, FRCNOD,
     &    F, FNINL, FNINR, FNINS, FNINSD,
     &    FRLF, FRRT, FRSTM
      REAL FREEZ1, FREEZ2
      REAL GDMSD, GRRAT1, GROWTH, GRWRES
!     REAL HARVFRAC(2)
      REAL LAIMX, LAGSD, LNGPEG, LTSEN
      REAL MAINR
      REAL NSTRES, NPLTD, NDMNEW,
     &    NADRT, NADST, NGRLF, NGRRT, NGRST,
     &    NADLF, NMINEA,
     &    NDMOLD, NDMREP, NDMSDR, NDMTOT, NDMVEG,
     &    NMINEP, NMOBR, NDTH, NFIXN, NODGR
      REAL NAVL, NRUSSH, NGRSD, NGRSH
      REAL NRUSLF, NRUSRT, NRUSST
      REAL PORPT, POTLIP, POTCAR, PPLTD,
     &    PAR, PCNL, PCNRT, PCNST, PCTMAT
      REAL PLIGLF, PLIGNO, PLIGRT, PLIGST
      REAL PODWTD, PG, PCLSD, PCCSD, PCNSD, PCNSH, PODWT
      REAL PODNO
      REAL PGAVL, PLTPOP, PCARSH, PCH2O, PLIPSH,
     &    PLIGSD, PLIGSH, PMINSD, PMINSH, POASD, POASH,
     &    PROLFI, PRORTI, PROSHI, PROSTI
!      REAL PPGFAC, PPTFAC, PPMFAC
      REAL R30C2, RCH2O, RES30C, RPROAV,
     &    RFIXN, RLIG, RLIP, RMIN, RNITP,
     &    RNH4C, RNO3C, ROA, RPRO
      REAL RHOL, RO, RP, RTDEP, RHOS, ROWSPC
      REAL RTWT, RVSTGE, RSPNO3, RSPNH4
      REAL SEEDNI, NAVLV, PROVEG, SDNPL

!     Senescence variables computed for soil N routines.
!      REAL SENCLN(0:NL, 3)
      REAL SENNOD(NL), SENRT(NL)

      REAL SATFAC, SWFAC, SDWTAM,  !, SDWTAH,
     &    SDRATE, SDWT, SDIDOT,
     &    SDVAR, SHVAR, SDGR, SDPROR, SHELWT,
     &    SLA, SLDOT, STMWT, SWIDOT, SEEDNO
      REAL SLPF
      REAL SLCADDOT, SRCADDOT, SSRCADDOT, SSCADDOT
      REAL SLNADDOT, SRNADDOT, SSRNADDOT, SSNADDOT 
      REAL SRDOT, SLAAD, SLNDOT, SSDOT, SSNDOT
      REAL TDAY, TDUMX, TDUMX2, TGROAV, TMIN, TURFAC
      REAL TAVG, TURADD, TRNH4U, TRNO3U, TRNU, TNLEAK
      REAL TRWUP, TTFIX, TOPWT, TOTWT
      REAL VSTAGE
      REAL WLFDOT, WSIDOT, WRIDOT
      REAL WTNCAN, WTNFX, WTNLA, WTNLO, WTNNA, WTNNAG
      REAL WTNNO, WTNNOD, WTNRA, WTNRO, WTNSA, WTNSDA
      REAL WTNSDO, WTNSHA, WTNSHO, WTNSO, WTNUP, WTNOO
      REAL WTLO, WTSO, WTRO, WSDDTN, WSHDTN, WTABRT, WTSHMT
      REAL WTCO, WTSHO, WTSDO, WCRLF, WCRRT,
     &    WCRSH, WLDOTN, WRDOTN, WSDOTN,
     &    WCRST, WNRLF, WNRRT, WNRSH, WNRST,
     &    WTNRT, WTNSD, WTNSH, WTNST, WTNEW, WTMAIN, WTNLF,
     &    WTLF, WSHIDT, WLIDOT
      REAL XPOD, XFRT, XHLAI, XLAI

      REAL BD(NL), DLAYR(NL), DS(NL), DUL(NL), LL(NL), SAT(NL),
     &    SW(NL), ST(NL), RLV(NL), WR(NL)
      REAL NH4(NL), NO3(NL), UNH4(NL), UNO3(NL)
      REAL PHTHRS(20)
      REAL TGRO(TS)
      REAL SDDES(NCOHORTS)
      REAL WTSD(NCOHORTS), WTSHE(NCOHORTS), SDNO(NCOHORTS) 
      REAL SHELN(NCOHORTS)
      REAL PHTIM(365)
      REAL PNTIM(365)

      REAL FLWN(NCOHORTS)

!CHP - puncture variables, not functional
      REAL PUNCSD, PUNCTR

!     Species-dependant variables exported to SPAM module:
      REAL EORATIO, KCAN, KEP, PORMIN, RWUMX, RWUEP1

C      Storage organ parameters for forage model
      CHARACTER*6 DRMST

      CHARACTER*3   TYPPGD,TYPPTD, TYPPMD, TYPHRD, TYPDHD

      REAL AGRSTR, ALPHSR, CADSR, CADSRF, CMOBSR, !CLAIT, 
     &   CMOBSRN, CMOBSRX, CPFSTR, CRUSSR, CSRFRZ, CSRW, CSTRM,
     &   DSTOR, FNINSR, FNINSRG, FRSTR, FRSTRF, FRSTRM,
     &   FRSTRMX, FRZDC, FRZDL, HARD1, HARD2, NADSR,
     &   NGRSR, NGRSRG, NMOBSR, NMOBSRN, NMOBSRX, NRUSSR, NSRALL,
     &   NSRDOT, NSROFF, PCARSR, PCNSR, PCSTRD,
     &   PLIGSR, PLIPSR, PMINSR, POASR, PPGFAC, PPMFAC,
     &   PPTFAC, PROSRF, PROSRG, PROSRI, PROSRT, 
     &   PSRLYR1, PSRLYRD, PSRSRFD, PSRSRFL, RCHDP, RDRMG, 
     &   RDRMM, RDRMT, RHOSR, SRDAM, SRFTEMP, SRLYRD,  !SENSR, 
     &   SRSRFD, SSRDOT, SSRNDOT, STRLYR1, STRSRFL, STRWT,
     &   TPSRLYR1, TPSRSRFL, WCRSR, WNRSR, WRCSRDT, WSFDOT,
     &   WSRDOT, WSRDOTN, WSRFDOT, WSRI, WSRIDOT, WTNSR, 
     &   WTNSRA, WTNSRO, WTSRO, XSTR

      REAL FNPGD(4), FNPMD(4), FNPTD(4), FRZDHD(4), FRZHRD(4),
     &   YSTOR(8)

      REAL CADRT, CADSH, NADSH  !, NRUSTOT 
      REAL PNMLF, PNMRT, PNMSH, PNMSR, PNMST
      REAL CSFRZ
      REAL NVSTL, NVSTR, NVSTS, NVSTSR
      REAL NUPNH4(NL), NUPNO3(NL)

      REAL LAIMOBR, VNMOBR    !, VEGNCNT, VEGNCMX
      CHARACTER*3 TYPLMOB, TYPNMOB 
      REAL LRMOB(4), NRMOB(4) !, VEGNPCT, VEGNPMX, VNSTAT

      CHARACTER*1 MRSWITCH, TRSWITCH
      CHARACTER*3 TRSTYP

      REAL LFMRC, mft, RTMRC, SDMRC, SHELMRC, STMMRC,
     &   STRMRC, TRST(4)

      CHARACTER*3 TYPCREF, TYPLREF, TYPPREF 
      REAL CRREF(4), LRREF(4), PRREF(4)
      REAL ALPHL, ALPHR, ALPHS, ALPHSH
      REAL CADPV, CDEBIT
      REAL NLALL, NRALL, NSALL
      REAL PCHOLFF, PCHORTF, PCHOSRF, PCHOSTF

      REAL LFSCMOB, RTSCMOB, SRSCMOB, STSCMOB, TSCMOB
      REAL LFSNMOB, RTSNMOB, SLMDOT, SRMDOT,             
     &  SRSNMOB, SSMDOT, SSRMDOT, STSNMOB, TSNMOB

      REAL ACMINELF, ACMINERT, ACMINESH, ACMINESR, ACMINEST
      REAL ANMINELF, ANMINERT, ANMINESH, ANMINESR, ANMINEST
      REAL CMINELF, CMINERT, CMINESH, CMINESR, CMINEST
      REAL LFCMINE, RTCMINE, SRCMINE, STCMINE
      REAL NMINELF, NMINERT, NMINESR, NMINEST, SHCMINE, SHNMINE

      REAL LFCDEBT, RTCDEBT, SRCDEBT, STCDEBT


      REAL LFSENWT, RLSEN(NL), SRNDOT
      REAL STLTSEN, STSENWT

      REAL RHOR, WLDOT, WRCLDT, WRCRDT, WRCSDT, WRCSHD,            
     &  WRDOT, WSDOT

      REAL CADVG, CHORECOVER, NLKSPENT, NLKNUSED
      REAL NLKCHK, TNLKCHK
      REAL PHZACC(20)


!     CHP/KJB 4/24/2017
      REAL Cumul_FHTOT, Cumul_FHTOTN, FHTOTN

C      Forage harvest damage/removal variables for forage model
      REAL HPDAM

!     REAL CURV
      real vstagp

      real pstres1    !,ktrans,ksevap
      type(weathertype) weather

C------------------------------------------------------------
C  PDA 5/6/2010 VARIABLES ADDED FOR FORAGE HARVEST SUBROUTINE
C------------------------------------------------------------
      REAL FHLEAF,FHSTEM,FHVSTG
      REAL FHWAH,FHLPH !DIEGO ADDED 10/26/2016
      REAL DWTCO, DWTLO, DWTSO !DIEGO ADDED 11/22/2016
      REAL PWTCO, PWTLO, PWTSO !DP & FO & TF ADDED 07/16/2019
      REAL fhpctn !DIEGO ADDED 01/18/2017
      REAL FREQ !,CUHT !DIEGO ADDED 02/14/2017
      REAL MOWC,RSPLC !DIEGO ADDED 03/10/2017
!     LOGICAL RUNYET

C FO/DP/TF - 2020-07-22 - AutomaticMOW
      LOGICAL ATMOW
      INTEGER HMFRQ, MOWCOUNT
      INTEGER HMGDD, CUTDAY
      REAL HMCUT
      REAL MOWGDD, TGMIN
      REAL VTO1, VTB1
      REAL MOWREF, RSREF
      
!      INTEGER,dimension(6) :: IXFREQ
      REAL,dimension(6) :: XFREQ
      REAL,dimension(6) :: YFREQ
!      INTEGER,dimension(6) :: IXCUTHT
      REAL,dimension(6) :: XCUTHT
      REAL,dimension(6) :: YCUTHT
!      INTEGER,dimension(6) :: IXCHMOW
      REAL,dimension(6) :: XCHMOW
      REAL,dimension(6) :: YCHMOW
!      INTEGER,dimension(6) :: IXFRGDD
      REAL,dimension(6) :: XFRGDD
      REAL,dimension(6) :: YRSREF
      
      REAL PROLFF,PROSTF
C TF/DP 2022-01-31 Simple version AutoMOW
      INTEGER HMMOW, HRSPL, HMVS
      CHARACTER*1 ATTP


!     Arrays which contain data for printing in SUMMARY.OUT file
      INTEGER, PARAMETER :: SUMNUM = 3
      CHARACTER*4, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

!     The variable "SOILPROP" is of type "SoilType".
      TYPE (SoilType) SOILPROP

!     The variable "ISWITCH" is of type "SwitchType".
      TYPE (SwitchType) ISWITCH

!     The variable "HARVRES" is of type "ResidueType".
      Type (ResidueType) HARVRES
      Type (ResidueType) SENESCE

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
      LL     = SOILPROP % LL     
      NLAYR  = SOILPROP % NLAYR  
      SAT    = SOILPROP % SAT    
      SLPF   = SOILPROP % SLPF  
      WR     = SOILPROP % WR     

      IDETO  = ISWITCH % IDETO
      ISWDIS = ISWITCH % ISWDIS
      ISWNIT = ISWITCH % ISWNIT
      ISWSYM = ISWITCH % ISWSYM
      ISWWAT = ISWITCH % ISWWAT
      MEPHO  = ISWITCH % MEPHO
      ATMOW  = ISWITCH % ATMOW
      HMFRQ  = ISWITCH % HMFRQ
      HMGDD  = ISWITCH % HMGDD
      HMCUT  = ISWITCH % HMCUT
      HMMOW  = ISWITCH % HMMOW
      HRSPL  = ISWITCH % HRSPL
      HMVS   = ISWITCH % HMVS
      ATTP   = ISWITCH % ATTP

      CO2    = WEATHER % CO2   
      DAYL   = WEATHER % DAYL  
      PAR    = WEATHER % PAR   
      TAVG   = WEATHER % TAVG  
      TDAY   = WEATHER % TDAY  
      TGRO   = WEATHER % TGRO  
      TGROAV = WEATHER % TGROAV
      TMIN   = WEATHER % TMIN  

! Disable P stress
      pstres1 = 1

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
!     Call input routine for CROPGRO module parameters
!-----------------------------------------------------------------------
      CALL FOR_IPPLNT(CONTROL, 
     &  CADPR1, CMOBMX, CROP, DETACH, ECONO, EORATIO,     !Output
     &  FILECC, FILEGC, FRCNOD, FREEZ1, FREEZ2, KCAN, KEP,!Output
     &  NOUTDO, PCARSH, PCH2O, PLIPSH, PLIGSD, PLIGSH,    !Output
     &  PMINSD, PMINSH, POASD, POASH, PORMIN, PROLFI,     !Output
     &  PRORTI, PROSHI, PROSTI, R30C2, RCH2O, RES30C,     !Output
     &  RFIXN, RLIG, RLIP, RMIN, RNH4C, RNO3C, ROA,       !Output
     &  RPRO, RWUEP1, RWUMX, TTFIX,                       !Output
     &  CADPV, PROSRI, STRSRFL, STRLYR1,                  !Output
     &  LFMRC, mft, MRSWITCH, RTMRC, SDMRC,SHELMRC,       !Output
     &  STMMRC, STRMRC, TRST, TRSTYP, TRSWITCH)           !Output

      PSRSRFL = STRSRFL
      PSRLYR1 = STRLYR1


      IF (CROP .NE. 'FA' .AND. MEPHO .EQ. 'C') THEN
        CALL FOR_PHOTO(CONTROL, 
     &   BETN, CO2, DXR57, EXCESS, NR5, PAR, SLPF,    !Input
     &   RNITP, SLAAD, SWFAC, TDAY, XHLAI, XPOD,      !Input
     &   AGEFAC, PG)                                  !Output

      ENDIF

!-----------------------------------------------------------------------
      CALL FOR_PHENOL(CONTROL, ISWITCH, 
     &    DAYL, NSTRES, SOILPROP, ST, SW, SWFAC, TGRO,    !Input
     &    TGROAV, TMIN, TURFAC, XPOD, YRPLT,              !Input
     &    DRPP, DTX, DXR57, FRACDN, MDATE, NDLEAF,        !Output
     &    NDSET, NR1, NR2, NR5, NR7, NVEG0, PHTHRS,       !Output
     &    PHZACC, RSTAGE, RVSTGE, STGDOY, TDUMX, TDUMX2,  !Output
     &    VSTAGE,vstagp, YREMRG, YRNR1, YRNR2, YRNR3, YRNR5, !Output
     &    YRNR7)                                          !Output

C-----------------------------------------------------------------------
C     Read initial Dormancy data 
C-----------------------------------------------------------------------
      CALL FOR_DORMANCY( CONTROL, 
     &    DAYL, TMIN,                                           !Input
     &    DRMST, FREEZ2, FRZDC, PPGFAC, PPTFAC, PPMFAC,         !Output

     &  FNPGD, FNPMD, FNPTD, FRZDHD, FRZHRD, HARD1,         !Output
     &  HARD2, RCHDP, RDRMG, RDRMM, RDRMT, TYPDHD, TYPHRD,  !Output
     &  TYPPGD, TYPPMD, TYPPTD)                             !Output


!-----------------------------------------------------------------------
      IF (ISWDIS .EQ. 'Y') THEN
        CALL FOR_PEST(CONTROL, ISWITCH, 
     &    AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, PGAVL,    !Input
     &    PHTIM, PLTPOP, RTWT, SLA, SLDOT, SOILPROP,      !Input
     &    SSDOT, STMWT, TOPWT, WLFDOT, WTLF, YRPLT,       !Input
     &    RLV, SDNO, SHELN, SWIDOT,                       !Input/Output
     &    VSTAGE, WSHIDT, WTSD, WTSHE,                    !Input/Output
     &    ASMDOT, DISLA, HPDAM, NPLTD, PPLTD,             !Output
     &    SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT,             !Output
     &  CSRW, SSRDOT, STRWT, WSFDOT, WSRFDOT,         !Input
     &  WSRIDOT,                                      !Output

     &  CSFRZ, CSRFRZ, CSTRM, DSTOR, SRDAM)           !Output
      ENDIF

!-----------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
        CALL FOR_DEMAND(RUNINIT,
     &  AGRLF, AGRRT, AGRSH2, AGRSTM, CROP, DRPP, DXR57,  !Input
     &  FILECC, FILEGC, FILEIO, FNINSH, FRACDN, LAGSD,    !Input
     &  LNGPEG, NDLEAF, NMINEP, NSTRES,                   !Input
     &  PAR, PGAVL, PHZACC, PUNCSD,                       !Input
     &  PUNCTR, PLTPOP, RPROAV, RTWT,                     !Input
     &  SDDES, SDNO, SDVAR, SHELN, SHVAR, SLDOT, SRDOT,   !Input
     &  SSDOT, SSRDOT, STMWT,                             !Input
     &  TAVG, TDUMX, TGRO, TURFAC,                        !Input 
     &  VSTAGE, WCRLF, WCRRT, WCRST,                      !Input
     &  WTLF, WTNLF, WTNRT, WTNSR, WTNST, WTSD,           !Input
     &  WTSHE, YRDOY,                                     !Input
     &  NVEG0, NR1, NR2, NR7, YRSIM,                      !Input

     &  AGRSD1, AGRSD2, AGRVG, AGRVG2, CDMREP, F, FNINL,  !Output
     &  FNINR, FNINS, FNINSD, FRLF, FRRT, FRSTM, GDMSD,   !Output
     &  GRRAT1, NDMNEW, NDMOLD, NDMREP, NDMSDR, NDMTOT,   !Output
     &  NDMVEG, PHTIM, PNTIM, POTCAR,                     !Output
     &  POTLIP, SDGR, TURADD, XFRT,                       !Output
     &  PPTFAC, STRWT,                                    !Input?
     &  WCRSR,                                            !Input?
     &  AGRSTR, FNINSR, FRSTR,                            !Output
     &  FRSTRF, FRSTRM, FRSTRMX, LRMOB,                   !Output
     &  NMOBSRN, NMOBSRX, NRMOB, NVSTL, NVSTR, NVSTS,     !Output
     &  NVSTSR, TYPLMOB, TYPNMOB, XSTR, YSTOR)            !Output

!-----------------------------------------------------------------------
C    Call plant COMPosition INitialization (for data input)
C-----------------------------------------------------------------------
        CALL FOR_INCOMP(
     &    ECONO, FILECC, FILEGC, FRLF, FRRT,              !Input
     &    FRSTM,                                          !Input
     &    AGRLF, AGRNOD, AGRRT, AGRSD1, AGRSD2,           !Output
     &    AGRSH1, AGRSH2, AGRSTM, AGRVG, AGRVG2,          !Output
     &  SDPROR,                                       !Output
     &  AGRSTR, FRSTR,                                !Output      
     &  RUNINIT)
C-----------------------------------------------------------------------
C      09/23/05 SJR Added subroutine
C    Call Subroutine to calculate Nitrogen mobilized from natural 
C      senescence and light stress
C-----------------------------------------------------------------------

      CALL FOR_SENMOB(
     &    FILECC, CLW, DLAYR, DTX, DUL, DXR57, FNINL,           !Input
     &    FNINR, FNINS, FNINSR, ISWWAT, LL, NLAYR, NR5,         !Input 
     &    NR7, NSTRES, PAR, PCNL, PCNRT, PCNSR, PCNST,          !Input
     &    PPMFAC, RLV, RTWT, SAT, SLAAD, STMWT,                 !Input
     &    STRWT, SW, SWFAC, TDUMX, TDUMX2, VSTAGE, WCRLF,       !Input
     &    WCRRT,WCRSH, WCRSR, WCRST, WNRLF, WNRRT, WNRSH,       !Input
     &    WNRSR,WNRST, WTLF, XLAI, XPOD,                        !Input
     &    YRDOY, YRSIM,                                         !Input
     &    CMINELF, CMINEP, CMINERT, CMINESH, CMINESR,           !Output
     &    CMINEST, CMOBMX, CMOBSR, LAIMOBR, LFCMINE,            !Output
     &    LFSCMOB, LFSENWT, LFSNMOB, LTSEN, NMINELF,            !Output
     &    NMINEP, NMINERT, NMINESR, NMINEST, NMOBR,             !Output
     &    NMOBSR, PORPT, RLSEN, RTCMINE, RTSCMOB, RTSNMOB,      !Output
     &    SHCMINE, SHNMINE, SLDOT, SLMDOT, SRCMINE,             !Output
     &    SRDOT, SRMDOT, SRNDOT, SRSCMOB, SRSNMOB, SSMDOT,      !OutpuT
     &    SSNDOT, SSDOT, SSRDOT, SSRMDOT, SSRNDOT, STCMINE,     !Output
     &    STSCMOB, STSNMOB, STLTSEN, STSENWT, TSCMOB,           !Output
     &    TSNMOB, VNMOBR,                                       !Output
     &    RUNINIT)                                              !Control

!-----------------------------------------------------------------------
        !IF (ISWNIT .EQ. 'Y') THEN
      CALL FOR_NUPTAK(
     &  BD, DLAYR, DUL, FILECC, LL, NDMSDR, NDMTOT, NH4,  !Input
     &  NO3, NLAYR, PGAVL, RLV, RNH4C, RNO3C, SAT, SW,    !Input
     &  NUPNH4, NUPNO3, TRNH4U, TRNO3U, TRNU, TSNMOB,     !Output
     &  UNH4, UNO3,                                       !Output
     &  RUNINIT)                                          !Control

!-----------------------------------------------------------------------
        IF (ISWSYM .EQ. 'Y') THEN
        CALL FOR_NFIX(
     &    AGRNOD, CNODMN, CTONOD, DLAYR, DXR57,       !Input
     &    FILECC, FILEIO, NLAYR, NR7, PLTPOP,         !Input
     &    SAT, ST, SW, TURFAC, YRDOY, YRSIM,          !Input
     &    CNOD, DWNOD, DWNODA, NDTH, NFIXN,           !Output
     &    NODGR, WTNFX, SENNOD,                       !Output
     &    RUNINIT) 
        ENDIF
        !ENDIF

!-----------------------------------------------------------------------
        CALL FOR_PODS(RUNINIT, 
     &    AGRSD1, AGRSH1, DLAYR, DRPP, DUL, FILECC,       !Input
     &    FILEGC,FILEIO, FNINL, FNINSD, FNINSH, GDMSD,    !Input
     &    GRRAT1, ISWWAT, LL, NAVL, NDSET, NLAYR, NRUSSH, !Input
     &    NSTRES, PGAVL, PHTHRS, PHTIM, PNTIM, PUNCSD,    !Input
     &    PUNCTR, RNITP, SDDES, SDGR, SHELWT, SW, SWFAC,  !Input
     &    TDUMX, TGRO, TURADD, XFRT, YRDOY, YRNR1, YRNR2, !Input
     7    YRSIM,                                          !Input
     &    AGRSD3, LAGSD, LNGPEG, NGRSD, NGRSH, PCTMAT,    !Output
     &    PODNO, POTCAR, POTLIP, SDNO, SDVAR, SEEDNO,     !Output
     &    SHELN, SHVAR, WSDDTN, WSHDTN, WTABRT, WTSD,     !Output
     &    WTSHE, WTSHMT, FLWN)                            !Output

!-----------------------------------------------------------------------
        IF (DETACH .EQ. 'Y') THEN
        CALL FOR_PODDET(
     &  FILECC, TGRO, WTLF, YRDOY, YRNR2,             !Input
     &  PODWTD, SDNO, SHELN, SWIDOT,                  !Output
     &  WSHIDT, WTSD, WTSHE,                          !Output
     &  RUNINIT)                                      !Control
        ENDIF
!-----------------------------------------------------------------------

        CALL FOR_CH2OREF(CONTROL,
     &  ALPHL, ALPHR, ALPHS, ALPHSR, CADPV, CRREF,        !INPUT
     &  LFSCMOB, LRREF, PG, PRREF, RTSCMOB,               !INPUT
     &  RTWT, SLDOT, SRDOT, SRSCMOB,                      !INPUT
     &  SSDOT, SSRDOT, STMWT, STRWT, STSCMOB,             !INPUT
     &  TYPCREF, TYPLREF, TYPPREF, WCRLF, WCRRT,          !INPUT
     &  WCRSR, WCRST,                                     !INPUT
     &  WTLF, XLAI,                                       !INPUT
     &  CDEBIT, CADVG, PGAVL,                             !INPUT/OUTPUT
     &  LFCDEBT, RTCDEBT, SRCDEBT, STCDEBT )              !OUTPUT


!-----------------------------------------------------------------------
        CALL FOR_VEGGR(
     &    AGRLF, AGRRT, AGRSTM, CADVG, CMINEP, CSAVEV,    !
     &    ECONO, FILECC, FILEGC, FNINL, FNINR, FNINS,     !
     &    LFCDEBT, LFCMINE, LFSCMOB, LFSNMOB, NAVL,       !
     &    NDMNEW, NDMOLD, PAR, PCH2O, PCNL, PCNST,        !Input
     &    PCNRT, PCNSR, PG, PGAVL, ROWSPC, RTCDEBT,       !
     &    RTCMINE, RTSCMOB, RTSNMOB, RTWT, RVSTGE,        !
     &    SHCMINE, SLDOT, SRCDEBT, SRCMINE, SRDOT,        !
     &    SRSCMOB, SRSNMOB, SSDOT, SSRDOT, STCDEBT,       !
     &    STCMINE, STMWT, STSCMOB, STSNMOB, TGRO, TSCMOB, !
     &    TSNMOB, TURFAC, VSTAGE, WCRLF, WCRRT, WCRST,    !
     &    WTLF, WTNLF, WTNRT, WTNSR, WTNST, XLAI,         !
     &    YRDOY, YREMRG, YRSIM,                           !
     &    AGRSTR, FNINSR, STRWT, WCRSR,                   !Input
     &    NLAYR, PROLFI, PRORTI, PROSTI, PROSRI,          !Input
     &    RNH4C, RNO3C, TRNH4U, TRNO3U, UNO3, UNH4,       !

     &    AGRVG, ANMINELF, ANMINERT, ANMINESR, ANMINEST,  !Input/Output
     &    FRLF, FRRT, FRSTM, FRSTR, NMINEA, NFIXN,        !
     &    NRUSLF, NRUSRT, NRUSSR, NRUSST, TRNU,           !

     &    ACMINELF, ACMINERT, ACMINESH, ACMINESR,         !
     &    ACMINEST, CADLF, CADST, CANHT, CANWH, CMINEA,   !
     &    CRUSLF, CRUSRT, CRUSSH, CRUSST, EXCESS, NADLF,  !
     &    NADRT, NADST, NGRLF, NGRRT, NGRST,              !Output
     &    NSTRES, TNLEAK, WLDOTN, WRDOTN, WSDOTN,         !
     &    PNMLF, PNMRT, PNMSH, PNMSR,PNMST,RPRO,          !
     &    CADRT, CADSH, NADSH, CADSR, CRUSSR, NADSR,      !Output
     &    NGRSR, WSRDOTN, CADSRF, CMOBSRN, CMOBSRX,       !Output
     &    FNINSRG, NGRSRG, PROSRG, PROSRT, CHORECOVER,    !Output
     &    NLKSPENT, NLKNUSED, NLKCHK, TNLKCHK,            !Output
     &    RUNINIT)                                        !Control
!-----------------------------------------------------------------------
C     Call leaf senescence routine for initialization
C-----------------------------------------------------------------------
!        CALL SENES(
!     &    FILECC, CADLF, CADSR, CADST, CLW, DAYL,DTX,      !Input
!     &    HPDAM, LFSNMOB, NADLF, NADSR, NADST, NR7,            !Input
!     &    NRUSLF, PAR, RHOL, SLAAD, SLMDOT, SSRMDOT,            !Input
!     &    SSMDOT, STMWT, SWFAC, VSTAGE, WTLF, XLAI,            !Input
!     &  YRDOY, YRSIM,                                                      !Input
!     &    LFSENWT, LTSEN, PORPT, SLDOT, SLNDOT, SLCADDOT, !Output
!     &  SLNADDOT, SSCADDOT, SSDOT, SSNADDOT, SSNDOT,      !Output
!     &  STRWT,                                                                  !Input
!     &  SSRCADDOT, SSRDOT, SSRNADDOT, SSRNDOT,                  !Output
!     &  STLTSEN, STSENWT,                                                !Output
!
!     &  SENSR,                                                                  !Output
!
!     &    RUNINIT)                                        !Control
C-----------------------------------------------------------------------
C     Call to root growth and rooting depth routine
C-----------------------------------------------------------------------
!        IF (ISWWAT .EQ. 'Y') THEN
        CALL FOR_ROOTS(RUNINIT,
     &    AGRRT, CROP, DLAYR, DS, DTX, DUL, FILECC,       !Input
     &    FILEIO, FRRT, ISWWAT, LL, NLAYR, PG,            !Input
     &    RLSEN, RO, RP, RTWT, SAT, SW,                   !Input
     &    SWFAC, VSTAGE, WR, WRDOTN, WTNEW,               !Input
     &    CUMDEP, RLV, RTDEP, SATFAC, SENRT,              !Output
     &    SRCADDOT, SRNADDOT)                             !Output
        ENDIF

!      ENDIF
!-----------------------------------------------------------------------
      CALL FOR_GROW(CONTROL, ISWITCH, RUNINIT, SOILPROP, 
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

      CALL FOR_OPMOB(CONTROL, ISWITCH, 
     &  YRPLT, MDATE, DAS, YRDOY, DTX, DXR57, PGAVL, NAVL, PG, PPMFAC, 
     &  NMOBR, NMOBSR, MAINR, ASMDOT, RSPNO3, RSPNH4, RPRO,       
     &  CNOD, CGRSD, CGRSH, CADVG, CSAVEV, AGRVG, PCH2O, WTLF, WLDOT, 
     &  WLIDOT, WLFDOT, WLDOTN, RTWT, WRDOT, WRDOTN, STMWT, WSDOT,
     &  WSIDOT, WSFDOT, WSDOTN, 
     &  STRWT, WSRDOT, WSRIDOT, WSRFDOT, WSRDOTN,
     &  SLMDOT, LFSENWT, LTSEN, SLNDOT, SLCADDOT, SLNADDOT, 
     &  SLDOT, SRMDOT, SRNDOT, SRCADDOT, SRNADDOT, SRDOT, 
     &  SSMDOT, STSENWT, STLTSEN, SSNDOT, SSCADDOT, SSNADDOT, 
     &  SSDOT, SSRMDOT, SSRNDOT, SSRCADDOT, SSRNADDOT, SSRDOT,
     &  NDMVEG, NDMNEW, NDMOLD, NDMTOT, NMINEP, NMINEA, SDNPL, TRNU, 
     &  NFIXN, NGRSD, NGRSH, TSNMOB,
     &  WTNLF, WNRLF, LFSNMOB, NMINELF, ANMINELF, 
     &  NGRLF, NLALL, NADLF, NRUSLF, PCNL,
     &  WTNRT, WNRRT, RTSNMOB, NMINERT, ANMINERT, 
     &  NGRRT, NRALL, NADRT, NRUSRT, PCNRT,
     &  WTNST, WNRST, STSNMOB, NMINEST, ANMINEST, 
     &  NGRST, NSALL, NADST, NRUSST, PCNST,
     &  WTNSR, WNRSR, SRSNMOB, NMINESR, ANMINESR, 
     &  NGRSR, NSRALL, NADSR, NRUSSR, PCNSR,
     &  WTNSH, WNRSH, SHNMINE, NRUSSH,    
     &  TSCMOB, WCRLF, WRCLDT, LFSCMOB, CMINELF, CADLF, CRUSLF, RHOL,
     &  WCRRT, WRCRDT, RTSCMOB, CMINERT, CADRT, CRUSRT, RHOR,          
     &  WCRST, WRCSDT, STSCMOB, CMINEST, CADST, CRUSST, RHOS,     
     &  WCRSR, WRCSRDT, SRSCMOB, CMINESR, CADSR, CRUSSR, RHOSR,
     &  WCRSH, WRCSHD, SHCMINE, CRUSSH, CHORECOVER, NLKSPENT, NLKNUSED,
     &  NLKCHK, TNLKCHK, CMOBSR, LAIMOBR, VNMOBR)     

      CALL FOR_OPGROW(CONTROL, ISWITCH,
     &    CADLF, CADST, CANHT, CANWH, CMINEA, DWNOD,  
     &    GROWTH, GRWRES, MAINR, MDATE, NFIXN, NSTRES, 
     &    PCLSD, PCCSD, PCNL, PCNRT, PCNSD, PCNSH, PCNST, 
     &    PG, PODNO, PODWT, PODWTD, RHOL, RHOS, RLV, RSTAGE, 
     &    RTDEP, RTWT, SATFAC, SDWT, SEEDNO, SLA, STMWT, SWFAC, 
     &    TGRO, TGROAV, TOPWT, TOTWT, TURFAC, VSTAGE, WTCO, 
     &    WTLF, WTLO, WTNCAN, WTNLF, WTNST, WTNSD, WTNUP, 
     &    WTNFX, WTSO, XLAI, YRPLT,
     &    DRMST, PPGFAC, PPMFAC, PPTFAC, SRFTEMP, ST, FREEZ2,
     &    AGRSTR, CADSR, CMOBSR, CPFSTR, CRUSSR, CSRFRZ, CSRW, 
     &    CSTRM, DSTOR, FNINSR, FNINSRG, FRSTR, FRSTRM, NADSR, 
     &    NGRSR, NGRSRG, NMOBSR, NRUSSR, NSRALL, NSRDOT, NSROFF, 
     &    NVSTSR, PCNSR, PCSTRD, PROSRT, PSRSRFD, PSRLYRD, 
     &    PSRSRFL, PSRLYR1, RHOSR, SRDAM, SRSRFD, SRLYRD, SSRDOT, 
     &    SSRNDOT, STRWT, TPSRSRFL, TPSRLYR1, WCRSR, WNRSR, 
     &    WRCSRDT, WSRDOT, WSRDOTN, WSRFDOT, WSRI, WSRIDOT, 
     &    WTNSR, WTNSRA, WTNSRO, WTSRO, XSTR,
     &    FRLF, FRSTM, FRRT,
     &    FHWAH, FHLPH, DWTCO, DWTLO, DWTSO,fhpctn,RHOR)

!!     Initialize Overview.out file.
!      CALL FOR_OPHARV(CONTROL, ISWITCH, 
!     &    CANHT, CANNAA, CANWAA, CROP, LAIMX, HARVFRAC,   !Input
!     &    MDATE, NSTRES, PCLSD, PCNSD, PODNO, PODWT,      !Input
!     &    SDRATE, SDWT, SDWTAM, SEEDNO, STGDOY, STMWT,    !Input
!     &    TOPWT, TURFAC, VSTAGE, WTNCAN, WTNFX, WTNSD,    !Input
!     &    WTNST, WTNUP, XLAI,                             !Input
!     &    YRNR1, YRNR3, YRNR5, YRNR7, YRPLT,              !Input
!     &    BWAH, SDWTAH)                                   !Output

!     If this is not a sequenced run, don't use any previously calculated
!       harvest residue.
      IF (RUN .EQ. 1 .OR. INDEX('QF',RNMODE) .LE. 0) THEN
        HARVRES % RESWT  = 0.0
        HARVRES % RESLIG = 0.0
        HARVRES % RESE   = 0.0
      ENDIF

      call forage_harvest(CONTROL,FILECC, ATMOW, ATTP,
     &    RHOL,RHOS,PCNL,PCNST,SLA,RTWT,STRWT,       !Input
     &    WTLF,STMWT,TOPWT,TOTWT,WCRLF,WCRST,        !Input/Output
     &    WTNLF,WTNST,WNRLF,WNRST,WTNCAN,            !Input/Output
     &    AREALF,XLAI,XHLAI,VSTAGE,vstagp,canht,     !Input/Output
     &    FHWAH,FHTOTN, FHLPH,fhpctn,FREQ,
     &    MOWC,RSPLC,HMFRQ,HMGDD,HMCUT,HMMOW,HRSPL,
     &    DWTCO, DWTLO, DWTSO, PWTCO, PWTLO, PWTSO,
     &    HMVS, WTCO, WTLO, WTSO, TAVG, MOWGDD,
     &    MOWCOUNT, TGMIN, VTO1, VTB1, MOWREF, 
     &    RSREF, YFREQ, YRSREF, YCUTHT, YCHMOW,
     &    XCUTHT, XCHMOW, XFRGDD, XFREQ, CUTDAY,
     &    PROLFF, PROSTF, pliglf, pligst)
     
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
      MAINR  = 0.0
      NAVL   = 0.0
      PGAVL  = 0.0
!     PGNOON = 0.0
      RO     = 0.0
      RP     = 0.0
      RPROAV = RFIXN

!     from SWFACS
      TURFAC = 1.0
      SWFAC  = 1.0

!     CHP/KJB 4/24/2017
      Cumul_FHTOT = 0.0
      Cumul_FHTOTN = 0.0

!-----------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
        IF (MEPHO .EQ. 'L') THEN
        !Retrieve AGEFAC and PG from ETPHOT routine.
        CALL GET('SPAM', 'AGEFAC', AGEFAC)
        CALL GET('SPAM', 'PG'    , PG)
        ELSEIF (MEPHO .EQ. 'C') THEN
        CALL FOR_PHOTO(CONTROL, 
     &    BETN, CO2, DXR57, EXCESS, NR5, PAR, SLPF,     !Input
     &    RNITP, SLAAD, SWFAC, TDAY, XHLAI, XPOD,       !Input
     &    AGEFAC, PG)                               !Output

        ENDIF
      ENDIF

!-----------------------------------------------------------------------
      CALL FOR_PHENOL(CONTROL, ISWITCH, 
     &    DAYL, NSTRES, SOILPROP, ST, SW, SWFAC, TGRO,    !Input
     &    TGROAV, TMIN, TURFAC, XPOD, YRPLT,              !Input
     &    DRPP, DTX, DXR57, FRACDN, MDATE, NDLEAF,        !Output
     &    NDSET, NR1, NR2, NR5, NR7, NVEG0, PHTHRS,       !Output
     &    PHZACC, RSTAGE, RVSTGE, STGDOY, TDUMX, TDUMX2,  !Output
     &    VSTAGE,vstagp, YREMRG, YRNR1, YRNR2, YRNR3, YRNR5,     !Output
     &  YRNR7)                                        !Output

!-----------------------------------------------------------------------
      call forage_harvest(CONTROL,FILECC, ATMOW, ATTP,
     &    RHOL,RHOS,PCNL,PCNST,SLA,RTWT,STRWT,       !Input
     &    WTLF,STMWT,TOPWT,TOTWT,WCRLF,WCRST,        !Input/Output
     &    WTNLF,WTNST,WNRLF,WNRST,WTNCAN,            !Input/Output
     &    AREALF,XLAI,XHLAI,VSTAGE,vstagp,canht,     !Input/Output
     &    FHWAH,FHTOTN, FHLPH,fhpctn,FREQ,
     &    MOWC,RSPLC,HMFRQ,HMGDD,HMCUT,HMMOW,HRSPL,
     &    DWTCO, DWTLO, DWTSO, PWTCO, PWTLO, PWTSO,
     &    HMVS, WTCO, WTLO, WTSO, TAVG, MOWGDD,
     &    MOWCOUNT, TGMIN, VTO1, VTB1, MOWREF, 
     &    RSREF, YFREQ, YRSREF, YCUTHT, YCHMOW,
     &    XCUTHT, XCHMOW, XFRGDD, XFREQ, CUTDAY,
     &    PROLFF, PROSTF, pliglf, pligst)
!-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     Seasonal initialization for Dormancy processes
C-----------------------------------------------------------------------

      CALL FOR_DORMANCY( CONTROL,
     &    DAYL, TMIN,                                       !Input
     &    DRMST, FREEZ2, FRZDC, PPGFAC, PPTFAC, PPMFAC,     !Output

     &    FNPGD, FNPMD, FNPTD, FRZDHD, FRZHRD, HARD1,       !Output
     &    HARD2, RCHDP, RDRMG, RDRMM, RDRMT, TYPDHD, TYPHRD,!Output
     &    TYPPGD, TYPPMD, TYPPTD)                           !Output


 
!-----------------------------------------------------------------------
C     Initialize pest coupling point and damage variables, first day only
!         Moved to FOR_PEST module - CHP
!-----------------------------------------------------------------------
      IF (CROP .NE. 'FA' .AND. ISWDIS .EQ. 'Y') THEN
        CALL FOR_PEST(CONTROL, ISWITCH, 
     &    AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, PGAVL,    !Input
     &    PHTIM, PLTPOP, RTWT, SLA, SLDOT, SOILPROP,      !Input
     &    SSDOT, STMWT, TOPWT, WLFDOT, WTLF, YRPLT,       !Input
     &    RLV, SDNO, SHELN, SWIDOT,                       !Input/Output
     &    VSTAGE, WSHIDT, WTSD, WTSHE,                    !Input/Output
     &    ASMDOT, DISLA, HPDAM, NPLTD, PPLTD,             !Output
     &    SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT,             !Output
     &  CSRW, SSRDOT, STRWT, WSFDOT, WSRFDOT,         !Input
     &  WSRIDOT,                                      !Output

     &  CSFRZ, CSRFRZ, CSTRM, DSTOR, SRDAM)           !Output
      ENDIF

!-----------------------------------------------------------------------
!     Initialization call to DEMAND must preceed initialization calls
!         to INCOMP and GROW (need to initialize values of F, FRLF,
!         FRRT, and FRSTM for use in those routines)  chp 9/22/98
!-----------------------------------------------------------------------
      CALL FOR_DEMAND(SEASINIT,
     &  AGRLF, AGRRT, AGRSH2, AGRSTM, CROP, DRPP, DXR57,  !Input
     &  FILECC, FILEGC, FILEIO, FNINSH, FRACDN, LAGSD,    !Input
     &  LNGPEG, NDLEAF, NMINEP, NSTRES,                   !Input
     &  PAR, PGAVL, PHZACC, PUNCSD,                       !Input
     &  PUNCTR, PLTPOP, RPROAV, RTWT,                     !Input
     &  SDDES, SDNO, SDVAR, SHELN, SHVAR, SLDOT, SRDOT,   !Input
     &  SSDOT, SSRDOT, STMWT,                             !Input
     &  TAVG, TDUMX, TGRO, TURFAC,                        !Input 
     &  VSTAGE, WCRLF, WCRRT, WCRST,                      !Input
     &  WTLF, WTNLF, WTNRT, WTNSR, WTNST, WTSD,           !Input
     &  WTSHE, YRDOY,                                     !Input
     &  NVEG0, NR1, NR2, NR7, YRSIM,                      !Input

     &  AGRSD1, AGRSD2, AGRVG, AGRVG2, CDMREP, F, FNINL,  !Output
     &  FNINR, FNINS, FNINSD, FRLF, FRRT, FRSTM, GDMSD,   !Output
     &  GRRAT1, NDMNEW, NDMOLD, NDMREP, NDMSDR, NDMTOT,   !Output
     &  NDMVEG, PHTIM, PNTIM, POTCAR,                     !Output
     &  POTLIP, SDGR, TURADD, XFRT,                       !Output
     &  PPTFAC, STRWT,                                    !Input?
     &  WCRSR,                                            !Input?
     &  AGRSTR, FNINSR, FRSTR,                            !Output
     &  FRSTRF, FRSTRM, FRSTRMX, LRMOB,                   !Output
     &  NMOBSRN, NMOBSRX, NRMOB, NVSTL, NVSTR, NVSTS,     !Output
     &  NVSTSR, TYPLMOB, TYPNMOB, XSTR, YSTOR)            !Output

!-----------------------------------------------------------------------
!     Call plant COMPosition INitialization
!     This call must preceed initialization call to GROW (need to
!         initialize value of SDPROR for use in that routine) chp 9/22/98
!-----------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
        CALL FOR_INCOMP(
     &    ECONO, FILECC, FILEGC, FRLF, FRRT,              !Input
     &    FRSTM,                                          !Input
     &    AGRLF, AGRNOD, AGRRT, AGRSD1, AGRSD2,           !Output
     &    AGRSH1, AGRSH2, AGRSTM, AGRVG, AGRVG2,          !Output
     &  SDPROR,                                       !Output
     &  AGRSTR, FRSTR,                                !Output      
     &  SEASINIT)
      ENDIF

!-----------------------------------------------------------------------
      CALL FOR_GROW(CONTROL, ISWITCH, SEASINIT, SOILPROP, 
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

C-----------------------------------------------------------------------
C      09/23/05 SJR Added subroutine
C    Call Subroutine to calculate Nitrogen mobilized from natural 
C      senescence and light stress
C-----------------------------------------------------------------------
      CALL FOR_SENMOB(
     &    FILECC, CLW, DLAYR, DTX, DUL, DXR57, FNINL,           !Input
     &    FNINR, FNINS, FNINSR, ISWWAT, LL, NLAYR, NR5,         !Input 
     &    NR7, NSTRES, PAR, PCNL, PCNRT, PCNSR, PCNST,          !Input
     &    PPMFAC, RLV, RTWT, SAT, SLAAD, STMWT,                 !Input
     &    STRWT, SW, SWFAC, TDUMX, TDUMX2, VSTAGE, WCRLF,       !Input
     &    WCRRT,WCRSH, WCRSR, WCRST, WNRLF, WNRRT, WNRSH,       !Input
     &    WNRSR,WNRST, WTLF, XLAI, XPOD,                        !Input
     &    YRDOY, YRSIM,                                         !Input
     &    CMINELF, CMINEP, CMINERT, CMINESH, CMINESR,           !Output
     &    CMINEST, CMOBMX, CMOBSR, LAIMOBR, LFCMINE,            !Output
     &    LFSCMOB, LFSENWT, LFSNMOB, LTSEN, NMINELF,            !Output
     &    NMINEP, NMINERT, NMINESR, NMINEST, NMOBR,             !Output
     &    NMOBSR, PORPT, RLSEN, RTCMINE, RTSCMOB, RTSNMOB,      !Output
     &    SHCMINE, SHNMINE, SLDOT, SLMDOT, SRCMINE,             !Output
     &    SRDOT, SRMDOT, SRNDOT, SRSCMOB, SRSNMOB, SSMDOT,      !OutpuT
     &    SSNDOT, SSDOT, SSRDOT, SSRMDOT, SSRNDOT, STCMINE,     !Output
     &    STSCMOB, STSNMOB, STLTSEN, STSENWT, TSCMOB,           !Output
     &    TSNMOB, VNMOBR,                                       !Output
     &    SEASINIT)                                             !Control
!-----------------------------------------------------------------------
      IF (ISWNIT .EQ. 'Y') THEN
        CALL FOR_NUPTAK(
     &  BD, DLAYR, DUL, FILECC, LL, NDMSDR, NDMTOT, NH4,  !Input
     &  NO3, NLAYR, PGAVL, RLV, RNH4C, RNO3C, SAT, SW,    !Input
     &  NUPNH4, NUPNO3, TRNH4U, TRNO3U, TRNU, TSNMOB,     !Output
     &  UNH4, UNO3,                                       !Output
     &    SEASINIT)                                       !Control
 
      ENDIF

!-----------------------------------------------------------------------
      CALL FOR_MOBIL(
     &    LFSNMOB, RPRO, RTSNMOB, SRSNMOB, STSNMOB,       !Input
     &    TSNMOB, ANMINELF, ANMINERT, ANMINESH,           !Output
     &    ANMINESR, ANMINEST, NMINEA, NRUSLF,             !Output
     &    NRUSRT, NRUSSH, NRUSST, NRUSSR,                 !Output
     &    PNMLF, PNMST, PNMRT, PNMSR, PNMSH,              !Output
     &    SEASINIT)                                       !Control

!-----------------------------------------------------------------------
      !IF (ISWNIT .EQ. 'Y' .AND. ISWSYM .EQ. 'Y') THEN
        CALL FOR_NFIX(
     &    AGRNOD, CNODMN, CTONOD, DLAYR, DXR57,           !Input
     &    FILECC, FILEIO, NLAYR, NR7, PLTPOP,             !Input
     &    SAT, ST, SW, TURFAC, YRDOY, YRSIM,              !Input
     &    CNOD, DWNOD, DWNODA, NDTH, NFIXN,               !Output
     &    NODGR, WTNFX, SENNOD,                           !Output
     &    SEASINIT)
      !ENDIF

!-----------------------------------------------------------------------
      CALL FOR_PODS(SEASINIT, 
     &    AGRSD1, AGRSH1, DLAYR, DRPP, DUL, FILECC,       !Input
     &    FILEGC,FILEIO, FNINL, FNINSD, FNINSH, GDMSD,    !Input
     &    GRRAT1, ISWWAT, LL, NAVL, NDSET, NLAYR, NRUSSH, !Input
     &    NSTRES, PGAVL, PHTHRS, PHTIM, PNTIM, PUNCSD,    !Input
     &    PUNCTR, RNITP, SDDES, SDGR, SHELWT, SW, SWFAC,  !Input
     &    TDUMX, TGRO, TURADD, XFRT, YRDOY, YRNR1, YRNR2, !Input
     7    YRSIM,                                          !Input
     &    AGRSD3, LAGSD, LNGPEG, NGRSD, NGRSH, PCTMAT,    !Output
     &    PODNO, POTCAR, POTLIP, SDNO, SDVAR, SEEDNO,     !Output
     &    SHELN, SHVAR, WSDDTN, WSHDTN, WTABRT, WTSD,     !Output
     &    WTSHE, WTSHMT, FLWN)                            !Output

!-----------------------------------------------------------------------
        CALL FOR_VEGGR(
     &    AGRLF, AGRRT, AGRSTM, CADVG, CMINEP, CSAVEV,    !
     &    ECONO, FILECC, FILEGC, FNINL, FNINR, FNINS,     !
     &    LFCDEBT, LFCMINE, LFSCMOB, LFSNMOB, NAVL,       !
     &    NDMNEW, NDMOLD, PAR, PCH2O, PCNL, PCNST,        !Input
     &    PCNRT, PCNSR, PG, PGAVL, ROWSPC, RTCDEBT,       !
     &    RTCMINE, RTSCMOB, RTSNMOB, RTWT, RVSTGE,        !
     &    SHCMINE, SLDOT, SRCDEBT, SRCMINE, SRDOT,        !
     &    SRSCMOB, SRSNMOB, SSDOT, SSRDOT, STCDEBT,       !
     &    STCMINE, STMWT, STSCMOB, STSNMOB, TGRO, TSCMOB, !
     &    TSNMOB, TURFAC, VSTAGE, WCRLF, WCRRT, WCRST,    !
     &    WTLF, WTNLF, WTNRT, WTNSR, WTNST, XLAI,         !
     &    YRDOY, YREMRG, YRSIM,                           !
     &    AGRSTR, FNINSR, STRWT, WCRSR,                   !Input
     &    NLAYR, PROLFI, PRORTI, PROSTI, PROSRI,          !Input
     &    RNH4C, RNO3C, TRNH4U, TRNO3U, UNO3, UNH4,       !

     &    AGRVG, ANMINELF, ANMINERT, ANMINESR, ANMINEST,  !Input/Output
     &    FRLF, FRRT, FRSTM, FRSTR, NMINEA, NFIXN,        !
     &    NRUSLF, NRUSRT, NRUSSR, NRUSST, TRNU,           !

     &    ACMINELF, ACMINERT, ACMINESH, ACMINESR,         !
     &    ACMINEST, CADLF, CADST, CANHT, CANWH, CMINEA,   !
     &    CRUSLF, CRUSRT, CRUSSH, CRUSST, EXCESS, NADLF,  !
     &    NADRT, NADST, NGRLF, NGRRT, NGRST,              !Output
     &    NSTRES, TNLEAK, WLDOTN, WRDOTN, WSDOTN,         !
     &    PNMLF, PNMRT, PNMSH, PNMSR,PNMST,RPRO,          !
     &    CADRT, CADSH, NADSH, CADSR, CRUSSR, NADSR,      !Output
     &    NGRSR, WSRDOTN, CADSRF, CMOBSRN, CMOBSRX,       !Output
     &    FNINSRG, NGRSRG, PROSRG, PROSRT, CHORECOVER,    !Output
     &    NLKSPENT, NLKNUSED, NLKCHK, TNLKCHK,            !Output
     &    SEASINIT)                                       !Control

!-----------------------------------------------------------------------
C     Call leaf senescence routine for initialization
C-----------------------------------------------------------------------
!      CALL SENES(
!     &    FILECC, CADLF, CADSR, CADST, CLW, DAYL,DTX,        !Input
!     &    HPDAM, LFSNMOB, NADLF, NADSR, NADST, NR7,            !Input
!     &    NRUSLF, PAR, RHOL, SLAAD, SLMDOT, SSRMDOT,            !Input
!     &    SSMDOT, STMWT, SWFAC, VSTAGE, WTLF, XLAI,            !Input
!     &  YRDOY, YRSIM,                                                      !Input
!     &    LFSENWT, LTSEN, PORPT, SLDOT, SLNDOT, SLCADDOT, !Output
!     &  SLNADDOT, SSCADDOT, SSDOT, SSNADDOT, SSNDOT,      !Output
!     &  STRWT,                                                                  !Input
!     &  SSRCADDOT, SSRDOT, SSRNADDOT, SSRNDOT,                  !Output
!     &  STLTSEN, STSENWT,                                                !Output
!
!     &  SENSR,                                                                  !Output
!
!     &    SEASINIT)                                       !Control

C-----------------------------------------------------------------------
C     Call to root growth and rooting depth routine
C-----------------------------------------------------------------------
      !IF (ISWWAT .EQ. 'Y') THEN
      CALL FOR_ROOTS(SEASINIT,
     &    AGRRT, CROP, DLAYR, DS, DTX, DUL, FILECC,       !Input
     &    FILEIO, FRRT, ISWWAT, LL, NLAYR, PG,            !Input
     &    RLSEN, RO, RP, RTWT, SAT, SW,                   !Input
     &    SWFAC, VSTAGE, WR, WRDOTN, WTNEW,               !Input
     &    CUMDEP, RLV, RTDEP, SATFAC, SENRT,              !Output
     &    SRCADDOT, SRNADDOT)                             !Output
!ENDIF

      CALL FOR_OPMOB(CONTROL, ISWITCH, 
     &  YRPLT, MDATE, DAS, YRDOY, DTX, DXR57, PGAVL, NAVL, PG, PPMFAC, 
     &  NMOBR, NMOBSR, MAINR, ASMDOT, RSPNO3, RSPNH4, RPRO,       
     &  CNOD, CGRSD, CGRSH, CADVG, CSAVEV, AGRVG, PCH2O, WTLF, WLDOT, 
     &  WLIDOT, WLFDOT, WLDOTN, RTWT, WRDOT, WRDOTN, STMWT, WSDOT, 
     &  WSIDOT, WSFDOT, WSDOTN, 
     &  STRWT, WSRDOT, WSRIDOT, WSRFDOT, WSRDOTN,
     &  SLMDOT, LFSENWT, LTSEN, SLNDOT, SLCADDOT, SLNADDOT, 
     &  SLDOT, SRMDOT, SRNDOT, SRCADDOT, SRNADDOT, SRDOT, 
     &  SSMDOT, STSENWT, STLTSEN, SSNDOT, SSCADDOT, SSNADDOT, 
     &  SSDOT, SSRMDOT, SSRNDOT, SSRCADDOT, SSRNADDOT, SSRDOT,
     &  NDMVEG, NDMNEW, NDMOLD, NDMTOT, NMINEP, NMINEA, SDNPL, TRNU, 
     &  NFIXN, NGRSD, NGRSH, TSNMOB,
     &  WTNLF, WNRLF, LFSNMOB, NMINELF, ANMINELF, 
     &  NGRLF, NLALL, NADLF, NRUSLF, PCNL,
     &  WTNRT, WNRRT, RTSNMOB, NMINERT, ANMINERT, 
     &  NGRRT, NRALL, NADRT, NRUSRT, PCNRT,
     &  WTNST, WNRST, STSNMOB, NMINEST, ANMINEST, 
     &  NGRST, NSALL, NADST, NRUSST, PCNST,
     &  WTNSR, WNRSR, SRSNMOB, NMINESR, ANMINESR, 
     &  NGRSR, NSRALL, NADSR, NRUSSR, PCNSR,
     &  WTNSH, WNRSH, SHNMINE, NRUSSH,    
     &  TSCMOB, WCRLF, WRCLDT, LFSCMOB, CMINELF, CADLF, CRUSLF, RHOL,
     &  WCRRT, WRCRDT, RTSCMOB, CMINERT, CADRT, CRUSRT, RHOR,          
     &  WCRST, WRCSDT, STSCMOB, CMINEST, CADST, CRUSST, RHOS,     
     &  WCRSR, WRCSRDT, SRSCMOB, CMINESR, CADSR, CRUSSR, RHOSR,
     &  WCRSH, WRCSHD, SHCMINE, CRUSSH, CHORECOVER, NLKSPENT, NLKNUSED,
     &  NLKCHK, TNLKCHK, CMOBSR, LAIMOBR, VNMOBR)     

!-----------------------------------------------------------------------
!     Write headings to output file GROWTH.OUT
!-----------------------------------------------------------------------
      CALL FOR_OPGROW(CONTROL, ISWITCH, 
     &    CADLF, CADST, CANHT, CANWH, CMINEA, DWNOD,  
     &    GROWTH, GRWRES, MAINR, MDATE, NFIXN, NSTRES, 
     &    PCLSD, PCCSD, PCNL, PCNRT, PCNSD, PCNSH, PCNST, 
     &    PG, PODNO, PODWT, PODWTD, RHOL, RHOS, RLV, RSTAGE, 
     &    RTDEP, RTWT, SATFAC, SDWT, SEEDNO, SLA, STMWT, SWFAC, 
     &    TGRO, TGROAV, TOPWT, TOTWT, TURFAC, VSTAGE, WTCO, 
     &    WTLF, WTLO, WTNCAN, WTNLF, WTNST, WTNSD, WTNUP, 
     &    WTNFX, WTSO, XLAI, YRPLT,
     &    DRMST, PPGFAC, PPMFAC, PPTFAC, SRFTEMP, ST, FREEZ2,
     &    AGRSTR, CADSR, CMOBSR, CPFSTR, CRUSSR, CSRFRZ, CSRW, 
     &    CSTRM, DSTOR, FNINSR, FNINSRG, FRSTR, FRSTRM, NADSR, 
     &    NGRSR, NGRSRG, NMOBSR, NRUSSR, NSRALL, NSRDOT, NSROFF, 
     &    NVSTSR, PCNSR, PCSTRD, PROSRT, PSRSRFD, PSRLYRD, 
     &    PSRSRFL, PSRLYR1, RHOSR, SRDAM, SRSRFD, SRLYRD, SSRDOT, 
     &    SSRNDOT, STRWT, TPSRSRFL, TPSRLYR1, WCRSR, WNRSR, 
     &    WRCSRDT, WSRDOT, WSRDOTN, WSRFDOT, WSRI, WSRIDOT, 
     &    WTNSR, WTNSRA, WTNSRO, WTSRO, XSTR,
     &    FRLF, FRSTM, FRRT,
     &    FHWAH, FHLPH, DWTCO, DWTLO, DWTSO,fhpctn,RHOR)

! CALL FOR_OPHARV (CONTROL, ISWITCH, 
!&    CANHT, CANNAA, CANWAA, CROP, LAIMX, HARVFRAC,   !Input
!&    MDATE, NSTRES, PCLSD, PCNSD, PODNO, PODWT,      !Input
!&    SDRATE, SDWT, SDWTAM, SEEDNO, STGDOY, STMWT,    !Input
!&    TOPWT, TURFAC, VSTAGE, WTNCAN, WTNFX, WTNSD,    !Input
!&    WTNST, WTNUP, XLAI,                             !Input
!&    YRNR1, YRNR3, YRNR5, YRNR7, YRPLT,              !Input
!&    BWAH, SDWTAH)                                   !Output

! Zero the value of HARVRES composite variable here 
!!!NOTE: At this time, the variable has already been used to 
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
      IF (YRDOY .GT. YREMRG .AND. YREMRG .GT. 0 
     &    .AND. ISWWAT .EQ. 'Y') THEN
!       Calculate daily water stess factors (from SWFACS)
        SWFAC  = 1.0
        TURFAC = 1.0
        IF (EOP .GT. 0.0) THEN
        EP1 = EOP * 0.1
        IF (TRWUP / EP1 .LT. RWUEP1) THEN
        TURFAC = (1./RWUEP1) * TRWUP / EP1
        ENDIF
        IF (EP1 .GE. TRWUP) THEN
        SWFAC = TRWUP / EP1
        ENDIF
        ENDIF
      ENDIF

!-----------------------------------------------------------------------
!     CALL vegetative and reproductive development subroutine
!-----------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
        CALL FOR_PHENOL(CONTROL, ISWITCH, 
     &    DAYL, NSTRES, SOILPROP, ST, SW, SWFAC, TGRO,    !Input
     &    TGROAV, TMIN, TURFAC, XPOD, YRPLT,              !Input
     &    DRPP, DTX, DXR57, FRACDN, MDATE, NDLEAF,        !Output
     &    NDSET, NR1, NR2, NR5, NR7, NVEG0, PHTHRS,       !Output
     &    PHZACC, RSTAGE, RVSTGE, STGDOY, TDUMX, TDUMX2,  !Output
     &    VSTAGE,vstagp, YREMRG, YRNR1, YRNR2, YRNR3, YRNR5,     !Output
     &  YRNR7)                                        !Output
      ENDIF


C-----------------------------------------------------------------------
C     Call Dormancy module to determine today's
C     dormancy status and adjustment factors.
C-----------------------------------------------------------------------
      CALL FOR_DORMANCY( CONTROL,
     &    DAYL, TMIN,                                       !Input
     &    DRMST, FREEZ2, FRZDC, PPGFAC, PPTFAC, PPMFAC,     !Output

     &    FNPGD, FNPMD, FNPTD, FRZDHD, FRZHRD, HARD1,       !Output
     &    HARD2, RCHDP, RDRMG, RDRMM, RDRMT, TYPDHD, TYPHRD,!Output
     &    TYPPGD, TYPPMD, TYPPTD)                           !Output

!----------------------------------------------------------------------
      IF (ISWDIS .EQ. 'Y') THEN
        CALL FOR_PEST(CONTROL, ISWITCH, 
     &    AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, PGAVL,    !Input
     &    PHTIM, PLTPOP, RTWT, SLA, SLDOT, SOILPROP,      !Input
     &    SSDOT, STMWT, TOPWT, WLFDOT, WTLF, YRPLT,       !Input
     &    RLV, SDNO, SHELN, SWIDOT,                       !Input/Output
     &    VSTAGE, WSHIDT, WTSD, WTSHE,                    !Input/Output
     &    ASMDOT, DISLA, HPDAM, NPLTD, PPLTD,             !Output
     &    SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT,             !Output
     &    CSRW, SSRDOT, STRWT, WSFDOT, WSRFDOT,           !Input
     &    WSRIDOT,                                        !Output
     &    CSFRZ, CSRFRZ, CSTRM, DSTOR, SRDAM)             !Output

      ENDIF

      IF (CROP .NE. 'FA' .AND. DAS .GT. NVEG0) THEN
        IF (MEPHO .EQ. 'L') THEN
        !Retrieve AGEFAC and PG from ETPHOT routine.
        CALL GET('SPAM', 'AGEFAC', AGEFAC)
        CALL GET('SPAM', 'PG'    , PG)
        ELSEIF (MEPHO .EQ. 'C') THEN
        CALL FOR_PHOTO(CONTROL, 
     &    BETN, CO2, DXR57, EXCESS, NR5, PAR, SLPF,     !Input
     &    RNITP, SLAAD, SWFAC, TDAY, XHLAI, XPOD,       !Input
     &    AGEFAC, PG)                                   !Output
 
        ENDIF
      ENDIF
      call forage_harvest(CONTROL,FILECC, ATMOW, ATTP,
     &    RHOL,RHOS,PCNL,PCNST,SLA,RTWT,STRWT,       !Input
     &    WTLF,STMWT,TOPWT,TOTWT,WCRLF,WCRST,        !Input/Output
     &    WTNLF,WTNST,WNRLF,WNRST,WTNCAN,            !Input/Output
     &    AREALF,XLAI,XHLAI,VSTAGE,vstagp,canht,     !Input/Output
     &    FHWAH,FHTOTN, FHLPH,fhpctn,FREQ,
     &    MOWC,RSPLC,HMFRQ,HMGDD,HMCUT,HMMOW,HRSPL,
     &    DWTCO, DWTLO, DWTSO, PWTCO, PWTLO, PWTSO,
     &    HMVS, WTCO, WTLO, WTSO, TAVG, MOWGDD,
     &    MOWCOUNT, TGMIN, VTO1, VTB1, MOWREF, 
     &    RSREF, YFREQ, YRSREF, YCUTHT, YCHMOW,
     &    XCUTHT, XCHMOW, XFRGDD, XFREQ, CUTDAY,
     &    PROLFF, PROSTF, pliglf, pligst)

!***********************************************************************
!***********************************************************************
!     DAILY INTEGRATION 
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. INTEGR .AND. CROP .NE. 'FA') THEN
!***********************************************************************
!-----------------------------------------------------------------------
!     Move FOR_PHENOL integration up here.
!     Need to set NVEG0 before test for DAS = NVEG0, otherwise,
!     initialization on day of emergence will never occur.
!-----------------------------------------------------------------------
      CALL FOR_PHENOL(CONTROL, ISWITCH, 
     &    DAYL, NSTRES, SOILPROP, ST, SW, SWFAC, TGRO,    !Input
     &    TGROAV, TMIN, TURFAC, XPOD, YRPLT,              !Input
     &    DRPP, DTX, DXR57, FRACDN, MDATE, NDLEAF,        !Output
     &    NDSET, NR1, NR2, NR5, NR7, NVEG0, PHTHRS,       !Output
     &    PHZACC, RSTAGE, RVSTGE, STGDOY, TDUMX, TDUMX2,  !Output
     &    VSTAGE,vstagp, YREMRG, YRNR1, YRNR2, YRNR3, YRNR5,     !Output
     &    YRNR7)                                          !Output
C-----------------------------------------------------------------------
C     Compute adjustment factors for Dormancy processes
C-----------------------------------------------------------------------

      CALL FOR_DORMANCY( CONTROL,
     &    DAYL, TMIN,                                        !Input
     &    DRMST, FREEZ2, FRZDC, PPGFAC, PPTFAC, PPMFAC,      !Output

     &    FNPGD, FNPMD, FNPTD, FRZDHD, FRZHRD, HARD1,        !Output
     &    HARD2, RCHDP, RDRMG, RDRMM, RDRMT, TYPDHD, TYPHRD, !Output
     &    TYPPGD, TYPPMD, TYPPTD)                            !Output

!-----------------------------------------------------------------------
      IF (DAS .EQ. NVEG0) THEN
!----------------------------------------------------------------------
!     On day of emergence, initialize:
!-----------------------------------------------------------------------
      CALL FOR_DEMAND(EMERG, 
     &  AGRLF, AGRRT, AGRSH2, AGRSTM, CROP, DRPP, DXR57,  !Input
     &  FILECC, FILEGC, FILEIO, FNINSH, FRACDN, LAGSD,    !Input
     &  LNGPEG, NDLEAF, NMINEP, NSTRES,                   !Input
     &  PAR, PGAVL, PHZACC, PUNCSD,                       !Input
     &  PUNCTR, PLTPOP, RPROAV, RTWT,                     !Input
     &  SDDES, SDNO, SDVAR, SHELN, SHVAR, SLDOT, SRDOT,   !Input
     &  SSDOT, SSRDOT, STMWT,                             !Input
     &  TAVG, TDUMX, TGRO, TURFAC,                        !Input 
     &  VSTAGE, WCRLF, WCRRT, WCRST,                      !Input
     &  WTLF, WTNLF, WTNRT, WTNSR, WTNST, WTSD,           !Input
     &  WTSHE, YRDOY,                                     !Input
     &  NVEG0, NR1, NR2, NR7, YRSIM,                      !Input

     &  AGRSD1, AGRSD2, AGRVG, AGRVG2, CDMREP, F, FNINL,  !Output
     &  FNINR, FNINS, FNINSD, FRLF, FRRT, FRSTM, GDMSD,   !Output
     &  GRRAT1, NDMNEW, NDMOLD, NDMREP, NDMSDR, NDMTOT,   !Output
     &  NDMVEG, PHTIM, PNTIM, POTCAR,                     !Output
     &  POTLIP, SDGR, TURADD, XFRT,                       !Output
     &  PPTFAC, STRWT,                                    !Input?
     &  WCRSR,                                            !Input?
     &  AGRSTR, FNINSR, FRSTR,                            !Output
     &  FRSTRF, FRSTRM, FRSTRMX, LRMOB,                   !Output
     &  NMOBSRN, NMOBSRX, NRMOB, NVSTL, NVSTR, NVSTS,     !Output
     &  NVSTSR, TYPLMOB, TYPNMOB, XSTR, YSTOR)            !Output

!-----------------------------------------------------------------------
      CALL FOR_GROW(CONTROL, ISWITCH, EMERG, SOILPROP, 
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

C-----------------------------------------------------------------------
C     Call to root growth and rooting depth routine
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C      2/21/05 - SJR - move conditional call for water stress from CROPGRO 
C      to FOR_ROOTS.  Allows root senescence when Water dynamics option is 
C      turned off.  Water stress options set to no stress levels. 
C-----------------------------------------------------------------------
!        IF (ISWWAT .EQ. 'Y') THEN
      CALL FOR_ROOTS(EMERG,
     &    AGRRT, CROP, DLAYR, DS, DTX, DUL, FILECC,       !Input
     &    FILEIO, FRRT, ISWWAT, LL, NLAYR, PG,            !Input
     &    RLSEN, RO, RP, RTWT, SAT, SW,                   !Input
     &    SWFAC, VSTAGE, WR, WRDOTN, WTNEW,               !Input
     &    CUMDEP, RLV, RTDEP, SATFAC, SENRT,              !Output
     &    SRCADDOT, SRNADDOT)                             !Output
!        ENDIF

!-----------------------------------------------------------------------
        CALL FOR_PODS(EMERG, 
     &    AGRSD1, AGRSH1, DLAYR, DRPP, DUL, FILECC,       !Input
     &    FILEGC,FILEIO, FNINL, FNINSD, FNINSH, GDMSD,    !Input
     &    GRRAT1, ISWWAT, LL, NAVL, NDSET, NLAYR, NRUSSH, !Input
     &    NSTRES, PGAVL, PHTHRS, PHTIM, PNTIM, PUNCSD,    !Input
     &    PUNCTR, RNITP, SDDES, SDGR, SHELWT, SW, SWFAC,  !Input
     &    TDUMX, TGRO, TURADD, XFRT, YRDOY, YRNR1, YRNR2, !Input
     7    YRSIM,                                          !Input
     &    AGRSD3, LAGSD, LNGPEG, NGRSD, NGRSH, PCTMAT,    !Output
     &    PODNO, POTCAR, POTLIP, SDNO, SDVAR, SEEDNO,     !Output
     &    SHELN, SHVAR, WSDDTN, WSHDTN, WTABRT, WTSD,     !Output
     &    WTSHE, WTSHMT, FLWN)                            !Output

!-----------------------------------------------------------------------
        CALL FOR_VEGGR(
     &    AGRLF, AGRRT, AGRSTM, CADVG, CMINEP, CSAVEV,    !
     &    ECONO, FILECC, FILEGC, FNINL, FNINR, FNINS,     !
     &    LFCDEBT, LFCMINE, LFSCMOB, LFSNMOB, NAVL,       !
     &    NDMNEW, NDMOLD, PAR, PCH2O, PCNL, PCNST,        !Input
     &    PCNRT, PCNSR, PG, PGAVL, ROWSPC, RTCDEBT,       !
     &    RTCMINE, RTSCMOB, RTSNMOB, RTWT, RVSTGE,        !
     &    SHCMINE, SLDOT, SRCDEBT, SRCMINE, SRDOT,        !
     &    SRSCMOB, SRSNMOB, SSDOT, SSRDOT, STCDEBT,       !
     &    STCMINE, STMWT, STSCMOB, STSNMOB, TGRO, TSCMOB, !
     &    TSNMOB, TURFAC, VSTAGE, WCRLF, WCRRT, WCRST,    !
     &    WTLF, WTNLF, WTNRT, WTNSR, WTNST, XLAI,         !
     &    YRDOY, YREMRG, YRSIM,                           !
     &    AGRSTR, FNINSR, STRWT, WCRSR,                   !Input
     &    NLAYR, PROLFI, PRORTI, PROSTI, PROSRI,          !Input
     &    RNH4C, RNO3C, TRNH4U, TRNO3U, UNO3, UNH4,       !

     &    AGRVG, ANMINELF, ANMINERT, ANMINESR, ANMINEST,  !Input/Output
     &    FRLF, FRRT, FRSTM, FRSTR, NMINEA, NFIXN,        !
     &    NRUSLF, NRUSRT, NRUSSR, NRUSST, TRNU,           !

     &    ACMINELF, ACMINERT, ACMINESH, ACMINESR,         !
     &    ACMINEST, CADLF, CADST, CANHT, CANWH, CMINEA,   !
     &    CRUSLF, CRUSRT, CRUSSH, CRUSST, EXCESS, NADLF,  !
     &    NADRT, NADST, NGRLF, NGRRT, NGRST,              !Output
     &    NSTRES, TNLEAK, WLDOTN, WRDOTN, WSDOTN,         !
     &    PNMLF, PNMRT, PNMSH, PNMSR,PNMST,RPRO,          !
     &    CADRT, CADSH, NADSH, CADSR, CRUSSR, NADSR,      !Output
     &    NGRSR, WSRDOTN, CADSRF, CMOBSRN, CMOBSRX,       !Output
     &    FNINSRG, NGRSRG, PROSRG, PROSRT, CHORECOVER,    !Output
     &    NLKSPENT, NLKNUSED, NLKCHK, TNLKCHK,            !Output
     &    EMERG)                                          !Control
      ENDIF

!***********************************************************************
C     Skip growth processes and N and C balances before plants emerge
C-----------------------------------------------------------------------
      IF (DAS .GE. NVEG0) THEN
!-----------------------------------------------------------------------
!     Initialize available N and C for beginning of daily calcs.
!-----------------------------------------------------------------------
      NAVL = 0.0
      PGAVL = 0.0
!      WLFDOT = 0.0
!      WSRFDOT = 0.0
!-----------------------------------------------------------------------
!     Reduce daily photosynthesis for soil water stress
!     This has been moved to photosynthesis routines
!-----------------------------------------------------------------------
!      IF (MEEVP .NE. 'Z') THEN
!        PG = PG * SWFAC
!        PGNOON = PGNOON * SWFAC
!      ENDIF

C-----------------------------------------------------------------------
C    Reduce daily photosynthesis for Dormancy effect
        PG = PG * PPGFAC

C-----------------------------------------------------------------------
C    Initialize variables that represent N and C availability during a day
C    Assume that Fraction CMOBMX of CH2O can be Mobilized per Day
C    PGAVL is the total available CH2O available for growth & respiration
C
C    8/26/97 KJB  DTX IN PLACE OF 1 TO SLOW IT DOWN A BIT AT ALL TIMES
C    AND TO BE SENSITIVE TO TEMPERATURE PRIOR TO R5 STAGE, BUT
C    STILL WANT THE SPEED-UP CAUSED BY THE "+ DXR57" FEATURE AFTER R5.
C
C-----------------------------------------------------------------------
C DSSAT4 code for CMINEP
!      CMINEP = CMOBMX * (DTX + DXR57) * (WCRST + WCRRT + WCRSH +WCRLF)


C-----------------------------------------------------------------------
C      09/23/05 SJR Added subroutine
C    Call Subroutine to calculate Nitrogen mobilized from natural 
C      senescence and light stress
C-----------------------------------------------------------------------

      CALL FOR_SENMOB(
     &    FILECC, CLW, DLAYR, DTX, DUL, DXR57, FNINL,           !Input
     &    FNINR, FNINS, FNINSR, ISWWAT, LL, NLAYR, NR5,         !Input 
     &    NR7, NSTRES, PAR, PCNL, PCNRT, PCNSR, PCNST,          !Input
     &    PPMFAC, RLV, RTWT, SAT, SLAAD, STMWT,                 !Input
     &    STRWT, SW, SWFAC, TDUMX, TDUMX2, VSTAGE, WCRLF,       !Input
     &    WCRRT,WCRSH, WCRSR, WCRST, WNRLF, WNRRT, WNRSH,       !Input
     &    WNRSR,WNRST, WTLF, XLAI, XPOD,                        !Input
     &    YRDOY, YRSIM,                                         !Input
     &    CMINELF, CMINEP, CMINERT, CMINESH, CMINESR,           !Output
     &    CMINEST, CMOBMX, CMOBSR, LAIMOBR, LFCMINE,            !Output
     &    LFSCMOB, LFSENWT, LFSNMOB, LTSEN, NMINELF,            !Output
     &    NMINEP, NMINERT, NMINESR, NMINEST, NMOBR,             !Output
     &    NMOBSR, PORPT, RLSEN, RTCMINE, RTSCMOB, RTSNMOB,      !Output
     &    SHCMINE, SHNMINE, SLDOT, SLMDOT, SRCMINE,             !Output
     &    SRDOT, SRMDOT, SRNDOT, SRSCMOB, SRSNMOB, SSMDOT,      !OutpuT
     &    SSNDOT, SSDOT, SSRDOT, SSRMDOT, SSRNDOT, STCMINE,     !Output
     &    STSCMOB, STSNMOB, STLTSEN, STSENWT, TSCMOB,           !Output
     &    TSNMOB, VNMOBR,                                       !Output
     &    INTEGR)                                              !Control

C-----------------------------------------------------------------------
C New code to deal with forages and dormancy
C Boosts mobilization from storage organs after harvests (LAI<CLAIT)
C Lowers mobilization during dormancy (when PPMFAC>0.0)
C-----------------------------------------------------------------------
!      IF (XLAI .LE. CLAIT) THEN
!      CMOBSR=CMOBSRX*(PPMFAC)
!      NMOBSR=NMOBSRX*(PPMFAC)
!      ELSE
!      CMOBSR=CMOBSRN*(PPMFAC)
!      NMOBSR=NMOBSRN*(PPMFAC)
!      ENDIF


!      LAIMOBR = CURV(TYPLMOB,LRMOB(1),LRMOB(2),LRMOB(3),
!     &         LRMOB(4), MIN(XLAI,LRMOB(4)))

C-----------------------------------------------------------------------
C      Increase mobilization from storage if N status of plant is high.
C-----------------------------------------------------------------------

!      VEGNCNT = PCNL/100*WTLF + PCNST/100*STMWT +  
!     &              PCNRT/100*RTWT + PCNSR/100*STRWT
!      VEGNCMX = FNINL*WTLF + FNINS*STMWT + FNINR*RTWT + FNINSR*STRWT
!      VNSTAT = MIN((VEGNCNT / VEGNCMX),  1.0)
      
!      VNMOBR = CURV(TYPNMOB,NRMOB(1),NRMOB(2),NRMOB(3),NRMOB(4),VNSTAT)
C-----------------------------------------------------------------------
C      Set N mobilization rate from storage
C      Default to NMOBSRN under most conditions
C      set to NMOBSRX (max rate) after harvest or severe damage
C      Reduce from either level depending on degree of dormancy
C      Mobilization from storage is unaffected by water or N stress
C      but is accelerated by low N status.
C-----------------------------------------------------------------------
!      NMOBSR = (NMOBSRN + (NMOBSRX-NMOBSRN)*VNMOBR)
!      NMOBSR = (NMOBSR + (NMOBSRX-NMOBSR)*LAIMOBR)*PPMFAC
C-----------------------------------------------------------------------
C      Set C mobilization rate from storage
C      Default to CMOBSRN under most conditions
C      set to CMOBSRX (max rate) after harvest or severe damage
C      Reduce from either level depending on degree of dormancy
C      Mobilization from storage is unaffected by water or N stress
C-----------------------------------------------------------------------
!      CMOBSR = (CMOBSRN + (CMOBSRX-CMOBSRN)*VNMOBR)
!      CMOBSR = (CMOBSR + (CMOBSRX-CMOBSR)*LAIMOBR)*PPMFAC
!      CMOBSR = (CMOBSRN + (CMOBSRX-CMOBSRN)*LAIMOBR)*PPMFAC

!      CMINEP= CMOBMX * (DTX + DXR57) * (WCRLF - WTLF * PCHOLFF) +
!     &        CMOBMX * (DTX + DXR57) * (WCRST - STMWT * PCHOSTF) +
!     &        CMOBMX * (DTX + DXR57) * WCRSH +
!     &        CMOBMX * (DTX+DXR57)*(PPMFAC)*(WCRRT - RTWT * PCHORTF) +
!     &        CMOBSR * (DTX+DXR57)*(WCRSR - STRWT * PCHOSRF)

      PGAVL = PG + CMINEP
C-----------------------------------------------------------------------
C       Compute maintenance respiration and subtract from available CH2O
C-----------------------------------------------------------------------
      CALL FOR_RESPIR(
     &    PG, R30C2, RES30C, TGRO, WTMAIN,                !Input
     &    RO, RP,                                         !Input/Output
     &    MAINR,                                          !Output
     &    LFMRC, mft, MRSWITCH, RTMRC, SDMRC, SHELMRC,    !Input
     &    STMMRC, STRMRC, TRSWITCH, TRST, TRSTYP, WTNLF,  !Input
     &  WTNRT, WTNSD, WTNSH, WTNSR, WTNST)            !Input

      IF (MAINR .GT. PGAVL) THEN
        PGAVL  = 0.0
      ELSE
        PGAVL = PGAVL - MAINR
      ENDIF
!-----------------------------------------------------------------------
!     Reduce PGAVL if pest damage occurs to C assimilation
!     Moved to PEST module - chp
!-----------------------------------------------------------------------
!     Call PEST Module for INTEGRATION calculations
!-----------------------------------------------------------------------
      IF (ISWDIS.EQ.'Y') THEN
        CALL FOR_PEST(CONTROL, ISWITCH, 
     &    AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, PGAVL,    !Input
     &    PHTIM, PLTPOP, RTWT, SLA, SLDOT, SOILPROP,      !Input
     &    SSDOT, STMWT, TOPWT, WLFDOT, WTLF, YRPLT,       !Input
     &    RLV, SDNO, SHELN, SWIDOT,                       !Input/Output
     &    VSTAGE, WSHIDT, WTSD, WTSHE,                    !Input/Output
     &    ASMDOT, DISLA, HPDAM, NPLTD, PPLTD,             !Output
     &    SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT,             !Output
     &  CSRW, SSRDOT, STRWT, WSFDOT, WSRFDOT,         !Input
     &  WSRIDOT,                                      !Output

     &  CSFRZ, CSRFRZ, CSTRM, DSTOR, SRDAM)           !Output

        IF (ASMDOT .GT. 0.0) THEN
        PGAVL = PGAVL - ASMDOT
        PGAVL = MAX(0.0,PGAVL)
        ENDIF

      ENDIF

!-----------------------------------------------------------------------
C     5/23/05 SJR Add call to CH2O Refill Module for routine refilling
C      of mobile CH2O during vegetaive stages
C      10/30/05 SJR - Move routine here.  Had been just prior to FOR_VEGGR
C      because that is where CSAVEV had been in CROPGRO.  Problem was
C      that we calculate DEMAND on PG+CMINEP-MAINR then reduce PGAVL by 
C      CSAVEV.  This results in slightly lower growth in FOR_VEGGR with
C      a correspondingly lower N requirement - leaving NLEAK.  This 
C      takes cake of NLEAK.
C      10/31/05 Change output variable name from CSAVEV to CADVG to 
C      to prevent confusion with CH2O reserved for refill after NR1
C      and used for later seed development.
!-----------------------------------------------------------------------      


      IF (DAS .LT. NR1) THEN
        CALL FOR_CH2OREF(CONTROL,
     &  ALPHL, ALPHR, ALPHS, ALPHSR, CADPV, CRREF,        !INPUT
     &  LFSCMOB, LRREF, PG, PRREF, RTSCMOB,               !INPUT
     &  RTWT, SLDOT, SRDOT, SRSCMOB,                      !INPUT
     &  SSDOT, SSRDOT, STMWT, STRWT, STSCMOB,             !INPUT
     &  TYPCREF, TYPLREF, TYPPREF, WCRLF, WCRRT,          !INPUT
     &  WCRSR, WCRST,                                     !INPUT
     &  WTLF, XLAI,                                       !INPUT
     &  CDEBIT, CADVG, PGAVL,                             !INPUT/OUTPUT
     &  LFCDEBT, RTCDEBT, SRCDEBT, STCDEBT )              !OUTPUT

      ELSE 
        CADVG = 0.0
      ENDIF
      PGAVL = PGAVL - CADVG 

        CSAVEV = CADPR1  * FRACDN* MIN(PG,PGAVL)
!-----------------------------------------------------------------------
C     CSAVEV is a faction of PG for vegetative growth that is stored
C     as CH2O.  Increase this as plant moves from R1 into seed fill.
!  These two statements came from the FOR_VEGGR subroutine - chp
C-----------------------------------------------------------------------

      PGAVL = PGAVL - CSAVEV

C-----------------------------------------------------------------------
C    Call Subroutine to calculate Nitrogen and Carbon Demand for new growth
C-----------------------------------------------------------------------
      CALL FOR_DEMAND(INTEGR, 
     &  AGRLF, AGRRT, AGRSH2, AGRSTM, CROP, DRPP, DXR57,  !Input
     &  FILECC, FILEGC, FILEIO, FNINSH, FRACDN, LAGSD,    !Input
     &  LNGPEG, NDLEAF, NMINEP, NSTRES,                   !Input
     &  PAR, PGAVL, PHZACC, PUNCSD,                       !Input
     &  PUNCTR, PLTPOP, RPROAV, RTWT,                     !Input
     &  SDDES, SDNO, SDVAR, SHELN, SHVAR, SLDOT, SRDOT,   !Input
     &  SSDOT, SSRDOT, STMWT,                             !Input
     &  TAVG, TDUMX, TGRO, TURFAC,                        !Input 
     &  VSTAGE, WCRLF, WCRRT, WCRST,                      !Input
     &  WTLF, WTNLF, WTNRT, WTNSR, WTNST, WTSD,           !Input
     &  WTSHE, YRDOY,                                     !Input
     &  NVEG0, NR1, NR2, NR7, YRSIM,                      !Input

     &  AGRSD1, AGRSD2, AGRVG, AGRVG2, CDMREP, F, FNINL,  !Output
     &  FNINR, FNINS, FNINSD, FRLF, FRRT, FRSTM, GDMSD,   !Output
     &  GRRAT1, NDMNEW, NDMOLD, NDMREP, NDMSDR, NDMTOT,   !Output
     &  NDMVEG, PHTIM, PNTIM, POTCAR,                     !Output
     &  POTLIP, SDGR, TURADD, XFRT,                       !Output
     &  PPTFAC, STRWT,                                    !Input?
     &  WCRSR,                                            !Input?
     &  AGRSTR, FNINSR, FRSTR,                            !Output
     &  FRSTRF, FRSTRM, FRSTRMX, LRMOB,                   !Output
     &  NMOBSRN, NMOBSRX, NRMOB, NVSTL, NVSTR, NVSTS,     !Output
     &  NVSTSR, TYPLMOB, TYPNMOB, XSTR, YSTOR)            !Output

C-----------------------------------------------------------------------
C    Compute N Available From Seed, During Early Growth
!     chp - this takes much longer than 7 ptd to deplete seed N.
C-----------------------------------------------------------------------
      IF (SDNPL .GT. 0.0001) THEN
        NAVL = MAX(SDNPL * DTX / 7. , 0.0)
        SDNPL = SDNPL - NAVL
      ELSE
        SDNPL = 0.0
        NAVL = 0.0
      ENDIF
        
C-----------------------------------------------------------------------
C    If ISWNIT = Y - Call soil N routines. Balance Available C and N
C    If ISWNIT = N - Do not call soil N routines, N assumed to be limited by C
C-----------------------------------------------------------------------
      IF (ISWNIT .EQ. 'Y') THEN
        CALL FOR_NUPTAK(
     &  BD, DLAYR, DUL, FILECC, LL, NDMSDR, NDMTOT, NH4,  !Input
     &  NO3, NLAYR, PGAVL, RLV, RNH4C, RNO3C, SAT, SW,    !Input
     &  NUPNH4, NUPNO3, TRNH4U, TRNO3U, TRNU, TSNMOB,     !Output
     &  UNH4, UNO3,                                       !Output
     &    INTEGR)                                         !Control

C-----------------------------------------------------------------------
C    Account for C Used to reduce N Uptake to protein
C-----------------------------------------------------------------------
        RSPNO3 = TRNO3U/0.16 * RNO3C
        RSPNH4 = TRNH4U/0.16 * RNH4C
        IF (PGAVL .LT. (RSPNO3+RSPNH4)) THEN
        PGAVL = 0.0
        ELSE
        PGAVL = PGAVL - (RSPNO3 + RSPNH4)
        ENDIF
C-----------------------------------------------------------------------
C       Accumulate nitrogen for today's growth, NAVL
C-----------------------------------------------------------------------
        NAVL = NAVL + TRNU
      ENDIF
C-----------------------------------------------------------------------
C    CALL Nitrogen mobilization subroutine
C    to compute availability of N from other tissue (NMINEA)
C-----------------------------------------------------------------------
      CALL FOR_MOBIL(
     &    LFSNMOB, RPRO, RTSNMOB, SRSNMOB, STSNMOB,       !Input
     &    TSNMOB, ANMINELF, ANMINERT, ANMINESH,           !Output
     &    ANMINESR, ANMINEST, NMINEA, NRUSLF,             !Output
     &    NRUSRT, NRUSSH, NRUSST, NRUSSR,                 !Output
     &    PNMLF, PNMST, PNMRT, PNMSR, PNMSH,              !Output
     &    INTEGR)                                         !Control
C-----------------------------------------------------------------------
C    Accumulate NAVL for growth, reduce PGAVL by protein re-synthesis cost
C-----------------------------------------------------------------------
      IF (PGAVL .GT. NMINEA/0.16*RPRO) THEN
        PGAVL = PGAVL - NMINEA/0.16*RPRO
      ELSE
        PGAVL = 0.0
      ENDIF
      NAVL = NAVL + NMINEA
C-----------------------------------------------------------------------
C     Allow some of today's PG to be used for N fixation, depending
C     on N uptake and mining, and on demand for N.
C     NAVLV = N available for veg growth from uptake and mining
C     CAVVEG = C available for veg growth
C     NDMVEG = N required for veg growth if all PGAVL is used as computed
C     CNDFX = carbon needed to fix N needed but not supplied by uptake or mining
C     PROVEG = average protein composition of growing tissue today
C     CTONOD = C to allocate to nodules to fix N needed for Rep and Veg growth
C-----------------------------------------------------------------------
      CTONODR = MAX(0.0, (NDMREP-NAVL)*RFIXN/0.16)
      CTONODR = MIN(CTONODR,PGAVL)
      CTONOD = 0.0
      CAVVEG = MAX(0.,(PGAVL - CDMREP))
      NAVLV = MAX(0.,(NAVL-NDMREP))
      CNDFX = MAX(0.,(RFIXN/0.16)*(NDMVEG-NAVLV))
      IF(CAVVEG .GT. 0.0 .AND. CNDFX .GT. 0.0) THEN
        PROVEG = PROLFI * FRLF + PRORTI * FRRT + PROSTI * FRSTM
        CTONOD = CAVVEG - (CAVVEG +
     &    (NAVLV*RFIXN/0.16))*AGRVG/(AGRVG+PROVEG*RFIXN)
      ENDIF

C-----------------------------------------------------------------------
C     Reserve for nodule growth an amount of C equivalent to a fixed
C     fraction (FRCNOD) of C allocated to root growth.  JWH 7/11/95
C-----------------------------------------------------------------------
      IF (DAS .LT. NR2) THEN
        CNODMN = CAVVEG * FRRT * FRCNOD
      ELSE
        CNODMN = 0.0
      END IF
      CTONOD = MIN(CNODMN + MAX(0.0, CTONOD), CAVVEG) + CTONODR

C-----------------------------------------------------------------------
C     Call nitrogen fixation routine if ISWSYM is set to Y
C     and if thermal time exceeds the lag phase for n-fixation
C-----------------------------------------------------------------------
      IF (ISWNIT .EQ. 'Y' .AND. ISWSYM .EQ. 'Y') THEN
        IF (VSTAGE .GT. TTFIX) THEN
        CALL FOR_NFIX(
     &  AGRNOD, CNODMN, CTONOD, DLAYR, DXR57,         !Input
     &  FILECC, FILEIO, NLAYR, NR7, PLTPOP,           !Input
     &  SAT, ST, SW, TURFAC, YRDOY, YRSIM,            !Input
     &  CNOD, DWNOD, DWNODA, NDTH, NFIXN,             !Output
     &  NODGR, WTNFX, SENNOD,                         !Output
     &  INTEGR)  
        ENDIF
      ENDIF
C-----------------------------------------------------------------------
C       If ISWSYM = U, then N-FIXATION is assumed to occur at a rate
C       that carbon will allow, and nodules are not grown explicitely
C-----------------------------------------------------------------------
      IF ((ISWNIT .EQ. 'Y') .AND. (ISWSYM .EQ. 'U') .OR.
     &   (ISWNIT .NE. 'Y')) THEN
        NFIXN = MAX(0.0,NDMREP + NDMVEG - NAVL)
        CNOD = RFIXN * NFIXN/0.16
      ENDIF
C-----------------------------------------------------------------------
C    Accumulate NAVL for growth, reduce PGAVL by cost to fix N
C-----------------------------------------------------------------------
      IF (PGAVL .GT. CNOD) THEN
        PGAVL = PGAVL - CNOD
      ELSE
        PGAVL = 0.0
      ENDIF
      NAVL = NAVL + NFIXN
C-----------------------------------------------------------------------
C     Call routine to compute actual seed and shell growth
C-----------------------------------------------------------------------
      CALL FOR_PODS(INTEGR, 
     &    AGRSD1, AGRSH1, DLAYR, DRPP, DUL, FILECC,       !Input
     &    FILEGC,FILEIO, FNINL, FNINSD, FNINSH, GDMSD,    !Input
     &    GRRAT1, ISWWAT, LL, NAVL, NDSET, NLAYR, NRUSSH, !Input
     &    NSTRES, PGAVL, PHTHRS, PHTIM, PNTIM, PUNCSD,    !Input
     &    PUNCTR, RNITP, SDDES, SDGR, SHELWT, SW, SWFAC,  !Input
     &    TDUMX, TGRO, TURADD, XFRT, YRDOY, YRNR1, YRNR2, !Input
     7    YRSIM,                                          !Input
     &    AGRSD3, LAGSD, LNGPEG, NGRSD, NGRSH, PCTMAT,    !Output
     &    PODNO, POTCAR, POTLIP, SDNO, SDVAR, SEEDNO,     !Output
     &    SHELN, SHVAR, WSDDTN, WSHDTN, WTABRT, WTSD,     !Output
     &    WTSHE, WTSHMT, FLWN)                            !Output

C-----------------------------------------------------------------------
C     Call specific routines for peanut to determine
C         Seed size
C         Pod color
C         Pod Detachment
C-----------------------------------------------------------------------
      IF (DETACH .EQ. 'Y' .AND. DAS .GE. NR1) THEN
        CALL FOR_PODDET(
     &    FILECC, TGRO, WTLF, YRDOY, YRNR2,               !Input
     &    PODWTD, SDNO, SHELN, SWIDOT,                    !Output
     &    WSHIDT, WTSD, WTSHE,                            !Output
     &    INTEGR)                                         !Control
      ENDIF

C-----------------------------------------------------------------------
C     Compute carbon required for seed (CGRSD) and shell (CGRSH) growth
C-----------------------------------------------------------------------
      CGRSD = WSDDTN * AGRSD3
      CGRSH = WSHDTN * AGRSH1
C-----------------------------------------------------------------------
C     Reduce PGAVL by C used for seed growth
C     Also reduce NAVL by N used for seed growth
C-----------------------------------------------------------------------
      IF (PGAVL .GT. (CGRSD + CGRSH)) THEN
        PGAVL = PGAVL - CGRSD - CGRSH
      ELSE
        PGAVL = 0.0
      ENDIF
      NAVL   = NAVL - (NGRSD + NGRSH)
      PGAVL  = MAX(0.0,PGAVL)
      NAVL   = MAX(0.0,NAVL)

C-----------------------------------------------------------------------
C     Call routine to compute actual vegetative growth, C to mine or add
C-----------------------------------------------------------------------
        CALL FOR_VEGGR(
     &    AGRLF, AGRRT, AGRSTM, CADVG, CMINEP, CSAVEV,    !
     &    ECONO, FILECC, FILEGC, FNINL, FNINR, FNINS,     !
     &    LFCDEBT, LFCMINE, LFSCMOB, LFSNMOB, NAVL,       !
     &    NDMNEW, NDMOLD, PAR, PCH2O, PCNL, PCNST,        !Input
     &    PCNRT, PCNSR, PG, PGAVL, ROWSPC, RTCDEBT,       !
     &    RTCMINE, RTSCMOB, RTSNMOB, RTWT, RVSTGE,        !
     &    SHCMINE, SLDOT, SRCDEBT, SRCMINE, SRDOT,        !
     &    SRSCMOB, SRSNMOB, SSDOT, SSRDOT, STCDEBT,       !
     &    STCMINE, STMWT, STSCMOB, STSNMOB, TGRO, TSCMOB, !
     &    TSNMOB, TURFAC, VSTAGE, WCRLF, WCRRT, WCRST,    !
     &    WTLF, WTNLF, WTNRT, WTNSR, WTNST, XLAI,         !
     &    YRDOY, YREMRG, YRSIM,                           !
     &    AGRSTR, FNINSR, STRWT, WCRSR,                   !Input
     &    NLAYR, PROLFI, PRORTI, PROSTI, PROSRI,          !Input
     &    RNH4C, RNO3C, TRNH4U, TRNO3U, UNO3, UNH4,       !

     &    AGRVG, ANMINELF, ANMINERT, ANMINESR, ANMINEST,  !Input/Output
     &    FRLF, FRRT, FRSTM, FRSTR, NMINEA, NFIXN,        !
     &    NRUSLF, NRUSRT, NRUSSR, NRUSST, TRNU,           !

     &    ACMINELF, ACMINERT, ACMINESH, ACMINESR,         !
     &    ACMINEST, CADLF, CADST, CANHT, CANWH, CMINEA,   !
     &    CRUSLF, CRUSRT, CRUSSH, CRUSST, EXCESS, NADLF,  !
     &    NADRT, NADST, NGRLF, NGRRT, NGRST,              !Output
     &    NSTRES, TNLEAK, WLDOTN, WRDOTN, WSDOTN,         !
     &    PNMLF, PNMRT, PNMSH, PNMSR,PNMST,RPRO,          !
     &    CADRT, CADSH, NADSH, CADSR, CRUSSR, NADSR,      !Output
     &    NGRSR, WSRDOTN, CADSRF, CMOBSRN, CMOBSRX,       !Output
     &    FNINSRG, NGRSRG, PROSRG, PROSRT, CHORECOVER,    !Output
     &    NLKSPENT, NLKNUSED, NLKCHK, TNLKCHK,            !Output
     &    INTEGR)                                         !Control
C-----------------------------------------------------------------------
C     Compute C required for LF, ST, and RT growth, and remaining C and N
C-----------------------------------------------------------------------
C DSSAT4 code
!      PGAVL = PGAVL - AGRVG * (WLDOTN + WSDOTN + WRDOTN)
C-----------------------------------------------------------------------
C Deduct cost of new storage tissue from PGAVL and NAVL
C-----------------------------------------------------------------------
      PGAVL = PGAVL + CHORECOVER - AGRVG * (WLDOTN + WSDOTN + WRDOTN 
     &    + WSRDOTN) - (CMINEP - CMINEA)
      NAVL = NAVL - NLKSPENT - (NGRLF + NGRST + NGRRT + NGRSR)
      NAVL = NAVL - (NADLF + NADST + NADRT + NADSR)
      PGAVL = PGAVL - (CADST + CADLF + CADSR + CADRT) * PCH2O + 
     &    CADVG + CSAVEV
C-----------------------------------------------------------------------
C     Call leaf senescence routine to compute leaf loss variables
C-----------------------------------------------------------------------
!      CALL SENES(
!     &    FILECC, CADLF, CADSR, CADST, CLW, DAYL,DTX,        !Input
!     &    HPDAM, LFSNMOB, NADLF, NADSR, NADST, NR7,            !Input
!     &    NRUSLF, PAR, RHOL, SLAAD, SLMDOT, SSRMDOT,            !Input
!     &    SSMDOT, STMWT, SWFAC, VSTAGE, WTLF, XLAI,            !Input
!     &  YRDOY, YRSIM,                                                      !Input
!     &    LFSENWT, LTSEN, PORPT, SLDOT, SLNDOT, SLCADDOT, !Output
!     &  SLNADDOT, SSCADDOT, SSDOT, SSNADDOT, SSNDOT,      !Output
!     &  STRWT,                                                                  !Input
!     &  SSRCADDOT, SSRDOT, SSRNADDOT, SSRNDOT,                  !Output
!     &  STLTSEN, STSENWT,                                                !Output
!
!     &  SENSR,                                                                  !Output
!
!     &    INTEGR)                                         !Control
C-----------------------------------------------------------------------
C     Call freeze damage routine if TMIN is less than FREEZ1 deg C
C-----------------------------------------------------------------------
      IF (TMIN .LT. FREEZ1 .OR. TMIN .LT. FREEZ2) THEN
        CALL FOR_FREEZE(
     &    FREEZ1, FREEZ2, FRZDC, NRUSLF, NRUSST,      !Input
     &    SLDOT, SSDOT, STMWT, TMIN, WTLF, YRDOY,     !Input
     &    YRPLT,                                      !Input
     &    MDATE,                                      !Input/Output
     &    CropStatus, FRZDL, PSRLYRD, PSRSRFD,        !Output
     &    WLFDOT, WSFDOT, WSRFDOT)                    !Output 

      ELSE
      WLFDOT = 0.0
      WSRFDOT = 0.0
      WSFDOT = 0.0
      ENDIF
C-----------------------------------------------------------------------
C     Call to root growth and rooting depth routine
!-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C      2/21/05 - SJR - move conditional call for water stress from CROPGRO 
C      to FOR_ROOTS.  Allows root senescence when Water dynamics option is 
C      turned off.  Water stress options set to no stress levels. 
C-----------------------------------------------------------------------
!     IF (ISWWAT .EQ. 'Y') THEN
        CALL FOR_ROOTS(INTEGR,
     &    AGRRT, CROP, DLAYR, DS, DTX, DUL, FILECC,       !Input
     &    FILEIO, FRRT, ISWWAT, LL, NLAYR, PG,            !Input
     &    RLSEN, RO, RP, RTWT, SAT, SW,                   !Input
     &    SWFAC, VSTAGE, WR, WRDOTN, WTNEW,               !Input
     &    CUMDEP, RLV, RTDEP, SATFAC, SENRT,              !Output
     &    SRCADDOT, SRNADDOT)                             !Output
!      ENDIF

C-----------------------------------------------------------------------
C     Compute total C cost for growing seed, shell, and vegetative tissue
C     for tomorrow's potential growth calculations
C-----------------------------------------------------------------------
C       Calculate the respiration required for seed, shell, and veg tissue
C       depending on the source of N uptake
C-----------------------------------------------------------------------
C      10/31/05 SJR dropped NMINEA from condition to use today's N cost 
C            tomorrow.  With natural senescence coming out in the morning, 
C            there will always be some NMINEA.  When there is only NMINEA, 
C            RPROAV is very low, DEMAND overpredicts new growth and we get 
C            NLEAK, which is returned to the soil.  Using the average of 
C            NO3 and NH4 uptake costs, the odds are that growth will be 
C            better predicted and reduce the chance of NLEAK.
C-----------------------------------------------------------------------
      IF ((TRNU + NFIXN + NMINEA) .GT. 0.0) THEN
!      IF ((TRNU + NFIXN) .GT. 0.0) THEN

        RPROAV = ((RSPNO3 + RSPNH4) * 0.16 + NFIXN * RFIXN
     &    + NMINEA * RPRO ) / (TRNU + NFIXN + NMINEA)
      ELSE
        RPROAV = (RNO3C + RNH4C) / 2.
      ENDIF
C-----------------------------------------------------------------------
C     AGRSD2 = SDPRO*RPROAV + PMINSD*RMIN + PLIGSD*RLIG + POASD*ROA
C    &     + (SDLIP*RLIP + PCARSD*RCH2O)*(1. - SDPROR)
C-----------------------------------------------------------------------
      AGRSD2 = FNINSD*6.25*RPROAV + PMINSD*RMIN + PLIGSD*RLIG
     &    + POASD*ROA   +  POTLIP*RLIP + POTCAR*RCH2O
      AGRSH2=  PROSHI*RPROAV + PLIPSH*RLIP + PLIGSH*RLIG + POASH*ROA
     &    + PMINSH*RMIN + PCARSH*RCH2O
C-----------------------------------------------------------------------
C     SJR 6/14/05 - Add cost for STOR protein to AGRVG2
C-----------------------------------------------------------------------


      AGRVG2 = AGRVG + (FRLF*PROLFI+FRRT*PRORTI+FRSTM*PROSTI+
     &    FRSTR*PROSRI)*RPROAV

C-----------------------------------------------------------------------
C     Call routine to integrate growth and damage
C-----------------------------------------------------------------------

      CALL FOR_GROW(CONTROL, ISWITCH, INTEGR, SOILPROP, 
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

      FHWAH= 0.0
      FHTOTN = 0.0
      FHLPH = 0.0
!      fhpctn = 0.0
      MOWC =0.0
      RSPLC =0.0
      call forage_harvest(CONTROL,FILECC, ATMOW, ATTP,
     &    RHOL,RHOS,PCNL,PCNST,SLA,RTWT,STRWT,       !Input
     &    WTLF,STMWT,TOPWT,TOTWT,WCRLF,WCRST,        !Input/Output
     &    WTNLF,WTNST,WNRLF,WNRST,WTNCAN,            !Input/Output
     &    AREALF,XLAI,XHLAI,VSTAGE,vstagp,canht,     !Input/Output
     &    FHWAH,FHTOTN, FHLPH,fhpctn,FREQ,
     &    MOWC,RSPLC,HMFRQ,HMGDD,HMCUT,HMMOW,HRSPL,
     &    DWTCO, DWTLO, DWTSO, PWTCO, PWTLO, PWTSO,
     &    HMVS, WTCO, WTLO, WTSO, TAVG, MOWGDD,
     &    MOWCOUNT, TGMIN, VTO1, VTB1, MOWREF, 
     &    RSREF, YFREQ, YRSREF, YCUTHT, YCHMOW,
     &    XCUTHT, XCHMOW, XFRGDD, XFREQ, CUTDAY,
     &    PROLFF, PROSTF, pliglf, pligst)

      Cumul_FHTOT  = Cumul_FHTOT  + FHWAH
      Cumul_FHTOTN = Cumul_FHTOTN + FHTOTN
!      IF (FHWAH > 1.E-3) THEN
!        WRITE(3333,"('FHWAH,FHTOTN',2F10.2)") FHWAH, FHTOTN
!      ENDIF
      CONTINUE
!-----------------------------------------------------------------------
!     End of DAS > NVEG0 if construct
!-----------------------------------------------------------------------
      ENDIF
!-----------------------------------------------------------------------

!***********************************************************************
!***********************************************************************
!-----------------------------------------------------------------------
!     OUTPUT / SEASEND section
!-----------------------------------------------------------------------
      ELSE IF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
        IF (YRDOY .EQ. YREND .AND. DYNAMIC .EQ. OUTPUT) THEN
        STGDOY(16) = YREND
        ENDIF

C-----------------------------------------------------------------------
C     CallDormancy module for daily printout.
C-----------------------------------------------------------------------
      CALL FOR_DORMANCY( CONTROL,                            !Input
     &    DAYL,TMIN,                                         !Input
     &    DRMST, FREEZ2, FRZDC, PPGFAC, PPTFAC, PPMFAC,      !Output
     &    FNPGD, FNPMD, FNPTD, FRZDHD, FRZHRD, HARD1,        !Output
     &    HARD2, RCHDP, RDRMG, RDRMM, RDRMT, TYPDHD, TYPHRD, !Output
     &    TYPPGD, TYPPMD, TYPPTD)                            !Output

        IF (ISWDIS.EQ.'Y') THEN
        CALL FOR_PEST(CONTROL, ISWITCH, 
     &  AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, PGAVL,    !Input
     &  PHTIM, PLTPOP, RTWT, SLA, SLDOT, SOILPROP,      !Input
     &  SSDOT, STMWT, TOPWT, WLFDOT, WTLF, YRPLT,       !Input
     &  RLV, SDNO, SHELN, SWIDOT,                       !I/O
     &  VSTAGE, WSHIDT, WTSD, WTSHE,                    !I/O
     &  ASMDOT, DISLA, HPDAM, NPLTD, PPLTD,             !Output
     &  SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT,             !Output
     &  CSRW, SSRDOT, STRWT, WSFDOT, WSRFDOT,           !Input
     &  WSRIDOT,                                        !Output
     &  CSFRZ, CSRFRZ, CSTRM, DSTOR, SRDAM)             !Output
        ENDIF

      CALL FOR_OPMOB(CONTROL, ISWITCH, 
     &  YRPLT, MDATE, DAS, YRDOY, DTX, DXR57, PGAVL, NAVL, PG, PPMFAC, 
     &  NMOBR, NMOBSR, MAINR, ASMDOT, RSPNO3, RSPNH4, RPRO,       
     &  CNOD, CGRSD, CGRSH, CADVG, CSAVEV, AGRVG, PCH2O, WTLF, WLDOT, 
     &  WLIDOT, WLFDOT, WLDOTN, RTWT, WRDOT, WRDOTN, STMWT, WSDOT,          
     &  WSIDOT, WSFDOT, WSDOTN, 
     &  STRWT, WSRDOT, WSRIDOT, WSRFDOT, WSRDOTN,
     &  SLMDOT, LFSENWT, LTSEN, SLNDOT, SLCADDOT, SLNADDOT, 
     &  SLDOT, SRMDOT, SRNDOT, SRCADDOT, SRNADDOT, SRDOT, 
     &  SSMDOT, STSENWT, STLTSEN, SSNDOT, SSCADDOT, SSNADDOT, 
     &  SSDOT, SSRMDOT, SSRNDOT, SSRCADDOT, SSRNADDOT, SSRDOT,
     &  NDMVEG, NDMNEW, NDMOLD, NDMTOT, NMINEP, NMINEA, SDNPL, TRNU, 
     &  NFIXN, NGRSD, NGRSH, TSNMOB,
     &  WTNLF, WNRLF, LFSNMOB, NMINELF, ANMINELF, 
     &  NGRLF, NLALL, NADLF, NRUSLF, PCNL,
     &  WTNRT, WNRRT, RTSNMOB, NMINERT, ANMINERT, 
     &  NGRRT, NRALL, NADRT, NRUSRT, PCNRT,
     &  WTNST, WNRST, STSNMOB, NMINEST, ANMINEST, 
     &  NGRST, NSALL, NADST, NRUSST, PCNST,
     &  WTNSR, WNRSR, SRSNMOB, NMINESR, ANMINESR, 
     &  NGRSR, NSRALL, NADSR, NRUSSR, PCNSR,
     &  WTNSH, WNRSH, SHNMINE, NRUSSH,    
     &  TSCMOB, WCRLF, WRCLDT, LFSCMOB, CMINELF, CADLF, CRUSLF, RHOL,
     &  WCRRT, WRCRDT, RTSCMOB, CMINERT, CADRT, CRUSRT, RHOR,          
     &  WCRST, WRCSDT, STSCMOB, CMINEST, CADST, CRUSST, RHOS,     
     &  WCRSR, WRCSRDT, SRSCMOB, CMINESR, CADSR, CRUSSR, RHOSR,
     &  WCRSH, WRCSHD, SHCMINE, CRUSSH, CHORECOVER, NLKSPENT, NLKNUSED,
     &  NLKCHK, TNLKCHK, CMOBSR, LAIMOBR, VNMOBR)     


      CALL FOR_OPGROW(CONTROL, ISWITCH, 
     &    CADLF, CADST, CANHT, CANWH, CMINEA, DWNOD,  
     &    GROWTH, GRWRES, MAINR, MDATE, NFIXN, NSTRES, 
     &    PCLSD, PCCSD, PCNL, PCNRT, PCNSD, PCNSH, PCNST, 
     &    PG, PODNO, PODWT, PODWTD, RHOL, RHOS, RLV, RSTAGE, 
     &    RTDEP, RTWT, SATFAC, SDWT, SEEDNO, SLA, STMWT, SWFAC, 
     &    TGRO, TGROAV, TOPWT, TOTWT, TURFAC, VSTAGE, WTCO, 
     &    WTLF, WTLO, WTNCAN, WTNLF, WTNST, WTNSD, WTNUP, 
     &    WTNFX, WTSO, XLAI, YRPLT,
     &    DRMST, PPGFAC, PPMFAC, PPTFAC, SRFTEMP, ST, FREEZ2,
     &    AGRSTR, CADSR, CMOBSR, CPFSTR, CRUSSR, CSRFRZ, CSRW, 
     &    CSTRM, DSTOR, FNINSR, FNINSRG, FRSTR, FRSTRM, NADSR, 
     &    NGRSR, NGRSRG, NMOBSR, NRUSSR, NSRALL, NSRDOT, NSROFF, 
     &    NVSTSR, PCNSR, PCSTRD, PROSRT, PSRSRFD, PSRLYRD, 
     &    PSRSRFL, PSRLYR1, RHOSR, SRDAM, SRSRFD, SRLYRD, SSRDOT, 
     &    SSRNDOT, STRWT, TPSRSRFL, TPSRLYR1, WCRSR, WNRSR, 
     &    WRCSRDT, WSRDOT, WSRDOTN, WSRFDOT, WSRI, WSRIDOT, 
     &    WTNSR, WTNSRA, WTNSRO, WTSRO, XSTR,
     &    FRLF, FRSTM, FRRT,
     &    FHWAH, FHLPH, DWTCO, DWTLO, DWTSO,fhpctn,RHOR)

!     !!   ! Write to Overview.out and summary.out files.
!     !!   CALL FOR_OPHARV (CONTROL, ISWITCH, 
!!!    !!&  SDRATE, SDWT, SDWTAM, SEEDNO, STGDOY, STMWT,    !Input
!     !!&  TOPWT, TURFAC, VSTAGE, WTNCAN, WTNFX, WTNSD,    !Input
!     !!&  WTNST, WTNUP, XLAI,                             !Input
!     !!&  YRNR1, YRNR3, YRNR5, YRNR7, YRPLT,              !Input
!     !!&  BWAH, SDWTAH)                                   !Output
      ENDIF

!       Call PlantNBal only for seasonal output.
      IF (DYNAMIC .EQ. SEASEND) THEN

      CALL FOR_DORMANCY( CONTROL,
     &    DAYL, TMIN,                                        !Input
     &    DRMST, FREEZ2, FRZDC, PPGFAC, PPTFAC, PPMFAC,      !Output
     &    FNPGD, FNPMD, FNPTD, FRZDHD, FRZHRD, HARD1,        !Output
     &    HARD2, RCHDP, RDRMG, RDRMM, RDRMT, TYPDHD, TYPHRD, !Output
     &    TYPPGD, TYPPMD, TYPPTD)                            !Output

        IF (CROP .NE. 'FA') THEN
        CALL FOR_PLANTNBAL (CONTROL, ISWITCH, 
     &  SEEDNI, TNLEAK, WTNFX, WTNLA, WTNLF, WTNLO,     !Input
     &  WTNNA, WTNNO, WTNNOD, WTNRA, WTNRO, WTNRT,      !Input
     &  WTNSA, WTNSD, WTNSDA, WTNSDO, WTNSH, WTNSHA,    !Input
     &  WTNSHO, WTNSO, WTNST, WTNUP,                    !Input
     &  WTNSR, WTNSRA, WTNSRO)                          !Input

        ENDIF

!-----------------------------------------------------------------------
        CALL FOR_HRES_CGRO(CONTROL,
     &    CROP, DLAYR, DWNOD, NLAYR,PLIGLF, PLIGNO,       !Input
     &    PLIGRT, PLIGSD, PLIGSH, PLIGST, RLV, RTWT,      !Input
     &    SDWT, SENESCE, SHELWT, STMWT, WTLF, WTNLF,      !Input
     &    WTNNOD, WTNRT, WTNSD, WTNSH, WTNST,             !Input
     &    STRWT, WTNSR, PLIGSR,     !Storage tissue       !Input
     &    HARVRES)                                        !Output

        !SENCLN = 0.0
        SENESCE % ResWt  = 0.0
        SENESCE % ResLig = 0.0
        SENESCE % ResE   = 0.0
      ENDIF

      IF (DYNAMIC == SEASEND) THEN
!     Store Summary.out labels and values in arrays to send to
!     OPSUM routines for printing.  Integers are temporarily 
!     saved as real numbers for placement in real array.
      LABEL(1) = 'HWAH'; VALUE(1) = Cumul_FHTOT*10.
      LABEL(2) = 'CNAM'; VALUE(2) = Cumul_FHTOTN*10.
      LABEL(3) = 'BWAH'; VALUE(3) = 0.0

      !Send labels and values to OPSUM
      CALL SUMVALS (SUMNUM, LABEL, VALUE) 
      ENDIF

!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
!     Store plant module data for use in ETPHOT.
      Call PUT('PLANT', 'CANHT', CANHT)
      Call PUT('PLANT', 'CANWH', CANWH)
      Call PUT('PLANT', 'DXR57', DXR57)
      Call PUT('PLANT', 'EXCESS', EXCESS)
      Call PUT('PLANT', 'NR5', NR5)   
      Call PUT('PLANT', 'PLTPOP', PLTPOP)
      Call PUT('PLANT', 'RNITP', RNITP) 
      Call PUT('PLANT', 'SLAAD', SLAAD) 
      Call PUT('PLANT', 'XPOD', XPOD)  

      RETURN
      END ! SUBROUTINE CROPGRO
!=======================================================================

!***********************************************************************
!     Variable listing
!***********************************************************************
! AGEFAC    Reduction in photosynthesis, correlated to a decrease in 
!             percentage of Nitrogen in leaves (0-1) (fraction)
! AGRLF     Mass of CH2O required for new leaf growth (g[CH2O] / g[leaf])
! AGRNOD    CH2O requirement for nodule growth (g[CH2O] / g[nodule])
! AGRRT     Mass of CH2O required for new root growth (g[CH2O] / g[root])
! AGRSD1    CH2O requirement for seed growth, excluding cost for protein 
!             content (g[CH2O] / g[seed])
! AGRSD2    CH2O requirement for seed growth, including cost for protein 
!             content (g[CH2O] / g[seed])
! AGRSD3    CH2O requirement for seed growth, with reduced N content
!             (g[CH2O] / g[seed])
! AGRSH1    CH2O required for shell growth, excluding cost for protein 
!             content (g[CH2O] / g[shell])
! AGRSH2    CH2O requirement for shell growth, including cost for protein 
!             content (g[CH2O] / g[shell])
! AGRSTM    Mass of CH2O required for new stem growth (g[CH2O] / g[stem])
! AGRVG     Mass of CH2O required for vegetative tissue growth including 
!             stoichiometry and respiration (g[CH2O] / g[tissue])
! AGRVG2    Total mass of CH2O required for vegetative tissue growth
!             (g[CH2O] / g[tissue])
! AREALF    Area of leaves (one side) per unit ground area
!             (cm2[leaf] / m2[ground])
! ASMDOT    Daily assimilative damage (g[CH2O] /m2 / d)
! BETN      Spacing between plants in a row (m / plant)
! CADLF     Mass of CH2O added to leaf reserves after growth
!             (g[CH2O] / m2 / d)
! CADPR1    Maximum fraction of stem growth after flowering that can be 
!             allocated to carbohydrate storage just before a full seed 
!             load is set. ( fraction)
! CADPV    Maximum fraction of PGAVL for vegetative growth that can be 
!            allocated to mobile carbohydrate storage under non-stress   
!            conditions during vegetative growth stages. ( fraction)
! CADST     Mass of CH2O added to stems (g[CH2O] / m2 / d)
! CANHT     Canopy height (m)
! CANNAA    Weight of N in total plant at flowering (g[N] / m2)
! CANWAA    Canopy weight at flowering stage (g[plant] / m2)
! CANWH     Canopy width normal to row (m)
! CAVVEG    C available for vegetative tissue growth (g[CH2O] / m2 / d)
! CDMREP    Total CH2O needed for potential reproductive growth
!             (g[CH2O] / m2 / d)
! CGRSD     Carbon required for seed growth (g[CH2O] / m2 / d)
! CGRSH     Carbon required for shell growth (g[CH2O] / m2 / d)
! CLW       Cumulative leaf growth (g[leaf]/m2)
! CMINEA    Actual carbon mined from vegetative tissue (g[CH2O] / m2 / d)
! CMINEP    Potential CH2O mobilization from storage (g[CH2O] / m2 / d)
! CMINELF   Potential mobile CH2O avaialable today from leaf (g [CH2O] m-2)
! CMINERT   Potential mobile CH2O avaialable today from root (g [CH2O] m-2)
! CMINESR   Potential mobile CH2O avaialable today from STOR (g [CH2O] m-2)
! CMINEST   Potential mobile CH2O avaialable today from stem (g [CH2O] m-2)
! CMOBMX    Maximum C pool mobilization rate (g[CH2O] / m2 / d)
! CMOBSRN   Minimum fraction of C which can be mobilized from
!                  storage organ in a day 
! CMOBSRX   Maximum fraction of C which can be mobilized from
!                  storage organ in a day 
! CNDFX     Carbon needed to fix N needed but not supplied by uptake or 
!             mining (g[CH2O] / m2 / d)
! CNOD      C used in N-Fixation and nodule growth (including respiration 
!             costs) today (g[CH2O] / m2 / d)
! CNODMN    Minimum C reserved for nodule growth (g[CH2O] / m2 / d)
! CROP      Crop identification code 
! CROPD     Name of crop 
! CRREF(4)  CURV response of routine refill of mobile CH2O during 
!                  vegetaive stages to current mobile CH2O status
! CRUSLF    C mobilized from leaf tissue in a day (g[CH2O] / m2 / d)
! CRUSRT    C mobilized from root tissue in a day (g[CH2O] / m2 / d)
! CRUSSH    C mobilized from shell tissue in a day (g[CH2O] / m2 / d)
! CRUSST    C mobilized from stem tissue in a day (g[CH2O] / m2 / d)
! CSAVEV    Fraction of PG for VEG that is stored as CH2O 
! CSFRZ      Cumulative frozen stem tissue (g[stem]/m2)
! CSW       Cumulative stem growth (g[stem]/m2)
! CTONOD    C to allocate to nodules to fix N needed for reproductive and 
!             vegetative growth (g[CH2O] / m2 / d)
! CTONODR   CH2O allocated to nodules for fixing N needed for reproductive 
!             growth (g[CH2O] / m2 / d)
! CUMDEP    Cumulative depth of soil profile (cm)
! DAS       Days after start of simulation (days)
! DAYL      Day length on day of simulation (from sunrise to sunset) (hr)
! DETACH    Switch to determine if pod detachment will be simulated (Y or 
!             N) 
! DISLA     Diseased leaf area (cm2/m2/d)
! DLAYR(L)  Soil Depth in layer L (cm)
! DRMST        Dormancy status (NODORM=not dormant, DORM=dormant - reversible, 
!              COLDHD= cold-hardened - dormancy no longer reversible until spring)
! DRPP      Photoperiod days which occur in a real day
!             (photoperiod days / day)
! DTX       Thermal time that occurs in a real day based on vegetative 
!             development temperature function (thermal days / day)
! DUL(L)    Volumetric soil water content at Drained Upper Limit in soil 
!             layer L (cm3 [H2O] /cm3 [soil])
! DWNOD     Current nodule mass (g[nodule] / m2)
! DWNODA    Cumulative nodule growth (g[nodule] / m2)
! DXR57     Relative time between first seed (NR5) and physiological 
!             maturity (NR7) 
! DYNAMIC   Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
!             INTEGR, OUTPUT, or SEASEND 
! ECONO     Ecotype code - used to match ECOTYP in .ECO file 
! ENAME     Experiment description 
! EXCESS    Factor based on excess PG used to affect tomorrow's PG 
!             calculation 
! EXPER     Experiment code (prefix of input files) 
! F         Specific leaf area of new leaf tissue growth, including N
!             (cm2[leaf] / g[leaf])
! FILECC    Path plus filename for species file (*.spe) 
! FILEGC    Pathname plus filename for ECO file 
! FILEIO    Filename for INP file (e.g., IBSNAT35.INP) 
! FLWN(J)   Number of flowers added for cohort J (# / m2)
! FNINL     Maximum fraction of N for growing leaf tissue (g[N] / g[leaf])
! FNINR     Maximum fraction of N for growing root tissue (g[N] / g[root])
! FNINS     Maximum fraction of N for growing stem tissue (g[N] / g[stem])
! FNINSD    Maximum fraction of N for growing seed tissue based on 
!             temperature (g[N] / g[seed])
! FNINSH    Maximum fraction of N for growing shell tissue
!             (g[N] / g[shell])
! FRACDN    Relative time between flowering (NR1) and last leaf appearance 
!             (NDLEAF) 
! FRCNOD    Fraction of new root dry matter allocation that is diverted to 
!             nodule growth 
! FREEZ1    Temperature below which plant loses all leaves, but development 
!             continues (°C)
! FREEZ2    Temperature below which plant growth stops completely. (°C)
! FRLF      Fraction of vegetative tissue growth that goes to leaves on a 
!             day (g[leaf] / g[veg])
! FRRT      Fraction of vegetative tissue growth that goes to roots on a 
!             day (g[root] / g[veg])
! FRSTM     Fraction of vegetative tissue growth that goes to stems on a 
!             day (g[stem] / g[veg])
! FRZDL        Todays death loss of storage tissue/plant population due to freezing (proportion of STRWT and PLNTPOP)
! GDMSD     Seed growth demand based on temperature and photoperiod
!             (g[seed] / m2 / d)
! GROWTH    Total new growth of plant tissue on a day (g[tissue] / m2 / d)
! GRRAT1    Maximum growth per individual shell (g / shell / d)
! GRWRES    Growth respiration (g[CH2O]/m2-d)
! HPDAM         Harvest removal (proportion of TOPWT)
! IDETO     Switch for printing OVERVIEW.OUT file 
! ISWDIS    Pest damage simulation switch (Y or N) 
! ISWNIT    Nitrogen simulation switch (Y or N) 
! ISWSYM    Nitrogen fixation simulation switch (Y = simulate nodule 
!             growth, N = no nodule growth, U = N-fixation occurs at a rate 
!             that carbon will allow, and nodules are not grown explicitly) 
! ISWWAT    Water simulation control switch (Y or N) 
! LAGSD     Time required between shell growth and seed growth, per cohort
!             (Photo-thermal days)
! LAIMOBR        Effect of LAI on N & C mobilization. (0= no effect, 
!              mobilization at minimum rate, 1.0 = increase mobilization
!              to maximum rate.  Increases mobilization after harvest, damage.
! LAIMX     Maximum leaf area index this season (m2[leaf] / m2[ground])
! LFCMINE        Today's maximum potential CH2O mobilization from leaf (g [CH2O] m-2)
! LFMRC   Maintenance respiration cost for leaves (g[CH2O] / g leaf protein / hr)
! LFNMINE   Today's maximum potential N mobilization from leaf (g [N] m-2)
! LFSCMOB   Mass of leaf CH2O mobilized from tissue lost to natural 
!              senescence (g [CH2O] m-2 d-1)
! LFSENWT   Leaf senescence due to N mobilization (g[leaf] / m2[ground])
! LFSNMOB   Mass of leaf N mobilized from tissue lost to natural  
!              senescence (g [N] m-2 d-1)
! LL(L)     Volumetric soil water content in soil layer L at lower limit
!             ( cm3/cm3)
! LNGPEG    Time between start of peg and rapid shell formation (for 
!             peanuts only).  Defines slow growth period. (Photo-thermal days)
! LRMOB(4)  CURV response of mobilization to current LAI
! LRREF(4)  CURV response of routine refill of mobile CH2O during 
!                  vegetaive stages to current LAI
! MAINR     Maintenance respiration (g[CH2O] / m2 / d)
! MEEVP     Method of evapotranspiration (P=Penman, R=Priestly-Taylor, 
!             Z=Zonal) 
! MODEL     Name of CROPGRO executable file 
! MRSWITCH Parameter to select the method of calculating portion of 
!            Maintenance Respiration (MR) associated with plant mass.
!            If MRSWITCH="P" then calculate MR based on prgan protein content 
!            If MRSWITCH="M"or otherwise, calculate the mass associated portion  
!            of MR based on plant mass (original CROPGRO approach).
! NADLF     N added to leaf N reserves (g[N] / m2 / d)
! NADRT     N added to root N reserves (g[N] / m2 / d)
! NADSH    N added to shell N reserves (g[N] / m2 / d)
! NADST     N added to stem N reserves (g[N] / m2 / d)
! NAVL      Total mass of nitrogen available for growth (g[N] / m2 / d)
! NAVLV     N available for vegetative growth (g[N] / m2 / d)
! NDLEAF    Day when leaf expansion ceased (days)
! NDMNEW    Total N demand for new growth (g[N] / m2 / d)
! NDMOLD    N demand for old tissue (g[N] / m2 / d)
! NDMREP    Total N needed for potential reproductive growth
!             (g[N] / m2 / d)
! NDMSDR    Amount of Mobilized N which can be used for seed growth
!             (g[N] / m2 / d)
! NDMTOT    Total N demand (g[N] / m2 / d)
! NDMVEG    N required for vegetative growth if all PGAVL is used as 
!             computed (g[N] / m2 / d)
! NDSET     Day when last pod can form (days)
! NDTH      Nodule death rate (g[nodule] / m2 / d)
! NFIXN     Amount of N fixed during the day (g[N] / m2 / d)
! NGRLF     Maximum N demand for leaf growth (g[leaf N] / m2[ground] / d)
! NGRRT     Maximum N demand for root growth (g[root N] / m2[ground] / d)
! NGRSD     Rate of N accumulation in new seeds (g[N] / m2 / d)
! NGRSH     Rate of N accumulation in new shells (g[N] / m2 / d)
! NGRST     Maximum N demand for stem growth (g[stem N] / m2[ground] / d)
! NH4(L)    Ammonium N in soil layer L (°g[N] / g[soil])
! NL        Maximum number of soil layers = 20 
! NLAYR     Number of soil layers 
! NMINEA    Actual Nitrogen mined from existing tissue (g[N] / m2 / d)
! NMINELF   Potential mobile N avaialable today from leaf (g [N] m-2)
! NMINEP    Potential N mobilization from storage (g[N] / m2 / d)
! NMINERT   Potential mobile N avaialable today from root (g [N] m-2)
! NMINESR   Potential mobile N avaialable today from STOR (g [N] m-2)
! NMINEST   Potential mobile N avaialable today from stem (g [N] m-2)
! NMOBR     Stage dependent N mining rate 
! NMOBSRN   Minimum/ "normal" fraction of N which can be mobilized from
!                  storage organ in a day 
! NMOBSRX   Maximum fraction of N which can be mobilized from
!                  storage organ in a day 
! NO3(L)    Nitrate in soil layer L (°g[N] / g[soil])
! NODGR     New nodule growth (g[nod] / m2 / d)
! NOUTDO    Logical unit for OVERVIEW.OUT file 
! NPLTD     Number of plants destroyed (#/m2/d)
! NR1       Day when 50% of plants have at least one flower (days)
! NR2       Day when 50% of plants have one peg (peanuts only) (days)
! NR5       Day when 50% of plants have pods with beginning seeds (days)
! NR7       Day when 50% of plants first have yellowing or maturing pods
!             (days)
! NRUSLF    N actually mobilized from leaves in a day (g[N]/m2-d)
! NRUSRT    N actually mobilized from roots in a day (g[N]/m2-d)
! NRUSSH    N actually mobilized from shells in a day (g[N]/m2-d)
! NRUSST    N actually mobilized from stems in a day (g[N]/m2-d)
! NSTRES    Nitrogen stress factor (1=no stress, 0=max stress) 
! NVEG0     Day of emergence (days)
! OUTD      File name for pest damage output file 
! OUTG      Growth output file name (typically 'GROWTH.OUT') 
! PAR       Daily photosynthetically active radiation or photon flux 
!             density (moles[quanta]/m2-d)
! PCARSH    Proportion of shell tissue that is carbohydrate (fraction)
! PCCSD     Percentage of carbohydrate in seed tissue (100 g[C] / g[seed])
! PCH2O     Respiration loss due to storage/mobilization of CH2O
!             (g[CH2O] / g[CH2O])
! PCHOLFF        Minimum/final leaf CH2O concentration at senescence (CH2O equivalent of PROLFF)
! PCHORTF        Minimum/final root CH2O concentration at senescence (CH2O equivalent of PRORTF)
! PCHOSRF        Minimum/final STOR CH2O concentration at senescence (CH2O equivalent of PROSRF)
! PCHOSTF        Minimum/final stem CH2O concentration at senescence (CH2O equivalent of PROSTF)
! PCLSD     Percentage of lipid in seed tissue (100 g[lipid] / g[seed])
! PCNL      Percentage of N in leaf tissue (100 g[N] / g[leaf])
! PCNRT     Percent N in root tissue (100 g[N] / g[root])
! PCNSD     Percentage of N in seed tissue (100 g[N] / g[seed])
! PCNSH     Percentage of N in shell tissue (100 g[N] / g[shell])
! PCNST     Percent N in stem tissue (100 g[N] / g[stem])
! PCTMAT    Fraction of pods that are mature (seed are 90% of final size) 
! PG        Daily gross photosynthesis (g[CH2O] / m2 / d)
! PGAVL     Total available CH2O available for growth & respiration
!             (g[CH2O] / m2)
! PHTHRS(I) Threshold time that must accumulate in phase I for the next 
!             stage to occur  (thermal or photothermal days)
! PHTIM(I)  Cumulative photothermal time ages of seeds and shells 
! PHTMAX   Maximum amount of CH20 which can be produced if 
!            photosynthetically active radiation (PAR) is very high (3 
!            times PARMAX) and all other factors are optimal (g[CH2O]/m2-d)
! PLIGSD    Proportion of seed tissue that is lignin (fraction)
! PLIGSH    Proportion of shell tissue that is lignin (fraction)
! PLIPSH    Proportion of shell tissue that is lipid (fraction)
! PLTPOP    Plant population (# plants / m2)
! PMINSD    Proportion of seed tissue that is mineral (fraction)
! PMINSH    Proportion of shell tissue that is mineral (fraction)
! PNMLF      Proportion of actually mobilized N mobilized from leaves in a day
! PNMST      Proportion of actually mobilized N mobilized from stems in a day
! PNMRT      Proportion of actually mobilized N mobilized from roots in a day
! PNMSR      Proportion of actually mobilized N mobilized from storage organ in a day
! PNMSH      Proportion of actually mobilized N mobilized from shells in a day
! PNTIM(I)  Photothermal days from first flower when flowers in age group I 
!             formed (p-t-d)
! POASD     Proportion of seed tissue that is organic acid (fraction)
! POASH     Proportion of shell tissue that is organic acid (fraction)
! PODNO     Total number of pods (#/m2)
! PODWT     Dry mass of seeds plus shells, including C and N
!             (g[pods] / m2[ground])
! PODWTD    Mass of detached pods (g[pods] / m2[ground])
! POTCAR    Potential carbohydrate composition of seed based on temperature
!             (fraction)
! POTLIP    Potential lipid composition of seed based on temperature
!             (fraction)
! PPGFAC        Reduction in photosynthetic rate due to dormancy 
! PPLTD     Percent plants destroyed (%/m2/d)
! PPMFAC        Reduction in mobilization rate due to dormancy 
! PPTFAC        Reduction in partitioning to vegetative tissues during dormancy
! PROLFI    Maximum protein composition in leaves during growth with 
!             luxurious supply of N (g[protein] / g[leaf tissue])
! PRORTI    Maximum protein composition in roots during growth with 
!             luxurious supply of N (g[protein] / g[root])
! PROSHI    Maximum protein composition in shells during growth with 
!             luxurious supply of N ( g[protein] / g[shell tissue])
! PROSTI    Maximum protein composition in stems during growth with 
!             luxurious supply of N (g[protein] / g[stem])
! PROVEG    Average protein composition of growing tissue today
!             (g[protein] / g[veget. tissue])
! PRREF(4)  CURV response of routine refill of mobile CH2O during 
!                  vegetaive stages to today's canopy PG rate
! PSRLYRD        Proportion of total storage organ tissue loss from below ground tissue
! PSRLYR1   Proportion of storage organ tissue in soil layer 1 (below soil surface)
! PSRSRFD   Proportion of total storage organ tissue loss from above ground tissue
! PSRSRFL   Proportion of storage organ tissue on/above soil surface
! PUNCSD    Cumulative puncture damage to seed (not yet implemented) 
! PUNCTR    Cumulative puncture damage (not yet implemented) 
! R30C2     Respiration coefficient that depends on total plant mass, value 
!             at 30C (g[CH2O] used / g[CH2O] fixed / hr)
! RCH2O     Respiration required for synthesizing CH2O structure
!             (g[CH2O] / g[tissue])
! RES30C    Respiration coefficient that depends on gross photosynthesis, 
!             value at 30C (g CH2O/g DW/hr)
! RFIXN     CH2O required for biological N fixation (g[CH2O] / g[protein])
! RHOL      Fraction of leaf which is carbohydrate (g [CH20] / g[leaf])
! RHOR     Fraction of root which is carbohydrate (g [CH2O] / g[root])
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
! RTCMINE        Today's maximum potential CH2O mobilization from root (g [CH2O] m-2)
! RTNMINE   Today's maximum potential N mobilization from root (g [N] m-2)
! RTSCMOB   Mass of root CH2O mobilized from tissue lost to natural senescence 
!              (g [CH2O] m-2 d-1)
! RTSNMOB   Mass of root N mobilized from tissue lost to natural senescence 
!              (g [N] m-2 d-1)
! RSTAGE    Number of RSTAGES which have occurred. 
! RTDEP     Root depth (cm)
! RTMRC   Maintenance respiration cost for roots (g[CH2O] / g root protein / hr)
! RTWT      Dry mass of root tissue, including C and N
!             (g[root] / m2[ground])
! RVSTGE    Rate of VSTAGE change (nodes/day)
! SAT(L)    Volumetric soil water content in layer L at saturation
!             (cm3 [water] / cm3 [soil])
! SATFAC    Root length weighted soil water excess stress factor ( 0 = no 
!             stress; 1 = saturated stress ) 
! SDDES(J)  Number of seeds destroyed today in cohort J when shells are 
!             not destroyed (#/m2/day)
! SDGR      Potential growth rate per seed (g / seed / d)
! SDIDOT    Number of seeds destroyed on the current day (#/m2/d)
! SDNO(J)   Number of seeds for cohort J (#/m2)
! SDMRC   Maintenance respiration cost for the portion of seed subject to 
!            maintenance respiration (g[CH2O] / g seed protein / hr)
! SDNPL     Seed N (g[N] / m2)
! SDPROR    Ratio to adjust lipid and carbohydrate proportions when seed 
!             protein differs from protein composition of standard cultivar 
!             (SDPROS) 
! SDRATE    Seeding rate, mass of seed sown (g[seed] / m2[ground])
! SDVAR     Maximum cultivar-dependent seed growth rate, per seed
!             (g / seed / d)
! SDWT      Dry mass of seed tissue, including C and N
!             (g[seed] / m2[ground])
! SDWTAM    Seed weight at maturity (g / m2)
! SEEDNI    Seed or transplant N at planting (g[N] / m2)
! SEEDNO    Total number of seeds (#/m2)
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
! SHCMINE   Potential mobile CH2O avaialable today from shell (g [CH2O] m-2)
! SHELMRC Maintenance respiration cost for shell (g[CH2O] / g shell protein / hr)
! SHELN(J)  Number of shells for cohort J (#/m2)
! SHELWT    Total mass of all shells (g / m2)
! SHNMINE   Potential mobile N avaialable today from shell (g [N] m-2)
! SHVAR     Shell growth rate during its rapid growth phase, per shell
!             (g / shell / d)
! SLA       Specific leaf area (cm2[leaf] / m2[ground])
! SLAAD     Specific leaf area, excluding weight of C stored in leaves
!             (cm2[leaf] / g[leaf])
! SLDOT     Defoliation due to daily leaf senescence (g/m2/day)
! SLNADDOT  Today's NADLF lost with senescing leaf tissue (g [CP]/m2/d)
! SLNDOT    Leaf senescence due to water stress (g/m2/day)
! SNH4(L)   Total extractable ammonium N in soil layer L (kg [N] / ha)
! SNO3(L)   Total extractable nitrate N in soil layer L (kg [N] / ha)
! SRCMINE        Today's maximum potential CH2O mobilization from STOR (g [CH2O] m-2)
! SRDOT     Daily root senescence (g / m2 / d)
! SRNADDOT  Today's NADRT lost with senescing root tissue (g [CP]/m2/d)
! SRNMINE   Today's maximum potential N mobilization from STOR (g [N] m-2)
! SRSCMOB   Mass of STOR CH2O mobilized from tissue lost to natural senescence 
!              (g [CH2O] m-2 d-1)
! SRSNMOB   Mass of STOR N mobilized from tissue lost to natural senescence 
!              (g [N] m-2 d-1)
! SSRNADDOT Today's NADSR lost with senescing STOR tissue (g [CP]/m2/d)
! SSDOT     Daily senescence of petioles (g / m2 / d)
! SSNADDOT  Today's NADST lost with senescing stem tissue (g [CP]/m2/d)
! SSNDOT    Petiole senescence due to water stress (g/m2/day)
! ST(L)     Soil temperature in soil layer L (°C)
! STCMINE        Today's maximum potential CH2O mobilization from stem (g [CH2O] m-2)
! STGDOY(I) Day when stage I occurred (YYDDD)
! STLTSEN   Stem senescence corresponding to LTSEN
! STMMRC  Maintenance respiration cost for stem (g[CH2O] / g stem protein / hr)
! STMWT     Dry mass of stem tissue, including C and N
!             (g[stem] / m2[ground)
! STNAME(I) Output headings for specified crops 
! STNMINE   Today's maximum potential N mobilization from stem (g [N] m-2)
! STRMRC  Maintenance respiration cost for storage 
!            (g[CH2O] / g storage protein / hr)
! STSCMOB   Mass of petiole CH2O mobilized from tissue lost to natural senescence 
!              (g [CH2O] m-2 d-1)
! STSENWT   Stem senescence corresponding to SENWT
! STSNMOB   Mass of petiole N mobilized from tissue lost to natural senescence 
!              (g [N] m-2 d-1)
! SW(L)     Volumetric soil water content in layer L
!             (cm3 [water] / cm3 [soil])
! SWFAC     Effect of soil-water stress on photosynthesis, 1.0=no stress, 
!             0.0=max stress 
! SWIDOT    Daily seed mass damage (g/m2/day)
! TAVG      Average daily temperature (°C)
! TDUMX     Photo-thermal time that occurs in a real day based on early 
!             reproductive development temperature function
!             (photo-thermal days / day)
! TDUMX2    Photo-thermal time that occurs in a real day based on late 
!             reproductive development temperature function
!             (photo-thermal days / day)
! TGRO(I)   Hourly air temperature (°C)
! TGROAV    Average daily air temperature (°C)
! TITLET    Description of treatment for this simulation 
! TMIN      Minimum daily temperature (°C)
! TNLEAK    Total nitrogen leak (g[N] / m2 / d)
! TOPWT     Total weight of above-ground portion of crop, including pods
!             (g[tissue] / m2)
! TOTWT     Total weight of crop (g[tissue] / m2)
! TRNH4U    Total N uptake in ammonium form in a day (g[N] / m2 / d)
! TRNO3U    Total N uptake in nitrate form in a day (g[N] / m2 / d)
! TRNU      Total N uptake in a day (g[N] / m2 / d)
! TS        Number of intermediate time steps (=24) 
! TTFIX     Physiological days delay in nodule initiation
!             (photo-thermal days / day)
! TURADD    Water stress factor (TURFAC) effect on reproductive growth and 
!             pod addition.  Stress is defined to INCREASE growth and 
!             addition. 
! TURFAC    Water stress factor for expansion (0 - 1) 
! TYPCREF        Shape of CURV response for refilling mobile CH2O to CH2O status
! TYPLMOB        Shape of CURV response for mobilization to LAI
! TYPLREF        Shape of CURV response for refilling mobile CH2O to LAI
! TYPNMOB        Shape of CURV response for mobilization to current vegetative N 
!              status of plant relative to maximum N concentration
! TYPPREF        Shape of CURV response for refilling mobile CH2O to canopy PG rate
! UNH4      Uptake of NH4 from soil (interim value) (kg N/ha)
! UNO3      Uptake of NO3 from soil (interim value) (kg N/ha)
! VEGNCNT        Current proportion of total vegetative tissue that is N
! VEGNCMX        Maximun possible proportion of total vegetative tissue that 
!              can be N
! VNMOBR        Effect of N status on N & C mobilization. (0= no effect, 
!              mobilization at minimum rate, 1.0 = increase mobilization
!              to maximum rate      
! VNSTAT        Current N status of vegetative tissues compared to maximum 
!              possible N content
! VSTAGE    Number of nodes on main stem of plant 
! WCRLF     Mass of CH2O reserves in leaves (g[leaf CH2O] / m2[ground])
! WCRRT     Mass of CH2O reserves in roots (g[root CH2O] / m2[ground])
! WCRSH     Mass of CH2O reserves in shells (g[shell CH2O] / m2[ground])
! WCRSR        Mass of CH2O reserves in storage organ (g[storage CH2O] / m2[ground])
! WCRST     Mass of CH2O reserves in stems (g[stem CH2O] / m2[ground])
! WLDOT    Net leaf growth rate (g[leaf] / m2 / d)
! WLDOTN    Dry weight growth rate of new leaf tissue including N but not C 
!             reserves (g[leaf] / m2[ground]-d)
! WLFDOT    Leaf weight losses due to freezing (g[leaf]/m2-d)
! WLIDOT    Daily pest or freeze damage to leaf mass (g/m2/day)
! WNRLF     N available for mobilization from leaves above lower limit of 
!             mining (g[N] / m2)
! WNRRT     N available for mobilization from roots above lower limit of 
!             mining (g[N] / m2)
! WNRSH     N available for mobilization from shells above lower limit of 
!             mining (g[N] / m2)
! WNRST     N available for mobilization from stems above lower limit of 
!             mining (g[N] / m2)
! WRCLDT   Net C addition for leaves (g[CH2O] / m2 /d)
! WRCRDT   Net C addition for roots (g[CH2O] / m2 /d)
! WRCSDT   Net C addition for stems (g[CH2O] / m2 /d)
! WRCSHD   Net C addition for shells (g[CH2O] / m2 /d)
! WRCSRDT  Net C addition for storage organ (g[CH2O] / m2 /d)
! WRDOT    Net root growth rate (g[root] / m2 / d)
! WRDOTN    Dry weight growth rate of new root tissue including N but not C 
!             reserves (g[root] / m2[ground]-d)
! WRIDOT    Daily pest damage to root mass ( g/m2/day)
! WSDDTN    New seed growth today (g[seed] / m2 / d)
! WSDOT    Net stem growth rate (g[stem] / m2 / d)
! WSDOTN    Dry weight growth rate of new stem tissue including N but not C 
!             reserves (g[stem] / m2[ground]-d)
! WSFDOT        Stem weight losses due to freezing (g[stem]/m2-d)
! WSHDTN    New shell growth today (g[shell] / m2 / d)
! WSHIDT    Weight of shell tissue consumed by pests today (g[shell]/m2-d)
! WSIDOT    Daily pest damage to stem mass (g/m2/day)
! WTABRT    Weight of shells aborted on a day (g[shell] / m2 / d)
! WTCO      Cumulative losses of plant tissue (g[tissue] / m2)
! WTLF      Dry mass of leaf tissue including C and N
!             (g[leaf] / m2[ground])
! WTLO      Cumulative leaf losses (g[leaf] / m2)
! WTMAIN    Mass of tissue assumed to require maintenance (g[tissue] / m2)
! WTNCAN    Mass of N in canopy (g[N] / m2[ground])
! WTNEW     Initial mass of seedling or seed (g / plant)
! WTNFX     Cumulative weight of N fixed (g[N] / m2)
! WTNLA     Cumulative N added to leaves (g[N]/m2-d)
! WTNLF     Mass of N in leaves (g[leaf N] / m2[ground])
! WTNLO     Cumulative N loss from leaves (g[N] / m2)
! WTNNA     Cumulative N added to nodules (g[N]/m2-d)
! WTNNAG    Total accumulated N used in nodule growth (g[N] / m2)
! WTNNO     Cumulative N loss from nodules (g[N] / m2)
! WTNNOD    Mass of N in nodules (g[N] / m2[ground])
! WTNOO     Cumulative nodule losses (g[nodule] / m2)
! WTNRA     Cumulative N added to roots (g[N]/m2-d)
! WTNRO     Cumulative N loss from roots (g[N] / m2)
! WTNRT     Mass of N in roots (g[root N] / m2[ground])
! WTNSA     Cumulative N added to stems (g[N]/m2-d)
! WTNSD     Mass of N in seeds (g[N] / m2[ground])
! WTNSDA    Cumulative N added to seeds (g[N]/m2-d)
! WTNSDO    Cumulative N loss from seeds (g[N] / m2)
! WTNSH     Mass of N in shells (g[N] / m2[ground])
! WTNSHA    Cumulative N added to shells (g[N]/m2-d)
! WTNSHO    Cumulative N loss from shells (g[N] / m2)
! WTNSO     Cumulative N loss from stems (g[N] / m2)
! WTNST     Mass of N in stems (g[stem N] / m2[ground])
! WTNUP     Cumulative N uptake (g[N] / m2)
! WTRO      Cumulative root losses (g[root] / m2)
! WTSD(J)   Seed mass  for cohort J (g/m2)
! WTSDO     Cumulative seed losses (g[seed] / m2)
! WTSHE(J)  Shell mass  for cohort J (g/m2)
! WTSHMT     Cohorts that reach THRESH today (g/m2)
! WTSHO     Cumulative shell losses (g[shell] / m2)
! WTSO      Cumulative stem losses (g[stem] / m2)
! XFRT      Current day's partitioning to reproductive growth (0-1)
!             (g[fruit] / g[plant])
! XHLAI     Leaf area index (m2[leaf] / m2[ground])
! XLAI      Leaf area (one side) per unit of ground area
!             (m2[leaf] / m2[ground])
! XPOD      Growth partitioning to pods which slows node appearance
!             (fraction)
! YRDOY     Current day of simulation (YYDDD)
! YREMRG    Day of emergence (YYDDD)
! YRNR1     Day when 50% of plants have at least one flower (YYDDD)
! YRNR2     Day when 50% of plants have one peg (peanuts only) (YYDDD)
! YRNR3     Day when 50% of plants have at least one beginning pod (YYDDD)
! YRNR5     Day when 50% of plants have pods with beginning seeds (YYDDD)
! YRNR7     Day when 50% of plants first have yellowing or maturing pods
!             (YYDDD)
! MDATE     Harvest maturity (YYDDD)
! YRPLT     Planting date (YYDDD)
! YRSIM     Start of simulation date (YYDDD)
!***********************************************************************
!      END SUBROUTINE FORAGE
!=======================================================================
