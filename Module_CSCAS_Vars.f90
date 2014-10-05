!=======================================================================
!  MODULE Module_CSCAS_Vars
!  09/09/2014 MJF Written
!=======================================================================

    MODULE Module_CSCAS_Vars
!     Contains definitions of derived data types and constants which are 
!     used throughout the CSCAS submodel.
        IMPLICIT NONE
!     Global constants

        INTEGER,PARAMETER::DINX =   3 ! Disease number,maximum
        INTEGER,PARAMETER::PSX  =  20 ! Principal stages,maximum
        INTEGER,PARAMETER::SSX  =  20 ! Secondary stages,maximum
        INTEGER,PARAMETER::KEYSTX = 9 ! Maximum number of key stages
        INTEGER,PARAMETER::DCNX =  10 ! Disease control #,maximum
        INTEGER,PARAMETER::LCNUMX=500 ! Maximum number of leaf cohorts
        INTEGER,PARAMETER::LNUMX= 500 ! Maximum number of leaves/axis
        INTEGER,PARAMETER::HANUMX= 40 ! Maximum # harvest instructions
        INTEGER,PARAMETER::NL   =  20 ! Maximum number of soil layers

        INTEGER,PARAMETER::RUNINIT= 1 ! Program initiation indicator
        INTEGER,PARAMETER::SEASINIT=2 ! Reinitialisation indicator
        INTEGER,PARAMETER::RATE =   3 ! Program rate calc.indicator
        INTEGER,PARAMETER::INTEGR=  4 ! Program update indicator
        INTEGER,PARAMETER::OUTPUT=  5 ! Program output indicator
        INTEGER,PARAMETER::SEASEND= 6 ! Program ending indicator

        CHARACTER(LEN=1),PARAMETER::BLANK = ' '
        CHARACTER(LEN=3),PARAMETER::DASH = ' - '

            !REAL,PARAMETER::PATM=101300.0! Pressure of air,Pa
            !REAL,PARAMETER::SHAIR=1005.0 ! Specific heat of air,MJ/kg
            !REAL,PARAMETER::SBZCON=4.903E-9 !Stefan Boltzmann,MJ/K4/m2/d
        SAVE


!=======================================================================

! All variables apart from dummy arguments and FUNCTIONS.
      
!           Integers:
      
        INTEGER ARGLEN      , CDAYS       , CNI         , COLNUM      !, CSIDLAYR    , CSTIMDIF    , CSYDOY      , CSYEARDOY   
        INTEGER CTRNUMPD    , CWADCOL     , DAE         , DAP         , DAS         , DATE        , DATECOL     !, DAPCALC      
        INTEGER DCDAT(DCNX) , DCTAR(DCNX) , DIDAT(DINX) , DIDOY(DINX) , DOM         , DOYCOL      , DYNAMICPREV , EDAP        
        INTEGER EDAPM       , EDATM       , EDATMX      , EMDATERR    , ERRNUM      , EVALOUT     , EVHEADNM    , EVHEADNMMAX 
        INTEGER EYEARDOY    , FAPPNUM     , FDAY(200)   , FILELEN     , FLDAP       , FNUMERA     , FNUMERR     , FNUMERT     
        INTEGER FNUMEVAL    , FNUMLVS     , FNUMMEAS    , FNUMOV      , FNUMPHA     , FNUMPHEM    , FNUMPHES    , FNUMPREM    
        INTEGER FNUMPRES    , FNUMPSUM    , FNUMREA     , FNUMT       , FNUMTMP     , FNUMWRK     , FROPADJ     , GDAP        
        INTEGER GDAPM       , GDATM       , GSTDCOL     , GYEARDOY    , HADOY       , HANUM       , HAYEAR      , HDAY        
        INTEGER HDOYF       , HDOYL       , HFIRST      , HIADCOL     , HLAST       , HNUMACOL    , HNUMBER     , HNUMECOL    
        INTEGER HSTG        , HWADCOL     , HWTUCOL     , HYEAR       , HYEARDOY(HANUMX)          , HYEARF      , HYEARL      
        INTEGER HYRDOY(HANUMX)            , I           , IDETGNUM    , KEYPS(KEYSTX)             , KEYPSNUM    , L           
        INTEGER L1          , L2          , LAIDCOL     , LBIRTHDAP(LCNUMX)         , LCNUM       , LDEATHDAP(LCNUMX)         
        INTEGER LENDIS      , LENENAME    , LENGROUP    , LENLINE     , LENLINESTAR , LENRNAME    , LENTNAME    , LINENUM     
        INTEGER LNUMCOL     , LNUMSG      , LNUMSOLDESTA, LRTIP       , LSEED       , MDAP        , MDAPM       , MDAT        
        INTEGER MDATERR     , MDATM       , MDOY        , MSTG        , NLAYRROOT   , NOUTPG      , NOUTPG2     , NOUTPGF     
        INTEGER NOUTPN      , NSDAYS      , ONI         , OUTCHOICE   , OUTCOUNT    , PATHL       , PDATE       , PDAYS(0:12) 
        INTEGER PGDAP       , PGROCOL(20) , PHINTSTG    , PLDAY       , PLDAYTMP    , PLTOHARYR   , PLYEAR      , PLYEARDOY   
        INTEGER PLYEARDOYPREV             , PLYEARDOYT  , PLYEARREAD  , PLYEARTMP   , PSDAP(PSX)  , PSDAPM (PSX), PSDAT(PSX)  
        INTEGER PSDATM (PSX), PSIDAP      , PSIDAPM     , PSIDATERR   , PSNUM       , PWDINF      , PWDINL      , PWDOYF      
        INTEGER PWDOYL      , PWYEARF     , PWYEARL     , RNI         , RPCOL       , RTSLXDATE   , RUNCRP      , SHDAP       
        INTEGER SHDAT       , SNI         , SRNOPD      , STARNUM     , STARNUMM    , STARNUMO    , STARTPOS    , STEPNUM     
        INTEGER STGEDAT     , TDATANUM    , TFCOLNUM    , TFDAP       , TFDAPCOL    , TIERNUM     , TLINENUM    , TLPOS       
        INTEGER TNI         , TVI1        , TVI2        , TVI3        , TVI4        , VALUEI      !, TVICOLNM    ,  TVILENT         
        INTEGER VARNUM(30)  , VERSION     , WSDAYS      , YEARCOL     , YEARDOY     , YEARDOYHARF , YEARDOYPREV , YEARM       
        INTEGER YEARSIM     

!           Reals:
    
        REAL    AFLF(0:LNUMX)             , AH2OPROFILE , AH2OROOTZONE, ALBEDOS     , AMTNIT      , AMTNITPREV  , ANDEM       
        REAL    ANFER(200)  , AREAPOSSIBLE, AREAPOSSIBLEN             , AVGSW       , BASELAYER   , BRFX(PSX)   , BRNUMSH     
        REAL    BRNUMSHM    , BRNUMST     , BRNUMSTPREV , BRSTAGEPREV , BRSTAGETMP  , CANHTG      , CANHTS      , CARBOADJ    
        REAL    CARBOBEG    , CARBOBEGI   , CARBOBEGIA  , CARBOBEGM   , CARBOBEGR   , CARBOC      , CARBOEND    , CARBOR      
        REAL    CARBOT      , CARBOTMP    , CARBOTMPI   , CARBOTMPM   , CARBOTMPR   , CHTPC(10)   , CLAPC(10)   , CNAD        
        REAL    CNADPREV    , CNADSTG(20) , CNAM        , CNAMERR     , CNAMM       , CNCTMP      , CNPCHM      , CO2AIR      
        REAL    CO2CAV      , CO2CC       , CO2COMPC    , CO2EX       , CO2F(10)    , CO2FP       , CO2FPI      , CO2INT      
        REAL    CO2INTPPM   , CO2INTPPMP  , CO2MAX      , CO2PAV(0:12), CO2PC       , CO2RF(10)   , CRFR        , CRRSWAD     
        REAL    CRRSWT      , CRWAD       , CRWADOUT    , CRWT        , CRWTM       , CUMDEP      ! CSVPSAT     , CSYVAL      , 
        REAL    CUMDU       , CUMSW       , CWAD        , CWADPREV    , CWADSTG(20) , CWADT       , CWAHC       , CWAHCM      
        REAL    CWAM        , CWAMERR     , CWAMM       , CWAN(HANUMX), DALF(0:LNUMX)             , DAYLCAV     , DAYLCC      
        REAL    DAYLPAV(0:12)             , DAYLPC      , DAYLPREV    , DAYLS(0:10) , DAYLST(0:12), DAYSUM      , DCDUR(DCNX) 
        REAL    DCFAC(DCNX) , DF          , DFNEXT      , DFOUT       , DFPE        , DGLF(0:LNUMX)             
        REAL    DIFFACR(DINX)             , DIGFAC(DINX), DLAYRTMP(20), DMP_EP      , DMP_ET      , DMP_IRR     , DMP_NAPP    
        REAL    DMP_NUPT    , DMP_RAIN    , DRAINC      , DSLF(0:LNUMX)             , DSTAGE      , DTRY        , DU          
        REAL    DUNEED      , DUPHASE     , DUPNEXT     , DUSRI       , DUTOMSTG    , EARLYN      , EARLYW      , EDAPFR      
        REAL    EDAYFR      , EMRGFR      , EMRGFRPREV  , EOC         , EOEBUD      , EOEBUDC     , EOEBUDCRP   , EOEBUDCRPC  
        REAL    EOEBUDCRPCO2, EOEBUDCRPCO2C             , EOEBUDCRPCO2H2O           , EOEBUDCRPCO2H2OC          , EOMPCRP     
        REAL    EOMPCRPC    , EOMPCRPCO2  , EOMPCRPCO2C , EOMPCRPCO2H2O             , EOMPCRPCO2H2OC            , EOMPEN      
        REAL    EOMPENC     , EOPEN       , EOPENC      , EOPT        , EOPTC       , EPCC        , EPPC(0:12)  , EPSRATIO    
        REAL    ERRORVAL    , ETCC        , ETPC(0:12)  , FAC(20)     , FERNITPREV  , FLN         , FNH4        , FNO3        
        REAL    FSOILH2O    , FSOILN      , GDAPFR      , GDAYFR      , GEDAYSE     , GEDAYSG     , GERMFR      , GESTAGE     
        REAL    GESTAGEPREV , GEUCUM      , GROCR       , GROCRADJ    , GROCRFR     , GROLF       , GROLFADJ    , GROLFNEEDED 
        REAL    GROLFP      , GROLS       , GROLSA      , GROLSP      , GROLSRS     , GROLSRT     , GROLSRTN    , GROLSSD     
        REAL    GROLSSEN    , GRORS       , GRORSADJ    , GROSR       , GROST       , GROSTADJ    , GROSTCR     , GROSTCRP    
        REAL    GROSTCRPSTORE             , GRP_EP      , GRP_ET      , GRP_IRR     , GRP_NAPP    , GRP_NUPT    , GRP_RAIN    
        REAL    H2OA        , H2OCF       , H2OPROFILE  , H2OROOTZONE , HAFR        , HAMT(HANUMX), HAWAD       , HBPC(HANUMX)
        REAL    HBPCF       , HDUR        , HIAD        , HIADT       , HIAM        , HIAMERR     , HIAMM       , HIAMMTMP    
        REAL    HIND        , HINM        , HINMM       , HMPC        , HNAD        , HNAM        , HNAMERR     , HNAMM       
        REAL    HNC         , HNPCM       , HNPCMERR    , HNPCMM      , HNUMAD      , HNUMAERR    , HNUMAM      , HNUMAMM     
        REAL    HNUMAT      , HNUMET      , HNUMGERR    , HNUMGM      , HNUMGMM     , HNUMPM      , HNUMPMM     , HPC(HANUMX) 
        REAL    HPCF        , HPRODN      , HSTAGE      , HWAD        , HWADT       , HWAHERR     , HWAHM       , HWAM        
        REAL    HWAMM       , HWUD        , HWUM        , HWUMERR     , HWUMM       , HWUMYLD     , HWUT        , HYAMM       
        REAL    ICWD        , IRRAMTC     , ISOILH2O    , ISOILN      , KCANI       , KEPI        , LA1S        , LAFND       
        REAL    LAFS        , LAGEG(0:LNUMX)            , LAGEP(0:LNUMX)            , LAGETT(0:LNUMX)           
        REAL    LAGL(1,LNUMX)             , LAI         , LAIA        , LAIPREV     , LAIPROD     , LAISTG(20)  , LAIX        
        REAL    LAIXERR     , LAIXM       , LAIXT       , LAIXX       , LANC        , LANCRS      , LAP(0:LNUMX), LAPD        
        REAL    LAPH        , LAPHC       , LAPOTX(LNUMX)             , LAPP(LNUMX) , LAPS(LNUMX) , LAPSTMP     
        REAL    LATL(1,LNUMX)             , LATL2(1,LNUMX)            , LATL3(1,LNUMX)            , LATL4(1,LNUMX)            
        REAL    LATLPOT(1,LNUMX)          , LATLPREV(1,LNUMX)         , LAWCF       , LAWFF       , LAWL(2)     , LAWMNFR     
        REAL    LAWS        , LAWTR       , LAWTS       , LAWWR       , LAXN2       , LAXNO       , LAXS        , LCOA(LCNUMX)
        REAL    LCOAS(LCNUMX)             , LEAFN       , LEAFNEXCESS , LFGSDU      , LFWT        , LFWTM       , LGPHASE(2)  
        REAL    LGPHASEDU(2), LLIFA       , LLIFATT     , LLIFG       , LLIFGTT     , LLIFS       , LLIFSTT     , LLIFX       
        REAL    LLIFXUNUSED , LLIGP       , LLNAD       , LLOSA       , LLRSWAD     , LLRSWT      , LLWAD       , LLWADOUT    
        REAL    LNCGL       , LNCGU       , LNCM        , LNCMN(0:1)  , LNCPL       , LNCPU       , LNCR        , LNCSEN      
        REAL    LNCSENF     , LNCX        , LNCXS(0:1)  , LNDEM       , LNDEM2      , LNDEMG      , LNDEMTU     , LNPCMN(0:1) 
        REAL    LNPCS(0:1)  , LNPH        , LNPHC       , LNUM        , LNUMG       , LNUMNEED    , LNUMPREV    , LNUMSERR    
        REAL    LNUMSIMTOSTG(0:PSX)       , LNUMSM      , LNUMSMM     , LNUMSTG(20) , LNUMT       , LNUMTMP     
        REAL    LNUMTOSTG(0:PSX)          , LNUSE(0:3)  , LPEAI       , LPEAW       , LPEFR       , LPERSWAD    , LPERSWT     
        REAL    LPEWAD      , LSENI       , LSNUM(HANUMX)             , LSTAGE      , LSWT(HANUMX), LWLOS       , LWPH        
        REAL    LWPHC       , MDAPFR      , MDATT       , MDAYFR      , MJPERE      , NCRG        , NDEM2       , NDEMG       
        REAL    NDEMMN      , NFG         , NFGCAV      , NFGCC       , NFGL        , NFGPAV(0:12), NFGPC       , NFGU        
        REAL    NFLF(LNUMX) , NFLF2(0:LNUMX)            , NFLFP(LNUMX), NFPCAV      , NFPCC       , NFPL        , NFPPAV(0:12)
        REAL    NFPPC       , NFPU        , NFRG        , NFSU        , NH4CF       , NH4FN       , NH4MN       , NLABPC      
        REAL    NLLG        , NO3CF       , NO3FN       , NO3MN       , NPOOLL      , NPOOLR      , NPOOLS      , NTUPF       
        REAL    NUF         , NULEFT      , NUPAC       , NUPACM      , NUPAD       , NUPAP       , NUPAPCRP    , NUPAPCSM    
        REAL    NUPAPCSM1   , NUPC        , NUPD        , NUPRATIO    , NUSEFAC     , PARFC       , PARI        , PARI1       
        REAL    PARIOUT     , PARIPREV    , PARIUE      , PARIUED     , PARIX       , PARMJC      , PARMJFAC    , PARMJIADJ   
        REAL    PARMJIC     , PARU        , PARU2       , PARUE       , PARUEC      , PARURFR     , PD(0:PSX)   , PD2ADJ      
        REAL    PDADJ       , PDFS        , PDL(0:PSX)  , PECM        , PEGD        , PFGCAV      , PFGPAV(0:12), PFPCAV      
        REAL    PFPPAV(0:12), PGERM       , PGVAL       , PHINT       , PHINTFAC    , PHINTS      , PHOTQR      , PHSV        
        REAL    PHTV        , PLA         , PLAGS2      , PLAGSB2     , PLAGSB3     , PLAGSB4     , PLAS        , PLASI       
        REAL    PLASL       , PLASN       , PLASP       , PLASS       , PLASTMP     , PLASTMP2    , PLASW       , PLAX        
        REAL    PLMAGE      , PLPH        , PLTPOP      , PLTPOPE     , PLTPOPP     , PPEXP       , PPTHR       , PSDAPFR(PSX)
        REAL    PSDAYFR(PSX), PSTART(0:PSX)             , PTF         , PTFA        , PTFMN       , PTFMX       , PTTN        
        REAL    PTX         , RAINC       , RAINCC      , RAINPAV(0:12)             , RAINPC(0:12), RANC        , RATM        
        REAL    RB          , RCROP       , RDGAF       , RDGS        , RESCAL(0:20), RESLGAL(0:20)             , RESNAL(0:20)
        REAL    RESPC       , RESPRC      , RESPTC      , RESWAL(0:20), RESWALG(0:20)             
        REAL    RFAC        , RLDF(20)    , RLF         , RLFC        , RLFN        , RLFWU       , RLIGP       , RLWR        
        REAL    RM          , RMSE(30)    , RNAD        , RNAM        , RNAMM       , RNCM        , RNCMN(0:1)  , RNCR        
        REAL    RNCX        , RNCXS(0:1)  , RNDEM       , RNDEMG      , RNDEMTU     , RNH4U(20)   , RNO3U(20)   , RNPCMN(0:1) 
        REAL    RNPCS(0:1)  , RNUMX       , RNUSE(0:2)  , ROOTN       , ROOTNEXCESS , ROOTNS      , ROWSPC      , RRESP       
        REAL    RSCD        , RSCLX       , RSCM        , RSCMM       , RSCX        , RSEN        , RSFP        , RSFPL       
        REAL    RSFPU       , RSFRS       , RSGLFADJ    , RSN         , RSNAD       , RSNEED      , RSNPH       , RSNPHC      
        REAL    RSNUSED     , RSPCO       , RSSRWTGLFADJ, RSSRWTGSTADJ, RSUSE       , RSWAD       , RSWAM       , RSWAMM      
        REAL    RSWPH       , RSWPHC      , RSWT        , RSWTGLFADJ  , RSWTM       , RSWTPREV    , RSWTTMP     , RSWTX       
        REAL    RTDEP       , RTDEPG      , RTDEPTMP    , RTNH4       , RTNO3       , RTNSL(20)   , RTRESP      , RTRESPADJ   
        REAL    RTUFR       , RTWT        , RTWTAL(20)  , RTWTG       , RTWTGADJ    , RTWTGL(20)  , RTWTL(20)   , RTWTM       
        REAL    RTWTSL(20)  , RTWTUL(20)  , RUNOFFC     , RWAD        , RWAM        , RWAMM       , RWUMXI      , SAID        
        REAL    SANC        , SANCOUT     , SAWS        , SDCOAT      , SDDUR       , SDEPTH      , SDEPTHU     , SDNAD       
        REAL    SDNAP       , SDNC        , SDNPCI      , SDRATE      , SDRSF       , SDSZ        , SDWAD       , SDWAM       
        REAL    SEEDN       , SEEDNI      , SEEDNUSE    , SEEDNUSE2   , SEEDRS      , SEEDRSAV    , SEEDRSAVR   , SEEDRSI     
        REAL    SEEDUSE     , SEEDUSER    , SEEDUSET    , SENCAGS     , SENCAS      , SENCL(0:20) , SENCS       , SENFR       
        REAL    SENLA       , SENLAGS     , SENLALITTER , SENLAS      , SENLFG      , SENLFGRS    , SENLL(0:20) , SENLS       
        REAL    SENNAGS     , SENNAL(0:20), SENNAS      , SENNATC     , SENNATCM    , SENNGS      , SENNL(0:20) , SENNLFG     
        REAL    SENNLFGRS   , SENNS       , SENROOT     , SENROOTA    , SENRTG      , SENSTFR     , SENTOPLITTER
        REAL    SENTOPLITTERA             , SENTOPLITTERG             , SENWACM     , SENWACMM    , SENWAGS     , SENWAL(0:20)
        REAL    SENWALG(0:20)             , SENWL(0:20) , SERX        , SHGR(22)    , SHLA(25)    , SHLAG2(25)  , SHLAGB2(25) 
        REAL    SHLAGB3(25) , SHLAGB4(25) , SHLAS(25)   , SHNUM       , SHNUMAD     , SHNUML(LNUMX)             , SHRTD       
        REAL    SHRTM       , SHRTMM      , SLA         , SLAOUT      , SLIGP       , SMDFR       , SNAD        
        REAL    SNCM        , SNCMN(0:1)  , SNCR        , SNCX        , SNCXS(0:1)  , SNDEM       , SNDEMG      , SNDEMTU     
        REAL    SNH4(20)    , SNH4PROFILE , SNH4ROOTZONE, SNO3(20)    , SNO3PROFILE , SNO3ROOTZONE, SNPCMN(0:1) , SNPCS(0:1)  
        REAL    SNPH        , SNPHC       , SNUSE(0:2)  , SPRL        , SRAD20      , SRAD20S     , SRADC       , SRADCAV     
        REAL    SRADCC      , SRADD(20)   , SRADPAV(0:12)             , SRADPC      , SRADPREV    , SRANC       , SRDAYFR     
        REAL    SRFR        , SRNAD       , SRNAM       , SRNDEM      , SRNDEMG     , SRNDEMTU    , SRNOAD      , SRNOAM      
        REAL    SRNOAMM     , SRNOGM      , SRNOGMM     , SRNOW       , SRNPCM      , SRNPCS      , SRNUSE(0:2) , SROOTN      
        REAL    SRPRS       , SRWAD       , SRWT        , SRWTGRS     , SRWUD       , SRWUM       , SRWUMM      , STADJ       
        REAL    STAI        , STAIG       , STAIS       , STAISS      , STDAY       , STEMN       , STEMNEXCESS , STGEFR      
        REAL    STRESS(20)  , STRESS20    , STRESS20N   , STRESS20NS  , STRESS20S   , STRESS20W   , STRESS20WS  , STRESSN(20) 
        REAL    STRESSW(20) , STRSWAD     , STRSWT      , STWAD       , STWADOUT    , STWT        , STWTM       , SWFR        
        REAL    SWFRN       , SWFRNL      , SWFRPREV    , SWFRS       , SWFRX       , SWFRXL      , SWP(0:20)   , SWPH        
        REAL    SWPHC       , SWPLTD      , SWPLTH      , SWPLTL      , SWPRTIP     , SWPSD       , TCAN        , TCDIF       
        REAL    TDIFAV      , TDIFNUM     , TDIFSUM     , TFD         , TFDF        , TFDLF(LNUMX), TFDNEXT     ! TFAC4       , 
        REAL    TFG         , TFGEM       , TFGLF(LNUMX), TFLAW       , TFLFLIFE    , TFP         , TFV         , TFVAL       
        REAL    TIMENEED    , TLCHC       , TMAXCAV     , TMAXCC      , TMAXM       , TMAXPAV(0:12)             , TMAXPC      
        REAL    TMAXSUM     , TMAXX       , TMEAN       , TMEAN20     , TMEAN20P    , TMEAN20S    , TMEANAV(0:12)             
        REAL    TMEANCC     , TMEAND(20)  , TMEANE      , TMEANEC     , TMEANG      , TMEANGC     , TMEANNUM    , TMEANPC     
        REAL    TMEANSUM    , TMEANSURF   , TMINCAV     , TMINCC      , TMINM       , TMINN       , TMINPAV(0:12)             
        REAL    TMINPC      , TMINSUM     , TNAD        , TNAMM       , TNOXC       , TOFIXC      , TOMIN       , TOMINC      
        REAL    TOMINFOMC   , TOMINSOM1C  , TOMINSOM2C  , TOMINSOM3C  , TOMINSOMC   , TPAR        , TRATIO      , TRDV1(4)    
        REAL    TRDV2(4)    , TRGEM(4)    , TRLDF       , TRLFG(4)    , TRLV        , TRPHS(4)    , TRWU        , TSDEP       
        REAL    TSRAD       , TT          , TT20        , TT20S       , TTCUM       , TTD(20)     , TTGEM       , TTLFLIFE    
        REAL    TTNEED      , TTNEXT      , TTOUT       , TVR1        , TVR2        , TVR3        , TVR4        , TVR5        
        REAL    TVR6        , TWAD        , VALUER      , VANC        , VARSUM(30)  , VARVAL      , VCNC        , VMNC        
        REAL    VNAD        , VNAM        , VNAMM       , VNPCM       , VNPCMM      , VPD         , VPDFP       , VWAD        
        REAL    VWAM        , VWAMERR     , VWAMM       , WAVR        , WFEU        , WFG         , WFGCAV      , WFGCC       
        REAL    WFGE        , WFGEM       , WFGL        , WFGPAV(0:12), WFGPC       , WFGU        , WFLAW       , WFLF(LNUMX) 
        REAL    WFNU        , WFP         , WFPCAV      , WFPCC       , WFPL        , WFPPAV(0:12), WFPPC       , WFPU        
        REAL    WFRG        , WFRTG       , WFSU        , WTDEP       , WUPR        , WUPRD(20)   , XDEP        , XDEPL       
        REAL    XMIN        !, YVALXY      

!           Character variables:
    
        CHARACTER(LEN=1)  CFLAFLF   , CFLFAIL   , CFLHAR    , CFLHARMSG , CFLLFLIFE , CFLPDADJ  , CFLPRES   , CFLSDRSMSG
        CHARACTER(LEN=1)  CFLTFG    , EMFLAG    , ESTABLISHED           , FNAME     , GROUP     , HOP(HANUMX)           
        CHARACTER(LEN=1)  IDETD     , IFERI     , IHARI     , IPLTI     , ISWNITEARLY           , ISWWATCROP
        CHARACTER(LEN=1)  ISWWATEARLY           , MEEVP     , MEEXP     , MENU      , MEPHO     , MEWNU     , PLME      
        CHARACTER(LEN=1)  PSTYP(PSX), SEASENDOUT, SSTYP(SSX), TIERNUMC  
        CHARACTER(LEN=2)  CNCHAR2   , CROP      , CROPPREV  , HPROD     , PPSEN     
        CHARACTER(LEN=3)  FILEIOT   , MERNU     , MONTH     , OUT       
        CHARACTER(LEN=4)  MEDEV     
        CHARACTER(LEN=5)  PSABV(PSX), PSABVO(PSX)           , SSABV(SSX), SSABVO(SSX)           , TVTRDV    
        CHARACTER(LEN=6)  BRSTAGEC  , CAIC      , CANHTC    , DAPWRITE  , ECONO     , ECONOPREV , HIAMCHAR  , HIAMMCHAR 
        CHARACTER(LEN=6)  HINDC     , HINMCHAR  , HINMMCHAR , HNPCMCHAR , HNPCMMCHAR, HWUDC     , HWUMCHAR  , HWUMMCHAR 
        CHARACTER(LEN=6)  LAIC      , LAIPRODC  , LAIXCHAR  , LAIXMCHAR , LAPC      , LAPOTXC   , LAPSC     , LATL2C    
        CHARACTER(LEN=6)  LATL3C    , LATL4C    , LATLC     , SDWADC    , SENN0C    , SENNSC    , SENROOTC  
        CHARACTER(LEN=6)  SENTOPLITTERAC        , TCHAR     , THEAD(20) , VARNO     , VARNOPREV , VNPCMCHAR , VNPCMMCHAR
        CHARACTER(LEN=8)  MODEL     , MODNAME   , RUNRUNI   , VARCHAR   
        CHARACTER(LEN=10) CNCHAR    , DAPCHAR   , EXCODE    , EXCODEPREV            , TL10      , TNCHAR     ! TL10FROMI , 
        CHARACTER(LEN=12) CUFILE    , ECFILE    , OUTPG     , OUTPG2    , OUTPGF    , OUTPN     , SPFILE    
        CHARACTER(LEN=13) PSNAME(PSX)           , SSNAME(SSX)           
        CHARACTER(LEN=14) EVHEADER  
        CHARACTER(LEN=16) VRNAME    
        CHARACTER(LEN=25) RUNNAME   , TNAME     
        CHARACTER(LEN=35) GENFLCHK  
        CHARACTER(LEN=40) TRUNNAME  
        CHARACTER(LEN=60) ENAME     
        CHARACTER(LEN=64) SPDIRFLE  , SPDIRFLPREV           
        CHARACTER(LEN=78) MESSAGE(10)           
        CHARACTER(LEN=79) OUTHED    
        CHARACTER(LEN=80) FAPPLINE(30)          , LINESTAR  , LINESTAR2 , PATHCR   , PATHEC     , PATHSP    
        CHARACTER(LEN=93) CUDIRFLE  , CUDIRFLPREV           , ECDIRFLE  , ECDIRFLPREV           
        CHARACTER(LEN=107)FILEADIR  
        CHARACTER(LEN=120)CFGDFILE  , FILEA     , FILEIO    , FILENEW  , FILET      , FILEX     , FNAMEERA  , FNAMEERT  
        CHARACTER(LEN=120)FNAMEEVAL , FNAMELEAVES           , FNAMEMEAS, FNAMEOV    , FNAMEPHASES           
        CHARACTER(LEN=120)FNAMEPHENOLM          , FNAMEPHENOLS          , FNAMEPREM, FNAMEPRES  , FNAMEPSUM 
        CHARACTER(LEN=128)ARG       
        CHARACTER(LEN=180)LINET     , TLINET    , TLINETMP  
        CHARACTER(LEN=254)TLINEGRO  
        CHARACTER(LEN=354)LINEERA   
 
      ! RNMODE is a switch for run mode conveyed on the command line
      ! Options: I=Interactive, A=All treatments, B=Batch,
      ! E=Sensitivity, D=Debug, N=Seasonal, Q=Sequence, G=Gencalc

      ! For when simulating more than one species
      !REAL          sradip        ! Srad interception,whole can    %
      !REAL          sradipcn(5)   ! Srad interception,component    %

      ! For AFRC photosynthesis
      ! REAL PM,RA,RM,VPD,QPAR,ALPHA,RS,RP,PMAX,PHSA,PHSB,PGROSS


    
    END MODULE Module_CSCAS_Vars