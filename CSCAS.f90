!---------------------------------------------------------------------------------------------------------------------------
!  CROPSIM CASSAVA GROWTH AND DEVELOPMENT MODULE V2014-1  
!  Last edit 040414 LAH 
!  (After problems implementing fixes of GH 180214,260314;PM,MF 260214)
!  The code that follows was modified by MJF August, 2014. The code in the RUNINIT, SEASINIT
!  RATE and INTEGR section was extracted into subroutines. The RUNINIT and SEASINIT subroutines are still in fixed
!  format (.FOR), the remainder is in free format (.f90)
!
!  Control of execution comes from CSM, passing through LAND, PLANT and CSCAS_Interface.
!---------------------------------------------------------------------------------------------------------------------------
 
    SUBROUTINE CSCAS (FILEIOIN, RUN, TN, RN, RNMODE,      &!Command line
     ISWWAT, ISWNIT, ISWDIS, MESOM,                       &!Contols
     IDETS, IDETO, IDETG, IDETL, FROP,                    &!Controls
     SN, ON, RUNI, REP, YEAR, DOY, STEP, CN,              &!Run+loop
     SRAD, TMAX, TMIN, TAIRHR, RAIN, CO2, TDEW,           &!Weather
     DRAIN, RUNOFF, IRRAMT,                               &!Water
     DAYL, WINDSP, DEWDUR, CLOUDS, ST, EO, ES,            &!Weather
     NLAYR, DLAYR, DEPMAX, LL, DUL, SAT, BD, SHF, SLPF,   &!Soil states
     SW, NO3LEFT, NH4LEFT, FERNIT,                        &!H2o,N states
     TLCHD, TNIMBSOM, TNOXD,                              &!N components
     TOMINFOM, TOMINSOM, TOMINSOM1, TOMINSOM2, TOMINSOM3, &!N components
     YEARPLTCSM, HARVFRAC,                                &!Pl.date
     PARIP, PARIPA, EOP, EP, ET, TRWUP, ALBEDOS,          &!Resources
     CAID, KCAN, KEP,                                     &!States
     RLV, NFP, RWUPM, RWUMX, CANHT, LAIL, LAILA,          &!States
     UNO3, UNH4, UH2O,                                    &!Uptake
     SENCALG, SENNALG, SENLALG,                           &!Senescence
     RESCALG, RESNALG, RESLGALG,                          &!Residues
     STGYEARDOY, BRSTAGE,                                 &!Stage dates
     DYNAMIC, WEATHER)                                     !Control ! MF 31AU14 WEATHER ADDED

    USE CRSIMDEF
    USE ModuleDefs                                         ! MF 31AU14 ADDED FOR ACCESS TO WEATHER
      
    IMPLICIT NONE
    
    Type (WeatherType) WEATHER                             ! MF 31AU14 ADDED FOR ACCESS TO WEATHER

    INTEGER,PARAMETER::DINX =   3 ! Disease number,maximum
    INTEGER,PARAMETER::PSX  =  20 ! Principal stages,maximum
    INTEGER,PARAMETER::SSX  =  20 ! Secondary stages,maximum
    INTEGER,PARAMETER::KEYSTX = 9 ! Maximum number of key stages
    INTEGER,PARAMETER::DCNX =  10 ! Disease control #,maximum
    INTEGER,PARAMETER::LCNUMX=500 ! Maximum number of leaf cohorts
    INTEGER,PARAMETER::LNUMX= 500 ! Maximum number of leaves/axis
    INTEGER,PARAMETER::HANUMX= 40 ! Maximum # harvest instructions
!    INTEGER,PARAMETER::NL   =  20 ! Maximum number of soil layers

!    INTEGER,PARAMETER::RUNINIT=1  ! Program initiation indicator
!    INTEGER,PARAMETER::SEASINIT=2 ! Reinitialisation indicator
!    INTEGER,PARAMETER::RATE = 3   ! Program rate calc.indicator
!    INTEGER,PARAMETER::INTEGR=4   ! Program update indicator
!    INTEGER,PARAMETER::OUTPUT=5   ! Program output indicator
!    INTEGER,PARAMETER::SEASEND= 6 ! Program ending indicator

!    CHARACTER(LEN=1),PARAMETER::BLANK = ' '                              ! Defined in 
!    CHARACTER(LEN=3),PARAMETER::DASH = ' - '

    INTEGER ARGLEN   , CDAYS    , CN       , CNI      , COLNUM   , CSIDLAYR , CSTIMDIF , CSYDOY   
    INTEGER CSYEARDOY, CTRNUMPD , CWADCOL  , DAE      , DAP      , DAPCALC  , DAS      , DATE     
    INTEGER DATECOL  , DCDAT(DCNX)         , DCTAR(DCNX)         , DIDAT(DINX)         
    INTEGER DIDOY(DINX)         , DOM      , DOY      , DOYCOL   , DYNAMIC  , DYNAMICPREV         
    INTEGER EDAP     , EDAPM    , EDATM    , EDATMX   , EMDATERR , ERRNUM   , EVALOUT  , EVHEADNM 
    INTEGER EVHEADNMMAX         , EYEARDOY , FAPPNUM  , FDAY(200), FILELEN  , FLDAP    , FNUMERA  
    INTEGER FNUMERR  , FNUMERT  , FNUMEVAL , FNUMLVS  , FNUMMEAS , FNUMOV   , FNUMPHA  , FNUMPHEM 
    INTEGER FNUMPHES , FNUMPREM , FNUMPRES , FNUMPSUM , FNUMREA  , FNUMT    , FNUMTMP  , FNUMWRK  
    INTEGER FROP     , FROPADJ  , GDAP     , GDAPM    , GDATM    , GSTDCOL  , GYEARDOY , HADOY    
    INTEGER HANUM    , HAYEAR   , HDAY     , HDOYF    , HDOYL    , HFIRST   , HIADCOL  , HLAST    
    INTEGER HNUMACOL , HNUMBER  , HNUMECOL , HSTG     , HWADCOL  , HWTUCOL  , HYEAR    
    INTEGER HYEARDOY(HANUMX)    , HYEARF   , HYEARL   , HYRDOY(HANUMX)      , I        , IDETGNUM 
    INTEGER KEYPS(KEYSTX)       , KEYPSNUM , L        , L1       , L2       , LAIDCOL  
    INTEGER LBIRTHDAP(LCNUMX)   , LCNUM    , LDEATHDAP(LCNUMX)   , LENDIS   , LENENAME , LENGROUP 
    INTEGER LENLINE  , LENLINESTAR         , LENRNAME , LENTNAME , LINENUM  , LNUMCOL  , LNUMSG   
    INTEGER LNUMSOLDESTA        , LRTIP    , LSEED    , MDAP     , MDAPM    , MDAT     , MDATERR  
    INTEGER MDATM    , MDOY     , MSTG     , NLAYR    , NLAYRROOT, NOUTPG   , NOUTPG2  , NOUTPGF  
    INTEGER NOUTPN   , NSDAYS   , ON       , ONI      , OUTCHOICE, OUTCOUNT , PATHL    , PDATE    
    INTEGER PDAYS(0:12)         , PGDAP    , PGROCOL(20)         , PHINTSTG , PLDAY    , PLDAYTMP 
    INTEGER PLTOHARYR, PLYEAR   , PLYEARDOY, PLYEARDOYPREV       , PLYEARDOYT          
    INTEGER PLYEARREAD          , PLYEARTMP, PSDAP(PSX)          , PSDAPM(PSX)         
    INTEGER PSDAT(PSX)          , PSDATM(PSX)         , PSIDAP   , PSIDAPM  , PSIDATERR, PSNUM    
    INTEGER PWDINF   , PWDINL   , PWDOYF   , PWDOYL   , PWYEARF  , PWYEARL  , REP      , RN       
    INTEGER RNI      , RPCOL    , RTSLXDATE, RUN      , RUNCRP   , RUNI     , SHDAP    , SHDAT    
    INTEGER SN       , SNI      , SRNOPD   , STARNUM  , STARNUMM , STARNUMO , STARTPOS , STEP     
    INTEGER STEPNUM  , STGEDAT  , STGYEARDOY(20)      , TDATANUM , TFCOLNUM , TFDAP    , TFDAPCOL 
    INTEGER TIERNUM  , TLINENUM , TLPOS    , TN       , TNI      , TVI1     , TVI2     , TVI3     
    INTEGER TVI4     , TVICOLNM , TVILENT  , VALUEI   , VARNUM(30)          , WSDAYS               ! VERSION REMOVED
    INTEGER YEAR     , YEARCOL  , YEARDOY  , YEARDOYHARF         , YEARDOYPREV         , YEARM    
    INTEGER YEARPLTCSM          , YEARSIM 
    
    REAL    AFLF(0:LNUMX)       , AH2OPROFILE         , AH2OROOTZONE        , ALBEDO   , ALBEDOS  
    REAL    AMTNIT   , AMTNITPREV          , ANDEM    , ANFER(200)          , AREAPOSSIBLE        
    REAL    AREAPOSSIBLEN       , AVGSW    , BASELAYER, BD(20)   , BRFX(PSX), BRNUMSH  , BRNUMSHM 
    REAL    BRNUMST  , BRNUMSTPREV         , BRSTAGE  , BRSTAGEPREV         , BRSTAGETMP          
    REAL    CAID     , CANHT    , CANHTG   , CANHTS   , CARBOADJ , CARBOBEG , CARBOBEGI
    REAL    CARBOBEGIA          , CARBOBEGM, CARBOBEGR, CARBOC   , CARBOEND , CARBOR   , CARBOT   
    REAL    CARBOTMP , CARBOTMPI, CARBOTMPM, CARBOTMPR, CHTPC(10), CLAPC(10), CLOUDS   , CNAD     
    REAL    CNADPREV , CNADSTG(20)         , CNAM     , CNAMERR  , CNAMM    , CNCTMP   , CNPCHM   
    REAL    CO2      , CO2AIR   , CO2CAV   , CO2CC    , CO2COMPC , CO2EX    , CO2F(10) , CO2FP    
    REAL    CO2FPI   , CO2INT   , CO2INTPPM, CO2INTPPMP          , CO2MAX   , CO2PAV(0:12)        
    REAL    CO2PC    , CO2RF(10), CRFR     , CRRSWAD  , CRRSWT   , CRWAD    , CRWADOUT , CRWT     
    REAL    CRWTM    , CSVPSAT  , CSYVAL   , CUMDEP   , CUMDU    , CUMSW    , CWAD     , CWADPREV 
    REAL    CWADSTG(20)         , CWADT    , CWAHC    , CWAHCM   , CWAM     , CWAMERR  , CWAMM    
    REAL    CWAN(HANUMX)        , DALF(0:LNUMX)       , DAYL     , DAYLCAV  , DAYLCC   
    REAL    DAYLPAV(0:12)       , DAYLPC   , DAYLPREV , DAYLS(0:10)         , DAYLST(0:12)        
    REAL    DAYSUM   , DCDUR(DCNX)         , DCFAC(DCNX)         , DEPMAX   , DEWDUR   , DF       
    REAL    DFNEXT   , DFOUT    , DFPE     , DGLF(0:LNUMX)       , DIFFACR(DINX)       
    REAL    DIGFAC(DINX)        , DLAYR(20), DLAYRTMP(20)        , DMP_EP   , DMP_ET   , DMP_IRR  
    REAL    DMP_NAPP , DMP_NUPT , DMP_RAIN , DRAIN    , DRAINC   , DSLF(0:LNUMX)       , DSTAGE   
    REAL    DTRY     , DU       , DUL(20)  , DUNEED   , DUPHASE  , DUPNEXT  , DUSRI    , DUTOMSTG 
    REAL    EARLYN   , EARLYW   , EDAPFR   , EDAYFR   , EMRGFR   , EMRGFRPREV          , EO       
    REAL    EOC      , EOEBUD   , EOEBUDC  , EOEBUDCRP, EOEBUDCRPC          , EOEBUDCRPCO2        
    REAL    EOEBUDCRPCO2C       , EOEBUDCRPCO2H2O     , EOEBUDCRPCO2H2OC    , EOMPCRP  , EOMPCRPC 
    REAL    EOMPCRPCO2          , EOMPCRPCO2C         , EOMPCRPCO2H2O       , EOMPCRPCO2H2OC      
    REAL    EOMPEN   , EOMPENC  , EOP      , EOPEN    , EOPENC   , EOPT     , EOPTC    , EP       
    REAL    EPCC     , EPPC(0:12)          , EPSRATIO , ERRORVAL , ES       , ET       , ETCC     
    REAL    ETPC(0:12)          , FAC(20)  , FERNIT   , FERNITPREV          , FLN      , FNH4     
    REAL    FNO3     , FSOILH2O , FSOILN   , GDAPFR   , GDAYFR   , GEDAYSE  , GEDAYSG  , GERMFR   
    REAL    GESTAGE  , GESTAGEPREV         , GEUCUM   , GROCR    , GROCRADJ , GROCRFR  , GROLF    
    REAL    GROLFADJ , GROLFNEEDED         , GROLFP   , GROLS    , GROLSA   , GROLSP   , GROLSRS  
    REAL    GROLSRT  , GROLSRTN , GROLSSD  , GROLSSEN , GRORS    , GRORSADJ , GROSR    , GROST    
    REAL    GROSTADJ , GROSTCR  , GROSTCRP , GROSTCRPSTORE       , GRP_EP   , GRP_ET   , GRP_IRR  
    REAL    GRP_NAPP , GRP_NUPT , GRP_RAIN , H2OA     , H2OCF    , H2OPROFILE          
    REAL    H2OROOTZONE         , HAFR     , HAMT(HANUMX)        , HARVFRAC(2)         , HAWAD    
    REAL    HBPC(HANUMX)        , HBPCF    , HDUR     , HIAD     , HIADT    , HIAM     , HIAMERR  
    REAL    HIAMM    , HIAMMTMP , HIND     , HINM     , HINMM    , HMPC     , HNAD     , HNAM     
    REAL    HNAMERR  , HNAMM    , HNC      , HNPCM    , HNPCMERR , HNPCMM   , HNUMAD   , HNUMAERR 
    REAL    HNUMAM   , HNUMAMM  , HNUMAT   , HNUMET   , HNUMGERR , HNUMGM   , HNUMGMM  , HNUMPM   
    REAL    HNUMPMM  , HPC(HANUMX)         , HPCF     , HPRODN   , HSTAGE   , HWAD     , HWADT    
    REAL    HWAHERR  , HWAHM    , HWAM     , HWAMM    , HWUD     , HWUM     , HWUMERR  , HWUMM    
    REAL    HWUMYLD  , HWUT     , HYAMM    , ICWD     , IRRAMT   , IRRAMTC  , ISOILH2O , ISOILN   
    REAL    KCAN     , KCANI    , KEP      , KEPI     , LA1S     , LAFND    , LAFS     
    REAL    LAGEG(0:LNUMX)      , LAGEP(0:LNUMX)      , LAGETT(0:LNUMX)     , LAGL(1,LNUMX)       
    REAL    LAI      , LAIA     , LAIL(30) , LAILA(30), LAIPREV  , LAIPROD  , LAISTG(20)          
    REAL    LAIX     , LAIXERR  , LAIXM    , LAIXT    , LAIXX    , LANC     , LANCRS   
    REAL    LAP(0:LNUMX)        , LAPD     , LAPH     , LAPHC    , LAPOTX(LNUMX)       
    REAL    LAPP(LNUMX)         , LAPS(LNUMX)         , LAPSTMP  , LATL(1,LNUMX)       
    REAL    LATL2(1,LNUMX)      , LATL3(1,LNUMX)      , LATL4(1,LNUMX)      , LATLPOT(1,LNUMX)    
    REAL    LATLPREV(1,LNUMX)   , LAWCF    , LAWFF    , LAWL(2)  , LAWMNFR  , LAWS     , LAWTR    
    REAL    LAWTS    , LAWWR    , LAXN2    , LAXNO    , LAXS     , LCOA(LCNUMX)        
    REAL    LCOAS(LCNUMX)       , LEAFN    , LEAFNEXCESS         , LFGSDU   , LFWT     , LFWTM    
    REAL    LGPHASE(2)          , LGPHASEDU(2)        , LL(20)   , LLIFA    , LLIFATT  , LLIFG    
    REAL    LLIFGTT  , LLIFS    , LLIFSTT  , LLIFX    , LLIFXUNUSED         , LLIGP    , LLNAD    
    REAL    LLOSA    , LLRSWAD  , LLRSWT   , LLWAD    , LLWADOUT , LNCGL    , LNCGU    , LNCM     
    REAL    LNCMN(0:1)          , LNCPL    , LNCPU    , LNCR     , LNCSEN   , LNCSENF  , LNCX     
    REAL    LNCXS(0:1)          , LNDEM    , LNDEM2   , LNDEMG   , LNDEMTU  , LNPCMN(0:1)         
    REAL    LNPCS(0:1)          , LNPH     , LNPHC    , LNUM     , LNUMEND  , LNUMG    , LNUMNEED 
    REAL    LNUMPREV , LNUMSERR , LNUMSIMTOSTG(0:PSX) , LNUMSM   , LNUMSMM  , LNUMSTG(20)         
    REAL    LNUMT    , LNUMTMP  , LNUMTOSTG(0:PSX)    , LNUSE(0:3)          , LPEAI    , LPEAW    
    REAL    LPEFR    , LPERSWAD , LPERSWT  , LPEWAD   , LSENI    , LSNUM(HANUMX)       , LSTAGE   
    REAL    LSWT(HANUMX)        , LWLOS    , LWPH     , LWPHC    , MDAPFR   , MDATT    , MDAYFR   
    REAL    MJPERE   , NCRG     , NDEM2    , NDEMG    , NDEMMN   , NFG      , NFGCAV   , NFGCC    
    REAL    NFGL     , NFGPAV(0:12)        , NFGPC    , NFGU     , NFLF(LNUMX)         
    REAL    NFLF2(0:LNUMX)      , NFLFP(LNUMX)        , NFP      , NFPCAV   , NFPCC    , NFPL     
    REAL    NFPPAV(0:12)        , NFPPC    , NFPU     , NFRG     , NFSU     , NH4CF    , NH4FN    
    REAL    NH4LEFT(20)         , NH4MN    , NLABPC   , NLLG     , NO3CF    , NO3FN    
    REAL    NO3LEFT(20)         , NO3MN    , NPOOLL   , NPOOLR   , NPOOLS   , NTUPF    , NUF      
    REAL    NULEFT   , NUPAC    , NUPACM   , NUPAD    , NUPAP    , NUPAPCRP , NUPAPCSM , NUPAPCSM1
    REAL    NUPC     , NUPD     , NUPRATIO , NUSEFAC  , PARFC    , PARI     , PARI1    , PARIOUT  
    REAL    PARIP    , PARIPA   , PARIPREV , PARIUE   , PARIUED  , PARIX    , PARMJC   , PARMJFAC 
    REAL    PARMJIADJ, PARMJIC  , PARU     , PARU2    , PARUE    , PARUEC   , PARURFR  , PD(0:PSX)
    REAL    PD2ADJ   , PDADJ    , PDFS     , PDL(0:PSX)          , PECM     , PEGD     , PFGCAV   
    REAL    PFGPAV(0:12)        , PFPCAV   , PFPPAV(0:12)        , PGERM    , PGVAL    , PHINT    
    REAL    PHINTFAC , PHINTS   , PHOTQR   , PHSV     , PHTV     , PLA      , PLAGS2   , PLAGSB2  
    REAL    PLAGSB3  , PLAGSB4  , PLAS     , PLASI    , PLASL    , PLASN    , PLASP    , PLASS    
    REAL    PLASTMP  , PLASTMP2 , PLASW    , PLAX     , PLMAGE   , PLPH     , PLTPOP   , PLTPOPE  
    REAL    PLTPOPP  , PPEXP    , PPTHR    , PSDAPFR(PSX)        , PSDAYFR(PSX)        
    REAL    PSTART(0:PSX)       , PTF      , PTFA     , PTFMN    , PTFMX    , PTTN     , PTX      
    REAL    RAIN     , RAINC    , RAINCC   , RAINPAV(0:12)       , RAINPC(0:12)        , RANC     
    REAL    RATM     , RB       , RCROP    , RDGAF    , RDGS     , RESCAL(0:20)        
    REAL    RESCALG(0:20)       , RESLGAL(0:20)       , RESLGALG(0:20)      , RESNAL(0:20)        
    REAL    RESNALG(0:20)       , RESPC    , RESPRC   , RESPTC   , RESWAL(0:20)        
    REAL    RESWALG(0:20)       , RFAC     , RLDF(20) , RLF      , RLFC     , RLFN     , RLFWU    
    REAL    RLIGP    , RLV(20)  , RLWR     , RM       , RMSE(30) , RNAD     , RNAM     , RNAMM    
    REAL    RNCM     , RNCMN(0:1)          , RNCR     , RNCX     , RNCXS(0:1)          , RNDEM    
    REAL    RNDEMG   , RNDEMTU  , RNH4U(20), RNO3U(20), RNPCMN(0:1)         , RNPCS(0:1)          
    REAL    RNUMX    , RNUSE(0:2)          , ROOTN    , ROOTNEXCESS         , ROOTNS   , ROWSPC   
    REAL    RRESP    , RSCD     , RSCLX    , RSCM     , RSCMM    , RSCX     , RSEN     , RSFP     
    REAL    RSFPL    , RSFPU    , RSFRS    , RSGLFADJ , RSN      , RSNAD    , RSNEED   , RSNPH    
    REAL    RSNPHC   , RSNUSED  , RSPCO    , RSSRWTGLFADJ        , RSSRWTGSTADJ        , RSUSE    
    REAL    RSWAD    , RSWAM    , RSWAMM   , RSWPH    , RSWPHC   , RSWT     , RSWTGLFADJ          
    REAL    RSWTM    , RSWTPREV , RSWTTMP  , RSWTX    , RTDEP    , RTDEPG   , RTDEPTMP , RTNH4    
    REAL    RTNO3    , RTNSL(20), RTRESP   , RTRESPADJ, RTUFR    , RTWT     , RTWTAL(20)          
    REAL    RTWTG    , RTWTGADJ , RTWTGL(20)          , RTWTL(20), RTWTM    , RTWTSL(20)          
    REAL    RTWTUL(20)          , RUNOFF   , RUNOFFC  , RWAD     , RWAM     , RWAMM    , RWUMX    
    REAL    RWUMXI   , RWUPM    , SAID     , SANC     , SANCOUT  , SAT(20)  , SAWS     , SDCOAT   
    REAL    SDDUR    , SDEPTH   , SDEPTHU  , SDNAD    , SDNAP    , SDNC     , SDNPCI   , SDRATE   
    REAL    SDRSF    , SDSZ     , SDWAD    , SDWAM    , SEEDN    , SEEDNI   , SEEDNUSE , SEEDNUSE2
    REAL    SEEDRS   , SEEDRSAV , SEEDRSAVR, SEEDRSI  , SEEDUSE  , SEEDUSER , SEEDUSET , SENCAGS  
    REAL    SENCALG(0:20)       , SENCAS   , SENCL(0:20)         , SENCS    , SENFR    , SENLA    
    REAL    SENLAGS  , SENLALG(0:20)       , SENLALITTER         , SENLAS   , SENLFG   , SENLFGRS 
    REAL    SENLL(0:20)         , SENLS    , SENNAGS  , SENNAL(0:20)        , SENNALG(0:20)       
    REAL    SENNAS   , SENNATC  , SENNATCM , SENNGS   , SENNL(0:20)         , SENNLFG  , SENNLFGRS
    REAL    SENNS    , SENROOT  , SENROOTA , SENRTG   , SENSTFR  , SENTOPLITTER        
    REAL    SENTOPLITTERA       , SENTOPLITTERG       , SENWACM  , SENWACMM , SENWAGS  
    REAL    SENWAL(0:20)        , SENWALG(0:20)       , SENWL(0:20)         , SERX     , SHF(20)  
    REAL    SHGR(22) , SHLA(25) , SHLAG2(25)          , SHLAGB2(25)         , SHLAGB3(25)         
    REAL    SHLAGB4(25)         , SHLAS(25), SHNUM    , SHNUMAD  , SHNUML(LNUMX)       , SHRTD    
    REAL    SHRTM    , SHRTMM   , SLA      , SLAOUT   , SLIGP    , SLPF     , SMDFR    , SNAD     
    REAL    SNCM     , SNCMN(0:1)          , SNCR     , SNCX     , SNCXS(0:1)          , SNDEM    
    REAL    SNDEMG   , SNDEMTU  , SNH4(20) , SNH4PROFILE         , SNH4ROOTZONE        , SNO3(20) 
    REAL    SNO3PROFILE         , SNO3ROOTZONE        , SNPCMN(0:1)         , SNPCS(0:1)          
    REAL    SNPH     , SNPHC    , SNUSE(0:2)          , SPRL     , SRAD     , SRAD20   , SRAD20S  
    REAL    SRADC    , SRADCAV  , SRADCC   , SRADD(20), SRADPAV(0:12)       , SRADPC   , SRADPREV 
    REAL    SRANC    , SRDAYFR  , SRFR     , SRNAD    , SRNAM    , SRNDEM   , SRNDEMG  , SRNDEMTU 
    REAL    SRNOAD   , SRNOAM   , SRNOAMM  , SRNOGM   , SRNOGMM  , SRNOW    , SRNPCM   , SRNPCS   
    REAL    SRNUSE(0:2)         , SROOTN   , SRPRS    , SRWAD    , SRWT     , SRWTGRS  , SRWUD    
    REAL    SRWUM    , SRWUMM   , ST(0:NL) , STADJ    , STAI     , STAIG    , STAIS    , STAISS   
    REAL    STDAY    , STEMN    , STEMNEXCESS         , STGEFR   , STRESS(20)          , STRESS20 
    REAL    STRESS20N, STRESS20NS          , STRESS20S, STRESS20W, STRESS20WS          
    REAL    STRESSN(20)         , STRESSW(20)         , STRSWAD  , STRSWT   , STWAD    , STWADOUT 
    REAL    STWT     , STWTM    , SW(20)   , SWFR     , SWFRN    , SWFRNL   , SWFRPREV , SWFRS    
    REAL    SWFRX    , SWFRXL   , SWP(0:20), SWPH     , SWPHC    , SWPLTD   , SWPLTH   , SWPLTL   
    REAL    SWPRTIP  , SWPSD    , TAIRHR(24)          , TCAN     , TCDIF    , TDEW     , TDIFAV   
    REAL    TDIFNUM  , TDIFSUM  , TFAC4    , TFD      , TFDF     , TFDLF(LNUMX)        , TFDNEXT  
    REAL    TFG      , TFGEM    , TFGLF(LNUMX)        , TFLAW    , TFLFLIFE , TFP      , TFV      
    REAL    TFVAL    , TIMENEED , TLCHC    , TLCHD    , TMAX     , TMAXCAV  , TMAXCC   , TMAXM    
    REAL    TMAXPAV(0:12)       , TMAXPC   , TMAXSUM  , TMAXX    , TMEAN    , TMEAN20  , TMEAN20P 
    REAL    TMEAN20S , TMEANAV(0:12)       , TMEANCC  , TMEAND(20)          , TMEANE   , TMEANEC  
    REAL    TMEANG   , TMEANGC  , TMEANNUM , TMEANPC  , TMEANSUM , TMEANSURF, TMIN     , TMINCAV  
    REAL    TMINCC   , TMINM    , TMINN    , TMINPAV(0:12)       , TMINPC   , TMINSUM  , TNAD     
    REAL    TNAMM    , TNIMBSOM , TNOXC    , TNOXD    , TOFIXC   , TOMIN    , TOMINC   , TOMINFOM 
    REAL    TOMINFOMC, TOMINSOM , TOMINSOM1, TOMINSOM1C          , TOMINSOM2, TOMINSOM2C          
    REAL    TOMINSOM3, TOMINSOM3C          , TOMINSOMC, TPAR     , TRATIO   , TRDV1(4) , TRDV2(4) 
    REAL    TRGEM(4) , TRLDF    , TRLFG(4) , TRLV     , TRPHS(4) , TRWU     , TRWUP    , TSDEP    
    REAL    TSRAD    , TT       , TT20     , TT20S    , TTCUM    , TTD(20)  , TTGEM    , TTLFLIFE 
    REAL    TTNEED   , TTNEXT   , TTOUT    , TVR1     , TVR2     , TVR3     , TVR4     , TVR5     
    REAL    TVR6     , TWAD     , UH2O(NL) , UNH4(20) , UNO3(20) , VALUER   , VANC     
    REAL    VARSUM(30)          , VARVAL   , VCNC     , VMNC     , VNAD     , VNAM     , VNAMM    
    REAL    VNPCM    , VNPCMM   , VPD      , VPDFP    , VWAD     , VWAM     , VWAMERR  , VWAMM    
    REAL    WAVR     , WFEU     , WFG      , WFGCAV   , WFGCC    , WFGE     , WFGEM    , WFGL     
    REAL    WFGPAV(0:12)        , WFGPC    , WFGU     , WFLAW    , WFLF(LNUMX)         , WFNU     
    REAL    WFP      , WFPCAV   , WFPCC    , WFPL     , WFPPAV(0:12)        , WFPPC    , WFPU     
    REAL    WFRG     , WFRTG    , WFSU     , WINDSP   , WTDEP    , WUPR     , WUPRD(20), XDEP     
    REAL    XDEPL    , XMIN     , YVALXY  
    
    CHARACTER (LEN=1)   CFLAFLF     , CFLFAIL     , CFLHAR      , CFLHARMSG   , CFLLFLIFE   , CFLPDADJ    , CFLPRES     
    CHARACTER (LEN=1)   CFLSDRSMSG  , CFLTFG      , EMFLAG      , ESTABLISHED , FNAME       , GROUP       , HOP(HANUMX) 
    CHARACTER (LEN=1)   IDETD       , IDETG       , IDETL       , IDETO       , IDETS       , IFERI       , IHARI       
    CHARACTER (LEN=1)   IPLTI       , ISWDIS      , ISWNIT      , ISWNITEARLY , ISWWAT      , ISWWATCROP  , ISWWATEARLY 
    CHARACTER (LEN=1)   MEEVP       , MEEXP       , MENU        , MEPHO       , MESOM       , MEWNU       , PLME        
    CHARACTER (LEN=1)   PSTYP(PSX)  , RNMODE      , SEASENDOUT  , SSTYP(SSX)  , TIERNUMC    
    CHARACTER (LEN=2)   CNCHAR2     , CROP        , CROPPREV    , HPROD       , PPSEN       
    CHARACTER (LEN=3)   FILEIOT     , MERNU       , MONTH       , OUT         
    CHARACTER (LEN=4)   MEDEV       
    CHARACTER (LEN=5)   PSABV(PSX)  , PSABVO(PSX) , SSABV(SSX)  , SSABVO(SSX) , TVTRDV      
    CHARACTER (LEN=6)   BRSTAGEC    , CAIC        , CANHTC      , DAPWRITE    , ECONO       , ECONOPREV   , HIAMCHAR    
    CHARACTER (LEN=6)   HIAMMCHAR   , HINDC       , HINMCHAR    , HINMMCHAR   , HNPCMCHAR   , HNPCMMCHAR  , HWUDC       
    CHARACTER (LEN=6)   HWUMCHAR    , HWUMMCHAR   , LAIC        , LAIPRODC    , LAIXCHAR    , LAIXMCHAR   , LAPC        
    CHARACTER (LEN=6)   LAPOTXC     , LAPSC       , LATL2C      , LATL3C      , LATL4C      , LATLC       , SDWADC      
    CHARACTER (LEN=6)   SENN0C      , SENNSC      , SENROOTC    , SENTOPLITTERAC            , TCHAR       , THEAD(20)          
    CHARACTER (LEN=6)   VARNO       , VARNOPREV   , VNPCMCHAR   , VNPCMMCHAR  
    CHARACTER (LEN=8)   MODEL       , MODNAME     , RUNRUNI     , VARCHAR     
    CHARACTER (LEN=10)  CNCHAR      , DAPCHAR     , EXCODE      , EXCODEPREV  , TL10        , TL10FROMI   , TNCHAR      
    CHARACTER (LEN=12)  CUFILE      , ECFILE      , OUTPG       , OUTPG2      , OUTPGF      , OUTPN       , SPFILE      
    CHARACTER (LEN=13)  PSNAME(PSX) , SSNAME(SSX) 
    CHARACTER (LEN=14)  EVHEADER    
    CHARACTER (LEN=16)  VRNAME      
    CHARACTER (LEN=25)  RUNNAME     , TNAME       
    CHARACTER (LEN=35)  GENFLCHK    
    CHARACTER (LEN=40)  TRUNNAME    
    CHARACTER (LEN=60)  ENAME       
    CHARACTER (LEN=64)  SPDIRFLE    , SPDIRFLPREV 
    CHARACTER (LEN=78)  MESSAGE(10) 
    CHARACTER (LEN=79)  OUTHED      
    CHARACTER (LEN=80)  FAPPLINE(30), LINESTAR    , LINESTAR2   , PATHCR      , PATHEC      , PATHSP      
    CHARACTER (LEN=93)  CUDIRFLE    , CUDIRFLPREV , ECDIRFLE    , ECDIRFLPREV 
    CHARACTER (LEN=107) FILEADIR    
    CHARACTER (LEN=120) CFGDFILE    , FILEA       , FILEIO      , FILEIOIN    , FILENEW     , FILET       , FILEX       
    CHARACTER (LEN=120) FNAMEERA    , FNAMEERT    , FNAMEEVAL   , FNAMELEAVES , FNAMEMEAS   , FNAMEOV     , FNAMEPHASES 
    CHARACTER (LEN=120) FNAMEPHENOLM, FNAMEPHENOLS, FNAMEPREM   , FNAMEPRES   , FNAMEPSUM   
    CHARACTER (LEN=128) ARG         
    CHARACTER (LEN=180) LINET       , TLINET      , TLINETMP    
    CHARACTER (LEN=254) TLINEGRO    
    CHARACTER (LEN=354) LINEERA     

    LOGICAL FEXIST   , FEXISTA  , FEXISTT  , FFLAG    , FFLAGEC  , FOPEN         
      ! Arrays for passing variables to OPSUM subroutine, CSM model only
    INTEGER,      PARAMETER :: SUMNUM = 37
    CHARACTER*5,  DIMENSION(SUMNUM) :: LABEL
    REAL,         DIMENSION(SUMNUM) :: VALUE
    
    

    INTRINSIC AMAX1,AMIN1,EXP,FLOAT,INDEX,INT,LEN,MAX,MIN,MOD,NINT,SQRT,ABS,TRIM

    ! RNMODE is a switch for run mode conveyed on the command line
    ! Options: I=Interactive, A=All treatments, B=Batch,
    ! E=Sensitivity, D=Debug, N=Seasonal, Q=Sequence, G=Gencalc

    ! For when simulating more than one species
    !REAL          sradip        ! Srad interception,whole can    %
    !REAL          sradipcn(5)   ! Srad interception,component    %

    ! For AFRC photosynthesis
    ! REAL PM,RA,RM,VPD,QPAR,ALPHA,RS,RP,PMAX,PHSA,PHSB,PGROSS

    SAVE
    !***********************************************************************************************************************
    IF (DYNAMIC.EQ.RUNINIT) THEN    ! Initialization
    !***********************************************************************************************************************
    !-----------------------------------------------------------------------------------------------------------------------
    !     Run initialization procedure in SUBROUTINE CS_RUNINIT
    !     SUBROUTINE CS_RUNIT takes all the code from CSCAS lines 1471 - 1813.
    !-----------------------------------------------------------------------------------------------------------------------


    CALL CS_RUNINIT( &
    CFGDFILE    , CFLAFLF     , CFLTFG      , CN          , CROPPREV    , CUDIRFLPREV , DOY         , ECDIRFLPREV , &
    ECONOPREV   , EXCODE      , FILEIO      , FILEIOIN    , FILEIOT     , FNAME       , FNAMEERA    , FNAMEERT    , &
    FNAMEEVAL   , FNAMELEAVES , FNAMEMEAS   , FNAMEOV     , FNAMEPHASES , FNAMEPHENOLM, FNAMEPHENOLS, FNAMEPREM   , &
    FNAMEPRES   , FNAMEPSUM   , FNUMERA     , FNUMERR     , FNUMERT     , FNUMEVAL    , FNUMLVS     , FNUMMEAS    , &
    FNUMOV      , FNUMPHA     , FNUMPHEM    , FNUMPHES    , FNUMPREM    , FNUMPRES    , FNUMPSUM    , FNUMT       , &
    FNUMTMP     , FNUMWRK     , FROP        , FROPADJ     , GENFLCHK    , IDETD       , IDETL       , ISWNIT      , &
    ISWWATCROP  , MEDEV       , MERNU       , MJPERE      , MODEL       , MODNAME     , NOUTPG      , NOUTPG2     , &
    NOUTPGF     , NOUTPN      , ON          , OUTPG       , OUTPG2      , OUTPGF      , OUTPN       , PARMJFAC    , &
    RN          , RNMODE      , RUN         , RUNCRP      , SEASENDOUT  , SN          , STDAY       , STEPNUM     , &
    TN          , VARNOPREV   , YEAR        , YEARSIM &                                                                ! REMOVED VERSION
    )

    !***********************************************************************************************************************
    ELSEIF (DYNAMIC.EQ.SEASINIT) THEN    ! Initialization
    !***********************************************************************************************************************
      
    !-----------------------------------------------------------------------------------------------------------------------
    !     Initialize both state and rate variables in SUBROUTINE VARINIT.
    !     SUBROUTINE VARINIT takes all the code from CSCAS lines 1819 - 2327.                   
    !-----------------------------------------------------------------------------------------------------------------------

    CALL VARINIT( &
    AFLF        , AMTNIT      , ANDEM       , BRNUMST     , BRSTAGE     , BRSTAGEPREV , CAID        , CANHT       , &
    CANHTG      , CARBOADJ    , CARBOBEG    , CARBOBEGI   , CARBOBEGM   , CARBOBEGR   , CARBOC      , CARBOEND    , &
    CARBOR      , CARBOT      , CDAYS       , CFLFAIL     , CFLHARMSG   , CFLLFLIFE   , CFLSDRSMSG  , CNAD        , &
    CNADPREV    , CNADSTG     , CNAM        , CNAMM       , CO2CC       , CO2FP       , CO2INTPPM   , CO2INTPPMP  , &
    CO2MAX      , CO2PAV      , CO2PC       , CRRSWT      , CRWAD       , CRWT        , CUMDU       , CWAD        , &
    CWADPREV    , CWADSTG     , CWAHC       , CWAHCM      , CWAM        , CWAMM       , DAE         , DALF        , &
    DAP         , DAYLCC      , DAYLPAV     , DAYLPC      , DAYLST      , DAYSUM      , DEWDUR      , DF          , &
    DFOUT       , DGLF        , DRAINC      , DSLF        , DSTAGE      , DU          , DUNEED      , DYNAMICPREV , &
    EDAP        , EDAPFR      , EDAPM       , EDAYFR      , EMFLAG      , EMRGFR      , EMRGFRPREV  , EOC         , &
    EOEBUD      , EOEBUDC     , EOEBUDCRP   , EOEBUDCRPC  , EOEBUDCRPCO2, EOEBUDCRPCO2C             , &
    EOEBUDCRPCO2H2O           , EOEBUDCRPCO2H2OC          , EOMPCRP     , EOMPCRPC    , EOMPCRPCO2  , EOMPCRPCO2C , &
    EOMPCRPCO2H2O             , EOMPCRPCO2H2OC            , EOMPEN      , EOMPENC     , EOPEN       , EOPENC      , &
    EOPT        , EOPTC       , EPCC        , EPSRATIO    , ESTABLISHED , ETCC        , EYEARDOY    , FAPPLINE    , &
    FAPPNUM     , FERNITPREV  , FLDAP       , FLN         , GDAP        , GDAPM       , GDAYFR      , GEDAYSE     , &
    GEDAYSG     , GERMFR      , GESTAGE     , GESTAGEPREV , GEUCUM      , GROCR       , GROCRADJ    , GROLF       , &
    GROLFADJ    , GRORS       , GROSR       , GROST       , GROSTADJ    , GROSTCR     , GYEARDOY    , H2OCF       , &
    HAMT        , HANUMX      , HBPC        , HBPCF       , HIAD        , HIAM        , HIAMM       , HIND        , &
    HINM        , HINMM       , HNAD        , HNAM        , HNAMM       , HNPCM       , HNPCMM      , HNUMAM      , &
    HNUMAMM     , HNUMBER     , HNUMGM      , HNUMGMM     , HOP         , HPC         , HPCF        , HSTAGE      , &
    HWAM        , HWAMM       , HWUM        , HWUMMCHAR   , HYEARDOY    , HYRDOY      , IDETGNUM    , IRRAMTC     , &
    LAGEP       , LAGETT      , LAGL        , LAI         , LAIL        , LAILA       , LAIPREV     , LAISTG      , &
    LAIX        , LAIXM       , LANC        , LAP         , LAPHC       , LAPP        , LAPS        , LATL        , &
    LATL2       , LATL3       , LATL4       , LCNUM       , LCNUMX      , LCOA        , LCOAS       , LEAFN       , &
    LFWT        , LFWTM       , LLNAD       , LLRSWAD     , LLRSWT      , LLWAD       , LNCR        , LNCX        , &
    LNDEM       , LNPHC       , LNUM        , LNUMG       , LNUMPREV    , LNUMSG      , LNUMSM      , LNUMSMM     , &
    LNUMSTG     , LNUMX       , LNUSE       , LPEAI       , LPERSWAD    , LPERSWT     , LPEWAD      , LSEED       , &
    LSTAGE      , LWPHC       , MDAP        , MDAT        , MDAYFR      , MDOY        , NFG         , NFGCC       , &
    NFGPAV      , NFGPC       , NFLF        , NFLF2       , NFLFP       , NFP         , NFPCAV      , NFPCC       , &
    NFPPAV      , NFPPC       , NH4CF       , NH4MN       , NL          , NO3CF       , NO3MN       , NSDAYS      , &
    NUF         , NUPAC       , NUPAD       , NUPAP       , NUPAPCRP    , NUPAPCSM    , NUPAPCSM1   , NUPC        , &
    NUPD        , NUPRATIO    , PARI        , PARI1       , PARIP       , PARIPA      , PARIUE      , PARMJC      , &
    PARMJIC     , PARU        , PDADJ       , PDAYS       , PHOTQR      , PLA         , PLAGS2      , PLAGSB2     , &
    PLAGSB3     , PLAGSB4     , PLAS        , PLASI       , PLASL       , PLASP       , PLASS       , PLAX        , &
    PLTPOP      , PLYEAR      , PLYEARDOY   , PSDAP       , PSDAPM      , PSDAT       , PSDAYFR     , PSX         , &
    PTF         , RAINC       , RAINCC      , RAINPAV     , RAINPC      , RANC        , RESCAL      , RESCALG     , &
    RESLGAL     , RESLGALG    , RESNAL      , RESNALG     , RESPC       , RESPRC      , RESPTC      , RESWAL      , &
    RESWALG     , RLF         , RLFC        , RLV         , RNAD        , RNAM        , RNAMM       , RNCR        , &
    RNDEM       , RNUSE       , ROOTN       , ROOTNS      , RSCD        , RSCM        , RSCX        , RSFP        , &
    RSN         , RSNAD       , RSNPH       , RSNPHC      , RSNUSED     , RSWAD       , RSWAM       , RSWAMM      , &
    RSWPHC      , RSWT        , RSWTM       , RSWTX       , RTDEP       , RTDEPG      , RTNH4       , RTNO3       , &
    RTNSL       , RTRESP      , RTRESPADJ   , RTSLXDATE   , RTWT        , RTWTAL      , RTWTG       , RTWTGADJ    , &
    RTWTGL      , RTWTL       , RTWTM       , RTWTSL      , RUNOFFC     , RWAD        , RWAM        , RWAMM       , &
    SAID        , SANC        , SANCOUT     , SDNAD       , SDNC        , SDWAD       , SDWAM       , SEEDUSE     , &
    SEEDUSER    , SEEDUSET    , SENCAGS     , SENCALG     , SENCAS      , SENCL       , SENCS       , SENLA       , &
    SENLAGS     , SENLALG     , SENLALITTER , SENLAS      , SENLFG      , SENLFGRS    , SENLL       , SENLS       , &
    SENNAGS     , SENNAL      , SENNALG     , SENNAS      , SENNATC     , SENNATCM    , SENNL       , SENNLFG     , &
    SENNLFGRS   , SENNS       , SENROOT     , SENROOTA    , SENRTG      , SENTOPLITTER, SENTOPLITTERG             , &
    SENWACM     , SENWACMM    , SENWAGS     , SENWAL      , SENWALG     , SENWL       , SHDAT       , SHLA        , &
    SHLAG2      , SHLAS       , SHNUM       , SHNUMAD     , SHNUML      , SHRTD       , SHRTM       , SLA         , &
    SNAD        , SNCR        , SNH4ROOTZONE, SNO3PROFILE , SNO3ROOTZONE, SNPH        , SNPHC       , SNUSE       , &
    SRADC       , SRADCAV     , SRADCC      , SRADD       , SRADPAV     , SRADPC      , SRADPREV    , SRANC       , &
    SRNAM       , SRNDEM      , SRNOAD      , SRNOAM      , SRNOGM      , SRNOPD      , SRNUSE      , SROOTN      , &
    SRWT        , SRWTGRS     , SRWUD       , SRWUM       , STAI        , STAIG       , STAIS       , STEMN       , &
    STGEDAT     , STGYEARDOY  , STRSWT      , STWAD       , STWT        , STWTM       , SWPHC       , TCAN        , &
    TDIFAV      , TDIFNUM     , TDIFSUM     , TFD         , TFDLF       , TFG         , TFGLF       , TFP         , &
    TLCHC       , TMAXCAV     , TMAXCC      , TMAXM       , TMAXPAV     , TMAXPC      , TMAXSUM     , TMAXX       , &
    TMEAN       , TMEANAV     , TMEANCC     , TMEAND      , TMEANE      , TMEANEC     , TMEANG      , TMEANGC     , &
    TMEANNUM    , TMEANPC     , TMEANSUM    , TMINCAV     , TMINCC      , TMINM       , TMINN       , TMINPAV     , &
    TMINPC      , TMINSUM     , TNAD        , TNOXC       , TOFIXC      , TOMINC      , TOMINFOMC   , TOMINSOM1C  , &
    TOMINSOM2C  , TOMINSOM3C  , TOMINSOMC   , TRATIO      , TRWUP       , TT          , TT20        , TTCUM       , &
    TTD         , TTGEM       , TTNEXT      , TWAD        , UH2O        , UNH4        , UNO3        , VANC        , &
    VCNC        , VMNC        , VNAD        , VNAM        , VNAMM       , VNPCM       , VNPCMM      , VPDFP       , &
    VWAD        , VWAM        , VWAMM       , WFG         , WFGCC       , WFGPAV      , WFGPC       , WFLF        , &
    WFP         , WFPCAV      , WFPCC       , WFPPAV      , WFPPC       , WSDAYS      , WUPR   &      
    )

    !-----------------------------------------------------------------------------------------------------------------------
    !     Read experiment information from Dssat input or X- file in SUBROUTINE READ_XFILE.
    !     SUBROUTINE READ_XFILE takes all the code from CSCAS lines 2333 -2525
    !-----------------------------------------------------------------------------------------------------------------------

    CALL READ_XFILE( &
    ANFER       , CFLLFLIFE   , CN          , CROP        , CUFILE      , CWAN        , DCDAT       , DCDUR       , &
    DCFAC       , DCNX        , DCTAR       , DIDAT       , DIFFACR     , DIGFAC      , DINX        , ECFILE      , &
    EDATMX      , ENAME       , EXCODE      , FDAY        , FILEA       , FILEADIR    , FILEIO      , FILEIOT     , &
    FILELEN     , FILENEW     , FILEX       , HANUMX      , HAMT        , HARVFRAC    , HBPC        , HBPCF       , &
    HDOYF       , HDOYL       , HFIRST      , HLAST       , HNUMBER     , HOP         , HPC         , HPCF        , &
    HYEARDOY    , HYEARF      , HYEARL      , HYRDOY      , ICWD        , IFERI       , IHARI       , IPLTI       , &
    ISWDIS      , LENDIS      , LSNUM       , LSWT        , MEEVP       , MEEXP       , MEPHO       , MEWNU       , &
    ON          , PATHCR      , PATHEC      , PATHSP      , PDATE       , PLDAY       , PLMAGE      , PLME        , &
    PLPH        , PLTPOPE     , PLTPOPP     , PLYEARREAD  , PLYEARTMP   , PTTN        , PTX         , PWDINF      , &
    PWDINL      , PWDOYF      , PWDOYL      , PWYEARF     , PWYEARL     , RN          , ROWSPC      , RUNNAME     , &
    SDEPTH      , SDRATE      , SN          , SPFILE      , SPRL        , SWPLTD      , SWPLTH      , SWPLTL      , &
    TN          , TNAME       , VARNO       , VRNAME      , YEARDOYHARF &
    )

    !-----------------------------------------------------------------------------------------------------------------------
    !     Set planting/harvesting dates (Will change if runs repeated) in SUBROUTINE PLANT_HARV_DAT.
    !     SUBROUTINE PLANT_HARV_DAT takes all the code from CSCAS lines 2530 - 2570.   
    !-----------------------------------------------------------------------------------------------------------------------
        
    CALL PLANT_HARV_DAT( &
    CFLLFLIFE   , DOY         , FILEIOT     , HDAY        , HFIRST      , HLAST       , HYEAR       , IPLTI       , &
    PLDAY       , PLTOHARYR   , PLYEAR      , PLYEARDOYT  , PLYEARREAD  , PLYEARTMP   , PWDINF      , PWDINL      , &
    TVI1        , YEAR        , YEARDOY     , YEARDOYHARF &
    )

    !-----------------------------------------------------------------------------------------------------------------------
    !     Create genotype file names in SUBROUTINE CULTECOSPP.
    !     SUBROUTINE CULTECOSPP takes all the code from CSCAS lines 2576 - 2647.
    !-----------------------------------------------------------------------------------------------------------------------


    CALL CULTECOSPP( &
    CFGDFILE    , CROP        , CUDIRFLE    , CUDIRFLPREV , CUFILE      , ECDIRFLE    , ECFILE      , &                ! BLANK REMOVED
    FFLAG       , FFLAGEC     , FILEIOT     , FNUMERR     , FNUMTMP     , FNUMWRK     , MODNAME     , PATHCR      , &
    PATHEC      , PATHL       , PATHSP      , RNMODE      , SPDIRFLE    , SPFILE      , VARNO       , VARNOPREV   &
    )

    !-----------------------------------------------------------------------------------------------------------------------
    !     Read the data from the genotype files in SUBROUTINE READ_GENOTYPE.
    !     SUBROUTINE READ_GENOTYPE takes all the code from CSCAS lines 2653 - 3099.
    !-----------------------------------------------------------------------------------------------------------------------

    CALL READ_GENOTYPE( &
    BRFX        , CANHTS      , CHTPC       , CLAPC       , CN          , CO2COMPC    , CO2EX       , CO2F        , &
    CO2RF       , CRFR        , CUDIRFLE    , DAYLS       , DFPE        , DIFFACR     , DINX        , DUSRI       , &
    ECDIRFLE    , ECONO       , FILEIO      , FILEIOT     , FNUMERR     , GENFLCHK    , H2OCF       , HDUR        , &
    HMPC        , HPROD       , KCAN        , LA1S        , LAFND       , LAFS        , LAIXX       , LAWCF       , &
    LAWFF       , LAWMNFR     , LAWS        , LAWTR       , LAWTS       , LAWWR       , LAXN2       , LAXNO       , &
    LAXS        , LLIFA       , LLIFG       , LLIFS       , LLIFX       , LLIGP       , LLOSA       , LNPCMN      , &
    LNPCS       , LPEAW       , LPEFR       , LSENI       , LWLOS       , NCRG        , NFGL        , NFGU        , &
    NFPL        , NFPU        , NFSU        , NH4MN       , NLABPC      , NLLG        , NO3CF       , NO3MN       , &
    NTUPF       , ON          , PARFC       , PARIX       , PARU2       , PARUE       , PD          , PDL         , &
    PECM        , PGERM       , PHINTFAC    , PHINTS      , PHSV        , PHTV        , PPEXP       , PPSEN       , &
    PPTHR       , PSABV       , PSNAME      , PSX         , PSTYP       , PTFA        , PTFMN       , PTFMX       , &
    RATM        , RCROP       , RDGAF       , RDGS        , RLFWU       , RLIGP       , RLWR        , RN          , &
    RNMODE      , RNPCMN      , RNPCS       , RRESP       , RSCLX       , RSEN        , RSFPL       , RSFPU       , &
    RSFRS       , RSPCO       , RSUSE       , RTNH4       , RTNO3       , RTUFR       , RWUMX       , RWUPM       , &
    SAWS        , SDDUR       , SDNPCI      , SDRSF       , SDSZ        , SHGR        , SLIGP       , SN          , &
    SNPCMN      , SNPCS       , SPDIRFLE    , SRFR        , SRNOW       , SRNPCS      , SRPRS       , SWFRN       , &
    SWFRNL      , SWFRS       , SWFRX       , SWFRXL      , TN          , TPAR        , TRDV1       , TRDV2       , &
    TRGEM       , TRLFG       , TRPHS       , TSRAD       , VARNO       , WFEU        , WFGEM       , WFGL        , &
    WFGU        , WFPL        , WFPU        , WFRTG       , WFSU        &
    )

    !-----------------------------------------------------------------------------------------------------------------------
    !     Set up the growth stages including branching, check coefficients, set defaults and calculate/set initial states 
    !     in SUBROUTINE SETUP_STAGES.
    !     SUBROUTINE SETUP_STAGES takes all the code from CSCAS lines 3105 - 3655.
    !-----------------------------------------------------------------------------------------------------------------------

    CALL SETUP_STAGES( &
    BRFX        , CANHTS      , CFLLFLIFE   , CN          , CTRNUMPD    , DAYLS       , DFPE        , DSTAGE      , &
    DUTOMSTG    , EXCODE      , FNUMERR     , FNUMWRK     , H2OCF       , HMPC        , HSTG        , ICWD        , &
    IHARI       , ISWNIT      , ISWWATCROP  , KCAN        , KEP         , KEYPS       , KEYPSNUM    , KEYSTX      , &
    LA1S        , LAFND       , LAIXX       , LAPOTX      , LAXNO       , LAXS        , LENRNAME    , LENTNAME    , &
    LLIFA       , LLIFATT     , LLIFG       , LLIFGTT     , LLIFS       , LLIFSTT     , LNCM        , LNCMN       , &
    LNCX        , LNCXS       , LNUM        , LNUMTMP     , LNUMTOSTG   , LNUMX       , LNPCMN      , LNPCS       , &
    LPEFR       , LSENI       , LWLOS       , MEEXP       , MEPHO       , MESSAGE     , MEWNU       , MODNAME     , &
    MSTG        , NFGL        , NFGU        , NFPL        , NFPU        , NFSU        , NH4CF       , NH4MN       , &
    NLLG        , NO3CF       , NO3MN       , NTUPF       , OUTHED      , PARIX       , PARUE       , PD          , &
    PDL         , PEGD        , PGERM       , PHINT       , PHINTFAC    , PHINTS      , PLMAGE      , PLME        , &
    PLPH        , PLTPOPP     , PPEXP       , PSABV       , PSABVO      , PSNUM       , PSTART      , PSTYP       , &
    PSX         , RDGS        , RLFWU       , RN          , RNCM        , RNCMN       , RNCX        , RNCXS       , &
    RNPCMN      , RNPCS       , RSEN        , RSFRS       , RTNH4       , RTNO3       , RTUFR       , RUN         , &
    RUNI        , RUNNAME     , RUNRUNI     , SDCOAT      , SDEPTH      , SDEPTHU     , SDNAP       , SDNPCI      , &
    SDRATE      , SDRSF       , SDSZ        , SEEDN       , SEEDNI      , SEEDRS      , SEEDRSAV    , SEEDRSI     , &
    SERX        , SHGR        , SLPF        , SNCM        , SNCMN       , SNCX        , SNCXS       , SNPCMN      , &
    SNPCS       , SPRL        , SRFR        , SRNPCS      , SRPRS       , STDAY       , SWFRN       , SWFRNL      , &
    SWFRS       , SWFRX       , SWFRXL      , TN          , TNAME       , TPAR        , TRUNNAME    , TSRAD       , &
    TVR1        , TVR2        , TVR3        , WFGL        , WFGU        , WFPL        , WFPU        , WFRTG       , &
    WFSU        , WTDEP        &
    )

    !-----------------------------------------------------------------------------------------------------------------------
    !     Set up the output descriptors, check controls and write information to the overview and work files in 
    !     SUBROUTINE FINAL_INIT.
    !     SUBROUTINE FINAL_INIT takes all the code from CSCAS lines 3547 - 3932. This is the end of setting up the run.
    !-----------------------------------------------------------------------------------------------------------------------
      
    CALL FINAL_INIT( &
    ALBEDO      , ALBEDOS     , AMTNIT      , BRFX        , CANHTS      , CFLLFLIFE   , CLOUDS      , CN          , &
    CNI         , CRFR        , CUDIRFLE    , DAYLS       , DCDAT       , DCDUR       , DCFAC       , DCNX        , &
    DCTAR       , DFPE        , DIDAT       , DIFFACR     , DIGFAC      , DUSRI       , ECDIRFLE    , EXCODE      , &
    FILEIO      , FILEIOT     , FNAME       , FNUMWRK     , FOPEN       , H2OCF       , HBPCF       , HFIRST      , &
    HLAST       , HMPC        , HNUMBER     , HPCF        , HPROD       , IDETG       , IHARI       , IPLTI       , &
    ISWDIS      , ISWNIT      , ISWWAT      , KCAN        , KCANI       , KEP         , KEPI        , LA1S        , &
    LAFND       , LAFS        , LAIXX       , LAWCF       , LAWFF       , LAWMNFR     , LAWS        , LAWTR       , &
    LAWTS       , LAWWR       , LAXN2       , LAXNO       , LAXS        , LENDIS      , LENRNAME    , LENTNAME    , &
    LLIFA       , LLIFG       , LLIFS       , LLIFX       , LNPCMN      , LNPCS       , LNUMTOSTG   , LPEFR       , &
    LWLOS       , MEEXP       , MEPHO       , MERNU       , MESSAGE     , MEWNU       , MODEL       , MODNAME     , &
    MSTG        , NFGL        , NFGU        , NFPL        , NFPU        , NFSU        , NH4MN       , NLLG        , &
    NO3CF       , NO3MN       , ON          , ONI         , OUTCHOICE   , OUTHED      , PARIX       , PARU2       , &
    PARUE       , PD          , PDL         , PHINTFAC    , PHINTS      , PLMAGE      , PLTPOPP     , PLYEARDOYT  , &
    PPEXP       , PPSEN       , PPTHR       , PSABV       , PSX         , PWDINF      , PWDINL      , RDGS        , &
    RN          , RNI         , RNMODE      , RNPCMN      , RNPCS       , ROWSPC      , RRESP       , RSFRS       , &
    RSPCO       , RTNH4       , RTNO3       , RUN         , RUNI        , RUNNAME     , RUNRUNI     , RWUMX       , &
    RWUMXI      , SAWS        , SDEPTH      , SDEPTHU     , SDRATE      , SEEDNI      , SEEDRSI     , SERX        , &
    SHGR        , SLPF        , SN          , SNI         , SNPCMN      , SNPCS       , SPDIRFLE    , SRFR        , &
    SRNPCS      , TAIRHR      , TN          , TNAME       , TNI         , TRDV1       , TRGEM       , TRLFG       , &
    TRPHS       , TRUNNAME    , TVR1        , VARNO       , VRNAME      , YEARDOYHARF &                                 ! REMOVED VERSION     , 
    )

    !***********************************************************************************************************************
    ELSEIF (DYNAMIC.EQ.RATE) THEN
    !***********************************************************************************************************************
    !-----------------------------------------------------------------------------------------------------------------------
    !     Set up the switches for establishment and determine whether today is a planting day in SUBROUTINE PRE_PLANT.
    !     SUBROUTINE PRE_PLANT takes all the code from CSCAS lines 3951 - 4069.
    !-----------------------------------------------------------------------------------------------------------------------

    CALL PRE_PLANT( &
    AVGSW       , BD          , CFLFAIL     , CO2         , CO2AIR      , CUMSW       , DAP         , DLAYR       , &
    DOY         , DTRY        , DUL         , EARLYN      , EARLYW      , FILEIOT     , IPLTI       , ISOILH2O    , &
    ISOILN      , ISWNITEARLY , ISWWATEARLY , LL          , LNUM        , LNUMX       , MESSAGE     , NH4LEFT     , &
    NL          , NLAYR       , NO3LEFT     , PLPH        , PLTPOP      , PLTPOPE     , PLTPOPP     , PLYEAR      , &
    PLYEARDOY   , PLYEARDOYT  , PTTN        , PTX         , PWDINF      , PWDINL      , RNMODE      , SHNUM       , &
    SHNUML      , ST          , STGYEARDOY  , SW          , SWPLTD      , SWPLTH      , SWPLTL      , TMAX        , &
    TMEAN       , TMEANSURF   , TMIN        , TSDEP       , XDEP        , XDEPL       , YEAR        , YEARDOY     , &
    YEARPLTCSM   &
    ) 

        !===================================================================================================================
        IF (YEARDOY.GT.PLYEARDOY) THEN ! If planted (assumed in evening)
        !===================================================================================================================
    !-----------------------------------------------------------------------------------------------------------------------
    !     Calculate the water and thermal conditions for initiation of growth of the planting material (called GERMINATION) 
    !     in SUBROUTINE GERMINATE. SUBROUTINE GERMINATE takes all the code from CSCAS lines 4075 - 4215.
    !-----------------------------------------------------------------------------------------------------------------------

    CALL CS_GERMINATE( &
    ALBEDO      , BRSTAGE     , CFLLFLIFE   , CLOUDS      , CO2         , DLAYR       , DUL         , EO          , &
    EOC         , EOEBUD      , EOEBUDC     , EOEBUDCRP   , EOEBUDCRPC  , EOEBUDCRPCO2C             , &
    EOEBUDCRPCO2H2O           , EOEBUDCRPCO2H2OC          , EOMPCRP     , EOMPCRPC    , EOMPCRPCO2  , EOMPCRPCO2C , &
    EOMPCRPCO2H2O             , EOMPCRPCO2H2OC            , EOMPEN      , EOMPENC     , EOP         , EOPEN       , &
    EOPENC      , EOPT        , EOPTC       , EPSRATIO    , ES          , FILEIOT     , FNUMWRK     , GESTAGE     , &
    ISWWAT      , ISWWATCROP  , KEP         , LAI         , LL          , LSEED       , MEWNU       , NL          , &
    NLAYR       , PHINTS      , PLYEAR      , RATM        , RCROP       , RLF         , RLFC        , RLFWU       , &
    RLV         , RTDEP       , RWUMX       , RWUPM       , SAT         , SDEPTH      , SRAD        , SW          , &
    SWP         , SWPSD       , TCAN        , TDEW        , TDIFAV      , TDIFNUM     , TDIFSUM     , TFD         , &
    TFDNEXT     , TFGEM       , TFLFLIFE    , TMAX        , TMEAN       , TMIN        , TRATIO      , TRDV1       , &
    TRDV2       , TRGEM       , TRWU        , TRWUP       , TT          , TTGEM       , TTLFLIFE    , TTNEXT      , &
    TVR1        , TVR2        , TVR3        , TVR4        , UH2O        , WFEU        , WFGE        , WFGEM       , &
    WFP         , WINDSP      , WTDEP       , YEAR         &
    )

              !=============================================================================================================
              IF (GEUCUM+TTGEM*WFGE.GE.PEGD) THEN  ! If germinated by endday
              !=============================================================================================================
    !-----------------------------------------------------------------------------------------------------------------------
    !     Calculate germination timing, daylength and development units, reserves and grazing (?), PAR interception, rate 
    !     factors, senescence, assimilation and its partitioning, growth of storage roots, leaves, stems and crowns, 
    !     reserves and plant height, and soil water.in SUBROUTINE GROWTH. SUBROUTINE GROWTH takes all the code from CSCAS 
    !     lines 4218 - 5516.
    !
    !     TO DO: Divide SUBROUTINE GROWTH into seveal subroutines to account For (at least) senescence, assimilation, and 
    !     partitioning.
    !-----------------------------------------------------------------------

    CALL CS_GROWTH( &
    HANUMX      , LCNUMX      , LNUMX       , PSX         , AFLF        , AH2OPROFILE , AH2OROOTZONE, ANDEM       , &
    AREAPOSSIBLE, AREAPOSSIBLEN             , BD          , BRFX        , BRNUMST     , BRSTAGE     , BRSTAGEPREV , &
    BRSTAGETMP  , CANHTG      , CARBOADJ    , CARBOBEG    , CARBOBEGI   , CARBOBEGIA  , CARBOBEGM   , CARBOBEGR   , &
    CARBOEND    , CARBOR      , CARBOT      , CARBOTMP    , CARBOTMPI   , CARBOTMPM   , CARBOTMPR   , CFLAFLF     , &
    CFLFAIL     , CFLTFG      , CO2         , CO2AIR      , CO2COMPC    , CO2EX       , CO2F        , CO2FP       , &
    CO2FPI      , CO2INT      , CO2INTPPM   , CO2INTPPMP  , CO2RF       , CRFR        , CRWT        , CUMDEP      , &
    CUMDU       , CWAD        , CWAN        , DAP         , DAYL        , DAYLS       , DF          , DFNEXT      , &
    DFOUT       , DFPE        , DLAYR       , DSTAGE      , DU          , DUL         , DUNEED      , DUPHASE     , &
    DUPNEXT     , DUSRI       , EMFLAG      , EMRGFR      , EMRGFRPREV  , EOP         , ESTABLISHED , FAC         , &
    FNH4        , FNO3        , FNUMWRK     , GERMFR      , GESTAGE     , GEUCUM      , GROCR       , GROCRADJ    , &
    GROCRFR     , GROLF       , GROLFADJ    , GROLFP      , GROLS       , GROLSA      , GROLSP      , GROLSRS     , &
    GROLSRT     , GROLSRTN    , GROLSSD     , GROLSSEN    , GRORS       , GROSR       , GROST       , GROSTADJ    , &
    GROSTCR     , GROSTCRP    , GROSTCRPSTORE             , H2OCF       , H2OPROFILE  , H2OROOTZONE , HAFR        , &
    HAMT        , HANUM       , HAWAD       , HOP         , HYEARDOY    , ISWDIS      , ISWNIT      , ISWNITEARLY , &
    ISWWAT      , ISWWATEARLY , KCAN        , LA1S        , LAFND       , LAFS        , LAGEG       , LAGETT      , &
    LAGL        , LAI         , LAIXX       , LANC        , LANCRS      , LAP         , LAPD        , LAPH        , &
    LAPOTX      , LAPS        , LAPSTMP     , LATL        , LATL2       , LATL3       , LATL4       , LATLPOT     , &
    LATLPREV    , LAWCF       , LAWFF       , LAWL        , LAWMNFR     , LAWS        , LAWTR       , LAWTS       , &
    LAWWR       , LAXN2       , LAXNO       , LAXS        , LBIRTHDAP   , LEAFN       , LENDIS      , LFWT        , &
    LL          , LLIFATT     , LLIFG       , LLIFGTT     , LLIFSTT     , LLIGP       , LLOSA       , LNCGL       , &
    LNCGU       , LNCM        , LNCPL       , LNCPU       , LNCSEN      , LNCX        , LNDEM       , LNDEMTU     , &
    LNPH        , LNUM        , LNUMG       , LNUMNEED    , LNUMSG      , LNUMTOSTG   , LNUSE       , LPEFR       , &
    LRTIP       , LWLOS       , LWPH        , MEPHO       , MESSAGE     , MJPERE      , MSTG        , NCRG        , &
    NDEM2       , NDEMMN      , NFG         , NFGL        , NFGU        , NFLF        , NFLF2       , NFLFP       , &
    NFP         , NFPL        , NFPU        , NFRG        , NFSU        , NH4CF       , NH4LEFT     , NH4MN       , &
    NLABPC      , NLAYR       , NLAYRROOT   , NLLG        , NO3CF       , NO3LEFT     , NO3MN       , NPOOLL      , &
    NPOOLR      , NPOOLS      , NTUPF       , NUF         , NULEFT      , NUPAD       , NUPAP       , NUPD        , &
    NUPRATIO    , NUSEFAC     , PARFC       , PARI        , PARI1       , PARIP       , PARIPA      , PARIPREV    , &
    PARMJFAC    , PARMJIADJ   , PARU        , PARUE       , PDL         , PECM        , PEGD        , PHINT       , &
    PHINTFAC    , PHINTS      , PHOTQR      , PHSV        , PHTV        , PLA         , PLAGS2      , PLAGSB2     , &
    PLAGSB3     , PLAGSB4     , PLAS        , PLASI       , PLASL       , PLASN       , PLASP       , PLASS       , &
    PLASW       , PLTPOP      , PPEXP       , PPSEN       , PPTHR       , PSTART      , PTF         , PTFA        , &
    PTFMN       , PTFMX       , RANC        , RATM        , RCROP       , RDGS        , RFAC        , RLDF        , &
    RLF         , RLFC        , RLIGP       , RLV         , RM          , RNCM        , RNCX        , RNDEM       , &
    RNDEMG      , RNDEMTU     , RNH4U       , RNO3U       , RNUSE       , RRESP       , RSCD        , RSEN        , &
    RSFP        , RSFPL       , RSFPU       , RSFRS       , RSN         , RSNPH       , RSNUSED     , RSPCO       , &
    RSSRWTGLFADJ, RSUSE       , RSWPH       , RSWT        , RTDEP       , RTDEPG      , RTDEPTMP    , RTNH4       , &
    RTNO3       , RTNSL       , RTRESP      , RTRESPADJ   , RTUFR       , RTWT        , RTWTG       , RTWTGADJ    , &
    RTWTGL      , RTWTL       , RTWTSL      , RTWTUL      , SANC        , SAT         , SDDUR       , SDEPTHU     , &
    SDRATE      , SDRSF       , SEEDN       , SEEDNI      , SEEDNUSE    , SEEDNUSE2   , SEEDRS      , SEEDRSAV    , &
    SEEDRSAVR   , SEEDRSI     , SENCAGS     , SENCALG     , SENFR       , SENLA       , SENLAGS     , SENLALG     , &
    SENLFG      , SENLFGRS    , SENNAGS     , SENNALG     , SENNLFG     , SENNLFGRS   , SENRTG      , &
    SENTOPLITTERG             , SENWAGS     , SENWALG     , SERX        , SHF         , SHGR        , SHLAG2      , &
    SHLAGB2     , SHLAGB3     , SHLAGB4     , SHNUM       , SHRTD       , SLPF        , SNCM        , SNCX        , &
    SNDEM       , SNDEMG      , SNDEMTU     , SNH4        , SNH4PROFILE , SNH4ROOTZONE, SNO3        , SNO3PROFILE , &
    SNO3ROOTZONE, SNPH        , SNUSE       , SRAD        , SRADPREV    , SRANC       , SRDAYFR     , SRFR        , &
    SRNDEM      , SRNDEMG     , SRNDEMTU    , SRNOPD      , SRNOW       , SRNPCS      , SRNUSE      , SRWT        , &
    SRWTGRS     , STAIG       , STAIS       , STDAY       , STEMN       , STWT        , SW          , SWFR        , &
    SWFRN       , SWFRNL      , SWFRX       , SWFRXL      , SWP         , SWPH        , SWPRTIP     , TDEW        , &
    TFD         , TFDF        , TFDLF       , TFG         , TFGLF       , TFLAW       , TFP         , TIMENEED    , &
    TMAX        , TMEAN       , TMIN        , TRLDF       , TRLFG       , TRLV        , TRPHS       , TRWUP       , &
    TT          , TTGEM       , TTLFLIFE    , TTNEED      , TTNEXT      , TTOUT       , TVR1        , TVR2        , &
    TVR3        , TVR5        , UNH4        , UNO3        , VPD         , VPDFP       , WFG         , WFGE        , &
    WFGL        , WFGU        , WFLAW       , WFLF        , WFNU        , WFP         , WFPL        , WFPU        , &
    WFRG        , WFRTG       , WFSU        , WUPR        , XMIN        , YEARDOY     , YEARDOYHARF , WEATHER       & ! MF 31AU14 WEATHER ADDED 
    )
            
              !=============================================================================================================
              ENDIF  ! End of after germinated section
              !=============================================================================================================

        !===================================================================================================================
        ENDIF  ! End of after planted (rate) section
        !===================================================================================================================

    !***********************************************************************************************************************
    ELSEIF (DYNAMIC.EQ.INTEGR) THEN
    !***********************************************************************************************************************

        !===================================================================================================================
        IF (YEARDOY.GE.PLYEARDOY) THEN
        !===================================================================================================================

    !-----------------------------------------------------------------------------------------------------------------------
    !     Update the seasonal data with the data for the current day: dry weights, produced and senesced leaf area, plant 
    !     height, root and length, and nitrogen. Update stages, dates and times. Calculate branch interval, N
    !     concentrations and whether to harvest. Calculate yields of plant parts and summaries of weather, soil variables
    !     and PAR utilization in SUBROUTINE INTEGRATE. SUBROUTINE INTEGRATE takes all the code from CSCAS lines 5534 - 6649.
    !
    !     TO DO: Divide SUBROUTINE INTEGRATE into several subroutines to account for (at least) growth stages, branch 
    !     intervals, plant yields, and soil and weather summaries.   
    !-----------------------------------------------------------------------------------------------------------------------
            
    CALL INTEGRATE( &
    LCNUMX      , LNUMX       , PSX         , ALBEDO      , ALBEDOS     , AMTNIT      , BASELAYER   , BD          , &
    BRNUMSH     , BRNUMST     , BRSTAGE     , BRSTAGEPREV , CAID        , CANHT       , CANHTG      , CARBOADJ    , &
    CARBOBEG    , CARBOC      , CDAYS       , CFLFAIL     , CFLHAR      , CFLHARMSG   , CFLSDRSMSG  , CNAD        , &
    CNADPREV    , CNADSTG     , CNAM        , CO2         , CO2CAV      , CO2CC       , CO2MAX      , CO2PAV      , &
    CO2PC       , CRRSWAD     , CRRSWT      , CRWAD       , CRWT        , CRWTM       , CUMDU       , CWAD        , &
    CWADPREV    , CWADSTG     , CWAHC       , CWAM        , DAE         , DALF        , DAP         , DAYL        , &
    DAYLCAV     , DAYLCC      , DAYLPAV     , DAYLPC      , DAYLST      , DAYSUM      , DEPMAX      , DGLF        , &
    DLAYR       , DLAYRTMP    , DOM         , DOY         , DRAIN       , DRAINC      , DSLF        , DSTAGE      , &
    DU          , EDAP        , EDAPFR      , EDAYFR      , EMRGFR      , EOP         , EP          , EPCC        , &
    EPPC        , ET          , ETCC        , ETPC        , EYEARDOY    , FAPPLINE    , FAPPNUM     , FERNIT      , &
    FERNITPREV  , FILEIOT     , FNUMERR     , FNUMWRK     , FSOILH2O    , FSOILN      , GDAP        , GDAPFR      , &
    GDAYFR      , GEDAYSE     , GEDAYSG     , GERMFR      , GESTAGE     , GESTAGEPREV , GEUCUM      , GROCRADJ    , &
    GROLFADJ    , GROLSRTN    , GROLSSD     , GRORS       , GROSR       , GROSTADJ    , GYEARDOY    , H2OA        , &
    HADOY       , HAFR        , HAYEAR      , HBPCF       , HFIRST      , HIAD        , HIAM        , HIND        , &
    HINM        , HNAD        , HNAM        , HNC         , HNPCM       , HNUMAD      , HNUMAM      , HNUMGM      , &
    HNUMPM      , HPRODN      , HSTG        , HWAD        , HWAM        , HWUD        , HWUM        , IHARI       , &
    IRRAMT      , IRRAMTC     , ISWNIT      , ISWWAT      , LAFND       , LAGEP       , LAGETT      , LAI         , &
    LAIPREV     , LAISTG      , LAIX        , LAIXX       , LANC        , LAP         , LAPD        , LAPH        , &
    LAPHC       , LAPS        , LDEATHDAP   , LCNUM       , LCOA        , LCOAS       , LEAFN       , LEAFNEXCESS , &
    LFWT        , LFWTM       , LL          , LLIFATT     , LLIFGTT     , LLIFSTT     , LLIFX       , LLIFXUNUSED , &
    LLIGP       , LLNAD       , LLRSWAD     , LLRSWT      , LLWAD       , LNCM        , LNCMN       , LNCR        , &
    LNCX        , LNCXS       , LNPH        , LNPHC       , LNUM        , LNUMG       , LNUMNEED    , LNUMPREV    , &
    LNUMSG      , LNUMSIMTOSTG, LNUMSM      , LNUMSOLDESTA, LNUMSTG     , LNUMTOSTG   , LNUSE       , LPEAI       , &
    LPEAW       , LPEFR       , LPERSWAD    , LPERSWT     , LPEWAD      , LWPH        , LWPHC       , MDAP        , &
    MDAPFR      , MDAT        , MDAYFR      , MDOY        , MEDEV       , MESSAGE     , MONTH       , MSTG        , &
    NFG         , NFGCAV      , NFGCC       , NFGPAV      , NFGPC       , NFP         , NFPCAV      , NFPCC       , &
    NFPPAV      , NFPPC       , NH4LEFT     , NLAYR       , NO3LEFT     , NSDAYS      , NUPAC       , NUPAD       , &
    NUPC        , NUPD        , PARI        , PARIUE      , PARIUED     , PARMJC      , PARMJFAC    , PARMJIADJ   , &
    PARMJIC     , PARUEC      , PD          , PDAYS       , PDL         , PECM        , PEGD        , PHINT       , &
    PLA         , PLAGSB4     , PLAS        , PLASP       , PLASTMP     , PLASTMP2    , PLAX        , PLTPOP      , &
    PLYEARDOY   , PSABV       , PSDAP       , PSDAPFR     , PSDAT       , PSTART      , RAIN        , RAINC       , &
    RAINCC      , RAINPAV     , RAINPC      , RANC        , RESCAL      , RESCALG     , RESLGAL     , RESLGALG    , &
    RESNAL      , RESNALG     , RESPC       , RESPRC      , RESPTC      , RESWAL      , RESWALG     , RLIGP       , &
    RLV         , RLWR        , RNAD        , RNAM        , RNCM        , RNCMN       , RNCR        , RNCX        , &
    RNCXS       , RNUSE       , ROOTN       , ROOTNEXCESS , ROOTNS      , RSCD        , RSCLX       , RSCM        , &
    RSCX        , RSN         , RSNAD       , RSNPH       , RSNPHC      , RSNUSED     , RSWAD       , RSWAM       , &
    RSWPH       , RSWPHC      , RSWT        , RSWTGLFADJ  , RSWTM       , RSWTX       , RTDEP       , RTDEPG      , &
    RTNSL       , RTRESP      , RTRESPADJ   , RTSLXDATE   , RTWT        , RTWTAL      , RTWTG       , RTWTGADJ    , &
    RTWTGL      , RTWTL       , RTWTM       , RTWTSL      , RTWTUL      , RUNOFF      , RUNOFFC     , RWAD        , &
    RWAM        , SAID        , SANC        , SANCOUT     , SDCOAT      , SDEPTH      , SDEPTHU     , SDNAD       , &
    SDNC        , SDWAD       , SDWAM       , SEEDN       , SEEDNUSE    , SEEDNUSE2   , SEEDRS      , SEEDRSAV    , &
    SEEDRSAVR   , SEEDUSE     , SEEDUSER    , SEEDUSET    , SENCAS      , SENCL       , SENCS       , SENFR       , &
    SENLA       , SENLALITTER , SENLAS      , SENLFG      , SENLFGRS    , SENLL       , SENLS       , SENNAL      , &
    SENNAS      , SENNATC     , SENNGS      , SENNL       , SENNLFG     , SENNLFGRS   , SENNS       , SENROOT     , &
    SENROOTA    , SENTOPLITTER, SENTOPLITTERA             , SENTOPLITTERG             , SENWACM     , SENWAL      , &
    SENWALG     , SENWL       , SHDAP       , SHDAT       , SHLA        , SHLAGB4     , SHLAS       , SHNUM       , &
    SHNUMAD     , SHRTD       , SHRTM       , SLA         , SLIGP       , SNAD        , SNCM        , SNCMN       , &
    SNCR        , SNCX        , SNCXS       , SNPH        , SNPHC       , SNUSE       , SRAD        , SRAD20      , &
    SRAD20S     , SRADC       , SRADCAV     , SRADCC      , SRADD       , SRADPAV     , SRADPC      , SRANC       , &
    SRNAD       , SRNAM       , SRNOPD      , SRNUSE      , SROOTN      , SRWAD       , SRWT        , SRWTGRS     , &
    SRWUD       , STAI        , STAIG       , STAIS       , STEMN       , STEMNEXCESS , STGYEARDOY  , STRESS      , &
    STRESS20    , STRESS20N   , STRESS20NS  , STRESS20S   , STRESS20W   , STRESS20WS  , STRESSN     , STRESSW     , &
    STRSWAD     , STRSWT      , STWAD       , STWT        , STWTM       , SW          , SWP         , SWPH        , &
    SWPHC       , SWPLTH      , SWPLTL      , TIMENEED    , TLCHC       , TLCHD       , TMAX        , TMAXCAV     , &
    TMAXCC      , TMAXM       , TMAXPAV     , TMAXPC      , TMAXSUM     , TMAXX       , TMEAN       , TMEAN20     , &
    TMEAN20P    , TMEAN20S    , TMEANAV     , TMEANCC     , TMEAND      , TMEANE      , TMEANEC     , TMEANG      , &
    TMEANGC     , TMEANNUM    , TMEANPC     , TMEANSUM    , TMIN        , TMINCAV     , TMINCC      , TMINM       , &
    TMINN       , TMINPAV     , TMINPC      , TMINSUM     , TNAD        , TNIMBSOM    , TNOXC       , TNOXD       , &
    TOFIXC      , TOMIN       , TOMINC      , TOMINFOM    , TOMINFOMC   , TOMINSOM    , TOMINSOM1   , TOMINSOM1C  , &
    TOMINSOM2   , TOMINSOM2C  , TOMINSOM3   , TOMINSOM3C  , TOMINSOMC   , TT          , TT20        , TT20S       , &
    TTCUM       , TTD         , TTGEM       , TTLFLIFE    , TVI1        , TVR1        , TWAD        , VANC        , &
    VCNC        , VMNC        , VNAD        , VNAM        , VNPCM       , VWAD        , VWAM        , WAVR        , &
    WFG         , WFGCAV      , WFGCC       , WFGE        , WFGPAV      , WFGPC       , WFP         , WFPCAV      , &
    WFPCC       , WFPPAV      , WFPPC       , WSDAYS      , WUPR        , WUPRD       , YEAR        , YEARDOY     , &
    YEARDOYHARF , WEATHER     &                                                                                     ! MF 31AU14 WEATHER ADDED
    )
        
        !===================================================================================================================
        ENDIF  ! End of after planted (integrate) section
        !===================================================================================================================


    !***********************************************************************************************************************
    ELSEIF (DYNAMIC.EQ.OUTPUT .AND. STEP.EQ.STEPNUM.OR.DYNAMIC.EQ.SEASEND .AND. SEASENDOUT.NE.'Y') THEN
    !***********************************************************************************************************************
    !-----------------------------------------------------------------------------------------------------------------------
    !    The output section of CSCAS has only been converted to free format input. It compiled with no errors, but there is
    !    guarantee that the output is error free, although it gives identical results to CSCAS.FOR for CCPA7901.CSX.
    !     TO DO: Divide the remaining code into several subroutines incorporating the various output options. 
    !-----------------------------------------------------------------------------------------------------------------------

        ! Simulated outputs only
        !  IDETG (GROUT in controls (Y,N))  Plant growth outputs
        !   Y->Work_details+Plantgro+Plantgr2+Plantgrf
        !      +PlantN(If N switched on)
        !   FROUT->#=number of days between outputs
        !  IDETS (SUMRY in controls (Y,N)) Summary outputs
        !   Y->Summary+Plantsum+Work(Harvest)                        
        !
        ! Simulated+Measured outputs
        !  IDETO (OVVEW in controls (Y,E,N)) Overview outputs
        !   Y->Overview+Evaluate(+Measured if IDETG=Y)
        !   E->Evaluate only
        !  IDETL (VBOSE in controls (0,N,Y,D,A))
        !   Y->Leaves+Tiers+Measured                 
        !   D->+Phenols+Phenolm+Plantres+Plantrem
        !   A->Errora+Errors+Errort+Full Reads
        !   0,A are meta switches:
        !     0 switches everything to N apart from IDETS,which given a Y,
        !       and IDETO,which given an E when RNMODE is not N (seasonal)
        !     A switches ALL outputs on  

        ! If model failure so that cycle not completed
        IF (DYNAMIC.EQ.SEASEND .AND. SEASENDOUT.NE.'Y') THEN
            laix = -99.0
            cwahc = -99.0
            nupac = -99.0
            hwam = -99.0
            hiam = -99.0
            sennatc = -99.0
        ENDIF
        
        DAS = MAX(0,CSTIMDIF(YEARSIM,YEARDOY))
        
        SLAOUT = -99.0
        ! Note possibilities. To change must recompile.
        IF (OUTCHOICE.EQ.1) THEN
            ! 1. Include reserves
            LLWADOUT = LLWAD+LLRSWAD
            STWADOUT = STWAD+STRSWAD + LPEWAD+LPERSWAD
            CRWADOUT = CRWAD+CRRSWAD
            IF (LFWT.GT.1.0E-6) SLAOUT=(PLA-SENLA-LAPHC)/(LFWT*(1.0-LPEFR)+LLRSWT)
        ELSEIF (OUTCHOICE.EQ.2) THEN
            ! 2. No reserves, stem wt includes petioles
            LLWADOUT = LLWAD
            STWADOUT = STWAD + LPEWAD
            CRWADOUT = CRWAD
            IF (LFWT.GT.1.0E-6)SLAOUT=(PLA-SENLA-LAPHC)/(LFWT*(1.0-LPEFR))
        ELSEIF (OUTCHOICE.EQ.3) THEN
            ! 3. No reserves, stem wt does not includes petioles
            LLWADOUT = LLWAD
            STWADOUT = STWAD
            CRWADOUT = CRWAD
            IF (LFWT.GT.1.0E-6)SLAOUT=(PLA-SENLA-LAPHC)/(LFWT*(1.0-LPEFR))
        ENDIF
        
        IF (SLA.LE.0.0) SLAOUT = -99.0
        
        CALL Csopline(sentoplitterac,(sentoplittera))
        CALL Csopline(senrootc,(senroota))
        CALL Csopline(laic,lai)
        CALL Csopline(caic,caid)
        CALL Csopline(hindc,hind)
        CALL Csopline(hwudc,hwud)
        CALL Csopline(sdwadc,sdwad)
        CALL Csopline(brstagec,brstage)
        
        ! Calculate Pari to equate to updated LAI
        PARIOUT = (1.0 - EXP((-KCAN)*LAI))
        
        !-------------------------------------------------------------------------------------------------------------------
        !       TIME SEQUENCE OUTPUTS (WorkPlantgro,gr2,grf)
        !-------------------------------------------------------------------------------------------------------------------

        IF (  (MOD(DAS,FROPADJ).EQ.0.AND.YEARDOY.GE.PLYEARDOY).OR. (YEARDOY.EQ.PLYEARDOY).OR. (YEARDOY.EQ.STGYEARDOY(1)).OR. &
            (YEARDOY.EQ.STGYEARDOY(HSTG)).OR. (YEARDOY.EQ.STGYEARDOY(11))) THEN
            !---------------------------------------------------------------------------------------------------------------
            !         IDETL = A OUTPUTS (Work details)
            !---------------------------------------------------------------------------------------------------------------     
            
            IF (IDETL.EQ.'A') THEN
                WRITE(fnumwrk,*)' '
                WRITE(fnumwrk,'(A25,I16,I7,I7)')' Year,day,DAP            ',YEAR,DOY,DAP
                WRITE(fnumwrk,'(A34,2F7.3)')' Rainfall,Irrigation mm           ',rain,irramt
                WRITE(fnumwrk,'(A34,2F7.3)')' Tmean,Tcan oC                    ',tmean,tcan
                WRITE(fnumwrk,'(A34,2F7.3)')' Tcan-Tmean Today and average oC  ',tcan-tmean,tdifav
                WRITE(fnumwrk,'(A34,F7.1)')' Windspeed m/s                    ',windsp     
                WRITE(fnumwrk,'(A34,2F7.3,2F7.1)')' Brstage,Lnum. Beginning,end of day',brstageprev,brstage,lnumprev,lnum
                IF (PLA-SENLA-LAPHC.LT.9999.9) THEN
                    WRITE(fnumwrk,'(A34,F7.1,F7.1)')' Laminae area end day /m2,/plant  ',lai,pla-senla-laphc
                ELSE
                    WRITE(fnumwrk,'(A34,F7.1,I7)')' Laminae area end day /m2,/plant  ',lai,NINT(pla-senla-laphc)
                ENDIF
                WRITE(fnumwrk,'(A25,I1,A7,2F7.3)')' PARI,competition model,C',CN,' 1-crop',PARI,PARI1
                IF (Rlf.GT.0.0) THEN
                    WRITE(fnumwrk,'(A34,2F7.1,2F7.1)')' Ratm,Rcrop,Rcrop*Rco2/R,*H2o     ',ratm,rcrop,rcrop*rlfc/rlf, &
                        rcrop*rlfc/rlf*(1.0-(1.0-wfp))
                ELSE
                    WRITE(fnumwrk,'(A34,2F7.1)')' Ratm,Rcrop                       ',ratm,rcrop                         
                ENDIF
                IF (FILEIOT.NE.'XFL') THEN
                    IF (IDETL.EQ.'D'.OR.IDETL.EQ.'A') THEN
                        IF (meevp.EQ.'R')THEN
                            WRITE(fnumwrk,'(A50)')' Model (CSM) pot.evap.method: Priestley-Taylor R  '
                        ELSEIF (meevp.EQ.'P')THEN
                            WRITE(fnumwrk,'(A51)')' Model (CSM) pot.evap.method: FAO Penman (FAO-24) P'
                        ELSEIF (meevp.EQ.'F')THEN
                            WRITE(fnumwrk,'(A50,A10)')' Model (CSM) pot.evap.method: FAO Penman-Monteith ', '(FAO-56) F'
                        ELSEIF (meevp.EQ.'D')THEN 
                            WRITE(fnumwrk,'(A53,A10)')' Model (CSM) pot.evap.method: Dynamic Penman-Monteith'
                        ENDIF
                        !MEEVP CSM Model routines
                        !   F  PETPEN  FAO Penman-Monteith (FAO-56) potential evapotranspiration 
                        !                with KC = 1.0
                        !   R  PETPT   Calculates Priestly-Taylor potential evapotranspiration
                        !                (default method)
                        !   D  PETDYN  Dynamic Penman-Monteith, pot. evapotranspiration, with
                        !                dynamic input of LAI, crop height effects on Ra and Rs
                        !   P  PETPNO  FAO Penman (FAO-24) potential evapotranspiration 
                        !   M  PETMEY  "Standard reference evaporation calculation for inland 
                        !                south eastern Australia" By Wayne Meyer 1993
                        WRITE(fnumwrk,'(A34,4F7.3,F8.3)')' EO  P-T,Pen,M-Pen,Ebud,Model     ',eopt,eopen,eompen,eoebud,eo
                        WRITE(fnumwrk,'(A34,4F7.3,F8.3)')' EOCrp                            ',eopt,eopen,eompcrp,eoebudcrp,eo
                        WRITE(fnumwrk,'(A34,4F7.3,F8.3)')' EOCrpCo2                         ',eopt,eopen,eompcrpco2, &
                            eoebudcrpco2,eo
                        WRITE(fnumwrk,'(A34,4F7.3,F8.3)')' EOCrpCo2H2o                         ',eopt,eopen,eompcrpco2h2o, &
                            eoebudcrpco2h2o,eo
                        IF (WFP.LT.1.0.AND.WFP.GT.0.0)WRITE(fnumwrk,'(A41,F4.2,A8,F4.1,A8,F4.1)') &
                            ' NB.Water stress effect operative. WFP = ',wfp,' TCAN = ',tcan,' TAIR = ',tmean
                        WRITE(fnumwrk,'(A34,4F7.1,F8.1)')' EOC P-T,Pen,M-P,Ebud,Model       ',eoptc,eopenc,eompenc,eoebudc,eoc
                        WRITE(fnumwrk,'(A34,4F7.1,F8.1)')' EOCrpC                           ',eoptc,eopenc,eompcrpc,eoebudcrpc,eoc
                        WRITE(fnumwrk,'(A34,4F7.1,F8.1)')' EOCrpCo2C                        ',eoptc,eopenc,eompcrpco2c, &
                            eoebudcrpco2c,eoc
                        WRITE(fnumwrk,'(A34,4F7.1,F8.1)')' EOCrpCo2h2oC                     ',eoptc,eopenc,eompcrpco2h2oc, &
                            eoebudcrpco2h2oc,eoc
                    ENDIF
                ENDIF
                IF (EYEARDOY.LE.YEARDOY) THEN
                    WRITE(fnumwrk,'(A34,2F7.3)')' Pot.pl./Pot.soil evap; /Pot.pl330',epsratio,tratio
                    WRITE(fnumwrk,'(A34,F7.3)')' Quantum requirement              ',photqr
                    WRITE(fnumwrk,'(A34,2F7.1)')' CO2,Estimated internal CO2 vpm   ',co2,co2intppm
                    WRITE(fnumwrk,'(A34,F7.3,6F7.3)')' Phs facs Co2,Temp,H2o,N,Rsvs,Vpd ',co2fp,tfp,wfp,nfp,rsfp,vpdfp
                    WRITE(fnumwrk,'(A34,3F7.3)')' Phs. Rue,Rue+Co2i,Resistances    ',carbobegr*pltpop,carbobegi*pltpop, &
                        carbobegm*pltpop
                    WRITE(fnumwrk,'(A34,3F7.2)')' CH2O Start,end,remobilized       ',carbobeg*pltpop*10.,carboend*pltpop*10.0, &
                        senlfgrs*pltpop*10.0
                    IF(CUMDU.LE.DUSRI.AND.CUMDU+DU.GT.DUSRI.AND.SRDAYFR.GT.0.0)THEN
                        WRITE(fnumwrk,'(A34, I7)')' STORAGE ROOT INITIATION (no/pl)  ',srnopd
                        WRITE(fnumwrk,'(A34,2F7.1)')' Canopy wt at end of day (kg/ha)  ',(lfwt+stwt+crwt+rswt)*pltpop*10.0
                        WRITE(fnumwrk,'(A34,F7.1)')' Storage root fraction            ',srfr
                    ENDIF
                    IF (SRWTGRS+SRWTGRS.GT.0.0) WRITE(FNUMWRK,'(A48)')' Surplus assimilates sent to storage roots      '
                    IF (LRTIP.EQ.1) WRITE(fnumwrk,'(A21)')' Root tip in layer 1 '
                    WRITE(FNUMWRK,'(A34,2F7.2)')' N demand,shortage (kg/ha)        ',andem,AMAX1(0.0,andem-nupap)
                    ! Folowing detailed outputs can be printed if tvi1=1
                    tvi1 = 0
                    IF (tvi1.eq.1) THEN
                        IF (ANDEM.LE.0.0) THEN
                            WRITE(FNUMWRK,'(A44)')' N demand at zero! Components of demand/use:' 
                        ELSE
                            WRITE(FNUMWRK,'(A47)')' N demand above zero! Components of demand/use:' 
                        ENDIF  
                        WRITE(FNUMWRK,*)' Leaves            ',lndem*pltpop*10.0
                        WRITE(FNUMWRK,*)' Stem              ',sndem*pltpop*10.0
                        WRITE(FNUMWRK,*)' Roots             ',rndem*pltpop*10.0
                        WRITE(FNUMWRK,*)' Storage root      ',srndem*pltpop*10.0
                        WRITE(FNUMWRK,*)' Seed use          ',(seednuse+seednuse2)*pltpop*10.0
                        WRITE(FNUMWRK,*)' Reserves use      ',rsnused*pltpop*10.0
                        IF (ANDEM.GT.0.0.AND.NUPAP.LT.ANDEM) WRITE(fnumwrk,'(A38)')'  N uptake insufficient to meet demand'
                        IF (ANDEM.GT.10.0) WRITE(fnumwrk,'(A11,F4.1,A23,F4.1)')' N demand (',ANDEM, &
                            ') very high! Uptake = ',nuf*andem
                    ENDIF 
                ENDIF ! End EYEARDOY.LE.YEARDOY
            ENDIF ! End detailed WORK writes  IDDETL = 'A'   
            
            !---------------------------------------------------------------------------------------------------------------
            !         IDETG NE N OUTPUTS (Plantgro,gr2,grf,n)
            !---------------------------------------------------------------------------------------------------------------     
            
            IF ((IDETG.NE.'N'.AND.IDETL.NE.'0').OR.IDETL.EQ.'A') THEN
                
                ! PlantGro
                IF (YEARDOY.EQ.PLYEARDOY) THEN
                    OPEN (UNIT = NOUTPG, FILE = OUTPG,POSITION = 'APPEND')
                    IF (FILEIOT(1:2).EQ.'DS') THEN
                        CALL HEADER(2, NOUTPG, RUN)
                    ELSE
                        WRITE (NOUTPG,'(/,A79,/)') OUTHED
                        WRITE (NOUTPG,103) MODEL
                        WRITE (NOUTPG,1031) MODNAME
                        WRITE (NOUTPG,104)EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
                        WRITE (NOUTPG,102) TN,TNAME
                        WRITE (NOUTPG,107) CROP,VARNO,VRNAME
                        CALL Calendar (year,doy,dom,month)
                        WRITE(NOUTPG,108)month,dom,plyeardoy,NINT(pltpopp),NINT(rowspc)
103                     FORMAT (' MODEL            ',A8)
1031                    FORMAT (' MODULE           ',A8)
104                     FORMAT (' EXPERIMENT       ',A8,A1,A2,A2,A47)
102                     FORMAT (' TREATMENT',I3,'     ',A25)
107                     FORMAT (' GENOTYPE         ',A2,A6,'  ',A16)
108                     FORMAT (' PLANTING         ',A3,I3,I8,2X,I4,' plants/m2 in ',I3,' cm rows',/)
                    ENDIF
                    WRITE (NOUTPG,2201)
2201                FORMAT ('@YEAR DOY   DAS   DAP TMEAN','  GSTD  L#SD',' PARID PARUD  AWAD','  LAID  SAID  CAID', &
                        '  TWAD SDWAD  RWAD  CWAD  LWAD  SWAD  HWAD  HIAD',' CRWAD RSWAD SNWLD SNWSD','  RS%D','  H#AD  HWUD', &
                        '  S#AD  SLAD  RDPD  PTFD','  SWXD WAVRD',' WUPRD  WFPD  WFGD','  NFPD  NFGD NUPRD','  TFPD  TFGD', &
                        ' DYLFD','      ','      ')
                ENDIF  ! End Plantgro header writes
                WRITE (NOUTPG,501)YEAR,DOY,DAS,DAP,TMEAN,BRSTAGEC,LNUM,PARIOUT,PARIUE,AMIN1(999.9,CARBOBEG*PLTPOP*10.0), &
                    LAIC,SAID,CAIC,NINT(TWAD),NINT(SDWAD),NINT(RWAD),NINT(CWAD),NINT(LLWADOUT),NINT(STWADOUT),NINT(HWAD),HIAD, &
                    NINT(CRWADOUT),NINT(RSWAD),SENTOPLITTERAC,SENROOTC,RSCD*100.0,NINT(HNUMAD),HWUDC,NINT(BRNUMST),SLAOUT, &
                    RTDEP/100.0,PTF,H2OA,AMIN1(99.9,WAVR),AMIN1(15.0,WUPR),1.0-WFP,1.0-WFG,1.0-NFP,1.0-NFG,AMIN1(2.0,NUPRATIO), &
                    1.0-TFP,1.0-TFG,1.0-DFOUT
501             FORMAT(I5,I4,2I6,F6.1,A6,F6.1,F6.3,F6.2,F6.1,A6,F6.3,A6,4I6,3I6,F6.3,2I6,2A6,F6.2,I6,A6,I6,F6.1,2F6.2,F6.1,F6.1, &
                    F6.2,2F6.2,2F6.2,F6.1,2F6.2,F6.2)
                     ! End Plantgro writes
                     
                     ! PlantGroReductionFactors
                     IF (YEARDOY.GT.PLYEARDOY) THEN
                         TCDIF = TCAN - TMEAN
                     ELSE  
                         TCDIF = -99
                     ENDIF
                     IF (YEARDOY.EQ.PLYEARDOY) THEN
                         OPEN (UNIT = NOUTPGF, FILE = OUTPGF,POSITION = 'APPEND')
                         IF (FILEIOT(1:2).EQ.'DS') THEN
                             CALL HEADER(2, NOUTPGF, RUN)
                         ELSE
                             WRITE (NOUTPGF,'(/,A79,/)') OUTHED
                             WRITE (NOUTPGF,103) MODEL
                             WRITE (NOUTPGF,1031) MODNAME
                             WRITE (NOUTPGF,104)EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
                             WRITE (NOUTPGF,102) TN,TNAME
                             WRITE (NOUTPGF,107) CROP,VARNO,VRNAME
                             CALL Calendar (year,doy,dom,month)
                             WRITE(NOUTPGF,108)month,dom,plyeardoy,NINT(pltpopp),NINT(rowspc)
                         ENDIF
                         WRITE (NOUTPGF,2215)
2215                     FORMAT ('!........DATES.......  ...TEMP... STAGE ',' ...PHENOLOGY.... ',' ....PHOTOSYNTHESIS.... ', &
                            ' .....GROWTH..... ','H2O STRESS DETERMINANTS',' .N STRESS DETERMINANTS.')
                         WRITE (NOUTPGF,2205)
2205                     FORMAT ('@YEAR DOY   DAS   DAP TMEAN TCDIF  GSTD','    DU DYLFD','  TFPD  WFPD  NFPD CO2FD RSFPD', &
                            '  TFGD  WFGD  NFGD',' WAVRD WUPRD  SWXD  EOPD','  SNXD LN%RD SN%RD RN%RD            ')
                     ENDIF  ! End Plantgro header writes
                     WRITE (NOUTPGF,507)YEAR,DOY,DAS,DAP,TMEAN,TCDIF,BRSTAGEC,DU,1.0-DFOUT,1.0-TFP,1.0-WFP,1.0-NFP,1.0-CO2FP, &
                         1.0-RSFP,1.0-TFG,1.0-WFG,1.0-NFG,AMIN1(99.9,WAVR),AMIN1(15.0,WUPR),H2OA,EOP,SNO3PROFILE+SNH4PROFILE, &
                         LNCR,SNCR,RNCR
507                      FORMAT(I5,I4,2I6,2F6.1,A6,F6.1,F6.2,5F6.2,3F6.2,2F6.2,F6.1,F6.2,F6.1,3F6.2)
                         ! End Plantgro reduction factor writes
                         
                         ! PlantGr2
                         IF (YEARDOY.EQ.PLYEARDOY) THEN
                             OPEN (UNIT = NOUTPG2, FILE = OUTPG2, STATUS='UNKNOWN',POSITION = 'APPEND')
                             IF (FILEIOT(1:2).EQ.'DS') THEN
                                 CALL HEADER(2, NOUTPG2, RUN)
                             ELSE
                                 WRITE (NOUTPG2,'(/,A79,/)') OUTHED
                                 WRITE (NOUTPG2,103) MODEL
                                 WRITE (NOUTPG2,1031) MODNAME
                                 WRITE (NOUTPG2,104) EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
                                 WRITE (NOUTPG2,102) TN,TNAME
                                 WRITE (NOUTPG2,107) CROP,VARNO,VRNAME
                                 WRITE(NOUTPG2,108) month,dom,plyeardoy,NINT(pltpopp),NINT(rowspc)
                             ENDIF 
                             WRITE (NOUTPG2,2251)
2251                         FORMAT ('@YEAR DOY   DAS   DAP TMEAN  GSTD  RSTD',' LAIPD LAISD  LAID  CHTD SDWAD SNWLD SNWSD', &
                                 '  H#AD  HWUD',' SHRTD  PTFD  RDPD','  RL1D  RL2D  RL3D  RL4D  RL5D  RL6D', &
                                 '  RL7D  RL8D  RL9D RL10D')
                         ENDIF   ! Plantgr2 header writes
                         LAIPROD = PLA*PLTPOP*0.0001
                         CALL Csopline(laiprodc,laiprod)
                         CALL Csopline(canhtc,canht)
                         L = MAX(1,LNUMSG-INT(LLIFG))
                         WRITE (NOUTPG2,502)YEAR,DOY,DAS,DAP,TMEAN,BRSTAGEC,BRSTAGE,LAIPRODC,SENLA*PLTPOP*0.0001,LAIC,CANHTC, &
                             SDWAD,SENTOPLITTERAC,SENROOTC,NINT(HNUMAD),HWUDC,SHRTD,PTF,RTDEP/100.0,(RLV(I),I=1,10)
502                          FORMAT(I5,I4,2I6,F6.1,A6,F6.2,A6,F6.2,A6,A6,F6.1,2A6,I6,A6,2F6.2,F6.3,10F6.2)
                             ! End PlantGr2 writes
                             
                             ! PlantN
                             IF (ISWNIT.NE.'N') THEN
                                 IF (YEARDOY.EQ.PLYEARDOY) THEN
                                     OPEN (UNIT = NOUTPN, FILE = OUTPN, STATUS='UNKNOWN',POSITION = 'APPEND')
                                     IF (FILEIOT(1:2).EQ.'DS') THEN
                                         CALL HEADER(2, NOUTPN, RUN)
                                     ELSE
                                         WRITE (NOUTPN,'(/,A79,/)') OUTHED
                                         WRITE (NOUTPN,103) MODEL
                                         WRITE (NOUTPN,1031) MODNAME
                                         WRITE (NOUTPN,104)EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
                                         WRITE (NOUTPN,102) TN,TNAME
                                         WRITE (NOUTPN,107) CROP,VARNO,VRNAME
                                         WRITE (NOUTPN,108)month,dom,plyeardoy,NINT(pltpopp),NINT(rowspc)
                                     ENDIF 
                                     WRITE (NOUTPN,2252)
2252                                 FORMAT ('@YEAR DOY   DAS   DAP TMEAN  GSTD  NUAD', &
                                        '  TNAD SDNAD  RNAD  CNAD  LNAD  SNAD  HNAD  HIND',' RSNAD SNN0D SNN1D', &
                                        '  RN%D  LN%D  SN%D  HN%D SDN%D  VN%D',' LN%RD SN%RD RN%RD  VCN%  VMN% NUPRD', &
                                        ' NDEMD NFLF2')
                                 ENDIF  ! Plantn header writes
                                 CALL Csopline(senn0c,sennal(0))
                                 CALL Csopline(sennsc,sennas)
                                 WRITE (NOUTPN,503)YEAR,DOY,DAS,DAP,TMEAN,BRSTAGEC,NUPAC,TNAD,SDNAD,RNAD,CNAD,LLNAD,SNAD,HNAD, &
                                     HINDC,RSNAD,SENN0C,SENNSC,RANC*100.0,LANC*100.0,SANCOUT*100.0,AMIN1(9.9,HNC*100.0), &
                                     SDNC*100.0,AMIN1(9.9,VANC*100.0),LNCR,SNCR,RNCR,VCNC*100.0,VMNC*100.0,AMIN1(2.,NUPRATIO), &
                                     ANDEM,1.0-NFLF2(0)
503                              FORMAT(I5,I4,2I6,F6.1,A6,F6.1,F6.1,2F6.2,4F6.1,A6,F6.2,2A6,3F6.3,3F6.3,3F6.3,F6.2,F6.2,F6.2, &
                                     F6.1,F6.1)
                                     
                             ENDIF  ! ISWNIT  Plantn writes
                             
            ELSE ! (IDETG.NE.'N'.AND.IDETL.NE.'0').OR.IDETL.EQ.'A'
                
                IF (IDETGNUM.LE.0) THEN
                    OPEN (UNIT=FNUMTMP, FILE=OUTPG, STATUS = 'UNKNOWN')
                    CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
                    OPEN (UNIT=FNUMTMP, FILE=OUTPG2, STATUS = 'UNKNOWN')
                    CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
                    OPEN (UNIT=FNUMTMP, FILE=OUTPGF, STATUS = 'UNKNOWN')
                    CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
                    OPEN (UNIT=FNUMTMP, FILE=OUTPN, STATUS = 'UNKNOWN')
                    CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
                    IDETGNUM = IDETGNUM + 1 
                ENDIF  
                
            ENDIF ! End ((IDETG.NE.'N'.AND.IDETL.NE.'0').OR.IDETL.EQ.'A'
            
            ELSEIF(YEARDOY.LT.PLYEARDOY.AND.(MOD(DAS,FROPADJ)).EQ.0.AND.IPLTI.EQ.'A') THEN
                
                ! Automatic planting
                !WRITE (fnumwrk,*) 'Yeardoy ',yeardoy
                !WRITE (fnumwrk,*) 'Water thresholds ',swpltl,swplth
                !WRITE (fnumwrk,*) 'Water ',avgsw
                !WRITE (fnumwrk,*) 'Temperature thresholds ',pttn,ptx
                !WRITE (fnumwrk,*) 'Temperature ',tsdep
                
            ENDIF  ! End time-course outputs (appropriate day, etc.)
            ! (MOD(DAS,FROPADJ).EQ.0.AND.YEARDOY.GE.PLYEARDOY),etc..
            
            !***************************************************************************************************************
            IF (STGYEARDOY(11).EQ.YEARDOY .OR.DYNAMIC.EQ.SEASEND) THEN         ! If harvest/failure day
            !***************************************************************************************************************


                !-----------------------------------------------------------------------------------------------------------
                !         IDETO OUTPUTS AND NECESSARY DATA INPUTS (Evaluate & Overview)
                !-----------------------------------------------------------------------------------------------------------
          
                IF (IDETO.NE.'N'.OR.IDETL.EQ.'0') THEN
                    
                    tiernum = 0
                    
                    cnamm = -99
                    cnpchm = -99
                    cwamm = -99
                    edatm = -99
                    edapm = -99
                    gdapm = -99
                    hnamm = -99
                    hnpcmm = -99
                    hiamm = -99
                    hinmm = -99 
                    hnumamm = -99
                    srnoamm = -99
                    srnogmm = -99
                    srnogmm = -99
                    srnpcm = -99
                    hwahm = -99
                    hwamm = -99
                    hyamm = -99
                    hwumm = -99
                    srwumm = -99
                    laixm = -99
                    lnumsmm = -99
                    mdapm = -99
                    mdatm = -99
                    nupacm = -99
                    rnamm = -99
                    rswamm = -99
                    rscmm = -99
                    rwamm = -99
                    sennatcm = -99
                    senwacmm = -99
                    shrtmm = -99
                    psdatm = -99
                    vnamm = -99
                    vnpcmm = -99
                    vwamm = -99
                    laixt = -99.0
                    valuer = -99.0
                    ! Variables from time-course file
                    LAIXT = -99.0 
                    LNUMT = -99.0 
                    CWADT = -99.0 
                    HWADT = -99.0 
                    HIADT = -99.0 
                    HWUT = -99.0 
                    HNUMAT = -99.0 
                    HNUMET = -99.0 
                    MDATT = -99 
                    
                    ! Reading A-file
                    CALL LTRIM2 (FILEIO,filenew)
                    FILELEN = TVILENT(FILENEW)
                    FILELEN = MAX(FILELEN-12, 0) 
                    
                    IF (TVILENT(FILEADIR).GT.3) THEN
                        IF (FILEADIR(TVILENT(FILEADIR):TVILENT(FILEADIR)).NE.'/') THEN                       ! MF31AU14 REPLACED SLASH  WITH A LITERAL
                        FILEA = FILEADIR(1:TVILENT(FILEADIR))//'/'//EXCODE(1:8)//'.'//EXCODE(9:10)//'A'      ! MF31AU14 REPLACED SLASH  WITH A LITERAL
                        ELSE
                            FILEA = FILEADIR(1:TVILENT(FILEADIR))//EXCODE(1:8)//'.'//EXCODE(9:10)//'A'
                        ENDIF
                    ELSE
                        FILEA = FILENEW(1:FILELEN-12)//EXCODE(1:8)//'.'//EXCODE(9:10)//'A'
                    ENDIF       
                    FEXISTA = .FALSE.
                    INQUIRE (FILE = FILEA,EXIST = FEXISTA)
                    IF (.not.FEXISTA) THEN
                        WRITE (Message(1),'(A23,A50)')'Could not find A-file: ',filea(1:50)
                        WRITE (Message(2),'(A23,A50)')'Experiment file:       ',fileio(1:50)
                        CALL WARNING(2,'CSCAS',MESSAGE)
                        OPEN (UNIT=FNUMTMP, FILE=FILEA, STATUS = 'UNKNOWN')
                        CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
                    ELSE
                        ! Yield at harvest   
                        CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HWAM',hwamm)
                        IF (hwamm.GT.0.0.AND.HWAMM.LT.50.0) HWAMM = HWAMM*1000.0
                        IF (HWAMM.LE.0.0) THEN
                            CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HYAM',hyamm)
                            IF (hyamm.LE.0.0) THEN
                                CALL AREADR (FILEA,TN,RN,SN,ON,CN,'GYAM',hyamm)
                            ENDIF  
                            IF (hyamm.GT.0.0.AND.HYAMM.LT.50.0) HYAMM = HYAMM*1000.0
                        ENDIF
                        IF (HWAMM.LE.0.0.AND.HYAMM.GT.0..AND.HMPC.GT.0.0) HWAMM = HYAMM * (1.0-HMPC/100.0)
                        CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HWUM',hwumm)
                        CALL AREADR (FILEA,TN,RN,SN,ON,CN,'LAIX',laixm)
                        CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CWAM',cwamm)
                        CALL AREADR (FILEA,TN,RN,SN,ON,CN,'BWAH',vwamm)
                        CALL AREADR (FILEA,TN,RN,SN,ON,CN,'H#AM',hnumamm)
                        IF (hnumamm.le.0.0)CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HNOAM',hnumamm)
                        CALL AREADR (FILEA,TN,RN,SN,ON,CN,'H#SM',hnumgmm)
                        IF (hnumgmm.le.0.0)CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HNOSM',hnumgmm)
                        IF (hnumgmm.le.0.0)CALL AREADR (FILEA,TN,RN,SN,ON,CN,'H#UM',hnumgmm)
                        IF (hnumgmm.le.0.0)CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HNOUM',hnumgmm)
                        CALL AREADR (FILEA,TN,RN,SN,ON,CN,'L#SM',lnumsmm)
                        IF (lnumsmm.le.0.0)CALL AREADR (FILEA,TN,RN,SN,ON,CN,'LNOSM',lnumsmm)
                        CALL AREADR (FILEA,TN,RN,SN,ON,CN,'BR#SH',brnumshm)
                        
                        CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CNAM',cnamm)
                        CALL AREADR (FILEA,TN,RN,SN,ON,CN,'VNAM',vnamm)
                        CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HNAM',hnamm)
                        CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CN%M',cnpchm)
                        IF (cnpchm.le.0.0)CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CNPCM',cnpchm)
                        CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HN%M',hnpcmm)
                        IF (hnpcmm.le.0.0)CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HNPCM',hnpcmm)
                        IF (HNPCMM.LE.0.0) HNPCMM = -99   
                        CALL AREADR (FILEA,TN,RN,SN,ON,CN,'VN%M',vnpcmm)
                        IF (vnpcmm.le.0.0)CALL AREADR (FILEA,TN,RN,SN,ON,CN,'VNPCM',vnpcmm)
                        
                        CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HIAM',hiamm)
                        IF (HIAMM.GE.1.0) HIAMM = HIAMM/100.0
                        
                        CALL AREADI (FILEA,TN,RN,SN,ON,CN,'EDAT',edatm)
                        CALL AREADI (FILEA,TN,RN,SN,ON,CN,'GDAT',gdatm)
                        
                        DO L = 1,PSNUM              
                            CALL AREADI (FILEA,TN,RN,SN,ON,CN,psabv(l),psdatm(l))
                            CALL LTRIM(PSABV(L)) 
                            IF (PSABV(L).EQ.'TSAT')CALL AREADI (FILEA,TN,RN,SN,ON,CN,'TSDAT',psdatm(l))
                            IF (PSDATM(L).GT.0.0.AND.PSDATM(L).LT.1000) THEN
                                CALL AREADI (FILEA,TN,RN,SN,ON,CN,'YEAR',yearm)
                                IF (YEARM.GT.0.0) PSDATM = CSYDOY(YEARM,PSDATM(L))
                            ENDIF
                            IF (psdatm(l).gt.0) then
                                psdapm(l) = Dapcalc(psdatm(l),plyear,plday)
                            ELSE
                                psdapm(l) = -99
                            ENDIF 
                        ENDDO
                    ENDIF ! File-A exists
                    
                    ! Reading T-file to complement A-data and writing MEASURED
                    IF (IDETG.NE.'N'.OR.IDETL.EQ.'A') THEN 
                        STARNUMO = STARNUMO + 1 ! Number of datasets in Simop file
                        CALL LTRIM2 (FILEIO,filenew)
                        FILELEN = TVILENT(FILENEW)
                        FILET=FILENEW(1:FILELEN-12)//EXCODE(1:8)//'.'//EXCODE(9:10)//'T'
                        FEXISTT  = .FALSE.
                        INQUIRE (FILE = FILET,EXIST = FEXISTT)
                        IF (.not.FEXISTT) THEN
                            WRITE (Message(1),'(A23,A50)')'Could not find T-file: ',filet(1:50)
                            CALL WARNING(1,'CSCAS',MESSAGE)
                        ELSE
                            TLINENUM = 0
                            OPEN (UNIT=FNUMT,FILE=FILET)
                            OPEN (UNIT=FNUMMEAS,FILE=FNAMEMEAS,POSITION='APPEND')
                            COLNUM = 1
                            L1 = 0
                            DO
                                READ(FNUMT,'(A180)',END = 5555)LINET
                                TLINENUM = TLINENUM + 1  ! Used to check if file empty
                                L1 = 0
                                L2 = 0
                                ! First IF to jump over comments and blanks
                                IF (LEN(LINET).GT.0.AND.LINET(1:1).NE.'!') THEN
                                    IF (LINET(1:7).EQ.'*DATA(T' .OR.LINET(1:7).EQ.'*EXP.DA' .OR.LINET(1:7).EQ.'*EXP. D' .OR. &
                                        LINET(1:7).EQ.'*TIME_C' .OR.LINET(1:7).EQ.'$EXPERI') THEN
                                        TNCHAR = TL10FROMI(TN)
                                        LENLINE = TVILENT(LINET)
                                        IF(LINET(1:7).EQ.'*EXP.DA'.OR.LINET(1:7).EQ.'*EXP. D'.OR.LINET(1:7).EQ.'$EXPERI')THEN
                                            GROUP = 'A'
                                            DO L = 1,30
                                                IF (LINET(L:L+1).EQ.': ') L1 = L+2
                                                IF (LINET(L:L).EQ.':'.AND.LINET(L+1:L+1).NE.' ')L1 = L+1   
                                                IF (L1.GT.0.AND.L.GT.L1+9.AND.LINET(L:L).NE.' ') THEN
                                                    L2 = L ! Start of group information in tfile
                                                    EXIT
                                                ENDIF
                                            ENDDO
                                            LENTNAME = MIN(15,TVILENT(TNAME))
                                            LENGROUP = MIN(L2+14,LENLINE)
                                            IF (TVILENT(TNCHAR).EQ.1) THEN
                                                LINESTAR = LINET(L1:L1+9)//' '//TNCHAR(1:1)//' '//TNAME(1:LENTNAME)
                                            ELSEIF (TVILENT(TNCHAR).EQ.2) THEN
                                                LINESTAR = LINET(L1:L1+9)//' '//TNCHAR(1:2)//' '//TNAME(1:LENTNAME)
                                            ELSEIF (TVILENT(TNCHAR).EQ.3) THEN
                                                LINESTAR = LINET(L1:L1+9)//' '//TNCHAR(1:3)//' '//TNAME(1:LENTNAME)
                                            ENDIF
                                            LENLINESTAR = TVILENT(LINESTAR)
                                        ENDIF
                                        ELSEIF (LINET(1:1).EQ.'@') THEN
                                            DO L = 1,TVILENT(LINET)
                                                IF (LINET(L:L+2).EQ.' GW') LINET(L:L+2) = ' HW'
                                            END DO
                                            DATECOL = Tvicolnm(linet,'DATE')
                                            YEARCOL = Tvicolnm(linet,'YEAR')
                                            DOYCOL = Tvicolnm(linet,'DOY')
                                            IF (DOYCOL.LE.0) DOYCOL = Tvicolnm(linet,'DAY')
                                            RPCOL = Tvicolnm(linet,'RP')
                                            LAIDCOL = Tvicolnm(linet,'LAID')
                                            LNUMCOL = Tvicolnm(linet,'L#SD')
                                            CWADCOL = Tvicolnm(linet,'CWAD')
                                            HWADCOL = Tvicolnm(linet,'HWAD')
                                            HIADCOL = Tvicolnm(linet,'HIAD')
                                            HWTUCOL = Tvicolnm(linet,'HWUD')
                                            HNUMACOL = Tvicolnm(linet,'H#AD')
                                            HNUMECOL = Tvicolnm(linet,'H#ED')
                                            GSTDCOL = Tvicolnm(linet,'GSTD')
                                            LENLINE = TVILENT(LINET)
                                            LINET(LENLINE+1:LENLINE+12) = '   DAP   DAS'
                                            LINET(1:1) = '@'
                                            TIERNUM = TIERNUM + 1
                                            IF (TIERNUM.LT.10) THEN
                                                WRITE(TIERNUMC,'(I1)') TIERNUM
                                            ELSE
                                                WRITE(TIERNUMC,'(I2)') TIERNUM
                                            ENDIF
                                            LINESTAR2 = '*TIER('//TIERNUMC//'):'//LINESTAR(1:LENLINESTAR)//LINET(14:LENLINE)
                                            IF (IDETG.NE.'N') THEN 
                                                WRITE (FNUMMEAS,*) ' '
                                                WRITE (FNUMMEAS,'(A80)') LINESTAR2(1:80)
                                                WRITE (FNUMMEAS,*) ' '
                                                WRITE (FNUMMEAS,'(A180)') LINET(1:180)
                                            ENDIF  
                                            STARNUMM = STARNUMM + 1              ! # datasets
                                        ELSE
                                            CALL Getstri (LINET,COLNUM,VALUEI)
                                            IF (VALUEI.EQ.TN) THEN
                                                IF (DATECOL.GT.0.OR.DOYCOL.GT.0) THEN
                                                    IF (DATECOL.GT.0) THEN
                                                        CALL Getstri (LINET,DATECOL,DATE)
                                                    ELSEIF (DATECOL.LE.0) THEN
                                                        CALL Getstri (LINET,DOYCOL,DOY)
                                                        CALL Getstri (LINET,YEARCOL,YEAR)
                                                        IF (YEAR.GT.2000) YEAR = YEAR-2000
                                                        IF (YEAR.GT.1900) YEAR = YEAR-1900
                                                        DATE = YEAR*1000+DOY
                                                    ENDIF
                                                    DAP = MAX(0,CSTIMDIF(PLYEARDOY,DATE))
                                                    DAS = MAX(0,CSTIMDIF(YEARSIM,DATE))
                                                    DAPCHAR = TL10FROMI(DAP)
                                                    IF (TVILENT(DAPCHAR).EQ.1) THEN
                                                        DAPWRITE = '     '//DAPCHAR(1:1)
                                                    ELSEIF (TVILENT(DAPCHAR).EQ.2) THEN
                                                        DAPWRITE = '    '//DAPCHAR(1:2)
                                                    ELSEIF (TVILENT(DAPCHAR).EQ.3) THEN
                                                        DAPWRITE = '   '//DAPCHAR(1:3)
                                                    ENDIF
                                                    LENLINE = TVILENT(LINET)
                                                    LINET(LENLINE+1:LENLINE+6) = DAPWRITE(1:6)
                                                    DAPCHAR = TL10FROMI(DAS)
                                                    IF (TVILENT(DAPCHAR).EQ.1) THEN
                                                        DAPWRITE = '     '//DAPCHAR(1:1)
                                                    ELSEIF (TVILENT(DAPCHAR).EQ.2) THEN
                                                        DAPWRITE = '    '//DAPCHAR(1:2)
                                                    ELSEIF (TVILENT(DAPCHAR).EQ.3) THEN
                                                        DAPWRITE = '   '//DAPCHAR(1:3)
                                                    ENDIF
                                                    LENLINE = TVILENT(LINET)
                                                    LINET(LENLINE+1:LENLINE+6) = DAPWRITE(1:6)
                                                ENDIF
                                                CALL Getstri (LINET,RPCOL,VALUEI)
                                                IF (IDETG.NE.'N') THEN 
                                                    IF (VALUEI.LE.0)WRITE (FNUMMEAS,'(A180)') LINET
                                                ENDIF  
                                                
                                                ! T-FILE STUFF FOR OUTPUT OF INDIVIDUAL VARS
                                                ! Below is to pick up variables for output files
                                                IF (IDETL.EQ.'A') THEN
                                                    IF (GROUP.EQ.'A') THEN
                                                        !WRITE(fnumwrk,*)' Picking vars from t-file'
                                                        CALL Getstrr (LINET,LAIDCOL,VALUER)
                                                        IF (VALUER.GT.LAIXT) LAIXT = VALUER
                                                        CALL Getstrr (LINET,LNUMCOL,VALUER)
                                                        IF (VALUER.GT.LNUMT) LNUMT = VALUER
                                                        CALL Getstrr (LINET,CWADCOL,VALUER)
                                                        IF (VALUER.GT.0.0) CWADT = VALUER
                                                        CALL Getstrr (LINET,HWADCOL,VALUER)
                                                        IF (VALUER.GT.0.0) HWADT = VALUER
                                                        CALL Getstrr (LINET,HIADCOL,VALUER)
                                                        IF (VALUER.GT.0.0) HIADT = VALUER
                                                        IF (HIADT.GE.1.0) HIADT = HIADT/100.0
                                                        CALL Getstrr (LINET,HWTUCOL,VALUER)
                                                        IF (VALUER.GT.0.0) HWUT = VALUER
                                                        CALL Getstrr (LINET,HNUMACOL,VALUER)
                                                        IF (VALUER.GT.0.0) HNUMAT = VALUER
                                                        CALL Getstrr (LINET,HNUMECOL,VALUER)
                                                        IF (VALUER.GT.0.0) HNUMET = VALUER
                                                        CALL Getstrr (LINET,GSTDCOL,VALUER)
                                                        IF (VALUER.GT.FLOAT(MSTG*10).AND.MDATT.LE.0)MDATT = DATE
                                                        ! To indicate that t data present
                                                        tdatanum = 1
                                                    ENDIF ! End picking variables from t for a     
                                                ENDIF ! End of details flag 
                                            ENDIF ! End correct treatment 
                                        ENDIF ! End particular data lines
                                ENDIF ! End valid (ie.non comment) line
                            ENDDO
5555                        CONTINUE
                            ! If T-file was empty
                            IF (TLINENUM.LT.4) THEN
                                tdatanum = 0
                                WRITE (Message(1),'(A23,A50)')'T-file was empty '
                                CALL WARNING(1,'CSCAS',MESSAGE)
                            ENDIF
                            CLOSE(FNUMT)
                            CLOSE(FNUMMEAS)
                        ENDIF ! End t-file reads,measured.out writes
                        
                        IF (IDETL.EQ.'A') THEN
                            ! Use T-data if A-data missing (whem output=all)
                            IF (FEXISTT) THEN
                                WRITE(Fnumwrk,*)' '
                                WRITE(Fnumwrk,'(A45)')' FINISHED SIMULATION. PREPARING FINAL OUTPUTS'
                                WRITE(Fnumwrk,*)' '
                                IF (HWAMM.LE.0.0) THEN
                                    IF (HWADT.GT.0.0) THEN
                                        HWAMM = HWADT
                                        WRITE(Fnumwrk,'(A32)')'  Time-course data used for HWAM'
                                    ENDIF
                                ELSE
                                    IF (HWADT.GT.0.0) THEN
                                        IF (ABS(100.0*ABS(HWAMM-HWADT)/HWAMM).GT.0.0) THEN
                                            WRITE(Fnumwrk,'(A48,F8.2)')'  Pc difference between final,time-course yields', &
                                                100.0*ABS(HWAMM-HWADT)/HWAMM
                                            WRITE(Fnumwrk,'(A22,I6)')'  Final yield         ',NINT(HWAMM)
                                            WRITE(Fnumwrk,'(A22,I6)')'  Time-course yield   ',NINT(HWADT)
                                        ENDIF
                                    ENDIF
                                ENDIF
                                IF (CWAMM.LE.0.0) THEN
                                    IF (CWADT.GT.0.0) THEN
                                        CWAMM = CWADT
                                        WRITE(Fnumwrk,'(A33)')'  Time-course data used for CWAMM'
                                    ENDIF
                                ELSE
                                    IF (CWADT.GT.0.0) THEN
                                        IF (ABS(100.0*ABS(CWAMM-CWADT)/CWAMM).GT.0.0) THEN
                                            WRITE(Message(1),'(A48,F8.2)')'Pc difference between final,time-course canopy =', &
                                                100.0*ABS(CWAMM-CWADT)/CWAMM
                                            WRITE(Message(2),'(A19,I6)')'Final canopy       ',NINT(CWAMM)
                                            WRITE(Message(3),'(A19,I6)')'Time-course canopy ',NINT(CWADT)
                                            CALL WARNING(3,'CSCAS',MESSAGE)
                                        ENDIF
                                    ENDIF
                                ENDIF
                                IF (LAIXM.LE.0.0.AND.LAIXT.GT.0.0) THEN
                                    LAIXM = LAIXT
                                    WRITE(Message(1),'(A31)')'Time-course data used for LAIXM'
                                    CALL WARNING(1,'CSCAS',MESSAGE)
                                    WRITE(Fnumwrk,'(A33)')'  Time-course data used for LAIXM'
                                ENDIF
                                IF (LNUMSMM.LE.0.0.AND.LNUMSMM.GT.0.0) THEN
                                    LNUMSMM = LNUMT
                                    WRITE(Message(1),'(A33)')'Time-course data used for LNUMSMM'
                                    CALL WARNING(1,'CSCAS',MESSAGE)
                                    WRITE(Fnumwrk,'(A35)')'  Time-course data used for LNUMSMM'
                                ENDIF
                                IF (HIAMM.LE.0.0.AND.HIADT.GT.0.0) THEN
                                    HIAMM = HIADT
                                    WRITE(Message(1),'(A31)')'Time-course data used for HIAMM'
                                    CALL WARNING(1,'CSCAS',MESSAGE)
                                    WRITE(Fnumwrk,'(A33)')'  Time-course data used for HIAMM'
                                ENDIF
                                IF (HWUMM.LE.0.0.AND.HWUT.GT.0.0) THEN
                                    HWUMM = HWUT
                                    WRITE(Message(1),'(A31)')'Time-course data used for HWUMM'
                                    CALL WARNING(1,'CSCAS',MESSAGE)
                                    WRITE(Fnumwrk,'(A33)')'  Time-course data used for HWUMM'
                                ENDIF
                                IF (HNUMAMM.LE.0.0.AND.HNUMAT.GT.0.0) THEN
                                    HNUMAMM = HNUMAT
                                    WRITE(Message(1),'(A31)')'Time-course data used for H#AT'
                                    CALL WARNING(1,'CSCAS',MESSAGE)
                                    WRITE(Fnumwrk,'(A33)')'  Time-course data used for H#AT'
                                ENDIF
                                IF (HNUMGMM.LE.0.0.AND.HNUMET.GT.0.0) THEN
                                    HNUMGMM = HNUMET
                                    WRITE(Message(1),'(A32)')'Time-course data used for H#GMM'
                                    CALL WARNING(1,'CSCAS',MESSAGE)
                                    WRITE(Fnumwrk,'(A34)')'  Time-course data used for H#GMM'
                                ENDIF
                            ENDIF
                            DO L = 1,PSNUM
                                IF (PSABV(L).EQ.'MDAT'.AND.PSDATM(L).LE.0.0) THEN
                                    IF (MDATT.GT.0) THEN
                                        PSDATM(L) = INT(MDATT)
                                        WRITE(Message(1),'(A31)')'Time-course data used for MDATM'
                                        CALL WARNING(1,'CSCAS',MESSAGE)
                                        WRITE(Fnumwrk,'(A33)')'  Time-course data used for MDATM'
                                    ENDIF  
                                ENDIF
                            ENDDO
                        ENDIF ! END OF USE T-DATA TO FILL IN FOR MISSING A-DATA
                    ELSE  ! For IDETG.NE.'N'.OR.IDETL.EQ.'A' 
                        ! No call for measured.out! Delete old files.
                        OPEN (UNIT=FNUMTMP,FILE=FNAMEMEAS,STATUS = 'UNKNOWN')
                        CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
                    ENDIF ! End A-file reads,T-file reads,Measured writes,T->A
                    
                    ! Check data and calculate equivalents,if needed
                    
                    ! Emergence and maturity dates 
                    IF (edatm.LE.0) edatm = edatmx ! If no Afile data,use Xfile
                    IF (mdatm.LE.0) mdatm = psdatm(mstg)
                    
                    ! Product wt at harvesst
                    IF (hwahm.GT.0.AND.hwamm.LE.0) hwamm = hwahm/(hpcf/100.0)
                    
                    ! Product wt at harvest
                    IF (hwamm.GT.0.AND.hwahm.LE.0) hwahm = hwamm*(hpcf/100.0)
                    
                    ! Canopy wt at harvest 
                    IF (vwamm.GT.0.AND.hwamm.GT.0) cwamm = vwamm+hwamm
                    
                    ! Vegetative wt at harvest 
                    IF (HPROD.NE.'SR') THEN
                        IF (hwamm.GT.0.AND.cwamm.GT.0) vwamm = cwamm-hwamm
                    ELSE
                        IF (cwamm.GT.0) vwamm = cwamm
                    ENDIF
                    
                    ! Harvest index at harvest 
                    IF (hiamm.LE.0.0) THEN
                        IF (cwamm.GT.0.AND.hwamm.GT.0) THEN
                            IF (HPROD.EQ.'SR') THEN
                                hiamm = hwamm/(cwamm+hwamm)
                            ELSE 
                                hiamm = hwamm/cwamm
                            ENDIF
                        ENDIF  
                    ELSE
                        IF (cwamm.GT.0.AND.hwamm.GT.0) THEN
                            hiammtmp = hwamm/cwamm
                            IF (hiammtmp/hiam.GT.1.1 .OR. hiammtmp/hiam.LT.0.9) THEN
                                IF (ABS(hiammtmp-hiamm)/hiamm.GT.0.05) THEN
                                    WRITE (fnumwrk,*) 'Reported HI not consistent',' with yield and total weight data  '
                                    WRITE (fnumwrk,*) ' Reported HI   ',hiamm
                                    WRITE (fnumwrk,*) ' Calculated HI ',hiammtmp
                                    WRITE (fnumwrk,*) ' Will use reported value '
                                ENDIF
                            ENDIF
                        ENDIF
                    ENDIF
                    
                    ! Product unit wt at harvest 
                    IF (hwumm.GT.1.0) hwumm = hwumm/1000.0 ! mg->g
                    IF (hwumm.LE.0.AND.hnumamm.GT.0) THEN
                        IF (hwamm.GT.0.0) hwumm=hwamm*0.1/hnumamm  ! kg->g
                    ELSE
                        IF (hwamm.gt.0.0.AND.hnumamm.GT.0.0) THEN
                            hwumyld = hwamm*0.1/hnumamm
                            IF (ABS(hwumyld-hwumm)/hwumm.GT.0.05) THEN
                                WRITE (fnumwrk,*)' '
                                WRITE (fnumwrk,'(A14)')' MEASURED DATA'
                                WRITE (fnumwrk,'(A36,A33)')' Reported product wt.not consistent', &
                                    ' with yield and product # data   '
                                WRITE (fnumwrk,*) ' Reported wt   ',hwumm
                                WRITE (fnumwrk,*) ' Calculated wt ',hwumyld
                                WRITE (fnumwrk,*) '   Yield       ',hwamm
                                WRITE (fnumwrk,*) '   Kernel no   ',hnumamm
                                WRITE (fnumwrk,*) ' Will use reported value '
                            ENDIF
                        ENDIF
                    ENDIF
                    
                    ! Product number at harvest 
                    IF (HNUMAMM.GT.0.0) THEN
                        IF (PLTPOPP.GT.0) THEN 
                            HNUMPMM = HNUMAMM/PLTPOPP
                        ELSE
                            HNUMPMM = -99.0
                        ENDIF  
                    ELSE
                        HNUMPMM = -99.0
                    ENDIF
                    
                    ! Shoot/root ratio at harvest 
                    IF (rwamm.GT.0.0) shrtmm = cwamm/rwamm
                    
                    ! Reserves concentration at harvest 
                    IF (vwamm+rwamm.GT.0.AND.rswamm.GT.0.0) rscmm = rswamm/(vwamm+rwamm)
                    
                    ! Canopy N at harvest 
                    IF (vnamm.GT.0.AND.cnamm.LE.0) cnamm = vnamm
                    
                    ! Total N at harvest 
                    IF (CNAMM.GT.0.0.AND.RNAMM.GT.0.0) THEN
                        tnamm = cnamm+rnamm
                    ELSE
                        tnamm = -99.0
                    ENDIF
                    
                    ! Vegetative N at harvest  
                    IF (vnamm.LE.0) THEN
                        IF (hnamm.GE.0.AND.cnamm.GT.0) vnamm=cnamm-hnamm
                    ENDIF
                    
                    ! Product N harvest index at harvest 
                    IF (cnamm.GT.0.AND.hnamm.GT.0) hinmm=hnamm/cnamm
                    
                    ! Vegetative N concentration at harvest 
                    IF (vnpcmm.LE.0) THEN
                        IF (vwamm.GT.0.AND.vnamm.GT.0) vnpcmm = (vnamm/vwamm)*100
                    ENDIF
                    
                    ! Product N concentration at harvest 
                    IF (hnpcmm.LE.0) THEN
                        IF (hwamm.GT.0.AND.hnamm.GT.0) hnpcmm = (hnamm/hwamm)*100
                    ENDIF
                    
                    ! Leaf N concentration at harvest 
                    IF (cnpchm.LE.0.AND.cnamm.GT.0.AND.cwamm.GT.0.0)cnpchm = cnamm/cwamm
                    
                    
                    ! Express dates as days after planting
                    edapm = -99
                    edapm = Dapcalc(edatm,plyear,plday)
                    IF (edapm.GT.200) THEN
                        WRITE (Message(1),'(A31,A31,A11)')'Measured emergence over 200DAP ', &
                            'Maybe reported before planting.','Check files'
                        CALL WARNING(1,'CSCAS',MESSAGE)
                    ENDIF
                    gdapm = Dapcalc(gdatm,plyear,plday)
                    
                    IF (mdapm.LE.0) mdapm = Dapcalc(mdatm,plyear,plday)
                    
                    ! Check that -99 not multiplied or divided 
                    IF (hnumgm.LT.0.0) hnumgm = -99
                    IF (hnumam.LT.0.0) hnumam = -99
                    IF (hnumgmm.LT.0.0) hnumgmm = -99
                    IF (hnumamm.LT.0.0) hnumamm = -99
                    
                    ! Put N variables to -99 if N switched off
                    IF (ISWNIT.EQ.'N') THEN
                        hnpcm = -99
                        vnpcm = -99
                        cnam = -99
                        hnam = -99
                        hinm = -99
                        sdnap = -99
                        rnam = -99
                        nupac = -99
                    ENDIF  
                    
                    ! Create character equivalents for outputing
                    CALL Csopline(hwumchar,hwum)
                    CALL Csopline(hwummchar,AMAX1(-99.0,hwumm))
                    CALL Csopline(hiamchar,AMAX1(-99.0,hiam))
                    CALL Csopline(hiammchar,AMAX1(-99.0,hiamm))
                    CALL Csopline(hinmchar,AMAX1(-99.0,hinm))
                    CALL Csopline(hinmmchar,AMAX1(-99.0,hinmm))
                    CALL Csopline(hnpcmchar,AMAX1(-99.0,hnpcm))
                    CALL Csopline(hnpcmmchar,AMAX1(-99.0,hnpcmm))
                    CALL Csopline(vnpcmchar,AMAX1(-99.0,vnpcm))
                    CALL Csopline(vnpcmmchar,AMAX1(-99.0,vnpcmm))
                    CALL Csopline(laixchar,AMAX1(-99.0,laix))
                    CALL Csopline(laixmchar,AMAX1(-99.0,laixm))
                    
                    ! Evaluate
                    EVHEADER = ' '
                    EVHEADER(1:14) = '*EVALUATION : '
                    IF (RUN.EQ.1.OR.(EXCODE.NE.EXCODEPREV.AND.EVALOUT.GT.1))THEN
                        IF (RUN.EQ.1) THEN
                            EVALOUT = 0
                            EVHEADNM = 0
                            EVHEADNMMAX = 7
                        ENDIF
                        IF (EXCODE.NE.EXCODEPREV) THEN
                            EVHEADNM = EVHEADNM + 1
                            OPEN (UNIT=FNUMEVAL,FILE=FNAMEEVAL,POSITION='APPEND')
                            IF (EVHEADNM.LT.EVHEADNMMAX.AND.EVHEADNMMAX.GT.1) THEN
                                LENENAME = TVILENT(ENAME)
                                WRITE (FNUMEVAL,*) ' '
                                WRITE (FNUMEVAL,993) EVHEADER,EXCODE,ENAME(1:25),MODNAME
993                             FORMAT (A14,A10,'  ',A25,2X,A8,/)
                            ELSE
                                IF (EVHEADNMMAX.GT.1) THEN
                                    WRITE (FNUMEVAL,*) ' '
                                    WRITE (FNUMEVAL,1995) EVHEADER,MODNAME,'ALL REMAIN','ING EXPERIMENTS        '
                                ELSEIF (EVHEADNM.LE.EVHEADNMMAX) THEN
                                    WRITE (FNUMEVAL,*) ' '
                                    WRITE (FNUMEVAL,1995) EVHEADER,MODNAME,'ALL EXPERI','MENTS                  '
1995                                FORMAT (A14,2X,A10,A23,2X,A8/)
                                ENDIF 
                            ENDIF
                        ENDIF
                        IF (EVHEADNM.LE.EVHEADNMMAX) THEN
                            WRITE (FNUMEVAL,994,ADVANCE='NO')
994                         FORMAT ('@RUN EXCODE      TRNO RN CR EDAPS EDAPM')
                            DO L = 1,KEYSTX
                                IF (KEYPS(L).GT.0) THEN
                                    IF (PSABVO(KEYPS(L))(1:1).NE.' ') WRITE (FNUMEVAL,'(A1)',ADVANCE='NO') ' '
                                    WRITE (FNUMEVAL,'(A5,A1)',ADVANCE='NO') PSABVO(KEYPS(L)),'S'
                                    IF (PSABVO(KEYPS(L))(1:1).NE.' ') WRITE (FNUMEVAL,'(A1)',ADVANCE='NO') ' '
                                    WRITE (FNUMEVAL,'(A5,A1)',ADVANCE='NO') PSABVO(KEYPS(L)),'M'
                                ENDIF 
                            ENDDO
                            WRITE (FNUMEVAL,9942)
9942                        FORMAT (' HWAMS HWAMM HWUMS HWUMM',' H#AMS H#AMM H#GMS H#GMM',' LAIXS LAIXM L#SMS L#SMM', &
                                ' CWAMS CWAMM VWAMS VWAMM',' HIAMS HIAMM HN%MS HN%MM VN%MS VN%MM', &
                                ' CNAMS CNAMM HNAMS HNAMM HINMS HINMM')
                            CLOSE(FNUMEVAL)
                        ENDIF  
                    ENDIF  ! End Evaluate header writes
                    IF (EXCODE.NE.EXCODEPREV) EVALOUT = 0
                    EVALOUT = EVALOUT + 1
                    OPEN (UNIT = FNUMEVAL,FILE = FNAMEEVAL,POSITION = 'APPEND')
                    WRITE (FNUMEVAL,8404,ADVANCE='NO') RUN,EXCODE,TN,RN,CROP,edap,edapm
8404                FORMAT (I4,1X,A10,I6,I3,1X,A2,2I6)
                    DO L = 1,KEYSTX
                        IF (KEYPS(L).GT.0) THEN 
                            IF (PSABVO(KEYPS(L))(1:1).NE.' ') WRITE (FNUMEVAL,'(A1)',ADVANCE='NO') ' '
                            WRITE (FNUMEVAL,'(I6)',ADVANCE='NO') PSDAP(KEYPS(L))
                            IF (PSABVO(KEYPS(L))(1:1).NE.' ') WRITE (FNUMEVAL,'(A1)',ADVANCE='NO') ' '
                            WRITE (FNUMEVAL,'(I6)',ADVANCE='NO') PSDAPM(KEYPS(L))
                        ENDIF
                    ENDDO
                    WRITE (FNUMEVAL,8406)NINT(hwam),NINT(hwamm),hwumchar,hwummchar,NINT(hnumam),NINT(hnumamm),hnumgm,hnumgmm, &
                        laix,laixm,lnumsm,lnumsmm,NINT(cwam),NINT(cwamm),NINT(vwam),NINT(vwamm),hiamchar,hiammchar,hnpcmchar, &
                        hnpcmmchar,vnpcmchar,vnpcmmchar,NINT(cnam),NINT(cnamm),NINT(hnam),NINT(hnamm),hinmchar,hinmmchar
8406                    FORMAT (I6,I6, A6,A6,I6,I6,F6.1,F6.1,F6.1,F6.1,F6.1,F6.1,I6,I6,I6,I6,A6,A6,A6,A6,A6,A6,I6,I6,I6,I6,A6,A6)
                        Close(FNUMEVAL)
                        ! End of Evaluation.Out writes
                        
                        ! Overview
                        IF (IDETO.NE.'E') THEN  ! No Overview if only need Evaluate
                            IF (FILEIOT(1:2).EQ.'DS') THEN  ! Overview headers for CSM
                                IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
                                    OPEN (UNIT = FNUMOV, FILE = FNAMEOV)
                                    WRITE(FNUMOV,'("*SIMULATION OVERVIEW FILE")')
                                ELSE
                                    INQUIRE (FILE = FNAMEOV, EXIST = FEXIST)
                                    IF (FEXIST) THEN
                                        OPEN (UNIT = FNUMOV, FILE = FNAMEOV, POSITION = 'APPEND')
                                    ELSE
                                        OPEN (UNIT = FNUMOV, FILE = FNAMEOV, STATUS = 'NEW')
                                        WRITE(FNUMOV,'("*SIMULATION OVERVIEW FILE")')
                                    ENDIF
                                ENDIF
                                WRITE (FNUMOV,*) ' '
                                CALL HEADER(1, FNUMOV, RUN)
                            ELSE    ! Overview header for standalone Cropsim
                                OPEN (UNIT = FNUMOV, FILE=FNAMEOV, POSITION='APPEND')
                                WRITE (FNUMOV,'(/,A79,/)') OUTHED
                                WRITE (FNUMOV,203) MODEL
203                             FORMAT (' MODEL            ',A8)
                                IF (ISWNIT.EQ.'N') THEN
                                    WRITE (FNUMOV,210) iswwat, iswnit
210                                 FORMAT (' MODEL SWITCHES   ','Water: ',A1,'  Nitrogen: ',A1)
                                ELSE
                                    WRITE (FNUMOV,211) iswwat, iswnit, mesom
211                                 FORMAT (' MODEL SWITCHES   ','Water: ',A1,'  Nitrogen: ',A1,' (OM decay: ',A1,')')
                                ENDIF
                                WRITE (FNUMOV,2031) MODNAME
2031                            FORMAT (' MODULE           ',A8)
                                WRITE (FNUMOV,2032) MEPHO
2032                            FORMAT (' MODULE SWITCHES  ','Photosynthesis: ',A1)
                                ! P=PARU effic,I=P+internal CO2,R=resistances(Monteith)
                                WRITE (FNUMOV,2034) FILENEW
2034                            FORMAT (' FILE             ',A60)
                                WRITE (FNUMOV,204)EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
204                             FORMAT (' EXPERIMENT       ',A8,A1,A2,A2,A47)
                                WRITE (FNUMOV,202) TN, TNAME
202                             FORMAT (' TREATMENT',I3,'     ',A25)
                                WRITE (FNUMOV,207) CROP,VARNO,VRNAME
207                             FORMAT (' GENOTYPE         ',A2,A6,'  ',A16)
                                WRITE(FNUMOV,*) ' '
                                CALL Calendar (plyear,plday,dom,month)
                                WRITE (FNUMOV,208)month,dom,plyeardoy,NINT(pltpop),NINT(rowspc)
208                             FORMAT(' PLANTING         ',A3,I3,I8,2X,I4,' plants/m2 ','in ',I3,' cm rows')
                                CALL CSYR_DOY(EYEARDOY,YEAR,DOY)
                                CALL Calendar(year,doy,dom,month)
                                WRITE(FNUMOV,109) month,dom,eyeardoy                  
109                             FORMAT (' EMERGENCE        ',A3,I3,I8)
                                WRITE(FNUMOV,*) ' '
                                WRITE (FNUMOV,209) tmaxx,tmaxm,tminn,tminm              
209                             FORMAT (' TEMPERATURES C   ','Tmax (max):',F5.1,' (mnth av):',F5.1,' Tmin (min):',F5.1, &
                                    ' (mnth av):',F5.1)
                                IF (ISWNIT.NE.'N') THEN
                                    WRITE(fnumov,2095)cnad+rnad+hnad,hnad,vnad
2095                                FORMAT (' CROP N kg/ha     ','Total:  ',F8.1,'  Product ',F12.1,'  Leaf+stem:  ',F6.1)
                                    WRITE(fnumov,2096)sennal(0),sennas            
2096                                FORMAT ('                  ','Leaf loss: ',F5.1,'  Root loss:  ',F8.1)
                                    WRITE(fnumov,2093)isoiln,amtnit,fsoiln
2093                                FORMAT (' SOIL N kg/ha     ','Initial:',F8.1,'  Applied:',F12.1,'  Final:      ',F6.1)
                                    WRITE(fnumov,2094)tnoxc,tlchc,tominfomc+tominsomc-tnimbsom  
2094                                FORMAT ('                  ','Denitrified:',F4.1,'  Leached:',F12.1,'  Net from OM:',F6.1)
                                    WRITE(fnumov,2099)tnimbsom,tominfomc,tominsomc   
2099                                FORMAT ('                  ','OM Fixation:',F4.1,'  Fresh OM decay:',F5.1,'  SOM decay:',F8.1)
                                    IF (tominsom1.GT.0.0)WRITE(fnumov,2098)NINT(tominsom1c),NINT(tominsom2c),NINT(tominsom3c)
2098                                FORMAT ('                  ','SOM1 decay:',I5,'  SOM2 decay:   ',I6,'  SOM3 decay:',I7)
                                ENDIF  
                                IF (ISWWAT.NE.'N') THEN
                                    WRITE(fnumov,2090)isoilh2o,rainc/10.0,irramtc/10.0
2090                                FORMAT (' SOIL WATER cm    ','Initial: ',F7.1,'  Precipitation: ',F5.1,'  Irrigation: ',F6.1)
                                    WRITE(fnumov,2091)runoffc/10.0,drainc/10.0,fsoilh2o
2091                                FORMAT ('                  ','Runoff: ',F8.1,'  Drainage: ',F10.1,'  Final:    ',F8.1)
                                    WRITE(fnumov,2089)eoc/10.0,eopenc/10.0,eompenc/10.0
2089                                FORMAT (' POTENTIAL ET cm  ','Crop model:',F5.1,'  Penman:    ',F9.1,'  Penman-M:  ',F7.1)
                                    WRITE(fnumov,2097)eoptc/10.0,eoebudc/10.0
2097                                FORMAT ('                  ','Priestley: ',F5.1,'  E.budget:  ',F9.1)
                                ENDIF
                            ENDIF  ! End of Overview header writes
                            WRITE(FNUMOV,9589)
                            WRITE(fnumov,*)' '
                            WRITE(fnumov,'(A11,I4,A3,A60)')' RUN NO.   ',RUN,'  ',ENAME
                            IF (DYNAMIC.EQ.SEASEND) THEN
                                WRITE(fnumov,*)' '
                                WRITE(fnumov,'(A50,A25)')' NB. RUN TERMINATED PREMATURELY (PROBABLY BECAUSE ', &
                                    'OF MISSING WEATHER DATA) '
                            ENDIF
                            WRITE(fnumov,9588)
                            WRITE(fnumov,9600)
                            DO L = 1, PSNUM
                                CALL Csopline(laic,laistg(l))
                                IF (STGYEARDOY(L).LT.9999999.AND.L.NE.10.AND.L.NE.11) THEN
                                    CALL CSYR_DOY(STGYEARDOY(L),YEAR,DOY)
                                    CALL Calendar(year,doy,dom,month)
                                    CNCTMP = 0.0
                                    IF (CWADSTG(L).GT.0.0) CNCTMP = CNADSTG(L)/CWADSTG(L)*100
                                    WRITE (FNUMOV,'(I8,I4,1X,A3,I4,1X,I1,1X,A13,I6,A6,F6.1,I6,F6.2,F6.2,F6.2)')STGYEARDOY(L), &
                                        DOM,MONTH,Dapcalc(stgyeardoy(L),(plyeardoy/1000),plday),l,psname(l),NINT(CWADSTG(L)), &
                                        LAIC,LNUMSTG(L),NINT(CNADSTG(L)),CNCTMP,1.0-WFPPAV(L-1),1.0-NFPPAV(L-1)
                                ENDIF
                            ENDDO
                            ! For harvest at specified date
                            IF (YEARDOYHARF.EQ.YEARDOY) THEN
                                
                                CALL Csopline(laic,lai)
                                CALL CSYR_DOY(YEARDOYHARF,YEAR,DOY)
                                CALL Calendar(year,doy,dom,month)
                                CNCTMP = 0.0
                                IF (CWAD.GT.0.0)CNCTMP = CNAD/CWAD*100
                                WRITE (FNUMOV,'(I8,I4,1X,A3,I4,1X,I1,1X,A13,I6,A6,F6.1,I6,F6.2,F6.2,F6.2)')YEARDOY,DOM,MONTH, &
                                    Dapcalc(yeardoy,(plyeardoy/1000),plday),l,'Harvest      ',NINT(CWAD),LAIC,LNUM,NINT(CNAD), &
                                    CNCTMP,1.0-WFPPAV(MSTG-1),1.0-NFPPAV(MSTG-1)
                            ENDIF 
                            IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
                                WRITE(fnumov,*)' '
                                WRITE(fnumov,*)'BIOMASS  = Above-ground dry weight (Excl.seed;kg/ha)'
                                WRITE(fnumov,*)'LEAF AREA  = Leaf area index (m2/m2)'
                                WRITE(fnumov,*)'LEAF NUMBER  = Leaf number produced on main axis'
                                WRITE(fnumov,*)'CROP N  = Above-ground N (Excl.seed;kg/ha)'
                                WRITE(fnumov,*)'CROP N%  = Above-ground N concentration(%)'
                                WRITE(fnumov,*)'H2O STRESS = ','Photosynthesis stress, prior to stage (0=none,1=max)'
                                WRITE(fnumov,*)'N STRESS = ','Photosynthesis stress, prior to stage (0=none,1=max)'
                            ENDIF
                            WRITE(fnumov,*)' '
                            WRITE (FNUMOV,206)
                            WRITE (FNUMOV,290) MAX(-99,gdap),MAX(-99,gdapm),MAX(-99,edap),MAX(-99,edapm)
                            DO L = 1,KEYSTX
                                IF (KEYPS(L).GT.0) THEN
                                    IF (psdap(keyps(l)).LT.-1) EXIT
                                    WRITE (FNUMOV,291)psname(KEYPS(L)),PSDap(KEYPS(L)),PSDapm(KEYPS(L))
                                ENDIF                 
                            ENDDO
                            ! For harvest at specified date
                            IF (YEARDOYHARF.EQ.YEARDOY) THEN
                                tvi1 = Dapcalc(yeardoy,(plyeardoy/1000),plday)
                                WRITE (FNUMOV,292)tvi1,tvi1
                            ENDIF
                            WRITE (FNUMOV,305)NINT(cwam),NINT(cwamm),MAX(-99,NINT(rwam+sdwam)),NINT(rwamm),NINT(senwacm), &
                                NINT(senwacmm),NINT(hwam),NINT(hwamm),NINT(vwam),NINT(vwamm),hiam,hiamm,NINT(rswam),NINT(rswamm)
                            IF (lwphc+swphc.GT.0.0) WRITE (FNUMOV,3051)NINT(cwahc),NINT(cwahcm)
                            WRITE (FNUMOV,3052)hwumchar,hwummchar,NINT(hnumam),NINT(hnumamm),hnumgm,hnumgmm,laix,laixm,lnumsm, &
                                lnumsmm,nupac,nupacm,cnam,cnamm,rnam,rnamm,sennatc,sennatcm,hnam,hnamm,vnam,vnamm,hinm,hinmm, &
                                hnpcm,hnpcmm,vnpcm,vnpcmm    
290                             FORMAT (6X, 'Germination  (dap)          ',6X,I7,  4X,I7,  /,6X, 'Emergence    (dap)          ', &
                                        6X,I7,  4X,I7    )
291                             FORMAT(6X,A13, '(dap)          '         ,6X,I7  ,4X,I7    )
292                             FORMAT(6X, 'Harvest      (dap)          ',6X,I7  ,4X,I7    )
305                             FORMAT(6X, 'AboveGround (kg dm/ha)      ',6X,I7,  4X,I7,  /,  &
                                       6X, 'Roots+seed residue (kg dm/ha)',5X,I7, 4X,I7,  /,  &
                                       6X, 'Senesced (kg dm/ha)         ',6X,I7,  4X,I7,  /,  &
                                       6X, 'Product (kg dm/ha)          ',6X,I7,  4X,I7,  /,  &
                                       6X, 'AboveGroundVegetative (kg dm/ha)  ',I7,4X,I7,  /, &
                                       6X, 'HarvestIndex (ratio)        ',6X,F7.2,4X,F7.2,/,  &
                                       6X, 'Reserves (kg dm/ha)         ',6X,I7,  4X,I7)
3051                            FORMAT(6X, 'Removed canopy (kg dm/ha)   ',7X,I6,  5X,I6  )
3052                            FORMAT(6X, 'Product unit wt (g dm)      ',7X,A6,  5X,A6,  /, &
                                    6X, 'Product number (/m2)        ',6X,I7,  4X,I7,  /,   &
                                    6X, 'Product number (/shoot)     ',6X,F7.1,4X,F7.1,/,   &
                                    6X, 'Maximum leaf area index     ',6X,F7.1,4X,F7.1,/,   &
                                    6X, 'Final leaf number (one axis)',6X,F7.1,4X,F7.1,/,   &
                                    6X, 'Assimilated N (kg/ha)       ',6X,F7.1,4X,F7.1,/,   &
                                    6X, 'AboveGround N (kg/ha)       ',6X,F7.1,4X,F7.1,/,   &
                                    6X, 'Root N (kg/ha)              ',6X,F7.1,4X,F7.1,/,   &
                                    6X, 'Senesced N (kg/ha)          ',6X,F7.1,4X,F7.1,/,   &
                                    6X, 'Product N (kg/ha)           ',6X,F7.1,4X,F7.1,/,   &
                                    6X, 'AboveGroundVegetative N (kg/ha)  ',F8.1,4X,F7.1,/, &
                                    6X, 'N HarvestIndex (ratio)      ',6X,F7.2,4X,F7.2,/,   &
                                    6X, 'Product N (% dm)            ',6X,F7.1,4X,F7.1,/,   &
                                    6X, 'AboveGroundVegetative N (% dm)    ',F7.1,4X,F7.1)  
                                WRITE(fnumov,500)
                                PFPPAV = -99.0
                                PFGPAV = -99.0
                                DO tvI1 = 1,mstg-2
                                    IF (pdays(tvi1).GT.0) THEN 
                                        WRITE(fnumov,600) psname(tvi1),'-',psname(tvi1+1),pdays(tvI1),tmaxpav(tvI1), &          ! MF31AU14 REPLACED DASH  WITH A LITERAL
                                            tminpav(tvI1),sradpav(tvI1),daylpav(tvI1),rainpc(tvI1),etpc(tvI1),1.-wfppav(tvi1), &
                                            1.0-wfgpav(tvi1), 1.0-nfppav(tvi1), 1.0-nfgpav(tvi1), pfppav(tvi1), pfgpav(tvi1)
                                    ENDIF
                                ENDDO
600                             FORMAT(1X,A10,A3,A10,I5,3F6.1,F7.2,2F7.1,4F7.3,2F7.2)
610                             FORMAT(1X,A10,13X,I5,3F6.1,F7.2,2I7,6F7.3)
                                IF(yeardoyharf.EQ.yeardoy)THEN
                                    WRITE(fnumov,600) psname(mstg-1),'-','Harvest   ', pdays(mstg-1),tmaxpav(mstg-1), &         ! MF31AU14 REPLACED DASH  WITH A LITERAL
                                        tminpav(mstg-1),sradpav(mstg-1),daylpav(mstg-1),rainpc(mstg-1),etpc(mstg-1), &
                                        1.-wfppav(mstg-1),1.0-wfgpav(mstg-1), 1.0-nfppav(mstg-1),1.0-nfgpav(mstg-1), &
                                        pfppav(mstg-1),pfgpav(mstg-1)
                                ELSE 
                                    WRITE(fnumov,600) psname(mstg-1),'-',psname(mstg),pdays(mstg-1),tmaxpav(mstg-1), &           ! MF31AU14 REPLACED DASH  WITH A LITERAL
                                        tminpav(mstg-1),sradpav(mstg-1),daylpav(mstg-1),rainpc(mstg-1),etpc(mstg-1), &
                                        tminpav(mstg-1),sradpav(mstg-1),daylpav(mstg-1),rainpc(mstg-1),etpc(mstg-1), &
                                        1.-wfppav(mstg-1),1.0-wfgpav(mstg-1),1.0-nfppav(mstg-1),1.0-nfgpav(mstg-1), &
                                        pfppav(mstg-1),pfgpav(mstg-1)
                                ENDIF
                                IF (pdays(mstg).GT.0.OR.yeardoyharf.EQ.yeardoy) THEN 
                                    WRITE(fnumov,*) ' '
                                    pfpcav = -99.0
                                    pfgcav = -99.0 
                                    IF (pdays(mstg).GT.0.) THEN 
                                        WRITE(fnumov,600) psname(1),'-',psname(mstg), cdays, tmaxcav, tmincav, sradcav, &        ! MF31AU14 REPLACED DASH  WITH A LITERAL
                                            daylcav, raincc, etcc, 1.0-wfpcav, 1.0-wfgcav, 1.0-nfpcav, 1.0-nfgcav,pfpcav, pfgcav
                                    ELSE  
                                        WRITE(fnumov,600) psname(1),'-','Harvest   ', cdays, tmaxcav, tmincav, sradcav, &        ! MF31AU14 REPLACED DASH  WITH A LITERAL
                                            daylcav, raincc, etcc, 1.0-wfpcav, 1.0-wfgcav, 1.0-nfpcav, 1.0-nfgcav,pfpcav, pfgcav
                                    ENDIF 
                                    ! Resource productivity calculations
                                    DMP_Rain = -99.
                                    GrP_Rain = -99.
                                    DMP_ET = -99.
                                    GrP_ET = -99.
                                    DMP_EP = -99.
                                    GrP_EP = -99.
                                    DMP_Irr = -99.    
                                    GrP_Irr = -99.
                                    DMP_NApp = -99.
                                    GrP_NApp = -99.
                                    DMP_NUpt = -99.
                                    GrP_NUpt = -99.
                                    IF (RAINCC > 1.E-3) THEN
                                        DMP_Rain = CWAM / RAINCC 
                                        GrP_Rain = HWAM  / RAINCC
                                    ENDIF
                                    IF (ETCC > 1.E-3) THEN
                                        DMP_ET = CWAM / ETCC 
                                        GrP_ET = HWAM  / ETCC 
                                    ENDIF
                                    IF (EPCC > 1.E-3) THEN
                                        DMP_EP = CWAM / EPCC 
                                        GrP_EP = HWAM  / EPCC 
                                    ENDIF
                                    IF (IRRAMTC > 1.E-3) THEN
                                        DMP_Irr = CWAM / IRRAMTC 
                                        GrP_Irr = HWAM  / IRRAMTC
                                    ENDIF
                                    IF (ISWNIT.NE.'N') THEN
                                        IF (Amtnit > 1.E-3) THEN
                                            DMP_NApp = CWAM / Amtnit
                                            GrP_NApp = HWAM  / Amtnit
                                        ENDIF
                                        IF (NUPAC > 1.E-3) THEN
                                            DMP_NUpt = CWAM / NUPAC
                                            GrP_NUpt = HWAM  / NUPAC
                                        ENDIF
                                    ENDIF ! ISWNIT NE 'N'
                                    WRITE (FNUMOV, 1200) CDAYS,RAINCC, DMP_Rain*0.1, DMP_Rain, GrP_Rain*0.1, GrP_Rain, &
                                        ETCC,DMP_ET*0.1,DMP_ET,GrP_ET*0.1,GrP_ET, EPCC,DMP_EP*0.1,DMP_EP,GrP_EP*0.1,GrP_EP
                                    IF (IRRAMTC > 1.E-3) THEN
                                        WRITE(FNUMOV, 1210) IRRAMTC, DMP_Irr*0.1, DMP_Irr, GrP_Irr*0.1, GrP_Irr
                                    ENDIF  
                                    IF (ISWNIT.NE.'N') THEN
                                        IF (Amtnit > 1.E-3) THEN
                                            WRITE(FNUMOV, 1220) Amtnit, DMP_NApp, GrP_NApp 
                                        ENDIF
                                        IF (NUPAC > 1.E-3) THEN
                                            WRITE(FNUMOV, 1230) NUPAC, DMP_NUpt,GrP_NUpt 
                                        ENDIF
                                    ENDIF ! ISWNIT NE 'N'
                                    WRITE(FNUMOV,270)
                                    WRITE(FNUMOV,300) 'CASSAVA', NINT(HWAM)
                                    WRITE(FNUMOV,'(110("*"))')
                                    CLOSE(FNUMOV)  ! Overview.out
                                ENDIF 
                                ! Basic info.to Work.out when calling for Overview
                                !  Summary of various environmental aspects
                                WRITE(fnumwrk,*) ' '
                                WRITE(fnumwrk,'(A28,A10,I3)')' OVERVIEW OF CONDITIONS FOR ',excode,tn
                                WRITE(fnumwrk,*) ' '
                                WRITE (fnumwrk,209) tmaxx,tmaxm,tminn,tminm              
                                IF (ISWNIT.NE.'N') THEN
                                    WRITE(fnumwrk,2095)cnad+rnad+hnad,hnad,vnad
                                    WRITE(fnumwrk,2096)sennal(0),sennas            
                                    WRITE(fnumwrk,2093)isoiln,amtnit,fsoiln
                                    WRITE(fnumwrk,2094)tnoxc,tlchc,tominsomc+tominfomc-tnimbsom
                                    WRITE(fnumwrk,2099)tnimbsom,tominfomc,tominsomc   
                                    IF (tominsom1.GT.0.0) WRITE(fnumwrk,2098)NINT(tominsom1c),NINT(tominsom2c),NINT(tominsom3c)
                                    !IF (FILEIOT.EQ.'DS4'.AND.IDETL.EQ.'D'.OR.FILEIOT.EQ.'DS4'.AND.IDETL.EQ.'A'.OR.FILEIOT &
                                    !    .NE.'DS4') THEN
                                    IF (FILEIOT.EQ.'DS4'.AND.IDETL.EQ.'D'.OR.FILEIOT.EQ.'DS4'.AND.IDETL.EQ.'A'.OR. &
                                        FILEIOT.NE.'DS4') THEN
                                        WRITE(fnumwrk,2090)isoilh2o,rainc/10.0,irramtc/10.0
                                        WRITE(fnumwrk,2091)runoffc/10.0,drainc/10.0,fsoilh2o
                                        WRITE(fnumwrk,2089)eoc/10.0,eopenc/10.0,eompenc/10.0
                                        WRITE(fnumwrk,2097)eoptc/10.0,eoebudc/10.0
                                    ENDIF
                                        IF (FAPPNUM.GT.0) THEN
                                            WRITE (fnumwrk,*) ' '
                                            WRITE (fnumwrk,'(A18,A10,I3)')' N FERTILIZER FOR ',excode,tn
                                            DO L = 1,FAPPNUM
                                                WRITE (fnumwrk,'(A80)') FAPPLINE(L)
                                            ENDDO
                                        ENDIF
                                        WRITE(FNUMWRK,*) ' '
                                        WRITE(FNUMWRK,'(A45)')' INORGANIC N (kg/ha) LEFT IN SOIL AT HARVEST '
                                        WRITE(FNUMWRK,'(A28,2F6.1)')'  NO3 and NH4 N in PROFILE: ',SNO3PROFILE,SNH4PROFILE
                                        WRITE(FNUMWRK,'(A28,2F6.1)')'  NO3 and NH4 N in ROOTZONE:',SNO3ROOTZONE,SNH4ROOTZONE
                                ENDIF   ! End Iswnit NE N
                                WRITE(FNUMWRK,*) ' '
                                WRITE(FNUMWRK,'(A34)')' H2O (mm) LEFT IN SOIL AT HARVEST '
                                WRITE(FNUMWRK,'(A36,2F6.1)')'  H2O and AVAILABLE H2O in PROFILE: ',H2OPROFILE,AH2OPROFILE
                                WRITE(FNUMWRK,'(A36,2F6.1)')'  H2O and AVAILABLE H2O in ROOTZONE:',H2OROOTZONE,AH2OROOTZONE
                                WRITE (fnumwrk,*) ' '
                                WRITE (fnumwrk,'(A32,A10,I3)')' CRITICAL PERIOD CONDITIONS FOR ',excode,tn
                                WRITE (fnumwrk,'(A38,F6.1)')'  Temperature mean,germination         ',TMEANG
                                WRITE (fnumwrk,'(A38,F6.1)')'  Temperature mean,germ-emergence      ',TMEANE
                                WRITE (fnumwrk,'(A38,F6.1)')'  Temperature mean,first 20 days       ',TMEAN20P
                        ELSE   ! For Overview
                            
                            OPEN (UNIT=FNUMOV, FILE=FNAMEOV, STATUS = 'UNKNOWN')
                            CLOSE (UNIT=FNUMOV, STATUS = 'DELETE')
                            
                        ENDIF  ! For Overview  (IDETO.NE.'E')                    
                        
                ELSE ! For Evaluate,Overview  IDETL.EQ.'0'.OR.IDETO.NE.'N'
                    
                    OPEN (UNIT=FNUMMEAS, FILE=FNAMEMEAS, STATUS = 'UNKNOWN')
                    CLOSE (UNIT=FNUMMEAS, STATUS = 'DELETE')
                    OPEN (UNIT=FNUMEVAL, FILE=FNAMEEVAL, STATUS = 'UNKNOWN')
                    CLOSE (UNIT=FNUMEVAL, STATUS = 'DELETE')
                    OPEN (UNIT=FNUMOV, FILE=FNAMEOV, STATUS = 'UNKNOWN')
                    CLOSE (UNIT=FNUMOV, STATUS = 'DELETE')
                    
                ENDIF  ! End Ideto outputs (Evaluate,Overview)'
                
                !-----------------------------------------------------------------------------------------------------------
                !         IDETS OUTPUTS (Plantsum)
                !-----------------------------------------------------------------------------------------------------------
                
                IF ((IDETS.NE.'N'.AND.IDETL.NE.'0').OR.IDETL.EQ.'A') THEN
                    
                    ! PLANT SUMMARY (SIMULATED)'
                    IF (CROP.NE.CROPPREV.OR.RUN.EQ.1) THEN
                        OPEN (UNIT=fnumpsum,FILE=FNAMEPSUM,POSITION='APPEND')
                        WRITE (FNUMPSUM,9953)
9953                    FORMAT (/,'*SUMMARY')
                        WRITE (FNUMPSUM,99,ADVANCE='NO')
99                      FORMAT ('@  RUN EXCODE    TRNO RN',' TNAME....................',' REP  RUNI S O C    CR PYEAR  PDAT')
                        DO L = 1,KEYSTX
                            IF (KEYPS(L).GT.0) THEN
                                WRITE (FNUMPSUM,'(A6)',ADVANCE='NO') PSABVO(KEYPS(L))
                                !IF (PSABVO(KEYPS(L)).EQ.'TSDAP') THEN
                                ! WRITE (FNUMPSUM,'(A6)',ADVANCE='NO') '  DAYL'
                                !ENDIF
                            ENDIF
                        ENDDO
                        WRITE (FNUMPSUM,299)
299                     FORMAT ('   FLN FLDAP HYEAR  HDAY SDWAP',' CWAHC  CWAM PARUE  HWAM  HWAH  VWAM  HWUM  H#AM  H#UM', &
                            ' SDNAP  CNAM  HNAM  RNAM  TNAM  NUCM  HN%M  VN%M',' D1INI D2INI D3INI ')
                        CLOSE(fnumpsum)  
                    ENDIF  ! End of Plantsum.Out headers
                    OPEN (UNIT=fnumpsum,FILE=FNAMEPSUM,POSITION='APPEND')
                    WRITE (fnumpsum,400,ADVANCE='NO') run,excode,tn,rn,tname,rep,runi,sn,on,cn,crop,plyear,plday
400                 FORMAT (I6,1X,A10,I4,I3,1X,A25,I4,I6,I2,I2,I2,4X,A2,I6,I6)
                    DO L = 1,KEYSTX
                        IF (KEYPS(L).GT.0) THEN
                            WRITE (FNUMPSUM,'(I6)',ADVANCE='NO') PSDAP(KEYPS(L))
                            IF (PSABVO(KEYPS(L)).EQ.'TSDAP') THEN
                                WRITE (FNUMPSUM,'(F6.1)',ADVANCE='NO') DAYLST(L)
                            ENDIF  
                        ENDIF
                    ENDDO
                    WRITE (fnumpsum,401)FLN, FLDAP,hayear,hadoy,NINT(sdrate),NINT(cwahc),NINT(cwam),pariued,NINT(hwam), &
                        NINT(hwam*hpcf/100.0),NINT(vwam),hwumchar,NINT(hnumam),NINT(hnumgm),sdnap,NINT(cnam),NINT(hnam), &
                        NINT(rnam),NINT(AMAX1(-99.0,cnam+rnam)),NINT(nupac),hnpcmchar,vnpcmchar,didoy(1),didoy(2),didoy(3)
401                     FORMAT (F6.1,I6,I6,I6,I6,I6,I6,F6.1,I6,I6,I6,A6,I6,I6,F6.1,I6,I6,I6,I6,I6,2A6,3I6)
                        CLOSE(fnumpsum)  
                ELSE  
                    OPEN (UNIT=FNUMPSUM,FILE=FNAMEPSUM,STATUS='UNKNOWN')
                    CLOSE (UNIT=FNUMPSUM, STATUS = 'DELETE')
                ENDIF
                ! End IDETS Outputs (Plantsum.Out)          
                
                !-----------------------------------------------------------------------------------------------------------
                !         IDETL = Y or D OUTPUTS (Leaves,Tiers)
                !-----------------------------------------------------------------------------------------------------------
                
                IF (IDETL.EQ.'Y'.OR.IDETL.EQ.'D'.OR.IDETL.EQ.'A') THEN
                    
                    ! LEAVES.OUT
                    OPEN(UNIT=FNUMLVS,FILE=FNAMELEAVES,POSITION='APPEND')
                    WRITE (FNUMLVS,'(/,A79,/)') OUTHED
                    WRITE (FNUMLVS,'(A14,F6.1)') '! LEAF NUMBER ',LNUM
                    WRITE (FNUMLVS,'(/,A48,A36,A30)')'@ LNUM AREAP AREA1 AREA2 AREA3 AREA4 AREAT AREAS', &
                        '  WFLF  NFLF NFLF2  AFLF TFGLF TFDLF',' LLIFG LLIFA LLIFS LLIFE   DAP'
                    DO I = 1, INT(LNUM+0.99)
                        CALL Csopline(lapotxc,lapotx(i))
                        CALL Csopline(latlc,AMAX1(0.0,latl(1,i)))
                        CALL Csopline(latl2c,AMAX1(0.0,latl2(1,i)))
                        CALL Csopline(latl3c,AMAX1(0.0,latl3(1,i)))
                        CALL Csopline(latl4c,AMAX1(0.0,latl4(1,i)))
                        CALL Csopline(lapc,lap(i))
                        CALL Csopline(lapsc,laps(i))
                        ! Adjust for growth period of non fullly expanded leaves
                        WFLF(I) = AMIN1(1.0,WFLF(I)/AMIN1(1.0,(LAGETT(I)/LLIFG)))
                        NFLF(I) = AMIN1(1.0,NFLF(I)/AMIN1(1.0,(LAGETT(I)/LLIFG)))
                        NFLFP(I) =AMIN1(1.0,NFLFP(I)/AMIN1(1.0,(LAGETT(I)/LLIFG)))
                        TFGLF(I) =AMIN1(1.0,TFGLF(I)/AMIN1(1.0,(LAGETT(I)/LLIFG)))
                        AFLF(I) = AMIN1(1.0,AFLF(I)/AMIN1(1.0,(LAGETT(I)/LLIFG)))
                        IF (LDEATHDAP(I).EQ.0) LDEATHDAP = -99
                        WRITE (fnumlvs,'(I6,7A6,6F6.2,4F6.1,I6)')I,LAPOTXC,LATLC,LATL2C,LATL3C,LATL4C,LAPC,LAPSC,1.0-WFLF(I), &
                            1.0-NFLF(I),1.0-NFLF2(I),1.0-AMAX1(0.0,AMIN1(1.0,AFLF(I))),1.0-TFGLF(I),1.0-TFDLF(I),DGLF(I), &
                            DALF(I),DSLF(I),DGLF(I)+DALF(I)+DSLF(I),LDEATHDAP(I)
                    ENDDO
                    IF (RUN.EQ.1) THEN
                        WRITE(fnumlvs,*)' '
                        WRITE(fnumlvs,'(A43)')'! NB. Data are summed over all fork branches'
                        WRITE(fnumlvs,*)' '
                        WRITE(fnumlvs,'(A36)')'! LNUM = Number of leaf on one axis '
                        WRITE(fnumlvs,'(A52)')'! AREAP = Potential area of leaf on main axis (cm2) '
                        WRITE(fnumlvs,'(A41,A26)')'! AREA1 = Area of youngest mature leaf on', ' main axis,no stress (cm2)'
                        WRITE(fnumlvs,'(A42,A16)')'! AREAT = Area of cohort of leaves at leaf',' position (cm2) '
                        WRITE(fnumlvs,'(A43,A35)')'! AREAS = Senesced area of cohort of leaves', &
                            ' at harvest at leaf position (cm2) '
                        WRITE(fnumlvs,'(A38,A17)')'! WFLF  = Water stress factor for leaf',' (0-1,1=0 stress)'
                        WRITE(fnumlvs,'(A51)')'! NFLF  = N stress factor for leaf (0-1,1=0 stress)'
                        WRITE(fnumlvs,'(A51)')'! NFLF  = N factor for area adjustment (0-1,1=0 stress)'
                        WRITE(fnumlvs,'(A44,A17)')'! NFLFP = N stress factor for photosynthesis',' (0-1,1=0 stress)'
                        WRITE(fnumlvs,'(A36,A24)')'! AFLF  = Assimilate factor for leaf',' (0-1,1=0 no limitation)'
                        WRITE(fnumlvs,'(A48,A24)')'! TFGLF = Temperature factor for leaf expansion ',' (0-1,1=0 no limitation)'
                        WRITE(fnumlvs,'(A49,A24)')'! TFDLF = Temperature factor for leaf development',' (0-1,1=0 no limitation)'
                        WRITE(fnumlvs,'(A37)')'! DGLF = Number of days growing      '
                        WRITE(fnumlvs,'(A37)')'! DALF = Number of days fully active '
                        WRITE(fnumlvs,'(A37)')'! DSLF = Number of days senescing    '
                    ENDIF
                    CLOSE (FNUMLVS)
                    ! End of Leaves.out
                    
                    ! Branching tier conditions (Simulated;TIERS.OUT)
                    OPEN(UNIT=FNUMPHA,FILE=FNAMEPHASES,POSITION='APPEND')
                    WRITE (FNUMPHA,'(/,A79,/)') OUTHED
                    WRITE (fnumpha,'(A42,A24,A12)')'@ TIER SRADA  TMXA  TMNA  PREA  TWLA  CO2A','  WFPA  WFGA  NFPA  NFGA', &
                        '  TIER_END  '
                    DO L=1,MSTG-2
                        IF (STGYEARDOY(L).LT.9999999.AND.L.NE.0.AND.L.NE.10.AND.L.NE.11) &
                            WRITE (fnumpha,'(I6,3F6.1,2F6.2,I6,4F6.2,1X,A13)')L,sradpav(L),tmaxpav(L),tminpav(L),rainpav(L), &
                            daylpav(L),NINT(co2pav(L)),1.0-wfppav(L),1.0-wfgpav(L),1.0-nfppav(L),1.0-nfgpav(L), &
                            psname(MIN(L+1,PSX))
                    ENDDO
                    IF(yeardoyharf.EQ.yeardoy)THEN
                        WRITE (fnumpha,'(I6,3F6.1,2F6.2,I6,4F6.2,1X,A13)')mstg-1,sradpav(mstg-1),tmaxpav(mstg-1), &
                            tminpav(mstg-1),rainpav(mstg-1),daylpav(mstg-1),NINT(co2pav(mstg-1)),1.0-wfppav(mstg-1), &
                            1.0-wfgpav(mstg-1),1.0-nfppav(mstg-1),1.0-nfgpav(mstg-1),'Harvest      '
                    ELSE 
                        WRITE (fnumpha,'(I6,3F6.1,2F6.2,I6,4F6.2,1X,A13)')mstg-1,sradpav(mstg-1),tmaxpav(mstg-1), &
                            tminpav(mstg-1),rainpav(mstg-1),daylpav(mstg-1),NINT(co2pav(mstg-1)),1.0-wfppav(mstg-1), &
                            1.0-wfgpav(mstg-1),1.0-nfppav(mstg-1),1.0-nfgpav(mstg-1),psname(mstg)
                    ENDIF
                    CLOSE (FNUMPHA)
                    
                ELSE
                    
                    OPEN (UNIT=FNUMLVS,FILE=FNAMELEAVES,STATUS='UNKNOWN')
                    CLOSE (UNIT=FNUMLVS, STATUS = 'DELETE')
                    OPEN (UNIT=FNUMPHA,FILE=FNAMEPHASES,STATUS = 'UNKNOWN')
                    CLOSE (UNIT=FNUMPHA, STATUS = 'DELETE')
                ENDIF
                ! End of Leaves and Tier writes
                
                ! If have not read measured data cannot produce A summaries
                IF (IDETL.EQ.'D'.AND.IDETO.EQ.'N') THEN
                    WRITE(Message(1),'(A35)')'IDETL flag called for detail files.'
                    WRITE(Message(2),'(A31,A31)')'But IDETO flag set at N so that','measured data not read.        '
                    WRITE(Message(3),'(A45)')'Therefore,could not write detailed summaries.'
                    CALL WARNING(3,'CSCAS',MESSAGE)
                ENDIF
                
                !-----------------------------------------------------------------------------------------------------------
                !         IDETL = D OUTPUTS (Work details;Phenols,m;Plantres,m)
                !-----------------------------------------------------------------------------------------------------------
                
                IF ((IDETL.EQ.'D'.AND.IDETO.NE.'N').OR.IDETL.EQ.'A') THEN
                    
                    ! WORK
                    WRITE(fnumwrk,*) ' '
                    WRITE(fnumwrk,'(A26,A10,I3)')' HARVEST/FAILURE DATA FOR ',excode,tn
                    WRITE(fnumwrk,*)' '
                    IF (DYNAMIC.EQ.SEASEND .AND. SEASENDOUT.NE.'Y') THEN
                        WRITE(fnumwrk,*)  ' Program terminated      ',YEARDOY
                    ELSE 
                        WRITE(fnumwrk,*)  ' Harvest reached         ',YEARDOY
                    ENDIF  
                    WRITE (fnumwrk,*)' '
                    WRITE (fnumwrk,'(A54,F5.1,F4.1)')'  Overall PAR use efficientcy(incident,intercepted) = ',paruec,pariued
                    WRITE(fnumwrk,*) ' '
                    WRITE(fnumwrk,'(A27,F11.2)')'  Harvest product (kg/ha)  ',HWAM
                    WRITE(fnumwrk,'(A27,F11.2)')'  Product/Total wt (HI)    ',HIAM
                    WRITE(fnumwrk,*) ' '
                    WRITE(fnumwrk,'(A26,A10,I3)')' CH2O BALANCE (kg/ha) FOR ',excode,tn
                    WRITE(fnumwrk,'(A27,3F11.1)')'  SEED+FIXED (1) Seed,fixed',(SEEDRSI+SDCOAT+CARBOC)*PLTPOP*10.0, &
                        (SEEDRSI+SDCOAT)*PLTPOP*10.0,CARBOC*PLTPOP*10.0
                    TVR1 = (SEEDRSI+SDCOAT+CARBOC)*PLTPOP*10.0
                    WRITE(fnumwrk,'(A27,3F11.1)')'  RESPIRED (2)  Tops,root  ',RESPC*PLTPOP*10.0,RESPTC*PLTPOP*10.0, &
                        RESPRC*PLTPOP*10.0 
                    TVR2 = RESPC*PLTPOP*10.0
                    WRITE(fnumwrk,'(A27,3F11.1)')'  SENESCED (3)  Tops,root  ',(SENTOPLITTER+SENROOT)*PLTPOP*10.0, &
                        SENTOPLITTER*PLTPOP*10.0,SENROOT*PLTPOP*10.0
                    TVR3 = (SENTOPLITTER+SENROOT)*PLTPOP*10.0
                    TVR4 = (SEEDRS+SDCOAT+RTWT+SRWT+LFWT+STWT+CRWT+RSWT)*PLTPOP*10.0
                    WRITE(fnumwrk,'(A27,3F11.1)')'  PLANT+SEED_RESIDUE Pl,sd ',(SEEDRS+SDCOAT+RTWT+SRWT+LFWT+STWT+CRWT+RSWT) &
                        *PLTPOP*10.0,(RTWT+SRWT+LFWT+STWT+CRWT+RSWT)*PLTPOP*10.0,(SEEDRS+SDCOAT)*PLTPOP*10.0
                    WRITE(fnumwrk,'(A27,2F11.1)')'  RESERVES (5)             ',RSWT*PLTPOP*10.0 
                    TVR5 = RSWT*PLTPOP*10.0
                    WRITE(fnumwrk,'(A29, F9.1)')'  HARVESTED DURING CYCLE (6) ',(LWPHC+SWPHC+RSWPHC)*PLTPOP*10.0
                    TVR6 = (LWPHC+SWPHC+RSWPHC)*PLTPOP*10.0
                    WRITE(fnumwrk,'(A27, F11.2)')'  BALANCE (1-(2+3+4+6))    ',TVR1 -(TVR2+TVR3+TVR4+TVR6)
                    IF (ABS(TVR1-(TVR2+TVR3+TVR4+TVR6)).GT.0.05)WRITE(fnumwrk,'(A29,A10,A1,I2)') &
                        '  *PROBLEM WITH CH2O BALANCE ',EXCODE,' ',TN
                    
                    WRITE (fnumwrk,*) ' '
                    WRITE (fnumwrk,'(A21,A10,I3)')' RESERVES STATUS FOR ',excode,tn
                    WRITE (fnumwrk,'(A22,F7.1)')'  Kg/ha at maximum    ',RSWTX*PLTPOP*10.0
                    WRITE (fnumwrk,'(A22,F7.1)')'  % above ground      ',RSCX*100.
                    WRITE (fnumwrk,'(A22,F7.1)')'  Kg/ha at harvest    ',RSWAD
                    IF (lfwt+stwt+crwt+rswt.GT.0) WRITE (fnumwrk,'(A22,F7.1)')'  % above ground      ', &
                        rswt/(lfwt+stwt+crwt+rswt)*100.0
                    WRITE (fnumwrk,*) ' '
                    WRITE (fnumwrk,'(A34,A10,I3)')' SEED USE (KG/HA or PER CENT) FOR ',excode,tn
                    WRITE (fnumwrk,'(A22,F7.3)')'  Initial reserves    ',seedrsi*pltpop*10.0
                    WRITE (fnumwrk,'(A22,F7.3)')'  Use for tops        ',seeduset*pltpop*10.0
                    WRITE (fnumwrk,'(A22,F7.3)')'  Use for roots       ',seeduser*pltpop*10.0
                    WRITE (fnumwrk,'(A22,F7.3)')'  Total use           ',(seeduset+seeduser)*pltpop*10.0
                    IF (seeduser+seeduset.GT.0.0)WRITE (fnumwrk,'(A22,F7.3)')'  Percent to tops     ', &
                        seeduset/(seeduset+seeduser)*100.0
                    WRITE(fnumwrk,*)' '
                    WRITE (fnumwrk,'(A35,A10,I3)')' DEAD MATTER AND ROOTS (KG/HA) FOR ',excode,tn
                    WRITE(fnumwrk,'(A32,F8.1)')'  DEAD MATERIAL LEFT ON SURFACE  ',SENTOPLITTERA
                    WRITE(fnumwrk,'(A32,F8.1)')'  DEAD MATERIAL LEFT IN SOIL     ',SENROOTA
                    WRITE(fnumwrk,'(A32,F8.1)')'  ROOT WEIGHT AT HARVEST/FAILURE ',RWAD
                    WRITE (fnumwrk,*) ' '
                    WRITE (fnumwrk,'(A20,A10,I3)')' ROOTS BY LAYER FOR ',excode,tn
                    WRITE (fnumwrk,'(A19)')'  LAYER  RTWT   RLV'
                    DO L=1,NLAYR
                        IF (RTWTAL(L).GT.0.0) WRITE (fnumwrk,'(I6,F7.1,F6.2)')L,RTWTAL(L),RLV(L)
                    ENDDO
                    IF (RTSLXDATE.GT.0) THEN
                        WRITE(fnumwrk,'(A30,I7)')'  FINAL SOIL LAYER REACHED ON ',RTSLXDATE
                        WRITE(fnumwrk,'(A15,I7,A1)')'  (MATURITY ON ',YEARDOY,')'
                    ELSE
                        WRITE(fnumwrk,*)' FINAL SOIL LAYER NOT REACHED '
                    ENDIF
                    WRITE (fnumwrk,*) ' '
                    WRITE (fnumwrk,'(A40)')' PRINCIPAL AND SECONDARY STAGES         '
                    WRITE (fnumwrk,'(A40)')'  STAGE NAME   DAYS > PLANTING  LEAF #  '
                    WRITE (fnumwrk,'(A15,F7.1)')'   Germination ',gdapfr
                    WRITE (fnumwrk,'(A15,F7.1)')'   Emergence   ',edapfr
                    DO L = 2,PSNUM
                        CALL CSUCASE (PSNAME(L))
                        IF (PSNAME(L)(1:3).EQ.'HAR'.AND.PSDAPFR(l).LE.0.0) psdapfr(l) = psdapfr(mstg)
                        IF (PSNAME(L)(1:3).EQ.'END'.AND.PSDAPFR(l).LE.0.0) psdapfr(l) = psdapfr(mstg)
                        IF (PSNAME(L)(1:3).NE.'FAI') THEN
                            IF (psdapfr(l).GT.0) WRITE (FNUMWRK,'(A3,A13,F6.1,9X,F6.1)')'   ',psname(l),psdapfr(l),lnumsimtostg(l)
                        ELSE
                            IF (CFLFAIL.EQ.'Y'.AND.psdapfr(l).GT.0)WRITE (FNUMWRK,'(A3,A13,F6.1)')'   ',psname(l),psdapfr(l)
                        ENDIF
                        IF (TVILENT(PSNAME(L)).LT.5) EXIT
                    ENDDO
                    WRITE (fnumwrk,*) ' '
                    WRITE (fnumwrk,'(A28,A10,I3)')' STRESS FACTOR AVERAGES FOR ',excode,tn
                    WRITE (fnumwrk,'(A55)')'  TIER   H2O(PS)   H2O(GR)   N(PS)     N(GR)   TIER_END'
                    DO L=1,MSTG-2 
                        WRITE (fnumwrk,'(I6,F8.2,3F10.2,2X,A13)')l,1.0-wfppav(l),1.0-wfgpav(l),1.0-nfppav(l),1.0-nfgpav(l), &
                            psname(MIN(L+1,PSX))
                    ENDDO
                    IF(yeardoyharf.EQ.yeardoy)THEN
                        WRITE (fnumwrk,'(I6,F8.2,3F10.2,2X,A13)')l,1.0-wfppav(l),1.0-wfgpav(l),1.0-nfppav(l),1.0-nfgpav(l), &
                            'HARVEST      '
                    ELSE 
                        WRITE (fnumwrk,'(I6,F8.2,3F10.2,2X,A13)')l,1.0-wfppav(mstg-1),1.0-wfgpav(mstg-1),1.0-nfppav(mstg-1), &
                            1.0-nfgpav(mstg-1),psname(mstg)
                    ENDIF
                    WRITE (fnumwrk,'(A42)')'  NB 0.0 = minimum ; 1.0 = maximum stress.'
                    ! LAH  Must change from daily leaf cohorts to bigger unit
                    ! Too much to output if daily
                    !WRITE (fnumwrk,*)' '
                    !WRITE (fnumwrk,'(A23)') ' COHORT   AREA    AREAS'
                    !DO L = 1, LCNUM
                    !  WRITE (fnumwrk,'(I7,2F8.3)') L,LCOA(L),LCOAS(L)
                    !ENDDO
                    WRITE (fnumwrk,*) ' '
                    WRITE (fnumwrk,'(A24,A10,I3)')' SHOOT SIZES (cm2) FOR  ',excode,tn
                    WRITE (fnumwrk,'(A32)') '   SHOOT  BIRTH   AREAP   AREAS '
                    DO I = 1,INT(SHNUM)
                        WRITE (fnumwrk,'(I7,I8,2I8)')I,SHDAT,NINT(SHLA(I)),NINT(SHLAS(I))
                    ENDDO
                    IF (ISWNIT.NE.'N') THEN
                        WRITE (fnumwrk,*) ' '
                        WRITE (fnumwrk,'(A25,A10,I3)')' N BALANCE (kg/ha) FOR ',excode,tn
                        WRITE (fnumwrk,'(A34,F8.2,2F11.2)')'   N UPTAKE + SEED (1)            ', (NUPC+SEEDNI)*PLTPOP*10.0, &
                            NUPC*PLTPOP*10.,SEEDNI*PLTPOP*10.
                        TVR1 = (NUPC+SEEDNI)*PLTPOP*10.0  
                        WRITE (fnumwrk,'(A33,F9.2,2F11.2)')'   TOTAL N SENESCED (2) Tops,Root',(SENNL(0)+SENNS)*PLTPOP*10.0, &
                            SENNL(0)*PLTPOP*10.0,SENNS*PLTPOP*10.0
                        TVR2 = (SENNL(0)+SENNS)*PLTPOP*10.0 
                        WRITE (fnumwrk,'(A34,F8.2)')'   TOTAL N IN PLANT (3)           ', &
                            PLTPOP*10.0*(ROOTN+SROOTN+LEAFN+STEMN+RSN+SEEDN)
                        TVR3 = (ROOTN+SROOTN+LEAFN+STEMN+RSN+SEEDN)*PLTPOP*10.0         
                        WRITE (fnumwrk,'(A33, F9.2)')'   HARVESTED DURING CYCLE (4)    ',PLTPOP*10.0*LNPHC+SNPHC+RSNPHC
                        TVR4 = (LNPHC+SNPHC+RSNPHC)* PLTPOP*10.0
                        WRITE (fnumwrk,'(A34,F8.3)')'   BALANCE (1-(2+3+4))            ',TVR1-TVR2-TVR3-TVR4
                        IF (ABS(TVR1-(TVR2+TVR3+TVR4)).GT.0.005)WRITE(fnumwrk,'(A26,A10,A1,I2)')'  *PROBLEM WITH N BALANCE ' &
                            ,EXCODE,' ',TN
                    ENDIF
                    ! End of Detailed WORK writes
                    
                    ! Phenology (Simulated;PHENOLS.OUT)
                    INQUIRE (FILE = FNAMEPHENOLS,EXIST = FFLAG)
                    OPEN(UNIT=FNUMPHES,FILE=FNAMEPHENOLS,POSITION='APPEND')
                    IF (CROP.NE.CROPPREV.OR.RUN.EQ.1.OR.(.NOT.(FFLAG))) THEN
                        WRITE (FNUMPHES,'(/,A14,A10)')'*PHENOLOGY(S):',EXCODE
                        WRITE (FNUMPHES,'(A16,A24)',ADVANCE='NO') '@ EXCODE    TRNO',' PYEAR  PDAT  GDAP  EDAP'
                        DO L = 1,KEYSTX
                            IF (KEYPS(L).GT.0)THEN
                                WRITE (FNUMPHES,'(A6)',ADVANCE='NO') PSABVO(KEYPS(L))
                            ENDIF
                        ENDDO
                        WRITE (fnumphes,'(/,A10,I6,2I6,2F6.1)',ADVANCE='NO')EXCODE,TN,PLYEAR,PLDAY,gdapfr,edapfr
                    ELSE  ! End Phenology simulated header writes
                        WRITE (fnumphes,'(A10,I6,2I6,2F6.1)',ADVANCE='NO')EXCODE,TN,PLYEAR,PLDAY,gdapfr,edapfr
                    ENDIF
                    DO L = 1,KEYSTX
                        IF (KEYPS(L).GT.0) WRITE (FNUMPHES,'(I6)',ADVANCE='NO')PSDAP(KEYPS(L))
                    ENDDO
                    CLOSE (FNUMPHES)
                    ! End Phenology simulated writes              
                    
                    ! Phenology (Measured;PHENOLM.OUT)
                    IF (TDATANUM.LE.0 .AND. .NOT.FEXISTA) THEN
                        WRITE (fnumwrk,*)' '
                        WRITE (fnumwrk,*)' No data so cannot write PHENOLOGY (MEASURED)' 
                        OPEN (UNIT=FNUMPHEM,FILE=FNAMEPHENOLM,STATUS ='UNKNOWN')
                        CLOSE (UNIT=FNUMPHEM, STATUS = 'DELETE')
                    ELSE
                        INQUIRE (FILE = FNAMEPHENOLM,EXIST = FFLAG)
                        OPEN(UNIT=FNUMPHEM,FILE=FNAMEPHENOLM,POSITION='APPEND')
                        IF (CROP.NE.CROPPREV.OR.RUN.EQ.1.OR.(.NOT.(FFLAG))) THEN
                            WRITE (FNUMPHEM,'(/,A14,A10)')'*PHENOLOGY(M):',EXCODE
                            WRITE (FNUMPHEM,'(A16,A24)',ADVANCE='NO')'@EXCODE     TRNO',' PYEAR  PDAT  GDAP  EDAP'
                            DO L = 1,KEYSTX
                                IF (KEYPS(L).GT.0) THEN
                                    WRITE (FNUMPHEM,'(A6)',ADVANCE='NO')PSABVO(KEYPS(L))
                                ENDIF 
                            ENDDO
                            WRITE (FNUMPHEM,'(/,A10,I6,2I6,2F6.1)',ADVANCE='NO')EXCODE,TN,PLYEAR,PLDAY,gdapfr,edapfr
                        ELSE ! End Phenology measured header writes
                            WRITE (FNUMPHEM,'(A10,I6,2I6,2F6.1)',ADVANCE='NO')EXCODE,TN,PLYEAR,PLDAY,gdapfr,edapfr
                        ENDIF
                        DO L = 1,KEYSTX
                            IF (KEYPS(L).GT.0) WRITE (FNUMPHEM,'(I6)',ADVANCE='NO')PSDAPM(KEYPS(L))
                        ENDDO
                        CLOSE (FNUMPHEM)
                    ENDIF  
                    ! End Phenology (Measured)
                    
                    ! Plant responses (Simulated)'
                    ! Set temporary planting date for overlapping year end
                    IF (RUNCRP.EQ.1) PLDAYTMP = -99
                    IF (PLDAY.LT.PLDAYTMP) THEN
                        IF (VARNO.EQ.VARNOPREV) THEN
                            PLDAYTMP = PLDAY + 365
                        ELSE
                            PLDAYTMP = PLDAY
                        ENDIF
                    ELSE
                        PLDAYTMP = PLDAY
                    ENDIF
                    PLDAYTMP = PLDAY
                    IF (EXCODE.NE.EXCODEPREV.OR.TNAME(1:1).EQ.'*') THEN
                        OPEN (UNIT=FNUMPRES,FILE=FNAMEPRES,POSITION='APPEND')
                        WRITE (FNUMPRES,*) ' '
                        WRITE (TLINETMP,9951) EXCODE,MODNAME
9951                    FORMAT ('*RESPONSES(S):',A10,'  ',A8)
                        IF (TNAME(1:1).EQ.'*') THEN
                            WRITE (FNUMPRES,'(A180)') TLINETMP
                        ELSE
                            WRITE (FNUMPRES,'(A180)') TLINETMP
                        ENDIF
                        WRITE (FNUMPRES,97,ADVANCE='NO')
97                      FORMAT ('@  RUN',' EXCODE   ',' TRNO RN    CR','  PDAT  EDAP')
                        DO L = 1,KEYSTX
                            IF (KEYPS(L).GT.0) THEN
                                WRITE (FNUMPRES,'(A6)',ADVANCE='NO') PSABVO(KEYPS(L))
                            ENDIF  
                        ENDDO
                        WRITE (FNUMPRES,297)
297                     FORMAT ('  HWAH  HWUH','  H#AH  H#GH  LAIX  L#SH BR#AH','  CWAH  VWAH  HIAH  RWAH','  HN%H  TNAH', &
                            '  CNAH  HNAH','  HINH PLPOP','  NICH',' SRADA TMAXA TMINA  PRCP')
                    ELSE
                        OPEN (UNIT=FNUMPRES,FILE=FNAMEPRES,POSITION='APPEND')
                    ENDIF  ! End Responses simulated header writes
                    WRITE (FNUMPRES,7401,ADVANCE='NO') RUN,EXCODE,TN,RN,CROP,PLDAYTMP,EDAPFR
7401                FORMAT (I6,1X,A10,I4,I3,4X,A2, I6, F6.1)  !Run,excode,tn,rn,crop, Pldaytmp, Edapfr
                    DO L = 1,KEYSTX
                        IF (L.EQ.MSTG.AND.HNUMBER.EQ.1) THEN
                            ! If harvested at a specific date
                            tvi1 = Dapcalc(yeardoyharf,plyear,plday)
                            WRITE (FNUMPRES,'(I6)',ADVANCE='NO') tvi1
                        ELSE
                            IF (KEYPS(L).GT.0) WRITE(FNUMPRES,'(I6)',ADVANCE='NO')PSDAP(KEYPS(L))
                        ENDIF
                    ENDDO
                    WRITE (fnumpres,409)NINT(hwam),hwumchar, NINT(hnumam),NINT(hnumgm),laixchar,lnumsm,brnumsh,NINT(cwam), &
                        NINT(vwam),hiamchar,NINT(rwam),hnpcmchar,NINT(AMAX1(-99.0,cnam+rnam)),NINT(cnam),NINT(hnam),hinmchar, &
                        pltpop,NINT(amtnit),sradcav,tmaxcav,tmincav,NINT(raincc)
409                 FORMAT (I6,A6,I6,I6,A6,F6.1,F6.1,I6,I6,A6,I6,A6,I6,I6,I6,A6,F6.1,I6,3F6.1,I6)
                    CLOSE(FNUMPRES)
                    ! End Responses simulated writes
                    
                    ! Plant responses (Measured)
                    IF (TDATANUM.LE.0 .AND. .NOT.FEXISTA) THEN
                        WRITE (fnumwrk,*)' '
                        WRITE (fnumwrk,*)' No data so cannot write PLANT RESPONSES (MEASURED)'
                        OPEN (UNIT = FNUMTMP,FILE = FNAMEPREM,STATUS='UNKNOWN')
                        CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
                    ELSE
                        IF (EXCODE.NE.EXCODEPREV.OR.TNAME(1:1).EQ.'*') THEN
                            OPEN (UNIT=FNUMPREM,FILE=FNAMEPREM,POSITION='APPEND')
                            WRITE (FNUMPREM,*) ' '
                            WRITE (TLINETMP,99511) EXCODE,MODNAME
99511                       FORMAT ('*RESPONSES(M):',A10,'  ',A8)
                            IF (TNAME(1:1).EQ.'*') THEN
                                WRITE (FNUMPREM,'(A180)') TLINETMP
                            ELSE
                                WRITE (FNUMPREM,'(A180)') TLINETMP
                            ENDIF
                            WRITE (FNUMPREM,97,ADVANCE='NO')
                            DO L = 1,KEYSTX
                                IF (KEYPS(L).GT.0) THEN
                                    WRITE (FNUMPREM,'(A6)',ADVANCE='NO') PSABVO(KEYPS(L))
                                ENDIF 
                            ENDDO
                            WRITE (FNUMPREM,298)
298                         FORMAT ('  HWAH  HWUH','  H#AH  H#GH  LAIX  L#SH BR#AH','  CWAH  VWAH  HIAH  RWAH','  HN%H  TNAH', &
                                '  CNAH  HNAH','  HINH PLPOP','  NICH',' SRADA TMAXA TMINA  PRCP')
                        ELSE
                            OPEN (UNIT=FNUMPREM,FILE=FNAMEPREM,POSITION='APPEND')
                        ENDIF ! End Responses measured header writes
                        WRITE (FNUMPREM,7401,ADVANCE='NO') RUN,EXCODE,TN,RN,CROP,PLDAYTMP,FLOAT(MAX(-99,edapm))
                        DO L = 1,KEYSTX
                            IF (L.EQ.MSTG.AND.HNUMBER.EQ.1) THEN
                                ! If harvested at a specific date
                                tvi1 = Dapcalc(yeardoyharf,plyear,plday)
                                WRITE (FNUMPREM,'(I6)',ADVANCE='NO') tvi1
                            ELSE
                                IF (KEYPS(L).GT.0) WRITE(FNUMPREM,'(I6)',ADVANCE='NO')PSDAPM(KEYPS(L))
                            ENDIF
                        ENDDO
                        !WRITE (FNUMPREM,'(I6)',ADVANCE='NO') PSDAPM(HSTG)
                        WRITE (FNUMPREM,409) NINT(hwamm),hwummchar,NINT(hnumamm),NINT(hnumgmm),laixmchar,lnumsmm,brnumshm, &
                            NINT(cwamm),NINT(vwamm),hiammchar,NINT(rwamm),hnpcmmchar,NINT(tnamm),NINT(cnamm),NINT(hnamm), &
                            hinmmchar,pltpopp,NINT(amtnit),sradcav,tmaxcav,tmincav,NINT(raincc)
                        CLOSE(FNUMPREM)
                    ENDIF  
                    ! End Responses (Measured)
                    
                ELSE  ! IDETL = 'D'
                    
                    OPEN (UNIT=FNUMPHES,FILE=FNAMEPHENOLS,STATUS = 'UNKNOWN')
                    CLOSE (UNIT=FNUMPHES, STATUS = 'DELETE')
                    OPEN (UNIT=FNUMPHEM,FILE=FNAMEPHENOLM,STATUS = 'UNKNOWN')
                    CLOSE (UNIT=FNUMPHEM, STATUS = 'DELETE')
                    OPEN (UNIT = FNUMPRES,FILE = FNAMEPRES,STATUS='UNKNOWN')
                    CLOSE (UNIT=FNUMPRES, STATUS = 'DELETE')
                    OPEN (UNIT = FNUMPREM,FILE = FNAMEPREM,STATUS='UNKNOWN')
                    CLOSE (UNIT=FNUMPREM, STATUS = 'DELETE')
                    
                ENDIF ! IDETL = 'D'
                
                !-----------------------------------------------------------------------------------------------------------
                !         IDETL = A OUTPUTS (Errora,Errort,Errors)
                !-----------------------------------------------------------------------------------------------------------
                
                IF (IDETL.EQ.'A') THEN     ! Write some error outputs
                    
                    ! Find intermediate stage dates
                    DO L = MSTG-1,1,-1
                        IF (psdapm(l).GT.0.0) THEN
                            psidapm = psdapm(l)
                            EXIT
                        ENDIF
                    ENDDO
                    IF (L.EQ.0) L = INT((FLOAT(MSTG)/2.0)+1)
                    IF (L.GT.0) THEN
                        PSIDAP = PSDAP(L)
                    ELSE
                        WRITE (fnumwrk,*)' '
                        WRITE (fnumwrk,*)' Problem in finding intermediate stage '
                        WRITE (fnumwrk,*)'  Mature stage       = ',mstg          
                        WRITE (fnumwrk,*)'  Intermediate stage = ',l             
                        WRITE (fnumwrk,*)' '
                    ENDIF
                    
                    ! Errors (A-data)
                    IF (TDATANUM.LE.0 .AND. .NOT.FEXISTA) THEN
                        WRITE (fnumwrk,*)' '
                        WRITE (fnumwrk,*)' No data so cannot write PLANTERA'
                        OPEN (UNIT=FNUMTMP,FILE=FNAMEERA,STATUS='UNKNOWN')
                        CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
                    ELSE ! If data availabe
                        IF (edapm.GT.0) THEN
                            emdaterr = 100.0*(Edap-Edapm)/edapm
                        ELSE
                            emdaterr = -99
                        Endif
                        IF (psidapm.GT.0) THEN
                            psidaterr = 100.0*(psidap-psidapm)/psidapm
                        ELSE
                            psidaterr = -99
                        Endif
                        IF (mdatm.GT.0) THEN
                            mdaterr = 100.0*(mdap-mdapm)/mdapm
                        ELSE
                            mdaterr = -99
                        Endif
                        IF (hwahm.GT.0.AND.hwam.GT.0.AND.hpcf.GT.0) THEN
                            hwaherr = 100.*(hwam*hpcf/100.-hwahm)/(hwahm*hpcf/100.)
                            IF (hwaherr.GT.99999.0) hwaherr = 99999.0
                            IF (hwaherr.LT.-9999.0) hwaherr = -9999.0
                        ELSE
                            hwaherr = -99
                        ENDIF
                        IF (hwumm.GT.0.AND.hwum.GT.0) THEN
                            hwumerr = 100.0*(hwum-hwumm)/hwumm
                        ELSE
                            hwumerr = -99
                        ENDIF
                        IF (hnumamm.GT.0.AND.hnumam.GT.0) THEN
                            hnumaerr = 100.0*(hnumam-hnumamm)/(hnumamm)
                        ELSE
                            hnumaerr = -99
                        ENDIF
                        IF (hnumgmm.GT.0.AND.hnumgm.GT.0) THEN
                            hnumgerr = 100.0*((hnumgm-hnumgmm)/hnumgmm)
                        ELSE
                            hnumgerr = -99
                        ENDIF
                        IF (laixm.GT.0.AND.laix.GT.0) THEN
                            laixerr = 100.0*((laix-laixm)/laixm)
                        ELSE
                            laixerr = -99
                        ENDIF
                        IF (lnumsmm.GT.0.AND.lnumsm.GT.0) THEN
                            lnumserr = 100.0*((lnumsm-lnumsmm)/lnumsmm)
                        ELSE
                            lnumserr = -99
                        ENDIF
                        IF (cwamm.GT.0.AND.cwam.GT.0) THEN
                            cwamerr = 100.0*(cwam-cwamm)/cwamm
                        ELSE
                            cwamerr = -99
                        Endif
                        IF (vwamm.GT.0.AND.vwam.GT.0) THEN
                            vwamerr = 100.0*(vwam-vwamm)/vwamm
                        ELSE
                            vwamerr = -99
                        Endif
                        IF (hiamm.GT.0.AND.hiam.GT.0) THEN
                            hiamerr = 100.0*(hiam-hiamm)/hiamm
                        ELSE
                            hiamerr = -99
                        Endif
                        IF (hnpcmm.GT.0.AND.hnpcm.GT.0) THEN
                            hnpcmerr = 100.0*(hnpcm-hnpcmm)/hnpcmm
                        ELSE
                            hnpcmerr = -99
                        Endif
                        IF (cnamm.GT.0.AND.cnam.GT.0) THEN
                            cnamerr = 100.0*(cnam-cnamm)/cnamm
                        ELSE
                            cnamerr = -99
                        Endif
                        IF (RUN.EQ.1) THEN
                            OPEN (UNIT=FNUMERA,FILE=FNAMEERA,POSITION='APPEND')
                            WRITE (FNUMERA,996)
996                         FORMAT (/,'*ERRORS(A)',/)
                            WRITE (FNUMERA,896)
896                         FORMAT ('@  RUN',' EXCODE     ','  TRNO RN','    CR','    EDAP   EDAPE','    MDAP   MDAPE', &
                                '    HWAH   HWAHE','    HWUM   HWUME','    H#AM   H#AME','    H#GM   H#GME','    LAIX   LAIXE', &
                                '    L#SM   L#SME','    CWAM   CWAME','    VWAM   VWAME','    HIAM   HIAME','    HN%M   HN%ME', &
                                '    CNAM   CNAME','    HNAM   HNAME')
                            
                            CLOSE(FNUMERA)
                        ENDIF  ! End ErrorA header writes
                        OPEN (UNIT = FNUMERA,FILE = FNAMEERA,POSITION = 'APPEND')
                        WRITE (FNUMERA,8401) RUN,EXCODE,TN,RN,CROP,Edap,emdaterr,mdap,mdaterr,NINT(hwam),NINT(hwaherr),hwum, &
                            NINT(hwumerr),NINT(hnumam),NINT(hnumaerr),hnumgm,NINT(hnumgerr),laix,NINT(laixerr),lnumsm, &
                            NINT(lnumserr),NINT(cwam),NINT(cwamerr),NINT(vwam),NINT(vwamerr),hiam,NINT(hiamerr),hnpcm, &
                            NINT(hnpcmerr),NINT(cnam),NINT(cnamerr),NINT(hnam),NINT(hnamerr)
8401                        FORMAT (I6,1X,A10,1X,I6,I3,4X,A2,6I8,F8.3,3I8,F8.1,I8,F8.1,I8,F8.1,5I8,F8.2,I8,F8.1,5I8)
                            CLOSE(FNUMERA)
                    ENDIF ! End ErrorA writes (If data available)
                    
                    ! Errors (T)
                    IF (.NOT.FEXISTT .OR. FROPADJ.GT.1 .OR. IDETG.EQ.'N') THEN
                        WRITE (fnumwrk,*) ' '
                        IF (FROPADJ.GT.1) THEN
                            WRITE (fnumwrk,*) ' Cannot write PLANT ERRORS (T).',' Frequency of output > 1 day'
                        ELSE  
                            WRITE (fnumwrk,*)' No data so cannot write PLANT ERRORS (T)'
                        ENDIF      
                        OPEN (UNIT=FNUMTMP,FILE=FNAMEERT,STATUS='UNKNOWN')
                        CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
                    ELSE
                        INQUIRE (FILE = 'PlantGro.OUT',OPENED = FOPEN)
                        IF (FOPEN) CLOSE (NOUTPG)
                        STARNUM = 0
                        OPEN (UNIT=FNUMT,FILE='Measured.out',STATUS='UNKNOWN')
                        DO WHILE (TLINET(1:1).NE.'@')
                            TLINET = ' '
                            READ (FNUMT,1502,END=1600,ERR=1600) TLINET
1502                        FORMAT(A180)
                            IF (TLINET(1:1).EQ.'*') STARNUM = STARNUM + 1
                            IF (TLINET(1:1).EQ.'@') THEN
                                IF (STARNUM.NE.STARNUMM) THEN
                                    TLINET = ' '
                                    READ (FNUMT,1502,END=1600,ERR=1600) TLINET
                                ENDIF
                            ENDIF
                        ENDDO
                        tlinet(1:1) = ' '
                        STARNUM = 0
                        OPEN (UNIT=NOUTPG,FILE='PlantGro.OUT',STATUS='UNKNOWN')
                        DO WHILE (TLINEGRO(1:1).NE.'@')
                            TLINEGRO = ' '
                            READ (NOUTPG,'(A254)') TLINEGRO
                            IF (TLINEGRO(1:4).EQ.'*RUN') STARNUM = STARNUM + 1
                            IF (TLINEGRO(1:1).EQ.'@') THEN
                                IF (STARNUM.NE.STARNUMO) THEN
                                    TLINEGRO = ' '
                                    READ (NOUTPG,'(A254)') TLINEGRO
                                ENDIF
                            ENDIF
                        ENDDO
                        tlinegro(1:1) = ' '
                        ! Find headers from Measured file
                        DO L = 1,20
                            CALL Getstr(tlinet,l,thead(l))
                            IF (THEAD(L)(1:3).EQ.'-99') EXIT
                            IF (THEAD(L)(1:3).EQ.'DAP') tfdapcol = l
                        ENDDO
                        TFCOLNUM = L-1
                        IF (TFCOLNUM.LE.0) THEN
                            WRITE (FNUMWRK,*) 'No columns found in T-file '
                            GO TO 7777
                        ENDIF
                        ! Make new header line
                        TLINETMP = ' '
                        TLINETMP(1:1) = '@'
                        DO L = 1, TFCOLNUM
                            TLPOS = (L-1)*6+1
                            IF (THEAD(L).EQ.'TRNO'.OR.THEAD(L).EQ.'YEAR'.OR.THEAD(L).EQ.'DATE') THEN
                                TLINETMP(TLPOS+2:TLPOS+5)=THEAD(L)(1:4)
                            ELSEIF(THEAD(L).EQ.'DOY'.OR.THEAD(L).EQ.'DAP' .OR.THEAD(L).EQ.'DAS'.OR.THEAD(L).EQ.'DAY') THEN
                                TLINETMP(TLPOS+3:TLPOS+5)=THEAD(L)(1:3)
                            ELSE
                                WRITE (TCHAR,'(I6)') NINT(ERRORVAL*100.0)
                                TLINETMP(TLPOS+1:TLPOS+4) = THEAD(L)(1:4)
                                TLINETMP(TLPOS+5:TLPOS+5) = 'E'
                            ENDIF
                        ENDDO
                        ! Find corresponding columns in PlantGro.OUT
                        DO L = 1,TFCOLNUM
                            pgrocol(l) = Tvicolnm(tlinegro,thead(l))
                        ENDDO
                        OPEN (UNIT=FNUMERT,FILE=FNAMEERT,POSITION='APPEND')
                        WRITE (FNUMERT,2996) OUTHED(12:79)
2996                    FORMAT (/,'*ERRORS(T):',A69,/)
                        tlinet(1:1) = '@'
                        WRITE (FNUMERT,'(A180)') TLINETMP
                        ! Read data lines, match dates, calculate errors, write
                        DO L1 = 1,200
                            TLINET = ' '
                            READ (FNUMT,7778,ERR=7777,END=7777) TLINET
7778                        FORMAT(A180)
                            IF (TLINET(1:1).EQ.'*') EXIT
                            IF (TLINET(1:6).EQ.'      ') GO TO 7776
                            CALL Getstri(tlinet,tfdapcol,tfdap)
                            IF (TFDAP.LE.0.0) THEN
                                EXIT
                            ENDIF
                            DO WHILE (tfdap.NE.pgdap)
                                TLINEGRO = ' '
                                READ (NOUTPG,7779,ERR=7777,END=7777) TLINEGRO
                                CALL Getstri(tlinegro,pgrocol(tfdapcol),pgdap)
                                IF (PGDAP.LT.0) THEN
                                    WRITE (FNUMWRK,*) 'DAP in Plantgro file < 0 '
                                    EXIT
                                ENDIF
                            ENDDO
7779                        FORMAT(A255)
                            TLINETMP = ' '
                            DO L = 1, TFCOLNUM
                                CALL Getstrr(tlinet,l,tfval)
                                CALL Getstrr(tlinegro,pgrocol(l),pgval)
                                ERRORVAL = 0.0
                                IF(TFVAL.GT.0.0.AND.PGVAL.GT.-99.AND.PGVAL.NE.0.0)THEN
                                    ERRORVAL = 100.0 * (PGVAL - TFVAL) / TFVAL
                                ELSE
                                    ERRORVAL = -99.0
                                ENDIF
                                IF (THEAD(L).EQ.'TRNO'.OR.THEAD(L).EQ.'YEAR' .OR.THEAD(L).EQ.'DOY'.OR.THEAD(L).EQ.'DAP' .OR. &
                                    THEAD(L).EQ.'DAY' .OR.THEAD(L).EQ.'DAS'.OR.THEAD(L).EQ.'DATE') THEN
                                    CALL Getstri(tlinet,l,tvi1)
                                    WRITE (TCHAR,'(I6)') TVI1
                                ELSE
                                    WRITE (TCHAR,'(I6)') NINT(ERRORVAL)
                                ENDIF
                                TLPOS = (L-1)*6+1
                                TLINETMP(TLPOS:TLPOS+5)=TCHAR
                            ENDDO
                            WRITE (FNUMERT,'(A180)') TLINETMP
7776                        CONTINUE
                        ENDDO
7777                    CONTINUE
                        GO TO 1601
1600                    CONTINUE
                        WRITE(fnumwrk,*)'End of file reading Measured.out'
                        WRITE(fnumwrk,*)'Starnum and starnumm were: ',starnum,starnumm
1601                    CONTINUE
                        CLOSE (FNUMERT)
                        CLOSE (FNUMT)
                        CLOSE (NOUTPG)
                        ! Re-open file if open at start of work here
                        IF (FOPEN) OPEN (UNIT=NOUTPG,FILE='PlantGro.OUT',POSITION='APPEND')
                    ENDIF  ! .NOT.FEXISTT .OR. FROPADJ.GT.1 .OR. IDETG.EQ.'N'
                    ! End of ErrorT writes
                    
                ELSE ! No ERROR files called for ... must be deleted          
                    
                    OPEN (UNIT=FNUMTMP,FILE=FNAMEERA,STATUS='UNKNOWN')
                    CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
                    OPEN (UNIT=FNUMTMP,FILE=FNAMEERT,STATUS='UNKNOWN')
                    CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
                    
                ENDIF ! End of Error writes  IDETL.EQ.'A'
                
                !-----------------------------------------------------------------------------------------------------------
                !         IDETD (DISPLAY) OUTPUTS IF WORKING IN CROPSIM SHELL
                !-----------------------------------------------------------------------------------------------------------
                
                ! Screen writes
                IF (IDETD.EQ.'S') THEN
                    IF (OUTCOUNT.LE.0) THEN
                        CALL CSCLEAR5
                        WRITE(*,*)' SIMULATION SUMMARY'
                        WRITE(*,*)' '
                        WRITE (*,499)
499                     FORMAT ('   RUN EXCODE    TRNO RN',' TNAME..................','.. REP  RUNI S O C CR  HWAM')
                    ENDIF
                    IF (OUTCOUNT .EQ. 25) THEN
                        OUTCOUNT = 1
                    ELSE
                        OUTCOUNT = OUTCOUNT + 1
                    ENDIF
                    WRITE (*,410) run,excode,tn,rn,tname(1:25),rep,runi,sn,on,cn,crop,NINT(hwam)
410                 FORMAT (I6,1X,A10,I4,I3,1X,A25,I4,I6,I2,I2,I2,1X,A2,I6)
                ELSEIF (IDETD.EQ.'M') THEN
                    ! Simulation and measured data
                    CALL CSCLEAR5
                    WRITE(*,'(A20,A10,I3)')' STAGES SUMMARY FOR ',EXCODE,TN
                    WRITE(*,*)' '
                    WRITE(*,9600)
                    DO L = 1, PSNUM
                        CALL Csopline(laic,laistg(l))
                        IF (STGYEARDOY(L).LT.9999999.AND.L.NE.10.AND.L.NE.11) THEN
                            CALL CSYR_DOY(STGYEARDOY(L),YEAR,DOY)
                            CALL Calendar(year,doy,dom,month)
                            CNCTMP = 0.0
                            IF (CWADSTG(L).GT.0.)CNCTMP = CNADSTG(L)/CWADSTG(L)*100
                            WRITE (*,'(I8,I4,1X,A3,I4,1X,I1,1X,A13,I6,A6,F6.1,I6,F6.2,F6.2,F6.2)')STGYEARDOY(L),DOM,MONTH, &
                                Dapcalc(stgyeardoy(L),plyear,plday),L,PSNAME(L),NINT(CWADSTG(L)),LAIC,LNUMSTG(L), &
                                NINT(CNADSTG(L)),CNCTMP,1.0-WFPPAV(L),1.0-NFPPAV(L)
                        ENDIF
                    ENDDO
                    WRITE(*,*)' '
                    WRITE(*,*)' Press ENTER to continue'
                    PAUSE ' '
                    CALL CSCLEAR5
                    WRITE(*,'(A36,A10,I3)')' SIMULATED-MEASURED COMPARISONS FOR ',EXCODE,TN
                    WRITE(*,*)' '
                    WRITE (*,206)
                    WRITE (*,290) MAX(-99,gdap),MAX(-99,gdapm),MAX(-99,edap),MAX(-99,edapm)
                    DO L = 1,KEYSTX
                        IF (KEYPS(L).GT.0) WRITE (*,291)psname(KEYPS(L)),PSDap(KEYPS(L)),PSDapm(KEYPS(L))
                    ENDDO
                    WRITE (*,305)NINT(cwam),NINT(cwamm),NINT(rwam+sdwam),NINT(rwamm),NINT(senwacm),NINT(senwacmm), &
                        NINT(hwam),NINT(hwamm),NINT(vwam),NINT(vwamm),hiam,hiamm,NINT(rswam),NINT(rswamm)
                ENDIF ! End IDETD.EQ.'S' 
                
                !-----------------------------------------------------------------------------------------------------------
                !         Screen writes for Dssat sensitivity mode
                !-----------------------------------------------------------------------------------------------------------
                
                IF (FILEIOT(1:3).EQ.'DS4' .AND. CN.EQ.1.AND. RNMODE.EQ.'E') THEN         
                    CALL CSCLEAR5
                    WRITE(*,9589)
                    WRITE(*,*) ' '
                    WRITE(*,9588)
                    WRITE(*,9600)
                    DO L = 1, PSNUM
                        CALL Csopline(laic,laistg(l))
                        IF (STGYEARDOY(L).LT.9999999.AND.L.NE.10.AND.L.NE.11) THEN
                            CALL CSYR_DOY(STGYEARDOY(L),YEAR,DOY)
                            CALL Calendar(year,doy,dom,month)
                            CNCTMP = 0.0
                            IF (CWADSTG(L).GT.0.0)CNCTMP = CNADSTG(L)/CWADSTG(L)*100
                            WRITE (*,'(I8,I4,1X,A3,I4,1X,I1,1X,A13,I6,A6,F6.1,I6,F6.2,F6.2,F6.2)')STGYEARDOY(L),DOM,MONTH, &
                                Dapcalc(stgyeardoy(L),(plyeardoy/1000),plday),l,psname(l),NINT(CWADSTG(L)),LAIC,LNUMSTG(L), &
                                NINT(CNADSTG(L)),CNCTMP,1.0-WFPPAV(L),1.0-NFPPAV(L)
                        ENDIF
                    ENDDO
                    ! For harvest at specified date
                    IF (YEARDOYHARF.EQ.YEARDOY) THEN
                        CALL Csopline(laic,lai)
                        CALL CSYR_DOY(YEARDOYHARF,YEAR,DOY)
                        CALL Calendar(year,doy,dom,month)
                        CNCTMP = 0.0
                        IF (CWAD.GT.0.0)CNCTMP = CNAD/CWAD*100
                        WRITE (*,'(I8,I4,1X,A3,I4,1X,I1,1X,A13,I6,A6,F6.1,I6,F6.2,F6.2,F6.2)') YEARDOY,DOM,MONTH,Dapcalc(yeardoy, &
                            (plyeardoy/1000),plday),l,'Harvest      ',NINT(CWAD),LAIC,LNUM,NINT(CNAD),CNCTMP,1.0-WFPCAV,1.0-NFPCAV
                    ENDIF 
                    
                    WRITE(*,*)' '
                    WRITE(*,*)' Press ENTER to continue'
                    PAUSE ' '
                    CALL CSCLEAR5
                    
                    !WRITE (*,206)
                    WRITE(*,'(A36,A10,I3)') ' SIMULATED-MEASURED COMPARISONS FOR ',EXCODE,TN
                    WRITE(*,*)' '
                    WRITE (*,206)
                    WRITE (*,290) MAX(-99,gdap),MAX(-99,gdapm),MAX(-99,edap),MAX(-99,edapm)
                    DO L = 1,KEYSTX
                        IF (KEYPS(L).GT.0) WRITE (*,291) psname(KEYPS(L)),psdap(KEYPS(L)),psdapm(KEYPS(L))
                    ENDDO
                    WRITE (*,305)NINT(cwam),NINT(cwamm),MAX(-99,NINT(rwam+sdwam)),NINT(rwamm),NINT(senwacm),NINT(senwacmm), &
                        NINT(hwam),NINT(hwamm),NINT(vwam),NINT(vwamm),hiam,hiamm, NINT(rswam),NINT(rswamm)
                    IF (lwphc+swphc.GT.0.0) WRITE (*,3051) NINT(cwahc),NINT(cwahcm)
                    WRITE(*,*)' '
                    WRITE(*,*)' Press ENTER to continue'
                    PAUSE ' '
                    CALL CSCLEAR5
                    
                    WRITE (*,2061)
2061                FORMAT(/,"@",5X,"VARIABLE",T42,"SIMULATED   MEASURED",/,6X,"--------",T42,"---------   --------")  
                    WRITE (*,3052) hwumchar,hwummchar,NINT(hnumam),NINT(hnumamm),hnumgm,hnumgmm, laix,laixm,lnumsm,lnumsmm, &
                        nupac,nupacm,cnam,cnamm,rnam,rnamm,sennatc,sennatcm,hnam,hnamm,vnam,vnamm,hinm,hinmm,hnpcm,hnpcmm, &
                        vnpcm,vnpcmm
                    
                    WRITE(*,*)' '
                    WRITE(*,*)' Press ENTER to continue'
                    PAUSE ' '
                    CALL CSCLEAR5
                    CALL CSCLEAR5
                    CALL CSCLEAR5
                    CALL CSCLEAR5
                    
                ENDIF ! END OF SCREEN WRITES FOR DSSAT SENSITIVITY MODE
                
                !-----------------------------------------------------------------------------------------------------------
                !         Store variables for sending to CSM summary output routines
                !-----------------------------------------------------------------------------------------------------------
                
                ! Store summary labels and values in arrays to send to
                ! CSM OPSUM routine for printing.  Integers are temporarily
                ! saved as real numbers for placement in real array.
                
                IF (IDETO.EQ.'E'.OR.IDETO.EQ.'N') THEN
                    ! Resource productivity calculations 
                    ! (Nnot done earlier because no Overview called for)
                    DMP_Rain = -99.
                    GrP_Rain = -99.
                    DMP_ET = -99.
                    GrP_ET = -99.
                    DMP_EP = -99.
                    GrP_EP = -99.
                    DMP_Irr = -99.    
                    GrP_Irr = -99.
                    DMP_NApp = -99.
                    GrP_NApp = -99.
                    DMP_NUpt = -99.
                    GrP_NUpt = -99.
                    IF (RAINCC > 1.E-3) THEN
                        DMP_Rain = CWAM / RAINCC 
                        GrP_Rain = HWAM  / RAINCC
                    ENDIF
                    IF (ETCC > 1.E-3) THEN
                        DMP_ET = CWAM / ETCC 
                        GrP_ET = HWAM  / ETCC 
                    ENDIF
                    IF (EPCC > 1.E-3) THEN
                        DMP_EP = CWAM / EPCC 
                        GrP_EP = HWAM  / EPCC 
                    ENDIF
                    IF (IRRAMTC > 1.E-3) THEN
                        DMP_Irr = CWAM / IRRAMTC 
                        GrP_Irr = HWAM  / IRRAMTC
                    ENDIF
                    IF (ISWNIT.NE.'N') THEN
                        IF (Amtnit > 1.E-3) THEN
                            DMP_NApp = CWAM / Amtnit
                            GrP_NApp = HWAM  / Amtnit
                        ENDIF
                        IF (NUPAC > 1.E-3) THEN
                            DMP_NUpt = CWAM / NUPAC
                            GrP_NUpt = HWAM  / NUPAC
                        ENDIF
                    ENDIF ! ISWNIT NE 'N'
                    WRITE (FNUMWRK, 1200) CDAYS,RAINCC, DMP_Rain*0.1, DMP_Rain, GrP_Rain*0.1, GrP_Rain,ETCC,  DMP_ET*0.1, &
                        DMP_ET,GrP_ET*0.1,GrP_ET,EPCC,DMP_EP*0.1,DMP_EP,GrP_EP*0.1,GrP_EP
                    IF (IRRAMTC > 1.E-3) THEN
                        WRITE(FNUMWRK, 1210) IRRAMTC, DMP_Irr*0.1, DMP_Irr, GrP_Irr*0.1, GrP_Irr
                    ENDIF  
                    IF (ISWNIT.NE.'N') THEN
                        IF (Amtnit > 1.E-3) THEN
                            WRITE(FNUMWRK, 1220) Amtnit, DMP_NApp, GrP_NApp 
                        ENDIF
                        IF (NUPAC > 1.E-3) THEN
                            WRITE(FNUMWRK, 1230) NUPAC, DMP_NUpt,GrP_NUpt
                        ENDIF
                    ENDIF ! ISWNIT NE 'N'
                ENDIF
                
                LABEL(1) = 'ADAT'; VALUE(1) = -99.0
                IF (stgyeardoy(mstg).LT.9999999) THEN
                    LABEL(2) = 'MDAT'; VALUE(2) = FLOAT(stgyeardoy(mstg))
                ELSE
                    LABEL(2) = 'MDAT'; VALUE(2) = -99.0
                ENDIF
                LABEL(3) = 'DWAP'; VALUE(3) = sdrate
                LABEL(4) = 'CWAM'; VALUE(4) = cwam
                LABEL(5) = 'HWAM'; VALUE(5) = hwam
                LABEL(6) = 'HWAH '; VALUE(6) = hwam * hpcf/100.0
                LABEL(7) = 'BWAH '; VALUE(7) = vwam * hbpcf/100.0
                LABEL(8) = 'HWUM'; VALUE(8) = hwum
                LABEL(9) = 'H#AM'; VALUE(9) = hnumam
                LABEL(10) = 'H#UM'; VALUE(10) = hnumgm
                LABEL(11) = 'NUCM'; VALUE(11) = nupac
                LABEL(12) = 'CNAM'; VALUE(12) = cnam
                LABEL(13) = 'GNAM'; VALUE(13) = hnam
                LABEL(14) = 'PWAM'; VALUE(14) = -99.0
                LABEL(15) = 'LAIX'; VALUE(15) = laix
                LABEL(16) = 'HIAM'; VALUE(16) = hiam
                
                LABEL(17) = 'DMPPM'; VALUE(17) = DMP_Rain 
                LABEL(18) = 'DMPEM'; VALUE(18) = DMP_ET                     
                LABEL(19) = 'DMPTM'; VALUE(19) = DMP_EP                     
                LABEL(20) = 'DMPIM'; VALUE(20) = DMP_Irr
                LABEL(21) = 'DPNAM'; VALUE(21) = DMP_NApp
                LABEL(22) = 'DPNUM'; VALUE(22) = DMP_NUpt
                
                LABEL(23) = 'YPPM ' ; VALUE(23) = GrP_Rain                  
                LABEL(24) = 'YPEM ' ; VALUE(24) = GrP_ET                   
                LABEL(25) = 'YPTM ' ; VALUE(25) = GrP_EP                    
                LABEL(26) = 'YPIM ' ; VALUE(26) = GrP_Irr
                LABEL(27) = 'YPNAM' ; VALUE(27) = GrP_NApp
                LABEL(28) = 'YPNUM' ; VALUE(28) = GrP_NUpt
                LABEL(29) = 'EDAT ' ; VALUE(29) = FLOAT(EYEARDOY)     
                
                LABEL(30) = 'NDCH ' ; VALUE(30) = FLOAT(CDAYS) 
                LABEL(31) = 'TMINA' ; VALUE(31) = TMINCAV       
                LABEL(32) = 'TMAXA' ; VALUE(32) = TMAXCAV       
                LABEL(33) = 'SRADA' ; VALUE(33) = SRADCAV       
                LABEL(34) = 'DAYLA' ; VALUE(34) = DAYLCAV       
                LABEL(35) = 'CO2A ' ; VALUE(35) = CO2CAV        
                LABEL(36) = 'PRCP ' ; VALUE(36) = RAINCC       
                LABEL(37) = 'ETCP ' ; VALUE(37) = ETCC      
                
                IF (FILEIOT(1:2).EQ.'DS') CALL SUMVALS (SUMNUM, LABEL, VALUE)
                
                !-----------------------------------------------------------------------------------------------------------
                !         Re-initialize
                !-----------------------------------------------------------------------------------------------------------
                
                ! Need to re-initialize following because of automatic
                ! fertilization routines in DSSAT
                NFG = 1.0
                NFP = 1.0
                WFG = 1.0
                WFP = 1.0
                
                UNO3 = 0.0
                UNH4 = 0.0
                
                WRITE (fnumwrk,*) ' '
                WRITE (fnumwrk,'(A50)')' END OF RUN. WILL BEGIN NEW CYCLE IF CALLED FOR.  '
                IF (IDETL.NE.'N') WRITE (fnumwrk,*) ' '
                SEASENDOUT = 'Y'
                
            ENDIF ! End STGYEARDOY(11).EQ.YEARDOY.OR.DYNAMIC.EQ.SEASEND
            !---------------------------------------------------------------------------------------------------------------
            !       Store variables for possible use next day/step
            !---------------------------------------------------------------------------------------------------------------
            
            CNADPREV = CNAD
            IF (CN.LE.1) CROPPREV = CROP
            IF (RNMODE.NE.'G') CUDIRFLPREV = CUDIRFLE
            CWADPREV = CWAD
            DAYLPREV = DAYL
            ECDIRFLPREV = ECDIRFLE
            ECONOPREV = ECONO
            EMRGFRPREV = EMRGFR
            GESTAGEPREV = GESTAGE
            BRSTAGEPREV = BRSTAGE
            LAIPREV = LAI
            LNUMPREV = LNUM
            PARIPREV = PARI
            PLYEARDOYPREV = PLYEARDOY
            BRNUMSTPREV = BRNUMST
            SPDIRFLPREV = SPDIRFLE
            SRADPREV = SRAD
            SWFRPREV = SWFR
            VARNOPREV = VARNO
            YEARDOYPREV = YEARDOY
            
    !***********************************************************************************************************************
    ELSEIF (DYNAMIC.EQ.SEASEND) THEN
    !***********************************************************************************************************************
        
        IF (STGYEARDOY(11).NE.YEARDOY) THEN  ! End for non-crop reason
            WRITE (fnumwrk,*)' '
            WRITE (fnumwrk,'(A50)') ' Run terminated.(Usually because ran out of weather data).'
        ENDIF
        
        EXCODEPREV = EXCODE
        
        CLOSE (NOUTPG)
        CLOSE (NOUTPG2)
        CLOSE (NOUTPGF)
        CLOSE (NOUTPN)
        CLOSE (FNUMWRK)
        
    !***********************************************************************************************************************
    ENDIF ! End of INITIATION-RATES-INTEGRATE-OUTPUT-SEASEND construct
    !***********************************************************************************************************************
    
    ! Store previous dynamic setting    DYNAMICPREV = DYNAMIC
    
    !***********************************************************************************************************************
    !    Call other modules
    !***********************************************************************************************************************
    
    IF (LENDIS.GT.0.AND.LENDIS.LT.3) THEN
        IF (ISWDIS(LENDIS:LENDIS).NE.'N') CALL Disease( &
            Spdirfle,run,runi,step,      &! Run+crop component
        fropadj,outhed,              &        ! Loop info.
        year,doy,dap,                &        ! Dates
        didat,digfac,diffacr,        &        ! Disease details
        dcdat,dcfac,dcdur,dctar,     &        ! Disease control
        tmax,tmin,dewdur,            &        ! Drivers - weather
        pla,plas,pltpop,             &        ! States - leaves
        lnumsg,lap,LAPP,laps,        &        ! States - leaves
        stgyeardoy,                  &        ! Stage dates
        didoy,                       &        ! Disease initiation
        dynamic)                              ! Control
    ENDIF
    
    IF (DYNAMIC.EQ.INTEGR.AND.LNUMSG.GT.0) CALL Cscrplayers( &
        chtpc,clapc,                     &        ! Canopy characteristics
    pltpop,lai,canht,                &        ! Canopy aspects
    lnumsg,lap,lapp,laps,            &        ! Leaf cohort number,size
    LAIL,LAILA,                      &        ! Leaf area indices,layers
    LAIA)                                     ! Leaf area index,active
    
9589 FORMAT (//,'*SIMULATED CROP AND SOIL STATUS AT MAIN DEVELOPMENT STAGES')
9588 FORMAT (/,' ...... DATE ....... GROWTH STAGE    BIOMASS   LEAF       CROP N      STRESS')     
9600 FORMAT(' YEARDOY DOM MON DAP ................ kg/ha AREA NUMBER  kg/ha   %   H2O    N')
206  FORMAT(/, &
    "*MAIN GROWTH AND DEVELOPMENT VARIABLES",//, &
    "@",5X,"VARIABLE",T42,"SIMULATED   MEASURED",/,6X, &
    "--------",T42,"---------   --------")
    
    500 FORMAT(/, &
    '*ENVIRONMENTAL AND STRESS FACTORS',//, &
    ' |-------Branching tier-------|-------------Environment--------','------|----------------Stress-----------------|',/, &
    30X,'|--------Average-------|---Cumulative--|         (0=Min, 1=','Max Stress)         |',/, &
    25X,'Time  Temp  Temp Solar Photop         Evapo |----Water---|-','-Nitrogen--|--Phosphorus-|',/, &
    25X,'Span   Max   Min   Rad  [day]   Rain  Trans  Photo',9X,'Pho','to         Photo',/, &
    25X,'days    øC    øC MJ/m2     hr     mm     mm  synth Growth  ','synth Growth  synth Growth',/,110('-'))

270 FORMAT(/, 110('-'))
300 FORMAT(/,10X,A," YIELD : ",I8," kg/ha    [Dry weight] ",/)

    1200 FORMAT(110('-'), ///, &
    '*RESOURCE PRODUCTIVITY',//, &
    ' Growing season length:', I4,' days ',//, &
    ' Precipitation during growth season',T42,F7.1,' mm[rain]',/, &
    '   Dry Matter Productivity',T42,F7.2,' kg[DM]/m3[rain]',T75,'=',F7.1,' kg[DM]/ha per mm[rain]',/, &
    '   Yield Productivity',T42,F7.2,' kg[Sroot yield]/m3[rain]',T75,'=',F7.1,' kg[yield]/ha per mm[rain]',//, &
    ' Evapotranspiration during growth season',T42,F7.1,' mm[ET]',/, &
    '   Dry Matter Productivity',T42,F7.2,' kg[DM]/m3[ET]',T75,'=',F7.1,' kg[DM]/ha per mm[ET]',/, &
    '   Yield Productivity',T42,F7.2,' kg[Sroot yield]/m3[ET]',T75,'=',F7.1,' kg[yield]/ha per mm[ET]',//, &
    ' Transpiration during growth season',T42,F7.1,' mm[EP]',/, &
    '   Dry Matter Productivity',T42,F7.2,' kg[DM]/m3[EP]',T75,'=',F7.1,' kg[DM]/ha per mm[EP]',/, &
    '   Yield Productivity',T42,F7.2,' kg[Sroot yield]/m3[EP]',T75,'=',F7.1,' kg[yield]/ha per mm[EP]')

    1210 FORMAT(/, &
    ' Irrigation during growing season',T42,F7.1,' mm[irrig]',/, &
    '   Dry Matter Productivity',T42,F7.2,' kg[DM]/m3[irrig]',T75,'=',F7.1,' kg[DM]/ha per mm[irrig]',/, &
    '   Yield Productivity',T42,F7.2,' kg[Sroot yield]/m3[irrig]',T75,'=',F7.1,' kg[yield]/ha per mm[irrig]')

    1220 FORMAT(/, &
    ' N applied during growing season',T42,F7.1,' kg[N applied]/ha'/, &
    '   Dry Matter Productivity',T42,F7.1,' kg[DM]/kg[N applied]',/,  &
    '   Yield Productivity',T42,F7.1,' kg[yield]/kg[N applied]')

    1230 FORMAT(/,&
    ' N uptake during growing season',T42,F7.1,' kg[N uptake]/ha'/, &
    '   Dry Matter Productivity',T42,F7.1,' kg[DM]/kg[N uptake]', /, &
    '   Yield Productivity',T42,F7.1,' kg[yield]/kg[N uptake]')

      END  ! CSCAS
