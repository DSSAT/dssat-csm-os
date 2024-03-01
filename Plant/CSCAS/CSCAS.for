!-----------------------------------------------------------------------
!  CROPSIM CASSAVA GROWTH AND DEVELOPMENT MODULE 010115  
!  Last edit 040414 LAH 
!  (After problems implementing fixes of GH 180214,260314;PM,MF 260214)
!  2023-01-26 CHP Reduce compile warnings: add EXTERNAL stmts, remove 
!                 unused variables, shorten lines.                    
!-----------------------------------------------------------------------

      SUBROUTINE CSCAS (FILEIOIN, RUN, TN, RN, RNMODE,     !Command line
     & ISWWAT, ISWNIT, ISWDIS, MESOM,                      !Contols
     & IDETS, IDETO, IDETG, IDETL, FROP,                   !Controls
     & SN, ON, RUNI, REP, YEAR, DOY, STEP, CN,             !Run+loop
     & SRAD, TMAX, TMIN, TAIRHR, RAIN, CO2, TDEW,          !Weather
     & DRAIN, RUNOFF, IRRAMT,                              !Water
     & DAYL, WINDSP, DEWDUR, CLOUDS, ST, EO, ES,           !Weather
     & NLAYR, DLAYR, DEPMAX, LL, DUL, SAT, BD, SHF, SLPF,  !Soil states
     & SW, NO3LEFT, NH4LEFT, FERNIT,                       !H2o,N states
     & TLCHD, TNIMBSOM, TNOXD,                             !N components
     & TOMINFOM, TOMINSOM, TOMINSOM1, TOMINSOM2, TOMINSOM3,!N components
     & YEARPLTCSM, HARVFRAC,                               !Pl.date
     & PARIP, PARIPA, EOP, EP, ET, TRWUP, ALBEDOS,         !Resources
     & CAID, KCAN, KEP,                                    !States
     & RLV, NFP, RWUPM, RWUMX, CANHT, LAIL, LAILA,         !States
     & UNO3, UNH4, UH2O,                                   !Uptake
     & SENCALG, SENNALG, SENLALG,                          !Senescence
     & RESCALG, RESNALG, RESLGALG,                         !Residues
     & STGYEARDOY, BRSTAGE,                                !Stage dates
     & DYNAMIC)                                            !Control

      ! For incorporation in CSM should:
      !  Ensure that Alt-plant routine:
      !   Sending control ISWDIS
      !   Sending DEWDUR(=-99.0?), CLOUDS(=0.0?), ES, ALBEDO(=0.2)
      !   Setting dummies for PARIP, PARIPA, LAIL, LAILA
      !  Eliminate '!' from SUMVALS call.

      ! And to run well in CSM should:
      !    Read ICIN (with -99 in profile) and ICSW variables
      
      ! Changes 8->151213:
      !    1. Individual leaf area expansion routine. More individual
      !       leaf sizes output to LEAVES.OUT (No stress, H2o,N,Temp.
      !       stress, H2o,N,Temp+assimilate limitation, H2o,N,Temp+
      !       assimilate+N concentration maintenance)  

      !    2. Leaf accelerated senescence function (for when loser
      !       canopy in deep shade). Now accelerates ageing in 
      !       active leaf phase, does not 'chop-off' leaf area.
      !       Amount of acceleration specified in SPE file.     
      
      ! Changes 140812 to make cassava work ok:
      !    1. Definition of DUTOMSTG changed. Before included PD(mstg),
      !       now stops at MSTG so that DSTAGE =  LSTAGE for cassava
      !    2. Stem N growth from leaves algorithm not woerking well.
      !       Changed to make sure that only works when stem N below 
      !       minimum and leaf N above minimum

      ! Temporary changes .. need firming-up:
      !    Height increase on emergence day has initialisation value

      ! Changes for cassava
      !    1  Product read-in from species file;product->harvest outputs
      !    2  Stages read-in from species file
      !    3. Temperature and water stress effects on specific leaf area
      !    4. Leaf appearance rate reduction with leaf number
      !    5. PTF parameters derived from root/shoot function
      !    6. Memory water stress factor calculated from WFG->leaf area
      !    7. Storage root initiation period introduced
      !    8. Storage root number initiation parameter introduced
      !    9. Storage root basic fraction parameter introduced
      !   10. Photoperiod sensitivity type read-in from species file
      !   11. Storage root N added
      !   12. Protection of N supply for leaf growth added. (Problem 
      !       with N on growth because lack of N->reduced growth->
      !       reduced uptake!

      USE OSDefinitions
      USE ModuleData
      
      IMPLICIT NONE
      EXTERNAL YR_DOY, WARNING, GETLUN, SUMVALS, Y4K_DOY, HEADER, 
     &  TVILENT, TVICOLNM, TL10FROMI, LTRIM, XREADC, XREADT, SPREADCA, 
     &  SPREADRA, TFAC4, CSTIMDIF, CSOPLINE, XREADI, XREADR, UCASE, 
     &  XREADIA, XREADRA, FVCHECK, FINDDIR, CUREADC, CUREADR, ECREADR, 
     &  SPREADR, YVALXY, CALENDAR, DAPCALC, LTRIM2, AREADR, AREADI, 
     &  CSYDOY, GETSTRI, CSCLEAR5, GETSTR, GETSTRR, XREADCA, CSUCASE, 
     &  SPREADC, EVAPO, CSCRPROOTWU, CSIDLAYR, CSVPSAT, CSYVAL, 
     &  DISEASE, CSCRPLAYERS

      INTEGER,PARAMETER::DINX =   3 ! Disease number,maximum
      INTEGER,PARAMETER::PSX  =  20 ! Principal stages,maximum
      INTEGER,PARAMETER::SSX  =  20 ! Secondary stages,maximum
      INTEGER,PARAMETER::KEYSTX = 9 ! Maximum number of key stages
      INTEGER,PARAMETER::DCNX =  10 ! Disease control #,maximum
      INTEGER,PARAMETER::LCNUMX=500 ! Maximum number of leaf cohorts
      INTEGER,PARAMETER::LNUMX= 500 ! Maximum number of leaves/axis
      INTEGER,PARAMETER::HANUMX= 40 ! Maximum # harvest instructions
!     INTEGER,PARAMETER::NL   =  20 ! Maximum number of soil layers

!     INTEGER,PARAMETER::RUNINIT=1  ! Program initiation indicator
!     INTEGER,PARAMETER::SEASINIT=2 ! Reinitialisation indicator
!     INTEGER,PARAMETER::RATE = 3   ! Program rate calc.indicator
!     INTEGER,PARAMETER::INTEGR=4   ! Program update indicator
!     INTEGER,PARAMETER::OUTPUT=5   ! Program output indicator
!     INTEGER,PARAMETER::SEASEND= 6 ! Program ending indicator

      CHARACTER(LEN=1),PARAMETER::BLANK = ' '
      CHARACTER(LEN=3),PARAMETER::DASH = ' - '
      CHARACTER(LEN=6),PARAMETER::ERRKEY = 'CSCAS '

      !REAL,PARAMETER::PATM=101300.0! Pressure of air,Pa
      !REAL,PARAMETER::SHAIR=1005.0 ! Specific heat of air,MJ/kg
      !REAL,PARAMETER::SBZCON=4.903E-9 !Stefan Boltzmann,MJ/K4/m2/d
      
      REAL          AFLF(0:LNUMX) ! CH2O factor for leaf,average   #
      REAL          AH2OROOTZONE  ! Available h2o in root zone     mm
      REAL          AH2OPROFILE   ! Available H2o,profile          mm
      REAL          ALBEDO        ! Canopy+soil albedo             fr
      REAL          ALBEDOS       ! soil albedo                    fr
      REAL          AMTNIT        ! Cumulative amount of N applied kg/ha
!     REAL          AMTNITPREV    ! Cumulative N,previous treatmnt kg/ha
      REAL          ANDEM         ! Crop N demand                  kg/ha
      REAL          ANFER(200)    ! N amount in fertilizer appln   kg/ha
      REAL          AREAPOSSIBLE  ! Leaf area growth at SLA limit  cm2 
      REAL          AREAPOSSIBLEN ! Leaf area growth at N limit    cm2 
      INTEGER       ARGLEN        ! Argument component length      #
      REAL          AVGSW         ! Average soil water in SWPLTD   %
      REAL          BASELAYER     ! Depth at base of layer         cm
      REAL          BD(20)        ! Bulk density (moist)           g/cm3
      REAL          BRFX(PSX)     ! Branch # per fork at each fork #
      REAL          BRNUMSH       ! Branch number/shoot at harvest #
      REAL          BRNUMSHM      ! Branch #/shoot,harvest,measurd #
      REAL          BRNUMST       ! Branch number/shoot (>forking) #
      REAL          BRNUMSTPREV   ! Branch number/shoot,previous   #
      REAL          BRSTAGE       ! Branching stage                #
      REAL          BRSTAGEPREV   ! Branching stage,previous       #
      REAL          BRSTAGETMP    ! Branching stage,temporary      #
      REAL          CAID          ! Canopy area index              #
      REAL          CANHT         ! Canopy height                  cm
      REAL          CANHTG        ! Canopy height growth           cm
      REAL          CANHTS        ! Canopy height standard         cm
      REAL          CARBOADJ      ! Ch2o adjustment for LAI change g/p
      REAL          CARBOBEG      ! Ch2o available,beginning day   g/p
      REAL          CARBOBEGI     ! Ch2o avail,internal co2 calc   g/p
      REAL          CARBOBEGIA    ! Ch2o avail,internal co2,adj    #
      REAL          CARBOBEGR     ! Ch2o avail,PARUE calculation   g/p
      REAL          CARBOBEGM     ! Ch2o avail,Monteith ccalc.     g/p
      REAL          CARBOC        ! Ch2o assimilated,cumulative    g/p
      REAL          CARBOEND      ! Ch2o available,end of day      g/p
      REAL          CARBOR        ! Ch2o available,roots           g/p
      REAL          CARBOT        ! Ch2o available,tops            g/p
      REAL          CARBOTMP      ! Ch2o available,temporary value g/p
      REAL          CARBOTMPI     ! Ch2o avail,internal co2 calc   g/p
      REAL          CARBOTMPR     ! Ch2o avail,PARUE calculation   g/p
      REAL          CARBOTMPM     ! Ch2o avail,Monteith calc       g/p
      INTEGER       CDAYS         ! Crop cycle duration            PVoCd
      REAL          CHTPC(10)     ! Canopy ht % associated w LA%   %
      REAL          CRFR          ! Crown growth rate,fr stem gr   #
      REAL          CRRSWAD       ! Crown reserves                 kg/ha
      REAL          CRRSWT        ! Crown reserves                 g/p
      REAL          CRWAD         ! Crown weight                   kg/ha
      REAL          CRWADOUT      ! Crown weight for output        kg/ha
      REAL          CRWT          ! Crown weight                   g/p
      REAL          CRWTM         ! Crown weight at maturity       g/p
      REAL          CLAPC(10)     ! Canopy lf area % down to ht    %
      REAL          CLOUDS        ! Cloudiness factor,relative,0-1 #
      INTEGER       CN            ! Crop component (multicrop)     #
      REAL          CNAD          ! Canopy nitrogen                kg/ha
      REAL          CNADPREV      ! Canopy nitrogen,previous day   kg/ha
      REAL          CNADSTG(20)   ! Canopy nitrogen,specific stage kg/ha
      REAL          CNAM          ! Canopy N at harvest            kg/ha
      REAL          CNAMERR       ! Canopy N,harvest,error         %
      REAL          CNAMM         ! Canopy N,harvest,measured      kg/ha
      REAL          CNCTMP        ! Canopy N concentration,tempry  %
      INTEGER       CNI           ! Crop component,initial value   #
      REAL          CNPCHM        ! Canopy N,harvest,measured      %
      REAL          CO2           ! CO2 concentration in air       vpm
      REAL          CO2AIR        ! CO2 concentration in air       g/m3
      REAL          CO2COMPC      ! CO2 compensation conc (vpm)    #
      REAL          CO2CAV        ! Average co2 for crop cycle     vpm
      REAL          CO2CC         ! CO2 sum for cycle              vpm
      REAL          CO2EX         ! Exponent for CO2-phs function  #
      REAL          CO2FP         ! CO2 factor,photosynthesis      #
      REAL          CO2FPI        ! CO2 factor,phs,internal Co2    #
      REAL          CO2F(10)      ! CO2 factor rel values 0-2      #
      REAL          CO2INT        ! CO2 concentration,internal     g/m3
      REAL          CO2INTPPM     ! CO2 concentration,internal     ppm
      REAL          CO2INTPPMP    ! CO2 concentration,internal,prv ppm
      REAL          CO2MAX        ! CO2 conc,maximum during cycle  vpm
      REAL          CO2PAV(0:12)  ! CO2 concentration in air       g/m3
      REAL          CO2PC         ! CO2 concentration,tier,cumul   ppm
      REAL          CO2RF(10)     ! CO2 reference concentration    vpm
      INTEGER       COLNUM        ! Column number                  #
      INTEGER       CSIDLAYR      ! Layer # output from function   #
      INTEGER       CSTIMDIF      ! Time difference function       #
      REAL          CSVPSAT       ! Vapour pressure function op    mb
      INTEGER       CSYDOY        ! Yr+Doy output from function    #
!     INTEGER       CSYEARDOY     ! Year+Doy from function         #
      REAL          CSYVAL        ! Y value from function          #
      INTEGER       CTRNUMPD      ! Control # missing tiers        #
      REAL          CUMDEP        ! Cumulative depth               cm
      REAL          CUMDU         ! Cumulative development units   #
      REAL          CUMSW         ! Soil water in depth SWPLTD     cm
      REAL          CWAD          ! Canopy weight                  kg/ha
      INTEGER       CWADCOL       ! Column number for canopy wt    #
      REAL          CWADPREV      ! Canopy weight,previous day     kg/ha
      REAL          CWADSTG(20)   ! Canopy weight,particular stage kg/ha
      REAL          CWADT         ! Canopy weight from t file      kg/ha
      REAL          CWAHC         ! Canopy weight harvested,forage kg/ha
      REAL          CWAHCM        ! Canopy wt harvested,forage,mes kg/ha
      REAL          CWAM          ! Canopy weight,maturity         kg/ha
      REAL          CWAMERR       ! Canopy weight,maturity,error   %
      REAL          CWAMM         ! Canopy wt,maturity,measured    kg/ha
      REAL          CWAN(HANUMX)  ! Canopy wt minimum after harvst kg/ha
      INTEGER       DAE           ! Days after emergence           d
      INTEGER       DAP           ! Days after planting            d
      INTEGER       DAPCALC       ! DAP output from funcion        #
      INTEGER       DAS           ! Days after start of simulation d
      INTEGER       DATE          ! Date (Yr+Doy)                  #
      INTEGER       DATECOL       ! Date column number             #
      REAL          DAYL          ! Daylength,6deg below horizon   h
      REAL          DAYLCAV       ! Daylength (6deg) av for cycle  h
      REAL          DAYLCC        ! Daylength,cycle sum            h.d 
      REAL          DAYLPAV(0:12) ! Daylength (6deg) av for tier   h
      REAL          DAYLPC        ! Daylength (6deg),cumulative    h
      REAL          DAYLPREV      ! Daylength previous day         h
      REAL          DAYLS(0:10)   ! Daylength sensitivity,tier     %/10h
      REAL          DAYLST(0:12)  ! Daylength (6deg) at stage      h
      REAL          DAYSUM        ! Days accumulated in month      #
      INTEGER       DCDAT(DCNX)   ! Disease control application YrDoy
      REAL          DCDUR(DCNX)   ! Disease control duration       d
      REAL          DCFAC(DCNX)   ! Disease control gr factor 0-1  #
      INTEGER       DCTAR(DCNX)   ! Disease control target         #
      REAL          SENTOPLITTER  ! Dead wt.to litter              g/p
      REAL          SENTOPLITTERA ! Dead wt.to litter              kg/ha
      REAL          DEPMAX        ! Maximum depth of soil profile  cm
      REAL          DEWDUR        ! Dew duration                   h
      REAL          DF            ! Daylength factor 0-1           #
      REAL          DFNEXT        ! Daylength factor,next tier     #
      REAL          DFOUT         ! Daylength factor for output    #
      REAL          DFPE          ! Development factor,pre-emerge  #
      REAL          DALF(0:LNUMX) ! Days during which leaf active  d
      REAL          DGLF(0:LNUMX) ! Days during which leaf growing #
      REAL          DSLF(0:LNUMX) ! Days during which leaf senesng #
      INTEGER       DIDAT(DINX)   ! Disease initiation date YrDoy  d
      INTEGER       DIDOY(DINX)   ! Disease infestation doy        d
      REAL          DIFFACR(DINX) ! Dis favourability requirement  #
      REAL          DIGFAC(DINX)  ! Disease growth factor 0-1      #
      REAL          DLAYR(20)     ! Depth of soil layers           cm
      REAL          DLAYRTMP(20)  ! Depth of soil layers with root cm
      REAL          DMP_EP        ! Dry matter per unit EP         g/mm
      REAL          DMP_ET        ! Dry matter per unit ET         g/mm 
      REAL          DMP_Irr       ! Dry matter per unit irrigation g/mm
      REAL          DMP_NApp      ! Dry matter per unit N applied  kg/kg
      REAL          DMP_NUpt      ! Dry matter per unit N taken uo kg/kg
      REAL          DMP_Rain      ! Dry matter per unit water      g/mm
      INTEGER       DOM           ! Day of month                   #
      INTEGER       DOY           ! Day of year                    d
      INTEGER       DOYCOL        ! Day of year column number      #
      REAL          DRAIN         ! Drainage from soil profile     mm/d
      REAL          DRAINC        ! Drainage from profile,cumulat  mm  
      REAL          DSTAGE        ! Development stage,linear       #
      REAL          DTRY          ! Effective depth of soil layer  cm
      REAL          DU            ! Developmental units            PVC.d
      REAL          DUL(20)       ! Drained upper limit for soil   #
      REAL          DUNEED        ! Developmental units needed tir PVC.d
      REAL          DUPHASE       ! Development units,current tier PVoCd
      REAL          DUPNEXT       ! Development units,next tier    PVoCd
      REAL          DUTOMSTG      ! Developmental units,germ->mat  Du
      INTEGER       DYNAMIC       ! Program control variable       #
      INTEGER       DYNAMICPREV   ! Program control varbl,previous #
      REAL          EARLYN        ! Leaf # for early N switch      # 
      REAL          EARLYW        ! Leaf # for early h20 switch    # 
      INTEGER       EDAP          ! Emergence DAP                  d
      REAL          EDAPFR        ! Emergence DAP+fraction         #
      INTEGER       EDAPM         ! Emergence DAP measured         #
      INTEGER       EDATM         ! Emergence date,measured (Afle) #
      INTEGER       EDATMX        ! Emergence date,measured (YrDy) #
      REAL          EDAYFR        ! Emergence day fraction         #
      INTEGER       EMDATERR      ! Emergence date error d         #
      REAL          EMRGFR        ! Fraction of day > emergence    #
      REAL          EMRGFRPREV    ! Fraction of day > em,previous  #
      REAL          EO            ! Potential evaporation          mm/d
      REAL          EOC           ! Potential evap,cumulative      mm 
      REAL          EOEBUD        ! Potential evap,Ebudget         mm/d
      REAL          EOEBUDC       ! Potential evap,Ebudget,cum     mm
      REAL          EOEBUDCRP     ! Potential evaporation,with res mm/d
      REAL          EOEBUDCRPCO2  ! Potential evaporation,res+co2  mm/d
      REAL          EOEBUDCRPCO2H2O ! Potential evap,res+co2+h2o   mm/d
      REAL          EOEBUDCRPC    ! Potential evaporation,res,cum  mm/d
      REAL          EOEBUDCRPCO2C ! Potential ev,res+co2,cum       mm/d
      REAL          EOEBUDCRPCO2H2OC ! Pot.ev,res+co2+h2o,cum      mm/d
      REAL          EOMPEN        ! Potential ev,M-Penman          mm/d
      REAL          EOMPENC       ! Potential ev,M-Penman,cum      mm  
      REAL          EOMPCRP       ! Potential ev,M-P with crop res mm/d
      REAL          EOMPCRPCO2    ! Potential ev,M-P,res+co2       mm/d
      REAL          EOMPCRPCO2H2O ! Potential ev,M-P,res+co2+h2o   mm/d
      REAL          EOMPCRPC      ! Potential ev,M-P with res,cum  mm/d
      REAL          EOMPCRPCO2C   ! Potential ev,M-P,res+co2,cum   mm/d
      REAL          EOMPCRPCO2H2OC ! Pot.ev,M-P,res+co2+h2o,cum    mm/d
      REAL          EOP           ! Potential evaporation,plants   mm/d
      REAL          EOPEN         ! Potential evaporation,Penman   mm/d
      REAL          EOPENC        ! Potential evaporation,Pen,cum  mm  
      REAL          EOPT          ! Potential evaporation,PT       mm/d
      REAL          EOPTC         ! Potential evaporation,PT,cum   mm  
      REAL          EP            ! Transpiration daily            mm/d 
      REAL          EPCC          ! Transpiration cycle sum        mm 
      REAL          EPPC(0:12)    ! Transpiration cycle sum        mm 
      REAL          EPSRATIO      ! Function,plant/soil evap rate  #
!     INTEGER       ERRNUM        ! Error number from compiler     #
      REAL          ERRORVAL      ! Plgro-tfile values/Plgro       #
      REAL          ES            ! Actual soil evaporation rate   mm/d
      REAL          ET            ! Evapotranspiration daily       mm/d 
      REAL          ETCC          ! Evapotranspiration cumulative  mm 
      REAL          ETPC(0:12)    ! Evapotranspiration tier sum    mm 
      INTEGER       EVALOUT       ! Evaluate output lines for exp  #
      INTEGER       EVHEADNM      ! Number of headings in ev file  #
      INTEGER       EVHEADNMMAX   ! Maximum no headings in ev file #
      INTEGER       EYEARDOY      ! Emergence Year+DOY             #
      REAL          FAC(20)       ! Factor ((g/Mg)/(kg/ha))        #
      INTEGER       FAPPNUM       ! Fertilization application number
      INTEGER       FDAY(200)     ! Dates of fertilizer appn       YrDoy
      REAL          FERNIT        ! Fertilizer N applied           kg/ha
      REAL          FERNITPREV    ! Fertilizer N applied to ystday kg/ha
      INTEGER       FILELEN       ! Length of file name            #
      INTEGER       FLDAP         ! Final leaf date                Yrdoy
      REAL          FLN           ! Final leaf #                   #
      REAL          FNH4          ! Unitless ammonium supply index #
      REAL          FNO3          ! Unitless nitrate supply index  #
      INTEGER       FNUMERA       ! File number,A-data errors      #
      INTEGER       FNUMERR       ! File number,error file         #
      INTEGER       FNUMERT       ! File number,T-data errors      #
      INTEGER       FNUMEVAL      ! Number used for evaluate op    #
      INTEGER       FNUMLVS       ! File number,leaves             #
      INTEGER       FNUMMEAS      ! Number used for measured data  #
      INTEGER       FNUMOV        ! Number used for overview op    #
      INTEGER       FNUMPHA       ! File number,tiers              #
      INTEGER       FNUMPHEM      ! File number,phenology,measured #
      INTEGER       FNUMPHES      ! File number,phenology,simulate #
      INTEGER       FNUMPREM      ! File number,measured responses #
      INTEGER       FNUMPRES      ! File number,simulated response #
      INTEGER       FNUMPSUM      ! Number used for plant summary  #
!      INTEGER       FNUMREA       ! File number,reads.out file     #
      INTEGER       FNUMT         ! Number used for T-file         #
      INTEGER       FNUMTMP       ! File number,temporary file     #
      INTEGER       FNUMWRK       ! File number,work file          #
      INTEGER       FROP          ! Frquency of outputs,as sent    d
      INTEGER       FROPADJ       ! Frquency of outputs,adjusted   d
      REAL          FSOILH2O      ! Final soil water               cm   
      REAL          FSOILN        ! Final soil inorganic N         kg/ha
      INTEGER       GDAP          ! Germination DAP                d
      REAL          GDAPFR        ! Germination DAP+fr             #
      INTEGER       GDAPM         ! Germination DAP,measured       #
      INTEGER       GDATM         ! Germination date,measured      #
      REAL          GDAYFR        ! Fraction of day to germination #
      REAL          GEDAYSE       ! Period germination->emergence  d
      REAL          GEDAYSG       ! Period planting->germination   d
      REAL          GERMFR        ! Fraction of day > germination  #
      REAL          GESTAGE       ! Germination,emergence stage    #
      REAL          GESTAGEPREV   ! Germ,emerg stage,previous day  #
      REAL          GEUCUM        ! Cumulative germ+emergence unit #
      REAL          GROCR         ! Crown growth rate              g/p
      REAL          GROCRADJ      ! Crown growth rate N adjusted   g/p
      REAL          GROCRFR       ! Crown growth rate,fraction st  #
      REAL          GROLF         ! Leaf growth rate               g/p
      REAL          GROLFADJ      ! Leaf growth rate N adjusted    g/p
!     REAL          GROLFNEEDED   ! Leaf growth rate for max SLA   g/p
      REAL          GROLFP        ! Leaf growth,potential          g/p
      REAL          GROLS         ! Leaf+stem growth               g/p
      REAL          GROLSA        ! Leaf+stem gr from assimilates  g/p
      REAL          GROLSP        ! Leaf+stem growth potential     g/p
      REAL          GROLSRS       ! Leaf+stem growth from reserves g/p
      REAL          GROLSRT       ! Leaf+stem gr from root dmatter g/p
      REAL          GROLSRTN      ! Leaf+stem N growth from root N g/p
      REAL          GROLSSEN      ! Leaf+stem growth from senesnce g/p
      REAL          GROLSSD       ! Leaf+stem gr from seed         g/p
      REAL          GRORS         ! Reserves growth                g/p
!     REAL          GRORSADJ      ! Reserves gr,adj to conc        g/p
      REAL          GROSR         ! Storage root growth            g/p
      REAL          GROST         ! Stem growth rate               g/p
      REAL          GROSTADJ      ! Stem growth rate N adjusted    g/p
      REAL          GROSTCR       ! Stem+crown growth rate         g/p
      REAL          GROSTCRP      ! Stem+crown growth potential    g/p 
      REAL          GROSTCRPSTORE ! Stem+crown gr potentl,previous g/p 
      REAL          GRP_ET        ! Harvest product per unit water g/mm
      REAL          GRP_Rain      ! Harvest product per unit water g/mm
      INTEGER       GSTDCOL       ! Growth stage column number     #
      INTEGER       GYEARDOY      ! Germination Year+DOY           #
      REAL          GrP_EP        ! Harvest product per unit EP    g/mm
      REAL          GrP_Irr       ! Harvest dm per unit irrigation g/mm
      REAL          GrP_NApp      ! Harvest dm per unit N appllied kg/kg
      REAL          GrP_NUpt      ! Harvest dm per unit N taken up kg/kg
      REAL          H2OA          ! Water available in root zone   mm
      REAL          H2OCF         ! Water 'potential' factor N upt mm
      REAL          H2OPROFILE    ! Total h2o in soil profile      mm   
      REAL          H2OROOTZONE   ! Total h2o in root zone         mm   
      INTEGER       HADOY         ! Harvest day of year            d
      REAL          HAFR          ! Harvested fraction             kg/ha
      REAL          HAMT(HANUMX)  ! Harvest amount                 #
      INTEGER       HANUM         ! Harvest instruction number     # 
      REAL          HARVFRAC(2)   ! Harvest fraction as brought in #
      REAL          HAWAD         ! Harvested weight (grazing,etc) kg/ha
      INTEGER       HAYEAR        ! Harvest year                   #
      REAL          HBPC(HANUMX)  ! Harvest by-product percentage  #
      REAL          HBPCF         ! Harvest by-product %,final     #
      INTEGER       HDAY          ! Harvest day as read            #
      INTEGER       HDOYF         ! Earliest doy for harvest       #
      INTEGER       HDOYL         ! Last doy for harvest           #
      REAL          HDUR          ! Hardening duration,days        d
      INTEGER       HFIRST        ! Earliest date for harvest      #
      REAL          HIAD          ! Harvest index,above ground     #
      INTEGER       HIADCOL       ! Harvest index column number    #
      REAL          HIADT         ! Harvest index from t file      #
      REAL          HIAM          ! Harvest index,above ground,mat #
      REAL          HIAMERR       ! Harvest index,maturity,error   %
      REAL          HIAMM         ! Harvest index,mature,measure   #
      REAL          HIAMMTMP      ! Harvest index,mature,temporary #
      REAL          HIND          ! Harvest index,N,above ground   #
      REAL          HINM          ! Harvest index,N,abground,mat   #
      REAL          HINMM         ! Harvest N index,mature,meas    %
      INTEGER       HLAST         ! Last date for harvest          #
      REAL          HMPC          ! Harvest moisture percent,std.. # 
      REAL          HNAD          ! Product N                      kg/ha
      REAL          HNAM          ! Harvest N at maturity          kg/ha
      REAL          HNAMERR       ! Harvest N,error                %
      REAL          HNAMM         ! Harvest N,harvest,measured     kg/ha
      REAL          HNC           ! Product N concentration,fr     #
      REAL          HNPCM         ! Harvest N%,maturity            %
      REAL          HNPCMERR      ! Harvest N%,error               %
      REAL          HNPCMM        ! Harvest N,mature,measured      %
      INTEGER       HNUMACOL      ! Harvest number per area column #
      REAL          HNUMAD        ! Harvest product#/unit area     #/m2
      REAL          HNUMAERR      ! Harvest #,maturity,error       %
      REAL          HNUMAM        ! Harvest no/area,maturity       #/m2
      REAL          HNUMAMM       ! Harvest no/area,mature,measure #/m2
      REAL          HNUMAT        ! Harvest number/area,t file     #/m2
      INTEGER       HNUMBER       ! Number of harvest instructions #
      INTEGER       HNUMECOL      ! Harvest number per ear column  #
      REAL          HNUMET        ! Harvest number/ear,t file      #/s
      REAL          HNUMGERR      ! Harvest #/group,error          %
      REAL          HNUMGM        ! Harvest #,mature               #/g
      REAL          HNUMGMM       ! Harvest #,mature,measured      #/g
      REAL          HNUMPM        ! Product# per plant,maturity    #/p
      REAL          HNUMPMM       ! Product #,mature,measured      #/p
      REAL          HPC(HANUMX)   ! Harvest percentage             %
      REAL          HPCF          ! Harvest percentage,final       %
      REAL          HPRODN        ! Harvest product N              g/p
      REAL          HSTAGE        ! Hardening stage  0-1           #
      INTEGER       HSTG          ! Harvest maturity stage         #
      REAL          HWAD          ! Product weight                 kg/ha
      INTEGER       HWADCOL       ! Product wt column number       #
      REAL          HWADT         ! Harvest weight from t file     kg/ha
      REAL          HWAHERR       ! Harvest wt,harvest,error       %
      REAL          HWAHM         ! Harvest wt,harvest,measured    kg/ha
      REAL          HWAM          ! Harvest product wt.,maturity   kg/ha
      REAL          HWAMM         ! Harvest product wt.,measured   kg/ha
      INTEGER       HWTUCOL       ! Harvest weight per unit column #
      REAL          HWUD          ! Harvest wt/unit                g
      REAL          HWUM          ! Harvest product size,maturity  g
      REAL          HWUMERR       ! Harvest wt per unit error      %
      REAL          HWUMM         ! Hprod wt/unit,mat,measured     g
      REAL          HWUMYLD       ! Harvest wt,mature,calculated   g/#
      REAL          HWUT          ! Product weight/unit,t file     mg
      REAL          HYAMM         ! Harvest product,msured,std.h2o kg/ha
      INTEGER       HYEAR         ! Harvest year as read           #
      INTEGER       HYEARDOY(HANUMX)! Dates of harvest operations    #
      INTEGER       HYEARF        ! Earliest year for harvest      #
      INTEGER       HYEARL        ! Last year for harvest          #
      INTEGER       HYRDOY(HANUMX)! Dates of harvest operations    #
      INTEGER       I             ! Loop counter                   #
      REAL          ICWD          ! Initial water table depth      cm
      INTEGER       IDETGNUM      ! Number of times into IDETG     #
      REAL          IRRAMT        ! Irrigation amount for today    mm
      REAL          IRRAMTC       ! Irrigation amount,cumulative   mm
      REAL          ISOILH2O      ! Initial soil water             cm   
      REAL          ISOILN        ! Initial soil inorganic N       kg/ha
      REAL          KCAN          ! Extinction coeff for PAR       #
      REAL          KCANI         ! Extinction coeff,PAR,init.val. #
      REAL          KEP           ! Extinction coeff for SRAD      #
      REAL          KEPI          ! Extinction coeff,SRAD,init val #
      INTEGER       KEYPS(KEYSTX) ! Principal key stage number     #
      INTEGER       KEYPSNUM      ! Principal key stage total #    # 
      INTEGER       L             ! Loop counter                   #
      INTEGER       L1            ! Loop counter                   #
      INTEGER       L2            ! Loop counter                   #
      REAL          LA1S          ! Area of leaf 1,standard        cm2
      REAL          LAFND         ! Node # (one axis)->final area  #
      REAL          LAFS          ! Leaf area/all nodes,final      cm2
      REAL          LAGL(1,LNUMX) ! Leaf area growth,shoot,lf pos  cm2/l
      REAL          LAGETT(0:LNUMX) ! Leaf age at leaf position    C.d
      REAL          LAGEG(0:LNUMX)! Leaf age increment             C.d
      REAL          LAGEP(0:LNUMX)! Leaf age (phyllochrons),lf pos #
      REAL          LAI           ! Leaf area index                #
      REAL          LAIA          ! Leaf area index,active         #
      INTEGER       LAIDCOL       ! Leaf area index column         #
      REAL          LAIL(30)      ! Leaf area index by layer       m2/m2
      REAL          LAILA(30)     ! Leaf area index,active,by layr m2/m2
      REAL          LAIPREV       ! Leaf area index,previous day   #
      REAL          LAIPROD       ! Leaf area index produced       #
      REAL          LAISTG(20)    ! Leaf area index,specific stage #
      REAL          LAIX          ! Leaf area index,maximum        #
      REAL          LAIXERR       ! Leaf area index,max,error      %
      REAL          LAIXM         ! Lf lamina area index,mx,meas   m2/m2
      REAL          LAIXT         ! Leaf area index,max,t-file     m2/m2
      REAL          LAIXX         ! Leaf area index,max posible    #
      REAL          LANC          ! Leaf actual N concentration    #
      REAL          LANCRS        ! Leaf N+rsN concentration       #
      REAL          LAP(0:LNUMX)  ! Leaf area at leaf position     cm2/p
      REAL          LAPD          ! Leaf area (green) per plant    cm2
      REAL          LAPH          ! Leaf area (green) harvested    cm2/d
      REAL          LAPHC         ! Leaf area (green) harvested,cu cm2/p
      REAL          LAPOTX(LNUMX) ! Leaf area potentials,maxima    cm2/l
      REAL          LAPP(LNUMX)   ! Leaf area diseased,leaf posn   cm2/p
      REAL          LAPS(LNUMX)   ! Leaf area senesced,leaf posn   cm2/p
      REAL          LAPSTMP       ! Leaf area senesced,temporary   cm2/p
      REAL          LATL(1,LNUMX) ! Leaf area,shoot,lf#,potential  cm2/l
      REAL          LATL2(1,LNUMX)! Leaf area,shoot,lf#,+h2o,n,tem cm2/l
      REAL          LATL3(1,LNUMX)! Leaf area,shoot,lf#,+assim.    cm2/l
      REAL          LATL4(1,LNUMX)! Leaf area,shoot,lf#,+assim.+N  cm2/l
      REAL          LATLPREV(1,LNUMX)! Leaf area,shoot,leaf,prev.  cm2/l
      REAL          LATLPOT(1,LNUMX)! Leaf area,shoot,leaf,pot     cm2/l
      REAL          LAWCF         ! Leaf area/wt change,fr.st      fr/lf
      REAL          LAWFF         ! Leaf area/wt flexibility,fr.st fr
      REAL          LAWL(2)       ! Area to wt ratio,n=youngest lf cm2/g
      REAL          LAWMNFR       ! Leaf area/wt ratio,min.fr.std. #    
      REAL          LAWS          ! Leaf area/wt ratio,standard    cm2/g
      REAL          LAWTR         ! Leaf area/weight,temp response fr/C
      REAL          LAWTS         ! Leaf area/weight,temp standard C
      REAL          LAWWR         ! Leaf area/weight,water respons fr
      REAL          LAXNO         ! Leaf # (one axis),maximum area #
      REAL          LAXN2         ! Leaf # (one axis),end max.area #
      REAL          LAXS          ! Area of biggest leaf,main stem cm2
      INTEGER       LBIRTHDAP(LCNUMX)! DAP on which leaf initiated #
      INTEGER       LCNUM         ! Leaf cohort number (inc.grow)  #
      REAL          LCOA(LCNUMX)  ! Leaf cohort area               cm2
      REAL          LCOAS(LCNUMX) ! Leaf cohort area senesced      cm2
      INTEGER       LDEATHDAP(LCNUMX)! DAP on which leaf 100% dead #
      REAL          LEAFN         ! Leaf N                         g/p
      REAL          LEAFNEXCESS   ! Leaf N > critical              g/p
      INTEGER       LENDIS        ! Length,ISWDIS flag             #
      INTEGER       LENENAME      ! Length,experiment description  #
      INTEGER       LENGROUP      ! Length of group name           #
      INTEGER       LENLINE       ! Length of character string     #
      INTEGER       LENLINESTAR   ! Length of character string     #
      INTEGER       LENRNAME      ! Length of run description      #
      INTEGER       LENTNAME      ! Length,treatment description   #
!     REAL          LFGSDU        ! Leaf growth start,d.units      #
      REAL          LFWT          ! Leaf weight                    g/p
      REAL          LFWTM         ! Leaf weight,maturity           g/p
!     REAL          LGPHASE(2)    ! Leaf growth tier,start,end     #
!     REAL          LGPHASEDU(2)  ! Leaf growth tier,start,end     Du
!     INTEGER       LINENUM       ! Line number in RMSE values     #
      REAL          LL(20)        ! Lower limit,soil h2o           #
      REAL          LLIFA         ! Leaf life duration,active,read #    
      REAL          LLIFATT       ! Leaf life duration,active      C.d
      REAL          LLIFG         ! Leaf growth duration,as read   #   
      REAL          LLIFGTT       ! Leaf growth durtion            C.d
      REAL          LLIFS         ! Leaf senescence dur,as read    d   
      REAL          LLIFX         ! Leaf ageing acceleration,max   Tu  
      REAL          LLIFXUNUSED   ! Leaf ageing accel.unused       Tu  
      REAL          LLIFSTT       ! Leaf senescence duration,Ttime C.d
      REAL          LLIGP         ! Leaf lignin percentage         #
      REAL          LLNAD         ! Leaf lamina nitrogen           kg/ha
      REAL          LLOSA         ! Leaf area loss,accelerated sen fr
      REAL          LLRSWAD       ! Leaf lamina reserves weight    kg/ha
      REAL          LLRSWT        ! Leaf lamina reserves           g/p
      REAL          LLWAD         ! Leaf lamina weight             kg/ha
      REAL          LLWADOUT      ! Leaf lamina weight for output  kg/ha
      REAL          LNCGL         ! N concentration,growth,lower   fr
      REAL          LNCGU         ! N concentration,growth,upper   fr
      REAL          LNCM          ! Leaf N conc,minimum            fr
      REAL          LNCMN(0:1)    ! Leaf N conc,minimum            fr
      REAL          LNCPL         ! Leaf N concentration,phs,lower fr
      REAL          LNCPU         ! Leaf N concentration,phs,upper fr
      REAL          LNCR          ! Leaf N relative to maximum     #
      REAL          LNCSEN        ! N conc.for senescence          fr
!     REAL          LNCSENF       ! Leaf N con,senescence,final    fr
      REAL          LNCX          ! Leaf N conc,maximum            fr
      REAL          LNCXS(0:1)    ! Leaf N conc,maximum,stage      fr
      REAL          LNDEM         ! Leaf demand for N              g/p
!     REAL          LNDEM2        ! Leaf demand for N for growth   g/p
      REAL          LNDEMG        ! Leaf demand for N,for growth   g/p
      REAL          LNDEMTU       ! Leaf demand for N,for topup    g/p
      REAL          LNPCMN(0:1)   ! Leaf N conc,minimum            %
      REAL          LNPCS(0:1)    ! Leaf N conc,standard,stage     %
      REAL          LNPH          ! Leaf N harvested               g/p
      REAL          LNPHC         ! Leaf N harvested,cumulative    g/p
      REAL          LNUM          ! Leaf number,Haun stage         #
      INTEGER       LNUMCOL       ! Leaf number column             #
      REAL          LNUMEND       ! Leaf number,Haun stage,end day #
      REAL          LNUMG         ! Leaf number increase per day   #
      REAL          LNUMNEED      ! Leaf # stage to start new leaf #
      REAL          LNUMPREV      ! Leaf number,Haun stage         #
      REAL          LNUMSERR      ! Leaf #,error                   %
      INTEGER       LNUMSG        ! Leaf number produced on axis   #
      INTEGER       LNUMSOLDESTA  ! Leaf number,oldest acive,axis  #
      REAL          LNUMSM        ! Leaf #/shoot,Haun,maturity     #
      REAL          LNUMSMM       ! Leaf #,mature,measured         #/s
      REAL          LNUMSTG(20)   ! Leaf number,specific stage     #
      REAL          LNUMT         ! Leaf number from t file        #
      REAL          LNUMTMP       ! Leaf #,temporary val for calc  #
      REAL          LNUMTOSTG(0:PSX) ! Leaf numbers at fork stages #
      REAL          LNUMSIMTOSTG(0:PSX) ! Leaf numbers,fork,simul  #
      REAL          LNUSE(0:3)    ! Leaf N use,overall and parts   g   
      REAL          LPEAI         ! Leaf petiole area index        m2/m2
      REAL          LPEAW         ! Leaf petiole area/wt,veg.ph    cm2/g
      REAL          LPEFR         ! Leaf petiole fraction of total #
      REAL          LPERSWAD      ! Leaf petiole reserves weight   kg/ha
      REAL          LPERSWT       ! Leaf petiole reserves          g/p
      REAL          LPEWAD        ! Leaf petiole weight            kg/ha
      INTEGER       LRTIP         ! Layer with root tip            #
      INTEGER       LSEED         ! Layer with seed                #
      REAL          LSENI         ! Leaf senescence,injury         %/d
      REAL          LWLOS         ! Leaf wt loss,normal senesce    fr
      REAL          LSNUM(HANUMX) ! Livestock number               #/ha
      REAL          LSTAGE        ! Leaf stage 0-1 over leaf tier  #
      REAL          LSWT(HANUMX)  ! Livestock weight (individual)  kg
      REAL          LWPH          ! Leaf weight harvested          g/p
      REAL          LWPHC         ! Leaf wt harvested,cumulative   g/p
      INTEGER       MDAP          ! Maturity date.Days>planting    #
      REAL          MDAPFR        ! Maturity DAP+fraction          #
      INTEGER       MDAPM         ! Maturity DAP,measured          #
      REAL          MDAT          ! Maturity date.Year+DOY         #
      INTEGER       MDATERR       ! Maturity date error            d
      INTEGER       MDATM         ! Maturity date,measured         #
      REAL          MDATT         ! Maturity date from t file      YrDoy
      REAL          MDAYFR        ! Maturity,fraction of day       #
      INTEGER       MDOY          ! Maturity day of year           d
      REAL          MJPERE        ! Energy per Einstein (300-170)  MJ/E
      INTEGER       MSTG          ! Maturity stage(eg.black layer) #
      REAL          NCRG          ! N factor,root growth           ppm
!     REAL          NDEMG         ! N demand for growth min->max   g/p
      REAL          NDEMMN        ! N demand for growth at minimum g/p
      REAL          NDEM2         ! N demand for growth>minimum    g/p
      REAL          NFG           ! N factor,growth 0-1            #
      REAL          NFGCAV        ! N factor,growth,average,cycle  #
      REAL          NFGCC         ! N factor,growh,cycle sum       # 
      REAL          NFGL          ! N factor,gr,lower limit        #
      REAL          NFGPAV(0:12)  ! N factor,growth,average,tier   #
      REAL          NFGPC         ! N factor,growth,cumulative     #
      REAL          NFGU          ! N factor,gr,upper limit        #
      REAL          NFLF(LNUMX)   ! N factor for leaf,average      #
      REAL          NFLF2(0:LNUMX)! N factor for leaf area adj     #
      REAL          NFLFP(LNUMX)  ! N factor phs leaf,average      #
      REAL          NFP           ! N factor,photosynthesis 0-1    #
      REAL          NFPCAV        ! N factor,phs,average,cycle     #
      REAL          NFPCC         ! N factor,phs,cumulative,cycle  #
      REAL          NFPL          ! N factor,phs,lower limit       #
      REAL          NFPPAV(0:12)  ! N factor,phs,average,tier      #
      REAL          NFPPC         ! N factor,phs,cumulative,tier   #
      REAL          NFPU          ! N factor,phs,upper limit       #
      REAL          NFRG          ! N factor,root growth 0-1       #
      REAL          NFSU          ! N factor,senescence,upper lim  #
      REAL          NH4CF         ! NH4 uptake concentration fac   #   
!     REAL          NH4FN         ! NH4 conc factor,NH4 uptake 0-1 #
      REAL          NH4LEFT(20)   ! NH4 concentration in soil      g/Mg
      REAL          NH4MN         ! NH4 conc minimum for uptake    g/Mg
      REAL          NLABPC        ! N labile fraction,standard     %
      INTEGER       NLAYR         ! Number of layers in soil       #
      INTEGER       NLAYRROOT     ! Number of layers with roots    #
      REAL          NLLG          ! N limit,leaf growth            #
      REAL          NO3CF         ! NO3 uptake concentration fac   #    
!     REAL          NO3FN         ! NO3 conc factor,NO3 uptake 0-1 #
      REAL          NO3LEFT(20)   ! NO3 concentration in soil      g/Mg
      REAL          NO3MN         ! NO3 conc minimum for uptake    g/Mg
      INTEGER       NOUTPG        ! Number for growth output file  #
      INTEGER       NOUTPG2       ! Number for growth output file2 #
      INTEGER       NOUTPGF       ! Number for growth factors file #
      INTEGER       NOUTPN        ! Number for growthN output file #
      REAL          NPOOLL        ! Leaf N pool (ie.above minimum) g/p
      REAL          NPOOLR        ! Root N pool (ie.above minimum) g/p
      REAL          NPOOLS        ! Stem N pool (ie.above minimum) g/p
      INTEGER       NSDAYS        ! N stress days                  #
      REAL          NTUPF         ! N top-up fraction              /d
      REAL          NUF           ! Plant N supply/demand,max=1.0  #
      REAL          NULEFT        ! N uptake remaining for use     g 
      REAL          NUPAC         ! N uptake,cumulative            kg/ha
      REAL          NUPACM        ! N uptake,cumulative,measured   kg/ha
      REAL          NUPAD         ! N uptake rate (/d)             kg/ha
      REAL          NUPAP         ! Total root N uptake rate,potnl kg/ha
      REAL          NUPAPCSM      ! Total N uptake rate,potnl,CSM  kg/ha
      REAL          NUPAPCSM1     ! Total N uptake rate,pot,CSMmod kg/ha
      REAL          NUPAPCRP      ! Total N uptake rate,potnl,CSCR kg/ha
      REAL          NUPC          ! N uptake,cumulative            g/p
      REAL          NUPD          ! N uptake                       g/p
      REAL          NUPRATIO      ! N uptake potential/demand      #
      REAL          NUSEFAC       ! N use factor                   #
      INTEGER       ON            ! Option number (sequence runs)  #
      INTEGER       ONI           ! Option number,initial value    #
      INTEGER       OUTCOUNT      ! Output counter                 #
      INTEGER       OUTCHOICE     ! Output choice (+reserves,etc)  #
      REAL          PARFC         ! Max photosynthesis/phs at 330  #
      REAL          PARI          ! PAR interception fraction      #
      REAL          PARI1         ! PAR interception fr,1-crop mdl #
      REAL          PARIOUT       ! PAR interception fr for output #
      REAL          PARIPREV      ! PAR interception fr,previous   #
      REAL          PARIP         ! PAR interception percentage    %
      REAL          PARIPA        ! PAR interception %, active     %
      REAL          PARIUE        ! PAR intercepted use efficiency g/MJ
      REAL          PARIUED       ! PAR intercepted use efficiency g/MJ
      REAL          PARIX         ! PAR interception,maximum,fr    #
      REAL          PARMJC        ! PAR,cumulative                 MJ/m2
      REAL          PARMJFAC      ! PAR conversion factor          MJ/MJ
      REAL          PARMJIADJ     ! PAR intercepted adjustment     MJ/m2
      REAL          PARMJIC       ! PAR intercepted,cumulative     MJ/m2
      REAL          PARU          ! PAR utilization effic          g/MJ
      REAL          PARU2         ! PAR use efficiency,afterchange g/MJ
      REAL          PARUE         ! PAR use efficiency,standard    g/MJ
      REAL          PARUEC        ! PAR use efficiency to today    g/MJ
!     REAL          PARURFR       ! PAR utilize factor,reprod fr   #
      INTEGER       PATHL         ! Path length                    #
      REAL          PD(0:PSX)     ! Tier durations                 deg.d
!     REAL          PD2ADJ        ! Tier 2+3 adjusted              deg.d
      REAL          PDADJ         ! Tier duration adjustment       deg.d
      INTEGER       PDATE         ! Planting Yrdoy from X-file     #
      INTEGER       PDAYS(0:12)   ! Tier durations                 PVoCd
!     REAL          PDFS          ! Final senescenc duration       deg.d
      REAL          PDL(0:PSX)    ! Tier durations,phint units     #    
      REAL          DUSRI         ! Degree days to st.root init.   oC.d
      REAL          PECM          ! Emergence duration             Cd/cm
      REAL          PEGD          ! Duration,germ+dormancy         deg.d
      REAL          PFGCAV        ! P factor,growh,cycle,av 0-1    # 
      REAL          PFGPAV(0:12)  ! P factor,growh,tier,av 0-1     # 
      REAL          PFPCAV        ! P factor,phs,cycle,average 0-1 #
      REAL          PFPPAV(0:12)  ! P factor,phs,tier,average 0-1  #
      INTEGER       PGDAP         ! Plantgro file days after plt   #
      REAL          PGERM         ! Germination duration           deg.d
      INTEGER       PGROCOL(20)   ! Plantgro column = t-file data  #
      REAL          PGVAL         ! Plantgro file value            #
      REAL          PHINT         ! Phylochron interval            deg.d
      REAL          PHINTFAC      ! Phylochron interval factor     #
      REAL          PHINTS        ! Phylochron interval,standard   deg.d
!     INTEGER       PHINTSTG      ! Phylochron stage               #    
      REAL          PHOTQR        ! Photon requirement,calculated  E/mol
      REAL          PHSV          ! Phs,fr reduction with VPD      /KPa
      REAL          PHTV          ! Phs,threshold VPD for reductin KPa
      REAL          PLA           ! Plant leaf area                cm2/p
      REAL          PLAGS2        ! Plant lf area g,all shts,H2oNT cm2/p
      REAL          PLAGSB2       ! Plant lf area g,sh+br,H2oNT    cm2/p
      REAL          PLAGSB3       ! Plant lf area g,sh+br,H2oNTA   cm2/p
      REAL          PLAGSB4       ! Plant lf area g,sh+br,H2oNTAN2 cm2/p
      REAL          PLAS          ! Leaf area senesced,normal      cm2/p
      REAL          PLASI         ! Leaf area senesced,injury      cm2/p
      REAL          PLASL         ! Leaf area senesced,low light   cm2/p
      REAL          PLASN         ! Leaf area senesced,N shortage  cm2/p
      REAL          PLASP         ! Leaf area senesced,phyllochron cm2/p
      REAL          PLASS         ! Leaf area senesced,stress      cm2/p
      REAL          PLASTMP       ! Leaf area senesced,temporary   cm2/p
      REAL          PLASTMP2      ! Leaf area senesced,temporary   cm2/p
      REAL          PLASW         ! Leaf area senesced,h2o stress  cm2/p
      REAL          PLAX          ! Plant leaf area,maximum        cm2
      INTEGER       PLDAY         ! Planting day of year           d
      INTEGER       PLDAYTMP      ! Planting day of year           #
      REAL          PLMAGE        ! Planting material age          d
      INTEGER       PLTOHARYR     ! Planting to final harvestyears #
      REAL          PLPH          ! Plants/hill or shoots/cutting  # 
      REAL          PLTPOP        ! Plant Population               #/m2
      REAL          PLTPOPE       ! Plant Population established   #/m2
      REAL          PLTPOPP       ! Plant Population planned       #/m2
      INTEGER       PLYEAR        ! Planting year                  #
      INTEGER       PLYEARDOY     ! Planting year*1000+DOY         #
      INTEGER       PLYEARDOYPREV ! Year+Doy for planting,previous #
      INTEGER       PLYEARDOYT    ! Planting year*1000+DOY target  #
      INTEGER       PLYEARREAD    ! Planting year as read          #
      INTEGER       PLYEARTMP     ! Year(Yr)+Doy,planting tem val  #
      REAL          PPEXP         ! Photoperiod response exponent  #
      REAL          PPTHR         ! Photoperiod threshold          h
      INTEGER       PSDAP  (PSX)  ! Stage DAP                      #
      REAL          PSDAPFR(PSX)  ! Stage DAP+fr                   #
      INTEGER       PSDAPM (PSX)  ! Stage DAP,measured             #
      INTEGER       PSDAT  (PSX)  ! Stage YrDoydate                #
      INTEGER       PSDATM (PSX)  ! Stage date,measured            #
      REAL          PSDAYFR(PSX)  ! Stage fraction of day          #
      INTEGER       PSIDAP        ! Principal stage,inter,date     dap
      INTEGER       PSIDAPM       ! Principal stg,inter,measured   dap
      INTEGER       PSIDATERR     ! Principal stage,inter,error    dap 
      INTEGER       PSNUM         ! Principal stage number         #
      REAL          PSTART(0:PSX) ! Principal thresholds           du
      REAL          PTF           ! Partition fraction to tops     #
      REAL          PTFA          ! Partition fr adjustment coeff. #
      REAL          PTFMN         ! Partition fraction,minimum     #
      REAL          PTFMX         ! Partition fraction,maximum     #
      REAL          PTTN          ! Minimum soil temperature,plt   C
      REAL          PTX           ! Maximum soil temperature,plt   C
      INTEGER       PWDINF        ! First YrDoy of planting window #
      INTEGER       PWDINL        ! Last YrDoy of planting window  #
      INTEGER       PWDOYF        ! First doy of planting window   #
      INTEGER       PWDOYL        ! Last doy of planting window    #
      INTEGER       PWYEARF       ! First year of planting window  #
      INTEGER       PWYEARL       ! Last year of planting window   #
      REAL          RAIN          ! Rainfall                       mm
      REAL          RAINC         ! Rainfall,cumulative            mm
      REAL          RAINCC        ! Precipitation cycle sum        mm 
      REAL          RAINPAV(0:12) ! Rainfall,average for tier      mm
      REAL          RAINPC(0:12)  ! Precipitation tier sum         mm
      REAL          RANC          ! Roots actual N concentration   #
      REAL          RATM          ! Boundary layer,air,resistance  s/m
!     REAL          RB            ! Leaf resistance addition fac   s/m
      REAL          RCROP         ! Stomatal res,crop basis        s/m
      REAL          RDGAF         ! Root depth gr,acceleration fac #
      REAL          RDGS          ! Root depth growth rate,standrd cm/d
      INTEGER       REP           ! Number of run repetitions      #
      REAL          RESCAL(0:20)  ! Residue C at maturity,by layer kg/ha
      REAL          RESCALG(0:20) ! Residue C added,by layer       kg/ha
      REAL          RESLGAL(0:20) ! Residue lignin,maturity,layer  kg/ha
      REAL          RESLGALG(0:20)! Residue lignin added,layer     kg/ha
      REAL          RESNAL(0:20)  ! Residue N at harvest by layer  kg/ha
      REAL          RESNALG(0:20) ! Residue N added,by layer       kg/ha
      REAL          RESPC         ! Respiration,total,cumulative   g/p
      REAL          RESPRC        ! Respiration,roots,cumulative   g/p
      REAL          RESPTC        ! Respiration,tops,cumulative    g/p
      REAL          RESWAL(0:20)  ! Residue om added by layer      kg/ha
      REAL          RESWALG(0:20) ! Residue om,maturity,by layer   kg/ha
      REAL          RFAC          ! Root length & H2O fac,N uptake #
      REAL          RLDF(20)      ! Root length density fac,new gr #
      REAL          RLF           ! Leaf stomatal res,330.0 ppmCO2 s/m
      REAL          RLFC          ! Leaf stomatal resistance       s/m
!     REAL          RLFN          ! Root length factor,N           #
      REAL          RLFWU         ! Root length factor,water uptk  /cm2
      REAL          RLIGP         ! Root lignin concentration      %
      REAL          RLV(20)       ! Root length volume by layer    cm-2
      REAL          RLWR          ! Root length/weight ratio     m/10mg
      REAL          RM            ! Mesophyll resistance           d/m
!     REAL          RMSE(30)      ! Root mean square error values  #
      INTEGER       RN            ! Treatment replicate            #
      REAL          RNAD          ! Root N                         kg/ha
      REAL          RNAM          ! Root N at maturity             kg/ha
      REAL          RNAMM         ! Root N at maturity,measured    kg/ha
      REAL          RNCM          ! Root N conc,minimum            fr
      REAL          RNCMN(0:1)    ! Root N conc,minimum            fr
      REAL          RNCR          ! Roots N relative to maximum    #
      REAL          RNCX          ! Root N concentration,maximum   fr
      REAL          RNCXS(0:1)    ! Roots N conc,maximum,by stage  fr
      REAL          RNDEM         ! Root demand for N              g/p
      REAL          RNDEMG        ! Root demand for N,for growth   g/p
      REAL          RNDEMTU       ! Root demand for N,for topup    g/p
      REAL          RNH4U(20)     ! Potential ammonium uptake      kg/ha
      INTEGER       RNI           ! Replicate number,initial value #
      REAL          RNO3U(20)     ! Potential nitrate uptake       kg/ha
      REAL          RNPCMN(0:1)   ! Root N conc,minimum            %
      REAL          RNPCS(0:1)    ! Roots N conc,standard,by stage %
!     REAL          RNUMX         ! Root N uptake,maximum          mg/cm
      REAL          RNUSE(0:2)    ! Root N use,overall and parts   g   
      REAL          ROOTN         ! Root N                         g/p
      REAL          ROOTNEXCESS   ! Root N > critical              g/p
      REAL          ROOTNS        ! Root N senesced                g/p
      REAL          ROWSPC        ! Row spacing                    cm
      INTEGER       RPCOL         ! Replicate column number        #
      REAL          RRESP         ! Root respiration fraction      #
      REAL          RSCD          ! Reserves concentration,end day fr
      REAL          RSFRS         ! Reserves fr.assim.for stem,std fr 
      REAL          RSCLX         ! Reserves conc,leaves,max.      #
      REAL          RSCM          ! Reserves concentration,mature  fr
      REAL          RSCMM         ! Reserves conc,maturity,msured  #
      REAL          RSPCO         ! Res conc.above which overflow  %
      REAL          RSCX          ! Max.reserves conc.reached      fr
      REAL          RSEN          ! Root senescence fraction       #
      REAL          RSFP          ! Reserves factor,photosynthesis fr
      REAL          RSFPL         ! Reserves conc.,phs.lower bound fr
      REAL          RSFPU         ! Reserves conc.,phs upper bound fr
      REAL          RSN           ! Reserve N                      g/p
      REAL          RSNAD         ! Reserve N                      kg/ha
!     REAL          RSNEED        ! Reserves need to bring to min  g/p
      REAL          RSNPH         ! Reserves N harvested           g/p
      REAL          RSNPHC        ! Reserves N harvested,cum       g/p
      REAL          RSNUSED       ! Reserve N used                 g/p  
!     REAL          RSGLFADJ      ! Reserves from leaf adj to N    g/p
      REAL          RSSRWTGLFADJ  ! Reserves+storage from lf N adj g/p
!     REAL          RSSRWTGSTADJ  ! Reserves+storage from st N adj g/p
      REAL          RSUSE         ! Reserves utilisation fraction  #
      REAL          RSWAD         ! Reserves weight                kg/ha
      REAL          RSWAM         ! Reserves at maturity           kg/ha
      REAL          RSWAMM        ! Reserves at maturity,measured  kg/ha
      REAL          RSWPH         ! Reserves weight harvested      g/p
      REAL          RSWPHC        ! Reserves wt harvested,cum      g/p
      REAL          RSWT          ! Reserves weight                g/p
      REAL          RSWTGLFADJ    ! Reserves from lf N adjustment  g/p
!     REAL          RSWTPREV      ! Reserves weight,previous       g/p
      REAL          RSWTM         ! Reserves weight,maturity       g/p
!     REAL          RSWTTMP       ! Reserves weight,temporary val  g/p
      REAL          RSWTX         ! Reserves weight,maximum        g/p
      REAL          RTDEP         ! Root depth                     cm
      REAL          RTDEPG        ! Root depth growth              cm/d
      REAL          RTDEPTMP      ! Root depth,temporary value     cm/d
      REAL          RTNO3         ! N uptake/root length           mg/cm
      REAL          RTNH4         ! N uptake/root length           mg/cm
      REAL          RTNSL(20)     ! Root N senesced by layer       g/p
      REAL          RTRESP        ! Root respiration               g/p
      REAL          RTRESPADJ     ! Root respiration rate N adj    g/p
      INTEGER       RTSLXDATE     ! Roots into last layer date     YYDDD
      REAL          RTUFR         ! Max fraction root wt useable   fr
      REAL          RTWT          ! Root weight                    g/p
      REAL          RTWTAL(20)    ! Root weight by layer           kg/ha
      REAL          RTWTG         ! Root weight growth             g/p
      REAL          RTWTGADJ      ! Root growth rate N adjusted    g/p
      REAL          RTWTGL(20)    ! Root weight growth by layer    g/p
      REAL          RTWTL(20)     ! Root weight by layer           g/p
      REAL          RTWTM         ! Root weight,maturity           g/p
      REAL          RTWTSL(20)    ! Root weight senesced by layer  g/p
      REAL          RTWTUL(20)    ! Root weight used for tops,lyr  g/p
      INTEGER       RUN           ! Run (from command line) number #
      INTEGER       RUNCRP        ! Run (internal within module)   #
      INTEGER       RUNI          ! Run (internal for sequences)   #
      REAL          RUNOFF        ! Calculated runoff              mm/d
      REAL          RUNOFFC       ! Calculated runoff,cumulative   mm   
      REAL          RWAD          ! Root weight                    kg/ha
      REAL          RWAM          ! Root weight,maturity           kg/ha
      REAL          RWAMM         ! Root wt at maturity,measured   kg/ha
      REAL          RWUMX         ! Root water uptake,max cm3/cm.d cm2.d
      REAL          RWUMXI        ! Root water uptake,max,init.val cm2/d
      REAL          RWUPM         ! Pore size for maximum uptake   fr
      REAL          SAID          ! Stem area index                m2/m2
      REAL          SANC          ! Stem N concentration           #
      REAL          SANCOUT       ! Stem+LeafPetiole N conc        #
      REAL          SAT(20)       ! Saturated limit,soil           #
      REAL          SAWS          ! Stem area to wt ratio,standard cm2/g
      REAL          SDCOAT        ! Non useable material in seed   g
      REAL          SDDUR         ! Seed reserves use duration     d
      REAL          SDEPTH        ! Sowing depth                   cm
      REAL          SDEPTHU       ! Sowing depth,uppermost level   cm
      REAL          SDNAD         ! Seed N                         kg/ha
      REAL          SDNAP         ! Seed N at planting             kg/ha
      REAL          SDNC          ! Seed N concentration           #
      REAL          SDNPCI        ! Seed N concentration,initial   %
      REAL          SDRATE        ! Seeding 'rate'                 kg/ha
      REAL          SDRSF         ! Seed reserves fraction of seed #
      REAL          SDSZ          ! Seed size                      g
      REAL          SDWAD         ! Seed weight                    kg/ha
      REAL          SDWAM         ! Seed at maturity               kg/ha
      REAL          SEEDN         ! Seed N                         g/p
      REAL          SEEDNI        ! Seed N,initial                 g/p
      REAL          SEEDNUSE      ! N use from seed                g
      REAL          SEEDNUSE2     ! N use from seed,supplementary  g
      REAL          SEEDRS        ! Seed reserves                  g/p
      REAL          SEEDRSAV      ! Seed reserves available        g/p
      REAL          SEEDRSAVR     ! Seed reserves available,roots  g/p
      REAL          SEEDRSI       ! Seed reserves,initial          g/p
      REAL          SEEDUSE       ! Seed reserves use              g/p
      REAL          SEEDUSER      ! Seed reserves use,roots        g/p
      REAL          SEEDUSET      ! Seed reserves use,tops         g/p
      REAL          SENCAGS       ! Senesced C added to soil       kg/ha
      REAL          SENCALG(0:20) ! Senesced C added,by layer      kg/ha
      REAL          SENCAS        ! Senesced C added to soil       kg/ha
      REAL          SENCL(0:20)   ! Senesced C,by layer            g/p
      REAL          SENCS         ! Senesced C added to soil       g/p
      REAL          SENFR         ! Senesced fraction lost from pl #
      REAL          SENLA         ! Senesced leaf area,total       cm2/p
      REAL          SENLALITTER   ! Senesced leaf area,litter      cm2/p
      REAL          SENLAGS       ! Senesced lignin added to soil  kg/ha
      REAL          SENLALG(0:20) ! Senesced lignin added,layer    kg/ha
      REAL          SENLAS        ! Senesced lignin added to soil  kg/ha
      REAL          SENLFG        ! Senesced leaf                  g/p
      REAL          SENLFGRS      ! Senesced leaf to reserves      g/p
      REAL          SENLL(0:20)   ! Senesced lignin added,by layer g/p
      REAL          SENLS         ! Senesced lignin added to soil  g/p
      REAL          SENNAGS       ! Senesced N added to soil       kg/ha
      REAL          SENNAL(0:20)  ! Senesced N,by layer            kg/ha
      REAL          SENNALG(0:20) ! Senesced N added,by layer      kg/ha
      REAL          SENNAS        ! Senesced N added to soil       kg/ha
      REAL          SENNATC       ! Senesced N,litter+soil,cum     kg/ha
      REAL          SENNATCM      ! Senesced N,litter+soil,cum,mes kg/ha
      REAL          SENNGS        ! Senesced N added to soil       g/p
      REAL          SENNL(0:20)   ! Senesced N,by layer            g/p
      REAL          SENNLFG       ! Senesced N from leaves         g/p
      REAL          SENNLFGRS     ! Senesced N from leaves,to rs   g/p
      REAL          SENNS         ! Senesced N added to soil       g/p
      REAL          SENRTG        ! Senescent root material growth g/p
!     REAL          SENSTFR       ! Senesced stem fraction         fr/d
      REAL          SENTOPLITTERG ! Senescent top->litter growth   g/p
      REAL          SENWACM       ! Senesced weight,total,cum to m kg/ha
      REAL          SENWACMM      ! Senesced om,litter+soil,cum,ms kg/ha
      REAL          SENWAGS       ! Senesced weight added to soil  kg/ha
      REAL          SENWAL(0:20)  ! Senesced om by layer           kg/ha
      REAL          SENWALG(0:20) ! Senesced om added by layer     kg/ha
      REAL          SENWL(0:20)   ! Senesced om (cumulative),layer g/p
      REAL          SENROOT       ! Senesced weight,soil,cum       g/p
      REAL          SENROOTA      ! Senesced weight,soil,cumulativ kg/ha
      REAL          SERX          ! Shoot elongation rate,max      cm/Du
      REAL          SHF(20)       ! Soil hospitality factor 0-1    #
      INTEGER       SHDAP         ! Shoot prodn.start DAP          #
      INTEGER       SHDAT         ! Shoot prodn.startdate YEARDOY  #
      REAL          SHGR(22)      ! Shoot size relative to 1       #
      REAL          SHLA(25)      ! Shoot leaf area produced       cm2
      REAL          SHLAG2(25)    ! Shoot lf area gr,1 axis,H2oNt  cm2
      REAL          SHLAGB2(25)   ! Shoot lf area gr,+br,H2oNt     cm2
      REAL          SHLAGB3(25)   ! Shoot lf area gr,+br,H2oNtA    cm2
      REAL          SHLAGB4(25)   ! Shoot lf area gr,+br,H2oNtAN2  cm2
      REAL          SHLAS(25)     ! Shoot leaf area senesced       cm2
      REAL          SHNUM         ! Shoot (incl.main stem) number  #/p
      REAL          SHNUMAD       ! Shoot (incl.main stem) number  #/m2
      REAL          SHNUML(LNUMX) ! Shoot # at leaf position       #/p
      REAL          SHRTD         ! Shoot/root ratio               #
      REAL          SHRTM         ! Shoot/root ratio,maturity      #
      REAL          SHRTMM        ! Shoot/root ratio,maturity,meas #
      REAL          SLA           ! Specific leaf area             cm2/g
      REAL          SLAOUT        ! Specific leaf area for output  cm2/g
      REAL          SLIGP         ! Stem lignin concentration      %
      REAL          SLPF          ! Soil factor for photosynthesis %
!     REAL          SMDFR         ! Soil moisture factor,N uptake  #
      INTEGER       SN            ! Sequence number,crop rotation  #
      REAL          SNAD          ! Stem N (stem+petiole+rs)       kg/ha
      REAL          SNCM          ! Stem N conc,minimum            fr
      REAL          SNCMN(0:1)    ! Stem N conc,minimum            fr
      REAL          SNCR          ! Stem N relative to maximum     #
      REAL          SNCX          ! Stem N conc,maximum            fr
      REAL          SNCXS(0:1)    ! Stem N conc,maximum,stage      fr
      REAL          SNDEM         ! Stem demand for N              g/p
      REAL          SNDEMG        ! Stem demand for N,for growth   g/p
      REAL          SNDEMTU       ! Stem demand for N,for topup    g/p
      REAL          SNH4(20)      ! Soil NH4 N                     kg/ha
      REAL          SNH4PROFILE   ! Soil NH4 N in profile          kg/ha
      REAL          SNH4ROOTZONE  ! Soil NH4 N in root zone        kg/ha
      INTEGER       SNI           ! Sequence number,as initiated   #
      REAL          SNO3(20)      ! Soil NO3 N                     kg/ha
      REAL          SNO3PROFILE   ! Soil NO3 N in profile          kg/ha
      REAL          SNO3ROOTZONE  ! Soil NO3 N in root zone        kg/ha
      REAL          SNPCMN(0:1)   ! Stem N conc,minimum            %
      REAL          SNPCS(0:1)    ! Stem N conc,standard,stage     %
      REAL          SNPH          ! Stem N harvested               g/p
      REAL          SNPHC         ! Stem N harvested,cumulative    g/p
      REAL          SNUSE(0:2)    ! Shoot N use,overall and parts  g    
      REAL          SPRL          ! Sprout/cutting length          cm
      REAL          SRAD          ! Solar radiation                MJ/m2
      REAL          SRAD20        ! Solar radiation av,20 days     MJ/m2
      REAL          SRAD20S       ! Solar radiation sum            MJ/m2
      REAL          SRADC         ! Solar radiation,cumulative     MJ/m2
      REAL          SRADCAV       ! Solar radiation,cycle average  MJ/m2
      REAL          SRADCC        ! Radiation,cycle sum            Mj/m2
      REAL          SRADD(20)     ! Solar radiation on specific d  MJ/m2
      REAL          SRADPAV(0:12) ! Solar radiation,tier average   MJ/m2
      REAL          SRADPC        ! Solar radiation,tier sum       MJ/m2
      REAL          SRADPREV      ! Solar radiation,previous day   MJ/m2
      REAL          SRANC         ! Storage root N concentration   g/p
      REAL          SRDAYFR       ! Storage root fraction of day   #
      REAL          SRFR          ! Storage root fraction,basic    #
      REAL          SRNAD         ! Storage root N                 kg/ha
      REAL          SRNAM         ! Storage root N at maturity     kg/ha
      REAL          SRNDEM        ! Storage root demand for N      g/p
      REAL          SRNDEMG       ! St.rt.demand for N,for growth  g/p
      REAL          SRNDEMTU      ! St.rt.demand for N,for topup   g/p
      REAL          SRNOAD        ! Storage root/group,given day   #/m2
      REAL          SRNOAM        ! Storage root #/area,maturity   #/m2
      REAL          SRNOAMM       ! Storage root/group,mature,meas #
      REAL          SRNOGM        ! Storage root/group,maturity    #
      REAL          SRNOGMM       ! Storage root/group,mature,meas #
      INTEGER       SRNOPD        ! Storage root number per plant  # 
      REAL          SRNOW         ! Cultivar coeff,storage root #  #/g
      REAL          SRNPCM        ! Storage root N%,maturity       %
      REAL          SRNPCS        ! Storage root N%,standard       #
      REAL          SRNUSE(0:2)   ! Storage root N use,total/parts g   
      REAL          SROOTN        ! Storage root N                 g/p
      REAL          SRPRS         ! Storage protein standard %     #
      REAL          SRWAD         ! Storage root weight            kg/ha
      REAL          SRWT          ! Root storage organ weight      g/p
      REAL          SRWTGRS       ! Root storage,reserves>std.%    g/p
      REAL          SRWUD         ! Storage root size              g
      REAL          SRWUM         ! Storage root wt/unit,maturity  g
      REAL          SRWUMM        ! Storage root wt/unit,mat,meas  g
      REAL          ST(0:NL)      ! Soil temperature in soil layer C
!     REAL          STADJ         ! Stem adjustment,N triggered    #
      REAL          STAI          ! Stem area index                m2/m2
      REAL          STAIG         ! Stem area index,growth         m2/m2
      REAL          STAIS         ! Stem area index senesced       m2/m2
!     REAL          STAISS        ! Stem area index,start senesce  m2/m2
      INTEGER       STARNUM       ! Star line number,as read file  #
      INTEGER       STARNUMM      ! Star line number,measured data #
      INTEGER       STARNUMO      ! Star line number,output file   #
!     INTEGER       STARTPOS      ! Starting position in line      #
      REAL          STDAY         ! Standard day                   C.d/d
      REAL          STEMN         ! Stem N                         g/p
      REAL          STEMNEXCESS   ! Stem N > critical              g/p
      INTEGER       STEP          ! Step number                    #
      INTEGER       STEPNUM       ! Step number per day            #
      INTEGER       STGEDAT       ! Stem growth end date (Yrdoy)   #
!     REAL          STGEFR        ! Stem growth end time,fr day    #
      INTEGER       STGYEARDOY(20)! Stage dates (Year+Doy)         #
      REAL          STRESS(20)    ! Min h2o,n factors for growth   #
      REAL          STRESS20      ! 20d av.,min h2o,n gr.factors   #
      REAL          STRESS20N     ! 20d av.,n gr.factor            #
      REAL          STRESS20NS    ! 20d sum,n gr.factors           #
      REAL          STRESS20S     ! 20d sum.,min h2o,n gr.factors  #
      REAL          STRESS20W     ! 20d av.,h2o gr.factor          #
      REAL          STRESS20WS    ! 20d sum,h2o gr.factors         #
      REAL          STRESSN(20)   ! 20d n gr.factors               #
      REAL          STRESSW(20)   ! 20d h2o gr.factors             #
      REAL          STRSWAD       ! Stem reserves                  kg/ha
      REAL          STRSWT        ! Stem reserves                  g/p
      REAL          STWAD         ! Stem structural weight         kg/ha
      REAL          STWADOUT      ! Stem weight for output         kg/ha
      REAL          STWT          ! Stem weight                    g/p
      REAL          STWTM         ! Stem weight,maturity           g/p
      REAL          SW(20)        ! Soil water content             #
      REAL          SWFR          ! Stem fraction,actual           #
      REAL          SWFRN         ! Stem fraction minimum          #
      REAL          SWFRNL        ! Leaf number for min stem fr    #
      REAL          SWFRPREV      ! Stem fraction,actual,previous  #
      REAL          SWFRS         ! Stem fraction,standard         #
      REAL          SWFRX         ! Stem fraction maximum          #
      REAL          SWFRXL        ! Leaf number for max stem fr    #
      REAL          SWP(0:20)     ! Soil water 'potential'         #
      REAL          SWPH          ! Stem weight harvested          g/p
      REAL          SWPHC         ! Stem wt harvested,cumulative   g/p
      REAL          SWPLTD        ! Depth for average soil water   cm
      REAL          SWPLTH        ! Upper limit on soil water,plt  %
      REAL          SWPLTL        ! Lower limit on soil water,plt  %
      REAL          SWPRTIP       ! Soil water potential,root tip  #
      REAL          SWPSD         ! Soil water potential at seed   #
      REAL          TAIRHR(24)    ! Hourly air temperature         C
      REAL          TCAN          ! Canopy temperature             C
      REAL          TCDIF         ! Canopy temperature - air temp  C
      INTEGER       TDATANUM      ! Number of data from t-file     #
      REAL          TDEW          ! Dewpoint temperature           C
      REAL          TDIFAV        ! Temperature difference,can-air C
      REAL          TDIFNUM       ! Temperature difference,# data  #
      REAL          TDIFSUM       ! Temperature difference,sum     C
      REAL          TFAC4         ! Temperature factor function    #
      INTEGER       TFCOLNUM      ! T-file column number           #
      REAL          TFD           ! Temperature factor,development #
      INTEGER       TFDAP         ! T-file days after planting     #
      INTEGER       TFDAPCOL      ! T-file DAP column #            #
      REAL          TFDF          ! Temperature factor,dayl sens   #
      REAL          TFDNEXT       ! Temperature factor,development #
      REAL          TFG           ! Temperature factor,growth 0-1  #
      REAL          TFGEM         ! Temperature factor,germ,emrg   #
      REAL          TFLFLIFE      ! Temperature factor,leaf life   #
      REAL          TFLAW         ! Temperature factor,lf area/wt  #
      REAL          TFGLF(LNUMX)  ! Temp factor,gr for leaf,av     #
      REAL          TFDLF(LNUMX)  ! Temp factor,dev for leaf,av    #
      REAL          TFP           ! Temperature factor,phs 0-1     #
!     REAL          TFV           ! Temperature factor,vernalizatn #
      REAL          TFVAL         ! T-file value                   #
      INTEGER       TIERNUM       ! Tier of data in t-file         #
      REAL          TIMENEED      ! Time needed to finish tier     fr
      REAL          TLCHC         ! Cumulative N leached>planting  kg/ha
      REAL          TLCHD         ! N leached this day             kg/ha
      INTEGER       TLINENUM      ! Temporary var,# lines in tfile #
      INTEGER       TLPOS         ! Position on temporary line     #
      REAL          TMAX          ! Temperature maximum            C
      REAL          TMAXCAV       ! Temperature,maximum,cycle av   C
      REAL          TMAXCC        ! Temperature,max,cycle sum      C.d 
      REAL          TMAXM         ! Temperature maximum,monthly av C
      REAL          TMAXPAV(0:12) ! Temperature,maximum,tier av    C
      REAL          TMAXPC        ! Temperature,maximum,tier  sum  C
      REAL          TMAXSUM       ! Temperature maximum,summed     C
      REAL          TMAXX         ! Temperature max during season  C
      REAL          TMEAN         ! Temperature mean (TMAX+TMIN/2) C
      REAL          TMEAN20       ! Temperature mean over 20 days  C
      REAL          TMEAN20P      ! Temperature mean,20 d>planting C
      REAL          TMEAN20S      ! Temperature sum over 20 days   C
      REAL          TMEANAV(0:12) ! Temperature,mean,tier av       C
      REAL          TMEANCC       ! Temperature,mean,cycle sum     C.d  
      REAL          TMEAND(20)    ! Temperature mean,specific day  C
      REAL          TMEANE        ! Temp mean,germination-emerge   C
      REAL          TMEANEC       ! Temp sum,germination-emergence C
      REAL          TMEANG        ! Temp mean,planting-germination C
      REAL          TMEANGC       ! Temp sum,planting-germination  C
      REAL          TMEANNUM      ! Temperature means in sum       #
      REAL          TMEANPC       ! Temperature,mean,tier sum      C
      REAL          TMEANSUM      ! Temperature means sum          #
      REAL          TMEANSURF     ! Temperature mean,soil surface  C
      REAL          TMIN          ! Temperature minimum            C
      REAL          TMINCAV       ! Temperature,minimum,cycle av   C
      REAL          TMINCC         ! Temperature,min,cycle sum     C.d  
      REAL          TMINM         ! Temperature minimum,monthly av C
      REAL          TMINN         ! Temperature min during season  C
      REAL          TMINPAV(0:12) ! Temperature,minimum,tier av    C
      REAL          TMINPC        ! Temperature,minimum,tier sum   C
      REAL          TMINSUM       ! Temperature minimum,summed     C
      INTEGER       TN            ! Treatment number               #
      REAL          TNAD          ! Total nitrogen (tops+roots)    kg/ha
      REAL          TNAMM         ! Total N at harvest,measured    kg/ha
      INTEGER       TNI           ! Treatment number,initial value #
      REAL          TNIMBSOM      ! Total N immobilised by SOM     kg/ha
      REAL          TNOXC         ! Cumulative N denitrified       kg/ha
      REAL          TNOXD         ! N denitrified this day         kg/ha
      REAL          TOFIXC        ! Cumulative inorganicN fixation kg/ha
      REAL          TOMIN         ! Daily N mineralized            kg/ha
      REAL          TOMINC        ! Cumulative N mineralized       kg/ha
      REAL          TOMINFOM      ! Daily mineralization,FOM       kg/ha
      REAL          TOMINFOMC     ! Cumulative mineralization,FOM  kg/ha
      REAL          TOMINSOM      ! Daily mineralization,SOM       kg/ha
      REAL          TOMINSOM1     ! Daily mineralization,SOM1      kg/ha
      REAL          TOMINSOM1C    ! Cumulative mineralization,SOM1 kg/ha
      REAL          TOMINSOM2     ! Daily mineralization,SOM2      kg/ha
      REAL          TOMINSOM2C    ! Cumulative mineralization,SOM2 kg/ha
      REAL          TOMINSOM3     ! Daily mineralization,SOM3      kg/ha
      REAL          TOMINSOM3C    ! Cumulative mineralization,SOM3 kg/ha
      REAL          TOMINSOMC     ! Cumulative mineralization,SOM  kg/ha
      REAL          TPAR          ! Transmission,PAR,fraction      #
      REAL          TRATIO        ! Function,relative tr rate      #
      REAL          TRDV1(4)      ! Temp response,development 1    #
      REAL          TRDV2(4)      ! Temp response,development 2    #
      REAL          TRGEM(4)      ! Temp response,germ.emergence   #
      REAL          TRLDF         ! Intermediate factor,new roots  #
      REAL          TRLFG(4)      ! Temp response,leaf growth      #
      REAL          TRLV          ! Total root length density      /cm2
      REAL          TRPHS(4)      ! Temp response,photosynthesis   #
      REAL          TRWU          ! Total water uptake             mm
      REAL          TRWUP         ! Total water uptake,potential   cm
      REAL          TSDEP         ! Average temp in top 10 cm soil C
      REAL          TSRAD         ! Transmission,SRAD,fraction     #
      REAL          TT            ! Daily thermal time             C.d
      REAL          TTGEM         ! Daily thermal time,germ,emrg.  C.d
      REAL          TTLFLIFE      ! Daily thermal time,leaf life   C.d
      REAL          TTNEED        ! Thermal time to start new leaf #
      REAL          TT20          ! Thermal time mean over 20 days C
      REAL          TT20S         ! Thermal time sum over 20 days  C
      REAL          TTCUM         ! Cumulative thermal time        C.d
      REAL          TTD(20)       ! Thermal time,specific day      C
      REAL          TTNEXT        ! Thermal time,next tier         oCd
      REAL          TTOUT         ! Thermal units output from func C.d
      INTEGER       TVI1          ! Temporary integer variable     #
      INTEGER       TVI2          ! Temporary integer variable     #
      INTEGER       TVI3          ! Temporary integer variable     #
      INTEGER       TVI4          ! Temporary integer variable     #
      INTEGER       TVICOLNM      ! Column number function output  #
      INTEGER       TVILENT       ! Temporary integer,function op  #
      REAL          TVR1          ! Temporary real variable        #
      REAL          TVR2          ! Temporary real variable        #
      REAL          TVR3          ! Temporary real variable        #
      REAL          TVR4          ! Temporary real variable        #
      REAL          TVR5          ! Temporary real variable        #
      REAL          TVR6          ! Temporary real variable        #
      REAL          TWAD          ! Total weight (tops+roots)      kg/ha
      REAL          UH2O(NL)      ! Uptake of water                cm/d
      REAL          UNH4(20)      ! Uptake of NH4 N                kg/ha
      REAL          UNO3(20)      ! Uptake of NO3 N                kg/ha
      INTEGER       VALUEI        ! Output from Getstri function   #
      REAL          VALUER        ! Output from Getstrr function   #
      REAL          VANC          ! Vegetative actual N conc       #
!     INTEGER       VARNUM(30)    ! Variable number in sum         #
!     REAL          VARSUM(30)    ! Temporary variables sum        #
!     REAL          VARVAL        ! Temporary variable             #
      REAL          VCNC          ! Vegetative critical N conc     #
      INTEGER       CSCAS_VERSION ! Version #                      #
      REAL          VMNC          ! Vegetative minimum N conc      #
      REAL          VNAD          ! Vegetative canopy nitrogen     kg/ha
      REAL          VNAM          ! Vegetative N,mature            kg/ha
      REAL          VNAMM         ! Vegetative N,mature,measured   kg/ha
      REAL          VNPCM         ! Vegetative N %,maturity        %
      REAL          VNPCMM        ! Vegetative N,mature,measure    %
      REAL          VPD           ! Vapour pressure deficit        KPa
      REAL          VPDFP         ! Vapour press deficit factor,phs #
      REAL          VWAD          ! Vegetative canopy weight       kg/ha
      REAL          VWAM          ! Vegetative canopy wt,maturity  kg/ha
      REAL          VWAMERR       ! Vegetative wt,error            %
      REAL          VWAMM         ! Veg wt,mature,measured         kg/ha
      REAL          WAVR          ! Water available/demand         #
      REAL          WFEU          ! Water factor,evaptp,upper      #
      REAL          WFG           ! Water factor,growth 0-1        #
      REAL          WFGCAV        ! Water factor,growth,av.,cylcle #
      REAL          WFGCC         ! H20 factor,growh,cycle sum     #
      REAL          WFGE          ! Water factor,germ,emergence    #
      REAL          WFGEM         ! Water factor,germ,emergence    #
      REAL          WFGL          ! Water factor,growth,lower      #
      REAL          WFGPAV(0:12)  ! Water factor,growth,average    #
      REAL          WFGPC         ! Water factor,growth,cumulative #
      REAL          WFGU          ! Water factor,growth,upper      #
      REAL          WFLAW         ! Water factor,leaf area/weight  #
      REAL          WFLF(LNUMX)   ! H2O factor for leaf,average    #
      REAL          WFNU          ! Water factor,N uptake          #
      REAL          WFP           ! Water factor,photosynthsis 0-1 #
      REAL          WFPCAV        ! Water factor,phs,av 0-1,cycle  #
      REAL          WFPCC         ! H20 factor,phs,cycle sum       # 
      REAL          WFPL          ! Water factor,phs,lower         #
      REAL          WFPPAV(0:12)  ! Water factor,phs,average 0-1   #
      REAL          WFPPC         ! Water factor,phs,cumulative    #
      REAL          WFPU          ! Water factor,phs,upper         #
      REAL          WFRG          ! Water factor,root growth,0-1   #
      REAL          WFRTG         ! Water factor,root gr           #
      REAL          WFSU          ! Water fac,senescence,upper 0-1 #
      REAL          WINDSP        ! Wind speed                     m/s
      INTEGER       WSDAYS        ! Water stress days              #
      REAL          WTDEP         ! Water table depth              cm
      REAL          WUPR          ! Water pot.uptake/demand        #
      REAL          WUPRD(20)     ! Water pot.uptake/demand,ind dy #
      REAL          XDEP          ! Depth to bottom of layer       cm
      REAL          XDEPL         ! Depth to top of layer          cm
      REAL          XMIN          ! Minimum NO3,NH4-N in soil layr kg/ha
      INTEGER       YEAR          ! Year                           #
      INTEGER       YEARCOL       ! Colum number for year data     #
      INTEGER       YEARDOY       ! Year+Doy (7digits)             #
      INTEGER       YEARDOYHARF   ! Harvest year+doy,fixed         #
      INTEGER       YEARDOYPREV   ! Year+Doy (7digits),previous    #
      INTEGER       YEARM         ! Year of measurement            #
      INTEGER       YEARSIM       ! Year+Doy for simulation start  #
      INTEGER       YEARPLTCSM    ! Planting year*1000+DOY,CSM     #
      REAL          YVALXY        ! Y value from function          #

      CHARACTER (LEN=128) ARG           ! Argument component
      CHARACTER (LEN=6)   CAIC          ! Canopy area index
      CHARACTER (LEN=6)   CANHTC        ! Canopy height
      CHARACTER (LEN=120) CFGDFILE      ! Configuration directory+file
      CHARACTER (LEN=1)   CFLAFLF       ! Control flag,assim.on lf area
      CHARACTER (LEN=1)   CFLFAIL       ! Control flag for failure
      CHARACTER (LEN=1)   CFLLFLIFE     ! Control flag,leaf life D,P,C
      CHARACTER (LEN=1)   CFLHAR        ! Control flag for final harvest
      CHARACTER (LEN=1)   CFLHARMSG     ! Control flag,harvest message
!     CHARACTER (LEN=1)   CFLPDADJ      ! Control flag,tire adjustment 
!     CHARACTER (LEN=1)   CFLPRES       ! Control flag for headers,PRES
      CHARACTER (LEN=1)   CFLSDRSMSG    ! Control flag,seed reserves msg
      CHARACTER (LEN=1)   CFLTFG        ! Control flag,temp.on lf area
      CHARACTER (LEN=10)  CNCHAR        ! Crop component (multicrop)
      CHARACTER (LEN=2)   CNCHAR2       ! Crop component (multicrop)
      CHARACTER (LEN=2)   CROP          ! Crop identifier (ie. CS)
      CHARACTER (LEN=2)   CROPPREV      ! Crop identifier,previous run
      CHARACTER (LEN=93)  CUDIRFLE      ! Cultivar directory+file
      CHARACTER (LEN=93)  CUDIRFLPREV   ! Cultivar directory+file,prev
      CHARACTER (LEN=12)  CUFILE        ! Cultivar file
      CHARACTER (LEN=10)  DAPCHAR       ! DAP in character form
      CHARACTER (LEN=6)   DAPWRITE      ! DAP character string -> output
      CHARACTER (LEN=93)  ECDIRFLE      ! Ecotype directory+file
      CHARACTER (LEN=93)  ECDIRFLPREV   ! Ecotype directory+file,prev
      CHARACTER (LEN=12)  ECFILE        ! Ecotype filename
      CHARACTER (LEN=6)   ECONO         ! Ecotype code
      CHARACTER (LEN=6)   ECONOPREV     ! Ecotype code,previous
      CHARACTER (LEN=60)  ENAME         ! Experiment description
      CHARACTER (LEN=1)   EMFLAG        ! Flag,emergence Y/N
      CHARACTER (LEN=1)   ESTABLISHED   ! Flag,crop establishment Y/N
      CHARACTER (LEN=14)  EVHEADER      ! Evaluater.out header
      CHARACTER (LEN=10)  EXCODE        ! Experiment code/name
      CHARACTER (LEN=10)  EXCODEPREV    ! Previous experiment code/name
      CHARACTER (LEN=80)  FAPPLINE(30)  ! Fertilizer application details
      CHARACTER (LEN=120) FILEA         ! Name of A-file
      CHARACTER (LEN=120) FILEX         ! Name of X-file
      CHARACTER (LEN=120) FILEIO        ! Name of input file,after check
      CHARACTER (LEN=120) FILEIOIN      ! Name of input file
      CHARACTER (LEN=107) FILEADIR      ! Name of A-file directory      
      CHARACTER (LEN=3)   FILEIOT       ! Type of input file
      CHARACTER (LEN=120) FILENEW       ! Temporary name of file
      CHARACTER (LEN=120) FILET         ! Name of T-file
      CHARACTER (LEN=1)   FNAME         ! File name switch (N->standard)
      CHARACTER (LEN=120) FNAMEERA      ! File name,A-errors
      CHARACTER (LEN=120) FNAMEERT      ! File name,T-errors
      CHARACTER (LEN=120) FNAMEEVAL     ! File name,evaluate outputs
      CHARACTER (LEN=120) FNAMELEAVES   ! File name,leaves outputs
      CHARACTER (LEN=120) FNAMEMEAS     ! File name,measured outputs
      CHARACTER (LEN=120) FNAMEOV       ! File name,overview outputs
      CHARACTER (LEN=120) FNAMEPHASES   ! File name,phases outputs
      CHARACTER (LEN=120) FNAMEPHENOLM  ! File name,phenology measured
      CHARACTER (LEN=120) FNAMEPHENOLS  ! File name,phenology outputs
      CHARACTER (LEN=120) FNAMEPREM     ! File name,responses,measured
      CHARACTER (LEN=120) FNAMEPRES     ! File name,responses,simulated
      CHARACTER (LEN=120) FNAMEPSUM     ! File name,plant summary
      CHARACTER (LEN=35)  GENFLCHK      ! Genotype file name for check
      CHARACTER (LEN=1)   GROUP         ! Flag for type of group
      CHARACTER (LEN=6)   HIAMCHAR      ! Harvest indx,at harvest
      CHARACTER (LEN=6)   HIAMMCHAR     ! Harvest indx,har,measured
      CHARACTER (LEN=6)   HINDC         ! Harvest index,nitrogen   
      CHARACTER (LEN=6)   HINMCHAR      ! Harvest N index,at harvest 
      CHARACTER (LEN=6)   HINMMCHAR     ! Harvest N index,mat,measured
      CHARACTER (LEN=6)   HNPCMCHAR     ! Harvest product N %,maturity  
      CHARACTER (LEN=6)   HNPCMMCHAR    ! Harvest product N%,mature,meas
      CHARACTER (LEN=1)   HOP(HANUMX)   ! Harvest operation code  
      CHARACTER (LEN=2)   HPROD         ! Code,harvested part of plant
      CHARACTER (LEN=6)   HWUDC         ! Harvest wt/unit
      CHARACTER (LEN=6)   HWUMCHAR      ! Harvest wt/unit
      CHARACTER (LEN=6)   HWUMMCHAR     ! Harvest wt/unit,mat,measured
      CHARACTER (LEN=1)   IDETD         ! Control flag,screen outputs
      CHARACTER (LEN=1)   IDETG         ! Control flag,growth outputs
      CHARACTER (LEN=1)   IDETL         ! Control switch,detailed output
      CHARACTER (LEN=1)   IDETO         ! Control flag,overview outputs
      CHARACTER (LEN=1)   IDETS         ! Control switch,summary outputs
      CHARACTER (LEN=1)   IFERI         ! Fertilizer switch (A,F,R,D,N)
      CHARACTER (LEN=1)   IHARI         ! Control flag,harvest
      CHARACTER (LEN=1)   IPLTI         ! Code for planting date method
      CHARACTER (LEN=1)   ISWDIS        ! Control switch,disease
      CHARACTER (LEN=1)   ISWNIT        ! Soil nitrogen balance switch
      CHARACTER (LEN=1)   ISWNITEARLY   ! Control flag,N stress early
      CHARACTER (LEN=1)   ISWWAT        ! Soil water balance switch Y/N
      CHARACTER (LEN=1)   ISWWATCROP    ! Control flag,H20 stress,crop 
      CHARACTER (LEN=1)   ISWWATEARLY   ! Control flag,H20 stress early
      CHARACTER (LEN=6)   LAIC          ! Leaf area index
      CHARACTER (LEN=6)   LAIPRODC      ! Leaf area index produced
      CHARACTER (LEN=6)   LAIXCHAR      ! Leaf area index,maximum
      CHARACTER (LEN=6)   LAIXMCHAR     ! Leaf area index,max,measured
      CHARACTER (LEN=6)   LAPC          ! Area of cohort of leaves
      CHARACTER (LEN=6)   LAPOTXC       ! Leaf area,potential
      CHARACTER (LEN=6)   LAPSC         ! Senesced area,cohort of leaves
      CHARACTER (LEN=6)   LATLC         ! Leaf area,no stress
      CHARACTER (LEN=6)   LATL2C        ! Leaf area,h2o,N,temp stress
      CHARACTER (LEN=6)   LATL3C        ! Leaf area,stress+assim control
      CHARACTER (LEN=6)   LATL4C        ! Leaf area,stress+assim&Ncntrol
!     CHARACTER (LEN=354) LINEERA       ! Temporary line,error-a file
      CHARACTER (LEN=80)  LINESTAR      ! Group header line (with star)
      CHARACTER (LEN=80)  LINESTAR2     ! Group header line (with star)
      CHARACTER (LEN=180) LINET         ! Line from T-file
      CHARACTER (LEN=4)   MEDEV         ! Switch,development control
      CHARACTER (LEN=1)   MEEXP         ! Switch,experimental method = E
      CHARACTER (LEN=1)   MEEVP         ! Switch,potential evapot method
      CHARACTER (LEN=1)   MEPHO         ! Switch,photosynthesis method
      CHARACTER (LEN=3)   MERNU         ! Switch,root N uptake methd
      CHARACTER (LEN=1)   MESOM         ! Switch,OM decay method       
      CHARACTER (LEN=78)  MESSAGE(10)   ! Messages for Warning.out
!     CHARACTER (LEN=1)   MENU          ! Switch,root N uptake methd
      CHARACTER (LEN=1)   MEWNU         ! Switch,root H2O/N uptake methd
      CHARACTER (LEN=8)   MODEL         ! Name of model
      CHARACTER (LEN=8)   MODNAME       ! Name of module
      CHARACTER (LEN=3)   MONTH         ! Month
      CHARACTER (LEN=3)   OUT           ! Output file extension
      CHARACTER (LEN=79)  OUTHED        ! Output file heading
      CHARACTER (LEN=12)  OUTPG         ! Growth output file code
      CHARACTER (LEN=12)  OUTPG2        ! Growth output file2 code
      CHARACTER (LEN=12)  OUTPGF        ! Growth factors file2 code
      CHARACTER (LEN=12)  OUTPN         ! GrowthN output file code
      CHARACTER (LEN=80)  PATHCR        ! Path to genotype (CUL) files
      CHARACTER (LEN=80)  PATHEC        ! Path to genotype (ECO) files
      CHARACTER (LEN=80)  PATHSP        ! Path to genotype (SPE) files
      CHARACTER (LEN=1)   PLME          ! Planting method (code)        
      CHARACTER (LEN=2)   PPSEN         ! Code,photoperiod sensitivity
      CHARACTER (LEN=5)   PSABV(PSX)    ! Principal stage abbreviation
      CHARACTER (LEN=5)   PSABVO(PSX)   ! Principal stage abv,output
      CHARACTER (LEN=13)  PSNAME(PSX)   ! Principal stage names
      CHARACTER (LEN=1)   PSTYP(PSX)    ! Principal stage type
      CHARACTER (LEN=6)   BRSTAGEC      ! Branching stage
      CHARACTER (LEN=1)   RNMODE        ! Run mode (eg.I=interactive)
      CHARACTER (LEN=25)  RUNNAME       ! Run title
      CHARACTER (LEN=8)   RUNRUNI       ! Run+internal run number
      CHARACTER (LEN=6)   SDWADC        ! Seed weight
      CHARACTER (LEN=1)   SEASENDOUT    ! Season end outputs flag     
      CHARACTER (LEN=6)   SENN0C        ! Senesced N added to litter
      CHARACTER (LEN=6)   SENNSC        ! Senesced N added to soil
      CHARACTER (LEN=6)   SENROOTC      ! Senesced OM,soil
      CHARACTER (LEN=6)   SENTOPLITTERAC! Senesced OM added to surface
      CHARACTER (LEN=64)  SPDIRFLE      ! Species directory+file
      CHARACTER (LEN=64)  SPDIRFLPREV   ! Species directory+file,last
      CHARACTER (LEN=12)  SPFILE        ! Species filename
!     CHARACTER (LEN=5)   SSABV(SSX)    ! Secondary stage abbreviation
!     CHARACTER (LEN=5)   SSABVO(SSX)   ! Secondary stage abv,output
!     CHARACTER (LEN=13)  SSNAME(SSX)   ! Secondary stage names
!     CHARACTER (LEN=1)   SSTYP(SSX)    ! Secoudary stage type
      CHARACTER (LEN=6)   TCHAR         ! Temporary character string
      CHARACTER (LEN=6)   THEAD(20)     ! T-file headings
      CHARACTER (LEN=1)   TIERNUMC      ! Tier number in t-file
!     CHARACTER (LEN=10)  TL10          ! Temporary line              
      CHARACTER (LEN=10)  TL10FROMI     ! Temporary line from integer
      CHARACTER (LEN=254) TLINEGRO      ! Temporary line from GRO file
      CHARACTER (LEN=180) TLINET        ! Temporary line from T-file
      CHARACTER (LEN=180) TLINETMP      ! Temporary line
      CHARACTER (LEN=25)  TNAME         ! Treatment name
      CHARACTER (LEN=10)  TNCHAR        ! Treatment number,characters
      CHARACTER (LEN=40)  TRUNNAME      ! Treatment+run composite name
!     CHARACTER (LEN=5)   TVTRDV        ! Temporary temp response char
!     CHARACTER (LEN=8)   VARCHAR       ! Temporary variable,character
      CHARACTER (LEN=6)   VARNO         ! Variety identification code
      CHARACTER (LEN=6)   VARNOPREV     ! Variety identification code
      CHARACTER (LEN=6)   VNPCMCHAR     ! Vegetative N %,maturity     
      CHARACTER (LEN=6)   VNPCMMCHAR    ! Vegetative N,mature,measured
      CHARACTER (LEN=16)  VRNAME        ! Variety name

      LOGICAL             FEXIST        ! File existence indicator
      LOGICAL             FEXISTA       ! File A existence indicator
      LOGICAL             FEXISTT       ! File T existence indicator
      LOGICAL             FFLAG         ! Temp file existance indicator
      LOGICAL             FFLAGEC       ! Temp file existance indicator
      LOGICAL             FOPEN         ! File open indicator

      ! Arrays for passing variables to OPSUM subroutine, CSM model only
      INTEGER,      PARAMETER :: SUMNUM = 37
      CHARACTER*5,  DIMENSION(SUMNUM) :: LABEL
      REAL,         DIMENSION(SUMNUM) :: VALUE

      INTRINSIC AMAX1,AMIN1,EXP,FLOAT,INDEX,INT,LEN,MAX,MIN,MOD,NINT
      INTRINSIC SQRT,ABS,TRIM

      ! RNMODE is a switch for run mode conveyed on the command line
      ! Options: I=Interactive, A=All treatments, B=Batch,
      ! E=Sensitivity, D=Debug, N=Seasonal, Q=Sequence, G=Gencalc

      ! For when simulating more than one species
      !REAL          sradip        ! Srad interception,whole can    %
      !REAL          sradipcn(5)   ! Srad interception,component    %

      ! For AFRC photosynthesis
      ! REAL PM,RA,RM,VPD,QPAR,ALPHA,RS,RP,PMAX,PHSA,PHSB,PGROSS

      SAVE

!***********************************************************************
      IF (DYNAMIC.EQ.RUNINIT) THEN    ! Initialization
!***********************************************************************

        IF (RUNCRP.LE.0) THEN          ! First time through

          MODNAME(1:8) = 'CSCAS048'
          CSCAS_VERSION = 010115
          GENFLCHK(1:15) = 'CSCAS048.20200721'

!-----------------------------------------------------------------------
!         Set parameters (Most should be placed in input files!)
!-----------------------------------------------------------------------

          ! Physical constants
          MJPERE = 220.0*1.0E-3  ! MJ per Einstein at 540 nm
          PARMJFAC = 0.5         ! PAR in SRAD (fr)

          ! Standard parameters
          STDAY = 20.0      ! TT in standard day
          STEPNUM = 1       ! Step number per day set to 1
          
          ! Methods that not in control file
          ! (Impementation needs a re-compile)
          MERNU = 'CSM'     ! Root N uptake 
          MEDEV = 'LNUM'    ! Control of development (Lnum or devu)

          ! Control flags that not in control file 
          ! (Impementation needs a  re-compile)
          ! (Only useful when examining how the model is working)
          ISWWATCROP = 'Y'    ! N=no water stress effect on crop growth
          CFLAFLF = 'Y'       ! Assimilate effect on leaf size
          CFLTFG = 'Y'        ! Temperature effect on leaf size

!-----------------------------------------------------------------------
!         Read command line arguments for model name and path (Cropsim)
!-----------------------------------------------------------------------

          arg = ' '
          tvi2 = 0
          tvi3 = 0
          tvi4 = 0
          ! Following not good for all platforms.Changed for portability
          ! CALL GETARG (0,arg,arglen)
          CALL GETARG(0,arg)
          arglen = len_trim(arg)
          DO tvi1 = 1,arglen
            IF (arg(tvi1:tvi1).EQ.Slash) tvi2=tvi1
            IF (arg(tvi1:tvi1).EQ.'.') tvi3=tvi1
            IF (arg(tvi1:tvi1).EQ.' ' .AND. tvi4.EQ.0) tvi4=tvi1
          ENDDO
          IF (TVI3.EQ.0 .AND. TVI4.GT.0) THEN
            tvi3 = tvi4
          ELSEIF (TVI3.EQ.0 .AND. TVI4.EQ.0) THEN
            tvi3 = arglen+1
          ENDIF
          MODEL = ARG(TVI2+1:TVI3-1)
          CALL UCASE(MODEL)

!-----------------------------------------------------------------------
!         Set configuration file name (Cropsim)
!-----------------------------------------------------------------------

          IF (FILEIOT(1:2).NE.'DS') THEN
            CFGDFILE = ' '
            IF (TVI2.GT.1) THEN
              CFGDFILE = ARG(1:TVI2)//'CROPSIM.CFG'
            ELSE
              CFGDFILE(1:12) = 'CROPSIM.CFG '
            ENDIF
          ENDIF

!-----------------------------------------------------------------------
!         Determine input file type (Dssat or X-file) and check if there
!-----------------------------------------------------------------------

          TVI1 = TVILENT(FILEIOIN)
          IF (FILEIOIN(TVI1-2:TVI1).EQ.'INP') THEN
            FILEIOIN(TVI1:TVI1) = 'H'
            FILEIOT = 'DS4'
          ELSE
            FILEIOT = 'XFL'
          ENDIF
          FILEIO = ' '
          FILEIO(1:TVI1) = FILEIOIN(1:TVI1)
          INQUIRE (FILE = FILEIO,EXIST = FFLAG)
          IF (.NOT.(FFLAG)) THEN
            CALL GETLUN ('ERROR.OUT',FNUMERR)
            OPEN (UNIT = FNUMERR,FILE = 'ERROR.OUT')
            WRITE(fnumerr,*) ' '
            WRITE(fnumerr,*) 'Input file not found!     '
            WRITE(fnumerr,*) 'File sought was:          '
            WRITE(fnumerr,*) Fileio(1:78)
            WRITE(fnumerr,*) 'Please check'
            WRITE(*,*) ' Input file not found!     '
            WRITE(*,*) 'File sought was:          '
            WRITE(*,*) Fileio(1:78)
            WRITE(*,*) ' Program will have to stop'
            CLOSE (FNUMERR)
            STOP ' '
          ENDIF

!-----------------------------------------------------------------------
!         Create output file extensions (For different components)
!-----------------------------------------------------------------------

          CNCHAR = ' '
          CNCHAR2 = '  '
          IF (CN.EQ.1.OR.CN.EQ.0) THEN
            OUT = 'OUT'
            CNCHAR2= '1 '
          ELSE
            CNCHAR = TL10FROMI(CN)
            OUT = 'OU'//CNCHAR(1:1)
            CNCHAR2(1:1) = CNCHAR(1:1)
          ENDIF

!-----------------------------------------------------------------------
!         Set output flags to agree with run modes and control switches
!-----------------------------------------------------------------------

          IF (FILEIOT.EQ.'XFL') THEN
            IF (RNMODE.EQ.'I'.OR.RNMODE.EQ.'E'.OR.RNMODE.EQ.'A') THEN
              IDETD = 'M'
            ELSEIF (RNMODE.EQ.'B'.OR.RNMODE.EQ.'N'.OR.RNMODE.EQ.'Q')THEN
              IDETD = 'S'
            ENDIF  
          ELSE
            IDETD = 'N'
          ENDIF
          FROPADJ = FROP
          IF (RNMODE.EQ.'T') FROPADJ = 1
          IF (IDETL.EQ.'D'.OR.IDETL.EQ.'A') FROPADJ = 1

!-----------------------------------------------------------------------
!         Set file names and determine file unit numbers
!-----------------------------------------------------------------------

          ! DATA FILES
          CALL GETLUN ('FILET',FNUMT)

          ! WORK,ERROR,AND TEMPORARY FILES
!          CALL GETLUN ('WORK.OUT',FNUMWRK)
          CALL GETLUN ('ERROR.OUT',FNUMERR)
          CALL GETLUN ('FNAMETMP',FNUMTMP)

          ! IDETG FILES
          ! Check if need to change file names
          CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'FNAME',fname)
          IF (FNAME.EQ.'Y') THEN   ! File name change required.
            CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'EXPER',excode)
            !NB. Renaming of Plantgro and Plantn handled by CSM
            OUTPG = 'PlantGro.'//OUT
            OUTPN = 'PlantN.'//OUT
            OUTPG2 = EXCODE(1:8)//'.OP2'
            OUTPGF = EXCODE(1:8)//'.OPF'
          ELSE  
            OUTPG = 'PlantGro.'//OUT
            OUTPN = 'PlantN.'//OUT
            OUTPG2 = 'PlantGr2.'//OUT
            OUTPGF = 'PlantGrf.'//OUT
          ENDIF
          CALL GETLUN (OUTPG,NOUTPG)
          CALL GETLUN (OUTPG2,NOUTPG2)
          CALL GETLUN (OUTPGF,NOUTPGF)
          CALL GETLUN (OUTPN,NOUTPN)

          ! IDETO FILES
          ! NB. Renaming of Overview and Evaluate handled by CSM
          FNAMEOV = 'Overview.'//out
          FNAMEEVAL = 'Evaluate.'//out
          FNAMEMEAS = 'Measured.'//out
          CALL GETLUN (FNAMEEVAL,fnumeval)
          CALL GETLUN (FNAMEOV,fnumov)
          CALL GETLUN (FNAMEMEAS,fnummeas)

          ! IDETS FILES
          FNAMEPSUM(1:12)   = 'Plantsum.'//OUT
          CALL GETLUN (FNAMEPSUM,  fnumpsum)

          ! RESPONSE FILES
          FNAMEPRES(1:12)   = 'Plantres.'//out
          FNAMEPREM(1:12) = 'Plantrem.'//out
          CALL GETLUN (FNAMEPRES,  fnumpres)
          CALL GETLUN (FNAMEPREM,fnumprem)

          ! LEAVES FILES
          FNAMELEAVES(1:10) = 'Leaves.'//OUT
          CALL GETLUN (FNAMELEAVES,fnumlvs)

          ! PHENOL FILES
          FNAMEPHASES(1:9) = 'Tiers.'//out
          FNAMEPHENOLS(1:11) = 'Phenols.'//out
          FNAMEPHENOLM(1:11) = 'Phenolm.'//out
          CALL GETLUN (FNAMEPHASES,fnumpha)
          CALL GETLUN (FNAMEPHENOLS,fnumphes)
          CALL GETLUN (FNAMEPHENOLM,fnumphem)

          ! ERROR FILES
          FNAMEERA(1:12) = 'Plantera.'//out
          FNAMEERT(1:12) = 'Plantert.'//out
          CALL GETLUN (FNAMEERT,fnumert)
          CALL GETLUN (FNAMEERA,fnumera)

!-----------------------------------------------------------------------
!         Open and write main headers to output files
!-----------------------------------------------------------------------

          ! WARNING AND WORK FILES
!          INQUIRE (FILE = 'WORK.OUT',OPENED = FOPEN)
!          IF (.NOT.FOPEN) THEN
!            IF (RUN.EQ.1) THEN
!              OPEN (UNIT = FNUMWRK,FILE = 'WORK.OUT')
!              WRITE(FNUMWRK,*) 'CSCAS  Cropsim Cassava Module '
!            ELSE
!              OPEN (UNIT = FNUMWRK,FILE = 'WORK.OUT',POSITION='APPEND',
!     &              ACTION = 'READWRITE')
!              WRITE(fnumwrk,*) ' '
!              WRITE(fnumwrk,*) 'CSCAS  Cropsim Cassava Module '
!              IF (IDETL.EQ.'0'.OR.IDETL.EQ.'Y'.OR.IDETL.EQ.'N') THEN
!                CLOSE (FNUMWRK)
!                OPEN (UNIT = FNUMWRK,FILE = 'WORK.OUT')
!                WRITE(fnumwrk,*) ' '
!                WRITE(fnumwrk,*) 'CSCAS  Cropsim Cassava Module '
!              ENDIF  
!            ENDIF
!          ELSE          
!            IF (IDETL.EQ.'0'.OR.IDETL.EQ.'Y'.OR.IDETL.EQ.'N') THEN
!              ! Close and re-open Work file
!              CLOSE (FNUMWRK, STATUS = 'DELETE')
!              OPEN (UNIT = FNUMWRK,FILE = 'WORK.OUT', STATUS = 'NEW',
!     &              ACTION = 'READWRITE')
!              WRITE(fnumwrk,*) ' '
!              WRITE(fnumwrk,*) 'CSCAS  Cropsim Cassava Module '
! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.              
!              CALL Getlun('READS.OUT',fnumrea)
!              ! Close and re-open Reads file
!              CLOSE (FNUMREA, STATUS = 'DELETE')
!              OPEN (UNIT = FNUMREA,FILE = 'READS.OUT', STATUS = 'NEW',
!     &              ACTION = 'READWRITE')
!              WRITE(fnumrea,*)' '
!              WRITE(fnumrea,*)
!     &        ' File closed and re-opened to avoid generating huge file'
!            ELSE  
!              WRITE(fnumwrk,*) ' '
!              WRITE(fnumwrk,*) 'CSCAS  Cropsim Cassava Module '
!            ENDIF
!          ENDIF  

          IF (RUN.EQ.1) THEN
            ! IDETG FILES
            OPEN (UNIT = NOUTPG, FILE = OUTPG)
            WRITE (NOUTPG,'(A27)')
     &      '$GROWTH ASPECTS OUTPUT FILE'
            CLOSE (NOUTPG)
            OPEN (UNIT = NOUTPG2, FILE = OUTPG2)
            WRITE (NOUTPG2,'(A38)')
     &      '$GROWTH ASPECTS SECONDARY OUTPUTS FILE'
            CLOSE (NOUTPG2)
            OPEN (UNIT = NOUTPGF, FILE = OUTPGF)
            WRITE (NOUTPGF,'(A27)')
     &      '$GROWTH FACTOR OUTPUTS FILE'
            CLOSE (NOUTPGF)
            IF (ISWNIT.NE.'N') THEN
              OPEN (UNIT = NOUTPN, FILE = OUTPN)
              WRITE (NOUTPN,'(A35)')
     &        '$PLANT NITROGEN ASPECTS OUTPUT FILE'
              CLOSE (NOUTPN)
            ELSE  
              INQUIRE (FILE = OUTPN,EXIST = FEXIST)
              IF (FEXIST) THEN
                OPEN (UNIT = NOUTPN, FILE = OUTPN, STATUS='UNKNOWN',
     &          POSITION = 'APPEND')
                CLOSE (UNIT=NOUTPN, STATUS = 'DELETE')
              ENDIF  
            ENDIF  

            ! IDETO FILES
            OPEN (UNIT = FNUMOV, FILE = FNAMEOV)
            WRITE(FNUMOV,'(A20)') '$SIMULATION_OVERVIEW'
            CLOSE(FNUMOV)
            OPEN (UNIT = FNUMEVAL, FILE = FNAMEEVAL)
            WRITE(FNUMEVAL,'(A17)') '$PLANT_EVALUATION'
            CLOSE(FNUMEVAL)
            OPEN (UNIT = FNUMMEAS,FILE = FNAMEMEAS)
            WRITE (FNUMMEAS,'(A22)') '$TIME_COURSE(MEASURED)'
            CLOSE(FNUMMEAS)
            
            ! IDETS FILES
            OPEN (UNIT = FNUMPSUM,FILE = FNAMEPSUM)
            WRITE (FNUMPSUM,'(A27)') '$PLANT_SUMMARY             '
            CLOSE(FNUMPSUM)
            
             OPEN(UNIT=FNUMLVS,FILE=FNAMELEAVES)
             WRITE (FNUMLVS,'(A11)') '$LEAF_SIZES'
             CLOSE(FNUMLVS)
            
             OPEN(UNIT=FNUMPHA,FILE=FNAMEPHASES)
             WRITE (FNUMPHA,'(A16)') '$TIER_CONDITIONS'
             CLOSE(FNUMPHA)
             OPEN(UNIT=FNUMPHES,FILE=FNAMEPHENOLS)
             WRITE (FNUMPHES,'(A27)') '$PHENOLOGY_DATES(SIMULATED)'
             CLOSE(FNUMPHES)
             OPEN(UNIT=FNUMPHEM,FILE=FNAMEPHENOLM)
             WRITE (FNUMPHEM,'(A27)') '$PHENOLOGY_DATES(MEASURED) '
             CLOSE(FNUMPHEM)
            
             OPEN (UNIT = FNUMPRES,FILE = FNAMEPRES,STATUS = 'UNKNOWN')
             WRITE (FNUMPRES,'(A27)') '$PLANT_RESPONSES(SIMULATED)'
             CLOSE(FNUMPRES)
             OPEN (UNIT = FNUMPREM,FILE = FNAMEPREM,STATUS = 'UNKNOWN')
             WRITE (FNUMPREM,'(A26)') '$PLANT_RESPONSES(MEASURED)'
             CLOSE(FNUMPREM)

             ! ERROR FILES
             INQUIRE (FILE = FNAMEERA,EXIST = FFLAG)
             OPEN (UNIT = FNUMERA,FILE = FNAMEERA,STATUS = 'UNKNOWN')
             WRITE (FNUMERA,'(A27)') '$ERRORS   As % of measured '
             CLOSE(FNUMERA)
             OPEN (UNIT = FNUMERT,FILE = FNAMEERT,STATUS = 'UNKNOWN')
             WRITE (FNUMERT,'(A20)') '$ERRORS(TIME_COURSE)'
             WRITE (FNUMERT,*)' '
             WRITE (FNUMERT,'(A25)')'! Errors as % of measured'
             CLOSE(FNUMERT)

            ! Initialize 'previous' variables
            CROPPREV = '  '
            VARNOPREV = ' '
            CUDIRFLPREV = ' '
            ECONOPREV = ' '
            ECDIRFLPREV = ' '
            SPDIRFLPREV = ' '
          ENDIF

        ENDIF ! End of first time through stuff
          
!-----------------------------------------------------------------------
!       Record/set starting information
!-----------------------------------------------------------------------

        YEARSIM = YEAR*1000 + DOY
        SEASENDOUT = 'N'  ! Season end outputs flag     

!-----------------------------------------------------------------------
!       Increment counter for re-initialized runs within module
!-----------------------------------------------------------------------

        RUNCRP = RUNCRP + 1

!***********************************************************************
      ELSEIF (DYNAMIC.EQ.SEASINIT) THEN    ! Initialization
!***********************************************************************
        
!-----------------------------------------------------------------------
!       Initialize both state and rate variables                   
!-----------------------------------------------------------------------

        aflf = 0.0
        amtnit = 0.0
        andem = 0.0
        brnumst = 1.0
        caid = 0.0
        canht = 0.0
        canhtg = 0.0
        carboadj = 0.0
        carbobeg = 0.0
        carbobegi = 0.0
        carbobegm = 0.0
        carbobegr = 0.0
        carboc = 0.0
        carboend = 0.0
        carbor = 0.0
        carbot = 0.0
        cdays  = 0
        cflfail = 'N'
        cfllflife = '-'
        cflharmsg = 'N'
        cflsdrsmsg = 'N'
        crrswt = 0.0
        crwad = 0.0
        crwt = 0.0
        cnad = 0.0
        cnadprev = 0.0
        cnadstg = 0.0
        cnam = -99.0
        cnamm = -99.0
        co2cc = 0.0
        co2fp = 1.0
        co2intppm = 0.0
        co2intppmp = 0.0
        co2max = -99.0
        co2pav = -99.0
        co2pc = 0.0
        cumdu = 0.0
        cwad = 0.0
        cwadprev = 0.0
        cwadstg = 0.0
        cwahc = 0.0
        cwahcm = -99.0
        cwam = -99.0
        cwamm = -99.0
        dae = -99
        dap = -99
        daylcc = 0.0
        daylpav = -99.0
        daylpc = 0.0
        daylst = 0.0
        daysum = 0.0
        sentoplitter = 0.0
        dewdur = -99.0
        df = 1.0
        dfout = 1.0
        dalf = 0
        dglf = 0
        dslf = 0
        drainc = 0.0
        dstage = 0.0
        du = 0.0
        duneed = 0.0
        dynamicprev = 99999
        edap = -99
        edapfr = 0.0
        edapm = -99
        edayfr = 0.0
        emrgfr = 0.0
        emrgfrprev = 0.0
        emflag = 'N'
        eoc = 0.0
        eoebud = 0.0
        eoebudc = 0.0
        eoebudcrp = 0.0
        eoebudcrpc = 0.0
        eoebudcrpco2 = 0.0
        eoebudcrpco2c = 0.0
        eoebudcrpco2h2o = 0.0
        eoebudcrpco2h2oc = 0.0
        eompen = 0.0
        eompenc = 0.0
        eompcrp = 0.0
        eompcrpc = 0.0
        eompcrpco2 = 0.0
        eompcrpco2c = 0.0
        eompcrpco2h2o = 0.0
        eompcrpco2h2oc = 0.0
        eopen = 0.0
        eopenc = 0.0
        eopt = 0.0
        eoptc = 0.0
        epcc   = 0.0
        epsratio = 0.0
        established = 'n'
        etcc   = 0.0
        eyeardoy = -99
        fappline = ' '
        fappnum = 0
        fernitprev = 0.0
        fldap = 0
        fln = 0.0
        gdap = -99
        gdap = -99
        gdapm = -99
        gdayfr = 0.0
        gedayse = 0.0
        gedaysg = 0.0
        germfr = -99.0
        gestage = 0.0
        gestageprev = 0.0
        geucum = 0.0
        grocr = 0.0
        grocradj = 0.0
        grolf = 0.0
        grolfadj = 0.0
        grors = 0.0
        grosr = 0.0
        grost = 0.0
        grostadj = 0.0
        grostcr = 0.0
        gyeardoy = -99
        hamt = 0.0
        hbpc = -99.0
        hbpcf = -99.0
        hiad = 0.0
        hiam = -99.0
        hiamm = -99.0
        hind = 0.0
        hinm = -99.0
        hinmm = -99.0
        hnad = 0.0
        hnam = -99.0
        hnamm = -99.0
        hnpcm = -99.0
        hnpcmm = -99.0
        hnumam = -99.0
        hnumamm = -99.0
        hnumber = -99
        hnumgm = -99.0
        hnumgmm = -99.0
        hop = ' '
        hpc = -99.0
        hpcf = -99.0
        hstage = 0.0
        hwam = -99.0 
        hwamm = -99.0
        hwum = -99.0
        hwummchar = ' -99.0'
        hyrdoy = -99
        hyeardoy = -99
        idetgnum = 0
        irramtc = 0.0
        lagett = 0.0
        lagep = 0.0
        lagl = 0.0
        lai = 0.0
        laiprev = 0.0
        lail = 0.0
        laila = 0.0
        laistg = 0.0
        laix = 0.0
        laixm = -99.0
        lanc = 0.0
        lap = 0.0
        laphc = 0.0
        lapp = 0.0
        laps = 0.0
        latl = 0.0
        latl2 = 0.0
        latl3 = 0.0
        latl4 = 0.0
        lcnum = 0
        lcoa = 0.0
        lcoas = 0.0
        leafn = 0.0
        lfwt = 0.0
        lfwtm = 0.0
        llnad = 0.0
        llrswad = 0.0
        llrswt = 0.0
        llwad = 0.0
        lncr = 0.0
        lncx = 0.0
        lndem = 0.0
        lnphc = 0.0
        lnum = 0.0
        lnumend = 0.0
        lnumg = 0.0
        lnumprev = 0.0
        lnumsg = 0
        lnumsm = -99.0
        lnumsmm = -99.0
        lnumstg = 0.0
        lnuse = 0.0
        lseed = -99
        lpeai = 0.0
        lperswad = 0.0
        lperswt = 0.0
        lpewad = 0.0
        lstage = 0.0
        lwphc = 0.0
        mdap = -99
        mdat = -99
        mdayfr = -99
        mdoy = -99
        nfg = 1.0
        nfgcc = 0.0
        nfgcc = 0.0
        nfgpav = 1.0
        nfgpc = 0.0
        nflf = 1.0
        nflf2 = 0.0
        nflfp = 1.0
        nfp = 1.0
        nfpcav = 1.0
        nfpcc = 0.0
        nfppav = 1.0
        nfppc = 0.0
        nsdays = 0
        nuf = 1.0
        nupac = 0.0
        nupad = 0.0
        nupap = 0.0
        nupapcsm = 0.0
        nupapcsm1 = 0.0
        nupapcrp = 0.0
        nupc = 0.0
        nupd = 0.0
        nupratio = 0.0
        pari = 0.0
        pari1 = 0.0
        parip = -99.0
        paripa = -99.0
        pariue = 0.0
        parmjc = 0.0
        parmjic = 0.0
        paru = 0.0
        pdadj = -99.0
        pdays  = 0
        photqr = 0.0
        pla = 0.0
        plags2 = 0.0
        plagsb2 = 0.0
        plagsb3 = 0.0
        plagsb4 = 0.0
        plas = 0.0
        plasi = 0.0
        plasl = 0.0
        plasp = 0.0
        plass = 0.0
        plax = 0.0
        pltpop = 0.0
        plyear = -99
        plyeardoy = 9999999
        psdap = -99
        psdapm = -99
        psdat = -99
        psdayfr = 0.0
        ptf = 0.0
        rainc = 0.0
        raincc = 0.0
        rainpav = -99.0
        rainpc = 0.0
        ranc = 0.0
        rescal = 0.0
        rescalg = 0.0
        reslgal = 0.0
        reslgalg = 0.0
        resnal = 0.0
        resnalg = 0.0
        respc = 0.0
        resprc = 0.0
        resptc = 0.0
        reswal = 0.0
        reswalg = 0.0
        rlf = 0.0
        rlfc = 0.0
        rlv = 0.0
        rnad = 0.0
        rnam = -99.0
        rnamm = -99.0
        rncr = 0.0
        rndem = 0.0
        rnuse = 0.0
        rootn = 0.0
        rootns = 0.0
        rscd = 0.0
        rscm = 0.0
        rscx = 0.0
        rsfp = 1.0
        rsn = 0.0
        rsnph = 0.0
        rsnphc = 0.0
        rsnad = 0.0
        rsnused = 0.0
        brstage = 0.0
        brstageprev = 0.0
        rswad = 0.0
        rswam = -99.0
        rswamm = -99.0
        rswphc = 0.0
        rswt = 0.0
        rswtm = 0.0
        rswtx = 0.0
        rtdep = 0.0
        rtdepg = 0.0
        rtnsl = 0.0
        rtresp = 0.0
        rtrespadj = 0.0
        rtslxdate = -99 
        rtwt = 0.0
        rtwtal = 0.0
        rtwtg = 0.0
        rtwtgadj = 0.0
        rtwtgl = 0.0
        rtwtl = 0.0
        rtwtm = 0.0
        rtwtsl = 0.0
        runoffc = 0.0
        rwad = 0.0
        rwam = -99.0
        rwamm = -99.0
        said = 0.0
        sanc = 0.0
        sancout = 0.0
        sdnad = 0.0
        sdnc = 0.0
        sdwad = 0.0
        sdwam = -99.0
        seeduse = 0.0
        seeduser = 0.0
        seeduset = 0.0
        sencags = 0.0
        sencalg = 0.0
        sencas = 0.0
        sencl = 0.0
        sencs = 0.0
        senla = 0.0
        senlalitter = 0.0
        senlags = 0.0
        senlalg = 0.0
        senlas = 0.0
        senlfg = 0.0
        senlfgrs = 0.0
        senll = 0.0
        senls = 0.0
        sennags = 0.0
        sennal = 0.0
        sennalg = 0.0
        sennas = 0.0
        sennatc = -99.0
        sennatcm = -99.0
        sennl = 0.0
        sennlfg = 0.0
        sennlfgrs = 0.0
        senns = 0.0
        senrtg = 0.0
        sentoplitterg = 0.0
        senwacm = -99.0
        senwacmm = -99.0
        senwags = 0.0
        senwal = 0.0
        senwalg = 0.0
        senwl = 0.0
        senroot = 0.0
        senroota = 0.0
        shdat = 0
        shla = 0.0
        shlag2 = 0.0
        shlas = 0.0
        shnum = 0.0
        shnumad = 0.0
        shnuml = 1.0
        shrtd = 0.0
        shrtm = 0.0
        sla = -99.0
        snad = 0.0
        sncr = 0.0
        sno3profile = 0.0
        sno3profile = 0.0
        sno3rootzone = 0.0
        snh4rootzone = 0.0
        snph = 0.0
        snphc = 0.0
        snuse = 0.0
        sradc = 0.0
        sradcav  = -99.0
        sradcc = 0.0
        sradd = 0.0
        sradpav  = -99.0
        sradpc = 0.0
        sradprev = 0.0
        srnam = -99.0
        sranc = 0.0
        srndem = 0.0
        srnoad = 0.0
        srnoam = 0.0
        srnogm = 0.0
        srnopd = 0.0
        srnuse = 0.0
        srootn = 0.0
        srwt = 0.0
        srwtgrs = 0.0
        srwud = 0.0
        srwum = 0.0
        srwum = 0.0
        stai = 0.0
        staig = 0.0
        stais = 0.0
        stemn = 0.0
        stgedat = 0
        stgyeardoy = 9999999
        strswt = 0.0
        stwad = 0.0
        stwt = 0.0
        stwtm = 0.0
        swphc = 0.0
        tcan = 0.0
        tdifav = -99.0
        tdifnum = 0
        tdifsum = 0.0
        tfd = 0.0
        tfg = 1.0
        tfglf = 0.0
        tfdlf = 0.0
        tfp = 1.0
        tlchc = 0.0
        tmaxcav  = -99.0
        tmaxcc = 0.0
        tmaxm = -99.0
        tmaxpav  = -99.0
        tmaxpc = 0.0
        tmaxsum = 0.0
        tmaxx = -99.0
        tmean = -99.0
        tmeanav = -99.0
        tmeancc = 0.0
        tmeand = 0.0
        tmeane  = 0.0
        tmeanec = 0.0
        tmeang  = 0.0
        tmeangc = 0.0
        tmeannum = 0.0
        tmeanpc = 0.0
        tmeansum = 0.0
        tmincav  = -99.0
        tmincc = 0.0
        tminm = 999.0
        tminn = 99.0
        tminpav  = -99.0
        tminpc = 0.0
        tminsum = 0.0
        tnad = 0.0
        tnoxc = 0.0
        tofixc = 0.0
        tominc = 0.0
        tominfomc = 0.0
        tominsom1c = 0.0
        tominsom2c = 0.0
        tominsom3c = 0.0
        tominsomc = 0.0
        tratio = 0.0
        trwup = 0.0
        tt = 0.0
        tt20 = -99.0
        ttgem = 0.0
        ttcum = 0.0
        ttd = 0.0
        ttnext = 0.0
        twad = 0.0
        uh2o = 0.0
        unh4 = 0.0
        uno3 = 0.0
        vanc = 0.0
        vcnc = 0.0
        vmnc = 0.0
        vnad = 0.0
        vnam = -99.0
        vnamm = -99.0
        vnpcm = -99.0
        vnpcmm = -99.0
        vpdfp = 1.0
        vwad = 0.0
        vwam = -99.0
        vwamm = -99.0
        wfg = 1.0
        wfgcc = 0.0
        wfgpav = 1.0
        wfgpc = 0.0
        wflf = 0.0
        wfp = 1.0
        wfpcav = 1.0
        wfppav = 1.0
        wfpcc = 0.0
        wfppc = 0.0
        wsdays = 0
        wupr = 1.0
        
        h2ocf = -99.0
        no3cf = -99.0
        nh4cf = -99.0
        rtno3 = -99.0
        rtnh4 = -99.0
        no3mn = -99.0
        nh4mn = -99.0
        
!-----------------------------------------------------------------------
!       Read experiment information from Dssat input or X- file
!-----------------------------------------------------------------------

        ! Methods
        CALL XREADC(FILEIO,TN,RN,SN,ON,CN,'PHOTO',mepho)
        CALL XREADC(FILEIO,TN,RN,SN,ON,CN,'EVAPO',meevp) 
        CALL XREADC(FILEIO,TN,RN,SN,ON,CN,'MEWNU',mewnu)
        CALL XREADC(FILEIO,TN,RN,SN,ON,CN,'METHODS',meexp)

        ! Experiment, treatment, and run control names
        CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'ENAME',ename)
        CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'EXPER',excode)
        CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'TNAME',tname)
        CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'SNAME',runname)

        ! Planting date information
        CALL XREADC(FILEIO,TN,RN,SN,ON,CN,'PLANT',iplti)
!       IF(IPLTI.EQ.'A'.OR.IPLTI.EQ.'a')THEN
        IF(IPLTI.EQ.'A'.OR.IPLTI.EQ.'a'.OR.
     &     IPLTI.EQ.'F'.OR.IPLTI.EQ.'f')THEN
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'PFRST',pwdinf)
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'PLAST',pwdinl)
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'PH2OL',swpltl)
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'PH2OU',swplth)
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'PH2OD',swpltd)
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'PSTMX',ptx)
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'PSTMN',pttn)
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'HFRST',hfirst)
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'HLAST',hlast)
        ELSE
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'PDATE',pdate)
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'EDATE',edatmx)
        ENDIF

        ! Other planting information
        CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'CR',crop)
        CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'INGENO',varno)
        CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'CNAME',vrname)
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPOP',pltpopp)
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPOE',pltpope)
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PLRS',rowspc)
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PLDP',sdepth)
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PLWT',sdrate)
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PAGE',plmage)
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'SPRL',sprl)
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PLPH',plph)
        CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'PLME',plme)

        ! Harvest instructions
        CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'HARVS',ihari)
        CALL XREADRA (FILEIO,TN,RN,SN,ON,CN,'HPC','40',hpc)
        CALL XREADRA (FILEIO,TN,RN,SN,ON,CN,'HBPC','40',hbpc)

        CALL XREADIA(FILEIO,TN,RN,SN,ON,CN,'HDATE','40',hyrdoy)
        CALL XREADCA(FILEIO,TN,RN,SN,ON,CN,'HOP','40',hop)
        CALL XREADRA(FILEIO,TN,RN,SN,ON,CN,'HAMT','40',hamt)
        CALL XREADRA(FILEIO,TN,RN,SN,ON,CN,'CWAN','40',cwan)
        CALL XREADRA(FILEIO,TN,RN,SN,ON,CN,'LSNUM','40',lsnum)
        CALL XREADRA(FILEIO,TN,RN,SN,ON,CN,'LSWT','40',lswt)
        
        DO I = 1,20
          IF (hyrdoy(i).EQ.-99) THEN
            hnumber = i - 1
            EXIT  
          ENDIF
C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY
          !hyeardoy(i) = CSYEARDOY(hyrdoy(i))
          CALL Y4K_DOY(hyrdoy(i),FILEX,0,ERRKEY,3)
          hyeardoy(i) = hyrdoy(i)
        ENDDO 
        IF (hnumber.LE.1) HOP(1) = 'F' 
        yeardoyharf = -99
        DO I = 1, 20
          IF (HYEARDOY(I).GT.0) THEN
            hnumber = I
            IF (hop(i).EQ.'F') THEN
              hpcf = hpc(i)
              hbpcf = hbpc(i)
              yeardoyharf = hyeardoy(i)
            ENDIF 
          ENDIF
        END DO
        IF (hnumber.EQ.1) THEN
          hpcf = hpc(1)
          hbpcf = hbpc(1)
          yeardoyharf = hyeardoy(1)
        ENDIF 
        ! If running CSM use harvfrac so as to handle automatic mngement
        IF (FILEIOT .NE. 'DS4') THEN
          hpcf = harvfrac(1)*100.0   ! Harvest %
          hbpcf = harvfrac(2)*100.0
        ENDIF

        ! Fertilization information (to calculate N appl during cycle)
        CALL XREADC(FILEIO,TN,RN,SN,ON,CN,'FERTI',iferi)
        CALL XREADIA(FILEIO,TN,RN,SN,ON,CN,'FDATE','200',fday)
        CALL XREADRA(FILEIO,TN,RN,SN,ON,CN,'FAMN','200',anfer)

        ! Water table depth
        CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'ICWD',icwd)

        ! Disease information
        LENDIS = TVILENT(ISWDIS)
        DIDAT = -99
        DIGFAC = -99
        DIFFACR = -99
        DCDAT = -99
        DCDUR = -99
        DCFAC = -99
        DCTAR = -99
        IF (LENDIS.EQ.1.AND.ISWDIS(LENDIS:LENDIS).EQ.'R') THEN
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'D1DAT',didat(1))
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'D2DAT',didat(2))
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'D3DAT',didat(3))
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'D1GF',digfac(1))
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'D2GF',digfac(2))
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'D3GF',digfac(3))
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'D1FFR',diffacr(1))
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'D2FFR',diffacr(2))
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'D3FFR',diffacr(3))
          CALL XREADIA(FILEIO,TN,RN,SN,ON,CN,'DCDAT','10',dcdat)
          CALL XREADRA(FILEIO,TN,RN,SN,ON,CN,'DCDUR','10',dcdur)
          CALL XREADRA(FILEIO,TN,RN,SN,ON,CN,'DCFAC','10',dcfac)
          CALL XREADIA(FILEIO,TN,RN,SN,ON,CN,'DCTAR','10',dctar)
        ELSEIF (LENDIS.EQ.1.AND.ISWDIS(LENDIS:LENDIS).EQ.'Y') THEN
          DIGFAC(1) = 1.0
          DIGFAC(2) = 1.0
          DIGFAC(3) = 1.0
        ELSEIF (LENDIS.GT.1) THEN
          ! LAH ISWDIS DEFINED AS NUMBER FOR WHICH DISEASES
          ! NOT JUST ONE NUMBER OR CHARACTER
          ! DISCUSS WITH CHP 
          CALL LTRIM(ISWDIS)
          !READ(ISWDIS,'(I1)') DIGFACTMP
          !DIGFAC(1) = DIGFACTMP/10.0
          !READ(ISWDIS,'(2X,I1)') DIGFACTMP
          !DIGFAC(2) = DIGFACTMP/10.0
          !READ(ISWDIS,'(4X,I1)') DIGFACTMP
          !DIGFAC(3) = DIGFACTMP/10.0
        ENDIF

        IF (FILEIOT(1:2).EQ.'DS') THEN
          ! Genotype file names and locations
          CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'CFILE',cufile)
          CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'CDIR',pathcr)
          CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'EFILE',ecfile)
          CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'EDIR',pathec)
          CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'SPFILE',spfile)
          CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'SPDIR',pathsp)
          ! A-file location
          CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'ADIR',fileadir)

          ! Additional controls that not handled by CSM
          ! To get name and location of x-file to -> special controls.
          CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'AFILE',filea)

!     CHP 2021-03-19
          IF (INDEX(FILEADIR,"-99") > 0) THEN
            FILEX = FILEA(1:TVILENT(FILEA))
          ELSE
            FILEX=FILEADIR(1:TVILENT(FILEADIR))//FILEA(1:TVILENT(FILEA))
          ENDIF

          CALL LTRIM2 (FILEX,filenew)
          FILELEN = TVILENT(FILENEW)
          FILENEW(FILELEN:FILELEN)= 'X'
          FILEX = FILENEW
          ! Experimental controls
          CALL XREADC(FILEX,TN,RN,SN,ON,CN,'LLIFE',cfllflife)
        ENDIF

!-----------------------------------------------------------------------
!       Correct case and dates
!-----------------------------------------------------------------------

        CALL CSUCASE (CROP)
        CALL CSUCASE (EXCODE)

C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY
        !HLAST = CSYEARDOY(hlast)
        CALL Y4K_DOY(hyrdoy(i),FILEX,0,ERRKEY,3)
        !HFIRST = CSYEARDOY(hfirst)
        CALL Y4K_DOY(hfirst,FILEX,0,ERRKEY,3)
        !PWDINF = CSYEARDOY(pwdinf)
        CALL Y4K_DOY(pwdinf,FILEX,0,ERRKEY,3)
        !PWDINL = CSYEARDOY(pwdinl)
        CALL Y4K_DOY(pwdinl,FILEX,0,ERRKEY,3)
        DO L = 1,DINX
          !DIDAT(L) = CSYEARDOY(DIDAT(L))
          CALL Y4K_DOY(DIDAT(L),FILEX,0,ERRKEY,3)
        ENDDO
        DO L = 1,DCNX
          !DCDAT(L) = CSYEARDOY(DCDAT(L))
          CALL Y4K_DOY(DCDAT(L),FILEX,0,ERRKEY,3)
        ENDDO

!        CALL CSYR_DOY(PWDINF,PWYEARF,PWDOYF)
!        CALL CSYR_DOY(PWDINL,PWYEARL,PWDOYL)
!        CALL CSYR_DOY(HFIRST,HYEARF,HDOYF)
!        CALL CSYR_DOY(HLAST,HYEARL,HDOYL)
!        CALL CSYR_DOY(PDATE,PLYEARTMP,PLDAY)
        CALL YR_DOY(PWDINF,PWYEARF,PWDOYF)
        CALL YR_DOY(PWDINL,PWYEARL,PWDOYL)
        CALL YR_DOY(HFIRST,HYEARF,HDOYF)
        CALL YR_DOY(HLAST,HYEARL,HDOYL)
        CALL YR_DOY(PDATE,PLYEARTMP,PLDAY)
        PLYEARREAD = PLYEARTMP

!-----------------------------------------------------------------------
!       Insert defaults for missing non-critical aspects
!-----------------------------------------------------------------------

        IF (digfac(1).LT.0.0) digfac(1) = 1.0
        IF (digfac(2).LT.0.0) digfac(2) = 1.0
        IF (digfac(3).LT.0.0) digfac(3) = 1.0
        IF (plmage.LE.-98.0) plmage = 0.0
        IF (hnumber.LE.0) THEN 
          hpcf = 100.0
          hbpcf = 0.0
        ENDIF  

!-----------------------------------------------------------------------
!       Set planting/harvesting dates (Will change if runs repeated)
!-----------------------------------------------------------------------

!         CHP 5/4/09 - for DSSAT runs, always set PLYEAR = YEAR
!         CHP 09/28/09 - account for planting date >> simulation date.
!        LPM 07/17/20 - account for simulation date when is a year before planting date
!        Avoid wrong value of yeardoyharf
        IF (FILEIOT(1:2) == 'DS' .AND. YEAR > PLYEAR) THEN
            IF (YEAR < PLYEARREAD) THEN
                PLYEAR = PLYEARREAD
                PLYEARTMP = PLYEARREAD
            ELSE
                PLYEAR = YEAR
                PLYEARTMP = YEAR
            ENDIF
        ENDIF

        ! Check final harvest date for seasonal runs        
!        CALL CSYR_DOY(YEARDOYHARF,HYEAR,HDAY)
        CALL YR_DOY(YEARDOYHARF,HYEAR,HDAY)
        PLTOHARYR = HYEAR - PLYEARREAD
        ! Upgrade harvest date for seasonal and sequential runs
        !yeardoyharf = (plyear+pltoharyr)*1000 +hday
!      LPM 10FEB2021 Move yeardoyharf after updating the PLYEAR when date of simulation is 
!      the year before planting

!       IF (IPLTI.NE.'A') THEN
        IF (IPLTI.NE.'A' .AND. IPLTI.NE.'F') THEN
          IF (PLDAY.GE.DOY) THEN
            PLYEARDOYT = PLYEARTMP*1000 + PLDAY
          ELSEIF (PLDAY.LT.DOY) THEN
            PLYEARDOYT = (YEAR+1)*1000 + PLDAY
            PLYEAR = YEAR+1
          ENDIF
        ELSE
          PLYEARDOYT = 9999999
          IF (PWDINF.GT.0 .AND. PWDINF.LT.YEARDOY) THEN
            TVI1 = INT((YEARDOY-PWDINF)/1000)
            PWDINF = PWDINF + TVI1*1000
            PWDINL = PWDINL + TVI1*1000
            IF (HFIRST.GT.0) HFIRST = HFIRST + TVI1*1000
            IF (HLAST.GT.0)  HLAST  = HLAST + (TVI1+1)*1000
          ENDIF
        ENDIF
        yeardoyharf = (plyear+pltoharyr)*1000 +hday

!-----------------------------------------------------------------------
!       Set control flags if not already done
!-----------------------------------------------------------------------

        IF (CFLLFLIFE.EQ.'T'.OR.CFLLFLIFE.EQ.'P'
     &                      .OR.CFLLFLIFE.EQ.'D') THEN
          CFLLFLIFE = CFLLFLIFE
        ELSE  
          CFLLFLIFE = 'T'  ! Default to thermal time 
        ENDIF

!-----------------------------------------------------------------------
!       Create genotype file names
!-----------------------------------------------------------------------

        IF (FILEIOT(1:2).EQ.'DS') THEN
          ! Cultivar
          PATHL = INDEX(PATHCR,BLANK)
          IF (PATHL.LE.5.OR.PATHCR(1:3).EQ.'-99') THEN
            CUDIRFLE = CUFILE
          ELSE
            IF (PATHCR(PATHL-1:PATHL-1) .NE. SLASH) THEN
              CUDIRFLE = PATHCR(1:(PATHL-1)) // SLASH // CUFILE
            ELSE
              CUDIRFLE = PATHCR(1:(PATHL-1)) // CUFILE
            ENDIF
          ENDIF
          ! Ecotype
          PATHL = INDEX(PATHEC,BLANK)
          IF (PATHL.LE.5.OR.PATHEC(1:3).EQ.'-99') THEN
            ECDIRFLE = ECFILE
          ELSE
            IF (PATHEC(PATHL-1:PATHL-1) .NE. SLASH) THEN
              ECDIRFLE = PATHEC(1:(PATHL-1)) // SLASH // ECFILE
            ELSE
              ECDIRFLE = PATHEC(1:(PATHL-1)) // ECFILE
            ENDIF
          ENDIF
          ! Species
          PATHL = INDEX(PATHSP,BLANK)
          IF (PATHL.LE.5.OR.PATHSP(1:3).EQ.'-99') THEN
            SPDIRFLE = SPFILE
          ELSE
            IF (PATHSP(PATHL-1:PATHL-1) .NE. SLASH) THEN
              SPDIRFLE = PATHSP(1:(PATHL-1)) // SLASH // SPFILE
            ELSE
              SPDIRFLE = PATHSP(1:(PATHL-1)) // SPFILE
            ENDIF
          ENDIF
        ELSE
          IF (CUDIRFLE.NE.CUDIRFLPREV .OR. VARNO.NE.VARNOPREV) THEN
            ! Cultivar
            CUFILE = CROP//MODNAME(3:8)//'.CUL'
            INQUIRE (FILE = CUFILE,EXIST = FFLAG)
            IF (FFLAG) THEN
              CUDIRFLE = CUFILE
            ELSE
              CALL FINDDIR (FNUMTMP,CFGDFILE,'CRD',CUFILE,CUDIRFLE)
            ENDIF
            IF (RNMODE.EQ.'G'.OR.RNMODE.EQ.'T') THEN
              CUFILE = 'GENCALC2.CUL'
              CUDIRFLE = ' '
              CUDIRFLE(1:12) = CUFILE
            ENDIF
            ! Ecotype
            ECFILE = CROP//MODNAME(3:8)//'.ECO'
            INQUIRE (FILE = ECFILE,EXIST = FFLAG)
            IF (FFLAG) THEN
              ECDIRFLE = ECFILE
            ELSE
              CALL FINDDIR (FNUMTMP,CFGDFILE,'CRD',ECFILE,ECDIRFLE)
            ENDIF
            IF (RNMODE.EQ.'G'.OR.RNMODE.EQ.'T') THEN
              ECFILE = 'GENCALC2.ECO'
              ECDIRFLE = ' '
              ECDIRFLE(1:12) = ECFILE
            ENDIF
            ! Species
            SPFILE = CROP//MODNAME(3:8)//'.SPE'
            INQUIRE (FILE = SPFILE,EXIST = FFLAG)
            IF (FFLAG) THEN
              SPDIRFLE = SPFILE
            ELSE
              CALL FINDDIR (FNUMTMP,CFGDFILE,'CRD',SPFILE,SPDIRFLE)
            ENDIF
          ENDIF
        ENDIF     ! End Genotype file names creation

!-----------------------------------------------------------------------
!       Check for cultivar number, genotype files existance and version
!-----------------------------------------------------------------------

        IF (VARNO.EQ.'-99   ') THEN
          OPEN (UNIT = FNUMERR,FILE = 'ERROR.OUT')
          WRITE(fnumerr,*)' '
          WRITE(fnumerr,*)'Cultivar number not found '
          WRITE(fnumerr,*)'Maybe an error in the the X-file headings'
          WRITE(fnumerr,*)'(eg.@-line dots connected to next header)'
          WRITE(fnumerr,*)' (OR sequence or crop components > 1,no 1)'
          WRITE(fnumerr,*)'Please check'
          WRITE (*,*) ' Problem reading the X-file'
          WRITE (*,*) ' Cultivar number not found '
          WRITE (*,*) ' Maybe an error in the the X-file headings'
          WRITE (*,*) ' (eg.@-line dots connected to next header)'
          WRITE (*,*) ' (OR sequence or crop components > 1,no 1)'
          WRITE (*,*) ' Program will have to stop'
          WRITE (*,*) ' Check WORK.OUT for details'
          CLOSE (fnumerr)
          STOP ' '
        ENDIF

        INQUIRE (FILE = CUDIRFLE,EXIST = FFLAG)
        IF (.NOT.(FFLAG)) THEN
!          WRITE(fnumwrk,*) ' '
!          WRITE(fnumwrk,*) 'Cultivar file not found!     '
!          WRITE(fnumwrk,*) 'File sought was:          '  
!          WRITE(fnumwrk,*) Cudirfle(1:78)
!          WRITE(fnumwrk,*) 'Will search in the working directory for:'
          CUDIRFLE = CUFILE
!          WRITE(fnumwrk,*)  Cudirfle(1:78)
          INQUIRE (FILE = CUDIRFLE,EXIST = FFLAG)
          IF (.NOT.(FFLAG)) THEN
            OPEN (UNIT = FNUMERR,FILE = 'ERROR.OUT')
            WRITE(fnumerr,*) ' '
            WRITE(fnumerr,*) 'Cultivar file not found!     '
            WRITE(fnumerr,*) 'File sought was:          '  
            WRITE(fnumerr,*) Cudirfle(1:78)
            WRITE(fnumerr,*) 'Please check'
            WRITE (*,*) ' Cultivar file not found!     '
            WRITE(*,*) 'File sought was:          '
            WRITE(*,*) Cudirfle(1:78)
            WRITE(*,*) ' Program will have to stop'
            CLOSE (fnumerr)
            STOP ' '
          ENDIF
        ENDIF

        INQUIRE (FILE = ECDIRFLE,EXIST = FFLAGEC)
        IF (.NOT.(FFLAGEC)) THEN
!          WRITE(fnumwrk,*) ' '
!          WRITE(fnumwrk,*) 'Ecotype file not found!     '
!          WRITE(fnumwrk,*) 'File sought was: ',Ecdirfle(1:60)  
          ECDIRFLE = ECFILE
!          WRITE(fnumwrk,*) 
!     &     'Will search in the working directory for:',Ecdirfle(1:60)
          INQUIRE (FILE = ECDIRFLE,EXIST = FFLAGEC)
          IF (.NOT.(FFLAGEC)) THEN
            OPEN (UNIT = FNUMERR,FILE = 'ERROR.OUT')
!            WRITE(fnumwrk,*) 'File not found in working directory!'
!            WRITE(fnumwrk,*) 'Please check'
            WRITE(*,*) ' Ecotype file not found!     '
            WRITE(*,*) ' File sought was: ',Ecdirfle(1:60)
            WRITE(*,*) ' Program will have to stop'
            STOP ' '
          ENDIF
        ENDIF

        INQUIRE (FILE = SPDIRFLE,EXIST = FFLAG)
        IF (.NOT.(FFLAG)) THEN
          OPEN (UNIT = FNUMERR,FILE = 'ERROR.OUT')
          WRITE(fnumerr,*) ' '
          WRITE(fnumerr,*) 'Species file not found!     '
          WRITE(fnumerr,*) 'File sought was:          '
          WRITE(fnumerr,*) Spdirfle
          WRITE(fnumerr,*) 'Please check'
          WRITE(*,*) ' Species file not found!     '
          WRITE(*,*) 'File sought was:          '
          WRITE(*,*) Spdirfle
          WRITE(*,*) ' Program will have to stop'
          CLOSE (fnumerr)
          STOP ' '
        ENDIF
        
!-----------------------------------------------------------------------
!       Read genotype (cultivar,ecotype,species) 
!-----------------------------------------------------------------------

        ! Cultivar coefficients initialized
        pdl = 0.0

        ! Ecotype coefficients re-set
        rspco = -99
        canhts = -99
        dayls = -99
        srnpcs = -99
        srprs = -99
        srprs = -99
        lseni = -99
        dusri = -99
        parue = -99
        paru2 = -99
        la1s = -99
        laxs = -99
        laws = -99
        rdgs = -99
        nfgu = -99
        nfgl = -99
        no3cf = -99
        h2ocf = -99
        rtno3 = -99
        rtnh4 = -99

        ! Species coefficients re-set
        pd = -99
        dusri = -99 
        llifa = -99 
        srfr = -99 
        rdgs = -99 
        laxs = -99 
        rlwr = -99 
        nfgl = -99 
        nfgu = -99 
        nfpu = -99 
        nfpl = -99 
        kcan = -99 
        lawcf = -99 
        lawmnfr = -99
        lawtr = -99
        lawts = -99
        lawmnfr = -99
        dfpe = -99 
        ppexp = -99 
        nh4mn = -99
        no3mn = -99
        rsfrs = -99

        IF (FILEIOT(1:2).EQ.'DS') THEN
          IF (RNMODE.NE.'T') CALL FVCHECK(CUDIRFLE,GENFLCHK)
          CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'ECO#',econo)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS1',dayls(1))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS2',dayls(2))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS3',dayls(3))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS4',dayls(4))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS5',dayls(5))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS6',dayls(6))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS7',dayls(7))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS8',dayls(8))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS9',dayls(9))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPEXP',ppexp) ! Trial
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPFPE',dfpe)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PHINT',phints)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P1',pd(1))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P2',pd(2))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P3',pd(3))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P4',pd(4))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P5',pd(5))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P6',pd(6))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P7',pd(7))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P8',pd(8))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P9',pd(9))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'B01ND',pdl(1))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'B12ND',pdl(2))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'B23ND',pdl(3))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'B34ND',pdl(4))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'B45ND',pdl(5))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'B56ND',pdl(6))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'B67ND',pdl(7))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'B78ND',pdl(8))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'B89ND',pdl(9))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LLIFA',llifa)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'STFR',swfrs)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'SR#WT',srnow)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'SRFR',srfr)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LAXS',laxs)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LAXND',laxno)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LAXN2',laxn2)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LAFS',lafs)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LAFND',lafnd)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'SLAS',laws)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFPU',nfpu)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFPL',nfpl)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFGU',nfgu)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFGL',nfgl)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'RDGS',rdgs)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'RLWR',rlwr)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PARUE',parue)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PARU2',paru2)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LA1S',la1s)
          ! New (Nov 2011) N uptake variables
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NUPNF',no3cf)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NUPWF',h2ocf)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'RTNUP',rtno3)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'RTNH4',rtnh4)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NO3MN',no3mn)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NH4MN',nh4mn)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'HMPC',hmpc)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LPEFR',lpefr)
        ELSE
          IF (RNMODE.NE.'T') CALL FVCHECK(CUDIRFLE,GENFLCHK)
          CALL CUREADC (CUDIRFLE,VARNO,'ECO#',econo)
          CALL CUREADR (CUDIRFLE,VARNO,'PPS1',dayls(1))
          CALL CUREADR (CUDIRFLE,VARNO,'PPS2',dayls(2))
          CALL CUREADR (CUDIRFLE,VARNO,'PPS3',dayls(3))
          CALL CUREADR (CUDIRFLE,VARNO,'PPS4',dayls(4))
          CALL CUREADR (CUDIRFLE,VARNO,'PPS5',dayls(5))
          CALL CUREADR (CUDIRFLE,VARNO,'PPS6',dayls(6))
          CALL CUREADR (CUDIRFLE,VARNO,'PPS7',dayls(7))
          CALL CUREADR (CUDIRFLE,VARNO,'PPS8',dayls(8))
          CALL CUREADR (CUDIRFLE,VARNO,'PPS9',dayls(9))
          CALL CUREADR (CUDIRFLE,VARNO,'PPEXP',ppexp)! Trial
          CALL CUREADR (CUDIRFLE,VARNO,'PPFPE',dfpe)
          CALL CUREADR (CUDIRFLE,VARNO,'P1',pd(1))
          CALL CUREADR (CUDIRFLE,VARNO,'P2',pd(2))
          CALL CUREADR (CUDIRFLE,VARNO,'P3',pd(3))
          CALL CUREADR (CUDIRFLE,VARNO,'P4',pd(4))
          CALL CUREADR (CUDIRFLE,VARNO,'P5',pd(5))
          CALL CUREADR (CUDIRFLE,VARNO,'P6',pd(6))
          CALL CUREADR (CUDIRFLE,VARNO,'P7',pd(7))
          CALL CUREADR (CUDIRFLE,VARNO,'P8',pd(8))
          CALL CUREADR (CUDIRFLE,VARNO,'P9',pd(9))
          CALL CUREADR (CUDIRFLE,VARNO,'B01ND',pdl(1))
          CALL CUREADR (CUDIRFLE,VARNO,'B12ND',pdl(2))
          CALL CUREADR (CUDIRFLE,VARNO,'B23ND',pdl(3))
          CALL CUREADR (CUDIRFLE,VARNO,'B34ND',pdl(4))
          CALL CUREADR (CUDIRFLE,VARNO,'B45ND',pdl(5))
          CALL CUREADR (CUDIRFLE,VARNO,'B56ND',pdl(6))
          CALL CUREADR (CUDIRFLE,VARNO,'B67ND',pdl(7))
          CALL CUREADR (CUDIRFLE,VARNO,'B78ND',pdl(8))
          CALL CUREADR (CUDIRFLE,VARNO,'B89ND',pdl(9))
          CALL CUREADR (CUDIRFLE,VARNO,'PHINT',phints)
          CALL CUREADR (CUDIRFLE,VARNO,'LLIFA',llifa)
          CALL CUREADR (CUDIRFLE,VARNO,'STFR',swfrs)
          CALL CUREADR (CUDIRFLE,VARNO,'SR#WT',srnow)
          CALL CUREADR (CUDIRFLE,VARNO,'SRFR',srfr)
          CALL CUREADR (CUDIRFLE,VARNO,'LAXS',laxs)
          CALL CUREADR (CUDIRFLE,VARNO,'LAXND',laxno)
          CALL CUREADR (CUDIRFLE,VARNO,'LAXN2',laxn2)
          CALL CUREADR (CUDIRFLE,VARNO,'LAFS',lafs)
          CALL CUREADR (CUDIRFLE,VARNO,'LAFND',lafnd)
          CALL CUREADR (CUDIRFLE,VARNO,'SLAS',laws)
          CALL CUREADR (CUDIRFLE,VARNO,'NFPU',nfpu)
          CALL CUREADR (CUDIRFLE,VARNO,'NFPL',nfpl)
          CALL CUREADR (CUDIRFLE,VARNO,'NFGU',nfgu)
          CALL CUREADR (CUDIRFLE,VARNO,'NFGL',nfgl)
          CALL CUREADR (CUDIRFLE,VARNO,'RDGS',rdgs)
          CALL CUREADR (CUDIRFLE,VARNO,'RLWR',rlwr)
          CALL CUREADR (CUDIRFLE,VARNO,'PARUE',parue)
          CALL CUREADR (CUDIRFLE,VARNO,'PARU2',paru2)
          CALL CUREADR (CUDIRFLE,VARNO,'LA1S',la1s)
          CALL CUREADR (CUDIRFLE,VARNO,'LPEFR',lpefr)
          ! New (Nov 2011) N uptake variables
          CALL CUREADR (CUDIRFLE,VARNO,'NUPNF',no3cf)
          CALL CUREADR (CUDIRFLE,VARNO,'NUPWF',h2ocf)
          CALL CUREADR (CUDIRFLE,VARNO,'RTNUP',rtno3)
          CALL CUREADR (CUDIRFLE,VARNO,'RTNH4',rtnh4)
          CALL CUREADR (CUDIRFLE,VARNO,'NO3MN',no3mn)
          CALL CUREADR (CUDIRFLE,VARNO,'NH4MN',nh4mn)
          CALL CUREADR (CUDIRFLE,VARNO,'HMPC',hmpc)
        ENDIF     ! End Cultivar reads

!-----------------------------------------------------------------------
!       Read ecotype information
!-----------------------------------------------------------------------

C       CALL FVCHECK(ECDIRFLE,GENFLCHK)
C-GH As per Tony Hunt 2017 for GenCalc
        IF (RNMODE.NE.'T') CALL FVCHECK(ECDIRFLE,GENFLCHK)
        CALL ECREADR (ECDIRFLE,ECONO,'HTSTD',canhts)
        CALL ECREADR (ECDIRFLE,ECONO,'SRN%S',srnpcs)
        IF (SRNPCS.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'SRP%S',srprs)
        IF (SRPRS.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'SRPRS',srprs)
        CALL ECREADR (ECDIRFLE,ECONO,'LSENI',lseni)
        CALL ECREADR (ECDIRFLE,ECONO,'DUSRI',dusri)
        ! LAH Following set up to allow for change in stem fraction
        ! Currently not used ... just one stem fraction (STFR)
        CALL ECREADR (ECDIRFLE,ECONO,'SWFRX',swfrx)
        CALL ECREADR (ECDIRFLE,ECONO,'SWFRN',swfrn)
        CALL ECREADR (ECDIRFLE,ECONO,'SWFNL',swfrnl)
        CALL ECREADR (ECDIRFLE,ECONO,'SWFXL',swfrxl)
        CALL ECREADR (ECDIRFLE,ECONO,'SLACF',lawcf)
        CALL ECREADR (ECDIRFLE,ECONO,'KCAN',kcan)
        CALL ECREADR (ECDIRFLE,ECONO,'BR1FX',brfx(1))
        CALL ECREADR (ECDIRFLE,ECONO,'BR2FX',brfx(2))
        CALL ECREADR (ECDIRFLE,ECONO,'BR3FX',brfx(3))
        CALL ECREADR (ECDIRFLE,ECONO,'BR4FX',brfx(4))
        CALL ECREADR (ECDIRFLE,ECONO,'BR5FX',brfx(5))
        CALL ECREADR (ECDIRFLE,ECONO,'BR6FX',brfx(6))
        ! Following may have been (temporarily) in the CUL file
        ! Radiation use efficiency
        IF (PARUE.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'PARUE',parue)
        IF (PARU2.LT.-89.0) CALL ECREADR (ECDIRFLE,ECONO,'PARU2',paru2)
        ! Leaf area
        IF (LA1S.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'LA1S',la1s)
        IF (LAXS.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'LAXS',laxs)
        IF (LAWS.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'SLAS',laws)
        ! Roots
        IF (RDGS.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'RDGS',rdgs)
        ! Reduction factors
        IF (NFGU.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'NFGU',nfgu)
        IF (NFGL.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'NFGL',nfgl)
        ! N uptake
        IF (NO3CF.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'NUPNF',no3cf)
        IF (H2OCF.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'NUPWF',h2ocf)
        IF (RTNO3.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'RTNUP',rtno3)
        IF (RTNH4.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'RTNH4',rtnh4)
        IF (NO3MN.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'NO3MN',no3mn)
        IF (NH4MN.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'NH4MN',nh4mn)
        
!-----------------------------------------------------------------------
!       Read species information
!-----------------------------------------------------------------------

        CALL FVCHECK(SPDIRFLE,GENFLCHK)
        CALL SPREADR (SPDIRFLE,'CRFR' ,crfr)
        CALL SPREADR (SPDIRFLE,'CO2CC',co2compc)
        CALL SPREADR (SPDIRFLE,'CO2EX',co2ex)
        CALL SPREADR (SPDIRFLE,'HDUR' ,hdur)
        CALL SPREADR (SPDIRFLE,'SLAFF',lawff)
        CALL SPREADR (SPDIRFLE,'SLATR',lawtr)
        CALL SPREADR (SPDIRFLE,'SLATS',lawts)
        CALL SPREADR (SPDIRFLE,'SLAWR',lawwr)
        CALL SPREADR (SPDIRFLE,'LLIFG',llifg)
        CALL SPREADR (SPDIRFLE,'LLIFS',llifs)
        CALL SPREADR (SPDIRFLE,'LLIFX',llifx)
        CALL SPREADR (SPDIRFLE,'LLIG%',lligp)
        CALL SPREADR (SPDIRFLE,'LLOSA',llosa)
        CALL SPREADR (SPDIRFLE,'LWLOS',lwlos)
        CALL SPREADR (SPDIRFLE,'NFSU' ,nfsu)
        CALL SPREADR (SPDIRFLE,'LPEAW',lpeaw)
        CALL SPREADR (SPDIRFLE,'NCRG',ncrg)
        CALL SPREADR (SPDIRFLE,'NTUPF',ntupf)
        CALL SPREADR (SPDIRFLE,'PARIX',parix)
        CALL SPREADR (SPDIRFLE,'LAIXX',laixx)
        CALL SPREADR (SPDIRFLE,'PARFC',parfc)
        CALL SPREADR (SPDIRFLE,'PEMRG',pecm)
        CALL SPREADR (SPDIRFLE,'PGERM',pgerm)
        CALL SPREADR (SPDIRFLE,'PHSV' ,phsv)
        CALL SPREADR (SPDIRFLE,'PHNTF',phintfac)
        CALL SPREADR (SPDIRFLE,'PHTV' ,phtv)
        CALL SPREADR (SPDIRFLE,'PPTHR',ppthr)
        CALL SPREADR (SPDIRFLE,'PTFA' ,ptfa)
        CALL SPREADR (SPDIRFLE,'PTFMN',ptfmn)
        CALL SPREADR (SPDIRFLE,'PTFMX',ptfmx)
        CALL SPREADR (SPDIRFLE,'RATM' ,ratm)
        CALL SPREADR (SPDIRFLE,'RCROP',rcrop)
        CALL SPREADR (SPDIRFLE,'RDGAF',rdgaf)
        CALL SPREADR (SPDIRFLE,'RWULF',rlfwu)
        CALL SPREADR (SPDIRFLE,'RLIG%',rligp)
        CALL SPREADR (SPDIRFLE,'RRESP',rresp)
        CALL SPREADR (SPDIRFLE,'RS%LX',rsclx)
        CALL SPREADR (SPDIRFLE,'RS%O' ,rspco)
        CALL SPREADR (SPDIRFLE,'RSEN' ,rsen)
        IF (RSEN.LT.0.0) CALL SPREADR (SPDIRFLE,'RSEN%' ,rsen)
        CALL SPREADR (SPDIRFLE,'RSFPL',rsfpl)
        CALL SPREADR (SPDIRFLE,'RSFPU',rsfpu)
        CALL SPREADR (SPDIRFLE,'RSUSE',rsuse)
        CALL SPREADR (SPDIRFLE,'RTUFR',rtufr)
        CALL SPREADR (SPDIRFLE,'RWUMX',rwumx)
        CALL SPREADR (SPDIRFLE,'RWUPM',rwupm)
        CALL SPREADR (SPDIRFLE,'SAWS' ,saws)
        CALL SPREADR (SPDIRFLE,'SDDUR',sddur)
        CALL SPREADR (SPDIRFLE,'SDN%',sdnpci)
        CALL SPREADR (SPDIRFLE,'SDRS%',sdrsf)
        CALL SPREADR (SPDIRFLE,'SDWT' ,sdsz)
        CALL SPREADR (SPDIRFLE,'SLIG%',sligp)
        CALL SPREADR (SPDIRFLE,'SGRO2',shgr(2))
        CALL SPREADR (SPDIRFLE,'TPAR' ,tpar)
        CALL SPREADR (SPDIRFLE,'TSRAD',tsrad)
        CALL SPREADR (SPDIRFLE,'WFEU' ,wfeu)
        CALL SPREADR (SPDIRFLE,'WFGEU',wfgem)
        CALL SPREADR (SPDIRFLE,'WFGU' ,wfgu)
        CALL SPREADR (SPDIRFLE,'WFGL' ,wfgl)
        CALL SPREADR (SPDIRFLE,'WFPU' ,wfpu)
        CALL SPREADR (SPDIRFLE,'WFPL' ,wfpl)
        CALL SPREADR (SPDIRFLE,'WFRGU',wfrtg)
        CALL SPREADR (SPDIRFLE,'WFSU' ,wfsu)
        CALL SPREADR (SPDIRFLE,'NLAB%',nlabpc)
        ! LAH Following set up to allow for change in stem fraction
        ! Currently not used ... just one stem fraction (STFR)
        IF (SWFRN.LE.0.0) CALL SPREADR (SPDIRFLE,'SWFRN',swfrn)
        IF (SWFRNL.LE.0.0) CALL SPREADR (SPDIRFLE,'SWFRNL',swfrnl)
        IF (SWFRXL.LE.0.0) CALL SPREADR (SPDIRFLE,'SWFXL',swfrxl)
        IF (SWFRX.LE.0.0) CALL SPREADR (SPDIRFLE,'SWFRX',swfrx)
        ! Following may be temporarily in ECO or CUL file
        IF (PD(9).LE.0.0) CALL SPREADR (SPDIRFLE,'P9',pd(9))
        IF (DUSRI.LT.0.0) CALL SPREADR (SPDIRFLE,'DUSRI',dusri)
        IF (LLIFA.LE.0.0) CALL SPREADR (SPDIRFLE,'LLIFA',llifa)

        IF (SRFR.LE.0.0) CALL SPREADR (SPDIRFLE,'SRFR',srfr)
        IF (RDGS.LE.0.0) CALL SPREADR (SPDIRFLE,'RDGS',rdgs)
        IF (LAXS.LE.0.0) CALL SPREADR (SPDIRFLE,'LAXS',laxs)
        IF (RLWR.LE.0.0) CALL SPREADR (SPDIRFLE,'RLWR',rlwr)
        IF (NFGL.LT.0.0) CALL SPREADR (SPDIRFLE,'NFGL',nfgl)
        IF (NLLG.LE.0.0) CALL SPREADR (SPDIRFLE,'NLLG',nllg)
        IF (NFGU.LE.0.0) CALL SPREADR (SPDIRFLE,'NFGU',nfgu)
        IF (NFPU.LE.0.0) CALL SPREADR (SPDIRFLE,'NFPU',nfpu)
        IF (NFPL.LE.0.0) CALL SPREADR (SPDIRFLE,'NFPL',nfpl)
        IF (KCAN.LE.0.0) CALL SPREADR (SPDIRFLE,'KCAN',kcan)
        IF (LAWCF.LE.0.0) CALL SPREADR (SPDIRFLE,'SLACF',lawcf)
        IF (LAWMNFR.LE.0.0) CALL SPREADR (SPDIRFLE,'SLAMN',lawmnfr)
        IF (DFPE.LT.0.0) CALL SPREADR (SPDIRFLE,'PPFPE',dfpe)
        IF (PPEXP.LT.0.0) CALL SPREADR (SPDIRFLE,'PPEXP',ppexp)
        ! N uptake 
        IF (NO3CF.LT.0.0) CALL SPREADR (SPDIRFLE,'NUPNF',no3cf)
        IF (H2OCF.LT.0.0) CALL SPREADR (SPDIRFLE,'NUPWF',h2ocf)
        IF (RTNO3.LT.0.0) CALL SPREADR (SPDIRFLE,'RTNUP',rtno3)
        IF (RTNH4.LT.0.0) CALL SPREADR (SPDIRFLE,'RTNH4',rtnh4)
        IF (NO3MN.LT.0.0) CALL SPREADR (SPDIRFLE,'NO3MN',no3mn)
        IF (NH4MN.LT.0.0) CALL SPREADR (SPDIRFLE,'NH4MN',nh4mn)
        IF (RSFRS.LT.0.0) CALL SPREADR (SPDIRFLE,'RSFRS',rsfrs)

        CALL SPREADC (SPDIRFLE,'HPROD',hprod)
        CALL SPREADC (SPDIRFLE,'PPSEN',ppsen)

        CALL SPREADRA (SPDIRFLE,'LN%S','2',lnpcs)
        CALL SPREADRA (SPDIRFLE,'RN%S','2',rnpcs)
        CALL SPREADRA (SPDIRFLE,'SN%S','2',snpcs)
        CALL SPREADRA (SPDIRFLE,'LN%MN','2',lnpcmn)
        CALL SPREADRA (SPDIRFLE,'RN%MN','2',rnpcmn)
        CALL SPREADRA (SPDIRFLE,'SN%MN','2',snpcmn)

        CALL SPREADRA (SPDIRFLE,'CHT%','10',chtpc)
        CALL SPREADRA (SPDIRFLE,'CLA%','10',clapc)

        CALL SPREADRA (SPDIRFLE,'CO2RF','10',co2rf)
        CALL SPREADRA (SPDIRFLE,'CO2F','10',co2f)

        ! Temperature responses
        CALL SPREADRA (SPDIRFLE,'TRDV1','4',trdv1)
        IF (trdv1(1).LT.-98.0) THEN
          OPEN (UNIT = FNUMERR,FILE = 'ERROR.OUT')
          WRITE(fnumerr,*) ' '
          WRITE(fnumerr,*) ' No temp response data for development'
          WRITE(fnumerr,*) ' Please check'
          WRITE(*,*) ' No temperature response data for development'
          WRITE(*,*) ' Program will have to stop'
          CLOSE (fnumerr)
          STOP ' '
        ENDIF        
        CALL SPREADRA (SPDIRFLE,'TRDV2','4',trdv2)
        IF (trdv2(1).LT.-98.0) TRDV2 = TRDV1

        CALL SPREADRA (SPDIRFLE,'TRGEM','4',trgem)
        CALL SPREADRA (SPDIRFLE,'TRLFG','4',trlfg)
        CALL SPREADRA (SPDIRFLE,'TRPHS','4',trphs)
        IF (diffacr(1).LT.0.0)
     &   CALL SPREADRA (SPDIRFLE,'DIFFR','3',diffacr)

        CALL SPREADCA (SPDIRFLE,'PSNAME','20',psname)
        CALL SPREADCA (SPDIRFLE,'PSABV','20',psabv)
        CALL SPREADCA (SPDIRFLE,'PSTYP','20',pstyp)

!-----------------------------------------------------------------------
!       Determine 'key' principal and secondary stages,and adjust names
!-----------------------------------------------------------------------

        KEYPSNUM = 0
        PSNUM = 0
        DO L = 1,PSX
          IF (TVILENT(PSTYP(L)).GT.0) THEN
            IF (PSTYP(L).EQ.'K'.OR.PSTYP(L).EQ.'k'.OR.
     &          PSTYP(L).EQ.'M')THEN
              KEYPSNUM = KEYPSNUM + 1
              KEYPS(KEYPSNUM) = L
            ENDIF
            IF (PSABV(L).EQ.'HDAT') HSTG = L
            IF (PSABV(L).EQ.'MDAT') MSTG = L
            PSNUM = PSNUM + 1
          ENDIF
        ENDDO
        ! IF MSTG not found, use maximum principal stage number
        IF (MSTG.LE.0) THEN
          MSTG = KEYPSNUM
        ENDIF
        ! IF HSTG not found, use maximum principal stage number + 1
        IF (HSTG.LE.0) THEN
          HSTG = MSTG+1  
          PSTART(HSTG) = 5000   ! Set to very long cycle
        ENDIF
        ! Check and adjust stage abbreviations (DAT -> DAP)
        DO L = 1,PSNUM
          IF (TVILENT(PSABV(L)).GT.3) THEN
            IF (TVILENT(PSABV(L)).EQ.4) THEN
              DO L1 = 5,1,-1
                IF (L1.GT.1) THEN
                  PSABV(L)(L1:L1) = PSABV(L)(L1-1:L1-1)
                ELSE
                  PSABV(L)(L1:L1) = ' '
                ENDIF
              ENDDO
            ENDIF
            PSABVO(L) = PSABV(L)
            ! DAS -> DAP for output
            PSABVO(L)(5:5) = 'P'
          ENDIF
        ENDDO

!-----------------------------------------------------------------------
!       Calculate/adjust branching tier durations and thresholds
!-----------------------------------------------------------------------
        
        ! Find number of tiers
        MSTG = 0 
        IF (PDL(1).GT.0.0) THEN
          ! If tier durations input as node units,calculte DU for tiers
           DO L = 1,8
             IF (PDL(L).GT.0.0) THEN
               MSTG = L+1
               HSTG = MSTG+1  
             ENDIF 
           ENDDO
          ! Check for missing tier durations and if so use previous
          IF (MSTG.GT.2) THEN
            DO L = 2,MSTG
              IF (PDL(L).LT.0.0) THEN
                PDL(L) = PDL(L-1)
              ENDIF
            ENDDO
          ENDIF  
          ! Calculate leaf # to MSTG
          TVR1 = 0.0
          DO L = 1,MSTG-1
            TVR1 = TVR1 + AMAX1(0.0,PDL(L))
          ENDDO  
          ! Now calculate tier durations in Thermal Units
          TVR2 = 0.0
          LNUMTMP = 0.0
          DSTAGE = 0.0
          DO L = 1,1000
            TVR2 = TVR2 + STDAY
            TVR3 = 1.0/((1.0/PHINTS)*(1.0-AMIN1(.8,PHINTFAC*DSTAGE)))
            ! TVR3 is a temporary name for PHINT within the loop 
            LNUMTMP = LNUMTMP + STDAY/TVR3
            DSTAGE = AMIN1(1.0,LNUM/TVR1)
            IF(LNUMTMP.GE.PDL(1).AND.
     &           PSTART(2).LE.0.0) PSTART(2) = TVR2
            IF(LNUMTMP.GE.PDL(1)+PDL(2).AND.
     &           PSTART(3).LE.0.0) PSTART(3) = TVR2
            IF(LNUMTMP.GE.PDL(1)+PDL(2)+PDL(3).AND.
     &           PSTART(4).LE.0.0) PSTART(4) = TVR2
            IF(LNUMTMP.GE.PDL(1)+PDL(2)+PDL(3)+PDL(4).AND.
     &           PSTART(5).LE.0.0) PSTART(5) = TVR2
            IF(LNUMTMP.GE.PDL(1)+PDL(2)+PDL(3)+PDL(4)+PDL(5).AND.
     &           PSTART(6).LE.0.0) PSTART(6) = TVR2
            IF(LNUMTMP.GE.PDL(1)+PDL(2)+PDL(3)+PDL(4)+PDL(5)+PDL(6).AND.
     &           PSTART(7).LE.0.0) PSTART(7) = TVR2
            IF(LNUMTMP.GE.
     &           PDL(1)+PDL(2)+PDL(3)+PDL(4)+PDL(5)+PDL(6)+PDL(7).
     &           AND.PSTART(8).LE.0.0) PSTART(8) = TVR2
          ENDDO 
          DO L = 1,MSTG-1
            PD(L) = PSTART(L+1) - PSTART(L)
          ENDDO
          PDL(MSTG) = 200.0
          PD(MSTG) = PDL(MSTG)*PHINTS
          DSTAGE = 0.0
        ELSE 
          ! If tier durations input as developmental units
          DO L = 1,8
            IF (PDL(L).GT.0.0) THEN
              MSTG = L+1
              HSTG = MSTG+1  
            ENDIF 
          ENDDO
          ! Check for missing tier durations and if so use previous
          IF (MSTG.GT.2) THEN
            Ctrnumpd = 0
            DO L = 2,MSTG-1
              IF (PD(L).LT.0.0) THEN
                PDL(L) = PDL(L-1)
                PD(L) = PD(L-1)
                CTRNUMPD = CTRNUMPD + 1
              ENDIF
            ENDDO
          ENDIF  
          IF (CTRNUMPD.GT.0) THEN
            WRITE(MESSAGE(1),'(A11,I2,A23)')
     &      'Duration of',CTRNUMPD,' tiers less than zero. '
            MESSAGE(2)='Used value(s) for preceding tier. '
            CALL WARNING(2,'CSCAS',MESSAGE)
          ENDIF
          PDL(MSTG) = 200.0
          PD(MSTG) = PDL(MSTG)*PHINTS
        ENDIF  

        ! Calculate thresholds
        DO L = 0,MSTG
          PSTART(L) = 0.0
        ENDDO
        DO L = 1,MSTG
          PSTART(L) = PSTART(L-1) + AMAX1(0.0,PD(L-1))
        ENDDO
        
        DUTOMSTG = 0.0
        LNUMTOSTG = 0.0
        DO L = 1, MSTG-1
          IF (PD(L).GT.0.0) THEN
            DUTOMSTG = DUTOMSTG + PD(L)
            LNUMTOSTG(L+1) = LNUMTOSTG(L) + PDL(L)
          ENDIF  
        ENDDO
        DO L = MSTG,PSX-1
          LNUMTOSTG(L+1) = LNUMTOSTG(L) + PDL(L)
        ENDDO
        
        IF (PHINTS.LE.0.0) THEN
            OPEN (UNIT = FNUMERR,FILE = 'ERROR.OUT')
            WRITE(fnumerr,*) ' '
            WRITE(fnumerr,*)
     &       'PHINT <= 0! Please correct genotype files.'
            WRITE(*,*)
     &       ' PHINT <= 0! Please correct genotype files.'
            WRITE(*,*) ' Program will have to stop'
            PAUSE
            CLOSE (fnumerr)
            STOP ' '
        ENDIF
        
        ! Adjust germination duration for seed dormancy
        IF (PLMAGE.LT.0.0.AND.PLMAGE.GT.-90.0) THEN
          PEGD = PGERM - (PLMAGE*STDAY) ! Dormancy has negative age
        ELSE
          PEGD = PGERM
        ENDIF

!-----------------------------------------------------------------------
!       Check and/or adjust coefficients and set defaults if not present
!-----------------------------------------------------------------------

        DO L = 0,1
        LNCXS(L) = LNPCS(L)/100.0 
        SNCXS(L) = SNPCS(L)/100.0 
        RNCXS(L) = RNPCS(L)/100.0 
        LNCMN(L) = LNPCMN(L)/100.0
        SNCMN(L) = SNPCMN(L)/100.0
        RNCMN(L) = RNPCMN(L)/100.0
        ENDDO
                
        IF (LA1S.LE.0.0) THEN
          LA1S = 5.0
          WRITE(MESSAGE(1),'(A47)')
     &    'Initial leaf size (LA1S) missing. Set to 5 cm2.'
          CALL WARNING(1,'CSCAS',MESSAGE)
        ENDIF
        IF (LAFND.GT.0.0.AND.LAFND.LE.LAXNO.OR.LAFND.LT.0) THEN
          LAFND = LAXNO + 10
          WRITE(MESSAGE(1),'(A59)')
     &    'Leaf # for final size missing or < maximum! Set to max+10.'
          CALL WARNING(1,'CSCAS',MESSAGE)
        ENDIF
        IF (DFPE.LT.0.0) THEN  
          DFPE = 1.0
          WRITE(MESSAGE(1),'(A51)')
     &    'Pre-emergence development factor missing. Set to 1.'
          CALL WARNING(1,'CSCAS',MESSAGE)
        ENDIF
        
        ! Stem fraction constant throughout lifecycle
        IF (SWFRS.GT.0.0) THEN
          SWFRX = SWFRS
          SWFRXL = 9999
          SWFRN = SWFRS
          SWFRNL = 0
        ENDIF 
        
        ! Storage root 
        IF (SRFR.LT.0.0) SRFR = 0.0
        IF (HMPC.LE.0.0) HMPC = 50.0
       
        ! Nitrogen uptake                  
        IF (rtno3.le.0.0) RTNO3 = 0.006  ! NO3 uptake/root lgth (mgN/cm)
        IF (rtnh4.le.0.0) RTNH4 = RTNO3  ! NH4 uptake/root lgth (mgN/cm)
        IF (h2ocf.lt.0.0) H2OCF = 1.0    ! H2O conc factor for N uptake 
        IF (no3cf.lt.0.0) NO3CF = 1.0    ! NO3 uptake conc factor (exp)
        IF (nh4cf.lt.0.0) NH4CF = NO3CF  ! NH4 uptake factor (exponent) 

        ! Radiation use efficiency
        IF (PARUE.LE.0.0) PARUE = 2.3
        
        ! Leaves
        IF (PHINTFAC.LE.0.0) PHINTFAC = 0.8
        IF (LA1S.LT.0.0) LA1S = 5.0
        IF (LAXS.LT.0.0) LAXS = 200.0
        IF (LWLOS.LT.0.0) LWLOS = 0.3
        IF (LPEFR.LT.0.0) LPEFR = 0.33
        
        ! Roots
        IF (RDGS.LT.0.0) RDGS = 3.0
        IF (RSEN.LT.0.0) RSEN = 5.0
        
        ! Reduction factor limits
        IF (WFPL.LT.0.0) WFPL = 0.0
        IF (WFPU.LT.0.0) WFPU = 1.0
        IF (WFGL.LT.0.0) WFGL = 0.0
        IF (WFGU.LT.0.0) WFGU = 1.0
        IF (NFPL.LT.0.0) NFPL = 0.0
        IF (NFPU.LT.0.0) NFPU = 1.0
        IF (NFGL.LT.0.0) NFGL = 0.0
        IF (NFGU.LT.0.0) NFGU = 1.0
        IF (NLLG.LE.0.0) NLLG = 0.95
        IF (NFSU.LT.0.0) NFSU = 0.2
        
        ! Various
        IF (RSFRS.LT.0.0) RSFRS = 0.05
        IF (LSENI.LT.0.0) LSENI = 0.0
        IF (PARIX.LE.0.0) PARIX = 0.995
        IF (NTUPF.LT.0.0) NTUPF = 0.2
        IF (PPEXP.LT.0.0) PPEXP = 2.0
        IF (RLFWU.LT.0.0) RLFWU = 0.5  
        IF (RTUFR.LT.0.0) RTUFR = 0.05
        IF (BRFX(1).LE.0.0) BRFX(1) = 3.0
        IF (BRFX(2).LE.0.0) BRFX(2) = 3.0
        IF (BRFX(3).LE.0.0) BRFX(3) = 3.0
        IF (BRFX(4).LE.0.0) BRFX(4) = 3.0
        IF (BRFX(5).LE.0.0) BRFX(5) = 3.0
        IF (BRFX(6).LE.0.0) BRFX(6) = 3.0
        IF (CANHTS.LE.0.0) CANHTS = 200.0
        IF (SHGR(20).LT.0.0) THEN 
          DO L = 3,22
            SHGR(L) = 1.0 !  Shoot sizes relative to main shoot
          ENDDO
        ENDIF

        IF (SLPF.LE.0.0 .OR. SLPF.GT.1.0) SLPF = 1.0
!        IF (SLPF.LT.1.0) THEN
!          WRITE (fnumwrk,*) ' '
!          WRITE (fnumwrk,'(A42,F5.1)')
!     &     ' Soil fertility factor was less than 1.0: ',slpf
!        ENDIF  

        IF (PLPH.LE.0.0) THEN
!          WRITE (fnumwrk,*) ' '
!          WRITE (fnumwrk,'(A27,F6.1,A14)')
!     &     ' Plants per hill <= 0.0 at ',PLPH,'  Reset to 1.0'
          PLPH = 1.0
        ENDIF  

        ! Soil inorganic N uptake aspects
        IF (NO3MN.LT.0.0) NO3MN = 0.5
        IF (NH4MN.LT.0.0) NH4MN = 0.5
        

!-----------------------------------------------------------------------
!       Calculate derived coefficients and set equivalences
!-----------------------------------------------------------------------

        ! Initial leaf growth aspects
        LAPOTX(1) = LA1S
        
        ! If max LAI not read-in,calculate from max interception
        IF (LAIXX.LE.0.0) LAIXX = LOG(1.0-PARIX)/(-KCAN)
        
        PHINT = PHINTS
        
        ! Leaf life
        IF (CFLLFLIFE.EQ.'P')THEN
          ! Read-in as phyllochrons 
          LLIFGTT = LLIFG * PHINT 
          LLIFATT = LLIFA * PHINT 
          LLIFSTT = LLIFS * PHINT 
        ELSEIF (CFLLFLIFE.EQ.'T')THEN
          ! Read-in as thermal time 
          LLIFGTT = LLIFG 
          LLIFATT = LLIFA 
          LLIFSTT = LLIFS
        ELSEIF (CFLLFLIFE.EQ.'D')THEN
          ! Read-in as days. Thermal time for each day set to PHINT
          LLIFGTT = LLIFG * PHINT 
          LLIFATT = LLIFA * PHINT 
          LLIFSTT = LLIFS * PHINT 
        ENDIF  

        ! Extinction coeff for SRAD
        KEP = (KCAN/(1.0-TPAR)) * (1.0-TSRAD)

        ! Photoperiod sensitivities
        DO L = 0,10
          IF (DAYLS(L).LT.0.0) DAYLS(L) = 0.0
        ENDDO
        IF (Dayls(1).EQ.0.0.AND.dfpe.LT.1.0) THEN
          WRITE(MESSAGE(1),'(A36,A41)')
     &    'Cultivar insensitive to photoperiod ',
     &    'but pre-emergence photoperiod factor < 1.' 
          WRITE(MESSAGE(2),'(A40)')
     &    'May be worthwhile to change PPFPE to 1.0'
          CALL WARNING(2,'CSCAS',MESSAGE)
        ENDIF

        ! Shoot growth rates relative to main shoot
        IF (SHGR(20).GE.0.0) THEN
          DO L = 3,22
            IF (L.LT.20) THEN
              SHGR(L) = SHGR(2)-((SHGR(2)-SHGR(20))/18)*(L-2)
            ELSEIF (L.GT.20) THEN
              SHGR(L) = SHGR(20)
            ENDIF  
          ENDDO
        ENDIF

        ! Critical and starting N concentrations
        LNCX = LNCXS(0)
        SNCX = SNCXS(0)
        RNCX = RNCXS(0)
        LNCM = LNCMN(0)
        SNCM = SNCMN(0)
        RNCM = RNCMN(0)

        ! Storage root N  NB.Conversion protein->N factor = 6.25
        IF (SRNPCS.LE.0.0) THEN
          IF(SRPRS.GT.0.0) THEN
            SRNPCS = SRPRS / 6.25
          ELSE
            SRNPCS = 0.65
          ENDIF
        ENDIF       

        ! Height growth
        SERX = CANHTS/PSTART(MSTG)

!-----------------------------------------------------------------------
!       Set coefficients that dependent on input switch
!-----------------------------------------------------------------------

        IF (ISWWATCROP.EQ.'N') THEN
          ! Plant water status effects on growth turned off
          WFGU = 0.0
          WFPU = 0.0
          WFSU = 0.0
          WFRTG = 0.0
        ENDIF

!-----------------------------------------------------------------------
!       Calculate/set initial states
!-----------------------------------------------------------------------

        IF (SDRATE.LE.0.0) SDRATE = SDSZ*PLTPOPP*10.0
        ! Reserves = SDRSF% of seed 
        SEEDRSI = (SDRATE/(PLTPOPP*10.0))*SDRSF/100.0
        SEEDRS = SEEDRSI
        SEEDRSAV = SEEDRS
        SDCOAT = (SDRATE/(PLTPOPP*10.0))*(1.0-SDRSF/100.0)
        ! Seed N calculated from total seed
        SDNAP = (SDNPCI/100.0)*SDRATE
        SEEDNI = (SDNPCI/100.0)*(SDRATE/(PLTPOPP*10.0))
        IF (ISWNIT.NE.'N') THEN
          SEEDN = SEEDNI
        ELSE
          SEEDN = 0.0
          SDNAP = 0.0
          SEEDNI = 0.0
        ENDIF

        ! Water table depth
!       WTDEP = ICWD
        CALL GET('WATER','WTDEP',WTDEP)

        ! Initial shoot and root placement
        IF (PLME.NE.'I') THEN
          IF (PLME.NE.'H') THEN
            IF (PLME.NE.'V') THEN
              WRITE(MESSAGE(1),'(A16,A1,A15,A24)')
     &          'PLANTING method ',PLME,' not an option ',
     &          ' Changed to V (Vertical)'
              CALL WARNING(1,'CSCAS',MESSAGE)
!              WRITE(FNUMWRK,*)' '
!              WRITE(FNUMWRK,'(A17,A1,A15,A24)')
!     &          ' Planting method ',PLME,' not an option ',
!     &          ' Changed to V (Vertical)'
              PLME = 'V'
            ENDIF
          ENDIF
        ENDIF
        IF (SPRL.LE.0.0) THEN
          WRITE(MESSAGE(1),'(A30,A20)')
     &      'Planting stick length <= 00  ',
     &      ' Changed to 25.0 cm '
          CALL WARNING(1,'CSCAS',MESSAGE)
!          WRITE(FNUMWRK,*)' '
!          WRITE(FNUMWRK,'(A31,A20)')
!     &      ' Planting stick length <= 0.0  ',
!     &      ' Changed to 25.0 cm '
          SPRL = 25.0
        ENDIF
        sdepthu = -99.0
        IF (PLME.EQ.'H') THEN
          sdepthu = sdepth
        ELSEIF (PLME.EQ.'I') THEN
          ! Assumes that inclined at 45o
          sdepthu = AMAX1(0.0,sdepth - 0.707*sprl)
        ELSEIF (PLME.EQ.'V') THEN
          sdepthu = AMAX1(0.0,sdepth - sprl)
        ENDIF
        IF (sdepthu.LT.0.0) sdepthu = sdepth

!-----------------------------------------------------------------------
!       Create output descriptors
!-----------------------------------------------------------------------

        ! Run name
        IF (runname(1:6).EQ.'      ' .OR.
     &    runname(1:3).EQ.'-99') runname = tname

        ! Composite run variable
        IF (RUNI.LT.10) THEN
          WRITE (RUNRUNI,'(I3,A1,I1,A3)') RUN,',',RUNI,'   '
        ELSEIF (RUNI.GE.10.AND.RUNI.LT.100) THEN
          WRITE (RUNRUNI,'(I3,A1,I2,A2)') RUN,',',RUNI,'  '
        ELSE
          WRITE (RUNRUNI,'(I3,A1,I3,A1)') RUN,',',RUNI,' '
        ENDIF
        IF (RUN.LT.10) THEN
          RUNRUNI(1:6) = RUNRUNI(3:8)
          RUNRUNI(7:8) = '  '
          ! Below is to give run number only for first run
          IF (RUNI.LE.1) RUNRUNI(2:8) = '       '
        ELSEIF (RUN.GE.10.AND.RUN.LT.100) THEN
          RUNRUNI(1:7) = RUNRUNI(2:8)
          RUNRUNI(8:8) = ' '
          ! Below is to give run number only for first run
          IF (RUNI.LE.1) RUNRUNI(3:8) = '      '
        ENDIF

        ! Composite treatment+run name
        CALL LTRIM (RUNNAME)
        RUNNAME = TRIM(RUNNAME)
        CALL LTRIM (TNAME)
        TNAME = TRIM(TNAME)
        LENTNAME = MIN(15,TVILENT(TNAME))
        LENRNAME = MIN(15,TVILENT(RUNNAME))
        IF (LENRNAME.GT.5) THEN
          TRUNNAME = RUNNAME(1:LENRNAME)//' '//MODNAME
        ELSE
          TRUNNAME = TNAME(1:LENTNAME)//' '//MODNAME
        ENDIF
        IF (MEEXP.EQ.'E') THEN
          CALL LTRIM (TRUNNAME)
          LENTNAME = TVILENT(TRUNNAME)
          TRUNNAME = TRUNNAME(1:LENTNAME)//' EXPERIMENTAL'
        ENDIF

        ! File header
        IF (CN.GT.1) THEN
          IF (TN.LT.10) THEN
            WRITE (OUTHED,7104) RUNRUNI(1:5),EXCODE,TN,RN,CN,TRUNNAME
 7104       FORMAT ('*RUN ',A5,A10,' ',I1,',',I1,' C',I1,' ',A40,'  ')
          ELSEIF (TN.GE.10.AND.TN.LT.100) THEN
            WRITE (OUTHED,7105) RUNRUNI,EXCODE,TN,RN,CN,TRUNNAME
 7105       FORMAT ('*RUN ',A5,A10,' ',I2,',',I1,' C',I1,' ',A40,' ')
          ELSEIF (TN.GE.10 .AND. TN.LT.100) THEN
            WRITE (OUTHED,7106) RUNRUNI,EXCODE,TN,RN,CN,TRUNNAME
 7106       FORMAT ('*RUN ',A5,A10,' ',I3,',',I1,' C',I1,' ',A40)
          ENDIF
        ELSE
          IF (TN.LT.10) THEN
            WRITE (OUTHED,7107) RUNRUNI(1:5),EXCODE,TN,TRUNNAME
 7107       FORMAT ('*RUN ',A5,': ',A10,' ',I1,' ',A40,'  ')
          ELSEIF (TN.GE.10.AND.TN.LT.100) THEN
            WRITE (OUTHED,7108) RUNRUNI,EXCODE,TN,RN,TRUNNAME
 7108       FORMAT ('*RUN ',A5,': 'A10,' ',I2,',',I1,' ',A40,' ')
          ELSEIF (TN.GE.10 .AND. TN.LT.100) THEN
            WRITE (OUTHED,7109) RUNRUNI,EXCODE,TN,RN,TRUNNAME
 7109       FORMAT ('*RUN ',A5,': 'A10,' ',I3,',',I1,' ',A40)
          ENDIF
        ENDIF

!-----------------------------------------------------------------------
!       Check controls
!-----------------------------------------------------------------------

        ! Water and N uptake methods .. MEWNU 
        ! R=RLV+LL complex,W=RLV for h20,N=RLV for N,B=RLV for both
        IF (MEWNU.NE.'R') THEN 
          IF (MEWNU.NE.'W') THEN 
            IF (MEWNU.NE.'N') THEN 
              IF (MEWNU.NE.'B') THEN 
                MEWNU = 'R'
              ENDIF        
            ENDIF        
          ENDIF        
        ENDIF        
        IF (MEPHO.NE.'I') THEN
          IF (MEPHO.NE.'M') THEN
            IF (MEPHO.NE.'R') THEN
              WRITE(MESSAGE(1),'(A22,A1,A15,A19)')
     &          'Photosynthesis method ',MEPHO,' not an option ',
     &          ' Changed to R (RUE)'
              CALL WARNING(1,'CSCAS',MESSAGE)
!              WRITE(FNUMWRK,*)' '
!              WRITE(FNUMWRK,'(A23,A1,A15,A19)')
!     &          'Photosynthesis method ',MEPHO,' not an option ',
!     &          ' Changed to R (RUE)'
              MEPHO = 'R'
            ENDIF
          ENDIF
        ENDIF
        ! Other CSM codes are:
        !  C Canopy photosynthesis curve.
        !  L Leaf photosynthesis response curve
        IF (IHARI.NE.'M') THEN
          IF (hnumber.LE.0) THEN 
            WRITE(MESSAGE(1),'(A37,A13,A1)')
     &        'No harvest date set although planting',
     &        'flag set to: ',IHARI
            MESSAGE(2)='Flag reset to M.'
            CALL WARNING(2,'CSCAS',MESSAGE)
            IHARI = 'M'                      
          ENDIF
        ENDIF

!-------------------------------------------------------------------
!       Write run information to Overview and Work output files
!-------------------------------------------------------------------

        ! To avoid problems of writing to closed file in Sequence mode 
!        INQUIRE (FILE = 'WORK.OUT',OPENED = FOPEN)
!        IF (.NOT.FOPEN) THEN
!          OPEN (UNIT = FNUMWRK,FILE = 'WORK.OUT')
!          WRITE(fnumwrk,*) 'CSCAS  Cassava Crop Module '
!        ENDIF
                    
!        WRITE(fnumwrk,*)' '
!        WRITE(fnumwrk,'(A18,A10,I3)')' GENERAL INFO FOR ',
!     &       excode,tn
!        WRITE(fnumwrk,*)' FILE       ',FILEIO(1:60)
!        WRITE(fnumwrk,*)' EXPERIMENT ',EXCODE
!        WRITE(fnumwrk,*)' TREATMENT  ',TN
!        WRITE(fnumwrk,*)' REPLICATE  ',RN
!        WRITE(fnumwrk,*)' '
!        WRITE(fnumwrk,*)' MODEL      ',MODEL
!        WRITE(fnumwrk,*)' MODULE     ',MODNAME
!        WRITE(fnumwrk,'(A13,I6)')'  VERSION    ',VERSION
!        WRITE(fnumwrk,*)' PRODUCT    ',HPROD
!        WRITE(fnumwrk,*)' RNMODE     ',RNMODE
!        IF (RUN.LT.10) THEN
!          WRITE(fnumwrk,'(A13,I1)')' RUN        ',RUN   
!        ELSEIF (RUN.GE.10.AND.RUN.LT.1000) THEN
!          WRITE(fnumwrk,'(A13,I2)')' RUN        ',RUN   
!        ELSE
!          WRITE(fnumwrk,'(A13,I3)')' RUN        ',RUN   
!        ENDIF
!        WRITE(fnumwrk,*)' CULTIVAR   ',CUDIRFLE(1:60)
!        WRITE(fnumwrk,*)' ECOTYPE    ',ECDIRFLE(1:60)
!        WRITE(fnumwrk,*)' SPECIES    ',SPDIRFLE(1:60)
!        WRITE(fnumwrk,*)' METHODS '
!        IF (MEEXP.EQ.'E')
!     &   WRITE(fnumwrk,'(A26,A1)')'   EXPERIMENTAL ALGORITHM ',MEEXP
!         WRITE(fnumwrk,'(A26,A1)') '   PHOTOSYNTHESIS         ',MEPHO
!         WRITE(fnumwrk,'(A26,A1,1X,A1)') '   WATER AND N SWITCHES   '
!     &     ,ISWWAT,ISWNIT
!         WRITE(fnumwrk,'(A26,A1)') '   LEAF LIFE SWITCH       '
!     &     ,CFLLFLIFE     
!         WRITE(fnumwrk,'(A26,A3)')  '   N UPTAKE               ',MERNU
!         WRITE(fnumwrk,'(A26,I1)') ' '
!         WRITE(fnumwrk,'(A26,I1)') '  CROP COMPONENT          ',CN
!         WRITE(fnumwrk,'(A26,A6,2X,A16)')
!     &     '  CULTIVAR                ',VARNO,VRNAME
!       IF (IPLTI.NE.'A') THEN
!        IF (IPLTI.NE.'A' .AND. IPLTI.NE.'F') THEN
!          WRITE(fnumwrk,'(A23,I7)')
!     &     '  PLANTING DATE TARGET:',PLYEARDOYT
!        ELSE
!          WRITE(fnumwrk,'(A23)')
!     &     '  AUTOMATIC PLANTING   '              
!          WRITE (fnumwrk,*) '  PFIRST,PLAST :',pwdinf,pwdinl
!          WRITE (fnumwrk,*) '  HFIRST,HLAST :',hfirst,hlast
!        ENDIF
!        WRITE (fnumwrk,'(A15,2F7.1)')'  PLTPOP,ROWSPC',PLTPOPP,ROWSPC
!        WRITE (fnumwrk,'(A15,2F7.1)')'  SDEPTH,SDRATE',SDEPTH,SDRATE
!        IF (sdepthu.LT.sdepth)
!     &   WRITE (fnumwrk,'(A15,F7.1)')'  SHOOT DEPTH  ',SDEPTHU      
!        WRITE (fnumwrk,'(A15,2F7.1,A6)')'  SEEDRS,SEEDN ',
!     &                   SEEDRSI*PLTPOPP*10.0,SEEDNI*PLTPOPP*10.0,
!     &                   ' kg/ha'
!        WRITE (fnumwrk,'(A15, F7.1)') '  PLMAGE       ',PLMAGE
        ! LAH NEED TO CHECK HARVEST OPTIONS FOR dap,growth stage.
        ! DISCUSS WITH CHP
!        IF (IHARI.NE.'M') THEN
!          IF (IHARI.NE.'A') THEN
!            WRITE(fnumwrk,'(A22,I7)')
!     &      '  HARVEST DATE TARGET:',YEARDOYHARF 
!          ELSE
!            WRITE(fnumwrk,'(A22,A9)')
!     &      '  HARVEST DATE TARGET:','AUTOMATIC'  
!          ENDIF 
!        ELSE
!          WRITE(fnumwrk,'(A22,A8)')
!     &     '  HARVEST DATE TARGET:','MATURITY'  
!        ENDIF
!        WRITE (fnumwrk,'(A15,2F7.1)') '  HPCF,HBPCF   ',HPCF,HBPCF

!        IF (IDETG.NE.'N') THEN
!          WRITE(fnumwrk,*)' '
!          WRITE(fnumwrk,*)' MAJOR COEFFICIENTS AFTER CHECKING'
!          WRITE(fnumwrk,*)' Development '
!          WRITE(fnumwrk,*)'  Ppsen  ',Ppsen   
!          WRITE(fnumwrk,*)'  Ppfpe  ',Dfpe 
!          IF (Ppsen.EQ.'LQ') WRITE(fnumwrk,*)'  Ppexp  ',Ppexp   
!          WRITE(fnumwrk,*)'  Ppthr  ',Ppthr   
!          WRITE(fnumwrk,*)'  Pps1   ',Dayls(1)
!          WRITE(fnumwrk,*)'  Pps2   ',Dayls(2)
!          WRITE(fnumwrk,*)'  Mstg   ',Mstg    
!          DO L = 1,MSTG
!            WRITE(fnumwrk,'(A15,I2,2F8.1)')'  B#->,Deg.d-> ',l,
!     &       Pdl(L),PD(l)
!          ENDDO  
!          WRITE(fnumwrk,*)' Crown stages '
!          WRITE(fnumwrk,*)'  Crfr   ',Crfr 
!          WRITE(fnumwrk,*)' CH2O reserves '
!          WRITE(fnumwrk,*)'  Rsfrs  ',rsfrs
!          WRITE(fnumwrk,*)'  Rspco  ',rspco
!          WRITE(fnumwrk,*)' Radiation use  '
!          WRITE(fnumwrk,*)'  Parue  ',Parue
!          WRITE(fnumwrk,*)'  Paru2  ',Paru2
!          WRITE(fnumwrk,*)' Shoots  '
!          WRITE(fnumwrk,*)'  Sgro   ',shgr(1),shgr(2)
!          WRITE(fnumwrk,*)'  Brfx(1)',brfx(1)
!          WRITE(fnumwrk,*)'  Brfx(2)',brfx(2)
!          WRITE(fnumwrk,*)'  Brfx(3)',brfx(3)
!          WRITE(fnumwrk,*)'  Brfx(4)',brfx(4)
!          WRITE(fnumwrk,*)'  Brfx(5)',brfx(5)
!          WRITE(fnumwrk,*)'  Brfx(6)',brfx(6)
!          WRITE(fnumwrk,*)'  Canhts ',canhts
!          WRITE(fnumwrk,*)' Leaves     '
!          WRITE(fnumwrk,*)'  La1s   ',La1s
!          WRITE(fnumwrk,*)'  Laxs   ',Laxs
!          WRITE(fnumwrk,*)'  Lafs   ',Lafs
!          WRITE(fnumwrk,*)'  Laxnd,2',Laxno,Laxn2
!          WRITE(fnumwrk,*)'  Lafnd  ',Lafnd
!          WRITE(fnumwrk,*)'  Slas,Slasmn  ',Laws,lawmnfr
!          WRITE(fnumwrk,*)'  Slacf,Slaff  ',Lawcf,Lawff
!          WRITE(fnumwrk,*)'  Slatr,Slats  ',Lawtr,Lawts
!          WRITE(fnumwrk,*)'  Slawr        ',Lawwr
!          WRITE(fnumwrk,*)'  Lpefr        ',Lpefr
!          WRITE(fnumwrk,*)'  Phints,Phntf ',Phints,Phintfac            
!          WRITE(fnumwrk,*)'  Llifg,a,s,x  ',Llifg,Llifa,Llifs,Llifx  
!          WRITE(fnumwrk,*)'  Lwlos  ',Lwlos                
!          WRITE(fnumwrk,*)'  Laixx,Parix  ',Laixx,Parix       
!          WRITE(fnumwrk,*)' Stems      '
!          WRITE(fnumwrk,*)'  Saws,Serx          ',Saws,Serx            
!          WRITE(fnumwrk,*)' Roots  '
!          WRITE(fnumwrk,*)'  Rdgs   ',Rdgs
!          WRITE(fnumwrk,*)'  Rresp  ',Rresp
!          WRITE(fnumwrk,*)' Storage roots '
!          WRITE(fnumwrk,*)'  Srfr   ',Srfr
!          WRITE(fnumwrk,*)'  Dusri  ',Dusri
!          WRITE(fnumwrk,*)'  Hmpc   ',Hmpc
!          WRITE(fnumwrk,*)' Nitrogen uptake concentration factors'
!          WRITE(fnumwrk,*)'  Rtno3,Rtnh4 ',rtno3,rtnh4 
!          WRITE(fnumwrk,*)'  No3cf,H2ocf ',no3cf,h2ocf
!          WRITE(fnumwrk,*)'  No3mn,Nh4mn ',no3mn,nh4mn
!          WRITE(fnumwrk,*)' Nitrogen concentrations'
!          WRITE(fnumwrk,*)'  Ln%s   ',LNPCS           
!          WRITE(fnumwrk,*)'  Ln%mn  ',LNPCMN            
!          WRITE(fnumwrk,*)'  Sn%s   ',SNPCS           
!          WRITE(fnumwrk,*)'  Sn%mn  ',SNPCMN           
!          WRITE(fnumwrk,*)'  Rn%s   ',RNPCS           
!          WRITE(fnumwrk,*)'  Rn%mn  ',RNPCMN          
!          WRITE(fnumwrk,*)'  Srnpcs ',SRNPCS          
!          WRITE(fnumwrk,*)' Nitrogen stress limits '
!          WRITE(fnumwrk,*)'  Nfpu,L ',NFPU,NFPL       
!          WRITE(fnumwrk,*)'  Nfgu,L ',NFGU,NFGL       
!          WRITE(fnumwrk,*)'  Nfsu   ',NFSU           
!          WRITE(fnumwrk,*)'  Nllg   ',NLLG           
         
!          WRITE(FNUMWRK,*)' '
!          WRITE(FNUMWRK,'(A17)')' BRANCH NODE NOS.'
!          WRITE(FNUMWRK,'(A23)')'   BRANCH NODE #    ABV'
!          DO L = 1,MSTG
!            WRITE(FNUMWRK,'(I6,I10,2X,A5)')L,NINT(LNUMTOSTG(L)),PSABV(L)
!          ENDDO
         
!          WRITE(FNUMWRK,*)' '
!          WRITE(FNUMWRK,'(A22)')' TEMPERATURE RESPONSES'
!          IF (TRGEM(1).GT.-99.0) THEN
!            WRITE(FNUMWRK,'(A9,4F5.1)')'   TRGEM ',
!     &       TRGEM(1),TRGEM(2),TRGEM(3),TRGEM(4)
!          ENDIF
!          WRITE(FNUMWRK,'(A9,4F5.1)')'   TRDV1 ',     
!     &     TRDV1(1),TRDV1(2),TRDV1(3),TRDV1(4)
!          WRITE(FNUMWRK,'(A9,4F5.1)')'   TRLFG ',     
!     &     TRLFG(1),TRLFG(2),TRLFG(3),TRLFG(4)
!          WRITE(FNUMWRK,'(A9,4F5.1)')'   TRPHS ',     
!     &     TRPHS(1),TRPHS(2),TRPHS(3),TRPHS(4)

!          IF (ISWDIS(LENDIS:LENDIS).NE.'N') THEN
!            WRITE (fnumwrk,*) ' '
!            WRITE (fnumwrk,*) 'DISEASE INITIATION AND GROWTH ASPECTS'
!            WRITE (fnumwrk,'(A13,A49)')'             ',
!     &       '  DATE   GROWTH FACTOR  FAVOURABILITY REQUIREMENT'
!            WRITE (fnumwrk,'(A12,I10,2F10.1)')
!     &       '  DISEASE 1 ',DIDAT(1),DIGFAC(1),DIFFACR(1)
!            WRITE (fnumwrk,'(A12,I10,2F10.1)')
!     &       '  DISEASE 2 ',DIDAT(2),DIGFAC(2),DIFFACR(2)
!            WRITE (fnumwrk,'(A12,I10,2F10.1)')
!     &       '  DISEASE 3 ',DIDAT(3),DIGFAC(3),DIFFACR(3)
!            WRITE (fnumwrk,*) ' '
!            IF (DCTAR(1).GT.0) WRITE (fnumwrk,*)
!     &       'DISEASE CONTROL DATES,GROWTH FACTORS,AND DURATION'
!            DO L=1,DCNX
!              IF (DCTAR(L).EQ.1) WRITE (fnumwrk,'(A12,I10,2F10.1)')
!     &         '  DISEASE 1 ',DCDAT(L),DCFAC(L),DCDUR(L)
!              IF (DCTAR(L).EQ.2) WRITE (fnumwrk,'(A12,I10,2F10.1)')
!     &         '  DISEASE 2 ',DCDAT(L),DCFAC(L),DCDUR(L)
!              IF (DCTAR(L).EQ.3) WRITE (fnumwrk,'(A12,I10,2F10.1)')
!     &         '  DISEASE 3 ',DCDAT(L),DCFAC(L),DCDUR(L)
!            ENDDO
!          ENDIF

!        ENDIF

!-----------------------------------------------------------------------
!       Set equivalences to avoid compile errors
!-----------------------------------------------------------------------
  
        tvr1 = tairhr(1)
        ! When running in CSM
        IF (FILEIOT.EQ.'DS4') THEN
          ALBEDO = ALBEDOS  ! Previously 0.2
          CLOUDS = 0.0
        ELSE
          ALBEDO = ALBEDOS  
        ENDIF

!-----------------------------------------------------------------------
!       Record starting values and files
!-----------------------------------------------------------------------

        CNI = CN
        KCANI = KCAN
        ONI = ON
        RNI = RN
        RWUMXI = RWUMX
        SNI = SN
        TNI = TN
        KEPI = KEP

!-----------------------------------------------------------------------
!       Create and write warning messages re. input and output variables
!-----------------------------------------------------------------------

!        IF (SLPF.LT.1.0) THEN
!          WRITE (fnumwrk,*) ' '
!          WRITE (fnumwrk,*)
!     &     ' WARNING  Soil fertility factor was less than 1.0: ',slpf
!        ENDIF  

!        WRITE(FNUMWRK,*)' '
!        WRITE(FNUMWRK,'(A22)')' OUTPUTS              '

        ! Control switch for OUTPUT file names
        CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'FNAME',fname)
!        IF (FNAME.EQ.'Y') THEN
!          WRITE(FNUMWRK,*)' File names switched from standard. '
!        ELSE  
!          WRITE(FNUMWRK,*)' Using standard file names. '
!        ENDIF   
        
        ! Output choices
        ! 1=Reserves in laminae,stem,crown,SLA;stem includes petioles
        ! 2=No reserves;stem includes petioles
        ! 3=No reserves;stem does not include petioles
        OUTCHOICE = 3
        IF (OUTCHOICE.EQ.2) THEN
          WRITE(MESSAGE(1),'(A36,A37)')
     &      '  NO reserve CH2O included in the wt',
     &      ' of leaves,stems,and crown.          ' 
          CALL WARNING(1,'CSCAS',MESSAGE)
!          WRITE(FNUMWRK,'(A36,A38)')
!     &      '  NO reserve CH2O included in the wt',
!     &      ' of leaves,stems,and crown.           '
!          WRITE(FNUMWRK,'(A36)')
!     &      '  Stem weight includes the petioles.'
        ELSEIF (OUTCHOICE.EQ.3) THEN
          WRITE(MESSAGE(1),'(A36,A37)')
     &      '  NO reserve CH2O included in the wt',
     &      ' of leaves,stems,and crown.          ' 
          CALL WARNING(1,'CSCAS',MESSAGE)
!        WRITE(FNUMWRK,'(A36,A38)')
!     &      '  NO reserve CH2O included in the wt',
!     &      ' of leaves,stems,and crown.           '
!          WRITE(FNUMWRK,'(A45)')
!     &      '  Stem weight does NOT includes the petioles.'
        ENDIF  

!        WRITE(FNUMWRK,*)' '
!        WRITE(FNUMWRK,'(A22)')' DURING RUN STATUS:   '

!***********************************************************************
      ELSEIF (DYNAMIC.EQ.RATE) THEN
!***********************************************************************

!-----------------------------------------------------------------------
!       Set 'establishment' switches
!-----------------------------------------------------------------------

        ! EARLY? are parameters that alllow for water and N stresses 
        ! to be switched off early in the life cycle. If they are set
        ! to -1.0, nothing is switched off. (NB. Not 0 because this 
        ! results in the stresses being switched off on the emergence
        ! day.)  
        ! NB. When N stress is switched off, the accumulation of N in 
        ! the plant still proceeds as it would otherwise have done,  
        ! and N stress may be more severe than it otherwise would 
        ! have been once N stress is switched back on.
        EARLYN = -1.0
        EARLYW = -1.0
        IF (LNUM.LE.EARLYN) THEN
          ISWNITEARLY = 'N'
        ELSE 
          ISWNITEARLY = 'Y'
        ENDIF  
        IF (LNUM.LE.EARLYW) THEN
          ISWWATEARLY = 'N'
        ELSE 
          ISWWATEARLY = 'Y'
        ENDIF  

!-----------------------------------------------------------------------
!       Set date and environmental equivalents
!-----------------------------------------------------------------------

        YEARDOY = YEAR*1000 + DOY
        TMEAN = (TMAX+TMIN)/2.0
        TMEANSURF = TMEAN
        CO2AIR = 1.0E12*CO2*1.0E-6*44.0 /       ! CO2 in g/m3
     &   (8.314*1.0E7*((TMAX+TMIN)*0.5+273.0))

!-----------------------------------------------------------------------
!       Determine if today is planting day
!-----------------------------------------------------------------------

        ! YEARPLTCSM established by CSM and brought across in argument.
        IF (FILEIOT.EQ.'DS4') THEN
!         IF (IPLTI.EQ.'A' .OR. (INDEX('FQN',RNMODE) > 0)) THEN
          IF (IPLTI.EQ.'A' .OR. IPLTI.EQ.'F' .OR. 
     &       (INDEX('FQNY',RNMODE) > 0)) THEN
            PLYEARDOYT = YEARPLTCSM
          ENDIF  
        ENDIF

        IF (PLYEARDOY.GT.9000000) THEN            ! If before planting
          IF(PLYEARDOYT.GT.0 .AND. PLYEARDOYT.LT.9000000)THEN
            ! Specified planting date
            IF(YEARDOY.EQ.PLYEARDOYT) THEN
              PLYEARDOY = YEARDOY
              PLYEAR = YEAR
            ENDIF
          ELSE
            IF (FILEIOT.NE.'DS4') THEN
!               Automatic planting
!               Check window for automatic planting,PWDINF<PLYEART<PWDINL
              IF (YEARDOY.GE.PWDINF.AND.YEARDOY.LE.PWDINL) THEN
                ! Within planting window.
                ! Determine if soil temperature and soil moisture ok
                ! Obtain soil temperature, TSDEP, at 10 cm depth
                I = 1
                TSDEP = 0.0
                XDEP = 0.0
                DO WHILE (XDEP .LT. 10.0)
                  XDEP = XDEP + DLAYR(I)
                  TSDEP = ST(I)
                  I = I + 1
                END DO
                ! Compute average soil moisture as percent, AVGSW
                I = 1
                AVGSW = 0.0
                CUMSW = 0.0
                XDEP = 0.0
                DO WHILE (XDEP .LT. SWPLTD)
                  XDEPL = XDEP
                  XDEP = XDEP + DLAYR(I)
                  IF (DLAYR(I) .LE. 0.) THEN
                    !If soil depth is less than SWPLTD
                    XDEP = SWPLTD
                    CYCLE
                  ENDIF
                  DTRY = MIN(DLAYR(I),SWPLTD - XDEPL)
                  CUMSW = CUMSW + DTRY *
     &             (MAX(SW(I) - LL(I),0.0)) / (DUL(I) - LL(I))
                  I = I + 1
                END DO
                AVGSW = (CUMSW / SWPLTD) * 100.0
                IF (TSDEP .GE. PTTN .AND. TSDEP .LE. PTX) THEN
                  IF (AVGSW .GE. SWPLTL .AND. AVGSW .LE. SWPLTH) THEN
                    PLYEARDOY = YEARDOY
                    PLYEAR = YEAR
                  ENDIF
                ENDIF
              ENDIF
            ELSE
              IF (YEARDOY.GT.PWDINL) THEN
                CFLFAIL = 'Y'
                STGYEARDOY(12) = YEARDOY  ! Failure
                STGYEARDOY(11) = YEARDOY  ! End Crop
                Message(1) = 'Automatic planting failure '
                CALL WARNING(1,'CSCAS',MESSAGE)
              ENDIF
            ENDIF
          ENDIF
        ENDIF

        IF (YEARDOY.EQ.PLYEARDOY) THEN
          DAP = 0
          ! Initial soil N and H2O
          ISOILN = 0.0
          ISOILH2O = 0.0
          DO I = 1, NLAYR
            ISOILN = ISOILN + NO3LEFT(I)/(10.0/(BD(I)*(DLAYR(I))))
     &                      + NH4LEFT(I)/(10.0/(BD(I)*(DLAYR(I))))
            ISOILH2O = ISOILH2O + SW(I)*DLAYR(I)
          ENDDO
          ! Plant population as established; if no data,as planted
          IF (PLTPOPE.GT.0) THEN
            PLTPOP = PLTPOPE
          ELSE
            PLTPOP = PLTPOPP
          ENDIF  
          ! Shoot # set equal to plants per hill
          IF (PLPH.GT.0.0) THEN
            SHNUM = PLPH
          ELSE
            SHNUM = 1.0
          ENDIF
          SHNUML(1) = SHNUM
        ENDIF

!=======================================================================
        IF (YEARDOY.GT.PLYEARDOY) THEN ! If planted (assumed in evening)
!=======================================================================

          IF (PLYEAR.LE.0) PLYEAR = YEAR

!-----------------------------------------------------------------------
!         Calculate potential plant evaporation,water uptake if neeeded
!-----------------------------------------------------------------------

          ! EO is brought into the module. The following calculations
          ! (apart from the root water uptake module) are for 
          ! comparative purposes only. The root water uptake module 
          ! is not necessary when running in CSM, but is necessary for
          ! CROPSIM.

          ! Co2 effect on stomatal resistances. General for C3 crops 
          RLF  = 9.72 + 0.0757 * 330.0 + 10.0
          RLFC = 9.72 + 0.0757 *  CO2  + 10.0
                                                         
          IF (FILEIOT.EQ.'DS4'.AND.IDETG.NE.'N'.OR.
     &        FILEIOT.NE.'DS4') THEN                  

            IF (ISWWAT.NE.'N') THEN    
            
            ! Call 1  Basic calculations with rcrop = 0  
            CALL EVAPO('M',SRAD,CLOUDS,TMAX,TMIN,TDEW,WINDSP,
     &       ALBEDO,RATM,RCROP*0.0,
     &       EO,EOPEN,EOMPEN,EOPT,EOEBUD,TCAN,'M')
            EOPENC = EOPENC + EOPEN 
            EOPTC = EOPTC + EOPT
            EOMPENC = EOMPENC + EOMPEN
            EOEBUDC = EOEBUDC + EOEBUD
             
            ! CSM with LAI=1.0,CROP=WH,TAVG=20.0,WINDSP=86.4 has
            ! RATM = 55  RCROP = 45
            ! Monteith had RATM = 300, RCROP = 150->500
            
            ! Call 2  Using rcrop as read-in from spe file
            CALL EVAPO('M',SRAD,CLOUDS,TMAX,TMIN,TDEW,WINDSP,
     &       ALBEDO,RATM,RCROP,
     &       EO,EOPEN,EOMPCRP,EOPT,EOEBUDCRP,TCAN,'M')
            EOMPCRPC = EOMPCRPC + EOMPCRP
            EOEBUDCRPC = EOEBUDCRPC + EOEBUDCRP

            ! Call 3 Using rcrop adjusted for CO2 effect     
            IF (RLF.GT.0.0)
     &       CALL EVAPO('M',SRAD,CLOUDS,TMAX,TMIN,TDEW,WINDSP,
     &       ALBEDO,RATM,RCROP*RLFC/RLF,
     &       EO,EOPEN,EOMPCRPCO2,EOPT,EOEBUDCRPCO2,TCAN,'M')
            EOMPCRPCO2C = EOMPCRPCO2C + EOMPCRPCO2
            EOEBUDCRPCO2C = EOEBUDCRPCO2C + EOEBUDCRPCO2

            ! Transpiration ratio (Pot.pl.evap/Pot.soil evap)
            EPSRATIO = 1.0
            IF (EOMPEN.GT.0.0) EPSRATIO = EOMPCRPCO2 / EOMPEN
            TRATIO = 1.0
            IF (EOMPCRP.GT.0.0) TRATIO = EOMPCRPCO2 / EOMPCRP
            ENDIF
            
            IF (fileiot(1:2).NE.'DS') THEN
              ! Calculate plant potential evaporation 
              EOP = MAX(0.0,EO/EOMPEN*EOMPCRPCO2 * (1.0-EXP(-LAI*KEP)))
              ! Ratio necessary because EO method may not be Monteith
              CALL CSCRPROOTWU(ISWWAT, 
     &         NLAYR, DLAYR, LL, SAT, WFEU, MEWNU,
     &         EOP, RLV, RWUPM, RLFWU, RWUMX, RTDEP,
     &         SW, WTDEP, uh2o, trwup, trwu)
            ENDIF
            
            ! Call 4 Using rcrop adjusted for CO2 & H2O effect     
            ! NB. Using previous days WFP. 
            ! If use this for other than comparison, must check  LAH
            IF (RLF.GT.0.0.AND.WFP.LE.1.0.AND.WFP.GT.0.0) THEN
             CALL EVAPO('M',SRAD,CLOUDS,TMAX,TMIN,TDEW,WINDSP,
     &        ALBEDO,RATM,RCROP*RLFC/RLF*(1.0+(1.0-WFP)),
     &        EO,EOPEN,EOMPCRPCO2H2O,EOPT,EOEBUDCRPCO2H2O,TCAN,'M')
            ELSE
!              WRITE(FNUMWRK,*)' WFP OUT OF RANGE! = ',WFP
            ENDIF
            EOMPCRPCO2H2OC = EOMPCRPCO2H2OC + EOMPCRPCO2H2O
            EOEBUDCRPCO2H2OC = EOEBUDCRPCO2H2OC + EOEBUDCRPCO2H2O
            
            ! Call 5 to calcuate canopy temperature
            TVR1 = (TRWU*10.0+ES)
            CALL EVAPO('M',SRAD,CLOUDS,TMAX,TMIN,TDEW,WINDSP,
     &      ALBEDO,RATM,RCROP*RLFC/RLF*(1.0+(1.0-WFP)),
     &      EO,tvr1,tvr2,tvr3,tvr4,TCAN,'C')
          ENDIF
          
          ! Cumulative potential ET as used
          IF (EO.GT.0.0) EOC = EOC + EO
          
          ! Cumulative canopy-air temperature difference
          TDIFSUM = TDIFSUM+(TCAN-TMEAN)
          TDIFNUM = TDIFNUM + 1
          TDIFAV = TDIFSUM/TDIFNUM

!-----------------------------------------------------------------------
!         Calculate thermal time
!-----------------------------------------------------------------------

          Tfd = TFAC4(trdv1,tmean,TT)
          IF (brstage+1.0.LT.10.0)
     &     Tfdnext = TFAC4(trdv2,tmean,TTNEXT)
          IF (trgem(3).GT.0.0) THEN
            Tfgem = TFAC4(trgem,tmean,TTGEM)
          ELSE
            Ttgem = tt
          ENDIF    
          IF (Cfllflife.EQ.'D') THEN
            ! Leaf life read-in as days (eg.7 phyllochrons->7 days)
            Ttlflife = Phints   
          ELSE  
            Tflflife = TFAC4(trdv1,tmean,TTlflife)
          ENDIF  
  
!-----------------------------------------------------------------------
!         Calculate soil water 'status' (Used as a sw 'potential')
!-----------------------------------------------------------------------

          DO L = 1,NLAYR
            SWP(L) =
     &       AMIN1(1.0,AMAX1(.0,((SW(L)-LL(L))/(DUL(L)-LL(L)))))
          ENDDO
          
!-----------------------------------------------------------------------
!         Calculate water factor for germination
!-----------------------------------------------------------------------

          WFGE = 1.0
          IF (ISWWAT.NE.'N') THEN
            IF (GESTAGE.LT.1.0) THEN
              IF (LSEED.LT.0) LSEED = CSIDLAYR (NLAYR, DLAYR, SDEPTH)
              IF (LSEED.GT.1) THEN
                SWPSD = SWP(LSEED)
              ELSE
               SWP(0) = AMIN1(1.,AMAX1(.0,(SWP(1)-0.5*(SWP(2)-SWP(1)))))
               SWPSD = SWP(0) + (SDEPTH/DLAYR(1))*(SWP(2)-SWP(0))
              ENDIF
              IF (WFGEM.GT.0.0)
     &         WFGE = AMAX1(0.0,AMIN1(1.0,(SWPSD/WFGEM)))
            ENDIF
          ENDIF
          IF (ISWWATCROP.EQ.'N') WFGE = 1.0

!=======================================================================
          IF (GEUCUM+TTGEM*WFGE.GE.PEGD) THEN  ! If germinated by endday
!=======================================================================

!-----------------------------------------------------------------------
!           Determine when in day germination and emergence occurred
!-----------------------------------------------------------------------

            ! Germination
            IF (GEUCUM.LT.PEGD.AND.GEUCUM+TTGEM*WFGE.LT.PEGD) THEN
              GERMFR = 0.0
            ELSEIF (GEUCUM.LE.PEGD.AND.GEUCUM+TTGEM*WFGE.GE.PEGD) THEN
              GERMFR = 1.0 - (PEGD-GEUCUM)/(TTGEM*WFGE)
            ELSEIF (GEUCUM.GT.PEGD) THEN
              GERMFR = 1.0
            ENDIF

            ! Emergence
            IF (GEUCUM.LT.PEGD+PECM*SDEPTHU.AND.
     &       GEUCUM+TTGEM*WFGE.LE.PEGD+PECM*SDEPTHU) THEN
              EMRGFR = 0.0
            ELSEIF (GEUCUM.LE.PEGD+PECM*SDEPTHU.AND.
     &       GEUCUM+TTGEM*WFGE.GT.PEGD+PECM*SDEPTHU) THEN
              EMRGFR = 1.0 - (PEGD+PECM*SDEPTHU-GEUCUM)/(TTGEM*WFGE)
              IF (EMFLAG.NE.'Y') THEN
!                WRITE(FNUMWRK,*)' ' 
!                WRITE(FNUMWRK,'(A18,I8)')' Emergence on day ',yeardoy 
                EMFLAG = 'Y'
              ENDIF
              LNUMSG = 1     ! LAH NEW
            ELSEIF (GEUCUM.GT.PEGD+PECM*SDEPTHU) THEN
              EMRGFR = 1.0
            ENDIF
     
!-----------------------------------------------------------------------
!           Calculate daylength factors for development
!-----------------------------------------------------------------------

            DF = 1.0
            DFNEXT = 1.0
            ! To ensure correct sensitivity on emergence day
            IF (BRSTAGE.LE.0.0) THEN
              BRSTAGETMP = 1.0
            ELSE
              BRSTAGETMP = BRSTAGE
            ENDIF
            IF (PPSEN.EQ.'SL') THEN      ! Short day response,linear 
              DF = 1.0 - DAYLS(INT(BRSTAGETMP))/1000.*(PPTHR-DAYL)
              IF (BRSTAGETMP.LT.FLOAT(MSTG)) THEN
               DFNEXT = 1.-DAYLS(INT(BRSTAGETMP+1))/1000.*(PPTHR-DAYL)
              ELSE
               DFNEXT = DF
              ENDIF 
            ELSEIF (PPSEN.EQ.'LQ') THEN  ! Long day response,quadratic
              DF = AMAX1(0.0,AMIN1(1.0,1.0-
     &        (DAYLS(INT(BRSTAGETMP))/10000.*(PPTHR-DAYL)**PPEXP)))
              IF (BRSTAGETMP.LT.10.0) DFNEXT = AMAX1(0.0,AMIN1(1.0,1.0-
     &        (DAYLS(INT(BRSTAGETMP+1.0))/10000.*(PPTHR-DAYL)**PPEXP)))
              Tfdf = AMAX1(0.0,1.0-AMAX1(0.0,(TMEAN-10.0)/10.0))
              Tfdf = 1.0  ! LAH No temperature effect on DF ! 
              DF = DF + (1.0-DF)*(1.0-TFDF)
              DFNEXT = DFNEXT + (1.0-DFNEXT)*(1.0-TFDF)
            ENDIF
            
            ! Set daylength factor for output (Is dfpe before emergence)
            IF (EMRGFR.GE.1.0) THEN
              DFOUT = DF
            ELSE
              DFOUT = DFPE
            ENDIF 

!-----------------------------------------------------------------------
!           Calculate development units
!-----------------------------------------------------------------------

            DU = 0.0
            DUPHASE = 0.0
            DUPNEXT = 0.0
            ! To avoid exceeding the array sizes
            IF (BRSTAGETMP.LT.10.0) THEN
              DUNEED = PSTART(INT(BRSTAGETMP+1.0))-CUMDU
              IF (DUNEED.GE.TT*(DFPE*(GERMFR-EMRGFR)+DF*EMRGFR))THEN
                DUPHASE = TT*(DFPE*(GERMFR-EMRGFR)+DF*EMRGFR)
                TIMENEED = 1.0
                DUPNEXT = 0.0
              ELSE  
                DUPHASE = DUNEED
                TIMENEED = DUNEED/
     &           (TT*(DFPE*(GERMFR-EMRGFR)+DF*EMRGFR))
                DUPNEXT = TTNEXT*(1.0-TIMENEED)*DFNEXT
              ENDIF
            ELSE
            ENDIF
            
            DU = DUPHASE+DUPNEXT
            
!-----------------------------------------------------------------------
!           Set seed reserve use for root growth and update av.reserves
!-----------------------------------------------------------------------

            IF (GERMFR.GT.0.0.OR.GESTAGE.GE.0.5) THEN
              SEEDRSAVR =
     &         AMIN1(SEEDRS,SEEDRSI/SDDUR*(TT/STDAY)*GERMFR)
            ELSE
              SEEDRSAVR = 0.0
            ENDIF
            ! Seed reserves available
            SEEDRSAV = SEEDRSAV-SEEDRSAVR

!-----------------------------------------------------------------------
!           Determine if today has a harvest instruction
!-----------------------------------------------------------------------

            DO I = 1, 20
              IF (HYEARDOY(I).EQ.YEARDOY) THEN
                HANUM = I
!                WRITE(fnumwrk,*) ' '
!                WRITE(fnumwrk,'(A20,i2,A12,A1,A6,i8)')
!     &           ' Harvest instruction',hanum,
!     &           '  Operation ',hop(i),
!     &           '  Day ',yeardoy
                CALL CSUCASE(HOP(I)) 
                IF (hop(i).EQ.'F') YEARDOYHARF = YEARDOY 
              ENDIF
            END DO

!-----------------------------------------------------------------------
!           Determine amounts removed by grazing,etc.   
!-----------------------------------------------------------------------

	    IF (HANUM.GT.0) THEN
	      IF (HOP(HANUM).EQ.'G'.AND.
     &	       CWAD.GT.0.0.AND.CWAD.GT.CWAN(HANUM)) THEN
                HAWAD = AMIN1((CWAD-CWAN(HANUM)),HAMT(HANUM))
	        HAWAD = AMAX1(0.0,HAWAD)
                HAFR = AMAX1(0.0,HAWAD/CWAD)
	      ELSE   
	        HAWAD = 0.0
	        HAFR = 0.0
	      ENDIF
            ENDIF
              
!            IF (HAFR.GT.0.0)
!     &       WRITE(fnumwrk,'(A23,3F6.1)')' HARVEST  FR,CWAN,CWAD ',
!     &        HAFR,CWAN(HANUM),CWAD

            ! For grazing 
            lwph = lfwt * hafr
            laph = lapd * hafr
            swph = stwt * hafr
            rswph = rswt * hafr
            lnph = leafn * hafr
            snph = stemn * hafr
            rsnph = rsn * hafr

!-----------------------------------------------------------------------
!           Check for or calculate PAR interception at start of day
!-----------------------------------------------------------------------

            PARI = 0.0
            PARI1 = (1.0 - EXP((-KCAN)*LAI))
            IF (PARIP.GT.0.0) THEN
              ! From competition model
              IF (ISWDIS(LENDIS:LENDIS).NE.'N') THEN
                PARI = PARIPA/100.0
              ELSE
                PARI = PARIP/100.0
              ENDIF
            ELSE
              PARI = PARI1
              ! LAH For row crops may need to change 
              ! In original Ceres maize, kcan is calculated as:
              ! 1.5 - 0.768*((rowspc*0.01)**2*pltpop)**0.1
              ! eg. 1.5 - 0.768*((75*0.01)**2*6.0)**0.1  =  0.63
            ENDIF

!-----------------------------------------------------------------------
!           Calculate adjustment to yesterday's C assimilation
!-----------------------------------------------------------------------

            ! End of day interception = today's starting interception
            IF (MEPHO.EQ.'M') CARBOEND = CARBOTMPM * PARI/PLTPOP
            IF (MEPHO.EQ.'I') CARBOEND = CARBOTMPI * PARI/PLTPOP
            IF (MEPHO.EQ.'R') CARBOEND = CARBOTMPR * PARI/PLTPOP

            CARBOADJ = (CARBOEND-CARBOBEG)/2.0*EMRGFRPREV
            ! But note, no adjustment if leaf kill
            PARMJIADJ = PARMJFAC*SRADPREV*(PARI-PARIPREV)/2.0*EMRGFR

!-----------------------------------------------------------------------
!           Calculate process rate factors
!-----------------------------------------------------------------------

            ! Water
            ! No water stress after emergence on day that emerges
            WFG = 1.0
            WFP = 1.0
            IF (ISWWAT.NE.'N') THEN
              IF (EOP.GT.0.0) THEN
                WUPR = TRWUP/(EOP*0.1)
                IF (WFGU-WFGL.GT.0.0)
     &           WFG = AMAX1(0.0,AMIN1(1.0,(WUPR-WFGL)/(WFGU-WFGL)))
                IF (WFPU-WFPL.GT.0.0)
     &           WFP = AMAX1(0.0,AMIN1(1.0,(WUPR-WFPL)/(WFPU-WFPL)))
              ENDIF
              IF (ISWWATEARLY.EQ.'N') THEN
                WFG = 1.0
                WFP = 1.0
              ENDIF
            ENDIF

            ! Nitrogen
            ! WARNING No N stress after emergence on day that emerges
            IF (ISWNIT.NE.'N') THEN
              IF (LFWT.GT.1.0E-5) THEN
                !NFG =AMIN1(1.0,AMAX1(0.0,(LANC-LNCGL)/(LNCGU-LNCGL)))
                LNCGL = LNCM + NFGL * (LNCX-LNCM)
                LNCGU = LNCM + NFGU * (LNCX-LNCM)
                IF (LNCGU - LNCGL > 1.E-6) THEN
                 NFG =AMIN1(1.0,AMAX1(0.0,(LANC-LNCGL)/(LNCGU-LNCGL)))
                ELSE
                 NFG = 1.0 
                ENDIF
                LNCPL = LNCM + NFPL * (LNCX-LNCM)
                LNCPU = LNCM + NFPU * (LNCX-LNCM)
                IF (LNCPU - LNCPL > 1.E-6) THEN
                 NFP =AMIN1(1.0,AMAX1(0.0,(LANC-LNCPL)/(LNCPU-LNCPL)))
                ELSE
                 NFP = 1.0 
                ENDIF
              ELSE
                NFG = 1.0
                NFP = 1.0  
              ENDIF
            ELSE  
              NFG = 1.0
              NFP = 1.0  
            ENDIF

            ! If N stress switched off early in cycle. 
            IF (ISWNITEARLY.EQ.'N') THEN
              NFG = 1.0
              NFP = 1.0  
            ENDIF

            ! Reserves
            IF (RSFPU.GT.0.0.AND.RSFPU.GT.0.0) THEN
              RSFP = 1.-AMIN1(1.,AMAX1(0.,(RSCD-RSFPL)/(RSFPU-RSFPL)))
            ELSE
              RSFP = 1.0
            ENDIF

            ! Temperature
            ! LAH No cold night effect.
            ! Maybe,one cold night --> reduced phs next day!!
            ! May want to introduce:
            ! IF (TMEAN20.LT.0.0) TFG = 0.0
            ! IF (TMEAN20.LT.0.0) TFP = 0.0
            Tfp = TFAC4(trphs,tmean,TTOUT)
            Tfg = TFAC4(trlfg,tmean,TTOUT)
            IF (CFLTFG.EQ.'N') TFG = 1.0

            ! Vapour pressure
            VPDFP = 1.0
            IF (PHTV.GT.0.0) THEN
              IF (TDEW.LE.-98.0) TDEW = TMIN
              VPD = CSVPSAT(tmax) - CSVPSAT(TDEW)    ! Pa 
              IF (VPD/1000.0.GT.PHTV)
     &         VPDFP = AMAX1(0.0,1.0+PHSV*(VPD/1000.0-PHTV))
            ENDIF

            ! CO2 factor using look-up table
            CO2FP = YVALXY(CO2RF,CO2F,CO2)
            ! Co2 factor using CROPGRO formula
            ! CO2EX Exponent for CO2-PHS relationship (0.05)  
            ! COCCC CO2 compensation concentration (80 vpm)
            ! CO2FP = 
            !   PARFC*((1.-EXP(-CO2EX*CO2))-(1.-EXP(-CO2EX*CO2COMPC)))

            !  LAH Notes from original cassava model                                                          
            !  IF (TEMPM .LT. SenCritTemp) THEN
            !     Life(I) = Life(I)-SenTempFac*(SenCritTemp-TEMPM)
            !  ENDIF
            !  IF (CumLAI .GT. SenCritLai) THEN
            !     Life(I) = Life(I)-SenLaiFac*(CumLAI-SenCritLai)
            !  ENDIF
            !  IF (Life(I) .LT. 0.0) Life(I) = 0.0
            !  LSCL   0.4  Leaf senescence,critical LAI
            !  LSCT  18.0  Leaf senescence,critical temperature (C)
            !  LSSL  0.05  Leaf senescence,sensitivity to LAI
            !  LSST   0.3  Leaf senescence,sensitivity to temp (fr/d)

!-----------------------------------------------------------------------
!           Calculate leaf number at end of day;adjust PHINT if needed
!-----------------------------------------------------------------------
                                               
            LAGEG = 0.0
            LNUMG = 0.0
            ! Reduce PHINT with development stage (LAH Sept 2012)
            PHINT = 1.0/((1.0/PHINTS)*(1.0-PHINTFAC*DSTAGE))
            LNUMEND = LNUM + (TT*EMRGFR)/PHINT
            
            ! Restrict to maximum
            LNUMEND = AMIN1(FLOAT(LNUMX),LNUMEND)
            IF(FLN.GT.0.0) LNUMEND = AMIN1(FLN,LNUMEND)
            !LNUMG = LNUMEND - LNUM
            LNUMG = (TT*EMRGFR)/PHINT

!-----------------------------------------------------------------------
!           Calculate senescence of leaves,stems,etc..
!-----------------------------------------------------------------------

            ! LAH Notes from original cassava model. May need to take
            ! into account procedure to calculate leaf senescence. 
            ! Leaves are assumed to have a variety-specific maximum 
            ! life, which can be influenced by temperature and shading
            ! by leaves above. Water stress is assumed not to have any
            ! effect on leaf life (Cock, pers. comm.). However, on 
            ! release of stress leaves drop off and are replaced by a 
            ! flush of new leaves. This is not yet built into the 
            ! model.

            PLASP = 0.0
            PLASI = 0.0
            PLASL = 0.0
            PLASS = 0.0

            ! Leaf senescence - phyllochron or real time driven
            LAPSTMP = 0.0
            DO L = 1,LNUMSG
              IF (LAGETT(L)+TTLFLIFE*EMRGFR.LE.LLIFATT+LLIFGTT) EXIT
              IF (LAP(L)-LAPS(L).GT.0.0) THEN
                LAPSTMP = AMIN1((LAP(L)-LAPS(L)),LAP(L)/LLIFSTT
     &          *AMIN1((LAGETT(L)+(TTLFLIFE*EMRGFR)-(LLIFGTT+LLIFATT))
     &          ,(TTLFLIFE*EMRGFR)))
                LAPS(L) = LAPS(L) + LAPSTMP
                PLASP = PLASP + LAPSTMP
              ENDIF
            ENDDO

            ! Leaf senescence - injury        ! LAH  To add later?
            !PLASI = PLA*(LSENI/100.0)*DU/STDAY  ! May need injury loss

            ! Leaf senescence - water or N stress
            ! LAH Need to accelerated senescence rather than lose leaf
            PLASW = 0.0
            PLASN = 0.0
            IF (ISWWAT.NE.'N') THEN
              IF (PLA-SENLA.GT.0.0.AND.WUPR.LT.WFSU)
     &          PLASW = AMAX1(0.0,AMIN1(
     &              (PLA-SENLA)-PLAS,(PLA-SENLA)*LLOSA))
            ENDIF
            IF (ISWNIT.NE.'N') THEN
              LNCSEN = LNCM + NFSU * (LNCX-LNCM)
              IF (PLA-SENLA.GT.0.0.AND.LANC.LT.LNCSEN)
     &          PLASN = AMAX1(0.0,AMIN1(
     &          (PLA-SENLA)-PLAS,(PLA-SENLA)*LLOSA))
            ENDIF
            ! LAH TMP
            PLASW = 0.0
            PLASN = 0.0
            PLASS = PLASW + PLASN    ! Loss because of stress
              
            ! Leaf senescence - low light at base of canopy
            ! NB. Just senesces any leaf below critical light fr 
            PLASL = 0.0
            IF (LAI.GT.LAIXX) THEN
             PLASL = (LAI-LAIXX) / (PLTPOP*0.0001)
             ! LAH Eliminated! Replaced by accelerated senescence
             PLASL = 0.0
            ENDIF
            
            ! Leaf senescence - overall
             PLAS =  PLASP + PLASI + PLASS + PLASL
            ! Overall check to restrict senescence to what available
            PLAS = AMAX1(0.0,AMIN1(PLAS,PLA-SENLA))

!-----------------------------------------------------------------------
!           Calculate C and N made available through senescence
!-----------------------------------------------------------------------

            SENLFG = 0.0
            SENLFGRS = 0.0
            SENNLFG = 0.0
            SENNLFGRS = 0.0
            IF (PLA-SENLA.GT.0.0) THEN
              ! LAH New algorithms 03/04/13
              SENLFG = AMIN1(LFWT*LWLOS,(AMAX1(0.0,         
     &        (LFWT*(PLAS/(PLA-SENLA))*LWLOS))))
              SENLFGRS = AMIN1(LFWT*(1.0-LWLOS),(AMAX1(0.0,         
     &        (LFWT*(PLAS/(PLA-SENLA))*(1.0-LWLOS)))))
            ENDIF
  
            IF (ISWNIT.NE.'N') THEN
              ! NB. N loss has a big effect if low N
              ! Assumes that all reserve N in leaves
              IF (LFWT.GT.0.0) LANCRS = (LEAFN+RSN) / LFWT
              SENNLFG = AMIN1(LEAFN,(SENLFG+SENLFGRS)*LNCM)
              SENNLFGRS = AMIN1(LEAFN-SENNLFG,
     &                    (SENLFG+SENLFGRS)*(LANC-LNCM))
            ELSE
              SENNLFG = 0.0
              SENNLFGRS = 0.0
            ENDIF

!-----------------------------------------------------------------------
!           Calculate overall senescence loss from tops
!-----------------------------------------------------------------------

            SENFR = 1.0
            SENTOPLITTERG = 0.0
            SENTOPLITTERG = SENLFG*SENFR

!-----------------------------------------------------------------------
!           Calculate C assimilation at beginning of day
!-----------------------------------------------------------------------

            ! PAR utilization efficiency
            PARU = PARUE

            ! Conventional method using PAR utilization efficiency (P)
            CARBOTMPR = AMAX1(0.0,(PARMJFAC*SRAD)*PARU*CO2FP*TFP
     &       * WFP * NFP * RSFP * VPDFP * SLPF)
            CARBOBEGR = CARBOTMPR * PARI / PLTPOP

            ! Modified conventional using internal CO2 (I)
            CARBOTMP = AMAX1(0.,PARMJFAC*SRAD*PARU*TFP*NFP*RSFP)
            ! Calculate for no water stress for WFPI determination
            CARBOTMPI = CARBOTMP
            CO2INTPPMP = CO2
            DO L = 1,20
              CO2INT = CO2AIR - CARBOTMPI * 
     &        (RATM+RCROP*RLFC/RLF*WFP*(1.0*(1.0-WFP)))*1.157407E-05
              CO2INTPPM = AMAX1(CO2COMPC+20.0,CO2INT *
     &        (8.314*1.0E7*((TMAX+TMIN)*.5+273.))/(1.0E12*1.0E-6*44.))
              CO2FPI = PARFC*
     &         ((1.-EXP(-CO2EX*CO2INTPPM))-(1.-EXP(-CO2EX*CO2COMPC)))
              CARBOTMPI = CARBOTMP * CO2FPI
              IF (ABS(CO2INTPPM-CO2INTPPMP).LT.1.0) EXIT
              CO2INTPPMP = CO2INTPPM
            ENDDO
            CARBOBEGIA = 0.0
            IF (CARBOTMPI.GT.0) CARBOBEGIA =(CARBOTMP*CO2FP)/CARBOTMPI
            CARBOTMPI = CARBOTMP
            CO2INTPPMP = CO2
            DO L = 1,20
              CO2INT = CO2AIR - CARBOTMPI * (RATM+RCROP*RLFC/RLF*WFP*
     &        (1.*(1.-WFP)))*1.157407E-05
              CO2INTPPM = AMAX1(CO2COMPC,CO2INT *
     &        (8.314*1.0E7*((TMAX+TMIN)*0.5+273.))/
     &        (1.0E12*1.0E-6*44.0))
              CO2FPI = PARFC*
     &        ((1.-EXP(-CO2EX*CO2INTPPM))-(1.-EXP(-CO2EX*CO2COMPC)))
              CARBOTMPI = CARBOTMP * CO2FPI
              IF (ABS(CO2INTPPM-CO2INTPPMP).LT.1.0) EXIT
              IF (ABS(CO2INTPPM-CO2COMPC).LT.1.0) EXIT
              CO2INTPPMP = CO2INTPPM
            ENDDO
            CARBOBEGI = CARBOTMPI * SLPF * PARI / PLTPOP * CARBOBEGIA

            ! Alternate method using resistances as per Monteith (M)
            ! Calculate photosynthetic efficiency
            ! Use 10 Mj.m2.d PAR to establish quantum requirement
            PHOTQR = (CO2AIR/(10.0*PARU)-
     &       ((RATM+RCROP*RLFC/RLF*WFP*(1.*(1.-WFP)))*1.157407E-05))*
     &       (10.0*30.0)/(CO2AIR*MJPERE) ! 30 = MW Ch2o
            RM = CO2AIR/(((SRAD*PARMJFAC/MJPERE)/PHOTQR)*30.0)
            CARBOTMPM = AMAX1(0.,
     &       (CO2AIR/((RATM+RCROP*RLFC/RLF*WFP*(1.*(1.-WFP)))*
     &       1.157407E-05+RM))*TFP*NFP*RSFP)
            CARBOBEGM = CARBOTMPM * SLPF * PARI / PLTPOP

            ! Select method depending on choice in CONTROL FILE
            IF (MEPHO.EQ.'R') CARBOBEG = CARBOBEGR
            IF (MEPHO.EQ.'M') CARBOBEG = CARBOBEGM
            IF (MEPHO.EQ.'I') CARBOBEG = CARBOBEGI

!-----------------------------------------------------------------------
!           Partitioning of C to above ground and roots (minimum) 
!-----------------------------------------------------------------------

            PTF = PTFMN+(PTFMX-PTFMN)*DSTAGE     
            ! Partition adjustment for stress effects
            PTF = AMIN1(PTFMX,PTF-PTFA*(1.0-AMIN1(WFG,NFG)))
            CARBOR = AMAX1(0.0,(CARBOBEG+CARBOADJ))*(1.0-PTF)
            CARBOT = AMAX1(0.0,(CARBOBEG+CARBOADJ)) - CARBOR

            ! Stem fraction or ratio to leaf whilst leaf still growing
            ! (If a constant STFR is entered,swfrx is set=stfr earlier)
            ! Increases linearly between specified limits
            SWFR = CSYVAL (LNUM,SWFRNL,SWFRN,SWFRXL,SWFRX)

            ! Crown fraction 
            GROCRFR = 0.0
            ! Increases linearly from start to end of growth cycle
            GROCRFR = CRFR * DSTAGE

!-----------------------------------------------------------------------
!           Storage root basic growth and number determination
!-----------------------------------------------------------------------

            GROSR = 0.0
            IF(CUMDU+DU.LT.DUSRI)THEN
                SRDAYFR = 0.0
            ELSEIF(CUMDU.LT.DUSRI.AND.CUMDU+DU.GE.DUSRI)THEN
                SRDAYFR = (DUSRI-CUMDU)/DU
            ELSEIF(CUMDU.GT.DUSRI)THEN
                SRDAYFR = 1.0
            ENDIF
            GROSR = SRFR*CARBOT*SRDAYFR
            
            IF(CUMDU.GE.DUSRI.AND.SRNOPD.LE.0.0) THEN
              SRNOPD = INT(SRNOW*((LFWT+STWT+CRWT+RSWT)))
            ENDIF
                     
!-----------------------------------------------------------------------
!           Specific leaf area
!-----------------------------------------------------------------------

            IF (LAWTR.GT.0.0.AND.LAWTS.GT.0.0.AND.LAWTS.GT.TMEAN) THEN
              TFLAW = 1.0+LAWTR*(TMEAN-LAWTS)
            ELSE
              TFLAW = 1.0
            ENDIF
            IF (LAWWR.GT.0.0.AND.WFG.LT.1.0) THEN
              WFLAW = 1.0+LAWWR*(WFG-1.0)
            ELSE
              WFLAW = 1.0
            ENDIF

            ! Position effect on standard SLA
            IF (LNUMSG.GT.0) THEN
              LAWL(1) = AMAX1(LAWS*LAWFF,LAWS+(LAWS*LAWCF)*(LNUMSG-1))
              ! Temperature and water stress effects on SLA at position
              LAWL(1) = AMAX1(LAWL(1)*LAWMNFR,LAWL(1)*TFLAW*WFLAW)
            ELSE  
              LAWL(1) = LAWS
            ENDIF 

!-----------------------------------------------------------------------
!           Leaf growth
!-----------------------------------------------------------------------

            GROLF = 0.0
            GROLFADJ = 0.0
            GROLFP = 0.0
            GROLSRS = 0.0
            GROLS = 0.0
            GROLSA = 0.0
            GROLSP = 0.0
            GROLSRT = 0.0
            GROLSSEN = 0.0
            GROLSRTN = 0.0
            GROLSSD = 0.0
            LAGEG = 0.0
            PLAGS2 = 0.0
            SHLAG2 = 0.0
            SHLAGB2 = 0.0
            SHLAGB3 = 0.0
            SHLAGB4 = 0.0
            
            ! BRANCH NUMBER            
            ! Old method (1 fork number throughout)
            ! BRNUMST = AMAX1(1.0,BRNUMFX**(INT(brstage)-1))
            ! New method (fork number specified for each forking point)
            ! First calculate new BRSTAGE as temporary variable
            ! (LAH Check whether can move brstage calc up here! 
            ! (If do this, brstage in brfx below must be reduced by 1))
            IF (PDL(INT(BRSTAGE)).GT.0.0) THEN
               TVR1 = FLOAT(INT(BRSTAGE))
     &              + (LNUM-LNUMTOSTG(INT(BRSTAGE)))/PDL(INT(BRSTAGE))
            ELSE
               TVR1 = FLOAT(INT(BRSTAGE))
            ENDIF
            IF (INT(TVR1).GT.INT(BRSTAGEPREV)) THEN
              IF (BRSTAGE.EQ.0.0) THEN
                BRNUMST = 1 
              ELSEIF (BRSTAGE.GT.0.0) THEN
                BRNUMST = BRNUMST*BRFX(INT(BRSTAGE))
              ENDIF
            ENDIF

            ! Potential leaf size for next growing leaf - main shoot 
            LNUMNEED = FLOAT(INT(LNUM+1)) - LNUM
            IF (ABS(LNUMNEED).LE.1.0E-6) LNUMNEED = 0.0
            IF (LNUMSG+1.LE.INT(LAXNO)) THEN
              LAPOTX(LNUMSG+1) = AMIN1(LAXS, LA1S + 
     &         LNUMSG*((LAXS-LA1S)/(LAXNO-1)))
            ELSEIF (LNUMSG+1.GT.INT(LAXNO).AND.
     &        LNUMSG+1.LE.INT(LAXN2)) THEN
              LAPOTX(LNUMSG+1) = LAXS
            ELSE
              LAPOTX(LNUMSG+1) = AMAX1(LAFS, LAXS - 
     &         ((LNUMSG+1)-LAXN2)*((LAXS-LAFS)/(LAFND-LAXN2)))
            ENDIF
            
            ! LAH Sept 2012 Eliminate fork # effect on leaf size 
            ! Adjust for fork#/shoot
            !IF (BRNUMST.GE.1)LAPOTX(LNUMSG+1)=LAPOTX(LNUMSG+1)/BRNUMST
            ! Keep track of 'forking';reduce potential>forking
            !IF (LNUMSG.GT.1.AND.BRNUMPT.GT.BRNUMSTPREV) THEN
            !  LAPOTX(LNUMSG+1) = LAPOTX(LNUMSG+1)/LAFF
            !  LNUMFORK = LNUMSG
            !ENDIF 
            
            ! Leaf area increase:growing leaves on 1 axis,main shoot
            SHLAG2(1) = 0.0
            DO L = MAX(1,LNUMSG-1-INT((LLIFG/PHINTS))),LNUMSG+1
              ! Basic leaf growth calculated on thermal time base. 
              ! Basic response (cm2/d) same as for development. 
              TTNEED = AMAX1(0.0,LLIFG-LAGETT(L))
              LATLPREV(1,L) = LATL(1,L)
              LATLPOT(1,L)=LAPOTX(L)*((LAGETT(L)+TTLFLIFE*EMRGFR)/LLIFG)
              IF (LATLPOT(1,L).LT.0.0) LATLPOT(1,L) = 0.0
              IF (LATLPOT(1,L).GT.LAPOTX(L)) LATLPOT(1,L) = LAPOTX(L)
              LATL(1,l) = LATL(1,L) + (LATLPOT(1,L)-LATLPREV(1,L))
              LATL2(1,l) = LATL2(1,L) + (LATLPOT(1,L)-LATLPREV(1,L))
     &                                * AMIN1(WFG,NFG)*TFG
              SHLAG2(1) = SHLAG2(1) + (LATLPOT(1,L)-LATLPREV(1,L))
     &                            * AMIN1(WFG,NFG)*TFG
              ! The 2 at the end of the names indicates that 2 groups 
              ! of stresses have been taken into account
              ! Stress factors for individual leaves
              WFLF(L) = AMIN1(1.0,
     &               WFLF(L)+WFG*(LATLPOT(1,L)-LATLPREV(1,L))/LAPOTX(L))
              NFLF(L) = AMIN1(1.0,
     &               NFLF(L)+NFG*(LATLPOT(1,L)-LATLPREV(1,L))/LAPOTX(L))
              NFLFP(L) = AMIN1(1.0,
     &              NFLFP(L)+NFP*(LATLPOT(1,L)-LATLPREV(1,L))/LAPOTX(L))
              TFGLF(L) = AMIN1(1.0,
     &              TFGLF(L)+TFG*(LATLPOT(1,L)-LATLPREV(1,L))/LAPOTX(L))
              TFDLF(L) = AMIN1(1.0,
     &              TFDLF(L)+TFD*(LATLPOT(1,L)-LATLPREV(1,L))/LAPOTX(L))
              ! New LEAF
              IF (L.EQ.LNUMSG.AND.LNUMG.GT.LNUMNEED) THEN
                LAGL(1,L+1) = LAPOTX(L+1) 
     &                      * (TTLFLIFE*EMRGFR)
     &                      * (((LNUMG-LNUMNEED)/LNUMG)/LLIFG) 
                LATL(1,L+1) = LATL(1,L+1) + LAGL(1,L+1)
                LATL2(1,L+1) = LATL2(1,L+1) + LAGL(1,L+1)
     &                       * AMIN1(WFG,NFG)*TFG
                SHLAG2(1) = SHLAG2(1) + LAGL(1,L+1)
     &                              * AMIN1(WFG,NFG)*TFG
                LBIRTHDAP(L+1) = DAP  
                ! Stress factors for individual leaves
                WFLF(L+1) = AMIN1(1.0,
     &                WFLF(L+1)+WFG*LATL(1,L+1)/LAPOTX(L+1))
                NFLF(L+1) = AMIN1(1.0,
     &                NFLF(L+1)+NFG*LATL(1,L+1)/LAPOTX(L+1))
                NFLFP(L+1) = AMIN1(1.0,
     &                NFLFP(L+1)+NFP*LATL(1,L+1)/LAPOTX(L+1))
                TFGLF(L+1) = AMIN1(1.0,
     &                TFGLF(L+1)+TFG*LATL(1,L+1)/LAPOTX(L+1))
                TFDLF(L+1) = AMIN1(1.0,
     &                TFDLF(L+1)+TFD*LATL(1,L+1)/LAPOTX(L+1))
              ENDIF

            ENDDO
 
            ! Leaf area increase:growing leaves on 1 axis,all shoots
            PLAGS2 = SHLAG2(1) ! To initialize before adding over shoots
            DO L = 2,INT(SHNUM+2) ! L is shoot cohort,main=cohort 1
              IF (SHNUM-FLOAT(L-1).GT.0.0) THEN
                PLAGS2 = PLAGS2+SHLAG2(1)*SHGR(L)           
     &                 * AMAX1(0.,AMIN1(FLOAT(L),SHNUM)-FLOAT(L-1))
                SHLAG2(L) = SHLAG2(1)*SHGR(L)             
     &                    * AMAX1(0.,AMIN1(FLOAT(L),SHNUM)-FLOAT(L-1))
              ENDIF
            ENDDO

            ! Leaf area increase:growing leaves on all axes,all shoots
            PLAGSB2 = PLAGS2*BRNUMST
            SHLAGB2(1) = SHLAG2(1)*BRNUMST
            DO L = 2,INT(SHNUM+2) ! L is shoot cohort,main= 1
              SHLAGB2(L) =  SHLAG2(L)*BRNUMST
            ENDDO
            
            ! Potential leaf weight increase.
            IF (LAWL(1).GT.0.0)
     &       GROLFP = (PLAGSB2/LAWL(1)) / (1.0-LPEFR)    

            ! Potential leaf+stem weight increase.
            IF (SWFR.GT.0.0.AND.SWFR.LT.1.0) THEN
              GROLSP = GROLFP * (1.0 + SWFR/(1.0-SWFR))
            ELSE
              GROLSP = GROLFP
            ENDIF

            IF (GROLSP.GT.0.0) THEN
              ! Leaf+stem weight increase from assimilates
              GROLSA = AMAX1(0.,AMIN1(GROLSP,CARBOT-GROSR))

              ! Leaf+stem weight increase from senescence 
              IF (GROLSA.LT.GROLSP) THEN
                GROLSSEN = AMIN1(GROLSP-GROLSA,SENLFGRS)
              ENDIF
            
              IF (GROLSA+GROLSSEN.LT.GROLSP) THEN
                ! Leaf+stem weight increase from seed reserves
                ! LAH May need to restrict seed use.To use by roots?
                GROLSSD = AMIN1((GROLSP-GROLSA-GROLSSEN),SEEDRSAV)
                SEEDRSAV = SEEDRSAV - GROLSSD
                IF ( LAI.LE.0.0.AND.GROLSSD.LE.0.0
     &                        .AND.SEEDRSAV.LE.0.0          
     &                        .AND.ESTABLISHED.NE.'Y') THEN
                  CFLFAIL = 'Y'
                  WRITE (Message(1),'(A41)')
     &             'No seed reserves to initiate leaf growth '
                  WRITE (Message(2),'(A33,F8.3,F6.1)')
     &            '  Initial seed reserves,seedrate ',seedrsi,sdrate
                  WRITE (Message(3),'(A33,F8.3,F6.1)')
     &            '  Reserves %,plant population    ',sdrsf,pltpop 
                  CALL WARNING(3,'CSCAS',MESSAGE)
                ENDIF
              ENDIF
              ! Leaf+stem weight increase from plant reserves
              IF (GROLSA+GROLSSD+GROLSSEN.LT.GROLSP) THEN
                GROLSRS = 
     &            AMIN1(RSWT*RSUSE,GROLSP-GROLSA-GROLSSD-GROLSSEN)
              ENDIF
              ! Leaf+stem weight increase from roots (after drought)
              GROLSRT = 0.0
              GROLSRTN = 0.0
              IF ((GROLSA+GROLSSD+GROLSRS+GROLSSEN).LT.GROLSP.AND.
     &         SHRTD.LT.1.0.AND.RTUFR.GT.0.0.AND.
     &         ESTABLISHED.EQ.'Y') THEN
                GROLSRT = AMIN1(RTWT*RTUFR,
     &            (GROLSP-GROLSA-GROLSSD-GROLSSEN-GROLSRS))
                IF (ISWNIT.NE.'N') THEN
                  GROLSRTN = GROLSRT * RANC
                ELSE
                  GROLSRTN = 0.0
                ENDIF  
                WRITE(Message(1),
     &           '(A16,A12,F3.1,A8,F7.4,A7,F7.4,A9,F7.4)')
     &           'Roots -> leaves ',
     &           ' Shoot/root ',shrtd,
     &           ' Grolsp ',grolsp,' Grols ',grols,
     &           ' Grolsrt ',grolsrt
                CALL WARNING(1,'CSCAS',MESSAGE)
              ENDIF

              ! Leaf+stem weight increase from all sources
              GROLS = GROLSA + GROLSSEN + GROLSSD + GROLSRS+GROLSRT

              ! Leaf weight increase from all sources
              IF ((GROLSP).GT.0.0) THEN
                GROLF = GROLS * GROLFP/GROLSP
              ELSE  
                GROLF = 0.0
              ENDIF
              
!               Check if enough assimilates to maintain SLA within limits
              AREAPOSSIBLE = GROLF*(1.0-LPEFR)*(LAWL(1)*(1.0+LAWFF))
                
!               If not enough assim.set assimilate factor
              IF (PLAGSB2.GT.AREAPOSSIBLE.AND.PLAGSB2.GT.0.0)THEN
                AFLF(0) = AREAPOSSIBLE/PLAGSB2
              ELSE  
                AFLF(0) = 1.0
              ENDIF
              IF (CFLAFLF.EQ.'N') AFLF(0) = 1.0
              
              ! Area and assimilate factors for each leaf
              DO L = MAX(1,LNUMSG-1-INT((LLIFG/PHINTS))),LNUMSG+1 
                IF (LNUMSG.LT.LNUMX) THEN
                  LATL3(1,L)= LATL2(1,L) * AFLF(0)            
                  AFLF(L) = AMIN1(1.0,AFLF(L)
     &                    + AMAX1(0.0,AFLF(0))
     &                    * (LATLPOT(1,L)-LATLPREV(1,L))/LAPOTX(L))
                  IF (CFLAFLF.EQ.'N') AFLF(L) = 1.0
                ENDIF  
              ENDDO
            
              PLAGSB3 = PLAGSB2 * AFLF(0)
              SHLAGB3(1) = SHLAGB2(1) * AFLF(0)
              SHLAGB3(2) = SHLAGB2(2) * AFLF(0)
              SHLAGB3(3) = SHLAGB2(3) * AFLF(0)

            ENDIF
            
!-----------------------------------------------------------------------
!           Stem and crown growth                                     
!-----------------------------------------------------------------------

            GROCR = 0.0
            GROSTCRP = 0.0
            GROST = 0.0
            GROSTCR = 0.0
            STAIG = 0.0
            STAIS = 0.0
            ! Potential stem weight increase.
            IF (SWFR.LT.1.0) THEN
              GROSTCRP = GROLFP * SWFR/(1.0-SWFR)
              GROSTCRPSTORE = AMAX1(GROLFP,GROSTCRPSTORE)
            ELSE  
              GROSTCRP = GROSTCRPSTORE
              ! LAH May need to change GROSTCRP as progress
            ENDIF
            
            IF (GROLFP+GROSTCRP.GT.0.0)
     &        GROSTCR = GROLS * GROSTCRP/(GROLFP+GROSTCRP)
     &                         * (1.0-RSFRS)
            ! LAH RSFRS is the fraction of stem growth to reserves
            ! May need to have this change as stem growth proceeds
     
            ! Crown (Planting stick) .. in balance with stem
            GROCR = GROSTCR * GROCRFR
            GROST = GROSTCR * (1.0-GROCRFR)
                          
!-----------------------------------------------------------------------
!           Root growth                                     
!-----------------------------------------------------------------------

            RTWTG = (CARBOR+SEEDRSAVR)*(1.0-RRESP) 
            RTRESP = RTWTG*RRESP/(1.0-RRESP)   

!-----------------------------------------------------------------------
!           N Uptake  -  and growth adjustments if inadequate N
!-----------------------------------------------------------------------

            ! Set adjusted values to unadjusted 
            ! For when no adjustment necessary, or when not simulating N
            GROCRADJ = GROCR
            GROLFADJ = GROLF
            GROSTADJ = GROST
            PLAGSB4 = PLAGSB3
            RSSRWTGLFADJ = 0.0
            RTRESPADJ = RTRESP   
            RTWTGADJ = RTWTG
            SHLAGB4 = SHLAGB3

            IF (ISWNIT.NE.'N') THEN

              ANDEM = 0.0
              RNDEM = 0.0
              LNDEM = 0.0
              SNDEM = 0.0
              SRNDEM = 0.0
              RNDEMG = 0.0
              LNDEMG = 0.0
              SNDEMG = 0.0
              SRNDEMG = 0.0
              RNDEMTU = 0.0
              LNDEMTU = 0.0
              SNDEMTU = 0.0
              SRNDEMTU = 0.0
              SEEDNUSE = 0.0
              SEEDNUSE2 = 0.0
              RSNUSED = 0.0

              SNO3PROFILE = 0.0
              SNH4PROFILE = 0.0
              SNO3ROOTZONE = 0.0
              SNH4ROOTZONE = 0.0
              TRLV = 0.0
              DO L = 1, NLAYR
                TRLV = TRLV + RLV(L)
                FAC(L) = 10.0/(BD(L)*DLAYR(L))
                SNO3(L) = NO3LEFT(L) / FAC(L)
                SNH4(L) = NH4LEFT(L) / FAC(L)
                SNO3PROFILE = SNO3PROFILE + SNO3(L)
                SNH4PROFILE = SNH4PROFILE + SNH4(L)
                IF (RLV(L).GT.0.0) THEN
                 SNO3ROOTZONE = SNO3ROOT ZONE + SNO3(L)
                 SNH4ROOTZONE = SNH4ROOTZONE + SNH4(L)
                ENDIF
              END DO

              LNDEM = GROLF*LNCX +
     &            (LFWT-SENLFG-SENLFGRS)*AMAX1(0.0,NTUPF*(LNCX-LANC)) -
     &            GROLSRTN
              SNDEM = AMAX1(0.0,GROST+GROCR)*SNCX +
     &              (STWT+CRWT)*AMAX1(0.0,NTUPF*(SNCX-SANC))
              RNDEM = RTWTG*RNCX + 
     &              (RTWT-SENRTG-GROLSRT)*AMAX1(0.0,NTUPF*(RNCX-RANC))
              SRNDEM = (GROSR+SRWTGRS)*(SRNPCS/100.0) +
     &            SRWT*AMAX1(0.0,NTUPF*((SRNPCS/100.0)-SRANC)) 
              
              ! Seed use if no roots
              ! N use same % of initial as for CH20,if needed.
              IF (RTWT.LE.0.0) THEN
                SEEDNUSE = AMAX1(0.0,
     &           AMIN1(SEEDN,LNDEM+SNDEM+RNDEM,SEEDNI/SDDUR*(TT/STDAY)))
              ELSE
                ! Some use of seed (0.5 need) even if may not be needed
                SEEDNUSE = AMAX1(0.0,AMIN1(SEEDN,
     &               0.5*(LNDEM+SNDEM+RNDEM),SEEDNI/SDDUR*(TT/STDAY)))
              ENDIF
              
              ! Reserves used before uptake
              RSNUSED = AMIN1(LNDEM+SNDEM+RNDEM+SRNDEM-SEEDNUSE,RSN)

              ! N uptake needed 
              ANDEM = AMAX1(0.0,PLTPOP*10.0*
     &          (LNDEM+SNDEM+RNDEM+SRNDEM-SEEDNUSE-RSNUSED))
              ! Uptake
              
              ! Original from CSM with some 'modification'.  
              ! RNUMX = RTNO3,RTNH4 = N uptake/root length (mgN/cm,.006)
              ! RNO3U,RNH4U  = Nitrogen uptake (kg N/ha)
              ! RNUMX = 0.006    
              WFNU = 1.0
              NUPAP = 0.0
              RNO3U = 0.0
              RNH4U = 0.0
              TVR1 = -0.08  ! Could be AMAX1(0.01,(0.20-NconcU*0.004))
              DO L=1,NLAYR
                IF (RLV(L) .GT. 0.0) THEN
                  NLAYRROOT = L
                  ! N concentration effects
                  FNH4 = 1.0-EXP(TVR1*NH4CF * NH4LEFT(L))
                  FNO3 = 1.0-EXP(TVR1*NO3CF * NO3LEFT(L))
                  IF (NO3LEFT(L) .LE. NO3MN) FNO3 = 0.0  
                  IF (FNO3 .GT. 1.0)  FNO3 = 1.0
                  IF (NH4LEFT(L) .LE. NH4MN) FNH4 = 0.0  
                  IF (FNH4 .GT. 1.0)  FNH4 = 1.0
                  ! Water effects
                  IF (SW(L) .LE. DUL(L)) THEN
                    WFNU = (SW(L) - LL(L)) / (DUL(L) - LL(L)) 
                  ELSE
                    WFNU = 1.0-(SW(L)-DUL(L))/(SAT(L)-DUL(L))
                    WFNU = 1.0 ! Wet soil effect not implemented
                  ENDIF
                  IF (WFNU.LT.0.0) WFNU = 0.0
                  ! LAH Note that WFNU squared
                  TVR2 = AMAX1(0.0,1.0 - H2OCF*(1.0-(WFNU*WFNU)))
                  RFAC = RLV(L) * TVR2 * DLAYR(L) * 100.0
                  RNO3U(L) = RFAC * FNO3 * RTNO3
                  RNH4U(L) = RFAC * FNH4 * RTNH4
                  RNO3U(L) = MAX(0.0,RNO3U(L))
                  RNH4U(L) = MAX(0.0,RNH4U(L))
                  NUPAP = NUPAP + RNO3U(L) + RNH4U(L) !kg[N]/ha
                ENDIF
              ENDDO

              ! Ratio (NUPRATIO) to indicate N supply for output
              IF (ANDEM.GT.0) THEN
                NUPRATIO = NUPAP/ANDEM
              ELSE
                IF (NUPAP.GT.0.0) THEN
                  NUPRATIO = 10.0
                ELSE  
                  NUPRATIO = 0.0
                ENDIF  
              ENDIF
              ! Factor (NUF) to reduce N uptake to level of demand
              NUF = 1.0
              IF (NUPAP.GT.0.0) THEN
                NUF = AMIN1(1.0,ANDEM/NUPAP)
              ENDIF 

              ! Actual N uptake by layer roots based on demand (kg/ha)
              UNO3 = 0.0
              UNH4 = 0.0
              NUPD = 0.0
              NUPAD = 0.0
              DO L = 1, NLAYRROOT
                UNO3(L) = RNO3U(L)*NUF
                UNH4(L) = RNH4U(L)*NUF
                IF (FAC(L).LE.0.0) THEN
                  XMIN = 0.0
                ELSE  
                  XMIN = NO3MN/FAC(L) 
                ENDIF  
                UNO3(L) = MAX(0.0,MIN (UNO3(L),SNO3(L)-XMIN))
                IF (FAC(L).LE.0.0) THEN
                  XMIN = 0.0
                ELSE  
                  XMIN = NH4MN/FAC(L) 
                ENDIF  
                XMIN = NH4MN/FAC(L) 
                UNH4(L) = MAX(0.0,MIN (UNH4(L),SNH4(L)-XMIN))
                NUPAD = NUPAD + UNO3(L) + UNH4(L)                  
              END DO
              IF (PLTPOP > 1.E-6) THEN
                NUPD = NUPAD/(PLTPOP*10.0)
              ELSE
                NUPD = 0.
              ENDIF

              SEEDNUSE2 = 0.0
              ! Seed use after using reserves and uptake
              IF (RTWT.GT.0.0.AND.ISWNIT.NE.'N') THEN
                SEEDNUSE2 = AMAX1(0.0,AMIN1(SEEDN-SEEDNUSE,LNDEM+SNDEM+
     &           RNDEM-RSNUSED-SEEDNUSE-NUPD,SEEDNI/SDDUR*(TT/STDAY)))
              ELSE
                SEEDNUSE2 = 0.0
              ENDIF

              ! Distribute N to leaves,stem,root,and storage root
              LNUSE = 0.0
              SNUSE = 0.0
              RNUSE = 0.0
              SRNUSE = 0.0
              NULEFT = SEEDNUSE+SEEDNUSE2+RSNUSED+NUPD

              ! For supplying minimum
              NDEMMN = GROLF*LNCM+RTWTG*RNCM
     &              +(GROST+GROCR)*SNCM+GROSR*(SRNPCS/100.0)*0.5
              LNUSE(1) = (GROLF*LNCM)*AMIN1(1.0,NULEFT/NDEMMN)
              RNUSE(1) = (RTWTG*RNCM)*AMIN1(1.0,NULEFT/NDEMMN)
              SNUSE(1) = 
     &         ((GROST+GROCR)*SNCM)*AMIN1(1.0,NULEFT/NDEMMN)
              SRNUSE(1) = 
     &           (GROSR*(SRNPCS/100.0)*0.5)*AMIN1(1.0,NULEFT/NDEMMN)

              ! Reduce stem,crown,root growth if N < supply minimum
              IF (NDEMMN.GT.NULEFT) THEN
                GROSTADJ = GROST*AMIN1(1.0,NULEFT/NDEMMN)
                GROCRADJ = GROCR*AMIN1(1.0,NULEFT/NDEMMN)
                RTWTGADJ = RTWTG*AMIN1(1.0,NULEFT/NDEMMN)
                RTRESPADJ = RTWTGADJ*RRESP/(1.0-RRESP)   
              ELSE
                GROSTADJ = GROST
                GROCRADJ = GROCR
                RTWTGADJ = RTWTG
                RTRESPADJ = RTRESP   
              ENDIF
     
              NULEFT = NULEFT - LNUSE(1)-RNUSE(1)-SNUSE(1)-SRNUSE(1)

              ! 5.For leaf growth to standard N (N to leaves first)
              LNUSE(2) = AMIN1(NULEFT,(GROLF*LNCX)-LNUSE(1))
              !Could use the NLLG parameter but may need to adjust 
              !the photosynthesis-leaf N response parameters, or
              !the standard PARUE  
              !LNUSE(2) = AMIN1(NULEFT,(GROLF*LNCX*NLLG)-LNUSE(1))
              NULEFT = NULEFT - LNUSE(2)

              ! 6.For distribution of remaining N to st,rt,storage root
              NDEM2 = SNDEM-SNUSE(1)+RNDEM-RNUSE(1)+SRNDEM-SRNUSE(1)
              IF (NDEM2.GT.0.0)THEN
                SNUSE(2) = (SNDEM-SNUSE(1)) * AMIN1(1.0,NULEFT/NDEM2)
                RNUSE(2) = (RNDEM-RNUSE(1)) * AMIN1(1.0,NULEFT/NDEM2)
                SRNUSE(2) = (SRNDEM-SRNUSE(1))*AMIN1(1.0,NULEFT/NDEM2)
                NULEFT = NULEFT - SNUSE(2) - RNUSE(2) - SRNUSE(2)
                IF (NULEFT.GT.0.0) THEN
                  LNUSE(3) = NULEFT
                ELSE
                  LNUSE(3) = 0.0
                ENDIF
              ELSE
                LNUSE(3) = 0.0
                SNUSE(2) = 0.0
                RNUSE(2) = 0.0
                SRNUSE(2) = 0.0
              ENDIF  

              LNUSE(0) = LNUSE(1) + LNUSE(2) + LNUSE(3)
              SNUSE(0) = SNUSE(1) + SNUSE(2)
              RNUSE(0) = RNUSE(1) + RNUSE(2) 
              SRNUSE(0) = SRNUSE(1) + SRNUSE(2) 

              ! N Pools available for re-mobilization
              NUSEFAC = NLABPC/100.0
              NPOOLR = AMAX1 (0.0,
     &         ((RTWT-SENRTG)*(RANC-RNCM)*NUSEFAC))
              NPOOLL = AMAX1 (0.0, 
     &            ((LFWT-SENLFG-SENLFGRS)*(LANC-LNCM)*NUSEFAC))
              NPOOLS = AMAX1 (0.0,
     &         ((STWT+CRWT)*(SANC-SNCM)*NUSEFAC))
              
              ! Check N and reduce leaf growth if not enough N  
              IF (ABS(NULEFT).LE.1.0E-5) THEN   ! Inadequate N
                IF(NLLG.GT.0.0.AND.LNCX.GT.0.0.AND.GROLF.GT.0.0) THEN 
                  IF ((LNUSE(1)+LNUSE(2))/GROLF.LT.(LNCX*NLLG)) THEN 
                    GROLFADJ = (LNUSE(1)+LNUSE(2))/(LNCX*NLLG)
                  ELSE  
                    GROLFADJ = GROLF
                  ENDIF  
                ENDIF
                RSSRWTGLFADJ = GROLF - GROLFADJ
                
                AREAPOSSIBLEN = 
     &            GROLFADJ*(1.0-LPEFR)*(LAWL(1)*(1.0+LAWFF))
                
                ! If not enough N set N factor
                IF (PLAGSB3.GT.AREAPOSSIBLEN.AND.
     &              PLAGSB3.GT.0.0)THEN
                  NFLF2(0) = AREAPOSSIBLEN/PLAGSB3
                ELSE  
                  NFLF2(0) = 1.0
                ENDIF
              
                ! Area and assimilate factors for each leaf
                DO L = MAX(1,LNUMSG-1-INT((LLIFG/PHINTS))),LNUMSG+1 
                  IF (LNUMSG.LT.LNUMX) THEN
                    LATL4(1,L)= LATL3(1,L) * NFLF2(0)            
                    NFLF2(L) = AMIN1(1.0,NFLF2(L)
     &                       + AMAX1(0.0,NFLF2(0))
     &                       * (LATLPOT(1,L)-LATLPREV(1,L))/LAPOTX(L))
                  ENDIF  
                ENDDO
            
                PLAGSB4 = PLAGSB3 * NFLF2(0)
                SHLAGB4(1) = SHLAGB3(1) * NFLF2(0)
                SHLAGB4(2) = SHLAGB3(2) * NFLF2(0)
                SHLAGB4(3) = SHLAGB3(3) * NFLF2(0)

              ELSE   ! Adequate N 

                NFLF2(0) = 1.0
                DO L = MAX(1,LNUMSG-1-INT((LLIFG/PHINTS))),LNUMSG+1 
                  IF (LNUMSG.LT.LNUMX) THEN
                    LATL4(1,L)= LATL3(1,L) * NFLF2(0)            
                    NFLF2(L) = AMIN1(1.0,NFLF2(L)
     &                       + AMAX1(0.0,NFLF2(0))
     &                       * (LATLPOT(1,L)-LATLPREV(1,L))/LAPOTX(L))
                  ENDIF  
                ENDDO
                
              ENDIF
              
            ELSE     ! ISWNIT = N   

              LATL4 = LATL3
              NFLF2 = 1.0            

            ENDIF    ! End of N uptake and growth adjustmenets

!-----------------------------------------------------------------------
!           Reserves growth
!-----------------------------------------------------------------------

            GRORS = 
     &        CARBOT+GROLSSD+GROLSRT+SENLFGRS
     &          -GROLFADJ-GROSTADJ-GROCRADJ-GROSR
            IF(GRORS.LT.0.0.AND.GRORS.GT.-1.0E-07) GRORS = 0.0

            ! Reserves to STORAGE ROOT if conc too great (overflow!)
            SRWTGRS = 0.0
            ! Determine potential new concentration
            IF (LFWT+GROLFADJ+STWT+CRWT+GROSTADJ+GROCRADJ.GT.0.0) 
     &        TVR1 = (RSWT+GRORS)/
     &        ((LFWT+GROLFADJ-SENLFG-SENLFGRS)
     &        +(STWT+GROSTADJ+CRWT+GROCRADJ)+(RSWT+GRORS))
            IF(TVR1.LT.0.0.AND.TVR1.GT.-1.0E-07) TVR1 = 0.0
            IF (TVR1.GT.RSPCO/100.0) THEN   ! If potential>standard 
              TVR2 = RSWT+GRORS             ! What rswt could be
              TVR3 =                        ! What rswt should be 
     &         ((RSPCO/100.0)
     &         *(LFWT+GROLFADJ-SENLFG-SENLFGRS
     &         +STWT+CRWT+GROSTADJ+GROCRADJ))
     &         /(1.0-(RSPCO/100.0))
              SRWTGRS = (TVR2 - TVR3) 
              ! Determine FINAL new concentration
              IF (LFWT+GROLFADJ+STWT+CRWT+GROSTADJ+GROCRADJ.GT.0.0) 
     &          TVR5 = (RSWT+GRORS-SRWTGRS)/
     &          ((LFWT+GROLFADJ-SENLFG-SENLFGRS)
     &          +(STWT+GROSTADJ+CRWT+GROCRADJ)
     &          +(RSWT+GRORS-SRWTGRS))
            ENDIF

!-----------------------------------------------------------------------
!           Height growth
!-----------------------------------------------------------------------

            CANHTG = 0.0
            CANHTG = SERX*DU

!-----------------------------------------------------------------------
!           Root depth and length growth, and distribution
!-----------------------------------------------------------------------

            RTWTGL = 0.0
            RTWTSL = 0.0
            RTWTUL = 0.0
            RTNSL = 0.0

            IF (GERMFR.GT.0.0.OR.GESTAGE.GE.0.5) THEN

              ! Establish water factor for root depth growth
              IF (ISWWAT.NE.'N') THEN
                LRTIP = CSIDLAYR (NLAYR, DLAYR, RTDEP) ! Root tip layer
                IF (LRTIP.GT.1) THEN
                  SWPRTIP = SWP(LRTIP)
                ELSE
                  SWPRTIP = AMIN1(SWP(2),
     &             (SWP(2)-((DLAYR(1)-RTDEP)/DLAYR(1))*(SWP(2)-SWP(1))))
                ENDIF
                WFRG = 1.0
                IF (WFRTG.GT.0.0)
     &           WFRG = AMAX1(0.0,AMIN1(1.0,(SWPRTIP/WFRTG)))
              ELSE
                WFRG = 1.0
              ENDIF

              ! Root depth growth
              RTDEPG = 0.0
              IF (ISWWAT.NE.'N') THEN
                ! LAH Note reduced effect of SHF, AND no acceleration
                RTDEPG = TT*RDGS/STDAY*GERMFR
     &                 * SQRT(AMAX1(0.3,SHF(LRTIP)))
     &                 * WFRG
              ELSE
                RTDEPG = TT*RDGS/STDAY*GERMFR
              ENDIF
              L = 0
              CUMDEP = 0.0
              RTDEPTMP = RTDEP+RTDEPG
              DO WHILE ((CUMDEP.LE.RTDEPTMP) .AND. (L.LT.NLAYR))
                L = L + 1
                CUMDEP = CUMDEP + DLAYR(L)
                ! LAH Limit on WFRG. 0 WFRG (when 1 layer) -> 0 TRLDF.
                IF (ISWWAT.NE.'N'.AND.WFRTG.GT.0.0) THEN
                  WFRG = AMIN1(1.0,AMAX1(0.1,SWP(L)/WFRTG))
                ELSE
                  WFRG = 1.0
                ENDIF
                IF (ISWNIT.NE.'N'.AND.NCRG.GT.0.0) THEN
                  NFRG = AMIN1(1.0,
     &             AMAX1(0.1,(NO3LEFT(L)+NH4LEFT(L))/NCRG))
                ELSE
                  NFRG = 1.0
                ENDIF 
                ! LAH Tried to use AMAX1 here because layer may have 
                ! lots H20,no N,or inverse, and therefore need roots
                ! But with KSAS8101,AMAX1 lowered yield. Return to AMIN1
                !RLDF(L) = AMAX1(WFRG,NFRG)*SHF(L)*DLAYR(L)
                RLDF(L) = AMIN1(WFRG,NFRG)*SHF(L)*DLAYR(L)
              END DO
              IF (L.GT.0.AND.CUMDEP.GT.RTDEPTMP)
     &         RLDF(L) = RLDF(L)*(1.0-((CUMDEP-RTDEPTMP)/DLAYR(L)))
              NLAYRROOT = L
              ! Root senescence
              SENRTG = 0.0
              DO L = 1, NLAYRROOT
                RTWTSL(L) = RTWTL(L)*(RSEN/100.0)*TT/STDAY 
                ! LAH Temperature effect above is not from soil temp
                IF (RTWT.GT.0.0) RTWTUL(L) = RTWTL(L)*GROLSRT/RTWT
                SENRTG = SENRTG + RTWTSL(L)
                IF (ISWNIT.NE.'N') THEN
                  RTNSL(L) = RTWTSL(L)*RANC
                ELSE
                  RTNSL(L) = 0.0
                ENDIF  
              ENDDO

              ! Root weight growth by layer
              TRLDF = 0.0
              DO  L = 1, NLAYRROOT
                TRLDF = TRLDF + RLDF(L)
              END DO
              IF (TRLDF.GT.0.0) THEN
                DO  L = 1, NLAYRROOT
                  RTWTGL(L) = (RLDF(L)/TRLDF)*(RTWTGADJ)
                END DO
              ENDIF
            ENDIF

!-----------------------------------------------------------------------
!           Water in profile and rootzone
!-----------------------------------------------------------------------
            
            AH2OPROFILE = 0.0
            H2OPROFILE = 0.0
            AH2OROOTZONE = 0.0
            H2OROOTZONE = 0.0
            DO L = 1, NLAYR
              AH2OPROFILE = AH2OPROFILE+((SW(L)-LL(L))*DLAYR(L))*10.
              H2OPROFILE = H2OPROFILE + SW(L)*DLAYR(L)*10.0
              IF (RLV(L).GT.0.0) THEN
               AH2OROOTZONE=AH2OROOTZONE+((SW(L)-LL(L))*DLAYR(L))*10.
               H2OROOTZONE = H2OROOTZONE+SW(L)*DLAYR(L)*10.
              ENDIF
            END DO

!-----------------------------------------------------------------------
!           Rate variables expressed on an area basis
!-----------------------------------------------------------------------

            ! C assimilation
            ! Senesced material added to litter or soil
            SENWALG = 0.0
            SENNALG = 0.0
            SENCALG = 0.0
            SENLALG = 0.0
            SENWAGS = 0.0
            SENCAGS = 0.0
            SENLAGS = 0.0
            SENNAGS = 0.0
            SENWALG(0) = SENTOPLITTERG * PLTPOP*10.0
            SENCALG(0) = SENWALG(0) * 0.4 
            SENLALG(0) =
     &       (SENLFG*LLIGP/100) * PLTPOP*10.0
            SENNALG(0) = SENNLFG * SENFR * PLTPOP*10.0
            ! Root senescence
            DO L = 1, NLAYR
              SENWALG(L) = RTWTSL(L) * PLTPOP*10.0
              SENNALG(L) = RTNSL(L) * PLTPOP*10.0
              SENCALG(L) = SENWALG(L) * 0.4
              SENLALG(L) = SENWALG(L) * RLIGP/100.0
              SENWAGS = SENWAGS + SENWALG(L)
              SENCAGS = SENCAGS + SENCALG(L)
              SENLAGS = SENLAGS + SENLALG(L)
              SENNAGS = SENNAGS + SENNALG(L)
            ENDDO

            IF (ESTABLISHED.NE.'Y'.AND.SHRTD.GT.2.0) ESTABLISHED = 'Y'

!=======================================================================
          ENDIF  ! End of after germinated section
!=======================================================================

!=======================================================================
        ENDIF  ! End of after planted (rate) section
!=======================================================================

!***********************************************************************
      ELSEIF (DYNAMIC.EQ.INTEGR) THEN
!***********************************************************************

!=======================================================================
        IF (YEARDOY.GE.PLYEARDOY) THEN
!=======================================================================

!-----------------------------------------------------------------------
!         Update ages
!-----------------------------------------------------------------------

          IF (YEARDOY.GT.PLYEARDOY) THEN
            DAP = DAP + 1
            IF (EYEARDOY.GT.0) DAE = DAE + 1
          ENDIF
          TVI1 = LNUMSOLDESTA
          LNUMSOLDESTA = -99
          ! Leaves that present at beginning of day
          DO L = 1,LNUMSG  
            LAGETT(L) = LAGETT(L) + TTLFLIFE*EMRGFR
            ! Accelerated senescence at base of dense leaf canopy
            IF (LAI.GT.LAIXX) THEN
              IF (L.EQ.TVI1) THEN
                ! Increase age if deep shading at base of canopy
                ! (Maximum accelerated ageing set in SPE file)
                ! Accelerated ageing of lowermost active leaf
                IF (LAGETT(L).LT.LLIFGTT+LLIFATT) THEN
                  LAGETT(L) = AMIN1(LAGETT(L)+LLIFX,LLIFGTT+LLIFATT)
                  LLIFXUNUSED = (LAGETT(L)+LLIFX)-(LLIFGTT+LLIFATT)
                ELSE
                  LLIFXUNUSED = LLIFX
                ENDIF  
              ENDIF
              ! If not all acceleration used up
              IF (L.EQ.TVI1+1) THEN
                IF (LLIFXUNUSED.GT.0.0) THEN
                  IF (LAGETT(L).LT.LLIFGTT+LLIFATT) LAGETT(L) = 
     &             AMIN1(LAGETT(L)+LLIFXUNUSED,LLIFGTT+LLIFATT)
                ENDIF  
              ENDIF
            ENDIF
            LAGEP(L) = LAGEP(L) + (TTLFLIFE*EMRGFR)/PHINT
            ! Days growing
            IF (LAGETT(L).LE.LLIFGTT) THEN
              DGLF(L) = DGLF(L) + EMRGFR
             ELSE  
               IF (LAGETT(L)-TTLFLIFE*EMRGFR.LT.LLIFGTT) THEN
                TVR1 = 
     &          (LLIFGTT-(LAGETT(L)-TTLFLIFE*EMRGFR))/(TTLFLIFE*EMRGFR)
                 DGLF(L) = DGLF(L) + TVR1
               ENDIF  
            ENDIF
            ! Days active
            IF (LAGETT(L).GT.LLIFGTT.AND.
     &          LAGETT(L).LT.LLIFGTT+LLIFATT) THEN
              IF (LNUMSOLDESTA.LT.0) THEN
                LNUMSOLDESTA = L
              ENDIF  
              IF (LAGETT(L)-TTLFLIFE*EMRGFR.LT.LLIFGTT) THEN
                TVR1 = 
     &           (LLIFGTT-(LAGETT(L)-TTLFLIFE*EMRGFR))/(TTLFLIFE*EMRGFR)
                DALF(L) = DALF(L) + (1.0-TVR1)
              ELSE
                IF (LAGETT(L).LE.LLIFGTT+LLIFATT) THEN
                  DALF(L) = DALF(L) + 1.0
                ELSE  
                  IF (LAGETT(L)-TTLFLIFE*EMRGFR.LT.LLIFGTT+LLIFATT) THEN
                    TVR1 = 
     &              ((LLIFGTT+LLIFATT)-(LAGETT(L)-TTLFLIFE*EMRGFR))
     &              /(TTLFLIFE*EMRGFR)
                    DALF(L) = DALF(L) + TVR1
                  ENDIF
                ENDIF
              ENDIF
            ENDIF   
            ! Days senescing
            IF (LAGETT(L).GT.LLIFGTT+LLIFATT) THEN
              IF (LAGETT(L)-TTLFLIFE*EMRGFR.LT.LLIFGTT+LLIFATT) THEN
                TVR1 = ((LLIFGTT+LLIFATT)-(LAGETT(L)-TTLFLIFE*EMRGFR))
     &                 /(TTLFLIFE*EMRGFR)
                DSLF(L) = DSLF(L) + (1.0-TVR1)
              ELSE
                IF (LAGETT(L).LE.LLIFGTT+LLIFATT+LLIFSTT) THEN
                  DSLF(L) = DSLF(L) + 1.0
                ELSE  
                  IF (LAGETT(L)-TTLFLIFE*EMRGFR.LT.
     &             LLIFGTT+LLIFATT+LLIFSTT) THEN
                    TVR1 = 
     &              ((LLIFGTT+LLIFATT+LLIFSTT)
     &              -(LAGETT(L)-TTLFLIFE*EMRGFR))
     &              /(TTLFLIFE*EMRGFR)
                    DSLF(L) = DSLF(L) + TVR1
                    LDEATHDAP(L) = DAP  
                  ENDIF
                ENDIF
              ENDIF
            ENDIF   
          ENDDO
          ! Leaves that emerged during day
          IF (LNUMG.GT.0.0) THEN
            IF (LNUMSG.LT.LNUMX) THEN
              LAGETT(LNUMSG+1) = LAGETT(LNUMSG+1)+
     &         (TTLFLIFE*EMRGFR)*AMAX1(0.0,LNUMG-LNUMNEED)/LNUMG
              LAGEP(LNUMSG+1)=LAGEP(LNUMSG+1)+AMAX1(0.0,LNUMG-LNUMNEED)
              DGLF(LNUMSG+1) = AMAX1(0.0,LNUMG-LNUMNEED)/LNUMG
            ENDIF  
          ENDIF

!-----------------------------------------------------------------------
!         Update dry weights
!-----------------------------------------------------------------------

          ! Dry weights
          ! LAH No growth/development on planting day
          IF (YEARDOY.GE.PLYEARDOY) THEN   

            ! Assimilation and respiration
            CARBOC = CARBOC + AMAX1(0.0,(CARBOBEG+CARBOADJ))
            RESPRC = RESPRC + RTRESPADJ
            RESPTC = 0.0  ! Respiration tops - not yet used
            RESPC = RESPRC + RESPTC

            LFWT = LFWT + GROLFADJ - SENLFG - SENLFGRS - LWPH
            LWPHC = LWPHC +  LWPH

            IF (LFWT.LT.-1.0E-8) THEN
              WRITE(Message(1),'(A35,F4.1,A14)')
     &         'Leaf weight less than 0! Weight of ',lfwt,
     &         ' reset to zero'
              CALL WARNING(1,'CSCAS',MESSAGE)
              LFWT = 0.0
            ENDIF
            RSWT = RSWT+GRORS-RSWPH-SRWTGRS+RSWTGLFADJ
            RSWPHC = RSWPHC +  RSWPH
            ! Reserves distribution 
            ! Max concentration in leaves increases through life cycle.
            IF (PSTART(MSTG).GT.0.0) LLRSWT = AMIN1(RSWT,
     &       LFWT*(1.0-LPEFR)*(RSCLX/100.0)*DSTAGE)
            IF (PSTART(MSTG).GT.0.0) LPERSWT = AMIN1(RSWT-LLRSWT,
     &       LFWT*LPEFR*(RSCLX/100.0)*DSTAGE)
            IF (STWT+CRWT.GT.0.0) THEN
              STRSWT = (RSWT-LLRSWT-LPERSWT)*STWT/(STWT+CRWT)
              CRRSWT = (RSWT-LLRSWT-LPERSWT)*CRWT/(STWT+CRWT)
            ELSE
              STRSWT = (RSWT-LLRSWT-LPERSWT)
              CRRSWT = 0.0
            ENDIF

            IF (RSWT.LT.0.0) THEN
              IF (ABS(RSWT).GT.1.0E-6) THEN
                WRITE(Message(1),'(A30,A11,F12.9)')
     &           'Reserves weight reset to zero.',
     &           'Weight was ',rswt
                CALL WARNING(1,'CSCAS',MESSAGE)
                RSWT = 0.0
              ENDIF
            ENDIF

            RSWTX = AMAX1(RSWTX,RSWT)
            STWT = STWT + GROSTADJ - SWPH
            IF (STWT.LT.1.0E-06) THEN
!              IF (STWT.LT.0.0) 
!     &         WRITE(fnumwrk,*)'Stem weight less than 0! ',STWT
              STWT = 0.0
            ENDIF
            SWPHC = SWPHC +  SWPH
            CRWT = CRWT + GROCRADJ 
            SENTOPLITTER = SENTOPLITTER + SENTOPLITTERG
            SENCL(0) = SENCL(0) + SENTOPLITTERG*0.4
            SENLL(0) = SENLL(0)
     &       + (SENLFG*LLIGP/100)*(SENFR)
            RTWT = 0.0
            DO L = 1, NLAYR
              RTWTL(L) = RTWTL(L) + RTWTGL(L) - RTWTSL(L) - RTWTUL(L)
              SENWL(L) = SENWL(L) + RTWTSL(L)
              SENCL(L) = SENCL(L) + RTWTSL(L) * 0.4
              SENLL(L) = SENLL(L) + RTWTSL(L) * RLIGP/100.0
              ! Totals
              RTWT = RTWT + RTWTL(L)
              SENROOT = SENROOT + RTWTSL(L)
              SENCS = SENCS + RTWTSL(L) * 0.4
              SENLS = SENLS + RTWTSL(L) * RLIGP/100.0
            END DO
            SRWT = SRWT + GROSR + SRWTGRS
     &           + (RTWTG-RTWTGADJ+RTRESP-RTRESPADJ) ! Root N adjustment
          ENDIF

          SEEDRS = AMAX1(0.0,SEEDRS-GROLSSD-SEEDRSAVR)
          IF (CFLSDRSMSG.NE.'Y'.AND.SEEDRS.LE.0.0.AND.LNUM.LT.4.0) THEN
            WRITE(Message(1),'(A44,F3.1)')
     &      'Seed reserves all used but leaf number only ',lnum
            WRITE(Message(2),'(A58)')
     &      'For good establishment seed reserves should last to leaf 4'
            WRITE(Message(3),'(A55)')
     &      'Maybe stick too small or specific leaf area set too low'
            CALL WARNING(3,'CSCAS',MESSAGE)
            CFLSDRSMSG = 'Y'
          ENDIF
          SEEDUSE = SEEDUSE + GROLSSD+SEEDRSAVR
          SEEDUSER = SEEDUSER + SEEDRSAVR
          SEEDUSET = SEEDUSET + GROLSSD
          SEEDRSAV = SEEDRS
          IF (SRNOPD.GT.0.0) SRWUD = SRWT/SRNOPD

          IF ((LFWT+STWT+CRWT+RSWT).GT.0.0) THEN
            HIAD = SRWT/(LFWT+STWT+CRWT+SRWT+RSWT)
          ENDIF
          IF (RTWT.GT.0.0)
     &     SHRTD = (LFWT+STWT+CRWT+RSWT) / RTWT

!-----------------------------------------------------------------------
!         Calculate reserve concentration
!-----------------------------------------------------------------------

          IF (LFWT+STWT+CRWT.GT.0.0) RSCD = RSWT/(LFWT+STWT+CRWT+RSWT)
          IF (RSCD.LT.0.0.AND.RSCD.GT.-1.0E-7) RSCD = 0.0
          RSCX = AMAX1(RSCX,RSCD)

!-----------------------------------------------------------------------
!         Update shoot leaf area (Must be done before PLA updated)
!-----------------------------------------------------------------------

          ! First for leaf senescence
          DO L = 1,INT(SHNUM+1)
            IF (SHNUM-FLOAT(L-1).GT.0.0) THEN
              IF (PLA-SENLA.GT.0.0) SHLAS(L) = SHLAS(L) +
     &             PLAS*(SHLA(L)-SHLAS(L))/(PLA-SENLA)
            ENDIF
          ENDDO

!-----------------------------------------------------------------------
!         Update produced leaf area
!-----------------------------------------------------------------------

          IF (TT*EMRGFR.GT.0.0) THEN
            PLA = PLA + PLAGSB4
            PLAX = AMAX1(PLAX,PLA)
            LAP(LNUMSG) = LAP(LNUMSG) + PLAGSB4
     
            DO L = 1,INT(SHNUM+1)
              IF (SHNUM.GE.1.0.OR.SHNUM-FLOAT(L-1).GT.0.0) THEN
                SHLA(L) = SHLA(L) + SHLAGB4(L)
              ENDIF
            ENDDO

            IF (LCNUM.LT.LCNUMX) THEN
              IF (PLAGSB4.GT.0.0) THEN
                LCNUM = LCNUM+1
                LCOA(LCNUM) = PLAGSB4
              ENDIF
            ELSE
              LCOA(LCNUM) = LCOA(LCNUM) + PLAGSB4
            ENDIF

          ENDIF

!-----------------------------------------------------------------------
!         Update senesced and harvested leaf area
!-----------------------------------------------------------------------

          SENLA = SENLA + PLAS
          SENLALITTER = SENLALITTER + PLAS * SENFR
          LAPHC = LAPHC + LAPH  ! Grazed leaf area
          ! Distribute senesced leaf over leaf positions and cohorts
          PLASTMP = PLAS - PLASP
          IF (LNUMSG.GT.0 .AND. PLASTMP.GT.0) THEN
            DO L = 1, LNUMSG
              IF (LAP(L)-LAPS(L).GT.PLASTMP) THEN
                LAPS(L) = LAPS(L) + PLASTMP
                PLASTMP = 0.0
              ELSE
                PLASTMP = PLASTMP - (LAP(L)-LAPS(L))
                LAPS(L) = LAP(L)
              ENDIF
              IF (PLASTMP.LE.0.0) EXIT
            ENDDO
            ! Cohorts
            PLASTMP2 = AMAX1(0.0,PLAS)
            DO L = 1, LCNUM
              IF (LCOA(L)-LCOAS(L).GT.PLASTMP2) THEN
                LCOAS(L) = LCOAS(L) + PLASTMP2
                PLASTMP2 = 0.0
              ELSE
                PLASTMP2 = PLASTMP2 - (LCOA(L)-LCOAS(L))
                LCOAS(L) = LCOA(L)
              ENDIF
              IF (PLASTMP2.LE.0.0) EXIT
            ENDDO
          ENDIF
          ! Distribute harvested leaf over leaf positions and cohorts
          ! Leaf positions
	    IF (LNUMSG.GT.0 .AND. LAPH.GT.0) THEN
            DO L = 1, LNUMSG
		      IF (LAP(L)-LAPS(L).GT.0.0)
     &	       LAPS(L) = LAPS(L) + (LAP(L)-LAPS(L)) * HAFR
		    ENDDO
            ! Cohorts
            DO L = 1, LCNUM
              IF (LCOA(L)-LCOAS(L).GT.0.0) THEN
                LCOAS(L) = LCOAS(L) + (LCOA(L)-LCOAS(L)) * HAFR
              ENDIF
            ENDDO
          ENDIF

!-----------------------------------------------------------------------
!         Update (green) leaf area                            
!-----------------------------------------------------------------------

          LAPD = AMAX1(0.0,(PLA-SENLA-LAPHC))
          LAI = AMAX1(0.0,(PLA-SENLA-LAPHC)*PLTPOP*0.0001)
          LAIX = AMAX1(LAIX,LAI)

!-----------------------------------------------------------------------
!         Update specific leaf area
!-----------------------------------------------------------------------

          SLA = -99.0
          IF (LFWT.GT.1.0E-6) SLA=(PLA-SENLA-LAPHC) / (LFWT*(1.0-LPEFR))
            
!-----------------------------------------------------------------------
!         Update leaf petiole and stem area
!-----------------------------------------------------------------------

          LPEAI = (LFWT*LPEFR*LPEAW)*PLTPOP*0.0001
          STAI = STAI + STAIG - STAIS

          SAID = STAI+LPEAI
          CAID = LAI + SAID

!-----------------------------------------------------------------------
!         Update height
!-----------------------------------------------------------------------

          CANHT = CANHT + CANHTG

!-----------------------------------------------------------------------
!         Update root depth and length
!-----------------------------------------------------------------------

          IF (SDEPTH.GT.0.0 .AND.RTDEP.LE.0.0) RTDEP = SDEPTH
          RTDEP = AMIN1 (RTDEP+RTDEPG,DEPMAX)
          DO L = 1, NLAYR
            RLV(L)=RTWTL(L)*RLWR*PLTPOP/DLAYR(L)   ! cm/cm3
            IF (L.EQ.NLAYR.AND.RLV(L).GT.0.0)THEN
              IF (RTSLXDATE.LE.0.0) RTSLXDATE = YEARDOY
            ENDIF
          END DO

!-----------------------------------------------------------------------
!         Update nitrogen amounts
!-----------------------------------------------------------------------
          NUPC = NUPC + NUPD
          LEAFNEXCESS = 0.0
          IF (LANC.GT.LNCX)
     &      LEAFNEXCESS = (LFWT-SENLFG-SENLFGRS)*(LANC-LNCX)
          LEAFN = LEAFN + GROLSRTN + LNUSE(0)
     &          - SENNLFG - SENNLFGRS - lnph
     &          - LEAFNEXCESS
         LNPHC = LNPHC +  LNPH
          IF (LEAFN.LT.1.0E-10) LEAFN = 0.0
          STEMNEXCESS = 0.0
          IF (SANC.GT.SNCX)
     &      STEMNEXCESS = (STWT+CRWT)*(SANC-SNCX)
          STEMN = STEMN + SNUSE(0)
     &          - SNPH - STEMNEXCESS
          SNPHC = SNPHC +  SNPH
          IF (STEMN.LT.1.0E-10) STEMN = 0.0
          ROOTNS = 0.0
          SENNGS = 0.0
          DO L = 1, NLAYR
            SENNL(L) = SENNL(L) + RTNSL(L)
            ROOTNS = ROOTNS + RTNSL(L)
            SENNS = SENNS + RTNSL(L)
            SENNGS = SENNGS + RTNSL(L)
          END DO
          ! LAH Maybe also need LEAFNEXESS if LANC > LNCX
          ROOTNEXCESS = 0.0
          IF (RANC.GT.RNCX)
     &      ROOTNEXCESS = (RTWT-(SENWALG(L)/(PLTPOP*10.0)))*(RANC-RNCX)
          ROOTN = ROOTN + (RNUSE(0)-ROOTNS-GROLSRTN)
     &                  - ROOTNEXCESS
          SROOTN = SROOTN + SRNUSE(0)
          SEEDN = SEEDN - SEEDNUSE - SEEDNUSE2
          IF (SEEDN.LT.1.0E-6) SEEDN = 0.0
          RSN = RSN - RSNUSED
     &        + SENNLFGRS - RSNPH
     &        + LEAFNEXCESS + STEMNEXCESS + ROOTNEXCESS

          RSNPHC = RSNPHC +  RSNPH
          SENNL(0) = SENNL(0) + SENNLFG

          HPRODN = SROOTN

          ! Harvest index for N
          HIND = 0.0
          IF ((LEAFN+STEMN+RSN).GT.0.0)
     &     HIND = HPRODN/(LEAFN+STEMN+HPRODN+RSN)

!-----------------------------------------------------------------------
!         Update stages
!-----------------------------------------------------------------------

          ! STAGES:Germination and emergence (Gestages)
          ! NB 0.5 factor used to equate to Zadoks)
          GEUCUM = GEUCUM + TTGEM*WFGE
          IF (GEUCUM.LT.PEGD) THEN
            GESTAGE = AMIN1(1.0,GEUCUM/PEGD*0.5)
          ELSE
            IF (PECM*SDEPTHU > 1.E-6) THEN 
              GESTAGE = AMIN1(1.0,0.5+0.5*(GEUCUM-PEGD)/(PECM*SDEPTHU))
            ELSE
              GESTAGE = 1.0
            ENDIF    
          ENDIF
          
          ! Germination conditions  
          IF (GESTAGEPREV.LT.0.5) THEN
            TMEANGC = TMEANGC + TMEAN
            GEDAYSG = GEDAYSG + 1
            TMEANG = TMEANGC/GEDAYSG
          ENDIF  

          ! Germination to emergence conditions  
          IF (GESTAGE.LT.0.5) THEN
            GOTO 6666                 
          ELSEIF (GESTAGEPREV.LT.1.0) THEN
            TMEANEC = TMEANEC + TMEAN
            GEDAYSE = GEDAYSE + 1
            TMEANE = TMEANEC/GEDAYSE
          ENDIF

          ! STAGES:Overall development
          CUMDU = CUMDU + DU
          IF (MEDEV.EQ.'DEVU'.AND.PSTART(MSTG).GT.0.0) THEN
            ! Calculate dstage from developmental unit accumulation
            IF (PSTART(MSTG).GT.0.0) DSTAGE = CUMDU/PSTART(MSTG)
          ELSE
            ! Alternative method.Calculate dstage from leaf number
            IF (LNUMTOSTG(MSTG).GT.0.0) DSTAGE = LNUM/LNUMTOSTG(MSTG)
            IF (LAFND.GT.0.0) DSTAGE = LNUM/LAFND
          ENDIF 

          ! STAGES:Branching
          IF (GESTAGE.GE.0.5) THEN
            IF (MEDEV.EQ.'DEVU') THEN
              DO L = HSTG,1,-1
                IF (CUMDU.GE.PSTART(L).AND.PD(L).GT.0.0) THEN
                  BRSTAGE = FLOAT(L) + (CUMDU-PSTART(L))/PD(L)
                  ! Brstage cannot go above harvest stage 
                  BRSTAGE = AMIN1(FLOAT(HSTG),BRSTAGE)
                  LNUMSIMTOSTG(L+1) = LNUM  ! To record simulated # 
                  EXIT
                ENDIF
              ENDDO
            ELSEIF (MEDEV.EQ.'LNUM') THEN 
              ! Alternative method based on leaf numbers 
              DO L = HSTG,0,-1
                IF (LNUM.GE.LNUMTOSTG(L)) THEN
                  IF (PDL(L).GT.0)
     &            BRSTAGE = FLOAT(L) + (LNUM-LNUMTOSTG(L))/PDL(L)
                  LNUMSIMTOSTG(L+1) = LNUM  ! To record simulated # 
                  ! Brstage cannot go above harvest stage 
                  BRSTAGE = AMIN1(FLOAT(HSTG),BRSTAGE)
                  EXIT
                ENDIF  
              ENDDO
            ENDIF  
          ENDIF

          ! STAGES:leaf numbers 
          LNUM = AMAX1(0.0,(AMIN1(FLOAT(LNUMX-1),(LNUM+LNUMG))))
          LNUMSG = INT(LNUM)+1  ! Youngest growing leaf

!-----------------------------------------------------------------------
!         Record stage dates and states
!-----------------------------------------------------------------------

          IF (INT(BRSTAGE).GT.10 .OR.
     &      INT(BRSTAGE).LT.0.AND.GESTAGE.GT.0.5) THEN
            OPEN (UNIT = FNUMERR,FILE = 'ERROR.OUT')
            WRITE(fnumerr,*) ' '
            WRITE(fnumerr,*)
     &       'Brstage out of range allowed for branching        '
            WRITE(fnumerr,*) 'Brstage was: ',brstage
            WRITE(fnumerr,*) 'Please contact model developer'
            WRITE(*,*)
     &       ' Brstage out of range allowed       '
            WRITE(*,*) ' Brstage was: ',brstage
            WRITE(*,*) ' Program will have to stop'
            PAUSE
            CLOSE (fnumerr)
            STOP ' '
          ENDIF
          ! NB. Status at start of branch tier
          IF (INT(BRSTAGE).GT.INT(BRSTAGEPREV).AND.
     &     STGYEARDOY(INT(BRSTAGE)).GE.9999999) THEN
              STGYEARDOY(INT(BRSTAGE)) = YEARDOY
              LAISTG(INT(BRSTAGE)) = LAIPREV 
              LNUMSTG(INT(BRSTAGE)) = LNUMPREV
              ! CWAD and CNAD not include the seed
              CWADSTG(INT(BRSTAGE)) = CWADPREV
              CNADSTG(INT(BRSTAGE)) = CNADPREV
          ENDIF

          ! Primary stages.   Calculated using Pstart
          IF (BRSTAGEPREV.LT.0.0) BRSTAGEPREV = 0.0
          L = INT(BRSTAGEPREV) + 1
          IF (PSDAT(L).LE.0.0.AND.CUMDU.GE.PSTART(L)) THEN
            PSDAT(L) = YEARDOY
            IF (DU.GT.0.0) PSDAPFR(L)=(PSTART(L)-(CUMDU-DU))/DU
            PSDAPFR(L) = FLOAT(DAP) + PSDAPFR(L)
            PSDAP(L) = DAP
            IF (PSABV(L).EQ.'MDAT '.OR.L.EQ.MSTG) THEN
              MDAT = YEARDOY
              MDOY = DOY
              MDAP = DAP
              MDAYFR = TIMENEED
              MDAPFR = FLOAT(MDAP) + MDAYFR
            ENDIF
          ENDIF

          IF (GYEARDOY.LE.0.0.AND.GERMFR.GT.0.0) THEN
            GYEARDOY = PLYEARDOY
            GDAP = DAP
            GDAYFR = 1.0 - GERMFR
            GDAPFR = FLOAT(DAP) + GDAYFR
          ENDIF

          IF (EYEARDOY.LE.0.0.AND.EMRGFR.GT.0.0) THEN
            EYEARDOY = YEARDOY
            EDAP = DAP
            EDAYFR = 1.0 - EMRGFR
            EDAPFR = FLOAT(DAP) + EDAYFR
            DAE = 0
          ENDIF

          IF (SHDAT.LE.0.0.AND.SHNUM.GT.1.0) THEN
            SHDAT = YEARDOY
            SHDAP = FLOAT(DAP)
          ENDIF

!-----------------------------------------------------------------------
!         Calculate branch interval and crop cycle conditions
!-----------------------------------------------------------------------

          IF (WFG.LT.0.99999) WSDAYS = WSDAYS + 1
          IF (NFG.LT.0.99999) NSDAYS = NSDAYS + 1

          IF (GESTAGE.GT.0.1) THEN
            IF (INT(BRSTAGE).NE.INT(BRSTAGEPREV)) THEN
              TMAXPC = 0.0
              TMINPC = 0.0
              TMEANPC = 0.0
              SRADPC = 0.0
              DAYLPC = 0.0
              NFPPC = 0.0
              NFGPC = 0.0
              WFPPC = 0.0
              WFGPC = 0.0
              CO2PC = 0.0
            ENDIF
            TMAXPC = TMAXPC + TMAX
            TMINPC = TMINPC + TMIN
            TMEANPC = TMEANPC + TMEAN
            SRADPC = SRADPC + SRAD
            DAYLPC = DAYLPC + DAYL
            TMAXCC = TMAXCC + TMAX
            TMINCC = TMINCC + TMIN
            TMEANCC = TMEANCC + TMEAN
            SRADCC = SRADCC + SRAD
            CO2CC = CO2CC + CO2
            DAYLCC = DAYLCC + DAYL
            RAINCC = RAINCC + RAIN
            
            RAINPC(INT(BRSTAGE)) = RAINPC(INT(BRSTAGE)) + RAIN
            ETPC(INT(BRSTAGE))   = ETPC(INT(BRSTAGE)) + ET 
            EPPC(INT(BRSTAGE))   = EPPC(INT(BRSTAGE)) + EP 
            
            CO2PC = CO2PC + CO2
            NFPPC = NFPPC + NFP
            NFGPC = NFGPC + NFG
            WFPPC = WFPPC + WFP
            WFGPC = WFGPC + WFG
            NFPCC = NFPCC + NFP
            NFGCC = NFGCC + NFG
            WFPCC = WFPCC + WFP
            
            WFGCC = WFGCC + WFG
            ETCC   = ETCC + ET
            EPCC   = EPCC + EP 
            
            PDAYS(INT(BRSTAGE)) = PDAYS(INT(BRSTAGE)) + 1
            CDAYS = CDAYS + 1
            IF (PDAYS(INT(BRSTAGE)).GT.0) THEN
              TMAXPAV(INT(BRSTAGE)) = TMAXPC / PDAYS(INT(BRSTAGE))
              TMINPAV(INT(BRSTAGE)) = TMINPC / PDAYS(INT(BRSTAGE))
              TMEANAV(INT(BRSTAGE)) = TMEANPC / PDAYS(INT(BRSTAGE))
              SRADPAV(INT(BRSTAGE)) = SRADPC / PDAYS(INT(BRSTAGE))
              DAYLPAV(INT(BRSTAGE)) = DAYLPC / PDAYS(INT(BRSTAGE))
              DAYLST(INT(BRSTAGE)) = DAYL
              CO2PAV(INT(BRSTAGE)) = CO2PC / PDAYS(INT(BRSTAGE))
              RAINPAV(INT(BRSTAGE)) = 
     &         RAINPC(INT(BRSTAGE)) / PDAYS(INT(BRSTAGE))
              NFPPAV(INT(BRSTAGE)) = NFPPC / PDAYS(INT(BRSTAGE))
              NFGPAV(INT(BRSTAGE)) = NFGPC / PDAYS(INT(BRSTAGE))
              WFPPAV(INT(BRSTAGE)) = WFPPC / PDAYS(INT(BRSTAGE))
              WFGPAV(INT(BRSTAGE)) = WFGPC / PDAYS(INT(BRSTAGE))
              ENDIF
            IF (CDAYS.GT.0) THEN              
              TMAXCAV = TMAXCC / CDAYS
              TMINCAV = TMINCC / CDAYS 
              SRADCAV = SRADCC / CDAYS
              DAYLCAV = DAYLCC / CDAYS
              CO2CAV = CO2CC / CDAYS
              NFPCAV = NFPCC / CDAYS
              NFGCAV = NFGCC / CDAYS
              WFPCAV = WFPCC / CDAYS
              WFGCAV = WFGCC / CDAYS
            ENDIF
          ENDIF

!-----------------------------------------------------------------------
!         Calculate nitrogen concentrations
!-----------------------------------------------------------------------

          IF (ISWNIT.NE.'N') THEN
            ! Critical and minimum N concentrations
              LNCX = LNCXS(0) + DSTAGE*(LNCXS(1)-LNCXS(0))
              SNCX = SNCXS(0) + DSTAGE*(SNCXS(1)-SNCXS(0))
              RNCX = RNCXS(0) + DSTAGE*(RNCXS(1)-RNCXS(0))
              LNCM = LNCMN(0) + DSTAGE*(LNCMN(1)-LNCMN(0))
              SNCM = SNCMN(0) + DSTAGE*(SNCMN(1)-SNCMN(0))
              RNCM = RNCMN(0) + DSTAGE*(RNCMN(1)-RNCMN(0))

            ! N concentrations
            RANC = 0.0
            LANC = 0.0
            SANC = 0.0
            VANC = 0.0
            VCNC = 0.0
            VMNC = 0.0
            IF (RTWT.GT.1.0E-5) RANC = ROOTN / RTWT
            IF (LFWT.GT.1.0E-5) LANC = LEAFN / LFWT
            IF (STWT+CRWT.GT.1.0E-5) SANC = STEMN / (STWT+CRWT)
            IF (VWAD.GT.0.0) VANC = VNAD/VWAD
            IF (LANC.LT.0.0) THEN 
              WRITE(Message(1),'(A27,F4.1)')
     &         'LANC below 0 with value of ',LANC
              WRITE(Message(2),'(A27,2F5.1)')
     &         'LEAFN,LFWT had values of   ',LEAFN,LFWT
              CALL WARNING(2,'CSCAS',MESSAGE)
              LANC = AMAX1(0.0,LANC)
            ENDIF
            IF (LFWT+STWT+CRWT.GT.0.0) VCNC = 
     &      (LNCX*AMAX1(0.0,LFWT)+SNCX*AMAX1(0.0,STWT+CRWT))/
     &      (AMAX1(0.0,LFWT)+AMAX1(0.0,STWT+CRWT))
            IF (LFWT+STWT+CRWT.GT.0.0) VMNC = 
     &      (LNCM*AMAX1(0.0,LFWT)+SNCM*AMAX1(0.0,STWT+CRWT))/
     &      (AMAX1(0.0,LFWT)+AMAX1(0.0,STWT+CRWT))

            SDNC = 0.0
            SRANC = 0.0
            IF (SEEDRS.GT.0.0) SDNC = SEEDN/(SEEDRS+SDCOAT)
            IF (SRWT.GT.0) SRANC = SROOTN/SRWT
            LNCR = 0.0
            SNCR = 0.0
            RNCR = 0.0
            IF (LNCX.GT.0.0) LNCR = AMAX1(0.0,AMIN1(1.0,LANC/LNCX))
            IF (SNCX.GT.0.0) SNCR = AMAX1(0.0,AMIN1(1.0,SANC/SNCX))
            IF (RNCX.GT.0.0) RNCR = AMAX1(0.0,AMIN1(1.0,RANC/RNCX))
          ELSE
            LNCR = 1.0
            SNCR = 1.0
            RNCR = 1.0
          ENDIF
          
 6666     CONTINUE  ! Jump to here if germinating

!-----------------------------------------------------------------------
!         Determine if to harvest or fail
!-----------------------------------------------------------------------

          ! Harvesting conditions
          IF (IHARI.EQ.'A' .AND. CUMDU.GE.PSTART(MSTG)) THEN
            ! Here need to check out if possible to harvest.
            IF (YEARDOY.GE.HFIRST) THEN
              IF (SW(1).GE.SWPLTL.AND.SW(1).LE.SWPLTH) 
     &         YEARDOYHARF=YEARDOY
            ENDIF
            ! Check if past earliest date; check if not past latest date
            ! Check soil water
            ! If conditions met set YEARDOYHARF = YEARDOY
            ! (Change YEARDOYHARF to more something more appropriate)
          ENDIF

          ! Determine if crop failure
          IF (DAP.GE.90 .AND. GESTAGE.LT.1.0) THEN
            CFLFAIL = 'Y'
            WRITE (Message(1),'(A40)')
     &       'No germination within 90 days of sowing '
             CALL WARNING(1,'CSCAS',MESSAGE)
          ENDIF
          IF (IHARI.NE.'A'.AND.MDAT.GE.0.AND.DAP-MDAP.GE.300) THEN
            CFLFAIL = 'Y'
            WRITE (Message(1),'(A32)')'300 days after maturity         '
            WRITE (Message(2),'(A21)')'Harvesting triggered.'
            CALL WARNING(2,'CSCAS',MESSAGE)
          ENDIF
          IF (IHARI.NE.'A'.AND.CUMDU.GE.PSTART(MSTG-1)) THEN
            ! NB. Not work if MSTG=2
            IF (TT20.LE.-98.0.AND.PSTART(MSTG-1).GT.0.0) THEN
              CFLFAIL = 'Y'
              WRITE (Message(1),'(A28)') '20day thermal time mean = 0 '
              CALL WARNING(1,'CSCAS',MESSAGE)
           ENDIF
          ENDIF

          ! Determine if to harvest
          CFLHAR = 'N'
          IF (IHARI.EQ.'R'.AND.YEARDOYHARF.EQ.YEARDOY .OR.
     &     IHARI.EQ.'D'.AND.YEARDOYHARF.EQ.DAP .OR.
     &     IHARI.EQ.'G'.AND.YEARDOYHARF.LE.BRSTAGE .OR.
     &     IHARI.EQ.'A'.AND.YEARDOYHARF.EQ.YEARDOY .OR.
     &     IHARI.EQ.'M'.AND.CUMDU.GE.PSTART(MSTG)) THEN
            CFLHAR = 'Y'
          ENDIF    

          IF(IHARI.EQ.'R'.AND.CFLHAR.EQ.'N')THEN
            IF (CUMDU.GT.PSTART(MSTG) .AND. CFLHARMSG .NE. 'Y') THEN
              WRITE(Message(1),'(A54,I7)')
     &        'Maturity reached but waiting for reported harvest on: ',
     &        YEARDOYHARF 
              CALL WARNING(1,'CSCAS',MESSAGE)
              CFLHARMSG = 'Y'
            ENDIF
          ENDIF
     
          IF (CFLFAIL.EQ.'Y' .OR. CFLHAR.EQ.'Y') THEN
          
            IF (CFLFAIL.EQ.'Y'
     &            .AND. BRSTAGE <= 12 .AND. BRSTAGE > 0 ) THEN       
              STGYEARDOY(12) = YEARDOY
              TMAXPAV(12) = TMAXPAV(INT(BRSTAGE))
              TMINPAV(12) = TMINPAV(INT(BRSTAGE))
              SRADPAV(12) = SRADPAV(INT(BRSTAGE))
              DAYLPAV(12) = DAYLPAV(INT(BRSTAGE))
              RAINPAV(12) = RAINPAV(INT(BRSTAGE))
              CO2PAV(12) = CO2PAV(INT(BRSTAGE))
              NFPPAV(12) = NFPPAV(INT(BRSTAGE))
              WFPPAV(12) = WFPPAV(INT(BRSTAGE))
              WFGPAV(12) = WFGPAV(INT(BRSTAGE))
              NFGPAV(12) = NFGPAV(INT(BRSTAGE))
            ENDIF
            STGYEARDOY(10) = YEARDOY  ! Harvest
            STGYEARDOY(11) = YEARDOY  ! Crop End
c           IF (HSTG.GT.0) THEN
c             PSDAPFR(HSTG) = FLOAT(DAP)
c             PSDAP(HSTG) = DAP
c           ENDIF  
c           IF (MSTG.GT.0.AND.PSDAPFR(MSTG).LE.0.0) THEN
c             PSDAPFR(MSTG) = FLOAT(DAP)
c             PSDAP(MSTG) = DAP
c           ENDIF  
            HADOY = DOY
            HAYEAR = YEAR
            CWADSTG(INT(10)) = CWAD
            LAISTG(INT(10)) = LAI
            LNUMSTG(INT(10)) = LNUM
            CNADSTG(INT(10)) = CNAD
            IF (MDAYFR.LT.0.0) THEN
              IF (CFLFAIL.EQ.'Y') THEN
                WRITE(Message(1),'(A26)')
     &           'Harvest/failure triggered '                 
                CALL WARNING(1,'CSCAS',MESSAGE)
              ENDIF  
            ENDIF
          ENDIF

!-----------------------------------------------------------------------
!         Calculate season end soil conditions                              
!-----------------------------------------------------------------------

          FSOILN = 0.0
          FSOILH2O = 0.0
          DO I = 1, NLAYR
            FSOILN = FSOILN + NO3LEFT(I)/(10.0/(BD(I)*(DLAYR(I))))
     &                          + NH4LEFT(I)/(10.0/(BD(I)*(DLAYR(I))))
            FSOILH2O = FSOILH2O + SW(I)*DLAYR(I)
          ENDDO

!-----------------------------------------------------------------------
!         Calculate variables that are generally measured and presented
!-----------------------------------------------------------------------

          ! Here,reserves are included in leaf,stem,and crown weights
          ! And weights are in kg/ha
          CWAD = (LFWT+STWT+CRWT+RSWT)*PLTPOP*10.0
          SRWAD = SRWT*PLTPOP*10.0
          LLWAD = LFWT*(1.0-LPEFR)*10.0*PLTPOP
          LPEWAD = LFWT*LPEFR*10.0*PLTPOP
          RWAD = RTWT*PLTPOP*10.0
          SDWAD = (SEEDRS+SDCOAT)*10.0*PLTPOP
          ! Leaf petioles NOT included in stem here
          STWAD = STWT*10.0*PLTPOP
          CRWAD = CRWT*PLTPOP*10.0
          RSWAD = RSWT*PLTPOP*10.0
          LLRSWAD = LLRSWT*PLTPOP*10.0
          LPERSWAD = LPERSWT*PLTPOP*10.0
          STRSWAD = STRSWT*PLTPOP*10.0
          CRRSWAD = CRRSWT*PLTPOP*10.0

          ! Need to CHECK these
          SENROOTA = SENROOT*10.0*PLTPOP
          SENCAS = SENCS*10.0*PLTPOP
          SENLAS = SENLS*10.0*PLTPOP
          SENTOPLITTERA = SENTOPLITTER*PLTPOP*10.0
          DO L =1,NLAYR
            RTWTAL(L) = RTWTL(L)*PLTPOP*10.0
            SENWAL(L) = SENWL(L)*PLTPOP*10.0
          ENDDO

          TWAD = 
     &      (SEEDRS+SDCOAT+RTWT+LFWT+STWT+CRWT+SRWT+RSWT)
     &         * PLTPOP*10.0

          VWAD = (LFWT+STWT+CRWT+RSWT)*PLTPOP * 10.0

          SHNUMAD = SHNUM*PLTPOP

          IF (NUPAC.LT.0.0) THEN
            NUPAC = NUPAD
          ELSE 
            NUPAC = NUPAC+NUPAD
          ENDIF  
          CNAD = (LEAFN+STEMN+RSN)*PLTPOP*10.0
          SRNAD = SROOTN*PLTPOP*10.0
          LLNAD = LEAFN*(1.0-LPEFR)*PLTPOP*10.0
          RNAD = ROOTN*PLTPOP*10.0
          RSNAD = RSN*PLTPOP*10.0
          SDNAD = SEEDN*PLTPOP*10.0
          SNAD = STEMN*PLTPOP*10.0
          TNAD = (ROOTN+LEAFN+STEMN+RSN+HPRODN+SEEDN)*PLTPOP*10.0
          VNAD = (LEAFN+STEMN+RSN)*PLTPOP*10.0
          
          ! LAH Note that no reserves included in sancout
          ! SANCOUT = SNAD/(STWAD+STRSWAD + LPEWAD+LPERSWAD)  ! With rs
          IF (STWAD.GT.1.0E-5) SANCOUT = SNAD/STWAD

          HWAD = SRWAD
          HWUD = SRWUD
          HNUMAD = SRNOPD * PLTPOP
          HNAD = SRNAD
          HNC = SRANC

          SENNAS = SENNS*10.0*PLTPOP
          SENNAL(0) = SENNL(0)*PLTPOP*10.0
          SENNATC = SENNAL(0)+SENNAS
          DO L =1,NLAYR
            SENNAL(L) = SENNL(L)*PLTPOP*10.0
          ENDDO

          ! After harvest residues
          IF (STGYEARDOY(11).EQ.YEARDOY) THEN
            ! Surface
            RESWALG(0) = VWAD*(1.0-HBPCF/100.0)
            RESNALG(0) = (LEAFN+STEMN)*PLTPOP*10.*(1.0-HBPCF/100.)
            RESCALG(0) = RESWALG(0) * 0.4
            RESLGALG(0) = LLWAD*LLIGP/100.0*(1.0-HBPCF/100.0)
     &                  + LPEWAD*SLIGP/100.0*(1.0-HBPCF/100.0)
     &                  + STWAD*SLIGP/100.0*(1.0-HBPCF/100.0)
            ! Soil
            DO L = 1, NLAYR
              RESWALG(L) = RTWTL(L)*PLTPOP*10.0
              RESNALG(L) = RTWTL(L)*PLTPOP*10.0 * RANC
              RESCALG(L) = RTWTL(L)*PLTPOP*10.0 * 0.4
              RESLGALG(L) = RTWTL(L)*PLTPOP*10.0 * RLIGP/100.0
            ENDDO

            ! Surface
            RESWAL(0) = RESWAL(0) + RESWALG(0)
            RESNAL(0) = RESNAL(0) + RESNALG(0)
            RESCAL(0) = RESCAL(0) + RESCALG(0)
            RESLGAL(0) = RESLGAL(0) + RESLGALG(0)
            ! Soil
            DO L = 1, NLAYR
              RESWAL(L) = RESWAL(L) + RESWALG(L)
              RESNAL(L) = RESNAL(L) + RESNALG(L)
              RESCAL(L) = RESCAL(L) + RESCALG(L)
              RESLGAL(L) = RESLGAL(L) + RESLGALG(L)
            ENDDO
          ENDIF

!-----------------------------------------------------------------------
!         Calculate weather and soil summary variables
!-----------------------------------------------------------------------

          ! Cumulatives
          TTCUM = TTCUM + TT
          RAINC = RAINC + RAIN
          DRAINC = DRAINC + DRAIN
          RUNOFFC = RUNOFFC + RUNOFF
          IRRAMTC = IRRAMTC + IRRAMT
          SRADC = SRADC + SRAD
          PARMJC = PARMJC + PARMJFAC*SRAD
          PARMJIC = PARMJIC + PARMJFAC*SRAD*PARI + PARMJIADJ
          TOMINC = TOMINC + TOMIN
          TOFIXC = TOFIXC + TNIMBSOM
          TOMINFOMC = TOMINFOMC + TOMINFOM
          TOMINSOMC = TOMINSOMC + TOMINSOM
          IF (TOMINSOM1.GE.0.0) THEN
            TOMINSOM1C = TOMINSOM1C + TOMINSOM1
            TOMINSOM2C = TOMINSOM2C + TOMINSOM2
            TOMINSOM3C = TOMINSOM3C + TOMINSOM3
          ELSE
            TOMINSOM1C = -99.0
            TOMINSOM2C = -99.0
            TOMINSOM3C = -99.0
          ENDIF  
          TLCHC = TLCHC + TLCHD
          TNOXC = TNOXC + TNOXD

          ! Extremes
          TMAXX = AMAX1(TMAXX,TMAX)
          TMINN = AMIN1(TMINN,TMIN)
          CO2MAX = AMAX1(CO2MAX,CO2)

          ! Growing season means
          TMEANNUM = TMEANNUM + 1
          TMEANSUM = TMEANSUM + TMEAN

          ! 20-day means
          SRAD20S = 0.0
          TMEAN20S = 0.0
          STRESS20S = 0.0
          STRESS20NS = 0.0
          STRESS20WS = 0.0
          TT20S = 0.0
          DO L = 20,2,-1
            SRADD(L) = SRADD(L-1)
            SRAD20S = SRAD20S + SRADD(L)
            TMEAND(L) = TMEAND(L-1)
            TMEAN20S = TMEAN20S + TMEAND(L)
            STRESS(L) = STRESS(L-1)
            STRESSN(L) = STRESSN(L-1)
            STRESSW(L) = STRESSW(L-1)
            STRESS20S = STRESS20S + STRESS(L)
            STRESS20NS = STRESS20NS + STRESSN(L)
            STRESS20WS = STRESS20WS + STRESSW(L)
            TTD(L) = TTD(L-1)
            TT20S = TT20S + TTD(L)
            WUPRD(L) = WUPRD(L-1)
          ENDDO
          SRADD(1) = SRAD
          SRAD20S = SRAD20S + SRAD
          TMEAND(1) = TMEAN
          TMEAN20S = TMEAN20S + TMEAND(1)
          STRESS(1) = AMIN1(WFG,NFG)
          STRESSN(1) = NFG
          STRESSW(1) = WFG
          STRESS20S = STRESS20S + STRESS(1)
          STRESS20NS = STRESS20NS + STRESSN(1)
          STRESS20WS = STRESS20WS + STRESSW(1)
          TTD(1) = TT
          TT20S = TT20S + TTD(1)
          WUPRD(1) = AMAX1(0.0,AMIN1(10.0,WUPR))
          IF (TMEANNUM.GE.20) THEN
            IF (TMEANNUM.LE.20) TMEAN20P = TMEAN20S/20.0
            SRAD20 = SRAD20S/20.0
            TMEAN20 = TMEAN20S/20.0
            TT20 = TT20S/20.0
            STRESS20 = STRESS20S/20.0
            STRESS20N = STRESS20NS/20.0
            STRESS20W = STRESS20WS/20.0
          ELSE
            SRAD20 = 0.0
            TT20 = 0.0
            TMEAN20 = 0.0
            STRESS20 = 0.0
            STRESS20N = 0.0
            STRESS20N = 0.0
          ENDIF

          ! Monthly means
          CALL Calendar (year,doy,dom,month)
          IF (DOM.GT.1) THEN
            TMAXSUM = TMAXSUM + TMAX
            TMINSUM = TMINSUM + TMIN
            DAYSUM = DAYSUM + 1.0
          ELSE
            IF (DAYSUM.GT.0) THEN
              IF (TMAXM.LT.TMAXSUM/DAYSUM) TMAXM=TMAXSUM/DAYSUM
              IF (TMINM.GT.TMINSUM/DAYSUM) TMINM=TMINSUM/DAYSUM
            ENDIF
              TMAXSUM = TMAX
              TMINSUM = TMIN
              DAYSUM =  1
          ENDIF

!-----------------------------------------------------------------------
!         Calculate PAR utilization efficiencies
!-----------------------------------------------------------------------

          IF (PARMJC.GT.0.0) PARUEC = AMAX1(0.0,
     &     (RTWT+LFWT+STWT+CRWT+SRWT+RSWT+SENTOPLITTER
     &     +SENROOT-SEEDUSE)
     &     * PLTPOP / PARMJC)
          IF (PARMJIC.GT.0.0) PARIUED = AMAX1(0.0,
     &     (RTWT+LFWT+STWT+CRWT+SRWT+RSWT+SENTOPLITTER
     &     +SENROOT-SEEDUSE)
     &     * PLTPOP / PARMJIC)

          IF (CARBOBEG.GT.0.0) THEN
            PARIUE = (CARBOBEG*PLTPOP)/(PARMJFAC*SRAD*PARI)
          ENDIF

!-----------------------------------------------------------------------
!         Determine if nitrogen fertilizer applied
!-----------------------------------------------------------------------

          IF (FERNIT.GT.FERNITPREV) THEN
            FAPPNUM = FAPPNUM + 1
            AMTNIT = FERNIT
            WRITE(fappline(fappnum),'(A1,I4,A10,I7,A13,I4,A6)')
     &        ' ',NINT(FERNIT-FERNITPREV),' kg/ha on ',
     &        YEARDOY,'     To date ',NINT(amtnit),' kg/ha'
            FERNITPREV = FERNIT
          ENDIF

!-----------------------------------------------------------------------
!         Calculate water availability ratio
!-----------------------------------------------------------------------

          BASELAYER = 0.0
          H2OA = 0.0
          IF (ISWWAT.NE.'N') THEN
            DO L = 1, NLAYR
              DLAYRTMP(L) = DLAYR(L)
              BASELAYER = BASELAYER + DLAYR(L)
              IF (RTDEP.GT.0.0.AND.RTDEP.LT.BASELAYER) THEN
                DLAYRTMP(L) = RTDEP-(BASELAYER-DLAYR(L))
                IF (DLAYRTMP(L).LE.0.0) EXIT
              ENDIF
              H2OA = H2OA + 10.0*AMAX1(0.0,(SW(L)-LL(L))*DLAYRTMP(L))
            ENDDO
            IF (EOP.GT.0.0) THEN
              WAVR = H2OA/EOP
            ELSE
              WAVR = 99.9
            ENDIF
          ENDIF

!-----------------------------------------------------------------------
!         Upgrade albedo
!-----------------------------------------------------------------------

          ! When running in CSM
          IF (FILEIOT.EQ.'DS4') THEN
            IF (LAI .LE. 0.0) THEN
              ALBEDO = ALBEDOS
            ELSE
              ALBEDO = 0.23-(0.23-ALBEDOS)*EXP(-0.75*LAI)
            ENDIF
          ELSE
            ALBEDO = ALBEDOS  
          ENDIF
          
!-----------------------------------------------------------------------
!         Compute weights,etc. at end crop
!-----------------------------------------------------------------------

          IF (STGYEARDOY(11).EQ.YEARDOY) THEN

            ! LAH No adjustment for fraction of day to maturity
            RSWTM = RSWT
            RTWTM = RTWT
            LFWTM = LFWT
            STWTM = STWT
            CRWTM = CRWT

            LNUMSM = LNUM

            IF (LFWTM+STWTM+CRWTM+RSWTM.GT.0.0)
     &       RSCM = RSWTM/(LFWTM+STWTM+CRWTM)
            IF (RTWTM.GT.0.0)
     &       SHRTM = (LFWTM+STWTM+CRWTM+RSWTM)/RTWTM

            CWAM = (LFWTM+STWTM+CRWTM+RSWTM)*PLTPOP*10.0
            VWAM = (LFWTM+STWTM+CRWTM+RSWTM)*PLTPOP * 10.0
            
            ! For Grazing
            cwahc = (lwphc+swphc+rswphc)*pltpop*10.0
            ! Adjustments for spikes that removed by grazing,etc..

            RWAM = RTWTM*PLTPOP*10.0
            SDWAM = (SEEDRS+SDCOAT)*PLTPOP*10.0

            IF (CWAM.GT.0.0) THEN
              HIAM = HIAD
            ENDIF

            SENWACM = SENTOPLITTERA+SENROOTA

            RSWAM = RSWAD

            CNAM = CNAD
            VNAM = VNAD
            VNPCM = VANC*100.0
            RNAM = RNAD
            SRNAM = SRNAD

            HINM = HIND

            ! Set harvest product outputs
            HWAM = SRWT * PLTPOP * 10.0
            HNAM = SRNAM
            IF (SRNOPD.GT.0.0) HWUM = SRWT/FLOAT(SRNOPD)
            HNUMAM = FLOAT(SRNOPD)*PLTPOP
            HNUMGM = FLOAT(SRNOPD)
            HNUMPM = FLOAT(SRNOPD)
            BRNUMSH = BRNUMST
            IF (SRWT.GT.0.0) HNPCM = SROOTN/SRWT*100.0

          ENDIF

!=======================================================================
        ENDIF  ! End of after planted (integrate) section
!=======================================================================


!***********************************************************************
      ELSEIF (DYNAMIC.EQ.OUTPUT .AND. STEP.EQ.STEPNUM. OR.
     &        DYNAMIC.EQ.SEASEND .AND. SEASENDOUT.NE.'Y') THEN
!***********************************************************************

!         Simulated outputs only
!          IDETG (GROUT in controls (Y,N))  Plant growth outputs
!           Y->Work_details+Plantgro+Plantgr2+Plantgrf
!              +PlantN(If N switched on)
!           FROUT->#=number of days between outputs
!          IDETS (SUMRY in controls (Y,N)) Summary outputs
!           Y->Summary+Plantsum+Work(Harvest)                        
!        
!         Simulated+Measured outputs
!          IDETO (OVVEW in controls (Y,E,N)) Overview outputs
!           Y->Overview+Evaluate(+Measured if IDETG=Y)
!           E->Evaluate only
!          IDETL (VBOSE in controls (0,N,Y,D,A))
!           Y->Leaves+Tiers+Measured                 
!           D->+Phenols+Phenolm+Plantres+Plantrem
!           A->Errora+Errors+Errort+Full Reads
!           0,A are meta switches:
!             0 switches everything to N apart from IDETS,which given a Y,
!               and IDETO,which given an E when RNMODE is not N (seasonal)
!             A switches ALL outputs on  

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
           IF (LFWT.GT.1.0E-6) 
     &      SLAOUT=(PLA-SENLA-LAPHC)/(LFWT*(1.0-LPEFR)+LLRSWT)
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

!-----------------------------------------------------------------------
!       TIME SEQUENCE OUTPUTS (WorkPlantgro,gr2,grf)
!-----------------------------------------------------------------------

        IF (  (MOD(DAS,FROPADJ).EQ.0.AND.YEARDOY.GE.PLYEARDOY)
     &   .OR. (YEARDOY.EQ.PLYEARDOY)
     &   .OR. (YEARDOY.EQ.STGYEARDOY(1))
     &   .OR. (YEARDOY.EQ.STGYEARDOY(HSTG))
     &   .OR. (YEARDOY.EQ.STGYEARDOY(11))) THEN

!-----------------------------------------------------------------------
!         IDETL = A OUTPUTS (Work details)
!-----------------------------------------------------------------------     

          IF (IDETL.EQ.'A') THEN
!            WRITE(fnumwrk,*)' '
!            WRITE(fnumwrk,'(A25,I16,I7,I7)')
!     &       ' Year,day,DAP            ',YEAR,DOY,DAP
!            WRITE(fnumwrk,'(A34,2F7.3)')
!     &       ' Rainfall,Irrigation mm           ',rain,irramt
!            WRITE(fnumwrk,'(A34,2F7.3)')
!     &       ' Tmean,Tcan oC                    ',tmean,tcan
!            WRITE(fnumwrk,'(A34,2F7.3)')
!     &       ' Tcan-Tmean Today and average oC  ',tcan-tmean,tdifav
!            WRITE(fnumwrk,'(A34,F7.1)')
!     &       ' Windspeed m/s                    ',windsp     
!            WRITE(fnumwrk,'(A34,2F7.3,2F7.1)')
!     &       ' Brstage,Lnum. Beginning,end of day',
!     &       brstageprev,brstage,lnumprev,lnum
!            IF (PLA-SENLA-LAPHC.LT.9999.9) THEN
!              WRITE(fnumwrk,'(A34,F7.1,F7.1)')
!     &        ' Laminae area end day /m2,/plant  ',lai,pla-senla-laphc
!            ELSE
!              WRITE(fnumwrk,'(A34,F7.1,I7)')
!     &        ' Laminae area end day /m2,/plant  ',
!     &        lai,NINT(pla-senla-laphc)
!            ENDIF
!            WRITE(fnumwrk,'(A25,I1,A7,2F7.3)')
!     &       ' PARI,competition model,C',CN,' 1-crop',PARI,PARI1
!            IF (Rlf.GT.0.0) THEN
!              WRITE(fnumwrk,'(A34,2F7.1,2F7.1)')
!     &         ' Ratm,Rcrop,Rcrop*Rco2/R,*H2o     ',
!     &        ratm,rcrop,rcrop*rlfc/rlf,rcrop*rlfc/rlf*(1.0-(1.0-wfp))
!            ELSE
!              WRITE(fnumwrk,'(A34,2F7.1)')
!     &         ' Ratm,Rcrop                       ',
!     &        ratm,rcrop                         
!            ENDIF
!            IF (FILEIOT.NE.'XFL') THEN
!             IF (IDETL.EQ.'D'.OR.IDETL.EQ.'A') THEN
!               IF (meevp.EQ.'R')THEN
!                 WRITE(fnumwrk,'(A50)')
!     &         ' Model (CSM) pot.evap.method: Priestley-Taylor R  '
!               ELSEIF (meevp.EQ.'P')THEN
!                 WRITE(fnumwrk,'(A51)')
!     &         ' Model (CSM) pot.evap.method: FAO Penman (FAO-24) P'
!               ELSEIF (meevp.EQ.'F')THEN
!                 WRITE(fnumwrk,'(A50,A10)')
!     &         ' Model (CSM) pot.evap.method: FAO Penman-Monteith ', 
!     &         '(FAO-56) F'
!               ELSEIF (meevp.EQ.'D')THEN
!                 WRITE(fnumwrk,'(A53,A10)')
!     &           ' Model (CSM) pot.evap.method: Dynamic Penman-Monteith'
!               ENDIF
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
!              WRITE(fnumwrk,'(A34,4F7.3,F8.3)')
!     &         ' EO  P-T,Pen,M-Pen,Ebud,Model     ',
!     &         eopt,eopen,eompen,eoebud,eo
!              WRITE(fnumwrk,'(A34,4F7.3,F8.3)')
!     &         ' EOCrp                            ',
!     &         eopt,eopen,eompcrp,eoebudcrp,eo
!              WRITE(fnumwrk,'(A34,4F7.3,F8.3)')
!     &         ' EOCrpCo2                         ',
!     &         eopt,eopen,eompcrpco2,eoebudcrpco2,eo
!              WRITE(fnumwrk,'(A34,4F7.3,F8.3)')
!     &         ' EOCrpCo2H2o                         ',
!     &         eopt,eopen,eompcrpco2h2o,eoebudcrpco2h2o,eo
!              IF (WFP.LT.1.0.AND.WFP.GT.0.0) 
!     &         WRITE(fnumwrk,'(A41,F4.2,A8,F4.1,A8,F4.1)')
!     &         ' NB.Water stress effect operative. WFP = ',wfp,
!     &         ' TCAN = ',tcan,' TAIR = ',tmean
!              WRITE(fnumwrk,'(A34,4F7.1,F8.1)')
!     &         ' EOC P-T,Pen,M-P,Ebud,Model       ',
!     &         eoptc,eopenc,eompenc,eoebudc,eoc
!              WRITE(fnumwrk,'(A34,4F7.1,F8.1)')
!     &         ' EOCrpC                           ',
!     &         eoptc,eopenc,eompcrpc,eoebudcrpc,eoc
!              WRITE(fnumwrk,'(A34,4F7.1,F8.1)')
!     &         ' EOCrpCo2C                        ',
!     &         eoptc,eopenc,eompcrpco2c,eoebudcrpco2c,eoc
!              WRITE(fnumwrk,'(A34,4F7.1,F8.1)')
!     &         ' EOCrpCo2h2oC                     ',
!     &         eoptc,eopenc,eompcrpco2h2oc,eoebudcrpco2h2oc,eoc
!             ENDIF
!            ENDIF
            IF (EYEARDOY.LE.YEARDOY) THEN
!              WRITE(fnumwrk,'(A34,2F7.3)')
!     &         ' Pot.pl./Pot.soil evap; /Pot.pl330',epsratio,tratio
!              WRITE(fnumwrk,'(A34,F7.3)')
!     &         ' Quantum requirement              ',photqr
!              WRITE(fnumwrk,'(A34,2F7.1)')
!     &         ' CO2,Estimated internal CO2 vpm   ',co2,co2intppm
!              WRITE(fnumwrk,'(A34,F7.3,6F7.3)')
!     &         ' Phs facs Co2,Temp,H2o,N,Rsvs,Vpd ',
!     &         co2fp,tfp,wfp,nfp,rsfp,vpdfp
!              WRITE(fnumwrk,'(A34,3F7.3)')
!     &         ' Phs. Rue,Rue+Co2i,Resistances    ',
!     &         carbobegr*pltpop,carbobegi*pltpop,carbobegm*pltpop
!              WRITE(fnumwrk,'(A34,3F7.2)')
!     &         ' CH2O Start,end,remobilized       ',
!     &         carbobeg*pltpop*10.,
!     &         carboend*pltpop*10.0,senlfgrs*pltpop*10.0
!              IF(CUMDU.LE.DUSRI.AND.CUMDU+DU.GT.DUSRI.AND.
!     &           SRDAYFR.GT.0.0)THEN
!                 WRITE(fnumwrk,'(A34, I7)')
!     &            ' STORAGE ROOT INITIATION (no/pl)  ',srnopd
!                 WRITE(fnumwrk,'(A34,2F7.1)')
!     &            ' Canopy wt at end of day (kg/ha)  ',
!     &            (lfwt+stwt+crwt+rswt)*pltpop*10.0
!                 WRITE(fnumwrk,'(A34,F7.1)')
!     &            ' Storage root fraction            ',srfr
!              ENDIF
!              IF (SRWTGRS+SRWTGRS.GT.0.0) WRITE(FNUMWRK,'(A48)')
!     &         ' Surplus assimilates sent to storage roots      '
!              IF (LRTIP.EQ.1) WRITE(fnumwrk,'(A21)')
!     &         ' Root tip in layer 1 '
!              WRITE(FNUMWRK,'(A34,2F7.2)')
!     &         ' N demand,shortage (kg/ha)        ',
!     &         andem,AMAX1(0.0,andem-nupap)
              ! Folowing detailed outputs can be printed if tvi1=1
              tvi1 = 0
!              IF (tvi1.eq.1) THEN
!                IF (ANDEM.LE.0.0) THEN
!                  WRITE(FNUMWRK,'(A44)')
!     &            ' N demand at zero! Components of demand/use:' 
!                ELSE
!                  WRITE(FNUMWRK,'(A47)')
!     &            ' N demand above zero! Components of demand/use:' 
!                ENDIF  
!                WRITE(FNUMWRK,*)
!     &          ' Leaves            ',lndem*pltpop*10.0
!                WRITE(FNUMWRK,*)
!     &          ' Stem              ',sndem*pltpop*10.0
!                WRITE(FNUMWRK,*)
!     &          ' Roots             ',rndem*pltpop*10.0
!                WRITE(FNUMWRK,*)
!     &          ' Storage root      ',srndem*pltpop*10.0
!                WRITE(FNUMWRK,*)
!     &          ' Seed use          ',(seednuse+seednuse2)*pltpop*10.0
!                WRITE(FNUMWRK,*)
!     &          ' Reserves use      ',rsnused*pltpop*10.0
!                IF (ANDEM.GT.0.0.AND.NUPAP.LT.ANDEM)
!     &          WRITE(fnumwrk,'(A38)')
!     &          '  N uptake insufficient to meet demand'
!                IF (ANDEM.GT.10.0)
!     &          WRITE(fnumwrk,'(A11,F4.1,A23,F4.1)')
!     &          ' N demand (',ANDEM,') very high! Uptake = ',nuf*andem
!              ENDIF 
            ENDIF ! End EYEARDOY.LE.YEARDOY
          ENDIF ! End detailed WORK writes  IDDETL = 'A'   

!-----------------------------------------------------------------------
!         IDETG NE N OUTPUTS (Plantgro,gr2,grf,n)
!-----------------------------------------------------------------------     

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
                WRITE (NOUTPG,104)
     &           EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
                WRITE (NOUTPG,102) TN,TNAME
                WRITE (NOUTPG,107) CROP,VARNO,VRNAME
                CALL Calendar (year,doy,dom,month)
                WRITE(NOUTPG,108)
     &           month,dom,plyeardoy,NINT(pltpopp),NINT(rowspc)
  103           FORMAT (' MODEL            ',A8)
 1031           FORMAT (' MODULE           ',A8)
  104           FORMAT (' EXPERIMENT       ',A8,A1,A2,A2,A47)
  102           FORMAT (' TREATMENT',I3,'     ',A25)
  107           FORMAT (' GENOTYPE         ',A2,A6,'  ',A16)
  108           FORMAT (' PLANTING         ',A3,I3,I8,2X,I4,
     &           ' plants/m2 in ',I3,' cm rows',/)
              ENDIF
              WRITE (NOUTPG,2201)
 2201         FORMAT ('@YEAR DOY   DAS   DAP TMEAN',
     A               '  GSTD  L#SD',
     B               ' PARID PARUD  AWAD',
     C               '  LAID  SAID  CAID',
     D               '  TWAD SDWAD  RWAD  CWAD  LWAD  SWAD  HWAD  HIAD',
     E               ' CRWAD RSWAD SNWLD SNWSD',
     F               '  RS%D',
     G               '  H#AD  HWUD',
     I               '  S#AD  SLAD  RDPD  PTFD',
     J               '  SWXD WAVRD',
     K               ' WUPRD  WFPD  WFGD',
     L               '  NFPD  NFGD NUPRD',
     M               '  TFPD  TFGD',
     N               ' DYLFD',
     O               '      ',
     P               '      ')
            ENDIF  ! End Plantgro header writes
            WRITE (NOUTPG,501)
     A      YEAR,DOY,DAS,DAP,TMEAN,BRSTAGEC,LNUM,
     B      PARIOUT,PARIUE,AMIN1(999.9,CARBOBEG*PLTPOP*10.0),
     &      LAIC,SAID,CAIC,
     C      NINT(TWAD),NINT(SDWAD),NINT(RWAD),NINT(CWAD),
     D      NINT(LLWADOUT),NINT(STWADOUT),NINT(HWAD),HIAD,
     E      NINT(CRWADOUT),NINT(RSWAD),
     &      SENTOPLITTERAC,SENROOTC,
     F      RSCD*100.0,NINT(HNUMAD),HWUDC,
     G      NINT(BRNUMST),SLAOUT,RTDEP/100.0,PTF,H2OA,
     H      AMIN1(99.9,WAVR),AMIN1(15.0,WUPR),
     I      1.0-WFP,1.0-WFG,
     J      1.0-NFP,1.0-NFG,AMIN1(2.0,NUPRATIO),
     K      1.0-TFP,1.0-TFG,1.0-DFOUT
  501       FORMAT(
     A      I5,I4,2I6,F6.1,A6,F6.1,
     B      F6.3,F6.2,F6.1,
     &      A6,F6.3,A6,
     C      4I6,
     D      3I6,F6.3,
     E      2I6,
     &      2A6,
     F      F6.2,I6,A6,
     G      I6,F6.1,2F6.2,F6.1,
     H      F6.1,F6.2,
     I      2F6.2,
     J      2F6.2,F6.1,
     K      2F6.2,F6.2)
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
                WRITE (NOUTPGF,104)
     &         EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
                WRITE (NOUTPGF,102) TN,TNAME
                WRITE (NOUTPGF,107) CROP,VARNO,VRNAME
                CALL Calendar (year,doy,dom,month)
                WRITE(NOUTPGF,108)
     &           month,dom,plyeardoy,NINT(pltpopp),NINT(rowspc)
              ENDIF
              WRITE (NOUTPGF,2215)
 2215         FORMAT ('!........DATES.......  ...TEMP... STAGE ',
     N               ' ...PHENOLOGY.... ',
     1               ' ....PHOTOSYNTHESIS.... ', 
     M               ' .....GROWTH..... ',
     2               'H2O STRESS DETERMINANTS',
     2               ' .N STRESS DETERMINANTS.')
              WRITE (NOUTPGF,2205)
 2205         FORMAT ('@YEAR DOY   DAS   DAP TMEAN TCDIF  GSTD',
     N               '    DU DYLFD',
     1               '  TFPD  WFPD  NFPD CO2FD RSFPD', 
     M               '  TFGD  WFGD  NFGD',
     &               ' WAVRD WUPRD  SWXD  EOPD',
     &               '  SNXD LN%RD SN%RD RN%RD            ')
            ENDIF  ! End Plantgro header writes
            WRITE (NOUTPGF,507)
     A      YEAR,DOY,DAS,DAP,TMEAN,TCDIF,BRSTAGEC,
     B      DU,1.0-DFOUT,
     C      1.0-TFP,1.0-WFP,1.0-NFP,1.0-CO2FP,1.0-RSFP,
     D      1.0-TFG,1.0-WFG,1.0-NFG,
     H      AMIN1(99.9,WAVR),AMIN1(15.0,WUPR),H2OA,EOP,
     I      SNO3PROFILE+SNH4PROFILE,LNCR,SNCR,RNCR
  507       FORMAT(
     a      I5,I4,2I6,2F6.1,A6,
     b      F6.1,F6.2,
     c      5F6.2,
     d      3F6.2,
     e      2F6.2,F6.1,F6.2,
     F      F6.1,3F6.2)
            ! End Plantgro reduction factor writes
            
            ! PlantGr2
            IF (YEARDOY.EQ.PLYEARDOY) THEN
              OPEN (UNIT = NOUTPG2, FILE = OUTPG2, STATUS='UNKNOWN',
     &        POSITION = 'APPEND')
              IF (FILEIOT(1:2).EQ.'DS') THEN
                CALL HEADER(2, NOUTPG2, RUN)
              ELSE
                WRITE (NOUTPG2,'(/,A79,/)') OUTHED
                WRITE (NOUTPG2,103) MODEL
                WRITE (NOUTPG2,1031) MODNAME
                WRITE (NOUTPG2,104)
     &           EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
                WRITE (NOUTPG2,102) TN,TNAME
                WRITE (NOUTPG2,107) CROP,VARNO,VRNAME
                WRITE(NOUTPG2,108)
     &           month,dom,plyeardoy,NINT(pltpopp),NINT(rowspc)
              ENDIF 
              WRITE (NOUTPG2,2251)
 2251         FORMAT ('@YEAR DOY   DAS   DAP TMEAN  GSTD  RSTD',
     A          ' LAIPD LAISD  LAID  CHTD SDWAD SNWLD SNWSD',
     a          '  H#AD  HWUD',
     B          ' SHRTD  PTFD  RDPD',
     C          '  RL1D  RL2D  RL3D  RL4D  RL5D  RL6D',
     D          '  RL7D  RL8D  RL9D RL10D')
            ENDIF   ! Plantgr2 header writes
            LAIPROD = PLA*PLTPOP*0.0001
            CALL Csopline(laiprodc,laiprod)
            CALL Csopline(canhtc,canht)
            L = MAX(1,LNUMSG-INT(LLIFG))
            WRITE (NOUTPG2,502)
     A       YEAR,DOY,DAS,DAP,TMEAN,BRSTAGEC,BRSTAGE,
     B       LAIPRODC,SENLA*PLTPOP*0.0001,LAIC,CANHTC,SDWAD,
     &       SENTOPLITTERAC,SENROOTC,
     &       NINT(HNUMAD),HWUDC,
     D       SHRTD,PTF,RTDEP/100.0,(RLV(I),I=1,10)
  502       FORMAT(
     A       I5,I4,2I6,F6.1,A6,F6.2,
     B       A6,F6.2,A6,A6,F6.1,2A6,I6,A6,
     D       2F6.2,F6.3,10F6.2)
            ! End PlantGr2 writes

            ! PlantN
            IF (ISWNIT.NE.'N') THEN
              IF (YEARDOY.EQ.PLYEARDOY) THEN
                OPEN (UNIT = NOUTPN, FILE = OUTPN, STATUS='UNKNOWN',
     &          POSITION = 'APPEND')
                IF (FILEIOT(1:2).EQ.'DS') THEN
                  CALL HEADER(2, NOUTPN, RUN)
                ELSE
                  WRITE (NOUTPN,'(/,A79,/)') OUTHED
                  WRITE (NOUTPN,103) MODEL
                  WRITE (NOUTPN,1031) MODNAME
                  WRITE (NOUTPN,104)
     &             EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
                  WRITE (NOUTPN,102) TN,TNAME
                  WRITE (NOUTPN,107) CROP,VARNO,VRNAME
                  WRITE (NOUTPN,108)
     &             month,dom,plyeardoy,NINT(pltpopp),NINT(rowspc)
                ENDIF 
                WRITE (NOUTPN,2252)
!               2021-02-15 chp Change NUAD to NUAC in header.
 2252           FORMAT ('@YEAR DOY   DAS   DAP TMEAN  GSTD  NUAC',
     A           '  TNAD SDNAD  RNAD  CNAD  LNAD  SNAD  HNAD  HIND',
     F           ' RSNAD SNN0D SNN1D',
     B           '  RN%D  LN%D  SN%D  HN%D SDN%D  VN%D',
     C           ' LN%RD SN%RD RN%RD  VCN%  VMN% NUPRD',
     D           ' NDEMD NFLF2')
              ENDIF  ! Plantn header writes
              CALL Csopline(senn0c,sennal(0))
              CALL Csopline(sennsc,sennas)
              WRITE (NOUTPN,503)
     A         YEAR,DOY,DAS,DAP,TMEAN,BRSTAGEC,NUPAC,
     B         TNAD,SDNAD,RNAD,CNAD,LLNAD,SNAD,HNAD,HINDC,
     H         RSNAD,SENN0C,SENNSC,
     C         RANC*100.0,LANC*100.0,SANCOUT*100.0,
     D         AMIN1(9.9,HNC*100.0),SDNC*100.0,AMIN1(9.9,VANC*100.0),
     E         LNCR,SNCR,RNCR,
     &         VCNC*100.0,VMNC*100.0,
     F         AMIN1(2.,NUPRATIO),ANDEM,
     g         1.0-NFLF2(0)
  503          FORMAT(
     1         I5,I4,2I6,F6.1,A6,F6.1,
     2         F6.1,2F6.2,4F6.1,A6,
     3         F6.2,2A6,
     4         3F6.3,
     5         3F6.3,
     6         3F6.3,
     2         F6.2,F6.2,
     8         F6.2,F6.1,
     9         F6.1)
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

        ELSEIF(YEARDOY.LT.PLYEARDOY.AND.(MOD(DAS,FROPADJ)).EQ.0.AND.
!    &   IPLTI.EQ.'A') THEN
     &   (IPLTI.EQ.'A' .OR. IPLTI.EQ.'F')) THEN
     
          ! Automatic planting
          !WRITE (fnumwrk,*) 'Yeardoy ',yeardoy
          !WRITE (fnumwrk,*) 'Water thresholds ',swpltl,swplth
          !WRITE (fnumwrk,*) 'Water ',avgsw
          !WRITE (fnumwrk,*) 'Temperature thresholds ',pttn,ptx
          !WRITE (fnumwrk,*) 'Temperature ',tsdep

        ENDIF  ! End time-course outputs (appropriate day, etc.)
               ! (MOD(DAS,FROPADJ).EQ.0.AND.YEARDOY.GE.PLYEARDOY),etc..

!***********************************************************************
        IF (STGYEARDOY(11).EQ.YEARDOY .OR.
     &      DYNAMIC.EQ.SEASEND) THEN         ! If harvest/failure day
!***********************************************************************


!-----------------------------------------------------------------------
!         IDETO OUTPUTS AND NECESSARY DATA INPUTS (Evaluate & Overview)
!-----------------------------------------------------------------------
          
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
              IF (FILEADIR(TVILENT(FILEADIR):TVILENT(FILEADIR)).NE.
     &         SLASH) THEN
                FILEA = FILEADIR(1:TVILENT(FILEADIR))//
     &           SLASH//EXCODE(1:8)//'.'//EXCODE(9:10)//'A'
              ELSE
                FILEA = FILEADIR(1:TVILENT(FILEADIR))//
     &           EXCODE(1:8)//'.'//EXCODE(9:10)//'A'
              ENDIF
            ELSE
              FILEA = FILENEW(1:FILELEN-12)//EXCODE(1:8)//'.'//
     &        EXCODE(9:10)//'A'
            ENDIF       
            FEXISTA = .FALSE.
            INQUIRE (FILE = FILEA,EXIST = FEXISTA)
            IF (.not.FEXISTA) THEN
              WRITE (Message(1),'(A23,A50)')
     &         'Could not find A-file: ',filea(1:50)
              WRITE (Message(2),'(A23,A50)')
     &         'Experiment file:       ',fileio(1:50)
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
              IF (HWAMM.LE.0.0.AND.HYAMM.GT.0..AND.HMPC.GT.0.0)
     &          HWAMM = HYAMM * (1.0-HMPC/100.0)
              
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HWUM',hwumm)
          
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'LAIX',laixm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CWAM',cwamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'BWAH',vwamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'H#AM',hnumamm)
              IF (hnumamm.le.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HNOAM',hnumamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'H#SM',hnumgmm)
              IF (hnumgmm.le.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HNOSM',hnumgmm)
              IF (hnumgmm.le.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'H#UM',hnumgmm)
              IF (hnumgmm.le.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HNOUM',hnumgmm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'L#SM',lnumsmm)
              IF (lnumsmm.le.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'LNOSM',lnumsmm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'BR#SH',brnumshm)
          
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CNAM',cnamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'VNAM',vnamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HNAM',hnamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CN%M',cnpchm)
              IF (cnpchm.le.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CNPCM',cnpchm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HN%M',hnpcmm)
              IF (hnpcmm.le.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HNPCM',hnpcmm)
              IF (HNPCMM.LE.0.0) HNPCMM = -99   
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'VN%M',vnpcmm)
              IF (vnpcmm.le.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'VNPCM',vnpcmm)
          
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HIAM',hiamm)
              IF (HIAMM.GE.1.0) HIAMM = HIAMM/100.0
          
              CALL AREADI (FILEA,TN,RN,SN,ON,CN,'EDAT',edatm)
              CALL AREADI (FILEA,TN,RN,SN,ON,CN,'GDAT',gdatm)
          
              DO L = 1,PSNUM              
               CALL AREADI (FILEA,TN,RN,SN,ON,CN,psabv(l),psdatm(l))
               CALL LTRIM(PSABV(L)) 
               IF (PSABV(L).EQ.'TSAT')
     &           CALL AREADI (FILEA,TN,RN,SN,ON,CN,'TSDAT',psdatm(l))
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
              FILET=
     &        FILENEW(1:FILELEN-12)//EXCODE(1:8)//'.'//EXCODE(9:10)//'T'
              FEXISTT  = .FALSE.
              INQUIRE (FILE = FILET,EXIST = FEXISTT)
              IF (.not.FEXISTT) THEN
                WRITE (Message(1),'(A23,A50)')
     &          'Could not find T-file: ',filet(1:50)
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
                    IF (LINET(1:7).EQ.'*DATA(T' .OR.
     &               LINET(1:7).EQ.'*EXP.DA' .OR.
     &               LINET(1:7).EQ.'*EXP. D' .OR.
     &               LINET(1:7).EQ.'*TIME_C' .OR.
     &               LINET(1:7).EQ.'$EXPERI') THEN
                      TNCHAR = TL10FROMI(TN)
                      LENLINE = TVILENT(LINET)
                      IF(LINET(1:7).EQ.'*EXP.DA'.OR.
     &                   LINET(1:7).EQ.'*EXP. D'.OR.
     &                   LINET(1:7).EQ.'$EXPERI')THEN
                        GROUP = 'A'
                        DO L = 1,30
                          IF (LINET(L:L+1).EQ.': ') L1 = L+2
                          IF (LINET(L:L).EQ.':'.AND.
     &                        LINET(L+1:L+1).NE.' ')
     &                      L1 = L+1
                          IF (L1.GT.0.AND.L.GT.L1+9.AND.
     &                                    LINET(L:L).NE.' ') THEN
                            L2 = L ! Start of group information in tfile
                            EXIT
                          ENDIF
                        ENDDO
                        LENTNAME = MIN(15,TVILENT(TNAME))
                        LENGROUP = MIN(L2+14,LENLINE)
                        IF (TVILENT(TNCHAR).EQ.1) THEN
                          LINESTAR = LINET(L1:L1+9)//' '//
     &                    TNCHAR(1:1)//' '//TNAME(1:LENTNAME)
                        ELSEIF (TVILENT(TNCHAR).EQ.2) THEN
                          LINESTAR = LINET(L1:L1+9)//' '//
     &                     TNCHAR(1:2)//' '//TNAME(1:LENTNAME)
                        ELSEIF (TVILENT(TNCHAR).EQ.3) THEN
                          LINESTAR = LINET(L1:L1+9)//' '//
     &                     TNCHAR(1:3)//' '//TNAME(1:LENTNAME)
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
                      LINESTAR2 = '*TIER('//TIERNUMC//'):'//
     &                 LINESTAR(1:LENLINESTAR)//LINET(14:LENLINE)
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
                          IF (VALUEI.LE.0) 
     &                       WRITE (FNUMMEAS,'(A180)') LINET
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
                          IF (VALUER.GT.FLOAT(MSTG*10).AND.MDATT.LE.0)
     &                      MDATT = DATE
                          ! To indicate that t data present
                          tdatanum = 1
                         ENDIF ! End picking variables from t for a     
                        ENDIF ! End of details flag 
                      ENDIF ! End correct treatment 
                    ENDIF ! End particular data lines
                  ENDIF ! End valid (ie.non comment) line
                ENDDO
 5555           CONTINUE
                ! If T-file was empty
                IF (TLINENUM.LT.4) THEN
                  tdatanum = 0
                  WRITE (Message(1),'(A23,A50)')
     &             'T-file was empty '
                  CALL WARNING(1,'CSCAS',MESSAGE)
                ENDIF
                CLOSE(FNUMT)
                CLOSE(FNUMMEAS)
              ENDIF ! End t-file reads,measured.out writes
              
              IF (IDETL.EQ.'A') THEN
                ! Use T-data if A-data missing (whem output=all)
                IF (FEXISTT) THEN
!                  WRITE(Fnumwrk,*)' '
!                  WRITE(Fnumwrk,'(A45)')
!     &              ' FINISHED SIMULATION. PREPARING FINAL OUTPUTS'
!                  WRITE(Fnumwrk,*)' '
                  IF (HWAMM.LE.0.0) THEN
                    IF (HWADT.GT.0.0) THEN
                      HWAMM = HWADT
!                      WRITE(Fnumwrk,'(A32)')
!     &                 '  Time-course data used for HWAM'
                    ENDIF
                  ELSE
                    IF (HWADT.GT.0.0) THEN
!                      IF (ABS(100.0*ABS(HWAMM-HWADT)/HWAMM).GT.0.0) THEN
!                      WRITE(Fnumwrk,'(A48,F8.2)')
!     &                '  Pc difference between final,time-course yields'
!     &                ,100.0*ABS(HWAMM-HWADT)/HWAMM
!                      WRITE(Fnumwrk,'(A22,I6)')
!     &                '  Final yield         ',NINT(HWAMM)
!                      WRITE(Fnumwrk,'(A22,I6)')
!     &                '  Time-course yield   ',NINT(HWADT)
!                      ENDIF
                    ENDIF
                  ENDIF
                  IF (CWAMM.LE.0.0) THEN
                    IF (CWADT.GT.0.0) THEN
                      CWAMM = CWADT
!                      WRITE(Fnumwrk,'(A33)')
!     &                 '  Time-course data used for CWAMM'
                    ENDIF
                  ELSE
                    IF (CWADT.GT.0.0) THEN
                      IF (ABS(100.0*ABS(CWAMM-CWADT)/CWAMM).GT.0.0) THEN
                      WRITE(Message(1),'(A48,F8.2)')
     &                'Pc difference between final,time-course canopy ='
     &                ,100.0*ABS(CWAMM-CWADT)/CWAMM
                      WRITE(Message(2),'(A19,I6)')
     &                'Final canopy       ',NINT(CWAMM)
                      WRITE(Message(3),'(A19,I6)')
     &                'Time-course canopy ',NINT(CWADT)
                      CALL WARNING(3,'CSCAS',MESSAGE)
                      ENDIF
                    ENDIF
                  ENDIF
                  IF (LAIXM.LE.0.0.AND.LAIXT.GT.0.0) THEN
                    LAIXM = LAIXT
                    WRITE(Message(1),'(A31)')
     &               'Time-course data used for LAIXM'
                    CALL WARNING(1,'CSCAS',MESSAGE)
!                    WRITE(Fnumwrk,'(A33)')
!     &               '  Time-course data used for LAIXM'
                  ENDIF
                  IF (LNUMSMM.LE.0.0.AND.LNUMSMM.GT.0.0) THEN
                    LNUMSMM = LNUMT
                    WRITE(Message(1),'(A33)')
     &               'Time-course data used for LNUMSMM'
                    CALL WARNING(1,'CSCAS',MESSAGE)
!                    WRITE(Fnumwrk,'(A35)')
!     &               '  Time-course data used for LNUMSMM'
                  ENDIF
                  IF (HIAMM.LE.0.0.AND.HIADT.GT.0.0) THEN
                    HIAMM = HIADT
                    WRITE(Message(1),'(A31)')
     &               'Time-course data used for HIAMM'
                    CALL WARNING(1,'CSCAS',MESSAGE)
!                    WRITE(Fnumwrk,'(A33)')
!     &               '  Time-course data used for HIAMM'
                  ENDIF
                  IF (HWUMM.LE.0.0.AND.HWUT.GT.0.0) THEN
                    HWUMM = HWUT
                    WRITE(Message(1),'(A31)')
     &               'Time-course data used for HWUMM'
                    CALL WARNING(1,'CSCAS',MESSAGE)
!                    WRITE(Fnumwrk,'(A33)')
!     &               '  Time-course data used for HWUMM'
                  ENDIF
                  IF (HNUMAMM.LE.0.0.AND.HNUMAT.GT.0.0) THEN
                    HNUMAMM = HNUMAT
                    WRITE(Message(1),'(A31)')
     &               'Time-course data used for H#AT'
                    CALL WARNING(1,'CSCAS',MESSAGE)
!                    WRITE(Fnumwrk,'(A33)')
!     &               '  Time-course data used for H#AT'
                  ENDIF
                  IF (HNUMGMM.LE.0.0.AND.HNUMET.GT.0.0) THEN
                    HNUMGMM = HNUMET
                    WRITE(Message(1),'(A32)')
     &               'Time-course data used for H#GMM'
                    CALL WARNING(1,'CSCAS',MESSAGE)
!                    WRITE(Fnumwrk,'(A34)')
!     &               '  Time-course data used for H#GMM'
                  ENDIF
                ENDIF
                DO L = 1,PSNUM
                  IF (PSABV(L).EQ.'MDAT'.AND.PSDATM(L).LE.0.0) THEN
                    IF (MDATT.GT.0) THEN
                      PSDATM(L) = INT(MDATT)
                      WRITE(Message(1),'(A31)')
     &                 'Time-course data used for MDATM'
                      CALL WARNING(1,'CSCAS',MESSAGE)
!                      WRITE(Fnumwrk,'(A33)')
!     &                 '  Time-course data used for MDATM'
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
!                  IF (ABS(hiammtmp-hiamm)/hiamm.GT.0.05) THEN
!                    WRITE (fnumwrk,*) 'Reported HI not consistent',
!     &               ' with yield and total weight data  '
!                    WRITE (fnumwrk,*) ' Reported HI   ',hiamm
!                    WRITE (fnumwrk,*) ' Calculated HI ',hiammtmp
!                    WRITE (fnumwrk,*) ' Will use reported value '
!                  ENDIF
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
!                IF (ABS(hwumyld-hwumm)/hwumm.GT.0.05) THEN
!                  WRITE (fnumwrk,*)' '
!                  WRITE (fnumwrk,'(A14)')' MEASURED DATA'
!                  WRITE (fnumwrk,'(A36,A33)')
!     &              ' Reported product wt.not consistent',
!     &              ' with yield and product # data   '
!                  WRITE (fnumwrk,*) ' Reported wt   ',hwumm
!                  WRITE (fnumwrk,*) ' Calculated wt ',hwumyld
!                  WRITE (fnumwrk,*) '   Yield       ',hwamm
!                  WRITE (fnumwrk,*) '   Kernel no   ',hnumamm
!                  WRITE (fnumwrk,*) ' Will use reported value '
!                ENDIF
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
            IF (vwamm+rwamm.GT.0.AND.rswamm.GT.0.0)
     &       rscmm = rswamm/(vwamm+rwamm)
            
            ! Canopy N at harvest 
            IF (vnamm.GT.0.AND.cnamm.LE.0)
     &        cnamm = vnamm
            
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
            IF (cnpchm.LE.0.AND.cnamm.GT.0.AND.cwamm.GT.0.0)
     &        cnpchm = cnamm/cwamm
            
            ! Express dates as days after planting
            edapm = -99
            edapm = Dapcalc(edatm,plyear,plday)
            IF (edapm.GT.200) THEN
              WRITE (Message(1),'(A31,A31,A11)')
     &         'Measured emergence over 200DAP ',
     &         'Maybe reported before planting.',
     &         'Check files'
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
                EVHEADNMMAX = 1
              ENDIF
              IF (EXCODE.NE.EXCODEPREV) THEN
                EVHEADNM = EVHEADNM + 1
                OPEN (UNIT=FNUMEVAL,FILE=FNAMEEVAL,POSITION='APPEND')
                IF (EVHEADNM.LE.EVHEADNMMAX.AND.EVHEADNMMAX.GE.1) THEN
                  LENENAME = TVILENT(ENAME)
                  WRITE (FNUMEVAL,*) ' '
                  WRITE (FNUMEVAL,993) 
     &             EVHEADER,EXCODE,ENAME(1:25),MODNAME
  993             FORMAT (A14,A10,'  ',A25,2X,A8,/)
                ELSE
                  IF (EVHEADNMMAX.GT.1) THEN
                    WRITE (FNUMEVAL,*) ' '
                    WRITE (FNUMEVAL,1995) EVHEADER,MODNAME, 
     &               'ALL REMAIN','ING EXPERIMENTS        '
                  ELSEIF (EVHEADNM.LE.EVHEADNMMAX) THEN
                    WRITE (FNUMEVAL,*) ' '
                    WRITE (FNUMEVAL,1995) EVHEADER,MODNAME,
     &               'ALL EXPERI','MENTS                  '
 1995               FORMAT (A14,2X,A10,A23,2X,A8/)
                  ENDIF 
                ENDIF
              ENDIF
              IF (EVHEADNM.LE.EVHEADNMMAX) THEN
                WRITE (FNUMEVAL,994,ADVANCE='NO')
  994           FORMAT ('@RUN EXCODE      TRNO RN CR EDAPS EDAPM')
                DO L = 1,KEYSTX
                  IF (KEYPS(L).GT.0) THEN
                    IF (PSABVO(KEYPS(L))(1:1).NE.' ') 
     &               WRITE (FNUMEVAL,'(A1)',ADVANCE='NO') ' '
                    WRITE (FNUMEVAL,'(A5,A1)',ADVANCE='NO') 
     &                      PSABVO(KEYPS(L)),'S'
                    IF (PSABVO(KEYPS(L))(1:1).NE.' ') 
     &               WRITE (FNUMEVAL,'(A1)',ADVANCE='NO') ' '
                    WRITE (FNUMEVAL,'(A5,A1)',ADVANCE='NO') 
     &                      PSABVO(KEYPS(L)),'M'
                  ENDIF 
                ENDDO
                WRITE (FNUMEVAL,9942)
 9942           FORMAT (
     &          ' HWAMS HWAMM HWUMS HWUMM',
     &          ' H#AMS H#AMM H#GMS H#GMM',
     &          ' LAIXS LAIXM L#SMS L#SMM',
     &          ' CWAMS CWAMM VWAMS VWAMM',
     &          ' HIAMS HIAMM HN%MS HN%MM VN%MS VN%MM',
     &          ' CNAMS CNAMM HNAMS HNAMM HINMS HINMM')
                CLOSE(FNUMEVAL)
              ENDIF  
            ENDIF  ! End Evaluate header writes
            IF (EXCODE.NE.EXCODEPREV) EVALOUT = 0
            EVALOUT = EVALOUT + 1
            OPEN (UNIT = FNUMEVAL,FILE = FNAMEEVAL,POSITION = 'APPEND')
            WRITE (FNUMEVAL,8404,ADVANCE='NO') RUN,EXCODE,TN,RN,CROP,
     &          edap,edapm
 8404       FORMAT (I4,1X,A10,I6,I3,1X,A2,2I6)
            DO L = 1,KEYSTX
              IF (KEYPS(L).GT.0) THEN 
                IF (PSABVO(KEYPS(L))(1:1).NE.' ') 
     &            WRITE (FNUMEVAL,'(A1)',ADVANCE='NO') ' '
                WRITE (FNUMEVAL,'(I6)',ADVANCE='NO') PSDAP(KEYPS(L))
                IF (PSABVO(KEYPS(L))(1:1).NE.' ') 
     &            WRITE (FNUMEVAL,'(A1)',ADVANCE='NO') ' '
                WRITE (FNUMEVAL,'(I6)',ADVANCE='NO') PSDAPM(KEYPS(L))
              ENDIF
            ENDDO
            WRITE (FNUMEVAL,8406)
     F       NINT(hwam),NINT(hwamm),
     G       hwumchar,hwummchar,
     &       NINT(hnumam),NINT(hnumamm),hnumgm,hnumgmm,
     H       laix,laixm,lnumsm,lnumsmm,
     J       NINT(cwam),NINT(cwamm),
     J       NINT(vwam),NINT(vwamm),
     L       hiamchar,hiammchar,
     M       hnpcmchar,hnpcmmchar,vnpcmchar,vnpcmmchar,
     &       NINT(cnam),NINT(cnamm),NINT(hnam),NINT(hnamm),
     &       hinmchar,hinmmchar
 8406       FORMAT (
     A      I6,I6,
     G      A6,A6,I6,I6,F6.1,F6.1,
     H      F6.1,F6.1,F6.1,F6.1,
     J      I6,I6,
     J      I6,I6,
     L      A6,A6,
     M      A6,A6,A6,A6,
     &      I6,I6,I6,I6,
     &      A6,A6)
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
                    OPEN (UNIT = FNUMOV, FILE = FNAMEOV, 
     &              POSITION = 'APPEND')
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
  203           FORMAT (' MODEL            ',A8)
                IF (ISWNIT.EQ.'N') THEN
                  WRITE (FNUMOV,210) iswwat, iswnit
  210             FORMAT (' MODEL SWITCHES   ','Water: ',A1,
     &            '  Nitrogen: ',A1)
                ELSE
                  WRITE (FNUMOV,211) iswwat, iswnit, mesom
  211             FORMAT (' MODEL SWITCHES   ','Water: ',A1,
     &            '  Nitrogen: ',A1,' (OM decay: ',A1,')')
                ENDIF
                WRITE (FNUMOV,2031) MODNAME
 2031           FORMAT (' MODULE           ',A8)
                WRITE (FNUMOV,2032) MEPHO
 2032           FORMAT (' MODULE SWITCHES  ','Photosynthesis: ',A1)
                ! P=PARU effic,I=P+internal CO2,R=resistances(Monteith)
                WRITE (FNUMOV,2034) FILENEW
 2034           FORMAT (' FILE             ',A60)
                WRITE (FNUMOV,204)
     &           EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
  204           FORMAT (' EXPERIMENT       ',A8,A1,A2,A2,A47)
                WRITE (FNUMOV,202) TN, TNAME
  202           FORMAT (' TREATMENT',I3,'     ',A25)
                WRITE (FNUMOV,207) CROP,VARNO,VRNAME
  207           FORMAT (' GENOTYPE         ',A2,A6,'  ',A16)
                WRITE(FNUMOV,*) ' '
                CALL Calendar (plyear,plday,dom,month)
                WRITE (FNUMOV,208)month,dom,plyeardoy,NINT(pltpop),
     &           NINT(rowspc)
  208           FORMAT(' PLANTING         ',A3,I3,I8,2X,I4,' plants/m2 '
     &          ,'in ',I3,' cm rows')
!                CALL CSYR_DOY(EYEARDOY,YEAR,DOY)
                CALL YR_DOY(EYEARDOY,YEAR,DOY)
                CALL Calendar(year,doy,dom,month)
                WRITE(FNUMOV,109) month,dom,eyeardoy                  
  109           FORMAT (' EMERGENCE        ',A3,I3,I8)
                WRITE(FNUMOV,*) ' '
                WRITE (FNUMOV,209) tmaxx,tmaxm,tminn,tminm              
  209           FORMAT (' TEMPERATURES C   ','Tmax (max):',F5.1,
     &           ' (mnth av):',F5.1,
     &           ' Tmin (min):',F5.1,
     &           ' (mnth av):',F5.1)
                IF (ISWNIT.NE.'N') THEN
                  WRITE(fnumov,2095)cnad+rnad+hnad,hnad,vnad
 2095             FORMAT (' CROP N kg/ha     ','Total:  ',F8.1,
     &             '  Product ',F12.1,
     &             '  Leaf+stem:  ',F6.1)
                  WRITE(fnumov,2096)sennal(0),sennas            
 2096             FORMAT ('                  ','Leaf loss: ',F5.1,
     &             '  Root loss:  ',F8.1)
                  WRITE(fnumov,2093)isoiln,amtnit,fsoiln
 2093             FORMAT (' SOIL N kg/ha     ','Initial:',F8.1,
     &             '  Applied:',F12.1,
     &             '  Final:      ',F6.1)
                  WRITE(fnumov,2094)tnoxc,tlchc,
     &              tominfomc+tominsomc-tnimbsom  
 2094             FORMAT ('                  ',
     &             'Denitrified:',F4.1,
     &             '  Leached:',F12.1,
     &             '  Net from OM:',F6.1)
                  WRITE(fnumov,2099)tnimbsom,tominfomc,tominsomc   
 2099             FORMAT ('                  ',
     &             'OM Fixation:',F4.1,
     &             '  Fresh OM decay:',F5.1,
     &             '  SOM decay:',F8.1)
                  IF (tominsom1.GT.0.0)
     &             WRITE(fnumov,2098)NINT(tominsom1c),NINT(tominsom2c),
     &             NINT(tominsom3c)
 2098              FORMAT ('                  ',
     &             'SOM1 decay:',I5,
     &             '  SOM2 decay:   ',I6,
     &             '  SOM3 decay:',I7)
                ENDIF  
                IF (ISWWAT.NE.'N') THEN
                  WRITE(fnumov,2090)isoilh2o,rainc/10.0,irramtc/10.0
 2090             FORMAT (' SOIL WATER cm    ','Initial: ',F7.1,
     &             '  Precipitation: ',F5.1,
     &             '  Irrigation: ',F6.1)
                  WRITE(fnumov,2091)runoffc/10.0,drainc/10.0,fsoilh2o
 2091             FORMAT ('                  ','Runoff: ',F8.1,
     &             '  Drainage: ',F10.1,
     &             '  Final:    ',F8.1)
                  WRITE(fnumov,2089)eoc/10.0,eopenc/10.0,eompenc/10.0
 2089             FORMAT (' POTENTIAL ET cm  ','Crop model:',F5.1,
     &              '  Penman:    ',F9.1,
     &              '  Penman-M:  ',F7.1)
                  WRITE(fnumov,2097)eoptc/10.0,eoebudc/10.0
 2097             FORMAT ('                  ','Priestley: ',F5.1,
     &            '  E.budget:  ',F9.1)
                ENDIF
              ENDIF  ! End of Overview header writes
              WRITE(FNUMOV,9589)
              WRITE(fnumov,*)' '
              WRITE(fnumov,'(A11,I4,A3,A60)')
     &         ' RUN NO.   ',RUN,'  ',ENAME
              IF (DYNAMIC.EQ.SEASEND) THEN
                WRITE(fnumov,*)' '
                WRITE(fnumov,'(A50,A25)')
     &          ' NB. RUN TERMINATED PREMATURELY (PROBABLY BECAUSE ',
     &          'OF MISSING WEATHER DATA) '
              ENDIF
              WRITE(fnumov,9588)
              WRITE(fnumov,9600)
              DO L = 1, PSNUM
                CALL Csopline(laic,laistg(l))
                IF (STGYEARDOY(L).LT.9999999.AND.
     &              L.NE.10.AND.L.NE.11) THEN
!                  CALL CSYR_DOY(STGYEARDOY(L),YEAR,DOY)
                  CALL YR_DOY(STGYEARDOY(L),YEAR,DOY)
                  CALL Calendar(year,doy,dom,month)
                  CNCTMP = 0.0
                  IF (CWADSTG(L).GT.0.0)
     &             CNCTMP = CNADSTG(L)/CWADSTG(L)*100
                  WRITE (FNUMOV,'(I8,I4,1X,A3,I4,1X,I1,1X,A13,I6,A6,
     &            F6.1,I6,F6.2,F6.2,F6.2)')
     &            STGYEARDOY(L),DOM,MONTH,
     &            Dapcalc(stgyeardoy(L),(plyeardoy/1000),plday),
     &            l,psname(l),
     &            NINT(CWADSTG(L)),LAIC,LNUMSTG(L),
     &            NINT(CNADSTG(L)),CNCTMP,
     &            1.0-WFPPAV(L-1),1.0-NFPPAV(L-1)
                ENDIF
              ENDDO
              ! For harvest at specified date
              IF (YEARDOYHARF.EQ.YEARDOY) THEN
                CALL Csopline(laic,lai)
!                  CALL CSYR_DOY(YEARDOYHARF,YEAR,DOY)
                  CALL YR_DOY(YEARDOYHARF,YEAR,DOY)
                  CALL Calendar(year,doy,dom,month)
                  CNCTMP = 0.0
                  IF (CWAD.GT.0.0)CNCTMP = CNAD/CWAD*100
                  WRITE (FNUMOV,'(I8,I4,1X,A3,I4,1X,I1,1X,A13,I6,A6,
     &            F6.1,I6,F6.2,F6.2,F6.2)')
     &            YEARDOY,DOM,MONTH,
     &            Dapcalc(yeardoy,(plyeardoy/1000),plday),
     &            l,'Harvest      ',
     &            NINT(CWAD),LAIC,LNUM,
     &            NINT(CNAD),CNCTMP,
     &            1.0-WFPPAV(MSTG-1),1.0-NFPPAV(MSTG-1)
              ENDIF 
              IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
               WRITE(fnumov,*)' '
               WRITE(fnumov,*)
     &          'BIOMASS  = Above-ground dry weight (Excl.seed;kg/ha)'
               WRITE(fnumov,*)'LEAF AREA  = Leaf area index (m2/m2)'
               WRITE(fnumov,*)
     &          'LEAF NUMBER  = Leaf number produced on main axis'
               WRITE(fnumov,*)
     &          'CROP N  = Above-ground N (Excl.seed;kg/ha)'
               WRITE(fnumov,*)
     &          'CROP N%  = Above-ground N concentration(%)'
               WRITE(fnumov,*)
     &         'H2O STRESS = ',
     &         'Photosynthesis stress, prior to stage (0=none,1=max)'
               WRITE(fnumov,*)
     &         'N STRESS = ',
     &         'Photosynthesis stress, prior to stage (0=none,1=max)'
              ENDIF
              WRITE(fnumov,*)' '
              WRITE (FNUMOV,206)
              WRITE (FNUMOV,290) MAX(-99,gdap),MAX(-99,gdapm),
     A         MAX(-99,edap),MAX(-99,edapm)
              DO L = 1,KEYSTX
                IF (KEYPS(L).GT.0) THEN
                  IF (psdap(keyps(l)).LT.-1) EXIT
                  WRITE (FNUMOV,291)
     &             psname(KEYPS(L)),PSDap(KEYPS(L)),PSDapm(KEYPS(L))
                ENDIF                 
              ENDDO
              ! For harvest at specified date
              IF (YEARDOYHARF.EQ.YEARDOY) THEN
                tvi1 = Dapcalc(yeardoy,(plyeardoy/1000),plday)
                WRITE (FNUMOV,292)tvi1,tvi1
              ENDIF
              WRITE (FNUMOV,305)
     H         NINT(cwam),NINT(cwamm),
     I         MAX(-99,NINT(rwam+sdwam)),NINT(rwamm),
     J         NINT(senwacm),NINT(senwacmm),
     L         NINT(hwam),NINT(hwamm),
     M         NINT(vwam),NINT(vwamm),
     N         hiam,hiamm,
     O         NINT(rswam),NINT(rswamm)
              IF (lwphc+swphc.GT.0.0) WRITE (FNUMOV,3051)
     &         NINT(cwahc),NINT(cwahcm)
              WRITE (FNUMOV,3052)
     A         hwumchar,hwummchar,
     B         NINT(hnumam),NINT(hnumamm),
     C         hnumgm,hnumgmm,
     E         laix,laixm,
     F         lnumsm,lnumsmm,
     G         nupac,nupacm,
     H         cnam,cnamm,
     I         rnam,rnamm,
     J         sennatc,sennatcm,
     K         hnam,hnamm,
     L         vnam,vnamm,
     M         hinm,hinmm,
     N         hnpcm,hnpcmm,
     O         vnpcm,vnpcmm
  290         FORMAT (                                                
     &           6X, 'Germination  (dap)          ',6X,I7,  4X,I7,  /,
     A           6X, 'Emergence    (dap)          ',6X,I7,  4X,I7    )
  291         FORMAT(                                                
     &           6X,A13, '(dap)          '         ,6X,I7  ,4X,I7    )
  292         FORMAT(                                                
     &           6X, 'Harvest      (dap)          ',6X,I7  ,4X,I7    )
  305         FORMAT(                                                
     &           6X, 'AboveGround (kg dm/ha)      ',6X,I7,  4X,I7,  /,
     I           6X, 'Roots+seed residue (kg dm/ha)',5X,I7, 4X,I7,  /,
     J           6X, 'Senesced (kg dm/ha)         ',6X,I7,  4X,I7,  /,
     L           6X, 'Product (kg dm/ha)          ',6X,I7,  4X,I7,  /,
     K          6X, 'AboveGroundVegetative (kg dm/ha)  ',I7,4X,I7,  /,
     N           6X, 'HarvestIndex (ratio)        ',6X,F7.2,4X,F7.2,/,
     O           6X, 'Reserves (kg dm/ha)         ',6X,I7,  4X,I7)
 3051         FORMAT(                                               
     &           6X, 'Removed canopy (kg dm/ha)   ',7X,I6,  5X,I6  )
 3052         FORMAT(                                                
     &           6X, 'Product unit wt (g dm)      ',7X,A6,  5X,A6,  /,
     A           6X, 'Product number (/m2)        ',6X,I7,  4X,I7,  /,
     B           6X, 'Product number (/shoot)     ',6X,F7.1,4X,F7.1,/,
     D           6X, 'Maximum leaf area index     ',6X,F7.1,4X,F7.1,/,
     E           6X, 'Final leaf number (one axis)',6X,F7.1,4X,F7.1,/,
     F           6X, 'Assimilated N (kg/ha)       ',6X,F7.1,4X,F7.1,/,
     G           6X, 'AboveGround N (kg/ha)       ',6X,F7.1,4X,F7.1,/,
     H           6X, 'Root N (kg/ha)              ',6X,F7.1,4X,F7.1,/,
     I           6X, 'Senesced N (kg/ha)          ',6X,F7.1,4X,F7.1,/,
     J           6X, 'Product N (kg/ha)           ',6X,F7.1,4X,F7.1,/,
     K         6X, 'AboveGroundVegetative N (kg/ha)  ',F8.1,4X,F7.1,/,
     L           6X, 'N HarvestIndex (ratio)      ',6X,F7.2,4X,F7.2,/,
     M           6X, 'Product N (% dm)            ',6X,F7.1,4X,F7.1,/,
     N         6X, 'AboveGroundVegetative N (% dm)    ',F7.1,4X,F7.1)
              WRITE(fnumov,500)
              PFPPAV = -99.0
              PFGPAV = -99.0
              DO tvI1 = 1,mstg-2
                IF (pdays(tvi1).GT.0) THEN 
                WRITE(fnumov,600) psname(tvi1),dash,psname(tvi1+1), 
     &          pdays(tvI1),tmaxpav(tvI1),tminpav(tvI1),sradpav(tvI1),
     &          daylpav(tvI1),rainpc(tvI1),etpc(tvI1),1.-wfppav(tvi1),
     &          1.0-wfgpav(tvi1), 1.0-nfppav(tvi1), 1.0-nfgpav(tvi1), 
     &          pfppav(tvi1), pfgpav(tvi1)
                ENDIF
              ENDDO
  600         FORMAT(1X,A10,A3,A10,I5,3F6.1,F7.2,2F7.1,4F7.3,2F7.2)
  610         FORMAT(1X,A10,13X,I5,3F6.1,F7.2,2I7,6F7.3)
              IF(yeardoyharf.EQ.yeardoy)THEN
                WRITE(fnumov,600) psname(mstg-1),dash,'Harvest   ', 
     &          pdays(mstg-1),tmaxpav(mstg-1),tminpav(mstg-1),
     &          sradpav(mstg-1),daylpav(mstg-1),rainpc(mstg-1),
     &          etpc(mstg-1),1.-wfppav(mstg-1),1.0-wfgpav(mstg-1), 
     &          1.0-nfppav(mstg-1),1.0-nfgpav(mstg-1), 
     &          pfppav(mstg-1),pfgpav(mstg-1)
              ELSE 
                WRITE(fnumov,600) psname(mstg-1),dash,psname(mstg), 
     &          pdays(mstg-1),tmaxpav(mstg-1),tminpav(mstg-1),
     &          sradpav(mstg-1),daylpav(mstg-1),rainpc(mstg-1),
     &          etpc(mstg-1),1.-wfppav(mstg-1),1.0-wfgpav(mstg-1), 
     &          1.0-nfppav(mstg-1),1.0-nfgpav(mstg-1), 
     &          pfppav(mstg-1),pfgpav(mstg-1)
              ENDIF
              IF (pdays(mstg).GT.0.OR.yeardoyharf.EQ.yeardoy) THEN 
              WRITE(fnumov,*) ' '
              pfpcav = -99.0
              pfgcav = -99.0 
              IF (pdays(mstg).GT.0.) THEN 
                WRITE(fnumov,600) psname(1),dash,psname(mstg), 
     &           cdays, tmaxcav, tmincav, sradcav,
     &           daylcav, raincc, etcc, 1.0-wfpcav, 
     &           1.0-wfgcav, 1.0-nfpcav, 1.0-nfgcav,
     &           pfpcav, pfgcav
              ELSE  
                WRITE(fnumov,600) psname(1),dash,'Harvest   ', 
     &           cdays, tmaxcav, tmincav, sradcav,
     &           daylcav, raincc, etcc, 1.0-wfpcav, 
     &           1.0-wfgcav, 1.0-nfpcav, 1.0-nfgcav,
     &           pfpcav, pfgcav
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
              WRITE (FNUMOV, 1200) CDAYS, 
     &         RAINCC, DMP_Rain*0.1, DMP_Rain, GrP_Rain*0.1, GrP_Rain,
     &         ETCC,  DMP_ET*0.1,   DMP_ET,   GrP_ET*0.1,   GrP_ET, 
     &         EPCC,  DMP_EP*0.1,   DMP_EP,   GrP_EP*0.1,   GrP_EP
              IF (IRRAMTC > 1.E-3) THEN
                WRITE(FNUMOV, 1210) 
     &            IRRAMTC, DMP_Irr*0.1, DMP_Irr, GrP_Irr*0.1, GrP_Irr
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
!              WRITE(fnumwrk,*) ' '
!              WRITE(fnumwrk,'(A28,A10,I3)')
!     &          ' OVERVIEW OF CONDITIONS FOR ',
!     &            excode,tn
!              WRITE(fnumwrk,*) ' '
!              WRITE (fnumwrk,209) tmaxx,tmaxm,tminn,tminm              
!              IF (ISWNIT.NE.'N') THEN
!                WRITE(fnumwrk,2095)cnad+rnad+hnad,hnad,vnad
!                WRITE(fnumwrk,2096)sennal(0),sennas            
!                WRITE(fnumwrk,2093)isoiln,amtnit,fsoiln
!                WRITE(fnumwrk,2094)
!     &            tnoxc,tlchc,tominsomc+tominfomc-tnimbsom
!                WRITE(fnumwrk,2099)tnimbsom,tominfomc,tominsomc   
!                IF (tominsom1.GT.0.0)
!     &            WRITE(fnumwrk,2098)NINT(tominsom1c),NINT(tominsom2c),
!     &             NINT(tominsom3c)
!                IF (FILEIOT.EQ.'DS4'.AND.IDETL.EQ.'D'.OR.
!     &             FILEIOT.EQ.'DS4'.AND.IDETL.EQ.'A'.OR.
!     &             FILEIOT.NE.'DS4') THEN                  
!                  WRITE(fnumwrk,2090)isoilh2o,rainc/10.0,irramtc/10.0
!                  WRITE(fnumwrk,2091)runoffc/10.0,drainc/10.0,fsoilh2o
!                  WRITE(fnumwrk,2089)eoc/10.0,eopenc/10.0,eompenc/10.0
!                  WRITE(fnumwrk,2097)eoptc/10.0,eoebudc/10.0
!                ENDIF
!                IF (FAPPNUM.GT.0) THEN
!                  WRITE (fnumwrk,*) ' '
!                  WRITE (fnumwrk,'(A18,A10,I3)')
!     &              ' N FERTILIZER FOR ',excode,tn
!                  DO L = 1,FAPPNUM
!                     WRITE (fnumwrk,'(A80)') FAPPLINE(L)
!                  ENDDO
!                ENDIF
!                WRITE(FNUMWRK,*) ' '
!                WRITE(FNUMWRK,'(A45)')
!     &            ' INORGANIC N (kg/ha) LEFT IN SOIL AT HARVEST '
!                WRITE(FNUMWRK,'(A28,2F6.1)')
!     &           '  NO3 and NH4 N in PROFILE: ',
!     &           SNO3PROFILE,SNH4PROFILE
!                WRITE(FNUMWRK,'(A28,2F6.1)')
!     &           '  NO3 and NH4 N in ROOTZONE:',
!     &           SNO3ROOTZONE,SNH4ROOTZONE
!              ENDIF   ! End Iswnit NE N
!              WRITE(FNUMWRK,*) ' '
!              WRITE(FNUMWRK,'(A34)')
!     &          ' H2O (mm) LEFT IN SOIL AT HARVEST '
!              WRITE(FNUMWRK,'(A36,2F6.1)')
!     &         '  H2O and AVAILABLE H2O in PROFILE: ',
!     &         H2OPROFILE,AH2OPROFILE
!              WRITE(FNUMWRK,'(A36,2F6.1)')
!     &         '  H2O and AVAILABLE H2O in ROOTZONE:',
!     &         H2OROOTZONE,AH2OROOTZONE
!              WRITE (fnumwrk,*) ' '
!              WRITE (fnumwrk,'(A32,A10,I3)')
!     &         ' CRITICAL PERIOD CONDITIONS FOR ',excode,tn
!              WRITE (fnumwrk,'(A38,F6.1)')
!     &         '  Temperature mean,germination         ',TMEANG
!              WRITE (fnumwrk,'(A38,F6.1)')
!     &         '  Temperature mean,germ-emergence      ',TMEANE
!              WRITE (fnumwrk,'(A38,F6.1)')
!     &         '  Temperature mean,first 20 days       ',TMEAN20P
              
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

!-----------------------------------------------------------------------
!         IDETS OUTPUTS (Plantsum)
!-----------------------------------------------------------------------
           
          IF ((IDETS.NE.'N'.AND.IDETL.NE.'0').OR.IDETL.EQ.'A') THEN
          
            ! PLANT SUMMARY (SIMULATED)'
            IF (CROP.NE.CROPPREV.OR.RUN.EQ.1) THEN
              OPEN (UNIT=fnumpsum,FILE=FNAMEPSUM,POSITION='APPEND')
              WRITE (FNUMPSUM,9953)
 9953         FORMAT (/,'*SUMMARY')
              WRITE (FNUMPSUM,99,ADVANCE='NO')
   99         FORMAT ('@  RUN EXCODE    TRNO RN',
     X         ' TNAME....................',
     A        ' REP  RUNI S O C    CR PYEAR  PDAT')
              DO L = 1,KEYSTX
                IF (KEYPS(L).GT.0) THEN
                  WRITE (FNUMPSUM,'(A6)',ADVANCE='NO') PSABVO(KEYPS(L))
                  !IF (PSABVO(KEYPS(L)).EQ.'TSDAP') THEN
                  ! WRITE (FNUMPSUM,'(A6)',ADVANCE='NO') '  DAYL'
                  !ENDIF
                ENDIF
              ENDDO
              WRITE (FNUMPSUM,299)
  299         FORMAT (
     B        '   FLN FLDAP HYEAR  HDAY SDWAP',
     C        ' CWAHC  CWAM PARUE  HWAM  HWAH  VWAM  HWUM  H#AM  H#UM',
     D        ' SDNAP  CNAM  HNAM  RNAM  TNAM  NUCM  HN%M  VN%M',
     E        ' D1INI D2INI D3INI ')
              CLOSE(fnumpsum)  
            ENDIF  ! End of Plantsum.Out headers
            OPEN (UNIT=fnumpsum,FILE=FNAMEPSUM,POSITION='APPEND')
            WRITE (fnumpsum,400,ADVANCE='NO') run,excode,tn,rn,tname,
     A        rep,runi,sn,on,cn,crop,
     B        plyear,plday
  400       FORMAT (I6,1X,A10,I4,I3,1X,A25,
     A       I4,I6,I2,I2,I2,4X,A2,
     B       I6,I6)
            DO L = 1,KEYSTX
              IF (KEYPS(L).GT.0) THEN
                WRITE (FNUMPSUM,'(I6)',ADVANCE='NO') PSDAP(KEYPS(L))
                IF (PSABVO(KEYPS(L)).EQ.'TSDAP') THEN
                  WRITE (FNUMPSUM,'(F6.1)',ADVANCE='NO') DAYLST(L)
                ENDIF  
              ENDIF
            ENDDO
            WRITE (fnumpsum,401)FLN, FLDAP,
     &        hayear,hadoy,
     C        NINT(sdrate),NINT(cwahc),
     D        NINT(cwam),pariued,NINT(hwam),
     E        NINT(hwam*hpcf/100.0),NINT(vwam),
     F        hwumchar,NINT(hnumam),NINT(hnumgm),
     G        sdnap,NINT(cnam),NINT(hnam),NINT(rnam),
     H        NINT(AMAX1(-99.0,cnam+rnam)),NINT(nupac),
     I        hnpcmchar,vnpcmchar,
     J        didoy(1),didoy(2),didoy(3)
  401       FORMAT (
     &       F6.1,I6, 
     B       I6,I6,
     C       I6,I6,
     D       I6,F6.1,I6,
     E       I6,I6,
     F       A6,I6,I6,
     G       F6.1,I6,I6,I6,
     H       I6,I6,
     I       2A6,
     J       3I6)
            CLOSE(fnumpsum)  
          ELSE  
            OPEN (UNIT=FNUMPSUM,FILE=FNAMEPSUM,STATUS='UNKNOWN')
            CLOSE (UNIT=FNUMPSUM, STATUS = 'DELETE')
          ENDIF
          ! End IDETS Outputs (Plantsum.Out)          

!-----------------------------------------------------------------------
!         IDETL = Y or D OUTPUTS (Leaves,Tiers)
!-----------------------------------------------------------------------

          IF (IDETL.EQ.'Y'.OR.IDETL.EQ.'D'.OR.IDETL.EQ.'A') THEN
          
            ! LEAVES.OUT
            OPEN(UNIT=FNUMLVS,FILE=FNAMELEAVES,POSITION='APPEND')
            WRITE (FNUMLVS,'(/,A79,/)') OUTHED
            WRITE (FNUMLVS,'(A14,F6.1)') '! LEAF NUMBER ',LNUM
            WRITE (FNUMLVS,'(/,A48,A36,A30)')
     &       '@ LNUM AREAP AREA1 AREA2 AREA3 AREA4 AREAT AREAS',
     &       '  WFLF  NFLF NFLF2  AFLF TFGLF TFDLF',
     &       ' LLIFG LLIFA LLIFS LLIFE   DAP'
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
              WRITE (fnumlvs,'(I6,7A6,6F6.2,4F6.1,I6)')
     &          I,LAPOTXC,
     &          LATLC,LATL2C,LATL3C,LATL4C,LAPC,LAPSC,
     &          1.0-WFLF(I),
     &          1.0-NFLF(I),1.0-NFLF2(I),
     &          1.0-AMAX1(0.0,AMIN1(1.0,AFLF(I))),
     &          1.0-TFGLF(I),1.0-TFDLF(I),
     &          DGLF(I),DALF(I),DSLF(I),DGLF(I)+DALF(I)+DSLF(I),
     &          LDEATHDAP(I)
            ENDDO
            IF (RUN.EQ.1) THEN
              WRITE(fnumlvs,*)' '
              WRITE(fnumlvs,'(A43)')
     &         '! NB. Data are summed over all fork branches'
              WRITE(fnumlvs,*)' '
              WRITE(fnumlvs,'(A36)')
     &          '! LNUM = Number of leaf on one axis '
              WRITE(fnumlvs,'(A52)')
     &          '! AREAP = Potential area of leaf on main axis (cm2) '
              WRITE(fnumlvs,'(A41,A26)')
     &          '! AREA1 = Area of youngest mature leaf on', 
     &          ' main axis,no stress (cm2)'
              WRITE(fnumlvs,'(A42,A16)')
     &          '! AREAT = Area of cohort of leaves at leaf',
     &          ' position (cm2) '
              WRITE(fnumlvs,'(A43,A35)')
     &          '! AREAS = Senesced area of cohort of leaves',
     &          ' at harvest at leaf position (cm2) '
              WRITE(fnumlvs,'(A38,A17)')
     &          '! WFLF  = Water stress factor for leaf',
     &          ' (0-1,1=0 stress)'
              WRITE(fnumlvs,'(A51)')
     &          '! NFLF  = N stress factor for leaf (0-1,1=0 stress)'
              WRITE(fnumlvs,'(A51)')
     &         '! NFLF  = N factor for area adjustment (0-1,1=0 stress)'
              WRITE(fnumlvs,'(A44,A17)')
     &          '! NFLFP = N stress factor for photosynthesis',
     &          ' (0-1,1=0 stress)'
              WRITE(fnumlvs,'(A36,A24)')
     &          '! AFLF  = Assimilate factor for leaf',
     &          ' (0-1,1=0 no limitation)'
              WRITE(fnumlvs,'(A48,A24)')
     &          '! TFGLF = Temperature factor for leaf expansion ',
     &          ' (0-1,1=0 no limitation)'
              WRITE(fnumlvs,'(A49,A24)')
     &          '! TFDLF = Temperature factor for leaf development',
     &          ' (0-1,1=0 no limitation)'
              WRITE(fnumlvs,'(A37)')
     &          '! DGLF = Number of days growing      '
              WRITE(fnumlvs,'(A37)')
     &          '! DALF = Number of days fully active '
              WRITE(fnumlvs,'(A37)')
     &          '! DSLF = Number of days senescing    '
            ENDIF
            CLOSE (FNUMLVS)
            ! End of Leaves.out
          
            ! Branching tier conditions (Simulated;TIERS.OUT)
            OPEN(UNIT=FNUMPHA,FILE=FNAMEPHASES,POSITION='APPEND')
            WRITE (FNUMPHA,'(/,A79,/)') OUTHED
            WRITE (fnumpha,'(A42,A24,A12)')
     &       '@ TIER SRADA  TMXA  TMNA  PREA  TWLA  CO2A',
     &       '  WFPA  WFGA  NFPA  NFGA',
     &       '  TIER_END  '
            DO L=1,MSTG-2
              IF (STGYEARDOY(L).LT.9999999.AND.
     &         L.NE.0.AND.L.NE.10.AND.L.NE.11)
     &         WRITE (fnumpha,'(I6,3F6.1,2F6.2,I6,4F6.2,1X,A13)')
     &         L,sradpav(L),tmaxpav(L),tminpav(L),
     &         rainpav(L),daylpav(L),NINT(co2pav(L)),
     &         1.0-wfppav(L),1.0-wfgpav(L),
     &         1.0-nfppav(L),1.0-nfgpav(L),
     &         psname(MIN(L+1,PSX))
            ENDDO
            IF(yeardoyharf.EQ.yeardoy)THEN
               WRITE (fnumpha,'(I6,3F6.1,2F6.2,I6,4F6.2,1X,A13)')
     &         mstg-1,sradpav(mstg-1),tmaxpav(mstg-1),tminpav(mstg-1),
     &         rainpav(mstg-1),daylpav(mstg-1),NINT(co2pav(mstg-1)),
     &         1.0-wfppav(mstg-1),1.0-wfgpav(mstg-1),
     &         1.0-nfppav(mstg-1),1.0-nfgpav(mstg-1),
     &         'Harvest      '
              ELSE 
               WRITE (fnumpha,'(I6,3F6.1,2F6.2,I6,4F6.2,1X,A13)')
     &         mstg-1,sradpav(mstg-1),tmaxpav(mstg-1),tminpav(mstg-1),
     &         rainpav(mstg-1),daylpav(mstg-1),NINT(co2pav(mstg-1)),
     &         1.0-wfppav(mstg-1),1.0-wfgpav(mstg-1),
     &         1.0-nfppav(mstg-1),1.0-nfgpav(mstg-1),
     &         psname(mstg)
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
            WRITE(Message(1),'(A35)')
     &       'IDETL flag called for detail files.'
            WRITE(Message(2),'(A31,A31)')
     &       'But IDETO flag set at N so that',
     &       'measured data not read.        '
            WRITE(Message(3),'(A45)')
     &       'Therefore,could not write detailed summaries.'
            CALL WARNING(3,'CSCAS',MESSAGE)
          ENDIF
          
!-----------------------------------------------------------------------
!         IDETL = D OUTPUTS (Work details;Phenols,m;Plantres,m)
!-----------------------------------------------------------------------
           
          IF ((IDETL.EQ.'D'.AND.IDETO.NE.'N').OR.IDETL.EQ.'A') THEN
                  
            ! WORK
!            WRITE(fnumwrk,*) ' '
!            WRITE(fnumwrk,'(A26,A10,I3)')' HARVEST/FAILURE DATA FOR ',
!     &       excode,tn
!            WRITE(fnumwrk,*)' '
!            IF (DYNAMIC.EQ.SEASEND .AND. SEASENDOUT.NE.'Y') THEN
!              WRITE(fnumwrk,*)  ' Program terminated      ',YEARDOY
!            ELSE 
!              WRITE(fnumwrk,*)  ' Harvest reached         ',YEARDOY
!            ENDIF  
!            WRITE (fnumwrk,*)' '
!            WRITE (fnumwrk,'(A54,F5.1,F4.1)')
!     &       '  Overall PAR use efficientcy(incident,intercepted) = ',
!     &       paruec,pariued
!            WRITE(fnumwrk,*) ' '
!            WRITE(fnumwrk,'(A27,F11.2)')'  Harvest product (kg/ha)  ',
!     &       HWAM
!            WRITE(fnumwrk,'(A27,F11.2)')'  Product/Total wt (HI)    ',
!     &       HIAM
     
!            WRITE(fnumwrk,*) ' '
!            WRITE(fnumwrk,'(A26,A10,I3)')
!     &       ' CH2O BALANCE (kg/ha) FOR ',excode,tn
!            WRITE(fnumwrk,'(A27,3F11.1)')
!     &       '  SEED+FIXED (1) Seed,fixed',
!     &       (SEEDRSI+SDCOAT+CARBOC)*PLTPOP*10.0,
!     &       (SEEDRSI+SDCOAT)*PLTPOP*10.0,CARBOC*PLTPOP*10.0
             TVR1 = (SEEDRSI+SDCOAT+CARBOC)*PLTPOP*10.0
!            WRITE(fnumwrk,'(A27,3F11.1)')
!     &       '  RESPIRED (2)  Tops,root  ',RESPC*PLTPOP*10.0,
!     &       RESPTC*PLTPOP*10.0,RESPRC*PLTPOP*10.0
             TVR2 = RESPC*PLTPOP*10.0
!            WRITE(fnumwrk,'(A27,3F11.1)')
!     &       '  SENESCED (3)  Tops,root  ',
!     &       (SENTOPLITTER+SENROOT)*PLTPOP*10.0,
!     &       SENTOPLITTER*PLTPOP*10.0,SENROOT*PLTPOP*10.0
             TVR3 = (SENTOPLITTER+SENROOT)*PLTPOP*10.0
             TVR4 = (SEEDRS+SDCOAT+RTWT+SRWT+LFWT+STWT+CRWT+RSWT
     &        )*PLTPOP*10.0
!            WRITE(fnumwrk,'(A27,3F11.1)')
!     &       '  PLANT+SEED_RESIDUE Pl,sd ',
!     &       (SEEDRS+SDCOAT+RTWT+SRWT+LFWT+STWT+CRWT+RSWT)
!     &       *PLTPOP*10.0,
!     &       (RTWT+SRWT+LFWT+STWT+CRWT+RSWT)*PLTPOP*10.0,
!     &       (SEEDRS+SDCOAT)*PLTPOP*10.0
!            WRITE(fnumwrk,'(A27,2F11.1)')
!     &       '  RESERVES (5)             ',RSWT*PLTPOP*10.0 
             TVR5 = RSWT*PLTPOP*10.0
!            WRITE(fnumwrk,'(A29, F9.1)')
!     &       '  HARVESTED DURING CYCLE (6) ',
!     &       (LWPHC+SWPHC+RSWPHC)*PLTPOP*10.0
             TVR6 = (LWPHC+SWPHC+RSWPHC)*PLTPOP*10.0
!            WRITE(fnumwrk,'(A27, F11.2)')
!     &       '  BALANCE (1-(2+3+4+6))    ',TVR1 -(TVR2+TVR3+TVR4+TVR6)
!            IF (ABS(TVR1-(TVR2+TVR3+TVR4+TVR6)).GT.0.05)
!     &      WRITE(fnumwrk,'(A29,A10,A1,I2)')
!     &       '  *PROBLEM WITH CH2O BALANCE ',EXCODE,' ',TN

!            WRITE (fnumwrk,*) ' '
!            WRITE (fnumwrk,'(A21,A10,I3)')
!     &       ' RESERVES STATUS FOR ',excode,tn
!            WRITE (fnumwrk,'(A22,F7.1)')
!     &       '  Kg/ha at maximum    ',RSWTX*PLTPOP*10.0
!            WRITE (fnumwrk,'(A22,F7.1)')
!     &       '  % above ground      ',RSCX*100.
!            WRITE (fnumwrk,'(A22,F7.1)')
!     &       '  Kg/ha at harvest    ',RSWAD
!            IF (lfwt+stwt+crwt+rswt.GT.0) WRITE (fnumwrk,'(A22,F7.1)')
!     &       '  % above ground      ',rswt/(lfwt+stwt+crwt+rswt)*100.0
!            WRITE (fnumwrk,*) ' '
!            WRITE (fnumwrk,'(A34,A10,I3)')
!     &       ' SEED USE (KG/HA or PER CENT) FOR ',excode,tn
!            WRITE (fnumwrk,'(A22,F7.3)')'  Initial reserves    ',
!     &       seedrsi*pltpop*10.0
!            WRITE (fnumwrk,'(A22,F7.3)')'  Use for tops        ',
!     &       seeduset*pltpop*10.0
!            WRITE (fnumwrk,'(A22,F7.3)')'  Use for roots       ',
!     &       seeduser*pltpop*10.0
!            WRITE (fnumwrk,'(A22,F7.3)')'  Total use           ',
!     &       (seeduset+seeduser)*pltpop*10.0
!            IF (seeduser+seeduset.GT.0.0)
!     &       WRITE (fnumwrk,'(A22,F7.3)')'  Percent to tops     ',
!     &        seeduset/(seeduset+seeduser)*100.0
!            WRITE(fnumwrk,*)' '
!            WRITE (fnumwrk,'(A35,A10,I3)')
!     &       ' DEAD MATTER AND ROOTS (KG/HA) FOR ',excode,tn
!            WRITE(fnumwrk,'(A32,F8.1)')
!     &       '  DEAD MATERIAL LEFT ON SURFACE  ',SENTOPLITTERA
!            WRITE(fnumwrk,'(A32,F8.1)')
!     &       '  DEAD MATERIAL LEFT IN SOIL     ',SENROOTA
!            WRITE(fnumwrk,'(A32,F8.1)')
!     &       '  ROOT WEIGHT AT HARVEST/FAILURE ',RWAD
!            WRITE (fnumwrk,*) ' '
!            WRITE (fnumwrk,'(A20,A10,I3)')
!     &       ' ROOTS BY LAYER FOR ',excode,tn
!            WRITE (fnumwrk,'(A19)')
!     &       '  LAYER  RTWT   RLV'
!            DO L=1,NLAYR
!              IF (RTWTAL(L).GT.0.0) WRITE (fnumwrk,'(I6,F7.1,F6.2)')
!     &        L,RTWTAL(L),RLV(L)
!            ENDDO
!            IF (RTSLXDATE.GT.0) THEN
!              WRITE(fnumwrk,'(A30,I7)')
!     &         '  FINAL SOIL LAYER REACHED ON ',RTSLXDATE
!              WRITE(fnumwrk,'(A15,I7,A1)')
!     &         '  (MATURITY ON ',YEARDOY,')'
!            ELSE  
!             WRITE(fnumwrk,*)' FINAL SOIL LAYER NOT REACHED '
!            ENDIF
!            WRITE (fnumwrk,*) ' '
!            WRITE (fnumwrk,'(A40)')
!     &       ' PRINCIPAL AND SECONDARY STAGES         '
!            WRITE (fnumwrk,'(A40)')
!     &       '  STAGE NAME   DAYS > PLANTING  LEAF #  '
!            WRITE (fnumwrk,'(A15,F7.1)')
!     &       '   Germination ',gdapfr
!            WRITE (fnumwrk,'(A15,F7.1)')
!     &       '   Emergence   ',edapfr
            DO L = 2,PSNUM
              CALL CSUCASE (PSNAME(L))
              IF (PSNAME(L)(1:3).EQ.'HAR'.AND.PSDAPFR(l).LE.0.0)
     &         psdapfr(l) = psdapfr(mstg)
              IF (PSNAME(L)(1:3).EQ.'END'.AND.PSDAPFR(l).LE.0.0)
     &         psdapfr(l) = psdapfr(mstg)
              IF (PSNAME(L)(1:3).NE.'FAI') THEN
!                IF (psdapfr(l).GT.0) 
!     &            WRITE (FNUMWRK,'(A3,A13,F6.1,9X,F6.1)')
!     &            '   ',psname(l),psdapfr(l),lnumsimtostg(l)
              ELSE
!                IF (CFLFAIL.EQ.'Y'.AND.psdapfr(l).GT.0)
!     &           WRITE (FNUMWRK,'(A3,A13,F6.1)')
!     &             '   ',psname(l),psdapfr(l)
              ENDIF
              IF (TVILENT(PSNAME(L)).LT.5) EXIT
            ENDDO
!            WRITE (fnumwrk,*) ' '
!            WRITE (fnumwrk,'(A28,A10,I3)')
!     &       ' STRESS FACTOR AVERAGES FOR ',excode,tn
!            WRITE (fnumwrk,'(A55)')
!     &       '  TIER   H2O(PS)   H2O(GR)   N(PS)     N(GR)   TIER_END'
!            DO L=1,MSTG-2 
!               WRITE (fnumwrk,'(I6,F8.2,3F10.2,2X,A13)')
!     &         l,1.0-wfppav(l),1.0-wfgpav(l),
!     &         1.0-nfppav(l),1.0-nfgpav(l),psname(MIN(L+1,PSX))
!            ENDDO
!            IF(yeardoyharf.EQ.yeardoy)THEN
!              WRITE (fnumwrk,'(I6,F8.2,3F10.2,2X,A13)')
!     &        l,1.0-wfppav(l),1.0-wfgpav(l),
!     &        1.0-nfppav(l),1.0-nfgpav(l),'HARVEST      '
!              ELSE 
!               WRITE (fnumwrk,'(I6,F8.2,3F10.2,2X,A13)')
!     &         l,1.0-wfppav(mstg-1),1.0-wfgpav(mstg-1),
!     &         1.0-nfppav(mstg-1),1.0-nfgpav(mstg-1),psname(mstg)
!              ENDIF
!            WRITE (fnumwrk,'(A42)')
!     &       '  NB 0.0 = minimum ; 1.0 = maximum stress.'
            ! LAH  Must change from daily leaf cohorts to bigger unit
            ! Too much to output if daily
            !WRITE (fnumwrk,*)' '
            !WRITE (fnumwrk,'(A23)') ' COHORT   AREA    AREAS'
            !DO L = 1, LCNUM
            !  WRITE (fnumwrk,'(I7,2F8.3)') L,LCOA(L),LCOAS(L)
            !ENDDO
!            WRITE (fnumwrk,*) ' '
!            WRITE (fnumwrk,'(A24,A10,I3)')
!     &       ' SHOOT SIZES (cm2) FOR  ',excode,tn
!            WRITE (fnumwrk,'(A32)') 
!     &       '   SHOOT  BIRTH   AREAP   AREAS '
            DO I = 1,INT(SHNUM)
!              WRITE (fnumwrk,'(I7,I8,2I8)')
!     &          I,SHDAT,NINT(SHLA(I)),NINT(SHLAS(I))
            ENDDO
            IF (ISWNIT.NE.'N') THEN
!            WRITE (fnumwrk,*) ' '
!            WRITE (fnumwrk,'(A25,A10,I3)')' N BALANCE (kg/ha) FOR ',
!     &       excode,tn
!            WRITE (fnumwrk,'(A34,F8.2,2F11.2)')
!     &       '   N UPTAKE + SEED (1)            ', 
!     &       (NUPC+SEEDNI)*PLTPOP*10.0,NUPC*PLTPOP*10.,SEEDNI*PLTPOP*10.
             TVR1 = (NUPC+SEEDNI)*PLTPOP*10.0  
!            WRITE (fnumwrk,'(A33,F9.2,2F11.2)')
!     &       '   TOTAL N SENESCED (2) Tops,Root',
!     &       (SENNL(0)+SENNS)*PLTPOP*10.0,SENNL(0)*PLTPOP*10.0,
!     &       SENNS*PLTPOP*10.0
             TVR2 = (SENNL(0)+SENNS)*PLTPOP*10.0 
!            WRITE (fnumwrk,'(A34,F8.2)')
!     &       '   TOTAL N IN PLANT (3)           ',PLTPOP*10.0*
!     &       (ROOTN+SROOTN+LEAFN+STEMN+RSN+SEEDN)
             TVR3 = (ROOTN+SROOTN+LEAFN+STEMN+RSN+SEEDN)*
     &              PLTPOP*10.0         
!            WRITE (fnumwrk,'(A33, F9.2)')
!     &       '   HARVESTED DURING CYCLE (4)    ',PLTPOP*10.0*
!     &       LNPHC+SNPHC+RSNPHC
             TVR4 = (LNPHC+SNPHC+RSNPHC)* PLTPOP*10.0             
!            WRITE (fnumwrk,'(A34,F8.3)')
!     &       '   BALANCE (1-(2+3+4))            ',TVR1-TVR2-TVR3-TVR4
!            IF (ABS(TVR1-(TVR2+TVR3+TVR4)).GT.0.005)
!     &      WRITE(fnumwrk,'(A26,A10,A1,I2)')
!     &       '  *PROBLEM WITH N BALANCE ',EXCODE,' ',TN
            ENDIF
            ! End of Detailed WORK writes
    
            ! Phenology (Simulated;PHENOLS.OUT)
            INQUIRE (FILE = FNAMEPHENOLS,EXIST = FFLAG)
            OPEN(UNIT=FNUMPHES,FILE=FNAMEPHENOLS,POSITION='APPEND')
            IF (CROP.NE.CROPPREV.OR.RUN.EQ.1.OR.(.NOT.(FFLAG))) THEN
              WRITE (FNUMPHES,'(/,A14,A10)')
     &         '*PHENOLOGY(S):',EXCODE
              WRITE (FNUMPHES,'(A16,A24)',ADVANCE='NO') 
     &         '@ EXCODE    TRNO',' PYEAR  PDAT  GDAP  EDAP'
              DO L = 1,KEYSTX
                IF (KEYPS(L).GT.0)THEN
                  WRITE (FNUMPHES,'(A6)',ADVANCE='NO') PSABVO(KEYPS(L))
                ENDIF
              ENDDO
              WRITE (fnumphes,'(/,A10,I6,2I6,2F6.1)',ADVANCE='NO')
     &        EXCODE,TN,PLYEAR,PLDAY,gdapfr,edapfr
            ELSE  ! End Phenology simulated header writes
              WRITE (fnumphes,'(A10,I6,2I6,2F6.1)',ADVANCE='NO')
     &        EXCODE,TN,PLYEAR,PLDAY,gdapfr,edapfr
            ENDIF
            DO L = 1,KEYSTX
              IF (KEYPS(L).GT.0) WRITE (FNUMPHES,'(I6)',ADVANCE='NO')
     &         PSDAP(KEYPS(L))
            ENDDO
            CLOSE (FNUMPHES)
            ! End Phenology simulated writes              
            
            ! Phenology (Measured;PHENOLM.OUT)
            IF (TDATANUM.LE.0 .AND. .NOT.FEXISTA) THEN
!              WRITE (fnumwrk,*)' '
!              WRITE (fnumwrk,*)
!     &          ' No data so cannot write PHENOLOGY (MEASURED)'
              OPEN (UNIT=FNUMPHEM,FILE=FNAMEPHENOLM,STATUS ='UNKNOWN')
              CLOSE (UNIT=FNUMPHEM, STATUS = 'DELETE')
            ELSE
              INQUIRE (FILE = FNAMEPHENOLM,EXIST = FFLAG)
              OPEN(UNIT=FNUMPHEM,FILE=FNAMEPHENOLM,POSITION='APPEND')
              IF (CROP.NE.CROPPREV.OR.RUN.EQ.1.OR.(.NOT.(FFLAG))) THEN
                WRITE (FNUMPHEM,'(/,A14,A10)')
     &            '*PHENOLOGY(M):',EXCODE
                WRITE (FNUMPHEM,'(A16,A24)',ADVANCE='NO') 
     &            '@EXCODE     TRNO',' PYEAR  PDAT  GDAP  EDAP'
                DO L = 1,KEYSTX
                  IF (KEYPS(L).GT.0) THEN
                    WRITE (FNUMPHEM,'(A6)',ADVANCE='NO')PSABVO(KEYPS(L))
                  ENDIF 
                ENDDO
                WRITE (FNUMPHEM,'(/,A10,I6,2I6,2F6.1)',ADVANCE='NO')
     &           EXCODE,TN,PLYEAR,PLDAY,gdapfr,edapfr
              ELSE ! End Phenology measured header writes
                WRITE (FNUMPHEM,'(A10,I6,2I6,2F6.1)',ADVANCE='NO')
     &           EXCODE,TN,PLYEAR,PLDAY,gdapfr,edapfr
              ENDIF
              DO L = 1,KEYSTX
                IF (KEYPS(L).GT.0) WRITE (FNUMPHEM,'(I6)',ADVANCE='NO')
     &           PSDAPM(KEYPS(L))
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
 9951         FORMAT ('*RESPONSES(S):',A10,'  ',A8)
              IF (TNAME(1:1).EQ.'*') THEN
                WRITE (FNUMPRES,'(A180)') TLINETMP
              ELSE
                WRITE (FNUMPRES,'(A180)') TLINETMP
              ENDIF
              WRITE (FNUMPRES,97,ADVANCE='NO')
   97         FORMAT ('@  RUN',
     A        ' EXCODE   ',
     B        ' TRNO RN    CR',
     D        '  PDAT  EDAP')
              DO L = 1,KEYSTX
                IF (KEYPS(L).GT.0) THEN
                  WRITE (FNUMPRES,'(A6)',ADVANCE='NO') PSABVO(KEYPS(L))
                ENDIF  
              ENDDO
              WRITE (FNUMPRES,297)
  297         FORMAT (
     G        '  HWAH  HWUH',
     H        '  H#AH  H#GH  LAIX  L#SH BR#AH',
     I        '  CWAH  VWAH  HIAH  RWAH',
     J        '  HN%H  TNAH',
     K        '  CNAH  HNAH',
     L        '  HINH PLPOP',
     F        '  NICH',
     M        ' SRADA TMAXA TMINA  PRCP')
            ELSE
              OPEN (UNIT=FNUMPRES,FILE=FNAMEPRES,POSITION='APPEND')
            ENDIF  ! End Responses simulated header writes
            WRITE (FNUMPRES,7401,ADVANCE='NO') RUN,EXCODE,TN,RN,CROP,
     A       PLDAYTMP,EDAPFR
            DO L = 1,KEYSTX
              IF (L.EQ.MSTG.AND.HNUMBER.EQ.1) THEN
                ! If harvested at a specific date
                tvi1 = Dapcalc(yeardoyharf,plyear,plday)
                WRITE (FNUMPRES,'(I6)',ADVANCE='NO') tvi1
              ELSE
                IF (KEYPS(L).GT.0) WRITE(FNUMPRES,'(I6)',ADVANCE='NO')
     &           PSDAP(KEYPS(L))
              ENDIF
            ENDDO
            WRITE (fnumpres,409)
     D       NINT(hwam),hwumchar,
     E       NINT(hnumam),NINT(hnumgm),
     F       laixchar,lnumsm,brnumsh,
     G       NINT(cwam),NINT(vwam),
     H       hiamchar,
     I       NINT(rwam),
     J       hnpcmchar,
     K       NINT(AMAX1(-99.0,cnam+rnam)),NINT(cnam),NINT(hnam),
     L       hinmchar,pltpop,
     C       NINT(amtnit),
     M       sradcav,tmaxcav,tmincav,
     N       NINT(raincc)            
 7401       FORMAT (I6,1X,A10,I4,I3,4X,A2,     !Run,excode,tn,rn,crop
     A       I6,                               !Pldaytmp
     B       F6.1)                             !Edapfr
  409       FORMAT (
     D       I6,A6,                            !gwam,hwum
     E       I6,I6,                            !g#am,g#gm
     F       A6,F6.1,F6.1                      !laix,lnumsm,shnumpm
     G       I6,I6,                            !Cwam,vwam
     H       A6,                               !hiam
     I       I6,                               !N(rwam)
     J       A6,                               !hnpcm
     K       I6,I6,I6,                         !N(cnam+rnam),(cn),(gn)
     L       A6,F6.1,                          !hinm,pltpop,shnumpm
     C       I6,                               !Amtnit
     M       3F6.1,                            !RAD,TX,TN
     N       I6)                               !RN   
            CLOSE(FNUMPRES)
            ! End Responses simulated writes
            
            ! Plant responses (Measured)
            IF (TDATANUM.LE.0 .AND. .NOT.FEXISTA) THEN
!              WRITE (fnumwrk,*)' '
!              WRITE (fnumwrk,*)
!     &         ' No data so cannot write PLANT RESPONSES (MEASURED)'
              OPEN (UNIT = FNUMTMP,FILE = FNAMEPREM,STATUS='UNKNOWN')
              CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
            ELSE
              IF (EXCODE.NE.EXCODEPREV.OR.TNAME(1:1).EQ.'*') THEN
                OPEN (UNIT=FNUMPREM,FILE=FNAMEPREM,POSITION='APPEND')
                WRITE (FNUMPREM,*) ' '
                WRITE (TLINETMP,99511) EXCODE,MODNAME
99511           FORMAT ('*RESPONSES(M):',A10,'  ',A8)
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
  298         FORMAT (
     G        '  HWAH  HWUH',
     H        '  H#AH  H#GH  LAIX  L#SH BR#AH',
     I        '  CWAH  VWAH  HIAH  RWAH',
     J        '  HN%H  TNAH',
     K        '  CNAH  HNAH',
     L        '  HINH PLPOP',
     F        '  NICH',
     M        ' SRADA TMAXA TMINA  PRCP')
              ELSE
                OPEN (UNIT=FNUMPREM,FILE=FNAMEPREM,POSITION='APPEND')
              ENDIF ! End Responses measured header writes
              WRITE (FNUMPREM,7401,ADVANCE='NO') RUN,EXCODE,TN,RN,CROP,
     A         PLDAYTMP,FLOAT(MAX(-99,edapm))
              DO L = 1,KEYSTX
                IF (L.EQ.MSTG.AND.HNUMBER.EQ.1) THEN
                  ! If harvested at a specific date
                  tvi1 = Dapcalc(yeardoyharf,plyear,plday)
                  WRITE (FNUMPREM,'(I6)',ADVANCE='NO') tvi1
                ELSE
                  IF (KEYPS(L).GT.0) WRITE(FNUMPREM,'(I6)',ADVANCE='NO')
     &             PSDAPM(KEYPS(L))
                ENDIF
              ENDDO
              !WRITE (FNUMPREM,'(I6)',ADVANCE='NO') PSDAPM(HSTG)
              WRITE (FNUMPREM,409)
     F         NINT(hwamm),hwummchar,
     G         NINT(hnumamm),NINT(hnumgmm),
     H         laixmchar,lnumsmm,brnumshm, 
     I         NINT(cwamm),NINT(vwamm),hiammchar,
     J         NINT(rwamm),
     K         hnpcmmchar,NINT(tnamm),NINT(cnamm),NINT(hnamm),
     L         hinmmchar,pltpopp,
     F         NINT(amtnit),
     M         sradcav,tmaxcav,tmincav,
     N         NINT(raincc)            
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

!-----------------------------------------------------------------------
!         IDETL = A OUTPUTS (Errora,Errort,Errors)
!-----------------------------------------------------------------------

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
            ENDIF
!            ELSE
!              WRITE (fnumwrk,*)' '
!              WRITE (fnumwrk,*)' Problem in finding intermediate stage '
!              WRITE (fnumwrk,*)'  Mature stage       = ',mstg          
!              WRITE (fnumwrk,*)'  Intermediate stage = ',l             
!              WRITE (fnumwrk,*)' '
!            ENDIF
            
            ! Errors (A-data)
            IF (TDATANUM.LE.0 .AND. .NOT.FEXISTA) THEN
!              WRITE (fnumwrk,*)' '
!              WRITE (fnumwrk,*)' No data so cannot write PLANTERA'
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
  996           FORMAT (/,'*ERRORS(A)',/)
                WRITE (FNUMERA,896)
  896           FORMAT ('@  RUN',
     A          ' EXCODE     ',
     B          '  TRNO RN',
     C          '    CR',
     D          '    EDAP   EDAPE',
     F          '    MDAP   MDAPE',
     G          '    HWAH   HWAHE',
     H          '    HWUM   HWUME',
     I          '    H#AM   H#AME',
     J          '    H#GM   H#GME',
     K          '    LAIX   LAIXE',
     L          '    L#SM   L#SME',
     N          '    CWAM   CWAME',
     O          '    VWAM   VWAME',
     P          '    HIAM   HIAME',
     Q          '    HN%M   HN%ME',
     R          '    CNAM   CNAME',
     S          '    HNAM   HNAME')
                CLOSE(FNUMERA)
              ENDIF  ! End ErrorA header writes
              OPEN (UNIT = FNUMERA,FILE = FNAMEERA,POSITION = 'APPEND')
              WRITE (FNUMERA,8401) RUN,EXCODE,TN,RN,CROP,
     A         Edap,emdaterr,
     C         mdap,mdaterr,
     D         NINT(hwam),NINT(hwaherr),
     E         hwum,NINT(hwumerr),
     F         NINT(hnumam),NINT(hnumaerr),
     G         hnumgm,NINT(hnumgerr),
     H         laix,NINT(laixerr),
     I         lnumsm,NINT(lnumserr),
     K         NINT(cwam),NINT(cwamerr),
     L         NINT(vwam),NINT(vwamerr),
     M         hiam,NINT(hiamerr),
     N         hnpcm,NINT(hnpcmerr),
     O         NINT(cnam),NINT(cnamerr),
     P         NINT(hnam),NINT(hnamerr)
 8401         FORMAT (I6,1X,A10,1X,I6,I3,4X,A2,
     A         I8,  I8,
     B         I8,  I8,
     D         I8,  I8,
     E         F8.3,I8,
     F         I8,  I8,
     G         F8.1,I8,
     H         F8.1,I8,
     I         F8.1,I8,
     K         I8,  I8,
     L         I8,  I8,
     M         F8.2,I8,
     N         F8.1,I8,
     O         I8,  I8,
     P         I8,  I8)
              CLOSE(FNUMERA)
            ENDIF ! End ErrorA writes (If data available)
          
            ! Errors (T)
            IF (.NOT.FEXISTT .OR. FROPADJ.GT.1 .OR. IDETG.EQ.'N') THEN
!              WRITE (fnumwrk,*) ' '
              IF (FROPADJ.GT.1) THEN
!                WRITE (fnumwrk,*) ' Cannot write PLANT ERRORS (T).',
!     &          ' Frequency of output > 1 day'
              ELSE  
!                WRITE (fnumwrk,*)
!     &          ' No data so cannot write PLANT ERRORS (T)'
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
 1502           FORMAT(A180)
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
!                WRITE (FNUMWRK,*) 'No columns found in T-file '
                GO TO 7777
              ENDIF
              ! Make new header line
              TLINETMP = ' '
              TLINETMP(1:1) = '@'
              DO L = 1, TFCOLNUM
                TLPOS = (L-1)*6+1
                IF (THEAD(L).EQ.'TRNO'.OR.THEAD(L).EQ.'YEAR'.OR.
     &            THEAD(L).EQ.'DATE') THEN
                  TLINETMP(TLPOS+2:TLPOS+5)=THEAD(L)(1:4)
                ELSEIF(THEAD(L).EQ.'DOY'.OR.THEAD(L).EQ.'DAP' .OR.
     &            THEAD(L).EQ.'DAS'.OR.THEAD(L).EQ.'DAY') THEN
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
 2996         FORMAT (/,'*ERRORS(T):',A69,/)
              tlinet(1:1) = '@'
              WRITE (FNUMERT,'(A180)') TLINETMP
              ! Read data lines, match dates, calculate errors, write
              DO L1 = 1,200
                TLINET = ' '
                READ (FNUMT,7778,ERR=7777,END=7777) TLINET
 7778           FORMAT(A180)
                IF (TLINET(1:1).EQ.'*') GO TO 7777
                IF (TLINET(1:6).EQ.'      ') GO TO 7776
                CALL Getstri(tlinet,tfdapcol,tfdap)
                IF (TFDAP.LE.0.0) THEN
                  GO TO 7777
                ENDIF
                DO WHILE (tfdap.NE.pgdap)
                  TLINEGRO = ' '
                  READ (NOUTPG,7779,ERR=7777,END=7777) TLINEGRO
                  CALL Getstri(tlinegro,pgrocol(tfdapcol),pgdap)
                  IF (PGDAP.LT.0) THEN
!                    WRITE (FNUMWRK,*) 'DAP in Plantgro file < 0 '
                    GO TO 7777
                  ENDIF
                ENDDO
 7779           FORMAT(A255)
                TLINETMP = ' '
                DO L = 1, TFCOLNUM
                  CALL Getstrr(tlinet,l,tfval)
                  CALL Getstrr(tlinegro,pgrocol(l),pgval)
                  ERRORVAL = 0.0
                  IF(TFVAL.GT.0.0.AND.PGVAL.GT.-99.AND.
     &             PGVAL.NE.0.0)THEN
                    ERRORVAL = 100.0 * (PGVAL - TFVAL) / TFVAL
                  ELSE
                    ERRORVAL = -99.0
                  ENDIF
                  IF (THEAD(L).EQ.'TRNO'.OR.THEAD(L).EQ.'YEAR' .OR.
     &              THEAD(L).EQ.'DOY'.OR.THEAD(L).EQ.'DAP' .OR.
     &              THEAD(L).EQ.'DAY' .OR.
     &              THEAD(L).EQ.'DAS'.OR.THEAD(L).EQ.'DATE') THEN
                    CALL Getstri(tlinet,l,tvi1)
                    WRITE (TCHAR,'(I6)') TVI1
                  ELSE
                    WRITE (TCHAR,'(I6)') NINT(ERRORVAL)
                  ENDIF
                  TLPOS = (L-1)*6+1
                  TLINETMP(TLPOS:TLPOS+5)=TCHAR
                ENDDO
                WRITE (FNUMERT,'(A180)') TLINETMP
 7776           CONTINUE
              ENDDO
 7777         CONTINUE
              GO TO 1601
 1600         CONTINUE
!              WRITE(fnumwrk,*)'End of file reading Measured.out'
!              WRITE(fnumwrk,*)
!     &         'Starnum and starnumm were: ',starnum,starnumm
 1601         CONTINUE
              CLOSE (FNUMERT)
              CLOSE (FNUMT)
              CLOSE (NOUTPG)
              ! Re-open file if open at start of work here
              IF (FOPEN) 
     &        OPEN (UNIT=NOUTPG,FILE='PlantGro.OUT',POSITION='APPEND')
            ENDIF  ! .NOT.FEXISTT .OR. FROPADJ.GT.1 .OR. IDETG.EQ.'N'
            ! End of ErrorT writes
          
          ELSE ! No ERROR files called for ... must be deleted          
          
            OPEN (UNIT=FNUMTMP,FILE=FNAMEERA,STATUS='UNKNOWN')
            CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
            OPEN (UNIT=FNUMTMP,FILE=FNAMEERT,STATUS='UNKNOWN')
            CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
          
          ENDIF ! End of Error writes  IDETL.EQ.'A'

!-----------------------------------------------------------------------
!         IDETD (DISPLAY) OUTPUTS IF WORKING IN CROPSIM SHELL
!-----------------------------------------------------------------------

          ! Screen writes
          IF (IDETD.EQ.'S') THEN
            IF (OUTCOUNT.LE.0) THEN
              CALL CSCLEAR5
              WRITE(*,*)' SIMULATION SUMMARY'
              WRITE(*,*)' '
              WRITE (*,499)
  499         FORMAT ('   RUN EXCODE    TRNO RN',
     X         ' TNAME..................',
     X         '.. REP  RUNI S O C CR  HWAM')
            ENDIF
            IF (OUTCOUNT .EQ. 25) THEN
              OUTCOUNT = 1
            ELSE
              OUTCOUNT = OUTCOUNT + 1
            ENDIF
            WRITE (*,410) run,excode,tn,rn,tname(1:25),
     X      rep,runi,sn,on,cn,crop,NINT(hwam)
  410       FORMAT (I6,1X,A10,I4,I3,1X,A25,
     X      I4,I6,I2,I2,I2,1X,A2,I6)
          ELSEIF (IDETD.EQ.'M') THEN
            ! Simulation and measured data
            CALL CSCLEAR5
            WRITE(*,'(A20,A10,I3)')' STAGES SUMMARY FOR ',EXCODE,TN
            WRITE(*,*)' '
            WRITE(*,9600)
            DO L = 1, PSNUM
              CALL Csopline(laic,laistg(l))
              IF (STGYEARDOY(L).LT.9999999.AND.
     &         L.NE.10.AND.L.NE.11) THEN
!                CALL CSYR_DOY(STGYEARDOY(L),YEAR,DOY)
                CALL YR_DOY(STGYEARDOY(L),YEAR,DOY)
                CALL Calendar(year,doy,dom,month)
                CNCTMP = 0.0
                IF (CWADSTG(L).GT.0.) 
     &            CNCTMP = CNADSTG(L)/CWADSTG(L)*100
                WRITE (*,'(I8,I4,1X,A3,I4,1X,I1,1X,A13,I6,A6,
     &           F6.1,I6,F6.2,F6.2,F6.2)')
     &           STGYEARDOY(L),DOM,MONTH,
     &           Dapcalc(stgyeardoy(L),plyear,plday),L,PSNAME(L),
     &           NINT(CWADSTG(L)),LAIC,LNUMSTG(L),
     &           NINT(CNADSTG(L)),CNCTMP,1.0-WFPPAV(L),1.0-NFPPAV(L)
              ENDIF
            ENDDO
            WRITE(*,*)' '
            WRITE(*,*)' Press ENTER to continue'
            PAUSE ' '
            CALL CSCLEAR5
            WRITE(*,'(A36,A10,I3)')
     &      ' SIMULATED-MEASURED COMPARISONS FOR ',EXCODE,TN
            WRITE(*,*)' '
            WRITE (*,206)
            WRITE (*,290) MAX(-99,gdap),MAX(-99,gdapm),
     A      MAX(-99,edap),MAX(-99,edapm)
            DO L = 1,KEYSTX
              IF (KEYPS(L).GT.0) WRITE (*,291)
     &        psname(KEYPS(L)),PSDap(KEYPS(L)),PSDapm(KEYPS(L))
            ENDDO
            WRITE (*,305)
     x       NINT(cwam),NINT(cwamm),
     x       NINT(rwam+sdwam),NINT(rwamm),
     x       NINT(senwacm),NINT(senwacmm),
     x       NINT(hwam),NINT(hwamm),
     x       NINT(vwam),NINT(vwamm),
     x       hiam,hiamm,
     x       NINT(rswam),NINT(rswamm)
          ENDIF ! End IDETD.EQ.'S' 

!-----------------------------------------------------------------------
!         Screen writes for Dssat sensitivity mode
!-----------------------------------------------------------------------
            
          IF (FILEIOT(1:3).EQ.'DS4' .AND. CN.EQ.1
     &                              .AND. RNMODE.EQ.'E') THEN         
            CALL CSCLEAR5
            WRITE(*,9589)
            WRITE(*,*) ' '
            WRITE(*,9588)
            WRITE(*,9600)
            DO L = 1, PSNUM
              CALL Csopline(laic,laistg(l))
              IF (STGYEARDOY(L).LT.9999999.AND.
     &            L.NE.10.AND.L.NE.11) THEN
!                CALL CSYR_DOY(STGYEARDOY(L),YEAR,DOY)
                CALL YR_DOY(STGYEARDOY(L),YEAR,DOY)
                CALL Calendar(year,doy,dom,month)
                CNCTMP = 0.0
                IF (CWADSTG(L).GT.0.0)
     &           CNCTMP = CNADSTG(L)/CWADSTG(L)*100
                WRITE (*,'(I8,I4,1X,A3,I4,1X,I1,1X,A13,I6,A6,
     &          F6.1,I6,F6.2,F6.2,F6.2)')
     &          STGYEARDOY(L),DOM,MONTH,
     &          Dapcalc(stgyeardoy(L),(plyeardoy/1000),plday),
     &          l,psname(l),
     &          NINT(CWADSTG(L)),LAIC,LNUMSTG(L),
     &          NINT(CNADSTG(L)),CNCTMP,1.0-WFPPAV(L),1.0-NFPPAV(L)
              ENDIF
            ENDDO
            ! For harvest at specified date
            IF (YEARDOYHARF.EQ.YEARDOY) THEN
              CALL Csopline(laic,lai)
!                CALL CSYR_DOY(YEARDOYHARF,YEAR,DOY)
                CALL YR_DOY(YEARDOYHARF,YEAR,DOY)
                CALL Calendar(year,doy,dom,month)
                CNCTMP = 0.0
                IF (CWAD.GT.0.0)CNCTMP = CNAD/CWAD*100
                WRITE (*,'(I8,I4,1X,A3,I4,1X,I1,1X,A13,I6,A6,
     &          F6.1,I6,F6.2,F6.2,F6.2)')
     &          YEARDOY,DOM,MONTH,
     &          Dapcalc(yeardoy,(plyeardoy/1000),plday),
     &          l,'Harvest      ',
     &          NINT(CWAD),LAIC,LNUM,
     &          NINT(CNAD),CNCTMP,1.0-WFPCAV,1.0-NFPCAV
            ENDIF 
          
            WRITE(*,*)' '
            WRITE(*,*)' Press ENTER to continue'
            PAUSE ' '
            CALL CSCLEAR5
          
            !WRITE (*,206)
            WRITE(*,'(A36,A10,I3)')
     &      ' SIMULATED-MEASURED COMPARISONS FOR ',EXCODE,TN
            WRITE(*,*)' '
            WRITE (*,206)
            WRITE (*,290) MAX(-99,gdap),MAX(-99,gdapm),
     A       MAX(-99,edap),MAX(-99,edapm)
            DO L = 1,KEYSTX
              IF (KEYPS(L).GT.0) WRITE (*,291)
     &         psname(KEYPS(L)),psdap(KEYPS(L)),psdapm(KEYPS(L))
            ENDDO
            WRITE (*,305)
     H       NINT(cwam),NINT(cwamm),
     I       MAX(-99,NINT(rwam+sdwam)),NINT(rwamm),
     J       NINT(senwacm),NINT(senwacmm),
     L       NINT(hwam),NINT(hwamm),
     M       NINT(vwam),NINT(vwamm),
     N       hiam,hiamm,
     O       NINT(rswam),NINT(rswamm)
            IF (lwphc+swphc.GT.0.0) WRITE (*,3051)
     &       NINT(cwahc),NINT(cwahcm)
          
            WRITE(*,*)' '
            WRITE(*,*)' Press ENTER to continue'
            PAUSE ' '
            CALL CSCLEAR5
          
            WRITE (*,2061)
2061        FORMAT(
     &       /,   
     &       "@",5X,"VARIABLE",T42,"SIMULATED   MEASURED",/,  
     &         6X,"--------",T42,"---------   --------")  
             WRITE (*,3052)
     A       hwumchar,hwummchar,
     B       NINT(hnumam),NINT(hnumamm),
     C       hnumgm,hnumgmm,
     E       laix,laixm,
     F       lnumsm,lnumsmm,
     G       nupac,nupacm,
     H       cnam,cnamm,
     I       rnam,rnamm,
     J       sennatc,sennatcm,
     K       hnam,hnamm,
     L       vnam,vnamm,
     M       hinm,hinmm,
     N       hnpcm,hnpcmm,
     O       vnpcm,vnpcmm
          
            WRITE(*,*)' '
            WRITE(*,*)' Press ENTER to continue'
            PAUSE ' '
            CALL CSCLEAR5
            CALL CSCLEAR5
            CALL CSCLEAR5
            CALL CSCLEAR5
          
          ENDIF ! END OF SCREEN WRITES FOR DSSAT SENSITIVITY MODE
        
!-----------------------------------------------------------------------
!         Store variables for sending to CSM summary output routines
!-----------------------------------------------------------------------
        
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
!              WRITE (FNUMWRK, 1200) CDAYS, 
!     &         RAINCC, DMP_Rain*0.1, DMP_Rain, GrP_Rain*0.1, GrP_Rain,
!     &         ETCC,  DMP_ET*0.1,   DMP_ET,   GrP_ET*0.1,   GrP_ET, 
!     &         EPCC,  DMP_EP*0.1,   DMP_EP,   GrP_EP*0.1,   GrP_EP
!              IF (IRRAMTC > 1.E-3) THEN
!                WRITE(FNUMWRK, 1210) 
!     &            IRRAMTC, DMP_Irr*0.1, DMP_Irr, GrP_Irr*0.1, GrP_Irr
!              ENDIF  
!              IF (ISWNIT.NE.'N') THEN
!                IF (Amtnit > 1.E-3) THEN
!                  WRITE(FNUMWRK, 1220) Amtnit, DMP_NApp, GrP_NApp 
!                ENDIF
!                IF (NUPAC > 1.E-3) THEN
!                  WRITE(FNUMWRK, 1230) NUPAC, DMP_NUpt,GrP_NUpt
!                ENDIF
!              ENDIF ! ISWNIT NE 'N'
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
         
!-----------------------------------------------------------------------
!         Re-initialize
!-----------------------------------------------------------------------
         
          ! Need to re-initialize following because of automatic
          ! fertilization routines in DSSAT
          NFG = 1.0
          NFP = 1.0
          WFG = 1.0
          WFP = 1.0
          
          UNO3 = 0.0
          UNH4 = 0.0
          
!          WRITE (fnumwrk,*) ' '
!          WRITE (fnumwrk,'(A50)')
!     &     ' END OF RUN. WILL BEGIN NEW CYCLE IF CALLED FOR.  '
!          IF (IDETL.NE.'N') WRITE (fnumwrk,*) ' '
          SEASENDOUT = 'Y'
          
        ENDIF ! End STGYEARDOY(11).EQ.YEARDOY.OR.DYNAMIC.EQ.SEASEND

!-----------------------------------------------------------------------
!       Store variables for possible use next day/step
!-----------------------------------------------------------------------

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

!***********************************************************************
      ELSEIF (DYNAMIC.EQ.SEASEND) THEN
!***********************************************************************

!        IF (STGYEARDOY(11).NE.YEARDOY) THEN  ! End for non-crop reason
!          WRITE (fnumwrk,*)' '
!          WRITE (fnumwrk,'(A50)')
!     &     ' Run terminated.(Usually because ran out of weather data).'
!        ENDIF

        EXCODEPREV = EXCODE

        CLOSE (NOUTPG)
        CLOSE (NOUTPG2)
        CLOSE (NOUTPGF)
        CLOSE (NOUTPN)
        CLOSE (FNUMWRK)

!***********************************************************************
      ENDIF ! End of INITIATION-RATES-INTEGRATE-OUTPUT-SEASEND construct
!***********************************************************************

      ! Store previous dynamic setting
      DYNAMICPREV = DYNAMIC

!***********************************************************************
!    Call other modules
!***********************************************************************

      IF (LENDIS.GT.0.AND.LENDIS.LT.3) THEN
        IF (ISWDIS(LENDIS:LENDIS).NE.'N')
     X   CALL Disease(Spdirfle,run,runi,step,  ! Run+crop component
     X    fropadj,outhed,                      ! Loop info.
     X    year,doy,dap,                        ! Dates
     X    didat,digfac,diffacr,                ! Disease details
     X    dcdat,dcfac,dcdur,dctar,             ! Disease control
     &    tmax,tmin,dewdur,                    ! Drivers - weather
     &    pla,plas,pltpop,                     ! States - leaves
     &    lnumsg,lap,LAPP,laps,                ! States - leaves
!    &    stgyeardoy,                          ! Stage dates
     &    didoy,                               ! Disease initiation
     &    dynamic)                             ! Control
      ENDIF

      IF (DYNAMIC.EQ.INTEGR.AND.LNUMSG.GT.0) CALL Cscrplayers
     & (chtpc,clapc,                        ! Canopy characteristics
     & pltpop,lai,canht,                    ! Canopy aspects
     & lnumsg,lap,lapp,laps,                ! Leaf cohort number,size
     & LAIL,LAILA,                          ! Leaf area indices,layers
     & LAIA)                                ! Leaf area index,active

 9589 FORMAT
     & (//,'*SIMULATED CROP AND SOIL STATUS AT MAIN DEVELOPMENT STAGES')
 9588 FORMAT(
     &/,' ...... DATE ....... GROWTH STAGE    BIOMASS   LEAF  
     &     CROP N      STRESS')     
 9600 FORMAT(' YEARDOY DOM MON DAP ................ kg/ha AREA NUMBER
     &  kg/ha   %   H2O    N')
  206       FORMAT(
     &    /,"*MAIN GROWTH AND DEVELOPMENT VARIABLES",//,   
     &       "@",5X,"VARIABLE",T42,"SIMULATED   MEASURED",/,  
     &           6X,"--------",T42,"---------   --------")  
 500  FORMAT(/,'*ENVIRONMENTAL AND STRESS FACTORS',//,
     &' |-------Branching tier-------|-------------Environment--------',
     &'------|----------------Stress-----------------|',/,
     &30X,'|--------Average-------|---Cumulative--|         (0=Min, 1=',
     &'Max Stress)         |',/,
     &25X,'Time  Temp  Temp Solar Photop         Evapo |----Water---|-',
     &'-Nitrogen--|--Phosphorus-|',/,
     &25X,'Span   Max   Min   Rad  [day]   Rain  Trans  Photo',9X,'Pho',
     &'to         Photo',/,
     &25X,'days    °C    °C MJ/m2     hr     mm     mm  synth Growth ',
     &' synth Growth  synth Growth',/,110('-'))
  270 FORMAT(/,'------------------------------------------------------',
     &'--------------------------------------------------------')
  300 FORMAT(/,10X,A," YIELD : ",I8," kg/ha    [Dry weight] ",/)
 1200     FORMAT(
     &'------------------------------------------------------',
     &'--------------------------------------------------------',
     &///,'*RESOURCE PRODUCTIVITY',
     &//,' Growing season length:', I4,' days ',
     &//,' Precipitation during growth season',T42,F7.1,' mm[rain]',
     & /,'   Dry Matter Productivity',T42,F7.2,' kg[DM]/m3[rain]',
     &                           T75,'=',F7.1,' kg[DM]/ha per mm[rain]',
     & /,'   Yield Productivity',T42,F7.2,' kg[Sroot yield]/m3[rain]',
     &                       T75,'=',F7.1,' kg[yield]/ha per mm[rain]',
     &//,' Evapotranspiration during growth season',T42,F7.1,' mm[ET]',
     & /,'   Dry Matter Productivity',T42,F7.2,' kg[DM]/m3[ET]',
     &                            T75,'=',F7.1,' kg[DM]/ha per mm[ET]',
     & /,'   Yield Productivity',T42,F7.2,' kg[Sroot yield]/m3[ET]',
     &                       T75,'=',F7.1,' kg[yield]/ha per mm[ET]',
     &//,' Transpiration during growth season',T42,F7.1,' mm[EP]',
     & /,'   Dry Matter Productivity',T42,F7.2,' kg[DM]/m3[EP]',
     &                            T75,'=',F7.1,' kg[DM]/ha per mm[EP]',
     & /,'   Yield Productivity',T42,F7.2,' kg[Sroot yield]/m3[EP]',
     &                       T75,'=',F7.1,' kg[yield]/ha per mm[EP]')

 1210 FORMAT(
     & /,' Irrigation during growing season',T42,F7.1,' mm[irrig]',
     & /,'   Dry Matter Productivity',T42,F7.2,' kg[DM]/m3[irrig]',
     &                       T75,'=',F7.1,' kg[DM]/ha per mm[irrig]',
     & /,'   Yield Productivity',T42,F7.2,' kg[Sroot yield]/m3[irrig]',
     &                       T75,'=',F7.1,' kg[yield]/ha per mm[irrig]')

 1220 FORMAT(
     & /,' N applied during growing season',T42,F7.1,' kg[N applied]/ha'
     & /,'   Dry Matter Productivity',T42,F7.1,' kg[DM]/kg[N applied]',
     & /,'   Yield Productivity',T42,F7.1,' kg[yield]/kg[N applied]')

 1230 FORMAT(
     & /,' N uptake during growing season',T42,F7.1,' kg[N uptake]/ha'
     & /,'   Dry Matter Productivity',T42,F7.1,' kg[DM]/kg[N uptake]',
     & /,'   Yield Productivity',T42,F7.1,' kg[yield]/kg[N uptake]')

      END  ! CSCAS

