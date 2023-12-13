        Module CRP_First_Trans_m
        
        
      INTEGER,PARAMETER::DINX =   3 ! Disease number,maximum
      INTEGER,PARAMETER::PSX  =  20 ! Principal stages,maximum
      INTEGER,PARAMETER::SSX  =  20 ! Secondary stages,maximum
      INTEGER,PARAMETER::PHSX =   5 ! Phyllochron stages,maximum
      INTEGER,PARAMETER::KEYSTX = 9 ! Maximum number of key stages
      INTEGER,PARAMETER::DCNX =  10 ! Disease control #,maximum
      INTEGER,PARAMETER::LCNUMX=500 ! Maximum number of leaf cohorts
      INTEGER,PARAMETER::LNUMX= 500 ! Maximum number of leaves/axis
      INTEGER,PARAMETER::HANUMX= 40 ! Maximum # harvest instructions
      !INTEGER,PARAMETER::NL   =  20 ! Maximum number of soil layers

      !INTEGER,PARAMETER::RUNINIT=1  ! Program initiation indicator
      !INTEGER,PARAMETER::SEASINIT=2 ! Reinitialisation indicator
      !INTEGER,PARAMETER::RATE = 3   ! Program rate calc.indicator
      !INTEGER,PARAMETER::INTEGR=4   ! Program update indicator
      !INTEGER,PARAMETER::OUTPUT=5   ! Program output indicator
      !INTEGER,PARAMETER::SEASEND= 6 ! Program ending indicator

      CHARACTER(LEN=1),PARAMETER::BLANK = ' '
      CHARACTER(LEN=3),PARAMETER::DASH = ' - '
      CHARACTER(LEN=6),PARAMETER::ERRKEY = 'CSCRP '

      !REAL,PARAMETER::PATM=101300.0! Pressure of air,Pa
      !REAL,PARAMETER::SHAIR=1005.0 ! Specific heat of air,MJ/kg
      !REAL,PARAMETER::SBZCON=4.903E-9 !Stefan Boltzmann,MJ/K4/m2/d
      
      INTEGER       ADAP          ! Anthesis,days after planting   d
      REAL          ADAPFR        ! Anthesis DAP+fraction          #
      INTEGER       ADAPM         ! Anthesis,DAP,measured          d
      INTEGER       ADAT          ! Anthesis date (Year+doy)       #
      INTEGER       ADATEND       ! Anthesis end date (Year+doy)   #
      INTEGER       ADATERR       ! Anthesis date error            d
      INTEGER       ADATM         ! Anthesis date,measured         #
      REAL          ADATT         ! Anthesis date from t file      YrDoy
      REAL          ADAYEFR       ! Anthesis end,fraction of day   #
      REAL          ADAYFR        ! Anthesis,fraction of day       #
      INTEGER       ADOY          ! Anthesis day of year           d
      REAL          AEDAPFR       ! Anthesis end date (DAP+fr)     #
      INTEGER       AEDAPM        ! Anthesis end date,measured     #
      REAL          AFLF(0:LNUMX) ! CH2O factor for leaf,average   #
      REAL          AH2OROOTZONE  ! Available h2o in root zone     mm
      REAL          AH2OPROFILE   ! Available H2o,profile          mm
      REAL          ALBEDO        ! Canopy+soil albedo             fr
      !REAL          ALBEDOS       ! soil albedo                    fr
      REAL          AMTNIT        ! Cumulative amount of N applied kg/ha
      REAL          ANDEM         ! Crop N demand                  kg/ha
      REAL          ANFER(200)    ! N amount in fertilizer appln   kg/ha
      INTEGER       ARGLEN        ! Argument component length      #
      INTEGER       ASTG          ! Anthesis stage                 #
      REAL          AVGSW         ! Average soil water in SWPLTD   %
      REAL          AWNAI         ! Awn area index                 m2/m2
      REAL          AWNS          ! Awn score,1-10                 #
      REAL          BASELAYER     ! Depth at base of layer         cm
      !REAL          BD(20)        ! Bulk density (moist)          g/cm3
      REAL          BLAYR(20)     ! Base of soil layers            cm
      REAL          CAID          ! Canopy area index              #
      !REAL          CANHT         ! Canopy height                  cm
      REAL          CANHTG        ! Canopy height growth           cm
      REAL          HTSTD         ! Canopy height standard         cm
      REAL          CARBOADJ      ! Ch2o adjustment for LAI change g/p
      REAL          CARBOBEG      ! Ch2o available,beginning day   g/p
      REAL          CARBOBEGI     ! Ch2o avail,internal co2 calc   g/p
      REAL          CARBOBEGIA    ! Ch2o avail,internal co2,adj    #
      REAL          CARBOBEGR     ! Ch2o avail,PARUE calculation   g/p
      REAL          CARBOBEGM     ! Ch2o avail,Monteith res calc   g/p
      REAL          CARBOC        ! Ch2o assimilated,cumulative    g/p
      REAL          CARBOEND      ! Ch2o available,end of day      g/p
      REAL          CARBOGF       ! Ch2o grain fill                g/p
      INTEGER       CARBOLIM      ! Ch2o limited grain growth Days d
      REAL          CARBOLSD      ! Ch2o used for leaves from seed g/p
      REAL          CARBOPM       ! Ch2o available,>mature         g/p
      REAL          CARBOR        ! Ch2o available,roots           g/p
      REAL          CARBOT        ! Ch2o available,tops            g/p
      REAL          CARBOTMP      ! Ch2o available,temporary value g/p
      REAL          CARBOTMPI     ! Ch2o avail,internal co2 calc   g/p
      REAL          CARBOTMPR     ! Ch2o avail,PARUE calculation   g/p
      REAL          CARBOTMPM     ! Ch2o avail,Monteith res calc   g/p
      INTEGER       CCOUNTV       ! Counter for days after max lf# #
      INTEGER       CDAYS         ! Crop cycle duration            PVoCd
      REAL          CHFR          ! Chaff growth rate,fr stem gr   #
      REAL          CHPHASE(2)    ! Chaff gr. start,etc.,stages    #
      REAL          CHPHASEDU(2)  ! Chaff gr. start,etc.,stages    PVTt
      REAL          CHRSWAD       ! Chaff reserves                 kg/ha
      REAL          CHRSWT        ! Chaff reserves                 g/p
      REAL          CHTPC(10)     ! Canopy ht % associated w LA%   %
      REAL          CHWAD         ! Chaff weight                   kg/ha
      REAL          CHWADOUT      ! Chaff weight for output        kg/ha
      REAL          CHWT          ! Chaff weight                   g/p
      REAL          CLAPC(10)     ! Canopy lf area % down to ht    %
      !REAL          CLOUDS        ! Cloudiness factor,relative,0-1 #
      !INTEGER       CN            ! Crop component (multicrop)     #
      REAL          CNAA          ! Canopy N at anthesis           kg/ha
      REAL          CNAAM         ! Canopy N,anthesis,measured     kg/ha
      REAL          CNAD          ! Canopy nitrogen                kg/ha
      REAL          CNADPREV      ! Canopy nitrogen,previous day   kg/ha
      REAL          CNADSTG(20)   ! Canopy nitrogen,specific stage kg/ha
      REAL          CNAM          ! Canopy N at maturity           kg/ha
      REAL          CNAMERR       ! Canopy N,maturity,error        %
      REAL          CNAMM         ! Canopy N,mature,measured       kg/ha
      REAL          CNCTMP        ! Canopy N concentration,temp    %
      INTEGER       CNI           ! Crop component,initial value   #
      REAL          CNPCA         ! Canopy N % at anthesis         #
      REAL          CNPCMM        ! Canopy N,maturity,measured     %
      !REAL          CO2           ! CO2 concentration in air       vpm
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
      REAL          CO2PC         ! CO2 concentration,phase cumul  ppm
      REAL          CO2RF(10)     ! CO2 reference concentration    vpm
      INTEGER       COLNUM        ! Column number                  #
      !INTEGER       CSIDLAYR      ! Layer # output from function   #
      !INTEGER       CSTIMDIF      ! Time difference function       #
      !REAL          CSVPSAT       ! Vapour pressure function op    mb
      !INTEGER       CSYDOY        ! Yr+Doy output from function    #
      !INTEGER       CSYEARDOY     ! Year+Doy from function         #
      !REAL          CSYVAL        ! Y value from function          #
      INTEGER       CTRNUMPD      ! Control # missing phases       #
      REAL          CUMDEP        ! Cumulative depth               cm
      REAL          CUMDU         ! Cumulative development units   #
      REAL          CUMDULAG      ! Cumulative DU lag phase        #
      REAL          CUMDULIN      ! Cumulative DU linear phase     #
      REAL          CUMDUS        ! Cumulative DU during stem ph   #
      REAL          CUMSW         ! Soil water in depth SWPLTD     cm
      REAL          CUMTT         ! Cumulative thermal times       #
      REAL          CUMVD         ! Cumulative vernalization days  d
      REAL          CWAA          ! Canopy weight at anthesis      kg/ha
      REAL          CWAAM         ! Canopy wt,anthesis,measured    kg/ha
      REAL          CWAD          ! Canopy weight                  kg/ha
      REAL          CWADGS        ! Canopy weight at grain set     kg/ha
      INTEGER       CWADCOL       ! Column number for canopy wt    #
      REAL          CWADPREV      ! Canopy weight,previous day     kg/ha
      REAL          CWADSTG(20)   ! Canopy weight,particular stage kg/ha
      REAL          CWADT         ! Canopy weight from t file      kg/ha
      REAL          CWAHC         ! Canopy weight harvested,forage kg/ha
      REAL          CWAHCM        ! Canopy wt harvested,forage,mes kg/ha
      REAL          CWAM          ! Canopy weight,maturity         kg/ha
      REAL          CWAMERR       ! Canopy weight,maturity,error   %
      REAL          CWAMM         ! Canopy wt,mature,measured      kg/ha
      REAL          CWAN(HANUMX)  ! Canopy wt minimum after harvst kg/ha
      INTEGER       DAE           ! Days after emergence           d
      INTEGER       DAP           ! Days after planting            d
      !INTEGER       DAPCALC       ! DAP output from funcion        #
      INTEGER       DAS           ! Days after start of simulation d
      INTEGER       DATE          ! Date (Year+Doy)                #
      INTEGER       DATECOL       ! Date column number             #
      !REAL          DAYLT         ! Daylength,6deg below horizon   h
      REAL          DAYLCAV       ! Daylength (6deg) av for cycle  h
      REAL          DAYLCC        ! Daylength,cycle sum            h.d 
      REAL          DAYLPAV(0:12) ! Daylength (6deg) av for phase  h
      REAL          DAYLPC        ! Daylength (6deg),cumulative    h
      REAL          DAYLPREV      ! Daylength previous day         h
      REAL          PPS(0:10)     ! Photoperiod sensitivity,phase  %/10h
      REAL          DAYLST(0:12)  ! Daylength (6deg) at stage      h
      REAL          DAYSUM        ! Days accumulated in month      #
      INTEGER       DCDAT(DCNX)   ! Disease control application YrDoy
      REAL          DCDUR(DCNX)   ! Disease control duration       d
      REAL          DCFAC(DCNX)   ! Disease control gr factor 0-1  #
      INTEGER       DCTAR(DCNX)   ! Disease control target         #
      REAL          DEADN         ! Dead leaf N retained on plant  g/p
      REAL          DEADNAD       ! Dead N retained on plant       kg/ha
      REAL          DEADWAM       ! Dead weight retained,maturity  kg/ha
      REAL          DEADWAMM      ! Dead weight retained,measured  kg/ha
      REAL          DEADWT        ! Dead leaf wt.retained or shed  g/p
      REAL          DEADWTM       ! Dead leaf wt.on plant,maturity g/p
      REAL          SENTOPRETAINED ! Dead leaf wt.retained on plantg/p
      REAL          SENTOPRETAINEDA ! Dead weight retained on plantkg/ha
      REAL          SENTOPLITTER  ! Dead wt.to litter              g/p
      REAL          SENTOPLITTERA ! Dead wt.to litter              kg/ha
      REAL          DEADWTSGE     ! Dead weight rtained,stem g end g/p
      !REAL          DEPMAX        ! Maximum depth of soil profile  cm
      !REAL          DEWDUR        ! Dew duration                   h
      REAL          DF            ! Daylength factor 0-1           #
      REAL          DFNEXT        ! Daylength factor,next phase    #
      REAL          DFOUT         ! Daylength factor for output    #
      REAL          PPFPE         ! Photoperiod factor,pre-emerge  #
      REAL          DGLF(LNUMX)   ! Days during which leaf growing #
      INTEGER       DIDAT(DINX)   ! Disease initiation date YrDoy  d
      INTEGER       DIDOY(DINX)   ! Disease infestation doy        d
      REAL          DIFFACR(DINX) ! Dis favourability requirement  #
      REAL          DIGFAC(DINX)  ! Disease growth factor 0-1      #
      !REAL          DLAYR(20)     ! Depth of soil layers           cm
      REAL          DLAYRTMP(20)  ! Depth of soil layers with root cm
      REAL          DMP_EP        ! Dry matter per unit EP         g/mm
      REAL          DMP_ET        ! Dry matter per unit ET         g/mm 
      REAL          DMP_Irr       ! Dry matter per unit irrigation g/mm
      REAL          DMP_NApp      ! Dry matter per unit N applied  kg/kg
      REAL          DMP_NUpt      ! Dry matter per unit N taken uo kg/kg
      REAL          DMP_Rain      ! Dry matter per unit water      g/mm
      INTEGER       DOM           ! Day of month                   #
      !INTEGER       DOY           ! Day of year                    d
      INTEGER       DOYCOL        ! Day of year column number      #
      !REAL          DRAIN         ! Drainage from soil profile     mm/d
      REAL          DRAINC        ! Drainage from profile,cumulat  mm  
      INTEGER       DRDAP         ! Double ridges date             dap
      INTEGER       DRDAT         ! Double ridges date             #
      INTEGER       DRDATM        ! Double ridges date,measured    #
      REAL          DRF1          ! Double ridges factor 1         #
      REAL          DRF2          ! Double ridges factor 2         #
      REAL          DRF3          ! Double ridges factor 3         #
      REAL          DRSTAGE       ! Double ridges stage            #
      REAL          DSTAGE        ! Development stage,linear       #
      REAL          DTRY          ! Effective depth of soil layer  cm
      REAL          DU            ! Developmental units            PVC.d
      !REAL          DUL(20)       ! Drained upper limit for soil   #
      REAL          DULAG         ! Developmental units,lag phase  PVC.d
      REAL          DULF          ! Development time,leaves        C.d
      REAL          DULFNEXT      ! Development time,leaves,next   oCd
      REAL          DULIN         ! Developmental units,linear ph  PVC.d
      REAL          DUNEED        ! Developmental units needed ph  PVC.d
      REAL          DUPHASE       ! Development units,current ph   PVoCd
      REAL          DUPNEXT       ! Development units,next phase   PVoCd
      REAL          DUTOMSTG      ! Developmental units,germ->mat  Du
      REAL          DWRPH         ! Reained dead wt harvested      g/p
      REAL          DWRPHC        ! Reained dead wt harvested,cum  g/p
      !INTEGER       DYNAMIC       ! Program control variable       #
      INTEGER       DYNAMICPREV   ! Program control varbl,previous #
      REAL          EARLYN        ! Leaf # for early N switch      # 
      REAL          EARLYW        ! Leaf # for early h20 switch    # 
      INTEGER       ECSTG         ! End crop stage                 #
      INTEGER       EDAP          ! Emergence DAP                  d
      REAL          EDAPFR        ! Emergence DAP+fraction         #
      INTEGER       EDAPM         ! Emergence DAP measured         #
      INTEGER       EDATM         ! Emergence date,measured (Afle) #
      INTEGER       EDATMX        ! Emergence date,measured (YrDy) #
      REAL          EDAYFR        ! Emergence day fraction         #
      INTEGER       EMDATERR      ! Emergence date error d         #
      REAL          EMRGFR        ! Fraction of day > emergence    #
      REAL          EMRGFRPREV    ! Fraction of day > em,previous  #
      !REAL          EO            ! Potential evaporation          mm/d
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
      !REAL          EOP           ! Potential evaporation,plants   mm/d
      REAL          EOPEN         ! Potential evaporation,Penman   mm/d
      REAL          EOPENC        ! Potential evaporation,Pen,cum  mm  
      REAL          EOPT          ! Potential evaporation,PT       mm/d
      REAL          EOPTC         ! Potential evaporation,PT,cum   mm
      REAL          EORATIO       ! Pevap increase w LAI (<6)      #
      !REAL          EP            ! Transpiration daily            mm/d 
      REAL          EPCC          ! Transpiration cycle sum        mm 
      REAL          EPPC(0:12)    ! Transpiration cycle sum        mm 
      REAL          EPSRATIO      ! Function,plant/soil evap rate  #
      INTEGER       ERRNUM        ! Error number from compiler     #
      REAL          ERRORVAL      ! Plgro-tfile values/Plgro       #
      !REAL          ES            ! Actual soil evaporation rate   mm/d
      !REAL          ET            ! Evapotranspiration daily       mm/d 
      REAL          ETCC          ! Evapotranspiration cycle sum   mm 
      REAL          ETPC(0:12)    ! Evapotranspiration phase sum   mm 
      INTEGER       EVALOUT       ! Evaluate output lines for exp  #
      INTEGER       EVHEADNM      ! Number of headings in ev file  #
      INTEGER       EVHEADNMMAX   ! Maximum no headings in ev file #
      REAL          EWAD          ! Ear weight                     kg/ha
      INTEGER       EYEARDOY      ! Emergence Year+DOY             #
      INTEGER       EYEARDOYM     ! Emergence measured Year+DOY    #
      REAL          FAC(20)       ! Factor ((g/Mg)/(kg/ha))        #
      INTEGER       FAPPNUM       ! Fertilization application number
      INTEGER       FDAY(200)     ! Dates of fertilizer appn (YrDoy)
      !REAL          FERNIT        ! Fertilizer N applied          kg/ha
      REAL          FERNITPREV    ! Fertilizer N applied to ystday kg/ha
      INTEGER       FILELEN       ! Length of file name            #
      INTEGER       FLDAP         ! Final leaf date                Yrdoy
      REAL          FLN           ! Final leaf #                   #
      REAL          FLNAITKEN     ! Final leaf #,AIKEN formula     #
      REAL          FNH4          ! Unitless ammonium supply index #
      REAL          FNO3          ! Unitless nitrate supply index  #
      INTEGER       FNUMERA       ! File number,A-data errors      #
      INTEGER       FNUMERR       ! File number,error file         #
      INTEGER       FNUMERT       ! File number,T-data errors      #
      INTEGER       FNUMEVAL      ! Number used for evaluate op    #
      INTEGER       FNUMLVS       ! File number,leaves             #
      INTEGER       FNUMMEAS      ! Number used for measured data  #
      INTEGER       FNUMOV        ! Number used for overview op    #
      INTEGER       FNUMPHA       ! File number,phases             #
      INTEGER       FNUMPHEM      ! File number,phenology,measured #
      INTEGER       FNUMPHES      ! File number,phenology,simulate #
      INTEGER       FNUMPREM      ! File number,measured responses #
      INTEGER       FNUMPRES      ! File number,simulated response #
      INTEGER       FNUMPSUM      ! Number used for plant summary  #
!      INTEGER       FNUMREA       ! File number,reads.out file     #
      INTEGER       FNUMT         ! Number used for T-file         #
      INTEGER       FNUMTMP       ! File number,temporary file     #
      !INTEGER       FNUMWRK       ! File number,work file          #
      !INTEGER       FROP          ! Frquency of outputs,as sent    d
      INTEGER       FROPADJ       ! Frquency of outputs,adjusted   d
      REAL          FSDU          ! Rstage when final sen started  PVoCd
      REAL          FSOILH2O      ! Final soil water               cm   
      REAL          G2A(0:3)      ! Grain growth rate,adjusted     mg/du
      REAL          G3            ! Cultivar coefficient,stem wt   g
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
      REAL          GFDAPFR       ! Grain filling start DAP+fr     #
      INTEGER       GFDAPM        ! Grain filling date,measured    #
      REAL          GFDAT         ! Grain filling start DAP        #
      REAL          GFDUR         ! Linear grain fill duration     d
      REAL          GGPHASE(4)    ! Grain set start,etc.,stages    #
      REAL          GGPHASEDU(4)  ! Grain set start,etc.,stages    PVTt
      REAL          GLIGPC        ! Grain lignin content           %
      REAL          GMPCH         ! Grain moisture percent,harvest # 
      REAL          GNAD          ! Grain N                        kg/ha
      REAL          GNAM          ! Grain N at maturity            kg/ha
      REAL          GNAMM         ! Harvest N,mature,measured      kg/ha
      REAL          GNOAD         ! Grains per unit area           #/m2
      REAL          GNOAM         ! Grains per unit area,maturity  #/m2
      REAL          GNOAMM        ! Grain #,mature,measured        #/m2
      REAL          GNOGM         ! Grains/tiller (group),maturity #/gr
      REAL          GNOGMM        ! Grain#/group,mature,measured   #/gr
      REAL          GNOPD         ! Grains per plant               #/p
      REAL          GNOPAS        ! Grains per plant after st.adj  #/p
      REAL          GNOPM         ! Grains per plant,maturity      #/p
      REAL          GNORF         ! Grain # radiation factor       fr/MJ
      REAL          GNORT         ! Grain # radiation threshold    MJ/m2
      REAL          GNOWTS        ! Cultivar coefficient,grain #   #/g
      REAL          GNOWTM        ! Grains/non-grain wt,maturity   #/g
      REAL          GNPCM         ! Harvest N%,maturity            %
      REAL          GNPCMM        ! Harvest N,mature,measured      %
      REAL          GNPH          ! Grain N harvested              g/p
      REAL          GNPHC         ! Grain N harvested,cumulative   g/p
      REAL          GPLASENF      ! Green leaf area,final sen strt #
      REAL          GRAINANC      ! Grain N concentration,fr       #
      REAL          GRAINN        ! Grain N                        g/p
      REAL          GRAINNDEM     ! N demand for grain filling     g/p
      REAL          GRAINNDEMLSR  ! Grain N demand,leaves+stem+rt  g/p
      REAL          GRAINNGL      ! Grain N growth from leaves     g/p
      REAL          GRAINNGR      ! Grain N growth from roots      g/p
      REAL          GRAINNGRS     ! Reserves N use for grain       g/p
      REAL          GRAINNGS      ! Grain N growth from stems      g/p
      REAL          GRAINNGU      ! Grain N growth,uptake          g/p
      REAL          GRAINNTMP     ! Grain N,temporary value        g/p
      REAL          GNPCMN        ! Grain N minimum conc,%         #
      REAL          GNPCMX        ! Grain N,maximum conc,%         #
      REAL          GNPCS         ! Grain N standard conc,%        #
      REAL          GROCH         ! Chaff growth rate              g/p
      REAL          GROCHFR       ! Chaff growth rate,fraction st  #
      REAL          GROGR         ! Grain growth                   g/p
      REAL          GROGRA        ! Grain growth,current assim     g/p
      REAL          GROGRP        ! Grain growth potential         g/p
      REAL          GROGRPA       ! Grain growth,possible,assim    g/p
      REAL          GROGRRS       ! Grain growth,from reserves     g/p
      REAL          GROLF         ! Leaf growth rate               g/p
      REAL          GROLFP        ! Leaf growth,potential          g/p
      REAL          GROLFRS       ! Leaf growth from reserves      g/p
      REAL          GROLFRT       ! Leaf growth from root d matter g/p
      REAL          GROLFRTN      ! Leaf N growth from root N      g/p
      REAL          GROLS         ! Leaf+stem growth               g/p
      REAL          GROLSP        ! Leaf+stem growth potential     g/p
      REAL          GRORS         ! Reserves growth                g/p
      REAL          GRORSGR       ! Reserves gr,unused grain assim g/p
      REAL          GRORSPM       ! Reserves growth,post-maturity  g/p
      REAL          GRORSPRM      ! Reserves growth,pre-maturity   g/p
      REAL          GROST         ! Stem growth rate               g/p
      REAL          GROSTP        ! Stem growth potential          g/p 
      REAL          GROSTPSTORE   ! Stem growth potential,previous g/p 
      REAL          GRP_ET        ! Harvest product per unit water g/mm
      REAL          GRP_Rain      ! Harvest product per unit water g/mm
      REAL          GRWT          ! Grain weight                   g/p
      REAL          GRWTM         ! Grain weight at maturity       g/p
      REAL          GRWTSGE       ! Grain weight,stem growth end   g/p
      REAL          GRWTTMP       ! Grain weight,temporary value   g/p
      !REAL          GSTAGE        ! Growth stage                   #
      INTEGER       GSTDCOL       ! Growth stage column number     #
      REAL          GWAD          ! Grain weight                   kg/ha
      REAL          GWAHM         ! Grain weight,harvest,measured  kg/ha
      REAL          GWAM          ! Grain weight,maturity          kg/ha
      REAL          GWAMM         ! Grain weight,maturity,measured kg/ha
      REAL          GWLINFR       ! Grain weight end linear,fr     #
      REAL          GWLAGFR       ! Grain weight end lag,fraction  #
      REAL          GWPH          ! Grain wt harvested             g/p
      REAL          GWPHC         ! Grain wt harvested,cumulative  g/p
      REAL          GWTA          ! Cultivar coeff,gr.wt.adjusted  mg
      REAL          GWTAF         ! Grain weight adjustment factor fr
      REAL          GWTAS         ! Cultivar coeff,gr.wt.adj.strss mg
      REAL          GWTAT         ! Grain weight adj threshold     oC
      REAL          GWTS          ! Cultivar coefficient,grain wt  mg
      REAL          GWUD          ! Grain size                     g
      REAL          GWUDELAG      ! Grain size,end lag period      g
      REAL          GWUM          ! Grain size,maturity            g
      REAL          GWUMM         ! Grain wt/unit,mat,measured     g
      INTEGER       GYEARDOY      ! Germination Year+DOY           #
      REAL          GrP_EP        ! Harvest product per unit EP    g/mm
      REAL          GrP_Irr       ! Harvest dm per unit irrigation g/mm
      REAL          GrP_NApp      ! Harvest dm per unit N appllied kg/kg
      REAL          GrP_NUpt      ! Harvest dm per unit N taken up kg/kg
      REAL          H2OA          ! Water available in root zone   mm
      REAL          NUPWF         ! Water factor for N uptake      #
      REAL          H2OPROFILE    ! Total h2o in soil profile      mm   
      REAL          H2OROOTZONE   ! Total h2o in root zone         mm   
      INTEGER       HADOY         ! Harvest day of year            d
      REAL          HAFR          ! Harvested fraction             kg/ha
      REAL          HAMT(HANUMX)  ! Harvest amount                 #
      INTEGER       HANUM         ! Harvest instruction number     # 
      REAL          HARDAYS       ! Accumulated hardening days     #
      REAL          HARDILOS      ! Hardening index loss           #
      !REAL          HARVFRAC(2)   ! Harvest fraction as brought in #
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
      REAL          HLOSF         ! Fraction hardiness days lost   fr
      REAL          HLOST         ! Temp threshold,hardiness loss  C
      REAL          HNAD          ! Product N                      kg/ha
      REAL          HNAM          ! Grain N at maturity            kg/ha
      REAL          HNAMERR       ! Harvest N,error                %
      REAL          HNAMM         ! Harvest N,mature,measured      kg/ha
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
      REAL          HWUMERR       ! Grain wt per unit error        %
      REAL          HWUMM         ! Hprod wt/unit,mat,measured     g
      REAL          HWUMYLD       ! Harest wt,mature,calculated    g/#
      REAL          HWUT          ! Product weight/unit,t file     mg
      REAL          HYAMM         ! Harvest product,msured,std.h2o kg/ha
      INTEGER       HYEAR         ! Harvest year as read           #
      INTEGER       HYEARDOY(HANUMX)! Dates of harvest operations    #
      INTEGER       HYEARF        ! Earliest year for harvest      #
      INTEGER       HYEARL        ! Last year for harvest          #
      INTEGER       HYRDOY(HANUMX)! Dates of harvest operations    #
      INTEGER       I             ! Loop counter                   #
      REAL          ICWD          ! Initial water table depth      cm
      INTEGER       IDATE1        ! First irrigation Yrdoy         #
      INTEGER       IDETGNUM      ! Number of times into IDETG     #
      INTEGER       IESTG         ! Inflorescence emergence stage  #
      !REAL          IRRAMT        ! Irrigation amount for today    mm
      REAL          IRRAMTC       ! Irrigation amount,cumulative   mm
      REAL          ISOILH2O      ! Initial soil water             cm   
      !REAL          KCAN          ! Extinction coeff for PAR       #
      REAL          KCANI         ! Extinction coeff,PAR,init.val. #
      !REAL          KEP           ! Extinction coeff for SRAD      #
      REAL          KEPI          ! Extinction coeff,SRAD,init val #
      INTEGER       KEYPS(KEYSTX) ! Principal key stage number     #
      INTEGER       KEYPSNUM      ! Principal key stage total #    # 
      INTEGER       KEYSS(KEYSTX) ! Secondary key stage number     #
      INTEGER       KEYSSNUM      ! Secondary key stage total #    #
      INTEGER       L             ! Loop counter                   #
      INTEGER       L1            ! Loop counter                   #
      INTEGER       L2            ! Loop counter                   #
      REAL          LA1S          ! Area of leaf 1,standard        cm2
      REAL          LAFR          ! Leaf area increase factor,rep  #
      REAL          LAFST         ! Leaf area factor change stage  #
      REAL          LAFSTDU       ! Leaf area factor change st,DU  #
      REAL          LAFSWITCH     ! Leaf # changed increase factor #
      REAL          LAFV          ! Leaf area increase factor,veg  #
      REAL          LAGEDU(0:LNUMX)! Leaf age at leaf position     C.d
      REAL          LAGEG(0:LNUMX)! Leaf age increment             C.d
      REAL          LAGEP(0:LNUMX)! Leaf age (phyllochrons),lf pos #
!      REAL          LAI           ! Leaf area index                #
      REAL          LAIA          ! Leaf area index,active         #
      INTEGER       LAIDCOL       ! Leaf area index column         #
!      REAL          LAIL(30)      ! Leaf area index by layer       m2/m2
!      REAL          LAILA(30)     ! Leaf area index,active,by layr m2/m2
      REAL          LAIPREV       ! Leaf area index,previous day   #
      REAL          LAIPROD       ! Leaf area index produced       #
      REAL          LAISTG(20)    ! Leaf area index,specific stage #
      REAL          LAIX          ! Leaf area index,maximum        #
      REAL          LAIXERR       ! Leaf area index,max,error      %
      REAL          LAIXM         ! Lf lamina area index,mx,meas   m2/m2
      REAL          LAIXT         ! Leaf area index,max,t-file     m2/m2
      REAL          LAIXX         ! Leaf area index,max posible    #
      REAL          LANC          ! Leaf actual N concentration    #
      REAL          LAP(0:LNUMX)  ! Leaf area at leaf position     cm2/p
      REAL          LAPD          ! Leaf area (green) per plant    cm2
      REAL          LAPH          ! Leaf area (green) harvested    cm2/d
      REAL          LAPHC         ! Leaf area (green) harvested,cu cm2/p
      INTEGER       LAPOTCHG      ! Leaf area gr pot,increment chg cm2/p
      REAL          LAPOTX(LNUMX) ! Leaf area potentials,maxima    cm2/l
      REAL          LAPOTXCHANGE  ! Leaf area potential,changover  cm2/l
      REAL          LAPP(LNUMX)   ! Leaf area diseased,leaf posn   cm2/p
      REAL          LAPS(LNUMX)   ! Leaf area senesced,leaf posn   cm2/p
      REAL          LAPSTMP       ! Leaf area senesced,temporary   cm2/p
      REAL          LATL(1,LNUMX) ! Leaf area,tiller1,leaf pos     cm2/l
      REAL          LAWCF         ! Leaf area/wt change,fr.st      fr/lf
      REAL          LAWFF         ! Leaf area/wt flexibility,fr.st fr
      REAL          LAWL(2)       ! Area to wt ratio,n=youngest lf cm2/g
      REAL          LAWS          ! Leaf area/wt ratio,standard    cm2/g
      REAL          LAWTR         ! Leaf area/weight,temp response  fr/C
      REAL          LAWTS         ! Leaf area/weight,temp standard  C
      REAL          LAWWR         ! Leaf area/weight,water response fr
      REAL          LAXS          ! Area of biggest leaf,main stem cm2
      INTEGER       LCNUM         ! Leaf cohort number (inc.grow)  #
      REAL          LCOA(LCNUMX)  ! Leaf cohort area               cm2
      REAL          LCOAS(LCNUMX) ! Leaf cohort area senesced      cm2
      REAL          LEAFN         ! Leaf N                         g/p
      REAL          LEAFNEXCESS   ! Leaf N > critical              g/p
      INTEGER       LENDIS        ! Length,ISWDIS flag             #
      INTEGER       LENENAME      ! Length,experiment description  #
      INTEGER       LENGROUP      ! Length of group name           #
      INTEGER       LENLINE       ! Length of character string     #
      INTEGER       LENLINESTAR   ! Length of character string     #
      INTEGER       LENRNAME      ! Length of run description      #
      INTEGER       LENTNAME      ! Length,treatment description   #
      REAL          LFENDFR       ! Fraction of day leaves growint #
      REAL          LFGSDU        ! Leaf growth start,d.units      #
      REAL          LFGSCUMDU     ! Cumulative DU during leaf ph   #
      REAL          LFWAA         ! Leaf weight at anthesis        g/m2 
      REAL          LFWAAM        ! Leaf weight,anthesis,measured  g/m2 
      REAL          LFWT          ! Leaf weight                    g/p
      REAL          LFWTA         ! Leaf weight,anthesis           g/p
      REAL          LFWTAE        ! Leaf weight,anthesis end       g/p
      REAL          LFWTM         ! Leaf weight,maturity           g/p
      REAL          LFWTSGE       ! Leaf weight,stem growth end    g/p
      REAL          LGPHASE(2)    ! Leaf growth phase start,end    #
      REAL          LGPHASEDU(2)  ! Leaf growth phase,start,end    Du
      INTEGER       LINENUM       ! Line number in RMSE values     #
      !REAL          LL(20)        ! Lower limit,soil h2o           #
      REAL          LLIFA         ! Leaf life duration,active,phyl #    
      REAL          LLIFATT       ! Leaf life duration,active      C.d
      REAL          LLIFEG(0:LNUMX) ! Leaf expansion growth,phyll  #    
      REAL          LLIFG         ! Leaf growth phase,phyll        #   
      REAL          LLIFGTT       ! Leaf growth phase              C.d
      REAL          LLIFS         ! Leaf senescence duration       phyl
      REAL          LLIFSTT       ! Leaf senescence duration,Ttime C.d
      REAL          LLIGPC        ! Leaf lignin percentage         #
      REAL          LLNAD         ! Leaf lamina nitrogen           kg/ha
      REAL          LLOSA         ! Leaf area loss,accelerated sen fr
      REAL          LLRSWAD       ! Leaf lamina reserves weight    kg/ha
      REAL          LLRSWT        ! Leaf lamina reserves           g/p
      INTEGER       LLSTG         ! Last leaf fully expanded stage #
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
      REAL          LNCSENF       ! Leaf N con,senescence,final    fr
      REAL          LNCTL         ! Leaf N conc,tillering,lower    fr
      REAL          LNCTU         ! Leaf N conc,tillering,upper    fr
      REAL          LNCX          ! Leaf N conc,maximum            fr
      REAL          LNCXS(0:1)    ! Leaf N conc,maximum,stage      fr
      REAL          LNDEM         ! Leaf demand for N              g/p
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
      REAL          LNUMSM        ! Leaf #/shoot,Haun,maturity     #
      REAL          LNUMSMM       ! Leaf #,mature,measured         #/s
      REAL          LNUMSTG(20)   ! Leaf number,specific stage     #
      REAL          LNUMT         ! Leaf number from t file        #
      REAL          LNUMTS        ! Leaf number,terminal spikelet  #
      REAL          LNUSE(0:2)    ! Leaf N use,overall and parts   g   
      REAL          LRETS         ! Stage --> dead leaves retained #
      REAL          LRETSDU       ! Stage --> dead leaves retained PVoCd
      INTEGER       LRTIP         ! Layer with root tip            #
      REAL          LSHAW         ! Leaf sheath area/wt            cm2/g
      REAL          LSHAR         ! Leaf sheath area/wt,end leaf   cm2/g
      REAL          LSHAV         ! Leaf sheath area/wt,veg.phases cm2/g
      INTEGER       LSEED         ! Layer with seed                #
      REAL          LSPHE         ! Leaf senescence,end stage      #
      REAL          LSPHEDU       ! Leaf senescence,end stage      #
      REAL          LSENI         ! Leaf senescence,injury         %/d
      REAL          LSPHS         ! Leaf senescence,start stage    #
      REAL          LSPHSDU       ! Leaf senescence,start stage    PVoCd
      REAL          LSHAI         ! Leaf sheath area index         m2/m2
      REAL          LSHFR         ! Leaf sheath fraction of total  #
      REAL          LSHRSWAD      ! Leaf sheath reserves weight    kg/ha
      REAL          LSHRSWT       ! Leaf sheath reserves           g/p
      REAL          LSHWAD        ! Leaf sheath weight             kg/ha
      REAL          LSNUM(HANUMX) ! Livestock number               #/ha
      REAL          LSTAGE        ! Leaf stage 0-1 over leaf phase #
      REAL          LSWT(HANUMX)  ! Livestock weight (individual)  kg
      REAL          LWLOS         ! Leaf wt loss,normal senesce    fr
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
      REAL          NFG           ! N factor,growth 0-1            #
      REAL          NFGCAV        ! N factor,growth,average,cycle  #
      REAL          NFGCC         ! N factor,growh,cycle sum       # 
      REAL          NFGL          ! N factor,gr,lower limit        #
      REAL          NFGPAV(0:12)  ! N factor,growth,average,phase  #
      REAL          NFGPC         ! N factor,growth,cumulative     #
      REAL          NFGU          ! N factor,gr,upper limit        #
      REAL          NFLF(LNUMX)   ! N factor for leaf,average      #
      REAL          NFLFP(LNUMX)  ! N factor phs leaf,average      #
      !REAL          NFP           ! N factor,photosynthesis 0-1    #
      REAL          NFPCAV        ! N factor,phs,average,cycle     #
      REAL          NFPCC         ! N factor,phs,cumulative,cycle  #
      REAL          NFPL          ! N factor,phs,lower limit       #
      REAL          NFPPAV(0:12)  ! N factor,phs,average,phase     #
      REAL          NFPPC         ! N factor,phs,cumulative,phase  #
      REAL          NFPU          ! N factor,phs,upper limit       #
      REAL          NFRG          ! N factor,root growth 0-1       #
      REAL          NFSU          ! N factor,senescence            #
      REAL          NFSF          ! N factor,final sen.trigger fr  #
      REAL          NFT           ! N factor,tillering 0-1         #
      REAL          NFTL          ! N factor,tillering,lower limit #
      REAL          NFTU          ! N factor,tillering,upper limit #
      REAL          NH4FN         ! NH4 conc factor,NH4 uptake 0-1 #
      !REAL          NH4LEFT(20)   ! NH4 concentration in soil      g/Mg
      REAL          NH4MN         ! NH4 conc minimum for uptake    g/Mg
      REAL          NLABPC        ! N labile fraction,standard     %
      !INTEGER       NLAYR         ! Number of layers in soil       #
      INTEGER       NLAYRROOT     ! Number of layers with roots    #
      INTEGER       NLIMIT        ! N limited grain growth  (Days) #
      REAL          NUPCF         ! N uptake cultivar factor       #    
      REAL          NUPNF         ! N uptake concentration factor  #    
      REAL          NO3FN         ! NO3 conc factor,NO3 uptake 0-1 #
      !REAL          NO3LEFT(20)   ! NO3 concentration in soil      g/Mg
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
      REAL          NUSEFAC       ! N use factor;mx nuselim        #
      REAL          NUSELIM       ! N limit on N for grain filling #
      !INTEGER       ON            ! Option number (sequence runs)  #
      INTEGER       ONI           ! Option number,initial value    #
      INTEGER       OUTCOUNT      ! Output counter                 #
      INTEGER       OUTCHOICE     ! Output choice (+reserves,etc)  #
      REAL          PARFC         ! Max photosynthesis/phs at 330  #
      REAL          PARI          ! PAR interception fraction      #
      REAL          PARI1         ! PAR interception fr,1-crop mdl #
      REAL          PARIOUT       ! PAR interception fr for output #
      REAL          PARIPREV      ! PAR interception fr,previous   #
      !REAL          PARIP         ! PAR interception percentage    %
      !REAL          PARIPA        ! PAR interception %, active     %
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
      REAL          PARURFR       ! PAR utilize factor,reprod fr   #
      INTEGER       PATHL         ! Path length                    #
      REAL          PD3AITKEN     ! Phase 3 duration,Aitken formla deg.d
      REAL          PD3NEW        ! Phase 3 duration,after check   deg.d
      REAL          PD(0:PSX)     ! Phase durations                deg.d
      REAL          PDMOD(0:PSX)  ! Phase durations modified       deg.d
      REAL          PDADJ         ! Phase duration > ts as adjsted deg.d
      INTEGER       PDATE         ! Planting Yrdoy from X-file     #
      INTEGER       PDAYS(0:12)   ! Phase durations                PVoCd
      REAL          PDFS          ! Phase duration,final senescenc deg.d
      REAL          PDL(0:10)     ! Phase durations,phint units    #    
      REAL          PDMTOHAR      ! Phase duration,mature->harvest deg.d
      REAL          PEMRG         ! Phase duration,emergence       Cd/cm
      REAL          PEGD          ! Phase duration,germ+dormancy   deg.d
      REAL          PFGCAV        ! P factor,growh,cycle,av 0-1    # 
      REAL          PFGPAV(0:12)  ! P factor,growh,phase,av 0-1    # 
      REAL          PFPCAV        ! P factor,phs,cycle,average 0-1 #
      REAL          PFPPAV(0:12)  ! P factor,phs,phase,average 0-1 #
      INTEGER       PGDAP         ! Plantgro file days after plt   #
      REAL          PGERM         ! Phase duration,germination     deg.d
      INTEGER       PGROCOL(20)   ! Plantgro column = t-file data  #
      REAL          PGVAL         ! Plantgro file value            #
      REAL          PHINT         ! Phylochron interval            deg.d
      REAL          PHINTSTORE    ! Phylochron interval,stored     deg.d
      REAL          PHINTF(PHSX)  ! Phylochron interval,factor lf# #
      REAL          PHINTL(PHSX)  ! Phylochron interval,change lf# #
      REAL          PHINTOUT      ! Phylochron interval,adjusted   deg.d
      REAL          PHINTS        ! Phylochron interval,standard   deg.d
      INTEGER       PHINTSTG      ! Phylochron stage               #    
      REAL          PHOTQR        ! Photon requirement,calculated  E/mol
      REAL          PHSV          ! Phs,fr reduction with VPD       /KPa
      REAL          PHTV          ! Phs,threshold VPD for reduction KPa
      REAL          PLA           ! Plant leaf area                cm2
      REAL          PLAG(2)       ! Plant leaf area growth,tiller1 cm2/t
      REAL          PLAGLF(LNUMX) ! Plant leaf area growth,by leaf cm2/t
      REAL          PLAGT(2)      ! Plant leaf area growth,total   cm2/p
      REAL          PLAGTP(2)     ! Plant lf area growth,potential cm2/p
      REAL          PLAGTTEMP     ! Plant leaf area gr,total,temp. cm2/p
      REAL          PLAS          ! Leaf area senesced,normal      cm2/p
      REAL          PLASC         ! Leaf area senesced,cold        cm2/p
      REAL          PLASCSUM      ! Leaf area senesced,cold,summed cm2/p
      REAL          PLASFS        ! Leaf area senesced,final sen   cm2/p
      REAL          PLASI         ! Leaf area senesced,injury      cm2/p
      REAL          PLASL         ! Leaf area senesced,low light   cm2/p
      REAL          PLASN         ! Leaf area senesced,N shortage  cm2/p
      REAL          PLASP         ! Leaf area senesced,phyllochron cm2/p
      REAL          PLASPM        ! Leaf area senesced,post mature cm2/p
      REAL          PLASS         ! Leaf area senesced,stress      cm2/p
      REAL          PLAST         ! Leaf area senesced,tiller loss cm2/p
      REAL          PLAST1        ! LA senesced,tiller,youngest co cm2/p
      REAL          PLAST2        ! LA senesced,tiller,2ndyonug co cm2/p
      REAL          PLASTMP       ! Leaf area senesced,temporary   cm2/p
      REAL          PLASTMP2      ! Leaf area senesced,temporary   cm2/p
      REAL          PLASW         ! Leaf area senesced,h2o stress  cm2/p
      REAL          PLAX          ! Plant leaf area,maximum        cm2
      INTEGER       PLDAY         ! Planting day of year           d
      INTEGER       PLDAYTMP      ! Planting day of year           #
      REAL          PLMAGE        ! Planting material age          d
      INTEGER       PLTOHARYR     ! Planting to harvest years      #
      REAL          PLPH          ! Plants/hill or shoots/cutting  # 
      REAL          PLTLOSS       ! Plant popn lost through cold   #/m2
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
      INTEGER       PSDAP  (0:PSX)! Stage DAP                      #
      REAL          PSDAPFR(0:PSX)! Stage DAP+fr                   #
      INTEGER       PSDAPM (0:PSX)! Stage DAP,measured             #
      INTEGER       PSDAT  (0:PSX)! Stage YrDoydate                #
      INTEGER       PSDATM (0:PSX)! Stage date,measured            #
      REAL          PSDAYFR(0:PSX)! Stage fraction of day          #
      INTEGER       PSIDAP        ! Principal stage,inter,date     dap
      INTEGER       PSIDAPM       ! Principal stg,inter,measured   dap
      INTEGER       PSIDATERR     ! Principal stage,inter,error    dap 
      INTEGER       PSNUM         ! Principal stage number         #
      REAL          PSTART(0:PSX) ! Principal phase thresholds     du
      REAL          PTF           ! Partition fraction to tops     #
      REAL          PTFA          ! Partition fr adjustment coeff. #
      REAL          PTFMN         ! Partition fraction,minimum     #
      REAL          PTFMX         ! Partition fraction,maximum     #
      REAL          PTTN          ! Minimum soil temperature,plt   C
      REAL          PTFXS         ! Growth stage for PTFMX         #
      REAL          PTX           ! Maximum soil temperature,plt   C
      INTEGER       PWDINF        ! First YrDoy of planting window #
      INTEGER       PWDINL        ! Last YrDoy of planting window  #
      INTEGER       PWDOYF        ! First doy of planting window   #
      INTEGER       PWDOYL        ! Last doy of planting window    #
      INTEGER       PWYEARF       ! First year of planting window  #
      INTEGER       PWYEARL       ! Last year of planting window   #
      !REAL          RAIN          ! Rainfall                       mm
      REAL          RAINC         ! Rainfall,cumulative            mm
      REAL          RAINCA        ! Rainfall,cumulativ to anthesis mm
      REAL          RAINCC        ! Precipitation cycle sum        mm 
      REAL          RAINPAV(0:12) ! Rainfall,average for phase     mm
      REAL          RAINPC(0:12)  ! Precipitation phase sum        mm
      REAL          RANC          ! Roots actual N concentration   #
      REAL          RATM          ! Boundary layer,air,resistance  s/m
      REAL          RB            ! Leaf resistance addition fac   s/m
      REAL          RCROP         ! Stomatal res,crop basis        s/m
      REAL          RDGAF         ! Root depth gr,acceleration fac #
      REAL          RDGS          ! Root depth growth rate,standrd cm/d
      !INTEGER       REP           ! Number of run repetitions      #
      REAL          RESCAL(0:20)  ! Residue C at harvest,by layer  kg/ha
!      REAL          RESCALG(0:20) ! Residue C added,by layer       kg/ha
      REAL          RESLGAL(0:20) ! Residue lignin,harvest,bylayer kg/ha
!      REAL          RESLGALG(0:20)! Residue lignin added,layer     kg/ha
      REAL          RESNAL(0:20)  ! Residue N at harvest by layer  kg/ha
!      REAL          RESNALG(0:20) ! Residue N added,by layer       kg/ha
      REAL          RESPC         ! Respiration,total,cumulative   g/p
      REAL          RESPGF        ! Respiration,grain fill         g/p
      REAL          RESPRC        ! Respiration,roots,cumulative   g/p
      REAL          RESPTC        ! Respiration,tops,cumulative    g/p
      REAL          RESWAL(0:20)  ! Residue om added by layer      kg/ha
      REAL          RESWALG(0:20) ! Residue om at harvest,by layer kg/ha
      REAL          RFAC          ! Root length & H2O fac,N uptake #
      REAL          RLDF(20)      ! Root length density fac,new gr #
      REAL          RLF           ! Leaf stomatal res,330.0 ppmCO2 s/m
      REAL          RLFC          ! Leaf stomatal resistance       s/m
      REAL          RLFN          ! Root length factor,N           #
      REAL          RLFWU         ! Root length factor,water uptk  /cm2
      REAL          RLIGPC        ! Root lignin concentration      %
      !REAL          RLV(20)       ! Root length volume by layer    cm-2
      REAL          RLWR          ! Root length/weight ratio     m/10mg
      REAL          RM            ! Mesophyll resistance           d/m
      REAL          RMSE(30)      ! Root mean square error values  #
!      INTEGER       RN      !       Treatment replicate            #
      REAL          RNAD    !       Root N                         kg/ha
      REAL          RNAM    !       Root N at maturity             kg/ha
      REAL          RNAMM   !       Root N at maturity,measured    kg/ha
      REAL          RNCM    !       Root N conc,minimum            fr
      REAL          RNCMN(0:1)    ! Root N conc,minimum            fr
      REAL          RNCR    !       Roots N relative to maximum    #
      REAL          RNCX    !       Root N concentration,maximum   fr
      REAL          RNCXS(0:1)    ! Roots N conc,maximum,by stage  fr
      REAL          RNDEM   !       Root demand for N              g/p
      REAL          RNH4U(20)     ! Potential ammonium uptake      kg/ha
      INTEGER       RNI     !       Replicate number,initial value #
      REAL          RNO3U(20)     ! Potential nitrate uptake       kg/ha
      REAL          RNPCMN(0:1)   ! Root N conc,minimum            %
      REAL          RNPCS(0:1)    ! Roots N conc,standard,by stage %
      REAL          RNUMX   !       Root N uptake,maximum          mg/cm
      REAL          RNUSE(0:2)    ! Root N use,overall and parts   g   
      REAL          ROOTN   !       Root N                         g/p
      REAL          ROOTNEXCESS   ! Root N > critical              g/p
      REAL          ROOTNS  !       Root N senesced                g/p
      REAL          ROWSPC  !       Row spacing                    cm
      INTEGER       RPCOL   !       Replicate column number        #
      REAL          RRESP   !       Root respiration fraction      #
      REAL          RSPCA   !       Reserves conc,anthesis         %
      REAL          RSCD    !       Reserves concentration,end day fr
      REAL          RSPCLX  !       Reserves conc,leaves,max.      #
      REAL          RSCM    !       Reserves concentration,mature  fr
      REAL          RSCMM   !       Reserves conc,maturity,msured  #
      REAL          RSPCX   !       Reserves conc.max.->overflow   #
      REAL          RSCX    !       Reserves concentration,maximum fr
      REAL          RSEN    !       Root senescence                %/d
      REAL          RSFP    !       Reserves factor,photosynthesis fr
      REAL          RSFPL   !       Reserves conc.,phs.lower bound fr
      REAL          RSFPU   !       Reserves conc.,phs upper bound fr
      REAL          RSN     !       Reserve N                      g/p
      REAL          RSNAD   !       Reserve N                      kg/ha
      REAL          RSNEED  !       Reserves need to bring to min  g/p
      REAL          RSNPH   !       Reserves N harvested           g/p
      REAL          RSNPHC  !       Reserves N harvested,cum       g/p
      REAL          RSNUSED !       Reserve N used                 g/p
      REAL          RSTAGE  !       Reproductive develoment stage  #
      REAL          RSTAGEFS!       Rstage when final sen started  #
      REAL          RSTAGEP !       Reproductive dc stage,previous #
      REAL          RSTAGETMP     ! Reproductive develoment stage  #
      REAL          RSUSE   !       Reserves utilisation fraction  #
      REAL          RSWAA   !       Reserve weight,anthesis        g/p 
      REAL          RSWAAM  !       Reserve wt,anthesis,measured   g/p
      REAL          RSWAD         ! Reserves weight                kg/ha
      REAL          RSWADPM       ! Reserves weight,post maturity  kg/ha
      REAL          RSWAM         ! Reserves at maturity           kg/ha
      REAL          RSWAMM        ! Reserves at maturity,measured  kg/ha
      REAL          RSWPH         ! Reserves weight harvested      g/p
      REAL          RSWPHC        ! Reserves wt harvested,cum      g/p
      REAL          RSWT          ! Reserves weight                g/p
      REAL          RSWTA         ! Reserves weight,anthesis       g/p
      REAL          RSWTAE        ! Reserves weight,anthesis end   g/p
      REAL          RSWTM         ! Reserves weight,maturity       g/p
      REAL          RSWTPM        ! Reserves weight,post maturity  g/p
      REAL          RSWTSGE       ! Reserves weight,stem gr end    g/p
      REAL          RSWTTMP       ! Reserves weight,temporary val  g/p
      REAL          RSWTX         ! Reserves weight,maximum        g/p
      REAL          RTDEP         ! Root depth                     cm
      REAL          RTDEPG        ! Root depth growth              cm/d
      REAL          RTDEPTMP      ! Root depth,temporary value     cm/d
      REAL          RTNUP         ! N uptake/root length           mg/cm
      REAL          RTNSL(20)     ! Root N senesced by layer       g/p
      REAL          RTRESP        ! Root respiration               g/p
      INTEGER       RTSLXDATE     ! Roots into last layer date     YYDDD
      REAL          RTUFR         ! Max fraction root wt useable   fr
      REAL          RTWT          ! Root weight                    g/p
      REAL          RTWTAL(20)    ! Root weight by layer           kg/ha
      REAL          RTWTG         ! Root weight growth             g/p
      REAL          RTWTGRS       ! Root growth from surplus rs    g/p
      REAL          RTWTGL(20)    ! Root weight growth by layer    g/p
      REAL          RTWTL(20)     ! Root weight by layer           g/p
      REAL          RTWTM         ! Root weight,maturity           g/p
!      REAL          RTWTSGE       ! Root weight,stem growth end    g/p
      REAL          RTWTSL(20)    ! Root weight senesced by layer  g/p
      REAL          RTWTUL(20)    ! Root weight used for tops,lyr  g/p
      REAL          RUESTG        ! Stage at which RUE changes     #
      REAL          RUESTGDU      ! Stage at which RUE changes     PVoCD
!      INTEGER       RUN           ! Run (from command line) number #
      INTEGER       RUNCRP        ! Run (internal within module)   #
!      INTEGER       RUNI          ! Run (internal for sequences)   #
!      REAL          RUNOFF        ! Calculated runoff              mm/d
      REAL          RUNOFFC       ! Calculated runoff,cumulative   mm   
      REAL          RWAD          ! Root weight                    kg/ha
      REAL          RWAM          ! Root weight,maturity           kg/ha
      REAL          RWAMM         ! Root wt at maturity,measured   kg/ha
!      REAL          RWUMX         ! Root water uptake,max cm3/cm.d cm2.d
      REAL          RWUMXI        ! Root water uptake,max,init.val cm2/d
!      REAL          RWUPM         ! Pore size for maximum uptake   fr
      REAL          SAID          ! Stem area index                m2/m2
      REAL          SANC          ! Stem N concentration           #
      REAL          SANCOUT       ! Stem+LeafSheaths N conc        #
      !REAL          SAT(20)       ! Saturated limit,soil           #
      REAL          SAWS          ! Stem area to wt ratio,standard cm2/g
      REAL          SDCOAT        ! Non useable material in seed   g
      REAL          SDDUR         ! Seed reserves use phase duratn d
      REAL          SDEPTH        ! Sowing depth                   cm
      REAL          SDEPTHU       ! Sowing depth,uppermost level   cm
      REAL          SDNAD         ! Seed N                         kg/ha
      REAL          SDNAP         ! Seed N at planting             kg/ha
      REAL          SDNC          ! Seed N concentration           #
      REAL          SDNPCI        ! Seed N concentration,initial   %
      REAL          SDRATE        ! Seeding 'rate'                 kg/ha
      REAL          SDRSPC        ! Seed reserves % of seed        #
      REAL          SDWT          ! Seed size                      g
      REAL          SDWAD         ! Seed weight                    kg/ha
      REAL          SDWAM         ! Seed at at maturity            kg/ha
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
!      REAL          SENCALG(0:20) ! Senesced C added,by layer      kg/ha
      REAL          SENCAS        ! Senesced C added to soil       kg/ha
      REAL          SENCL(0:20)   ! Senesced C,by layer            g/p
      REAL          SENCS         ! Senesced C added to soil       g/p
      REAL          SENFR         ! Senesced fraction lost from pl #
      REAL          SENGF         ! Senesced during grain fill     g/p
      REAL          SENLA         ! Senesced leaf area,total       cm2/p
      REAL          SENLALITTER   ! Senesced leaf area,litter      cm2/p
      REAL          SENLARETAINED ! Senesced leaf area,retained    cm2/p
      REAL          SENLAGS       ! Senesced lignin added to soil  kg/ha
!      REAL          SENLALG(0:20) ! Senesced lignin added,layer    kg/ha
      REAL          SENLAS        ! Senesced lignin added to soil  kg/ha
      REAL          SENLFG        ! Senesced leaf                  g/p
      REAL          SENLFGRS      ! Senesced leaf to reserves      g/p
      REAL          SENLL(0:20)   ! Senesced lignin added,by layer g/p
      REAL          SENLS         ! Senesced lignin added to soil  g/p
      REAL          SENNAGS       ! Senesced N added to soil       kg/ha
      REAL          SENNAL(0:20)  ! Senesced N,by layer            kg/ha
!      REAL          SENNALG(0:20) ! Senesced N added,by layer      kg/ha
      REAL          SENNAS        ! Senesced N added to soil       kg/ha
      REAL          SENNATC       ! Senesced N,litter+soil,cum     kg/ha
      REAL          SENNATCM      ! Senesced N,litter+soil,cum,mes kg/ha
      REAL          SENNGS        ! Senesced N added to soil       g/p
      REAL          SENNL(0:20)   ! Senesced N,by layer            g/p
      REAL          SENNLFG       ! Senesced N from leaves         g/p
      REAL          SENNLFGRS     ! Senesced N from leaves,to rs   g/p
      REAL          SENNRS        ! Senescence (loss) N reserves   g/p
      REAL          SENNS         ! Senesced N added to soil       g/p
      REAL          SENNSTG       ! Senesced N from stems          g/p
      REAL          SENNSTGRS     ! Senesced N to rs from stems    g/p
      REAL          SENRS         ! Senescence (loss) reserves     g/p
      REAL          SENRTG        ! Senescent root material growth g/p
      REAL          SENRTGGF      ! Senescent root,grain filling   g/p
      REAL          SENSTFR       ! Senesced stem fraction         fr/d
      REAL          SENSTG        ! Senesced material from stems   g/p
      REAL          SENSTGRS      ! Senesced stem to reserves      g/p
      REAL          SENTOPLITTERG ! Senescent top->litter growth   g/p
      REAL          SENTOPLITTERGGF ! Senescent top gr.,grain fill g/p
      REAL          SENWACM       ! Senesced weight,total,cum to m kg/ha
      REAL          SENWACMM      ! Senesced om,litter+soil,cum,ms kg/ha
      REAL          SENWAGS       ! Senesced weight added to soil  kg/ha
      REAL          SENWAL(0:20)  ! Senesced om by layer           kg/ha
      REAL          SENWALG(0:20) ! Senesced om added by layer     kg/ha
      REAL          SENWL(0:20)   ! Senesced om (cumulative),layer g/p
      REAL          SENROOT       ! Senesced weight,soil,cum       g/p
      REAL          SENROOTA      ! Senesced weight,soil,cumulativ kg/ha
      REAL          SERX          ! Shoot elongation rate,max      cm/Du
      REAL          SGEDAPFR      ! Stem growth end DAP+fr         #
      INTEGER       SGEDAPM       ! Stem growth end date,measured  #
      REAL          SGPHASE(2)    ! Stem growth phase start,end    #
      REAL          SGPHASEDU(2)  ! Stem growth phase start,end    Du
      !REAL          SHF(20)       ! Soil hospitality factor 0-1    #
      REAL          SHRTD         ! Shoot/root ratio               #
      REAL          SHRTM         ! Shoot/root ratio,maturity      #
      REAL          SHRTMM        ! Shoot/root ratio,maturity,meas #
      REAL          SLA           ! Specific leaf area             cm2/g
      REAL          SLAOUT        ! Specific leaf area for output  cm2/g
      REAL          SLIGPC        ! Stem lignin concentration      %
      !REAL          SLPF          ! Soil factor for photosynthesis %
      REAL          SMDFR         ! Soil moisture factor,N uptake  #
      !INTEGER       SN            ! Sequence number,crop rotation  #
      REAL          SNAD          ! Stem N (stem+sheath+rs)        kg/ha
      REAL          SNCM          ! Stem N conc,minimum            fr
      REAL          SNCMN(0:1)    ! Stem N conc,minimum            fr
      REAL          SNCR          ! Stem N relative to maximum     #
      REAL          SNCX          ! Stem N conc,maximum            fr
      REAL          SNCXS(0:1)    ! Stem N conc,maximum,stage      fr
      REAL          SNDEM         ! Stem demand for N              g/p
      REAL          SNH4(20)      ! Soil NH4 N                     kg/ha
      REAL          SNH4PROFILE   ! Soil NH4 N in profile          kg/ha
      REAL          SNH4ROOTZONE  ! Soil NH4 N in root zone        kg/ha
      INTEGER       SNI           ! Sequence number,as initiated   #
      REAL          SNO3(20)      ! Soil NO3 N                     kg/ha
      REAL          SNO3PROFILE   ! Soil NO3 N in profile          kg/ha
      REAL          SNO3ROOTZONE  ! Soil NO3 N in root zone        kg/ha
      !REAL          SNOW          ! Snow                           cm
      REAL          SNPCMN(0:1)   ! Stem N conc,minimum            %
      REAL          SNPCS(0:1)    ! Stem N conc,standard,stage     %
      REAL          SNPH          ! Stem N harvested               g/p
      REAL          SNPHC         ! Stem N harvested,cumulative    g/p
      REAL          SNUSE(0:2)    ! Shoot N use,overall and parts  g    
      REAL          SOILN         ! Soil inorganic N               kg/ha
      REAL          SOILNF        ! Soil inorganic N,final         kg/ha
      REAL          SOILNI        ! Soil inorganic N,initial       kg/ha
      REAL          SPANPOS       ! Position along kill,etc.range  fr
      REAL          SPNUMH        ! Spike number harvested         #  
      REAL          SPNUMHC       ! Spike number harvested,cumulat #  
      REAL          SPNUMHCM      ! Spike # harvested,cum,measured #  
      REAL          SPNUMHFAC     ! Spike # harvestd factor (0-1)  #  
      REAL          SPRL          ! Sprout/cutting length          cm
      !REAL          SRAD          ! Solar radiation               MJ/m2
      REAL          SRAD20        ! Solar radiation av,20 days     MJ/m2
      REAL          SRAD20ANTH    ! Solar radn av,20 days,anthesis MJ/m2
      REAL          SRAD20S       ! Solar radiation sum            MJ/m2
      REAL          SRADC         ! Solar radiation,cumulative     MJ/m2
      REAL          SRADCAV       ! Solar radiation,cycle average  MJ/m2
      REAL          SRADCC         ! Radiation,cycle sum           Mj/m2
      REAL          SRADD(20)     ! Solar radiation on specific d  MJ/m2
      REAL          SRADPAV(0:12) ! Solar radiation,phase average  MJ/m2
      REAL          SRADPC        ! Solar radiation,phase sum      MJ/m2
      REAL          SRADPREV      ! Solar radiation,previous day   MJ/m2
      INTEGER       SSDAP(SSX)    ! Stage DAP                      #
      REAL          SSDAPFR(SSX)  ! Stage DAP+fr                   #
      INTEGER       SSDAPM(SSX)   ! Stage DAP,measured             #
      INTEGER       SSDAT(SSX)    ! Secondary stage dates          Yrdoy
      INTEGER       SSDATM(SSX)   ! Stage date,measured            #
      REAL          SSDAYFR(SSX)  ! Stage fraction of day          #
      REAL          SSENF         ! Stem N loss fr when senesce    #
      INTEGER       SSNUM         ! Secondary stage number         #
      REAL          SSPHS         ! Stem senesce phase start       #
      REAL          SSPHE         ! Stem senesce phase end         #
      REAL          SSPHSDU       ! Stem senesce phase start       Du
      REAL          SSPHEDU       ! Stem senesce phase end         Du
      REAL          SSTG(SSX)     ! Secondary stage occurence,rstg #
      REAL          SSTH(SSX)     ! Secondary stage thresholds     du
      !REAL          ST(0:NL)      ! Soil temperature in soil layer C
      REAL          STAI          ! Stem area index                m2/m2
      REAL          STAIG         ! Stem area index,growth         m2/m2
      REAL          STAIS         ! Stem area index senesced       m2/m2
      REAL          STAISS        ! Stem area index,start senesce  m2/m2
      INTEGER       STARNUM       ! Star line number,as read file  #
      INTEGER       STARNUMM      ! Star line number,measured data #
      INTEGER       STARNUMO      ! Star line number,output file   #
      INTEGER       STARTPOS      ! Starting position in line      #
      REAL          STDAY         ! Standard day                   C.d/d
      REAL          STEMN         ! Stem N                         g/p
      REAL          STEMNEXCESS   ! Stem N > critical              g/p
      REAL          STEMNGL       ! Stem N growth from leaves      g/p
      !INTEGER       STEP          ! Step number                    #
      INTEGER       STEPNUM       ! Step number per day            #
      INTEGER       STGEDAT       ! Stem growth end date (Yrdoy)   #
      REAL          STGEFR        ! Stem growth end time,fr day    #
      !INTEGER       STGYEARDOY(20)! Stage dates (Year+Doy)         #
      REAL          STRESS(20)    ! Min h2o,n factors for growth   #
      REAL          STRESS20      ! 20d av.,min h2o,n gr.factors   #
      REAL          STRESS20GS    ! 20d stress factor,grain set    # 
      REAL          STRESS20N     ! 20d av.,n gr.factor            #
      REAL          STRESS20NS    ! 20d sum,n gr.factors           #
      REAL          STRESS20S     ! 20d sum.,min h2o,n gr.factors  #
      REAL          STRESS20W     ! 20d av.,h2o gr.factor          #
      REAL          STRESS20WS    ! 20d sum,h2o gr.factors         #
      REAL          STRESSN(20)   ! 20d n gr.factors               #
      REAL          STRESSW(20)   ! 20d h2o gr.factors             #
      REAL          STRSWAD       ! Stem reserves                  kg/ha
      REAL          STRSWT        ! Stem reserves                  g/p
      REAL          STSTAGE       ! Stem stage 0-1 over stem phase #
      REAL          STVSTG        ! Stem visible stage             #
      REAL          STVSTGDU      ! Stem visible stage             Du
      REAL          STVWT         ! Stem weight,visible part       g/p
      REAL          STVWTG        ! Stem weight,visible part,grwth g/p
      REAL          STWAA         ! Stem weight,anthesis           g
      REAL          STWAAM        ! Stem weight,anthesis,measured  g
      REAL          STWAD         ! Stem structural weight         kg/ha
      REAL          STWADOUT      ! Stem weight for output         kg/ha
      REAL          STWT          ! Stem weight                    g/p
      REAL          STWTA         ! Stem weight,anthesis           g/p
      REAL          STWTAE        ! Stem weight,anthesis end       g/p
      REAL          STWTM         ! Stem weight,maturity           g/p
      REAL          STWTSGE       ! Stem weight,stem growth end    g/p
      !REAL          SW(20)        ! Soil water content             #
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
      !REAL          TAIRHR(24)    ! Hourly air temperature         C
      REAL          TCAN          ! Canopy temperature             C
      REAL          TCDIF         ! Canopy temperature - air temp  C
      INTEGER       TDATANUM      ! Number of data from t-file     #
      !REAL          TDEW          ! Dewpoint temperature           C
      REAL          TDIFAV        ! Temperature difference,can-air C
      REAL          TDIFNUM       ! Temperature difference,# data  #
      REAL          TDIFSUM       ! Temperature difference,sum     C
      !REAL          TFAC4         ! Temperature factor function    #
      REAL          TFACBETA      ! Temperature factor function    #
      INTEGER       TFCOLNUM      ! T-file column number           #
      REAL          TFD           ! Temperature factor,development #
      INTEGER       TFDAP         ! T-file days after planting     #
      INTEGER       TFDAPCOL      ! T-file DAP column #            #
      REAL          TFDF          ! Temperature factor,dayl sens   #
      REAL          TFDNEXT       ! Temperature factor,development #
      REAL          TFG           ! Temperature factor,growth 0-1  #
      REAL          TFGEM         ! Temperature factor,germ,emrg   #
      REAL          TFGF          ! Temperature factor,gr fill 0-1 #
      REAL          TFGN          ! Temperature factor,grain N 0-1 #
      REAL          TFH           ! Temperature factor,hardening   #
 !     REAL          TFLA          ! Temperature factor,leaf ap 0-1 #
      REAL          TFLFLIFE      ! Temperature factor,leaf life   #
      REAL          TFLAW         ! Temperature factor,lf area/wt  #
      REAL          TFLF(LNUMX)   ! Temp factor for leaf,average   #
      REAL          TFP           ! Temperature factor,phs 0-1     #
      REAL          TFV           ! Temperature factor,vernalizatn #
      REAL          TFVAL         ! T-file value                   #
      REAL          TGR(22)       ! Tiller size relative to 1      #
      REAL          TI1LF         ! Tiller 1 site (leaf #)         #
      INTEGER       TIERNUM       ! Tier of data in t-file         #
      REAL          TIFAC         ! Tillering rate factor 0-2      #
      REAL          TILBIRTHL(25) ! Tiller birth leaf number       #
      INTEGER       TILDAP        ! Tillering start DAP            #
      INTEGER       TILDAPM       ! Tillering date,measured        #
      INTEGER       TILDAT        ! Tillering start date (YEARDOY) #
      REAL          TDPHE         ! Tiller death end stage         #
      REAL          TDPHEDU       ! Tiller death end stage         #
      REAL          TDFAC         ! Tiller death rate,fr (wt=2xst) #
      REAL          TDPHS         ! Tiller death start stage       #
      REAL          TDPHSDU       ! Tiller death start stage       #
      REAL          TILIFAC       ! Tiller initiation factor 0-1   #
      REAL          TILIP         ! Tiller initiation phase Leaves #
      REAL          TILPE         ! Tiller number growth end stg   #
      REAL          TILPEDU       ! Tiller number growth end stg   PVoCd
      REAL          TDSF          ! Tiller death stress 0-2 factor # 
      REAL          TILSW         ! Tiller standard weight         g/s
      REAL          TILWT         ! Tiller weight                  g/s
      REAL          TILWTGS       ! Tiller weight at grain set     g/s
      REAL          TILWTR        ! Tiller wt relative to standard #
      REAL          TIMENEED      ! Time needed to finish phase    fr
      REAL          TILNOX        ! Shoot number,maximum           #/p
      REAL          TKFH          ! Start kill temp.fully hardened C
      REAL          TKILL         ! Temperature for plant death    C
      REAL          TKSPN         ! Temperature span for kill,etc. C
      REAL          TKDLF         ! Start kill difference,leaves   C
      REAL          TKDTI         ! Start kill difference,tillers  C
      REAL          TKGF          ! TMAX at which 'haying off'     C 
      REAL          TKUH          ! Start kill temp,unhardened     C
      REAL          TLA(25)       ! Tiller leaf area produced      cm2
      REAL          TLAG(25)      ! Tiller leaf area growth        cm2
      REAL          TLAGP(25)     ! Tiller potential leaf area gr  cm2
      REAL          TLAS(25)      ! Tiller leaf area senesced      cm2
      REAL          TLCHC         ! Cumulative N leached>planting  kg/ha
      !REAL          TLCHD         ! N leached this day            kg/ha
      INTEGER       TLIMIT        ! Temp.limited grain gr (Days)   #
      INTEGER       TLINENUM      ! Temporary var,# lines in tfile #
      INTEGER       TLPOS         ! Position on temporary line     #
      !REAL          TMAX          ! Temperature maximum            C
      REAL          TMAXCAV       ! Temperature,maximum,cycle av   C
      REAL          TMAXCC        ! Temperature,max,cycle sum      C.d 
      REAL          TMAXGFILL     ! Temperature max.,grain fill    C
      REAL          TMAXM         ! Temperature maximum,monthly av C
      REAL          TMAXPAV(0:12) ! Temperature,maximum,phase av   C
      REAL          TMAXPC        ! Temperature,maximum,phase sum  C
      REAL          TMAXSUM       ! Temperature maximum,summed     C
      REAL          TMAXX         ! Temperature max during season  C
      REAL          TMEAN         ! Temperature mean (TMAX+TMIN/2) C
      REAL          TMEAN20       ! Temperature mean over 20 days  C
      REAL          TMEAN20ANTH   ! Temperature mean,20 d~anthesis C
      REAL          TMEAN20P      ! Temperature mean,20 d>planting C
      REAL          TMEAN20S      ! Temperature sum over 20 days   C
      REAL          TMEANPAV(0:12)! Temperature,mean,phase av      C
      REAL          TMEANCAV      ! Temperature,mean,cycle av      C
      REAL          TMEANCC        ! Temperature,mean,cycle sum    C.d  
      REAL          TMEAND(20)    ! Temperature mean,specific day  C
      REAL          TMEANE        ! Temp mean,germination-emerge   C
      REAL          TMEANEC       ! Temp sum,germination-emergence C
      REAL          TMEANG        ! Temp mean,planting-germination C
      REAL          TMEANGC       ! Temp sum,planting-germination  C
      REAL          TMEANNUM      ! Temperature means in sum       #
      REAL          TMEANPC       ! Temperature,mean,phase sum     C
      REAL          TMEANSUM      ! Temperature means sum          #
      REAL          TMEANSURF     ! Temperature mean,soil surface  C
      !REAL          TMIN          ! Temperature minimum            C
      REAL          TMINCAV       ! Temperature,minimum,cycle av   C
      REAL          TMINCC        ! Temperature,min,cycle sum     C.d  
      REAL          TMINM         ! Temperature minimum,monthly av C
      REAL          TMINN         ! Temperature min during season  C
      REAL          TMINPAV(0:12) ! Temperature,minimum,phase av   C
      REAL          TMINPC        ! Temperature,minimum,phase sum  C
      REAL          TMINSUM       ! Temperature minimum,summed     C
      !INTEGER       TN            ! Treatment number               #
      REAL          TNAD          ! Total nitrogen (tops+roots)    kg/ha
      REAL          TNAMM         ! Total N at maturity,measured   kg/ha
      INTEGER       TNI           ! Treatment number,initial value #
      !REAL          TNIMBSOM      ! Total N immobilised by SOM    kg/ha
      REAL          TNOXC         ! Cumulative N denitrified       kg/ha
      !REAL          TNOXD         ! N denitrified this day        kg/ha
      REAL          TNUM          ! Tiller (incl.main stem) number #/p
      REAL          TNUMAD        ! Tiller (incl.main stem) number #/m2
      REAL          TNUMAERR      ! Shoot #,error                  %
      REAL          TNUMAM        ! Tiller (+main stem) #,maturity #/m2
      REAL          TNUMAMM       ! Shoot #,mature,measured        #/m2
      INTEGER       TNUMCOL       ! Treatment number column        #
      REAL          TNUMD         ! Tiller number death            #/p
      REAL          TNUMG         ! Tiller number growth           #/p
      REAL          TNUMIFF       ! Tiller number fibonacci factor #
      REAL          TNUML(LNUMX)  ! Tiller # at leaf position      #/p
      REAL          TNUMLOSS      ! Tillers lost through death     #/p
      REAL          TNUMPM        ! Tiller (+main stem) #,maturity #/p
      REAL          TNUMPMM       ! Shoot #,mature,measured        #/pl
      REAL          TNUMPREV      ! Tiller (+main stem) #,previous #/p
      REAL          TNUMT         ! Shoot number from t file       #/m2
      REAL          TNUMX         ! Tiller (incl.main stem) max.#  #/p
      REAL          TOFIXC        ! Cumulative inorganicN fixation kg/ha
      REAL          TOMIN         ! Daily N mineralized            kg/ha
      REAL          TOMINC        ! Cumulative N mineralized       kg/ha
      !REAL          TOMINFOM      ! Daily mineralization,FOM      kg/ha
      REAL          TOMINFOMC     ! Cumulative mineralization,FOM  kg/ha
      !REAL          TOMINSOM      ! Daily mineralization,SOM      kg/ha
      !REAL          TOMINSOM1     ! Daily mineralization,SOM1     kg/ha
      REAL          TOMINSOM1C    ! Cumulative mineralization,SOM1 kg/ha
      !REAL          TOMINSOM2     ! Daily mineralization,SOM2     kg/ha
      REAL          TOMINSOM2C    ! Cumulative mineralization,SOM2 kg/ha
      !REAL          TOMINSOM3     ! Daily mineralization,SOM3     kg/ha
      REAL          TOMINSOM3C    ! Cumulative mineralization,SOM3 kg/ha
      REAL          TOMINSOMC     ! Cumulative mineralization,SOM  kg/ha
      REAL          TPAR          ! Transmission,PAR,fraction      #
      REAL          TRATIO        ! Function,relative tr rate      #
      REAL          TRCOH(4)      ! Temp response,cold hardening   #
      REAL          TRDV1(4)      ! Temp response,development 1    #
      REAL          TRDV2(4)      ! Temp response,development 2    #
      REAL          TRDV3(4)      ! Temp response,development 3    #
      REAL          TRDV4(4)      ! Temp response,development 4    #
      REAL          TRDV5(4)      ! Temp response,development 5    #
      REAL          TRDV6(4)      ! Temp response,development 6    #
      REAL          TRDV7(4)      ! Temp response,development 7    #
      REAL          TRDV8(4)      ! Temp response,development 8    #
      REAL          TRGEM(4)      ! Temp response,germ.emergence   #
      REAL          TRGFC(4)      ! Temp response,grain fill,C     #
      REAL          TRGFN(4)      ! Temp response,grain fill,N     #
      REAL          TRLDF         ! Intermediate factor,new roots  #
      REAL          TRLFG(4)      ! Temp response,leaf growth      #
      REAL          TRLV          ! Total root length density      /cm2
      REAL          TRPHS(4)      ! Temp response,photosynthesis   #
      REAL          TRVRN(4)      ! Temp response,vernalization    #
      REAL          TRWU          ! Total water uptake             mm
      !REAL          TRWUP         ! Total water uptake,potential   cm
      REAL          TSDEP         ! Average temp in top 10 cm soil C
      REAL          TSRAD         ! Transmission,SRAD,fraction     #
      INTEGER       TSSTG         ! Terminal spikelet stage        #
      REAL          TT            ! Daily thermal time             C.d
      REAL          TTGEM         ! Daily thermal time,germ,emrg.  C.d
      REAL          TTLFLIFE      ! Daily thermal time,leaf life   C.d
   !   REAL          TTLA          ! Daily thermal time,lf.appearnc C.d
      REAL          TT20          ! Thermal time mean over 20 days C
      REAL          TT20S         ! Thermal time sum over 20 days  C
      REAL          TTCUM         ! Cumulative thermal time        C.d
      REAL          TTD(20)       ! Thermal time,specific day      C
      REAL          TTNEXT        ! Thermal time,next phase        oCd
      REAL          TTOUT         ! Thermal units output from func C.d
      INTEGER       TVI1          ! Temporary integer variable     #
      INTEGER       TVI2          ! Temporary integer variable     #
      INTEGER       TVI3          ! Temporary integer variable     #
      INTEGER       TVI4          ! Temporary integer variable     #
      !INTEGER       TVICOLNM      ! Column number function output  #
      !INTEGER       TVILENT       ! Temporary integer,function op  #
      REAL          TVR1          ! Temporary real variable        #
      REAL          TVR2          ! Temporary real variable        #
      REAL          TVR3          ! Temporary real variable        #
      REAL          TVR4          ! Temporary real variable        #
      REAL          TVR5          ! Temporary real variable        #
      REAL          TVR6          ! Temporary real variable        #
      REAL          TWAD          ! Total weight (tops+roots)      kg/ha
      !REAL          UH2O(NL)      ! Uptake of water               cm/d
      !REAL          UNH4(20)      ! Uptake of NH4 N               kg/ha
      !REAL          UNO3(20)      ! Uptake of NO3 N               kg/ha
      INTEGER       VALUEI        ! Output from Getstri function   #
      REAL          VALUER        ! Output from Getstrr function   #
      REAL          VANC          ! Vegetative actual N conc       #
      INTEGER       VARNUM(30)    ! Variable number in sum         #
      REAL          VARSUM(30)    ! Temporary variables sum        #
      REAL          VARVAL        ! Temporary variable             #
      REAL          VBASE         ! Vrn requirement before devment d
      REAL          VCNC          ! Vegetative critical N conc     #
      REAL          VDLOS         ! Vernalization lost (de-vern)   d
      REAL          VEEND         ! Vernalization effect end Rstge #
      REAL          VEENDDU       ! Vernalization effect end DU    #
      REAL          VEFF          ! Vernalization effect,max.reduc fr
      !INTEGER       VERSION       ! Version #                      #
      INTEGER       VERSIONCSCRP  ! Version #                 
      REAL          VF            ! Vernalization factor 0-1       #
      REAL          VFNEXT        ! Vernalization factor,next ph   #
      REAL          VLOSS         ! Vernalization stage for 0 loss #
      REAL          VLOSF         ! Fraction of vernalization lost fr
      REAL          VLOST         ! Vernalization loss threshold   C
      REAL          VMNC          ! Vegetative minimum N conc      #
      REAL          VNAD          ! Vegetative canopy nitrogen     kg/ha
      REAL          VNAM          ! Vegetative N,mature            kg/ha
      REAL          VNAMM         ! Vegetative N,mature,measured   kg/ha
      REAL          VNPCM         ! Vegetative N %,maturity        %
      REAL          VNPCMM        ! Vegetative N,mature,measure    %
      REAL          VPD           ! Vapour pressure deficit        KPa
      REAL          VPDFP         ! Vapour press deficit factor,phs #
      REAL          VPEND         ! Vernalization process end Rstg #
      REAL          VPENDDU       ! Vernalization process end DU   #
      !REAL          VPENDFR       ! Vernalization process end frdy #
      REAL          VREQ          ! Vernalization requirement      d
      REAL          VRNSTAGE      ! Vernalization stage            #
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
      REAL          WFGEU         ! Water factor,germ,emergence    #
      REAL          WFGL          ! Water factor,growth,lower      #
      REAL          WFGPAV(0:12)  ! Water factor,growth,average    #
      REAL          WFGPC         ! Water factor,growth,cumulative #
      REAL          WFGU          ! Water factor,growth,upper      #
      REAL          WFLAW         ! Water factor,leaf area/weight  #
      REAL          WFLF(LNUMX)   ! H2O factor for leaf,average    #
      REAL          WFLFP(LNUMX)  ! H2O factor,phs leaf,average    #
      REAL          WFNU          ! Water factor,N uptake          #
      REAL          WFP           ! Water factor,photosynthsis 0-1 #
      REAL          WFPCAV        ! Water factor,phs,av 0-1,cycle  #
      REAL          WFPCC         ! H20 factor,phs,cycle sum       # 
      REAL          WFPL          ! Water factor,phs,lower         #
      REAL          WFPPAV(0:12)  ! Water factor,phs,average 0-1   #
      REAL          WFPPC         ! Water factor,phs,cumulative    #
      REAL          WFPU          ! Water factor,phs,upper         #
      REAL          WFRG          ! Water factor,root growth,0-1   #
      REAL          WFRGU         ! Water factor,root gr           #
      REAL          WFSU          ! Water factor,senescence 0-1    #
      REAL          WFSF          ! WFS trigger,final senescence   #
      REAL          WFT           ! Water factor,tillering 0-1     #
      REAL          WFTL          ! Water factor,tillering,lower   #
      REAL          WFTU          ! Water factor,tillering,upper   #
      !REAL          WINDSP        ! Wind speed                     m/s
      INTEGER       WSDAYS        ! Water stress days              #
      REAL          WTDEP         ! Water table depth              cm
      REAL          WUPR          ! Water pot.uptake/demand        #
      REAL          WUPRD(20)     ! Water pot.uptake/demand,ind dy #
      REAL          XDEP          ! Depth to bottom of layer       cm
      REAL          XDEPL         ! Depth to top of layer          cm
      REAL          XMIN          ! Minimum NO3,NH4-N in soil layr kg/ha
      !INTEGER       YEAR          ! Year                           #
      INTEGER       YEARCOL       ! Colum number for year data     #
      INTEGER       YEARDOY       ! Year+Doy (7digits)             #
      INTEGER       YEARDOYHARF   ! Harvest year+doy,fixed         #
      INTEGER       YEARM         ! Year of measurement            #
      INTEGER       YEARSIM       ! Year+Doy for simulation start  #
      !INTEGER       YEARPLTCSM    ! Planting year*1000+DOY,CSM     #
      !REAL          YVALXY        ! Y value from function          #
      REAL          ZSTAGE        ! Zadoks stage of development    #

      ! Following are to test Doug Stewart s function 
      REAL tmaxwheat,tminwheat,tequiv,ttwheat,ttmaize,ttold,TFDOLD
      
      integer pausenum

      CHARACTER (LEN=250) ARG           ! Argument component
      CHARACTER (LEN=6)   CAIC          ! Canopy area index
      CHARACTER (LEN=6)   CANHTC        ! Canopy height
      CHARACTER (LEN=250) CFGDFILE      ! Configuration directory+file
      CHARACTER (LEN=1)   CFLFAIL       ! Control flag for failure
      CHARACTER (LEN=1)   CFLFLN        ! Control flag,final leaf # Y/N
      CHARACTER (LEN=1)   CFLHAR        ! Control flag for final harvest
      CHARACTER (LEN=1)   CFLHARMSG     ! Control flag,harvest message
      CHARACTER (LEN=1)   CFLNOUTPUTS   ! Control flag,detailed Noutputs
      CHARACTER (LEN=1)   CFLPHASEADJ   ! Control flag,phase adjustment 
      CHARACTER (LEN=1)   CFLPHINTADJ   ! Control flag,phint adjustment 
      CHARACTER (LEN=1)   CFLPDATE      ! Control flag,planting date I,P
      CHARACTER (LEN=1)   CFLPRES       ! Control flag for headers,PRES
      CHARACTER (LEN=1)   CFLSDRSMSG    ! Control flag,seed reserves msg
      CHARACTER (LEN=10)  CNCHAR        ! Crop component (multicrop)
      CHARACTER (LEN=2)   CNCHAR2       ! Crop component (multicrop)
      CHARACTER (LEN=2)   CROP          ! Crop identifier (ie. WH, BA)
      CHARACTER (LEN=2)   CROPPREV      ! Crop identifier,previous run
      CHARACTER (LEN=250)  CUDIRFLE      ! Cultivar directory+file
      CHARACTER (LEN=250)  CUDIRFLPREV   ! Cultivar directory+file,prev
      CHARACTER (LEN=12)  CUFILE        ! Cultivar file
      CHARACTER (LEN=10)  DAPCHAR       ! DAP in character form
      CHARACTER (LEN=6)   DAPWRITE      ! DAP character string -> output
      CHARACTER (LEN=8)   DATEWRITE     ! YEARDOY character string -> op
      CHARACTER (LEN=250)  ECDIRFLE      ! Ecotype directory+file
      CHARACTER (LEN=250)  ECDIRFLPREV   ! Ecotype directory+file,prev
      CHARACTER (LEN=12)  ECFILE        ! Ecotype filename
      CHARACTER (LEN=6)   ECONO         ! Ecotype code
      CHARACTER (LEN=6)   ECONOPREV     ! Ecotype code,previous
      CHARACTER (LEN=60)  ENAME         ! Experiment description
      CHARACTER (LEN=1)   ESTABLISHED   ! Flag,crop establishment Y/N
      CHARACTER (LEN=14)  EVHEADER      ! Evaluater.out header
      CHARACTER (LEN=1)   EXAMINE(20)   ! Flag for examination of parts
      CHARACTER (LEN=10)  EXCODE        ! Experiment code/name
      CHARACTER (LEN=10)  EXCODEPREV    ! Previous experiment code/name
      CHARACTER (LEN=80)  FAPPLINE(30)  ! Fertilizer application details
      CHARACTER (LEN=250) FILEA         ! Name of A-file
      CHARACTER (LEN=250) FILEX         ! Name of X-file
      CHARACTER (LEN=250) FILEIO        ! Name of input file,after check
      !CHARACTER (LEN=250) FILEIOIN      ! Name of input file
      CHARACTER (LEN=250) FILEADIR      ! Name of A-file directory     
      CHARACTER (LEN=3)   FILEIOT       ! Type of input file
      CHARACTER (LEN=250) FILENEW       ! Temporary name of file
      CHARACTER (LEN=250) FILET         ! Name of T-file
      CHARACTER (LEN=1)   FNAME         ! File name switch (N->standard)
      CHARACTER (LEN=250) FNAMEERA      ! File name,A-errors
      CHARACTER (LEN=250) FNAMEERT      ! File name,T-errors
      CHARACTER (LEN=250) FNAMEEVAL     ! File name,evaluate outputs
      CHARACTER (LEN=250) FNAMELEAVES   ! File name,leaves outputs
      CHARACTER (LEN=250) FNAMEMEAS     ! File name,measured outputs
      CHARACTER (LEN=250) FNAMEOV       ! File name,overview outputs
      CHARACTER (LEN=250) FNAMEPHASES   ! File name,phases outputs
      CHARACTER (LEN=250) FNAMEPHENOLM  ! File name,phenology measured
      CHARACTER (LEN=250) FNAMEPHENOLS  ! File name,phenology outputs
      CHARACTER (LEN=250) FNAMEPREM     ! File name,responses,measured
      CHARACTER (LEN=250) FNAMEPRES     ! File name,responses,simulated
      CHARACTER (LEN=250) FNAMEPSUM     ! File name,plant summary
      CHARACTER (LEN=35)  GENFLCHK      ! Genotype file name for check
      CHARACTER (LEN=1)   GROUP         ! Flag for type of group
      CHARACTER (LEN=6)   GSTAGEC       ! Growth stage
      CHARACTER (LEN=6)   HIAMCHAR      ! Harvest indx,at maturity
      CHARACTER (LEN=6)   HIAMMCHAR     ! Harvest indx,mat,measured
      CHARACTER (LEN=6)   HINDC         ! Harvest index,nitrogen   
      CHARACTER (LEN=6)   HINMCHAR      ! Harvest N index,at maturity
      CHARACTER (LEN=6)   HINMMCHAR     ! Harvest N index,mat,measured
      CHARACTER (LEN=6)   HNPCMCHAR     ! Harvest product N %,maturity  
      CHARACTER (LEN=6)   HNPCMMCHAR    ! Harvest product N%,mature,meas
      CHARACTER (LEN=1)   HOP(HANUMX)   ! Harvest operation code  
      CHARACTER (LEN=6)   HWUDC         ! Harvest wt/unit
      CHARACTER (LEN=6)   HWUMCHAR      ! Harvest wt/unit
      CHARACTER (LEN=6)   HWUMMCHAR     ! Harvest wt/unit,mat,measured
      CHARACTER (LEN=1)   IDETD         ! Control flag,screen outputs
!      CHARACTER (LEN=1)   IDETG         ! Control flag,growth outputs
!      CHARACTER (LEN=1)   IDETL         ! Control switch,detailed output
!      CHARACTER (LEN=1)   IDETO         ! Control flag,overview outputs
!      CHARACTER (LEN=1)   IDETS         ! Control switch,summary outputs
      CHARACTER (LEN=1)   IFERI         ! Fertilizer switch (A,F,R,D,N)
      CHARACTER (LEN=1)   IHARI         ! Control flag,harvest
      CHARACTER (LEN=1)   IPLTI         ! Code for planting date method
      !CHARACTER (LEN=1)   ISWDIS        ! Control switch,disease
      !CHARACTER (LEN=1)   ISWNIT        ! Soil nitrogen balance switch
      CHARACTER (LEN=1)   ISWNITEARLY   ! Control flag,N stress early
      !CHARACTER (LEN=1)   ISWWAT        ! Soil water balance switch Y/N
      CHARACTER (LEN=1)   ISWWATCROP    ! Control flag,H20 stress,crop 
      CHARACTER (LEN=1)   ISWWATEARLY   ! Control flag,H20 stress early
      CHARACTER (LEN=6)   LAIC          ! Leaf area index
      CHARACTER (LEN=6)   LAIPRODC      ! Leaf area index produced
      CHARACTER (LEN=6)   LAIXCHAR      ! Leaf area index,maximum
      CHARACTER (LEN=6)   LAIXMCHAR     ! Leaf area index,max,measured
      CHARACTER (LEN=6)   LAPC          ! Area of cohort of leaves
      CHARACTER (LEN=6)   LAPOTXC       ! Leaf area,potential
      CHARACTER (LEN=6)   LAPSC         ! Senesced area,cohort of leaves
      CHARACTER (LEN=6)   LATLC         ! Leaf area,actual
      CHARACTER (LEN=354) LINEERA       ! Temporary line,error-a file
      CHARACTER (LEN=80)  LINESTAR      ! Group header line (with star)
      CHARACTER (LEN=80)  LINESTAR2     ! Group header line (with star)
      CHARACTER (LEN=180) LINET         ! Line from T-file
      CHARACTER (LEN=1)   MEEXP         ! Switch,experimental method = E
      CHARACTER (LEN=1)   MEEVP         ! Switch,potential evapot method
      CHARACTER (LEN=1)   MEPHS         ! Switch,photosynthesis method
      !CHARACTER (LEN=1)   MESOM         ! Switch,OM decay method       
      CHARACTER (LEN=78)  MESSAGE(10)   ! Messages for Warning.out
      CHARACTER (LEN=1)   MENU          ! Switch,root N uptake methd
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
      CHARACTER (LEN=250)  PATHCR        ! Path to genotype (CUL) files
      CHARACTER (LEN=250)  PATHEC        ! Path to genotype (ECO) files
      CHARACTER (LEN=250)  PATHSP        ! Path to genotype (SPE) files
      CHARACTER (LEN=1)   PLME          ! Planting method (code)        
      CHARACTER (LEN=2)   PPSEN         ! Code,photoperiod sensitivity
      CHARACTER (LEN=5)   PSABV(PSX)    ! Principal stage abbreviation
      CHARACTER (LEN=5)   PSABVO(PSX)   ! Principal stage abv,output
      CHARACTER (LEN=13)  PSNAME(PSX)   ! Principal stage names
      CHARACTER (LEN=1)   PSTYP(PSX)    ! Principal stage type
      !CHARACTER (LEN=1)   RNMODE        ! Run mode (eg.I=interactive)
      CHARACTER (LEN=25)  RUNNAME       ! Run title
      CHARACTER (LEN=8)   RUNRUNI       ! Run+internal run number
      CHARACTER (LEN=6)   SDWADC        ! Seed weight
      CHARACTER (LEN=1)   SEASENDOUT    ! Season end outputs flag     
      CHARACTER (LEN=6)   SENN0C        ! Senesced N added to litter
      CHARACTER (LEN=6)   SENNSC        ! Senesced N added to soil
      CHARACTER (LEN=6)   SENROOTC      ! Senesced OM,soil
      CHARACTER (LEN=6)   SENTOPLITTERAC! Senesced OM added to surface
      CHARACTER (LEN=250)  SPDIRFLE      ! Species directory+file
      CHARACTER (LEN=250)  SPDIRFLPREV   ! Species directory+file,last
      CHARACTER (LEN=12)  SPFILE        ! Species filename
      CHARACTER (LEN=5)   SSABV(SSX)    ! Secondary stage abbreviation
      CHARACTER (LEN=5)   SSABVO(SSX)   ! Secondary stage abv,output
      CHARACTER (LEN=13)  SSNAME(SSX)   ! Secondary stage names
      CHARACTER (LEN=1)   SSTYP(SSX)    ! Secoudary stage type
      CHARACTER (LEN=6)   TCHAR         ! Temporary character string
      CHARACTER (LEN=6)   THEAD(20)     ! T-file headings
      CHARACTER (LEN=1)   TIERNUMC      ! Tier number in t-file
      CHARACTER (LEN=10)  TL10          ! Temporary line              
      !CHARACTER (LEN=10)  TL10FROMI     ! Temporary line from integer
      CHARACTER (LEN=254) TLINEGRO      ! Temporary line from GRO file
      CHARACTER (LEN=180) TLINET        ! Temporary line from T-file
      CHARACTER (LEN=180) TLINETMP      ! Temporary line
      CHARACTER (LEN=25)  TNAME         ! Treatment name
      CHARACTER (LEN=10)  TNCHAR        ! Treatment number,characters
      CHARACTER (LEN=40)  TRUNNAME      ! Treatment+run composite name
      CHARACTER (LEN=5)   TVTRDV        ! Temporary temp response char
      CHARACTER (LEN=8)   VARCHAR       ! Temporary variable,character
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

      !INTRINSIC AMAX1,AMIN1,EXP,FLOAT,INDEX,INT,LEN,MAX,MIN,MOD,NINT
      !INTRINSIC SQRT,ABS,TRIM

    
        End Module CRP_First_Trans_m
        
        
        subroutine clear_CRP_First_Trans_m()
            USE CRP_First_Trans_m
            implicit none
            
            
            L = 0
            L1 = 0
            L2 = 0
            I = 0
            NLAYRROOT = 0
            
            amtnit = 0.0
            andem = 0.0
            caid = 0.0
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
            cflharmsg = 'N'
            cflsdrsmsg = 'N'
            sdwt = 0.0
            cnad = 0.0
            cnadprev = 0.0
            cnadstg = 0.0
            cnam = -99.0
            cnamm = -99.0
            co2cc = 0.0
            co2fp = 1.0
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
            df = 1.0
            dfout = 1.0
            drainc = 0.0
            du = 0.0
            duneed = 0.0
            dynamicprev = 99999
            edap = -99
            edapfr = 0.0
            edapm = -99
            edayfr = 0.0
            emrgfr = 0.0
            emrgfrprev = 0.0
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
            grolf = 0.0
            grors = 0.0
            grost = 0.0
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
            laiprev = 0.0
            laistg = 0.0
            laix = 0.0
            laixm = -99.0
            !lanc = 0.0
            laphc = 0.0
            leafn = 0.0
            lfwt = 0.0
            lfwtm = 0.0
            llnad = 0.0
            llwad = 0.0
            !lncr = 0.0
            lndem = 0.0
            lnphc = 0.0
            lnum = 0.0
            lnumg = 0.0
            lnumprev = 0.0
            lnumsg = 0
            lnumsm = -99.0
            lnumsmm = -99.0
            lnumstg = 0.0
            lnuse = 0.0
            lseed = -99
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
            pariue = 0.0
            parmjc = 0.0
            parmjic = 0.0
            paru = 0.0
            pdadj = -99.0
            pdays  = 0
            photqr = 0.0
            pla = 0.0
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
            rainc = 0.0
            raincc = 0.0
            rainpav = -99.0
            rainpc = 0.0
            ranc = 0.0
            rescal = 0.0
            reslgal = 0.0
            resnal = 0.0
            respc = 0.0
            resprc = 0.0
            resptc = 0.0
            reswal = 0.0
            reswalg = 0.0
            rlf = 0.0
            rlfc = 0.0
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
            rswad = 0.0
            rswam = -99.0
            rswamm = -99.0
            rswph = 0.0
            rswphc = 0.0
            rswt = 0.0
            rswtm = 0.0
            rswtx = 0.0
            rtdep = 0.0
            rtdepg = 0.0
            rtnsl = 0.0
            rtresp = 0.0
            rtslxdate = -99 
            rtwt = 0.0
            rtwtal = 0.0
            rtwtg = 0.0
            rtwtgl = 0.0
            rtwtl = 0.0
            rtwtm = 0.0
            rtwtsl = 0.0
            runoffc = 0.0
            rwad = 0.0
            rwam = -99.0
            rwamm = -99.0
            said = 0.0
            sancout = 0.0
            sdnad = 0.0
            sdnc = 0.0
            sdwad = 0.0
            sdwam = -99.0
            seedrs = -99.0
            seeduse = 0.0 
            seeduser = 0.0
            seeduset = 0.0
            sencags = 0.0
            sencas = 0.0
            sencl = 0.0
            sencs = 0.0
            senla = 0.0
            senlalitter = 0.0
            senlags = 0.0
            senlas = 0.0
            senlfg = 0.0
            senlfgrs = 0.0
            senll = 0.0
            senls = 0.0
            sennags = 0.0
            sennal = 0.0
            sennas = 0.0
            sennatc = -99.0
            sennatcm = -99.0
            sennl = 0.0
            sennlfg = 0.0
            sennlfgrs = 0.0
            senns = 0.0
            senrtg = 0.0
            SENTOPLITTERG = 0.0
            senwacm = -99.0
            senwacmm = -99.0
            senwags = 0.0
            senwal = 0.0
            senwalg = 0.0
            senwl = 0.0
            senroot = 0.0
            senroota = 0.0
            shrtd = 0.0
            shrtm = 0.0
            sla = -99.0
            snad = 0.0
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
            stai = 0.0
            staig = 0.0
            stais = 0.0
            stemn = 0.0
            stgedat = 0
            stwad = 0.0
            STRESSW = 1.0
            stwt = 0.0
            stwtm = 0.0
            swphc = 0.0
            tcan = 0.0
            tdifav = -99.0
            tdifnum = 0
            tdifsum = 0.0
            tfd = 0.0
            tfg = 1.0
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
            tt = 0.0
            tt20 = -99.0
            ttgem = 0.0
            ttcum = 0.0
            ttd = 0.0
            ttnext = 0.0
            twad = 0.0
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
            wfp = 1.0
            wfpcav = 1.0
            wfppav = 1.0
            wfpcc = 0.0
            wfppc = 0.0
            wsdays = 0
            wupr = 1.0
            
            no3mn = -99.0
            nh4mn = -99.0

        end subroutine clear_CRP_First_Trans_m
        