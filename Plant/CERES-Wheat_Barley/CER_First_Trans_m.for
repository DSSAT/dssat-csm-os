        Module CER_First_Trans_m

      CHARACTER(LEN=1),PARAMETER::BLANK = ' '
      !CHARACTER(LEN=3),PARAMETER::DASH = ' - '
      !CHARACTER(LEN=6),PARAMETER::ERRKEY = 'CSCER '        
     
      !INTEGER       NL            ! Maximum number of soil layers  #
      !PARAMETER     (NL = 20)     ! Maximum number of soil layers  #
      INTEGER       LNUMX         ! Maximum number of leaves       #
      PARAMETER     (LNUMX = 100) ! Maximum number of leaves       #
      INTEGER       SUMNUM        ! Number of variables passed     #
      PARAMETER     (SUMNUM = 37) ! Number of variables passed     #
      CHARACTER*6   ERRKEY

      INTEGER       A1DATM        ! Apex 1cm date,measured         #
      INTEGER       ADAP          ! Anthesis,days after planting   d
      INTEGER       ADAPM         ! Anthesis,DAP,measured          d
      INTEGER       ADAT          ! Anthesis date (Year+doy)       #
      !INTEGER       ADAT10        ! Anthesis date (Year+doy) + 10  #
      INTEGER       ADATEAA       ! Anthesis date abs error avg    #
      INTEGER       ADATEAV       ! Anthesis date average error    #
      INTEGER       ADATEND       ! Anthesis end date (Year+doy)   #
      INTEGER       ADATERR       ! Anthesis date error            d
      INTEGER       ADATM         ! Anthesis date,measured         #
      INTEGER       ADATNUM       ! Anthesis date error #          #
      INTEGER       ADATSUA       ! Anthesis date abs error #      #
      INTEGER       ADATSUM       ! Anthesis date error sum        #
      REAL          ADATT         ! Anthesis date from t file      YrDoy
      INTEGER       ADAY          ! Anthesis day of year           d
      INTEGER       ADAYH         ! Anthesis harvest d>anthesis    d
      INTEGER       ADAYM         ! Anthesis day of year,measured  d
      REAL          AFLF(LNUMX)   ! CH2O factor for leaf,average   #
      REAL          AFLFSUM(LNUMX)! CH2O factor for leaf,sum       #
      REAL          AH2OPROFILE   ! Available h2o in profile       mm
      REAL          AH2OPROFILEI  ! Available H2o,profile,initial  mm 
      REAL          AH2OROOTZONE  ! Available h2o in root zone     mm
      REAL          AH2OROOTZONEI ! Available H2o,rootzone,initial mm
      REAL          AMDAYM        ! Anthesis-maturity period,meas  d
      REAL          AMDAYS        ! Anthesis-maturity period,sim   d
      REAL          ANDEM         ! Crop N demand                  kg/ha
      CHARACTER*250 ARG           ! Argument component             text
      INTEGER       ARGLEN        ! Argument component length      #
      REAL          ASTAGE        ! Stage,start of anthesis/silk   #
      REAL          ASTAGEND      ! Stage at end of anthesis       #
      REAL          AVGSW         ! Average soil water in SWPLTD   %
      REAL          AWNAI         ! Awn area index                 m2/m2
      REAL          AWNS          ! Awn score,1-10                 #
      INTEGER       AYEAR         ! Anthesis year                  #
      INTEGER       AYEARM        ! Anthesis year,measured         #
      CHARACTER*10  BASTGNAM(20)  ! Barley stage names             text
!      REAL          BD(20)        ! Bulk density (moist)           g/cm3
!      CHARACTER*1   BLANK         ! Blank character                text
      REAL          BLAYER        ! Depth at base of layer         cm
      REAL          CANANC        ! Canopy N concentration         %
!      REAL          CANHT         ! Canopy height                  cm
      CHARACTER*6   CANHTC        ! Canopy height                  cm
      REAL          CANHTG        ! Canopy height growth           cm
      REAL          CANHTS        ! Canopy height standard         cm
      REAL          CARBO         ! Carbohydrate available,phs     g/p
      REAL          CARBOACM      ! Carbohydrate assimilated,cum,m kg/ha
      REAL          CARBOAPM      ! Carbohydrate available,>mature g/p
      REAL          CARBOR        ! Carbohydrate available,roots   g/p
      REAL          CARBOASD      ! Carbohydrate available,seed    g/p
      REAL          CARBOAT       ! Carbohydrate available,tops    g/p
      REAL          CARBOC        ! Carbohydrate assimilated,cum   g/p
      INTEGER       CCOUNTV       ! Counter for days after max lf# #
      CHARACTER*250 CFGDFILE      ! Configuration directory+file   text
      CHARACTER*1   CFLFAIL       ! Control flag for failure       text
      CHARACTER*1   CFLHEAD       ! Control flag to write headers  code
      CHARACTER*1   CFLINIT       ! Control flag for initiation    text
      CHARACTER*1   CFLTFILE      ! Control flag for T-file        code
      INTEGER       CH2OLIM       ! Number of days CH2O limited gr #
      REAL          CHFR          ! Chaff fraction of assimilates  #
      REAL          CHRS          ! Chaff reserves weight          g/p
      REAL          CHRSWT        ! Chaff reserves                 g/p
      REAL          CHSTG         ! Chaff growth X-stage           #
      REAL          CHWT          ! Chaff weight                   g/p
      REAL          CKCOLD        ! Cold temp factor,leaf death    #
      REAL          CLAPC(10)     ! Canopy lf area fraction w ht   %
      !INTEGER       CN            ! Crop component (multicrop)     #
      REAL          CNAAM         ! Canopy N,anthesis,measured     kg/ha
      REAL          CNAM          ! Canopy N at maturity           kg/ha
      REAL          CNAMEAA       ! Canopy N/area abs error avg    #
      REAL          CNAMEAV       ! Canopy N/area average error    #
      REAL          CNAMERR       ! Canopy N,maturity,error        %
      REAL          CNAMM         ! Canopy N,mature,measured       kg/ha
      INTEGER       CNAMNUM       ! Canopy N/area error #          #
      REAL          CNAMSUA       ! Canopy N/area abs error #      #
      REAL          CNAMSUM       ! Canopy N/area error sum        #
      CHARACTER*10  CNCHAR        ! Crop component (multicrop)     text
      CHARACTER*2   CNCHAR2       ! Crop component (multicrop)     text
      REAL          CNCTMP        ! Canopy N concentration,temp    %
      INTEGER       CNI           ! Crop component,initial value   #
      !REAL          CO2           ! CO2 concentration in air       vpm
      REAL          CO2AV(0:9)    ! Average co2 in phase           vpm
      REAL          CO2FP         ! CO2 factor,photosynthesis 0-1  #
      REAL          CO2F(10)      ! CO2 factor rel values 0-1      #
      REAL          CO2MAX        ! CO2 conc,maximum during cycle  vpm
      REAL          CO2RF(10)     ! CO2 reference concentration    vpm
      REAL          CO2SUM        ! CO2 sum in phase               vpm
      REAL          CO2SUM0       ! CO2 sum for cycle              vpm
      INTEGER       COLNUM        ! Column number                  #
      CHARACTER*2   CR            ! Crop identifier (ie. WH, BA)   text
      CHARACTER*2   CROP          ! Crop identifier (ie. WH, BA)   text
      CHARACTER*2   CROPP         ! Crop identifier,previous run   text
      !INTEGER       CSINCDAT      ! Increment day function output  #
      !INTEGER       CSTIMDIF      ! Time difference function       #
      CHARACTER*1   CSWDIS        ! Control switch,disease         code
      !INTEGER       CSYDOY        ! Yr+Doy output from function    #
      !INTEGER       CSYEARDOY     ! Cropsim function ouptut        #
      CHARACTER*250 CUDIRFLE      ! Cultivar directory+file        text
      CHARACTER*250 CUDIRFLP      ! Cultivar directory+file,prev   text
      CHARACTER*12  CUFILE        ! Cultivar file                  text
      REAL          CUMDEP        ! Cumulative depth               cm
      REAL          CUMDU         ! Cumulative development units   #
      REAL          CUMGEU        ! Cumulative GE units (TDD*WFGE) #
      REAL          CUMSW         ! Soil water in depth SWPLTD     cm
      REAL          CUMTT         ! Cumulative thermal time        C.d
      REAL          CUMTU         ! Cumulative thermal units       #
      REAL          CUMVD         ! Cumulative vernalization days  d
      REAL          CWAAM         ! Canopy wt,anthesis,measured    kg/ha
      REAL          CWADT         ! Canopy weight from t file      kg/ha
      REAL          CWAM          ! Canopy weight at maturity      kg/ha
      REAL          CWAMEAA       ! Canopy wt/area abs error avg   #
      REAL          CWAMEAV       ! Canopy wt/area average error   #
      REAL          CWAMERR       ! Canopy weight,maturity,error   %
      REAL          CWAMM         ! Canopy wt,mature,measured      kg/ha
      INTEGER       CWAMNUM       ! Canopy wt/area error #         #
      REAL          CWAMSUA       ! Canopy wt/area abs error #     #
      REAL          CWAMSUM       ! Canopy wt/area error sum       #
      INTEGER       DAE           ! Days after emergence           #
      INTEGER       DAP           ! Days after planting            #
      !INTEGER       DAPCALC       ! DAP output from funcion        #
      CHARACTER*10  DAPCHAR       ! DAP in character form          text
      INTEGER       DAPM          ! Days after phys.maturity       #
      CHARACTER*6   DAPWRITE      ! DAP character string -> output text
      INTEGER       DAS           ! Days after start of simulation #
      CHARACTER*3   DASH          ! Dash character for output      text
      INTEGER       DATE          ! Date (Yr+Doy)                  #
      INTEGER       DATECOL       ! Date column number             #
      REAL          DAYLAV(0:9)   ! Daylength average in phase     h
      REAL          DAYLSUM       ! Daylength sum in phase         h
      REAL          DAYLSUM0      ! Daylength sum for cycle        h
      !REAL          DAYLT         ! Daylength (6deg below horizon) h
      REAL          DAYLTP        ! Daylength previous day         h
      INTEGER       DAYSC(0:9)    ! Days in growth phase           #
      REAL          DAYSUM        ! Days accumulated in month     
      REAL          DEADN         ! Dead leaf N retained on plant  g/p
      REAL          DEADWT        ! Dead leaf wt.retained on plant g/p
      !REAL          DEPMAX        ! Maximum depth of soil profile  cm
      !REAL          DEWDUR        ! Dew duration                   h
      REAL          DF            ! Daylength factor 0-1           #
      !REAL          DLAYR(20)     ! Depth of soil layers           cm
      REAL          DLAYRTMP(20)  ! Depth of soil layers with root cm
      REAL          DLEAFN        ! Change in leaf N               g/p
      REAL          DMP_EP        ! Dry matter per unit EP         g/mm
      REAL          DMP_ET        ! Dry matter per unit ET         g/mm
      REAL          DMP_IRR       ! Dry matter per unit irrigation g/mm
      REAL          DMP_NAPP      ! Dry matter per unit N applied  kg/kg
      REAL          DMP_NUPT      ! Dry matter per unit N taken uo kg/kg
      REAL          DMP_RAIN      ! Dry matter per unit precip     g/mm
      INTEGER       DOM           ! Day of month                   #
      !INTEGER       DOY           ! Day of year                    #
      INTEGER       DOYCOL        ! Day of year column number      #
      INTEGER       DRDAP         ! Double ridges date             #
      INTEGER       DRDAPM        ! Double ridges date,measured    #
      INTEGER       DRDAT         ! Double ridges date             #
      INTEGER       DRDATM        ! Double ridges date,measured    #
      REAL          DRF1          ! Double ridges factor 1         #
      REAL          DRF2          ! Double ridges factor 1         #
      REAL          DRF3          ! Double ridges factor 1         #
      REAL          DROOTN        ! Change in plant root N         g/p
      REAL          DROOTNA       ! Daily adjustment in root N gr  g/p
      REAL          DRSTAGE       ! Double ridges stage            #
      REAL          DSTEMN        ! Change in stem N               g/p
      REAL          DSTOVN        ! Change in stover N             g/p
      REAL          DTOPN         ! Change in tops N               g/p
      REAL          DTRY          ! Effective depth of soil layer  cm
      REAL          DU            ! Developmental units            PVC.d
      !REAL          DUL(20)       ! Drained upper limit for soil   #
      !INTEGER       DYNAMIC       ! Program control variable       text
      !INTEGER       DYNAMICI      ! Module control,internal        code
      CHARACTER*250 ECDIRFLE      ! Ecotype directory+file         text
      CHARACTER*250 ECDIRFLP      ! Ecotype directory+file,prev    text
      CHARACTER*12  ECFILE        ! Ecotype filename               text
      CHARACTER*6   ECONO         ! Ecotype code                   text
      CHARACTER*6   ECONOP        ! Ecotype code,previous          text
      INTEGER       EDAP          ! Emergence DAP                  #
      INTEGER       EDAPM         ! Emergence DAP measured         #
      INTEGER       EDATM         ! Emergence date,measured (Afle) #
      INTEGER       EMDATEAA      ! Emergence date abs error avg   #
      INTEGER       EMDATEAV      ! Emergence date average error   #
      INTEGER       EMDATERR      ! Emergence date error           d
      INTEGER       EMDATM        ! Emergence yr+d,measured (Xfle) #
      INTEGER       EMDATNUM      ! Emergence date error #         #
      INTEGER       EMDATSUA      ! Emergence date abs error #     #
      INTEGER       EMDATSUM      ! Emergence date error sum       #
      CHARACTER*60  ENAME         ! Experiment description         text
      !REAL          EO            ! Potential evapotranspiration   mm/d
      !REAL          EOP           ! Potential evaporation,plants   mm/d
      !REAL          EP            ! Actual plant transpiration     mm/d
      REAL          EPC(0:9)      ! Plant evaporation in phase     mm   
      REAL          EPSUM         ! Plant evaporation sum in phase mm
      REAL          EPSUM0        ! Plant evaporation for cycle    mm
      REAL          ERRORVAL      ! Plgro-tfile values/Plgro       #
      !REAL          ET            ! Actual total evaporation       mm/d
      REAL          ETC(0:9)      ! Total evaporation in phase     mm 
      REAL          ETSUM         ! Total evaporation sum in phase mm
      REAL          ETSUM0        ! Total evaporation for cycle    mm
      INTEGER       EVALOUT       ! Evaluate output lines for exp  #
      CHARACTER*14  EVHEADER      ! Evaluater.out header           text
      INTEGER       EVHEADNM      ! Number of headings in ev file  #
      CHARACTER*1   EXAMINE(20)   ! Flag for examination of parts  code
      CHARACTER*10  EXCODE        ! Experiment code/name           text
      CHARACTER*10  EXCODEP       ! Previous experiment code/name  text
      REAL          FAC(20)       ! Factor ((mg/Mg)/(kg/ha))       #
      INTEGER       FDAY(200)     ! Dates of fertilizer applns     #
      LOGICAL       FEXIST        ! File existence indicator       code
      LOGICAL       FEXISTA       ! File A existence indicator     code
      LOGICAL       FEXISTT       ! File T existence indicator     code
      LOGICAL       FFLAG         ! Temp file existance indicator  code
      CHARACTER*250 FILEA         ! Name of A-file                 text
      CHARACTER*250 FILEADIR      ! Name of A-file directory       text
      CHARACTER*250 FILEIO        ! Name of input file,after check text
      !CHARACTER*250 FILEIOIN      ! Name of input file             text
      CHARACTER*3   FILEIOT       ! Type of input file             text
      INTEGER       FILELEN       ! Length of file name            #
      CHARACTER*250 FILENEW       ! Temporary name of file         text
      CHARACTER*250 FILET         ! Name of T-file                 text
      REAL          FLN           ! Final leaf number,Aitken calc. #
      REAL          FLNMODEL      ! Final leaf number              #
      CHARACTER*1   FNAME         ! File name switch (N->standard) code
      CHARACTER*12  FNAMETMP      ! File name,temporary            text
      REAL          FNH4          ! Unitless ammonium supply index #
      REAL          FNO3          ! Unitless nitrate supply index  #
      INTEGER       FNUMLVS       ! File number,leaves             #
!      INTEGER       FNUMREA       ! File number,reads.out file     #
      INTEGER       FNUMT         ! Number used for T-file         #
      INTEGER       FNUMTMP       ! File number,temporary file     #
      !INTEGER       FNUMWRK       ! File number,work file          #
      LOGICAL       FOPEN         ! File open indicator            code
      !INTEGER       FROP          ! Frquency of outputs            d
      INTEGER       FROPADJ       ! Frquency of outputs,adjusted   d
      REAL          G1CWT         ! Cultivar coefficient,grain #   #/g
      REAL          G2            ! Cultivar coefficient,grain gr  mg/du
      REAL          G2KWT         ! Cultivar coefficient,grain wt  mg
      REAL          G3            ! Cultivar coefficient,stem wt   g
      INTEGER       GEDSUM        ! Germ+emergence duration        d
      CHARACTER*35  GENFLCHK      ! Genotype file name for check   text
      REAL          GESTAGE       ! Germination,emergence stage    #
      REAL          GETMEAN       ! Germ+emergence temperature av  C
      REAL          GETSUM        ! Germ+emergence temperature sum C
      REAL          GEU           ! Germination,emergence units    #
      INTEGER       GFDSUM        ! Grain filling duration         d
      REAL          GFTMEAN       ! Grain filling temperature mean C
      REAL          GFTSUM        ! Grain filling temperature sum  C
      REAL          GLIGP         ! Grain lignin content           %
      INTEGER       GMDSUM        ! Grain maturity duration        d
      REAL          GMTMEAN       ! Grain maturity temperature av  C
      REAL          GMTSUM        ! Grain maturity temperature sum C
      REAL          GNAM          ! Harvest N,mature               kg/ha
      REAL          GNAMEAA       ! Grain N/area abs error avg     #
      REAL          GNAMEAV       ! Grain N/area average error     #
      REAL          GNAMERR       ! Harvest N,error                %
      REAL          GNAMM         ! Harvest N,mature,measured      kg/ha
      INTEGER       GNAMNUM       ! Grain N/area error #           #
      REAL          GNAMSUA       ! Grain N/area abs error #       #
      REAL          GNAMSUM       ! Grain N/area error sum         #
      REAL          GNPCM         ! Harvest N%,measured            %
      CHARACTER*6   GNPCMC        ! Harvest N,mature               char
      REAL          GNPCMEAA      ! Grain N% abs error avg         #
      REAL          GNPCMEAV      ! Grain N% average error         #
      REAL          GNPCMERR      ! Harvest N%,error               %
      REAL          GNPCMM        ! Harvest N,mature,measured      %
      CHARACTER*6   GNPCMMC       ! Harvest N,mature,measured      char
      INTEGER       GNPCMNUM      ! Grain N% error #               #
      REAL          GNPCMSUA      ! Grain N% abs error #           #
      REAL          GNPCMSUM      ! Grain N% error sum             #
      REAL          GPLA(10)      ! Green leaf area                cm2/p
      REAL          GPLASENF      ! Green leaf area,final sen strt #
      REAL          GPLASENS      ! Green leaf area,senesce stage  cm2/p
      REAL          GRAINANC      ! Grain N concentration          #
      REAL          GRAINN        ! Grain N                        g/p
      REAL          GRAINNG       ! Grain N growth,uptake          g/p
      REAL          GRAINNGL      ! Grain N growth from leaves     g/p
      REAL          GRAINNGR      ! Grain N growth from roots      g/p
      REAL          GRAINNGS      ! Grain N growth from stems      g/p
      REAL          GRAINNGV      ! Grain N growth from veg parts  g/p
      REAL          GRAINNTMP     ! Grain N,temporary value        g/p
      REAL          GRNMN         ! Grain N minimum concentration  %
      REAL          GRNMX         ! Grain N,maximum concentration  %
      REAL          GRNS          ! Grain N standard concentration %
      REAL          GRNUM         ! Grains per plant               #/p
      REAL          GRNUMAD       ! Grains per unit area           #/m2
      REAL          GROGR         ! Grain growth,current assim     g/p
      REAL          GROGRADJ      ! Grain growth adj,max reached   g/p
      REAL          GROGRP        ! Grain growth potential         g/p
      REAL          GROGRPA       ! Grain growth,possible,assim    g/p
      REAL          GROGRPN       ! Grain growth,possible,N        g/p
      REAL          GROGRST       ! Grain growth from stem ch2o    g/p
      REAL          GROLF         ! Leaf growth rate               g/p
      REAL          GROLFP        ! Leaf growth,potential          g/p
      REAL          GROLFRS       ! Leaf growth from reserves      g/p
      REAL          GRORS         ! Reserves growth                g/p
      REAL          GRORSGR       ! Reserves gr,unused grain assim g/p
      REAL          GRORSP        ! Reserves growth,potential      g/p
      REAL          GRORSPM       ! Reserves growth,post-maturity  g/p
      REAL          GRORSSD       ! Seed reserves used for tops    g/p
      REAL          GROST         ! Stem growth rate               g/p
      REAL          GROSTP        ! Stem growth,potential          g/p
      CHARACTER*1   GROUP         ! Flag for type of group         code
      REAL          GRP_EP        ! Grain dm per unit EP           g/mm
      REAL          GRP_ET        ! Grain dm per unit EP           g/mm
      REAL          GRP_IRR       ! Grain dm per unit irrigation   g/mm
      REAL          GRP_NAPP      ! Grain dm per unit N appllied   kg/kg
      REAL          GRP_NUPT      ! Grain dm per unit N taken up   kg/kg
      REAL          GRP_RAIN      ! Grain dm per unit EP           g/mm
      REAL          GRWT          ! Grain weight                   g/p
      REAL          GRWTTMP       ! Grain weight,temporary value   g/p
      CHARACTER*6   GSTAGEC       ! Growth stage                   #    
      REAL          GSTDM         ! Growth stage,measured          #
      REAL          GWADT         ! Grain weight from t file       kg/ha
      REAL          GWAM          ! Harvest wt,maturity            kg/ha
      REAL          GWAMM         ! Harvest wt,mature,measured     kg/ha
      REAL          GWGD          ! Grain wt per unit              mg
      REAL          GWGM          ! Grain wt per unit,maturity     mg
      REAL          GWGMM         ! Grain wt per unit,mat,measured mg
      REAL          GWUD          ! Grain size                     g
      REAL          GWUM          ! Grain wt per unit,maturity     g
      CHARACTER*6   GWUMC         ! Grain wt/unit,mat              text
      REAL          GWUMEAA       ! Grain wt/unit abs error avg    #
      REAL          GWUMEAV       ! Grain wt/unit average error    #
      REAL          GWUMERR       ! Grain wt per unit error        %
      REAL          GWUMM         ! Grain wt/unit,mat,measured     g
      CHARACTER*6   GWUMMC        ! Grain wt/unit,mat,measured     text
      INTEGER       GWUMNUM       ! Grain wt/unit error #          #
      REAL          GWUMSUA       ! Grain wt/unit abs error #      #
      REAL          GWUMSUM       ! Grain wt/unit error sum        #
      REAL          GWUMYLD       ! Grain wt,mature,calculated     g/#
      REAL          GWUT          ! Grain weight/unit,t file       mg
      REAL          H2OA          ! Water available in root zone   mm
      REAL          H2OPROFILE    ! Total h2o in soil profile      mm   
      REAL          H2OPROFILEI   ! H2o in profile,initial         mm   
      REAL          H2OROOTZONE   ! Total h2o in root zone         mm   
      REAL          H2OROOTZONEI  ! H2o in root zone,initial       mm   
      INTEGER       HADAY         ! Harvest day of year            #
      REAL          HARDAYS       ! Accumulated hardening days     #
      REAL          HARDI         ! Hardening index                #
      REAL          HARDILOS      ! Hardening index loss           #
      !REAL          HARVFRAC(2)   ! Harvested fr from main         #
      INTEGER       HAYEAR        ! Harvest year                   #
      REAL          HBPC          ! Harvest by-product percentage  %
      INTEGER       HDAY          ! Harvest day as read            #
      INTEGER       HDOYF         ! Earliest doy for harvest       #
      INTEGER       HDOYL         ! Last doy for harvest           #
      REAL          HDUR          ! Hardening duration,days        d
      INTEGER       HFIRST        ! Earliest date for harvest      #
      REAL          HIAD          ! Harvest index,above ground     #
      REAL          HIADT         ! Harvest index from t file      #
      REAL          HIAM          ! Harvest index,mature           %
      CHARACTER*6   HIAMC         ! Harvest index,mature           char
      REAL          HIAMEAA       ! Harvest index abs error avg    #
      REAL          HIAMEAV       ! Harvest index average error    #
      REAL          HIAMERR       ! Harvest index,maturity,error   %
      REAL          HIAMM         ! Harvest index,mature,measure   #
      CHARACTER*6   HIAMMC        ! Harvest index,mature,measure   char
      REAL          HIAMMTMP      ! Harvest index,mature,temporary #
      INTEGER       HIAMNUM       ! Harvest index error #          #
      REAL          HIAMSUA       ! Harvest index abs error #      #
      REAL          HIAMSUM       ! Harvest index error sum        #
      REAL          HIND          ! Harvest index,N,above ground   #
      REAL          HINM          ! Harvest index,N,abground,mat   #
      CHARACTER*6   HINMC         ! Harvest index,N,abground,mat   char
      REAL          HINMM         ! Harvest N index,mature,meas    %
      CHARACTER*6   HINMMC        ! Harvest N index,mature,meas    char
      INTEGER       HLAST         ! Last date for harvest          #
      REAL          HNUMAEAA      ! Harvest #/area abs error avg   #
      REAL          HNUMAEAV      ! Harvest #/area average error   #
      REAL          HNUMAERR      ! Harvest #,maturity,error       %
      REAL          HNUMAM        ! Harvest #,maturity             #/m2
      REAL          HNUMAMM       ! Harvest #,mature,measured      #/m2
      INTEGER       HNUMANUM      ! Harvest #/area error #         #
      REAL          HNUMASUA      ! Harvest #/area abs error #     #
      REAL          HNUMASUM      ! Harvest #/area error sum       #
      REAL          HNUMAT        ! Harvest number/area,t file     #/m2
      REAL          HNUMET        ! Harvest number/ear,t file      #/s
      REAL          HNUMGEAA      ! Harvest #/group abs error avg  #
      REAL          HNUMGEAV      ! Harvest #/group average error  #
      REAL          HNUMGERR      ! Harvest #/group,error          %
      REAL          HNUMGM        ! Harvest #,maturity             #/g
      REAL          HNUMGMM       ! Harvest #,mature,measured      #/g
      INTEGER       HNUMGNUM      ! Harvest #/group error #        #
      REAL          HNUMGSUA      ! Harvest #/group abs error #    #
      REAL          HNUMGSUM      ! Harvest #/group error sum      #
      REAL          HPC           ! Harvest percentage             %
      REAL          CHTPC(10)     ! Canopy ht % assoc with lf area %
      REAL          HWADM         ! Harvest wt,measured            kg/ha
      REAL          HWAHEAA       ! Harvest wt/area abs error avg  #
      REAL          HWAHEAV       ! Harvest wt/area average error  #
      REAL          HWAHERR       ! Harvest wt,harvest,error       %
      REAL          HWAHM         ! Harvest wt,harvest,measured    kg/ha
      INTEGER       HWAHNUM       ! Harvest wt/area error #        #
      REAL          HWAHSUA       ! Harvest wt/area abs error #    #
      REAL          HWAHSUM       ! Harvest wt/area error sum      #
      CHARACTER*6   HWUDC         ! Harvest wt/unit                g
      INTEGER       HYEAR         ! Harvest year as read           #
      INTEGER       HYEARF        ! Earliest year for harvest      #
      INTEGER       HYEARL        ! Last year for harvest          #
      INTEGER       I             ! Loop counter                   #
      INTEGER       ICSDUR        ! Stage (phase) duration         d
      INTEGER       ICSDUR0       ! Growth duration                d
      !CHARACTER*1   IDETG         ! Control flag,growth outputs    code
      !CHARACTER*1   IDETL         ! Control switch,detailed output code
      !CHARACTER*1   IDETO         ! Control flag,overall outputs   code
      CHARACTER*1   IDETOU        ! Control flag,error op,inputs   code
      !CHARACTER*1   IDETS         ! Control switch,summary outputs code
      INTEGER       IEDAT         ! Inflorescence emergenceYeardoy #
      CHARACTER*1   IFERI         ! Fertilizer switch (A,R,D)      code
      CHARACTER*1   IHARI         ! Control flag,harvest           code
      !INTEGER       INTEGR        ! Program control variable (=4)  #
      CHARACTER*1   IPLTI         ! Code for planting date method  code
      INTEGER       ISTAGE        ! Developmental stage            #
      INTEGER       ISTAGEP       ! Developmental stage,previous   #
      !CHARACTER*1   ISWNIT        ! Soil nitrogen balance switch   code
      !CHARACTER*1   ISWWAT        ! Soil water balance switch Y/N  code
      INTEGER       JDAP          ! Jointing date (Year+doy)       #
      INTEGER       JDAPM         ! Jointing date,measured,Yeardoy #
      INTEGER       JDAT          ! Jointing date (Year+doy)       #
      INTEGER       JDATM         ! Jointing date,measured,Yeardoy #
      !REAL          KCAN          ! Extinction coeff for PAR       #
      REAL          KCANI         ! Extinction coeff,PAR,init.val. #
      !REAL          KEP           ! Extinction coeff for SRAD      #
      REAL          KEPI          ! Extinction coeff,SRAD,init val #
      INTEGER       L             ! Loop counter                   #
      INTEGER       L0            ! Layer with seed                #
      INTEGER       L1            ! Loop counter                   #
      INTEGER       L2            ! Loop counter                   #
      REAL          LA1FAC        ! Area of early leaves increment #
      CHARACTER*5   LABEL(SUMNUM) ! Name of variables passed       text
      REAL          LAFR          ! Leaf area increase factor,rep  #
      REAL          LAFST         ! Leaf area factor change stage  #
      REAL          LAFV          ! Leaf area increase factor,veg  #
      REAL          LAGSTAGE      ! Lag phase,grain filling stage  #
      !REAL          LAI           ! Leaf area index                #
      CHARACTER*6   LAIC          ! Leaf area index                m2/m2
      REAL          LAIL(30)      ! Leaf area index by layer       m2/m2
      REAL          LAIPROD       ! Leaf area index produced       #
      CHARACTER*6   LAIPRODC      ! Leaf area index produced       m2/m2
      REAL          LAISTG(20)    ! Leaf area index,specific stage #
      REAL          LAIX          ! Leaf area index,maximum        #
      REAL          LAIXEAA       ! Leaf area index,max,abs err av #
      REAL          LAIXEAV       ! Leaf area index,max,average er #
      REAL          LAIXERR       ! Leaf area index,max,error      %
      REAL          LAIXM         ! Lf lamina area index,mx,meas   m2/m2
      INTEGER       LAIXNUM       ! Leaf area index,max, error #   #
      REAL          LAIXSUA       ! Leaf area index,max, abs error #
      REAL          LAIXSUM       ! Leaf area index,max, error sum #
      REAL          LAIXT         ! Leaf area index,max,t-file     m2/m2
      REAL          LALOSSF       ! Leaf area lost if tillers die  fr
      REAL          LANC          ! Leaf N concentration           #
      REAL          LAP(LNUMX)    ! Leaf area at leaf position     cm2/p
      REAL          LAPOT(LNUMX)  ! Leaf area potentials           cm2/l
      REAL          LAPOTNXT      ! Leaf area potential of next lf cm2/l
      REAL          LAPP(LNUMX)   ! Leaf area diseased,leaf posn   cm2/p
      REAL          LAPS(LNUMX)   ! Leaf area senesced,leaf posn   cm2/p
      REAL          LARFAC        ! Area of later leaves increment #
      REAL          LASENLF       ! Leaf area of senescing leaf ch cm2/p
      REAL          LASWITCH      ! Leaf area at increment change  cm2/l
      REAL          LATFR(20)     ! Leaf area of tillers,fr main   #
      REAL          LATL(1,LNUMX) ! Leaf area,tiller1,leaf pos     cm2/l
      REAL          LAWCF         ! Leaf area/wt change,fr         fr/lf
      REAL          LAWCFCALC     ! Leaf area/wt change,fr,calc    #
      REAL          LAWFRMN       ! Leaf area/wt min.,fr.standard  #
      REAL          LAW           ! Area to weight ratio,lamina    cm2/g
      REAL          LAWS          ! Leaf area/wt ratio,standard    cm2/g
      REAL          LAXS          ! Area of biggest leaf,main stem cm2
      REAL          LCNC          ! Leaf critical N conc           #
      REAL          LCNCG         ! Critical N conc for growth     #
      REAL          LCNCP         ! Critical N conc for phs        #
      REAL          LCNCS(0:9)    ! Leaf critical N conc,stage     #
      REAL          LCNCSEN       ! Critical N conc for senescence #
      REAL          LCNCT         ! Critical N conc for tillering  #
      REAL          LCNF          ! Leaf N as fr of critical 0-1   #
      INTEGER       LCNUM         ! Leaf cohort number (inc.grow)  #
      REAL          LEAFN         ! Leaf N                         g/p
      INTEGER       LENGROUP      ! Length of group name           #
      INTEGER       LENLINE       ! Length of characgter string    #
      INTEGER       LENRNAME      ! Length of run description      #
      INTEGER       LENTNAME      ! Length,treatment description   #
      REAL          LFFR          ! Leaf fraction,during phase     #
      REAL          LFWT          ! Leaf weight                    g/p
      REAL          LIF1          ! Light interception factor 1    #
      REAL          LIF2          ! Light interception factor 2    #
      CHARACTER*80  LINESTAR      ! Group header line (with star)  text
      CHARACTER*180 LINET         ! Line from T-file               text
      !REAL          LL(20)        ! Lower limit,soil h2o           #
      INTEGER       LLDATM        ! Last leaf (mature) date,measured
      INTEGER       LLIFE         ! Leaf longevity (phyllochrons)  #
      REAL          LLIGP         ! Leaf lignin percentage         #
      REAL          LLOSN         ! Leaf area lost,N accelerated   fr/d
      REAL          LLOSW         ! Leaf area lost,H2O accelerated fr/d
      REAL          LLRSWAD       ! Leaf lamina reserves weight    kg/ha
      REAL          LLRSWT        ! Leaf lamina reserves           g/p
      REAL          LMNC          ! Leaf minimum N conc            #
      REAL          LMNCG         ! Minimum N conc for growth      #
      REAL          LMNCP         ! Minimum N conc for phs         #
      REAL          LMNCS(0:9)    ! Leaf minimum N conc,stage      #
      REAL          LMNCT         ! Minimum N conc for tillering   #
      REAL          LNAAM         ! Leaf N,anthesis,measured       kg/ha
      REAL          LNDEM         ! Leaf demand for N              g/p
      REAL          LNPCA         ! Leaf N % at anthesis           #
      REAL          LNPCAM        ! Leaf N,anthesis,measured       %
      REAL          LNPCMN(0:9)   ! Leaf minimum N conc,stage      %
      REAL          LNPCS(0:9)    ! Leaf standard N conc,stage     %
      REAL          LNSWITCH      ! Leaf # at increment change     #
      REAL          LNUMSD        ! Leaf number,Haun stage         #
      REAL          LNUMSEAA      ! Leaf #/shoot abs error avg     #
      REAL          LNUMSEAV      ! Leaf #/shoot average error     #
      REAL          LNUMSERR      ! Leaf #,error                   %
      INTEGER       LNUMSG        ! Growing leaf number            #
      REAL          LNUMSM        ! Leaf #/shoot,maturity          #
      REAL          LNUMSMM       ! Leaf #,mature,measured         #/s
      INTEGER       LNUMSNUM      ! Leaf #/shoot error #           #
      REAL          LNUMSSUA      ! Leaf #/shoot abs error #       #
      REAL          LNUMSSUM      ! Leaf #/shoot error sum         #
      REAL          LNUMSTG(20)   ! Leaf number,specific stage     #
      REAL          LNUMT         ! Leaf number from t file        #
      REAL          LNUMTS        ! Leaf number,Terminal spikelet  #
      INTEGER       LLDAP         ! Last leaf fully expanded date  Dap
      INTEGER       LLDAT         ! Last leaf fully expanded date  YrDoy
      REAL          LRETS         ! Stage --> dead leaves retained #
      REAL          LSENE         ! Leaf senescence,end stage      #
      REAL          LSENNF        ! Leaf N loss when senesce,fr    #
      REAL          LSENS         ! Leaf senescence,start stage    #
      REAL          LWLOS         ! Leaf wt loss when senesce,fr   #
      REAL          LSHAWS        ! Leaf sheath area/wt ratio,std  cm2/g
      REAL          LSHAI         ! Leaf sheath area index         #
      REAL          LSHFR         ! Leaf sheath fraction of total  #
      REAL          LSHRSWT       ! Leaf sheath reserves           g/p
      REAL          LT50H         ! Lethal temp,50%kill,hardened   C
      REAL          LT50S         ! Lethal temp,50%kill,seedling   C
      REAL          LWAAM         ! Leaf wt,anthesis,measured      kg/ha
      INTEGER       MDAP          ! Maturity days after planting   #
      INTEGER       MDAPM         ! Maturity DAP,measured          #
      INTEGER       MDATEAA       ! Maturity date abs error avg    #
      INTEGER       MDATEAV       ! Maturity date average error    #
      INTEGER       MDATERR       ! Maturity date error            d
      INTEGER       MDATM         ! Maturity date,measured yr+d    #
      INTEGER       MDATNUM       ! Maturity date error #          #
      INTEGER       MDATSUA       ! Maturity date abs error #      #
      INTEGER       MDATSUM       ! Maturity date error sum        #
      REAL          MDATT         ! Maturity date from t file      YrDoy
      INTEGER       MDAY          ! Maturity day of year           d
      INTEGER       MDAYM         ! Maturity day of year,measured  d
      CHARACTER*8   MODEL         ! Name of model                  text
      CHARACTER*8   MODNAME       ! Name of module                 text
      CHARACTER*3   MONTH         ! Month                          text
      INTEGER       MYEAR         ! Maturity year                  #
      INTEGER       MYEARM        ! Maturity year,measured         #
      REAL          NCNU          ! N conc.for maximum N uptake    ppm  
      REAL          NCRG          ! N concentration for max r.gr.  ppm
      REAL          NDEMSOIL      ! Plant nitrogen demand,soil     g/p
      INTEGER       NFERT         ! Fertilizer applns #            #
      REAL          NFG           ! N factor,growth 0-1            #
      REAL          NFGAV(0:9)    ! N factor,growth,average,phase  #
      REAL          NFGC          ! N factor,growth,cumulative     #
      REAL          NFGC0         ! N factor,growth,cycle          #
      REAL          NFGL          ! N factor,gr,lower limit        #
      REAL          NFGU          ! N factor,gr,upper limit        #
      REAL          NFLF(LNUMX)   ! N factor for leaf,average      #
      REAL          NFLFSUM(LNUMX)! N factor for leaf,sum          #
      !REAL          NFP           ! N factor,photosynthesis 0-1    #
      REAL          NFPAV(0:9)    ! N factor,phs,average,phase     #
      REAL          NFPC          ! N factor,phs,cumulative        #
      REAL          NFPC0         ! N factor,photosynthesis,cycle  #
      REAL          NFPL          ! N factor,phs,lower limit       #
      REAL          NFPU          ! N factor,phs,upper limit       #
      REAL          NFRG          ! N factor,root growth 0-1       #  
      REAL          NFSF          ! N factor,final senescn trigger #
      REAL          NFSU          ! N factor,sen,upper limit       #
      REAL          NFT           ! N factor,tillering 0-1         #
      REAL          NFTL          ! N factor,tillering,lower limit #
      REAL          NFTU          ! N factor,tillering,upper limit #
!      REAL          NH4LEFT(20)   ! NH4 concentration in soil      mg/Mg
      REAL          NH4MN         ! NH4 conc minimum for uptake    mg/Mg
!      INTEGER       NLAYR         ! Number of layers in soil       #
      REAL          NLAYRROOT
      INTEGER       NLIMIT        ! Number of days N limited gr gr #
!      REAL          NO3LEFT(20)   ! NO3 concentration in soil      mg/Mg
      REAL          NO3MN         ! NO3 conc minimum for uptake    mg/Mg
      INTEGER       NOUTPG        ! Number for growth output file  #
      INTEGER       NOUTPN        ! Number for plant N output file #
      INTEGER       NOUTPG2       ! Number for growth2 output file #
      INTEGER       NOUTPGF       ! Number for growthF output file #
      REAL          NPOOLL        ! Leaf N available for grain     g/p
      REAL          NPOOLR        ! Root N available for grain     g/p
      REAL          NPOOLST       ! Stem N available for grain     g/p
      REAL          NPOOLV        ! Vegetative N available for gr  g/p
      REAL          NSFAC         ! N standard adjustment factor   fr 
      REAL          NMNFC         ! N minimum adjustment factor    fr 
      REAL          NSINK         ! N demand for grain filling     g/p
      REAL          NTUPF         ! N top-up fraction              /d
      REAL          NUAD          ! N uptake,cumulative            kg/ha
      REAL          NUAMM         ! N uptake,cumulative,maturity,m kg/ha
      REAL          NUAG          ! N uptake                       kg/ha
      REAL          NUAM          ! N uptake,cumulative,maturity   kg/ha
      REAL          NUF           ! Plant N supply/demand ratio    ft
      REAL          NUFACM        ! N uptake factor,maturation,0-1 #
      REAL          NUPAP         ! N uptake potential             g/p
      REAL          NUPC          ! N uptake,cumulative            g/p
      REAL          NUPD          ! N uptake                       g/p
      REAL          NUPR          ! N uptake ratio to demand       #
      REAL          NUSEFAC       ! N use factor;mx nuselim,navfr  #
      REAL          NUSELIM       ! N limit on N for grain filling #
      !INTEGER       ON            ! Option number (sequence runs)  #
      INTEGER       ONI           ! Option number,initial value    #
      CHARACTER*3   OUT           ! Output file extension          text
      INTEGER       OUTCHOICE     ! Output choice (+reserves,etc)  #
      INTEGER       OUTCOUNT      ! Output counter                 #
      CHARACTER*77  OUTHED        ! Output file heading            text
      CHARACTER*12  OUTPG         ! Growth output file code        code
      CHARACTER*12  OUTPG2        ! Growth output2 file code       code
      CHARACTER*12  OUTPGF        ! Growth output GF file code     code
      CHARACTER*12  OUTPN         ! GrowthN output file code       code
      !INTEGER       OUTPUT        ! Program control variable (=5)  #
      REAL          P1D           ! Photoperiod sensitivity coeff. %/10h
      REAL          P1DA          ! Photoperiod coeff,age adjusted /h
      REAL          P1DAFAC       ! Photoperiod coeff,adjust fac   /lf
      REAL          P1DT          ! Photoperiod threshold          h
      REAL          P1V           ! Vernalization requirement      d
      REAL          P4SGE         ! Stem growth end,X-stage        #
      REAL          PARAD         ! Photosynthetically active radn MJ/m2
      REAL          PARADCUM      ! PAR cumulative                 MJ/m2
      REAL          PARADICUM     ! Par intercepted,cumulative     MJ/m2
      REAL          PARADFAC      ! PAR conversion factor          MJ/MJ
      REAL          PARI          ! PAR interception fraction      #
      REAL          PARIOUT       ! PAR interception for output    #
      REAL          PARIP         ! PAR interception percentage    %
      REAL          PART          ! PAR transmission fraction      #
      REAL          PARUED        ! PAR utilization effic,actual   g/MJ
      REAL          PARIUEM       ! PARUE,actual,at maturity       g/MJ
      REAL          PARUR         ! PAR utilization effic,reprod   g/MJ
      REAL          PARUV         ! PAR utilization effic,veg      g/MJ
      CHARACTER*250 PATHCR        ! Path to genotype files         text
      INTEGER       PATHL         ! Path length                    #
      REAL          PCARB         ! Potential carbon fixation      g/p
      REAL          PD(0:10)      ! Phase durations                deg.d
      REAL          PD2(3)        ! Phase 2 sub-durations;1<joint  deg.d
      REAL          PD2ADJ        ! Phase 2 adjusted               deg.d
      REAL          PD2FR(3)      ! Phase 2 sub-durations;1<joint  fr
      REAL          PD3FR(3)      ! Phase 3 sub-durations;1<headng fr
      REAL          PD4(3)        ! Phase 4 sub-durations;1<anthes deg.d
      REAL          PD4FR(3)      ! Phase 4 sub-durations;1<anthes fr
      REAL          PDADJ         ! Phase duration adjustment      deg.d
      INTEGER       PDATE         ! Planting date from X-file      #
      REAL          PEMRG         ! Phase duration,emergence       Cd/cm
      REAL          PGERM         ! Phase duration,germination     deg.d
      REAL          PEGD          ! Phase duration,germ+dormancy   deg.d
      REAL          PFGAV(0:9)    ! Phosphorous factor,growth      #
      REAL          PFPAV(0:9)    ! Phosphorous factor,phs         #
      INTEGER       PGDAP         ! Plantgro file days after plt   #
      INTEGER       PGROCOL(20)   ! Plantgro column = t-file data  #
      REAL          PGVAL         ! Plantgro file value            #
      REAL          PHINT         ! Phylochron interval            deg.d
      REAL          PHINTF(5)     ! Phint factor                   #
      REAL          PHINTL(5)     ! Phylochron interval,change lf# #
      REAL          PHINTS        ! Phylochron interval,standard   deg.d
      REAL          PLA           ! Plant leaf area                cm2
      REAL          PLAG(2)       ! Plant leaf area growth,tiller1 cm2/t
      REAL          PLAGT(2)      ! Plant leaf area growth,total   cm2/p
      REAL          PLAGTP(2)     ! Plant lf area growth,potential cm2/p
      REAL          PLAS          ! Leaf area senesced,normal      cm2/p
      REAL          PLASC         ! Leaf area senesced,cold        cm2/p
      REAL          PLASF(10)     ! Leaf area senesced,fr in phase #
      REAL          PLASS         ! Leaf area senesced,stress      cm2/p
      REAL          PLAST         ! Leaf area senesced,tiller loss cm2/p
      REAL          PLASTMP       ! Leaf area senesced,temporary   cm2/p
      INTEGER       PLDAY         ! Planting day of year           #
      INTEGER       PLDAYP        ! Planting day of year           #
      REAL          PLMAGE        ! Planting material age          d
      REAL          PLTLOSS       ! Plant popn lost through cold   #/m2
      INTEGER       PLTOHARYR     ! Planting to harvest years      #
      REAL          PLTPOP        ! Plant Population               #/m2
      REAL          PLTPOPP       ! Plant Population planned       #/m2
      INTEGER       PLYEAR        ! Planting year                  #
      INTEGER       PLYEARREAD    ! Planting year as read          #
      REAL          PPEND         ! Photoperiod sensitivity end    gstg
      REAL          PPFPE         ! Photoperiod factor,pre-emerge  #
      REAL          PTF           ! Partition fraction to tops     #
      REAL          PTFA(10)      ! Partition fr adjustment coeff. #
      REAL          PTFS(10)      ! Partition fraction by stage    #
      REAL          PTFSS         ! Partition fraction for stage   #
      REAL          PTFX          ! Partition fraction,maximum     #
      REAL          PTH(0:10)     ! Phase thresholds               du
      REAL          PTHOLD        ! Phase threshold,previous       du
      REAL          PTTN          ! Minimum soil temperature,plt   C
      REAL          PTX           ! Maximum soil temperature,plt   C
      REAL          PWAM          ! Chaff + seed wt,maturity       kg/ha
      INTEGER       PWDINF        ! First day of planting window   YYDDD
      INTEGER       PWDINL        ! Last day of planting window    YYDDD
      INTEGER       PWDOYF        ! First doy of planting window   doy
      INTEGER       PWDOYL        ! Last doy of planting window    doy
      INTEGER       PWYEARF       ! First year of planting window  yr
      INTEGER       PWYEARL       ! Last year of planting window   yr
      !REAL          RAIN          ! Rainfall                       mm
      REAL          RAINC         ! Rainfall,cumulative            mm
      REAL          RAINCA        ! Rainfall,cumulativ to anthesis mm
      REAL          RAINCP(0:9)   ! Cumulative precip in phase     mm
      REAL          RAINSUM       ! Precipitation sum in phase     mm
      REAL          RAINSUM0      ! Precipitation during cycle     mm
      REAL          RANC          ! Roots actual N concentration   #
      !INTEGER       RATE          ! Program control variable (=3)  #
      REAL          RCNC          ! Root critical N concentration  #
      REAL          RCNCS(0:9)    ! Roots critical N conc,by stage #
      REAL          RCNF          ! Roots N as fr of critical 0-1  #
      REAL          RCNP          ! Root critical N conc,original  #
      REAL          RDGS1         ! Root depth growth rate,initial cm/d
      REAL          RDGS2         ! Root depth growth rate,2nd     cm/d
      REAL          RDGTH         ! Rooting depth growth thrshold  cm
      !INTEGER       REP           ! Number of run repetitions      #
      REAL          RESPC         ! Respiration,total,cumulative   g/p
      REAL          RFAC          ! Root length & H2O fac,N uptake #
      REAL          RLDF(20)      ! Root length density fac,new gr #
      REAL          RLDGR         ! Root length/root depth g ratio cm/cm
      REAL          RLFNU         ! Root length factor,N uptake    #
      REAL          RLIGP         ! Root lignin concentration      %
      !REAL          RLV(20)       ! Root length volume by layer    /cm2
      REAL          RLWR          ! Root length/weight ratio       m/mg?
      REAL          RMNC          ! Root minimum N conc            g/g
      REAL          RMNCS(0:9)    ! Roots minimum N conc,by stage  #
      REAL          RMNP          ! Root minimum N conc,original   g/g
      !INTEGER       RN            ! Treatment replicate            #
      REAL          RNAM          ! Root N at maturity             kg/ha
      REAL          RNAMM         ! Root N at maturity,measured    kg/ha
      REAL          RNDEM         ! Root demand for N              g/p
      REAL          RNDEM1        ! Root demand for N,overall      g/p
      REAL          RNDEMSOIL     ! Root demand for N,soil         g/p
      REAL          RNH4U(20)     ! Potential ammonium uptake      kg/ha
      INTEGER       RNI           ! Replicate number,initial value #
      !CHARACTER*1   RNMODE        ! Run mode (eg.I=interactive)    #
      REAL          RNO3U(20)     ! Potential nitrate uptake       kg/ha
      REAL          RNPCMN(0:9)   ! Roots minimum N conc,by stage  %
      REAL          RNPCS(0:9)    ! Roots standard N conc,by stage %
      REAL          ROOTN         ! Root N                         g/p
      REAL          ROOTNS        ! Root N senesced                g/p
      REAL          ROWSPC        ! Row spacing                    cm
      REAL          RRESP         ! Root respiration fraction      #
      REAL          RSCD          ! Reserves concentration today   fr
      REAL          RSCA          ! Reserves concentration,anthess fr
      REAL          RSCLX         ! Reserves conc,leaves,max.      #
      REAL          RSEN          ! Root senescence fraction       #
      REAL          RSFP          ! Reserves factor,photosynthesis #
      REAL          RSFR          ! Reserves fraction              fr
      REAL          RSPCS         ! Reserves %,standard            #
      REAL          RSN           ! Reserve N                      g/p
      REAL          RSNGL         ! Reserves N growth from leaves  g/p
      REAL          RSNGR         ! Reserves N growth from roots   g/p
      REAL          RSNGS         ! Reserves N growth from stems   g/p
      REAL          RSNUSEG       ! Reserves N use for grain       g/p
      REAL          RSNUSER       ! Reserves N use for root growth g/p
      REAL          RSNUSET       ! Reserves N use for top growth  g/p
      REAL          RSNV          ! Reserve N for vegetative gr    g/p
      REAL          RSPCX         ! Reserves conc.max.->overflow   #
      REAL          RSTAGE        ! Reproductive develoment stage  #
      REAL          RSTAGEP       ! Reproductive stage,previous    #
      REAL          RSUSE         ! Reserves utilisation fraction  #
      REAL          RSWAM         ! Reserves at maturity           kg/ha
      REAL          RSWAMM        ! Reserves at maturity,measured  kg/ha
      REAL          RSWT          ! Reserves weight                g/p
      REAL          RSWTPM        ! Reserves weight,post maturity  g/p
      REAL          RTDEP         ! Root depth                     cm
      REAL          RTDEPG        ! Root depth growth              cm/d
      REAL          RTDEPTMP      ! Root depth,temporary value     cm/d
      REAL          RTNO3         ! N uptake/root length           mg/cm
      REAL          RTNH4         ! N uptake/root length           mg/cm
      REAL          RTNSL(20)     ! Root N senesced by layer       g/p
      REAL          RTRESP        ! Root respiration               g/p
      INTEGER       RTSLXDATE     ! Roots into last layer date     YYDDD
      REAL          RTSW          ! Tiller wt relative to standard #
      REAL          RTWT          ! Root weight                    g/p
      REAL          RTWTG         ! Root weight growth             g/p
      REAL          RTWTGL(20)    ! Root weight growth by layer    g/p
      REAL          RTWTGRS       ! Root growth from surplus rs    g/p
      REAL          RTWTGS        ! Root weight growth from seed   g/p
      REAL          RTWTL(20)     ! Root weight by layer           g/p
      REAL          RTWTS         ! Root weight senesced           g/p
      REAL          RTWTSL(20)    ! Root weight senesced by layer  g/p
      !INTEGER       RUN           ! Run (from command line) number #
      !INTEGER       RUNI          ! Run (internal for sequences)   #
      !INTEGER       RUNINIT       ! Program control variable (= 1) #
      CHARACTER*25  RUNNAME       ! Run title                      text
      CHARACTER*8   RUNRUNI       ! Run+internal run number        text
      REAL          RWAM          ! Root wt at maturity            kg/ha
      REAL          RWAMM         ! Root wt at maturity,measured   kg/ha
!      REAL          RWUMX         ! Root water uptake,maximum      mm2/m
      REAL          RWUMXI        ! Root water uptake,max,init.val mm2/m
      REAL          RWUMXS        ! Root water uptake,maximum,std  mm2/m
!      REAL          RWUPM         ! Pore space threshold,pl effect #
      REAL          RWUPMI        ! Pore space threshold,initial   #
      REAL          SAID          ! Stem area index                m2/m2
      REAL          SAIDOUT       ! Stem area index for output     m2/m2
      REAL          SANC          ! Stem N concentration           #
!      REAL          SAT(20)       ! Saturated limit,soil           #
      REAL          SAWS          ! Stem area to wt ratio,standard cm2/g
      REAL          SCNC          ! Stem critical N conc           #
      REAL          SCNCS(0:9)    ! Stem critical N conc,stage     #
      REAL          SCNF          ! Stem N as fr of critical 0-1   #
      REAL          SDAFR         ! Seed reserves fraction avail   #
      REAL          SDCOAT        ! Non useable material in seed   g
      REAL          SDEPTH        ! Sowing depth                   cm
      REAL          SDNAP         ! Seed N at planting             kg/ha
      REAL          SDNC          ! Seed N concentration           #
      REAL          SDNPCI        ! Seed N concentration,initial   %
      REAL          SDSZ          ! Seed size                      g
!      INTEGER       SEASEND       ! Program control variable (= 6) #
      CHARACTER*1   SEASENDOUT    ! Season end outputs flag        text
!      INTEGER       SEASINIT      ! Program control variable (=2)  #
      REAL          SEEDN         ! Seed N                         g/p
      REAL          SEEDNI        ! Seed N,initial                 g/p
      REAL          SEEDNR        ! Seed N used by roots           g/p
      REAL          SEEDNRA       ! Seed N used by roots,additionl g/p
      REAL          SEEDNRB       ! Seed N used by roots,basic     g/p
      REAL          SEEDNT        ! Seed N used by tops            g/p
      REAL          SEEDNTA       ! Seed N used by tops,additional g/p
      REAL          SEEDNTB       ! Seed N used by tops,basic      g/p
      REAL          SEEDRS        ! Seed reserves                  g/p
      REAL          SEEDRSAV      ! Seed reserves available        g/p
      REAL          SEEDRSI       ! Seed reserves,initial          g/p
      REAL          SEEDRSUX      ! Seed reserves use maximum      g/pdd
      REAL          SENCL(0:20)   ! Senesced C,by layer            g/p
      REAL          SENCS         ! Senesced C added to soil       g/p
      REAL          SENLA         ! Senesced leaf area,total       cm2/p
      REAL          SENLALITTER   ! Senesced leaf area,litter      cm2/p
      REAL          SENLARETAINED ! Senesced leaf area,retained    cm2/p
      REAL          SENLFG        ! Senesced leaf                  g/p
      REAL          SENLFGRS      ! Senesced leaf to reserves      g/p
      REAL          SENRS         ! Reserves lost with killed leaf g/p
      REAL          SENRSC        ! Reserves lost,killed leaf,cum  g/p
      REAL          SENLGL(0:20)  ! Senesced lignin added,by layer g/p
      REAL          SENLGS        ! Senesced lignin added to soil  g/p
      REAL          SENNATC       ! Senesced N,litter+soil,cum     kg/ha
      REAL          SENNATCM      ! Senesced N,litter+soil,cum,mes kg/ha
      REAL          SENNL(0:20)   ! Senesced N,by layer            g/p
      REAL          SENNLFG       ! Senesced N from leaves         g/p
      REAL          SENNLFGRS     ! Senesced N from leaves,to rs   g/p
      REAL          SENNS         ! Senesced N added to soil       g/p
      REAL          SENNSTG       ! Senesced N from stems          g/p
      REAL          SENNSTGRS     ! Senesced N to rs from stems    g/p
      REAL          SENSTG        ! Senesced material from stems   g/p
      REAL          SENWATC       ! Senesced om,litter+soil,cum    kg/ha
      REAL          SENWATCM      ! Senesced om,litter+soil,cum    kg/ha
      REAL          SENWL(0:20)   ! Senesced om added by layer     g/p
      REAL          SENWS         ! Senesced weight,soil           g/p
!      REAL          SHF(20)       ! Soil hospitality factor 0-1    #
      REAL          SHRTD         ! Shoot/root ratio               #
      REAL          SLA           ! Specific leaf area             cm2/g
      REAL          SLAOUT        ! Specific leaf area for output  cm2/g
      REAL          SLIGP         ! Stem lignin concentration      %
!      REAL          SLPF          ! Soil factor for photosynthesis %
      REAL          SMDFR         ! Soil moisture factor,N uptake  #
      REAL          SMNC          ! Stem minimum N conc            #
      REAL          SMNCS(0:9)    ! Stem minimum N conc,stage      #
!      INTEGER       SN            ! Sequence number,crop rotation  #
      REAL          SNDEM         ! Stem demand for N              g/p
      REAL          SNH4(20)      ! Soil NH4 N                     kg/ha
      REAL          SNH4PROFILE   ! NH4 N in soil profile          kg/ha
      REAL          SNH4PROFILEI  ! NO3 N in soil profile,initial  kg/ha
      REAL          SNH4ROOTZONE  ! NH4 N in root zone             kg/ha
      REAL          SNH4ROOTZONEI ! NH4 N in root zone,initial     kg/ha
      INTEGER       SNI           ! Sequence number,as initiated   #
      REAL          SNO3(20)      ! Soil NO3 N                     kg/ha
      REAL          SNO3PROFILE   ! NO3 N in soil profile          kg/ha
      REAL          SNO3PROFILEI  ! NO3 N in soil profile,initial  kg/ha
      REAL          SNO3ROOTZONE  ! NO3 N in root zone             kg/ha
      REAL          SNO3ROOTZONEI ! NO3 N in root zone,initial     kg/ha
      !REAL          SNOW          ! Snow                           cm
      REAL          SNPCMN(0:9)    ! Stem minimum N conc,stage     %
      REAL          SNPCS(0:9)    ! Stem standard N conc,stage     %
      INTEGER       SPDATM        ! Spike emergence date,measured  #
      CHARACTER*250 SPDIRFLE      ! Species directory+file         text
      CHARACTER*250 SPDIRFLP      ! Species directory+file,last    text
      CHARACTER*12  SPFILE        ! Species filename               text
!      REAL          SRAD          ! Solar radiation                MJ/m2
      REAL          SRAD20        ! Solar radiation av,20 days     MJ/m2
      REAL          SRAD20A       ! Solar radn av,20 days,anthesis MJ/m2
      REAL          SRAD20S       ! Solar radiation sum            MJ/m2
      REAL          SRADAV(0:9)   ! Average solar radn in phase    MJ/m2
      REAL          SRADC         ! Solar radiation,cumulative     MJ/m2
      REAL          SRADD(20)     ! Solar radiation on specific d  MJ/m2
      REAL          SRADSUM       ! Solar radiation sum in phase   MJ/m2
      REAL          SRADSUM0      ! Solar radiation sum for cycle  MJ/m2
      REAL          SRADT         ! SRAD transmission fraction     #
      REAL          SSEN          ! Stem senescence fraction       #
      REAL          SSENF         ! Stem N loss when senesce       #
      REAL          SSSTG         ! Stem senescence start stage    #
      REAL          SSTAGE        ! Secondary stage of development #
!     REAL          ST(0:NL)      ! Soil temperature in soil layer C
      INTEGER       STARNUM       ! Star line number,as read file  #
      INTEGER       STARNUMM      ! Star line number,measured data #
      INTEGER       STARNUMO      ! Star line number,output file   #
      REAL          STDAY         ! Standard day                   C.d/d
      REAL          STEMN         ! Stem N                         g/p
!     INTEGER       STEP          ! Step number                    #
      INTEGER       STEPNUM       ! Step number per day            #/d
      REAL          STFR(10)      ! Stem fractions of assimilates  #
      REAL          STFRSTG       ! Stem fraction,todays stage     #
!     INTEGER       STGDOY(20)    ! Stage dates (Year+Doy)         #
      CHARACTER*10  STNAME(20)    ! Stage names                    text
      REAL          STRESS(20)    ! Minim H2O,N factors,last 20day # 
      REAL          STRESS20      ! Minim H2O,N factors,20day avg  #
      REAL          STRESS20A     ! Minim H2O,N factors,20day avg  #
      REAL          STRESS20S     ! Minim H2O,N factors,20day sum  #
      REAL          STRSWT        ! Stem reserves                  g/p
      REAL          STWT          ! Stem weight                    g/p
!     REAL          SW(20)        ! Soil water content             #
      REAL          SWDF          ! Soil water deficit factor 0-1  #
      REAL          SWP(0:20)     ! Soil water 'potential'         #
      REAL          SWPLTD        ! Depth for average soil water   cm
      REAL          SWPLTH        ! Upper limit on soil water,plt  %
      REAL          SWPLTL        ! Lower limit on soil water,plt  %
      REAL          SWPSD         ! Soil water potential at seed   #
      CHARACTER*6   TCHAR         ! Temporary character string     #
      REAL          TCNP          ! Critical N concentration       %
      REAL          TDAY          ! Temperature during light hours C
!     REAL          TFAC4         ! Temperature factor function    #
      INTEGER       TFCOLNUM      ! T-file column number           #
      INTEGER       TFDAP         ! T-file days after planting     #
      INTEGER       TFDAPCOL      ! T-file DAP column #            #
      REAL          TFG           ! Temperature factor,growth 0-1  #
      REAL          TFGEM         ! Temperature factor,germ,emrg   #
      REAL          TFGF          ! Temperature factor,gr fill 0-1 #
      REAL          TFGN          ! Temperature factor,grain N 0-1 #
      REAL          TFH           ! Temperature factor,hardening   #
      REAL          TFLEAVA(10)   ! ERT-file average absolute err  #
      REAL          TFLEAVE(10)   ! ERT-file average error         #
      INTEGER       TFLECOL(10)   ! ERT-file error column          #
      REAL          TFLENUM(10)   ! ERT-file error number          #
      REAL          TFLEVAA(10)   ! ERT-file error absoulte value  #
      REAL          TFLEVAL(10)   ! ERT-file error value           #
      REAL          TFLF(LNUMX)   ! Temp factor for leaf,average   #
      REAL          TFLFSUM(LNUMX)! Temp factor for leaf,sum       #
      REAL          TFOUT         ! Temperature factor,fn output   #
      REAL          TFP           ! Temperature factor,phs 0-1     #
      REAL          TFV           ! Temperature factor,vernalizatn #
      REAL          TFVAL         ! T-file value                   #
      CHARACTER*6   THEAD(20)     ! T-file headings                #
      REAL          TI1LF         ! Tiller 1 site (leaf #)         #
      REAL          TIFAC         ! Tillering rate factor 0-2      #
      REAL          TILDE         ! Tiller death end stage         #
      REAL          TILDF         ! Tiller death rate,max fraction #
      REAL          TILDS         ! Tiller death start stage       #
      REAL          TILPE         ! Tiller number growth end stg   #
      REAL          TILSW         ! Tiller standard weight         g/s
      REAL          TILWT         ! Tiller weight                  g/s
      REAL          TKILL         ! Temperature for plant death    C
      REAL          TKLF          ! Temperature for leaf kill      C
!     CHARACTER*10  TL10FROMI     ! Temporary line from integer    text
      INTEGER       TLIMIT        ! Number of days temp limited gr #
      CHARACTER*180 TLINEGRO      ! Temporary line from GRO file   text
      INTEGER       TLINENUM      ! Temporary var,# lines in tfile #
      CHARACTER*180 TLINET        ! Temporary line from T-file     text
      CHARACTER*180 TLINETMP      ! Temporary line                 #
      INTEGER       TLPOS         ! Position on temporary line     #
!     REAL          TMAX          ! Temperature maximum            C
      REAL          TMAXAV(0:9)   ! Average maximum temp in phase  C
      REAL          TMAXM         ! Temperature maximum,monthly av C
      REAL          TMAXSUM       ! Temperature maximum,summed     C
      REAL          TMAXSUMP      ! Tmax sum in phase              oC 
      REAL          TMAXSUMP0     ! Tmax sum for growth cycle      oC
      REAL          TMAXX         ! Temperature max during season  C
      REAL          TMEAN         ! Temperature mean (TMAX+TMIN/2) C
      REAL          TMEAN20       ! Temperature mean over 20 days  C
      REAL          TMEAN20A      ! Temperature mean,20 d~anthesis C
      REAL          TMEAN20P      ! Temperature mean,20 d>planting C
      REAL          TMEAN20S      ! Temperature sum over 20 days   C
      REAL          TMEAND(20)    ! Temperature mean,specific day  C
      REAL          TMEANNUM      ! Temperature means in sum       #
      REAL          TMEANS        ! Temperature mean under snow    C
!     REAL          TMIN          ! Temperature minimum            C
      REAL          TMINAV(0:9)   ! Average minimum temp in phase  C
      REAL          TMINM         ! Temperature minimum,monthly av C
      REAL          TMINN         ! Temperature min during season  C
      REAL          TMINSUM       ! Temperature minimum,summed     C
      REAL          TMINSUMP      ! Tmin sum in phase              oC
      REAL          TMINSUMP0     ! Tmin sum for growth cycle      oC
      REAL          TMNC          ! Tmin cumulative                oC 
!     INTEGER       TN            ! Treatment number               #
      CHARACTER*25  TNAME         ! Treatment name                 text
      REAL          TNAMM         ! Total N at maturity,measured   kg/ha
      CHARACTER*10  TNCHAR        ! Treatment number,characters    text
      REAL          TNDEM         ! Plant tops demand for N        g/p
      REAL          TNDEMSOIL     ! Plant tops demand for N,soil   g/p
      INTEGER       TNI           ! Treatment number,initial value #
      REAL          TNIGHT        ! Temperature during night hours C
      REAL          TNUM          ! Tiller (incl.main stem) number #/p
      REAL          TNUMAD        ! Tiller (incl.main stem) number #/m2
      REAL          TNUMAEAA      ! Shoot #/area abs error avg     #
      REAL          TNUMAEAV      ! Shoot #/area average error     #
      REAL          TNUMAERR      ! Shoot #,error                  %
      REAL          TNUMAFAC      ! Tiller # Aassimilates factor   #/p
      REAL          TNUMAM        ! Shoot #,maturity               #/m2
      REAL          TNUMAMM       ! Shoot #,mature,measured        #/m2
      INTEGER       TNUMANUM      ! Shoot #/area error #           #
      REAL          TNUMASUA      ! Shoot #/area abs error #       #
      REAL          TNUMASUM      ! Shoot #/area error sum         #
      REAL          TNUMD         ! Tiller number death            #/p
      REAL          TNUMG         ! Tiller number growth           #/p
      REAL          TNUMIFF       ! Tiller number fibonacci factor #
      REAL          TNUML(LNUMX)  ! Tiller # at leaf position      #/p
      REAL          TNUMLOSS      ! Tillers lost through death     #/p
      INTEGER       TNUMOUT       ! Tiller # warning op counter    #
      REAL          TNUMT         ! Shoot number from t file       #/m2
!     REAL          TOTIR         ! Total irrigation during cycle  mm 
      REAL          TRDV1(4)      ! Temp response,development 1    #
      REAL          TRDV2(4)      ! Temp response,development 2    #
      REAL          TRGEM(4)      ! Temp response,germ.emergence   #
      REAL          TRGFN(4)      ! Temp response,grain fill,N     #
      REAL          TRGFW(4)      ! Temp response,grain fill,d.wt. #
      REAL          TRLDF         ! Intermediate factor,new roots  #
      REAL          TRLFG(4)      ! Temp response,leaf growth      #
      REAL          TRLTH(4)      ! Temp response,lethal tempature #
      REAL          TRLV          ! Total root length density      cm-2
      REAL          TRNU          ! Total N uptake,csm apprach     kg/ha
      REAL          TRPHS(4)      ! Temp response,photosynthesis   #
      CHARACTER*40  TRUNNAME      ! Treatment+run composite name   text
      REAL          TRVRN(4)      ! Temp response,vernalization    #
!     REAL          TRWUP         ! Total water uptake,potential   cm
      INTEGER       TSDAP         ! Terminal spkelet date          #
      INTEGER       TSDAPM        ! Terminal spkelet date,measured #
      INTEGER       TSDAT         ! Terminal spkelet date          #
      INTEGER       TSDATM        ! Terminal spkelet date,measured #
      REAL          TSDEP         ! Average temp in top 10 cm soil C
      REAL          TT            ! Daily thermal time             C.d
      REAL          TTMAX         ! Daily thermal time,maximum     C.d
      REAL          TTGEM         ! Daily thermal time,germ,emrg.  C.d
      REAL          TT20          ! Thermal time mean over 20 days C
      REAL          TT20S         ! Thermal time sum over 20 days  C
      REAL          TTD(20)       ! Thermal time,specific day      C
      REAL          TTNUM         ! Thermal time means in sum      #
      REAL          TTOUT         ! Thermal units output from func C.d
      REAL          TTTMP         ! Thermal time,temporary value   C
      INTEGER       TVI1          ! Temporary integer variable     #
      INTEGER       TVI2          ! Temporary integer variable     #
      INTEGER       TVI3          ! Temporary integer variable     #
      INTEGER       TVI4          ! Temporary integer variable     #
!     INTEGER       TVICOLNM      ! Column number function output  #
!     INTEGER       TVILENT       ! Temporary integer,function op  #
      REAL          TVR1          ! Temporary real variable        #
      REAL          TVR2          ! Temporary real variable        #
      REAL          TVR3          ! Temporary real variable        #
      REAL          TVR4          ! Temporary real variable        #
      REAL          TVR5          ! Temporary real variable        #
!     REAL          UH2O(NL)      ! Uptake of water                cm/d
      REAL          VALUE(SUMNUM) ! Value of variable passed       #
      INTEGER       VALUEI        ! Output from Getstri function   #
      REAL          VALUER        ! Output from Getstrr function   #
      REAL          VALUERR       ! Value of error                 #
      REAL          VANC          ! Vegetative actual N conc       #
      CHARACTER*6   VARNO         ! Variety identification code    text
      CHARACTER*6   VARNOP        ! Variety identification code    text
      REAL          VCNC          ! Vegetative critical N conc     #
      REAL          VDLOST        ! Vernalization lost (de-vern)   d
      INTEGER       VERSIONCSCER  ! Version #                      #
      REAL          VF            ! Vernalization factor 0-1       #
      REAL          VMNC          ! Vegetative minimum N conc      #
      REAL          VNAA          ! Vegetative N,anthesis          kg/ha
      REAL          VNAM          ! Vegetative N,mature            kg/ha
      REAL          VNAMM         ! Vegetative N,mature,measured   kg/ha
      REAL          VNPCM         ! Vegetative N %,maturity        %
      CHARACTER*6   VNPCMC        ! Vegetative N %,maturity        char
      REAL          VNPCMM        ! Vegetative N,mature,measure    %
      CHARACTER*6   VNPCMMC       ! Vegetative N,mature,measure    char
      CHARACTER*16  VRNAME        ! Variety name or identifier     text
      REAL          VWAM          ! Vegetative wt,mature           kg/ha
      REAL          VWAMEAA       ! Vegetative wt/area abs err avg #
      REAL          VWAMEAV       ! Vegetative wt/area average err #
      REAL          VWAMERR       ! Vegetative wt,error            %
      REAL          VWAMM         ! Veg wt,mature,measured         kg/ha
      INTEGER       VWAMNUM       ! Vegetative wt/area error #     #
      REAL          VWAMSUA       ! Vegetative wt/area abs error # #
      REAL          VWAMSUM       ! Vegetative wt/area error sum   #
      REAL          WAVR          ! Water available/demand         #
      REAL          WFG           ! Water factor,growth 0-1        #
      REAL          WFGAV(0:9)    ! Water factor,growth,average    #
      REAL          WFGC          ! Water factor,growth,cumulative #
      REAL          WFGC0         ! H2O factor,growth.cycle        #
      REAL          WFGE          ! Water factor,germ,emergence    #
      REAL          WFGEU         ! Water factor,GE,upper limit    #
      REAL          WFGU          ! Water factor,growth,upper      #
      REAL          WFLF(LNUMX)   ! H2O factor for leaf,average    #
      REAL          WFLFNUM(LNUMX)! H2O factor for leaf,# in sum   #
      REAL          WFLFSUM(LNUMX)! H2O factor for leaf,sum        #
      REAL          WFNL          ! Water content,N uptake,lower   #
      REAL          WFNU          ! Water content,N uptake,upper   #
      REAL          WFNUL         ! Water factor,N uptake,lower    #
      REAL          WFNUU         ! Water factor,N uptake,upper    #
      REAL          WFP           ! Water factor,photosynthsis 0-1 #
      REAL          WFPAV(0:9)    ! Water factor,phs,average 0-1   #
      REAL          WFPC          ! Water factor,phs,cumulative    #
      REAL          WFPC0         ! H2O factor,photosynthsis,cycle #
      REAL          WFPGF         ! Water factor,phs,grain filling #
      REAL          WFPU          ! Water factor,phs,upper         #
      REAL          WFRG          ! Water factor,root growth 0-1   #
      REAL          WFRGU         ! Water factor,root gr,up.bound  #
      REAL          WFSU          ! Water fact,senescence,up.bound #
      REAL          WFT           ! Water factor,tillering 0-1     #
      REAL          WFTL          ! Water factor,tillering,lower   #
      REAL          WFTU          ! Water factor,tillering,upper   #
      CHARACTER*10  WHSTGNAM(20)  ! Wheat stage names              text
!     REAL          WINDSP        ! Wind speed                     km/d
      REAL          WUPR          ! Water pot.uptake/demand        #
      REAL          XDEP          ! Depth to bottom of layer       cm
      REAL          XDEPL         ! Depth to top of layer          cm
      REAL          XMIN          ! Minimum NO3,NH4-N for uptake   mg/Mg
      REAL          XNFS          ! N labile fraction,standard     #
      REAL          XSTAGE        ! Stage of development           #
      REAL          XSTAGEFS      ! Xstage when final sen started  #
      REAL          XSTAGEP       ! Stage of development,previous  #
!     INTEGER       YEAR          ! Year                           #
      INTEGER       YEARCOL       ! Colum number for year data     #
      INTEGER       YEARDOY       ! Year+Doy (7digits)             #
      INTEGER       YEARHARF      ! Harvest year+doy,fixed         #
      INTEGER       YEARPLT       ! Year(or Yr)+Doy,planting date  #
      INTEGER       YEARPLTP      ! Year(or Yr)+Doy,planting trget #
      INTEGER       YEARSIM       ! Year+Doy for simulation start  #
      INTEGER       YRHARF        ! Harvest date,fixed             #
      CHARACTER*1   YRHARFF       ! Year+Doy flag for message      text
!     INTEGER       YEARPLTCSM    ! Year+Doy,planting date,CSM     #
      REAL          YSTAGE        ! Previous Zadoks calc variable  # 
!     REAL          YVAL1         ! Output from array function     #
!     REAL          YVALXY        ! Y value from function          #
      REAL          ZS2           ! Previous Zadoks calc variable  #
      REAL          ZS3           ! Previous Zadoks calc variable  #
      REAL          ZS4           ! Previous Zadoks calc variable  # 
      REAL          ZSTAGE        ! Zadoks stage of development    #
      REAL          ZSTAGEP       ! Zadoks stage,previous day      #
 
!      Variables expressed on a per hectare basis
!      Inputs
      REAL          AMTNIT        ! Cumulative amount of N applied kg/ha
      REAL          ANFER(200)    ! N amount in fertilizer appln   kg/ha
!      Outputs
      REAL          CHRSWAD       ! Chaff reserves                 kg/ha
      REAL          CNAA          ! Canopy N at anthesis           kg/ha
      REAL          CWAA          ! Canopy weight at anthesis      kg/ha
      REAL          CARBOA        ! Carbohydrate assimilated       kg/ha
      REAL          CARBOAC       ! Carbohydrate assimilated,cum   kg/ha
      REAL          CHWAD         ! Chaff weight                   kg/ha
      REAL          CHWADOUT      ! Chaff+Ch.reserves weight       kg/ha
      REAL          CNAD          ! Canopy nitrogen                kg/ha
      REAL          CNADSTG(20)   ! Canopy nitrogen,specific stage kg/ha
      REAL          CWAD          ! Canopy weight                  kg/ha
      REAL          CWADSTG(20)   ! Canopy weight,particular stage kg/ha
      REAL          DNAD          ! Dead N retained on plant       kg/ha
      REAL          DWAD          ! Dead weight retained on plant  kg/ha
      REAL          EWAD          ! Ear weight                     kg/ha
      REAL          GNAD          ! Grain N                        kg/ha
      REAL          GWAD          ! Grain weight                   kg/ha
      REAL          LLNAD         ! Leaf lamina nitrogen           kg/ha
      REAL          LLWAD         ! Leaf lamina weight             kg/ha
      REAL          LLWADOUT      ! Leaf lamina weight for output  kg/ha
      REAL          LSHWAD        ! Leaf sheath weight             kg/ha
      REAL          LSHWADOUT     ! Leaf sheath weight for output  kg/ha
      REAL          LSHRSWAD      ! Leaf sheath reserves weight    kg/ha
      REAL          RESCAL(0:20)  ! Residue C at harvest,by layer  kg/ha
!     REAL          RESCALG(0:20) ! Residue C added,by layer       kg/ha
!     REAL          RESLGAL(0:20) ! Residue lignin,harvest,bylayer kg/ha
!     REAL          RESLGALG(0:20)! Residue lignin added,layer     kg/ha
!     REAL          RESNAL(0:20)  ! Residue N at harvest by layer  kg/ha
!     REAL          RESNALG(0:20) ! Residue N added,by layer       kg/ha
      REAL          RESPAC        ! Respiration,total,cumulative   kg/ha
!     REAL          RESWAL(0:20)  ! Residue om added by layer      kg/ha
!     REAL          RESWALG(0:20) ! Residue om at harvest,by layer kg/ha
      REAL          RNAD          ! Root N                         kg/ha
      REAL          RSNAD         ! Reserve N                      kg/ha
      REAL          RSWAA         ! Reserves weight at anthesis    kg/ha
      REAL          RSWAD         ! Reserves weight                kg/ha
      REAL          RSWADPM       ! Reserves weight,post maturity  kg/ha
      REAL          RTWTAL(20)    ! Root weight by layer           kg/ha
      REAL          RWAD          ! Root weight                    kg/ha
      REAL          SDNAD         ! Seed N                         kg/ha
      REAL          SDRATE        ! Seeding 'rate'                 kg/ha
      REAL          SDWAD         ! Seed weight                    kg/ha
      REAL          SENCAL(0:20)  ! Senesced C,by layer            kg/ha
!     REAL          SENCALG(0:20) ! Senesced C added,by layer      kg/ha
      REAL          SENCAS        ! Senesced C added to soil       kg/ha
      REAL          SENLGAL(0:20) ! Senesced lignin added,by layer kg/ha
!     REAL          SENLGALG(0:20)! Senesced lignin added,layer    kg/ha
      REAL          SENLGAS       ! Senesced lignin added to soil  kg/ha
      CHARACTER*6   SENN0C        ! Senesced N added to litter     kg/ha
      REAL          SENNAL(0:20)  ! Senesced N,by layer            kg/ha
!     REAL          SENNALG(0:20) ! Senesced N added,by layer      kg/ha
      REAL          SENNAS        ! Senesced N added to soil       kg/ha
      CHARACTER*6   SENNSC        ! Senesced N added to soil       kg/ha
      CHARACTER*6   SENWSC        ! Senesced Wt,soil,output char   kg/ha
      REAL          SENWAL(0:20)  ! Senesced om added by layer     kg/ha
      REAL          SENWALG(0:20) ! Senesced om added by layer     kg/ha
      REAL          SENWAS        ! Senesced weight,soil           kg/ha
      CHARACTER*6   SENW0C        ! Senesced wt,to litter,cum      kg/ha
      REAL          SNAD          ! Stem N (stem+sheath+rs)        kg/ha
      REAL          STWAD         ! Stem (actal) weight            kg/ha
      REAL          STWADOUT      ! Stem+St.reserves weight        kg/ha
      REAL          STLSRWAD      ! Stem wt (stem+sheath+reserves) kg/ha
      REAL          STRSWAD       ! Stem reserves                  kg/ha
      REAL          TBAM          ! Temporary temp base anth-mat   C    
      REAL          TNAD          ! Total nitrogen (tops+roots)    kg/ha
      REAL          TWAD          ! Total weight (tops+roots)      kg/ha
!     REAL          UNH4ALG(20)   ! Uptake of NH4 N                kg/ha
!     REAL          UNO3ALG(20)   ! Uptake of NO3 N                kg/ha
      REAL          VEFF          ! Vernalization effect,max.reduc fr
      REAL          VNAD          ! Vegetative canopy nitrogen     kg/ha
      REAL          VWAD          ! Vegetative canopy weight       kg/ha
      REAL          VRNSTAGE      ! Vernalization stage            #
      
!!     2021-02-14 chp
!      REAL          Nuptake_daily !Daily N uptake (kg [N]/ha)
!      REAL          NUAD_Y        !Yesterdays cumulative N uptake

!      Condition at end of phase
      DATA BASTGNAM/'Max Prim  ','End Veg   ','End Ear Gr',
     1              'Bg Gr Fill','End Gr Fil','Harvest   ',
     2              'Sowing    ','Germinate ','Emergence ',
     3              'Failure   ','End Crop  ','          ',
     4              '          ','          ','          ',
     5              '          ','          ','          ',
     6              '          ','          '/

      DATA WHSTGNAM/'Term Spklt','End Veg   ','End Ear Gr',
     1              'Beg Gr Fil','End Gr Fil','Harvest   ',
     2              'Sowing    ','Germinate ','Emergence ',
     3              'Failure   ','End crop  ','          ',
     4              '          ','          ','          ',
     5              '          ','          ','          ',
     6              '          ','          '/


    
        End Module CER_First_Trans_m
        
        
      subroutine clear_CER_First_Trans_m()
          USE CER_First_Trans_m
          implicit none
            
            
        ISTAGE = 7   ! Pre-planting
        XSTAGE = 7.0 ! Pre-planting
        YEARPLT = 9999999
        YEARPLTP = 9999999
        

        SEASENDOUT = 'N'  ! Season end outputs flag     
        
        adat = -99
!       adat10 = -99
        adatend = -99
        amtnit = 0.
        awnai = 0.0          ! awns not yet used
        canhtg = 0.0
        carboc = 0.0
        ccountv = 0
        cflfail = 'n'
        cflinit = 'n'
        ch2olim = 0
        chrs = 0.0
        chrswad = -99.0
        chrswt = -99.0
        chwt = 0.0
        cnaa = 0.0
        cnadstg = 0.0
        co2fp = 1.0
        cumdu = 0.0
        cumgeu = 0.0
        cumtt = 0.0
        cumtu = 0.0
        cumvd = 0.0
        cwaa = 0.0
        cwadstg = 0.0
        dapm = 0
        dayltp = 0.0
        daysc = 0
        deadn = 0.0
        deadwt = 0.0
        !dewdur = -99.0
        dleafn = 0.0
        drdat = -99
        drootn = 0.0
        dstemn = 0.0
        du = 0.0
        gedsum = 0.0
        gestage = 0.0
        getmean = 0.0
        getsum = 0.0
        gfdsum = 0.0
        gftmean = -99.0
        gftsum = 0.0
        gmdsum = 0.0
        gmtmean = -99.0
        gmtsum = 0.0
        gpla = 0.0
        grainanc = 0.0
        grainn = 0.0
        grainngl = 0.0
        grainngr = 0.0
        grainngs = 0.0
        grnum = 0.0
        grnumad = 0.0
        grogr = 0.0
        grogradj = 0.0
        grolf = 0.0
        grors = 0.0
        grorsgr = 0.0
        grorspm = 0.0
        grost = 0.0
        grwt = 0.0
        gwud = 0.0
        hardi = 0.0
        hiad = 0.0
        hind = 0.0
        iedat = -99
        icsdur = 0
        icsdur0 = 0
        istagep = 0
        jdat = -99
        lagstage = -99.0
        !lai = 0.0
        lail = 0.0
        laistg = 0.0
        laix = 0.0
        lanc = 0.0
        lap = 0.0
        lapp = 0.0
        laps = 0.0
        latl = 0.0
        lcnc = 0.0
        lcnf = 0.0
        leafn = 0.0
        lfwt = 0.0
        lldat = -99
        llrswad = -99.0
        llrswt = -99.0
        lnpca = 0.0
        lnumsd = 0.0
        lnumsg = 1
        lnumstg = 0.0
        lshrswad = -99.0
        lshrswt = -99.0
        nfg = 1.0
        nfgav = 1.0
        nfgc = 0.0
        nfgc0 = 0.0
        !nfp = 1.0
        nfpav = 1.0
        nfpc = 0.0
        nfpc0 = 0.0
        nft = 1.0
        nlimit = 0
        nupc = 0.0
        nupd = 0.0
        nupr = 0.0
        paradcum = 0.0               
        paradicum = 0.0               
        pari = 0.0
        parip = -99.0
        parued = 0.0
        pdadj = -99.0
        pla = 0.0
        plagt = 0.0
        plast = 0.0
        ptf = 0.0
        rainc = 0.0
        rainca = 0.0
        ranc = 0.0
        rcnc = 0.0
        rcnf = 0.0
        rescal = 0.0
        !rescalg = 0.0
        !reslgal = 0.0
        !reslgalg = 0.0
        !resnal = 0.0
        !resnalg = 0.0
        respc = 0.0
        !reswal = 0.0
        !reswalg = 0.0
        !rlv = 0.0
        rmnc = 0.0
        rootn = 0.0
        rootns = 0.0
        rscd = 0.0
        rsca = 0.0
        rsfp = 1.0
        rsn = 0.0
        rsnuseg = 0.0
        rsnuser = 0.0
        rsnuset = 0.0
        rstage = 0.0
        rstagep = 0.0
        rswt = 0.0
        rswtpm = 0.0
        rtdep = 0.0
        rtdepg = 0.0
        rtslxdate = -99
        rtwt = 0.0
        rtwtl = 0.0
        rtwts = 0.0
        said = 0.0
        sanc = 0.0
        scnc = 0.0
        scnf = 0.0
        sdnc = 0.0
        seednr = 0.0
        seednt = 0.0
        sencal = 0.0
        sencl = 0.0
        sencs = 0.0
        senla = 0.0
        senlaretained = 0.0
        senlalitter = 0.0
        senlfg = 0.0
        senlfgrs = 0.0
        senrs = 0.0
        senrsc = 0.0
        senlgal = 0.0
        senlgl = 0.0
        senlgs = 0.0
        sennal = 0.0
        sennl = 0.0
        sennlfg = 0.0
        sennlfgrs = 0.0
        senns = 0.0
        sennstg = 0.0
        sennstgrs = 0.0
        senstg = 0.0
        senwal = 0.0
        senwl = 0.0
        senws = 0.0
        !snow = 0.0
        srad20a = -99.0
        sradc = 0.0
        sradd = 0.0
        stemn = 0.0
        stress = 0.0
        strswad = -99.0
        strswt = -99.0
        stwt = 0.0
        tfg = 1.0
        tfp = 1.0
        tlimit = 0
        tmaxm = -99.0
        tmean20a = -99.0
        tmeand = 0.0
        tmeannum = 0.0
        tminm = 999.0
        tnum = 0.0
        !trwup = 0.0
        tsdat = -99
        tt = 0.0
        tt20 = -99.0
        ttd = 0.0
        ttnum = 0.0
        !uh2o = 0.0
        vanc = 0.0
        vcnc = 0.0
        vf = 1.0
        vrnstage = 0.0
        vmnc = 0.0
        vnad = 0.0
        vwad = 0.0
        wfg = 1.0
        wfgav = 1.0
        wfgc = 0.0
        wfgc0 = 0.0
        wfp = 1.0
        wfpav = 1.0
        wfpc = 0.0
        wfpc0 = 0.0
        wft = 1.0
        wupr = 1.0
        zstage = 0.0
        zstagep = 0.0
        daylsum = 0.0 
        daylsum0 = 0.0
        epsum = 0.0 
        epsum0 = 0.0 
        etsum = 0.0 
        etsum0 = 0.0 
        laswitch = -99.0
        lnswitch = -99.0
        rainsum = 0.0 
        rainsum0 = 0.0
        sradsum = 0.0 
        sradsum0 = 0.0
        co2sum = 0.0
        co2sum0 = 0.0
        tmaxsump = 0.0 
        tmaxsump0 = 0.0
        tminsump = 0.0 
        tminsump0 = 0.0
        xstagefs = 0.0
        gplasenf = 0.0


      end subroutine clear_CER_First_Trans_m
        