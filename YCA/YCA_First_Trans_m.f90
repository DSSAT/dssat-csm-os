Module YCA_First_Trans_m
    USE YCA_Node        ! DA 17MAR2017
    
    
    !CHARACTER(LEN=1),PARAMETER::SLASH = '\'                                     ! (From SeasInit) (In ModuleDefs) 
    CHARACTER(LEN=1),PARAMETER::BLANK = ' '                                     ! (From SeasInit)                
    CHARACTER(LEN=3),PARAMETER::DASH = ' - '                                    ! (From Output)
    
    
    !INTEGER,PARAMETER::NL   =  20 ! Maximum number of soil layers               ! (In ModuleDefs) 
    INTEGER,PARAMETER::DINX =   3 ! Disease number,maximum
    INTEGER,PARAMETER::PSX  =  10 ! Principal stages,maximum !LPM 07MAR15 change to a maximum value of branching levels of 10 
    INTEGER,PARAMETER::SSX  =  20 ! Secondary stages,maximum
    INTEGER,PARAMETER::KEYSTX =10 ! Maximum number of key stages
    INTEGER,PARAMETER::DCNX =  10 ! Disease control #,maximum
    !INTEGER,PARAMETER::LCNUMX=500 ! Maximum number of leaf cohorts
    INTEGER,PARAMETER::LCNUMX=500 ! Maximum number of leaf cohorts LPM 23JUL15 using LNUMX to avoid errors for late or non branching varieties
    INTEGER,PARAMETER::NGMAX=30  ! Maximum number of groups of nodes
    REAL,PARAMETER::LLIFGD = 10.0 ! Leaf growing duration in days
    INTEGER,PARAMETER::LNUMX= 500 ! Maximum number of leaves/axis
    INTEGER,PARAMETER::HANUMX= 40 ! Maximum # harvest instructions
    REAL, PARAMETER:: ZERO = 1.0E-5 ! The "first" real number after zero (0) #MathUtils


    TYPE (Node_type),DIMENSION(0:PSX,0:LCNUMX)  :: plant
    
    REAL    :: AH2OPROFILE             ! Available H2o,profile          mm         ! (From Growth)    
    REAL    :: AH2OROOTZONE            ! Available h2o in root zone     mm         ! (From Growth)    
    REAL    :: ALBEDO                  ! Canopy+soil albedo             fr         ! (From SeasInit)  
    REAL    :: AMTNIT                  ! Cumulative amount of N applied kg/ha      ! (From SeasInit)  
    REAL    :: ANDEM                   ! Crop N demand                  kg/ha      ! (From SeasInit)  
    REAL    :: ANFER(200)              ! N amount in fertilizer appln   kg/ha      ! (From SeasInit)  
    REAL    :: AREAPOSSIBLE            ! Leaf area growth at SLA limit  cm2        ! (From Growth)    
    REAL    :: AREAPOSSIBLEN           ! Leaf area growth at N limit    cm2        ! (From Growth)    
    INTEGER :: ARGLEN                  ! Argument component length      #          ! (From RunInit)   
    REAL    :: AVGSW                   ! Average soil water in SWPLTD   %          ! (From Growth)    
    REAL    :: b_slope_lsize           ! Slope to define  max leaf size #          ! LPM 28feb15 
    REAL    :: BASELAYER               ! Depth at base of layer         cm         ! (From Integrate) 
    REAL    :: Bcount                  ! counters for iterations in branches (Bcount)
    INTEGER :: BR                      ! Index for branch number/cohorts#          ! (From SeasInit)  
    INTEGER :: BRDAE(PSX)              ! DAE when a new branch appears  d          ! LPM 11APR15 To save the date of branch appearance
    REAL    :: BRFX(0:PSX)             ! Branch # per fork at each fork #          ! (From SeasInit)  
    REAL    :: BRNUMSH(0:PSX)          ! Branch number/shoot at harvest #          ! (From Integrate) !LPM 28MAR15 to have the apex number by branch level 
    REAL    :: BRNUMSHM                ! Branch #/shoot,harvest,measurd #          ! (From Output)    
    REAL    :: BRNUMST(0:PSX)          ! Branch number/shoot (>forking) #          ! (From RunInit) !LPM 23MAR15 to have the apex number by branch level   
    REAL    :: BRNUMSTPREV(0:PSX)      ! Branch number/shoot,previous   #          ! (From Output)  !LPM 23MAR15 to have the apex number by branch level      
    INTEGER :: BROLDESTA               ! Br with Leaf  ,oldest active   #          ! (From Integrate)
    REAL    :: BRSTAGEPREV             ! Branching stage,previous       #          ! (From SeasInit)  
    REAL    :: BRSTAGETMP              ! Branching stage                #          ! (From Growth)    
    REAL    :: CANHTG                  ! Canopy height growth           cm         ! (From SeasInit)  
    REAL    :: CARBOADJ                ! Ch2o adjustment for LAI change g/p        ! (From SeasInit)  
    REAL    :: CARBOBEG                ! Ch2o available,beginning day   g/p        ! (From SeasInit)  
    REAL    :: CARBOBEGI               ! Ch2o avail,internal co2 calc   g/p        ! (From SeasInit)  
    REAL    :: CARBOBEGIA              ! Ch2o avail,internal co2,adj    #          ! (From Growth)    
    REAL    :: CARBOBEGM               ! Ch2o avail,Monteith ccalc.     g/p        ! (From SeasInit)  
    REAL    :: CARBOBEGR               ! Ch2o avail,PARUE calculation   g/p        ! (From SeasInit)  
    REAL    :: CARBOC                  ! Ch2o assimilated,cumulative    g/p        ! (From SeasInit)  
    REAL    :: CARBOEND                ! Ch2o available,end of day      g/p        ! (From SeasInit)  
    REAL    :: CARBOR                  ! Ch2o available,roots           g/p        ! (From SeasInit)  
    REAL    :: CARBOT                  ! Ch2o available,tops            g/p        ! (From SeasInit)  
    INTEGER :: CDAYS                   ! Crop cycle duration            PVoCd      ! (From SeasInit)     
    REAL    :: CNAD                    ! Canopy nitrogen                kg/ha      ! (From RunInit)   
    REAL    :: CNADPREV                ! Canopy nitrogen,previous day   kg/ha      ! (From RunInit)   
    REAL    :: CNADSTG(20)             ! Canopy nitrogen,specific stage kg/ha      ! (From RunInit)   
    REAL    :: CNAM                    ! Canopy N at harvest            kg/ha      ! (From SeasInit)  
    REAL    :: CNAMERR                 ! Canopy N,harvest,error         %          ! (From Output)    
    REAL    :: CNAMM                   ! Canopy N,harvest,measured      kg/ha      ! (From SeasInit)  
    REAL    :: CNCTMP                  ! Canopy N concentration,tempry  %          ! (From Output)    
    INTEGER :: CNI                     ! Crop component,initial value   #          ! (From SeasInit)  
    REAL    :: CNPCHM                  ! Canopy N,harvest,measured      %          ! (From Output)    
    REAL    :: CO2AIR                  ! CO2 concentration in air       g/m3       ! (From Growth)    
    REAL    :: CO2CAV                  ! Average co2 for crop cycle     vpm        ! (From Integrate) 
    REAL    :: CO2CC                   ! CO2 sum for cycle              vpm        ! (From SeasInit)  
    REAL    :: CO2COMPC                ! CO2 compensation conc (vpm)    #          ! (From SeasInit)  
    REAL    :: CO2EX                   ! Exponent for CO2-phs function  #          ! (From SeasInit)  
    REAL    :: CO2F(10)                ! CO2 factor rel values 0-2      #          ! (From SeasInit)  
    REAL    :: CO2FP                   ! CO2 factor,photosynthesis      #          ! (From SeasInit)  
    REAL    :: CO2FPI                  ! CO2 factor,phs,internal Co2    #          ! (From Growth)    
    REAL    :: CO2MAX                  ! CO2 conc,maximum during cycle  vpm        ! (From SeasInit)  
    REAL    :: CO2PAV(0:12)            ! CO2 concentration in air       g/m3       ! (From SeasInit)  
    REAL    :: CO2PC                   ! CO2 concentration,tier,cumul   ppm        ! (From SeasInit)  
    REAL    :: CO2RF(10)               ! CO2 reference concentration    vpm        ! (From SeasInit)  
    INTEGER :: COLNUM                  ! Column number                  #          ! (From Integrate)   
    !REAL    :: CRRSWAD                 ! Plant. stick reserves               kg/ha ! (From Integrate) !LPM 21MAY2015 The reserves distribution will not be included, it needs to be reviewed 
    !REAL    :: CRRSWT                  ! Plant. stick reserves               g/p   ! (From SeasInit) 
    !REAL    :: CRWAD                   ! Crown weight                        kg/ha ! (From SeasInit)         ! DA not used in Cassava
    !REAL    :: CRWADOUT                ! Crown weight for output             kg/ha ! (From Output)           ! DA not used in Cassava
    REAL    :: CRWT                    ! Plant. stick weight                 g/p   ! (From SeasInit)  
    REAL    :: CRWTM                   ! Plant. stick weight at maturity     g/p   ! (From Integrate) 
    REAL    :: CRWTP                   ! Plant. stick weight potential       g/p   ! (From SeasInit)  !LPM 23MAY2015 Added to keep the potential planting stick weight
    INTEGER :: CTRNUMPD                ! Control # missing tiers        #          ! (From SeasInit)  
    REAL    :: CUMDEP                  ! Cumulative depth               cm         ! (From Growth)    
    REAL    :: CUMDU                   ! Cumulative development units   #          ! (From SeasInit)  
    REAL    :: CUMSW                   ! Soil water in depth SWPLTD     cm         ! (From Growth)    
    REAL    :: CWAD                    ! Canopy weight                  kg/ha      ! (From SeasInit)  
    INTEGER :: CWADCOL                 ! Column number for canopy wt    #          ! (From Output)    
    REAL    :: CWADPREV                ! Canopy weight,previous day     kg/ha      ! (From SeasInit)  
    REAL    :: CWADSTG(20)             ! Canopy weight,particular stage kg/ha      ! (From SeasInit)  
    REAL    :: CWADT                   ! Canopy weight from t file      kg/ha      ! (From Output)    
    REAL    :: CWAHC                   ! Canopy weight harvested,forage kg/ha      ! (From SeasInit)  
    REAL    :: CWAHCM                  ! Canopy wt harvested,forage,mes kg/ha      ! (From SeasInit)  
    REAL    :: CWAM                    ! Canopy weight,maturity         kg/ha      ! (From SeasInit)  
    REAL    :: CWAMERR                 ! Canopy weight,maturity,error   %          ! (From Output)    
    REAL    :: CWAMM                   ! Canopy wt,maturity,measured    kg/ha      ! (From SeasInit)  
    REAL    :: CWAN(HANUMX)            ! Canopy wt minimum after harvst kg/ha      ! (From SeasInit)
    REAL    :: DABR                    ! Cumulative dev. units stress   #          ! (From SeasInit) 
    INTEGER :: DAE                     ! Days after emergence           d          ! (From SeasInit)
    INTEGER :: DAG                     ! Days after germination         d          !  !LPM 10JUL2017 To consider root and stem development after germination and before emergence (planting stick below-ground)
    REAL    :: DAGERM                  ! Dev. age for germination       #          ! (From SeasInit)  !LPM 21MAR2015 DAGERM added to save develpomental age at germination (with stress)
    REAL    :: DALS                    ! Development Age leaf size (2)  C.d        ! (From SeasInit) !LPM 24APR2016 DALS added to save Dev. age for potential leaf size (with stress)
    INTEGER :: DALSMAX                 ! DAE with the max leaf size     d          ! LPM 28FEB15 
    INTEGER :: DAP                     ! Days after planting            d          ! (From SeasInit)  
    INTEGER :: DAS                     ! Days after start of simulation d          ! (From Output)    
    INTEGER :: DATE                    ! Date (Yr+Doy)                  #          ! (From Output)    
    INTEGER :: DATECOL                 ! Date column number             #          ! (From Output)    
    REAL    :: DAWWP                   ! Development Age Whole Plant    C.d        ! (From SeasInit) !LPM 06MAR2016 DAWWP added to save Development Age (with stress)
    REAL    :: DAYLCAV                 ! Daylength (6deg) av for cycle  h          ! (From Integrate) 
    REAL    :: DAYLCC                  ! Daylength,cycle sum            h.d        ! (From SeasInit)  
    REAL    :: DAYLPAV(0:12)           ! Daylength (6deg) av for tier   h          ! (From SeasInit)  
    REAL    :: DAYLPC                  ! Daylength (6deg),cumulative    h          ! (From SeasInit)  
    REAL    :: DAYLPREV                ! Daylength previous day         h          ! (From Output)    
    REAL    :: DAYLS(0:10)             ! Daylength sensitivity,tier     %/10h      ! (From SeasInit)  
    REAL    :: DAYLST(0:12)            ! Daylength (6deg) at stage      h          ! (From SeasInit)  
    REAL    :: DAYSUM                  ! Days accumulated in month      #          ! (From SeasInit)  
    INTEGER :: DCDAT(DCNX)             ! Disease control application    YrDoy      ! (From SeasInit)  
    REAL    :: DCDUR(DCNX)             ! Disease control duration       d          ! (From SeasInit)  
    REAL    :: DCFAC(DCNX)             ! Disease control gr factor 0-1  #          ! (From SeasInit)  
    INTEGER :: DCTAR(DCNX)             ! Disease control target         #          ! (From SeasInit)  
    REAL    :: DF                      ! Daylength factor 0-1           #          ! (From SeasInit)  
    REAL    :: DFNEXT                  ! Daylength factor,next tier     #          ! (From Growth)    
    REAL    :: DFOUT                   ! Daylength factor for output    #          ! (From SeasInit)  
    REAL    :: DFPE                    ! Development factor,pre-emerge  #          ! (From SeasInit)  
    INTEGER :: DIDAT(DINX)             ! Disease initiation date YrDoy  d          ! (From SeasInit)  
    INTEGER :: DIDOY(DINX)             ! Disease infestation doy        d          ! (From Output)    
    REAL    :: DIFFACR(DINX)           ! Dis favourability requirement  #          ! (From SeasInit)  
    REAL    :: DIGFAC(DINX)            ! Disease growth factor 0-1      #          ! (From SeasInit)  
    REAL    :: DLAYRTMP(20)            ! Depth of soil layers with root cm         ! (From Integrate) 
    REAL    :: DMP_EP                  ! Dry matter per unit EP         g/mm       ! (From Output)    
    REAL    :: DMP_ET                  ! Dry matter per unit ET         g/mm       ! (From Output)    
    REAL    :: DMP_Irr                 ! Dry matter per unit irrigation g/mm       ! (From Output)    
    REAL    :: DMP_NApp                ! Dry matter per unit N applied  kg/kg      ! (From Output)    
    REAL    :: DMP_NUpt                ! Dry matter per unit N taken uo kg/kg      ! (From Output)    
    REAL    :: DMP_Rain                ! Dry matter per unit water      g/mm       ! (From Output)    
    INTEGER :: DOM                     ! Day of month                   #          ! (From Integrate) 
    INTEGER :: DOYCOL                  ! Day of year column number      #          ! (From Output)    
    REAL    :: DRAINC                  ! Drainage from profile,cumulat  mm         ! (From SeasInit)  
    !REAL    :: DSTAGE                  ! Development stage,linear       #          ! (From SeasInit)  !LPM 05JUN2015 DSTAGE is not used
    REAL    :: DTRY                    ! Effective depth of soil layer  cm         ! (From Growth)    
    REAL    :: DU                      ! Developmental units            PVC.d      ! (From SeasInit)  
    REAL    :: DUNEED                  ! Developmental units needed tir PVC.d      ! (From SeasInit)  
    REAL    :: DUPHASE                 ! Development units,current tier PVoCd      ! (From Growth)    
    REAL    :: DUPNEXT                 ! Development units,next tier    PVoCd      ! (From Growth)    
    !REAL    :: DUSRI                   ! Degree days to st.root init.   oC.d       ! (From SeasInit)  !LPM 05JUN2015 DUSRI is not used
    REAL    :: DUTOMSTG                ! Developmental units,germ->mat  Du         ! (From SeasInit)  
    INTEGER :: DYNAMICPREV             ! Program control varbl,previous #          ! (From SeasInit)  
    REAL    :: EARLYN                  ! Leaf # for early N switch      #          ! (From Growth)    
    REAL    :: EARLYW                  ! Leaf # for early h20 switch    #          ! (From Growth)    
    INTEGER :: EDAP                    ! Emergence DAP                  d          ! (From SeasInit)  
    REAL    :: EDAPFR                  ! Emergence DAP+fraction         #          ! (From SeasInit)  
    INTEGER :: EDAPM                   ! Emergence DAP measured         #          ! (From SeasInit)  
    INTEGER :: EDATM                   ! Emergence date,measured (Afle) #          ! (From Output)    
    INTEGER :: EDATMX                  ! Emergence date,measured (YrDy) #          ! (From SeasInit)  
    REAL    :: EDAYFR                  ! Emergence day fraction         #          ! (From SeasInit)  
    INTEGER :: EMDATERR                ! Emergence date error d         #          ! (From Output)    
    REAL    :: EMRGFR                  ! Fraction of day > emergence    #          ! (From SeasInit)  
    REAL    :: EMRGFRPREV              ! Fraction of day > em,previous  #          ! (From SeasInit)  
    REAL    :: EOC                     ! Potential evap,cumulative      mm         ! (From SeasInit)  
    REAL    :: EOEBUD                  ! Potential evap,Ebudget         mm/d       ! (From SeasInit)  
    REAL    :: EOEBUDC                 ! Potential evap,Ebudget,cum     mm         ! (From SeasInit)  
    REAL    :: EOEBUDCRP               ! Potential evaporation,with res mm/d       ! (From SeasInit)  
    REAL    :: EOEBUDCRPC              ! Potential evaporation,res,cum  mm/d       ! (From SeasInit)  
    REAL    :: EOEBUDCRPCO2            ! Potential evaporation,res+co2  mm/d       ! (From SeasInit)  
    REAL    :: EOEBUDCRPCO2C           ! Potential ev,res+co2,cum       mm/d       ! (From SeasInit)  
    REAL    :: EOEBUDCRPCO2H2O         ! Potential evap,res+co2+h2o     mm/d       ! (From SeasInit)  
    REAL    :: EOEBUDCRPCO2H2OC        ! Pot.ev,res+co2+h2o,cum         mm/d       ! (From SeasInit)  
    REAL    :: EOMPCRP                 ! Potential ev,M-P with crop res mm/d       ! (From SeasInit)  
    REAL    :: EOMPCRPC                ! Potential ev,M-P with res,cum  mm/d       ! (From SeasInit)  
    REAL    :: EOMPCRPCO2              ! Potential ev,M-P,res+co2       mm/d       ! (From SeasInit)  
    REAL    :: EOMPCRPCO2C             ! Potential ev,M-P,res+co2,cum   mm/d       ! (From SeasInit)  
    REAL    :: EOMPCRPCO2H2O           ! Potential ev,M-P,res+co2+h2o   mm/d       ! (From SeasInit)  
    REAL    :: EOMPCRPCO2H2OC          ! Pot.ev,M-P,res+co2+h2o,cum     mm/d       ! (From SeasInit)  
    REAL    :: EOMPEN                  ! Potential ev,M-Penman          mm/d       ! (From SeasInit)  
    REAL    :: EOMPENC                 ! Potential ev,M-Penman,cum      mm         ! (From SeasInit)  
    REAL    :: EOPEN                   ! Potential evaporation,Penman   mm/d       ! (From SeasInit)  
    REAL    :: EOPENC                  ! Potential evaporation,Pen,cum  mm         ! (From SeasInit)  
    REAL    :: EOPT                    ! Potential evaporation,PT       mm/d       ! (From SeasInit)  
    REAL    :: EOPTC                   ! Potential evaporation,PT,cum   mm         ! (From SeasInit)  
    REAL    :: EPCC                    ! Transpiration cycle sum        mm         ! (From SeasInit)  
    REAL    :: EPPC(0:12)              ! Transpiration cycle sum        mm         ! (From Integrate) 
    REAL    :: EPSRATIO                ! Function,plant/soil evap rate  #          ! (From SeasInit)  
    REAL    :: ERRORVAL                ! Plgro-tfile values/Plgro       #          ! (From Output)    
    REAL    :: ETCC                    ! Evapotranspiration cumulative  mm         ! (From SeasInit)  
    REAL    :: ETPC(0:12)              ! Evapotranspiration tier sum    mm         ! (From Integrate) 
    INTEGER :: EVALOUT                 ! Evaluate output lines for exp  #          ! (From Output)    
    INTEGER :: EVHEADNM                ! Number of headings in ev file  #          ! (From Output)    
    INTEGER :: EVHEADNMMAX             ! Maximum no headings in ev file #          ! (From Output)    
    INTEGER :: EYEARDOY                ! Emergence Year+DOY             #          ! (From SeasInit)  
    REAL    :: FAC(20)                 ! Factor ((g/Mg)/(kg/ha))        #          ! (From Growth)    
    INTEGER :: FAPPNUM                 ! Fertilization application number          ! (From SeasInit)  
    INTEGER :: FDAY(200)               ! Dates of fertilizer appn       YrDoy      ! (From SeasInit)  
    REAL    :: FERNITPREV              ! Fertilizer N applied to ystday kg/ha      ! (From SeasInit)  
    INTEGER :: FILELEN                 ! Length of file name            #          ! (From SeasInit)  
    INTEGER :: FLDAP                   ! Final leaf date                Yrdoy      ! (From SeasInit)  
    REAL    :: FLN                     ! Final leaf #                   #          ! (From SeasInit)  
    REAL    :: FNH4                    ! Unitless ammonium supply index #          ! (From Growth)    
    REAL    :: FNO3                    ! Unitless nitrate supply index  #          ! (From Growth)    
    INTEGER :: FNUMERA                 ! File number,A-data errors      #          ! (From RunInit)   
    INTEGER :: FNUMERR                 ! File number,error file         #          ! (From RunInit)   
    INTEGER :: FNUMERT                 ! File number,T-data errors      #          ! (From RunInit)   
    INTEGER :: FNUMEVAL                ! Number used for evaluate op    #          ! (From RunInit)   
    INTEGER :: FNUMLVS                 ! File number,leaves             #          ! (From RunInit)   
    INTEGER :: FNUMMEAS                ! Number used for measured data  #          ! (From RunInit)   
    INTEGER :: FNUMOV                  ! Number used for overview op    #          ! (From RunInit)   
    INTEGER :: FNUMPHA                 ! File number,tiers              #          ! (From RunInit)   
    INTEGER :: FNUMPHEM                ! File number,phenology,measured #          ! (From RunInit)   
    INTEGER :: FNUMPHES                ! File number,phenology,simulate #          ! (From RunInit)   
    INTEGER :: FNUMPREM                ! File number,measured responses #          ! (From RunInit)   
    INTEGER :: FNUMPRES                ! File number,simulated response #          ! (From RunInit)   
    INTEGER :: FNUMPSUM                ! Number used for plant summary  #          ! (From RunInit)   
    INTEGER :: FNUMREA                 ! File number,reads.out file     #          ! (From RunInit)   
    INTEGER :: FNUMT                   ! Number used for T-file         #          ! (From RunInit)   
    INTEGER :: FNUMTMP                 ! File number,temporary file     #          ! (From RunInit)   
    INTEGER :: FNUMWRK                 ! File number,work file          #          ! (From RunInit)   
    INTEGER :: FROPADJ                 ! Frequency of outputs,adjusted  d          ! (From RunInit)   
    REAL    :: FSOILH2O                ! Final soil water               cm         ! (From Integrate) 
    REAL    :: FSOILN                  ! Final soil inorganic N         kg/ha      ! (From Integrate) 
    INTEGER :: GDAP                    ! Germination DAP                d          ! (From SeasInit)  
    REAL    :: GDAPFR                  ! Germination DAP+fr             #          ! (From Integrate) 
    INTEGER :: GDAPM                   ! Germination DAP,measured       #          ! (From SeasInit)  
    INTEGER :: GDATM                   ! Germination date,measured      #          ! (From Output)    
    REAL    :: GDAYFR                  ! Fraction of day to germination #          ! (From SeasInit)  
    REAL    :: GEDAYSE                 ! Period germination->emergence  d          ! (From SeasInit)  
    REAL    :: GEDAYSG                 ! Period planting->germination   d          ! (From SeasInit)  
    REAL    :: GERMFR                  ! Fraction of day > germination  #          ! (From SeasInit)  
    REAL    :: GESTAGE                 ! Germination,emergence stage    #          ! (From SeasInit)  
    REAL    :: GESTAGEPREV             ! Germ,emerg stage,previous day  #          ! (From SeasInit)  
    REAL    :: GEUCUM                  ! Cumulative germ+emergence unit #          ! (From SeasInit)  
    REAL    :: GROCR                   ! Plant. stick growth rate              g/p ! (From SeasInit)  
    REAL    :: GROCRADJ                ! Plant. stick growth rate N adjusted   g/p ! (From SeasInit)  
    REAL    :: GROCRFR                 ! Plant. stick growth rate,fraction st  #   ! (From Growth)    
    REAL    :: GROCRP                  ! Plant. stick growth rate potential    g/p ! (From SeasInit)
    REAL    :: GROLF                   ! Leaf growth rate               g/p        ! (From SeasInit)  
    REAL    :: GROLFADJ                ! Leaf growth rate N adjusted    g/p        ! (From SeasInit)  
    REAL    :: GROLFP                  ! Leaf growth,potential          g/p        ! (From Growth)    
    REAL    :: GROLS                   ! Leaf+stem growth               g/p        ! (From Growth)    
    REAL    :: GROLSA                  ! Leaf+stem gr from assimilates  g/p        ! (From Growth)    
    REAL    :: GROLSP                  ! Leaf+stem growth potential     g/p        ! (From Growth)    
    REAL    :: GROLSRS                 ! Leaf+stem growth from reserves g/p        ! (From Growth)    
    REAL    :: GROLSRT                 ! Leaf+stem gr from root dmatter g/p        ! (From Growth)    
    REAL    :: GROLSRTN                ! Leaf+stem N growth from root N g/p        ! (From Growth)    
    REAL    :: GROLSSD                 ! Leaf+stem gr from seed         g/p        ! (From Growth)    
    REAL    :: GROLSSEN                ! Leaf+stem growth from senesnce g/p        ! (From Growth) 
    REAL    :: GRORP                   ! Potential root growth          g/p        ! (From SeasInit) !LPM 22DEC2016 potential root growth
    REAL    :: GRORS                   ! Reserves growth                g/p        ! (From SeasInit)  
    !REAL    :: GROSR                   ! Storage root growth            g/p        ! (From SeasInit)  !LPM 05JUN2105 GROSR or basic growth of storage roots will not be used
    REAL    :: GROST                   ! Stem growth rate               g/p        ! (From SeasInit)  
    REAL    :: GROSTADJ                ! Stem growth rate N adjusted    g/p        ! (From SeasInit)  
    REAL    :: GROSTCR                 ! Stem+Plant. stick growth rate         g/p ! (From SeasInit)  
    REAL    :: GROSTCRP                ! Stem+Plant. stick growth potential    g/p ! (From Growth)    
    REAL    :: GROSTCRPSTORE           ! Stem+Plant. stick gr potentl,previous g/p ! (From Growth)    
    REAL    :: GROSTP                  ! Potential stem growth rate     g/p        ! LPM11APR15  
    REAL    :: GrP_EP                  ! Harvest product per unit EP    g/mm       ! (From Output)    
    REAL    :: GRP_ET                  ! Harvest product per unit water g/mm       ! (From Output)    
    REAL    :: GrP_Irr                 ! Harvest dm per unit irrigation g/mm       ! (From Output)    
    REAL    :: GrP_NApp                ! Harvest dm per unit N appllied kg/kg      ! (From Output)    
    REAL    :: GrP_NUpt                ! Harvest dm per unit N taken up kg/kg      ! (From Output)    
    REAL    :: GRP_Rain                ! Harvest product per unit water g/mm       ! (From Output)    
    INTEGER :: GSTDCOL                 ! Growth stage column number     #          ! (From Output)    
    INTEGER :: GYEARDOY                ! Germination Year+DOY           #          ! (From SeasInit)  
    REAL    :: H2OA                    ! Water available in root zone   mm         ! (From Integrate) 
    REAL    :: H2OCF                   ! Water 'potential' factor N upt mm         ! (From SeasInit)  
    REAL    :: H2OPROFILE              ! Total h2o in soil profile      mm         ! (From Growth)    
    REAL    :: H2OROOTZONE             ! Total h2o in root zone         mm         ! (From Growth)    
    INTEGER :: HADOY                   ! Harvest day of year            d          ! (From Integrate) 
    REAL    :: HAFR                    ! Harvested fraction             kg/ha      ! (From Growth)    
    REAL    :: HAMT(HANUMX)            ! Harvest amount                 #          ! (From SeasInit)  
    INTEGER :: HANUM                   ! Harvest instruction number     #          ! (From Growth)    
    REAL    :: HAWAD                   ! Harvested weight (grazing,etc) kg/ha      ! (From Growth)    
    INTEGER :: HAYEAR                  ! Harvest year                   #          ! (From Integrate) 
    REAL    :: HBPC(HANUMX)            ! Harvest by-product percentage  #          ! (From SeasInit)  
    REAL    :: HBPCF                   ! Harvest by-product %,final     #          ! (From SeasInit)  
    INTEGER :: HDAY                    ! Harvest day as read            #          ! (From SeasInit)  
    INTEGER :: HDOYF                   ! Earliest doy for harvest       #          ! (From SeasInit)  
    INTEGER :: HDOYL                   ! Last doy for harvest           #          ! (From SeasInit)  
    REAL    :: HDUR                    ! Hardening duration,days        d          ! (From SeasInit)  
    INTEGER :: HFIRST                  ! Earliest date for harvest      #          ! (From SeasInit)  
    REAL    :: HIAD                    ! Harvest index,above ground     #          ! (From SeasInit)  
    INTEGER :: HIADCOL                 ! Harvest index column number    #          ! (From Output)    
    REAL    :: HIADT                   ! Harvest index from t file      #          ! (From Output)    
    REAL    :: HIAM                    ! Harvest index,above ground,mat #          ! (From SeasInit)  
    REAL    :: HIAMERR                 ! Harvest index,maturity,error   %          ! (From Output)    
    REAL    :: HIAMM                   ! Harvest index,mature,measure   #          ! (From SeasInit)  
    REAL    :: HIAMMTMP                ! Harvest index,mature,temporary #          ! (From Output)    
    REAL    :: HIND                    ! Harvest index,N,above ground   #          ! (From SeasInit)  
    REAL    :: HINM                    ! Harvest index,N,abground,mat   #          ! (From SeasInit)  
    REAL    :: HINMM                   ! Harvest N index,mature,meas    %          ! (From SeasInit)  
    INTEGER :: HLAST                   ! Last date for harvest          #          ! (From SeasInit)  
    ! REAL    :: HMPC                    ! Harvest moisture percent,std.. #          ! (From SeasInit)  ! issue 49
    REAL    :: HNAD                    ! Product N                      kg/ha      ! (From SeasInit)  
    REAL    :: HNAM                    ! Harvest N at maturity          kg/ha      ! (From SeasInit)  
    REAL    :: HNAMERR                 ! Harvest N,error                %          ! (From Output)    
    REAL    :: HNAMM                   ! Harvest N,harvest,measured     kg/ha      ! (From SeasInit)  
    REAL    :: HNC                     ! Product N concentration,fr     #          ! (From Integrate) 
    REAL    :: HNPCM                   ! Harvest N%,maturity            %          ! (From SeasInit)  
    REAL    :: HNPCMERR                ! Harvest N%,error               %          ! (From Output)    
    REAL    :: HNPCMM                  ! Harvest N,mature,measured      %          ! (From SeasInit)  
    INTEGER :: HNUMACOL                ! Harvest number per area column #          ! (From Output)    
    ! REAL    :: HNUMAD                  ! Harvest product#/unit area     #/m2       ! (From Integrate) ! issue 50
    REAL    :: HNUMAERR                ! Harvest #,maturity,error       %          ! (From Output)    
    REAL    :: HNUMAM                  ! Harvest no/area,maturity       #/m2       ! (From SeasInit)  
    REAL    :: HNUMAMM                 ! Harvest no/area,mature,measure #/m2       ! (From SeasInit)  
    REAL    :: HNUMAT                  ! Harvest number/area,t file     #/m2       ! (From Output)    
    INTEGER :: HNUMBER                 ! Number of harvest instructions #          ! (From SeasInit)  
    INTEGER :: HNUMECOL                ! Harvest number per ear column  #          ! (From Output)    
    REAL    :: HNUMET                  ! Harvest number/ear,t file      #/s        ! (From Output)    
    REAL    :: HNUMGERR                ! Harvest #/group,error          %          ! (From Output)    
    REAL    :: HNUMGM                  ! Harvest #,mature               #/g        ! (From SeasInit)  
    REAL    :: HNUMGMM                 ! Harvest #,mature,measured      #/g        ! (From SeasInit)  
    REAL    :: HNUMPM                  ! Product# per plant,maturity    #/p        ! (From Integrate) 
    REAL    :: HNUMPMM                 ! Product #,mature,measured      #/p        ! (From Output)    
    REAL    :: HPC(HANUMX)             ! Harvest percentage             %          ! (From SeasInit)  
    REAL    :: HPCF                    ! Harvest percentage,final       %          ! (From SeasInit)  
    REAL    :: HPRODN                  ! Harvest product N              g/p        ! (From SeasInit)  
    REAL    :: HSTAGE                  ! Hardening stage  0-1           #          ! (From SeasInit)  
    INTEGER :: HSTG                    ! Harvest maturity stage         #          ! (From SeasInit)  
    REAL    :: HWAD                    ! Product weight                 kg/ha      ! (From Integrate) 
    INTEGER :: HWADCOL                 ! Product wt column number       #          ! (From Output)    
    REAL    :: HWADT                   ! Harvest weight from t file     kg/ha      ! (From Output)    
    REAL    :: HWAHERR                 ! Harvest wt,harvest,error       %          ! (From Output)    
    REAL    :: HWAHM                   ! Harvest wt,harvest,measured    kg/ha      ! (From Output)    
    REAL    :: HWAM                    ! Harvest product wt.,maturity   kg/ha      ! (From SeasInit)  
    REAL    :: HWAMM                   ! Harvest product wt.,measured   kg/ha      ! (From SeasInit)  
    INTEGER :: HWTUCOL                 ! Harvest weight per unit column #          ! (From Output)    
    ! REAL    :: HWUD                    ! Harvest wt/unit                g          ! (From Integrate) ! issue 50
    REAL    :: HWUM                    ! Harvest product size,maturity  g          ! (From SeasInit)  
    REAL    :: HWUMERR                 ! Harvest wt per unit error      %          ! (From Output)    
    REAL    :: HWUMM                   ! Hprod wt/unit,mat,measured     g          ! (From Output)    
    REAL    :: HWUMYLD                 ! Harvest wt,mature,calculated   g/#        ! (From Output)    
    REAL    :: HWUT                    ! Product weight/unit,t file     mg         ! (From Output)    
    REAL    :: HYAMM                   ! Harvest product,msured,std.h2o kg/ha      ! (From Output)    
    INTEGER :: HYEAR                   ! Harvest year as read           #          ! (From SeasInit)  
    INTEGER :: HYEARDOY(HANUMX)        ! Dates of harvest operations    #          ! (From SeasInit)  
    INTEGER :: HYEARF                  ! Earliest year for harvest      #          ! (From SeasInit)  
    INTEGER :: HYEARL                  ! Last year for harvest          #          ! (From SeasInit)  
    INTEGER :: HYRDOY(HANUMX)          ! Dates of harvest operations    #          ! (From SeasInit)  
    INTEGER :: I                       ! Loop counter                   #          ! (From SeasInit)  
    REAL    :: ICWD                    ! Initial water table depth      cm         ! (From SeasInit)  
    INTEGER :: IDETGNUM                ! Number of times into IDETG     #          ! (From SeasInit)  
    REAL    :: IRRAMTC                 ! Irrigation amount,cumulative   mm         ! (From SeasInit)  
    REAL    :: ISOILH2O                ! Initial soil water             cm         ! (From Growth)    
    REAL    :: ISOILN                  ! Initial soil inorganic N       kg/ha      ! (From Growth)    
    REAL    :: KCANI                   ! Extinction coeff,PAR,init.val. #          ! (From SeasInit)  
    REAL    :: KEPI                    ! Extinction coeff,SRAD,init val #          ! (From SeasInit)  
    INTEGER :: KEYPS(0:KEYSTX)         ! Principal key stage number     #          ! (From SeasInit) !LPM 28MAR15 Adjusted to start from 0 
    INTEGER :: KEYPSNUM                ! Principal key stage total #    #          ! (From SeasInit)  
    INTEGER :: L                       ! Loop counter                   #          ! (From SeasInit)  
    INTEGER :: L1                      ! Loop counter                   #          ! (From SeasInit)  
    INTEGER :: L2                      ! Loop counter                   #          ! (From Output)    
    REAL    :: LA1S                    ! Area of leaf 1,standard        cm2        ! (From SeasInit)  
    REAL    :: LAI                     ! Leaf area index                #          ! (From SeasInit)  
    REAL    :: LAIA                    ! Leaf area index,active         #          ! (From Output)    
    INTEGER :: LAIDCOL                 ! Leaf area index column         #          ! (From Output)    
    REAL    :: LAIPREV                 ! Leaf area index,previous day   #          ! (From SeasInit)  
    REAL    :: LAIPROD                 ! Leaf area index produced       #          ! (From Output)    
    REAL    :: LAISTG(0:20)            ! Leaf area index,specific stage #          ! (From SeasInit)  
    REAL    :: LAIX                    ! Leaf area index,maximum        #          ! (From SeasInit)  
    REAL    :: LAIXERR                 ! Leaf area index,max,error      %          ! (From Output)    
    REAL    :: LAIXM                   ! Lf lamina area index,mx,meas   m2/m2      ! (From SeasInit)  
    REAL    :: LAIXT                   ! Leaf area index,max,t-file     m2/m2      ! (From Output)    
    REAL    :: LAIXX                   ! Leaf area index,max posible    #          ! (From SeasInit)  
    REAL    :: LANC                    ! Leaf actual N concentration    #          ! (From SeasInit)  
    REAL    :: LANCRS                  ! Leaf N+rsN concentration       #          ! (From Growth)    
    REAL    :: LAPD                    ! Leaf area (green) per plant    cm2        ! (From Growth)    
    REAL    :: LAPH                    ! Leaf area (green) harvested    cm2/d      ! (From Growth)    
    REAL    :: LAPHC                   ! Leaf area (green) harvested,cu cm2/p      ! (From SeasInit)  
    REAL    :: LAPSTMP                 ! Leaf area senesced,temporary   cm2/p      ! (From Growth)    
    !REAL    :: LAWCF                   ! Leaf area/wt change,fr.st      fr/lf      ! (From SeasInit) !LPM 12DEC2016 Delete temperature, water and leaf position factors in SLA 
    !REAL    :: LAWFF                   ! Leaf area/wt flexibility,fr.st fr         ! (From SeasInit) !LPM 12DEC2016 Delete temperature, water and leaf position factors in SLA  
    REAL    :: LAWL(2)                 ! Area to wt ratio,n=youngest lf cm2/g      ! (From Growth)    
    !REAL    :: LAWMNFR                 ! Leaf area/wt ratio,min.fr.std. #          ! (From SeasInit) !LPM 12DEC2016 Delete temperature, water and leaf position factors in SLA   
    REAL    :: LAWS                    ! Leaf area/wt ratio,standard    cm2/g      ! (From SeasInit)  
    REAL    :: LAWTR                   ! Leaf area/weight,temp response fr/C       ! (From SeasInit)  
    REAL    :: LAWTS                   ! Leaf area/weight,temp standard C          ! (From SeasInit)  
    !REAL    :: LAXN2                   ! Leaf # (one axis),end max.area #          ! (From SeasInit)  
    !REAL    :: LAXNO                   ! Leaf # (one axis),maximum area #          ! (From SeasInit) !LPM 05JUN2016 LAXNO LAXN2 are not used 
    REAL    :: LAXS                    ! Area of biggest leaf,main stem cm2        ! (From SeasInit)  
    !INTEGER :: LCNUM                   ! Leaf cohort number (inc.grow)  #          ! (From SeasInit) !LPM 28MAR15 Non necessary variables  
    !REAL    :: LCOA(LCNUMX)            ! Leaf cohort area               cm2        ! (From SeasInit) !LPM 25MAR15 Non necessary variables  
    !REAL    :: LCOAS(LCNUMX)           ! Leaf cohort area senesced      cm2        ! (From SeasInit)  
    REAL    :: Lcount                   ! counter for iterations in leafs (Lcount)
    REAL    :: LEAFN                   ! Leaf N                         g/p        ! (From SeasInit)  
    REAL    :: LEAFNEXCESS             ! Leaf N > critical              g/p        ! (From Integrate) 
    INTEGER :: LENDIS                  ! Length,ISWDIS flag             #          ! (From SeasInit)  
    INTEGER :: LENENAME                ! Length,experiment description  #          ! (From Output)    
    INTEGER :: LENGROUP                ! Length of group name           #          ! (From Output)    
    INTEGER :: LENLINE                 ! Length of character string     #          ! (From Output)    
    INTEGER :: LENLINESTAR             ! Length of character string     #          ! (From Output)    
    INTEGER :: LENRNAME                ! Length of run description      #          ! (From SeasInit)  
    INTEGER :: LENTNAME                ! Length,treatment description   #          ! (From SeasInit)  
    INTEGER :: LF                      ! Loop counter leaves            #          !LPM 21MAR15 to add a leaf counter
    REAL    :: LFWT                    ! Leaf weight                    g/p        ! (From SeasInit)  
    REAL    :: LFWTM                   ! Leaf weight,maturity           g/p        ! (From SeasInit)  
    REAL    :: LLIFA                   ! Leaf life duration,active,read #          ! (From SeasInit)  
    REAL    :: LLIFATT                 ! Leaf life duration,active      C.d        ! (From SeasInit)  
    REAL    :: LLIFG                   ! Leaf growth duration,as read   #          ! (From SeasInit)  
    REAL    :: LLIFGTT                 ! Leaf growth duration           C.d        ! (From SeasInit)  
    REAL    :: LLIFS                   ! Leaf senescence dur,as read    d          ! (From SeasInit)  
    REAL    :: LLIFSTT                 ! Leaf senescence duration,Ttime C.d        ! (From SeasInit)  
    REAL    :: LLIFX                   ! Leaf ageing acceleration,max   Tu         ! (From SeasInit)  
    !REAL    :: LLIFXUnused             ! Leaf ageing accel.unused       Tu         ! (From Integrate) 
    REAL    :: LLIGP                   ! Leaf lignin percentage         #          ! (From SeasInit)  
    REAL    :: LLNAD                   ! Leaf lamina nitrogen           kg/ha      ! (From SeasInit)  
    REAL    :: LLOSA                   ! Leaf area loss,accelerated sen fr         ! (From SeasInit)  
    !REAL    :: LLRSWAD                 ! Leaf lamina reserves weight    kg/ha      ! (From SeasInit) !LPM 21MAY2015 The reserves distribution will not be included, it needs to be reviewed 
    !REAL    :: LLRSWT                  ! Leaf lamina reserves           g/p        ! (From SeasInit) !LPM 21MAY2015 The reserves distribution will not be included, it needs to be reviewed 
    REAL    :: LLWAD                   ! Leaf lamina weight             kg/ha      ! (From SeasInit)  
    REAL    :: LLWADOUT                ! Leaf lamina weight for output  kg/ha      ! (From Output)    
    REAL    :: LNCGL                   ! N concentration,growth,lower   fr         ! (From Growth)    
    REAL    :: LNCGU                   ! N concentration,growth,upper   fr         ! (From Growth)    
    REAL    :: LNCM                    ! Leaf N conc,minimum            fr         ! (From SeasInit)  
    REAL    :: LNCMN(0:1)              ! Leaf N conc,minimum            fr         ! (From SeasInit)  
    REAL    :: LNCPL                   ! Leaf N concentration,phs,lower fr         ! (From Growth)    
    REAL    :: LNCPU                   ! Leaf N concentration,phs,upper fr         ! (From Growth)    
    REAL    :: LNCR                    ! Leaf N relative to maximum     #          ! (From SeasInit)  
    REAL    :: LNCSEN                  ! N conc.for senescence          fr         ! (From Growth)    
    REAL    :: LNCX                    ! Leaf N conc,maximum            fr         ! (From SeasInit)  
    REAL    :: LNCXS(0:1)              ! Leaf N conc,maximum,stage      fr         ! (From SeasInit)  
    REAL    :: LNDEM                   ! Leaf demand for N              g/p        ! (From SeasInit)  
    REAL    :: LNDEMG                  ! Leaf demand for N,for growth   g/p        ! (From Growth)    
    REAL    :: LNDEMTU                 ! Leaf demand for N,for topup    g/p        ! (From Growth)    
    REAL    :: LNPCMN(0:1)             ! Leaf N conc,minimum            %          ! (From SeasInit)  
    REAL    :: LNPCS(0:1)              ! Leaf N conc,standard,stage     %          ! (From SeasInit)  
    REAL    :: LNPH                    ! Leaf N harvested               g/p        ! (From Growth)    
    REAL    :: LNPHC                   ! Leaf N harvested,cumulative    g/p        ! (From SeasInit)  
    REAL    :: LNSLP                   ! Slope for leaf production      #          ! LPM 08JUN2015 New cultivar coefficient 
    REAL    :: LNUM                    ! Leaf number,Haun stage         #          ! (From SeasInit)  
    INTEGER :: LNUMCOL                 ! Leaf number column             #          ! (From Output)    
    REAL    :: LNUMEND                 ! Leaf number,Haun stage,end day #          ! (From SeasInit)  
    REAL    :: LNUMG                   ! Leaf number increase per day   #          ! (From SeasInit)  
    REAL    :: LNUMNEED                ! Leaf # stage to start new leaf #          ! (From Growth)    
    REAL    :: LNUMPREV                ! Leaf number,Haun stage         #          ! (From SeasInit)  
    REAL    :: LNUMSERR                ! Leaf #,error                   %          ! (From Output)    
    INTEGER :: LNUMSG                  ! Leaf number produced on axis   #          ! (From SeasInit)  
    REAL    :: LNUMSIMSTG(0:PSX)       ! Leaf numbers by br level sim   #          ! LPM 21MAR15 to save the number of leaves by cohort
    REAL    :: LNUMSIMTOSTG(0:PSX)     ! Leaf numbers,fork,simul        #          ! (From Integrate) 
    REAL    :: LNUMSM                  ! Leaf #/shoot,Haun,maturity     #          ! (From SeasInit)  
    REAL    :: LNUMSMM                 ! Leaf #,mature,measured         #/s        ! (From SeasInit)  
    INTEGER :: LNUMSOLDESTA            ! Leaf number,oldest acive,axis  #          ! (From Integrate) 
    REAL    :: LNUMSTG(20)             ! Leaf number,specific stage     #          ! (From SeasInit)  
    REAL    :: LNUMT                   ! Leaf number from t file        #          ! (From Output)    
    REAL    :: LNUMTMP                 ! Leaf #,temporary val for calc  #          ! (From SeasInit)  
    REAL    :: LNUMTOSTG(0:PSX)        ! Leaf numbers at fork stages    #          ! (From SeasInit)  
    REAL    :: LNUSE(0:3)              ! Leaf N use,overall and parts   g          ! (From SeasInit)  
    REAL    :: LPEAI                   ! Leaf petiole area index        m2/m2      ! (From SeasInit)  
    REAL    :: LPEAW                   ! Leaf petiole area/wt,veg.ph    cm2/g      ! (From SeasInit)  
    REAL    :: LPEFR                   ! Leaf petiole fraction of total #          ! (From SeasInit)  
    !REAL    :: LPERSWAD                ! Leaf petiole reserves weight   kg/ha      ! (From SeasInit) !LPM 21MAY2015 The reserves distribution will not be included, it needs to be reviewed  
    REAL    :: LPERSWT                 ! Leaf petiole reserves          g/p        ! (From SeasInit)  
    REAL    :: LPEWAD                  ! Leaf petiole weight            kg/ha      ! (From SeasInit)  
    INTEGER :: LRTIP                   ! Layer with root tip            #          ! (From Growth)    
    INTEGER :: LSEED                   ! Layer with seed                #          ! (From SeasInit)  
    REAL    :: LSENI                   ! Leaf senescence,injury         %/d        ! (From SeasInit)  
    REAL    :: LSNUM(HANUMX)           ! Livestock number               #/ha       ! (From SeasInit)  
    REAL    :: LSTAGE                  ! Leaf stage 0-1 over leaf tier  #          ! (From SeasInit)  
    REAL    :: LSWT(HANUMX)            ! Livestock weight (individual)  kg         ! (From SeasInit)  
    REAL    :: LWLOS                   ! Leaf wt loss,normal senesce    fr         ! (From SeasInit)  
    REAL    :: LWPH                    ! Leaf weight harvested          g/p        ! (From Growth)    
    REAL    :: LWPHC                   ! Leaf wt harvested,cumulative   g/p        ! (From SeasInit)  
    INTEGER :: MDAP                    ! Maturity date.Days>planting    #          ! (From SeasInit)  
    REAL    :: MDAPFR                  ! Maturity DAP+fraction          #          ! (From Integrate) 
    INTEGER :: MDAPM                   ! Maturity DAP,measured          #          ! (From Output)    
    REAL    :: MDAT                    ! Maturity date.Year+DOY         #          ! (From SeasInit)  
    INTEGER :: MDATERR                 ! Maturity date error            d          ! (From Output)    
    INTEGER :: MDATM                   ! Maturity date,measured         #          ! (From Output)    
    REAL    :: MDATT                   ! Maturity date from t file      YrDoy      ! (From Output)    
    REAL    :: MDAYFR                  ! Maturity,fraction of day       #          ! (From SeasInit)  
    INTEGER :: MDOY                    ! Maturity day of year           d          ! (From SeasInit)  
    REAL    :: MJPERE                  ! Energy per Einstein (300-170)  MJ/E       ! (From RunInit)   
    !INTEGER :: MSTG                    ! Maturity stage(eg.black layer) #          ! (From SeasInit) !LPM 05JAN2015 MSTG is not used  
    REAL    :: NCRG                    ! N factor,root growth           ppm        ! (From SeasInit)  
    REAL    :: NDEM2                   ! N demand for growth>minimum    g/p        ! (From Growth)    
    REAL    :: NDEMMN                  ! N demand for growth at minimum g/p        ! (From Growth)    
    REAL    :: NFG                     ! N factor,growth 0-1            #          ! (From SeasInit)  
    REAL    :: NFGCAV                  ! N factor,growth,average,cycle  #          ! (From Integrate) 
    REAL    :: NFGCC                   ! N factor,growh,cycle sum       #          ! (From SeasInit)  
    REAL    :: NFGL                    ! N factor,gr,lower limit        #          ! (From SeasInit)  
    REAL    :: NFGPAV(0:12)            ! N factor,growth,average,tier   #          ! (From SeasInit)  
    REAL    :: NFGPC                   ! N factor,growth,cumulative     #          ! (From SeasInit)  
    REAL    :: NFGU                    ! N factor,gr,upper limit        #          ! (From SeasInit)  
    REAL    :: NFPCAV                  ! N factor,phs,average,cycle     #          ! (From SeasInit)  
    REAL    :: NFPCC                   ! N factor,phs,cumulative,cycle  #          ! (From SeasInit)  
    REAL    :: NFPL                    ! N factor,phs,lower limit       #          ! (From SeasInit)  
    REAL    :: NFPPAV(0:12)            ! N factor,phs,average,tier      #          ! (From SeasInit)  
    REAL    :: NFPPC                   ! N factor,phs,cumulative,tier   #          ! (From SeasInit)  
    REAL    :: NFPU                    ! N factor,phs,upper limit       #          ! (From SeasInit)  
    REAL    :: NFRG                    ! N factor,root growth 0-1       #          ! (From Growth)    
    REAL    :: NFSU                    ! N factor,senescence,upper lim  #          ! (From SeasInit)  
    INTEGER :: NG                      ! Index for group number/cohorts #          ! DA 13DIC2016
    REAL    :: NH4CF                   ! NH4 uptake concentration fac   #          ! (From SeasInit)  
    REAL    :: NH4MN                   ! NH4 conc minimum for uptake    g/Mg       ! (From SeasInit)  
    REAL    :: NLABPC                  ! N labile fraction,standard     %          ! (From SeasInit)  
    INTEGER :: NLAYRROOT               ! Number of layers with roots    #          ! (From Growth)    
    REAL    :: NLLG                    ! N limit,leaf growth            #          ! (From SeasInit)  
    REAL    :: NO3CF                   ! NO3 uptake concentration fac   #          ! (From SeasInit)  
    REAL    :: NO3MN                   ! NO3 conc minimum for uptake    g/Mg       ! (From SeasInit)  
    REAL    :: NODLT                   ! internode length Br=0 lignified cm        ! LPM 08JUN2015
    REAL    :: NODWT                   ! Node wt Br=0  at 3400 ˚Cd      g/node     ! LPM 08JUN2015
    INTEGER :: NOUTPG                  ! Number for growth output file  #          ! (From RunInit)   
    INTEGER :: NOUTPG2                 ! Number for growth output file2 #          ! (From RunInit)   
    INTEGER :: NOUTPGF                 ! Number for growth factors file #          ! (From RunInit)   
    INTEGER :: NOUTPN                  ! Number for growthN output file #          ! (From RunInit)   ! Stem N pool (ie.above minimum) g/p        !
    REAL    :: NPOOLL                  ! Leaf N pool (ie.above minimum) g/p        ! (From Growth)    
    REAL    :: NPOOLR                  ! Root N pool (ie.above minimum) g/p        ! (From Growth)    
    REAL    :: NPOOLS                  ! Stem N pool (ie.above minimum) g/p        ! (From Growth)    
    INTEGER :: NSDAYS                  ! N stress days                  #          ! (From SeasInit)  
    REAL    :: NTUPF                   ! N top-up fraction              /d         ! (From SeasInit)  
    REAL    :: NUF                     ! Plant N supply/demand,max=1.0  #          ! (From SeasInit)  
    REAL    :: NULEFT                  ! N uptake remaining for use     g          ! (From Growth)    
    REAL    :: NUPAC                   ! N uptake,cumulative            kg/ha      ! (From SeasInit)  
    REAL    :: NUPACM                  ! N uptake,cumulative,measured   kg/ha      ! (From Output)    
    REAL    :: NUPAD                   ! N uptake rate (/d)             kg/ha      ! (From SeasInit)  
    REAL    :: NUPAP                   ! Total root N uptake rate,potnl kg/ha      ! (From SeasInit)  
    REAL    :: NUPAPCRP                ! Total N uptake rate,potnl,CSCR kg/ha      ! (From SeasInit)  
    REAL    :: NUPAPCSM                ! Total N uptake rate,potnl,CSM  kg/ha      ! (From SeasInit)  
    REAL    :: NUPAPCSM1               ! Total N uptake rate,pot,CSMmod kg/ha      ! (From SeasInit)  
    REAL    :: NUPC                    ! N uptake,cumulative            g/p        ! (From SeasInit)  
    REAL    :: NUPD                    ! N uptake                       g/p        ! (From SeasInit)  
    REAL    :: NUPRATIO                ! N uptake potential/demand      #          ! (From SeasInit)  
    REAL    :: NUSEFAC                 ! N use factor                   #          ! (From Growth)    
    INTEGER :: ONI                     ! Option number,initial value    #          ! (From SeasInit)  
    INTEGER :: OUTCHOICE               ! Output choice (+reserves,etc)  #          ! (From SeasInit)  
    INTEGER :: OUTCOUNT                ! Output counter                 #          ! (From Output)    
    REAL    :: PARFC                   ! Max photosynthesis/phs at 330  #          ! (From SeasInit)  
    REAL    :: PARI                    ! PAR interception fraction      #          ! (From SeasInit)  
    REAL    :: PARI1                   ! PAR interception fr,1-crop mdl #          ! (From SeasInit)  
    REAL    :: PARIOUT                 ! PAR interception fr for output #          ! (From Output)    
    REAL    :: PARIPREV                ! PAR interception fr,previous   #          ! (From Growth)    
    REAL    :: PARIUE                  ! PAR intercepted use efficiency g/MJ       ! (From SeasInit)  
    REAL    :: PARIUED                 ! PAR intercepted use efficiency g/MJ       ! (From Integrate) 
    REAL    :: PARIX                   ! PAR interception,maximum,fr    #          ! (From SeasInit)  
    REAL    :: PARMJC                  ! PAR,cumulative                 MJ/m2      ! (From SeasInit)  
    REAL    :: PARMJFAC                ! PAR conversion factor          MJ/MJ      ! (From RunInit)   
    REAL    :: PARMJIADJ               ! PAR intercepted adjustment     MJ/m2      ! (From Growth)    
    REAL    :: PARMJIC                 ! PAR intercepted,cumulative     MJ/m2      ! (From SeasInit)  
    REAL    :: PARU                    ! PAR utilization effic          g/MJ       ! (From SeasInit)  
    REAL    :: PARU2                   ! PAR use efficiency,afterchange g/MJ       ! (From SeasInit)  
    REAL    :: PARUE                   ! PAR use efficiency,standard    g/MJ       ! (From SeasInit)  
    REAL    :: PARUEC                  ! PAR use efficiency to today    g/MJ       ! (From Integrate) 
    INTEGER :: PATHL                   ! Path length                    #          ! (From SeasInit)  
    REAL    :: PD(0:PSX)               ! Tier durations                 deg.d      ! (From SeasInit)  
    REAL    :: PDADJ                   ! Tier duration adjustment       deg.d      ! (From SeasInit)  
    INTEGER :: PDATE                   ! Planting Yrdoy from X-file     #          ! (From SeasInit)  
    INTEGER :: PDAYS(0:12)             ! Tier durations                 PVoCd      ! (From SeasInit)  
    REAL    :: PDL(0:PSX)              ! Tier durations,phint units     #          ! (From SeasInit)  
    REAL    :: PECM                    ! Emergence duration             Cd/cm      ! (From SeasInit)  
    !REAL    :: PEGD                    ! Duration,germ                  deg.d      ! (From SeasInit) !LPM 21MAR2016 Deleted, instead use PGERM 
    REAL    :: PEMRG                   ! Reserves use by TT for emerg   g/Cd       ! (From SeasInit)
    REAL    :: PFGCAV                  ! P factor,growh,cycle,av 0-1    #          ! (From Output)    
    REAL    :: PFGPAV(0:12)            ! P factor,growh,tier,av 0-1     #          ! (From Output)    
    REAL    :: PFPCAV                  ! P factor,phs,cycle,average 0-1 #          ! (From Output)    
    REAL    :: PFPPAV(0:12)            ! P factor,phs,tier,average 0-1  #          ! (From Output)    
    INTEGER :: PGDAP                   ! Plantgro file days after plt   #          ! (From Output)    
    REAL    :: PGERM                   ! Germination duration           deg.d      ! (From SeasInit)  
    INTEGER :: PGROCOL(20)             ! Plantgro column = t-file data  #          ! (From Output)           
    REAL    :: PGVAL                   ! Plantgro file value            #          ! (From Output)    
    !REAL    :: PHINT                   ! Phylochron interval            deg.d      ! (From SeasInit)  !LPM 21MAY2015 PHINT is not used
    REAL    :: PHINTFAC                ! Phylochron interval factor     #          ! (From SeasInit)  
    !REAL    :: PHINTS                  ! Phylochron interval,standard   deg.d      ! (From SeasInit) !LPM 21MAY2015 PHINT is not used 
    REAL    :: PHOTQR                  ! Photon requirement,calculated  E/mol      ! (From SeasInit)  
    REAL    :: PHSV                    ! Phs,fr reduction with VPD       /KPa      ! (From SeasInit)  
    REAL    :: PHTV                    ! Phs,threshold VPD for reduction KPa       ! (From SeasInit)  
    REAL    :: PLA                     ! Plant leaf area                cm2        ! (From SeasInit)  
    !REAL    :: PLAGS2                  ! Plant lf area g,sh+br,H2oNT cm2/p         ! (From SeasInit) !LPM 23MAR15 non necessary PLAGSB2 considers all the branches and shoots  
    REAL    :: PLAGSB2                 ! Plant lf area g,sh+br,H2oNT    cm2/p      ! (From SeasInit)!LPM 23MAR15  
    REAL    :: PLAGSB3                 ! Plant lf area g,sh+br,H2oNTA   cm2/p      ! (From SeasInit)  
    REAL    :: PLAGSB4                 ! Plant lf area g,sh+br,H2oNTAN2 cm2/p      ! (From SeasInit)  
    REAL    :: PLAS                    ! Leaf area senesced,normal      cm2/p      ! (From SeasInit)  
    REAL    :: PLASI                   ! Leaf area senesced,injury      cm2/p      ! (From SeasInit)  
    REAL    :: PLASL                   ! Leaf area senesced,low light   cm2/p      ! (From SeasInit)  
    REAL    :: PLASN                   ! Leaf area senesced,N shortage  cm2/p      ! (From Growth)    
    REAL    :: PLASP                   ! Leaf area senesced,phyllochron cm2/p      ! (From SeasInit)  
    REAL    :: PLASS                   ! Leaf area senesced,stress      cm2/p      ! (From SeasInit)  
    REAL    :: PLASTMP                 ! Leaf area senesced,temporary   cm2/p      ! (From Integrate) 
    REAL    :: PLASTMP2                ! Leaf area senesced,temporary   cm2/p      ! (From Integrate) 
    REAL    :: PLASW                   ! Leaf area senesced,h2o stress  cm2/p      ! (From Growth)    
    REAL    :: PLAX                    ! Plant leaf area,maximum        cm2        ! (From SeasInit)  
    INTEGER :: PLDAY                   ! Planting day of year           d          ! (From SeasInit)  
    INTEGER :: PLDAYTMP                ! Planting day of year           #          ! (From Output)    
    REAL    :: PLMAGE                  ! Planting material age          d          ! (From SeasInit)  
    REAL    :: PLPH                    ! Plants/hill or shoots/cutting  #          ! (From SeasInit)  
    INTEGER :: PLTOHARYR               ! Planting to final harvestyears #          ! (From SeasInit)  
    REAL    :: PLTPOP                  ! Plant Population               #/m2       ! (From SeasInit)  
    REAL    :: PLTPOPE                 ! Plant Population established   #/m2       ! (From SeasInit)  
    REAL    :: PLTPOPP                 ! Plant Population planned       #/m2       ! (From SeasInit)  
    INTEGER :: PLYEAR                  ! Planting year                  #          ! (From SeasInit)  
    INTEGER :: PLYEARDOY               ! Planting year*1000+DOY         #          ! (From SeasInit)  
    INTEGER :: PLYEARDOYPREV           ! Year+Doy for planting,previous #          ! (From Output)    
    INTEGER :: PLYEARDOYT              ! Planting year*1000+DOY target  #          ! (From SeasInit)  
    INTEGER :: PLYEARREAD              ! Planting year as read          #          ! (From SeasInit)  
    INTEGER :: PLYEARTMP               ! Year(Yr)+Doy,planting tem val  #          ! From CSCAS_29DE  
    REAL    :: PPEXP                   ! Photoperiod response exponent  #          ! (From SeasInit)  
    REAL    :: PPOE                    ! Plant Population established   #/m2       ! (From SeasInit) !LPM 06MAR2016 To have just one name for PPOE
    REAL    :: PPOP                    ! Plant Population planned       #/m2       ! (From SeasInit) !LPM 06MAR2016 To have just one name for PPOP
    REAL    :: PPTHR                   ! Photoperiod threshold          h          ! (From SeasInit)  
    INTEGER :: PSDAP  (0:PSX)          ! Stage DAP                      #          ! (From SeasInit)  !LPM 28MAR15 Starting from 0 PSX
    REAL    :: PSDAPFR(0:PSX)          ! Stage DAP+fr                   #          ! (From Integrate) !LPM 28MAR15 Starting from 0 PSX
    INTEGER :: PSDAPM (0:PSX)          ! Stage DAP,measured             #          ! (From SeasInit)  !LPM 28MAR15 Starting from 0 PSX
    INTEGER :: PSDAT  (0:PSX)          ! Stage YrDoydate                #          ! (From SeasInit)  !LPM 28MAR15 Starting from 0 PSX
    INTEGER :: PSDATM (0:PSX)          ! Stage date,measured            #          ! (From Output)    !LPM 28MAR15 Starting from 0 PSX
    REAL    :: PSDAYFR(0:PSX)          ! Stage fraction of day          #          ! (From SeasInit)  
    INTEGER :: PSIDAP                  ! Principal stage,inter,date     dap        ! (From Output)    
    INTEGER :: PSIDAPM                 ! Principal stg,inter,measured   dap        ! (From Output)    
    INTEGER :: PSIDATERR               ! Principal stage,inter,error    dap        ! (From Output)    
    INTEGER :: PSNUM                   ! Principal stage number         #          ! (From SeasInit)  
    REAL    :: PSTART(0:PSX)           ! Principal thresholds           du         ! (From SeasInit)  
    !REAL    :: PTF                     ! Partition fraction to tops     #          ! (From SeasInit) !LPM 19MAY2015 PTF deleted   
    REAL    :: PTFA                    ! Partition fr adjustment coeff. #          ! (From SeasInit)  
    REAL    :: PTTN                    ! Minimum soil temperature,plt   C          ! (From SeasInit)  
    REAL    :: PTX                     ! Maximum soil temperature,plt   C          ! (From SeasInit)  
    INTEGER :: PWDINF                  ! First YrDoy of planting window #          ! (From SeasInit)  
    INTEGER :: PWDINL                  ! Last YrDoy of planting window  #          ! (From SeasInit)  
    INTEGER :: PWDOYF                  ! First doy of planting window   #          ! From CSCAS_29DE  
    INTEGER :: PWDOYL                  ! Last doy of planting window    #          ! From CSCAS_29DE  
    INTEGER :: PWYEARF                 ! First year of planting window  #          ! From CSCAS_29DE  
    INTEGER :: PWYEARL                 ! Last year of planting window   #          ! From CSCAS_29DE  
    REAL    :: RAINC                   ! Rainfall,cumulative            mm         ! (From SeasInit)  
    REAL    :: RAINCC                  ! Precipitation cycle sum        mm         ! (From SeasInit)  
    REAL    :: RAINPAV(0:12)           ! Rainfall,average for tier      mm         ! (From SeasInit)  
    REAL    :: RAINPC(0:12)            ! Precipitation tier sum         mm         ! (From SeasInit)  
    REAL    :: RANC                    ! Roots actual N concentration   #          ! (From SeasInit)  
    REAL    :: RATM                    ! Boundary layer,air,resistance  s/m        ! (From SeasInit)  
    REAL    :: RAW                     ! Total soil water 'potential'   #          ! (From Growth)
    REAL    :: RCROP                   ! Stomatal res,crop basis        s/m        ! (From SeasInit)  
    REAL    :: RDGS                    ! Root depth growth rate,standrd cm/d       ! (From SeasInit)  
    REAL    :: RESCAL(0:20)            ! Residue C at maturity,by layer kg/ha      ! (From SeasInit)  
    REAL    :: RESLGAL(0:20)           ! Residue lignin,maturity,layer  kg/ha      ! (From SeasInit)  
    REAL    :: RESNAL(0:20)            ! Residue N at harvest by layer  kg/ha      ! (From SeasInit)  
    REAL    :: RESPC                   ! Respiration,total,cumulative   g/p        ! (From SeasInit)  
    REAL    :: RESPRC                  ! Respiration,roots,cumulative   g/p        ! (From SeasInit)  
    REAL    :: RESPTC                  ! Respiration,tops,cumulative    g/p        ! (From SeasInit)  
    REAL    :: RESWAL(0:20)            ! Residue om added by layer      kg/ha      ! (From SeasInit)  
    REAL    :: RESWALG(0:20)           ! Residue om,maturity,by layer   kg/ha      ! (From SeasInit)  
    REAL    :: RFAC                    ! Root length & H2O fac,N uptake #          ! (From SeasInit)  
    REAL    :: RLDF(20)                ! Root length density fac,new gr #          ! (From Growth)    
    REAL    :: RLF                     ! Leaf stomatal res,330.0 ppmCO2 s/m        ! (From SeasInit)  
    REAL    :: RLFC                    ! Leaf stomatal resistance       s/m        ! (From SeasInit)  
    REAL    :: RLFWU                   ! Root length factor,water uptk  /cm2       ! (From SeasInit)  
    REAL    :: RLIGP                   ! Root lignin concentration      %          ! (From SeasInit)  
    REAL    :: RLWR                    ! Root length/weight ratio     m/10mg       ! (From SeasInit)  
    REAL    :: RM                      ! Mesophyll resistance           d/m        ! (From Growth)    
    REAL    :: RNAD                    ! Root N                         kg/ha      ! (From SeasInit)  
    REAL    :: RNAM                    ! Root N at maturity             kg/ha      ! (From SeasInit)  
    REAL    :: RNAMM                   ! Root N at maturity,measured    kg/ha      ! (From SeasInit)  
    REAL    :: RNCM                    ! Root N conc,minimum            fr         ! (From SeasInit)  
    REAL    :: RNCMN(0:1)              ! Root N conc,minimum            fr         ! (From SeasInit)  
    REAL    :: RNCR                    ! Roots N relative to maximum    #          ! (From SeasInit)  
    REAL    :: RNCX                    ! Root N concentration,maximum   fr         ! (From SeasInit)  
    REAL    :: RNCXS(0:1)              ! Roots N conc,maximum,by stage  fr         ! (From SeasInit)  
    REAL    :: RNDEM                   ! Root demand for N              g/p        ! (From SeasInit)  
    REAL    :: RNDEMG                  ! Root demand for N,for growth   g/p        ! (From Growth)    
    REAL    :: RNDEMTU                 ! Root demand for N,for topup    g/p        ! (From Growth)    
    REAL    :: RNH4U(20)               ! Potential ammonium uptake      kg/ha      ! (From Growth)    
    INTEGER :: RNI                     ! Replicate number,initial value #          ! (From SeasInit)  
    REAL    :: RNO3U(20)               ! Potential nitrate uptake       kg/ha      ! (From Growth)    
    REAL    :: RNPCMN(0:1)             ! Root N conc,minimum            %          ! (From SeasInit)  
    REAL    :: RNPCS(0:1)              ! Roots N conc,standard,by stage %          ! (From SeasInit)  
    REAL    :: RNUSE(0:2)              ! Root N use,overall and parts   g          ! (From SeasInit)  
    REAL    :: ROOTN                   ! Root N                         g/p        ! (From SeasInit)  
    REAL    :: ROOTNEXCESS             ! Root N > critical              g/p        ! (From Integrate) 
    REAL    :: ROOTNS                  ! Root N senesced                g/p        ! (From SeasInit)  
    REAL    :: ROWSPC                  ! Row spacing                    cm         ! (From SeasInit)  
    INTEGER :: RPCOL                   ! Replicate column number        #          ! (From Output)    
    REAL    :: RRESP                   ! Root respiration fraction      #          ! (From SeasInit)  
    REAL    :: RSCD                    ! Reserves concentration,end day fr         ! (From SeasInit)    
    REAL    :: RSCM                    ! Reserves concentration,mature  fr         ! (From SeasInit)  
    REAL    :: RSCMM                   ! Reserves conc,maturity,msured  #          ! (From Output)    
    REAL    :: RSCX                    ! Max.reserves conc.reached      fr         ! (From SeasInit)  
    REAL    :: RSEN                    ! Root senescence fraction       #          ! (From SeasInit)  
    REAL    :: RSFP                    ! Reserves factor,photosynthesis fr         ! (From SeasInit)  
    REAL    :: RSFPL                   ! Reserves conc.,phs.lower bound fr         ! (From SeasInit)  
    REAL    :: RSFPU                   ! Reserves conc.,phs upper bound fr         ! (From SeasInit)  
    REAL    :: RSFRS                   ! Reserves fr.assim.for stem,std fr         ! (From SeasInit)  
    REAL    :: RSN                     ! Reserve N                      g/p        ! (From SeasInit)  
    REAL    :: RSNAD                   ! Reserve N                      kg/ha      ! (From SeasInit)  
    REAL    :: RSNPH                   ! Reserves N harvested           g/p        ! (From SeasInit)  
    REAL    :: RSNPHC                  ! Reserves N harvested,cum       g/p        ! (From SeasInit)  
    REAL    :: RSNUSED                 ! Reserve N used                 g/p        ! (From SeasInit)  
    REAL    :: RSPCO                   ! Res conc.above which overflow  %          ! (From SeasInit)  
    REAL    :: RSSRWTGLFADJ            ! Reserves+storage from lf N adj g/p        ! (From Growth)    
    REAL    :: RSUSE                   ! Reserves utilisation fraction  #          ! (From SeasInit)  
    REAL    :: RSWAD                   ! Reserves weight                kg/ha      ! (From SeasInit)  
    REAL    :: RSWAM                   ! Reserves at maturity           kg/ha      ! (From SeasInit)  
    REAL    :: RSWAMM                  ! Reserves at maturity,measured  kg/ha      ! (From SeasInit)  
    REAL    :: RSWPH                   ! Reserves weight harvested      g/p        ! (From Growth)    
    REAL    :: RSWPHC                  ! Reserves wt harvested,cum      g/p        ! (From SeasInit)  
    REAL    :: RSWT                    ! Reserves weight                g/p        ! (From SeasInit)  
    REAL    :: RSWTGLFADJ              ! Reserves from lf N adjustment  g/p        ! (From Integrate) 
    REAL    :: RSWTM                   ! Reserves weight,maturity       g/p        ! (From SeasInit)  
    REAL    :: RSWTX                   ! Reserves weight,maximum        g/p        ! (From SeasInit)  
    REAL    :: RTDEP                   ! Root depth                     cm         ! (From SeasInit)  
    REAL    :: RTDEPG                  ! Root depth growth              cm/d       ! (From SeasInit)  
    REAL    :: RTDEPTMP                ! Root depth,temporary value     cm/d       ! (From Growth)    
    REAL    :: RTNH4                   ! N uptake/root length           mg/cm      ! (From SeasInit)  
    REAL    :: RTNO3                   ! N uptake/root length           mg/cm      ! (From SeasInit)  
    REAL    :: RTNSL(20)               ! Root N senesced by layer       g/p        ! (From SeasInit)  
    REAL    :: RTRESP                  ! Root respiration               g/p        ! (From SeasInit)  
    REAL    :: RTRESPADJ               ! Root respiration rate N adj    g/p        ! (From SeasInit)  
    INTEGER :: RTSLXDATE               ! Roots into last layer date     YYDDD      ! (From SeasInit)  
    REAL    :: RTUFR                   ! Max fraction root wt useable   fr         ! (From SeasInit)  
    REAL    :: RTWT                    ! Root weight                    g/p        ! (From SeasInit)  
    REAL    :: RTWTAL(20)              ! Root weight by layer           kg/ha      ! (From SeasInit)  
    REAL    :: RTWTG                   ! Root weight growth             g/p        ! (From SeasInit)  
    REAL    :: RTWTGADJ                ! Root growth rate N adjusted    g/p        ! (From SeasInit)  
    REAL    :: RTWTGL(20)              ! Root weight growth by layer    g/p        ! (From SeasInit)  
    REAL    :: RTWTL(20)               ! Root weight by layer           g/p        ! (From SeasInit)  
    REAL    :: RTWTM                   ! Root weight,maturity           g/p        ! (From SeasInit)  
    REAL    :: RTWTSL(20)              ! Root weight senesced by layer  g/p        ! (From SeasInit)  
    REAL    :: RTWTUL(20)              ! Root weight used for tops,lyr  g/p        ! (From Growth)    
    INTEGER :: RUNCRP                  ! Run (internal within module)   #          ! (From RunInit)   
    REAL    :: RUNOFFC                 ! Calculated runoff,cumulative   mm         ! (From SeasInit)  
    REAL    :: RWAD                    ! Root weight                    kg/ha      ! (From SeasInit)  
    REAL    :: RWAM                    ! Root weight,maturity           kg/ha      ! (From SeasInit)  
    REAL    :: RWAMM                   ! Root wt at maturity,measured   kg/ha      ! (From SeasInit)  
    REAL    :: RWUMXI                  ! Root water uptake,max,init.val cm2/d      ! (From SeasInit)  
    REAL    :: SAID                    ! Stem area index                m2/m2      ! (From SeasInit)  
    REAL    :: SANCOUT                 ! Stem+LeafPetiole N conc        #          ! (From SeasInit)   
    REAL    :: SCNCT                   ! Stem critical max N conc       #          !LPM 25MAY2015 Added to estimate the total value for the stems and then VCNC
    REAL    :: SCNMT                   ! Stem critical min N conc       #          !LPM 25MAY2015 Added to estimate the total value for the stems and then VCNC
    REAL    :: SDCOAT                  ! Non useable material in seed   g          ! (From SeasInit)  
    REAL    :: SDDUR                   ! Seed reserves use duration     d          ! (From SeasInit)  
    REAL    :: SDEPTH                  ! Sowing depth                   cm         ! (From SeasInit)  
    REAL    :: SDEPTHU                 ! Sowing depth,uppermost level   cm         ! (From SeasInit)  
    REAL    :: SDNAD                   ! Seed N                         kg/ha      ! (From SeasInit)  
    REAL    :: SDNAP                   ! Seed N at planting             kg/ha      ! (From SeasInit)  
    REAL    :: SDNC                    ! Seed N concentration           #          ! (From SeasInit)  
    REAL    :: SDNPCI                  ! Seed N concentration,initial   %          ! (From SeasInit)  
    REAL    :: SDRATE                  ! Seeding 'rate'                 kg/ha      ! (From SeasInit)  
    REAL    :: SDRS                    ! Seed reserves fraction of seed #          ! (From SeasInit)  !LPM 22MAR2016 Keep value SDRS  
    REAL    :: SDSZ                    ! Seed size                      g          ! (From SeasInit) !LPM 22MAR2016 Keep value SDWT  
    REAL    :: SDWAD                   ! Seed/Stick weight              kg/ha      ! (From SeasInit)  
    REAL    :: SDWADOUT                ! Seed/Stick weight for output   kg/ha      ! (From Output)    
    REAL    :: SDWAM                   ! Seed at maturity               kg/ha      ! (From SeasInit)  
    REAL    :: SDWT                    ! Seed size by unit of length    g/cm       ! (From SeasInit) 
    REAL    :: SEEDN                   ! Seed N                         g/p        ! (From SeasInit)  
    REAL    :: SEEDNI                  ! Seed N,initial                 g/p        ! (From SeasInit)  
    REAL    :: SEEDNUSE                ! N use from seed                g          ! (From Growth)    
    REAL    :: SEEDNUSE2               ! N use from seed,supplementary  g          ! (From Growth)    
    REAL    :: SEEDRS                  ! Seed reserves                  g/p        ! (From SeasInit)  
    REAL    :: SEEDRSAV                ! Seed reserves available        g/p        ! (From SeasInit)  
    !REAL    :: SEEDRSAVR               ! Seed reserves available,roots  g/p        ! (From Growth) !LPM 22DEC2016 Root growth based on top growth (deleted SEEDRSAVR)     
    REAL    :: SEEDRSI                 ! Seed reserves,initial          g/p        ! (From SeasInit)  
    REAL    :: SEEDUSE                 ! Seed reserves use              g/p        ! (From SeasInit) 
    REAL    :: SEEDUSED                ! Seed reserves use by germ day  g/p/d      ! (From SeasInit) !LPM 22MAR2016 Added SEEDUSED to estimate the use of reserves in germination
    REAL    :: SEEDUSER                ! Seed reserves use,roots        g/p        ! (From SeasInit)  
    REAL    :: SEEDUSES                ! Seed reserves use shoot dev    g/p        ! (From SeasInit) !LPM 22MAR2016 Added SEEDUSES to estimate the use of reserves for shoot development
    REAL    :: SEEDUSET                ! Seed reserves use,tops         g/p        ! (From SeasInit)  
    REAL    :: SELONG                  ! Daily shoot elongation         cm         ! (From SeasInit)
    REAL    :: SELONGT                 ! Total shoot elongation         cm         ! (From SeasInit)
    REAL    :: SENCAGS                 ! Senesced C added to soil       kg/ha      ! (From SeasInit)  
    REAL    :: SENCAS                  ! Senesced C added to soil       kg/ha      ! (From SeasInit)  
    REAL    :: SENCL(0:20)             ! Senesced C,by layer            g/p        ! (From SeasInit)  
    REAL    :: SENCS                   ! Senesced C added to soil       g/p        ! (From SeasInit)  
    REAL    :: SENFR                   ! Senesced fraction lost from pl #          ! (From Growth)    
    REAL    :: SENLA                   ! Senesced leaf area,total       cm2/p      ! (From SeasInit)  
    REAL    :: SENLAGS                 ! Senesced lignin added to soil  kg/ha      ! (From SeasInit)  
    REAL    :: SENLALITTER             ! Senesced leaf area,litter      cm2/p      ! (From SeasInit)  
    REAL    :: SENLAS                  ! Senesced lignin added to soil  kg/ha      ! (From SeasInit)  
    REAL    :: SENLFG                  ! Senesced leaf                  g/p        ! (From SeasInit)  
    REAL    :: SENLFGRS                ! Senesced leaf to reserves      g/p        ! (From SeasInit)  
    REAL    :: SENLL(0:20)             ! Senesced lignin added,by layer g/p        ! (From SeasInit)  
    REAL    :: SENLS                   ! Senesced lignin added to soil  g/p        ! (From SeasInit)  
    REAL    :: SENNAGS                 ! Senesced N added to soil       kg/ha      ! (From SeasInit)  
    REAL    :: SENNAL(0:20)            ! Senesced N,by layer            kg/ha      ! (From SeasInit)  
    REAL    :: SENNAS                  ! Senesced N added to soil       kg/ha      ! (From SeasInit)  
    REAL    :: SENNATC                 ! Senesced N,litter+soil,cum     kg/ha      ! (From SeasInit)  
    REAL    :: SENNATCM                ! Senesced N,litter+soil,cum,mes kg/ha      ! (From SeasInit)  
    REAL    :: SENNGS                  ! Senesced N added to soil       g/p        ! (From Integrate) 
    REAL    :: SENNL(0:20)             ! Senesced N,by layer            g/p        ! (From SeasInit)  
    REAL    :: SENNLFG                 ! Senesced N from leaves         g/p        ! (From SeasInit)  
    REAL    :: SENNLFGRS               ! Senesced N from leaves,to rs   g/p        ! (From SeasInit)  
    REAL    :: SENNS                   ! Senesced N added to soil       g/p        ! (From SeasInit)  
    REAL    :: SENROOT                 ! Senesced weight,soil,cum       g/p        ! (From SeasInit)  
    REAL    :: SENROOTA                ! Senesced weight,soil,cumulativ kg/ha      ! (From SeasInit)  
    REAL    :: SENRTG                  ! Senescent root material growth g/p        ! (From SeasInit)  
    REAL    :: SENTOPLITTER            ! Dead wt.to litter              g/p        ! (From SeasInit)  
    REAL    :: SENTOPLITTERA           ! Dead wt.to litter              kg/ha      ! (From Integrate) 
    REAL    :: SENTOPLITTERG           ! Senescent top->litter growth   g/p        ! (From SeasInit)  
    REAL    :: SENWACM                 ! Senesced weight,total,cum to m kg/ha      ! (From SeasInit)  
    REAL    :: SENWACMM                ! Senesced om,litter+soil,cum,ms kg/ha      ! (From SeasInit)  
    REAL    :: SENWAGS                 ! Senesced weight added to soil  kg/ha      ! (From SeasInit)  
    REAL    :: SENWAL(0:20)            ! Senesced om by layer           kg/ha      ! (From SeasInit)  
    REAL    :: SENWALG(0:20)           ! Senesced om added by layer     kg/ha      ! (From SeasInit)  
    REAL    :: SENWL(0:20)             ! Senesced om (cumulative),layer g/p        ! (From SeasInit)    
    REAL    :: SESR                    ! Shoot elongation rate+reserves cm2/g      ! (From SeasInit) !LPM 21MAR2016 Added SESR   
    INTEGER :: SHDAP                   ! Shoot prodn.start DAP          #          ! (From Integrate) 
    INTEGER :: SHDAT                   ! Shoot prodn.startdate YEARDOY  #          ! (From SeasInit)  
    REAL    :: SHGR(22)                ! Shoot size relative to 1       #          ! (From SeasInit)  
    REAL    :: SHLA(25)                ! Shoot leaf area produced       cm2        ! (From SeasInit)  
    REAL    :: SHLAG2(25)              ! Shoot lf area gr,all axis,H2oN cm2        ! (From SeasInit)  
    REAL    :: SHLAG2B(0:PSX)          ! Shoot lf area gr by br,H2oNt   cm2        ! (From SeasInit)
    REAL    :: SHLAGB2(25)             ! Shoot lf area gr,+br,H2oNt     cm2        ! (From Growth)    
    REAL    :: SHLAGB3(25)             ! Shoot lf area gr,+br,H2oNtA    cm2        ! (From Growth)    
    REAL    :: SHLAGB4(25)             ! Shoot lf area gr,+br,H2oNtAN2  cm2        ! (From Growth)    
    REAL    :: SHLAS(25)               ! Shoot leaf area senesced       cm2        ! (From SeasInit)  
    REAL    :: SHNUM                   ! Shoot (incl.main stem) number  #/p        ! (From SeasInit)  
    REAL    :: SHNUMAD                 ! Shoot (incl.main stem) number  #/m2       ! (From SeasInit)  
    REAL    :: SHNUML(LNUMX)           ! Shoot # at leaf position       #/p        ! (From SeasInit)  
    REAL    :: SHRTD                   ! Shoot/root ratio               #          ! (From SeasInit)  
    REAL    :: SHRTM                   ! Shoot/root ratio,maturity      #          ! (From SeasInit)  
    REAL    :: SHRTMM                  ! Shoot/root ratio,maturity,meas #          ! (From Output)    
    REAL    :: SLA                     ! Specific leaf area             cm2/g      ! (From SeasInit)  
    REAL    :: SLAOUT                  ! Specific leaf area for output  cm2/g      ! (From Output)    
    REAL    :: SLIGP                   ! Stem lignin concentration      %          ! (From SeasInit)  
    REAL    :: SNAD                    ! Stem N (stem+petiole+rs)       kg/ha      ! (From SeasInit)  
    REAL    :: SNCMN(0:1)              ! Stem N conc,minimum            fr         ! (From SeasInit)  
    REAL    :: SNCRM                   ! Stem N relative to maximum (mean)#        !LPM 25MAY2015
    REAL    :: SNCXS(0:1)              ! Stem N conc,maximum,stage      fr         ! (From SeasInit)  
    REAL    :: SNDEM                   ! Stem demand for N              g/p        ! (From Growth)    
    REAL    :: SNDEMG                  ! Stem demand for N,for growth   g/p        ! (From Growth)    
    REAL    :: SNDEMTU                 ! Stem demand for N,for topup    g/p        ! (From Growth)    
    REAL    :: SNH4(20)                ! Soil NH4 N                     kg/ha      ! (From Growth)    
    REAL    :: SNH4PROFILE             ! Soil NH4 N in profile          kg/ha      ! (From Growth)    
    REAL    :: SNH4ROOTZONE            ! Soil NH4 N in root zone        kg/ha      ! (From SeasInit)  
    INTEGER :: SNI                     ! Sequence number,as initiated   #          ! (From SeasInit)  
    REAL    :: SNO3(20)                ! Soil NO3 N                     kg/ha      ! (From Growth)    
    REAL    :: SNO3PROFILE             ! Soil NO3 N in profile          kg/ha      ! (From SeasInit)  
    REAL    :: SNO3ROOTZONE            ! Soil NO3 N in root zone        kg/ha      ! (From SeasInit)  
    REAL    :: SNPCMN(0:1)             ! Stem N conc,minimum            %          ! (From SeasInit)  
    REAL    :: SNPCS(0:1)              ! Stem N conc,standard,stage     %          ! (From SeasInit)  
    REAL    :: SNPH                    ! Stem N harvested               g/p        ! (From SeasInit)  
    REAL    :: SNPHC                   ! Stem N harvested,cumulative    g/p        ! (From SeasInit)  
    REAL    :: SNUSE(0:2)              ! Shoot N use,overall and parts  g          ! (From SeasInit)  
    REAL    :: SNUSEN(0:2,0:PSX,0:LCNUMX)!Shoot N use by canopy level   g          !LPM 23MAY2015 added to consider N concentration by node
    REAL    :: SPRL                    ! Sprout/cutting length          cm         ! (From SeasInit)  
    REAL    :: SRAD20                  ! Solar radiation av,20 days     MJ/m2      ! (From Integrate) 
    REAL    :: SRAD20S                 ! Solar radiation sum            MJ/m2      ! (From Integrate) 
    REAL    :: SRADC                   ! Solar radiation,cumulative     MJ/m2      ! (From SeasInit)  
    REAL    :: SRADCAV                 ! Solar radiation,cycle average  MJ/m2      ! (From SeasInit)  
    REAL    :: SRADCC                  ! Radiation,cycle sum            Mj/m2      ! (From SeasInit)  
    REAL    :: SRADD(20)               ! Solar radiation on specific d  MJ/m2      ! (From SeasInit)  
    REAL    :: SRADPAV(0:12)           ! Solar radiation,tier average   MJ/m2      ! (From SeasInit)  
    REAL    :: SRADPC                  ! Solar radiation,tier sum       MJ/m2      ! (From SeasInit)  
    REAL    :: SRADPREV                ! Solar radiation,previous day   MJ/m2      ! (From SeasInit)  
    REAL    :: SRANC                   ! Storage root N concentration   g/p        ! (From SeasInit)  
    !REAL    :: SRDAYFR                 ! Storage root fraction of day   #          ! (From Growth) !LPM 05 JUN2015 SRDAYFR is not used    
    !REAL    :: SRFR                    ! Storage root fraction,basic    #          ! (From SeasInit)!LPM 08 JUN2015 SRFR is not used     
    REAL    :: SRNAD                   ! Storage root N                 kg/ha      ! (From Integrate) 
    REAL    :: SRNAM                   ! Storage root N at maturity     kg/ha      ! (From SeasInit)  
    REAL    :: SRNDEM                  ! Storage root demand for N      g/p        ! (From SeasInit)  
    REAL    :: SRNDEMG                 ! St.rt.demand for N,for growth  g/p        ! (From Growth)    
    REAL    :: SRNDEMTU                ! St.rt.demand for N,for topup   g/p        ! (From Growth)    
    REAL    :: SRNOAD                  ! Storage root/group,given day   #/m2       ! (From SeasInit)  
    REAL    :: SRNOAM                  ! Storage root #/area,maturity   #/m2       ! (From SeasInit)  
    REAL    :: SRNOAMM                 ! Storage root/group,mature,meas #          ! (From Output)    
    REAL    :: SRNOGM                  ! Storage root/group,maturity    #          ! (From SeasInit)  
    REAL    :: SRNOGMM                 ! Storage root/group,mature,meas #          ! (From Output)    
    ! INTEGER :: SRNOPD                  ! Storage root number per plant  #          ! (From SeasInit)  ! issue 50
    ! REAL    :: SRNOW                   ! Cultivar coeff,storage root #  #/g        ! (From SeasInit)  ! issue 50  SR#WT is not used
    REAL    :: SRNPCS                  ! Storage root N%,standard       #          ! (From SeasInit)  
    REAL    :: SRNPCM                  ! Storage root N%,maturity       %          ! (From Output)    
    REAL    :: SRNUSE(0:2)             ! Storage root N use,total/parts g          ! (From SeasInit)  
    REAL    :: SROOTN                  ! Storage root N                 g/p        ! (From SeasInit)  
    REAL    :: SRPRS                   ! Storage protein standard %     #          ! (From SeasInit)  
    REAL    :: SRWAD                   ! Storage root weight            kg/ha      ! (From Integrate) 
    REAL    :: SRWT                    ! Root storage organ weight      g/p        ! (From SeasInit)  
    REAL    :: SRWTGRS                 ! Root storage,reserves>std.%    g/p        ! (From SeasInit)  
    ! REAL    :: SRWUD                   ! Storage root size              g          ! (From SeasInit)   ! issue 50
    REAL    :: SRWUM                   ! Storage root wt/unit,maturity  g          ! (From SeasInit)  
    REAL    :: SRWUMM                  ! Storage root wt/unit,mat,meas  g          ! (From SeasInit)  
    REAL    :: STAI                    ! Stem area index                m2/m2      ! (From SeasInit)  
    REAL    :: STAIG                   ! Stem area index,growth         m2/m2      ! (From SeasInit)  
    REAL    :: STAIS                   ! Stem area index senesced       m2/m2      ! (From SeasInit)  
    INTEGER :: STARNUM                 ! Star line number,as read file  #          ! (From Output)    
    INTEGER :: STARNUMM                ! Star line number,measured data #          ! (From Output)    
    INTEGER :: STARNUMO                ! Star line number,output file   #          ! (From Output)    
    REAL    :: STDAY                   ! Standard day                   C.d/d      ! (From RunInit)   
    REAL    :: STEMN                   ! Stem N                         g/p        ! (From SeasInit)  
    REAL    :: STEMNEXCESS             ! Stem N > critical              g/p        ! (From Integrate) 
    INTEGER :: STEPNUM                 ! Step number per day            #          ! (From RunInit)   
    INTEGER :: STGEDAT                 ! Stem growth end date (Yrdoy)   #          ! (From SeasInit)  
    REAL    :: STRESS(20)              ! Min h2o,n factors for growth   #          ! (From Integrate) 
    REAL    :: STRESS20                ! 20d av.,min h2o,n gr.factors   #          ! (From Integrate) 
    REAL    :: STRESS20N               ! 20d av.,n gr.factor            #          ! (From Integrate) 
    REAL    :: STRESS20NS              ! 20d sum,n gr.factors           #          ! (From Integrate) 
    REAL    :: STRESS20S               ! 20d sum.,min h2o,n gr.factors  #          ! (From Integrate) 
    REAL    :: STRESS20W               ! 20d av.,h2o gr.factor          #          ! (From Integrate) 
    REAL    :: STRESS20WS              ! 20d sum,h2o gr.factors         #          ! (From Integrate) 
    REAL    :: STRESSN(20)             ! 20d n gr.factors               #          ! (From Integrate) 
    REAL    :: STRESSW(20)             ! 20d h2o gr.factors             #          ! (From Integrate) 
    !REAL    :: STRSWAD                 ! Stem reserves                  kg/ha      ! (From Integrate) !LPM 21MAY2015 The reserves distribution will not be included, it needs to be reviewed
    !REAL    :: STRSWT                  ! Stem reserves                  g/p        ! (From SeasInit)  !LPM 21MAY2015 The reserves distribution will not be included, it needs to be reviewed
    REAL    :: STWAD                   ! Stem structural weight         kg/ha      ! (From SeasInit)  
    REAL    :: STWADOUT                ! Stem weight for output         kg/ha      ! (From Output)    
    REAL    :: STWT                    ! Stem weight                    g/p        ! (From SeasInit)  
    REAL    :: STWTM                   ! Stem weight,maturity           g/p        ! (From SeasInit)  
    REAL    :: STWTP                   ! Stem weight potential          g/p        !LPM 23MAY2015 Added to keep the potential stem weight
    !REAL    :: SWFR                    ! Stem fraction,actual           #          ! (From Growth)  !LPM 05JUN2015 SWFR is not used    
    !REAL    :: SWFRN                   ! Stem fraction minimum          #          ! (From SeasInit)  !LPM 05JUN2015 SWFRN is not used 
    !REAL    :: SWFRNL                  ! Leaf number for min stem fr    #          ! (From SeasInit) !LPM 05JUN2015 SWFRNL is not used  
    !REAL    :: SWFRS                   ! Stem fraction,standard         #          ! (From SeasInit) !LPM 05JUN2015 SWFRS is not used 
    !REAL    :: SWFRX                   ! Stem fraction maximum          #          ! (From SeasInit) !LPM 05JUN2015 SWFRX is not used   
    !REAL    :: SWFRXL                  ! Leaf number for max stem fr    #          ! (From SeasInit)  !LPM 05JUN2015 SWFRXL is not used 
    REAL    :: SWP(0:20)               ! Soil water 'potential'         #          ! (From Growth) 
    REAL    :: SWPH                    ! Stem weight harvested          g/p        ! (From Growth)    
    REAL    :: SWPHC                   ! Stem wt harvested,cumulative   g/p        ! (From SeasInit)  
    REAL    :: SWPLTD                  ! Depth for average soil water   cm         ! (From SeasInit)  
    REAL    :: SWPLTH                  ! Upper limit on soil water,plt  %          ! (From SeasInit)  
    REAL    :: SWPLTL                  ! Lower limit on soil water,plt  %          ! (From SeasInit)  
    !REAL    :: SWFRPREV                ! Stem fraction,actual,previous  #          ! (From Output) !LPM 06JUN2015 SWFR is not used  
    REAL    :: SWPRTIP                 ! Soil water potential,root tip  #          ! (From Growth)    
    REAL    :: SWPSD                   ! Soil water potential at seed   #          ! (From Growth)    
    REAL    :: TBLSZ                   ! Base temp. for leaf develop  (˚C)         !LPM 09JUN2015
    REAL    :: TCAN                    ! Canopy temperature             C          ! (From SeasInit)  
    REAL    :: TCDIF                   ! Canopy temperature - air temp  C          ! (From Output)    
    INTEGER :: TDATANUM                ! Number of data from t-file     #          ! (From Output)    
    REAL    :: TDIFAV                  ! Temperature difference,can-air C          ! (From SeasInit)  
    REAL    :: TDIFNUM                 ! Temperature difference,# data  #          ! (From SeasInit)  
    REAL    :: TDIFSUM                 ! Temperature difference,sum     C          ! (From SeasInit)  
    INTEGER :: TFCOLNUM                ! T-file column number           #          ! (From Output)    
    REAL    :: TFB                     ! Temperature factor,branch 0-1  #          ! (From SeasInit) !LPM 19APR2016 Temperature factor for branching 
    REAL    :: TFD                     ! Temperature factor,development #          ! (From SeasInit)  
    INTEGER :: TFDAP                   ! T-file days after planting     #          ! (From Output)    
    INTEGER :: TFDAPCOL                ! T-file DAP column #            #          ! (From Output)    
    REAL    :: TFDF                    ! Temperature factor,dayl sens   #          ! (From Growth)    
    REAL    :: TFDNEXT                 ! Temperature factor,development #          ! (From Growth)    
    REAL    :: TFG                     ! Temperature factor,growth 0-1  #          ! (From SeasInit)  
    REAL    :: TFGEM                   ! Temperature factor,germ,emrg   #          ! (From Growth)    
    !REAL    :: TFLAW                   ! Temperature factor,lf area/wt  #          ! (From Growth)  !LPM 12DEC2016 Delete temperature, water and leaf position factors in SLA   
    REAL    :: TFLFLIFE                ! Temperature factor,leaf life   #          ! (From Growth)
    REAL    :: TFLFSIZE                ! Temperature factor,leaf size   #          ! LPM 12JUL2015 Added to consider a different optimum temperature for leaf size
    REAL    :: TFP                     ! Temperature factor,phs 0-1     #          ! (From SeasInit)  
    REAL    :: TFVAL                   ! T-file value                   #          ! (From Output)    
    INTEGER :: TIERNUM                 ! Tier of data in t-file         #          ! (From Output)    
    REAL    :: TIMENEED                ! Time needed to finish tier     fr         ! (From Growth)    
    REAL    :: TLCHC                   ! Cumulative N leached>planting  kg/ha      ! (From SeasInit)  
    INTEGER :: TLINENUM                ! Temporary var,# lines in tfile #          ! (From Output)    
    INTEGER :: TLPOS                   ! Position on temporary line     #          ! (From Output)    
    REAL    :: TMAXCAV                 ! Temperature,maximum,cycle av   C          ! (From SeasInit)  
    REAL    :: TMAXCC                  ! Temperature,max,cycle sum      C.d        ! (From SeasInit)  
    REAL    :: TMAXM                   ! Temperature maximum,monthly av C          ! (From SeasInit)  
    REAL    :: TMAXPAV(0:12)           ! Temperature,maximum,tier av    C          ! (From SeasInit)  
    REAL    :: TMAXPC                  ! Temperature,maximum,tier  sum  C          ! (From SeasInit)  
    REAL    :: TMAXSUM                 ! Temperature maximum,summed     C          ! (From SeasInit)  
    REAL    :: TMAXX                   ! Temperature max during season  C          ! (From SeasInit)  
    REAL    :: TMEAN                   ! Temperature mean (TMAX+TMIN/2) C          ! (From SeasInit)  
    REAL    :: TMEAN20                 ! Temperature mean over 20 days  C          ! (From Integrate) 
    REAL    :: TMEAN20P                ! Temperature mean,20 d>planting C          ! (From Integrate) 
    REAL    :: TMEAN20S                ! Temperature sum over 20 days   C          ! (From Integrate) 
    REAL    :: TMEANAV(0:12)           ! Temperature,mean,tier av       C          ! (From SeasInit)  
    REAL    :: TMEANCC                 ! Temperature,mean,cycle sum     C.d        ! (From SeasInit)  
    REAL    :: TMEAND(20)              ! Temperature mean,specific day  C          ! (From SeasInit)  
    REAL    :: TMEANE                  ! Temp mean,germination-emerge   C          ! (From SeasInit)  
    REAL    :: TMEANEC                 ! Temp sum,germination-emergence C          ! (From SeasInit)  
    REAL    :: TMEANG                  ! Temp mean,planting-germination C          ! (From SeasInit)  
    REAL    :: TMEANGC                 ! Temp sum,planting-germination  C          ! (From SeasInit)  
    REAL    :: TMEANNUM                ! Temperature means in sum       #          ! (From SeasInit)  
    REAL    :: TMEANPC                 ! Temperature,mean,tier sum      C          ! (From SeasInit)  
    REAL    :: TMEANSUM                ! Temperature means sum          #          ! (From SeasInit)  
    REAL    :: TMEANSURF               ! Temperature mean,soil surface  C          ! (From Growth)    
    REAL    :: TMINCAV                 ! Temperature,minimum,cycle av   C          ! (From SeasInit)  
    REAL    :: TMINCC                   ! Temperature,min,cycle sum     C.d        ! (From SeasInit)  
    REAL    :: TMINM                   ! Temperature minimum,monthly av C          ! (From SeasInit)  
    REAL    :: TMINN                   ! Temperature min during season  C          ! (From SeasInit)  
    REAL    :: TMINPAV(0:12)           ! Temperature,minimum,tier av    C          ! (From SeasInit)  
    REAL    :: TMINPC                  ! Temperature,minimum,tier sum   C          ! (From SeasInit)  
    REAL    :: TMINSUM                 ! Temperature minimum,summed     C          ! (From SeasInit)  
    REAL    :: TNAD                    ! Total nitrogen (tops+roots)    kg/ha      ! (From SeasInit)  
    REAL    :: TNAMM                   ! Total N at harvest,measured    kg/ha      ! (From Output)    
    INTEGER :: TNI                     ! Treatment number,initial value #          ! (From SeasInit)  
    REAL    :: TNOXC                   ! Cumulative N denitrified       kg/ha      ! (From SeasInit)  
    REAL    :: TOFIXC                  ! Cumulative inorganicN fixation kg/ha      ! (From SeasInit)  
    REAL    :: TOMIN                   ! Daily N mineralized            kg/ha      ! (From Integrate) 
    REAL    :: TOMINC                  ! Cumulative N mineralized       kg/ha      ! (From SeasInit)  
    REAL    :: TOMINFOMC               ! Cumulative mineralization,FOM  kg/ha      ! (From SeasInit)  
    REAL    :: TOMINSOM1C              ! Cumulative mineralization,SOM1 kg/ha      ! (From SeasInit)  
    REAL    :: TOMINSOM2C              ! Cumulative mineralization,SOM2 kg/ha      ! (From SeasInit)  
    REAL    :: TOMINSOM3C              ! Cumulative mineralization,SOM3 kg/ha      ! (From SeasInit)  
    REAL    :: TOMINSOMC               ! Cumulative mineralization,SOM  kg/ha      ! (From SeasInit)  
    REAL    :: TPAR                    ! Transmission,PAR,fraction      #          ! (From SeasInit)  
    REAL    :: TRATIO                  ! Function,relative tr rate      #          ! (From SeasInit)  
    REAL    :: TRBRG(4)                ! Temp response,development br   #          ! (From SeasInit) !LPM 19APR2016 new variable for the cardinal temperatures for branching
    REAL    :: TRDV1(4)                ! Temp response,development 1    #          ! (From SeasInit)  
    REAL    :: TRDV2(4)                ! Temp response,development 2    #          ! (From SeasInit)  
    REAL    :: TRDV3(4)                ! Temp response,leaf size Tb cul #          ! LPM 28feb15 new variable to change base temperature for leaf size
    REAL    :: TRDV4(4)                ! Temp response,leaf size Top spe#          ! LPM 12JUL2015 new variable to change optimum temperature for potential leaf size
    REAL    :: TRGEM(4)                ! Temp response,germ.emergence   #          ! (From SeasInit)  
    REAL    :: TRLDF                   ! Intermediate factor,new roots  #          ! (From Growth)    
    REAL    :: TRLFG(4)                ! Temp response,lf growth, br dev#          ! (From SeasInit) 18MAR15 
    REAL    :: TRLV                    ! Total root length density      /cm2       ! (From Growth)    
    REAL    :: TRPHS(4)                ! Temp response,photosynthesis   #          ! (From SeasInit)  
    REAL    :: TRWU                    ! Total water uptake             mm         ! (From Growth)    
    REAL    :: TSDEP                   ! Average temp in top 10 cm soil C          ! (From Growth)    
    REAL    :: TSRAD                   ! Transmission,SRAD,fraction     #          ! (From SeasInit)  
    REAL    :: TT                      ! Daily thermal time             C.d        ! (From SeasInit)  
    REAL    :: TT20                    ! Thermal time mean over 20 days C          ! (From SeasInit)  
    REAL    :: TT20S                   ! Thermal time sum over 20 days  C          ! (From Integrate) 
    REAL    :: TTB                     ! Daily thermal time, branching  C.d        ! LPM 21MAR15 to Change DU based on TTB
    REAL    :: TTCUM                   ! Cumulative thermal time        C.d        ! (From SeasInit)  
    REAL    :: TTCUMLS                 ! Cumulative thermal time lf sizeC.d        ! LPM 12JUL2015 new variable to change optimum temperature for potential leaf size
    REAL    :: TTCUMWS                 ! Cumulative TT with water stressC.d        ! LPM 31JUL2015 new variable to create an additional clock under water stress
    REAL    :: TTD(20)                 ! Thermal time,specific day      C          ! (From SeasInit)  
    REAL    :: TTGEM                   ! Daily thermal time,germ,emrg.  C.d        ! (From SeasInit)  
    REAL    :: TTL                     ! Daily thermal time, leaves     C.d        ! LPM 19APR2016 Added to separate the thermal clock for branching and leaf development
    REAL    :: TTLFLIFE                ! Daily thermal time,leaf life   C.d        ! (From Growth) 
    REAL    :: TTLFSIZE                ! Daily thermal time,leaf size   C.d        ! LPM 12JUL2015 Added to consider a different optimum temperature for leaf size
    REAL    :: TTNEED                  ! Thermal time to start new leaf #          ! (From Growth)    
    REAL    :: TTNEXT                  ! Thermal time,next tier         oCd        ! (From SeasInit)  
    REAL    :: TTOUT                   ! Thermal units output from func C.d        ! (From Growth)    
    INTEGER :: TVI1                    ! Temporary integer variable     #          ! (From RunInit)   
    INTEGER :: TVI2                    ! Temporary integer variable     #          ! (From RunInit)   
    INTEGER :: TVI3                    ! Temporary integer variable     #          ! (From RunInit)   
    INTEGER :: TVI4                    ! Temporary integer variable     #          ! (From RunInit)   
    REAL    :: TVR1                    ! Temporary real variable        #          ! (From SeasInit)  
    REAL    :: TVR2                    ! Temporary real variable        #          ! (From SeasInit)  
    REAL    :: TVR3                    ! Temporary real variable        #          ! (From SeasInit)  
    REAL    :: TVR4                    ! Temporary real variable        #          ! (From Growth)    
    REAL    :: TVR5                    ! Temporary real variable        #          ! (From Growth)    
    REAL    :: TVR6                    ! Temporary real variable        #          ! (From Output)    
    REAL    :: TWAD                    ! Total weight (tops+roots)      kg/ha      ! (From SeasInit)  
    INTEGER :: VALUEI                  ! Output from Getstri function   #          ! (From Output)    
    REAL    :: VALUER                  ! Output from Getstrr function   #          ! (From Output)    
    REAL    :: VANC                    ! Vegetative actual N conc       #          ! (From SeasInit)  
    REAL    :: VCNC                    ! Vegetative critical N conc     #          ! (From SeasInit)  
    INTEGER :: VERSION                 ! Version #                      #          ! (From RunInit)   
    INTEGER :: VERSIONCSCAS            ! Version #                      #          ! (From RunInit)   
    REAL    :: VMNC                    ! Vegetative minimum N conc      #          ! (From SeasInit)  
    REAL    :: VNAD                    ! Vegetative canopy nitrogen     kg/ha      ! (From SeasInit)  
    REAL    :: VNAM                    ! Vegetative N,mature            kg/ha      ! (From SeasInit)  
    REAL    :: VNAMM                   ! Vegetative N,mature,measured   kg/ha      ! (From SeasInit)  
    REAL    :: VNPCM                   ! Vegetative N %,maturity        %          ! (From SeasInit)  
    REAL    :: VNPCMM                  ! Vegetative N,mature,measure    %          ! (From SeasInit)  
    REAL    :: VPD                     ! Vapour pressure deficit        KPa        ! (From Growth)    
    REAL    :: VPDFP                   ! Vapour press deficit factor,phs #         ! (From SeasInit)  
    REAL    :: VWAD                    ! Vegetative canopy weight       kg/ha      ! (From SeasInit)  
    REAL    :: VWAM                    ! Vegetative canopy wt,maturity  kg/ha      ! (From SeasInit)  
    REAL    :: VWAMERR                 ! Vegetative wt,error            %          ! (From Output)    
    REAL    :: VWAMM                   ! Veg wt,mature,measured         kg/ha      ! (From SeasInit)  
    REAL    :: WAVR                    ! Water available/demand         #          ! (From Integrate) 
    REAL    :: WFEU                    ! Water factor,evaptp,upper      #          ! (From SeasInit)  
    REAL    :: WFG                     ! Water factor,growth 0-1        #          ! (From SeasInit)  
    REAL    :: WFGCAV                  ! Water factor,growth,av.,cylcle #          ! (From Integrate) 
    REAL    :: WFGCC                   ! H20 factor,growh,cycle sum     #          ! (From SeasInit)  
    REAL    :: WFGE                    ! Water factor,germ,emergence    #          ! (From Growth)    
    REAL    :: WFGEM                   ! Water factor,germ,emergence    #          ! (From SeasInit)  
    REAL    :: WFGL                    ! Water factor,growth,lower      #          ! (From SeasInit)  
    REAL    :: WFGPAV(0:12)            ! Water factor,growth,average    #          ! (From SeasInit)  
    REAL    :: WFGPC                   ! Water factor,growth,cumulative #          ! (From SeasInit)  
    REAL    :: WFGU                    ! Water factor,growth,upper      #          ! (From SeasInit)  
    !REAL    :: WFLAW                   ! Water factor,leaf area/weight  #          ! (From Growth)    !LPM 12DEC2016 Delete temperature, water and leaf position factors in SLA
    REAL    :: WFNU                    ! Water factor,N uptake          #          ! (From Growth)    
    REAL    :: WFP                     ! Water factor,photosynthsis 0-1 #          ! (From SeasInit)  
    REAL    :: WFPCAV                  ! Water factor,phs,av 0-1,cycle  #          ! (From SeasInit)  
    REAL    :: WFPCC                   ! H20 factor,phs,cycle sum       #          ! (From SeasInit)  
    REAL    :: WFPL                    ! Water factor,phs,lower         #          ! (From SeasInit)  
    REAL    :: WFPPAV(0:12)            ! Water factor,phs,average 0-1   #          ! (From SeasInit)  
    REAL    :: WFPPC                   ! Water factor,phs,cumulative    #          ! (From SeasInit)  
    REAL    :: WFPU                    ! Water factor,phs,upper         #          ! (From SeasInit)  
    REAL    :: WFRG                    ! Water factor,root growth,0-1   #          ! (From Growth)    
    REAL    :: WFRTG                   ! Water factor,root gr           #          ! (From SeasInit)  
    REAL    :: WFSU                    ! Water fac,senescence,upper 0-1 #          ! (From SeasInit)  
    INTEGER :: WSDAYS                  ! Water stress days              #          ! (From SeasInit)  
    REAL    :: WTDEP                   ! Water table depth              cm         ! (From SeasInit)  
    REAL    :: WUPR                    ! Water pot.uptake/demand        #          ! (From SeasInit)  
    REAL    :: WUPRD(20)               ! Water pot.uptake/demand,ind dy #          ! (From Integrate) 
    REAL    :: XDEP                    ! Depth to bottom of layer       cm         ! (From Growth)    
    REAL    :: XDEPL                   ! Depth to top of layer          cm         ! (From Growth)    
    REAL    :: XMIN                    ! Minimum NO3,NH4-N in soil layr kg/ha      ! (From Growth)    
    INTEGER :: YEARCOL                 ! Column number for year data    #          ! (From Output)    
    INTEGER :: YEARDOY                 ! Year+Doy (7digits)             #          ! (From SeasInit)  
    INTEGER :: YEARDOYHARF             ! Harvest year+doy,fixed         #          ! (From SeasInit)  
    INTEGER :: YEARDOYPREV             ! Year+Doy (7digits),previous    #          ! (From Output)           
    INTEGER :: YEARM                   ! Year of measurement            #          ! (From Output)    
    INTEGER :: YEARSIM                 ! Year+Doy for simulation start  #          ! (From RunInit)   
    
    CHARACTER (LEN=128) ARG           ! Argument component                        ! (From RunInit)  
    CHARACTER (LEN=6)   BRSTAGEC      ! Branching stage                           ! (From Output)  
    CHARACTER (LEN=6)   CAIC          ! Canopy area index                         ! (From Output)  
    CHARACTER (LEN=6)   CANHTC        ! Canopy height                             ! (From Output)  
    CHARACTER (LEN=120) CFGDFILE      ! Configuration directory+file              ! (From RunInit)  
    CHARACTER (LEN=1)   CFLAFLF       ! Control flag,assim.on lf area             ! (From RunInit)  
    CHARACTER (LEN=1)   CFLFAIL       ! Control flag for failure                  ! (From RunInit)  
    CHARACTER (LEN=1)   CFLHAR        ! Control flag for final harvest            ! (From Integrate)
    CHARACTER (LEN=1)   CFLHARMSG     ! Control flag,harvest message              ! (From RunInit)  
    CHARACTER (LEN=1)   CFLLFLIFE     ! Control flag,leaf life D,P,C              ! (From RunInit)  
    CHARACTER (LEN=1)   CFLSDRSMSG    ! Control flag,seed reserves msg            ! (From RunInit)  
    CHARACTER (LEN=1)   CFLTFG        ! Control flag,temp.on lf area              ! (From RunInit)  
    CHARACTER (LEN=10)  CNCHAR        ! Crop component (multicrop)                ! (From RunInit)  
    CHARACTER (LEN=2)   CNCHAR2       ! Crop component (multicrop)                ! (From RunInit)  
    CHARACTER (LEN=2)   CROP          ! Crop identifier (ie. CS)                  ! (From SeasInit) 
    CHARACTER (LEN=2)   CROPPREV      ! Crop identifier,previous run              ! (From RunInit)  
    CHARACTER (LEN=93)  CUDIRFLE      ! Cultivar directory+file                   ! (From RunInit)  
    CHARACTER (LEN=93)  CUDIRFLPREV   ! Cultivar directory+file,prev              ! (From RunInit)  
    CHARACTER (LEN=12)  CUFILE        ! Cultivar file                             ! (From SeasInit) 
    CHARACTER (LEN=10)  DAPCHAR       ! DAP in character form                     ! (From Output)   
    CHARACTER (LEN=6)   DAPWRITE      ! DAP character string -> output            ! (From Output)   
    CHARACTER (LEN=93)  ECDIRFLE      ! Ecotype directory+file                    ! (From SeasInit) 
    CHARACTER (LEN=93)  ECDIRFLPREV   ! Ecotype directory+file,prev               ! (From RunInit)  
    CHARACTER (LEN=12)  ECFILE        ! Ecotype filename                          ! (From SeasInit) 
    CHARACTER (LEN=6)   ECONO         ! Ecotype code                              ! (From SeasInit) 
    CHARACTER (LEN=6)   ECONOPREV     ! Ecotype code,previous                     ! (From RunInit)  
    CHARACTER (LEN=1)   EMFLAG        ! Flag,emergence Y/N                        ! (From RunInit)  
    CHARACTER (LEN=60)  ENAME         ! Experiment description                    ! (From SeasInit) 
    CHARACTER (LEN=1)   ESTABLISHED   ! Flag,crop establishment Y/N               ! (From SeasInit) 
    CHARACTER (LEN=14)  EVHEADER      ! Evaluater.out header                      ! (From Output)   
    CHARACTER (LEN=10)  EXCODE        ! Experiment code/name                      ! (From RunInit)  
    CHARACTER (LEN=10)  EXCODEPREV    ! Previous experiment code/name             ! (From Output)  
    CHARACTER (LEN=80)  FAPPLINE(30)  ! Fertilizer application details            ! (From SeasInit) 
    CHARACTER (LEN=120) FILEA         ! Name of A-file                            ! (From SeasInit) 
    CHARACTER (LEN=107) FILEADIR      ! Name of A-file directory                  ! (From SeasInit) 
    CHARACTER (LEN=120) FILEIO        ! Name of input file,after check            ! (From RunInit)  
    CHARACTER (LEN=3)   FILEIOT       ! Type of input file                        ! (From RunInit)  
    CHARACTER (LEN=120) FILENEW       ! Temporary name of file                    ! (From SeasInit) 
    CHARACTER (LEN=120) FILET         ! Name of T-file                            ! (From Output)   
    CHARACTER (LEN=120) FILEX         ! Name of X-file                            ! (From SeasInit) 
    CHARACTER (LEN=1)   FNAME         ! File name switch (N->standard)            ! (From RunInit)  
    CHARACTER (LEN=120) FNAMEERA      ! File name,A-errors                        ! (From RunInit)  
    CHARACTER (LEN=120) FNAMEERT      ! File name,T-errors                        ! (From RunInit)  
    CHARACTER (LEN=120) FNAMEEVAL     ! File name,evaluate outputs                ! (From RunInit)  
    CHARACTER (LEN=120) FNAMELEAVES   ! File name,leaves outputs                  ! (From RunInit)  
    CHARACTER (LEN=120) FNAMEMEAS     ! File name,measured outputs                ! (From RunInit)  
    CHARACTER (LEN=120) FNAMEOV       ! File name,overview outputs                ! (From RunInit)  
    CHARACTER (LEN=120) FNAMEPHASES   ! File name,phases outputs                  ! (From RunInit)  
    CHARACTER (LEN=120) FNAMEPHENOLM  ! File name,phenology measured              ! (From RunInit)  
    CHARACTER (LEN=120) FNAMEPHENOLS  ! File name,phenology outputs               ! (From RunInit)  
    CHARACTER (LEN=120) FNAMEPREM     ! File name,responses,measured              ! (From RunInit)  
    CHARACTER (LEN=120) FNAMEPRES     ! File name,responses,simulated             ! (From RunInit)  
    CHARACTER (LEN=120) FNAMEPSUM     ! File name,plant summary                   ! (From RunInit)  
    CHARACTER (LEN=35)  GENFLCHK      ! Genotype file name for check              ! (From RunInit)  
    CHARACTER (LEN=1)   GROUP         ! Flag for type of group                    ! (From Output)  
    CHARACTER (LEN=6)   HIAMCHAR      ! Harvest indx,at harvest                   ! (From Output)  
    CHARACTER (LEN=6)   HIAMMCHAR     ! Harvest indx,har,measured                 ! (From Output)  
    CHARACTER (LEN=6)   HINDC         ! Harvest index,nitrogen                    ! (From Output)  
    CHARACTER (LEN=6)   HINMCHAR      ! Harvest N index,at harvest                ! (From Output)  
    CHARACTER (LEN=6)   HINMMCHAR     ! Harvest N index,mat,measured              ! (From Output)  
    CHARACTER (LEN=6)   HNPCMCHAR     ! Harvest product N %,maturity              ! (From Output)  
    CHARACTER (LEN=6)   HNPCMMCHAR    ! Harvest product N%,mature,meas            ! (From Output)  
    CHARACTER (LEN=1)   HOP(HANUMX)   ! Harvest operation code                    ! (From SeasInit) 
    CHARACTER (LEN=2)   HPROD         ! Code,harvested part of plant              ! (From SeasInit) 
    !CHARACTER (LEN=6)   HWUDC         ! Harvest wt/unit                           ! (From Output)  
    CHARACTER (LEN=6)   HWUMCHAR      ! Harvest wt/unit                           ! (From SeasInit) 
    CHARACTER (LEN=6)   HWUMMCHAR     ! Harvest wt/unit,mat,measured              ! (From SeasInit) 
    CHARACTER (LEN=1)   IDETD         ! Control flag,screen outputs               ! (From RunInit)  
    CHARACTER (LEN=1)   IFERI         ! Fertilizer switch (A,F,R,D,N)             ! (From SeasInit) 
    CHARACTER (LEN=1)   IHARI         ! Control flag,harvest                      ! (From SeasInit) 
    CHARACTER (LEN=1)   IPLTI         ! Code for planting date method             ! (From SeasInit) 
    CHARACTER (LEN=1)   ISWNITEARLY   ! Control flag,N stress early               ! (From Growth)   
    CHARACTER (LEN=1)   ISWWATCROP    ! Control flag,H20 stress,crop              ! (From RunInit)  
    CHARACTER (LEN=1)   ISWWATEARLY   ! Control flag,H20 stress early             ! (From Growth)   
    CHARACTER (LEN=6)   LAIC          ! Leaf area index                           ! (From Output)  
    CHARACTER (LEN=6)   LAIPRODC      ! Leaf area index produced                  ! (From Output)  
    CHARACTER (LEN=6)   LAIXCHAR      ! Leaf area index,maximum                   ! (From Output)  
    CHARACTER (LEN=6)   LAIXMCHAR     ! Leaf area index,max,measured              ! (From Output)  
    CHARACTER (LEN=6)   LAPC          ! Area of cohort of leaves                  ! (From Output)  
    CHARACTER (LEN=6)   LAPOTXC       ! Leaf area,potential                       ! (From Output)  
    CHARACTER (LEN=6)   LAPSC         ! Senesced area,cohort of leaves            ! (From Output)  
    CHARACTER (LEN=6)   LATL2C        ! Leaf area,h2o,N,temp stress               ! (From Output)  
    CHARACTER (LEN=6)   LATL3C        ! Leaf area,assimlate shortage              ! (From Output)  
    CHARACTER (LEN=6)   LATL4C        ! Leaf area,assimlate shortage              ! (From Output)  
    CHARACTER (LEN=6)   LATLC         ! Leaf area,no stress                       ! (From Output)  
    CHARACTER (LEN=80)  LINESTAR      ! Group header line (with star)             ! (From Output)   
    CHARACTER (LEN=80)  LINESTAR2     ! Group header line (with star)             ! (From Output)   
    CHARACTER (LEN=180) LINET         ! Line from T-file                          ! (From Output)   
    CHARACTER (LEN=4)   MEDEV         ! Switch,development control                ! (From RunInit)  
    CHARACTER (LEN=1)   MEEVP         ! Switch,potential evapot method            ! (From SeasInit) 
    CHARACTER (LEN=1)   MEEXP         ! Switch,experimental method = E            ! (From SeasInit) 
    CHARACTER (LEN=1)   MEPHO         ! Switch,photosynthesis method              ! (From SeasInit) 
    CHARACTER (LEN=3)   MERNU         ! Switch,root N uptake method               ! (From RunInit)  
    CHARACTER (LEN=78)  MESSAGE(10)   ! Messages for Warning.out                  ! (From SeasInit) 
    CHARACTER (LEN=1)   MEWNU         ! Switch,root H2O/N uptake method           ! (From SeasInit) 
    CHARACTER (LEN=8)   MODEL         ! Name of model                             ! (From RunInit)  
    CHARACTER (LEN=8)   MODNAME       ! Name of module                            ! (From RunInit)  
    CHARACTER (LEN=3)   MONTH         ! Month                                     ! (From Integrate)
    CHARACTER (LEN=3)   OUT           ! Output file extension                     ! (From RunInit)  
    CHARACTER (LEN=79)  OUTHED        ! Output file heading                       ! (From SeasInit) 
    CHARACTER (LEN=12)  OUTPG         ! Growth output file code                   ! (From RunInit)  
    CHARACTER (LEN=12)  OUTPG2        ! Growth output file2 code                  ! (From RunInit)  
    CHARACTER (LEN=12)  OUTPGF        ! Growth factors file2 code                 ! (From RunInit)  
    CHARACTER (LEN=12)  OUTPN         ! GrowthN output file code                  ! (From RunInit)  
    CHARACTER (LEN=80)  PATHCR        ! Path to genotype (CUL) files              ! (From SeasInit) 
    CHARACTER (LEN=80)  PATHEC        ! Path to genotype (ECO) files              ! (From SeasInit) 
    CHARACTER (LEN=80)  PATHSP        ! Path to genotype (SPE) files              ! (From SeasInit) 
    CHARACTER (LEN=1)   PLME          ! Planting method (code)                    ! (From SeasInit) 
    CHARACTER (LEN=2)   PPSEN         ! Code,photoperiod sensitivity              ! (From SeasInit) 
    CHARACTER (LEN=5)   PSABV(0:PSX)  ! Principal stage abbreviation              ! (From SeasInit) !LPM 28MAR15 Starting from 0 PSX
    CHARACTER (LEN=5)   PSABVO(0:PSX) ! Principal stage abv,output                ! (From SeasInit) !LPM 28MAR15 Starting from 0 PSX
    CHARACTER (LEN=13)  PSNAME(0:PSX) ! Principal stage names                     ! (From SeasInit) !LPM 28MAR15 Starting from 0 PSX
    CHARACTER (LEN=1)   PSTYP(0:PSX)  ! Principal stage type                      ! (From SeasInit) !LPM 28MAR15 Starting from 0 PSX
    CHARACTER (LEN=25)  RUNNAME       ! Run title                                 ! (From SeasInit) 
    CHARACTER (LEN=8)   RUNRUNI       ! Run+internal run number                   ! (From SeasInit) 
    CHARACTER (LEN=6)   SDWTC         ! Seed weight                               ! (From Output)  
    CHARACTER (LEN=1)   SEASENDOUT    ! Season end outputs flag                   ! (From RunInit)  
    CHARACTER (LEN=6)   SENN0C        ! Senesced N added to litter                ! (From Output)  
    CHARACTER (LEN=6)   SENNSC        ! Senesced N added to soil                  ! (From Output)  
    CHARACTER (LEN=6)   SENROOTC      ! Senesced OM,soil                          ! (From Output)  
    CHARACTER (LEN=6)   SENTOPLITTERAC! Senesced OM added to surface              ! (From Output)  
    CHARACTER (LEN=64)  SPDIRFLE      ! Species directory+file                    ! (From SeasInit) 
    CHARACTER (LEN=64)  SPDIRFLPREV   ! Species directory+file,last               ! (From RunInit)  
    CHARACTER (LEN=12)  SPFILE        ! Species filename                          ! (From SeasInit) 
    CHARACTER (LEN=6)   TCHAR         ! Temporary character string                ! (From Output)  
    CHARACTER (LEN=6)   THEAD(20)     ! T-file headings                           ! (From Output)   
    CHARACTER (LEN=1)   TIERNUMC      ! Tier number in t-file                     ! (From Output)  
    CHARACTER (LEN=10)  TL10          ! Temporary line                            ! (From RunInit)  
    CHARACTER (LEN=254) TLINEGRO      ! Temporary line from GRO file              ! (From Output)   
    CHARACTER (LEN=180) TLINET        ! Temporary line from T-file                ! (From Output)   
    CHARACTER (LEN=180) TLINETMP      ! Temporary line                            ! (From Output)   
    CHARACTER (LEN=25)  TNAME         ! Treatment name                            ! (From SeasInit) 
    CHARACTER (LEN=10)  TNCHAR        ! Treatment number,characters               ! (From Output)   
    CHARACTER (LEN=40)  TRUNNAME      ! Treatment+run composite name              ! (From SeasInit) 
    CHARACTER (LEN=6)   VARNO         ! Variety identification code               ! (From SeasInit) 
    CHARACTER (LEN=6)   VARNOPREV     ! Variety identification code               ! (From RunInit)  
    CHARACTER (LEN=6)   VNPCMCHAR     ! Vegetative N %,maturity                   ! (From Output)  
    CHARACTER (LEN=6)   VNPCMMCHAR    ! Vegetative N,mature,measured              ! (From Output)  
    CHARACTER (LEN=16)  VRNAME        ! Variety name                              ! (From SeasInit) 

    LOGICAL             FEXIST        ! File existence indicator                  ! (From RunInit)
    LOGICAL             FEXISTA       ! File A existence indicator
    LOGICAL             FEXISTT       ! File T existence indicator
    LOGICAL             FFLAG         ! Temp file existance indicator             ! (From RunInit)
    LOGICAL             FFLAGEC       ! Temp file existance indicator             ! (From SeasInit)
    LOGICAL             FOPEN         ! File open indicator                       ! (From RunInit)

    INTEGER ::          IOCHECK       ! File open indicator                       ! (From Output)

    ! Arrays for passing variables to OPSUM subroutine, CSM model only             ! (From Output)
    INTEGER,      PARAMETER :: SUMNUM = 37                                         ! (From Output)
    CHARACTER*5,  DIMENSION(SUMNUM) :: LABEL                                       ! (From Output)
    REAL,         DIMENSION(SUMNUM) :: VALUE                                       ! (From Output)
    
        !INTEGER CN          , DOY         , FROP        , ON          , RN          , RUN         , SN          , TN          
        !INTEGER YEAR
        !INTEGER TVILENT                                                                       ! Integer function call
        !
        !CHARACTER(LEN=1)   IDETL       , ISWNIT      , ISWWAT      , RNMODE      
        !CHARACTER(LEN=120) FILEIOIN    
        !CHARACTER(LEN=10)  TL10FROMI                                                          ! Character function call

    End Module YCA_First_Trans_m
    
    
    subroutine clear_YCA_First_Trans_m()
        USE YCA_First_Trans_m
        implicit none
        plant=Node_type_constructor()
        
        ARGLEN=0
        BR=0
        BRDAE(PSX)=0
        BROLDESTA=0
        CDAYS=0
        CNI=0
        COLNUM=0
        CTRNUMPD=0
        CWADCOL=0
        DAE=0
        DAG=0
        DALSMAX=0
        DAP=0
        DAS=0
        DATE=0
        DATECOL=0
        DCDAT(DCNX)=0
        DCTAR(DCNX)=0
        DIDAT(DINX)=0
        DIDOY(DINX)=0
        DOM=0
        DOYCOL=0
        DYNAMICPREV=0
        EDAP=0
        EDAPM=0
        EDATM=0
        EDATMX=0
        EMDATERR=0
        EVALOUT=0
        EVHEADNM=0
        EVHEADNMMAX=0
        EYEARDOY=0
        FAPPNUM=0
        FDAY(200)=0
        FLDAP=0
        GDAP=0
        GDAPM=0
        GDATM=0
        GSTDCOL=0
        GYEARDOY=0
        HADOY=0
        HANUM=0
        HAYEAR=0
        HDAY=0
        HDOYF=0
        HDOYL=0
        HFIRST=0
        HIADCOL=0
        HLAST=0
        HNUMACOL=0
        HNUMBER=0
        HNUMECOL=0
        HSTG=0
        HWADCOL=0
        HWTUCOL=0
        HYEAR=0
        HYEARDOY(HANUMX)=0
        HYEARF=0
        HYEARL=0
        HYRDOY(HANUMX)=0
        I=0
        IDETGNUM=0
        KEYPS(0:KEYSTX)=0
        KEYPSNUM=0
        L=0
        L1=0
        L2=0
        LAIDCOL=0
        LENDIS=0
        LENENAME=0
        LENGROUP=0
        LENLINE=0
        LENLINESTAR=0
        LENRNAME=0
        LENTNAME=0
        LF=0
        LNUMCOL=0
        LNUMSG=0
        LNUMSOLDESTA=0
        LRTIP=0
        LSEED=0
        MDAP=0
        MDAPM=0
        MDATERR=0
        MDATM=0
        MDOY=0
        NG=0
        NLAYRROOT=0
        NOUTPG=0
        NOUTPG2=0
        NOUTPGF=0
        NOUTPN=0
        NSDAYS=0
        ONI=0
        OUTCHOICE=0
        OUTCOUNT=0
        PATHL=0
        PDATE=0
        PDAYS(0:12)=0
        PGDAP=0
        PGROCOL(20)=0
        PLDAY=0
        PLDAYTMP=0
        PLTOHARYR=0
        PLYEAR=0
        PLYEARDOYPREV=0
        PLYEARDOYT=0
        PLYEARREAD=0
        PLYEARTMP=0
        PSDAP=0
        PSDAPM=0
        PSDAT=0
        PSDATM=0
        PSIDAP=0
        PSIDAPM=0
        PSIDATERR=0
        PSNUM=0
        PWDINF=0
        PWDINL=0
        PWDOYF=0
        PWDOYL=0
        PWYEARF=0
        PWYEARL=0
        RNI=0
        RPCOL=0
        RTSLXDATE=0
        RUNCRP=0
        SHDAP=0
        SHDAT=0
        SNI=0
        STARNUM=0
        STARNUMM=0
        STARNUMO=0
        STEPNUM=0
        STGEDAT=0
        TDATANUM=0
        TFCOLNUM=0
        TFDAP=0
        TFDAPCOL=0
        TIERNUM=0
        TLINENUM=0
        TLPOS=0
        TNI=0
        TVI1=0
        TVI2=0
        TVI3=0
        TVI4=0
        VALUEI=0
        VERSION=0
        VERSIONCSCAS=0
        WSDAYS=0
        YEARCOL=0
        YEARDOY=0
        YEARDOYHARF=0
        YEARDOYPREV=0
        YEARM=0
        YEARSIM=0
        AH2OPROFILE=0.0
        AH2OROOTZONE=0.0
        ALBEDO=0.0
        AMTNIT=0.0
        ANDEM=0.0
        ANFER(200)=0.0
        AREAPOSSIBLE=0.0
        AREAPOSSIBLEN=0.0
        AVGSW=0.0
        b_slope_lsize=0.0
        BASELAYER=0.0
        Bcount=0.0
        BRFX(0:PSX)=0.0
        BRNUMSH(0:PSX)=0.0
        BRNUMSHM=0.0
        BRNUMST(0:PSX)=0.0
        BRNUMSTPREV(0:PSX)=0.0
        BRSTAGEPREV=0.0
        BRSTAGETMP=0.0
        CANHTG=0.0
        CARBOADJ=0.0
        CARBOBEG=0.0
        CARBOBEGI=0.0
        CARBOBEGIA=0.0
        CARBOBEGM=0.0
        CARBOBEGR=0.0
        CARBOC=0.0
        CARBOEND=0.0
        CARBOR=0.0
        CARBOT=0.0
        CNAD=0.0
        CNADPREV=0.0
        CNADSTG(20)=0.0
        CNAM=0.0
        CNAMERR=0.0
        CNAMM=0.0
        CNCTMP=0.0
        CNPCHM=0.0
        CO2AIR=0.0
        CO2CAV=0.0
        CO2CC=0.0
        CO2COMPC=0.0
        CO2EX=0.0
        CO2F(10)=0.0
        CO2FP=0.0
        CO2FPI=0.0
        CO2MAX=0.0
        CO2PAV(0:12)=0.0
        CO2PC=0.0
        CO2RF(10)=0.0
        CRWT=0.0
        CRWTM=0.0
        CRWTP=0.0
        CUMDEP=0.0
        CUMDU=0.0
        CUMSW=0.0
        CWAD=0.0
        CWADPREV=0.0
        CWADSTG(20)=0.0
        CWADT=0.0
        CWAHC=0.0
        CWAHCM=0.0
        CWAM=0.0
        CWAMERR=0.0
        CWAMM=0.0
        CWAN(HANUMX)=0.0
        DABR=0.0
        DAGERM=0.0
        DALS=0.0
        DAWWP=0.0
        DAYLCAV=0.0
        DAYLCC=0.0
        DAYLPAV(0:12)=0.0
        DAYLPC=0.0
        DAYLPREV=0.0
        DAYLS(0:10)=0.0
        DAYLST(0:12)=0.0
        DAYSUM=0.0
        DCDUR(DCNX)=0.0
        DCFAC(DCNX)=0.0
        DF=0.0
        DFNEXT=0.0
        DFOUT=0.0
        DFPE=0.0
        DIFFACR(DINX)=0.0
        DIGFAC(DINX)=0.0
        DLAYRTMP(20)=0.0
        DMP_EP=0.0
        DMP_ET=0.0
        DMP_Irr=0.0
        DMP_NApp=0.0
        DMP_NUpt=0.0
        DMP_Rain=0.0
        DRAINC=0.0
        DTRY=0.0
        DU=0.0
        DUNEED=0.0
        DUPHASE=0.0
        DUPNEXT=0.0
        DUTOMSTG=0.0
        EARLYN=0.0
        EARLYW=0.0
        EDAPFR=0.0
        EDAYFR=0.0
        EMRGFR=0.0
        EMRGFRPREV=0.0
        EOC=0.0
        EOEBUD=0.0
        EOEBUDC=0.0
        EOEBUDCRP=0.0
        EOEBUDCRPC=0.0
        EOEBUDCRPCO2=0.0
        EOEBUDCRPCO2C=0.0
        EOEBUDCRPCO2H2O=0.0
        EOEBUDCRPCO2H2OC=0.0
        EOMPCRP=0.0
        EOMPCRPC=0.0
        EOMPCRPCO2=0.0
        EOMPCRPCO2C=0.0
        EOMPCRPCO2H2O=0.0
        EOMPCRPCO2H2OC=0.0
        EOMPEN=0.0
        EOMPENC=0.0
        EOPEN=0.0
        EOPENC=0.0
        EOPT=0.0
        EOPTC=0.0
        EPCC=0.0
        EPPC(0:12)=0.0
        EPSRATIO=0.0
        ERRORVAL=0.0
        ETCC=0.0
        ETPC(0:12)=0.0
        FAC(20)=0.0
        FERNITPREV=0.0
        FLN=0.0
        FNH4=0.0
        FNO3=0.0
        FSOILH2O=0.0
        FSOILN=0.0
        GDAPFR=0.0
        GDAYFR=0.0
        GEDAYSE=0.0
        GEDAYSG=0.0
        GERMFR=0.0
        GESTAGE=0.0
        GESTAGEPREV=0.0
        GEUCUM=0.0
        GROCR=0.0
        GROCRADJ=0.0
        GROCRFR=0.0
        GROCRP=0.0
        GROLF=0.0
        GROLFADJ=0.0
        GROLFP=0.0
        GROLS=0.0
        GROLSA=0.0
        GROLSP=0.0
        GROLSRS=0.0
        GROLSRT=0.0
        GROLSRTN=0.0
        GROLSSD=0.0
        GROLSSEN=0.0
        GRORP=0.0
        GRORS=0.0
        GROST=0.0
        GROSTADJ=0.0
        GROSTCR=0.0
        GROSTCRP=0.0
        GROSTCRPSTORE=0.0
        GROSTP=0.0
        GrP_EP=0.0
        GRP_ET=0.0
        GrP_Irr=0.0
        GrP_NApp=0.0
        GrP_NUpt=0.0
        GRP_Rain=0.0
        H2OA=0.0
        H2OCF=0.0
        H2OROOTZONE=0.0
        HAFR=0.0
        HAMT(HANUMX)=0.0
        HAWAD=0.0
        HBPC(HANUMX)=0.0
        HBPCF=0.0
        HDUR=0.0
        HIAD=0.0
        HIADT=0.0
        HIAM=0.0
        HIAMERR=0.0
        HIAMM=0.0
        HIAMMTMP=0.0
        HIND=0.0
        HINM=0.0
        HINMM=0.0
        HNAD=0.0
        HNAM=0.0
        HNAMERR=0.0
        HNAMM=0.0
        HNC=0.0
        HNPCM=0.0
        HNPCMERR=0.0
        HNPCMM=0.0
        HNUMAERR=0.0
        HNUMAM=0.0
        HNUMAMM=0.0
        HNUMAT=0.0
        HNUMET=0.0
        HNUMGERR=0.0
        HNUMGM=0.0
        HNUMGMM=0.0
        HNUMPM=0.0
        HNUMPMM=0.0
        HPC(HANUMX)=0.0
        HPCF=0.0
        HPRODN=0.0
        HSTAGE=0.0
        HWAD=0.0
        HWADT=0.0
        HWAHERR=0.0
        HWAHM=0.0
        HWAM=0.0
        HWAMM=0.0
        HWUM=0.0
        HWUMERR=0.0
        HWUMM=0.0
        HWUMYLD=0.0
        HWUT=0.0
        HYAMM=0.0
        ICWD=0.0
        IRRAMTC=0.0
        ISOILH2O=0.0
        ISOILN=0.0
        KCANI=0.0
        KEPI=0.0
        LA1S=0.0
        LAI=0.0
        LAIA=0.0
        LAIPREV=0.0
        LAIPROD=0.0
        LAISTG(0:20)=0.0
        LAIX=0.0
        LAIXERR=0.0
        LAIXM=0.0
        LAIXT=0.0
        LAIXX=0.0
        LANC=0.0
        LANCRS=0.0
        LAPD=0.0
        LAPH=0.0
        LAPHC=0.0
        LAPSTMP=0.0
        LAWL(2)=0.0
        LAWS=0.0
        LAWTR=0.0
        LAWTS=0.0
        LAXS=0.0
        Lcount=0.0
        LEAFN=0.0
        LEAFNEXCESS=0.0
        LFWT=0.0
        LFWTM=0.0
        LLIFA=0.0
        LLIFATT=0.0
        LLIFG=0.0
        LLIFGTT=0.0
        LLIFS=0.0
        LLIFSTT=0.0
        LLIFX=0.0
        LLIGP=0.0
        LLNAD=0.0
        LLOSA=0.0
        LLWAD=0.0
        LLWADOUT=0.0
        LNCGL=0.0
        LNCGU=0.0
        LNCM=0.0
        LNCMN(0:1)=0.0
        LNCPL=0.0
        LNCPU=0.0
        LNCR=0.0
        LNCSEN=0.0
        LNCX=0.0
        LNCXS(0:1)=0.0
        LNDEM=0.0
        LNDEMG=0.0
        LNDEMTU=0.0
        LNPCMN(0:1)=0.0
        LNPCS(0:1)=0.0
        LNPH=0.0
        LNPHC=0.0
        LNSLP=0.0
        LNUM=0.0
        LNUMEND=0.0
        LNUMG=0.0
        LNUMNEED=0.0
        LNUMPREV=0.0
        LNUMSERR=0.0
        LNUMSIMSTG(0:PSX)=0.0
        LNUMSIMTOSTG(0:PSX)=0.0
        LNUMSM=0.0
        LNUMSMM=0.0
        LNUMSTG(20)=0.0
        LNUMT=0.0
        LNUMTMP=0.0
        LNUMTOSTG(0:PSX)=0.0
        LNUSE(0:3)=0.0
        LPEAI=0.0
        LPEAW=0.0
        LPEFR=0.0
        LPERSWT=0.0
        LPEWAD=0.0
        LSENI=0.0
        LSNUM(HANUMX)=0.0
        LSTAGE=0.0
        LSWT(HANUMX)=0.0
        LWLOS=0.0
        LWPH=0.0
        LWPHC=0.0
        MDAPFR=0.0
        MDAT=0.0
        MDATT=0.0
        MDAYFR=0.0
        MJPERE=0.0
        NCRG=0.0
        NDEM2=0.0
        NDEMMN=0.0
        NFG=0.0
        NFGCAV=0.0
        NFGCC=0.0
        NFGL=0.0
        NFGPAV(0:12)=0.0
        NFGPC=0.0
        NFGU=0.0
        NFPCAV=0.0
        NFPCC=0.0
        NFPL=0.0
        NFPPAV(0:12)=0.0
        NFPPC=0.0
        NFPU=0.0
        NFRG=0.0
        NFSU=0.0
        NH4CF=0.0
        NH4MN=0.0
        NLABPC=0.0
        NLLG=0.0
        NO3CF=0.0
        NO3MN=0.0
        NODLT=0.0
        NODWT=0.0
        NPOOLL=0.0
        NPOOLR=0.0
        NPOOLS=0.0
        NTUPF=0.0
        NUF=0.0
        NULEFT=0.0
        NUPAC=0.0
        NUPACM=0.0
        NUPAD=0.0
        NUPAP=0.0
        NUPAPCRP=0.0
        NUPAPCSM=0.0
        NUPAPCSM1=0.0
        NUPC=0.0
        NUPD=0.0
        NUPRATIO=0.0
        NUSEFAC=0.0
        PARFC=0.0
        PARI=0.0
        PARI1=0.0
        PARIOUT=0.0
        PARIPREV=0.0
        PARIUE=0.0
        PARIUED=0.0
        PARIX=0.0
        PARMJC=0.0
        PARMJFAC=0.0
        PARMJIADJ=0.0
        PARMJIC=0.0
        PARU=0.0
        PARU2=0.0
        PARUE=0.0
        PARUEC=0.0
        PD(0:PSX)=0.0
        PDADJ=0.0
        PDL(0:PSX)=0.0
        PECM=0.0
        PEMRG=0.0
        PFGCAV=0.0
        PFGPAV(0:12)=0.0
        PFPCAV=0.0
        PFPPAV(0:12)=0.0
        PGERM=0.0
        PGVAL=0.0
        PHINTFAC=0.0
        PHOTQR=0.0
        PHSV=0.0
        PHTV=0.0
        PLA=0.0
        PLAGSB2=0.0
        PLAGSB3=0.0
        PLAGSB4=0.0
        PLAS=0.0
        PLASI=0.0
        PLASL=0.0
        PLASN=0.0
        PLASP=0.0
        PLASS=0.0
        PLASTMP=0.0
        PLASTMP2=0.0
        PLASW=0.0
        PLAX=0.0
        PLMAGE=0.0
        PLPH=0.0
        PLTPOP=0.0
        PLTPOPE=0.0
        PLTPOPP=0.0
        PPEXP=0.0
        PPOE=0.0
        PPOP=0.0
        PPTHR=0.0
        PSDAPFR(0:PSX)=0.0
        PSDAYFR(0:PSX)=0.0
        PSTART(0:PSX)=0.0
        PTFA=0.0
        PTTN=0.0
        PTX=0.0
        RAINC=0.0
        RAINCC=0.0
        RAINPAV(0:12)=0.0
        RAINPC(0:12)=0.0
        RANC=0.0
        RATM=0.0
        RAW=0.0
        RCROP=0.0
        RDGS=0.0
        RESCAL(0:20)=0.0
        RESLGAL(0:20)=0.0
        RESNAL(0:20)=0.0
        RESPC=0.0
        RESPRC=0.0
        RESPTC=0.0
        RESWAL(0:20)=0.0
        RESWALG(0:20)=0.0
        RFAC=0.0
        RLDF(20)=0.0
        RLF=0.0
        RLFC=0.0
        RLFWU=0.0
        RLIGP=0.0
        RLWR=0.0
        RM=0.0
        RNAD=0.0
        RNAM=0.0
        RNAMM=0.0
        RNCM=0.0
        RNCMN(0:1)=0.0
        RNCR=0.0
        RNCX=0.0
        RNCXS(0:1)=0.0
        RNDEM=0.0
        RNDEMG=0.0
        RNDEMTU=0.0
        RNH4U(20)=0.0
        RNO3U(20)=0.0
        RNPCMN(0:1)=0.0
        RNPCS(0:1)=0.0
        RNUSE(0:2)=0.0
        ROOTN=0.0
        ROOTNEXCESS=0.0
        ROOTNS=0.0
        ROWSPC=0.0
        RRESP=0.0
        RSCD=0.0
        RSCM=0.0
        RSCMM=0.0
        RSCX=0.0
        RSEN=0.0
        RSFP=0.0
        RSFPL=0.0
        RSFPU=0.0
        RSFRS=0.0
        RSN=0.0
        RSNAD=0.0
        RSNPH=0.0
        RSNPHC=0.0
        RSNUSED=0.0
        RSPCO=0.0
        RSSRWTGLFADJ=0.0
        RSUSE=0.0
        RSWAD=0.0
        RSWAM=0.0
        RSWAMM=0.0
        RSWPH=0.0
        RSWPHC=0.0
        RSWT=0.0
        RSWTGLFADJ=0.0
        RSWTM=0.0
        RSWTX=0.0
        RTDEP=0.0
        RTDEPG=0.0
        RTDEPTMP=0.0
        RTNH4=0.0
        RTNO3=0.0
        RTNSL(20)=0.0
        RTRESP=0.0
        RTRESPADJ=0.0
        RTUFR=0.0
        RTWT=0.0
        RTWTAL(20)=0.0
        RTWTG=0.0
        RTWTGADJ=0.0
        RTWTGL(20)=0.0
        RTWTL(20)=0.0
        RTWTM=0.0
        RTWTSL(20)=0.0
        RTWTUL(20)=0.0
        RUNOFFC=0.0
        RWAD=0.0
        RWAM=0.0
        RWAMM=0.0
        RWUMXI=0.0
        SAID=0.0
        SANCOUT=0.0
        SCNCT=0.0
        SCNMT=0.0
        SDCOAT=0.0
        SDDUR=0.0
        SDEPTH=0.0
        SDEPTHU=0.0
        SDNAD=0.0
        SDNAP=0.0
        SDNC=0.0
        SDNPCI=0.0
        SDRATE=0.0
        SDRS=0.0
        SDSZ=0.0
        SDWAD=0.0
        SDWADOUT=0.0
        SDWAM=0.0
        SDWT=0.0
        SEEDN=0.0
        SEEDNI=0.0
        SEEDNUSE=0.0
        SEEDNUSE2=0.0
        SEEDRS=0.0
        SEEDRSAV=0.0
        SEEDRSI=0.0
        SEEDUSE=0.0
        SEEDUSED=0.0
        SEEDUSER=0.0
        SEEDUSES=0.0
        SEEDUSET=0.0
        SELONG=0.0
        SELONGT=0.0
        SENCAGS=0.0
        SENCAS=0.0
        SENCL(0:20)=0.0
        SENCS=0.0
        SENFR=0.0
        SENLA=0.0
        SENLAGS=0.0
        SENLALITTER=0.0
        SENLAS=0.0
        SENLFG=0.0
        SENLFGRS=0.0
        SENLL(0:20)=0.0
        SENLS=0.0
        SENNAGS=0.0
        SENNAL(0:20)=0.0
        SENNAS=0.0
        SENNATC=0.0
        SENNATCM=0.0
        SENNGS=0.0
        SENNL(0:20)=0.0
        SENNLFG=0.0
        SENNLFGRS=0.0
        SENNS=0.0
        SENROOT=0.0
        SENROOTA=0.0
        SENRTG=0.0
        SENTOPLITTER=0.0
        SENTOPLITTERA=0.0
        SENTOPLITTERG=0.0
        SENWACM=0.0
        SENWACMM=0.0
        SENWAGS=0.0
        SENWAL(0:20)=0.0
        SENWALG(0:20)=0.0
        SENWL(0:20)=0.0
        SESR=0.0
        SHGR(22)=0.0
        SHLA(25)=0.0
        SHLAG2(25)=0.0
        SHLAG2B(0:PSX)=0.0
        SHLAGB2(25)=0.0
        SHLAGB3(25)=0.0
        SHLAGB4(25)=0.0
        SHLAS(25)=0.0
        SHNUM=0.0
        SHNUMAD=0.0
        SHNUML(LNUMX)=0.0
        SHRTD=0.0
        SHRTM=0.0
        SHRTMM=0.0
        SLA=0.0
        SLAOUT=0.0
        SLIGP=0.0
        SNAD=0.0
        SNCMN(0:1)=0.0
        SNCRM=0.0
        SNCXS(0:1)=0.0
        SNDEM=0.0
        SNDEMG=0.0
        SNDEMTU=0.0
        SNH4(20)=0.0
        SNH4ROOTZONE=0.0
        SNO3(20)=0.0
        SNO3ROOTZONE=0.0
        SNPCMN(0:1)=0.0
        SNPCS(0:1)=0.0
        SNPH=0.0
        SNPHC=0.0
        SNUSE(0:2)=0.0
        SPRL=0.0
        SRAD20=0.0
        SRAD20S=0.0
        SRADC=0.0
        SRADCAV=0.0
        SRADCC=0.0
        SRADD(20)=0.0
        SRADPAV(0:12)=0.0
        SRADPC=0.0
        SRADPREV=0.0
        SRANC=0.0
        SRNAD=0.0
        SRNAM=0.0
        SRNDEM=0.0
        SRNDEMG=0.0
        SRNDEMTU=0.0
        SRNOAD=0.0
        SRNOAM=0.0
        SRNOAMM=0.0
        SRNOGM=0.0
        SRNOGMM=0.0
        SRNPCS=0.0
        SRNPCM=0.0
        SRNUSE(0:2)=0.0
        SROOTN=0.0
        SRPRS=0.0
        SRWAD=0.0
        SRWT=0.0
        SRWTGRS=0.0
        SRWUM=0.0
        SRWUMM=0.0
        STAI=0.0
        STAIG=0.0
        STAIS=0.0
        STDAY=0.0
        STEMN=0.0
        STEMNEXCESS=0.0
        STRESS(20)=0.0
        STRESS20=0.0
        STRESS20N=0.0
        STRESS20NS=0.0
        STRESS20S=0.0
        STRESS20W=0.0
        STRESS20WS=0.0
        STRESSN(20)=0.0
        STRESSW(20)=0.0
        STWAD=0.0
        STWADOUT=0.0
        STWT=0.0
        STWTM=0.0
        STWTP=0.0
        SWP(0:20)=0.0
        SWPH=0.0
        SWPHC=0.0
        SWPLTD=0.0
        SWPLTH=0.0
        SWPLTL=0.0
        SWPRTIP=0.0
        SWPSD=0.0
        TBLSZ=0.0
        TCAN=0.0
        TCDIF=0.0
        TDIFAV=0.0
        TDIFNUM=0.0
        TDIFSUM=0.0
        TFB=0.0
        TFD=0.0
        TFDF=0.0
        TFDNEXT=0.0
        TFG=0.0
        TFGEM=0.0
        TFLFLIFE=0.0
        TFLFSIZE=0.0
        TFP=0.0
        TFVAL=0.0
        TIMENEED=0.0
        TLCHC=0.0
        TMAXCAV=0.0
        TMAXCC=0.0
        TMAXM=0.0
        TMAXPAV(0:12)=0.0
        TMAXPC=0.0
        TMAXSUM=0.0
        TMAXX=0.0
        TMEAN=0.0
        TMEAN20=0.0
        TMEAN20P=0.0
        TMEAN20S=0.0
        TMEANAV(0:12)=0.0
        TMEANCC=0.0
        TMEAND(20)=0.0
        TMEANE=0.0
        TMEANEC=0.0
        TMEANG=0.0
        TMEANGC=0.0
        TMEANNUM=0.0
        TMEANPC=0.0
        TMEANSUM=0.0
        TMEANSURF=0.0
        TMINCAV=0.0
        TMINCC=0.0
        TMINM=0.0
        TMINN=0.0
        TMINPAV(0:12)=0.0
        TMINPC=0.0
        TMINSUM=0.0
        TNAD=0.0
        TNAMM=0.0
        TNOXC=0.0
        TOFIXC=0.0
        TOMIN=0.0
        TOMINC=0.0
        TOMINFOMC=0.0
        TOMINSOM1C=0.0
        TOMINSOM2C=0.0
        TOMINSOM3C=0.0
        TOMINSOMC=0.0
        TPAR=0.0
        TRATIO=0.0
        TRBRG(4)=0.0
        TRDV1(4)=0.0
        TRDV2(4)=0.0
        TRDV3(4)=0.0
        TRDV4(4)=0.0
        TRGEM(4)=0.0
        TRLDF=0.0
        TRLFG(4)=0.0
        TRLV=0.0
        TRPHS(4)=0.0
        TRWU=0.0
        TSDEP=0.0
        TSRAD=0.0
        TT=0.0
        TT20=0.0
        TT20S=0.0
        TTB=0.0
        TTCUM=0.0
        TTCUMLS=0.0
        TTCUMWS=0.0
        TTD(20)=0.0
        TTGEM=0.0
        TTL=0.0
        TTLFLIFE=0.0
        TTLFSIZE=0.0
        TTNEED=0.0
        TTNEXT=0.0
        TTOUT=0.0
        TVR1=0.0
        TVR2=0.0
        TVR3=0.0
        TVR4=0.0
        TVR5=0.0
        TVR6=0.0
        TWAD=0.0
        VALUER=0.0
        VANC=0.0
        VCNC=0.0
        VMNC=0.0
        VNAD=0.0
        VNAM=0.0
        VNAMM=0.0
        VNPCM=0.0
        VNPCMM=0.0
        VPD=0.0
        VPDFP=0.0
        VWAD=0.0
        VWAM=0.0
        VWAMERR=0.0
        VWAMM=0.0
        WAVR=0.0
        WFEU=0.0
        WFG=0.0
        WFGCAV=0.0
        WFGCC=0.0
        WFGE=0.0
        WFGEM=0.0
        WFGL=0.0
        WFGPAV(0:12)=0.0
        WFGPC=0.0
        WFGU=0.0
        WFNU=0.0
        WFP=0.0
        WFPCAV=0.0
        WFPCC=0.0
        WFPL=0.0
        WFPPAV(0:12)=0.0
        WFPPC=0.0
        WFPU=0.0
        WFRG=0.0
        WFRTG=0.0
        WFSU=0.0
        WTDEP=0.0
        WUPR=0.0
        WUPRD(20)=0.0
        XDEP=0.0
        XDEPL=0.0
        XMIN=0.0


    end subroutine clear_YCA_First_Trans_m
    