Module YCA_First_Trans_m
    USE YCA_Node        ! DA 17MAR2017
    
    
    !CHARACTER(LEN=1),PARAMETER::SLASH = '\'                                     ! (From SeasInit) (In ModuleDefs) 
    CHARACTER(LEN=1),PARAMETER::BLANK = ' '                                     ! (From SeasInit)                
    CHARACTER(LEN=3),PARAMETER::DASH = ' - '                                    ! (From Output)
    
    
    !INTEGER,PARAMETER::NL   =  20 ! Maximum number of soil layers               ! (In ModuleDefs) 
    INTEGER,PARAMETER::DINX =   3 ! Disease number,maximum
    INTEGER,PARAMETER::PSX  =  17 ! Principal stages,maximum !LPM 07MAR15 change to a maximum value of branching levels of 10 
    INTEGER,PARAMETER::SSX  =  20 ! Secondary stages,maximum
    INTEGER,PARAMETER::KEYSTX =17 ! Maximum number of key stages
    INTEGER,PARAMETER::DCNX =  10 ! Disease control #,maximum
    !INTEGER,PARAMETER::LCNUMX=500 ! Maximum number of leaf cohorts
    INTEGER,PARAMETER::LCNUMX=500 ! Maximum number of leaf cohorts LPM 23JUL15 using LNUMX to avoid errors for late or non branching varieties
    INTEGER,PARAMETER::NGMAX=30  ! Maximum number of groups of nodes
    REAL,PARAMETER::LLIFGD = 10.0 ! Leaf growing duration in days
    INTEGER,PARAMETER::LNUMX= 500 ! Maximum number of leaves/axis
    INTEGER,PARAMETER::HANUMX= 40 ! Maximum # harvest instructions
    REAL, PARAMETER:: ZERO = 1.0E-5 ! The "first" real number after zero (0) #MathUtils


    TYPE (Node_type),DIMENSION(0:PSX,0:LCNUMX)  :: node
    
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
    REAL    :: BASELAYER               ! Depth at base of layer         cm         ! (From Integrate) 
    INTEGER :: BRDAE(PSX)              ! DAE when a new branch appears  d          ! LPM 11APR15 To save the date of branch appearance
    REAL    :: BRFX(0:PSX)             ! Branch # per fork at each fork #          ! (From SeasInit)  
    REAL    :: BRNAM                   ! Branch #, all shoots at harvest#          ! (From Output)
    REAL    :: BRNUMSH(0:25)           ! Branch number/shoot            #          ! (From Integrate) !LPM 28MAR15 to have the apex number by branch level 
    REAL    :: BRNUMSHM                ! Branch #, all shoots           #          ! (From Output)    
    REAL    :: BRNUMSHMM               ! Branch #,harv,all shoots obs   #          ! (From Output)
    REAL    :: BRNUMST(0:PSX)          ! Branch number/shoot (>forking) #          ! (From RunInit) !LPM 23MAR15 to have the apex number by branch level   
    REAL    :: BRNUMSTPREV(0:PSX)      ! Branch number/shoot,previous   #          ! (From Output)  !LPM 23MAR15 to have the apex number by branch level      
    INTEGER :: BROLDESTA               ! Br with Leaf  ,oldest active   #          ! (From Integrate)
    INTEGER :: BRSTAGEINT              ! Branching stage,integer        #          
    REAL    :: BRSTAGEPREV             ! Branching stage,previous       #          ! (From SeasInit)
    REAL    :: CAID                    ! Canopy leaf area index         #          ! (From SeasInit)
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
    INTEGER :: CNI                     ! Crop component,initial value   #          ! (From SeasInit)  
    REAL    :: CNPCHM                  ! Canopy N,harvest,measured      %          ! (From Output)    
    REAL    :: CO2AIR                  ! CO2 concentration in air       g/m3       ! (From Growth)    
    REAL    :: CO2CAV                  ! Average co2 for crop cycle     vpm        ! (From Integrate) 
    REAL    :: CO2CC                   ! CO2 sum for cycle              vpm        ! (From SeasInit)  
    !REAL    :: CO2COMPC                ! CO2 compensation conc (vpm)    #          ! (From SeasInit)  
    !REAL    :: CO2EX                   ! Exponent for CO2-phs function  #          ! (From SeasInit)  
    REAL    :: CO2F(10)                ! CO2 factor rel values 0-2      #          ! (From SeasInit)  
    REAL    :: CO2FP                   ! CO2 factor,photosynthesis      #          ! (From SeasInit)  
    REAL    :: CO2FPI                  ! CO2 factor,phs,internal Co2    #          ! (From Growth)    
    REAL    :: CO2MAX                  ! CO2 conc,maximum during cycle  vpm        ! (From SeasInit)  
    REAL    :: CO2PAV(0:19)            ! CO2 concentration in air       g/m3       ! (From SeasInit)  
    REAL    :: CO2PC                   ! CO2 concentration,tier,cumul   ppm        ! (From SeasInit)  
    REAL    :: CO2RF(10)               ! CO2 reference concentration    vpm        ! (From SeasInit)  
    INTEGER :: COLNUM                  ! Column number                  #          ! (From Integrate)   
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
    REAL    :: DAYLPAV(0:19)           ! Daylength (6deg) av for tier   h          ! (From SeasInit)  
    REAL    :: DAYLPC                  ! Daylength (6deg),cumulative    h          ! (From SeasInit)  
    REAL    :: DAYLPREV                ! Daylength previous day         h          ! (From Output)    
    REAL    :: DAYLS(0:PSX)             ! Daylength sensitivity,tier     %/10h      ! (From SeasInit)  
    REAL    :: DAYLST(0:PSX)            ! Daylength (6deg) at stage      h          ! (From SeasInit)  
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
    REAL    :: DMAG                    ! Thermal age threshold dry mat. oCd        ! (From SeasInit)
    REAL    :: DMIC                    ! Dry matter c. increase by temp %          ! (From SeasInit)
    REAL    :: DMP_EP                  ! Dry matter per unit EP         g/mm       ! (From Output)    
    REAL    :: DMP_ET                  ! Dry matter per unit ET         g/mm       ! (From Output)    
    REAL    :: DMP_Irr                 ! Dry matter per unit irrigation g/mm       ! (From Output)    
    REAL    :: DMP_NApp                ! Dry matter per unit N applied  kg/kg      ! (From Output)    
    REAL    :: DMP_NUpt                ! Dry matter per unit N taken uo kg/kg      ! (From Output)    
    REAL    :: DMP_Rain                ! Dry matter per unit water      g/mm       ! (From Output)    
    REAL    :: DMRD                    ! Dry matter reduction by age    %/1000 oCd ! (From SeasInit)
    INTEGER :: DOM                     ! Day of month                   #          ! (From Integrate) 
    INTEGER :: DOYCOL                  ! Day of year column number      #          ! (From Output)    
    REAL    :: DRAINC                  ! Drainage from profile,cumulat  mm         ! (From SeasInit)  
    REAL    :: DTRY                    ! Effective depth of soil layer  cm         ! (From Growth)    
    REAL    :: DU                      ! Developmental units            PVC.d      ! (From SeasInit)  
    REAL    :: DUNEED                  ! Developmental units needed tir PVC.d      ! (From SeasInit)  
    REAL    :: DUPHASE                 ! Development units,current tier PVoCd      ! (From Growth)    
    REAL    :: DUPNEXT                 ! Development units,next tier    PVoCd      ! (From Growth)    
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
    REAL    :: EPPC(0:PSX)              ! Transpiration cycle sum        mm         ! (From Integrate) 
    REAL    :: EPSRATIO                ! Function,plant/soil evap rate  #          ! (From SeasInit)  
    REAL    :: ERRORVAL                ! Plgro-tfile values/Plgro       #          ! (From Output)    
    REAL    :: ETCC                    ! Evapotranspiration cumulative  mm         ! (From SeasInit)  
    REAL    :: ETPC(0:PSX)              ! Evapotranspiration tier sum    mm         ! (From Integrate) 
    INTEGER :: EVALOUT                 ! Evaluate output lines for exp  #          ! (From Output)    
    INTEGER :: EVHEADNM                ! Number of headings in ev file  #          ! (From Output)    
    INTEGER :: EVHEADNMMAX             ! Maximum no headings in ev file #          ! (From Output)    
    INTEGER :: EYEARDOY                ! Emergence Year+DOY             #          ! (From SeasInit)  
    REAL    :: FAC(20)                 ! Factor ((g/Mg)/(kg/ha))        #          ! (From Growth)    
    INTEGER :: FAPPNUM                 ! Fertilization application number          ! (From SeasInit)  
    INTEGER :: FDAY(200)               ! Dates of fertilizer appn       YrDoy      ! (From SeasInit)  
    REAL    :: FERNITPREV              ! Fertilizer N applied to ystday kg/ha      ! (From SeasInit)
    REAL    :: FHWAD                   ! Harvest product (fresh)        kg/ha
    INTEGER :: FHWADCOL                ! Product wt column number       #          ! (From Output)
    REAL    :: FHWADT                  ! Harvest weight (fresh) t file  kg/ha      ! (From Output)
    REAL    :: FHWAM                   ! Harvest product wt.,maturity   kg/ha      !
    REAL    :: FHWAMM                  ! Harvest wt.(fresh),measured    kg/ha      ! (From SeasInit) 
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
!    INTEGER :: FNUMREA                 ! File number,reads.out file     #          ! (From RunInit)   
    INTEGER :: FNUMT                   ! Number used for T-file         #          ! (From RunInit)   
    INTEGER :: FNUMTMP                 ! File number,temporary file     #          ! (From RunInit)   
!    INTEGER :: FNUMWRK                 ! File number,work file          #          ! (From RunInit)   
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
    REAL    :: GROLSRS05S(5)           ! GROLSRS values for last 5 days g/p  
    REAL    :: GROLSRT                 ! Leaf+stem gr from root dmatter g/p        ! (From Growth)    
    REAL    :: GROLSRTN                ! Leaf+stem N growth from root N g/p        ! (From Growth)    
    REAL    :: GROLSSD                 ! Leaf+stem gr from seed         g/p        ! (From Growth)    
    REAL    :: GROLSSEN                ! Leaf+stem growth from senesnce g/p        ! (From Growth) 
    REAL    :: GRORP                   ! Potential root growth          g/p        ! (From SeasInit) !LPM 22DEC2016 potential root growth
    REAL    :: GRORS                   ! Reserves growth                g/p        ! (From SeasInit)  
    REAL    :: GRORSP                  ! Reserves growth potential      g/p        ! (From SeasInit)
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
    REAL    :: HIND                    ! Harvest index,N,above ground   #          ! (From SeasInit)  
    REAL    :: HINM                    ! Harvest index,N,abground,mat   #          ! (From SeasInit)  
    REAL    :: HINMM                   ! Harvest N index,mature,meas    %          ! (From SeasInit)  
    INTEGER :: HLAST                   ! Last date for harvest          #          ! (From SeasInit)  
    REAL    :: HMPC                    ! Harvest dry percent,std..      #          ! (From SeasInit)  ! issue 49
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
    REAL    :: HWFMM                   ! Harvest prod.(fresh) measured  kg/ha      ! (From SeasInit)
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
    !REAL    :: LAI                     ! Leaf area index                #          ! (From SeasInit)  
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
    REAL    :: LANCM                   ! Leaf actual mean N conc.       #
    REAL    :: LANCRS                  ! Leaf N+rsN concentration       #          ! (From Growth)    
    REAL    :: LAPD                    ! Leaf area (green) per plant    cm2        ! (From Growth)    
    REAL    :: LAPH                    ! Leaf area (green) harvested    cm2/d      ! (From Growth)    
    REAL    :: LAPHC                   ! Leaf area (green) harvested,cu cm2/p      ! (From SeasInit)  
    REAL    :: LAWL(2)                 ! Area to wt ratio,n=youngest lf cm2/g      ! (From Growth)    
    REAL    :: LAWS                    ! Leaf area/wt ratio,standard    cm2/g      ! (From SeasInit)     
    REAL    :: LAXS                    ! Area of biggest leaf,main stem cm2        ! (From SeasInit)  
    REAL    :: LCNCT                   ! Leaf critical max N conc       #          !
    REAL    :: LCNMT                   ! Leaf critical min N conc       #          !
    REAL    :: LEAFN                   ! Leaf N                         g/p        ! (From SeasInit)  
    REAL    :: LEAFNEXCESS             ! Leaf N > critical              g/p        ! (From Integrate) 
    INTEGER :: LENDIS                  ! Length,ISWDIS flag             #          ! (From SeasInit)  
    INTEGER :: LENENAME                ! Length,experiment description  #          ! (From Output)    
    INTEGER :: LENGROUP                ! Length of group name           #          ! (From Output)    
    INTEGER :: LENLINE                 ! Length of character string     #          ! (From Output)    
    INTEGER :: LENLINESTAR             ! Length of character string     #          ! (From Output)    
    INTEGER :: LENRNAME                ! Length of run description      #          ! (From SeasInit)  
    INTEGER :: LENTNAME                ! Length,treatment description   #          ! (From SeasInit)  
    REAL    :: LFWT                    ! Leaf weight                    g/p        ! (From SeasInit)  
    REAL    :: LFWTM                   ! Leaf weight,maturity           g/p        ! (From SeasInit)  
    REAL    :: LLIFA                   ! Leaf active duration,read      #          ! (From SeasInit)  
    REAL    :: LLIFATT                 ! Leaf active duration           C.d        ! (From SeasInit)  
    REAL    :: LLIFG                   ! Leaf expansion duration,read   #          ! (From SeasInit)  
    REAL    :: LLIFGTT                 ! Leaf expansion duration        C.d        ! (From SeasInit)  
    REAL    :: LLIFS                   ! Leaf senescence dur,as read    d          ! (From SeasInit)  
    REAL    :: LLIFSTT                 ! Leaf senescence duration,Ttime C.d        ! (From SeasInit)  
    !REAL    :: LLIFX                   ! Leaf ageing acceleration,max   Tu         ! (From SeasInit)  
    !REAL    :: LLIFXUnused             ! Leaf ageing accel.unused       Tu         ! (From Integrate) 
    REAL    :: LLIGP                   ! Leaf lignin percentage         #          ! (From SeasInit)  
    REAL    :: LLNAD                   ! Leaf lamina nitrogen           kg/ha      ! (From SeasInit)  
    !REAL    :: LLOSA                   ! Leaf area loss,accelerated sen fr         ! (From SeasInit)  
    !REAL    :: LLRSWAD                 ! Leaf lamina reserves weight    kg/ha      ! (From SeasInit) !LPM 21MAY2015 The reserves distribution will not be included, it needs to be reviewed 
    !REAL    :: LLRSWT                  ! Leaf lamina reserves           g/p        ! (From SeasInit) !LPM 21MAY2015 The reserves distribution will not be included, it needs to be reviewed 
    REAL    :: LLWAD                   ! Leaf lamina weight             kg/ha      ! (From SeasInit)  
    REAL    :: LLWADOUT                ! Leaf lamina weight for output  kg/ha      ! (From Output)    
    REAL    :: LNCGL                   ! N concentration,growth,lower   fr         ! (From Growth)    
    REAL    :: LNCGU                   ! N concentration,growth,upper   fr         ! (From Growth)      
    REAL    :: LNCMN(0:1)              ! Leaf N conc,minimum            fr         ! (From SeasInit)  
    REAL    :: LNCPL                   ! Leaf N concentration,phs,lower fr         ! (From Growth)    
    REAL    :: LNCPU                   ! Leaf N concentration,phs,upper fr         ! (From Growth)      
    REAL    :: LNCRM                   ! Leaf N relative to maximum (mean)#        !
    REAL    :: LNCSEN                  ! N conc.for senescence          fr         ! (From Growth)      
    REAL    :: LNCXS(0:1)              ! Leaf N conc,maximum,stage      fr         ! (From SeasInit)  
    REAL    :: LNDEM                   ! Leaf demand for N              g/p        ! (From SeasInit)  
    REAL    :: LNDEMG                  ! Leaf demand for N,for growth   g/p        ! (From Growth)    
    REAL    :: LNDEMTU                 ! Leaf demand for N,for topup    g/p        ! (From Growth)    
    REAL    :: LNPCMN(0:1)             ! Leaf N conc,minimum            %          ! (From SeasInit)  
    REAL    :: LNPCS(0:1)              ! Leaf N conc,standard,stage     %          ! (From SeasInit)  
    REAL    :: LNPH                    ! Leaf N harvested               g/p        ! (From Growth)    
    REAL    :: LNPHC                   ! Leaf N harvested,cumulative    g/p        ! (From SeasInit)  
    REAL    :: LNSC                    ! N conc.at senescence           %          !
    REAL    :: LNSLP                   ! Slope for leaf production      #          ! LPM 08JUN2015 New cultivar coefficient 
    REAL    :: LNUM                    ! Leaf number,Haun stage         #          ! (From SeasInit)  
    INTEGER :: LNUMCOL                 ! Leaf number column             #          ! (From Output)    
    REAL    :: LNUMEND                 ! Leaf number,Haun stage,end day #          ! (From SeasInit)  
    REAL    :: LNUMG                   ! Leaf number increase per day   #          ! (From SeasInit)  
    REAL    :: LNUMNEED                ! Leaf # stage to start new leaf #          ! (From Growth)    
    REAL    :: LNUMPREV                ! Leaf number,Haun stage         #          ! (From SeasInit)  
    REAL    :: LNUMSERR                ! Leaf #,error                   %          ! (From Output)    
    INTEGER :: LNUMSG                  ! Leaf number produced on axis   #          ! (From SeasInit)  
    INTEGER :: LNUMSIMSTG(0:PSX)       ! Leaf numbers by br level sim   #          ! LPM 21MAR15 to save the number of leaves by cohort
    REAL    :: LNUMSIMTOSTG(0:PSX)     ! Leaf numbers,fork,simul        #          ! (From Integrate) 
    REAL    :: LNUMSM                  ! Leaf #/shoot,Haun,maturity     #          ! (From SeasInit)  
    REAL    :: LNUMSMM                 ! Leaf #,mature,measured         #/s        ! (From SeasInit)  
    INTEGER :: LNUMSOLDESTA            ! Leaf number,oldest acive,axis  #          ! (From Integrate) 
    REAL    :: LNUMSTG(20)             ! Leaf number,specific stage     #          ! (From SeasInit)  
    REAL    :: LNUMT                   ! Leaf number from t file        #          ! (From Output)    
    REAL    :: LNUMTOSTG(0:PSX)        ! Leaf numbers at fork stages    #          ! (From SeasInit)  
    REAL    :: LNUSE(0:3)              ! Leaf N use,overall and parts   g          ! (From SeasInit)
    REAL    :: LNUSEN(0:2,0:PSX,0:LCNUMX)!Leaf N use by canopy level    g
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
    REAL    :: MSWT                    ! Water weight storage roots     g/p        ! (From Integrate)
    !INTEGER :: MSTG                    ! Maturity stage(eg.black layer) #          ! (From SeasInit) !LPM 05JAN2015 MSTG is not used  
    REAL    :: NCRG                    ! N factor,root growth           ppm        ! (From SeasInit)  
    REAL    :: NDEM2                   ! N demand for growth>minimum    g/p        ! (From Growth)    
    REAL    :: NDEMMN                  ! N demand for growth at minimum g/p        ! (From Growth)    
    REAL    :: NFG                     ! N factor,growth 0-1            #          ! (From SeasInit)  
    REAL    :: NFGCAV                  ! N factor,growth,average,cycle  #          ! (From Integrate) 
    REAL    :: NFGCC                   ! N factor,growh,cycle sum       #          ! (From SeasInit)  
    REAL    :: NFGL                    ! N factor,gr,lower limit        #          ! (From SeasInit)  
    REAL    :: NFGPAV(0:19)            ! N factor,growth,average,tier   #          ! (From SeasInit)  
    REAL    :: NFGPC                   ! N factor,growth,cumulative     #          ! (From SeasInit)  
    REAL    :: NFGU                    ! N factor,gr,upper limit        #          ! (From SeasInit)  
    REAL    :: NFPCAV                  ! N factor,phs,average,cycle     #          ! (From SeasInit)  
    REAL    :: NFPCC                   ! N factor,phs,cumulative,cycle  #          ! (From SeasInit)  
    REAL    :: NFPL                    ! N factor,phs,lower limit       #          ! (From SeasInit)  
    REAL    :: NFPPAV(0:19)            ! N factor,phs,average,tier      #          ! (From SeasInit)  
    REAL    :: NFPPC                   ! N factor,phs,cumulative,tier   #          ! (From SeasInit)  
    REAL    :: NFPU                    ! N factor,phs,upper limit       #          ! (From SeasInit)  
    REAL    :: NFRG                    ! N factor,root growth 0-1       #          ! (From Growth)    
    !REAL    :: NFSU                    ! N factor,senescence,upper lim  #          ! (From SeasInit)  
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
    INTEGER :: NOUTPF                  ! Number for fresh growth output #          ! (From RunInit)
    INTEGER :: NOUTPGF                 ! Number for growth factors file #          ! (From RunInit)   
    INTEGER :: NOUTPN                  ! Number for growthN output file #          ! (From RunInit)   ! Stem N pool (ie.above minimum) g/p        !
    REAL    :: NPOOLL                  ! Leaf N pool (ie.above minimum) g/p        ! (From Growth)    
    REAL    :: NPOOLR                  ! Root N pool (ie.above minimum) g/p        ! (From Growth)    
    REAL    :: NPOOLS                  ! Stem N pool (ie.above minimum) g/p        ! (From Growth)    
    INTEGER :: NSDAYS                  ! N stress days                  #          ! (From SeasInit)   
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
    !REAL    :: PARFC                   ! Max photosynthesis/phs at 330  #          ! (From SeasInit)  
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
    INTEGER :: PDAYS(0:PSX)             ! Tier durations                 PVoCd      ! (From SeasInit)  
    REAL    :: PDL(0:PSX)              ! Tier durations,phint units     #          ! (From SeasInit)
    REAL    :: PDMCD                   ! Dry matter cont. storage roots %          ! (From Integrate)
    REAL    :: PDSV                    ! Phs,fr reduction with VPD daily  /KPa    ! (From SeasInit)  
    REAL    :: PDTV                    ! Phs,threshold VPD for reduction daily KPa! (From SeasInit)  
    REAL    :: PECM                    ! Emergence duration             Cd/cm      ! (From SeasInit)  
    !REAL    :: PEGD                    ! Duration,germ                  deg.d      ! (From SeasInit) !LPM 21MAR2016 Deleted, instead use PGERM 
    REAL    :: PEMRG                   ! Reserves use by TT for emerg   g/Cd       ! (From SeasInit)
    REAL    :: PFGCAV                  ! P factor,growh,cycle,av 0-1    #          ! (From Output)    
    REAL    :: PFGPAV(0:PSX)            ! P factor,growh,tier,av 0-1     #          ! (From Output)    
    REAL    :: PFPCAV                  ! P factor,phs,cycle,average 0-1 #          ! (From Output)    
    REAL    :: PFPPAV(0:PSX)            ! P factor,phs,tier,average 0-1  #          ! (From Output)    
    INTEGER :: PGDAP                   ! Plantgro file days after plt   #          ! (From Output)    
    REAL    :: PGERM                   ! Germination duration           deg.d      ! (From SeasInit)  
    INTEGER :: PGROCOL(20)             ! Plantgro column = t-file data  #          ! (From Output)           
    REAL    :: PGVAL                   ! Plantgro file value            #          ! (From Output)    
    !REAL    :: PHINT                   ! Phylochron interval            deg.d      ! (From SeasInit)  !LPM 21MAY2015 PHINT is not used
    REAL    :: PHINTFAC                ! Phylochron interval factor     #          ! (From SeasInit)  
    !REAL    :: PHINTS                  ! Phylochron interval,standard   deg.d      ! (From SeasInit) !LPM 21MAY2015 PHINT is not used 
    REAL    :: PHOTQR                  ! Photon requirement,calculated  E/mol      ! (From SeasInit)  
    REAL    :: PHSV                    ! Phs,fr reduction with VPD hourly  /KPa    ! (From SeasInit)  
    REAL    :: PHTV                    ! Phs,threshold VPD for reduction hourly KPa! (From SeasInit)  
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
    REAL    :: RAINPAV(0:19)           ! Rainfall,average for tier      mm         ! (From SeasInit)  
    REAL    :: RAINPC(0:PSX)            ! Precipitation tier sum         mm         ! (From SeasInit)  
    REAL    :: RANC                    ! Roots actual N concentration   #          ! (From SeasInit)   
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
    REAL    :: RLIGP                   ! Root lignin concentration      %          ! (From SeasInit)  
    REAL    :: RLWR                    ! Root length/weight ratio       m/g        ! (From SeasInit)  
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
    !REAL    :: RSFRS                   ! Reserves fr.assim.for stem,std fr         ! (From SeasInit)   !LPM 09OCT2019 Remove the reserve fraction to the stems (RSFRS)
    REAL    :: RSN                     ! Reserve N                      g/p        ! (From SeasInit)  
    REAL    :: RSNAD                   ! Reserve N                      kg/ha      ! (From SeasInit)  
    REAL    :: RSNPH                   ! Reserves N harvested           g/p        ! (From SeasInit)  
    REAL    :: RSNPHC                  ! Reserves N harvested,cum       g/p        ! (From SeasInit)  
    REAL    :: RSNUSED                 ! Reserve N used                 g/p        ! (From SeasInit)    
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
    REAL    :: SDSZ                    ! Seed size                      g/cm       ! (From SeasInit) !LPM 22MAR2016 Keep value SDWT  
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
    REAL    :: SHGR(25)                ! Shoot size relative to 1       #          ! (From SeasInit)  
    REAL    :: SHLA(25)                ! Shoot leaf area produced       cm2        ! (From SeasInit)  
    REAL    :: SHLAG2(25)              ! Shoot lf area gr,all axis,H2oN cm2        ! (From SeasInit)  
    REAL    :: SHLAG2B(0:PSX)          ! Shoot lf area gr by br         cm2        ! (From SeasInit)
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
    REAL    :: SLATR                   ! Leaf area/weight,temp response fr/C       ! (From SeasInit)
    REAL    :: SLATS                   ! Leaf area/weight,temp standard C          ! (From SeasInit) 
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
    REAL    :: SRADPAV(0:19)           ! Solar radiation,tier average   MJ/m2      ! (From SeasInit)  
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
    REAL    :: SRWFAD                  ! Storage root weight (fresh)    kg/ha      ! (From Integrate)
    REAL    :: SRWT                    ! Root storage organ weight      g/p        ! (From SeasInit)
    REAL    :: SRWTF                   ! Root storage weight (fresh)    g/p        ! (From SeasInit)
    REAL    :: SRWTFPREV               ! Root storage wt(fresh)last day g/p        ! (From SeasInit)
    REAL    :: SRWTG                   ! Root storage growth            g/p        ! (From SeasInit)
    REAL    :: SRWTGRS                 ! Root storage,reserves>std.%    g/p        ! (From SeasInit)  
    REAL    :: SRWTGRSP                ! Root storage potential         g/p        ! (From SeasInit)
    REAL    :: SRWTGRSADJ              ! Root storage adjusted          g/p        ! (From SeasInit)
    REAL    :: SRWTPREV                ! Root storage wt(dry) last day  g/p        ! (From SeasInit)
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
    REAL    :: TFDMC                   ! Temperature factor, dry matter #          ! (From Growth)
    REAL    :: TFDNEXT                 ! Temperature factor,development #          ! (From Growth)    
    REAL    :: TFG                     ! Temperature factor,growth 0-1  #          ! (From SeasInit)  
    REAL    :: TFGEM                   ! Temperature factor,germ,emrg   #          ! (From Growth)    
    !REAL    :: TFLAW                   ! Temperature factor,lf area/wt #          ! (From Growth)  !LPM 12DEC2016 Delete temperature, water and leaf position factors in SLA   
    !REAL    :: TFLFGROWTH              ! Temperature factor,leaf expansion #       ! (From Growth)  !LPM 14SEP2017 New cardinal temperatures for leaf expansion
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
    REAL    :: TMAXPAV(0:19)           ! Temperature,maximum,tier av    C          ! (From SeasInit)  
    REAL    :: TMAXPC                  ! Temperature,maximum,tier  sum  C          ! (From SeasInit)  
    REAL    :: TMAXSUM                 ! Temperature maximum,summed     C          ! (From SeasInit)  
    REAL    :: TMAXX                   ! Temperature max during season  C          ! (From SeasInit)  
    REAL    :: TMEAN                   ! Temperature mean (TMAX+TMIN/2) C          ! (From SeasInit)  
    REAL    :: TMEAN20                 ! Temperature mean over 20 days  C          ! (From Integrate) 
    REAL    :: TMEAN20P                ! Temperature mean,20 d>planting C          ! (From Integrate) 
    REAL    :: TMEAN20S                ! Temperature sum over 20 days   C          ! (From Integrate) 
    REAL    :: TMEANAV(0:19)           ! Temperature,mean,tier av       C          ! (From SeasInit)  
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
    REAL    :: TMINPAV(0:19)           ! Temperature,minimum,tier av    C          ! (From SeasInit)  
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
    REAL    :: TRDMC(4)                ! Temp response,dry matter       #          ! (From SeasInit)
    REAL    :: TRDV1(4)                ! Temp response,development 1    #          ! (From SeasInit)  
    REAL    :: TRDV2(4)                ! Temp response,development 2    #          ! (From SeasInit)  
    REAL    :: TRDV3(4)                ! Temp response,leaf size Tb cul #          ! LPM 28feb15 new variable to change base temperature for leaf size
    REAL    :: TRDV4(4)                ! Temp response,leaf size Top spe#          ! LPM 12JUL2015 new variable to change optimum temperature for potential leaf size
    REAL    :: TRGEM(4)                ! Temp response,germ.emergence   #          ! (From SeasInit)  
    REAL    :: TRLDF                   ! Intermediate factor,new roots  #          ! (From Growth)    
    REAL    :: TRLFG(4)                ! Temp response,lf growth, br dev#          ! (From SeasInit) 18MAR15 
    REAL    :: TRLFL(4)                ! Temp response,lf life          #          ! (From SeasInit) !LPM 14SEP2017 new cardinal temperatures
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
    REAL    :: TTDMC                   ! Daily thermal time, dry matter #          ! (From Growth)
    REAL    :: TTGEM                   ! Daily thermal time,germ,emrg.  C.d        ! (From SeasInit)  
    REAL    :: TTL                     ! Daily thermal time, leaves     C.d        ! LPM 19APR2016 Added to separate the thermal clock for branching and leaf development
    !REAL    :: TTLFGROWTH              ! Daily thermal time,leaf growth C.d        ! (From Growth) !LPM 14SEP2017 Adding new cardinal temperatures for leaf growth
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
    REAL    :: WFG                     ! Water factor,growth 0-1        #          ! (From SeasInit)  
    REAL    :: WFGCAV                  ! Water factor,growth,av.,cylcle #          ! (From Integrate) 
    REAL    :: WFGCC                   ! H20 factor,growh,cycle sum     #          ! (From SeasInit)  
    REAL    :: WFGE                    ! Water factor,germ,emergence    #          ! (From Growth)    
    REAL    :: WFGEM                   ! Water factor,germ,emergence    #          ! (From SeasInit)  
    REAL    :: WFGL                    ! Water factor,growth,lower      #          ! (From SeasInit)  
    REAL    :: WFGPAV(0:19)            ! Water factor,growth,average    #          ! (From SeasInit)  
    REAL    :: WFGPC                   ! Water factor,growth,cumulative #          ! (From SeasInit)  
    REAL    :: WFGREA                  ! Water factor, reallocation     #               
    REAL    :: WFGU                    ! Water factor,growth,upper      #          ! (From SeasInit)  
    !REAL    :: WFLAW                   ! Water factor,leaf area/weight  #          ! (From Growth)    !LPM 12DEC2016 Delete temperature, water and leaf position factors in SLA
    REAL    :: WFNU                    ! Water factor,N uptake          #          ! (From Growth)    
    REAL    :: WFP                     ! Water factor,photosynthsis 0-1 #          ! (From SeasInit)  
    REAL    :: WFPCAV                  ! Water factor,phs,av 0-1,cycle  #          ! (From SeasInit)  
    REAL    :: WFPCC                   ! H20 factor,phs,cycle sum       #          ! (From SeasInit)  
    REAL    :: WFPL                    ! Water factor,phs,lower         #          ! (From SeasInit) !LPM 15SEP2020 No water stress factor on photosynthesis 
    REAL    :: WFPPAV(0:19)            ! Water factor,phs,average 0-1   #          ! (From SeasInit)  
    REAL    :: WFPPC                   ! Water factor,phs,cumulative    #          ! (From SeasInit)  
    REAL    :: WFPU                    ! Water factor,phs,upper         #          ! (From SeasInit)  
    REAL    :: WFRG                    ! Water factor,root growth,0-1   #          ! (From Growth)    
    REAL    :: WFRTG                   ! Water factor,root gr           #          ! (From SeasInit)  
    REAL    :: WFSU                    ! Water fac,leaf senescence,0-1  #          ! (From SeasInit)  
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
    CHARACTER (LEN=12)  OUTPG3        ! Growth output file3 code                  ! (From RunInit)
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
    INTEGER,      PARAMETER :: SUMNUM = 42                                         ! (From Output)
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
        node=Node_type_constructor()
        
        
        L = 0
        L1 = 0
        L2 = 0
        I = 0
        NLAYRROOT = 0
        
        amtnit = 0.0
        andem = 0.0
        brfx = 0.0
        BRNAM = -99.0
        brnumst = 1.0
        BRNUMSHM = 0.0
        BRNUMSHMM = -99.0
        BRSTAGEINT = 0
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
        cfllflife = '-'
        cflharmsg = 'N'
        cflsdrsmsg = 'N'
        sdwt = 0.0
        crwt = 0.0
        crwtp = 0.0 !LPM 23MAY2015 Added to keep the potential planting stick weight
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
        dabr = 0.0 !LPM 24APR2016 DABR added as developmental age for branching (with stress)
        dag=-99 !LPM 10JUL2017 To consider root and stem develpment after germination and before emergence (planting stick below-ground)
        dagerm = 0.0 !LPM 21MAR2015 DAGERM added to save develpomental age at germination (with stress)
        dals = 0.0 !LPM 24APR2016 DALS added as developmental age for leaf size (when DAWWP> 900)
        dap = -99
        dawwp = 0.0 !LPM 06MAR2016 DAWWP added to save Development Age (with stress)
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
        fhwam = -99.0
        FHWAMM = -99.0
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
        GROLSRS = 0.0
        GROLSRS05S = 1.0
        grors = 0.0
        grorsp = 0.0
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
        laiprev = 0.0
        laistg = 0.0
        laix = 0.0
        laixm = -99.0
        !lanc = 0.0
        LANCM = 0.0
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
        lnumsimstg = 0     !LPM 09AGO2015 To initialize the variable lnumsimstg
        lnumsimtostg = 0.0 !LPM 09AGO2015 To initialize the variable lnumsimtostg
        lnumg = 0.0
        lnumprev = 0.0
        lnumsg = 0
        lnumsm = -99.0
        lnumsmm = -99.0
        lnumstg = 0.0
        lnuse = 0.0
        lseed = -99
        lpeai = 0.0
        lperswt = 0.0
        lpewad = 0.0
        lstage = 0.0
        lwphc = 0.0
        mdap = -99
        mdat = -99
        mdayfr = -99
        mdoy = -99
        MSWT = 0.0
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
        PDMCD = 0.0
        photqr = 0.0
        pla = 0.0
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
        brstageprev = 0.0
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
        sancout = 0.0
        sdnad = 0.0
        sdnc = 0.0
        sdwad = 0.0
        sdwam = -99.0
        seedrs = -99.0 !LPM 22MAR2016 to define initially the reserves based on stick length (YCA_Growth_init.f90)
        seeduse = 0.0 
        seedused = 0.0 !LPM 22MAR2016 Added SEEDUSED 
        seeduser = 0.0
        seeduses = 0.0 !LPM 22MAR2016 Added SEEDUSES 
        seeduset = 0.0
        selong = 0.0 !LPM 22MAR2016 Added SELONG
        selongt = 0.0 !LPM 22MAR2016 Added SELONGT
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
        shdat = 0
        shla = 0.0
        shlag2 = 0.0
        shlag2b = 0.0 !LPM 23MAR15 add new variable
        shlas = 0.0
        shnum = 0.0
        shnumad = 0.0
        shnuml = 1.0
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
        srnam = -99.0
        sranc = 0.0
        srndem = 0.0
        srnoad = 0.0
        srnoam = 0.0
        srnogm = 0.0
        srnuse = 0.0
        srootn = 0.0
        srwt = 0.0
        srwtf = 0.0
        SRWTFPREV = 0.0
        SRWTG = 0.0
        SRWTPREV = 0.0
        srwtgrs = 0.0
        SRWTGRSADJ = 0.0
        srwum = 0.0
        srwum = 0.0
        stai = 0.0
        staig = 0.0
        stais = 0.0
        stemn = 0.0
        stgedat = 0
        stwad = 0.0
        STRESSW = 1.0
        stwt = 0.0
        stwtp = 0.0 !LPM 23MAY2015 Added to keep the potential stem weight
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
        tt = 0.0
        tt20 = -99.0
        ttgem = 0.0
        ttcum = 0.0
        ttcumws = 0.0 !LPM 31JUL2015 Added to have a new clock with water stress
        ttcumls = 0.0 !LPM 12JUL2015 Added to have a new clock using a different optimum temperature for leaf size
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
        WFGREA = 1.0
        wsdays = 0
        wupr = 1.0
        
        h2ocf = -99.0
        no3cf = -99.0
        nh4cf = -99.0
        rtno3 = -99.0
        rtnh4 = -99.0
        no3mn = -99.0
        nh4mn = -99.0

    end subroutine clear_YCA_First_Trans_m
    