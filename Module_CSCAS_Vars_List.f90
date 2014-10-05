!=======================================================================
!  MODULE Module_CSCAS_Vars_List
!  13/09/2014 MJF Written
!=======================================================================

    MODULE Module_CSCAS_Vars_List
        !     Contains definitions of derived data types and constants which are 
        !     used throughout the CSCAS submodel.
        
        USE ModuleDefs
        
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
        !INTEGER,PARAMETER::NL   =  20 ! Maximum number of soil layers
        !
        !INTEGER,PARAMETER::RUNINIT=1  ! Program initiation indicator
        !INTEGER,PARAMETER::SEASINIT=2 ! Reinitialisation indicator
        !INTEGER,PARAMETER::RATE = 3   ! Program rate calc.indicator
        !INTEGER,PARAMETER::INTEGR=4   ! Program update indicator
        !INTEGER,PARAMETER::OUTPUT=5   ! Program output indicator
        !INTEGER,PARAMETER::SEASEND= 6 ! Program ending indicator
        !
        CHARACTER(LEN=1),PARAMETER::BLANK = ' '
        CHARACTER(LEN=3),PARAMETER::DASH = ' - '

            !REAL,PARAMETER::PATM=101300.0! Pressure of air,Pa
            !REAL,PARAMETER::SHAIR=1005.0 ! Specific heat of air,MJ/kg
            !REAL,PARAMETER::SBZCON=4.903E-9 !Stefan Boltzmann,MJ/K4/m2/d
        SAVE


       !=======================================================================-========================
       !
       ! All variables in CSCAS.f90 excluding FUNCTIONS ans dummy ARGUMENTS (listed but commented out).
       !Type    :: VarName(Dims)       ! Description and units
       !================================================================================================

        REAL    :: AFLF(0:LNUMX)       ! CH2O factor for leaf,average   #
        REAL    :: AH2OPROFILE         ! Available H2o,profile          mm
        REAL    :: AH2OROOTZONE        ! Available h2o in root zone     mm
        !REAL    :: ALBEDO              ! Canopy+soil albedo             fr          ARGUMENT 
        REAL    :: ALBEDOS             ! soil albedo                    fr
        REAL    :: AMTNIT              ! Cumulative amount of N applied kg/ha
        REAL    :: AMTNITPREV          ! Cumulative N,previous treatmnt kg/ha
        REAL    :: ANDEM               ! Crop N demand                  kg/ha
        REAL    :: ANFER(200)          ! N amount in fertilizer appln   kg/ha
        REAL    :: AREAPOSSIBLE        ! Leaf area growth at SLA limit  cm2 
        REAL    :: AREAPOSSIBLEN       ! Leaf area growth at N limit    cm2 
        INTEGER :: ARGLEN              ! Argument component length      #
        REAL    :: AVGSW               ! Average soil water in SWPLTD   %
        REAL    :: BASELAYER           ! Depth at base of layer         cm
        !REAL    :: BD(20)              ! Bulk density (moist)           g/cm3       ARGUMENT
        REAL    :: BRFX(PSX)           ! Branch # per fork at each fork #
        REAL    :: BRNUMSH             ! Branch number/shoot at harvest #
        REAL    :: BRNUMSHM            ! Branch #/shoot,harvest,measurd #
        REAL    :: BRNUMST             ! Branch number/shoot (>forking) #
        REAL    :: BRNUMSTPREV         ! Branch number/shoot,previous   # 
        !REAL    :: BRSTAGE             ! Branching stage                #           ARGUMENT
        REAL    :: BRSTAGEPREV         ! Branching stage,previous       #
        REAL    :: BRSTAGETMP          ! Branching stage,temporary      #
        !REAL    :: CAID                ! Canopy area index              #           ARGUMENT
        !REAL    :: CANHT               ! Canopy height                  cm          ARGUMENT
        REAL    :: CANHTG              ! Canopy height growth           cm
        REAL    :: CANHTS              ! Canopy height standard         cm
        REAL    :: CARBOADJ            ! Ch2o adjustment for LAI change g/p
        REAL    :: CARBOBEG            ! Ch2o available,beginning day   g/p
        REAL    :: CARBOBEGI           ! Ch2o avail,internal co2 calc   g/p
        REAL    :: CARBOBEGIA          ! Ch2o avail,internal co2,adj    #
        REAL    :: CARBOBEGM           ! Ch2o avail,Monteith ccalc.     g/p
        REAL    :: CARBOBEGR           ! Ch2o avail,PARUE calculation   g/p
        REAL    :: CARBOC              ! Ch2o assimilated,cumulative    g/p
        REAL    :: CARBOEND            ! Ch2o available,end of day      g/p
        REAL    :: CARBOR              ! Ch2o available,roots           g/p
        REAL    :: CARBOT              ! Ch2o available,tops            g/p
        REAL    :: CARBOTMP            ! Ch2o available,temporary value g/p
        REAL    :: CARBOTMPI           ! Ch2o avail,internal co2 calc   g/p
        REAL    :: CARBOTMPM           ! Ch2o avail,Monteith calc       g/p
        REAL    :: CARBOTMPR           ! Ch2o avail,PARUE calculation   g/p
        INTEGER :: CDAYS               ! Crop cycle duration            PVoCd
        REAL    :: CHTPC(10)           ! Canopy ht % associated w LA%   %
        REAL    :: CLAPC(10)           ! Canopy lf area % down to ht    %
        !REAL    :: CLOUDS              ! Cloudiness factor,relative,0-1 #           ARGUMENT
        !INTEGER :: CN                  ! Crop component (multicrop)     #           ARGUMENT
        REAL    :: CNAD                ! Canopy nitrogen                kg/ha
        REAL    :: CNADPREV            ! Canopy nitrogen,previous day   kg/ha
        REAL    :: CNADSTG(20)         ! Canopy nitrogen,specific stage kg/ha
        REAL    :: CNAM                ! Canopy N at harvest            kg/ha
        REAL    :: CNAMERR             ! Canopy N,harvest,error         %
        REAL    :: CNAMM               ! Canopy N,harvest,measured      kg/ha
        REAL    :: CNCTMP              ! Canopy N concentration,tempry  %
        INTEGER :: CNI                 ! Crop component,initial value   #
        REAL    :: CNPCHM              ! Canopy N,harvest,measured      %
        !REAL    :: CO2                 ! CO2 concentration in air       vpm         ARGUMENT
        REAL    :: CO2AIR              ! CO2 concentration in air       g/m3
        REAL    :: CO2CAV              ! Average co2 for crop cycle     vpm
        REAL    :: CO2CC               ! CO2 sum for cycle              vpm
        REAL    :: CO2COMPC            ! CO2 compensation conc (vpm)    #
        REAL    :: CO2EX               ! Exponent for CO2-phs function  #
        REAL    :: CO2F(10)            ! CO2 factor rel values 0-2      #
        REAL    :: CO2FP               ! CO2 factor,photosynthesis      #
        REAL    :: CO2FPI              ! CO2 factor,phs,internal Co2    #
        REAL    :: CO2INT              ! CO2 concentration,internal     g/m3
        REAL    :: CO2INTPPM           ! CO2 concentration,internal     ppm
        REAL    :: CO2INTPPMP          ! CO2 concentration,internal,prv ppm
        REAL    :: CO2MAX              ! CO2 conc,maximum during cycle  vpm
        REAL    :: CO2PAV(0:12)        ! CO2 concentration in air       g/m3
        REAL    :: CO2PC               ! CO2 concentration,tier,cumul   ppm
        REAL    :: CO2RF(10)           ! CO2 reference concentration    vpm
        INTEGER :: COLNUM              ! Column number                  #
        REAL    :: CRFR                ! Crown growth rate,fr stem gr   #
        REAL    :: CRRSWAD             ! Crown reserves                 kg/ha
        REAL    :: CRRSWT              ! Crown reserves                 g/p
        REAL    :: CRWAD               ! Crown weight                   kg/ha
        REAL    :: CRWADOUT            ! Crown weight for output        kg/ha
        REAL    :: CRWT                ! Crown weight                   g/p
        REAL    :: CRWTM               ! Crown weight at maturity       g/p
        !INTEGER :: CSIDLAYR            ! Layer # output from function   #           FUNCTION
        !INTEGER :: CSTIMDIF            ! Time difference function       #           FUNCTION
        !REAL    :: CSVPSAT             ! Vapour pressure function op    mb          FUNCTION
        !INTEGER :: CSYDOY              ! Yr+Doy output from function    #           FUNCTION
        !INTEGER :: CSYEARDOY           ! Year+Doy from function         #           FUNCTION
        !REAL    :: CSYVAL              ! Y value from function          #           FUNCTION
        INTEGER :: CTRNUMPD            ! Control # missing tiers        #
        REAL    :: CUMDEP              ! Cumulative depth               cm
        REAL    :: CUMDU               ! Cumulative development units   #
        REAL    :: CUMSW               ! Soil water in depth SWPLTD     cm
        REAL    :: CWAD                ! Canopy weight                  kg/ha
        INTEGER :: CWADCOL             ! Column number for canopy wt    #
        REAL    :: CWADPREV            ! Canopy weight,previous day     kg/ha
        REAL    :: CWADSTG(20)         ! Canopy weight,particular stage kg/ha
        REAL    :: CWADT               ! Canopy weight from t file      kg/ha
        REAL    :: CWAHC               ! Canopy weight harvested,forage kg/ha
        REAL    :: CWAHCM              ! Canopy wt harvested,forage,mes kg/ha
        REAL    :: CWAM                ! Canopy weight,maturity         kg/ha
        REAL    :: CWAMERR             ! Canopy weight,maturity,error   %
        REAL    :: CWAMM               ! Canopy wt,maturity,measured    kg/ha
        REAL    :: CWAN(HANUMX)        ! Canopy wt minimum after harvst kg/ha
        INTEGER :: DAE                 ! Days after emergence           d
        REAL    :: DALF(0:LNUMX)       ! Days during which leaf active  d
        INTEGER :: DAP                 ! Days after planting            d
        !INTEGER :: DAPCALC             ! DAP output from funcion        #           FUNCTION
        INTEGER :: DAS                 ! Days after start of simulation d
        INTEGER :: DATE                ! Date (Yr+Doy)                  #
        INTEGER :: DATECOL             ! Date column number             #
        !REAL    :: DAYL                ! Daylength,6deg below horizon   h           ARGUMENT
        REAL    :: DAYLCAV             ! Daylength (6deg) av for cycle  h
        REAL    :: DAYLCC              ! Daylength,cycle sum            h.d 
        REAL    :: DAYLPAV(0:12)       ! Daylength (6deg) av for tier   h
        REAL    :: DAYLPC              ! Daylength (6deg),cumulative    h
        REAL    :: DAYLPREV            ! Daylength previous day         h
        REAL    :: DAYLS(0:10)         ! Daylength sensitivity,tier     %/10h
        REAL    :: DAYLST(0:12)        ! Daylength (6deg) at stage      h
        REAL    :: DAYSUM              ! Days accumulated in month      #
        INTEGER :: DCDAT(DCNX)         ! Disease control application    YrDoy
        REAL    :: DCDUR(DCNX)         ! Disease control duration       d
        REAL    :: DCFAC(DCNX)         ! Disease control gr factor 0-1  #
        INTEGER :: DCTAR(DCNX)         ! Disease control target         #
        !REAL    :: DEPMAX              ! Maximum depth of soil profile  cm          ARGUMENT         
        !REAL    :: DEWDUR              ! Dew duration                   h           ARGUMENT
        REAL    :: DF                  ! Daylength factor 0-1           #
        REAL    :: DFNEXT              ! Daylength factor,next tier     #
        REAL    :: DFOUT               ! Daylength factor for output    #
        REAL    :: DFPE                ! Development factor,pre-emerge  #
        REAL    :: DGLF(0:LNUMX)       ! Days during which leaf growing #
        INTEGER :: DIDAT(DINX)         ! Disease initiation date YrDoy  d
        INTEGER :: DIDOY(DINX)         ! Disease infestation doy        d
        REAL    :: DIFFACR(DINX)       ! Dis favourability requirement  #
        REAL    :: DIGFAC(DINX)        ! Disease growth factor 0-1      #
        !REAL    :: DLAYR(20)           ! Depth of soil layers           cm          ARGUMENT 
        REAL    :: DLAYRTMP(20)        ! Depth of soil layers with root cm
        REAL    :: DMP_EP              ! Dry matter per unit EP         g/mm
        REAL    :: DMP_ET              ! Dry matter per unit ET         g/mm 
        REAL    :: DMP_Irr             ! Dry matter per unit irrigation g/mm
        REAL    :: DMP_NApp            ! Dry matter per unit N applied  kg/kg
        REAL    :: DMP_NUpt            ! Dry matter per unit N taken uo kg/kg
        REAL    :: DMP_Rain            ! Dry matter per unit water      g/mm
        INTEGER :: DOM                 ! Day of month                   #
        !INTEGER :: DOY                 ! Day of year                    d           ARGUMENT
        INTEGER :: DOYCOL              ! Day of year column number      #
        !REAL    :: DRAIN               ! Drainage from soil profile     mm/d
        REAL    :: DRAINC              ! Drainage from profile,cumulat  mm           ARGUMENT  
        REAL    :: DSLF(0:LNUMX)       ! Days during which leaf senesng #
        REAL    :: DSTAGE              ! Development stage,linear       #
        REAL    :: DTRY                ! Effective depth of soil layer  cm
        REAL    :: DU                  ! Developmental units            PVC.d
        !REAL    :: DUL(20)             ! Drained upper limit for soil   #
        REAL    :: DUNEED              ! Developmental units needed tir PVC.d
        REAL    :: DUPHASE             ! Development units,current tier PVoCd
        REAL    :: DUPNEXT             ! Development units,next tier    PVoCd
        REAL    :: DUSRI               ! Degree days to st.root init.   oC.d
        REAL    :: DUTOMSTG            ! Developmental units,germ->mat  Du
        !INTEGER :: DYNAMIC             ! Program control variable       #           ARGUMENT
        INTEGER :: DYNAMICPREV         ! Program control varbl,previous #
        REAL    :: EARLYN              ! Leaf # for early N switch      # 
        REAL    :: EARLYW              ! Leaf # for early h20 switch    # 
        INTEGER :: EDAP                ! Emergence DAP                  d
        REAL    :: EDAPFR              ! Emergence DAP+fraction         #
        INTEGER :: EDAPM               ! Emergence DAP measured         #
        INTEGER :: EDATM               ! Emergence date,measured (Afle) #
        INTEGER :: EDATMX              ! Emergence date,measured (YrDy) #
        REAL    :: EDAYFR              ! Emergence day fraction         #
        INTEGER :: EMDATERR            ! Emergence date error d         #
        REAL    :: EMRGFR              ! Fraction of day > emergence    #
        REAL    :: EMRGFRPREV          ! Fraction of day > em,previous  #
        !REAL    :: EO                  ! Potential evaporation          mm/d        ARGUMENT
        REAL    :: EOC                 ! Potential evap,cumulative      mm 
        REAL    :: EOEBUD              ! Potential evap,Ebudget         mm/d
        REAL    :: EOEBUDC             ! Potential evap,Ebudget,cum     mm
        REAL    :: EOEBUDCRP           ! Potential evaporation,with res mm/d
        REAL    :: EOEBUDCRPC          ! Potential evaporation,res,cum  mm/d
        REAL    :: EOEBUDCRPCO2        ! Potential evaporation,res+co2  mm/d
        REAL    :: EOEBUDCRPCO2C       ! Potential ev,res+co2,cum       mm/d
        REAL    :: EOEBUDCRPCO2H2O     ! Potential evap,res+co2+h2o   mm/d
        REAL    :: EOEBUDCRPCO2H2OC    ! Pot.ev,res+co2+h2o,cum      mm/d
        REAL    :: EOMPCRP             ! Potential ev,M-P with crop res mm/d
        REAL    :: EOMPCRPC            ! Potential ev,M-P with res,cum  mm/d
        REAL    :: EOMPCRPCO2          ! Potential ev,M-P,res+co2       mm/d
        REAL    :: EOMPCRPCO2C         ! Potential ev,M-P,res+co2,cum   mm/d
        REAL    :: EOMPCRPCO2H2O       ! Potential ev,M-P,res+co2+h2o   mm/d
        REAL    :: EOMPCRPCO2H2OC      ! Pot.ev,M-P,res+co2+h2o,cum    mm/d
        REAL    :: EOMPEN              ! Potential ev,M-Penman          mm/d
        REAL    :: EOMPENC             ! Potential ev,M-Penman,cum      mm  
        !REAL    :: EOP                 ! Potential evaporation,plants   mm/d        ARGUMENT 
        REAL    :: EOPEN               ! Potential evaporation,Penman   mm/d
        REAL    :: EOPENC              ! Potential evaporation,Pen,cum  mm  
        REAL    :: EOPT                ! Potential evaporation,PT       mm/d
        REAL    :: EOPTC               ! Potential evaporation,PT,cum   mm  
        !REAL    :: EP                  ! Transpiration daily            mm/d        ARGUMENT 
        REAL    :: EPCC                ! Transpiration cycle sum        mm 
        REAL    :: EPPC(0:12)          ! Transpiration cycle sum        mm 
        REAL    :: EPSRATIO            ! Function,plant/soil evap rate  #
        INTEGER :: ERRNUM              ! Error number from compiler     #
        REAL    :: ERRORVAL            ! Plgro-tfile values/Plgro       #
        !REAL    :: ES                  ! Actual soil evaporation rate   mm/d        ARGUMENT
        !REAL    :: ET                  ! Evapotranspiration daily       mm/d        ARGUMENT 
        REAL    :: ETCC                ! Evapotranspiration cumulative  mm 
        REAL    :: ETPC(0:12)          ! Evapotranspiration tier sum    mm 
        INTEGER :: EVALOUT             ! Evaluate output lines for exp  #
        INTEGER :: EVHEADNM            ! Number of headings in ev file  #
        INTEGER :: EVHEADNMMAX         ! Maximum no headings in ev file #
        INTEGER :: EYEARDOY            ! Emergence Year+DOY             #
        REAL    :: FAC(20)             ! Factor ((g/Mg)/(kg/ha))        #
        INTEGER :: FAPPNUM             ! Fertilization application no   #
        INTEGER :: FDAY(200)           ! Dates of fertilizer appn       YrDoy
        !REAL    :: FERNIT              ! Fertilizer N applied           kg/ha       ARGUMENT
        REAL    :: FERNITPREV          ! Fertilizer N applied to ystday kg/ha
        INTEGER :: FILELEN             ! Length of file name            #
        INTEGER :: FLDAP               ! Final leaf date                Yrdoy
        REAL    :: FLN                 ! Final leaf #                   #
        REAL    :: FNH4                ! Unitless ammonium supply index #
        REAL    :: FNO3                ! Unitless nitrate supply index  #
        INTEGER :: FNUMERA             ! File number,A-data errors      #
        INTEGER :: FNUMERR             ! File number,error file         #
        INTEGER :: FNUMERT             ! File number,T-data errors      #
        INTEGER :: FNUMEVAL            ! Number used for evaluate op    #
        INTEGER :: FNUMLVS             ! File number,leaves             #
        INTEGER :: FNUMMEAS            ! Number used for measured data  #
        INTEGER :: FNUMOV              ! Number used for overview op    #
        INTEGER :: FNUMPHA             ! File number,tiers              #
        INTEGER :: FNUMPHEM            ! File number,phenology,measured #
        INTEGER :: FNUMPHES            ! File number,phenology,simulate #
        INTEGER :: FNUMPREM            ! File number,measured responses #
        INTEGER :: FNUMPRES            ! File number,simulated response #
        INTEGER :: FNUMPSUM            ! Number used for plant summary  #
        INTEGER :: FNUMREA             ! File number,reads.out file     #
        INTEGER :: FNUMT               ! Number used for T-file         #
        INTEGER :: FNUMTMP             ! File number,temporary file     #
        INTEGER :: FNUMWRK             ! File number,work file          #
        !INTEGER :: FROP                ! Frquency of outputs,as sent    d           ARGUMENT
        INTEGER :: FROPADJ             ! Frquency of outputs,adjusted   d
        REAL    :: FSOILH2O            ! Final soil water               cm   
        REAL    :: FSOILN              ! Final soil inorganic N         kg/ha
        INTEGER :: GDAP                ! Germination DAP                d
        REAL    :: GDAPFR              ! Germination DAP+fr             #
        INTEGER :: GDAPM               ! Germination DAP,measured       #
        INTEGER :: GDATM               ! Germination date,measured      #
        REAL    :: GDAYFR              ! Fraction of day to germination #
        REAL    :: GEDAYSE             ! Period germination->emergence  d
        REAL    :: GEDAYSG             ! Period planting->germination   d
        REAL    :: GERMFR              ! Fraction of day > germination  #
        REAL    :: GESTAGE             ! Germination,emergence stage    #
        REAL    :: GESTAGEPREV         ! Germ,emerg stage,previous day  #
        REAL    :: GEUCUM              ! Cumulative germ+emergence unit #
        REAL    :: GROCR               ! Crown growth rate              g/p
        REAL    :: GROCRADJ            ! Crown growth rate N adjusted   g/p
        REAL    :: GROCRFR             ! Crown growth rate,fraction st  #
        REAL    :: GROLF               ! Leaf growth rate               g/p
        REAL    :: GROLFADJ            ! Leaf growth rate N adjusted    g/p
        REAL    :: GROLFNEEDED         ! Leaf growth rate for max SLA   g/p
        REAL    :: GROLFP              ! Leaf growth,potential          g/p
        REAL    :: GROLS               ! Leaf+stem growth               g/p
        REAL    :: GROLSA              ! Leaf+stem gr from assimilates  g/p
        REAL    :: GROLSP              ! Leaf+stem growth potential     g/p
        REAL    :: GROLSRS             ! Leaf+stem growth from reserves g/p
        REAL    :: GROLSRT             ! Leaf+stem gr from root dmatter g/p
        REAL    :: GROLSRTN            ! Leaf+stem N growth from root N g/p
        REAL    :: GROLSSD             ! Leaf+stem gr from seed         g/p
        REAL    :: GROLSSEN            ! Leaf+stem growth from senesnce g/p
        REAL    :: GRORS               ! Reserves growth                g/p
        REAL    :: GRORSADJ            ! Reserves gr,adj to conc        g/p
        REAL    :: GROSR               ! Storage root growth            g/p
        REAL    :: GROST               ! Stem growth rate               g/p
        REAL    :: GROSTADJ            ! Stem growth rate N adjusted    g/p
        REAL    :: GROSTCR             ! Stem+crown growth rate         g/p
        REAL    :: GROSTCRP            ! Stem+crown growth potential    g/p 
        REAL    :: GROSTCRPSTORE       ! Stem+crown gr potentl,previous g/p 
        REAL    :: GrP_EP              ! Harvest product per unit EP    g/mm
        REAL    :: GRP_ET              ! Harvest product per unit water g/mm
        REAL    :: GrP_Irr             ! Harvest dm per unit irrigation g/mm
        REAL    :: GrP_NApp            ! Harvest dm per unit N appllied kg/kg
        REAL    :: GrP_NUpt            ! Harvest dm per unit N taken up kg/kg
        REAL    :: GRP_Rain            ! Harvest product per unit water g/mm
        INTEGER :: GSTDCOL             ! Growth stage column number     #
        INTEGER :: GYEARDOY            ! Germination Year+DOY           #
        REAL    :: H2OA                ! Water available in root zone   mm
        REAL    :: H2OCF               ! Water 'potential' factor N upt mm
        REAL    :: H2OPROFILE          ! Total h2o in soil profile      mm   
        REAL    :: H2OROOTZONE         ! Total h2o in root zone         mm   
        INTEGER :: HADOY               ! Harvest day of year            d
        REAL    :: HAFR                ! Harvested fraction             kg/ha
        REAL    :: HAMT(HANUMX)        ! Harvest amount                 #
        INTEGER :: HANUM               ! Harvest instruction number     # 
        !REAL    :: HARVFRAC(2)         ! Harvest fraction as brought in #           ARGUMENT
        REAL    :: HAWAD               ! Harvested weight (grazing,etc) kg/ha
        INTEGER :: HAYEAR              ! Harvest year                   #
        REAL    :: HBPC(HANUMX)        ! Harvest by-product percentage  #
        REAL    :: HBPCF               ! Harvest by-product %,final     #
        INTEGER :: HDAY                ! Harvest day as read            #
        INTEGER :: HDOYF               ! Earliest doy for harvest       #
        INTEGER :: HDOYL               ! Last doy for harvest           #
        REAL    :: HDUR                ! Hardening duration,days        d
        INTEGER :: HFIRST              ! Earliest date for harvest      #
        REAL    :: HIAD                ! Harvest index,above ground     #
        INTEGER :: HIADCOL             ! Harvest index column number    #
        REAL    :: HIADT               ! Harvest index from t file      #
        REAL    :: HIAM                ! Harvest index,above ground,mat #
        REAL    :: HIAMERR             ! Harvest index,maturity,error   %
        REAL    :: HIAMM               ! Harvest index,mature,measure   #
        REAL    :: HIAMMTMP            ! Harvest index,mature,temporary #
        REAL    :: HIND                ! Harvest index,N,above ground   #
        REAL    :: HINM                ! Harvest index,N,abground,mat   #
        REAL    :: HINMM               ! Harvest N index,mature,meas    %
        INTEGER :: HLAST               ! Last date for harvest          #
        REAL    :: HMPC                ! Harvest moisture percent,std.. # 
        REAL    :: HNAD                ! Product N                      kg/ha
        REAL    :: HNAM                ! Harvest N at maturity          kg/ha
        REAL    :: HNAMERR             ! Harvest N,error                %
        REAL    :: HNAMM               ! Harvest N,harvest,measured     kg/ha
        REAL    :: HNC                 ! Product N concentration,fr     #
        REAL    :: HNPCM               ! Harvest N%,maturity            %
        REAL    :: HNPCMERR            ! Harvest N%,error               %
        REAL    :: HNPCMM              ! Harvest N,mature,measured      %
        INTEGER :: HNUMACOL            ! Harvest number per area column #
        REAL    :: HNUMAD              ! Harvest product#/unit area     #/m2
        REAL    :: HNUMAERR            ! Harvest #,maturity,error       %
        REAL    :: HNUMAM              ! Harvest no/area,maturity       #/m2
        REAL    :: HNUMAMM             ! Harvest no/area,mature,measure #/m2
        REAL    :: HNUMAT              ! Harvest number/area,t file     #/m2
        INTEGER :: HNUMBER             ! Number of harvest instructions #
        INTEGER :: HNUMECOL            ! Harvest number per ear column  #
        REAL    :: HNUMET              ! Harvest number/ear,t file      #/s
        REAL    :: HNUMGERR            ! Harvest #/group,error          %
        REAL    :: HNUMGM              ! Harvest #,mature               #/g
        REAL    :: HNUMGMM             ! Harvest #,mature,measured      #/g
        REAL    :: HNUMPM              ! Product# per plant,maturity    #/p
        REAL    :: HNUMPMM             ! Product #,mature,measured      #/p
        REAL    :: HPC(HANUMX)         ! Harvest percentage             %
        REAL    :: HPCF                ! Harvest percentage,final       %
        REAL    :: HPRODN              ! Harvest product N              g/p
        REAL    :: HSTAGE              ! Hardening stage  0-1           #
        INTEGER :: HSTG                ! Harvest maturity stage         #
        REAL    :: HWAD                ! Product weight                 kg/ha
        INTEGER :: HWADCOL             ! Product wt column number       #
        REAL    :: HWADT               ! Harvest weight from t file     kg/ha
        REAL    :: HWAHERR             ! Harvest wt,harvest,error       %
        REAL    :: HWAHM               ! Harvest wt,harvest,measured    kg/ha
        REAL    :: HWAM                ! Harvest product wt.,maturity   kg/ha
        REAL    :: HWAMM               ! Harvest product wt.,measured   kg/ha
        INTEGER :: HWTUCOL             ! Harvest weight per unit column #
        REAL    :: HWUD                ! Harvest wt/unit                g
        REAL    :: HWUM                ! Harvest product size,maturity  g
        REAL    :: HWUMERR             ! Harvest wt per unit error      %
        REAL    :: HWUMM               ! Hprod wt/unit,mat,measured     g
        REAL    :: HWUMYLD             ! Harvest wt,mature,calculated   g/#
        REAL    :: HWUT                ! Product weight/unit,t file     mg
        REAL    :: HYAMM               ! Harvest product,msured,std.h2o kg/ha
        INTEGER :: HYEAR               ! Harvest year as read           #
        INTEGER :: HYEARDOY(HANUMX)    ! Dates of harvest operations    #
        INTEGER :: HYEARF              ! Earliest year for harvest      #
        INTEGER :: HYEARL              ! Last year for harvest          #
        INTEGER :: HYRDOY(HANUMX)      ! Dates of harvest operations    #
        INTEGER :: I                   ! Loop counter                   #
        REAL    :: ICWD                ! Initial water table depth      cm
        INTEGER :: IDETGNUM            ! Number of times into IDETG     #
        !REAL    :: IRRAMT              ! Irrigation amount for today    mm          ARGUMENT
        REAL    :: IRRAMTC             ! Irrigation amount,cumulative   mm
        REAL    :: ISOILH2O            ! Initial soil water             cm   
        REAL    :: ISOILN              ! Initial soil inorganic N       kg/ha
        !REAL    :: KCAN                ! Extinction coeff for PAR       #           ARGUMENT
        REAL    :: KCANI               ! Extinction coeff,PAR,init.val. #
        !REAL    :: KEP                 ! Extinction coeff for SRAD      #           ARGUMENT
        REAL    :: KEPI                ! Extinction coeff,SRAD,init val #
        INTEGER :: KEYPS(KEYSTX)       ! Principal key stage number     #
        INTEGER :: KEYPSNUM            ! Principal key stage total #    # 
        INTEGER :: L                   ! Loop counter                   #
        INTEGER :: L1                  ! Loop counter                   #
        INTEGER :: L2                  ! Loop counter                   #
        REAL    :: LA1S                ! Area of leaf 1,standard        cm2
        REAL    :: LAFND               ! Node # (one axis)->final area  #
        REAL    :: LAFS                ! Leaf area/all nodes,final      cm2
        REAL    :: LAGEG(0:LNUMX)      ! Leaf age increment             C.d
        REAL    :: LAGEP(0:LNUMX)      ! Leaf age (phyllochrons),lf pos #
        REAL    :: LAGETT(0:LNUMX)     ! Leaf age at leaf position    C.d
        REAL    :: LAGL(1,LNUMX)       ! Leaf area growth,shoot,lf pos  cm2/l
        REAL    :: LAI                 ! Leaf area index                #
        REAL    :: LAIA                ! Leaf area index,active         #
        INTEGER :: LAIDCOL             ! Leaf area index column         #
        !REAL    :: LAIL(30)            ! Leaf area index by layer       m2/m2       ARGUMENT
        !REAL    :: LAILA(30)           ! Leaf area index,active,by layr m2/m2       ARGUMENT
        REAL    :: LAIPREV             ! Leaf area index,previous day   #
        REAL    :: LAIPROD             ! Leaf area index produced       #
        REAL    :: LAISTG(20)          ! Leaf area index,specific stage #
        REAL    :: LAIX                ! Leaf area index,maximum        #
        REAL    :: LAIXERR             ! Leaf area index,max,error      %
        REAL    :: LAIXM               ! Lf lamina area index,mx,meas   m2/m2
        REAL    :: LAIXT               ! Leaf area index,max,t-file     m2/m2
        REAL    :: LAIXX               ! Leaf area index,max posible    #
        REAL    :: LANC                ! Leaf actual N concentration    #
        REAL    :: LANCRS              ! Leaf N+rsN concentration       #
        REAL    :: LAP(0:LNUMX)        ! Leaf area at leaf position     cm2/p
        REAL    :: LAPD                ! Leaf area (green) per plant    cm2
        REAL    :: LAPH                ! Leaf area (green) harvested    cm2/d
        REAL    :: LAPHC               ! Leaf area (green) harvested,cu cm2/p
        REAL    :: LAPOTX(LNUMX)       ! Leaf area potentials,maxima    cm2/l
        REAL    :: LAPP(LNUMX)         ! Leaf area diseased,leaf posn   cm2/p
        REAL    :: LAPS(LNUMX)         ! Leaf area senesced,leaf posn   cm2/p
        REAL    :: LAPSTMP             ! Leaf area senesced,temporary   cm2/p
        REAL    :: LATL(1,LNUMX)       ! Leaf area,shoot,lf#,potential  cm2/l
        REAL    :: LATL2(1,LNUMX)      ! Leaf area,shoot,lf#,+h2o,n,tem cm2/l
        REAL    :: LATL3(1,LNUMX)      ! Leaf area,shoot,lf#,+assim.    cm2/l
        REAL    :: LATL4(1,LNUMX)      ! Leaf area,shoot,lf#,+assim.+N  cm2/l
        REAL    :: LATLPOT(1,LNUMX)    ! Leaf area,shoot,leaf,pot     cm2/l
        REAL    :: LATLPREV(1,LNUMX)   ! Leaf area,shoot,leaf,prev.  cm2/l
        REAL    :: LAWCF               ! Leaf area/wt change,fr.st      fr/lf
        REAL    :: LAWFF               ! Leaf area/wt flexibility,fr.st fr
        REAL    :: LAWL(2)             ! Area to wt ratio,n=youngest lf cm2/g
        REAL    :: LAWMNFR             ! Leaf area/wt ratio,min.fr.std. #    
        REAL    :: LAWS                ! Leaf area/wt ratio,standard    cm2/g
        REAL    :: LAWTR               ! Leaf area/weight,temp response fr/C
        REAL    :: LAWTS               ! Leaf area/weight,temp standard C
        REAL    :: LAWWR               ! Leaf area/weight,water respons fr
        REAL    :: LAXN2               ! Leaf # (one axis),end max.area #
        REAL    :: LAXNO               ! Leaf # (one axis),maximum area #
        REAL    :: LAXS                ! Area of biggest leaf,main stem cm2
        INTEGER :: LBIRTHDAP(LCNUMX)   ! DAP on which leaf initiated #
        INTEGER :: LCNUM               ! Leaf cohort number (inc.grow)  #
        REAL    :: LCOA(LCNUMX)        ! Leaf cohort area               cm2
        REAL    :: LCOAS(LCNUMX)       ! Leaf cohort area senesced      cm2
        INTEGER :: LDEATHDAP(LCNUMX)   ! DAP on which leaf 100% dead #
        REAL    :: LEAFN               ! Leaf N                         g/p
        REAL    :: LEAFNEXCESS         ! Leaf N > critical              g/p
        INTEGER :: LENDIS              ! Length,ISWDIS flag             #
        INTEGER :: LENENAME            ! Length,experiment description  #
        INTEGER :: LENGROUP            ! Length of group name           #
        INTEGER :: LENLINE             ! Length of character string     #
        INTEGER :: LENLINESTAR         ! Length of character string     #
        INTEGER :: LENRNAME            ! Length of run description      #
        INTEGER :: LENTNAME            ! Length,treatment description   #
        REAL    :: LFGSDU              ! Leaf growth start,d.units      #
        REAL    :: LFWT                ! Leaf weight                    g/p
        REAL    :: LFWTM               ! Leaf weight,maturity           g/p
        REAL    :: LGPHASE(2)          ! Leaf growth tier,start,end     #
        REAL    :: LGPHASEDU(2)        ! Leaf growth tier,start,end     Du
        INTEGER :: LINENUM             ! Line number in RMSE values     #
        !REAL    :: LL(20)              ! Lower limit,soil h2o           #           ARGUMENT
        REAL    :: LLIFA               ! Leaf life duration,active,read #    
        REAL    :: LLIFATT             ! Leaf life duration,active      C.d
        REAL    :: LLIFG               ! Leaf growth duration,as read   #   
        REAL    :: LLIFGTT             ! Leaf growth durtion            C.d
        REAL    :: LLIFS               ! Leaf senescence dur,as read    d   
        REAL    :: LLIFSTT             ! Leaf senescence duration,Ttime C.d
        REAL    :: LLIFX               ! Leaf ageing acceleration,max   Tu  
        REAL    :: LLIFXUNUSED         ! Leaf ageing accel.unused       Tu  
        REAL    :: LLIGP               ! Leaf lignin percentage         #
        REAL    :: LLNAD               ! Leaf lamina nitrogen           kg/ha
        REAL    :: LLOSA               ! Leaf area loss,accelerated sen fr
        REAL    :: LLRSWAD             ! Leaf lamina reserves weight    kg/ha
        REAL    :: LLRSWT              ! Leaf lamina reserves           g/p
        REAL    :: LLWAD               ! Leaf lamina weight             kg/ha
        REAL    :: LLWADOUT            ! Leaf lamina weight for output  kg/ha
        REAL    :: LNCGL               ! N concentration,growth,lower   fr
        REAL    :: LNCGU               ! N concentration,growth,upper   fr
        REAL    :: LNCM                ! Leaf N conc,minimum            fr
        REAL    :: LNCMN(0:1)          ! Leaf N conc,minimum            fr
        REAL    :: LNCPL               ! Leaf N concentration,phs,lower fr
        REAL    :: LNCPU               ! Leaf N concentration,phs,upper fr
        REAL    :: LNCR                ! Leaf N relative to maximum     #
        REAL    :: LNCSEN              ! N conc.for senescence          fr
        REAL    :: LNCSENF             ! Leaf N con,senescence,final    fr
        REAL    :: LNCX                ! Leaf N conc,maximum            fr
        REAL    :: LNCXS(0:1)          ! Leaf N conc,maximum,stage      fr
        REAL    :: LNDEM               ! Leaf demand for N              g/p
        REAL    :: LNDEM2              ! Leaf demand for N for growth   g/p
        REAL    :: LNDEMG              ! Leaf demand for N,for growth   g/p
        REAL    :: LNDEMTU             ! Leaf demand for N,for topup    g/p
        REAL    :: LNPCMN(0:1)         ! Leaf N conc,minimum            %
        REAL    :: LNPCS(0:1)          ! Leaf N conc,standard,stage     %
        REAL    :: LNPH                ! Leaf N harvested               g/p
        REAL    :: LNPHC               ! Leaf N harvested,cumulative    g/p
        REAL    :: LNUM                ! Leaf number,Haun stage         #
        INTEGER :: LNUMCOL             ! Leaf number column             #
        REAL    :: LNUMEND             ! Leaf number,Haun stage,end day #
        REAL    :: LNUMG               ! Leaf number increase per day   #
        REAL    :: LNUMNEED            ! Leaf # stage to start new leaf #
        REAL    :: LNUMPREV            ! Leaf number,Haun stage         #
        REAL    :: LNUMSERR            ! Leaf #,error                   %
        INTEGER :: LNUMSG              ! Leaf number produced on axis   #
        REAL    :: LNUMSIMTOSTG(0:PSX) ! Leaf numbers,fork,simul  #
        REAL    :: LNUMSM              ! Leaf #/shoot,Haun,maturity     #
        REAL    :: LNUMSMM             ! Leaf #,mature,measured         #/s
        INTEGER :: LNUMSOLDESTA        ! Leaf number,oldest acive,axis  #
        REAL    :: LNUMSTG(20)         ! Leaf number,specific stage     #
        REAL    :: LNUMT               ! Leaf number from t file        #
        REAL    :: LNUMTMP             ! Leaf #,temporary val for calc  #
        REAL    :: LNUMTOSTG(0:PSX)    ! Leaf numbers at fork stages #
        REAL    :: LNUSE(0:3)          ! Leaf N use,overall and parts   g   
        REAL    :: LPEAI               ! Leaf petiole area index        m2/m2
        REAL    :: LPEAW               ! Leaf petiole area/wt,veg.ph    cm2/g
        REAL    :: LPEFR               ! Leaf petiole fraction of total #
        REAL    :: LPERSWAD            ! Leaf petiole reserves weight   kg/ha
        REAL    :: LPERSWT             ! Leaf petiole reserves          g/p
        REAL    :: LPEWAD              ! Leaf petiole weight            kg/ha
        INTEGER :: LRTIP               ! Layer with root tip            #
        INTEGER :: LSEED               ! Layer with seed                #
        REAL    :: LSENI               ! Leaf senescence,injury         %/d
        REAL    :: LSNUM(HANUMX)       ! Livestock number               #/ha
        REAL    :: LSTAGE              ! Leaf stage 0-1 over leaf tier  #
        REAL    :: LSWT(HANUMX)        ! Livestock weight (individual)  kg
        REAL    :: LWLOS               ! Leaf wt loss,normal senesce    fr
        REAL    :: LWPH                ! Leaf weight harvested          g/p
        REAL    :: LWPHC               ! Leaf wt harvested,cumulative   g/p
        INTEGER :: MDAP                ! Maturity date.Days>planting    #
        REAL    :: MDAPFR              ! Maturity DAP+fraction          #
        INTEGER :: MDAPM               ! Maturity DAP,measured          #
        INTEGER :: MDAT                ! Maturity date.Year+DOY         #
        INTEGER :: MDATERR             ! Maturity date error            d
        INTEGER :: MDATM               ! Maturity date,measured         #
        REAL    :: MDATT               ! Maturity date from t file      YrDoy
        REAL    :: MDAYFR              ! Maturity,fraction of day       #
        INTEGER :: MDOY                ! Maturity day of year           d
        REAL    :: MJPERE              ! Energy per Einstein (300-170)  MJ/E
        INTEGER :: MSTG                ! Maturity stage(eg.black layer) #
        REAL    :: NCRG                ! N factor,root growth           ppm
        REAL    :: NDEM2               ! N demand for growth>minimum    g/p
        REAL    :: NDEMG               ! N demand for growth min->max   g/p
        REAL    :: NDEMMN              ! N demand for growth at minimum g/p
        REAL    :: NFG                 ! N factor,growth 0-1            #
        REAL    :: NFGCAV              ! N factor,growth,average,cycle  #
        REAL    :: NFGCC               ! N factor,growh,cycle sum       # 
        REAL    :: NFGL                ! N factor,gr,lower limit        #
        REAL    :: NFGPAV(0:12)        ! N factor,growth,average,tier   #
        REAL    :: NFGPC               ! N factor,growth,cumulative     #
        REAL    :: NFGU                ! N factor,gr,upper limit        #
        REAL    :: NFLF(LNUMX)         ! N factor for leaf,average      #
        REAL    :: NFLF2(0:LNUMX)      ! N factor for leaf area adj     #
        REAL    :: NFLFP(LNUMX)        ! N factor phs leaf,average      #
        !REAL    :: NFP                 ! N factor,photosynthesis 0-1    #
        REAL    :: NFPCAV              ! N factor,phs,average,cycle     #
        REAL    :: NFPCC               ! N factor,phs,cumulative,cycle  #
        REAL    :: NFPL                ! N factor,phs,lower limit       #
        REAL    :: NFPPAV(0:12)        ! N factor,phs,average,tier      #
        REAL    :: NFPPC               ! N factor,phs,cumulative,tier   #
        REAL    :: NFPU                ! N factor,phs,upper limit       #
        REAL    :: NFRG                ! N factor,root growth 0-1       #
        REAL    :: NFSU                ! N factor,senescence,upper lim  #
        REAL    :: NH4CF               ! NH4 uptake concentration fac   #   
        REAL    :: NH4FN               ! NH4 conc factor,NH4 uptake 0-1 #
        !REAL    :: NH4LEFT(20)         ! NH4 concentration in soil      g/Mg        ARGUMENT
        REAL    :: NH4MN               ! NH4 conc minimum for uptake    g/Mg         ARGUMENT
        REAL    :: NLABPC              ! N labile fraction,standard     %
        !INTEGER :: NLAYR               ! Number of layers in soil       #           ARGUMENT
        INTEGER :: NLAYRROOT           ! Number of layers with roots    #
        REAL    :: NLLG                ! N limit,leaf growth            #
        REAL    :: NO3CF               ! NO3 uptake concentration fac   #    
        REAL    :: NO3FN               ! NO3 conc factor,NO3 uptake 0-1 #
        !REAL    :: NO3LEFT(20)         ! NO3 concentration in soil      g/Mg        ARGUMENT
        REAL    :: NO3MN               ! NO3 conc minimum for uptake    g/Mg
        INTEGER :: NOUTPG              ! Number for growth output file  #
        INTEGER :: NOUTPG2             ! Number for growth output file2 #
        INTEGER :: NOUTPGF             ! Number for growth factors file #
        INTEGER :: NOUTPN              ! Number for growthN output file #
        REAL    :: NPOOLL              ! Leaf N pool (ie.above minimum) g/p
        REAL    :: NPOOLR              ! Root N pool (ie.above minimum) g/p
        REAL    :: NPOOLS              ! Stem N pool (ie.above minimum) g/p
        INTEGER :: NSDAYS              ! N stress days                  #
        REAL    :: NTUPF               ! N top-up fraction              /d
        REAL    :: NUF                 ! Plant N supply/demand,max=1.0  #
        REAL    :: NULEFT              ! N uptake remaining for use     g 
        REAL    :: NUPAC               ! N uptake,cumulative            kg/ha
        REAL    :: NUPACM              ! N uptake,cumulative,measured   kg/ha
        REAL    :: NUPAD               ! N uptake rate (/d)             kg/ha
        REAL    :: NUPAP               ! Total root N uptake rate,potnl kg/ha
        REAL    :: NUPAPCRP            ! Total N uptake rate,potnl,CSCR kg/ha
        REAL    :: NUPAPCSM            ! Total N uptake rate,potnl,CSM  kg/ha
        REAL    :: NUPAPCSM1           ! Total N uptake rate,pot,CSMmod kg/ha
        REAL    :: NUPC                ! N uptake,cumulative            g/p
        REAL    :: NUPD                ! N uptake                       g/p
        REAL    :: NUPRATIO            ! N uptake potential/demand      #
        REAL    :: NUSEFAC             ! N use factor                   #
        !INTEGER :: ON                  ! Option number (sequence runs)  #           ARGUMENT
        INTEGER :: ONI                 ! Option number,initial value    #
        INTEGER :: OUTCHOICE           ! Output choice (+reserves,etc)  #
        INTEGER :: OUTCOUNT            ! Output counter                 #
        REAL    :: PARFC               ! Max photosynthesis/phs at 330  #
        REAL    :: PARHR(TS)           ! PAR, hourly                    MJ/m2        MF 15SE14 For hourly PS and VPD
        REAL    :: PARI                ! PAR interception fraction      #
        REAL    :: PARI1               ! PAR interception fr,1-crop mdl #
        REAL    :: PARIOUT             ! PAR interception fr for output #
        !REAL    :: PARIP               ! PAR interception percentage    %           ARGUMENT
        !REAL    :: PARIPA              ! PAR interception %, active     %           ARGUMENT
        REAL    :: PARIPREV            ! PAR interception fr,previous   #
        REAL    :: PARIUE              ! PAR intercepted use efficiency g/MJ
        REAL    :: PARIUED             ! PAR intercepted use efficiency g/MJ
        REAL    :: PARIX               ! PAR interception,maximum,fr    #
        REAL    :: PARMJC              ! PAR,cumulative                 MJ/m2
        REAL    :: PARMJFAC            ! PAR conversion factor          MJ/MJ
        REAL    :: PARMJIADJ           ! PAR intercepted adjustment     MJ/m2
        REAL    :: PARMJIC             ! PAR intercepted,cumulative     MJ/m2
        REAL    :: PARU                ! PAR utilization effic          g/MJ
        REAL    :: PARU2               ! PAR use efficiency,afterchange g/MJ
        REAL    :: PARUE               ! PAR use efficiency,standard    g/MJ
        REAL    :: PARUEC              ! PAR use efficiency to today    g/MJ
        REAL    :: PARURFR             ! PAR utilize factor,reprod fr   #
        INTEGER :: PATHL               ! Path length                    #
        REAL    :: PD(0:PSX)           ! Tier durations                 deg.d
        REAL    :: PD2ADJ              ! Tier 2+3 adjusted              deg.d
        REAL    :: PDADJ               ! Tier duration adjustment       deg.d
        INTEGER :: PDATE               ! Planting Yrdoy from X-file     #
        INTEGER :: PDAYS(0:12)         ! Tier durations                 PVoCd
        REAL    :: PDFS                ! Final senescenc duration       deg.d
        REAL    :: PDL(0:PSX)          ! Tier durations,phint units     #    
        REAL    :: PECM                ! Emergence duration             Cd/cm
        REAL    :: PEGD                ! Duration,germ+dormancy         deg.d
        REAL    :: PFGCAV              ! P factor,growh,cycle,av 0-1    # 
        REAL    :: PFGPAV(0:12)        ! P factor,growh,tier,av 0-1     # 
        REAL    :: PFPCAV              ! P factor,phs,cycle,average 0-1 #
        REAL    :: PFPPAV(0:12)        ! P factor,phs,tier,average 0-1  #
        INTEGER :: PGDAP               ! Plantgro file days after plt   #
        REAL    :: PGERM               ! Germination duration           deg.d
        INTEGER :: PGROCOL(20)         ! Plantgro column = t-file data  #
        REAL    :: PGVAL               ! Plantgro file value            #
        REAL    :: PHINT               ! Phylochron interval            deg.d
        REAL    :: PHINTFAC            ! Phylochron interval factor     #
        REAL    :: PHINTS              ! Phylochron interval,standard   deg.d
        INTEGER :: PHINTSTG            ! Phylochron stage               #    
        REAL    :: PHOTQR              ! Photon requirement,calculated  E/mol
        REAL    :: PHSV                ! Phs,fr reduction with VPD      /KPa
        REAL    :: PHTV                ! Phs,threshold VPD for reductin KPa
        REAL    :: PLA                 ! Plant leaf area                cm2/p
        REAL    :: PLAGS2              ! Plant lf area g,all shts,H2oNT cm2/p
        REAL    :: PLAGSB2             ! Plant lf area g,sh+br,H2oNT    cm2/p
        REAL    :: PLAGSB3             ! Plant lf area g,sh+br,H2oNTA   cm2/p
        REAL    :: PLAGSB4             ! Plant lf area g,sh+br,H2oNTAN2 cm2/p
        REAL    :: PLAS                ! Leaf area senesced,normal      cm2/p
        REAL    :: PLASI               ! Leaf area senesced,injury      cm2/p
        REAL    :: PLASL               ! Leaf area senesced,low light   cm2/p
        REAL    :: PLASN               ! Leaf area senesced,N shortage  cm2/p
        REAL    :: PLASP               ! Leaf area senesced,phyllochron cm2/p
        REAL    :: PLASS               ! Leaf area senesced,stress      cm2/p
        REAL    :: PLASTMP             ! Leaf area senesced,temporary   cm2/p
        REAL    :: PLASTMP2            ! Leaf area senesced,temporary   cm2/p
        REAL    :: PLASW               ! Leaf area senesced,h2o stress  cm2/p
        REAL    :: PLAX                ! Plant leaf area,maximum        cm2
        INTEGER :: PLDAY               ! Planting day of year           d
        INTEGER :: PLDAYTMP            ! Planting day of year           #
        REAL    :: PLMAGE              ! Planting material age          d
        REAL    :: PLPH                ! Plants/hill or shoots/cutting  # 
        INTEGER :: PLTOHARYR           ! Planting to final harvestyears #
        REAL    :: PLTPOP              ! Plant Population               #/m2
        REAL    :: PLTPOPE             ! Plant Population established   #/m2
        REAL    :: PLTPOPP             ! Plant Population planned       #/m2
        INTEGER :: PLYEAR              ! Planting year                  #
        INTEGER :: PLYEARDOY           ! Planting year*1000+DOY         #
        INTEGER :: PLYEARDOYPREV       ! Year+Doy for planting,previous #
        INTEGER :: PLYEARDOYT          ! Planting year*1000+DOY target  #
        INTEGER :: PLYEARREAD          ! Planting year as read          #
        INTEGER :: PLYEARTMP           ! Year(Yr)+Doy,planting tem val  #
        REAL    :: PPEXP               ! Photoperiod response exponent  #
        REAL    :: PPTHR               ! Photoperiod threshold          h
        INTEGER :: PSDAP (PSX)         ! Stage DAP                      #
        REAL    :: PSDAPFR(PSX)        ! Stage DAP+fr                   #
        INTEGER :: PSDAPM (PSX)        ! Stage DAP,measured             #
        INTEGER :: PSDAT (PSX)         ! Stage YrDoydate                #
        INTEGER :: PSDATM (PSX)        ! Stage date,measured            #
        REAL    :: PSDAYFR(PSX)        ! Stage fraction of day          #
        INTEGER :: PSIDAP              ! Principal stage,inter,date     dap
        INTEGER :: PSIDAPM             ! Principal stg,inter,measured   dap
        INTEGER :: PSIDATERR           ! Principal stage,inter,error    dap 
        INTEGER :: PSNUM               ! Principal stage number         #
        REAL    :: PSTART(0:PSX)       ! Principal thresholds           du
        REAL    :: PTF                 ! Partition fraction to tops     #
        REAL    :: PTFA                ! Partition fr adjustment coeff. #
        REAL    :: PTFMN               ! Partition fraction,minimum     #
        REAL    :: PTFMX               ! Partition fraction,maximum     #
        REAL    :: PTTN                ! Minimum soil temperature,plt   C
        REAL    :: PTX                 ! Maximum soil temperature,plt   C
        INTEGER :: PWDINF              ! First YrDoy of planting window #
        INTEGER :: PWDINL              ! Last YrDoy of planting window  #
        INTEGER :: PWDOYF              ! First doy of planting window   #
        INTEGER :: PWDOYL              ! Last doy of planting window    #
        INTEGER :: PWYEARF             ! First year of planting window  #
        INTEGER :: PWYEARL             ! Last year of planting window   #
        REAL    :: RADHR(TS)           ! Solar radiation, hourly        MJ/m2        MF 15SE14 For hourly PS and VPD
        !REAL    :: RAIN                ! Rainfall                       mm          ARGUMENT
        REAL    :: RAINC               ! Rainfall,cumulative            mm
        REAL    :: RAINCC              ! Precipitation cycle sum        mm 
        REAL    :: RAINPAV(0:12)       ! Rainfall,average for tier      mm
        REAL    :: RAINPC(0:12)        ! Precipitation tier sum         mm
        REAL    :: RANC                ! Roots actual N concentration   #
        REAL    :: RATM                ! Boundary layer,air,resistance  s/m
        REAL    :: RB                  ! Leaf resistance addition fac   s/m
        REAL    :: RCROP               ! Stomatal res,crop basis        s/m
        REAL    :: RDGAF               ! Root depth gr,acceleration fac #
        REAL    :: RDGS                ! Root depth growth rate,standrd cm/d
        !INTEGER :: REP                 ! Number of run repetitions      #           ARGUMENT
        REAL    :: RESCAL(0:20)        ! Residue C at maturity,by layer kg/ha
        !REAL    :: RESCALG(0:20)       ! Residue C added,by layer       kg/ha       ARGUMENT
        REAL    :: RESLGAL(0:20)       ! Residue lignin,maturity,layer  kg/ha
        !REAL    :: RESLGALG(0:20)      ! Residue lignin added,layer     kg/ha
        REAL    :: RESNAL(0:20)        ! Residue N at harvest by layer  kg/ha
        !REAL    :: RESNALG(0:20)       ! Residue N added,by layer       kg/ha       ARGUMENT
        REAL    :: RESPC               ! Respiration,total,cumulative   g/p
        REAL    :: RESPRC              ! Respiration,roots,cumulative   g/p
        REAL    :: RESPTC              ! Respiration,tops,cumulative    g/p
        REAL    :: RESWAL(0:20)        ! Residue om added by layer      kg/ha
        REAL    :: RESWALG(0:20)       ! Residue om,maturity,by layer   kg/ha
        REAL    :: RFAC                ! Root length & H2O fac,N uptake #
        REAL    :: RHUMHR(TS)          ! Relative humidity, hourly      %            MF 15SE14 For hourly PS and VPD
        REAL    :: RLDF(20)            ! Root length density fac,new gr #
        REAL    :: RLF                 ! Leaf stomatal res,330.0 ppmCO2 s/m
        REAL    :: RLFC                ! Leaf stomatal resistance       s/m
        REAL    :: RLFN                ! Root length factor,N           #
        REAL    :: RLFWU               ! Root length factor,water uptk  /cm2
        REAL    :: RLIGP               ! Root lignin concentration      %
        !REAL    :: RLV(20)             ! Root length volume by layer    cm-2        ARGUMENT
        REAL    :: RLWR                ! Root length/weight ratio       m/10mg
        REAL    :: RM                  ! Mesophyll resistance           d/m
        REAL    :: RMSE(30)            ! Root mean square error values  #
        !INTEGER :: RN                  ! Treatment replicate            #           ARGUMENT
        REAL    :: RNAD                ! Root N                         kg/ha
        REAL    :: RNAM                ! Root N at maturity             kg/ha
        REAL    :: RNAMM               ! Root N at maturity,measured    kg/ha
        REAL    :: RNCM                ! Root N conc,minimum            fr
        REAL    :: RNCMN(0:1)          ! Root N conc,minimum            fr
        REAL    :: RNCR                ! Roots N relative to maximum    #
        REAL    :: RNCX                ! Root N concentration,maximum   fr
        REAL    :: RNCXS(0:1)          ! Roots N conc,maximum,by stage  fr
        REAL    :: RNDEM               ! Root demand for N              g/p
        REAL    :: RNDEMG              ! Root demand for N,for growth   g/p
        REAL    :: RNDEMTU             ! Root demand for N,for topup    g/p
        REAL    :: RNH4U(20)           ! Potential ammonium uptake      kg/ha
        INTEGER :: RNI                 ! Replicate number,initial value #
        REAL    :: RNO3U(20)           ! Potential nitrate uptake       kg/ha
        REAL    :: RNPCMN(0:1)         ! Root N conc,minimum            %
        REAL    :: RNPCS(0:1)          ! Roots N conc,standard,by stage %
        REAL    :: RNUMX               ! Root N uptake,maximum          mg/cm
        REAL    :: RNUSE(0:2)          ! Root N use,overall and parts   g   
        REAL    :: ROOTN               ! Root N                         g/p
        REAL    :: ROOTNEXCESS         ! Root N > critical              g/p
        REAL    :: ROOTNS              ! Root N senesced                g/p
        REAL    :: ROWSPC              ! Row spacing                    cm
        INTEGER :: RPCOL               ! Replicate column number        #
        REAL    :: RRESP               ! Root respiration fraction      #
        REAL    :: RSCD                ! Reserves concentration,end day fr
        REAL    :: RSCLX               ! Reserves conc,leaves,max.      #
        REAL    :: RSCM                ! Reserves concentration,mature  fr
        REAL    :: RSCMM               ! Reserves conc,maturity,msured  #
        REAL    :: RSCX                ! Max.reserves conc.reached      fr
        REAL    :: RSEN                ! Root senescence fraction       #
        REAL    :: RSFP                ! Reserves factor,photosynthesis fr
        REAL    :: RSFPL               ! Reserves conc.,phs.lower bound fr
        REAL    :: RSFPU               ! Reserves conc.,phs upper bound fr
        REAL    :: RSFRS               ! Reserves fr.assim.for stem,std fr 
        REAL    :: RSGLFADJ            ! Reserves from leaf adj to N    g/p
        REAL    :: RSN                 ! Reserve N                      g/p
        REAL    :: RSNAD               ! Reserve N                      kg/ha
        REAL    :: RSNEED              ! Reserves need to bring to min  g/p
        REAL    :: RSNPH               ! Reserves N harvested           g/p
        REAL    :: RSNPHC              ! Reserves N harvested,cum       g/p
        REAL    :: RSNUSED             ! Reserve N used                 g/p  
        REAL    :: RSPCO               ! Res conc.above which overflow  %
        REAL    :: RSSRWTGLFADJ        ! Reserves+storage from lf N adj g/p
        REAL    :: RSSRWTGSTADJ        ! Reserves+storage from st N adj g/p
        REAL    :: RSUSE               ! Reserves utilisation fraction  #
        REAL    :: RSWAD               ! Reserves weight                kg/ha
        REAL    :: RSWAM               ! Reserves at maturity           kg/ha
        REAL    :: RSWAMM              ! Reserves at maturity,measured  kg/ha
        REAL    :: RSWPH               ! Reserves weight harvested      g/p
        REAL    :: RSWPHC              ! Reserves wt harvested,cum      g/p
        REAL    :: RSWT                ! Reserves weight                g/p
        REAL    :: RSWTGLFADJ          ! Reserves from lf N adjustment  g/p
        REAL    :: RSWTM               ! Reserves weight,maturity       g/p
        REAL    :: RSWTPREV            ! Reserves weight,previous       g/p
        REAL    :: RSWTTMP             ! Reserves weight,temporary val  g/p
        REAL    :: RSWTX               ! Reserves weight,maximum        g/p
        REAL    :: RTDEP               ! Root depth                     cm
        REAL    :: RTDEPG              ! Root depth growth              cm/d
        REAL    :: RTDEPTMP            ! Root depth,temporary value     cm/d
        REAL    :: RTNH4               ! N uptake/root length           mg/cm
        REAL    :: RTNO3               ! N uptake/root length           mg/cm
        REAL    :: RTNSL(20)           ! Root N senesced by layer       g/p
        REAL    :: RTRESP              ! Root respiration               g/p
        REAL    :: RTRESPADJ           ! Root respiration rate N adj    g/p
        INTEGER :: RTSLXDATE           ! Roots into last layer date     YYDDD
        REAL    :: RTUFR               ! Max fraction root wt useable   fr
        REAL    :: RTWT                ! Root weight                    g/p
        REAL    :: RTWTAL(20)          ! Root weight by layer           kg/ha
        REAL    :: RTWTG               ! Root weight growth             g/p
        REAL    :: RTWTGADJ            ! Root growth rate N adjusted    g/p
        REAL    :: RTWTGL(20)          ! Root weight growth by layer    g/p
        REAL    :: RTWTL(20)           ! Root weight by layer           g/p
        REAL    :: RTWTM               ! Root weight,maturity           g/p
        REAL    :: RTWTSL(20)          ! Root weight senesced by layer  g/p
        REAL    :: RTWTUL(20)          ! Root weight used for tops,lyr  g/p
        !INTEGER :: RUN                 ! Run (from command line) number #           ARGUMENT
        INTEGER :: RUNCRP              ! Run (internal within module)   #
        !INTEGER :: RUNI                ! Run (internal for sequences)   #           ARGUMENT
        !REAL    :: RUNOFF              ! Calculated runoff              mm/d        ARGUMENT
        REAL    :: RUNOFFC             ! Calculated runoff,cumulative   mm   
        REAL    :: RWAD                ! Root weight                    kg/ha
        REAL    :: RWAM                ! Root weight,maturity           kg/ha
        REAL    :: RWAMM               ! Root wt at maturity,measured   kg/ha
        !REAL    :: RWUMX               ! Root water uptake,max cm3/cm.d cm2.d       ARGUMENT
        REAL    :: RWUMXI              ! Root water uptake,max,init.val cm2/d
        !REAL    :: RWUPM               ! Pore size for maximum uptake   fr          ARGUMENT
        REAL    :: SAID                ! Stem area index                m2/m2
        REAL    :: SANC                ! Stem N concentration           #
        REAL    :: SANCOUT             ! Stem+LeafPetiole N conc        #
        !REAL    :: SAT(20)             ! Saturated limit,soil           #           ARGUMENT
        REAL    :: SAWS                ! Stem area to wt ratio,standard cm2/g
        REAL    :: SDCOAT              ! Non useable material in seed   g
        REAL    :: SDDUR               ! Seed reserves use duration     d
        REAL    :: SDEPTH              ! Sowing depth                   cm
        REAL    :: SDEPTHU             ! Sowing depth,uppermost level   cm
        REAL    :: SDNAD               ! Seed N                         kg/ha
        REAL    :: SDNAP               ! Seed N at planting             kg/ha
        REAL    :: SDNC                ! Seed N concentration           #
        REAL    :: SDNPCI              ! Seed N concentration,initial   %
        REAL    :: SDRATE              ! Seeding 'rate'                 kg/ha
        REAL    :: SDRSF               ! Seed reserves fraction of seed #
        REAL    :: SDSZ                ! Seed size                      g
        REAL    :: SDWAD               ! Seed weight                    kg/ha
        REAL    :: SDWAM               ! Seed at maturity               kg/ha
        REAL    :: SEEDN               ! Seed N                         g/p
        REAL    :: SEEDNI              ! Seed N,initial                 g/p
        REAL    :: SEEDNUSE            ! N use from seed                g
        REAL    :: SEEDNUSE2           ! N use from seed,supplementary  g
        REAL    :: SEEDRS              ! Seed reserves                  g/p
        REAL    :: SEEDRSAV            ! Seed reserves available        g/p
        REAL    :: SEEDRSAVR           ! Seed reserves available,roots  g/p
        REAL    :: SEEDRSI             ! Seed reserves,initial          g/p
        REAL    :: SEEDUSE             ! Seed reserves use              g/p
        REAL    :: SEEDUSER            ! Seed reserves use,roots        g/p
        REAL    :: SEEDUSET            ! Seed reserves use,tops         g/p
        REAL    :: SENCAGS             ! Senesced C added to soil       kg/ha
        !REAL    :: SENCALG(0:20)       ! Senesced C added,by layer      kg/ha       ARGUMENT
        REAL    :: SENCAS              ! Senesced C added to soil       kg/ha
        REAL    :: SENCL(0:20)         ! Senesced C,by layer            g/p
        REAL    :: SENCS               ! Senesced C added to soil       g/p
        REAL    :: SENFR               ! Senesced fraction lost from pl #
        REAL    :: SENLA               ! Senesced leaf area,total       cm2/p
        REAL    :: SENLAGS             ! Senesced lignin added to soil  kg/ha
        !REAL    :: SENLALG(0:20)       ! Senesced lignin added,layer    kg/ha       ARGUMENT
        REAL    :: SENLALITTER         ! Senesced leaf area,litter      cm2/p
        REAL    :: SENLAS              ! Senesced lignin added to soil  kg/ha
        REAL    :: SENLFG              ! Senesced leaf                  g/p
        REAL    :: SENLFGRS            ! Senesced leaf to reserves      g/p
        REAL    :: SENLL(0:20)         ! Senesced lignin added,by layer g/p
        REAL    :: SENLS               ! Senesced lignin added to soil  g/p
        REAL    :: SENNAGS             ! Senesced N added to soil       kg/ha
        REAL    :: SENNAL(0:20)        ! Senesced N,by layer            kg/ha
        !REAL    :: SENNALG(0:20)       ! Senesced N added,by layer      kg/ha       ARGUMENT
        REAL    :: SENNAS              ! Senesced N added to soil       kg/ha
        REAL    :: SENNATC             ! Senesced N,litter+soil,cum     kg/ha
        REAL    :: SENNATCM            ! Senesced N,litter+soil,cum,mes kg/ha
        REAL    :: SENNGS              ! Senesced N added to soil       g/p
        REAL    :: SENNL(0:20)         ! Senesced N,by layer            g/p
        REAL    :: SENNLFG             ! Senesced N from leaves         g/p
        REAL    :: SENNLFGRS           ! Senesced N from leaves,to rs   g/p
        REAL    :: SENNS               ! Senesced N added to soil       g/p
        REAL    :: SENROOT             ! Senesced weight,soil,cum       g/p
        REAL    :: SENROOTA            ! Senesced weight,soil,cumulativ kg/ha
        REAL    :: SENRTG              ! Senescent root material growth g/p
        REAL    :: SENSTFR             ! Senesced stem fraction         fr/d
        REAL    :: SENTOPLITTER        ! Dead wt.to litter              g/p
        REAL    :: SENTOPLITTERA       ! Dead wt.to litter              kg/ha
        REAL    :: SENTOPLITTERG       ! Senescent top->litter growth   g/p
        REAL    :: SENWACM             ! Senesced weight,total,cum to m kg/ha
        REAL    :: SENWACMM            ! Senesced om,litter+soil,cum,ms kg/ha
        REAL    :: SENWAGS             ! Senesced weight added to soil  kg/ha
        REAL    :: SENWAL(0:20)        ! Senesced om by layer           kg/ha
        REAL    :: SENWALG(0:20)       ! Senesced om added by layer     kg/ha
        REAL    :: SENWL(0:20)         ! Senesced om (cumulative),layer g/p
        REAL    :: SERX                ! Shoot elongation rate,max      cm/Du
        INTEGER :: SHDAP               ! Shoot prodn.start DAP          #
        INTEGER :: SHDAT               ! Shoot prodn.startdate YEARDOY  #
        !REAL    :: SHF(20)             ! Soil hospitality factor 0-1    #           ARGUMENT
        REAL    :: SHGR(22)            ! Shoot size relative to 1       #
        REAL    :: SHLA(25)            ! Shoot leaf area produced       cm2
        REAL    :: SHLAG2(25)          ! Shoot lf area gr,1 axis,H2oNt  cm2
        REAL    :: SHLAGB2(25)         ! Shoot lf area gr,+br,H2oNt     cm2
        REAL    :: SHLAGB3(25)         ! Shoot lf area gr,+br,H2oNtA    cm2
        REAL    :: SHLAGB4(25)         ! Shoot lf area gr,+br,H2oNtAN2  cm2
        REAL    :: SHLAS(25)           ! Shoot leaf area senesced       cm2
        REAL    :: SHNUM               ! Shoot (incl.main stem) number  #/p
        REAL    :: SHNUMAD             ! Shoot (incl.main stem) number  #/m2
        REAL    :: SHNUML(LNUMX)       ! Shoot # at leaf position       #/p
        REAL    :: SHRTD               ! Shoot/root ratio               #
        REAL    :: SHRTM               ! Shoot/root ratio,maturity      #
        REAL    :: SHRTMM              ! Shoot/root ratio,maturity,meas #
        REAL    :: SLA                 ! Specific leaf area             cm2/g
        REAL    :: SLAOUT              ! Specific leaf area for output  cm2/g
        REAL    :: SLIGP               ! Stem lignin concentration      %
        !REAL    :: SLPF                ! Soil factor for photosynthesis %           ARGUMENT
        REAL    :: SMDFR               ! Soil moisture factor,N uptake  #
        !INTEGER :: SN                  ! Sequence number,crop rotation  #           ARGUMENT
        REAL    :: SNAD                ! Stem N (stem+petiole+rs)       kg/ha
        REAL    :: SNCM                ! Stem N conc,minimum            fr
        REAL    :: SNCMN(0:1)          ! Stem N conc,minimum            fr
        REAL    :: SNCR                ! Stem N relative to maximum     #
        REAL    :: SNCX                ! Stem N conc,maximum            fr
        REAL    :: SNCXS(0:1)          ! Stem N conc,maximum,stage      fr
        REAL    :: SNDEM               ! Stem demand for N              g/p
        REAL    :: SNDEMG              ! Stem demand for N,for growth   g/p
        REAL    :: SNDEMTU             ! Stem demand for N,for topup    g/p
        REAL    :: SNDN                ! Time of sunset                 hr           MF 23SE14 Added for integral of VPD
        REAL    :: SNH4(20)            ! Soil NH4 N                     kg/ha
        REAL    :: SNH4PROFILE         ! Soil NH4 N in profile          kg/ha
        REAL    :: SNH4ROOTZONE        ! Soil NH4 N in root zone        kg/ha
        INTEGER :: SNI                 ! Sequence number,as initiated   #
        REAL    :: SNO3(20)            ! Soil NO3 N                     kg/ha
        REAL    :: SNO3PROFILE         ! Soil NO3 N in profile          kg/ha
        REAL    :: SNO3ROOTZONE        ! Soil NO3 N in root zone        kg/ha
        REAL    :: SNPCMN(0:1)         ! Stem N conc,minimum            %
        REAL    :: SNPCS(0:1)          ! Stem N conc,standard,stage     %
        REAL    :: SNPH                ! Stem N harvested               g/p
        REAL    :: SNPHC               ! Stem N harvested,cumulative    g/p
        REAL    :: SNUP                ! Time of sunrise                hr           MF 23SE14 Added for integral of VPD
        REAL    :: SNUSE(0:2)          ! Shoot N use,overall and parts  g    
        REAL    :: SPRL                ! Sprout/cutting length          cm
        !REAL    :: SRAD                ! Solar radiation                MJ/m2
        REAL    :: SRAD20              ! Solar radiation av,20 days     MJ/m2
        REAL    :: SRAD20S             ! Solar radiation sum            MJ/m2
        REAL    :: SRADC               ! Solar radiation,cumulative     MJ/m2
        REAL    :: SRADCAV             ! Solar radiation,cycle average  MJ/m2
        REAL    :: SRADCC              ! Radiation,cycle sum            Mj/m2
        REAL    :: SRADD(20)           ! Solar radiation on specific d  MJ/m2
        REAL    :: SRADPAV(0:12)       ! Solar radiation,tier average   MJ/m2
        REAL    :: SRADPC              ! Solar radiation,tier sum       MJ/m2
        REAL    :: SRADPREV            ! Solar radiation,previous day   MJ/m2
        REAL    :: SRANC               ! Storage root N concentration   g/p
        REAL    :: SRDAYFR             ! Storage root fraction of day   #
        REAL    :: SRFR                ! Storage root fraction,basic    #
        REAL    :: SRNAD               ! Storage root N                 kg/ha
        REAL    :: SRNAM               ! Storage root N at maturity     kg/ha
        REAL    :: SRNDEM              ! Storage root demand for N      g/p
        REAL    :: SRNDEMG             ! St.rt.demand for N,for growth  g/p
        REAL    :: SRNDEMTU            ! St.rt.demand for N,for topup   g/p
        REAL    :: SRNOAD              ! Storage root/group,given day   #/m2
        REAL    :: SRNOAM              ! Storage root #/area,maturity   #/m2
        REAL    :: SRNOAMM             ! Storage root/group,mature,meas #
        REAL    :: SRNOGM              ! Storage root/group,maturity    #
        REAL    :: SRNOGMM             ! Storage root/group,mature,meas #
        INTEGER :: SRNOPD              ! Storage root number per plant  # 
        REAL    :: SRNOW               ! Cultivar coeff,storage root #  #/g
        REAL    :: SRNPCM              ! Storage root N%,maturity       %
        REAL    :: SRNPCS              ! Storage root N%,standard       #
        REAL    :: SRNUSE(0:2)         ! Storage root N use,total/parts g   
        REAL    :: SROOTN              ! Storage root N                 g/p
        REAL    :: SRPRS               ! Storage protein standard %     #
        REAL    :: SRWAD               ! Storage root weight            kg/ha
        REAL    :: SRWT                ! Root storage organ weight      g/p
        REAL    :: SRWTGRS             ! Root storage,reserves>std.%    g/p
        REAL    :: SRWUD               ! Storage root size              g
        REAL    :: SRWUM               ! Storage root wt/unit,maturity  g
        REAL    :: SRWUMM              ! Storage root wt/unit,mat,meas  g
        !REAL    :: ST(0:NL)            ! Soil temperature in soil layer C
        REAL    :: STADJ               ! Stem adjustment,N triggered    #
        REAL    :: STAI                ! Stem area index                m2/m2
        REAL    :: STAIG               ! Stem area index,growth         m2/m2
        REAL    :: STAIS               ! Stem area index senesced       m2/m2
        REAL    :: STAISS              ! Stem area index,start senesce  m2/m2
        INTEGER :: STARNUM             ! Star line number,as read file  #
        INTEGER :: STARNUMM            ! Star line number,measured data #
        INTEGER :: STARNUMO            ! Star line number,output file   #
        INTEGER :: STARTPOS            ! Starting position in line      #
        REAL    :: STDAY               ! Standard day                   C.d/d
        REAL    :: STEMN               ! Stem N                         g/p
        REAL    :: STEMNEXCESS         ! Stem N > critical              g/p
        !INTEGER :: STEP                ! Step number                    #           ARGUMENT
        INTEGER :: STEPNUM             ! Step number per day            #
        INTEGER :: STGEDAT             ! Stem growth end date (Yrdoy)   #
        REAL    :: STGEFR              ! Stem growth end time,fr day    #
        !INTEGER :: STGYEARDOY(20)      ! Stage dates (Year+Doy)         #           ARGUMENT
        REAL    :: STRESS(20)          ! Min h2o,n factors for growth   #
        REAL    :: STRESS20            ! 20d av.,min h2o,n gr.factors   #
        REAL    :: STRESS20N           ! 20d av.,n gr.factor            #
        REAL    :: STRESS20NS          ! 20d sum,n gr.factors           #
        REAL    :: STRESS20S           ! 20d sum.,min h2o,n gr.factors  #
        REAL    :: STRESS20W           ! 20d av.,h2o gr.factor          #
        REAL    :: STRESS20WS          ! 20d sum,h2o gr.factors         #
        REAL    :: STRESSN(20)         ! 20d n gr.factors               #
        REAL    :: STRESSW(20)         ! 20d h2o gr.factors             #
        REAL    :: STRSWAD             ! Stem reserves                  kg/ha
        REAL    :: STRSWT              ! Stem reserves                  g/p
        REAL    :: STWAD               ! Stem structural weight         kg/ha
        REAL    :: STWADOUT            ! Stem weight for output         kg/ha
        REAL    :: STWT                ! Stem weight                    g/p
        REAL    :: STWTM               ! Stem weight,maturity           g/p
        !REAL    :: SW(20)              ! Soil water content             #           ARGUMENT
        REAL    :: SWFR                ! Stem fraction,actual           #
        REAL    :: SWFRN               ! Stem fraction minimum          #
        REAL    :: SWFRNL              ! Leaf number for min stem fr    #
        REAL    :: SWFRPREV            ! Stem fraction,actual,previous  #
        REAL    :: SWFRS               ! Stem fraction,standard         #
        REAL    :: SWFRX               ! Stem fraction maximum          #
        REAL    :: SWFRXL              ! Leaf number for max stem fr    #
        REAL    :: SWP(0:20)           ! Soil water 'potential'         #
        REAL    :: SWPH                ! Stem weight harvested          g/p
        REAL    :: SWPHC               ! Stem wt harvested,cumulative   g/p
        REAL    :: SWPLTD              ! Depth for average soil water   cm
        REAL    :: SWPLTH              ! Upper limit on soil water,plt  %
        REAL    :: SWPLTL              ! Lower limit on soil water,plt  %
        REAL    :: SWPRTIP             ! Soil water potential,root tip  #
        REAL    :: SWPSD               ! Soil water potential at seed   #
        REAL    :: TAIRHR(TS)          ! Hourly air temperature         C           ARGUMENT
        REAL    :: TCAN                ! Canopy temperature             C
        REAL    :: TCDIF               ! Canopy temperature - air temp  C
        INTEGER :: TDATANUM            ! Number of data from t-file     #
        REAL    :: TDEW                ! Dewpoint temperature           C           ARGUMENT
        REAL    :: TDIFAV              ! Temperature difference,can-air C
        REAL    :: TDIFNUM             ! Temperature difference,# data  #
        REAL    :: TDIFSUM             ! Temperature difference,sum     C
        !REAL    :: TFAC4               ! Temperature factor function    #           FUNCTION
        INTEGER :: TFCOLNUM            ! T-file column number           #
        REAL    :: TFD                 ! Temperature factor,development #
        INTEGER :: TFDAP               ! T-file days after planting     #
        INTEGER :: TFDAPCOL            ! T-file DAP column #            #
        REAL    :: TFDF                ! Temperature factor,dayl sens   #
        REAL    :: TFDLF(LNUMX)        ! Temp factor,dev for leaf,av    #
        REAL    :: TFDNEXT             ! Temperature factor,development #
        REAL    :: TFG                 ! Temperature factor,growth 0-1  #
        REAL    :: TFGEM               ! Temperature factor,germ,emrg   #
        REAL    :: TFGLF(LNUMX)        ! Temp factor,gr for leaf,av     #
        REAL    :: TFLAW               ! Temperature factor,lf area/wt  #
        REAL    :: TFLFLIFE            ! Temperature factor,leaf life   #
        REAL    :: TFP                 ! Temperature factor,phs 0-1     #
        REAL    :: TFV                 ! Temperature factor,vernalizatn #
        REAL    :: TFVAL               ! T-file value                   #
        INTEGER :: TIERNUM             ! Tier of data in t-file         #
        REAL    :: TIMENEED            ! Time needed to finish tier     fr
        REAL    :: TLCHC               ! Cumulative N leached>planting  kg/ha
        !REAL    :: TLCHD               ! N leached this day             kg/ha       ARGUMENT
        INTEGER :: TLINENUM            ! Temporary var,# lines in tfile #
        INTEGER :: TLPOS               ! Position on temporary line     #
        REAL    :: TMAX                ! Temperature maximum            C           ARGUMENT
        REAL    :: TMAXCAV             ! Temperature,maximum,cycle av   C
        REAL    :: TMAXCC              ! Temperature,max,cycle sum      C.d 
        REAL    :: TMAXM               ! Temperature maximum,monthly av C
        REAL    :: TMAXPAV(0:12)       ! Temperature,maximum,tier av    C
        REAL    :: TMAXPC              ! Temperature,maximum,tier  sum  C
        REAL    :: TMAXSUM             ! Temperature maximum,summed     C
        REAL    :: TMAXX               ! Temperature max during season  C
        REAL    :: TMEAN               ! Temperature mean (TMAX+TMIN/2) C
        REAL    :: TMEAN20             ! Temperature mean over 20 days  C
        REAL    :: TMEAN20P            ! Temperature mean,20 d>planting C
        REAL    :: TMEAN20S            ! Temperature sum over 20 days   C
        REAL    :: TMEANAV(0:12)       ! Temperature,mean,tier av       C
        REAL    :: TMEANCC             ! Temperature,mean,cycle sum     C.d  
        REAL    :: TMEAND(20)          ! Temperature mean,specific day  C
        REAL    :: TMEANE              ! Temp mean,germination-emerge   C
        REAL    :: TMEANEC             ! Temp sum,germination-emergence C
        REAL    :: TMEANG              ! Temp mean,planting-germination C
        REAL    :: TMEANGC             ! Temp sum,planting-germination  C
        REAL    :: TMEANNUM            ! Temperature means in sum       #
        REAL    :: TMEANPC             ! Temperature,mean,tier sum      C
        REAL    :: TMEANSUM            ! Temperature means sum          #
        REAL    :: TMEANSURF           ! Temperature mean,soil surface  C
        REAL    :: TMIN                ! Temperature minimum            C           ARGUMENT
        REAL    :: TMINCAV             ! Temperature,minimum,cycle av   C
        REAL    :: TMINCC              ! Temperature,min,cycle sum      C.d  
        REAL    :: TMINM               ! Temperature minimum,monthly av C
        REAL    :: TMINN               ! Temperature min during season  C
        REAL    :: TMINPAV(0:12)       ! Temperature,minimum,tier av    C
        REAL    :: TMINPC              ! Temperature,minimum,tier sum   C
        REAL    :: TMINSUM             ! Temperature minimum,summed     C
        !INTEGER :: TN                  ! Treatment number               #           ARGUMENT
        REAL    :: TNAD                ! Total nitrogen (tops+roots)    kg/ha
        REAL    :: TNAMM               ! Total N at harvest,measured    kg/ha
        INTEGER :: TNI                 ! Treatment number,initial value #
        !REAL    :: TNIMBSOM            ! Total N immobilised by SOM     kg/ha       ARGUMENT
        REAL    :: TNOXC               ! Cumulative N denitrified       kg/ha
        !REAL    :: TNOXD               ! N denitrified this day         kg/ha       ARGUMENT
        REAL    :: TOFIXC              ! Cumulative inorganicN fixation kg/ha
        REAL    :: TOMIN               ! Daily N mineralized            kg/ha
        REAL    :: TOMINC              ! Cumulative N mineralized       kg/ha
        !REAL    :: TOMINFOM            ! Daily mineralization,FOM       kg/ha       ARGUMENT
        REAL    :: TOMINFOMC           ! Cumulative mineralization,FOM  kg/ha
        !REAL    :: TOMINSOM            ! Daily mineralization,SOM       kg/ha       ARGUMENT
        !REAL    :: TOMINSOM1           ! Daily mineralization,SOM1      kg/ha       ARGUMENT
        REAL    :: TOMINSOM1C          ! Cumulative mineralization,SOM1 kg/ha
        !REAL    :: TOMINSOM2           ! Daily mineralization,SOM2      kg/ha       ARGUMENT
        REAL    :: TOMINSOM2C          ! Cumulative mineralization,SOM2 kg/ha
        !REAL    :: TOMINSOM3           ! Daily mineralization,SOM3      kg/ha       ARGUMENT
        REAL    :: TOMINSOM3C          ! Cumulative mineralization,SOM3 kg/ha
        REAL    :: TOMINSOMC           ! Cumulative mineralization,SOM  kg/ha
        REAL    :: TPAR                ! Transmission,PAR,fraction      #
        REAL    :: TRATIO              ! Function,relative tr rate      #
        REAL    :: TRDV1(4)            ! Temp response,development 1    #
        REAL    :: TRDV2(4)            ! Temp response,development 2    #
        REAL    :: TRGEM(4)            ! Temp response,germ.emergence   #
        REAL    :: TRLDF               ! Intermediate factor,new roots  #
        REAL    :: TRLFG(4)            ! Temp response,leaf growth      #
        REAL    :: TRLV                ! Total root length density      /cm2
        REAL    :: TRPHS(4)            ! Temp response,photosynthesis   #
        REAL    :: TRWU                ! Total water uptake             mm
        !REAL    :: TRWUP               ! Total water uptake,potential   cm          ARGUMENT
        REAL    :: TSDEP               ! Average temp in top 10 cm soil C
        REAL    :: TSRAD               ! Transmission,SRAD,fraction     #
        REAL    :: TT                  ! Daily thermal time             C.d
        REAL    :: TT20                ! Thermal time mean over 20 days C
        REAL    :: TT20S               ! Thermal time sum over 20 days  C
        REAL    :: TTCUM               ! Cumulative thermal time        C.d
        REAL    :: TTD(20)             ! Thermal time,specific day      C
        REAL    :: TTGEM               ! Daily thermal time,germ,emrg.  C.d
        REAL    :: TTLFLIFE            ! Daily thermal time,leaf life   C.d
        REAL    :: TTNEED              ! Thermal time to start new leaf #
        REAL    :: TTNEXT              ! Thermal time,next tier         oCd
        REAL    :: TTOUT               ! Thermal units output from func C.d
        INTEGER :: TVI1                ! Temporary integer variable     #
        INTEGER :: TVI2                ! Temporary integer variable     #
        INTEGER :: TVI3                ! Temporary integer variable     #
        INTEGER :: TVI4                ! Temporary integer variable     #
        !INTEGER :: TVICOLNM            ! Column number function output  #           FUNCTION
        !INTEGER :: TVILENT             ! Temporary integer,function op  #           FUNCTION
        REAL    :: TVR1                ! Temporary real variable        #
        REAL    :: TVR2                ! Temporary real variable        #
        REAL    :: TVR3                ! Temporary real variable        #
        REAL    :: TVR4                ! Temporary real variable        #
        REAL    :: TVR5                ! Temporary real variable        #
        REAL    :: TVR6                ! Temporary real variable        #
        REAL    :: TWAD                ! Total weight (tops+roots)      kg/ha
        !REAL    :: UH2O(NL)            ! Uptake of water                cm/d        ARGUMENT
        !REAL    :: UNH4(20)            ! Uptake of NH4 N                kg/ha       ARGUMENT
        !REAL    :: UNO3(20)            ! Uptake of NO3 N                kg/ha       ARGUMENT
        INTEGER :: VALUEI              ! Output from Getstri function   #
        REAL    :: VALUER              ! Output from Getstrr function   #
        REAL    :: VANC                ! Vegetative actual N conc       #
        INTEGER :: VARNUM(30)          ! Variable number in sum         #
        REAL    :: VARSUM(30)          ! Temporary variables sum        #
        REAL    :: VARVAL              ! Temporary variable             #
        REAL    :: VCNC                ! Vegetative critical N conc     #
        !INTEGER :: VERSION             ! Version #                      #           MF 15SE14 Defined in ModuleDefs
        INTEGER :: VERSIONCSCAS        ! Vwrsion # (CSCAS)              #            MF 15SE14 Mad unique to CSCAS
        REAL    :: VMNC                ! Vegetative minimum N conc      #
        REAL    :: VNAD                ! Vegetative canopy nitrogen     kg/ha
        REAL    :: VNAM                ! Vegetative N,mature            kg/ha
        REAL    :: VNAMM               ! Vegetative N,mature,measured   kg/ha
        REAL    :: VNPCM               ! Vegetative N %,maturity        %
        REAL    :: VNPCMM              ! Vegetative N,mature,measure    %
        REAL    :: VPD                 ! Vapour pressure deficit        KPa
        REAL    :: VPDHR(TS)           ! Vapour pressure def, hourly    kPa          MF 15SE14 For hourly PS and VPD
        REAL    :: VPDFP               ! Vapour press deficit factor,phs#
        REAL    :: VPDFPHR(TS)         ! VPD factor for phs, hourly     #            MF 15SE14 For hourly PS and VPD
        REAL    :: VPDFPMEAN           ! VPD factor, mean for light hrs #            MF 23SE14 To adjust daily ET
        REAL    :: VPDFPPREV           ! VPD factor for previous hr, phs#            MF 15SE14 For hourly PS and VPD
        REAL    :: VPDFPSUM            ! VPD factor sum for light hrs   #            MF 23SE14 To adjust daily ET
        REAL    :: VWAD                ! Vegetative canopy weight       kg/ha
        REAL    :: VWAM                ! Vegetative canopy wt,maturity  kg/ha
        REAL    :: VWAMERR             ! Vegetative wt,error            %
        REAL    :: VWAMM               ! Veg wt,mature,measured         kg/ha
        REAL    :: WAVR                ! Water available/demand         #
        REAL    :: WFEU                ! Water factor,evaptp,upper      #
        REAL    :: WFG                 ! Water factor,growth 0-1        #
        REAL    :: WFGCAV              ! Water factor,growth,av.,cylcle #
        REAL    :: WFGCC               ! H20 factor,growh,cycle sum     #
        REAL    :: WFGE                ! Water factor,germ,emergence    #
        REAL    :: WFGEM               ! Water factor,germ,emergence    #
        REAL    :: WFGL                ! Water factor,growth,lower      #
        REAL    :: WFGPAV(0:12)        ! Water factor,growth,average    #
        REAL    :: WFGPC               ! Water factor,growth,cumulative #
        REAL    :: WFGU                ! Water factor,growth,upper      #
        REAL    :: WFLAW               ! Water factor,leaf area/weight  #
        REAL    :: WFLF(LNUMX)         ! H2O factor for leaf,average    #
        REAL    :: WFNU                ! Water factor,N uptake          #
        REAL    :: WFP                 ! Water factor,photosynthsis 0-1 #
        REAL    :: WFPCAV              ! Water factor,phs,av 0-1,cycle  #
        REAL    :: WFPCC               ! H20 factor,phs,cycle sum       # 
        REAL    :: WFPL                ! Water factor,phs,lower         #
        REAL    :: WFPPAV(0:12)        ! Water factor,phs,average 0-1   #
        REAL    :: WFPPC               ! Water factor,phs,cumulative    #
        REAL    :: WFPU                ! Water factor,phs,upper         #
        REAL    :: WFRG                ! Water factor,root growth,0-1   #
        REAL    :: WFRTG               ! Water factor,root gr           #
        REAL    :: WFSU                ! Water fac,senescence,upper 0-1 #
        !REAL    :: WINDSP              ! Wind speed                     m/s
        INTEGER :: WSDAYS              ! Water stress days              #
        REAL    :: WTDEP               ! Water table depth              cm
        REAL    :: WUPR                ! Water pot.uptake/demand        #
        REAL    :: WUPRD(20)           ! Water pot.uptake/demand,ind dy #
        REAL    :: XDEP                ! Depth to bottom of layer       cm
        REAL    :: XDEPL               ! Depth to top of layer          cm
        REAL    :: XMIN                ! Minimum NO3,NH4-N in soil layr kg/ha
        !INTEGER :: YEAR                ! Year                           #           ARGUMENT
        INTEGER :: YEARCOL             ! Colum number for year data     #
        INTEGER :: YEARDOY             ! Year+Doy (7digits)             #
        INTEGER :: YEARDOYHARF         ! Harvest year+doy,fixed         #
        INTEGER :: YEARDOYPREV         ! Year+Doy (7digits),previous    #
        INTEGER :: YEARM               ! Year of measurement            #
        !INTEGER :: YEARPLTCSM          ! Planting year*1000+DOY,CSM     #           ARGUMENT
        INTEGER :: YEARSIM             ! Year+Doy for simulation start  #
        !REAL    :: YVALXY              ! Y value from function          #           FUNCTION

    
    
        LOGICAL :: FEXIST              ! File existence indicator
        LOGICAL :: FEXISTA             ! File A existence indicator
        LOGICAL :: FEXISTT             ! File T existence indicator
        LOGICAL :: FFLAG               ! Temp file existance indicator
        LOGICAL :: FFLAGEC             ! Temp file existance indicator
        LOGICAL :: FOPEN               ! File open indicator
    
        CHARACTER (LEN=128):: ARG           ! Argument component
        CHARACTER (LEN=6)  :: BRSTAGEC      ! Branching stage
        CHARACTER (LEN=6)  :: CAIC          ! Canopy area index
        CHARACTER (LEN=6)  :: CANHTC        ! Canopy height
        CHARACTER (LEN=120):: CFGDFILE      ! Configuration directory+file
        CHARACTER (LEN=1)  :: CFLAFLF       ! Control flag,assim.on lf area
        CHARACTER (LEN=1)  :: CFLFAIL       ! Control flag for failure
        CHARACTER (LEN=1)  :: CFLHAR        ! Control flag for final harvest
        CHARACTER (LEN=1)  :: CFLHARMSG     ! Control flag,harvest message
        CHARACTER (LEN=1)  :: CFLLFLIFE     ! Control flag,leaf life D,P,C
        CHARACTER (LEN=1)  :: CFLPDADJ      ! Control flag,tire adjustment
        CHARACTER (LEN=1)  :: CFLPRES       ! Control flag for headers,PRES
        CHARACTER (LEN=1)  :: CFLSDRSMSG    ! Control flag,seed reserves msg
        CHARACTER (LEN=1)  :: CFLTFG        ! Control flag,temp.on lf area
        CHARACTER (LEN=10) :: CNCHAR        ! Crop component (multicrop)
        CHARACTER (LEN=2)  :: CNCHAR2       ! Crop component (multicrop)
        CHARACTER (LEN=2)  :: CROP          ! Crop identifier (ie. CS)
        CHARACTER (LEN=2)  :: CROPPREV      ! Crop identifier,previous run
        CHARACTER (LEN=93) :: CUDIRFLE      ! Cultivar directory+file
        CHARACTER (LEN=93) :: CUDIRFLPREV   ! Cultivar directory+file,prev
        CHARACTER (LEN=12) :: CUFILE        ! Cultivar file
        CHARACTER (LEN=10) :: DAPCHAR       ! DAP in character form
        CHARACTER (LEN=6)  :: DAPWRITE      ! DAP character string -> output
        CHARACTER (LEN=93) :: ECDIRFLE      ! Ecotype directory+file
        CHARACTER (LEN=93) :: ECDIRFLPREV   ! Ecotype directory+file,prev
        CHARACTER (LEN=12) :: ECFILE        ! Ecotype filename
        CHARACTER (LEN=6)  :: ECONO         ! Ecotype code
        CHARACTER (LEN=6)  :: ECONOPREV     ! Ecotype code,previous
        CHARACTER (LEN=1)  :: EMFLAG        ! Flag,emergence Y/N
        CHARACTER (LEN=60) :: ENAME         ! Experiment description
        CHARACTER (LEN=1)  :: ESTABLISHED   ! Flag,crop establishment Y/N
        CHARACTER (LEN=14) :: EVHEADER      ! Evaluater.out header
        CHARACTER (LEN=10) :: EXCODE        ! Experiment code/name
        CHARACTER (LEN=10) :: EXCODEPREV    ! Previous experiment code/name
        CHARACTER (LEN=80) :: FAPPLINE(30)  ! Fertilizer application details
        CHARACTER (LEN=120):: FILEA         ! Name of A-file
        CHARACTER (LEN=107):: FILEADIR      ! Name of A-file directory
        CHARACTER (LEN=120):: FILEIO        ! Name of input file,after check
        !CHARACTER (LEN=120):: FILEIOIN      ! Name of input file                    ARGUMENT
        CHARACTER (LEN=3)  :: FILEIOT       ! Type of input file
        CHARACTER (LEN=120):: FILENEW       ! Temporary name of file
        CHARACTER (LEN=120):: FILET         ! Name of T-file
        CHARACTER (LEN=120):: FILEX         ! Name of X-file
        CHARACTER (LEN=1)  :: FNAME         ! File name switch (N->standard)
        CHARACTER (LEN=120):: FNAMEERA      ! File name,A-errors
        CHARACTER (LEN=120):: FNAMEERT      ! File name,T-errors
        CHARACTER (LEN=120):: FNAMEEVAL     ! File name,evaluate outputs
        CHARACTER (LEN=120):: FNAMELEAVES   ! File name,leaves outputs
        CHARACTER (LEN=120):: FNAMEMEAS     ! File name,measured outputs
        CHARACTER (LEN=120):: FNAMEOV       ! File name,overview outputs
        CHARACTER (LEN=120):: FNAMEPHASES   ! File name,phases outputs
        CHARACTER (LEN=120):: FNAMEPHENOLM  ! File name,phenology measured
        CHARACTER (LEN=120):: FNAMEPHENOLS  ! File name,phenology outputs
        CHARACTER (LEN=120):: FNAMEPREM     ! File name,responses,measured
        CHARACTER (LEN=120):: FNAMEPRES     ! File name,responses,simulated
        CHARACTER (LEN=120):: FNAMEPSUM     ! File name,plant summary
        CHARACTER (LEN=35) :: GENFLCHK      ! Genotype file name for check
        CHARACTER (LEN=1)  :: GROUP         ! Flag for type of group
        CHARACTER (LEN=6)  :: HIAMCHAR      ! Harvest indx,at harvest
        CHARACTER (LEN=6)  :: HIAMMCHAR     ! Harvest indx,har,measured
        CHARACTER (LEN=6)  :: HINDC         ! Harvest index,nitrogen
        CHARACTER (LEN=6)  :: HINMCHAR      ! Harvest N index,at harvest
        CHARACTER (LEN=6)  :: HINMMCHAR     ! Harvest N index,mat,measured
        CHARACTER (LEN=6)  :: HNPCMCHAR     ! Harvest product N %,maturity
        CHARACTER (LEN=6)  :: HNPCMMCHAR    ! Harvest product N%,mature,meas
        CHARACTER (LEN=1)  :: HOP(HANUMX)   ! Harvest operation code
        CHARACTER (LEN=2)  :: HPROD         ! Code,harvested part of plant
        CHARACTER (LEN=6)  :: HWUDC         ! Harvest wt/unit
        CHARACTER (LEN=6)  :: HWUMCHAR      ! Harvest wt/unit
        CHARACTER (LEN=6)  :: HWUMMCHAR     ! Harvest wt/unit,mat,measured
        CHARACTER (LEN=1)  :: IDETD         ! Control flag,screen outputs
        !CHARACTER (LEN=1)  :: IDETG         ! Control flag,growth outputs           ARGUMENT
        !CHARACTER (LEN=1)  :: IDETL         ! Control switch,detailed output        ARGUMENT
        !CHARACTER (LEN=1)  :: IDETO         ! Control flag,overview outputs         ARGUMENT
        !CHARACTER (LEN=1)  :: IDETS         ! Control switch,summary outputs        ARGUMENT
        CHARACTER (LEN=1)  :: IFERI         ! Fertilizer switch (A,F,R,D,N)
        CHARACTER (LEN=1)  :: IHARI         ! Control flag,harvest
        CHARACTER (LEN=1)  :: IPLTI         ! Code for planting date method
        !CHARACTER (LEN=1)  :: ISWDIS        ! Control switch,disease                ARGUMENT
        !CHARACTER (LEN=1)  :: ISWNIT        ! Soil nitrogen balance switch          ARGUMENT
        CHARACTER (LEN=1)  :: ISWNITEARLY   ! Control flag,N stress early
        !CHARACTER (LEN=1)  :: ISWWAT        ! Soil water balance switch Y/N         ARGUMENT
        CHARACTER (LEN=1)  :: ISWWATCROP    ! Control flag,H20 stress,crop
        CHARACTER (LEN=1)  :: ISWWATEARLY   ! Control flag,H20 stress early
        CHARACTER (LEN=6)  :: LAIC          ! Leaf area index
        CHARACTER (LEN=6)  :: LAIPRODC      ! Leaf area index produced
        CHARACTER (LEN=6)  :: LAIXCHAR      ! Leaf area index,maximum
        CHARACTER (LEN=6)  :: LAIXMCHAR     ! Leaf area index,max,measured
        CHARACTER (LEN=6)  :: LAPC          ! Area of cohort of leaves
        CHARACTER (LEN=6)  :: LAPOTXC       ! Leaf area,potential
        CHARACTER (LEN=6)  :: LAPSC         ! Senesced area,cohort of leaves
        CHARACTER (LEN=6)  :: LATL2C        ! Leaf area,h2o,N,temp stress
        CHARACTER (LEN=6)  :: LATL3C        ! Leaf area,stress+assim control
        CHARACTER (LEN=6)  :: LATL4C        ! Leaf area,stress+assim&Ncntrol
        CHARACTER (LEN=6)  :: LATLC         ! Leaf area,no stress
        CHARACTER (LEN=354):: LINEERA       ! Temporary line,error-a file
        CHARACTER (LEN=80) :: LINESTAR      ! Group header line (with star)
        CHARACTER (LEN=80) :: LINESTAR2     ! Group header line (with star)
        CHARACTER (LEN=180):: LINET         ! Line from T-file
        CHARACTER (LEN=4)  :: MEDEV         ! Switch,development control
        CHARACTER (LEN=1)  :: MEEVP         ! Switch,potential evapot method
        CHARACTER (LEN=1)  :: MEEXP         ! Switch,experimental method = E
        CHARACTER (LEN=1)  :: MENU          ! Switch,root N uptake methd
        CHARACTER (LEN=1)  :: MEPHO         ! Switch,photosynthesis method
        CHARACTER (LEN=3)  :: MERNU         ! Switch,root N uptake methd
        !CHARACTER (LEN=1)  :: MESOM         ! Switch,OM decay method                ARGUMENT
        CHARACTER (LEN=78) :: MESSAGE(10)   ! Messages for Warning.out
        CHARACTER (LEN=1)  :: MEWNU         ! Switch,root H2O/N uptake methd
        CHARACTER (LEN=8)  :: MODEL         ! Name of model
        CHARACTER (LEN=8)  :: MODNAME       ! Name of module
        CHARACTER (LEN=3)  :: MONTH         ! Month
        CHARACTER (LEN=3)  :: OUT           ! Output file extension
        CHARACTER (LEN=79) :: OUTHED        ! Output file heading
        CHARACTER (LEN=12) :: OUTPG         ! Growth output file code
        CHARACTER (LEN=12) :: OUTPG2        ! Growth output file2 code
        CHARACTER (LEN=12) :: OUTPGF        ! Growth factors file2 code
        CHARACTER (LEN=12) :: OUTPN         ! GrowthN output file code
        CHARACTER (LEN=80) :: PATHCR        ! Path to genotype (CUL) files
        CHARACTER (LEN=80) :: PATHEC        ! Path to genotype (ECO) files
        CHARACTER (LEN=80) :: PATHSP        ! Path to genotype (SPE) files
        CHARACTER (LEN=1)  :: PLME          ! Planting method (code)
        CHARACTER (LEN=2)  :: PPSEN         ! Code,photoperiod sensitivity
        CHARACTER (LEN=5)  :: PSABV(PSX)    ! Principal stage abbreviation
        CHARACTER (LEN=5)  :: PSABVO(PSX)   ! Principal stage abv,output
        CHARACTER (LEN=13) :: PSNAME(PSX)   ! Principal stage names
        CHARACTER (LEN=1)  :: PSTYP(PSX)    ! Principal stage type
        !CHARACTER (LEN=1)  :: RNMODE        ! Run mode (eg.I=interactive)           ARGUMENT
        CHARACTER (LEN=25) :: RUNNAME       ! Run title
        CHARACTER (LEN=8)  :: RUNRUNI       ! Run+internal run number
        CHARACTER (LEN=6)  :: SDWADC        ! Seed weight
        CHARACTER (LEN=1)  :: SEASENDOUT    ! Season end outputs flag
        CHARACTER (LEN=6)  :: SENN0C        ! Senesced N added to litter
        CHARACTER (LEN=6)  :: SENNSC        ! Senesced N added to soil
        CHARACTER (LEN=6)  :: SENROOTC      ! Senesced OM,soil
        CHARACTER (LEN=6)  :: SENTOPLITTERAC! Senesced OM added to surface
        CHARACTER (LEN=64) :: SPDIRFLE      ! Species directory+file
        CHARACTER (LEN=64) :: SPDIRFLPREV   ! Species directory+file,last
        CHARACTER (LEN=12) :: SPFILE        ! Species filename
        CHARACTER (LEN=5)  :: SSABV(SSX)    ! Secondary stage abbreviation
        CHARACTER (LEN=5)  :: SSABVO(SSX)   ! Secondary stage abv,output
        CHARACTER (LEN=13) :: SSNAME(SSX)   ! Secondary stage names
        CHARACTER (LEN=1)  :: SSTYP(SSX)    ! Secoudary stage type
        CHARACTER (LEN=6)  :: TCHAR         ! Temporary character string
        CHARACTER (LEN=6)  :: THEAD(20)     ! T-file headings
        CHARACTER (LEN=1)  :: TIERNUMC      ! Tier number in t-file
        CHARACTER (LEN=10) :: TL10          ! Temporary line
        !CHARACTER (LEN=10) :: TL10FROMI     ! Temporary line from integer           FUNCTION
        CHARACTER (LEN=254):: TLINEGRO      ! Temporary line from GRO file
        CHARACTER (LEN=180):: TLINET        ! Temporary line from T-file
        CHARACTER (LEN=180):: TLINETMP      ! Temporary line
        CHARACTER (LEN=25) :: TNAME         ! Treatment name
        CHARACTER (LEN=10) :: TNCHAR        ! Treatment number,characters
        CHARACTER (LEN=40) :: TRUNNAME      ! Treatment+run composite name
        CHARACTER (LEN=5)  :: TVTRDV        ! Temporary temp response char
        CHARACTER (LEN=8)  :: VARCHAR       ! Temporary variable,character
        CHARACTER (LEN=6)  :: VARNO         ! Variety identification code
        CHARACTER (LEN=6)  :: VARNOPREV     ! Variety identification code
        CHARACTER (LEN=6)  :: VNPCMCHAR     ! Vegetative N %,maturity
        CHARACTER (LEN=6)  :: VNPCMMCHAR    ! Vegetative N,mature,measured
        CHARACTER (LEN=16) :: VRNAME        ! Variety name
    
    
    
        ! Arrays for passing variables to OPSUM subroutine, CSM model only
        INTEGER,     PARAMETER         :: SUMNUM = 37
        CHARACTER*5, DIMENSION(SUMNUM) :: LABEL
        REAL,        DIMENSION(SUMNUM) :: VALUE
    
        ! RNMODE is a switch for run mode conveyed on the command line
        ! Options: I=Interactive, A=All treatments, B=Batch,
        ! E=Sensitivity, D=Debug, N=Seasonal, Q=Sequence, G=Gencalc
    
        ! For when simulating more than one species
        !REAL          sradip        ! Srad interception,whole can    %
        !REAL          sradipcn(5)   ! Srad interception,component    %
    
        ! For AFRC photosynthesis
        ! REAL PM,RA,RM,VPD,QPAR,ALPHA,RS,RP,PMAX,PHSA,PHSB,PGROSS

    END MODULE Module_CSCAS_Vars_List