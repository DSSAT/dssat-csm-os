!-----------------------------------------------------------------------
!  CROPSIM CEREAL GROWTH AND DEVELOPMENT MODULE V4.5   
!  Last edit 27/06/10 LAH
!-----------------------------------------------------------------------

      SUBROUTINE CSCROP (FILEIOIN, RUN, TN, RN, RNMODE,    !Command line
     & ISWWAT, ISWNIT, ISWDIS, MESOM,                      !Contols
     & IDETS, IDETO, IDETG, IDETL, FROP,                   !Controls
     & SN, ON, RUNI, REP, YEAR, DOY, STEP, CN,             !Run+loop
     & SRAD, TMAX, TMIN, TAIRHR, RAIN, CO2, TDEW,          !Weather
     & DRAIN, RUNOFF, IRRAMT,                              !Water
     & DAYL, WINDSP, DEWDUR, CLOUDS, ST, EO, ES,           !Weather
     & NLAYR, DLAYR, DEPMAX, LL, DUL, SAT, BD, SHF, SLPF,  !Soil states
     & SNOW, SW, NO3LEFT, NH4LEFT, FERNIT,                 !H2o,N states
     & TLCHD, TNIMBSOM, TNOXD,                     !NEW    !N components
     & TOMINFOM, TOMINSOM, TOMINSOM1, TOMINSOM2, TOMINSOM3,!N components
     & YRPLT, HARVFRAC,                                    !Pl.date
     & PARIP, PARIPA, EOP, EP, ET, TRWUP, ALBEDOS,         !Resources
     & CAID, KCAN, KEP,                                    !States
     & RLV, NFP, RWUPM, RWUMX, CANHT, LAIL, LAILA,         !States
     & UNO3, UNH4, UH2O,                                   !Uptake
     & SENCALG, SENNALG, SENLALG,                          !Senescence
     & RESCALG, RESNALG, RESLGALG,                         !Residues
     & STGYEARDOY, GSTAGE,                                 !Stage dates
     & DYNAMIC)                                            !Control

      ! For incorporation in CSM should:
      !  Ensure that Alt-plant routine:
      !   Sending control ISWDIS
      !   Sending DEWDUR(=-99.0?), CLOUDS(=0.0?), ES, ALBEDO(=0.2)
      !   Setting dummies for PARIP, PARIPA, LAIL, LAILA
      !  Eliminate '!' from SUMVALS call.
      !  Need to do something with metadata name if INH file

      ! And to run well in CSM should:
      !    Read ICIN (with -99 in profile) and ICSW variables

      ! Temporary changes .. need firming-up:
      !    Height increase on emergence day has initialisation value

      ! Thoughts about possible changes:
      !    1. Phyllochron senescence delayed if low light interception
      !    2. Leaf and stem N top-up reduced in stages 4 and 5 (NTUPF)

      ! Questions
      !    1. What x-file information is essential to run

      ! Checks needed 
      !    1. Algorthms for fraction of day at changeovers
      !    2. Blips in response curves and reasons for these
      !    3  All declarations,initialisations to 0 or -99

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


      USE CRSIMDEF
      
      IMPLICIT NONE

      INTEGER,PARAMETER::MESSAGENOX=10 ! Messages to Warning.out

      INTEGER,PARAMETER::DINX =   3 ! Disease number,maximum
      INTEGER,PARAMETER::PSX  =  20 ! Principal stages,maximum
      INTEGER,PARAMETER::SSX  =  20 ! Secondary stages,maximum
      INTEGER,PARAMETER::PHSX =   5 ! Phyllochron stages,maximum
      INTEGER,PARAMETER::KEYSTX = 9 ! Maximum number of key stages
      INTEGER,PARAMETER::DCNX =  10 ! Disease control #,maximum
      INTEGER,PARAMETER::LCNUMX=500 ! Maximum number of leaf cohorts
      INTEGER,PARAMETER::LNUMX= 500 ! Maximum number of leaves/axis
      INTEGER,PARAMETER::HANUMX= 40 ! Maximum # harvest instructions
      INTEGER,PARAMETER::NL   =  20 ! Maximum number of soil layers

      INTEGER,PARAMETER::RUNINIT=1  ! Program initiation indicator
      INTEGER,PARAMETER::SEASINIT=2 ! Reinitialisation indicator
      INTEGER,PARAMETER::RATE = 3   ! Program rate calc.indicator
      INTEGER,PARAMETER::INTEGR=4   ! Program update indicator
      INTEGER,PARAMETER::OUTPUT=5   ! Program output indicator
      INTEGER,PARAMETER::SEASEND= 6 ! Program ending indicator

      CHARACTER(LEN=1),PARAMETER::BLANK = ' '
      CHARACTER(LEN=3),PARAMETER::DASH = ' - '

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
      REAL          ALBEDOS       ! soil albedo                    fr
      REAL          AMTNIT        ! Cumulative amount of N applied kg/ha
      REAL          AMTNITPREV    ! Cumulative N,previous treatmnt kg/ha
      REAL          ANDEM         ! Crop N demand                  kg/ha
      REAL          ANFER(200)    ! N amount in fertilizer appln   kg/ha
      INTEGER       ARGLEN        ! Argument component length      #
      INTEGER       ASTG          ! Anthesis stage                 #
      REAL          AVGSW         ! Average soil water in SWPLTD   %
      REAL          AWNAI         ! Awn area index                 m2/m2
      REAL          AWNS          ! Awn score,1-10                 #
      REAL          BASELAYER     ! Depth at base of layer         cm
      REAL          BD(20)        ! Bulk density (moist)           g/cm3
      REAL          CAID          ! Canopy area index              #
      REAL          CANHT         ! Canopy height                  cm
      REAL          CANHTG        ! Canopy height growth           cm
      REAL          CANHTS        ! Canopy height standard         cm
      REAL          CARBO         ! Ch2o available,phs+reserves    g/p
      REAL          CARBOADJ      ! Ch2o adjustment for LAI change g/p
      REAL          CARBOBEG      ! Ch2o available,beginning day   g/p
      REAL          CARBOBEGI     ! Ch2o avail,internal co2 calc   g/p
      REAL          CARBOBEGIA    ! Ch2o avail,internal co2,adj    #
      REAL          CARBOBEGP     ! Ch2o avail,PARUE calculation   g/p
      REAL          CARBOBEGR     ! Ch2o avail,resistances calc    g/p
      REAL          CARBOC        ! Ch2o assimilated,cumulative    g/p
      REAL          CARBOEND      ! Ch2o available,end of day      g/p
      REAL          CARBOENDI     ! Ch2o avail,internal co2 calc   g/p
      REAL          CARBOENDP     ! Ch2o avail,PARUE calculation   g/p
      REAL          CARBOENDR     ! Ch2o avail,resistances calc    g/p
      REAL          CARBOGF       ! Ch2o grain fill                g/p
      INTEGER       CARBOLIM      ! Ch2o limited grain growth Days d
      REAL          CARBOLSD      ! Ch2o used for leaves from seed g/p
      REAL          CARBOPM       ! Ch2o available,>mature         g/p
      REAL          CARBOR        ! Ch2o available,roots           g/p
      REAL          CARBORRS      ! Ch2o to roots from reserves    g/p
      REAL          CARBOT        ! Ch2o available,tops            g/p
      REAL          CARBOTMP      ! Ch2o available,temporary value g/p
      REAL          CARBOTMPI     ! Ch2o avail,internal co2 calc   g/p
      REAL          CARBOTMPP     ! Ch2o avail,PARUE calculation   g/p
      REAL          CARBOTMPR     ! Ch2o avail,resistances calc    g/p
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
      REAL          CLOUDS        ! Cloudiness factor,relative,0-1 #
      INTEGER       CN            ! Crop component (multicrop)     #
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
      REAL          CO2PC         ! CO2 concentration,phase cumul  ppm
      REAL          CO2RF(10)     ! CO2 reference concentration    vpm
      INTEGER       COLNUM        ! Column number                  #
      INTEGER       CSIDLAYR      ! Layer # output from function   #
      INTEGER       CSTIMDIF      ! Time difference function       #
      REAL          CSVPSAT       ! Vapour pressure function op    mb
      INTEGER       CSYDOY        ! Yr+Doy output from function    #
      INTEGER       CSYEARDOY     ! Year+Doy from function         #
      REAL          CSYVAL        ! Y value from function          #
      INTEGER       CTRNUMPD      ! Control # missing phases       #
      REAL          CUMDEP        ! Cumulative depth               cm
      REAL          CUMDU         ! Cumulative development units   #
      REAL          CUMDULAG      ! Cumulative DU lag phase        #
      REAL          CUMDULF       ! Cumulative DU during leaf ph   #
      REAL          CUMDULIN      ! Cumulative DU linear phase     #
      REAL          CUMDUS        ! Cumulative DU during stem ph   #
      REAL          CUMSW         ! Soil water in depth SWPLTD     cm
      REAL          CUMTT         ! Cumulative thermal times       #
      REAL          CUMVD         ! Cumulative vernalization days  d
      REAL          CWAA          ! Canopy weight at anthesis      kg/ha
      REAL          CWAAM         ! Canopy wt,anthesis,measured    kg/ha
      REAL          CWAD          ! Canopy weight                  kg/ha
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
      INTEGER       DAPCALC       ! DAP output from funcion        #
      INTEGER       DAS           ! Days after start of simulation d
      INTEGER       DATE          ! Date (Year+Doy)                #
      INTEGER       DATECOL       ! Date column number             #
      REAL          DAYL          ! Daylength,6deg below horizon   h
      REAL          DAYLCAV       ! Daylength (6deg) av for cycle  h
      REAL          DAYLCC         ! Daylength,cycle sum           h.d 
      REAL          DAYLPAV(0:12) ! Daylength (6deg) av for phase  h
      REAL          DAYLPC        ! Daylength (6deg),cumulative    h
      REAL          DAYLPREV      ! Daylength previous day         h
      REAL          DAYLS(0:10)   ! Daylength sensitivity,phase    %/10h
      REAL          DAYLST(0:12)  ! Daylength (6deg) at stage      h
      REAL          DAYSUM        ! Days accumulated in month      #
      INTEGER       DCDAT(DCNX)   ! Disease control application YrDoy
      REAL          DCDUR(DCNX)   ! Disease control duration       d
      REAL          DCFAC(DCNX)   ! Disease control gr factor 0-1  #
      INTEGER       DCTAR(DCNX)   ! Disease control target         #
      REAL          DEADN         ! Dead leaf N retained on plant  g/p
      REAL          DEADNAD       ! Dead N retained on plant       kg/ha
      REAL          DEADWAD       ! Dead weight retained on plant  kg/ha
      REAL          DEADWAM       ! Dead weight retained,maturity  kg/ha
      REAL          DEADWAMM      ! Dead weight retained,measured  kg/ha
      REAL          DEADWT        ! Dead leaf wt.retained or shed  g/p
      REAL          DEADWTM       ! Dead leaf wt.on plant,maturity g/p
      REAL          DEADWTR       ! Dead leaf wt.retained on plant g/p
      REAL          DEADWTS       ! Dead leaf wt.shed from plant   g/p
      REAL          DEADWTSGE     ! Dead weight rtained,stem g end g/p
      REAL          DEPMAX        ! Maximum depth of soil profile  cm
      REAL          DEWDUR        ! Dew duration                   h
      REAL          DF            ! Daylength factor 0-1           #
      REAL          DFNEXT        ! Daylength factor,next phase    #
      REAL          DFOUT         ! Daylength factor for output    #
      REAL          DFPE          ! Development factor,pre-emerge  #
      REAL          DGLF(LNUMX)   ! Days during which leaf growing #
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
      INTEGER       DRDAT         ! Double ridges date             #
      INTEGER       DRDATM        ! Double ridges date,measured    #
      REAL          DRF1          ! Double ridges factor 1         #
      REAL          DRF2          ! Double ridges factor 2         #
      REAL          DRF3          ! Double ridges factor 3         #
      REAL          DRSTAGE       ! Double ridges stage            #
      REAL          DSTAGE        ! Develoment stage,linear        #
      REAL          DTRY          ! Effective depth of soil layer  cm
      REAL          DU            ! Developmental units            PVC.d
      REAL          DUL(20)       ! Drained upper limit for soil   #
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
      INTEGER       DYNAMIC       ! Program control variable       #
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
      REAL          EO            ! Potential evaporation          mm/d
      REAL          EOC           ! Potential evap,cumulative      mm 
      REAL          EOEBUD        ! Potential evap,Ebudget         mm/d
      REAL          EOEBUDC       ! Potential evap,Ebudget,cumulat mm
      REAL          EOEBUDCRP     ! Potential evaporation,crop     mm/d
      REAL          EOEBUDCRP2    ! Potential evaporation,crop     mm/d
      REAL          EOMPEN        ! Potential evap,Penman          mm/d
      REAL          EOMPENC       ! Potential evap,Penman,cum      mm  
      REAL          EOP           ! Potential evaporation,plants   mm/d
      REAL          EOP330        ! Potential tp,full cover,330ppm mm/d
      REAL          EOPCO2        ! Potential tp,full cover,xppm   mm/d
      REAL          EOPEN         ! Potential evaporation,cumulatv mm/d
      REAL          EOPENC        ! Potential evaporation,cumulatv mm  
      REAL          EOPT          ! Potential evaporation,cumulatv mm/d
      REAL          EOPTC         ! Potential evaporation,cumulatv mm  
      REAL          EOSOIL        ! Potential evaporation,no cover mm/d
      REAL          EP            ! Transpiration daily            mm/d 
      REAL          EPCC          ! Transpiration cycle sum        mm 
      REAL          EPPC(0:12)    ! Transpiration cycle sum        mm 
      REAL          EPSRATIO      ! Function,plant/soil evap rate  #
      INTEGER       ERRNUM        ! Error number from compiler     #
      REAL          ERRORVAL      ! Plgro-tfile values/Plgro       #
      REAL          ES            ! Actual soil evaporation rate   mm/d
      REAL          ET            ! Evapotranspiration daily       mm/d 
      REAL          ETCC          ! Evapotranspiration cycle sum   mm 
      REAL          ETPC(0:12)    ! Evapotranspiration phase sum   mm 
      INTEGER       EVALOUT       ! Evaluate output lines for exp  #
      INTEGER       EVHEADNM      ! Number of headings in ev file  #
      INTEGER       EVHEADNMMAX   ! Maximum no headings in ev file #
      REAL          EWAD          ! Ear weight                     kg/ha
      INTEGER       EYEARDOY      ! Emergence Year+DOY             #
      REAL          FAC(20)       ! Factor ((mg/Mg)/(kg/ha))       #
      INTEGER       FAPPNUM       ! Fertilization application number
      INTEGER       FDAY(200)     ! Dates of fertilizer appn (YrDoy)
      REAL          FERNIT        ! Fertilizer N applied           kg/ha
      REAL          FERNITPREV    ! Fertilizer N applied to ystday kg/ha
      INTEGER       FILELEN       ! Length of file name            #
      INTEGER       FLDAP         ! Final leaf date                Yrdoy
      REAL          FLN           ! Final leaf #                   #
      REAL          FLNAITKEN     ! Final leaf #,AIKEN formula     #
      REAL          FNH4          ! Unitless ammonium supply index #
      REAL          FNO3          ! Unitless nitrate supply index  #
      INTEGER       FNUMERA       ! File number,A-data errors      #
      INTEGER       FNUMERR       ! File number,error file         #
      INTEGER       FNUMERS       ! File number,A-data error summary
      INTEGER       FNUMERT       ! File number,T-data errors      #
      INTEGER       FNUMEVAL      ! Number used for evaluate op    #
      INTEGER       FNUMLVS       ! File number,leaves             #
      INTEGER       FNUMMEAS      ! Number used for measured data  #
      !INTEGER      FNUMMETA      ! File number,metadata file      #
      INTEGER       FNUMOV        ! Number used for overview op    #
      INTEGER       FNUMPHA       ! File number,phases             #
      INTEGER       FNUMPHEM      ! File number,phenology,measured #
      INTEGER       FNUMPHES      ! File number,phenology,simulate #
      INTEGER       FNUMPREM      ! File number,measured responses #
      INTEGER       FNUMPRES      ! File number,simulated response #
      INTEGER       FNUMPSUM      ! Number used for plant summary  #
      INTEGER       FNUMREA       ! File number,reads.out file     #
      INTEGER       FNUMT         ! Number used for T-file         #
      INTEGER       FNUMTMP       ! File number,temporary file     #
      INTEGER       FNUMWRK       ! File number,work file          #
      INTEGER       FROP          ! Frquency of outputs,as sent    d
      INTEGER       FROPADJ       ! Frquency of outputs,adjusted   d
      REAL          FSDU          ! Rstage when final sen started  PVoCd
      REAL          FSOILH2O      ! Final soil water               cm   
      REAL          FSOILN        ! Final soil inorganic N         kg/ha
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
      REAL          GLIGP         ! Grain lignin content           %
      REAL          GNAD          ! Grain N                        kg/ha
      REAL          GNAM          ! Grain N at maturity            kg/ha
      REAL          GNAMM         ! Harvest N,mature,measured      kg/ha
      REAL          GNOAD         ! Grains per unit area           #/m2
      REAL          GNOAM         ! Grains per unit area,maturity  #/m2
      REAL          GNOAMM        ! Grain #,mature,measured        #/m2
      REAL          GNOGM         ! Grains/tiller (group),maturity #/gr
      REAL          GNOGMM        ! Grain#/group,mature,measured   #/gr
      REAL          GNONADJ       ! Grain # N stress adj.factor    # 
      REAL          GNONF         ! Grain # N stress adj.factor    % 
      REAL          GNOPD         ! Grains per plant               #/p
      REAL          GNOPM         ! Grains per plant,maturity      #/p
      REAL          GNOWADJ       ! Grain # H2O stress adj.factor  # 
      REAL          GNOWF         ! Grain # H2O stress adj.factor  % 
      REAL          GNOWS         ! Cultivar coefficient,grain #   #/g
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
      REAL          GRNMN         ! Grain N minimum conc,%         #
      REAL          GRNMX         ! Grain N,maximum conc,%         #
      REAL          GRNS          ! Grain N standard conc,%        #
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
      REAL          GROSR         ! Storage root growth            g/p
      REAL          GROST         ! Stem growth rate               g/p
      REAL          GROSTP        ! Stem growth potential          g/p 
      REAL          GROSTPSTORE   ! Stem growth potential,previous g/p 
      REAL          GRP_ET        ! Harvest product per unit water g/mm
      REAL          GRP_Rain      ! Harvest product per unit water g/mm
      REAL          GRWT          ! Grain weight                   g/p
      REAL          GRWTM         ! Grain weight at maturity       g/p
      REAL          GRWTSGE       ! Grain weight,stem growth end   g/p
      REAL          GRWTTMP       ! Grain weight,temporary value   g/p
      REAL          GSTAGE        ! Growth stage                   #
      INTEGER       GSTDCOL       ! Growth stage column number     #
      REAL          GWAD          ! Grain weight                   kg/ha
      REAL          GWAHM         ! Grain weight,harvest,measured  kg/ha
      REAL          GWAM          ! Grain weight,maturity          kg/ha
      REAL          GWAMM         ! Grain weight,maturity,measured kg/ha
      REAL          GWEFR         ! Grain weight end lag,fraction  #
      REAL          GWLFR         ! Grain weight end lag,fraction  #
      REAL          GWPH          ! Grain wt harvested             g/p
      REAL          GWPHC         ! Grain wt harvested,cumulative  g/p
      REAL          GWTA          ! Cultivar coeff,gr.wt.adjusted  mg
      REAL          GWTAA         ! Grain weight adjustment,above  %/g
      REAL          GWTAS         ! Cultivar coeff,gr.wt.adj.sress mg
      REAL          GWTAT         ! Grain weight adj threshold     g/p
      REAL          GWTCF         ! Grain wt compensation factor   #
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
      REAL          H2OPROFILE    ! Total h2o in soil profile      mm   
      REAL          H2OROOTZONE   ! Total h2o in root zone         mm   
      INTEGER       HADOY         ! Harvest day of year            d
      REAL          HAFR          ! Harvested fraction             kg/ha
      REAL          HAMT(HANUMX)  ! Harvest amount                 #
      INTEGER       HANUM         ! Harvest instruction number     # 
      REAL          HARDAYS       ! Accumulated hardening days     #
      REAL          HARDILOS      ! Hardening index loss           #
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
      REAL          HLOSSFR       ! Fraction hardiness days lost   fr
      REAL          HLOSSTEMP     ! Temp threshold,hardiness loss  C
      REAL          HMPC          ! Harvest moisture percent,std.. # 
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
      REAL          ICWT          ! Initial water table depth      cm
      INTEGER       IDETGNUM      ! Number of times into IDETG     #
      INTEGER       IESTG         ! Inflorescence emergence stage  #
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
      INTEGER       KEYSS(KEYSTX) ! Secondary key stage number     #
      INTEGER       KEYSSNUM      ! Secondary key stage total #    #
      INTEGER       L             ! Loop counter                   #
      INTEGER       L1            ! Loop counter                   #
      INTEGER       L2            ! Loop counter                   #
      REAL          LA1S          ! Area of leaf 1,standard        cm2
      REAL          LAFNO         ! Leaf # (one axis),final area   #
      REAL          LAFR          ! Leaf area increase factor,rep  #
      REAL          LAFS          ! Leaf area/all nodes,final      cm2
      REAL          LAFST         ! Leaf area factor change stage  #
      REAL          LAFSTDU       ! Leaf area factor change st,DU  #
      REAL          LAFSWITCH     ! Leaf # changed increase factor #
      REAL          LAFV          ! Leaf area increase factor,veg  #
      REAL          LAGE(0:LNUMX) ! Leaf age at leaf position      C.d
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
      REAL          LANCRS        ! Leaf N concentration,+reserves #
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
      REAL          LAXNO         ! Leaf # (one axis),maximum area #
      REAL          LAXS          ! Area of biggest leaf,main stem cm2
      INTEGER       LCNUM         ! Leaf cohort number (inc.grow)  #
      REAL          LCOA(LCNUMX)  ! Leaf cohort area               cm2
      REAL          LCOAS(LCNUMX) ! Leaf cohort area senesced      cm2
      REAL          LEAFN         ! Leaf N                         g/p
      INTEGER       LENDIS        ! Length,ISWDIS flag             #
      INTEGER       LENENAME      ! Length,experiment description  #
      INTEGER       LENGROUP      ! Length of group name           #
      INTEGER       LENLINE       ! Length of character string     #
      INTEGER       LENLINESTAR   ! Length of character string     #
      INTEGER       LENRNAME      ! Length of run description      #
      INTEGER       LENTNAME      ! Length,treatment description   #
      REAL          LFENDFR       ! Fraction of day leaves growint #
      REAL          LFGSDU        ! Leaf growth start,d.units      #
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
      REAL          LL(20)        ! Lower limit,soil h2o           #
      REAL          LLIFA         ! Leaf life duration,active,phyl #    
      REAL          LLIFATT       ! Leaf life duration,active      C.d
      REAL          LLIFEG(0:LNUMX) ! Leaf expansion growth,phyll  #    
      REAL          LLIFG         ! Leaf growth phase,phyll        #   
      REAL          LLIFGTT       ! Leaf growth phase              C.d
      REAL          LLIFS         ! Leaf senescence duration       phyl
      REAL          LLIFSTT       ! Leaf senescence duration,Ttime C.d
      REAL          LLIGP         ! Leaf lignin percentage         #
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
      REAL          LNGU          ! Leaf N supplied from uptake    g/p
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
      REAL          LNUSE(0:3)    ! Leaf N use,overall and parts   g   
      REAL          LRETS         ! Stage --> dead leaves retained #
      REAL          LRETSDU       ! Stage --> dead leaves retained PVoCd
      INTEGER       LRTIP         ! Layer with root tip            #
      REAL          LSAW          ! Leaf sheath area/wt            cm2/g
      REAL          LSAWR         ! Leaf sheath area/wt,end leaf   cm2/g
      REAL          LSAWV         ! Leaf sheath area/wt,veg.phases cm2/g
      INTEGER       LSEED         ! Layer with seed                #
      REAL          LSENE         ! Leaf senescence,end stage      #
      REAL          LSENEDU       ! Leaf senescence,end stage      #
      REAL          LSENI         ! Leaf senescence,injury         %/d
      REAL          LSENS         ! Leaf senescence,start stage    #
      REAL          LSENSDU       ! Leaf senescence,start stage    PVoCd
      REAL          LSEWF         ! Leaf wt loss,normal senesce    fr
      REAL          LSHAI         ! Leaf sheath area index         m2/m2
      REAL          LSHFR         ! Leaf sheath fraction of total  #
      REAL          LSHRSWAD      ! Leaf sheath reserves weight    kg/ha
      REAL          LSHRSWT       ! Leaf sheath reserves           g/p
      REAL          LSHWAD        ! Leaf sheath weight             kg/ha
      REAL          LSNDEM        ! Leaf and stem N demand         g/p
      REAL          LSNUM(HANUMX) ! Livestock number               #/ha
      REAL          LSTAGE        ! Leaf stage 0-1 over leaf phase #
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
      INTEGER       MESSAGENO     ! Number of Warning messages     #
      REAL          MJPERE        ! Energy per Einstein (300-170)  MJ/E
      INTEGER       MSTG          ! Maturity stage                 #
      REAL          NCNU          ! NO3,NH4 conc factor,N uptake   ppm
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
      REAL          NFP           ! N factor,photosynthesis 0-1    #
      REAL          NFPCAV        ! N factor,phs,average,cycle     #
      REAL          NFPCC         ! N factor,phs,cumulative,cycle  #
      REAL          NFPL          ! N factor,phs,lower limit       #
      REAL          NFPPAV(0:12)  ! N factor,phs,average,phase     #
      REAL          NFPPC         ! N factor,phs,cumulative,phase  #
      REAL          NFPU          ! N factor,phs,upper limit       #
      REAL          NFRG          ! N factor,root growth 0-1       #
      REAL          NFS           ! N factor,senescence            #
      REAL          NFSF          ! N factor,final sen.trigger fr  #
      REAL          NFT           ! N factor,tillering 0-1         #
      REAL          NFTL          ! N factor,tillering,lower limit #
      REAL          NFTU          ! N factor,tillering,upper limit #
      REAL          NH4FN         ! NH4 conc factor,NH4 uptake 0-1 #
      REAL          NH4LEFT(20)   ! NH4 concentration in soil      mg/Mg
      REAL          NLABPC        ! N labile fraction,standard     %
      INTEGER       NLAYR         ! Number of layers in soil       #
      INTEGER       NLAYRROOT     ! Number of layers with roots    #
      INTEGER       NLIMIT        ! N limited grain growth  (Days) #
      REAL          NO3FN         ! NO3 conc factor,NO3 uptake 0-1 #
      REAL          NO3LEFT(20)   ! NO3 concentration in soil      mg/Mg
      INTEGER       NOUTPG        ! Number for growth output file  #
      INTEGER       NOUTPG2       ! Number for growth output file2 #
      INTEGER       NOUTPGF       ! Number for growth factors file #
      INTEGER       NOUTPGN       ! Number for growthN output file #
      REAL          NPOOLL        ! Leaf N pool (ie.above minimum) g/p
      REAL          NPOOLNEED     ! N required for grain growth    g/p
      REAL          NPOOLR        ! Root N pool (ie.above minimum) g/p
      REAL          NPOOLS        ! Stem N pool (ie.above minimum) g/p
      REAL          NPTFL         ! Minimum N uptake used by leaf  frFR
      INTEGER       NSDAYS        ! N stress days                  #
      REAL          NTUPF         ! N top-up fraction              /d
      REAL          NUF           ! Plant N supply/demand,max=1.0  #
      REAL          NULEFT        ! N uptake remaining for use     g 
      REAL          NULEFTL       ! N uptake remaining fro leaves  g
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
      REAL          NUSEFAC       ! N use factor;mx nuselim,navfr  #
      REAL          NUSELIM       ! N limit on N for grain filling #
      INTEGER       ON            ! Option number (sequence runs)  #
      INTEGER       ONI           ! Option number,initial value    #
      INTEGER       OUTCOUNT      ! Output counter                 #
      REAL          PARFC         ! Max photosynthesis/phs at 330  #
      REAL          PARIF         ! PAR interception fraction      #
      REAL          PARIF1        ! PAR interception fr,1-crop mdl #
      REAL          PARIFOUT      ! PAR interception fr for output #
      REAL          PARIFPREV     ! PAR interception fr,previous   #
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
      REAL          PARURFR       ! PAR utilize factor,reprod fr   #
      INTEGER       PATHL         ! Path length                    #
      REAL          PD(0:PSX)     ! Phase durations                deg.d
      REAL          PD2ADJ        ! Phase 2+3 adjusted             deg.d
      REAL          PDADJ         ! Phase duration adjustment      deg.d
      INTEGER       PDATE         ! Planting Yrdoy from X-file     #
      INTEGER       PDAYS(0:12)   ! Phase durations                PVoCd
      REAL          PDFS          ! Phase duration,final senescenc deg.d
      REAL          PDL(0:10)     ! Phase durations,phint units    #    
      REAL          PDMTOHAR      ! Phase duration,mature->harvest deg.d
      REAL          PDSRI         ! Storage root initiation phase  oC.d
      REAL          PECM          ! Phase duration,emergence       Cd/cm
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
      REAL          PLWTGS        ! Plant weight at grain set      g/p 
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
      REAL          PSTART(0:PSX) ! Principal phase thresholds     du
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
      INTEGER       REP           ! Number of run repetitions      #
      REAL          RESCAL(0:20)  ! Residue C at harvest,by layer  kg/ha
      REAL          RESCALG(0:20) ! Residue C added,by layer       kg/ha
      REAL          RESLGAL(0:20) ! Residue lignin,harvest,bylayer kg/ha
      REAL          RESLGALG(0:20)! Residue lignin added,layer     kg/ha
      REAL          RESNAL(0:20)  ! Residue N at harvest by layer  kg/ha
      REAL          RESNALG(0:20) ! Residue N added,by layer       kg/ha
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
      REAL          RLFNU         ! Root length factor,N uptake   cm/dm3
      REAL          RLFWU         ! Root length factor,water uptk  /cm2
      REAL          RLIGP         ! Root lignin concentration      %
      REAL          RLV(20)       ! Root length volume by layer    cm-2
      REAL          RLWR          ! Root length/weight ratio     m/10mg
      REAL          RM            ! Mesophyll resistance           d/m
      REAL          RMSE(30)      ! Root mean square error values  #
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
      REAL          RNH4U(20)     ! Potential ammonium uptake      kg/ha
      INTEGER       RNI           ! Replicate number,initial value #
      REAL          RNO3U(20)     ! Potential nitrate uptake       kg/ha
      REAL          RNPCMN(0:1)   ! Root N conc,minimum            %
      REAL          RNPCS(0:1)    ! Roots N conc,standard,by stage %
      REAL          RNUMX         ! Root N uptake,maximum          mg/cm
      REAL          RNUSE(0:3)    ! Root N use,overall and parts   g   
      REAL          ROOTN         ! Root N                         g/p
      REAL          ROOTNS        ! Root N senesced                g/p
      REAL          ROWSPC        ! Row spacing                    cm
      INTEGER       RPCOL         ! Replicate column number        #
      REAL          RRESP         ! Root respiration fraction      #
      REAL          RSADJ         ! Stomatal res,adjusted Co2+H2o  d/m
      REAL          RSCA          ! Reserves conc,anthesis         %
      REAL          RSCD          ! Reserves concentration,end day fr
      REAL          RSCLX         ! Reserves conc,leaves,max.      #
      REAL          RSCM          ! Reserves concentration,mature  fr
      REAL          RSCMM         ! Reserves conc,maturity,msured  #
      REAL          RSCO2         ! Stomatal res,adjusted for Co2  d/m
      REAL          RSCS          ! Reserves concentration,std     #
      REAL          RSCX          ! Reserves concentration,maximum fr
      REAL          RSEN          ! Root senescence fraction       #
      REAL          RSFP          ! Reserves factor,photosynthesis fr
      REAL          RSFPL         ! Reserves conc.,phs.lower bound fr
      REAL          RSFPU         ! Reserves conc.,phs upper bound fr
      REAL          RSFRS         ! Reserves fraction,standard     #
      REAL          RSN           ! Reserve N                      g/p
      REAL          RSNAD         ! Reserve N                      kg/ha
      REAL          RSNEED        ! Reserves need to bring to min  g/p
      REAL          RSNPH         ! Reserves N harvested           g/p
      REAL          RSNPHC        ! Reserves N harvested,cum       g/p
      REAL          RSNUSE        ! Reserve N use                  g  
      REAL          RSOIL         ! Soil resistance                s/m
      REAL          RSTAGE        ! Reproductive develoment stage  #
      REAL          RSTAGEFS      ! Rstage when final sen started  #
      REAL          RSTAGEP       ! Reproductive dc stage,previous #
      REAL          RSTAGETMP     ! Reproductive develoment stage  #
      REAL          RSUSE         ! Reserves utilisation fraction  #
      REAL          RSWAA         ! Reserve weight,anthesis        g/p 
      REAL          RSWAAM        ! Reserve wt,anthesis,measured   g/p
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
      REAL          RTNO3         ! N uptake/root length           mg/cm
      REAL          RTNH4         ! N uptake/root length           mg/cm
      REAL          RTNSL(20)     ! Root N senesced by layer       g/p
      REAL          RTRESP        ! Root respiration               g/p
      INTEGER       RTSLXDATE     ! Roots into last layer date     YYDDD
      REAL          RTUFR         ! Max fraction root wt useable   fr
      REAL          RTWT          ! Root weight                    g/p
      REAL          RTWTAL(20)    ! Root weight by layer           kg/ha
      REAL          RTWTG         ! Root weight growth             g/p
      REAL          RTWTGL(20)    ! Root weight growth by layer    g/p
      REAL          RTWTL(20)     ! Root weight by layer           g/p
      REAL          RTWTM         ! Root weight,maturity           g/p
      REAL          RTWTSGE       ! Root weight,stem growth end    g/p
      REAL          RTWTSL(20)    ! Root weight senesced by layer  g/p
      REAL          RTWTUL(20)    ! Root weight used for tops,lyr  g/p
      REAL          RUESTG        ! Stage at which RUE changes     #
      REAL          RUESTGDU      ! Stage at which RUE changes     PVoCD
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
      REAL          SANCOUT       ! Stem+LeafSheaths N conc        #
      REAL          SAT(20)       ! Saturated limit,soil           #
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
      REAL          SDRSF         ! Seed reserves fraction of seed #
      REAL          SDSZ          ! Seed size                      g
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
      REAL          SENCALG(0:20) ! Senesced C added,by layer      kg/ha
      REAL          SENCAS        ! Senesced C added to soil       kg/ha
      REAL          SENCL(0:20)   ! Senesced C,by layer            g/p
      REAL          SENCS         ! Senesced C added to soil       g/p
      REAL          SENFR         ! Senesced fraction lost from pl #
      REAL          SENGF         ! Senesced during grain fill     g/p
      REAL          SENLA         ! Senesced leaf area,total       cm2/p
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
      REAL          SENTOPG       ! Senescent top material growth  g/p
      REAL          SENTOPGGF     ! Senescent top gr.,grain fill   g/p
      REAL          SENWACM       ! Senesced weight,total,cum to m kg/ha
      REAL          SENWACMM      ! Senesced om,litter+soil,cum,ms kg/ha
      REAL          SENWAGS       ! Senesced weight added to soil  kg/ha
      REAL          SENWAL(0:20)  ! Senesced om by layer           kg/ha
      REAL          SENWALG(0:20) ! Senesced om added by layer     kg/ha
      REAL          SENWAS        ! Senesced weight,soil,cumulativ kg/ha
      REAL          SENWL(0:20)   ! Senesced om (cumulative),layer g/p
      REAL          SENWS         ! Senesced weight,soil,cum       g/p
      REAL          SERX          ! Shoot elongation rate,max      cm/Du
      REAL          SGEDAPFR      ! Stem growth end DAP+fr         #
      INTEGER       SGEDAPM       ! Stem growth end date,measured  #
      REAL          SGPHASE(2)    ! Stem growth phase start,end    #
      REAL          SGPHASEDU(2)  ! Stem growth phase start,end    Du
      REAL          SHF(20)       ! Soil hospitality factor 0-1    #
      REAL          SHRTD         ! Shoot/root ratio               #
      REAL          SHRTM         ! Shoot/root ratio,maturity      #
      REAL          SHRTMM        ! Shoot/root ratio,maturity,meas #
      REAL          SLA           ! Specific leaf area             cm2/g
      REAL          SLAOUT        ! Specific leaf area for output  cm2/g
      REAL          SLIGP         ! Stem lignin concentration      %
      REAL          SLPF          ! Soil factor for photosynthesis %
      REAL          SMDFR         ! Soil moisture factor,N uptake  #
      INTEGER       SN            ! Sequence number,crop rotation  #
      REAL          SNAD          ! Stem N (stem+sheath+rs)        kg/ha
      REAL          SNCM          ! Stem N conc,minimum            fr
      REAL          SNCMN(0:1)    ! Stem N conc,minimum            fr
      REAL          SNCR          ! Stem N relative to maximum     #
      REAL          SNCX          ! Stem N conc,maximum            fr
      REAL          SNCXS(0:1)    ! Stem N conc,maximum,stage      fr
      REAL          SNDEM         ! Stem demand for N              g/p
      REAL          SNDEMMIN      ! Stem demand for N,minimum      g/p
      REAL          SNGU          ! Stem N growth from uptake      g/p
      REAL          SNGUL         ! Stem N from uptake used for lv g/p
      REAL          SNH4(20)      ! Soil NH4 N                     kg/ha
      REAL          SNH4PROFILE   ! Soil NH4 N in profile          kg/ha
      REAL          SNH4ROOTZONE  ! Soil NH4 N in root zone        kg/ha
      INTEGER       SNI           ! Sequence number,as initiated   #
      REAL          SNO3(20)      ! Soil NO3 N                     kg/ha
      REAL          SNO3PROFILE   ! Soil NO3 N in profile          kg/ha
      REAL          SNO3ROOTZONE  ! Soil NO3 N in root zone        kg/ha
      REAL          SNOFX         ! Shoot number per fork          #
      REAL          SNOT          ! Shoot number/tiller (>forking) #
      REAL          SNOTPREV      ! Shoot number/tiller,previous   #
      REAL          SNOW          ! Snow                           cm
      REAL          SNPCMN(0:1)   ! Stem N conc,minimum            %
      REAL          SNPCS(0:1)    ! Stem N conc,standard,stage     %
      REAL          SNPH          ! Stem N harvested               g/p
      REAL          SNPHC         ! Stem N harvested,cumulative    g/p
      REAL          SNUSE(0:3)    ! Shoot N use,overall and parts  g    
      REAL          SPANPOS       ! Position along kill,etc.range  fr
!     INTEGER       SPDATM        ! Spike emergence date,measured  #
      REAL          SPNUMH        ! Spike number harvested         #  
      REAL          SPNUMHC       ! Spike number harvested,cumulat #  
      REAL          SPNUMHCM      ! Spike # harvested,cum,measured #  
      REAL          SPNUMHFAC     ! Spike # harvestd factor (0-1)  #  
      REAL          SPRL          ! Sprout/cutting length          cm
      REAL          SRAD          ! Solar radiation                MJ/m2
      REAL          SRAD20        ! Solar radiation av,20 days     MJ/m2
      REAL          SRAD20A       ! Solar radn av,20 days,anthesis MJ/m2
      REAL          SRAD20S       ! Solar radiation sum            MJ/m2
      REAL          SRADC         ! Solar radiation,cumulative     MJ/m2
      REAL          SRADCAV       ! Solar radiation,cycle average  MJ/m2
      REAL          SRADCC         ! Radiation,cycle sum           Mj/m2
      REAL          SRADD(20)     ! Solar radiation on specific d  MJ/m2
      REAL          SRADPAV(0:12) ! Solar radiation,phase average  MJ/m2
      REAL          SRADPC        ! Solar radiation,phase sum      MJ/m2
      REAL          SRADPREV      ! Solar radiation,previous day   MJ/m2
      REAL          SRDAYFR       ! Storage root fraction of day   #
      REAL          SRFR          ! Storage root fraction,basic    #
      REAL          SRNAD         ! Storage root N                 kg/ha
      REAL          SRNAM         ! Storage root N at maturity     kg/ha
      REAL          SRNC          ! Storage root N concentration   g/p
      REAL          SRNDEM        ! Storage root demand for N      g/p
      REAL          SRNOAD        ! Storage root/group,given day   #/m2
      REAL          SRNOAM        ! Storage root #/area,maturity   #/m2
      REAL          SRNOAMM       ! Storage root/group,mature,meas #
      REAL          SRNOGM        ! Storage root/group,maturity    #
      REAL          SRNOGMM       ! Storage root/group,mature,meas #
      INTEGER       SRNOPD        ! Storage root number per plant  # 
      REAL          SRNOW         ! Cultivar coeff,storage root #  #/g
      REAL          SRNPCM        ! Storage root N%,maturity       %
      REAL          SRNS          ! Storage root N conc,standard   #
      REAL          SRNUSE(0:3)   ! Storage root N use,total/parts g   
      REAL          SROOTN        ! Storage root N                 g/p
      REAL          SRPRS         ! Storage protein standard %     #
      INTEGER       SRSTAGE       ! Storage root 2ndary stage #    #
      REAL          SRWAD         ! Storage root weight            kg/ha
      REAL          SRWT          ! Root storage organ weight      g/p
      REAL          SRWTGRS       ! Root storage organ gr,reserves g/p
      REAL          SRWUD         ! Storage root size              g
      REAL          SRWUM         ! Storage root wt/unit,maturity  g
      REAL          SRWUMM        ! Storage root wt/unit,mat,meas  g
      INTEGER       SSDAP(SSX)    ! Stage DAP                      #
      REAL          SSDAPFR(SSX)  ! Stage DAP+fr                   #
      INTEGER       SSDAPM(SSX)   ! Stage DAP,measured             #
      INTEGER       SSDAT(SSX)    ! Secondary stage dates          Yrdoy
      INTEGER       SSDATM(SSX)   ! Stage date,measured            #
      REAL          SSDAYFR(SSX)  ! Stage fraction of day          #
      REAL          SSENF         ! Stem N loss fr when senesce    #
      INTEGER       SSNUM         ! Secondary stage number         #
      REAL          SSPHASE(2)    ! Stem senesce phase start,end   #
      REAL          SSPHASEDU(2)  ! Stem senesce phase start,end   Du
      REAL          SSTG(SSX)     ! Secondary stage occurence,rstg #
      REAL          SSTH(SSX)     ! Secondary stage thresholds     du
      REAL          ST(0:NL)      ! Soil temperature in soil layer C
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
      REAL          STEMNGL       ! Stem N growth from leaves      g/p
      INTEGER       STEP          ! Step number                    #
      INTEGER       STEPNUM       ! Step number per day            #
      INTEGER       STGEDAT       ! Stem growth end date (Yrdoy)   #
      REAL          STGEFR        ! Stem growth end time,fr day    #
      INTEGER       STGYEARDOY(20)! Stage dates (Year+Doy)         #
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
      REAL          TFGF          ! Temperature factor,gr fill 0-1 #
      REAL          TFGN          ! Temperature factor,grain N 0-1 #
      REAL          TFH           ! Temperature factor,hardening   #
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
      REAL          TILDAP        ! Tillering start DAP            #
      INTEGER       TILDAPM       ! Tillering date,measured        #
      REAL          TILDAT        ! Tillering start date (YEARDOY) #
      REAL          TILDE         ! Tiller death end stage         #
      REAL          TILDEDU       ! Tiller death end stage         #
      REAL          TILDF         ! Tiller death rate,fr (wt=2xst) #
      REAL          TILDS         ! Tiller death start stage       #
      REAL          TILDSDU       ! Tiller death start stage       #
      REAL          TILIFAC       ! Tiller initiation factor 0-1   #
      REAL          TILIP         ! Tiller initiation phase Leaves #
      REAL          TILPE         ! Tiller number growth end stg   #
      REAL          TILPEDU       ! Tiller number growth end stg   PVoCd
      REAL          TILSF         ! Tiller death stress 0-2 factor # 
      REAL          TILSW         ! Tiller standard weight         g/s
      REAL          TILWT         ! Tiller weight                  g/s
      REAL          TILWTR        ! Tiller wt relative to standard #
      REAL          TIMENEED      ! Time needed to finish phase    fr
      REAL          TINOX         ! Shoot number,maximum           #/p
      REAL          TKFH          ! Start kill temp.fully hardened C
      REAL          TKILL         ! Temperature for plant death    C
      REAL          TKLF          ! Start killing temp,leaves      C
      REAL          TKLFD         ! Start killing temp,leaves v pl C
      REAL          TKSPAN        ! Temperature span for kill,etc. C
      REAL          TKTI          ! Start kill temperature,tillers C
      REAL          TKTID         ! Start kill difference,till/pl  C
      REAL          TKUH          ! Start kill temp,unhardened     C
      REAL          TLA(25)       ! Tiller leaf area produced      cm2
      REAL          TLAG(25)      ! Tiller leaf area growth        cm2
      REAL          TLAGP(25)     ! Tiller potential leaf area gr  cm2
      REAL          TLAS(25)      ! Tiller leaf area senesced      cm2
      REAL          TLCHC         ! Cumulative N leached>planting  kg/ha
      REAL          TLCHD         ! N leached this day             kg/ha
      INTEGER       TLIMIT        ! Temp.limited grain gr (Days)   #
      INTEGER       TLINENUM      ! Temporary var,# lines in tfile #
      INTEGER       TLPOS         ! Position on temporary line     #
      REAL          TMAX          ! Temperature maximum            C
      REAL          TMAXCAV       ! Temperature,maximum,cycle av   C
      REAL          TMAXCC         ! Temperature,max,cycle sum     C.d 
      REAL          TMAXM         ! Temperature maximum,monthly av C
      REAL          TMAXPAV(0:12) ! Temperature,maximum,phase av   C
      REAL          TMAXPC        ! Temperature,maximum,phase sum  C
      REAL          TMAXSUM       ! Temperature maximum,summed     C
      REAL          TMAXX         ! Temperature max during season  C
      REAL          TMEAN         ! Temperature mean (TMAX+TMIN/2) C
      REAL          TMEAN20       ! Temperature mean over 20 days  C
      REAL          TMEAN20A      ! Temperature mean,20 d~anthesis C
      REAL          TMEAN20P      ! Temperature mean,20 d>planting C
      REAL          TMEAN20S      ! Temperature sum over 20 days   C
      REAL          TMEANAV(0:12) ! Temperature,mean,phase av      C
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
      REAL          TMIN          ! Temperature minimum            C
      REAL          TMINCAV       ! Temperature,minimum,cycle av   C
      REAL          TMINCC         ! Temperature,min,cycle sum     C.d  
      REAL          TMINM         ! Temperature minimum,monthly av C
      REAL          TMINN         ! Temperature min during season  C
      REAL          TMINPAV(0:12) ! Temperature,minimum,phase av   C
      REAL          TMINPC        ! Temperature,minimum,phase sum  C
      REAL          TMINSUM       ! Temperature minimum,summed     C
      INTEGER       TN            ! Treatment number               #
      REAL          TNAD          ! Total nitrogen (tops+roots)    kg/ha
      REAL          TNAMM         ! Total N at maturity,measured   kg/ha
      INTEGER       TNI           ! Treatment number,initial value #
      REAL          TNIMBSOM      ! Total N immobilised by SOM     kg/ha
      REAL          TNOXC         ! Cumulative N denitrified       kg/ha
      REAL          TNOXD         ! N denitrified this day         kg/ha
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
      REAL          TNUMPREV      ! Tiller (incl.main stem) number #/p
      REAL          TNUMT         ! Shoot number from t file       #/m2
      REAL          TNUMX         ! Tiller (incl.main stem) max.#  #/p
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
      REAL          TRCOH(4)      ! Temp response,cold hardening   #
      REAL          TRDEV(4,9)    ! Temp response,development,phse #
      REAL          TRDV1(4)      ! Temp response,development 1    #
      REAL          TRDV2(4)      ! Temp response,development 2    #
      REAL          TRDVX(4)      ! Temp response,development,X    #
      REAL          TRGFC(4)      ! Temp response,grain fill,C     #
      REAL          TRGFN(4)      ! Temp response,grain fill,N     #
      REAL          TRLDF         ! Intermediate factor,new roots  #
      REAL          TRLFG(4)      ! Temp response,leaf growth      #
      REAL          TRLV          ! Total root length density      /cm2
      REAL          TRPHS(4)      ! Temp response,photosynthesis   #
      REAL          TRVRN(4)      ! Temp response,vernalization    #
      REAL          TRWU          ! Total water uptake             mm
      REAL          TRWUP         ! Total water uptake,potential   cm
      REAL          TSDEP         ! Average temp in top 10 cm soil C
      REAL          TSRAD         ! Transmission,SRAD,fraction     #
      INTEGER       TSSTG         ! Terminal spikelet stage        #
      REAL          TT            ! Daily thermal time             C.d
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
      INTEGER       TVICOLNM      ! Column number function output  #
      INTEGER       TVILENT       ! Temporary integer,function op  #
      REAL          TVR1          ! Temporary real variable        #
      REAL          TVR2          ! Temporary real variable        #
      REAL          TVR3          ! Temporary real variable        #
      REAL          TVR4          ! Temporary real variable        #
      REAL          TVR5          ! Temporary real variable        #
      REAL          TWAD          ! Total weight (tops+roots)      kg/ha
      REAL          UH2O(NL)      ! Uptake of water                cm/d
      REAL          UNH4(20)      ! Uptake of NH4 N                kg/ha
      REAL          UNO3(20)      ! Uptake of NO3 N                kg/ha
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
      REAL          VF            ! Vernalization factor 0-1       #
      REAL          VFNEXT        ! Vernalization factor,next ph   #
      REAL          VLOSS0STG     ! Vernalization stage for 0 loss #
      REAL          VLOSSFR       ! Fraction of vernalization lost fr
      REAL          VLOSSTEMP     ! Vernalization loss threshold   C
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
      REAL          VPENDFR       ! Vernalization process end frdy #
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
      REAL          WFGEM         ! Water factor,germ,emergence    #
      REAL          WFGL          ! Water factor,growth,lower      #
      REAL          WFGPAV(0:12)  ! Water factor,growth,average    #
      REAL          WFGPC         ! Water factor,growth,cumulative #
      REAL          WFGU          ! Water factor,growth,upper      #
      REAL          WFLAW         ! Water factor,leaf area/weight  #
      REAL          WFLF(LNUMX)   ! H2O factor for leaf,average    #
      REAL          WFLFP(LNUMX)  ! H2O factor,phs leaf,average    #
      REAL          WFNU          ! Water factor,N uptake          #
      REAL          WFNUL         ! Water factor,N uptake,lower    #
      REAL          WFNUU         ! Water factor,N uptake,upper    #
      REAL          WFP           ! Water factor,photosynthsis 0-1 #
      REAL          WFPCAV        ! Water factor,phs,av 0-1,cycle  #
      REAL          WFPCC         ! H20 factor,phs,cycle sum       # 
      REAL          WFPL          ! Water factor,phs,lower         #
      REAL          WFPPAV(0:12)  ! Water factor,phs,average 0-1   #
      REAL          WFPPC         ! Water factor,phs,cumulative    #
      REAL          WFPU          ! Water factor,phs,upper         #
      REAL          WFRG          ! Water factor,root growth,0-1   #
      REAL          WFRTG         ! Water factor,root gr           #
      REAL          WFS           ! Water factor,senescence 0-1    #
      REAL          WFSF          ! WFS trigger,final senescence   #
      REAL          WFT           ! Water factor,tillering 0-1     #
      REAL          WFTL          ! Water factor,tillering,lower   #
      REAL          WFTU          ! Water factor,tillering,upper   #
      REAL          WINDSP        ! Wind speed                     m/s
      INTEGER       WSDAYS        ! Water stress days              #
      REAL          WTDEP         ! Water table depth              cm
      REAL          WUPR          ! Water pot.uptake/demand        #
      REAL          WUPRD(20)     ! Water pot.uptake/demand,ind dy #
      REAL          XDEP          ! Depth to bottom of layer       cm
      REAL          XDEPL         ! Depth to top of layer          cm
      INTEGER       YEAR          ! Year                           #
      INTEGER       YEARCOL       ! Colum number for year data     #
      INTEGER       YEARDOY       ! Year+Doy (7digits)             #
      INTEGER       YEARDOYHARF   ! Harvest year+doy,fixed         #
      INTEGER       YEARDOYPREV   ! Year+Doy (7digits),previous    #
      INTEGER       YEARM         ! Year of measurement            #
      INTEGER       YEARSIM       ! Year+Doy for simulation start  #
      INTEGER       YRPLT         ! Planting year*1000+DOY,CSM     #
      REAL          YVALXY        ! Y value from function          #
      REAL          ZSTAGE        ! Zadoks stage of development    #

      CHARACTER (LEN=128) ARG           ! Argument component
      CHARACTER (LEN=6)   CAIC          ! Canopy area index
      CHARACTER (LEN=6)   CANHTC        ! Canopy height
      CHARACTER (LEN=120) CFGDFILE      ! Configuration directory+file
      CHARACTER (LEN=1)   CFLFAIL       ! Control flag for failure
      CHARACTER (LEN=1)   CFLFLN        ! Control flag,final leaf # Y/N
      CHARACTER (LEN=1)   CFLHAR        ! Control flag for final harvest
      CHARACTER (LEN=1)   CFLHARMSG     ! Control flag,harvest message
      CHARACTER (LEN=1)   CFLPDADJ      ! Control flag,phase adjustment 
      CHARACTER (LEN=1)   CFLPRES       ! Control flag for headers,PRES
      CHARACTER (LEN=1)   CFLSDRSMSG    ! Control flag,seed reserves msg
      CHARACTER (LEN=10)  CNCHAR        ! Crop component (multicrop)
      CHARACTER (LEN=2)   CNCHAR2       ! Crop component (multicrop)
      CHARACTER (LEN=2)   CROP          ! Crop identifier (ie. WH, BA)
      CHARACTER (LEN=2)   CROPPREV      ! Crop identifier,previous run
      CHARACTER (LEN=93)  CUDIRFLE      ! Cultivar directory+file
      CHARACTER (LEN=93)  CUDIRFLPREV   ! Cultivar directory+file,prev
      CHARACTER (LEN=12)  CUFILE        ! Cultivar file
      CHARACTER (LEN=10)  DAPCHAR       ! DAP in character form
      CHARACTER (LEN=6)   DAPWRITE      ! DAP character string -> output
      CHARACTER (LEN=64)  ECDIRFLE      ! Ecotype directory+file
      CHARACTER (LEN=64)  ECDIRFLPREV   ! Ecotype directory+file,prev
      CHARACTER (LEN=12)  ECFILE        ! Ecotype filename
      CHARACTER (LEN=6)   ECONO         ! Ecotype code
      CHARACTER (LEN=6)   ECONOPREV     ! Ecotype code,previous
      CHARACTER (LEN=60)  ENAME         ! Experiment description
      CHARACTER (LEN=1)   ESTABLISHED   ! Flag,crop establishment Y/N
      CHARACTER (LEN=14)  EVHEADER      ! Evaluater.out header
      CHARACTER (LEN=10)  EXCODE        ! Experiment code/name
      CHARACTER (LEN=10)  EXCODEPREV    ! Previous experiment code/name
      CHARACTER (LEN=80)  FAPPLINE(30)  ! Fertilizer application details
      CHARACTER (LEN=120) FILEA         ! Name of A-file
      CHARACTER (LEN=120) FILEIO        ! Name of input file,after check
      CHARACTER (LEN=120) FILEIOIN      ! Name of input file
	CHARACTER (LEN=107) FILEADIR      ! Name of A-file directory       
      CHARACTER (LEN=3)   FILEIOT       ! Type of input file
      CHARACTER (LEN=120) FILENEW       ! Temporary name of file
      CHARACTER (LEN=120) FILET         ! Name of T-file
      CHARACTER (LEN=120) FNAMEERA      ! File name,A-errors
      CHARACTER (LEN=120) FNAMEERS      ! File name,A-errors summary
      CHARACTER (LEN=120) FNAMEERT      ! File name,T-errors
      CHARACTER (LEN=120) FNAMEEVAL     ! File name,evaluate outputs
      CHARACTER (LEN=120) FNAMELEAVES   ! File name,leaves outputs
      CHARACTER (LEN=120) FNAMEMEAS     ! File name,measured outputs
      !CHARACTER (LEN=120) FNAMEMETA    ! File name,metadata outputs
      CHARACTER (LEN=120) FNAMEOV       ! File name,overview outputs
      CHARACTER (LEN=120) FNAMEPHASES   ! File name,phases outputs
      CHARACTER (LEN=120) FNAMEPHENOLM  ! File name,phenology measured
      CHARACTER (LEN=120) FNAMEPHENOLS  ! File name,phenology outputs
      CHARACTER (LEN=120) FNAMEPREM     ! File name,responses,measured
      CHARACTER (LEN=120) FNAMEPRES     ! File name,responses,simulated
      CHARACTER (LEN=120) FNAMEPSUM     ! File name,plant summary
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
      CHARACTER (LEN=1)   MEPHS         ! Switch,photosynthesis method
      CHARACTER (LEN=3)   MERNU         ! Switch,root N uptake methd
      CHARACTER (LEN=1)   MESOM         ! Switch,OM decay method       
      CHARACTER (LEN=78)  MESSAGE(Messagenox) ! Messages for Warning.out
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
      CHARACTER (LEN=80)  PATHCR        ! Path to genotype (CUL) files
      CHARACTER (LEN=80)  PATHEC        ! Path to genotype (ECO) files
      CHARACTER (LEN=80)  PATHSP        ! Path to genotype (SPE) files
      CHARACTER (LEN=1)   PLME          ! Planting method (code)        
      CHARACTER (LEN=2)   PPSEN         ! Code,photoperiod sensitivity
      CHARACTER (LEN=5)   PSABV(PSX)    ! Principal stage abbreviation
      CHARACTER (LEN=5)   PSABVO(PSX)   ! Principal stage abv,output
      CHARACTER (LEN=13)  PSNAME(PSX)   ! Principal stage names
      CHARACTER (LEN=1)   PSTYP(PSX)    ! Principal stage type
      CHARACTER (LEN=1)   RNMODE        ! Run mode (eg.I=interactive)
      CHARACTER (LEN=75)  RUNNAME       ! Run title
      CHARACTER (LEN=8)   RUNRUNI       ! Run+internal run number
      CHARACTER (LEN=6)   SDWADC        ! Seed weight
      CHARACTER (LEN=1)   SEASENDOUT    ! Season end outputs flag     
      CHARACTER (LEN=6)   SENN0C        ! Senesced N added to litter
      CHARACTER (LEN=6)   SENNSC        ! Senesced N added to soil
      CHARACTER (LEN=6)   SENWSC        ! Senesced OM,soil
      CHARACTER (LEN=6)   SENW0C        ! Senesced OM added to surface
      CHARACTER (LEN=64)  SPDIRFLE      ! Species directory+file
      CHARACTER (LEN=64)  SPDIRFLPREV   ! Species directory+file,last
      CHARACTER (LEN=12)  SPFILE        ! Species filename
      CHARACTER (LEN=5)   SSABV(SSX)    ! Secondary stage abbreviation
      CHARACTER (LEN=5)   SSABVO(SSX)   ! Secondary stage abv,output
      CHARACTER (LEN=13)  SSNAME(SSX)   ! Secondary stage names
      CHARACTER (LEN=1)   SSTYP(SSX)    ! Secoudary stage type
      CHARACTER (LEN=6)   TCHAR         ! Temporary character string
      CHARACTER (LEN=6)   THEAD(20)     ! T-file headings
      CHARACTER (LEN=1)   TIERNUMC      ! Tier number in t-file
      CHARACTER (LEN=10)  TL10          ! Temporary line              
      CHARACTER (LEN=10)  TL10FROMI     ! Temporary line from integer
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

      ! For Ceres temperature response
      ! REAL RGFILL
     
      ! For Jamieson model need:
      !REAL  DAYLSAT,LNUMDLR,LNUMMIN,PRNUM,PRNUMCRIT 
      
      ! PROBLEMS
      !   1. Chaff weight is too low for stressed treatment (UXMC)
      !      Need reserves to be pumped into chaff somehow!

      SAVE

!-----------------------------------------------------------------------
!       Set date, environmental equivalents, and run flag
!-----------------------------------------------------------------------

      ! LAH CHP CHECK THIS 
      IF (DYNAMIC.LT.DYNAMICPREV) THEN
        YEARDOY = YEAR*1000 + DOY
        TMEAN = (TMAX+TMIN)/2.0
        IF (SNOW.LE.0) THEN
          TMEANSURF = TMEAN
        ELSE
          TMEANSURF = 0.0
        ENDIF
        CO2AIR = 1.0E12*CO2*1.0E-6*44.0 /       ! CO2 in g/m3
     &   (8.314*1.0E7*((TMAX+TMIN)*0.5+273.0))
        IF (EXCODE.EQ.EXCODEPREV) THEN
          IF (TNUM.NE.TNUMPREV) THEN
            RUNCRP = 0
          ELSEIF (TNUM.EQ.TNUMPREV) THEN  
            IF (YEARDOY.LT.YEARDOYPREV) RUNCRP = 0
          ENDIF  
        ELSE
          RUNCRP = 0  
        ENDIF
      ENDIF

!***********************************************************************
      IF (DYNAMIC.EQ.SEASINIT) THEN    ! Initialization
!***********************************************************************

        IF (RUNCRP.LE.0) THEN          ! First time through

          Messageno = 0 ! Warning.out message counter
          
!-----------------------------------------------------------------------
!         Record/set starting information
!-----------------------------------------------------------------------

          YEARSIM = YEAR*1000 + DOY
          RUNCRP = 1

!-----------------------------------------------------------------------
!         Set parameters (Most should be placed in input files!)
!-----------------------------------------------------------------------

          MODNAME(1:8) = 'CSCRP045'

          DAS = 0
          SEASENDOUT = 'N'  ! Season end outputs flag     

          ! Physical constants
          MJPERE = 220.0*1.0E-3  ! MJ per Einstein at 540 nm
          PARMJFAC = 0.5         ! PAR in SRAD (fr)

          ! Model standard parameters
          STDAY = 20.0      ! TT in standard day
          STEPNUM = 1       ! Step number per day set to 1
          
          ! Model methods that not in control file
          MERNU = 'CSM'     ! Root N uptake 

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
!         Determine input file type (Dssat or X-file) and check if exists
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
!         Read command line arguments for model name and path
!-----------------------------------------------------------------------

          arg = ' '
          tvi2 = 0
          tvi3 = 0
          tvi4 = 0
          ! CALL GETARG (0,arg,arglen)
          !portability
          call getarg(0,arg)
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
!         Set configuration file name
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
          CALL GETLUN ('WORK.OUT',FNUMWRK)
          CALL GETLUN ('ERROR.OUT',FNUMERR)
          CALL GETLUN ('FNAMETMP',FNUMTMP)

          ! IDETG FILES
          OUTPG = 'PlantGro.'//OUT
          OUTPG2 = 'PlantGr2.'//OUT
          OUTPGF = 'PlantGrf.'//OUT
          OUTPN = 'PlantN.'//OUT
          CALL GETLUN (OUTPG,NOUTPG)
          CALL GETLUN (OUTPG2,NOUTPG2)
          CALL GETLUN (OUTPGF,NOUTPGF)
          CALL GETLUN (OUTPN,NOUTPGN)

          ! IDETO FILES
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
          FNAMEPHASES(1:10) = 'Phases.'//out
          FNAMEPHENOLS(1:11) = 'Phenols.'//out
          FNAMEPHENOLM(1:11) = 'Phenolm.'//out
          CALL GETLUN (FNAMEPHASES,fnumpha)
          CALL GETLUN (FNAMEPHENOLS,fnumphes)
          CALL GETLUN (FNAMEPHENOLM,fnumphem)

          ! LAH March 2010 Metadata taken out. Check later if needed
          ! METADATA FILES
          !CALL GETLUN ('META',FNUMMETA)

          ! ERROR FILES
          FNAMEERA(1:12) = 'Plantera.'//out
          FNAMEERS(1:12) = 'Planters.'//out
          FNAMEERT(1:12) = 'Plantert.'//out
          CALL GETLUN (FNAMEERT,fnumert)
          CALL GETLUN (FNAMEERA,fnumera)
          CALL GETLUN (FNAMEERS,fnumers)

!-----------------------------------------------------------------------
!         Open and write main headers to output files
!-----------------------------------------------------------------------

          ! WARNING AND WORK FILES
          INQUIRE (FILE = 'WORK.OUT',OPENED = FOPEN)
          IF (.NOT.FOPEN) THEN
            IF (RUN.EQ.1) THEN
              OPEN (UNIT = FNUMWRK,FILE = 'WORK.OUT')
              WRITE(fnumwrk,*) 'CSCRP  Cropsim Crop Module '
            ELSE
              OPEN (UNIT = FNUMWRK,FILE = 'WORK.OUT',POSITION='APPEND')
              WRITE(fnumwrk,*) ' '
              WRITE(fnumwrk,*) 'CSCRP  Cropsim Crop Module '
              IF (IDETL.EQ.'0'.OR.IDETL.EQ.'Y'.OR.IDETL.EQ.'N') THEN
                CLOSE (FNUMWRK)
                OPEN (UNIT = FNUMWRK,FILE = 'WORK.OUT')
                WRITE(fnumwrk,*) ' '
                WRITE(fnumwrk,*) 'CSCRP  Cropsim Crop Module '
              ENDIF  
            ENDIF
          ELSE          
            IF (IDETL.EQ.'0'.OR.IDETL.EQ.'Y'.OR.IDETL.EQ.'N') THEN
              CLOSE (FNUMWRK)
              OPEN (UNIT = FNUMWRK,FILE = 'WORK.OUT')
              WRITE(fnumwrk,*) ' '
              WRITE(fnumwrk,*) 'CSCRP  Cropsim Crop Module '
              CALL Getlun('READS.OUT',fnumrea)
              ! Close and re-open Reads file
              CLOSE (FNUMREA)
              OPEN (UNIT = FNUMREA,FILE = 'READS.OUT')
              WRITE(fnumrea,*)' '
              WRITE(fnumrea,*)
     &        ' File closed and re-opened to avoid generating huge file'
            ELSE  
              WRITE(fnumwrk,*) ' '
              WRITE(fnumwrk,*) 'CSCRP  Cropsim Crop Module '
            ENDIF
          ENDIF  

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
              OPEN (UNIT = NOUTPGN, FILE = OUTPN)
              WRITE (NOUTPGN,'(A35)')
     &        '$PLANT NITROGEN ASPECTS OUTPUT FILE'
              CLOSE (NOUTPGN)
            ELSE  
              INQUIRE (FILE = NOUTPGN,EXIST = FEXIST)
              IF (FEXIST) THEN
                OPEN (UNIT = NOUTPGN, FILE = OUTPN, STATUS='UNKNOWN',
     &          POSITION = 'APPEND')
                CLOSE (UNIT=NOUTPGN, STATUS = 'DELETE')
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
             WRITE (FNUMPHA,'(A17)') '$PHASE_CONDITIONS'
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
             OPEN (UNIT = FNUMERS,FILE = FNAMEERS,STATUS = 'UNKNOWN')
             WRITE (FNUMERS,'(A15,A33)') '$ERRORS_SUMMARY',
     &        '  RMSE expressed as % of measured'
             WRITE (FNUMERS,*) ' '
             CLOSE(FNUMERS)
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
!       Initialize both state and rate variables                   
!-----------------------------------------------------------------------

        adap = -99
        adat = -99
        adatend = -99
        adayfr = -99
        adoy = -99
        aflf = 0.0
        amtnit = 0.0
        andem = 0.0
        awnai = 0.0
        caid = 0.0
        canht = 0.0
        canhtg = 0.0
        carboadj = 0.0
        carbobeg = 0.0
        carbobegi = 0.0
        carbobegp = 0.0
        carbobegr = 0.0
        carboc = 0.0
        carboend = 0.0
        carbogf = 0.0
        carbolim = 0
        carbor = 0.0
        carborrs = 0.0
        carbot = 0.0
        ccountv = 0
        cdays  = 0
        cflfail = 'n'
        cflfln = 'n'
        cflharmsg = 'n'
        cflsdrsmsg = 'n'
        chrswt = 0.0
        chwad = 0.0
        chwt = 0.0
        cnaa = -99.0
        cnaam = -99.0
        cnad = 0.0
        cnadstg = 0.0
        cnam = -99.0
        cnamm = -99.0
        cnpca = 0.0
        co2cc = 0.0
        co2fp = 1.0
        co2intppm = 0.0
        co2intppmp = 0.0
        co2max = -99.0
        co2pav = -99.0
        co2pc = 0.0
        cumdu = 0.0
        cumdulag = 0.0
        cumdulf = 0.0
        cumdulin = 0.0
        cumdus = 0.0
        cumtt = 0.0
        cumvd = 0.0
        cwaa = -99.0
        cwaam = -99.0
        cwad = 0.0
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
        deadn = 0.0
        deadnad = 0.0
        deadwad = 0.0
        deadwam = 0.0
        deadwt = 0.0
        deadwtm = 0.0
        deadwtr = 0.0
        deadwts = 0.0
        deadwtsge = 0.0
        dewdur = -99.0
        df = 1.0
        dfout = 1.0
        dglf = 0
        drainc = 0.0
        dstage = 0.0
        du = 0.0
        dulag = 0.0
        dulf = 0.0
        dulfnext = 0.0
        dulin = 0.0
        duneed = 0.0
        dwrphc = 0.0
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
        eoebudcrp2 = 0.0
        eompen = 0.0
        eompenc = 0.0
        eop330 = 0.0
        eopco2 = 0.0
        eopen = 0.0
        eopenc = 0.0
        eopt = 0.0
        eoptc = 0.0
        eosoil = 0.0
        epcc   = 0.0
        epsratio = 0.0
        established = 'n'
        etcc   = 0.0
        ewad = 0.0
        eyeardoy = -99
        fappline = ' '
        fappnum = 0
        fernitprev = 0.0
        fldap = 0
        fln = 0.0
        fsdu = -99.0
        g2a = -99.0
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
        gfdat = -99
        gfdur = -99
        gnad = 0.0
        gnoad = 0.0
        gnoam = 0.0
        gnopd = 0.0
        gnpcm = 0.0
        gplasenf = 0.0
        grainanc = 0.0
        grainn = 0.0
        grainndem = 0.0
        grainngl = 0.0
        grainngr = 0.0
        grainngrs = 0.0
        grainngs = 0.0
        grainngu = 0.0
        grainntmp = 0.0
        groch = 0.0
        grogr = 0.0
        grogrp = 0.0
        grogrpa = 0.0
        grolf = 0.0
        grors = 0.0
        grorsgr = 0.0
        grorspm = 0.0
        grosr = 0.0
        grost = 0.0
        grwt = 0.0
        grwtm = 0.0
        grwtsge = 0.0
        grwttmp = 0.0
        gstage = 0.0
        gwad = 0.0
        gwam = 0.0
        gwph = 0.0
        gwphc = 0.0
        gwud = 0.0
        gwudelag = 0.0
        gwum = -99.0
        gyeardoy = -99
        hardays = 0.0
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
        hnumgm = -99.0
        hnumgmm = -99.0
        hnumgmm = -99.0
        hstage = 0.0
        hwam = -99.0 
        hwamm = -99.0
        hwum = -99.0
        hwummchar = ' -99.0'
        idetgnum = 0
        irramtc = 0.0
        lafswitch = -99.0
        lage = 0.0
        lagep = 0.0
        lai = 0.0
        lail = 0.0
        laila = 0.0
        laistg = 0.0
        laix = 0.0
        laixm = -99.0
        lanc = 0.0
        lap = 0.0
        lapotchg = 0
        lapp = 0.0
        laps = 0.0
        latl = 0.0
        lcnum = 0
        lcoa = 0.0
        lcoas = 0.0
        leafn = 0.0
        lfwaa = -99.0
        lfwaam = -99.0
        lfwt = 0.0
        lfwtm = 0.0
        lfwtsge = 0.0
        llnad = 0.0
        llrswad = 0.0
        llrswt = 0.0
        llwad = 0.0
        lncr = 0.0
        lncx = 0.0
        lndem = 0.0
        lngu = 0.0
        lnum = 0.0
        lnumend = 0.0
        lnumg = 0.0
        lnumprev = 0.0
        lnumsg = 0
        lnumsm = -99.0
        lnumsmm = -99.0
        lnumstg = 0.0
        lnumts = 0.0
        lnuse = 0.0
        lseed = -99
        lshai = 0.0
        lshrswad = 0.0
        lshrswt = 0.0
        lshwad = 0.0
        lsndem = 0.0
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
        nflf = 0.0
        nflfp = 0.0
        nfp = 1.0
        nfpcav = 1.0
        nfpcc = 0.0
        nfppav = 1.0
        nfppc = 0.0
        nft = 1.0
        nlimit = 0
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
        parif = 0.0
        parif1 = 0.0
        parip = -99.0
        paripa = -99.0
        pariue = 0.0
        parmjc = 0.0
        parmjic = 0.0
        paru = 0.0
        pdadj = -99.0
        pdays  = 0
        phintout = 0.0
        photqr = 0.0
        pla = 0.0
        plagt = 0.0
        plas = 0.0
        plasc = 0.0
        plascsum = 0.0
        plasfs = 0.0
        plasi = 0.0
        plasl = 0.0
        plasp = 0.0
        plaspm = 0.0
        plass = 0.0
        plast = 0.0
        plax = 0.0
        pltpop = 0.0
        pltloss = 0.0
        plwtgs = 0.0
        plyear = -99
        plyeardoy = 9999999
        psdap = -99
        psdapm = -99
        psdat = -99
        psdayfr = 0.0
        ptf = 0.0
        rainc = 0.0
        rainca = 0.0
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
        respgf = 0.0
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
        rsadj = 0.0
        rscd = 0.0
        rscm = 0.0
        rsco2 = 0.0
        rscx = 0.0
        rsfp = 1.0
        rsn = 0.0
        rsnad = 0.0
        rsnuse = 0.0
        rstage = 0.0
        rstagefs = -99.0
        rstagep = 0.0
        rswaa = -99.0
        rswaam = -99.0
        rswad = 0.0
        rswadpm = 0.0
        rswam = -99.0
        rswamm = -99.0
        rswphc = 0.0
        rswt = 0.0
        rswtm = 0.0
        rswtpm = 0.0
        rswtsge = 0.0
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
        sanc = 0.0
        sancout = 0.0
        sdnad = 0.0
        sdnc = 0.0
        sdwad = 0.0
        sdwam = -99.0
        !seednl = 0.0
        !seednr = 0.0
        seeduse = 0.0
        seeduser = 0.0
        seeduset = 0.0
        sencags = 0.0
        sencalg = 0.0
        sencas = 0.0
        sencl = 0.0
        sencs = 0.0
        sengf = 0.0
        senla = 0.0
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
        sennstg = 0.0
        sennstgrs = 0.0
        senrtg = 0.0
        senrtggf = 0.0
        senstg = 0.0
        senstgrs = 0.0
        sentopg = 0.0
        sentopggf = 0.0
        senwacm = -99.0
        senwacmm = -99.0
        senwags = 0.0
        senwal = 0.0
        senwalg = 0.0
        senwas = 0.0
        senwl = 0.0
        senws = 0.0
        shrtd = 0.0
        shrtm = 0.0
        sla = -99.0
        snad = 0.0
        sncr = 0.0
        sngu = 0.0
        sngul = 0.0
        snow = 0.0
        sno3profile = 0.0
        sno3profile = 0.0
        sno3rootzone = 0.0
        snh4rootzone = 0.0
        snuse = 0.0
        spnumhc = 0.0
        spnumhcm = -99.0
        srad20a = -99.0
        sradc = 0.0
        sradcav  = -99.0
        sradcc = 0.0
        sradd = 0.0
        sradpav  = -99.0
        sradpc = 0.0
        sradprev = 0.0
        srnam = -99.0
        srnc = 0.0
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
        ssdap = -99
        ssdat = -99
        stai = 0.0
        staig = 0.0
        stais = 0.0
        stemn = 0.0
        stemngl = 0.0
        stgedat = 0
        stgyeardoy = 9999999
        strswt = 0.0
        ststage = 0.0
        stvwt = 0.0
        stwaa = -99.0
        stwaam = -99.0
        stwaam = -99.0
        stwad = 0.0
        stwt = 0.0
        stwtm = 0.0
        stwtsge = 0.0
        swphc = 0.0
        tcan = 0.0
        tdifav = -99.0
        tdifnum = 0
        tdifsum = 0.0
        tfd = 0.0
        tfg = 1.0
        tflf = 0.0
        tfp = 1.0
        tilbirthl = 0.0
        tildat = 0.0
        tkill = -99.0 
        tla = 0.0
        tlas = 0.0
        tlchc = 0.0
        tlimit = 0
        tmaxcav  = -99.0
        tmaxcc = 0.0
        tmaxm = -99.0
        tmaxpav  = -99.0
        tmaxpc = 0.0
        tmaxsum = 0.0
        tmaxx = -99.0
        tmean = (tmax+tmin)/2.0
        tmean20a = -99.0
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
        tnum = 0.0
        tnumd = 0.0
        tnumg = 0.0
        tnumad = 0.0
        tnumam = -99.0
        tnumamm = -99.0
        tnuml = 1.0
        tnumloss = 0.0
        tnumprev = 0.0
        tnumx = 0.0
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
        ttcum = 0.0
        ttd = 0.0
        twad = 0.0
        uh2o = 0.0
        unh4 = 0.0
        uno3 = 0.0
        vanc = 0.0
        vcnc = 0.0
        vf = -99.0
        vmnc = 0.0
        vnad = 0.0
        vnam = -99.0
        vnamm = -99.0
        vnpcm = -99.0
        vnpcmm = -99.0
        vpdfp = 1.0
        vrnstage = 0.0
        vwad = 0.0
        vwam = -99.0
        vwamm = -99.0
        wfg = 1.0
        wfgcc = 0.0
        wfgpav = 1.0
        wfgpc = 0.0
        wflf = 0.0
        wflfp = 0.0
        wfp = 1.0
        wfpcav = 1.0
        wfpcc = 0.0
        wfppc = 0.0
        wft = 1.0
        wsdays = 0
        wupr = 1.0
        zstage = 0.0
        
!-----------------------------------------------------------------------
!       Read experiment information from Dssat input or X- file
!-----------------------------------------------------------------------

        ! Methods
        CALL XREADC(FILEIO,TN,RN,SN,ON,CN,'PHOTO',mephs)
        CALL XREADC(FILEIO,TN,RN,SN,ON,CN,'MEWNU',mewnu)
        CALL XREADC(FILEIO,TN,RN,SN,ON,CN,'METHODS',meexp)

        ! Experiment, treatment, and run control names
        EXCODEPREV = EXCODE
        CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'ENAME',ename)
        CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'EXPER',excode)
        CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'TNAME',tname)
        CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'SNAME',runname)

        ! Planting date information
        CALL XREADC(FILEIO,TN,RN,SN,ON,CN,'PLANT',iplti)
        IF(IPLTI.EQ.'A'.OR.IPLTI.EQ.'a')THEN
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
          hyeardoy(i) = CSYEARDOY(hyrdoy(i))
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
        CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'ICWT',icwt)

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
        ENDIF

!-----------------------------------------------------------------------
!       Correct case and dates
!-----------------------------------------------------------------------

        CALL CSUCASE (CROP)
        CALL CSUCASE (EXCODE)

        HLAST = CSYEARDOY(hlast)
        HFIRST = CSYEARDOY(hfirst)
        PWDINF = CSYEARDOY(pwdinf)
        PWDINL = CSYEARDOY(pwdinl)
        DO L = 1,DINX
          DIDAT(L) = CSYEARDOY(DIDAT(L))
        ENDDO
        DO L = 1,DCNX
          DCDAT(L) = CSYEARDOY(DCDAT(L))
        ENDDO

        CALL CSYR_DOY(PWDINF,PWYEARF,PWDOYF)
        CALL CSYR_DOY(PWDINL,PWYEARL,PWDOYL)
        CALL CSYR_DOY(HFIRST,HYEARF,HDOYF)
        CALL CSYR_DOY(HLAST,HYEARL,HDOYL)
        CALL CSYR_DOY(PDATE,PLYEARTMP,PLDAY)
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
        IF (spnumhfac.LE.0.0) spnumhfac = 0.1

!-----------------------------------------------------------------------
!       Set planting/harvesting dates (Will change if runs repeated)
!-----------------------------------------------------------------------

        ! CHP 5/4/09 - for DSSAT runs, always set PLYEAR = YEAR
        ! CHP 09/28/2009 account for planting date >> simulation date.
        IF (FILEIOT(1:2).EQ.'DS' .AND. YEAR > PLYEAR) THEN
          PLYEAR = YEAR
          PLYEARTMP = YEAR
        ENDIF

        ! Check final harvest date for seasonal runs        
        CALL CSYR_DOY(YEARDOYHARF,HYEAR,HDAY)
        PLTOHARYR = HYEAR - PLYEARREAD
        ! Upgrade harvest date for seasonal and sequential runs
        yeardoyharf = (plyear+pltoharyr)*1000 +hday

        IF (IPLTI.NE.'A') THEN
          IF (PLDAY.GE.DOY) THEN
            PLYEARDOYT = PLYEARTMP*1000 + PLDAY
          ELSEIF (PLDAY.LT.DOY) THEN
            PLYEARDOYT = (YEAR+1)*1000 + PLDAY
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
          ! Following added Sept 2008 for running under VB environment.
          WRITE(fnumwrk,*) ' '
          WRITE(fnumwrk,*) 'Cultivar file not found!     '
          WRITE(fnumwrk,*) 'File sought was:          '  
          WRITE(fnumwrk,*) Cudirfle(1:78)
          WRITE(fnumwrk,*) 'Will search in the working directory for:'
          CUDIRFLE = CUFILE
          WRITE(fnumwrk,*)  Cudirfle(1:78)
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

        INQUIRE (FILE = ECDIRFLE,EXIST = FFLAG)
        IF (.NOT.(FFLAG)) THEN
          ! Following added Sept 2008 for running under VB environment.
          WRITE(fnumwrk,*) ' '
          WRITE(fnumwrk,*) 'Ecotype file not found!     '
          WRITE(fnumwrk,*) 'File sought was:          '  
          WRITE(fnumwrk,*) Ecdirfle
          WRITE(fnumwrk,*) 'Will search in the working directory for:'
          ECDIRFLE = ECFILE
          WRITE(fnumwrk,*)  Ecdirfle
          INQUIRE (FILE = ECDIRFLE,EXIST = FFLAG)
          IF (.NOT.(FFLAG)) THEN
            OPEN (UNIT = FNUMERR,FILE = 'ERROR.OUT')
            WRITE(fnumerr,*) ' '
            WRITE(fnumerr,*) 'Ecotype file not found!     '
            WRITE(fnumerr,*) 'File sought was:          '
            WRITE(fnumerr,*) Ecdirfle
            WRITE(fnumerr,*) 'Please check'
            WRITE(*,*) ' Ecotype file not found!     '
            WRITE(*,*) 'File sought was:          '
            WRITE(*,*) Ecdirfle
            WRITE(*,*) ' Program will have to stop'
            CLOSE (fnumerr)
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
!       Read cultivar information
!-----------------------------------------------------------------------

        ! Ecotype coefficients re-set
        canhts = -99
        awns = -99
        rsca = -99
        ti1lf = -99
        tifac = -99
        tilpe = -99
        tilds = -99
        tilde = -99
        grns = -99
        srns = -99
        srprs = -99
        srprs = -99
        tkfh = -99
        lsens = -99
        lsene = -99
        ssphase = -99
        lseni = -99
        pdsri = -99
        phintl = -99
        phintf = -99
        snofx = -99
        gwtcf = -99
        gnowf = -99
        gnonf = -99
        gwtat = -99
        gwtaa = -99
        parue = -99
        paru2 = -99
        la1s = -99
        laxs = -99
        laws = -99
        lafv = -99
        lafr = -99
        rdgs = -99
        tildf = -99
        tilsf = -99
        nfgu = -99
        nfgl = -99

        ! Species coefficients re-set
        pd = -99
        lsene = -99 
        pdsri = -99 
        pdsri = -99 
        tilde = -99 
        tilds = -99 
        tilpe = -99 
        llifa = -99 
        phintl = -99 
        phintf = -99 
        srfr = -99 
        wfnuu = -99 
        wfnul = -99 
        tildf = -99 
        rdgs = -99 
        laxs = -99 
        rlfnu = -99 
        rlwr = -99 
        nfgl = -99 
        nfgu = -99 
        nfpu = -99 
        nfpl = -99 
        ncnu = -99 
        kcan = -99 
        lafr = -99 
        lafv = -99 
        lawcf = -99 
        dfpe = -99 
        ppexp = -99 
        tilsf = -99 
        gwtaa = -99 
        gwtat = -99 
        gwtcf = -99 
        gnowf = -99 
        gnonf = -99 

        IF (FILEIOT(1:2).EQ.'DS') THEN
          CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'ECO#',econo)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'VREQ',vreq)
          IF (VREQ.LT.0.0)
     &     CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'VREQX',vreq)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'VBASE',vbase)
          IF (VBASE.LT.0.0)
     &     CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'VREQN',vbase)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'VEFF',veff)
          IF (VEFF.LT.0.0)
     &     CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'VEFFX',veff)
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
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'G#WTS',gnows)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'GWTS',gwts)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'SHWTS',g3)
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
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P1L',pdl(1))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P2L',pdl(2))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P3L',pdl(3))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P4L',pdl(4))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P5L',pdl(5))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P6L',pdl(6))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P7L',pdl(7))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P8L',pdl(8))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P9L',pdl(9))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LLIFA',llifa)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'STFR',swfrs)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'SR#WT',srnow)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'SRFR',srfr)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LAXS',laxs)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LAXND',laxno)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LAFS',lafs)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LAF#',lafno)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LAWS',laws)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'WFNUU',wfnuu)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'WFNUL',wfnul)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NCNU',ncnu)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'RLFNU',rlfnu)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFPU',nfpu)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFPL',nfpl)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFGU',nfgu)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFGL',nfgl)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'RDGS',rdgs)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'RLWR',rlwr)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PARUE',parue)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PARU2',paru2)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'TDFAC',tildf)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'TDSF',tilsf)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'SHWAS',gwtat)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'GWWF',gwtaa)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'GW#F',gwtcf)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'G#WSF',gnowf)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'G#NSF',gnonf)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LA1S',la1s)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LAFV',lafv)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LAFR',lafr)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PHL2',phintl(2))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PHF3',phintf(3))
        ELSE
          CALL CUREADC (CUDIRFLE,VARNO,'ECO#',econo)
          CALL CUREADR (CUDIRFLE,VARNO,'VREQ',vreq)
          IF (VREQ.LT.0.0)CALL CUREADR (CUDIRFLE,VARNO,'VREQX',vreq)
          CALL CUREADR (CUDIRFLE,VARNO,'VBASE',vbase)
          IF (VBASE.LT.0.0)CALL CUREADR (CUDIRFLE,VARNO,'VREQN',vbase)
          CALL CUREADR (CUDIRFLE,VARNO,'VEFF',veff)
          IF (VEFF.LT.0.0)CALL CUREADR (CUDIRFLE,VARNO,'VEFFX',veff)
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
          CALL CUREADR (CUDIRFLE,VARNO,'P1L',pdl(1))
          CALL CUREADR (CUDIRFLE,VARNO,'P2L',pdl(2))
          CALL CUREADR (CUDIRFLE,VARNO,'P3L',pdl(3))
          CALL CUREADR (CUDIRFLE,VARNO,'P4L',pdl(4))
          CALL CUREADR (CUDIRFLE,VARNO,'P5L',pdl(5))
          CALL CUREADR (CUDIRFLE,VARNO,'P6L',pdl(6))
          CALL CUREADR (CUDIRFLE,VARNO,'P7L',pdl(7))
          CALL CUREADR (CUDIRFLE,VARNO,'P8L',pdl(8))
          CALL CUREADR (CUDIRFLE,VARNO,'P9L',pdl(9))
          CALL CUREADR (CUDIRFLE,VARNO,'G#WTS',gnows)
          CALL CUREADR (CUDIRFLE,VARNO,'GWTS',gwts)
          CALL CUREADR (CUDIRFLE,VARNO,'SHWTS',g3)
          CALL CUREADR (CUDIRFLE,VARNO,'PHINT',phints)
          CALL CUREADR (CUDIRFLE,VARNO,'LLIFA',llifa)
          CALL CUREADR (CUDIRFLE,VARNO,'STFR',swfrs)
          CALL CUREADR (CUDIRFLE,VARNO,'SR#WT',srnow)
          CALL CUREADR (CUDIRFLE,VARNO,'SRFR',srfr)
          CALL CUREADR (CUDIRFLE,VARNO,'LAXS',laxs)
          CALL CUREADR (CUDIRFLE,VARNO,'LAXND',laxno)
          CALL CUREADR (CUDIRFLE,VARNO,'LAFS',lafs)
          CALL CUREADR (CUDIRFLE,VARNO,'LAF#',lafno)
          CALL CUREADR (CUDIRFLE,VARNO,'LAWS',laws)
          CALL CUREADR (CUDIRFLE,VARNO,'WFNUU',wfnuu)
          CALL CUREADR (CUDIRFLE,VARNO,'WFNUL',wfnul)
          CALL CUREADR (CUDIRFLE,VARNO,'NCNU',ncnu)
          CALL CUREADR (CUDIRFLE,VARNO,'RLFNU',rlfnu)
          CALL CUREADR (CUDIRFLE,VARNO,'NFPU',nfpu)
          CALL CUREADR (CUDIRFLE,VARNO,'NFPL',nfpl)
          CALL CUREADR (CUDIRFLE,VARNO,'NFGU',nfgu)
          CALL CUREADR (CUDIRFLE,VARNO,'NFGL',nfgl)
          CALL CUREADR (CUDIRFLE,VARNO,'RDGS',rdgs)
          CALL CUREADR (CUDIRFLE,VARNO,'RLWR',rlwr)
          CALL CUREADR (CUDIRFLE,VARNO,'PARUE',parue)
          CALL CUREADR (CUDIRFLE,VARNO,'PARU2',paru2)
          CALL CUREADR (CUDIRFLE,VARNO,'TDFAC',tildf)
          CALL CUREADR (CUDIRFLE,VARNO,'TDSF',tilsf)
          CALL CUREADR (CUDIRFLE,VARNO,'SHWAS',gwtat)
          CALL CUREADR (CUDIRFLE,VARNO,'GWWF',gwtaa)
          CALL CUREADR (CUDIRFLE,VARNO,'GW#F',gwtcf)
          CALL CUREADR (CUDIRFLE,VARNO,'G#WSF',gnowf)
          CALL CUREADR (CUDIRFLE,VARNO,'G#NSF',gnonf)
          CALL CUREADR (CUDIRFLE,VARNO,'LA1S',la1s)
          CALL CUREADR (CUDIRFLE,VARNO,'LAFV',lafv)
          CALL CUREADR (CUDIRFLE,VARNO,'LAFR',lafr)
          CALL CUREADR (CUDIRFLE,VARNO,'PHL2',phintl(2))
          CALL CUREADR (CUDIRFLE,VARNO,'PHF3',phintf(3))
        ENDIF     ! End Cultivar reads

!-----------------------------------------------------------------------
!       Read ecotype information
!-----------------------------------------------------------------------

        CALL ECREADR (ECDIRFLE,ECONO,'HTSTD',canhts)
        CALL ECREADR (ECDIRFLE,ECONO,'AWNS',awns)
        CALL ECREADR (ECDIRFLE,ECONO,'RS%A',rsca)
        CALL ECREADR (ECDIRFLE,ECONO,'TIL#S',ti1lf)
        IF (TI1LF.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'TIPHS',ti1lf)
        CALL ECREADR (ECDIRFLE,ECONO,'TIFAC',tifac)
        CALL ECREADR (ECDIRFLE,ECONO,'TIPHE',tilpe)
        CALL ECREADR (ECDIRFLE,ECONO,'TDPHS',tilds)
        CALL ECREADR (ECDIRFLE,ECONO,'TDPHE',tilde)
        CALL ECREADR (ECDIRFLE,ECONO,'GN%S',grns)
        CALL ECREADR (ECDIRFLE,ECONO,'SRN%S',srns)
        IF (SRNS.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'SRP%S',srprs)
        IF (SRPRS.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'SRPRS',srprs)
        CALL ECREADR (ECDIRFLE,ECONO,'TKFH',tkfh)
        CALL ECREADR (ECDIRFLE,ECONO,'LSPHS',lsens)
        CALL ECREADR (ECDIRFLE,ECONO,'LSPHE',lsene)
        CALL ECREADR (ECDIRFLE,ECONO,'SSPHS',ssphase(1))
        CALL ECREADR (ECDIRFLE,ECONO,'SSPHE',ssphase(2))
        CALL ECREADR (ECDIRFLE,ECONO,'LSENI',lseni)
        CALL ECREADR (ECDIRFLE,ECONO,'PSRI',pdsri)
        IF (PHINTL(1).LE.0) 
     &   CALL ECREADR(ECDIRFLE,ECONO,'PHL1',phintl(1))
        IF (PHINTL(2).LE.0) 
     &   CALL ECREADR(ECDIRFLE,ECONO,'PHL2',phintl(2))
        IF (PHINTF(2).LE.0)
     &   CALL ECREADR(ECDIRFLE,ECONO,'PHF2',phintf(2))
        IF (PHINTF(3).LE.0)
     &   CALL ECREADR(ECDIRFLE,ECONO,'PHF3',phintf(3))
        CALL ECREADR (ECDIRFLE,ECONO,'S#FX',snofx)
        IF (SNOFX.LE.0.0)
     &   CALL ECREADR (ECDIRFLE,ECONO,'SNOFX',snofx)
        ! LAH Following set up to allow for change in stem fraction
        ! Currently not used ... just one stem fraction (STFR)
        CALL ECREADR (ECDIRFLE,ECONO,'SWFRX',swfrx)
        CALL ECREADR (ECDIRFLE,ECONO,'SWFRN',swfrn)
        CALL ECREADR (ECDIRFLE,ECONO,'SWFNL',swfrnl)
        CALL ECREADR (ECDIRFLE,ECONO,'SWFXL',swfrxl)
        CALL ECREADR (ECDIRFLE,ECONO,'LAWCF',lawcf)
        CALL ECREADR (ECDIRFLE,ECONO,'KCAN',kcan)
        ! Following may have been (temporarily) in the CUL file
        ! Grains
        IF (GWTCF.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'GW#F',gwtcf)
        IF (GNOWF.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'G#WSF',gnowf)
        IF (GNONF.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'G#NSF',gnonf)
        IF (GWTAT.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'SHWAS',gwtat)
        IF (GWTAA.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'GWWF',gwtaa)
        ! Nitrogen uptake
        IF (WFNUU.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'WFNUU',wfnuu)
        IF (WFNUL.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'WFNUL',wfnul)
        IF (NCNU.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'NCNU',ncnu)
        IF (RLFNU.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'RLFNU',rlfnu)
        ! Radiation use efficiency
        IF (PARUE.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'PARUE',parue)
        IF (PARU2.LT.-89.0) CALL ECREADR (ECDIRFLE,ECONO,'PARU2',paru2)
        ! Leaf area
        IF (LA1S.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'LA1S',la1s)
        IF (LAXS.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'LAXS',laxs)
        IF (LAWS.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'LAWS',laws)
        IF (LAFV.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'LAFV',lafv)
        IF (LAFR.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'LAFR',lafr)
        ! Roots
        IF (RDGS.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'RDGS',rdgs)
        ! Tillers
        IF (TILDF.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'TDFAC',tildf)
        IF (TILSF.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'TDSF',tilsf)
        ! Reduction factors
        IF (NFGU.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'NFGU',nfgu)
        IF (NFGL.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'NFGL',nfgl)
        
!-----------------------------------------------------------------------
!       Read species information
!-----------------------------------------------------------------------

        CALL SPREADR (SPDIRFLE,'CHFR' ,chfr)
        CALL SPREADR (SPDIRFLE,'CO2CC',co2compc)
        CALL SPREADR (SPDIRFLE,'CO2EX',co2ex)
        CALL SPREADR (SPDIRFLE,'GLIG%',gligp)
        CALL SPREADR (SPDIRFLE,'GN%MN',grnmn)
        CALL SPREADR (SPDIRFLE,'GN%MX',grnmx)
        CALL SPREADR (SPDIRFLE,'GWLAG',gwlfr)
        CALL SPREADR (SPDIRFLE,'GWLIN',gwefr)
        CALL SPREADR (SPDIRFLE,'HDUR' ,hdur)
        CALL SPREADR (SPDIRFLE,'HLOSF',hlossfr)
        CALL SPREADR (SPDIRFLE,'HLOST',hlosstemp)
        CALL SPREADR (SPDIRFLE,'HMPC', hmpc)
        CALL SPREADR (SPDIRFLE,'LAFST',lafst)
        CALL SPREADR (SPDIRFLE,'LAWFF',lawff)
        CALL SPREADR (SPDIRFLE,'LAWTR',lawtr)
        CALL SPREADR (SPDIRFLE,'LAWTS',lawts)
        CALL SPREADR (SPDIRFLE,'LAWWR',lawwr)
        CALL SPREADR (SPDIRFLE,'LLIFG',LLIFG)
        CALL SPREADR (SPDIRFLE,'LLIFS',llifs)
        CALL SPREADR (SPDIRFLE,'LLIG%',lligp)
        CALL SPREADR (SPDIRFLE,'LLOSA',llosa)
        CALL SPREADR (SPDIRFLE,'LWLOS',lsewf)
        CALL SPREADR (SPDIRFLE,'NFSU'  ,nfs)
        CALL SPREADR (SPDIRFLE,'NFSF' ,nfsf)
        CALL SPREADR (SPDIRFLE,'NFTL ',nftl)
        CALL SPREADR (SPDIRFLE,'NFTU ',nftu)
        CALL SPREADR (SPDIRFLE,'LSHAR',lsawr)
        CALL SPREADR (SPDIRFLE,'LSHAW',lsawv)
        CALL SPREADR (SPDIRFLE,'LSHFR',lshfr)
        CALL SPREADR (SPDIRFLE,'NCRG',ncrg)
        CALL SPREADR (SPDIRFLE,'NTUPF',ntupf)
        CALL SPREADR (SPDIRFLE,'NPTFL',nptfl)
        CALL SPREADR (SPDIRFLE,'PARIX',parix)
        CALL SPREADR (SPDIRFLE,'PARFC',parfc)
        CALL SPREADR (SPDIRFLE,'PEMRG',pecm)
        CALL SPREADR (SPDIRFLE,'PGERM',pgerm)
        CALL SPREADR (SPDIRFLE,'PDMH' ,pdmtohar)
        CALL SPREADR (SPDIRFLE,'PHSV' ,phsv)
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
        !CALL SPREADR (SPDIRFLE,'RNUMX',rnumx) ! Taken out of spp file
        CALL SPREADR (SPDIRFLE,'RRESP',rresp)
        CALL SPREADR (SPDIRFLE,'RS%LX',rsclx)
        CALL SPREADR (SPDIRFLE,'RS%S' ,rscs)
        CALL SPREADR (SPDIRFLE,'RSEN%' ,rsen)
        CALL SPREADR (SPDIRFLE,'RSFPL',rsfpl)
        CALL SPREADR (SPDIRFLE,'RSFPU',rsfpu)
        CALL SPREADR (SPDIRFLE,'RSUSE',rsuse)
        CALL SPREADR (SPDIRFLE,'RTUFR',rtufr)
        CALL SPREADR (SPDIRFLE,'RUESG',ruestg)
        CALL SPREADR (SPDIRFLE,'RWUMX',rwumx)
        CALL SPREADR (SPDIRFLE,'RWUPM',rwupm)
        CALL SPREADR (SPDIRFLE,'SAWS' ,saws)
        CALL SPREADR (SPDIRFLE,'SDDUR',sddur)
        CALL SPREADR (SPDIRFLE,'SDN%',sdnpci)
        CALL SPREADR (SPDIRFLE,'SDRS%',sdrsf)
        CALL SPREADR (SPDIRFLE,'SDWT' ,sdsz)
        CALL SPREADR (SPDIRFLE,'SLIG%',sligp)
        CALL SPREADR (SPDIRFLE,'TGR02',tgr(2))
        CALL SPREADR (SPDIRFLE,'TGR20',tgr(20))
        CALL SPREADR (SPDIRFLE,'TILIP',tilip)
        CALL SPREADR (SPDIRFLE,'TIL#X',tinox)
        CALL SPREADR (SPDIRFLE,'TKLFD',tklfd)
        CALL SPREADR (SPDIRFLE,'TKSPN',tkspan)
        CALL SPREADR (SPDIRFLE,'TKTID',tktid)
        CALL SPREADR (SPDIRFLE,'TKUH' ,tkuh)
        CALL SPREADR (SPDIRFLE,'TPAR' ,tpar)
        CALL SPREADR (SPDIRFLE,'TSRAD',tsrad)
        CALL SPREADR (SPDIRFLE,'VEEND',veend)
        CALL SPREADR (SPDIRFLE,'VLOSF',vlossfr)
        CALL SPREADR (SPDIRFLE,'VLOSS',vloss0stg)
        CALL SPREADR (SPDIRFLE,'VLOST',vlosstemp)
        CALL SPREADR (SPDIRFLE,'VPEND',vpend)
        CALL SPREADR (SPDIRFLE,'WFEU' ,wfeu)
        CALL SPREADR (SPDIRFLE,'WFGEU',wfgem)
        CALL SPREADR (SPDIRFLE,'WFGU' ,wfgu)
        CALL SPREADR (SPDIRFLE,'WFGL' ,wfgl)
        CALL SPREADR (SPDIRFLE,'WFPU' ,wfpu)
        CALL SPREADR (SPDIRFLE,'WFPL' ,wfpl)
        CALL SPREADR (SPDIRFLE,'WFRGU',wfrtg)
        CALL SPREADR (SPDIRFLE,'WFSU'  ,wfs)
        CALL SPREADR (SPDIRFLE,'WFSF' ,wfsf)
        CALL SPREADR (SPDIRFLE,'WFTL' ,wftl)
        CALL SPREADR (SPDIRFLE,'WFTU' ,wftu)
        CALL SPREADR (SPDIRFLE,'NLAB%',nlabpc)
        CALL SPREADR (SPDIRFLE,'RTNO3',rtno3)
        CALL SPREADR (SPDIRFLE,'RTNH4',rtnh4)
        ! LAH Following set up to allow for change in stem fraction
        ! Currently not used ... just one stem fraction (STFR)
        IF (SWFRN.LE.0.0) CALL SPREADR (SPDIRFLE,'SWFRN',swfrn)
        IF (SWFRNL.LE.0.0) CALL SPREADR (SPDIRFLE,'SWFRNL',swfrnl)
        IF (SWFRXL.LE.0.0) CALL SPREADR (SPDIRFLE,'SWFXL',swfrxl)
        IF (SWFRX.LE.0.0) CALL SPREADR (SPDIRFLE,'SWFRX',swfrx)
        ! Following may be temporarily in ECO or CUL file
        IF (PD(9).LE.0.0) CALL SPREADR (SPDIRFLE,'P9',pd(9))
        IF (lsene.LE.0.0) CALL SPREADR (SPDIRFLE,'LSENE',lsene)
        IF (PDSRI.LE.0.0) CALL SPREADR (SPDIRFLE,'PSRI',pdsri)
        IF (PDSRI.LE.0.0) CALL SPREADR (SPDIRFLE,'PDSRI',pdsri)
        IF (tilde.LE.0.0) CALL SPREADR (SPDIRFLE,'TDPHE',tilde)
        IF (tilds.LE.0.0) CALL SPREADR (SPDIRFLE,'TDPHS',tilds)
        IF (tilpe.LE.0.0) CALL SPREADR (SPDIRFLE,'TILPE',tilpe)
        IF (LLIFA.LE.0.0) CALL SPREADR (SPDIRFLE,'LLIFA',llifa)

        IF (PHINTL(1).LE.0) CALL SPREADR (SPDIRFLE,'PHL1',phintl(1))
        IF (PHINTF(1).LE.0) CALL SPREADR (SPDIRFLE,'PHF1',phintf(1))
        IF (PHINTL(2).LE.0) CALL SPREADR (SPDIRFLE,'PHL2',phintl(2))
        IF (PHINTF(2).LE.0) CALL SPREADR (SPDIRFLE,'PHF2',phintf(2))
        IF (PHINTF(3).LE.0) CALL SPREADR (SPDIRFLE,'PHF3',phintf(3))

        IF (SRFR.LE.0.0) CALL SPREADR (SPDIRFLE,'SRFR',srfr)
        IF (WFNUU.LE.0.0) CALL SPREADR (SPDIRFLE,'WFNUU',wfnuu)
        IF (WFNUL.LE.0.0) CALL SPREADR (SPDIRFLE,'WFNUL',wfnul)
        IF (TILDF.LE.0.0) CALL SPREADR (SPDIRFLE,'TDFAC',tildf)
        IF (RDGS.LE.0.0) CALL SPREADR (SPDIRFLE,'RDGS',rdgs)
        IF (LAXS.LE.0.0) CALL SPREADR (SPDIRFLE,'LAXS',laxs)
        IF (RLFNU.LE.0.0) CALL SPREADR (SPDIRFLE,'RLFNU',rlfnu)
        IF (RLWR.LE.0.0) CALL SPREADR (SPDIRFLE,'RLWR',rlwr)
        IF (NFGL.LT.0.0) CALL SPREADR (SPDIRFLE,'NFGL',nfgl)
        IF (NFGU.LE.0.0) CALL SPREADR (SPDIRFLE,'NFGU',nfgu)
        IF (NFPU.LE.0.0) CALL SPREADR (SPDIRFLE,'NFPU',nfpu)
        IF (NFPL.LE.0.0) CALL SPREADR (SPDIRFLE,'NFPL',nfpl)
        IF (NCNU.LE.0.0) CALL SPREADR (SPDIRFLE,'NCNU',ncnu)
        IF (KCAN.LE.0.0) CALL SPREADR (SPDIRFLE,'KCAN',kcan)
        IF (LAFR.LE.0.0) CALL SPREADR (SPDIRFLE,'LAFR',lafr)
        IF (LAFV.LE.0.0) CALL SPREADR (SPDIRFLE,'LAFV',lafv)
        IF (LAWCF.LE.0.0) CALL SPREADR (SPDIRFLE,'LAWCF',lawcf)
        IF (DFPE.LT.0.0) CALL SPREADR (SPDIRFLE,'PPFPE',dfpe)
        IF (PPEXP.LT.0.0) CALL SPREADR (SPDIRFLE,'PPEXP',ppexp)
        IF (TILSF.LT.0.0) CALL SPREADR (SPDIRFLE,'TDSF',tilsf)
        ! Grain coefficients
        IF (GWTAA.LT.0.0) CALL SPREADR (SPDIRFLE,'GWWF',gwtaa)
        IF (GWTAT.LT.0.0) CALL SPREADR (SPDIRFLE,'SHWAS',gwtat)
        IF (GWTCF.LT.0.0) CALL SPREADR (SPDIRFLE,'GW#F',gwtcf)
        IF (GNOWF.LT.0.0) CALL SPREADR (SPDIRFLE,'G#WSF',gnowf)
        IF (GNONF.LT.0.0) CALL SPREADR (SPDIRFLE,'G#NSF',gnonf)

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

        DO I = 1,9
          TL10 = TL10FROMI(I) 
          TVTRDV = 'TRDV'//TL10(1:1)
          CALL SPREADRA (SPDIRFLE,TVTRDV,'4',trdvx)
          IF (I.EQ.1) TRDV1 = TRDVX
          ! Temperature responses for each phase put into 2D array
          DO L = 1,4
            IF (trdvx(1).GT.-98.0) THEN
              TRDEV(L,I) = TRDVX(L)
            ELSE
              IF (I.GT.1) THEN
                TRDEV(L,I) = TRDEV(L,I-1)
              ELSE
                OPEN (UNIT = FNUMERR,FILE = 'ERROR.OUT')
                WRITE(fnumerr,*) ' '
                WRITE(fnumerr,*) ' No temp response data for phase 1'
                WRITE(fnumerr,*) ' Please check'
                WRITE(*,*) ' No temperature response data for phase 1'
                WRITE(*,*) ' Program will have to stop'
                CLOSE (fnumerr)
                STOP ' '
              ENDIF
            ENDIF
           IF (I.EQ.2) TRDV2(L) = TRDEV(L,2)
          ENDDO
        ENDDO

        CALL SPREADRA (SPDIRFLE,'TRGFW','4',trgfc)
        CALL SPREADRA (SPDIRFLE,'TRGFN','4',trgfn)
        CALL SPREADRA (SPDIRFLE,'TRLFG','4',trlfg)
        CALL SPREADRA (SPDIRFLE,'TRHAR','4',trcoh)
        CALL SPREADRA (SPDIRFLE,'TRPHS','4',trphs)
        CALL SPREADRA (SPDIRFLE,'TRVRN','4',trvrn)
        IF (diffacr(1).LT.0.0)
     &   CALL SPREADRA (SPDIRFLE,'DIFFR','3',diffacr)

        CALL SPREADCA (SPDIRFLE,'PSNAME','20',psname)
        CALL SPREADCA (SPDIRFLE,'SSNAME','20',ssname)
        CALL SPREADCA (SPDIRFLE,'PSABV','20',psabv)
        CALL SPREADCA (SPDIRFLE,'SSABV','20',ssabv)
        CALL SPREADCA (SPDIRFLE,'PSTYP','20',pstyp)
        CALL SPREADCA (SPDIRFLE,'SSTYP','20',sstyp)
        CALL SPREADRA (SPDIRFLE,'SSTG','20',sstg)

!-----------------------------------------------------------------------
!       Determine 'key' principal and secondary stages,and adjust names
!-----------------------------------------------------------------------

        ! LAH Must introduce type for changing phase duration eg after
        !     terminal spiklet until leaves fully expanded (maize?)
        !     Previously used CFLPDADJ = 'Y','N'

        KEYPSNUM = 0
        KEYSSNUM = 0
        SSNUM = 0
        PSNUM = 0
        TSSTG = -99
        LLSTG = -99
        DO L = 1,PSX
          IF (TVILENT(PSTYP(L)).GT.0) THEN
            IF (PSTYP(L).EQ.'K'.OR.PSTYP(L).EQ.'k'.OR.
     &          PSTYP(L).EQ.'M')THEN
              KEYPSNUM = KEYPSNUM + 1
              KEYPS(KEYPSNUM) = L
            ENDIF
            IF (PSABV(L).EQ.'ADAT') ASTG = L
            IF (PSABV(L).EQ.'ECDAT') ECSTG = L
            IF (PSABV(L).EQ.'HDAT') HSTG = L
            IF (PSABV(L).EQ.'IEDAT') IESTG = L
            IF (PSABV(L).EQ.'LLDAT') LLSTG = L
            IF (PSABV(L).EQ.'MDAT') MSTG = L
            IF (PSABV(L).EQ.'TSDAT') TSSTG = L
            PSNUM = PSNUM + 1
          ENDIF
        ENDDO
        ! IF MSTG not found, use maximum principal stage number
        IF (MSTG.LE.0) THEN
          MSTG = KEYPSNUM
        ENDIF
        ! IF HSTG not found, use maximum principal stage number
        IF (HSTG.LE.0) THEN
          HSTG = PSX
          HSTG = MSTG+1  ! LAH 230311
          pstart(hstg) = 5000   ! Set to very long cycle
        ENDIF
        
        KEYSS = -99
        SSNUM = 0
        DO L = 1,SSX
          IF (TVILENT(SSTYP(L)).GT.0) THEN
            SSNUM = SSNUM + 1
            IF (SSTYP(L).EQ.'K') THEN
              KEYSSNUM = KEYSSNUM + 1
              KEYSS(KEYSSNUM) = L
            ENDIF
          ENDIF
        ENDDO
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
        DO L = 1,SSNUM
          IF (TVILENT(SSABV(L)).GT.3) THEN
            IF (TVILENT(SSABV(L)).EQ.4) THEN
              DO L1 = 5,1,-1
                IF (L1.GT.1) THEN
                  SSABV(L)(L1:L1) = SSABV(L)(L1-1:L1-1)
                ELSE
                  SSABV(L)(L1:L1) = ' '
                ENDIF
              ENDDO
            ENDIF
            SSABVO(L) = SSABV(L)
            ! DAS -> DAP for output
            SSABVO(L)(5:5) = 'P'
          ENDIF
        ENDDO

        ! Set stage numbers
        LAFST = -99.0
        LRETS = -99.0
        GGPHASE = -99.0
        LGPHASE = -99.0
        RUESTG = -99.0
        SGPHASE = -99.0
        SRSTAGE = -99.0
        DO L = 1,SSNUM
          IF (SSABV(L).EQ.'LAFST') LAFST = SSTG(L)
          IF (SSABV(L).EQ.'LGPHE') LGPHASE(2) = SSTG(L)
          IF (SSABV(L).EQ.'LRPHS') LRETS = SSTG(L)
          IF (SSABV(L).EQ.'GGPHS') GGPHASE(1) = SSTG(L)
          IF (SSABV(L).EQ.'GLPHS') GGPHASE(2) = SSTG(L)
          IF (SSABV(L).EQ.'GLPHE') GGPHASE(3) = SSTG(L)
          IF (SSABV(L).EQ.'GGPHE') GGPHASE(4) = SSTG(L)
          IF (SSABV(L).EQ.'RUESG') RUESTG = SSTG(L)
          IF (SSABV(L).EQ.'SGPHS') SGPHASE(1) = SSTG(L)
          IF (SSABV(L).EQ.'SGPHE') SGPHASE(2) = SSTG(L)
          IF (SSABV(L).EQ.'SVPHS') STVSTG = SSTG(L)
          IF (SSABV(L).EQ.'CHPHS') CHPHASE(1) = SSTG(L)
          IF (SSABV(L).EQ.'CHPHE') CHPHASE(2) = SSTG(L)
          IF (SSABV(L).EQ.'SRIN '.OR.SSABV(L).EQ.' SRIN' 
     &     .OR.SSABV(L).EQ.'SRINI'.OR.SSABV(L).EQ.'SRDAT') SRSTAGE = L 
        ENDDO
        
        ! Set stage defaults if necessary
        IF (LAFST.LE.0.0) LAFST = MIN(PSX,MSTG+1)
        
        ! For CSM N uptake routine 
        IF (rtno3.le.0.0) THEN
          RTNO3 = 0.006    ! N uptake/root length (mgN/cm,.006)
          WRITE (fnumwrk,*) ' '
          WRITE (fnumwrk,*) ' Default of 0.006 used for RTNO3'
        ENDIF  
        IF (rtnh4.le.0.0) THEN
          RTNH4 = RTNO3     ! N uptake/root length (mgN/cm,.006)
          WRITE (fnumwrk,*) ' '
          WRITE (fnumwrk,*) ' Default of ',RTNO3,' used for RTNH4'
        ENDIF  


!-----------------------------------------------------------------------
!       Calculate PHASE DURATIONS FROM phint if missing
!-----------------------------------------------------------------------
                  
        ! BASED ON ORIGINAL CERES -- FOR INITIAL CALIBRATION
        IF (CROP.EQ.'WH') THEN
          IF (PD(1).LE.0.0) THEN
            PD(1) = 400 * PHINTS / 95
            TVR1 = (3.0*PHINTS)
            PD(2) = 0.25 * (3.0*PHINTS)
            PD(3) = 0.75 * (3.0*PHINTS)
            PD(4) = 2.4 * PHINTS
            PD(5) = 0.25 * (2.4*PHINTS)
            PD(6) = 0.10 * (2.4*PHINTS)
            PD(7) = 0.65 * (2.4*PHINTS)
            Write (fnumwrk,*) ' '
            Write (fnumwrk,*) 'CALCULATED phase duration being used'
            Write (fnumwrk,*) ' P1 (400*PHINT/95)     = ',PD(1)
            Write (fnumwrk,*) ' P2 (0.25*(3.0*PHINT)) = ',PD(2)
            Write (fnumwrk,*) ' P3 (0.75*(3.0*PHINT)) = ',PD(3)
            Write (fnumwrk,*) ' P4 (2.40*PHINT)       = ',PD(4)
            Write (fnumwrk,*) ' P5 (0.25*(2.4*PHINT)) = ',PD(5)
            Write (fnumwrk,*) ' P6 (0.10*(2.4*PHINT)) = ',PD(6)
            Write (fnumwrk,*) ' P7 (0.65*(2.4*PHINT)) = ',PD(7)
            Write (fnumwrk,*) ' '
            Write (fnumwrk,*) ' PHINT                 = ',PHINTS
          ENDIF  
        ENDIF  
        IF (CROP.EQ.'BA') THEN
          IF (PD(1).LE.0.0) THEN
            PD(1) = 300 * PHINTS / 70
            TVR1 = 225.0   ! Original vallue in CERES
            PD(2) = 0.25 * (3.2*PHINTS)
            PD(3) = 0.75 * (3.2*PHINTS)
            ! Original = 150
            PD(4) = 2.15 * PHINTS
            ! Original = 200
            PD(5) = 0.25 * (2.9*PHINTS)    ! Original = 60
            PD(6) = 0.10 * (2.9*PHINTS)
            PD(7) = 0.65 * (2.9*PHINTS)
            Write (fnumwrk,*) ' '
            Write (fnumwrk,*) 'CALCULATED phase duration being used'
            Write (fnumwrk,*) ' P1 (300*PHINT/70)     = ',PD(1)
            Write (fnumwrk,*) ' P2 (0.25*(3.2*PHINT)) = ',PD(2)
            Write (fnumwrk,*) ' P3 (0.75*(3.2*PHINT)) = ',PD(3)
            Write (fnumwrk,*) ' P4 (2.415*PHINT)      = ',PD(4)
            Write (fnumwrk,*) ' P5 (0.25*(2.9*PHINT)) = ',PD(5)
            Write (fnumwrk,*) ' P6 (0.10*(2.9*PHINT)) = ',PD(6)
            Write (fnumwrk,*) ' P7 (0.65*(2.9*PHINT)) = ',PD(7)
            Write (fnumwrk,*) ' '
            Write (fnumwrk,*) ' PHINT                 = ',PHINTS
          ENDIF  
        ENDIF  

!-----------------------------------------------------------------------
!       Calculate/adjust phase durations and thresholds
!-----------------------------------------------------------------------
        
        ! Check if phase durations input as leaf units
        DO L = 1,8
          IF (PDL(L).GT.0.0) PD(L) = PDL(L) * PHINTS
        ENDDO
        
        ! Check for missing phase durations and if so use previous
        Ctrnumpd = 0
        DO L = 2,MSTG
          IF (PD(L).LT.0.0) THEN
            PD(L) = PD(L-1)
            CTRNUMPD = CTRNUMPD + 1
          ENDIF
        ENDDO
        ! PD(MSTG=9) read from CUL file for cereals.
        IF (PD(MSTG).LT.0.0) PD(MSTG) = PDMTOHAR
        IF (CTRNUMPD.GT.0) THEN
          Messageno = Min(Messagenox,Messageno+1)
          WRITE(MESSAGE(Messageno),'(A11,I2,A23)')
     &    'Duration of',CTRNUMPD,' phases less than zero.'
          Messageno = Min(Messagenox,Messageno+1)
          MESSAGE(Messageno)='Used value(s) for preceding phase.'
        ENDIF

        ! Calculate thresholds
        GGPHASEDU = 0.
        CHPHASEDU = 0.
        LAFSTDU = 0.
        LGPHASEDU(1) = 0.
        LGPHASEDU(2) = 0.
        LRETSDU = 0. 
        LSENSDU = 0.
        SSPHASEDU = 0.
        RUESTGDU = 0.
        SGPHASEDU = 0.
        SSPHASEDU = 0.
        STVSTGDU =  0.
        TILDEDU = 0.
        TILDSDU = 0.
        TILPEDU = 0.
        VEENDDU = 0.
        VPENDDU = 0.
        DO L = 0,MSTG
          PSTART(L) = 0.0
        ENDDO
        DO L = 1,MSTG
          PSTART(L) = PSTART(L-1) + PD(L-1)
          DO L1 = 1,SSNUM
            IF (INT(SSTG(L1)).EQ.L)
     &        SSTH(L1) = PSTART(L)+(SSTG(L1)-FLOAT(INT(SSTG(L1))))*PD(L)
          ENDDO       
          IF (L.EQ.INT(LAFST))
     &      LAFSTDU = PSTART(L)+(LAFST-FLOAT(INT(LAFST)))*PD(L)
          IF (L.EQ.INT(VEEND))
     &      VEENDDU = PSTART(L)+(VEEND-FLOAT(INT(VEEND)))*PD(L)
          IF (L.EQ.INT(VPEND))
     &      VPENDDU = PSTART(L)+(VPEND-FLOAT(INT(VPEND)))*PD(L)
          IF (L.EQ.INT(RUESTG))
     &      RUESTGDU = PSTART(L)+(RUESTG-FLOAT(INT(RUESTG)))*PD(L)
          IF (L.EQ.INT(LSENS))
     &      LSENSDU = PSTART(L)+(LSENS-FLOAT(INT(LSENS)))*PD(L)
          IF (L.EQ.INT(LSENE))
     &      LSENEDU = PSTART(L)+(LSENE-FLOAT(INT(LSENE)))*PD(L)
          IF (L.EQ.INT(SSPHASE(1))) SSPHASEDU(1) =
     &     PSTART(L)+(SSPHASE(1)-FLOAT(INT(SSPHASE(1))))*PD(L)
          IF (L.EQ.INT(SSPHASE(2))) SSPHASEDU(2) =
     &     PSTART(L)+(SSPHASE(2)-FLOAT(INT(SSPHASE(2))))*PD(L)
          IF (L.EQ.INT(TILPE))
     &      TILPEDU = PSTART(L)+(TILPE-FLOAT(INT(TILPE)))*PD(L)
          IF (L.EQ.INT(TILDS))
     &      TILDSDU = PSTART(L)+(TILDS-FLOAT(INT(TILDS)))*PD(L)
          IF (L.EQ.INT(TILDE))
     &      TILDEDU = PSTART(L)+(TILDE-FLOAT(INT(TILDE)))*PD(L)
          IF (L.EQ.INT(LRETS))
     &      LRETSDU = PSTART(L)+(LRETS-FLOAT(INT(LRETS)))*PD(L)
          IF (L.EQ.INT(CHPHASE(1))) CHPHASEDU(1) = 
     &     PSTART(L)+(CHPHASE(1)-FLOAT(INT(CHPHASE(1))))*PD(L)
          IF (L.EQ.INT(CHPHASE(2))) CHPHASEDU(2) = 
     &     PSTART(L)+(CHPHASE(2)-FLOAT(INT(CHPHASE(2))))*PD(L)
          IF (L.EQ.INT(GGPHASE(1))) GGPHASEDU(1) =
     &     PSTART(L)+(GGPHASE(1)-FLOAT(INT(GGPHASE(1))))*PD(L)
          IF (L.EQ.INT(GGPHASE(2))) GGPHASEDU(2) =
     &     PSTART(L)+(GGPHASE(2)-FLOAT(INT(GGPHASE(2))))*PD(L)
          IF (L.EQ.INT(GGPHASE(3))) GGPHASEDU(3) =
     &     PSTART(L)+(GGPHASE(3)-FLOAT(INT(GGPHASE(3))))*PD(L)
          IF (L.EQ.INT(GGPHASE(4))) GGPHASEDU(4) =
     &     PSTART(L)+(GGPHASE(4)-FLOAT(INT(GGPHASE(4))))*PD(L)
          IF (L.EQ.INT(LGPHASE(1))) LGPHASEDU(1) =
     &     PSTART(L)+(LGPHASE(1)-FLOAT(INT(LGPHASE(1))))*PD(L)
          IF (L.EQ.INT(LGPHASE(2))) LGPHASEDU(2) =
     &     PSTART(L)+(LGPHASE(2)-FLOAT(INT(LGPHASE(2))))*PD(L)
          IF (L.EQ.INT(SGPHASE(1))) SGPHASEDU(1) =
     &     PSTART(L)+(SGPHASE(1)-FLOAT(INT(SGPHASE(1))))*PD(L)
          IF (L.EQ.INT(SGPHASE(2))) SGPHASEDU(2) =
     &     PSTART(L)+(SGPHASE(2)-FLOAT(INT(SGPHASE(2))))*PD(L)
          IF (L.EQ.INT(STVSTG)) STVSTGDU =
     &     PSTART(L)+(STVSTG-FLOAT(INT(STVSTG)))*PD(L)
        ENDDO
        
        DUTOMSTG = 0.0
        DO L = 1, MSTG
          DUTOMSTG = DUTOMSTG + PD(L)
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
        
        ! Adjust germination phase for seed dormancy
        IF (PLMAGE.LT.0.0.AND.PLMAGE.GT.-90.0) THEN
          PEGD = PGERM - (PLMAGE*STDAY) ! Dormancy has negative age
        ELSE
          PEGD = PGERM
        ENDIF

        ! Set stage defaults if necessary
        !  Leaf increment factor
        IF (LAFSTDU.LE.0.0) LAFSTDU = DUTOMSTG+1000.0

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
          Messageno = Min(Messagenox,Messageno+1)
          WRITE(MESSAGE(Messageno),'(A47)')
     &    'Initial leaf size (LA1S) missing. Set to 5 cm2.'
        ENDIF
        IF (LAFNO.GT.0.0.AND.LAFNO.LE.LAXNO) THEN
          LAFNO = LAXNO + 10
          Messageno = Min(Messagenox,Messageno+1)
          WRITE(MESSAGE(Messageno),'(A59)')
     &    'Leaf # for final size < # for maximum! Set to # for max+10.'
        ENDIF
        IF (DFPE.LT.0.0) THEN  
          DFPE = 1.0
          Messageno = Min(Messagenox,Messageno+1)
          WRITE(MESSAGE(Messageno),'(A51)')
     &    'Pre-emergence development factor missing. Set to 1.'
        ENDIF
        IF (G3.LE.0.0.AND.CROP.NE.'CS') THEN  
          G3 = 1.0
          Messageno = Min(Messagenox,Messageno+1)
          WRITE(MESSAGE(Messageno),'(A41)')
     &    'Standard shoot weight missing. Set to 1.0'
        ENDIF
        ! Stem fraction constant throughout lifecycle
        IF (SWFRS.GT.0.0) THEN
          SWFRX = SWFRS
          SWFRXL = 9999
          SWFRN = SWFRS
          SWFRNL = 0
        ENDIF 

        ! Phases
        IF (LGPHASE(1).LT.0.0) LGPHASEDU(1) = 0.0
        IF (LGPHASE(2).LE.0.0) LGPHASEDU(2) = DUTOMSTG
        IF (SGPHASEDU(1).LT.0.0) SGPHASEDU(1) = 0.0
        IF (SGPHASEDU(2).LE.0.0) SGPHASEDU(2) = DUTOMSTG
        IF (GGPHASE(1).LE.0.0) GGPHASEDU(1) = 99999
        IF (CHPHASE(1).LE.0.0) CHPHASEDU(1) = 99999
        IF (CHPHASE(2).LE.0.0) CHPHASEDU(2) = 99999
       
        ! Grain                
        IF (GWTS.LE.0.0.AND.CROP.NE.'CS') THEN  
          GWTS = 30.0
          Messageno = Min(Messagenox,Messageno+1)
          WRITE(MESSAGE(Messageno),'(A33)')
     &    'Grain weight missing. Set to 30.0'
        ENDIF
        IF (GWTCF.LE.0.0) GWTCF = 0.0
        IF (GWTAA.LE.0.0) GWTAA = 0.0
        IF (GWTAT.LE.0.0) GWTAT = 1.0
        IF (GNOWS.LE.0.0.AND.CROP.NE.'CS') THEN  
          GNOWS = 20.0
          Messageno = Min(Messagenox,Messageno+1)
          WRITE(MESSAGE(Messageno),'(A49)')
     &    'Grain number per unit weight missing. Set to 20.0'
        ENDIF
        IF (GNOWF.LE.0.0) Gnowf = 0.0
        IF (GNONF.LE.0.0) Gnonf = 0.0
        IF (GRNS.LE.0.0) GRNS = 2.0
        IF (GRNMX.LE.0.0) GRNMX = 5.0
        IF (GRNMN.LT.0.0) GRNMN = 0.0

        ! Nitrogen uptake                  
        IF (WFNUL.LT.0.0) WFNUL = 0.0
        IF (WFNUU.LE.0.0) WFNUU = 1.0
        IF (WFNUL.GE.WFNUU) WFNUL = AMAX1(0.0,WFNUU-0.5)
        IF (NCNU.LT.0.0) NCNU = 20.0  
        IF (RLFNU.LT.0.0) RLFNU = 2.0

        ! Radiation use efficiency
        IF (PARUE.LE.0.0) PARUE = 2.3
        
        ! Leaf area
        IF (LA1S.LT.0.0) LA1S = 5.0
        IF (LAFV.LT.0.0) LAFV = 0.1
        IF (LAFR.LT.0.0) LAFR = 0.3
        IF (LAXS.LT.0.0) LAXS = 200.0
        
        ! Roots
        IF (RDGS.LT.0.0) RDGS = 3.0
                
        ! Tillers
        IF (G3.LT.0.0) G3 = 0.0
        IF (TILDF.LT.0.0) TILDF = 0.0
        IF (TILSF.LT.0.0) TILSF = 0.0
        IF (HLOSSFR.LT.0.0) HLOSSFR = 0.2
        IF (HLOSSTEMP.LT.0.0) HLOSSTEMP = 20.0
        IF (GWLFR.LE.0.0) GWLFR = 0.05
        IF (GWEFR.LE.0.0) GWEFR = 0.95
        IF (WFPL.LT.0.0) WFPL = 0.0
        IF (WFPU.LT.0.0) WFPU = 1.0
        IF (WFGL.LT.0.0) WFGL = 0.0
        IF (WFGU.LT.0.0) WFGU = 1.0
        IF (NFTL.LT.0.0) NFTL = 0.0
        IF (NFTU.LT.0.0) NFTU = 1.0
        IF (NFPL.LT.0.0) NFPL = 0.0
        IF (NFPU.LT.0.0) NFPU = 1.0
        IF (NFGL.LT.0.0) NFGL = 0.0
        IF (NFGU.LT.0.0) NFGU = 1.0
        IF (NFS.LT.0.0)  NFS = 0.2
        IF (NFSF.LT.0.0) NFSF = 0.1
        IF (LSEWF.LT.0.0) LSEWF = 0.3
        IF (LRETS.GE.10.OR.LRETS.LE.0.0) LRETSDU = 99999
        IF (LSENE.LE.0.0) LSENEDU = 99999
        IF (LSENI.LT.0.0) LSENI = 0.0
        IF (PARIX.LE.0.0) PARIX = 0.995
        IF (LSENS.GE.10.OR.LSENS.LE.0.0) LSENSDU = 99999
        IF (NTUPF.LT.0.0) NTUPF = 0.2
        IF (NPTFL.LE.0.0) NPTFL = 0.5
        IF (PPEXP.LT.0.0) PPEXP = 2.0
        IF (RLFWU.LT.0.0) RLFWU = 0.5  
        IF (RTUFR.LT.0.0) RTUFR = 0.05
        IF (SNOFX.LE.0.0) SNOFX = 1.0
        IF (SSPHASEDU(1).LT.0.0) SSPHASEDU(1) = 99999
        IF (TGR(20).LT.0.0) THEN 
          DO L = 3,22
            TGR(L) = 1.0 ! Tiller (shoot) sizes relative to main shoot
          ENDDO
        ENDIF
        IF (TI1LF.LT.0.0) TI1LF = 1000
        IF (TIFAC.LT.0.0) TIFAC = 1.0
        IF (TILDE.LT.0.0) TILDEDU = 99999
        IF (TILDS.LT.0.0) TILDSDU = 99999
        IF (TILIP.LT.0.0) TILIP = 0.0
        IF (TILPE.LT.0.0) TILPEDU = 99999
        IF (TINOX.LE.0.0) TINOX = 30
        IF (TKLFD.LT.0.0) TKLFD = 5.0
        IF (TKSPAN.LT.0.0) TKSPAN = 6.0
        IF (TKTID.LT.0.0) TKTID = 3.0
        IF (VEEND.LE.0.0) VEEND = 0.0
        IF (VEFF.LT.0.0) VEFF = 0.0
        IF (VEFF.GT.1.0) VEFF = 1.0
        IF (VLOSS0STG.LT.0.0) VLOSS0STG = 0.2
        IF (VLOSSFR.LT.0.0) VLOSSFR = 0.2
        IF (VLOSSTEMP.LT.0.0) VLOSSTEMP = 30.0
        IF (VREQ.LE.0.0) VREQ = 0.0
        IF (VBASE.LT.0.0) VBASE = 0.0
        IF (VPEND.LE.0.0) VPEND = 0.0
        IF (WFTL.LT.0.0) WFTL = 0.0
        IF (WFTU.LT.0.0) WFTU = 0.0
        IF (TRGFC(2).LE.0.0) THEN      ! Grain temperature responses
          DO L = 1,4
             TRGFC(L) = TRPHS(L)
             TRGFN(L) = TRPHS(L)
          ENDDO
        ENDIF

        IF (SLPF.LE.0.0 .OR. SLPF.GT.1.0) SLPF = 1.0
        IF (SLPF.LT.1.0) THEN
          WRITE (fnumwrk,*) ' '
          WRITE (fnumwrk,*)
     &     ' WARNING  Soil fertility factor was less than 1.0: ',slpf
        ENDIF  

!-----------------------------------------------------------------------
!       Calculate derived coefficients and set equivalences
!-----------------------------------------------------------------------

        ! Initial leaf growth aspects
        Lapotx(1) = La1s
        LAIXX = LOG(1.0-PARIX)/(-KCAN)
        
        ! New lah march 2010
        DO L = 1,PHSX
          IF (PHINTL(L).LE.0.0) PHINTL(L) = 1000.0
          IF (PHINTF(L).LE.0.0) PHINTF(L) = 1.0
        ENDDO
        phintstg = 1
        IF (phintf(1).GT.0.0) THEN
          phint = phints*phintf(1)
        ELSE
          phint = phints
        ENDIF
        LLIFGTT = LLIFG * PHINT 
        LLIFATT = LLIFA * PHINT 
        LLIFSTT = LLIFS * PHINT 
        ! End New

        ! Extinction coeff for SRAD
        KEP = (KCAN/(1.0-TPAR)) * (1.0-TSRAD)

        ! Photoperiod sensitivities
        DO L = 0,10
          IF (DAYLS(L).LE.0.0) DAYLS(L) = 0.0
        ENDDO
        IF (Dayls(1).EQ.0.0.AND.dfpe.LT.1.0) THEN
          Messageno = Min(Messagenox,Messageno+1)
          WRITE(MESSAGE(Messageno),'(A36,A41)')
     &    'Cultivar insensitive to photoperiod ',
     &    'but pre-emergence photoperiod factor < 1.' 
          Messageno = Min(Messagenox,Messageno+1)
          WRITE(MESSAGE(Messageno),'(A40)')
     &    'May be worthwhile to change PPFPE to 1.0'
        ENDIF

        ! Reserve accumulation in stem
        IF (RSCA.GT.0.0) THEN
          RSFRS = RSCA * 0.01
        ELSE
          RSFRS = 0.0
        ENDIF
        ! NB.RSFRS is the fraction of stem assimilates going to reserves
        !    instead of structural material. The conversion from the
        !    ecotype coefficient is only approximate because the actual
        !    tops percentage at anthesis depends on leaf amount as well
        !    as stem.

        ! Tiller growth rates relative to main shoot
        IF (TGR(20).GE.0.0) THEN
          DO L = 3,22
            IF (L.LT.20) THEN
              TGR(L) = TGR(2)-((TGR(2)-TGR(20))/18)*(L-2)
            ELSEIF (L.GT.20) THEN
              TGR(L) = TGR(20)
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
        IF (SRNS.LE.0.0) THEN
          IF(SRPRS.GT.0.0) THEN
            SRNS = (SRPRS/100.0) / 6.25
          ELSE
            SRNS = 0.65
          ENDIF
        ENDIF       

        ! Height growth
          IF (CROP.EQ.'WH'.OR.CROP.EQ.'BA') THEN
            SERX = CANHTS/(PD(1)+PD(2)+PD(3)+PD(4)+PD(5)+PD(6))
          ELSE
            IF (SGPHASE(2)-SGPHASE(1).GT.0.0) THEN
              SERX = CANHTS/(SGPHASEDU(2)-SGPHASEDU(1))
            ELSE
              SERX = 0.0
            ENDIF  
          ENDIF  

!-----------------------------------------------------------------------
!       Set coefficients that dependent on input switch
!-----------------------------------------------------------------------

        IF (ISWWAT.EQ.'E') THEN
          ! Plant water status effects on growth turned off
          WFTU = 0.0
          WFGU = 0.0
          WFPU = 0.0
          WFS = 0.0
          WFSF = 0.0
          WFRTG = 0.0
        ENDIF

!-----------------------------------------------------------------------
!       Calculate/set initial states
!-----------------------------------------------------------------------

        IF (SDRATE.LE.0.0) SDRATE = SDSZ*PLTPOPP*10.0
        ! Reserves = 80% of seed (42% Ceres3.5)
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
        TKILL = TKUH
        VF = (1.0-VEFF)    

        ! Water table depth
        WTDEP = ICWT

        ! Initial shoot and root placement
        IF (SPRL.LT.0.0) SPRL = 0.0
        sdepthu = -99.0
        IF (PLME.EQ.'H') THEN
          sdepthu = sdepth
        ELSEIF (PLME.EQ.'I') THEN
          ! Assumes that inclined at 45o
          sdepthu = AMAX1(0.0,sdepth - 0.5*sprl)
        ELSEIF (PLME.EQ.'V') THEN
          sdepthu = AMAX1(0.0,sdepth - sprl)
        ENDIF
        IF (sdepthu.LT.0.0) sdepthu = sdepth

!-----------------------------------------------------------------------
!       Create output descriptors
!-----------------------------------------------------------------------

        ! Run name
        IF (runname(1:6).EQ.'      ' .OR.
     &    runname(1:3).EQ.'-99') runname(1:25) = tname

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
        ! LAH NEED TO  KNOW WHAT TO DO ABOUT RUNNAME
        IF (LENRNAME.GT.5) THEN
          TRUNNAME = TNAME(1:LENTNAME)//' '//MODNAME
!     &     //' ('//RUNNAME(1:LENRNAME)//')'
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

!-------------------------------------------------------------------
!       Write run information to Overview and Work output files
!-------------------------------------------------------------------
                    
        WRITE(fnumwrk,*)' '
        WRITE(fnumwrk,'(A18,A10,I3)')' GENERAL INFO FOR ',
     &       excode,tn
        WRITE(fnumwrk,*)' FILE       ',FILEIO(1:60)
        WRITE(fnumwrk,*)' EXPERIMENT ',EXCODE
        WRITE(fnumwrk,*)' TREATMENT  ',TN
        WRITE(fnumwrk,*)' REPLICATE  ',RN
        WRITE(fnumwrk,*)' '
        WRITE(fnumwrk,*)' MODEL      ',MODEL
        WRITE(fnumwrk,*)' MODULE     ',MODNAME
        WRITE(fnumwrk,*)' PRODUCT    ',HPROD
        WRITE(fnumwrk,*)' RNMODE     ',RNMODE
        IF (RUN.LT.10) THEN
          WRITE(fnumwrk,'(A13,I1)')' RUN        ',RUN   
        ELSEIF (RUN.GE.10.AND.RUN.LT.1000) THEN
          WRITE(fnumwrk,'(A13,I2)')' RUN        ',RUN   
        ELSE
          WRITE(fnumwrk,'(A13,I3)')' RUN        ',RUN   
        ENDIF
        WRITE(fnumwrk,*)' CULTIVAR   ',CUDIRFLE(1:60)
        WRITE(fnumwrk,*)' ECOTYPE    ',ECDIRFLE(1:60)
        WRITE(fnumwrk,*)' SPECIES    ',SPDIRFLE(1:60)
        WRITE(fnumwrk,*)' METHODS '
        IF (MEEXP.EQ.'E')
     &   WRITE(fnumwrk,'(A26,A1)')'   EXPERIMENTAL ALGORITHM ',MEEXP
         WRITE(fnumwrk,'(A26,A1)') '   PHOTOSYNTHESIS         ',MEPHS
         WRITE(fnumwrk,'(A26,A1,1X,A1)') '   WATER AND N SWITCHES   '
     &     ,ISWWAT,ISWNIT
         WRITE(fnumwrk,'(A26,A3)')  '   N UPTAKE               ',MERNU
         WRITE(fnumwrk,'(A26,I1)') ' '
         WRITE(fnumwrk,'(A26,I1)') '  CROP COMPONENT          ',CN
         WRITE(fnumwrk,'(A26,A6,2X,A16)')
     &     '  CULTIVAR                ',VARNO,VRNAME
        IF (IPLTI.NE.'A') THEN
          WRITE(fnumwrk,'(A23,I7)')
     &     '  PLANTING DATE TARGET:',PLYEARDOYT
        ELSE
          WRITE (fnumwrk,*) 'PFIRST,PLAST : ',pwdinf,pwdinl
          WRITE (fnumwrk,*) 'HFIRST,HLAST : ',hfirst,hlast
        ENDIF
        WRITE (fnumwrk,'(A15,2F7.1)')'  PLTPOP,ROWSPC',PLTPOPP,ROWSPC
        WRITE (fnumwrk,'(A15,2F7.1)')'  SDEPTH,SDRATE',SDEPTH,SDRATE
        IF (sdepthu.LT.sdepth)
     &   WRITE (fnumwrk,'(A15,F7.1)')'  SHOOT DEPTH  ',SDEPTHU      
        WRITE (fnumwrk,'(A15,2F7.1,A6)')'  SEEDRS,SEEDN ',
     &                   SEEDRSI*PLTPOPP*10.0,SEEDNI*PLTPOPP*10.0,
     &                   ' kg/ha'
        WRITE (fnumwrk,'(A15, F7.1)') '  PLMAGE       ',PLMAGE
        ! LAH NEED TO CHECK HARVEST OPTIONS FOR dap,growth stage.
        ! DISCUSS WITH CHP
        IF (IHARI.NE.'M') THEN
          IF (IHARI.NE.'A') THEN
            WRITE(fnumwrk,'(A22,I7)')
     &      '  HARVEST DATE TARGET:',YEARDOYHARF 
          ELSE
            WRITE(fnumwrk,'(A22,A9)')
     &      '  HARVEST DATE TARGET:','AUTOMATIC'  
          ENDIF 
        ELSE
          WRITE(fnumwrk,'(A22,A8)')
     &     '  HARVEST DATE TARGET:','MATURITY'  
        ENDIF
        WRITE (fnumwrk,'(A15,2F7.1)') '  HPCF,HBPCF   ',HPCF,HBPCF

        IF (IDETG.NE.'N') THEN
          WRITE(fnumwrk,*)' '
          WRITE(fnumwrk,*)' MAJOR COEFFICIENTS AFTER CHECKING'
          WRITE(fnumwrk,*)' Development '
          WRITE(fnumwrk,*)'  Vreq   ',Vreq 
          WRITE(fnumwrk,*)'  Vbase  ',Vbase
          WRITE(fnumwrk,*)'  Veff   ',Veff 
          WRITE(fnumwrk,*)'  Vpend  ',Vpend
          WRITE(fnumwrk,*)'  Veend  ',Veend
          WRITE(fnumwrk,*)'  Ppsen  ',Ppsen   
          WRITE(fnumwrk,*)'  Ppfpe  ',Dfpe 
          IF (Ppsen.EQ.'LQ') WRITE(fnumwrk,*)'  Ppexp  ',Ppexp   
          WRITE(fnumwrk,*)'  Ppthr  ',Ppthr   
          WRITE(fnumwrk,*)'  Pps1   ',Dayls(1)
          WRITE(fnumwrk,*)'  Pps2   ',Dayls(2)
          WRITE(fnumwrk,*)'  P1     ',Pd(1)
          WRITE(fnumwrk,*)'  P2     ',Pd(2)
          WRITE(fnumwrk,*)'  P3     ',Pd(3)
          WRITE(fnumwrk,*)'  P4     ',Pd(4)
          WRITE(fnumwrk,*)'  P5     ',Pd(5)
          WRITE(fnumwrk,*)'  P6     ',Pd(6)
          WRITE(fnumwrk,*)'  P7     ',Pd(7)
          WRITE(fnumwrk,*)'  P8     ',Pd(8)
          WRITE(fnumwrk,*)'  P9     ',Pd(9)
          WRITE(fnumwrk,*)' Chaff stages '
          WRITE(fnumwrk,*)'  S1     ',Chphase(1),Chphasedu(1)
          WRITE(fnumwrk,*)'  S2     ',Chphase(2),Chphasedu(2)
          WRITE(fnumwrk,*)' Shoot weights '
          WRITE(fnumwrk,*)'  Shwas  ',Gwtat
          WRITE(fnumwrk,*)'  Shwts  ',G3
          WRITE(fnumwrk,*)' Grain stages '
          WRITE(fnumwrk,*)'  S1     ',GGphase(1),Ggphasedu(1)
          WRITE(fnumwrk,*)'  S2     ',GGphase(2),Ggphasedu(2)
          WRITE(fnumwrk,*)'  s3     ',GGphase(3),Ggphasedu(3)
          WRITE(fnumwrk,*)'  S4     ',GGphase(4),Ggphasedu(4)
          WRITE(fnumwrk,*)' Grain weight '
          WRITE(fnumwrk,*)'  Gwts   ',Gwts 
          WRITE(fnumwrk,*)'  Gwlag,Gwlin   ',Gwlfr,gwefr
          WRITE(fnumwrk,*)'  Gw#f   ',Gwtcf
          WRITE(fnumwrk,*)'  Gwwf   ',Gwtaa
          WRITE(fnumwrk,*)' Grain number '
          WRITE(fnumwrk,*)'  G#wts  ',Gnows
          WRITE(fnumwrk,*)'  G#wsf  ',Gnowf
          WRITE(fnumwrk,*)'  G#nsf  ',Gnonf
          WRITE(fnumwrk,*)' Nitrogen uptake'
          WRITE(fnumwrk,*)'  Wfnul  ',Wfnul
          WRITE(fnumwrk,*)'  Wfnuu  ',Wfnuu
          WRITE(fnumwrk,*)'  Ncnu   ',Ncnu
          WRITE(fnumwrk,*)'  Rlfnu  ',Rlfnu
          WRITE(fnumwrk,*)' Radiation use  '
          WRITE(fnumwrk,*)'  Parue  ',Parue
          WRITE(fnumwrk,*)'  Paru2  ',Paru2
          WRITE(fnumwrk,*)' Tillers  '
          WRITE(fnumwrk,*)'  Shwts  ',g3
          WRITE(fnumwrk,*)'  Tdfac  ',Tildf
          WRITE(fnumwrk,*)'  Tdsf   ',Tilsf
          WRITE(fnumwrk,*)' Leaves     '
          WRITE(fnumwrk,*)'  La1s   ',La1s
          WRITE(fnumwrk,*)'  Lafv   ',Lafv
          WRITE(fnumwrk,*)'  Lafr   ',Lafr
          WRITE(fnumwrk,*)'  Laxs   ',Laxs
          WRITE(fnumwrk,*)'  Phints ',Phints                
          WRITE(fnumwrk,*)'  Phl1,Phf1 ',Phintl(1),Phintf(1)
          WRITE(fnumwrk,*)'  Phl2,Phf2 ',Phintl(2),Phintf(2)
          WRITE(fnumwrk,*)'  Phl3,Phf3 ',Phintl(3),Phintf(3)
          WRITE(fnumwrk,*)'  Lsens,Lsendsdu ',Lsens,lsensdu
          WRITE(fnumwrk,*)'  Lsene,Lsendedu ',Lsene,lsenedu
          WRITE(fnumwrk,*)' Stems      '
          WRITE(fnumwrk,*)'  Ssphs,Ssphasedu(1)',Ssphase(1),Ssphasedu(1)
          WRITE(fnumwrk,*)'  Ssphe,Ssphasedu(2)',Ssphase(2),Ssphasedu(2)
          WRITE(fnumwrk,*)'  Saws,Serx          ',Saws,Serx            
          WRITE(fnumwrk,*)' Roots  '
          WRITE(fnumwrk,*)'  Rdgs   ',Rdgs
          WRITE(fnumwrk,*)'  Rresp  ',Rresp
          WRITE(fnumwrk,*)' Storage roots '
          WRITE(fnumwrk,*)'  Srfr   ',Srfr
          WRITE(fnumwrk,*)' Nitrogen concentrations'
          WRITE(fnumwrk,*)'  Ln%s   ',LNPCS           
          WRITE(fnumwrk,*)'  Ln%mn  ',LNPCMN            
          WRITE(fnumwrk,*)'  Sn%s   ',SNPCS           
          WRITE(fnumwrk,*)'  Sn%mn  ',SNPCMN           
          WRITE(fnumwrk,*)'  Rn%s   ',RNPCS           
          WRITE(fnumwrk,*)'  Rn%mn  ',RNPCMN          
          WRITE(fnumwrk,*)' Nitrogen stress limits '
          WRITE(fnumwrk,*)'  Nfpu,L ',NFPU,NFPL       
          WRITE(fnumwrk,*)'  Nfgu,L ',NFGU,NFGL       
          WRITE(fnumwrk,*)'  Nftu,L ',NFTU,NFTL       
          WRITE(fnumwrk,*)'  Nfsu,Sf',NFS,NFSF      

          WRITE(fnumwrk,*)' '                            
          WRITE(fnumwrk,*)' CASSAVA                '
          WRITE(fnumwrk,*)' Storage root initiation',PDSRI
          WRITE(fnumwrk,*)' LLIFG,LLIFA,LLIFS      ',LLIFG,LLIFA,LLIFS
         
          WRITE(FNUMWRK,*)' '
          WRITE(FNUMWRK,'(A17)')' PHASE THRESHOLDS'
          WRITE(FNUMWRK,'(A18)')'   PHASE START(DU)'
          DO L = 1,10
            WRITE(FNUMWRK,'(I6,I10)')L,NINT(PSTART(L))
          ENDDO

          IF (ISWDIS(LENDIS:LENDIS).NE.'N') THEN
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,*) 'DISEASE INITIATION AND GROWTH ASPECTS'
            WRITE (fnumwrk,'(A13,A49)')'             ',
     &       '  DATE   GROWTH FACTOR  FAVOURABILITY REQUIREMENT'
            WRITE (fnumwrk,'(A12,I10,2F10.1)')
     &       '  DISEASE 1 ',DIDAT(1),DIGFAC(1),DIFFACR(1)
            WRITE (fnumwrk,'(A12,I10,2F10.1)')
     &       '  DISEASE 2 ',DIDAT(2),DIGFAC(2),DIFFACR(2)
            WRITE (fnumwrk,'(A12,I10,2F10.1)')
     &       '  DISEASE 3 ',DIDAT(3),DIGFAC(3),DIFFACR(3)
            WRITE (fnumwrk,*) ' '
            IF (DCTAR(1).GT.0) WRITE (fnumwrk,*)
     &       'DISEASE CONTROL DATES,GROWTH FACTORS,AND DURATION'
            DO L=1,DCNX
              IF (DCTAR(L).EQ.1) WRITE (fnumwrk,'(A12,I10,2F10.1)')
     &         '  DISEASE 1 ',DCDAT(L),DCFAC(L),DCDUR(L)
              IF (DCTAR(L).EQ.2) WRITE (fnumwrk,'(A12,I10,2F10.1)')
     &         '  DISEASE 2 ',DCDAT(L),DCFAC(L),DCDUR(L)
              IF (DCTAR(L).EQ.3) WRITE (fnumwrk,'(A12,I10,2F10.1)')
     &         '  DISEASE 3 ',DCDAT(L),DCFAC(L),DCDUR(L)
            ENDDO
          ENDIF

        ENDIF

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

        ! Set flags

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
        IF (MEPHS.NE.'I') THEN
          IF (MEPHS.NE.'R') THEN
            IF (MEPHS.NE.'P') THEN
               Messageno = Min(Messagenox,Messageno+1)
               WRITE(MESSAGE(Messageno),'(A22,A1,A15,A35)')
     &          'Photosynthesis method ',MEPHS,' not an option ',
     &          ' Changed to P (PAR use efficiency.)'
              MEPHS = 'P'
            ENDIF
          ENDIF
        ENDIF
        IF (IHARI.NE.'M') THEN
          IF (hnumber.LE.0) THEN 
            Messageno = Min(Messagenox,Messageno+1)
            WRITE(MESSAGE(Messageno),'(A37,A13,A1)')
     &        'No harvest date set although planting',
     &        'flag set to: ',IHARI
            Messageno = Min(Messagenox,Messageno+1)
            MESSAGE(Messageno)='Flag reset to M.'
            IHARI = 'M'                      
          ENDIF
        ENDIF

!-----------------------------------------------------------------------
!       Create and write warning messages re. input and output variables
!-----------------------------------------------------------------------

        IF (SLPF.LT.1.0) THEN
          WRITE (fnumwrk,*) ' '
          WRITE (fnumwrk,*)
     &     ' WARNING  Soil fertility factor was less than 1.0: ',slpf
        ENDIF  

        Messageno = Min(Messagenox,Messageno+1)
        WRITE(MESSAGE(Messageno),'(A36,A37)')
     &    'Note:Reserve CH2O included in the wt',
     &    ' of leaves,stems,and (cereals) chaff.' 
        CALL WARNING(Messageno,'CSCRP',MESSAGE)
        Messageno = 0

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
        IF (SNOW.LE.0) THEN
          TMEANSURF = TMEAN
        ELSE
          TMEANSURF = 0.0
        ENDIF
        CO2AIR = 1.0E12*CO2*1.0E-6*44.0 /       ! CO2 in g/m3
     &   (8.314*1.0E7*((TMAX+TMIN)*0.5+273.0))

!-----------------------------------------------------------------------
!       Determine if today is planting day
!-----------------------------------------------------------------------

        ! YRPLT established by CSM and brought across in argument list.
        IF (FILEIOT.EQ.'DS4'.AND.RNMODE.EQ.'Q') THEN
          PLYEARDOYT = YRPLT
        ENDIF

        IF (PLYEARDOY.GT.9000000) THEN            ! If before planting
          IF(PLYEARDOYT.GT.0 .AND. PLYEARDOYT.LT.9000000)THEN
            ! Specified planting date
            IF(YEARDOY.EQ.PLYEARDOYT) THEN
              PLYEARDOY = YEARDOY
              PLYEAR = YEAR
            ENDIF
          ELSE
            ! Automatic planting
            ! Check window for automatic planting,PWDINF<PLYEART<PWDINL
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
     &           (MAX(SW(I) - LL(I),0.0)) / (DUL(I) - LL(I))
                I = I + 1
              END DO
              AVGSW = (CUMSW / SWPLTD) * 100.0
              IF (TSDEP .GE. PTTN .AND. TSDEP .LE. PTX) THEN
                IF (AVGSW .GE. SWPLTL .AND. AVGSW .LE. SWPLTH) THEN
                  PLYEARDOY = YEARDOY
                  PLYEAR = YEAR
                ENDIF
              ENDIF
            ELSE
              IF (YEARDOY.GT.PWDINL) THEN
                CFLFAIL = 'Y'
                STGYEARDOY(12) = YEARDOY  ! Failure
                STGYEARDOY(11) = YEARDOY  ! End Crop
                Message(1) = 'Automatic planting failure '
                CALL WARNING(1,'CSCRP',MESSAGE)
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
          ! Tiller # set equal to plants per hill
          IF (PLPH.GT.0.0) THEN
            TNUM = PLPH
            TNUML(1) = TNUM
          ELSE
            TNUM = 1.0
          ENDIF
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
          IF (FILEIOT.EQ.'DS4'.AND.IDETG.NE.'N'.OR.
     &        FILEIOT.NE.'DS4') THEN                  
            IF (ISWWAT.NE.'N') THEN
            ! Co2 effect
            IF(CROP.EQ.'MZ'.OR.CROP.EQ.'ML'.OR.CROP.EQ.'SG')THEN
              ! C-4 Crops  EQ 7 from Allen (1986) for corn.
              RB = 10.0
              RLF =(1.0/(0.0328-5.49E-5*330.0+2.96E-8*330.0**2))+RB
              RLFC=(1.0/(0.0328-5.49E-5*CO2+2.96E-8*CO2**2))+RB
            ELSE
              ! C-3 Crops
              RLF  = 9.72 + 0.0757 * 330.0 + 10.0
              RLFC = 9.72 + 0.0757 *  CO2  + 10.0
            ENDIF
            RSOIL = 0.0 ! s/m
            CALL EVAPO('M',SRAD,CLOUDS,TMAX,TMIN,TDEW,WINDSP,
     &       ALBEDO,RATM,RSOIL,
     &       TVR1,EOPEN,EOSOIL,EOPT,EOEBUD,TCAN,'M')
             EOMPEN = EOSOIL
             EOPENC = EOPENC + EOPEN 
             EOMPENC = EOMPENC + EOMPEN
             EOPTC = EOPTC + EOPT
             EOEBUDC = EOEBUDC + EOEBUD
            ! CSM with LAI=1.0,CROP=WH,TAVG=20.0,WINDSP=86.4 has
            ! RATM = 55  RCROP = 45
            ! Monteith had RATM = 300, RCROP = 150->500
            CALL EVAPO('M',SRAD,CLOUDS,TMAX,TMIN,TDEW,WINDSP,
     &       ALBEDO,RATM,RCROP,
     &       TVR1,TVR2,EOP330,TVR3,eoebudcrp,TCAN,'M')
            IF (RLF.GT.0.0)
     &       CALL EVAPO('M',SRAD,CLOUDS,TMAX,TMIN,TDEW,WINDSP,
     &       ALBEDO,RATM,RCROP*RLFC/RLF,
     &       TVR1,TVR2,EOPCO2,TVR3,eoebudcrp2,TCAN,'M')
            ! Transpiration ratio (Pot.pl.evap/Pot.soil evap)
            EPSRATIO = 1.0
            IF (EOSOIL.GT.0.0) EPSRATIO = EOPCO2 / EOSOIL
            TRATIO = 1.0
            IF (EOP330.GT.0.0) TRATIO = EOPCO2 / EOP330
            IF (fileiot(1:2).NE.'DS')
     &       EOP = MAX(0.0,EO/EOSOIL*EOPCO2 * (1.0-EXP(-LAI*KEP)))
            ! Ratio necessary because EO method may not be Monteith
            ENDIF
            IF (fileiot(1:2).NE.'DS') THEN
              CALL CSCRPROOTWU(ISWWAT, 
     &         NLAYR, DLAYR, LL, SAT, WFEU, MEWNU,
     &         EOP,
     &         RLV, RWUPM, RLFWU, RWUMX, RTDEP,
     &         SW, WTDEP,
     &         uh2o, trwup, trwu)
            ENDIF
            ! Stomatal resistance adjustment if water shortage
            RSCO2 = RCROP
            IF (RLF.GT.0.0) RSCO2 = RSCO2 * RLFC / RLF
            RSADJ = RSCO2
            IF (LAI > 1.E-6) THEN
              IF (EOP.GT.(10.0*TRWU/(1.0-EXP(-LAI*KEP)))) THEN
                RSADJ = 9999.9
                IF (TRWU.GT.0.0) THEN
                  TVR1 = (10.0*TRWU/(1.0-EXP(-LAI*KEP)))
                  CALL EVAPO('M',SRAD,CLOUDS,TMAX,TMIN,TDEW,WINDSP,
     &            ALBEDO,RATM,RCROP,
     &            TVR1,TVR2,EOSOIL,RSADJ,TVR4,TCAN,'M')
                ENDIF
              ENDIF
            ENDIF
            ! Canopy temperature
            TVR1 = (TRWU*10.0+ES)
            CALL EVAPO('M',SRAD,CLOUDS,TMAX,TMIN,TDEW,WINDSP,
     &      ALBEDO,RATM,RCROP,TVR1,TVR2,TVR3,TVR4,TVR5,
     &      TCAN,'C')
          ENDIF
          ! Cumulative potential ET as used
          IF (EO.GT.0.0) EOC = EOC + EO
          TDIFSUM = TDIFSUM+(TCAN-TMEAN)
          TDIFNUM = TDIFNUM + 1
          TDIFAV = TDIFSUM/TDIFNUM

!-----------------------------------------------------------------------
!         Calculate thermal time
!-----------------------------------------------------------------------

          !Tfd = TFAC4P(trdev,tmean,rstage,TT)
          Tfd = TFAC4(trdv1,tmean,TT)
          IF (rstage+1.LT.10)
     &     Tfdnext = TFAC4(trdv2,tmean,TTNEXT)
!     &      Tfdnext = TFAC4P(trdev,tmean,rstage+1.0,TTNEXT)

          ! Angus approach
          !IF (RSTAGE.LT.2.0) THEN
          !  TT = AMAX1(0.0,1.0 - exp(-0.231*(tmean-3.28)))*(TMEAN-3.28)
          !ELSE 
          !  TT = AMAX1(0.0,1.0 - exp(-0.116*(tmean-5.11)))*(TMEAN-5.11)
          !ENDIF
          !IF (RSTAGE.LT.1.0) THEN
          !  TTNEXT = AMAX1(0.0,1.0 - exp(-0.231*(tmean-3.28)))
     ^    !   *(TMEAN-3.28)
          !ELSE  
          !  TTNEXT = AMAX1(0.0,1.0 - exp(-0.116*(tmean-5.11)))
     &    !   *(TMEAN-5.11)
          !ENDIF  
          ! End Angus

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

!=======================================================================
          IF (GEUCUM+TT*WFGE.GE.PEGD) THEN  ! If germinated by endday
!=======================================================================

!-----------------------------------------------------------------------
!           Determine when in day germination and emergence occurred
!-----------------------------------------------------------------------

            ! Germination
            IF (GEUCUM.LT.PEGD.AND.GEUCUM+TT*WFGE.LE.PEGD) THEN
              GERMFR = 0.0
            ELSEIF (GEUCUM.LE.PEGD.AND.GEUCUM+TT*WFGE.GT.PEGD) THEN
              GERMFR = 1.0 - (PEGD-GEUCUM)/(TT*WFGE)
              STGYEARDOY(1) = YEARDOY
            ELSEIF (GEUCUM.GT.PEGD) THEN
              GERMFR = 1.0
            ENDIF

            ! Emergence
            IF (GEUCUM.LT.PEGD+PECM*SDEPTHU.AND.
     &       GEUCUM+TT*WFGE.LE.PEGD+PECM*SDEPTHU) THEN
              EMRGFR = 0.0
            ELSEIF (GEUCUM.LE.PEGD+PECM*SDEPTHU.AND.
     &       GEUCUM+TT*WFGE.GT.PEGD+PECM*SDEPTHU) THEN
              EMRGFR = 1.0 - (PEGD+PECM*SDEPTHU-GEUCUM)/(TT*WFGE)
            ELSEIF (GEUCUM.GT.PEGD+PECM*SDEPTHU) THEN
              EMRGFR = 1.0
            ENDIF

!-----------------------------------------------------------------------
!           Calculate temperature factors for vernalization,hardening
!-----------------------------------------------------------------------

            ! Vernalization
            Tfv = TFAC4(trvrn,tmeansurf,TTOUT)
            ! Loss of vernalization (De-vernalization)
            VDLOS = 0.0
            IF (CUMVD.LT.VREQ*VLOSS0STG .AND. TMEAN.GE.VLOSSTEMP) THEN
              ! In Ceres was 0.5*(TMAX-30.0)
              VDLOS = VLOSSFR * CUMVD  ! From AFRC
            ENDIF

            ! Cold hardening
            Tfh = TFAC4(trcoh,tmeansurf,TTOUT)
            ! Loss of cold hardiness
            HARDILOS = 0.0
            IF (TMEAN.GE.HLOSSTEMP) THEN
              HARDILOS = HLOSSFR * HARDAYS
            ENDIF
     
!-----------------------------------------------------------------------
!           Calculate daylength factors for development
!-----------------------------------------------------------------------

            DF = 1.0
            DFNEXT = 1.0
            ! To ensure correct sensitivity on emergence day
            IF (RSTAGE.LE.0.0) THEN
              RSTAGETMP = 1.0
            ELSE
              RSTAGETMP = RSTAGE
            ENDIF
            IF (PPSEN.EQ.'SL') THEN      ! Short day response,linear 
              DF = 1.0 - DAYLS(INT(RSTAGETMP))/1000.*(PPTHR-DAYL)
              IF (RSTAGETMP.LT.FLOAT(MSTG)) THEN
               DFNEXT = 1.-DAYLS(INT(RSTAGETMP+1))/1000.*(PPTHR-DAYL)
              ELSE
               DFNEXT = DF
              ENDIF 
            ELSEIF (PPSEN.EQ.'LQ') THEN  ! Long day response,quadratic
              DF = AMAX1(0.0,AMIN1(1.0,1.0-
     &        (DAYLS(INT(RSTAGETMP))/10000.*(PPTHR-DAYL)**PPEXP)))
              IF (RSTAGETMP.LT.10) DFNEXT = AMAX1(0.0,AMIN1(1.0,1.0-
     &        (DAYLS(INT(RSTAGETMP+1))/10000.*(PPTHR-DAYL)**PPEXP)))
              ! Angus approach
              !IF (rstage.lt.2.0) then
              !  DF = AMAX1(0.0,1.0-EXP(-0.0927*(DAYL-4.77))) 
              !else
              !  DF = AMAX1(0.0,1.0-EXP(-0.283*(DAYL-9.27))) 
              !endif  
              !IF (rstage.lt.1.0) then
              !  DFNEXT = AMAX1(0.0,1.0-EXP(-0.0927*(DAYL-4.77))) 
              !else
              !  DFNEXT = AMAX1(0.0,1.0-EXP(-0.283*(DAYL-9.27))) 
              !endif  
              ! End Angus
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
            IF (RSTAGETMP.LT.10) THEN
              DUNEED = PSTART(INT(RSTAGETMP+1))-CUMDU
              IF (DUNEED.GE.TT*VF*(DFPE*(GERMFR-EMRGFR)+DF*EMRGFR))THEN
                DUPHASE = TT*VF*(DFPE*(GERMFR-EMRGFR)+DF*EMRGFR)
                ! CERES:
                !DU = TT*VF*DF*LIF2    ! NB. Changed from Ceres 3.5
                TIMENEED = 1.0
                DUPNEXT = 0.0
              ELSE  
                DUPHASE = DUNEED
                TIMENEED = DUNEED/
     &           (TT*VF*(DFPE*(GERMFR-EMRGFR)+DF*EMRGFR))
                DUPNEXT = TTNEXT*(1.0-TIMENEED)*VFNEXT*DFNEXT
              ENDIF
            ELSE
              ! Taken out LAH 230311 to allow progress to specified harvest
!             WRITE (fnumwrk,*)' '                  
!             WRITE (fnumwrk,'(A54,/,A54,/,A26)')
!     &         ' Maximum number (10) of developmental stages reached! ',
!     &         ' Presumably,the phase lengths specified were too short',
!     &         ' Will assume crop failure.'
!              CFLFAIL = 'Y'
            ENDIF
            
            DU = DUPHASE+DUPNEXT

            ! Leaf growth units
            IF (CUMDU.LT.LGPHASEDU(2)) THEN
              IF (DU.GT.0.0) THEN
               LFENDFR = AMAX1(0.0,AMIN1(1.0,(LGPHASEDU(2)-CUMDU)/DU))
              ELSE
                LFENDFR = 1.0
              ENDIF
              ! LAH TEMPORARY JAN 2010
              ! LAH Needs generalizing to not use PHINTCF with number
              ! For cereals need terminal spikelet date+FLN calculation
              IF (TSSTG.GT.0) THEN 
                ! Crop has a terminal spikelet stage
                IF (CUMDU+DU.LT.PSTART(TSSTG)) THEN
                  ! Not reached TS yet
                  DULF = TT*EMRGFR*LFENDFR  ! Thermal time
                ELSE
                  IF (CUMDU.LT.PSTART(TSSTG)) THEN
                    ! Reach TS this day
                    DULF = (TT*TIMENEED+DUPNEXT)*EMRGFR*LFENDFR
                    LNUMTS = LNUM + (TT*TIMENEED*EMRGFR*LFENDFR)/PHINT
                    FLN = LNUMTS+(PD(2)+PD(3))/PHINT
                    ! Calculate FLN using Aitken formula-for comparison
                    FLNAITKEN = LNUMTS + (2.8 + 0.1*LNUMTS) 
                    
                    ! Maybe calculated FLN using Jamieson model
                    !DAYLSAT = 15.0
                    !LNUMDLR = 0.70
                    !LNUMMIN = 7.0
                    !PRNUM = 4.0 + 2.0*LNUM 
                    !PRNUMCRIT = 2.0
                    !TVR1 = LNUMMIN+AMAX1(0.0,LNUMDLR*(DAYLSAT-DAYL))
                    !IF (PRNUM.GE.TVR1+PRNUMCRIT.AND.LNUM.GT.0.0.AND.
     &               ! CFLFLN.NE.'Y') THEN
                     ! CFLFLN = 'Y'
                     ! FLDAP = DAP
                     ! FLN = LNUMMIN+AMAX1(0.0,LNUMDLR*(DAYLSAT-DAYL))
                    !ENDIF
                    ! END JAMIESON MODEL
                    
                    ! LAH WORKING FLN if last leaf expands 90% 
                    ! FLN = FLN+1.0-MOD(FLN,(FLOAT(INT(FLN))))-0.1
                    ! WRITE(FNUMWRK,*) ' NEW FLN ',TVR1
                    ! LAH WORKING Change PHINT so expands to new FLN
                    ! PHINT = (PD(2)+PD(3))/(TVR1-LNUMTS)
                    ! WRITE(FNUMWRK,*) ' NEW PHINT ',PHINT
                    ! LAH WORKING Alternate FLN calculation 
                    ! If > 0.5 becomes final leaf, < 0.5 is ignored
                    ! TVR1 = MOD(FLN,(FLOAT(INT(FLN))))
                    ! IF (TVR1.LT.0.5) THEN
                    !  FLN = FLOAT(INT(FLN))-0.001
                    ! ELSE
                    !  FLN = FLOAT(INT(FLN))+0.999
                    ! ENDIF
                    CFLPDADJ = 'N'
                    IF (CFLPDADJ.EQ.'Y') THEN
                      IF (PHINTL(2).GT.0..AND.FLN+1..LE.PHINTL(2)) THEN
                        PD2ADJ = (FLN-LNUMTS) * PHINTS*PHINTF(2)  
                      ELSEIF (PHINTL(2).GT.0. .AND.
     &                 FLN+1..GT.PHINTL(2)) THEN
                        IF (LNUM.GE.PHINTL(2)) THEN
                          PD2ADJ = (FLN-LNUMTS) * PHINTS*PHINTF(3)  
                        ELSE
                          PD2ADJ = ((FLN-PHINTL(2)))*PHINTS*PHINTF(2)  
     &                          + (PHINTL(2)-LNUMTS)*PHINTS*PHINTF(3) 
                        ENDIF   
                      ELSE  
                        PD2ADJ = ((FLN-LNUMTS)) * PHINT
                      ENDIF
                      TVR2 = AMAX1(0.0,PD2ADJ-(PD(2)+PD(3)))
                      PD(3) = PD(3)+TVR2
                      PD(4) = PD(4)-TVR2
                      WRITE(fnumwrk,*)' '  
                      WRITE(fnumwrk,'(A22,I7)')
     &                 ' TERMINAL SPIKELET ON ',YEARDOY
                      WRITE(fnumwrk,'(A32,F5.1)')
     &                 '  T.SPK LEAF NUMBER             ',LNUMTS
                      WRITE(fnumwrk,'(A32,F5.1,2X,F5.1)')
     &                 '  FINAL LEAF NUMBER CUL,AITKEN  ',FLN,FLNAITKEN 
                      WRITE(fnumwrk,'(A32,I5,I7)')
     &                 '  P2+3 DURATIONS C FILE,ADJUSTED',
     &                 INT(PD(2)+PD(3)),INT(PD2ADJ)
                      WRITE(fnumwrk,'(A32,F5.1,F7.1)')
     &                 'PD(3) NEW,OLD                 ',PD(3),PD(3)-TVR2
                      WRITE(fnumwrk,'(A32,F5.1,F7.1)')
     &                 'PD(4) NEW,OLD                 ',PD(4),PD(4)+TVR2

                      ! Re-calculate thresholds
                      LAFSTDU = 0.
                      VEENDDU = 0.
                      VPENDDU = 0.
                      RUESTGDU = 0.
                      LSENSDU = 0.
                      TILPEDU = 0.
                      TILDSDU = 0.
                      TILDEDU = 0.
                      LRETSDU = 0.0
                      GGPHASEDU = 0.0
                      CHPHASEDU = 0.0
                      LGPHASEDU = 0.0
                      SGPHASEDU = 0.0
                      SSPHASEDU = 0.0
                      STVSTGDU = 0.0
                      DO L = 1,MSTG
                        PSTART(L) = PSTART(L-1) + PD(L-1)
                      ENDDO
                      DO L = 1,MSTG
                        DO L1 = 1,SSNUM
                          IF (INT(SSTG(L1)).EQ.L)
     &                      SSTH(L1) = PSTART(L)
     &                          + (SSTG(L1)-FLOAT(INT(SSTG(L1))))*PD(L)
                        ENDDO       
                        IF (L.EQ.INT(LAFST))
     &                    LAFSTDU = PSTART(L)
     &                            + (LAFST-FLOAT(INT(LAFST)))*PD(L)
                        IF (L.EQ.INT(VEEND))
     &                    VEENDDU = PSTART(L)
     &                            + (VEEND-FLOAT(INT(VEEND)))*PD(L)
                        IF (L.EQ.INT(VPEND))
     &                    VPENDDU = PSTART(L)
     &                            + (VPEND-FLOAT(INT(VPEND)))*PD(L)
                        IF (L.EQ.INT(RUESTG))
     &                    RUESTGDU = PSTART(L)
     &                             + (RUESTG-FLOAT(INT(RUESTG)))*PD(L)
                        IF (L.EQ.INT(LSENS))
     &                    LSENSDU = PSTART(L)
     &                            + (LSENS-FLOAT(INT(LSENS)))*PD(L)
                        IF (L.EQ.INT(LSENE))
     &                    LSENEDU = PSTART(L)
     &                            + (LSENE-FLOAT(INT(LSENE)))*PD(L)
                        IF (L.EQ.INT(SSPHASE(1)))
     &                    SSPHASEDU(1) = PSTART(L) + (SSPHASE(1)
     &                                  -FLOAT(INT(SSPHASE(1))))*PD(L)
                        IF (L.EQ.INT(SSPHASE(2))) 
     &                    SSPHASEDU(2) = PSTART(L) + (SSPHASE(2)
     &                            - FLOAT(INT(SSPHASE(2))))*PD(L)
                        IF (L.EQ.INT(TILPE))
     &                    TILPEDU = PSTART(L)
     &                            + (TILPE-FLOAT(INT(TILPE)))*PD(L)
                        IF (L.EQ.INT(TILDS))
     &                    TILDSDU = PSTART(L)
     &                            + (TILDS-FLOAT(INT(TILDS)))*PD(L)
                        IF (L.EQ.INT(TILDE))
     &                    TILDEDU = PSTART(L)
     &                            + (TILDE-FLOAT(INT(TILDE)))*PD(L)
                        IF (L.EQ.INT(LRETS))
     &                    LRETSDU = PSTART(L)
     &                            + (LRETS-FLOAT(INT(LRETS)))*PD(L)
                        IF (L.EQ.INT(CHPHASE(1))) 
     &                   CHPHASEDU(1) = PSTART(L) + (CHPHASE(1)
     &                                - FLOAT(INT(CHPHASE(1))))*PD(L)
                        IF (L.EQ.INT(CHPHASE(2))) 
     &                   CHPHASEDU(2) = PSTART(L) + (CHPHASE(2)
     &                        -FLOAT(INT(CHPHASE(2))))*PD(L)
                        IF (L.EQ.INT(GGPHASE(1))) 
     &                   GGPHASEDU(1) = PSTART(L) + (GGPHASE(1)
     &                                 - FLOAT(INT(GGPHASE(1))))*PD(L)
                        IF (L.EQ.INT(GGPHASE(2))) 
     &                   GGPHASEDU(2) = PSTART(L) + (GGPHASE(2)
     &                                - FLOAT(INT(GGPHASE(2))))*PD(L)
                        IF (L.EQ.INT(GGPHASE(3))) 
     &                   GGPHASEDU(3) = PSTART(L) + (GGPHASE(3)
     &                                - FLOAT(INT(GGPHASE(3))))*PD(L)
                        IF (L.EQ.INT(GGPHASE(4))) 
     &                   GGPHASEDU(4) = PSTART(L) + (GGPHASE(4)
     &                                - FLOAT(INT(GGPHASE(4))))*PD(L)
                        IF (L.EQ.INT(LGPHASE(1))) 
     &                   LGPHASEDU(1) = PSTART(L) + (LGPHASE(1)
     &                                - FLOAT(INT(LGPHASE(1))))*PD(L)
                        IF (L.EQ.INT(LGPHASE(2))) 
     &                   LGPHASEDU(2) = PSTART(L) + (LGPHASE(2)
     &                                - FLOAT(INT(LGPHASE(2))))*PD(L)
                        IF (L.EQ.INT(SGPHASE(1))) 
     &                   SGPHASEDU(1) = PSTART(L) + (SGPHASE(1)
     &                                 - FLOAT(INT(SGPHASE(1))))*PD(L)
                        IF (L.EQ.INT(SGPHASE(2))) 
     &                   SGPHASEDU(2) = PSTART(L) + (SGPHASE(2)
     &                                - FLOAT(INT(SGPHASE(2))))*PD(L)
                        IF (L.EQ.INT(STVSTG)) 
     &                   STVSTGDU = PSTART(L) + (STVSTG
     &                            - FLOAT(INT(STVSTG)))*PD(L)
                      ENDDO
                    ENDIF  ! End CFLPDADJ
                  ELSE  
                    ! After TS 
                    DULF = DU*EMRGFR*LFENDFR  ! Development units
                  ENDIF                
                ENDIF
              ELSE
                ! Crop does not have a terminal spikelet stage
                DULF = TT*EMRGFR*LFENDFR  ! Thermal time
              ENDIF
            ENDIF
            
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

!=======================================================================
            IF (GEUCUM+TT*WFGE.GT.PGERM+PECM*SDEPTHU) THEN ! If emerged
!=======================================================================

!-----------------------------------------------------------------------
!             Determine if today has a harvest instruction
!-----------------------------------------------------------------------

              DO I = 1, 20
                IF (HYEARDOY(I).EQ.YEARDOY) THEN
                  HANUM = I
                  WRITE(fnumwrk,*) ' '
                  WRITE(fnumwrk,'(A21,i2,A12,A1,A6,i8)')
     &             '  Harvest instruction',hanum,
     &             '  Operation ',hop(i),
     &             '  Day ',yeardoy
                  CALL CSUCASE(HOP(I)) 
                  IF (hop(i).EQ.'F') YEARDOYHARF = YEARDOY 
                ENDIF
              END DO

!-----------------------------------------------------------------------
!             Determine amounts removed by grazing,etc.   
!-----------------------------------------------------------------------

	      IF (HANUM.GT.0) THEN
	        IF (HOP(HANUM).EQ.'G'.AND.
     &	         CWAD.GT.0.0.AND.CWAD.GT.CWAN(HANUM)) THEN
                  IF (LSNUM(HANUM).GT.0) THEN
                    ! LAH To fit with Zhang should have Animal routine
!                   CALL ANIMAL(TMAX,TMIN,LSNUM(HANUM),
!    &              LSWT(HANUM),CWAD,CWAN(HANUM),hawad)
                  ELSE
	            HAWAD = AMIN1((CWAD-CWAN(HANUM)),HAMT(HANUM))
	          ENDIF  
	          HAWAD = AMAX1(0.0,HAWAD)
                  HAFR = AMAX1(0.0,HAWAD/CWAD)
	        ELSE   
	          HAWAD = 0.0
	          HAFR = 0.0
	        ENDIF
              ENDIF
              
              IF (HAFR.GT.0.0)
     &         WRITE(fnumwrk,'(A23,3F6.1)')' HARVEST  FR,CWAN,CWAD ',
     &          HAFR,CWAN(HANUM),CWAD

              ! For grazing 
              lwph = lfwt * hafr
              laph = lapd * hafr
              swph = stwt * hafr
              rswph = rswt * hafr
              gwph = grwt * hafr
              dwrph = deadwtr * hafr
              lnph = leafn * hafr
              snph = stemn * hafr
              rsnph = rsn * hafr
              gnph = grainn * hafr
              IF (rstage.GT.3.0) spnumh = tnum * hafr*spnumhfac
              snph = 0.0
              rsnph = 0.0
              gnph = 0.0

!-----------------------------------------------------------------------
!             Set aspects that determined on emergence day
!-----------------------------------------------------------------------

              IF (DAE.LT.0) THEN
                IF (CROP.EQ.'BA') THEN
                  !PHINTS = 77.5 - 232.6*(DAYL-DAYLPREV)
                ENDIF
                LNUMSG = 1
              ENDIF

!-----------------------------------------------------------------------
!             Check for or calculate PAR interception at start of day
!-----------------------------------------------------------------------

              PARIF = 0.0
              PARIF1 = (1.0 - EXP((-KCAN)*LAI))
              IF (PARIP.GT.0.0) THEN
                ! From competition model
                IF (ISWDIS(LENDIS:LENDIS).NE.'N') THEN
                  PARIF = PARIPA/100.0
                ELSE
                  PARIF = PARIP/100.0
                ENDIF
                ! LAH  Should use canopy area during grain filling
                !IF (CUMDU.GT.PSTART(IESTG)) THEN
                !  PARIF = AMAX1(PARIF,(1.0-EXP(-KCAN*CAID)))
                !ENDIF
              ELSE
                PARIF = PARIF1
                ! LAH For row crops may need to change 
                ! In original Ceres maize, kcan is calculated as:
                ! 1.5 - 0.768*((rowspc*0.01)**2*pltpop)**0.1
                ! eg. 1.5 - 0.768*((75*0.01)**2*6.0)**0.1  =  0.63
              ENDIF

!-----------------------------------------------------------------------
!             Calculate adjustment to yesterday's C assimilation
!-----------------------------------------------------------------------

              ! End of day interception = today's starting interception
              CARBOENDI = CARBOTMPI * PARIF/PLTPOP
              CARBOENDP = CARBOTMPP * PARIF/PLTPOP
              CARBOENDR = CARBOTMPR * PARIF/PLTPOP

              IF (MEPHS.EQ.'R') CARBOEND = CARBOENDR
              IF (MEPHS.EQ.'I') CARBOEND = CARBOENDI
              IF (MEPHS.EQ.'P') CARBOEND = CARBOENDP

              CARBOADJ = (CARBOEND-CARBOBEG)/2.0*EMRGFRPREV
              ! But note, no adjustment if leaf kill
              PARMJIADJ = PARMJFAC*SRADPREV*(PARIF-PARIFPREV)/2.0*EMRGFR

!-----------------------------------------------------------------------
!             Calculate process rate factors
!-----------------------------------------------------------------------

              ! Water
              ! No water stress after emergence on day that emerges
              WFG = 1.0
              WFP = 1.0
              WFT = 1.0
              IF (ISWWAT.NE.'N') THEN
                IF (EOP.GT.0.0) THEN
                  WUPR = TRWUP/(EOP*0.1)
                  IF (WFGU-WFGL.GT.0.0)
     &             WFG = AMAX1(0.0,AMIN1(1.0,(WUPR-WFGL)/(WFGU-WFGL)))
                  IF (WFPU-WFPL.GT.0.0)
     &             WFP = AMAX1(0.0,AMIN1(1.0,(WUPR-WFPL)/(WFPU-WFPL)))
                  IF (WFTU-WFTL.GT.1.0E-6) WFT =
     &              AMAX1(0.0,AMIN1(1.0,(WUPR-WFTL)/(WFTU-WFTL)))
                ENDIF
                IF (ISWWATEARLY.EQ.'N') THEN
                  WFG = 1.0
                  WFP = 1.0
                  WFT = 1.0
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
                  LNCTL = LNCM + NFTL * (LNCX-LNCM)
                  LNCTU = LNCM + NFTU * (LNCX-LNCM)
                  IF (LNCTU - LNCTL > 1.E-6) THEN
                   NFT =AMIN1(1.0,AMAX1(0.0,(LANC-LNCTL)/(LNCTU-LNCTL)))
                  ELSE
                   NFT = 1.0 
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
                  NFT = 1.0
                ENDIF
              ENDIF

              ! If N stress switched off early in cycle. 
              IF (ISWNITEARLY.EQ.'N') THEN
                NFG = 1.0
                NFP = 1.0  
                NFT = 1.0
              ENDIF

              ! Reserves
              IF (RSFPU.GT.0.0.AND.RSFPU.GT.0.0) THEN
              RSFP = 1.0-AMIN1(1.0,AMAX1(0.,(RSCD-RSFPL)/(RSFPU-RSFPL)))
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
              Tfgf = TFAC4(trgfc,tmean,TTOUT)
              Tfgn = TFAC4(trgfn,tmean,TTOUT)

              ! Vapour pressure
              VPDFP = 1.0
              IF (PHTV.GT.0.0) THEN
                IF (TDEW.LE.-98.0) TDEW = TMIN
                VPD = CSVPSAT(tmax) - CSVPSAT(TDEW)    ! Pa 
                IF (VPD/1000.0.GT.PHTV)
     &           VPDFP = AMAX1(0.0,1.0+PHSV*(VPD/1000.0-PHTV))
              ENDIF

              ! Co2 factor using CROPGRO formula
              ! CO2EX Exponent for CO2-PHS relationship (0.05)  
              ! COCCC CO2 compensation concentration (80 vpm)
              !CO2FP = PARFC*((1.-EXP(-CO2EX*CO2))-(1.-EXP(-CO2EX*CO2COMPC)))
              ! CO2 factor
              CO2FP = YVALXY(CO2RF,CO2F,CO2)

              !  LAH Notes from original cassava model                                                          
              !  IF (TEMPM .LT. SenCritTemp) THEN
              !     Life(I) = Life(I)-SenTempFac*(SenCritTemp-TEMPM)
              !  ENDIF
              !  IF (CumLAI .GT. SenCritLai) THEN
              !     Life(I) = Life(I)-SenLaiFac*(CumLAI-SenCritLai)
              !  ENDIF
              !  IF (Life(I) .LT. 0.0) Life(I) = 0.0
              ! LSCL   0.4  Leaf senescence,critical LAI
              ! LSCT  18.0  Leaf senescence,critical temperature (C)
              ! LSSL  0.05  Leaf senescence,sensitivity to LAI
              ! LSST   0.3  Leaf senescence,sensitivity to temperature (fr d-1)

!-----------------------------------------------------------------------
!             Calculate leaf number at end of day;adjust PHINT if needed
!-----------------------------------------------------------------------
                                               
              DULFNEXT = 0.0
              LAGEG = 0.0
              LNUMG = 0.0
              ! If in leaf growth phase
              IF(CUMDU.GE.LGPHASEDU(1).AND.CUMDU.LT.LGPHASEDU(2)) THEN
              
                ! NEW LAH MARCH 2010
                IF (PHINTL(PHINTSTG).LE.0.0.
     &               OR.LNUM+DULF/PHINT.LE.PHINTL(PHINTSTG)) THEN
                  LNUMEND = LNUM + DULF/PHINT
                  IF (INT(LNUMEND).GT.INT(LNUM)) 
     &             DULFNEXT = (LNUMEND-FLOAT(INT(LNUMEND))) * PHINT
                ELSEIF(PHINTL(PHINTSTG).GT.0.
     &            .AND. LNUM+DULF/PHINT.GT.PHINTL(PHINTSTG))THEN
                  TVR1 = AMAX1(0.0,DULF-((PHINTL(PHINTSTG)-LNUM)*PHINT))
                  LNUMEND = PHINTL(PHINTSTG) 
     &                    + TVR1/(PHINT*PHINTF(PHINTSTG+1))
                  IF (INT(LNUMEND).GT.INT(LNUM)) THEN
                    ! Below not fully accurate - assumes that new 
                    ! leaf growing entirely at new PHINT
                    DULFNEXT = LNUMEND
     &              - FLOAT(INT(LNUMEND))*(PHINT*PHINTF(PHINTSTG+1))
                  ENDIF
                  WRITE(FNUMWRK,*)' '
                  WRITE(FNUMWRK,'(A20,I3,A4,I3,A11,F4.1)')
     &             ' PHINT changed from ',INT(PHINT),
     &             ' to ',INT(phints*phintf(phintstg+1)),
     &             '. Leaf # = ',lnum
                  PHINT = PHINTS * PHINTF(phintstg+1)
                  Phintstg = Phintstg + 1
                  ! Leaf growth,active,senescence phases adjusted
                  LLIFGTT = LLIFG * PHINT 
                  LLIFATT = LLIFA * PHINT 
                  LLIFSTT = LLIFS * PHINT 
                ENDIF
                ! END NEW
              ENDIF
              
              ! Restrict to maximum
              LNUMEND = AMIN1(FLOAT(LNUMX),LNUMEND)
              IF(FLN.GT.0.0) LNUMEND = AMIN1(FLN,LNUMEND)
              LNUMG = LNUMEND - LNUM
              ! Calculate an overall PHINT for output
              IF (LNUMG.GT.0.0) PHINTOUT = DULF/LNUMG

!-----------------------------------------------------------------------
!             Calculate senescence of leaves,stems,etc..
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

              TNUMLOSS = 0.0
              PLTLOSS = 0.0
              PLASC = 0.0
              PLASP = 0.0
              PLASI = 0.0
              PLASL = 0.0
              PLASFS = 0.0
              PLASPM = 0.0
              PLASS = 0.0
              PLAST = 0.0
              PLAST1 = 0.0
              PLAST2 = 0.0
              SENRS = 0.0
              SENNRS = 0.0
              SENSTG = 0.0
              SENSTGRS = 0.0
              SENNSTG = 0.0
              SENNSTGRS = 0.0
              SSENF = 0.0

              ! Leaf senescence - cold kill
              TKLF = AMIN1(0.0,TKILL+TKLFD)
              IF (PLA-SENLA.GT.0.0.AND.TMEAN.LT.TKLF) THEN
                !SPANPOS = AMIN1(1.0,(TKLF-TMEAN)/TKSPAN)
                !PLASC = (PLA-SENLA)*SPANPOS
                PLASC = (PLA-SENLA)*SPANPOS
                PLASCSUM = PLASCSUM + PLASC
                CARBOADJ = 0.0
                Messageno = Min(Messagenox,Messageno+1)
                WRITE (Message(Messageno),899)
     &           TKLF,(TMIN+TMAX)/2.0,HSTAGE,
     &           (PLA-SENLA)*PLTPOP*0.0001,
     &           (PLA-SENLA)*PLTPOP*0.0001*SPANPOS
 899            FORMAT ('Leaf kill ',
     &           ' TKLF =',F5.1,1X,'TMEAN =',F5.1,1X,
     &           'HSTAGE =',F5.1,1X,'LAI =',  F5.2,1X,'LOSS =',F6.3)
              ENDIF

              ! Tiller kill
              TNUMLOSS = 0.0
              TKTI = TKILL+TKTID
              IF (TNUM.GT.1.0.AND.TMEAN.LT.TKTI) THEN
                SPANPOS = AMIN1(1.0,(TKTI-TMEAN)/TKSPAN)
                TNUMLOSS = (TNUM-1.0)*SPANPOS
                IF (TNUMLOSS.GT.0.0)THEN
                  Messageno = Min(Messagenox,Messageno+1)
                  WRITE (Message(Messageno),900)
     &            TKTI,(TMIN+TMAX)/2.0,HSTAGE,TNUM,TNUMLOSS
                ENDIF
 900             FORMAT ('Tiller kill ',
     &           ' TKTI =',F5.1,1X,'TMEAN =',F5.1,1X,
     &           'HSTAGE =',F5.1,1X,'TNO =',  F5.2,1X,'LOSS =',F5.2)
              ENDIF

              ! Plant kill
              IF (TMEAN.LT.TKILL) THEN
                SPANPOS = AMIN1(1.0,(TKILL-TMEAN)/TKSPAN)
                PLTLOSS = PLTPOP*SPANPOS
                IF (PLTPOP-PLTLOSS.GE.0.05*PLTPOPP) THEN
                  Messageno = Min(Messagenox,Messageno+1)
                  WRITE (Message(Messageno),901)
     &            TKILL,(TMIN+TMAX)/2.0,HSTAGE,PLTPOP,PLTLOSS
 901              FORMAT ('Plant kill ',
     &             ' TKILL =',F5.1,1X,'TMEAN =',F5.1,1X,
     &             'HSTAGE =',F5.1,1X,'PLNO =',  F5.1,1X,'LOSS =',F5.1)
                ELSE
                  CFLFAIL = 'Y'
                  Messageno = Min(Messagenox,Messageno+1)
                  IF (Messageno.LT.Messagenox)
     &               WRITE (Message(Messageno),1100)
     &            TKILL,(TMIN+TMAX)/2.0,HSTAGE,PLTPOP,PLTLOSS
 1100             FORMAT ('Kill > 95%',
     &             ' TKILL =',F5.1,1X,'TMEAN =',F5.1,1X,
     &             'HSTAGE =',F5.1,1X,'P# =',F5.1,1X,'LOSS =',F5.1)
                ENDIF
              ENDIF

              ! Write to Warning.out if cold kill  
              IF (Messageno.GT.0)CALL WARNING(Messageno,'CSCRP',MESSAGE)
              Messageno = 0

              ! If cold kill, other senescence not calculated
              IF (PLASC.LE.0.0) THEN
              
                ! Leaf senescence - phyllochron driven
                LAPSTMP = 0.0
                IF (CUMDU+DU.LE.LGPHASEDU(2)) THEN
                  DO L = 1,LNUMSG
                    IF (LAGE(L)+TT*EMRGFR.LE.LLIFATT+LLIFGTT) EXIT
                    IF (LAP(L)-LAPS(L).GT.0.0) THEN
                      LAPSTMP = AMIN1((LAP(L)-LAPS(L)),LAP(L)/LLIFSTT
     &                 *AMIN1((LAGE(L)+DULF*EMRGFR-(LLIFATT+LLIFGTT)),
     &                 DULF*EMRGFR))
                      LAPS(L) = LAPS(L) + LAPSTMP
                      PLASP = PLASP + LAPSTMP
                    ENDIF
                  ENDDO
                ENDIF

                ! Leaf senescence - final triggers
                IF (RSTAGEFS.LE.0.0) THEN
                  IF (CUMDU+DU.GE.LSENSDU) THEN
                    RSTAGEFS = LSENS
                    FSDU = LSENSDU
                  ENDIF
                  IF (ISWNIT.NE.'N') THEN
                   LNCSENF = LNCM + NFSF * (LNCX-LNCM)
                   IF (CUMDU.GE.PSTART(MSTG-1).AND.LANC.LE.LNCSENF) THEN
                     RSTAGEFS = RSTAGE
                     FSDU = CUMDU
                     WRITE(Message(1),'(A34)')
     &                'Final senescence trigger(Nitrogen)'
                     CALL WARNING(1,'CSCRP',MESSAGE)
                   ENDIF
                  ENDIF 
                  IF (ISWWAT.NE.'N') THEN
                   IF (CUMDU.GE.PSTART(MSTG-1).AND.WUPR.LE.WFSF) THEN
                     RSTAGEFS = RSTAGE
                     FSDU = CUMDU
                     WRITE(Message(1),'(A32)')
     &                'Final senescence trigger(Water) '         
                     CALL WARNING(1,'CSCRP',MESSAGE)
                   ENDIF
                  ENDIF
                  ! Determine duration of final senescence phase
                  IF (FSDU.GT.0.0) THEN
                    PDFS = LSENEDU - FSDU
                  ELSE
                    PDFS = LSENEDU
                  ENDIF  
                ENDIF

                ! Leaf senescence - injury
                IF (CUMDU+DU.GT.LGPHASEDU(2)) THEN
                  IF (CUMDU.LT.LGPHASEDU(2)) THEN
                    PLASI = PLA*(LSENI/100.0)*(DU-DUNEED)/STDAY
                  ELSE
                    IF (RSTAGEFS.GT.0.0.AND.DU.GT.0.0) THEN
                      PLASI = AMAX1(0.0,PLA*(LSENI/100.0)*DU/STDAY*
     &                 (FSDU-CUMDU)/DU)
                      IF (GPLASENF.LE.0.0)
     &                 GPLASENF = AMAX1(0.0,PLA-SENLA-PLASI)
                    ELSE
                      PLASI = PLA*(LSENI/100.0)*DU/STDAY
                    ENDIF
                  ENDIF
                ENDIF

                ! Leaf senescence - final,before end of grain filling
                IF (RSTAGEFS.GT.0.0.AND.PDFS.GT.0.0) THEN
                  IF (CUMDU.LT.FSDU.AND.CUMDU+DU.GT.FSDU) THEN
                    PLASFS = AMAX1(0.0,GPLASENF*(CUMDU+DU-FSDU)/PDFS)
                  ELSEIF(CUMDU.GE.FSDU.AND.
     &             CUMDU+DU.LE.PSTART(MSTG))THEN
                    PLASFS = AMIN1(PLA-SENLA,AMAX1(0.,GPLASENF*DU/PDFS))
                  ELSEIF(CUMDU.GE.FSDU.AND.
     &             CUMDU+DU.LT.PSTART(MSTG))THEN
                    PLASFS = AMIN1(PLA-SENLA,
     &                       AMAX1(0.,GPLASENF*DUNEED/PDFS))
                  ENDIF
                ENDIF

                ! Leaf senescence - final,after end of grain filling
                IF (CUMDU+DU.GT.PSTART(MSTG) .AND. PDFS > 1.E-6) THEN
                  IF (CUMDU.LT.PSTART(MSTG)) THEN
                    PLASPM  = AMIN1(PLA-SENLA-PLASFS,
     &                        AMAX1(0.0,GPLASENF*(DU-DUNEED)/PDFS))
                  ELSE
                    PLASPM  = AMIN1(PLA-SENLA-PLASFS,
     &                        AMAX1(0.0,GPLASENF*DU/PDFS))
                  ENDIF
                ELSE
                  PLASPM = 0.0
                ENDIF

                ! Leaf senescence - water or N stress
                PLASW = 0.0
                PLASN = 0.0
                IF (ISWWAT.NE.'N') THEN
                  ! LAH NEED WATER STUFF 
                  IF (PLA-SENLA.GT.0.0.AND.WUPR.LT.WFS)
     &              PLASW = AMAX1(0.0,AMIN1(
     &                  (PLA-SENLA)-PLAS,(PLA-SENLA)*LLOSA))
                ENDIF
                IF (ISWNIT.NE.'N') THEN
                  LNCSEN = LNCM + NFS * (LNCX-LNCM)
                  IF (PLA-SENLA.GT.0.0.AND.LANC.LT.LNCSEN)
     &              PLASN = AMAX1(0.0,AMIN1(
     &              (PLA-SENLA)-PLAS,(PLA-SENLA)*LLOSA))
                ENDIF
                PLASS = PLASW + PLASN    ! Loss because of stress

                ! Tiller death - physiological
                TILWTR = 1.0
                TILWT = 0.0
                TNUMD = 0.0
                ! Original .. no tilsf
                  TILSW = G3 * (CUMDU+DU/2.0)/PSTART(MSTG)
     &                     * (1.0+(1.0-AMIN1(WFT,NFT)))
                ! New basic .. with tilsf
                  TILSW = G3 * (CUMDU+DU/2.0)/PSTART(MSTG)
     &                     * (1.0+((1.0-AMIN1(WFT,NFT))*tilsf))
                ! LAH JAN 2009 ADDED STRESSES TO INCREASE 
                !                        TARGET SIZE->MORE DEATH
                IF (TNUM.GT.0.0)
     &           TILWT = (LFWT+STWT+RSWT+GRWT+CARBOT/2.0)/TNUM
                IF (TILSW.GT.0.0) TILWTR = TILWT/TILSW
                IF (CUMDU+DU.GT.TILDSDU.AND.CUMDU.LT.TILDEDU) THEN
                  TNUMD = AMAX1(.0,
     &             (TNUM-1.)*(1.-TILWTR)*TT/STDAY*(TILDF/100.0))
                   ! 100.0 because tildf read as a percentage 
                ENDIF

                ! Leaf senescence when tillers die
                IF (TNUM.GT.0.0) THEN
                  IF (INT(TNUM).EQ.INT(TNUM-(TNUMD+TNUMLOSS))) THEN
                    PLAST1 = (TNUMD+TNUMLOSS)
     &                     * (TLA(INT(TNUM+1.0))-TLAS(INT(TNUM+1.0)))
                  ELSE
                    PLAST1 = (TNUM-INT(TNUM))*TLA(INT(TNUM+1.0))
                    PLAST2 = (TNUMD+TNUMLOSS-(TNUM-INT(TNUM)))
     &                     * (TLA(INT(TNUM+1.0))-TLAS(INT(TNUM+1.0)))
                  ENDIF
                  PLAST = PLAST1 + PLAST2
                ENDIF
                
                ! Leaf senescence - low light at base of canopy
                ! NB. Just senesces any leaf below critical light fr 
                PLASL = 0.0
                IF (LAI.GT.LAIXX) THEN
                 PLASL = (LAI-LAIXX) / (PLTPOP*0.0001)
                ENDIF
              ENDIF

              ! Leaf senescence - overall
               PLAS =  PLASP + PLASI + PLASFS + PLASPM + PLASS + PLASC
     &             + PLAST + PLASL
              ! Overall check to restrict senescence to what available
              PLAS = AMAX1(0.0,AMIN1(PLAS,PLA-SENLA))

!-----------------------------------------------------------------------
!             Calculate C and N made available through senescence
!-----------------------------------------------------------------------

              SENLFG = 0.0
              SENLFGRS = 0.0
              SENNLFG = 0.0
              SENNLFGRS = 0.0
              SENRS = 0.0
              IF (PLA-SENLA.GT.0.0) THEN
                IF (PLASC.GT.0.0) THEN
                  ! If cold kill
                  SENLFG = AMIN1(LFWT,LFWT*PLASC/(PLA-SENLA))
                  SENRS = AMIN1(RSWT,RSWT*PLASC/(PLA-SENLA))
                ELSE
                  ! If normal senescence
                  SENLFG = AMIN1(LFWT*LSEWF,(AMAX1(0.0,         
     &            ((PLAS*LSEWF))
     &            * (LFWT/(PLA-SENLA)))))
                  SENLFGRS = AMIN1(LFWT*(1.0-LSEWF),(AMAX1(0.0,  
     &            (PLAS*(1.0-LSEWF))
     &            * (LFWT/(PLA-SENLA)))))
                ENDIF
              ENDIF

              ! Stem senescence
              ! LAH JAN2010 NEED TO MAKE SURE IS LINKED TO ACCELERATED 
              ! SENESCENCE OF LEAVES, AND TO STEM AREA
              !  Start may be accelerated,same rate as normal
              SENSTFR = 0.0
              SENSTG = 0.0
              SENSTGRS = 0.0
              IF (SSPHASEDU(1).GT.0.0.AND.SSPHASEDU(2).GT.0.0) THEN
                IF (CUMDU+DU.GT.SSPHASEDU(1)+(FSDU-LSENSDU)) THEN
                  IF (CUMDU.GT.SSPHASE(1)+(FSDU-LSENSDU)) THEN
!                 IF (CUMDU+DU.GT.SSPHASEDU(1)) THEN
!                  IF (CUMDU.GT.SSPHASEDU(1)) THEN
                    IF ((SSPHASEDU(2)-SSPHASEDU(1)).GT.0.0) SENSTFR = 
     &              AMAX1(0.0,AMIN1(1.0,DU/(SSPHASEDU(2)-SSPHASEDU(1))))
                  ELSE
                   IF ((SSPHASEDU(2)-SSPHASEDU(1)).GT.0.0)
     &               SENSTFR = AMAX1(0.0,AMIN1(1.0,
     &                 ((CUMDU+DU)-SSPHASEDU(1))/
     &                             (SSPHASEDU(2)-SSPHASEDU(1))))
                  ENDIF
                  ! LAH JAN 2010 NB No weight loss from stem. 
                  ! SENSTFR used currently for area only
                  !IF (SENSTFR.GT.0.0) SENSTG = STWT*SENSTFR
                ENDIF
              ELSE  
                ! For cassava, no final stem senescence
                SENSTFR = 0.0
              ENDIF  
              IF (ISWNIT.NE.'N') THEN
                ! NB. N loss has a big effect if low N
                IF (PLASC.GT.0.0) THEN
                  SENNLFG = AMIN1(LEAFN,SENLFG*LANC)
                  SENNRS = AMIN1(RSN,RSN*PLASC/(PLA-SENLA))
                ELSE
                  ! Assumes that all reserve N in leaves
                  IF (LFWT.GT.0.0) LANCRS = (LEAFN+RSN) / LFWT
                    SENNLFG = AMIN1(LEAFN,(SENLFG+SENLFGRS)*LNCM)
                    SENNLFGRS = AMIN1(LEAFN-SENNLFG,
     &                               (SENLFG+SENLFGRS)*(LANC-LNCM))
                ENDIF
                IF (SANC.GT.0.0) SSENF = (1.0-((SANC-SNCM)/SANC))
                SENNSTG = SENSTG*SANC*SSENF
                SENNSTGRS = SENSTG*SANC*(1.0-SSENF)
                IF (SENNSTG+SENNSTGRS.GT.STEMN) THEN
                  WRITE(Message(1),'(A28)')
     &              'N removal from stem > stem N'
                  CALL WARNING(1,'CSCRP',MESSAGE)
                  SENNSTG = STEMN-SENNSTGRS
                ENDIF
              ENDIF

!-----------------------------------------------------------------------
!             Calculate overall senescence loss from tops
!-----------------------------------------------------------------------

              SENFR = 1.0
              SENTOPG = 0.0
              SENTOPGGF = 0.0
              IF (DU.GT.0.0) SENFR =
     &         1.0 - AMAX1(0.0,AMIN1(1.0,(CUMDU+DU-LRETSDU)/DU))
              SENTOPG = (SENLFG+SENSTG+SENRS)*SENFR
              ! Following for checking purposes only
              IF (CUMDU.GE.SGPHASEDU(2).AND.CUMDU.LT.PSTART(MSTG))
     &         SENTOPGGF = SENTOPG

!-----------------------------------------------------------------------
!             Calculate C assimilation at beginning of day
!-----------------------------------------------------------------------

              ! PAR utilization efficiency
              IF (RUESTGDU.GT.0.0) THEN
                IF (CUMDU+DU/2.0.LT.RUESTGDU) THEN
                  PARU = PARUE
                ELSE
                  IF (PARU2.LT.0.0) THEN
                    PARU = PARUE
                  ELSE     
                    ! Following is to make a gradual changeover
                    PARURFR = AMIN1(1.0,(CUMDU+DU/2-RUESTGDU)/150.0)
                    PARU = PARUE+(PARU2-PARUE)*PARURFR
                  ENDIF
                ENDIF
              ELSE
                  PARU = PARUE
              ENDIF  

              ! Conventional method using PAR utilization efficiency (P)
              CARBOTMPP = AMAX1(0.0,(PARMJFAC*SRAD)*PARU*CO2FP*TFP
     &         * WFP * NFP * RSFP * VPDFP * SLPF)
              CARBOBEGP = CARBOTMPP * PARIF / PLTPOP

              ! Modified conventional using internal CO2 (I)
              CARBOTMP = AMAX1(0.,PARMJFAC*SRAD*PARU*TFP*NFP*RSFP)
              ! Calculate for no water stress for WFPI determination
              CARBOTMPI = CARBOTMP
              CO2INTPPMP = CO2
              DO L = 1,20
                CO2INT = CO2AIR - CARBOTMPI * (RATM+RSCO2)*1.157407E-05
                CO2INTPPM = AMAX1(CO2COMPC+20.0,CO2INT *
     &          (8.314*1.0E7*((TMAX+TMIN)*.5+273.))/(1.0E12*1.0E-6*44.))
                CO2FPI = PARFC*
     &           ((1.-EXP(-CO2EX*CO2INTPPM))-(1.-EXP(-CO2EX*CO2COMPC)))
                CARBOTMPI = CARBOTMP * CO2FPI
                IF (ABS(CO2INTPPM-CO2INTPPMP).LT.1.0) EXIT
                CO2INTPPMP = CO2INTPPM
              ENDDO
              CARBOBEGIA = 0.0
              IF (CARBOTMPI.GT.0) CARBOBEGIA =(CARBOTMP*CO2FP)/CARBOTMPI
              CARBOTMPI = CARBOTMP
              CO2INTPPMP = CO2
              DO L = 1,20
                CO2INT = CO2AIR - CARBOTMPI * (RATM+RSADJ)*1.157407E-05
                CO2INTPPM = AMAX1(CO2COMPC,CO2INT *
     &          (8.314*1.0E7*((TMAX+TMIN)*0.5+273.))/
     &          (1.0E12*1.0E-6*44.0))
                CO2FPI = PARFC*
     &          ((1.-EXP(-CO2EX*CO2INTPPM))-(1.-EXP(-CO2EX*CO2COMPC)))
                CARBOTMPI = CARBOTMP * CO2FPI
                IF (ABS(CO2INTPPM-CO2INTPPMP).LT.1.0) EXIT
                IF (ABS(CO2INTPPM-CO2COMPC).LT.1.0) EXIT
                CO2INTPPMP = CO2INTPPM
              ENDDO
              CARBOBEGI = CARBOTMPI * SLPF * PARIF / PLTPOP * CARBOBEGIA

              ! Alternate method using resistances as per Monteith (M)
              ! Calculate photosynthetic efficiency
              ! Use 10 Mj.m2.d PAR to establish quantum requirement
              PHOTQR = (CO2AIR/(10.0*PARU)-
     &         ((RATM+RCROP)*1.157407E-05))*
     &         (10.0*30.0)/(CO2AIR*MJPERE) ! 30 = MW Ch2o
              RM = CO2AIR/(((SRAD*PARMJFAC/MJPERE)/PHOTQR)*30.0)
              CARBOTMPR = AMAX1(0.,
     &         (CO2AIR/((RATM+RSADJ)*1.157407E-05+RM))*TFP*NFP*RSFP)
              CARBOBEGR = CARBOTMPR * SLPF * PARIF / PLTPOP

              ! Select method depending on choice in CONTROL FILE
              IF (MEPHS.EQ.'P') CARBOBEG = CARBOBEGP
              IF (MEPHS.EQ.'R') CARBOBEG = CARBOBEGR
              IF (MEPHS.EQ.'I') CARBOBEG = CARBOBEGI

!-----------------------------------------------------------------------
!             Calculate total C available for growth
!-----------------------------------------------------------------------

              ! Assimilation cannot go below zero 
              CARBO = AMAX1(0.0,CARBOBEG+CARBOADJ) + SENLFGRS + SENSTGRS

!-----------------------------------------------------------------------
!             C available to roots (minimum) and stem
!-----------------------------------------------------------------------

              IF (PSTART(MSTG).GT.0) THEN
                PTF = AMIN1(PTFMX,
     &           PTFMN+((PTFMX-PTFMN)*((CUMDU+DU/2.0))/PSTART(MSTG)))
              ELSE
                PTF = (PTFMX+PTFMN)/2.0
              ENDIF
              ! Partition adjustment for stress effects
              PTF = AMIN1(PTFMX,PTF-PTFA*(1.0-AMIN1(WFG,NFG)))
              CARBOR = AMAX1(0.0,(CARBOBEG+CARBOADJ))*(1.0-PTF)
              CARBOT = CARBO - CARBOR

              ! Stem fraction or ratio to leaf whilst leaf still growing
              IF (SWFRX.GT.0.0.AND.SWFRN.GT.0.0) THEN
                ! Increases linearly between specified limits
                SWFR = CSYVAL (LNUM,SWFRNL,SWFRN,SWFRXL,SWFRX)
              ELSE
                IF (CUMDU+DU.GT.SGPHASEDU(1)) THEN
                  ! If stem growth started
                  IF (CUMDU.LT.LGPHASEDU(2).AND.
     &              ABS(LGPHASEDU(2)-SGPHASEDU(1))>1.E-6) THEN 
                    ! Increases linearly from start stem to end leaf
                    IF (CUMDU+DU.LT.LGPHASEDU(2)) THEN
                      SWFR =  AMAX1(0.0,AMIN1(1.0,
     &                 (CUMDU+DU/2.0-SGPHASEDU(1))/
     &                 (LGPHASEDU(2)-SGPHASEDU(1))))
                    ELSE
                      ! Adjust for period when only stem growing
                      SWFR = (SWFRPREV+(1.0-SWFRPREV)/2.0)*
     &                 (LGPHASEDU(2)-CUMDU)/DU
                    ENDIF
                  ELSE
                    ! All to stem after end leaf
                    SWFR = 1.0
                  ENDIF
                ELSE  
                  ! Zero before stem growth starts 
                  SWFR = 0.0
                ENDIF 
              ENDIF

              ! Chaff fraction 
              GROCHFR = 0.0
              GROCH = 0.0
              IF (CHPHASE(1).GT.0.0.AND.CHPHASE(1).LT.9000) THEN
               ! Increases linearly from start chaff to end stem
               IF (CUMDU+DU/2.GT.CHPHASEDU(1)) THEN
                 GROCHFR = CHFR * AMAX1(0.0,AMIN1(1.0,
     &           ((CUMDU+DU/2)-CHPHASE(1))/(CHPHASE(2)-CHPHASE(1))))
                ENDIF
              ENDIF

!-----------------------------------------------------------------------
!             Storage root initiation and basic growth
!-----------------------------------------------------------------------
              
              GROSR = 0.0
              IF(PDSRI.GT.0.0) THEN
                SRDAYFR = 0.0
                IF(CUMDU.LT.PDSRI.AND.CUMDU+DU.GE.PDSRI)THEN
                  SRDAYFR = (PDSRI-CUMDU)/DU
                  SRNOPD = INT(SRNOW*((LFWT+STWT+RSWT)+SRDAYFR*CARBOT))
                  IF (SRSTAGE.GE.1.AND.SRSTAGE.LE.SSX) THEN
                   SSDAPFR(SRSTAGE) = FLOAT(DAP) + SRDAYFR
                  ENDIF 
                ELSEIF(CUMDU.GT.PDSRI)THEN
                  SRDAYFR = 1.0
                ENDIF
                IF (SRNOPD.GT.0) GROSR = SRFR*CARBOT * SRDAYFR
              ENDIF

!-----------------------------------------------------------------------
!                Grain set and potential growth
!-----------------------------------------------------------------------

              GROGRP = 0.0
              GROGRPA = 0.0
              ! If in graingrowth phase
              IF(CUMDU+DU.GT.GGPHASEDU(1))THEN
              ! Just entering lag phase
              IF(CUMDU.LT.GGPHASEDU(1).AND.CUMDU+DU.GE.GGPHASEDU(1))THEN
                IF (DU.GE.0.0) THEN
                  ADAYEFR = (GGPHASEDU(1)-CUMDU)/DU
                ELSE 
                  ADAYEFR = 0.0
                ENDIF 
                GNOPD = GNOWS*((LFWT+STWT+RSWT)+ADAYEFR*CARBOT)
                ! Kernel number adjustment based on 20-day avg.stress 
                STRESS20GS = STRESS20N*STRESS20W
                Gnonadj = stress20n+(1.0-stress20n)*(1.0-(gnonf/100.0))
                Gnowadj = stress20w+(1.0-stress20w)*(1.0-(gnowf/100.0))
                ! 100 because is read as percentage
                WRITE(FNUMWRK,*)' '
                WRITE(FNUMWRK,'(A50)')
     &          ' GRAIN SET                                        '
                WRITE(FNUMWRK,'(A35,3F6.1)')
     &          '  20 day stress averages N,H2O,Min ',
     &          stress20n,stress20w,stress20
                WRITE(FNUMWRK,'(A50,2I6)')
     &          '  Grain#/plant before,after N stress adjustment   '
     &          ,NINT(GNOPD),NINT(GNOPD*GNONADJ)    
                WRITE(FNUMWRK,'(A21,F4.1,A1)')
     &          '  (N stress factor = ',gnonf,')'                     
                WRITE(FNUMWRK,'(A50,2I6)')
     &          '  Grain#/plant before,after H2O stress adjustment '
     &          ,NINT(GNOPD*GNONADJ),NINT(GNOPD*GNONADJ*GNOWADJ)    
                WRITE(FNUMWRK,'(A23,F4.1,A1)')
     &          '  (H2O stress factor = ',gnowf,')'                     
                GNOPD = GNOPD * Gnonadj * Gnowadj
                ! Kernel size adjustment when # reduced by stress
                gwtas = gwts*(1.0+(1.0-(gnonadj*gnowadj))*(Gwtcf/100.0))
                WRITE(FNUMWRK,'(A50,2F6.1)')
     &          '  Grain size before,after compensation for stress '
     &           ,gwts,GWTAS
                ! Kernel size adjustment based on plant size
                PLWTGS = ((LFWT+STWT+RSWT)+ADAYEFR*CARBOT)
                IF (PLWTGS.GT.GWTAT) THEN
                  ! LAH Note only 50% reduction in size allowed. 
                  GWTA = GWTAS * 
     &             (1.-AMIN1(0.5,(PLWTGS-GWTAT)*(GWTAA/100.0)))
                   ! 100.0 because read as percentage 
                ELSEIF (PLWTGS.LT.GWTAT) THEN
                  GWTA = GWTAS * 
     &             (1.-AMIN1(0.5,(GWTAT-PLWTGS)*(GWTAA/100.0)))
                   ! 100.0 because read as percentage 
                ELSE
                  GWTA = GWTS
                ENDIF
                WRITE(FNUMWRK,'(A50,2F6.1)')
     &          '  Grain size before,after plant size adjustment   '
     &          ,GWTAS,GWTA
                WRITE(FNUMWRK,'(A50,2F6.1)')
     &          '  Plant size,actual and threshold for effect (g)  '
     &          ,plwtgs,gwtat
                ! Kernel growth rates in lag,linear,end phases
                G2A(1) =(GWTA*GWLFR)/(GGPHASEDU(2)-GGPHASEDU(1))
                G2A(2) =(GWTA*(GWEFR-GWLFR))/(GGPHASEDU(3)-GGPHASEDU(2))
                G2A(3) =(GWTA*(1.0-GWEFR))/(GGPHASEDU(4)-GGPHASEDU(3))
                G2A(0) = G2A(1)* (CUMDU+DU-GGPHASEDU(1))/DU
              ELSEIF
     &        (CUMDU.GT.GGPHASEDU(1).AND.CUMDU+DU.LE.GGPHASEDU(2))THEN
              ! In lag phase
                G2A(0) = G2A(1)
              ELSEIF
     &         (CUMDU.LE.GGPHASEDU(2).AND.CUMDU+DU.GT.GGPHASEDU(2))THEN
              ! Just entering linear phase
                G2A(0) = G2A(1)*(1.0-(CUMDU+DU-GGPHASEDU(2))/DU) 
     &                 + G2A(2)*(CUMDU+DU-GGPHASEDU(2))/DU!
              ELSEIF
     &        (CUMDU.GT.GGPHASEDU(2).AND.CUMDU+DU.LE.GGPHASEDU(3))THEN
              ! In linear phase
                G2A(0) = G2A(2)
              ELSEIF
     &        (CUMDU.LE.GGPHASEDU(3).AND.CUMDU+DU.GT.GGPHASEDU(3))THEN
              ! Just entering ending phase
                G2A(0) = G2A(2)*(1.0-(CUMDU+DU-GGPHASEDU(3))/DU) 
     &                 + G2A(3)*(CUMDU+DU-GGPHASEDU(3))/DU
              ELSEIF
     &       (CUMDU.GT.GGPHASEDU(3).AND.CUMDU+DU.LE.GGPHASEDU(4))THEN
              ! In ending phase
                G2A(0) = G2A(3)
              ELSEIF
     &        (CUMDU.LE.GGPHASEDU(4).AND.CUMDU+DU.GT.GGPHASEDU(4))THEN
              ! Finishing ending phase
                G2A(0) = G2A(3)*(1.0-(CUMDU+DU-GGPHASEDU(4))/DU) 
              ENDIF

              ! Potential grain growth rate overall 
              GROGRP = GNOPD*G2A(0)*0.001*TFGF*DU
 
              ! Grain growth rate as limited by potential or assimilates
              GROGRA = AMIN1(GROGRP,AMAX1(0.,CARBOT))
              GROGRRS = AMIN1(GROGRP-GROGRA,RSWT)
              GROGRPA = GROGRA + GROGRRS

              ! Record days of stress
              IF (CUMDU+DU.LE.PSTART(MSTG).AND.
     &         CUMDU+DU.GT.PSTART(MSTG-1))THEN
                IF (GROGRPA.LT.GROGRP) CARBOLIM = CARBOLIM+1
                IF (TFGF.LT.1.0) THEN
                  TLIMIT = TLIMIT+1
                ENDIF 
              ENDIF
 
              ENDIF
                     
!-----------------------------------------------------------------------
!             Specific leaf area
!-----------------------------------------------------------------------

              ! LAH Not yet implemented
              IF (LAWTR.GT.0.0.AND.LAWTS.GT.0.0) THEN
                TFLAW = 1.0+LAWTR*(TMEAN-LAWTS)
              ELSE
                TFLAW = 1.0
              ENDIF
              ! LAH Not yet implemented
              IF (LAWWR.GT.0.0) THEN
                WFLAW = (1.0-LAWWR)+LAWWR*WFG
              ELSE
                WFLAW = 1.0
              ENDIF

              LAWL(1) = AMAX1(LAWS*LAWFF,LAWS-(LAWS*LAWCF)*(LNUMSG-1))
              LAWL(2) = AMAX1(LAWS*LAWFF,LAWS-(LAWS*LAWCF)*LNUMSG)
              LAWL(1) = LAWL(1) * TFLAW * WFLAW
              LAWL(2) = LAWL(2) * TFLAW * WFLAW

!-----------------------------------------------------------------------
!             Leaf growth
!-----------------------------------------------------------------------

              CARBOLSD = 0.0
              GROLF = 0.0
              GROLFP = 0.0
              GROLS = 0.0
              GROLSP = 0.0
              GROLFRT = 0.0
              GROLFRTN = 0.0
              PLAG = 0.0
              PLAGLF = 0.0
              PLAGT = 0.0
              PLAGTP = 0.0
              PLAGTTEMP = 0.0
              TLAG = 0.0
              TLAGP = 0.0
              LAGEG = 0.0
              
              SNOT = AMAX1(1.0,SNOFX**(INT(rstage)-1))

              ! If in leaf growth phase
              IF (CUMDU.GE.LGPHASEDU(1).AND.CUMDU.LT.LGPHASEDU(2)) THEN
                ! Potential leaf sizes
                !  1.Wheat type calculation
                IF (LNUMSG.LT.LNUMX) THEN
                  IF (LAFSWITCH.LE.0) THEN
                    LAPOTX(LNUMSG+1) = LAPOTX(LNUMSG)*(1.0+LAFV)
                  ELSE  
                    LAPOTX(LNUMSG+1) = LAPOTX(LNUMSG)*(1.0+LAFR)
                  ENDIF
                ENDIF    
                IF (LAPOTX(LNUMSG+1).GT.LAXS) LAPOTX(LNUMSG+1) = LAXS
                !  2.Cassava type calculation
                IF (LAXS.GT.0.0.AND.LAXNO.GT.0.0) THEN
                  IF (LNUMSG+1.LE.INT(LAXNO)) THEN
                    LAPOTX(LNUMSG+1) = AMIN1(LAXS, LA1S + 
     &               LNUMSG*((LAXS-LA1S)/LAXNO))
                  ELSE
                    LAPOTX(LNUMSG+1) = AMAX1(LAFS, LAXS - 
     &               ((LNUMSG+1)-LAXNO)*((LAXS-LAFS)/(LAFNO-LAXNO)))
                  ENDIF
                  ! Adjust for shoot#/tiller
                  !IF (SNOT.GE.1)LAPOTX(LNUMSG+1)=LAPOTX(LNUMSG+1)/SNOT
                  ! Keep track of 'forking';reduce potential>forking
                  !IF (LNUMSG.GT.1.AND.SNOT.GT.SNOTPREV) THEN
                  !  LAPOTX(LNUMSG+1) = LAPOTX(LNUMSG+1)/LAFF
                  !  LNUMFORK = LNUMSG
                  !ENDIF
                ENDIF

                ! Original CROPSIM calculations 
                !   .. changes potential of growing leaf
!               IF (LAFSWITCH.LE.0.0.AND.
!     &          LAFSTDU.GT.0.0.AND.CUMDU+DU.GT.LAFSTDU) THEN
!                 ! Change potential of growing leaf at switch point 
!                 LAFSWITCH = LNUM+LNUMG*((LAFSTDU-CUMDU)/DU)
!                 LAPOTX(LNUMSG) = LAPOTX(LNUMSG-1)
!     &                          + LA1S*LAFV*((LAFSTDU-CUMDU)/DU)
!     &                          + LA1S*LAFR*(1.0-((LAFSTDU-CUMDU)/DU))
!                 LAPOTX(LNUMSG+1) = LAPOTX(LNUMSG)+LA1S*LAFR
!                 LAPOTXCHANGE = LAPOTX(LNUMSG-1)
!     &                        + LA1S*LAFV*(LAFSWITCH-FLOAT(LNUMSG-1))
!     &                        + LA1S*LAFR*(FLOAT(LNUMSG)-LAFSWITCH)
!               ENDIF
                ! Simpler calculations from CROPSIM-CERES
                IF (RSTAGE.GE.LAFST) THEN
                  IF(LNUMSG.GT.0 .AND. LAFSWITCH.LE.0.0) THEN
                  LAFSWITCH = LNUMSG
                  WRITE(fnumwrk,*) '    '
                  WRITE(fnumwrk,*) ' Leaf size increment factor changed'
                  WRITE(fnumwrk,*) 
     &             '  Leaf number             ',lafswitch
                  Lapotxchange = lapotx(lnumsg)
                  WRITE(fnumwrk,*)
     &             '  Leaf potential size     ',
     &              Lapotxchange
                    LAPOTX(LNUMSG+1) = LAPOTX(LNUMSG)*(1.0+LAFR)
                  WRITE(fnumwrk,*)
     &             '  Next leaf potential size',
     &              Lapotx(lnumsg+1)
                  ENDIF     
                ENDIF
                
                ! If it is the final leaf,next leaf potential = 0
                IF (FLN.GT.0.0.AND.LNUMSG.EQ.INT(FLN+1)) THEN
                  LAPOTX(LNUMSG+1) = 0.0
                ENDIF

                ! Leaf area increase (no assim) - main shoot (tiller 1)
                LNUMNEED = FLOAT(INT(LNUM+1)) - LNUM
                IF (ABS(LNUMNEED).LE.1.0E-6) LNUMNEED = 0.0
                
                DO L = MAX(1,LNUMSG-INT(LLIFG)),LNUMSG
                  LLIFEG(L) = AMIN1(AMAX1(0.0,LLIFG-LAGEP(L)),LNUMG)
                  PLAGLF(L) = LAPOTX(L)*LLIFEG(L)/LLIFG
     &                      * AMIN1(WFG,NFG)*TFG
                  IF (LNUMG*EMRGFR.GT.0.0) THEN
                    DGLF(L) = DGLF(L)+LLIFEG(L)/LNUMG*EMRGFR
                  ENDIF
                  WFLF(L) = WFLF(L)+WFG*LLIFEG(L)/LLIFG
                  WFLFP(L) = WFLFP(L)+WFP*LLIFEG(L)/LLIFG
                  NFLF(L) = NFLF(L)+NFG*LLIFEG(L)/LLIFG
                  NFLFP(L) = NFLFP(L)+NFP*LLIFEG(L)/LLIFG
                  TFLF(L) = TFLF(L)+TFG*LLIFEG(L)/LLIFG
                  PLAG(1) = PLAG(1) + LAPOTX(L)
     &                    * AMIN1(LLIFEG(L),LNUMNEED)/LLIFG
     &                    * AMIN1(WFG,NFG)*TFG
                  PLAG(2) = AMAX1(0.0,PLAG(2)+(PLAGLF(L)-PLAG(1)))
                ENDDO

                ! New leaf
                IF (LNUMSG.LT.LNUMX) THEN
                L = LNUMSG + 1
                LLIFEG(L) = AMAX1(0.0,(LNUMG-LNUMNEED))
                PLAGLF(L)=LAPOTX(L)*LLIFEG(L)/LLIFG
     &                   * AMIN1(WFG,NFG)*TFG
                IF (LNUMG.GT.0.0) DGLF(L) = DGLF(L)+LLIFEG(L)/LNUMG
                WFLF(L) = WFLF(L)+WFG*LLIFEG(L)/LLIFG
                WFLFP(L) = WFLFP(L)+WFP*LLIFEG(L)/LLIFG
                NFLF(L) = NFLF(L)+NFG*LLIFEG(L)/LLIFG
                NFLFP(L) = NFLFP(L)+NFP*LLIFEG(L)/LLIFG
                TFLF(L) = TFLF(L)+TFG*LLIFEG(L)/LLIFG
                PLAG(2) = PLAG(2) + PLAGLF(L)
                ENDIF

                ! Potential leaf area increase - all tillers
                TLAGP(1) = PLAG(1)+PLAG(2)
                PLAGTP(1) = PLAG(1)
                PLAGTP(2) = PLAG(2)
                DO L = 2,INT(TNUM+2) ! L is tiller cohort,main=cohort 1
                  IF (TNUM-FLOAT(L-1).GT.0.0) THEN
                    TILIFAC = 1.0
                    IF (TILIP.GT.0) TILIFAC =
     &                           AMIN1(1.0,(LNUM-TILBIRTHL(L))/TILIP)
                    PLAGTP(1) = PLAGTP(1)+PLAG(1)*TGR(L)*TILIFAC
     &                       * AMAX1(0.,AMIN1(FLOAT(L),TNUM)-FLOAT(L-1))
                    PLAGTP(2) = PLAGTP(2)+PLAG(2)*TGR(L)*TILIFAC
     &                       * AMAX1(0.,AMIN1(FLOAT(L),TNUM)-FLOAT(L-1))
                    TLAGP(L) = (PLAG(1)+PLAG(2))*TGR(L)*TILIFAC
     &                       * AMAX1(0.,AMIN1(FLOAT(L),TNUM)-FLOAT(L-1))
                  ENDIF
                ENDDO

                ! Potential leaf weight increase.
                IF (LAWL(1).GT.0.0 .AND. LAWL(2).GT.0.0)
     &           GROLFP = ( PLAGTP(1)/LAWL(1) + PLAGTP(2)/LAWL(2))
     &                  / (1.0-LSHFR)

                ! Potential leaf+stem weight increase.
                IF (SWFR.GT.0.0.AND.SWFR.LT.1.0) THEN
                  GROLSP = GROLFP * (1.0 + SWFR/(1.0-SWFR))
                ELSE
                  GROLSP = GROLFP
                ENDIF

                IF (GROLSP.GT.0.0) THEN
                  ! Leaf+stem weight increase from assimilates
                  GROLS = AMAX1(0.,AMIN1(GROLSP,CARBOT-GROGRPA-GROSR))
                
                  IF (GROLS.LT.GROLSP) THEN
                    ! Leaf weight increase from seed reserves
                    ! LAH May need to restrict seed use.To use by roots?
                    CARBOLSD = AMIN1((GROLSP-GROLS),SEEDRSAV)
                    SEEDRSAV = SEEDRSAV - CARBOLSD
                    GROLS = GROLS + CARBOLSD
                    IF (LAI.LE.0.0.AND.CARBOLSD.LE.0.0
     &                            .AND.SEEDRSAV.LE.0.0          
     &                            .AND.ESTABLISHED.NE.'Y') THEN
                      CFLFAIL = 'Y'
                      WRITE (Message(1),'(A41)')
     &                 'No seed reserves to initiate leaf growth '
                      WRITE (Message(2),'(A33,F8.3,F6.1)')
     &                '  Initial seed reserves,seedrate ',seedrsi,sdrate
                      WRITE (Message(3),'(A33,F8.3,F6.1)')
     &                '  Reserves %,plant population    ',sdrsf,pltpop 
                      CALL WARNING(3,'CSCRP',MESSAGE)
                    ENDIF
                  ENDIF
                  ! Leaf weight increase from plant reserves
                  GROLFRS = 0.0
                  IF (GROLS.LT.GROLSP) THEN
                    GROLFRS = AMIN1(RSWT*RSUSE,GROLSP-GROLS)
                    GROLS = GROLS+GROLFRS
                  ENDIF
                  ! Leaf weight increase from roots (eg.,after winter)
                  GROLFRT = 0.0
                  GROLFRTN = 0.0
                  IF (GROLS.LT.GROLSP.AND.SHRTD.LT.1.0.AND.
     &                RTUFR.GT.0.0.AND.ESTABLISHED.EQ.'Y') THEN
                    GROLFRT = AMIN1(RTWT*RTUFR,(GROLSP-GROLS))
                    IF (ISWNIT.NE.'N') THEN
                      GROLFRTN = GROLFRT * RANC
                    ELSE
                      GROLFRTN = 0.0
                    ENDIF  
                    WRITE(Message(1),
     &               '(A16,A12,F3.1,A8,F7.4,A7,F7.4,A9,F7.4)')
     &               'Roots -> leaves ',
     &               ' Shoot/root ',shrtd,
     &               ' Grolsp ',grolsp,' Grols ',grols,
     &               ' Grolfrt ',grolfrt
                    CALL WARNING(1,'CSCRP',MESSAGE)
                    GROLS = GROLS + GROLFRT
                  ENDIF

                  IF ((GROLSP).GT.0.0) THEN
                    GROLF = GROLS * GROLFP/GROLSP
                  ELSE  
                    GROLF = 0.0
                  ENDIF

                  ! Assimilate factor overall for today
                  !IF (GROLS.LT.(GROLSP*(1.0-LAWFF))) THEN
                  IF (GROLS.LT.GROLSP.AND.GROLSP.GT.0.0) THEN
                    !AFLF(0) = GROLS/(GROLSP*(1.0-LAWFF))
                    AFLF(0) = GROLS/GROLSP
                  ELSE
                    AFLF(0) = 1.0
                  ENDIF

                  ! Assimilate factor average for each leaf
                  DO L = MAX(1,LNUMSG-(INT(LLIFG)+1)),LNUMSG+1
                    IF (LNUMSG.LT.LNUMX) THEN
                      AFLF(L) = AFLF(L)+AFLF(0)*(LLIFEG(L)/LLIFG)
                      PLAGLF(L) = PLAGLF(L) * AFLF(0)
                    ENDIF  
                  ENDDO

                  ! Actual leaf cohort expansion
                  PLAGT(1) = PLAGTP(1)*AFLF(0)
                  PLAGT(2) = PLAGTP(2)*AFLF(0)

                  ! Actual leaf area growth - each tiller
                  DO L = 1,INT(TNUM+1)
                    TLAG(L) = TLAGP(L)*
     &               (PLAGT(1)+PLAGT(2))/(PLAGTP(1)+PLAGTP(2))
                  ENDDO

                ENDIF

              ENDIF
                          
!-----------------------------------------------------------------------
!             Stem and chaff growth
!-----------------------------------------------------------------------

              GROSTP = 0.0
              GROST = 0.0
              STAIG = 0.0
              STAIS = 0.0
              ! Potential stem weight increase.
              IF (SWFR.LT.1.0) THEN
                GROSTP = GROLFP * SWFR/(1.0-SWFR)
                GROSTPSTORE = AMAX1(GROLFP,GROSTPSTORE)
              ELSE  
                GROSTP = GROSTPSTORE
                ! LAH May need to change GROSTP as progress thru phase
              ENDIF
              IF (CUMDU+DU.LE.LGPHASEDU(2)) THEN  
                IF (GROLFP+GROSTP.GT.0.0)
     &           GROST = GROLS * GROSTP/(GROLFP+GROSTP)
              ELSE
                IF (CUMDU+DU.GT.LGPHASEDU(2).AND.
     &           CUMDU+DU.LE.GGPHASEDU(1))
     &           GROST = AMAX1(0.,CARBOT-GROGRPA-GROSR)*(1.0-RSFRS)
                 ! LAH RSFRS is the fraction of stem growth to reserves
                 ! May need to have this change as stem growth proceeds
              ENDIF
              ! Chaff (In balance with stem growth)
              GROCH = GROST * GROCHFR
              GROST = GROST * (1.0-GROCHFR)
              ! Visible stem
              STVSTG = 0.0
              STVSTGDU = 0.0
              IF (CUMDU.LE.STVSTGDU.AND.CUMDU+DU.GT.STVSTGDU) THEN
                STVWTG =
     &           GROST*AMAX1(0.0,(AMIN1(1.0,(CUMDU+DU-STVSTG)/DU)))
              ELSEIF (CUMDU.GT.STVSTGDU) THEN
                STVWTG = GROST
              ENDIF

              IF (FSDU.LE.0.0) THEN     ! Visible stem growing
                STAIG = STVWTG*SAWS*PLTPOP*0.0001
              ELSE    ! Visible stem senescing
                IF (CUMDU.LE.FSDU) THEN
                  STAIG = STVWTG*SAWS*PLTPOP*0.0001
                  STAISS = STAI + STAIG
                  IF ((PSTART(MSTG)-FSDU).GT.0.0) THEN
                    STAIS = STAISS*(CUMDU+DU-FSDU)/(PSTART(MSTG)-FSDU)
                    IF (STAIS.LT.1.0E-6) STAIS = 0.0
                  ELSE
                    STAIS = 0.0
                  ENDIF 
                ELSE
                  STAIG = 0.0
                  IF ((PSTART(MSTG)-FSDU).GT.0.0) THEN
                    STAIS = AMIN1(STAI,STAISS*DU/(PSTART(MSTG)-FSDU))
                  ELSE
                    STAIS = 0.0
                  ENDIF  
                ENDIF
                STAIS = AMIN1(STAI,STAISS*SENSTFR)
              ENDIF

!-----------------------------------------------------------------------
!             Reserves growth
!-----------------------------------------------------------------------

              CARBORRS = 0.0   ! Not calculated until later
              GRORS = 
     &         CARBOT+CARBOLSD+GROLFRT-GROLF-GROST-GROGRPA-GROSR
              ! Check if RSWT -ve (Generally v.small computer error)
              ! If so adjust growth aspect
              RSWTTMP = RSWT+GRORS+GRORSGR-SENRS-RSWPH-CARBORRS
              IF (RSWTTMP.LT.-1.E-6) THEN ! Reduce growth 
                GROST = AMAX1(0.0,GROST-ABS(RSWTTMP))
                GRORS =CARBOT+CARBOLSD+GROLFRT-GROLF-GROST-GROGRPA-GROSR
                RSWTTMP = RSWT+GRORS+GRORSGR-SENRS-RSWPH-CARBORRS
                GROLF = AMAX1(0.0,GROLF-ABS(RSWTTMP))
                GRORS =CARBOT+CARBOLSD+GROLFRT-GROLF-GROST-GROGRPA-GROSR
                RSWTTMP = RSWT+GRORS+GRORSGR-SENRS-RSWPH-CARBORRS
                GROGRPA = AMAX1(0.0,GROGRPA-ABS(RSWTTMP))
                GRORS =CARBOT+CARBOLSD+GROLFRT-GROLF-GROST-GROGRPA-GROSR
                RSWTTMP = RSWT+GRORS+GRORSGR-SENRS-RSWPH-CARBORRS
                GROSR = AMAX1(0.0,GROSR-ABS(RSWTTMP))
               ENDIF
              GRORS = CARBOT+CARBOLSD+GROLFRT-GROLF-GROST-GROGRPA-GROSR

              GRORSPRM = 0.0
              GRORSPM = 0.0
              IF (LSENEDU.GT.PSTART(MSTG).AND.
     &         CUMDU+DU.GT.PSTART(MSTG).AND.
     &         CUMDU.LT.PSTART(MSTG)) THEN
                GRORSPRM =
     &           (CARBOT+CARBOLSD-GROLF-GROST-GROSR)*TIMENEED-GROGRPA
                GRORSPM = GRORS - GRORSPRM
              ELSEIF (CUMDU.GE.PSTART(MSTG)) THEN
                GRORSPM = CARBOPM
              ENDIF

              ! Send some reserves to ROOT if conc too great
              CARBORRS = 0.0
              IF (CUMDU.LT.SGPHASEDU(1).OR.HPROD.EQ.'SR') THEN
                ! First determine need (Cannot go below 0)
                ! Below zero could arise if lots tissue senescence,kill
                RSNEED = AMAX1(0.0,
                ! NB.Reserve conc based on weight including reserves
     &         (((RSCS/100.0)*(LFWT+STWT-SENLFG-SENLFGRS+GROLF+GROST))/
     &         ((1.-RSCS/100.0))- RSWT))
                ! Then calculate amount to send to roots
                IF (RSNEED.GE.0.0.AND.RSNEED.LT.GRORS) THEN
                  CARBORRS = AMAX1(0.0,GRORS - RSNEED)
                ELSEIF (RSNEED.LT.0.0.AND.ABS(RSNEED).GT.GRORS) THEN
                  CARBORRS = ABS(RSNEED) - GRORS
                ENDIF  
              ENDIF

!-----------------------------------------------------------------------
!             Tiller number increase
!-----------------------------------------------------------------------

              TNUMG = 0.0
              IF (LNUMEND.GT.TI1LF.AND.(LNUMEND-LNUM).GT.0.0) THEN
                IF (LNUMEND.LT.ti1lf+3) THEN    ! Fibonacci factors
                 tnumiff=1.0
                ELSEIF(LNUMEND.GE.ti1lf+3 .AND. LNUMEND.LT.ti1lf+4) THEN
                 tnumiff=1.5
                ELSEIF(LNUMEND.GE.ti1lf+4 .AND. LNUMEND.LT.ti1lf+5) THEN
                 tnumiff = 1.5     ! tnumiff=3.0
                ELSEIF(LNUMEND.GE.ti1lf+5 .AND. LNUMEND.LT.ti1lf+6) THEN
                  tnumiff = 1.5     ! tnumiff=4.0
                ELSEIF(LNUMEND.GE.ti1lf+6 .AND. LNUMEND.LT.ti1lf+7) THEN
                 tnumiff = 1.5     ! tnumiff=6.0
                ENDIF
                IF ((CUMDU+DU).LT.TILPEDU) THEN
                  TNUMG = DULF/PHINT * TNUMIFF * (AMIN1(WFT,NFT))! cscer
                ELSE
                  TNUMG = 0.0
                ENDIF  
              ENDIF
              ! Tillering factor
              TNUMG = TNUMG * TIFAC

!-----------------------------------------------------------------------
!             Height growth
!-----------------------------------------------------------------------

              CANHTG = 0.0
              IF (CROP.EQ.'WH'.OR.CROP.EQ.'BA') THEN
                IF (RSTAGE.LT.7.0) CANHTG = SERX*DU
              ELSE  
               IF (CUMDU.GT.SGPHASEDU(1).AND.CUMDU.LT.SGPHASEDU(2)) THEN
                CANHTG = SERX*DU
               ELSEIF (CUMDU.LE.SGPHASEDU(1).AND.GESTAGE.GE.1.0) THEN
                IF (TT.GT.0.0) CANHTG = 0.5
               ENDIF
              ENDIF 

!=======================================================================
            ENDIF ! End of above-ground growth (after emerged) section
!=======================================================================

!-----------------------------------------------------------------------
!           Root growth and respiration
!-----------------------------------------------------------------------

            RTWTG = 0.0
            RTRESP = 0.0
            SRWTGRS = 0.0
            IF (HPROD.EQ.'SR') THEN
              SRWTGRS = CARBORRS*SRDAYFR
              RTWTG = (CARBOR+CARBORRS*
     &                  (1.0-SRDAYFR)+SEEDRSAVR)*(1.0-RRESP)
              RTRESP = (CARBOR+CARBORRS*(1.0-SRDAYFR)+SEEDRSAVR)*RRESP
            ELSE
              RTWTG = (CARBOR+CARBORRS+SEEDRSAVR)*(1.0-RRESP)
              RTRESP = (CARBOR+CARBORRS+SEEDRSAVR)*RRESP
              SRWTGRS = 0.0
            ENDIF

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
!     &                 * 1.0+AMAX1(0.0,RDGAF*(10.0-WUPR))
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
              SENRTGGF = 0.0
              DO L = 1, NLAYRROOT
                RTWTSL(L) = RTWTL(L)*(RSEN/100.0)*TT/STDAY 
                ! LAH Temperature effect above is not from soil temp
                IF (RTWT.GT.0.0) RTWTUL(L) = RTWTL(L)*GROLFRT/RTWT
                SENRTG = SENRTG + RTWTSL(L)
                IF (ISWNIT.NE.'N') THEN
                  RTNSL(L) = RTWTSL(L)*RANC
                ELSE
                  RTNSL(L) = 0.0
                ENDIF  
              ENDDO
              ! Following for checking purposes only
              IF (CUMDU.GE.SGPHASEDU(2).AND.CUMDU.LT.PSTART(MSTG))
     &         SENRTGGF = SENRTG

              ! Root weight growth by layer
              TRLDF = 0.0
              DO  L = 1, NLAYRROOT
                TRLDF = TRLDF + RLDF(L)
              END DO
              IF (TRLDF.GT.0.0) THEN
                DO  L = 1, NLAYRROOT
                  RTWTGL(L) = (RLDF(L)/TRLDF)*(RTWTG)
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
!           Nitrogen movement and uptake
!-----------------------------------------------------------------------

            LNGU = 0.0
            SNGU = 0.0
            SNGUL = 0.0
            GRAINNGU = 0.0
            GRAINNGL = 0.0
            GRAINNGR = 0.0
            GRAINNGS = 0.0

            IF (ISWNIT.NE.'N') THEN

              ANDEM = 0.0
              RNDEM = 0.0
              LSNDEM = 0.0
              SRNDEM = 0.0
              SEEDNUSE = 0.0
              SEEDNUSE2 = 0.0
              RSNUSE = 0.0

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

              ! Grain N demand
              GRAINNDEM = 0.0
              IF (GNOPD.GT.0.0 .AND. CUMDU.LT.PSTART(MSTG)) GRAINNDEM =
     &         AMIN1(GROGRPA*(GRNMX/100.0),TFGN*GROGRP*(GRNS/100.))

              ! Leaf,stem,root,and storage root N demand
              LNDEM = GROLF*LNCX +
     &            (LFWT-SENLFG-SENLFGRS)*AMAX1(0.0,NTUPF*(LNCX-LANC)) -
     &            GROLFRTN
              SNDEM = AMAX1(0.0,GROST)*SNCX +
     &              (STWT-SENSTG)*AMAX1(0.0,NTUPF*(SNCX-SANC))
              SNDEMMIN =AMAX1(0.,(GROST*SNCM-(STWT-SENSTG)*(SNCM-SANC)))
              LSNDEM = LNDEM + SNDEM
              RNDEM = RTWTG*RNCX + 
     &              (RTWT-SENRTG-GROLFRT)*AMAX1(0.0,NTUPF*(RNCX-RANC))
              SRNDEM = (GROSR+SRWTGRS)*SRNS
              
              ! Seed use if no roots
              ! N use same % of initial as for CH20,if needed.
              IF (RTWT.LE.0.0) THEN
                SEEDNUSE = AMAX1(0.0,
     &           AMIN1(SEEDN,LSNDEM+RNDEM,SEEDNI/SDDUR*(TT/STDAY)))
              ELSE
                ! Some use of seed (0.5 need) even if may not be needed
                SEEDNUSE = AMAX1(0.0,
     &          AMIN1(SEEDN,0.5*(LSNDEM+RNDEM),SEEDNI/SDDUR*(TT/STDAY)))
              ENDIF
              
              ! Reserves used before uptake
              RSNUSE = AMIN1(GRAINNDEM+LSNDEM+RNDEM+SRNDEM,RSN)

              ! N uptake needed 
              ANDEM = PLTPOP*10.0*
     &          (GRAINNDEM+LSNDEM+RNDEM+SRNDEM-SEEDNUSE-RSNUSE)

              ! Uptake
              
              IF (MERNU.EQ.'CSM') THEN
                ! Original from CSM with some 'modification'.  
                ! RNUMX = RTNO3,RTNH4 = N uptake/root length (mgN/cm,.006)
                ! RNO3U,RNH4  = Nitrogen uptake (kg N/ha)
                !RNUMX = 0.006    
                WFNU = 1.0
                NUPAP = 0.0
                RNO3U = 0.0
                RNH4U = 0.0
                ! LAH WORKING
                TVR1 = - AMAX1(0.01,(0.20-NCNU*0.004))
                ! TVR1 originally set at 0.08,which comes from NCNU=30
                TVR1 = -0.08  ! May 2011
                DO L=1,NLAYR
                  IF (RLV(L) .GT. 0.0) THEN
                    NLAYRROOT = L
                    FNH4 = 1.0-EXP(TVR1 * NH4LEFT(L))
                    FNO3 = 1.0-EXP(TVR1 * NO3LEFT(L))
                   ! LAH Note that no uptake below 0.04
                    IF (FNO3 .LT. 0.04) FNO3 = 0.0
                    IF (FNO3 .GT. 1.0)  FNO3 = 1.0
                    IF (FNH4 .LT. 0.04) FNH4 = 0.0
                    IF (FNH4 .GT. 1.0)  FNH4 = 1.0
                    IF (SW(L) .LE. DUL(L)) THEN
                      IF (WFNUU-WFNUL.GT.0.0) WFNU = 
     &                AMIN1(1.0,AMAX1(0.0,(SWP(L)-WFNUL)/(WFNUU-WFNUL)))
                    WFNU = (SW(L) - LL(L)) / (DUL(L) - LL(L)) ! May 2011
                    ELSE
                      WFNU = 1.0-(SW(L)-DUL(L))/(SAT(L)-DUL(L))
                      ! Wet soil effect not implemented
                      WFNU = 1.0
                    ENDIF
                    IF (WFNU.LT.0.0) WFNU = 0.0
                    !RNO3U(L) = RLV(L)*WFNU*DLAYR(L)*100.0 *RNUMX*FNO3
                    RFAC = RLV(L) * WFNU * WFNU * DLAYR(L) * 100.0
                    RNO3U(L) = RFAC * FNO3 * RTNO3
                    RNH4U(L) = RFAC * FNH4 * RTNH4
                    RNO3U(L) = MAX(0.0,RNO3U(L))
                    RNH4U(L) = MAX(0.0,RNH4U(L))
                    NUPAP = NUPAP + RNO3U(L) + RNH4U(L) !kg[N]/ha
                    NUPAPCSM1 = NUPAP
                  ENDIF
                ENDDO
                ! Below as in CSCER
                ! Calculate potential N uptake in soil layers with roots
                RNO3U = 0.0
                RNH4U = 0.0
                NUPAP = 0.0
                NUPAPCSM = 0.0
                DO L=1,NLAYR
                 IF (RLV(L) .GT. 1.E-6) THEN
                  FNH4 = 1.0 - EXP(-0.08 * NH4LEFT(L))
                  FNO3 = 1.0 - EXP(-0.08 * NO3LEFT(L))
                  IF (FNO3 .LT. 0.04) FNO3 = 0.0  
                  IF (FNO3 .GT. 1.0)  FNO3 = 1.0
                  IF (FNH4 .LT. 0.04) FNH4 = 0.0  
                  IF (FNH4 .GT. 1.0)  FNH4 = 1.0
                  SMDFR = (SW(L) - LL(L)) / (DUL(L) - LL(L))
                  IF (SMDFR .LT. 0.0) THEN
                    SMDFR = 0.0
                  ENDIF
                  IF (SW(L) .GT. DUL(L)) THEN
                    SMDFR = 1.0 - (SW(L) - DUL(L)) / (SAT(L) - DUL(L))
                    ! Wet soil effect not implemented
                    SMDFR = 1.0
                  ENDIF
                  RFAC = RLV(L) * SMDFR * SMDFR * DLAYR(L) * 100.0
                  !  RLV = Rootlength density (cm/cm3)
                  !  SMDFR = relative drought factor
                  !  RTNO3 + RTNH4 = Nitrogen uptake / root length (mg N/cm)  
                  !  RNO3U + RNH4  = Nitrogen uptake (kg N/ha)
                  RNO3U(L) = RFAC * FNO3 * RTNO3
                  RNH4U(L) = RFAC * FNH4 * RTNH4
                  RNO3U(L) = MAX(0.0,RNO3U(L))
                  RNH4U(L) = MAX(0.0,RNH4U(L))
                  !TRNU = TRNU + RNO3U(L) + RNH4U(L) !kg[N]/ha
                  NUPAP = NUPAP + RNO3U(L) + RNH4U(L) !kg[N]/ha
                  NUPAPCSM = NUPAP
                 ENDIF
                ENDDO
                ! To choose which one!
                nupap = nupapcsm
              ENDIF
              
              IF (MERNU.EQ.'CRP') THEN
                WFNU = 0.0
                NUPAP = 0.0
                RNO3U = 0.0
                RNH4U = 0.0
                ! Potential N supply in soil layers with roots (NUPAP)
                DO L = 1, NLAYR
                  IF (RLV(L).GT.0.0) THEN
                    NLAYRROOT = L
                    IF (SW(L).LE.DUL(L)) THEN
                      IF (WFNUU-WFNUL.GT.0.0) WFNU = 
     &                AMIN1(1.0,AMAX1(0.0,(SWP(L)-WFNUL)/(WFNUU-WFNUL)))
                    ELSE
                      ! LAH Wet soil effect. Not implemented because 
                      ! problem when irrigated at planting/emergence
                      WFNU = AMAX1
     &                 (0.,AMIN1(1.,(SW(L)-DUL(L))/(SAT(L)-DUL(L))))
                      WFNU = 1.0
                    ENDIF
                    NH4FN = AMIN1(1.0,AMAX1(0.0,NH4LEFT(L)/NCNU))
                    NO3FN = AMIN1(1.0,AMAX1(0.0,NO3LEFT(L)/NCNU))
                    RLFN =  AMAX1(0.,AMIN1(1.,RLV(l)/(RLFNU/1000.0)))
                    ! The 1000 is because read as cm/dm3,not cm/cm3
                    ! Original also had SHF as a limitant
                    RNO3U(L) = NO3LEFT(L)/FAC(L)*AMIN1(RLFN,WFNU,NO3FN)
                    RNH4U(L) = NH4LEFT(L)/FAC(L)*AMIN1(RLFN,WFNU,NH4FN)
                    NUPAP = NUPAP + RNO3U(L) + RNH4U(L)
                    NUPAPCRP = NUPAP
                  ENDIF
                END DO
              ENDIF

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
                UNO3(L) = MAX(0.0,MIN (UNO3(L),SNO3(L)))
                UNH4(L) = MAX(0.0,MIN (UNH4(L),SNH4(L)))
                NUPAD = NUPAD + UNO3(L) + UNH4(L)
              END DO
              IF (PLTPOP > 1.E-6) THEN
                NUPD = NUPAD/(PLTPOP*10.0)
              ELSE
                NUPD = 0.
              ENDIF

              SEEDNUSE2 = 0.0
              ! Seed use after using reserves and uptake
              ! (Assumes all seed gone by time of grain filling)
              IF (RTWT.GT.0.0.AND.ISWNIT.NE.'N') THEN
                SEEDNUSE2 = AMAX1(0.0,AMIN1(SEEDN,GRAINNDEM+LSNDEM+
     &           RNDEM-RSNUSE-SEEDNUSE-NUPD,SEEDNI/SDDUR*(TT/STDAY)))
              ELSE
                SEEDNUSE2 = 0.0
              ENDIF
              SEEDNUSE = SEEDNUSE + SEEDNUSE2

              ! Distribute N to grain,leaves,stem,root,and storage root
              LNUSE = 0.0
              SNUSE = 0.0
              RNUSE = 0.0
              SRNUSE = 0.0
              NULEFT = SEEDNUSE+RSNUSE+NUPD
              NULEFTL = 0.0              ! 1.For grain
              GRAINNGU = AMIN1(NULEFT,GRAINNDEM)
              NULEFT = NULEFT - GRAINNGU
              ! 2.For leaf at minimum
              LNUSE(1) = AMIN1(NULEFT,GROLF*LNCM)
              NULEFT = NULEFT - LNUSE(1)
              ! 3.For root at minimum
              RNUSE(1) = AMIN1(NULEFT,RTWTG*RNCM)
              NULEFT = NULEFT - RNUSE(1)
              ! 4.For stem at minimum     
              SNUSE(1) = AMIN1(NULEFT,GROST*SNCM)
              ! ? Storage root at minimum - will ultimately need
              NULEFT = NULEFT - SNUSE(1)
              ! 5.For leaf growth and topping-up
              LNUSE(2) = AMIN1(NPTFL*NULEFT,LNDEM-LNUSE(1))
              NULEFT = NULEFT - LNUSE(2)
              ! 6.For distribution between root and stem
              IF (NULEFT.GT.0.0.AND.
     &         SNDEM-SNUSE(1)+RNDEM-RNUSE(1)+SRNDEM.GT.0.0) THEN
                IF (NULEFT.GT.
     &           (SNDEM-SNUSE(1))+(RNDEM-RNUSE(1))+SRNDEM) THEN
                  NULEFTL = NULEFT-
     &             ((SNDEM-SNUSE(1))+(RNDEM-RNUSE(1))+SRNDEM)
                  LNUSE(3) = LNUSE(3) + NULEFTL
                  SNUSE(2) = SNDEM-SNUSE(1)
                  RNUSE(2) = RNDEM-RNUSE(1)
                  SRNUSE(2) = SRNDEM
                ELSE 
                  SNUSE(2) = NULEFT * (SNDEM-SNUSE(1))/
     &                   ((SNDEM-SNUSE(1))+(RNDEM-RNUSE(1))+SRNDEM)
                  RNUSE(2) = NULEFT * (RNDEM-RNUSE(1))/
     &                   ((SNDEM-SNUSE(1))+(RNDEM-RNUSE(1))+SRNDEM)
                  NULEFT = NULEFT - SNUSE(2) - RNUSE(2) 
                  SRNUSE(2) = NULEFT      
                  NULEFT = NULEFT - SRNUSE(2)
                ENDIF   
              ENDIF   
              LNUSE(0) = LNUSE(1) + LNUSE(2) + LNUSE(3)
              SNUSE(0) = SNUSE(1) + SNUSE(2) + SNUSE(3)
              RNUSE(0) = RNUSE(1) + RNUSE(2) + RNUSE(3)
              SRNUSE(0) = SRNUSE(1) + SRNUSE(2) + SRNUSE(3)

              ! N Pools available for re-mobilization
              ! (Labile N increases during grain fill)
              IF (RSTAGE.GE.8.0.AND.RSTAGE.LE.9.0) THEN
                NUSELIM = AMIN1(1.0,RSTAGE-8.0)
                NUSEFAC = AMAX1(NUSELIM,(NLABPC/100.0))
              ELSE  
                NUSEFAC = NLABPC/100.0
              ENDIF
              NPOOLR = AMAX1 (0.0,
     &         ((RTWT-SENRTG)*(RANC-RNCM)*NUSEFAC))
              NPOOLL = AMAX1 (0.0, 
     &            ((LFWT-SENLFG-SENLFGRS)*(LANC-LNCM)*NUSEFAC))
              NPOOLS = AMAX1 (0.0,
     &         ((STWT-SENSTG)*(SANC-SNCM)*NUSEFAC))

              ! Move N to grain from roots and tops if necessary
              GRAINNGR = 0.0
              GRAINNGL = 0.0
              GRAINNGS = 0.0
              GRAINNDEMLSR = AMAX1(0.0,(GRAINNDEM-GRAINNGRS-GRAINNGU))
              ! Draw N from roots,then stems,then leaves.
               GRAINNGR = AMIN1(NPOOLR,GRAINNDEMLSR)
               GRAINNGS = AMIN1(NPOOLS,GRAINNDEMLSR-GRAINNGR)
               GRAINNGL = AMIN1(NPOOLL,
     &                            GRAINNDEMLSR-GRAINNGR-GRAINNGS)

              ! Move N to stem from leaves if necessary.
              STEMNGL = 0.0
              IF (CUMDU.LT.SGPHASEDU(2)) THEN
                ! To keep concentration at minimum
!     &              STEMNGL = AMAX1(0.0,
!    &                    AMIN1(SNDEMMIN-RSNUSES-SNGU,NPOOLL-GRAINNGL))
                ! To avoid too rapid fall in conc.when stem very small
                IF (SNUSE(0).LT.((SANC-0.001)*(STWT+GROST)-STEMN)) THEN 
                  STEMNGL = AMIN1(LFWT*LNCM,
     &            ((SANC-0.0005)*(STWT+GROST)-STEMN)-SNUSE(0))
                ENDIF
              ENDIF  

            ENDIF

!-----------------------------------------------------------------------
!           Actual grain growth
!-----------------------------------------------------------------------

            GRORSGR = 0.0
            IF (ISWNIT.EQ.'N') THEN
              GROGR = GROGRPA
            ELSE
              IF (GRNMN.LE.0.0) THEN
                GROGR = GROGRPA
              ELSE
                ! Minimum grain N% control
                GRWTTMP = GRWT + GROGRPA
                GRAINNTMP = GRAINN +
     &           (GRAINNGU+GRAINNGR+GRAINNGL+GRAINNGS+GRAINNGRS)
                IF (GRWTTMP > 1.E-6 .AND. 
     &           GRAINNTMP/GRWTTMP*100.0 .LT. GRNMN) THEN
                  GRWTTMP = GRAINNTMP*(100.0/GRNMN)
                  GROGR = GRWTTMP - GRWT
                  GRORSGR = GROGRPA - GROGR
                  NLIMIT = NLIMIT + 1
                ELSE
                  GROGR = GROGRPA
                ENDIF
              ENDIF
            ENDIF

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
            ! LAH working here 
            SENWALG(0) = SENTOPG * PLTPOP*10.0
            SENCALG(0) = SENWALG(0) * 0.4 
            SENLALG(0) =
     &       (SENLFG*LLIGP/100+SENSTG*SLIGP/100) * PLTPOP*10.0
            SENNALG(0) = (SENNLFG+SENNSTG) * SENFR * PLTPOP*10.0
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
          DO L = 1,LNUMSG
            IF (EMRGFR.GT.0.0) LAGE(L) = LAGE(L) + DULF*EMRGFR
            LAGEP(L) = LAGEP(L) + LNUMG
          ENDDO
          IF (LNUMG.GT.0.0) THEN
            IF (LNUMSG.LT.LNUMX) THEN
              LAGE(LNUMSG+1) = LAGE(LNUMSG+1)+
     &         DULF*EMRGFR*AMAX1(0.0,LNUMG-LNUMNEED)/LNUMG
              LAGEP(LNUMSG+1)=LAGEP(LNUMSG+1)+AMAX1(0.0,LNUMG-LNUMNEED)
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
            RESPRC = RESPRC + RTRESP
            RESPTC = 0.0  ! Respiration tops - not yet used
            RESPC = RESPRC + RESPTC
            ! Variables for balancing during grain fill
            IF (CUMDU.GE.SGPHASEDU(2).AND.CUMDU.LT.PSTART(MSTG)) THEN
              RESPGF = RESPGF + RTRESP
              CARBOGF = CARBOGF + AMAX1(0.0,CARBOBEG+CARBOADJ)
            ENDIF

            LFWT = LFWT + GROLF - SENLFG - SENLFGRS - LWPH
            LWPHC = LWPHC +  LWPH

            IF (LFWT.LT.-1.0E-8) THEN
              WRITE(Message(1),'(A35,F4.1,A14)')
     &         'Leaf weight less than 0! Weight of ',lfwt,
     &         ' reset to zero'
              CALL WARNING(1,'CSCRP',MESSAGE)
              LFWT = 0.0
            ENDIF
            RSWT = RSWT + GRORS + GRORSGR - SENRS - RSWPH - CARBORRS
            RSWPHC = RSWPHC +  RSWPH
            ! Reserves distribution 
            ! Max concentration in leaves increases through life cycle.
            IF (PSTART(MSTG).GT.0.0) LLRSWT = AMIN1(RSWT,
     &       LFWT*(1.0-LSHFR)*(RSCLX/100.0)*CUMDU/PSTART(MSTG))
            IF (PSTART(MSTG).GT.0.0) LSHRSWT = AMIN1(RSWT-LLRSWT,
     &       LFWT*LSHFR*(RSCLX/100.0)*CUMDU/PSTART(MSTG))
            IF (STWT+CHWT.GT.0.0) THEN
              STRSWT = (RSWT-LLRSWT-LSHRSWT)*STWT/(STWT+CHWT)
              CHRSWT = (RSWT-LLRSWT-LSHRSWT)*CHWT/(STWT+CHWT)
            ELSE
              STRSWT = (RSWT-LLRSWT-LSHRSWT)
              CHRSWT = 0.0
            ENDIF

            IF (RSWT.LT.0.0) THEN
              IF (ABS(RSWT).GT.1.0E-6) THEN
                WRITE(Message(1),'(A30,A11,F12.9)')
     &           'Reserves weight reset to zero.',
     &           'Weight was ',rswt
                CALL WARNING(1,'CSCRP',MESSAGE)
                RSWT = 0.0
              ENDIF
            ENDIF

            RSWTX = AMAX1(RSWTX,RSWT)
            STWT = STWT + GROST - SENSTG - SENSTGRS - SWPH
            IF (STWT.LT.1.0E-06) THEN
              IF (STWT.LT.0.0) 
     &         WRITE(fnumwrk,*)'Stem weight less than 0! ',STWT
              STWT = 0.0
            ENDIF
            SWPHC = SWPHC +  SWPH
            STVWT = STVWT + STVWTG
            DEADWTR = DEADWTR + (SENLFG+SENSTG)*(1.0-SENFR) - dwrph
            dwrphc = dwrphc + dwrph
            DEADWTS = DEADWTS + (SENLFG+SENSTG)*SENFR
            GRWT = GRWT + GROGR - GWPH
            gwphc = gwphc + gwph
            CHWT = CHWT + GROCH 
            SENWL(0) = SENWL(0) + SENTOPG
            SENCL(0) = SENCL(0) + SENTOPG*0.4
            SENLL(0) = SENLL(0)
     &       + (SENLFG*LLIGP/100+SENSTG*SLIGP/100)*(SENFR)
            RTWT = 0.0
            DO L = 1, NLAYR
              RTWTL(L) = RTWTL(L) + RTWTGL(L) - RTWTSL(L) - RTWTUL(L)
              SENWL(L) = SENWL(L) + RTWTSL(L)
              SENCL(L) = SENCL(L) + RTWTSL(L) * 0.4
              SENLL(L) = SENLL(L) + RTWTSL(L) * RLIGP/100.0
              ! Totals
              RTWT = RTWT + RTWTL(L)
              SENWS = SENWS + RTWTSL(L)
              SENCS = SENCS + RTWTSL(L) * 0.4
              SENLS = SENLS + RTWTSL(L) * RLIGP/100.0
            END DO
            SRWT = SRWT + SRWTGRS + GROSR
          ENDIF

          SEEDRS = AMAX1(0.0,SEEDRS-CARBOLSD-SEEDRSAVR)
          IF (CFLSDRSMSG.NE.'Y'.AND.SEEDRS.LE.0.0.AND.LNUM.LT.4.0) THEN
            WRITE(Message(1),'(A44,F3.1)')
     &      'Seed reserves all used but leaf number only ',lnum
            WRITE(Message(2),'(A58)')
     &      'For good establishment seed reserves should last to leaf 4'
            WRITE(Message(3),'(A55)')
     &      'Maybe seeds too small or specific leaf area set too low'
            CALL WARNING(3,'CSCRP',MESSAGE)
            CFLSDRSMSG = 'Y'
          ENDIF
          SEEDUSE = SEEDUSE + CARBOLSD+SEEDRSAVR
          SEEDUSER = SEEDUSER + SEEDRSAVR
          SEEDUSET = SEEDUSET + CARBOLSD
          SEEDRSAV = SEEDRS

          RSWTPM = RSWTPM + GRORSPM
          SENGF = SENGF + SENRTGGF + SENTOPGGF

          IF (GNOPD.GT.0.0) GWUD = GRWT/GNOPD
          IF (SRNOPD.GT.0.0) SRWUD = SRWT/SRNOPD

          IF ((LFWT+STWT+GRWT+RSWT).GT.0.0) THEN
            IF (HPROD.EQ.'SR') THEN
              HIAD = SRWT/(LFWT+STWT+SRWT+RSWT+DEADWTR)
            ELSE
              HIAD = GRWT/(LFWT+STWT+GRWT+RSWT+DEADWTR)
            ENDIF
          ENDIF
          IF (RTWT.GT.0.0)
     &     SHRTD = (LFWT+STWT+GRWT+RSWT+DEADWTR) / RTWT

!-----------------------------------------------------------------------
!         Calculate reserve concentration
!-----------------------------------------------------------------------

          IF (LFWT+STWT.GT.0.0) RSCD = RSWT/(LFWT+STWT+RSWT)
          IF (RSCD.LT.0.0.AND.RSCD.GT.-1.0E-7) RSCD = 0.0
          RSCX = AMAX1(RSCX,RSCD)

!-----------------------------------------------------------------------
!         Update tiller leaf area (Must be done before PLA updated)
!-----------------------------------------------------------------------

          ! First for leaf senescence
          DO L = 1,INT(TNUM+1)
            IF (TNUM-FLOAT(L-1).GT.0.0) THEN
              IF (PLA-SENLA.GT.0.0) TLAS(L) = TLAS(L) +
     &             PLAS*(TLA(L)-TLAS(L))/(PLA-SENLA)
            ENDIF
          ENDDO

!-----------------------------------------------------------------------
!         Update produced leaf area
!-----------------------------------------------------------------------

          IF (DULF.GT.0.0) THEN
            DO L = 1,LNUMSG+1
              IF (LNUMSG.LT.LNUMX) THEN
                IF (PLAGTP(1)+PLAGTP(2).GT.0.0)
     &           LATL(1,L) = LATL(1,L)+PLAGLF(L)*
     &           ((PLAGT(1)+PLAGT(2))/(PLAGTP(1)+PLAGTP(2)))
              ENDIF
            ENDDO
            PLA = PLA + PLAGT(1) + PLAGT(2)
            PLAX = AMAX1(PLAX,PLA)
            LAP(LNUMSG) = LAP(LNUMSG) + PLAGT(1)
            IF (LNUMSG.LT.LNUMX)
     &       LAP(LNUMSG+1) = LAP(LNUMSG+1) + PLAGT(2)

            DO L = 1,INT(TNUM+1)
              IF (TNUM.GE.1.0.OR.TNUM-FLOAT(L-1).GT.0.0) THEN
                TLA(L) = TLA(L) + TLAG(L)
              ENDIF
            ENDDO

            IF (LCNUM.LT.LCNUMX) THEN
              IF (PLAGT(1).GT.0.0) THEN
                LCNUM = LCNUM+1
                LCOA(LCNUM) = PLAGT(1)
              ENDIF
              IF (LCNUM.LT.LCNUMX.AND.PLAGT(2).GT.0.0001) THEN
                LCNUM = LCNUM+1
                LCOA(LCNUM) = PLAGT(2)
              ELSE
                IF (LCNUM.GT.0)              
     &           LCOA(LCNUM) = LCOA(LCNUM) + PLAGT(2)
              ENDIF
            ELSE
              LCOA(LCNUM) = LCOA(LCNUM) + PLAGT(1) + PLAGT(2)
            ENDIF

          ENDIF

!-----------------------------------------------------------------------
!         Update senesced and harvested leaf area
!-----------------------------------------------------------------------

          SENLA = SENLA + PLAS
    	    ! Grazed leaf area
          laphc = laphc + laph
          ! Distribute senesced leaf over leaf positions and cohorts
          ! Leaf positions
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
          IF (LFWT.GT.1.0E-6) SLA=(PLA-SENLA-LAPHC) / (LFWT*(1.0-LSHFR))
            
!-----------------------------------------------------------------------
!         Update leaf sheath,stem,and awn area
!-----------------------------------------------------------------------

          IF (HPROD.EQ.'SR') THEN
            LSHAI = (LFWT*LSHFR*LSAWV)*PLTPOP*0.0001
          ELSE
            IF (TSSTG.LE.0.OR.LLSTG.LE.0) THEN
              LSHAI = (LFWT*LSHFR*LSAWV)*PLTPOP*0.0001
            ELSE
              IF (RSTAGE.LE.TSSTG) THEN
                LSHAI = (LFWT*LSHFR*LSAWV)*PLTPOP*0.0001
              ELSEIF (RSTAGE.GT.TSSTG.AND.RSTAGE.LT.LLSTG) THEN
                LSAW = LSAWV+((LSAWR-LSAWV)*(RSTAGE-TSSTG))
                LSHAI = (LFWT*LSHFR*LSAW)*PLTPOP*0.0001
              ELSE
                ! Use RSEN as temporary fix for sheath senescence
                LSHAI = LSHAI * (1.0-(RSEN/100.0)*TT/STDAY)  
              ENDIF  
            ENDIF  
          ENDIF

          STAI = STAI + STAIG - STAIS

          IF (CUMDU.GT.PSTART(IESTG).AND.AWNS.GT.0.0)
     &     AWNAI = AWNAI + STAIG*AWNS/10.0

          SAID = STAI+AWNAI+LSHAI
          CAID = LAI + SAID

!-----------------------------------------------------------------------
!         Update height
!-----------------------------------------------------------------------

          CANHT = CANHT + CANHTG

!-----------------------------------------------------------------------
!         Update tiller numbers (Limited to a maximum of 20 per plant)
!-----------------------------------------------------------------------

          TNUM = AMIN1(TINOX,AMAX1(1.0,TNUM+TNUMG-TNUMD-TNUMLOSS))
          ! Fof Zhang
          spnumhc = spnumhc + spnumh
          ! End for Zhang
          IF (LNUMSG.GT.0) TNUML(LNUMSG) = TNUM
          IF (TNUM-FLOAT(INT(TNUM)).GT.0.0.AND.
     &     TILBIRTHL(INT(TNUM+1)).LE.0.0) THEN
            IF (ABS(TNUM-TNUMPREV) > 1.E-6) THEN   
              TILBIRTHL(INT(TNUM)+1) = LNUMPREV
     &        + (FLOAT(INT(TNUM))-TNUMPREV)/(TNUM-TNUMPREV)
     &        * (LNUM-LNUMPREV)
            ELSE
              TILBIRTHL(INT(TNUM)+1) = LNUMPREV
            ENDIF
          ENDIF
          TNUMX = AMAX1(TNUMX,TNUM)

!-----------------------------------------------------------------------
!         Update plant number
!-----------------------------------------------------------------------

          PLTPOP = PLTPOP - PLTLOSS
          IF (PLTPOP.LE.0.0) THEN
            Write (Message(1),'(A23)') 'Plant population < 0.0  '
            Write (Message(2),'(A12,F4.1)') 'Plant loss: ',pltloss      
            WRITE (Message(3),'(A20)') 'Crop failure assumed'
            CALL WARNING(4,'CSCRP',MESSAGE)
            CFLFAIL = 'Y'
          ENDIF

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
          LEAFN = LEAFN + LNGU + GROLFRTN + LNUSE(0)
     &          - STEMNGL - GRAINNGL - SENNLFG - SENNLFGRS - lnph
          LNPHC = LNPHC +  LNPH
          IF (LEAFN.LT.1.0E-10) LEAFN = 0.0
          STEMN = STEMN + SNGU + STEMNGL + SNUSE(0)
     &          - GRAINNGS - SENNSTG - SENNSTGRS - SNPH
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
          ROOTN = ROOTN + (RNUSE(0)-GRAINNGR-ROOTNS-GROLFRTN)
          SROOTN = SROOTN + SRNUSE(0)
          SEEDN = SEEDN - SEEDNUSE
          IF (SEEDN.LT.1.0E-6) SEEDN = 0.0
          GRAINN = GRAINN + GRAINNGU + GRAINNGL + GRAINNGS + GRAINNGR
     &           + GRAINNGRS - GNPH
          GNPHC = GNPHC +  GNPH
          RSN = RSN - GRAINNGRS - RSNUSE
     &        + SENNLFGRS + SENNSTGRS - SENNRS - RSNPH
          RSNPHC = RSNPHC +  RSNPH
          IF (LRETSDU.GT.0.0.AND.LRETSDU.LT.CUMDU) THEN
            DEADN = DEADN + SENNLFG + SENNSTG
          ELSE
            SENNL(0) = SENNL(0) + SENNLFG + SENNSTG
          ENDIF

          IF (HPROD.EQ.'SR') THEN
            HPRODN = SROOTN
          ELSE
            HPRODN = GRAINN
          ENDIF

          ! Harvest index for N
          HIND = 0.0
          IF ((LEAFN+STEMN+GRAINN+RSN+DEADN).GT.0.0)
     &     HIND = HPRODN/(LEAFN+STEMN+HPRODN+RSN+DEADN)

!-----------------------------------------------------------------------
!         Update stages
!-----------------------------------------------------------------------

          ! STAGES:Germination and emergence (Gstages)
          ! NB 0.5 factor used to equate to Zadoks)
          GEUCUM = GEUCUM + TT*WFGE
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
          CUMTT = CUMTT + TT
          IF (PSTART(MSTG).GT.0.0) THEN
            DSTAGE = CUMDU/PSTART(MSTG)
          ENDIF 

          ! STAGES:Reproductive development (Rstages)
          RSTAGEP = RSTAGE
          IF (GESTAGE.GE.0.5) THEN
            DO L = HSTG,1,-1
              IF (CUMDU.GE.PSTART(L).AND.PD(L).GT.0.0) THEN
                RSTAGE = FLOAT(L) + (CUMDU-PSTART(L))/PD(L)
                ! Rstage cannot go above harvest stage 
                RSTAGE = AMIN1(FLOAT(HSTG),RSTAGE)
                EXIT
              ENDIF
            ENDDO
          ENDIF

          ! STAGES:Leaf development (Lstages)
          IF (GESTAGE.GE.1.0) THEN
            IF (EYEARDOY.LE.0) THEN
              LFGSDU = CUMDU - DU + TT*VF*DFPE*(GERMFR-EMRGFR)
              CUMDULF = TT*VF*DF*EMRGFR
            ELSE
              CUMDULF = AMAX1(0.0,AMIN1(LGPHASEDU(2)-LFGSDU,CUMDULF+DU))
            ENDIF
            IF (LGPHASEDU(2).GT.0.0)
     &       LSTAGE = AMAX1(0.0,AMIN1(1.0,CUMDULF/LGPHASEDU(2)))
            ENDIF

          ! STAGES:Leaf numbers
          LNUM = AMAX1(0.0,(AMIN1(FLOAT(LNUMX-1),(LNUM+LNUMG))))
          
          LNUMSG = INT(LNUM)+1
          IF (LNUM.GE.FLOAT(LNUMX-1)+0.9.AND.RSTAGE.LT.4.0) THEN
            IF (CCOUNTV.EQ.0) THEN
             WRITE (Message(1),'(A35)')
     &       'Maximum leaf number reached on day '
             CALL WARNING(1,'CSCRP',MESSAGE)
            ENDIF
            CCOUNTV = CCOUNTV + 1
            IF (CCOUNTV.EQ.50.AND.VREQ.GT.0.0) THEN
              WRITE (Message(1),'(A34)')
     &         '50 days after maximum leaf number '
              WRITE (Message(2),'(A54)')
     &         'Presumably vernalization requirement could not be met '
              WRITE (Message(3),'(A25)')
     &         'Will assume crop failure.'
              CALL WARNING(3,'CSCRP',MESSAGE)
              CFLFAIL = 'Y'
            ENDIF
          ENDIF

          ! STAGES:Apical development - double ridges. 
          !  Factors by calibration from LAMS
          !  Only used for comparison with calc based on spe input
          IF (CROP.NE.'CS') THEN
            drf1 = 1.9
            drf2 = 0.058
            drf3 = 3.3
            DRSTAGE = AMAX1(1.1,drf1-drf2*(LNUM-drf3))
            IF (DRDAT.EQ.-99 .AND. RSTAGE.GE.DRSTAGE) THEN
              DRDAT = YEARDOY
              WRITE(fnumwrk,*)'Double ridges. Rstage,Drtage,Leaf#: ',
     &         RSTAGE,DRSTAGE,LNUM
               ! NB. Experimental. DR occurs at later apical stage when
               !     leaf # less, earlier when leaf # greater (ie.when
               !     early planting of winter type).
            ENDIF
          ENDIF  
  
          ! STAGES:Stem development (Ststages)
          IF (CUMDU.GT.SGPHASEDU(1).AND.
     &        SGPHASEDU(2)-SGPHASEDU(1).GT.0.)THEN
            CUMDUS =AMAX1(.0,AMIN1(SGPHASEDU(2)-SGPHASEDU(1),CUMDUS+DU))
            STSTAGE = CUMDUS/(SGPHASEDU(2)-SGPHASEDU(1))
          ENDIF

          ! STAGES:Zadoks
          ! Zadoks (1974) codes
          ! CODE     DESCRIPTION                           RSTAGE
          ! 00-09    Germination                                .
          !  00=Dry seed at planting                            .
          !  01=begining of seed imbibition                     .
          !  05=germination (when radicle emerged)            0.0
          !  09=coleoptile thru soil surface                    .
          ! 10-19    Seedling growth                            .
          !  10=first leaf emerged from coleoptile              .
          !  11=first leaf fully expanded                       .
          !  1n=nth leaf fully expanded                         .
          ! 20-29    Tillering, no. of tillers + 20             .
          !  20=first tiller appeared on some plants            .
          !  2n=nth tiller                                      .
          !  21 Main shoot plus 1 tiller                        .
          !  22 Main shoot plus 2 tillers                       .
          ! 30-39    Stem elongation, no. nodes + 30            .
          !  30 Pseudo stem erection                          3.0
          !  31 1st node detectable. Jointing                 3.1
          !  32 2nd node detectable                             .
          !  37 Flag leaf just visible                          .
          !  39 Flag leaf ligule just visible                   .
          ! 40-49    Booting                                    .
          !  40 Flag sheath extending. Last leaf              4.0
          !  45 Boots swollen                                   .
          !  47 Flag sheath opening                             .
          !  49 First awns visible                              .
          ! 50-59    Inflorescence emergence                    .
          !  50 First spikelet just visible. Inflorescence    5.0
          !  59 Inflorescence emergence completed               .
          ! 60-69    Flowering                                  .
          !  60 Beginning of anthesis                         6.0
          !  65 Anthesis half way                               .
          !  69 Anthesis complete                             7.0
          ! 70-79    Milk development                           .
          !  70 Caryopsis water ripe                            .
          !  71 V.early milk                                    .
          !  73 Early milk                                      .
          !  75 Medium milk                                     .
          !  77 Late milk                                       .
          ! 80-89    Dough development                          .
          !  80 Milk -> dough                                 8.0
          !  81 V.early dough                                   .
          !  83 Early dough                                     .
          !  85 Soft dough (= end of grain fill?)               .
          !  87 Hard dought                                     .
          ! 90-99    Ripening                                   .
          !  90 Late hard dough (=end of grain fill?)         9.0
          !  91 Hard;difficult to divide by thumb-nail          .
          !     Binder ripe 16% h2o. Harvest                    .
          !     Chlorophyll of inflorescence largely lost       .
          !  92 Hard;can no longer be dented by thumb-nail   10.0
          !     Combine ripe < 16% h2o                          .
          !  93 Caryopsis loosening in daytime                  .
          !  93 = harvest maturity ?                            .

          IF (GESTAGE.LT.1.0) THEN
              ZSTAGE = GESTAGE * 10.0
          ELSEIF (GESTAGE.GE.1.0.AND.RSTAGE.LE.3.0) THEN
            IF (TNUM.LT.2.0) THEN
              ZSTAGE = 10.0 + LNUM
            ELSE
              ZSTAGE = AMIN1(30.0,20.0+(TNUM-1.0))
            ENDIF
          ELSEIF (RSTAGE.GT.3.0.AND.RSTAGE.LE.7.0) THEN
            ZSTAGE = 30.0 + 10.0*(RSTAGE-3.0)
            ! LAH Staging hereafter based on data from M.Fernandes:
            !  RSTAGE ZSTAGE
            !    70.6 69
            !    80.7 71
            !    82.8 75
            !    86.1 85
            !    88.1 91
            !ELSEIF (RSTAGE.GT.7.0.AND.RSTAGE.LE.8.0) THEN
            !  ZSTAGE = 70.0 + 2.0*(RSTAGE-7.0)
            !ELSEIF (RSTAGE.GT.8.0.AND.RSTAGE.LE.8.3) THEN
            !  ZSTAGE = 72.0 + 1.0*(RSTAGE-8.0)*10.0
            !ELSEIF (RSTAGE.GT.8.3.AND.RSTAGE.LE.8.6) THEN
            !  ZSTAGE = 75.0 + 3.3*(RSTAGE-8.3)*10.0
            !ELSEIF (RSTAGE.GT.8.6.AND.RSTAGE.LE.9.0) THEN
            !  ZSTAGE = 85.0 + 2.0*(RSTAGE-8.6)*10.0
            !ENDIF
            ! BUT taken out because not match with CSCER045
            ! Need to check when end of milk stage in RSTAGE terms.
          ELSEIF (RSTAGE.GT.7.0.AND.RSTAGE.LE.9.0) THEN
            ZSTAGE = 10.0 * RSTAGE          
          ENDIF

          IF (HPROD.EQ.'SR') THEN
            GSTAGE = RSTAGE
          ELSE
            GSTAGE = ZSTAGE
          ENDIF

!-----------------------------------------------------------------------
!         Record stage dates and states
!-----------------------------------------------------------------------

          IF (INT(RSTAGE).GT.10.OR.
     &      INT(RSTAGE).LT.0.AND.GESTAGE.GT.0.5) THEN
            OPEN (UNIT = FNUMERR,FILE = 'ERROR.OUT')
            WRITE(fnumerr,*) ' '
            WRITE(fnumerr,*)
     &       'Rstage out of range allowed for phase thresholds '
            WRITE(fnumerr,*) 'Rstage was: ',rstage
            WRITE(fnumerr,*) 'Please contact model developer'
            WRITE(*,*)
     &       ' Rstage out of range allowed for phase thresholds '
            WRITE(*,*) ' Rstage was: ',rstage
            WRITE(*,*) ' Program will have to stop'
            PAUSE
            CLOSE (fnumerr)
            STOP ' '
          ENDIF
          IF (CUMDU-DU.LT.PSTART(INT(RSTAGE))) THEN
            STGYEARDOY(INT(RSTAGE)) = YEARDOY
            ! NB.Conditions are at start of phase
            IF (DU.GT.0.0) THEN
              CWADSTG(INT(RSTAGE)) = CWADPREV+DUNEED/DU*(CWAD-CWADPREV)
              LAISTG(INT(RSTAGE)) = LAIPREV + DUNEED/DU*(LAI-LAIPREV)
              LNUMSTG(INT(RSTAGE)) = LNUMPREV+DUNEED/DU*(LNUM-LNUMPREV)
              CNADSTG(INT(RSTAGE)) = CNADPREV+DUNEED/DU*(CNAD-CNADPREV)
            ENDIF
          ENDIF

          ! Primary stages
          IF (RSTAGEP.LT.0) RSTAGEP = 0.0
          L = INT(RSTAGEP) + 1
          IF (PSDAT(L).LE.0.0.AND.CUMDU.GE.PSTART(L)) THEN
            PSDAT(L) = YEARDOY
            IF (DU.GT.0.0) PSDAPFR(L)=(PSTART(L)-(CUMDU-DU))/DU
            PSDAPFR(L) = FLOAT(DAP) + PSDAPFR(L)
            PSDAP(L) = DAP
            IF (PSABV(L).EQ.'ADAT '.OR.PSABV(L).EQ.' ADAT') THEN
              ADAT = YEARDOY
              ADOY = DOY
              ADAP = DAP
              ADAYFR = TIMENEED
              ADAPFR = FLOAT(ADAP) + ADAYFR
              RSWTA = RSWT - (1.0-ADAYFR)*(GRORS+GRORSGR)
              STWTA = STWT-(1.0-ADAYFR)*(GROST-SENSTG)
              LFWTA = LFWT-(1.0-ADAYFR)*(GROLF-SENLFG-SENLFGRS)
              CWAA = (LFWTA+STWTA+RSWTA)*PLTPOP*10.0
              LFWAA = LFWTA*PLTPOP*10.0
              STWAA = STWTA*PLTPOP*10.0
              RSWAA = RSWTA*PLTPOP*10.0
              ! LAH  Below are not adjusted for adayfr
              CNAA = CNAD
              IF (CWAA.GT.0.0) CNPCA = CNAA/CWAA*100.0
            ENDIF
            IF (PSABV(L).EQ.'AEDAT') THEN
              ADATEND = YEARDOY
              AEDAPFR = FLOAT(DAP) + ADAYEFR
              RSWTAE = RSWT - (1.-ADAYEFR)*(GRORS+GRORSGR)
              LFWTAE = LFWT-(1.0-ADAYEFR)*(GROLF-SENLFG-SENLFGRS)
              STWTAE = STWT - (1.0-ADAYEFR)*(GROST-SENSTG)
              TMEAN20A = TMEAN20
              SRAD20A = SRAD20
              IF ((GNOPD*PLTPOP).LT.100.0) THEN
                WRITE (Message(1),'(A44)')
     &           'Very few grains set! Failure as a grain crop'
                WRITE (Message(2),'(A26,F6.1)')
     &           '  Plant population      = ',pltpop
                WRITE (Message(3),'(A26,F6.1)')
     &           '  Above ground  (kg/ha) = '
     &           ,(LFWT+STWT+RSWT)*pltpop*10.0
                WRITE (Message(4),'(A26,F6.1)')
     &           '  Grain number coeff    = ',gnows
                WRITE (Message(5),'(A26,F6.1)')
     &           '  Leaf area index       = ',lai
                CALL WARNING(5,'CSCRP',MESSAGE)
                Message = ' '
              ENDIF
            ENDIF
            IF (PSABV(L).EQ.'MDAT '.OR.L.EQ.MSTG) THEN
              MDAT = YEARDOY
              MDOY = DOY
              MDAP = DAP
              MDAYFR = TIMENEED
              MDAPFR = FLOAT(MDAP) + MDAYFR
              GFDUR = (MDAPFR-GFDAPFR)
            ENDIF
            IF (PSABV(L).EQ.'GFDAT') THEN
              GFDAPFR = FLOAT(DAP) + TIMENEED
            ENDIF
          ENDIF

          ! Secondary stages
          DO L = 1,SSNUM
            IF (SSDAT(L).LE.0 .AND. CUMDU.GE.SSTH(L)) THEN
              SSDAT(L) = YEARDOY
              IF (DU.GT.0.0) SSDAYFR(L) = (SSTH(L)-(CUMDU-DU))/DU
              SSDAPFR(L) = FLOAT(DAP) + SSDAYFR(L)
              SSDAP(L) = DAP
            ENDIF
          ENDDO

          IF (STGEDAT.LE.0.AND.CUMDU.GE.SGPHASEDU(2)) THEN
            STGEDAT = YEARDOY
            IF (DU.GT.0.0) STGEFR = (SGPHASEDU(2)-(CUMDU-DU))/DU
            SGEDAPFR = FLOAT(DAP) + STGEFR
            ! Following for balancing only.Start at beginning of day
            STWTSGE = STWT
            LFWTSGE = LFWT
            DEADWTSGE = DEADWTR
            RTWTSGE = RTWT
            GRWTSGE = GRWT
            RSWTSGE = RSWT
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

          IF (TILDAT.LE.0.0.AND.TNUM.GT.1.0) THEN
            TILDAT = YEARDOY
            TILDAP = FLOAT(DAP)
          ENDIF

          ! Vernalization  Process starts at germination,stops at VPEND
          IF (DU.GT.0.0) THEN
            VPENDFR = AMAX1(0.0,AMIN1(1.0,(VPENDDU-CUMDU)/DU))
          ELSE
            VPENDFR = 1.0
          ENDIF
          CUMVD = CUMVD+TFV*GERMFR*VPENDFR-VDLOS
          IF (VREQ.GT.0.0) THEN
            VRNSTAGE =AMAX1(0.,AMIN1(1.,(CUMVD-VBASE)/(VREQ-VBASE)))
          ELSE
            VRNSTAGE = 1.0
          ENDIF
          ! Vernalization  Effect starts at germination,stops at VEEND
          IF (CUMDU.LE.VEENDDU) THEN
            VF = AMAX1(0.,(1.0-VEFF) + VEFF*VRNSTAGE)
            IF (CUMDU+DU.LT.VEENDDU) THEN
              VFNEXT = VF
            ELSE
              VFNEXT = 1.0
            ENDIF
          ELSE
            VF = 1.0
            VFNEXT = 1.0
          ENDIF

          ! STAGES:Cold hardening
          ! NB. Hardening starts at germination,does not stop
          ! Hardening loss occurs throughout growth cycle
          HARDAYS = HARDAYS+TFH*GERMFR-HARDILOS
          IF (HDUR.GT.1.0) THEN
            HSTAGE = AMIN1(1.0,HARDAYS/HDUR)
          ELSE
            HSTAGE = 1.0
          ENDIF
          TKILL = TKUH + (TKFH-TKUH)*HSTAGE

!-----------------------------------------------------------------------
!         Calculate phase and crop cycle conditions
!-----------------------------------------------------------------------

          IF (WFG.LT.0.99999) WSDAYS = WSDAYS + 1
          IF (NFG.LT.0.99999) NSDAYS = NSDAYS + 1

          IF (GESTAGE.GT.0.1) THEN
            IF (CUMDU-DU.LT.PSTART(INT(RSTAGE))) THEN
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
            
            RAINPC(INT(RSTAGE)) = RAINPC(INT(RSTAGE)) + RAIN
            ETPC(INT(RSTAGE))   = ETPC(INT(RSTAGE)) + ET 
            EPPC(INT(RSTAGE))   = EPPC(INT(RSTAGE)) + EP 
            
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
            
            PDAYS(INT(RSTAGE)) = PDAYS(INT(RSTAGE)) + 1
            CDAYS = CDAYS + 1
            IF (PDAYS(INT(RSTAGE)).GT.0) THEN
              TMAXPAV(INT(RSTAGE)) = TMAXPC / PDAYS(INT(RSTAGE))
              TMINPAV(INT(RSTAGE)) = TMINPC / PDAYS(INT(RSTAGE))
              TMEANAV(INT(RSTAGE)) = TMEANPC / PDAYS(INT(RSTAGE))
              SRADPAV(INT(RSTAGE)) = SRADPC / PDAYS(INT(RSTAGE))
              DAYLPAV(INT(RSTAGE)) = DAYLPC / PDAYS(INT(RSTAGE))
              DAYLST(INT(RSTAGE)) = DAYL
              CO2PAV(INT(RSTAGE)) = CO2PC / PDAYS(INT(RSTAGE))
              RAINPAV(INT(RSTAGE)) = 
     &         RAINPC(INT(RSTAGE)) / PDAYS(INT(RSTAGE))
              NFPPAV(INT(RSTAGE)) = NFPPC / PDAYS(INT(RSTAGE))
              NFGPAV(INT(RSTAGE)) = NFGPC / PDAYS(INT(RSTAGE))
              WFPPAV(INT(RSTAGE)) = WFPPC / PDAYS(INT(RSTAGE))
              WFGPAV(INT(RSTAGE)) = WFGPC / PDAYS(INT(RSTAGE))
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
            LNCX = LNCXS(0) + LSTAGE*(LNCXS(1)-LNCXS(0))
            SNCX = SNCXS(0) + STSTAGE*(SNCXS(1)-SNCXS(0))
            RNCX = RNCXS(0) + DSTAGE*(RNCXS(1)-RNCXS(0))
            LNCM = LNCMN(0) + LSTAGE*(LNCMN(1)-LNCMN(0))
            SNCM = SNCMN(0) + STSTAGE*(SNCMN(1)-SNCMN(0))
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
            IF (STWT.GT.1.0E-5) SANC = STEMN / STWT
            IF (VWAD.GT.0.0) VANC = VNAD/VWAD
            IF (LANC.LT.0.0) THEN 
              WRITE(Message(1),'(A27,F4.1)')
     &         'LANC below 0 with value of ',LANC
              WRITE(Message(2),'(A27,2F5.1)')
     &         'LEAFN,LFWT had values of   ',LEAFN,LFWT
              CALL WARNING(2,'CSCRP',MESSAGE)
              LANC = AMAX1(0.0,LANC)
            ENDIF
            IF (LFWT+STWT.GT.0.0) VCNC = 
     &      (LNCX*AMAX1(0.0,LFWT)+SNCX*AMAX1(0.0,STWT))/
     &      (AMAX1(0.0,LFWT)+AMAX1(0.0,STWT))
            IF (LFWT+STWT.GT.0.0) VMNC = 
     &      (LNCM*AMAX1(0.0,LFWT)+SNCM*AMAX1(0.0,STWT))/
     &      (AMAX1(0.0,LFWT)+AMAX1(0.0,STWT))

            SDNC = 0.0
            GRAINANC = 0.0
            SRNC = 0.0
            IF (SEEDRS.GT.0.0) SDNC = SEEDN/(SEEDRS+SDCOAT)
            IF (GRWT.GT.0) GRAINANC = GRAINN/GRWT
            IF (SRWT.GT.0) SRNC = SROOTN/SRWT
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

!-----------------------------------------------------------------------
!         Reset phyllochron interval and phase lengths if appropriate
!-----------------------------------------------------------------------

          ! LAH None of this working. Need to check primordia number,etc
          ! Also,adjust for fraction of day, and ensure that stage does
          ! not drop after adjustment
          IF (CROP.EQ.'MZ'.AND.PDADJ.LE.-99.0.AND.RSTAGE.GE.3.0) THEN
            IF (CUMDU-DU.GT.0.0)
     &       PDADJ = (CUMTT-TT)/(CUMDU-DU)
            DO L = 3,MSTG
              PSTART(L) = PSTART(L) + AMAX1(0.0,PDADJ-1.0)*PD(2)
            ENDDO

            ! Re-calculate thresholds
            LAFSTDU = 0.
            VEENDDU = 0.
            VPENDDU = 0.
            RUESTGDU = 0.
            LSENSDU = 0.
            TILPEDU = 0.
            TILDSDU = 0.
            TILDEDU = 0.
            LRETSDU = 0.0
            GGPHASEDU(1) = 0.0
            LGPHASEDU(1) = 0.0
            LGPHASEDU(2) = 0.0
            SGPHASEDU(1) = 0.0
            SGPHASEDU(2) = 0.0
            SGPHASEDU(1) = 0.0
            DO L = 1,MSTG
              PSTART(L) = PSTART(L-1) + PD(L-1)
            ENDDO
            DO L = 1,MSTG
              IF (INT(SSTG(L1)).EQ.L)
     &          SSTH(L) = PSTART(L)
     &                      +(SSTG(L1)-FLOAT(INT(SSTG(L1))))*PD(L)
              IF (L.EQ.INT(LAFST))
     &          LAFSTDU = PSTART(L)+(LAFST-FLOAT(INT(LAFST)))*PD(L)
              IF (L.EQ.INT(VEEND))
     &          VEENDDU = PSTART(L)+(VEEND-FLOAT(INT(VEEND)))*PD(L)
              IF (L.EQ.INT(VPEND))
     &          VPENDDU = PSTART(L)+(VPEND-FLOAT(INT(VPEND)))*PD(L)
              IF (L.EQ.INT(RUESTG))
     &          RUESTGDU = PSTART(L)+(RUESTG-FLOAT(INT(RUESTG)))*PD(L)
              IF (L.EQ.INT(LSENS))
     &          LSENSDU = PSTART(L)+(LSENS-FLOAT(INT(LSENS)))*PD(L)
              IF (L.EQ.INT(LSENE))
     &          LSENEDU = PSTART(L)+(LSENE-FLOAT(INT(LSENE)))*PD(L)
              IF (L.EQ.INT(SSPHASE(1))) SSPHASEDU(1) =
     &         PSTART(L)+(SSPHASE(1)-FLOAT(INT(SSPHASE(1))))*PD(L)
              IF (L.EQ.INT(SSPHASE(2))) SSPHASEDU(2) =
     &         PSTART(L)+(SSPHASE(2)-FLOAT(INT(SSPHASE(2))))*PD(L)
              IF (L.EQ.INT(TILPE))
     &          TILPEDU = PSTART(L)+(TILPE-FLOAT(INT(TILPE)))*PD(L)
              IF (L.EQ.INT(TILDS))
     &          TILDSDU = PSTART(L)+(TILDS-FLOAT(INT(TILDS)))*PD(L)
              IF (L.EQ.INT(TILDE))
     &          TILDEDU = PSTART(L)+(TILDE-FLOAT(INT(TILDE)))*PD(L)
              IF (L.EQ.INT(LRETS))
     &          LRETSDU = PSTART(L)+(LRETS-FLOAT(INT(LRETS)))*PD(L)
              IF (L.EQ.INT(GGPHASE(1))) GGPHASEDU(1) =
     &         PSTART(L)+(GGPHASE(1)-FLOAT(INT(GGPHASE(1))))*PD(L)
              IF (L.EQ.INT(LGPHASE(1))) LGPHASEDU(1) =
     &         PSTART(L)+(LGPHASE(1)-FLOAT(INT(LGPHASE(1))))*PD(L)
              IF (L.EQ.INT(LGPHASE(2))) LGPHASEDU(2) =
     &         PSTART(L)+(LGPHASE(2)-FLOAT(INT(LGPHASE(2))))*PD(L)
              IF (L.EQ.INT(SGPHASE(1))) SGPHASEDU(1) =
     &         PSTART(L)+(SGPHASE(1)-FLOAT(INT(SGPHASE(1))))*PD(L)
              IF (L.EQ.INT(SGPHASE(2))) SGPHASEDU(2) =
     &         PSTART(L)+(SGPHASE(2)-FLOAT(INT(SGPHASE(2))))*PD(L)
            ENDDO
          ENDIF
          
 6666     CONTINUE    

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
             CALL WARNING(1,'CSCRP',MESSAGE)
          ENDIF
          IF (IHARI.NE.'A'.AND.MDAT.GE.0.AND.DAP-MDAP.GE.90) THEN
            CFLFAIL = 'Y'
            WRITE (Message(1),'(A32)')'90 days after end of grain fill '
            WRITE (Message(2),'(A21)')'Harvesting triggered.'
            CALL WARNING(2,'CSCRP',MESSAGE)
          ENDIF
          IF (IHARI.NE.'A'.AND.CUMDU.GE.PSTART(MSTG-1)) THEN
            IF (TT20.LE.-98.0.AND.TT20.LE.0.0) THEN
              CFLFAIL = 'Y'
              WRITE (Message(1),'(A28)') '20day thermal time mean = 0 '
              CALL WARNING(1,'CSCRP',MESSAGE)
           ENDIF
          ENDIF

          ! Determine if to harvest
          CFLHAR = 'N'
          IF (IHARI.EQ.'R'.AND.YEARDOYHARF.EQ.YEARDOY .OR.
     &     IHARI.EQ.'D'.AND.YEARDOYHARF.EQ.DAP .OR.
     &     IHARI.EQ.'G'.AND.YEARDOYHARF.LE.RSTAGE .OR.
     &     IHARI.EQ.'A'.AND.YEARDOYHARF.EQ.YEARDOY .OR.
     &     IHARI.EQ.'M'.AND.CUMDU.GE.PSTART(MSTG)) THEN
            CFLHAR = 'Y'
          ENDIF    

          IF(IHARI.EQ.'R'.AND.CFLHAR.EQ.'N')THEN
!            IF (CUMDU.GT.PSTART(HSTG) .AND. CFLHARMSG .NE. 'Y') THEN
            IF (CUMDU.GT.PSTART(MSTG) .AND. CFLHARMSG .NE. 'Y') THEN
              WRITE(Message(1),'(A54,I7)')
     &        'Maturity reached but waiting for reported harvest on: ',
     &        YEARDOYHARF 
              CALL WARNING(1,'CSCRP',MESSAGE)
              CFLHARMSG = 'Y'
            ENDIF
          ENDIF
     
          IF (CFLFAIL.EQ.'Y' .OR. CFLHAR.EQ.'Y') THEN
          
            IF (CFLFAIL.EQ.'Y'
     &            .AND. RSTAGE <= 12 .AND. RSTAGE > 0 ) THEN       
              STGYEARDOY(12) = YEARDOY
              TMAXPAV(12) = TMAXPAV(INT(RSTAGE))
              TMINPAV(12) = TMINPAV(INT(RSTAGE))
              SRADPAV(12) = SRADPAV(INT(RSTAGE))
              DAYLPAV(12) = DAYLPAV(INT(RSTAGE))
              RAINPAV(12) = RAINPAV(INT(RSTAGE))
              CO2PAV(12) = CO2PAV(INT(RSTAGE))
              NFPPAV(12) = NFPPAV(INT(RSTAGE))
              WFPPAV(12) = WFPPAV(INT(RSTAGE))
              WFGPAV(12) = WFGPAV(INT(RSTAGE))
              NFGPAV(12) = NFGPAV(INT(RSTAGE))
            ENDIF
            STGYEARDOY(10) = YEARDOY  ! Harvest
            STGYEARDOY(11) = YEARDOY  ! Crop End
            IF (HSTG.GT.0) PSDAPFR(HSTG) = FLOAT(DAP)
            IF (ECSTG.GT.0) PSDAPFR(ECSTG) = FLOAT(DAP)
            IF (MSTG.GT.0.AND.PSDAPFR(MSTG).LE.0.0)PSDAPFR(MSTG) = -99.0
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
                CALL WARNING(1,'CSCRP',MESSAGE)
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

          ! Here,reserves are included in leaf,stem,and chaff weights
          ! And weights are in kg/ha
          CWAD = (LFWT+STWT+CHWT+GRWT+RSWT+DEADWTR)*PLTPOP*10.0
          IF (LRETS.GT.0.AND.LRETS.LT.99999.AND.DEADWTR.GT.0.0) THEN
            DEADWAD = DEADWTR*PLTPOP*10.0
          ELSEIF (LRETS.LE.0.OR.LRETS.GE.99999) THEN
            DEADWAD = DEADWTS*PLTPOP*10.0
          ENDIF
          GWAD = GRWT*PLTPOP*10.0
          SRWAD = SRWT*PLTPOP*10.0
          LLWAD = LFWT*(1.0-LSHFR)*10.0*PLTPOP
          LSHWAD = LFWT*LSHFR*10.0*PLTPOP
          RWAD = RTWT*PLTPOP*10.0
          SDWAD = (SEEDRS+SDCOAT)*10.0*PLTPOP
          ! Leaf sheaths NOT included in stem here
          STWAD = STWT*10.0*PLTPOP
          CHWAD = CHWT*PLTPOP*10.0
          RSWAD = RSWT*PLTPOP*10.0
          RSWADPM = RSWTPM*PLTPOP*10.0
          LLRSWAD = LLRSWT*PLTPOP*10.0
          LSHRSWAD = LSHRSWT*PLTPOP*10.0
          STRSWAD = STRSWT*PLTPOP*10.0
          CHRSWAD = CHRSWT*PLTPOP*10.0

          ! Need to CHECK these
          SENWAS = SENWS*10.0*PLTPOP
          SENCAS = SENCS*10.0*PLTPOP
          SENLAS = SENLS*10.0*PLTPOP
          SENWAL(0) = SENWL(0)*PLTPOP*10.0
          DO L =1,NLAYR
            RTWTAL(L) = RTWTL(L)*PLTPOP*10.0
            SENWAL(L) = SENWL(L)*PLTPOP*10.0
          ENDDO

          TWAD = (SEEDRS+SDCOAT+RTWT+LFWT+STWT+GRWT+RSWT+DEADWTR)
     &         * PLTPOP*10.0

          VWAD = (LFWT+STWT+RSWT+DEADWTR)*PLTPOP * 10.0
          EWAD = (GRWT+CHWT)*PLTPOP * 10.0

          GNOAD = GNOPD*PLTPOP
          TNUMAD = TNUM*PLTPOP

          IF (NUPAC.LT.0.0) THEN
            NUPAC = NUPAD
          ELSE 
            NUPAC = NUPAC+NUPAD
          ENDIF  
          CNAD = (LEAFN+STEMN+GRAINN+RSN+DEADN)*PLTPOP*10.0
          DEADNAD = DEADN*PLTPOP*10.0
          GNAD = GRAINN*PLTPOP*10.0
          SRNAD = SROOTN*PLTPOP*10.0
          LLNAD = LEAFN*(1.0-LSHFR)*PLTPOP*10.0
          RNAD = ROOTN*PLTPOP*10.0
          RSNAD = RSN*PLTPOP*10.0
          SDNAD = SEEDN*PLTPOP*10.0
!lah          SNAD = (STEMN+LEAFN*LSHFR)*PLTPOP*10.0
          SNAD = STEMN*PLTPOP*10.0
          TNAD = (ROOTN+LEAFN+STEMN+RSN+HPRODN+SEEDN+DEADN)*PLTPOP*10.0
          VNAD = (LEAFN+STEMN+RSN+DEADN)*PLTPOP*10.0
          
          ! LAH Note that no reserves included in sancout
          ! SANCOUT = SNAD/(STWAD+STRSWAD + LSHWAD+LSHRSWAD)
clah          IF ((STWAD + LSHWAD).GT.1.0E-5)
clah     &     SANCOUT = SNAD/(STWAD + LSHWAD)
          IF (STWAD.GT.1.0E-5)
     &     SANCOUT = SNAD/STWAD

          IF (HPROD.EQ.'SR') THEN
            HWAD = SRWAD
            HWUD = SRWUD
            HNUMAD = SRNOPD * PLTPOP
            HNAD = SRNAD
            HNC = SRNC
          ELSE
            HWAD = GWAD
            HWUD = GWUD
            HNUMAD = GNOAD
            HNAD = GNAD
            HNC = GRAINANC
          ENDIF

          SENNAS = SENNS*10.0*PLTPOP
          SENNAL(0) = SENNL(0)*PLTPOP*10.0
          SENNATC = SENNAL(0)+SENNAS
          DO L =1,NLAYR
            SENNAL(L) = SENNL(L)*PLTPOP*10.0
          ENDDO

          ! After harvest residues
          IF (STGYEARDOY(11).EQ.YEARDOY) THEN
            ! Surface
            RESWALG(0) = VWAD*(1.0-HBPCF/100.0) + GWAD*(1.0-HPCF/100.0)
            RESNALG(0) = (LEAFN+STEMN+DEADN)*PLTPOP*10.*(1.0-HBPCF/100.)
     &                 + GNAD*(1.0-HPCF/100.0)
            RESCALG(0) = RESWALG(0) * 0.4
            RESLGALG(0) = LLWAD*LLIGP/100.0*(1.0-HBPCF/100.0)
     &                  + LSHWAD*SLIGP/100.0*(1.0-HBPCF/100.0)
     &                  + STWAD*SLIGP/100.0*(1.0-HBPCF/100.0)
     &                  + GWAD*GLIGP/100.0*(1.0-HPCF/100.0)
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
          IF (ADAT.LT.0) RAINCA = RAINCA + RAIN
          SRADC = SRADC + SRAD
          PARMJC = PARMJC + PARMJFAC*SRAD
          PARMJIC = PARMJIC + PARMJFAC*SRAD*PARIF + PARMJIADJ
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
     &     (RTWT+LFWT+STWT+GRWT+RSWT+DEADWTR+SENWL(0)+SENWS-SEEDUSE)
     &     * PLTPOP / PARMJC)
          IF (PARMJIC.GT.0.0) PARIUED = AMAX1(0.0,
     &     (RTWT+LFWT+STWT+GRWT+RSWT+DEADWTR+SENWL(0)+SENWS-SEEDUSE)
     &     * PLTPOP / PARMJIC)

          IF (CARBOBEG.GT.0.0) THEN
            PARIUE = (CARBOBEG*PLTPOP)/(PARMJFAC*SRAD*PARIF)
          ENDIF

!-----------------------------------------------------------------------
!         Determine if nitrogen fertilizer applied
!-----------------------------------------------------------------------

          ! LAH Handled differently in stand-alone Cropsim. 
          ! Need to change Cropsim per se
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
            GRWTM = GRWT
            RSWTM = RSWT
            RTWTM = RTWT
            LFWTM = LFWT
            DEADWTM = DEADWTR
            STWTM = STWT
            RSWTM = RSWT

            LNUMSM = LNUM
            TNUMAM = TNUMAD
            GNOAM = GNOAD

            IF (GNOPD.GT.0.0) THEN
              GWUM = GRWTM/GNOPD
            ELSE
              GWUM = 0.0
            ENDIF
            IF (TNUMAM.GT.0.0) THEN
              GNOGM = GNOAM/TNUMAM
            ELSE
              GNOGM = 0.0
            ENDIF
            IF (PLTPOP.GT.0.0) THEN
              GNOPM = GNOAM/PLTPOP
              TNUMPM = TNUMAM/PLTPOP
            ELSE
              GNOPM = 0.0
              TNUMPM = 0.0
            ENDIF

            IF (LFWTM+STWTM+RSWTM.GT.0.0)
     &       RSCM = RSWTM/(LFWTM+STWTM)
            IF (RTWTM.GT.0.0)
     &       SHRTM = (LFWTM+STWTM+RSWTM+GRWTM+DEADWTM)/RTWTM

            CWAM = (LFWTM+STWTM+GRWTM+RSWTM+DEADWTM)*PLTPOP*10.0
            VWAM = (LFWTM+STWTM+RSWTM+DEADWTM)*PLTPOP * 10.0
            DEADWAM = DEADWTM*PLTPOP*10.0
            GWAM = GRWTM*PLTPOP*10.0
            
            ! For Grazing
            cwahc = (lwphc+swphc+rswphc+gwphc+dwrphc)*pltpop*10.0
            ! Adjustments for spikes that removed by grazing,etc..
            IF (TNUM.GT.0.0) THEN
              GWAM = GWAM * (TNUM-SPNUMHC)/TNUM
              GNOAM = GNOAM * (TNUM-SPNUMHC)/TNUM
            ENDIF  

            RWAM = RTWTM*PLTPOP*10.0
            SDWAM = (SEEDRS+SDCOAT)*PLTPOP*10.0

            IF (CWAM.GT.0.0) THEN
              HIAM = HIAD
              GNOWTM = GNOAM/(CWAM*0.1)
            ENDIF

            SENWACM = SENWAL(0)+SENWAS

            RSWAM = RSWAD

            CNAM = CNAD
            GNAM = GNAD
            GNPCM = GRAINANC*100.0
            VNAM = VNAD
            VNPCM = VANC*100.0
            RNAM = RNAD
            SRNAM = SRNAD

            HINM = HIND

            ! Set harvest product outputs
            IF (HPROD.EQ.'SR') THEN
              HWAM = SRWT * PLTPOP * 10.0
              HNAM = SRNAM
              IF (SRNOPD.GT.0.0) HWUM = SRWT/FLOAT(SRNOPD)
              HNUMAM = FLOAT(SRNOPD)*PLTPOP
              HNUMGM = FLOAT(SRNOPD)
              HNUMPM = FLOAT(SRNOPD)
              IF (SRWT.GT.0.0) HNPCM = SROOTN/SRWT*100.0
            ELSE
              HWAM = GWAM
              HNAM = GNAM
              HWUM = GWUM
              HNUMAM = GNOAM
              HNUMGM = GNOGM
              HNUMPM = GNOPM
              HNPCM = GNPCM
            ENDIF


          ENDIF

!=======================================================================
        ENDIF  ! End of after planted (integrate) section
!=======================================================================

!***********************************************************************
      ELSEIF (DYNAMIC.EQ.OUTPUT .AND. STEP.EQ.STEPNUM. OR.
     &        DYNAMIC.EQ.SEASEND .AND. SEASENDOUT.NE.'Y') THEN
!***********************************************************************

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
        !   Y->Leaves+Phases+Measured                 
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
          gfdur = -99
        ENDIF

        DAS = MAX(0,CSTIMDIF(YEARSIM,YEARDOY))

        LLWADOUT = LLWAD+LLRSWAD
        STWADOUT = STWAD+STRSWAD + LSHWAD+LSHRSWAD
        CHWADOUT = CHWAD+CHRSWAD
        ! Note other possibilities. To introduce must recompile.
        !LLWADOUT = LLWAD
        !STWADOUT = STWAD + LSHWAD
        !STWADOUT = STWAD
        !CHWADOUT = CHWAD
        SLAOUT = -99.0
        IF (LFWT.GT.1.0E-6) SLAOUT = 
     &   (PLA-SENLA-LAPHC) / (LFWT*(1.0-LSHFR)+LLRSWT)
        IF (SLA.LE.0.0) SLAOUT = -99.0
        
        CALL Csopline(senw0c,(senwal(0)))
        CALL Csopline(senwsc,(senwas))
        CALL Csopline(laic,lai)
        CALL Csopline(caic,caid)
        CALL Csopline(hindc,hind)
        CALL Csopline(hwudc,hwud)
        CALL Csopline(sdwadc,sdwad)
        CALL Csopline(gstagec,gstage)
        
        ! Calculate Parif to equate to updated LAI
        PARIFOUT = (1.0 - EXP((-KCAN)*LAI))

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
            WRITE(fnumwrk,*)' '
            WRITE(fnumwrk,'(A25,I16)')
     &       ' Days after planting DAP ',DAP
            WRITE(fnumwrk,'(A25,I16,I7)')
     &       ' Year,day                ',YEAR,DOY
            WRITE(fnumwrk,'(A34,2F7.3)')
     &       ' Rainfall,Irrigation mm           ',rain,irramt
            WRITE(fnumwrk,'(A34,2F7.3)')
     &       ' Tair,Tcan oC                     ',tmean,tcan
            WRITE(fnumwrk,'(A34,2F7.3)')
     &       ' Tcan-Tair Today and average oC   ',tcan-tmean,tdifav
            WRITE(fnumwrk,'(A34,F7.1)')
     &       ' Windspeed m/s                    ',windsp     
            WRITE(fnumwrk,'(A34,4F7.3)')
     &       ' EOsoil,EOp330,EOPpco2            ',eosoil,eop330,eopco2
            WRITE(fnumwrk,'(A34,4F7.3)')
     &       ' EOEBsoil,EOEBp330,EOEBPpco2      ',
     &       eoebud,eoebudcrp,eoebudcrp2
            IF (Rlf.GT.0.0) THEN
              WRITE(fnumwrk,'(A34,2F7.3,2F7.1)')
     &         ' Ratm,Rsoil,Rcrop,Rcrop*Rco2/R    ',
     &        ratm,rsoil,rcrop,rcrop*rlfc/rlf
            ELSE
              WRITE(fnumwrk,'(A34,2F7.3,2F7.1)')
     &         ' Ratm,Rsoil,Rcrop                 ',
     &        ratm,rsoil,rcrop                         
            ENDIF
            IF (FILEIOT.NE.'XFL') THEN
             IF (IDETL.EQ.'D'.OR.IDETL.EQ.'A') THEN
              WRITE(fnumwrk,'(A34,F7.3,F7.1)')
     &         ' EO Model Daily,Cumulative         ',
     &         eo,eoc                       
              WRITE(fnumwrk,'(A34,4F7.3)')
     &         ' EO P-T,Penman,M-Penman,Ebudget   ',
     &         eopt,eopen,eompen,eoebud
              WRITE(fnumwrk,'(A34,4F7.1)')
     &         ' EOSum P-T,Penman,M-Penman,Ebudget',
     &         eoptc,eopenc,eompenc,eoebudc
             ENDIF
            ENDIF
            WRITE(fnumwrk,'(A34,2F7.3)')
     &       ' Rstage at beginning,end of day   ',rstagep,rstage
            WRITE(fnumwrk,'(A34,2F7.1)')
     &       ' Leaf no at start,end of day      ',lnumprev,lnum
            IF (CROP.EQ.'BA'.AND.EYEARDOY.EQ.YEARDOY)
     &       WRITE(fnumwrk,'(A36,F5.1,A9,F5.1)')
     &        ' Phyllochron interval calculated as ',
     &        77.5-232.6*(DAYL-DAYLPREV),
     &        ' Read as ',phints
            IF (CUMDU.GE.LGPHASEDU(1).AND.CUMDU.LT.LGPHASEDU(2)) THEN
              WRITE(fnumwrk,'(A36,F5.1,F7.1)')
     &         ' Phyllochron interval. Std.,actual  ',phints,phintout
            ENDIF 
            IF (PLA-SENLA-LAPHC.LT.9999.9) THEN
              WRITE(fnumwrk,'(A34,F7.1,F7.1)')
     &        ' Laminae area end day /m2,/plant  ',lai,pla-senla-laphc
            ELSE
              WRITE(fnumwrk,'(A34,F7.1,I7)')
     &        ' Laminae area end day /m2,/plant  ',
     &        lai,NINT(pla-senla-laphc)
            ENDIF
            IF (EYEARDOY.LE.YEARDOY) THEN
              WRITE(fnumwrk,'(A34,F7.1,F7.1)')
     &         ' RA crop,RS crop (standard)       ',ratm,rcrop
              WRITE(fnumwrk,'(A34,2F7.1)')
     &         ' RS with CO2,H2O effects          ',
     &         rsco2,AMIN1(9999.9,rsadj)
              WRITE(fnumwrk,'(A34,F7.3)')
     &         ' Pot.pl.evap/Pot.soil evap        ',epsratio
              WRITE(fnumwrk,'(A34,F7.3)')
     &         ' Pot.pl.evap/Pot.pl.evap330       ',tratio
              WRITE(fnumwrk,'(A26,I1,A7,2F7.3)')
     &         ' PARIF,competition model,C',CN,' 1-crop',PARIF,PARIF1
              WRITE(fnumwrk,'(A34,F7.3)')
     &         ' Quantum requirement              ',photqr
              WRITE(fnumwrk,'(A34,2F7.1)')
     &         ' CO2,Estimated internal CO2 vpm   ',co2,co2intppm
              WRITE(fnumwrk,'(A34,F7.3,6F7.3)')
     &         ' Phs facs Co2,Temp,H2o,N,Rsvs,Vpd ',
     &         CO2FP,TFP,WFP,NFP,RSFP,VPDFP
              WRITE(fnumwrk,'(A34,3F7.3)')
     &         ' Phs. Rue,Rue+Co2i,Resistances    ',
     &         carbobegp*pltpop,carbobegi*pltpop,carbobegr*pltpop
              WRITE(fnumwrk,'(A34,3F7.2)')
     &         ' CH2O Start,end,remobilized       ',
     &         carbobeg*pltpop*10.,
     &         carboend*pltpop*10.0,senlfgrs*pltpop*10.0
              IF (CUMDU.GE.PSTART(MSTG-1).AND.
     &         CUMDU-DU.LT.PSTART(MSTG-1).AND.HPROD.NE.'SR') THEN
                 WRITE(fnumwrk,*)' '
                 WRITE(fnumwrk,'(A34,F7.2)')
     &            ' Above ground weight at grain set ',plwtgs      
                 WRITE(fnumwrk,'(A34,2F7.2)')
     &            ' Kernel wt standard,adjusted      ',gwts,gwta
                 WRITE(fnumwrk,'(A34,F7.3)')
     &            ' Kernel wt at start of linear fill',GWUDELAG
                 WRITE(fnumwrk,*)' '
              ENDIF
              IF (CUMDU.GE.PSTART(MSTG-2).AND.
     &         CUMDU-DU.LT.PSTART(MSTG-2)) THEN
                 WRITE(fnumwrk,'(A34, I7)')
     &            ' KERNEL SET (no/m2)               ',
     &            NINT(gnopd*pltpop)
                 WRITE(fnumwrk,'(A34,2F7.1)')
     &            ' Canopy wt at end of day (kg/ha)  ',
     &            (lfwt+stwt+rswt)*pltpop*10.0
              ENDIF
              IF(CUMDU.LE.PDSRI.AND.CUMDU+DU.GT.PDSRI.AND.
     &           SRDAYFR.GT.0.0)THEN
                 WRITE(fnumwrk,'(A34, I7)')
     &            ' STORAGE ROOT INITIATION (no/pl)  ',srnopd
                 WRITE(fnumwrk,'(A34,2F7.1)')
     &            ' Canopy wt at end of day (kg/ha)  ',
     &            (lfwt+stwt+rswt)*pltpop*10.0
                 WRITE(fnumwrk,'(A34,F7.1)')
     &            ' Storage root fraction            ',srfr
              ENDIF
              IF (CARBORRS.GT.0.0) WRITE(FNUMWRK,'(A34)')
     &         ' Surplus assimilates sent to roots'
              IF (LRTIP.EQ.1) WRITE(fnumwrk,'(A21)')
     &         ' Root tip in layer 1 '
              IF (NUPAP.LT.ANDEM) WRITE(fnumwrk,'(A38,F7.2)')
     &         ' N uptake shortage                    ',andem-nupap
              WRITE(FNUMWRK,'(A38,F7.2)')
     &         ' N demand (kg/ha)                     ',andem 
              IF (ANDEM.LE.0.0) THEN
                WRITE(FNUMWRK,'(A44)')
     &            ' N demand at zero! Components of demand/use:' 
              ELSE
                WRITE(FNUMWRK,'(A47)')
     &            ' N demand above zero! Components of demand/use:' 
              ENDIF  
              !ANDEM = PLTPOP*10.0*
              ! (GRAINNDEM+LSNDEM+RNDEM+SRNDEM-SEEDNUSE-RSNUSE)
              WRITE(FNUMWRK,*)
     &          ' Grain             ',grainndem*pltpop*10.0
              WRITE(FNUMWRK,*)
     &          ' Leaves and stem   ',lsndem*pltpop*10.0
              WRITE(FNUMWRK,*)
     &          ' Roots             ',rndem*pltpop*10.0
              WRITE(FNUMWRK,*)
     &          ' Storage root      ',srndem*pltpop*10.0
              WRITE(FNUMWRK,*)
     &          ' Seed use          ',seednuse*pltpop*10.0
              WRITE(FNUMWRK,*)
     &          ' Reserves use      ',rsnuse*pltpop*10.0
              IF (ANDEM.GT.0.0.AND.NUPAP.LT.ANDEM)
     &         WRITE(fnumwrk,'(A38)')
     &          '  N uptake insufficient to meet demand'
              IF (ANDEM.GT.10.0)
     &         WRITE(fnumwrk,'(A11,F4.1,A23,F4.1)')
     &         ' N demand (',ANDEM,') very high! Uptake = ',nuf*andem
              IF (ISWNIT.NE.'N'.AND.GRWTTMP.GT.0.0.AND.
     &         GRAINNTMP/GRWTTMP*100.0.LT.GRNMN)
     &          WRITE(fnumwrk,'(A45,F4.2)')
     &           ' N limit on grain growth. N pc at minimum of ',grnmn
              IF (GROGRP-GROGR.GT.0.0) WRITE(fnumwrk,'(A34,2F7.3)')
     &         ' Grain CH2O shortage (mg/p,mg/k)  ',
     &         (GROGRP-GROGR)*1000.0,(GROGRP-GROGR)*1000.0/GNOPD
              IF (CCOUNTV.EQ.1) WRITE (fnumwrk,'(A35,I4)')
     &         ' Maximum leaf number reached on day',DOY
              IF (CCOUNTV.EQ.50) WRITE (fnumwrk,'(A47,/,A44,/A26)')
     &         ' 50 days after maximum leaf number! Presumably ',
     &         ' vernalization requirement could not be met ',
     &         ' Will assume crop failure.'
              IF (TILDAT.LE.0.0.AND.TNUM.GT.1.0)
     &         WRITE (fnumwrk,'(A18)')' Tillering started'
              IF (CROP.EQ.'MZ'.AND.
     &             CUMDU+DU.GE.PSTART(1).AND.CUMDU.LE.PSTART(1)) THEN
                WRITE(fnumwrk,'(A26,F6.1)')
     &           ' Phase adjustment         ',(PDADJ-1.0)*PD(2)
                WRITE(fnumwrk,'(A22)') ' Phase New_end'
                DO L = 3,MSTG
                  WRITE(fnumwrk,'(I6,F8.1)') L,PSTART(L)
                ENDDO
              ENDIF
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
 2201         FORMAT ('@YEAR DOY   DAS   DAP TMEAN TKILL',
     A               '  GSTD  L#SD',
     B               ' PARID PARUD  AWAD',
     C               '  LAID  SAID  CAID',
     D               '  TWAD SDWAD  RWAD  CWAD  LWAD  SWAD  HWAD  HIAD',
     E               ' CHWAD  EWAD RSWAD SNWPD SNW0D SNW1D',
     F               '  RS%D',
     G               '  H#AD  HWUD',
     I               '  T#AD  LAWD  RDPD  PTFD',
     J               '  SWXD WAVRD',
     K               ' WUPRD  WFTD  WFPD  WFGD',
     L               '  NFTD  NFPD  NFGD NUPRD',
     M               '  TFPD  TFGD',
     N               ' VRNFD DYLFD',
     O               '      ',
     P               '      ')
            ENDIF  ! End Plantgro header writes
            WRITE (NOUTPG,501)
     A      YEAR,DOY,DAS,DAP,TMEAN,TKILL,GSTAGEC,LNUM,
     B      PARIFOUT,PARIUE,AMIN1(999.9,CARBOBEG*PLTPOP*10.0),
     &      LAIC,SAID,CAIC,
     C      NINT(TWAD),NINT(SDWAD),NINT(RWAD),NINT(CWAD),
     D      NINT(LLWADOUT),NINT(STWADOUT),NINT(HWAD),HIAD,
     E      NINT(CHWADOUT),NINT(EWAD),NINT(RSWAD),
     &      NINT(DEADWAD),SENW0C,SENWSC,
     F      RSCD*100.0,NINT(HNUMAD),HWUDC,
     G      NINT(TNUMAD),NINT(SLAOUT),RTDEP/100.0,PTF,H2OA,
     H      AMIN1(99.9,WAVR),AMIN1(15.0,WUPR),
     I      1.0-WFT,1.0-WFP,1.0-WFG,
     J      1.0-NFT,1.0-NFP,1.0-NFG,AMIN1(2.0,NUPRATIO),
     K      1.0-TFP,1.0-TFG,1.0-VF,1.0-DFOUT
  501       FORMAT(
     A      I5,I4,2I6,F6.1,F6.1,A6,F6.1,F6.3,F6.2,
     B      F6.1,A6,F6.3,A6,
     C      7I6,F6.3,4I6,2A6,
     D      F6.1,I6,A6,
     E      2I6,2F6.2,F6.1,
     F      F6.1,F6.2,
     G      3F6.2,
     H      3F6.2,F6.1,
     I      2F6.2,
     J      2F6.2)
            ! End Plantgro writes
            
            ! PlantGroReductionFactors
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
     1               ' .......PHOTOSYNTHESIS....... ', 
     M               ' .....GROWTH.....  ..TILLERS. ',
     2               'WATER STRESS DETERMINANTS',
     2               ' N STRESS DETERMINANTS        ')
              WRITE (NOUTPGF,2205)
 2205         FORMAT ('@YEAR DOY   DAS   DAP TMEAN TCDIF  GSTD',
     N               '    DU VRNFD DYLFD',
     1               '  TFPD  WFPD  NFPD CO2FD RSFPD', 
     M               '  TFGD  WFGD  NFGD  WFTD  NFTD',
     &               ' WAVRD WUPRD  SWXD  EOPD',
     &               '  SNXD LN%RD SN%RD RN%RD            ')
            ENDIF  ! End Plantgro header writes
            WRITE (NOUTPGF,507)
     A      YEAR,DOY,DAS,DAP,TMEAN,TCAN-TMEAN,GSTAGEC,
     B      DU,1.0-VF,1.0-DFOUT,
     C      1.0-TFP,1.0-WFP,1.0-NFP,1.0-CO2FP,1.0-RSFP,
     D      1.0-TFG,1.0-WFG,1.0-NFG,1.0-WFT,1.0-NFT,
     H      AMIN1(99.9,WAVR),AMIN1(15.0,WUPR),H2OA,EOP,
     I      SNO3PROFILE+SNH4PROFILE,LNCR,SNCR,RNCR
  507       FORMAT(
     a      I5,I4,2I6,2F6.1,A6,
     b      F6.1,2F6.2,
     c      5F6.2,
     d      5F6.2,
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
     A          ' LAIPD LAISD  LAID  CHTD SDWAD SNW0D SNW1D',
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
     A       YEAR,DOY,DAS,DAP,TMEAN,GSTAGEC,RSTAGE,
     B       LAIPRODC,SENLA*PLTPOP*0.0001,LAIC,CANHTC,SDWAD,
     &       SENW0C,SENWSC,
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
                OPEN (UNIT = NOUTPGN, FILE = OUTPN, STATUS='UNKNOWN',
     &          POSITION = 'APPEND')
                IF (FILEIOT(1:2).EQ.'DS') THEN
                  CALL HEADER(2, NOUTPGN, RUN)
                ELSE
                  WRITE (NOUTPGN,'(/,A79,/)') OUTHED
                  WRITE (NOUTPGN,103) MODEL
                  WRITE (NOUTPGN,1031) MODNAME
                  WRITE (NOUTPGN,104)
     &             EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
                  WRITE (NOUTPGN,102) TN,TNAME
                  WRITE (NOUTPGN,107) CROP,VARNO,VRNAME
                  WRITE(NOUTPGN,108)
     &             month,dom,plyeardoy,NINT(pltpopp),NINT(rowspc)
                ENDIF 
                WRITE (NOUTPGN,2252)
 2252           FORMAT ('@YEAR DOY   DAS   DAP TMEAN  GSTD  NUAD',
     A           '  TNAD SDNAD  RNAD  CNAD  LNAD  SNAD  HNAD  HIND',
     F           ' RSNAD SNNPD SNN0D SNN1D',
     B           '  RN%D  LN%D  SN%D  HN%D SDN%D  VN%D',
     C           ' LN%RD SN%RD RN%RD  NFPD  NFGD NUPRD',
     D           ' NDEMD')
              ENDIF  ! Plantn header writes
              CALL Csopline(senn0c,sennal(0))
              CALL Csopline(sennsc,sennas)
              WRITE (NOUTPGN,503)
     A         YEAR,DOY,DAS,DAP,TMEAN,GSTAGEC,NUPAC,
     B         TNAD,SDNAD,RNAD,CNAD,LLNAD,SNAD,HNAD,HINDC,
     H         RSNAD,DEADNAD,SENN0C,SENNSC,
     C         RANC*100.0,LANC*100.0,SANCOUT*100.0,
     D         HNC*100.0,SDNC*100.0,AMIN1(9.9,VANC*100.0),
     E         LNCR,SNCR,RNCR,
     &         1.0-NFP,1.0-NFG,
     F         AMIN1(2.,NUPRATIO),ANDEM
  503          FORMAT(
     1         I5,I4,2I6,F6.1,A6,F6.1,
     2         F6.1,2F6.2,4F6.1,A6,
     3         2F6.2,2A6,
     4         3F6.3,
     5         3F6.3,
     6         3F6.3,
     2         2F6.3,
     8         F6.2,F6.1)
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
     &   IPLTI.EQ.'A') THEN
     
          ! Automatic planting
          WRITE (fnumwrk,*) 'Water thresholds ',swpltl,swplth
          WRITE (fnumwrk,*) 'Water ',avgsw
          WRITE (fnumwrk,*) 'Temperature thresholds ',pttn,ptx
          WRITE (fnumwrk,*) 'Temperature ',tsdep

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
          
            adatm = -99
            adapm = -99
            cnaam = -99
            cnamm = -99
            cnpcmm = -99
            cwaam = -99
            cwamm = -99
            deadwamm = -99.0
            edatm = -99
            edapm = -99
            gdapm = -99
            tildapm = -99
            sgedapm = -99
            aedapm = -99
            gfdapm = -99
            gdatm = -99
            gnamm = -99
            hnamm = -99
            hnpcmm = -99
            hiamm = -99
            hinmm = -99
            gnoamm = -99
            gnogmm = -99
            hnumamm = -99
            srnoamm = -99
            srnogmm = -99
            srnogmm = -99
            gnpcmm = -99
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
            tnumamm = -99
            tnumpmm = -99
            psdatm = -99
            ssdatm = -99
            vnamm = -99
            vnpcmm = -99
            vwamm = -99
            laixt = -99.0
            valuer = -99.0
                      
            ! Reading A-file
            CALL LTRIM2 (FILEIO,filenew)
            FILELEN = TVILENT(FILENEW)
            FILELEN = MAX(FILELEN-12, 0) 

            IF (TVILENT(FILEADIR).GT.3) THEN
              FILEA = FILEADIR(1:TVILENT(FILEADIR))//
     &        SLASH //EXCODE(1:8)//'.'//EXCODE(9:10)//'A'
            ELSE
              FILEA = FILENEW(1:FILELEN-12)//EXCODE(1:8)//'.'//
     &        EXCODE(9:10)//'A'
            ENDIF       
            FEXISTA = .FALSE.
            INQUIRE (FILE = FILEA,EXIST = FEXISTA)
            IF (.not.FEXISTA) THEN
              Messageno = Min(Messagenox,Messageno+1)
              WRITE (Message(Messageno),'(A23,A50)')
     &         'Could not find A-file: ',filea(1:50)
              Messageno = Min(Messagenox,Messageno+1)
              WRITE (Message(Messageno),'(A23,A50)')
     &         'Experiment file:       ',fileio(1:50)
              OPEN (UNIT=FNUMTMP, FILE=FILEA, STATUS = 'UNKNOWN')
              CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
            ELSE
              ! Yield at maturity  
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HWAM',hwamm)
              IF (hwamm.GT.0.0.AND.HWAMM.LT.50.0) HWAMM = HWAMM*1000.0
              IF (hwamm.LE.0.0)THEN
                CALL AREADR (FILEA,TN,RN,SN,ON,CN,'GWAM',gwamm)
                IF (GWAMM.GT.0.0) THEN
                  IF (gwamm.GT.0.0.AND.GWAMM.LT.50.0) GWAMM=GWAMM*1000.0
                  HWAMM = GWAMM
                ENDIF  
              ENDIF  
              IF (HWAMM.LE.0.0) THEN
                CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HYAM',hyamm)
                IF (hyamm.LE.0.0) THEN
                  CALL AREADR (FILEA,TN,RN,SN,ON,CN,'GYAM',hyamm)
                ENDIF  
                IF (hyamm.GT.0.0.AND.HYAMM.LT.50.0) HYAMM = HYAMM*1000.0
              ENDIF
              IF (HWAMM.LE.0.0.AND.HYAMM.GT.0..AND.HMPC.GT.0.0)
     &          HWAMM = HYAMM * (1.0-HMPC/100.0)
              
              ! Yield at harvest
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'GWAH',gwahm)
             
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HWUM',hwumm)
              IF (hwumm.le.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'GWUM',gwumm)
          
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'LAIX',laixm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CWAM',cwamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'BWAH',vwamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CWAA',cwaam)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'T#AM',tnumamm)
              IF (tnumamm.le.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'TNOAM',tnumamm)
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
          
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CNAM',cnamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'VNAM',vnamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CNAA',cnaam)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HNAM',hnamm)
              IF (hnamm.LE.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'GNAM',gnamm)
              IF (HNAMM.LE.0.0) HNAMM = GNAMM
              IF (HNAMM.LE.0.0) HNAMM = -99   
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CN%M',cnpcmm)
              IF (cnpcmm.le.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CNPCM',cnpcmm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HN%M',hnpcmm)
              IF (hnpcmm.le.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HNPCM',hnpcmm)
              IF (hnpcmm.le.0.0) THEN
                CALL AREADR (FILEA,TN,RN,SN,ON,CN,'GN%M',gnpcmm)
                IF (gnpcmm.le.0.0)
     &           CALL AREADR (FILEA,TN,RN,SN,ON,CN,'GNPCM',gnpcmm)
              ENDIF
              IF (HNPCMM.LE.0.0) HNPCMM = GNPCMM
              IF (HNPCMM.LE.0.0) HNPCMM = -99   
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'VN%M',vnpcmm)
              IF (vnpcmm.le.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'VNPCM',vnpcmm)
          
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HIAM',hiamm)
              IF (HIAMM.GE.1.0) HIAMM = HIAMM/100.0
          
              CALL AREADI (FILEA,TN,RN,SN,ON,CN,'EDAT',edatm)
              CALL AREADI (FILEA,TN,RN,SN,ON,CN,'GDAT',gdatm)
          
              IF (HWUMM.LE.0.0) HWUMM = GWUMM
              IF (HNAMM.LE.0.0) HNAMM = GNAMM
          
              DO L = 1,PSNUM              
               CALL AREADI (FILEA,TN,RN,SN,ON,CN,psabv(l),psdatm(l))
               IF (PSDATM(L).GT.0.0.AND.PSDATM(L).LT.1000) THEN
                 CALL AREADI (FILEA,TN,RN,SN,ON,CN,'YEAR',yearm)
                 IF (YEARM.GT.0.0) PSDATM = CSYDOY(YEARM,PSDATM(L))
               ENDIF
               psdapm(l) = Dapcalc(psdatm(l),plyear,plday)
              ENDDO
              DO L = 1,SSNUM
               CALL AREADI (FILEA,TN,RN,SN,ON,CN,ssabv(l),ssdatm(l))
               IF (SSDATM(L).GT.0.0.AND.SSDATM(L).LT.1000) THEN
                 CALL AREADI (FILEA,TN,RN,SN,ON,CN,'YEAR',yearm)
                 IF (YEARM.GT.0.0) SSDATM = CSYDOY(YEARM,SSDATM(L))
               ENDIF
               ssdapm(l) = Dapcalc(ssdatm(l),plyear,plday)
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
                Messageno = Min(Messagenox,Messageno+1)
                WRITE (Message(Messageno),'(A23,A50)')
     &          'Could not find T-file: ',filet(1:50)
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
                      TNUMCOL = Tvicolnm(linet,'T#AD')
                      LNUMCOL = Tvicolnm(linet,'L#SD')
                      CWADCOL = Tvicolnm(linet,'CWAD')
                      HWADCOL = Tvicolnm(linet,'HWAD')
                      HIADCOL = Tvicolnm(linet,'HIAD')
                      !HWTUCOL = Tvicolnm(linet,'HWNOD')
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
                          CALL Getstrr (LINET,TNUMCOL,VALUER)
                          IF (VALUER.GT.0.0) TNUMT = VALUER
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
                          IF (VALUER.GT.FLOAT(ASTG*10).AND.ADATT.LE.0)
     &                      ADATT = DATE
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
                  Messageno = Min(Messagenox,Messageno+1)
                  WRITE (Message(Messageno),'(A23,A50)')
     &             'T-file was empty '
                ENDIF
                CLOSE(FNUMT)
                CLOSE(FNUMMEAS)
              ENDIF ! End t-file reads,measured.out writes
              
              IF (IDETL.EQ.'A') THEN
                ! Use T-data if A-data missing (whem output=all)
                IF (FEXISTT) THEN
                  WRITE(Fnumwrk,*)' '
                  WRITE(Fnumwrk,'(A27)') ' EXAMINING TIME-COURSE DATA'
                  IF (HWAMM.LE.0.0) THEN
                    IF (HWADT.GT.0.0) THEN
                      HWAMM = HWADT
                      Messageno = Min(Messagenox,Messageno+1)
                      WRITE(Message(messageno),'(A30)')
     &                 'Time-course data used for HWAM'
                      WRITE(Fnumwrk,'(A32)')
     &                 '  Time-course data used for HWAM'
                    ENDIF
                  ELSE
                    IF (HWADT.GT.0.0) THEN
                      IF (ABS(100.0*ABS(HWAMM-HWADT)/HWAMM).GT.0.0) THEN
                      Messageno = Min(Messagenox,Messageno+1)
                      WRITE(Message(messageno),'(A48,F8.2)')
     &                'Pc difference between final,time-course yields ='
     &                ,100.0*ABS(HWAMM-HWADT)/HWAMM
                      Messageno = Min(Messagenox,Messageno+1)
                      WRITE(Message(messageno),'(A20,I6)')
     &                'Final yield         ',NINT(HWAMM)
                      Messageno = Min(Messagenox,Messageno+1)
                      WRITE(Message(messageno),'(A20,I6)')
     &                'Time-course yield   ',NINT(HWADT)
                      ENDIF
                    ENDIF
                  ENDIF
                  IF (CWAMM.LE.0.0) THEN
                    IF (CWADT.GT.0.0) THEN
                      CWAMM = CWADT
                      Messageno = Min(Messagenox,Messageno+1)
                      WRITE(Message(messageno),'(A31)')
     &                 'Time-course data used for CWAMM'
                      WRITE(Fnumwrk,'(A33)')
     &                 '  Time-course data used for CWAMM'
                    ENDIF
                  ELSE
                    IF (CWADT.GT.0.0) THEN
                      IF (ABS(100.0*ABS(CWAMM-CWADT)/CWAMM).GT.0.0) THEN
                      Messageno = Min(Messagenox,Messageno+1)
                      WRITE(Message(messageno),'(A48,F8.2)')
     &                'Pc difference between final,time-course canopy ='
     &                ,100.0*ABS(CWAMM-CWADT)/CWAMM
                      Messageno = Min(Messagenox,Messageno+1)
                      WRITE(Message(messageno),'(A19,I6)')
     &                'Final canopy       ',NINT(CWAMM)
                      Messageno = Min(Messagenox,Messageno+1)
                      WRITE(Message(messageno),'(A19,I6)')
     &                'Time-course canopy ',NINT(CWADT)
                      ENDIF
                    ENDIF
                  ENDIF
                  IF (LAIXM.LE.0.0.AND.LAIXT.GT.0.0) THEN
                    LAIXM = LAIXT
                    Messageno = Min(Messagenox,Messageno+1)
                    WRITE(Message(messageno),'(A31)')
     &               'Time-course data used for LAIXM'
                    WRITE(Fnumwrk,'(A33)')
     &               '  Time-course data used for LAIXM'
                  ENDIF
                  IF (LNUMSMM.LE.0.0.AND.LNUMSMM.GT.0.0) THEN
                    LNUMSMM = LNUMT
                    Messageno = Min(Messagenox,Messageno+1)
                    WRITE(Message(messageno),'(A33)')
     &               'Time-course data used for LNUMSMM'
                    WRITE(Fnumwrk,'(A35)')
     &               '  Time-course data used for LNUMSMM'
                  ENDIF
                  IF (TNUMAMM.LE.0.0.AND.TNUMT.GT.0.0) THEN
                    TNUMAMM = TNUMT
                    Messageno = Min(Messagenox,Messageno+1)
                    WRITE(Message(messageno),'(A33)')
     &               'Time-course data used for TNUMAMM'
                    WRITE(Fnumwrk,'(A35)')
     &               '  Time-course data used for TNUMAMM'
                  ENDIF
                  IF (HIAMM.LE.0.0.AND.HIADT.GT.0.0) THEN
                    HIAMM = HIADT
                    Messageno = Min(Messagenox,Messageno+1)
                    WRITE(Message(messageno),'(A31)')
     &               'Time-course data used for HIAMM'
                    WRITE(Fnumwrk,'(A33)')
     &               '  Time-course data used for HIAMM'
                  ENDIF
                  IF (HWUMM.LE.0.0.AND.HWUT.GT.0.0) THEN
                    HWUMM = HWUT
                    Messageno = Min(Messagenox,Messageno+1)
                    WRITE(Message(messageno),'(A31)')
     &               'Time-course data used for HWUMM'
                    WRITE(Fnumwrk,'(A33)')
     &               '  Time-course data used for HWUMM'
                  ENDIF
                  IF (HNUMAMM.LE.0.0.AND.HNUMAT.GT.0.0) THEN
                    HNUMAMM = HNUMAT
                    WRITE(Message(messageno),'(A31)')
     &               'Time-course data used for H#AT'
                    WRITE(Fnumwrk,'(A33)')
     &               '  Time-course data used for H#AT'
                  ENDIF
                  IF (HNUMGMM.LE.0.0.AND.HNUMET.GT.0.0) THEN
                    HNUMGMM = HNUMET
                    Messageno = Min(Messagenox,Messageno+1)
                    WRITE(Message(messageno),'(A32)')
     &               'Time-course data used for H#GMM'
                    WRITE(Fnumwrk,'(A34)')
     &               '  Time-course data used for H#GMM'
                  ENDIF
                ENDIF
                DO L = 1,PSNUM
                  IF (PSABV(L).EQ.'ADAT'.AND.PSDATM(L).LE.0.0) THEN
                    IF (ADATT.GT.0) THEN
                      PSDATM(L) = INT(ADATT)
                      Messageno = Min(Messagenox,Messageno+1)
                      WRITE(Message(messageno),'(A31)')
     &                 'Time-course data used for ADATM'
                      WRITE(Fnumwrk,'(A33)')
     &                 '  Time-course data used for ADATM'
                    ENDIF  
                  ENDIF
                  IF (PSABV(L).EQ.'MDAT'.AND.PSDATM(L).LE.0.0) THEN
                    IF (MDATT.GT.0) THEN
                      PSDATM(L) = INT(MDATT)
                      Messageno = Min(Messagenox,Messageno+1)
                      WRITE(Message(messageno),'(A31)')
     &                 'Time-course data used for MDATM'
                      WRITE(Fnumwrk,'(A33)')
     &                 '  Time-course data used for MDATM'
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
            
            ! Product wt at maturity
            IF (hwahm.GT.0.AND.hwamm.LE.0) hwamm = hwahm/(hpcf/100.0)
            
            ! Product wt at harvest
            IF (hwamm.GT.0.AND.hwahm.LE.0) hwahm = hwamm*(hpcf/100.0)
            
            ! Canopy wt at maturity
            IF (vwamm.GT.0.AND.hwamm.GT.0) cwamm = vwamm+hwamm
            
            ! Vegetative wt at maturity
            IF (HPROD.NE.'SR') THEN
              IF (hwamm.GT.0.AND.cwamm.GT.0) vwamm = cwamm-hwamm
            ELSE
              IF (cwamm.GT.0) vwamm = cwamm
            ENDIF
            
            ! Harvest index at maturity
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
                    WRITE (fnumwrk,*) 'Reported HI not consistent',
     &               ' with yield and total weight data  '
                    WRITE (fnumwrk,*) ' Reported HI   ',hiamm
                    WRITE (fnumwrk,*) ' Calculated HI ',hiammtmp
                    WRITE (fnumwrk,*) ' Will use reported value '
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
            
            ! Product unit wt at maturity
            IF (hwumm.GT.1.0) hwumm = hwumm/1000.0 ! mg->g
            IF (hwumm.LE.0.AND.hnumamm.GT.0) THEN
              IF (hwamm.GT.0.0) hwumm=hwamm*0.1/hnumamm  ! kg->g
            ELSE
              IF (hwamm.gt.0.0.AND.hnumamm.GT.0.0) THEN
                hwumyld = hwamm*0.1/hnumamm
                IF (ABS(hwumyld-hwumm)/hwumm.GT.0.05) THEN
                  WRITE (fnumwrk,*)' '
                  WRITE (fnumwrk,'(A14)')' MEASURED DATA'
                  WRITE (fnumwrk,'(A36,A33)')
     &              ' Reported product wt.not consistent',
     &              ' with yield and product # data   '
                  WRITE (fnumwrk,*) ' Reported wt   ',hwumm
                  WRITE (fnumwrk,*) ' Calculated wt ',hwumyld
                  WRITE (fnumwrk,*) '   Yield       ',hwamm
                  WRITE (fnumwrk,*) '   Kernel no   ',hnumamm
                  WRITE (fnumwrk,*) ' Will use reported value '
                ENDIF
              ENDIF
            ENDIF
            
            ! Product number at maturity
            IF (HNUMAMM.LE..0.AND.HNUMGMM.GT..0.AND.TNUMAMM.GT..0) THEN
              HNUMAMM = HNUMGMM * TNUMAMM
              Messageno = Min(Messagenox,Messageno+1)
              WRITE(Message(messageno),'(A39)')
     &         'ShootNo * product/shoot used for H#AMM'
            ENDIF
            IF (HNUMAMM.GT.0.0) THEN
              IF (PLTPOPP.GT.0) THEN 
                HNUMPMM = HNUMAMM/PLTPOPP
              ELSE
                HNUMPMM = -99.0
              ENDIF  
            ELSE
              HNUMPMM = -99.0
            ENDIF
            IF (HNUMGMM.LE.0.AND.TNUMAMM.GT.0.AND.HNUMAMM.GT.0) THEN
              HNUMGMM = HNUMAMM/TNUMAMM
              Messageno = Min(Messagenox,Messageno+1)
              WRITE(Message(messageno),'(A38)')
     &         'ProductNo/area/ShootNo used for H#GMM '
            ENDIF
            
            ! Tiller number at maturity
            IF (tnumamm.LE.0.AND.hnumamm.GT.0.AND.hnumgmm.GT.0)
     &         tnumamm = hnumamm/hnumgmm
            IF (pltpopp.GT.0..AND.tnumamm.GT.0.) tnumpmm=tnumamm/pltpopp
            
            ! Shoot/root ratio at maturity
            IF (rwamm.GT.0.0) shrtmm = cwamm/rwamm
            
            ! Reserves concentration at maturity
            IF (vwamm+rwamm.GT.0.AND.rswamm.GT.0.0)
     &       rscmm = rswamm/(vwamm+rwamm)
            
            ! Canopy N at maturity
            IF (vnamm.GT.0.AND.gnamm.GT.0.AND.cnamm.LE.0)
     &        cnamm = vnamm + gnamm
            
            ! Total N at maturity
            IF (CNAMM.GT.0.0.AND.RNAMM.GT.0.0) THEN
              tnamm = cnamm+rnamm
            ELSE
              tnamm = -99.0
            ENDIF
            
            ! Vegetative N at maturity
            IF (vnamm.LE.0) THEN
             IF (hnamm.GE.0.AND.cnamm.GT.0) vnamm=cnamm-hnamm
            ENDIF
            
            ! Product N harvest index at maturity
            IF (cnamm.GT.0.AND.hnamm.GT.0) hinmm=hnamm/cnamm
            
            ! Vegetative N concentration at maturity
            IF (vnpcmm.LE.0) THEN
             IF (vwamm.GT.0.AND.vnamm.GT.0) vnpcmm = (vnamm/vwamm)*100
            ENDIF
            
            ! Product N concentration at maturity
            IF (hnpcmm.LE.0) THEN
             IF (hwamm.GT.0.AND.hnamm.GT.0) hnpcmm = (hnamm/hwamm)*100
            ENDIF
            
            ! Leaf N concentration at maturity
            IF (cnpcmm.LE.0.AND.cnamm.GT.0.AND.cwamm.GT.0.0)
     &        cnpcmm = cnamm/cwamm
            
            ! Express dates as days after planting
            edapm = -99
            edapm = Dapcalc(edatm,plyear,plday)
            IF (edapm.GT.200) THEN
              Messageno = Min(Messagenox,Messageno+1)
              WRITE (Message(messageno),'(A31,A31,A11)')
     &         'Measured emergence over 200DAP ',
     &         'Maybe reported before planting.',
     &         'Check files'
            ENDIF
            gdapm = Dapcalc(gdatm,plyear,plday)
            adapm = Dapcalc(adatm,plyear,plday)
            IF (mdapm.LE.0) mdapm = Dapcalc(mdatm,plyear,plday)

            ! Write messages re.measured variables to Warning.out
            IF (Messageno.GT.0) THEN
              CALL WARNING(Messageno,'CSCRP',MESSAGE)
            ENDIF  
            Messageno = 0

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
                  WRITE (FNUMEVAL,993) 
     &             EVHEADER,EXCODE,ENAME(1:25),MODNAME
  993             FORMAT (A14,A10,'  ',A25,2X,A8,/)
                ELSE
                  WRITE (FNUMEVAL,*) ' '
                  IF (EVHEADNMMAX.GT.1) THEN
                    WRITE (FNUMEVAL,1995) EVHEADER,MODNAME, 
     &               'ALL REMAIN','ING EXPERIMENTS        '
                  ELSEIF (EVHEADNM.LE.EVHEADNMMAX) THEN
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
                DO L = 1,KEYSTX
                  IF (KEYSS(L).GT.0) THEN 
                    IF (SSABVO(KEYSS(L))(1:1).NE.' ') 
     &               WRITE (FNUMEVAL,'(A1)',ADVANCE='NO') ' '
                    WRITE (FNUMEVAL,'(A5,A1)',ADVANCE='NO') 
     &                      SSABVO(KEYSS(L)),'S'
                    IF (SSABVO(KEYSS(L))(1:1).NE.' ') 
     &               WRITE (FNUMEVAL,'(A1)',ADVANCE='NO') ' '
                    WRITE (FNUMEVAL,'(A5,A1)',ADVANCE='NO') 
     &                      SSABVO(KEYSS(L)),'M'
                  ENDIF
                ENDDO
!9941           FORMAT (A5,A1,A5,A1)
                WRITE (FNUMEVAL,9942)
 9942           FORMAT (
     &          ' HWAMS HWAMM HWUMS HWUMM',
     &          ' H#AMS H#AMM H#GMS H#GMM',
     &          ' LAIXS LAIXM L#SMS L#SMM',
     &          ' T#AMS T#AMM CWAMS CWAMM',
     &          ' HIAMS HIAMM HN%MS HN%MM VN%MS VN%MM',
     &          ' CNAMS CNAMM HNAMS HNAMM HINMS HINMM',
     &          ' CWACS CWACM ')
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
            DO L = 1,KEYSTX
              IF (KEYSS(L).GT.0) THEN 
                IF (SSABVO(KEYSS(L))(1:1).NE.' ') 
     &            WRITE (FNUMEVAL,'(A1)',ADVANCE='NO') ' '
                WRITE (FNUMEVAL,'(I6)',ADVANCE='NO') SSDAP(KEYSS(L))
                IF (SSABVO(KEYSS(L))(1:1).NE.' ') 
     &            WRITE (FNUMEVAL,'(A1)',ADVANCE='NO') ' '
                WRITE (FNUMEVAL,'(I6)',ADVANCE='NO') SSDAPM(KEYSS(L))
              ENDIF
            ENDDO
!8405       FORMAT (2I6)
            WRITE (FNUMEVAL,8406)
     F       NINT(hwam),NINT(hwamm),
     G       hwumchar,hwummchar,
     &       NINT(hnumam),NINT(hnumamm),hnumgm,hnumgmm,
     H       laix,laixm,lnumsm,lnumsmm,
     I       NINT(tnumam),NINT(tnumamm),
     J       NINT(cwam),NINT(cwamm),
     L       hiamchar,hiammchar,
     M       hnpcmchar,hnpcmmchar,vnpcmchar,vnpcmmchar,
     &       NINT(cnam),NINT(cnamm),NINT(hnam),NINT(hnamm),
     &       hinmchar,hinmmchar,
     N       NINT(cwahc),NINT(cwahcm)
 8406       FORMAT (
     A      I6,I6,
     G      A6,A6,I6,I6,F6.1,F6.1,
     H      F6.1,F6.1,F6.1,F6.1,
     I      I6,I6,
     J      I6,I6,
     L      A6,A6,
     M      A6,A6,A6,A6,
     &      I6,I6,I6,I6,
     &      A6,A6,
     &      I6,I6)
            Close(FNUMEVAL)
            ! End of Evaluation.Out writes

            ! Overview
            IF (IDETO.NE.'E') THEN  ! No Overview if only need Evaluate
              IF (FILEIOT(1:2).EQ.'DS') THEN
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
              ELSE
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
                WRITE (FNUMOV,2032) MEPHS
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
                CALL CSYR_DOY(EYEARDOY,YEAR,DOY)
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
                ENDIF
                ! LAH CHECK THIS .. ARE IN FILEIOT.NE.'DS4' !!!!
                IF (FILEIOT.EQ.'DS4'.AND.IDETL.EQ.'D'.OR.
     &           FILEIOT.EQ.'DS4'.AND.IDETL.EQ.'A'.OR.
     &           FILEIOT.NE.'DS4') THEN                  
                  WRITE(fnumov,2089)eoc/10.0,eopenc/10.0,eompenc/10.0
 2089             FORMAT (' POTENTIAL ET cm  ','Crop model:',F5.1,
     &             '  Penman:    ',F9.1,
     &             '  Penman-M:  ',F7.1)
                  WRITE(fnumov,2097)eoptc/10.0,eoebudc/10.0
 2097             FORMAT ('                  ','Priestley: ',F5.1,
     &             '  E.budget:  ',F9.1)
                ENDIF
              ENDIF 
              ! End of Overview header writes
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
                  CALL CSYR_DOY(STGYEARDOY(L),YEAR,DOY)
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
     &            NINT(CNADSTG(L)),CNCTMP,1.0-WFPPAV(L),1.0-NFPPAV(L)
                ENDIF
              ENDDO
              ! For harvest at specified date
              IF (YEARDOYHARF.EQ.YEARDOY) THEN
                CALL Csopline(laic,lai)
                  CALL CSYR_DOY(YEARDOYHARF,YEAR,DOY)
                  CALL Calendar(year,doy,dom,month)
                  CNCTMP = 0.0
                  IF (CWAD.GT.0.0)CNCTMP = CNAD/CWAD*100
                  WRITE (FNUMOV,'(I8,I4,1X,A3,I4,1X,I1,1X,A13,I6,A6,
     &            F6.1,I6,F6.2,F6.2,F6.2)')
     &            YEARDOY,DOM,MONTH,
     &            Dapcalc(yeardoy,(plyeardoy/1000),plday),
     &            l,'Harvest      ',
     &            NINT(CWAD),LAIC,LNUM,
     &            NINT(CNAD),CNCTMP,1.0-WFPCAV,1.0-NFPCAV
              ENDIF 
              IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
               WRITE(fnumov,*)' '
               WRITE(fnumov,*)
     &          'BIOMASS  = Above-ground dry weight (kg/ha)'
               WRITE(fnumov,*)'LEAF AREA  = Leaf area index (m2/m2)'
               WRITE(fnumov,*)
     &          'LEAF NUMBER  = Leaf number produced on main axis'
               WRITE(fnumov,*)'CROP N  = Above-ground N (kg/ha)'
               WRITE(fnumov,*)
     &          'CROP N%  = Above-ground N concentration(%)'
               WRITE(fnumov,*)
     &         'H2O STRESS = Photosynthesis stress,average (0-1,0=none)'
               WRITE(fnumov,*)
     &          'N STRESS = Photosynthesis stress,average (0-1,0=none)'
              ENDIF
              WRITE(fnumov,*)' '
              WRITE (FNUMOV,206)
              WRITE (FNUMOV,290) MAX(-99,gdap),MAX(-99,gdapm),
     A         MAX(-99,edap),MAX(-99,edapm)
              DO L = 1,KEYSTX
                IF (KEYPS(L).GT.0) WRITE (FNUMOV,291)
     &           psname(KEYPS(L)),PSDap(KEYPS(L)),PSDapm(KEYPS(L))
              ENDDO
              WRITE (FNUMOV,305)
     H         NINT(cwam),NINT(cwamm),
     I         MAX(-99,NINT(rwam+sdwam)),NINT(rwamm),
     J         NINT(senwacm),NINT(senwacmm),
     L         NINT(hwam),NINT(hwamm),
     M         NINT(vwam),NINT(vwamm),
     N         hiam,hiamm,
     O         NINT(rswam),NINT(rswamm)
              IF (lwphc+swphc.GT.0.0) WRITE (FNUMOV,3051)
     &         NINT(cwahc),NINT(cwahcm),
     &         NINT(spnumhc*pltpop),MAX(-99,NINT(spnumhcm*pltpop))
              WRITE (FNUMOV,3052)
     A         hwumchar,hwummchar,
     B         NINT(hnumam),NINT(hnumamm),
     C         hnumgm,hnumgmm,
     D         NINT(tnumam),NINT(tnumamm),
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
              IF (cwaa.GT.0.0) WRITE (FNUMOV,3053)
     P         NINT(cwaa),NINT(cwaam),
     Q         cnaa,cnaam,
     R         NINT(lfwaa),NINT(lfwaam),
     S         NINT(stwaa),NINT(stwaam),
     T         NINT(rswaa),NINT(rswaam)
  290         FORMAT (                                                
     &           6X, 'Germination  (dap)          ',6X,I7,  4X,I7,  /,
     A           6X, 'Emergence    (dap)          ',6X,I7,  4X,I7    )
  291         FORMAT(                                                
     &           6X,A13, '(dap)          '         ,6X,I7  ,4X,I7    )
  305         FORMAT(                                                
     &           6X, 'AboveGround (kg dm/ha)      ',6X,I7,  4X,I7,  /,
     I           6X, 'Roots+seed residue (kg dm/ha)',5X,I7, 4X,I7,  /,
     J           6X, 'Senesced (kg dm/ha)         ',6X,I7,  4X,I7,  /,
     L           6X, 'Product (kg dm/ha)          ',6X,I7,  4X,I7,  /,
     K          6X, 'AboveGroundVegetative (kg dm/ha)  ',I7,4X,I7,  /,
     N           6X, 'HarvestIndex (ratio)        ',6X,F7.2,4X,F7.2,/,
     O           6X, 'Reserves (kg dm/ha)         ',6X,I7,  4X,I7)
 3051         FORMAT(                                               
     &           6X, 'Removed canopy (kg dm/ha)   ',7X,I6,  5X,I6,/,
     C           6X, 'Removed spikes (no/m2)      ',6X,I7  ,4X,I7  )  
 3052         FORMAT(                                                
     &           6X, 'Product unit wt (g dm)      ',7X,A6,  5X,A6,  /,
     A           6X, 'Product number (/m2)        ',6X,I7,  4X,I7,  /,
     B           6X, 'Product number (/shoot)     ',6X,F7.1,4X,F7.1,/,
     C           6X, 'Final shoot number (no/m2)  ',6X,I7  ,4X,I7  ,/,
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
 3053         FORMAT(
     O           6X, 'Straw wt,anthesis (kg dm/ha)',6X,I7,  4X,I7  ,/,
     P           6X, 'Straw N,anthesis (kg/ha)    ',6X,F7.1,4X,F7.1,/,
     Q           6X, 'Leaf wt,anthesis (kg dm/ha) ',6X,I7  ,4X,I7  ,/,
     R           6X, 'Stem wt,anthesis (kg dm/ha) ',6X,I7  ,4X,I7  ,/,
     S           6X, 'Res. wt,anthesis (kg dm/ha) ',6X,I7  ,4X,I7  ,/)
              WRITE(fnumov,500)
              PFPPAV = -99.0
              PFGPAV = -99.0
              DO tvI1 = 1,mstg
                IF (pdays(tvi1).GT.0) THEN  
                WRITE(fnumov,600) psname(tvi1),dash,psname(tvi1+1), 
     &          pdays(tvI1),tmaxpav(tvI1),tminpav(tvI1),sradpav(tvI1),
     &          daylpav(tvI1),rainpc(tvI1),etpc(tvI1),1.-wfppav(tvi1),
     &          1.0-wfgpav(tvi1), 1.0-nfppav(tvi1), 1.0-nfgpav(tvi1), 
     &          pfppav(tvi1), pfgpav(tvi1)
  600           FORMAT(1X,A10,A3,A10,I5,3F6.1,F7.2,2F7.1,4F7.3,2F7.2)
  610           FORMAT(1X,A10,13X,I5,3F6.1,F7.2,2I7,6F7.3)
                ENDIF
              ENDDO
              IF (pdays(mstg).GT.0) THEN 
              WRITE(fnumov,*) ' '
              pfpcav = -99.0
              pfgcav = -99.0 
              WRITE(fnumov,600) psname(1),dash,psname(mstg), 
     &         cdays, tmaxcav, tmincav, sradcav,
     &         daylcav, raincc, etcc, 1.0-wfpcav, 
     &         1.0-wfgcav, 1.0-nfpcav, 1.0-nfgcav,
     &         pfpcav, pfgcav
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
               GrP_Rain = GWAM  / RAINCC
              ENDIF
              IF (ETCC > 1.E-3) THEN
               DMP_ET = CWAM / ETCC 
               GrP_ET = GWAM  / ETCC 
              ENDIF
              IF (EPCC > 1.E-3) THEN
               DMP_EP = CWAM / EPCC 
               GrP_EP = GWAM  / EPCC 
              ENDIF
              IF (IRRAMTC > 1.E-3) THEN
                DMP_Irr = CWAM / IRRAMTC 
                GrP_Irr = GWAM  / IRRAMTC
              ENDIF
              IF (ISWNIT.NE.'N') THEN
                IF (Amtnit > 1.E-3) THEN
                  DMP_NApp = CWAM / Amtnit
                  GrP_NApp = GWAM  / Amtnit
                ENDIF
                IF (NUPAC > 1.E-3) THEN
                  DMP_NUpt = CWAM / NUPAC
                  GrP_NUpt = GWAM  / NUPAC
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
              IF (CROP.EQ.'WH') THEN 
                WRITE(FNUMOV,300) 'WHEAT', NINT(HWAM)
              ELSEIF (CROP.EQ.'BA') THEN 
                WRITE(FNUMOV,300) 'BARLEY', NINT(HWAM)
              ELSEIF (CROP.EQ.'CS') THEN 
                WRITE(FNUMOV,300) 'CASSAVA', NINT(HWAM)
              ENDIF  
              WRITE(FNUMOV,'(110("*"))')
              CLOSE(FNUMOV)  ! Overview.out
              ENDIF 
              ! Basic info.to Work.out when calling for Overview
              !  Summary of various environmental aspects
              WRITE(fnumwrk,*) ' '
              WRITE(fnumwrk,'(A28,A10,I3)')
     &          ' OVERVIEW OF CONDITIONS FOR ',
     &            excode,tn
              WRITE(fnumwrk,*) ' '
              WRITE (fnumwrk,209) tmaxx,tmaxm,tminn,tminm              
              IF (ISWNIT.NE.'N') THEN
                WRITE(fnumwrk,2095)cnad+rnad+hnad,hnad,vnad
                WRITE(fnumwrk,2096)sennal(0),sennas            
                WRITE(fnumwrk,2093)isoiln,amtnit,fsoiln
                WRITE(fnumwrk,2094)
     &            tnoxc,tlchc,tominsomc+tominfomc-tnimbsom
                WRITE(fnumwrk,2099)tnimbsom,tominfomc,tominsomc   
                IF (tominsom1.GT.0.0)
     &            WRITE(fnumwrk,2098)NINT(tominsom1c),NINT(tominsom2c),
     &             NINT(tominsom3c)
                IF (FILEIOT.EQ.'DS4'.AND.IDETL.EQ.'D'.OR.
     &             FILEIOT.EQ.'DS4'.AND.IDETL.EQ.'A'.OR.
     &             FILEIOT.NE.'DS4') THEN                  
                  WRITE(fnumwrk,2090)isoilh2o,rainc/10.0,irramtc/10.0
                  WRITE(fnumwrk,2091)runoffc/10.0,drainc/10.0,fsoilh2o
                  WRITE(fnumwrk,2089)eoc/10.0,eopenc/10.0,eompenc/10.0
                  WRITE(fnumwrk,2097)eoptc/10.0,eoebudc/10.0
                ENDIF
                IF (FAPPNUM.GT.0) THEN
                  WRITE (fnumwrk,*) ' '
                  WRITE (fnumwrk,'(A18,A10,I3)')
     &              ' N FERTILIZER FOR ',excode,tn
                  DO L = 1,FAPPNUM
                     WRITE (fnumwrk,'(A80)') FAPPLINE(L)
                  ENDDO
                ENDIF
                WRITE(FNUMWRK,*) ' '
                WRITE(FNUMWRK,'(A45)')
     &            ' INORGANIC N (kg/ha) LEFT IN SOIL AT MATURITY'
                WRITE(FNUMWRK,'(A28,2F6.1)')
     &           '  NO3 and NH4 N in PROFILE: ',
     &           SNO3PROFILE,SNH4PROFILE
                WRITE(FNUMWRK,'(A28,2F6.1)')
     &           '  NO3 and NH4 N in ROOTZONE:',
     &           SNO3ROOTZONE,SNH4ROOTZONE
              ENDIF   ! End Iswnit NE N
              WRITE(FNUMWRK,*) ' '
              WRITE(FNUMWRK,'(A34)')
     &          ' H2O (mm) LEFT IN SOIL AT MATURITY'
              WRITE(FNUMWRK,'(A36,2F6.1)')
     &         '  H2O and AVAILABLE H2O in PROFILE: ',
     &         H2OPROFILE,AH2OPROFILE
              WRITE(FNUMWRK,'(A36,2F6.1)')
     &         '  H2O and AVAILABLE H2O in ROOTZONE:',
     &         H2OROOTZONE,AH2OROOTZONE
              WRITE (fnumwrk,*) ' '
              WRITE (fnumwrk,'(A32,A10,I3)')
     &         ' CRITICAL PERIOD CONDITIONS FOR ',excode,tn
              WRITE (fnumwrk,'(A38,F6.1)')
     &         '  Temperature mean,germination         ',TMEANG
              WRITE (fnumwrk,'(A38,F6.1)')
     &         '  Temperature mean,germ-emergence      ',TMEANE
              WRITE (fnumwrk,'(A38,F6.1)')
     &         '  Temperature mean,first 20 days       ',TMEAN20P
              IF (HPROD.NE.'SR') THEN
                IF (TMEAN20A.GT.0.0) WRITE (fnumwrk,'(A38,F6.1)')
     &           '  Temperature mean,20d around anthesis ',TMEAN20A
                IF (CUMDU.GE.MSTG-1) WRITE (fnumwrk,'(A38,F6.1)')
     &           '  Temperature mean,grain filling       ',
     &           TMEANAV(MSTG-1)
                IF (SRAD20A.GT.0.0) THEN
                  WRITE (fnumwrk,*)' '
                  WRITE (fnumwrk,'(A38,F6.1)')
     &             '  Solar radn. mean,20d around anthesis ',SRAD20A
                ENDIF
                WRITE (fnumwrk,'(A38,F6.1)')
     &           '  Stress fac. mean,20d before grain set',STRESS20GS
              END IF
              
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
   99         FORMAT ('@  RUN EXCODE      TRNO RN',
     A        ' REP  RUNI S O C    CR PYEAR  PDOY')
              DO L = 1,KEYSTX
                IF (KEYPS(L).GT.0) THEN
                  WRITE (FNUMPSUM,'(A6)',ADVANCE='NO') PSABVO(KEYPS(L))
                  IF (PSABVO(KEYPS(L)).EQ.'TSDAP') THEN
                   WRITE (FNUMPSUM,'(A6)',ADVANCE='NO') '  DAYL'
                  ENDIF
                ENDIF
              ENDDO
              WRITE (FNUMPSUM,299)
  299         FORMAT (
     B        '   FLN FLDAP HYEAR  HDAY SDWAP',
     C        ' CWAHC  CWAM  HWAM  HWAH  VWAM  HWUM  H#AM  H#UM',
     D        ' SDNAP  CNAM  HNAM  RNAM  TNAM  NUCM  HN%M  VN%M',
     E        ' D1INI D2INI D3INI ')
              CLOSE(fnumpsum)  
            ENDIF  ! End of Plantsum.Out headers
            OPEN (UNIT=fnumpsum,FILE=FNAMEPSUM,POSITION='APPEND')
            WRITE (fnumpsum,400,ADVANCE='NO') run,excode,tn,rn,
     A        rep,runi,sn,on,cn,crop,
     B        plyear,plday
  400       FORMAT (I6,1X,A10,I6,I3,
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
     D        NINT(cwam),NINT(hwam),
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
     D       I6,I6,
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
!         IDETL = Y or D OUTPUTS (Leaves,Phases)
!-----------------------------------------------------------------------

          IF (IDETL.EQ.'Y'.OR.IDETL.EQ.'D'.OR.IDETL.EQ.'A') THEN
          
            ! LEAVES.OUT
            OPEN(UNIT=FNUMLVS,FILE=FNAMELEAVES,POSITION='APPEND')
            WRITE (FNUMLVS,'(/,A79,/)') OUTHED
            WRITE (FNUMLVS,'(A14,F6.1)') '! LEAF NUMBER ',LNUM
            IF (LAFSWITCH.GT.0.0) THEN
              WRITE(FNUMLVS,'(A42,F6.2)')
     &         '! LEAF NUMBER WHEN INCREASE FACTOR CHANGED',lafswitch
              !Taken out whilst use simpler calculations at change-over 
              !LAPOTX(INT(LAFSWITCH+1)) = LAPOTXCHANGE
              WRITE(FNUMLVS,'(A35,F6.2)')
     &         '! AREA OF LEAF WHEN FACTOR CHANGED  ',lapotxchange
            ENDIF     
            WRITE (FNUMLVS,'(/,A42,A30)')
     &       '@ LNUM AREAP AREA1 AREAT AREAS  T#PL  T#AL',
     &       '  WFLF  NFLF  AFLF  TFLF DAYSG'
            DO I = 1, INT(LNUM+0.99)
              CALL Csopline(lapotxc,lapotx(i))
              CALL Csopline(latlc,latl(1,i))
              CALL Csopline(lapc,lap(i))
              CALL Csopline(lapsc,laps(i))
              ! Adjust for growth period
              WFLF(I) = WFLF(I)/AMIN1(1.0,(LAGEP(I)/LLIFG))
              WFLFP(I) = WFLFP(I)/AMIN1(1.0,(LAGEP(I)/LLIFG))
              NFLF(I) = NFLF(I)/AMIN1(1.0,(LAGEP(I)/LLIFG))
              NFLFP(I) = NFLFP(I)/AMIN1(1.0,(LAGEP(I)/LLIFG))
              TFLF(I) = TFLF(I)/AMIN1(1.0,(LAGEP(I)/LLIFG))
              AFLF(I) = AFLF(I)/AMIN1(1.0,(LAGEP(I)/LLIFG))
              WRITE (fnumlvs,'(I6,4A6,F6.1,I6,4F6.1,F6.1)')
     &          I,LAPOTXC,
     &          LATLC,LAPC,LAPSC,
     &          TNUML(I),NINT(TNUML(I)*PLTPOP),1.0-WFLF(I),
     &          1.0-NFLF(I),1.0-AFLF(I),1.0-TFLF(I),DGLF(I)
            ENDDO
            IF (RUN.EQ.1) THEN
              WRITE(fnumlvs,*)' '
              IF (CROP.EQ.'CS') THEN
                WRITE(fnumlvs,'(A51)')
     &           '! NB. Cassava data are summed over all fork branches'
                WRITE(fnumlvs,*)' '
              ENDIF
              WRITE(fnumlvs,'(A36)')
     &          '! LNUM = Number of leaf on one axis '
              WRITE(fnumlvs,'(A52)')
     &          '! AREAP = Potential area of leaf on main axis (cm2) '
              WRITE(fnumlvs,'(A41,A16)')
     &          '! AREA1 = Area of youngest mature leaf on', 
     &          ' main axis (cm2)'
              WRITE(fnumlvs,'(A42,A16)')
     &          '! AREAT = Area of cohort of leaves at leaf',
     &          ' position (cm2) '
              WRITE(fnumlvs,'(A43,A35)')
     &          '! AREAS = Senesced area of cohort of leaves',
     &          ' at maturity at leaf position (cm2)'
              WRITE(fnumlvs,'(A45)')
     &          '! T#PL = Tiller number/plant at leaf position'
              WRITE(fnumlvs,'(A48)')
     &          '! T#AL = Tiller number/area(m2) at leaf position'
              WRITE(fnumlvs,'(A38,A17)')
     &          '! WFLF  = Water stress factor for leaf',
     &          ' (0-1,1=0 stress)'
              WRITE(fnumlvs,'(A48,A17)')
     &          '! WFLFP = Water stress factor for photosynthesis',
     &          ' (0-1,1=0 stress)'
              WRITE(fnumlvs,'(A51)')
     &          '! NFLF  = N stress factor for leaf (0-1,1=0 stress)'
              WRITE(fnumlvs,'(A44,A17)')
     &          '! NFLFP = N stress factor for photosynthesis',
     &          ' (0-1,1=0 stress)'
              WRITE(fnumlvs,'(A36,A24)')
     &          '! AFLF  = Assimilate factor for leaf',
     &          ' (0-1,1=0 no limitation)'
              WRITE(fnumlvs,'(A37,A24)')
     &          '! TFLF  = Temperature factor for leaf',
     &          ' (0-1,1=0 no limitation)'
              WRITE(fnumlvs,'(A37)')
     &          '! DAYSG = Number of days of growth   '
            ENDIF
            CLOSE (FNUMLVS)
            ! End of Leaves.out
          
            ! Phase conditions (Simulated;PHASES.OUT)
            OPEN(UNIT=FNUMPHA,FILE=FNAMEPHASES,POSITION='APPEND')
            WRITE (FNUMPHA,'(/,A79,/)') OUTHED
            WRITE (fnumpha,'(A42,A24,A12)')
     &       '@PHASE  SRAA  TMXA  TMNA  PREA  TWLA  CO2A',
     &       '  WFPA  WFGA  NFPA  NFGA',
     &       ' PHASE_END  '
            DO L=1,PSNUM
              IF (STGYEARDOY(L).LT.9999999.AND.
     &         L.NE.0.AND.L.NE.10.AND.L.NE.11)
     &         WRITE (fnumpha,'(I6,3F6.1,2F6.2,I6,4F6.2,1X,A13)')
     &         L,sradpav(L),tmaxpav(L),tminpav(L),
     &         rainpav(L),daylpav(L),NINT(co2pav(L)),
     &         1.0-wfppav(L),1.0-wfgpav(L),
     &         1.0-nfppav(L),1.0-nfgpav(L),
     &         psname(MIN(L+1,PSX))
            ENDDO
            CLOSE (FNUMPHA)
              
          ELSE
          
            OPEN (UNIT=FNUMLVS,FILE=FNAMELEAVES,STATUS='UNKNOWN')
            CLOSE (UNIT=FNUMLVS, STATUS = 'DELETE')
            OPEN (UNIT=FNUMPHA,FILE=FNAMEPHASES,STATUS = 'UNKNOWN')
            CLOSE (UNIT=FNUMPHA, STATUS = 'DELETE')
            
          ENDIF
          ! End of Leaves and Phases writes
          
          ! If have not read measured data cannot produce A summaries
          IF (IDETL.EQ.'D'.AND.IDETO.EQ.'N') THEN
            WRITE(Message(1),'(A35)')
     &       'IDETL flag called for detail files.'
            WRITE(Message(2),'(A31,A31)')
     &       'But IDETO flag set at N so that',
     &       'measured data not read.        '
            WRITE(Message(3),'(A45)')
     &       'Therefore,could not write detailed summaries.'
            CALL WARNING(3,'CSCRP',MESSAGE)
          ENDIF
          
!-----------------------------------------------------------------------
!         IDETL = D OUTPUTS (Work details;Phenols,m;Plantres,m)
!-----------------------------------------------------------------------
           
          IF ((IDETL.EQ.'D'.AND.IDETO.NE.'N').OR.IDETL.EQ.'A') THEN
                  
            ! WORK
            WRITE(fnumwrk,*) ' '
            WRITE(fnumwrk,'(A26,A10,I3)')' HARVEST/FAILURE DATA FOR ',
     &       excode,tn
            WRITE(fnumwrk,*)' '
            IF (DYNAMIC.EQ.SEASEND .AND. SEASENDOUT.NE.'Y') THEN
              WRITE(fnumwrk,*)  ' Program terminated      ',YEARDOY
            ELSE 
              WRITE(fnumwrk,*)  ' Harvest reached         ',YEARDOY
            ENDIF  
            WRITE(fnumwrk,*) ' '
            WRITE(fnumwrk,'(A27,F11.2)')'  Harvest product (kg/ha)  ',
     &       HWAM
            WRITE(fnumwrk,'(A27,F11.2)')'  Product/Total wt (HI)    ',
     &       HIAM
            IF (GNOWS.GT.0.0.AND.CWAM.GT.0.0) THEN
              WRITE(fnumwrk,'(A28,F10.2)')
     &         '  Grain #/(Total wt)        ',HNUMAM/(CWAM*.1)
              WRITE(fnumwrk,'(A28,F10.2)')
     &         '  Grain #/(Total product)   ',HNUMAM/((CWAM-HWAM)*.1)
              WRITE(fnumwrk,*) ' '
              WRITE(fnumwrk,'(A28,F10.2,A1)')
     &        '  (Grain #/Total standard   ',GNOWS,')'
            ENDIF
            IF (GNOWS.GT.0.0) THEN
             WRITE (fnumwrk,*)' '
             IF (gnopm.GT.0.0) WRITE (fnumwrk,'(A22,F7.1)')
     &        '  Grain weight mg     ',GRWT/GNOPM*1000.0
             WRITE (fnumwrk,'(A22,F7.1)')
     &        '  Grain weight coeff  ',gwta
             IF (carbolim.GT.0.OR.NLIMIT.GT.0.OR.TLIMIT.GT.0) THEN
               WRITE (fnumwrk,'(A38)')
     &          '  Grain growth limited by some factor!'
               WRITE(fnumwrk,'(A24,I5)')
     &          '   Days of Ch2o limit   ',carbolim
               WRITE(fnumwrk,'(A24,I5)')
     &          '   Days of N limit      ',nlimit
               WRITE(fnumwrk,'(A24,I5)')
     &          '   Days of temp limit   ',tlimit
             ENDIF
             WRITE(fnumwrk,'(A24,I5)')
     &        '  Days of linear fill   ',pdays(MSTG-1)
             IF (grwt.GT.0.0) WRITE (fnumwrk,'(A24,F7.1)')
     &        '  Grain nitrogen %       ',grainn/grwt*100.0
             WRITE (fnumwrk,'(A24,F7.1)')
     &        '  Minimum nitrogen %     ',grnmn
             WRITE (fnumwrk,'(A24,F7.1)')
     &        '  Standard nitrogen %   ',grns
            ENDIF
            WRITE(fnumwrk,*) ' '
            WRITE(fnumwrk,'(A28,A10,I3)')
     &       ' CH2O BALANCE (g/plant) FOR ',excode,tn
            WRITE(fnumwrk,'(A27,3F11.4)')
     &       '  SEED+FIXED (A) Seed,fixed',
     &       SEEDRSI+SDCOAT+CARBOC,SEEDRSI+SDCOAT,CARBOC
            WRITE(fnumwrk,'(A27,3F11.4)')
     &       '  RESPIRED (B)  Tops,root  ',RESPC,RESPTC,RESPRC
            WRITE(fnumwrk,'(A27,3F11.4)')
     &       '  SENESCED (C)  Tops,root  ',
     &       SENWL(0)+SENWS,SENWL(0),SENWS
            WRITE(fnumwrk,'(A27,3F11.4)')
     &       '  LIVE+DEAD (D) Live,dead  ',
     &       (SEEDRS+SDCOAT+RTWT+SRWT+LFWT+STWT+GRWT+RSWT+DEADWTR),
     &       (SEEDRS+SDCOAT+RTWT+SRWT+LFWT+STWT+GRWT+RSWT),DEADWTR
            WRITE(fnumwrk,'(A27,3F11.4)')
     &       '  PLANT+SEED_RESIDUE Pl,sd ',
     &       (SEEDRS+SDCOAT+RTWT+SRWT+LFWT+STWT+GRWT+RSWT),
     &       (RTWT+SRWT+LFWT+STWT+GRWT+RSWT),
     &       (SEEDRS+SDCOAT)
            WRITE(fnumwrk,'(A27,2F11.4)')
     &       '  RESERVES (E)  Post-mat   ',RSWT,RSWTPM
            WRITE(fnumwrk,'(A29, F9.4)')
     &       '  HARVESTED DURING CYCLE (F) ',
     &       LWPHC+SWPHC+RSWPHC+GWPHC+DWRPHC
            WRITE(fnumwrk,'(A27, F11.4)')
     &       '  BALANCE (A-(B+C+D+F))    ',
     &         SEEDRSI+SDCOAT+CARBOC
     &        -RESPC
     &        -(SENWL(0)+SENWS)
     &        -(SEEDRS+SDCOAT+RTWT+SRWT+LFWT+STWT+GRWT+RSWT+DEADWTR)
     &        - (LWPHC+SWPHC+RSWPHC+GWPHC+DWRPHC) 
            IF ((SEEDRS+SDCOAT+RTWT+SRWT+LFWT+STWT+GRWT+RSWT+DEADWTR)
     &         .GT.0.0
     &       .AND. ABS(SEEDRSI+SDCOAT+CARBOC-RESPC-(SENWL(0)+SENWS)
     &        - (SEEDRS+SDCOAT+RTWT+SRWT+LFWT+STWT+GRWT+RSWT+DEADWTR)
     &        - (LWPHC+SWPHC+RSWPHC+GWPHC+DWRPHC))/
     &           (SEEDRS+SDCOAT+RTWT+SRWT+LFWT+STWT+GRWT+RSWT+DEADWTR)
     &       .GT. 0.01)
     &       WRITE(fnumwrk,'(A29,A10,A1,I2)')
     &       '  PROBLEM WITH CH2O BALANCE  ',EXCODE,' ',TN
            IF (GNOWS.GT.0.0.AND.lfwtsge.GT.0.0) THEN
              WRITE (fnumwrk,*) ' '
              WRITE (fnumwrk,'(A42,A10,I3)')
     &         ' CH2O BALANCE FROM END OF STEM GROWTH FOR ',excode,tn
              WRITE (fnumwrk,'(A53)')
     &         '  NB.Balance assumes that no dead matter is shed     '
              WRITE (fnumwrk,'(A22,F7.1)')'  Above ground at SGE ',
     &         (lfwtsge+stwtsge+rswtsge+grwtsge+deadwtsge)*pltpop*10.0
              WRITE (fnumwrk,'(A22,F7.1)')'  Leaves at SGE       ',
     &         (lfwtsge)*pltpop*10.0
              WRITE (fnumwrk,'(A22,F7.1)')'  Stems at SGE        ',
     &         (stwtsge)*pltpop*10.0
              WRITE (fnumwrk,'(A22,F7.1)')'  Reserves at SGE     ',
     &         (rswtsge)*pltpop*10.0
              WRITE (fnumwrk,'(A22,F7.1)')'  Grain at SGE        ',
     &         (grwtsge)*pltpop*10.0
              WRITE (fnumwrk,'(A22,F7.1)')'  Dead at SGE         ',
     &         (deadwtsge)*pltpop*10.0
              WRITE (fnumwrk,'(A22,F7.1)')'  Roots at SGE        ',
     &         (rtwtsge)*pltpop*10.0
              WRITE (fnumwrk,'(A22,F7.1)')'  Above ground at end ',
     &         (lfwt+stwt+rswt+grwt+deadwtr)*pltpop*10.0
              WRITE (fnumwrk,'(A22,F7.1)')'  Leaves at end       ',
     &         (lfwt)*pltpop*10.0
              WRITE (fnumwrk,'(A22,F7.1)')'  Stems at end        ',
     &         (stwt)*pltpop*10.0
              WRITE (fnumwrk,'(A22,F7.1)')'  Reserves at end     ',
     &         (rswt)*pltpop*10.0
              WRITE (fnumwrk,'(A22,F7.1)')'  Grain at end        ',
     &         (grwt)*pltpop*10.0
              WRITE (fnumwrk,'(A22,F7.1)')'  Dead at end         ',
     &         (deadwtr)*pltpop*10.0
              WRITE (fnumwrk,'(A22,F7.1)')'  Roots at end        ',
     &         (rtwt)*pltpop*10.0
              WRITE (fnumwrk,'(A22,F7.1)')'  Assimilation > SGE  ',
     &         carbogf*pltpop*10.0
              WRITE (fnumwrk,'(A22,F7.1)')'  Respiration > SGE   ',
     &         (respgf)*pltpop*10.0
              WRITE (fnumwrk,'(A22,F7.1)')'  Senesced > SGE      ',
     &         sengf*pltpop*10.0
              WRITE (fnumwrk,'(A22,F7.1)')'  Leaves > SGE        ',
     &         (lfwt-lfwtsge)*pltpop*10.0
              WRITE (fnumwrk,'(A22,F7.1)')'  Stems > SGE         ',
     &         (stwt-stwtsge)*pltpop*10.0
              WRITE (fnumwrk,'(A22,F7.1)')'  Reserves > SGE      ',
     &         (rswt-rswtsge)*pltpop*10.0
              WRITE (fnumwrk,'(A22,F7.1)')'  Grain > SGE         ',
     &         (grwt-grwtsge)*pltpop*10.0
              WRITE (fnumwrk,'(A22,F7.1)')'  Dead > SGE          ',
     &         (deadwtr-deadwtsge)*pltpop*10.0
              WRITE (fnumwrk,'(A22,F7.1)')'  Roots > SGE         ',
     &         (rtwt-rtwtsge)*pltpop*10.0
              WRITE (fnumwrk,'(A22,F7.1)')'  Total > SGE         ',
     &         ((lfwt+stwt+rswt+grwt+deadwtr+rtwt)*pltpop*10.0) -
     &         ((lfwtsge+stwtsge+rswtsge+grwtsge+deadwtsge+rtwtsge)
     &         *pltpop*10.0)
              WRITE (fnumwrk,'(A22,F7.1)')'  Assim-R-S-Diff > SGE',
     &        ((carbogf-respgf)*pltpop*10.0) - sengf*pltpop*10.0 -
     &        (((lfwt+stwt+rswt+grwt+deadwtr+rtwt)*pltpop*10.0) -
     &        ((lfwtsge+stwtsge+rswtsge+grwtsge+deadwtsge+rtwtsge)
     &        *pltpop*10.0))
            ENDIF
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A21,A10,I3)')
     &       ' RESERVES STATUS FOR ',excode,tn
            IF (HPROD.NE.'SR') THEN
              WRITE (fnumwrk,'(A22,F7.2)')'  Reserves coeff      ',
     &         RSFRS
              WRITE (fnumwrk,'(A22,F7.1)')'  Kg/ha at anthesis   ',
     &         RSWTA*PLTPOP*10.0
              IF (rswta+stwta+lfwta.GT.0) WRITE (fnumwrk,'(A22,F7.1)')
     &        '  % above ground      ',rswta/(rswta+stwta+lfwta)*100.0
              IF (stwta.GT.0) WRITE (fnumwrk,'(A22,F7.1)')
     &        '  % stem+reserves     ',rswta/(rswta+stwta)*100.0
              WRITE (fnumwrk,'(A22,F7.1)')'  Kg/ha at grain set  ',
     &        RSWTAE*PLTPOP*10.0
              IF (rswtae+stwtae+lfwtae.GT.0) 
     &         WRITE(fnumwrk,'(A22,F7.1)') '  % above ground      ',
     &         rswtae/(rswtae+stwtae+lfwtae)*100.
              WRITE (fnumwrk,'(A22,F7.1)')'  Kg/ha at end stem   ',
     &        RSWTSGE*PLTPOP*10.0
              IF ((rswtsge+stwtsge+lfwtsge).GT.0)
     &        WRITE(fnumwrk,'(A22,F7.1)') '  % above ground      ',
     &        rswtsge/(rswtsge+stwtsge+lfwtsge)*100.
              IF (stwtsge.GT.0) WRITE (fnumwrk,'(A22,F7.1)')
     &        '  Pc stem+reserves    ',rswtsge/(rswtsge+stwtsge)*100.0
            ENDIF
            WRITE (fnumwrk,'(A22,F7.1)')
     &       '  Kg/ha at maximum    ',RSWTX*PLTPOP*10.0
            WRITE (fnumwrk,'(A22,F7.1)')
     &       '  % above ground      ',RSCX*100.
            WRITE (fnumwrk,'(A22,F7.1)')
     &       '  Kg/ha at maturity   ',RSWAD
            IF (lfwt+stwt+rswt.GT.0) WRITE (fnumwrk,'(A22,F7.1)')
     &       '  % above ground      ',rswt/(lfwt+stwt+rswt)*100.0
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A34,A10,I3)')
     &       ' SEED USE (KG/HA or PER CENT) FOR ',excode,tn
            WRITE (fnumwrk,'(A22,F7.3)')'  Initial reserves    ',
     &       seedrsi*pltpop*10.0
            WRITE (fnumwrk,'(A22,F7.3)')'  Use for tops        ',
     &       seeduset*pltpop*10.0
            WRITE (fnumwrk,'(A22,F7.3)')'  Use for roots       ',
     &       seeduser*pltpop*10.0
            WRITE (fnumwrk,'(A22,F7.3)')'  Total use           ',
     &       (seeduset+seeduser)*pltpop*10.0
            IF (seeduser+seeduset.GT.0.0)
     &       WRITE (fnumwrk,'(A22,F7.3)')'  Percent to tops     ',
     &        seeduset/(seeduset+seeduser)*100.0
            WRITE(fnumwrk,*)' '
            WRITE (fnumwrk,'(A35,A10,I3)')
     &       ' DEAD MATTER AND ROOTS (KG/HA) FOR ',excode,tn
            WRITE(fnumwrk,'(A32,F8.1)')
     &       '  DEAD MATERIAL LEFT ON SURFACE  ',SENWAL(0)
            WRITE(fnumwrk,'(A32,F8.1)')
     &       '  DEAD MATERIAL LEFT IN SOIL     ',SENWAS
            WRITE(fnumwrk,'(A32,F8.1)')
     &       '  ROOT WEIGHT AT HARVEST/FAILURE ',RWAD
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A20,A10,I3)')
     &       ' ROOTS BY LAYER FOR ',excode,tn
            WRITE (fnumwrk,'(A19)')
     &       '  LAYER  RTWT   RLV'
            DO L=1,NLAYR
              IF (RTWTAL(L).GT.0.0) WRITE (fnumwrk,'(I6,F7.1,F6.2)')
     &        L,RTWTAL(L),RLV(L)
            ENDDO
            IF (RTSLXDATE.GT.0) THEN
              WRITE(fnumwrk,'(A30,I7)')
     &         '  FINAL SOIL LAYER REACHED ON ',RTSLXDATE
              WRITE(fnumwrk,'(A15,I7,A1)')
     &         '  (MATURITY ON ',YEARDOY,')'
            ELSE  
             WRITE(fnumwrk,*)' FINAL SOIL LAYER NOT REACHED '
            ENDIF
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A40)')
     &       ' PRINCIPAL AND SECONDARY STAGES         '
            WRITE (fnumwrk,'(A40)')
     &       '  STAGE NAME   DAYS > PLANTING          '
            WRITE (fnumwrk,'(A15,F7.1)')
     &       '   Germination ',gdapfr
            WRITE (fnumwrk,'(A15,F7.1)')
     &       '   Emergence   ',edapfr
            IF (CROP.EQ.'WH'.OR.CROP.EQ.'BA') THEN
              WRITE (fnumwrk,'(A15,F7.1)')
     &        '   Tillering   ',tildap
            ENDIF 
            DO L = 2,PSNUM
              CALL CSUCASE (PSNAME(L))
              IF (PSNAME(L)(1:3).EQ.'HAR'.AND.PSDAPFR(l).LE.0.0)
     &         psdapfr(l) = psdapfr(mstg)
              IF (PSNAME(L)(1:3).EQ.'END'.AND.PSDAPFR(l).LE.0.0)
     &         psdapfr(l) = psdapfr(mstg)
              IF (PSNAME(L)(1:3).NE.'FAI') THEN
                IF (psdapfr(l).GT.0) WRITE (FNUMWRK,'(A3,A13,F6.1)')
     &            '   ',psname(l),psdapfr(l)
              ELSE
                IF (CFLFAIL.EQ.'Y'.AND.psdapfr(l).GT.0)
     &           WRITE (FNUMWRK,'(A3,A13,F6.1)')
     &             '   ',psname(l),psdapfr(l)
              ENDIF
              IF (TVILENT(PSNAME(L)).LT.5) EXIT
            ENDDO
            IF (SSNUM.GT.0) WRITE (FNUMWRK,*) ' '
            DO L = 1,SSNUM
             IF (ssdapfr(l).GT.0.0)
     &        WRITE(FNUMWRK,'(A3,A13,F6.1)')'   ',ssname(l),ssdapfr(l)
              IF (TVILENT(SSNAME(L)).LT.5) EXIT
            ENDDO
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A28,A10,I3)')
     &       ' STRESS FACTOR AVERAGES FOR ',excode,tn
            WRITE (fnumwrk,'(A55)')
     &       '  PHASE  H2O(PS)   H2O(GR)   N(PS)     N(GR)  PHASE_END'
            DO L=1,PSNUM
              IF (STGYEARDOY(L).LT.9999999.AND.
     &         L.NE.1.AND.L.NE.10.AND.L.NE.11)
     &         WRITE (fnumwrk,'(I6,F8.2,3F10.2,2X,A13)')
     &         l,1.0-wfppav(l),1.0-wfgpav(l),
     &         1.0-nfppav(l),1.0-nfgpav(l),psname(MIN(L+1,PSX))
            ENDDO
            WRITE (fnumwrk,'(A42)')
     &       '  NB 0.0 = minimum ; 1.0 = maximum stress.'
            ! LAH  Must change from daily leaf cohorts to bigger unit
            ! Too much to output if daily
            !WRITE (fnumwrk,*)' '
            !WRITE (fnumwrk,'(A23)') ' COHORT   AREA    AREAS'
            !DO L = 1, LCNUM
            !  WRITE (fnumwrk,'(I7,2F8.3)') L,LCOA(L),LCOAS(L)
            !ENDDO
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A18,A10,I3)')
     &       ' TILLER SIZES FOR ',excode,tn
            WRITE (fnumwrk,'(A25,F6.1)')
     &       '   MAXIMUM TILLER NUMBER ',tnumx
            WRITE (fnumwrk,'(A31)') '   TILL   BIRTH   AREAP   AREAS'
            DO I = 1,INT(TNUMX)
              WRITE (fnumwrk,'(I7,3I8)')
     &          I,NINT(TILBIRTHL(I)),NINT(TLA(I)),NINT(TLAS(I))
            ENDDO
            IF (INT(TNUMX-FLOAT(INT(TNUMX))).GT.0) THEN
              I = INT(TNUMX) + 1
              WRITE (fnumwrk,'(I7,3I8)')
     &          I,NINT(TILBIRTHL(I)),NINT(TLA(I)),NINT(TLAS(I))
            ENDIF
            IF (ISWNIT.NE.'N') THEN
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A25,A10,I3)')' N BALANCE (g/plant) FOR ',
     &       excode,tn
            WRITE (fnumwrk,'(A34,F8.4)')
     &       '   N UPTAKE + SEED (A)            ', NUPC+SEEDN
            WRITE (fnumwrk,'(A33,F9.4,2F11.4)')
     &       '   TOTAL N SENESCED (B) Tops,Root',
     &       SENNL(0)+SENNS,SENNL(0),SENNS
            WRITE (fnumwrk,'(A34,F8.4)')
     &       '   N IN DEAD MATTER               ', DEADN
            WRITE (fnumwrk,'(A34,F8.4)')
     &       '   TOTAL N IN PLANT (C)           ',
     &       (ROOTN+SROOTN+LEAFN+STEMN+RSN+GRAINN+SEEDN+DEADN)
            WRITE (fnumwrk,'(A33, F9.4)')
     &       '   HARVESTED DURING CYCLE (D)    ',
     &       LNPHC+SNPHC+RSNPHC+GNPHC             
            WRITE (fnumwrk,'(A34,F8.4)')
     &       '   BALANCE (A-(B+C+D))            ',
     &       NUPC+SEEDNI
     &       -(SENNL(0)+SENNS)
     &       -(ROOTN+SROOTN+LEAFN+STEMN+RSN+GRAINN+SEEDN+DEADN)
     &       - (LNPHC+SNPHC+RSNPHC+GNPHC) 
            IF ((ROOTN+SROOTN+LEAFN+STEMN+RSN+GRAINN+SEEDN+DEADN)
     &       .GT.0.0 .AND. ABS(NUPC+SEEDNI-(SENNL(0)+SENNS)
     &       -(ROOTN+SROOTN+LEAFN+STEMN+RSN+GRAINN+SEEDN+DEADN)
     &       - (LNPHC+SNPHC+RSNPHC+GNPHC))/
     &       (ROOTN+SROOTN+LEAFN+STEMN+RSN+GRAINN+SEEDN+DEADN).GT..01)
     &       WRITE(fnumwrk,'(A26,A10,A1,I2)')
     &       '   PROBLEM WITH N BALANCE ',EXCODE,' ',TN
            ENDIF
            ! End of Detailed WORK writes
    
            ! Phenology (Simulated;PHENOLS.OUT)
            OPEN(UNIT=FNUMPHES,FILE=FNAMEPHENOLS,POSITION='APPEND')
            IF (CROP.NE.CROPPREV.OR.RUN.EQ.1) THEN
              WRITE (FNUMPHES,'(/,A14,A10)')
     &         '*PHENOLOGY(S):',EXCODE
              WRITE (FNUMPHES,'(A16,A24)',ADVANCE='NO') 
     &         '@ EXCODE    TRNO',' PYEAR  PDOY  GDAP  EDAP'
              DO L = 1,KEYSTX
                IF (KEYPS(L).GT.0)THEN
                  WRITE (FNUMPHES,'(A6)',ADVANCE='NO') PSABVO(KEYPS(L))
                ENDIF
              ENDDO
              DO L = 1,KEYSTX
                IF (KEYSS(L).GT.0) WRITE (FNUMPHES,'(A6)',ADVANCE='NO')
     &           SSABVO(KEYSS(L))
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
            DO L = 1,KEYSTX
             IF (KEYSS(L).GT.0) WRITE (FNUMPHES,'(I6)',ADVANCE='NO')
     &         SSDAP(KEYSS(L))
            ENDDO
            CLOSE (FNUMPHES)
            ! End Phenology simulated writes              
            
            ! Phenology (Measured;PHENOLM.OUT)
            IF (TDATANUM.LE.0 .AND. .NOT.FEXISTA) THEN
              WRITE (fnumwrk,*)' '
              WRITE (fnumwrk,*)
     &          ' No data so cannot write PHENOLOGY (MEASURED)'
              OPEN (UNIT=FNUMPHEM,FILE=FNAMEPHENOLM,STATUS ='UNKNOWN')
              CLOSE (UNIT=FNUMPHEM, STATUS = 'DELETE')
            ELSE
              OPEN(UNIT=FNUMPHEM,FILE=FNAMEPHENOLM,POSITION='APPEND')
              IF (CROP.NE.CROPPREV.OR.RUN.EQ.1) THEN
                WRITE (FNUMPHEM,'(/,A14,A10)')
     &            '*PHENOLOGY(M):',EXCODE
                WRITE (FNUMPHEM,'(A16,A24)',ADVANCE='NO') 
     &            '@EXCODE     TRNO',' PYEAR  PDOY  GDAP  EDAP'
                DO L = 1,KEYSTX
                  IF (KEYPS(L).GT.0) THEN
                    WRITE (FNUMPHEM,'(A6)',ADVANCE='NO')PSABVO(KEYPS(L))
                  ENDIF 
                ENDDO
                DO L = 1,KEYSTX
                  IF (KEYSS(L).GT.0) WRITE(FNUMPHEM,'(A6)',ADVANCE='NO')
     &             SSABVO(KEYSS(L))
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
              DO L = 1,KEYSTX
                IF (KEYSS(L).GT.0) WRITE (FNUMPHEM,'(I6)',ADVANCE='NO')
     &           SSDAPM(KEYSS(L))
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
            IF (EXCODE.NE.EXCODEPREV) AMTNITPREV = 0.0
            OPEN (UNIT = FNUMPRES,FILE = FNAMEPRES)
            DO WHILE (.true.)
              READ(FNUMPRES,'(A180)',end=9950)TLINETMP
            ENDDO
 9950       CONTINUE    
            CLOSE(FNUMPRES)
            READ(TLINETMP,'(7X,A10)') EXCODEPREV
            READ(TLINETMP,'(74X,I6)',IOSTAT=ERRNUM) AMTNITPREV
            IF (ERRNUM /= 0) AMTNITPREV = 0.0
            ! LAH Here should write new header if new response group
            IF (CFLPRES.NE.'Y'.OR.TNAME(1:1).EQ.'*') THEN
              CFLPRES = 'Y'
              OPEN(UNIT=FNUMPRES,FILE=FNAMEPRES,POSITION='APPEND')
              WRITE (FNUMPRES,*) ' '
              IF (TNAME(1:1).EQ.'*') THEN
                WRITE (FNUMPRES,9951) EXCODE,TNAME(2:25)
 9951           FORMAT ('*RESPONSES(S):',A10,'  ',A24)
              ELSEIF (AMTNIT.LT.AMTNITPREV.OR.
     &                PLYEARDOY.LT.PLYEARDOYPREV)THEN
                WRITE (FNUMPRES,9951) EXCODE,TNAME(1:24)
              ELSE
                WRITE (FNUMPRES,995) EXCODE,ENAME(1:55)
  995           FORMAT ('*RESPONSES(S):',A10,'  ',A55)
              ENDIF
              WRITE (FNUMPRES,97,ADVANCE='NO')
   97         FORMAT ('@  RUN',
     A        ' EXCODE     ',
     B        ' TRNO RN',
     D        '  PDOY  EDAP')
              DO L = 1,KEYSTX
                IF (KEYPS(L).GT.0) THEN
                  WRITE (FNUMPRES,'(A6)',ADVANCE='NO') PSABVO(KEYPS(L))
                ENDIF  
              ENDDO
              WRITE (FNUMPRES,297)
  297         FORMAT (
     F        '  NICM',
     G        '  HWAM  HWUM',
     H        '  H#AM  H#PM  H#GM  LAIX  L#SM',
     I        '  CWAM  VWAM  RS%M  HIAM  RWAM SHRTM',
     J        '  HN%M  TNAM',
     K        '  CNAM  HNAM',
     L        '  HINM PLPOP  T#AM',
     M        '  SRAA  TMXA  TMNA  TAVA  PREC  TWLD',
     N        ' PARIU PARUE GFDUR WSDAY NSDAY')
            ELSE
              OPEN (UNIT=FNUMPRES,FILE=FNAMEPRES,POSITION='APPEND')
            ENDIF  ! End Responses simulated header writes
            WRITE (FNUMPRES,7401,ADVANCE='NO') RUN,EXCODE,TN,RN,
     A       PLDAYTMP,EDAPFR
            DO L = 1,KEYSTX
              IF (KEYPS(L).GT.0) WRITE (FNUMPRES,'(I6)',ADVANCE='NO')
     &        PSDAP(KEYPS(L))
            ENDDO
            WRITE (fnumpres,409)
     C       NINT(amtnit),
     D       NINT(hwam),hwumchar,
     E       NINT(hnumam),NINT(hnumpm),NINT(hnumgm),
     F       laixchar,lnumsm,
     G       NINT(cwam),NINT(vwam),
     H       rscm*100.0, hiamchar,
     I       NINT(rwam), AMIN1(99.9,shrtm),
     J       hnpcmchar,
     K       NINT(AMAX1(-99.0,cnam+rnam)),NINT(cnam),NINT(gnam),
     L       hinmchar,pltpop,NINT(tnumpm*pltpop),
     M       sradcav,tmaxcav,tmincav,
     M       (tmaxcav+tmincav)/2.0,
     N       NINT(raincc),daylcav,
     O       pariued,paruec,gfdur,
     P       wsdays,nsdays
 7401       FORMAT (I6,1X,A10,I6,I3,           !Run,excode,tn,rn
     A       I6,                               !Pldaytmp
     B       F6.2)                             !Edapfr
  409       FORMAT (
     C       I6,                               !Amtnit
     D       I6,A6,                            !gwam,hwum
     E       I6,I6,I6,                         !g#am,g#pm,g#gm
     F       A6,F6.1,                          !laix,lnumsm
     G       I6,I6,                            !Cwam,vwam
     H       F6.1,A6,                          !rs%m, hiam
     I       I6,F6.1,                          !N(rwam),shrtm
     J       A6,                               !hnpcm
     K       I6,I6,I6,                         !N(cnam+rnam),(cn),(gn)
     L       A6,F6.1,I6,                       !hinm,pltpop,tnumpm
     M       4F6.1,                            !RAD,TX,TN,TMEAN
     N       I6,F6.2,                          !RN,DL
     O       F6.3,F6.3,F6.2,                   !PIRUED,PARUEC,GFDUR
     P       2I6)                              !WSDAYS,NSDAYS
            CLOSE(FNUMPRES)
            ! End Responses simulated writes
            
            ! Plant responses (Measured)
            IF (TDATANUM.LE.0 .AND. .NOT.FEXISTA) THEN
              WRITE (fnumwrk,*)' '
              WRITE (fnumwrk,*)
     &         ' No data so cannot write PLANT RESPONSES (MEASURED)'
              OPEN (UNIT = FNUMTMP,FILE = FNAMEPREM,STATUS='UNKNOWN')
              CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
            ELSE
               IF (CROP.NE.CROPPREV.OR.RUN.EQ.1) THEN                
                OPEN (UNIT=FNUMPREM,FILE=FNAMEPREM,POSITION='APPEND')
                WRITE (FNUMPREM,*) ' '
                IF (TNAME(1:1).EQ.'*') THEN
                  WRITE (FNUMPREM,8951) EXCODE,TNAME(2:25)
 8951             FORMAT ('*RESPONSES(M):',A10,'  ',A24)
                ELSEIF (AMTNIT.LT.AMTNITPREV.OR.
     &                  PLYEARDOY.LT.PLYEARDOYPREV)THEN
                  WRITE (FNUMPREM,9952) EXCODE,TNAME(1:24)
 9952             FORMAT ('*RESPONSES(M):',A10,'  ',A24)
                ELSE
                  WRITE (FNUMPREM,895) EXCODE,ENAME(1:55)
  895             FORMAT ('*RESPONSES(M):',A10,'  ',A55)
                ENDIF
                WRITE (FNUMPREM,97,ADVANCE='NO')
                DO L = 1,KEYSTX
                  IF (KEYPS(L).GT.0) THEN
                   WRITE (FNUMPREM,'(A6)',ADVANCE='NO') PSABVO(KEYPS(L))
                  ENDIF 
                ENDDO
                WRITE (FNUMPREM,298)
  298         FORMAT (
     F        '  NICM',
     G        '  HWAM  HWUM',
     H        '  H#AM  H#PM  H#GM  LAIX  L#SM',
     I        '  CWAM  VWAM RSPCM  HIAM  RWAM SHRTM',
     J        '  HN%M  TNAM',
     K        '  CNAM  HNAM',
     L        '  HINM PLPOP  T#AM')
              ELSE
                OPEN (UNIT=FNUMPREM,FILE=FNAMEPREM,POSITION='APPEND')
              ENDIF ! End Responses measured header writes
              WRITE (FNUMPREM,7401,ADVANCE='NO') RUN,EXCODE,TN,RN,
     A         PLDAYTMP,EDAPFR
              DO L = 1,KEYSTX
                IF (KEYPS(L).GT.0) WRITE (FNUMPREM,'(I6)',ADVANCE='NO')
     &           PSDAPM(KEYPS(L))
              ENDDO
              WRITE (FNUMPREM,409)
     F         NINT(amtnit),NINT(hwamm),hwummchar,
     G         NINT(hnumamm),NINT(hnumpmm),NINT(hnumgmm),
     H         laixmchar,lnumsmm, 
     I         NINT(cwamm),NINT(vwamm),rscmm,hiammchar,
     J         NINT(rwamm),shrtmm,
     K         hnpcmmchar,NINT(tnamm),NINT(cnamm),NINT(hnamm),
     L         hinmmchar,pltpopp,
     M         MAX(-99,NINT(tnumpmm*pltpopp))
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
              IF (adapm.GT.0) THEN
               adaterr = 100.0*(adap-adapm)/adapm
              ELSE
               adaterr = -99
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
              IF (tnumamm.GT.0.AND.tnumam.GT.0) THEN
               tnumaerr = 100.0*((tnumam-tnumamm)/tnumamm)
              ELSE
               tnumaerr = -99
              Endif
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
              IF (gnamm.GT.0.AND.gnam.GT.0) THEN
               hnamerr = 100.0*(hnam-hnamm)/hnamm
              ELSE
               hnamerr = -99
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
     E          '    IDAP   IDAPE',
     F          '    MDAP   MDAPE',
     G          '    HWAH   HWAHE',
     H          '    HWUM   HWUME',
     I          '    H#AM   H#AME',
     J          '    H#GM   H#GME',
     K          '    LAIX   LAIXE',
     L          '    L#SM   L#SME',
     M          '    S#AM   S#AME',
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
     B         psidap,psidaterr,
     C         mdap,mdaterr,
     D         NINT(hwam),NINT(hwaherr),
     E         hwum,NINT(hwumerr),
     F         NINT(hnumam),NINT(hnumaerr),
     G         hnumgm,NINT(hnumgerr),
     H         laix,NINT(laixerr),
     I         lnumsm,NINT(lnumserr),
     J         NINT(tnumam),NINT(tnumaerr),
     K         NINT(cwam),NINT(cwamerr),
     L         NINT(vwam),NINT(vwamerr),
     M         hiam,NINT(hiamerr),
     N         hnpcm,NINT(hnpcmerr),
     O         NINT(cnam),NINT(cnamerr),
     P         NINT(hnam),NINT(hnamerr)
 8401         FORMAT (I6,1X,A10,1X,I6,I3,4X,A2,
     A         I8,  I8,
     B         I8,  I8,
     C         I8,  I8,
     D         I8,  I8,
     E         F8.3,I8,
     F         I8,  I8,
     G         F8.1,I8,
     H         F8.1,I8,
     I         F8.1,I8,
     J         I8  ,I8,
     K         I8,  I8,
     L         I8,  I8,
     M         F8.2,I8,
     N         F8.1,I8,
     O         I8,  I8,
     P         I8,  I8)
              CLOSE(FNUMERA)
            ENDIF ! End ErrorA writes (If data available)
          
            ! Errors summary (S)'
            IF (RUN.EQ.1) THEN
              OPEN(UNIT = FNUMERS,FILE = FNAMEERS,POSITION = 'APPEND')
              WRITE (FNUMERS,1996)
 1996         FORMAT ('*ERRORSUM',/)
              WRITE (FNUMERS,1896)
 1896         FORMAT ('@ RUNS',
     A        ' ADDED:EXCD ',
     B        '  TRNO RN',
     C        '    CR',
     D        '   EDAP#   EDAPE',
     E        '   IDAP#   IDAPE',
     F        '   MDAP#   MDAPE',
     G        '   HWAH#   HWAHE',
     H        '   HWUM#   HWUME',
     I        '   H#AM#   H#AME',
     J        '   H#GM#   H#GME',
     K        '   LAIX#   LAIXE',
     L        '   L#SM#   L#SME',
     M        '   S#AM#   S#AME',
     N        '   CWAM#   CWAME',
     O        '   VWAM#   VWAME',
     P        '   HIAM#   HIAME',
     Q        '   HN%M#   HN%ME',
     R        '   CNAM#   CNAME',
     S        '   HNAM#   HNAME')
              CLOSE(FNUMERS)
            ENDIF  ! END ErrorS header writes
            VARSUM = 0
            VARNUM = 0
            OPEN (UNIT = FNUMERA,FILE = FNAMEERA,STATUS = 'UNKNOWN')
            DO
              READ(FNUMERA,'(A354)',END = 7759)LINEERA
              IF (LINEERA(1:1).EQ.'@') EXIT
            ENDDO
            LINENUM = 0
            DO
              READ(FNUMERA,'(A354)',END = 7759)LINEERA
              LINENUM = LINENUM + 1
              DO L = 1,16
                STARTPOS = 42 + (L-1)*16
                VARCHAR = LINEERA(STARTPOS:STARTPOS+7)
                IF (VARCHAR.NE.'     -99') THEN
                  READ (VARCHAR,'(F8.0)') VARVAL
                  VARSUM(L) = VARSUM(L) + VARVAL*VARVAL
                  VARNUM(L) = VARNUM(L) + 1
                ENDIF
              ENDDO
            ENDDO
 7759       CONTINUE
            CLOSE (FNUMERA)
            DO L = 1,16
              IF (VARNUM(L).GT.0) THEN
                RMSE(L) = (VARSUM(L)/VARNUM(L))**0.5
              ELSE
                RMSE(L) = -99
              ENDIF
            ENDDO
            OPEN (UNIT = FNUMERS,FILE = FNAMEERS,POSITION = 'APPEND')
            WRITE (FNUMERS,6401) LINENUM,EXCODE,TN,RN,CROP,
     A       VARNUM(1),RMSE(1),
     B       VARNUM(2),RMSE(2),
     C       VARNUM(3),RMSE(3),
     D       VARNUM(4),RMSE(4),
     E       VARNUM(5),RMSE(5),
     F       VARNUM(6),RMSE(6),
     G       VARNUM(7),RMSE(7),
     H       VARNUM(8),RMSE(8),
     I       VARNUM(9),RMSE(9),
     J       VARNUM(10),RMSE(10),
     K       VARNUM(11),RMSE(11),
     L       VARNUM(12),RMSE(12),
     M       VARNUM(13),RMSE(13),
     N       VARNUM(14),RMSE(14),
     O       VARNUM(15),RMSE(15),
     P       VARNUM(16),RMSE(16)
 6401        FORMAT (I6,1X,A10,1X,I6,I3,4X,A2,
     A       I8,  F8.1,
     B       I8,  F8.1,
     C       I8,  F8.1,
     D       I8,  F8.1,
     E       I8,  F8.1,
     F       I8,  F8.1,
     G       I8,  F8.1,
     H       I8,  F8.1,
     I       I8,  F8.1,
     J       I8,  F8.1,
     K       I8,  F8.1,
     L       I8,  F8.1,
     M       I8,  F8.1,
     N       I8,  F8.1,
     O       I8,  F8.1,
     P       I8,  F8.1)
            CLOSE(FNUMERS)
            ! End of ErrorS writes
          
            ! Errors (T)
            IF (.NOT.FEXISTT .OR. FROPADJ.GT.1 .OR. IDETG.EQ.'N') THEN
              WRITE (fnumwrk,*) ' '
              IF (FROPADJ.GT.1) THEN
                WRITE (fnumwrk,*) ' Cannot write PLANT ERRORS (T).',
     &          ' Frequency of output > 1 day'
              ELSE  
                WRITE (fnumwrk,*)
     &          ' No data so cannot write PLANT ERRORS (T)'
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
                WRITE (FNUMWRK,*) 'No columns found in T-file '
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
                  WRITE (FNUMWRK,*) 'DAP in T-file <= 0 '
                  GO TO 7777
                ENDIF
                DO WHILE (tfdap.NE.pgdap)
                  TLINEGRO = ' '
                  READ (NOUTPG,7779,ERR=7777,END=7777) TLINEGRO
                  CALL Getstri(tlinegro,pgrocol(tfdapcol),pgdap)
                  IF (PGDAP.LT.0) THEN
                    WRITE (FNUMWRK,*) 'DAP in Plantgro file < 0 '
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
              WRITE(fnumwrk,*)'End of file reading Measured.out'
              WRITE(fnumwrk,*)
     &         'Starnum and starnumm were: ',starnum,starnumm
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
            OPEN (UNIT=FNUMTMP,FILE=FNAMEERS,STATUS='UNKNOWN')
            CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
            OPEN (UNIT=FNUMTMP,FILE=FNAMEERT,STATUS='UNKNOWN')
            CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
          
          ENDIF ! End of Error writes  IDETL.EQ.'A'

!-----------------------------------------------------------------------
!         IDETD (DISPLAY) OUTPUTS IF WORKING IN CROPSIM SHELL
!-----------------------------------------------------------------------

          ! Screen writes
          ! LAH April 2010 Must check this for Cropsimstand-alone
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
                CALL CSYR_DOY(STGYEARDOY(L),YEAR,DOY)
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
                CALL CSYR_DOY(STGYEARDOY(L),YEAR,DOY)
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
                CALL CSYR_DOY(YEARDOYHARF,YEAR,DOY)
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
     &         psname(KEYPS(L)),PSDap(KEYPS(L)),PSDapm(KEYPS(L))
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
     &       NINT(cwahc),NINT(cwahcm),
     &       NINT(spnumhc*pltpop),MAX(-99,NINT(spnumhcm*pltpop))
          
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
     D       NINT(tnumam),NINT(tnumamm),
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
          
          LABEL(1) = 'ADAT'; VALUE(1) = FLOAT(adat)
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
          IF (CROP.NE.'CS') THEN
            LABEL(13) = 'GNAM'; VALUE(13) = gnam
            LABEL(14) = 'PWAM'; VALUE(14) = hwam+chwt*pltpop*10.0
          ELSE
            LABEL(13) = 'GNAM'; VALUE(13) = hnam
            LABEL(14) = 'PWAM'; VALUE(14) = -99.0
          ENDIF  
          LABEL(15) = 'LAIX'; VALUE(15) = laix
          LABEL(16) = 'HIAM'; VALUE(16) = hiam
            
          LABEL(17) = 'DMPPM'; VALUE(17) = DMP_Rain 
          LABEL(18) = 'DMPEM'; VALUE(18) = DMP_ET                     
          LABEL(19) = 'DMPTM'; VALUE(19) = DMP_EP                     
          LABEL(20) = 'DMPIM'; VALUE(20) = DMP_Irr
          LABEL(21) = 'DMPNA'; VALUE(21) = DMP_NApp
          LABEL(22) = 'DMPNU'; VALUE(22) = DMP_NUpt
           
          LABEL(23) = 'YPPM ' ; VALUE(23) = GrP_Rain                  
          LABEL(24) = 'YPEM ' ; VALUE(24) = GrP_ET                   
          LABEL(25) = 'YPTM ' ; VALUE(25) = GrP_EP                    
          LABEL(26) = 'YPIM ' ; VALUE(26) = GrP_Irr
          LABEL(27) = 'YPNAM' ; VALUE(27) = GrP_NApp
          LABEL(28) = 'YPNUM' ; VALUE(28) = GrP_NUpt
           
          LABEL(29) = 'EDAP ' ; VALUE(29) = FLOAT(EDAP)     
          
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
          NFT = 1.0
          WFG = 1.0
          WFP = 1.0
          WFT = 1.0
          
          UNO3 = 0.0
          UNH4 = 0.0
          
          RUNCRP = RUNCRP + 1
          
          WRITE (fnumwrk,*) ' '
          WRITE (fnumwrk,'(A50)')
     &     ' END OF RUN. WILL BEGIN NEW CYCLE IF CALLED FOR.  '
          IF (IDETL.NE.'N') WRITE (fnumwrk,*) ' '
          SEASENDOUT = 'Y'
          
        ENDIF ! End STGYEARDOY(11).EQ.YEARDOY.OR.DYNAMIC.EQ.SEASEND

!-----------------------------------------------------------------------
!       Store variables for possible use next day/step
!-----------------------------------------------------------------------

        AMTNITPREV = AMTNIT
        CNADPREV = CNAD
        IF (CN.LE.1) CROPPREV = CROP
        IF (RNMODE.NE.'G') CUDIRFLPREV = CUDIRFLE
        CWADPREV = CWAD
        DAYLPREV = DAYL
        ECDIRFLPREV = ECDIRFLE
        ECONOPREV = ECONO
        EMRGFRPREV = EMRGFR
        GESTAGEPREV = GESTAGE
        LAIPREV = LAI
        LNUMPREV = LNUM
        PARIFPREV = PARIF
        PLYEARDOYPREV = PLYEARDOY
        SNOTPREV = SNOT
        SPDIRFLPREV = SPDIRFLE
        SRADPREV = SRAD
        SWFRPREV = SWFR
        TNUMPREV = TNUM
        VARNOPREV = VARNO
        YEARDOYPREV = YEARDOY

!***********************************************************************
      ELSEIF (DYNAMIC.EQ.SEASEND) THEN
!***********************************************************************

        IF (STGYEARDOY(11).NE.YEARDOY) THEN  ! End for non-crop reason
          WRITE (fnumwrk,*)' '
          WRITE (fnumwrk,'(A50)')
     &     ' Run terminated.(Usually because ran out of weather data).'
        ENDIF

        CLOSE (NOUTPG)
        CLOSE (NOUTPG2)
        CLOSE (NOUTPGF)
        CLOSE (NOUTPGN)
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
     X   CALL Disease(Spdirfle,run,runi,step, ! Run+crop component
     X    fropadj,outhed,                      ! Loop info.
     X    year,doy,dap,                        ! Dates
     X    didat,digfac,diffacr,                ! Disease details
     X    dcdat,dcfac,dcdur,dctar,             ! Disease control
     &    tmax,tmin,dewdur,                    ! Drivers - weather
     &    pla,plas,pltpop,                     ! States - leaves
     &    lnumsg,lap,LAPP,laps,                ! States - leaves
     &    stgyeardoy,                          ! Stage dates
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
     &' |-----Development Phase------|-------------Environment--------',
     &'------|----------------Stress-----------------|',/,
     &30X,'|--------Average-------|---Cumulative--|         (0=Min, 1=',
     &'Max Stress)         |',/,
     &25X,'Time  Temp  Temp Solar Photop         Evapo |----Water---|-',
     &'-Nitrogen--|--Phosphorus-|',/,
     &25X,'Span   Max   Min   Rad  [day]   Rain  Trans  Photo',9X,'Pho',
     &'to         Photo',/,
     &25X,'days    øC    øC MJ/m2     hr     mm     mm  synth Growth  ',
     &'synth Growth  synth Growth',/,110('-'))
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
     & /,'   Yield Productivity',T42,F7.2,' kg[grain yield]/m3[rain]',
     &                       T75,'=',F7.1,' kg[yield]/ha per mm[rain]',
     &//,' Evapotranspiration during growth season',T42,F7.1,' mm[ET]',
     & /,'   Dry Matter Productivity',T42,F7.2,' kg[DM]/m3[ET]',
     &                            T75,'=',F7.1,' kg[DM]/ha per mm[ET]',
     & /,'   Yield Productivity',T42,F7.2,' kg[grain yield]/m3[ET]',
     &                       T75,'=',F7.1,' kg[yield]/ha per mm[ET]',
     &//,' Transpiration during growth season',T42,F7.1,' mm[EP]',
     & /,'   Dry Matter Productivity',T42,F7.2,' kg[DM]/m3[EP]',
     &                            T75,'=',F7.1,' kg[DM]/ha per mm[EP]',
     & /,'   Yield Productivity',T42,F7.2,' kg[grain yield]/m3[EP]',
     &                       T75,'=',F7.1,' kg[yield]/ha per mm[EP]')

 1210 FORMAT(
     & /,' Irrigation during growing season',T42,F7.1,' mm[irrig]',
     & /,'   Dry Matter Productivity',T42,F7.2,' kg[DM]/m3[irrig]',
     &                       T75,'=',F7.1,' kg[DM]/ha per mm[irrig]',
     & /,'   Yield Productivity',T42,F7.2,' kg[grain yield]/m3[irrig]',
     &                       T75,'=',F7.1,' kg[yield]/ha per mm[irrig]')

 1220 FORMAT(
     & /,' N applied during growing season',T42,F7.1,' kg[N applied]/ha'
     & /,'   Dry Matter Productivity',T42,F7.1,' kg[DM]/kg[N applied]',
     & /,'   Yield Productivity',T42,F7.1,' kg[yield]/kg[N applied]')

 1230 FORMAT(
     & /,' N uptake during growing season',T42,F7.1,' kg[N uptake]/ha'
     & /,'   Dry Matter Productivity',T42,F7.1,' kg[DM]/kg[N uptake]',
     & /,'   Yield Productivity',T42,F7.1,' kg[yield]/kg[N uptake]')

      END  ! CSCRP

!-----------------------------------------------------------------------
!  CSCRPROOTWU Subroutine
!  Root water uptake rate for each soil layer and total rate.
!-----------------------------------------------------------------------

      SUBROUTINE CSCRPROOTWU(ISWWAT,                       !Control
     & NLAYR, DLAYR, LL, SAT, WFEU, MEWNU,                 !Soil
     & EOP,                                                !Pot.evap.
     & RLV, RWUPM, RLFWU, RWUMX, RTDEP,                    !Crop state
     & SW, WTDEP,                                          !Soil h2o
     & uh2o, trwup, trwu)                                  !H2o uptake

      IMPLICIT NONE

      INTEGER,PARAMETER::MESSAGENOX=10 ! Messages to Warning.out
      INTEGER,PARAMETER::NL=20         ! Maximum number soil layers,20

      REAL          BASELAYER     ! Depth at base of layer         cm
      REAL          DLAYR(20)     ! Depth of soil layers           cm
      REAL          DLAYRTMP(20)  ! Depth of soil layers with root cm
      REAL          EOP           ! Potential evaporation,plants   mm/d
      INTEGER       FNUMWRK       ! File number,work file          #
      LOGICAL       FOPEN         ! File open indicator
      INTEGER       L             ! Loop counter                   #
      REAL          LL(NL)        ! Lower limit,soil h2o           #
      INTEGER       MESSAGENO     ! Number of Warning messages     #
      INTEGER       NLAYR         ! Actual number of soil layers   #
      REAL          RLFWU         ! Root length factor,water,upper /cm2
      REAL          RLFW          ! Root length factor,water uptak #
      REAL          RLV(20)       ! Root length volume by layer    /cm2
      REAL          RLVTMP(20)    ! Root length volume by layer    #
      REAL          RTDEP         ! Root depth                     cm
      REAL          RWUMX         ! Root water uptake,maximum      mm2/m
      REAL          RWUP          ! Root water uptake,potential    cm/d
      REAL          SAT(20)       ! Saturated limit,soil           #
      REAL          SW(20)        ! Soil water content             #
      REAL          SWCON1        ! Constant for root water uptake #
      REAL          SWCON2(NL)    ! Variable for root water uptake #
      REAL          SWCON3        ! Constant for root water uptake #
      REAL          TRWU          ! Total water uptake             mm
      REAL          TRWUP         ! Total water uptake,potential   cm
      REAL          TSS(NL)       ! Number of days saturated       d
      REAL          UH2O(NL)      ! Uptake of water                cm/d
      REAL          WTDEP         ! Water table depth              cm
      REAL          WFEU          ! Water factor,evapotp,upper     #
      REAL          WFEL          ! Water factor,evapotp,lower     #
      REAL          WFEWU         ! Water excess fac,water uptake  #
      REAL          RWUPM         ! Pors size for max uptake       fr
      REAL          WUF           ! Water uptake factor            #
      REAL          WUP(NL)       ! Water uptake                   cm/d
      REAL          WUPR          ! Water pot.uptake/demand        #

      CHARACTER (LEN=1)   ISWWAT  ! Soil water balance switch Y/N
      CHARACTER (LEN=1)   MEWNU   ! Switch,root water uptake method
      CHARACTER (LEN=78)  MESSAGE(10)   ! Messages for Warning.out

      INTRINSIC ALOG,AMAX1,AMIN1,EXP,MAX,MIN

      SAVE

      IF (ISWWAT.EQ.'N') RETURN

      IF (FNUMWRK.LE.0.0) THEN

        CALL GETLUN ('WORK.OUT',FNUMWRK)
        INQUIRE (FILE = 'WORK.OUT',OPENED = FOPEN)
        IF (.NOT.FOPEN) OPEN (UNIT = FNUMWRK,FILE = 'WORK.OUT')

        ! Compute SWCON2 for each soil layer.  Adjust SWCON2 for very
        ! high LL to avoid water uptake limitations.
        WFEWU = 1.0
        DO L = 1,NL
          SWCON2(L) = 0.0
          RWUP = 0.0
        ENDDO
        DO L = 1,NLAYR
          SWCON2(L) = 120. - 250. * LL(L)
          IF (LL(L) .GT. 0.30) SWCON2(L) = 45.0
        ENDDO

        ! Set SWCON1 and SWCON3.
        SWCON1 = 1.32E-3
        SWCON3 = 7.01

        WFEL = 0.0

      ENDIF

      WUP = 0.0
      TRWUP   = 0.0
      BASELAYER = 0.0

      DO L = 1,NLAYR
        DLAYRTMP(L) = DLAYR(L)
        RLVTMP(L) = RLV(L)
        BASELAYER = BASELAYER + DLAYR(L)
        IF (RTDEP.GT.0.0.AND.RTDEP.LT.BASELAYER) THEN
        ! LAH Attempt to increase RLV when first penetrate a layer
        ! DLAYRTMP(L) = RTDEP-(BASELAYER-DLAYR(L))
        ! IF (DLAYRTMP(L).LE.0.0) EXIT
        ! RLVTMP(L) = RLV(L)*DLAYR(L)/DLAYRTMP(L)
        ENDIF
      ENDDO

      Messageno = 0
      DO L = 1,NLAYR
        RWUP = 0.
        IF (RLVTMP(L).LE.0.00001 .OR. SW(L).LE.LL(L)) THEN
          ! 1,* Use water below LL if just germinated and next layer >LL
          IF (L.EQ.1.AND.RTDEP.LE.DLAYR(1)) THEN
            IF (SW(2).GT.LL(2)) WUP(L) = EOP*0.1
            WRITE(Message(1),'(A57)') 
     &      'To avoid early stress,h2o uptake set equal to demand.  '
            CALL WARNING(1,'CSCRP',MESSAGE)
          ENDIF
        ELSE
          RWUP = SWCON1*EXP(MIN((SWCON2(L)*(SW(L)-LL(L))),40.))/
     &    (SWCON3-ALOG(RLVTMP(L)))
          ! Excess water effect
          WFEWU = 1.0
          IF (RWUPM.GT.0.0) THEN
            ! RWUPM = Relative position in SAT-DUL range before effect
            ! TSS(L) = number of days soil layer L has been saturated
            IF ((SAT(L)-SW(L)) .LT. RWUPM) THEN
              TSS(L) = 0.
            ELSE
              TSS(L) = TSS(L) + 1.
            ENDIF
            ! 2 days after saturation before water uptake is affected
            IF (TSS(L).GT.2.0) THEN
               WFEWU = MIN(1.0,MAX(0.0,(SAT(L)-SW(L))/RWUPM))
               WFEWU = 1.0 - (1.0-WFEWU)
               IF (WFEWU.LT.0.0) THEN
                 Messageno = Min(Messagenox,Messageno+1)
                 WRITE(Message(Messageno),'(A52,I3,a26,F4.2)')
     &           ' Water uptake resticted by saturation,layer',L,
     &           ' Uptake saturation factor ',wfewu
               ENDIF
            ENDIF
          ENDIF
          If (Messageno.GT.0) CALL WARNING(Messageno,'CSCRP',MESSAGE)
          RWUP = MIN(RWUP,RWUMX*WFEWU)
          WUP(L) = RWUP*RLVTMP(L)*DLAYRTMP(L)
          ! Alternative method.Linear decline below threshold RLV
          IF (MEWNU.EQ.'W'.OR.MEWNU.EQ.'B') THEN
            RLFW = 1.0
            IF (RLFWU.GT.0.0) RLFW = AMAX1(0.,AMIN1(1.,RLV(l)/RLFWU))
            WUP(L) = DLAYRTMP(L)*(SW(L)-LL(L))*RLFW
          ENDIF
        ENDIF
        TRWUP = TRWUP+WUP(L)
        IF (RLVTMP(L).LE.0.0) EXIT
      ENDDO

      IF (TRWUP.GT.0.0) THEN
        WUPR = TRWUP/(EOP*0.1)
        IF (WUPR.GE.WFEU) THEN
          WUF = (EOP*0.1) / TRWUP
        ELSEIF (WUPR.LT.WFEU) THEN
          ! Old function below
          !WUF = 1.0/WFEU + (1.0-1.0/WFEU)*(1.0-(TRWUP/(EOP*0.1*WFEU)))
          WUF = 1.0-(AMAX1(0.0,WUPR-WFEL)/(WFEU-WFEL))*(1.0-1.0/WFEU)
        ENDIF

        TRWU = 0.0
        DO L = 1, NLAYR
          UH2O(L) = WUP(L) * WUF
          TRWU = TRWU + UH2O(L)
        END DO

        IF (WTDEP.GT.0.0.AND.RTDEP.GE.WTDEP) THEN
          TRWU = EOP*0.1
          TRWUP = 20.0*TRWU
        ENDIF

      ELSE        !No root extraction of soil water
        TRWU = 0.0
        DO L = 1,NLAYR
          UH2O(L) = 0.0
        ENDDO
      ENDIF

      RETURN

      END  ! CSCRPROOTWU

!***********************************************************************
!  CSCRPLAYERS Subroutine
!  Leaf distribution module
!-----------------------------------------------------------------------

      SUBROUTINE Cscrplayers
     X (chtpc,clapc,                   ! Height-leaf area distribution
     X plpop,                          ! Plant population
     X lai,canht,                      ! Canopy area indices and height
     X lnumsg,lap,lapp,laps,           ! Leaf cohort number and size
     X LAIL,LAILA,                     ! Leaf area indices by layers
     X LAIA)                           ! Leaf area index,active

      IMPLICIT NONE

      INTEGER       clx           ! Canopy layers,maximum          #
      INTEGER       lcx           ! Leaf cohort number,maximum     #
      PARAMETER     (clx=30)      ! Canopy layers,maximum          #
      PARAMETER     (lcx=500)     ! Leaf cohort number,maximum     #

      REAL          caid          ! Canopy area index              m2/m2
      REAL          cailds(5,clx) ! Canopy area index,spp,layer    m2/m2
      REAL          canfrl        ! Canopy fraction,bottom of layr #
      REAL          canht         ! Canopy height                  cm
      REAL          clbase        ! Canopy base layer height       cm
      REAL          clthick       ! Canopy layer thickness         cm
      REAL          clthick1      ! Canopy top layer thickness     cm
      INTEGER       cltot         ! Canopy layer number,total      #
      REAL          chtpc(10)     ! Canopy ht % for lf area        %
      REAL          clapc(10)     ! Canopy lf area % down to ht    %
      REAL          lai           ! Leaf lamina area index         m2/m2
      REAL          laia          ! Leaf lamina area index,active  m2/m2
      REAL          lail(clx)     ! Leaf lamina area index         m2/m2
      REAL          laila(clx)    ! Lf lamina area index,active    m2/m2
      REAL          lailatmp      ! Leaf lamina area,active,temp   m2/m2
      REAL          lailtmp       ! Leaf lamina area,temporary     m2/m2
      REAL          lap(0:lcx)    ! Leaf lamina area,cohort        cm2/p
      REAL          lapp(lcx)     ! Leaf lamina area,infected      cm2/p
      REAL          laps(lcx)     ! Leaf lamina area,senescent     cm2/p
      REAL          lfrltmp       ! Leaves above bottom of layer   fr
      REAL          lfrutmp       ! Leaves above top of layer      fr
      INTEGER       lnumsg        ! Leaf cohort number             #
      REAL          plpop         ! Plant population               #/m2
      INTEGER       spp           ! Species                        #
      INTEGER       fnumwrk       ! Unit number for work file      #
      INTEGER       tvi1          ! Temporary variable,integer     #
      INTEGER       tvilc         ! Temporary value,lf cohort      #
      REAL          yvalxy        ! Y value from function          #

      LOGICAL       fopen         ! File status indicator          code

      INTRINSIC     AINT,MOD,INT

      SAVE

      spp = 1
      caid = lai
      lail = 0.0
      laila = 0.0
      laia=0.0                         ! LAI of active leaf

      IF(caid.LE.0.0)RETURN

      IF (FNUMWRK.LE.0.OR.FNUMWRK.GT.1000) THEN
        CALL Getlun ('WORK.OUT',fnumwrk)
        INQUIRE (FILE = 'WORK.OUT',OPENED = fopen)
        IF (.NOT.fopen) OPEN (UNIT = fnumwrk,FILE = 'WORK.OUT')
      ENDIF

      lfrutmp=1.0
      clthick=10.0                     ! Starting layer thickness (cm)

  10  CONTINUE
      cltot=INT(AINT(canht/clthick)+1.0)
      clthick1=MOD(canht,clthick)
      IF (clthick1.LE.0.0) THEN
        cltot = cltot - 1
        clthick1 = clthick
      ENDIF

      IF(cltot.GT.clx)THEN             ! Cannot have > clx layers
       clthick=clthick*2.0
       GOTO 10
      ENDIF

      cailds=0.0                       ! Caid by layer for species
      lailtmp=0.0
      lailatmp=0.0

      DO tvi1=cltot,1,-1               ! Do over layers down to soil

       IF (TVI1.EQ.CLTOT) THEN
         clbase = canht - clthick1
       ELSE
         clbase = clbase - clthick
       ENDIF
       canfrl = clbase / canht
       lfrltmp = YVALXY(CHTpc,CLApc,CANFRL)

       cailds(spp,tvi1)=caid*(lfrutmp-lfrltmp)

       DO tvilc=lnumsg,1,-1            ! Do over living cohorts
        IF(tvilc.GT.0)THEN
         lail(tvi1)=lail(tvi1)+lailtmp+
     x   (lap(tvilc)-laps(tvilc))*plpop*0.0001
         laila(tvi1)=laila(tvi1)+lailatmp
         IF ((lap(tvilc)-laps(tvilc)).GT.0.0)
     x    laila(tvi1)=laila(tvi1)+
     x    (lap(tvilc)-laps(tvilc)-lapp(tvilc))*plpop*0.0001

         ! Could adjust above for effect on activity as well as area
         ! ie multiply by a 0-1 factor dependent on pathogen and area
         lailtmp=0.0
         lailatmp=0.0
        ENDIF

        IF(caid.GT.0.AND.
     x   lail(tvi1).GE.cailds(spp,tvi1)*(lai/caid))THEN
          lailtmp=lail(tvi1)-cailds(spp,tvi1)*(lai/caid)
          lailatmp=laila(tvi1)*lailtmp/lail(tvi1)
          lail(tvi1)=lail(tvi1)-lailtmp
          laila(tvi1)=laila(tvi1)-lailatmp
        ENDIF

       ENDDO

       laia = laia + laila(tvi1)

       lfrutmp=lfrltmp

      ENDDO

      RETURN
      END

!-----------------------------------------------------------------------
!  EVAPO Subroutine
!  Calculates evapotranspiration,canopy temperature,resistances
!-----------------------------------------------------------------------

      SUBROUTINE EVAPO(MEEVP,                              !Input file
     & SRAD, CLOUDS, TMAX, TMIN, TDEW, WINDSP,             !Input,wther
     & ALBEDO, RATM, RCROP,                                !Input,crop
     & TVR1, TVR2, TVR3, TVR4, TVR5, TCAN, TASKFLAG)       !Output
      ! EO   EOP   EMP   EOPT  EOE

      IMPLICIT NONE

      REAL          TVR1
      REAL          ALBEDO        ! Reflectance of soil-crop surf  fr
      REAL          CLOUDS        ! Relative cloudiness factor,0-1 #
      REAL          CSVPSAT       ! Saturated vapor pressure air   Pa
      REAL          DAIR          ! Density of air
      REAL          EEQ           ! Equilibrium evaporation        mm/d
      REAL          EO            ! Potential evapotranspiration   mm/d
      REAL          EOPT          ! Potential evapot.,Priestly-T   mm/d
      REAL          EOP           ! Potential evapot.,Penman       mm/d
      REAL          EMP           ! Potential evap,Penman-Monteith mm/d
      REAL          EOE           ! Potential evapot.,Energy budgt mm/d
      REAL          VPSAT         ! Vapor pressure of air          Pa
      REAL          G             ! Soil heat flux density         MJ/m2
      REAL          LHVAP         ! Latent head of vaporization    J/kg
      REAL          PATM          ! Pressure of air = 101300.0     Pa
      REAL          PSYCON        ! Psychrometric constant         Pa/K
      REAL          RADB          ! Net outgoing thermal radiation MJ/m2
      REAL          RNET          ! Net radiation                  MJ/m2
      REAL          RNETMG        ! Radiant energy portion,Penman  mm/d
      REAL          RT            ! Gas const*temperature          #
      REAL          S             ! Change of sat vp with temp     Pa/K
      REAL          SBZCON        ! Stefan Boltzmann = 4.093E-9    MJ/m2
      REAL          SHAIR         ! Specific heat of air = 1005.0
      REAL          SLANG         ! Long wave solar radiation      MJ/m2
      REAL          SRAD          ! Solar radiation                MJ/m2
      REAL          TD            ! Approximation,av daily temp    C
      REAL          TDEW          ! Dewpoint temperature           C
      REAL          TK4           ! Temperature to 4th power       K**4
      REAL          TMAX          ! Maximum daily temperature      C
      REAL          TMIN          ! Minimum daily temperature      C
      REAL          VPD           ! Vapor pressure deficit         Pa
      REAL          VPSLOP        ! Sat vapor pressure v temp      Pa/K
      REAL          WFNFAO        ! FAO 24 hour wind function      #
      REAL          WINDSP        ! Wind speed                     m/s
      INTEGER       FNUMWRK       ! File number,work file          #
      LOGICAL       FOPEN         ! File open indicator            code
      REAL          emisa
      REAL          TAIR
      REAL          VPAIR
      REAL          emisac
      REAL          DLW           ! Downward long wave radiation   MJ/m2
      REAL          SIGMA
      REAL          RHOA
      REAL          CP
      REAL          hfluxc
      REAL          lefluxc
      REAL          vpaircan
      REAL          ULW
      REAL          RATM
      REAL          RATMSTORE
      REAL          HTVAP
      REAL          APRESS
      REAL          TCAN
      REAL          INPUT
      REAL          OUTPUT
      INTEGER       LOOPS
      REAL          ADDVAR
      REAL          SUBVAR
      REAL          RCROP
      REAL          RSADJ         ! Stomatal res,adjusted Co2+H2o
      REAL          TVR2
      REAL          TVR3
      REAL          TVR4
      REAL          TVR5

      CHARACTER (LEN=1)   MEEVP         ! Evaptr calculation method
      CHARACTER (LEN=1)   TASKFLAG      ! Flag for required task

      INTRINSIC AMAX1,EXP,SQRT,ABS

      SAVE

      PARAMETER     (PATM=101300.0)    !
      PARAMETER     (SBZCON=4.903E-9)  !(MJ/K4/m2/d) fixed constant
      PARAMETER     (SHAIR=1005.0)     ! MJ/kg/K?? or need*10-5

      TCAN = -99.0

      IF (FNUMWRK.LE.0.OR.FNUMWRK.GT.1000) THEN
        CALL Getlun ('WORK.OUT',fnumwrk)
        INQUIRE (FILE = 'WORK.OUT',OPENED = fopen)
        IF (.NOT.fopen) OPEN (UNIT = fnumwrk,FILE = 'WORK.OUT')
      ENDIF

      RT = 8.314 * ((TMAX+TMIN)*0.5 + 273.0)             ! N.m/mol ??
      VPAIR = CSVPSAT(TDEW)                              ! Pa
      DAIR = 0.1*18.0/RT*((PATM-VPAIR)/0.622+VPAIR)      ! kg/m3
      LHVAP = (2501.0-2.373*(TMAX+TMIN)*0.5)*1000.0      ! J/kg
      PSYCON = SHAIR * PATM / (0.622*LHVAP)              ! Pa/K
      VPSAT = (CSVPSAT(TMAX)+CSVPSAT(TMIN)) / 2.0        ! Pa
      VPD = VPSAT - VPAIR                                ! Pa
      S = (VPSLOP(TMAX)+VPSLOP(TMIN)) / 2.0              ! Pa/K

      ! Default for use when no RATM specified for EO
      IF (TASKFLAG.EQ.'O') THEN
        RCROP = 0.0
        IF (RATMSTORE.LE.0.0) RATMSTORE = 300.0
        RATM = RATMSTORE
        TASKFLAG = 'A'
      ELSE
        RATMSTORE = RATM
      ENDIF

      ! Adjusted resistance
      IF (TASKFLAG.EQ.'R') THEN
         RSADJ =
     &    ((((((S*RNETMG+(DAIR*1.0E-2*SHAIR*1.0E-6*VPD)
     &    /(RATM*1.157407E-05))/
     &    TVR1)-S))/PSYCON)-1)*(RATM*1.157407E-05)
         TVR3 = RSADJ/1.157407E-05
         RETURN
      ENDIF

      IF (TASKFLAG.EQ.'A'.OR.TASKFLAG.EQ.MEEVP) THEN
        ! Penman
        ! Net radiation (MJ/m2/d). RADB constants Jensen et al (1989)
        ! for semi-humid conditions. 0.005 changes 0.158 from kPa to Pa.
        G = 0.0
        TK4 = ((TMAX+273.)**4+(TMIN+273.)**4) / 2.0
        RADB = SBZCON*TK4*(0.4-0.005*SQRT(VPAIR))*(1.1*(1.-CLOUDS)-0.1)
        RNET= (1.0-ALBEDO)*SRAD - RADB
        RNETMG = (RNET-G) / LHVAP * 1.0E6 ! MJ/m2.d to mm/day
        ! Resistance using FAO wind function. Multipliers for WNDFAO are
        ! 1000 times smaller than Jensen et al (1979) to convert VPD Pa
        ! to kPa.
        WFNFAO = 0.0027 * (1.0+0.01*WINDSP)
        EOP = (S*RNETMG + PSYCON*WFNFAO*VPD) / (S+PSYCON)
        EO = AMAX1(EOP,0.0)
      ENDIF

      ! Monteith
      IF (TASKFLAG.EQ.'A'.OR.TASKFLAG.EQ.MEEVP) THEN
        EMP = ((S*RNETMG+(DAIR*1.0E-2*SHAIR*1.0E-6*VPD)
     &   /(RATM*1.157407E-05))/
     &   (S+PSYCON*(1+rcrop/ratm)))
        EO = AMAX1(EMP,0.0)
      ENDIF

      ! Priestley-Taylor (so-called!)
      IF (TASKFLAG.EQ.'A'.OR.TASKFLAG.EQ.MEEVP) THEN
        TD = 0.60*TMAX+0.40*TMIN
        SLANG = SRAD*23.923
        EEQ = SLANG*(2.04E-4-1.83E-4*ALBEDO)*(TD+29.0)
        EOPT = EEQ*1.1
        IF (TMAX .GT. 35.0) THEN
          EOPT = EEQ*((TMAX-35.0)*0.05+1.1)
        ELSE IF (TMAX .LT. 5.0) THEN
          EOPT = EEQ*0.01*EXP(0.18*(TMAX+20.0))
        ENDIF
        EO = AMAX1(EOPT,0.0)
      ENDIF

      ! Energy budget
      IF (TASKFLAG.EQ.'A'.OR.TASKFLAG.EQ.MEEVP.OR.TASKFLAG.EQ.'C') THEN
        IF (TASKFLAG.EQ.'C') lefluxc = tvr1/1.0E6*lhvap
        tair = (tmax+tmin)/2.0
        sigma=5.673*1.0E-8     ! Stephan Bolzman constant (J m-2k-4s-1)
        !emisa=0.72+0.005*tair
        emisa=0.61+0.05*sqrt(vpair/100)! Emissivity,cloudless Campbell
        emisac=emisa+clouds*(1.-emisa-(8.0/(tair+273.0)))
        dlw=emisac*sigma*(tair+273.0)**4.
        cp=1.005                           ! Specific heat (J g-1 c-1)
        apress=100000.0                    ! Atmospheric pressure (pa)
        rhoa=(3.4838*apress/(tair+273.))   ! Air density  (g/m3)
        htvap=2500.3-2.297*tair            ! Latent ht  J g-1
        dlw = dlw*(60.0*60.0*24.0*1.0E-6)  ! MJ/,2.d <-- J/m2.s
        tcan = tair
        loops = 0
        addvar = 1.0
        subvar = 1.0
 333    CONTINUE
        loops = loops + 1
        vpaircan = CSVPSAT(TCAN)                    ! Pa
        hfluxc=(cp*rhoa*(tcan-tair)/(ratm*0.6))     ! Heat J/m2.s
        hfluxc = hfluxc/(1.0E6/(60.0*60.0*24.0))    ! MJ/,2.d <-- J/m2.s
        IF (taskflag.NE.'C') THEN
          lefluxc=(htvap*rhoa*0.622/apress*(vpaircan-vpair)/
     X      (rcrop+ratm*0.6))                       ! H2o,plant J/m2.s
          lefluxc = lefluxc/(1.0E6/(60.0*60.0*24.0))! MJ/,2.d <-- J/m2.s
        ENDIF
        ulw=sigma*(tcan+273.)**4.   !! Upward long wave radiation
        ulw = ulw/(1.0E6/(60.0*60.0*24.0))  ! MJ/,2.d <-- J/m2.s
        input = (srad+dlw)
        output = ulw + hfluxc + lefluxc
        IF (input.GT.output) THEN
          IF (addvar.GE.1.0) subvar = 0.5
          IF (addvar.GE.0.5.AND.addvar.LT.1.0) subvar = 0.3
          IF (addvar.GE.0.3.AND.addvar.LT.0.5) subvar = 0.2
          IF (addvar.GE.0.2.AND.addvar.LT.0.3) subvar = 0.1
          tcan = tcan + addvar
          IF (loops.LT.20.AND.ABS(input-output).GE.1.0) GO TO 333
        ENDIF
        IF (input.LT.output) THEN
          IF (subvar.GE.1.0) addvar = 0.5
          IF (subvar.GE.0.5.AND.subvar.LT.1.0) addvar = 0.3
          IF (subvar.GE.0.3.AND.subvar.LT.0.5) addvar = 0.2
          IF (subvar.GE.0.2.AND.subvar.LT.0.3) addvar = 0.1
          TCAN = TCAN - subvar
          IF (loops.LT.20.AND.ABS(input-output).GE.1.0) GO TO 333
        ENDIF
        eoe = lefluxc/lhvap*1.0E6
        eo = AMAX1(eoe,0.0)
      ENDIF

      TVR1 = EO
      TVR2 = EOP
      TVR3 = EMP
      TVR4 = EOPT
      TVR5 = EOE

      RETURN

      END  ! EVAPO


      ! Stuff for exploring temperature responses
      !   DO  L = 1,50
      !    TMEAN = FLOAT(L)
      !    tmax = tmean
      !    tmin = tmean
      !   ! Below is the function for max phs (Pm) for AFRC
      !   PM =
      !  &1.0/3.21447*
      !  &(0.044*6.0*1.0E9*(tmean+273.0)*
      !  &exp(-14200/(1.987*(tmean+273.0))))/
      !  &(1.0+exp(-47000/(1.987*(tmean+273.0)))*exp(153.4/1.987))
      !   ! And below for actual
      !   RA = 30.0  ! s/m
      !   RM = 400.0 ! s/m
      !   VPD = 0.0
      !   QPAR = 600.0
      !   ALPHA = 0.009   ! mg/J
      !   RS = 1.56*75.0*(1.0+100.0/QPAR)*(1.0-0.3*VPD)
      !   ! RS = 234,156,137 s/m at QPAR'S of 100,300,600 (0.0VPD)
      !   RP = RA + RS + RM
      !   PMAX = 0.995*QPAR/RP
      !   PHSA = (0.995/PMAX)*((1.0/(ALPHA*QPAR))+(1.0/PM))
      !   PHSB = -((1.0/(ALPHA*QPAR))+(1.0/PM)+(1.0/PMAX))
      !   PGROSS =( -PHSB - SQRT(PHSB**2-4.0*PHSA))/(2.0*PHSA)
      !   Below is Ceres grain fill
      !   IF (Tmean .GT. 10.0) THEN
      !     RGFILL =
      !      0.65+(0.0787-0.00328*(TMAX-TMIN))*(Tmean-10.)**0.8
      !   ENDIF
      !   IF (Tmean .LE. 10.0) THEN
      !      RGFILL = 0.065*Tmean
      !   ENDIF
      !   Below is AFRC grain fill
      !   rgfill = 0.045*tmean + 0.4
      !   Below is for Swheat
      !   write(1,'(3f10.5)')TMEAN,(0.000913*TMEAN+0.003572),
      !  &  (0.000913*TMEAN+0.003572)/((0.000913*15.0+0.003572))
      !   ENDDO
      !   STOP

      ! Wang and Engel daylength factor algorithm
      ! pps0 =  8.65
      ! IF (DAYL.GT.Pps0) THEN
      !   tvr1 = 4.0/(ppthr-pps0)
      !   DF1(1) =
      !    AMAX1(0.0,AMIN1(1.,1.0 - exp(-tvr1*(dayl-pps0))))
      ! ELSE
      !   DF1(1) = 0.0
      ! ENDIF
      ! df1(2) = df1(1)
      ! df2 = df1(1)
