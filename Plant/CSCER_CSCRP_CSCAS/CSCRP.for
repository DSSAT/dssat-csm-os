!-----------------------------------------------------------------------
!  CROPSIM CEREAL GROWTH AND DEVELOPMENT MODULE  Version 010115
!
!  Version changed to accomodate new CUL,ECO files;minor other changes
!
!  Last edit 050415  Few more writes to Work.out 
!-----------------------------------------------------------------------

      SUBROUTINE CSCRP (FILEIOIN, RUN, TN, RN, RNMODE,     !Command line
     & ISWWAT, ISWNIT, ISWDIS, MESOM,                      !Contols
     & IDETS, IDETO, IDETG, IDETL, FROP,                   !Controls
     & SN, ON, RUNI, REP, YEAR, DOY, STEP, CN,             !Run+loop
     & SRAD, TMAX, TMIN, TAIRHR, RAIN, CO2, TDEW,          !Weather
     & DRAIN, RUNOFF, IRRAMT,                              !Water
     & DAYLT, WINDSP, DEWDUR, CLOUDS, ST, EO, ES,          !Weather
     & NLAYR, DLAYR, DEPMAX, LL, DUL, SAT, BD, SHF, SLPF,  !Soil states
     & SNOW, SW, NO3LEFT, NH4LEFT, FERNIT,                 !H2o,N states
     & TLCHD, TNIMBSOM, TNOXD,                     !NEW    !N components
     & TOMINFOM, TOMINSOM, TOMINSOM1, TOMINSOM2, TOMINSOM3,!N components
     & YEARPLTCSM, HARVFRAC,                               !Pl.date
     & PARIP, PARIPA, EOP, EP, ET, TRWUP, ALBEDOS,         !Resources
     & CAID, KCAN, KEP,                                    !States
     & RLV, NFP, RWUPM, RWUMX, CANHT, LAIL, LAILA,         !States
     & UNO3, UNH4, UH2O,                                   !Uptake
     & SENCALG, SENNALG, SENLALG,                          !Senescence
     & RESCALG, RESNALG, RESLGALG,                         !Residues
     & STGYEARDOY, GSTAGE,                                 !Stage dates
     & DYNAMIC)                                            !Control
     
      ! Staging in comparison with Ceres
      !  1 Germinate               1 Germinate
      !  2 Terminal spikelet       2 Terminal spikelet
      !  3 Pseudo-stem
      !  4 End leaf                3 End leaf
      !  5 Head emerge             4 End spike growth
      !  6 Anthesis
      !  7 End anthesis
      !  8 Milk end/start dough    5 Begin rapid grain geroowth
      !  9 Hard dough              6 Physiological maturity  
      ! 10 Harvest                 6.9 Harvest 
      
      ! Changes made after sending model to Jeff for Agmip work.
      !  1. Variables passed to main module for outputting in Summary.out 
      !  2. Version check for genotype files re-introduced
      !  3. Phenology temperature responses made to work for differrent 
      !     phases (was not working so responses for phase 1 for all)
      !  4. Initialized tfdnext. Not initialized so sequence of runs 
      !     used previous tfdnext for 1st phase, hence ADAT differed
      !     for sequence of runs.
      !  5. Capability to start at time of 1st irrig or at emergence 
      !     introduced
      !  6. Aitken formula introduced to calculate leaf number
      !     to emerge after terminal spikelet.
      !  7. Radiation effect on grain number set at anthesis
      !  8. Potential grain size reduced if canopy size over
      !     threshold.
      !  9. Temperature responses changed.
      ! 10. Minor 'bugs' corrected.
      
      ! NB. Some additional controls that are not handled by the CSM 
      !     inputs routine are read directly from the x-file in the
      !     section of code dealing with 'additional controls'. The
      !     same approach could be used to input grazing information
      !     if CSM is not modified to handle the additional grazing
      !     inputs necessary. 
         
     
      ! NB. TN, et.are not provided when running under CSM (DS4 files). 
      !     They are read in XREADC in the CSREA module. 

      ! Changes for AZMC  01/02/2014
      !  1. Account for 'haying off' (Using TKGF from SPE file)
      !  2. SRAD in phase 5 (head emergence to anthesis) effect on grain #
      !     (using G#RF,G#RT from ECO file)
      !  3. Potential grain size reduced as temperature in phase 5 increases
      !     (Using GWTAF,GWTAT from ECO file)  

      ! Changes for RORO  28/11/2013
      !  1. HINM and VNPCM calculated without retained dead leaf
      !  2. Root N taken out of remobilization pool
      !     (N movement to grain from stem first,leaves 2nd.) 
      !  3. Stem N limits changed. End minimum from 0.4 to 0.15
      !  4. Labile N set at 10%
      !  5. NUSEFAC left as NLABPC/100.0. No increase in late grain fill
      !  6. NUPWF left at 1.0 after trying 0.0 (1.0->0.0 with changes 07/10/2014).
      !  7. Calibration to increase tiller death (TDFAC 6->9); increase
      !     leaf area (LAFV 0.1->0.3).  NB. Do not have a response to N
      !     if potential leaf area too small (LAIS or increase factors)
      !  8. Kernel wt.(GWTS) set too high at 50 to enable better 
      !     simulation of yield. Often cannot simulate both K.Wt.and
      !     yield ... maybe k.wt measurement after too good winnowing,
      !     or moisture % problems.
      !  9. Vegetative N% too low for higher N treatments ... need to
      !     translocate less when have high soilN. 

      ! Changes for KSAS  02/12/2013
      !  1. Established flag set when LAI>0.0,not when shoot/root>2 
      !     (But ? this as allows re-use in the fall)
      !  2. May need to take reserve CH2O out of calculation of VNPCM

      ! Changes for SWSW  05/12/2013
      !  1. Minimum grain N% from species to ecotype characteristic.
      

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
      !    3. Organise echo of input variables in Work.out into logical 
      !       groups

      ! Questions
      !    1. What x-file information is essential to run
      !    2. Impact of N uptake variable (NUPCF,NUPNF,NUPWF) 

      ! Checks needed 
      !    1. Algorthms for fraction of day at changeovers
      !    2. Blips in response curves and reasons for these
      !    3  All declarations,initialisations to 0 or -99

      USE OSDefinitions
      
      IMPLICIT NONE

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
      REAL          ALBEDOS       ! soil albedo                    fr
      REAL          AMTNIT        ! Cumulative amount of N applied kg/ha
      REAL          ANDEM         ! Crop N demand                  kg/ha
      REAL          ANFER(200)    ! N amount in fertilizer appln   kg/ha
      INTEGER       ARGLEN        ! Argument component length      #
      INTEGER       ASTG          ! Anthesis stage                 #
      REAL          AVGSW         ! Average soil water in SWPLTD   %
      REAL          AWNAI         ! Awn area index                 m2/m2
      REAL          AWNS          ! Awn score,1-10                 #
      REAL          BASELAYER     ! Depth at base of layer         cm
      REAL          BD(20)        ! Bulk density (moist)           g/cm3
      REAL          BLAYR(20)     ! Base of soil layers            cm
      REAL          CAID          ! Canopy area index              #
      REAL          CANHT         ! Canopy height                  cm
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
      INTEGER       DAPCALC       ! DAP output from funcion        #
      INTEGER       DAS           ! Days after start of simulation d
      INTEGER       DATE          ! Date (Year+Doy)                #
      INTEGER       DATECOL       ! Date column number             #
      REAL          DAYLT         ! Daylength,6deg below horizon   h
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
      REAL          DEPMAX        ! Maximum depth of soil profile  cm
      REAL          DEWDUR        ! Dew duration                   h
      REAL          DF            ! Daylength factor 0-1           #
      REAL          DFNEXT        ! Daylength factor,next phase    #
      REAL          DFOUT         ! Daylength factor for output    #
      REAL          PPFPE         ! Photoperiod factor,pre-emerge  #
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
      REAL          EORATIO       ! Pevap increase w LAI (<6)      #
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
      INTEGER       EYEARDOYM     ! Emergence measured Year+DOY    #
      REAL          FAC(20)       ! Factor ((g/Mg)/(kg/ha))        #
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
      INTEGER       FNUMWRK       ! File number,work file          #
      INTEGER       FROP          ! Frquency of outputs,as sent    d
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
      REAL          GSTAGE        ! Growth stage                   #
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
      REAL          IRRAMT        ! Irrigation amount for today    mm
      REAL          IRRAMTC       ! Irrigation amount,cumulative   mm
      REAL          ISOILH2O      ! Initial soil water             cm   
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
      REAL          LAFR          ! Leaf area increase factor,rep  #
      REAL          LAFST         ! Leaf area factor change stage  #
      REAL          LAFSTDU       ! Leaf area factor change st,DU  #
      REAL          LAFSWITCH     ! Leaf # changed increase factor #
      REAL          LAFV          ! Leaf area increase factor,veg  #
      REAL          LAGEDU(0:LNUMX)! Leaf age at leaf position     C.d
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
      REAL          LL(20)        ! Lower limit,soil h2o           #
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
      REAL          NFP           ! N factor,photosynthesis 0-1    #
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
      REAL          NH4LEFT(20)   ! NH4 concentration in soil      g/Mg
      REAL          NH4MN         ! NH4 conc minimum for uptake    g/Mg
      REAL          NLABPC        ! N labile fraction,standard     %
      INTEGER       NLAYR         ! Number of layers in soil       #
      INTEGER       NLAYRROOT     ! Number of layers with roots    #
      INTEGER       NLIMIT        ! N limited grain growth  (Days) #
      REAL          NUPCF         ! N uptake cultivar factor       #    
      REAL          NUPNF         ! N uptake concentration factor  #    
      REAL          NO3FN         ! NO3 conc factor,NO3 uptake 0-1 #
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
      REAL          NUSEFAC       ! N use factor;mx nuselim        #
      REAL          NUSELIM       ! N limit on N for grain filling #
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
      REAL          RLFWU         ! Root length factor,water uptk  /cm2
      REAL          RLIGPC        ! Root lignin concentration      %
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
      REAL          RNUSE(0:2)    ! Root N use,overall and parts   g   
      REAL          ROOTN         ! Root N                         g/p
      REAL          ROOTNEXCESS   ! Root N > critical              g/p
      REAL          ROOTNS        ! Root N senesced                g/p
      REAL          ROWSPC        ! Row spacing                    cm
      INTEGER       RPCOL         ! Replicate column number        #
      REAL          RRESP         ! Root respiration fraction      #
      REAL          RSPCA         ! Reserves conc,anthesis         %
      REAL          RSCD          ! Reserves concentration,end day fr
      REAL          RSPCLX        ! Reserves conc,leaves,max.      #
      REAL          RSCM          ! Reserves concentration,mature  fr
      REAL          RSCMM         ! Reserves conc,maturity,msured  #
      REAL          RSPCX         ! Reserves conc.max.->overflow   #
      REAL          RSCX          ! Reserves concentration,maximum fr
      REAL          RSEN          ! Root senescence                %/d
      REAL          RSFP          ! Reserves factor,photosynthesis fr
      REAL          RSFPL         ! Reserves conc.,phs.lower bound fr
      REAL          RSFPU         ! Reserves conc.,phs upper bound fr
      REAL          RSN           ! Reserve N                      g/p
      REAL          RSNAD         ! Reserve N                      kg/ha
      REAL          RSNEED        ! Reserves need to bring to min  g/p
      REAL          RSNPH         ! Reserves N harvested           g/p
      REAL          RSNPHC        ! Reserves N harvested,cum       g/p
      REAL          RSNUSED       ! Reserve N used                 g/p
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
      REAL          SENCALG(0:20) ! Senesced C added,by layer      kg/ha
      REAL          SENCAS        ! Senesced C added to soil       kg/ha
      REAL          SENCL(0:20)   ! Senesced C,by layer            g/p
      REAL          SENCS         ! Senesced C added to soil       g/p
      REAL          SENFR         ! Senesced fraction lost from pl #
      REAL          SENGF         ! Senesced during grain fill     g/p
      REAL          SENLA         ! Senesced leaf area,total       cm2/p
      REAL          SENLALITTER   ! Senesced leaf area,litter      cm2/p
      REAL          SENLARETAINED ! Senesced leaf area,retained    cm2/p
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
      REAL          SHF(20)       ! Soil hospitality factor 0-1    #
      REAL          SHRTD         ! Shoot/root ratio               #
      REAL          SHRTM         ! Shoot/root ratio,maturity      #
      REAL          SHRTMM        ! Shoot/root ratio,maturity,meas #
      REAL          SLA           ! Specific leaf area             cm2/g
      REAL          SLAOUT        ! Specific leaf area for output  cm2/g
      REAL          SLIGPC        ! Stem lignin concentration      %
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
      REAL          SNH4(20)      ! Soil NH4 N                     kg/ha
      REAL          SNH4PROFILE   ! Soil NH4 N in profile          kg/ha
      REAL          SNH4ROOTZONE  ! Soil NH4 N in root zone        kg/ha
      INTEGER       SNI           ! Sequence number,as initiated   #
      REAL          SNO3(20)      ! Soil NO3 N                     kg/ha
      REAL          SNO3PROFILE   ! Soil NO3 N in profile          kg/ha
      REAL          SNO3ROOTZONE  ! Soil NO3 N in root zone        kg/ha
      REAL          SNOW          ! Snow                           cm
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
      REAL          SRAD          ! Solar radiation                MJ/m2
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
      REAL          STEMNEXCESS   ! Stem N > critical              g/p
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
      REAL          TCDIF         ! Canopy temperature - air temp  C
      INTEGER       TDATANUM      ! Number of data from t-file     #
      REAL          TDEW          ! Dewpoint temperature           C
      REAL          TDIFAV        ! Temperature difference,can-air C
      REAL          TDIFNUM       ! Temperature difference,# data  #
      REAL          TDIFSUM       ! Temperature difference,sum     C
      REAL          TFAC4         ! Temperature factor function    #
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
      REAL          TLCHD         ! N leached this day             kg/ha
      INTEGER       TLIMIT        ! Temp.limited grain gr (Days)   #
      INTEGER       TLINENUM      ! Temporary var,# lines in tfile #
      INTEGER       TLPOS         ! Position on temporary line     #
      REAL          TMAX          ! Temperature maximum            C
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
      REAL          TMIN          ! Temperature minimum            C
      REAL          TMINCAV       ! Temperature,minimum,cycle av   C
      REAL          TMINCC        ! Temperature,min,cycle sum     C.d  
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
      REAL          TNUMPREV      ! Tiller (+main stem) #,previous #/p
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
      REAL          TRWUP         ! Total water uptake,potential   cm
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
      INTEGER       VARNUM(30)    ! Variable number in sum         #
      REAL          VARSUM(30)    ! Temporary variables sum        #
      REAL          VARVAL        ! Temporary variable             #
      REAL          VBASE         ! Vrn requirement before devment d
      REAL          VCNC          ! Vegetative critical N conc     #
      REAL          VDLOS         ! Vernalization lost (de-vern)   d
      REAL          VEEND         ! Vernalization effect end Rstge #
      REAL          VEENDDU       ! Vernalization effect end DU    #
      REAL          VEFF          ! Vernalization effect,max.reduc fr
      INTEGER       VERSION       ! Version #                      #    
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
      INTEGER       YEARM         ! Year of measurement            #
      INTEGER       YEARSIM       ! Year+Doy for simulation start  #
      INTEGER       YEARPLTCSM    ! Planting year*1000+DOY,CSM     #
      REAL          YVALXY        ! Y value from function          #
      REAL          ZSTAGE        ! Zadoks stage of development    #

      ! Following are to test Doug Stewart's function 
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
      CHARACTER (LEN=250) FILEIOIN      ! Name of input file
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
      CHARACTER (LEN=6)   LATLC         ! Leaf area,actual
      CHARACTER (LEN=354) LINEERA       ! Temporary line,error-a file
      CHARACTER (LEN=80)  LINESTAR      ! Group header line (with star)
      CHARACTER (LEN=80)  LINESTAR2     ! Group header line (with star)
      CHARACTER (LEN=180) LINET         ! Line from T-file
      CHARACTER (LEN=1)   MEEXP         ! Switch,experimental method = E
      CHARACTER (LEN=1)   MEEVP         ! Switch,potential evapot method
      CHARACTER (LEN=1)   MEPHS         ! Switch,photosynthesis method
      CHARACTER (LEN=1)   MESOM         ! Switch,OM decay method       
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
      CHARACTER (LEN=1)   RNMODE        ! Run mode (eg.I=interactive)
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

      ! For Ceres temperature response
      ! REAL RGFILL
     
      ! For Jamieson model need:
      !REAL  DAYLSAT,LNUMDLR,LNUMMIN,PRNUM,PRNUMCRIT 
      
      ! PROBLEMS
      !   1. Chaff weight is too low for stressed treatment (UXMC)
      !      Need reserves to be pumped into chaff somehow!

      SAVE


!***********************************************************************
      IF (DYNAMIC.EQ.RUNINIT) THEN    ! Initialization
!***********************************************************************

       IF (RUNCRP.LE.0) THEN          ! First time through

          MODNAME(1:8) = 'CSCRP047'
          VERSION = 010115         
          GENFLCHK(3:15) = 'CRP047.08102017'
          ! Control flags/switches
          CFLPDATE = 'P'      ! P=at planting;I=at first irrigation;
                              ! E=relative to emergence
          CFLNOUTPUTS = 'N'   ! Y=special N outputs written
          ISWWATCROP = 'Y'    ! E=no water stress effect on crop growth

!-----------------------------------------------------------------------
!         Set parameters (Most should be placed in input files!)
!-----------------------------------------------------------------------

          ! Physical constants
          MJPERE = 220.0*1.0E-3  ! MJ per Einstein at 540 nm
          PARMJFAC = 0.5         ! PAR in SRAD (fr)

          ! Model standard parameters
          STDAY = 20.0      ! TT in standard day
          STEPNUM = 1       ! Step number per day set to 1

!-----------------------------------------------------------------------
!         Determine base of soil layers
!-----------------------------------------------------------------------

          BLAYR = 0.0
          BLAYR(1) = DLAYR(1)
          DO L = 2,NLAYR
            BLAYR(L) = BLAYR(L-1) + DLAYR(L)
          ENDDO  
          
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
            ELSEIF (RNMODE.EQ.'B'.OR.RNMODE.EQ.'N'.OR.RNMODE.EQ.'Q'
     &               .OR.RNMODE.EQ.'Y')THEN
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
          CALL GETLUN (FNAMEPRES,fnumpres)
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

          ! ERROR FILES
          FNAMEERA(1:12) = 'Plantera.'//out
          FNAMEERT(1:12) = 'Plantert.'//out
          CALL GETLUN (FNAMEERT,fnumert)
          CALL GETLUN (FNAMEERA,fnumera)

          ! LAH March 2010 Metadata taken out. Check later if needed
          ! METADATA FILES
          !CALL GETLUN ('META',FNUMMETA)

!-----------------------------------------------------------------------
!         Open and write main headers to output files
!-----------------------------------------------------------------------

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
!       Open or close and re-open work and reads files  
!-----------------------------------------------------------------------

        INQUIRE (FILE = 'WORK.OUT',OPENED = FOPEN)
        ! Normally is closed at the end of the program.
        IF (.NOT.FOPEN) THEN
          IF (RUN.EQ.1) THEN
            OPEN (UNIT = FNUMWRK,FILE = 'WORK.OUT')
            WRITE(FNUMWRK,*) 'CSCRP  Cropsim Cereal Crop Module '
          ELSE
            OPEN (UNIT = FNUMWRK,FILE = 'WORK.OUT',POSITION='APPEND',
     &            ACTION = 'READWRITE')
            WRITE(fnumwrk,*) ' '
            WRITE(fnumwrk,*) 'CSCRP  Cropsim Cereal Crop Module '
            ! If do not wish to accumulate for all runs
            !IF (IDETL.EQ.'0'.OR.IDETL.EQ.'Y'.OR.IDETL.EQ.'N') THEN
            !  CLOSE (FNUMWRK)
            !  OPEN (UNIT = FNUMWRK,FILE = 'WORK.OUT')
            !  WRITE(fnumwrk,*) ' '
            !  WRITE(fnumwrk,*) 'CSCRP  Cropsim Cereal Crop Module '
            !ENDIF  
            IF (IDETL.EQ.'A') THEN
              CLOSE (FNUMWRK, STATUS = 'DELETE')
              OPEN (UNIT = FNUMWRK,FILE = 'WORK.OUT')
              WRITE(fnumwrk,*) ' '
              WRITE(fnumwrk,*) 'CSCRP  Cropsim Cereal Crop Module '
            ENDIF  
! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.            
!            IF (FNUMREA.LE.0) CALL Getlun('READS.OUT',fnumrea)
!            ! Close and re-open Reads file
!            CLOSE (FNUMREA, STATUS = 'DELETE')
!            OPEN (UNIT = FNUMREA,FILE = 'READS.OUT', STATUS = 'NEW',
!     &            ACTION = 'READWRITE')
!            WRITE(fnumrea,*)' '
!            WRITE(fnumrea,*)
!     &      ' File closed and re-opened to avoid generating huge file'
          ENDIF
        ELSE  ! File is open .. not closed at end of run!        
          IF (IDETL.EQ.'0'.OR.IDETL.EQ.'Y'.OR.IDETL.EQ.'N') THEN
            ! IDETL is headed VBOSE
            ! Close and re-open Work file. 
            CLOSE (FNUMWRK, STATUS = 'DELETE')
            OPEN (UNIT = FNUMWRK,FILE = 'WORK.OUT', STATUS = 'NEW',
     &            ACTION = 'READWRITE')
            WRITE(fnumwrk,*) ' '
            WRITE(fnumwrk,*) 'CSCRP  Cropsim Cereal Crop Module '
! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.            
!            CALL Getlun('READS.OUT',fnumrea)
!            ! Close and re-open Reads file
!            CLOSE (FNUMREA, STATUS = 'DELETE')
!            OPEN (UNIT = FNUMREA,FILE = 'READS.OUT', STATUS = 'NEW',
!     &            ACTION = 'READWRITE')
!            WRITE(fnumrea,*)' '
!            WRITE(fnumrea,*)
!     &      ' File closed and re-opened to avoid generating huge file'
          ELSE  
            WRITE(fnumwrk,*) ' '
            WRITE(fnumwrk,*) 'CSCRP  Cropsim Cereal Crop Module '
          ENDIF
        ENDIF  
          
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
        carbobegr = 0.0
        carbobegm = 0.0
        carboc = 0.0
        carboend = 0.0
        carbogf = 0.0
        carbolim = 0
        carbor = 0.0
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
        cnadprev = 0.0
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
        cumdulin = 0.0
        cumdus = 0.0
        cumtt = 0.0
        cumvd = 0.0
        cwaa = -99.0
        cwaam = -99.0
        cwad = 0.0
        cwadprev = 0.0
        cwadstg = 0.0
        cwahc = 0.0
        cwahcm = -99.0
        cwam = -99.0
        cwamm = -99.0
        cwan = -99.0
        dae = -99
        dap = -99
        daylcc = 0.0
        daylpav = -99.0
        daylpc = 0.0
        daylst = 0.0
        daysum = 0.0
        deadn = 0.0
        deadnad = 0.0
        deadwam = 0.0
        deadwt = 0.0
        deadwtm = 0.0
        sentopretained = 0.0
        sentopretaineda = 0.0
        sentoplitter = 0.0
        deadwtsge = 0.0
        dewdur = -99.0
        df = 1.0
        dfout = 1.0
        dglf = 0
        drainc = 0.0
        drdat = -99
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
        gnph = 0.0
        gnphc = 0.0
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
        hamt = 0.0
        hardays = 0.0
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
        idate1 = -99
        idetgnum = 0
        irramtc = 0.0
        lafswitch = -99.0
        lagedu = 0.0
        lagep = 0.0
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
        lapotchg = 0
        lapp = 0.0
        laps = 0.0
        latl = 0.0
        lcnum = 0
        lcoa = 0.0
        lcoas = 0.0
        leafn = 0.0
        lfgscumdu = 0.0
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
        lnphc = 0.0
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
        lsphs = -99
        lsphe = -99
        lshai = 0.0
        lshrswad = 0.0
        lshrswt = 0.0
        lshwad = 0.0
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
        rscd = 0.0
        rscm = 0.0
        rscx = 0.0
        rsfp = 1.0
        rsn = 0.0
        rsnph = 0.0
        rsnphc = 0.0
        rsnad = 0.0
        rsnused = 0.0
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
        rtwtgrs = 0.0
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
        sengf = 0.0
        senla = 0.0
        senlaretained = 0.0
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
        sennstg = 0.0
        sennstgrs = 0.0
        senrtg = 0.0
        senrtggf = 0.0
        senstg = 0.0
        senstgrs = 0.0
        sentoplitterg = 0.0
        sentoplitterggf = 0.0
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
        sncr = 0.0
        snow = 0.0
        sno3profile = 0.0
        sno3profile = 0.0
        sno3rootzone = 0.0
        snh4rootzone = 0.0
        snph = 0.0
        snphc = 0.0
        snuse = 0.0
        spnumhc = 0.0
        spnumhcm = -99.0
        srad20anth = -99.0
        sradc = 0.0
        sradcav  = -99.0
        sradcc = 0.0
        sradd = 0.0
        sradpav  = -99.0
        sradpc = 0.0
        sradprev = 0.0
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
   !     tfla = 1.0
        tflf = 0.0
        tfp = 1.0
        tilbirthl = 0.0
        tildat = 0  
        tkill = -99.0 
        tla = 0.0
        tlas = 0.0
        tlchc = 0.0
        tlimit = 0
        tmaxcav  = -99.0
        tmaxcc = 0.0
        tmaxgfill = 0.0
        tmaxm = -99.0
        tmaxpav  = -99.0
        tmaxpc = 0.0
        tmaxsum = 0.0
        tmaxx = -99.0
        tmean = -99.0
        tmean20anth = -99.0
        tmeanpav = -99.0
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
   !     ttla = 0.0
        ttnext = 0.0
        tt20 = -99.0
        ttgem = 0.0
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
        wfppav = 1.0
        wfpcc = 0.0
        wfppc = 0.0
        wft = 1.0
        wsdays = 0
        wupr = 1.0
        zstage = 0.0
        
        nupwf = -99.0
        nupnf = -99.0
        nupcf = -99.0
        rtnup = -99.0
        no3mn = -99.0
        nh4mn = -99.0
        
!-----------------------------------------------------------------------
!       Read experiment information from Dssat input or X- file
!-----------------------------------------------------------------------

        ! Methods
        CALL XREADC(FILEIO,TN,RN,SN,ON,CN,'PHOTO',mephs)
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
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'IDATE',idate1)
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
        
            ! LAH Following inserted to allow examination of grazing
            IF (EXCODE.EQ.'KSAS8101WH'.AND.TN.EQ.1) THEN
              tvi1 = 0
            ELSE
              tvi1 = 0
            ENDIF  
            IF (tvi1.EQ.1) THEN
              HYRDOY(1) = 82100
              HOP(1) = 'G'
              HAMT(1) = 500.0
              CWAN(1) = 100.0
              HPC(1) = -99.0
              HBPC(1) = -99.0
              HYRDOY(2) = 82130
              HOP(2) = 'G'
              HAMT(2) = 900.0
              CWAN(2) = 100.0
              HPC(2) = -99.0
              HBPC(2) = -99.0
              HYRDOY(3) = 82180
              HOP(3) = 'F'
              HAMT(3) = -99.0
              CWAN(3) = -99.0
              HPC(3) = 100.0
              HBPC(3) = 100.0
            ENDIF
        
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
          CALL XREADC(FILEX,TN,RN,SN,ON,CN,'PHASE',cflphaseadj)
          IF (CFLPHASEADJ.NE.'N'.AND.CFLPHASEADJ.NE.'Y') THEN
           CFLPHASEADJ = 'Y'  ! Default to adjustment 
          ENDIF   
          CALL XREADC(FILEX,TN,RN,SN,ON,CN,'PHINT',cflphintadj)
          IF (CFLPHINTADJ.NE.'N'.AND.CFLPHINTADJ.NE.'Y') THEN
           CFLPHINTADJ = 'N'   ! Default to no adjustment
          ENDIF   
        ENDIF

!-----------------------------------------------------------------------
!       Correct case and dates
!-----------------------------------------------------------------------

        CALL CSUCASE (CROP)
        GENFLCHK = CROP//GENFLCHK(3:15)
        CALL CSUCASE (EXCODE)

C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY
        !HLAST = CSYEARDOY(hlast)
        CALL Y4K_DOY(hlast,FILEX,0,ERRKEY,3)
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
        IF (spnumhfac.LE.0.0) spnumhfac = 0.1

!-----------------------------------------------------------------------
!       Set planting/harvesting dates (Will change if runs repeated)
!-----------------------------------------------------------------------

        ! CHP 5/4/09 - for DSSAT runs, always set PLYEAR = YEAR
        ! CHP 09/28/2009 account for planting date >> simulation date.
        !LPM 07/17/20 - account for simulation date when is a year before planting date
        !Avoid wrong value of yeardoyharf
        IF (FILEIOT(1:2) == 'DS' .AND. YEAR > PLYEAR) THEN
            IF (YEAR < PLYEARREAD) THEN
                PLYEAR = PLYEARREAD
                PLYEARTMP = PLYEARREAD
            ELSE
                PLYEAR = YEAR
                PLYEARTMP = YEAR
            ENDIF
        ENDIF

        IF (IDATE1.GT.0.AND.CFLPDATE.EQ.'I') THEN
          WRITE(FNUMWRK,*)' '
          WRITE(FNUMWRK,*)
     &     ' Planting date set at time of first irrigation'
          WRITE(FNUMWRK,*)'  Planting date    : ',pdate
          WRITE(FNUMWRK,*)'  Irrigation 1 date: ',idate1
          PDATE = IDATE1
        ENDIF

        ! Check final harvest date for seasonal runs        
!        CALL CSYR_DOY(YEARDOYHARF,HYEAR,HDAY)
        CALL YR_DOY(YEARDOYHARF,HYEAR,HDAY)
        PLTOHARYR = HYEAR - PLYEARREAD
        ! Upgrade harvest date for seasonal and sequential runs
        yeardoyharf = (plyear+pltoharyr)*1000 +hday

!       IF (IPLTI.NE.'A') THEN
        IF (IPLTI.NE.'A' .AND. IPLTI.NE.'F') THEN
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

        INQUIRE (FILE = ECDIRFLE,EXIST = FFLAGEC)
        IF (.NOT.(FFLAGEC)) THEN
          ! Following added Sept 2008 for running under VB environment.
          WRITE(fnumwrk,*) ' '
          WRITE(fnumwrk,*) 'Ecotype file not found!     '
          WRITE(fnumwrk,*) 'File sought was: ',Ecdirfle(1:60)  
          ECDIRFLE = ECFILE
          WRITE(fnumwrk,*) 
     &     'Will search in the working directory for:',Ecdirfle(1:60)
          INQUIRE (FILE = ECDIRFLE,EXIST = FFLAGEC)
          IF (.NOT.(FFLAGEC)) THEN
            OPEN (UNIT = FNUMERR,FILE = 'ERROR.OUT')
            WRITE(fnumwrk,*) 'File not found in working directory!'
            WRITE(fnumwrk,*) 'Please check'
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
!       Read cultivar information
!-----------------------------------------------------------------------

        ! Ecotype coefficients re-set
        gmpch = -99
        htstd = -99
        pps = -99
        awns = -99
        rspca = -99
        ti1lf = -99
        tifac = -99
        tilpe = -99
        tdphs = -99
        tdphe = -99
        gnpcs = -99
        tkfh = -99
        lsphs = -99
        lsphe = -99
        ssphs = -99
        ssphe = -99
        lseni = -99
        phintl = -99
        phintf = -99
        gnorf = -99
        gnort = -99
        gwtaf = -99
        gwtat = -99
        parue = -99
        paru2 = -99
        la1s = -99
        laxs = -99
        laws = -99
        lafv = -99
        lafr = -99
        rdgs = -99
        tdfac = -99
        tdsf = -99
        nfgu = -99
        nfgl = -99
        nupcf = -99
        nupnf = -99
        nupwf = -99
        rtnup = -99
        lsphs = -99
        lsphe = -99

        ! Species coefficients re-set
        pd = -99
        lsphe = -99 
        tdphe = -99 
        tdphs = -99 
        tilpe = -99 
        llifa = -99 
        phintl = -99 
        phintf = -99 
        tdfac = -99 
        rdgs = -99 
        laxs = -99 
        rlwr = -99 
        nfgl = -99 
        nfgu = -99 
        nfpu = -99 
        nfpl = -99 
        kcan = -99 
        lafr = -99 
        lafv = -99 
        lawcf = -99
        pgerm = -99
        pemrg = -99 
        ppfpe = -99 
        ppexp = -99 
        tdsf = -99 
        gwtaf = -99
        gwtat = -99
        gnorf = -99 
        gnort = -99 
        nh4mn = -99
        no3mn = -99
        
        IF (FILEIOT(1:2).EQ.'DS') THEN
          IF (RNMODE.NE.'T') CALL FVCHECK(CUDIRFLE,GENFLCHK)

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
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS1',pps(1))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS2',pps(2))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS3',pps(3))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS4',pps(4))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS5',pps(5))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS6',pps(6))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS7',pps(7))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS8',pps(8))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS9',pps(9))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPEXP',ppexp) ! Trial
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPFPE',ppfpe)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'G#WTS',gnowts)
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
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LAXS',laxs)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'SLAS',laws)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFPU',nfpu)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFPL',nfpl)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFGU',nfgu)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFGL',nfgl)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'RDGS',rdgs)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'RLWR',rlwr)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PARUE',parue)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PARU2',paru2)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'TDFAC',tdfac)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'TDSF',tdsf)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'GWTAT',gwtat)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'GWTAF',gwtaf)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'G#RF',gnorf)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'G#RT',gnort)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LA1S',la1s)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LAFV',lafv)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LAFR',lafr)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PHL2',phintl(2))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PHF3',phintf(3))
          ! New (Nov 2011) N uptake variables
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NUPNF',nupnf)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NUPWF',nupwf)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'RTNUP',rtnup)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LSPHS',lsphs)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LSPHE',lsphe)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NO3MN',no3mn)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NH4MN',nh4mn)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PGERM',pgerm)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PEMRG',pemrg)
        ELSE
          IF (RNMODE.NE.'T') CALL FVCHECK(CUDIRFLE,GENFLCHK)
          CALL CUREADC (CUDIRFLE,VARNO,'ECO#',econo)
          CALL CUREADR (CUDIRFLE,VARNO,'VREQ',vreq)
          IF (VREQ.LT.0.0)CALL CUREADR (CUDIRFLE,VARNO,'VREQX',vreq)
          CALL CUREADR (CUDIRFLE,VARNO,'VBASE',vbase)
          IF (VBASE.LT.0.0)CALL CUREADR (CUDIRFLE,VARNO,'VREQN',vbase)
          CALL CUREADR (CUDIRFLE,VARNO,'VEFF',veff)
          IF (VEFF.LT.0.0)CALL CUREADR (CUDIRFLE,VARNO,'VEFFX',veff)
          CALL CUREADR (CUDIRFLE,VARNO,'PPS1',pps(1))
          CALL CUREADR (CUDIRFLE,VARNO,'PPS2',pps(2))
          CALL CUREADR (CUDIRFLE,VARNO,'PPS3',pps(3))
          CALL CUREADR (CUDIRFLE,VARNO,'PPS4',pps(4))
          CALL CUREADR (CUDIRFLE,VARNO,'PPS5',pps(5))
          CALL CUREADR (CUDIRFLE,VARNO,'PPS6',pps(6))
          CALL CUREADR (CUDIRFLE,VARNO,'PPS7',pps(7))
          CALL CUREADR (CUDIRFLE,VARNO,'PPS8',pps(8))
          CALL CUREADR (CUDIRFLE,VARNO,'PPS9',pps(9))
          CALL CUREADR (CUDIRFLE,VARNO,'PPEXP',ppexp)! Trial
          CALL CUREADR (CUDIRFLE,VARNO,'PPFPE',ppfpe)
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
          CALL CUREADR (CUDIRFLE,VARNO,'G#WTS',gnowts)
          CALL CUREADR (CUDIRFLE,VARNO,'GWTS',gwts)
          CALL CUREADR (CUDIRFLE,VARNO,'SHWTS',g3)
          CALL CUREADR (CUDIRFLE,VARNO,'PHINT',phints)
          CALL CUREADR (CUDIRFLE,VARNO,'LLIFA',llifa)
          CALL CUREADR (CUDIRFLE,VARNO,'STFR',swfrs)
          CALL CUREADR (CUDIRFLE,VARNO,'LAXS',laxs)
          CALL CUREADR (CUDIRFLE,VARNO,'SLAS',laws)
          CALL CUREADR (CUDIRFLE,VARNO,'NFPU',nfpu)
          CALL CUREADR (CUDIRFLE,VARNO,'NFPL',nfpl)
          CALL CUREADR (CUDIRFLE,VARNO,'NFGU',nfgu)
          CALL CUREADR (CUDIRFLE,VARNO,'NFGL',nfgl)
          CALL CUREADR (CUDIRFLE,VARNO,'RDGS',rdgs)
          CALL CUREADR (CUDIRFLE,VARNO,'RLWR',rlwr)
          CALL CUREADR (CUDIRFLE,VARNO,'PARUE',parue)
          CALL CUREADR (CUDIRFLE,VARNO,'PARU2',paru2)
          CALL CUREADR (CUDIRFLE,VARNO,'TDFAC',tdfac)
          CALL CUREADR (CUDIRFLE,VARNO,'TDSF',tdsf)
          CALL CUREADR (CUDIRFLE,VARNO,'GWTAT',gwtat)
          CALL CUREADR (CUDIRFLE,VARNO,'GWTAF',gwtaf)
          CALL CUREADR (CUDIRFLE,VARNO,'G#RF',gnorf)
          CALL CUREADR (CUDIRFLE,VARNO,'G#RT',gnort)
          CALL CUREADR (CUDIRFLE,VARNO,'LA1S',la1s)
          CALL CUREADR (CUDIRFLE,VARNO,'LAFV',lafv)
          CALL CUREADR (CUDIRFLE,VARNO,'LAFR',lafr)
          CALL CUREADR (CUDIRFLE,VARNO,'PHL2',phintl(2))
          CALL CUREADR (CUDIRFLE,VARNO,'PHF3',phintf(3))
          CALL CUREADR (CUDIRFLE,VARNO,'LSHPS',lsphs)
          CALL CUREADR (CUDIRFLE,VARNO,'LSPHE',lsphe)
          ! New (Nov 2011) N uptake variables
          CALL CUREADR (CUDIRFLE,VARNO,'NUPNF',nupnf)
          CALL CUREADR (CUDIRFLE,VARNO,'NUPWF',nupwf)
          CALL CUREADR (CUDIRFLE,VARNO,'RTNUP',rtnup)
          CALL CUREADR (CUDIRFLE,VARNO,'NO3MN',no3mn)
          CALL CUREADR (CUDIRFLE,VARNO,'NH4MN',nh4mn)
          CALL CUREADR (CUDIRFLE,VARNO,'PGERM',pgerm)
          CALL CUREADR (CUDIRFLE,VARNO,'PEMRG',pemrg)
        ENDIF     ! End Cultivar reads

!-----------------------------------------------------------------------
!       Read ecotype information
!-----------------------------------------------------------------------

        IF (RNMODE.NE.'T') CALL FVCHECK(ECDIRFLE,GENFLCHK)
        CALL ECREADR (ECDIRFLE,ECONO,'HTSTD',htstd)
        CALL ECREADR (ECDIRFLE,ECONO,'AWNS',awns)
        CALL ECREADR (ECDIRFLE,ECONO,'RS%A',rspca)
        CALL ECREADR (ECDIRFLE,ECONO,'TIL#S',ti1lf)
        IF (TI1LF.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'TIPHS',ti1lf)
        IF (PPS(2).LT.0.0) 
     &   CALL ECREADR (ECDIRFLE,ECONO,'PPS2',pps(2))
        CALL ECREADR (ECDIRFLE,ECONO,'TIFAC',tifac)
        CALL ECREADR (ECDIRFLE,ECONO,'TILPE',tilpe)
        CALL ECREADR (ECDIRFLE,ECONO,'TDPHS',tdphs)
        CALL ECREADR (ECDIRFLE,ECONO,'TDPHE',tdphe)
        CALL ECREADR (ECDIRFLE,ECONO,'GN%MN',gnpcmn)
        CALL ECREADR (ECDIRFLE,ECONO,'GN%S',gnpcs)
        CALL ECREADR (ECDIRFLE,ECONO,'TKFH',tkfh)
        IF (LSPHS.LE.0) CALL ECREADR (ECDIRFLE,ECONO,'LSPHS',lsphs)
        IF (LSPHE.LE.0) CALL ECREADR (ECDIRFLE,ECONO,'LSPHE',lsphe)
        CALL ECREADR (ECDIRFLE,ECONO,'SSPHS',ssphs)
        CALL ECREADR (ECDIRFLE,ECONO,'SSPHE',ssphe)
        CALL ECREADR (ECDIRFLE,ECONO,'LSENI',lseni)
        IF (PHINTL(1).LE.0) 
     &   CALL ECREADR(ECDIRFLE,ECONO,'PHL1',phintl(1))
        IF (PHINTL(2).LE.0) 
     &   CALL ECREADR(ECDIRFLE,ECONO,'PHL2',phintl(2))
        IF (PHINTF(2).LE.0)
     &   CALL ECREADR(ECDIRFLE,ECONO,'PHF2',phintf(2))
        IF (PHINTF(3).LE.0)
     &   CALL ECREADR(ECDIRFLE,ECONO,'PHF3',phintf(3))
        ! LAH Following set up to allow for change in stem fraction
        ! Currently not used ... just one stem fraction (STFR)
        CALL ECREADR (ECDIRFLE,ECONO,'SWFRX',swfrx)
        CALL ECREADR (ECDIRFLE,ECONO,'SWFRN',swfrn)
        CALL ECREADR (ECDIRFLE,ECONO,'SWFNL',swfrnl)
        CALL ECREADR (ECDIRFLE,ECONO,'SWFXL',swfrxl)
        CALL ECREADR (ECDIRFLE,ECONO,'SLACF',lawcf)
        CALL ECREADR (ECDIRFLE,ECONO,'KCAN',kcan)
        ! Following may have been (temporarily) in the CUL file
        ! Grains
        IF (GMPCH.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'GM%H',gmpch)
        IF (GNORF.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'G#RF',gnorf)
        IF (GNORT.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'G#RT',gnort)
        IF (GWTAT.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'GWTAT',gwtat)
        IF (GWTAF.LT.-10.0) CALL ECREADR (ECDIRFLE,ECONO,'GWTAF',gwtaf)
        ! Radiation use efficiency
        IF (PARUE.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'PARUE',parue)
        IF (PARU2.LT.-89.0) CALL ECREADR (ECDIRFLE,ECONO,'PARU2',paru2)
        ! Leaf area
        IF (LA1S.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'LA1S',la1s)
        IF (LAXS.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'LAXS',laxs)
        IF (LAWS.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'SLAS',laws)
        IF (LAFV.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'LAFV',lafv)
        IF (LAFR.LT.-90.0) CALL ECREADR (ECDIRFLE,ECONO,'LAFR',lafr)
        ! Roots
        IF (RDGS.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'RDGS',rdgs)
        ! Tillers
        IF (TDFAC.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'TDFAC',tdfac)
        IF (TDSF.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'TDSF',tdsf)
        ! Reduction factors
        IF (NFGU.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'NFGU',nfgu)
        IF (NFGL.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'NFGL',nfgl)
        ! N uptake
        IF (NUPNF.LT.-90.0) CALL ECREADR (ECDIRFLE,ECONO,'NUPNF',nupnf)
        IF (NUPWF.LT.-90.0) CALL ECREADR (ECDIRFLE,ECONO,'NUPWF',nupwf)
        IF (NUPCF.LT.-90.0) CALL ECREADR (ECDIRFLE,ECONO,'NUPCF',nupcf)
        IF (RTNUP.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'RTNUP',rtnup)
        IF (NO3MN.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'NO3MN',no3mn)
        IF (NH4MN.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'NH4MN',nh4mn)
        
!-----------------------------------------------------------------------
!       Read species information
!-----------------------------------------------------------------------

        IF (RNMODE.NE.'T') CALL FVCHECK(SPDIRFLE,GENFLCHK)
        CALL SPREADR (SPDIRFLE,'CHFR' ,chfr)
        CALL SPREADR (SPDIRFLE,'CO2CC',co2compc)
        CALL SPREADR (SPDIRFLE,'CO2EX',co2ex)
        CALL SPREADR (SPDIRFLE,'GLIG%',gligpc)
        CALL SPREADR (SPDIRFLE,'GN%MX',gnpcmx)
        CALL SPREADR (SPDIRFLE,'GWLAG',gwlagfr)
        CALL SPREADR (SPDIRFLE,'GWLIN',gwlinfr)
        CALL SPREADR (SPDIRFLE,'HDUR' ,hdur)
        CALL SPREADR (SPDIRFLE,'HLOSF',hlosf)
        CALL SPREADR (SPDIRFLE,'HLOST',hlost)
        CALL SPREADR (SPDIRFLE,'LAFST',lafst)
        CALL SPREADR (SPDIRFLE,'SLAFF',lawff)
        CALL SPREADR (SPDIRFLE,'SLATR',lawtr)
        CALL SPREADR (SPDIRFLE,'SLATS',lawts)
        CALL SPREADR (SPDIRFLE,'SLAWR',lawwr)
        CALL SPREADR (SPDIRFLE,'LLIFG',LLIFG)
        CALL SPREADR (SPDIRFLE,'LLIFS',llifs)
        CALL SPREADR (SPDIRFLE,'LLIG%',lligpc)
        CALL SPREADR (SPDIRFLE,'LLOSA',llosa)
        CALL SPREADR (SPDIRFLE,'LWLOS',lwlos)
        CALL SPREADR (SPDIRFLE,'NFSU' ,nfsu)
        CALL SPREADR (SPDIRFLE,'NFSF' ,nfsf)
        CALL SPREADR (SPDIRFLE,'NFTL ',nftl)
        CALL SPREADR (SPDIRFLE,'NFTU ',nftu)
        CALL SPREADR (SPDIRFLE,'LSHAR',lshar)
        CALL SPREADR (SPDIRFLE,'LSHAV',lshav)
        CALL SPREADR (SPDIRFLE,'LSHFR',lshfr)
        CALL SPREADR (SPDIRFLE,'NCRG',ncrg)
        CALL SPREADR (SPDIRFLE,'NTUPF',ntupf)
        CALL SPREADR (SPDIRFLE,'PARIX',parix)
        CALL SPREADR (SPDIRFLE,'LAIXX',laixx)
        CALL SPREADR (SPDIRFLE,'PARFC',parfc)
        IF (PEMRG.LE.0.0) CALL SPREADR (SPDIRFLE,'PEMRG',pemrg)
        IF (PGERM.LE.0.0) CALL SPREADR (SPDIRFLE,'PGERM',pgerm)
        CALL SPREADR (SPDIRFLE,'PDMH' ,pdmtohar)
        CALL SPREADR (SPDIRFLE,'PHSV' ,phsv)
        CALL SPREADR (SPDIRFLE,'PHTV' ,phtv)
        CALL SPREADR (SPDIRFLE,'PPTHR',ppthr)
        CALL SPREADR (SPDIRFLE,'PTFXS',ptfxs)
        CALL SPREADR (SPDIRFLE,'PTFA' ,ptfa)
        CALL SPREADR (SPDIRFLE,'PTFMN',ptfmn)
        CALL SPREADR (SPDIRFLE,'PTFMX',ptfmx)
        CALL SPREADR (SPDIRFLE,'RATM' ,ratm)
        CALL SPREADR (SPDIRFLE,'RCROP',rcrop)
        CALL SPREADR (SPDIRFLE,'EORAT',eoratio)
        CALL SPREADR (SPDIRFLE,'RDGAF',rdgaf)
        CALL SPREADR (SPDIRFLE,'RLIG%',rligpc)
        !CALL SPREADR (SPDIRFLE,'RNUMX',rnumx) ! Taken out of spp file
        CALL SPREADR (SPDIRFLE,'RRESP',rresp)
        CALL SPREADR (SPDIRFLE,'RS%LX',rspclx)
        CALL SPREADR (SPDIRFLE,'RS%X' ,rspcx)
        CALL SPREADR (SPDIRFLE,'RSEN',rsen)
        IF (RSEN.LT.0.0) CALL SPREADR (SPDIRFLE,'RSEN%',rsen)
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
        CALL SPREADR (SPDIRFLE,'SDRS%',sdrspc)
        CALL SPREADR (SPDIRFLE,'SDWT' ,sdwt)
        CALL SPREADR (SPDIRFLE,'SLIG%',sligpc)
        CALL SPREADR (SPDIRFLE,'TGR02',tgr(2))
        CALL SPREADR (SPDIRFLE,'TGR20',tgr(20))
        CALL SPREADR (SPDIRFLE,'TILIP',tilip)
        CALL SPREADR (SPDIRFLE,'TIL#X',tilnox)
        CALL SPREADR (SPDIRFLE,'TKDLF',tkdlf)
        CALL SPREADR (SPDIRFLE,'TKSPN',tkspn)
        CALL SPREADR (SPDIRFLE,'TKDTI',tkdti)
        CALL SPREADR (SPDIRFLE,'TKUH' ,tkuh)
        CALL SPREADR (SPDIRFLE,'TKGF' ,tkgf)
        CALL SPREADR (SPDIRFLE,'TPAR' ,tpar)
        CALL SPREADR (SPDIRFLE,'TSRAD',tsrad)
        CALL SPREADR (SPDIRFLE,'VEEND',veend)
        CALL SPREADR (SPDIRFLE,'VLOSF',vlosf)
        CALL SPREADR (SPDIRFLE,'VLOSS',vloss)
        CALL SPREADR (SPDIRFLE,'VLOST',vlost)
        CALL SPREADR (SPDIRFLE,'VPEND',vpend)
        CALL SPREADR (SPDIRFLE,'WFEU' ,wfeu)
        CALL SPREADR (SPDIRFLE,'WFGEU',wfgeu)
        CALL SPREADR (SPDIRFLE,'WFGU' ,wfgu)
        CALL SPREADR (SPDIRFLE,'WFGL' ,wfgl)
        CALL SPREADR (SPDIRFLE,'WFPU' ,wfpu)
        CALL SPREADR (SPDIRFLE,'WFPL' ,wfpl)
        CALL SPREADR (SPDIRFLE,'WFRGU',wfrgu)
        CALL SPREADR (SPDIRFLE,'WFSU' ,wfsu)
        CALL SPREADR (SPDIRFLE,'WFSF' ,wfsf)
        CALL SPREADR (SPDIRFLE,'WFTL' ,wftl)
        CALL SPREADR (SPDIRFLE,'WFTU' ,wftu)
        CALL SPREADR (SPDIRFLE,'NLAB%',nlabpc)
        ! LAH Following set up to allow for change in stem fraction
        ! Currently not used ... just one stem fraction (STFR)
        IF (SWFRN.LE.0.0) CALL SPREADR (SPDIRFLE,'SWFRN',swfrn)
        IF (SWFRNL.LE.0.0) CALL SPREADR (SPDIRFLE,'SWFRNL',swfrnl)
        IF (SWFRXL.LE.0.0) CALL SPREADR (SPDIRFLE,'SWFXL',swfrxl)
        IF (SWFRX.LE.0.0) CALL SPREADR (SPDIRFLE,'SWFRX',swfrx)
        ! Following may be temporarily in ECO or CUL file
        IF (PD(9).LE.0.0) CALL SPREADR (SPDIRFLE,'P9',pd(9))
        IF (lsphe.LE.0.0) CALL SPREADR (SPDIRFLE,'LSPHE',lsphe)
        IF (tdphe.LE.0.0) CALL SPREADR (SPDIRFLE,'TDPHE',tdphe)
        IF (tdphs.LE.0.0) CALL SPREADR (SPDIRFLE,'TDPHS',tdphs)
        IF (tilpe.LE.0.0) CALL SPREADR (SPDIRFLE,'TILPE',tilpe)
        IF (LLIFA.LE.0.0) CALL SPREADR (SPDIRFLE,'LLIFA',llifa)

        IF (PHINTL(1).LE.0) CALL SPREADR (SPDIRFLE,'PHL1',phintl(1))
        IF (PHINTF(1).LE.0) CALL SPREADR (SPDIRFLE,'PHF1',phintf(1))
        IF (PHINTL(2).LE.0) CALL SPREADR (SPDIRFLE,'PHL2',phintl(2))
        IF (PHINTF(2).LE.0) CALL SPREADR (SPDIRFLE,'PHF2',phintf(2))
        IF (PHINTF(3).LE.0) CALL SPREADR (SPDIRFLE,'PHF3',phintf(3))

        IF (TDFAC.LE.0.0) CALL SPREADR (SPDIRFLE,'TDFAC',tdfac)
        IF (RDGS.LE.0.0) CALL SPREADR (SPDIRFLE,'RDGS',rdgs)
        IF (LAXS.LE.0.0) CALL SPREADR (SPDIRFLE,'LAXS',laxs)
        IF (RLWR.LE.0.0) CALL SPREADR (SPDIRFLE,'RLWR',rlwr)
        IF (NFGL.LT.0.0) CALL SPREADR (SPDIRFLE,'NFGL',nfgl)
        IF (NFGU.LE.0.0) CALL SPREADR (SPDIRFLE,'NFGU',nfgu)
        IF (NFPU.LE.0.0) CALL SPREADR (SPDIRFLE,'NFPU',nfpu)
        IF (NFPL.LE.0.0) CALL SPREADR (SPDIRFLE,'NFPL',nfpl)
        IF (KCAN.LE.0.0) CALL SPREADR (SPDIRFLE,'KCAN',kcan)
        IF (LAFR.LE.-90.0) CALL SPREADR (SPDIRFLE,'LAFR',lafr)
        IF (LAFV.LE.0.0) CALL SPREADR (SPDIRFLE,'LAFV',lafv)
        IF (LAWCF.LE.0.0) CALL SPREADR (SPDIRFLE,'SLACF',lawcf)
        IF (PPFPE.LT.0.0) CALL SPREADR (SPDIRFLE,'PPFPE',ppfpe)
        IF (PPEXP.LT.0.0) CALL SPREADR (SPDIRFLE,'PPEXP',ppexp)
        IF (TDSF.LT.0.0) CALL SPREADR (SPDIRFLE,'TDSF',tdsf)
        ! Grain coefficients
        IF (GWTAF.LT.-10.0) CALL SPREADR (SPDIRFLE,'GWTAF',gwtaf)
        IF (GWTAT.LT.0.0) CALL SPREADR (SPDIRFLE,'GWTAT',gwtat)
        IF (GNORF.LT.0.0) CALL SPREADR (SPDIRFLE,'G#RF',gnorf)
        IF (GNORT.LT.0.0) CALL SPREADR (SPDIRFLE,'G#RT',gnort)
        ! N uptake 
        IF (NUPNF.LT.-90.0) CALL SPREADR (SPDIRFLE,'NUPNF',nupnf)
        IF (NUPWF.LT.-90.0) CALL SPREADR (SPDIRFLE,'NUPWF',nupwf)
        IF (RTNUP.LT.0.0) CALL SPREADR (SPDIRFLE,'RTNUP',rtnup)
        IF (NO3MN.LT.0.0) CALL SPREADR (SPDIRFLE,'NO3MN',no3mn)
        IF (NH4MN.LT.0.0) CALL SPREADR (SPDIRFLE,'NH4MN',nh4mn)

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
        !RRATE TRGEM TRDV1 TRDV4 TRDV8 TRLFG TRPHS TRVRN TRHAR TRGFW TRGFN  
        !    0     1     0     0     0     0     0    -5    -5     0     0  
        !  1.0    26    26    26    30    10     5     0     0    16    16  
        !  1.0    50    50    50    50    20    25     7     5    35    35
        !    0    60    60    60    60    35    35    15    10    45    45
        CALL SPREADRA (SPDIRFLE,'TRDV1','4',trdv1)
        IF (trdv1(1).LT.-98.0) THEN
          OPEN (UNIT = FNUMERR,FILE = 'ERROR.OUT')
          WRITE(fnumerr,*) ' '
          WRITE(fnumerr,*) ' No temp response data for phase 1'
          WRITE(fnumerr,*) ' Please check'
          WRITE(*,*) ' No temperature response data for phase 1'
          WRITE(*,*) ' Program will have to stop'
          CLOSE (fnumerr)
          STOP ' '
        ENDIF        
        CALL SPREADRA (SPDIRFLE,'TRDV2','4',trdv2)
        IF (trdv2(1).LT.-98.0) TRDV2 = TRDV1
        CALL SPREADRA (SPDIRFLE,'TRDV3','4',trdv3)
        IF (trdv3(1).LT.-98.0) TRDV3 = TRDV2
        CALL SPREADRA (SPDIRFLE,'TRDV4','4',trdv4)
        IF (trdv4(1).LT.-98.0) TRDV4 = TRDV3
        CALL SPREADRA (SPDIRFLE,'TRDV5','4',trdv5)
        IF (trdv5(1).LT.-98.0) TRDV5 = TRDV4
        CALL SPREADRA (SPDIRFLE,'TRDV6','4',trdv6)
        IF (trdv6(1).LT.-98.0) TRDV6 = TRDV5
        CALL SPREADRA (SPDIRFLE,'TRDV7','4',trdv7)
        IF (trdv7(1).LT.-98.0) TRDV7 = TRDV6
        CALL SPREADRA (SPDIRFLE,'TRDV8','4',trdv8)
        IF (trdv8(1).LT.-98.0) TRDV8 = TRDV7

        CALL SPREADRA (SPDIRFLE,'TRGEM','4',trgem)
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

        ! The following are to allow examination of the functioning of 
        ! different parts of the module, and comparison with CSCER     
        
        ! Commented acronyms are those used in the genotype files
       
        ! Set which sections to be examined
        EXAMINE(1)  = 'N'    ! Basic leaf and root growth
        EXAMINE(2)  = 'N'    ! Leaf senescencs
        EXAMINE(3)  = 'N'    ! Tillering
        EXAMINE(4)  = 'N'    ! Reproductive development
        EXAMINE(20) = 'N'    ! Everything from all genotype files
        
        IF (EXAMINE(1).EQ.'Y') THEN                                     

          WRITE(fnumwrk,*)' '
          WRITE(fnumwrk,*)' RUNNING EXAMINE '

              ! CSCRP
              PGERM = 10.0      
              PEMRG =  8.0      

              Pd(0) = 0.0       
              PARUE = 2.7       
              PARU2 = 2.7       
              KCAN = 0.85       
              PTFMN = 0.75                                              
              PTFMX = 0.98      
              PTFA = 0.00                                              
              PTFMX = 0.75 ! From 0.98
              PTFXS = 6.0 
          
          
          
          
          
          
          

              PPFPE = 1.0   
              PPS(1)= 97.0
              VREQ = 10000.0
              VBASE = 0.0                                               
              VEFF = 1.0    
              TI1LF = 200.0 
              LLIFA = 200.0 
              LLIFG = 1.0                                              
              LLIFS = 3.0                                              
              TKDLF = 2.0    
              TKUH = -50.0  
              TKFH = -90.0  
              TKSPN = 2.0                                              
              HDUR = 10.0   
              HLOST = 50.0                                             
              HLOSF = 0.2                                               
          
          
          
          
          
          
          
          
          
          LA1S = 5.0        
          LAFV = 0.1        
          LAFR = 0.5        
          PHINT = 95.0
          PHINTL(1) =2.0!PHL
          PHINTF(1) =0.8!PHF
          PHINTL(2) = 150    
          PHINTF(3) = 1.8   
          LAWS = 400        
          LAWCF = 0.02      
          
          LSHFR = 0.33      
          
          LSHAV =50.0    
          LSHAR = 50.0                                         
          RSPCX = 80.0
          RSPCA = 15.0      

          RRESP = 0.40 ! From 0.8
          RSEN = 0.1
          RLWR = 0.90 

          SDDUR = 5.0

          RTNUP = .006      
          NUPCF = 0.0          
          NUPNF = 0.0          
          NUPWF = 0.0       
          NH4MN = 0.5  
          NO3MN = 0.5  
          NTUPF = 0.05 
          LNPCS(0) = 6.5                             
          LNPCS(1) = 6.5                             
          SNPCS(0) = 1.5                              
          SNPCS(1) = 1.5                            
          RNPCS(0) = 2.0                             
          RNPCS(1) = 2.0                             
          LNPCMN(0) = 0.8                            
          LNPCMN(1) = 0.8                            
          SNPCMN(0) = 0.5                            
          SNPCMN(1) = 0.5                            
          RNPCMN(0) = 1.5                            
          RNPCMN(1) = 1.5                            
          
        ENDIF    ! EXAMINE(1)  Basic growth
        
        IF (EXAMINE(2).EQ.'Y') THEN                                    
          ! Leaf senescence 
          LLIFA = 4.0     
          LLIFG = 1.0
          LLIFS = 3.0
          LWLOS = 0.5        
          ! LNPCMN  Determines N loss
       ENDIF  ! EXAMINE = 2  Senescence
        
        IF (EXAMINE(3).EQ.'Y') THEN                                    
          ! Tillering        
          TI1LF = 3.5 ! 4.45  
          TIFAC = 1.0   
          TDPHS = 3.0   
          TDPHE = 7.0   
          TDFAC = 5.0   
          TGR(2) = 0.80
          TGR(20) = 0.10                                                
          TILIP = 6.0                                                  
          TILNOX = 20                                          
          TDSF = 1.0                                                   
        ENDIF    ! EXAMINE(3)  Tillering

        IF (EXAMINE(4).EQ.'Y') THEN                                    
          ! Reproductive development
          ! CSCRP           CSCER
          VREQ = 8.0            !  P1V =    8.0           
          PPS(1) = 97.0         !  P1D =     97.0         
          PD(8) = 500.0         !  PD(5) =   500          
          PD(1) = 200.0 !380    !  PD(1) = 200            
          PD(2) = 50.0  ! 70    !  PD(2) = 200            
          PD(3) = 150.0 !200    !  PD(3) = 200            
          PD(4) = 200.0 !200    !  PD(4) = 200            
                        !       !  PD2FR(1) = 0.25        
                        !       !  PD4FR(1) = 0.25        
                        !       !  PD4FR(2) = 0.10        
          PD(5) = 50.0  ! 60                                            
          PD(6) = 20.0  ! 25                                            
          PD(7) = 130.0 !150
          
          ! PHASE DURATIONS FROM CERES RUN                                      
          !  CERES INPUTS     =  200.0 200.0 200.0 200.0 500.0
          !  CERES from PHINT =  400.0 285.0 190.0 200.0 500.0
          !  ZADOKS FROM CERES INPUTS AND PHINT  
          !   P1   ->TS  2 200.0  400.0
          !   P2   ->Jt  3  50.0   71.2
          !   P3  TS->LL 4 150.0  213.8
          !   P4  LL->SE 5 200.0  190.0
          !   P5 ESG->AN 6  50.0   50.0
          !   P6  AN->EA 7  20.0   20.0
          !   P7  EA->EL 8 130.0  130.0
          !   P8  EL->PM 9 500.0  500.0

          ! PSNO PSNAME                        ! PSNO PSTYP PSABV PSNAME                                          
          !   1  T.Spikelet                    !   1      S GDAT  Germinate                                       
          !   2  EndVegetative                 !   2      K TSAT  T.Spikelet                                      
          !   3  EndEarGrowth                  !   3      S PSDAT Pseudo_Stem                                     
          !   4  BeginGrainFill                !   4      S LLDAT End_Leaf                                        
          !   5  EndGrainFill                  !   5      S IEDAT Head_Emerge                                     
          !   6  Harvest                       !   6      K ADAT  Anthesis                                        
          !   7  Sowing                        !   7      S AEDAT EndAnthesis                                    
          !   8  Germinate                     !   8      S GFDAT MilkToDough                                     
          !   9  Emergance                     !   9      M MDAT  HardDough                                       
          
        ENDIF   ! Reproductive development
        
        ! The 'Examine' section is to alllow standardisation of coefficients for
        ! comparison with CERES. 
        IF (EXAMINE(20).EQ.'Y') THEN  
          ! Everything from all genotype files    
          WRITE(fnumwrk,*)' '
          WRITE(fnumwrk,*)' RUNNING EXAMINE. CODE = 2'
          WRITE(fnumwrk,*)' '
        
          ! CULTIVAR:
          VREQ = 8.0
          PPS(1) = 100.0
          PD(8) = 500.0
          GNOWTS = 25.0
          GWTS = 40.0
          G3 = 2.5
          PHINT = 80
          PD(1) = 380
          PD(2) =  70
          PD(3) = 200
          PD(4) = 200 
          PD(5) =  60
          PD(6) =  25
          PD(7) = 150 
          ! PSNO PSTYP PSABV PSNAME
          !   1      S GDAT  Germinate
          !   2      K TSAT  T.Spikelet
          !   3      S PSDAT Pseudo_Stem
          !   4      S LLDAT End_Leaf
          !   5      S IEDAT Head_Emerge
          !   6      K ADAT  Anthesis
          !   7      S AEDAT End_Anthesis
          !   8      S GFDAT MilkToDough
          !   9      M MDAT  HardDough
          LA1S = 3.0
          LAFV = 0.1
          LAFR = 0.5
          VBASE = 0.0
          VEFF = 1.0
          PPS(2) = 0.0
          
       ! ECOTYPE:
          PARUE = 2.7
          PARU2 = 2.7
          PHINTL(2) = 12
          PHINTF(3) = 1.3
          LAWS = 400
          LSENI = 1.5
          LSPHS = 8.0
          LSPHE = 9.3
          TI1LF = 4.45
          TIFAC = 1.0
          TDPHS = 3.0
          TDPHE = 7.0
          TDFAC = 5.0
          TDSF = 1.0
          RDGS = 3.0
          HTSTD = 100
          AWNS = 5.0
          KCAN = 0.85
          RSPCA = 15.0
          GNPCS = 2.0
          TKFH = -15
          SSPHS = 8.0
          SSPHE = 9.3
          GWTAF = 0.0        
          GWTAT = 26.0         
          RTNUP = 0.006          
          NUPCF = 0.0               
          NUPNF = 0.0               
          NUPWF = 0.0
          
        ! SPECIES  
          GMPCH = 15
          ! Secondary Stages (S=Standard,K=Key(those that outputed to Evaluate.out)
          ! SSNO SSTYP  SSTG SSABV SSNAME
          !   1      S   1.5 LAFST Lafac_change          
          !   2      K   1.6 DRAT  DoubleRidges
          !   3      S 3.143 JDAT  Jointing
          !   4      S   3.3 SGPHS StemStart             
          !   5      S   4.0 RUESG RUE_change            
          !   6      S   4.0 LGPHE LeafEnd               
          !   7      S   4.0 LRPHS LfRetention           
          !   8      S   4.8 CHPHS ChaffStart            
          !   9      S   5.2 SVPHS StemVisible           
          !  10      S   6.5 SGPHE StemEnd               
          !  11      S   6.7 CHPHE ChaffEnd              
          !  12      S   7.0 GGPHS GrainStart            
          !  13      S   8.0 GLPHS GrLinearStart         
          !  14      S   8.7 GLPHE GrLinearEnd           
          !  15      S   9.0 GGPHE GrainEnd              
          PGERM = 10.0
          PEMRG = 8.0
          Pd(0) = 0.0
          Pd(9) = 200.0
          PPFPE = 1.0
          PPTHR = 20.0
          PPEXP = 2.0
          VPEND = 2.0
          VEEND = 2.0
          VLOSS = 0.2
          VLOST = 30.0
          VLOSF = 0.5
          RLIGPC = 10.0
          RLWR = 0.9
          RSEN = 0.1
          RRESP = 0.80
          RDGAF = 0.5
          RTUFR = 0.20
          LLIGPC = 10.0
          LAXS = 100.0
          LAWCF = 0.02
          LAWFF = 0.20
          LSHFR = 0.33
          LSHAV = 50.0      
          LSHAR = 80.0       
          PHINTL(1) = 2.0    ! PHL
          PHINTF(1) = 0.8    ! PHF
          LLIFA = 4.0
          LLIFG = 1.0
          LLIFS = 3.0
          LWLOS = 0.5        ! LWLOS
          LAIXX = 14.0
          ! CHT%  CLA% ! Height%, cumulative leaf area %
          !    0     0
          !   50    50
          !  100   100
          TPAR = 0.07
          TSRAD = 0.25
          TGR(2) = 0.80
          TGR(20) = 0.10
          TILIP = 6.0
          TILNOX = 20   
          RSPCX = 80
          RSPCLX = 80.0
          RSUSE = 0.1
          ! Stems          
          SLIGPC = 10.0
          SAWS = 25.0
          ! Chaff
          CHFR = 0.65
          ! Grain  
          GLIGPC = 10.0
          GWLAGFR = 0.05  ! GWLAG
          GWLINFR = 0.90  ! GWLIN 
          ! Seed
          SDWT = 0.28
          SDDUR = 20.0
          SDRSPC = 80.0
          ! Photosynthesis
          RSFPU = 3.0
          RSFPL = 2.0
          !CO2RF  CO2F
          !    0  0.00
          !  220  0.71
          !  330  1.00
          !  440  1.08
          !  550  1.17
          !  660  1.25
          !  770  1.32
          !  880  1.38
          !  990  1.43
          ! 9999  1.50
          ! CH2O distribution/mobilization
          PTFMN = 0.75
          PTFMX = 0.98
          PTFA = 0.10
          PTFXS = 6.0
          ! Cold hardiness
          TKUH = -6.0
          TKDTI = 2.0
          TKDLF = 2.0
          TKSPN = 2.0
          HDUR = 10.0
          HLOST = 10.0
          HLOSF = 0.2

          ! Water shortage effects
          WFPU = 1.0
          WFPL = 0.0
          WFGU = 1.3
          WFGL = 0.0
          WFTU = 1.0
          WFTL = 0.5
          WFSU = 0.6
          WFSF = 0.5
          LLOSA = 0.02
          WFGEU = 0.5
          WFRGU = 0.25

          ! NITROGEN uptake,distribution,etc.
           NH4MN = 0.5 
           NO3MN = 0.5
           NTUPF = 0.05
           ! Nitrogen concentrations
           GNPCMX = 4.5 
           GNPCMN = 0.5 
           SDNPCI = 1.9             
           ! LN%S LN%MN  SN%S SN%MN  RN%S RN%MN   
           !  8.0  0.80  2.50  0.65  2.00  1.50                        
           !  1.0  0.55  0.40  0.40  1.70  1.25                         
           NLABPC = 20
           NFPU = 1.0 
           NFPL = 0.0 
           NFGU = 1.0 
           NFGL = 0.0 
           NFTU = 1.0 
           NFTL = 0.0 
           NFSU = 0.4 
           NFSF = 0.1
           NCRG = 30

        ENDIF   ! EXAMINE(20)

!-----------------------------------------------------------------------
!       Determine 'key' principal and secondary stages,and adjust names
!-----------------------------------------------------------------------

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
            IF (PSABV(L).EQ.'TSAT') TSSTG = L
            PSNUM = PSNUM + 1
          ENDIF
        ENDDO
        ! IF MSTG not found, use maximum principal stage number
        IF (MSTG.LE.0) THEN
          MSTG = KEYPSNUM
        ENDIF
        ! IF HSTG not found, use maximum principal stage number
        IF (HSTG.LE.0) THEN
          HSTG = MSTG+1  ! LAH 230311
          PSTART(HSTG) = 5000   ! Set to very long cycle
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
        ENDDO
        
!-----------------------------------------------------------------------
!       Calculate PHASE DURATIONS FROM phints if missing
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
        IF (PD(MSTG).LT.0.0) PD(MSTG) = PDMTOHAR
        IF (CTRNUMPD.GT.0) THEN
          WRITE(MESSAGE(1),'(A11,I2,A23,A34)')
     &    'Duration of',CTRNUMPD,' phases less than zero.',
     &    'Used value(s) for preceding phase.'
          CALL WARNING(1,'CSCRP',MESSAGE)
          WRITE(Fnumwrk,*)' '
          WRITE(Fnumwrk,'(A12,I2,A23,A34)')
     &    ' Duration of',CTRNUMPD,' phases less than zero.',
     &    'Used value(s) for preceding phase.'
        ENDIF

        ! Calculate thresholds
        GGPHASEDU = 0.
        CHPHASEDU = 0.
        LAFSTDU = 0.
        LGPHASEDU = 0.
        LRETSDU = 0. 
        LSPHSDU = 0.
        SSPHSDU = 0.
        SSPHEDU = 0.
        RUESTGDU = 0.
        SGPHASEDU = 0.
        STVSTGDU =  0.
        TDPHEDU = 0.
        TDPHSDU = 0.
        TILPEDU = 0.
        VEENDDU = 0.
        VPENDDU = 0.
        DO L = 0,MSTG
          PSTART(L) = 0.0
        ENDDO
        DO L = 1,MSTG
          PSTART(L) = PSTART(L-1) + AMAX1(0.0,PD(L-1))
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
          IF (L.EQ.INT(LSPHS))
     &      LSPHSDU = PSTART(L)+(LSPHS-FLOAT(INT(LSPHS)))*PD(L)
          IF (L.EQ.INT(LSPHE))
     &      LSPHEDU = PSTART(L)+(LSPHE-FLOAT(INT(LSPHE)))*PD(L)
          IF (L.EQ.INT(SSPHS)) SSPHSDU =
     &     PSTART(L)+(SSPHS-FLOAT(INT(SSPHS)))*PD(L)
          IF (L.EQ.INT(SSPHE)) SSPHEDU =
     &     PSTART(L)+(SSPHE-FLOAT(INT(SSPHE)))*PD(L)
          IF (L.EQ.INT(TILPE))
     &      TILPEDU = PSTART(L)+(TILPE-FLOAT(INT(TILPE)))*PD(L)
          IF (L.EQ.INT(TDPHS))
     &      TDPHSDU = PSTART(L)+(TDPHS-FLOAT(INT(TDPHS)))*PD(L)
          IF (L.EQ.INT(TDPHE))
     &      TDPHEDU = PSTART(L)+(TDPHE-FLOAT(INT(TDPHE)))*PD(L)
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
        DO L = 1, MSTG-1
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
        IF (PLMAGE.LT.5.0.AND.PLMAGE.GT.-90.0) THEN
          PEGD = AMAX1(0.0,PGERM - (PLMAGE*STDAY)) ! Dormancy -ve age
        ELSE
          PEGD = PGERM
        ENDIF

!-----------------------------------------------------------------------
!       Check and/or adjust coefficients and set defaults if not present
!-----------------------------------------------------------------------

        ! Phases
        IF (LGPHASE(1).LT.0.0) LGPHASEDU(1) = 0.0
        IF (LGPHASE(2).LE.0.0) LGPHASEDU(2) = DUTOMSTG
        IF (SGPHASEDU(1).LT.0.0) SGPHASEDU(1) = 0.0
        IF (SGPHASEDU(2).LE.0.0) SGPHASEDU(2) = DUTOMSTG
        IF (GGPHASE(1).LE.0.0) GGPHASEDU(1) = 99999
        IF (CHPHASE(1).LE.0.0) CHPHASEDU(1) = 99999
        IF (CHPHASE(2).LE.0.0) CHPHASEDU(2) = 99999
        ! Photoperiodism
        IF (PPEXP.LT.0.0) PPEXP = 2.0
        IF (PPFPE.LT.0.0) THEN  
          PPFPE = 1.0
          WRITE(MESSAGE(1),'(A51)')
     &    'Pre-emergence development factor missing. Set to 1.'
          CALL WARNING(1,'CSCRP',MESSAGE)
          WRITE(FNUMWRK,*) ' '
          WRITE(FNUMWRK,'(A51)')
     &    ' Pre-emergence development factor missing. Set to 1'
        ENDIF
        ! Vernalization
        IF (VEEND.LE.0.0) VEEND = 0.0
        IF (VEFF.LT.0.0) VEFF = 0.0
        IF (VEFF.GT.1.0) VEFF = 1.0
        IF (VLOSS.LT.0.0) VLOSS = 0.2
        IF (VLOSF.LT.0.0) VLOSF = 0.2
        IF (VLOST.LT.0.0) VLOST = 30.0
        IF (VREQ.LE.0.0) VREQ = 0.0
        IF (VBASE.LT.0.0) VBASE = 0.0
        IF (VPEND.LE.0.0) VPEND = 0.0
        ! Photosynthesis
        IF (PARUE.LE.0.0) PARUE = 2.3
        IF (SLPF.LE.0.0 .OR. SLPF.GT.1.0) SLPF = 1.0
        IF (SLPF.LT.1.0) THEN
          WRITE (fnumwrk,*) ' '
          WRITE (fnumwrk,*)
     &     'WARNING  Soil fertility factor was less than 1.0: ',slpf
        ENDIF  
        ! Partitioning
        IF (PTFMN.LE.0.0) PTFMN = 0.75
        IF (PTFMX.LE.0.0) PTFMX = 0.09
        IF (PTFA.LT.0.0) PTFA = 0.10
        IF (PTFXS.LE.0.0) PTFXS = 6.0
        ! Leaves
        IF (LA1S.LE.0.0) THEN
          LA1S = 5.0
          WRITE(MESSAGE(1),'(A47)')
     &    'Initial leaf size (LA1S) missing. Set to 5 cm2.'
          CALL WARNING(1,'CSCRP',MESSAGE)
          WRITE(Fnumwrk,*)' '
          WRITE(Fnumwrk,'(A48)')
     &    ' Initial leaf size (LA1S) missing. Set to 5 cm2.'
        ENDIF
        IF (LAFV.LT.0.0) LAFV = 0.1
        IF (LAFR.LT.-90.0) LAFR = 0.1
        IF (LAXS.LT.0.0) LAXS = 200.0
        IF (LAFST.LE.0.0) LAFST = MIN(PSX,MSTG+1)
        IF (LWLOS.LT.0.0) LWLOS = 0.3
        IF (LRETS.GE.10.OR.LRETS.LE.0.0) LRETSDU = 99999
        IF (LSPHE.LE.0.0) LSPHEDU = 99999
        IF (LSENI.LT.0.0) LSENI = 0.5
        IF (PARIX.LE.0.0) PARIX = 0.995
        IF (LSPHS.GE.10.OR.LSPHS.LE.0.0) LSPHSDU = 99999
        ! Tillers
        IF (G3.LE.0.0) THEN  
          G3 = 1.0
          WRITE(MESSAGE(1),'(A41)')
     &    'Standard shoot weight missing. Set to 1.0'
          CALL WARNING(1,'CSCRP',MESSAGE)
          WRITE(Fnumwrk,*)' '
          WRITE(Fnumwrk,'(A42)')
     &    ' Standard shoot weight missing. Set to 1.0'
        ENDIF
        IF (TDFAC.LT.0.0) TDFAC = 0.0
        IF (TDSF.LT.0.0) TDSF = 1.0
        IF (TI1LF.LT.0.0) TI1LF = 1000
        IF (TIFAC.LT.0.0) TIFAC = 1.0
        IF (TDPHE.LT.0.0) TDPHEDU = 99999
        IF (TDPHS.LT.0.0) TDPHSDU = 99999
        IF (TILIP.LT.0.0) TILIP = 6.0
        IF (TILPE.LT.0.0) TILPEDU = 99999
        IF (TILNOX.LE.0.0) TILNOX = 30
        IF (TGR(20).LT.0.0) THEN 
          DO L = 3,22
            TGR(L) = 1.0 ! Tiller (shoot) sizes relative to main shoot
          ENDDO
        ENDIF
        ! Stems
        IF (SSPHSDU.LT.0.0) SSPHSDU = 99999
        IF (SSPHEDU.LT.0.0) SSPHEDU = 99999
        IF (SWFRS.GT.0.0) THEN
          SWFRX = SWFRS  ! (Constant stem fr)
          SWFRXL = 9999
          SWFRN = SWFRS
          SWFRNL = 0
        ENDIF 
        ! Grain                
        IF (GMPCH.LT.0.0) THEN
          GMPCH = 15.0
          WRITE(Fnumwrk,*)' '
          WRITE(Fnumwrk,'(A38)')
     &    ' Grain moisture % missing. Set to 15.0'
        ENDIF
        IF (GWTS.LE.0.0) THEN  
          GWTS = 30.0
          WRITE(MESSAGE(1),'(A33)')
     &    'Grain weight missing. Set to 30.0'
          CALL WARNING(1,'CSCRP',MESSAGE)
          WRITE(Fnumwrk,*)' '
          WRITE(Fnumwrk,'(A34)')
     &    ' Grain weight missing. Set to 30.0'
        ENDIF
        IF (GWTAT.LT.0.0) GWTAT = 20.0
        IF (GWTAF.LT.0.0) GWTAF = 0.0
        IF (GNOWTS.LE.0.0) THEN  
          GNOWTS = 20.0
          WRITE(MESSAGE(1),'(A49)')
     &    'Grain number per unit weight missing. Set to 20.0'
          CALL WARNING(1,'CSCRP',MESSAGE)
          WRITE(Fnumwrk,*)' '
          WRITE(Fnumwrk,'(A50)')
     &    ' Grain number per unit weight missing. Set to 20.0'
        ENDIF
        IF (GNORF.LT.0.0) GNORF = 0.0
        IF (GNORT.LT.0.0) GNORT = 10.0
        IF (GNPCS.LE.0.0) GNPCS = 2.0
        IF (GNPCMX.LE.0.0) GNPCMX = 5.0
        IF (GNPCMN.LT.0.0) GNPCMN = 0.0
        IF (GWLAGFR.LE.0.0) GWLAGFR = 0.05
        IF (GWLINFR.LE.0.0) GWLINFR = 0.95
        IF (TRGFC(2).LE.0.0) THEN        ! Grain temperature responses
          DO L = 1,4
             TRGFC(L) = TRPHS(L)
             TRGFN(L) = TRPHS(L)
          ENDDO
        ENDIF
        ! Roots
        IF (RDGS.LT.0.0) RDGS = 3.0
        IF (RSEN.LT.0.0) RSEN = 0.1
        IF (RTUFR.LT.0.0) RTUFR = 0.05   ! (Useable to reestablish tops)
        ! Reserves
        IF (RSPCA.LT.0.0) RSPCA = 15.0
        IF (RSPCX.LT.0.0) RSPCX = 80.0
        IF (RSUSE.LT.0.0) RSUSE = 0.10
        ! Water uptake
        IF (RLFWU.LT.0.0) RLFWU = 0.5    ! (Only alternate method)
        ! N uptake  
        IF (rtnup.le.0.0) RTNUP = 0.006  ! NO3 uptake/root lgth (mgN/cm)
        IF (nupcf.lt.-90.0) NUPCF = 0.0  ! H2O conc factor for N uptake 
        IF (nupnf.lt.-90.0) NUPNF = 0.0  ! NO3 uptake conc factor (exp)
        IF (nupwf.lt.-90.0) NUPWF = 0.0  ! H2O conc factor for N uptake 
        IF (no3mn.lt.0.0) no3mn = 0.5
        IF (nh4mn.lt.0.0) nh4mn = 0.5
        IF (NTUPF.LT.0.0) NTUPF = 0.2
        DO L = 0,1
        LNCXS(L) = LNPCS(L)/100.0 
        SNCXS(L) = SNPCS(L)/100.0 
        RNCXS(L) = RNPCS(L)/100.0 
        LNCMN(L) = LNPCMN(L)/100.0
        SNCMN(L) = SNPCMN(L)/100.0
        RNCMN(L) = RNPCMN(L)/100.0
        ENDDO
        ! Stress factors
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
        IF (NFSU.LT.0.0) NFSU = 0.2
        IF (NFSF.LT.0.0) NFSF = 0.1
        ! Cold hardiness
        IF (HLOSF.LT.0.0) HLOSF = 0.2
        IF (HLOST.LT.0.0) HLOST = 20.0
        IF (TKDLF.LT.-90.0) TKDLF = 2.0
        IF (TKSPN.LT.0.0) TKSPN = 6.0
        IF (TKDTI.LT.0.0) TKDTI = 2.0
        ! High temperature sensitivity
        IF (TKGF.LE.0.0) TKGF = 45.0
        
!-----------------------------------------------------------------------
!       Calculate derived coefficients and set equivalences
!-----------------------------------------------------------------------

        ! Initial leaf growth aspects
        Lapotx(1) = La1s
        ! If max LAI not read-in,calculate from max interception
        IF (LAIXX.LE.0.0) LAIXX = LOG(1.0-PARIX)/(-KCAN)
        
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
        phintstore = phint
        LLIFGTT = LLIFG * PHINT 
        LLIFATT = LLIFA * PHINT 
        LLIFSTT = LLIFS * PHINT 
        ! End New

        ! Extinction coeff for SRAD
        KEP = (KCAN/(1.0-TPAR)) * (1.0-TSRAD)

        ! Photoperiod sensitivities
        DO L = 0,10
          IF (PPS(L).LE.0.0) PPS(L) = 0.0
        ENDDO
        IF (Pps(1).EQ.0.0.AND.Ppfpe.LT.1.0) THEN
          WRITE(MESSAGE(1),'(A36,A41)')
     &    'Cultivar insensitive to photoperiod ',
     &    'but pre-emergence photoperiod factor < 1.' 
          WRITE(MESSAGE(2),'(A40)')
     &    'May be worthwhile to change PPFPE to 1.0'
          CALL WARNING(2,'CSCRP',MESSAGE)
          WRITE(Fnumwrk,*)' '
          WRITE(Fnumwrk,'(A37,A41)')
     &    ' Cultivar insensitive to photoperiod ',
     &    'but pre-emergence photoperiod factor < 1.' 
          WRITE(Fnumwrk,'(A41)')
     &    ' May be worthwhile to change PPFPE to 1.0'
        ENDIF

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

        ! Height growth
        SERX = HTSTD/(PD(1)+PD(2)+PD(3)+PD(4)+PD(5)+PD(6))

!-----------------------------------------------------------------------
!       Set coefficients that dependent on input switch
!-----------------------------------------------------------------------

        IF (ISWWATCROP.EQ.'E') THEN
          ! Plant water status effects on growth turned off
          WFTU = 0.0
          WFGU = 0.0
          WFPU = 0.0
          WFSU = 0.0
          WFSF = 0.0
          WFRGU = 0.0
        ENDIF

!-----------------------------------------------------------------------
!       Calculate/set initial states
!-----------------------------------------------------------------------

        IF (SDRATE.LE.0.0) SDRATE = SDWT*PLTPOPP*10.0
        ! Reserves = 80% of seed (42% Ceres3.5)
        SEEDRSI = (SDRATE/(PLTPOPP*10.0))*SDRSPC/100.0
        SEEDRS = SEEDRSI
        SEEDRSAV = SEEDRS
        SDCOAT = (SDRATE/(PLTPOPP*10.0))*(1.0-SDRSPC/100.0)
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
        WTDEP = ICWD

        ! Initial shoot and root placement
        IF (SPRL.LT.0.0) SPRL = 0.0
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
        IF (MEPHS.NE.'I') THEN
          IF (MEPHS.NE.'M') THEN
            IF (MEPHS.NE.'R') THEN
              WRITE(MESSAGE(1),'(A22,A1,A15,A19)')
     &          'Photosynthesis method ',MEPHS,' not an option ',
     &          ' Changed to R (RUE)'
              CALL WARNING(1,'CSCRP',MESSAGE)
              WRITE(FNUMWRK,*)' '
              WRITE(FNUMWRK,'(A32,A1,A15,A19)')
     &          ' WARNING  Photosynthesis method ',MEPHS,
     &          ' not an option ',
     &          ' Changed to R (RUE)'
              MEPHS = 'R'
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
            CALL WARNING(1,'CSCRP',MESSAGE)
            WRITE(Fnumwrk,*)' '
            WRITE(Fnumwrk,'(A38,A13,A1)')
     &        ' No harvest date set although planting',
     &        'flag set to: ',IHARI
            WRITE(Fnumwrk,'(A17)')
     &       ' Flag reset to M.'
            IHARI = 'M'                      
          ENDIF
        ENDIF

!-------------------------------------------------------------------
!       Write run information to Overview and Work output files
!-------------------------------------------------------------------

        ! To avoid problems of writing to closed file in Sequence mode 
        INQUIRE (FILE = 'WORK.OUT',OPENED = FOPEN)
        IF (.NOT.FOPEN) THEN
          OPEN (UNIT = FNUMWRK,FILE = 'WORK.OUT')
          WRITE(fnumwrk,*) 'CSCRP  Cropsim Cereal Crop Module '
        ENDIF
                    
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
        WRITE(fnumwrk,'(A13,I6)')'  VERSION    ',VERSION
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
         WRITE(fnumwrk,'(A26,A1,1X,A1)') '   WATER & N SWITCHES     '
     &     ,ISWWAT,ISWNIT
         WRITE(fnumwrk,'(A26,A1,1X,A1)') '   PHASE & PHINT SWITCHES '
     &     ,CFLPHASEADJ,CFLPHINTADJ
         WRITE(fnumwrk,'(A54)')                              
     &     '   (NB.Default phase switch is Y = use Aitken formula;'
         WRITE(fnumwrk,'(A58)')                              
     &     '   Default phint switch is N = no daylength change effect)'
         WRITE(fnumwrk,'(A26,I1)') ' '
         WRITE(fnumwrk,'(A26,I1)') '  CROP COMPONENT          ',CN
         WRITE(fnumwrk,'(A26,A6,2X,A16)')
     &     '  CULTIVAR                ',VARNO,VRNAME
!       IF (IPLTI.NE.'A') THEN
        IF (IPLTI.NE.'A' .AND. IPLTI.NE.'F') THEN
          WRITE(fnumwrk,'(A23,I7)')
     &     '  PLANTING DATE TARGET:',PLYEARDOYT
        ELSE
          WRITE(fnumwrk,'(A23)')
     &     '  AUTOMATIC PLANTING   '              
          WRITE (fnumwrk,*) '  PFIRST,PLAST :',pwdinf,pwdinl
          WRITE (fnumwrk,*) '  HFIRST,HLAST :',hfirst,hlast
        ENDIF
        WRITE (fnumwrk,'(A15,2F7.1)')'  PLTPOP,ROWSPC',PLTPOPP,ROWSPC
        WRITE (fnumwrk,'(A15,2F7.1)')'  SDEPTH,SDRATE',SDEPTH,SDRATE
        WRITE (fnumwrk,'(A15,F7.4,F7.2)')'  SDWT,SDNPCI  ',SDWT,SDNPCI
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
          WRITE(fnumwrk,*)' '
          WRITE(fnumwrk,*)' SEEDLING ASPECTS'
          WRITE(fnumwrk,*)'  Seed '
          WRITE(fnumwrk,*)'   Sdwt,Sdrs% ',Sdwt,Sdrspc 
          WRITE(fnumwrk,*)'   Sddur  ',Sddur 
          WRITE(fnumwrk,*)'  Germination and Emergence '
          WRITE(fnumwrk,*)'   Pgerm  ',Pgerm 
          WRITE(fnumwrk,*)'   Pemrg  ',Pemrg 
          WRITE(fnumwrk,*)' DEVELOPMENT '
          WRITE(fnumwrk,*)'  Thermal time between major stages'
          WRITE(fnumwrk,*)'   P0     ',Pd(0)
          WRITE(fnumwrk,*)'   P1     ',Pd(1)
          WRITE(fnumwrk,*)'   P2     ',Pd(2)
          WRITE(fnumwrk,*)'   P3     ',Pd(3)
          WRITE(fnumwrk,*)'   P4     ',Pd(4)
          WRITE(fnumwrk,*)'   P5     ',Pd(5)
          WRITE(fnumwrk,*)'   P6     ',Pd(6)
          WRITE(fnumwrk,*)'   P7     ',Pd(7)
          WRITE(fnumwrk,*)'   P8     ',Pd(8)
          WRITE(fnumwrk,*)'   P9     ',Pd(9)
          WRITE(fnumwrk,*)'  Stage at which intermediate events occur '
          WRITE(fnumwrk,*)'   Chaff '
          WRITE(fnumwrk,*)'    Chphs  ',Chphase(1)               
          WRITE(fnumwrk,*)'    Chphe  ',Chphase(2)             
          WRITE(fnumwrk,*)'   Grain  '
          WRITE(fnumwrk,*)'    Ggphs  ',GGphase(1)               
          WRITE(fnumwrk,*)'    Glphs  ',GGphase(2)               
          WRITE(fnumwrk,*)'    Glphe  ',GGphase(3)              
          WRITE(fnumwrk,*)'    Ggphe  ',GGphase(4)              
          WRITE(fnumwrk,*)'  Thermal time modifiers '              
          WRITE(fnumwrk,*)'   Vernalization'
          WRITE(fnumwrk,*)'    Vreq,Vbase,Veff ',Vreq,Vbase,Veff
          WRITE(fnumwrk,*)'    Vpend,Veend ',Vpend,Veend
          WRITE(fnumwrk,*)'    Vloss,Vlost,Vlosf ',Vloss,vlost,vlosf
          WRITE(fnumwrk,*)'   Photoperiodism '
          IF (Ppsen.EQ.'LQ') WRITE(fnumwrk,*)'    Ppexp  ',Ppexp   
          WRITE(fnumwrk,*)'    Ppsen,Ppfpe ',Ppsen,Ppfpe   
          WRITE(fnumwrk,*)'    Ppthr  ',Ppthr   
          WRITE(fnumwrk,*)'    Pps1,2 ',Pps(1),Pps(2)
          WRITE(fnumwrk,*)' '              
          WRITE(fnumwrk,*)' GROWTH '              
          WRITE(fnumwrk,*)' Photosynthesis '
          WRITE(fnumwrk,*)'  Parue  ',Parue
          WRITE(fnumwrk,*)'  Rsfpu,Rsfpl ',Rsfpu,Rsfpl
          WRITE(fnumwrk,*)'  Parfc,Co2cc,Co2ex ',Parfc,Co2cc,Co2ex
          WRITE(fnumwrk,*)'  Co2rf,   Co2f  '
          Do l = 1,10
            WRITE(fnumwrk,'(I8,F6.1)')Nint(Co2rf(l)),Co2f(l)
          Enddo
          WRITE(fnumwrk,*)' Partitioning     '
          WRITE(fnumwrk,*)'  Ptfmx,Ptfmn  ',Ptfmx,Ptfmn
          WRITE(fnumwrk,*)'  Ptfxs,Ptfa   ',Ptfxs,Ptfa
          WRITE(fnumwrk,*)'  Paru2  ',Paru2
          WRITE(fnumwrk,*)' CH2O reserves '
          WRITE(fnumwrk,*)'  Rspca,Rspcx ',rspca,rspcx 
          WRITE(fnumwrk,*)'  Rspclx      ',rspclx 
          WRITE(fnumwrk,*)'  Rsuse       ',rsuse 
          WRITE(fnumwrk,*)' Leaves     '
          WRITE(fnumwrk,*)'  Phyllochron  '
          WRITE(fnumwrk,*)'   Phints ',Phints               
          WRITE(fnumwrk,*)'   Phl1,Phf1 ',Phintl(1),Phintf(1)
          WRITE(fnumwrk,*)'   Phl2,Phf2 ',Phintl(2),Phintf(2)
          WRITE(fnumwrk,*)'   Phl3,Phf3 ',Phintl(3),Phintf(3)
          WRITE(fnumwrk,*)'  Lamina areas    '
          WRITE(fnumwrk,*)'   Laxs   ',Laxs
          WRITE(fnumwrk,*)'   La1s   ',La1s
          WRITE(fnumwrk,*)'   Lafv   ',Lafv
          WRITE(fnumwrk,*)'   Lafr   ',Lafr
          WRITE(fnumwrk,*)'  Lamina weights '
          WRITE(fnumwrk,*)'   Slas   ',Laws
          WRITE(fnumwrk,*)'   Slacf,Slaff  ',Lawcf,Lawff
          WRITE(fnumwrk,*)'  Sheaths '
          WRITE(fnumwrk,*)'   Lshfr  ',Lshfr
          WRITE(fnumwrk,*)'   Lshav,Lshar ',Lshav,Lshar
          WRITE(fnumwrk,*)'  Longevity and senescence '
          WRITE(fnumwrk,*)'   Llifg,a,s ',Llifg,Llifa,Llifs  
          WRITE(fnumwrk,*)'   Lseni  ',Lseni                
          WRITE(fnumwrk,*)'   Lsphs,Lsphsdu ',Lsphs,lsphsdu
          WRITE(fnumwrk,*)'   Lsphe,Lsphedu ',Lsphe,lsphedu
          WRITE(fnumwrk,*)'   Lwlos  ',Lwlos                
          WRITE(fnumwrk,*)'   Llig%  ',Lligpc                
          WRITE(fnumwrk,*)' Stems         '
          WRITE(fnumwrk,*)'  Ssphs,Ssphe ',Ssphs,Ssphe
          WRITE(fnumwrk,*)'  Saws   ',Saws                 
          WRITE(fnumwrk,*)'  Slig%  ',Sligpc              
          WRITE(fnumwrk,*)' Tillers  '
          WRITE(fnumwrk,*)'  Shwts  ',g3
          WRITE(fnumwrk,*)'  Til#x  ',Tilnox
          WRITE(fnumwrk,*)'  Tgro2,Tgr20  ',Tgr(2),Tgr(20)
          WRITE(fnumwrk,*)'  Til#s,Tilpe  ',ti1lf,Tilpe
          WRITE(fnumwrk,*)'  Tifac  ',tifac
          WRITE(fnumwrk,*)'  Tilip  ',tilip
          WRITE(fnumwrk,*)'  Tdphs,Tdphe ',tdphs,Tdphe
          WRITE(fnumwrk,*)'  Tdfac,Tdsf  ',Tdfac,Tdsf
          WRITE(fnumwrk,*)' Chaff '
          WRITE(fnumwrk,*)'  Chfrp  ',chfr 
          WRITE(fnumwrk,*)' Grain '
          WRITE(fnumwrk,*)'  Number '
          WRITE(fnumwrk,*)'   G#wts  ',Gnowts
          WRITE(fnumwrk,*)'   G#rf,G#rt   ',Gnorf,Gnort
          WRITE(fnumwrk,*)'  Weight '
          WRITE(fnumwrk,*)'   Gwts   ',Gwts 
          WRITE(fnumwrk,*)'   Gwlag,Gwlin ',Gwlagfr,gwlinfr
          WRITE(fnumwrk,*)'   Gwtat,Gwtaf ',Gwtat,gwtaf
          WRITE(fnumwrk,*)'  Lignin '
          WRITE(fnumwrk,*)'   Glig%  ',Gligpc
          WRITE(fnumwrk,*)' Roots  '
          WRITE(fnumwrk,*)'  Rdgs,Rdgaf   ',Rdgs,rdgaf
          WRITE(fnumwrk,*)'  Rlwr,Rlig%   ',Rlwr,rligpc
          WRITE(fnumwrk,*)'  Rresp,Rsen   ',Rresp,rsen
          WRITE(fnumwrk,*)' Moisture contents      '
          WRITE(fnumwrk,*)'  Gm%h   ',GMPCH           
          WRITE(fnumwrk,*)' Canopy '
          WRITE(fnumwrk,*)'  Morphology '
          WRITE(fnumwrk,*)'   Htstd  ',Htstd
          WRITE(fnumwrk,'(A10,3F5.1)')'    Cht%  ',Chtpc          
          WRITE(fnumwrk,'(A10,3F5.1)')'    Cla%  ',Clapc          
          WRITE(fnumwrk,*)'   Awns   ',Awns 
          WRITE(fnumwrk,*)'   Laixx  ',Laixx             
          WRITE(fnumwrk,*)'  Radiation interception'
          WRITE(fnumwrk,*)'   Kcan   ',Kcan 
          WRITE(fnumwrk,*)'   Tpar,Tsrad ',Tpar,Tsrad 
          WRITE(fnumwrk,*)' Kill   '
          WRITE(fnumwrk,*)'  Tkuh,Tkfh   ',Tkuh,Tkfh
          WRITE(fnumwrk,*)'  Tkdlf,Tkdti ',Tkdlf,Tkdti
          WRITE(fnumwrk,*)'  Tkspn   ',Tkspn
          WRITE(fnumwrk,*)'  Hdur,Hlost,Hlosf ',Hdur,Hlost,Hlosf
          WRITE(fnumwrk,*)'  Tkgf    ',Tkgf
          WRITE(fnumwrk,*)' '        
          WRITE(fnumwrk,*)' WATER ASPECTS'
          WRITE(fnumwrk,*)'  Ratm,Rcrop,EOratio ',Ratm,Rcrop,Eoratio   
          WRITE(fnumwrk,*)'  Rwupm(Pormin)',Rwupm      
          WRITE(fnumwrk,*)'  Rwumx        ',Rwumx      
          WRITE(fnumwrk,*)'  Llosa        ',Llosa      
          WRITE(fnumwrk,*)' Water stress limits '
          WRITE(fnumwrk,*)'  Wfpu,L ',WFPU,WFPL       
          WRITE(fnumwrk,*)'  Wfgu,L ',WFGU,WFGL       
          WRITE(fnumwrk,*)'  Wftu,L ',WFTU,WFTL       
          WRITE(fnumwrk,*)'  Wfsu,Sf',WFSU,WFSF      
          WRITE(fnumwrk,*)'  Wfeu   ',WFEU           
          WRITE(fnumwrk,*)'  Wfgeu,Wfrgu ',Wfgeu,Wfrgu
          WRITE(fnumwrk,*)' '        
          WRITE(fnumwrk,*)' NITROGEN ASPECTS'
          WRITE(fnumwrk,*)' Uptake '
          WRITE(fnumwrk,*)'  Rtnup             ',rtnup       
          WRITE(fnumwrk,*)'  Nupnf,Nupwf,Nupcf ',nupnf,nupwf,nupcf
          WRITE(fnumwrk,*)'  No3mn,Nh4mn       ',no3mn,nh4mn
          WRITE(fnumwrk,*)'  Ntupf             ',ntupf       
          WRITE(fnumwrk,*)' Concentrations'
          WRITE(fnumwrk,*)'  Sdn%        ',Sdnpci       
          WRITE(fnumwrk,*)'  Ln%s,Ln%mn ',LnpcS,Lnpcmn    
          WRITE(fnumwrk,*)'  Sn%s,Sn%mn ',Snpcs,Snpcmn           
          WRITE(fnumwrk,*)'  Rn%s,Rn%mn ',Rnpcs,Rnpcmn    
          WRITE(fnumwrk,*)'  Gn%s,Gn%mn,Gn%mx ',Gnpcs,Gnpcmn,Gnpcmx
          WRITE(fnumwrk,*)' Stress limits '
          WRITE(fnumwrk,*)'  Nfpu,l ',Nfpu,Nfpl       
          WRITE(fnumwrk,*)'  Nfgu,l ',Nfgu,Nfgl       
          WRITE(fnumwrk,*)'  Nftu,l ',Nftu,Nftl       
          WRITE(fnumwrk,*)'  Nfsu,Sf',Nfsu,Nfsf      
          WRITE(fnumwrk,*)'  Ncrg   ',Ncrg
          WRITE(fnumwrk,*)' Mobilization'
          WRITE(fnumwrk,*)'  Nlab%  ',Nlabpc
          WRITE(fnumwrk,*)' '
          WRITE(fnumwrk,*)' TEMPERATURE RESPONSES  '
          WRITE(fnumwrk,'(A10,4F5.1)')'   Trgem  ',TRGEM           
          WRITE(fnumwrk,'(A10,4F5.1)')'   Trdv1  ',TRDV1          
          WRITE(fnumwrk,'(A10,4F5.1)')'   Trdv4  ',TRDV4          
          WRITE(fnumwrk,'(A10,4F5.1)')'   Trdv8  ',TRDV8          
          WRITE(fnumwrk,'(A10,4F5.1)')'   Trvrn  ',TRVRN          
          WRITE(fnumwrk,'(A10,4F5.1)')'   Trphs  ',TRPHS          
          WRITE(fnumwrk,'(A10,4F5.1)')'   Trlfg  ',TRLFG          
          WRITE(fnumwrk,'(A10,4F5.1)')'   Trgfw  ',TRGFC           
          WRITE(fnumwrk,'(A10,4F5.1)')'   Trgfn  ',TRGFN           
          WRITE(fnumwrk,'(A10,4F5.1)')'   Trhar  ',TRCOH          
         
          WRITE(FNUMWRK,*)' '
          WRITE(FNUMWRK,'(A17)')' PHASE THRESHOLDS'
          WRITE(FNUMWRK,'(A23)')'   PHASE START(DU) NAME'
          DO L = 1,10
            WRITE(FNUMWRK,'(I6,I10,3X,A13)')L,NINT(PSTART(L)),PSNAME(L)
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
!       Create and write warning messages re. input and output variables
!-----------------------------------------------------------------------

        WRITE(FNUMWRK,*)' '
        WRITE(FNUMWRK,'(A22)')' OUTPUTS              '

        ! Control switch for OUTPUT file names
        CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'FNAME',fname)
        IF (FNAME.EQ.'Y') THEN
          WRITE(FNUMWRK,*)' File names switched from standard. '
        ELSE  
          WRITE(FNUMWRK,*)' Using standard file names. '
        ENDIF   
        
        ! Output choices
        ! 1=Reserves in laminae,stem,crown,SLA;stem includes sheaths
        ! 2=No reserves;stem includes sheaths
        ! 3=No reserves;stem does not include sheaths 
        OUTCHOICE = 1
        IF (OUTCHOICE.EQ.2) THEN
          WRITE(MESSAGE(1),'(A34,A37)')
     &      'NO reserve CH2O included in the wt',
     &      ' of leaves,stems,and crown.          ' 
          CALL WARNING(1,'CSCAS',MESSAGE)
          WRITE(Fnumwrk,*)' '
          WRITE(FNUMWRK,'(A35,A38)')
     &      ' NO reserve CH2O included in the wt',
     &      ' of leaves,stems,and crown.           '
          WRITE(FNUMWRK,'(A35)')
     &      ' Stem weight includes the sheaths..'
        ELSEIF (OUTCHOICE.EQ.3) THEN
          WRITE(MESSAGE(1),'(A34,A37)')
     &      'NO reserve CH2O included in the wt',
     &      ' of leaves,stems,and crown.          ' 
          CALL WARNING(1,'CSCAS',MESSAGE)
          WRITE(Fnumwrk,*)' '
          WRITE(FNUMWRK,'(A35,A38)')
     &      ' NO reserve CH2O included in the wt',
     &      ' of leaves,stems,and crown.           '
          WRITE(FNUMWRK,'(A44)')
     &      ' Stem weight does NOT includes the sheaths. '
        ENDIF  

        WRITE(FNUMWRK,*)' '
        WRITE(FNUMWRK,'(A22)')' DURING RUN STATUS:   '

!***********************************************************************
      ELSEIF (DYNAMIC.EQ.RATE) THEN
!***********************************************************************

         IF (YEARDOY.GE.PLYEARDOY) THEN
           DAP = DAP + 1
           IF (EYEARDOY.GT.0) DAE = DAE + 1
         ELSE
           DAP = 0
         ENDIF

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
        IF (SNOW.LE.0.0) THEN
          TMEANSURF = TMEAN
        ELSE
          TMEANSURF = 0.0
        ENDIF
        CO2AIR = 1.0E12*CO2*1.0E-6*44.0 /       ! CO2 in g/m3
     &   (8.314*1.0E7*((TMAX+TMIN)*0.5+273.0))

!-----------------------------------------------------------------------
!       Determine if today is planting day
!-----------------------------------------------------------------------

        ! YEARPLTCSM established by CSM and brought across in argument.
        !IF (FILEIOT.EQ.'DS4'.AND.RNMODE.EQ.'Q') THEN
        !  PLYEARDOYT = YEARPLTCSM
        !ENDIF
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
              ELSE
                IF (YEARDOY.GT.PWDINL) THEN
                  CFLFAIL = 'Y'
                  STGYEARDOY(12) = YEARDOY  ! Failure
                  STGYEARDOY(11) = YEARDOY  ! End Crop
                  Message(1) = 'Automatic planting failure '
                  CALL WARNING(1,'CSCRP',MESSAGE)
                  Write(Fnumwrk,*)' '
                  Write(Fnumwrk,*)' Automatic planting failure '
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF

        IF (YEARDOY.EQ.PLYEARDOY) THEN
          ! Initial soil N and H2O
          SOILNI = 0.0
          ISOILH2O = 0.0
          DO I = 1, NLAYR
            SOILNI = SOILNI + NO3LEFT(I)/(10.0/(BD(I)*(DLAYR(I))))
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
          
          ! Leaf (stomatal) resistances
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

                                                         
          IF (FILEIOT.EQ.'DS4'.AND.IDETG.NE.'N'.OR.
     &        FILEIOT.NE.'DS4') THEN                  
            ! Evapotranspiration calculations for comparison
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
              IF (RLF.GT.0.0)
     &         WRITE(FNUMWRK,*)' WFP OUT OF RANGE. WFP = ',WFP
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
          ! Used when when working with Doug Stewart's function
          !TTOLD = TT  
          !TFDOLD = TFD
          Tfdnext = TFAC4(trdv2,tmean,TTNEXT)
          IF (rstage.GE.2.0.AND.rstage.LT.3.0)THEN
            Tfd = TFAC4(trdv2,tmean,TT)
            Tfdnext = TFAC4(trdv3,tmean,TTNEXT)
          ELSEIF (rstage.GE.3.0.AND.rstage.LT.4.0)THEN
            Tfd = TFAC4(trdv3,tmean,TT)
            Tfdnext = TFAC4(trdv4,tmean,TTNEXT)
          ELSEIF (rstage.GE.4.0.AND.rstage.LT.5.0)THEN
            Tfd = TFAC4(trdv4,tmean,TT)
            Tfdnext = TFAC4(trdv5,tmean,TTNEXT)
          ELSEIF (rstage.GE.5.0.AND.rstage.LT.6.0)THEN
            Tfd = TFAC4(trdv5,tmean,TT)
            Tfdnext = TFAC4(trdv6,tmean,TTNEXT)
          ELSEIF (rstage.GE.6.0.AND.rstage.LT.7.0)THEN
            Tfd = TFAC4(trdv6,tmean,TT)
            Tfdnext = TFAC4(trdv7,tmean,TTNEXT)
          ELSEIF (rstage.GE.7.0.AND.rstage.LT.8.0)THEN
            Tfd = TFAC4(trdv7,tmean,TT)
            Tfdnext = TFAC4(trdv8,tmean,TTNEXT)
          ELSEIF (rstage.GE.8.0)THEN
            Tfd = TFAC4(trdv8,tmean,TT)
          ENDIF

          ! To test Beta function
          !Tfd = TFACBETA(trdv1,tmean,TT)
          
!         ! To test Doug Stewart's function
!         tmaxwheat = trdv1(2)
!         tminwheat = trdv1(1)
!         ! 1. Calculate where would be along temp axis for maize
!         Tequiv = tmean*(32.226/(Tmaxwheat-tminwheat))
!         ! 2.Calculate maize value according to Stewart
!         TTmaize =  0.043*tequiv*tequiv - 0.000894*tequiv*tequiv*tequiv
!         ! 3. Calculte TTwheat (from temperature span from max to min)
!         TTwheat = TTmaize * (Tmaxwheat-Tminwheat)/14.869
!         IF (tmean.LE.tmaxwheat) THEN
!           TT = TTWHEAT 
!           TTNEXT = TTWHEAT  
!           Tfd = TT/(TMAXWHEAT-TMINWHEAT)
!           Tfdnext = TT/(TMAXWHEAT-TMINWHEAT)
!         ELSE
!           ! Use standard function if temp above maximum 
!           TT = TTOLD 
!           TTNEXT = TTOLD  
!           Tfd = TFDOLD
!           Tfdnext = TFDOLD
!         ENDIF  
          
          IF (trgem(3).GT.0.0) THEN
            Tfgem = TFAC4(trgem,tmean,TTGEM)
          ELSE
            Ttgem = tt
          ENDIF    

          Tflflife = TFAC4(trdv1,tmean,TTlflife)

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
            ! If not yet germinated
            IF (GESTAGE.LT.1.0) THEN
              ! If the layer in which the seed is located not defined
              IF (LSEED.LT.0) LSEED = CSIDLAYR (NLAYR, DLAYR, SDEPTH)
              IF (LSEED.GT.1) THEN
                ! If the seed layer is > 1, use the water 'potential' 
                ! of the layer 
                SWPSD = SWP(LSEED)
              ELSE
                ! If the seed layer is = 1, use a weighted average of 
                ! the 'water potentials' of layers 1 and 2 
               SWP(0) = AMIN1(1.,AMAX1(.0,(SWP(1)-0.5*(SWP(2)-SWP(1)))))
               SWPSD = SWP(0) + (SDEPTH/DLAYR(1))*(SWP(2)-SWP(0))
              ENDIF
              IF (WFGEU.GT.0.0)
     &         WFGE = AMAX1(0.0,AMIN1(1.0,(SWPSD/WFGEU)))
            ENDIF
          ENDIF
          ! If the water effect is turned off for crop growth.
          IF (ISWWATCROP.EQ.'E') WFGE = 1.0

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
              STGYEARDOY(1) = YEARDOY
            ELSEIF (GEUCUM.GT.PEGD) THEN
              GERMFR = 1.0
            ENDIF

            ! Emergence
            IF (GEUCUM.LT.PEGD+PEMRG*SDEPTHU.AND.
     &       GEUCUM+TTGEM*WFGE.LE.PEGD+PEMRG*SDEPTHU) THEN
              EMRGFR = 0.0
            ELSEIF (GEUCUM.LE.PEGD+PEMRG*SDEPTHU.AND.
     &       GEUCUM+TTGEM*WFGE.GT.PEGD+PEMRG*SDEPTHU) THEN
              EMRGFR = 1.0 - (PEGD+PEMRG*SDEPTHU-GEUCUM)/(TTGEM*WFGE)
            ELSEIF (GEUCUM.GT.PEGD+PEMRG*SDEPTHU) THEN
              EMRGFR = 1.0
            ENDIF

!-----------------------------------------------------------------------
!           Calculate temperature factors for vernalization,hardening
!-----------------------------------------------------------------------

            ! Vernalization
            Tfv = TFAC4(trvrn,tmeansurf,TTOUT)
            ! Loss of vernalization (De-vernalization)
            VDLOS = 0.0
            IF (RSTAGE.LT.2.0) THEN
              IF (CUMVD.LT.VREQ*VLOSS .AND. TMEAN.GE.VLOST) THEN
                ! In Ceres was 0.5*(TMAX-30.0)
                VDLOS = VLOSF * CUMVD  ! From AFRC
                WRITE(Fnumwrk,*)' '
                WRITE(FNUMWRK,*)' Warning. De-vernalization! ' 
              ENDIF
            ENDIF  

            ! Cold hardening
            Tfh = TFAC4(trcoh,tmeansurf,TTOUT)
            ! Loss of cold hardiness
            HARDILOS = 0.0
            IF (TMEAN.GE.HLOST) THEN
              HARDILOS = HLOSF * HARDAYS
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
              DF = 1.0 - PPS(INT(RSTAGETMP))/1000.*(PPTHR-DAYLT)
              IF (RSTAGETMP.LT.FLOAT(MSTG)) THEN
               DFNEXT = 1.-PPS(INT(RSTAGETMP+1))/1000.*(PPTHR-DAYLT)
              ELSE
               DFNEXT = DF
              ENDIF 
            ELSEIF (PPSEN.EQ.'LQ') THEN  ! Long day response,quadratic
              DF = AMAX1(0.0,AMIN1(1.0,1.0-
     &        (PPS(INT(RSTAGETMP))/10000.*(PPTHR-DAYLT)**PPEXP)))
              IF (RSTAGETMP.LT.10) DFNEXT = AMAX1(0.0,AMIN1(1.0,1.0-
     &        (PPS(INT(RSTAGETMP+1))/10000.*(PPTHR-DAYLT)**PPEXP)))
              ! Angus approach
              !IF (rstage.lt.2.0) then
              !  DF = AMAX1(0.0,1.0-EXP(-0.0927*(DAYLT-4.77))) 
              !else
              !  DF = AMAX1(0.0,1.0-EXP(-0.283*(DAYLT-9.27))) 
              !endif  
              !IF (rstage.lt.1.0) then
              !  DFNEXT = AMAX1(0.0,1.0-EXP(-0.0927*(DAYLT-4.77))) 
              !else
              !  DFNEXT = AMAX1(0.0,1.0-EXP(-0.283*(DAYLT-9.27))) 
              !endif  
              ! End Angus
              Tfdf = AMAX1(0.0,1.0-AMAX1(0.0,(TMEAN-10.0)/10.0))
              Tfdf = 1.0  ! LAH No temperature effect on DF ! 
              DF = DF + (1.0-DF)*(1.0-TFDF)
              DFNEXT = DFNEXT + (1.0-DFNEXT)*(1.0-TFDF)
            ENDIF
            
            ! Set daylength factor for output (= ppfpe before emergence)
            IF (EMRGFR.GE.1.0) THEN
              DFOUT = DF
            ELSE
              DFOUT = PPFPE
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
              IF (DUNEED.GE.TT*VF*(PPFPE*(GERMFR-EMRGFR)+DF*EMRGFR))THEN
                DUPHASE = TT*VF*(PPFPE*(GERMFR-EMRGFR)+DF*EMRGFR)
                ! CERES:
                !DU = TT*VF*DF*LIF2    ! NB. Changed from Ceres 3.5
                TIMENEED = 1.0
                DUPNEXT = 0.0
              ELSE  
                DUPHASE = DUNEED
                TIMENEED = DUNEED/
     &           (TT*VF*(PPFPE*(GERMFR-EMRGFR)+DF*EMRGFR))
                DUPNEXT = TTNEXT*(1.0-TIMENEED)*VFNEXT*DFNEXT
              ENDIF
            ENDIF
            
            DU = DUPHASE+DUPNEXT

            ! Leaf growth units
            IF (CUMDU.LT.LGPHASEDU(2)) THEN
            
              IF (DU.GT.0.0) THEN
               LFENDFR = AMAX1(0.0,AMIN1(1.0,(LGPHASEDU(2)-CUMDU)/DU))
              ELSE
                LFENDFR = 1.0
              ENDIF
              
              ! Leaf appearance driven by its own temp response till TS
              ! (DULF based solely on temperature response till TS)
              ! Thereafter linked to crop development units
              IF (CUMDU+DU.LT.PSTART(TSSTG)) THEN
                ! Not reached TS yet
         !       DULF = TTLA*EMRGFR*LFENDFR  ! Thermal time
                DULF = TT*EMRGFR*LFENDFR  ! Thermal time
              ELSE
                IF (CUMDU.LT.PSTART(TSSTG)) THEN
                  ! Reach TS this day
                  WRITE(fnumwrk,*)' '
                  WRITE(fnumwrk,'(A37,2F7.2)')
     &              ' Terminal spikelet. Rstage,Leaf#:    ',
     &              RSTAGE,LNUM
 !                 DULF = (TTLA*TIMENEED+DUPNEXT)*EMRGFR*LFENDFR
                  DULF = (TT*TIMENEED+DUPNEXT)*EMRGFR*LFENDFR
 !                 LNUMTS = LNUM + (TTLA*TIMENEED*EMRGFR*LFENDFR)/PHINT
                  LNUMTS = LNUM + (TT*TIMENEED*EMRGFR*LFENDFR)/PHINT
                  FLN = LNUMTS+(PD(2)+PD(3))/PHINT
                  ! Calculate FLN using Aitken formula
                  FLNAITKEN = LNUMTS + (2.8 + 0.1*LNUMTS) 

                  IF (CFLPHASEADJ.EQ.'Y') THEN  
                    ! Use Aitken formula for FLN 
                    FLN = FLNAITKEN               
                    ! Re-calculate PD(3) and PD(4). PDADJ = PD(2+3)ADJUSTED     
                    IF (PHINTL(2).GT.0..AND.FLN+1..LE.PHINTL(2)) THEN
                      ! Final leaf# < (leaf# for change in phint)
                      PDADJ = (FLN-LNUMTS) * PHINTS*PHINTF(2)
                    ELSEIF (PHINTL(2).GT.0. .AND.
     &               FLN+1..GT.PHINTL(2)) THEN
                      ! Final leaf# > (leaf# for change in phint)
                      IF (LNUM.GE.PHINTL(2)) THEN
                        ! Leaf# at TS > (leaf# for change in phint)
                        PDADJ = (FLN-LNUMTS) * PHINTS*PHINTF(3)  
                      ELSE
                        ! Leaf# at TS < (leaf# for change in phint)
                        PDADJ = ((FLN-PHINTL(2)))*PHINTS*PHINTF(3)  
     &                      + (PHINTL(2)-LNUMTS)*PHINTS*PHINTF(2) 
                      ENDIF   
                    ENDIF
                    WRITE(fnumwrk,*)' '
                    WRITE(fnumwrk,'(A47)')
     &               ' Phase 3 (Pseudo-stem to last leaf) adjusted   '
                     PD3AITKEN = PDADJ-PD(2)
                    IF ((PD3AITKEN-PD(3)).GT.(PD(4)-10.0)) THEN
                      PD3NEW = PD(3) + (PD(4)-10.0)
                      WRITE(fnumwrk,'(A47)')
     &                '   Pd3 Aitken adjustment > Pd(4). Not possible.'
                      WRITE(fnumwrk,'(A29,2F7.1)')
     &                '   Pd3 Aitken,what possible: ',pd3aitken,pd3new
                    ELSE
                      PD3NEW = PD3AITKEN  
                    ENDIF
                    WRITE(fnumwrk,'(A16,2F7.1)')
     &               '   Pd3 old,new  ',PD(3),PD3NEW
     
                    WRITE(fnumwrk,'(A16,2F7.1)')
     &               '   Pd4 old,new  ',
     &                   PD(4),PD(4)+(PD(3)-PD3NEW)
                    ! Re-set thresholds
                    PSTART(4) = PSTART(4) + PD3NEW - PD(3)
                    ! Need to re-calculate following: 
                    ! (If set to occur in phase 3 or 4)
                    ! TILPEDU = 
                    ! TDPHSDU = 
                    ! TDPHEDU = 
                    ! Re-set end of leaf growth phase
                    WRITE(fnumwrk,'(A16,2F7.1)')
     &               '   End of leaf growth phase. Old,new ',
     &               LGPHASEDU(2),LGPHASEDU(2) + PD3NEW - PD(3)
                    LGPHASEDU(2) = LGPHASEDU(2) + PD3NEW - PD(3)
                    ! Re-set PD(3) and PD(4)
                    PD(4) = PD(4) + (PD(3)-PD3NEW) 
                    PD(3) = PD3NEW
                  ENDIF    ! End of CFLADJ calculations 
                  
                ELSE  ! After TS 
                  DULF = DU*EMRGFR*LFENDFR  ! Development units
                ENDIF                
                
              ENDIF  ! End of before,afer terminal spikelet
              
            ENDIF  ! End of leaf growth phase calculations
            
!-----------------------------------------------------------------------
!           Set seed reserve use for root growth and update av.reserves
!-----------------------------------------------------------------------

            IF (GERMFR.GT.0.0.OR.GESTAGE.GE.0.5) THEN
              ! SDDUR control on what available for roots
              SEEDRSAVR =
     &         AMIN1(SEEDRS,SEEDRSI/SDDUR*(TT/STDAY)*GERMFR)
            ELSE
              SEEDRSAVR = 0.0
            ENDIF
            ! Seed reserves available
            SEEDRSAV = SEEDRSAV-SEEDRSAVR

!=======================================================================
            IF (GEUCUM+TTGEM*WFGE.GT.PGERM+PEMRG*SDEPTHU) THEN  !If emrg
!=======================================================================

!-----------------------------------------------------------------------
!             Determine if today has a harvest instruction
!-----------------------------------------------------------------------

              HANUM = -99
              HAFR = 0.0
              DO I = 1, 20
                IF (HYEARDOY(I).EQ.YEARDOY) THEN
                  HANUM = I
                  WRITE(fnumwrk,*) ' '
                  WRITE(fnumwrk,'(A20,i2,A12,A1,A6,i8)')
     &             ' Harvest instruction',hanum,
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
     &	           CWAD.GT.0.0.AND.CWAD.GT.CWAN(HANUM)) THEN
                    IF (LSNUM(HANUM).GT.0) THEN
                      ! LAH To fit with Zhang should have Animal routine
!                     CALL ANIMAL(TMAX,TMIN,LSNUM(HANUM),
!    &                LSWT(HANUM),CWAD,CWAN(HANUM),hawad)
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
              dwrph = sentopretained * hafr
              lnph = leafn * hafr
              snph = stemn * hafr
              rsnph = rsn * hafr
              gnph = grainn * hafr
              IF (rstage.GT.3.0) spnumh = tnum * hafr*spnumhfac

!-----------------------------------------------------------------------
!             Set aspects that determined on emergence day
!-----------------------------------------------------------------------

              IF (DAE.LT.0) THEN
                IF (CROP.EQ.'BA') THEN
                  WRITE(FNUMWRK,*) 'Emergence day ',yeardoy,plyeardoy
                  WRITE(FNUMWRK,*)
     &             ' PHINTS as read-in ',phints
                  PHINTS = PHINTS - 232.6*(DAYLT-DAYLPREV)
                ENDIF
                LNUMSG = 1  
              ENDIF

!-----------------------------------------------------------------------
!             Check for or calculate PAR interception at start of day
!-----------------------------------------------------------------------

              ! Updated here rather than in integrate because may need
              ! interception from companion crops.
              PARI = 0.0
              PARI1 = (1.0 - EXP((-KCAN)*LAI))
              IF (PARIP.GT.0.0) THEN
                ! From competition model
                IF (ISWDIS(LENDIS:LENDIS).NE.'N') THEN
                  PARI = PARIPA/100.0
                ELSE
                  PARI = PARIP/100.0
                ENDIF
                ! LAH  Should use canopy area during grain filling
                !IF (CUMDU.GT.PSTART(IESTG)) THEN
                !  PARI = AMAX1(PARI,(1.0-EXP(-KCAN*CAID)))
                !ENDIF
              ELSE
                PARI = PARI1
                ! LAH For row crops may need to change 
                ! In original Ceres maize, kcan is calculated as:
                ! 1.5 - 0.768*((rowspc*0.01)**2*pltpop)**0.1
                ! eg. 1.5 - 0.768*((75*0.01)**2*6.0)**0.1  =  0.63
              ENDIF

!-----------------------------------------------------------------------
!             Calculate adjustment to yesterday's C assimilation
!-----------------------------------------------------------------------

              ! End of day interception = today's starting interception

              IF (MEPHS.EQ.'M') CARBOEND = CARBOTMPM * PARI/PLTPOP
              IF (MEPHS.EQ.'I') CARBOEND = CARBOTMPI * PARI/PLTPOP
              IF (MEPHS.EQ.'R') CARBOEND = CARBOTMPR * PARI/PLTPOP

              CARBOADJ = (CARBOEND-CARBOBEG)/2.0*EMRGFRPREV
              ! But note, no adjustment if leaf kill
              PARMJIADJ = PARMJFAC*SRADPREV*(PARI-PARIPREV)/2.0*EMRGFR

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
              IF (RSFPU.GT.0.0.AND.RSFPL.GT.0.0) THEN
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
                  WRITE(FNUMWRK,'(A20,I3,A4,I3,A11,F4.1,A2,I7)')
     &             ' PHINT changed from ',INT(PHINT),
     &             ' to ',INT(PHINTS*PHINTF(PHINTSTG+1)),
     &             '. Leaf # = ',lnum,'  ',yeardoy
                  PHINT = PHINTS * PHINTF(PHINTSTG+1)
                  PHINTSTORE = PHINT
                  PHINTSTG = PHINTSTG + 1
                  ! Leaf growth,active,senescence phases adjusted
                  LLIFGTT = LLIFG * PHINT 
                  LLIFATT = LLIFA * PHINT 
                  LLIFSTT = LLIFS * PHINT 
                ENDIF
                IF (CFLPHINTADJ.EQ.'Y') THEN
                  ! Adjust phint for rate of change of daylength.Kirby
                  !tvr1 = 1.0/(0.00949+0.000988*((DAYLT-DAYLPREV)*60.0))
                  TVR2 = 
     &             1.0/((1.0/PHINTSTORE)+.000988*((DAYLT-DAYLPREV)*60.))
!                  WRITE(FNUMWRK,*)
!     &              'PHINT adjusted. Normal,f(Delta daylngth)',
!     &              PHINTSTORE,TVR2
                    PHINT = PHINTSTORE 
     &               - PHINTSTORE*0.1*((DAYLT-DAYLPREV)*60.0)
                ENDIF
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
              IF (PLA-SENLA.GT.0.0.AND.TMEAN.LT.(TKILL+TKDLF)) THEN
                SPANPOS = AMIN1(1.0,((TKILL+TKDLF)-TMEAN)/TKSPN)
                PLASC = (PLA-SENLA)*SPANPOS
                PLASCSUM = PLASCSUM + PLASC
                WRITE (Message(1),899)
     &           (TKILL+TKDLF),(TMIN+TMAX)/2.0,HSTAGE,
     &           (PLA-SENLA)*PLTPOP*0.0001,
     &           (PLA-SENLA)*PLTPOP*0.0001*SPANPOS
                CALL WARNING(1,'CSCRP',MESSAGE)
                WRITE (Fnumwrk,*) ' '
                WRITE (Fnumwrk,893) yeardoy
 893            FORMAT (' Leaf kill on ',I7)
                WRITE (Fnumwrk,899)
     &           (TKILL+TKDLF),(TMIN+TMAX)/2.0,HSTAGE,
     &           (PLA-SENLA)*PLTPOP*0.0001,
     &           (PLA-SENLA)*PLTPOP*0.0001*SPANPOS
 899            FORMAT (
     &           ' TKLF =',F5.1,1X,'TMEAN =',F5.1,1X,
     &           'HSTAGE =',F5.1,1X,'LAI =',  F5.3,1X,'LOSS =',F6.3)
              ENDIF

              ! Tiller kill
              TNUMLOSS = 0.0
              IF (TNUM.GT.1.0.AND.TMEAN.LT.(TKILL+TKDTI)) THEN
                SPANPOS = AMIN1(1.0,((TKILL+TKDTI)-TMEAN)/TKSPN)
                TNUMLOSS = (TNUM-1.0)*SPANPOS
                IF (TNUMLOSS.GT.0.0)THEN
                  WRITE (Message(1),900)
     &            (TKILL+TKDTI),(TMIN+TMAX)/2.0,HSTAGE,TNUM,TNUMLOSS
                  CALL WARNING(1,'CSCRP',MESSAGE)
                  WRITE (Fnumwrk,*) ' '
                  WRITE (Fnumwrk,900)
     &            (TKILL+TKDTI),(TMIN+TMAX)/2.0,HSTAGE,TNUM,TNUMLOSS
                  CALL WARNING(1,'CSCRP',MESSAGE)
                ENDIF
 900             FORMAT (' Tiller kill',
     &           ' TKTI =',F5.1,1X,'TMEAN =',F5.1,1X,
     &           'HSTAGE =',F5.1,1X,'TNO =',  F5.2,1X,'LOSS =',F5.2)
              ENDIF

              ! Plant kill
              IF (TMEAN.LT.TKILL) THEN
                SPANPOS = AMIN1(1.0,(TKILL-TMEAN)/TKSPN)
                PLTLOSS = PLTPOP*SPANPOS
                IF (PLTPOP-PLTLOSS.GE.0.05*PLTPOPP) THEN
                  WRITE (Message(1),901)
     &            TKILL,(TMIN+TMAX)/2.0,HSTAGE,PLTPOP,PLTLOSS
 901              FORMAT (' Plant kill',
     &             ' TKILL =',F5.1,1X,'TMEAN =',F5.1,1X,
     &             'HSTAGE =',F5.1,1X,'PLNO =',  F5.1,1X,'LOSS =',F5.1)
                  CALL WARNING(1,'CSCRP',MESSAGE)
                  WRITE(Fnumwrk,*)' '
                  WRITE(Fnumwrk,991)' Plant kill on ',YEARDOY
 991              FORMAT(A15,I7)                 
                  WRITE(fnumwrk,902)
     &            TKILL,(TMIN+TMAX)/2.0,HSTAGE,PLTPOP,PLTLOSS
 902              FORMAT (
     &             ' TKILL =',F5.1,1X,'TMEAN =',F5.1,1X,
     &             'HSTAGE =',F5.1,1X,'PLNO =',  F5.1,1X,'LOSS =',F5.1)
                ELSE
                  CFLFAIL = 'Y'
                  WRITE (Message(1),1100)
     &            TKILL,(TMIN+TMAX)/2.0,HSTAGE,PLTPOP,PLTLOSS
 1100             FORMAT (' Kill>95% ',
     &             ' TKILL =',F5.1,1X,'TMEAN =',F5.1,1X,
     &             'HSTAGE =',F5.1,1X,'P# =',F5.1,1X,'LOSS =',F5.1)
                  CALL WARNING(1,'CSCRP',MESSAGE)
                  WRITE(Fnumwrk,*)' '
                  WRITE(Fnumwrk,992)' Plant kill > 95% on ',YEARDOY
 992              FORMAT(A21,I7)                 
                  WRITE(fnumwrk,1101)
     &            TKILL,(TMIN+TMAX)/2.0,HSTAGE,PLTPOP,PLTLOSS
 1101             FORMAT (
     &             ' TKILL =',F5.1,1X,'TMEAN =',F5.1,1X,
     &             'HSTAGE =',F5.1,1X,'P# =',F5.1,1X,'LOSS =',F5.1)
                ENDIF
              ENDIF

              ! If cold kill, other senescence not calculated
              IF (PLASC.LE.0.0) THEN
              
                ! Leaf senescence - phyllochron (really thermal time) driven
                LAPSTMP = 0.0
                IF (CUMDU+DU.LE.LGPHASEDU(2)) THEN
                  DO L = 1,LNUMSG
                    IF (LAGEDU(L)+DULF.LE.LLIFATT+LLIFGTT) EXIT
                    IF (LAP(L)-LAPS(L).GT.0.0) THEN
                      LAPSTMP = AMIN1((LAP(L)-LAPS(L)),LAP(L)/LLIFSTT
     &                 *AMIN1((LAGEDU(L)+DULF-(LLIFATT+LLIFGTT)),
     &                 DULF))
                      LAPS(L) = LAPS(L) + LAPSTMP
                      PLASP = PLASP + LAPSTMP
                    ENDIF
                  ENDDO
                ENDIF

                ! Leaf senescence - final triggers
                IF (RSTAGEFS.LE.0.0) THEN
                  IF (CUMDU+DU.GE.LSPHSDU) THEN
                    RSTAGEFS = LSPHS
                    FSDU = LSPHSDU
                  ENDIF
                  IF (ISWNIT.NE.'N') THEN
                   LNCSENF = LNCM + NFSF * (LNCX-LNCM)
                   IF (CUMDU.GE.PSTART(MSTG-1).AND.LANC.LE.LNCSENF) THEN
                     RSTAGEFS = RSTAGE
                     FSDU = CUMDU
                     WRITE(Message(1),'(A34)')
     &                'Final senescence trigger(Nitrogen)'
                     CALL WARNING(1,'CSCRP',MESSAGE)
                     WRITE(Fnumwrk,*)' '
                     WRITE(Fnumwrk,'(A43,F6.1)')
     &                ' Final senescence trigger(Nitrogen). Stage:'
     &                ,gstage
                     WRITE(Fnumwrk,'(A42,2F6.3)')
     &                ' N factor,N conc.for final senescence      ',
     &                nfsf,lncsenf
                     WRITE(Fnumwrk,'(A42,3F6.3)')
     &                ' N conc; actual,min,max                    ',
     &                lanc,lncm,lncx
                   ENDIF
                  ENDIF 
                  IF (ISWWAT.NE.'N') THEN
                   IF (CUMDU.GE.PSTART(MSTG-1).AND.WUPR.LE.WFSF) THEN
                     RSTAGEFS = RSTAGE
                     FSDU = CUMDU
                     WRITE(Message(1),'(A32)')
     &                'Final senescence trigger(Water) '         
                     CALL WARNING(1,'CSCRP',MESSAGE)
                     WRITE(Fnumwrk,*)' '
                     WRITE(Fnumwrk,'(A33)')
     &                ' Final senescence trigger(Water) '         
                   ENDIF
                  ENDIF
                  ! Determine duration of final senescence phase
                  IF (FSDU.GT.0.0) THEN
                    PDFS = LSPHEDU - FSDU
                  ELSE
                    PDFS = LSPHEDU
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
                  IF (PLA-SENLA.GT.0.0.AND.WUPR.LT.WFSU)
     &              PLASW = AMAX1(0.0,AMIN1(
     &                  (PLA-SENLA)-PLAS,(PLA-SENLA)*LLOSA))
                ENDIF
                IF (ISWNIT.NE.'N') THEN
                  LNCSEN = LNCM + NFSU * (LNCX-LNCM)
                  IF (PLA-SENLA.GT.0.0.AND.LANC.LT.LNCSEN)
     &              PLASN = AMAX1(0.0,AMIN1(
     &              (PLA-SENLA)-PLAS,(PLA-SENLA)*LLOSA))
                ENDIF
                PLASS = PLASW + PLASN    ! Loss because of stress

                ! Tiller death - physiological
                TILWTR = 1.0
                TILWT = 0.0
                TNUMD = 0.0
                ! Originally no tdsf
                  TILSW = G3 * (CUMDU+DU/2.0)/PSTART(MSTG)
     &                     * (1.0+((1.0-AMIN1(WFT,NFT))*TDSF))
                ! LAH JAN 2009 ADDED STRESSES TO INCREASE 
                !                        TARGET SIZE->MORE DEATH
                IF (TNUM.GT.0.0)
     &           TILWT = (LFWT+STWT+RSWT+GRWT+CARBOT/2.0)/TNUM
                IF (TILSW.GT.0.0) TILWTR = TILWT/TILSW
                IF (CUMDU+DU.GT.TDPHSDU.AND.CUMDU.LT.TDPHEDU) THEN
                  TNUMD = AMAX1(0.0,
     &             (TNUM-1.)*(1.-TILWTR)*TT/STDAY*(TDFAC/100.0))
                   ! 100.0 because tdfac read as a percentage 
                   ! CSceres TNUMD = AMAX1(0.0,
                   ! (TNUM-1.0)*(1.0-RTSW)*TT*(TDFAC/100.0))
                   ! 
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
                  SENLFG = AMIN1(LFWT*LWLOS,(AMAX1(0.0,         
     &            (LFWT*(PLAS/(PLA-SENLA))*LWLOS))))
                  SENLFGRS = AMIN1(LFWT*(1.0-LWLOS),(AMAX1(0.0,         
     &            (LFWT*(PLAS/(PLA-SENLA))*(1.0-LWLOS)))))
                ENDIF
              ENDIF

              ! Stem senescence
              ! LAH JAN2010 NEED TO MAKE SURE IS LINKED TO ACCELERATED 
              ! SENESCENCE OF LEAVES, AND TO STEM AREA
              !  Start may be accelerated,same rate as normal
              SENSTFR = 0.0
              SENSTG = 0.0
              SENSTGRS = 0.0
              IF (SSPHSDU.GT.0.0.AND.SSPHEDU.GT.0.0) THEN
                IF (CUMDU+DU.GT.SSPHSDU+(FSDU-LSPHSDU)) THEN
                  IF (CUMDU.GT.SSPHS+(FSDU-LSPHSDU)) THEN
!                 IF (CUMDU+DU.GT.SSPHSDU) THEN
!                  IF (CUMDU.GT.SSPHSDU) THEN
                    IF ((SSPHEDU-SSPHSDU).GT.0.0) SENSTFR = 
     &              AMAX1(0.0,AMIN1(1.0,DU/(SSPHEDU-SSPHSDU)))
                  ELSE
                   IF ((SSPHSDU-SSPHEDU).GT.0.0)
     &               SENSTFR = AMAX1(0.0,AMIN1(1.0,
     &                 ((CUMDU+DU)-SSPHSDU)/
     &                             (SSPHEDU-SSPHSDU)))
                  ENDIF
                  ! LAH JAN 2010 NB No weight loss from stem. 
                  ! SENSTFR used currently for area only
                  !IF (SENSTFR.GT.0.0) SENSTG = STWT*SENSTFR
                ENDIF
              ENDIF  
              
              IF (ISWNIT.NE.'N') THEN
                ! NB. N loss has a big effect if low N
                IF (PLASC.GT.0.0) THEN   ! When leaf kill
                  SENNLFG = AMIN1(LEAFN,SENLFG*LANC) ! Loss functional N
                  SENNRS = AMIN1(RSN,RSN*PLASC/(PLA-SENLA)) ! Loss rs N
                ELSE   ! When normal senescence .. N>minimum to reserves
                    SENNLFG = AMAX1(0.0,AMIN1(LEAFN,
     &                             (SENLFG+SENLFGRS)*LNCM))
                    SENNLFGRS = AMAX1(0.0,AMIN1(LEAFN-SENNLFG,
     &                               (SENLFG+SENLFGRS)*(LANC-LNCM)))
                ENDIF

                IF (SANC.GT.0.0) SSENF = (1.0-((SANC-SNCM)/SANC))
                SENNSTG = SENSTG*SANC*SSENF
                SENNSTGRS = SENSTG*SANC*(1.0-SSENF)
                IF (SENNSTG+SENNSTGRS.GT.STEMN) THEN
                  WRITE(Message(1),'(A28)')
     &              'N removal from stem > stem N'
                  CALL WARNING(1,'CSCRP',MESSAGE)
                  WRITE(Fnumwrk,*)' '
                  WRITE(Fnumwrk,'(A29)')
     &              ' N removal from stem > stem N'
                  SENNSTG = STEMN-SENNSTGRS
                ENDIF
              ENDIF

!-----------------------------------------------------------------------
!             Calculate overall senescence loss from tops
!-----------------------------------------------------------------------

              SENFR = 1.0
              SENTOPLITTERG = 0.0
              SENTOPLITTERGGF = 0.0
              IF (DU.GT.0.0) SENFR =
     &         1.0 - AMAX1(0.0,AMIN1(1.0,(CUMDU+DU-LRETSDU)/DU))
              SENTOPLITTERG = (SENLFG+SENSTG+SENRS)*SENFR
              ! Following for checking purposes only
              IF (CUMDU.GE.SGPHASEDU(2).AND.CUMDU.LT.PSTART(MSTG))
     &         SENTOPLITTERGGF = (SENLFG+SENSTG+SENRS)*SENFR

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
              CARBOTMPR = AMAX1(0.0,(PARMJFAC*SRAD)*PARU*CO2FP*TFP
                ! Note possible change.  LAH
!     &         * WFP * NFP * RSFP * VPDFP * SLPF)
     &         * AMIN1(WFP,NFP,RSFP,VPDFP,SLPF))
              CARBOBEGR = CARBOTMPR * PARI / PLTPOP

              ! Modified conventional using internal CO2 (I)
              CARBOTMP = AMAX1(0.,PARMJFAC*SRAD*PARU*TFP*NFP*RSFP)
              ! Calculate for no water stress for WFPI determination
              CARBOTMPI = CARBOTMP
              CO2INTPPMP = CO2
              DO L = 1,20
                CO2INT = CO2AIR - CARBOTMPI * 
     &          (RATM+RCROP*RLFC/RLF*WFP*(1.0*(1.0-WFP)))*1.157407E-05
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
                CO2INT = CO2AIR - CARBOTMPI * (RATM+RCROP*RLFC/RLF*WFP*
     &          (1.*(1.-WFP)))*1.157407E-05
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
              CARBOBEGI = CARBOTMPI * SLPF * PARI / PLTPOP * CARBOBEGIA

              ! Alternate method using resistances as per Monteith (M)
              ! Calculate photosynthetic efficiency
              ! Use 10 Mj.m2.d PAR to establish quantum requirement
              PHOTQR = (CO2AIR/(10.0*PARU)-
     &         ((RATM+RCROP)*1.157407E-05))* 
     &         (10.0*30.0)/(CO2AIR*MJPERE) ! 30 = MW Ch2o
              RM = CO2AIR/(((SRAD*PARMJFAC/MJPERE)/PHOTQR)*30.0)
              CARBOTMPM = AMAX1(0.,
     &         (CO2AIR/((RATM+RCROP*RLFC/RLF*WFP*(1.*(1.-WFP)))*
     &         1.157407E-05+RM))*TFP*NFP*RSFP)
              CARBOBEGM = CARBOTMPM * SLPF * PARI / PLTPOP

              ! Select method depending on choice in CONTROL FILE
              IF (MEPHS.EQ.'R') CARBOBEG = CARBOBEGR  ! PAR conversion
              IF (MEPHS.EQ.'M') CARBOBEG = CARBOBEGM  ! Monteith 
              IF (MEPHS.EQ.'I') CARBOBEG = CARBOBEGI  ! Internal CO2

!-----------------------------------------------------------------------
!             C available to roots (minimum) and stem
!-----------------------------------------------------------------------

              IF (PSTART(MSTG).GT.0) THEN
                PTF = AMIN1(PTFMX,
!    &           PTFMN+((PTFMX-PTFMN)*((CUMDU+DU/2.0))/PSTART(MSTG)))
     &           PTFMN+((PTFMX-PTFMN)*
     &                 ((CUMDU+DU/2.0))/PSTART(INT(PTFXS))))
              ELSE
                PTF = (PTFMX+PTFMN)/2.0
              ENDIF
              ! Partition adjustment for stress effects
              PTF = AMIN1(PTFMX,PTF-PTFA*(1.0-AMIN1(WFG,NFG)))
              CARBOR = AMAX1(0.0,(CARBOBEG+CARBOADJ))*(1.0-PTF)
              CARBOT = AMAX1(0.0,(CARBOBEG+CARBOADJ)) - CARBOR

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
!                Grain set and potential growth
!-----------------------------------------------------------------------

              GROGRP = 0.0
              GROGRPA = 0.0
              ! If in graingrowth phase
              IF(CUMDU+DU.GT.GGPHASEDU(1))THEN
              ! Just entering lag phase
              IF(CUMDU.LE.GGPHASEDU(1).AND.CUMDU+DU.GE.GGPHASEDU(1))THEN
                IF (DU.GE.0.0) THEN
                  ADAYEFR = (GGPHASEDU(1)-CUMDU)/DU
                ELSE 
                  ADAYEFR = 0.0
                ENDIF 
                GNOPD = GNOWTS*((LFWT+STWT+RSWT)+ADAYEFR*CARBOT)
                ! Based on radiation. Experimental
                IF (GNOWTS.LT.5.0) THEN
                  GNOPD = GNOWTS*SRADPAV(5)
                  WRITE(FNUMWRK,*)' '
                  WRITE(FNUMWRK,'(A50)')
     &             ' GRAIN NUMBER BASED ON RADIATION IN PHASE 5       '
                WRITE(FNUMWRK,'(A35,3F6.1)')
     &          '  Radiation average,Coefficient    ',
     &          sradpav(5),gnowts             
                ENDIF
                
                ! Tiller and canopy weights at grain set
                TILWTGS = TILWT
                CWADGS = CWAD
                
                ! Possible strees factor for grain set
                STRESS20GS = STRESS20N*STRESS20W
                
                ! Kernel number adjustment based on phase(5) radiation 
                IF (SRADPAV(5).GT.GNORT) THEN
                  GNOPAS = GNOPD+GNOPD*((SRADPAV(5)-GNORT)*GNORF)
                ELSE
                  GNOPAS = GNOPD 
                ENDIF  
                WRITE(FNUMWRK,*)' '
                WRITE(FNUMWRK,'(A50)')
     &          ' GRAIN NUMBER ADJUSTMENT                          '
                WRITE(FNUMWRK,'(A42,F6.2)')
     &            '  Grain number adjustment factor (fr/MJ)  ',gnorf
                WRITE(FNUMWRK,'(A42,F6.2)')
     &            '  Adjustment threshold (MJ/m2.d)          ',gnort
                WRITE(FNUMWRK,'(A35,3F6.1)')
     &          '  20 day stress averages N,H2O,Min ',
     &          stress20n,stress20w,stress20
                WRITE(FNUMWRK,'(A35,2F6.1)')
     &          '  Radiation averages,20d,phase5    ',
     &          srad20,sradpav(5)               
                WRITE(FNUMWRK,'(A48,2I6)')
     &          '  Grain#/plant before,after radiation adjustment   '
     &          ,NINT(GNOPD),NINT(GNOPAS)    
                GNOPD = GNOPAS
                
                ! Potential kernel size adjustment
                WRITE(FNUMWRK,'(A22)')
     &          ' GRAIN SIZE ADJUSTMENT'

                IF (TMEANPAV(5).GT.GWTAT) THEN
                  GWTA = GWTS - GWTS*((TMEANPAV(5)-GWTAT)*GWTAF)
                ELSE
                  GWTA = GWTS 
                ENDIF  
                
                WRITE(FNUMWRK,'(A42,F6.2)')
     &            '  Grain size adjustment factor (fr/oC)    ',gwtaf
                WRITE(FNUMWRK,'(A42,F6.2)')
     &            '  Adjustment threshold (oC)               ',gwtat
                WRITE(FNUMWRK,'(A42,F6.2)')
     &            '  Temperature during phase 5 (oC)         ',
     &            tmeanpav(5)
                WRITE(FNUMWRK,'(A42,2F6.1)')
     &            '  Grain size before,after adjustment (mg) '
     &          ,GWTS,GWTA    
     
                ! Kernel growth rates in lag,linear,end phases
                G2A(1) =(GWTA*GWLAGFR)/(GGPHASEDU(2)-GGPHASEDU(1))
                G2A(2) =(GWTA*(GWLINFR-GWLAGFR))/
     &           (GGPHASEDU(3)-GGPHASEDU(2))
                G2A(3) =(GWTA*(1.0-GWLINFR))/(GGPHASEDU(4)-GGPHASEDU(3))
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
                IF (TMAX.GE.TKGF) THEN
                  CFLFAIL = 'Y'
                  WRITE(FNUMWRK,'(A47)')
     &              ' Premature maturity because of high temperature'
                  WRITE(FNUMWRK,'(A9,F6.1,A13,F6.1)')
     &              '  TMAX = ',TMAX,' Fail temp = ',TKGF
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
              
              ! If just ended leaf growth phase
              IF (CUMDU.GT.LGPHASEDU(2).AND.FLDAP.LE.0) THEN
                FLDAP = DAP-1
              ENDIF

              ! If in leaf growth phase
              IF (CUMDU.GE.LGPHASEDU(1).AND.CUMDU.LT.LGPHASEDU(2)) THEN
                ! Potential leaf sizes
                IF (LNUMSG.LT.LNUMX) THEN
                  IF (LAFSWITCH.LE.0) THEN
                    LAPOTX(LNUMSG+1) = LAPOTX(LNUMSG)*(1.0+LAFV)
                  ELSE  
                    LAPOTX(LNUMSG+1) = LAPOTX(LNUMSG)*(1.0+LAFR)
                  ENDIF
                ENDIF    
                IF (LAPOTX(LNUMSG+1).GT.LAXS) LAPOTX(LNUMSG+1) = LAXS

                IF (RSTAGE.GE.LAFST) THEN
                  IF(LNUMSG.GT.0 .AND. LAFSWITCH.LE.0.0) THEN
                  LAFSWITCH = LNUMSG
                  WRITE(fnumwrk,*) '    '
                  WRITE(fnumwrk,*)
     &              'Leaf size increment factor changed ',yeardoy
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
                ! Tilip is the # of phyllochrns that elapse before a 
                ! new tiller reaches it's standard size
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
                  GROLS = AMAX1(0.,AMIN1(GROLSP,CARBOT-GROGRPA))
                
                  IF (GROLSP.GT.0.0.AND.GROLS.LT.GROLSP) THEN
                    ! Leaf weight increase from seed reserves
                    ! LAH Takes all that needed. No restriction!
                    CARBOLSD = AMIN1((GROLSP-GROLS),SEEDRSAV)
                    SEEDRSAV = SEEDRSAV - CARBOLSD
                    GROLS = GROLS + CARBOLSD
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
                    WRITE(Fnumwrk,*)' '
                    WRITE(Fnumwrk,'(A39,I7)')
     &               ' Root material used for leaf growth on ',yeardoy
                    WRITE(Fnumwrk,
     &               '(A5,F6.3,A12,F3.1,A8,F7.4,A7,F7.4,A9,F7.4)')
     &               ' LAI ',lai,
     &               ' Shoot/root ',shrtd,
     &               ' Grolsp ',grolsp,' Grols ',grols,
     &               ' Grolfrt ',grolfrt
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
     &           GROST = AMAX1(0.,CARBOT-GROGRPA)*(1.0-(RSPCA/100))
                 ! RSPCA is the % of assim going to reserves
                 ! May need to have this change as stem growth proceeds
              ENDIF
              ! Chaff (In balance with stem growth). 
              ! NB. Chaff is not just part of stem
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

              RTWTGRS = 0.0   ! Not calculated until later
              GRORS = 
     &         CARBOT+CARBOLSD+GROLFRT-GROLF-GROST-GROCH-GROGRPA
              ! Check if RSWT -ve (Generally v.small computer error)
              ! If so adjust growth aspect
              RSWTTMP = RSWT+GRORS+GRORSGR-SENRS-RSWPH-RTWTGRS
              IF (RSWTTMP.LT.-1.E-6) THEN ! Reduce growth 
                GROST = AMAX1(0.0,GROST-ABS(RSWTTMP))
                GRORS =
     &           CARBOT+CARBOLSD+GROLFRT-GROLF-GROST-GROCH-GROGRPA
                RSWTTMP = RSWT+GRORS+GRORSGR-SENRS-RSWPH-RTWTGRS
                GROLF = AMAX1(0.0,GROLF-ABS(RSWTTMP))
                GRORS =
     &           CARBOT+CARBOLSD+GROLFRT-GROLF-GROST-GROCH-GROGRPA
                RSWTTMP = RSWT+GRORS+GRORSGR-SENRS-RSWPH-RTWTGRS
                GROGRPA = AMAX1(0.0,GROGRPA-ABS(RSWTTMP))
                GRORS =
     &           CARBOT+CARBOLSD+GROLFRT-GROLF-GROST-GROCH-GROGRPA
                RSWTTMP = RSWT+GRORS+GRORSGR-SENRS-RSWPH-RTWTGRS
              ENDIF
              GRORS = 
     &          CARBOT+CARBOLSD+GROLFRT+SENLFGRS
     &                -GROLF-GROST-GROCH-GROGRPA

              ! Reserves to ROOT if conc too great (overflow!)
              RTWTGRS = 0.0
              ! Determine potential new concentration
              IF (LFWT+GROLF+STWT+CHWT+GROST+GROCH.GT.0.0) TVR1 = ! Conc
     &          (RSWT+GRORS-SENRS)/
     &          ((LFWT+GROLF-SENLFG-SENLFGRS)
     &          +(STWT+GROST+CHWT+GROCH)+(RSWT+GRORS))
              IF(TVR1.LT.0.0.AND.TVR1.GT.-1.0E-07) TVR1 = 0.0
              IF (TVR1.GT.RSPCX/100.0) THEN   ! If potential>max        
                TVR2 = RSWT+GRORS-SENRS       ! What rswt could be
                TVR3 =                        ! What rswt should be 
     &           ((RSPCX/100.0)
     &           *(LFWT+GROLF-SENLFG-SENLFGRS
     &           +STWT+CHWT+GROST+GROCH))
     &           /(1.0-(RSPCX/100.0))
                RTWTGRS = (TVR2 - TVR3) 
                ! Determine FINAL new concentration
                IF (LFWT+GROLF+STWT+CHWT+GROST+GROCH.GT.0.0) TVR5 = 
     &            (RSWT+GRORS-SENRS-RTWTGRS)/
     &            ((LFWT+GROLF-SENLFG-SENLFGRS)
     &            +(STWT+GROST+CHWT+GROCH)
     &            +(RSWT+GRORS-SENRS-RTWTGRS))
              ENDIF
              
              IF (RSTAGE.GE.8.2) RTWTGRS = 0.0
              ! No overflow if fillint grain

              GRORSPRM = 0.0
              GRORSPM = 0.0
              IF (LSPHEDU.GT.PSTART(MSTG).AND.
     &         CUMDU+DU.GT.PSTART(MSTG).AND.
     &         CUMDU.LT.PSTART(MSTG)) THEN
                GRORSPRM =
     &           (CARBOT+CARBOLSD-GROLF-GROST)*TIMENEED-GROGRPA
                GRORSPM = GRORS - GRORSPRM
              ELSEIF (CUMDU.GE.PSTART(MSTG)) THEN
                GRORSPM = CARBOPM
              ENDIF

!-----------------------------------------------------------------------
!             Tiller number increase
!-----------------------------------------------------------------------

              TNUMG = 0.0
              IF (LNUM.GT.TI1LF.AND.LNUM.GT.0.0) THEN
                IF (LNUM.LT.ti1lf+3) THEN    ! Fibonacci factors
                 tnumiff=1.0
                ELSEIF(LNUM.GE.ti1lf+3 .AND. LNUM.LT.ti1lf+4) THEN
                 tnumiff=1.5
                ELSEIF(LNUM.GE.ti1lf+4 .AND. LNUM.LT.ti1lf+5) THEN
                 tnumiff = 1.5     ! tnumiff=3.0
                ELSEIF(LNUM.GE.ti1lf+5 .AND. LNUM.LT.ti1lf+6) THEN
                  tnumiff = 1.5     ! tnumiff=4.0
                ELSEIF(LNUM.GE.ti1lf+6 .AND. LNUM.LT.ti1lf+7) THEN
                 tnumiff = 1.5     ! tnumiff=6.0
                ENDIF
                IF ((CUMDU+DU).LT.TILPEDU) THEN
                  TNUMG = DULF/PHINT * TNUMIFF * (AMIN1(WFT,NFT))
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
              IF (RSTAGE.LT.7.0) CANHTG = SERX*DU
              !  IF (TT.GT.0.0) CANHTG = 0.5

!=======================================================================
            ENDIF ! End of above-ground growth (after emerged) section
!=======================================================================

!-----------------------------------------------------------------------
!           Root growth and respiration
!-----------------------------------------------------------------------

            RTWTG = 0.0
            RTRESP = 0.0
            RTWTG = (CARBOR+RTWTGRS+SEEDRSAVR)*(1.0-RRESP)
            RTRESP = (CARBOR+RTWTGRS+SEEDRSAVR)*RRESP

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
                IF (WFRGU.GT.0.0)
     &           WFRG = AMAX1(0.0,AMIN1(1.0,(SWPRTIP/WFRGU)))
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
                IF (ISWWAT.NE.'N'.AND.WFRGU.GT.0.0) THEN
                  WFRG = AMIN1(1.0,AMAX1(0.1,SWP(L)/WFRGU))
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

            GRAINNGU = 0.0
            GRAINNGL = 0.0
            GRAINNGR = 0.0
            GRAINNGS = 0.0

            IF (ISWNIT.NE.'N') THEN

              ANDEM = 0.0
              RNDEM = 0.0
              LNDEM = 0.0
              SNDEM = 0.0
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

              ! Grain N demand
              GRAINNDEM = 0.0
              IF (GNOPD.GT.0.0 .AND. CUMDU.LT.PSTART(MSTG)) GRAINNDEM =
              ! lah working may 2014
     &         AMIN1(GROGRPA*(GNPCMX/100.0),TFGN*GROGRP*(GNPCS/100.))
!    &         AMIN1(GROGRPA*(GNPCMX/100.0),
!    &                GNOPD*G2A(0)*(GNPCS/100.)*0.001*TFGN*DU)

              ! Leaf,stem,root N demand
              LNDEM = GROLF*LNCX +
     &            (LFWT-SENLFG-SENLFGRS)*AMAX1(0.0,NTUPF*(LNCX-LANC)) -
     &            GROLFRTN
              SNDEM = AMAX1(0.0,GROST)*SNCX +
     &              (STWT-SENSTG)*AMAX1(0.0,NTUPF*(SNCX-SANC))
              RNDEM = RTWTG*RNCX + 
     &              (RTWT-SENRTG-GROLFRT)*AMAX1(0.0,NTUPF*(RNCX-RANC))
              
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
              RSNUSED = AMIN1(GRAINNDEM+LNDEM+SNDEM+RNDEM,RSN)

              ! N uptake needed 
              ANDEM = PLTPOP*10.0*
     &        (GRAINNDEM+LNDEM+SNDEM+RNDEM-SEEDNUSE-RSNUSED)

              ! Original from CSM with some 'modification'.  
              ! RNUMX = RTNO3,RTNH4 = N uptake/root length (mgN/cm,.006)
              ! RNO3U,RNH4U  = Nitrogen uptake (kg N/ha)
              ! RNUMX = 0.006    
              WFNU = 1.0
              NUPAP = 0.0
              RNO3U = 0.0
              RNH4U = 0.0
              IF (CFLNOUTPUTS.EQ.'Y') THEN
                WRITE(FNUMWRK,*)' '
                WRITE(FNUMWRK,*)
     &           'Potential uptake and limitants  ',YearDoy
                WRITE(FNUMWRK,'(A48,3F7.2)')
     &           '  Previous day N demand,uptake,shortage (kg/ha) ',
     &           andem,nupap,AMAX1(0.0,andem-nupap)
                WRITE(FNUMWRK,'(A48,3F3.2)')
     $           '  Adjustment factors for Water,N.Conc,Cultivar: ',
     &           nupwf,nupnf,nupcf
                WRITE(fnumwrk,'(A38,A39)')
     &            '   Layer   BLayer Soiln(g/Mg)  (kg/ha)',
     &            '  Pot.uptake   WFac  NConcFac       RLV'
              ENDIF
              DO L=1,NLAYR
                IF (RLV(L) .GT. 0.0) THEN
                  NLAYRROOT = L
                  ! N concentration effects
                  FNH4 = 1.0-EXP(-0.08*(1.0+NUPNF) * NH4LEFT(L))
                  FNO3 = 1.0-EXP(-0.08*(1.0+NUPNF) * NO3LEFT(L))
                  ! The following limits are not those in NUPTAK,CERES
                  ! Here set to 0 when NO3,NH4 < minimum,
                  ! In NUPTAK when FNO3,FNH4 < 0.04
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
                  TVR2 = (1.0+NUPWF)*(WFNU*WFNU)
                  RFAC = RLV(L) * TVR2 * DLAYR(L) * 100.0
                  RNO3U(L) = RFAC * FNO3 * RTNUP*(1.0+NUPCF)
                  RNH4U(L) = RFAC * FNH4 * RTNUP*(1.0+NUPCF)
                  IF (CFLNOUTPUTS.EQ.'Y') THEN
                    WRITE(Fnumwrk,'(I7,F10.3,6F10.3)')
     &               l,blayr(l),(no3left(l)+nh4left(l)),sno3(l)+snh4(l),
     &               (rno3u(l)+rnh4u(l)),tvr2,fno3,rlv(l) 
                  ENDIF
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
              ! (Assumes all seed gone by time of grain filling)
              IF (RTWT.GT.0.0.AND.ISWNIT.NE.'N') THEN
                SEEDNUSE2 = AMAX1(0.0,
     &           AMIN1(SEEDN-SEEDNUSE,GRAINNDEM+LNDEM+SNDEM+
     &           RNDEM-RSNUSED-SEEDNUSE-NUPD,SEEDNI/SDDUR*(TT/STDAY)))
              ELSE
                SEEDNUSE2 = 0.0
              ENDIF
              SEEDNUSE = SEEDNUSE + SEEDNUSE2

              ! N available for distribution
              NULEFT = SEEDNUSE+RSNUSED+NUPD

              ! Distribute N to grain,leaves,stem,root
              LNUSE = 0.0
              SNUSE = 0.0
              RNUSE = 0.0
              ! 1.For grain
              GRAINNGU = AMIN1(NULEFT,GRAINNDEM)
              NULEFT = NULEFT - GRAINNGU
              ! 2.For new leaf at minimum
              LNUSE(1) = AMIN1(NULEFT,GROLF*LNCM)
              NULEFT = NULEFT - LNUSE(1)
              ! 3.For new root at minimum
              RNUSE(1) = AMIN1(NULEFT,RTWTG*RNCM)
              NULEFT = NULEFT - RNUSE(1) 
              ! 4.For new stem at minimum     
              SNUSE(1) = AMIN1(NULEFT,GROST*SNCM)
              NULEFT = NULEFT - SNUSE(1)
              ! 5.For leaf growth and topping-up (N to leaves first)
              LNUSE(2) = AMIN1(NULEFT,LNDEM-LNUSE(1))  
              NULEFT = NULEFT - LNUSE(2)
              ! 6.For distribution between root,stem
              IF (NULEFT.GT.0.0.AND.
     &         SNDEM-SNUSE(1)+RNDEM-RNUSE(1).GT.0.0) THEN
                IF (NULEFT.GE.
     &           (SNDEM-SNUSE(1))+(RNDEM-RNUSE(1))) THEN
                  SNUSE(2) = SNDEM-SNUSE(1)
                  RNUSE(2) = RNDEM-RNUSE(1)
                  NULEFT = NULEFT - SNUSE(2) - RNUSE(2)
                ELSE 
                  SNUSE(2) = NULEFT * (SNDEM-SNUSE(1))/
     &                   ((SNDEM-SNUSE(1))+(RNDEM-RNUSE(1)))
                  RNUSE(2) = NULEFT * (RNDEM-RNUSE(1))/
     &                   ((SNDEM-SNUSE(1))+(RNDEM-RNUSE(1)))
                  NULEFT = NULEFT - SNUSE(2) - RNUSE(2)
                ENDIF   
              ENDIF
              LNUSE(0) = LNUSE(1) + LNUSE(2) 
              SNUSE(0) = SNUSE(1) + SNUSE(2)
              RNUSE(0) = RNUSE(1) + RNUSE(2) 

              ! N Pools available for re-mobilization
              ! (Labile N increases during grain fill)
              ! LAH Nuselim allows labile to go to 100%!
              !IF (RSTAGE.GE.8.0.AND.RSTAGE.LE.9.0) THEN
              !  NUSELIM = AMIN1(1.0,RSTAGE-8.0)
              !  NUSEFAC = AMAX1(NUSELIM,(NLABPC/100.0))
              !ELSE  
              NUSEFAC = NLABPC/100.0
              !ENDIF
              NPOOLR = AMAX1 (0.0,
     &         ((RTWT-SENRTG)*(RANC-RNCM)*NUSEFAC))
              NPOOLL = AMAX1 (0.0, 
     &            ((LFWT-SENLFG-SENLFGRS)*(LANC-LNCM)*NUSEFAC))
              NPOOLS = AMAX1 (0.0,
     &         ((STWT-SENSTG)*(SANC-SNCM)*NUSEFAC))

              ! Move N to grain from tops if necessary (not from roots)
              GRAINNGR = 0.0
              GRAINNGL = 0.0
              GRAINNGS = 0.0
              GRAINNDEMLSR = AMAX1
     &           (0.0,(GRAINNDEM-GRAINNGRS-GRAINNGU))
              ! Draw N from stems,then leaves.
              !GRAINNGR = AMIN1(NPOOLR,GRAINNDEMLSR) Taken out RORO7491
              GRAINNGS = AMIN1(NPOOLS,GRAINNDEMLSR-GRAINNGR)
              GRAINNGL = AMIN1(NPOOLL,
     &                            GRAINNDEMLSR-GRAINNGR-GRAINNGS)

              ! Move N to stem from leaves if needed to keep stem at min
              STEMNGL = 0.0
              IF (CUMDU.LT.SGPHASEDU(2)) THEN
                IF (LANC.GT.LNCM.AND.SANC.LT.SNCM) THEN
                  STEMNGL = AMIN1(0.01*LFWT*(LANC-LNCM),
     &            ((SNCM-SANC)*(STWT+GROST)-STEMN-SNUSE(0)))
                  ! The 0.01 is the fraction of leaf N that is available
                  ! to try to maintain stem N at the minimum     
                  IF (STEMNGL.GT.0.0) THEN 
                    WRITE(fnumwrk,'(A34,F7.3)')
     &              ' N moved from leaves to stem      ',stemngl    
                  ENDIF
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
              IF (GNPCMN.LE.0.0) THEN
                GROGR = GROGRPA
              ELSE
                ! Minimum grain N% control
                GRWTTMP = GRWT + GROGRPA
                GRAINNTMP = GRAINN + (GRAINNGU+GRAINNGR+GRAINNGL+
     &            GRAINNGS+GRAINNGRS)
                IF (GRWTTMP > 1.E-6 .AND. 
     &           GRAINNTMP/GRWTTMP*100.0 .LT. GNPCMN) THEN
                  GRWTTMP = GRAINNTMP*(100.0/GNPCMN)
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
            SENWALG(0) = SENTOPLITTERG * PLTPOP*10.0
            SENCALG(0) = SENWALG(0) * 0.4 
            SENLALG(0) =
     &       (SENLFG*LLIGPC/100+SENSTG*SLIGPC/100) * PLTPOP*10.0
            SENNALG(0) = (SENNLFG+SENNSTG) * SENFR * PLTPOP*10.0
            ! Root senescence
            DO L = 1, NLAYR
              SENWALG(L) = RTWTSL(L) * PLTPOP*10.0
              SENNALG(L) = RTNSL(L) * PLTPOP*10.0
              SENCALG(L) = SENWALG(L) * 0.4
              SENLALG(L) = SENWALG(L) * RLIGPC/100.0
              SENWAGS = SENWAGS + SENWALG(L)
              SENCAGS = SENCAGS + SENCALG(L)
              SENLAGS = SENLAGS + SENLALG(L)
              SENNAGS = SENNAGS + SENNALG(L)
            ENDDO

            ! Set established flag (Used to determine if to fail when
            ! seed reserves used)
            IF (ESTABLISHED.NE.'Y'.AND.LAI.GT.0.0) ESTABLISHED = 'Y'

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

          ! Leaves that present at beginning of day
          DO L = 1,LNUMSG
            LAGEDU(L) = LAGEDU(L) + DULF
            LAGEP(L) = LAGEP(L) + LNUMG
          ENDDO
          ! New leavea   
          IF (LNUMG.GT.0.0) THEN
            IF (LNUMSG.LT.LNUMX) THEN
              LAGEP(LNUMSG+1)=LAGEP(LNUMSG+1)+AMAX1(0.0,LNUMG-LNUMNEED)
              LAGEDU(LNUMSG+1) = LAGEDU(LNUMSG+1)+
     &         DULF*AMAX1(0.0,LNUMG-LNUMNEED)/LNUMG
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
              WRITE(Fnumwrk,*)' '
              WRITE(Fnumwrk,'(A36,F4.1,A14)')
     &         ' Leaf weight less than 0! Weight of ',lfwt,
     &         ' reset to zero'
              LFWT = 0.0
            ENDIF
            RSWT = RSWT + 
     &             GRORS + GRORSGR - SENRS - RSWPH - RTWTGRS
            RSWPHC = RSWPHC +  RSWPH
            ! Reserves distribution 
            ! Max concentration in leaves increases through life cycle.
            IF (PSTART(MSTG).GT.0.0) LLRSWT = AMIN1(RSWT,
     &       LFWT*(1.0-LSHFR)*(RSPCLX/100.0)*CUMDU/PSTART(MSTG))
            IF (PSTART(MSTG).GT.0.0) LSHRSWT = AMIN1(RSWT-LLRSWT,
     &       LFWT*LSHFR*(RSPCLX/100.0)*CUMDU/PSTART(MSTG))
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
                WRITE(Fnumwrk,*)' '
                WRITE(Fnumwrk,'(A31,A11,F12.9)')
     &           ' Reserves weight reset to zero.',
     &           'Weight was ',rswt
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
            GRWT = GRWT + GROGR - GWPH
            GWPHC = GWPHC + GWPH
            CHWT = CHWT + GROCH 
            SENTOPLITTER = SENTOPLITTER + SENTOPLITTERG
            SENTOPRETAINED = 
     &        SENTOPRETAINED + (SENLFG+SENSTG+SENRS)*(1.0-SENFR) - DWRPH
            DWRPHC = DWRPHC + DWRPH
            SENCL(0) = SENCL(0) + SENTOPLITTERG*0.4
            SENLL(0) = SENLL(0)
     &       + (SENLFG*LLIGPC/100+SENSTG*SLIGPC/100)*(SENFR)
            RTWT = 0.0
            DO L = 1, NLAYR
              RTWTL(L) = RTWTL(L) + RTWTGL(L) - RTWTSL(L) - RTWTUL(L)
              SENWL(L) = SENWL(L) + RTWTSL(L)
              SENCL(L) = SENCL(L) + RTWTSL(L) * 0.4
              SENLL(L) = SENLL(L) + RTWTSL(L) * RLIGPC/100.0
              ! Totals
              RTWT = RTWT + RTWTL(L)
              SENROOT = SENROOT + RTWTSL(L)
              SENCS = SENCS + RTWTSL(L) * 0.4
              SENLS = SENLS + RTWTSL(L) * RLIGPC/100.0
            END DO
          ENDIF

          SEEDRS = AMAX1(0.0,SEEDRS-CARBOLSD-SEEDRSAVR)
          IF (CFLSDRSMSG.NE.'Y'.AND.SEEDRS.LE.0.0.AND.LNUM.LT.4.0) THEN
            CFLSDRSMSG = 'Y'
            IF (LAI.LE.0.0) THEN
              WRITE (Message(1),'(A41)')
     &          'No seed reserves to initiate leaf growth '
              WRITE (Message(2),'(A33,F8.3,F6.1)')
     &          '  Initial seed reserves,seedrate ',seedrsi,sdrate
              WRITE (Message(3),'(A33,F8.3,F6.1)')
     &          '  Reserves %,plant population    ',sdrspc,pltpop 
              CALL WARNING(3,'CSCRP',MESSAGE)
              WRITE (Fnumwrk,*)' '
              WRITE (Fnumwrk,'(A55,I8)')
     &        ' WARNING  Before emergence but no seed reserves left on',
     &          Yeardoy
              WRITE (Fnumwrk,*)
     &          ' Initial seed reserves,seedrate ',seedrsi,sdrate
              WRITE (Fnumwrk,*)
     &          ' Reserves %                     ',sdrspc    
              WRITE (Fnumwrk,*)
     &          'Should reduce the rate at which reserves are used.'    
              WRITE (Fnumwrk,*)
     &          '(This is done by increasing the period over which '
              WRITE (Fnumwrk,*)
     &          ' reserves are used [SDUR in the species file])'    
              WRITE (Fnumwrk,'(A53)')
     &          ' Program should stop but to allow the run to continue'
              WRITE (Fnumwrk,'(A43)')
     &          ' the initial seed reserves will be doubled.'
              SEEDRS =  SEEDRS + SEEDRSI
              !CFLFAIL = 'Y'  ! Taken out to allow run to continue
            ELSE
              WRITE(Message(1),'(A44,F3.1)')
     &        'Seed reserves all used but leaf number only ',lnum
              WRITE(Message(2),'(A56)')
     &        'For good establishment seed reserves should last to lf 4'
              WRITE(Message(3),'(A55)')
     &        'Maybe seeds too small or specific leaf area set too low'
              CALL WARNING(3,'CSCRP',MESSAGE)
              WRITE(Fnumwrk,*)' '
              WRITE(Fnumwrk,'(A36,I7,A9,F3.1)')
     &        ' WARNING  Seed reserves all used on ',yeardoy,
     &        ' Leaf# = ',Lnum
              WRITE(Fnumwrk,'(A54)')
     &        ' For good establishment reserves should last to leaf 4'
              WRITE(Fnumwrk,'(A56)')
     &        'Maybe seeds too small or specific leaf area set too low'
            ENDIF
          ENDIF


          SEEDUSE = SEEDUSE + CARBOLSD+SEEDRSAVR
          SEEDUSER = SEEDUSER + SEEDRSAVR
          SEEDUSET = SEEDUSET + CARBOLSD
          SEEDRSAV = SEEDRS

          RSWTPM = RSWTPM + GRORSPM
          SENGF = SENGF + SENRTGGF + SENTOPLITTERGGF

          IF (GNOPD.GT.0.0) GWUD = GRWT/GNOPD

          IF ((LFWT+STWT+GRWT+RSWT).GT.0.0) THEN
!            HIAD = GRWT/(LFWT+STWT+GRWT+RSWT+SENTOPRETAINED)
            HIAD = (GRWT/(1.0-GMPCH/100.0))
     &             / (LFWT+STWT+GRWT+RSWT+SENTOPRETAINED)
          ENDIF
          IF (RTWT.GT.0.0)
     &     SHRTD = (LFWT+STWT+GRWT+RSWT+SENTOPRETAINED) / RTWT

!-----------------------------------------------------------------------
!         Calculate reserve concentration
!-----------------------------------------------------------------------

          IF (LFWT+STWT+CHWT.GT.0.0) RSCD = RSWT/(LFWT+STWT+CHWT+RSWT)
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
          SENLALITTER = SENLALITTER + PLAS * SENFR
          SENLARETAINED = SENLARETAINED + PLAS * (1.0-SENFR)
    	    ! Grazed leaf area
          LAPHC = LAPHC + LAPH
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

          IF (TSSTG.LE.0.OR.LLSTG.LE.0) THEN
            LSHAI = (LFWT*LSHFR*LSHAV)*PLTPOP*0.0001
          ELSE
            IF (RSTAGE.LE.TSSTG) THEN
              LSHAI = (LFWT*LSHFR*LSHAV)*PLTPOP*0.0001
            ELSEIF (RSTAGE.GT.TSSTG.AND.RSTAGE.LT.LLSTG) THEN
              LSHAW = LSHAV+((LSHAR-LSHAV)*(RSTAGE-TSSTG))
              LSHAI = (LFWT*LSHFR*LSHAW)*PLTPOP*0.0001
            ELSE
              ! Use RSEN as temporary fix for sheath senescence
              LSHAI = LSHAI * (1.0-(RSEN/100.0)*TT/STDAY)  
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

          TNUM = AMIN1(TILNOX,AMAX1(1.0,TNUM+TNUMG-TNUMD-TNUMLOSS))
          SPNUMHC = SPNUMHC + SPNUMH  ! Harvested
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
            CFLFAIL = 'Y'
            Write (Message(1),'(A23)') 'Plant population < 0.0  '
            Write (Message(2),'(A12,F4.1)') 'Plant loss: ',pltloss      
            WRITE (Message(3),'(A20)') 'Crop failure assumed'
            CALL WARNING(3,'CSCRP',MESSAGE)
            Write (Fnumwrk,*) ' '
            Write (Fnumwrk,'(A23)') ' Plant population < 0.0 '
            Write (Fnumwrk,'(A13,F6.1)') ' Plant loss: ',pltloss      
            WRITE (Fnumwrk,'(A21)') ' Crop failure assumed'
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
          LEAFNEXCESS = 0.0
          IF (LANC.GT.LNCX)
     &      LEAFNEXCESS = (LFWT-SENLFG-SENLFGRS)*(LANC-LNCX)
         
          LEAFN = LEAFN + GROLFRTN + LNUSE(0)
     &          - STEMNGL - GRAINNGL - SENNLFG - SENNLFGRS - LNPH
     &          - LEAFNEXCESS
          LNPHC = LNPHC +  LNPH
          IF (LEAFN.LT.1.0E-10) LEAFN = 0.0
          STEMNEXCESS = 0.0
          IF (SANC.GT.SNCX)
     &      STEMNEXCESS = (STWT-SENSTG-SENSTGRS)*(SANC-SNCX)
          STEMN = STEMN + STEMNGL + SNUSE(0)
     &          - GRAINNGS - SENNSTG - SENNSTGRS - SNPH
     &          - STEMNEXCESS
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
          ! CHECK IF LANC > LNCX
          ROOTNEXCESS = 0.0
          IF (RANC.GT.RNCX)
     &      ROOTNEXCESS = (RTWT-(SENWALG(L)/(PLTPOP*10.0)))*(RANC-RNCX)
          ROOTN = ROOTN + (RNUSE(0)-GRAINNGR-ROOTNS-GROLFRTN)
     &                  - ROOTNEXCESS
          SEEDN = SEEDN - SEEDNUSE
          IF (SEEDN.LT.1.0E-6) SEEDN = 0.0
          GRAINN = GRAINN + GRAINNGU + GRAINNGL + GRAINNGS
     &      + GRAINNGR + GRAINNGRS - GNPH
          RSN = RSN - GRAINNGRS - RSNUSED
     &        - SENNRS - RSNPH + SENNLFGRS + SENNSTGRS
     &        + LEAFNEXCESS + STEMNEXCESS + ROOTNEXCESS
          RSNPHC = RSNPHC +  RSNPH
          IF (LRETSDU.GT.0.0.AND.LRETSDU.LT.CUMDU) THEN
            DEADN = DEADN + SENNLFG + SENNSTG
          ELSE
            SENNL(0) = SENNL(0) + SENNLFG + SENNSTG + SENNRS
          ENDIF

          ! Harvest index for N
          HIND = 0.0
          IF ((LEAFN+STEMN+GRAINN+RSN+DEADN).GT.0.0)
     &     HIND = GRAINN/(LEAFN+STEMN+GRAINN+RSN+DEADN)

!-----------------------------------------------------------------------
!         Update stages
!-----------------------------------------------------------------------

          ! STAGES:Germination and emergence (Gestages)
          ! NB 0.5 factor used to equate to Zadoks)
          GEUCUM = GEUCUM + TTGEM*WFGE
          IF (GEUCUM.LT.PEGD) THEN
            GESTAGE = AMIN1(1.0,GEUCUM/PEGD*0.5)
          ELSE
            IF (PEMRG*SDEPTHU > 1.E-6) THEN 
              GESTAGE = AMIN1(1.0,0.5+0.5*(GEUCUM-PEGD)/(PEMRG*SDEPTHU))
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
              LFGSDU = CUMDU - DU + TT*VF*PPFPE*(GERMFR-EMRGFR)
              LFGSCUMDU = TT*VF*DF*EMRGFR
            ELSE
              LFGSCUMDU = 
     &           AMAX1(0.0,AMIN1(LGPHASEDU(2)-LFGSDU,LFGSCUMDU+DU))
            ENDIF
            IF (LGPHASEDU(2).GT.0.0)
     &       LSTAGE = AMAX1(0.0,AMIN1(1.0,LFGSCUMDU/LGPHASEDU(2)))
            ENDIF

          ! STAGES:Leaf numbers
          LNUM = AMAX1(0.0,(AMIN1(FLOAT(LNUMX-1),(LNUM+LNUMG))))
          
          LNUMSG = INT(LNUM)+1
          IF (LNUM.GE.FLOAT(LNUMX-1)+0.9.AND.RSTAGE.LT.4.0) THEN
            IF (CCOUNTV.EQ.0) THEN
             WRITE (Message(1),'(A35)')
     &       'Maximum leaf number reached on day '
             CALL WARNING(1,'CSCRP',MESSAGE)
             WRITE(Fnumwrk,*)' '
             WRITE (Fnumwrk,'(A36,I8)')
     &       ' Maximum leaf number reached on day ',yeardoy
            ENDIF
            CCOUNTV = CCOUNTV + 1
            IF (CCOUNTV.EQ.50.AND.VREQ.GT.0.0) THEN
              CFLFAIL = 'Y'
              WRITE (Message(1),'(A34)')
     &         '50 days after maximum leaf number '
              WRITE (Message(2),'(A54)')
     &         'Presumably vernalization requirement could not be met '
              WRITE (Message(3),'(A25)')
     &         'Will assume crop failure.'
              CALL WARNING(3,'CSCRP',MESSAGE)
              WRITE (Fnumwrk,*)' '
              WRITE (Fnumwrk,'(A34)')
     &         '50 days after maximum leaf number '
              WRITE (Fnumwrk,'(A54)')
     &         'Presumably vernalization requirement could not be met '
              WRITE (Fnumwrk,'(A25)')
     &         'Will assume crop failure.'
            ENDIF
          ENDIF

          ! STAGES:Apical development - double ridges. 
          !  Factors by calibration from LAMS
          !  Only used for comparison with calc based on spe input
          drf1 = 1.9
          drf2 = 0.058
          drf3 = 3.3
          DRSTAGE = AMAX1(1.1,drf1-drf2*(LNUM-drf3))
          IF (DRDAT.EQ.-99 .AND. RSTAGE.GE.DRSTAGE) THEN
            DRDAT = YEARDOY
            DRDAP = MAX(0,CSTIMDIF(PLYEARDOY,YEARDOY))
            WRITE(fnumwrk,*)' '
            WRITE(fnumwrk,'(A37,3F7.2,I9)')
     &       ' Double ridges. Rstage,Drtage,Leaf#: ',
     &       RSTAGE,DRSTAGE,LNUM,YEARDOY
             ! NB. Experimental. DR occurs at later apical stage when
             !     leaf # less, earlier when leaf # greater (ie.when
             !     early planting of winter type).
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
              ZSTAGE = AMIN1(20.0,10.0 + LNUM)
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

          GSTAGE = ZSTAGE

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
          IF (RSTAGEP.LT.0.0) RSTAGEP = 0.0
          L = INT(RSTAGEP) + 1
          IF (PSDAT(L).LE.0.AND.CUMDU.GE.PSTART(L)) THEN
            PSDAT(L) = YEARDOY
            IF (DU.GT.0.0) PSDAPFR(L)=(PSTART(L)-(CUMDU-DU))/DU
            PSDAPFR(L) = FLOAT(DAP) + PSDAPFR(L)
            PSDAP(L) = DAP
            ! If has jumped a phase 
            ! This was preventing setting ADAT in one case. Taken out. 
            !IF (RSTAGE-RSTAGEP.GT.1.0) THEN
            !  PSDAT(L+1) = YEARDOY
            !  PSDAP(L+1) = DAP
            !ENDIF
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
              TMEAN20ANTH = TMEAN20
              SRAD20ANTH = SRAD20
              IF ((GNOPD*PLTPOP).LT.100.0) THEN
                WRITE (Message(1),'(A44)')
     &           'Very few grains set! Failure as a grain crop'
                WRITE (Message(2),'(A26,F6.1)')
     &           '  Plant population      = ',pltpop
                WRITE (Message(3),'(A26,F6.1)')
     &           '  Above ground  (kg/ha) = '
     &           ,(LFWT+STWT+RSWT)*pltpop*10.0
                WRITE (Message(4),'(A26,F6.1)')
     &           '  Grain number coeff    = ',gnowts
                WRITE (Message(5),'(A26,F6.1)')
     &           '  Leaf area index       = ',lai
                CALL WARNING(5,'CSCRP',MESSAGE)
                Message = ' '
                WRITE(Fnumwrk,*)' '
                WRITE (Fnumwrk,'(A45)')
     &           ' Very few grains set! Failure as a grain crop'
                WRITE (Fnumwrk,'(A27,F6.1)')
     &           '   Plant population      = ',pltpop
                WRITE (Fnumwrk,'(A27,F6.1)')
     &           '   Above ground  (kg/ha) = '
     &           ,(LFWT+STWT+RSWT)*pltpop*10.0
                WRITE (Fnumwrk,'(A27,F6.1)')
     &           '   Grain number coeff    = ',gnowts
                WRITE (Fnumwrk,'(A27,F6.1)')
     &           '   Leaf area index       = ',lai
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
            DEADWTSGE = SENTOPRETAINED
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
            
            WRITE(FNUMWRK,*)' '
            WRITE(FNUMWRK,*)'EMERGENCE ',eyeardoy
            IF (SEEDRSAV.GT.0.0) THEN
            WRITE(FNUMWRK,*)'  Rtwtl(1)/seedrsi  ',rtwtl(1)/seedrsi
            WRITE(FNUMWRK,*)'  Rtwtl(2)/seedrsi  ',rtwtl(2)/seedrsi
            WRITE(FNUMWRK,*)'  Rtwtl(3)/seedrsi  ',rtwtl(3)/seedrsi
            ENDIF
            IF (SEEDNI.GT.0.0) THEN
            WRITE(FNUMWRK,*)'  Rootn/seedni      ',rootn/seedni
            ENDIF
            WRITE(FNUMWRK,*)'  Rtdep            ',rtdep
            WRITE(FNUMWRK,*)'  Rstage,Cumdu     ',rstage,cumdu

          ENDIF

          IF (TILDAT.LE.0.AND.TNUM.GT.1.0) THEN
            TILDAT = YEARDOY
            TILDAP = DAP
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
              IF (INT(RSTAGE).EQ.8)
     &        TMAXGFILL = AMAX1(TMAX,TMAXGFILL) 
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
            DAYLPC = DAYLPC + DAYLT
            TMAXCC = TMAXCC + TMAX
            TMINCC = TMINCC + TMIN
            TMEANCC = TMEANCC + TMEAN
            SRADCC = SRADCC + SRAD
            CO2CC = CO2CC + CO2
            DAYLCC = DAYLCC + DAYLT
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
              TMEANPAV(INT(RSTAGE)) = TMEANPC / PDAYS(INT(RSTAGE))
              SRADPAV(INT(RSTAGE)) = SRADPC / PDAYS(INT(RSTAGE))
              DAYLPAV(INT(RSTAGE)) = DAYLPC / PDAYS(INT(RSTAGE))
              DAYLST(INT(RSTAGE)) = DAYLT
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
              TMEANCAV = TMEANCC / CDAYS
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
              ! Change from lstage,ststage 05/10/11
              LNCX = LNCXS(0) + DSTAGE*(LNCXS(1)-LNCXS(0))
              SNCX = SNCXS(0) + DSTAGE*(SNCXS(1)-SNCXS(0))
              RNCX = RNCXS(0) + DSTAGE*(RNCXS(1)-RNCXS(0))
              LNCM = LNCMN(0) + DSTAGE*(LNCMN(1)-LNCMN(0))
              SNCM = SNCMN(0) + DSTAGE*(SNCMN(1)-SNCMN(0))
              RNCM = RNCMN(0) + DSTAGE*(RNCMN(1)-RNCMN(0))
              ! Original
              ! LNCX = LNCXS(0) + LSTAGE*(LNCXS(1)-LNCXS(0))
              ! SNCX = SNCXS(0) + STSTAGE*(SNCXS(1)-SNCXS(0))
              ! RNCX = RNCXS(0) + DSTAGE*(RNCXS(1)-RNCXS(0))
              ! LNCM = LNCMN(0) + LSTAGE*(LNCMN(1)-LNCMN(0))
              ! SNCM = SNCMN(0) + STSTAGE*(SNCMN(1)-SNCMN(0))
              ! RNCM = RNCMN(0) + DSTAGE*(RNCMN(1)-RNCMN(0))

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
            ! Originally included retained dead matter and reserves
            IF (((LFWT+STWT+RSWT)*PLTPOP*10.0).GT.0.0)
     &       VANC = VNAD/((LFWT+STWT)*PLTPOP*10.0)
            IF (LANC.LT.0.0) THEN 
              WRITE(Message(1),'(A27,F4.1)')
     &         'LANC below 0 with value of ',LANC
              WRITE(Message(2),'(A27,2F5.1)')
     &         'LEAFN,LFWT had values of   ',LEAFN,LFWT
              CALL WARNING(2,'CSCRP',MESSAGE)
              WRITE(Fnumwrk,*)' '
              WRITE(Fnumwrk,'(A28,F4.1)')
     &         ' LANC below 0 with value of ',LANC
              WRITE(Fnumwrk,'(A28,2F5.1)')
     &         ' LEAFN,LFWT had values of   ',LEAFN,LFWT
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
            IF (SEEDRS.GT.0.0) SDNC = SEEDN/(SEEDRS+SDCOAT)
            IF (GRWT.GT.0) GRAINANC = GRAINN/GRWT
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
!         Continuation point if not germinated
!-----------------------------------------------------------------------

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
          IF (DAP.GE.150 .AND. GESTAGE.LT.1.0) THEN
            CFLFAIL = 'Y'
            WRITE (Message(1),'(A41)')
     &       ' No germination within 150 days of sowing '
             CALL WARNING(1,'CSCRP',MESSAGE)
            WRITE (Fnumwrk,*) ' '
            WRITE (Fnumwrk,'(A41)')
     &       ' No germination within 150 days of sowing '
          ENDIF
          IF (IHARI.NE.'A'.AND.MDAT.GE.0.AND.DAP-MDAP.GE.90) THEN
            CFLFAIL = 'Y'
            WRITE (Message(1),'(A32)')'90 days after end of grain fill '
            WRITE (Message(2),'(A21)')'Harvesting triggered.'
            CALL WARNING(2,'CSCRP',MESSAGE)
            WRITE (Fnumwrk,*) ' '
            WRITE (Fnumwrk,'(A32)')'90 days after end of grain fill '
            WRITE (Fnumwrk,'(A21)')'Harvesting triggered.'
          ENDIF
          IF (IHARI.NE.'A'.AND.CUMDU.GE.PSTART(MSTG-1)) THEN
            IF (TT20.LE.-98.0.AND.TT20.LE.0.0) THEN
              CFLFAIL = 'Y'
              WRITE (Message(1),'(A28)') '20day thermal time mean = 0 '
              CALL WARNING(1,'CSCRP',MESSAGE)
              WRITE (Fnumwrk,*) ' '
              WRITE (Fnumwrk,'(A28)') '20day thermal time mean = 0 '
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
            IF (CUMDU.GT.PSTART(MSTG) .AND. CFLHARMSG .NE. 'Y') THEN
              CFLHARMSG = 'Y'
              WRITE(Message(1),'(A54,I7)')
     &        'Maturity reached but waiting for reported harvest on: ',
     &        YEARDOYHARF 
              CALL WARNING(1,'CSCRP',MESSAGE)
              WRITE(Fnumwrk,*)' '
              WRITE(Fnumwrk,'(A55,I7)')
     &        ' Maturity reached but waiting for reported harvest on: ',
     &        YEARDOYHARF 
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
            IF (MDAYFR.LT.0.0) THEN     ! Maturity not reached
              IF (CFLFAIL.EQ.'Y') THEN
                WRITE(Message(1),'(A26)')
     &           'Harvest/failure triggered '                 
                CALL WARNING(1,'CSCRP',MESSAGE)
                WRITE(Fnumwrk,*)' '
                WRITE(Fnumwrk,*)'Harvest/failure triggered on ',yeardoy
              ENDIF  
            ENDIF
          ENDIF

!-----------------------------------------------------------------------
!         Calculate season end soil conditions                              
!-----------------------------------------------------------------------

          SOILNF = 0.0
          FSOILH2O = 0.0
          DO I = 1, NLAYR
            SOILNF = SOILNF + NO3LEFT(I)/(10.0/(BD(I)*(DLAYR(I))))
     &                          + NH4LEFT(I)/(10.0/(BD(I)*(DLAYR(I))))
            FSOILH2O = FSOILH2O + SW(I)*DLAYR(I)
          ENDDO

!-----------------------------------------------------------------------
!         Calculate variables that are generally measured and presented
!-----------------------------------------------------------------------

          ! Here,reserves are included in leaf,stem,and chaff weights
          ! And weights are in kg/ha
          CWAD = (LFWT+STWT+CHWT+GRWT+RSWT+SENTOPRETAINED)*PLTPOP*10.0
          IF
     &    (LRETS.GT.0.AND.LRETS.LT.99999.AND.SENTOPRETAINED.GT.0.0) THEN
            SENTOPRETAINEDA = SENTOPRETAINED*PLTPOP*10.0
          ELSEIF (LRETS.LE.0.OR.LRETS.GE.99999) THEN
            SENTOPRETAINEDA = 0.0
          ENDIF
          GWAD = GRWT*PLTPOP*10.0
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
          SENROOTA = SENROOT*10.0*PLTPOP
          SENCAS = SENCS*10.0*PLTPOP
          SENLAS = SENLS*10.0*PLTPOP
          SENTOPLITTERA = SENTOPLITTER*PLTPOP*10.0
          DO L =1,NLAYR
            RTWTAL(L) = RTWTL(L)*PLTPOP*10.0
            SENWAL(L) = SENWL(L)*PLTPOP*10.0
          ENDDO

          TWAD = 
     &      (SEEDRS+SDCOAT+RTWT+LFWT+STWT+GRWT+RSWT+SENTOPRETAINED)
     &         * PLTPOP*10.0

          VWAD = (LFWT+STWT+RSWT+SENTOPRETAINED)*PLTPOP * 10.0
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
          LLNAD = LEAFN*(1.0-LSHFR)*PLTPOP*10.0
          RNAD = ROOTN*PLTPOP*10.0
          RSNAD = RSN*PLTPOP*10.0
          SDNAD = SEEDN*PLTPOP*10.0
          ! LAH Stem N does not include leaf sheaths
          SNAD = STEMN*PLTPOP*10.0
          ! SNAD = (STEMN+LEAFN*LSHFR)*PLTPOP*10.0
          TNAD = (LEAFN+STEMN+GRAINN+RSN+DEADN+SEEDN+ROOTN)*PLTPOP*10.0
          VNAD = (LEAFN+STEMN+RSN+DEADN)*PLTPOP*10.0
          
          ! LAH Note that no reserves included in sancout
          ! SANCOUT = SNAD/(STWAD+STRSWAD + LSHWAD+LSHRSWAD)
          ! LAH  IF ((STWAD + LSHWAD).GT.1.0E-5)
          ! LAH     SANCOUT = SNAD/(STWAD + LSHWAD)
          IF (STWAD.GT.1.0E-5)
     &     SANCOUT = SNAD/STWAD

          HWAD = GWAD
          HWUD = GWUD
          HNUMAD = GNOAD
          HNAD = GNAD
          HNC = GRAINANC

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
            RESLGALG(0) = LLWAD*LLIGPC/100.0*(1.0-HBPCF/100.0)
     &                  + LSHWAD*SLIGPC/100.0*(1.0-HBPCF/100.0)
     &                  + STWAD*SLIGPC/100.0*(1.0-HBPCF/100.0)
     &                  + GWAD*GLIGPC/100.0*(1.0-HPCF/100.0)
            ! Soil
            DO L = 1, NLAYR
              RESWALG(L) = RTWTL(L)*PLTPOP*10.0
              RESNALG(L) = RTWTL(L)*PLTPOP*10.0 * RANC
              RESCALG(L) = RTWTL(L)*PLTPOP*10.0 * 0.4
              RESLGALG(L) = RTWTL(L)*PLTPOP*10.0 * RLIGPC/100.0
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
     &     (RTWT+LFWT+STWT+GRWT+RSWT+SENTOPRETAINED+SENTOPLITTER
     &     +SENROOT-SEEDUSE)
     &     * PLTPOP / PARMJC)
          IF (PARMJIC.GT.0.0) PARIUED = AMAX1(0.0,
     &     (RTWT+LFWT+STWT+GRWT+RSWT+SENTOPRETAINED+SENTOPLITTER
     &     +SENROOT-SEEDUSE)
     &     * PLTPOP / PARMJIC)

          IF (CARBOBEG.GT.0.0) THEN
            PARIUE = (CARBOBEG*PLTPOP)/(PARMJFAC*SRAD*PARI)
          ELSE  
            PARIUE = 0.0
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
            DEADWTM = SENTOPRETAINED
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

            SENWACM = SENTOPLITTERA+SENROOTA

            RSWAM = RSWAD

            CNAM = CNAD
            GNAM = GNAD
            GNPCM = GRAINANC*100.0
            VNAM = VNAD
            VNPCM = VANC*100.0
            RNAM = RNAD

            HINM = HIND

            ! Set harvest product outputs
            HWAM = GWAM/(1.0-GMPCH/100.0)! To give yield at GMPCH 
            HNAM = GNAM
            HWUM = GWUM
            HNUMAM = GNOAM
            HNUMGM = GNOGM
            HNUMPM = GNOPM
            HNPCM = GNPCM

          ENDIF

!-----------------------------------------------------------------------
!       Set planting date if flag indicates relative to emergence date
!-----------------------------------------------------------------------
          
          IF (EDATMX.GT.0.AND.CFLPDATE.EQ.'E') THEN
            IF (GEUCUM.LT.PEGD) GEUCUM = 0.0 
            IF (EDATMX.EQ.YEARDOY) THEN
              WRITE(fnumwrk,*)' '
              WRITE(fnumwrk,*)
     &       'Planting date set relative to measured emergence date on '
     &       ,yeardoy
              GEUCUM = PEGD
              GERMFR = 1.0
              EMRGFR = 1.0
              Rtwtl(1) = seedrsi * 0.030
              Rtwtl(2) = seedrsi * 0.025
              Rtwtl(3) = seedrsi * 0.000
              Rootn = seedni* 0.04
              Rtdep = 7.5
              Cumdu = 20.0
              DAP = 10  ! Arbitrary
              GDAP = 4
              STGYEARDOY(1) = YEARDOY - 6
              DAE = 0
              EYEARDOY = YEARDOY
              EDAP = DAP
              EDAYFR = 0.0
              EDAPFR = FLOAT(DAP) + EDAYFR
              LNUMSG = 1
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

        SLAOUT = -99.0
        !OUTCHOICE = 3  ! Set earlier when writing to work.out
        ! Note possibilities. To change must recompile.
        IF (OUTCHOICE.EQ.1) THEN
          ! 1. Include reserves, stem wt includes sheath
          LLWADOUT = LLWAD+LLRSWAD
          STWADOUT = STWAD+STRSWAD + LSHWAD+LSHRSWAD
          CHWADOUT = CHWAD+CHRSWAD
          IF (LFWT.GT.1.0E-6) 
     &     SLAOUT=(PLA-SENLA-LAPHC)/(LFWT*(1.0-LSHFR)+LLRSWT)
        ELSEIF (OUTCHOICE.EQ.2) THEN
          ! 2. No reserves, stem wt includes sheath
          LLWADOUT = LLWAD
          STWADOUT = STWAD + LSHWAD
          CHWADOUT = CHWAD
          IF (LFWT.GT.1.0E-6)SLAOUT=(PLA-SENLA-LAPHC)/(LFWT*(1.0-LSHFR))
        ELSEIF (OUTCHOICE.EQ.3) THEN
          ! 3. No reserves, stem wt does not includes sheath
          LLWADOUT = LLWAD
          STWADOUT = STWAD
          CHWADOUT = CHWAD
          IF (LFWT.GT.1.0E-6)SLAOUT=(PLA-SENLA-LAPHC)/(LFWT*(1.0-LSHFR))
          ! Stem area (NOT including leaf sheath)
          SAID = STAI
        ENDIF  
        IF (SLA.LE.0.0) SLAOUT = -99.0
        
        CALL Csopline(sentoplitterac,(sentoplittera))
        CALL Csopline(senrootc,(senroota))
        CALL Csopline(laic,lai)
        CALL Csopline(caic,caid)
        CALL Csopline(hindc,hind)
        CALL Csopline(hwudc,hwud)
        CALL Csopline(sdwadc,sdwad)
        CALL Csopline(gstagec,gstage)
        
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
            WRITE(fnumwrk,*)' '
            WRITE(fnumwrk,'(A25,I16,I7,I7)')
     &       ' Year,day,DAP            ',YEAR,DOY,DAP
            WRITE(fnumwrk,'(A34,2F7.3)')
     &       ' Rainfall,Irrigation mm           ',rain,irramt
            WRITE(fnumwrk,'(A34,2F7.3)')
     &       ' Tmean,Tcan oC                    ',tmean,tcan
            WRITE(fnumwrk,'(A34,2F7.3)')
     &       ' Tcan-Tmean Today and average oC  ',tcan-tmean,tdifav
            WRITE(fnumwrk,'(A34,F7.1)')
     &       ' Windspeed m/s                    ',windsp     
            WRITE(fnumwrk,'(A34,2F7.3,2F7.1)')
     &       ' Rstage,Lnum. Beginning,end of day',
     &       rstagep,rstage,lnumprev,lnum
            IF (CUMDU.GE.LGPHASEDU(1).AND.CUMDU.LT.LGPHASEDU(2)) THEN
              WRITE(fnumwrk,'(A36,F5.1,F7.1)')
     &         ' Phyllochron interval. Std.,actual  ',phints,phint
            ENDIF 
            IF (PLA-SENLA-LAPHC.LT.9999.9) THEN
              WRITE(fnumwrk,'(A34,F7.1,F7.1)')
     &        ' Laminae area end day /m2,/plant  ',lai,pla-senla-laphc
            ELSE
              WRITE(fnumwrk,'(A34,F7.1,I7)')
     &        ' Laminae area end day /m2,/plant  ',
     &        lai,NINT(pla-senla-laphc)
            ENDIF
            WRITE(fnumwrk,'(A25,I1,A8,2F7.3)')
     &       ' PARI,competition model,C',CN,' 1-crop ',PARI,PARI1
            IF (Rlf.GT.0.0) THEN
              WRITE(fnumwrk,'(A34,2F7.1,2F7.1)')
     &         ' Ratm,Rcrop,Rcrop*Rco2/R,*H2o     ',
     &        ratm,rcrop,rcrop*rlfc/rlf,rcrop*rlfc/rlf*(1.0-(1.0-wfp))
            ELSE
              WRITE(fnumwrk,'(A34,2F7.1)')
     &         ' Ratm,Rcrop                       ',
     &        ratm,rcrop                         
            ENDIF
            IF (FILEIOT.NE.'XFL') THEN
             IF (IDETL.EQ.'D'.OR.IDETL.EQ.'A') THEN
               IF (meevp.EQ.'R')THEN
                 WRITE(fnumwrk,'(A50)')
     &         ' Model (CSM) pot.evap.method: Priestley-Taylor R  '
               ELSEIF (meevp.EQ.'P')THEN
                 WRITE(fnumwrk,'(A51)')
     &         ' Model (CSM) pot.evap.method: FAO Penman (FAO-24) P'
               ELSEIF (meevp.EQ.'F')THEN
                 WRITE(fnumwrk,'(A50,A10)')
     &         ' Model (CSM) pot.evap.method: FAO Penman-Monteith ', 
     &         '(FAO-56) F'
               ELSEIF (meevp.EQ.'D')THEN
                 WRITE(fnumwrk,'(A53,A10)')
     &           ' Model (CSM) pot.evap.method: Dynamic Penman-Monteith'
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
              WRITE(fnumwrk,'(A34,4F7.3,F8.3)')
     &         ' EO  P-T,Pen,M-Pen,Ebud,Model     ',
     &         eopt,eopen,eompen,eoebud,eo
              WRITE(fnumwrk,'(A34,4F7.3,F8.3)')
     &         ' EOCrp                            ',
     &         eopt,eopen,eompcrp,eoebudcrp,eo
              WRITE(fnumwrk,'(A34,4F7.3,F8.3)')
     &         ' EOCrpCo2                         ',
     &         eopt,eopen,eompcrpco2,eoebudcrpco2,eo
              WRITE(fnumwrk,'(A34,4F7.3,F8.3)')
     &         ' EOCrpCo2H2o                         ',
     &         eopt,eopen,eompcrpco2h2o,eoebudcrpco2h2o,eo
              IF (WFP.LT.1.0.AND.WFP.GT.0.0) 
     &         WRITE(fnumwrk,'(A41,F4.2,A8,F4.1,A8,F4.1)')
     &         ' NB.Water stress effect operative. WFP = ',wfp,
     &         ' TCAN = ',tcan,' TAIR = ',tmean
              WRITE(fnumwrk,'(A34,4F7.1,F8.1)')
     &         ' EOC P-T,Pen,M-P,Ebud,Model       ',
     &         eoptc,eopenc,eompenc,eoebudc,eoc
              WRITE(fnumwrk,'(A34,4F7.1,F8.1)')
     &         ' EOCrpC                           ',
     &         eoptc,eopenc,eompcrpc,eoebudcrpc,eoc
              WRITE(fnumwrk,'(A34,4F7.1,F8.1)')
     &         ' EOCrpCo2C                        ',
     &         eoptc,eopenc,eompcrpco2c,eoebudcrpco2c,eoc
              WRITE(fnumwrk,'(A34,4F7.1,F8.1)')
     &         ' EOCrpCo2h2oC                     ',
     &         eoptc,eopenc,eompcrpco2h2oc,eoebudcrpco2h2oc,eoc
             ENDIF
            ENDIF
            IF (EYEARDOY.LE.YEARDOY) THEN
              WRITE(fnumwrk,'(A34,2F7.3)')
     &         ' Pot.pl./Pot.soil evap; /Pot.pl330',epsratio,tratio
              WRITE(fnumwrk,'(A34,F7.3)')
     &         ' Quantum requirement              ',photqr
              WRITE(fnumwrk,'(A34,2F7.1)')
     &         ' CO2,Estimated internal CO2 vpm   ',co2,co2intppm
              WRITE(fnumwrk,'(A34,F7.3,6F7.3)')
     &         ' Phs facs Co2,Temp,H2o,N,Rsvs,Vpd ',
     &         co2fp,tfp,wfp,nfp,rsfp,vpdfp
              WRITE(fnumwrk,'(A34,3F7.3)')
     &         ' Phs. Rue,Rue+Co2i,Resistances    ',
     &         carbobegr*pltpop,carbobegi*pltpop,carbobegm*pltpop
              WRITE(fnumwrk,'(A34,3F7.2)')
     &         ' CH2O Start,end,remobilized       ',
     &         carbobeg*pltpop*10.,
     &         carboend*pltpop*10.0,senlfgrs*pltpop*10.0
              IF (RTWTGRS.GT.0.0) WRITE(FNUMWRK,'(A34)')
     &         ' Surplus assimilates sent to roots'
              IF (LRTIP.EQ.1) WRITE(fnumwrk,'(A21)')
     &         ' Root tip in layer 1 '
              WRITE(FNUMWRK,'(A34,3F7.2)')
     &         ' N demand,uptake,shortage (kg/ha) ',
     &         andem,nupap,AMAX1(0.0,andem-nupap)
              ! Folowing detailed outputs printed if CFLNOUTPUTS=Y
              IF (CFLNOUTPUTS.EQ.'Y') THEN
                IF (ANDEM.LE.0.0) THEN
                  WRITE(FNUMWRK,'(A44)')
     &              ' N demand at zero! Components of demand/use:' 
                ELSE
                  WRITE(FNUMWRK,'(A47)')
     &              ' N demand above zero! Components of demand/use:' 
                ENDIF  
                WRITE(FNUMWRK,*)
     &            ' Leaves            ',lndem*pltpop*10.0
                WRITE(FNUMWRK,*)
     &            ' Stem              ',sndem*pltpop*10.0
                WRITE(FNUMWRK,*)
     &            ' Roots             ',rndem*pltpop*10.0
                WRITE(FNUMWRK,*)
     &            ' Grain             ',grainndem*pltpop*10.0
                WRITE(FNUMWRK,'(A47)')
     &            ' N supplied to grain:                          ' 
                WRITE(FNUMWRK,*)
     &            ' Grainngrs,grainngu',
     &            grainngrs*pltpop*10.0,grainngu*pltpop*10.0
                WRITE(FNUMWRK,*)
     &            ' Grainngr,pool     ',grainngr*pltpop*10.0,
     &             npoolr*pltpop*10.0
                WRITE(FNUMWRK,*)
     &            ' Grainngs,pool     ',grainngs*pltpop*10.0,
     &             npools*pltpop*10.0
                WRITE(FNUMWRK,*)
     &            ' Grainngl,,pool    ',grainngl*pltpop*10.0,
     &             npooll*pltpop*10.0
                WRITE(FNUMWRK,'(A47)')
     &            ' N useages:                                    ' 
                WRITE(FNUMWRK,*)
     &            ' Seed use          ',seednuse*pltpop*10.0
                WRITE(FNUMWRK,*)
     &            ' Reserves use      ',rsnused*pltpop*10.0
                WRITE(FNUMWRK,*)
     &            ' Leaf N use        ',grainngl*pltpop*10.0
                WRITE(FNUMWRK,*)
     &            ' Stem N use        ',grainngs*pltpop*10.0
                WRITE(FNUMWRK,*)
     &            ' Root N use        ',grainngr*pltpop*10.0
                WRITE(FNUMWRK,'(A47)')
     &            ' N contents:                                   ' 
                ! Soil N
                SOILN = 0.0
                DO I = 1, NLAYR
                  SOILN = SOILN + NO3LEFT(I)/(10.0/(BD(I)*(DLAYR(I))))
     &                      + NH4LEFT(I)/(10.0/(BD(I)*(DLAYR(I))))
                ENDDO
                WRITE(FNUMWRK,'(A20,F7.1)')
     &            '  Soil inorganic N  ',soiln
                IF (lanc.GT.0.0.AND.lncm.GT.0.0) THEN
                  tvr1 = leafn*pltpop*10.0*(lanc-lncm)/lanc       
                ELSE
                  tvr1 = 0.0   
                ENDIF
                WRITE(FNUMWRK,'(A20,2F7.1)')
     &            '  Leaf N,total,>min ',leafn*pltpop*10.0,tvr1
                IF (sanc.GT.0.0.AND.sncm.GT.0.0) THEN
                  tvr1 = stemn*pltpop*10.0*(sanc-sncm)/sanc       
                ELSE
                  tvr1 = 0.0   
                ENDIF
                WRITE(FNUMWRK,'(A20,2F7.1)')
     &            '  Stem N,total,>min ',stemn*pltpop*10.0,tvr1
                IF (ranc.GT.0.0.AND.rncm.GT.0.0) THEN
                  tvr1 = rootn*pltpop*10.0*(ranc-rncm)/ranc       
                ELSE
                  tvr1 = 0.0   
                ENDIF
                WRITE(FNUMWRK,'(A20,2F7.1)')
     &            '  Root N,total,>min ',rootn*pltpop*10.0,tvr1
                WRITE(FNUMWRK,'(A20,F7.1)')
     &            '  Grain N           ',grainn*pltpop*10.0  
              ENDIF
              IF (CCOUNTV.EQ.1) WRITE (fnumwrk,'(A35,I4)')
     &         ' Maximum leaf number reached on day',DOY
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
              YEARDOY = YEAR*1000 + DOY
              WRITE (NOUTPG,2201)
 2201         FORMAT ('@YEAR DOY   DAS   DAP TMEAN TKILL',
     A               '  GSTD  L#SD',
     B               ' PARID PARUD  AWAD',
     C               '  LAID  SAID  CAID',
     D               '  TWAD SDWAD  RWAD  CWAD  LWAD  SWAD  GWAD  HIAD',
     E               ' CHWAD  EWAD RSWAD SNWPD SNWLD SNWSD',
     F               '  RS%D',
     G               '  H#AD  HWUD',
     I               '  T#AD  SLAD  RDPD  PTFD',
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
     B      PARIOUT,PARIUE,AMIN1(999.9,CARBOBEG*PLTPOP*10.0),
     &      LAIC,SAID,CAIC,
     C      NINT(TWAD),SDWADC,NINT(RWAD),NINT(CWAD),
     D      NINT(LLWADOUT),NINT(STWADOUT),NINT(HWAD),HIAD,
     E      NINT(CHWADOUT),NINT(EWAD),NINT(RSWAD),
     &      NINT(SENTOPRETAINEDA),SENTOPLITTERAC,SENROOTC,
     F      RSCD*100.0,NINT(HNUMAD),HWUDC,
     G      NINT(TNUMAD),NINT(SLAOUT),RTDEP/100.0,PTF,H2OA,
     H      AMIN1(99.9,WAVR),AMIN1(15.0,WUPR),
     I      1.0-WFT,1.0-WFP,1.0-WFG,
     J      1.0-NFT,1.0-NFP,1.0-NFG,AMIN1(2.0,NUPRATIO),
     K      1.0-TFP,1.0-TFG,1.0-VF,1.0-DFOUT
  501       FORMAT(
     A      I5,I4,2I6,F6.1,F6.1,A6,F6.1,
     B      F6.3,F6.2,F6.1,
     &      A6,F6.3,A6,
     C      I6,A6,2I6,
     C      3I6,F6.3,
     C      3I6,
     C      I6,2A6,
     D      F6.1,I6,A6,
     E      2I6,2F6.2,F6.1,
     F      F6.1,F6.2,
     G      3F6.2,
     H      3F6.2,F6.1,
     I      2F6.2,
     J      2F6.2)
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
     &           EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
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
     A      YEAR,DOY,DAS,DAP,TMEAN,TCDIF,GSTAGEC,
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
     A       YEAR,DOY,DAS,DAP,TMEAN,GSTAGEC,RSTAGE,
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
     F           ' RSNAD SNNPD SNN0D SNN1D',
     B           '  RN%D  LN%D  SN%D  HN%D SDN%D  VN%D',
     C           ' LN%RD SN%RD RN%RD  VCN%  VMN% NUPRD',
     D           ' NDEMD')
              ENDIF  ! Plantn header writes
              CALL Csopline(senn0c,sennal(0))
              CALL Csopline(sennsc,sennas)
              WRITE (NOUTPN,503)
     A         YEAR,DOY,DAS,DAP,TMEAN,GSTAGEC,NUPAC,
     B         TNAD,SDNAD,RNAD,CNAD,LLNAD,SNAD,HNAD,HINDC,
     H         RSNAD,DEADNAD,SENN0C,SENNSC,
     C         RANC*100.0,LANC*100.0,SANCOUT*100.0,
     D         AMIN1(9.9,HNC*100.0),SDNC*100.0,AMIN1(9.9,VANC*100.0),
     E         LNCR,SNCR,RNCR,
     &         VCNC*100.0,VMNC*100.0,
     F         AMIN1(2.,NUPRATIO),ANDEM
  503          FORMAT(
     1         I5,I4,2I6,F6.1,A6,F6.1,
     2         F6.1,2F6.2,4F6.1,A6,
     3         2F6.2,2A6,
     4         3F6.3,
     5         3F6.3,
     6         3F6.3,
     2         F6.1,F6.2,
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
            gnpcmm = -99
            hwahm = -99
            hwamm = -99
            hyamm = -99
            hwumm = -99
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
            ! Variables from time-course file
            LAIXT = -99.0 
            TNUMT = -99.0 
            LNUMT = -99.0 
            CWADT = -99.0 
            HWADT = -99.0 
            HIADT = -99.0 
            HWUT = -99.0 
            HNUMAT = -99.0 
            HNUMET = -99.0 
            ADATT = -99 
            MDATT = -99 

            WRITE(Fnumwrk,*)' '
            WRITE(Fnumwrk,*)' '
            WRITE(Fnumwrk,'(A45)')
     &        ' FINISHED SIMULATION. PREPARING FINAL OUTPUTS'
                      
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
              CALL WARNING(2,'CSCRP',MESSAGE)
              WRITE(Fnumwrk,*)' '
              WRITE (Fnumwrk,'(A24,A50)')
     &         ' Could not find A-file: ',filea(1:50)
              WRITE (Fnumwrk,'(A24,A50)')
     &         ' Experiment file:       ',fileio(1:50)
              OPEN (UNIT=FNUMTMP, FILE=FILEA, STATUS = 'UNKNOWN')
              CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
            ELSE
              WRITE(Fnumwrk,*)' '
              WRITE (Fnumwrk,'(A15,A50)')
     &         ' Found A-file: ',filea(1:50)
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
              DO L = 1,SSNUM
               CALL AREADI (FILEA,TN,RN,SN,ON,CN,ssabv(l),ssdatm(l))
               CALL LTRIM(SSABV(L)) 
               IF (SSABV(L).EQ.'DRAT')
     &           CALL AREADI (FILEA,TN,RN,SN,ON,CN,'DRDAT',ssdatm(l))
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
              IF (TVILENT(FILEADIR).GT.3) THEN
                IF (FILEADIR(TVILENT(FILEADIR):TVILENT(FILEADIR)).NE.
     &           SLASH) THEN
                  FILET = FILEADIR(1:TVILENT(FILEADIR))//
     &             SLASH//EXCODE(1:8)//'.'//EXCODE(9:10)//'T'
                ELSE
                  FILET = FILEADIR(1:TVILENT(FILEADIR))//
     &             EXCODE(1:8)//'.'//EXCODE(9:10)//'T'
                ENDIF
              ELSE
                FILET = FILENEW(1:FILELEN-12)//EXCODE(1:8)//'.'//
     &          EXCODE(9:10)//'T'
              ENDIF       
              FEXISTT  = .FALSE.
              INQUIRE (FILE = FILET,EXIST = FEXISTT)
              IF (.not.FEXISTT) THEN
                WRITE (Message(1),'(A23,A50)')
     &          'Could not find T-file: ',filet(1:50)
                CALL WARNING(1,'CSCRP',MESSAGE)
                WRITE(Fnumwrk,*)' '
                WRITE (Fnumwrk,'(A24,A50)')
     &          ' Could not find T-file: ',filet(1:50)
              ELSE
                WRITE(Fnumwrk,*)' '
                WRITE (Fnumwrk,'(A15,A50)')
     &          ' Found T-file: ',filet(1:50)
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
                      LINET(LENLINE+1:LENLINE+20)='   DAP   DAS YEARDOY'
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
                            ! July 2013 ... to get full yeardoy
                            IF (DATE.GT.40001) THEN   ! After 1940
                              DATE = 1900000 + DATE 
                            ELSE  
                              DATE = 2000000 + DATE 
                            ENDIF
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
                          DAPCHAR = TL10FROMI(DATE)
                          IF (TVILENT(DAPCHAR).EQ.7) THEN
                            DATEWRITE = ' '//DAPCHAR(1:7)
                          ENDIF
                          LENLINE = TVILENT(LINET)
                          LINET(LENLINE+1:LENLINE+8) = DATEWRITE(1:8)
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
                  WRITE (Message(1),'(A23,A50)')
     &             'T-file was empty '
                  CALL WARNING(1,'CSCRP',MESSAGE)
                  WRITE(Fnumwrk,*)' '
                  WRITE (Fnumwrk,'(A24,A50)')
     &             ' T-file was empty '
                ENDIF
                CLOSE(FNUMT)
                CLOSE(FNUMMEAS)
              ENDIF ! End t-file reads,measured.out writes
              
              IF (IDETL.EQ.'A') THEN
                ! Use T-data if A-data missing (whem output=all)
                IF (FEXISTT) THEN
                  IF (HWAMM.LE.0.0) THEN
                    IF (HWADT.GT.0.0) THEN
                      HWAMM = HWADT
                      WRITE(Message(1),'(A30)')
     &                 'Time-course data used for HWAM'
                      CALL WARNING(1,'CSCRP',MESSAGE)
                      WRITE(Fnumwrk,*)' '
                      WRITE(Fnumwrk,'(A32)')
     &                 '  Time-course data used for HWAM'
                    ENDIF
                  ELSE
                    IF (HWADT.GT.0.0) THEN
                      IF (ABS(100.0*ABS(HWAMM-HWADT)/HWAMM).GT.0.0) THEN
                      WRITE(Message(1),'(A48,F8.2)')
     &                'Pc difference between final,time-course yields ='
     &                ,100.0*ABS(HWAMM-HWADT)/HWAMM
                      WRITE(Message(2),'(A20,I6)')
     &                'Final yield         ',NINT(HWAMM)
                      WRITE(Message(3),'(A20,I6)')
     &                'Time-course yield   ',NINT(HWADT)
                      CALL WARNING(3,'CSCRP',MESSAGE)
                      WRITE(Fnumwrk,*)' '
                      WRITE(Fnumwrk,'(A48,F8.2)')
     &                ' % difference between final,time-course yields ='
     &                ,100.0*ABS(HWAMM-HWADT)/HWAMM
                      WRITE(Fnumwrk,'(A21,I6)')
     &                ' Final yield         ',NINT(HWAMM)
                      WRITE(Fnumwrk,'(A21,I6)')
     &                ' Time-course yield   ',NINT(HWADT)
                      ENDIF
                    ENDIF
                  ENDIF
                  IF (CWAMM.LE.0.0) THEN
                    IF (CWADT.GT.0.0) THEN
                      CWAMM = CWADT
                      WRITE(Message(1),'(A31)')
     &                 'Time-course data used for CWAMM'
                      CALL WARNING(1,'CSCRP',MESSAGE)
                      WRITE(Fnumwrk,*)' '
                      WRITE(Fnumwrk,'(A33)')
     &                 '  Time-course data used for CWAMM'
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
                      CALL WARNING(3,'CSCRP',MESSAGE)
                      WRITE(Fnumwrk,*)' '
                      WRITE(Fnumwrk,'(A49,F8.2)')
     &                ' % difference between final,time-course canopy ='
     &                ,100.0*ABS(CWAMM-CWADT)/CWAMM
                      WRITE(Fnumwrk,'(A20,I6)')
     &                ' Final canopy       ',NINT(CWAMM)
                      WRITE(Fnumwrk,'(A20,I6)')
     &                ' Time-course canopy ',NINT(CWADT)
                     ENDIF
                    ENDIF
                  ENDIF
                  IF (LAIXM.LE.0.0.AND.LAIXT.GT.0.0) THEN
                    LAIXM = LAIXT
                    WRITE(Message(1),'(A31)')
     &               'Time-course data used for LAIXM'
                    CALL WARNING(1,'CSCRP',MESSAGE)
                    WRITE(Fnumwrk,*)' '
                    WRITE(Fnumwrk,'(A32)')
     &               ' Time-course data used for LAIXM'
                  ENDIF
                  IF (LNUMSMM.LE.0.0.AND.LNUMSMM.GT.0.0) THEN
                    LNUMSMM = LNUMT
                    WRITE(Message(1),'(A33)')
     &               'Time-course data used for LNUMSMM'
                    CALL WARNING(1,'CSCRP',MESSAGE)
                    WRITE(Fnumwrk,*)' '
                    WRITE(Fnumwrk,'(A34)')
     &               ' Time-course data used for LNUMSMM'
                  ENDIF
                  IF (TNUMAMM.LE.0.0.AND.TNUMT.GT.0.0) THEN
                    TNUMAMM = TNUMT
                    WRITE(Message(1),'(A33)')
     &               'Time-course data used for TNUMAMM'
                    CALL WARNING(1,'CSCRP',MESSAGE)
                    WRITE(Fnumwrk,*)' '
                    WRITE(Fnumwrk,'(A34)')
     &               ' Time-course data used for TNUMAMM'
                  ENDIF
                  IF (HIAMM.LE.0.0.AND.HIADT.GT.0.0) THEN
                    HIAMM = HIADT
                    WRITE(Message(1),'(A31)')
     &               'Time-course data used for HIAMM'
                    CALL WARNING(1,'CSCRP',MESSAGE)
                    WRITE(Fnumwrk,*)' '
                    WRITE(Fnumwrk,'(A32)')
     &               ' Time-course data used for HIAMM'
                  ENDIF
                  IF (HWUMM.LE.0.0.AND.HWUT.GT.0.0) THEN
                    HWUMM = HWUT
                    WRITE(Message(1),'(A31)')
     &               'Time-course data used for HWUMM'
                    CALL WARNING(1,'CSCRP',MESSAGE)
                    WRITE(Fnumwrk,*)' '
                    WRITE(Fnumwrk,'(A32)')
     &               ' Time-course data used for HWUMM'
                  ENDIF
                  IF (HNUMAMM.LE.0.0.AND.HNUMAT.GT.0.0) THEN
                    HNUMAMM = HNUMAT
                    WRITE(Message(1),'(A31)')
     &               'Time-course data used for H#AT'
                    CALL WARNING(1,'CSCRP',MESSAGE)
                    WRITE(Fnumwrk,*)' '
                    WRITE(Fnumwrk,'(A32)')
     &               ' Time-course data used for H#AT'
                  ENDIF
                  IF (HNUMGMM.LE.0.0.AND.HNUMET.GT.0.0) THEN
                    HNUMGMM = HNUMET
                    WRITE(Message(1),'(A32)')
     &               'Time-course data used for H#GMM'
                    CALL WARNING(1,'CSCRP',MESSAGE)
                    WRITE(Fnumwrk,*)' '
                    WRITE(Fnumwrk,'(A33)')
     &               ' Time-course data used for H#GMM'
                  ENDIF
                ENDIF
                DO L = 1,PSNUM
                  IF (PSABV(L).EQ.'ADAT'.AND.PSDATM(L).LE.0.0) THEN
                    IF (ADATT.GT.0) THEN
                      PSDATM(L) = INT(ADATT)
                      WRITE(Message(1),'(A31)')
     &                 'Time-course data used for ADATM'
                      CALL WARNING(1,'CSCRP',MESSAGE)
                      WRITE(Fnumwrk,*)' '
                      WRITE(Fnumwrk,'(A32)')
     &                 ' Time-course data used for ADATM'
                    ENDIF  
                  ENDIF
                  IF (PSABV(L).EQ.'MDAT'.AND.PSDATM(L).LE.0.0) THEN
                    IF (MDATT.GT.0) THEN
                      PSDATM(L) = INT(MDATT)
                      WRITE(Message(1),'(A31)')
     &                 'Time-course data used for MDATM'
                      CALL WARNING(1,'CSCRP',MESSAGE)
                      WRITE(Fnumwrk,*)' '
                      WRITE(Fnumwrk,'(A32)')
     &                 ' Time-course data used for MDATM'
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
            IF (hwamm.GT.0.AND.cwamm.GT.0) vwamm = cwamm-hwamm
            
            ! Harvest index at maturity
            IF (hiamm.LE.0.0) THEN
              IF (cwamm.GT.0.AND.hwamm.GT.0) THEN
                hiamm = hwamm/cwamm
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
              WRITE(Message(1),'(A39)')
     &         'ShootNo * product/shoot used for H#AMM'
              CALL WARNING(1,'CSCRP',MESSAGE)
              WRITE(Fnumwrk,*)' '
              WRITE(Fnumwrk,'(A40)')
     &         ' ShootNo * product/shoot used for H#AMM'
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
              WRITE(Message(1),'(A38)')
     &         'ProductNo/area/ShootNo used for H#GMM '
              CALL WARNING(1,'CSCRP',MESSAGE)
              WRITE(Fnumwrk,*)' '
              WRITE(Fnumwrk,'(A39)')
     &         ' ProductNo/area/ShootNo used for H#GMM '
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
              WRITE (Message(1),'(A31,A31,A11)')
     &         'Measured emergence over 200DAP ',
     &         'Maybe reported before planting.',
     &         'Check files'
              CALL WARNING(1,'CSCRP',MESSAGE)
              WRITE(Fnumwrk,*)' '
              WRITE (Fnumwrk,'(A32,A31,A11)')
     &         ' Measured emergence over 200DAP ',
     &         'Maybe reported before planting.',
     &         'Check files'
            ENDIF
            gdapm = Dapcalc(gdatm,plyear,plday)
            adapm = Dapcalc(adatm,plyear,plday)
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

            ! Calculate N% without dead matter            
            VNPCM = 100.0*(LEAFN+STEMN+RSN)/(LFWT+STWT+RSWT)
            HINM = GRAINN/(GRAINN+LEAFN+STEMN+RSN)
          
           
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
     &          ' H#AMS H#AMM',
     &          ' LAIXS LAIXM L#SMS L#SMM',
     &          ' T#AMS T#AMM CWAMS CWAMM VWAMS VWAMM',
     &          ' HIAMS HIAMM HN%MS HN%MM VN%MS VN%MM',
     &          ' CNAMS CNAMM HNAMS HNAMM VNAMS VNAMM HINMS HINMM',
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
              IF (KEYSS(L).GT.0) THEN
                !SSDAP(2) = DRDAP  ! If need to check formula calculation
                IF (SSABVO(KEYSS(L))(1:1).NE.' ') 
     &            WRITE (FNUMEVAL,'(A1)',ADVANCE='NO') ' '
                WRITE (FNUMEVAL,'(I6)',ADVANCE='NO') SSDAP(KEYSS(L))
                IF (SSABVO(KEYSS(L))(1:1).NE.' ') 
     &            WRITE (FNUMEVAL,'(A1)',ADVANCE='NO') ' '
                WRITE (FNUMEVAL,'(I6)',ADVANCE='NO') SSDAPM(KEYSS(L))
              ENDIF
            ENDDO
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
     &       NINT(hnumam),NINT(hnumamm),  
     H       laix,laixm,lnumsm,lnumsmm,
     I       NINT(tnumam),NINT(tnumamm),
     J       NINT(cwam),NINT(cwamm),
     J       NINT(vwam),NINT(vwamm),
     L       hiamchar,hiammchar,
     M       hnpcmchar,hnpcmmchar,vnpcmchar,vnpcmmchar,
     &       NINT(cnam),NINT(cnamm),NINT(hnam),NINT(hnamm),
     &       NINT(vnam),NINT(vnamm),
     &       hinmchar,hinmmchar,
     N       NINT(cwahc),NINT(cwahcm)
 8406       FORMAT (
     A      I6,I6,
     G      A6,A6,I6,I6,
     H      F6.1,F6.1,F6.1,F6.1,
     I      I6,I6,
     J      I6,I6,
     J      I6,I6,
     L      A6,A6,
     M      A6,A6,A6,A6,
     &      I6,I6,I6,I6,I6,I6,
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
                  WRITE(fnumov,2093)soilni,amtnit,soilnf
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
     &            1.0-WFPCAV,1.0-NFPCAV
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
     &          'CROP N%  = Above-ground N concentration (%)'
               WRITE(fnumov,*)
     &         'H2O STRESS = Photosynthesis stress,',
     &         ' prior to stage (0-1,0=none)'
               WRITE(fnumov,*)
     &         'N STRESS = Photosynthesis stress,',
     &         ' prior to stage(0-1,0=none)'
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
                WRITE (FNUMOV,291)psname(mstg),tvi1,tvi1
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
                WRITE(fnumov,600) psname(1),dash,'Harvest', 
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
              IF (CROP.EQ.'WH') THEN 
                WRITE(FNUMOV,300) 'WHEAT', NINT(HWAM),GMPCH
              ELSEIF (CROP.EQ.'BA') THEN 
                WRITE(FNUMOV,300) 'BARLEY', NINT(HWAM),GMPCH
              ENDIF  
              WRITE(FNUMOV,'(110("*"))')
              CLOSE(FNUMOV)  ! Overview.out
              ENDIF 
              ! Basic info.to Work.out when calling for Overview
              !  Summary of various environmental aspects
              WRITE(fnumwrk,*) ' '
              WRITE(fnumwrk,'(A16,A10,I3)')
     &          ' CONDITIONS FOR ',
     &            excode,tn
              WRITE(fnumwrk,*) ' '
              WRITE (fnumwrk,209) tmaxx,tmaxm,tminn,tminm              
              IF (ISWNIT.NE.'N') THEN
                WRITE(fnumwrk,2095)cnad+rnad+hnad,hnad,vnad
                WRITE(fnumwrk,2096)sennal(0),sennas            
                WRITE(fnumwrk,2093)soilni,amtnit,soilnf
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
              WRITE (fnumwrk,'(A40,F6.1)')
     &         '  Temperature mean,germination         ',TMEANG
              WRITE (fnumwrk,'(A40,F6.1)')
     &         '  Temperature mean,germ-emergence      ',TMEANE
              WRITE (fnumwrk,'(A40,F6.1)')
     &         '  Temperature mean,first 20 days       ',TMEAN20P
              IF (TMEAN20ANTH.GT.0.0) WRITE (fnumwrk,'(A40,F6.1)')
     &         '  Temperature mean,20d around anthesis ',TMEAN20ANTH
              IF (CUMDU.GE.MSTG-1) THEN 
               WRITE (fnumwrk,'(A40,3F6.1)')
     &         '  Temperature mean,max,min,grain fill  ',
     &         TMEANPAV(MSTG-1),TMAXPAV(MSTG-1),TMINPAV(MSTG-1)
               WRITE (fnumwrk,'(A40,F6.1)')
     &         '  Temperature max.reached,grain fill   ',
     &         TMAXGFILL
              ENDIF
              IF (SRAD20ANTH.GT.0.0) THEN
                WRITE (fnumwrk,*)' '
                WRITE (fnumwrk,'(A38,F6.1)')
     &           '  Solar radn. mean,20d around anthesis ',SRAD20ANTH
              ENDIF
              WRITE (fnumwrk,'(A38,F6.1)')
     &         '  Stress fac. mean,20d before grain set',STRESS20GS
              
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
     A        ' REP  RUNI S O C    CR PYEAR  PDAT  GDAP  EDAP')
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
     D        ' SDNAP  CNAM  HNAM  RNAM  TNAM  NUCM  HN%M  VN%M TMAXG',
     E        ' SRADA  TAVA CWADG TILWG D1INI D2INI D3INI ')
              CLOSE(fnumpsum)  
            ENDIF  ! End of Plantsum.Out headers
            OPEN (UNIT=fnumpsum,FILE=FNAMEPSUM,POSITION='APPEND')
            WRITE (fnumpsum,400,ADVANCE='NO') run,excode,tn,rn,tname,
     A        rep,runi,sn,on,cn,crop,
     B        plyear,plday,gdap,edap 
  400       FORMAT (I6,1X,A10,I4,I3,1X,A25,
     A       I4,I6,I2,I2,I2,4X,A2,
     B       I6,I6,I6,I6)
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
     I        hnpcmchar,vnpcmchar,tmaxgfill,sradcav,tmeancav,
     &        NINT(cwadgs),tilwtgs,
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
     I       2A6,3F6.1,
     &       I6,F6.3,
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
            WRITE (fnumpha,'(A45,A24,A12)')
     &       '@PH PDAYS SRADA  TMXA  TMNA  PREA  TWLA  CO2A',
     &       '  WFPA  WFGA  NFPA  NFGA',
     &       ' PHASE_END  '
            DO L=1,PSNUM
              IF (STGYEARDOY(L).LT.9999999.AND.
     &         L.NE.0.AND.L.NE.10.AND.L.NE.11)
     &         WRITE (fnumpha,'(I3,I6,3F6.1,2F6.2,I6,4F6.2,1X,A13)')
     &         L,pdays(L),sradpav(L),tmaxpav(L),tminpav(L),
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
            WRITE(Fnumwrk,*)' '
            WRITE(Fnumwrk,'(A36)')
     &       ' IDETL flag called for detail files.'
            WRITE(Fnumwrk,'(A32,A31)')
     &       ' But IDETO flag set at N so that',
     &       'measured data not read.        '
            WRITE(Fnumwrk,'(A46)')
     &       ' Therefore,could not write detailed summaries.'
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
            WRITE (fnumwrk,*)' '
            WRITE (fnumwrk,'(A54,F5.1,F4.1)')
     &       '  Overall PAR use efficientcy(incident,intercepted) = ',
     &       paruec,pariued
            WRITE(fnumwrk,*) ' '
            WRITE(fnumwrk,'(A27,F11.2)')'  Harvest product (kg/ha)  ',
     &       HWAM
            WRITE(fnumwrk,'(A27,F11.2)')'  Product/Total wt (HI)    ',
     &       HIAM
            IF (GNOWTS.GT.0.0.AND.CWAM.GT.0.0) THEN
              WRITE(fnumwrk,'(A28,F10.2)')
     &         '  Grain #/(Total wt)        ',HNUMAM/(CWAM*.1)
              WRITE(fnumwrk,'(A28,F10.2)')
     &         '  Grain #/(Total product)   ',HNUMAM/((CWAM-HWAM)*.1)
              WRITE(fnumwrk,*) ' '
              WRITE(fnumwrk,'(A28,F10.2,A1)')
     &        '  (Grain #/Total standard   ',GNOWTS,')'
            ENDIF
            IF (GNOWTS.GT.0.0) THEN
             WRITE (fnumwrk,*)' '
             IF (gnopm.GT.0.0) WRITE (fnumwrk,'(A22,F7.1)')
     &        '  Grain weight mg     ',GRWT/GNOPM*1000.0
             WRITE (fnumwrk,'(A22,F7.1)')
     &        '  Grain weight coeff  ',gwta
             IF (carbolim.GT.0.OR.Nlimit.GT.0.OR.Tlimit.GT.0) THEN
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
     &        '  Minimum nitrogen %     ',gnpcmn
             WRITE (fnumwrk,'(A24,F7.1)')
     &        '  Standard nitrogen %   ',gnpcs
            ENDIF
            WRITE(fnumwrk,*) ' '
            WRITE(fnumwrk,'(A26,A10,I3)')
     &       ' CH2O BALANCE (kg/ha) FOR ',excode,tn
            WRITE(fnumwrk,'(A27,3F11.1)')
     &       '  SEED+FIXED (1) Seed,fixed',
     &       (SEEDRSI+SDCOAT+CARBOC)*PLTPOP*10.0,
     &       (SEEDRSI+SDCOAT)*PLTPOP*10.0,CARBOC*PLTPOP*10.0
             TVR1 = (SEEDRSI+SDCOAT+CARBOC)*PLTPOP*10.0
            WRITE(fnumwrk,'(A27,3F11.1)')
     &       '  RESPIRED (2)  Tops,root  ',RESPC*PLTPOP*10.0,
     &       RESPTC*PLTPOP*10.0,RESPRC*PLTPOP*10.0
             TVR2 = RESPC*PLTPOP*10.0
            WRITE(fnumwrk,'(A27,3F11.1)')
     &       '  SENESCED (3)  Tops,root  ',
     &       (SENTOPLITTER+SENROOT)*PLTPOP*10.0,
     &       SENTOPLITTER*PLTPOP*10.0,SENROOT*PLTPOP*10.0
             TVR3 = (SENTOPLITTER+SENROOT)*PLTPOP*10.0
            WRITE(fnumwrk,'(A27,3F11.1)')
     &       '  LIVE+DEAD (4) Live,dead  ',
     &       (SEEDRS+SDCOAT+RTWT+LFWT+STWT+CHWT+GRWT+RSWT+
     &        SENTOPRETAINED)*PLTPOP*10.0,
     &       (SEEDRS+SDCOAT+RTWT+LFWT+STWT+CHWT+GRWT+RSWT)
     &       *PLTPOP*10.0,
     &        SENTOPRETAINED*PLTPOP*10.0
             TVR4 = (SEEDRS+SDCOAT+RTWT+LFWT+STWT+CHWT+GRWT+RSWT+
     &        SENTOPRETAINED)*PLTPOP*10.0
            WRITE(fnumwrk,'(A27,3F11.1)')
     &       '  PLANT+SEED_RESIDUE Pl,sd ',
     &       (SEEDRS+SDCOAT+RTWT+LFWT+STWT+CHWT+GRWT+RSWT)
     &       *PLTPOP*10.0,
     &       (RTWT+LFWT+STWT+CHWT+GRWT+RSWT)*PLTPOP*10.0,
     &       (SEEDRS+SDCOAT)*PLTPOP*10.0
            WRITE(fnumwrk,'(A27,2F11.1)')
     &       '  RESERVES (5)  Post-mat   ',RSWT*PLTPOP*10.0,
     &       RSWTPM*PLTPOP*10.0
             TVR5 = RSWT*PLTPOP*10.0
            WRITE(fnumwrk,'(A29, F9.1)')
     &       '  HARVESTED DURING CYCLE (6) ',
     &       (LWPHC+SWPHC+RSWPHC+GWPHC+DWRPHC)*PLTPOP*10.0
             TVR6 = (LWPHC+SWPHC+RSWPHC+GWPHC+DWRPHC)*PLTPOP*10.0
            WRITE(fnumwrk,'(A27, F11.2)')
     &       '  BALANCE (1-(2+3+4+6))    ',TVR1 -(TVR2+TVR3+TVR4+TVR6)
            IF (ABS(TVR1-(TVR2+TVR3+TVR4+TVR6)).GT.0.05)
     &      WRITE(fnumwrk,'(A29,A10,A1,I2)')
     &       '   PROBLEM WITH CH2O BALANCE ',EXCODE,' ',TN
            IF (GNOWTS.GT.0.0.AND.lfwtsge.GT.0.0) THEN
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
     &         (lfwt+stwt+rswt+grwt+SENTOPRETAINED)*pltpop*10.0
              WRITE (fnumwrk,'(A22,F7.1)')'  Leaves at end       ',
     &         (lfwt)*pltpop*10.0
              WRITE (fnumwrk,'(A22,F7.1)')'  Stems at end        ',
     &         (stwt)*pltpop*10.0
              WRITE (fnumwrk,'(A22,F7.1)')'  Reserves at end     ',
     &         (rswt)*pltpop*10.0
              WRITE (fnumwrk,'(A22,F7.1)')'  Grain at end        ',
     &         (grwt)*pltpop*10.0
              WRITE (fnumwrk,'(A22,F7.1)')'  Dead at end         ',
     &         (SENTOPRETAINED)*pltpop*10.0
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
     &         (SENTOPRETAINED-deadwtsge)*pltpop*10.0
              WRITE (fnumwrk,'(A22,F7.1)')'  Roots > SGE         ',
     &         (rtwt-rtwtsge)*pltpop*10.0
              WRITE (fnumwrk,'(A22,F7.1)')'  Total > SGE         ',
     &         ((lfwt+stwt+rswt+grwt+SENTOPRETAINED+rtwt)*pltpop*10.0) -
     &         ((lfwtsge+stwtsge+rswtsge+grwtsge+deadwtsge+rtwtsge)
     &         *pltpop*10.0)
              WRITE (fnumwrk,'(A22,F7.1)')'  Assim-R-S-Diff > SGE',
     &        ((carbogf-respgf)*pltpop*10.0) - sengf*pltpop*10.0 -
     &        (((lfwt+stwt+rswt+grwt+SENTOPRETAINED+rtwt)*pltpop*10.0) -
     &        ((lfwtsge+stwtsge+rswtsge+grwtsge+deadwtsge+rtwtsge)
     &        *pltpop*10.0))
            ENDIF

            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A21,A10,I3)')
     &       ' RESERVES STATUS FOR ',excode,tn
            WRITE (fnumwrk,'(A22,F7.2)')'  Reserves coeff      ',
     &         RSPCA
            WRITE (fnumwrk,'(A22,F7.1)')'  Kg/ha at anthesis   ',
     &       RSWTA*PLTPOP*10.0
            IF (rswta+stwta+lfwta.GT.0) WRITE (fnumwrk,'(A22,F7.1)')
     &      '  % above ground      ',rswta/(rswta+stwta+lfwta)*100.0
            IF (stwta.GT.0) WRITE (fnumwrk,'(A22,F7.1)')
     &      '  % stem+reserves     ',rswta/(rswta+stwta)*100.0
            WRITE (fnumwrk,'(A22,F7.1)')'  Kg/ha at grain set  ',
     &      RSWTAE*PLTPOP*10.0
            IF (rswtae+stwtae+lfwtae.GT.0) 
     &       WRITE(fnumwrk,'(A22,F7.1)') '  % above ground      ',
     &       rswtae/(rswtae+stwtae+lfwtae)*100.
            WRITE (fnumwrk,'(A22,F7.1)')'  Kg/ha at end stem   ',
     &      RSWTSGE*PLTPOP*10.0
            IF ((rswtsge+stwtsge+lfwtsge).GT.0)
     &      WRITE(fnumwrk,'(A22,F7.1)') '  % above ground      ',
     &      rswtsge/(rswtsge+stwtsge+lfwtsge)*100.
            IF (stwtsge.GT.0) WRITE (fnumwrk,'(A22,F7.1)')
     &        '  Pc stem+reserves    ',rswtsge/(rswtsge+stwtsge)*100.0
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
     &       '  DEAD MATERIAL LEFT ON SURFACE  ',SENTOPLITTERA
            WRITE(fnumwrk,'(A32,F8.1)')
     &       '  DEAD MATERIAL LEFT IN SOIL     ',SENROOTA
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
              WRITE (fnumwrk,'(A15,I7)')
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
            WRITE (fnumwrk,'(A25,A10,I3)')' N BALANCE (kg/ha) FOR ',
     &       excode,tn
            WRITE (fnumwrk,'(A34,F8.2,2F11.2)')
     &       '   N UPTAKE + SEED (1)            ', 
     &       (NUPC+SEEDNI)*PLTPOP*10.0,NUPC*PLTPOP*10.,SEEDNI*PLTPOP*10.
             TVR1 = (NUPC+SEEDNI)*PLTPOP*10.0  
            WRITE (fnumwrk,'(A33,F9.2,2F11.2)')
     &       '   TOTAL N SENESCED (2) Tops,Root',
     &       (SENNL(0)+SENNS)*PLTPOP*10.0,SENNL(0)*PLTPOP*10.0,
     &       SENNS*PLTPOP*10.0
             TVR2 = (SENNL(0)+SENNS)*PLTPOP*10.0 
            WRITE (fnumwrk,'(A34,F8.2)')
     &       '   N IN DEAD MATTER               ', PLTPOP*10.0*DEADN
            WRITE (fnumwrk,'(A34,F8.2)')
     &       '   TOTAL N IN PLANT (3)           ',PLTPOP*10.0*
     &       (ROOTN+LEAFN+STEMN+RSN+GRAINN+SEEDN+DEADN)
             TVR3 = (ROOTN+LEAFN+STEMN+RSN+GRAINN+SEEDN+DEADN)*
     &              PLTPOP*10.0         
            WRITE (fnumwrk,'(A33, F9.2)')
     &       '   HARVESTED DURING CYCLE (4)    ',PLTPOP*10.0*
     &       LNPHC+SNPHC+RSNPHC+GNPHC
             TVR4 = (LNPHC+SNPHC+RSNPHC+GNPHC)* PLTPOP*10.0             
            WRITE (fnumwrk,'(A34,F8.3)')
     &       '   BALANCE (1-(2+3+4))            ',TVR1-TVR2-TVR3-TVR4
            IF (TVR1-(TVR2+TVR3+TVR4).GT.0.005)
     &      WRITE(fnumwrk,'(A26,A10,A1,I2)')
     &       '   PROBLEM WITH N BALANCE ',EXCODE,' ',TN
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
     G        '  HWAM  HWUM',
     H        '  H#AM  H#GM  LAIX  L#SM  T#AM',
     I        '  CWAM  VWAM  HIAM  RWAM',
     J        '  HN%M  TNAM',
     K        '  CNAM  HNAM',
     L        '  HINM PLPOP',
     F        '  NICM WFPAV NFPAV',
     M        ' SRADA TMEAN TMAXA TMINA  PRCP')
            ELSE
              OPEN (UNIT=FNUMPRES,FILE=FNAMEPRES,POSITION='APPEND')
            ENDIF  ! End Responses simulated header writes
            WRITE (FNUMPRES,7401,ADVANCE='NO') RUN,EXCODE,TN,RN,CROP,
     A       PLDAYTMP,EDAPFR
            DO L = 1,KEYSTX
              IF (KEYPS(L).GT.0) WRITE (FNUMPRES,'(I6)',ADVANCE='NO')
     &        PSDAP(KEYPS(L))
            ENDDO
            WRITE (fnumpres,409)
     D       NINT(hwam),hwumchar,
     E       NINT(hnumam),NINT(hnumgm),
     F       laixchar,lnumsm,NINT(tnumam),
     G       NINT(cwam),NINT(vwam),
     H       hiamchar,
     I       NINT(rwam),
     J       hnpcmchar,
     K       NINT(AMAX1(-99.0,cnam+rnam)),NINT(cnam),NINT(gnam),
     L       hinmchar,pltpop,
     C       NINT(amtnit),
     M       1.0-wfpcav,1.0-nfpcav,
!    M       sradcav,tmeancav,tmaxcav,tmincav,   ! For crop cycle
     M       sradpav(1),tmeanpav(1),tmaxpav(1),tminpav(1), !For phase ?
     N       NINT(raincc)            
 7401       FORMAT (I6,1X,A10,I4,I3,4X,A2,     !Run,excode,tn,rn,crop
     A       I6,                               !Pldaytmp
     B       F6.1)                             !Edapfr
  409       FORMAT (
     D       I6,A6,                            !gwam,hwum
     E       I6,I6,                            !g#am,g#gm
     F       A6,F6.1,I6,                       !laix,lnumsm,tnumam
     G       I6,I6,                            !Cwam,vwam
     H       A6,                               !hiam
     I       I6,                               !N(rwam)
     J       A6,                               !hnpcm
     K       I6,I6,I6,                         !N(cnam+rnam),(cn),(gn)
     L       A6,F6.1,                          !hinm,pltpop,tnumpm
     C       I6,                               !Amtnit
     M       2F6.1,                            !wfpcav,nfpcav
     M       4F6.1,                            !RAD,TX,TN
     N       I6)                               !RN   
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
     G        '  HWAM  HWUM',
     H        '  H#AM  H#GM  LAIX  L#SM  T#AM',
     I        '  CWAM  VWAM  HIAM  RWAM',
     J        '  HN%M  TNAM',
     K        '  CNAM  HNAM',
     L        '  HINM PLPOP',
     F        '  NICM WFPAV NFPAV',
     M        ' SRADA TMEAN TMAXA TMINA  PRCP')
              ELSE
                OPEN (UNIT=FNUMPREM,FILE=FNAMEPREM,POSITION='APPEND')
              ENDIF ! End Responses measured header writes
              WRITE (FNUMPREM,7401,ADVANCE='NO') RUN,EXCODE,TN,RN,CROP,
     A         PLDAYTMP,FLOAT(MAX(-99,edapm))
              DO L = 1,KEYSTX
                IF (KEYPS(L).GT.0) WRITE (FNUMPREM,'(I6)',ADVANCE='NO')
     &           PSDAPM(KEYPS(L))
              ENDDO
              WRITE (FNUMPREM,409)
     F         NINT(hwamm),hwummchar,
     G         NINT(hnumamm),NINT(hnumgmm),
     H         laixmchar,lnumsmm,NINT(tnumamm), 
     I         NINT(cwamm),NINT(vwamm),hiammchar,
     J         NINT(rwamm),
     K         hnpcmmchar,NINT(tnamm),NINT(cnamm),NINT(hnamm),
     L         hinmmchar,pltpopp,
     F         NINT(amtnit),
     M         1.0-wfpcav,1.0-nfpcav,
!    M         sradcav,tmeancav,tmaxcav,tmincav,               ! Cycle
     M         sradpav(1),tmeanpav(1),tmaxpav(1),tminpav(1),   ! Phase ?
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
     E          '    ADAP   ADAPE',
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
     B         adap,adaterr,
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
              INQUIRE (FILE = OUTPG,OPENED = FOPEN)
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
              OPEN (UNIT=NOUTPG,FILE=OUTPG,STATUS='UNKNOWN')
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
     &        OPEN (UNIT=NOUTPG,FILE=OUTPG,POSITION='APPEND')
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
          
          IF (IDETO.EQ.'E'.OR.IDETO.EQ.'N') THEN
              ! Resource productivity calculations 
              ! (Not done earlier because no Overview called for)
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
              WRITE (FNUMWRK, 1200) CDAYS, 
     &         RAINCC, DMP_Rain*0.1, DMP_Rain, GrP_Rain*0.1, GrP_Rain,
     &         ETCC,  DMP_ET*0.1,   DMP_ET,   GrP_ET*0.1,   GrP_ET, 
     &         EPCC,  DMP_EP*0.1,   DMP_EP,   GrP_EP*0.1,   GrP_EP
              IF (IRRAMTC > 1.E-3) THEN
                WRITE(FNUMWRK, 1210) 
     &            IRRAMTC, DMP_Irr*0.1, DMP_Irr, GrP_Irr*0.1, GrP_Irr
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
          LABEL(13) = 'GNAM'; VALUE(13) = gnam
          LABEL(14) = 'PWAM'; VALUE(14) = hwam+chwt*pltpop*10.0
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
          NFT = 1.0
          WFG = 1.0
          WFP = 1.0
          WFT = 1.0
          
          UNO3 = 0.0
          UNH4 = 0.0
          
          !RUNCRP = RUNCRP + 1
          
          WRITE (fnumwrk,*) ' '
          WRITE (fnumwrk,'(A50)')
     &     ' END OF RUN. WILL BEGIN NEW CYCLE IF CALLED FOR.  '
          IF (IDETL.NE.'N') WRITE (fnumwrk,*) ' '
          SEASENDOUT = 'Y'
          
        ENDIF ! End STGYEARDOY(11).EQ.YEARDOY WITHIN DYNAMIC = OUTPUT

!-----------------------------------------------------------------------
!       Store variables for possible use next day/step
!-----------------------------------------------------------------------

        CNADPREV = CNAD
        IF (CN.LE.1) CROPPREV = CROP
        IF (RNMODE.NE.'G') CUDIRFLPREV = CUDIRFLE
        CWADPREV = CWAD
        DAYLPREV = DAYLT
        ECDIRFLPREV = ECDIRFLE
        ECONOPREV = ECONO
        EMRGFRPREV = EMRGFR
        GESTAGEPREV = GESTAGE
        LAIPREV = LAI
        LNUMPREV = LNUM
        PARIPREV = PARI
        PLYEARDOYPREV = PLYEARDOY
        SPDIRFLPREV = SPDIRFLE
        SRADPREV = SRAD
        SWFRPREV = SWFR
        TNUMPREV = TNUM
        VARNOPREV = VARNO

!***********************************************************************
      ELSEIF (DYNAMIC.EQ.SEASEND) THEN
!***********************************************************************

        IF (STGYEARDOY(11).NE.YEARDOY) THEN  ! End for non-crop reason
          WRITE (fnumwrk,*)' '
          WRITE (fnumwrk,'(A50)')
     &     ' Run terminated.(Usually because ran out of weather data).'
        ENDIF

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
     &25X,'days    øC    øC MJ/m2     hr     mm     mm  synth Growth ',
     &' synth Growth  synth Growth',/,110('-'))
  270 FORMAT(/,'------------------------------------------------------',
     &'--------------------------------------------------------')
  300 FORMAT(/,10X,A," YIELD : ",I8," kg/ha    [",F4.1,"%Moisture] ",/)
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

      INTEGER,PARAMETER::NL=20         ! Maximum number soil layers,20

      REAL          BASELAYER     ! Depth at base of layer         cm
      REAL          DLAYR(20)     ! Depth of soil layers           cm
      REAL          DLAYRTMP(20)  ! Depth of soil layers with root cm
      REAL          EOP           ! Potential evaporation,plants   mm/d
      INTEGER       FNUMWRK       ! File number,work file          #
      LOGICAL       FOPEN         ! File open indicator
      INTEGER       L             ! Loop counter                   #
      REAL          LL(NL)        ! Lower limit,soil h2o           #
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

      DO L = 1,NLAYR
        RWUP = 0.
        IF (RLVTMP(L).LE.0.00001 .OR. SW(L).LE.LL(L)) THEN
          ! 1,* Use water below LL if just germinated and next layer >LL
          IF (L.EQ.1.AND.RTDEP.LE.DLAYR(1)) THEN
            IF (SW(2).GT.LL(2)) WUP(L) = EOP*0.1
            WRITE(Message(1),'(A57)') 
     &      'To avoid early stress,h2o uptake set equal to demand.  '
            CALL WARNING(1,'CSCRP',MESSAGE)
            WRITE(Fnumwrk,*)' '
            WRITE(Fnumwrk,'(A58)') 
     &      ' To avoid early stress,h2o uptake set equal to demand.  '
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
                 WRITE(Message(1),'(A52,I3,a26,F4.2)')
     &           ' Water uptake resticted by saturation,layer',L,
     &           ' Uptake saturation factor ',wfewu
                 CALL WARNING(1,'CSCRP',MESSAGE)
                 WRITE(Fnumwrk,*)' '
                 WRITE(Fnumwrk,'(A52,I3,a26,F4.2)')
     &           ' Water uptake resticted by saturation,layer',L,
     &           ' Uptake saturation factor ',wfewu
               ENDIF
            ENDIF
          ENDIF
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

      SUBROUTINE EVAPO(MEEVP,                              !Input,method
     & SRAD, CLOUDS, TMAX, TMIN, TDEW, WINDSP,             !Input,wether
     & ALBEDO, RATM, RCROP,                                !Input,crop
     & EO,                                                 !Input,pot.ev
     & EOPEN, EOMP, EOPT, EOE, TCAN,                       !Output
     & TASKFLAG)                                           !Calculation
      
      ! Should be that MEEVP indicates method, TASKFLAG indicates what 
      ! to be calculated (evap,resistance,canopy temperature).BUT not
      ! running this way 05/12/12 LAH. Needs work!

      IMPLICIT NONE

      REAL          TVR1
      REAL          ALBEDO        ! Reflectance of soil-crop surf  fr
      REAL          CLOUDS        ! Relative cloudiness factor,0-1 #
      REAL          CSVPSAT       ! Saturated vapor pressure air   Pa
      REAL          DAIR          ! Density of air
      REAL          EEQ           ! Equilibrium evaporation        mm/d
      REAL          EO            ! Potential evapotranspiration   mm/d
      REAL          EOPT          ! Potential evapot.,Priestly-T   mm/d
      REAL          EOPEN         ! Potential evapot.,Penman       mm/d
      REAL          EOMP          ! Potential evap,Penman-Monteith mm/d
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
      
! FO - 11/20/2020 - Removed unused statement for WORKS.OUT
!      CSYCA uses this subroutine as well.
!      IF (FNUMWRK.LE.0.OR.FNUMWRK.GT.1000) THEN
!        CALL Getlun ('WORK.OUT',fnumwrk)
!        INQUIRE (FILE = 'WORK.OUT',OPENED = fopen)
!        IF (.NOT.fopen) OPEN (UNIT = fnumwrk,FILE = 'WORK.OUT')
!      ENDIF

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
        EOPEN = (S*RNETMG + PSYCON*WFNFAO*VPD) / (S+PSYCON)
      ENDIF

      ! Monteith-Penman
      IF (TASKFLAG.EQ.'A'.OR.TASKFLAG.EQ.MEEVP) THEN
        EOMP = ((S*RNETMG+(DAIR*1.0E-2*SHAIR*1.0E-6*VPD)
     &   /(RATM*1.157407E-05))/
     &   (S+PSYCON*(1+rcrop/ratm)))
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
        ! From CSM (Gives slightly different results.Maybe albedo)
        !  TD = 0.60*TMAX+0.40*TMIN
        !  IF (XHLAI .LE. 0.0) THEN
        !   ALBEDO = MSALB
        !  ELSE
        !   ALBEDO = 0.23-(0.23-MSALB)*EXP(-0.75*XHLAI)
        !  ENDIF
        !  SLANG = SRAD*23.923
        !  EEQ = SLANG*(2.04E-4-1.83E-4*ALBEDO)*(TD+29.0)
        !  EO = EEQ*1.1
        !  IF (TMAX .GT. 35.0) THEN
        !   EO = EEQ*((TMAX-35.0)*0.05+1.1)
        !  ELSE IF (TMAX .LT. 5.0) THEN
        !   EO = EEQ*0.01*EXP(0.18*(TMAX+20.0))
        !  ENDIF
        !  EO = MAX(EO,0.0001)
      ENDIF

      ! Energy budget
      IF (TASKFLAG.EQ.'A'.OR.TASKFLAG.EQ.MEEVP.OR.TASKFLAG.EQ.'C') THEN
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
        vpaircan = CSVPSAT(tcan)                    ! Pa
        hfluxc=(cp*rhoa*(tcan-tair)/(ratm*0.6))     ! Heat J/m2.s
        hfluxc = hfluxc/(1.0E6/(60.0*60.0*24.0))    ! MJ/,2.d <-- J/m2.s
        IF (taskflag.NE.'C') THEN
          ! Calculate leflux (ie.EO}
          lefluxc=(htvap*rhoa*0.622/apress*(vpaircan-vpair)/
     X      (rcrop+ratm*0.6))                       ! H2o,plant J/m2.s
          lefluxc = lefluxc/(1.0E6/(60.0*60.0*24.0))! MJ/,2.d <-- J/m2.s
        ELSEIF (TASKFLAG.EQ.'C') THEN
          ! Use eo brought into routine (Calculate canopy temp)
          lefluxc = eo/1.0E6*lhvap  
        ENDIF
        ulw=sigma*(tcan+273.)**4.   ! Upward long wave radiation
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
      ENDIF

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

