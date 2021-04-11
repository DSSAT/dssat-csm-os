!=======================================================================
!  CROPSIM-CERES CEREAL GROWTH AND DEVELOPMENT MODULE  Version 010115
!
!  Last edit 050415  Changes to the way Gencalc runs handled 
!
!  Developed from Ceres3.5 and Cropsim. 

!  Ceres was modified to fit within the frameworks of Cropsim and CSM, 
!  and to conform to a number of the concepts that were used in the 
!  construction of Cropsim and CSM ... completion of all rate 
!  calculations before state variable updating, no embedding of 
!  variables in the code, modules to read their own inputs and to 
!  generate their own outputs,etc.. In so doing, a number of  formulae 
!  in the original Ceres with embedded coefficients were simplified and 
!  the coefficients placed in external 'coefficient' files (cultivar,
!  ecotype, or species), and some concepts (eg.how vernalization and 
!  senescence are dealt with) and tools from Cropsim were used. This 
!  modified model should thus not be considered as Ceres, but as a 
!  derivative of Ceres best referred to as Cropsim-Ceres rather Ceres 
!  per se.. 
!
!  The module is used within both the Cropsim and CSM models.
! 
!=======================================================================

      ! For Cropsim
!     SUBROUTINE CSCER (FILEIOIN, RUN, TN, RN,             !Command line
!    & ISWWAT, ISWNIT, IDETO, IDETG, IDETL, FROP,          !Controls
!    & SN, ON, RUNI, REP, YEAR, DOY, STEP, CN,             !Run+loop
!    & SRAD, TMAX, TMIN, CO2, RAIN, DEWDUR,                !Weather
!    & DAYLT, WINDSP, ST, EO,                              !Weather
!    & NLAYR, DLAYR, DEPMAX, LL, DUL, SAT, BD, SHF, SLPF,  !Soil states
!    & SNOW, SW, NO3LEFT, NH4LEFT,                         !H2o,N states
!    & YEARPLT, YEARPLTCSM, HARVFRAC,                      !Pl.date
!    & PARIP, EOP, TRWUP,                                  !Resources
!    & LAI, KCAN, KEP,                                     !States
!    & RLV, NFP, RWUPM, RWUMX, CANHT, LAIL,                !States
!    & UNO3ALG, UNH4ALG, UH2O,                             !Uptake
!    & SENCALG, SENNALG, SENLGALG,                         !Senescence
!    & RESCALG, RESNALG, RESLGALG,                         !Residues
!    & STGDOY,                                             !Stage dates
!    & DYNAMIC)                                            !Control

      ! For CSM
      SUBROUTINE CSCER (FILEIOIN, RUN, TN, RN, RNMODE,     !Command line
     & ISWWAT, ISWNIT, IDETS, IDETO, IDETG, IDETL, FROP,   !Controls
     & SN, ON, RUNI, REP, YEAR, DOY, STEP, CN,             !Run+loop
     & SRAD, TMAX, TMIN, CO2, RAIN, TOTIR,                 !Weather
     & DAYLT, WINDSP, ST, EO,                              !Weather
     & NLAYR, DLAYR, DEPMAX, LL, DUL, SAT, BD, SHF, SLPF,  !Soil states
     & SNOW, SW, NO3LEFT, NH4LEFT,                         !H2o,N states
     & YEARPLT, YEARPLTCSM, HARVFRAC,                      !Pl.date
     & EOP, EP, ET, TRWUP,                                 !Resources
     & LAI, KCAN, KEP,                                     !States
     & RLV, NFP, RWUPM, RWUMX, CANHT,                      !States
     & UNO3ALG, UNH4ALG, UH2O,                             !Uptake
     & SENCALG, SENNALG, SENLGALG,                         !Senescence
     & RESCALG, RESNALG, RESLGALG,                         !Residues
     & STGDOY,                                             !Stage dates
     & DYNAMIC)                                            !Control

      ! Substantive changes as worked on comparison
      !   1. Reserves overflow. Introduced
      !   2. Tillering.
      !        Fibonacci factors to match CSCRP
      !        AMIN1 function for stresses (instead of multiplicative)
      !        Assimilates factor talen out
      !   3. Leaf appearance. Fof barley, rate of change of daylength 
      !                       algorithm taken out.
      !   4. Introduced RSUSE to help maintain leaf growth
      
      ! Need to change.
      !   1. Seed reserves (ch2o and N use and early root growth.

      ! For incorporation in CSM should:
      !    Change argument above.
      !    Eliminate '!' from SUMVALS call.
      !    Comment out call to Disease module.
      
      ! Changes for 4.50
      !
      !  A. Changes that should not affect results.
      ! 
      !  1.  Added subroutine code to the main program for those 
      !      subroutines used when running in CSM. Deleted the
      !      subroutines. 
      !  2.  Cleaned the list of variable declarations.
      !  2.  Added a few additional outputs, especially to WORK.OUT
      !  4.  Added outputs (-99's mostly) when run stops because 
      !      of missing weather or equivalent problem.
      !     
      !  A.  Changes that MAY affect results.
      !     
      ! 1.  Took out the 0.6 adjustment for RLWR (SPE file) that stemmed
      !     from the original Ceres 'exudation' loss of dry matter sent 
      !     to the roots. With the 0.6 factor the 'real' RLWR parameter 
      !     would have been 1.633. It was left at 0.98 rather than being
      !     changed to 1.633 because there was very little water stress 
      !     being predicted for experiments in which there should have 
      !     been stress. 
      !     
      ! 2.  Set upper water threshold for photosynthesis (WFPU in ECO
      !     file) to 1.0 following discussion with KJB.  
      !     
      ! 3.  Set upper water threshold for growth (WFGU in ECO
      !     file) to 1.3 following discussion with KJB. 
      !     
      ! 4.  Brought the soil fertility factor SLPF into the
      !     module and now use it to modify photosynthesis in conformity
      !     with other CSM crop modules. 
      !     
      ! 5.  Brought the planting date when running a sequence in CSM 
      !     (ie.under DSSAT) across from the planting section 
      !     of CSM. 
      !     
      ! 6.  Brought the harvest fractions (both product and by-product) 
      !     when running in CSM (IE.under DSSAT) across for
      !     all modes of operation.
      !     
      ! 7.  Changed the planting coding to ensure that neither 
      !     growth nor development occur on the day of planting. This 
      !     contrasts with the previous version, in which growth and 
      !     development ocurred on the day of planting. Phenology will
      !     be affected by this change.
      !     
      ! 8.  Changed the meaning of the so called water factor for root  
      !     growth (WFRG) in the species file. It previously was
      !     interpreted as a multiplier to be applied to the soil water
      !     'potential' to determine the degree to which root 
      !     growth would be reduced below potential. It is now 
      !     interpreted as an upper threshold value that is applied 
      !     to the soil water 'potential' to determine the factor 
      !     (now called the water factor for root growth) used to 
      !     reduce root growth below potential. The change 
      !     alters the degree to which root growth is reduced in 
      !     layers in which the water content is below the upper 
      !     limit. The parameter can be VERY IMPORTANT. It is now
      !     used 'as is' (not the square root) in the algoorithm for
      !     root depth growth.
      !     
      ! 9.  Eliminated the second root growth water factor (WFRDG),which
      !     was used as a multiplier to determine the impact of the 
      !     photosynthesis water factor on root depth growth.   
      !
      ! 10. Took out the N factor from the algorithm determining root
      !     dry matter distribution with depth.
      !
      ! 11. Corrected the algorithm used to calculate water 'potential'
      !     at the seed depth. Previously, if seding was in layer 1, the
      !     algorithm was always giving the potential of layer 2. It
      !     should produce an interpolated value, which hopefully it
      !     now does.
      !
      ! 12. Introduced code to re-initialize the leaf loss through cold
      !     parameter so that leaf area did not continue to be lost 
      !     after a cold spell ended.          

      ! Needed!
      ! 1. Leaf # and time to terminal spikelet too great
      !    when leaf # > 15. Need limit. See LAMS
      ! 2. Yield and kernel # from early and late plantings
      !    high. Need stress factor that affects kernel set
      !    See MCCR
      ! 3. Reserves can go to zero during grain fill, then
      !    recover. Need accelerated senescence! See KSAS.
      ! 4. The saturation factor for water uptake (SATFACL)
      !    gave far too great stress for one Australian data
      !    set (CSGR9802). It is now controlled (for standalone
      !    Cropsim, not CSM) by a switch in the ecotype file 
      !    that is currently set to 0 .. hence no saturation 
      !    effect. Need to use switch in CSM somehow.

      USE OSDefinitions
      USE CSVOUTPUT  ! VSH
      
      IMPLICIT NONE
      SAVE
      
      INTEGER       NL            ! Maximum number of soil layers  #
      PARAMETER     (NL = 20)     ! Maximum number of soil layers  #
      INTEGER       LNUMX         ! Maximum number of leaves       #
      PARAMETER     (LNUMX = 100) ! Maximum number of leaves       #
      INTEGER       SUMNUM        ! Number of variables passed     #
      PARAMETER     (SUMNUM = 37) ! Number of variables passed     #
      CHARACTER*6   ERRKEY

      INTEGER       A1DATM        ! Apex 1cm date,measured         #
      INTEGER       ADAP          ! Anthesis,days after planting   d
      INTEGER       ADAPM         ! Anthesis,DAP,measured          d
      INTEGER       ADAT          ! Anthesis date (Year+doy)       #
      INTEGER       ADAT10        ! Anthesis date (Year+doy) + 10  #
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
      REAL          BD(20)        ! Bulk density (moist)           g/cm3
      CHARACTER*1   BLANK         ! Blank character                text
      REAL          BLAYER        ! Depth at base of layer         cm
      REAL          CANANC        ! Canopy N concentration         %
      REAL          CANHT         ! Canopy height                  cm
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
      INTEGER       CN            ! Crop component (multicrop)     #
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
      REAL          CO2           ! CO2 concentration in air       vpm
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
      INTEGER       CSINCDAT      ! Increment day function output  #
      INTEGER       CSTIMDIF      ! Time difference function       #
      CHARACTER*1   CSWDIS        ! Control switch,disease         code
      INTEGER       CSYDOY        ! Yr+Doy output from function    #
      INTEGER       CSYEARDOY     ! Cropsim function ouptut        #
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
      INTEGER       DAPCALC       ! DAP output from funcion        #
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
      REAL          DAYLT         ! Daylength (6deg below horizon) h
      REAL          DAYLTP        ! Daylength previous day         h
      INTEGER       DAYSC(0:9)    ! Days in growth phase           #
      REAL          DAYSUM        ! Days accumulated in month     
      REAL          DEADN         ! Dead leaf N retained on plant  g/p
      REAL          DEADWT        ! Dead leaf wt.retained on plant g/p
      REAL          DEPMAX        ! Maximum depth of soil profile  cm
      REAL          DEWDUR        ! Dew duration                   h
      REAL          DF            ! Daylength factor 0-1           #
      REAL          DLAYR(20)     ! Depth of soil layers           cm
      REAL          DLAYRTMP(20)  ! Depth of soil layers with root cm
      REAL          DLEAFN        ! Change in leaf N               g/p
      REAL          DMP_EP        ! Dry matter per unit EP         g/mm
      REAL          DMP_ET        ! Dry matter per unit ET         g/mm
      REAL          DMP_IRR       ! Dry matter per unit irrigation g/mm
      REAL          DMP_NAPP      ! Dry matter per unit N applied  kg/kg
      REAL          DMP_NUPT      ! Dry matter per unit N taken uo kg/kg
      REAL          DMP_RAIN      ! Dry matter per unit precip     g/mm
      INTEGER       DOM           ! Day of month                   #
      INTEGER       DOY           ! Day of year                    #
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
      REAL          DUL(20)       ! Drained upper limit for soil   #
      INTEGER       DYNAMIC       ! Program control variable       text
      INTEGER       DYNAMICI      ! Module control,internal        code
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
      REAL          EO            ! Potential evapotranspiration   mm/d
      REAL          EOP           ! Potential evaporation,plants   mm/d
      REAL          EP            ! Actual plant transpiration     mm/d
      REAL          EPC(0:9)      ! Plant evaporation in phase     mm   
      REAL          EPSUM         ! Plant evaporation sum in phase mm
      REAL          EPSUM0        ! Plant evaporation for cycle    mm
      REAL          ERRORVAL      ! Plgro-tfile values/Plgro       #
      REAL          ET            ! Actual total evaporation       mm/d
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
      CHARACTER*250 FILEIOIN      ! Name of input file             text
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
      INTEGER       FNUMWRK       ! File number,work file          #
      LOGICAL       FOPEN         ! File open indicator            code
      INTEGER       FROP          ! Frquency of outputs            d
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
      REAL          HARVFRAC(2)   ! Harvested fr from main         #
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
      CHARACTER*1   IDETG         ! Control flag,growth outputs    code
      CHARACTER*1   IDETL         ! Control switch,detailed output code
      CHARACTER*1   IDETO         ! Control flag,overall outputs   code
      CHARACTER*1   IDETOU        ! Control flag,error op,inputs   code
      CHARACTER*1   IDETS         ! Control switch,summary outputs code
      INTEGER       IEDAT         ! Inflorescence emergenceYeardoy #
      CHARACTER*1   IFERI         ! Fertilizer switch (A,R,D)      code
      CHARACTER*1   IHARI         ! Control flag,harvest           code
      INTEGER       INTEGR        ! Program control variable (=4)  #
      CHARACTER*1   IPLTI         ! Code for planting date method  code
      INTEGER       ISTAGE        ! Developmental stage            #
      INTEGER       ISTAGEP       ! Developmental stage,previous   #
      CHARACTER*1   ISWNIT        ! Soil nitrogen balance switch   code
      CHARACTER*1   ISWWAT        ! Soil water balance switch Y/N  code
      INTEGER       JDAP          ! Jointing date (Year+doy)       #
      INTEGER       JDAPM         ! Jointing date,measured,Yeardoy #
      INTEGER       JDAT          ! Jointing date (Year+doy)       #
      INTEGER       JDATM         ! Jointing date,measured,Yeardoy #
      REAL          KCAN          ! Extinction coeff for PAR       #
      REAL          KCANI         ! Extinction coeff,PAR,init.val. #
      REAL          KEP           ! Extinction coeff for SRAD      #
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
      REAL          LAI           ! Leaf area index                #
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
      REAL          LL(20)        ! Lower limit,soil h2o           #
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
      REAL          NFP           ! N factor,photosynthesis 0-1    #
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
      REAL          NH4LEFT(20)   ! NH4 concentration in soil      mg/Mg
      REAL          NH4MN         ! NH4 conc minimum for uptake    mg/Mg
      INTEGER       NLAYR         ! Number of layers in soil       #
      REAL          NLAYRROOT
      INTEGER       NLIMIT        ! Number of days N limited gr gr #
      REAL          NO3LEFT(20)   ! NO3 concentration in soil      mg/Mg
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
      INTEGER       ON            ! Option number (sequence runs)  #
      INTEGER       ONI           ! Option number,initial value    #
      CHARACTER*3   OUT           ! Output file extension          text
      INTEGER       OUTCHOICE     ! Output choice (+reserves,etc)  #
      INTEGER       OUTCOUNT      ! Output counter                 #
      CHARACTER*77  OUTHED        ! Output file heading            text
      CHARACTER*12  OUTPG         ! Growth output file code        code
      CHARACTER*12  OUTPG2        ! Growth output2 file code       code
      CHARACTER*12  OUTPGF        ! Growth output GF file code     code
      CHARACTER*12  OUTPN         ! GrowthN output file code       code
      INTEGER       OUTPUT        ! Program control variable (=5)  #
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
      REAL          RAIN          ! Rainfall                       mm
      REAL          RAINC         ! Rainfall,cumulative            mm
      REAL          RAINCA        ! Rainfall,cumulativ to anthesis mm
      REAL          RAINCP(0:9)   ! Cumulative precip in phase     mm
      REAL          RAINSUM       ! Precipitation sum in phase     mm
      REAL          RAINSUM0      ! Precipitation during cycle     mm
      REAL          RANC          ! Roots actual N concentration   #
      INTEGER       RATE          ! Program control variable (=3)  #
      REAL          RCNC          ! Root critical N concentration  #
      REAL          RCNCS(0:9)    ! Roots critical N conc,by stage #
      REAL          RCNF          ! Roots N as fr of critical 0-1  #
      REAL          RCNP          ! Root critical N conc,original  #
      REAL          RDGS1         ! Root depth growth rate,initial cm/d
      REAL          RDGS2         ! Root depth growth rate,2nd     cm/d
      REAL          RDGTH         ! Rooting depth growth thrshold  cm
      INTEGER       REP           ! Number of run repetitions      #
      REAL          RESPC         ! Respiration,total,cumulative   g/p
      REAL          RFAC          ! Root length & H2O fac,N uptake #
      REAL          RLDF(20)      ! Root length density fac,new gr #
      REAL          RLDGR         ! Root length/root depth g ratio cm/cm
      REAL          RLFNU         ! Root length factor,N uptake    #
      REAL          RLIGP         ! Root lignin concentration      %
      REAL          RLV(20)       ! Root length volume by layer    /cm2
      REAL          RLWR          ! Root length/weight ratio       m/mg?
      REAL          RMNC          ! Root minimum N conc            g/g
      REAL          RMNCS(0:9)    ! Roots minimum N conc,by stage  #
      REAL          RMNP          ! Root minimum N conc,original   g/g
      INTEGER       RN            ! Treatment replicate            #
      REAL          RNAM          ! Root N at maturity             kg/ha
      REAL          RNAMM         ! Root N at maturity,measured    kg/ha
      REAL          RNDEM         ! Root demand for N              g/p
      REAL          RNDEM1        ! Root demand for N,overall      g/p
      REAL          RNDEMSOIL     ! Root demand for N,soil         g/p
      REAL          RNH4U(20)     ! Potential ammonium uptake      kg/ha
      INTEGER       RNI           ! Replicate number,initial value #
      CHARACTER*1   RNMODE        ! Run mode (eg.I=interactive)    #
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
      INTEGER       RUN           ! Run (from command line) number #
      INTEGER       RUNI          ! Run (internal for sequences)   #
      INTEGER       RUNINIT       ! Program control variable (= 1) #
      CHARACTER*25  RUNNAME       ! Run title                      text
      CHARACTER*8   RUNRUNI       ! Run+internal run number        text
      REAL          RWAM          ! Root wt at maturity            kg/ha
      REAL          RWAMM         ! Root wt at maturity,measured   kg/ha
      REAL          RWUMX         ! Root water uptake,maximum      mm2/m
      REAL          RWUMXI        ! Root water uptake,max,init.val mm2/m
      REAL          RWUMXS        ! Root water uptake,maximum,std  mm2/m
      REAL          RWUPM         ! Pore space threshold,pl effect #
      REAL          RWUPMI        ! Pore space threshold,initial   #
      REAL          SAID          ! Stem area index                m2/m2
      REAL          SAIDOUT       ! Stem area index for output     m2/m2
      REAL          SANC          ! Stem N concentration           #
      REAL          SAT(20)       ! Saturated limit,soil           #
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
      INTEGER       SEASEND       ! Program control variable (= 6) #
      CHARACTER*1   SEASENDOUT    ! Season end outputs flag        text
      INTEGER       SEASINIT      ! Program control variable (=2)  #
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
      REAL          SHF(20)       ! Soil hospitality factor 0-1    #
      REAL          SHRTD         ! Shoot/root ratio               #
      REAL          SLA           ! Specific leaf area             cm2/g
      REAL          SLAOUT        ! Specific leaf area for output  cm2/g
      REAL          SLIGP         ! Stem lignin concentration      %
      REAL          SLPF          ! Soil factor for photosynthesis %
      REAL          SMDFR         ! Soil moisture factor,N uptake  #
      REAL          SMNC          ! Stem minimum N conc            #
      REAL          SMNCS(0:9)    ! Stem minimum N conc,stage      #
      INTEGER       SN            ! Sequence number,crop rotation  #
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
      REAL          SNOW          ! Snow                           cm
      REAL          SNPCMN(0:9)    ! Stem minimum N conc,stage     %
      REAL          SNPCS(0:9)    ! Stem standard N conc,stage     %
      INTEGER       SPDATM        ! Spike emergence date,measured  #
      CHARACTER*250 SPDIRFLE      ! Species directory+file         text
      CHARACTER*250 SPDIRFLP      ! Species directory+file,last    text
      CHARACTER*12  SPFILE        ! Species filename               text
      REAL          SRAD          ! Solar radiation                MJ/m2
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
      REAL          ST(0:NL)      ! Soil temperature in soil layer C
      INTEGER       STARNUM       ! Star line number,as read file  #
      INTEGER       STARNUMM      ! Star line number,measured data #
      INTEGER       STARNUMO      ! Star line number,output file   #
      REAL          STDAY         ! Standard day                   C.d/d
      REAL          STEMN         ! Stem N                         g/p
      INTEGER       STEP          ! Step number                    #
      INTEGER       STEPNUM       ! Step number per day            #/d
      REAL          STFR(10)      ! Stem fractions of assimilates  #
      REAL          STFRSTG       ! Stem fraction,today's stage    #
      INTEGER       STGDOY(20)    ! Stage dates (Year+Doy)         #
      CHARACTER*10  STNAME(20)    ! Stage names                    text
      REAL          STRESS(20)    ! Minim H2O,N factors,last 20day # 
      REAL          STRESS20      ! Minim H2O,N factors,20day avg  #
      REAL          STRESS20A     ! Minim H2O,N factors,20day avg  #
      REAL          STRESS20S     ! Minim H2O,N factors,20day sum  #
      REAL          STRSWT        ! Stem reserves                  g/p
      REAL          STWT          ! Stem weight                    g/p
      REAL          SW(20)        ! Soil water content             #
      REAL          SWDF          ! Soil water deficit factor 0-1  #
      REAL          SWP(0:20)     ! Soil water 'potential'         #
      REAL          SWPLTD        ! Depth for average soil water   cm
      REAL          SWPLTH        ! Upper limit on soil water,plt  %
      REAL          SWPLTL        ! Lower limit on soil water,plt  %
      REAL          SWPSD         ! Soil water potential at seed   #
      CHARACTER*6   TCHAR         ! Temporary character string     #
      REAL          TCNP          ! Critical N concentration       %
      REAL          TDAY          ! Temperature during light hours C
      REAL          TFAC4         ! Temperature factor function    #
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
      CHARACTER*10  TL10FROMI     ! Temporary line from integer    text
      INTEGER       TLIMIT        ! Number of days temp limited gr #
      CHARACTER*180 TLINEGRO      ! Temporary line from GRO file   text
      INTEGER       TLINENUM      ! Temporary var,# lines in tfile #
      CHARACTER*180 TLINET        ! Temporary line from T-file     text
      CHARACTER*180 TLINETMP      ! Temporary line                 #
      INTEGER       TLPOS         ! Position on temporary line     #
      REAL          TMAX          ! Temperature maximum            C
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
      REAL          TMIN          ! Temperature minimum            C
      REAL          TMINAV(0:9)   ! Average minimum temp in phase  C
      REAL          TMINM         ! Temperature minimum,monthly av C
      REAL          TMINN         ! Temperature min during season  C
      REAL          TMINSUM       ! Temperature minimum,summed     C
      REAL          TMINSUMP      ! Tmin sum in phase              oC
      REAL          TMINSUMP0     ! Tmin sum for growth cycle      oC
      REAL          TMNC          ! Tmin cumulative                oC 
      INTEGER       TN            ! Treatment number               #
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
      REAL          TOTIR         ! Total irrigation during cycle  mm 
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
      REAL          TRWUP         ! Total water uptake,potential   cm
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
      INTEGER       TVICOLNM      ! Column number function output  #
      INTEGER       TVILENT       ! Temporary integer,function op  #
      REAL          TVR1          ! Temporary real variable        #
      REAL          TVR2          ! Temporary real variable        #
      REAL          TVR3          ! Temporary real variable        #
      REAL          TVR4          ! Temporary real variable        #
      REAL          TVR5          ! Temporary real variable        #
      REAL          UH2O(NL)      ! Uptake of water                cm/d
      REAL          VALUE(SUMNUM) ! Value of variable passed       #
      INTEGER       VALUEI        ! Output from Getstri function   #
      REAL          VALUER        ! Output from Getstrr function   #
      REAL          VALUERR       ! Value of error                 #
      REAL          VANC          ! Vegetative actual N conc       #
      CHARACTER*6   VARNO         ! Variety identification code    text
      CHARACTER*6   VARNOP        ! Variety identification code    text
      REAL          VCNC          ! Vegetative critical N conc     #
      REAL          VDLOST        ! Vernalization lost (de-vern)   d
      INTEGER       VERSION       ! Version #                      #
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
      REAL          WINDSP        ! Wind speed                     km/d
      REAL          WUPR          ! Water pot.uptake/demand        #
      REAL          XDEP          ! Depth to bottom of layer       cm
      REAL          XDEPL         ! Depth to top of layer          cm
      REAL          XMIN          ! Minimum NO3,NH4-N for uptake   mg/Mg
      REAL          XNFS          ! N labile fraction,standard     #
      REAL          XSTAGE        ! Stage of development           #
      REAL          XSTAGEFS      ! Xstage when final sen started  #
      REAL          XSTAGEP       ! Stage of development,previous  #
      INTEGER       YEAR          ! Year                           #
      INTEGER       YEARCOL       ! Colum number for year data     #
      INTEGER       YEARDOY       ! Year+Doy (7digits)             #
      INTEGER       YEARHARF      ! Harvest year+doy,fixed         #
      INTEGER       YEARPLT       ! Year(or Yr)+Doy,planting date  #
      INTEGER       YEARPLTP      ! Year(or Yr)+Doy,planting trget #
      INTEGER       YEARSIM       ! Year+Doy for simulation start  #
      INTEGER       YRHARF        ! Harvest date,fixed             #
      CHARACTER*1   YRHARFF       ! Year+Doy flag for message      text
      INTEGER       YEARPLTCSM    ! Year+Doy,planting date,CSM     #
      REAL          YSTAGE        ! Previous Zadoks calc variable  # 
      REAL          YVAL1         ! Output from array function     #
      REAL          YVALXY        ! Y value from function          #
      REAL          ZS2           ! Previous Zadoks calc variable  #
      REAL          ZS3           ! Previous Zadoks calc variable  #
      REAL          ZS4           ! Previous Zadoks calc variable  # 
      REAL          ZSTAGE        ! Zadoks stage of development    #
      REAL          ZSTAGEP       ! Zadoks stage,previous day      #
 
      ! Variables expressed on a per hectare basis
      ! Inputs
      REAL          AMTNIT        ! Cumulative amount of N applied kg/ha
      REAL          ANFER(200)    ! N amount in fertilizer appln   kg/ha
      ! Outputs
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
      REAL          RESCALG(0:20) ! Residue C added,by layer       kg/ha
      REAL          RESLGAL(0:20) ! Residue lignin,harvest,bylayer kg/ha
      REAL          RESLGALG(0:20)! Residue lignin added,layer     kg/ha
      REAL          RESNAL(0:20)  ! Residue N at harvest by layer  kg/ha
      REAL          RESNALG(0:20) ! Residue N added,by layer       kg/ha
      REAL          RESPAC        ! Respiration,total,cumulative   kg/ha
      REAL          RESWAL(0:20)  ! Residue om added by layer      kg/ha
      REAL          RESWALG(0:20) ! Residue om at harvest,by layer kg/ha
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
      REAL          SENCALG(0:20) ! Senesced C added,by layer      kg/ha
      REAL          SENCAS        ! Senesced C added to soil       kg/ha
      REAL          SENLGAL(0:20) ! Senesced lignin added,by layer kg/ha
      REAL          SENLGALG(0:20)! Senesced lignin added,layer    kg/ha
      REAL          SENLGAS       ! Senesced lignin added to soil  kg/ha
      CHARACTER*6   SENN0C        ! Senesced N added to litter     kg/ha
      REAL          SENNAL(0:20)  ! Senesced N,by layer            kg/ha
      REAL          SENNALG(0:20) ! Senesced N added,by layer      kg/ha
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
      REAL          UNH4ALG(20)   ! Uptake of NH4 N                kg/ha
      REAL          UNO3ALG(20)   ! Uptake of NO3 N                kg/ha
      REAL          VEFF          ! Vernalization effect,max.reduc fr
      REAL          VNAD          ! Vegetative canopy nitrogen     kg/ha
      REAL          VWAD          ! Vegetative canopy weight       kg/ha
      REAL          VRNSTAGE      ! Vernalization stage            #

!!     2021-02-14 chp
!      REAL          Nuptake_daily !Daily N uptake (kg [N]/ha)
!      REAL          NUAD_Y        !Yesterday's cumulative N uptake

      PARAMETER     (BLANK = ' ')
      PARAMETER     (RUNINIT = 1)
      PARAMETER     (SEASINIT = 2)
      PARAMETER     (RATE = 3)
      PARAMETER     (INTEGR = 4)
      PARAMETER     (OUTPUT = 5)
      PARAMETER     (SEASEND = 6)
      PARAMETER     (ERRKEY = 'CSCER ')

      ! Condition at end of phase
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

      YEARDOY = YEAR*1000 + DOY
        
      IF (DYNAMIC.EQ.RUNINIT .OR. DYNAMIC.EQ.SEASINIT) THEN

        FROPADJ = FROP
        IF (RNMODE.EQ.'T') FROPADJ = 1
        IF (IDETL.EQ.'D'.OR.IDETL.EQ.'A') FROPADJ = 1

        ! Following is used because Dssat has seasinit after runinit

        IF (CFLINIT.EQ.'Y') THEN
          TN = TNI
          CN = CNI
          SN = SNI
          ON = ONI
          RN = RNI
          KCAN = KCANI
          KEP = KEPI
          RWUPM = RWUPMI
          RWUMX = RWUMXI
          STGDOY = 9999999
          RETURN
        ENDIF

        ! Open Work.out file
        IF (FNUMWRK.LE.0.OR.FNUMWRK.GT.1000) 
     &    CALL Getlun ('WORK.OUT',fnumwrk)
        INQUIRE (FILE = 'WORK.OUT',OPENED = fopen)
        IF (.NOT.fopen) THEN
          IF (RUN.EQ.1) THEN
            OPEN (UNIT = fnumwrk,FILE = 'WORK.OUT')
            WRITE(fnumwrk,*) 'CSCER  Cropsim-Ceres Crop Module '
          ELSE
            OPEN (UNIT = fnumwrk,FILE = 'WORK.OUT',POSITION='APPEND',
     &      ACTION = 'READWRITE')
            WRITE(fnumwrk,*) ' '
            WRITE(fnumwrk,*) 'CSCER  Cropsim-Ceres Crop Module '
          ENDIF
        ENDIF

! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.          
!        ! Set Reads file #
!        IF (FNUMREA.LE.0.OR.FNUMREA.GT.1000) 
!     &      CALL Getlun('READS.OUT',fnumrea)
        ! Set temporary file #
        IF (FNUMTMP.LE.0.OR.FNUMTMP.GT.1000) 
     &      CALL Getlun ('FNAMETMP',fnumtmp)
      
        ! Set input file type
        TVI1 = TVILENT(FILEIOIN)
        IF (FILEIOIN(TVI1-2:TVI1).EQ.'INP') THEN
          FILEIOIN(TVI1:TVI1) = 'H'
          fileiot = 'DS4'
        ELSE
          fileiot = 'XFL'
        ENDIF
        FILEIO = ' '
        FILEIO(1:TVI1) = FILEIOIN(1:TVI1)

        IF (DYNAMIC.EQ.RUNINIT) THEN

          MODNAME = 'CSCER047'
          VERSION = 010115
          GENFLCHK(3:15) = 'CER047.08102017'

          ! Parameters
          STDAY = 20.0    ! TT in standard day
          PARADFAC = 0.5  ! PAR in SRAD (fr)
          STEPNUM = 1     ! Step number per day set to 1

          YEARSIM = YEARDOY

          ! Control switches for error outputs and input echo
          IF (FILEIOT.NE.'DS4') THEN
            CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'OUTPUTS',idetou)
          ENDIF

        ENDIF
        
        ISTAGE = 7   ! Pre-planting
        XSTAGE = 7.0 ! Pre-planting
        YEARPLT = 9999999
        YEARPLTP = 9999999
        DYNAMICI = DYNAMIC

        SEASENDOUT = 'N'  ! Season end outputs flag     
        
        adat = -99
        adat10 = -99
        adatend = -99
        amtnit = 0.
        awnai = 0.0          ! awns not yet used
        canht = 0.0
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
        dewdur = -99.0
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
        lai = 0.0
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
        nfp = 1.0
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
        rescalg = 0.0
        reslgal = 0.0
        reslgalg = 0.0
        resnal = 0.0
        resnalg = 0.0
        respc = 0.0
        reswal = 0.0
        reswalg = 0.0
        rlv = 0.0
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
        snow = 0.0
        srad20a = -99.0
        sradc = 0.0
        sradd = 0.0
        stemn = 0.0
        stgdoy = 9999999
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
        trwup = 0.0
        tsdat = -99
        tt = 0.0
        tt20 = -99.0
        ttd = 0.0
        ttnum = 0.0
        uh2o = 0.0
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


        IF (RUN.EQ.1.AND.RUNI.LE.1) THEN
          CFGDFILE = ' '
          CUFILE = ' '
          CUDIRFLE = ' '
          CROPP = '  '
        ENDIF

        ! Planting information
        CALL XREADC(FILEIO,TN,RN,SN,ON,CN,'PLANT',iplti)
!       IF(IPLTI.EQ.'A')THEN
        IF(IPLTI.EQ.'A' .OR. IPLTI.EQ.'F')THEN
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'PFRST',pwdinf)
C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY          
          !PWDINF = CSYEARDOY(pwdinf)
          CALL Y4K_DOY(pwdinf,FILEIO,0,ERRKEY,3)
!          CALL CSYR_DOY(PWDINF,PWYEARF,PWDOYF)
          CALL YR_DOY(PWDINF,PWYEARF,PWDOYF)
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'PLAST',pwdinl)
          !PWDINL = CSYEARDOY(pwdinl)
          CALL Y4K_DOY(pwdinl,FILEIO,0,ERRKEY,3)
!          CALL CSYR_DOY(PWDINL,PWYEARL,PWDOYL)
          CALL YR_DOY(PWDINL,PWYEARL,PWDOYL)
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'PH2OL',swpltl)
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'PH2OU',swplth)
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'PH2OD',swpltd)
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'PSTMX',ptx)
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'PSTMN',pttn)
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'HFRST',hfirst)
          !HFIRST = CSYEARDOY(hfirst)
          CALL Y4K_DOY(hfirst,FILEIO,0,ERRKEY,3)
!          CALL CSYR_DOY(HFIRST,HYEARF,HDOYF)
          CALL YR_DOY(HFIRST,HYEARF,HDOYF)
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'HLAST',hlast)
          !HLAST = CSYEARDOY(hlast)
          CALL Y4K_DOY(hlast,FILEIO,0,ERRKEY,3)
!          CALL CSYR_DOY(HLAST,HYEARL,HDOYL)
          CALL YR_DOY(HLAST,HYEARL,HDOYL)
          !IF (DYNAMIC.EQ.SEASINIT) THEN
          ! IF (PWDINF.GT.0 .AND. PWDINF.LT.YEARDOY) THEN
          !   WRITE (fnumwrk,*) ' '
          !   WRITE (fnumwrk,*) 'Automatic planting set: '              
          !   WRITE (fnumwrk,*)'PFIRST,PLAST AS READ  : ',pwdinf,pwdinl
          !   WRITE (fnumwrk,*)'HFIRST,HLAST AS READ  : ',hfirst,hlast
          !   TVI1 = INT((YEARDOY-PWDINF)/1000)
          !   PWDINF = PWDINF + TVI1*1000
          !   PWDINL = PWDINL + TVI1*1000
          !   IF (HFIRST.GT.0) HFIRST = HFIRST + TVI1*1000
          !   HLAST  = HLAST + (TVI1+1)*1000
          ! ENDIF
          ! WRITE (fnumwrk,*) 'PFIRST,PLAST AS USED  : ',pwdinf,pwdinl
          ! WRITE (fnumwrk,*) 'HFIRST,HLAST AS USED  : ',hfirst,hlast
          !ENDIF
        ELSE
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'PDATE',pdate)
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'EDATE',emdatm)
!          CALL CSYR_DOY(PDATE,PLYEAR,PLDAY)
          CALL YR_DOY(PDATE,PLYEAR,PLDAY)
          PLYEARREAD = PLYEAR
          
          ! CHP 2/13/2009 - increment yr for seasonal and sequence runs
          ! IF (INDEX('FQN',RNMODE) > 0) THEN
          ! What about multiple years with other RNMODE's?  This only 
          !   fixes sequence and seasonal runs.
          ! CHP 5/4/09 - for DSSAT runs, always set PLYEAR = YEAR
          ! 09/28/2009 CHP need to account for planting date >> simulation date.
          !  IF (FILEIOT(1:2).EQ.'DS') THEN
          !LPM 07/17/20 - account for simulation date when is a year before planting date
          !Avoid wrong value of yeardoyharf
          IF (FILEIOT(1:2) == 'DS' .AND. YEAR > PLYEAR) THEN
              IF (YEAR < PLYEARREAD) THEN
                  PLYEAR = PLYEARREAD
              ELSE
                  PLYEAR = YEAR
              ENDIF
          ENDIF

          IF (PLDAY.GE.DOY) THEN
            YEARPLTP = PLYEAR*1000 + PLDAY
          ELSEIF (PLDAY.LT.DOY) THEN
            YEARPLTP = (YEAR+1)*1000 + PLDAY
          ENDIF
        ENDIF

        arg = ' '
        tvi2 = 0
        tvi3 = 0
        tvi4 = 0
        call getarg(0,arg)
        arglen = len_trim(arg)

        DO tvi1 = 1,arglen
          IF (arg(tvi1:tvi1).EQ. SLASH) tvi2=tvi1
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
        
        ! Re-open Work.out and Reads.out if only require 1 run info..   
        IF (IDETL.EQ.'0'.OR.IDETL.EQ.'Y'.OR.IDETL.EQ.'N') THEN
          CLOSE (FNUMWRK, STATUS = 'DELETE')
          OPEN (UNIT = FNUMWRK,FILE = 'WORK.OUT', STATUS = 'NEW',
     &      ACTION = 'READWRITE')
          WRITE(FNUMWRK,*) 'CSCER  Cropsim-Ceres Crop Module '
! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.          
!          CLOSE (FNUMREA, STATUS = 'DELETE')
!          OPEN (UNIT = FNUMREA,FILE = 'READS.OUT', STATUS = 'NEW',
!     &      ACTION = 'READWRITE')
!          WRITE(FNUMREA,*)' '
!          WRITE(FNUMREA,*)
!     &      ' File closed and re-opened to avoid generating huge file'
        ENDIF

        ! Create composite run variable
        IF (RUNI.LT.10) THEN
          WRITE (RUNRUNI,'(I3,A1,I1,A3)') MOD(RUN,1000),',',RUNI,'   '
        ELSEIF (RUNI.GE.10.AND.RUNI.LT.100) THEN
          WRITE (RUNRUNI,'(I3,A1,I2,A2)') MOD(RUN,1000),',',RUNI,'  '
        ELSE
          WRITE (RUNRUNI,'(I3,A1,I3,A1)') MOD(RUN,1000),',',RUNI,' '
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

        CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'CR',crop)
        CALL UCASE (CROP)
        GENFLCHK = CROP//GENFLCHK(3:15)
        CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'INGENO',varno)
        IF (varno.EQ.'-99   ') THEN
          WRITE(fnumwrk,*)' '
          WRITE(fnumwrk,*)'Cultivar number not found!'
          WRITE(fnumwrk,*)'Maybe an error in the the X-file headings'
          WRITE(fnumwrk,*)'(eg.@-line dots connected to next header)'
          WRITE(fnumwrk,*)'Please check'
          WRITE (*,*) ' Problem reading the X-file'
          WRITE (*,*) ' Cultivar number not found!'
          WRITE (*,*) ' Maybe an error in the the X-file headings'
          WRITE (*,*) ' (eg.@-line dots connected to next header)'
          WRITE (*,*) ' Program will have to stop'
          WRITE (*,*) ' Check WORK.OUT for details of run'
          STOP ' '
        ENDIF
        IF (varno.EQ.'-99   ') THEN
          WRITE(fnumwrk,*)' '
          WRITE(fnumwrk,*)'Cultivar number not found!'
          WRITE(fnumwrk,*)'Maybe an error in the the X-file headings'
          WRITE(fnumwrk,*)'(eg.@-line dots connected to next header)'
          WRITE(fnumwrk,*)'Please check'
          WRITE (*,*) ' Problem reading the X-file'
          WRITE (*,*) ' Cultivar number not found!'
          WRITE (*,*) ' Maybe an error in the the X-file headings'
          WRITE (*,*) ' (eg.@-line dots connected to next header)'
          WRITE (*,*) ' Program will have to stop'
          WRITE (*,*) ' Check WORK.OUT for details of run'
          STOP ' '
        ENDIF
        CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'CNAME',vrname)
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPOP',pltpopp)
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PLRS',rowspc)
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PLDP',sdepth)
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PLWT',sdrate)
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PLMAG',plmage)
        IF (PLMAGE.LE.-90.0) THEN
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PAGE',plmage)
          IF (PLMAGE.LE.-99.0) PLMAGE = 0.0
        ENDIF

        CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'TNAME',tname)
        CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'SNAME',runname)

        CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'ENAME',ename)
        CALL LTRIM (ENAME)
        CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'EXPER',excode)
        CALL UCASE (EXCODE)

        CALL XREADI (FILEIO,TN,RN,SN,ON,CN,'HDATE',yrharf)
        !YEARHARF = CSYEARDOY(yrharf)
C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY        
        CALL Y4K_DOY(yrharf,FILEIO,0,ERRKEY,3)
!        CALL CSYR_DOY(YRHARF,HYEAR,HDAY)
        CALL YR_DOY(YRHARF,HYEAR,HDAY)
        PLTOHARYR = HYEAR - PLYEARREAD
        ! Upgrade harvest date for seasonal and sequential runs
        yearharf = (plyear+pltoharyr)*1000 +hday
        
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'HPC',hpc)
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'HBPC',hbpc)
        CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'HARVS',ihari)
        IF (hpc .LT. 0.0) hpc = 100.0   ! Harvest %
        IF (hbpc .LT. 0.0) hbpc = 0.0
        ! If running CSM use harvfrac so as to handle automatic mngement
        IF (FILEIOT .NE. 'DS4') THEN
          hpc = harvfrac(1)*100.0   ! Harvest %
          hbpc = harvfrac(2)*100.0
        ENDIF  

        ! Read fertilizer info (for calculation of N appln during cycle)
        CALL XREADC(FILEIO,TN,RN,SN,ON,CN,'FERTI',iferi)
        CALL XREADIA(FILEIO,TN,RN,SN,ON,CN,'FDATE','200',fday)
        CALL XREADRA(FILEIO,TN,RN,SN,ON,CN,'FAMN','200',anfer)
        NFERT = 0
        DO I = 1, 200
          IF (anfer(I).LE.0.0) EXIT
C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY          
          !FDAY(I) = CSYEARDOY(fday(i))
          CALL Y4K_DOY(fday(i),FILEIO,0,ERRKEY,3)
          NFERT = NFERT + 1
        ENDDO

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

        IF (FILEIOT(1:2).EQ.'DS') THEN
          !IF (CROP.NE.CROPP .OR. VARNO.NE.VARNOP) THEN
            CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'CFILE',cufile)
            CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'CDIR',pathcr)

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

            CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'EFILE',ecfile)
            CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'EDIR',pathcr)

            PATHL = INDEX(PATHCR,BLANK)
            IF (PATHL.LE.5.OR.PATHCR(1:3).EQ.'-99') THEN
              ECDIRFLE = ECFILE
            ELSE
              IF (PATHCR(PATHL-1:PATHL-1) .NE. SLASH) THEN
                ECDIRFLE = PATHCR(1:(PATHL-1)) // SLASH // ECFILE
              ELSE
                ECDIRFLE = PATHCR(1:(PATHL-1)) // ECFILE
              ENDIF
            ENDIF

            CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'SPFILE',spfile)
            CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'SPDIR',pathcr)

            PATHL = INDEX(PATHCR,BLANK)
            IF (PATHL.LE.5.OR.PATHCR(1:3).EQ.'-99') THEN
              SPDIRFLE = SPFILE
            ELSE
              IF (PATHCR(PATHL-1:PATHL-1) .NE. SLASH) THEN
                SPDIRFLE = PATHCR(1:(PATHL-1)) // SLASH // SPFILE
              ELSE
                SPDIRFLE = PATHCR(1:(PATHL-1)) // SPFILE
              ENDIF
            ENDIF
          !ENDIF
          
          ! Ecotype coefficients re-set
          PD = -99
          PD2FR = -99 
          PD(2) = -99 
          PD(3) = -99 
          PD(4) = -99
          PD4FR = -99
          paruv = -99
          parur = -99
          lapot = -99
          laws = -99
          lafv = -99
          lafr = -99
          veff = -99
          grnmn = -99
          grns = -99
          nfpu = -99
          nfpl = -99
          nfgu = -99
          nfgl = -99
          rdgs1 = -99
          rtno3 = -99
          rtnh4 = -99
          ppfpe = -99
          P4SGE = -99
          canhts = -99
          awns = -99
          kcan = -99
          tvr1 = -99
          rspcs = -99
          ti1lf = -99
          wfpu = -99
          wfgu = -99
          wfpgf = -99
          lt50h = -99
          rdgs2 = -99
          tifac = -99
          tilpe = -99
          tilds = -99
          tilde = -99
          tildf = -99
          lsens = -99
          lsene = -99
          phintf = -99
          phintl = -99

          ! Species re-set
          PD4FR = -99
          ppfpe = -99
          lsens = -99
          lsene = -99
          lwlos = -99
          tildf = -99
          lt50s = -99
          tklf = -99
          hdur = -99
          pd2fr = -99
          p4sge = -99
          nfgu = -99
          nfgl = -99
          nfpu = -99
          nfpl = -99
          wfpu = -99
          wfpu = -99
          wfpgf = -99
          tvr1 = -99
          nftu = -99
          nftl = -99
          wfsu = -99
          nfsu = -99
          wfnuu = -99
          rlfnu = -99
          ncnu = -99
          xnfs = -99
          rtno3 = -99
          rtnh4 = -99
 
          IF (RNMODE.NE.'T') CALL FVCHECK(CUDIRFLE,GENFLCHK)

          CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'ECO#',econo)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P1V',p1v)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P1D',p1d)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P5',pd(5))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'G1',g1cwt)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'G2',g2kwt)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'G3',g3)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PHINT',phints)
          ! Eco characteristics that may temporarily added to cul file
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P1',pd(1))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P2',pd(2))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P2FR1',pd2fr(1))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P3',pd(3))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P4',pd(4))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P4FR1',pd4fr(1))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P4FR2',pd4fr(2))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'VEFF',veff)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PARUE',paruv)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PARU2',parur)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LA1S',lapot(1))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'SLAS',laws)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LAFV',lafv)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LAFR',lafr)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'TBAM',tbam)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'GN%S',grns)
          ! N uptake variables in species file
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NCNU',ncnu)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'RLFNU',rlfnu)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'WFNUU',wfnuu)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFPU',nfpu)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFGU',nfgu)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFTU',nftu)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFPL',nfpl)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFGL',nfgl)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFTL',nftl)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'RTNO3',rtno3)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'RTNH4',rtnh4)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NSFAC',nsfac)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NMNFC',nmnfc)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NLAB%',xnfs)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'RDGS',rdgs1)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'GN%MN',grnmn)
          ! NB. TBAM is only used experimentally;should not be in coeff.files
          
          CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'ADIR',fileadir)

        ELSE

          IF (CUDIRFLE.NE.CUDIRFLP .OR. VARNO.NE.VARNOP) THEN
            arg=' '
            tvi3=0
            ! CALL GETARG (0,ARG,ARGLEN)
            ! Portability
            call getarg(0,arg)
            arglen = len_trim(arg)

            DO tvi1 = 1,arglen
              IF(arg(tvi1:tvi1).EQ. SLASH)tvi2=tvi1
              IF(arg(tvi1:tvi1).EQ.'.')tvi3=tvi1
            ENDDO
            IF(TVI3.EQ.0)then
              tvi3=arglen+1
              ARG(TVI3:TVI3)='.'
            ENDIF
            cfgdfile = ' '
            cfgdfile = ARG(1:TVI3)//'CFG'
            ! Change cfg file name from module specific to general
            DO L = LEN(TRIM(CFGDFILE)),1,-1
              IF (CFGDFILE(L:L).EQ. SLASH) EXIT
            ENDDO
            IF (L.GT.1) THEN
              cfgdfile = CFGDFILE(1:L-1)// SLASH //'CROPSIM.CFG'
            ELSE
              cfgdfile(1:11) = 'CROPSIM.CFG'
            ENDIF
            INQUIRE (FILE = cfgdfile,EXIST = fflag)
            IF (.NOT.fflag) THEN
              WRITE (fnumwrk,*)
     &         'Could not find Cfgdfile: ',cfgdfile(1:60)
              WRITE (*,*) ' Could not find Cfgdfile: ',cfgdfile(1:60)
              WRITE (*,*) ' Program will have to stop'
              WRITE (*,*) ' Check WORK.OUT for details of run'
              STOP ' '
            ELSE
              WRITE (fnumwrk,*) ' Config.file: ',CFGDFILE(1:60)
            ENDIF

            cufile = crop//modname(3:8)//'.CUL'
            INQUIRE (FILE = cufile,EXIST = fflag)
            IF (fflag) THEN
              cudirfle = cufile
            ELSE
              CALL Finddir (fnumtmp,cfgdfile,'CRD',cufile,cudirfle)
            ENDIF
            !IF (RNMODE.EQ.'G'.OR.RNMODE.EQ.'T') THEN  LAH
            !  CUFILE = 'GENCALC2.CUL'
            !  CUDIRFLE = ' '
            !  CUDIRFLE(1:12) = CUFILE
            !ENDIF

            ecfile = crop//modname(3:8)//'.ECO'
            INQUIRE (FILE = ecfile,EXIST = fflag)
            IF (fflag) THEN
              ecdirfle = ecfile
            ELSE
              CALL Finddir (fnumtmp,cfgdfile,'CRD',ecfile,ecdirfle)
            ENDIF

            spfile = crop//modname(3:8)//'.SPE'
            INQUIRE (FILE = ecfile,EXIST = fflag)
            IF (fflag) THEN
              spdirfle = spfile
            ELSE
              CALL Finddir (fnumtmp,cfgdfile,'CRD',spfile,spdirfle)
            ENDIF
          ENDIF

        ENDIF     ! End Genotype file names creation

        IF (RNMODE.EQ.'G'.OR.RNMODE.EQ.'T') THEN
          CUFILE = 'GENCALC2.CUL'
          CUDIRFLE = ' '
          CUDIRFLE(1:12) = CUFILE
        ENDIF

        IF (FILEIOT .NE. 'DS4') THEN
          !IF (CUDIRFLE.NE.CUDIRFLP .OR. VARNO.NE.VARNOP) THEN
           IF (RNMODE.NE.'T') CALL FVCHECK(CUDIRFLE,GENFLCHK)
            WRITE (fnumwrk,*) ' '
            CALL CUREADC (CUDIRFLE,VARNO,'ECO#',econo)
            CALL CUREADR (CUDIRFLE,VARNO,'P1V',p1v)
            CALL CUREADR (CUDIRFLE,VARNO,'P1D',p1d)
            CALL CUREADR (CUDIRFLE,VARNO,'P5',pd(5))
            CALL CUREADR (CUDIRFLE,VARNO,'G1',g1cwt)
            CALL CUREADR (CUDIRFLE,VARNO,'G2',g2kwt)
            CALL CUREADR (CUDIRFLE,VARNO,'G3',g3)
            CALL CUREADR (CUDIRFLE,VARNO,'PHINT',phints)
          ! Eco characteristics that may temporarily added to cul file
            CALL CUREADR (CUDIRFLE,VARNO,'P1',pd(1))
            CALL CUREADR (CUDIRFLE,VARNO,'P2',pd(2))
            CALL CUREADR (CUDIRFLE,VARNO,'P2FR1',pd2fr(1))
            CALL CUREADR (CUDIRFLE,VARNO,'P3',pd(3))
            CALL CUREADR (CUDIRFLE,VARNO,'P4',pd(4))
            CALL CUREADR (CUDIRFLE,VARNO,'P4FR1',pd4fr(1))
            CALL CUREADR (CUDIRFLE,VARNO,'P4FR2',pd4fr(2))
            CALL CUREADR (CUDIRFLE,VARNO,'VEFF',veff)
            CALL CUREADR (CUDIRFLE,VARNO,'PARUE',paruv)
            CALL CUREADR (CUDIRFLE,VARNO,'PARU2',parur)
            CALL CUREADR (CUDIRFLE,VARNO,'LA1S',lapot(1))
            CALL CUREADR (CUDIRFLE,VARNO,'SLAS',laws)
            CALL CUREADR (CUDIRFLE,VARNO,'LAFV',lafv)
            CALL CUREADR (CUDIRFLE,VARNO,'LAFR',lafr)
            CALL CUREADR (CUDIRFLE,VARNO,'TBAM',tbam)
            CALL CUREADR (CUDIRFLE,VARNO,'GN%S',grns)
            ! N uptake variables in species file
            CALL CUREADR (CUDIRFLE,VARNO,'NCNU',ncnu)
            CALL CUREADR (CUDIRFLE,VARNO,'RLFNU',rlfnu)
            CALL CUREADR (CUDIRFLE,VARNO,'WFNUU',wfnuu)
            CALL CUREADR (CUDIRFLE,VARNO,'NFPL',nfpl)
            CALL CUREADR (CUDIRFLE,VARNO,'NFGL',nfgl)
            CALL CUREADR (CUDIRFLE,VARNO,'NFTL',nftl)
            CALL CUREADR (CUDIRFLE,VARNO,'NFPU',nfpu)
            CALL CUREADR (CUDIRFLE,VARNO,'NFGU',nfgu)
            CALL CUREADR (CUDIRFLE,VARNO,'NFTU',nftu)
            CALL CUREADR (CUDIRFLE,VARNO,'RTNO3',rtno3)
            CALL CUREADR (CUDIRFLE,VARNO,'RTNH4',rtnh4)
            CALL CUREADR (CUDIRFLE,VARNO,'NSFAC',nsfac)
            CALL CUREADR (CUDIRFLE,VARNO,'NMNFC',nmnfc)
            CALL CUREADR (CUDIRFLE,VARNO,'NLAB%',xnfs)
            CALL CUREADR (CUDIRFLE,VARNO,'RDGS',rdgs1)
            CALL CUREADR (CUDIRFLE,VARNO,'GN%MN',grnmn)
            ! NB. TBAM is only used experimentally;should not be in coeff.files
            ! Below are 3.5 expressions
            !P1V = P1V*0.0054545 + 0.0003
            !P1D = P1D*0.002
            !PD(5) = 430.0 + PD(5)*20.00
            !IF (CROP.EQ.'BA') PD(5) = 300.0 + PD(5)*40.00
            !IF (G1 .NE. 0.0) G1 = 5.0 + G1* 5.00
            !IF (G2 .NE. 0.0) G2 = 0.65 + G2* 0.35
            !IF (G3 .NE. 0.0) G3 = -0.005 + G3* 0.35
          !ENDIF
        ENDIF

        IF (RNMODE.NE.'T') CALL FVCHECK(ECDIRFLE,GENFLCHK)
        IF (PD(1).LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'P1',PD(1))
        IF (PD2FR(1).LE.0)CALL ECREADR (ECDIRFLE,ECONO,'P2FR1',PD2FR(1))
        IF (PD(2).LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'P2',PD(2))
        IF (PD(3).LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'P3',PD(3))
        IF (PD(4).LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'P4',PD(4))
        IF (PD4FR(1).LE.0)CALL ECREADR (ECDIRFLE,ECONO,'P4FR1',PD4FR(1))
        IF (PD4FR(2).LE.0)CALL ECREADR (ECDIRFLE,ECONO,'P4FR2',PD4FR(2))
        IF (PARUV.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'PARUE',paruv)
        IF (PARUR.LT.-92.0) CALL ECREADR (ECDIRFLE,ECONO,'PARU2',parur)
        IF (LAPOT(1).LE.0.0) 
     &   CALL ECREADR (ECDIRFLE,ECONO,'LA1S',lapot(1))
        IF (LAWS.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'SLAS',laws)
        IF (LAFV.LE.0.0)  CALL ECREADR (ECDIRFLE,ECONO,'LAFV',lafv)
        IF (LAFR.LE.0.0)  CALL ECREADR (ECDIRFLE,ECONO,'LAFR',lafr)
        IF (VEFF.LT.0.0)  CALL ECREADR (ECDIRFLE,ECONO,'VEFF',veff)
        IF (GRNMN.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'GN%MN',grnmn)
        IF (GRNS.LT.0.0)  CALL ECREADR (ECDIRFLE,ECONO,'GN%S',grns)
        IF (NFPU.LT.0.0)  CALL ECREADR (ECDIRFLE,ECONO,'NFPU',nfpu)
        IF (NFPL.LT.0.0)  CALL ECREADR (ECDIRFLE,ECONO,'NFPL',nfpl)
        IF (NFGU.LT.0.0)  CALL ECREADR (ECDIRFLE,ECONO,'NFGU',nfgu)
        IF (NFGL.LT.0.0)  CALL ECREADR (ECDIRFLE,ECONO,'NFGL',nfgl)
        IF (RDGS1.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'RDGS',rdgs1)
        IF (RTNO3.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'RTNO3',rtno3)
        IF (RTNH4.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'RTNH4',rtnh4)
        CALL ECREADR (ECDIRFLE,ECONO,'PPFPE',ppfpe)
        CALL ECREADR (ECDIRFLE,ECONO,'SGPHE',P4SGE)
        CALL ECREADR (ECDIRFLE,ECONO,'HTSTD',canhts)
        CALL ECREADR (ECDIRFLE,ECONO,'AWNS',awns)
        CALL ECREADR (ECDIRFLE,ECONO,'KCAN',kcan)
        CALL ECREADR (ECDIRFLE,ECONO,'LLIFE',tvr1)
        LLIFE = INT(TVR1)
        CALL ECREADR (ECDIRFLE,ECONO,'RS%S',rspcs)
        CALL ECREADR (ECDIRFLE,ECONO,'TIL#S',ti1lf)
        CALL ECREADR (ECDIRFLE,ECONO,'WFPU',wfpu)
        CALL ECREADR (ECDIRFLE,ECONO,'WFPGF',wfpgf)
        CALL ECREADR (ECDIRFLE,ECONO,'WFGU',wfgu)
        CALL ECREADR (ECDIRFLE,ECONO,'TKFH',lt50h)
        CALL ECREADR (ECDIRFLE,ECONO,'RDG2',rdgs2)
        CALL ECREADR (ECDIRFLE,ECONO,'TIFAC',tifac)
        CALL ECREADR (ECDIRFLE,ECONO,'TIPHE',tilpe)
        CALL ECREADR (ECDIRFLE,ECONO,'TDPHS',tilds)
        CALL ECREADR (ECDIRFLE,ECONO,'TDPHE',tilde)
        CALL ECREADR (ECDIRFLE,ECONO,'TDFAC',tildf)
        CALL ECREADR (ECDIRFLE,ECONO,'LSPHS',lsens)
        CALL ECREADR (ECDIRFLE,ECONO,'LSPHE',lsene)
        CALL ECREADR (ECDIRFLE,ECONO,'PHF3',phintf(3))
        CALL ECREADR (ECDIRFLE,ECONO,'PHL2',phintl(2))

        CALL FVCHECK(SPDIRFLE,GENFLCHK)
        IF (PD4FR(1).LE.0.0) CALL SPREADR (SPDIRFLE,'P4FR1',PD4FR(1))
        IF (PD4FR(2).LE.0.0) CALL SPREADR (SPDIRFLE,'P4FR2',PD4FR(2))
        IF (ppfpe.LT.0.0)    CALL SPREADR (SPDIRFLE,'PPFPE',ppfpe)
        IF (LSENS.LE.0.0)    CALL SPREADR (SPDIRFLE,'LSPHS',lsens)
        IF (LSENE.LE.0.0)    CALL SPREADR (SPDIRFLE,'LSPHE',lsene)
        IF (LWLOS.LE.0.0)    CALL SPREADR (SPDIRFLE,'LWLOS',lwlos)
        IF (TILDF.LT.0.0)    CALL SPREADR (SPDIRFLE,'TDFAC',tildf)
        IF (PD2FR(1).LE..0)  CALL SPREADR (SPDIRFLE,'P2FR1',pd2fr(1))
        IF (P4SGE.LE..0)     CALL SPREADR (SPDIRFLE,'SGPHE',p4sge)
        IF (NFGU.LE.0.0)     CALL SPREADR (SPDIRFLE,'NFGU',nfgu)
        IF (NFGL.LT.0.0)     CALL SPREADR (SPDIRFLE,'NFGL',nfgl)
        IF (NFPU.LE.0.0)     CALL SPREADR (SPDIRFLE,'NFPU',nfpu)
        IF (NFPL.LT.0.0)     CALL SPREADR (SPDIRFLE,'NFPL',nfpl)
        IF (WFPU.LE.0.0)     CALL SPREADR (SPDIRFLE,'WFPU',wfpu)
        IF (WFPGF.LE.0.0)    CALL SPREADR (SPDIRFLE,'WFPGF',wfpgf)
        IF (WFGU.LT.0.0)     CALL SPREADR (SPDIRFLE,'WFGU',wfgu)
        IF (LLIFE.LE.0)      CALL SPREADR (SPDIRFLE,'LLIFE',tvr1)
        LLIFE = INT(TVR1)
        IF (NFTU.LT.0.0)     CALL SPREADR (SPDIRFLE,'NFTU',nftu)
        IF (NFTL.LT.0.0)     CALL SPREADR (SPDIRFLE,'NFTL',nftl)
        IF (WFSU.LT.0.0)     CALL SPREADR (SPDIRFLE,'WFS',wfsu)
        IF (NFSU.LT.0.0)     CALL SPREADR (SPDIRFLE,'NFS',nfsu)
        IF (WFNUU.LE.0.0)    CALL SPREADR (SPDIRFLE,'WFNUU',wfnuu)
        IF (RLFNU.LE.0.0)    CALL SPREADR (SPDIRFLE,'RLFNU',rlfnu)
        IF (NCNU.LE.0.0)     CALL SPREADR (SPDIRFLE,'NCNU',ncnu)
        IF (XNFS.LT.0.0)     CALL SPREADR (SPDIRFLE,'NLAB%',xnfs)
        IF (RTNO3.LT.0.0)    CALL SPREADR (SPDIRFLE,'RTNO3',rtno3)
        IF (RTNH4.LT.0.0)    CALL SPREADR (SPDIRFLE,'RTNH4',rtnh4)
        CALL SPREADR (SPDIRFLE,'PGERM',pgerm)
        CALL SPREADR (SPDIRFLE,'PEMRG',pemrg)
        CALL SPREADR (SPDIRFLE,'P0',PD(0))
        CALL SPREADR (SPDIRFLE,'PPTHR',p1dt)
        CALL SPREADR (SPDIRFLE,'PPEND',ppend)
        ! In spe file main stem is considered as tiller 1 whereas below
        ! ths is not the case and tiller 1 is the first branch.
        CALL SPREADR (SPDIRFLE,'TGR02',latfr(1))
        !                                    CSCRP
        LATFR(2) =  LATFR(1)       ! 0.8      0.80
        LATFR(3) =  LATFR(1)       ! 0.8      0.76
        LATFR(4) =  LATFR(1)       ! 0.8      0.72
        LATFR(5) =  0.8 * LATFR(1) ! 0.6      0.68
        LATFR(6) =  0.8 * LATFR(1) ! 0.6      0.64
        LATFR(7) =  0.6 * LATFR(1) ! 0.4      0.61
        LATFR(8) =  0.6 * LATFR(1) ! 0.4      0.57
        LATFR(9) =  0.6 * LATFR(1) ! 0.4      0.53
        LATFR(10) = 0.4 * LATFR(1) !  0.3     0.49
        LATFR(11) = 0.4 * LATFR(1) !  0.3     0.45
        LATFR(12) = 0.4 * LATFR(1) !  0.3     0.41
        LATFR(13) = 0.4 * LATFR(1) !  0.3     0.37  
        LATFR(14) = 0.2 * LATFR(1) !  0.2     0.33
        LATFR(15) = 0.2 * LATFR(1) !  0.2     0.29
        LATFR(16) = 0.2 * LATFR(1) !  0.2     0.26
        LATFR(17) = 0.1 * LATFR(1) !  0.1     0.22
        LATFR(18) = 0.1 * LATFR(1) !  0.1     0.18
        LATFR(19) = 0.1 * LATFR(1) !  0.1     0.14
        LATFR(20) = 0.1 * LATFR(1) !  0.1
        CALL SPREADR  (SPDIRFLE,'LRPHS',lrets)

        CALL SPREADRA (SPDIRFLE,'PTFS','10',ptfs)
        CALL SPREADRA (SPDIRFLE,'PTFA','10',ptfa)
        CALL SPREADRA (SPDIRFLE,'STFR','10',stfr)
        CALL SPREADRA (SPDIRFLE,'CHT%','10',chtpc)
        CALL SPREADRA (SPDIRFLE,'CLA%','10',clapc)
        
        CALL SPREADR (SPDIRFLE,'LLIG%',lligp)
        CALL SPREADR (SPDIRFLE,'SLIG%',sligp)
        CALL SPREADR (SPDIRFLE,'RLIG%',rligp)
        CALL SPREADR (SPDIRFLE,'GLIG%',gligp)
        CALL SPREADR (SPDIRFLE,'RLWR',rlwr)
        CALL SPREADR (SPDIRFLE,'RWUMX',rwumxs)
        rwumx = rwumxs
        CALL SPREADR (SPDIRFLE,'RSUSE',rsuse)
        CALL SPREADR (SPDIRFLE,'WFRGU',wfrgu)
        CALL SPREADR (SPDIRFLE,'NCRG',ncrg)
        CALL SPREADR (SPDIRFLE,'RWUPM',rwupm)
        CALL SPREADR (SPDIRFLE,'SDWT',sdsz)
        CALL SPREADR (SPDIRFLE,'SDN%',sdnpci)
        CALL SPREADR (SPDIRFLE,'WFGEU',wfgeu)
        CALL SPREADR (SPDIRFLE,'TKUH',lt50s)
        CALL SPREADR (SPDIRFLE,'TKLF',tklf)
        CALL SPREADR (SPDIRFLE,'WFTU',wftu)
        CALL SPREADR (SPDIRFLE,'WFTL',wftl)
        CALL SPREADR (SPDIRFLE,'WFSU',wfsu)
        CALL SPREADR (SPDIRFLE,'LLOSW',llosw)
        CALL SPREADR (SPDIRFLE,'NFSU',nfsu)
        CALL SPREADR (SPDIRFLE,'NFSF',nfsf)
        CALL SPREADR (SPDIRFLE,'LLOSN',llosn)
        CALL SPREADR (SPDIRFLE,'WFNUL',wfnul)
        CALL SPREADR (SPDIRFLE,'NO3MN',no3mn)
        CALL SPREADR (SPDIRFLE,'NH4MN',nh4mn)
        CALL SPREADR (SPDIRFLE,'P6',pd(6))
        CALL SPREADR (SPDIRFLE,'LSHFR',lshfr)
        CALL SPREADR (SPDIRFLE,'LAXS',laxs)
        CALL SPREADR (SPDIRFLE,'PHF1',phintf(1))
        CALL SPREADR (SPDIRFLE,'PHL1',phintl(1))
        IF (PHINTF(3).LE.0) CALL SPREADR (SPDIRFLE,'PHF3',phintf(3))
        IF (PHINTL(2).LE.0) CALL SPREADR (SPDIRFLE,'PHL2',phintl(2))
        CALL SPREADR (SPDIRFLE,'GN%MX',grnmx)
        CALL SPREADR (SPDIRFLE,'RSEN',rsen)
        IF (RSEN.LT.0.0) CALL SPREADR (SPDIRFLE,'RSEN%',rsen)
        CALL SPREADR (SPDIRFLE,'RS%X' ,rspcx)
        CALL SPREADR (SPDIRFLE,'SSEN',ssen)
        IF (SSEN.LT.0.0) CALL SPREADR (SPDIRFLE,'SSEN%',ssen)
        CALL SPREADR (SPDIRFLE,'SSPHS',ssstg)
        CALL SPREADR (SPDIRFLE,'SAWS',saws)
        CALL SPREADR (SPDIRFLE,'LSHAW',lshaws)
        CALL SPREADR (SPDIRFLE,'SDAFR',sdafr)
        CALL SPREADR (SPDIRFLE,'RLDGR',rldgr)
        CALL SPREADR (SPDIRFLE,'PTFMX',ptfx)
        CALL SPREADR (SPDIRFLE,'RRESP',rresp)
        CALL SPREADR (SPDIRFLE,'SLAMN',lawfrmn)
        CALL SPREADR (SPDIRFLE,'HDUR',hdur)
        CALL SPREADR (SPDIRFLE,'CHFR',chfr)
        CALL SPREADR (SPDIRFLE,'CHSTG',chstg)
        CALL SPREADR (SPDIRFLE,'NTUPF',ntupf)
        CALL SPREADR (SPDIRFLE,'RDGTH',rdgth)
        CALL SPREADR (SPDIRFLE,'TPAR',part)
        CALL SPREADR (SPDIRFLE,'TSRAD',sradt)
        KEP = (KCAN/(1.0-PART)) * (1.0-SRADT)
        IF (LAWCF.LE.0.0) CALL SPREADR (SPDIRFLE,'SLACF',lawcf)

        CALL SPREADRA (SPDIRFLE,'LASF','10',plasf)
        CALL SPREADRA (SPDIRFLE,'LN%S','10',lnpcs)
        CALL SPREADRA (SPDIRFLE,'SN%S','10',snpcs)
        CALL SPREADRA (SPDIRFLE,'RN%S','10',rnpcs)
        CALL SPREADRA (SPDIRFLE,'LN%MN','10',lnpcmn)
        CALL SPREADRA (SPDIRFLE,'SN%MN','10',snpcmn)
        CALL SPREADRA (SPDIRFLE,'RN%MN','10',rnpcmn)
        CALL SPREADRA (SPDIRFLE,'TRGEM','4',trgem)
        CALL SPREADRA (SPDIRFLE,'TRDV1','4',trdv1)
        CALL SPREADRA (SPDIRFLE,'TRDV2','4',trdv2)
        IF (tbam.gt.-90.0) THEN
          WRITE (FNUMWRK,*)' '
          WRITE (FNUMWRK,*)' Base temperature for post anthesis period'
          WRITE (FNUMWRK,*)' changed from ',trdv2(1),' to ',tbam        
          trdv2(1) = tbam
          ! NB. TBAM is only used experimentally;should not be in coeff.files
        ENDIF
        CALL SPREADRA (SPDIRFLE,'TRLFG','4',trlfg)
        CALL SPREADRA (SPDIRFLE,'TRPHS','4',trphs)
        CALL SPREADRA (SPDIRFLE,'TRVRN','4',trvrn)
        CALL SPREADRA (SPDIRFLE,'TRHAR','4',trlth)
        CALL SPREADRA (SPDIRFLE,'TRGFW','4',trgfw)
        CALL SPREADRA (SPDIRFLE,'TRGFN','4',trgfn)
        CALL SPREADRA (SPDIRFLE,'CO2RF','10',co2rf)
        CALL SPREADRA (SPDIRFLE,'CO2F','10',co2f)

        ! Variables that should be read-in!
        LALOSSF = 0.2     ! Leaf area lost when tillers die
        LAFST = 1.6       ! Stage at which incremment in lf size changes
        RSCLX = 0.2       ! Reserves concentration in leaves,maximum   
        
        WRITE(fnumwrk,*) ' '
        WRITE(fnumwrk,'(A18)')' RUN OVERVIEW     '
        WRITE(fnumwrk,*)' MODEL   ',MODEL
        WRITE(fnumwrk,*)' MODULE  ',MODNAME
        WRITE(fnumwrk,'(A10,I6)')'  VERSION ',VERSION
        WRITE(fnumwrk,*)' RNMODE  ',RNMODE

        WRITE(fnumwrk,*)' '
        WRITE(fnumwrk,'(A13,A1)')'  N SWITCH   ',ISWNIT
        WRITE(fnumwrk,'(A13,A1)')'  H2O SWITCH ',ISWWAT
        WRITE(fnumwrk,'(A18,A1)')'  PLANTING SWITCH ',IPLTI

        WRITE(fnumwrk,*)' '
        WRITE(fnumwrk,'(A10,I8  )')'  RUN     ',RUN    
        WRITE(fnumwrk,*)' '
        WRITE(fnumwrk,'(A23,A10)')'  EXPERIMENT           ',excode
        WRITE(fnumwrk,'(A21, I3)')'  TREATMENT          ',tn
        WRITE(fnumwrk,'(A23,I1)') '  CROP COMPONENT       ',CN
!       IF (IPLTI.NE.'A') THEN
        IF (IPLTI.NE.'A' .AND. IPLTI.NE.'F') THEN
          WRITE(fnumwrk,'(A23,I7)') '  PLANTING DATE TARGET ',YEARPLTP
        ELSE  
          WRITE(fnumwrk,'(A40)')
     &      '  AUTOMATIC PLANTING.  THRESHOLD DAYS:  '
!          CALL CSYR_DOY(PWDINF,TVI1,TVI2)
          CALL YR_DOY(PWDINF,TVI1,TVI2)
          WRITE(fnumwrk,'(A14,I7)') '   EARLIEST   ',TVI2
!          CALL CSYR_DOY(PWDINL,TVI1,TVI2)
          CALL YR_DOY(PWDINL,TVI1,TVI2)
          WRITE(fnumwrk,'(A14,I7)') '   LATEST     ',TVI2
        ENDIF
        
        ! The following are to allow examination of the functioning of 
        ! different parts of the module, and comparison with CSCRP     
        
        ! Commented acronyms are those used in the genotype files
       
        ! Set which sections to be examined
        EXAMINE(1) = 'N'    ! Basic leaf and root growth
        EXAMINE(2) = 'N'    ! Leaf senescencs
        EXAMINE(3) = 'N'    ! Tillering
        EXAMINE(4) = 'N'    ! Reproductive development
        EXAMINE(20)= 'N'    ! Everything
        
        IF (EXAMINE(1).EQ.'Y') THEN                                     

          ! No tillering,no senescence,no reproductive development 
          
          WRITE(fnumwrk,*)' '
          WRITE(fnumwrk,*)' RUNNING EXAMINE. '
          WRITE(fnumwrk,*)' '
          
          ! CSCER                                    !  CSCRP
          
          PGERM = 10.0                               !    PGERM = 10.0  
          PEMRG = 8.0                                !    PEMRG =  8.0  

          Pd(0) = 0.0                                !    Pd(0) = 0.0   
          
          PARUV = 2.7                                !    PARUE = 2.7   
          PARUR = 2.7                                !    PARU2 = 2.7   
          
          KCAN = 0.85                                !    KCAN = 0.85   
                                                                                                            
          PTFX = 0.98  ! PTFMX                       !    PTFMX = 0.98 
                                                                        
          PTFS(1) = 0.75                             !    PTFMN = 0.75  
          PTFS(2) = 0.75                             !    PTFMX = 0.75 
          PTFS(3) = 0.75                                             
          PTFS(4) = 0.75                             
          PTFS(5) = 0.75                             
          PTFS(6) = 0.75                             
          
          PTFA(1) = 0.0                              !    PTFA = 0.00
          PTFA(2) = 0.0
          PTFA(3) = 0.0
          PTFA(4) = 0.0
          PTFA(5) = 0.0
          PTFA(6) = 0.0
          
          STFR(1) = 0.0                                              
          STFR(2) = 0.0                                              
          STFR(3) = 0.0                                              
          STFR(4) = 0.0                                              
          STFR(5) = 0.0                                              
          STFR(6) = 0.0                                              
          
          ! Actuals in SPE file:
          ! PTFS  PTFA  STFR                         
          ! 0.65  0.10  0.00
          ! 0.70  0.10  0.15
          ! 0.75  0.10  0.51
          ! 0.80  0.10  1.00
          ! 0.98  0.35  1.00
          ! 0.98  0.35  1.00

          PPFPE = 1.0                                !    PPFPE = 1.0   
          P1D = 97.0                                 !    PPS(1)= 97.0
          
          VEFF = 1.0                                 !    VEFF = 1.0    
                                                     !    VBASE = 0.0                                                        
          
           
          PHINT =    95.0                            !    PHINT = 95.0
          PHINTL(1) = 2.0    ! Headed as PHLx        !    PHINTL(1) =2.0
          PHINTF(1) = 0.8    ! Headed as PHFx        !    PHINTF(1) =0.8
          PHINTL(2) = 150                            !    PHINTL(2) =150
          PHINTF(3) = 1.8                            !    PHINTF(3) =1.8
                                                     
          LAPOT(1) = 5.0                             !    LA1S = 5.0    
          LAFV = 0.1                                 !    LAFV = 0.1    
          LAFR = 0.5                                 !    LAFR = 0.5    
            
          LAWS = 400                                 !    LAWS = 400    
          LAWCF = 0.02                               !    LAWCF = 0.02  
          LAWFRMN = 0.5  ! SLAMN
            
          LSHFR = 0.33                               !    LSHFR = 0.33  
          LRETS = 3.0                                !    LRPHS
          
          LSHAWS = 50.0  ! LSHAW                     !    LSHAWV = 50.0   ! LSHAV
                                                     !    LSHAWR = 50.0   ! LSHAR

          TKLF = -50.0                               !    TKDLF = 2.0 
          LT50S = -50.0                              !    TKUH = -50.0  
          LT50H = -90.0                              !    TKFH = -90.0  
                                                     !    TKSPAN = 2.0                                                       
          HDUR = 10.0                                !    HDUR = 10.0   
          
                                                     !    HLOST = 50.0                                                       
                                                     !    HLOSF = 0.2 

          RSPCS = 20.0                               !    RSPCA = 15.0 
          RSPCX = 80.0                               !    RSPCO = 80.0

          RRESP = 0.40                               !    RRESP = 0.40 
          RSEN = 0.1                                 !    RSEN = 0.1
          RLWR = 0.90  ! 0.98                        !    RLWR = 0.90 
          
          SDAFR = 0.50  ! From 0.50

          RTNO3 = 0.006  ! RTNUP                     !    RTNO3 = .006!RTNUP
          RTNH4 = 0.006                              !    RTNH4 = .006
                                                     !    NO3CF = 1.0 !NUPNF
                                                     !    NUPWF = 1.0       
          NH4MN = 0.5                                !    NH4MN = 0.5  
          NO3MN = 0.5                                !    NO3MN = 0.5  
          NTUPF = 0.05                               !    NTUPF = 0.05 

             ! CSCER
          LNPCS(0) = 6.5                             !    LNPCS(0) = 6.5
          LNPCS(1) = 6.5                             !    LNPCS(1) = 6.5
          LNPCS(2) = 6.5                             
          LNPCS(3) = 6.5   
          LNPCS(4) = 6.5   
          LNPCS(5) = 6.5   
          LNPCS(6) = 6.5   
          LNPCS(7) = 6.5   
          LNPCS(8) = 6.5   
          SNPCS(0) = 1.5                             !    SNPCS(0) = 1.5
          SNPCS(1) = 1.5                             !    SNPCS(1) = 1.5
          SNPCS(2) = 1.5                             
          SNPCS(3) = 1.5   
          SNPCS(4) = 1.5   
          SNPCS(5) = 1.5   
          SNPCS(6) = 1.5   
          SNPCS(7) = 1.5   
          SNPCS(8) = 1.5   
          RNPCS(0) = 2.0                             !    RNPCS(0) = 2.0
          RNPCS(1) = 2.0                             !    RNPCS(1) = 2.0
          RNPCS(2) = 2.0                             
          RNPCS(3) = 2.0   
          RNPCS(4) = 2.0   
          RNPCS(5) = 2.0   
          RNPCS(6) = 2.0   
          RNPCS(7) = 2.0   
          RNPCS(8) = 2.0   
          LNPCMN(0) = 0.8                            !   LNPCMN(0) = 0.8
          LNPCMN(1) = 0.8                            !   LNPCMN(1) = 0.8
          LNPCMN(2) = 0.8                            
          LNPCMN(3) = 0.8   
          LNPCMN(4) = 0.8   
          LNPCMN(5) = 0.8   
          LNPCMN(6) = 0.8   
          LNPCMN(7) = 0.8   
          LNPCMN(8) = 0.8   
          SNPCMN(0) = 0.5                            !   SNPCMN(0) = 0.5
          SNPCMN(1) = 0.5                            !   SNPCMN(1) = 0.5
          SNPCMN(2) = 0.5                            
          SNPCMN(3) = 0.5   
          SNPCMN(4) = 0.5   
          SNPCMN(5) = 0.5   
          SNPCMN(6) = 0.5   
          SNPCMN(7) = 0.5   
          SNPCMN(8) = 0.5   
          RNPCMN(0) = 1.5                            !   RNPCMN(0) = 1.5
          RNPCMN(1) = 1.5                            !   RNPCMN(1) = 1.5
          RNPCMN(2) = 1.5                            
          RNPCMN(3) = 1.5   
          RNPCMN(4) = 1.5   
          RNPCMN(5) = 1.5   
          RNPCMN(6) = 1.5   
          RNPCMN(7) = 1.5   
          RNPCMN(8) = 1.5   
          ! Actuals in SPE file:
          ! CSCRP
          ! LN%S LN%MN  SN%S SN%MN  RN%S RN%MN                              
          !  8.0  0.80  2.50  0.65  2.00  1.50                              
          !  1.0  0.55  0.40  0.40  1.70  1.25                              
          ! CSCER
          ! LN%S  SN%S  RN%S LN%MN SN%MN RN%MN
          !  8.0   2.5  2.04  0.80  0.65  1.53     
          !  6.4   2.0  1.97  0.75  0.60  1.47
          !  5.3   1.0  1.89  0.70  0.55  1.41
          !  4.0   0.8  1.82  0.65  0.50  1.36
          !  3.1   0.6  1.77  0.60  0.45  1.33
          !  2.7   0.5  1.73  0.60  0.40  1.29
          !  1.5   0.4  1.68  0.60  0.40  1.26
          !  1.0   0.4  1.68  0.55  0.40  1.26
          
          XNFS = 20     ! NLABPC                     !     NLABPC = 20  
          NFPU = 1.0                                 !     NFPU = 1.0   
          NFPL = 0.0                                 !     NFPL = 0.0   
          NFGU = 1.0                                 !     NFGU = 1.0   
          NFGL = 0.0                                 !     NFGL = 0.0   
          NFTU = 1.0   ! NFTU                        !     NFTU = 1.0   
          NFTL = 0.0   ! NFTL                        !     NFTL = 0.0   
          NFSU = 0.4                                 !     NFSU = 0.4   
          NFSF = 0.1                                 !     NFSF = 0.1   
          NCRG = 30                                  !     NCRG = 30    
          LLOSN = 0.02 
          ! For low N accelerated senescence

          ! To eliminate leaf senescence
          LLIFE = 200.0                              !    LLIFA = 200.0 
          ! To eliminate tillering                      
          TI1LF = 200.0                              !    TI1LF = 200.0 
          ! To eliminate reproductive development
          P1V = 100000.0                             !    VREQ = 10000.0

        ENDIF    ! EXAMINE(1)
        
        
        IF (EXAMINE(2).EQ.'Y') THEN                                    
          ! Leaf senescence 
          LLIFE = 6.0                                !     LLIFA = 4.0 
          ! Must be 6 because of senescence period differences (1 vs 3)
          ! Growth = 1 phyllochron                   !     LLIFG = 1.0
          ! Senescence =  1 phyllochron              !     LLIFS = 3.0
          LWLOS = 0.5                                !     LWLOS = 0.5  
          ! LNPCMN -> N loss                         !     LNPCMN -> N loss
          ! Calculates LSENNF(fr N > min)        
        ENDIF    ! EXAMINE(2)  Senescence

        
        IF (EXAMINE(3).EQ.'Y') THEN                                    
          ! Tillering        
          TI1LF = 3.5                                !    TI1LF = 4.45  
          TILPE = 2.5
          TIFAC = 1.0                                !    TIFAC = 1.0   
          TILDS = 2.5                                !    TDPHS = 3.0   
          TILDE = 6.0                                !    TDPHE = 7.0   
          TILDF = 4.0                                !    TDFAC = 5.0   
          LATFR(1) = 0.80   ! TGR(2)                 !    TGR(2) = 0.80
                                                     !    TGR(20) = 0.10                                                     
                                                     !    TILIP = 6.0                                                        
                                                     !    TINOX = 20 ! TIL#X                                               
                                                     !    TDSF = 1.0                                                         
          !          CSCRP    CSCER
          LATFR(2)  = 0.80   ! 0.8  LATFR(1)       
          LATFR(3)  = 0.76   ! 0.8  LATFR(1)       
          LATFR(4)  = 0.72   ! 0.8  LATFR(1)       
          LATFR(5)  = 0.68   ! 0.6  0.8 * LATFR(1) 
          LATFR(6)  = 0.64   ! 0.6  0.8 * LATFR(1) 
          LATFR(7)  = 0.61   ! 0.4  0.6 * LATFR(1) 
          LATFR(8)  = 0.57   ! 0.4  0.6 * LATFR(1) 
          LATFR(9)  = 0.53   ! 0.4  0.6 * LATFR(1) 
          LATFR(10) = 0.49   ! 0.3  0.4 * LATFR(1)
          LATFR(11) = 0.45   ! 0.3  0.4 * LATFR(1)
          LATFR(12) = 0.41   ! 0.3  0.4 * LATFR(1)
          LATFR(13) = 0.37   ! 0.3  0.4 * LATFR(1)
          LATFR(14) = 0.33   ! 0.2  0.2 * LATFR(1)
          LATFR(15) = 0.29   ! 0.2  0.2 * LATFR(1)
          LATFR(16) = 0.26   ! 0.2  0.2 * LATFR(1)
          LATFR(17) = 0.22   ! 0.1  0.1 * LATFR(1)
          LATFR(18) = 0.18   ! 0.1  0.1 * LATFR(1)
          LATFR(19) = 0.14   ! 0.1  0.1 * LATFR(1)
          LATFR(20) = 0.10   ! 0.1  0.1 * LATFR(1)
          
        ENDIF    ! EXAMINE(3)  Tillering

        IF (EXAMINE(4).EQ.'Y') THEN                                    
          ! Reproductive development
            P1V =    8.0                     !45 !    VREQ = 8.0        
            P1D =     97.0                       !    PPS(1) = 97.0    
            PD(5) =   500                        !    PD(8) = 500.0     
            PD(1) = 200                          !    PD(1) = 380       
            PD(2) = 200                          !    PD(2) =  70       
            PD(3) = 200                          !    PD(3) = 200       
            PD(4) = 200                          !    PD(4) = 200       
            PD2FR(1) = 0.25
            PD4FR(1) = 0.25
            PD4FR(2) = 0.10
                                                 !    PD(5) =  60                                                        
                                                 !    PD(6) =  25                                                        
                                                 !    PD(7) = 150                                                        
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
        ENDIF
            
        IF (EXAMINE(20).EQ.'Y') THEN  
          ! Everything from all genotype files    
          
        ! CSCER                                  !  CSCRP
        
        ! CULTIVAR  Newton(IB0488)  
            P1V =    1000.0                  !45 !    VREQ = 8.0        
            P1D =     75.0                       !    PPS(1) = 100.0    
            PD(5) =   500                        !    PD(8) = 500.0     
            G1CWT =    26.0                      !    GNOWTS = 25.0     
            G2KWT =    25.0                      !    GWTS = 40.0       
            G3 =        2.0                      !    G3 = 2.5          
            PHINT =    95.0                      !    PHINT = 80        
            
        ! ECOTYPE
            PD(1) = 200                          !    PD(1) = 380       
            PD(2) = 200                          !    PD(2) =  70       
            PD(3) = 200                          !    PD(3) = 200       
            PD(4) = 200                          !    PD(4) = 200       
            PD2FR(1) = 0.25
            PD4FR(1) = 0.25
            PD4FR(2) = 0.10
                                                 !    PD(5) =  60                                                        
                                                 !    PD(6) =  25                                                        
                                                 !    PD(7) = 150                                                        
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

            LAPOT(1) = 5.0                       !    LA1S = 3.0        
            LAFV = 0.1                           !    LAFV = 0.1        
            LAFR = 0.5                           !    LAFR = 0.5        

            VEFF = 0.5                           !    VEFF = 1.0        
                                                 !    VBASE = 0.0                                                        

                                                 !    PPS(2) = 0.0                                                       
                                                                                                                         
                                             !   ECOTYPE:
                                             
            PARUV = 2.7                          !    PARUE = 2.7       
            PARUR = 2.7                          !    PARU2 = 2.7       
            
          ! Leaves 
            PHINTL(2) = 15                       !    PHINTL(2) = 12    
            PHINTF(3) = 1.8                      !    PHINTF(3) = 1.3   
            LAWS = 400                           !    LAWS = 400        
                                                 !    LSENI = 1.5                                                        
            LSENS = 5.0                          !    LSPHS = 8.0       
            LSENE = 6.3                          !    LSPHE = 9.3
            
          ! Tillers 
            TI1LF = 3.5                          !    TI1LF = 4.45      
            TILPE = 2.5
            TIFAC = 1.0                          !    TIFAC = 1.0       
            TILDS = 2.5                          !    TDPHS = 3.0       
            TILDE = 6.0                          !    TDPHE = 7.0       
            TILDF = 4.0                          !    TDFAC = 5.0       
                                                 !    TDSF = 1.0                                                         
           ! Roots                                                 
            RDGS1 = 3.0                          !    RDGS = 3.0        
            CANHTS = 100                         !    HTSTD = 100       
            AWNS = 0.0                           !    AWNS = 5.0        
            KCAN = 0.85                          !    KCAN = 0.85       
            RSPCS = 20.0                         !    RSPCA = 15.0      
          ! Grain 
            GRNS = 3.0                           !    GNPCS = 2.0       
            GRNMN = 0.0
                                                 !    GWTAA = 0.0    ! GWWF                                              
          ! Hardiness  
            LT50H = -10                          !    TKFH = -15        
            
          ! Stems  
                                                 !    SSPHS = 8.0       
                                                 !    SSPHE = 9.3                                                        
                                                 !    GWTAT = 1.0    ! SHWTA                                             
          ! N uptake                                                 
            RTNO3 = 0.006  ! RTNUP               !    RTNO3 = .006!RTNUP
                                                 !    NO3CF = 1.0! NUPNF
                                                 !    NUPWF = 1.0       
                                                 !                                                                       
        ! SPECIES                                !                                                                       
                                                 !    HMPC = 15                                                          
                                   ! Secondary Stages (S=Standard,K=Key)
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
                                   !  13      S   8.0 GLPHS GrLinearStar
                                   !  14      S   8.7 GLPHE GrLinearEnd 
                                   !  15      S   9.0 GGPHE GrainEnd    
          ! Phase durations and modifiers 
            PGERM = 10.0                         !    PGERM = 10.0      
            PEMRG =  8.0                         !    PEMRG = 8.0      
            Pd(0) = 0.0                          !    Pd(0) = 0.0       
            Pd(6) = 200.0                        !    Pd(9) = 200.0     
            PPFPE= 1.0                           !    PPFPE = 1.0       
            P1DT = 20.0  ! PPTHR                 !    PPTHR = 20.0      
            PPEND = 2.0
                                                 !    PPEXP = 2.0                                                        
                                                 !    VPEND = 2.0                                                        
                                                 !    VEEND = 2.0                                                        
                                                 !    VLOSS = 0.2                                                        
                                                 !    VLOST = 30.0                                                       
                                                 !    VLOSF = 0.5                                                        
          ! Roots
            RLIGP = 10.0  ! RLIG%                !    RLIGPC = 10.0     
            RLWR = 0.98                          !    RLWR = 0.9        
            RSEN = 0.10                          !    RSEN = 0.10     
            RRESP = 0.40                         !    RRESP = 0.80      
            RLDGR = 500.0
                                                 !    RDGAF = 0.5                                                        
                                                 !    RTUFR = 0.20                                                       
          ! Leaves
            LLIGP = 10.0  ! LLIG%                !    LLIGPC = 10.0     
            LAXS = 900.0                         !    LAXS = 100.0      
            LAWCF = 0.02                         !    LAWCF = 0.02      
            LAWFRMN = 0.5  ! SLAMN
            LSHFR = 0.33                         !    LSHFR = 0.33      
            LRETS = 3.0        ! LRPHS
            LSHAWS = 50.0      ! LSHAWS          !    LSHAWV =50.0    ! LSHAV
                                                 !    LSHAWR = 80.0   ! LSHAR                                         
            PHINTL(1) = 2.0    ! PHL             !    PHINTL(1) =2.0!PHL
            PHINTF(1) = 0.8    ! PHF             !    PHINTF(1) =0.8!PHF
            
            LLIFE = 4.0                          !    LLIFA = 4.0       
                                                 !    LLIFG = 1.0                                                        
                                                 !    LLIFS = 3.0                                                        
            !   LASF
            !   0.00
            !   0.10
            !   0.10
            !   0.10
            !   0.10
            !   1.00

            
            LWLOS = 0.50    ! LWLOS              !    LWLOS = 0.5!LWLOS
                                                 !    LAIXX = 14.0
                                                                                                        
            ! CHT%  CLA%                         !    ! CHT%  CLA%                                      
            !    0     0                         !    !    0     0                                                       
            !   50    50                         !    !   50    50                                                       
            !  100   100                         !    !  100   100                                                       

            PART = 0.07  ! TPAR                  !    TPAR = 0.07       
            SRADT = 0.25 ! TSRAD                 !    TSRAD = 0.25      
          ! Tillers      
            LATFR(1) = 0.80   ! TGR(2)           !    TGR(2) = 0.80  
                                                 !    TGR(20) = 0.10                                                     
                                                 !    TILIP = 6.0                                                        
                                                 !    TINOX = 20   ! TIL#X                                               
          ! Reserves
                                                 !    RSPCS = 20                                                         
            RSPCX = 80.0                         !    RSPCX = 80.0      
                                                 !    RSUSE = 0.1                                                        
                                                 !    RSPCLX = 80.0                                                      
          ! Stems                                                                                                        
            SLIGP = 10.0  ! SLIG%                !    SLIGPC = 10.0     
            SAWS = 25.0                          !    SAWS = 25.0       
            P4SGE = 4.45   ! SGPHE
            SSSTG = 5.8    ! SSPHS
            SSEN = 0.53  ! SSEN%
          ! Chaff                                !    ! Chaff                                                            
            CHFR = 0.65                          !    CHFR = 0.65       
            CHSTG = 3.8
          ! Grain                                !    ! Grain                                                            
            GLIGP = 10.0  ! GLIG%                !    GLIGPC = 10.0     
                                                 !    GWLAGFR = 0.05  ! GWLAG                                            
                                                 !    GWLINFR = 0.90  ! GWLIN                                            
          ! Seed                                 !    ! Seed                                                             
            SDSZ = 0.284   ! SDWT                !    SDWT = 0.28       
                                                 !    SDDUR = 20.0                                                       
                                                 !    SDRSPC = 80.0                                                      
            SDAFR = 0.50  ! From 0.50
          ! Photosynthesis                       !    ! Photosynthesis                                                   
                                                 !    RSFPU = 3.0                                                        
                                                 !    RSFPL = 2.0                                                        
            !CO2RF  CO2F                         !    !CO2RF  CO2F                                                       
            !    0  0.00                         !    !    0  0.00                                                       
            !  220  0.71                         !    !  220  0.71                                                       
            !  330  1.00                         !    !  330  1.00                                                       
            !  440  1.08                         !    !  440  1.08                                                       
            !  550  1.17                         !    !  550  1.17                                                       
            !  660  1.25                         !    !  660  1.25                                                       
            !  770  1.32                         !    !  770  1.32                                                       
            !  880  1.38                         !    !  880  1.38                                                       
            !  990  1.43                         !    !  990  1.43                                                       
            ! 9999  1.50                         !    ! 9999  1.50                                                       

          ! CH2O distribution/mobilizatio        !    ! CH2O distribution/mobilization                                   
                                                 !    PTFMN = 0.75                                                       
            PTFX = 0.98  ! PTFMX                 !    PTFMX = 0.98      
                                                 !    PTFA = 0.10                                                        
            ! PTFS  PTFA  STFR
            ! 0.65  0.10  0.00
            ! 0.70  0.10  0.15
            ! 0.75  0.10  0.51
            ! 0.80  0.10  1.00
            ! 0.98  0.35  1.00
            ! 0.98  0.35  1.00
            
            ! Cold hardiness                     !    ! Cold hardiness                                                   
            LT50S = -6.0  ! TKUH                 !    TKUH = -6.0       
                                                 !    TKDTI = 2.0                                                        
                                                 !    TKLF = 0.0                                                         
                                                 !    TKSPAN = 2.0                                                       
            HDUR = 10.0                          !    HDUR = 10.0       
                                                 !    HLOST = 10.0                                                       
                                                 !    HLOSF = 0.2                                                        
            ! Temperature responses
            ! CSCRP                                       
            !RRATE TRGEM TRDV1 TRDV4 TRDV8 TRLFG TRPHS TRVRN TRHAR TRGFW TRGFN 
            !    0     1     0     0     0     0     0    -5    -5     0     0 
            !  1.0    26    26    26    30    10     5     0     0    16    16 
            !  1.0    50    50    50    50    20    25     7     5    35    35 
            !    0    60    60    60    60    35    35    15    10    45    45 
            ! CSCER                              
            !RRATE TRGEM TRDV1 TRDV2 TRLFG TRPHS TRVRN TRHAR TRGFW TRGFN             
            !    0     1     0     0     0     0    -5    -5     0     0             
            !  1.0    26    26    30    10     5     0     0    16    16             
            !  1.0    50    50    50    20    25     7     5    35    35             
            !    0    60    60    60    35    35    15    10    45    45             
                                                                                                                         
          ! Water shortage effects                                                                                     
            RWUPM = 0.02
            RWUMX = 0.03
            WFPU = 1.0                           !    WFPU = 1.0        
            WFPGF = 1.0
                                                 !    WFPL = 0.0                                                         
            WFGU = 1.3                           !    WFGU = 1.3        
                                                 !    WFGL = 0.0                                                         
            WFTU = 1.0   ! WFTU                  !    WFTU = 1.0        
            WFTL = 0.5   ! WFTL                  !    WFTL = 0.5        
            WFSU = 0.6                           !    WFSU = 0.6        
                                                 !    WFSF = 0.5                                                         
            LLOSW = 0.02                         !    LLOSA = 0.02      
                                                 !    WFGEU = 0.5                                                        
                                                 !    WFRGU = 0.25                                                       
                                                 !                                                                       
           ! NITROGEN uptake,distribution,      
             NH4MN = 0.5                         !     NH4MN = 0.5      
             NO3MN = 0.5                         !     NO3MN = 0.5      
             NTUPF = 0.05                        !     NTUPF = 0.05     
           ! Nitrogen concentrations                                                                         
             GRNMX = 3.2  ! GN%MS                !     GNPCMX = 4.5     
                                                 !     GNPCMN = 0.5                                                      
             SDNPCI = 1.9                        !     SDNPCI = 1.9     
             ! CSCRP
             ! LN%S LN%MN  SN%S SN%MN  RN%S RN%MN                              
             !  8.0  0.80  2.50  0.65  2.00  1.50                              
             !  1.0  0.55  0.40  0.40  1.70  1.25                              
             ! CSCER
             ! LN%S  SN%S  RN%S LN%MN SN%MN RN%MN
             !  8.0   2.5  2.04  0.80  0.65  1.53     
             !  6.4   2.0  1.97  0.75  0.60  1.47
             !  5.3   1.0  1.89  0.70  0.55  1.41
             !  4.0   0.8  1.82  0.65  0.50  1.36
             !  3.1   0.6  1.77  0.60  0.45  1.33
             !  2.7   0.5  1.73  0.60  0.40  1.29
             !  1.5   0.4  1.68  0.60  0.40  1.26
             !  1.0   0.4  1.68  0.55  0.40  1.26
             XNFS = 20     ! NLABPC              !     NLABPC = 20      
             NFPU = 1.0                          !     NFPU = 1.0       
             NFPL = 0.0                          !     NFPL = 0.0       
             NFGU = 1.0                          !     NFGU = 1.0       
             NFGL = 0.0                          !     NFGL = 0.0       
             NFTU = 1.0   ! NFTU                 !     NFTU = 1.0       
             NFTL = 0.0   ! NFTL                 !     NFTL = 0.0       
             NFSU = 0.4                          !     NFSU = 0.4       
             NFSF = 0.1                          !     NFSF = 0.1       
             NCRG = 30                           !     NCRG = 30        
             LLOSN = 0.02
         ENDIF    !  EXAMINE(20)                                      
        
        ! DEFAULTS
        IF (LAXS.LE.0.0) LAXS = 900.0
        IF (LAWCF.LE.0.0) THEN
          LAWCF = 0.01    
          WRITE (fnumwrk,*) ' '
          WRITE (fnumwrk,*) ' Default of 0.01 used for LAWCF'
        ENDIF
        IF (TRGEM(3).LE.0.0) TRGEM = TRDV1
        IF (PPEND.LE.0.0) PPEND = 2.0          
        IF (VEFF.LE.0.0) VEFF = 0.5
        IF (VEFF.GT.1.0) VEFF = 1.0

        IF (PHINTL(1).LE.0.0) PHINTL(1) = 2.0
        IF (PHINTL(2).LE.0.0) PHINTL(2) = 20.0
        IF (PHINTL(3).LE.0.0) PHINTL(3) = 200.0
        IF (PHINTF(1).LE.0.0) PHINTF(1) = 0.8
        IF (PHINTF(2).LE.0.0) PHINTF(2) = 1.0
        IF (PHINTF(3).LE.0.0) PHINTF(3) = 1.0

        IF (RDGS2.LE.0.0) RDGS2 = RDGS1
        IF (RDGTH.LE.0.0) RDGTH = 275
        IF (RSEN.LT.0.0) RSEN = 0.1
        IF (RSPCX.LT.0.0) RSPCX = 80.0
        IF (RSUSE.LT.0.0) RSUSE = 0.10

        IF (TKLF.LT.-98.0) TKLF = -10.0

        ! For new (but not used!) N uptake routine.
        IF (NCNU.LE.0.0) NCNU = 30
        IF (RLFNU.LE.0.0) RLFNU = 2.0
        IF (WFNUU.LE.0.0) WFNUU = 1.0
        IF (WFNUL.LE.0.0) WFNUL = 0.0

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
        
        ! BASED ON ORIGINAL CERES -- FOR INITIAL CALIBRATION
        IF (CROP.EQ.'WH') THEN
          IF (PD(1).LE.0.0) THEN
            PD(1) = 400 * PHINTS / 95
            PD(2) = 3.0 * PHINTS
            PD(3) = 2.0 * PHINTS
            PD(4) = 200.0
            Write (fnumwrk,*) ' '
            Write (fnumwrk,*) 'CALCULATED phase duration being used'
            Write (fnumwrk,'(2X,4(F5.1,2X))') PD(1),PD(2),PD(3),PD(4)
            Write (fnumwrk,*) ' (P1=400*PHINT/95;P2=3.0*PHINT'
            Write (fnumwrk,*) ' (P3=2.0*PHINT;P4=200.0)'
          ENDIF  
        ENDIF  
        IF (CROP.EQ.'BA') THEN
          IF (PD(1).LE.0.0) THEN
            PD(1) = 300 * PHINTS / 70
            PD(2) = 3.2 * PHINTS
            PD(3) = 2.15* PHINTS
            PD(4) = 200.0
            Write (fnumwrk,*) ' '
            Write (fnumwrk,*) 'CALCULATED phase duration being used'
            Write (fnumwrk,'(2X,4(F5.1,2X))') PD(1),PD(2),PD(3),PD(4)
            Write (fnumwrk,*) ' (P1=300*PHINT/70;P2=3.2*PHINT'
            Write (fnumwrk,*) ' (P3=2.15*PHINT;P4=200.0)'
          ENDIF  
        ENDIF  

        WRITE (fnumwrk,*) ' '
        WRITE (fnumwrk,*) 'DERIVED COEFFICIENTS'
        
        ! NSFAC and NMNFAC are used for checking the component N concentrations only
        IF (nsfac.LE.0.0) NSFAC = 1.0
        IF (nmnfc.LE.0.0) NMNFC = 1.0
        DO L =0,9
          LCNCS(L) = LNPCS(L)/100.0*NSFAC
          SCNCS(L) = SNPCS(L)/100.0*NSFAC
          RCNCS(L) = RNPCS(L)/100.0*NSFAC
          LMNCS(L) = LNPCMN(L)/100.0*NMNFC
          SMNCS(L) = SNPCMN(L)/100.0*NMNFC
          RMNCS(L) = RNPCMN(L)/100.0*NMNFC
        ENDDO

        PD2(1) = PD2FR(1) * PD(2)
        Write (fnumwrk,*) '  PD2,PD2FR1 ',
     &     PD(2),PD2FR(1)
        PD4(1) = PD4FR(1) * PD(4)
        PD4(2) = PD4FR(2) * PD(4)
        PD4(3) = PD(4) -  PD4(1) - PD4(2)
        Write (fnumwrk,*) '  PD4,PD4FR1,PD4FR2 ',
     &     PD(4),PD4FR(1),PD4FR(2)
        IF (PD4(3).LE.0.0) THEN
          Write (fnumwrk,*) 'Lag phase duration <= 0.0!   '
          Write (*,*) 'Lag phase duration <= 0.0!   '
          WRITE (*,*) 'Program will have to stop'
          WRITE (*,*) 'Check WORK.OUT for details of run'
          STOP ' '
        ENDIF
        ! Kernel growing at half rate during lag period
        ! (=full rate for half period)
        G2 = G2KWT / (PD(5)+(PD(4)-PD4(1)-PD4(2))*0.50)
        
        WRITE (fnumwrk,*) '  Pd2(1)      :  ',pd2(1)
        WRITE (fnumwrk,*) '  Pd4(1)      :  ',pd4(1)
        WRITE (fnumwrk,*) '  Pd4(2)      :  ',pd4(2)
        WRITE (fnumwrk,*) '  Pd4(3)      :  ',pd4(3)
        WRITE (fnumwrk,*) '  G2          :  ',g2

        ! Critical stages
        ASTAGE = 4.0 + PD4(1) / PD(4)
        ASTAGEND = 4.0 + (PD4(1)+PD4(2)) / PD(4)
        WRITE (fnumwrk,*) '  Astage      :  ',astage
        WRITE (fnumwrk,*) '  Astagend    :  ',astagend

        ! Phase thresholds
        DO L = 0,10
          PTH(L) = 0.0
        ENDDO
        PTH(0) = PD(0)
        DO L = 1,10
          PTH(L) = PTH(L-1) + AMAX1(0.0,PD(L))
        ENDDO

        WRITE (fnumwrk,*) ' '
        WRITE (fnumwrk,*) 'DERIVED DATA'
        ! Check seedrate and calculate seed reserves
        IF (SDRATE.LE.0.0) SDRATE = SDSZ*PLTPOPP*10.0
        ! Reserves = 80% of seed (42% Ceres3.5)
        SEEDRSI = (SDRATE/(PLTPOPP*10.0))*0.8
        SDCOAT = (SDRATE/(PLTPOPP*10.0))*0.2 ! Non useable material
        ! Seed N calculated from total seed
        SDNAP = (SDNPCI/100.0)*SDRATE
        SEEDNI = (SDNPCI/100.0)*(SDRATE/(PLTPOPP*10.0))
        WRITE (fnumwrk,'(A16,2F7.1,A6)') '   Seedrs,Seedn:',
     &        SEEDRSI*PLTPOPP*10.0,SEEDNI*PLTPOPP*10.0,' kg/ha'

        ! Check dormancy
        IF (PLMAGE.LT.0.0) THEN
          PEGD = PGERM - (PLMAGE*STDAY)
          WRITE (fnumwrk,*)' '
          WRITE (fnumwrk,'(A30,F6.2)')
     &     '   Planting material dormancy ',plmage
          WRITE (fnumwrk,'(A30,F6.2)')
     &     '   Emergence+dormancy degdays ',pegd
        ELSE
          PEGD = PGERM
        ENDIF

        TKILL = LT50S 
        DF = PPFPE
        ! Define names for growth stages
        DO I = 1, 20
          STNAME(I) = '          '
          IF (CROP.EQ.'BA') THEN
            STNAME(I) = BASTGNAM (I)
          ELSEIF (CROP.EQ.'WH') THEN
            STNAME(I) = WHSTGNAM (I)
          ENDIF
        END DO
        IF (SLPF.LE.0.0 .OR. SLPF.GT.1.0) SLPF = 1.0
        IF (SLPF.LT.1.0) THEN
          WRITE (fnumwrk,*) ' '
          WRITE (fnumwrk,*)
     &     'WARNING  Soil fertility factor was less than 1.0: ',slpf
        ENDIF  

        ! Write-out inputs if required
        WRITE (fnumwrk,*) ' '
        WRITE (fnumwrk,*) 'EXPERIMENTAL DETAILS'
        WRITE (fnumwrk,*) '  TRUNNAME      ',TRUNNAME
        WRITE (fnumwrk,'(A18,2F7.1)')'   PLTPOP,ROWSPC  ',PLTPOPP,ROWSPC
        WRITE (fnumwrk,'(A18,2F7.1)')'   SDEPTH,SDRATE  ',SDEPTH,SDRATE
        WRITE (fnumwrk,'(A18,F7.4,F7.2)')
     &                               '   SDSZ,SDNPCI    ',SDSZ,SDNPCI
        WRITE (fnumwrk,'(A18, F7.1)')'   PLMAGE         ',PLMAGE
        WRITE (fnumwrk,'(A18,I7,A7)')'   YRHARF,IHARI   ',YRHARF,IHARI
        WRITE (fnumwrk,'(A18,2F7.1)')'   HPC,HBPC       ',HPC,HBPC
        WRITE (fnumwrk,'(A18,2A7  )')'   CROP,VARNO     ',CROP,VARNO
 
        WRITE (fnumwrk,*) ' '
        !IF (CUFILE.EQ.CUDIRFLE) THEN
          ! Following not used because cfg file set as Cropsim.cfg        
          !IF (CROP.EQ.'WH') THEN
          !  CALL Finddir (fnumtmp,cfgdfile,'WHD',cufile,cudirfle)
          !ELSEIF (CROP.EQ.'BA') THEN
          !  CALL Finddir (fnumtmp,cfgdfile,'BAD',cufile,cudirfle)
          !ENDIF
        !ENDIF
        IF (FILEIOT .EQ. 'DS4') THEN
          WRITE (fnumwrk,*) 'CULTIVAR DETAILS FROM: ',FILEIO(1:12)
          WRITE (fnumwrk,*) ' Originals from: ',CUDIRFLE(1:60)
        ELSE  
          WRITE (fnumwrk,*) 'CULTIVAR DETAILS FROM: ',CUDIRFLE
        ENDIF
        WRITE (fnumwrk,*) '  Varno,econo :  ',varno,' ',econo
        WRITE (fnumwrk,*) '  P1v,p1d,p5  :  ',p1v,p1d,pd(5)
        WRITE (fnumwrk,*) '  G1,g2kwt,g3 :  ',g1cwt,g2kwt,g3
        WRITE (fnumwrk,*) '  G2 mg/oC.d  :  ',g2
        WRITE (fnumwrk,*) '  Phint,Veff  :  ',phints,veff
 
        WRITE (fnumwrk,*) ' '
        WRITE (fnumwrk,*) 'ECOTYPE DETAILS FROM: ',ECDIRFLE
        WRITE (fnumwrk,*) '  TIl#S,TIPHE :  ',ti1lf,tilpe
        WRITE (fnumwrk,*) '  P1,2,3      :  ',(pd(i),i = 1,3)
        WRITE (fnumwrk,*) '  P4,5,6      :  ',(pd(i),i = 4,6)
        WRITE (fnumwrk,*) '  P2(1)       :  ',pd2(1)
        WRITE (fnumwrk,*) '  P4(1),P4(2) :  ',pd4(1),pd4(2)
        WRITE (fnumwrk,*) '  P4(1),P4(2) :  ',pd4(1),pd4(2)
        WRITE (fnumwrk,*) '  PHL1,PHF1   :  ',phintl(1),phintf(1)
        WRITE (fnumwrk,*) '  PHL2,PHF2   :  ',phintl(2),phintf(2)
        WRITE (fnumwrk,*) '  PHL3,PHF3   :  ',phintl(3),phintf(3)
        WRITE (fnumwrk,*) '  PARUE,PARU2 :  ',paruv,parur
        WRITE (fnumwrk,*) '  WFGU,WFPU   :  ',wfgu,wfpu
        WRITE (fnumwrk,*) '  WFPGF       :  ',wfpgf
        WRITE (fnumwrk,*) '  NFPU,NFPL   :  ',nfpu,nfpl
        WRITE (fnumwrk,*) '  KCAN,KEP    :  ',kcan,kep
        WRITE (fnumwrk,*) '  LA1S,LLIFE  :  ',lapot(1),llife
        WRITE (fnumwrk,*) '  LAFV,LAFR   :  ',lafv,lafr      
        WRITE (fnumwrk,*) '  AWNS,SGPHE  :  ',awns,p4sge
        WRITE (fnumwrk,*) '  TKFH        :  ',lt50h
        WRITE (fnumwrk,*) '  HTSTD       :  ',canhts
        WRITE (fnumwrk,*) '  LAWS,LAWCF  :  ',laws,lawcf
        WRITE (fnumwrk,*) '  RS%S,RS%X   :  ',rspcs,rspcx
        WRITE (fnumwrk,*) '  RSUSE       :  ',rsuse        
        WRITE (fnumwrk,*) '  NB.Rs%s is the percentage of stem ',
     &                          'assimilates going to reserves'
        WRITE (fnumwrk,*) '     instead of structural material.'
        WRITE (fnumwrk,*) '  GN%S        :  ',grns 
        WRITE (fnumwrk,*) '  LSPHS,LSPHE :  ',lsens,lsene

        WRITE(fnumwrk,*) ' '
        WRITE(fnumwrk,*) 'SPECIES DETAILS  FROM: ',SPDIRFLE

        WRITE(fnumwrk,'(A17,2F8.2)')'  PGERM,PEMRG :  ',pgerm,pemrg
        WRITE(fnumwrk,*)            '  P0          :  ',pd(0)
        WRITE(fnumwrk,*)            '  PPTHR,PPFPE :  ',p1dt,ppfpe
        WRITE(fnumwrk,*)            '  PPEND       :  ',ppend
        WRITE(fnumwrk,'(A17,2F8.2)')'   P6         : ',pd(6)
        WRITE(fnumwrk,*)            '  TDFAC       :  ',tildf
        WRITE(fnumwrk,*)            '  LWLOS,LRPHS :  ',lwlos,lrets
        WRITE(fnumwrk,*)            '  TGO02       :  ',latfr(1)
        WRITE(fnumwrk,'(A8, 4F7.1)')   ' TRGEM',(trgem(i),i = 1,4)
        WRITE(fnumwrk,'(A8, 4F7.1)')   ' TRDV1',(trdv1(i),i = 1,4)
        WRITE(fnumwrk,'(A8, 4F7.1)')   ' TRDV2',(trdv2(i),i = 1,4)
        WRITE(fnumwrk,'(A8, 4F7.1)')   ' TRLFG',(trlfg(i),i = 1,4)
        WRITE(fnumwrk,'(A8, 4F7.1)')   ' TRPHS',(trphs(i),i = 1,4)
        WRITE(fnumwrk,'(A8, 4F7.1)')   ' TRVRN',(trvrn(i),i = 1,4)
        WRITE(fnumwrk,'(A8, 4F7.1)')   ' TRHAR',(trlth(i),i = 1,4)
        WRITE(fnumwrk,'(A8, 4F7.1)')   ' TRGFW',(trgfw(i),i = 1,4)
        WRITE(fnumwrk,'(A8, 4F7.1)')   ' TRGFN',(trgfn(i),i = 1,4)
 
        WRITE(fnumwrk,'(A8,10F7.1)')   ' CO2RF',(co2rf(i),i = 1,10)
        WRITE(fnumwrk,'(A8,10F7.1)')   ' CO2F ',(co2f(i),i = 1,10)
        WRITE(fnumwrk,'(A8,10F7.2)')   ' PTFS ',(ptfs(i),i = 1,10)
        WRITE(fnumwrk,'(A8,10F7.2)')   ' PTFA ',(ptfa(i),i = 1,10)
        WRITE(fnumwrk,'(A8,10F7.2)')   ' STFR ',(stfr(i),i = 1,10)
        WRITE(fnumwrk,'(A8,10F7.2)')   ' LASF ',(plasf(i),i = 1,10)
        WRITE(fnumwrk,'(A8,10F7.2)')   ' CHT% ',(chtpc(i),i = 1,10)
        WRITE(fnumwrk,'(A8,10F7.2)')   ' CLA% ',(clapc(i),i = 1,10)
        WRITE(fnumwrk,'(A8, 7F7.4)')   ' LCNCS',(lcncs(i),i = 0,6)
        WRITE(fnumwrk,'(A8, 7F7.4)')   ' SCNCS',(scncs(i),i = 0,6)
        WRITE(fnumwrk,'(A8, 7F7.4)')   ' RCNCS',(rcncs(i),i = 0,6)
        WRITE(fnumwrk,'(A8, 7F7.4)')   ' LMNCS',(lmncs(i),i = 0,6)
        WRITE(fnumwrk,'(A8, 7F7.4)')   ' SMNCS',(smncs(i),i = 0,6)
        WRITE(fnumwrk,'(A8, 7F7.4)')   ' RMNCS',(rmncs(i),i = 0,6)
        WRITE(fnumwrk,'(A17,3F8.2)')   ' L,S,R LIGNIN  ',
     &                                   lligp,sligp,rligp
        WRITE(fnumwrk,'(A17, F8.2)')'   GRAIN LIGNIN  ',gligp
        WRITE(fnumwrk,'(A17,2F8.2)')'   RWUMXS,RWUMX  ',rwumxs,rwumx
        WRITE(fnumwrk,'(A17, F8.2)')'   RWUPM         ',rwupm 
        WRITE(fnumwrk,'(A17, F8.2)')'   RLWR cm/g     ',rlwr
        WRITE(fnumwrk,'(A17,2F8.4)')'   WFRGU,NCRG    ',wfrgu,ncrg
        WRITE(fnumwrk,'(A17,2F8.2)')'   WFGEU,SDAFR   ',wfgeu,sdafr
        WRITE(fnumwrk,'(A17,2F8.4)')'   SDWT,SDN%I    ',sdsz,sdnpci
        WRITE(fnumwrk,'(A17,2F8.4)')'   SDRATE,SDNI   ',sdrate,seedni
        WRITE(fnumwrk,'(A17, F8.2)')'   RDGTH         ',rdgth
        WRITE(fnumwrk,'(A17,2F8.2)')'   RDGS,RDG2     ',rdgs1,rdgs2
        WRITE(fnumwrk,'(A17,2F8.2)')'   RLDGR,RRESP   ',rldgr,rresp
        WRITE(fnumwrk,'(A17,2F8.2)')'   WFTU,WFTL     ',wftu,wftl
        WRITE(fnumwrk,'(A17,2F8.2)')'   NFTU,NFTL     ',nftu,nftl
        WRITE(fnumwrk,'(A17,2F8.2)')'   NFGU,NFGL     ',nfgu,nfgl
        WRITE(fnumwrk,'(A17,2F8.2)')'   WFSU,LLOSW    ',wfsu,llosw
        WRITE(fnumwrk,'(A17,2F8.2)')'   NFSU,LLOSN    ',nfsu,llosn
        WRITE(fnumwrk,'(A17, F8.2)')'   NFSF          ',nfsf
        WRITE(fnumwrk,'(A17,2F8.2)')'   WFNUU,WFNUL   ',wfnuu,wfnul
        WRITE(fnumwrk,'(A17,2F8.2)')'   NO3MN,NH4MN   ',no3mn,nh4mn
        WRITE(fnumwrk,'(A17,2F8.2)')'   RLFNU,NCNU    ',rlfnu,ncnu
        WRITE(fnumwrk,'(A17,2F8.2)')'   TKUH,HDUR     ',lt50s,hdur
        WRITE(fnumwrk,'(A17, F8.2)')'   TKLF          ',tklf
        WRITE(fnumwrk,'(A17,2F8.2)')'   PTFMX         ',ptfx
        WRITE(fnumwrk,'(A17,2F8.2)')'   SSEN,RSEN     ',ssen,rsen
        WRITE(fnumwrk,'(A17,2F8.2)')'   SAWS,LSHAWS   ',saws,lshaws
        WRITE(fnumwrk,'(A17,2F8.2)')'   LSHFR,LAXS    ',lshfr,laxs
        WRITE(fnumwrk,'(A17,2F8.2)')'   PHL1,PHF1     ',
     &   phintl(1),phintf(1)
        WRITE(fnumwrk,'(A17,2F8.2)')'   SSPHS         ',ssstg
        WRITE(fnumwrk,'(A17,2F8.2)')'   NLAB%,GN%MX   ',xnfs,grnmx
        WRITE(fnumwrk,'(A17,2F8.2)')'   NTUPF         ',ntupf       
        WRITE(fnumwrk,'(A17,2F8.2)')'   CHFR,CHSTG    ',chfr,chstg
        WRITE(fnumwrk,'(A17,2F8.2)')'   TPAR,TSRAD    ',part,sradt

        IF (CROP.EQ.'WH') THEN
          WRITE(fnumwrk,*) ' '
          WRITE(fnumwrk,'(A55)')
     &     ' PHASE DURATIONS                                       '
          WRITE(fnumwrk,'(A21,5F6.1)') '  CERES INPUTS     = ',
     &     PD(1),PD(2),PD(3),PD(4),PD(5)
          WRITE(fnumwrk,'(A21,5F6.1)') '  CERES from PHINT = ',
     &     PHINTS*400.0/95.0,PHINTS*3.0,PHINTS*2.0,200.0,PD(5)
          WRITE(fnumwrk,'(A38)')
     &     '  ZADOKS FROM CERES INPUTS AND PHINT  '
          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P1   ->TS  2 ',
     &      PD(1),PHINTS*400.0/95.0  
          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P2   ->Jt  3 ',
     &      PD(2)*PD2FR(1),PHINTS*3.0*PD2FR(1)  
          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P3  TS->LL 4 ',
     &     PD(2)*(1.0-PD2FR(1)),PHINTS*3.0*(1.0-PD2FR(1))             
          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P4  LL->SE 5 ',
     &     PD(3),PHINTS*2.0      
          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P5 ESG->AN 6 ',
     &     PD(4)*PD4FR(1),200.0*PD4FR(1)           
          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P6  AN->EA 7 ', 
     &     PD(4)*PD4FR(2),200.0*PD4FR(2)
          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P7  EA->EL 8 ',
     &     PD(4)*(1.0-(PD4FR(1)+PD4FR(2))),200*(1.0-(PD4FR(1)+PD4FR(2)))
          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P8  EL->PM 9 ',
     &     PD(5),PD(5)
        ENDIF

        IF (CROP.EQ.'BA') THEN
          WRITE(fnumwrk,*) ' '
          WRITE(fnumwrk,'(A55)')
     &     ' PHASE DURATIONS                                       '
          WRITE(fnumwrk,'(A21,5F6.1)') '  CERES INPUTS     = ',
     &     PD(1),PD(2),PD(3),PD(4),PD(5)
          WRITE(fnumwrk,'(A21,5F6.1)') '  CERES from PHINT = ',
     &     PHINTS*300.0/75.0,225.0,150.0,200.0,PD(5)
          WRITE(fnumwrk,'(A38)')
     &     '  ZADOKS FROM CERES INPUTS AND PHINT  '
          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P1   ->TS  2 ',
     &      PD(1),PHINTS*300.0/75.0  
          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P2   ->Jt  3 ',
     &      PD(2)*PD2FR(1),225.0*PD2FR(1)  
          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P3  TS->LL 4 ',
     &     PD(2)*(1.0-PD2FR(1)),225.0*(1.0-PD2FR(1))             
          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P4  LL->SE 5 ',
     &     PD(3),150.0      
          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P5 ESG->AN 6 ',
     &     PD(4)*PD4FR(1),200.0*PD4FR(1)           
          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P6  AN->EA 7 ', 
     &     PD(4)*PD4FR(2),200.0*PD4FR(2)
          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P7  EA->EL 8 ',
     &     PD(4)*(1.0-(PD4FR(1)+PD4FR(2))),200*(1.0-(PD4FR(1)+PD4FR(2)))
          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P8  EL->PM 9 ',
     &     PD(5),PD(5)
        ENDIF

        ! End of initiation flags,etc..
        CFLINIT = 'Y'
        TNI = TN
        CNI = CN
        SNI = SN
        ONI = ON
        RNI = RN
        KEPI = KEP
        KCANI = KCAN
        RWUPMI = RWUPM 
        RWUMXI = RWUMX

        CROPP = CROP
        VARNOP = ' '
        VARNOP = VARNO
        CUDIRFLP = ' '
        CUDIRFLP = CUDIRFLE
        IF (RNMODE.EQ.'T') CUDIRFLP = ' '
        ECONOP = ' '
        ECONOP = ECONO
        ECDIRFLP = ' '
        ECDIRFLP = ECDIRFLE
        SPDIRFLP = ' '
        SPDIRFLP = SPDIRFLE

        SEEDRSUX = 0.0
        LAGSTAGE = -99.0
        TNUMOUT = 0.0
        GPLASENS = -99.0
        DO L=1,LNUMX
          AFLFSUM(L) = 0.0
          WFLFSUM(L) = 0.0
          NFLFSUM(L) = 0.0
          TFLFSUM(L) = 0.0
          WFLFNUM(L) = 0
          AFLF(L) = 0.0
          WFLF(L) = 0.0
          NFLF(L) = 0.0
          TFLF(L) = 0.0
        ENDDO

        IF (FILEIOT.EQ.'DS4') WRITE(fnumwrk,*)' '
        WRITE(FNUMWRK,'(A22)')' OUTPUTS              '
        ! Control switch for OUTPUT file names
        CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'FNAME',fname)
        IF (FNAME.EQ.'Y') THEN
          WRITE(FNUMWRK,*)'File names switched from standard. '
        ELSE  
          WRITE(FNUMWRK,*)'Standard file names. '
        ENDIF   

        WRITE(FNUMWRK,*)' '
        WRITE(FNUMWRK,'(A22)')' DURING RUN STATUS:   '

      ELSEIF (DYNAMIC.EQ.RATE) THEN
      
          ! Update so that temporary outputs in rate have correct DAP
          DAE = MAX(0,CSTIMDIF(STGDOY(9),YEARDOY))
          DAP = MAX(0,CSTIMDIF(STGDOY(7),YEARDOY))
          DAS = MAX(0,CSTIMDIF(YEARSIM,YEARDOY))
          IF (ISTAGE.EQ.6) DAPM = DAPM + 1
      
        ! LAH 07/12/2008 For problem of date calculation.
        ! YEARPLTCSM established by CSM;brought across in argument list.
        ! LAH 29/06/11 Added automatic as well
        IF (FILEIOT.EQ.'DS4') THEN
!         IF (IPLTI.EQ.'A' .OR. (INDEX('FQN',RNMODE) > 0)) THEN
          IF (IPLTI.EQ.'A' .OR. IPLTI.EQ.'F' .OR. 
     &       (INDEX('FQNY',RNMODE) > 0)) THEN
            YEARPLTP = YEARPLTCSM
          ENDIF  
        ENDIF

        IF (FILEIOT.EQ.'XFL') WRITE(fnumwrk,'(A28,I3,I8,2F6.2)')
     &   ' CN,YEARDOY,XSTAGE1,LEAFNUM ',cn,yeardoy,xstage,lnumsd
     
        IF (YEARDOY.LT.YEARPLTP)
     &  WRITE(fnumwrk,*) 'yeardoy,YEARPLT,YEARPLTP   ',
     &                     yeardoy,YEARPLT,YEARPLTP

        CFLINIT = 'N'    ! Reset initiation flag for next run

        IF (YEARPLT.GT.9000000) THEN            ! If before planting
          ! Initialize planting depth temperature and water variables
          TSDEP = 0.0
          CUMSW = 0.0
          AVGSW = 0.0
          IF(YEARPLTP.GT.0 .AND. YEARPLTP.LT.9000000)THEN
            IF(YEARDOY.EQ.YEARPLTP)THEN
C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY              
              !YEARPLT = CSYEARDOY(YEARPLTP)
              CALL Y4K_DOY(YEARPLTP,FILEIO,0,ERRKEY,3)
              YEARPLT = YEARPLTP
              PLTPOP = PLTPOPP
              TNUM = 1.0
            ENDIF
          ELSE
            ! Automatic planting
            ! Check window for automatic planting,PWDINF<YEARPLTP<PWDINL
            IF (YEARDOY.GE.PWDINF.AND.YEARDOY.LE.PWDINL) THEN
              ! Within planting window.
              ! Determine if soil temperature and soil moisture ok
              ! Obtain soil temperature, TSDEP, at 10 cm depth
              I = 1
              XDEP = 0.0
              DO WHILE (XDEP .LT. 10.0)
                XDEP = XDEP + DLAYR(I)
                TSDEP = ST(I)
                I = I + 1
              END DO
              ! Compute average soil moisture as percent, AVGSW
              I = 1
              XDEP = 0.0
              CUMSW = 0.0
              DO WHILE (XDEP .LT. SWPLTD)
                XDEPL = XDEP
                XDEP = XDEP + DLAYR(I)
                IF (DLAYR(I) .LE. 0.) THEN
                  !IF SOIL DEPTH IS LOWER THAN SWPLTD -US
                  XDEP = SWPLTD
                  CYCLE
                ENDIF
                DTRY = MIN(DLAYR(I),SWPLTD - XDEPL)
                CUMSW = CUMSW + DTRY *
     &           (MAX(SW(I) - LL(I),0.0)) / (DUL(I) - LL(I))
                I = I + 1
              END DO
              AVGSW = (CUMSW / SWPLTD) * 100.0
              WRITE (fnumwrk,*) 'Date thresholds ',pwdinf,pwdinl
              WRITE (fnumwrk,*) 'Water thresholds ',swpltl,swplth
              WRITE (fnumwrk,*) 'Water ',avgsw
              WRITE (fnumwrk,*) 'Temperature thresholds ',pttn,ptx
              WRITE (fnumwrk,*) 'Temperature ',tsdep
              IF (TSDEP .GE. PTTN .AND. TSDEP .LE. PTX) THEN
                IF (AVGSW .GE. SWPLTL .AND. AVGSW .LE. SWPLTH) THEN
                  YEARPLT = YEARDOY
                  PLTPOP = PLTPOPP
                  CFLFAIL = 'N'
                ENDIF
              ENDIF
            ELSE
              IF (YEARDOY.GT.PWDINL) THEN
                CFLFAIL = 'Y'
                STGDOY(1) = -99
                STGDOY(2) = -99
                STGDOY(3) = -99
                STGDOY(4) = -99
                STGDOY(5) = -99
                STGDOY(6) = -99
                STGDOY(7) = -99
                STGDOY(8) = -99
                STGDOY(9) = -99
                STGDOY(10) = YEARDOY
                STGDOY(11) = YEARDOY
                ISTAGE = 7
                XSTAGE = 7.0
                WRITE (fnumwrk,*) ' '
                WRITE (fnumwrk,*)
     &           'Automatic planting failure on ',yeardoy
              ENDIF
            ENDIF
          ENDIF

          IF (YEARDOY.EQ.YEARPLTP) WRITE (fnumwrk,*)
     &      'Planting on: ',yeardoy
          WRITE (fnumwrk,*)
     &      'Initialising soil profile and other N aspects on: ',yeardoy

          STGDOY(7) = YEARPLT
          SEEDN = SEEDNI
          SEEDRS = SEEDRSI
          SEEDRSAV = SEEDRS
                    
          H2OPROFILEI = 0.0
          H2OROOTZONEI = 0.0
          AH2OPROFILEI = 0.0
          AH2OROOTZONEI = 0.0
          SNO3PROFILEI = 0.0
          SNH4PROFILEI = 0.0
          SNO3ROOTZONEI = 0.0
          SNH4ROOTZONEI = 0.0
          DO L = 1, NLAYR
            FAC(L) = 10.0/(BD(L)*DLAYR(L))
            SNO3PROFILEI = SNO3PROFILEI + (NO3LEFT(L)/FAC(L))
            SNH4PROFILEI = SNH4PROFILEI + (NH4LEFT(L) / FAC(L))
            AH2OPROFILEI = AH2OPROFILEI + ((SW(L)-LL(L))*DLAYR(L))*10.0
            H2OPROFILEI = H2OPROFILEI + SW(L)*DLAYR(L)*10.0
            IF (RLV(L).GT.0.0) THEN
              AH2OROOTZONEI = AH2OROOTZONEI+((SW(L)-LL(L))*DLAYR(L))*10.
              H2OROOTZONEI = H2OROOTZONEI+SW(L)*DLAYR(L)*10.
              SNO3ROOTZONEI = SNO3ROOTZONEI + (NO3LEFT(L)/FAC(L))
              SNH4ROOTZONEI = SNH4ROOTZONEI + (NH4LEFT(L)/FAC(L))
            ENDIF
          END DO

        ENDIF

        ! Reset control flag
        !IF (YEARDOY.GE.YEARPLT) DYNAMICI = 0

        IF (YEARDOY.GE.YEARPLT) THEN   

          ! Photosynthetically active radiation
          PARAD = PARADFAC*SRAD

          ! Mean temperature
          TMEAN = (TMAX+TMIN)/2.0
C-GH      IF (snow.GT.0) THEN
          IF (snow .GT. 0.0) THEN
            tmeans = 0.0
          ELSE
            tmeans = tmean
          ENDIF

          ! Day and night temperatures
          TDAY = TMEAN
          TNIGHT = TMEAN
          ! NB.These could be set in various ways. In Ceres 3.5,there
          ! were various modifications for different processes. Work
          ! with G.McMaster, however, showed that (at least for
          ! development) these were no better than using the daily
          ! mean. Hence the day and night temperatures are set equal
          ! to the daily mean. Other simple settings could be:
          ! TDAY = TMAX
          ! TNIGHT = TMIN
          ! TDAY = TMEAN + 0.5*(TMAX-TMEAN)
          ! TNIGHT = TMIN + 0.5*(TMEAN-TMIN)
          ! And more complex settings could involve using the hourly
          ! temperatures, or modifying values depending on the
          ! difference between TMAX and TMIN.
          TMEAN20S = 0.0
          SRAD20S = 0.0
          STRESS20S = 0.0
          TMEANNUM = TMEANNUM + 1.0
          DO L = 20,2,-1
            TMEAND(L) = TMEAND(L-1)
            TMEAN20S = TMEAN20S + TMEAND(L)
            SRADD(L) = SRADD(L-1)
            SRAD20S = SRAD20S + SRADD(L)
            STRESS(L) = STRESS(L-1)
            STRESS20S = STRESS20S + STRESS(L)
          ENDDO
          TMEAND(1) = TMEAN
          TMEAN20S = TMEAN20S + TMEAND(1)
          SRADD(1) = SRAD
          SRAD20S = SRAD20S + SRAD
          STRESS(1) = AMIN1(WFG,NFG)
          STRESS20S = STRESS20S + STRESS(1)
          IF (TMEANNUM.GE.20.0) THEN
            IF (TMEANNUM.EQ.20.0) TMEAN20P = TMEAN20S/20.0
            TMEAN20 = TMEAN20S/20.0
            SRAD20 = SRAD20S/20.0
            STRESS20 = STRESS20S/20.0
          ELSE
            TMEAN20 = 0.0
            SRAD20 = 0.0
            STRESS20 = 0.0
          ENDIF
          IF (ADAT.GT.0) THEN
            ADAT10 = CSINCDAT(ADAT,10)
            !IF (YEARDOY.EQ.ADAT10) TMEAN20A = TMEAN20
            !IF (YEARDOY.EQ.ADAT10) SRAD20A = SRAD20
            IF (XSTAGE.GE.ASTAGEND.AND.TMEAN20A.LE.0.0) THEN
              TMEAN20A = TMEAN20
              SRAD20A = SRAD20
              STRESS20A = STRESS20
            ENDIF
          ENDIF

!  For investigation of temperature responses
!          WRITE(fnumwrk,*)' tmean   tfd   tfg  tfdg    tt'
!          DO 9821 TVI1 = 0,40
!           TMEAN = FLOAT(TVI1)
!           TMEANS = TMEAN
!         End of stuff to investigate temperature responses          
           
          ! Thermal time
          IF (ISTAGE.GT.4 .AND. ADAT.GT.0 .AND. ISTAGE.LE.6) THEN
            Tfout = TFAC4(trdv2,tmean,TT)
          ELSE
            Tfout = TFAC4(trdv1,tmeans,TT)
            TTmax = trdv1(2) - trdv1(1)
            IF (trgem(3).GT.0.0) THEN
              Tfgem = TFAC4(trgem,tmeans,TTGEM)
            ELSE
              Ttgem = tt
            ENDIF    
          ENDIF

          ! Thermal time averages for various periods
          IF (ISTAGE.LT.7.AND.ISTAGE.GE.4) THEN
            TT20S = 0.0
            TTNUM = TTNUM + 1
            DO L = 20,2,-1
              TTD(L) = TTD(L-1)
              TT20S = TT20S + TTD(L)
            ENDDO
            TTD(1) = TT
            TT20S = TT20S + TTD(1)
            IF (TTNUM.GE.20.0) THEN
              TT20 = TT20S/20.0
            ELSE
              TT20 = -99.0
            ENDIF
          ENDIF

          ! Temperature factors
          IF (ISTAGE.GE.9 .OR. ISTAGE.LE.2) THEN
            Tfv = TFAC4(trvrn,tmeans,TTOUT)
            Tfh = TFAC4(trlth,tmeans,TTOUT)
          ELSE
            TFV = 0.0
            TFH = 0.0
          ENDIF
          !IF (ISTAGE.LE.2) THEN
            Tfg = TFAC4(trlfg,tmean,TTOUT)
          !ENDIF
          IF (ISTAGE.LE.6) THEN
            ! The original temperature response for photosynthesis had
            ! a more rapid rise at low temperatures than the 4 cardinal
            ! temperatures response now used. This is shown below in the
            ! original data:
            !TREFS  TFGR  TFPR  TFVR  TFHR TFGFR TFGNR TFGNM
            ! -5.0   -99   -99  0.00  0.00   -99   -99   -99
            !  0.0  0.00  0.00  1.00  1.00  0.00  0.00  0.00
            !  2.0   -99  0.40   -99   -99   -99   -99   -99
            !  5.0   -99   -99   -99  1.00   -99   -99   -99
            !  7.0   -99  0.70  1.00   -99   -99   -99   -99
            ! 10.0  1.00  0.85   -99  0.00   -99   -99   -99
            ! 15.0  1.00  1.00  0.00   -99   -99   -99   -99
            ! 16.0   -99   -99   -99   -99  1.00  1.00  1.00
            ! 20.0  1.00  1.00   -99   -99   -99   -99   -99
            ! 25.0  1.00   -99   -99   -99   -99   -99   -99
            ! 26.0   -99  0.85   -99   -99   -99   -99   -99
            ! 30.0   -99  0.50   -99   -99   -99   -99   -99
            ! 35.0  0.00  0.00   -99   -99  1.00  1.00  1.00
            ! 45.0   -99   -99   -99   -99  1.00  1.00  1.00
            ! The original call to obtain TFP was:
            ! TFP = TFCALC2(TREFS,TFPR,'20',TDAY,TDAY,SNOW)
            Tfp = TFAC4(trphs,tmean,TTOUT)
            ! Ceres35 PRFT = 1.0-0.0025*((0.25*TMIN+0.75*TMAX)-18.0)**2
          ENDIF
          IF (ISTAGE.EQ.4.OR.ISTAGE.EQ.5) THEN
            Tfgf = TFAC4(trgfw,tmean,TTOUT)
            Tfgn = TFAC4(trgfn,tmean,TTOUT)
          ENDIF
!         To investigate temperature responses          
!          write(fnumwrk,'(5f6.2)')tmean,tfout,tfg,tfout*tfg,tt 
!9821      continue
!          write(fnumwrk,*)'! ttmax = ',trdv1(2)-trdv1(1)
!          stop
!         End of stuff to investigate temperature responses          
          ! Radiation interception (if from competition model)
          IF (PARIP.GE.0.0) THEN
            PARI = PARIP/100.0
            WRITE(fnumwrk,'(A39,F6.2,A11,I2)')
     &       ' PARI from competition model          :',PARI,
     &       ' Component:',CN
            WRITE(fnumwrk,'(A39,F6.2,7X,F6.2)')
     &       ' Leaf area (laminae). Index,Per plant: ',
     &       LAI,PLA-SENLA
          ENDIF

          IF (fileiot(1:2).NE.'DS')
     &     CALL CSTRANS(ISWWAT,                            !Control
     &     TMAX, TMIN, WINDSP, CO2, EO,                    !Weather
     &     CROP, LAI, KEP,                                 !Crop,LAI
     &     eop,                                            !Pot.pl.evap
     &     DYNAMICI)                                       !Control

          IF (fileiot(1:2).NE.'DS')
     &     CALL CSROOTWU(ISWWAT,                           !Control
     &     NLAYR, DLAYR, LL, SAT,                          !Soil
     &     EOP,                                            !Pot.evap.
     &     RLV, RWUPM, RWUMX,                              !Crop state
     &     SW,                                             !Soil h2o
     &     uh2o, trwup,                                    !H2o uptake
     &     DYNAMICI)                                       !Control

          ! Water status factors
          WFG = 1.0
          WFP = 1.0
          WFT = 1.0
          IF (ISWWAT.EQ.'Y' .AND. ISTAGE.LT.7) THEN
            IF (EOP.GT.0.0) THEN
              WUPR = TRWUP/(EOP*0.1)
              WFG = AMAX1(0.0,AMIN1(1.0,WUPR/WFGU))
              WFP = AMAX1(0.0,AMIN1(1.0,WUPR/WFPU))
              IF (XSTAGE.GE.4.0) WFP = 1.0-(1.0-WFP)*WFPGF !Grain fill
              WFT = AMAX1(0.0,AMIN1(1.0,(WUPR-WFTL)/(WFTU-WFTL)))
            ENDIF
          ENDIF

          ! Nitrogen status factors
          NFG = 1.0
          NFP = 1.0
          NFT = 1.0
          IF (ISWNIT.EQ.'Y' .AND. ISTAGE.LT.7) THEN
            LMNCG = LMNC + NFGL * (LCNC-LMNC)
            LCNCG = LMNC + NFGU * (LCNC-LMNC)
            LMNCP = LMNC + NFPL * (LCNC-LMNC)
            LCNCP = LMNC + NFPU * (LCNC-LMNC)
            LCNCSEN = LMNC + NFSU * (LCNC-LMNC)
            LMNCT = LMNC + NFTL * (LCNC-LMNC)
            LCNCT = LMNC + NFTU * (LCNC-LMNC)
            IF (LFWT.GT.0.0) THEN
              NFG = AMIN1(1.0,AMAX1(0.0,(LANC-LMNCG)/(LCNCG-LMNCG)))
              NFP = AMIN1(1.0,AMAX1(0.0,(LANC-LMNCP)/(LCNCP-LMNCP)))
              NFT = AMIN1(1.0,AMAX1(0.0,(LANC-LMNCT)/(LCNCT-LMNCT)))
            ENDIF
          ENDIF
          
          ! Daylength factor. 
          IF (ISTAGE.GE.7) THEN
            DF = PPFPE
          ELSEIF (ISTAGE.GE.1) THEN
            IF (P1D.GE.0.0) THEN      ! Long day plants
              ! Below is a possible age adjusted P1D. But not used
              P1DA=AMAX1(.0,(P1D/10000)-(P1D/10000)*P1DAFAC*(LNUMSD-5.))
              !P1DAFAC is an age adjustment factor
              DF = AMAX1(0.0,AMIN1(1.0,1.0-(P1D/10000)*(P1DT-DAYLT)**2))
            ELSE                      ! Short day plants
              DF = 
     &         AMAX1(0.0,AMIN1(1.0,1.0-(ABS(P1D)/1000)*(DAYLT-P1DT)))
            ENDIF
          ENDIF  
          
          IF (RSTAGE.GT.PPEND .AND.ISTAGE.LT.7) THEN
            DF = 1.0
          ENDIF
          
          ! Light intensity factor
          LIF2 = 1.0
          IF (CROP.EQ.'BA' .AND. SRAD.LE.10.0) THEN
            LIF2 = 1.0 - ( 10.0-SRAD)**2/PLTPOP
          ENDIF
          
          ! Developmental units
          DU = TT*VF*DF*LIF2    ! NB. Changed from Ceres 3.5
          ! DU = TT*AMIN1(VF,DF)*LIF2    ! Ceres 3.5
          DU = AMAX1(0.0, DU)
          
          ! Water factor for germination
          IF (ISWWAT.EQ.'Y' .AND. ISTAGE.GT.7) THEN
            DO L = 1, NLAYR
              CUMDEP = CUMDEP + DLAYR(L)
             IF (SDEPTH.LT.CUMDEP) GO TO 100
            END DO
  100       CONTINUE
            L0 = L                       ! L0 is layer with seed
            DO L = 1,NLAYR
              SWP(L) = 
     &         AMIN1(1.0,AMAX1(.0,((SW(L)-LL(L))/(DUL(L)-LL(L)))))
            ENDDO
            ! LAH Changed after query by Hong via GH
            !SWP(0) = AMIN1(1.0,AMAX1(0.0,(SWP(1)-(SWP(2)-SWP(1)))))
            !SWP(0) is a value at the soil surface
            SWP(0) = AMIN1(1.0,AMAX1(0.0,(SWP(1)-0.5*(SWP(2)-SWP(1)))))
            IF (L0.GT.1) THEN
              SWPSD = SWP(L0)
            ELSE
              ! LAH Changed after query by Hong via GH
              ! SDEPTH*(SWP(2)-SWP(0))))
              SWPSD = SWP(0) + (SDEPTH/DLAYR(1))*(SWP(2)-SWP(0))
            ENDIF
            WFGE = AMAX1(0.0,AMIN1(1.0,(SWPSD/WFGEU)))
          ELSE
            WFGE = 1.0
          ENDIF
          
          ! Germination units
          !GEU = TT*WFGE
          GEU = TTGEM*WFGE
          
          ! Initializations for Growth calculations
          CARBO = 0.0
          GROST = 0.0
          GROLF = 0.0
          GROLFP = 0.0
          GROGRP = 0.0
          GROGRPA = 0.0
          GROGRST = 0.0
          GRORS = 0.0
          GRORSSD = 0.0
          GRORSPM = 0.0
          GRORSP = 0.0
          GROST = 0.0
          GROSTP = 0.0
          PCARB = 0.0
          PLAS = 0.0
          PLASS = 0.0
          PLTLOSS = 0.0
          RSFR = 0.0
          RTRESP = 0.0
          RTWTG = 0.0
          RTWTGRS = 0.0
          TNUMD = 0.0
          TNUMG = 0.0
          TNUMLOSS = 0.0
          DO L = 1,2
            PLAG(L) = 0.0
            PLAGT(L) = 0.0
            PLAGTP(L) = 0.0
          ENDDO
          XSTAGEFS = 0.0
          
          ! 'Light intensity' factor (0.6,1.0 at SRAD's of 0,7)
          LIF1 = 1.0
          IF (CROP.EQ.'BA' .AND. SRAD.LE.10.0) THEN
            LIF1 = 1.0 - ((10.0-SRAD)**2*PLTPOP*0.000025)
          ENDIF
          
          ! CO2 factor
          CO2FP = YVALXY(CO2RF,CO2F,CO2)
          
          ! Temperature factors
          ! Must check. One cold night --> no phs next day!!
          
          ! Tops partition fraction; standard first,then adjustment
          IF (PTFS(ISTAGE+1).GT.0)
     &     PTFSS = PTFS(ISTAGE) + (PTFS(ISTAGE+1)-PTFS(ISTAGE))*SSTAGE
          IF (PTFA(ISTAGE).GT.0) THEN
           PTF = AMIN1(PTFX,PTFSS + PTFA(ISTAGE)*AMIN1(WFP,NFG,LIF1))
          ELSE
           PTF = AMIN1(PTFX,PTFSS)
          ENDIF
          
          ! Within tops distribution fractions
          LFFR = 1.0
          STFRSTG = 0.0
          STFRSTG = (STFR(ISTAGE)
     &       + ((STFR(ISTAGE+1)-STFR(ISTAGE))*(XSTAGE-FLOAT(ISTAGE))))
          IF (ISTAGE.GE.3.AND.ISTAGE.LT.7) STFRSTG = 1.0
          IF (ISTAGE.GT.6) STFRSTG = 0.0
          LFFR = 1.0 - STFRSTG                                          
          IF (XSTAGE.GT.P4SGE) THEN
            RSFR = 1.0
          ELSE
            RSFR = RSPCS/100.0
          ENDIF
          
          ! CO2 Assimilation
          PCARB = PARUV * PARAD/PLTPOP * PARI
          IF (XSTAGE.GE.3.0) THEN
            IF (PARUR.GT.0.0) THEN
             PCARB = PARUR * PARAD/PLTPOP * PARI
            ELSE
             PCARB = PARUV * PARAD/PLTPOP * PARI
            ENDIF  
          ENDIF
          
          ! PCARB = 7.5 * PARAD**0.6/PLTPOP*(1.0-EXP(-0.85*LAI)) !Ceres3
          ! Y1 = 1.5 - 0.768 * ((ROWSPC * 0.01)**2 * PLTPOP)**0.1
          ! PCARB = 1.48 * SRAD/PLTPOP * (1.0 - EXP(-Y1 * LAI)) ! Maize
          ! Following is as in Ceres 3.5. Now eliminated minimum choice
          ! CARBO = AMAX1(0.0,PCARB*CO2FP*TFP*AMIN1(WFP,NFP)*RSFP)
          
          ! LAH 07/08/2008 Added SLPF to match other models.
          !CARBO = AMAX1(0.0,PCARB*CO2FP*TFP*WFP*NFP*RSFP)
          CARBO = AMAX1(0.0,PCARB*CO2FP*TFP*WFP*NFP*RSFP*SLPF)
          ! Following is to stop assim once mature
          IF (XSTAGE.GT.6.0) CARBO = AMAX1(0.0,CARBO*(6.3-XSTAGE)/0.3)
          
          ! Available carbohydrate for growth
          IF (ISTAGE.EQ.6) THEN
            CARBOAT = 0.0
            CARBOR = 0.0
            CARBOASD = 0.0
            CARBOAPM = CARBO
          ELSE
            ! Can mobilize more than required! 
            ! Hence CH2O reserves may increase
            ! SDAVFR operates for both tops and roots
            IF (SEEDRSAV.GT.1.0E-9) THEN
              CARBOASD = SDAFR*(TT/STDAY)*SEEDRSAV
            ELSE
              CARBOASD = 0.0 
            ENDIF  
            SEEDRSAV = SEEDRSAV - CARBOASD
            CARBOAT = CARBOASD+CARBO*PTF
            CARBOR = CARBO*(1.0-PTF)
            CARBOAPM = 0.0
          ENDIF

          IF (XSTAGE.GE.LAFST.AND.XSTAGE.LT.7.0) THEN
            IF(LNUMSG.GT.0 .AND. LNSWITCH.LE.0.0) THEN
              LNSWITCH = LNUMSD
              WRITE(fnumwrk,*)' '
              WRITE(fnumwrk,*)
     &         'Leaf number when size increment changed ',lnswitch
              LASWITCH = lapot(lnumsg)
              WRITE(fnumwrk,*)
     &         'Leaf p.size when size increment changed ',Laswitch
               WRITE(fnumwrk,*)
     &         'Next p.size when size increment changed ',
     &          Lapot(lnumsg+1)
            ENDIF
          ENDIF   
          
          IF (LNUMSG.GT.0) THEN
            IF (LNUMSG.GT.0.AND.LNUMSG.LT.LNUMX) THEN
              IF (LNSWITCH.LE.0.0) THEN
                LAPOT(LNUMSG+1) = 
     &            AMAX1(LAPOT(1),LAPOT(LNUMSG))*(1.0+LAFV)
              ELSE  
                LAPOT(LNUMSG+1) = LAPOT(LNUMSG)*(1.0+LAFR)
              ENDIF
              IF (LAPOT(LNUMSG+1).GT.LAXS) LAPOT(LNUMSG+1) = LAXS
              ! NB. In Ceres 3.5 LAPOT(n) was LAPOT(1)*(LNUMSD**0.5) 
            ENDIF    
          ENDIF
          
          ! Growth potentials
          IF (ISTAGE.LE.2) THEN
            LAW = AMAX1(LAWS*LAWFRMN,LAWS-(LAWS*LAWCF)*(LNUMSG-1))
            ! LAW=Leaf area/weight (specific leaf area).Chages with lf #
            IF (LNUMSG.GT.0) THEN
              ! In Ceres overall temperature response for lf growth was:
              ! EGFT = 1.2 - 0.0042*(TEMPM-17.0)**2 
              ! Here, temperature response is a composite of temp response
              ! of development (leaf # increase) and leaf expansion.
              ! So, EGFT would equal TFD*TFG 
              ! Assimilates may control expansion if no reserve available
              ! Current leaf expands completely at current phint
              ! Leaves expand for 1 PHINT
              ! For leaf area growth (PLAG) Ceres 3.5 used: 
              !  PLAG(1) = LA1S * (LNUMSD**0.5) * ....
              ! (with LA1S = 7.5 (= LAPOT(1),potential area of leaf 1)
              PLAG(1) = LAPOT(LNUMSG) * AMIN1(WFG,NFG) * TFG *
     &         AMIN1(TT/PHINT,(FLOAT(LNUMSG)-LNUMSD)) 
              ! NB. Temp response of development (TT) taken into acount 
              ! If new leaf. Will expand at current or new phint
              IF ((TT/PHINT).GT.(FLOAT(LNUMSG)-LNUMSD)) THEN
                ! If new leaf will be first leaf in 3rd phint phase
                IF (LNUMSD.LE.(PHINTL(1)+PHINTL(2)).AND.
     &              LNUMSD+(TT/PHINT).GT.(PHINTL(1)+PHINTL(2))) THEN
                    TTTMP = TT - PHINT*(FLOAT(LNUMSG)-LNUMSD)
                    PLAG(2) = LAPOT(lnumsg+1) * AMIN1(WFG,NFG) * TFG *
     &                        TTTMP/(PHINTS*PHINTF(3))
                ! If new leaf will be first in 2nd phint phase
                ELSEIF (LNUMSD.LE.PHINTL(1).AND.
     &                  LNUMSD+(TT/PHINT).GT.PHINTL(1)) THEN
                  TTTMP = TT - PHINT*(FLOAT(LNUMSG)-LNUMSD)
                  PLAG(2) = LAPOT(lnumsg+1) * AMIN1(WFG,NFG) * TFG *
     &             TTTMP/(PHINTS)
                ELSE
                ! New leaf in current phint phase
                  PLAG(2) = LAPOT(lnumsg+1) * AMIN1(WFG,NFG) * TFG *
     &            (TT/PHINT-(FLOAT(LNUMSG)-LNUMSD))
                ENDIF
              ! No new leaf
              ELSE
                PLAG(2) = 0.0
              ENDIF
              IF (TT.GT.0.0) THEN
                WFLFNUM(LNUMSG) = WFLFNUM(LNUMSG)+1.0
                WFLFSUM(LNUMSG) = WFLFSUM(LNUMSG)+WFG
                NFLFSUM(LNUMSG) = NFLFSUM(LNUMSG)+NFG
                TFLFSUM(LNUMSG) = TFLFSUM(LNUMSG)+TFG
                WFLF(LNUMSG) = WFLFSUM(LNUMSG)/WFLFNUM(LNUMSG)
                NFLF(LNUMSG) = NFLFSUM(LNUMSG)/WFLFNUM(LNUMSG)
                TFLF(LNUMSG) = TFLFSUM(LNUMSG)/WFLFNUM(LNUMSG)
              ENDIF
              PLAGTP(1) = PLAG(1)
              PLAGTP(2) = PLAG(2)
              DO L = 1,INT(TNUM)
                ! LAH MAY 2013 CHECK THIS ... PLAG SHOULD BE PLAGTP??
                IF (L.LT.20) THEN
                  PLAGTP(1) = PLAGTP(1) + PLAG(1)*LATFR(L+1)
     &                      * AMAX1(0.0,AMIN1(1.0,(TNUM-1.0)))
                  PLAGTP(2) = PLAGTP(2) + PLAG(2)*LATFR(L+1)
     &                      * AMAX1(0.0,AMIN1(1.0,(TNUM-1.0)))
                ELSE
                  TNUMOUT = TNUMOUT + 1
                  IF (TNUMOUT.LT.2)
     &             WRITE(fnumwrk,*)'Tiller number at limit of 20! '
                ENDIF
              ENDDO
            ENDIF
          ENDIF
          
          IF (LAW.GT.0.0) 
     &      GROLFP = ((PLAGTP(1)+PLAGTP(2))/LAW)/(1.0-LSHFR)          
          IF (LFFR.GT.0.0) THEN
            GROSTP = GROLFP*(STFRSTG/LFFR)
          ELSE
            GROSTP = CARBOAT*(1.0-RSFR)
          ENDIF  
          GRORSP = CARBOAT*RSFR
          IF (ISTAGE.EQ.4.OR.ISTAGE.EQ.5) THEN
            GROGRP = AMAX1(0.0,LAGSTAGE*TFGF*GRNUM*G2*DU*0.001)
            IF (LAGSTAGE.GT.0.0.AND.TFGF.LT.1.0) THEN
              WRITE(fnumwrk,'(A44,F6.2)')
     &         ' Temperature limit on grain growth at xstage',xstage
              TLIMIT = TLIMIT+1
            ENDIF
          ENDIF
          
          ! Actual growth
          GROLF = AMIN1(GROLFP,CARBOAT*LFFR)
                  ! From CSCRP
                  ! Leaf weight increase from plant reserves
                  GROLFRS = 0.0
                  IF (GROLF.LT.GROLFP) THEN
                    GROLFRS = AMIN1(RSWT*RSUSE,GROLFP-GROLF)
                    GROLF = GROLF+GROLFRS
                  ENDIF
          IF (LNUMSG.GT.0.AND.GROLFP.GT.0.0) THEN
            AFLFSUM(LNUMSG) = AFLFSUM(LNUMSG)
     &       + AMIN1(1.0,((CARBOAT*LFFR)/GROLFP))            
          ENDIF
          IF (LNUMSG.GT.0.AND.WFLFNUM(LNUMSG).GT.0.0)
     &     AFLF(LNUMSG) = AFLFSUM(LNUMSG)/WFLFNUM(LNUMSG)
          
          GROST = AMIN1(GROSTP,CARBOAT*STFRSTG*(1.0-RSFR))
          
          IF (GROGRP.GT.GRORSP+RSWT) THEN
            GROGRST = 0.0
            IF (GROST.GT.0.0) THEN
              GROGRST = AMIN1(GROST,GROGRP-(GRORSP+RSWT))
              IF (GROGRST.GT.0.0) THEN
                WRITE(fnumwrk,*)'CH2O destined for stem used for grain'
              ENDIF
            ENDIF
            IF (GROGRP.GT.GRORSP+RSWT+GROGRST) THEN
              WRITE(fnumwrk,*)'CH2O limit on grain growth.'
              CH2OLIM = CH2OLIM+1
              if (grnum > 1.e-6) then
              WRITE(fnumwrk,'(A15,F6.2,A5,F6.3,A10)') ' CH2O shortage:',
     &         (GROGRP-(GRORSP+RSWT+GROGRST)),' g/p ',
     &         (GROGRP-(GRORSP+RSWT+GROGRST))/GRNUM,' g/kernel '
              endif
            ENDIF
          ENDIF
          GROGRPA = AMIN1(GROGRP,GRORSP+RSWT+GROGRST)
          
          GRORSSD = -(CARBOASD-AMAX1(.0,CARBOASD-GROLF-GROST-GROGRPA))
          GRORS = AMAX1(-RSWT,
     &     (CARBOAT-CARBOASD-GRORSSD)-GROLF-GROST-GROGRPA)
     

              ! Reserves to ROOT if conc too great (overflow!)
              RTWTGRS = 0.0
              ! Determine potential new concentration
              ! NB. Chaff is simply a part of stem;hence not separate here

!----------------------------------------------------------------
! palderman commit of 2019-07-29 in private repo e5c680514bce4af85d9f463c11a3e5ad522252f8
!              IF (LFWT+GROLF+STWT+GROST.GT.0.0) TVR1 = ! Conc
!     &          (RSWT+GRORS-SENRS)/
!     &          ((LFWT+GROLF-SENLFG-SENLFGRS)
!     &          +(STWT+GROST)+(RSWT+GRORS))

              IF (((LFWT+GROLF-SENLFG-SENLFGRS) ! Prevent divide by zero
     &              +(STWT+GROST)+(RSWT+GRORS)) .GT. 0.0)THEN
                  TVR1 = ! Conc
     &              (RSWT+GRORS-SENRS)/
     &              ((LFWT+GROLF-SENLFG-SENLFGRS)
     &              +(STWT+GROST)+(RSWT+GRORS))
              ELSE
                  TVR1 = 0.0
              END IF
!----------------------------------------------------------------

              IF(TVR1.LT.0.0.AND.TVR1.GT.-1.0E-07) TVR1 = 0.0
              IF (TVR1.GT.RSPCX/100.0) THEN   ! If potential>max        
                TVR2 = RSWT+GRORS-SENRS       ! What rswt could be
                TVR3 =                        ! What rswt should be 
     &           ((RSPCX/100.0)
     &           *(LFWT+GROLF-SENLFG-SENLFGRS
     &           +STWT+GROST))
     &           /(1.0-(RSPCX/100.0))
                RTWTGRS = (TVR2 - TVR3) 
                ! Determine FINAL new concentration
                IF (LFWT+GROLF+STWT+GROST.GT.0.0) TVR5 = 
     &            (RSWT+GRORS-SENRS-RTWTGRS)/
     &            ((LFWT+GROLF-SENLFG-SENLFGRS)
     &            +(STWT+GROST)
     &            +(RSWT+GRORS-SENRS-RTWTGRS))
              ENDIF
     
          GRORSPM = CARBOAPM
          
          IF (PLAGTP(1)+PLAGTP(2).GT.0.0) THEN
            PLAGT(1) = GROLF*(1.0-LSHFR)*LAW*
     &       (PLAGTP(1)/(PLAGTP(1)+PLAGTP(2)))
            PLAGT(2) = GROLF*(1.0-LSHFR)*LAW*
     &       (PLAGTP(2)/(PLAGTP(1)+PLAGTP(2)))
          ENDIF
          DO L = 1,2
           IF(PLAGTP(1)+PLAGTP(2).GT.0.0)
     &       PLAG(L) = PLAG(L)*(PLAGT(1)+PLAGT(2))/(PLAGTP(1)+PLAGTP(2))
          ENDDO
          
          ! Growth adjusted for respiration
          RTWTG = AMAX1(0.0,(CARBOR+RTWTGRS)*(1.0-RRESP)) ! LAH 280211 
         
          ! Leaf senescence
          IF (LNUMSG.GT.0.AND.LNUMSG.GT.LLIFE) THEN
            PLASTMP = AMAX1(0.0,LAP(LNUMSG-LLIFE)*TT/PHINT)
            ! Senesces over 1 PHINT
            ! Senescence cannot be greater than area remaining for
            ! senescing leaf. May have been killed by cold,etc..
            LASENLF = AMAX1(0.0,LAP(LNUMSG-LLIFE)-LAPS(LNUMSG-LLIFE))
            PLASTMP = AMIN1(LASENLF,PLASTMP)
          ELSE
            PLASTMP = 0.0
          ENDIF
          
          PLAS = 0.0
          IF (ISTAGE.EQ.1) THEN
            PLAS = PLASTMP          ! Dependent on leaf longevity
          ELSEIF (ISTAGE.EQ.2) THEN
            PLAS = AMIN1(PLASTMP,   ! Dependent on input coefficient 
     &       PLASF(ISTAGE)*GPLA(ISTAGE-1)*DU/PD(ISTAGE))
          ELSEIF (ISTAGE.EQ.3.OR.ISTAGE.EQ.4.OR.ISTAGE.EQ.5) THEN
            ! Determine if N shortage triggers final senescence
            IF (ISWNIT.NE.'N'.AND.XSTAGE.GT.5.0.AND.LCNF.LT.NFSF) THEN
              XSTAGEFS = XSTAGE
              GPLASENF = AMAX1(0.0,PLA-SENLA)
            ENDIF
            ! Calculate leaf area senesced
            IF (XSTAGE.GT.5.0.AND.
     &        XSTAGEFS.GT.0.0.AND.XSTAGE.LT.LSENS) THEN
              PLAS = GPLASENF*(XSTAGE-XSTAGEFS)/(LSENE-XSTAGEFS)
            ELSE
              IF (XSTAGE.GT.LSENS) THEN
                IF (GPLASENS.LT.0.0) GPLASENS = AMAX1(0.0,PLA-SENLA)
                ! NB. Leaf senescence ends at stage LSENE (6.?)
                PLAS = GPLASENS*(XSTAGE-XSTAGEP)/(LSENE-LSENS)
              ELSE
                PLAS = PLASF(ISTAGE) * GPLA(ISTAGE-1)*DU/PD(ISTAGE)
              ENDIF
            ENDIF
          ELSEIF (ISTAGE.EQ.6) THEN 
            ! Originally senesced over 10 standard days after stage 6
            !PLAS = GPLA(ISTAGE-1)*TT/20.0*0.1
            ! Following is to use LSPHE
            PLAS = GPLASENS*(XSTAGE-XSTAGEP)/(LSENE-LSENS)
          ENDIF
          
          ! Increased senescence if reserves fall too low
          ! NB. 3% of green leaf area senesces ... must check
          IF (RSCD.LT.0.10 .AND. ISTAGE.GE.4. AND. PLA.GT.0.0) THEN
           PLAS = PLAS + AMAX1(0.0,0.03*(PLA-PLAS-SENLA))
           WRITE(fnumwrk,'(A52,I4)')
     &      ' Senescence accelerated because low reserves on day:',doy
          ENDIF
          
          ! Overall check to restrict senescence to what available
          PLAS = AMAX1(0.0,AMIN1(PLAS,PLA-SENLA))
          
          ! Tillering
          IF (GROLFP+GROSTP.GT.0.0) THEN
            TNUMAFAC = AMIN1(1.0,(GROLF+GROST)/(GROLFP+GROSTP))
          ELSE
            TNUMAFAC = 0.0
          ENDIF
          IF (LNUMSD.GE.TI1LF) THEN
            ! LAH SEPT 2009  Maybe introduce TILPE and make variable
            !TILPE = 2.0   ! Had been changed to 3.0
            IF (XSTAGE.LT.TILPE) THEN
              IF (LNUMSD.LT.ti1lf+3) THEN    ! Fibonacci factors
                tnumiff=1.0
              ELSEIF(LNUMSD.GE.ti1lf+3 .AND. LNUMSD.LT.ti1lf+4) THEN
                tnumiff=1.5
              ! PUT = 1.5 TO MATCH ADJUSTMENT IN CSCRP  
              ELSEIF(LNUMSD.GE.ti1lf+4 .AND. LNUMSD.LT.ti1lf+5) THEN
                tnumiff=1.5  ! 3.0
              ELSEIF(LNUMSD.GE.ti1lf+5 .AND. LNUMSD.LT.ti1lf+6) THEN
                tnumiff=1.5  ! 4.0
              ELSEIF(LNUMSD.GE.ti1lf+6 .AND. LNUMSD.LT.ti1lf+7) THEN
                tnumiff=1.5  ! 6.0
              ENDIF
              ! CHANGED BACK TO USING AMIN1 FUNCTION TO MATCH CSCRP
              TNUMG = TT/PHINT * TNUMIFF * AMIN1(WFT,NFT,LIF1)
              !TNUMG = TT/PHINT * TNUMIFF * WFT*NFT*LIF1
              ! TAKEN OUT TO MATCH CSCRP
              ! IF (LNUMSD.GT.TI1LF+3) TNUMG = TNUMG * TNUMAFAC
            ENDIF
          ELSE
            TNUMG = 0.0
          ENDIF
          
          ! TIFAC = Tillering rate factor (multiplier) 
          TNUMG = TNUMG * TIFAC
          
          ! Tiller death
          RTSW = 1.0
          TILWT = 0.0
          TILSW = G3 * CUMDU/PTH(5)
          IF (TNUM.GT.0.0) TILWT = (LFWT+STWT+RSWT+GRWT)/TNUM
          IF (TILSW.GT.0.0) RTSW = TILWT/TILSW
          !TILDS = 2.0     ! Ceres 3.5 = 2.4
          !TILDE = 4.0     ! Ceres 3.5 = 5.8
          IF (XSTAGE.GE.TILDS .AND. XSTAGE.LT.TILDE)
     &     TNUMD = 
     &       AMAX1(0.0,(TNUM-1.0)*(1.0-RTSW)*(TT/20.0)*(TILDF/100.0))
             ! 20.0 is to change TT to standard days          
          ! Root respiration
          RTRESP = (CARBOR+RTWTGRS) * RRESP
          
          XSTAGEP = XSTAGE

          ! Canopy height                                               
          IF (XSTAGE.LT.7) THEN
            IF (XSTAGE.GT.1.0) THEN
              CANHTG =
     &         AMAX1(0.0,(CANHTS*AMIN1(1.0,(XSTAGE-1.0)/4.0)-CANHT))
            ELSEIF (XSTAGE.EQ.1.0 .AND. PLAGT(1).GT.0.0) THEN
              ! Height growth on day of emergence or if no development
              CANHTG = 0.5
            ENDIF
          ENDIF
          
          ! Root growth initializations
          RTWTGS = 0.0
          DO L = 1, NLAYR
            RTWTGL(L) = 0.0
            RTWTSL(L) = 0.0
            RTNSL(L) = 0.0
          ENDDO
      
          ! Root growth calculations
          IF (ISTAGE.LT.6 .OR. ISTAGE.GE.8) THEN
         
            ! Establish root tip layer
            CUMDEP = 0.0
            L = 0
            DO WHILE ((RTDEP.GE.CUMDEP) .AND. (L.LT.NLAYR))
              L = L + 1
              CUMDEP = CUMDEP + DLAYR(L)
            END DO
          
            ! Water factor for root depth growth
            IF (ISWWAT.NE.'N') THEN
              WFRG = AMAX1(0.1,AMIN1(1.0,
     &         ((SW(L)-LL(L))/(DUL(L)-LL(L)))/WFRGU))
              IF (L.EQ.1) THEN
                ! Layer 2 SWDF used if less stress, because
                ! elongation most likely at base of layer
                TVR1 = AMAX1(0.1,AMIN1(1.0,
     &          ((SW(2)-LL(2))/(DUL(2)-LL(2)))/WFRGU))
                IF (TVR1.GT.WFRG) WFRG = TVR1
              ENDIF
            ELSE  
              WFRG = 1.0
            ENDIF
          
            ! Root depth growth
            RTDEPG = 0.0                                                
            IF (ISTAGE.LT.7) THEN                                      
              IF (RTWTG.GT.0.0) THEN                                   
                IF (CUMTT.LT.RDGTH) THEN
                  RTDEPG = (TT/STDAY)*RDGS1                           
                ELSE                                                   
                  RTDEPG = (TT/STDAY)*RDGS2                           
                ENDIF                     
                ! Add water content and hospitality factors 
                IF (ISWWAT.NE.'N') 
     &            RTDEPG = RTDEPG * SQRT(AMAX1(0.3,SHF(L))) * WFRG
              ELSE                                                     
                ! This is to handle planting on the surface and to     
                ! allow root depth growth immediately after emergence  
                IF (DAE.LT.20) RTDEPG = (TT/STDAY)*RDGS1               
              ENDIF                                                    
            ELSEIF (ISTAGE.GE.9) THEN      ! Germination to emergence  
              RTDEPG = (TT/STDAY)*RDGS1                                
            ENDIF                                                      
          
            ! Root dry matter from seed if not enough assimilate
            ! Root length/depth growth ratio (120) derived from Ceres3.5
            ! Used in pre-emergence stage to calculate depth growth 
            ! and then need to calculate total root growth
            IF (RTWTG.LT.RLDGR*RTDEPG/((RLWR*1.0E4)*PLTPOP)) THEN
              RTWTGS = AMAX1(0.0,AMIN1(SDAFR*SEEDRSAV,
     &        (RLDGR*RTDEPG/((RLWR*1.0E4)*PLTPOP) - RTWTG)))
              IF (RTWTGS < 1.E-10) RTWTGS = 0.0
              SEEDRSAV = SEEDRSAV - RTWTGS
            ENDIF
                                                                       
            ! Root dry matter distribution with depth
            L = 0                                                      
            CUMDEP = 0.0
            RTDEPTMP = RTDEP+RTDEPG
            DO WHILE ((CUMDEP.LE.RTDEPTMP) .AND. (L.LT.NLAYR))
              L = L + 1
              CUMDEP = CUMDEP + DLAYR(L)
              SWDF = AMAX1(0.1,AMIN1(1.0,
     &         ((SW(L)-LL(L))/(DUL(L)-LL(L)))/WFRG))
              ! Original CERES
              !F (ISWNIT.EQ.'Y') THEN
              !  RNFAC = AMAX1(0.01,
              !       (1.0-(1.17*EXP(NFRG*(NO3LEFT(L)+NH4LEFT(L))))))
              !ELSE
              !  RNFAC = 1.0
              !ENDIF
              ! Below from Cropsim
                IF (ISWNIT.NE.'N'.AND.NCRG.GT.0.0) THEN
                  NFRG = AMIN1(1.0,
     &             AMAX1(0.1,(NO3LEFT(L)+NH4LEFT(L))/NCRG))
                ELSE
                  NFRG = 1.0
                ENDIF 
              ! LAH HAWAII 2007   N factor taken out.
              ! LAH GUELPH 2010   Replaced with Cropsim function.
              ! RLDF(L) = AMIN1(SWDF,RNFAC)*SHF(L)*DLAYR(L)
              ! RLDF(L) = SWDF*SHF(L)*DLAYR(L)
              ! Below from CROPSIM
              RLDF(L) = AMIN1(WFRG,NFRG)*SHF(L)*DLAYR(L)
            END DO
            
            ! Distribution factor adjusted for root tip layer
            IF (L.GT.0) RLDF(L) = 
     &        RLDF(L)*(1.0-(CUMDEP-RTDEPTMP)/DLAYR(L)) 
            L1 = L
            
            TRLDF = 0.0
            DO  L = 1, L1
              TRLDF = TRLDF + RLDF(L)
            END DO
            RTWTGL = 0.0 ! LAH Feb 28 2011
            IF (TRLDF.GT.1.E-8 .AND. (RTWTG+RTWTGS)> 1.E-10) THEN
              DO  L = 1, L1
                RTWTGL(L) = (RLDF(L)/TRLDF)*(RTWTG+RTWTGS)
              END DO
            ENDIF
                   
            ! Root senescence   
            RTWTS = 0.0
            RTWTSL = 0.0 
            RTNSL = 0.0 
            DO L = 1, L1
              RTWTSL(L) = RTWTL(L)*RSEN/100.0*TT/STDAY ! NB. Temp effect
              RTWTS = RTWTS + RTWTL(L)*RSEN/100.0*TT/STDAY
              RTNSL(L) = AMIN1(RTWTSL(L)*RMNC,RTWTSL(L)*RANC)
            ENDDO

          ENDIF
          
          ! Devernalization
          VDLOST = 0.0                                                  
          IF (CUMVD.LT.10.0 .AND. TMAX.GT.30.0) THEN                    
            VDLOST = 0.5*(TMAX-30.0)
          ENDIF
          
          ! Loss of cold hardiness
          IF (ISTAGE.GE.9 .OR. ISTAGE.LT.7) THEN
            IF (ISTAGE.GE.2) THEN
              HARDILOS = AMAX1(0.0,(TMAX-10.0)*0.1)
            ELSE
              HARDILOS = AMAX1(0.0,(TMAX-10.0)*0.01)
            ENDIF
          ENDIF
          
          IF (ISTAGE.LE.7.0) THEN
            TNUMLOSS = 0.0
            PLTLOSS = 0.0
            PLASC = 0.0
            CKCOLD = 0.0
            ! Leaf senescence
            IF ((TMIN+TMAX)/2.0.LT.TKLF) THEN
              CKCOLD = (TKLF-(TMIN+TMAX)/2.0)*0.20
              ! 20% loss for every degree below Tklf
            ENDIF
            ! Following is original (as CK)
            ! CKCOLD = AMAX1(0.00,AMIN1(0.96,
            ! (0.020*HARDI-0.10)*(TMIN*0.85+TMAX*0.15+10.0+.25*SNOW)))
            PLASC = AMAX1(0.0,
     &            AMIN1(CKCOLD*(PLA-SENLA),((PLA-SENLA)-TNUM*0.035)))
            IF (PLASC.GT.0.0.AND.(PLA-SENLA).GT.0.0) 
     &       WRITE(fnumwrk,'(A30,I4,A14,F4.1)')
     &       ' Leaves damaged by cold on day',doy,
     &       ' Fraction lost',plasc/(pla-senla)
          
            ! Tiller and plant death
            IF (TKILL.GT.(TMIN+TMAX)/2.0) THEN
              IF (TNUM.GE.1.0) THEN
                TNUMLOSS=TNUM *
     &           (1.0-(0.9-0.02*ABS(((TMIN+TMAX)/2.0-TKILL))**2))
              ENDIF
              IF (TNUM-TNUMLOSS.GE.1.0) THEN
                WRITE (FNUMWRK,900)
     &           DOY,TKILL,(TMIN+TMAX)/2.0,HARDI,TNUM,PLTPOP
 900            FORMAT (' Crop was damaged by cold on day',I4,/,
     &            ' TKILL =',F5.1,5X,'TMEAN=',F5.1,5X,
     &            'HARDI=',F5.2,5X,'TNUM =',  F7.2,5X,'PLTPOP=',F4.0)
              ELSE
                PLTLOSS =
     &           PLTPOP*(1.0-(0.95-0.02*((TMIN+TMAX)/2.0-TKILL)**2))
                 IF (ISTAGE.GE.4) PLTLOSS = 0.0
                IF (PLTPOP-PLTLOSS.GE.0.05*PLTPOPP) THEN
                  WRITE (FNUMWRK,900) DOY,TKILL,
     &             (TMIN+TMAX)/2.0,HARDI,TNUM,PLTPOP
                ELSE
                  CFLFAIL = 'Y'
                  PLTLOSS = AMIN1(PLTPOP,PLTLOSS)
                  IF (ISTAGE.GE.4) PLTLOSS = 0.0
                  WRITE (FNUMWRK,1100) DOY,TKILL,(TMIN+TMAX)/2.0,HARDI
 1100             FORMAT (' At least 95% killed by cold on day',I4,/,
     &            ' TKILL =',F5.1,5X,'TMEAN =',F5.1,5X,
     &            'HARDII =',F5.2)
                ENDIF
              ENDIF
            ENDIF
          ENDIF

          ! Senescent leaf - NB. N loss has a big effect if low N
          LSENNF = 0.0
          IF (LANC.GT.0.0) LSENNF = (1.0-((LANC-LMNC)/LANC))
          PLASS = 0.0
          PLAST = 0.0
          SENLFG = 0.0
          SENLFGRS = 0.0
          SENRS = 0.0
          SENNLFG = 0.0
          SENNLFGRS = 0.0
          ! Following is to lose some leaf area when tillers die
          IF (TNUMD.GT.0.0) PLAST = TNUMD * ((PLA-SENLA)/TNUM) * LALOSSF
          IF (ISWWAT.NE.'N') THEN
            IF (PLA-SENLA.GT.0.0.AND.WUPR.LT.WFSU) THEN
              PLASS = AMAX1(0.0,AMIN1((PLA-SENLA)-PLAS,
     &            (PLA-SENLA)*LLOSW))
            ENDIF
          ENDIF
          IF (ISWNIT.NE.'N') THEN
            ! Low N accelerated senescence
            IF (PLA-SENLA.GT.0.0.AND.LANC.LT.LCNCSEN) THEN
              PLASS = PLASS +  AMAX1(0.0,AMIN1((PLA-SENLA)-PLAS,
     &            (PLA-SENLA)*LLOSN))
            ENDIF
          ENDIF
          IF (PLA-SENLA.GT.0.0) THEN
            IF (PLASC.GT.0.0) THEN
              ! If cold kill
              SENLFG = AMIN1(LFWT,LFWT*PLASC/(PLA-SENLA))
              SENRS = AMIN1(RSWT,RSWT*PLASC/(PLA-SENLA))
            ELSE
              ! If normal senescence
              SENLFG = AMIN1(LFWT*LWLOS,(AMAX1(0.0,         
     &         (LFWT*((PLAS+PLASS)/(PLA-SENLA))*LWLOS))))
              SENLFGRS = AMIN1(LFWT*(1.0-LWLOS),(AMAX1(0.0,         
     &        (LFWT*((PLAS+PLASS)/(PLA-SENLA))*(1.0-LWLOS)))))
            ENDIF
            SENNLFG = AMIN1((LEAFN-GRAINNGL)*LSENNF,
     &                (SENLFG+SENLFGRS)*LANC*LSENNF)
            SENNLFGRS = AMIN1((LEAFN-GRAINNGL)*(1.0-LSENNF),
     &                  (SENLFG+SENLFGRS)*LANC*(1.0-LSENNF))
            IF (((SENNLFG+SENNLFGRS)-LEAFN).GT.1.0E-8) THEN
              WRITE(fnumwrk,'(A40,F6.2)')
     &         ' Adjusted N removal from leaves at stage',xstage
              SENNLFGRS = LEAFN-SENNLFG
              IF (SENNLFGRS.LT.0.0) THEN
                SENNLFG = SENNLFG - ABS(SENNLFGRS)
                SENNLFGRS = 0.0
                IF (SENNLFG.LT.0.0) SENNLFG = 0.0
              ENDIF
            ENDIF
          ENDIF

          ! Senescent stem
          SENSTG = 0.0
          SENNSTG = 0.0
          SENNSTGRS = 0.0
          SSENF = 0.0
          IF (XSTAGE.GT.SSSTG .AND. XSTAGE.LT.6.0) THEN
            SENSTG = AMAX1(0.0,STWT*(SSEN/100.0)*(TT/STDAY))
            IF (SANC.GT.0.0) SSENF = (1.0-((SANC-SMNC)/SANC))
            SENNSTG = SENSTG*SANC*SSENF
            SENNSTGRS = SENSTG*SANC*(1.0-SSENF)
            IF (SENNSTG+SENNSTGRS.GT.STEMN) THEN
              WRITE(fnumwrk,*)'N removal from stem > stem N'
              SENNSTG = STEMN-SENNSTGRS
            ENDIF
          ENDIF

          ! N Uptake
          IF (ISWNIT.EQ.'Y') THEN                                       
      
            ! N uptake initializations                        
            DTOPN = 0.0
            DROOTN = 0.0
            DSTEMN = 0.0
            DLEAFN = 0.0
            DSTOVN = 0.0
            GRAINNG = 0.0
            NUPD = 0.0
            NUAG = 0.0
            NUF = 0.0
            SEEDNR = 0.0
            SEEDNRA = 0.0
            SEEDNRB = 0.0
            SEEDNT = 0.0
            SEEDNTA = 0.0
            SEEDNTB = 0.0
            RSNUSET = 0.0
            RSNUSER = 0.0
            RSNV = 0.0
            ANDEM = 0.0
            NDEMSOIL = 0.0
            RNDEM = 0.0
            RNDEMSOIL = 0.0
            RNDEM1 = 0.0
            TNDEM = 0.0
            TNDEMSOIL = 0.0
            TRLV = 0.0
            DROOTNA = 0.0
            GRAINNGR = 0.0
            GRAINNGV = 0.0
            NPOOLL = 0.0
            NPOOLST = 0.0
            NPOOLV = 0.0
              
            SNO3PROFILE = 0.0
            SNH4PROFILE = 0.0
            SNO3ROOTZONE = 0.0
            SNH4ROOTZONE = 0.0
            DO L = 1, NLAYR
              UNO3ALG(L) = 0.0
              UNH4ALG(L) = 0.0
              RNO3U(L) = 0.0
              RNH4U(L) = 0.0
              TRLV = TRLV + RLV(L)
              FAC(L) = 10.0/(BD(L)*DLAYR(L))
              SNO3(L) = NO3LEFT(L) / FAC(L)
              SNH4(L) = NH4LEFT(L) / FAC(L)
              SNO3PROFILE = SNO3PROFILE + SNO3(L)
              SNH4PROFILE = SNH4PROFILE + SNH4(L)
              IF (RLV(L).GT.0.0) THEN
                SNO3ROOTZONE = SNO3ROOTZONE + SNO3(L)
                SNH4ROOTZONE = SNH4ROOTZONE + SNH4(L)
              ENDIF
            END DO
          
            ! Grain N uptake.
            NSINK = 0.0
            IF (GRNUM.GT.0.0 .AND. XSTAGE.LT.6.0) THEN
              NSINK = AMIN1(GROGRPA*(GRNMX/100.0),
     &         LAGSTAGE*TFGN*GRNUM*G2*DU*.001*(GRNS/100.))
            ENDIF
          
            ! N uptake factor after maturity
            IF (XSTAGE.GT.6.0 .AND. XSTAGE.LT.7.0) THEN
              NUFACM = AMAX1(0.0,1.0 - (XSTAGE-6.0)/(6.5-6.0))
            ELSE
              NUFACM = 1.0
            ENDIF
            
            LNDEM = NUFACM *
     &              ((LFWT-SENLFG-SENLFGRS)*AMAX1(0.0,NTUPF*(LCNC-LANC))
     &              + GROLF*LCNC)
            SNDEM = NUFACM *
     &              ((STWT-SENSTG)*AMAX1(0.0,NTUPF*(SCNC-SANC))
     &              + AMAX1(0.0,GROST)*SCNC)
            RNDEM = NUFACM *
     &              ((RTWT-RTWTS) * AMAX1(0.0,NTUPF*(RCNC-RANC))
     &              + (RTWTG+RTWTGS) * RCNC)
     
            ! Reserve N use
            RSNUSEG = 0.0
            RSNUSET = 0.0
            RSNUSER = 0.0
            IF (NSINK.GT.0.0) RSNUSEG = AMAX1(0.0,AMIN1(NSINK,RSN))
            RSNV = RSN - RSNUSEG
            IF (LNDEM+SNDEM+RNDEM.GT.0.0 .AND. RSNV.GT.0.0) THEN
              IF (LNDEM+SNDEM+RNDEM.LT.RSNV) THEN
                RSNUSET = LNDEM + SNDEM
                RSNUSER = RNDEM
                TNDEM = 0.0
                RNDEM = 0.0
              ELSE
                RSNUSET = RSNV * (LNDEM+SNDEM)/(LNDEM+SNDEM+RNDEM)
                RSNUSER = RSNV * RNDEM/(LNDEM+SNDEM+RNDEM)
                TNDEM = (LNDEM+SNDEM) - RSNUSET
                RNDEM = RNDEM - RSNUSER
              ENDIF
              IF (RSNUSET.LT.1E-20) RSNUSET = 0.0
              IF (RSNUSER.LT.1E-20) RSNUSER = 0.0
            ELSE
              TNDEM = LNDEM + SNDEM
              RNDEM = RNDEM
            ENDIF
          
            ! Seed N use (Basic)
            ! Need a little seed use regardless of deficit after uptake
            SEEDNTB = 0.0
            SEEDNRB = 0.0
            IF (XSTAGE.LT.4 .OR. XSTAGE.GT.6) THEN
              ! Originaal: 
              !IF (SEEDN.GT.1.0E-6) THEN
              !  SEEDNTB = AMIN1(TNDEM,SEEDN*0.020)  
              !  SEEDNRB = AMIN1(RNDEM,AMAX1(0.0,SEEDN*0.025-SEEDNTB))
              !ENDIF
              ! To match Cropsim  
              IF (RTWT.LE.0.0) THEN
                SEEDNTB = AMIN1(TNDEM,SEEDN*0.2)  
                SEEDNRB = AMIN1(RNDEM,SEEDN*0.2)
              ELSE
                ! Some use of seed (0.2 need) even if may not be needed
                SEEDNTB = AMAX1(0.0,AMIN1(0.2*TNDEM,SEEDN*0.2))
                SEEDNRB = AMAX1(0.0,AMIN1(0.2*RNDEM,SEEDN*0.2))
              ENDIF
            ENDIF
          
            IF ((NSINK-RSNUSEG).GT.0.0) THEN    
              TNDEMSOIL = TNDEM - SEEDNTB + (NSINK-RSNUSEG)
            ELSE
              TNDEMSOIL = TNDEM - SEEDNTB
            ENDIF  
            RNDEMSOIL = RNDEM - SEEDNRB
            
            NDEMSOIL = TNDEMSOIL + RNDEMSOIL
            ANDEM = NDEMSOIL * PLTPOP*10.0
            TRNU = 0.0
            RNO3U(L) = 0.0
            RNH4U(L) = 0.0
            ! Calculate potential N uptake in soil layers with roots
            DO L=1,NLAYR
              IF (RLV(L) .GT. 1.E-6) THEN
                FNH4 = 1.0 - EXP(-0.08 * NH4LEFT(L))
                FNO3 = 1.0 - EXP(-0.08 * NO3LEFT(L))
                ! The following limits are those in NUPTAK
                IF (FNO3 .LT. 0.04) FNO3 = 0.0  
                IF (FNO3 .GT. 1.0)  FNO3 = 1.0
                IF (FNH4 .LT. 0.04) FNH4 = 0.0  
                IF (FNH4 .GT. 1.0)  FNH4 = 1.0
                SMDFR = (SW(L) - LL(L)) / (DUL(L) - LL(L))
                IF (SMDFR .LT. 0.0) SMDFR = 0.0
                IF (SW(L) .GT. DUL(L)) THEN
                  SMDFR = 1.0 - (SW(L) - DUL(L)) / (SAT(L) - DUL(L))
                  ! Wet soil effect not implemented
                  SMDFR = 1.0
                ENDIF
                RFAC = RLV(L) * SMDFR * SMDFR * DLAYR(L) * 100.0
                !  RLV = Rootlength density (cm/cm3);SMDFR = relative drought factor
                !  RTNO3 + RTNH4 = Nitrogen uptake / root length (mg N/cm)  
                !  RNO3U + RNH4  = Nitrogen uptake (kg N/ha)
                RNO3U(L) = RFAC * FNO3 * RTNO3
                RNH4U(L) = RFAC * FNH4 * RTNH4
                RNO3U(L) = MAX(0.0,RNO3U(L))
                RNH4U(L) = MAX(0.0,RNH4U(L))
                TRNU = TRNU + RNO3U(L) + RNH4U(L) !kg[N]/ha
              ENDIF
            ENDDO
 
            ! Ratio (NUPR) to indicate N supply for output
            IF (ANDEM.GT.0) THEN
              NUPR = TRNU/ANDEM
            ELSE
              IF (TRNU.LE.0) THEN
                NUPR = 0.0
              ELSE
                NUPR = 10.0
              ENDIF
            ENDIF
            ! Factor (NUF) to reduce N uptake to level of demand
            IF (TRNU.GT.0.0) NUF = AMIN1(1.0,ANDEM/TRNU)
            ! Actual N uptake by layer roots based on demand (kg/ha)
            DO L = 1, MIN(L1,NLAYR)
              UNO3ALG(L) = RNO3U(L)*NUF
              XMIN = NO3MN/FAC(L) ! Original XMIN = 0.25/FAC(L)
              UNO3ALG(L) = MAX(0.0,MIN (UNO3ALG(L),SNO3(L) - XMIN))
              UNH4ALG(L) = RNH4U(L)*NUF
              IF (FAC(L).GT.0.0) THEN
                XMIN = NH4MN/FAC(L) ! Original XMIN = 0.5/FAC(L)
              ELSE
                XMIN = 0.0
              ENDIF  
              UNH4ALG(L) = MAX(0.0,MIN (UNH4ALG(L),SNH4(L) - XMIN))
              NUAG = NUAG + UNO3ALG(L) + UNH4ALG(L)
            END DO
          
            NUPD = NUAG/(PLTPOP*10.0)
          
            ! Change in above and below ground N
            IF (NDEMSOIL.LE.0.0 .OR. NUPD.LE.0.0) THEN
              DTOPN = 0.0 + RSNUSET
              DROOTN = 0.0 + RSNUSER
            ELSE
              DTOPN = (TNDEMSOIL/NDEMSOIL)*NUPD + RSNUSET
              DROOTN = (RNDEMSOIL/NDEMSOIL)*NUPD + RSNUSER
            ENDIF
          
            ! Make sure that roots do not fall below minimum
            IF ((RTWTG+RTWTGS)*RMNC.GT.DROOTN) THEN
              DROOTNA = AMIN1(DTOPN,((RTWTG+RTWTGS)*RMNC)-DROOTN)
              DROOTN = DROOTN + DROOTNA
              DTOPN = DTOPN - DROOTNA
            ENDIF
          
            ! Use N allotted to tops and roots for grain if filling     
            IF ((NSINK-RSNUSEG).GT.0.0) THEN
              IF ((NSINK-RSNUSEG).GE.DTOPN+DROOTN) THEN
                GRAINNG = DTOPN + DROOTN
                DSTOVN = 0.0
                DROOTN = 0.0
              ELSE
                GRAINNG = (NSINK-RSNUSEG)
                TVR1 = DTOPN+DROOTN-(NSINK-RSNUSEG)
                IF (TNDEM+RNDEM > 1.E-6) THEN
                DSTOVN = TNDEM/(TNDEM+RNDEM) * TVR1
                DROOTN = RNDEM/(TNDEM+RNDEM) * TVR1
                ELSE
                  DSTOVN = 0.0
                  DROOTN = 0.0
                ENDIF
              ENDIF
            ELSE
              DSTOVN = DTOPN
            ENDIF
          
            ! Use additional seed if not sufficient so far              
            IF (XSTAGE.LT.4 .OR. XSTAGE.GT.6) THEN
              SEEDNTA = AMAX1(0.0,
     &         AMIN1(SEEDN-SEEDNTB-SEEDNRB,TNDEMSOIL-DSTOVN))
              SEEDNRA = AMAX1 (0.0,
     &         AMIN1(SEEDN-SEEDNTB-SEEDNTA-SEEDNRB,RNDEMSOIL-DROOTN))
              SEEDNT = SEEDNTB + SEEDNTA
              SEEDNR = SEEDNRB + SEEDNRA
            ENDIF
          
            ! Move N to grain from roots and tops if necessary
            IF((NSINK-RSNUSEG).GT.0..AND.(NSINK-RSNUSEG).GT.GRAINNG)THEN
              NPOOLR = AMAX1 (0.0,(XNFS/100.0)*(RTWT*(RANC-RMNC)))
              IF (NPOOLR.GT.(NSINK-RSNUSEG)-GRAINNG) THEN
                GRAINNGR = (NSINK-RSNUSEG)-GRAINNG
              ELSE
                GRAINNGR = NPOOLR
              ENDIF
              IF ((NSINK-RSNUSEG).GT.GRAINNG+GRAINNGR) THEN
                ! N use limit 
                IF (XSTAGE.GE.5.0.AND.XSTAGE.LE.6.0) THEN
                  NUSELIM = AMIN1(1.0,DU/((6.0-XSTAGE)*PD(5)))
                ELSEIF (XSTAGE.GE.4.0.AND.XSTAGE.LE.5.0) THEN
                  NUSELIM = DU/(((5.0-XSTAGE)*PD(4))+PD(5))
                ENDIF
                NUSEFAC = AMAX1(NUSELIM,(XNFS/100.0))
                NPOOLL = AMAX1 (0.0,
     &           NUSEFAC*((LFWT-SENLFG-SENLFGRS)*(LANC-LMNC)))
                NPOOLST = AMAX1 (0.0,
     &           NUSEFAC*((STWT-SENSTG)*(SANC-SMNC)))
                NPOOLV = NPOOLL + NPOOLST
                IF (NPOOLV.GT.(NSINK-RSNUSEG)-(GRAINNG+GRAINNGR)) THEN
                  GRAINNGV = (NSINK-RSNUSEG)-GRAINNG-GRAINNGR
                ELSE
                  GRAINNGV = NPOOLV
                ENDIF
              ENDIF
            ENDIF
          
            ! Split tops aspects into leaf and stem aspects
            IF (LNDEM+SNDEM.GT.0.0) THEN
              DLEAFN = DSTOVN * LNDEM / (LNDEM+SNDEM)
              DSTEMN = DSTOVN * SNDEM / (LNDEM+SNDEM)
            ELSE
              DLEAFN = 0.0
              DSTEMN = 0.0
            ENDIF
            IF (NPOOLL+NPOOLST.GT.0.0) THEN
              GRAINNGL = GRAINNGV * NPOOLL / (NPOOLL+NPOOLST)
              GRAINNGS = GRAINNGV * NPOOLST / (NPOOLL+NPOOLST)
            ELSE
              GRAINNGL = 0.0
              GRAINNGS = 0.0
            ENDIF
          
          ENDIF

          ! Minimum grain N control
          GRORSGR = 0.0
          IF (ISWNIT.NE.'N'.AND.GRNMN.GT.0.0) THEN
            GROGRPN =
     &       (GRAINNG+GRAINNGR+GRAINNGL+GRAINNGS+RSNUSEG)*(100./GRNMN)
            IF (GROGRPN.LT.GROGRPA.AND.GRAINANC*100.0.LE.GRNMN) THEN
              GRORSGR = GROGRPA - GROGRPN
              GROGR = GROGRPN
            ELSE
              GROGR = GROGRPA
              GRWTTMP = GRWT + GROGRPA
              IF (GRWTTMP.GT.0.0) THEN
                GRAINNTMP = GRAINN +
     &          (GRAINNG+GRAINNGR+GRAINNGL+GRAINNGS+RSNUSEG)
                IF (GRAINNTMP/GRWTTMP*100.0 .LT. GRNMN) THEN
                  GRWTTMP = GRAINNTMP*(100.0/GRNMN)
                  GROGR = GRWTTMP - GRWT
                  IF (GROGR.LT.GROGRPA) GRORSGR = GROGRPA - GROGR
                ENDIF
              ENDIF
            ENDIF
            IF (GROGR.LT.GROGRPA) THEN
              WRITE(fnumwrk,'(A42,F4.2)')
     &         ' N limit on grain growth. N at minimum of ',grnmn
              NLIMIT = NLIMIT + 1
            ENDIF
          ELSE
            GROGR = GROGRPA
          ENDIF

          ! Maximum grain weight control
          GROGRADJ = 0.0
          IF (GRNUM.GT.0.0) THEN
            IF ((GRWT+GROGR)/GRNUM - G2KWT/1000.0 > 1.E-5) THEN
              WRITE(fnumwrk,*)'Maximum kernel wt reached on:',YEARDOY
              GROGRADJ = GROGR - (G2KWT/1000.0-(GRWT/GRNUM))*GRNUM
            ENDIF
          ENDIF

          ! Growth variables expressed on area basis
          ! C assimilation
          CARBOA = CARBO*PLTPOP*10.0
          ! N assimilation
          !NUAG = NUPD*PLTPOP*10.0
          ! Above ground senescence
          SENWALG(0) = 0.0
          SENNALG(0) = 0.0
          SENCALG(0) = 0.0
          SENLGALG(0) = 0.0
          IF (XSTAGE.LT.LRETS) THEN
            SENWALG(0) = (SENLFG+SENSTG) * PLTPOP*10.0
            SENNALG(0) = (SENNLFG+SENNSTG) * PLTPOP*10.0
            SENCALG(0) = (SENLFG+SENSTG) * 0.4 * PLTPOP*10.0
            SENLGALG(0) =
     &       (SENLFG*LLIGP/100+SENSTG*SLIGP/100) * PLTPOP*10.0
          ENDIF
          ! Root senescence
          DO L = 1, NLAYR
            SENWALG(L) = RTWTSL(L) * PLTPOP*10.0
            SENNALG(L) = RTNSL(L) * PLTPOP*10.0
            SENCALG(L) = SENWALG(L) * 0.4
            SENLGALG(L) = SENWALG(L) * RLIGP/100.0
          ENDDO

          DYNAMICI = 0

        ENDIF


      ELSEIF (DYNAMIC.EQ.INTEGR) THEN

        IF (YEARDOY.GE.YEARPLT) THEN

          ! Dry weights
          ! LAH No growth/development on planting day
          IF (YEARDOY.GT.YEARPLT) THEN   
            SENRSC = SENRSC + SENRS
            CARBOC = CARBOC + CARBO
            RESPC = RESPC + RTRESP
            LFWT = LFWT + GROLF - SENLFG - SENLFGRS
            IF (LFWT.LT.1.0E-12) THEN
              IF (LFWT.LT.0.0) 
     &          WRITE(fnumwrk,*)'Leaf weight less than 0! ',LFWT
              LFWT = 0.0
            ENDIF
            STWT = STWT + GROST - SENSTG - GROGRST
            IF (STWT.LT.1.0E-06) THEN
              IF (STWT.LT.0.0) 
     &         WRITE(fnumwrk,*)'Stem weight less than 0! ',STWT
              STWT = 0.0
            ENDIF
            GRWT = GRWT + GROGR - GROGRADJ
            RSWT = RSWT + GRORS + GRORSGR + GROGRADJ + SENLFGRS - 
     &             SENRS - RTWTGRS
            
            ! Reserves distribution 
            ! Max concentration in leaves increases through life cycle.
            LLRSWT = AMIN1(RSWT,
     &       LFWT*(1.0-LSHFR)*RSCLX*CUMDU/(Pd(1)+pd(2)+pd(3)+pd(4)))
            LSHRSWT = AMIN1(RSWT-LLRSWT,
     &       LFWT*LSHFR*RSCLX*CUMDU/(Pd(1)+pd(2)+pd(3)+pd(4)))
            IF (STWT.GT.0.0) THEN
!-GH        IF (STWT+CHWT.GT.0.0) THEN
              STRSWT = (RSWT-LLRSWT-LSHRSWT)*(STWT-CHWT)/STWT
              CHRSWT = (RSWT-LLRSWT-LSHRSWT)*CHWT/STWT
            ELSE
              STRSWT = (RSWT-LLRSWT-LSHRSWT)
              CHRSWT = 0.0
            ENDIF
            IF (XSTAGE.GE.LRETS) THEN
              DEADWT = DEADWT + SENLFG + SENSTG
            ELSE
              SENWL(0) = SENWL(0) + (SENLFG+SENSTG)
              SENCL(0) = SENCL(0) + (SENLFG+SENSTG) * 0.4
              SENLGL(0) = SENLGL(0)+(SENLFG*LLIGP/100+SENSTG*SLIGP/100)
            ENDIF
            RTWT = 0.0
            DO L = 1, NLAYR
              RTWTL(L) = RTWTL(L) + RTWTGL(L) - RTWTSL(L)
              SENWL(L) = SENWL(L) + RTWTSL(L)
              SENCL(L) = SENCL(L) + RTWTSL(L) * 0.4
              SENLGL(L) = SENLGL(L) + RTWTSL(L) * RLIGP/100.0
              ! Totals
              RTWT = RTWT + RTWTL(L)
              SENWS = SENWS + RTWTSL(L)
              SENCS = SENCS + RTWTSL(L) * 0.4
              SENLGS = SENLGS + RTWTSL(L) * RLIGP/100.0
            END DO
          ENDIF 
          SEEDRS = AMAX1(0.0,SEEDRS+GRORSSD-RTWTGS)
          SEEDRSAV = SEEDRS

          IF (ISTAGE.GE.6) RSWTPM = RSWTPM + GRORSPM

          ! Chaff. Calculated for output. 
          ! STWT includes chaff, but STWAD excludes chaff.
          IF (XSTAGE.GT.CHSTG) THEN
            IF (GROST.GT.0.0) CHWT = CHWT + GROST*CHFR
          ENDIF

          IF (GRNUM.GT.0.0) THEN
            GWUD = GRWT/GRNUM
          ELSE
            GWUD = 0.0
          ENDIF
            
          GWGD = GWUD*1000.0

          HIAD = 0.0
          SHRTD = 0.0
          IF ((LFWT+STWT+GRWT+RSWT+DEADWT).GT.0.0)
     &     HIAD = GRWT/(LFWT+STWT+GRWT+RSWT+DEADWT)
          IF (RTWT.GT.0.0)
     &     SHRTD = (LFWT+STWT+GRWT+RSWT+DEADWT) / RTWT

          ! Reserve concentration and factor
          RSCD = 0.0
          IF (LFWT+STWT.GT.0.0)
     &     RSCD = RSWT/(LFWT+STWT+RSWT)

          ! Radiation use efficiency
          PARUED = 0.0
          PARADCUM = PARADCUM + PARAD
          IF (PARAD*PARI.GT.0.0) THEN
            PARUED = CARBO*PLTPOP/(PARAD*PARI)
            PARADICUM = PARADICUM + PARAD*PARI
          ENDIF  

          ! Height
          CANHT = CANHT + CANHTG

          ! Leaf areas
          PLA = PLA + PLAGT(1) + PLAGT(2)
          SENLA = SENLA + PLAS + PLASS + PLASC + PLAST
          IF (XSTAGE.GE.LRETS) THEN
            SENLARETAINED = SENLARETAINED
     &                        + (PLAS+PLASS+PLASC+PLAST)
          ELSE
            SENLALITTER = SENLALITTER + (PLAS+PLASS+PLASC+PLAST)
          ENDIF
          IF (LNUMSG.GT.0) THEN
            LAP(LNUMSG) = LAP(LNUMSG) + PLAGT(1)
            LATL(1,LNUMSG) = LATL(1,LNUMSG)+PLAG(1)
            IF (PLAG(2).GT.0.0) THEN
              IF (LNUMSG.LT.LNUMX) THEN
                LAP(LNUMSG+1) = LAP(LNUMSG+1) + PLAGT(2)
                LATL(1,LNUMSG+1) = LATL(1,LNUMSG+1)+PLAG(2)
              ELSEIF (LNUMSG.GE.LNUMX) THEN
                LAP(LNUMSG) = LAP(LNUMSG) + PLAGT(2)
                LATL(1,LNUMSG) = LATL(1,LNUMSG)+PLAG(2)
              ENDIF
            ENDIF
          ENDIF

          IF (ISTAGE.GT.0) GPLA(ISTAGE) = AMAX1(0.0,PLA-SENLA)
          LAI = AMAX1 (0.0,(PLA-SENLA)*PLTPOP*0.0001)
          LAIX = AMAX1(LAIX,LAI)

          PLASTMP = PLAS + PLASS + PLASC + PLAST
          ! Distribute senesced leaf over leaf positions
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
          ENDIF

          IF (fileiot(1:2).NE.'DS') THEN
          IF (LNUMSG.GT.0) CALL Cslayers
     X     (chtpc,clapc,               ! Canopy characteristics
     X     pltpop,lai,canht,           ! Canopy aspects
     X     lnumsg,lap,lap(lnumsg),     ! Leaf cohort number and size
     X     LAIL)                       ! Leaf area indices by layer
          ENDIF

          ! PAR interception
          IF (PARIP.LT.0.0.AND.LAI.GT.0.0) THEN
            PARI = (1.0 - EXP(-KCAN*(LAI+AWNAI)))
            !WRITE(fnumwrk,'(A28,F5.3)')
     X      ! '  PARI from one-crop model: ',PARI
            ! For maize, kcan is calculated as:
            ! 1.5 - 0.768*((rowspc*0.01)**2*pltpop)**0.1
            ! eg. 1.5 - 0.768*((75*0.01)**2*6.0)**0.1  =  0.63
          ELSE
            PARI = 0.0
          ENDIF

          ! Specific leaf area
          SLA = -99.0
          IF (LFWT.GT.0) SLA = (PLA-SENLA) / (LFWT*(1.0-LSHFR))
          ! Warning if SLA too low
          IF (SLA.LT.0.0.AND.SLA.GT.-90.0) THEN
            WRITE(fnumwrk,'(A21,F8.3,A12)')
     X       '  SLA below zero at: ',sla,' Reset to 0'
            SLA = -99.0
          ENDIF

          ! Leaf sheath area
          IF (RSTAGE.LE.4.0) THEN
            LSHAI = (LFWT*LSHFR*LSHAWS)*PLTPOP*0.0001
          ELSE
            ! 2.0 senesces per day
            LSHAI =LSHAI * (1.0-(2.0/100.0)*TT/20.0)  
           ENDIF  

          ! Stem area 
          SAID = AMAX1 (0.0,(STWT*SAWS*PLTPOP*0.0001))

          ! Tillers (Limited to maximum of 20)
          TNUM = AMIN1(20.0,AMAX1(1.0,TNUM+TNUMG-TNUMD-TNUMLOSS))
          IF (LNUMSG.GT.0) TNUML(LNUMSG) = TNUM

          ! Plants
          PLTPOP = PLTPOP - PLTLOSS   

          IF (PLTPOP < 1.E-5) THEN
            WRITE(fnumwrk,'(I5,1X,I3.3,
     &        " PLTPOP is zero or negative. Set to zero.",/)') YEAR, DOY
            PLTPOP = AMAX1(PLTPOP, 1.E-5)
          ENDIF

          ! Root depth and length
          IF (SDEPTH.GT.0.0 .AND.RTDEP.LE.0.0) RTDEP = SDEPTH
          RTDEP = AMIN1 (RTDEP+RTDEPG,DEPMAX)
          
          DO L = 1, NLAYR
            RLV(L)=RTWTL(L)*RLWR*PLTPOP/DLAYR(L)
            IF (L.EQ.NLAYR.AND.RLV(L).GT.0.0)THEN
              IF (RTSLXDATE.LE.0) RTSLXDATE = YEARDOY
            ENDIF
          END DO
          ! Originally: RLV(L)=RTWTL(L)*(RLWR/0.6)*PLTPOP/DLAYR(L)
          ! 0.6 above kept to keep similarity with Ceres,
          ! in which a value of 0.98 for RLWR was applied to assimilate
          ! going to root. This was multiplied by a factor of 0.6 to 
          ! account for root repiration to get actual root dry weight 
          ! increase.

          ! Vernalization.  NB. Starts at germination
          CUMVD = AMAX1(0.0,CUMVD+TFV-VDLOST)
          IF (ISTAGE.GE.7 .OR. ISTAGE.LE.1) THEN
            IF (P1V.GT.0.0) THEN
              IF (p1v.GT.0.0) THEN
                VRNSTAGE =AMAX1(0.,AMIN1(1.,CUMVD/p1v))
              ELSE
                VRNSTAGE = 1.0
              ENDIF
              !VF = AMAX1(0.,AMAX1(0.,AMIN1(1.,CUMVD/P1V)))
              ! BELOW FROM CSCRP
              VF = AMAX1(0.,(1.0-VEFF) + VEFF*VRNSTAGE)
            ELSE
              VF = 1.0   
            ENDIF
          ELSEIF (ISTAGE.GT.1 .AND. ISTAGE.LT.7) THEN
              VF = 1.0
          ENDIF

          ! Cold hardiness
          HARDAYS = AMAX1(HARDAYS+TFH-HARDILOS,0.0)
          HARDI = AMIN1(1.0,HARDAYS/HDUR)
          TKILL = LT50S + (LT50H-LT50S)*HARDI

          ! Nitrogen
          NUPC = NUPC + NUPD
          LEAFN = LEAFN + DLEAFN + SEEDNT
     &          - GRAINNGL - SENNLFG - SENNLFGRS
          IF (LEAFN.LT.1.0E-10) LEAFN = 0.0
          STEMN = STEMN + DSTEMN
     &          - GRAINNGS - SENNSTG - SENNSTGRS
          IF (STEMN.LT.1.0E-10) STEMN = 0.0
          ROOTNS = 0.0
          DO L = 1, NLAYR
            SENNL(L) = SENNL(L) + RTNSL(L)
            ROOTNS = ROOTNS + RTNSL(L)
            SENNS = SENNS + RTNSL(L)
          END DO
          ROOTN = ROOTN + DROOTN + SEEDNR - GRAINNGR - ROOTNS
          SEEDN = SEEDN - SEEDNR - SEEDNT
          IF (SEEDN.LT.1.0E-6) SEEDN = 0.0
          GRAINN = GRAINN + GRAINNG + GRAINNGL + GRAINNGS + GRAINNGR
     &           + RSNUSEG
          RSN = RSN - RSNUSEG - RSNUSER - RSNUSET
     &        + SENNLFGRS + SENNSTGRS
          IF (XSTAGE.GE.LRETS) THEN
            DEADN = DEADN + SENNLFG + SENNSTG
          ELSE
            SENNL(0) = SENNL(0) + (SENNLFG+SENNSTG)
          ENDIF

          ! Harvest index for N
          HIND = 0.0
          IF ((LEAFN+STEMN+GRAINN+RSN+DEADN).GT.0.0)
     &     HIND = GRAINN/(LEAFN+STEMN+GRAINN+RSN+DEADN)

          ! Variables expressed per unit ground area:living plant
          CARBOAC = CARBOC*PLTPOP*10.0
          RESPAC = RESPC*PLTPOP*10.0

          CWAD = AMAX1(0.0,(LFWT+STWT+GRWT+RSWT+DEADWT)*PLTPOP*10.0)
          DWAD = AMAX1(0.0,DEADWT*PLTPOP*10.0)
          GWAD = AMAX1(0.0,GRWT*PLTPOP*10.0)
          LLWAD = AMAX1(0.0,LFWT*(1.0-LSHFR)*10.0*PLTPOP)
          LSHWAD = AMAX1(0.0,LFWT*LSHFR*10.0*PLTPOP)
          CHWAD = AMAX1(0.0,CHWT*PLTPOP*10.0)
          ! NB.No reserves in chaff
          RSWAD = AMAX1(0.0,RSWT*PLTPOP*10.0)
          RSWADPM = AMAX1(0.0,RSWTPM*PLTPOP*10.0)
          RWAD = AMAX1(0.0,RTWT*PLTPOP*10.0)
          SDWAD = AMAX1(0.0,(SEEDRS+SDCOAT)*10.0*PLTPOP)
          STWAD = AMAX1(0.0,(STWT-CHWT)*10.0*PLTPOP)
          ! NB. Stem weigh here excludes chaff wt.
          STLSRWAD = AMAX1(0.0,(STWT-CHWT+LFWT*LSHFR+RSWT)*10.0*PLTPOP)
          LLRSWAD = AMAX1(0.0,LLRSWT*PLTPOP*10.0)
          LSHRSWAD = AMAX1(0.0,LSHRSWT*PLTPOP*10.0)
          STRSWAD = AMAX1(0.0,STRSWT*PLTPOP*10.0)
          CHRSWAD = AMAX1(0.0,CHRSWT*PLTPOP*10.0)

          SENWAS = SENWS*10.0*PLTPOP
          SENCAS = SENCS*10.0*PLTPOP
          SENLGAS = SENLGS*10.0*PLTPOP
          SENWAL(0) = SENWL(0)*PLTPOP*10.0
          SENCAL(0) = SENCL(0)*PLTPOP*10.0
          SENLGAL(0) = SENLGL(0)*PLTPOP*10.0
          DO L =1,NLAYR
            RTWTAL(L) = RTWTL(L)*PLTPOP*10.0
            SENWAL(L) = SENWL(L)*PLTPOP*10.0
            SENCAL(L) = SENCL(L)*PLTPOP*10.0
            SENLGAL(L) = SENLGL(L)*PLTPOP*10.0
          ENDDO

          TWAD = (SEEDRS+SDCOAT+RTWT+LFWT+STWT+GRWT+RSWT+DEADWT)
     &         * PLTPOP*10.0
          VWAD = (LFWT+STWT+RSWT+DEADWT)*PLTPOP * 10.0
          EWAD = (GRWT+CHWT)*PLTPOP * 10.0

          IF (GRNUM.GT.0.0) THEN 
            GRNUMAD = GRNUM*PLTPOP
          ELSE
            GRNUMAD = 0.0
          ENDIF  
          TNUMAD = TNUM*PLTPOP

          NUAD = NUPC*PLTPOP*10.0
          CNAD = (LEAFN+STEMN+GRAINN+RSN+DEADN)*PLTPOP*10.0
          DNAD = DEADN*PLTPOP*10.0
          GNAD = GRAINN*PLTPOP*10.0
          LLNAD = LEAFN*(1.0-LSHFR)*PLTPOP*10.0
          RNAD = ROOTN*PLTPOP*10.0
          RSNAD = RSN*PLTPOP*10.0
          SDNAD = SEEDN*PLTPOP*10.0
          SNAD = STEMN*PLTPOP*10.0
          TNAD = (ROOTN+LEAFN+STEMN+RSN+GRAINN+SEEDN+DEADN)*PLTPOP*10.0
          VNAD = (LEAFN+STEMN+RSN+DEADN)*PLTPOP*10.0

          SENNAS = SENNS*10.0*PLTPOP
          SENNAL(0) = SENNL(0)*PLTPOP*10.0
          DO L =1,NLAYR
            SENNAL(L) = SENNL(L)*PLTPOP*10.0
          ENDDO

          ! STAGES:Reproductive development (Rstages)
          CUMDU = CUMDU + DU
          IF (GESTAGE.GE.1.0) CUMTU = CUMTU + TT

          IF (CUMDU.LT.PTH(0) .AND. PD(0) > 0.) THEN
            RSTAGE = CUMDU/PD(0)
          ELSE
            DO L = 6,1,-1
              IF (CUMDU.GE.PTH(L-1)) THEN
                RSTAGE = FLOAT(L) + (CUMDU-PTH(L-1))/PD(L)
                ! Following is necessary because xstages non-sequential
                RSTAGE = AMIN1(6.9,RSTAGE)
                EXIT
              ENDIF
            ENDDO
          ENDIF
          IF (CROP.EQ.'MZ'.AND.PDADJ.LE.-99.0.AND.RSTAGE.GT.2.0) THEN
            PDADJ = (CUMTU-TT-PD(0))/(CUMDU-DU-PD(0))
            WRITE(fnumwrk,'(A26,F6.1)')
     &       ' Phase adjustment         ',(PDADJ-1.0)*PD(2)
            WRITE(fnumwrk,'(A24)')'   PHASE OLD_END NEW_END'
            DO L = 2,10
              PTHOLD = PTH(L)
              PTH(L) = PTH(L) + AMAX1(0.0,PDADJ-1.0)*PD(2)
              WRITE(fnumwrk,'(I8,2F8.1)')L,PTHOLD,PTH(L)
            ENDDO
          ENDIF

          ! STAGES:Germination and emergence (Gstages)
          ! NB 0.5 factor used to equate to Zadoks)
          IF (ISTAGE.GT.7) CUMGEU = CUMGEU + GEU
          IF (CUMGEU.LT.PEGD) THEN
            GESTAGE = AMIN1(1.0,CUMGEU/PEGD*0.5)
          ELSE
            GESTAGE = AMIN1(1.0,0.5+0.5*(CUMGEU-PEGD)/(PEMRG*SDEPTH))
          ENDIF

          ! STAGES:Leaf numbers
          IF (LNUMSG.GT.0 .AND. ISTAGE.LE.2) THEN
            ! If new leaf to be produced
            IF ((TT/PHINT).GT.(FLOAT(LNUMSG)-LNUMSD)) THEN 
              ! If new leaf will be in 3rd phint phase
              IF (LNUMSD.LE.(PHINTL(1)+PHINTL(2)).AND.
     &          LNUMSD+(TT/PHINT).GT.(PHINTL(1)+PHINTL(2))) THEN
                  TTTMP = TT - PHINT*(FLOAT(LNUMSG)-LNUMSD)
                  LNUMSD = LNUMSD+(TT-TTTMP)/PHINT+
     &             TTTMP/(PHINTS*PHINTF(3))
              ! If new leaf will be in 2nd phint phase
              ELSEIF (LNUMSD.LE.PHINTL(1).AND.
     &          LNUMSD+(TT/PHINT).GT.PHINTL(1)) THEN
                  TTTMP = TT - PHINT*(FLOAT(LNUMSG)-LNUMSD)
                  LNUMSD = LNUMSD+(TT-TTTMP)/PHINT+
     &             TTTMP/PHINTS
              ELSE
              ! New leaf in current phint phase
              LNUMSD = AMIN1(FLOAT(LNUMX-1)+0.9,LNUMSD+TT/PHINT)
              ENDIF
              LNUMSD = AMIN1(FLOAT(LNUMX-1)+0.9,LNUMSD)
            ! NO new leaf  
            ELSE
              LNUMSD = AMIN1(FLOAT(LNUMX-1)+0.9,LNUMSD+TT/PHINT)
            ENDIF
          ENDIF
          IF (LNUMSD.GE.FLOAT(LNUMX-1)+0.9) THEN
            IF (CCOUNTV.EQ.0) WRITE (fnumwrk,'(A35,I4)')
     &       ' Maximum leaf number reached on day',DOY
            CCOUNTV = CCOUNTV + 1
            IF (CCOUNTV.EQ.50) THEN
              WRITE (fnumwrk,'(A47,/,A44,/A26)')
     &         ' 50 days after maximum leaf number! Presumably ',
     &         ' vernalization requirement could not be met!',
     &         ' Will assume crop failure.'
              CFLFAIL = 'Y'
            ENDIF
          ENDIF
          LNUMSG = INT(LNUMSD)+1
          IF (LNUMSG.EQ.100) THEN
           WRITE (fnumwrk,'(A47,/,A47,/,A47,/,A26)')
     &      ' Maximum leaf number reached! Presumably       ',
     &      ' vernalization requirement could not be met,   ',
     &      ' or photoperiod too short.                     ',
     &      ' Will assume crop failure.'
            CFLFAIL = 'Y'
          ENDIF
          LCNUM = INT(LNUMSD)+1

          ! STAGES:Overall development (Istages)      Zadoks  Rstages
          ! 8 - Sowing date                             00      0.0
          ! 9 - Germination                             05      1.0
          ! 1 - Emergence                               10      1.0
          ! 2 - End spikelet production (=t spikelet)   ??      2.0
          ! 3 - End leaf growth                         40      3.0
          ! 4 - End spike growth                        50      4.0
          ! 5 - End lag phase of grain growth           80      5.0
          ! 6 - End grain fill (Physiological maturity) 90      6.0
          ! 7 - Harvest maturity or harvest             92      6.9
          ! More precisely translated as:
          !  Xstage Zstage
          !      .1  1.00
          !     1.0
          !     2.0
          !     2.5 31.00  Jointing after tsp at 2.0+PD2(1)/PD(2)
          !     3.0 40.00
          !     4.0 57.00
          !     5.0 71.37
          !     5.5 80.68
          !     6.0 90.00
          ! Possible new rstages:
          !  Rstnew?               Xstage Zstage
          !       0 Wetted up         1.0     0.0
          !     1.0 End juvenile
          !     2.0 Double ridges
          !     3.0 Terminal spikelet 2.0
          !         Jointing          2.?    31.0
          !     4.0 Last leaf         3.0    39.0
          !     5.0 Spike emergence          50.0
          !         End spike growth  4.0
          !     6.0 Start anthesis           60.0
          !     7.0 End anthesis             70.0
          !     8.0 End lag           5.0    71.4
          !         End milk          5.5    80.7
          !     9.0 End grain fill    6.0    90.0
          !    10.0 Harvest           6.9    92.0

          IF (ISTAGE.EQ.7) THEN                       ! Pre-planting
            ISTAGE = 8
            XSTAGE = 8.0
          ELSEIF (ISTAGE.EQ.8) THEN                   ! Planted
            XSTAGE = FLOAT(ISTAGE) + GESTAGE*2.0
            IF(GESTAGE.GE.0.5) THEN
              ISTAGE = 9
              XSTAGE = FLOAT(ISTAGE) + (GESTAGE-0.5)*2.0
            ENDIF
          ELSEIF (ISTAGE.EQ.9) THEN                   ! Germination
            XSTAGE = FLOAT(ISTAGE) + (GESTAGE-0.5)*2.0
            IF(GESTAGE.GE.1.0) THEN
              ISTAGE = 1
              XSTAGE = 1.0
            ENDIF
          ELSE                                        ! Emergence on
            ISTAGE = INT(RSTAGE)
            XSTAGE = AMIN1(6.9,RSTAGE)                ! < 7 (=pre-plant)
          ENDIF
          ! Secondary stages
          SSTAGE = AMAX1(0.0,AMIN1(1.0,(XSTAGE-AINT(XSTAGE))))

          ! STAGES:Overall development (Zadoks)
          ! 01=begining of seed imbibition (assumed to be at planting)
          ! 05=germination (assumed to be when the radicle emerged)
          ! 09=coleoptile thru soil surface
          ! 10=first leaf emerged from the coleoptile (= emergence)
          ! 11=first leaf fully expanded --> 1n=nth leaf fully expanded
          ! 20=first tiller appeared on some plants --> 2n=nth tiller
          ! 30=f(reproductive stage)

          IF (XSTAGE.GE.8.0 .AND. XSTAGE.LE.9.0) THEN
            ZSTAGE =  ((XSTAGE-8.0)/2.0)*10.0
          ELSEIF (XSTAGE.GT.9.0) THEN
            ZSTAGE = (0.5+((XSTAGE-9.0)/2.0))*10.0
          ELSEIF (XSTAGE.GE.0.0 .AND. XSTAGE.LE.(2.0+pd2fr(1))) THEN
            IF (TNUM.LT.2.0) THEN
              ZSTAGE = AMIN1(20.0,10.0 + LNUMSD)
            ELSE
              ZSTAGE = AMIN1(30.0,20.0 + (TNUM-1.0))
            ENDIF
            IF (ZSTAGE.LT.ZSTAGEP) ZSTAGE = ZSTAGEP
            ZSTAGEP = ZSTAGE
          ELSEIF (XSTAGE.GT.(2.0+pd2fr(1)) .AND. XSTAGE.LE.3.0) THEN
            ZSTAGE = 30.0 + 10.0*(XSTAGE-(2.0+pd2fr(1)))/(1.0-pd2fr(1))
          ELSEIF (XSTAGE.GT.3.0 .AND. XSTAGE.LE.4.0) THEN
            ZSTAGE = 40.0 + 10.0*(XSTAGE-3.0)
          ELSEIF (XSTAGE.GT.4.0 .AND. XSTAGE.LE.5.0) THEN
            IF (XSTAGE.LT.ASTAGE) THEN
              ZSTAGE = 50.0 + 10.0*((XSTAGE-4.0)/(ASTAGE-4.0))
            ELSEIF (XSTAGE.GE.ASTAGE.AND.XSTAGE.LT.ASTAGEND) THEN
              ZSTAGE = 60.0 + 10.0*((XSTAGE-ASTAGE)/(ASTAGEND-ASTAGE))
            ELSE
              ZSTAGE = 70.0 + 10.0*((XSTAGE-ASTAGEND)/(5.0-ASTAGEND))
            ENDIF
          ELSEIF (XSTAGE.GT.5.0 .AND. XSTAGE.LE.6.0) THEN
            ZSTAGE = 80.0 + 10.0*(XSTAGE-5.0)
          ELSEIF (XSTAGE.GT.6.0 .AND. XSTAGE.LE.7.0) THEN
            ZSTAGE = 90.0 + 10.0*(XSTAGE-6.0)
          ENDIF
          
          GO TO 9345   ! Jump over WDBachelor-CERES calculations
          ! Following were introduced for comparative purposes only
          ! Original CRITICAL AND MINIMA N concentrations.
          ! Below from w_nfacto as reworked by WDB
          ! Conversions from XSTAGE to ZADOKS growth stage
          ! From CROPSIM-CERES
          IF (XSTAGE.GE.8.0 .AND. XSTAGE.LE.9.0) THEN
            ZSTAGE =  ((XSTAGE-8.0)/2.0)
          ELSEIF (XSTAGE.GT.9.0) THEN
            ZSTAGE = (0.5+((XSTAGE-9.0)/2.0))
          ENDIF
          ! From CERES 9WDB)
          IF (XSTAGE .LE. 2.0) THEN
              ZSTAGE = XSTAGE
          ENDIF
          IF (XSTAGE .GT. 2.0 .AND. XSTAGE .LE. 3.0) THEN
              ZSTAGE = 2.00 + 2.0000*(XSTAGE-2.0)
          ENDIF
          IF (XSTAGE .GT. 3.0 .AND. XSTAGE .LE. 4.0) THEN
              ZSTAGE = 4.00 + 1.7000*(XSTAGE-3.0)
          ENDIF
          IF (XSTAGE .GT. 4.0 .AND. XSTAGE .LE. 4.4) THEN
              ZSTAGE = 5.70 + 0.8000*(XSTAGE-4.0)
          ENDIF
          IF (XSTAGE .GT. 4.4 .AND. XSTAGE .LE. 6.0) THEN
              ZSTAGE = 6.02 + 1.8625*(XSTAGE-4.4)
          ENDIF
          YSTAGE = XSTAGE
          ZS2    = ZSTAGE*ZSTAGE
          ZS3    = ZS2*ZSTAGE
          ZS4    = ZS3*ZSTAGE
          IF (P1V .GE. 0.03) THEN
             TCNP = -5.0112400-6.350677*ZSTAGE+14.9578400*SQRT(ZSTAGE)
     1              +0.2238197*ZS2
           ELSE
             TCNP =  7.4531813-1.7907829*ZSTAGE+0.6092849*SQRT(ZSTAGE)
     1              +0.0933967*ZS2
          ENDIF
          IF (ZSTAGE .GT. 6.0) THEN
             TCNP = TCNP - (ZSTAGE-6.0)*0.140
          ENDIF
          TCNP  = TCNP/100.0
          TMNC  = (2.97-0.455*XSTAGE)/100.0
          RCNP  = (2.10-0.14*SQRT(ZSTAGE))*0.01 ! RCNC in original Ceres
          RMNP  = 0.75 * RCNP
          ZSTAGE = ZSTAGE * 10.0
9345      CONTINUE     

          ! Stage dates and characteristics
          ! NB. Characeristics are at end of phase
          IF (ISTAGE.NE.ISTAGEP.AND.ISTAGEP.GT.0) THEN
            STGDOY(ISTAGEP) = YEARDOY
            CWADSTG(ISTAGEP) = CWAD
            LAISTG(ISTAGEP) = LAI
            LNUMSTG(ISTAGEP) = LNUMSD
            CNADSTG(ISTAGEP) = CNAD
          ENDIF
          
          ! Double ridge factors by calibration from LAMS experiment
          drf1 = 1.9
          drf2 = 0.058
          drf3 = 3.3
          !DRSTAGE = AMAX1(1.1,drf1-drf2*(LNUMSD-drf3))
          ! Changed to same as in CCSRP
          DRSTAGE = 1.6
          IF (DRDAT.EQ.-99 .AND. RSTAGE.GE.DRSTAGE) THEN
            DRDAT = YEARDOY
            WRITE(fnumwrk,*)' '
            WRITE(fnumwrk,*)'Double ridges. Stage,Leaf#: ',
     &       DRSTAGE,LNUMSD
             ! NB. Experimental. DR occurs at later apical stage when
             !     leaf # less, earlier when leaf # greater (ie.when
             !     early planting of winter type).
          ENDIF
          IF (TSDAT.EQ.-99 .AND. RSTAGE.GE.2.00) THEN
            TSDAT = YEARDOY
            LNUMTS = LNUMSD
            ! Final leaf# algorithm. LAH Coefficients need to be in file
            FLN = LNUMSD + (2.8 + 0.1 * LNUMTS)
            TVR1 = MOD(FLN,(FLOAT(INT(FLN))))
            IF (TVR1.LT.0.5) THEN
              FLN = FLOAT(INT(FLN))-0.001
            ELSE
              FLN = FLOAT(INT(FLN))+0.999
            ENDIF
            PD2ADJ = ((FLN-LNUMTS)) * PHINTS
            WRITE(fnumwrk,*)' '  
            WRITE(fnumwrk,'(A25,I12)')' Terminal spikelet       ',tsdat
            WRITE(fnumwrk,*)' Terminal spilelet leaf #       ',LNUMTS  
            WRITE(fnumwrk,*)' Final leaf # (Aitken formula)  ',FLN      
            WRITE(fnumwrk,*)' P2 Durations Input,From Aitken ',
     &       PD(2),PD2ADJ
            IF (PD(2).LE.0.0) THEN
              PD(2) = PD2ADJ
              DO L = 2,10
                PTH(L) = PTH(L-1) + PD(L)
              ENDDO
              WRITE(fnumwrk,*)' AITKEN FORMULA USED TO CALCULATE P2  '
            ENDIF 
          ENDIF  
          IF (JDAT.EQ.-99 .AND. RSTAGE.GE.2.0+PD2(1)/PD(2)) THEN
            JDAT = YEARDOY
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A25,I12)') ' Jointing:               ',jdat
          ENDIF
          IF (LLDAT.EQ.-99 .AND. RSTAGE.GE.3.0) THEN
            LLDAT = YEARDOY
            FLNMODEL = LNUMSD
            WRITE (fnumwrk,'(A25,I12)')' Last leaf emergence:    ',lldat
          ENDIF
          IF (IEDAT.EQ.-99 .AND. RSTAGE.GE.4.0) THEN
            IEDAT = YEARDOY
            WRITE (fnumwrk,'(A25,I12)')' Inflorescence emergence:',iedat
          ENDIF
          IF (ADAT.LE.0.0 .AND. RSTAGE.GE.4.0+PD4(1)/PD(4)) THEN
            ADAT = YEARDOY
            WRITE (fnumwrk,'(A25,I12)') ' Anthesis:               ',adat
            RSWAA = RSWAD
            RSCA = RSCD
            CWAA = CWAD
            CNAA = CNAD
            LNPCA = LANC*100.0
            ADATEND = -99
          ENDIF
          IF (ADATEND.LE.0.0 .AND.
     &      RSTAGE.GE.4.0+(PD4(1)+PD4(2))/PD(4)) THEN
            ADATEND = YEARDOY
            TMEAN20A = TMEAN20
            SRAD20A = SRAD20
            STRESS20A = STRESS20
            GRNUM = (LFWT+STWT+RSWT)*G1CWT
            WRITE (fnumwrk,'(A25,I12)')
     &       ' End of anthesis:        ',adatend
            WRITE (fnumwrk,'(A27,F7.1)')
     &       ' Prior 20d mean temperature   ',tmean20a
            WRITE (fnumwrk,'(A27,F7.1)')
     &       ' Prior 20d mean stress factor ',stress20a
            WRITE (fnumwrk,'(A27)')'  NB. 1.0 = 0 stress          '
            WRITE (fnumwrk,*)'Grain #/m2,Nfg ',GRNUM*PLTPOP,NFG
            IF ((GRNUM*PLTPOP).LT.100.0) THEN
              WRITE (fnumwrk,*)'Crop failure - few grains set!'
            ENDIF
          ENDIF
          IF (RSTAGE.GT.ASTAGEND) THEN
            LAGSTAGE = AMAX1(0.0,
     &       AMIN1(1.0,(RSTAGE-ASTAGEND)/(5.0-ASTAGEND)))
          ENDIF

          ! Average nitrogen and water stress, and environmental vars
          ! LAH Changed from 6 to 7 JUNE 2008
          IF (ISTAGEP.GT.0 .AND. ISTAGEP.LE.7) THEN
            NFPC = NFPC + NFP
            NFGC = NFGC + NFG
            WFPC = WFPC + WFP
            WFGC = WFGC + WFG
            ICSDUR = ICSDUR + 1
            tmaxsump = tmaxsump + tmax
            tminsump = tminsump + tmin
            sradsum = sradsum + srad
            daylsum = daylsum + daylt
            co2sum = co2sum + co2
            rainsum = rainsum + rain
            etsum = etsum + et
            epsum = epsum + ep
            ICSDUR0 = ICSDUR0 + 1
            NFPC0 = NFPC0 + NFP
            NFGC0 = NFGC0 + NFG
            WFPC0 = WFPC0 + WFP
            WFGC0 = WFGC0 + WFG
            tmaxsump0 = tmaxsump0 + tmax
            tminsump0 = tminsump0 + tmin
            sradsum0 = sradsum0 + srad
            daylsum0 = daylsum0 + daylt
            co2sum0 = co2sum0 + co2
            rainsum0 = rainsum0 + rain
            etsum0 = etsum0 + et
            epsum0 = epsum0 + ep
            IF (ICSDUR.GT.0) THEN
              NFPAV(ISTAGEP) = NFPC / ICSDUR
              NFGAV(ISTAGEP) = NFGC / ICSDUR
              WFPAV(ISTAGEP) = WFPC / ICSDUR
              WFGAV(ISTAGEP) = WFGC / ICSDUR
              DAYSC(ISTAGEP) = ICSDUR
              tmaxav(ISTAGEP) = tmaxsump / ICSDUR
              tminav(ISTAGEP) = tminsump / ICSDUR 
              sradav(ISTAGEP) = sradsum / ICSDUR
              daylav(ISTAGEP) = daylsum / ICSDUR
              co2av(ISTAGEP) = co2sum / ICSDUR
              raincp(ISTAGEP) = rainsum 
              etc(ISTAGEP)   = etsum 
              epc(ISTAGEP)   = epsum 
              DAYSC(0) = ICSDUR0
              NFPAV(0) = NFPC0 / ICSDUR0
              NFGAV(0) = NFGC0 / ICSDUR0
              WFPAV(0) = WFPC0 / ICSDUR0
              WFGAV(0) = WFGC0 /ICSDUR0
              tmaxav(0) = tmaxsump0 / ICSDUR0
              tminav(0) = tminsump0 / ICSDUR0 
              sradav(0) = sradsum0 / ICSDUR0
              daylav(0) = daylsum0 / ICSDUR0
              co2av(0) = co2sum0 / ICSDUR0
              raincp(0) = rainsum0 
              etc(0)   = etsum0 
              epc(0)   = epsum0 
            ENDIF
            IF (ISTAGE.NE.ISTAGEP) THEN
              NFPAV(ISTAGE) = 1.0
              NFGAV(ISTAGE) = 1.0
              WFPAV(ISTAGE) = 1.0
              WFGAV(ISTAGE) = 1.0
              tmaxav(ISTAGE) = -99.0
              tminav(ISTAGE) = -99.0 
              sradav(ISTAGE) = -99.0
              daylav(ISTAGE) = -99.0
              co2av(ISTAGE) = -99.0
              etc(ISTAGE)   = 0.0
              epc(ISTAGE)   = 0.0
              raincp(ISTAGE) = -99.0
              NFPC = 0.0
              NFGC = 0.0
              WFPC = 0.0
              WFGC = 0.0
              ICSDUR = 0
              tmaxsump = 0.0 
              tminsump = 0.0 
              sradsum = 0.0 
              daylsum = 0.0 
              co2sum = 0.0 
              rainsum = 0.0 
              etsum = 0.0 
              epsum = 0.0 
              IF (ISTAGE.EQ.6) THEN
                NFPAV(ISTAGE) = NFP
                NFGAV(ISTAGE) = NFG
                WFPAV(ISTAGE) = WFP
                WFGAV(ISTAGE) = WFG
                tmaxav(ISTAGE) = TMAX
                tminav(ISTAGE) = TMIN 
                sradav(ISTAGE) = SRAD
                daylav(ISTAGE) = DAYLT
                co2av(ISTAGE) = co2
                etc(ISTAGE)   = 0
                epc(ISTAGE)   = 0
                raincp(ISTAGE) = 0
              ENDIF
            ENDIF
          ENDIF

          ! Phyllochron intervals
          IF (CROP.EQ.'BA'.AND.ISTAGE.NE.ISTAGEP.AND.ISTAGE.EQ.1) THEN
            tvr1 = 77.5 - 232.6*(DAYLT-DAYLTP)
            WRITE(FNUMWRK,*)' '
            WRITE(FNUMWRK,*)
     &       ' PHINT calculated from daylength change: ',tvr1
            WRITE(FNUMWRK,*)
     &       ' PHINT being used:                       ',phints 
          ENDIF
          IF (LNUMSG.GT.0) THEN
            IF (LNUMSD.LT.PHINTL(1)) THEN
              PHINT = PHINTS*PHINTF(1)
            ELSEIF (LNUMSD.GE.PHINTL(1)+PHINTL(2)) THEN
              PHINT = PHINTS*PHINTF(3)
            ELSE
              PHINT = PHINTS
            ENDIF  
          ENDIF

          ! Critical and minimum N concentrations
          IF (ISTAGE.LT.7) THEN
            LCNC = YVAL1(LCNCS,'0','9',XSTAGE)
            SCNC = YVAL1(SCNCS,'0','9',XSTAGE)
            RCNC = YVAL1(RCNCS,'0','9',XSTAGE)
            LMNC = YVAL1(LMNCS,'0','9',XSTAGE)
            SMNC = YVAL1(SMNCS,'0','9',XSTAGE)
            RMNC = YVAL1(RMNCS,'0','9',XSTAGE)
          ELSE
            RCNC = RCNCS(0) + (RCNCS(1)-RCNCS(0))*((XSTAGE-8.0)/2.0)
            RMNC = RMNCS(0) + (RCNCS(1)-RCNCS(0))*((XSTAGE-8.0)/2.0)
          ENDIF

          ! N concentrations and adjustments
          ! (Adjustements to account for changes in criticals)
          RANC = 0.0
          LANC = 0.0
          SANC = 0.0
          VANC = 0.0
          IF (RTWT.GT.0.0) RANC = ROOTN / RTWT
          IF (LFWT.GT.0.0) LANC = LEAFN / LFWT
          IF (STWT.GT.0.0) SANC = STEMN / STWT
          IF (VWAD.GT.0.0) VANC = VNAD/VWAD
          RSNGR = AMAX1(0.0,RTWT*(RANC-RCNC))
          RSNGL = AMAX1(0.0,LFWT*(LANC-LCNC))
          RSNGS = AMAX1(0.0,STWT*(SANC-SCNC))
          RSN = RSN + RSNGR + RSNGL + RSNGS
          ROOTN = ROOTN - RSNGR
          LEAFN = LEAFN - RSNGL
          STEMN = STEMN - RSNGS
          IF (RTWT.GT.0.0) RANC = ROOTN/RTWT
          IF (LFWT.GT.0) LANC = LEAFN/LFWT
          IF (STWT.GT.0.0) SANC = STEMN/STWT
          IF (VWAD.GT.0.0) VANC = VNAD/VWAD
          IF (LANC.LT.0.0) THEN
            WRITE(fnumwrk,*)'LANC below 0 with value of ',LANC
            WRITE(fnumwrk,*)'LEAFN,LFWT had values of   ',LEAFN,LFWT
            LANC = AMAX1(0.0,LANC)
          ENDIF
          IF (LFWT+STWT.GT.0.0) VCNC = 
     &     (LCNC*AMAX1(0.0,LFWT)+SCNC*AMAX1(0.0,STWT))/
     &     (AMAX1(0.0,LFWT)+AMAX1(0.0,STWT))
          IF (LFWT+STWT.GT.0.0) VMNC = 
     &     (LMNC*AMAX1(0.0,LFWT)+SMNC*AMAX1(0.0,STWT))/
     &     (AMAX1(0.0,LFWT)+AMAX1(0.0,STWT))

          CANANC = 0.0
          SDNC = 0.0
          GRAINANC = 0.0
          IF ((LFWT+STWT+GRWT+RSWT+DEADWT).GT.0.0)
     &     CANANC = (LEAFN+STEMN+GRAINN+RSN+DEADN)/
     &      (LFWT+STWT+GRWT+RSWT+DEADWT)
          IF (SEEDRS.GT.0.0) SDNC = SEEDN/(SEEDRS+SDCOAT)
          IF (GRWT.GT.0) GRAINANC = GRAINN/GRWT

          LCNF = 0.0
          SCNF = 0.0
          RCNF = 0.0
          IF (LCNC.GT.0.0) LCNF = LANC/LCNC
          IF (LCNF.GT.1.0001 .OR. LCNF.LT.0.0) THEN
            WRITE(fnumwrk,*)'LCNF out of limits with value of ',LCNF
            LCNF = AMAX1(0.0,AMIN1(1.0,LCNF))
          ENDIF
          IF (SCNC.GT.0.0.AND.STWT.GT.1.0E-10) SCNF = SANC/SCNC
          IF (RCNC.GT.0.0.AND.RTWT.GT.0.0) RCNF = RANC/RCNC

          ! Harvesting conditions
          IF (IHARI.EQ.'A' .AND. ISTAGE.EQ.6) THEN
            ! Here need to check out if possible to harvest.
            IF (YEARDOY.GE.HFIRST) THEN
              IF (SW(1).GE.SWPLTL.AND.SW(1).LE.SWPLTH) YEARHARF=YEARDOY
            ENDIF
            ! Check if past earliest date; check if not past latest date
            ! Check soil water
            ! If conditions met set YEARHARF = YEARDOY
            ! (Change YEARHARF to more something more appropriate)
          ENDIF

          ! Harvesting or failure
          IF (DAP.GE.90 .AND. ISTAGE.EQ.8) THEN
            CFLFAIL = 'Y'
            WRITE (FNUMWRK,*)'No germination within 90 days of sowing!'
          ENDIF
          
          IF (IHARI.NE.'A'.AND.DAPM.GE.90) THEN
            CFLFAIL = 'Y'
            WRITE (FNUMWRK,*)'90 days after physiological maturity!'
            WRITE (FNUMWRK,*)'Harvesting triggered!'
          ENDIF
          
          IF (IHARI.NE.'A'.AND.ISTAGE.GE.4.AND.ISTAGE.LT.7) THEN
            IF (TT20.NE.-99.0.AND.TT20.LE.0.0) THEN
              CFLFAIL = 'Y'
              WRITE (FNUMWRK,*)'20day thermal time mean = 0!'
              WRITE (FNUMWRK,*)'Harvesting triggered!'
            ENDIF
          ENDIF

          IF (CFLFAIL.EQ.'Y' .OR.
     &     IHARI.EQ.'R'.AND.YEARHARF.GT.-99.AND.YEARHARF.EQ.YEARDOY .OR.
     &     IHARI.EQ.'D'.AND.YEARHARF.GT.-99.AND.YEARHARF.EQ.DAP .OR.
     &     IHARI.EQ.'G'.AND.YEARHARF.GT.-99.AND.YEARHARF.LE.XSTAGE .OR.
     &     IHARI.EQ.'A'.AND.YEARHARF.GT.-99.AND.YEARHARF.EQ.YEARDOY .OR.
     &     IHARI.EQ.'M'.AND.XSTAGE.GE.6.0.AND.XSTAGE.LT.7.0 .OR.
     &     YEARHARF.LE.-99 .AND. XSTAGE.GE.6.9.AND.XSTAGE.LT.7.0) THEN
            IF (CFLFAIL.EQ.'Y') THEN
              STGDOY(10) = YEARDOY
              IF (STGDOY(9).EQ.9999999) STGDOY(9) = -99
              IF (STGDOY(8).EQ.9999999) STGDOY(8) = -99
              IF (STGDOY(5).EQ.9999999) STGDOY(5) = -99
              IF (STGDOY(4).EQ.9999999) STGDOY(4) = -99
              IF (STGDOY(3).EQ.9999999) STGDOY(3) = -99
              IF (STGDOY(2).EQ.9999999) STGDOY(2) = -99
              IF (STGDOY(1).EQ.9999999) STGDOY(1) = -99
              WFPAV(6) = WFPAV(ISTAGE)
              NFPAV(6) = NFPAV(ISTAGE)
            ENDIF
            IF (STGDOY(10).EQ.9999999) STGDOY(10) = -99
            STGDOY(6) = YEARDOY
            STGDOY(11) = YEARDOY
            CWADSTG(6) = CWAD
            LAISTG(6) = LAI
            LNUMSTG(6) = LNUMSD
            CNADSTG(6) = CNAD
            
            ! If running CSM use harvfrac to handle automatic mngement
            ! LAH May 2009 Was NE  CHP to check
            IF (FILEIOT .EQ. 'DS4') THEN
              hpc = harvfrac(1)*100.0   ! Harvest %
              hbpc = harvfrac(2)*100.0
            ENDIF  
 
            WRITE(fnumwrk,*)' '
            WRITE(fnumwrk,*)'HARVEST REACHED ',YEARDOY
            
            
            PARIUEM = -99.0
            IF (PARADCUM.GT.0.0.AND.PARADICUM.GT.0.0) THEN
              WRITE (fnumwrk,*)' '
              WRITE (fnumwrk,'(A53,F5.1,F4.1)')
     &         ' OVERALL PAR USE EFFICIENCY (INCIDENT,INTERCEPTED) = ',
     &         CWAD/PARADCUM/10.0, CWAD/PARADICUM/10.0 
               PARIUEM = CWAD/PARADICUM/10.0
            ENDIF  
                  
            WRITE(FNUMWRK,*) ' '
            WRITE(FNUMWRK,*) 'INORGANIC NO3 AND NH4 (kg/ha)'
            WRITE(FNUMWRK,*) ' PROFILE:  '
            WRITE(FNUMWRK,'(A15,2F6.1)')
     &        '  SEASON START:',SNO3PROFILEI,SNH4PROFILEI
            WRITE(FNUMWRK,'(A15,2F6.1)')
     &        '  SEASON END:  ',SNO3PROFILE,SNH4PROFILE
            WRITE(FNUMWRK,*) ' ROOTZONE: '
            WRITE(FNUMWRK,'(A15,2F6.1)')
     &        '  SEASON END:  ',SNO3ROOTZONE,SNH4ROOTZONE
 
            WRITE(FNUMWRK,*) ' '
            WRITE(FNUMWRK,*) 'TOTAL AND AVAILABLE WATER (mm) '
            WRITE(FNUMWRK,*) ' PROFILE:  '
            WRITE(FNUMWRK,'(A15,2F6.1)')
     &        '  SEASON START:',H2OPROFILEI,AH2OPROFILEI
            WRITE(FNUMWRK,'(A15,2F6.1)') 
     &       '  SEASON END:  ',H2OPROFILE,AH2OPROFILE
            WRITE(FNUMWRK,*) ' ROOTZONE: '
            WRITE(FNUMWRK,'(A15,2F6.1)')
     &        '  SEASON END:  ',H2OROOTZONE,AH2OROOTZONE
            
            ! Reset crop stage
            ISTAGE = 7
            XSTAGE = 7.0
          ENDIF

          IF(IHARI.EQ.'R'.AND.YRHARF.GT.-99.AND.YEARDOY.LT.YEARHARF)THEN
            IF (XSTAGE.GT.6.9 .AND. YRHARFF .NE. 'Y') THEN
              ! This loop is necessary because of non-sequential staging
              IF (XSTAGE.LT.7.0) THEN
                WRITE(fnumwrk,*)
     &           'WAITING FOR HARVEST! YEARDOY,YRHAR ',YEARDOY,YRHARF
                YRHARFF = 'Y'
              ENDIF
            ENDIF
          ENDIF

          ! After harvest residues
          IF (STGDOY(11).EQ.YEARDOY) THEN
            ! Surface
            RESWALG(0) = VWAD*(1.0-HBPC/100.0) + GWAD*(1.0-HPC/100.0)
            RESNALG(0) = (LEAFN+STEMN+DEADN)*PLTPOP*10.*(1.-HBPC/100.)
     &                 + GNAD*(1.0-HPC/100.0)
            RESCALG(0) = RESWALG(0) * 0.4
            RESLGALG(0) = LLWAD*LLIGP/100.0*(1.0-HBPC/100.0)
     &                  + LSHWAD*SLIGP/100.0*(1.0-HBPC/100.0)
     &                  + STWAD*SLIGP/100.0*(1.0-HBPC/100.0)
     &                  + GWAD*GLIGP/100.0*(1.0-HPC/100.0)
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

          ! Weather summary variables
          CUMTT = CUMTT + TT
          TMAXX = AMAX1(TMAXX,TMAX)
          TMINN = AMIN1(TMINN,TMIN)
          CO2MAX = AMAX1(CO2MAX,CO2)
          RAINC = RAINC + RAIN
          IF (ADAT.LT.0) RAINCA = RAINCA + RAIN
          SRADC = SRADC + SRAD
          IF (XSTAGE.GE.5.0 .AND. XSTAGE.LT.6.0) THEN
            GFTSUM = GFTSUM + TMEAN
            GFDSUM = GFDSUM + 1
            GFTMEAN = GFTSUM/GFDSUM
          ENDIF
          IF (XSTAGE.GE.5.7 .AND. XSTAGE.LT.6.0) THEN
            GMTSUM = GMTSUM + TMEAN
            GMDSUM = GMDSUM + 1
            GMTMEAN = GMTSUM/GMDSUM
          ENDIF
          IF (XSTAGE.GE.8.0 .AND. XSTAGE.LT.10.0) THEN
            GETSUM = GETSUM + TMEAN
            GEDSUM = GEDSUM + 1
            GETMEAN = GETSUM/GEDSUM
          ENDIF
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

          ! N fertilizer applications
          IF (NFERT.GT.0.AND.IFERI.EQ.'R'.AND.YEARDOY.EQ.YEARPLT) THEN
            DO I = 1, NFERT
              IF (FDAY(I).GT.YEARDOY) EXIT
              IF (FDAY(I).LE.-99) EXIT
              AMTNIT = AMTNIT + ANFER(I)
            END DO
            IF (FILEIOT.EQ.'XFL') WRITE(fnumwrk,*)' '
            WRITE(fnumwrk,'(A24,I4,A6)')
     &       ' Fertilizer N pre-plant ',NINT(amtnit),' kg/ha'
          ENDIF
          IF (NFERT.GT.0.AND.IFERI.EQ.'R'.AND.YEARDOY.GT.YEARPLT) THEN
            DO I = 1, NFERT
              IF (FDAY(I).GT.YEARDOY) EXIT
              IF (FDAY(I).EQ.YEARDOY) THEN
                AMTNIT = AMTNIT + ANFER(I)
                WRITE(fnumwrk,'(A14,I4,A10,I9,A13,I4,A6)')
     &          ' Fertilizer N ',NINT(anfer(i)),' kg/ha on ',
     &          YEARDOY,'     To date ',NINT(amtnit),' kg/ha'
              ENDIF
            END DO
          ENDIF

          ! Adjustment of kernel growth rate
          ! Originally set temperature response here
          IF (ISTAGE.EQ.5.AND.ISTAGEP.EQ.4) THEN
            WRITE(fnumwrk,*)'Start of linear kernel growth    '
            WRITE(fnumwrk,*)' Original kernel growth rate (G2) ',g2
!           chp handle zero divide
            IF (GRNUM .GT. 1.E-6) THEN
              G2 = (G2KWT-(GRWT/GRNUM)*1000.0) / (PD(5)*(6.0-XSTAGE))
            ELSE
              G2 = (G2KWT) / (PD(5)*(6.0-XSTAGE))
            ENDIF
            WRITE(fnumwrk,*)' Adjusted kernel growth rate (G2) ',g2
            WRITE(fnumwrk,*)' (Adjustment because growing at lag rate',
     &      ' for overlap into linear filling period)'
          ENDIF

          ! Stored variables (For use next day or step)
          ISTAGEP = ISTAGE
          ZSTAGEP = ZSTAGE
          RSTAGEP = RSTAGE
          DAYLTP = DAYLT

          ! Soil water aspects
          BLAYER = 0.0
          H2OA = 0.0
          IF (ISWWAT.EQ.'Y') THEN
            DO L = 1, NLAYR
              DLAYRTMP(L) = DLAYR(L)
              BLAYER = BLAYER + DLAYR(L)
              IF (RTDEP.GT.0.0.AND.RTDEP.LT.BLAYER) THEN
                DLAYRTMP(L) = RTDEP-(BLAYER-DLAYR(L))
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
 
          H2OPROFILE = 0.0
          H2OROOTZONE = 0.0
          AH2OPROFILE = 0.0
          AH2OROOTZONE = 0.0
          DO L = 1, NLAYR
            AH2OPROFILE = AH2OPROFILE + ((SW(L)-LL(L))*DLAYR(L))*10.0
            H2OPROFILE = H2OPROFILE + SW(L)*DLAYR(L)*10.0
            IF (RLV(L).GT.0.0) THEN
              AH2OROOTZONE = AH2OROOTZONE + ((SW(L)-LL(L))*DLAYR(L))*10.
              H2OROOTZONE = H2OROOTZONE + SW(L)*DLAYR(L)*10.
            ENDIF
          END DO

        ENDIF


      ELSEIF (DYNAMIC.EQ.OUTPUT .OR. 
     &        DYNAMIC.EQ.SEASEND .AND. SEASENDOUT.NE.'Y') THEN

        IF (YEARDOY.GE.YEARPLT .AND. STEP.EQ.STEPNUM) THEN             
 
          ! General file header
          IF (TN.LT.10) THEN
            WRITE (OUTHED,7104) RUNRUNI(1:5),EXCODE,TN,TRUNNAME
 7104       FORMAT ('*RUN ',A5,':',A10,' ',I1,' ',A40,'  ')
          ELSEIF (TN.GE.10. AND. TN.LT.100) THEN
           WRITE (OUTHED,7105) RUNRUNI,EXCODE,TN,RN,TRUNNAME
 7105      FORMAT ('*RUN ',A5,A10,' ',I2,',',I1,' ',A40,' ')
          ELSEIF (TN.GE.10 .AND. TN.LT.100) THEN
           WRITE (OUTHED,7106) RUNRUNI,EXCODE,TN,RN,TRUNNAME
 7106      FORMAT ('*RUN ',A5,A10,' ',I3,',',I1,' ',A40)
          ENDIF
 
          ! If seeding day
          IF (YEARDOY.EQ.STGDOY(7)) THEN
            CNCHAR = ' '
            IF (CN.EQ.1) THEN
              OUTPG = 'PlantGro.OUT'
              OUTPG2 = 'PlantGr2.OUT'
              OUTPGF = 'PlantGrf.OUT'
              OUTPN = 'PlantN.OUT  '

! File names are changed at end of simulation by CSM
! Changing names here eliminates wheat output in sequence runs.
              !IF (FNAME.EQ.'Y') THEN
              !  OUTPG = EXCODE(1:8)//'.OPG'
              !  OUTPG2 = EXCODE(1:8)//'.OG2'
              !  OUTPGF = EXCODE(1:8)//'.OGF'
              !  OUTPN = EXCODE(1:8)//'.ONI'
              !ENDIF

              CALL GETLUN ('PlantGro.OUT',NOUTPG)
              CALL GETLUN ('PlantN.OUT',NOUTPN)
              CALL GETLUN ('PlantGr2.OUT',NOUTPG2)
              CALL GETLUN ('PlantGrf.OUT',NOUTPGF)
            ELSE
              CNCHAR = TL10FROMI(CN)
              OUTPG = 'PlantGro.OU'//CNCHAR(1:1)
              OUTPN = 'PlantN.OU'//CNCHAR(1:1)
              CALL GETLUN (OUTPG,NOUTPG)
              CALL GETLUN (OUTPN,NOUTPN)
              CALL GETLUN (OUTPG2,NOUTPG2)
              CALL GETLUN (OUTPGF,NOUTPGF)
            ENDIF
            ! Open output file(s)
            IF (RUN.EQ.1 .AND. RUNI.LE.1) THEN
              OPEN (UNIT = NOUTPG, FILE = OUTPG)
              WRITE (NOUTPG,'(A27)')
     &        '$GROWTH ASPECTS OUTPUT FILE'
              OPEN (UNIT = NOUTPG2, FILE = OUTPG2)
              WRITE (NOUTPG2,'(A37)')
     &        '$GROWTH ASPECTS SECONDARY OUTPUT FILE'
              OPEN (UNIT = NOUTPN, FILE = OUTPN)
              WRITE (NOUTPN,'(A35)')
     &        '$PLANT NITROGEN ASPECTS OUTPUT FILE'
              OPEN (UNIT = NOUTPGF, FILE = OUTPGF)
              WRITE (NOUTPGF,'(A26)')
     &        '$GROWTH FACTOR OUTPUT FILE'
              CLOSE (NOUTPG)
              CLOSE (NOUTPG2)
              CLOSE (NOUTPN)
              CLOSE (NOUTPGF)
            ENDIF
 
            IF (IDETG.NE.'N'.AND.IDETL.NE.'0') THEN
              OPEN (UNIT = NOUTPG, FILE = OUTPG, STATUS='UNKNOWN',
     &        POSITION = 'APPEND')
              OPEN (UNIT = NOUTPN, FILE = OUTPN, STATUS = 'UNKNOWN',
     &        POSITION = 'APPEND')
     
              IF (FILEIOT(1:2).EQ.'DS') THEN
                CALL HEADER(2, NOUTPG, RUN)
                CALL HEADER(2, NOUTPN, RUN)
              ELSE
               WRITE (NOUTPG,'(/,A77,/)') OUTHED
               WRITE (NOUTPN,'(/,A77,/)') OUTHED
               WRITE (NOUTPG,103) MODEL
               WRITE (NOUTPN,103) MODEL
  103          FORMAT (' MODEL            ',A8)
               WRITE (NOUTPG,1031) MODNAME
               WRITE (NOUTPN,1031) MODNAME
 1031          FORMAT (' MODULE           ',A8)
               WRITE (NOUTPG,104)
     &          EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
               WRITE (NOUTPN,104)
     &          EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
  104          FORMAT (' EXPERIMENT       ',A8,A1,A2,A2,A47)
               WRITE (NOUTPG,102) TN,TNAME
               WRITE (NOUTPN,102) TN,TNAME
  102          FORMAT (' TREATMENT',I3,'     ',A25)
               WRITE (NOUTPG,107) CROP,VARNO,VRNAME
               WRITE (NOUTPN,107) CROP,VARNO,VRNAME
  107          FORMAT (' GENOTYPE         ',A2,A6,'  ',A16)
               CALL Calendar (year,doy,dom,month)
               WRITE (NOUTPG,108) month,dom,NINT(pltpop),NINT(rowspc)
               WRITE (NOUTPN,108) month,dom,NINT(pltpop),NINT(rowspc)
  108          FORMAT(' ESTABLISHMENT    ',A3,I3,2X,I4,' plants/m2 in ',
     &          I3,' cm rows',/)
              ENDIF
              ! Write variable headings
              WRITE (NOUTPG,2201)
 2201         FORMAT ('@YEAR DOY   DAS   DAP TMEAN TKILL',
     &        '  GSTD  L#SD',
     &        ' PARID PARUD  AWAD',
     &        '  LAID  SAID  CAID',
     &        '  TWAD SDWAD  RWAD  CWAD  LWAD  SWAD  HWAD  HIAD',
     &        ' CHWAD  EWAD RSWAD SNWPD SNWLD SNWSD',
     &        '  RS%D',
     &        '  H#AD  HWUD',
     &        '  T#AD  SLAD  RDPD  PTFD',
     &        '  SWXD WAVRD',
     &        ' WUPRD  WFTD  WFPD  WFGD',
     &        '  NFTD  NFPD  NFGD NUPRD',
     &        '  TFPD  TFGD',
     &        ' VRNFD DYLFD')
     
              WRITE (NOUTPN,2251)
!             2021-02-15 chp Change NUAD to NUAC in header.
 2251         FORMAT ('@YEAR DOY   DAS   DAP TMEAN  GSTD  NUAC',
     &        '  TNAD SDNAD  RNAD  CNAD  LNAD  SNAD  HNAD  HIND',
     &        ' RSNAD SNNPD SNN0D SNN1D',
     B        '  RN%D  LN%D  SN%D  HN%D SDN%D  VN%D',
     C        ' LN%RD SN%RD RN%RD  VCN%  VMN% NUPRD',
     &        ' NDEMD')
  
              ! Delete PlantN if N switched off   
              IF (ISWNIT.NE.'Y') THEN
                CLOSE (UNIT=NOUTPN, STATUS = 'DELETE')
              ENDIF
     
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
     &           month,dom,yearplt,NINT(pltpopp),NINT(rowspc)
              ENDIF 
              WRITE (NOUTPG2,2252)
 2252         FORMAT ('@YEAR DOY   DAS   DAP TMEAN  GSTD  RSTD',
     A        ' LAIPD LAISD  LAID  CHTD SDWAD SNWLD SNWSD',
     a        '  H#AD  HWUD',
     B        ' SHRTD  PTFD  RDPD',
     C        '  RL1D  RL2D  RL3D  RL4D  RL5D  RL6D',
     D        '  RL7D  RL8D  RL9D RL10D')
     
            ! PlantGroReductionFactors
              OPEN (UNIT = NOUTPGF, FILE = OUTPGF, STATUS='UNKNOWN',
     &        POSITION = 'APPEND')  !chp 4/16/10
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
     &           month,dom,yearplt,NINT(pltpopp),NINT(rowspc)
              ENDIF
              WRITE (NOUTPGF,2215)
 2215         FORMAT ('!........DATES.......  TEMP STAGE ',
     N        ' ...PHENOLOGY.... ',
     1        ' .......PHOTOSYNTHESIS....... ', 
     M        ' .....GROWTH.....  ..TILLERS. ',
     2        'WATER STRESS DETERMINANTS',
     2        ' N STRESS DETERMINANTS       ')
              WRITE (NOUTPGF,2205)
 2205         FORMAT ('@YEAR DOY   DAS   DAP TMEAN  GSTD',
     N        '    DU VRNFD DYLFD TFGEM  WFGE',
     1        '  TFPD  WFPD  NFPD CO2FD RSFPD', 
     M        '  TFGD  WFGD  NFGD  WFTD  NFTD',
     &        ' WAVRD WUPRD  SWXD  EOPD',
     &        '  SNXD LN%RD SN%RD RN%RD      ')
     
            ELSE
              OPEN (UNIT=NOUTPG, FILE=OUTPG, STATUS = 'UNKNOWN')
              CLOSE (UNIT=NOUTPG, STATUS = 'DELETE')
              OPEN (UNIT=NOUTPN, FILE=OUTPN, STATUS = 'UNKNOWN')
              CLOSE (UNIT=NOUTPN, STATUS = 'DELETE')
              OPEN (UNIT=NOUTPG2, FILE=OUTPG2, STATUS = 'UNKNOWN')
              CLOSE (UNIT=NOUTPG2, STATUS = 'DELETE')
              OPEN (UNIT=NOUTPGF, FILE=OUTPGF, STATUS = 'UNKNOWN')
              CLOSE (UNIT=NOUTPGF, STATUS = 'DELETE')
            ENDIF
          ENDIF
 
          IF ((MOD(DAS,FROPADJ).EQ.0)
     &     .OR. (YEARDOY.EQ.STGDOY(7))
     &     .OR. (YEARDOY.EQ.STGDOY(11))) THEN
            IF (IDETG.NE.'N'.OR.IDETL.EQ.'0') THEN
              OUTCHOICE = 1
              ! Note possibilities. To change must recompile.
              IF (OUTCHOICE.EQ.1) THEN
                ! 1. Include reserves. Stem includes sheath
                LLWADOUT = AMAX1(0.0,LLWAD+LLRSWAD)
                LSHWADOUT = AMAX1(0.0,LSHWAD+LSHRSWAD)
                STWADOUT = AMAX1(0.0,STWAD+STRSWAD+LSHWAD+LSHRSWAD)
                SAIDOUT = SAID + LSHAI
                CHWADOUT = AMAX1(0.0,CHWAD+CHRSWAD)
                IF (LFWT.GT.1.0E-6) 
     &           SLAOUT = (PLA-SENLA) / (LFWT*(1.0-LSHFR)+LLRSWT)
              ELSEIF (OUTCHOICE.EQ.2) THEN
                ! 2. No reserves. Stem does not includes sheath
                LLWADOUT = AMAX1(0.0,LLWAD)
                LSHWADOUT = AMAX1(0.0,LSHWAD)
                STWADOUT = AMAX1(0.0,STWAD)
                SAIDOUT = SAID
                CHWADOUT = AMAX1(0.0,CHWAD)
                SLAOUT = -99.0
                IF (LFWT.GT.1.0E-6) 
     &           SLAOUT = (PLA-SENLA) / (LFWT*(1.0-LSHFR))
              ENDIF  
              IF (SLA.LE.0.0) SLAOUT = -99.0
              
              CALL Csopline(senw0c,senwal(0))
              CALL Csopline(senwsc,senwas)
              IF (PARIP.GE.0.0) THEN
                PARIOUT = PARIP/100.0
              ELSE
                PARIOUT = PARI
              ENDIF
              CALL Csopline(hwudc,gwud)

              IF (IDETG.NE.'N') THEN
              WRITE (NOUTPG,
     &        '(I5,I4,2I6,2F6.1,
     &        2F6.2,
     &        2F6.2,F6.1,F6.2,F6.3,
     &        F6.2,
     &        7I6,F6.2,
     &        4I6,2A6,
     &        F6.1,
     &        I6,A6,
     &        2I6,2F6.2,
     &        2F6.1,
     &        4F6.2,
     &        3F6.2,F6.1,
     &        2F6.2,
     &        2F6.2,
     &        F6.1)')      
     &        YEAR,DOY,DAS,DAP,TMEAN,TKILL,
     &        ZSTAGE,LNUMSD,   ! Zadoks staging
!     &        XSTAGE,LNUMSD,    ! Ceres staging
     &        PARIOUT,PARUED,AMIN1(999.9,CARBOA),
     &        LAI,SAIDOUT,LAI+SAIDOUT,
     &        NINT(TWAD),
     &        NINT(SDWAD),
     &        NINT(RWAD),
     &        NINT(CWAD),
     &        NINT(LLWADOUT),
     &        NINT(STWADOUT),NINT(GWAD),HIAD,
     &        NINT(CHWADOUT),
     &        NINT(EWAD),
     &        NINT(RSWAD),
     &        NINT(DWAD),SENW0C,SENWSC,
     &        RSCD*100.0,
     &        NINT(GRNUMAD),HWUDC,
              ! NB. SLA includes reserves   
     &        NINT(TNUMAD),NINT(SLAOUT),RTDEP/100.0,PTF,
     &        H2OA,AMIN1(99.9,WAVR),
     &        AMIN1(15.0,WUPR),1.0-WFT,1.0-WFP,1.0-WFG,
     &        1.0-NFT,1.0-NFP,1.0-NFG,AMIN1(2.0,NUPR),
     &        1.0-TFP,1.0-TFG,
     &        1.0-VF,1.0-DF 

!     VSH CSV output corresponding to PlantGro.OUT
      IF (FMOPT == 'C') THEN 
         CALL CsvOut(EXCODE, RUN,TN,RN,SN, ON, REP, CN, YEAR,DOY,
     &DAS, DAP, TMEAN, TKILL, ZSTAGE, LNUMSD, PARIOUT, PARUED, CARBOA, 
     &LAI, SAIDOUT, TWAD, SDWAD, RWAD, CWAD, LLWADOUT, STWADOUT, GWAD,
     &HIAD, CHWADOUT, EWAD, RSWAD, DWAD, SENW0C, SENWSC, RSCD, GRNUMAD,
     &HWUDC, TNUMAD, SLAOUT, RTDEP, PTF, H2OA, WAVR, WUPR, WFT, WFP,
     &WFG, NFT, NFP, NFG, NUPR, TFP, TFG, VF, DF,
     &vCsvlineCsCer, vpCsvlineCsCer, vlngthCsCer)
     
         CALL LinklstCsCer(vCsvlineCsCer)
      END IF

              LAIPROD = PLA*PLTPOP*0.0001
              CALL Csopline(laic,lai)
              CALL Csopline(laiprodc,laiprod)
              CALL Csopline(canhtc,canht)
              CALL Csopline(gstagec,Zstage)
              !L = MAX(1,LNUMSG-INT(LLIFG))
              WRITE (NOUTPG2,503)
     A         YEAR,DOY,DAS,DAP,TMEAN,GSTAGEC,RSTAGE,
     B         LAIPRODC,SENLA*PLTPOP*0.0001,LAIC,CANHTC,SDWAD,
     &         SENW0C,SENWSC,
     &         NINT(GRNUMAD),hwudc,
     D         SHRTD,PTF,RTDEP/100.0,(RLV(I),I=1,10)
  503         FORMAT(
     A         I5,I4,2I6,F6.1,A6,F6.2,
     B         A6,F6.2,A6,A6,F6.1,2A6,I6,A6,
     D         2F6.2,F6.3,10F6.2)
             
!     VSH CSV output corresponding to PlantGr2.OUT
      IF (FMOPT == 'C') THEN
         CALL CsvOutPlGr2(EXCODE, RUN,TN,RN,SN, ON, REP, CN, YEAR,
     &DOY, DAS, DAP, TMEAN, GSTAGEC, RSTAGE, LAIPRODC, SENLA, PLTPOP, 
     &LAIC, CANHTC, SDWAD, SENW0C, SENWSC, GRNUMAD, hwudc, SHRTD, PTF,
     &RTDEP, NL, RLV,
     &vCsvlinePlGr2, vpCsvlinePlGr2, vlngthPlGr2)
     
         CALL LinklstPlGr2(vCsvlinePlGr2)
      END IF 
            
              ! Plant Growth factors outputs
              WRITE (NOUTPGF,507)
     A        YEAR,DOY,DAS,DAP,TMEAN,ZSTAGE,
     B        DU,1.0-VF,1.0-DF,1.0-TFGEM,1.0-WFGE,
     C        1.0-TFP,1.0-WFP,1.0-NFP,1.0-CO2FP,1.0-RSFP,
     D        1.0-TFG,1.0-WFG,1.0-NFG,1.0-WFT,1.0-NFT,
     H        AMIN1(99.9,WAVR),AMIN1(15.0,WUPR),H2OA,EOP,
     I        SNH4PROFILE+SNO3PROFILE,LCNF,SCNF,RCNF
  507         FORMAT(
     a        I5,I4,2I6,F6.1,F6.1,
     b        F6.1,4F6.2,
     c        5F6.2,
     d        5F6.2,
     e        2F6.2,2F6.1,F6.1,3F6.2)
 
 !    VSH CSV output corresponding to PlantGrf.OUT
      IF (FMOPT == 'C') then
       CALL CsvOutPlGrf(EXCODE, RUN,TN,RN,SN, ON, REP, CN, YEAR,DOY,
     &DAS, DAP, TMEAN, ZSTAGE, DU, VF, DF, TFGEM, WFGE, TFP, WFP, NFP, 
     &CO2FP, RSFP, TFG, WFG, NFG, WFT, NFT, WAVR, WUPR, H2OA, EOP, 
     &SNH4PROFILE, SNO3PROFILE, LCNF, SCNF, RCNF, 
     &vCsvlinePlGrf, vpCsvlinePlGrf, vlngthPlGrf)
     
         CALL LinklstPlGrf(vCsvlinePlGrf)
      END IF
 
!!             2021-02-14 chp 
!!             NUAD should be a daily variable, but here it's cumulative. 
!!             Introduce a new variable that is daily.
!              Nuptake_daily = NUAD - NUAD_Y

              ! Plant N outputs
              IF (ISWNIT.EQ.'Y') THEN
                CALL Csopline(senn0c,sennal(0))
                CALL Csopline(sennsc,sennas)
                WRITE (NOUTPN,'(
     &           I5,I4,2I6,F6.1,F6.2,F6.1,
     &           F6.1,F6.2,
     &           F6.2,
     &           3F6.1,
     &           1F6.1,3F6.2,2A6,
     &           3F6.3,
     &           3F6.3,
     &           3F6.2,
     &           F6.1,F6.2,
     &           2F6.2)')
     &           YEAR,DOY,DAS,DAP,TMEAN,ZSTAGE,NUAD,
!    &           YEAR,DOY,DAS,DAP,TMEAN,ZSTAGE,
!    &           Nuptake_daily,
     &           TNAD,SDNAD,
     &           RNAD,
     &           CNAD,LLNAD,SNAD,
     &           GNAD,HIND,RSNAD,DNAD,SENN0C,SENNSC,
     &           RANC*100.0, LANC*100.0, SANC*100.0,
     &           GRAINANC*100.0,SDNC*100.0, VANC*100.0,
     &           LCNF, SCNF, RCNF,
     &           VCNC*100.0, VMNC*100.0, AMIN1(2.0,NUPR),ANDEM

!     VSH
      IF (FMOPT == 'C') then  
         CALL CsvOutPlNCsCer(EXCODE, RUN, TN, 
     &RN, REP, YEAR, DOY, DAS, DAP, 
     &TMEAN,ZSTAGE,NUAD,TNAD,SDNAD,RNAD,CNAD,LLNAD,SNAD, GNAD, 
     &HIND,RSNAD,DNAD,SENN0C,SENNSC,RANC, LANC, SANC, GRAINANC, 
     &SDNC, VANC,LCNF, SCNF, RCNF,VCNC, VMNC, NUPR,ANDEM,  
     &vCsvlinePlNCsCer, vpCsvlinePlNCsCer, vlngthPlNCsCer)
     
         CALL LinklstPlNCsCer(vCsvlinePlNCsCer)
      END IF 

              ENDIF  ! ISWNIT = Y

!!             2021-02-14 chp 
!!             Keep cumulative value for use tomorrow.
!              NUAD_Y = NUAD

            ENDIF    ! IDETG.NE.'N'
            ENDIF    ! IDETG.NE.'N'.OR.IDETL.EQ.'0'
          ENDIF      ! MOD(FROPADJ)

          ! Harvest date or failure writes
          IF (STGDOY(11).EQ.YEARDOY .OR.
     &     DYNAMIC.EQ.SEASEND .AND. SEASENDOUT.NE.'Y') THEN
     
            IF (DYNAMIC.EQ.SEASEND) THEN
              WRITE (fnumwrk,*)' '
              WRITE (fnumwrk,'(A46,A25)')
     &         ' RUN TERMINATED PREMATURELY (PROBABLY BECAUSE ',
     &         'OF MISSING WEATHER DATA) '
            ENDIF
            
            WRITE(fnumwrk,*)' '
            WRITE(fnumwrk,'(A17,I2)')' CROP COMPONENT: ',CN
            WRITE(fnumwrk,'(A32,F8.1)')
     &       '  DEAD MATERIAL LEFT ON SURFACE  ',SENWAL(0)
            WRITE(fnumwrk,'(A32,F8.1)')
     &       '  DEAD MATERIAL LEFT IN SOIL     ',SENWAS
            WRITE(fnumwrk,'(A32,F8.1)')
     &       '  ROOT WEIGHT AT HARVEST         ',RWAD
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A20,A10,I3)')
     &       ' ROOTS BY LAYER FOR ',excode,tn
            WRITE (fnumwrk,'(A19)')
     &       '  LAYER  RTWT   RLV'
            DO L=1,NLAYR
              WRITE (fnumwrk,'(I6,F7.1,F6.2)')
     &        L,RTWTAL(L),RLV(L)
            ENDDO
            IF (RTSLXDATE.GT.0) THEN
              WRITE(fnumwrk,'(A30,I7)')
     &         '  FINAL SOIL LAYER REACHED ON ',RTSLXDATE
              WRITE(fnumwrk,'(A23,I7,A1)')
     &         '  (MATURITY/FAILURE ON ',YEARDOY,')'
            ELSE  
              WRITE(fnumwrk,*)' FINAL SOIL LAYER NOT REACHED '
            ENDIF
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A15,A10,I3)')' N BALANCE FOR ',excode,tn
            WRITE (fnumwrk,'(A25,F8.4)')'   N UPTAKE + SEED       ',
     &       NUAD+SDNAP
            WRITE (fnumwrk,'(A25,3F8.4)')'   TOTAL N SENESCED      ',
     &       SENNAL(0)+SENNAS,SENNAL(0),SENNAS
            WRITE (fnumwrk,'(A25,F8.4)')'   N IN DEAD MATTER      ',
     &       DNAD
            WRITE (fnumwrk,'(A25,F8.4)')'   TOTAL N IN PLANT      ',
     &       TNAD
            WRITE (fnumwrk,'(A25,F8.4)')'   BALANCE (A-(B+C+D))   ',
     &       NUAD+SDNAP
     &       - (SENNAL(0)+SENNAS)
     &       - TNAD
            IF (TNAD.GT.0.0 .AND.
     &       ABS(NUAD+SDNAP-(SENNAL(0)+SENNAS)-TNAD)/TNAD.GT.0.01)
     &       WRITE(fnumwrk,'(A26,A10,A1,I2)')
     &       '   PROBLEM WITH N BALANCE ',EXCODE,' ',TN
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A18,A10,I3)')' CH2O BALANCE FOR ',excode,tn
            WRITE (fnumwrk,'(A27, F11.4)')'   SEED + CH2O FIXED A     ',
     &       SDRATE+CARBOAC
            WRITE (fnumwrk,'(A27, F11.4)')'   CH2O RESPIRED B         ',
     &       RESPAC
     
            WRITE (fnumwrk,'(A27,3F11.4)')'   CH2O SENESCED C  Tops,rt',
     &       SENWAL(0)+SENWAS,SENWAL(0),SENWAS                          
            WRITE (fnumwrk,'(A27,F11.4)') '   CH2O LF RESERVES LOST C2',
     &       SENRSC*10.0*PLTPOP
            WRITE (fnumwrk,'(A27, F11.4)')'   CH2O IN LIVE+DEAD D     ',
     &       TWAD
            WRITE (fnumwrk,'(A27, F11.4)')'   CH2O IN DEAD MATTER     ',
     &       DWAD
            WRITE (fnumwrk,'(A27, F11.4)')'   CH2O IN LIVE PLANT      ',
     &       TWAD-DWAD
            WRITE (fnumwrk,'(A27, F11.4)')'   POST MATURITY RESERVES E',
     &       RSWADPM
            WRITE (fnumwrk,'(A27, F11.4)')'   BALANCE (A-(B+C+C2+D+E))',
     &         SDRATE+CARBOAC-RESPAC-(SENWAL(0)+SENWAS)
     &       - TWAD-RSWADPM-(SENRSC*10.0*PLTPOP)
            IF (TWAD.GT.0.0 .AND.
     &       ABS(SDRATE+CARBOAC-RESPAC-(SENWAL(0)+SENWAS)
     &       - TWAD-RSWADPM-(SENRSC*10.0*PLTPOP)     )
     &       /TWAD .GT. 0.01)
     &       WRITE(fnumwrk,'(A29,A10,A1,I2)')
     &       '   PROBLEM WITH CH2O BALANCE ',EXCODE,' ',TN

            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A22,A10,I3)')
     &       ' STAGE CONDITIONS FOR ',excode,tn
            WRITE (fnumwrk,'(A38,F6.1)')
     &       '  Temperature mean,germ+emergence      ',GETMEAN
            WRITE (fnumwrk,'(A38,F6.1)')
     &       '  Temperature mean,first 20 days       ',TMEAN20P
            WRITE (fnumwrk,'(A38,F6.1)')
     &       '  Temperature mean,20d around anthesis ',TMEAN20A
            WRITE (fnumwrk,'(A38,F6.1)')
     &       '  Solar radn. mean,20d around anthesis ',SRAD20A
            WRITE (fnumwrk,'(A38,F6.1)')
     &       '  Stress fac. mean,20d around anthesis ',STRESS20A
            WRITE (fnumwrk,'(A38,F6.1)')
     &       '  Temperature mean,grain filling       ',GFTMEAN
            WRITE (fnumwrk,'(A38,F6.1)')
     &       '  Temperature mean,grain maturing      ',GMTMEAN
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A17,A10,I3)')' STAGE DATES FOR ',excode,tn
            WRITE (fnumwrk,'(A26)')
     &       '  STAGE   DATE  STAGE NAME'
            DO I = 1, 11
              WRITE (fnumwrk,'(I7,I8,A1,A10)')
     &               I,STGDOY(I),' ',STNAME(I)
            ENDDO
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A27,A10,I3)')
     &       ' LEAF NUMBER AND SIZES FOR ',excode,tn
            WRITE (fnumwrk,'(A15,F4.1)') '   LEAF NUMBER ',LNUMSD
            WRITE (fnumwrk,'(A55)')
     &       '   LEAF AREAP AREA1 AREAT AREAS TNUML  WFLF  NFLF  AFLF'
            IF (LNUMSG.GT.0) THEN
              DO I = 1, LNUMSG
                WRITE (fnumwrk,'(I7,8F6.1)')
     &           I,LAPOT(I),LATL(1,I),LAP(I),LAPS(I),TNUML(I),
     &            1.0-WFLF(I),1.0-NFLF(I),1.0-AFLF(I)
              ENDDO
            ELSE
              WRITE (fnumwrk,*) ' Leaf number < 1!'
            ENDIF
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A28,A10,I3)')
     &       ' STRESS FACTOR AVERAGES FOR ',excode,tn
            WRITE (fnumwrk,'(A55)')
     &       '  PHASE  H2O(PS)   H2O(GR)   N(PS)     N(GR)  PHASE END'
            DO tvi1=1,5
              WRITE (fnumwrk,'(I6,F8.2,3F10.2,2X,A10)')
     &        tvi1,1.0-wfpav(tvi1),1.0-wfgav(tvi1),
     &        1.0-nfpav(tvi1),1.0-nfgav(tvi1),stname(tvi1)
            ENDDO
            WRITE (fnumwrk,'(A42)')
     &       '  NB 0.0 = minimum ; 1.0 = maximum stress.'
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A22,A10,I3)')
     &       ' RESERVES STATUS FOR ',excode,tn
            WRITE (fnumwrk,'(A20,I6)')'  Kg/ha at anthesis ',NINT(RSWAA)
            WRITE (fnumwrk,'(A20,I6)')'  Kg/ha at maturity ',NINT(RSWAD)
            IF (cwaa.GT.0) WRITE (fnumwrk,'(A20,F6.1)')
     &       '  % at anthesis     ',rsca*100.0
            IF (lfwt+stwt+rswt.GT.0) WRITE (fnumwrk,'(A20,F6.1)')
     &       '  % at maturity     ',rswt/(lfwt+stwt+rswt)*100.0
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A20,F6.2)')'  Reserves coeff    ',RSPCS
            WRITE (fnumwrk,'(A20,F6.2)')'  Stem gr end stage ',P4SGE
            WRITE (fnumwrk,'(A20,F6.2)')
     &       '  Anthesis stage    ',(4.0+PD4(1)/PD(4))
            WRITE (fnumwrk,*) ' '
            IF (grnum.GT.0.0) WRITE (fnumwrk,'(A20,F6.1)')
     &       '  Grain weight mg   ',GRWT/GRNUM*1000.0
            WRITE (fnumwrk,'(A20,F6.1)')
     &       '  Grain weight coeff',g2kwt
            IF (GRNUM.GT.0.0.AND.G2KWT-GRWT/GRNUM*1000.0.GT.0.1) THEN
              WRITE (fnumwrk,'(A34)')
     &         '  Some limitation on grain growth!'
              WRITE(fnumwrk,'(A22,I4)')'   Days of Ch2o limit ',ch2olim
              WRITE(fnumwrk,'(A22,I4)')'   Days of N limit    ',nlimit
              WRITE(fnumwrk,'(A22,I4)')'   Days of temp limit ',tlimit
            ENDIF
            IF (grwt.GT.0.0) WRITE (fnumwrk,'(A20,F6.1)')
     &       '  Grain N %         ',grainn/grwt*100.0
            WRITE (fnumwrk,'(A20,F6.1)')
     &       '  Minimum grain N % ',grnmn
            WRITE (fnumwrk,'(A20,F6.1)')
     &       '  Standard grain N %',grns
     
            ! BEGIN MAIN OUTPUTS 
            
            CR = CROP
            GWAM = GWAD
            PWAM = GWAD + CHWAD
            GWUM = GWUD
            NUAM = NUAD
            HNUMAM = GRNUMAD
            HIAM = HIAD
            LNUMSM = LNUMSD
            TNUMAM = TNUMAD
            SENWATC = SENWAL(0) + SENWAS
            CWAM = CWAD
            VWAM = VWAD
            CNAM = CNAD
            VNAM = VNAD
            SENNATC = SENNAL(0) + SENNAS
            GNAM = GNAD
            RWAM = RWAD
            RNAM = RNAD
            RSWAM = RSWAD
            HINM = HIND
            GNPCM = GRAINANC *100.0
            VNPCM = VANC * 100.0 
            VNAA = CNAA
            
            IF (tnumam.GT.0.0) hnumgm = hnumam/tnumam
            gwgm = gwum*1000.0    ! mg

            ! Check that -99 not multiplied or divided 
            IF (hnumgm.LT.0.0) hnumgm = -99
            IF (hnumam.LT.0.0) hnumam = -99
            IF (hnumgmm.LT.0.0) hnumgmm = -99
            IF (hnumamm.LT.0.0) hnumamm = -99

            ! Put N variables to -99 if N switched off
            IF (ISWNIT.EQ.'N') THEN
              gnpcm = -99
              vnpcm = -99
              cnam = -99
              gnam = -99
              hinm = -99
              sdnap = -99
              rnam = -99
              nuam = -99
            ENDIF
            
            WRITE (fnumwrk,*) ' '

            IF (DYNAMIC.EQ.SEASEND) THEN
            
              WRITE(fnumwrk,*)  'WRITING END OF RUN OUTPUTS     '

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
              !    0 switches everything to N apart from IDETS,which->Y,      
              !      and IDETO,which->E when RNMODE is not N (seasonal)
              !    A switches ALL outputs on  
              
              laix = -99.0
              gwam = -99.0
              gwum = -99.0
              hnumam = -99.0
              hnumgm = -99.0
              hiam = -99.0
              lnumsm = -99.0
              tnumam = -99.0
              cwam = -99.0
              vwam = -99.0
              rwam = -99.0
              carboac = -99.0
              senwatc = -99.0
              rswam = -99.0
              sennatc = -99.0
              cnam = -99.0
              rnam = -99.0
              vnam = -99.0
              gnam = -99.0
              hinm = -99.0
              gnpcm = -99.0
              cwaa = -99.0
              vnaa = -99.0
              lnpca = -99.0
              stress20a = -99.0
              mday = -99
              hayear = -99
              haday = -99
            ELSE 
              WRITE(fnumwrk,*)  'WRITING HARVEST DAY OUTPUTS         '
            ENDIF  
            
            IF (STEP.NE.1) THEN
              WRITE (fnumwrk,*) ' '
              WRITE (fnumwrk,*) ' Step number greater than 1!'
              WRITE (fnumwrk,*) ' Not set up for hourly runs!'
              WRITE (fnumwrk,*) ' Will skip final outputs.'
              GO TO 8888
            ENDIF
            WRITE(fnumwrk,*)
     &       ' Harvest percentage (Technology coeff) ',hpc
            
            CNCHAR = ' '
            CNCHAR2 = '  '
            IF (CN.EQ.1) THEN
              OUT = 'OUT'
              CNCHAR2= '1 '
            ELSE
              CNCHAR = TL10FROMI(CN)
              OUT = 'OU'//CNCHAR(1:1)
              CNCHAR2(1:1) = CNCHAR(1:1)
            ENDIF
            
!            CALL CSYR_DOY (STGDOY(7),PLYEAR,PLDAY)
            CALL YR_DOY (STGDOY(7),PLYEAR,PLDAY)
            IF (ADAT.GT.0) THEN
!              CALL CSYR_DOY (ADAT,AYEAR,ADAY)
              CALL YR_DOY (ADAT,AYEAR,ADAY)
            ELSE
              AYEAR = -99
              ADAY = -99
            ENDIF
            IF (STGDOY(5).GT.0) THEN
!              CALL CSYR_DOY (STGDOY(5),MYEAR,MDAY)
              CALL YR_DOY (STGDOY(5),MYEAR,MDAY)
            ELSE
              MYEAR = -99
              MDAY = -99
            ENDIF
!            CALL CSYR_DOY (STGDOY(11),HAYEAR,HADAY)
            CALL YR_DOY (STGDOY(11),HAYEAR,HADAY)
            
!-----------------------------------------------------------------------
            
            ! SCREEN OUTPUTS (CROPSIM)
            
            IF (FILEIOT(1:3).EQ.'XFL'.AND.CN.EQ.1.AND.RNMODE.NE.'T')THEN
            
              IF (OUTCOUNT.LE.0 .OR. OUTCOUNT.EQ.25) THEN
                WRITE (*,499)
  499           FORMAT ('   RUN EXCODE      TN RN',
     X           ' TNAME..................',
     X           '.. REP  RUNI S O C CR  GWAM')
              ENDIF
              IF (OUTCOUNT .EQ. 25) THEN
                OUTCOUNT = 1
              ELSE
                OUTCOUNT = OUTCOUNT + 1
              ENDIF
              WRITE (*,410) run,excode,tn,rn,tname(1:25),
     X        rep,runi,sn,on,cn,cr,NINT(gwam)
  410         FORMAT (I6,1X,A10,I4,I3,1X,A25,
     X        I4,I6,I2,I2,I2,1X,A2,I6)
            ENDIF
            
            ! END OF SCREEN OUTPUTS
            
!-----------------------------------------------------------------------
            
            ! PLANT SUMMARY
           
            IF ((IDETS.NE.'N'.AND.IDETL.NE.'0').OR.IDETL.EQ.'A') THEN

              WRITE (fnumwrk,*) ' '                       
              WRITE (fnumwrk,*) 'Writing PLANT SUMMARY'
              
              FNAMETMP = ' '
              FNAMETMP(1:12) = 'Plantsum.'//out
              
              IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
               OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,STATUS = 'UNKNOWN')
               WRITE (FNUMTMP,'(A14)') '$PLANT SUMMARY'
               WRITE (FNUMTMP,*) ' '
               WRITE (fnumtmp,96)
               WRITE (fnumtmp,99)
   96          FORMAT ('*PLANTSUM',/)
   99          FORMAT ('@  RUN EXCODE    TRNO RN ',                     
     X         'TNAME..................',
     X         '.. REP  RUNI S O C    CR',
     X         ' PYEAR  PDOY  TSAP  ADAP  MDAP   FLN FLDAP',
     x         ' HYEAR  HDAY SDWAP CWAHC  CWAM',
     X         ' PARUE',
     X         '  HWAM  HWAH  BWAH  HWUM  H#AM  H#UM',
     x         ' SDNAP  CNAM  HNAM  RNAM  TNAM  NUAM  HN%M  VN%M',
     E         ' D1INI D2INI D3INI ')
               CLOSE(fnumtmp)
              ENDIF
              
              OPEN (UNIT=fnumtmp,FILE=FNAMETMP,POSITION='APPEND')

              IF (tsdat.GT.0) THEN 
                tsdap = Dapcalc(tsdat,plyear,plday)
              ELSE
                tsdap = -99
              ENDIF  
              IF (lldat.GT.0) THEN 
                lldap = Dapcalc(lldat,plyear,plday)
              ELSE
                lldap = -99
              ENDIF  
              IF (adat.LT.9999999) THEN 
                adap = Dapcalc(adat,plyear,plday)
              ELSE
                adap = -99
              ENDIF  
              IF (stgdoy(5).LT.9999999) THEN 
                 mdap = Dapcalc(stgdoy(5),plyear,plday)
              ELSE 
                mdap = -99
                stgdoy(5) = -99
              ENDIF     
              
              WRITE (fnumtmp,400) run,excode,tn,rn,tname(1:25),
     X          rep,runi,sn,on,cn,cr,
     X          plyear,plday,tsdap,adap,mdap,
     x          flnmodel,lldap,hayear,haday,
     X          NINT(sdrate),
     X          0,NINT(cwam),pariuem,NINT(gwam),
     X          NINT(gwam*hpc/100.0),NINT(cwam-gwam),
     X          gwgm/1000.0,NINT(hnumam),NINT(hnumgm),
     X          sdnap,NINT(cnam),NINT(gnam),NINT(rnam),
     X          NINT(AMAX1(-99.0,cnam+rnam)),NINT(nuad),
     X          gnpcm,vnpcm,'   -99','   -99','   -99'
  400         FORMAT (I6,1X,A10,I4,I3,1X,A25,
     X         I4,I6,I2,I2,I2,4X,A2,
     X         I6,I6,I6,I6,I6,F6.1,I6,I6,I6,I6,
     X         I6,
     X         I6,F6.1,I6,
     X         I6,I6,
     X         1X,F5.3,I6,I6,
     X         F6.1,I6,I6,I6,
     X         I6,I6,
     X         F6.2,F6.2,
     X         3A6)
              
              CLOSE(fnumtmp)       
             
            ELSE
            
               OPEN (UNIT=FNUMTMP,FILE='Plantsum.out',STATUS='UNKNOWN')
               CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
               
            ENDIF  ! END OF PLANT SUMMARY
            
!-----------------------------------------------------------------------            
            
            ! LEAVES
           
            IF (IDETL.EQ.'Y'.OR.IDETL.EQ.'A') THEN

              IF (CN.EQ.1) THEN
                IF (FNUMLVS.LE.0.OR.FNUMLVS.GT.1000) THEN
                  CALL Getlun ('Leaves.OUT',fnumlvs)
                ENDIF
                IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
                 OPEN(UNIT=FNUMLVS,FILE='Leaves.OUT',STATUS='UNKNOWN')
                 WRITE (FNUMLVS,'(A11)') '$LEAF SIZES'
                 CLOSE(FNUMLVS)
                ENDIF
                OPEN(UNIT=FNUMLVS,FILE='Leaves.OUT',POSITION='APPEND')
                WRITE (FNUMLVS,'(/,A77,/)') OUTHED
                WRITE (FNUMLVS,'(A14,F6.1)') '! LEAF NUMBER ',LNUMSD
                IF (LNSWITCH.GT.0.0) THEN
                  WRITE(FNUMLVS,'(A42,F6.1)')
     &             '! LEAF NUMBER WHEN INCREASE FACTOR CHANGED',lnswitch
                  WRITE(FNUMLVS,'(A35,F6.2)')
     &             '! AREA OF LEAF WHEN FACTOR CHANGED  ',laswitch
                ENDIF     
                WRITE (FNUMLVS,'(/,A42,A30)')
     &          '@ LNUM AREAP AREA1 AREAT AREAS  T#PL  T#AL',
     &          '  WFLF  NFLF  AFLF  TFLF DAYSG'
                IF (LNUMSG.GT.0) THEN
                  DO I = 1, LNUMSG
                    WRITE (fnumlvs,'(I6,5F6.1,I6,5F6.1)')
     &               I,LAPOT(I),LATL(1,I),LAP(I),LAPS(I),
     &               TNUML(I),NINT(TNUML(I)*PLTPOP),
     &               1.0-WFLF(I),1.0-NFLF(I),1.0-AFLF(I),1.0-TFLF(I),
     &               WFLFNUM(I)
                  ENDDO
                ENDIF
                IF (run.EQ.1.AND.runi.EQ.1) THEN
                  WRITE(fnumlvs,*)' '
                  WRITE(fnumlvs,'(A37)')
     &             '! LNUM  = Number of leaf on main axis'
                  WRITE(fnumlvs,'(A38)')
     &             '! AREAP = Potential area of leaf (cm2)'
                  WRITE(fnumlvs,'(A41)')
     &             '! AREA1 = Area of leaf on main axis (cm2)'
                  WRITE(fnumlvs,'(A44,A16)')
     &             '! AREAT = Area of leaves on all axes at leaf',
     &             ' position (cm2) '
                  WRITE(fnumlvs,'(A50,A6)')
     &             '! AREAS = Area of leaves senesced at leaf position',
     &             ' (cm2)'
                  WRITE(fnumlvs,'(A46)')
     &             '! T#PL  = Tiller number/plant at leaf position'
                  WRITE(fnumlvs,'(A49)')
     &             '! T#AL  = Tiller number/area(m2) at leaf position'
                  WRITE(fnumlvs,'(A38,A17)')
     &             '! WFLF  = Water stress factor for leaf',
     &             ' (0-1,0=0 stress)'
                  WRITE(fnumlvs,'(A51)')
     &             '! NFLF  = N stress factor for leaf (0-1,0=0 stress)'
                  WRITE(fnumlvs,'(A36,A24)')
     &             '! AFLF  = Assimilate factor for leaf',
     &             ' (0-1,0=0 no limitation)'
                  WRITE(fnumlvs,'(A37,A24)')
     &              '! TFLF  = Temperature factor for leaf',
     &              ' (0-1,1=0 no limitation)'
                  WRITE(fnumlvs,'(A37)')
     &              '! DAYSG = Number of days of growth   '
                ENDIF
                CLOSE (FNUMLVS)
              ENDIF

            ELSE
            
               OPEN (UNIT=FNUMTMP,FILE='Leaves.OUT',STATUS='UNKNOWN')
               CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
               
            ENDIF  ! END OF LEAVES
            
!-----------------------------------------------------------------------
            
            ! A-FILE READS
            
            adatm = -99
            adatt = -99
            adayh = -99
            carboacm = -99
            cnaam = -99
            cnamm = -99
            cwadt = -99
            cwamm = -99
            drdatm = -99
            edatm = -99
            gnamm = -99
            gnpcmm = -99
            gstdm = -99
            gwadt = -99
            hiadt = -99
            hiamm = -99
            hinmm = -99
            hnumamm = -99
            hnumat = -99
            hnumet = -99
            hnumgmm = -99
            hwadm = -99
            hwahm = -99
            gwamm = -99
            gwut = -99
            gwumm = -99
            jdatm = -99
            laixm = -99
            lnaam = -99
            lnpcam = -99
            lnumsmm = -99
            lnumt = -99
            lwaam = -99
            mdatm = -99
            mdatt = -99
            nuamm = -99
            rnamm = -99
            rswamm = -99
            rwamm = -99
            sennatcm = -99
            senwatcm = -99
            tnumamm = -99
            tnumt = -99
            tsdatm = -99
            vnamm = -99
            vnpcmm = -99
            vwamm = -99                       
            cwaam = -99
            
            CALL LTRIM2 (FILEIO,filenew)
            FILELEN = TVILENT(FILENEW)
            ! Jed Greene, Citadel - keep index from neg
            FILELEN = MAX(FILELEN-12, 0) 
            
            IF (TVILENT(FILEADIR).GT.3) THEN
              IF (FILEADIR(TVILENT(FILEADIR):
     &            TVILENT(FILEADIR)).NE.SLASH)THEN
                FILEA = FILEADIR(1:TVILENT(FILEADIR))//
     &           SLASH //EXCODE(1:8)//'.'//EXCODE(9:10)//'A'
              ELSE
                FILEA = FILEADIR(1:TVILENT(FILEADIR))//
     &           EXCODE(1:8)//'.'//EXCODE(9:10)//'A'
              ENDIF
            ELSE
              FILEA = FILENEW(1:FILELEN-12)//EXCODE(1:8)//'.'//
     &         EXCODE(9:10)//'A'
            ENDIF       
            FEXISTA = .FALSE.
            INQUIRE (FILE = FILEA,EXIST = FEXISTA)
            IF (.not.FEXISTA) THEN
              WRITE (fnumwrk,*) 'A-file not found!'
            ELSE
              WRITE (fnumwrk,*) 'A-file found: ',filea(1:60)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'GWAM',gwamm)
              IF (gwamm.LE.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HWAM',gwamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HWAH',hwahm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HWUM',gwumm)
              IF (gwumm.le.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'GWUM',gwumm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'LAIX',laixm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CWAM',cwamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'BWAH',vwamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CWAA',cwaam)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'T#AM',tnumamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'H#AM',hnumamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'H#UM',hnumgmm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'L#SM',lnumsmm)
            
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CNAM',cnamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'VNAM',vnamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CNAA',cnaam)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'GNAM',gnamm)
              IF (gnamm.LE.0.0) 
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HNAM',gnamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'LN%A',lnpcam)
              IF (lnpcam.LE.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'LNCA',lnpcam)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'GN%M',gnpcmm)
              IF (gnpcmm.LE.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'GNCM',gnpcmm)
              IF (gnpcmm.LE.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HN%M',gnpcmm)
              IF (gnpcmm.LE.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HNCM',gnpcmm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'VN%M',vnpcmm)
              IF (vnpcmm.le.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'VN%D',vnpcmm)
              IF (vnpcmm.le.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'VNCD',vnpcmm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'L#SM',lnumsmm)
              IF (lnumsmm.le.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'LNOSM',lnumsmm)
            
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HIAM',hiamm)
              IF (HIAMM.GE.1.0) HIAMM = HIAMM/100.0
            
              CALL AREADI (FILEA,TN,RN,SN,ON,CN,'EDAT',edatm)
              CALL AREADI (FILEA,TN,RN,SN,ON,CN,'DRDAT',drdatm)
              CALL AREADI (FILEA,TN,RN,SN,ON,CN,'TSDAT',tsdatm)
              CALL AREADI (FILEA,TN,RN,SN,ON,CN,'A1DAT',a1datm)
              CALL AREADI (FILEA,TN,RN,SN,ON,CN,'LLDAT',lldatm)
              CALL AREADI (FILEA,TN,RN,SN,ON,CN,'SPDAT',spdatm)
              CALL AREADI (FILEA,TN,RN,SN,ON,CN,'ADAT',adatm)
              CALL AREADI (FILEA,TN,RN,SN,ON,CN,'JDAT',jdatm)
              IF (ADATM.LE.0) THEN
                CALL AREADI (FILEA,TN,RN,SN,ON,CN,'GS059',adatm)
                IF (ADATM.GT.0) THEN
                  ADATM = ADATM + 2
                  WRITE (fnumwrk,*) 'WARNING  ADAT = GS059 + 2'
                ENDIF
              ENDIF
              IF (ADATM.LE.0) THEN
                CALL AREADI (FILEA,TN,RN,SN,ON,CN,'ADAY',adaym)
                CALL AREADI (FILEA,TN,RN,SN,ON,CN,'YEAR',ayearm)
                ADATM = CSYDOY(AYEARM,ADAYM)
              ENDIF
              CALL AREADI (FILEA,TN,RN,SN,ON,CN,'MDAT',mdatm)
              IF (MDATM.LE.0) THEN
                CALL AREADI (FILEA,TN,RN,SN,ON,CN,'MDAY',mdaym)
                CALL AREADI (FILEA,TN,RN,SN,ON,CN,'YEAR',myearm)
                MDATM = CSYDOY(MYEARM,MDAYM)
              ENDIF
            ENDIF
            
            ! If nothing in A-file,use X-file
            IF (EDATM.LE.0) edatm = emdatm   
            
            ! END OF A-FILE READS
            
!-----------------------------------------------------------------------
            
            ! CHECK DATA AND USE EQUIVALENTS IF NECESSARY
            
            ! Product wt at maturity
            IF (hwahm.GT.0 .AND. gwamm.LE.0) gwamm = hwahm/(hpc/100.0)
            
            ! Product wt at harvest
            IF (gwamm.GT.0 .AND. hwahm.LE.0) hwahm = gwamm*(hpc/100.0)
            
            ! Canopy wt at maturity
            IF (vwamm.GT.0 .AND. gwamm.GT.0) cwamm = vwamm+gwamm
            
            ! Vegetative wt at maturity
            IF (gwamm.GT.0 .AND. cwamm.GT.0) vwamm = cwamm-gwamm
            
            ! Harvest index at maturity
            IF (hiamm.LE.0.0) THEN
              IF (cwamm.GT.0 .AND. gwamm.GT.0) hiamm = gwamm/cwamm
            ELSE
              IF (cwamm.GT.0 .AND. gwamm.GT.0) THEN
                hiammtmp = gwamm/cwamm
                IF (hiammtmp/hiam.GT.1.1 .OR. hiammtmp/hiam.LT.0.9) THEN
                  IF (ABS(hiammtmp-hiamm)/hiamm.GT.0.05) THEN
                    WRITE (fnumwrk,*) 'Reported HI not consistent',
     &               ' with yield and total weight data!!'
                    WRITE (fnumwrk,*) ' Reported HI   ',hiamm
                    WRITE (fnumwrk,*) ' Calculated HI ',hiammtmp
                    WRITE (fnumwrk,*) ' Will use reported value '
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
            
            ! Product unit wt at maturity
            IF (gwumm.GT.1.0) gwumm = gwumm/1000.0 ! mg->g
            IF (gwumm.LE.0 .AND. hnumamm.GT.0) THEN
              IF (gwamm.GT.0.0) gwumm=gwamm*0.1/hnumamm  ! kg->g
            ELSE
              IF (gwamm.gt.0.0.AND.hnumamm.GT.0.0) THEN
                gwumyld = gwamm*0.1/hnumamm
                IF (ABS(gwumyld-gwumm)/gwumm.GT.0.05) THEN
                  WRITE (fnumwrk,*) 'Reported kernel wt.not consistent',
     &            ' with yield and kernel # data!!'
                  WRITE (fnumwrk,*) ' Reported wt   ',gwumm
                  WRITE (fnumwrk,*) ' Calculated wt ',gwumyld
                  WRITE (fnumwrk,*) '   Yield       ',gwamm
                  WRITE (fnumwrk,*) '   Kernel #    ',hnumamm
                  WRITE (fnumwrk,*) ' Will use reported value '
                ENDIF
              ENDIF
            ENDIF
            gwgmm = gwumm*1000.0  ! mg
            
            ! Product number at maturity
            IF (HNUMAMM.LE..0.AND.HNUMGMM.GT..0.AND.TNUMAMM.GT..0) THEN
             HNUMAMM = HNUMGMM * TNUMAMM
             WRITE(fnumwrk,*)'Tiller # * grains/tiller used for HNUMAMM'
            ENDIF
            IF (hnumgmm.LE.0. AND. tnumamm.GT.0 .AND. hnumamm.GT.0) THEN
              hnumgmm = hnumamm/tnumamm
              WRITE(fnumwrk,*)'Grains/area / tiller # used for HNUMGMM'
            ENDIF
            
            ! Tiller number at maturity
            IF (tnumamm.LE.0 .AND. hnumamm.GT.0. AND. hnumgmm.GT.0)
     &         tnumamm = hnumamm/hnumgmm
            
            ! Canopy N at maturity
            IF (vnamm.GT.0 .AND. gnamm.GT.0 .AND. cnamm.LE.0)
     &        cnamm = vnamm + gnamm
            
            ! Vegetative N at maturity
            IF (vnamm.LE.0) THEN
             IF (gnamm.GE.0 .AND. cnamm.GT.0) vnamm=cnamm-gnamm
            ENDIF
            
            ! Product N harvest index at maturity
            IF (cnamm.GT.0 .AND. gnamm.GT.0) hinmm=gnamm/cnamm
            
            ! Vegetative N concentration at maturity
            IF (vnpcmm.LE.0) THEN
             IF (vwamm.GT.0 .AND. vnamm.GT.0) vnpcmm = (vnamm/vwamm)*100
            ENDIF
            
            ! Product N concentration at maturity
            IF (gnpcmm.LE.0) THEN
             IF (gwamm.GT.0 .AND. gnamm.GT.0) gnpcmm = (gnamm/gwamm)*100
            ENDIF
            
            ! Leaf N concentration at maturity
            IF (lnpcam.LE.0 .AND. lnaam.GT.0 .AND. lwaam.GT.0.0)
     &        lnpcam = lnaam/lwaam
     
            ! END OF CHECKING DATA

!-----------------------------------------------------------------------

            ! T-FILE READS AND MEASURED.OUT WRITES
            
            IF ((IDETG.NE.'N'.AND.IDETL.NE.'0').OR.
     &           IDETL.EQ.'D'.OR.IDETL.EQ.'A') THEN
            
              ! T-FILE READS AND MEASURED.OUT WRITES
            
              WRITE (fnumwrk,*)
     &         'Trying to read T-file and write MEASURED.OUT'
            
              Fnametmp = ' '
              Fnametmp(1:12) = 'Measured.OUT'
              IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
               CALL Getlun ('FILET',FNUMT)
               OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,STATUS = 'UNKNOWN')
               WRITE (FNUMTMP,'(A22)') '$TIME_COURSE(MEASURED)'
               CLOSE(FNUMTMP)
              ENDIF
            
              STARNUMO = STARNUMO + 1  ! # datasets in sim output file
              
              IF (TVILENT(FILEADIR).GT.3) THEN
                IF (FILEADIR(TVILENT(FILEADIR):
     &            TVILENT(FILEADIR)).NE.SLASH)THEN
                  FILET = FILEADIR(1:TVILENT(FILEADIR))//
     &           SLASH //EXCODE(1:8)//'.'//EXCODE(9:10)//'T'
                ELSE
                  FILET = FILEADIR(1:TVILENT(FILEADIR))//
     &           EXCODE(1:8)//'.'//EXCODE(9:10)//'T'
                ENDIF
              ELSE
                FILET = FILENEW(1:FILELEN-12)//EXCODE(1:8)//'.'//
     &          EXCODE(9:10)//'T'
              ENDIF       
              FEXISTT  = .FALSE.
              INQUIRE (FILE = FILET,EXIST = FEXISTT)
            
              CFLTFILE = 'N'
              LAIXT = -99.0
              VALUER = -99.0
            
              IF (.not.FEXISTT) THEN
                WRITE (fnumwrk,*) 'T-file not found: ',filet(1:60)
              ELSE
                WRITE (fnumwrk,*) 'T-file found: ',filet(1:60)
                TLINENUM = 0
                OPEN (UNIT = FNUMT,FILE = FILET)
                OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,POSITION ='APPEND')
                COLNUM = 1
                L1 = 0
                DO
8453              CONTINUE                    
                  READ(FNUMT,'(A180)',END = 5555)LINET
                  IF (LINET(1:1).EQ.'!') GO TO 8453
                  TLINENUM = TLINENUM + 1  ! Only to check if file empty
                  IF (LINET(1:5).EQ.'*FLAG'.OR.LINET(1:5).EQ.'*CODE'.OR.
     &                LINET(1:5).EQ.'*GENE') THEN
                    DO
8454                  CONTINUE                    
                      READ(FNUMT,'(A180)',END = 5555)LINET
                      IF (LINET(1:1).EQ.'!') GO TO 8454
                      IF (LINET(1:1).EQ.'*') THEN
                        IF (LINET(1:7).EQ.'*DATA(T') EXIT
                        IF (LINET(1:7).EQ.'*EXP.DA') EXIT
                        IF (LINET(1:7).EQ.'*EXP. D') EXIT
                        IF (LINET(1:7).EQ.'*AVERAG') EXIT
                        IF (LINET(1:7).EQ.'*TIME_C') EXIT
                      ENDIF
                    ENDDO
                  ENDIF
                  L1 = 0
                  L2 = 0
                  IF (LINET(1:7).EQ.'*DATA(T' .OR.
     &             LINET(1:7).EQ.'*EXP.DA' .OR.
     &             LINET(1:7).EQ.'*EXP. D' .OR.
     &             LINET(1:7).EQ.'*TIME_C' .OR.
     &             LINET(1:7).EQ.'*AVERAG') THEN
                    TNCHAR = TL10FROMI(TN)
                    LENLINE = LEN(TRIM(LINET))
                    IF(LINET(1:7).EQ.'*EXP.DA'.OR.
     &                LINET(1:7).EQ.'*EXP. D')THEN
                      GROUP = 'A'
                      DO L = 1,30
                        IF (LINET(L:L+1).EQ.': ') L1 = L+2
                        IF (LINET(L:L).EQ.':' .AND. 
     &                    LINET(L+1:L+1).NE.' ')
     &                    L1 = L+1
                        IF (L1.GT.0.AND.L.GT.L1+9.AND.
     &                   LINET(L:L).NE.' ') THEN
                          L2 = L  ! Start of group information in t-file
                          EXIT
                        ENDIF
                      ENDDO
                      CALL LTRIM (TNAME)
                      LENTNAME = MIN(15,LEN(TRIM(TNAME)))
                      LENGROUP = MIN(L2+14,LENLINE)
                      IF (LEN(TRIM(TNCHAR)).EQ.1) THEN
                        LINESTAR = '*DATA(T):'//LINET(L1:L1+9)//
     &                  ' '//TNCHAR(1:1)//' C'//
     &                  CNCHAR2//TNAME(1:LENTNAME)//
     &                  ' '//LINET(L2:LENGROUP)
                      ELSEIF (LEN(TRIM(TNCHAR)).EQ.2) THEN              
                        LINESTAR = '*DATA(T):'//LINET(L1:L1+9)//
     &                  ' '//TNCHAR(1:2)//' C'//CNCHAR2//
     &                  TNAME(1:LENTNAME)//
     &                  ' '//LINET(L2:LENGROUP)
                      ELSEIF (LEN(TRIM(TNCHAR)).EQ.3) THEN
                        LINESTAR = '*DATA(T):'//LINET(L1:L1+9)//
     &                  ' '//TNCHAR(1:3)//' C'//CNCHAR2//
     &                  TNAME(1:LENTNAME)//
     &                  ' '//LINET(L2:LENGROUP)
                      ENDIF
                    ENDIF
                    IF (LINET(1:7).EQ.'*DATA(T') THEN
                      GROUP = 'D'
                      DO L = 1,30
                        IF (LINET(L:L).EQ.' ') L1 = L-1
                        IF (L1.NE.0 .AND. LINET(L:L).NE.' ') THEN
                          L2 = L  ! Start of group information in t-file
                          EXIT
                        ENDIF
                      ENDDO
                      CALL LTRIM (TNAME)
                      LENTNAME = MIN(15,LEN(TRIM(TNAME)))
                      LENGROUP = MIN(L2+14,LENLINE)
                      IF (LEN(TRIM(TNCHAR)).EQ.1) THEN
                        LINESTAR = LINET(1:L1)//' TN:'//
     &                  TNCHAR(1:1)//' C'//CNCHAR2//TNAME(1:LENTNAME)//
     &                  ' '//LINET(L2:LENGROUP)
                      ELSEIF (LEN(TRIM(TNCHAR)).EQ.2) THEN
                        LINESTAR = LINET(1:L1)//' TN:'//
     &                  TNCHAR(1:2)//' C'//CNCHAR2//TNAME(1:LENTNAME)//
     &                  ' '//LINET(L2:LENGROUP)
                      ELSEIF (LEN(TRIM(TNCHAR)).EQ.3) THEN
                        LINESTAR = LINET(1:L1)//' TN:'//
     &                  TNCHAR(1:3)//' C'//CNCHAR2//TNAME(1:LENTNAME)//
     &                  ' '//LINET(L2:LENGROUP)
                      ENDIF
                    ENDIF
                    IF (LINET(1:7).EQ.'*AVERAG' .OR.
     &                  LINET(1:7).EQ.'*TIME_C') THEN
                      GROUP = 'A'
                      DO L = 1,30
                        IF (LINET(L:L).EQ.' ') L1 = L-1
                        IF (L1.NE.0 .AND. LINET(L:L).NE.' ') THEN
                          L2 = L  ! Start of group information in t-file
                          EXIT
                        ENDIF
                      ENDDO
                      CALL LTRIM (TNAME)
                      LENTNAME = MIN(15,LEN(TRIM(TNAME)))
                      LENGROUP = MIN(L2+14,LENLINE)
                      IF (LEN(TRIM(TNCHAR)).EQ.1) THEN
                        LINESTAR = LINET(1:L1)//' TN:'//
     &                  TNCHAR(1:1)//' C'//CNCHAR2//TNAME(1:LENTNAME)//
     &                  ' '//LINET(L2:LENGROUP)
                      ELSEIF (LEN(TRIM(TNCHAR)).EQ.2) THEN
                        LINESTAR = LINET(1:L1)//' TN:'//
     &                  TNCHAR(1:2)//' C'//CNCHAR2//TNAME(1:LENTNAME)//
     &                  ' '//LINET(L2:LENGROUP)
                      ELSEIF (LEN(TRIM(TNCHAR)).EQ.3) THEN
                        LINESTAR = LINET(1:L1)//' TN:'//
     &                  TNCHAR(1:3)//' C'//CNCHAR2//TNAME(1:LENTNAME)//
     &                  ' '//LINET(L2:LENGROUP)
                      ENDIF
                    ENDIF
                  ELSEIF (LINET(1:1).EQ.'@') THEN
                    DATECOL = Tvicolnm(linet,'DATE')
                    YEARCOL = Tvicolnm(linet,'YEAR')
                    DOYCOL = Tvicolnm(linet,'DOY')
                    IF (DOYCOL.LE.0) DOYCOL = Tvicolnm(linet,'DAY')
                    LENLINE = LEN(TRIM(LINET))
                    LINET(LENLINE+1:LENLINE+12) = '   DAP   DAS'
                    LINET(1:1) = '@'
                    WRITE (FNUMTMP,*) ' '
                    WRITE (FNUMTMP,'(A80)') LINESTAR(1:80)
                    WRITE (FNUMTMP,*) ' '
                    WRITE (FNUMTMP,'(A180)') LINET(1:180)
                    STARNUMM = STARNUMM + 1       ! Number of datasets  
                    CFLTFILE = 'Y'
                  ELSE
                    IF (LINET(1:1).NE.'!') THEN 
                      LENLINE = LEN(TRIM(LINET))
                      IF (LENLINE.GT.3)THEN
                        CALL Getstri (LINET,COLNUM,VALUEI)
                      ENDIF
                    ENDIF
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
                        DAP = MAX(0,CSTIMDIF(YEARPLT,DATE))
                        DAS = MAX(0,CSTIMDIF(YEARSIM,DATE))
                        DAPCHAR = TL10FROMI(DAP)
                        IF (LEN(TRIM(DAPCHAR)).EQ.1) THEN
                          DAPWRITE = '     '//DAPCHAR(1:1)
                        ELSEIF (LEN(TRIM(DAPCHAR)).EQ.2) THEN
                          DAPWRITE = '    '//DAPCHAR(1:2)
                        ELSEIF (LEN(TRIM(DAPCHAR)).EQ.3) THEN
                          DAPWRITE = '   '//DAPCHAR(1:3)
                        ENDIF
                        LENLINE = LEN(TRIM(LINET))
                        LINET(LENLINE+1:LENLINE+6) = DAPWRITE(1:6)
                        DAPCHAR = TL10FROMI(DAS)
                        IF (LEN(TRIM(DAPCHAR)).EQ.1) THEN
                          DAPWRITE = '     '//DAPCHAR(1:1)
                        ELSEIF (LEN(TRIM(DAPCHAR)).EQ.2) THEN
                          DAPWRITE = '    '//DAPCHAR(1:2)
                        ELSEIF (LEN(TRIM(DAPCHAR)).EQ.3) THEN
                          DAPWRITE = '   '//DAPCHAR(1:3)
                        ENDIF
                        LENLINE = LEN(TRIM(LINET))
                        LINET(LENLINE+1:LENLINE+6) = DAPWRITE(1:6)
                      ENDIF
                      WRITE (FNUMTMP,'(A180)') LINET
                    ENDIF
                  ENDIF
                ENDDO
 5555           CONTINUE
                ! If T-file was empty
                IF (TLINENUM.LT.4) THEN
                  WRITE (fnumwrk,*) 'T-file was empty!'
                ENDIF
              ENDIF
            
              CLOSE(FNUMT)
              CLOSE(FNUMTMP)
              
            ELSE
               OPEN (UNIT=FNUMTMP,FILE='MEASURED.OUT',STATUS='UNKNOWN')
               CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
            ENDIF ! END OF T-FILE READS AND MEASURED.OUT WRITES
          
            
!-----------------------------------------------------------------------
            
            ! Check that -99 not multiplied or divided 
            IF (hnumgmm.LT.0.0) hnumgmm = -99
            IF (hnumamm.LT.0.0) hnumamm = -99
          
            ! Create character equivalents for outputing
            IF (gwumm.LE.0) THEN
              gwummc = ' -99.0'
            ELSE
              gwummc = ' '
              WRITE (gwummc,'(F6.3)') gwumm
            ENDIF
            IF (gwum.LE.0) THEN
              gwumc = ' -99.0'
            ELSE
              gwumc = ' '
              WRITE (gwumc,'(F6.3)') gwum
            ENDIF
            IF (hiamm.LE.0) THEN
              hiammc = ' -99.0'
            ELSE
              hiammc = ' '
              WRITE (hiammc,'(F6.3)') hiamm
            ENDIF
            IF (hiam.LE.0) THEN
              hiamc = ' -99.0'
            ELSE
              hiamc = ' '
              WRITE (hiamc,'(F6.3)') hiam
            ENDIF
            IF (hinmm.LE.0) THEN
              hinmmc = ' -99.0'
            ELSE
              hinmmc = ' '
              WRITE (hinmmc,'(F6.3)') hinmm
            ENDIF
            IF (hinm.LE.0) THEN
              hinmc = ' -99.0'
            ELSE
              hinmc = ' '
              WRITE (hinmc,'(F6.3)') hinm
            ENDIF
            IF (vnpcmm.LE.0) THEN
              vnpcmmc = ' -99.0'
            ELSE
              vnpcmmc = ' '
              WRITE (vnpcmmc,'(F6.3)') vnpcmm
            ENDIF
            IF (vnpcm.LE.0) THEN
              vnpcmc = ' -99.0'
            ELSE
              vnpcmc = ' '
              WRITE (vnpcmc,'(F6.3)') vnpcm
            ENDIF
            IF (gnpcm.LE.0) THEN
              gnpcmc = ' -99.0'
            ELSE
              gnpcmc = ' '
              WRITE (gnpcmc,'(F6.3)') gnpcm
            ENDIF
            IF (gnpcmm.LE.0) THEN
              gnpcmmc = ' -99.0'
            ELSE
              gnpcmmc = ' '
              WRITE (gnpcmmc,'(F6.3)') gnpcmm
            ENDIF

            ! Calculate DAP data
            IF (stgdoy(9).LT.9999999) THEN 
              edap = Dapcalc(stgdoy(9),plyear,plday)
            ELSE
              EDAP = -99
              stgdoy(9) = -99
            ENDIF  
            IF (edatm.GT.0) THEN
             edapm = Dapcalc(edatm,plyear,plday)
            ELSE
             edapm = -99
            ENDIF 
            IF (drdat.GT.0) THEN 
              drdap = Dapcalc(drdat,plyear,plday)
            ELSE
             drdap = -99
            ENDIF  
            IF (drdatm.GT.0) THEN
             drdapm = Dapcalc(drdatm,plyear,plday)
            ELSE
             drdapm = -99
            ENDIF 
            IF (tsdat.GT.0) THEN 
              tsdap = Dapcalc(tsdat,plyear,plday)
            ELSE
             tsdap = -99
            ENDIF  
            IF (tsdatm.GT.0) THEN
             tsdapm = Dapcalc(tsdatm,plyear,plday)
            ELSE
             tsdapm = -99
            ENDIF 
            IF (jdat.GT.0) THEN 
              jdap = Dapcalc(jdat,plyear,plday)
            ELSE
             jdap = -99
            ENDIF  
            IF (jdatm.GT.0) THEN
             jdapm = Dapcalc(jdatm,plyear,plday)
            ELSE
             jdapm = -99
            ENDIF 
            IF (adat.LT.9999999) THEN 
              adap = Dapcalc(adat,plyear,plday)
            ELSE
              adap = -99
            ENDIF  
            IF (adatm.GT.0) THEN
              adapm = Dapcalc(adatm,plyear,plday)
            ELSE
              adapm = -99
            ENDIF  
            IF (stgdoy(5).LT.9999999) THEN 
              mdap = Dapcalc(stgdoy(5),plyear,plday)
            ELSE 
              mdap = -99
              stgdoy(5) = -99
            ENDIF     
            IF (mdatm.GT.0) THEN
              mdapm = Dapcalc(mdatm,plyear,plday)
            ELSE
              mdapm = -99 
            ENDIF
            
!-----------------------------------------------------------------------
            
            ! SCREEN WRITES FOR DSSAT IN SENSITIVITY RNMODE
            
            IF (FILEIOT(1:3).EQ.'DS4' .AND. CN.EQ.1
     &                                .AND. RNMODE.EQ.'E') THEN         
            
              CALL CSCLEAR5
              WRITE(*,9589)
              WRITE(*,*) ' '
              WRITE(*,9588)
              WRITE(*,9600)
              DO L = 7, 9
!                CALL CSYR_DOY(STGDOY(L),YEAR,DOY)
                CALL YR_DOY(STGDOY(L),YEAR,DOY)
                CALL Calendar(year,doy,dom,month)
                CNCTMP = 0.0
                IF (CWADSTG(L).GT..0) CNCTMP = CNADSTG(L)/CWADSTG(L)*100
                WRITE (*,'(I8,I4,1X,A3,I4,1X,I1,1X,A10,I6,F6.2,
     &           F6.1,I6,F6.1,F6.2,F6.2)')
     &           STGDOY(L),DOM,MONTH,
     &           Dapcalc(stgdoy(L),plyear,plday),L,STNAME(L),
     &           NINT(CWADSTG(L)),LAISTG(L),LNUMSTG(L),
     &           NINT(CNADSTG(L)),CNCTMP,1.0-WFPAV(L),1.0-NFPAV(L)
              ENDDO
              DO L = 1, 6
!                CALL CSYR_DOY(STGDOY(L),YEAR,DOY)
                CALL YR_DOY(STGDOY(L),YEAR,DOY)
                CALL Calendar(year,doy,dom,month)
                CNCTMP = 0.0
                IF (CWADSTG(L).GT..0) CNCTMP = CNADSTG(L)/CWADSTG(L)*100
                IF (STGDOY(L).GT.0) THEN
                  WRITE (*,'(I8,I4,1X,A3,I4,1X,I1,1X,A10,I6,F6.2,
     &             F6.1,I6,F6.1,F6.2,F6.2)')
     &             STGDOY(L),DOM,MONTH,
     &             Dapcalc(stgdoy(L),plyear,plday),L,STNAME(L),
     &             NINT(CWADSTG(L)),LAISTG(L),LNUMSTG(L),
     &             NINT(CNADSTG(L)),CNCTMP,1.0-WFPAV(L),1.0-NFPAV(L)
                ENDIF
              ENDDO
            
              WRITE(*,*)' '
              WRITE(*,*)' Press ENTER to continue'
              PAUSE ' '
              CALL CSCLEAR5
           
              WRITE (*,206)
              WRITE (*,3051) MAX(-99,edap),MAX(-99,edapm),
     x         adap,adapm,
     x         mdap,mdapm,
     x         NINT(gwam),NINT(gwamm),
     x         gwumc,gwummc,
     x         NINT(hnumam),NINT(hnumamm),
     x         hnumgm,hnumgmm,
     x         hiam,hiamm,
     x         laix,laixm,
     x         lnumsm,lnumsmm,
     x         NINT(tnumam),NINT(tnumamm),
     x         NINT(cwam),NINT(cwamm),
     x         NINT(vwam),NINT(vwamm),
     x         NINT(rwam),NINT(rwamm),
     x         NINT(carboac),NINT(carboacm),
     x         NINT(senwatc),NINT(senwatcm),
     x         NINT(rswam),NINT(rswamm)
 3051         FORMAT (
     x        6X, 'Emergence (DAP)                 ',4X,I7,  6X,I7,  /,
     x        6X, 'Anthesis (DAP)                  ',4X,I7,  6X,I7,  /,
     x        6X, 'Maturity (DAP)                  ',4X,I7,  6X,I7,  /,
     x        6X, 'Product wt (kg dm/ha;no loss)   ',4X,I7, 6X,I7,  /,
     x        6X, 'Product unit weight (g dm)      ',5X,A6, 7X,A6,  /,
     x        6X, 'Product number (no/m2)          ',4X,I7,  6X,I7,  /,
     x        6X, 'Product number (no/group)       ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Product harvest index (ratio)   ',4X,F7.2,6X,F7.2,/,
     x        6X, 'Maximum leaf area index         ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Final leaf number (one axis)    ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Final shoot number (#/m2)       ',4X,I7  ,6X,I7  ,/,
     x        6X, 'Canopy (tops) wt (kg dm/ha)     ',4X,I7,  6X,I7,  /,
     x        6X, 'Vegetative wt (kg dm/ha)        ',4X,I7,  6X,I7,  /,
     x        6X, 'Root wt (kg dm/ha)              ',4X,I7,  6X,I7,  /,
     x        6X, 'Assimilate wt (kg dm/ha)        ',4X,I7,  6X,I7,  /,
     x        6X, 'Senesced wt (kg dm/ha)          ',4X,I7,  6X,I7,  /,
     x        6X, 'Reserves wt (kg dm/ha)          ',4X,I7,  6X,I7)

              WRITE(*,*)' '
              WRITE(*,*)' Press ENTER to continue'
              PAUSE ' '
              CALL CSCLEAR5
     
              WRITE (*,206)
              WRITE (*,3052)                              
     x         nuam,nuamm,
     x         sennatc,sennatcm,
     x         cnam,cnamm,
     x         rnam,rnamm,
     x         vnam,vnamm,
     x         gnam,gnamm,
     x         hinm,hinmm,
     x         gnpcm,gnpcmm,
     x         vnpcm,vnpcmm,
     x         NINT(cwaa),NINT(cwaam),
     x         vnaa,cnaam,
     x         lnpca,lnpcam
 3052         FORMAT (
     x        6X, 'N uptake (kg/ha)                 ',4X,F7.1,6X,F7.1,/,
     x        6X, 'N senesced (kg/ha)               ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Above-ground N (kg/ha)           ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Root N (kg/ha)                   ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Vegetative N (kg/ha)             ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Product N (kg/ha)                ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Product N harvest index (ratio)  ',4X,F7.2,6X,F7.2,/,
     x        6X, 'Product N (%)                    ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Vegetative N (%)                 ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Leaf+stem wt,anthesis (kg dm/ha) ',4X,  I7,6X,I7,  /,
     x        6X, 'Leaf+stem N,anthesis (kg/ha)     ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Leaf N,anthesis (%)              ',4X,F7.1,6X,F7.1)

              WRITE(*,*)' '
              WRITE(*,*)' Press ENTER to continue'
              PAUSE ' '
              CALL CSCLEAR5
              CALL CSCLEAR5
              CALL CSCLEAR5
              CALL CSCLEAR5
            
            ENDIF
            
            ! END OF SCREEN WRITES FOR DSSAT SENSITIVITY RNMODE
            
!-----------------------------------------------------------------------

            IF (IDETO.NE.'N'.OR.IDETL.EQ.'A') THEN
            
              ! PLANT EVALUATION (MEASURED - SIMULATED COMPARISONS)
              
              WRITE (fnumwrk,*) 'Writing EVALUATION'
              
              EVHEADER = ' '
              FNAMETMP = ' '
              EVHEADER(1:14) = '*EVALUATION : '
              FNAMETMP(1:12) = 'Evaluate.OUT'
              IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
               OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,STATUS = 'UNKNOWN')
               WRITE (FNUMTMP,'(A17)') '$PLANT EVALUATION'
               CLOSE(FNUMTMP)
               EVALOUT = 0
               EVHEADNM = 0
              ENDIF
              
              IF (EVHEADNM.LT.7) THEN
                IF (EXCODE.NE.EXCODEP.AND.EVALOUT.GT.1 .OR.
     &              RUN.EQ.1.AND.RUNI.EQ.1) THEN
                 EVHEADNM = EVHEADNM + 1
                 OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,
     &            POSITION = 'APPEND')
                 WRITE (FNUMTMP,*) ' '
                 IF (EVHEADNM.LT.7) THEN
                   WRITE (FNUMTMP,993) EVHEADER,EXCODE,
     &              ENAME(1:25),MODNAME
  993              FORMAT (A14,A10,'  ',A25,2X,A8,/)
                 ELSE
                   WRITE (FNUMTMP,1995) EVHEADER, 
     &             'MANY??????','  REMAINING EXPERIMENTS  ',MODNAME
 1995              FORMAT (A14,A10,A25,A8,/)
                 ENDIF
                 WRITE (FNUMTMP,994)
  994            FORMAT ('@RUN',
     x            ' EXCODE    ',
     x            '  TRNO RN',
     x            ' CR',
     x            ' EDAPS EDAPM',
     x            ' DRAPS DRAPM',
     x            ' TSAPS TSAPM',
     x            ' ADAPS ADAPM',
     x            ' MDAPS MDAPM',
     x            ' HWAMS HWAMM',
     x            ' HWUMS HWUMM',
     x            ' H#AMS H#AMM',
     x            ' H#GMS H#GMM',
     x            ' LAIXS LAIXM',
     x            ' L#SMS L#SMM',
     x            ' T#AMS T#AMM',
     x            ' CWAMS CWAMM',
     x            ' VWAMS VWAMM',
     x            ' HIAMS HIAMM',
     x            ' HN%MS HN%MM',
     x            ' VN%MS VN%MM',
     x            ' CNAMS CNAMM',
     x            ' HNAMS HNAMM',
     x            ' HINMS HINMM')
                 CLOSE(FNUMTMP)
                ENDIF
              ENDIF
                
              IF (EXCODE.NE.EXCODEP) EVALOUT = 0
              EVALOUT = EVALOUT + 1
              
              IF (STGDOY(5).GT.0 .AND. ADAT.GT.0) THEN
                AMDAYS = Dapcalc(stgdoy(5),plyear,plday) 
     X                 - Dapcalc(adat,plyear,plday)
              ELSE
                AMDAYS = -99
              ENDIF 
              IF (MDATM.GT.0 .AND. ADATM.GT.0) THEN
                AMDAYM = Dapcalc(mdatm,plyear,plday) 
     X                 - Dapcalc(adatm,plyear,plday)
              ELSE
                AMDAYM = -99
              ENDIF 
              
              OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,POSITION = 'APPEND') 
              
              WRITE (FNUMTMP,8404) RUN,EXCODE,TN,RN,CR,
     x        edap,edapm,
     x        drdap,drdapm,
     x        tsdap,tsdapm,
     x        adap,adapm,
     x        mdap,mdapm,         
     x        NINT(gwam),NINT(gwamm),
     x        gwumc,gwummc,
     x        NINT(hnumam),NINT(hnumamm),
     x        hnumgm,hnumgmm,
     x        laix,laixm,
     x        lnumsm,lnumsmm,
     x        NINT(tnumam),NINT(tnumamm),
     x        NINT(cwam),NINT(cwamm),
     x        NINT(vwam),NINT(vwamm),
     x        hiamc,hiammc,
     x        gnpcm,gnpcmm,
     x        vnpcmc,vnpcmmc,
     x        NINT(cnam),NINT(cnamm),
     x        NINT(gnam),NINT(gnamm),
     x        hinmc,hinmmc             
              
 8404         FORMAT (I4,1X,A10,I6,I3,1X,A2,
     x        I6,I6,
     x        I6,I6,
     x        I6,I6,
     x        I6,I6,
     x        I6,I6,
     x        I6,I6,
     x        A6,A6,
     x        I6,I6,
     x        F6.1,F6.1,
     x        F6.1,F6.1,
     x        F6.1,F6.1,
     x        I6,I6,
     x        I6,I6,
     x        I6,I6,
     x        A6,A6,
     x        F6.1,F6.1,
     x        A6,A6,
     x        I6,I6,
     x        I6,I6,
     x        A6,A6)
!     x        I7,I7,
!     x        I7,I7,
!     x        I6,I6)
              
              Close(FNUMTMP)       

!     VSH CSV output corresponding to Evaluate.OUT
      IF (FMOPT == 'C') then
         CALL CsvOutEvalCsCer(EXCODE, RUN, TN, RN,  REP, 
     &CR,Edap,Edapm,Drdap,Drdapm,Tsdap,Tsdapm,Adap,Adapm,Mdap,
     &Mdapm, Gwam, Gwamm, Gwumc, Gwummc, Hnumam, Hnumamm, Hnumgm,
     &Hnumgmm,Laix,Laixm, Lnumsm,Lnumsmm,Tnumam, Tnumamm, Cwam, 
     &Cwamm, Vwam, Vwamm, Hiamc,Hiammc,Gnpcm,Gnpcmm,Vnpcmc,Vnpcmmc,
     &Cnam, Cnamm, Gnam, Gnamm, Hinmc, Hinmmc,  
     &vCsvlineEvalCsCer, vpCsvlineEvalCsCer, vlngthEvalCsCer) 
     
         CALL LinklstEvalCsCer(vCsvlineEvalCsCer)
      END IF 
      
            ELSE
            
               OPEN (UNIT=FNUMTMP,FILE='EVALUATE.OUT',STATUS='UNKNOWN')
               CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
               
            ENDIF  ! END OF PLANT EVALUATION
            
!-----------------------------------------------------------------------
            
            ! OVERVIEW
            
            IF (IDETO.EQ.'Y'.OR.IDETL.EQ.'A') THEN

              WRITE (fnumwrk,*) 'Writing OVERVIEW'
              
              FNAMETMP = ' '
              FNAMETMP(1:12) = 'Overview.'//out
              
              IF (FILEIOT(1:2).EQ.'DS') THEN
                IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
                  OPEN (UNIT = FNUMTMP, FILE = FNAMETMP)
                  WRITE(FNUMTMP,'("*SIMULATION OVERVIEW FILE")')
                ELSE
                  INQUIRE (FILE = FNAMETMP, EXIST = FEXIST)
                  IF (FEXIST) THEN
                    INQUIRE (FILE = 'OVERVIEW.OUT',OPENED = fopen)
                    IF (.NOT.fopen) THEN
                      OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,
     &                 POSITION = 'APPEND')
                    ENDIF
                  ENDIF  
                ENDIF
                WRITE (FNUMTMP,*) ' '
                CALL HEADER(1, FNUMTMP, RUN)
              ELSE
                IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
                  OPEN (UNIT = FNUMTMP, FILE = FNAMETMP)
                  WRITE (FNUMTMP,'(A15)') '$OVERVIEW'
                ELSE
                 OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,POSITION='APPEND')
                ENDIF
              
                WRITE (FNUMTMP,'(/,A77,/)') OUTHED
                WRITE (FNUMTMP,103) MODEL
                WRITE (FNUMTMP,1031) MODNAME
                WRITE (FNUMTMP,1032) FILENEW
 1032           FORMAT (' FILE             ',A12)
                WRITE (FNUMTMP,104)
     &           EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
                WRITE (FNUMTMP,102) TN, TNAME
                WRITE (FNUMTMP,107) CR,VARNO,VRNAME
                CALL Calendar (plyear,plday,dom,month)
                WRITE (FNUMTMP,108)month,dom,NINT(pltpop),NINT(rowspc)
                WRITE (fnumtmp,109) tmaxx,tminn,NINT(co2max)
  109           FORMAT (' ENVIRONMENT      ','Tmax (max): ',F4.1,
     &           '  Tmin (min): ',F5.1,
     &           '  Co2 (max):',I4)
                WRITE (fnumtmp,110) iswwat, iswnit
  110           FORMAT (' MODEL SWITCHES   ','Water: ',A1,
     &           '  Nitrogen: ',A1)
              ENDIF
              
              !edap = Dapcalc(stgdoy(9),plyear,plday)
              !edapm = Dapcalc(edatm,plyear,plday)
              IF (edapm.GT.200) THEN
                WRITE (Fnumwrk,*)' '
                WRITE (Fnumwrk,'(A31,A31,A11)')
     &           'Measured emergence over 200DAP ',
     &           'Maybe reported before planting.',
     &           'Check files'
              ENDIF
              !adap = Dapcalc(adat,plyear,plday)
              !adapm = Dapcalc(adatm,plyear,plday)
              !mdap = Dapcalc(stgdoy(5),plyear,plday)
              !mdapm = Dapcalc(mdatm,plyear,plday)
              
              WRITE(FNUMTMP,9589)
              WRITE(fnumtmp,*)' '
              WRITE(FNUMTMP,'(A11,I4,A3,A60)')
     &         ' RUN NO.   ',RUN,'  ',ENAME                             
              IF (DYNAMIC.EQ.SEASEND) THEN
                WRITE(fnumtmp,*)' '
                WRITE(fnumtmp,'(A50,A25)')
     &          ' NB. RUN TERMINATED PREMATURELY (PROBABLY BECAUSE ',
     &          'OF MISSING WEATHER DATA) '
              ENDIF
              WRITE(fnumtmp,9588)
              WRITE(fnumtmp,9600)
              DO L = 7, 9
!                 CALL CSYR_DOY(STGDOY(L),YEAR,DOY)
                 CALL YR_DOY(STGDOY(L),YEAR,DOY)
                 CALL Calendar(year,doy,dom,month)
                 CNCTMP = 0.0
                 IF (CWADSTG(L).GT.0.0) 
     &             CNCTMP = CNADSTG(L)/CWADSTG(L)*100
                 WRITE (fnumtmp,'(I8,I4,1X,A3,I4,1X,I1,1X,A10,I6,F6.2, 
     &            F6.1,I6,F6.1,F6.2,F6.2)')
     &            STGDOY(L),DOM,MONTH,
     &            Dapcalc(stgdoy(L),plyear,plday),L,STNAME(L),
     &            NINT(CWADSTG(L)),LAISTG(L),LNUMSTG(L),
     &            NINT(CNADSTG(L)),CNCTMP,1.0-WFPAV(L),1.0-NFPAV(L)
              ENDDO
              DO L = 1, 6
!                CALL CSYR_DOY(STGDOY(L),YEAR,DOY)
                CALL YR_DOY(STGDOY(L),YEAR,DOY)
                CALL Calendar(year,doy,dom,month)
                CNCTMP = 0.0
                IF (CWADSTG(L).GT.0.0)CNCTMP = CNADSTG(L)/CWADSTG(L)*100
                IF (STGDOY(L).GT.0.AND.STGDOY(L).LT.9999999) THEN
                  WRITE (fnumtmp,'(I8,I4,1X,A3,I4,1X,I1,1X,A10,I6,F6.2,
     &             F6.1,I6,F6.1,F6.2,F6.2)')
     &             STGDOY(L),DOM,MONTH,
     &             Dapcalc(stgdoy(L),plyear,plday),L,STNAME(L),
     &             NINT(CWADSTG(L)),LAISTG(L),LNUMSTG(L),
     &             NINT(CNADSTG(L)),CNCTMP,1.0-WFPAV(L),1.0-NFPAV(L)
                ENDIF
              ENDDO
              IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
              WRITE(fnumtmp,*)' '
              WRITE(fnumtmp,*)'BIOMASS = Above-ground dry wt (kg/ha)'
              WRITE(fnumtmp,*)'LEAF AREA  = Leaf area index (m2/m2)'
              WRITE(fnumtmp,*)
     &         'LEAF NUMBER  = Leaf number produced on main axis'
              WRITE(fnumtmp,*)'CROP N  = Above-ground N (kg/ha)'
              WRITE(fnumtmp,*)'CROP N% = Above-ground N conc (%)'
              WRITE(fnumtmp,*)
     &         'H2O STRESS = Photosynthesis stress,average (0-1,0=none)'
              WRITE(fnumtmp,*)
     &         'N STRESS = Photosynthesis stress,average (0-1,0=none)'
              ENDIF
              WRITE(fnumtmp,*)' '
              
              WRITE (FNUMTMP,206)
              WRITE (FNUMTMP,305) MAX(-99,edap),MAX(-99,edapm),
     x         adap,adapm,
     x         mdap,mdapm,
     x         NINT(gwam),NINT(gwamm),
     x         gwumc,gwummc,
     x         NINT(hnumam),NINT(hnumamm),
     x         hnumgm,hnumgmm,
     x         hiam,hiamm,
     x         laix,laixm,
     x         lnumsm,lnumsmm,
     x         NINT(tnumam),NINT(tnumamm),
     x         NINT(cwam),NINT(cwamm),
     x         NINT(vwam),NINT(vwamm),
     x         NINT(rwam),NINT(rwamm),
     x         NINT(carboac),NINT(carboacm),
     x         NINT(senwatc),NINT(senwatcm),
     x         NINT(rswam),NINT(rswamm),
     x         nuam,nuamm,
     x         sennatc,sennatcm,
     x         cnam,cnamm,
     x         rnam,rnamm,
     x         vnam,vnamm,
     x         gnam,gnamm,
     x         hinm,hinmm,
     x         gnpcm,gnpcmm,
     x         vnpcm,vnpcmm,
     x         NINT(cwaa),NINT(cwaam),
     x         vnaa,cnaam,
     x         lnpca,lnpcam
              
  305         FORMAT (
     x        6X, 'Emergence (DAP)                  ',4X,I7,  6X,I7,  /,
     x        6X, 'Anthesis (DAP)                   ',4X,I7,  6X,I7,  /,
     x        6X, 'Maturity (DAP)                   ',4X,I7,  6X,I7,  /,
     x        6X, 'Product wt (kg dm/ha;no loss)    ',4X,I7, 6X,I7,  /,
     x        6X, 'Product unit weight (g dm)       ',5X,A6, 7X,A6,  /,
     x        6X, 'Product number (no/m2)           ',4X,I7,  6X,I7,  /,
     x        6X, 'Product number (no/group)        ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Product harvest index (ratio)    ',4X,F7.2,6X,F7.2,/,
     x        6X, 'Maximum leaf area index          ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Final leaf number (one axis)     ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Final shoot number (#/m2)        ',4X,I7  ,6X,I7  ,/,
     x        6X, 'Canopy (tops) wt (kg dm/ha)      ',4X,I7,  6X,I7,  /,
     x        6X, 'Vegetative wt (kg dm/ha)         ',4X,I7,  6X,I7,  /,
     x        6X, 'Root wt (kg dm/ha)               ',4X,I7,  6X,I7,  /,
     x        6X, 'Assimilate wt (kg dm/ha)         ',4X,I7,  6X,I7,  /,
     x        6X, 'Senesced wt (kg dm/ha)           ',4X,I7,  6X,I7,  /,
     x        6X, 'Reserves wt (kg dm/ha)           ',4X,I7,  6X,I7,  /,
     x        6X, 'N uptake (kg/ha)                 ',4X,F7.1,6X,F7.1,/,
     x        6X, 'N senesced (kg/ha)               ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Above-ground N (kg/ha)           ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Root N (kg/ha)                   ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Vegetative N (kg/ha)             ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Product N (kg/ha)                ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Product N harvest index (ratio)  ',4X,F7.2,6X,F7.2,/,
     x        6X, 'Product N (%)                    ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Vegetative N (%)                 ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Leaf+stem wt,anthesis (kg dm/ha) ',4X,  I7,6X,I7,  /,
     x        6X, 'Leaf+stem N,anthesis (kg/ha)     ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Leaf N,anthesis (%)              ',4X,F7.1,6X,F7.1)
              
              IF (run.EQ.1.AND.runi.EQ.1) THEN
            
              WRITE (FNUMTMP,'(/,A55,/,A55)')
     x        '  Seed N must be added to N uptake to obtain a         ',
     x        '  balance with N in above-ground plus root material    '
              WRITE (FNUMTMP,'(/,17(A55,/))')
     x        '  Measured data are obtained from the A file,either    ',
     x        '  directly or by calculation from other other variables',
     x        '  using the expressions given below:                   ',
     x        '                                                       ',
     x        '  Product wt       Harvest wt (HWAH) /                 ',
     x        '                               (Harvest%/100) (HPC)    ',
     x        '  Canopy wt        Grain wt (HWAM)+Vegetative wt (VWAm)',
     x        '  Vegetative wt    Canopy wt (CWAM) - grain wt (HWAM)  ',
     x        '    = leaf+stem+retained dead material                 ',
     x        '  Product unit wt  Grain yield (HWAM)/Grain # (G#AM)   ',
     x        '  Product #/area   Product#/tiller (H#SM) *            ',
     x        '                                   tiller number (T#AM)',
     x        '  Product #/group  Product#/area (H#AM) /              ',
     x        '                                tiller number (T#AM)   ',
     x        '  Harvest index    Product wt (HWAM)/Canopy wt.(CWAM)  ',
     x        '                                                       ',
     x        '  The same procedure is followed for nitrogen aspects  '
     
              ENDIF
            
              WRITE(fnumtmp,5500)
            
              PFPAV = -99.0
              PFGAV = -99.0
              DASH = ' - '
              DO tvI1 = 1,5  
                IF (TVI1.EQ.1)THEN
                  WRITE(fnumtmp,600) stname(8), dash, stname(tvi1), 
     &             daysc(tvI1), TMAXav(tvI1),TMINav(tvI1),sRADav(tvI1),
     &             DAYLav(tvI1),RAINcp(tvI1),ETC(tvI1),1.0-wfpav(tvi1),
     &             1.0-wfgav(tvi1), 1.0-nfpav(tvi1), 1.0-nfgav(tvi1), 
     &             PFPAV(TVI1), PFGAV(TVI1)
                ELSE
                  IF (DAYSC(tvi1).GT.0) THEN
                    WRITE(fnumtmp,600) stname(tvi1-1),dash,stname(tvi1),
     &              daysc(tvI1), TMAXav(tvI1),TMINav(tvI1),sRADav(tvI1),
     &              DAYLav(tvI1),RAINcp(tvI1),ETC(tvI1),1.0-wfpav(tvi1),
     &              1.0-wfgav(tvi1), 1.0-nfpav(tvi1), 1.0-nfgav(tvi1), 
     &              PFPAV(TVI1), PFGAV(TVI1)
                  ENDIF
                ENDIF
  600           FORMAT(1X,A10,A3,A10,I5,3F6.1,F7.2,2F7.1,4F7.3,2F7.2)
  610           FORMAT(1X,A10,13X,I5,3F6.1,F7.2,2I7,6F7.3)
              ENDDO
              IF (daysc(5).gt.0) then
                WRITE(fnumtmp,*) ' ' 
                WRITE(fnumtmp,600) stname(8),dash,stname(5), 
     &           daysc(0), TMAXav(0),TMINav(0),sRADav(0), 
     &           DAYLav(0), RAINcp(0),ETC(0), 1.0-wfpav(0), 
     &           1.0-wfgav(0), 1.0-nfpav(0), 1.0-nfgav(0), 
     &           PFPAV(0), PFGAV(0)
              ENDIF     
            
              !Resource productivity calculations
   
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
 
              IF (RAINCP(0) > 1.E-3) THEN
                DMP_Rain = CWAM / RAINCp(0) 
                GrP_Rain = GWAM  / RAINCp(0)
              ENDIF
            
              IF (ETC(0) > 1.E-3) THEN
                DMP_ET = CWAM / ETC(0) 
                GrP_ET = GWAM  / ETC(0) 
              ENDIF
            
              IF (EPC(0) > 1.E-3) THEN
                DMP_EP = CWAM / EPC(0) 
                GrP_EP = GWAM  / EPC(0) 
              ENDIF
 
              IF (TOTIR > 1.E-3) THEN
                DMP_Irr = CWAM / TOTIR 
                GrP_Irr = GWAM  / TOTIR
              ENDIF
 
              IF (ISWNIT.NE.'N') THEN
                IF (Amtnit > 1.E-3) THEN
                  DMP_NApp = CWAM / Amtnit
                  GrP_NApp = GWAM  / Amtnit
                ENDIF
            
                IF (NUAD > 1.E-3) THEN
                  DMP_NUpt = CWAM / NUAD
                  GrP_NUpt = GWAM  / NUAD
                ENDIF
              ENDIF !ISWNIT == 'Y'
                        
              ! Productiviity outputs not written if run aborted        
              IF (DYNAMIC.NE.SEASEND) THEN
                WRITE (FNUMTMP, 1200) daysc(0), 
     &           RAINCP(0), DMP_Rain*0.1,DMP_Rain,GrP_Rain*0.1,GrP_Rain,
     &           ETC(0),  DMP_ET*0.1,   DMP_ET,   GrP_ET*0.1,   GrP_ET, 
     &           EPc(0),  DMP_EP*0.1,   DMP_EP,   GrP_EP*0.1,   GrP_EP
 
                IF (TOTIR > 1.E-3) THEN
                 WRITE(FNUMTMP, 1210) 
     &            TOTIR, DMP_Irr*0.1, DMP_Irr, GrP_Irr*0.1, GrP_Irr
                ENDIF
 
                IF (ISWNIT.NE.'N') THEN
                  IF (Amtnit > 1.E-3) THEN
                   WRITE(FNUMTMP, 1220) Amtnit, DMP_NApp, GrP_NApp 
                  ENDIF
                          
                  IF (NUAD > 1.E-3) THEN
                   WRITE(FNUMTMP, 1230) NUAD, DMP_NUpt,GrP_NUpt
                  ENDIF
                ENDIF !ISWNIT == 'Y'
              ENDIF
            
              WRITE(FNUMTMP,270)
              IF (CR.EQ.'WH') THEN 
                WRITE(FNUMTMP,300) 'WHEAT', NINT(GWAM)
              ELSEIF (CR.EQ.'BA') THEN 
                WRITE(FNUMTMP,300) 'BARLEY', NINT(GWAM)
              ENDIF  
              WRITE(FNUMTMP,'(110("*"))')
            
              CLOSE(FNUMTMP)   

            ELSE
            
               OPEN (UNIT=FNUMTMP,FILE='OVERVIEW,OUT')
               CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
               
            ENDIF  ! END OF OVERVIEW

!-----------------------------------------------------------------------
            
            ! CSM SUMMARY ... not used in CROPSIM
            
            ! Store Summary labels and values in arrays to send to
            ! OPSUM routine for printing.  Integers are temporarily
            ! saved as real numbers for placement in real array.
            
            LABEL(1) = 'ADAT '; VALUE(1) = FLOAT(adat)
            LABEL(2) = 'MDAT '; VALUE(2) = FLOAT(stgdoy(5))
            LABEL(3) = 'DWAP '; VALUE(3) = sdrate
            LABEL(4) = 'CWAM '; VALUE(4) = cwam
            LABEL(5) = 'HWAM '; VALUE(5) = gwam
            LABEL(6) = 'HWAH '; VALUE(6) = gwam * hpc / 100.
            LABEL(7) = 'BWAH '; VALUE(7) = vwam * hbpc / 100. 
            
            LABEL(8)  = 'HWUM '; VALUE(8) = gwum
            LABEL(9)  = 'H#AM '; VALUE(9) = hnumam
            LABEL(10) = 'H#UM '; VALUE(10) = hnumgm
            LABEL(11) = 'NUCM '; VALUE(11) = nuad
            LABEL(12) = 'CNAM '; VALUE(12) = cnam
            LABEL(13) = 'GNAM '; VALUE(13) = gnam
            LABEL(14) = 'PWAM '; VALUE(14) = PWAM    
            LABEL(15) = 'LAIX '; VALUE(15) = LAIX    
            LABEL(16) = 'HIAM '; VALUE(16) = HIAM    
 
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
 
            LABEL(29) = 'EDAT ' ; VALUE(29) = FLOAT(STGDOY(9))  
            
            LABEL(30) = 'NDCH ' ; VALUE(30) = FLOAT(DAYSC(0)) 
            LABEL(31) = 'TMINA' ; VALUE(31) = TMINAV(0)       
            LABEL(32) = 'TMAXA' ; VALUE(32) = TMAXAV(0)       
            LABEL(33) = 'SRADA' ; VALUE(33) = SRADAV(0)       
            LABEL(34) = 'DAYLA' ; VALUE(34) = DAYLAV(0)       
            LABEL(35) = 'CO2A ' ; VALUE(35) = CO2AV(0)        
            LABEL(36) = 'PRCP ' ; VALUE(36) = RAINCP(0)       
            LABEL(37) = 'ETCP ' ; VALUE(37) = ETC(0)      

            IF (FILEIOT(1:2).EQ.'DS') CALL SUMVALS (SUMNUM,LABEL,VALUE)
            
            ! END OF CSM SUMMARY
            
!-----------------------------------------------------------------------
            
            ! PLANT RESPONSES (SIMULATED)
            
            IF (IDETL.EQ.'D'.OR.IDETL.EQ.'A') THEN

              WRITE (fnumwrk,*) 'Writing PLANT RESPONSES (SIMULATED)'
              
              FNAMETMP = ' '
              FNAMETMP(1:12) = 'Plantres.'//out
              IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
                OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,STATUS = 'UNKNOWN')
                WRITE (FNUMTMP,'(A28)') '$PLANT RESPONSES (SIMULATED)'
                CLOSE(FNUMTMP)
              ENDIF
              
              IF (EXCODE.NE.EXCODEP.OR.TNAME(1:1).EQ.'*') THEN
                OPEN (UNIT=FNUMTMP,FILE=FNAMETMP,POSITION='APPEND')
                WRITE (FNUMTMP,*) ' '
                WRITE (TLINETMP,9951) EXCODE,MODNAME
 9951           FORMAT ('*RESPONSES(S):',A10,'  ',A8)
                IF (TNAME(1:1).EQ.'*') THEN
                  WRITE (FNUMTMP,'(A180)') TLINETMP
                ELSE
                  WRITE (FNUMTMP,'(A180)') TLINETMP
                ENDIF
                PLDAYP = 0
                WRITE (FNUMTMP,97)
   97           FORMAT ('@  RUN',
     x          ' EXCODE    ',
     x          'TRNO RN',
     x          '    CR',
     x          '  PDOY  EDAP',
     x          ' TSDAP  ADAP  MDAP',
     x          '  HWAM  HWUM',
     x          '  H#AM  H#GM  LAIX  L#SM  T#AM',
     x          '  CWAM  VWAM  HIAM  RWAM',
     x          '  HN%M  TNAM',
     x          '  CNAM  HNAM',
     x          '  HINM PLPOP',
     x          '  NICM',
     x          ' SRADA TMAXA TMINA  PRCP')
              ELSE
                OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,POSITION ='APPEND')
              ENDIF
              
              IF (plday.LT.pldayp) THEN
                IF (varno.EQ.varnop) THEN
                  pldayp = plday + 365
                ELSE
                  pldayp = plday
                ENDIF
              ELSE
                pldayp = plday
              ENDIF
              varnop = varno
              
              WRITE (FNUMTMP,7401) RUN,EXCODE,TN,RN,CR,
     x         PLDAYP,
     x         edap,tsdap,adap,mdap,
     x         NINT(gwam),gwumc,
     x         NINT(hnumam),NINT(hnumgm),laix,lnumsm,tnumam,
     x         NINT(cwam),
     x         NINT(vwam),hiamc, NINT(rwam),
     x         gnpcmc,NINT(AMAX1(-99.0,cnam+rnam)),
     x         NINT(cnam),NINT(gnam),
     x         hinmc,pltpop,
     x         NINT(amtnit),
     &         sradav(0),tmaxav(0),tminav(0),NINT(raincp(0))
              
 7401          FORMAT (I6,1X,A10,1X,I3,I3,4X,A2,
     x         I6,
     x         I6,I6,I6,I6,
     x         I6,A6,
     x         I6,I6,F6.1,F6.1,F6.1,
     x         I6,
     x         I6,A6,I6,
     x         A6,I6,
     x         I6,I6,
     x         A6,F6.1,
     x         I6,
     x         3F6.1,I6)
              
              CLOSE(FNUMTMP)       ! END OF PLANT RESPONSES (SIMULATED)
            
            ELSE
               OPEN(UNIT=FNUMTMP,FILE='Plantres.out',STATUS = 'UNKNOWN')
               CLOSE(UNIT=FNUMTMP,STATUS='DELETE')
            ENDIF
            
!-----------------------------------------------------------------------
            
            ! PLANT RESPONSES (MEASURED)

            IF (IDETL.EQ.'D'.OR.IDETL.EQ.'A') THEN
            
              WRITE (fnumwrk,*) 'Writing PLANT RESPONSES (MEASURED)'
              
              FNAMETMP = ' '
              FNAMETMP(1:12) = 'Plantrem.'//out
              IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
               OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,STATUS = 'UNKNOWN')
               WRITE (FNUMTMP,'(A27)') '$PLANT RESPONSES (MEASURED)'
               CLOSE(FNUMTMP)
              ENDIF
              
              IF (EXCODE.NE.EXCODEP .OR.TNAME(1:1).EQ.'*') THEN
                OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,POSITION='APPEND')
                WRITE (FNUMTMP,*) ' '
                TLINETMP = ' '
                WRITE (TLINETMP,99511) EXCODE,MODNAME
99511           FORMAT ('*RESPONSES(M):',A10,'  ',A8)
                IF (TNAME(1:1).EQ.'*') THEN
                  WRITE (FNUMTMP,'(A180)') TLINETMP
                ELSE
                  WRITE (FNUMTMP,'(A180)') TLINETMP
                ENDIF
                WRITE (FNUMTMP,97)
              ELSE
                OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,POSITION='APPEND')
              ENDIF
              
              IF (CNAMM.GT.0.0 .AND. RNAMM.GT.0.0) THEN
                tnamm = cnamm+rnamm
              ELSE
                tnamm = -99.0
              ENDIF
              
              WRITE (FNUMTMP,7402) RUN,EXCODE,TN,RN,CR,
     x         PLDAYP,
     x         MAX(-99,edapm),
     x         tsdapm,
     x         Dapcalc(adatm,plyear,plday),
     x         Dapcalc(mdatm,plyear,plday),
     x         NINT(gwamm),gwummc,
     x         NINT(hnumamm),NINT(hnumgmm),laixm,lnumsmm,tnumamm,
     x         NINT(cwamm),
     x         NINT(vwamm),hiammc, NINT(rwamm),
     x         gnpcmmc,NINT(tnamm),
     x         NINT(cnamm),NINT(gnamm),
     x         hinmmc,pltpop,
     x         NINT(amtnit),
     &         sradav(0),tmaxav(0),tminav(0),NINT(raincp(0))
              
 7402          FORMAT (I6,1X,A10,1X,I3,I3,4X,A2,
     x         I6,
     x         I6,I6,I6,I6,
     x         I6,A6,
     x         I6,I6,F6.1,F6.1,F6.1,
     x         I6,
     x         I6,A6,I6,
     x         A6,I6,
     x         I6,I6,
     x         A6,F6.1,
     x         I6,
     x         3F6.1,I6)
              
              CLOSE(FNUMTMP)       

            ELSE
               OPEN(UNIT=FNUMTMP,FILE='Plantrem.out',STATUS = 'UNKNOWN')
               CLOSE(UNIT=FNUMTMP,STATUS='DELETE')
            ENDIF    ! END OF PLANT RESPONSES (MEASURED)
            
!-----------------------------------------------------------------------

            IF (IDETL.EQ.'A') THEN
            
              ! PLANT ERRORS (A-file data)
              
              WRITE (fnumwrk,*) 'Writing PLANT ERRORS (A)'
              
              FNAMETMP = ' '
              FNAMETMP(1:12) = 'Plantera.'//out
              IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
                OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,STATUS = 'UNKNOWN')
                WRITE (FNUMTMP,'(A21)') '$ERRORS (A-FILE DATA)'
                WRITE (FNUMTMP,501)
                CLOSE(FNUMTMP)
              ENDIF
              
              IF (EXCODE.NE.EXCODEP) THEN
                OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,POSITION='APPEND')
                WRITE (FNUMTMP,996) OUTHED(11:77)
  996           FORMAT (/,'*ERRORS(A):',A67,/)
                WRITE (FNUMTMP,896)
  896           FORMAT ('@  RUN',
     x          ' EXCODE    ',
     B          '   TRNO RN',
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
                CLOSE(FNUMTMP)
              ENDIF
              
              OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,POSITION = 'APPEND')
              
              IF (edatm.GT.0) THEN
               emdaterr = Dapcalc(stgdoy(9),plyear,plday)-
     X         Dapcalc(edatm,plyear,plday)
              ELSE
               emdaterr = -99
              Endif
              IF (adatm.GT.0) THEN
               adaterr = Dapcalc(adat,plyear,plday)-
     X         Dapcalc(adatm,plyear,plday)
              ELSE
               adaterr = -99
              Endif
              IF (mdatm.GT.0) THEN
               mdaterr = Dapcalc(stgdoy(5),plyear,plday)-
     X         Dapcalc(mdatm,plyear,plday)
              ELSE
               mdaterr = -99
              Endif
              IF (hwahm.GT.0 .AND. gwam.GT.0 .AND. hpc.GT.0) THEN
               hwaherr = 100.*(gwam*hpc/100.-hwahm)/(gwam*hpc/100.)
               IF (hwaherr.GT.99999.0) hwaherr = 99999.0
               IF (hwaherr.LT.-9999.0) hwaherr = -9999.0
              ELSE
               hwaherr = -99
              ENDIF
              IF (gwumm.GT.0 .AND. gwum.GT.0) THEN
               gwumerr = 100.0*(gwum-gwumm)/gwum
              ELSE
               gwumerr = -99
              ENDIF
              IF (hnumamm.GT.0 .AND. hnumam.GT.0) THEN
               hnumaerr = 100.0*(hnumam-hnumamm)/(hnumam)
              ELSE
               hnumaerr = -99
              ENDIF
              IF (hnumgmm.GT.0 .AND. hnumgm.GT.0) THEN
               hnumgerr = 100.0*((hnumgm-hnumgmm)/hnumgm)
              ELSE
               hnumgerr = -99
              ENDIF
              IF (laixm.GT.0 .AND. laix.GT.0) THEN
               laixerr = 100.0*((laix-laixm)/laix)
              ELSE
               laixerr = -99
              ENDIF
              IF (lnumsmm.GT.0 .AND. lnumsm.GT.0) THEN
               lnumserr = 100.0*((lnumsm-lnumsmm)/lnumsm)
              ELSE
               lnumserr = -99
              ENDIF
              IF (tnumamm.GT.0 .AND. tnumam.GT.0) THEN
               tnumaerr = 100.0*((tnumam-tnumamm)/tnumam)
              ELSE
               tnumaerr = -99
              Endif
              IF (cwamm.GT.0 .AND. cwam.GT.0) THEN
               cwamerr = 100.0*(cwam-cwamm)/cwam
              ELSE
               cwamerr = -99
              Endif
              IF (vwamm.GT.0 .AND. vwam.GT.0) THEN
               vwamerr = 100.0*(vwam-vwamm)/vwam
              ELSE
               vwamerr = -99
              Endif
              IF (hiamm.GT.0 .AND. hiam.GT.0) THEN
               hiamerr = 100.0*(hiam-hiamm)/hiam
              ELSE
               hiamerr = -99
              Endif
              IF (gnpcmm.GT.0 .AND. gnpcm.GT.0) THEN
               gnpcmerr = 100.0*(gnpcm-gnpcmm)/gnpcm
              ELSE
               gnpcmerr = -99
              Endif
              IF (cnamm.GT.0 .AND. cnam.GT.0) THEN
               cnamerr = 100.0*(cnam-cnamm)/cnam
              ELSE
               cnamerr = -99
              Endif
              IF (gnamm.GT.0 .AND. gnam.GT.0) THEN
               gnamerr = 100.0*(gnam-gnamm)/gnam
              ELSE
               gnamerr = -99
              Endif
              
              WRITE (FNUMTMP,8401) RUN,EXCODE,TN,RN,CR,
     x         edap,emdaterr,
     x         adap,adaterr,
     x         mdap,mdaterr,
     x         NINT(gwam),NINT(hwaherr),
     x         gwumc,NINT(gwumerr),
     x         NINT(hnumam),NINT(hnumaerr),
     x         hnumgm,NINT(hnumgerr),
     x         laix,NINT(laixerr),
     x         lnumsm,NINT(lnumserr),
     x         NINT(tnumam),NINT(tnumaerr),
     x         NINT(cwam),NINT(cwamerr),
     x         NINT(vwam),NINT(vwamerr),
     x         hiamc,NINT(hiamerr),
     x         gnpcm,NINT(gnpcmerr),
     x         NINT(cnam),NINT(cnamerr),
     x         NINT(gnam),NINT(gnamerr)

 8401         FORMAT (I6,1X,A10,1X,I6,I3,4X,A2,
     A         I8,  I8,
     B         I8,  I8,
     C         I8,  I8,
     D         I8,  I8,
     E      2X,A6,  I8,
     F         I8,  I8,
     G         F8.1,I8,
     H         F8.1,I8,
     I         F8.1,I8,
     J         I8  ,I8,
     K         I8,  I8,
     L         I8,  I8,
     M      2X,A6,  I8,
     N         F8.1,I8,
     O         I8,  I8,
     P         I8,  I8)
              
              CLOSE(FNUMTMP)       

            ELSE
               OPEN(UNIT=FNUMTMP,FILE='Plantera.out',STATUS = 'UNKNOWN')
               CLOSE(UNIT=FNUMTMP,STATUS='DELETE')
            ENDIF    ! END OF PLANT ERRORS (A)
           
!-----------------------------------------------------------------------
            
            ! PLANT ERRORS (TIME-COURSE)

            IF (IDETL.EQ.'A') THEN
            
              IF (CFLTFILE.NE.'Y' .OR. FROPADJ.GT.1) THEN
              
                WRITE (fnumwrk,*) 'Cannot write PLANT ERRORS (T)'
                IF (FROPADJ.GT.1)
     &           WRITE (fnumwrk,*) 'Frequency of output > 1 day'  
                IF (RUN.EQ.1 .AND. RUNI.EQ.1) CFLHEAD = 'Y'
              
              ELSE
              
                WRITE (fnumwrk,*) 'Writing PLANT ERRORS (T)'
              
                FNAMETMP = ' '
                FNAMETMP(1:12) = 'Plantert.'//out
                IF (RUN.EQ.1 .AND. RUNI.EQ.1 .OR. CFLHEAD.EQ.'Y') THEN
                 CFLHEAD = 'N'
                 OPEN (UNIT=FNUMTMP,FILE=FNAMETMP,STATUS='UNKNOWN')
                 WRITE (FNUMTMP,'(A21)') '$ERRORS (T-FILE DATA)'
                 WRITE (FNUMTMP,1501)
                 CLOSE(FNUMTMP)
                ENDIF
              
                INQUIRE (FILE = 'PlantGro.OUT',OPENED = fopen)
                IF (fopen) CLOSE (NOUTPG)
              
                STARNUM = 0
                OPEN (UNIT=FNUMT,FILE='Measured.out',STATUS='UNKNOWN')
                DO WHILE (TLINET(1:1).NE.'@')
                  TLINET = ' '
                  READ (FNUMT,1502,END=1600,ERR=1600) TLINET
 1502             FORMAT(A180)
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
                  READ (NOUTPG,'(A180)') TLINEGRO
                  IF (TLINEGRO(1:4).EQ.'*RUN') STARNUM = STARNUM + 1
                  IF (TLINEGRO(1:1).EQ.'@') THEN
                    IF (STARNUM.NE.STARNUMO) THEN
                      TLINEGRO = ' '
                      READ (NOUTPG,'(A180)') TLINEGRO
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
                  WRITE (FNUMWRK,*) 'No columns found in T-file!'
                  GO TO 7777
                ENDIF
              
                ! Make new header line
                TLINETMP = ' '
                TLINETMP(1:1) = '@'
                DO L = 1, TFCOLNUM
                  TLPOS = (L-1)*6+1
                  IF (THEAD(L).EQ.'TRNO'.OR.THEAD(L).EQ.'YEAR'.OR.
     &              THEAD(L).EQ.'DATE') THEN
                    TLINETMP(TLPOS+2:TLPOS+5)=THEAD(L)(1:4)
                  ELSEIF(THEAD(L).EQ.'DOY'.OR.THEAD(L).EQ.'DAP' .OR.
     &              THEAD(L).EQ.'DAS'.OR.THEAD(L).EQ.'DAY') THEN
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
              
                OPEN (UNIT=FNUMTMP,FILE=FNAMETMP,POSITION='APPEND')
              
                 WRITE (FNUMTMP,2996) OUTHED(11:77)
 2996            FORMAT (/,'*ERRORS(T):',A67,/)
                 tlinet(1:1) = '@'
                 WRITE (FNUMTMP,'(A180)') TLINETMP
              
                ! Read data lines, match dates, calculate errors, write
                DO L1 = 1,200
                  TLINET = ' '
                  READ (FNUMT,7778,ERR=7777,END=7777) TLINET
 7778             FORMAT(A180)
                  IF (TLINET(1:1).EQ.'*') GO TO 7777
                  IF (TLINET(1:6).EQ.'      ') GO TO 7776
                  IF (TLINET(1:1).EQ.'!') GO TO 7776
                  CALL Getstri(tlinet,tfdapcol,tfdap) 
                  IF (TFDAP.LE.0) THEN
                    WRITE (FNUMWRK,*) 'DAP in T-file <= 0!'
                    GO TO 7777
                  ENDIF
                  DO WHILE (tfdap.NE.pgdap)
                    TLINEGRO = ' '
                    READ (NOUTPG,7779,ERR=7777,END=7777) TLINEGRO
                    CALL Getstri(tlinegro,pgrocol(tfdapcol),pgdap)
                    IF (PGDAP.LT.0) THEN
                      WRITE (FNUMWRK,*) 'DAP in Plantgro file < 0!'
                      GO TO 7777
                    ENDIF
                  ENDDO
 7779             FORMAT(A180)
                  TLINETMP = ' '
                  DO L = 1, TFCOLNUM
                    CALL Getstrr(tlinet,l,tfval)
                    CALL Getstrr(tlinegro,pgrocol(l),pgval)
                    ERRORVAL = 0.0
                    IF (TFVAL.GT.0.0 .AND. 
     &               PGVAL.NE.-99 .AND.PGVAL.NE.0.0) THEN
                      ERRORVAL = 100.0 * (PGVAL - TFVAL) / PGVAL
                    ELSE
                      ERRORVAL = -99.0
                    ENDIF
                    IF (THEAD(L).EQ.'TRNO'.OR.THEAD(L).EQ.'YEAR' .OR.
     &                THEAD(L).EQ.'DOY'.OR.THEAD(L).EQ.'DAP' .OR.
     &                THEAD(L).EQ.'DAY' .OR.
     &                THEAD(L).EQ.'DAS'.OR.THEAD(L).EQ.'DATE') THEN
                      CALL Getstri(tlinet,l,tvi1)
                      WRITE (TCHAR,'(I6)') TVI1
                    ELSE
                      WRITE (TCHAR,'(I6)') NINT(ERRORVAL)
                    ENDIF
                    TLPOS = (L-1)*6+1
                    TLINETMP(TLPOS:TLPOS+5)=TCHAR
                  ENDDO
                  WRITE (FNUMTMP,'(A180)') TLINETMP
 7776             CONTINUE
                ENDDO
              
 7777           CONTINUE
                GO TO 1601
              
 1600           CONTINUE
                WRITE(fnumwrk,*)'End of file reading Measured.out'
                WRITE(fnumwrk,*)'Starnum and starnumm were: ',         
     &            starnum,starnumm
 1601           CONTINUE
              
                CLOSE (FNUMTMP)      
                CLOSE (FNUMT)
                CLOSE (NOUTPG)
                IF (FOPEN) THEN
                OPEN (UNIT=NOUTPG,FILE='PlantGro.OUT',POSITION='APPEND')
                ENDIF
              ENDIF

            ELSE
               OPEN(UNIT=FNUMTMP,FILE='Plantert.out',STATUS = 'UNKNOWN')
               CLOSE(UNIT=FNUMTMP,STATUS='DELETE')
            ENDIF      ! END OF PLANT ERRORS (TIME-COURSE)
            
!-----------------------------------------------------------------------
            
 8888       CONTINUE   ! Jump to here if cannot write outputs
            
!-----------------------------------------------------------------------
            
            EXCODEP = EXCODE
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,*) 'END OF HARVEST DAY OUTPUTS'
            WRITE (fnumwrk,*) 'WILL BEGIN NEW CYCLE (IF CALLED FOR)'
            WRITE (fnumwrk,*) ' '
            
            SEASENDOUT = 'Y'
 
            ! END MAIN OUTPUTS

            ! Need to re-initialize here because of automatic
            ! fertilization routines in DSSAT
            NFG = 1.0
            NFP = 1.0
            NFT = 1.0
            WFG = 1.0
            WFP = 1.0
            WFT = 1.0

          
            IF (IDETO.EQ.'E'.OR.IDETO.EQ.'N') THEN
                      
            ! CSM SUMMARY ... not used in CROPSIM
            
            ! Store Summary labels and values in arrays to send to
            ! OPSUM routine for printing.  Integers are temporarily
            ! saved as real numbers for placement in real array.
            ! (Nnot done earlier because no Overview called for)
              !Resource productivity calculations
   
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
 
              IF (RAINCP(0) > 1.E-3) THEN
                DMP_Rain = CWAM / RAINCp(0) 
                GrP_Rain = GWAM  / RAINCp(0)
              ENDIF
            
              IF (ETC(0) > 1.E-3) THEN
                DMP_ET = CWAM / ETC(0) 
                GrP_ET = GWAM  / ETC(0) 
              ENDIF
            
              IF (EPC(0) > 1.E-3) THEN
                DMP_EP = CWAM / EPC(0) 
                GrP_EP = GWAM  / EPC(0) 
              ENDIF
 
              IF (TOTIR > 1.E-3) THEN
                DMP_Irr = CWAM / TOTIR 
                GrP_Irr = GWAM  / TOTIR
              ENDIF
 
              IF (ISWNIT.NE.'N') THEN
                IF (Amtnit > 1.E-3) THEN
                  DMP_NApp = CWAM / Amtnit
                  GrP_NApp = GWAM  / Amtnit
                ENDIF
            
                IF (NUAD > 1.E-3) THEN
                  DMP_NUpt = CWAM / NUAD
                  GrP_NUpt = GWAM  / NUAD
                ENDIF
              ENDIF !ISWNIT == 'Y'
            
              LABEL(1) = 'ADAT '; VALUE(1) = FLOAT(adat)
              LABEL(2) = 'MDAT '; VALUE(2) = FLOAT(stgdoy(5))
              LABEL(3) = 'DWAP '; VALUE(3) = sdrate
              LABEL(4) = 'CWAM '; VALUE(4) = cwam
              LABEL(5) = 'HWAM '; VALUE(5) = gwam
              LABEL(6) = 'HWAH '; VALUE(6) = gwam * hpc / 100.
              LABEL(7) = 'BWAH '; VALUE(7) = vwam * hbpc / 100. 
            
              LABEL(8)  = 'HWUM '; VALUE(8) = gwum
              LABEL(9)  = 'H#AM '; VALUE(9) = hnumam
              LABEL(10) = 'H#UM '; VALUE(10) = hnumgm
              LABEL(11) = 'NUCM '; VALUE(11) = nuad
              LABEL(12) = 'CNAM '; VALUE(12) = cnam
              LABEL(13) = 'GNAM '; VALUE(13) = gnam
              LABEL(14) = 'PWAM '; VALUE(14) = PWAM    
              LABEL(15) = 'LAIX '; VALUE(15) = LAIX    
              LABEL(16) = 'HIAM '; VALUE(16) = HIAM    
 
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
 
              LABEL(29) = 'EDAP ' ; VALUE(29) = FLOAT(EDAP)     
              
              LABEL(30) = 'NDCH ' ; VALUE(30) = FLOAT(DAYSC(0)) 
              LABEL(31) = 'TMINA' ; VALUE(31) = TMINAV(0)       
              LABEL(32) = 'TMAXA' ; VALUE(32) = TMAXAV(0)       
              LABEL(33) = 'SRADA' ; VALUE(33) = SRADAV(0)       
              LABEL(34) = 'DAYLA' ; VALUE(34) = DAYLAV(0)       
              LABEL(35) = 'CO2A ' ; VALUE(35) = CO2AV(0)        
              LABEL(36) = 'PRCP ' ; VALUE(36) = RAINCP(0)       
              LABEL(37) = 'ETCP ' ; VALUE(37) = ETC(0)      

              IF (FILEIOT(1:2).EQ.'DS') CALL SUMVALS(SUMNUM,LABEL,VALUE)
            ENDIF

            ! To prevent massive Work.out files
            IF (FILEIOT.EQ.'DS4') CLOSE(FNUMWRK)

          ENDIF

        ENDIF        ! YEARDOY >= YEARPLT


      ELSEIF (DYNAMIC.EQ.SEASEND) THEN

        CLOSE (NOUTPG)
        INQUIRE (FILE = OUTPN, EXIST = FEXIST)
        IF (FEXIST) CLOSE (NOUTPN)
        CLOSE (NOUTPG2)
        CLOSE (NOUTPGF)
        CLOSE (FNUMWRK)

      ENDIF   ! Tasks

      cswdis = 'N'
!     IF(cswdis.EQ.'Y')
!    X CALL Disease(   If needed must check argument variables  )

  206          FORMAT(
     &          /,"*MAIN GROWTH AND DEVELOPMENT VARIABLES",//,   
     &             "@",5X,"VARIABLE",T44,"SIMULATED     MEASURED",/,  
     &                 6X,"--------",T44,"---------     --------")  
  270 FORMAT(/,'------------------------------------------------------',
     &'--------------------------------------------------------')
  300       FORMAT(/,10X,A," YIELD : ",I8," kg/ha    [Dry weight] ",/)

  501  FORMAT(/,
     & '! This file presents the differences between simulated and',/,
     & '! single-time measured values (eg.date of anthesis,yield) for'/,
     & '! individual runs. The abbreviations are based on those',/,
     & '! listed in the DATA.CDE file, with the simple abbreviation',/,
     & '! indicating the simulated value,the basic abbreviation plus',/,
     & '! a final E the error. The units for the latter are days for',/,
     & '! time differences (EDAP,ADAP,MDAP) and percentages of',/,
     & '! simulated values for the remainder.,'/,
     & ' ',/,
     & '! As usual, a -99 indicates that no data were available.')
     
  502  FORMAT(/,
     & '! This file summarizes the error differences reported in',/,
     & '! the files PLANTERA.OUT and PLANTERT.OUT. The errors in',/,
     & '! these files have been averaged taking actual values first,'/,
     & '! then the absolute value of the original errors. The number',/,
     & '! of runs used is indicated in the RUNS column.',/,
     & ' ',/,
     & '! The abbreviations are based on those listed in the',/,
     & '! DATA.CDE file, with the abbreviation plus a final #',/,
     & '! indicating the number of values actually used for',/,
     & '! averaging, the basic abbreviation plus a final E the',/,
     & '! averaged error. The units for the latter are days for',/,
     & '! time differences (EDAP,ADAP,MDAP) and percentages of',/,
     & '! simulated values for the remainder.',/,
     & ' ',/,
     & '! The major heading (the * line) includes codes for model',/,
     & '! and plant module. The batch column, as generated, is',/,
     & '! filled with a 1, and the batchcode with the code for',/,
     & '! the last experiment in the PLANTERA/T.OUT files. The',/,
     & '! entries in these columns can (and should) be changed if',/,
     & '! an overall summary file is constructed by combining files',/,
     & '! from different model and module runs.',/,
     & ' ',/,
     & '! As usual, a -99 indicates that no data were available.')

 1501  FORMAT(/,
     & '! This file summarizes the differences between simulated',/,
     & '! and measured values for individual runs. Abbreviations',/,
     & '! are based on those listed in the DATA.CDE file, but with',/,
     & '! an E added to indicate error. The units for the errors',/,
     & '! are % of simulated values (ie.100*[SIM-MEAS]/SIM).'/,
     & ' ',/,
     & '! A -99 indicates that no data were available. Here, this'/,
     & '! could be simulated as well as measured data.')

 1200       FORMAT(
     &      '------------------------------------------------------',
     &      '--------------------------------------------------------',
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

 5500  FORMAT(/,'*ENVIRONMENTAL AND STRESS FACTORS',//,
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

 9588       FORMAT(
     &      /,' ...... DATE ......  GROWTH STAGE BIOMASS   LEAF  
     &     CROP N      STRESS')     
 9589         FORMAT(//,
     &     '*SIMULATED CROP AND SOIL STATUS AT MAIN DEVELOPMENT STAGES')
 9600       FORMAT(' YEARDOY DOM MON DAP ............. kg/ha AREA NUMBER
     &  kg/ha   %   H2O    N')

      END  ! CSCER040

!=======================================================================
!  CSTRANS Subroutine
!  Calculates potential plant evaporation (ie.transpiration) rate
!-----------------------------------------------------------------------
!  Revision history
!  01/01/89 JR  Written
!  01/01/89 JWJ Modified for climate change using ETRATIO subroutine.
!  12/05/93 NBP Made into subroutine and changed to TRATIO function.
!  10/13/97 CHP Modified for modular format.
!  11/25/97 CHP Put in file TRANS.for w/ TRATIO and BLRRES
!  09/01/99 GH  Incorporated into CROPGRO
!  01/13/00 NBP Added DYNAMIC contruct to input KCAN
!  04/09/01 LAH Modified for CROPGRO-SIM
!=======================================================================

      SUBROUTINE CSTRANS(ISWWAT,                          !Control
     & TMAX, TMIN, WINDSP, CO2, EO,                       !Weather
     & CROP, LAI, KEP,                                    !Crop,LAI
     & eop,                                               !Pot.pl.evap
     & DYNAMICI)                                          !Control

      IMPLICIT NONE
      SAVE

      REAL          BLRESD        ! Boundary layer resistance      s/m
      REAL          BLRESE1       ! Boundary layer resistance      s/m
      REAL          BLRESE2       ! Boundary layer resistance      s/m
      REAL          BLRESE3       ! Boundary layer resistance      s/m
      REAL          BLRESE4       ! Boundary layer resistance      s/m
      REAL          BLRESE5       ! Boundary layer resistance      s/m
      REAL          BLRESE6       ! Boundary layer resistance      s/m
      REAL          BLRESEN       ! Boundary layer resistance      s/m
      REAL          BLRESRC1      ! Boundary layer resistance      s/m
      REAL          BLRESRC2      ! Boundary layer resistance      s/m
      REAL          BLRESRS1      ! Boundary layer resistance      s/m
      REAL          BLRESRS2      ! Boundary layer resistance      s/m
      REAL          BLRESX        ! Boundary layer resistance      s/m
      REAL          BLRESZ0       ! Boundary layer resistance      s/m
      REAL          BLRESZ1       ! Boundary layer resistance      s/m
      REAL          CHIGHT        ! Reference height for crop      m
      REAL          CO2           ! CO2 concentration in air       vpm
      CHARACTER*2   CROP          ! Crop identifier (ie. WH, BA)   text
      REAL          DELTA         ! Slope,sat vapor pres/tem curve Pa/K
      INTEGER       DYNAMICI      ! Module control,internal        code
      REAL          EO            ! Potential evapotranspiration   mm/d
      REAL          EOP           ! Potential evaporation,plants   mm/d
      REAL          GAMMA         ! Variable in Penman formula     #
      CHARACTER*1   ISWWAT        ! Soil water balance switch Y/N  code
      REAL          KEP           ! Extinction coeff for SRAD      #
      REAL          LAI           ! Leaf area index                #
      REAL          LAIMAX        ! Leaf area index,maximum        #
      REAL          LHV           ! Latent heat of vaporization    J/g
      REAL          RA            ! Atmospheric resistance         s/m
      REAL          RATIO         ! Ratio of LAI to maximum LAI    #
      REAL          RB            ! Leaf resistance addition fac   s/m
      REAL          RL            ! Canopy resistance for CO2      s/m
      REAL          RLC           ! Canopy resistance,actual CO2   s/m
      REAL          RLF           ! Leaf stomatal res,330.0 ppmCO2 s/m
      REAL          RLFC          ! Leaf stomatal resistance       s/m
      INTEGER       RUNINIT       ! Control variable,initiation    #
      REAL          TMAX          ! Temperature maximum            C
      REAL          TMIN          ! Temperature minimum            C
      REAL          TRATIO        ! Function,relative tr rate      #
      REAL          UAVG          ! Average wind speed             m/s
      REAL          VPSLOP        ! Slope,sat vapor pres/tem curve Pa/K
      REAL          WINDSP        ! Wind speed                     km/d
      REAL          XDEN          ! Transpiration,actual CO2       g/m2
      REAL          XNUM          ! Transpiration,standaard CO2    g/m2

      PARAMETER     (RUNINIT=1)

      EOP = 0.0
      DYNAMICI = DYNAMICI    ! Temporary until used for initialisation

      IF(ISWWAT.EQ.'Y')THEN
        IF (LAI .LT. 0.01) THEN
          TRATIO = 1.0
          GO TO 9999    ! Don't calculate tratio if LAI very small
        ENDIF

        ! Initialize.
        IF (WINDSP .LE. 0.0) WINDSP = 86.4
        UAVG = WINDSP / 86.4
        RB = 10.0
        LAIMAX = 3.5

        ! Set canopy height
        CHIGHT = 1.0

        ! Canopy resistances, RL and RLC.
        ! RLF = Leaf stomatal resistance at 330.0 ppm CO2, s/m
        ! RLFC = Leaf stomatal resistance at other CO2 conc., s/m
        ! (Allen, 1986), Plant responses to rising CO2.
        IF (INDEX('MZMLSG',CROP) .GT. 0) THEN
           ! C-4 Crops  EQ 7 from Allen (1986) for corn.
          RLF =(1.0/(0.0328 - 5.49E-5*330.0 + 2.96E-8 * 330.0**2))+RB
          RLFC=(1.0/(0.0328 - 5.49E-5* CO2  + 2.96E-8 * CO2  **2))+RB
        ELSE
          ! C-3 Crops
          RLF  = 9.72 + 0.0757 * 330.0 + 10.0
          RLFC = 9.72 + 0.0757 *  CO2  + 10.0
        ENDIF

        RL = RLF / LAI
        RLC = RLFC / LAI

        ! Boundary layer resistance (Jagtap and Jones, 1990).
        BLRESEN = 3.0
        BLRESZ1 = 0.01
        BLRESX = 2.0
        BLRESD = 0.7 * CHIGHT**0.979
        BLRESZ0 = 0.13 * CHIGHT**0.997

        BLRESE1 = EXP(BLRESEN*(1. - (BLRESD + BLRESZ0) / CHIGHT))
        BLRESE2 = EXP(BLRESEN)
        BLRESE3 = CHIGHT/(BLRESEN*(CHIGHT-BLRESD))
        BLRESE4 = ALOG((BLRESX - BLRESD)/BLRESZ0)
        BLRESE5 = 0.4 * 0.4 * UAVG
        BLRESE6 = ALOG((BLRESX - BLRESD) / (CHIGHT - BLRESD))

        BLRESRS2 = BLRESE4 * BLRESE3 * (BLRESE2 - BLRESE1)/BLRESE5
        BLRESRC2 = BLRESE4*(BLRESE6+BLRESE3*(BLRESE1-1.))/BLRESE5
        BLRESRS1 = ALOG(BLRESX/BLRESZ1)*
     &             ALOG((BLRESD+BLRESZ0)/BLRESZ1)/BLRESE5
        BLRESRC1 = (ALOG(BLRESX/BLRESZ1)**2)/BLRESE5
        BLRESRC1 = BLRESRC1-BLRESRS1

        RATIO = LAI/LAIMAX
        IF (RATIO .GT. 1.0) RATIO = 1.0
        RA = BLRESRC1 + (BLRESRC2 - BLRESRC1) * RATIO

        ! Transpiration ratio (CO2=330 vpm gives 1.0)
        DELTA = VPSLOP((TMAX+TMIN)/2.0) / 100.0
        LHV    = 2500.9 - 2.345*(TMAX+TMIN)/2.0
        GAMMA  = 1013.0*1.005/(LHV*0.622)
        XNUM = DELTA + GAMMA*(1.0+RL/RA)
        XDEN = DELTA + GAMMA*(1.0+RLC/RA)
        TRATIO = XNUM / XDEN

 9999   CONTINUE

        EOP = EO * (1.0-EXP(-LAI*KEP)) * TRATIO
        EOP = MAX(EOP,0.0)

      ENDIF

      RETURN

      END  ! CSTRANS

!=======================================================================
!  CSROOTWU Subroutine
!  Root water uptake rate for each soil layer and total rate.
!-----------------------------------------------------------------------
!  Revision history
!  01/01/89 JR  Written
!  12/05/93 NBP Made into subroutine.
!  01/18/96 JWJ Added flooding effect on water uptake
!  01/06/96 GH  Added soil water excess stress
!  10/10/97 CHP Updated for modular format.
!  09/01/99 GH  Incorporated in CROPGRO
!  01/10/00 NBP Added SAVE for stored variables and set SWCON2=RWU=0.0
!  01/12/00 NBP Removed FILECC from input
!  01/25/00 NBP Added IOSTAT to READ statements to set ERRNUM.  Cleaned.
!  05/09/01 LAH Modified for CROPSIM
!=======================================================================

      SUBROUTINE CSROOTWU(ISWWAT,                          !Control
     & NLAYR, DLAYR, LL, SAT,                              !Soil
     & EOP,                                                !Pot.evap.
     & RLV, RWUPM, RWUMX,                                  !Crop state
     & SW,                                                 !Soil h2o
     & UH2O, TRWUP,                                        !H2o uptake
     & DYNAMICI)                                           !Control

      IMPLICIT NONE
      SAVE

      INTEGER       NL            ! Maximum number soil layers,20
      PARAMETER     (NL=20)       !

      REAL          BLAYER        ! Depth at base of layer         cm
      INTEGER       DYNAMICI      ! Module control,internal        code
      REAL          DLAYR(20)     ! Depth of soil layers           cm
      REAL          EOP           ! Potential evaporation,plants   mm/d
      INTEGER       FNUMWRK       ! File number,work file          #
      CHARACTER*1   ISWWAT        ! Soil water balance switch Y/N  code
      INTEGER       L             ! Loop counter                   #
      REAL          LL(NL)        ! Lower limit,soil h2o           #
      INTEGER       NLAYR         ! Actual number of soil layers   #
      REAL          RWUPM         ! Pore space threshold,pl effect #
      REAL          RLV(20)       ! Root length volume by layer    #
      REAL          RLVSUM        ! Temporary RLV sum              #
      INTEGER       RUNINIT       ! Control variable,initiation    #
      REAL          RWU(20)       ! Root water uptake by layer     mm/d
      REAL          RWUMX         ! Root water uptake,maximum      mm2/m
      REAL          RWUP          ! Root water uptake,potential    cm/d
      REAL          SAT(20)       ! Saturated limit,soil           #
      REAL          SATFACL       ! Soil water excess stress factr #
      REAL          SW(20)        ! Soil water content             #
      REAL          SWAFOLD       ! Soil water availability CERES  #
      REAL          SWCON1        ! Constant for root water uptake #
      REAL          SWCON2(NL)    ! Variable for root water uptake #
      REAL          SWCON3        ! Constant for root water uptake #
      REAL          TRWUP         ! Total water uptake,potential   cm
      REAL          TSS(NL)       ! Number of days saturated       d
      REAL          UH2O(NL)      ! Uptake of water                cm/d
      REAL          WFSAT         ! Soil water excess stress fact  #
      REAL          WUF           ! Water uptake factor            #
      REAL          WUP(NL)       ! Water uptake                   cm/d
      REAL          WUT           ! Water uptake,total             cm/d

      PARAMETER     (RUNINIT=1)


      IF (ISWWAT.NE.'Y') RETURN

      IF (DYNAMICI.EQ.RUNINIT) THEN

        CALL Getlun('WORK.OUT',fnumwrk)

        ! Compute SWCON2 for each soil layer.  Adjust SWCON2 for very
        ! high LL to avoid water uptake limitations.
        SATFACL = 1.0
        DO L = 1,NL
          SWCON2(L) = 0.0
          RWUP = 0.0
          RWU(L) = 0.0
        ENDDO
        DO L = 1,NLAYR
          SWCON2(L) = 120. - 250. * LL(L)
          IF (LL(L) .GT. 0.30) SWCON2(L) = 45.0
        ENDDO

        ! Set SWCON1 and SWCON3.
        SWCON1 = 1.32E-3
        SWCON3 = 7.01

      ENDIF

      TRWUP   = 0.0
      RLVSUM = 0.0
      BLAYER = 0.0

      DO L = 1,NLAYR
        BLAYER = BLAYER + DLAYR(L)
      ENDDO

      DO L = 1,NLAYR
        IF (RLV(L).LE.0.00001 .OR. SW(L).LE.LL(L)) THEN
          RWUP = 0.
        ELSE
          RWUP = SWCON1*EXP(MIN((SWCON2(L)*(SW(L)-LL(L))),40.))/
     &    (SWCON3-ALOG(RLV(L)))
          ! Excess water effect
          ! RWUPM = Minimum pore space required for supplying oxygen
          ! TSS(L) = number of days soil layer L has been saturated
          IF ((SAT(L)-SW(L)) .GE. RWUPM) THEN
            TSS(L) = 0.
          ELSE
            TSS(L) = TSS(L) + 1.
          ENDIF
          ! 2 days after saturation before water uptake is affected
          IF (TSS(L).GT.2.0 .AND. RWUPM.GT.0.0) THEN
             SATFACL = MIN(1.0,MAX(0.0,(SAT(L)-SW(L))/RWUPM))
          ELSE
             SATFACL = 1.0
          ENDIF
          WFSAT = 1.0 - (1.0-SATFACL)       
          RWUP = MIN(RWUP,RWUMX*WFSAT)
          RWUP = MIN(RWUP,RWUMX)
        ENDIF
        IF (RLV(L).GT.0.0) THEN
          SWAFOLD = AMIN1(1.0,AMAX1(0.0,(RWUP*RLV(L))/(SW(L)-LL(L))))
          WUP(L) = SWAFOLD*DLAYR(L)*(SW(L)-LL(L))
          TRWUP = TRWUP+WUP(L)
        ELSE
          WUP(L) = 0.0
        ENDIF
      ENDDO

      IF (TRWUP .GT. 0.0) THEN
        IF (EOP*0.1 .GE. TRWUP) THEN
          WUF = 1.0
        ELSE
          WUF = (EOP*0.1) / TRWUP
        ENDIF
        WUT = 0.0
        DO L = 1, NLAYR
          UH2O(L) = WUP(L) * WUF
          WUT = WUT + UH2O(L)
        END DO
      ELSE        !No root extraction of soil water
        WUT = 0.0
        DO L = 1,NLAYR
          UH2O(L) = 0.0
        ENDDO
      ENDIF

      RETURN

      END  ! CSROOTWU

!=======================================================================
!  CSLAYERS Subroutine
!  Leaf distribution module
!-----------------------------------------------------------------------
!  Revision history
!  1. Written for Cropsim                         L.A.H.      ?-?-98
!=======================================================================

      SUBROUTINE Cslayers
     X (chtpc,clapc,              ! Canopy characteristics
     X pltpop,lai,canht,          ! Canopy aspects
     X lcnum,lap,lap0,            ! Leaf cohort number and size
     X LAIL)                      ! Leaf area indices by layer

      IMPLICIT NONE
      SAVE

      INTEGER       clx           ! Canopy layers,maximum          #
      INTEGER       lcx           ! Leaf cohort number,maximum     #
      PARAMETER     (clx=30)      ! Canopy layers,maximum          #
      PARAMETER     (lcx=100)     ! Leaf cohort number,maximum     #

      REAL          caid          ! Canopy area index              m2/m2
      REAL          canfr         ! Canopy fraction                #
      REAL          canht         ! Canopy height                  cm
      !REAL         clpos         ! Canopy layer,temporary         #
      REAL          clthick       ! Canopy layer thickness,temp    cm
      !INTEGER      cltot         ! Canopy layer number,total      #
      INTEGER       clnum         ! Canopy layer number,species    #
      !REAL         cltotfr       ! Canopy fraction in top layer   #
      !INTEGER      cn            ! Component                      #
      REAL          chtpc(10)     ! Canopy ht % assoc with lf area %
      INTEGER       l             ! Loop counter                   #
      REAL          clapc(10)     ! Canopy lf area % w height      %
      REAL          lai           ! Leaf lamina area index         m2/m2
      !REAL         lailad(clx)   ! Lf lamina area index,active    m2/m2
      REAL          lailatmp      ! Leaf lamina area,active,temp   m2/m2
      REAL          lail(clx)     ! Leaf lamina area index         m2/m2
      REAL          lailtmp       ! Leaf lamina area,temporary     m2/m2
      REAL          lap(lcx)      ! Leaf lamina area,cohort        cm2/p
      REAL          lap0          ! Leaf lamina area gr,cohort     cm2/p
      REAL          lapp(lcx)     ! Leaf lamina area,infected      cm2/p
      REAL          laps(lcx)     ! Leaf lamina area,senescent     cm2/p
      INTEGER       lcnum         ! Leaf cohort number             #
      INTEGER       lcnumr        ! Leaf cohorts remaining         #
      REAL          lfrltmp       ! Leaves above bottom of layer   fr
      REAL          lfrutmp       ! Leaves above top of layer      fr
      REAL          pltpop        ! Plant population               #/m2
      INTEGER       tvi1          ! Temporary variable,integer     #
      !INTEGER      tvilc         ! Temporary value,lf cohort      #
      REAL          YVALXY        ! Y value from function          #

!-----------------------------------------------------------------------

      caid = lai
      DO L = 1,25
        LAPP(L) = lap(l)  ! Temporary - avoid warning
        LAPS(L) = 0.0
      ENDDO

!-----------------------------------------------------------------------

      IF (caid.LT.0.0) RETURN
      IF (canht.LE.0.0) RETURN

!-----------------------------------------------------------------------

      ! Establish layer thickness (Must be ok for tallest species!)
      clthick=10.0                     ! Layer thickness (cm)

!-----------------------------------------------------------------------

      ! Determine layer number for species
      IF(MOD(canht,clthick).GT.0)THEN
       clnum=AINT(canht/clthick)+1
      ELSE
       clnum=AINT(canht/clthick)
      ENDIF
      clnum = MAX(clx,clnum)

      ! Distribute leaf area over layers

      DO tvi1=30,1,-1                  ! Do all layers;top down (old=1)
       lail(tvi1)=0.0                  ! Lai by layer
      ENDDO

      lfrutmp=1.0

      lfrutmp=0.0
      DO tvi1=clnum,1,-1               ! Do over layers down to soil
       canfr = (canht-(clthick*(tvi1-1))) / canht
       IF (canfr.GT.0.0) THEN
         lfrltmp = YVALXY(CHTPC,CLAPC,CANFR)
         lail(tvi1)=lai*(lfrltmp-lfrutmp)
         lfrutmp=lfrltmp
        ENDIF
      ENDDO

!-----------------------------------------------------------------------

      ! Calculate active leaf area in layers
      ! Disease damage

      lcnumr=lcnum
      lailtmp=lap0*pltpop*0.0001
      lailatmp=lap0*pltpop*0.0001

c     DO tvi1=cltot,1,-1               ! Do over layers down to soil
c      lail(tvi1)=lailtmp
c      lailad(tvi1)=lailatmp
c      lailtmp=0.0
c      lailatmp=0.0
c
c      DO tvilc=lcnumr,0,-1            ! Do damage,living cohorts
c       IF(tvilc.GT.0)THEN
c        lail(tvi1)=lail(tvi1)+
c    x   (lap(tvilc)-laps(tvilc))*pltpop*0.0001
c        lailad(tvi1)=lailad(tvi1)+
c    x   (lap(tvilc)-laps(tvilc)-lapp(tvilc))*pltpop*0.0001
c        ! Could adjust above for effect on activity as well as area -
c        ! ie multiply by a 0-1 factor dependent on pathogen and area
c       ENDIF
c
c       IF(caid.GT.0.AND.
c    x  lail(tvi1).GE.cails(cn,tvi1)*(lai/caid))THEN
c        lailtmp=lail(tvi1)-cails(cn,tvi1)*(lai/caid)
c        lail(tvi1)=lail(tvi1)-lailtmp
c        IF(tvilc.GT.0)THEN
c         IF(lap(tvilc)-laps(tvilc).GT.0)THEN
c          lailatmp=lailtmp*
c    x     (lap(tvilc)-laps(tvilc)-lapp(tvilc))/
c    x     (lap(tvilc)-laps(tvilc))
c          lailad(tvi1)=lailad(tvi1)-lailatmp
c         ENDIF
c        ENDIF
c        EXIT
c       ENDIF
c
c      ENDDO
c      lcnumr=tvilc
c      ! End damaged and senescent area section
c
c     ENDDO

      RETURN

      END  ! CSLAYERS


