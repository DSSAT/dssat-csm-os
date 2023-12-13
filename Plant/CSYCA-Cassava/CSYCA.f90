!*******************************************************************************************************************************
!      CROPSIM CASSAVA GROWTH AND DEVELOPMENT MODULE V2014-1 by LA Hunt, modified for CROPSIM from the GUMCAS model 
!      of Matthews and Hunt, Field Crops Research, 36 (1994) 69-84.
!  
!      The original CSCAS code was preceded by the header:
!          CROPSIM CASSAVA GROWTH AND DEVELOPMENT MODULE V2014-1  
!          Last edit 040414 LAH 
!         (After problems implementing fixes of GH 180214, 260314; LPM, MJF 260214).
!    
!     The code of CSCAS.FOR version V2014-1 (see header above) was modified into 42 subroutines and 2 modules by 
!     MJF August-October, 2014. The modified code gave identical results with the same X-file as the original
!     version. The structure follows the original CSCAS. Some additional checks were included for WORK.OUT, which
!     USE YCA_Albedo_Check_m.
!       
!     All variables, apart from those that are arguments of CSCAS, are declared in the module YCA_First_Trans_m.f90,
!     which is USEd in the subsequent subroutines. Most numbered FORMAT statements were converted to character
!     variables of the form FRMnnnn, where nnnn is the number of the original FORMAT statement and put in a module
!     YCA_Formats_m.f90, which is USEd as necessary.
!       
!     29OC14:  The new version of the original CSCAS.FOR (see above) was converted into .f90 format.  CSCAS.f90 gave
!     the same output as the original CSCAS.FOR and then the new subroutines were added one by one to verify that
!     the output did not change. The output of this final version was the same in all 26 .OUT files apart from the
!     time stamp of each run.
!     
!     2023-01-26 CHP Reduce compile warnings: add EXTERNAL stmts, remove 
!                 unused variables, shorten lines.                    
!*******************************************************************************************************************************

      SUBROUTINE  CSYCA (FILEIOIN, RUN, TN, RN, RNMODE,                                       & !Command line
       ISWWAT, ISWNIT, ISWDIS, MESOM, IDETS, IDETO, IDETG, IDETL, FROP,                       & !Controls
       SN, ON, RUNI, REP, YEAR, DOY, STEP, CN,                                                & !Run+loop
       SRAD, TMAX, TMIN, TAIRHR, RAIN, CO2, TDEW,                                             & !Weather
       DRAIN, RUNOFF, IRRAMT,                                                                 & !Water
       DAYL, DEWDUR, CLOUDS, ST, EO,                                              & !Weather        !TWILEN = DAYL; SOILTEMP = ST 
       NLAYR, DLAYR, DEPMAX, LL, DUL, SAT, BD, SHF, SLPF,                                     & !Soil states
       SW, NO3LEFT, NH4LEFT, FERNIT,                                                          & !H2O,N states
       TLCHD, TNIMBSOM, TNOXD, TOMINFOM, TOMINSOM, TOMINSOM1, TOMINSOM2, TOMINSOM3,           & !N components
       YEARPLTCSM, HARVFRAC,                                                                  & !Pl.date         ! YRPLT = YEARPLTCSM
       PARIP, PARIPA, EOP, EP, ET, TRWUP, ALBEDOS,                                            & !Resources       ! REPLACED ALBEDO WITH ALBEDOS
       LAI, KCAN, KEP, RLV, NFP, RWUPM, RWUMX, CANHT, LAIL, LAILA,                           & !States          ! PORMIN = RWUMP
       UNO3, UNH4, UH2O,                                                                      & !Uptake
       SENCALG, SENNALG, SENLALG,                                                             & !Senescence
       RESCALG, RESNALG, RESLGALG,                                                            & !Residues
       STGYEARDOY, BRSTAGE,                                                                   & !Stage dates     !GSTAGE = BRSTAGE
       WEATHER     , SOILPROP    , CONTROL     ,                                              & 
       DYNAMIC) !, WEATHER)                                                                        !Control

! 2023-01-25 chp removed unused variables
!       WINDSP, ES, 

    USE ModuleDefs
    USE YCA_First_Trans_m
    USE YCA_Albedo_Check_m                                                                     ! MF 18JA15 For WORK.OUT

      
    IMPLICIT NONE
    EXTERNAL YCA_RunInit, YCA_SeasInit, YCA_Growth, YCA_Integrate, YCA_Output
      
    TYPE (ControlType), intent (in) :: CONTROL    ! Defined in ModuleDefs
    TYPE (WeatherType), intent (in) :: WEATHER    ! Defined in ModuleDefs
    TYPE (SoilType), intent (in) ::   SOILPROP   ! Defined in ModuleDefs
    
    INTEGER :: CN       , DOY         , DYNAMIC     , FROP        , NLAYR       , ON          , REP        , RN          
    INTEGER :: RUN      , RUNI        , SN          , STEP        , STGYEARDOY(0:19)            , TN         , YEAR
    INTEGER :: YEARPLTCSM 
!   INTEGER :: CSTIMDIF , CSYDOY      , DAPCALC     , TVICOLNM    , TVILENT     , CSIDLAYR    , CSYEARDOY              ! Integer function calls

    REAL    ALBEDOS     , BD(NL)      , BRSTAGE     , LAI         , CANHT       , CLOUDS      , CO2         , DAYL      ! REPLACED ALBEDO WITH ALBEDOS
    REAL    DEPMAX      , DEWDUR      , DLAYR(NL)   , DRAIN       , DUL(NL)     , EO          , EOP         , EP          
    REAL    ET          , FERNIT      , HARVFRAC(2) , IRRAMT      , KCAN        , KEP         , LAIL(30)    ! ES          , 
    REAL    LAILA(30)   , LL(NL)      , NFP         , NH4LEFT(NL) , NO3LEFT(NL) , PARIP       , PARIPA      , RAIN        
    REAL    RESCALG(0:NL)             , RESLGALG(0:NL)            , RESNALG(0:NL)             , RLV(NL)     , RUNOFF      
    REAL    RWUMX       , RWUPM       , SAT(NL)     , SENCALG(0:NL)             , SENLALG(0:NL)             , TDEW
    REAL    SENNALG(0:NL)             , SHF(NL)     , SLPF        , SRAD        , ST(NL)      , SW(NL)       
    REAL    TLCHD       , TAIRHR(24)  , TMAX        , TMIN        , TNIMBSOM    , TNOXD       , TOMINFOM    , TOMINSOM                                                   
    REAL    TOMINSOM1   , TOMINSOM2   , TOMINSOM3   , TRWUP       , UH2O(NL)    , UNH4(NL)    , UNO3(NL)    ! , WINDSP      
!    REAL    PARHR(24)   , RADHR(24)   , RHUMHR(24)  , VPDHR(24)                                                        !MF 14SE14 Hourly weather data
!   REAL    CSVPSAT     , TFAC4       , TFAC5       ,YVALXY      , CSYVAL                                               ! Real function calls !LPM 15sep2017 Added TFAC5 

    CHARACTER(LEN=1)  :: IDETG, IDETL, IDETO, IDETS, ISWDIS, ISWNIT, ISWWAT      
    CHARACTER(LEN=1)  :: MESOM, RNMODE      
    CHARACTER(LEN=120):: FILEIOIN    
!   CHARACTER(LEN=10) :: TL10FROMI                                                                                     ! Character function call

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
      ! For incorporation in CSM should:
      !   Ensure that Alt-plant routine:
      !   Sending control ISWDIS
      !   Sending DEWDUR(=-99.0?), CLOUDS(=0.0?), ES, ALBEDO(=0.2)
      !   Setting dummies for PARIP, PARIPA, LAIL, LAILA
      !   Eliminate '!' from SUMVALS call.

      ! And to run well in CSM should:
      !    Read ICIN (with -99 in profile) and ICSW variables
      
      ! Changes 8->151213:
      !    1. Individual leaf area expansion routine. More individual leaf sizes output to LEAVES.OUT (No stress, H2o,N,Temp.
      !       stress, H2o,N,Temp+assimilate limitation, H2o,N,Temp+ assimilate+N concentration maintenance)  

      !    2. Leaf accelerated senescence function (for when loser canopy in deep shade). Now accelerates ageing in 
      !       active leaf phase, does not 'chop-off' leaf area. Amount of acceleration specified in SPE file.     
      
      ! Changes 140812 to make cassava work ok:
      !    1. Definition of DUTOMSTG changed. Before included PD(mstg), now stops at MSTG so that DSTAGE =  LSTAGE for cassava
      !    2. Stem N growth from leaves algorithm not woerking well. Changed to make sure that only works when stem N below 
      !       minimum and leaf N above minimum

      ! Temporary changes .. need firming-up:
      !    Height increase on emergence day has initialisation value

      ! Changes for cassava
      !    1  Product read-in from species file;product->harvest outputs
      !    2  Stages read-in from species file
      !    3. Temperature and water stress effects on specific leaf area
      !    4. Leaf appearance rate reduction with leaf number
      !    5. PTF parameters derived from root/shoot function !LPM 19MAY2015 PTF is not considered in the model 
      !    6. Memory water stress factor calculated from WFG->leaf area
      !    7. Storage root initiation period introduced
      !    8. Storage root number initiation parameter introduced
      !    9. Storage root basic fraction parameter introduced
      !   10. Photoperiod sensitivity type read-in from species file
      !   11. Storage root N added
      !   12. Protection of N supply for leaf growth added. (Problem with N on growth because lack of N->reduced growth->
      !       reduced uptake!
      
        ALBEDOS_In = ALBEDOS                                                                                           ! MF 19JA15  For WORK.OUT
    
!*******************************************************************************************************************************
    IF (DYNAMIC == RUNINIT) THEN    ! Initialization                           ! MF Equivalent to line 1476 in CSCAS.FOR
!*******************************************************************************************************************************
        !-----------------------------------------------------------------------------------------------------------------------
        !     Run initialization procedure in SUBROUTINE YCA_RunInit..
        !     SUBROUTINE YCA_RunInit takes all the code from CSCAS.f90 (29OC14) lines 1490 - 1827.
        !-----------------------------------------------------------------------------------------------------------------------

        CALL YCA_RunInit (&
            CN          , DOY         , FILEIOIN    , FROP        , IDETL       , ISWNIT      , ON          , RN          , &
            RNMODE      , RUN         , SN          , TN          , YEAR        & 
            )
                    
!*******************************************************************************************************************************
    ELSEIF (DYNAMIC == SEASINIT) THEN    ! Initialization                      ! MF Equivalent to line 1824 in CSCAS.FOR
!*******************************************************************************************************************************
        
        !-----------------------------------------------------------------------------------------------------------------------
        !     Run seasonal initialization procedures in SUBROUTINE YCA_SeasInit: call six subroutines to initialize state and   
        !     rate variables, read the Xfile, set the planting and harvest dates, create the file names for the genotype files 
        !     and read the genetic coefficients, set up the growth stages including branching, check coefficients, set defaults 
        !     and calculate/set initial states, set up the output  descriptors, check controls and write information to the 
        !     overview and work files.
        !           
        !-----------------------------------------------------------------------------------------------------------------------
          
        CALL YCA_SeasInit ( &
            ALBEDOS     , BRSTAGE     , LAI         , CANHT       , CLOUDS      , CN          , DEWDUR      , DOY         , &
            HARVFRAC    , ISWDIS      , ISWNIT      , KCAN        , KEP         , LAIL        , &
            LAILA       , NFP         , ON          , PARIP       , PARIPA      , RESCALG     , RESLGALG    , RESNALG     , &
            RLV         , RN          , RNMODE      , RUN         , RUNI        , RWUMX       , RWUPM       , SENCALG     , &
            UH2O        , UNH4        , UNO3        , YEAR        , SENLALG     , SENNALG     , SLPF        , SN          , &
            STGYEARDOY  , TAIRHR      , TN          , TRWUP       &
            )

            
!*******************************************************************************************************************************
    ELSEIF (DYNAMIC == RATE) THEN                                              ! MF Equivalent to line 3943 in CSCAS.FOR
!*******************************************************************************************************************************

        !-----------------------------------------------------------------------------------------------------------------------
        !     In Subroutine YCA_Growth, set up the switches for establishment and determine whether today is a planting day. 
        !     If so, then run the daily rate procedures by calling eight subroutines to calculate germination timing,  
        !     daylength and development units, reserves and grazing (?), PAR interception, photosynthesis, rate factors,  
        !     senescence, assimilation and its partitioning, growth of storage roots, leaves, stems and Plant. sticks, reserves  
        !     and plant height, and soil water.         
        !-----------------------------------------------------------------------------------------------------------------------
           
            CALL YCA_Growth ( &  
                BD          , BRSTAGE     , CO2         , DAYL        , DLAYR       , DOY         , DUL         , &
                EO          , ISWDIS      , ISWNIT      , ISWWAT      , KCAN        , LL          , &
                NFP         , NH4LEFT     , NLAYR       , NO3LEFT     , PARIP       , PARIPA      , RLV         , &
                RNMODE      , SAT         , SENCALG     , SENLALG     , SENNALG     , SHF         , SLPF        , &
                SRAD        , ST          , STGYEARDOY  , SW          , TDEW        , TMAX        , &
                TMIN        , UNH4        , UNO3        , WEATHER     , SOILPROP    , CONTROL     , &  !   MF WEATHER needed for VPD
                YEAR        , YEARPLTCSM  , LAI         )         !LPM 06MAR2016 Added to keep automatic planting
        !!=======================================================================================================================
        !ENDIF  ! End of after planted (rate) section
        !!=======================================================================================================================

!*******************************************************************************************************************************
    ELSEIF (DYNAMIC == INTEGR) THEN                                            ! MF Equivalent to line 5535 in CSCAS.FOR
!*******************************************************************************************************************************

            CALL YCA_Integrate ( &
                ALBEDOS     , BD          , BRSTAGE     , LAI         , CANHT       , CO2         , DAYL        , DEPMAX      , &
                DLAYR       , DOY         , DRAIN       , EOP         , EP          , ET          , FERNIT      , IRRAMT      , &
                ISWNIT      , ISWWAT      , LL          , NFP         , NH4LEFT     , NLAYR       , NO3LEFT     , RAIN        , &
                RESCALG     , RESLGALG    , RESNALG     , RLV         , RUNOFF      , SRAD        , STGYEARDOY  , SW          , &
                TLCHD       , TMAX        , TMIN        , TNIMBSOM    , TNOXD       , TOMINFOM    , TOMINSOM    , TOMINSOM1   , &
                TOMINSOM2   , TOMINSOM3   , YEAR        & 
                )

!*******************************************************************************************************************************
    ELSEIF (DYNAMIC  ==  OUTPUT .AND. STEP  ==  STEPNUM .OR. DYNAMIC  ==  SEASEND .AND. SEASENDOUT  /=  'Y') THEN
                                                                               ! MF Equivalent to line 6665 in CSCAS.FOR
!*******************************************************************************************************************************
        
        !-----------------------------------------------------------------------------------------------------------------------
        !     Output model failure (if any), time sequences, N balance, harvest and evaluation,  OVERVIEW.OUT, summary output 
        !     for the simulation run, for leaf cohorts and branching levels, details for the WORK.OUT file, details for the 
        !     ERROR.OUT file, outputs for the DSSAT shell, screen outputs for sensitivity mode, store  variables and interface 
        !     with CSM and other program segments. 
        !-----------------------------------------------------------------------------------------------------------------------
  
        CALL YCA_Output ( & 
            BRSTAGE     , LAI         , CANHT       , CN          , DOY         , DYNAMIC     , &
            EOP         , IDETG       , IDETL       , IDETO       , IDETS       , ISWNIT      , ISWWAT      , &
            KCAN        , MESOM       , NFP         , ON          , REP         , RLV         , &
            RN          , RNMODE      , RUN         , RUNI        , SN          , SRAD        , STGYEARDOY  , &
            TN          , TNIMBSOM    , TOMINSOM1   , UNH4        , UNO3        , YEAR        &
            )

!*******************************************************************************************************************************
      ELSEIF (DYNAMIC == SEASEND) THEN                                         ! MF Equivalent to line 9630 in CSCAS.FOR
!*******************************************************************************************************************************

        EXCODEPREV = EXCODE

        CLOSE (NOUTPG)
        CLOSE (NOUTPG2)
        CLOSE (NOUTPGF)
        CLOSE (NOUTPF)
        CLOSE (NOUTPN)

!*******************************************************************************************************************************
      ENDIF ! End of INITIATION-RATES-INTEGRATE-OUTPUT-SEASEND construct       ! MF Equivalent to line 9648 in CSCAS.FOR
!*******************************************************************************************************************************
      END  ! CSYCA
