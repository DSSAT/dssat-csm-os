
      
C=======================================================================    
      SUBROUTINE XXCRP(FILEIOIN, RUN, TN, RN, RNMODE,                                        
     &  ISWWAT, ISWNIT, ISWDIS, MESOM,                                     !Controls
     &  IDETS, IDETO, IDETG, IDETL, FROP,                                                                            !Controls
     &  SN, ON, RUNI, REP, YEAR, DOY, STEP, CN,                                                 !Run+loop
     &  SRAD, TMAX, TMIN, TAIRHR, RAIN, CO2, TDEW,                                              !Weather
     &  DRAIN, RUNOFF, IRRAMT,                                                                  !Water
     &  DAYLT, WINDSP, DEWDUR, CLOUDS, ST, EO, ES,                                               !Weather        
     &  NLAYR, DLAYR, DEPMAX, LL, DUL, SAT, BD, SHF, SLPF,                                      !Soil states
     &  SNOW, SW, NO3LEFT, NH4LEFT, FERNIT,                                                           !H2O,N states
     &  TLCHD, TNIMBSOM, TNOXD,                                  !N components
     &  TOMINFOM, TOMINSOM, TOMINSOM1, TOMINSOM2, TOMINSOM3,                                                                  !N components
     &  YEARPLTCSM, HARVFRAC,                                                                   !Pl.date         ! YRPLT = YEARPLTCSM
     &  PARIP, PARIPA, EOP, EP, ET, TRWUP, ALBEDOS,                                             !Resources       ! REPLACED ALBEDO WITH ALBEDOS
     &  LAI, KCAN, KEP, 
     &  RLV, NFP, RWUPM, RWUMX, CANHT, LAIL, LAILA,                             !States          ! PORMIN = RWUMP
     &  UNO3, UNH4, UH2O,                                                                       !Uptake
     &  SENCALG, SENNALG, SENLALG,                                                              !Senescence
     &  RESCALG, RESNALG, RESLGALG,                                                             !Residues
     &  STGYEARDOY, GSTAGE,                                                                    !Stage dates   
     &  WEATHER, SOILPROP, CONTROL,                                               
     &  DYNAMIC) !, WEATHER)                                                                       !Control

      USE OSDefinitions

      USE ModuleDefs
      USE CRP_First_Trans_m
      
      IMPLICIT NONE
        
      TYPE (ControlType), intent (in) :: CONTROL    ! Defined in ModuleDefs
      TYPE (WeatherType), intent (in) :: WEATHER    ! Defined in ModuleDefs
      TYPE (SoilType), intent (in) ::   SOILPROP   ! Defined in ModuleDefs
    
      INTEGER CN, DOY, DYNAMIC, FROP, NLAYR, ON, REP, RN          
      INTEGER RUN, RUNI, SN, STEP, STGYEARDOY(20), TN, YEAR
      INTEGER YEARPLTCSM 
      INTEGER CSTIMDIF, CSYDOY, DAPCALC, TVICOLNM    ! Integer function calls
      INTEGER CSIDLAYR, CSYEARDOY              ! Integer function calls

      REAL ALBEDOS, BD(NL), GSTAGE, LAI, CANHT, CLOUDS, CO2, DAYL      ! REPLACED ALBEDO WITH ALBEDOS
      REAL DAYLT
      REAL DEPMAX, DEWDUR, DLAYR(NL), DRAIN, DUL(NL), EO, EOP, EP          
      REAL ES, ET, FERNIT, HARVFRAC(2), IRRAMT, KCAN, KEP, LAIL(30)
      REAL LAILA(30), LL(NL), NFP, NH4LEFT(NL), NO3LEFT(NL), PARIP
      REAL PARIPA, RAIN, SNOW
      REAL RESCALG(0:NL), RESLGALG(0:NL), RESNALG(0:NL), RLV(NL)
      REAL RWUMX, RWUPM, SAT(NL), SENCALG(0:NL), SENLALG(0:NL), TDEW
      REAL SENNALG(0:NL), SHF(NL), SLPF, SRAD, ST(0:NL), SW(NL), RUNOFF    
      REAL TLCHD, TAIRHR(24), TMAX, TMIN, TNIMBSOM, TNOXD, TOMINFOM                                                  
      REAL TOMINSOM, TOMINSOM1, TOMINSOM2, TOMINSOM3, TRWUP, UH2O(NL)
      REAL UNH4(NL), UNO3(NL), WINDSP    
!         REAL    PARHR(24)   , RADHR(24)   , RHUMHR(24)  , VPDHR(24)    !MF 14SE14 Hourly weather data
      REAL CSVPSAT, TFAC4, YVALXY, CSYVAL
  
      CHARACTER(LEN=1)   IDETG, IDETL, IDETO, IDETS, ISWDIS, ISWNIT      
      CHARACTER(LEN=1)   ISWWAT, MESOM, RNMODE      
      CHARACTER(LEN=250) FILEIOIN   
      CHARACTER(LEN=10)  TL10FROMI                               ! Character function call

      INTRINSIC AMAX1,AMIN1,EXP,FLOAT,INDEX,INT,LEN,MAX,MIN,MOD,NINT
      INTRINSIC SQRT,ABS,TRIM

!*******************************************************************************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN    ! Initialization                           ! MF Equivalent to line 1476 in CSCAS.FOR
!*******************************************************************************************************************************
        !-----------------------------------------------------------------------------------------------------------------------
        !     Run initialization procedure in SUBROUTINE CRP_RunInit..
        !     SUBROUTINE CRP_RunInit takes all the code from CSCAS.f90 (29OC14) lines 1490 - 1827.
        !-----------------------------------------------------------------------------------------------------------------------

        CALL CRP_RunInit (CN, DOY, FILEIOIN, FROP, IDETL, ISWNIT,
     &     ON, RN, RNMODE, RUN, SN, TN, YEAR)
                    
!*******************************************************************************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN    ! Initialization                      ! MF Equivalent to line 1824 in CSCAS.FOR
!*******************************************************************************************************************************
        
        !-----------------------------------------------------------------------------------------------------------------------
        !     Run seasonal initialization procedures in SUBROUTINE CRP_SeasInit: call six subroutines to initialize state and   
        !     rate variables, read the Xfile, set the planting and harvest dates, create the file names for the genotype files 
        !     and read the genetic coefficients, set up the growth stages including branching, check coefficients, set defaults 
        !     and calculate/set initial states, set up the output  descriptors, check controls and write information to the 
        !     overview and work files.
        !           
        !-----------------------------------------------------------------------------------------------------------------------
          
        CALL CRP_SeasInit (ALBEDOS, GSTAGE, LAI, CANHT, CLOUDS,
     &     CN, DEWDUR, DOY, HARVFRAC, IDETG, ISWDIS, ISWNIT,
     &     ISWWAT, KCAN, KEP, LAIL, LAILA, NFP, ON, PARIP,
     &     PARIPA, RESCALG, RESLGALG, RESNALG, RLV, RN, RNMODE,
     &     RUN, RUNI, RWUMX, RWUPM, SENCALG,
     &     UH2O, UNH4, UNO3, YEAR, SENLALG, SENNALG, SLPF, SN,
     &     STGYEARDOY, TAIRHR, TN, TRWUP)
            
!*******************************************************************************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN                                              ! MF Equivalent to line 3943 in CSCAS.FOR
!*******************************************************************************************************************************

        !-----------------------------------------------------------------------------------------------------------------------
        !     In Subroutine CRP_Growth, set up the switches for establishment and determine whether today is a planting day. 
        !     If so, then run the daily rate procedures by calling eight subroutines to calculate germination timing,  
        !     daylength and development units, reserves and grazing (?), PAR interception, photosynthesis, rate factors,  
        !     senescence, assimilation and its partitioning, growth of storage roots, leaves, stems and Plant. sticks, reserves  
        !     and plant height, and soil water.         
        !-----------------------------------------------------------------------------------------------------------------------
        CALL CRP_Growth (ALBEDOS, BD, GSTAGE, CLOUDS, CO2, DAYLT,
     &     DLAYR, DOY, DUL, EO, EOP, ES, ISWDIS, ISWNIT , ISWWAT,
     &     KCAN, KEP, LL, NFP, NH4LEFT, NLAYR , NO3LEFT, PARIP,
     &     PARIPA, RLV, RNMODE, SAT , SENCALG, SENLALG, SENNALG,
     &     SHF, SLPF, SNOW, SRAD, ST, STGYEARDOY, SW, TAIRHR, TDEW,
     &     TMAX, TMIN, TRWUP, UH2O, UNH4, UNO3, WEATHER,
     &     SOILPROP, CONTROL, WINDSP, YEAR, YEARPLTCSM, LAI,
     &     IDETG)
        !!=======================================================================================================================
        !ENDIF  ! End of after planted (rate) section
        !!=======================================================================================================================

!*******************************************************************************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN                                            ! MF Equivalent to line 5535 in CSCAS.FOR
!*******************************************************************************************************************************

        CALL CRP_Integrate (ALBEDOS, BD, GSTAGE, LAI, CANHT, CO2,
     &     DAYLT, DEPMAX, DLAYR, DOY, DRAIN, EOP, EP, ET, FERNIT,
     &     IRRAMT, ISWNIT, ISWWAT, LL, NFP, NH4LEFT, NLAYR,
     &     NO3LEFT, RAIN, RESCALG, RESLGALG, RESNALG, RLV, RUNOFF,
     &     SRAD, STGYEARDOY, SW, TLCHD, TMAX, TMIN, TNIMBSOM,
     &     TNOXD, TOMINFOM, TOMINSOM, TOMINSOM1, TOMINSOM2,
     &     TOMINSOM3, YEAR)

!*******************************************************************************************************************************
      ELSEIF (DYNAMIC.EQ.OUTPUT .AND. STEP.EQ.STEPNUM. OR.
     &        DYNAMIC.EQ.SEASEND .AND. SEASENDOUT.NE.'Y') THEN
!*******************************************************************************************************************************
        
        !-----------------------------------------------------------------------------------------------------------------------
        !     Output model failure (if any), time sequences, N balance, harvest and evaluation,  OVERVIEW.OUT, summary output 
        !     for the simulation run, for leaf cohorts and branching levels, details for the WORK.OUT file, details for the 
        !     ERROR.OUT file, outputs for the DSSAT shell, screen outputs for sensitivity mode, store  variables and interface 
        !     with CSM and other program segments. 
        !-----------------------------------------------------------------------------------------------------------------------
  
        CALL CRP_Output (GSTAGE, LAI, CANHT, CN, CO2, DAYLT, DOY, 
     &     DYNAMIC, EO, EOP, IDETG, IDETL, IDETO, IDETS, IRRAMT, ISWNIT,
     &     ISWWAT, KCAN, MESOM, NFP, NLAYR, ON, RAIN, REP, RLV,
     &     RN, RNMODE, RUN, RUNI, SN, SRAD, STGYEARDOY, TN,
     &     TNIMBSOM, TOMINSOM1, UNH4 , UNO3, WINDSP, YEAR)

!*******************************************************************************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN                                         ! MF Equivalent to line 9630 in CSCAS.FOR
!*******************************************************************************************************************************

        EXCODEPREV = EXCODE

        CLOSE (NOUTPG)
        CLOSE (NOUTPG2)
        CLOSE (NOUTPGF)
        CLOSE (NOUTPN)
        CLOSE (FNUMWRK)

!*******************************************************************************************************************************
      ENDIF ! End of INITIATION-RATES-INTEGRATE-OUTPUT-SEASEND construct       ! MF Equivalent to line 9648 in CSCAS.FOR
!*******************************************************************************************************************************
      END SUBROUTINE XXCRP
