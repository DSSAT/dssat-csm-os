!***************************************************************************************************************************
! This is the code from the section (DYNAMIC == SEASINIT) lines 1826 - 3941 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine YCA_SeasInit calls six subroutines to initialize state and rate variables, read the Xfile, set the planting 
! and harvest dates, create the file names for the genotype files, set up the growth stages including branching, check 
! coefficients, set defaults and calculate/set initial states, set up the output descriptors, check controls and write 
! information to the overview and work files.
!***************************************************************************************************************************
    SUBROUTINE YCA_SeasInit ( &  
        ALBEDOS     , BRSTAGE     , LAI         , CANHT       , CLOUDS      , CN          , DEWDUR      , DOY         , &
        HARVFRAC    , ISWDIS      , ISWNIT      , KCAN        , KEP         , LAIL        , &
        LAILA       , NFP         , ON          , PARIP       , PARIPA      , RESCALG     , RESLGALG    , RESNALG     , &
        RLV         , RN          , RNMODE      , RUN         , RUNI        , RWUMX       , RWUPM       , SENCALG     , &
        UH2O        , UNH4        , UNO3        , YEAR        , SENLALG     , SENNALG     , SLPF        , SN          , &
        STGYEARDOY  , TAIRHR      , TN          , TRWUP       &
        )

! 2023-01-25 chp removed unused variables
!       IDETG       , ISWWAT      , 

        USE ModuleDefs
        USE YCA_First_Trans_m

        IMPLICIT     NONE
        EXTERNAL YCA_SeasInit_VarInit, YCA_SeasInit_ReadXfile, YCA_SeasInit_PlHarvDat, YCA_SeasInit_ReadGeno, &
          YCA_SeasInit_SetStage, YCA_SeasInit_Final
        
        INTEGER STGYEARDOY(0:19)            , CN          , DOY         , ON          , RN          , RUN         , RUNI        
        INTEGER SN          , TN          , YEAR
        
        REAL    ALBEDOS     , BRSTAGE     , LAI         , CANHT       , CLOUDS      , DEWDUR      , HARVFRAC(2) , KCAN        
        REAL    KEP         , LAIL(30)    , LAILA(30)   , NFP         , PARIP       , PARIPA      , RESCALG(0:NL)             
        REAL    RESLGALG(0:NL)            , RESNALG(0:NL)             , RLV(NL)     , RWUMX       , RWUPM       
        REAL    SENCALG(0:NL)             , SENLALG(0:NL)             , SENNALG(0:NL)             , SLPF        , TAIRHR(24)   
        REAL    TRWUP       , UH2O(NL)    , UNH4(NL)    , UNO3(NL)
        
        CHARACTER(LEN=1)  :: ISWDIS      , ISWNIT      , RNMODE       ! IDETG       , ISWWAT      , 
        
        !-----------------------------------------------------------------------------------------------------------------------
        !       Initialize both state and rate variables                       ! MF Equivalent to line 1828 in CSCAS.FOR
        !-----------------------------------------------------------------------------------------------------------------------
        
        CALL YCA_SeasInit_VarInit ( &
            BRSTAGE     , LAI         , CANHT       , DEWDUR      , LAIL        , LAILA       , NFP         , PARIP       , &
            PARIPA      , RESCALG     , RESLGALG    , RESNALG     , RLV         , SENCALG     , SENLALG     , SENNALG     , &
            STGYEARDOY  , TRWUP       , UH2O        , UNH4        , UNO3         &
            ) 
            
        !-----------------------------------------------------------------------------------------------------------------------
        !       Read experiment information from Dssat input or X- file        ! MF Equivalent to line 2338 in CSCAS.FOR
        !-----------------------------------------------------------------------------------------------------------------------
        
        CALL YCA_SeasInit_ReadXfile ( &
            CN          , HARVFRAC    , ISWDIS      , ON          , RN          , SN          , TN          &
            ) 
            
        !-----------------------------------------------------------------------------------------------------------------------
        !       Set planting/harvesting dates (Will change if runs repeated)   ! MF Equivalent to line 2536 in CSCAS.FOR
        !-----------------------------------------------------------------------------------------------------------------------
            
        CALL YCA_SeasInit_PlHarvDat ( &
            DOY         , YEAR         &
            )
       
        !-----------------------------------------------------------------------------------------------------------------------
        !       Create the file names for the species, ecotype and cultivar files and read the data from them.
                                                                               ! MF Equivalent to line 1824 in CSCAS.FOR 
        !-----------------------------------------------------------------------------------------------------------------------
        
        CALL YCA_SeasInit_ReadGeno ( &
            CN          , KCAN        , ON          , RN          , RNMODE      , RWUMX       , RWUPM       , SN          , &
            TN           &
            )
    
        !-----------------------------------------------------------------------------------------------------------------------
        !       Set up the growth stages including branching, check coefficients, set defaults and calculate/set initial states.
                                                                               ! MF Equivalent to line 3109 in CSCAS.FOR
        !-----------------------------------------------------------------------------------------------------------------------
        
        CALL YCA_SeasInit_SetStage( &
            ISWNIT      , KCAN        , KEP         , SLPF          &
            )
            
        !-----------------------------------------------------------------------------------------------------------------------
        !       Set up the output descriptors, check controls and write information to the overview and work files.
                                                                               ! MF Equivalent to line 3551 in CSCAS.FOR 
        !-----------------------------------------------------------------------------------------------------------------------
        CALL YCA_SeasInit_Final ( &
            ALBEDOS     , CLOUDS      , CN          , KCAN        , KEP         , ON          , &
            RN          , RUN         , RUNI        , RWUMX       , SN          , TAIRHR      , &
            TN           &
            )
            
    END SUBROUTINE YCA_SeasInit
