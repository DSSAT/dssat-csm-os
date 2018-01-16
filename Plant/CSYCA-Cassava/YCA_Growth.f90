!***************************************************************************************************************************
! This is the code from the section (DYNAMIC == RATE) lines 4084 - 5516 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine YCA_Growth calls eight subroutines to calculate photosynthesis germination timing, daylength and development 
! units, reserves and grazing (?), PAR interception, rate factors, senescence, assimilation and its partitioning, growth of 
! storage roots, leaves, stems and Plant. sticks, reserves and plant height, and soil water.
!
!***************************************************************************************************************************

    SUBROUTINE YCA_Growth ( &  
        ALBEDOS     , BD          , BRSTAGE     ,  CLOUDS     , CO2         , DAYL        , DLAYR       , DOY         , &
        DUL         , EO          , EOP         , ES          , ISWDIS      , ISWNIT      , ISWWAT      , KCAN        , &
        KEP         , LL          , NFP         , NH4LEFT     , NLAYR       , NO3LEFT     , PARIP       , PARIPA      , &
        RLV         , RNMODE      , RWUMX       , RWUPM       , SAT         , SENCALG     , SENLALG     , SENNALG     , &
        SHF         , SLPF        , SRAD        , ST          , STGYEARDOY  , SW          , TAIRHR      , TDEW        , &
        TMAX        , TMIN        , TRWUP       , UH2O        , UNH4        , UNO3        , &
        !WEATHER     ,                                                                                                       ! MF WEATHER needed for VPD
        WINDSP      , YEAR        , YEARPLTCSM  , &         !LPM 06MAR2016 Added to keep automatic planting
        IDETG         )
    
        USE ModuleDefs
        USE YCA_First_Trans_m

        IMPLICIT NONE
        
        !TYPE (WeatherType) WEATHER                                                                                          ! MF Defined in ModuleDefs
    
        INTEGER DOY         , NLAYR       , STGYEARDOY(0:19)            , YEAR        , YEARPLTCSM      !LPM 25MAY2015 STGYEARDOY changed according to STGDOY(20) in plant.for            
        INTEGER CSIDLAYR                 

        REAL    ALBEDOS     , BD(NL)      , BRSTAGE     , CLOUDS      , CO2         , DAYL        , DLAYR(NL)   , DUL(NL)     
        REAL    EO          , EOP         , ES          , KCAN        , kep         , LL(NL)      , NFP         , NH4LEFT(NL) 
        REAL    NO3LEFT(NL) , PARIP       , PARIPA      , RLV(NL)     , RWUMX       , RWUPM       , SAT(NL)      
        REAL    SENCALG(0:NL)             , SENLALG(0:NL)             , SENNALG(0:NL)             , SHF(NL)     , SLPF        
        REAL    SRAD        , ST(NL)      , SW(NL)      , TAIRHR(24)  , TDEW        , TMAX        , TMIN        , TRWUP       
        REAL    UH2O(NL)    , UNH4(NL)    , UNO3(NL)    , WINDSP
        
        REAL    CSVPSAT     , CSYVAL      , TFAC4                                                                          ! Real function calls 
        REAL    YVALXY      , TFAC5                                                                                        ! Real function calls !LPM 15sep2017 Added TFAC5 

        CHARACTER(LEN=1) IDETG       , ISWDIS      , ISWNIT      , ISWWAT      , RNMODE  
    
        !-----------------------------------------------------------------------------------------------------------------------
        !     Set up the switches for establishment and determine whether today is a planting day.
        !-----------------------------------------------------------------------------------------------------------------------
        
        CALL  YCA_PrePlant( &  
            BD          , CO2         , DLAYR       , DOY         , DUL         , LL          , NH4LEFT     , NLAYR       , &
            NO3LEFT     , RNMODE      , ST          , STGYEARDOY  , SW          , TMAX        , TMIN        , YEAR        , &
            YEARPLTCSM  &                 ! WEATHER     ,      
            )
        
        !===================================================================================================================
        IF (YEARDOY > PLYEARDOY) THEN ! If planted (assumed in evening)
        !===================================================================================================================
        !----------------------------------------------------------------------------------------------------------------------
        !     Determine whether the water and thermal conditions allow the planting material to grow (called germination).
        !----------------------------------------------------------------------------------------------------------------------
            
            CALL YCA_Growth_Evapo ( & 
                ALBEDOS     , BRSTAGE     , CLOUDS      , CO2         , DLAYR       , DUL         , EO          , EOP         , &
                ES          , ISWWAT      , KEP         , LL          , NLAYR       , RLV         , RWUMX       , RWUPM       , &
                SAT         , SRAD        , SW          , TAIRHR      , TDEW        , TMAX        , TMIN        , TRWUP       , &
                UH2O        , & 
                !WEATHER     , 
                WINDSP      , YEAR        , ST          &         !LPM20MAR2016 To consider ST for germination
                )
            
            !=============================================================================================================
            IF (DAGERM+TTGEM*WFGE >= PGERM) THEN  ! If germinated by endday
            !=============================================================================================================
        
                !--------------------------------------------------------------------------------
                !      Calculate germination timing, daylength and development units, reserves
                !--------------------------------------------------------------------------------
                CALL YCA_Growth_Init ( &
                    BRSTAGE     , DAYL        &  
                    )
        
                !--------------------------------------------------------------------------------
                !       Calcualate PAR interception and rate factors
                !--------------------------------------------------------------------------------
                CALL YCA_Growth_Rates ( &
                    CO2         , EOP         , ISWDIS      , ISWNIT      , ISWWAT      , KCAN        , NFP         , &
                    PARIP       , PARIPA      , TDEW        , TMAX        , TMIN        , TRWUP       , RLV         , &
                    SRAD        , SLPF &
                    )
        
                !-----------------------------------------------------------------------
                !           Calculate senescence of leaves,stems,etc..
                !-----------------------------------------------------------------------
                CALL YCA_Growth_Senesce ( &
                    ISWNIT      , ISWWAT      , BRSTAGE      & 
                    )
                
                
                !-----------------------------------------------------------------------
                !           Calculate C assimilation at beginning of day
                !-----------------------------------------------------------------------
                
                CALL YCA_Growth_Photo ( &
                    CO2         , NFP         , SLPF        , SRAD        , TAIRHR      , TDEW        , TMAX        , &
                    TMIN        &
                    !, WEATHER     &        
                    )
                
                !-----------------------------------------------------------------------
                !           Partition C to above ground and roots (minimum) 
                !-----------------------------------------------------------------------
                CALL YCA_Growth_Part ( &
                    BRSTAGE     , ISWNIT      , NFP         &
                    )
                
                !-----------------------------------------------------------------------
                !           Calculate N uptake  -  and growth adjustments if inadequate N
                !-----------------------------------------------------------------------
                CALL YCA_Growth_NUptake ( &
                    BD          , DLAYR       , DUL         , ISWNIT      , LL          , NH4LEFT     , NLAYR       , &
                    NO3LEFT     , RLV         , SAT         , SW          , UNH4        , UNO3        , BRSTAGE     & 
                    )
                
                !-----------------------------------------------------------------------
                !           Distribute growth
                !-----------------------------------------------------------------------
                CALL YCA_Growth_Distribute ( &
                    DLAYR       , ISWNIT      , ISWWAT      , LL          , NH4LEFT     , NLAYR       , NO3LEFT     , &
                    RLV         , SENCALG     , SENLALG     , SENNALG     , SHF         , SW          , BRSTAGE       & 
                    )
                
            !=============================================================================================================
            ENDIF  ! End of after germinated section
            !=============================================================================================================
            
        !=================================================================================================================
        ENDIF  ! End of after planted (rate) section
        !=================================================================================================================
        


    END SUBROUTINE YCA_GROWTH
