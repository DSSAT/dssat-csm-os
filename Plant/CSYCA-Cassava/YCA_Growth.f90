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
        BD          , BRSTAGE     , CO2         , DAYL        , DLAYR       , DOY         , DUL         , &
        EO          , ISWDIS      , ISWNIT      , ISWWAT      , KCAN        , LL          , &
        NFP         , NH4LEFT     , NLAYR       , NO3LEFT     , PARIP       , PARIPA      , RLV         , &
        RNMODE      , SAT         , SENCALG     , SENLALG     , SENNALG     , SHF         , SLPF        , &
        SRAD        , ST          , STGYEARDOY  , SW          , TDEW        , TMAX        , &
        TMIN        , UNH4        , UNO3        , WEATHER     , SOILPROP    , CONTROL     , &  !   MF WEATHER needed for VPD
        YEAR        , YEARPLTCSM  , LAI         )         !LPM 06MAR2016 Added to keep automatic planting

! 2023-01-25 chp removed unused variables
!       ALBEDOS     , CLOUDS     , EOP          , ES          , KEP         , TAIRHR      , TRWUP       , 
!       UH2O        , WINDSP     , IDETG

        USE ModuleDefs
        USE YCA_First_Trans_m

        IMPLICIT NONE
        EXTERNAL YCA_PREPLANT, YCA_GROWTH_EVAPO, YCA_GROWTH_INIT, YCA_GROWTH_RATES, YCA_GROWTH_SENESCE, & 
          YCA_GROWTH_PHOTO, YCA_GROWTH_PART, YCA_GROWTH_NUPTAKE, YCA_GROWTH_DISTRIBUTE

        TYPE (ControlType), intent (in) :: CONTROL    ! Defined in ModuleDefs
        TYPE (WeatherType), intent (in) :: WEATHER    ! Defined in ModuleDefs
        TYPE (SoilType), intent (in) ::   SOILPROP   ! Defined in ModuleDefsR                                                                                          ! MF Defined in ModuleDefs
    
        INTEGER DOY         , NLAYR       , STGYEARDOY(0:19)            , YEAR        , YEARPLTCSM      !LPM 25MAY2015 STGYEARDOY changed according to STGDOY(20) in plant.for            
!       INTEGER CSIDLAYR                 

        REAL    BD(NL)      , BRSTAGE     , CO2         , DAYL        , DLAYR(NL)   , DUL(NL)     ! ALBEDOS     , CLOUDS      , 
        REAL    EO          , KCAN        , LL(NL)      , NFP         , NH4LEFT(NL) ! EOP         , ES          , kep         , 
        REAL    NO3LEFT(NL) , PARIP       , PARIPA      , RLV(NL)     , SAT(NL)      
        REAL    SENCALG(0:NL)             , SENLALG(0:NL)             , SENNALG(0:NL)             , SHF(NL)     , SLPF        
        REAL    SRAD        , ST(NL)      , SW(NL)      , TDEW        , TMAX        , TMIN       !, TAIRHR(24)  , TRWUP       
        REAL    UNH4(NL)    , UNO3(NL)    , LAI         ! UH2O(NL)    , WINDSP      , 
        
!       REAL    CSVPSAT     , CSYVAL      , TFAC4                                                                          ! Real function calls 
!       REAL    YVALXY      , TFAC5                                                                                        ! Real function calls !LPM 15sep2017 Added TFAC5 

        CHARACTER(LEN=1) ISWDIS      , ISWNIT      , ISWWAT      , RNMODE  !IDETG       , 
    
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
              BRSTAGE     , CO2         , DLAYR       , DUL         , EO          ,     &
              ISWWAT      , LL          , NLAYR       , SW          , YEAR        , ST)
            
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
                    CO2         , ISWDIS      , ISWNIT      , ISWWAT      , KCAN        , NFP         , PARIP       , &
                    PARIPA      , TDEW        , TMAX        , TMIN        , RLV         , SRAD        , SLPF        , &     !LPM 26MAR2016 RLV added
                    LAI         , CONTROL     , WEATHER     , SOILPROP &  
                    )
        
                !-----------------------------------------------------------------------
                !           Calculate senescence of leaves,stems,etc..
                !-----------------------------------------------------------------------
                CALL YCA_Growth_Senesce ( &
                    ISWNIT      , LAI & 
                    )
                
                
                !-----------------------------------------------------------------------
                !           Calculate C assimilation at beginning of day
                !-----------------------------------------------------------------------
                
                CALL YCA_Growth_Photo ( &
                    SLPF        , SRAD        ,      &
                    CONTROL     , WEATHER     , SOILPROP )
                
                !-----------------------------------------------------------------------
                !           Partition C to above ground and roots (minimum) 
                !-----------------------------------------------------------------------
                CALL YCA_Growth_Part ( &
                    BRSTAGE     , ISWNIT      , NFP         , LAI         )
                
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
