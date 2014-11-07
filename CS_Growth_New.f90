!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.RATE) lines 4218 - 5516 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement. The variables are declared in 
! Module_CSCAS_Vars_List.
!
! This subroutine calls subroutines to calculate germination timing, daylength and development units, reserves and 
! grazing (?), PAR interception, rate factors, senescence, assimilation and its partitioning, growth of storage roots, 
! leaves, stems and crowns, N uptake and distribution, reserves and plant height, and soil water.
!
!***************************************************************************************************************************

    SUBROUTINE CS_Growth ( &
        BD          , BRSTAGE     , CO2         , DAYL        , DLAYR       , DUL         , EOP         , ISWDIS      , &
        ISWNIT      , ISWWAT      , KCAN        , LL          , NFP         , NH4LEFT     , NLAYR       , NO3LEFT     , &
        PARIP       , PARIPA      , RLV         , SAT         , SENCALG     , SENLALG     , SENNALG     , SHF         , &
        SLPF        , SRAD        , SW          , TRWUP       , UNH4        , UNO3        , WEATHER       & 
        )
    
        USE ModuleDefs
        USE Module_CSCAS_Vars_List
    
        IMPLICIT NONE
        
        TYPE (WeatherType) WEATHER
    
        CHARACTER(LEN=1) ISWDIS      , ISWNIT      , ISWWAT      
        INTEGER NLAYR       
        REAL    BD(NL)      , BRSTAGE     , CO2         , DAYL        , DLAYR(NL)   , DUL(NL)     , EOP         , KCAN        
        REAL    LL(NL)      , NFP         , NH4LEFT(NL) , NO3LEFT(NL) , PARIP       , PARIPA      , RLV(NL)     , SAT(NL)     
        REAL    SENCALG(0:NL)             , SENLALG(0:NL)             , SENNALG(0:NL)             , SHF(NL)     , SLPF        
        REAL    SRAD        , SW(NL)      , TRWUP       , UNH4(NL)    , UNO3(NL)
    
        !-----------------------------------------------------------------------
        !           Initialization, calculate germination, daylength factors,
        !           development units, reserves.
        !-----------------------------------------------------------------------
        CALL CS_Growth_Init ( &
            BRSTAGE     , DAYL        &
            )
            
        !-----------------------------------------------------------------------
        !           Calculate PAR and process rate factors.
        !-----------------------------------------------------------------------
        CALL CS_Growth_Rates ( &
            CO2         , EOP         , ISWDIS      , ISWNIT      , ISWWAT      , KCAN        , NFP         , PARIP       , &
            PARIPA      , TRWUP       & 
            )
            
        !-----------------------------------------------------------------------
        !           Calculate senescence of leaves, stems, etc. and remobilization.
        !-----------------------------------------------------------------------
        CALL CS_Growth_Senesce ( &
            ISWNIT      , ISWWAT      & 
            )

        !-----------------------------------------------------------------------
        !           Calculate C assimilation at beginning of day.
        !-----------------------------------------------------------------------

        CALL CS_Photo ( &
            CO2         , NFP         , SLPF        , SRAD        , WEATHER     &
            )
        
        !-----------------------------------------------------------------------
        !           Calculate partitioning of assimilate.
        !-----------------------------------------------------------------------
        CALL CS_Growth_Part ( &
            BRSTAGE     , ISWNIT      , NFP         &
            )
            
        !-----------------------------------------------------------------------
        !           N uptake and partitioning, adjust growth  if N inadequate N.
        !-----------------------------------------------------------------------
        CALL CS_Growth_N_Uptake ( &
            BD          , DLAYR       , DUL         , ISWNIT      , LL          , NH4LEFT     , NLAYR       , NO3LEFT     , &
            RLV         , SAT         , SW          , UNH4        , UNO3          & 
            )
            
        !-----------------------------------------------------------------------
        !           Growth of reserves, canopy and water balance.
        !-----------------------------------------------------------------------
        CALL Growth_Distribute ( &
            DLAYR       , ISWNIT      , ISWWAT      , LL          , NH4LEFT     , NLAYR       , NO3LEFT     , RLV         , &
            SENCALG     , SENLALG     , SENNALG     , SHF         , SW          & 
            )

    END SUBROUTINE CS_GROWTH
