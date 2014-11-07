!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.INTEGR) lines 5534 - 6649 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement. The variables are described in CSCAS.
!
! This subroutine calls subroutines to update the seasonal data with the data for the current day: dry weights, produced and
! senesced leaf area, plant height, root and length, and nitrogen. It updates stages, dates and times. It calculates branch
! interval, N concentrations and whether to harvest. It calculates summaries of weather, soil variables, par utilization,
! and yields of plant parts. 
!
!***************************************************************************************************************************
    
    
    SUBROUTINE CS_Integrate ( &
        ALBEDO      , BD          , BRSTAGE     , CAID        , CO2         , DAYL        , &
        DLAYR       , DOY         , DRAIN       , EOP         , EP          , ET          , FERNIT      , IRRAMT      , &
        ISWNIT      , ISWWAT      , LL          , NFP         , NH4LEFT     , NLAYR       , NO3LEFT     , RAIN        , &
        RESCALG     , RESLGALG    , RESNALG     , RLV         , RUNOFF      , SRAD        , STGYEARDOY  , SW          , &
        TLCHD       , TNIMBSOM    , TNOXD       , TOMINFOM    , TOMINSOM    , TOMINSOM1   , &                             
        TOMINSOM2   , TOMINSOM3   , YEAR        & 
        )
        
        USE ModuleDefs
        USE Module_CSCAS_Vars_List
        
        CHARACTER(LEN=1) ISWNIT     , ISWWAT     
        INTEGER DOY         , NLAYR       , STGYEARDOY(20)            , YEAR
        REAL    ALBEDO      , BD(NL)      , BRSTAGE     , CAID        , CO2         , DAYL        , DLAYR(NL)   
        REAL    DRAIN       , EOP         , EP          , ET          , FERNIT      , IRRAMT      , LL(NL)      
        REAL    NFP         , NH4LEFT(NL) , NO3LEFT(NL) , RAIN        , RESCALG(0:NL)             , RESLGALG(0:NL)            
        REAL    RESNALG(0:NL)             , RLV(NL)     , RUNOFF      , SRAD        , SW(NL)      , TLCHD                          
        REAL    TNIMBSOM    , TNOXD       , TOMINFOM    , TOMINSOM    , TOMINSOM1   , TOMINSOM2   , TOMINSOM3           
        
        !-----------------------------------------------------------------------
        !         Update ages and dry weights
        !-----------------------------------------------------------------------
        CALL CS_Integ_Ages_Wts( &
            NLAYR       & 
            )
            
        !-----------------------------------------------------------------------
        !         Calculate reserve concentration, shoot and total leaf area
        !         Update senesced, harvested, green leaf area, stems and petioles,
        !         plant height, and root depth and length
        !-----------------------------------------------------------------------
        CALL CS_Integ_LA ( &
            CAID        , DLAYR       , RLV            & 
            )
            
        !-----------------------------------------------------------------------
        !         Update nitrogen amounts
        !-----------------------------------------------------------------------
        CALL CS_Integ_N ( &
            NLAYR       & 
            )
            
        !-----------------------------------------------------------------------
        !         Update stages, states and dates, calculate branch interval and
        !         crop cycle conditions
        !-----------------------------------------------------------------------
        CALL CS_Integ_Stages ( &
            BRSTAGE     , CAID        , CO2         , DAYL        , DLAYR       , DOY         , EP          , ET          , &
            NFP         , NLAYR       , RAIN        , RLV         , SRAD        , STGYEARDOY   & 
            )
            
        !-----------------------------------------------------------------------
        !         Calculate nitrogen concentrations
        !-----------------------------------------------------------------------
        IF (GESTAGE .GE. 0.5) THEN                                               ! If not germinating
            CALL CS_Integ_Nconc ( & 
                ISWNIT       & 
                )
        ENDIF
!6666        CONTINUE  ! Jump to here if germinating                             ! Old GOTO construction in CCSCAS
        
        !-----------------------------------------------------------------------
        !         Determine if to harvest or fail
        !-----------------------------------------------------------------------
        CALL CS_Integ_Hst_Fail ( &
            BRSTAGE     , DOY         , STGYEARDOY  , SW          , YEAR        & 
            )
            
        !-----------------------------------------------------------------------
        !         Calculate season end soil conditions, other general variables
        !-----------------------------------------------------------------------
        CALL CS_Integ_SeasEnd ( &
            BD          , DLAYR       , NH4LEFT     , NLAYR       , NO3LEFT     , RESCALG     , RESLGALG    , RESNALG     , &
            STGYEARDOY  , SW          &
            )
            
        !-----------------------------------------------------------------------
        !         Calculate summary of weather, soil variables, PAR use efficiency
        !-----------------------------------------------------------------------
        CALL CS_Integ_WthrSum ( &
            CO2         , DOY         , DRAIN       , IRRAMT      , RAIN        , RUNOFF      , SRAD        , TLCHD       , &
            TNIMBSOM    , TNOXD       , TOMINFOM    , TOMINSOM    , TOMINSOM1   , TOMINSOM2   , TOMINSOM3   , YEAR        & 
            )

        !-----------------------------------------------------------------------
        !         N fert, available water, upgrade albedo, weights at end of crop
        !-----------------------------------------------------------------------
        CALL CS_Integ_EndCrop ( &
            ALBEDO      , DLAYR       , EOP         , FERNIT      , ISWWAT      , LL          , NLAYR       , STGYEARDOY  , &
            SW          & 
            )
                
    END SUBROUTINE CS_Integrate
    
    