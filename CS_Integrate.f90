!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.INTEGR) lines 5534 - 6649 of the original CSCAS code.  The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module CS_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
    
! Subroutine CS_Integrate calls nine subroutines that update the seasonal data with the data for the current day: dry 
! weights, produced and senesced leaf area, plant height, root and length, and nitrogen; update stages, dates and times; 
! calculate branch interval, N concentrations and whether to harvest; calculate yields of plant parts and summaries of 
! weather and soil variables and par utilization; and finally season-end soil conditions, and other general variables. 
!***************************************************************************************************************************
    
    SUBROUTINE CS_Integrate ( &
        ALBEDOS     , BD          , BRSTAGE     , CAID        , CANHT       , CO2         , DAYL        , DEPMAX      , &
        DLAYR       , DOY         , DRAIN       , EOP         , EP          , ET          , FERNIT      , IRRAMT      , &
        ISWNIT      , ISWWAT      , LL          , NFP         , NH4LEFT     , NLAYR       , NO3LEFT     , RAIN        , &
        RESCALG     , RESLGALG    , RESNALG     , RLV         , RUNOFF      , SRAD        , STGYEARDOY  , SW          , &
        TLCHD       , TMAX        , TMIN        , TNIMBSOM    , TNOXD       , TOMINFOM    , TOMINSOM    , TOMINSOM1   , &
        TOMINSOM2   , TOMINSOM3   , YEAR        & 
        )
        
        USE ModuleDefs
        USE CS_First_Trans_m
        
        IMPLICIT NONE
        
        INTEGER DOY         , NLAYR       , STGYEARDOY(0:19)            , YEAR
        
        REAL    ALBEDOS     , BD(NL)      , BRSTAGE     , CAID        , CANHT       , CO2         , DAYL        , DEPMAX
        REAL    DLAYR(NL)   , DRAIN       , EOP         , EP          , ET          , FERNIT      , IRRAMT      , LL(NL)      
        REAL    NFP         , NH4LEFT(NL) , NO3LEFT(NL) , RAIN        , RESCALG(0:NL)             , RESLGALG(0:NL)            
        REAL    RESNALG(0:NL)             , RLV(NL)     , RUNOFF      , SRAD        , SW(NL)      , TLCHD       , TMAX      
        REAL    TMIN        , TNIMBSOM    , TNOXD       , TOMINFOM    , TOMINSOM    , TOMINSOM1   , TOMINSOM2   , TOMINSOM3           

        CHARACTER(LEN=1) ISWNIT     , ISWWAT     
 
        !-----------------------------------------------------------------------
        !         Update ages
        !----------------------------------------------------------------------
        CALL CS_Integ_AgesWts ( &
            NLAYR,      BRSTAGE       & 
            )
            
        !-----------------------------------------------------------------------
        !         Calculate reserve concentrations, shoot and total leaf area.
        !-----------------------------------------------------------------------
        CALL  CS_Integ_LA ( &
            CAID        , CANHT       , DEPMAX      , DLAYR       , NLAYR       , RLV         , BRSTAGE   & 
            ) 
            
        !-----------------------------------------------------------------------
        !         Update nitrogen amounts
        !-----------------------------------------------------------------------
        CALL CS_Integ_N ( &
            NLAYR  , BRSTAGE     & 
            )
        
        !-----------------------------------------------------------------------
        !         Update stages; returns if germinating.
        !-----------------------------------------------------------------------
        CALL CS_Integ_Stages ( &
            BRSTAGE     , CO2         , DAYL        , DOY         , EP          , ET          , NFP         , TMAX        , &
            TMIN        , RAIN        , SRAD        , STGYEARDOY   & 
            )    
            
        !-----------------------------------------------------------------------
        !         Calculate nitrogen concentrations, skip if germinating.
        !-----------------------------------------------------------------------
        IF (GESTAGE.GE.0.5) CALL CS_Integ_Nconc ( &
            ISWNIT      , BRSTAGE           & 
            )   
            
        ! 6666        CONTINUE  ! Jump to here if germinating ! MF From CS_Integ_Stages (48)
            
        !-----------------------------------------------------------------------
        !         Determine if to harvest or fail
        !-----------------------------------------------------------------------
         CALL CS_Integ_HstFail ( &   
            BRSTAGE     , DOY         , STGYEARDOY  , SW          , YEAR        & 
            ) 
             
        !-----------------------------------------------------------------------
        !         Calculate season end soil conditions                              
        !-----------------------------------------------------------------------
         CALL CS_Integ_SeasEnd ( &
            BD          , DLAYR       , NH4LEFT     , NLAYR       , NO3LEFT     , RESCALG     , RESLGALG    , RESNALG     , &
            STGYEARDOY  , SW          & 
            )  
        
        !-----------------------------------------------------------------------
        !         Calculate weather and soil summary variables
        !-----------------------------------------------------------------------
        CALL CS_Integ_WthrSum ( &
            CO2         , DOY         , DRAIN       , IRRAMT      , RAIN        , RUNOFF      , SRAD        , TLCHD       , &
            TMAX        , TMIN        , TNIMBSOM    , TNOXD       , TOMINFOM    , TOMINSOM    , TOMINSOM1   , TOMINSOM2   , &
            TOMINSOM3   , YEAR        & 
            )
        
        !-----------------------------------------------------------------------
        !         Calculate ASW and yields of plant parts at the end of the crop
        !-----------------------------------------------------------------------
        CALL CS_Integ_EndCrop ( &
            ALBEDOS     , DLAYR       , EOP         , FERNIT      , ISWWAT      , LL          , NLAYR       , STGYEARDOY  , &
            SW          & 
            )
            
    END SUBROUTINE CS_Integrate