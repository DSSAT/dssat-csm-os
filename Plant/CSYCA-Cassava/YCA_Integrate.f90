!***************************************************************************************************************************
! This is the code from the section (DYNAMIC == INTEGR) lines 5534 - 6649 of the original CSCAS code.  The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
    
! Subroutine YCA_Integrate calls nine subroutines that update the seasonal data with the data for the current day: dry 
! weights, produced and senesced leaf area, plant height, root and length, and nitrogen; update stages, dates and times; 
! calculate branch interval, N concentrations and whether to harvest; calculate yields of plant parts and summaries of 
! weather and soil variables and par utilization; and finally season-end soil conditions, and other general variables. 
!***************************************************************************************************************************
    
    SUBROUTINE YCA_Integrate ( &
        ALBEDOS     , BD          , BRSTAGE     , LAI         , CANHT       , CO2         , DAYL        , DEPMAX      , &
        DLAYR       , DOY         , DRAIN       , EOP         , EP          , ET          , FERNIT      , IRRAMT      , &
        ISWNIT      , ISWWAT      , LL          , NFP         , NH4LEFT     , NLAYR       , NO3LEFT     , RAIN        , &
        RESCALG     , RESLGALG    , RESNALG     , RLV         , RUNOFF      , SRAD        , STGYEARDOY  , SW          , &
        TLCHD       , TMAX        , TMIN        , TNIMBSOM    , TNOXD       , TOMINFOM    , TOMINSOM    , TOMINSOM1   , &
        TOMINSOM2   , TOMINSOM3   , YEAR        & 
        )
        
        USE ModuleDefs
        USE YCA_First_Trans_m
        
        IMPLICIT NONE
        EXTERNAL YCA_INTEG_AGESWTS, YCA_INTEG_LA, YCA_INTEG_N, YCA_INTEG_STAGES, YCA_INTEG_NCONC, YCA_INTEG_HSTFAIL, &
            YCA_INTEG_SEASEND, YCA_INTEG_WTHRSUM, YCA_INTEG_ENDCROP
        
        INTEGER DOY         , NLAYR       , STGYEARDOY(0:19)            , YEAR
        
        REAL    ALBEDOS     , BD(NL)      , BRSTAGE     , LAI         , CANHT       , CO2         , DAYL        , DEPMAX
        REAL    DLAYR(NL)   , DRAIN       , EOP         , EP          , ET          , FERNIT      , IRRAMT      , LL(NL)      
        REAL    NFP         , NH4LEFT(NL) , NO3LEFT(NL) , RAIN        , RESCALG(0:NL)             , RESLGALG(0:NL)            
        REAL    RESNALG(0:NL)             , RLV(NL)     , RUNOFF      , SRAD        , SW(NL)      , TLCHD       , TMAX      
        REAL    TMIN        , TNIMBSOM    , TNOXD       , TOMINFOM    , TOMINSOM    , TOMINSOM1   , TOMINSOM2   , TOMINSOM3           

        CHARACTER(LEN=1) ISWNIT     , ISWWAT     
 
        !-----------------------------------------------------------------------
        !         Update ages
        !----------------------------------------------------------------------
        CALL YCA_Integ_AgesWts ( &
            NLAYR       & 
            )
            
        !-----------------------------------------------------------------------
        !         Calculate reserve concentrations, shoot and total leaf area.
        !-----------------------------------------------------------------------
        CALL  YCA_Integ_LA ( &
            LAI        , CANHT       , DEPMAX      , DLAYR       , NLAYR       , RLV         & 
            ) 
            
        !-----------------------------------------------------------------------
        !         Update nitrogen amounts
        !-----------------------------------------------------------------------
        IF (ISWNIT  /=  'N') THEN
            CALL YCA_Integ_N (NLAYR  )
        ENDIF
        !-----------------------------------------------------------------------
        !         Update stages; returns if germinating.
        !-----------------------------------------------------------------------
        CALL YCA_Integ_Stages ( &
            BRSTAGE     , CO2         , DAYL        , DOY         , EP          , ET          , NFP         , TMAX        , &
            TMIN        , RAIN        , SRAD        , STGYEARDOY   & 
            )    
            
        !-----------------------------------------------------------------------
        !         Calculate nitrogen concentrations, skip if germinating.
        !-----------------------------------------------------------------------
        !IF (GESTAGE >= 0.5) CALL YCA_Integ_Nconc ( &  !LPM 21MAR2016 To separate germination and emergence
        IF (GESTAGE >= 1.0) CALL YCA_Integ_Nconc ( &
            ISWNIT                 & 
            )   
            
        ! 6666        CONTINUE  ! Jump to here if germinating ! MF From CS_Integ_Stages (48)
            
        !-----------------------------------------------------------------------
        !         Determine if to harvest or fail
        !-----------------------------------------------------------------------
         CALL YCA_Integ_HstFail ( &   
            BRSTAGE     , DOY         , STGYEARDOY  , SW          , YEAR        , LAI      & 
            ) 
             
        !-----------------------------------------------------------------------
        !         Calculate season end soil conditions                              
        !-----------------------------------------------------------------------
         CALL YCA_Integ_SeasEnd ( &
            BD          , DLAYR       , NH4LEFT     , NLAYR       , NO3LEFT     , RESCALG     , RESLGALG    , RESNALG     , &
            STGYEARDOY  , SW          & 
            )  
        
        !-----------------------------------------------------------------------
        !         Calculate weather and soil summary variables
        !-----------------------------------------------------------------------
        CALL YCA_Integ_WthrSum ( &
            CO2         , DOY         , DRAIN       , IRRAMT      , RAIN        , RUNOFF      , SRAD        , TLCHD       , &
            TMAX        , TMIN        , TNIMBSOM    , TNOXD       , TOMINFOM    , TOMINSOM    , TOMINSOM1   , TOMINSOM2   , &
            TOMINSOM3   , YEAR        , ISWWAT      & 
            )
        
        !-----------------------------------------------------------------------
        !         Calculate ASW and yields of plant parts at the end of the crop
        !-----------------------------------------------------------------------
        CALL YCA_Integ_EndCrop ( &
            ALBEDOS     , DLAYR       , EOP         , FERNIT      , ISWWAT      , LL          , NLAYR       , STGYEARDOY  , &
            SW          , LAI         & 
            )
            
    END SUBROUTINE YCA_Integrate