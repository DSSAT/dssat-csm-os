!***************************************************************************************************************************
! Subroutine YCA_Output takes all the code from CSCAS lines 6656 - 9620. The names of the dummy arguments are the same 
! as in the original CSCAS code and the call statement and are declared here. The variables  that are not arguments 
! are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of the original CSCAS.FOR code.
!    
! Subroutine YCA_Output calls 12 subroutines that cover model failure, time sequence outputs, output of N balance, outputs 
! for harvest and evaluation,  output for OVERVIEW.OUT, summary output for the simulation run, for leaf cohorts and 
! branching levels, details for the WORK.OUT file, details for the ERROR.OUT file, outputs for the DSSAT shell, screen 
! outputs for sensitivity mode, store  variables and interface with CSM summary output. 
!***************************************************************************************************************************
    
    SUBROUTINE YCA_Output ( & 
        BRSTAGE     , LAI         , CANHT       , CN          , DOY         , DYNAMIC     , &
        EOP         , IDETG       , IDETL       , IDETO       , IDETS       , ISWNIT      , ISWWAT      , &
        KCAN        , MESOM       , NFP         , ON          , REP         , RLV         , &
        RN          , RNMODE      , RUN         , RUNI        , SN          , SRAD        , STGYEARDOY  , &
        TN          , TNIMBSOM    , TOMINSOM1   , UNH4        , UNO3        , YEAR        &
        )

! 2023-01-25 chp removed unused variables
!       CO2         , EO          , IRRAMT      , NLAYR       , RAIN        , WINDSP      , 

        USE ModuleDefs
        USE YCA_First_Trans_m
        USE YCA_Formats_m
     
        IMPLICIT NONE 
        EXTERNAL YCA_OUT_MODFAIL, YCA_OUT_PLGROW, YCA_OUT_FRESHWT, YCA_OUT_EVAL, YCA_OUT_PLANTSUM, YCA_OUT_LFTIER, &
            YCA_OUT_WRKPHENRES, YCA_OUT_ERROR, YCA_OUT_CRPSIM, YCA_OUT_SENS, YCA_OUT_STOREVARS, YCA_OUT_REINIT
     
        INTEGER :: CN          , DOY         , DYNAMIC     , ON          , REP         , RN          ! NLAYR       , 
        INTEGER :: RUN         , RUNI        , SN          , STGYEARDOY(0:19)          , TN          , YEAR

        REAL    :: BRSTAGE     , LAI         , CANHT       , EOP         ! , CO2         , DAYL        , EO          , IRRAMT    
        REAL    :: KCAN        , NFP         , RLV(NL)     , SRAD        , TNIMBSOM    , TOMINSOM1   , UNH4(NL)    ! , RAIN        
        REAL    :: UNO3(NL)   !, WINDSP      

        CHARACTER(LEN=1)  :: IDETG       , IDETL       , IDETO       , IDETS       , ISWNIT      , ISWWAT      
        CHARACTER(LEN=1)  :: MESOM       , RNMODE              
        
        !INTEGER,PARAMETER::SEASEND= 6 ! Program ending indicator                             ! MF In ModuleDefs 
        
        ! Simulated outputs only                                                              ! LAH comments in original code
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
        !   Y->Leaves+Tiers+Measured                 
        !   D->+Phenols+Phenolm+Plantres+Plantrem
        !   A->Errora+Errors+Errort+Full Reads
        !   0,A are meta switches:
        !     0 switches everything to N apart from IDETS,which given a Y,
        !       and IDETO,which given an E when RNMODE is not N (seasonal)
        !     A switches ALL outputs on 
        
        !-------------------------------------------------------------------------------------------------------------------
        !               ! If model failure so that cycle not completed
        !-------------------------------------------------------------------------------------------------------------------
        CALL YCA_Out_ModFail ( &
            BRSTAGE     , LAI        , DYNAMIC     , KCAN        &
            )

        !-------------------------------------------------------------------------------------------------------------------
        !       TIME SEQUENCE OUTPUTS (Work, Plantgro, gr2, grf)
        !-------------------------------------------------------------------------------------------------------------------

        IF (  (MOD(DAS,FROPADJ) == 0.AND.YEARDOY >= PLYEARDOY).OR. (YEARDOY == PLYEARDOY).OR. (YEARDOY == STGYEARDOY(0)).OR. &
            (YEARDOY == STGYEARDOY(HSTG)).OR. (YEARDOY == STGYEARDOY(PSX+1))) THEN
            
            !---------------------------------------------------------------------------------------------------------------
            !         Output plant growth factors (Plantgro, gr2, grf, N) (IDETG NE N)
            !---------------------------------------------------------------------------------------------------------------
             CALL YCA_Out_PlGrow ( & 
                BRSTAGE     , CANHT       , DOY         ,  EOP         , IDETG       , IDETL       , ISWNIT      , &
                NFP         , RLV         , RUN         , TN          , YEAR        &
                ) 
             CALL YCA_Out_FreshWt ( & 
                DOY         ,  IDETG       , IDETL       , RUN         , YEAR        &
                )
            
!       ELSEIF(YEARDOY < PLYEARDOY.AND.(MOD(DAS,FROPADJ)) == 0.AND.IPLTI == 'A') THEN
        ELSEIF(YEARDOY < PLYEARDOY.AND.(MOD(DAS,FROPADJ)) == 0.AND.(IPLTI == 'A' .OR. IPLTI == 'F')) THEN
                
                
        ENDIF  ! End time-course outputs (appropriate day, etc.)
        ! (MOD(DAS,FROPADJ) == 0.AND.YEARDOY >= PLYEARDOY),etc..
            
        !***************************************************************************************************************
        IF (STGYEARDOY(PSX+1) == YEARDOY .OR.DYNAMIC == SEASEND) THEN         ! If harvest/failure day
        !***************************************************************************************************************

            !-----------------------------------------------------------------------------------------------------------
            !          Output Evaluate & Overview and necessary input data (IDETO)
            !-----------------------------------------------------------------------------------------------------------
            CALL YCA_Out_Eval ( &  
                CN          , DOY         , DYNAMIC     , IDETG       , IDETL       , IDETO       , ISWNIT      , ISWWAT      , &
                MESOM       , ON          , RN          , RUN         , RUNI        , SN          , STGYEARDOY  , TN          , &
                TNIMBSOM    , TOMINSOM1   , YEAR        , LAI         &
                )
                
            !-----------------------------------------------------------------------------------------------------------
            !         Plantsum outputs (IDETS)
            !-----------------------------------------------------------------------------------------------------------
            CALL YCA_Out_PlantSum ( &  
                CN          , IDETL       , IDETS       , ON          , REP         , RN          , RUN         , RUNI        , &
                SN          , TN          &
                )   
                
            !-----------------------------------------------------------------------------------------------------------
            !         Output leaves and tiers data (IDETL = Y or D)
            !-----------------------------------------------------------------------------------------------------------
            CALL YCA_Out_LfTier ( &  
                IDETL       , RUN         , STGYEARDOY  &
                )
                
            !-----------------------------------------------------------------------------------------------------------
            !          Outputs work details, phenology and plant responses (IDETL = D).
            !-----------------------------------------------------------------------------------------------------------
            CALL YCA_Out_WrkPhenRes ( & 
                IDETL       , IDETO       , ISWNIT      , RN          , RUN         , TN          &
                )
                
            !-----------------------------------------------------------------------------------------------------------
            !         Errora, Errort, Errors (IDETL = A)
            !-----------------------------------------------------------------------------------------------------------
            CALL YCA_Out_Error ( & 
                IDETL       , RN          , RUN         , TN          &
                )    
                
            !-----------------------------------------------------------------------------------------------------------
            !         Screens for CROPSIM SHELL (IDETD)
            !-----------------------------------------------------------------------------------------------------------
            CALL YCA_Out_CrpSim ( & 
                CN          , DOY         , ON          , REP         , RN          , RUN         , RUNI        , &
                SN          , STGYEARDOY  , TN          , YEAR        &
                )
                
            !-----------------------------------------------------------------------------------------------------------
            !         Screens for sensitivity mode
            !-----------------------------------------------------------------------------------------------------------
            CALL YCA_Out_Sens ( & 
                CN          , DOY         , RNMODE      , STGYEARDOY  , TN          , YEAR        , LAI        &
                )   
                
            !-----------------------------------------------------------------------------------------------------------
            !         Store variables for sending to CSM summary output routines
            !-----------------------------------------------------------------------------------------------------------
            CALL YCA_Out_StoreVars ( & 
                IDETO       , ISWNIT      , STGYEARDOY  &
                )
                
        ENDIF ! End STGYEARDOY(PSX-1) == YEARDOY.OR.DYNAMIC == SEASEND
        
        !---------------------------------------------------------------------------------------------------------------
        !       Store variables for possible use next day/step and re-initialize if harvest/fail
        !---------------------------------------------------------------------------------------------------------------
        CALL YCA_Out_ReInit ( &  
        BRSTAGE     , CN          , DYNAMIC     , NFP         , RNMODE      , SRAD        , STGYEARDOY  , &
        UNH4        , UNO3        , LAI         &
            )
            
    END SUBROUTINE YCA_Output

