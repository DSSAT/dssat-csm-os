!***************************************************************************************************************************
! This is the code from the section (DYNAMIC == INTEGR) lines 6305 - 6418 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine YCA_Integ_SeasEnd calculate season end soil conditions, and other general variables. 
!***************************************************************************************************************************
    
    SUBROUTINE YCA_Integ_SeasEnd ( &
        BD          , DLAYR       , NH4LEFT     , NLAYR       , NO3LEFT     , RESCALG     , RESLGALG    , RESNALG     , &
        STGYEARDOY  , SW          & 
        )
        
        USE ModuleDefs
        USE YCA_First_Trans_m
        USE YCA_Control_Plant
        
        IMPLICIT NONE
        
        INTEGER NLAYR       , STGYEARDOY(0:19)
        REAL    BD(NL)      , DLAYR(NL)   , NH4LEFT(NL) , NO3LEFT(NL) , RESCALG(0:NL)             , RESLGALG(0:NL)            
        REAL    RESNALG(0:NL)             , SW(NL)      
        
        !-----------------------------------------------------------------------
        !         Calculate season end soil conditions              
        !-----------------------------------------------------------------------
                
        FSOILN = 0.0
        FSOILH2O = 0.0
        DO I = 1, NLAYR
            FSOILN = FSOILN + NO3LEFT(I)/(10.0/(BD(I)*(DLAYR(I)))) + NH4LEFT(I)/(10.0/(BD(I)*(DLAYR(I))))
            FSOILH2O = FSOILH2O + SW(I)*DLAYR(I)
        ENDDO
                
        !-----------------------------------------------------------------------
        !         Calculate variables that are generally measured and presented
        !-----------------------------------------------------------------------
                
        ! Here,reserves are included in leaf,stem,and Plant. stick weights
        ! And weights are in kg/ha
        CWAD = (vegetativeCanopyWeight())*PLTPOP*10.0                                                                       !EQN 318
        SRWAD = SRWT*PLTPOP*10.0
        LLWAD = LFWT*(1.0-LPEFR)*10.0*PLTPOP
        LPEWAD = LFWT*LPEFR*10.0*PLTPOP
        RWAD = RTWT*PLTPOP*10.0
        IF (SEEDRS < 0.0) THEN 
            SEEDRS = SDSZ*(SDRS/100.0)*SPRL   !LPM 23MAR2016  to initialize the value of SEEDRS
        ENDIF
        SDWT = (SEEDRS+SDCOAT)*10.0*PLTPOP
        TWAD = (SEEDRS+SDCOAT+RTWT+totalWeight())* PLTPOP*10.0

        
        ! Leaf petioles NOT included in stem here
        STWAD = STWT*PLTPOP*10.0
        SDWAD = CRWT*PLTPOP*10.0
        RSWAD = RSWT*PLTPOP*10.0
        !LLRSWAD = LLRSWT*PLTPOP*10.0 !LPM 21MAY2015 The reserves distribution will not be included, it needs to be reviewed
        !LPERSWAD = LPERSWT*PLTPOP*10.0 
        !STRSWAD = STRSWT*PLTPOP*10.0
        !CRRSWAD = CRRSWT*PLTPOP*10.0
                
        ! Need to CHECK these
        SENROOTA = SENROOT*10.0*PLTPOP
        SENCAS = SENCS*10.0*PLTPOP
        SENLAS = SENLS*10.0*PLTPOP
        SENTOPLITTERA = SENTOPLITTER*PLTPOP*10.0
        DO L =1,NLAYR
            RTWTAL(L) = RTWTL(L)*PLTPOP*10.0
            SENWAL(L) = SENWL(L)*PLTPOP*10.0
        ENDDO
                
        
                
        VWAD = (vegetativeCanopyWeight())*PLTPOP * 10.0                                                                     !EQN 019
                
        SHNUMAD = SHNUM*PLTPOP
                
        IF (NUPAC < 0.0) THEN
            NUPAC = NUPAD
        ELSE 
            NUPAC = NUPAC+NUPAD
        ENDIF  
        CNAD = (LEAFN+STEMN+RSN)*PLTPOP*10.0
        SRNAD = SROOTN*PLTPOP*10.0
        LLNAD = LEAFN*(1.0-LPEFR)*PLTPOP*10.0
        RNAD = ROOTN*PLTPOP*10.0
        RSNAD = RSN*PLTPOP*10.0
        SDNAD = SEEDN*PLTPOP*10.0
        SNAD = STEMN*PLTPOP*10.0
        TNAD = (ROOTN+LEAFN+STEMN+RSN+HPRODN+SEEDN)*PLTPOP*10.0
        VNAD = (LEAFN+STEMN+RSN)*PLTPOP*10.0                                                                           !EQN 018
                
        ! LAH Note that no reserves included in sancout
        ! SANCOUT = SNAD/(STWAD+STRSWAD + LPEWAD+LPERSWAD)  ! With rs
        IF (STWAD > ZERO) SANCOUT = SNAD/STWAD
                
        HWAD = SRWAD
        ! HWUD = SRWUD ! issue 50
        ! HNUMAD = SRNOPD * PLTPOP ! issue 50
        HNAD = SRNAD
        HNC = SRANC
        SENNAS = SENNS*10.0*PLTPOP
        SENNAL(0) = SENNL(0)*PLTPOP*10.0
        SENNATC = SENNAL(0)+SENNAS
        DO L =1,NLAYR
            SENNAL(L) = SENNL(L)*PLTPOP*10.0
        ENDDO
                
        ! After harvest residues
        IF (STGYEARDOY(11) == YEARDOY) THEN
            ! Surface
            RESWALG(0) = VWAD*(1.0-HBPCF/100.0)
            RESNALG(0) = (LEAFN+STEMN)*PLTPOP*10.*(1.0-HBPCF/100.)
            RESCALG(0) = RESWALG(0) * 0.4
            RESLGALG(0) = LLWAD*LLIGP/100.0*(1.0-HBPCF/100.0)+ LPEWAD*SLIGP/100.0*(1.0-HBPCF/100.0)+ &
                STWAD*SLIGP/100.0*(1.0-HBPCF/100.0)
            ! Soil
            DO L = 1, NLAYR
                RESWALG(L) = RTWTL(L)*PLTPOP*10.0
                RESNALG(L) = RTWTL(L)*PLTPOP*10.0 * RANC
                RESCALG(L) = RTWTL(L)*PLTPOP*10.0 * 0.4
                RESLGALG(L) = RTWTL(L)*PLTPOP*10.0 * RLIGP/100.0
            ENDDO
                    
            ! Surface
            RESWAL(0) = RESWAL(0) + RESWALG(0)
            RESNAL(0) = RESNAL(0) + RESNALG(0)
            RESCAL(0) = RESCAL(0) + RESCALG(0)
            RESLGAL(0) = RESLGAL(0) + RESLGALG(0)
            ! Soil
            DO L = 1, NLAYR
                RESWAL(L) = RESWAL(L) + RESWALG(L)
                RESNAL(L) = RESNAL(L) + RESNALG(L)
                RESCAL(L) = RESCAL(L) + RESCALG(L)
                RESLGAL(L) = RESLGAL(L) + RESLGALG(L)
            ENDDO
        ENDIF
                
    END SUBROUTINE YCA_Integ_SeasEnd
        
