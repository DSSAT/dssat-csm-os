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
        CWAD = (canopyWeight())*plantPopulation()                                                                       !EQN 318
        SRWAD = SRWT*plantPopulation()
        FHWAD = SRWTF*plantPopulation()
        LLWAD = LFWT*plantPopulation()
        LPEWAD = LFWT*LPEFR*plantPopulation()
        RWAD = RTWT*plantPopulation()
        IF (SEEDRS < 0.0) THEN 
            SEEDRS = SDSZ*(SDRS/100.0)*SPRL   !LPM 23MAR2016  to initialize the value of SEEDRS
        ENDIF
        SDWT = (SEEDRS+SDCOAT)*plantPopulation()
        TWAD = totalWeight()* plantPopulation()

        
        ! Leaf petioles NOT included in stem here
        STWAD = STWT*plantPopulation()
        SDWAD = (CRWT*plantPopulation())+SDWT
        RSWAD = RSWT*plantPopulation()
        !LLRSWAD = LLRSWT*plantPopulation() !LPM 21MAY2015 The reserves distribution will not be included, it needs to be reviewed
        !LPERSWAD = LPERSWT*plantPopulation() 
        !STRSWAD = STRSWT*plantPopulation()
        !CRRSWAD = CRRSWT*plantPopulation()
                
        ! Need to CHECK these
        SENROOTA = SENROOT*plantPopulation()
        SENCAS = SENCS*plantPopulation()
        SENLAS = SENLS*plantPopulation()
        SENTOPLITTERA = SENTOPLITTER*plantPopulation()
        DO L =1,NLAYR
            RTWTAL(L) = RTWTL(L)*plantPopulation()
            SENWAL(L) = SENWL(L)*plantPopulation()
        ENDDO
                
        
                
        VWAD = (canopyWeight())*plantPopulation()                                                                    !EQN 019
                
        SHNUMAD = SHNUM*PLTPOP
                
        IF (NUPAC < 0.0) THEN
            NUPAC = NUPAD
        ELSE 
            NUPAC = NUPAC+NUPAD
        ENDIF  
        CNAD = (LEAFN+STEMN+RSN)*plantPopulation()
        SRNAD = SROOTN*plantPopulation()
        LLNAD = LEAFN*(1.0-LPEFR)*plantPopulation()
        RNAD = ROOTN*plantPopulation()
        RSNAD = RSN*plantPopulation()
        SDNAD = SEEDN*plantPopulation()
        SNAD = STEMN*plantPopulation()
        TNAD = (ROOTN+LEAFN+STEMN+RSN+HPRODN+SEEDN)*plantPopulation()
        VNAD = (LEAFN+STEMN+RSN)*plantPopulation()                                                                           !EQN 018
                
        ! LAH Note that no reserves included in sancout
        ! SANCOUT = SNAD/(STWAD+STRSWAD + LPEWAD+LPERSWAD)  ! With rs
        IF (STWAD > ZERO) SANCOUT = SNAD/STWAD
        IF (LLWAD > ZERO) LANCM = LLNAD/LLWAD
                
        HWAD = SRWAD
        !LPM 14apr2021 Adding the % of dry matter 
        IF (FHWAD > 0.0) THEN
            PDMCD = HWAD / FHWAD
        ENDIF
        
        ! HWUD = SRWUD ! issue 50
        ! HNUMAD = SRNOPD * PLTPOP ! issue 50
        HNAD = SRNAD
        HNC = SRANC
        SENNAS = SENNS*plantPopulation()
        SENNAL(0) = SENNL(0)*plantPopulation()
        SENNATC = SENNAL(0)+SENNAS
        DO L =1,NLAYR
            SENNAL(L) = SENNL(L)*plantPopulation()
        ENDDO
                
        ! After harvest residues
        IF (STGYEARDOY(PSX+1) == YEARDOY) THEN
            ! Surface
            RESWALG(0) = VWAD*(1.0-HBPCF/100.0)
            RESNALG(0) = (LEAFN+STEMN)*PLTPOP*10.*(1.0-HBPCF/100.)
            RESCALG(0) = RESWALG(0) * 0.4
            RESLGALG(0) = LLWAD*LLIGP/100.0*(1.0-HBPCF/100.0)+ LPEWAD*SLIGP/100.0*(1.0-HBPCF/100.0)+ &
                STWAD*SLIGP/100.0*(1.0-HBPCF/100.0)
            ! Soil
            DO L = 1, NLAYR
                RESWALG(L) = RTWTL(L)*plantPopulation()
                RESNALG(L) = RTWTL(L)*plantPopulation() * RANC
                RESCALG(L) = RTWTL(L)*plantPopulation() * 0.4
                RESLGALG(L) = RTWTL(L)*plantPopulation() * RLIGP/100.0
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
        
