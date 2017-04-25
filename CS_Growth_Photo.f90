!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.RATE) lines 4643 - 4706 of the original CSCAS code.The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module CS_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine CS_Growth_Photo calculates assimilation.
!
! NOTE: Hidden code (commented out) gives very different output. Why?
!***************************************************************************************************************************
    
    SUBROUTINE CS_Growth_Photo ( &
        CO2         , NFP         , SLPF        , SRAD        , TAIRHR      , TDEW        , TMAX        , TMIN         &
        )
        
        USE ModuleDefs
        USE ModuleData
        USE CS_First_Trans_m
        USE YCA_Photosyntesis
        
        IMPLICIT  NONE
        
        REAL    CO2         , NFP         , SLPF        , SRAD        , TAIRHR(24)  , TDEW        , TMAX        , TMIN        
        REAL    CSVPSAT                                                                            ! REAL function call
        real availableC
          
        SAVE


        !-----------------------------------------------------------------------
        !           Calculate C assimilation at beginning of day. 
        !-----------------------------------------------------------------------

        ! PAR utilization efficiency
        PARU = PARUE
                
        ! Select method depending on choice in CONTROL FILE
        select case(MEPHO)
            case ('R')
                availableC = availableCarbohydrate_methodR(PARMJFAC, SRAD, PARU, CO2FP, TFP, RSFP, VPDFP, SLPF, PARI, PLTPOP)
                CARBOBEG =  availableC * PARI / PLTPOP                                                                                          !EQN 259
            case ('I')
                availableC = availableCarbohydrate_methodI(CO2, CO2AIR, CO2EX, CO2FP, CO2COMPC, PARMJFAC, PARFC, PARI, PARU, PLTPOP, RATM, RCROP, RLFC, RLF, RSFP, SLPF, SRAD, TMAX, TMIN, TFP, WFP)
                CARBOBEG =  availableC  * SLPF * PARI / PLTPOP * CARBOBEGIA                                                                     !EQN 272
            case ('M')
                availableC = availableCarbohydrate_methodM(CO2AIR,PARU, RATM, RCROP,RLFC, RLF, WFP, MJPERE, PARMJFAC, SRAD, TFP, RSFP, SLPF, PARI, PLTPOP)
                CARBOBEG =  availableC * SLPF * PARI / PLTPOP                                                                            !EQN 276
        end select
        
    END SUBROUTINE CS_Growth_Photo
    