!***************************************************************************************************************************
! This is the code from the section (DYNAMIC == RATE) lines 4643 - 4706 of the original CSCAS code.The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine YCA_Growth_Photo calculates assimilation.
!
! NOTE: Hidden code (commented out) gives very different output. Why?
!***************************************************************************************************************************
    
    SUBROUTINE YCA_Growth_Photo ( &
        CO2         , NFP         , SLPF        , SRAD        , TAIRHR      , TDEW        , TMAX        , TMIN         &
        , CONTROL, WEATHER, SOILPROP)
        
        USE ModuleDefs
        USE ModuleData
        USE YCA_First_Trans_m
        USE YCA_Control_Photosyntesis
        
        IMPLICIT  NONE

        TYPE (ControlType), intent (in) :: CONTROL    ! Defined in ModuleDefs
        TYPE (WeatherType), intent (in) :: WEATHER    ! Defined in ModuleDefs
        TYPE (SoilType), intent (in) ::   SOILPROP   ! Defined in ModuleDefs
        
        REAL    CO2         , NFP         , SLPF        , SRAD        , TAIRHR(24)  , TDEW        , TMAX        , TMIN        
        REAL    CSVPSAT                                                                            ! REAL function call
        real availableCH2O
        
          
        SAVE


        !-----------------------------------------------------------------------
        !           Calculate C assimilation at beginning of day. 
        !-----------------------------------------------------------------------

        ! PAR utilization efficiency
        PARU = PARUE
                
        ! Select method depending on choice in CONTROL FILE
        select case(MEPHO)
            case ('R')
                availableCH2O = availableCarbohydrate_methodR(PARMJFAC, SRAD, PARU, CO2FP, TFP, RSFP, SLPF, PARI, PLTPOP, WEATHER, CONTROL, SOILPROP)
            case ('I')
                availableCH2O = availableCarbohydrate_methodI(CO2, CO2AIR, CO2EX, CO2FP, CO2COMPC, PARMJFAC, PARFC, PARI, PARU, PLTPOP, RATM, RCROP, RLFC, RLF, RSFP, SLPF, SRAD, TMAX, TMIN, TFP, WFP)
            case ('M')
                availableCH2O = availableCarbohydrate_methodM(CO2AIR,PARU, RATM, RCROP,RLFC, RLF, WFP, MJPERE, PARMJFAC, SRAD, TFP, RSFP, SLPF, PARI, PLTPOP)
            end select
        CARBOBEG = availableCH2O
        
    END SUBROUTINE YCA_Growth_Photo
    