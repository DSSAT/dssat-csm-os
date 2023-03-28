!***************************************************************************************************************************
! This module is intended to calculate photosynthesis effects in the plant
! 12/09/2017 converted from UTF-8 to ANSI
! Atributes:
!   
! Object functions:
!        
! Static functions:
!        
! Authors
! @danipilze
!*********

    Module YCA_Control_Photosyntesis !Module of environment

    
    
    
    
    contains
    
    
    
    
    !-------------------------------------------
    ! STATIC FUNCTIONS
    !-------------------------------------------
 ! Conventional method using PAR utilization efficiency (R)
     real function availableCarbohydrate_methodR(PARMJFAC, SRAD, PARU, CO2FP, TFP, RSFP, VPDFP, SLPF, PARI, PLTPOP, WFP)
        implicit none
        real, intent (in) :: PARMJFAC, SRAD, PARU, CO2FP, TFP, RSFP, VPDFP, SLPF, PARI, PLTPOP, WFP
        real :: CARBOTMPR = 0
          
         CARBOTMPR = AMAX1(0.0,(PARMJFAC*SRAD)*PARU*CO2FP*TFP* RSFP * VPDFP * SLPF*WFP)
        
        availableCarbohydrate_methodR = CARBOTMPR * PARI / PLTPOP		
        
    end function availableCarbohydrate_methodR
      

    ! Conventional method using PAR utilization efficiency (V)
!   real function availableCarbohydrate_methodV(PARMJFAC, SRAD, PARU, CO2FP, TFP, RSFP, SLPF, PARI, PLTPOP, LAI, WFP, WEATHER, CONTROL, SOILPROP)
    real function availableCarbohydrate_methodV(PARMJFAC,       PARU, CO2FP, TFP, RSFP, SLPF, PARI, PLTPOP,      WFP, WEATHER, CONTROL, SOILPROP)
        USE ModuleDefs
        USE YCA_Model_VPD_Interface
        
        implicit none
        external VPSAT
        
        
        real, intent (in) :: PARMJFAC, PARU, CO2FP, TFP, RSFP, SLPF, PARI, PLTPOP, WFP !, SRAD
        TYPE (ControlType), intent (in) :: CONTROL    ! Defined in ModuleDefs
        TYPE (WeatherType) WEATHER    ! Defined in ModuleDefs
        TYPE (SoilType), intent (in) ::   SOILPROP   ! Defined in ModuleDefs
        
        TYPE (YCA_VPD_Type) :: VPD
        real :: CARBOTMPR = 0
        REAL VPDF, VPD_TRANSP, VPSAT, TDEW  !, LAI
        real, DIMENSION(TS) :: CARBOTMPRHR, RADHR, VPDHR, TAIRHR  !VPDFPHR, 
        integer :: hour = 1
        
        RADHR  = WEATHER % RADHR
        TAIRHR = WEATHER % TAIRHR
        TDEW   = WEATHER % TDEW
        
        
        VPD = YCA_VPD_Type(WEATHER, CONTROL, SOILPROP)
        
        CARBOTMPR = 0.0
        VPD_TRANSP = 0.0

        !LPM 01APR2021 Bring back WFP
        DO hour = 1, TS
            VPDHR(hour) = (VPSAT(TAIRHR(hour)) - VPSAT(TDEW))/1000.0                   ! VPDHR = VPD, hourly (kPa) 
            VPD_TRANSP = VPD_TRANSP + VPDHR(hour)
            CARBOTMPRHR(hour) = AMAX1(0.0,(PARMJFAC*RADHR(hour)*3.6/1000.)*PARU*CO2FP*TFP* RSFP * VPD%get_YCA_VPDFPHR(hour) * SLPF *WFP)       ! MF 17SE14 RADHR is in J/m2/s. Multiply by 3600 for hour, divide by 10^6 for MJ.
            CARBOTMPR = CARBOTMPR + CARBOTMPRHR(hour)
        END DO
        
        availableCarbohydrate_methodv = CARBOTMPR * PARI / PLTPOP                   !EQN 259
        
        VPD_TRANSP = VPD_TRANSP/TS
        
!       VPDF = AMAX1(0.0,(1.0 - VPD%get_YCA_VPDFP(LAI)))
        VPDF = AMAX1(0.0,(1.0 - VPD%get_YCA_VPDFP()))
        Weather % VPDF = vpdf
        Weather % VPD_TRANSP = VPD_TRANSP
        
    end function availableCarbohydrate_methodV
        

    
    
END Module YCA_Control_Photosyntesis
    