!***************************************************************************************************************************
! This module is intended to proporcionate variables from the YCA_First_Trans_m
! in a safe and encapsulated way

! Authors
! @danipilze
!*********

    Module YCA_Model_VPD_Interface       
    
        
    contains


    ! get DAP
    integer function getDAP()
        USE YCA_First_Trans_m
        implicit none
        
        getDAP = DAP
    end function getDAP
    
    ! get LAI
    real function getLAI()
        USE YCA_First_Trans_m
        implicit none
        
        getLAI = LAI
    end function getLAI
    
    ! get PHSV
    real function getPHSV()
        USE YCA_First_Trans_m
        implicit none
        
        getPHSV = PHSV
    end function getPHSV
    
    ! get PHTV
    real function getPHTV()
        USE YCA_First_Trans_m
        implicit none
        
        getPHTV = PHTV
    end function getPHTV
    
    ! get EOP
    real function getVPDFP(WEATHER, CONTROL, SOILPROP)
        USE ModuleDefs
        Use YCA_Growth_VPD
        
        implicit none
        
        TYPE (ControlType), intent (in) :: CONTROL    ! Defined in ModuleDefs
        TYPE (WeatherType), intent (in) :: WEATHER    ! Defined in ModuleDefs
        TYPE (SoilType), intent (in) ::   SOILPROP   ! Defined in ModuleDefs

        
        getVPDFP = get_Growth_VPDFP (getDAP(), getLAI(), getPHSV(), getPHTV(),  WEATHER, CONTROL, SOILPROP)
    end function getVPDFP
    
    
    
END Module YCA_Model_VPD_Interface
    