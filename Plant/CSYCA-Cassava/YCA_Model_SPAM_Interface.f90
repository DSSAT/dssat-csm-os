!***************************************************************************************************************************
! This module is intended to proporcionate variables from the YCA_First_Trans_m
! in a safe and encapsulated way

! Authors
! @danipilze
!*********

    Module YCA_Model_SPAM_Interface       
    
        
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
    real function getEO(WEATHER, CONTROL, SOILPROP)
        USE ModuleDefs
        USE YCA_Growth_VPD
        
        implicit none
        
        TYPE (ControlType), intent (in) :: CONTROL    ! Defined in ModuleDefs
        TYPE (WeatherType), intent (in) :: WEATHER    ! Defined in ModuleDefs
        TYPE (SoilType), intent (in) ::   SOILPROP   ! Defined in ModuleDefs

        
        getEO = getAffectedEO (getDAP(), getLAI(), getPHSV(), getPHTV(),  WEATHER, CONTROL, SOILPROP)
    end function getEO
    
    
    
END Module YCA_Model_SPAM_Interface
    