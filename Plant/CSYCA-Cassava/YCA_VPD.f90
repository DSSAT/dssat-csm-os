!***************************************************************************************************************************
! This module is intended to proporcionate variables from the YCA_First_Trans_m
! in a safe and encapsulated way

! Authors
! @danipilze
!*********

    Module YCA_VPD       
    
    USE ModuleDefs
    type VPD_type
        
        TYPE (ControlType) :: CONTROL_    ! Defined in ModuleDefs
        TYPE (WeatherType) :: WEATHER_    ! Defined in ModuleDefs
        TYPE (SoilType) :: SOILPROP_   ! Defined in ModuleDefs
        
    contains

        procedure, pass (this) :: getEOP
        procedure, pass (this) :: getVPDFP
        
    
    end Type VPD_type
    
    ! interface to reference the constructor
    interface VPD_type
        module procedure VPD_type_constructor
    end interface VPD_type
    
    contains
    
    ! constructor for the type
    type (VPD_type) function VPD_type_constructor(WEATHER, CONTROL, SOILPROP)
        implicit none
        
        
        TYPE (ControlType), intent (in) :: CONTROL    ! Defined in ModuleDefs
        TYPE (WeatherType), intent (in) :: WEATHER    ! Defined in ModuleDefs
        TYPE (SoilType), intent (in) ::   SOILPROP   ! Defined in ModuleDefs
        
        VPD_type_constructor%WEATHER_ = WEATHER
        VPD_type_constructor%CONTROL_ = CONTROL
        VPD_type_constructor%SOILPROP_= SOILPROP
        
    end function VPD_type_constructor    


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
    real function getEOP(this)
        USE ModuleDefs
        Use YCA_Growth_VPD
        
        implicit none
        class (VPD_type), intent(in) :: this
        
        getEOP = get_Growth_EOP(getDAP(), getLAI(), getPHSV(), getPHTV(),  this%WEATHER_, this%CONTROL_, this%SOILPROP_)
        
    end function getEOP
    
    ! get VPDFP
    real function getVPDFP(this)
        USE ModuleDefs
        Use YCA_Growth_VPD
        
        implicit none
        class (VPD_type), intent(in) :: this
        
        getVPDFP = get_Growth_VPDFP(getDAP(), getLAI(), getPHSV(), getPHTV(),  this%WEATHER_, this%CONTROL_, this%SOILPROP_)
        
    end function getVPDFP
    
    
    
END Module YCA_VPD
    