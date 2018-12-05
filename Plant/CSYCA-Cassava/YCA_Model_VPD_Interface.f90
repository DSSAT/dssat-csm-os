!***************************************************************************************************************************
! This module is intended to proporcionate variables from the YCA_First_Trans_m
! in a safe and encapsulated way

! Authors
! @danipilze
!*********

    Module YCA_Model_VPD_Interface       
    
    USE ModuleDefs
    type YCA_VPD_type
        
        TYPE (ControlType) :: CONTROL_    ! Defined in ModuleDefs
        TYPE (WeatherType) :: WEATHER_    ! Defined in ModuleDefs
        TYPE (SoilType) :: SOILPROP_   ! Defined in ModuleDefs
        
    contains

        procedure, pass (this) :: get_CSYCA_EO
        procedure, pass (this) :: get_CSYCA_EOP
        procedure, pass (this) :: get_CSYCA_VPDFP
        
    
    end Type YCA_VPD_type
    
    ! interface to reference the constructor
    interface YCA_VPD_type
        module procedure YCA_VPD_type_constructor
    end interface YCA_VPD_type
    
    contains
    
    ! constructor for the type
    type (YCA_VPD_type) function YCA_VPD_type_constructor(WEATHER, CONTROL, SOILPROP)
        implicit none
        
        
        TYPE (ControlType), intent (in) :: CONTROL    ! Defined in ModuleDefs
        TYPE (WeatherType), intent (in) :: WEATHER    ! Defined in ModuleDefs
        TYPE (SoilType), intent (in) ::   SOILPROP   ! Defined in ModuleDefs
        
        YCA_VPD_type_constructor%WEATHER_ = WEATHER
        YCA_VPD_type_constructor%CONTROL_ = CONTROL
        YCA_VPD_type_constructor%SOILPROP_= SOILPROP
        
    end function YCA_VPD_type_constructor    


    ! get_CSYCA_ DAP
    integer function get_CSYCA_DAP()
        USE YCA_First_Trans_m
        implicit none
        
        get_CSYCA_DAP = DAP
    end function get_CSYCA_DAP
    
    ! get_CSYCA_ LAI
    real function get_CSYCA_LAI()
        USE YCA_First_Trans_m
        implicit none
        
        get_CSYCA_LAI = LAI
    end function get_CSYCA_LAI
    
    ! get_CSYCA_ PHSV
    real function get_CSYCA_PHSV()
        USE YCA_First_Trans_m
        implicit none
        
        get_CSYCA_PHSV = PHSV
    end function get_CSYCA_PHSV
    
    ! get_CSYCA_ PHTV
    real function get_CSYCA_PHTV()
        USE YCA_First_Trans_m
        implicit none
        
        get_CSYCA_PHTV = PHTV
    end function get_CSYCA_PHTV
    
    ! get_CSYCA_ EO
    real function get_CSYCA_EO(this)
        USE ModuleDefs
        Use YCA_Growth_VPD
        
        implicit none
        class (YCA_VPD_type), intent(in) :: this
        
        get_CSYCA_EO = get_Growth_EO(get_CSYCA_DAP(), get_CSYCA_LAI(), get_CSYCA_PHSV(), get_CSYCA_PHTV(),  this%WEATHER_, this%CONTROL_, this%SOILPROP_)
        
    end function get_CSYCA_EO
    
    ! get_CSYCA_ EOP
    real function get_CSYCA_EOP(this)
        USE ModuleDefs
        Use YCA_Growth_VPD
        
        implicit none
        class (YCA_VPD_type), intent(in) :: this
        
        get_CSYCA_EOP = get_Growth_EOP(get_CSYCA_DAP(), get_CSYCA_LAI(), get_CSYCA_PHSV(), get_CSYCA_PHTV(),  this%WEATHER_, this%CONTROL_, this%SOILPROP_)
        
    end function get_CSYCA_EOP
    
    ! get_CSYCA_ VPDFP
    real function get_CSYCA_VPDFP(this)
        USE ModuleDefs
        Use YCA_Growth_VPD
        
        implicit none
        class (YCA_VPD_type), intent(in) :: this
        
        get_CSYCA_VPDFP = get_Growth_VPDFP(get_CSYCA_DAP(), get_CSYCA_LAI(), get_CSYCA_PHSV(), get_CSYCA_PHTV(),  this%WEATHER_, this%CONTROL_, this%SOILPROP_)
        
    end function get_CSYCA_VPDFP
    
    
    
END Module YCA_Model_VPD_Interface
    