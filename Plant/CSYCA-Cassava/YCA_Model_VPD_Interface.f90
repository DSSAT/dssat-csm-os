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

        procedure, pass (this) :: get_YCA_EO
        procedure, pass (this) :: get_YCA_EOP
        procedure, pass (this) :: get_YCA_VPDFP
        
    
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


    ! get_YCA_ DAP
    integer function get_YCA_DAP()
        USE YCA_First_Trans_m
        implicit none
        
        get_YCA_DAP = DAP
    end function get_YCA_DAP
    
    ! get_YCA_ LAI
    real function get_YCA_LAI()
        USE YCA_First_Trans_m
        implicit none
        
        get_YCA_LAI = LAI
    end function get_YCA_LAI
    
    ! get_YCA_ PHSV
    real function get_YCA_PHSV()
        USE YCA_First_Trans_m
        implicit none
        
        get_YCA_PHSV = PHSV
    end function get_YCA_PHSV
    
    ! get_YCA_ PHTV
    real function get_YCA_PHTV()
        USE YCA_First_Trans_m
        implicit none
        
        get_YCA_PHTV = PHTV
    end function get_YCA_PHTV
    
    ! get_YCA_ EO
    real function get_YCA_EO(this)
        USE ModuleDefs
        Use YCA_Growth_VPD
        
        implicit none
        class (YCA_VPD_type), intent(in) :: this
        
        get_YCA_EO = get_Growth_EO(get_YCA_DAP(), get_YCA_LAI(), get_YCA_PHSV(), get_YCA_PHTV(),  this%WEATHER_, this%CONTROL_, this%SOILPROP_)
        
    end function get_YCA_EO
    
    ! get_YCA_ EOP
    real function get_YCA_EOP(this)
        USE ModuleDefs
        Use YCA_Growth_VPD
        
        implicit none
        class (YCA_VPD_type), intent(in) :: this
        
        get_YCA_EOP = get_Growth_EOP(get_YCA_DAP(), get_YCA_LAI(), get_YCA_PHSV(), get_YCA_PHTV(),  this%WEATHER_, this%CONTROL_, this%SOILPROP_)
        
    end function get_YCA_EOP
    
    ! get_YCA_ VPDFP
    real function get_YCA_VPDFP(this)
        USE ModuleDefs
        Use YCA_Growth_VPD
        
        implicit none
        class (YCA_VPD_type), intent(in) :: this
        
        get_YCA_VPDFP = get_Growth_VPDFP(get_YCA_DAP(), get_YCA_LAI(), get_YCA_PHSV(), get_YCA_PHTV(),  this%WEATHER_, this%CONTROL_, this%SOILPROP_)
        
    end function get_YCA_VPDFP
    
    
    
END Module YCA_Model_VPD_Interface
    