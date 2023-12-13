!***************************************************************************************************************************
! This module is intended to proporcionate variables from the YCA_First_Trans_m
! in a safe and encapsulated way

! Authors
! @danipilze
!*********

    Module YCA_Model_VPD_Interface       
    
    USE ModuleDefs
    type YCA_VPD_type
        
        TYPE (ControlType) :: CONTROL    ! Defined in ModuleDefs
        TYPE (WeatherType) :: WEATHER    ! Defined in ModuleDefs
        TYPE (SoilType) :: SOILPROP   ! Defined in ModuleDefs
        
    contains

        procedure, pass (this) :: get_YCA_VPDFPHR
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
        
        YCA_VPD_type_constructor%WEATHER = WEATHER
        YCA_VPD_type_constructor%CONTROL = CONTROL
        YCA_VPD_type_constructor%SOILPROP= SOILPROP
        
    end function YCA_VPD_type_constructor    


    ! get_YCA_ DAP
    integer function get_YCA_DAP()
        USE YCA_First_Trans_m
        implicit none
        
        get_YCA_DAP = DAP
    end function get_YCA_DAP
    
   
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
    
    
    
    ! get_YCA_ VPDFPHR
    real function get_YCA_VPDFPHR(this, hour)
        USE ModuleDefs
        Use YCA_Growth_VPD
        
        implicit none
        class (YCA_VPD_type), intent(in) :: this
        integer, intent(in) :: hour
        
        get_YCA_VPDFPHR = get_Growth_VPDFPHR(get_YCA_PHSV(), get_YCA_PHTV(),  this%WEATHER % TDEW, this%WEATHER % TMIN, this%WEATHER % TAIRHR,hour)
    end function get_YCA_VPDFPHR
    
    ! get_YCA_ VPDFP
    real function get_YCA_VPDFP(this) !,LAI)
        USE ModuleDefs
        Use YCA_Growth_VPD
        
        implicit none
        class (YCA_VPD_type), intent(in) :: this
!       REAL LAI
        
!       get_YCA_VPDFP = get_Growth_VPDFP(get_YCA_DAP(), LAI, get_YCA_PHSV(), get_YCA_PHTV(),  this%WEATHER, this%CONTROL, this%SOILPROP)
        get_YCA_VPDFP = get_Growth_VPDFP(get_YCA_PHSV(), get_YCA_PHTV(),  this%WEATHER, this%SOILPROP)

    end function get_YCA_VPDFP
    
    
    
END Module YCA_Model_VPD_Interface
    