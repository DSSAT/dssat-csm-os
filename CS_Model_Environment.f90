Module Environment_module !Module of environment
    type Environment_type
        
        integer :: HOD_ = 0 ! HOD = Hour of day
        
    contains
    
        procedure, pass (this) :: getHOD
        procedure, pass (this) :: setHOD 
    
    end Type Environment_type
    
    ! interface to reference the constructor
    interface Environment_type
        module procedure environment_type_constructor
    end interface Environment_type
    
    contains
    
    ! constructor for the type
    type (Environment_type) function environment_type_constructor(HOD)
        implicit none
        integer, intent (in) :: HOD
        environment_type_constructor%HOD_ = HOD
    end function environment_type_constructor
    

    ! get HOD
    integer function getHOD(this)
        implicit none
        class (Environment_type), intent(in) :: this
        
        getHOD = this%HOD_
    end function getHOD
    
    
    ! set HOD    
    subroutine setHOD(this, HOD)
        implicit none
        class (Environment_type), intent(inout) :: this
        integer, intent (in) :: HOD
        
        this%HOD_ = HOD
    end subroutine setHOD
    
    
    real function fetchTemperature(this)
        implicit none
        class (Environment_type), intent(in) :: this
        
        fetchTemperature = 20
        
        SELECT CASE (this%HOD_) ! DA dummy standard curve, this is an example for testing purpuses, TODO the correct is to retrive the temp from somewhere
            CASE (8)
                fetchTemperature = 21.0
            CASE (9)
                fetchTemperature = 23.0
            CASE (10)
                fetchTemperature = 24.0
            CASE (11)
                fetchTemperature = 26.0
            CASE (12)
                fetchTemperature = 27.0
            CASE (13)
                fetchTemperature = 28.0
            CASE (14)
                fetchTemperature = 28.2
            CASE (15)
                fetchTemperature = 26
             CASE (16)
                fetchTemperature = 25
            CASE (17)
                fetchTemperature = 24
            CASE (18)
                fetchTemperature = 23
            CASE (19)
                fetchTemperature = 22
            CASE (20)
                fetchTemperature = 21
            CASE DEFAULT
                fetchTemperature = 20
        END SELECT
        
        
        
    end function fetchTemperature
    
    ! obtain the Saturation Vapour Pressure (pascals)
    real function fetchSVP(this)
        implicit none
        class (Environment_type), intent(in) :: this
        
        fetchSVP = 610.78 * exp( fetchTemperature(this) / ( fetchTemperature(this)  + 238.3 ) * 17.2694 )        !  DA Saturation vapour pressure in pascals: svp = 610.78 *exp( t / ( t + 238.3 ) *17.2694 ) 

    end function fetchSVP
    
    ! obtain the water holding capacity of the air (kg/m3)
    real function fetchWHC(this)
        implicit none
        class (Environment_type), intent(in) :: this
        
        fetchWHC = 0.002166 * fetchSVP(this) / ( fetchTemperature(this) + 273.16 )                                !  DA water holding capacity of the air WHC = 0.002166 * SVP / ( t + 273.16 )   

    end function fetchWHC

        
END Module Environment_module    
    