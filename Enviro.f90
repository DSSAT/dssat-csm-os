Module Enviro_module !Module of environment
    Type Enviro_type
        
        integer :: HOD_ = 0 ! HOD = Hour of day
        
    contains
    
        procedure, pass (this) :: getHOD
        procedure, pass (this) :: setHOD 
    
    END Type Enviro_type
    
    contains
    
    ! get HOD
    integer function getHOD(this)
        implicit none
        class (Enviro_type), intent(in) :: this
        
        getHOD = this%HOD_
    end function getHOD
    
    
    ! set HOD    
    subroutine setHOD(this, HOD)
        implicit none
        class (Enviro_type), intent(inout) :: this
        integer, intent (in) :: HOD
        
        this%HOD_ = HOD
    end subroutine setHOD
    
    
    integer function fetchTemperature(this)
        implicit none
        class (Enviro_type), intent(in) :: this
        
        fetchTemperature = this%HOD_ +5
    end function fetchTemperature
    
    integer function fetchSVP(this)
        implicit none
        class (Enviro_type), intent(in) :: this
        
        fetchSVP = 610.78 *exp( fetchTemperature(this) / ( fetchTemperature(this)  + 238.3 ) *17.2694 )              ! Saturation vapour pressure, ps, in pascals: ps = 610.78 *exp( t / ( t + 238.3 ) *17.2694 ) 

    end function fetchSVP

        
END Module Enviro_module    
    