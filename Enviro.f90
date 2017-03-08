Module Enviro_module !Module of environment
    Type Enviro_type
        
        integer :: HOD_ = 0 ! HOD = Hour of day
        
    contains
    
        procedure, pass (this) :: getHOD
        procedure, pass (this) :: setHOD 
        procedure, pass (this) :: fetchTemperature
    
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
    
    
    subroutine fetchTemperature(this)
        implicit none
        class (Enviro_type), intent(in) :: this
        
        fetchTemperature = this%HOD_ +5
    end subroutine fetchTemperature

        
END Module Enviro_module    
    