!***************************************************************************************************************************
! This module is intended to calculate plant VPD effects in the plant
! Atributes:
!   
! Object functions:
!        
! Static functions:
!        
! Authors
! @danipilze
!*********

    Module CS_Model_VPDEffect !Module of environment
    type VPDEffect_type
        
        real, private :: VPDTreshold =  0
        
    contains
    
        procedure, pass (this) :: getVPDTreshold
        procedure, pass (this) :: setVPDTreshold
    
    end Type VPDEffect_type
    
    ! interface to reference the constructor
    interface Plant_type
        module procedure Plant_type_constructor
    end interface Plant_type
    
    contains
    
    ! constructor for the type
    type (Plant_type) function Plant_type_constructor(VPDTreshold)
        implicit none
        real, intent (in) :: VPDTreshold
        Plant_type_constructor%VPDTreshold_ = VPDTreshold
        
    end function Plant_type_constructor    
    
        
    !-------------------------------------------
    ! OBJECT FUNCTIONS
    !-------------------------------------------
    
  
   
    !-------------------------------------------
    ! STATIC FUNCTIONS
    !-------------------------------------------
    
    
    !-------------------------------------------
    ! GETTERS AND SETTERS
    !------------------------------------------
    ! get TMin
    real function getVPDTreshold(this)
        implicit none
        class (VPDEffect_type), intent(in) :: this
        
        getVPDTreshold = this%VPDTreshold_
    end function getVPDTreshold
    
    ! set TMin    
    subroutine setTMin(this, VPDTreshold)
        implicit none
        class (VPDEffect_type), intent(inout) :: this
        real, intent (in) :: VPDTreshold
        
        this%VPDTreshold_ = VPDTreshold
    end subroutine setTMin
 
    
    
END Module CS_Model_Environment    
    