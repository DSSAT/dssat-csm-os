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
        
        real, private :: VPDTreshold_ =  0
        
    contains
    
        procedure, pass (this) :: getVPDTreshold
        procedure, pass (this) :: setVPDTreshold
    
    end Type VPDEffect_type
    
    ! interface to reference the constructor
    interface VPDEffect_type
        module procedure VPDEffect_type_constructor
    end interface VPDEffect_type
    
    contains
    
    ! constructor for the type
    type (VPDEffect_type) function VPDEffect_type_constructor(VPDTreshold)
        implicit none
        real, intent (in) :: VPDTreshold
        VPDEffect_type_constructor%VPDTreshold_ = VPDTreshold
        
    end function VPDEffect_type_constructor    
    
        
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
    subroutine setVPDTreshold(this, VPDTreshold)
        implicit none
        class (VPDEffect_type), intent(inout) :: this
        real, intent (in) :: VPDTreshold
        
        this%VPDTreshold_ = VPDTreshold
    end subroutine setVPDTreshold
 
    
    
END Module CS_Model_VPDEffect    
    