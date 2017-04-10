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
        
        real, private :: VPDTreshold_ =  0                ! VPD Treshold to start reducing stomatal conductance (KPa)
        real, private :: VPDSensitivity_ =  0             ! Plant sensitivity to VPD (fraction/Kpa) 
        real, private :: stomatalConductance_ =  1         ! Currente stomatal conductance
        
    contains
    
        procedure, pass (this) :: getVPDTreshold
        procedure, pass (this) :: setVPDTreshold
        procedure, pass (this) :: getVPDSensitivity
        procedure, pass (this) :: setVPDSensitivity
    
    end Type VPDEffect_type
    
    ! interface to reference the constructor
    interface VPDEffect_type
        module procedure VPDEffect_type_constructor
    end interface VPDEffect_type
    
    contains
    
    ! constructor for the type
    type (VPDEffect_type) function VPDEffect_type_constructor(VPDTreshold, VPDSensitivity)
        implicit none
        real, intent (in) :: VPDTreshold, VPDSensitivity
        VPDEffect_type_constructor%VPDTreshold_ = VPDTreshold
        VPDEffect_type_constructor%VPDSensitivity_ = VPDSensitivity
        VPDEffect_type_constructor%stomatalConductance_= 1
        
    end function VPDEffect_type_constructor    
    
        
    !-------------------------------------------
    ! OBJECT FUNCTIONS
    !-------------------------------------------
    
    ! reset the stomatal conductante to the maximum fraction
    real function resetStomatalConductance(this)
        implicit none
        class (VPDEffect_type), intent(in) :: this
        this%StomatalConductance_ = 1 

    end function resetStomatalConductance
    
    ! calculates stomatal conductante to the maximum fraction
    real function getStomatalConductance(this, VPD)
        implicit none
        class (VPDEffect_type), intent(in) :: this
        
        !TODO
        this%stomatalConductance_ = 1 
        
        getStomatalConductance = this%stomatalConductance_

    end function getStomatalConductance
   
    !-------------------------------------------
    ! STATIC FUNCTIONS
    !-------------------------------------------
    
    
    !-------------------------------------------
    ! GETTERS AND SETTERS
    !------------------------------------------
    ! get VPDTreshold
    real function getVPDTreshold(this)
        implicit none
        class (VPDEffect_type), intent(in) :: this
        
        getVPDTreshold = this%VPDTreshold_
    end function getVPDTreshold
    
    ! set VPDTreshold    
    subroutine setVPDTreshold(this, VPDTreshold)
        implicit none
        class (VPDEffect_type), intent(inout) :: this
        real, intent (in) :: VPDTreshold
        
        this%VPDTreshold_ = VPDTreshold
    end subroutine setVPDTreshold
    
    ! get VPDSensitivity
    real function getVPDSensitivity(this)
        implicit none
        class (VPDEffect_type), intent(in) :: this
        
        getVPDSensitivity = this%VPDSensitivity_
    end function getVPDSensitivity
    
    ! set VPDSensitivity    
    subroutine setVPDSensitivity(this, VPDSensitivity)
        implicit none
        class (VPDEffect_type), intent(inout) :: this
        real, intent (in) :: VPDSensitivity
        
        this%VPDSensitivity_ = VPDSensitivity
    end subroutine setVPDSensitivity
    
    
END Module CS_Model_VPDEffect    
    