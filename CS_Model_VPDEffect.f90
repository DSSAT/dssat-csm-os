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
        
        real, private :: VPDThreshold_ =  0                ! VPD Threshold to start reducing stomatal conductance (KPa)
        real, private :: VPDSensitivity_ =  0              ! Plant sensitivity to VPD (fraction/Kpa) 
        real, private :: stomatalConductance_ =  1         ! Current stomatal conductance
        
    contains
    
        procedure, pass (this) :: getVPDThreshold
        procedure, pass (this) :: setVPDThreshold
        procedure, pass (this) :: getVPDSensitivity
        procedure, pass (this) :: setVPDSensitivity
        procedure, pass (this) :: getStomatalConductance
        procedure, pass (this) :: setStomatalConductance
        procedure, pass (this) :: affectStomatalConductance
    
    end Type VPDEffect_type
    
    ! interface to reference the constructor
    interface VPDEffect_type
        module procedure VPDEffect_type_constructor
    end interface VPDEffect_type
    
    contains
    
    ! constructor for the type
    type (VPDEffect_type) function VPDEffect_type_constructor(VPDThreshold, VPDSensitivity)
        implicit none
        real, intent (in) :: VPDThreshold, VPDSensitivity
        VPDEffect_type_constructor%VPDThreshold_ = VPDThreshold
        VPDEffect_type_constructor%VPDSensitivity_ = VPDSensitivity
        VPDEffect_type_constructor%stomatalConductance_= 1
        
    end function VPDEffect_type_constructor    
    
        
    !-------------------------------------------
    ! OBJECT FUNCTIONS
    !-------------------------------------------
    
    ! resets the stomatal conductante to the maximum fraction
    subroutine resetStomatalConductance(this)
        implicit none
        class (VPDEffect_type), intent(inout) :: this
        this%StomatalConductance_ = 1 

    end subroutine resetStomatalConductance
    
    ! retrieves stomatal conductance
    real function affectStomatalConductance(this, VPD)
        implicit none
        class (VPDEffect_type), intent(inout) :: this
        real, intent(in) :: VPD
        real :: value
        
        value = calculateStomatalConductance(VPD, this%VPDThreshold_, this%VPDSensitivity_)
        call this%setStomatalConductance(value)
        affectStomatalConductance = value

    end function affectStomatalConductance
   
    !-------------------------------------------
    ! STATIC FUNCTIONS
    !-------------------------------------------
    
    ! calculates stomatal conductante acording to the VPD
    real function calculateStomatalConductance(VPD, VPDThreshold, VPDSensitivity)
        implicit none
        real, intent (in) :: VPD, VPDThreshold, VPDSensitivity
        real :: value = 0
        
        if(VPD > VPDThreshold) then
             value = 1 + (VPDSensitivity * (VPD-VPDThreshold)) 
                if(value < 0) then
                    value = 0
                end if
        else
            value = 1 
        end if 
        
        calculateStomatalConductance = value

    end function calculateStomatalConductance
    
    
    !-------------------------------------------
    ! GETTERS AND SETTERS
    !------------------------------------------
    ! get VPDTreshold
    real function getVPDThreshold(this)
        implicit none
        class (VPDEffect_type), intent(in) :: this
        
        getVPDThreshold = this%VPDThreshold_
    end function getVPDThreshold
    
    ! set VPDTreshold    
    subroutine setVPDThreshold(this, VPDThreshold)
        implicit none
        class (VPDEffect_type), intent(inout) :: this
        real, intent (in) :: VPDThreshold
        
        this%VPDThreshold_ = VPDThreshold
    end subroutine setVPDThreshold
    
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
    
    ! get stomatal conductance
    real function getStomatalConductance(this)
        implicit none
        class (VPDEffect_type), intent(in) :: this
        
        getStomatalConductance = this%stomatalConductance_
    end function getStomatalConductance
    
    ! set stomatal conductance    
    subroutine setStomatalConductance(this, stomatalConductance)
        implicit none
        class (VPDEffect_type), intent(inout) :: this
        real, intent (in) :: stomatalConductance
        
        this%stomatalConductance_ = stomatalConductance
    end subroutine setStomatalConductance
    
    
END Module CS_Model_VPDEffect    
    