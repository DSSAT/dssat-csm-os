!***************************************************************************************************************************
! This module is intended to calculate environment effects in the plant
! 12/09/2017 converted from UTF-8 to ANSI
! Atributes:
!   
! Object functions:
!        
! Static functions:
!        
! Authors
! @danipilze
!*********

    Module YCA_Control_VPDEffect !Module of environment
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
        procedure, pass (this) :: affectStomatalConductance_restricted
    
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
    
    ! retrieves stomatal conductance based on VPD
    real function affectStomatalConductance(this, VPD)
        implicit none
        class (VPDEffect_type), intent(inout) :: this
        real, intent(in) :: VPD
        
        affectStomatalConductance = affectStomatalConductance_restricted(this, VPD, 1.0)        ! running with the maximum value (1.0)

    end function affectStomatalConductance
    
    ! retrieves stomatal conductance based on VPD with restriction 
    ! limit from 0.0 to 1.0
    real function affectStomatalConductance_restricted(this, VPD, limit)
        implicit none
        class (VPDEffect_type), intent(inout) :: this
        real, intent(in) :: VPD, limit
        real :: value
        
        value = calculateStomatalConductance(this%stomatalConductance_, VPD, this%VPDThreshold_, this%VPDSensitivity_, AMIN1(1.0, limit))
        call this%setStomatalConductance(value)
        affectStomatalConductance_restricted = value

    end function affectStomatalConductance_restricted
   
    !-------------------------------------------
    ! STATIC FUNCTIONS
    !-------------------------------------------
    
    ! calculates stomatal conductance acording to the VPD
    ! limit is 1.0 when there is no restrictions                                                ! fraction
    real function calculateStomatalConductance(previousStomatalConductance, VPD, VPDThreshold, VPDSensitivity, limit)
        implicit none
        real, intent (in) :: previousStomatalConductance, VPD, VPDThreshold, VPDSensitivity, limit
        real :: value = 0
        
        if(VPD > VPDThreshold) then
            ! y = mx + b
            ! x = VPD-VPDThreshold
            ! m = VPDSensitivity
            ! b = limit
             value =  (VPDSensitivity * (VPD-VPDThreshold)) + limit
                if(value < 0) then
                    value = 0
                end if
        else
            value = limit 
        end if 
        
        value = AMIN1(value, previousStomatalConductance) ! never increase
        
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
    
    
END Module YCA_Control_VPDEffect    
    