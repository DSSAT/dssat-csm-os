!***************************************************************************************************************************
! This module is intended to calculate environment effects in the plant
! Atributes:
!   
! Object functions:
!        
! Static functions:
!        
! Authors
! @danipilze
!*********

    Module CS_Model_EnvironmentEffect !Module of environment
    type EnvironmentEffect_type
        
        contains

    end Type EnvironmentEffect_type
    
    ! interface to reference the constructor
    interface EnvironmentEffect_type
        module procedure EnvironmentEffect_type_constructor
    end interface EnvironmentEffect_type
    
    contains
    
    ! constructor for the type
    type (EnvironmentEffect_type) function EnvironmentEffect_type_constructor()
        implicit none
       
    end function EnvironmentEffect_type_constructor    
    

   
    !-------------------------------------------
    ! STATIC FUNCTIONS
    !-------------------------------------------

    
    ! calculates proportion of radiation
    ! K is the extinction coefficient
    ! LAI is leaf area index
    real function calculateRadiationP(K, LAI)
        implicit none
        real, intent (in) :: K, LAI
        real :: value = 0
        
        calculateRadiationP = 1 - exp(K*LAI)

    end function calculateRadiationP
    
    
END Module CS_Model_EnvironmentEffect    
    