!!***************************************************************************************************************************
!! This module is intended to calculate behavior of the plant leaf 
!! 10/11/2017 converted from UTF-8 to ANSI
!! Atributes:
!!   
!! Object functions:
!!        
!! Static functions:
!!        
!! Authors
!! @danipilze
!!*********
!
    Module YCA_Control_Plant 
    
     USE YCA_First_Trans_m

    contains
    
    ! real value of the woody weight of the plant: leaves, stem, crown and reserves
    real function woodyWeight()
        implicit none
        
        woodyWeight = STWT+CRWT+SEEDRS+SDCOAT
    end function woodyWeight
    
    ! real value of the canopy weight of the plant: leaves, stem, crown and reserves
    real function canopyWeight()
        implicit none
        
        canopyWeight = woodyWeight()+LFWT
    end function canopyWeight
    
    ! real value of the total weight of the plant: leaves, stem, crown, reserves and root storage organ
    real function totalWeight()
        implicit none
        
        totalWeight = canopyWeight()+SRWT+RTWT
    end function totalWeight
    
    ! real value the plant poulation
    real function plantPopulation()
        implicit none
        
        plantPopulation = PLTPOP*10.0
    end function plantPopulation
    
    
    END module YCA_Control_Plant 