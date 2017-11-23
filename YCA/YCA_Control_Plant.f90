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
    
    ! real value of the plant green leaf area
    real function vegetativeCanopyWeight()
        implicit none
        
        vegetativeCanopyWeight = LFWT+STWT+CRWT+RSWT
    end function vegetativeCanopyWeight
    
    ! real value of the plant green leaf area
    real function totalWeight()
        implicit none
        
        totalWeight = vegetativeCanopyWeight()+SRWT
    end function totalWeight
    
    
    END module YCA_Control_Plant 