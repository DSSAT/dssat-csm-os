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
    Module YCA_LeafControl 
    
     USE YCA_First_Trans_m
     USE YCA_Node
     
    contains
    
  
    
    ! true is leaf is active
    logical function isActive(leaf)
        implicit none
        class (Node_type), intent(in) :: leaf
        
        isActive = leaf%LAGETT < LLIFGTT+LLIFATT
    end function isActive
    
    ! true is leaf is alive
    logical function isAlive(leaf)
        implicit none
        class (Node_type), intent(in) :: leaf
        
        isAlive = leaf%LAGETT < LLIFGTT+LLIFATT+LLIFSTT
    end function isAlive

    
    
    END module YCA_LeafControl 