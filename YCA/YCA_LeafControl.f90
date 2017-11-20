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
    logical function isLeafActive(node)
        implicit none
        class (Node_type), intent(in) :: node
        
        isLeafActive = node%LAGETT < LLIFGTT+LLIFATT
    end function isLeafActive
    
    ! true is leaf is alive
    logical function isLeafAlive(node)
        implicit none
        class (Node_type), intent(in) :: node
        
        isLeafAlive = node%LAGETT < LLIFGTT+LLIFATT+LLIFSTT
    end function isLeafAlive
    
    ! set leaf age to active 
    subroutine setLeafAsActive(node)
        implicit none
        class (Node_type), intent(inout) :: node
        
        node%LAGETT = LLIFGTT
    end subroutine setLeafAsActive
    
        ! set leaf age to fall 
    subroutine setLeafAsSenescing(node)
        implicit none
        class (Node_type), intent(inout) :: node
        
        node%LAGETT = LLIFGTT+LLIFATT
    end subroutine setLeafAsSenescing
    
    ! set leaf age to fall 
    subroutine setLeafAsFall(node)
        implicit none
        class (Node_type), intent(inout) :: node
        
        node%LAGETT = LLIFGTT+LLIFATT+LLIFSTT
    end subroutine setLeafAsFall

    
    
    END module YCA_LeafControl 