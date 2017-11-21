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
    
    ! true is leaf is senescing
    logical function isLeafSenescing(node)
        implicit none
        class (Node_type), intent(in) :: node
        
        isLeafSenescing = node%LAGETT > LLIFGTT+LLIFATT .AND. node%LAGETT < LLIFGTT+LLIFATT+LLIFSTT
    end function isLeafSenescing
    
    ! true is leaf is alive
    logical function isLeafAlive(node)
        implicit none
        class (Node_type), intent(in) :: node
        
        isLeafAlive = node%LAGETT < LLIFGTT+LLIFATT+LLIFSTT
    end function isLeafAlive
    
    ! true is leaf is alive
    logical function didLeafFallToday(node)
        implicit none
        class (Node_type), intent(in) :: node
        
        didLeafFallToday = node%LAGETT-TTLFLife*EMRGFR  < LLIFGTT+LLIFATT+LLIFSTT .AND. .NOT. isLeafAlive(node)
    end function didLeafFallToday
    
        ! true is leaf is alive
    logical function didLeafSenescingToday(node)
        implicit none
        class (Node_type), intent(in) :: node
        
        didLeafSenescingToday = node%LAGETT-TTLFLife*EMRGFR  < LLIFGTT+LLIFATT .AND. isLeafSenescing(node)
    end function didLeafSenescingToday
    
    ! set leaf age to active 
    subroutine leafAsActive(node)
        implicit none
        class (Node_type), intent(inout) :: node
        
        node%LAGETT = LLIFGTT
    end subroutine leafAsActive
    
        ! set leaf age to fall 
    subroutine leafAsSenescing(node)
        implicit none
        class (Node_type), intent(inout) :: node
        
        node%LAGETT = LLIFGTT+LLIFATT
    end subroutine leafAsSenescing
    
    ! set leaf age to fall 
    subroutine leafAsFall(node)
        implicit none
        class (Node_type), intent(inout) :: node
        
        node%LAGETT = LLIFGTT+LLIFATT+LLIFSTT
    end subroutine leafAsFall
    
    ! set leaf age to fall 
    subroutine leafAge(node)
        implicit none
        class (Node_type), intent(inout) :: node
        
        node%LAGETT = node%LAGETT + TTLFLife*EMRGFR                                              !EQN 358
    end subroutine leafAge

    
    
    END module YCA_LeafControl 