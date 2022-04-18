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
    Module YCA_Control_Leaf 
    
     USE YCA_First_Trans_m
     USE YCA_Node
     
    contains
    
  
    
    ! true is leaf is active
    logical function isLeafExpanding(node)
        implicit none
        class (Node_type), intent(in) :: node
        
        isLeafExpanding = node%LAGETT <= LLIFGTT
    end function isLeafExpanding
    
    ! true is leaf is active
    logical function isLeafActive(node)
        implicit none
        class (Node_type), intent(in) :: node
        
        isLeafActive = node%LAGETT >= LLIFGTT .AND. node%LAGETT < LLIFGTT+LLIFATT
    end function isLeafActive
    
    ! true is leaf is senescing
    logical function isLeafSenescing(node)
        implicit none
        class (Node_type), intent(in) :: node
        
        isLeafSenescing = node%LAGETT >= LLIFGTT+LLIFATT .AND. node%LAGETT < LLIFGTT+LLIFATT+LLIFSTT
    end function isLeafSenescing
    
    ! true is leaf is alive
    logical function isLeafAlive(node)
        implicit none
        class (Node_type), intent(in) :: node
        
        isLeafAlive = node%LAGETT < LLIFGTT+LLIFATT+LLIFSTT
    end function isLeafAlive
    
    ! true is leaf was active today
    logical function didLeafStartActiveToday(node)
        implicit none
        class (Node_type), intent(in) :: node
        
        didLeafStartActiveToday = node%LAGETT-dailyGrowth()  < LLIFGTT .AND. isLeafActive(node)
    end function didLeafStartActiveToday
    
    ! true is leaf started senescing today
    logical function didLeafStartSenescingToday(node)
        implicit none
        class (Node_type), intent(in) :: node
        
        didLeafStartSenescingToday = node%LAGETT-dailyGrowth()  < LLIFGTT+LLIFATT .AND. isLeafSenescing(node)
    end function didLeafStartSenescingToday
    
    ! true is leaf will start senescing today
    logical function willLeafStillGrowingToday(node)
        implicit none
        class (Node_type), intent(in) :: node
        
        willLeafStillGrowingToday =  node%LAGETT+dailyGrowth()  <= LLIFGTT+LLIFATT                                                      !EQN 371
    end function willLeafStillGrowingToday
    
    ! true is leaf fell today
    logical function didLeafFallToday(node)
        implicit none
        class (Node_type), intent(in) :: node
        
        didLeafFallToday = node%LAGETT-dailyGrowth()  < LLIFGTT+LLIFATT+LLIFSTT .AND. .NOT. isLeafAlive(node)
    end function didLeafFallToday
    
    ! real value of the leaf area left to senesce
    real function leafAreaLeftToSenesce(node)
        implicit none
        class (Node_type), intent(in) :: node
        
        leafAreaLeftToSenesce = node%LATL3T - node%LAPS
    end function leafAreaLeftToSenesce
    
    ! real value of the plant leaf area left to senesce
    real function plantLeafAreaLeftToSenesce()
        implicit none
        
        plantLeafAreaLeftToSenesce = PLA-SENLA
    end function plantLeafAreaLeftToSenesce
    
    ! real value of the plant green leaf area
    real function plantGreenLeafArea()
        implicit none
        
        plantGreenLeafArea = plantLeafAreaLeftToSenesce()-LAPHC
    end function plantGreenLeafArea
    
    ! set leaf age to active 
    subroutine setLeafAsActive(node)
        implicit none
        class (Node_type), intent(inout) :: node
        
        node%LAGETT = LLIFGTT
    end subroutine setLeafAsActive
    
    ! set leaf age to senescing 
    subroutine setLeafAsSenescing(node)
        implicit none
        class (Node_type), intent(inout) :: node
        
        node%LAGETT = LLIFGTT+LLIFATT                                             !EQN 359
    end subroutine setLeafAsSenescing
    
    ! set leaf age to fall 
    subroutine setLeafAsFall(node)
        implicit none
        class (Node_type), intent(inout) :: node
        
        node%LAGETT = LLIFGTT+LLIFATT+LLIFSTT
    end subroutine setLeafAsFall
    
    ! increase leaf age 
    subroutine leafAge(node)
        implicit none
        class (Node_type), intent(inout) :: node
        
        node%LAGETT = node%LAGETT + dailyGrowth()                                              !EQN 358
    end subroutine leafAge
    
    ! real value of the daily growth
    real function dailyGrowth()
        implicit none
        
        IF (WFG < 1.0) THEN
            dailyGrowth = (TTLFLife*EMRGFR)*(1.0 + (WFSU * (1.0 - WFG)))
        ELSE 
            dailyGrowth = (TTLFLife*EMRGFR)
        ENDIF
        
    end function dailyGrowth
    
    ! real value of leaf total senesced weight
    real function leafTotalSenescedWeight()
        implicit none
        
        leafTotalSenescedWeight = SENLFG + SENLFGRS
    end function leafTotalSenescedWeight

    
    
    END module YCA_Control_Leaf 