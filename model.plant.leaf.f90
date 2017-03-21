Module Leaf_module !Module of environment
    type Leaf_type
        
        real :: areaGrowth_ = 0                 ! Leaf area growth,shoot,lf pos  cm2/l 
        ! TODO 
        ! [ ] rename variables to give more semantic
        ! [ ] generate getters and setters
        REAL    :: LAGETT                       ! Leaf age after growing         C.d        ! (From SeasInit) !LPM 25MAR15 Adjusted to consider two dimensions    
        REAL    :: LAGL                         ! Leaf area growth,shoot,lf pos  cm2/l      ! (From SeasInit) !LPM 25MAR15 Adjusted to consider two dimensions  
        REAL    :: LAGL3                        ! Leaf area growth,shoot,lf+assim cm2/l     ! (From SeasInit) !LPM 15NOV15 Added to save leaf area by cohort
        REAL    :: LAGL3T                       ! Leaf area by cohort lf+assim   cm2/cohort ! (From SeasInit) !LPM 15NOV15 Added to save leaf area by cohort
        REAL    :: LAGLT                        ! Leaf area growth by cohort     cm2/cohort ! (From SeasInit) !LPM 25OCT15 added to keep the leaf area by cohort
        
    contains
    
        procedure, pass (this) :: getAreaGrowth
        procedure, pass (this) :: setAreaGrowth 
    
    end Type Leaf_type
    
    ! interface to reference the leaf constructor
    interface Leaf_type
        module procedure leaf_type_constructor
    end interface leaf_type
    
    contains
    
    ! constructor for the leaf type
    type (Leaf_type) function leaf_type_constructor()
        implicit none
        !real, intent (in) :: AreaGrowth
        leaf_type_constructor%AreaGrowth_ = 0
    end function leaf_type_constructor
    

    ! get AreaGrowth
    real function getAreaGrowth(this)
        implicit none
        class (Leaf_type), intent(in) :: this
        
        getAreaGrowth = this%AreaGrowth_
    end function getAreaGrowth
    
    
    ! set AreaGrowth    
    subroutine setAreaGrowth(this, AreaGrowth)
        implicit none
        class (Leaf_type), intent(inout) :: this
        real, intent (in) :: AreaGrowth
        
        this%AreaGrowth_ = AreaGrowth
    end subroutine setAreaGrowth
    
    
END Module Leaf_module    
    