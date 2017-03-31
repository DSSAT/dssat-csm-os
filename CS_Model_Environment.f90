Module Environment_module !Module of environment
    type Environment_type
        
        real :: TMin_ = 0 !
        real :: TMax_ = 0 ! 
        integer :: Hour_ = 0 ! 
        
    contains
    
        procedure, pass (this) :: getTMin
        procedure, pass (this) :: setTMin
        procedure, pass (this) :: getTMax
        procedure, pass (this) :: setTMax
        procedure, pass (this) :: getHour
        procedure, pass (this) :: setHour
    
    end Type Environment_type
    
    ! interface to reference the constructor
    interface Environment_type
        module procedure environment_type_constructor
    end interface Environment_type
    
    contains
    
    ! constructor for the type
    type (Environment_type) function environment_type_constructor(TMin, TMax, Hour)
        implicit none
        real, intent (in) :: TMin, TMax
        integer, intent (in) :: Hour
        environment_type_constructor%TMin_ = TMin
        environment_type_constructor%TMax_ = TMax
        environment_type_constructor%Hour_ = Hour
    end function environment_type_constructor    

    ! get TMin
    real function getTMin(this)
        implicit none
        class (Environment_type), intent(in) :: this
        
        getTMin = this%TMin_
    end function getTMin
    
    ! set TMin    
    subroutine setTMin(this, TMin)
        implicit none
        class (Environment_type), intent(inout) :: this
        real, intent (in) :: TMin
        
        this%TMin_ = TMin
    end subroutine setTMin
    
    ! get TMax
    real function getTMax(this)
        implicit none
        class (Environment_type), intent(in) :: this
        
        getTMax = this%TMax_
    end function getTMax
    
    
    ! set TMax    
    subroutine setTMax(this, TMax)
        implicit none
        class (Environment_type), intent(inout) :: this
        real, intent (in) :: TMax
        
        this%TMax_ = TMax
    end subroutine setTMax
    
    ! get Hour
     integer function getHour(this)
         implicit none
         class (Environment_type), intent(in) :: this
         
         getHour = this%Hour_
     end function getHour
          
     ! set Hour    
     subroutine setHour(this, Hour)
         implicit none
         class (Environment_type), intent(inout) :: this
         integer, intent (in) :: Hour
         
         this%Hour_ = Hour
     end subroutine setHour
    
    ! obtain the temperature accrding to the hour of the day
    ! T(t) = Asin[w(t − a)] + C.
    ! Amplitude  is called the amplitude (the height of each peak above the baseline)	
    ! C is the vertical offset (height of the baseline) 
    ! P is the period or wavelength (the length of each cycle) 
    ! w is the angular frequency, given by w = 2PI/P 
    ! a is the phase shift (the horizontal offset of the basepoint; where the curve crosses the baseline as it ascends)
    real function fetchTemperature(this)
        implicit none
        class (Environment_type), intent(in) :: this
        REAL :: Amplitude, C, P, w, a, t, g, pi

        pi= 4 * atan (1.0_8)
        a = 11
        P = 24
        w = (2*pi)/2
        t = 8
        Amplitude = ((this%TMax_ - this%TMin_)/2)
        C = (this%TMin_ + this%TMax_)/2
        t = 12
        g = w*(t-a)
        
        fetchTemperature =  Amplitude*SIN(g)+C
        
        
        
    end function fetchTemperature
    
    ! obtain the Saturation Vapour Pressure (pascals)
    real function fetchSVP(this)
        implicit none
        class (Environment_type), intent(in) :: this
        
        fetchSVP = 610.78 * exp( fetchTemperature(this) / ( fetchTemperature(this)  + 238.3 ) * 17.2694 )        !  DA Saturation vapour pressure in pascals: svp = 610.78 *exp( t / ( t + 238.3 ) *17.2694 ) 

    end function fetchSVP
    
    ! obtain the water holding capacity of the air (kg/m3)
    real function fetchWaterHoldingCapacity(this)
        implicit none
        class (Environment_type), intent(in) :: this
        
        fetchWaterHoldingCapacity = 0.002166 * fetchSVP(this) / ( fetchTemperature(this) + 273.16 )                                !  DA water holding capacity of the air WHC = 0.002166 * SVP / ( t + 273.16 )   

    end function fetchWaterHoldingCapacity

        
END Module Environment_module    
    