Module CS_Model_Environment !Module of environment
    type DailyEnvironment_type
        
        real, private :: HOURS_OF_DAY = 24
        real, private :: HOURS_OF_LIGHT = 8
        real, private :: TMin_ = 0 !
        real, private :: TMax_ = 0 ! 
        
    contains
    
        procedure, pass (this) :: getTMin
        procedure, pass (this) :: setTMin
        procedure, pass (this) :: getTMax
        procedure, pass (this) :: setTMax
        procedure, pass (this) :: fetchTemperature
        procedure, pass (this) :: fetchSVP
        procedure, pass (this) :: fetchWaterHoldingCapacity
    
    end Type DailyEnvironment_type
    
    ! interface to reference the constructor
    interface DailyEnvironment_type
        module procedure DailyEnvironment_type_constructor
    end interface DailyEnvironment_type
    
    contains
    
    ! constructor for the type
    type (DailyEnvironment_type) function DailyEnvironment_type_constructor(TMin, TMax)
        implicit none
        real, intent (in) :: TMin, TMax
        DailyEnvironment_type_constructor%TMin_ = TMin
        DailyEnvironment_type_constructor%TMax_ = TMax
    end function DailyEnvironment_type_constructor    

    ! get TMin
    real function getTMin(this)
        implicit none
        class (DailyEnvironment_type), intent(in) :: this
        
        getTMin = this%TMin_
    end function getTMin
    
    ! set TMin    
    subroutine setTMin(this, TMin)
        implicit none
        class (DailyEnvironment_type), intent(inout) :: this
        real, intent (in) :: TMin
        
        this%TMin_ = TMin
    end subroutine setTMin
    
    ! get TMax
    real function getTMax(this)
        implicit none
        class (DailyEnvironment_type), intent(in) :: this
        
        getTMax = this%TMax_
    end function getTMax
    
    
    ! set TMax    
    subroutine setTMax(this, TMax)
        implicit none
        class (DailyEnvironment_type), intent(inout) :: this
        real, intent (in) :: TMax
        
        this%TMax_ = TMax
    end subroutine setTMax
    
    
    ! obtain the temperature for any given hour of the day
    ! T(Hour) = Amplitude*sin[w(t - a)] + C.
    ! Amplitude is called the amplitude the height of each peak above the baseline
    ! Hod is Hours Of Day, the period or wavelength (the length of each cycle) 
    ! a  is the phase shift (the horizontal offset of the basepoint; where the curve crosses the baseline as it ascends)
    ! C is average temperature,  the vertical offset (height of the baseline) 
    ! w is the angular frequency, given by w = 2PI/hod 
    real function fetchTemperature(this, hour)
        implicit none
        class (DailyEnvironment_type), intent(in) :: this
        integer, intent (in) :: hour
        REAL :: Amplitude, C, w, g, pi, dawnTime

        dawnTime = 5                                            !dawn time
        pi =  4 * atan (1.0_8)
        w = (2*pi)/this%HOURS_OF_DAY
        Amplitude = ((this%TMax_ - this%TMin_)/2)           ! half distance between temperatures
        C = (this%TMin_ + this%TMax_)/2                     ! mean temperature
        g = w*(hour - this%HOURS_OF_LIGHT)

        
        fetchTemperature = Amplitude*SIN(g)+C           ! calculate temperature acording to the current time

    end function fetchTemperature
    
    ! obtain the Saturation Vapour Pressure (pascals) for any given temperature
    real function fetchSVP(this, temperature)
        implicit none
        class (DailyEnvironment_type), intent(in) :: this
        real, intent (in) :: temperature
        
        fetchSVP = 610.78 * exp( temperature/ ( temperature  + 238.3 ) * 17.2694 )        !  DA Saturation vapour pressure in pascals: svp = 610.78 *exp( t / ( t + 238.3 ) *17.2694 ) 

    end function fetchSVP
    
    ! obtain the water holding capacity of the air (kg/m3) for any given temperature and SVP
    real function fetchWaterHoldingCapacity(this, temperature, SVP)
        implicit none
        class (DailyEnvironment_type), intent(in) :: this
        real, intent (in) :: temperature, SVP
        
        fetchWaterHoldingCapacity = 0.002166 * SVP / ( temperature  + 273.16 )                                !  DA water holding capacity of the air WHC = 0.002166 * SVP / ( t + 273.16 )   

    end function fetchWaterHoldingCapacity
    
    ! obtain the incoming radiation at the given hour
    !real function fetchIncomingRadiation(this)
    !    implicit none
    !    class (DailyEnvironment_type), intent(in) :: this
    !    
    !    fetchIncomingRadiation = Radiation * (Tmax_- Tmin_)
    !
    !end function fetchIncomingRadiation

        
END Module CS_Model_Environment    
    