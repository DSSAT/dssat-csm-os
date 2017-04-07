Module CS_Model_Environment !Module of environment
    type DailyEnvironment_type
        
        real, private :: HOURS_OF_DAY = 24
        real, private :: HOURS_OF_LIGHT = 8
        real, private :: tMin_ = 0 !
        real, private :: tMax_ = 0 ! 
        real, private :: dewPoint_ = 0 !
        
    contains
    
        procedure, pass (this) :: getTMin
        procedure, pass (this) :: setTMin
        procedure, pass (this) :: getTMax
        procedure, pass (this) :: setTMax
        procedure, pass (this) :: getDewPoint
        procedure, pass (this) :: setDewPoint
        procedure, pass (this) :: getTemperature
        procedure, pass (this) :: getSVP
        procedure, pass (this) :: getWHCAIR
        procedure, pass (this) :: getRH
    
    end Type DailyEnvironment_type
    
    ! interface to reference the constructor
    interface DailyEnvironment_type
        module procedure DailyEnvironment_type_constructor
    end interface DailyEnvironment_type
    
    contains
    
    ! constructor for the type
    type (DailyEnvironment_type) function DailyEnvironment_type_constructor(tMin, tMax, dewPoint)
        implicit none
        real, intent (in) :: tMin, tMax, dewPoint
        DailyEnvironment_type_constructor%TMin_ = tMin
        DailyEnvironment_type_constructor%TMax_ = tMax
        DailyEnvironment_type_constructor%dewPoint_ = dewPoint
    end function DailyEnvironment_type_constructor    
    
    !-------------------------------------------
    ! GETTERS AND SETTERS
    
    ! get TMin
    real function getTMin(this)
        implicit none
        class (DailyEnvironment_type), intent(in) :: this
        
        getTMin = this%tMin_
    end function getTMin
    
    ! set TMin    
    subroutine setTMin(this, tMin)
        implicit none
        class (DailyEnvironment_type), intent(inout) :: this
        real, intent (in) :: tMin
        
        this%tMin_ = tMin
    end subroutine setTMin
    
    ! get tMax
    real function getTMax(this)
        implicit none
        class (DailyEnvironment_type), intent(in) :: this
        
        getTMax = this%tMax_
    end function getTMax
    
    
    ! set tMax    
    subroutine setTMax(this, tMax)
        implicit none
        class (DailyEnvironment_type), intent(inout) :: this
        real, intent (in) :: tMax
        
        this%tMax_ = tMax
    end subroutine setTMax
    
    ! get dewPoint
    real function getDewPoint(this)
        implicit none
        class (DailyEnvironment_type), intent(in) :: this
        
        getDewPoint = this%dewPoint_
    end function getDewPoint
    
    ! set dewPoint    
    subroutine setDewPoint(this, dewPoint)
        implicit none
        class (DailyEnvironment_type), intent(inout) :: this
        real, intent (in) :: dewPoint
        
        this%dewPoint_ = dewPoint
    end subroutine setDewPoint
    
    !-------------------------------------------
    ! OBJECT FUNCTIONS
    
    ! obtain the temperature for any given hour of the day
    ! T(Hour) = Amplitude*sin[w(t - a)] + C.
    ! Amplitude is called the amplitude the height of each peak above the baseline
    ! Hod is Hours Of Day, the period or wavelength (the length of each cycle) 
    ! a  is the phase shift (the horizontal offset of the basepoint; where the curve crosses the baseline as it ascends)
    ! C is average temperature,  the vertical offset (height of the baseline) 
    ! w is the angular frequency, given by w = 2PI/hod 
    real function getTemperature(this, hour)
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

        
        getTemperature = Amplitude*SIN(g)+C           ! calculate temperature acording to the current time

    end function getTemperature
    
    
    !obtain the Saturation Vapour Pressure (pascals) for any given hour
    real function getSVP(this, Hour)
        implicit none
        class (DailyEnvironment_type), intent(in) :: this
        integer, intent (in) :: Hour
        
        getSVP = calculateSVP(getTemperature(this, Hour))                   !  DA Saturation vapour pressure in pascals: svp = 610.78 *exp( t / ( t + 238.3 ) *17.2694 ) 

    end function getSVP
    
    ! obtain the water holding capacity of the air (kg/m3) for any given hour
    real function getWHCAIR(this, hour)
        implicit none
        class (DailyEnvironment_type), intent(in) :: this
        integer, intent (in) :: Hour
        
        getWHCAIR = calculateWHCAIR(getTemperature(this, Hour))                                !  DA water holding capacity of the air WHC = 0.002166 * SVP / ( t + 273.16 )   

    end function getWHCAIR
    
    ! obtain the relative humidity for any given hour
    real function getRH(this, hour)
        implicit none
        class (DailyEnvironment_type), intent(in) :: this
        integer, intent (in) :: Hour
        
        getRH =  calculateRH(getTemperature(this, Hour), this%dewpoint_)                               !  DA water holding capacity of the air WHC = 0.002166 * SVP / ( t + 273.16 )   

    end function getRH
   
    !-------------------------------------------
    ! STATIC FUNCTIONS
    ! obtain the Saturation Vapour Pressure (pascals) for any given temperature
    real function calculateSVP(temperature)
        implicit none
        real, intent (in) :: temperature
        
        calculateSVP = 610.78 * exp( temperature/ ( temperature  + 238.3 ) * 17.2694 )        !  DA Saturation vapour pressure in pascals: svp = 610.78 *exp( t / ( t + 238.3 ) *17.2694 ) 

    end function calculateSVP
    
    ! obtain the water holding capacity of the air (kg/m3) for any given temperature
    real function calculateWHCAIR(temperature)
        implicit none
        real, intent (in) :: temperature
        
        calculateWHCAIR = 0.002166 * calculateSVP(temperature) / ( temperature  + 273.16 )                                !  DA water holding capacity of the air WHC = 0.002166 * SVP / ( t + 273.16 )   

    end function calculateWHCAIR
     
    ! obtain the relative humidity 
    real function calculateRH(temperature, dewPoint)
        implicit none
        real, intent (in) :: temperature, dewPoint
        
        calculateRH =  calculateWHCAIR(temperature)/calculateWHCAIR(dewPoint)                              !  DA water holding capacity of the air WHC = 0.002166 * SVP / ( t + 273.16 )   

    end function calculateRH
    
END Module CS_Model_Environment    
    