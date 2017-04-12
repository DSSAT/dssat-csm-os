!***************************************************************************************************************************
! This module is intended to calculate environmental factors 
! Atributes:
!   - tMin minimmun temprature of the day
!   - tMax maximun temprature of the day
!   - dewPoint dew point temperature of the day
!   - radiation registered during the day
! Object functions:
!        hourlyTemperature
!        hourlySVP
!        hourlyWHCAIR
!        hourlyRH
!        hourlyVPD
!        hourlyRadiation
! Static functions:
!        calculateSVP
!        calculateWHCAIR
!        calculateRH
!        calculateVPD
! Authors
! @danipilze
!*********

    Module CS_Model_Environment !Module of environment
    type DailyEnvironment_type
        
        real, private :: HOURS_OF_DAY = 24
        real, private :: tMin_ = 0 !
        real, private :: tMax_ = 0 ! 
        real, private :: dewPoint_ = 0 !
        real, private :: radiation_ = 0 ! solar radiation
        real, private :: PI =  4 * atan (1.0_8)
        
    contains
    
        procedure, pass (this) :: getTMin
        procedure, pass (this) :: setTMin
        procedure, pass (this) :: getTMax
        procedure, pass (this) :: setTMax
        procedure, pass (this) :: getDewPoint
        procedure, pass (this) :: setDewPoint
        procedure, pass (this) :: getRadiation
        procedure, pass (this) :: setRadiation
        procedure, pass (this) :: hourlyTemperature
        procedure, pass (this) :: hourlySVP
        procedure, pass (this) :: hourlyWHCAIR
        procedure, pass (this) :: hourlyRH
        procedure, pass (this) :: hourlyVPD
        procedure, pass (this) :: hourlyRadiation
    
    end Type DailyEnvironment_type
    
    ! interface to reference the constructor
    interface DailyEnvironment_type
        module procedure DailyEnvironment_type_constructor
    end interface DailyEnvironment_type
    
    contains
    
    ! constructor for the type
    type (DailyEnvironment_type) function DailyEnvironment_type_constructor(tMin, tMax, dewPoint, radiation)
        implicit none
        real, intent (in) :: tMin, tMax, dewPoint,radiation
        DailyEnvironment_type_constructor%TMin_ = tMin
        DailyEnvironment_type_constructor%TMax_ = tMax
        DailyEnvironment_type_constructor%dewPoint_ = dewPoint
        DailyEnvironment_type_constructor%radiation_ = radiation
    end function DailyEnvironment_type_constructor    
    
    
    !-------------------------------------------
    ! OBJECT FUNCTIONS
    !-------------------------------------------
    
    
    ! obtain the temperature for any given hour of the day
    ! T(Hour) = Amplitude*sin[w(t - a)] + C.
    ! Amplitude is called the amplitude the height of each peak above the baseline
    ! Hod is Hours Of Day, the period or wavelength (the length of each cycle) 
    ! a  is the phase shift (the horizontal offset of the basepoint; where the curve crosses the baseline as it ascends)
    ! C is average temperature,  the vertical offset (height of the baseline) 
    ! w is the angular frequency, given by w = 2PI/hod 
    real function hourlyTemperature(this, hour)
        implicit none
        class (DailyEnvironment_type), intent(in) :: this
        integer, intent (in) :: hour
        real :: Amplitude, C, w, g,  dawnTime

        dawnTime = 5                                            !dawn time
        w = (2*this%PI)/this%HOURS_OF_DAY
        Amplitude = ((this%TMax_ - this%TMin_)/2)           ! half distance between temperatures
        C = (this%TMin_ + this%TMax_)/2                     ! mean temperature
        g = w*(hour - 8)

        
        hourlyTemperature = Amplitude*SIN(g)+C           ! calculate temperature acording to the current time

    end function hourlyTemperature
    
    
    !obtain the Saturation Vapour Pressure (pascals) for any given hour
    real function hourlySVP(this, Hour)
        implicit none
        class (DailyEnvironment_type), intent(in) :: this
        integer, intent (in) :: hour
        
        hourlySVP = calculateSVP(hourlyTemperature(this, Hour))                   !  DA Saturation vapour pressure in pascals: svp = 610.78 *exp( t / ( t + 238.3 ) *17.2694 ) 

    end function hourlySVP
    
    ! obtain the water holding capacity of the air (kg/m3) for any given hour
    real function hourlyWHCAIR(this, hour)
        implicit none
        class (DailyEnvironment_type), intent(in) :: this
        integer, intent (in) :: hour
        
        hourlyWHCAIR = calculateWHCAIR(hourlyTemperature(this, Hour))                                !  DA water holding capacity of the air WHC = 0.002166 * SVP / ( t + 273.16 )   

    end function hourlyWHCAIR
    
    ! obtain the relative humidity for any given hour
    real function hourlyRH(this, hour)
        implicit none
        class (DailyEnvironment_type), intent(in) :: this
        integer, intent (in) :: hour
        
        hourlyRH =  calculateRH(hourlyTemperature(this, Hour), this%dewpoint_)                               !  DA water holding capacity of the air WHC = 0.002166 * SVP / ( t + 273.16 )   

    end function hourlyRH
    
    ! obtain the Vapor Preasure Deficit VPD for any given hour
    real function hourlyVPD(this, hour)
        implicit none
        class (DailyEnvironment_type), intent(in) :: this
        integer, intent (in) :: hour
        
        hourlyVPD =  calculateVPD(hourlyTemperature(this, Hour), this%dewpoint_)                               !  DA water holding capacity of the air WHC = 0.002166 * SVP / ( t + 273.16 )   

    end function hourlyVPD
    
    ! obtain the radiation VPD for any given hour
    real function hourlyRadiation(this, hour)
        implicit none
        class (DailyEnvironment_type), intent(in) :: this
        integer, intent (in) :: hour
        real :: Amplitude, C, w, g, dawnTime, lightHours
               
        lightHours = 12
        dawnTime = 5                                            !dawn time
        w = (2*this%PI)/24   
        C = 0   
        Amplitude = (this%radiation_* this%PI)/24
        g = w*(hour - dawnTime)
        
        hourlyRadiation = Amplitude*SIN(g)+C           ! hourly radiation
        if(hourlyRadiation < 0.0 ) hourlyRadiation = 0 ! no radiation on non-light hours
        
        
    end function hourlyRadiation
   
    !-------------------------------------------
    ! STATIC FUNCTIONS
    !-------------------------------------------
    
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
        
        ! WHCAIR = 0.002166 *SVP / ( t + 273.16 )   
        calculateWHCAIR = 0.002166 * calculateSVP(temperature) / ( temperature  + 273.16 )                                !  DA water holding capacity of the air WHC = 0.002166 * SVP / ( t + 273.16 )   

    end function calculateWHCAIR
     
    ! obtain the relative humidity 
    real function calculateRH(temperature, dewPoint)
        implicit none
        real, intent (in) :: temperature, dewPoint
        
        !(RHt) = WHCAIRdp/WHCAIRt
        calculateRH =  calculateWHCAIR(dewPoint)/calculateWHCAIR(temperature)                              !  DA water holding capacity of the air WHC = 0.002166 * SVP / ( t + 273.16 )   

    end function calculateRH
    
    ! obtain the Vapor Preasure Deficit VPD (KPa)
    real function calculateVPD(temperature, dewPoint)
        implicit none
        real, intent (in) :: temperature, dewPoint
        
        ! VPD = (1 - (RH/100)) * SVP 
        calculateVPD =  ((1 - (calculateRH(temperature, dewPoint)/100)) * calculateSVP(temperature))/1000                               !  DA water holding capacity of the air WHC = 0.002166 * SVP / ( t + 273.16 )   

    end function calculateVPD
    
    ! calculates proportion of radiation
    ! K is the extinction coefficient
    ! LAI is leaf area index
    real function calculateRadiationP(K, LAI)
        implicit none
        real, intent (in) :: K, LAI
        real :: value = 0
        
        calculateRadiationP = 1 - exp(- K*LAI)

    end function calculateRadiationP
    
    !-------------------------------------------
    ! GETTERS AND SETTERS
    !------------------------------------------
    
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
    
    ! get radiation
    real function getRadiation(this)
        implicit none
        class (DailyEnvironment_type), intent(in) :: this
        
        getRadiation = this%radiation_
    end function getRadiation
    
    ! set radiation    
    subroutine setRadiation(this, radiation)
        implicit none
        class (DailyEnvironment_type), intent(inout) :: this
        real, intent (in) :: radiation
        
        this%radiation_ = radiation
    end subroutine setRadiation
    
END Module CS_Model_Environment    
    