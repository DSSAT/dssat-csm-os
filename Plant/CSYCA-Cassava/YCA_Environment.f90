!***************************************************************************************************************************
! This module is intended to calculate environmental factors 
! 12/09/2017 converted from UTF-8 to ANSI
! Atributes:
!   - tMin minimmun temprature of the day
!   - tMax maximun temprature of the day
!   - dewPoint dew point temperature of the day
!   - dayRadiation registered during the day
! Type functions:
!        hourlyTemperature
!        hourlySVP
!        hourlyWHCAIR
!        hourlyRH
!        hourlyVPD
!        hourlyRadiation
!        hourlyBiomass
!        hourlyTranspiration
! Static functions:
!        calculateSVP
!        calculateWHCAIR
!        calculateRH
!        calculateVPD
!        calculatePortionOfRadiation
!        calculatateHourlyRadiation
!        calculateBiomass
!        calculateTranspiration
! Authors
! @danipilze
!*********

    Module YCA_Environment         !Module of environment
    
    ! STATIC ATRIBUTES
    real, private :: PI =  4 * atan (1.0_8)
    real, private :: HOURS_OF_DAY = 24
    real, private :: LIGHT_HOURS = 12
    real, private :: DAWN_TIME = 5                                              !dawn time
    
    type DailyEnvironment_type
        
        ! OBJECT ATRIBUTES
        
        real, private :: tMin_ = 0
        real, private :: tMax_ = 0
        real, private :: dewPoint_ = 0
        real, private :: dayRadiation_ = 0                                      ! solar radiation
        
        
    contains
    
        ! type functions
        procedure, pass (this) :: hourlyTemperature
        procedure, pass (this) :: hourlySVP
        procedure, pass (this) :: hourlyWHCAIR
        procedure, pass (this) :: hourlyRH
        procedure, pass (this) :: hourlyVPD
        procedure, pass (this) :: hourlyRadiation
        procedure, pass (this) :: hourlyBiomass
        procedure, pass (this) :: hourlyTranspiration
        
        ! getters and setters
        procedure, pass (this) :: getTMin
        procedure, pass (this) :: setTMin
        procedure, pass (this) :: getTMax
        procedure, pass (this) :: setTMax
        procedure, pass (this) :: getDewPoint
        procedure, pass (this) :: setDewPoint
        procedure, pass (this) :: getDayRadiation
        procedure, pass (this) :: setDayRadiation
        
    
    end Type DailyEnvironment_type
    
    ! interface to reference the constructor
    interface DailyEnvironment_type
        module procedure DailyEnvironment_type_constructor
    end interface DailyEnvironment_type
    
    contains
    
    ! constructor for the type
    ! tMin          mandatory
    ! tMax          mandatory 
    ! dewPoint
    ! dayRadiation
    type (DailyEnvironment_type) function DailyEnvironment_type_constructor(tMin, tMax, dewPoint, dayRadiation)
        implicit none
        real, intent (in) :: tMin, tMax, dewPoint,dayRadiation
        DailyEnvironment_type_constructor%TMin_ = tMin
        DailyEnvironment_type_constructor%TMax_ = tMax
        DailyEnvironment_type_constructor%dewPoint_ = dewPoint
        DailyEnvironment_type_constructor%dayRadiation_ = dayRadiation
    end function DailyEnvironment_type_constructor    
    
    
    !-------------------------------------------
    ! TYPE FUNCTIONS
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
        real :: Amplitude, C, w, g

        w = (2*PI)/HOURS_OF_DAY
        Amplitude = ((this%TMax_ - this%TMin_)/2)                                               ! half distance between temperatures
        C = (this%TMin_ + this%TMax_)/2                                                         ! mean temperature
        g = w*(hour - 8)

        
        hourlyTemperature = Amplitude*SIN(g)+C                                                  ! calculate temperature acording to the current time

    end function hourlyTemperature
    
    
    !obtain the Saturation Vapour Pressure for any given hour                                   ! (pascals)
    real function hourlySVP(this, Hour)  
        implicit none
        class (DailyEnvironment_type), intent(in) :: this
        integer, intent (in) :: hour
        
        hourlySVP = calculateSVP(hourlyTemperature(this, Hour))

    end function hourlySVP
    
    ! obtain the water holding capacity of the air for any given hour
    real function hourlyWHCAIR(this, hour) ! (kg/m3)
        implicit none
        class (DailyEnvironment_type), intent(in) :: this
        integer, intent (in) :: hour
        
        hourlyWHCAIR = calculateWHCAIR(hourlyTemperature(this, Hour))

    end function hourlyWHCAIR
    
    ! obtain the relative humidity for any given hour
    real function hourlyRH(this, hour) 
        implicit none
        class (DailyEnvironment_type), intent(in) :: this
        integer, intent (in) :: hour
        
        hourlyRH =  calculateRH(hourlyTemperature(this, Hour), this%dewpoint_)

    end function hourlyRH
    
    ! obtain the Vapor Preasure Deficit VPD for any given hour
    real function hourlyVPD(this, hour)
        implicit none
        class (DailyEnvironment_type), intent(in) :: this
        integer, intent (in) :: hour
        
        hourlyVPD =  calculateVPD(hourlyTemperature(this, Hour), this%dewpoint_)

    end function hourlyVPD
    
    ! obtain the radiation VPD for any given hour
    real function hourlyRadiation(this, hour)
        implicit none
        class (DailyEnvironment_type), intent(in) :: this
        integer, intent (in) :: hour
              
                
        hourlyRadiation = calculatateHourlyRadiation(hour, DAWN_TIME, this%dayRadiation_ , LIGHT_HOURS) 
        
    end function hourlyRadiation
    
    ! obtain the biomass produced at a given hour
    real function hourlyBiomass(this, hour, extinctionCoefficient,LAI, radiationUseEfficiency,stomatalConductance)
        implicit none
        class (DailyEnvironment_type), intent(in) :: this
        integer, intent (in) :: hour
        real, intent (in) :: extinctionCoefficient, LAI,radiationUseEfficiency,stomatalConductance
               
        hourlyBiomass = calculateBiomass(calculatateHourlyRadiation(hour, DAWN_TIME, this%dayRadiation_ , LIGHT_HOURS) , extinctionCoefficient , LAI, radiationUseEfficiency, stomatalConductance )
        
    end function hourlyBiomass
    
    ! obtain the transpiration
    real function hourlyTranspiration(this, hour, stomatalConductance)
        implicit none
        class (DailyEnvironment_type), intent(in) :: this
        integer, intent (in) :: hour
        real, intent (in) :: stomatalConductance
        
        hourlyTranspiration = calculateTranspiration(hourlyVPD(this, hour), stomatalConductance)

    end function hourlyTranspiration
   
    !-------------------------------------------
    ! STATIC FUNCTIONS
    !-------------------------------------------
    
    ! obtain the Saturation Vapour Pressure  for any given temperature                          ! (pascals)
    real function calculateSVP(temperature) 
        implicit none
        real, intent (in) :: temperature
        
        !Saturation vapour pressure in pascals: svp = 610.78 *exp( t / ( t + 238.3 ) *17.2694 )
        calculateSVP = 610.78 * exp( temperature/ ( temperature  + 238.3 ) * 17.2694 )

    end function calculateSVP
    
    ! obtain the water holding capacity of the air for any given temperature                    ! (kg/m3)
    real function calculateWHCAIR(temperature) 
        implicit none
        real, intent (in) :: temperature
        
        ! WHCAIR = 0.002166 *SVP / ( t + 273.16 )
        calculateWHCAIR = 0.002166 * calculateSVP(temperature) / ( temperature  + 273.16 )  

    end function calculateWHCAIR
     
    ! obtain the relative humidity                                                              ! % 
    real function calculateRH(temperature, dewPoint)
        implicit none
        real, intent (in) :: temperature, dewPoint
        
        !(RHt) = WHCAIRdp/WHCAIRt
        calculateRH =  calculateWHCAIR(dewPoint)/calculateWHCAIR(temperature)

    end function calculateRH
    
    ! obtain the Vapor Preasure Deficit VPD                                                     ! (KPa)
    real function calculateVPD(temperature, dewPoint)
        implicit none
        real, intent (in) :: temperature, dewPoint
        
        ! VPD = (1 - (RH/100)) * SVP
        calculateVPD =  ((1 - (calculateRH(temperature, dewPoint)/100)) * calculateSVP(temperature))/1000

    end function calculateVPD
    
    ! calculates proportion of radiation
    ! K is the extinction coefficient
    ! LAI is leaf area index
    real function calculatePortionOfRadiation(K, LAI)                                           ! fraction
        implicit none
        real, intent (in) :: K, LAI
        real :: value = 0
        
        calculatePortionOfRadiation = 1.0 - exp(-K*LAI)

    end function calculatePortionOfRadiation
    
    ! obtain the radiation at a given hour
    real function calculatateHourlyRadiation(hour, dawnTime, dayRadiation, lightHours)          !MJ/m2
        implicit none
        integer, intent (in) :: hour
        real :: Amplitude, C, w, g, value, dawnTime, dayRadiation, lightHours
               
        w = (2*PI)/24   
        C = 0   
        Amplitude = (dayRadiation * PI)/24
        g = w*(hour - dawnTime)
        
        value = Amplitude*SIN(g)+C           ! calculate hourly radiation
        if(value < 0.0 ) value = 0           ! no radiation on non-light hours
        
        calculatateHourlyRadiation=value
        
    end function calculatateHourlyRadiation
    
    ! obtain the biomass produced accoording to the radiation at a given moment             ! MJ/m2 * # * # * g/MJ * fraction = g/m2
    real function calculateBiomass(radiation, extinctionCoefficient , LAI, radiationUseEfficiency, stomatalConductance)
        implicit none
        real, intent (in) :: radiation, extinctionCoefficient, LAI, radiationUseEfficiency, stomatalConductance
        real :: value = 0
        
        calculateBiomass = calculatePortionOfRadiation(extinctionCoefficient, LAI) * radiation * radiationUseEfficiency  * stomatalConductance

    end function calculateBiomass
    
    ! obtain the transpiration                                                              
    real function calculateTranspiration(VPD, stomatalConductance)
        implicit none
        real, intent (in) :: stomatalConductance, VPD
        
        calculateTranspiration = stomatalConductance * VPD

    end function calculateTranspiration
    
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
    
    ! get day radiation
    real function getDayRadiation(this)
        implicit none
        class (DailyEnvironment_type), intent(in) :: this
        
        getDayRadiation = this%dayRadiation_
    end function getDayRadiation
    
    ! set day radiation    
    subroutine setDayRadiation(this, dayRadiation)
        implicit none
        class (DailyEnvironment_type), intent(inout) :: this
        real, intent (in) :: dayRadiation
        
        this%dayRadiation_ = dayRadiation
    end subroutine setDayRadiation
    
END Module YCA_Environment
    