subroutine TempHour(tmaxday,tminday,doy,lat,a,b,c,thour)
!Calculates the Hourly temperature based on Parton & Logan (1981) model
    
	Implicit None	
	
    integer hour
    integer doy
	real tsunset                               !Temperature related variables !oC        
	real decsol                                ! astronomic variables
	real ahn                                   ! astronomic variables
	real timnight                              ! time related variables
	real timday                                ! time related variables
	real sunset
	real sunrise
	real photop
	real nigthp
    real bb
    real be
    real bbd_iday
    real bbd_inight
    real bbd_inight2
    real bbe
    real ddy
    real tmaxday
    real tminday
    real lat
    real d_2_r
    real r_2_d
    
    real a          !       = 1.607 Calibrated for Sao Paulo State   (original constants from Parton and Logan paper = 2.000)
	real b          !       = 2.762 Calibrated for Sao Paulo State   (original constants from Parton and Logan paper = 2.200)
	real c          !       = 1.179 Calibrated for Sao Paulo State   (original constants from Parton and Logan paper = -0.17)	
    
    real thour(24)
    
    real :: pi      =  3.14159265
    
    save
    
    d_2_r = pi/180.
    r_2_d = 180./pi

	!calculating photoperiod			
    decsol  = 23.45 * sin(((360./365.)*(doy-80.)*d_2_r))
    
	!ahn     = acos((-tan(d_2_r*lat)*tan(d_2_r*decsol)))
	photop  = acos((-tan((lat)*d_2_r))*(tan((decsol)*d_2_r))) * r_2_d * (2./15.)
	nigthp  = 24 - photop
	sunrise = 12 - photop/2
	sunset  = 12 + photop/2
		
    bb      = 12. - photop / 2. + c
    be      = 12. + photop / 2.
    ddy     = photop - c
        
    !Calculating air temperature follow Parton & Logan (1981)				
    tsunset = (tmaxday - tminday) * sin(((pi*ddy)/(photop+2*a))) + tminday
        
	!Initial Conditions
	do hour = 1,24
			    
            bbd_iday    = hour - bb
            bbe         = hour - be
            bbd_inight2 = (24. + be) + hour
                
            !Rescaling time
            if(hour .gt. sunset) then
                bbd_inight  = hour - sunset
            else
                bbd_inight = (24. + hour) - sunset
            endif
                
            !Day time temperature
            if(hour .ge. bb .and. hour .le. sunset) then
                
                thour(hour) = (tmaxday - tminday) * sin(((pi * bbd_iday) / (photop + 2*a))) + tminday
                    
            else
                !Night time temperature
                    
                thour(hour) = tminday + (tsunset - tminday) * exp(-b * bbd_inight/(24. - photop))
                    
                    
            endif						
	enddo
		
	return
end subroutine TempHour

