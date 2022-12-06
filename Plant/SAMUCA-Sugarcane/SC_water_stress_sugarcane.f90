subroutine waterstress(method_ws,     &
                        ndws,         &
                        ndews,        &
                        eop,          &
                        trwup,        &
                        rwuep1,       &
                        rwuep2 ,      &
                        t_max_ws_pho, &
                        t_mid_ws_pho, &
                        t_min_ws_pho, &   
                        t_max_ws_exp, &
                        t_mid_ws_exp, &
                        t_min_ws_exp, &
                        t_max_ws_til, &
                        t_mid_ws_til, &
                        t_min_ws_til, &
                        t_max_ws_fpf, &
                        t_mid_ws_fpf, &
                        t_min_ws_fpf, &
                        threshews,    &
                        swfacp,       &
                        swface,       &
                        swfact,       &
                        swfacf)
    
    !----------------------------------------------!
    !--- Soil Water Stress factor for Sugarcane ---!
    !----------------------------------------------!
    
    Implicit none
    
    integer method_ws
    integer ndws
    integer ndews
    real    eop
    real    trwup
    real    rwuep1
    real    rwuep2    
    real    t_max_ws_pho
    real    t_mid_ws_pho
    real    t_min_ws_pho    
    real    t_max_ws_exp
    real    t_mid_ws_exp
    real    t_min_ws_exp
    real    t_max_ws_til
    real    t_mid_ws_til
    real    t_min_ws_til    
    real    t_max_ws_fpf
    real    t_mid_ws_fpf
    real    t_min_ws_fpf    
    real    threshews
    
    real    swfacp  ! Photosynthesis Water Stress Factor [0-1]
    real    swface  ! Expansion Water Stress Factor [0-1]
    real    swfact  ! Tillering Water Stress Factor [0-1]
    real    swfacf  ! Fiber partitioning factor Water Stress Factor [0-1]
    
    real :: max_ws = 0.d0
    real :: min_ws = 1.d0
    
    real asy_ws     ! Function    
    real watdmd     ! Water demand ratio (source/sink)
    
    save
    
    !--- Initiate
    swfacp      = 1.d0
    swface      = 1.d0 
    swfact      = 1.d0
    swfacf      = 1.d0
    
    !--- water stress factors in photosynthesis and expansion
        if (eop .le. 0.d0) then
        watdmd = 0.
        swfacp = 1.d0
        swface = 1.d0
        swfact = 1.d0
        swfacf = 1.d0
        else
          
            !--- Water deficit (supply/demand)
            watdmd = max(trwup/(eop/10.),0.d0)          
          
            if(method_ws .eq. 1)then
              
                !--- Linear response to water depletion              
                if(watdmd .lt. rwuep1) then
                    swfacp = max(0.,min((1./rwuep1) * watdmd,1.))          
                endif
          
                if(watdmd .lt. rwuep2) then
                    swface = max(0.,min((1./rwuep2) * watdmd,1.))
                endif              
              
            else if(method_ws .eq. 2)then
              
                !--- Asymptote response to water depletion            
                swfacp = asy_ws(max_ws, min_ws, t_max_ws_pho, t_mid_ws_pho, t_min_ws_pho, watdmd)
                swface = asy_ws(max_ws, min_ws, t_max_ws_exp, t_mid_ws_exp, t_min_ws_exp, watdmd)
                swfact = asy_ws(max_ws, min_ws, t_max_ws_til, t_mid_ws_til, t_min_ws_til, watdmd)
                swfacf = asy_ws(max_ws, min_ws, t_max_ws_fpf, t_mid_ws_fpf, t_min_ws_fpf, watdmd)
              
            endif          
        endif
      
        !--- Consecutive days of water stress counter
        if(swfacp .lt. 1.) then
            ndws = ndws + 1
        else
            ndws = 0
        endif
      
        !--- Consecutive days of extreme water stress counter (threshews is the threshold to account for extreme stress)
        if(watdmd .lt. threshews) then
            ndews = ndews + 1
        else
            ndews = 0
        endif
      
    return
    
end subroutine waterstress