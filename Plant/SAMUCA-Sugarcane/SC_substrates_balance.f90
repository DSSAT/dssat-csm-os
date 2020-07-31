subroutine subs_balance(supply_subs,            &
                        reserves,               &
                        demand_subs,            &
                        demand_subs_mresp,      &
                        demand_subs_gresp,      &
                        demand_subs_dw,         &
                        sup_ratio, supply_rate, &
                        supply_used,            &
                        supply_used_mresp,      &
                        supply_used_gresp,      &
                        supply_used_dw,         &
                        reserves_used_mresp,    &
                        maintenance_factor,     &
                        reduc_growth_factor)
    
    Implicit None
    
    !--- Input Variables
    real    supply_subs    
    real    reserves
    real    demand_subs
    real    demand_subs_mresp
    real    demand_subs_gresp
    real    demand_subs_dw
    
    !--- Local Variables
    real    available_growth
    real    mresp_deficit
    
    !--- Output Variables
    real    sup_ratio
    real    supply_rate
    real    supply_used
    real    supply_used_mresp
    real    supply_used_gresp
    real    supply_used_dw
    real    maintenance_factor
    real    reduc_growth_factor
    real    reserves_used_mresp
    
    save
    
    if(supply_subs .ge. demand_subs)then
        
        !-----------------------------------------!
        !--- Potential Growth Supplies Balance ---!
        !-----------------------------------------!
        
        !--- Daily amount of CH2O assimilation is enough to sustain growth and maintenance
        sup_ratio           = 1.d0
        maintenance_factor  = 1.d0
        supply_rate         = supply_subs - demand_subs ! Surpluss
        supply_used         = demand_subs
        supply_used_mresp   = demand_subs_mresp
        supply_used_gresp   = demand_subs_gresp
        supply_used_dw      = demand_subs_dw
        mresp_deficit       =  0.d0
        reserves_used_mresp =  0.d0
        
    else
        
        !--- Supply cannot provide enough CH2O for full growth
        !--- Supply factor
        if(demand_subs .gt. 0.d0) sup_ratio  = supply_subs / demand_subs
        
        supply_rate = -supply_subs
        
        supply_used = supply_subs ! All supplies were used
        
        if(supply_subs .ge. demand_subs_mresp)then
            
            !---------------------------------------!
            !--- Reduced Growth Supplies Balance ---!
            !---------------------------------------!
            
            !--- Maintenance is priority
            available_growth    =  max(0.d0, supply_subs - demand_subs_mresp)
            supply_used_mresp   =  demand_subs_mresp
            mresp_deficit       =  0.d0
            reserves_used_mresp =  0.d0
            
            !--- Reduced Growth Rate            
            reduc_growth_factor = available_growth / (demand_subs_gresp + demand_subs_dw)
            supply_used_gresp   = demand_subs_gresp * reduc_growth_factor
            supply_used_dw      = demand_subs_dw    * reduc_growth_factor
            
        else
            
            !----------------------------------!
            !--- No Growth Supplies Balance ---!
            !----------------------------------!
            
            !--- Everything used for maintenance
            supply_used_mresp   = supply_subs
            mresp_deficit       = demand_subs_mresp - supply_subs
            
            !--- Check if reserves can meet the remaining deficit of maintenance respiration
            if(reserves .gt. mresp_deficit)then
                
                !--- Reserves can sustain maintenance respiration
                reserves_used_mresp = mresp_deficit
                mresp_deficit       = 0.d0                
                
            else
                !--- Reserves can not sustain maintenance respiration, crop is under risk of starvation.
                reserves_used_mresp = reserves
                mresp_deficit       = mresp_deficit - reserves
                
            endif            
            
            !--- No Growth
            supply_used_gresp   = 0.d0
            supply_used_dw      = 0.d0
            
            !--- 
            if(demand_subs_mresp .le. 0.d0)then                
                !--- No demand for maintenance
                maintenance_factor = 1.d0
            else
                maintenance_factor = (demand_subs_mresp - mresp_deficit) / demand_subs_mresp
            endif
            
        endif
    endif        
    return
        
end subroutine subs_balance