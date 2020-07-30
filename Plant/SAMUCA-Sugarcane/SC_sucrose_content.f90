subroutine sucrose_content( dw_it,              &   ! Input Variable
                            ts_it,              &   ! Input Variable
                            suc_min,            &   ! Input Parameter
                            hex_min,            &   ! Input Parameter
                            suc_acc_ini,        &   ! Input Parameter
                            suc_frac_rate_ts,   &   ! Input Parameter
                            suc_it,             &   ! Output Variable
                            hex_it)                 ! Output Variable
    
    Implicit None
    
    !--- Input Variables/Parameters
    real dw_it
    real ts_it
    real suc_min
    real hex_min
    real suc_acc_ini
    real suc_frac_rate_ts
    real avail_frac_sucrose
    
    !--- Local Variables
    real frac_ts
    real frac_suc
    real frac_hex
    
    !--- Output Variables
    real suc_it
    real hex_it
    
    save
    
    !--- Fraction of internode total sugars
    if(dw_it .gt. 0.d0)then
        frac_ts     =   ts_it / dw_it
    else
        !--- Internode without any biomass yet
        frac_ts     =   0.d0
        suc_it      =   0.d0
        hex_it      =   0.d0
        return
    endif
    
    !--- Available fraction for sucrose
    avail_frac_sucrose  =   max(0.d0, (frac_ts - hex_min))    
       
    !--- Fraction of Internode Sucrose
    if(frac_ts .le. suc_acc_ini)then
        !--- Sugar Fraction of Internode is not at start point for sucrose accumulation        
        frac_suc    =   min(suc_min, avail_frac_sucrose)  
    else
        !--- Sugar Fraction of Internode is enough to accumulate sucrose        
        frac_suc    = suc_min + (frac_ts - suc_acc_ini) * suc_frac_rate_ts
        
        !--- Do not let sucrose be greater than total sugars and minimum hexose content
        frac_suc    = min(frac_suc, avail_frac_sucrose)
    endif
            
    !--- Fraction of Internode Hexoses
    frac_hex = frac_ts - frac_suc
    
    !--- Sucrose and Hexoses Content in the Internode [g]
    suc_it  =   dw_it   *   frac_suc
    hex_it  =   dw_it   *   frac_hex
        
    return
        
end subroutine sucrose_content