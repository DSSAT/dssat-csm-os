!This subroutine not used? chp 2023-01-16
!subroutine root_len_dens(nlay, srlMax, srlMin, effective_rd, initcropdepth, rpup, ddw_rt, rgf, flemerged, dw_rt_prof, slthickness, drld)
subroutine root_len_dens(nlay, srlMax, srlMin, effective_rd, initcropdepth, rpup, ddw_rt, rgf, dw_rt_prof, slthickness, drld)
    
    Implicit none
    
!   logical flemerged
    integer sl
    integer nlay
    
    real    srl
    real    srlMax
    real    srlMin
    
    real    effective_rd
    real    initcropdepth
    real    rpup
    real    ddw_rt
    
    real    rgf(nlay+1,3)
    real    dw_rt_prof(nlay)
    real    slthickness(nlay)
    real    drld(nlay)
    
    save
    
    
    ! The amount of daily allocated root biomass is equaliy distributed among compartments
    ! Root depth front will use a higher SRL according to findings of Laclau & Laclau (2009) - Fig8
    
    !Note: The reference depth is the planting depth or ratoon initial depth, therefore,
    !roots are able to explore the soil profile downward and upward from this reference point;
    !The upward extension is set to stop when crop emerges and because the rate of 
    !root extension (0.048 cm dd-1 [Laclau & Laclau, 2009]) is lower than shoot extension 
    !towards soil surface (0.08 cm dd-1[Keating, 2009]) this avoid overestimates of RLD in the
    !surface layer which may bias the water stress effect! (following Klaas Metselaar suggestion)
        
    !Further improvements: Strong evidences that SRL is variable with soil structure, specially
    !over different soil operations (plowing, tillage, direct planting) -> See Azevedo et al(2011) [DOI: 10.1590/S0103-90162011000100014]
    !Would SRL be directly related to soil macro pores?
    !For improvements insights check: http://www.csc.univie.ac.at/rootbox/
        
    !RLD Referece values final season (1st layer):
    !Laclau & Laclau 2009: 0.45         [cm cm-3] 
    !Azevedo et al(2011) : 0.45 - 1.1   [cm cm-3] (DOI: 10.1590/S0103-90162011000100014)
    dw_rt_prof = 0.d0    
    do sl = 1, nlay         
        if((sl .eq. nlay .and. rgf(sl,2) .gt. 0.d0) .or. (rgf(sl+1,2) .le. 0.d0 .and. rgf(sl,2) .gt. 0.d0))then
            srl = srlMax
            rgf(sl,1)       = (rgf(sl,2) * slthickness(sl))         / ((effective_rd - initcropdepth) + rpup)
            dw_rt_prof(sl)  = rgf(sl,1)  * (ddw_rt * (1.e6/1.e4))   / (slthickness(sl) / 100.) ! g m-3
            drld(sl)        = dw_rt_prof(sl)  * srl ! m(roots) m-3(soil)
            drld(sl)        = drld(sl)      * 100./1.e6 !cm(roots) cm-3(soil)
            
        elseif(rgf(sl,2) .gt. 0.d0)then
            srl = srlMin
            rgf(sl,1)       = (rgf(sl,2) * slthickness(sl)) / ((effective_rd - initcropdepth) + rpup)
            dw_rt_prof(sl)  = rgf(sl,1)  * (ddw_rt * (1.e6/1.e4)) / (slthickness(sl) / 100.) ! g m-3
            drld(sl)        = dw_rt_prof(sl) * srl ! m(roots) m-3(soil)
            drld(sl)        = drld(sl) * 100./1.e6 !cm(roots) cm-3(soil)
                
        else
            !no roots
            srl             = 0.d0 
            rgf(sl,1)       = 0.d0
            dw_rt_prof(sl)  = 0.d0
            drld(sl)        = 0.d0                
        endif
    enddo
    
    return
     
    end subroutine root_len_dens