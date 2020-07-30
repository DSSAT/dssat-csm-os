subroutine root_profile(nlay			            , & ! Input
                        nsublay						, & ! Input
                        initcropdepth				, & ! Input
                        rpup						, & ! Input
                        effective_rd				, & ! Input
                        root_front_size				, & ! Input
                        srlmax						, & ! Input
                        srlmin						, & ! Input
                        bottom						, & ! Input
                        slthickness                 , & ! Input
                        ddw_rt                      , & ! Input
                        flemerged					, & ! Input
                        drld						, & ! Output
                        ddw_rt_sl)                      ! Output
    
    Implicit none
    
    integer sl                      ! V
    integer sub_sl                  ! V
    integer nlay                    ! I [#]
    integer nsublay                 ! I [#]
    integer n_above_idepth          ! V
    integer n_below_idepth          ! V 
    integer n_subnodes              ! V    
    real    initcropdepth           ! I [cm]
    real    rpup                    ! I [cm]
    real    effective_rd            ! I [cm]
    real    ddw_rt                  ! I [ton ha-1]
    real    root_front_size         ! P [cm]
    real    srlmax                  ! P [m g-1]
    real    srlmin                  ! P [m g-1]
    real    rootprof(nsublay)       ! V
    real    srl_prof(nsublay)       ! V
    real    ddw_rt_prof(nsublay)    ! V
    real    drld_prof(nsublay)      ! V
    real    drld(nlay)              ! O [cm cm-3]
    real    ddw_rt_sl(nlay)         ! O [g cm-2 layer-1]
    real    bottom(nlay)            ! I [cm]
    real    slthickness(nlay)       ! I [cm]
    real    nrootprof               ! V
    real    pfac_root_cm            ! V
    logical flemerged               ! I
    
    !--- Height of sublayer [cm]
    real :: sublayer_h      =   1.d0
    
    save
    
    !--- Root profile with sublayer of 1 cm resolution (1 = has root, 0 = no roots)
    rootprof        = 0.d0 !Array init
    nrootprof       = 0.d0
    pfac_root_cm    = 0.d0
    n_above_idepth  = 0
    n_below_idepth  = 0   
    do sl = 1, nsublay
        if(sl .le. aint(initcropdepth))then
            if(flemerged)then
                rootprof(sl) = 1.d0
            else
                if((sl - aint(initcropdepth) + aint(rpup)) .gt. 0.d0)then
                    rootprof(sl) = 1.d0
                endif                
            endif            
            !--- Number of sublayer above planting depth
            n_above_idepth  = n_above_idepth + rootprof(sl)
        else
            if(sl .le. aint(effective_rd))then
                rootprof(sl) = 1.d0
            endif
        endif        
        
        !--- Total number of sublayer vertically explored by roots
        nrootprof       = nrootprof + rootprof(sl)
    enddo
    
    do sl = nsublay, aint(initcropdepth+1.d0), -1        
        !--- Number of sublayer below planting depth 
        n_below_idepth  = n_below_idepth + rootprof(sl)
    enddo   
        
    !--- Root Pfac for each sublayer
    if(nrootprof .gt. 0.d0) pfac_root_cm = 1.d0 / nrootprof
    
    !--- Specific Root Lenght for each sublayer above stool point
    srl_prof        = 0.d0
    do sl = n_above_idepth, 1, -1        
        if(sl .lt. root_front_size .and. .not. flemerged)then
            !--- Sublayer at Root Front
            srl_prof(aint(initcropdepth) + 1 - sl) = srlmax
        else
            !--- Sublayer Not at Root Front
            srl_prof(aint(initcropdepth) + 1 - sl) = srlmin
        endif        
    enddo
    
    !--- Specific Root Lenght for each sublayer below stool point
    do sl = n_below_idepth, 1, -1        
        if(sl .le. root_front_size .and. effective_rd .lt. nsublay)then
            !--- Sublayer at Root Front
            srl_prof((aint(initcropdepth) + 1 + n_below_idepth) - sl) = srlmax
        else
            !--- Sublayer Not at Root Front
            srl_prof((aint(initcropdepth) + 1 + n_below_idepth) - sl) = srlmin
        endif        
    enddo
    
    !--- Root Dry Weight Gain by subnode [g m-2]
    ddw_rt_prof = ddw_rt        * pfac_root_cm * rootprof * (1.e6 / 1.e4)
    
    !--- Root Lenght Density Gain by subnode [cm cm-3]
    drld_prof   = ddw_rt_prof   * srl_prof     * (1.e2 / 1.e4) / sublayer_h  
    
    !--- Allocate to soil layers
    sl          = 1
    drld        = 0.d0
    ddw_rt_sl   = 0.d0
    n_subnodes  = 1
    do sub_sl = 1, aint(effective_rd)
        
        !--- Cascade
        if(sub_sl .gt. aint(bottom(sl)))then            
            sl          = sl + 1            
            n_subnodes  = 1 
        endif
        
        !--- Integrate over soil layer
        drld(sl)        = drld(sl)      + drld_prof(sub_sl)     ! [cm layer-1]
        ddw_rt_sl(sl)   = ddw_rt_sl(sl) + ddw_rt_prof(sub_sl)   ! [g cm-2 layer-1]
        
        if(sub_sl .eq. aint(bottom(sl)) .or. sub_sl .eq. aint(effective_rd))then
            !--- Root Length Density [cm cm-3]
            drld(sl)    = drld(sl) / slthickness(sl)
        endif        
        
        n_subnodes = n_subnodes + 1        
    enddo    
    
    return
     
    end subroutine root_profile