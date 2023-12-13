real function afgen(table,iltab,x)
! ----------------------------------------------------------------------
!     source             : kees rappoldt, 1/86
!     purpose            : linear interpolation in table table with
!                          length iltab for a given value of the
!                          independent variable x.
! ----------------------------------------------------------------------
      implicit none

      integer i,iltab
      real  table(iltab),slope,x
! ----------------------------------------------------------------------
      if (table(1).ge.x)  goto 40
      do 10 i = 3,iltab-1,2
        if (table(i).ge.x) goto 30
        if (table(i).lt.table(i-2)) goto 20
 10   continue
! --- table fully filled, argument larger then last x in table
      afgen = table(iltab)
      return
! --- table partly filled, argument larger then last x in table
 20   afgen = table(i-1)
      return
! --- argument between first and last x in table, interpolation
 30   slope = (table(i+1)-table(i-1))/(table(i)-table(i-2))
      afgen = table(i-1) + (x-table(i-2))*slope
      return
! --- argument less or equal to first x in table
 40   afgen = table(2)
      return
    end
        
real function fgrowth(d,wend,wini,tb,tm,te,t,delta)

    !--- A flexible sigmoid function of determinate growth.
    !--- https://doi.org/10.1093/aob/mcg029

    implicit none
    
    !--- Input parameters
    integer d               ! Derivative type (Integral form [d=0], 1st derivative [d=1]), 1st + CM = 1 [d=2]
    real    wini            ! Min weight at ini time of growth (tb)
    real    wend            ! Max weight at end time of growth (te)
    real    tb              ! Initial time of growth (usually = 0)
    real    tm              ! Time of maximun growth rate
    real    te              ! Time for growth end
    real    t               ! Current time
    real    delta           ! Asymmetry shape factor
    
    !--- Variables
    real    cm              ! Maximum Growth Rate [dw/dt]
    
    !--- Outputs
    real    w               ! Current Biomass in time t
    real    dw              ! Current Growth Rate in time t [dw/dt]
    
    !--- Initialize Variables & Outputs
    cm  = 0.d0
    w   = 0.d0
    dw  = 0.d0
    
    select case(d)
    
    case (0)
        
        !--- Integral Form
        if(t .ge. te)then            
            w   = wend
        elseif(t .le. tb)then
            w   = wini
        else
            w   = wini + (wend - wini) * (1.d0 + (te-t)/(te-tm)) * (t/te)**(te/(te-tm))
        endif
        
        fgrowth = w        
        return
        
    case (1)
        !--- 1st Derivative Form        
        if(t .ge. te .or. t .le. tb)then
            !--- No dev out of te-tb range
            dw  = 0.d0
        else
            cm  = (2.d0 * te - tm) / (te * (te-tm)) * (tm / te) ** (tm/(te-tm)) * (wend - wini)
            dw  = cm * (((te-t)/(te-tm)) * ((t-tb)/(tm-tb))**((tm-tb)/(te-tm))) ** delta
        endif
        
        fgrowth = dw
        
        return
        
    case (2)
        
        !--- 1st Derivative + cm = 1 for relative process rate modeling
        if(t .ge. te .or. t .le. tb)then
            !--- No dev out of te-tb range
            dw  = 0.d0
        else
            cm  = 1.d0
            dw  = cm * (((te-t)/(te-tm)) * ((t-tb)/(tm-tb))**((tm-tb)/(te-tm))) ** delta
        endif
        
        !--- Set minimum and maximum values
        dw  = dw * (wend - wini) + wini 
        
        fgrowth = dw
        
        return
        
    end select    

end
        
real function asy_ws(max_ws, min_ws, t_max_ws, t_mid_ws, t_min_ws, watdmd)   
! ----------------------------------------------------------------------
!     source             : Yu. Asymptote curve to model water stress
! ----------------------------------------------------------------------
      implicit none

      real  max_ws, min_ws, t_max_ws, t_mid_ws, t_min_ws
      real  watdmd
      
! ----------------------------------------------------------------------
      
      if(watdmd .ge. t_min_ws)then
          
          !--- Optimun water conditions
          asy_ws = min_ws
          
      else if(watdmd .le. t_max_ws)then
          
          !--- Extreme drought
          asy_ws = max_ws
            
      else
          
          !--- Transient water stress
          asy_ws = max_ws + (min_ws - max_ws) * (1.d0 + (t_min_ws - watdmd) / (t_min_ws - t_mid_ws)) * (((watdmd - t_max_ws)/(t_min_ws - t_max_ws)) ** ((t_min_ws - t_max_ws) / (t_min_ws - t_mid_ws)))
          
      endif
      
      return
    end
        
real function it_struc_pfac(it_struc_tb_ini,            &   ! Input Parameter
                            it_struc_to1,               &   ! Input Parameter
                            it_struc_to2,               &   ! Input Parameter
                            it_struc_tb_end,            &   ! Input Parameter
                            it_struc_pfac_temp_max_red, &   ! Input Parameter
                            it_struc_pfac_wate_max_red, &   ! Input Parameter
                            it_struc_pfac_max,          &   ! Input Parameter
                            it_struc_pfac_min,          &   ! Input Parameter
                            it_struc_pfac_tb,           &   ! Input Parameter
                            it_struc_pfac_tm,           &   ! Input Parameter
                            it_struc_pfac_te,           &   ! Input Parameter
                            it_struc_pfac_delta,        &   ! Input Parameter
                            it_age,                     &   ! Input Variable
                            t,                          &   ! Input Variable
                            swfacf)                         ! Input Variable
            
    !------------------------------------------------!
    !--- Internode Structural Partitioning Factor ---!
    !------------------------------------------------!
    
    !--- Required Functions:
    !---    temperature_factor()
    !---    fgrowth()    
    
    !--- MSV, Mar-2019
    !------------------------------------------------!
    
    implicit none
    external temperature_factor, fgrowth
    
    !--- Input parameters
    real it_struc_tb_ini    
    real it_struc_to1       
    real it_struc_to2       
    real it_struc_tb_end
    real it_struc_pfac_temp_max_red
    real it_struc_pfac_wate_max_red
                  
    real it_struc_pfac_max  
    real it_struc_pfac_min  
    real it_struc_pfac_tb   
    real it_struc_pfac_tm   
    real it_struc_pfac_te   
    real it_struc_pfac_delta
    
    !--- Input Variables
    real t
    real swfacf
    real it_age
    
    !--- Local variables
    real it_struc_tfac
    real it_struc_wfac
    real it_struc_pfac_red    
    
    !--- Required Functions
    real temperature_factor
    real fgrowth
    
    !--- Relative reduction of biomass partitioning to structural due to non-optimum temperature condition
    it_struc_tfac   = (1.d0 - temperature_factor(   t,				&
                                                    it_struc_tb_ini,&
                                                    it_struc_to1,   &
                                                    it_struc_to2,   &
                                                    it_struc_tb_end))
                                                        
    !--- Absolute reduction of biomass partitioning to structural due to non-optimum temperature condition
    it_struc_tfac   = it_struc_tfac * min(it_struc_pfac_temp_max_red, (it_struc_pfac_max - it_struc_pfac_min))
                                                        
    !--- Relative reduction of biomass partitioning to structural due to non-optimum soil moisture condtition    
    it_struc_wfac   = (1.d0 - swfacf) ! Following doi.org/10.1071/CP10182
    
    !--- Absolute reduction of biomass partitioning to structural due to non-optimum soil moisture condtition
    it_struc_wfac   = it_struc_wfac * min(it_struc_pfac_wate_max_red, (it_struc_pfac_max - it_struc_pfac_min))         
        
    !--- Overall absolute reduction of biomass partitioning to structural tissues due to temperature and soil moisture condition
    it_struc_pfac_red = max(it_struc_tfac, it_struc_wfac)
        
    !--- Partitioning factor to structural tissues considering temperature and soil moisture conditions
    it_struc_pfac   = fgrowth(2,  (it_struc_pfac_max - it_struc_pfac_red),&
                                    it_struc_pfac_min,                    &
                                    it_struc_pfac_tb,                     &
                                    it_struc_pfac_tm,                     &
                                    it_struc_pfac_te,                     &
                                    it_age,                               &
                                    it_struc_pfac_delta)
    
    !--- Check Bounds (0-1)
    if(it_struc_pfac .lt. 0.d0)then
        
        !--- Limit to zero for carbon balance (WRG)
        it_struc_pfac = 0.d0        
        write(*,*) "Structural Partitioning Factor Limited to Zero (0.d0 < it_struc_pfac < 1.0d0)."
        
    else if(it_struc_pfac .gt. 1.d0)then
        
        !--- Limit to unit for carbon balance (WRG)
        it_struc_pfac = 1.d0
        write(*,*) "Structural Partitioning Factor Limited to Unit (0.d0 < it_struc_pfac < 1.0d0)."
        
    endif    
    
    return

    end
    
real function temperature_factor(t,tb_ini,to1,to2,tb_end)
            
    !--------------------------!
    !--- Temperature Factor ---!
    !--------------------------!
    
    !--- Compute The Relative Reduction Factor Due to Temperature Based on cardinal temperatures (trapeze shape)
    
    !--- Input Parameters:
    !---    tb_ini          Minimum temperature for process rate
    !---    to1             Temperature threshold onset of optimun condition 
    !---    to2             Temperature threshold end of optimun condition
    !---    tb_end          Maximum temperature for process rate
    
    !--- MSV, Mar-2019
    !--------------------------!
    
    implicit none
    
    !--- Input parameters
    real    tb_ini
    real    to1
    real    to2
    real    tb_end
    
    !--- Input data
    real    t
    
    !--- Local Variables
    real    t_fac
    
    !--- Compute Temperature Factor    
    if(t .le. tb_ini .or. t .ge. tb_end)then
        
        !--- Out of the Temperature limits range (tb_ini,tb_end) 
        t_fac   = 0.d0
        
    elseif(t .ge. to1 .and. t .le. to2)then
        
        !--- Within optimun Temperature range (to1,to2) 
        t_fac   = 1.d0
        
    elseif(t .gt. tb_ini .and. t .lt. to1)then
        
        !--- Within Lower Meso-Optimun Temperature range (tb_ini, to1) 
        t_fac   = (t - tb_ini) / (to1 - tb_ini)
        
    else
        
        !--- Within Upper Meso-Optimun Temperature range (to2, tb_end)
        t_fac   = 1.d0 - (t - to2) / (tb_end - to2)
        
    endif
    
    !--- return
    temperature_factor = t_fac
    return

    end   
    
real function tiller_senes(dw,nstk,dnstk,tilleragefac,tillerageprof,atln)
    

    Implicit none
    
    integer tl
    integer atln
    real    dw
    real    nstk
    real    dnstk
    real    tilleragefac    
    real    tillerageprof(100,2)

    real    dw_ref_till
    real    virt_dw_ref_till
    real    corr_fac_dead
    real    young_till_dw
    real    dead_biomass
    
    !--- Initalize
    dead_biomass = 0.d0
    
    if((nstk * tilleragefac) .gt. 0.d0 .and. dw .gt. 0.d0)then
    
        !--- Convert to g/m2
        dw      = dw * (1.e6/1.e4)
    
        !--- Dry biomass per reference tiller [g tiller-1]
        dw_ref_till  =   dw / (nstk * tilleragefac)
            
        !--- Biomass for each tiller according to its age
        virt_dw_ref_till = 0.d0
        do tl = 1, atln
            virt_dw_ref_till =   virt_dw_ref_till + tillerageprof(tl,2) * dw_ref_till
        enddo
            
        !--- Correction Factor to meet carbon balance
        corr_fac_dead =  dw / virt_dw_ref_till    
            
        !--- Biomass elegible for die [g m-2] (Youngest Tiller)
        young_till_dw = tillerageprof(atln,2) * dw_ref_till * corr_fac_dead
               
        !--- Dead Biomass [ton ha-1]
        dead_biomass = max(0.d0, young_till_dw * abs(dnstk) * (1.e4 / 1.e6))
        
        !--- Return DW to original unit (FORTRAN has this "side effect" of altering the function input argument)
        dw  =   dw * (1.e4/1.e6)
    
    endif
        tiller_senes    =     dead_biomass
    
    return
    
    end
    

    
