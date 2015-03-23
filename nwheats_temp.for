! JZW note: need to read p_root_n_min, p_init_grain_nconc
!The following is line 2264 in *tmp
*     ===========================================================
      subroutine nwheats_lyrck (layer)	   
*     ===========================================================
      implicit none
      !*! include 'const.inc'              ! err_user
      !*! include 'nwheats.inc'             ! CERES_Wheat Common Block
      !*! include 'data.pub'                          
      !*! include 'error.pub'                         

*+  Sub-Program Arguments	******OK*******
      integer    layer                 ! (INPUT) layer counter

*+  Purpose
*       checks that layer layer lies in range of 1 - nlayr

*+  Notes
*             reports error if layer < minlyr
*             or layer > nlayr

*+  Changes
*       180789 specified and programmed (jngh)
*       221191 jngh expanded messages, l to layer and removed unused
*              common blocks. Sprofl and swater - cr33
*                   parameterised minlyr - cr32
*       270592 jngh moved count to global section
*                   declared count in external calls - cr302
*                   moved nlayr to internal section - cr302
*                   nlayr changed to come from function and
*                   no longer from arguments.  Included sprofl
*                   common block and mxlayr.  Also used count.

*+  Constant Values
      integer    minlyr                ! lowest value for a layer number
      parameter (minlyr = 1)
      character  my_name*(*)    ! name of procedure
      parameter (my_name  = 'cw_layrck')

*+  Local Variables	 ******OK*******
      character  e_messg*200           ! error message
      integer    nlayr                 ! max layers

*- Implementation Section ----------------------------------
      !*! call push_routine(my_name)
 
      nlayr = count_of_real_vals (dlayr, mxlayr)
 
      if (layer.lt.minlyr) then
         write (e_messg,'(2(a,i3))')
     :                         ' Soil layer no. ', layer,
     :                         ' is below mimimum of ', minlyr
         call warning_error (err_user, e_messg)
 
      else if (layer.gt.nlayr) then
         write (e_messg,'(2(a,i3))')
     :                         ' Soil layer no. ', layer,
     :                         ' is above maximum of ', nlayr
         call warning_error (err_user, e_messg)
 
      endif
      !*! call pop_routine(my_name)
 
      return
      end



*     ===========================================================
      subroutine nwheats_ndmd (ndmd)	      *****OK*****
*     ===========================================================
      implicit none
      !*! include 'nwheats.inc'             ! CERES_Wheat Common Block
      !*! include 'data.pub'                          
      !*! include 'error.pub'                         

*+  Sub-Program Arguments    *****OK*****
      real       ndmd (*)              ! (OUTPUT) plant nitrogen demand
                                       !   (g/plant part)

*+  Purpose
*       return plant nitrogen demand for each plant component

*+  Notes
*           nitrogen required for grain growth has already been removed
*           from the stover.  Thus the total N demand is the sum of the
*           demands of the stover and roots.  Stover N demand consists of
*           two components: firstly, the demand due to the difference between
*           the actual N concentration and the critical N concentration
*           of the tops (stover), which can be positive or negative; and
*           secondly, the demand for nitrogen by the potential new growth.

*+  Changes
*     <insert here>

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'nwheats_ndmd')
*
      real       tolnce                ! tolerance for calculation errors
      parameter (tolnce = 1.0e-5)

*+  Local Variables	  *****OK*****
      real       cnit                  ! critical N amount              (g/plant part)
      real       demnew                ! demand for N by new growth     (g/plant part)
     
      real       demold                ! demand for N by old biomass    (g/plant part)
      
      integer    part                  ! plant part
      real       pgro(mxpart)          ! potential dry weight increase  (g/plant part)
      real pndem(mxpart) ! add by jzw

*- Implementation Section ----------------------------------
 
      !*! call push_routine (myname)
 
               ! --------------- N demand by plant -------------
 
          ! now, get N demand of potential new growth of tops and roots from
          ! potential dry weight increase of the tops and roots and the
          ! critical N concentration of the tops and roots.
 
                   ! ...  first component ...
 
          ! calculate potential shoot and root growth
 
      do 500 part = 1, mxpart
         !*! pgro(part) = pcarb* divide (growt(part), carbo, 0.0)
         pgro(part) = pcarb* (growt(part)/ carbo)
         !*! pgro(part) = bound (pgro(part), 0.0, pcarb)
         pgro(part) = max (pgro(part), 0.0)
         pgro(part) = min (pgro(part), pcarb)
500   continue
 
      if (xstage.le.1.2) then
         pgro(stem) = 0.0
         pgro(leaf) = 0.0
         pgro(lfsheath) = 0.0
      else
cnh this algorithm will not work for translocation events and so if we really
cnh need to check this here we will need a new approach.
cnh         call bndchk (pcarb - sum_real_array (pgro, mxpart) ,
cnh     :                       -tolnce, tolnce, 'pcarb - sum of pgro')
 
      endif
 
                  ! ... second component ...
 
          ! get N demands due to difference between actual N concentrations
          ! and critical N concentrations of tops (stover) and roots.
 
      do 1000 part = 1, mxpart
         if (pl_wt(part).gt.0.0) then
 
            cnit = pl_wt(part)*cnc(part)
            demold = cnit - pl_nit(part)
 
 
                ! get potential N demand (critical N) of potential growth
 
            demnew = pgro(part)*cnc(part)
 
            ndmd(part) = demold + demnew
 
cjh    this matches the old version after changing it to be the same.
cjh    previously it had no lower limit. however pers. comm with bak, the
cjh    excess n should be translocated. jngh 170993.
            !*! ndmd(part) = l_bound (ndmd(part), 0.0)
            ndmd(part) = max (ndmd(part), 0.0)
 
         else
            ndmd(part) = 0.0
         endif
 
1000  continue
 
cjh correct parts when total is <= 0
 
      if (sum_real_array (pndem, mxpart).lt.0.0) then
         !*! call fill_real_array (pndem, 0.0, mxpart)
         pndem = 0.
      else
      endif
 
 
cjh  end of correction
 
      !*! call pop_routine (myname)
      return
      end



*     ===========================================================
OK    subroutine nwheats_nuptk (snuptk_no3, snuptk_nh4, pnuptk)      *****OK*****
*     ===========================================================
      implicit none
      include 'convert.inc'            ! gm2kg, sm2ha
      include 'nwheats.inc'             ! CERES_Wheat Common Block
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments	     *****OK*****
      real       pnuptk (mxpart)       ! (OUTPUT) actual plant N uptake into each plant part (g/plant part)


      real       snuptk_no3 (mxlayr)   ! (OUTPUT) actual plant N uptake from NO3 in each layer (kg/ha)


      real       snuptk_nh4 (mxlayr)   ! (OUTPUT) actual plant N uptake from NH4 in each layer (kg/ha)


*+  Purpose
*       Return actual plant nitrogen uptake to each plant part and from
*       each soil layer and for each nitrogen type.

*+  Notes
*       The return of pndem in common block is out of place and needs
*       to be sorted out. JNGH 230693

*+  Changes
*     <insert here>

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'nwheats_nuptk')

*+  Local Variables		  *****OK*****
      real       avail_no3 (mxlayr)    ! potential NO3 (supply) from soil (g/plant)

      real       avail_nh4 (mxlayr)    ! potential NH4 (supply) from soil (g/plant)  

      real       fr_part               ! fraction of nitrogen to use (0-1) for plant part                                     ! 

      integer    layer                 ! soil layer number of profile
      real       n_demand              ! total nitrogen demand (g/plant)
      real       n_supply              ! total nitrogen supply (g/plant)
      integer    nrlayr                ! number of layers with roots
      integer    num_layers
      integer    part                  ! plant part number
      real       pnuptk_tot            ! total plant N uptake (g/plant)
      real       scalef                ! scaling factor for uptake (0-1)
      real       capped_n_demand       !

*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
         call fill_real_array (snuptk_no3, 0.0, mxlayr)
         call fill_real_array (snuptk_nh4, 0.0, mxlayr)
 
            ! N demand by plant
 
            ! pndem is in common and is being changed here
            ! because it is needed for cumulative totals. It is not a state.
            ! Needs to be fixed ????????????????? JNGH 230693
            ! State variables must be only changed by deltas at the top level,
            ! and deltas can be changed at lower levels if they are in
            ! common blocks. Is this acceptable ????
 
      call nwheats_ndmd (pndem)
      n_demand = sum_real_array (pndem, mxpart)
 
      if (g_uptake_source .eq. 'calc') then
            ! First we put a cap on the total uptake that can occur
            ! on any given day by bounding the demand. Note: we do
            ! not bound the "real" n demand, just the value here.
         capped_n_demand = u_bound(n_demand,p_max_n_uptake/plants)
 
            ! find potential N uptake (supply, available N)
 
         call nwheats_nsupply (avail_NO3, avail_NH4)
         nrlayr = count_of_real_vals (rlv, mxlayr)
 
         n_supply = sum_real_array (avail_no3, nrlayr)
     :            + sum_real_array (avail_nh4, nrlayr)
 
         scalef = divide (capped_n_demand
     :                   ,n_supply/ha2sm/gm2kg/plants
     :                   ,0.0)
         scalef = bound (scalef, 0.0, 1.0)
         pnuptk_tot = 0.0
 
         do 1100 layer = 1,nrlayr
 
            snuptk_no3(layer) = avail_no3(layer) * scalef
            snuptk_nh4(layer) = avail_nh4(layer) * scalef
 
            pnuptk_tot = pnuptk_tot + snuptk_no3(layer)
     :                              + snuptk_nh4(layer)
1100     continue
      else
         num_layers = count_of_real_vals (dlayr, mxlayr)
 
         do 1200 layer = 1, num_layers
            snuptk_no3(layer) = uptake_no3(layer)
            snuptk_nh4(layer) = uptake_nh4(layer)
 
            pnuptk_tot = pnuptk_tot + snuptk_no3(layer)
     :                              + snuptk_nh4(layer)
 
 1200    continue
 
      endif
               ! find proportion of uptake to be
               ! distributed to to each plant part and distribute it.
 
      do 1300 part = 1, mxpart
 
         fr_part = divide (pndem(part), n_demand, 0.0)
         pnuptk (part) = pnuptk_tot*fr_part/ha2sm/gm2kg/plants
 
1300  continue
 
      call pop_routine (myname)
      return
      end






















*     ===========================================================
      real function nwheats_rnfac (layer)  *******OK*******
*     ===========================================================
      implicit none
      !*! include 'nwheats.inc'             ! CERES_Wheat Common Block
      !*! include 'data.pub'                          
      !*! include 'error.pub'                         

*+  Sub-Program Arguments      *******OK*******
      integer    layer                 ! (INPUT) layer number

*+  Purpose
*       describes mineral N availability effect on root length growth in a
*       layer (0-1) for root length volume.

*+  Changes
*       020392 jngh specified and programmed
*       120692 jngh corrected spelling of length - cr361
*                   sorted the variable names - cr361
*                   introduced check on layer number - cr362
*       190892 jngh corrected external calls section - cr467(2)
*                   removed soil N array (snit) - cr467(1)

*+  Calls
      real       nwheats_fac            ! function

*+  Constant Values
      character  myname*(*)
      parameter (myname = 'nwheats_rnfac')

*+  Local Variables	   *******OK*******
      real       nh4ppm                ! ammonia concentration (ppm)
      real       no3ppm                ! nitrate concentration (ppm)
      real       rnfac                 ! N factor               (0-1)
      real       totn                  ! total N concentration (ppm)

*- Implementation Section ----------------------------------
 
      !*! call push_routine (myname)
 
      call nwheats_lyrck (layer)
 
      no3ppm = sno3(layer)*nwheats_fac (layer)
      nh4ppm = snh4(layer)*nwheats_fac (layer)
      totn = no3ppm + nh4ppm
 
          ! a weighting factor for the influence of nitrogen on
          ! distribution of daily root growth among layers is
          ! calculated on the basis of mineral N availability.
 
      ! note that this function returns a value of 0.1 with N limiting
      ! (under 1.75) and approaching 1 when N > 150.  This means that
      ! under optimum conditions, this never quite reaches 1.
 
      rnfac = 1.0 - (1.17* exp (-0.15*totn))
 
cjh   this is a v2 lower limit - revert to 0.01 as in v1
cjh      cw_rnfac = bound (rnfac, 0.1, 1.0)
cbak  reinstate v2 lower limit of 0.1 for rnfac  6-6-94
cbak      cw_rnfac = bound (rnfac, 0.01, 1.0)
      nwheats_rnfac = bound (rnfac, 0.1, 1.0)
 
 
      !*! call pop_routine (myname)
 
      return
      end

* ====================================================================
       subroutine nwheats_nsupply(avail_no3, avail_nh4)	  *****OK*****
* ====================================================================
      implicit none
      !*! include 'convert.inc'            ! gm2kg, sm2ha
      !*! include 'nwheats.inc'             ! CERES_Wheat Common Block
      !*! include 'data.pub'                          
      !*! include 'error.pub'                         

*+  Sub-Program Arguments   *****OK*****
       !*! real avail_no3(*)
       real avail_no3(NL)
       !*! real avail_nh4(*)
       real avail_nh4(NL)
*+  Purpose
*     <insert here>

*+  Changes
*     040595 jngh changed calculation of max available NO3 and NH4 to reduce
*                 rounding error

*+  Calls
      real     nwheats_fac              ! function

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'nwheats_nsupply')
*
      real potrate                       ! potential N uptake rate
*
cbak  reduced potrate by factor of 2 ....... 5/6/94
      parameter (potrate = .45e-6)        ! (g N/mm root/day)
*
cbak      parameter (potrate = .9e-6)        ! (g n/mm root/day)

*+  Local Variables   *****OK*****
      real    fnh4
      real    fno3
      integer layer
      real    max_avail_no3
      real    max_avail_nh4
      real    nh4
      real    no3
      integer nrlayr
      real    rlength                    ! root length (mm/ha)
      real    smdfr
      ! add following by JZW
      REAL  sm2ha
      PARAMETER   (sm2ha = 0.0001)  ! conversion factor from NWheat.
      REAL  smm2sm
      PARAMETER   (smm2sm = 0.000001) ! conversion factor from NWheat.
*- Implementation Section ----------------------------------
      !*! call push_routine (myname)
 
      !*! call fill_real_array(avail_no3, 0.0, mxlayr)
      avail_no3 = 0.0
      !*! call fill_real_array(avail_nh4, 0.0, mxlayr)
      avail_nh4 = 0.0
 
      nrlayr = count_of_real_vals (rlv, mxlayr)
 
      do 100 layer = 1, nrlayr
CSenthold
         nh4 = snh4(layer) * nwheats_fac(layer)
cbak         fnh4 = 1.0 - exp (-0.025 * (nh4 - 0.5))
cnh         fnh4 = 1.0 - exp (-0.065 * (nh4 - 0.5))
         fnh4 = 1.0 - exp (-p_enupf_nh4 * (nh4 - p_mnupf_nh4))
         fnh4 = bound (fnh4, 0.0, 1.0)
 
         no3 = sno3(layer) * nwheats_fac(layer)
cnh         fno3 = 1.0 - exp (-0.0275 * no3)
cbak : adjust to reflect reduced partitioning to roots
!         fno3 = 1.0 - exp (-0.0275 * (no3 - 0.2))
cnh          fno3 = 1.0 - exp (-0.0675 * (no3 - 0.2))
         fno3 = 1.0 - exp (-p_enupf_no3 * (no3 - p_mnupf_no3))
         fno3 = bound (fno3, 0.0, 1.0)
 
         ! note - the following should be put somewhere else
         if (swdep(layer) .le. duldep(layer)) then
            smdfr = (swdep(layer) - lldep(layer))/
     :              (duldep(layer) - lldep(layer))
         else
 
! reduce the n availability as sw approaches saturation
            smdfr = (satdep(layer) - swdep(layer))/
     :              (satdep(layer) - duldep(layer))
 
         endif
         !*! smdfr = bound (smdfr, 0.0, 1.0)
         smdfr = max(smdfr, 0.0)
         smdfr = min(smdfr, 1.0)
 
         ! note - the following equations are arranged just as the original
         ! code. these need to be rearranged to show meaning.
 
         ! units for rlength are mm per ha
         rlength = rlv(layer) * dlayr(layer) / smm2sm / sm2ha
 
cnh         avail_no3(layer) = rlength * fno3 * smdfr**2 * potrate*gm2kg
         avail_no3(layer) = rlength * fno3 * smdfr**p_wfnu_power
     :                      * p_pot_nuprate*gm2kg
 
         max_avail_no3 = sno3(layer) - no3mn(layer)
         avail_no3(layer) = u_bound (avail_no3(layer),max_avail_no3)
 
cnh         avail_nh4(layer) = rlength * fnh4 * smdfr**2 * potrate*gm2kg
         avail_nh4(layer) = rlength * fnh4 * smdfr**p_wfnu_power
     :                      * p_pot_nuprate*gm2kg
         max_avail_nh4 = snh4(layer) - nh4mn(layer)
         avail_nh4(layer) = u_bound (avail_nh4(layer), max_avail_nh4)
 
  100 continue
 
      !*! call pop_routine (myname)
      return
      end

*     ===========================================================
      !*! subroutine nwheats_plnin (plantn)
      subroutine nwheats_plnin (istage, stgdur, pl_wt, mnc,    !input
     &    p_root_n_min, p_init_grain_nconc,                     !input  
     &    pl_nit )                                              !output
*     ===========================================================
      USE ModuleDefs
      implicit none
      !*! include 'nwheats.inc'             ! CERES_Wheat Common Block
      !*! include 'data.pub'                          
      !*! include 'error.pub'                         

*+  Sub-Program Arguments  *******OK********
      !*!real       plantn (*)            ! (INPUT/OUTPUT) plant part nitrogen
                                       !   (g/plant)

*+  Purpose
*       initialise plant nitrogen at required instances

*+  Changes
*       210198 nih specified and programmed

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'nwheats_plwin')

*+  Local Variables	*******OK********
      real avail_stem_n
      real avail_root_n
      real root_n_moved
      ! JZW add the following variables
      Integer istage, stgdur(20)
      REAL pl_nit(mxpart) !plant part nitrogen (g/plant)  nwheat
      REAL mnc(mxpart)  ! Minimum N concentration, by plant part
      REAL pl_wt(mxpart), p_root_n_min, p_init_grain_nconc
*- Implementation Section ----------------------------------
 
      !*! call push_routine (myname)
 
 
cnh need to initialise plant n after these weights are initialised.
      If((Istage.eq.1).and.(Stgdur(istage).eq.1)) then
!*!         plantn(root) = .045*pl_wt(root)
!*!         plantn(stem) = .045*pl_wt(stem)
!*!         plantn(leaf) = .045*pl_wt(leaf)
! JZW replace above to the following
          pl_nit(root_part) = .045*pl_wt(root_part)
          pl_nit(stem_part) = .045*pl_wt(stem_part)
          pl_nit(leaf_part) = .045*pl_wt(leaf_part)
cnh 29-9-95 - need to remove early N stress instability
cnh I increase initial N conc to delay stress but the total N balance
cnh should stay OK because plant weights are so small the mass of N is small.
cnh         pl_nit(root) = .01 !g/plant
cnh         pl_nit(stem) = .01
cnh         pl_nit(leaf) = .01
 
!*!         plantn(lfsheath) = pl_nit(leaf)
!*!         plantn(grain) = 0.0
            pl_nit(lfsheath_part) = pl_nit(leaf_part)
            pl_nit(grain_part) = 0.0
      elseif((Istage.eq.grnfil).and.(Stgdur(istage).eq.1)) then
 
!         avail_stem_n = plantn(stem) - mnc(stem) * pl_wt(stem)
!         avail_stem_n = l_bound (avail_stem_n, 0.0)
         avail_stem_n = pl_nit(stem_part) - 
     &                  mnc(stem_part) * pl_wt(stem_part)
         !*! avail_stem_n = l_bound (avail_stem_n, 0.0)
         avail_stem_n = max(avail_stem_n, 0.0)
         if (p_root_n_min.gt.0.0) then
!            avail_root_n = plantn(root) - p_root_n_min * pl_wt(root)
!            avail_root_n = l_bound (avail_root_n, 0.0)
            avail_root_n = pl_nit(root_part) - 
     &                     p_root_n_min * pl_wt(root_part)
            !*! avail_root_n = l_bound (avail_root_n, 0.0)
            avail_root_n = max(avail_root_n, 0.0)
         else
            avail_root_n = 0.0
         endif
!*!      plantn(grain) = p_init_grain_nconc * pl_wt(grain)
         pl_nit(grain_part) = p_init_grain_nconc * pl_wt(grain_part)
!*!      plantn(grain) = u_bound (plantn(grain)
!*!     :                           ,avail_stem_n+avail_root_n)
         pl_nit(grain_part) = min(pl_nit(grain_part)
     :                           , avail_stem_n+avail_root_n)
!*!      plantn(stem) = plantn(stem) - min(avail_stem_n, plantn(grain))    
         pl_nit(stem_part) = pl_nit(stem_part) - 
     &                  min(avail_stem_n, pl_nit(grain_part))
!*!      root_n_moved = max(0.0, plantn(grain) - avail_stem_n)         
         root_n_moved = max(0.0, pl_nit(grain_part) - avail_stem_n)
!*!      plantn(root) = plantn(root) - root_n_moved
         pl_nit(root_part) = pl_nit(root_part) - root_n_moved
 
      else
      endif
 
 
 
      !*! call pop_routine (myname)
      return
      end


