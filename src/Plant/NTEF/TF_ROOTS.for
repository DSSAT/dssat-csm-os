C=======================================================================
C  nwheats_set_adf, Subroutine
C    Returns
C    this is a new routine for senthold's stuff.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/28/2012  FSR adapted from nwheats
!  01/11/2018 KEP converted WH_ sub-routines to TF_.
!-----------------------------------------------------------------------
C  Called by: nwheats_crppr
C  Calls    : integer nwheats_level, nwheats_ad_rtloss
C=======================================================================
*      SUBROUTINE nwheats_set_adf (CONTROL,
*     &    dlayr_nw, duldep, g_water_table, istage, nrlayr,       !Input
*     &    p3af, p4af, p5af, p6af, rtdep_nw,                      !Input
*     &    satdep, swdep, xstag_nw, p_fdsw, p_adf, p_afs, p_stage, !Input
*     &    nlayr_nw, adf, afs)                                    !Output
*
*!     ------------------------------------------------------------------
*      USE ModuleDefs
*      USE TF_module
*      IMPLICIT NONE
*      SAVE
*
*      integer count_of_real_vals
*      integer istage
*      integer L         ! layer no.
*      integer nlayr_nw  ! no. of layers
*      integer nrlayr    ! no. of root layers
*      integer num_stage ! (maybe real?)
*      integer nwheats_level ! Function from nwheats. Returns layer
*                            ! number of depth in profile dlayr_nw
*      integer YRDOY
*      real adf(NL)  !  ?
*      real ADLAI  ! threshold aeration deficit (AF2) affecting LAI
*      real ADPHO  ! threshold aeration deficit (AF2) affecting photosyn
*      real ADTIL  ! threshold aeration deficit (AF2) affecting tillering
*      real afs          !
*      real af1          ! AD affecting root system as a base for
*                        ! above-ground crop effect
*      real af1_time
*      real af2          ! Aeration factor (?)
*      real af2_lai
*      real af2_tiller
*      real af2_photo
*      real ALIN
*      real dlayr_nw(NL) !
*      real duldep(NL)   ! drained upper limit by soil layer
*      real fdsw             ! fraction of drainable soil water (0-1)
*      real g_water_table    ! water table depth (soil parameter)
*      real incorp_root(NL)   ! root to incorporate in each layer
*      real incorp_rootn(NL)  ! root N to incorporate in each layer
*      real num_fdsw     !Number of parameter values returned for fdsw(?)
*      real P3AF         ! Cul parameter: length of root not affected
*                        ! under aeration deficit
*      real p4af         !Days until aeration deficit affects root growth
*      real p5af         ! Cultivar specific power term
*      real p6af         !
*      real p_adf(3)     ! aeration deficit (1 = no stress)
*      real p_afs(2)     ! crop sensitivity to aeration deficit, as a
*                        !  funct of phenol (1 = aeration deficit tolerant crop)
*      real p_fdsw(3)    ! fraction of drainable soil water in layer
*      real p_stage(2)   ! istage (growth stage): emergence - grain fill
*      real pl_nit(mxpart) !plant part nitrogen (g/plant)
*      real plantwt(mxpart)  !plant part weights (g/plant)
*      real rtdep_nw     ! overall root depth in profile (mm) (?)
*      real satdep(NL)   ! Soil parameter: Sat water capacity
*      real sum_adf      ! sum of all adf's across root layers
*      real sum_real_array ! Function in TF_SW_SUBS.for
*      real swdep(NL)    ! water content by soil layer (mm)
*      real xstag_nw     ! growth stage index for use in plant nitrogen
*      REAL TABEX
*
*!     The variable "CONTROL" is of type "ControlType".
*      TYPE (ControlType) CONTROL
*      YRDOY= CONTROL % YRDOY
*
*
*      nlayr_nw = count_of_real_vals (dlayr_nw, NL)
*
*      do L = 1, nlayr_nw
*         ! calculate fraction of drainable soil water
*!*!      fdsw = divide(swdep(L)-duldep(L),satdep(L)-duldep(L),0.0)
*         if((satdep(L)-duldep(L)) .GT. 0.0) then
*            fdsw = (swdep(L)-duldep(L)) / (satdep(L)-duldep(L))
*         else
*            fdsw = 0.0
*         endif
*!*!      fdsw = bound (fdsw,0.0,1.0)
*         fdsw = MAX (fdsw, 0.0)
*         fdsw = MIN (fdsw, 1.0)
*         ! TABEX(Y,X,Xo,n); linear_interp_real(Xo,X,Y)
*         adf(L) = TABEX (p_adf, p_fdsw, fdsw ,3)
*!*!      adf(L) = linear_interp_real(fdsw,p_fdsw,p_adf,num_fdsw)
*!*! Temp solution - FSR   adf(L) = ALIN(fdsw,p_fdsw,p_adf,num_fdsw)
*      enddo
*
*      ! Root killing routine
*      call nwheats_ad_rtloss (CONTROL,
*     &  dlayr_nw, g_water_table,                                  !Input
*     &  P3AF, P4AF,  YRDOY,                                       !Input
*     &  incorp_root, incorp_rootn,                               !Output
*     &  pl_nit, plantwt, rtdep_nw)                               !Output
*
*      ! find no. of layers with roots
*         nrlayr = nwheats_level(rtdep_nw, dlayr_nw, NL)
*
*
*      ! sum adf across all layers with roots
*      sum_adf = sum_real_array(adf,nrlayr)
*
*      ! Calculate AF1, AD affecting root system as a base for
*      ! above-ground crop effect.
*!*!   af1 = divide (sum_adf,real(nrlayr),0.0)**p5af
*      if(nrlayr .GT. 0) then
*         af1 = (sum_adf / real(nrlayr))**p5af
*      else
*         af1 = 0.0
*      endif
*
*      ! Find time for which af1 < threshold value
*      if (af1.lt.p6af) then
*         af1_time = af1_time + 1
*      else
*         af1_time = 0
*      endif
*
*      ! Get phenological sensitivity to AD
*      if ((xstag_nw.ge.1).and.(xstag_nw.le.6)) then
*      ! afs is crop sensitivity to aeration deficit, as a funct of phenol (1 = aeration deficit tolerant crop)
*      afs = TABEX (p_afs, p_stage, xstag_nw, 2)
*!*!      afs = linear_interp_real (xstag, p_stage, p_afs, num_stage)
*!*! Temp solution - FSR   afs = ALIN (xstage, p_stage, p_afs, num_stage)
*      else
*         afs = 1.0
*      endif
*
*      ! If time of AD is > p4af then calculate crop effect AF2 and
*      ! related stresses for growth components
*      if (af1_time .ge. p4af) then
*
*         af2 = max(af1,afs)
*
*         if (af2 .lt. ADLAI) then
*            af2_lai = min(af2,1.0)
*         else
*            af2_lai = 1.0
*         endif
*
*         if (af2 .lt. ADTIL) then
*            af2_tiller = min(af2,1.0)
*         else
*            af2_tiller = 1.0
*         endif
*
*         if (af2 .lt. ADPHO) then
*            af2_photo = min(af2,1.0)
*         else
*            af2_photo = 1.0
*         endif
*      else
*         ! No growth effect by AD so set all factors to one.
*         af2 = 1.0
*         af2_lai = 1.0
*         af2_tiller = 1.0
*         af2_photo = 1.0
*      endif
*
*      return
*      end subroutine  nwheats_set_adf
C=======================================================================
C  nwheats_ad_rtloss, Subroutine
C    Returns
C    (another) new routine for senthold's stuff.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  03/02/2012  FSR adapted from nwheats
!-----------------------------------------------------------------------
C  Called by: nwheats_set_adf
C  Calls    : nwheats_level, nwheats_root_distrib
C=======================================================================
*      SUBROUTINE nwheats_ad_rtloss (CONTROL,
*     &  dlayr_nw, g_water_table,                                  !Input
*     &  P3AF, P4AF,  YRDOY,                                       !Input
*     &  incorp_root, incorp_rootn,                               !Output
*     &  pl_nit, plantwt, rtdep_nw)                               !Output
*!     ------------------------------------------------------------------
*      USE ModuleDefs
*      USE TF_module
*      IMPLICIT NONE
*      SAVE
*
*      integer count_of_real_vals
*      real dlayr_nw(NL)      ! Soil thickness in layer L (mm) NWHEAT
*      real layer_top (NL)    ! depth of top of each layer
*      real layer_bottom (NL) ! depth of bottom of each layer
*      real layer_root(NL)    ! root wt in each layer
*      real layer_rootn(NL)   ! root n in each layer
*      real kill_fraction     ! fraction of roots to kill in a layer
*      real incorp_root(NL)   ! root to incorporate in each layer
*      real incorp_rootn(NL)  ! root N to incorporate in each layer
*      real kill_depth        ! depth below which we kill roots
*      real P3AF              ! Cul parameter: length of root not
*                             ! affected under aeration deficit
*      real P4AF              ! Cul param: days accumulated before
*                             ! aeration deficit effects root growth
*      real g_water_table     ! water table depth (soil parameter)
*      real pl_nit(mxpart)    ! plant part nitrogen (g/plant)
*      real plantwt(mxpart)   ! plant part weights (g/plant)
*      real PLTPOP
*      real rlv_nw(NL)        ! root length volume array in mm/mm3
*      real root_array(NL)    ! array to contain distributed material
*      real root_sum
*      integer ad_time(NL)
*      integer kill_layer     ! layer that includes kill_depth
*      integer L              ! layer number
*!**!      integer NL         !
*      integer nrlayr         ! number of layers with roots
*      integer nwheats_level  ! Function from nwheats. Returns layer
*                             ! number of depth in profile dlayr_nw
*      integer YRDOY
*      character string*(1000) ! output string
*      real effective_water_table ! effective water table depth (mm)
*      real rtdep_nw
*
*      REAL        gm2kg
*      PARAMETER   (gm2kg = 0.001) ! from NWheat.
*      REAL        sm2ha
*      PARAMETER   (sm2ha = 0.0001)  ! from NWheat.
*      REAL        sm2smm
*      PARAMETER   (sm2smm = 1000000) ! from NWheat.
*!     The variable "CONTROL" is of type "ControlType".
*      TYPE (ControlType) CONTROL
*      YRDOY= CONTROL % YRDOY
*
*      ! FIND THE NUMBER OF LAYERS WITH ROOTS
*      ! ------------------------------------
*      nrlayr = nwheats_level (rtdep_nw, dlayr_nw, NL)
*
*      ! FIND THE EFFECTIVE WATER TABLE THAT ACCOUNTS FOR P3AF
*      ! -----------------------------------------------------
*      effective_water_table = g_water_table + P3AF   ! P3AF for p3af
*
*      ! RECORD THE TIME OF AERATION DEFICIT FOR EACH LAYER
*      ! --------------------------------------------------
*      ! AND THE TOP AND BOTTOM DEPTH OF EACH LAYER
*      ! ------------------------------------------
*      do L = 1, nrlayr
*         if (L.eq.1) then
*            layer_top(L) = 0.0
*            layer_bottom(L) = dlayr_nw(L)
*         else
*            layer_top(L) = layer_bottom(L-1)
*            layer_bottom(L)= layer_bottom(L-1) + dlayr_nw(L)
*         endif
*
*         if (effective_water_table.lt. layer_bottom(L)) then
*            ! the top of the water table is above the lower
*            ! boundary of this layer.
*            ad_time(L) = ad_time(L) + 1
*         else
*            ad_time(L) = 0
*         endif
*      enddo
*
*      ! FIND THE DEPTH BELOW WHICH ANY ROOTS NEED TO BE KILLED
*      ! ------------------------------------------------------
*      do L = 1, nrlayr
*         if (ad_time(L) .ge. P4AF) then
*            kill_depth = effective_water_table
*            kill_depth = min(kill_depth, rtdep_nw)
*            if (kill_depth.lt.rtdep_nw) then
*               goto 201
*            else
*               kill_depth = 0.0
*            endif
*         else
*            kill_depth = 0.0
*         endif
*      enddo
*
*  201 continue
*
*      ! SEE IF WE NEED TO INCORPORATE ANY ROOT MATERIAL
*      ! -----------------------------------------------
*      if (kill_depth .gt. 0.0) then
*
*         write (string, '(a,f6.1,a,i3)')
*     &                  'Killing all roots below ',kill_depth,
*!*!  &                  ' mm due to water logging on day ',day_of_year
*     &                  ' mm due to water logging on day ',YRDOY
*!*!      call write_string (lu_summary_file, string)
*
*
*         ! GET THE DISTRIBUTION OF ROOT MATERIAL IN PROFILE
*         ! ------------------------------------------------
*         ! - - - - - - all root dry matter - - - - - - -
*      root_sum = plantwt(root_part) * PLTPOP * gm2kg /sm2ha
*
*      call nwheats_root_distrib (CONTROL,
*     &  dlayr_nw, rtdep_nw, root_sum,                             !Input
*     &  root_array)                                              !Output
*
*      layer_root = root_array
*         ! - - - - - - root N only - - - - - - -
*      root_sum = pl_nit(root_part) * PLTPOP * gm2kg /sm2ha
*
*      call nwheats_root_distrib (CONTROL,
*     &  dlayr_nw, rtdep_nw, root_sum,                             !Input
*     &  root_array)                                              !Output
*
*      layer_rootn = root_array
*
*!*!   call fill_real_array (incorp_root,0.0,NL)
*         do L=1, NL
*            incorp_root(L) = 0.0
*         enddo
*!*!   call fill_real_array (incorp_rootn,0.0,mxlayr)
*         do L=1, NL
*            incorp_rootn(L) = 0.0
*         enddo
*
*!*!      kill_layer = find_layer_no (kill_depth, dlayr_nw, mxlayr)
*         kill_layer = nwheats_level (kill_depth, dlayr_nw, NL)
*
*         ! RECALCULATE POOLS FROM KILL_LAYER DOWN
*         ! --------------------------------------
*         do L = kill_layer, nrlayr
*
*            ! CALCULATE FRACTION OF LAYER TO KILL
*            ! -----------------------------------
*            if ((L.eq.kill_layer).and.(L.ne.nrlayr)) then
*
*               if(dlayr_nw(kill_layer) .GT. 0.0) then
*                  kill_fraction = 1.0 -
*!*!  :                        divide(kill_depth - layer_top(kill_layer)
*!*!  :                              ,dlayr_nw(kill_layer)
*!*!  :                              ,0.0)
*     &                        (kill_depth - layer_top(kill_layer))
*     &                        / dlayr_nw(kill_layer)
*               else
*                  kill_fraction = 1.0
*               endif
*            else
*              kill_fraction = 1.0
*            endif
*
*            if (kill_fraction.lt.1.0) then
*               ! keep the timer going
*            else
*               ! no roots left - reset the timer
*               ad_time(L) = 0
*            endif
*
*            incorp_root(L) = layer_root(L)*kill_fraction
*            incorp_rootn(L) = layer_rootn(L)*kill_fraction
*            plantwt(root_part) = plantwt(root_part) - incorp_root(L)
*     :                                /(PLTPOP * gm2kg /sm2ha)
*            pl_nit(root_part) = pl_nit(root_part) - incorp_rootn(L)
*     :                                /(PLTPOP * gm2kg /sm2ha)
*            rlv_nw(L) = rlv_nw(L) * (1. - kill_fraction)
*         enddo
*         rtdep_nw = kill_depth
*
*         ! BUILD THE MESSAGE TO SEND FOR SOILN TO INCORPORATE
*         ! --------------------------------------------------
*         !             FRESH ORGANIC MATERIAL
*         !             ----------------------
*
*!*!      call New_PostBox ()
*
*!*!         call post_char_var   ('dlt_fom_type'
*!*!     :                        ,'()'
*!*!     :                        ,crop_type)
*
*!*!        call post_real_array ('dlt_fom_wt'
*!*!     :                        ,'(kg/ha)'
*!*!     :                        , incorp_root
*!*!     :                        ,nrlayr)
*
*!*!         call post_real_array ('dlt_fom_n'
*!*!     :                        ,'(kg/ha)'
*!*!     :                        , incorp_rootn
*!*!     :                        ,nrlayr)
*
*!*!        string = blank
*!*!        call message_send_immediate (all_active_modules
*!*!     :                              ,'incorp_fom'
*!*!     :                              ,string)
*!*!
*!*!        call Delete_PostBox ()
*
*      else
*      endif
*
*      return
*      end subroutine nwheats_ad_rtloss

C=======================================================================
C  nwheats_root_distrib, Subroutine
C    Distribute root material over profile
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  03/02/2012  FSR adapted from nwheats
!-----------------------------------------------------------------------
C  Called by: nwheats_ad_rtloss, nwheats_add_sen_roots, nwheats_harvest_crop,
C  Calls    : nwheats_level, sum_real_array
C=======================================================================
*      SUBROUTINE nwheats_root_distrib (CONTROL,
*     &  dlayr_nw, rtdep_nw, root_sum,                             !Input
*     &  root_array)                                              !Output
*!     ------------------------------------------------------------------
*      USE ModuleDefs
*      USE TF_module
*      IMPLICIT NONE
*      SAVE
*
*      real root_array(NL)   ! (OUTPUT) array to contain
*                                ! distributed material
*      real root_sum             ! (INPUT) Material to be distributed
*      real cum_depth            ! cumulative depth (mm)
*      integer L                 ! layer number ()
*      integer deepest_layer     ! deepest layer in which the roots are
*                                ! growing
*      real root_distrb(NL)  ! root distribution ()
*      real root_distrb_sum      ! sum of root distribution array
*      real dlayr_nw(NL)     ! Soil thickness in layer L (mm) NWHEAT
*      real rtdep_nw
*      real pl_nit(mxpart)       ! plant part nitrogen (g/plant)
*      real plantwt(mxpart)      ! plant part weights (g/plant)
*      real PLTPOP
*      real sum_real_array
*!**!      integer NL            !
*      integer nrlayr            ! number of layers with roots
*      integer nwheats_level     ! Function from nwheats. Returns layer
*                                ! number of depth in profile dlayr_nw
*      REAL        gm2kg
*      PARAMETER   (gm2kg = 0.001)   ! from NWheat.
*      REAL        sm2ha
*      PARAMETER   (sm2ha = 0.0001)  ! from NWheat.
*      TYPE (ControlType) CONTROL
*
*! distribute roots over profile to root_depth
*
*!*!   call fill_real_array (root_array, 0.0, mxlayr)
*         do L=1, NL
*               root_array(L) = 0.0
*         enddo
*!*!   call fill_real_array (root_distrb, 0.0, mxlayr)
*         do L=1, NL
*               root_distrb(L) = 0.0
*         enddo
*
*      deepest_layer = nwheats_level (rtdep_nw, dlayr_nw, NL)
*      cum_depth = 0.0
*      do L = 1, deepest_layer
*         cum_depth = cum_depth + dlayr_nw(L)
*!*!      cum_depth = u_bound (cum_depth, rtdep_nw)
*         cum_depth = MIN(cum_depth, rtdep_nw)
*!*!      root_distrb(L) = exp (-3.0 * divide (cum_depth
*!*!  &                                          , rtdep_nw, 0.0))
*         if(rtdep_nw .GT. 0.0) then
*            root_distrb(L) = exp (-3.0 * (cum_depth / rtdep_nw))
*         else
*            root_distrb(L) = 0.0
*         endif
*      enddo
*
*      root_distrb_sum = sum_real_array (root_distrb, deepest_layer)
*      do L = 1, deepest_layer
*!*!      root_array(L) = root_sum * divide (root_distrb(L)
*!*!   &                                        , root_distrb_sum, 0.0)
*         if(root_distrb_sum .GT. 0.0) then
*            root_array(L) = root_sum * root_distrb(L) / root_distrb_sum
*         else
*            root_array(L) = 0.0
*         endif
*
*      enddo
*      return
*      end subroutine nwheats_root_distrib

C=======================================================================
C  nwheats_rtdp, Subroutine
C    returns the potential increase in root depth (mm)
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/06/2012  FSR adapted from nwheats
!  Note there is a discrepency when the root crosses into
!  another layer. - cr380
!-----------------------------------------------------------------------
C  Called by: nwheats_crppr,
C  Calls to : nwheats_level (function), nheats_swafc (function),
C             nwheats_rtdp_swf (function)
C=======================================================================
*      SUBROUTINE nwheats_rtdp (CONTROL, SOILPROP,
*     &  ADPHO, dlayr_nw, dtt, duldep, g_water_table, istage,      !Input
*     &  lldep, nrlayr, p3af, rtdep_nw,                            !Input
*     &  RTDEP, RTDP1, RTDP2, SDEPTH, stgdur, swdef, swdep, wr,    !Input
*     &  rtdnew)                                                   !Output
*!     ------------------------------------------------------------------
*      USE ModuleDefs
*      USE TF_module
*      IMPLICIT NONE
*      SAVE
*
*      integer count_of_real_vals
*      integer istage
*      INTEGER mxstag
*      PARAMETER (mxstag = 9)
*!**!      integer NL
*      integer nrlayr   ! deepest layer in which the roots are growing
*      integer nwheats_level ! Function from nwheats. Returns layer
*                               ! number of depth in profile dlayr_nw
*      integer num_layers
*      REAL    RTDP1    ! soil-water effect parameter, 0 = old version
*      REAL    RTDP2    !crop stress effect parameter, 0 = old version
*      INTEGER stgdur(20) ! Number of days accum in a growth stage as
*                         ! expressed by stgdur(istage)
*      real ADPHO ! threshold aeration deficit (AF2) affecting photosyn
*      real dlayr_nw(NL)     ! Soil thickness in layer L (mm) NWHEAT
*      real dtt
*      real duldep(NL)
*      real g_water_table ! water table depth (soil parameter)
*      real lldep(NL)
*      real new_depth
*      real nwheats_rtdp_swf ! function
*      real nwheats_swafc ! function
*      real optfr    ! fraction of optimum conditions (0-1)
*      real P3AF     ! Cul parameter: length of root not affected
*                          ! under aeration deficit
*      REAL RTDEP
*      real rtdep_nw
*      real rtdnew    ! (OUTPUT) increase in root depth (mm)
*      real rtrate(mxstag) !root growth rate potential(mm /deg day)
*      REAL SDEPTH     ! Sowing depth, cm
*      real stress_factor
*      real sum_real_array
*      real swdep(NL)
*      REAL swdef(10)
*      real sw_factor
*      real wr(NL) ! root weighting by soil layer
*      real nem(NL) ! nem (9) changed by JZW
*
**+  Initial Data Values
*      save rtrate
*      data rtrate (germ)   / 1.0 /
*      data rtrate (emergence)  / 2.2 /
*      data rtrate (endjuv) / 2.2 /
*      data rtrate (endveg) / 2.2 /
*      data rtrate (endear) / 2.2 /
*      data rtrate (grnfil) / 2.2 /
*      data rtrate (mature) / 0.0 /
*      data rtrate (fallow) / 0.0 /
*      data rtrate (sowing) / 0.0 /
*
*      TYPE (ControlType) CONTROL
*      TYPE (SoilType) SOILPROP
*      ! nem (1) = 0.869; nem(2)= 0.869; nem(3)=0.657 ; nem(4)=0.507;
*      ! nem(5) = 0.333; nem(6)= 0.333; nem(7)= 0.333; nem(8) = 0.333;
*      ! nem(9) = 0.333
*      nem = SOILPROP % WR
*C----------------------------------------------------------------------
*C            DYNAMIC = RUNINIT OR SEASINIT
*C----------------------------------------------------------------------
*      IF(CONTROL%DYNAMIC.EQ.RUNINIT.OR.CONTROL%DYNAMIC.EQ.SEASINIT) THEN
*        ! JZW add the following initial value
*!        rtdep_nw = 0.
*!        nrlayr = 1.
*!        rtdnew  = 0.
*!        stress_factor = 1.
*!        sw_factor = 1.
*!        optfr = 1.
*!        new_depth = 0.
*C----------------------------------------------------------------------
*C                 DYNAMIC = RATE
*C----------------------------------------------------------------------
*      ! JZW add the following. These should be called in the DYNAMIC = INTEGR
*      ELSEIF(CONTROL % DYNAMIC .EQ. RATE) THEN
*         ! increase root depth   ! sowing     = 8, stgdur(8) = 2
*         if (stgdur(sowing).eq.1 .and. istage.eq.sowing) then
*             ! initialise root depth
*             ! this version (ceres wheat) does take account of sowing depth.
*             rtdep_nw = SDEPTH * 10 ! Units from cm to mm.
*         else if (istage .eq. germ) then !germ = 9
*            ! soil moisture does not affect rooting depth early on
*      !**!     nrlayr = nwheats_level(rtdep_nw)
*            nrlayr = nwheats_level (rtdep_nw,dlayr_nw,NL)
*
* !*!        optfr = nem(nrlayr) ! Need to introduce nematode effect (0-1.0)
*
*            optfr = 1.0
*            rtdnew = rtrate(germ) * dtt * optfr
*         endif
*      rtdnew  = rtrate(istage)*optfr*dtt !*95./phint in orig ceres wheat
*
*      ! This root depth increase must not take roots too far into any water table.
*      ! --------------------------------------------------------------------------
*      new_depth = rtdep_nw + rtdnew
*!*!   new_depth = u_bound (new_depth, g_water_table + p3af)
*      new_depth = MIN (new_depth, g_water_table + p3af)
*!*!   rtnew = l_bound (new_depth - rtdep_nw ,0.0)
*      rtdnew = MAX (new_depth - rtdep_nw ,0.0)
*
*!         nrlayr = nwheats_level (rtdep_nw,dlayr_nw,NL)
*!         optfr = 1.0
*!         rtdnew =0
*C----------------------------------------------------------------------
*C             DYNAMIC = INTEGR
*C----------------------------------------------------------------------
*
*      ELSEIF(CONTROL % DYNAMIC.EQ.INTEGR) THEN
**- Implementation Section ----------------------------------
*
*             ! increase root depth   ! sowing     = 8, stgdur(8) = 2
*      if (stgdur(sowing).eq.1 .and. istage.eq.sowing) then
*             ! initialise root depth
*             ! this version (ceres wheat) does take account of sowing depth.
*         rtdep_nw = SDEPTH * 10 ! Units from cm to mm.
*
*      else if (istage .eq. germ) then !germ = 9 JZW, istage 9 will not go es to Dynamic = INTEGR
*         ! soil moisture does not affect rooting depth early on
*!**!     nrlayr = nwheats_level(rtdep_nw)
*         nrlayr = nwheats_level (rtdep_nw,dlayr_nw,NL)
*
* !*!        optfr = nem(nrlayr) ! Need to introduce nematode effect (0-1.0)
*         optfr = 1.0
*         rtdnew = rtrate(germ) * dtt * optfr
*
*      else if ((istage. ge. emergence) .and. (istage .lt. mature)) then
*             ! we have root growth (in a vegetative phase)
*             ! find the deepest layer in which roots are growing
*
*!**!         nrlayr = nwheats_level(rtdep_nw)
*         nrlayr = nwheats_level (rtdep_nw,dlayr_nw,NL)
*
*           ! this equation allows soil water in the deepest
*           ! layer in which roots are growing and water stress on growth
*           ! to affect the daily increase in rooting depth.
*
*cnh senthold
*           ! Growth stress effect on root front
*           ! ----------------------------------
* !*!     if (p_rtdp_stress_switch.eq.0) then
*         if (RTDP2 .eq. 0) then    ! RTDP2 = p_rtdp_stress_switc
* !*!        stress_factor = 2.0 * swdef(photo)
*            stress_factor = 2.0 * swdef(photo_nw) ! or ADPHO?
*
*         else
*            stress_factor = 1.0
*         endif
*
*           ! Soil dryness effect on root front
*           ! ---------------------------------
*!*!  FSR - May need to substitute DSSAT soil-water values for nwheat
*!*!        functions nwheats_swafc and/or nwheats_rtdp_swf,
*
*!!!      if (p_rtdp_swf_switch.eq.0) then
*         if (RTDP1 .eq. 0) then ! RTDP1 = p_rtdp_swf_switch; 0 = old ver
*!**!         sw_factor = nwheats_swafc(nrlayr)
*             sw_factor = nwheats_swafc(nrlayr, duldep, lldep, NL, swdep)
*
*         else
*!*!         sw_factor = nwheats_rtdp_swf(nrlayr)
*            num_layers = count_of_real_vals (dlayr_nw, NL)
*            sw_factor = nwheats_rtdp_swf(nrlayr, dlayr_nw, duldep,
*     &                  lldep, NL, num_layers, rtdep_nw, swdep)
*
*         endif
*
*!*!      optfr = min(stress_factor, sw_factor, nem(nrlayr))
*         !  replace nem by wr
*         !optfr = min(stress_factor, sw_factor)
*         optfr = min(stress_factor, sw_factor, nem(nrlayr))  ! remove nem() until
*                                                ! further notice - FSR
*      endif
*
*      rtdnew  = rtrate(istage)*optfr*dtt !*95./phint in orig ceres wheat
*
*      ! This root depth increase must not take roots too far into any water table.
*      ! --------------------------------------------------------------------------
*      new_depth = rtdep_nw + rtdnew
*!*!   new_depth = u_bound (new_depth, g_water_table + p3af)
*      new_depth = MIN (new_depth, g_water_table + p3af)
*!*!   rtnew = l_bound (new_depth - rtdep_nw ,0.0)
*      rtdnew = MAX (new_depth - rtdep_nw ,0.0)
*
*C----------------------------------------------------------------------
*C                     DYNAMIC = OUTPUT
*C----------------------------------------------------------------------
*      ELSEIF(CONTROL % DYNAMIC.EQ.OUTPUT) THEN
*
*      ! {no procedures to date}
*
*      ENDIF !Dynamic loop
*      return
*      end SUBROUTINE nwheats_rtdp

C=======================================================================
C  nwheats_rtlv, Subroutine
C   returns potential root-length-volume (density) increase
C    (mm root/mm^3 soil)
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  19/09/2012  FSR adapted from nwheats
!-----------------------------------------------------------------------
C  Called by: nwheats_crppr,
C  Calls to : nwheats_level (integer function), nheats_swafc (real function),
C             nwheats_rnfac (real function)
C=======================================================================
*      SUBROUTINE nwheats_rtlv (CONTROL, SOILPROP, sno3, snh4,
*     &  dlayr_nw, istage, nrlayr, p2af, p3af, PLTPOP,             !Input
*     &  nwheats_level, rtdep_nw, stgdur, wr,                      !Input
*     &  duldep, lldep, swdep, nlayr_nw, rlv_nw, gro_wt, adf,      !Input
*       ! duldep, lldep should be removed if we use SOILPROP !!!!!!!!!!!
*     &  rlvnew)                                                   !Output
*!     ------------------------------------------------------------------
*      USE ModuleDefs
*      USE TF_module
*      IMPLICIT NONE
*      SAVE
*
*      integer istage
*      real    rlvnew (NL)  ! (OUTPUT) potential new root-length-volume
*                          !          (mm root/mm^3 soil)
**+  Constant Values
*      real   grort_nw       ! assumed root growth at emergence (g/plant)
*      parameter (grort_nw = 0.06)
*      real   rlrate    ! rate of root length growth per gram root (mm/g)
*      parameter (rlrate = 105000.0)
*
**+  Local Variables    **************
*      real adf(NL)  !
*      real cumdep_nw   ! depth of deepest layer with roots (mm)
*      real gro_wt(mxpart)  ! root, leaf, lfsheath, stem, grain
*      integer L ! layer number
*!**!      integer NL
*      integer nlayr_nw  ! Number of layers in specific soil profile
*      integer nrlayr ! number of layers with roots
*      integer nwheats_level ! Function from nwheats. Returns layer
*                            ! number of depth in profile dlayr_nw
*      INTEGER     stgdur(20)
*      real duldep(NL)   ! drained upper limit by soil layer
*      real lldep(NL)
*      real swdep(NL)    ! water content by soil layer (mm)
*      real dlayr_nw(NL) ! Soil thickness in layer L (mm) NWHEAT
*      real layer_bottom ! depth of bottom of the layer
*      real nwheats_rnfac ! Function from nwheats
*      real nwheats_swafc ! Function from nwheats: mineral N availability
*                         ! effect on root length by soil layer
*      real optfr    ! fraction of optimum condition (0-1)
*      real p2af ! cultivar parameter: threshold AD in a soil layer
*      real p3af ! cultivar parameter: length of downwards root
*                ! not effected under aeration deficit
*      real PLTPOP
*      real rldf (NL) ! root length density factor for each soil
*                         ! layer used to calculate new root growth distribution
*      real rlnew_nw ! new root growth to be added to total root system
*                    ! (mm root/mm^2 soil)
*
*      real rlnew_layr ! new root growth to be added to root system in a
*                      ! layer (mm root/mm^2 soil)
*      real rlv_nw(NL) ! root length volume array in mm/mm3
*      real rtdep_nw
*      real rtdepl ! root depth in deepest root layer (mm)
*      real trldf_nw  ! the total root length density factor for root
*                        ! growth for all layers in which roots occur. (0-NL)
*      real rlvmax
*      real sum_real_array
*      real wr(NL) ! root weighting by soil layer
*      real      sm2smm
*      parameter (sm2smm = 1000000) ! from NWheat.
*      real sno3(NL), snh4(NL) ! add by JZW
*
*      character*78 msg(1) !warning and error messages
*      CHARACTER*8     ERRKEY
*      PARAMETER       (ERRKEY='TF_ROOTS')
*
*      TYPE (ControlType) CONTROL
*      TYPE (SoilType) SOILPROP
**- Implementation Section ----------------------------------
*!**!  nrlayr = nwheats_level (rtdep_nw)
*      nrlayr = nwheats_level(rtdep_nw,dlayr_nw,NL)
*    ! cumdep_nw = sum_real_array (dlayr_nw, nlayr_nw) JZW changed
*      cumdep_nw = sum_real_array (dlayr_nw, nrlayr)
*
*            ! note that if root depth > depth of profile then
*            ! rtdep_nw > cumdep. This is corrected where it is used.
*
*!*!   call fill_real_array (rldf, 0.0, mxlayr)
*      do L=1, NL
*         rldf(L) = 0.0
*      enddo
*
*!**!  Temporarily set root weighting in all soil layers to 1.0 - FSR
*!**!  This must be removed when wr is added to DSSAT soil profiles
*!**!  do L=1, NL
*!**!     wr(L) = 1.0
*!**!  enddo
*
*          if (rtdep_nw .eq. 0.) then
*            write(msg(1),*) "there will be divide zero in nwheats_rtlv"
*            call warning(1,"NWheat",msg)
*            call error("NWheat",99," ",0)
*           endif
*
*      if (stgdur(emergence).eq.1 .and. istage.eq.emergence) then
*
*             ! we have emergence
*
*!*!      rlnew = rlrate*grort* (plants/sm2smm)
*         rlnew_nw = rlrate * grort_nw * (PLTPOP/sm2smm)
*        !   mm        mm        g           plant/m2
*        !--------  = ----- * -------- * --------------
*        !   mm3        g       plant      1000000 mm2/m2
*
*         do L = 1,nrlayr
*            rldf(L) = wr(L)
*!*!  :                   * divide (dlayr(layer), rtdep, 0.0)
*     &                   * (dlayr_nw(L) / rtdep_nw)
*         enddo
*      else
*             ! we have increase in root length
*
*          ! convert the daily growth of the root system (g/plant) to root
*          ! length (mm root/mm^2 soil surface area)
*
*         rlnew_nw  = rlrate * gro_wt(root_part) *(PLTPOP/sm2smm) !JZW root_part  = 1, gro_wt is not given
*        !   mm           mm             g              plant/m2
*        !--------  = ------- *  --------------- *  ---------------
*        !   mm3          g          plant             1000000 mm2/m2
*         layer_bottom = 0.0
*
*!*!      do 1000 layer = 1,nrlayr
*         do L = 1,nrlayr
*
*             ! now, calculate the 0-1 root length density factor for root
*             ! growth in a layer.  It is calculated from the soil water
*             ! availability factor, a root growth weighting factor for
*             ! soil depth, and a 0-1 root growth factor dependent on mineral
*             ! N availability.
*cnh senthold
*         ! calculate the depth of the bottom of the layer
*         layer_bottom = layer_bottom + dlayr_nw(L) ! JZW layer_bottom is DS(L)?
*
*         ! No root growth in layers with AD.  Layers < p3af are
*         ! exempt from this criteria to make sure that some root is
*         ! always left behind in the top of the profile.
*
*         if ((adf(L).lt.p2af).and.(layer_bottom.gt.p3af)) then
*            ! there is AD and depth > p3af
*            optfr = 0.0
*            goto 1001
*         else
*            ! no AD or layer too shallow so use original factors
*           ! optfr = nwheats_swafc(L, duldep, lldep, NL, swdep) ! add by JZW
*            !*! optfr = min (nwheats_swafc(L), nwheats_rnfac(L))
*            optfr = min (nwheats_swafc(L,duldep, lldep, NL, swdep),
*     &             nwheats_rnfac(L, sno3, snh4, SOILPROP))
*            continue
*            ! Dec 12 Fred was not negative, but Jin get negative
*         endif
*
*            rldf(L) = wr(L) *optfr
*!*!  :                   *divide (dlayr_nw(L), rtdep, 0.0)
*     &                   * dlayr_nw(L) / rtdep_nw
*
*!*! 1000    continue
*         enddo
* 1001    continue
*      endif
*
*          ! adjust the root length density factor of the deepest layer
*          ! in which roots are growing, for the fraction of that layer that
*          ! has been explored.
*      !rtdepl is root depth in deepest root layer (mm)
*      ! JZW should be rtdep_nw - ds_nw(nrlayr-1)
*      rtdepl = dlayr_nw(nrlayr) - (cumdep_nw - rtdep_nw)
*!*!   rtdepl = bound (rtdepl, 0.0, dlayr_nw(nrlayr))  ! in case rtdep is out
*      rtdepl = MAX (rtdepl, 0.0)
*      rtdepl = MIN (rtdepl, dlayr_nw(nrlayr))
*!*!   rldf(nrlayr) = rldf(nrlayr)*divide (rtdepl, dlayr_nw(nrlayr), 0.0)
*      rldf(nrlayr) = rldf(nrlayr) * rtdepl / dlayr_nw(nrlayr)
*
*         ! get the total root length density factor for root growth for all
*         ! layers in which roots occur.
*
*      trldf_nw = sum_real_array (rldf, nrlayr)
*cnh senthold
*      ! if all layers are waterlogged put the root into the top layer
*      if (trldf_nw .eq. 0) then
*         trldf_nw = 1.0
*         rldf(1) = 1.014
*      else
*      endif
*
*         ! distribute newly formed root length
*         ! throughout the soil profile from the amount of new root length
*         ! per unit area of soil.
*
*!**!  call fill_real_array (rlvnew, 0.0, mxlayr)
*            do L=1, NL
*               rlvnew(L) = 0.0
*            enddo
*
*      cumdep_nw = 0.0
*      do L = 1,nrlayr
*         cumdep_nw = cumdep_nw + dlayr_nw(L)
*         if ( (trldf_nw .eq. 0.) .or. (dlayr_nw(L) .eq. 0.)) then
*             rlnew_layr = 0. !JZW set
*         else
*!*!      rlnew_layr = divide (rldf(L), trldf, 0.0) *rlnew
*           rlnew_layr = (rldf(L) / trldf_nw) * rlnew_nw
*         endif
*!*!      rlvnew(L) = divide (rlnew_layr, dlayr_nw(L), 0.0)
*         rlvnew(L) = rlnew_layr / dlayr_nw(L)
*cnh needs tidying up!!!
*cnh cmsat was wrong!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*
*         if (cumdep_nw .gt. 1150.) then
*            rlvmax = 0.00377 - 0.0000015 * cumdep_nw
*!*!         rlvmax = l_bound (rlvmax, rlv(L))
*            rlvmax = MAX(rlvmax, rlv_nw(L))
*!*!         rlvnew(L) = u_bound (rlvnew(L), rlvmax - rlv(L))
*            rlvnew(L) = MIN(rlvnew(L), rlvmax - rlv_nw(L))
*         else
*         endif
*      enddo
*      return
*      end
*
C=======================================================================
C  nwheats_add_sen_roots, Subroutine
C   returns senesced root dry matter and N to soil
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  20/09/2012  FSR adapted from nwheats
!-----------------------------------------------------------------------
C  Called by: nwheats_process,
C  Calls to : nwheats_root_distrib
C=======================================================================
*      SUBROUTINE nwheats_add_sen_roots (CONTROL,
*     &  dlayr_nw, rootsen, rtdep_nw,                             !Input
*     &  deepest_layer, root_array)                              !Output
*!     ------------------------------------------------------------------
*      USE ModuleDefs
*      IMPLICIT NONE
*      SAVE
*
*      integer istage
*      integer deepest_layer   ! deepest layer in which roots grow
*      integer layer                 ! layer number
*!**!      integer NL    ! max number of layers permitted in any soil(?)
*      integer nwheats_level ! Function from nwheats. Returns layer
*                               ! number of depth in profile dlayr_nw
*      real dlayr_nw(NL)! Thickness increment of soil layer L  (mm)
*      real dlt_dm_incorp(NL) ! root residue (kg/ha)
*      real dlt_N_incorp(NL)  ! root residue N (kg/ha)
*      real root_array(NL)   ! (OUTPUT) array to contain
*                                ! distributed material
*      real rootsen  ! Senesced root
*      real root_sum             ! (INPUT) Material to be distributed
*      real rtdep_nw
*
*!      real cum_depth            ! cumulative depth (mm)
*!      integer L                 ! layer number ()
*
*      TYPE (ControlType) CONTROL
*
**  Implementation Section ----------------------------------
*
*      if (rootsen.gt.0.0) then
*
*            ! send out root residue
*C=======================================================================
*!*!  Note the following 2 calls to the same subroutine send it different
*!*!  inputs (rootsum; the second argument), to get different output
*!*!  arrays (root_array): DM (dlt_dm_incorp) and N (dlt_N_incorp) - FSR
*
*!*!      call nwheats_root_distrib (dlt_dm_incorp
*!*!  :                          , rootsen * plants * gm2kg /sm2ha)
*
*         call nwheats_root_distrib (CONTROL,
*     &      dlayr_nw, rtdep_nw, root_sum,                         !Input
*     &      root_array)                                          !Output
*
*!*!      call nwheats_root_distrib (dlt_N_incorp
*!*!  :                          , rootnsen * plants * gm2kg /sm2ha)
*
*         call nwheats_root_distrib (CONTROL,
*     &      dlayr_nw, rtdep_nw, root_sum,                         !Input
*     &      root_array)                                          !Output
*C=======================================================================
*!*!      deepest_layer = find_layer_no (rtdep, dlayr, mxlayr)
*         deepest_layer = nwheats_level (rtdep_nw, dlayr_nw, NL)
*
*!*! The following code will be modified to provide results to DSSAT
*!*! soil routines - FSR
*C=======================================================================
*!*!      call New_PostBox ()
*!*!
*!*!      call post_char_var   ('dlt_fom_type'
*!*!  :                        ,'()'
*!*!  :                        ,crop_type)
*!*!
*!*!      call post_real_array ('dlt_fom_wt'
*!*!  :                        ,'(kg/ha)'
*!*!  :                        , dlt_dm_incorp
*!*!  :                        ,deepest_layer)
*!*!
*!*!      call post_real_array ('dlt_fom_n'
*!*!  :                        ,'(kg/ha)'
*!*!  :                        , dlt_n_incorp
*!*!  :                        ,deepest_layer)
*!*!
*!*!     call message_send_immediate (all_active_modules
*!*!  :                              ,'incorp_fom'
*!*!  :                              ,blank)
*!*!
*!*!     call Delete_PostBox ()
*C=======================================================================
*      else
*         ! no roots to incorporate
*      endif
*
*      return
*      end




C=======================================================================
! ADLAI     threshold aeration deficit (AF2) affecting LAI
! ADTIL     threshold aeration deficit (AF2) affecting tillering
! ADPHO     threshold aeration deficit (AF2) affecting photosyn.
! CUMDEP    !Cumulative depth of soil, cm
! cumdep_nw !Cumulative depth of soil from nwheat
! CUMDTT    !Cumulative daily thermal time after germination, C
! DEPMAX    !Depth of soil, cm
! DLAYR(L)  Soil thickness in layer L (cm)
! dlayr_nw(L) Soil thickness in layer L (mm) NWHEAT
! DTT       !Growing degrees occurring today (Base 8C), C
! ESW(20)   !Extractable water in soil layer L, cm
! GRORT     !Root growth rate, g/plant/day
! ISTAGE    !Crop growth stage (1-9)
!RISWNIT*1  !Switch indicating if soil nitrogen balance is on (Y/N)
! L         !Loop counter
! L1        !Loop counter
! LL(20)    !Volumetric lower limit of soil water in soil layer L, cm3/cm3
! NH4(20)   !Ammonium in soil layer L, ppm
! NLAYR     !Number of soil layers
! nlayr_nw  !WHAPS version, number of soil layers
! NO3(20)   !Nitrate in soil layer L, ppm
! PLTPOP    !Plant population, pl/m2
! RLDF(20)  !A root length density factor for soil layer L used to calculate
!           !new root growth distribution - unitless
! RLNEW     !New root length to be added to the total root system length, cm. root cm2 ground
! RLV(20)   !Root length density, cm root/cm2 soil
! RLWR      !Root length to weight ratio, cm/g
! RNFAC     !Zero to unity factor describing mineral N availability effect on
!           !root growth in Layer L
! RNLF      !Intermediate factor used to calculate distribution of new root
!           !growth in the soil - unitless value between 0 and 1
! RTDEP     !Rooting depth (cm), Initially set at emergence
! SDEPTH    !Sowing depth, cm
! SHF(20)   !Relative root distribution in soil layer L (0-1)
! STGDOY(20)!Year and day of year that a growth stage occurred on
! SW(20)    !Volumetric soil water content of soil layer L, cm3/cm3
! SWDF      !Soil water deficit factor for Layer L used to calculate root
!           !growth and water uptake - unitless value between 0 and 1
! SWFAC     !Soil water stress effect on growth (0-1), 1 is no stress, 0 is full stress
! TRLDF     !An intermediate calculation used to calculate distribution of new root growth in soil
! YRDOY     !Year and day of year



