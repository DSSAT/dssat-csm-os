!======================================================================
!  WH_temp, subroutines
!
!  WHAPS subroutines
!----------------------------------------------------------------------
!  Revision history
!  11/01/2021 FO Added missing CONTROL type for nwheats_* subroutines
!  11/01/2021 FO Added ERRKEY paramter for all nwheats_* subroutines
!  01/18/2022 TF Added statments to prevent divisions by zero
!----------------------------------------------------------------------

! JZW note: need to read p_root_n_min, p_init_grain_nconc, g_uptake_source='apsim' or 'calc'
!p_min_grain_nc_ratio = min_grain_nc_ratio, p_max_n_uptake
! where is pndem? it is ndmd
! uptake_no3 is soil pool/property variables ??
!The following is line 1546 in *tmp
! JG moved some CUL parameters to ECO file 01/21/2020
! JG moved ozone parameters to ECO file 07/24/2020
! JG cleaned ozone parameters in ECO file 01/18/2022

*     ===========================================================
      !*! real function nwheats_fac (layer)
      real function nwheats_fac (layer, SOILPROP)
*     ===========================================================
      USE ModuleDefs
      implicit none
      !*! include 'nwheats.inc'             ! CERES_Wheat Common Block
      !*! include 'data.pub'                          
      !*! include 'error.pub'                         

*+  Sub-Program Arguments
      integer    layer                 ! (INPUT) soil layer counter

*+  Purpose
*      convert kg/ha to ppm of soil in a given layer

*+  Changes
*        310393   specified and programmed jngh (j hargreaves

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'nwheats_fac')
      real BD(NL)! Bulk density (moist)           g/cm3
      real dlayr_nw(NL)
      TYPE (SoilType)    SOILPROP
      dlayr_nw  = SOILPROP % DLAYR  * 10.0 
      bd = SOILPROP % BD

*- Implementation Section ----------------------------------
 
      !*! call push_routine (myname)
 
              ! calculate conversion factor from kg/ha to ppm (mg/kg)
 
!     This should be handled in the soil dynamics routine.
      !!*! nwheats_fac = divide (100.0, bd(layer)*dlayr(layer), 0.0)
      !! DSSAT: KG2PPM(1) = 1.0/(BD1*1.E-01*DLAYR(1))

      nwheats_fac = 100.0 /( bd(layer)*dlayr_nw(layer))
!         mg[N]      100             ha        m2      1000000mg   mm
!        ------   =------------ * --------*---------- * ---------*-------
!         kg[soil]  mm*  g/cm3     10000m2  10000cm2    kg         0.1cm
!      *! call pop_routine (myname)
      return
      end



*     ===========================================================
      subroutine nwheats_germn (sdepth, stgdur, swdep,          !Input
     &   dlayr_nw, lldep,                                       !Input
     &   stagno)	                                              !Output
*     ===========================================================
      USE ModuleDefs
      USE WH_module
      implicit none
      EXTERNAL nwheats_level
      !*! include 'nwheats.inc'             ! CERES_Wheat Common Block
      !*! include 'data.pub'                          
      !*! include 'error.pub'                         

*+  Sub-Program Arguments    *****OK******
      integer    stagno            ! (OUTPUT) phenological stage number

*+  Purpose
*      determine germination date

*+  Notes
*     there may be problems with the weighting of the mean.  The weighting
*     should vary with the distance the sowing depth is from the top of the
*     next layer.  If the sowing occurrs in a lower layer than the top one,
*     then perhaps the proximity to the bottom of the layer above should
*     be taken into account.  I this a fudge to take account of factors
*     that water balance doesn't do? jngh 180892

*+  Changes
*       020392 jngh specified and programmed
*       180892 jngh included sowing - cr460(1)
*                   included sowing as criteria for germination - cr460(2)
*                   documented weighting problems in notes. - cr460(3)
*       290892 jngh changed soil water to depth of water

*+  Calls
      integer    nwheats_level          ! function

*+  Constant Values
      real       grmsw            ! required mean plant extractable soil
      parameter (grmsw = 0.02)    !   water for germination (mm/mm)
*
      real       grmsw0           ! plant extractable soil water in
      parameter (grmsw0 = 0.0)    !   seedling layer inadequate for
                                  !   germination (mm/mm)
*
      character  myname*(*)       ! name of subroutine
      parameter (myname = 'nwheats_germn')
*
      real       wtmean                ! weight for weighted mean
      parameter (wtmean = 0.65)

*+  Local Variables    *******OK******
      integer    slayer ! seedling layer number
      real       spesw  ! weighted mean plant extractable soil water 
                        ! (mm/mm) available for germination (mm/mm)


      real       spesw0 ! plant extractable soil water in seedling layer
                        ! available for germination ( mm/mm)


      real       spesw1 ! plant extractable soil water 
                        ! below seedling layer ( mm/mm)
      ! JZW add the following
      integer stgdur(20)
      real  sdepth, dlayr_nw(NL), lldep(NL), swdep(NL)
      !TYPE (SoilType)    SOILPROP
      !dlayr_nw  = SOILPROP % DLAYR  * 10.0  
      !lldep     = SOILPROP % LL     
         
*- Implementation Section ----------------------------------
 
 
      !*! call push_routine (myname)
 
!           determine if soil water content is sufficient to allow germination.
!           either soil water content of the seeded layer must be > the
!           lower limit or a weighted average of the water content of the
!           seeded layer and next layer below must be adequate for germination.
 
      if (      stagno.eq.sowing
     :    .and. stgdur(sowing).ge.1) then
         !*! slayer = nwheats_level (sdepth)
         slayer = nwheats_level (sdepth, dlayr_nw, NL)
 !*!        spesw0 = divide (swdep(slayer) - lldep(slayer)
 !*!    :                   , dlayr(slayer), 0.0)

         spesw0 =  (swdep(slayer) - lldep(slayer))
     :                   / dlayr_nw(slayer)
 
  !*!       spesw1 = divide (swdep(slayer+1) - lldep(slayer+1)
  !*!   :                   , dlayr(slayer+1), 0.0)
         spesw1 = (swdep(slayer+1) - lldep(slayer+1))
     :                   / dlayr_nw(slayer+1)
         spesw =  spesw0*wtmean + spesw1*(1.0 - wtmean)
 
         if (spesw0.gt.grmsw0 .or. spesw.ge.grmsw) then
               ! we have germination
            stagno = germ
 
         else
                ! no germination yet
         endif
      else
             ! no sowing yet ! JZW do we need to set stango=0?
      endif
 
      !*! call pop_routine (myname)
      return
      end
*     ===========================================================
      !*! subroutine nwheats_gndmd (gndmd)        
      subroutine nwheats_gndmd (Istage, tempmx, tempmn,    !Input 
     &     dtt, gpp,                                       !Input
     &     gndmd)                                         !Output
*     ===========================================================
      USE WH_module
      implicit none
!*!      include 'nwheats.inc'             ! CERES_Wheat Common Block
!*!      include 'data.pub'                          
!*!      include 'error.pub'                         

*+  Sub-Program Arguments  *****OK*****
      real gndmd    ! (OUTPUT) grain N demand (g/plant)

*+  Purpose
*           calculate the actual grain nitrogen demand

*+  Changes
*       020392 jngh specified and programmed
*       141093 jngh removed grain nitrogen concentration demand to a function.

*+  Constant Values
      real ug2g     ! convert micro g to g
*
      parameter (ug2g = 1.e-6)
*
      character  myname*(*)  ! name of subroutine
      parameter (myname = 'nwheats_gndmd')

*+  Local Variables	  *****OK*****
      real rgnfil
      real tempm
      Real tempmx, tempmn, dtt, gpp, g_gndmd, g_rgnfil ! add by JZW 
      
      INTEGER ISTAGE  ! add by JZW, assume grnfil is integer
*- Implementation Section ----------------------------------
 
  !*! call push_routine (myname)
 
             ! calculate the grain N demand
      if (istage .eq. grnfil) then
         tempm = (tempmx + tempmn)/2.0
 
         if (tempm .gt. 10.0) then
            rgnfil = 4.829666 - 3.2488*dtt + 0.2503*(tempmx-tempmn)
     :               + 4.3067 * tempm
 
         else
            rgnfil = 0.49 * tempm
 
         endif
         !*! rgnfil = l_bound (rgnfil, 0.0)
         rgnfil = max(rgnfil, 0.0)
         gndmd  = rgnfil * gpp * ug2g
 
cnh Senthold
c moved to calling routine
cbak
!       delta_grainc is the daily increment in grain weight (after stress)
!       check to see if the ratio of delta n /delta c is too high
!       that is, c stops but n continues. set max limit of 0.10
 
!*!       write (*,*) 'day=',day_of_year,'grain n fill =',gndmd
 
cnh         delta_grainC = growt(grain)  + transwt(grain)
 
!*!       write (*,*) 'day=',day_of_year,'c increase=',delta_grainc
 
cnh         delta_N_fraction = divide (gndmd,delta_grainC,0.0)
cnh         delta_N_fraction = u_bound (delta_N_fraction
c     :                              ,p_max_grain_nc_ratio)
cnh         gndmd = delta_N_fraction * delta_grainC
 
!*!       write (*,*) 'day=',day_of_year,'ratio=',delta_n_fraction
!*!       write (*,*) 'day=',day_of_year,'adj n fill =',gndmd
 
 
       else
         ! No demand for Grain N outside of grain filling
         gndmd = 0.0
         rgnfil = 0.0
      endif
 
cnh bad programming practice to set globals here but this is for
cnh to allow watching of these variables
        g_gndmd = gndmd
        g_rgnfil= rgnfil
 
      !*! call pop_routine (myname)
      return
      end

*     ==================================================================
      ! find actual grain uptake by translocation
      subroutine nwheats_grnit (CONTROL,                  !Input
     &        Istage, dtt, gpp, gro_wt, mnc, MXNCR, nfact,        !Input
     &        nitmn, npot, optfr, part, pl_la, pl_nit,            !Input
     &        plantwt, sen_la, tempmn, tempmx, trans_wt,          !Input
     &        pnout)                                             !Output

! 2023-01-18 CHP removed unused variables from argument list:
!  ISWITCH,

*     ==================================================================

      USE ModuleDefs
      USE WH_module
      implicit none
      EXTERNAL nwheats_gndmd, GrN_Ptl, sum_real_array, warning, ERROR, 
     &  count_of_real_vals
      !*! include 'nwheats.inc'             ! CERES_Wheat Common Block
      !*! include 'data.pub'                          
      !*! include 'error.pub'                         

*+  Sub-Program Arguments      ******OK*******
!      *! real       pnout (*)             ! (OUTPUT) plant N taken out from plant parts ()
      real pnout (mxpart)
      CHARACTER*78 MSG(1)

*+  Purpose
*              calculate the nitrogen uptake from the various plant parts
*              to the grain.

*+  Changes
*       020392 jngh specified and programmed
*       260892 jngh corrected uptake from roots - cr477
*                   corrected N uptake when tops N is adequate - cr475
*                   added check that calculations are not wild.
*                   removed subtraction of grain N avail. - cr473

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'nwheats_grnit')
      CHARACTER*6 ERRKEY
      PARAMETER (ERRKEY = 'NWGRNI')

*+  Local Variables	 ******OK*******
      real gndmd         ! grain N demand
      real gnuptk        ! actual grain N uptake 
      real navil         ! total N available for transfer to grain 

      real navl (mxpart) ! N available for transfer to grain

      integer part       ! plant part number
      real tnavil        ! total N available in top (stover)




      real delta_N_fraction ! N as proportion of daily C transfer to
              ! today's increment in C in grain (growth + translocation)
      real delta_grainC     
      ! JZW add the following
      real gro_wt(mxpart), trans_wt (mxpart) 
!       Real p_max_grain_nc_ratio ! JZW get from crop.apr: ! JG replaced with MXNCR 7/23/20
!       max_grain_nc_ratio = 0.04;  0.035=20%;  .04=23% protein, max n:c ratio of grain growth 
!       parameter (p_max_grain_nc_ratio = 0.04) ! JG replaced with MXNCR 7/23/20
      Real MXNCR  ! JG added 7/23/20
      real sum_real_array 
!     real rgnfil
      REAL g_navl(mxpart) 
      REAL mnc(mxpart)  ! Minimum N concentration, by plant part
      INTEGER ISTAGE 
      Real tempmx, tempmn, dtt, gpp  !, g_gndmd, g_rgnfil
      REAL nfact(10) ! Nstress sensitivity by plant organ (0-1)
      REAL nitmn  ! nitrogen minimum level
      REAL npot   ! potential N available for transfer  (g/part) 
      real optfr ! fraction of optimum conditions from stress (0-1)
      REAL pl_la    ! Plant leaf area, mm2/plant         nwheat
      REAL pl_nit(mxpart) !plant part nitrogen (g/plant)  nwheat
      REAL plantwt(mxpart) 
      REAL sen_la   ! Senesced leaf area, mm2/plant      nwheat
      TYPE (ControlType) CONTROL
!     TYPE (SwitchType) ISWITCH
      
*- Implementation Section ----------------------------------
 
      !*! call push_routine (myname)
 
              ! -------------- get grain N demand -----------
 
      !*! call nwheats_gndmd (gndmd)
         call nwheats_gndmd (Istage, tempmx, tempmn, dtt, gpp, !Input
     &   gndmd)                                               !Output
!       delta_grainc is the daily increment in grain weight (after stress)
!       check to see if the ratio of delta n /delta c is too high
!       that is, c stops but n continues. set max limit of 0.10
        !*! delta_grainC = growt(grain)  + transwt(grain)
         delta_grainC =gro_wt(grain_part)  + trans_wt(grain_part)
         !*! delta_N_fraction = divide (gndmd,delta_grainC,0.0)
         if (delta_grainC .gt. 0.) then
           delta_N_fraction = gndmd / delta_grainC
         else
           delta_N_fraction = 0. 
!          JZW add this case in Oct, 2014. On 1st day of emergence, goes here
         endif
         
!          JG replaced p_max_grain_nc_ratio with MXNCR 7/23/20
!         delta_N_fraction = u_bound (delta_N_fraction
!     :                              ,p_max_grain_nc_ratio)
         delta_N_fraction = min(delta_N_fraction
     &                              ,MXNCR)     
         gndmd = delta_N_fraction * delta_grainC
 
!               -------------- get grain N potential (supply) -----------
 
      !*! call nwheats_gnptl (navl) 
      ! FSR Created from NWheat subroutine "nwheats_gnptl"
      !Calculates N available for transfer to grain (g/plant) from each
      !       plant part. 
      Call GrN_Ptl (CONTROL, 
     &   mnc, nfact, nitmn, npot, optfr, part, pl_la, pl_nit,     !INPUT
     &   plantwt, sen_la,                                         !INPUT
     &   navl)            
      navil  =  sum_real_array (navl, mxpart)
!   tops n avail
      tnavil = navil - navl(root_part)
cnh added for watch purposes
      do 10 part=1,mxpart
         g_navl(part) = navl(part)
   10 continue
 
              ! -------------- get actual grain N uptake -----------
 
                   ! check if navil has inadequate supply of N
                   ! for grain N demand.
       !gnuptk is actual grain N uptake (g/plant) intermediate data
      !*! gnuptk = u_bound (gndmd, navil)
      gnuptk = min(gndmd, navil)
      if (gndmd .le. 0.0) then
         ! No demand so no supply required
         !*! call fill_real_array (pnout, 0.0, mxpart)
             pnout = 0.
      else if (gnuptk.ge.tnavil) then
 
             ! leaf and stem N inadequate to supply grain N, so take
             ! excess from roots
 
         !*! call fill_real_array (pnout, 0.0, mxpart)
         pnout = 0.
         pnout(leaf_part) = navl(leaf_part)
         pnout(stem_part) = navl(stem_part)
         pnout(lfsheath_part) = navl(lfsheath_part) 
!         JZW run EEST, 1938198, when reach here, pnout(sten_part) is not assigned, until finish endif
!         navl(stem_part) was 5.3382293*10-5, pnout(stem_part) is 5.338304x10-5
 
! take the rest of the grain n uptake from the roots
         pnout(root_part)  =  gnuptk - tnavil
 
! grain n uptake > zero but < tot n available for uptake
      elseif (gnuptk.gt.0.0) then
 
             ! there is adequate N in stem and leaf to supply grain N.
 
         !*! call fill_real_array (pnout, 0.0, mxpart)
         pnout = 0.
         pnout(leaf_part) = gnuptk* navl(leaf_part)/tnavil
         pnout(lfsheath_part) = gnuptk* navl(lfsheath_part)/tnavil
         pnout(stem_part) = gnuptk* navl(stem_part)/tnavil
      else 
             ! we have no grain N uptake
         !*! call fill_real_array (pnout, 0.0, mxpart)
         pnout = 0.
      endif
             ! just check that we got the maths right.
!*!      write(*,*) 'grain n uptake=',gnuptk,'day=', day_of_year
!      lai = (pl_la - sen_la)*plants /1000000
!*!      write (*,*) 'lai =', lai
!      grainnpc = divide (pl_nit(grain), pl_wt(grain), 0.0)*100.
!*!      write (*,*) 'grain n % =', grainnpc
      do 1000 part = 1, mxpart
!         call bound_check_real_var (pnout(part), 0.0, !JZW need to solve soon
!     :        navl(part), 'pnout(part)', 'Grnit') !'Grnit is error key

          if (pnout(part) .lt. 0.) then
             msg(1) = "pnout bound check <0 error"
             call warning(1, ERRKEY, msg)
             call error(ERRKEY,99," ",0)
          endif

          if  (pnout(part). gt. navl(part) ) then
!               pnout is pntrans calculated by nwheats_grnit for finding actual grain uptake by translocation
!               navl (mxpart) is N available for transfer to grain
              msg(1) = "pnout bound check >navl error"
              call warning(1, ERRKEY, msg)
              pnout(part) = navl(part) !JZW add this case in Oct, 2014
          endif
1000  continue
 
      !*! call pop_routine (myname)
      return
      end
*     ===========================================================
      !*! subroutine nwheats_lyrck (layer)	  
      subroutine nwheats_lyrck (layer, dlayr_nw)	
*     ===========================================================
      USE ModuleDefs
      implicit none
      EXTERNAL count_of_real_vals, warning
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
      !JZW add the following
      REAL dlayr_nw(NL) 
      integer count_of_real_vals
      CHARACTER*6, PARAMETER :: ERRKEY = 'NWLYRC'
*- Implementation Section ----------------------------------
      !*! call push_routine(my_name)
 
      !*! nlayr = count_of_real_vals (dlayr, mxlayr)
      nlayr = count_of_real_vals (dlayr_nw, NL)
      if (layer.lt.minlyr) then
         write (e_messg,'(2(a,i3))')
     :                         ' Soil layer no. ', layer,
     :                         ' is below mimimum of ', minlyr
         !*! call warning_error (err_user, e_messg)
         call warning (1, ERRKEY, e_messg)
      else if (layer.gt.nlayr) then
         write (e_messg,'(2(a,i3))')
     :                         ' Soil layer no. ', layer,
     :                         ' is above maximum of ', nlayr
         !*! call warning_error (err_user, e_messg)
         call warning (1, ERRKEY, e_messg)
      endif
      !*! call pop_routine(my_name)
 
      return
      end

*     ===========================================================
!*!   subroutine nwheats_ndmd (ndmd)
      subroutine nwheats_ndmd (xstag_nw,  gro_wt, plantwt, !Input
     &      pl_nit, pcarbo, carbh, cnc,                    !Input 
     &      pndem)	                                      !Output
*     ===========================================================
      USE WH_module
      implicit none
      external sum_real_array, error, warning
      !*! include 'nwheats.inc'             ! CERES_Wheat Common Block
      !*! include 'data.pub'                          
      !*! include 'error.pub'                         

*+  Sub-Program Arguments    *****OK*****
      real       pndem (mxpart)       ! (OUTPUT) plant nitrogen demand
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
      character  myname*(*) ! name of subroutine
      parameter (myname = 'nwheats_ndmd')
      character*78 msg(1)
      CHARACTER*6 ERRKEY
      PARAMETER (ERRKEY = 'NWNDMD')
*
      real       tolnce     ! tolerance for calculation errors
      parameter (tolnce = 1.0e-5)

*+  Local Variables	  *****OK*****
      real       cnit       ! critical N amount  (g/plant part)
      real       demnew     ! demand for N by new growth (g/plant part)
     
      real       demold     ! demand for N by old biomass (g/plant part)
      
      integer    part       ! plant part
      real       pgro(mxpart) ! potential DW increase (g/plant part)
      !The following are add by jzw
      REAL gro_wt(mxpart), cnc(mxpart), pl_nit(mxpart)  
      REAL plantwt(mxpart)
!*!   real pcarb  ! same as pcarbo; only pcarbo used in WHAPS 
      REAL pcarbo !Potential CH2O output (production) (gDM/plant)
      REAL carbh, xstag_nw
      real sum_real_array
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
         !*! pgro(part) = pcarb* (growt(part)/ carbo)
          if (carbh .eq. 0.) then
              msg(1) = "carbh is zero"
              call warning(1,ERRKEY, msg)
              call error(ERRKEY,99," ",0)
          endif
         pgro(part) = pcarbo* (gro_wt(part)/ carbh)
         !*! pgro(part) = bound (pgro(part), 0.0, pcarb)
         pgro(part) = max (pgro(part), 0.0)
         pgro(part) = min (pgro(part), pcarbo)
500   continue
 
      if (xstag_nw.le.1.2) then
         !*! pgro(stem) = 0.0
         pgro(stem_part) = 0.0
         !*! pgro(leaf) = 0.0
         pgro(leaf_part) = 0.0
         !*! pgro(lfsheath) = 0.0
         pgro(lfsheath_part) = 0.0
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
         if (plantwt(part).gt.0.0) then
 
            cnit = plantwt(part)*cnc(part)
            demold = cnit - pl_nit(part)
 
 
      ! get potential N demand (critical N) of potential growth
 
            demnew = pgro(part)*cnc(part)
 
            pndem(part) = demold + demnew
 
cjh    this matches the old version after changing it to be the same.
cjh    previously it had no lower limit. however pers. comm with bak, the
cjh    excess n should be translocated. jngh 170993.
        !*!  ndmd(part) = l_bound (ndmd(part), 0.0)
            pndem(part) = max (pndem(part), 0.0)
 
         else
            pndem(part) = 0.0
         endif
 
1000  continue
 
cjh correct parts when total is <= 0
 
      if (sum_real_array (pndem, mxpart).lt.0.0) then
         !*! call fill_real_array (pndem, 0.0, mxpart)
         pndem = 0.
      else
      endif
 
 
cjh  end of correction
      return
      end
*     ==================================================================
      !*! subroutine nwheats_nuptk (snuptk_no3, snuptk_nh4, pnuptk)
      subroutine nwheats_nuptk (CONTROL, SOILPROP,                !Input
     &      carbh, cnc, EXNH4,     EXNO3,                         !Input
     &      g_uptake_source, gro_wt, MNNH4, MNNO3, MXNUP,         !Input
     &      pcarbo, pl_nit,  plantwt, PLTPOP,                     !Input
     &      PNUPR,         rlv_nw, snh4, sno3, swdep,             !Input
     &      WFNU, xstag_nw,                                       !Input
     &      pnup, snup_nh4, snup_no3)                            !Output
*     ==================================================================

      USE ModuleDefs
      USE WH_module
      implicit none
      external count_of_real_vals, getlun, error, ignore, 
     &  nwheats_ndmd, sum_real_array, nwheats_nsupply
      !*! include 'convert.inc'            ! gm2kg, sm2ha
      !*! include 'nwheats.inc'             ! CERES_Wheat Common Block
      !*! include 'data.pub'                          
      !*! include 'error.pub'                         

*+  Sub-Program Arguments	     *****OK*****
      real pnup (mxpart)  ! (OUTPUT) actual plant N uptake into each 
                          !          plant part (g/plant part)  
      real snup_no3 (NL)  ! (OUTPUT) actual plant N uptake from NO3 
                          !          in each layer (kg/ha)
      real snup_nh4 (NL)  ! (OUTPUT) actual plant N uptake from NH4 
                          !          in each layer (kg/ha)

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
      real avail_no3(NL) ! potential NO3 (supply) from soil (g/plant) 
      real avail_nh4(NL) ! potential NH4 (supply) from soil (g/plant)  
      real fr_part     !fraction of nitrogen to use (0-1) for plant part
      integer layer      ! soil layer number of profile
      real n_demand      ! total nitrogen demand (g/plant)
      real n_supply      ! total nitrogen supply (g/plant)
      integer nrlayr     ! number of layers with roots
      integer num_layers
      integer part       ! plant part number
      real pnuptk_tot    ! total plant N uptake (g/plant)
      real scalef        ! scaling factor for uptake (0-1)
      real capped_n_demand
      ! JZW add the following
      REAL xstag_nw
      REAL plantwt(mxpart)
      REAL gro_wt(mxpart)
      REAL pl_nit(mxpart)
      REal cnc(mxpart)
!     real       cnit
      real pcarbo
      REAL            carbh
      REAL            dlayr_nw(NL), swdep(NL)
      REAL            rlv_nw(NL) 
      real sno3(NL), snh4(NL)
      real uptake_no3(NL), uptake_nh4(NL)
      real pndem(mxpart)
      real sum_real_array
      REAL PLTPOP
      real EXNH4, MNNH4, EXNO3, MNNO3, WFNU, PNUPR
      real MXNUP
!       real p_max_n_uptake  , btk 03/02/2017
!       PARAMETER (p_max_n_uptake = .6)  !(g/m2) max N uptake per day ; commented out by btk, 03/02/2017
      character*5 G_UPTAKE_SOURCE
      Integer count_of_real_vals
      REAL  ha2sm
      PARAMETER   (ha2sm = 10000)
      REAL  gm2kg
      PARAMETER   (gm2kg = 0.001)   ! conversion factor from NWheat.
      
      ! JG added for ecotype file
      INTEGER         DYNAMIC         
      CHARACTER*12    FILEE 
      CHARACTER*92    FILEGC
      CHARACTER*1     BLANK 
      PARAMETER (BLANK = ' ')
      CHARACTER*6     ECOTYP
      CHARACTER*355   C255  ! JG increased for large ecotype file
      CHARACTER*16    ECONAM
      CHARACTER*6     ECONO 
      CHARACTER*6     ERRKEY   
      INTEGER     ERRNUM
      INTEGER     ISECT
      INTEGER     PATHL
      INTEGER     LNUM
      INTEGER     LUNECO
      INTEGER     NOUTDO
      CHARACTER*80    PATHER 
      REAL        TBASE,TOPT,ROPT,TTOP, P2O,VREQ,GDDE,DSGFT,RUE1,RUE2
      REAL        KVAL1,KVAL2,SLAP2,TC1P1,TC1P2,DTNP1,PLGP1,PLGP2
      REAL        P2AF,P3AF,P4AF,P5AF,P6AF,ADLAI,ADTIL,ADPHO,STEMN
      REAL        MXNCR,INGWT,INGNC,FREAR,MNNCR,GPPSS,GPPES,MXGWT
      REAL        MNRTN,NOMOB,RTDP1,RTDP2,FOZ1,SFOZ1
      ! JG end for ecotype variables
      
C     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL
      TYPE (SoilType)    SOILPROP
      
      PARAMETER (ERRKEY = 'NWNPTK')
      
!  JG added section to read ecotype file
!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
!----------------------------------------------------------------------
!         DYNAMIC = RUNINIT OR DYNAMIC = SEASINIT
! ---------------------------------------------------------------------
      IF (DYNAMIC.EQ.RUNINIT .OR. DYNAMIC.EQ.SEASINIT) THEN

!       Do this just once in RUNINIT
        IF (DYNAMIC .EQ. RUNINIT) THEN
          CALL GETLUN('OUTO', NOUTDO)

!-----------------------------------------------------------------------
!     Open Ecotype File FILEE
!-----------------------------------------------------------------------
          LNUM = 0
          PATHL  = INDEX(PATHER,BLANK)
          IF (PATHL .LE. 1) THEN
            FILEGC = FILEE
          ELSE
            FILEGC = PATHER(1:(PATHL-1)) // FILEE
          ENDIF

!-----------------------------------------------------------------------
!    Read Ecotype Parameter File
!-----------------------------------------------------------------------
          CALL GETLUN('FILEE', LUNECO)
          OPEN (LUNECO,FILE = FILEGC,STATUS = 'OLD',IOSTAT=ERRNUM)
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEE,0)
          ECOTYP = '      '
          LNUM = 0
          DO WHILE (ECOTYP .NE. ECONO)
            CALL IGNORE(LUNECO, LNUM, ISECT, C255)
            IF (ISECT .EQ. 1 .AND. C255(1:1) .NE. ' ' .AND.
     &            C255(1:1) .NE. '*') THEN
              READ(C255,3100,IOSTAT=ERRNUM) ECOTYP,ECONAM,TBASE,TOPT,
     &             ROPT,TTOP, P2O,VREQ,GDDE,DSGFT,RUE1,RUE2,KVAL1,KVAL2,
     &             SLAP2,TC1P1,TC1P2,DTNP1,PLGP1,PLGP2,P2AF,P3AF,P4AF,
     &             P5AF,P6AF,ADLAI,ADTIL,ADPHO,STEMN,MXNUP,MXNCR,WFNU,
     &             PNUPR,EXNO3,MNNO3,EXNH4,MNNH4,INGWT,INGNC,FREAR,
     &             MNNCR,GPPSS,GPPES,MXGWT,MNRTN,NOMOB,RTDP1,RTDP2,
     &             FOZ1,SFOZ1
3100          FORMAT (A6,1X,A16,1X,48(F6.0))
              IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEE,LNUM)
        
            ELSEIF (ISECT .EQ. 0) THEN
              CALL ERROR(ERRKEY,7,FILEE,LNUM)
            ENDIF
          ENDDO
          CLOSE (LUNECO)
        ENDIF
      ENDIF
!  JG end of ecotype addition
      
      dlayr_nw = SOILPROP % DLAYR * 10.
*- Implementation Section ----------------------------------
 
      !*! call push_routine (myname)
 
         !*! call fill_real_array (snuptk_no3, 0.0, mxlayr)
         snup_no3 = 0.0
         !*! call fill_real_array (snuptk_nh4, 0.0, mxlayr)
         snup_nh4 = 0.0
!             N demand by plant
 
!             pndem is in common and is being changed here
!             because it is needed for cumulative totals. It is not a state.
!             Needs to be fixed ????????????????? JNGH 230693
!             State variables must be only changed by deltas at the top level,
!             and deltas can be changed at lower levels if they are in
!             common blocks. Is this acceptable ????
      !*! call nwheats_ndmd (pndem)
* ====================================================================
      CALL nwheats_ndmd (xstag_nw,  gro_wt, plantwt,       !Input
     &     pl_nit, pcarbo, carbh, cnc,                     !Input 
     &     pndem)	                                       !Output
* ====================================================================
      n_demand = sum_real_array (pndem, mxpart)
 
      if (g_uptake_source .eq. 'calc') then
!             First we put a cap on the total uptake that can occur
!             on any given day by bounding the demand. Note: we do
!             not bound the "real" n demand, just the value here.
!         *! capped_n_demand = u_bound(n_demand,p_max_n_uptake/plants)
!          capped_n_demand = min(n_demand,p_max_n_uptake/PLTPOP) !  btk 03/02/2017
!         Added to void divisions by zero (TF - 01/18/2022)
          IF (PLTPOP .GT. 0.0) THEN 
!           changed by btk 03/02/2017
            capped_n_demand = min(n_demand,MXNUP/PLTPOP) 
          ELSE
            capped_n_demand = 0
          ENDIF             
         !                                  g/m2
         !          g   =                 ______
         !                               plant/m2
            ! find potential N uptake (supply, available N)
         !*! call nwheats_nsupply (avail_NO3, avail_NH4)
* ====================================================================
         call nwheats_nsupply (CONTROL, SOILPROP, rlv_nw,       !Input
     &     sno3, snh4, swdep, EXNH4, MNNH4, EXNO3,              !Input
     &     MNNO3, WFNU, PNUPR,                                  !Input
     &     avail_no3, avail_nh4)                               !Output
* ====================================================================
           
         !*! nrlayr = count_of_real_vals (rlv, mxlayr)
         nrlayr = count_of_real_vals (rlv_nw, NL)
         n_supply = sum_real_array (avail_no3, nrlayr)
     :            + sum_real_array (avail_nh4, nrlayr)
 
!*!         scalef = divide (capped_n_demand
!*!     :                   ,n_supply/ha2sm/gm2kg/plants
!*!     :                   ,0.0)
         if (n_supply .gt. 0.) then
!          Added to void divisions by zero (TF - 01/18/2022)
           IF (PLTPOP .GT. 0.0) THEN 
            scalef = capped_n_demand/(n_supply/ha2sm/gm2kg/PLTPOP)
           ELSE
            scalef = 0
           ENDIF
        !scalef = (capped_n_demand/n_supply)*ha2sm *  gm2kg  * PLTPOP
         !             g /plant             10000m2  0.001*kg   plant
         !      = (-------------------) *  -------* -------- *------
         !            kg/ha                  ha        g        m2
         !*! scalef = bound (scalef, 0.0, 1.0)
           scalef = max(scalef, 0.0)
           scalef = min(scalef, 1.0)
         else 
           scalef = 0. !JZW add this case in Oct, 2014
         endif
         pnuptk_tot = 0.0
 
         do 1100 layer = 1,nrlayr
 
            snup_no3(layer) = avail_no3(layer) * scalef
            snup_nh4(layer) = avail_nh4(layer) * scalef
 
            pnuptk_tot = pnuptk_tot + snup_no3(layer)
     :                              + snup_nh4(layer)
1100     continue
      else
         !*! num_layers = count_of_real_vals (dlayr, mxlayr)
         num_layers = count_of_real_vals (dlayr_nw, NL)
         do 1200 layer = 1, num_layers
            snup_no3(layer) = uptake_no3(layer)
            snup_nh4(layer) = uptake_nh4(layer)
 
            pnuptk_tot = pnuptk_tot + snup_no3(layer)
     :                              + snup_nh4(layer)
 
 1200    continue
 
      endif
               ! find proportion of uptake to be
               ! distributed to to each plant part and distribute it.
 
      do 1300 part = 1, mxpart
 
         !*! fr_part = divide (pndem(part), n_demand, 0.0)
         ! To avoid devide zero, JZW add the if statement
         if (n_demand .GE. 0.000001) then 
            fr_part = pndem(part) / n_demand
         else 
            fr_part = 0.
         endif
         ! pnuptk_tot is total plant N uptake (g/plant) Wrong ????
         !*! pnuptk (part) = pnuptk_tot*fr_part/ha2sm/gm2kg/plants
         ! Added IF statment to void divisions by zero (TF - 01/18/2022)
         IF (PLTPOP .GT. 0.0) THEN
            pnup (part) = pnuptk_tot*fr_part/ha2sm/gm2kg/PLTPOP
         ELSE
            pnup (part) = 0
         ENDIF         !    g           kg                      1
!          --------  = ----------*     -------------------------------------- 
!           plant         ha          (10000m2/ha) * (0.001kg/g) * (plant/m2)
1300  continue
 
      !*! call pop_routine (myname)
      return
      end
* ====================================================================
!*!    subroutine nwheats_nsupply(avail_no3, avail_nh4)
       subroutine nwheats_nsupply(CONTROL, SOILPROP, rlv_nw,    !Input
     &     sno3, snh4, swdep, EXNH4, MNNH4, EXNO3,              !Input
     &     MNNO3, WFNU, PNUPR,                                  !Input
     &     avail_no3, avail_nh4)                               !Output
* ====================================================================
      USE ModuleDefs
      implicit none
      EXTERNAL GETLUN, ERROR, IGNORE, COUNT_OF_REAL_VALS, NWHEATS_FAC
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
      REAL  gm2kg
      PARAMETER   (gm2kg = 0.001)   ! conversion factor from NWheat.
      integer count_of_real_vals
      real duldep(NL), lldep(NL), satdep(NL), snh4(NL), sno3(NL)
      real rlv_nw(NL), swdep(NL), dlayr_nw(NL)
      real nh4mn(NL) ! nh4ppm_min, min allowable NH4 (ppm) soil n init
      real no3mn(NL) ! no3ppm_min, minimum allowable NO3 (ppm
      PARAMETER   (nh4mn = 0.0) ! from APSIM soiln.ini
      PARAMETER   (no3mn = 0.0) ! from APSIM soiln.ini
      real EXNH4 
      real MNNH4 
      real EXNO3 
      real MNNO3 
      real WFNU  
      real PNUPR 
!EXNH4 = APSIM p_enupf_nh4 exponent for NH4 supply factor, FSR: change this unwieldy parameter by a factor of 100
!MNNH4 = APSIM p_mnupf_nh4, minimum for NH4 supply factor
!EXNO3 = APSIM p_enupf_no3, exponent for NO3 supply factor, FSR: change this unwieldy parameter by a factor of 100
!MNNO3 = APSIM p_mnupf_no3,  minimum for NO3 supply factor
!WFNU  = APSIM p_wfnu_power,  power term for water effect on N supply
!PNUPR = APSIM p_pot_nuprate potential uptake rate (g/mm root/day)  [FSR: change this unwieldy parameter to 0.45 mg/m]
     
      ! JG added for ecotype file
      INTEGER         DYNAMIC         
      CHARACTER*12    FILEE 
      CHARACTER*92    FILEGC
      CHARACTER*1     BLANK 
      PARAMETER (BLANK = ' ')
      CHARACTER*6     ECOTYP
      CHARACTER*355   C255  ! JG increased for large ecotype file
      CHARACTER*16    ECONAM
      CHARACTER*6     ECONO 
      CHARACTER*6     ERRKEY   
      INTEGER     ERRNUM
      INTEGER     ISECT
      INTEGER     PATHL
      INTEGER     LNUM
      INTEGER     LUNECO
      INTEGER     NOUTDO
      CHARACTER*80    PATHER 
      REAL        TBASE,TOPT,ROPT,TTOP, P2O,VREQ,GDDE,DSGFT,RUE1,RUE2
      REAL        KVAL1,KVAL2,SLAP2,TC1P1,TC1P2,DTNP1,PLGP1,PLGP2
      REAL        P2AF,P3AF,P4AF,P5AF,P6AF,ADLAI,ADTIL,ADPHO,STEMN
      REAL        MXNUP,MXNCR,INGWT,INGNC,FREAR,MNNCR,GPPSS,GPPES
      REAL        MXGWT,MNRTN,NOMOB,RTDP1,RTDP2,FOZ1,SFOZ1
      TYPE (ControlType) CONTROL
      ! JG end for ecotype variables
      
      TYPE (SoilType)  SOILPROP
      
      PARAMETER (ERRKEY = 'NWNSUP')
      
!  JG added section to read ecotype file
!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
!----------------------------------------------------------------------
!         DYNAMIC = RUNINIT OR DYNAMIC = SEASINIT
! ---------------------------------------------------------------------
      IF (DYNAMIC.EQ.RUNINIT .OR. DYNAMIC.EQ.SEASINIT) THEN

!       Do this just once in RUNINIT
        IF (DYNAMIC .EQ. RUNINIT) THEN
          CALL GETLUN('OUTO', NOUTDO)

!-----------------------------------------------------------------------
!     Open Ecotype File FILEE
!-----------------------------------------------------------------------
          LNUM = 0
          PATHL  = INDEX(PATHER,BLANK)
          IF (PATHL .LE. 1) THEN
            FILEGC = FILEE
          ELSE
            FILEGC = PATHER(1:(PATHL-1)) // FILEE
          ENDIF

!-----------------------------------------------------------------------
!    Read Ecotype Parameter File
!-----------------------------------------------------------------------
          CALL GETLUN('FILEE', LUNECO)
          OPEN (LUNECO,FILE = FILEGC,STATUS = 'OLD',IOSTAT=ERRNUM)
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEE,0)
          ECOTYP = '      '
          LNUM = 0
          DO WHILE (ECOTYP .NE. ECONO)
            CALL IGNORE(LUNECO, LNUM, ISECT, C255)
            IF (ISECT .EQ. 1 .AND. C255(1:1) .NE. ' ' .AND.
     &            C255(1:1) .NE. '*') THEN
              READ(C255,3100,IOSTAT=ERRNUM) ECOTYP,ECONAM,TBASE,TOPT,
     &             ROPT,TTOP, P2O,VREQ,GDDE,DSGFT,RUE1,RUE2,KVAL1,KVAL2,
     &             SLAP2,TC1P1,TC1P2,DTNP1,PLGP1,PLGP2,P2AF,P3AF,P4AF,
     &             P5AF,P6AF,ADLAI,ADTIL,ADPHO,STEMN,MXNUP,MXNCR,WFNU,
     &             PNUPR,EXNO3,MNNO3,EXNH4,MNNH4,INGWT,INGNC,FREAR,
     &             MNNCR,GPPSS,GPPES,MXGWT,MNRTN,NOMOB,RTDP1,RTDP2,
     &             FOZ1,SFOZ1
3100          FORMAT (A6,1X,A16,1X,48(F6.0))
              IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEE,LNUM)
        
            ELSEIF (ISECT .EQ. 0) THEN
              CALL ERROR(ERRKEY,7,FILEE,LNUM)
            ENDIF
          ENDDO
          CLOSE (LUNECO)
        ENDIF
      ENDIF
!  JG end of ecotype addition
      
      duldep =  SOILPROP % DUL
      lldep  =  SOILPROP % LL
      satdep =  SOILPROP % SAT
      dlayr_nw  =  SOILPROP % DLAYR * 10.0 
*- Implementation Section ----------------------------------
      !*! call push_routine (myname)
 
      !*! call fill_real_array(avail_no3, 0.0, mxlayr)
      avail_no3 = 0.0
      !*! call fill_real_array(avail_nh4, 0.0, mxlayr)
      avail_nh4 = 0.0
 
      !*! nrlayr = count_of_real_vals (rlv, mxlayr)
      nrlayr = count_of_real_vals (rlv_nw, NL)
 
      do 100 layer = 1, nrlayr
CSenthold
         nh4 = snh4(layer) * nwheats_fac(layer, SOILPROP)
         ! nh4 is in ppm, nwheats_fac is kg2ppm
cbak         fnh4 = 1.0 - exp (-0.025 * (nh4 - 0.5))
cnh         fnh4 = 1.0 - exp (-0.065 * (nh4 - 0.5))
         !*! fnh4 = 1.0 - exp (-p_enupf_nh4 * (nh4 - p_mnupf_nh4))
         fnh4 = 1.0 - exp (- EXNH4 * (nh4 - MNNH4 ))
         !*! fnh4 = bound (fnh4, 0.0, 1.0)
         fnh4 = max (fnh4, 0.0)
         fnh4 = min (fnh4, 1.0)
 
         no3 = sno3(layer) * nwheats_fac(layer, SOILPROP)
cnh         fno3 = 1.0 - exp (-0.0275 * no3)
cbak : adjust to reflect reduced partitioning to roots
!         fno3 = 1.0 - exp (-0.0275 * (no3 - 0.2))
cnh          fno3 = 1.0 - exp (-0.0675 * (no3 - 0.2))
         !*! fno3 = 1.0 - exp (-p_enupf_no3 * (no3 - p_mnupf_no3))
         fno3 = 1.0 - exp (-EXNO3 * (no3 - MNNO3))
         !*! fno3 = bound (fno3, 0.0, 1.0)
         fno3 = max (fno3, 0.0)
         fno3 = min (fno3, 1.0)
         
 
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
 
!          note - the following equations are arranged just as the original
!          code. these need to be rearranged to show meaning.
 
!          units for rlength are mm per ha
!         *! rlength = rlv(layer) * dlayr(layer) / smm2sm / sm2ha
         rlength = rlv_nw(layer) * dlayr_nw(layer) / smm2sm / sm2ha
!           mm       mm                              1
!          ---- = ------------  *    mm          *--------* --------
!           ha       mm3                    0.000001* m2/mm2 0.0001 *ha/m2
cnh         avail_no3(layer) = rlength * fno3 * smdfr**2 * potrate*gm2kg
!*!         avail_no3(layer) = rlength * fno3 * smdfr**p_wfnu_power
!*!     :                      * p_pot_nuprate*gm2kg
         avail_no3(layer) = rlength * fno3 * smdfr**WFNU
     :                      * PNUPR * gm2kg
!            kg               mm                        g          0.001kg
!         --------------- = ------- *      *      * ----------- * ---------
!             ha               ha                     mm[root]*d      g
!         no3ppm_min in soiln.ini is in ppm
         max_avail_no3 = sno3(layer) - no3mn(layer) 
!          kg/ha        = kg/ha   - mg/MG ! NO3MN in *spe is in mg/MG ????????
!         *! avail_no3(layer) = u_bound (avail_no3(layer),max_avail_no3)
         avail_no3(layer) = min (avail_no3(layer),max_avail_no3)
cnh         avail_nh4(layer) = rlength * fnh4 * smdfr**2 * potrate*gm2kg
!*!         avail_nh4(layer) = rlength * fnh4 * smdfr**p_wfnu_power
!*!     :                      * p_pot_nuprate*gm2kg
         avail_nh4(layer) = rlength * fnh4 * smdfr**WFNU
     :                      * PNUPR*gm2kg
         max_avail_nh4 = snh4(layer) - nh4mn(layer)
!         *! avail_nh4(layer) = u_bound (avail_nh4(layer), max_avail_nh4)
         avail_nh4(layer) = min (avail_nh4(layer), max_avail_nh4)
  100 continue
 
      !*! call pop_routine (myname)
      return
      end

*     ===========================================================
      !*! subroutine nwheats_plnin (plantn)
      !Initial PLant N
      subroutine nwheats_plnin (CONTROL, istage, stgdur, plantwt,
 !   &    p_init_grain_nconc, p_root_n_min,                !input
     &    mnc,         INGNC,        MNRTN,                !input
     &    pl_nit )                                      !input & output
*     ===========================================================
      USE ModuleDefs  ! JG added for ecotype file
      USE WH_module
      implicit none
      EXTERNAL GETLUN, ERROR, IGNORE, WARNING
      !*! include 'nwheats.inc'             ! CERES_Wheat Common Block
      !*! include 'data.pub'                          
      !*! include 'error.pub'                         

*+  Sub-Program Arguments  *******OK********
!      *!real       plantn (*)            ! (INPUT/OUTPUT) plant part nitrogen
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
      real avail_root_n, avail_leaf_n, avail_lfsheath_n
      real INGNC  ! p_init_grain_nconc from APSIM
!           .CUL parameter INGNC = initial grain N conc(g N/g biomass) 17.1% protein
      real MNRTN  ! p_root_n_min from APSIM, * 1000
!           .CUL parameter MNRTN = min root n due to grain n initialisation (0=off)
      real root_n_moved, lfsheath_n_moved, leaf_n_moved 
      ! JZW add the following variables
      Integer istage, stgdur(20)
      REAL pl_nit(mxpart) !plant part nitrogen (g/plant)  nwheat
      REAL mnc(mxpart)  ! Minimum N concentration, by plant part
      REAL plantwt(mxpart)

      ! JG added for ecotype file
      INTEGER         DYNAMIC         
      CHARACTER*12    FILEE 
      CHARACTER*92    FILEGC
      CHARACTER*1     BLANK 
      PARAMETER (BLANK = ' ')
      CHARACTER*6     ECOTYP
      CHARACTER*355   C255  ! JG increased for large ecotype file
      CHARACTER*16    ECONAM
      CHARACTER*6     ECONO 
      CHARACTER*6     ERRKEY   
      INTEGER     ERRNUM
      INTEGER     ISECT
      INTEGER     PATHL
      INTEGER     LNUM
      INTEGER     LUNECO
      INTEGER     NOUTDO
      CHARACTER*80    PATHER 
      REAL        TBASE,TOPT,ROPT,TTOP, P2O,VREQ,GDDE,DSGFT,RUE1,RUE2
      REAL        KVAL1,KVAL2,SLAP2,TC1P1,TC1P2,DTNP1,PLGP1,PLGP2
      REAL        P2AF,P3AF,P4AF,P5AF,P6AF,ADLAI,ADTIL,ADPHO,STEMN
      REAL        MXNUP,MXNCR,WFNU,PNUPR,EXNO3,MNNO3,EXNH4,MNNH4,INGWT
      REAL        FREAR,MNNCR,GPPSS,GPPES,MXGWT,NOMOB,RTDP1,RTDP2
      REAL        FOZ1,SFOZ1
      
      PARAMETER (ERRKEY = 'NWPLWI')
      
      TYPE (ControlType) CONTROL
      ! JG end for ecotype variables
      
!  JG added section to read ecotype file
!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
!----------------------------------------------------------------------
!         DYNAMIC = RUNINIT OR DYNAMIC = SEASINIT
! ---------------------------------------------------------------------
      IF (DYNAMIC.EQ.RUNINIT .OR. DYNAMIC.EQ.SEASINIT) THEN

!       Do this just once in RUNINIT
        IF (DYNAMIC .EQ. RUNINIT) THEN
          CALL GETLUN('OUTO', NOUTDO)

!-----------------------------------------------------------------------
!     Open Ecotype File FILEE
!-----------------------------------------------------------------------
          LNUM = 0
          PATHL  = INDEX(PATHER,BLANK)
          IF (PATHL .LE. 1) THEN
            FILEGC = FILEE
          ELSE
            FILEGC = PATHER(1:(PATHL-1)) // FILEE
          ENDIF

!-----------------------------------------------------------------------
!    Read Ecotype Parameter File
!-----------------------------------------------------------------------
          CALL GETLUN('FILEE', LUNECO)
          OPEN (LUNECO,FILE = FILEGC,STATUS = 'OLD',IOSTAT=ERRNUM)
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEE,0)
          ECOTYP = '      '
          LNUM = 0
          DO WHILE (ECOTYP .NE. ECONO)
            CALL IGNORE(LUNECO, LNUM, ISECT, C255)
            IF (ISECT .EQ. 1 .AND. C255(1:1) .NE. ' ' .AND.
     &            C255(1:1) .NE. '*') THEN
              READ(C255,3100,IOSTAT=ERRNUM) ECOTYP,ECONAM,TBASE,TOPT,
     &             ROPT,TTOP, P2O,VREQ,GDDE,DSGFT,RUE1,RUE2,KVAL1,KVAL2,
     &             SLAP2,TC1P1,TC1P2,DTNP1,PLGP1,PLGP2,P2AF,P3AF,P4AF,
     &             P5AF,P6AF,ADLAI,ADTIL,ADPHO,STEMN,MXNUP,MXNCR,WFNU,
     &             PNUPR,EXNO3,MNNO3,EXNH4,MNNH4,INGWT,INGNC,FREAR,
     &             MNNCR,GPPSS,GPPES,MXGWT,MNRTN,NOMOB,RTDP1,RTDP2,
     &             FOZ1,SFOZ1
3100          FORMAT (A6,1X,A16,1X,48(F6.0))
              IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEE,LNUM)
        
            ELSEIF (ISECT .EQ. 0) THEN
              CALL ERROR(ERRKEY,7,FILEE,LNUM)
            ENDIF
          ENDDO
          CLOSE (LUNECO)
        ENDIF
      ENDIF
!  JG end of ecotype addition
      
*- Implementation Section ----------------------------------

cnh need to initialise plant n after these weights are initialised.
      If((Istage.eq.1).and.(Stgdur(istage).eq.1)) then
!*!         plantn(root) = .045*pl_wt(root)
!*!         plantn(stem) = .045*pl_wt(stem)
!*!         plantn(leaf) = .045*pl_wt(leaf)
! JZW replace above to the following
          pl_nit(root_part) = .045*plantwt(root_part)
          pl_nit(stem_part) = .045*plantwt(stem_part)
          pl_nit(leaf_part) = .045*plantwt(leaf_part)
          pl_nit(lfsheath_part) = pl_nit(leaf_part)
          pl_nit(grain_part) = 0.0

      elseif((Istage.eq.grnfil).and.(Stgdur(istage).eq.1)) then
!         avail_stem_n = plantn(stem) - mnc(stem) * pl_wt(stem)
         avail_stem_n = pl_nit(stem_part) - 
     &                  mnc(stem_part) * plantwt(stem_part)
         !*! avail_stem_n = l_bound (avail_stem_n, 0.0)
         avail_stem_n = max(avail_stem_n, 0.0)
         avail_lfsheath_n = pl_nit(lfsheath_part) - 
     :                  mnc(lfsheath_part) * plantwt(lfsheath_part)
         avail_lfsheath_n = max(avail_lfsheath_n, 0.0)
         avail_leaf_n = pl_nit(leaf_part) - 
     &                  mnc(leaf_part) * plantwt(leaf_part)
         avail_leaf_n = max(avail_leaf_n, 0.0)
      
         if (MNRTN.gt.0.0) then
!            avail_root_n = plantn(root) - p_root_n_min * pl_wt(root)
!            avail_root_n = l_bound (avail_root_n, 0.0)
            avail_root_n = pl_nit(root_part) - 
     &                     MNRTN /1000 * plantwt(root_part)
            !*! avail_root_n = l_bound (avail_root_n, 0.0)
            avail_root_n = max(avail_root_n, 0.0)
         else
            avail_root_n = 0.0
         endif
!*!      plantn(grain) = p_init_grain_nconc * pl_wt(grain)
         pl_nit(grain_part) = INGNC * plantwt(grain_part)
!*!      plantn(grain) = u_bound (plantn(grain)
!*!     :                           ,avail_stem_n+avail_root_n)
         pl_nit(grain_part) = min(pl_nit(grain_part)
     :  ,avail_leaf_n + avail_lfsheath_n+ avail_stem_n+avail_root_n)
         ! first of all, try to take N from stem
!*!      plantn(stem) = plantn(stem) - min(avail_stem_n, plantn(grain))    
         pl_nit(stem_part) = pl_nit(stem_part) - 
     &                  min(avail_stem_n, pl_nit(grain_part))
!          If there is no enough available N from stem, take N from lfsheath 
         lfsheath_n_moved = max(0.0, pl_nit(grain_part) - avail_stem_n)
         lfsheath_n_moved = min(lfsheath_n_moved, avail_lfsheath_n)
         pl_nit(lfsheath_part) = pl_nit(lfsheath_part)- lfsheath_n_moved
!          If there is no enough available N from lfsheath, take N from leaf 
         leaf_n_moved = max(0.0, pl_nit(grain_part) - avail_stem_n-
     :                  avail_lfsheath_n)
         leaf_n_moved = min(leaf_n_moved, avail_leaf_n)
         pl_nit(leaf_part) = pl_nit(leaf_part)- leaf_n_moved
!*!      root_n_moved = max(0.0, plantn(grain) - avail_stem_n)         
         root_n_moved = max(0.0, pl_nit(grain_part) - avail_stem_n-
     :                  avail_lfsheath_n-avail_leaf_n)
!*!      plantn(root) = plantn(root) - root_n_moved
         pl_nit(root_part) = pl_nit(root_part) - root_n_moved
 
      else
       continue ! means Pl_nit has no change
      endif
 
 
 
      !*! call pop_routine (myname)
      return
      end subroutine nwheats_plnin
 !*******************************************
!TODO: where does this come from? I am taking a total guess that this is what it does.
!       TODO: I am guessing that it checks if it is between the bounds and then sends a warning to the summary file it is outside
!             the bounds. I assume that is why they have the final parameter which is a string. 
!             I am guessing it is used in the message to the summary file to specify which variable was outside the bounds. 
!             Unlike u_bound and l_bound it does not force the variable to be between the bounds. It just warns the user in the summary file.
      subroutine bound_check_real_var(Variable, LowerBound, 
     &            UpperBound, VariableName, ERRKEY)
!       JZW if pass IDETO in the argument, the warning will go to the summary.out
!       IDETO  = ISWITCH % IDETO   
          IMPLICIT  NONE
          EXTERNAL GETLUN, WARNING
          
          real Variable, LowerBound, UpperBound
          Integer NOUTDO
          CHARACTER*1 IDETO
          character*6 ERRKEY
          character*12 VariableName
          CHARACTER*78 MESSAGE(10) 
          !character warningMsg
          CALL GETLUN('OUTO', NOUTDO)
         
         if(Variable .GT. UpperBound) then
            !Warning.out
            WRITE(MESSAGE(1), 110) VariableName, UpperBound
            CALL WARNING(1, ERRKEY, MESSAGE)

            !Overview.out
            IF (IDETO .EQ. 'Y') THEN
              WRITE(NOUTDO,120) MESSAGE(1)
            ENDIF
  110 FORMAT("The variable: /'", 12A, 
     &     "/' is below the expected upper bound of: ", F6.2)
!            warningMsg = "The variable: /'" + VariableName + "/' is above the expected upper bound of: " + UpperBound;
!            warning_error(warningMsg);
         endif
         !if(Variable .GT. LowerBound) then
         if(Variable .LT. LowerBound) then
            !Warning.out
            WRITE(MESSAGE(1), 115) VariableName, LowerBound
            CALL WARNING(1, ERRKEY, MESSAGE)

            !Overview.out
            IF (IDETO .EQ. 'Y') THEN
              ! NOUTDO    Logical unit for OVERVIEW.OUT file 
              WRITE(NOUTDO,120) MESSAGE(1)
  115 FORMAT("The variable: /'", 12A, 
     &     "/' is below the expected lower bound of: ", F6.2)           
            ENDIF
            
!            warningMsg = "The variable: /'" + VariableName + "/' is below the expected lower bound of: " + LowerBound;
!            warning_error(warningMsg);
         endif

  120 FORMAT(/,5X,A78,/,5X,A78,/)
         Return
         end

     
