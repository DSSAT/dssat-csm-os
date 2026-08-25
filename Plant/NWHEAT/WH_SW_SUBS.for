C=======================================================================
C  WH_SW_SUBS, Subroutne   
C  Adapted from APSIM nwheats (v 0.3) routines:  
C   
C-----------------------------------------------------------------------
C  PURPOSE: 
C  
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/14/2012 Written for WHAPS wheat model, from nwheat subroutines: 
C             nwheats_set_swdef_new, nwheats_watup_new, nwheats_cwdmd, 
C             nwheats_cwpot (the nwheats_ prefix has been dropped), 
C             nwheats_swafc and nwheats_rtdp_swf.
C-----------------------------------------------------------------------
C  Called by  : 
C  Calls      : 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
!*! Call order from crppr:
!*!                     set_swdef_new 
!*!                     watup_new
!*! Call from prepare:
!*!                     set_swdef_new
!*!                     cwdmd 
!*! Call from watup_new:
!*!                     cwdmd
!*!                     cwpot
C-----------------------------------------------------------------------
C=======================================================================
C  set_swdef_new, Subroutine
C            Calculates the soil water availability factor (0-1), 
*            also called deficit factor:
*              type 1, the least sensitive, used for photosynthesis.
*              type 2, more sensitive, used for plant cell expansion.
*              type 3, used for tillering.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/24/2012  FSR adapted from nwheats
!-----------------------------------------------------------------------
C  Called by: crppr; prepare 
C  Calls    : nwheats_level
C=======================================================================
      SUBROUTINE set_swdef_new (swdep, uptake_source,
     &    cwdemand, istage, rtdep_nw, rwu_nw, SOILPROP,          !Input
     &    swdef)                                                 !Output

! 2023-01-18 CHP removed unused variables from argument list:
!  CONTROL

!     ------------------------------------------------------------------
      USE ModuleDefs 
      USE WH_module
      IMPLICIT NONE
      EXTERNAL nwheats_level, sum_real_array
      SAVE

      integer istage 

      real    swdef(3)  ! soil-water deficit stress factor for each of 3
                        !  three processes: photo, cellxp, and tiller.
      real    rwu_nw (NL)! (nwheats_watup_new OUTPUT) root water 
                          !  uptake (mm)
      character*6 uptake_source ! relocate parameter to FileX (*.WHX)
!     Local Variables
!**!      integer NL ! max number of layers permitted in any soil(?)
      integer L      ! Loop counter
!*!   integer nlayr_nw  ! Number of layers in specific soil profile
      integer nrlayr ! number of layers with roots
      integer nslayr ! number of layers in top 20cm
      integer nwheats_level ! Function from nwheats. Returns layer  
                            ! number of depth in profile dlayr_nw
      
      real cumdep_nw ! depth of deepest layer with roots (mm)
!     real depth_nw  ! Depth below soil surface to the point of interest
      real dlayr_nw(NL)! Thickness increment of soil layer L  (mm)
      real ep_nw     !  plant evapotranspiration (mm)
      real duldep(NL)! drained upper limit by soil layer
      real fesw      ! fraction of extractable soil water
      real lldep(NL) ! drained lower limit by soil layer
      real part_layer! part of the upper soil layers used in t
      real pesw_nw(NL)! available water in layer
      ! real pesw_nw ! available water in layer
      real pot_esw(NL)! potential esw for layer
      real rtdep_nw
      real rtdepl     ! root depth in deepest root layer (mm)
      real sum_real_array 
      real cwdemand    ! (nwheats_cwdmd OUTPUT) crop water demand (mm)
      real swdep(NL)! water content by soil layer (mm) (?)
      real tempsum 
      real till_sw_depth ! the depth to use to calulate fesw 
                         ! equals rtdep_nw up to a max of 30 cm

      real top_cumdep   ! Cumulative depth of the top layers that
      real top_fesw     ! fesw for topsoil
      real tpesw_nw     ! total available water currently in profile
      real tpot_esw     ! potential esw for profile
      
!     The variable "CONTROL" is of type "ControlType".
!     TYPE (ControlType) CONTROL 
      TYPE (SoilType) SOILPROP
            
      dlayr_nw = SOILPROP % DLAYR * 10.
      duldep   = SOILPROP % DUL    
      lldep    = SOILPROP % LL     

      ! find soil water deficit factors that: 
      !     restrict photosynthesis (type 1)
      !     restrict plant cell expansion (type 2)
      !     slow crop phenology (to point of dying) (type 3)
      !     (upper limit is 1)
!----------------------------------------------------------------------- 
!*!   nrlayr = nwheats_level (rtdep_nw)  
!*!   nrlayr = nwheats_level (rtdep_nw)
      nrlayr = nwheats_level (rtdep_nw,dlayr_nw,NL)
!-----------------------------------------------------------------------          
C cbak find the distance roots have extended into the deepest layer with roots
 
!*!    cumdep = sum_real_array (dlayr, nrlayr)
       tempsum = 0.0
       do L = 1, nrlayr
          tempsum = tempsum + dlayr_nw(L)
       enddo
       cumdep_nw = tempsum

      rtdepl = dlayr_nw(nrlayr) - (cumdep_nw - rtdep_nw)
!*!   rtdepl = bound (rtdepl, 0.0, dlayr(nrlayr))
      rtdepl = MAX(rtdepl, 0.0)
      rtdepl = MIN(rtdepl, dlayr_nw(nrlayr))
!-----------------------------------------------------------------------      
C cbak now estimate extractable soil water in each layer that contains roots
C      excluding the deepest layer
 
      do L = 1, nrlayr-1
        pesw_nw(L) = swdep(L) - lldep(L)
!*!     pesw_nw(L) = l_bound(pesw_nw(L),0.0)
        pesw_nw(L) = MAX(pesw_nw(L),0.0)
        pot_esw(L) = duldep(L) - lldep(L)
      enddo
!-----------------------------------------------------------------------
cbak   now estimate extractable soil water for the deepest layer which is only
cbak   partly explored by roots
        pesw_nw(nrlayr) = (swdep(nrlayr) - lldep(nrlayr)) *
!*!  :  divide (rtdepl, dlayr_nw(nrlayr), 0.0 )
     &  (rtdepl / dlayr_nw(nrlayr))
 
        pesw_nw(nrlayr) = MAX(pesw_nw(nrlayr),0.0)
 
        pot_esw(nrlayr) = (duldep(nrlayr) - lldep(nrlayr)) *
!*!  :  divide (rtdepl, dlayr_nw(nrlayr), 0.0 )
     &  (rtdepl / dlayr_nw(nrlayr))   

!-----------------------------------------------------------------------
cbak now accumulate for the profile that contains roots
 
      tpesw_nw = sum_real_array (pesw_nw, nrlayr)
 
      tpot_esw = sum_real_array (pot_esw, nrlayr)
 
!*!   fesw = divide (tpesw_nw, tpot_esw, 0.0)
      if(tpot_esw .GT. 0.0) then
         fesw = (tpesw_nw / tpot_esw)
      else 
         fesw = 0.0
      endif 
 
!     calculate water stress factors
 
      if (istage.eq.germ) then
         swdef(photo_nw)  = 1.0
         swdef(cellxp)    = 1.0
         swdef(tiller_nw) = 1.0
 
      else if ((istage. ge. emergence) .and. (istage .lt. mature)) then
         ! we have crop growth processes that are sensitive
         !            to water stresses
 
         ep_nw = sum_real_array (rwu_nw, nrlayr)
 
         if (uptake_source.eq.'calc') then
              ! we use water content in soil to slow down growth
              ! Photosynthesis is limited when fesw < 0.25
              ! Photosynthesis stops when fesw = 0
!*!         swdef(photo) = divide (fesw, 0.25, 0.0)
            swdef(photo_nw) = (fesw / 0.25)
!*!         swdef(photo) = bound(swdef(photo), 0.0, 1.0)
            swdef(photo_nw) = MAX(swdef(photo_nw), 0.0)
            swdef(photo_nw) = MIN(swdef(photo_nw), 1.0)
        else
              ! we need to use the measured uptake response of the
              ! water balance to get a photo stress factor.
!*!         swdef(photo) = divide (ep, sw_demand, 0.0) 
              !APSIM uses SW_demand, read from respond2get_real_var
!*!         swdef(photo) = divide (ep_nw, cwdemand, 0.0)
            if (cwdemand .eq. 0.) then
              swdef(photo_nw) = 1.0 !JZW set
            else
              swdef(photo_nw) = (ep_nw / cwdemand)
            endif
!*!         swdef(photo) = bound(swdef(photo), 0.0, 1.0)
            swdef(photo_nw) = MAX(swdef(photo_nw), 0.0)
            swdef(photo_nw) = MIN(swdef(photo_nw), 1.0)
         endif
 
           ! Expansion growth is limited when fesw < 0.45
           ! Expansion growth stops when fesw = 0.15
 
!*!      swdef(cellxp) = divide (fesw, 0.3, 0.0) - 0.5
         swdef(cellxp) = (fesw / 0.3) - 0.5  
!         fesw is fraction of extractable soil water
!*!      swdef(cellxp) = bound(swdef(cellxp), 0.0, 1.0)
         swdef(cellxp) = MAX(swdef(cellxp), 0.0)
         swdef(cellxp) = MIN(swdef(cellxp), 1.0)
 
           ! Tillering is affected when fesw < 1.0
           ! Tillering stops when fesw =  0.5
 
           ! calculate the water content of the top 40cm
           ! and use to limit tillering
 
         till_sw_depth = rtdep_nw
!*!      till_sw_depth = bound(till_sw_depth, 0.0, 400.0)
         till_sw_depth = MAX(till_sw_depth, 0.0, 400.0)
         till_sw_depth = MIN(till_sw_depth, 0.0, 400.0)
 
           ! calculate number of layers that span the top 40cm
 
         nslayr = nwheats_level (till_sw_depth,dlayr_nw,NL)
!-----------------------------------------------------------------------
cbak now estimate extractable soil water in each top layer that contains roots
cbak excluding the deepest layer
 
         do L = 1, nslayr-1
            pesw_nw(L) = swdep(L) - lldep(L)
!*!         pesw_nw(L) = l_bound(pesw_nw(L),0.0)
            pesw_nw(L) = MAX(pesw_nw(L), 0.0)
            pot_esw(L) = duldep(L) - lldep(L)
         enddo
!----------------------------------------------------------------------- 
cbak   now estimate extractable soil water for the deepest layer of the top-soil
cbak   this maybe part of a layer
 
         top_cumdep = sum_real_array ( dlayr_nw, nslayr)
         part_layer = dlayr_nw(nslayr) - (top_cumdep - till_sw_depth)
         pesw_nw(nslayr) = (swdep(nslayr) - lldep(nslayr)) *
!*!  :                  divide (part_layer, dlayr_nw(nslayr), 0.0 )
     &                  (part_layer / dlayr_nw(nslayr))
 
         pesw_nw(nslayr) = MAX(pesw_nw(nslayr),0.0)
         
         pot_esw(nslayr) = (duldep(nslayr) - lldep(nslayr)) *
!*!  :                  divide (part_layer, dlayr_nw(nslayr), 0.0 )
     &                  (part_layer / dlayr_nw(nslayr))
 
!----------------------------------------------------------------------- 
cbak now accumulate for the profile that contains roots or is within 40cm of sur
 
         tpesw_nw = sum_real_array (pesw_nw, nslayr)
         tpot_esw = sum_real_array (pot_esw, nslayr)
 
!*!      top_fesw = divide (tpesw_nw, tpot_esw, 0.0)
         if (tpot_esw .gt. 0.) then
           top_fesw = (tpesw_nw / tpot_esw) 
!           tpesw is total available water currently in profile
         else 
           top_fesw = 0. 
!           fesw for topsoil. fesw is fraction of extractable soil water
         endif
 
!*!      swdef(tiller) = divide (top_fesw, 0.5, 0.0) - 0.5
         swdef(tiller_nw) = (top_fesw / 0.5) - 0.5
!*!      swdef(tiller) = bound(swdef(tiller), 0.0, 1.0)
         swdef(tiller_nw) = MAX(swdef(tiller_nw), 0.0)
         swdef(tiller_nw) = MIN(swdef(tiller_nw), 1.0)
!-----------------------------------------------------------------------
 
      endif
      return
      END SUBROUTINE set_swdef_new
C=======================================================================
C  nwheats_watup_new, Subroutine  
C    Returns actual water uptake from soil profile by the crop.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/24/2012  FSR adapted from nwheats
!-----------------------------------------------------------------------
C  Called by: 
C  Calls    : 
C=======================================================================
      SUBROUTINE nwheats_watup_new (SOILPROP,swdep,uptake_water,
     &    cwdemand, nrlayr, potntl, uptake_source,       !Input
!    &    rlv_nw, above_gnd_pcarb, carbh, nwheats_topsfr, PLTPOP, pcarbo, temp_c, TMAX, TMIN,  
!     need above by call nwheats_cwdmd 
!    &  cumph_nw, cwdemand, istage,  nrlayr, pl_la, PLTPOP, sen_la, 
!     Need above by call nwheats_cwpot
     &    prwu, rwu_nw)                                          !Output
!         nwheats_cwpot may be removed from input if keep call  nwheats_cwpot inside this subroutine
!        jzw ADD ARGUMENT rwu_nw ON Nov 28

! 2023-01-18 CHP removed unused variables from argument list:
!  CONTROL, istage

!     ------------------------------------------------------------------
      USE ModuleDefs    
      IMPLICIT NONE
      EXTERNAL count_of_real_vals, sum_real_array
      SAVE

      integer count_of_real_vals
!     INTEGER istage
!**!      integer NL    ! max number of layers permitted in any soil(?)
      integer num_layers 
      
!     real carbh   !H2O stressed CH2O output (production) (gDM/plant)
      real cwdemand     ! crop water demand (mm).
!     It is sw_demand in nwheats.for,  water demand from soil by the crop (mm)
!    call respond2get_real_var ('sw_demand'
!     :        , '()', sw_demand)

      real dlayr_nw(NL) ! Thickness increment of soil layer L  (mm)
!     real fill_real_array
!     real swdef(3)     ! soil-water deficit stress factor for each of 3
                        !  three processes: photo, cellxp, and tiller.
      real lldep(NL)! drained lower limit by soil layer
      
!     real cumph_nw(istage)!Cumulative phyllochron intervals
                           ! or fully expanded leaves
      !real lai             ! leaf area index (m2/m2)
!     real pl_la           !Total leaf area per plant (green + senesced)
!     real nwheats_topsfr ! Senthold alternative to topsfr (tops fract)
!     real pcarbo        !Potential CH2O output (production) (gDM/plant)
!     REAL PLTPOP        ! Plant population at emergence (plants/m-2)
!*! real potntl(*)    ! (nwheats_cwpot OUTPUT) potential crop water
      real potntl(NL)    ! (nwheats_cwpot OUTPUT) potential crop water 
                        !   uptake for each layer (mm)
      real pesw_nw ! available water in layer
      !*! real pesw_nw(NL)! available water in layer
      real prwu (NL)! (nwheats_watup_new OUTPUT) root water uptake
                        !  (mm)
      real rwu_nw (NL)! (nwheats_watup_new OUTPUT) root water uptake
                       !  (mm)
!     real sen_la         !Senesced leaf area for plant
      real sum_real_array
      real swdep(NL)! water content by soil layer (mm) (?)
      character*6 uptake_source ! relocate parameter to FileX (*.WHX)
!     Local Variables
!*!   integer layer     ! (??)
      integer L         ! Loop counter
!*!   integer nlayr_nw  ! Number of layers in specific soil profile
      integer nrlayr    ! number of layers with roots
!     real cumdep_nw    ! depth of deepest layer with roots (mm)
!     real temp_c       ! dummy temperature for function (oC)
!     REAL TMAX
!     REAL TMIN
      real tpot 
      real uptake_water(NL)
      ! add the following by JZW
!     real rlv_nw(NL) ! root length volume array in mm/mm3
      ! real above_gnd_pcarb ! potential increase in above ground biom
!     The variable "CONTROL" is of type "ControlType".
!     TYPE (ControlType) CONTROL
      TYPE (SoilType)    SOILPROP
      dlayr_nw  = SOILPROP % DLAYR  * 10.0 
      lldep = SOILPROP % LL

      if (uptake_source .eq. 'calc') then
 
          ! get water demand by the plant for water non-limiting.
!----------------------------------------------------------------------- 
!      call nwheats_cwdmd (CONTROL, uptake_source, 
!     &    carbh, nwheats_topsfr, PLTPOP,                   !Input 
!     &    pcarbo, temp_c, TMAX, TMIN,                         !Input
!     &    cwdemand)                                        !Output
     
!-----------------------------------------------------------------------
 
!        Now we have to partition trwu over the root system
!        Use the previous system with RLV, soil water etc determining potntl
!-----------------------------------------------------------------------
!      call nwheats_cwpot (CONTROL, SOILPROP, rlv_nw, swdep,
!     &    cumph_nw, cwdemand, istage,                            !Input
!     &    nrlayr, pl_la, PLTPOP, sen_la,                         !Input
!     &    potntl)                                                !Output
!-----------------------------------------------------------------------
 
         nrlayr = count_of_real_vals (potntl, NL)
         tpot = sum_real_array (potntl, nrlayr)
 
         if (tpot.le.0.0 .or. cwdemand.le.0.0) then
 
               ! we have no uptake - there is no demand or potential
 
!*!         call fill_real_array (prwu, 0.0, mxlayr)
            do L=1, NL
               prwu(L) = 0.0
            enddo
 
         else
                 ! get actual uptake -
                 ! demand is already discounted by water stress.
 
!*!         call fill_real_array (prwu, 0.0, mxlayr)
            do L=1, NL
               prwu(L) = 0.0
            enddo
 
         ! partition trwu over the root system and down the soil profile
         ! uptake should not exceed potential because water availability
         ! discounts water demand but we check anyway.
 
            do L = 1, nrlayr
               prwu(L) = cwdemand * (potntl(L)/tpot)
!*!            pesw_nw = l_bound(swdep(layer) - lldep(layer),0.0)
               pesw_nw = MAX((swdep(L) - lldep(L)) *dlayr_nw(L) ,0.0)
 
cnh I assume we really do not want error messages for these small
cnh errors anymore.
cnh                  call bound_check_real_var
cnh     :                 (prwu(layer), 0.0, pesw, 'prwu')
 
!*!            prwu(layer) = bound (prwu(layer),0.0,pesw_nw)
               prwu(L) = MAX (prwu(L), 0.0)
               prwu(L) = MIN (prwu(L), pesw_nw)
            enddo
         endif
 
      else
         ! Use the values given by APSIM.
 
         num_layers = count_of_real_vals (dlayr_nw, NL)
         do L = 1, num_layers
            rwu_nw(L) = uptake_water(L)
         enddo
      endif
      return
      END SUBROUTINE nwheats_watup_new
C=======================================================================
C  nwheats_cwdmd, Subroutine  
C    Returns crop water demand from soil by the crop (mm).
C-----------------------------------------------------------------------
*+  Assumptions (from original Nwheat code)
*       the temperatures are > -237.3 oC for the svp function.
C
*+  Notes (from original Nwheat code)
*       This is not how CERES-Wheat does this - it used EO-ES but we
*       are using this method for now as it seems more stable.
*       This may need looking into later on.
*
*       average saturation vapour pressure for ambient temperature
*       during transpiration is calculated as part-way between that
*       for minimum temperature and that for the maximum temperature.
*       tanner & sinclair (1983) used .75 and .67 of the distance as
*       representative of the positive net radiation (rn).  Daily svp
*       should be integrated from about 0900 hours to evening when rn
*       becomes negetive.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/24/2012  FSR adapted from nwheats
!-----------------------------------------------------------------------
C  Called by: 
C  Calls    : 
C=======================================================================
      SUBROUTINE nwheats_cwdmd (uptake_source,
     &    carbh, nwheats_topsfr, PLTPOP,                      !Input 
     &    pcarbo, TMAX, TMIN, transp_eff_coeff,               !Input
     &    cwdemand, vpd_transp)                              !Output

! 2023-01-26 chp removed unused variables from argument list: CONTROL

!     ------------------------------------------------------------------
      USE ModuleDefs    
      IMPLICIT NONE
      SAVE

!*!      integer istage
!*!      integer mxlayr    ! max number of layers permitted in any soil(?)
!*!      integer num_layers 
!*!
!*!      real demand       ! (OUTPUT) crop water demand mm
!*!      real count_of_real_vals
!*!      real cwdemand     ! crop water demand (mm)
!     The variable "CONTROL" is of type "ControlType".
!     TYPE (ControlType) CONTROL
*+  Calls   real nwheats_topsfr         ! function

*+  Constant Values
      character myname*(*)            ! name of subroutine
      parameter (myname = 'nwheats_cwdmd')
*
      real svp_fr            ! fraction of distance between svp at
      parameter (svp_fr = 0.75)    ! min temp and svp at max temp where
                                   ! average svp during transpiration
                                   ! lies. (0-1)
      real mb2kpa            ! conversion milibars to kpa ?
      parameter (mb2kpa = 0.10)

      real g2mm              ! conversion grams H2O to mm^3 H2O
      parameter (g2mm = 0.001)

      real transp_eff_coeff  ! transpiration efficiency coefficient
*
cbak
!      parameter (transp_eff_coeff = 0.006) 
!       To convert vpd to transpiration efficiency (kpa) although this is 
!       expressed as a pressure, it is really in the form
!       kpa*g carbo per m^2 / g water per m^2 and this can be converted to
!       kpa*g carbo per m^2 / mm water because 1g water = 1 cm^3 water

*+  Local Variables   
      real above_gnd_pcarb ! potential increase in above ground biom
      real carbh   !H2O stressed CH2O output (production) (gDM/plant)
      real cwdemand        ! (OUTPUT) crop water demand mm
      real nwheats_topsfr ! Senthold alternative to topsfr (tops fract)
      real pcarbo        !Potential CH2O output (production) (gDM/plant)
      REAL PLTPOP        ! Plant population at emergence (plants/m-2)
      real potential_ep  ! potential transpiration of crop (mm)
      real svp           ! function to get saturation vapour
                         ! pressure for a given temperature
                         ! in oC (kpa)
      real temp_c        ! dummy temperature for function (oC)
      REAL TMAX
      REAL TMIN
      real transp_eff    ! transpiration efficiency in converting
                         ! mm water to g carbohydrate
                         ! (g carbo/m^2/mm water)
      character*6 uptake_source ! relocate parameter to FileX (*.WHX)
      real vpd_transp    ! vapour pressure deficit (kpa) during
                         ! positive net radiation period
                         ! (tanner & sinclair, 1983)
*+  Initial Data Values
         ! set up saturation vapour pressure function

      svp(temp_c) = 6.1078  ! should be range of 4-5
     &              * exp (17.269 * temp_c / (237.3 + temp_c))
!*!  &              * mb2kpa ! conversion milibars to kpa ? 
     &              * 0.1  
     
         ! get vapour pressure deficit when net radiation is positive.
 
      vpd_transp = svp_fr * (svp (TMAX) - svp (TMIN))
 
         ! get potential transpiration from potential
         ! carbohydrate production and transpiration efficiency
 
!*!   transp_eff = divide (transp_eff_coeff, vpd_transp, 0.0) /g2mm
      if(vpd_transp .GT. 0.0) then
         transp_eff = (transp_eff_coeff / vpd_transp) /g2mm
      else
         transp_eff = 0.0
      endif 

!*!   if (g_uptake_source.eq.'apsim') then
      if (uptake_source .eq. 'apsim') then
         ! todays water demand is the amount of water to satisfy the
         ! potential growth of the plant
 
         above_gnd_pcarb = pcarbo * nwheats_topsfr

      else
!          todays water demand is actually a supply term.  We use carbo
!          instead of pcarbo to capture the decrease in growth with water
!          content because carbo is pcarbo after water stress
 
         above_gnd_pcarb = carbh * nwheats_topsfr

      endif
 
!*!   potential_ep = divide (above_gnd_pcarb*plants, transp_eff, 0.0)
      if(transp_eff .GT. 0.0) then
         potential_ep = (above_gnd_pcarb *  PLTPOP) / transp_eff
      else
         potential_ep = 0.0
      endif
 
      cwdemand  = potential_ep

      return
      END SUBROUTINE nwheats_cwdmd
C=======================================================================
C  nwheats_cwpot, Subroutine
*     Returns potential water uptake from each layer of the soil profile
*     by the crop (mm water). This represents the maximum amount in each
*     layer regardless of the root distribution.
*+  Notes (??)
*     Neil was too lazy to change the units in the equations from cm to mm - t
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/24/2012  FSR adapted from nwheats
!-----------------------------------------------------------------------
C  Called by: 
C  Calls    : 
C=======================================================================
      SUBROUTINE nwheats_cwpot (SOILPROP, rlv_nw, swdep,
     &    cumph_nw, istage,                                      !Input
     &    nrlayr, pl_la, PLTPOP, sen_la,                         !Input
     &    potntl)                                                !Output

! 2023-01-26 chp removed unused variables in argument list: 
!         CONTROL, cwdemand, 
!     ------------------------------------------------------------------
      USE ModuleDefs    
      IMPLICIT NONE
      EXTERNAL count_of_real_vals
      SAVE
      
!     The variable "CONTROL" is of type "ControlType".
!     TYPE (ControlType) CONTROL

      integer count_of_real_vals
      integer istage
!**!      integer NL    ! max number of layers permitted in any soil(?)
!     integer num_layers 
      
      real potntl(NL)! Potential crop H2O uptake by soil layer (mm)
!     real cwdemand     ! crop water demand (mm)
      real       rwumax ! maximum value of rwu (cm)
      parameter (rwumax = 0.03) ! Move to species file?

      real mm2cm           ! conversion mm to cm
      parameter (mm2cm = 0.10)

      real avolsw          ! available volumetric soil water (mm3/mm
      real cumph_nw(istage)!Cumulative phyllochron intervals
                           ! or fully expanded leaves
      real dlayr_nw(NL)! Thickness increment of soil layer L  (mm)
      real lai             ! leaf area index (m2/m2)
      real lldep(NL)   ! drained lower limit by soil layer
      integer L            ! soil profile layer number
      integer nrlayr       ! number of layers with roots
      real pl_la           !Total leaf area per plant (green + senesced)

      REAL PLTPOP         ! Plant population per m2
      real rlv_cm         ! root length volume in cm/cm3
      real rlv_nw(NL) ! root length volume array in mm/mm3
      real p_available_sw ! temporay plant available sw in a layer
      real sen_la         !Senesced leaf area for plant  
      real sm2smm         ! conversion sq meters to sq milimeters
      parameter (sm2smm = 1000000)

      real swdep(NL)  ! water content by soil layer (mm)
      TYPE (SoilType)    SOILPROP
      dlayr_nw  = SOILPROP % DLAYR  * 10.0 
      lldep = SOILPROP % LL
  
*- Implementation Section ----------------------------------      
      
      ! get potential uptake
!*!   call fill_real_array (potntl, 0.0, mxlayr)
         do L=1, NL
            potntl(L) = 0.0      !*! zero-initialize array
         enddo

      nrlayr = count_of_real_vals (rlv_nw, NL)
 
      do L = 1, nrlayr
 
         if (swdep(L) .gt. lldep(L)) then
            rlv_cm = rlv_nw(L)*100.
            avolsw = (swdep(L) - lldep(L))/dlayr_nw(L)
            ! -----------------------------------------------------
            potntl(L) = 2.67e-3*exp(62.*avolsw)/(6.68-log(rlv_cm))
            ! -----------------------------------------------------
            if (potntl(L) .gt. rwumax) potntl(L) = rwumax
            ! -----------------------------------------------------
            potntl(L) = potntl(L)*dlayr_nw(L)*mm2cm*rlv_cm*
     :                            (0.18+0.00272*(rlv_cm-18.)**2)
            ! -----------------------------------------------------
            lai = (pl_la - sen_la) *PLTPOP/sm2smm
            if (cumph_nw(istage) .lt. 4 .and. lai .lt. 1) then
               potntl(L) = potntl(L)*(3. - 2. * lai)
 
            else
            endif
!             --------------------------------------------------------------
         else
            potntl(L) = 0.0
         endif
 
cnh this unit conversion was previously after the following boundary
cnh check.  The units need to be changed to mm before you can do the check
         potntl(L) = potntl(L)/mm2cm     ! convert to mm H2O
                                                 ! -----------------
 
cbak   bound potential water uptake in a layer (in cm) to between 0 and the
cbak   max avail water over the lower limit
 
         p_available_sw = swdep(L) - lldep(L)
!*!      p_available_sw = l_bound (p_available_sw, 0.0)
         p_available_sw = MAX(p_available_sw, 0.0)
 
!*!      potntl(L) = bound (potntl(L), 0.0, p_available_sw )
         potntl(L) = MAX(potntl(L), 0.0)
         potntl(L) = MIN(potntl(L), p_available_sw)
 
      enddo
      return
      end subroutine nwheats_cwpot 

C=======================================================================
C  nwheats_level, Integer Function
C  Determines the soil layer corresponding to depth beneath soil surface.
C-----------------------------------------------------------------------
C  02/20/2012 Created by FSR from integer function nwheats_level (depth)
C-----------------------------------------------------------------------
C  INPUT  : depth_nw, NL, dlayr_nw(layer)
C  LOCAL  : L, tempsum, count
C  OUTPUT : nlayr_nw, nwheats_level
C-----------------------------------------------------------------------
C                         DEFINITIONS
C  nlayr_nw   : Number of layers in specific soil profile
C  dlayr_nw(L): Thickness increment of soil layer L  (mm)
C  L          : Loop counter
C  depth_nw   : Depth below soil surface to the point of interest (mm)
C  NL         : Max number of layers permitted in any soil 
C=======================================================================

!*!   integer function nwheats_level (nlayr_nw,dlayr_nw,depth_nw,mxlayr)
      integer function nwheats_level (depth_nw,dlayr_nw,NL)

      implicit  NONE

      integer    L, NL, nlayr_nw, count 
!**!   integer   L, nlayr_nw, count 
!*!   integer   L,      M,        N, count 
      
      real      dlayr_nw(NL), depth_nw, tempsum
!*!   real                Z,        X, tempsum

       tempsum = 0.0
       count = 0
       
       do L = 1, NL
          if (dlayr_nw(L) .GT. 0.0) then
             count = count+1
             nlayr_nw = count
             tempsum = tempsum + dlayr_nw(L)
             if (depth_nw .LE. tempsum+0.01) then
	         nwheats_level = nlayr_nw
               exit
             endif
          endif
       end do
     
      return
      end function nwheats_level
C=======================================================================
C  sum_real_array, Real Function, FSR Nwheat-to-DSSAT 02/24/2012
C  Sum elements of array y(L) from L = 1 through N.
C-----------------------------------------------------------------------
C  INPUT  : N, y(N)
C  LOCAL  : L, tempsum, count
C  OUTPUT : sum_real_array
C=======================================================================
      real function sum_real_array (y, N)

      implicit  NONE
      integer   L, N, count 
      real      y(N), tempsum

      tempsum = 0.0
      count = 0

      do L = 1, N
         tempsum = tempsum + y(L)
      end do
      sum_real_array = tempsum 
      
      return
      end function sum_real_array
C=======================================================================
C  add_real_array, Subroutine, FSR Nwheat-to-DSSAT 02/24/2012
C  Add elements of delt(L) to y(L) from L = 1 through N.
C-----------------------------------------------------------------------
C  INPUT  : N, y(N)
C  LOCAL  : L, tempsum, count
C  OUTPUT : add_real_array
C=======================================================================
      Subroutine add_real_array (delt, y, N)

      implicit  NONE
      integer   N, L
      real      delt(N), y(N)

      do L = 1, N
         y(L) = y(L) + delt(L)
      end do
  
      return
      end subroutine add_real_array     

  !     ===========================================================
      subroutine subtract_real_array (amount, store, dimen)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      real       amount(*)      ! (INPUT) amount to be removed
      integer    dimen          ! (INPUT) number of elements to be
                                !   removed
      real       store(*)       ! (INPUT/OUTPUT) array to be removed
                                !   from

!   + Purpose
!        remove contents of each element of an array from each element of
!        another array.

!   +  Definition
!        This subroutine subtracts each of the "dimen" elements of
!        "amount" from its corresponding element of "store".

!   +  Mission Statement
!        Subtract array %1 from %2

!   + Changes
!        270591 specified and programmed jngh

!   + Calls

!   + Local Variables
      integer    indx                  ! do counter

   !- Implementation Section ----------------------------------

      do 1000 indx  = 1,dimen
         store(indx ) = store(indx ) - amount(indx )
1000  continue

      return
      end subroutine

C=======================================================================
C  count_of_real_vals, Integer Function, FSR Nwheat-to-DSSAT 02/24/2012
C      counts elememts of array that are greater than zero.
C-----------------------------------------------------------------------
C  INPUT  : N, y(N)
C  LOCAL  : L, count
C  OUTPUT : count_of_real_vals
C=======================================================================
      integer function count_of_real_vals (y, N)

      implicit  NONE
      integer   L, N, count 
      real      y(N)
         count = 0
            do L = 1, N
                 if (y(L) .GT. 0.0) then
                    count = count+1
                 endif
            end do      
         count_of_real_vals = count
      
      return
      end function count_of_real_vals
C=======================================================================
C nwheats_rtdp_swf, Real Function, FSR Nwheat-to-DSSAT 09/21/2012
C
C   Gets the soil water availability factor. For a layer, it is 1.0  
C   unless the plant-extractable soil water declines below a fraction 
C   of plant-extractable soil water capacity or that layer.  This option
C   f takes the fasw in the next deepest  layer into account to
C   determine if roots are growing into dry soil or if the roots are
C   ahead of the extraction front.
C-----------------------------------------------------------------------
C  INPUT  : L 
C  LOCAL  : swaf, num_layers, fasw, fasw1, fasw2, weighting_factor, 
C           cumdep, rtdep_in_layer
C  OUTPUT : nwheats_rtdp_swf
C=======================================================================
      real function nwheats_rtdp_swf ( L, dlayr_nw, duldep, 
     &              lldep, NL, num_layers, rtdep_nw, swdep)

      implicit  NONE
      external sum_real_array
      integer   L                  ! (INPUT) soil profile layer number
!     integer   N, count
!     integer   count_of_real_vals ! integer function
      integer   NL  ! max number of layers permitted in any soil(?)
      integer   num_layers         !

      real       frpesw            !   fraction of plant extractable
      parameter (frpesw = 0.25)    !   soil-water below which stress
                                   !   occurs (0-1
      real cumdep
      real dlayr_nw(NL)! Thickness increment of soil layer L  (mm)  
      real duldep(NL)   ! drained upper limit by soil layer
      real fasw
      real fasw1
      real fasw2
      real lldep(NL)   ! drained lower limit by soil layer
      real rtdep_in_layer ! root depth in lower root layer (mm)
      real rtdep_nw     ! overall root depth in profile (mm) 
      real sum_real_array
      real swaf           ! soil water availability factor (0-1)
      real swdep(NL)  ! water content by soil layer (mm)
      real weighting_factor

!     character*78 msg(1)

*- Implementation Section ----------------------------------

!**!   num_layers = count_of_real_vals (dlayr_nw, NL)
 
      if (swdep(L).le.lldep(L)) then
         fasw = 0
      elseif (L.lt.num_layers) then
         ! calculate weighting factor for two layers
         cumdep = sum_real_array (dlayr_nw, L)
         rtdep_in_layer = dlayr_nw(L) - (cumdep - rtdep_nw)
!*!      rtdep_in_layer = bound (rtdep_in_layer, 0.0, dlayr_nw(L))
         rtdep_in_layer = MAX (rtdep_in_layer, 0.0)
         rtdep_in_layer = MIN (rtdep_in_layer, dlayr_nw(L))

!*!      weighting_factor = divide (rtdep_in_layer,dlayr_nw(L),0.0)
         weighting_factor = rtdep_in_layer/dlayr_nw(L)
!*!      fasw1 = divide (swdep(L)-lldep(L),duldep(L)-lldep(L),0.0)
!*!      fasw2 = divide (swdep(L)-lldep(L),duldep(L)-lldep(L),0.0)
            !*! fasw1 and fasw2 are identical? FSR
         if(duldep(L)-lldep(L) .GT. 0.0) then
           fasw1 = (swdep(L)-lldep(L)) / (duldep(L)-lldep(L))
           fasw2 = (swdep(L)-lldep(L)) / (duldep(L)-lldep(L))
         else 
           fasw1 = 0.0
           fasw2 = 0.0
         endif

         fasw = weighting_factor * fasw1
     &        + (1. - weighting_factor) * fasw2
 
         elseif (L .eq. num_layers) then
!*!      fasw = divide (swdep(L)-lldep(L),duldep(L)-lldep(L),0.0)
         fasw = (swdep(L)-lldep(L)) / (duldep(L)-lldep(L))
 
      else
!*!      call fatal_error (err_user, 'layer number too large')
           !*! replace this with a DSSAT-format error message - FSR      
         fasw = 0.0
      endif
 
!*!   swaf = divide (fasw, frpesw, 0.0)
      swaf = fasw / frpesw
!*!   nwheats_rtdp_swf = bound (swaf, 0.0, 1.0)
      nwheats_rtdp_swf = MAX (swaf, 0.0)
      nwheats_rtdp_swf = MIN (nwheats_rtdp_swf, 1.0)
 
      return
      end
C=======================================================================
C nwheats_swafc, Real Function, FSR Nwheat-to-DSSAT 09/24/2012
C
C   Gets the soil water availability factor.  For a layer,
C   it is 1.0 unless the plant-extractable soil water declines
C   below a fraction of plant-extractable soil water capacity for
C   that layer.
C-----------------------------------------------------------------------
C  INPUT  : L 
C  LOCAL  : ctpesw, pesw, peswcp, swaf
C  OUTPUT : nwheats_swafc
C=======================================================================
      real function nwheats_swafc (L, duldep, lldep, NL, swdep)
   
      implicit  NONE
      integer  L               ! (INPUT) soil profile layer number
      integer  NL  ! max number of layers permitted in any soil(?)
!*!   integer   N, count
*+  Constant Values
      real       frpesw         ! fraction of plant extractable
      parameter (frpesw = 0.25) !   soil-water below which stress
                                !   occurs (0-1)
*+  Local Variables    *******OK*******
      real ctpesw ! critical plant extractable soil-water below 
                  ! which stress occurs (mm/mm) 
      real duldep(NL)   ! drained upper limit by soil layer
      real lldep(NL)    ! drained lower limit by soil layer
!*!   character  e_messg*200   ! error message
      real pesw   ! plant extractable soil-water (mm/mm)
      real peswcp ! plant extractable soil-water capacity (mm/mm)
      
      real swaf   ! soil water availability factor (0-1)
      real swdep(NL)  ! water content by soil layer (mm)

*- Implementation Section ----------------------------------
      
      if (lldep(L).ge.duldep(L)) then
!*!       write (e_messg, *)
!*!  :          ' lower limit .ge. drained upper limit'
!*!  :          , new_line
!*!  :          , ' ll = ', divide (lldep(layer), dlayr(layer), 0.0)
!*!  :          , new_line
!*!  :          , ' dul = ', divide (duldep(layer), dlayr(layer), 0.0)
!*!  :          , new_line
!*!  :          , ' layer = ', layer
!*!      call warning_error (err_user, e_messg)
 
         nwheats_swafc = -1000000.0
 
      else
         pesw = swdep(L) - lldep(L)
         peswcp = duldep(L) - lldep(L)
         ctpesw = frpesw * peswcp
 
!*!      swaf   = divide (pesw, ctpesw, 0.0)
         if (ctpesw .eq. 0.) then
           swaf = 1.0
         else
           swaf   = pesw / ctpesw
         endif
!*!      nwheats_swafc = bound (swaf, 0.0, 1.0)
         nwheats_swafc = MAX (swaf, 0.0)
         nwheats_swafc = MIN (nwheats_swafc, 1.0)
      endif
 
      return
      end
C=======================================================================

C===========================================================
      !*! real function nwheats_rnfac (L)
      real function nwheats_rnfac (L, sno3, snh4, SOILPROP)
C===========================================================
      USE ModuleDefs
      implicit none
      EXTERNAL nwheats_lyrck, nwheats_fac
*+  Sub-Program Arguments
      integer    L                  ! (INPUT) layer number

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

*+  Local Variables
      real       nh4ppm                ! ammonia concentration (ppm)
      real       no3ppm                ! nitrate concentration (ppm)
      real       rnfac                 ! N factor (0-1)
      real       totn                  ! total N concentration (ppm)
      real sno3(NL), snh4(NL)
      ! The following is add by JZW
      real dlayr_nw(NL), bd(NL)
      TYPE (SoilType)    SOILPROP
      dlayr_nw  = SOILPROP % DLAYR  * 10.0 
      bd = SOILPROP % BD

*- Implementation Section ----------------------------------
 
!*!      call push_routine (myname)
 
!*!      call nwheats_lyrck (L)
      call nwheats_lyrck (L, dlayr_nw)
 
!*!      no3ppm = sno3(L)*nwheats_fac (L)
! JZW ! nwheats_fac is convert from kg/ha to ppm (mg/kg), i.e. kg2ppm
      no3ppm = sno3(L)*nwheats_fac (L, SOILPROP)
     
!*!      nh4ppm = snh4(L)*nwheats_fac (L)
      nh4ppm = snh4(L)*nwheats_fac (L, SOILPROP)
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
!*!      nwheats_rnfac = bound (rnfac, 0.1, 1.0)
      nwheats_rnfac = max(rnfac, 0.1 )
      nwheats_rnfac = min (nwheats_rnfac, 1.0)
 
!*!      call pop_routine (myname)
 
      return
      end
C=======================================================================
! pcarbo      Potential dry matter (carbohydrate) production (g/plant)  (Nwheat)
