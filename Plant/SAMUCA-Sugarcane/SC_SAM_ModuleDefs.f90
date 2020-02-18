	module SAM_ModuleDefs
    !-------------------------------------------------------------------------------!
    !     Contains defintion of derived data types and constants which are          !
    !     to be used in the intermediate SAMUCA model (Following CANEGRO - MJ)      !
    !-------------------------------------------------------------------------------!
    
    !--- DSSAT's Global Variables
    use ModuleDefs    
    save

    !--- The name of the (current) cultivar
      CHARACTER*20 CULTIVAR, SCSTGNAM(20), GROWTHPHASES(5)

!     Phenological stages (to cope with DSSAT GSTD):      
!     Sugarcane 'agronomical' growth stages:
!       1: emergence
!       2: tillering
!       3: tillering following stalk emergence
!       4: Stalk elongation and tiller senescence
!     Note that sugarcane has 11 physiological stages (see Moore & Botha, 2013)
      
      type CaneSamuca
            
          !--- Samuca's states variables
          integer   seqnow
          integer   year        
          integer   doy        
          integer   das        
          integer   dap
          integer   n_lf_AG_dewlap    
          integer   nratoon
          integer   n_ph
          real      diac
          real      dw_total
          real      dw_aerial
          real      dw_BG
          real      dw_it_ag
          real      dw_lf
          real      dw_rt
          real      dw_it_dead
          real      dw_it_dead_AG
          real      dw_it_dead_BG
          real      dw_lf_dead
          real      fw_it_ag
          real      suc_it_ag
          real      pol
          real      lai
          real      lai_ass
          real      nstk
          real      stk_h    
          real      swface     
          real      swfacp
          real      rd
          real      rld(nl)
          real      frac_li_pho
          real      frac_li_till
          real      trwup
          real      eop
          real      watdmd
          real      tot_gresp_crop
          real      tot_mresp_crop
          real      gstd
          real      dtg
          real      drue_calc
          real      rue_calc
          real      cstat
          real      amax_out
          real      eff_out
          real      tmn
          real      tempfac_pho
          real      tempfac_per
          real      co2
          real      pho_fac_co2
          real      diacem
          real      agefactor_amax
          real      agefactor_per
          real      sug_it_BG
          real      amaxfbfac
          real      per
          
          logical   flemerged
          character 	(len = 6)	pltype      ! Planting type (Ratoon or PlCane)    
          character 	(len = 6)	cropstatus  ! Dead or Alive
	      character 	(len = 6)	cropdstage  ! Development Stage - Only Sprout or Emergd
          
          !--- Arrays
          real      phprof(200,60)              ! Phytomer profiles
          real      tillerageprof(100,2)        ! Tillers age profiles
          real      Acanopy(3+1,5+1)            ! Instantaneous CO2 Assimilation Rate at three hours and five canopy depth in kg(CO2) ha-1(leaf) h-1 
          real      Qleaf(3+1,5+1)              ! Instantaneous par absorbed by leaves at three hours and five canopy depth in W m-2
          real      incpar(3,4)                 ! Incoming direct, difuse and total par radiation above canopy in three hours W m-2
          real      photo_layer_act(3)          ! Actual Total Daily Photosynthesis per canopy Layer  
          logical   fl_it_AG(200)               ! Above Ground Internode Flag
          logical   fl_lf_AG(200)               ! Above Ground Leaf Flag
          logical   fl_lf_alive(200)            ! This leaf is alive (T/F)? 
          
        end type CaneSamuca

      end module SAM_ModuleDefs

