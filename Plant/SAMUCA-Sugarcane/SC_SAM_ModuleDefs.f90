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
          real      diac
          real      dw_total
          real      dw_aerial
          real      dw_BG
          real      dw_it_ag
          real      dw_lf
          real      dw_rt
          real      fw_it_ag
          real      suc_it_ag
          real      pol
          real      lai
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
          real      tot_gresp_crop
          real      tot_mresp_crop
          real      gstd
          real      dtg
          real      drue_calc
          real      rue_calc
          real      cstat
              
          character 	(len = 6)	pltype      ! Planting type (Ratoon or PlCane)    
          character 	(len = 6)	cropstatus  ! Dead or Alive
	      character 	(len = 6)	cropdstage  ! Development Stage - Only Sprout or Emergd
          
        end type CaneSamuca

      end module SAM_ModuleDefs

