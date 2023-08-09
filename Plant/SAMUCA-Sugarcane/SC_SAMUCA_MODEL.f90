subroutine SAMUCA(CONTROL, ISWITCH,                                 &
!        CO2, DAYL, EOP, EP, EO, ES, HARVFRAC, NH4, NO3, SNOW,      &  !Input
         CO2, DAYL, EOP,                                            &  !Input
!        SOILPROP, tsoil, SRAD, SW, TMAX, TMIN, TRWUP, TRWU, EOS,   &  !Input
         SOILPROP, tsoil, SRAD,     TMAX, TMIN, TRWUP,              &  !Input
!        RWUEP1, TWILEN, YREND, YRPLT, WEATHER, IRRAMT,             &  !Input
         RWUEP1,         YREND, YRPLT, WEATHER,                     &  !Input
!        CANHT, HARVRES, KCAN, KTRANS, MDATE, NSTRES,               &  !Output
         CANHT,          KCAN, KTRANS, MDATE, NSTRES,               &  !Output
!        PORMIN, RLV, RWUMX,SENESCE, STGDOY, UNH4,                  &  !Output
                 RLV, RWUMX,         STGDOY,                        &  !Output
!        UNO3, XLAI, XHLAI, EORATIO)
               XLAI, XHLAI, EORATIO)

!     2023-01-26 chp removed unused variables in argument list:
!       EP, EO, ES, HARVFRAC, NH4, NO3, SNOW, 
!       SW, TRWU, EOS, TWILEN, IRRAMT, HARVRES, 
!       PORMIN, SENESCE, UNH4, UNO3, 

    !-------------------------------------------------------------------------
    !---------- Agronomic Modular Simulator for Sugarcane (SAMUCA) -----------
    !-------------------------------------------------------------------------
    !  Written in Microsoft Visual Studio FORTRAN              
	!  
    !  1st Version:
    !  Author: FABIO R. MARIN (2014)
    !  Scientific base - dx.doi.org/10.1590/S0103-90162014000100001
    !  Source Code     - murilodsv@bitbucket.org/murilodsv/samuca_standalone.git
    !
    !  2nd Version (latest):
    !  Authors: MURILO VIANNA; FABIO MARIN (2020)
    !  Scientific base - doi.org/10.1016/j.compag.2020.105361
    !  Source Code     - murilodsv@bitbucket.org/murilodsv/samuca.git
    !
    !  Dev. History:
	!  Edited in: Sep-2015 by Murilo dos S. Vianna  -> Model Review and Assessment
    !  Edited in: Feb-2016 by Murilo dos S. Vianna  -> Coupled to SWAP: https://scisoc.confex.com/crops/2017am/webprogram/Paper105395.html
    !  Edited in: Dec-2017 by Murilo dos S. Vianna  -> New Version Including Layered Photosynthesis, Source-Sink at Phytomer Level, Tillering: https://www.teses.usp.br/teses/disponiveis/11/11152/tde-01082018-150704/pt-br.php
    !  Edited in: Aug-2019 by Murilo dos S. Vianna  -> Refined and tested New Version with standalone platform: https://doi.org/10.1016/j.compag.2020.105361
    !  Edited in: Jan-2020 by Murilo dos S. Vianna  -> Coupled into DSSAT
    !  01/26/2023 CHP Reduce compile warnings: add EXTERNAL stmts, remove unused variables, shorten lines. 

   !------------------------------------------------------------------------
    
    !--- Global variables
    use ModuleDefs
    use SAM_ModuleDefs
    
    Implicit None
    EXTERNAL DAYLEN, FGROWTH, FIND_INP_SAM, GET_CULTIVAR_COEFF, GET_SPECIES_COEFF, IT_STRUC_PFAC,     &
      PGS, ROOT_PROFILE, SC_OPGROW_SAM, SC_OPGROW_SAM_DETAILED, SC_OPHARV_SAM, SC_WATERSTRESS, SOLAR, &
      SUBS_BALANCE, SUCROSE_CONTENT, TEMPERATURE_FACTOR, TILLER_SENES, TOTASS
    save
    
    integer     DYNAMIC         ! This is the dynamic call initialization, rate, integration (~task) (IN) 
    
    integer 	YREND	        ! (IN)
    integer 	YRPLT			! (IN)
    integer     STGDOY(20)      ! (IN)
	integer 	MDATE			! (OUT)
	real    	CO2             ! (IN)
    real    	DAYL            ! (IN)
    real    	EOP             ! (IN)
!   real    	EP              ! (IN)
!	real		EP1             ! (IN)
	real		RWUEP1          ! (IN)
	real		RWUEP2          ! (IN)
!   real    	EO              ! (IN)
!	real		EOS             ! (IN)
!	real		ES              ! (IN)
!   real    	HARVFRAC(2)     ! (IN)
!   real    	NH4(NL)         ! (IN)
!   real    	NO3(NL)         ! (IN)
!   real    	SNOW            ! (IN)
    real    	SRAD            ! (IN)    
!   real    	SW(NL)          ! (IN)
    real    	TMAX            ! (IN)
    real    	TMIN            ! (IN)
    real    	TRWUP           ! (IN)
!   real    	TRWU            ! (IN)
!   real    	TWILEN          ! (IN)
!   real 		IRRAMT          ! (IN)	
	real     	CANHT			! (OUT)
    real     	KCAN            ! (OUT)
    real     	KTRANS          ! (OUT)
    real     	NSTRES			! (OUT)
!   real     	PORMIN			! (OUT)
    real     	RLV(NL)			! (OUT)	    
    real        RWUMX           ! (OUT)
!   real        UNH4(NL)        ! (OUT)
!   real        UNO3(NL)        ! (OUT)
    real        XLAI            ! (OUT)
    real        XHLAI           ! (OUT)
    
    !--- DSSAT composite variables:
	TYPE (ControlType) CONTROL
	TYPE (SoilType)    SOILPROP
	TYPE (SwitchType)  ISWITCH
!	Type (ResidueType) HARVRES 
!	Type (ResidueType) SENESCE
	Type (WeatherType) WEATHER
    
    
    
    !--- Local composite variables:
    type (CaneSamuca)   CaneCrop
    
    logical     cf_err          ! Error flag when reading .CUL and .ECO parameters
    logical     spc_error       ! Error flag when reading .SPE parameters
    
    !-----------------------------------------------------------!-------------------------------------------------------------------------------!
    ! Local Variables and Input Parameters                      ! Description                                                                   !
    !-----------------------------------------------------------!-------------------------------------------------------------------------------!
    integer		task                                      		! Model task:           1 = initialization, 2 = step rate and integration of potential rates, 3 = step rate and integration of actual rates
    integer     method_pop                                      ! Tillering Method:     1 = Cdays; 2 = Cdays + Light Transmission; 3 = Source-Sink
    integer		nseason                                   		! Crop season counter
    integer		atln                                      		! Absolute tiller number
    integer		atln_now                                  		! Absolute tiller number at current timestep (storer)
    integer		dn_lf_alive_dewlap                        		! Daily Rate for new leaves with dewlap
    integer		maxdgl                                    		! Max number of developed green leaves on a stalk [parameter]
    integer		maxgl                                     		! Max total number of green leaves on a stalk (maxgl > maxdgl) [parameter]
    integer		n_it                                      		! Number of internodes at primary stalk
    integer		n_it_ag                                   		! Number of internodes at primary stalk aboveground
    integer		n_it_bg                                   		! Number of internodes at primary stalk belowground
    integer		n_lf                                      		! Total number of leaves initiated at primary stalk
    integer		n_lf_ag                                   		! Total number of leaves initiated at primary stalk when its aboveground
    integer		n_lf_ag_dewlap                            		! Number of developed green leaves
    integer		n_lf_alive                                		! Number of alive leaves
    integer		n_lf_alive_ag                             		! Number of alive leaves at primary stalk initiated when internodes were aboveground 
    integer		n_lf_alive_bg                             		! Number of alive leaves at primary stalk initiated when internodes were belowground 
    integer		n_lf_alive_dewlap                         		! Number of alive developed (dewlap) leaves
    integer		n_lf_alive_juveni                         		! Number of alive non-developed (without dewlap formed) leaves
    integer		n_lf_alive_juveni_ag                      		! Number of alive non-developed (without dewlap formed) leaves aboveground 
    integer		n_lf_alive_juveni_bg                      		! Number of alive non-developed (without dewlap formed) leaves belowground 
    integer		n_lf_bg                                   		! Total Number of leaves initated when internodes were belowground
    integer		n_lf_dead                                 		! Total Number of senesced leaves
    integer		n_lf_dead_ag                              		! Total Number of senesced leaves aboveground
    integer		n_lf_dead_bg                              		! Total Number of senesced leaves belowground
    integer		n_lf_it_form                              		! Number of leaves appeared before internode formation (#/tiller) [parameter]
    integer		n_lf_when_stk_emerg                       		! Number of leaves appeared before stalks emerges at soil surface (#/tiller) [parameter]
    integer		n_ph                                      		! Number of phytomers at primary stalk
    integer		n_ph_ag                                   		! Number of phytomers at primary stalk initiated aboveground
    integer		n_ph_bg                                   		! Number of phytomers at primary stalk initiated belowground
    integer		nphy_bground                              		! Total number of phytomers at primary stalk initiated belowground
    integer		nsublay                                   		! Number of soil sublayers for root profile rate 
    integer		phy                                       		! Phytomer iterator
    integer		pos_it_bg                                 		! Position of first Below Ground Internode
    integer		sl                                        		! Soil layer iterator
    integer		tl                                        		! Tiller iterator
    integer     nlay                                            ! Number of soil layers
    integer     das                                             ! Days after simulation started
    integer     dap                                             ! Days after planting
    integer     year                                            ! year
    logical		fl_potential                              		! Flag for potential simulations only [no water/nutrients stress]
    logical		fl_appear_leaf                            		! Flag for appeared leaf [leaf can be initiated but not yet visible on the top of stalks]
    logical		fl_hasleaf                                		! Flag indicating if internode has a leaf still attached [F=sensesced]
    logical		fl_it_visible                             		! Flag for internodes that can be seen on top stalks 
    logical		fl_lf_ag_phy                              		! Flag indicating if leaf is aboveground
    logical		fl_shed_leaf                              		! Flag indicating if leaf was sensesced
    logical		fl_stalk_emerged                          		! Flag indicating stalks emergence at soil surface
    logical		fl_tiller_decrease                        		! Flag for start of tiller senescence phase
    logical		fl_tiller_increase                        		! Flag for start of tiller increase phase
    logical		fl_tiller_peaked                          		! Flag for peak of tillering phase
    logical		fl_tiller_stop                            		! Flag for stabilisation of tillering population phase
    logical		fl_use_reserves                           		! Flag controlling when the crop should use reserves for growing or not
    logical     flcropalive                                     ! Flag indicating if crop is alive
    real		agefactor_fac_amax                        		! Age factor parameter on amax [photsynthesis]
    real		agefactor_fac_rue                         		! Age factor parameter on rue [when usinf RUE approach]
    real		agefactor_fac_per                         		! Age factor parameter on per [plant elongation]
    real		max_lf_dw                                 		! Maximum dry weight of a fully-developed leaf without supply restrictions [parameter]
    real		init_stalkfw                              		! Fresh weight of cutted stalks used at planting date [used to initialise the model]
    real		init_stalkht                              		! Height of cutted stalks at planting date [used to initialise the model]
    real		nstalks_planting                          		! Number of stalks used at planting [used to initialise the model]
    real		ini_nstk                                  		! Initial number of stalks [plants m-2]
    real		tilleragefac                              		! Correction factor to extrapolate source-sink relationship to younger tillers [this avoids explicit calculation at every tiller-phytomer]
    real		initcropdepth                             		! Initial crop soil depth 
    real		init_plantdepth_ratoon                    		! Initial crop soil depth for ratooning [parameter]
    real		dw_rt                                     		! Dry weight of roots
    real		max_rt_dw                                 		! Maximum roots dry weights of a fully-developed root system without supply restrictions [parameter]
    real		dw_it_bg                                  		! Dry weight of belowground internodes
    real		str_it_bg                                 		! Dry weight of internodes structural fraction
    real		sug_it_bg                                 		! Weight of sugars within internodes
    real		suc_it_bg                                 		! Weight of sucrose within internodes sugars
    real		hex_it_bg                                 		! Weight of hexoses within internodes sugars
    real		dw_it                                     		! Dry weight of internodes
    real        dw_it_dead                                      ! Dry weight of dead internodes [tillering senescence]
    real        dw_it_dead_AG                                   ! Dry weight of dead internodes aboveground [tillering senescence]
    real        dw_it_dead_BG                                   ! Dry weight of dead internodes belowground [tillering senescence]
    real		ini_dw_rt                                 		! Initial dry weight of the root system
    real		rootleftfrac                              		! Fraction of roots that will remain alive from last cut [1-rootleftfrac=roots that imediately dies]
    real		dw_total	                              		! Total dry weight of the crop
    real		age_it_phy                                		! Age of internode [thermal-time]
    real		age_lf_phy                                		! Age of leaf [thermal-time]
    real		agefactor_amax                            		! Calculated Age factor on amax 
    real		agefactor_per                             		! Calculated Age factor on RUE 
    real		agefactor_rue                             		! Calculated Age factor on per 
    real		amax_conv                                 		! Converted amax [kgCO2 ha-1 h-1]
    real		amax_mod                                  		! Amax modified after stress factors
    real		amaxfbfac                                 		! Amax stress factor
    real		avail_subs_crop                           		! Available substrates for the crop
    real		c_check_tol                               		! Carbon balance check
    real		c_scattering                              		! Canopy light scattering coefficient
    real		can_ipar                                  		! Canopy intercepted PAR
    real		chudec_lt                                 		! Thermal time at start of tillering senescence phase
    real		chumat_lt                                 		! Thermal time at start of tillering stabilisation phase
    real		co2_pho_res_end                           		! Parameter controlling crop response to atmospheric CO2
    real		co2_pho_res_ini                           		! Parameter controlling crop response to atmospheric CO2
    real		cr_source_sink_ratio                      		! Crop source-sink ratio
    real		cr_source_sink_ratio_ruse                 		! Source-sink ratio threshold crop can use its reserves [parameter]
    real		dage_it_phy                               		! Rate of internode aging 
    real		dage_lf_phy                               		! Rate of leaf aging 
    real        ddw                                             ! Rate of crop dry weight
    real		ddw_it                                    		! Rate of internode dry weight
    real		ddw_it_ag                                 		! Rate of aboveground internode dry weight
    real		ddw_it_ag_dead                            		! Rate of aboveground senesced internode dry weight
    real		ddw_it_bg                                 		! Rate of belowground internode dry weight
    real		ddw_it_bg_dead                            		! Rate of belowground senesced internode dry weight
    real		ddw_it_dead                               		! Rate of senesced internode dry weight
    real		ddw_it_phy_growth                         		! Rate of internode growth
    real		ddw_it_phy_reserves                       		! Rate of internode reserves
    real		ddw_lf                                    		! Rate of green leaves dry weight
    real		ddw_lf_ag                                 		! Rate of aboveground green leaves dry weight
    real		ddw_lf_appear                             		! Rate of appeared green leaves dry weight
    real		ddw_lf_bg                                 		! Rate of belowground green leaves dry weight
    real		ddw_lf_dead                               		! Rate of senesced leaves dry weight still attached to internodes
    real		ddw_lf_shed                               		! Rate of senesced leaves dry weight falling on the ground
    real		ddw_rt                                    		! Rate of root dry weights
    real		ddw_rt_dead                               		! Rate of sensesced root dry weights
    real		dead_lai                                  		! Leaf area index of dry leaves that are still attached to internodes
    real		diac_at_emergence                         		! Thermal time at crop emergence
    real		diacem                                    		! Thermal time from emergence
    real		diacsoil                                  		! Thermal time with soil temperature
    real		diacsoilem                                		! Thermal time from emergence with soil temperature
    real		diair                                     		! Degree-day using air temperature
    real		diam_stk                                  		! Stalk diameter
    real		diphy                                     		! Degree-day for internode
    real		disoil                                    		! Degree-day using soil temperature
    real		dla_gain_ref_till                         		! Rate of leaf area gain at reference primary tiller
    real		dla_phy                                   		! Rate of leaf area gain at phytomer
    real		dlai_dead                                 		! Rate of sensesced leaf area index
    real		dlai_gain                                 		! Rate of gained leaf area index due to growth
    real		dlai_gain_appear                          		! Rate of gained leaf area index due to appearance of new leaves
    real		dlai_shed                                 		! Rate of dry leaf area falling on the ground
    real		dnstk                                     		! Rate of tillering
    real		dnstk_dead_rate                           		! Rate of tillering senescence
    real		dphy_stimuli                              		! Rate of stimuli for new phytomer creation
    real		dr_itss                                   		! Relative sink-strenght of internodes
    real		dr_lfss                                   		! Relative sink-strenght of leaves
    real		dr_rtss                                   		! Relative sink-strenght of roots
    real		drdepth                                   		! Root front velocity
    real		dshootext_bg                              		! Calculated rate of shoot elongation belowground
    real		dshootext_bg_rate                         		! Parameter controling shoot elongation belowground
    real		dstr_it_ag                                		! Rate of dry weight of aboveground internode structural parts
    real		dstr_it_ag_dead                           		! Rate of dry weight of senesced aboveground internode structural parts
    real		dstr_it_bg                                		! Rate of dry weight of belowground internode structural parts
    real		dstr_it_bg_dead                           		! Rate of dry weight of senesced belowground internode structural parts
    real		dstr_it_phy                               		! Rate of dry weight of internode in phytomer
    real		dstr_it_phy_growth                        		! Rate of dry weight growth of internode in phytomer
    real		dsubsres                                  		! Rate of substrate reserves balance
    real		dsubsres_it                               		! Rate of substrate reserves to internodes
    real		dsubsres_lf                               		! Rate of substrate reserves to leaves
    real		dsubsres_ratio                            		! Rate of substrate reserves ratio
    real		dsubsres_rt                               		! Rate of substrate reserves to roots
    real		dsug_corr_fac_ag                          		! Rate correction of sugars reserves aboveground
    real		dsug_corr_fac_bg                          		! Rate correction of sugars reserves belowground
    real		dsug_it_ag                                		! Rate of sugar weight in aboveground internodes
    real		dsug_it_ag_dead                           		! Rate of sugar weight in sensesced aboveground internodes
    real		dsug_it_bg                                		! Rate of sugar weight in belowground internodes
    real		dsug_it_bg_dead                           		! Rate of sugar weight in sensesced belowground internodes
    real		dsug_it_phy                               		! Rate of sugar weight in internode phytomer
    real		dsug_it_phy_growth                        		! Rate of sugar weight in internode phytomer to growth
    real		dsug_it_phy_reserves                      		! Rate of sugar weight in internode phytomer to reserves
    real		dswat_ddws                                		! Parameter controlling dry-to-fresh weight conversion
    real		dswat_dsuc                                		! Parameter controlling dry-to-fresh weight conversion
    real		dtcrss                                    		! Rate of Total substrates needed for crop growth
    real		dtg                                       		! Rate of carbon assimilation
    real		dtg_avail_it                              		! Rate of carbon assimilation available to internodes
    real		dtg_avail_it_ag                           		! Rate of carbon assimilation available to aboveground internodes
    real		dtg_avail_it_ag_ref_till                  		! Rate of carbon assimilation available to aboveground internodes at primary stalk
    real		dtg_avail_it_bg                           		! Rate of carbon assimilation available to belowground internodes
    real		dtg_avail_it_bg_ref_till                  		! Rate of carbon assimilation available to belowground internodes at primary stalk
    real		dtg_avail_it_phy                          		! Rate of carbon assimilation available to the phytomer
    real		dtg_avail_it_ref_till                     		! Rate of carbon assimilation available to internodes at primary stalk
    real		dtg_avail_lf                              		! Rate of carbon assimilation available to leaves
    real		dtg_avail_lf_phy                          		! Rate of carbon assimilation available to leaves at phytomer
    real		dtg_avail_lf_ref_till                     		! Rate of carbon assimilation available to leaves at primary stalk
    real		dtg_avail_rt                              		! Rate of carbon assimilation available to roots
    real		dtitss                                    		! Rate of substrate demand from internodes
    real		dtitss_ag                                 		! Rate of substrate demand from aboveground internodes
    real		dtitss_ag_ref_till                        		! Rate of substrate demand from aboveground internodes at primary stalk
    real		dtitss_bg                                 		! Rate of substrate demand from belowground internodes
    real		dtitss_bg_ref_till                        		! Rate of substrate demand from belowground internodes at primary stalk
    real		dtitss_phy                                		! Rate of substrate demand from internodes at phytomer level
    real		dtitss_ref_till                           		! Rate of substrate demand from internodes at primary stalk
    real		dtlfss                                    		! Rate of substrate demand from leaves
    real		dtlfss_phy                                		! Rate of substrate demand from leaves at phytomer level
    real		dtlfss_ref_till                           		! Rate of substrate demand from leaves at primary stalk
    real		dtot_str_dw_ref_till                      		! Fraction of stalk extension partitioned among internodes as function of structural gain
    real		dtrtss                                    		! Rate of substrate demand from roots
    real		dw_aerial                                 		! Dry weight of aerial crop parts
    real		dw_it_phy                                 		! Dry weight of internode at phytomer level
    real		dw_lf                                     		! Dry weight of green leaves
    real        dw_lf_dead                                      ! Dry weight of sensesced leaves
    real		dw_lf_ag                                  		! Dry weight of green leaves aboveground
    real		dw_lf_bg                                  		! Dry weight of green leaves belowground
    real		dw_lf_phy                                 		! Dry weight of green leaves at phytomer level
    real		dw_lf_shed_phy                            		! Dry weight of senesced leaves that fallen on the ground at phytomer level
    real		dw_ss_it                                  		! Internode sink-strenght
    real		dw_ss_it_phy                              		! Internode sink-strenght at phytomer level
    real		dw_ss_lf                                  		! Leaf sink-strenght
    real		dw_ss_lf_phy                              		! Leaf sink-strenght at phytomer level
    real		dw_ss_rt                                  		! Root sink-strenght
    real		dwat_it_ag                                		! Water fraction rate of Stalks aboveground
    real		dwat_it_ag_dead                           		! Water fraction rate of senesced Stalks aboveground
    real		eff_conv                                  		! Canopy Quantum Efficiency [kgCO2 ha-1 h-1 (J m-2 s-1)-1]
    real		eff_mod                                   		! Canopy Quantum Efficiency after stress factors
    real		effective_rd                              		! Effective root depth where roots are actively growing
    real		end_tt_it_growth                          		! Thermal time internode stop growing
    real		end_tt_lf_growth                          		! Thermal time leaf stop growing
    real		end_tt_rt_growth                          		! Thermal time roots stop growing
    real		exc_dtg_it                                		! Exceeded Substrates in internodes
    real		exc_dtg_lf                                		! Exceeded Substrates in leaves
    real		exc_dtg_rt                                		! Exceeded Substrates in roots
    real		fdeadlf                                   		! Fraction of dryied leaves blade area considered on canopy light transmission (0-1) [parameter]
    real		frac_ag                                   		! Fraction of internodes growing aboveground
    real		frac_bg                                   		! Fraction of internodes growing belowground
    real		frac_hex_bg                               		! Fraction of hexoses belowground
    real		frac_li                                   		! Fraction of light intercepted
    real		frac_suc_bg                               		! Fraction of sugars belowground
    real		fw_it_ag                                  		! Stalk Fresh Mass
    real		gresp                                     		! Growth respiration of crop
    real		gresp_it                                  		! Growth respiration of internodes
    real		gresp_it_phy                              		! Growth respiration of internodes at phytomer level
    real		gresp_lf                                  		! Growth respiration of leaves
    real		gresp_lf_phy                              		! Growth respiration of leaves at phytomer level
    real		gresp_rt                                  		! Growth respiration of roots
    real		hex_it_ag                                 		! Hexoses at aboveground internodes
    real		hex_it_ag_ref_till                        		! Hexoses at aboveground internodes at primary stalk
    real		hex_it_bg_ref_till                        		! Hexoses at belowground internodes at primary stalk
    real		hex_it_phy                                		! Hexoses at aboveground internodes at phytomer level
    real		hex_min                                   		! Minimum hexose concentration at internode
    real		hour                                      		! Hour counter
    real		ini_dw_lf_phy                             		! Initial dry weight of new leaves
    real		ini_la                                    		! Initial leaf area index
    real		init_leaf_area                            		! Initial leaf area of single leaf    
    real		it_struc_pfac_delta                       		! Parameter controlling structural carbon partitioning in internodes
    real		it_struc_pfac_max                         		! Parameter controlling structural carbon partitioning in internodes
    real		it_struc_pfac_min                         		! Parameter controlling structural carbon partitioning in internodes
    real		it_struc_pfac_rate                        		! Parameter controlling structural carbon partitioning in internodes
    real		it_struc_pfac_tb                          		! Parameter controlling structural carbon partitioning in internodes
    real		it_struc_pfac_te                          		! Parameter controlling structural carbon partitioning in internodes
    real		it_struc_pfac_temp_max_red                		! Parameter controlling structural carbon partitioning in internodes
    real		it_struc_pfac_tm                          		! Parameter controlling structural carbon partitioning in internodes
    real		it_struc_pfac_wate_max_red                		! Parameter controlling structural carbon partitioning in internodes
    real		it_struc_tb_end                           		! Parameter controlling structural carbon partitioning in internodes
    real		it_struc_tb_ini                           		! Parameter controlling structural carbon partitioning in internodes
    real		it_struc_to1                              		! Parameter controlling structural carbon partitioning in internodes
    real		it_struc_to2                              		! Parameter controlling structural carbon partitioning in internodes
    real		k                                         		! Canopy light extinction coefficient
    real		kmr_it_phy                                		! Michaelis–Menten kinetics of the O2 on internode at phytomer level
    real		kmr_leaf                                  		! Michaelis–Menten kinetics of the O2 on Leaves
    real		kmr_root                                  		! Michaelis–Menten kinetics of the O2 on Roots
    real		kmr_stem                                  		! Michaelis–Menten kinetics of the O2 on Stems[structural]
    real		kmr_stor                                  		! Michaelis–Menten kinetics of the O2 on Reserves 
    real		la_lf_shed_phy                            		! Leaf area falling to ground at phytomer level
    real		lai_ass                                   		! Leaf area index used for carbon assimilation
    real		laimod                                    		! Leaf area index used for light transmission through the canopy
    real		lf_dpos                                   		! Dead leaf position below the living leaves profile
    real		lgpf                                      		! Initial leaf partitioning factor
    real	    sgpf		                                    ! Initial stem partitioning factor
    real		lt                                        		! Fraction of Transmitted Light Through Canopy
    real		ltthreshold                               		! Threshold of light transmited through canopy to start tiller senescence (0-1)
    real		maintenance_factor_crop                   		! Balance factor of crop maintenance respiration
    real		maintenance_factor_it                     		! Balance factor of internode maintenance respiration
    real		maintenance_factor_it_ag                  		! Balance factor of aboveground internode maintenance respiration
    real		maintenance_factor_it_bg                  		! Balance factor of belowground internode maintenance respiration
    real		maintenance_factor_it_phy                 		! Balance factor of internode maintenance respiration at phytomer level
    real		maintenance_factor_lf                     		! Balance factor of leaves maintenance respiration
    real		maintenance_factor_lf_phy                 		! Balance factor of leaves maintenance respiration at phytomer level
    real		maintenance_factor_rt                     		! Balance factor of roots maintenance respiration
    real		max_ini_la                                		! Maximum initial leaf area
    real		max_it_dw                                 		! Maximum dry weight of a fully-developed internode without supply restrictions [parameter]
    real		max_it_dw_bg                              		! Maximum dry weight of a fully-developed belowground internode without supply restrictions
    real		max_it_dw_phy                             		! Maximum dry weight of a fully-developed internode without supply restrictions at phytomer level
    real		max_per_it                                		! Maximum internode elongation
    real		mid_tt_it_growth                          		! Thermal-time when internode growth rate is at peak
    real		mid_tt_lf_growth                          		! Thermal-time when leaf growth rate is at peak
    real		mid_tt_rt_growth                          		! Thermal-time when root growth rate is at peak
    real		mresp_it                                  		! Maintenance respiration of internodes
    real		mresp_it_phy                              		! Maintenance respiration of internodes at phytomer level
    real		mresp_lf                                  		! Maintenance respiration of leaves
    real		mresp_lf_phy                              		! Maintenance respiration of leaves at phytomer level
    real		mresp_rt                                  		! Maintenance respiration of roots
    real		n_lf_max_ini_la                           		! Number of leaves when leaves appear at it maximum initial area (#/tiller)
    real		n_lf_tiller                               		! Number of leaves per tiller
    real		nsenesleaf_effect                         		! Number of dry leaves to consider on canopy light transmission (#/tiller)
    real		nstk_at_appearance                        		! Number of stalks when a leaf appears
    real		nstk_now                                  		! Current tiller number (helper)
    real		par_rad                                   		! PAR radiation as 50% of SRAD
    real		per                                       		! Plant elongation rate
    real		per_hour                                  		! Plant elongation rate at hourly step
    real		per_it_phy                                		! Internode elongatino rate at phytomer level
    real		pho_fac_co2                               		! Stress factor due to atm CO2
    real		phy_stimuli                               		! Phytomer stimuli
    real		phyllochron                               		! Phyllochron
    real		plastochron                               		! Plastochron
    real		poppeak_lt                                		! Peak of stalk population
    real		q10_it_phy                                		! Internode proportional change in respiration with a 10 °C increase in temperature
    real		q10_leaf                                  		! Leaf proportional change in respiration with a 10 °C increase in temperature
    real		q10_root                                  		! Root proportional change in respiration with a 10 °C increase in temperature
    real		q10_stem                                  		! Stem[structural] proportional change in respiration with a 10 °C increase in temperature
    real		q10_stor                                  		! Reserves proportional change in respiration with a 10 °C increase in temperature
    real		rdprof                                    		! Root depth profile
    real		reduc_growth_factor_crop                  		! Reduced factor due to supply limitation
    real		reduc_growth_factor_it                    		! Reduced factor due to supply limitation in internodes
    real		reduc_growth_factor_it_ag                 		! Reduced factor due to supply limitation in internodes aboveground
    real		reduc_growth_factor_it_bg                 		! Reduced factor due to supply limitation in internodes belowground
    real		reduc_growth_factor_it_phy                		! Reduced factor due to supply limitation in internodes phytomer level
    real		reduc_growth_factor_lf                    		! Reduced factor due to supply limitation in leaves
    real		reduc_growth_factor_lf_phy                		! Reduced factor due to supply limitation in leaves phytomer level
    real		reduc_growth_factor_rt                    		! Reduced factor due to supply limitation in roots
    real		rel_ss_it_phy                             		! Relative sink-strength internode phytomer level
    real		rel_ss_lf_phy                             		! Relative sink-strength leaf phytomer level
    real		res_used_emerg                            		! Total reserves used before emergence (crop 'memory')
    real		res_used_emerg_fac                        		! Factor controlling when crop should use reserves or not based on reserves balance [parameter]
    real		reserves_used_growth_it                   		! Reserves used for internode growth
    real		reserves_used_growth_lf                   		! Reserves used for leaf growth
    real		reserves_used_growth_rt                   		! Reserves used for root growth
    real		reserves_used_mresp_crop                  		! Reserves used for maintenance respiration
    real		reserves_used_mresp_it                    		! Reserves used for maintenance respiration in internodes
    real		reserves_used_mresp_it_ag                 		! Reserves used for maintenance respiration in aboveground internodes
    real		reserves_used_mresp_it_bg                 		! Reserves used for maintenance respiration in belowground internodes
    real		reserves_used_mresp_it_phy                		! Reserves used for maintenance respiration in internodes at phytomer level
    real		reserves_used_mresp_lf                    		! Reserves used for maintenance respiration in leaves
    real		reserves_used_mresp_lf_phy                		! Reserves used for maintenance respiration in leaves at phytomer level
    real		reserves_used_mresp_rt                    		! Reserves used for maintenance respiration in roots
    real		rgpf                                      		! Initial partitioning factor to roots
    real		root_front_size                           		! Root front size
    real		rootdrate                                 		! Rooting depth rate
    real		rootshape                                 		! Shape of root profile
    real		rpup                                      		! Rate of belowground shoot upward elongation 
    real		rue_mod                                   		! RUE after stress factors [metpg=1]
    real		shared_it_str_bg                          		! Shared Structual parts among below ground internodes
    real		shared_it_sug_bg                          		! Shared Sugars among below ground internodes
    real		shootdepth                                		! Shoot depth before emergence
    real		soiltemperature                           		! soil temperature
    real		srlmax                                    		! Max specific root length density
    real		srlmin                                    		! Min specific root length density
    real		stk_h                                     		! Stalk height
    real		str_it_ag                                 		! Structural dry weight of aboveground internodes
    real		str_it_phy                                		! Structural dry weight of internode at phytoemr level
    real		subs_avail_growth_crop                    		! Substrates available for crop growth
    real		subs_avail_growth_it                      		! Substrates available for internodes growth
    real		subs_avail_growth_it_ag                   		! Substrates available for aboveground internodes growth
    real		subs_avail_growth_it_ag_ref_till          		! Substrates available for aboveground internodes growth at primary stalk
    real		subs_avail_growth_it_bg                   		! Substrates available for belowground internodes growth
    real		subs_avail_growth_it_bg_ref_till          		! Substrates available for belowground internodes growth at primary stalk
    real		subs_avail_growth_it_phy                  		! Substrates available for internodes growth phytomer level
    real		subs_avail_growth_it_ref_till             		! Substrates available for internodes growth at priamry stalk
    real		subs_avail_growth_lf                      		! Substrates available for leaf growth
    real		subs_avail_growth_lf_phy                  		! Substrates available for leaf growth phytomer level
    real		subs_avail_growth_lf_ref_till             		! Substrates available for leaf growth at primary stalk
    real		subs_avail_growth_rt                      		! Substrates available for root growth
    real		subsres                                   		! Available substrates for crop growth and maintenance
    real		subsres_avail_it                          		! Substrates available for internodes
    real		subsres_avail_it_ag                       		! Substrates available for internodes aboveground
    real		subsres_avail_it_ag_ref_till              		! Substrates available for internodes aboveground at primary stalk
    real		subsres_avail_it_bg                       		! Substrates available for internodes belowground
    real		subsres_avail_it_bg_ref_till              		! Substrates available for internodes belowground at primary stalk
    real		subsres_avail_it_phy                      		! Substrates available for internodes at phytomer level
    real		subsres_avail_it_ref_till                 		! Substrates available for internodes at primary stalk
    real		subsres_avail_lf                          		! Substrates available for leaves
    real		subsres_avail_lf_phy                      		! Substrates available for leaves at phytomer level
    real		subsres_avail_lf_ref_till                 		! Substrates available for leaves at primary stalk
    real		subsres_avail_rt                          		! Substrates available for roots
    real		suc_acc_ini                               		! Internode total sugars concentration where sucrose accumulation onsets (0-1)
    real		suc_frac_rate_ts                          		! Sucrose weight increment per unit of total sugars increment in internodes (TSUG > suc_acc_ini) (d[SUC]/d[TSUG])
    real		suc_it_ag                                 		! Sucrose weight in internodes
    real		suc_it_ag_ref_till                        		! Sucrose weight in aboveground internodes at primary stalk
    real		suc_it_bg_ref_till                        		! Sucrose weight in belowground internodes at primary stalk
    real		suc_it_phy                                		! Sucrose weight in internodes at phytomer level
    real		suc_min                                   		! Min sucrose concentration at internode
    real		sug_cont                                  		! Overall sugar content in dry mass basis
    real		sug_it_ag                                 		! Sugar weight in aboveground internodes
    real		sug_it_phy                                		! Sugar weight in internodes at phytomer level
    real		sup_ratio_it                              		! Ratio of substrate balance for internodes
    real		sup_ratio_it_ag                           		! Ratio of substrate balance for internodes aboveground
    real		sup_ratio_it_bg                           		! Ratio of substrate balance for internodes belowground
    real		sup_ratio_it_phy                          		! Ratio of substrate balance for internodes phytomer level
    real		sup_ratio_lf                              		! Ratio of substrate balance for leaves
    real		sup_ratio_lf_phy                          		! Ratio of substrate balance for leaves phytomer level
    real		sup_ratio_rt                              		! Ratio of substrate balance for roots
    real		supply_rate_it                            		! Supply rate for internodes
    real		supply_rate_it_ag                         		! Supply rate for internodes aboveground
    real		supply_rate_it_bg                         		! Supply rate for internodes belowground
    real		supply_rate_it_phy                        		! Supply rate for internodes phytomer level
    real		supply_rate_lf                            		! Supply rate for leaves
    real		supply_rate_lf_phy                        		! Supply rate for leaves phytomer level
    real		supply_rate_rt                            		! Supply rate for roots
    real		supply_used_crop                          		! Supply used by crop
    real		supply_used_dw_crop                       		! Supply used dry weight by crop
    real		supply_used_dw_it                         		! Supply used dry weight by internodes
    real		supply_used_dw_it_ag                      		! Supply used dry weight by internodes aboveground
    real		supply_used_dw_it_bg                      		! Supply used dry weight by internodes belowground
    real		supply_used_dw_it_phy                     		! Supply used dry weight by internodes at phytomer level
    real		supply_used_dw_lf                         		! Supply used dry weight by leaves
    real		supply_used_dw_lf_phy                     		! Supply used dry weight by leaves at phytomer level
    real		supply_used_dw_rt                         		! Supply used dry weight by roots
    real		supply_used_gresp_crop                    		! Supply used for crop growth respiration 
    real		supply_used_gresp_it                      		! Supply used for internodes growth respiration
    real		supply_used_gresp_it_ag                   		! Supply used for aboveground internodes growth respiration
    real		supply_used_gresp_it_bg                   		! Supply used for belowground internodes growth respiration
    real		supply_used_gresp_it_phy                  		! Supply used for internodes growth respiration at phytomer level
    real		supply_used_gresp_lf                      		! Supply used for leaves growth respiration
    real		supply_used_gresp_lf_phy                  		! Supply used for leaves growth respiration at phytomer level
    real		supply_used_gresp_rt                      		! Supply used for roots growth respiration
    real		supply_used_it                            		! Supply used by internodes
    real		supply_used_it_ag                         		! Supply used by aboveground internodes
    real		supply_used_it_bg                         		! Supply used by belowground internodes
    real		supply_used_it_phy                        		! Supply used by internodes at phytomer level
    real		supply_used_lf                            		! Supply used by leaves
    real		supply_used_lf_phy                        		! Supply used by leaves at phytomer level
    real		supply_used_mresp_crop                    		! Supply used for crop maitenance respiration 
    real		supply_used_mresp_it                      		! Supply used for internodes maitenance respiration 
    real		supply_used_mresp_it_ag                   		! Supply used for aboveground internodes maitenance respiration 
    real		supply_used_mresp_it_bg                   		! Supply used for belowground internodes maitenance respiration 
    real		supply_used_mresp_it_phy                  		! Supply used for internodes maitenance respiration at phytomer level
    real		supply_used_mresp_lf                      		! Supply used for leaves maitenance respiration 
    real		supply_used_mresp_lf_phy                  		! Supply used for leaves maitenance respiration at phytomer level
    real		supply_used_mresp_rt                      		! Supply used for roots maitenance respiration 
    real		supply_used_rt                            		! Supply used by roots 
    real		t_mresp                                   		! Temperature used for maintenance respiration
    real		tb0pho                                    		! Base temperature - Tb0 for photosynthesis
    real		tb1pho                                    		! Base temperature - Tb1 for photosynthesis [optimal]
    real		tb2pho                                    		! Base temperature - Tb2 for photosynthesis [optimal]
    real		tbfpho                                    		! Base temperature - Tbf for photosynthesis
    real		tbmax_per                                 		! Base temperature for plant elongation    
    real		tempfac_per                               		! Temperature factor for plant elongation
    real		tempfac_pho                               		! Temperature factor for photosynthesis
    real		tilleragefac_adjust                       		! Tiller age adjusting factor for young tillers to the primary tiller
    real		tillochron                                		! Tillochron
    real		tot_dw_ss_crop                            		! Total substrates needed for crop growth/maintenance respiration and growth
    real		tot_dw_ss_it                              		! Total substrates needed for internodes growth/maintenance respiration and growth
    real		tot_dw_ss_it_ag                           		! Total substrates needed for aboveground internodes growth/maintenance respiration and growth
    real		tot_dw_ss_it_bg                           		! Total substrates needed for belowground internodes growth/maintenance respiration and growth
    real		tot_dw_ss_lf                              		! Total substrates needed for leaves growth/maintenance respiration and growth
    real		tot_dw_ss_rt                              		! Total substrates needed for roots growth/maintenance respiration and growth
    real		tot_gresp_crop                            		! Total substrates needed for crop growth respiration
    real		tot_gresp_it                              		! Total substrates needed for internodes growth respiration
    real		tot_gresp_it_ag                           		! Total substrates needed for aboveground internodes growth respiration
    real		tot_gresp_it_bg                           		! Total substrates needed for belowground internodes growth respiration
    real		tot_gresp_lf                              		! Total substrates needed for leaves growth respiration
    real		tot_gresp_rt                              		! Total substrates needed for roots growth respiration
    real		tot_mresp_crop                            		! Total substrates needed for crop maitenance respiration
    real		tot_mresp_it                              		! Total substrates needed for internodes maitenance respiration
    real		tot_mresp_it_ag                           		! Total substrates needed for aboveground internodes maitenance respiration
    real		tot_mresp_it_bg                           		! Total substrates needed for belowground internodes maitenance respiration
    real		tot_mresp_lf                              		! Total substrates needed for leaves maitenance respiration
    real		tot_mresp_rt                              		! Total substrates needed for roots maitenance respiration
    real		tref_mr                                   		! Reference temperature for maintenance respiration
    real		tref_mr_it_phy                            		! Reference temperature for maintenance respiration at phytomer level
    real		ts_it_phy                                 		! Total sugars in internode
    real		tt_chumat_lt                              		! Thermal time required after peak of population for tillering stabilization (oCdays)
    real		wat_con                                   		! Fraction of water in stalks
    real		wat_it_ag                                 		! Fresh weight of aboveground internodes
    real        acc_par                                         ! Accumulated intercepted PAR
    real        dacc_par                                        ! Rate of intercepted PAR
    real        drue_calc                                       ! Calculated daily RUE
    real        rue_calc                                        ! Calculated RUE
    real        gstd                                            ! Growth stage 
    real        cstat                                           ! Crop status level
    real        rowsp				     	                    ! rowspacing  
    real        plantdepth                                      ! planting depth    
    real        rd                                              ! Root depth
    real        srl                                             ! Specific root length density
    real        dileaf                                          ! degree-days for leaf development
    real        z                                               ! zero helper fortran
    real        resp                                            ! Canopy respiration used for carbon assimilation [deprecated method] 
    real        pol                                             ! Percentage of sucrose content in fresh stalk biomass
    real        kc                                              ! Crop coefficient for evapotranspiration calculations
    real        maxlai                                          ! Maximum LAI hit throughout the season (needed for SC_OPHARV_SAM)     

    !--- Arrays Variables
    real        phprof(200,60)                                  ! Phytomer profile and attributes dimensions    
    real        ddw_rt_sl(nl)                                   ! Rate of dry weight of roots per soil layer
    real        geot(SOILPROP%NLAYR)                            ! Geotropism parameter
    real        dw_rt_prof(SOILPROP%NLAYR)                      ! Dry weight of roots per soil layer
    real        tillerageprof(100,2)                            ! Tiller age profile
    real        tempfac_h_per(24)                               ! 24 hours
    real        Acanopy(3+1,5+1)                                ! Instantaneous CO2 Assimilation Rate at three hours and five canopy depth in kg(CO2) ha-1(leaf) h-1 
    real        Qleaf(3+1,5+1)                                  ! Instantaneous par absorbed by leaves at three hours and five canopy depth in W m-2
    real        incpar(3,4)                                     ! Incoming direct, difuse and total par radiation above canopy in three hours W m-2
    real        photo_layer_act(3)                              ! Actual Total Daily Photosynthesis per canopy Layer  
    real        rgf(SOILPROP%NLAYR+1,3)                         ! Root growth factor
    real        lroot(SOILPROP%NLAYR)                           ! Root length
    real        drld(nl)                                        ! Rate of root lenght density 
    real        drld_dead(nl)                                   ! Rate of root lenght density senescence
    real 	    bottom				(nl)                        ! bottom level of soil layer 
    real        upper               (nl)                        ! upper level of soil layer
    real        slthickness         (nl)                        ! soil	layer thickness 
    real        dep                 (nl)                        ! depth of soil layer [=bottom]
    real        rld                 (nl)                        ! root length density at soil layer
    real        thour(24)                                       ! Hourly temperature
    real        tsoil(nl)                                       ! Soil temperature at soil layer
    logical     fl_it_AG(200)                                   ! Above Ground Internode Flag
    logical     fl_lf_AG(200)                                   ! Above Ground Leaf Flag
    logical     fl_lf_alive(200)                                ! Flag of alive leaf    
    
    !--- Real Functions
    real        fgrowth                                         ! Flexible growth function
    real        tiller_senes                                    ! Tiller senescence function    
    real		it_struc_pfac                             		! Structural partitioning factor in internodes
    real		temperature_factor                        		! Interpolation function
    
    !--- Variable further coupled to DSSAT states or outputs
    real 	    AMAX		                                    ! Assimilation rate at light saturation point (mmol/m2/s)
    real 	    EFF			                                    ! Carboxylation efficiency (mmol[CO2]/m2/s (mmol[PPFD]/m2/s)-1)
    real 	    PHTMAX		                                    ! Parameter controlling carbon assimilation [deprecated method] 
    real 	    PARMAX		                                    ! Parameter controlling carbon assimilation [deprecated method] 
    real 	    CCMP		                                    ! Parameter controlling carbon assimilation [deprecated method] 
    real 	    CCMAX		                                    ! Parameter controlling carbon assimilation [deprecated method] 
    real 	    CCEFF		                                    ! Parameter controlling carbon assimilation [deprecated method] 
    real 	    RUE			                                    ! Radiation use efficiency [metpg=1]
    real 	    TB			                                    ! Base temperature for crop development
    real 	    TBPER		                                    ! Base temperature for crop elongation
    real 	    CHUSTK		                                    ! TT for stalk emergence (oCdays)
    real 	    CHUPEAK		                                    ! TT for tillering peak (only used when competition for light is switched-off, method_pop = 1) (oCdays)
    real 	    CHUDEC                                          ! TT for tillering senescence (oCdays)
    real 	    CHUMAT		                                    ! TT for population stabilization (oCdays)
    real 	    POPMAT		                                    ! Tiller population at tillering stabilization (tillers/m2)
    real 	    POPPEAK		                                    ! Number of tillers at peak of population (tillers/m2, only used if competition for light is switched-off,method_pop=1)
    real 	    SLA			                                    ! Specific Leaf Area (cm2/g)
    real 	    RDM			                                    ! Maximum rooting depth
    real 	    KDIF		                                    ! Difuse light extinction coefficient
    real 	    DPERCOEFF	                                    ! Maximum plant expansion rate (mm/h)
    real 	    MLA			                                    ! Maximum leaf area (cm2)
    real 	    KC_MIN		                                    ! Minimum Crop coefficient for evapotranspiration calculations
    real 	    EORATIO		                                    ! Ratio used to calculate actual Crop coefficient for evapotranspiration
    real 	    T_MAX_WS_PHO                                    ! Supply/Demand ratio where water stress effect is maximum for photosynthesis (0-1)
    real 	    T_MID_WS_PHO                                    ! Supply/Demand ratio where half of maximum water stress effect (0.5) for photosynthesis occurs (0-1)
    real 	    T_MIN_WS_PHO                                    ! Supply/Demand ratio where water stress effect on photosynthesis onsets (0-1)
    real	    T_MAX_WS_EXP                                    ! Supply/Demand ratio where water stress effect is maximum for expansion (0-1)
    real	    T_MID_WS_EXP                                    ! Supply/Demand ratio where half of maximum water stress effect (0.5) for expansionoccurs (0-1)
    real	    T_MIN_WS_EXP                                    ! Supply/Demand ratio where water stress effect on expansion onsets (0-1)
    real	    MAXLAI_EO	                                    ! Max LAI for Kc calculations
    real	    TBM			                                    ! Maximum temperature when crop development stops
    real	    THRESHEWS	                                    ! Thresold controlling water stress curve
    real	    RWUMAX			                                ! Max root water uptake
    real	    T_MAX_WS_FPF                                    ! Parameeter controlling partitioning factors response to waterstress
    real	    T_MID_WS_FPF                                    ! Parameeter controlling partitioning factors response to waterstress
    real	    T_MIN_WS_FPF                                    ! Parameeter controlling partitioning factors response to waterstress
    real	    T_MAX_WS_TIL                                    ! Parameeter controlling tillering response to waterstress
    real	    T_MID_WS_TIL                                    ! Parameeter controlling tillering response to waterstress
    real	    T_MIN_WS_TIL                                    ! Parameeter controlling tillering response to waterstress    
    real	    DW_IT_AG	                                    ! Dry weight of aboveground internodes
    real	    NSTK		                                    ! Tiller population
    real	    LAI			                                    ! Leaf area index
    real	    DIAC		                                    ! Accumulated thermal-time
    real	    DI		                                        ! Daily degree-day
    real	    LI		                                        ! Fraction of light interception
    real	    SWFACP                                          ! Waterstress factor on photosynthesis
    real	    SWFACE	                                        ! Waterstress factor on elongation
    real	    SWFACT	                                        ! Waterstress factor on tillering
    real	    SWFACF	                                        ! Waterstress factor on 
    real	    TMN		                                        ! Daily mean air temperature
    integer	    DOY		                                        ! Day of year
    real	    LAT_SIM	                                        ! Latitude
    integer     NDWS                                            ! Consecutive days of waterstress
    integer     NDEWS	                                        ! Consecutive days of extreme waterstress
    logical     FLEMERGED	                                    ! Flag indicating crop has emerged
    logical     writedcrop                                      ! Flag for creating detailed output
    logical     usetsoil                                        ! Flag for using soil temperature
    logical     mulcheffect                                     ! Flag for using mulch cover effect [e.g. soil temperature]
    logical     ratoon				                            ! Flag indicating if its a ratoon crop
    integer     metpg                                           ! Photosynthesis method
    integer     seqnow                                          ! Counter of sequential cuts [ratoons]
    integer     nratoon                                         ! Ratoon counter
    integer     max_ratoon                                      ! Max number of sequential ratoons
    real        DEC, SNDN, SNUP, CLOUDS, ISINB, S0N             ! DSSAT astro calculations
    real        par_sim                                         ! simulated PAR
    character 	(len = 6)	pltype                              ! Character indicating Planting type (Ratoon or PlCane)    
    character 	(len = 6)	cropstatus                          ! Character indicating  Dead or Alive
	character 	(len = 6)	cropdstage                          ! Character indicating Development Stage - Only Sprout or Emergd
    character   (len=7)     YRDOY_ch                            ! Character indicating year and doy [from CONTROL%YRDOY]
    
    !--- Helpers for reading integer parameter as real and convert afterwards
    !--- This is easier than creating a dedicated subroutine only for that
    real        maxgl_r
    real        maxdgl_r
    real        n_lf_when_stk_emerg_r
    real        n_lf_it_form_r   
    real        ratoon_r
    
    !-----------------------------------------------------------!-------------------------------------------------------------------------------!
    
    save
    
    !--- define fixed parameters
    parameter   (z = 1.e-14)
    
    !--- Coupling DSSAT DYNAMIC control variable with SAMUCA task variable
    dynamic = control % dynamic
    
    !--- DSSAT DYNAMIC variable values (From ModuleDefs):
    ! RUNINIT  = 1, 
    ! INIT     = 2,  !Will take the place of RUNINIT & SEASINIT (not fully implemented)
    ! SEASINIT = 2, 
    ! RATE     = 3,
    ! EMERG    = 3,  !Used for some plant processes.  
    ! INTEGR   = 4,  
    ! OUTPUT   = 5,  
    ! SEASEND  = 6,
    ! ENDRUN   = 7 
    
    !---------------------!
    !--- RUNNING MODES ---!
    !---------------------!
    
    !--- Sequential mode    -> Control%RNMODE .eq. 'Q':
    !--- The model will pass through the RUNINIT task for every run
    
    !--- Seasonal mode      -> Control%RNMODE .eq. 'N':
    !--- The model will pass through the RUNINIT task only at the first year (Use this for carrying over ratoon)
    
    !--- Setting up SAMUCA task based on CONTROL DYNAMIC
    if(dynamic .eq. RUNINIT)    task = 1    ! Initialize internal counters
    if(dynamic .eq. SEASINIT)   task = 2    ! Crop State Variable Initialization and reading crop parameters
    if(dynamic .eq. RATE)       task = 4    ! Step-Rate and Integration are in the same block for SAMUCA
    if(dynamic .eq. INTEGR)     return      ! Embedded in task=4 (consider moving move here)
    if(dynamic .eq. OUTPUT)     task = 5    ! Write output files
    if(dynamic .eq. SEASEND)    task = 6    ! Close PlantGro if Ratooning is not carring over
    if(dynamic .eq. ENDRUN)     return      ! Not in use by SAMUCA   
        
    !--- Linking Weather Variables
    tmax        = WEATHER % TMAX
    tmin        = WEATHER % TMIN
    srad        = WEATHER % SRAD
    lat_sim     = WEATHER % XLAT    
    tmn         = (tmax + tmin) * 0.5    
    
    !--- Hourly Temperature
    !--- The original version uses the model described by Parton & Logan (1981) 
    !--- Now we are using DSSAT's hourly temperature:
    thour = weather % tairhr
    
    !--- Time Control
    das         = CONTROL % DAS
    
    !--- To get year and doy as integers, we read YRDOY as character and break down
    write(YRDOY_ch, '(I7)')     CONTROL % YRDOY
    read(YRDOY_ch,'(I4)')       year
    read(YRDOY_ch,'(4X,I3)')    doy
            
    !--- Number of soil layers (according to soil profile)
    nlay        = SOILPROP % NLAYR
    bottom		= SOILPROP % DS
    slthickness = SOILPROP % DLAYR
    upper       = bottom - slthickness
    dep         = bottom        
    
    !--- Detailed output switch
    writedcrop = .false.
    if(ISWITCH % IDETL .eq. 'D') writedcrop = .true.
    
    !--- Go to task!
    goto(10,20,30,40,50,60) task
    
10  continue
    
    !--- Season count control
    seqnow  = 0
    nseason = 0
    nratoon = 0
    
    !--- Plant or Ratoon from the control file
    ratoon              = .false.  
    ratoon_r            = 0.d0
    call find_inp_sam(ratoon_r, 'RATOON', Control)
    if(ratoon_r .ge. 0.99) ratoon = .true.
        
    !--- Delete any detailed file regardless if detailed is switched on
    call SC_OPGROW_SAM_DETAILED (CONTROL, CaneCrop,YRPLT)
    
    !--- Prepare for summary outputs
    flemerged           = .false.
    maxlai              = 0.d0
    xlai                = 0.d0    
    swfacp				= 1.d0
    swface              = 1.d0
    STGDOY              = 0
    
    !--- Update flags for reading cultivar/species file 
    !--- Note: We are borrowing the below subroutines from CANEGRO for the sake of i/o
    call get_cultivar_coeff(maxgl_r,'dummy', CONTROL, CF_ERR)
    call get_species_coeff(tb,      'dummy', CONTROL, SPC_ERROR)
    
    call SC_OPHARV_SAM(CONTROL, ISWITCH,  &
    CaneCrop, flemerged, maxlai,          &
    swfacp, swface, STGDOY, XLAI, YRPLT)
    
    return
    
20  continue
        
    !-------------------------------!
    !--- Reading crop parameters ---!
    !-------------------------------!

    !-------------------------------------------------------------------!
    !--------------------------- IMPORTANT -----------------------------!
    !-------------------------------------------------------------------!
    !--- get_cultivar_coeff() is coming from CANEGRO (SC_COEFFS.f90)
    !--- Also add/remove new parameters in:
    !---    OPTEMPXY2K.for 
    !---    IPVAR.for
    !---    COMGEN.blk
    !--- Dont forget to update the .cul/.eco/.spe files too!
    !-------------------------------------------------------------------!
    
    !--- Reading crop parameters with MJ's subroutine used in CANEGRO
    !--- Cultivar parameters
    call get_cultivar_coeff(                       maxgl_r,          'MAXGL', CONTROL, CF_ERR)
    call get_cultivar_coeff(         n_lf_when_stk_emerg_r,    'N_LF_STK_EM', CONTROL, CF_ERR)
    call get_cultivar_coeff(                n_lf_it_form_r,   'N_LF_IT_FORM', CONTROL, CF_ERR)
    call get_cultivar_coeff(                      maxdgl_r,         'MAXDGL', CONTROL, CF_ERR)
    call get_cultivar_coeff(                          amax,           'AMAX', CONTROL, CF_ERR)
    call get_cultivar_coeff(                           eff,            'EFF', CONTROL, CF_ERR)
    call get_cultivar_coeff(                        chustk,         'CHUSTK', CONTROL, CF_ERR)
    call get_cultivar_coeff(                       chupeak,        'CHUPEAK', CONTROL, CF_ERR)
    call get_cultivar_coeff(                        chudec,         'CHUDEC', CONTROL, CF_ERR)
    call get_cultivar_coeff(                        chumat,         'CHUMAT', CONTROL, CF_ERR)
    call get_cultivar_coeff(                        popmat,         'POPMAT', CONTROL, CF_ERR)
    call get_cultivar_coeff(                       poppeak,        'POPPEAK', CONTROL, CF_ERR)
    call get_cultivar_coeff(                    tillochron,     'TILLOCHRON', CONTROL, CF_ERR)
    call get_cultivar_coeff(                           sla,            'SLA', CONTROL, CF_ERR)
    call get_cultivar_coeff(                           mla,            'MLA', CONTROL, CF_ERR)
    call get_cultivar_coeff(                   plastochron,    'PLASTOCHRON', CONTROL, CF_ERR)
    call get_cultivar_coeff(                init_leaf_area,   'INIT_LF_AREA', CONTROL, CF_ERR)
    call get_cultivar_coeff(                    max_ini_la,     'MAX_INI_LA', CONTROL, CF_ERR)
    call get_cultivar_coeff(                     max_it_dw,      'MAX_IT_DW', CONTROL, CF_ERR)
    call get_cultivar_coeff(              mid_tt_it_growth,  'MID_TT_IT_GRO', CONTROL, CF_ERR)
    call get_cultivar_coeff(              end_tt_it_growth,  'END_TT_IT_GRO', CONTROL, CF_ERR)
    call get_cultivar_coeff(              mid_tt_lf_growth,  'MID_TT_LF_GRO', CONTROL, CF_ERR)
    call get_cultivar_coeff(              end_tt_lf_growth,  'END_TT_LF_GRO', CONTROL, CF_ERR)
    
    !--- Ecotype parameters
    call get_cultivar_coeff(             nsenesleaf_effect,      'NS_LF_TIL', CONTROL, CF_ERR)
    call get_cultivar_coeff(               n_lf_max_ini_la,   'N_LF_MAX_ILA', CONTROL, CF_ERR)
    call get_cultivar_coeff(                        tb0pho,         'TB0PHO', CONTROL, CF_ERR)
    call get_cultivar_coeff(                        tb1pho,         'TB1PHO', CONTROL, CF_ERR)
    call get_cultivar_coeff(                        tb2pho,         'TB2PHO', CONTROL, CF_ERR)
    call get_cultivar_coeff(                        tbfpho,         'TBFPHO', CONTROL, CF_ERR)
    call get_cultivar_coeff(                         tbper,          'TBPER', CONTROL, CF_ERR)
    call get_cultivar_coeff(                     tbMax_per,      'TBMAX_PER', CONTROL, CF_ERR)
    call get_cultivar_coeff(                   ltthreshold,    'LTTHRESHOLD', CONTROL, CF_ERR)
    call get_cultivar_coeff(                       fdeadlf,        'FDEADLF', CONTROL, CF_ERR)
    call get_cultivar_coeff(                           rdm,            'RDM', CONTROL, CF_ERR)
    call get_cultivar_coeff(                     dpercoeff,      'DPERCOEFF', CONTROL, CF_ERR)
    call get_cultivar_coeff(                        rwuep1,         'RWUEP1', CONTROL, CF_ERR)
    call get_cultivar_coeff(                        rwuep2,         'RWUEP2', CONTROL, CF_ERR)
    call get_cultivar_coeff(                  t_max_ws_pho,   'T_MAX_WS_PHO', CONTROL, CF_ERR)
    call get_cultivar_coeff(                  t_mid_ws_pho,   'T_MID_WS_PHO', CONTROL, CF_ERR)
    call get_cultivar_coeff(                  t_min_ws_pho,   'T_MIN_WS_PHO', CONTROL, CF_ERR)
    call get_cultivar_coeff(                  t_max_ws_exp,   'T_MAX_WS_EXP', CONTROL, CF_ERR)
    call get_cultivar_coeff(                  t_mid_ws_exp,   'T_MID_WS_EXP', CONTROL, CF_ERR)
    call get_cultivar_coeff(                  t_min_ws_exp,   'T_MIN_WS_EXP', CONTROL, CF_ERR)
    call get_cultivar_coeff(                   frac_suc_BG,    'FRAC_SUC_BG', CONTROL, CF_ERR)
    call get_cultivar_coeff(                   frac_hex_BG,    'FRAC_HEX_BG', CONTROL, CF_ERR)
    call get_cultivar_coeff(        init_plantdepth_ratoon,    'INIT_PD_RAT', CONTROL, CF_ERR)
    call get_cultivar_coeff(               it_struc_tb_ini,  'IT_STR_TB_INI', CONTROL, CF_ERR)
    call get_cultivar_coeff(                  it_struc_to1,     'IT_STR_TO1', CONTROL, CF_ERR)
    call get_cultivar_coeff(                  it_struc_to2,     'IT_STR_TO2', CONTROL, CF_ERR)
    call get_cultivar_coeff(               it_struc_tb_end,  'IT_STR_TB_END', CONTROL, CF_ERR)
    call get_cultivar_coeff(             it_struc_pfac_max,  'IT_STR_PF_MAX', CONTROL, CF_ERR)
    call get_cultivar_coeff(             it_struc_pfac_min,  'IT_STR_PF_MIN', CONTROL, CF_ERR)
    call get_cultivar_coeff(              it_struc_pfac_tb,   'IT_STR_PF_TB', CONTROL, CF_ERR)
    call get_cultivar_coeff(              it_struc_pfac_tm,   'IT_STR_PF_TM', CONTROL, CF_ERR)
    call get_cultivar_coeff(              it_struc_pfac_te,   'IT_STR_PF_TE', CONTROL, CF_ERR)
    call get_cultivar_coeff(           it_struc_pfac_delta,    'IT_STR_PF_D', CONTROL, CF_ERR)
    call get_cultivar_coeff(    it_struc_pfac_temp_max_red,   'IT_STR_T_RED', CONTROL, CF_ERR)
    call get_cultivar_coeff(    it_struc_pfac_wate_max_red,   'IT_STR_W_RED', CONTROL, CF_ERR)
    call get_cultivar_coeff(                    max_per_it,     'MAX_PER_IT', CONTROL, CF_ERR)
    call get_cultivar_coeff(                    dswat_ddws,     'DSWAT_DDWS', CONTROL, CF_ERR)
    call get_cultivar_coeff(                    dswat_dsuc,     'DSWAT_DSUC', CONTROL, CF_ERR)
    call get_cultivar_coeff(                       hex_min,        'HEX_MIN', CONTROL, CF_ERR)
    call get_cultivar_coeff(                   suc_acc_ini,    'SUC_ACC_INI', CONTROL, CF_ERR)
    call get_cultivar_coeff(              suc_frac_rate_ts,   'DSUC_FRAC_TS', CONTROL, CF_ERR)
    call get_cultivar_coeff(                  tt_chumat_lt,   'TT_CHUMAT_LT', CONTROL, CF_ERR)
    
    !--- Species parameters
    call get_species_coeff(                             tb,             'TB', CONTROL, SPC_ERROR)
    call get_species_coeff(                         srlMax,         'SRLMAX', CONTROL, SPC_ERROR)
    call get_species_coeff(                         srlMin,         'SRLMIN', CONTROL, SPC_ERROR)
    call get_species_coeff(                      rootdrate,      'ROOTDRATE', CONTROL, SPC_ERROR)
    call get_species_coeff(                      max_rt_dw,      'MAX_RT_DW', CONTROL, SPC_ERROR)
    call get_species_coeff(               end_tt_rt_growth,  'END_TT_RT_GRO', CONTROL, SPC_ERROR)
    call get_species_coeff(                   rootleftfrac,   'ROOTLEFTFRAC', CONTROL, SPC_ERROR)
    call get_species_coeff(                           kdif,           'KDIF', CONTROL, SPC_ERROR)
    call get_species_coeff(                         kc_min,         'KC_MIN', CONTROL, SPC_ERROR)
    call get_species_coeff(                        eoratio,        'EORATIO', CONTROL, SPC_ERROR)
    call get_species_coeff(      cr_source_sink_ratio_ruse,   'SO2SI_USERES', CONTROL, SPC_ERROR)
    call get_species_coeff(                      maxlai_eo,      'MAXLAI_EO', CONTROL, SPC_ERROR)
    call get_species_coeff(                          gresp,          'GRESP', CONTROL, SPC_ERROR)
    call get_species_coeff(                       kmr_leaf,       'KMR_LEAF', CONTROL, SPC_ERROR)
    call get_species_coeff(                       kmr_stem,       'KMR_STEM', CONTROL, SPC_ERROR)
    call get_species_coeff(                       kmr_root,       'KMR_ROOT', CONTROL, SPC_ERROR)
    call get_species_coeff(                       kmr_stor,       'KMR_STOR', CONTROL, SPC_ERROR)
    call get_species_coeff(                       q10_leaf,       'Q10_LEAF', CONTROL, SPC_ERROR)
    call get_species_coeff(                       q10_stem,       'Q10_STEM', CONTROL, SPC_ERROR)
    call get_species_coeff(                       q10_root,       'Q10_ROOT', CONTROL, SPC_ERROR)
    call get_species_coeff(                       q10_stor,       'Q10_STOR', CONTROL, SPC_ERROR)
    call get_species_coeff(                        tref_mr,        'TREF_MR', CONTROL, SPC_ERROR)
    call get_species_coeff(                            tbm,            'TBM', CONTROL, SPC_ERROR)
    call get_species_coeff(                      threshews,      'THRESHEWS', CONTROL, SPC_ERROR)
    call get_species_coeff(              dshootext_BG_rate,     'DSHOOT_EXT', CONTROL, SPC_ERROR)
    call get_species_coeff(               mid_tt_rt_growth,  'MID_TT_RT_GRO', CONTROL, SPC_ERROR)
    call get_species_coeff(                   max_it_dw_BG,   'MAX_IT_DW_BG', CONTROL, SPC_ERROR)
    call get_species_coeff(                        suc_min,        'SUC_MIN', CONTROL, SPC_ERROR)
    call get_species_coeff(            tilleragefac_adjust,     'TILLAGEFAC', CONTROL, SPC_ERROR)
    call get_species_coeff(                      rootshape,      'ROOTSHAPE', CONTROL, SPC_ERROR)
    call get_species_coeff(                         rwumax,         'RWUMAX', CONTROL, SPC_ERROR)
    call get_species_coeff(             res_used_emerg_fac,   'FRES_USED_EM', CONTROL, SPC_ERROR)
    call get_species_coeff(             agefactor_fac_amax,    'AGEFAC_AMAX', CONTROL, SPC_ERROR)
    call get_species_coeff(              agefactor_fac_per,     'AGEFAC_PER', CONTROL, SPC_ERROR)
    call get_species_coeff(                   c_scattering,   'C_SCATTERING', CONTROL, SPC_ERROR)
    call get_species_coeff(                              k,              'K', CONTROL, SPC_ERROR)
    call get_species_coeff(                root_front_size,  'RT_FRONT_SIZE', CONTROL, SPC_ERROR)
    
    !--- Get integer variables
    maxgl               = aint(maxgl_r)
    maxdgl              = aint(maxdgl_r)
    n_lf_when_stk_emerg = aint(n_lf_when_stk_emerg_r)
    n_lf_it_form        = aint(n_lf_it_form_r)
    
    !--- Species-related response to CO2
    co2_pho_res_end =   270.d0
    co2_pho_res_ini =   0.d0
    
    !--- Maximum number of sequential ratoons
    max_ratoon      = 5         ! Make this an ecotype coefficient
    
    !--- Convert parameters for i/o purpose
    agefactor_fac_amax           = agefactor_fac_amax / 1.e5
    agefactor_fac_rue            = agefactor_fac_rue  / 1.e5
    agefactor_fac_per            = agefactor_fac_per  / 1.e5
        
    !--- Assume same response for tillering and partitioning factor (NOT IN USE)
    t_max_ws_fpf    = t_max_ws_exp
    t_mid_ws_fpf    = t_mid_ws_exp
    t_min_ws_fpf    = t_min_ws_exp
    t_max_ws_til    = t_max_ws_pho
    t_mid_ws_til    = t_mid_ws_pho
    t_min_ws_til    = t_min_ws_pho
    
    !--- Assume phyllochron=plastochron for simplicity
    !--- We do not have such high level of precision yet
    phyllochron = plastochron
    
    !--------------------------!
    !--- Simulation Options ---!
    !--------------------------!
    
    !--- Simulate Water Stress
    if(ISWITCH % ISWWAT .eq. 'Y') then
        fl_potential      = .false.
    else
        fl_potential      = .true.
    endif
    
    !---------------!
    !--- Methods ---!
    !---------------!
    
    !--- Tillering
    method_pop      = 2               ! Using light + temperature method
    
    !--- Photosynthesis Method
    metpg           = 2               ! Using 5-point layered canopy as default
    
    !--- Use Soil Temperature
    usetsoil            = .false.     ! Soil Temperature effect [switched-off until DSSAT soil temp is corrected]
    
    !--- Use mulch effect
    mulcheffect         = .false.     ! Mulch effect [switched-off until DSSAT soil temp is corrected]
    
    !----------------------------------!
    !--- Crop States Initialization ---!
    !----------------------------------!
        
    !--- Rowspacing
    rowsp   = 1.4       ! Default Value in case the below function doesnt find any value
    call find_inp_sam(rowsp, 'ROWSPC', Control)
    rowsp   =   rowsp * 100.        ! Convert to [cm]
    
    !--- Planting depth
    plantdepth  = 20.   ! Default Value in case the below function doesnt find any value
    call find_inp_sam(plantdepth, 'PLDP', Control)
    if(plantdepth .lt. z) plantdepth  = 20. ! in case its -99.0    
    
    !--- Leaf dry weight at end of life-spam of a leaf grown under optimun conditions [g]
    max_lf_dw       = mla / sla ! Use this while the model considers fixed SLA (PIT)
    
    !--- Initial dry mass [t ha-1] in planting date (using flat bed method for planting)
    !--- Assuming stalks with 1.5 kg and 2 meters
    init_stalkfw        = 1.5d0 !kg   
    init_stalkht        = 2.d0  !m
    nstalks_planting    = 2.d0  !#
    ini_nstk            = 5. * 1. / (rowsp / 100.) ! plants m-2 - Assuming 5 emerged stems per 1 linear meter (20 cm between each other)
    tilleragefac        = 1.
    
    !--- Time and season control
    dap     = 1        
        
    !--- Check if this is a sequential run
    if((Control%RNMODE .eq. 'N') .or. (Control%RNMODE .eq. 'F'))then
        
        !----------------------!
        !--- Sequential Run ---!
        !---  Seasonal Mode ---!
        !----------------------!        
        
        seqnow  = seqnow + 1
        nseason = seqnow
        
        !--- First Ratoon
        if((.not. ratoon) .and. (nseason .gt. 1))then            
            ratoon = .true.            
        endif
                
        !--- Sequential Ratoon
        if(ratoon)then
            
            !--- Ratoon count
            nratoon  = nratoon + 1
            
            if(nratoon .gt. max_ratoon)then                
                !--- Reached the maximum number of sequential ratoons
                nratoon = 0
                ratoon  = .false. ! This will be carried                
            endif            
        endif
        
    else
        
        !--- Not a sequential run       
        seqnow              = 1
        nseason             = 1
        if(ratoon) nratoon  = 1
    endif
    
    if(ratoon)then
        
        !-----------------!
        !--- Ratooning ---!
        !-----------------!
        
        pltype = 'Ratoon'
    
        !--- Initializing phytomer profile    
        phprof    = 0.d0 
    
        initcropdepth = min(init_plantdepth_ratoon, plantdepth)
        if(bottom(1) .ge. initcropdepth)then            
            initcropdepth = bottom(1) + 0.01 !Ensure that the ratoon depth shoot is below first soil layer           
        endif
        
        !--- Check root dry weight left from last season
        if(nseason .eq. 1) then
        
            !--- If ratoonng is the 1st season, assume the previous season had the potential growth
            dw_rt   = max_rt_dw * (1.e4 / 1.e6) ! [ton ha-1]
        
            !--- Substrates for initial growth - Use same as plant cane when ratoon is the first season        
            dw_it_BG    =   (init_stalkfw / init_stalkht * nstalks_planting) / (rowsp/100.) / 1.e3 * 1.e4 ! [ton ha-1]
            
            !--- Considering 70% of water, 15% structural biomass. Substrates reserves are sucrose (13%) and hexoses (2%)
            str_it_BG   =   dw_it_BG *  0.15d0  ! 15% Fiber
            sug_it_BG   =   dw_it_BG *  0.15d0  ! 15% Sugars
            suc_it_BG   =   dw_it_BG *  0.13d0  ! 13% Sucrose
            hex_it_BG   =   dw_it_BG *  0.02d0  !  2% Hexoses        
            dw_it_BG    =   dw_it_BG *  0.3d0   ! 70% Water  
            dw_it       =   dw_it_BG
        
            !--- Fraction of roots left alive after harvesting
            ini_dw_rt   = dw_rt     *   rootleftfrac        
            dw_rt       = ini_dw_rt
            dw_total    = dw_rt     +   str_it_BG   +   sug_it_BG
        
            !--- Roots reached in maximum depth
            rdm             = min(rdm, bottom(nlay))        
            rd              = rdm
            effective_rd    = initcropdepth
            rpup            = 0.d0
        
            !--- Equalize to soil layers depth
            rdprof = 0.d0
            do sl = 1, nlay
                if(rd .ge. upper(sl)) then
                    rdprof = rdprof + slthickness(sl)
                endif              
            enddo
        
            !--- Geotropism function
            do sl = 1, nlay
                geot(sl) = max(0.d0,(1.d0-dep(sl)/rdprof)) ** rootshape
            enddo
        
            !--- Convert dryweight to root length density
            drld = 0.d0
            rld  = 0.d0
            srl  = (srlmin + srlmax) / 2.d0
            
            do sl = 1, nlay
                rgf(sl,1)       = geot(sl)/sum(geot)
                dw_rt_prof(sl)  = rgf(sl,1) * dw_rt * (1.e6/1.e8)   ! [g cm-2]
                lroot(sl)       = dw_rt_prof(sl)    * srl * 100.d0  ! [cm cm-2]
                drld(sl)        = lroot(sl) / slthickness(sl)       ! [cm cm-3]
                rld(sl)         = rld(sl) + drld(sl)
            enddo
        
            cropdstage      =  'Sprout'
            cropstatus      = ' Alive'
            cstat           = 1.d0
            flcropalive     = .true.    
            
            !--- below ground phytomers
            nphy_BGround = 0
            
        else
            
            !--- Sequential Ratooning
            !--- Last season below ground biomass will not be reseted in order to be used as initial conditions
            
            !--- Roots remained alive            
            ini_dw_rt   = dw_rt     *   rootleftfrac        
            dw_rt       = ini_dw_rt
            rld         = rld       *   rootleftfrac
            dw_total    = dw_rt
            
            !--- Maximum Root depth [cm]
            !--- Note that the last season root depth is not modified
            rdm             = min(rdm, bottom(nlay))       
            effective_rd    = initcropdepth
            rpup            = 0.d0
            
            !--- Crop Stage
            if(flcropalive) cropdstage  = 'Sprout'
        
        endif
            
    else
        
        !------------------!
        !--- Plant Cane ---!
        !------------------!
        
        pltype = 'PlCane'
        
        !Initial Partitionioning factors
        rgpf = 1.00
        sgpf = 0.d0
        lgpf = 0.d0
        
        !--- Initializing phytomer profile    
        phprof    = 0.d0  
        
        !--- Initial Crop depth as same as the planting depth [cm]
        initcropdepth = plantdepth         
            
        !--- Substrates reserve for before emergence is considered as sugars content remaining in the chopped stalks
        dw_it_BG    =   (init_stalkfw / init_stalkht * nstalks_planting) / (rowsp/100.) / 1.e3 * 1.e4 ! [ton ha-1]
        
        !--- Considering 70% of water, 15% structural biomass. Substrates reserves are sucrose (13%) and hexoses (2%)
        str_it_BG   =   dw_it_BG *  0.15d0  ! 15% Fiber
        sug_it_BG   =   dw_it_BG *  0.15d0  ! 15% Sugars
        suc_it_BG   =   dw_it_BG *  0.13d0  ! 13% Sucrose
        hex_it_BG   =   dw_it_BG *  0.02d0  !  2% Hexoses        
        dw_it_BG    =   dw_it_BG *  0.3d0   ! 70% Water  
        dw_it       =   dw_it_BG
        
        !--- Fraction of roots left alive after harvesting
        ini_dw_rt   = 0.d0  ! No Roots in plant cane initial conditions
        rld         = 0.d0
        dw_rt       = ini_dw_rt
        dw_total    = str_it_BG   +   sug_it_BG        
        
        !--- Crop Depth [cm]
        rdm             = min(rdm, bottom(nlay))    
        effective_rd    = initcropdepth
        rd              = initcropdepth
        rpup            = 0.d0
        
        !--- Crop Stage
        cropdstage      =  'Sprout'
        cropstatus      = ' Alive'
        cstat           = 1.d0
        flcropalive     = .true.
        
        !--- below ground phytomers
        nphy_BGround = 0
        
    endif
            
    !--- Biomass and Aerial Conditions
    dw_aerial   			=   0.d0
    dw_lf       			=   0.d0
    dw_lf_BG    			=   0.d0
    dw_lf_AG    			=   0.d0
    dw_it_AG    			=   0.d0
    dw_it       			=   dw_it_AG + dw_it_BG
    str_it_AG   			=   0.d0
    sug_it_AG   			=   0.d0
    wat_it_AG   			=   0.d0
    fw_it_AG    			=   0.d0
    suc_it_AG   			=   0.d0
    hex_it_AG   			=   0.d0
    nstk					=	0.d0
    lai         			=   0.d0
    laimod                  =   0.d0
    maxlai                  =   0.d0
    lai_ass     			=   0.d0
    stk_h					=	0.d0
    diac        			=   0.d0
    diacsoil    			=   0.d0
    diacem      			=   0.d0
    diacsoilem  			=   0.d0
    nstk_now    			=   ini_nstk
    n_lf_tiller 			=   0.d0
    sug_cont    			=   0.d0
    dw_it_dead              =   0.d0
    dw_lf_dead              =   0.d0
    dw_it_dead_AG           =   0.d0
    dw_it_dead_BG           =   0.d0
    
    !--- Counters
    n_lf_dead               = 0			
    n_lf_dead_AG            = 0		
    n_lf_dead_BG            = 0		
    n_lf_alive_juveni	    = 1
    n_lf_alive_dewlap       = 0
    n_lf_AG_dewlap          = 0
    n_ph_AG                 = 0				
    n_lf_alive_AG		    = 0
    n_lf_alive_juveni_AG    = 0
    n_it_AG                 = 0				
    n_ph_BG                 = 1				
    n_lf_BG                 = 1				
    n_lf_alive_BG           = 1		
    n_lf_alive_juveni_BG    = 1
    n_ph		            = 1    
    n_lf_alive              = 1
    n_it                    = 1
    n_it_BG                 = 1
    n_lf                    = 1
    n_lf_AG                 = 0
    
    !--- phytomer stimuli
    phy_stimuli = 0.d0
    
    if(.not. (aint(nstk_now) .eq. nstk_now))then
        !--- Increase one tiller to account for decimals
        atln_now    = aint(nstk_now) + 1
    else
        atln_now    = aint(nstk_now)
    endif
    
    !--- Shared Sugars among below ground internodes
    shared_it_sug_BG =  (sug_it_BG * (1.e6/1.e4) / (ini_nstk * tilleragefac)) / n_it_BG
    shared_it_str_BG =  (str_it_BG * (1.e6/1.e4) / (ini_nstk * tilleragefac)) / n_it_BG
    
    !--- Update profile total sugars and biomass [g]    
    phprof(1:n_ph,50) = shared_it_sug_BG + shared_it_str_BG
    phprof(1:n_ph,51) = shared_it_str_BG
    phprof(1:n_ph,52) = shared_it_sug_BG
    phprof(1:n_ph,53) = shared_it_sug_BG * frac_hex_BG
    phprof(1:n_ph,54) = shared_it_sug_BG * frac_suc_BG
    phprof(1:n_ph,14) = max_it_dw_BG
    phprof(1:n_ph,11) = kmr_stor
    phprof(1:n_ph,13) = q10_stor
    
    !--- initialize tiller age profile
    tillerageprof = 0.d0
    
    !--- Flags
    fl_use_reserves     = .true.
    flemerged           = .false.
    fl_stalk_emerged    = .false.    
    fl_shed_leaf        = .false.
    fl_it_visible       = .false.
    fl_hasleaf          = .false.	
    fl_appear_leaf      = .false.
    fl_tiller_increase  = .true.
    fl_tiller_peaked    = .false.
    fl_tiller_decrease  = .false.
    fl_tiller_stop      = .false.
    fl_it_AG            = .false.
    fl_lf_AG            = .false.        
    fl_lf_alive(1:n_ph) = .true.
    
    poppeak_lt          = 0.d0
    chudec_lt           = 0.d0
    dnstk_dead_rate     = 0.d0
    chumat_lt           = 0.d0    
    
    !--- Growth stage
    gstd = 0.d0
    
    !--- Initial root dry weight in g m-2
    ini_dw_rt   =   ini_dw_rt   *   (1.e6 / 1.e4)
    
    !--- Initial Crop Depth [cm]
    shootdepth          = initcropdepth
    diac_at_emergence   = 0.d0
    
    !--- Resources used for emergence (reset plant memory)
    res_used_emerg      = 0.d0        
    
    !--- Light accumulated for photosynthesis (MJ m-2)
    acc_par             = 0.d0
    drue_calc           = 0.d0
    rue_calc            = 0.d0
    
    !--- Prepare for summary outputs
    xlai                = 0.d0    
    swfacp				= 1.d0
    swface              = 1.d0
    
    !---------------------------------------------------!
    !--- Sugarcane phenological stages names/numbers ---!
    !---------------------------------------------------!
    !--- Following CANEGRO (SC_CNG_mods.for at 04/01/2020):
     ! DATA SCSTGNAM /
     !&  '          ',   !1
     !&  'Peak pop. ',   !2
     !&  'Phylswitch',   !3
     !&  'Stalk emrg',   !4
     !&  '          ',   !5
     !&  'Flowr init',   !6
     !&  'Flowr emrg',   !7
     !&  'Plant/ratn',   !8
     !&  'Germinate ',   !9
     !&  'Emergence ',   !10
     !&  'Peak popn ',   !11
     !&  'Maturity  ',   !12
     !&  '          ',   !13
     !&  'Start Sim ',   !14
     !&  'End Sim   ',   !15
     !&  'Harvest   ',   !16
     !&  '          ',   !17
     !&  '          ',   !18
     !&  '          ',   !19
     !&  'Harvest   '/   !20
    
    !--- Season Init
    STGDOY(14) = Control%YRSIM

    !--- Plant/ratoon date (same as season init)
    STGDOY(8)  = Control%YRDOY
    
    call SC_OPHARV_SAM(CONTROL, ISWITCH,        &
        CaneCrop, flemerged, maxlai,            &
        swfacp, swface, STGDOY, XLAI, YRPLT)
    
    !--------------------!
    !--- Output Files ---!
    !--------------------!
    
    !--- Call the output routine, to initialize output
    !--- *** Borrowing SC_OPGROW.for from CANEGRO ***
    call sc_opgrow_SAM( CONTROL,        &
                        CaneCrop,       &                        
                        YRPLT)
    
    !------------------------!
    !--- Detailed Outputs ---!
    !------------------------!    
    if(writedcrop) call SC_OPGROW_SAM_DETAILED (CONTROL, CaneCrop, YRPLT)    
          
    !--- Linking with DSSAT globals
    CANHT       =   stk_h   ! Canopy height (m)    
    KCAN        =   k       ! Canopy light extinction coefficient for daily PAR, for equidistant plant spacing, modified when in-row and between row spacing are not equal 
    KTRANS      =   k       ! Light extinction coefficient used for computation of plant transpiration 
    NSTRES      =   1.      ! No Nitrogen Stress at this version
    RLV         =   rld     ! Root length density for soil layer L (cm[root] / cm3[soil])
    RWUMX       =   rwumax  ! Maximum water uptake per unit root length, constrained by soil water (cm3[water] / cm [root])        
    XHLAI       =   lai     ! Healthy leaf area index (m2[leaf] / m2[ground])
    XLAI        =   laimod  ! Leaf area (one side) per unit of ground area (m2[leaf] / m2[ground]) [NOTE: we are following canegro and considering as]

    !--- Total LAI must exceed or be equal to healthy LAI
    !--- Following MJ:
    XLAI = MAX(XLAI, XHLAI)
    
    !HARVRES     ! Composite variable containing harvest residue amounts for total dry matter, lignin, and N amounts.  Structure of variable is defined in ModuleDefs.for. 
    !MDATE       ! Harvest maturity date (YYYYDDD)
    !PORMIN      ! Read in SCSAM047.SPE            
    !SENESCE     ! Composite variable containing data about daily senesced plant matter. Structure of variable is defined in ModuleDefs.for
    !STGDOY      ! Day when plant stage I occurred (YYYYDDD)
    !UNH4        ! Not in use 
    !UNO3        ! Not in use
    !EORATIO     ! Read in SCSAM047.SPE            
    
    return
    
30  continue    
    
    !-----------------------!
    !--- Potential rates ---!
    !-----------------------!
    
    !--- Not in use anymore. 
    !--- To run potential conditions, turn-off the Limiting Factors
    
    return
    
40  continue
	
    !-------------------------------------!
    !--- Time-Step Rate Initialization ---!
    !-------------------------------------!
    
    !--- Crop Step-Rates
    di							= 0.d0
    disoil						= 0.d0
    drdepth						= 0.d0
    dshootext_BG				= 0.d0
    rgf     					= 0.d0
    dw_rt_prof                  = 0.d0
    drld						= 0.d0
    drld_dead					= 0.d0
    dphy_stimuli				= 0.d0
    dla_gain_ref_till           = 0.d0
    dlai_gain					= 0.d0
    dlai_dead					= 0.d0
    dlai_shed					= 0.d0
    dlai_gain_appear            = 0.d0
    ddw_lf_appear               = 0.d0
    per							= 0.d0
    per_hour					= 0.d0
    dnstk						= 0.d0
    ddw_rt         				= 0.d0
    ddw_lf         				= 0.d0
    ddw_lf_BG                   = 0.d0
    ddw_lf_AG                   = 0.d0
    ddw_it         				= 0.d0
    ddw_it_AG      				= 0.d0
    dstr_it_AG     				= 0.d0
    dsug_it_AG     				= 0.d0
    ddw_it_BG      				= 0.d0
    dstr_it_BG     				= 0.d0
    dsug_it_BG     				= 0.d0
    dwat_it_AG     				= 0.d0
    ddw_lf_shed    				= 0.d0
    dsubsres       				= 0.d0
    ddw_rt_dead    				= 0.d0
    ddw_lf_dead    				= 0.d0
    ddw_it_dead    				= 0.d0
    ddw_it_AG_dead 				= 0.d0
    dstr_it_AG_dead				= 0.d0
    dsug_it_AG_dead				= 0.d0
    ddw_it_BG_dead 				= 0.d0
    dstr_it_BG_dead				= 0.d0
    dsug_it_BG_dead				= 0.d0
    dwat_it_AG_dead				= 0.d0

    !--- Respiration Rates and Sink Strenght
    tot_gresp_lf 				= 0.d0
    tot_mresp_lf 				= 0.d0
    tot_dw_ss_lf 				= 0.d0
    tot_gresp_it_AG 			= 0.d0               
    tot_mresp_it_AG 			= 0.d0        
    tot_dw_ss_it_AG 			= 0.d0
    tot_gresp_it_BG 			= 0.d0   
    tot_mresp_it_BG 			= 0.d0
    tot_dw_ss_it_BG 			= 0.d0
    tot_gresp_rt				= 0.d0
    tot_mresp_rt				= 0.d0
    dw_ss_rt					= 0.d0
    dtlfss						= 0.d0
    dtitss						= 0.d0
    dtitss_AG					= 0.d0
    dtitss_BG					= 0.d0
    dtrtss						= 0.d0
    dtcrss						= 0.d0
    tot_gresp_it				= 0.d0
    tot_mresp_it				= 0.d0
    tot_dw_ss_it				= 0.d0		
    tot_gresp_crop 				= 0.d0
    tot_mresp_crop 				= 0.d0
    tot_dw_ss_crop 				= 0.d0
    dr_rtss 					= 0.d0
    dr_lfss 					= 0.d0
    dr_itss 					= 0.d0		
    frac_AG   					= 0.d0
    frac_BG   					= 0.d0		
    rel_ss_lf_phy				= 0.d0
    rel_ss_it_phy				= 0.d0
    dw_ss_lf					= 0.d0
	gresp_lf					= 0.d0
	mresp_lf					= 0.d0
	dw_ss_it					= 0.d0
	gresp_it					= 0.d0
	mresp_it					= 0.d0

    !--- Substrates Assimilated
    dtg							= 0.d0
    frac_li                     = 0.d0
    li                          = 0.d0
    dacc_par                    = 0.d0
    drue_calc                   = 0.d0
    rue_calc                    = 0.d0
    Acanopy				        = 0.d0
    Qleaf				        = 0.d0
    incpar				        = 0.d0
    photo_layer_act				= 0.d0
    avail_subs_crop				= 0.d0
    dtg_avail_rt    			= 0.d0 
    dtg_avail_lf    			= 0.d0 
    dtg_avail_it    			= 0.d0 
    dtg_avail_it_BG 			= 0.d0 
    dtg_avail_it_AG 			= 0.d0
    subsres_avail_rt    		= 0.d0
    subsres_avail_lf    		= 0.d0
    subsres_avail_it    		= 0.d0
    subsres_avail_it_BG 		= 0.d0
    subsres_avail_it_AG 		= 0.d0
    sup_ratio_lf				= 0.d0
    supply_rate_lf				= 0.d0
    supply_used_lf				= 0.d0
    supply_used_mresp_lf		= 0.d0
    supply_used_gresp_lf		= 0.d0
    supply_used_dw_lf			= 0.d0
    reserves_used_mresp_lf		= 0.d0
    maintenance_factor_lf		= 1.d0
    reduc_growth_factor_lf		= 1.d0
    sup_ratio_it				= 0.d0
    supply_rate_it				= 0.d0
    supply_used_it				= 0.d0
    supply_used_mresp_it		= 0.d0
    supply_used_gresp_it		= 0.d0
    supply_used_dw_it			= 0.d0
    reserves_used_mresp_it		= 0.d0
    maintenance_factor_it		= 1.d0
    reduc_growth_factor_it		= 1.d0
    sup_ratio_it_BG				= 0.d0
    supply_rate_it_BG			= 0.d0
    supply_used_it_BG			= 0.d0
    supply_used_mresp_it_BG		= 0.d0
    supply_used_gresp_it_BG		= 0.d0
    supply_used_dw_it_BG		= 0.d0
    reserves_used_mresp_it_BG	= 0.d0
    maintenance_factor_it_BG	= 1.d0
    reduc_growth_factor_it_BG	= 1.d0
    sup_ratio_it_AG				= 0.d0
    supply_rate_it_AG			= 0.d0
    supply_used_it_AG			= 0.d0
    supply_used_mresp_it_AG		= 0.d0
    supply_used_gresp_it_AG		= 0.d0
    supply_used_dw_it_AG		= 0.d0
    reserves_used_mresp_it_AG	= 0.d0
    maintenance_factor_it_AG	= 1.d0
    reduc_growth_factor_it_AG	= 1.d0
    sup_ratio_rt				= 0.d0
    supply_rate_rt				= 0.d0
    supply_used_rt				= 0.d0
    supply_used_mresp_rt		= 0.d0
    supply_used_gresp_rt		= 0.d0
    supply_used_dw_rt			= 0.d0
    reserves_used_mresp_rt		= 0.d0
    maintenance_factor_rt		= 1.d0
    reduc_growth_factor_rt		= 1.d0
    supply_used_crop		    = 0.d0
    supply_used_mresp_crop	    = 0.d0
    supply_used_gresp_crop	    = 0.d0
    supply_used_dw_crop		    = 0.d0
    reserves_used_mresp_crop    = 0.d0
    maintenance_factor_crop	    = 1.d0
    reduc_growth_factor_crop    = 1.d0
    dtg_avail_lf_ref_till       = 0.d0
    dtg_avail_it_ref_till       = 0.d0
    dtg_avail_it_BG_ref_till    = 0.d0
    dtg_avail_it_AG_ref_till    = 0.d0
    subsres_avail_lf_ref_till   = 0.d0
    subsres_avail_it_ref_till   = 0.d0
    subsres_avail_it_BG_ref_till= 0.d0
    subsres_avail_it_AG_ref_till= 0.d0
    dtlfss_ref_till             = 0.d0
    dtitss_ref_till             = 0.d0
    dtitss_BG_ref_till          = 0.d0
    dtitss_AG_ref_till          = 0.d0
    dtg_avail_lf_phy          	= 0.d0
    subsres_avail_lf_phy      	= 0.d0
    dtlfss_phy                	= 0.d0
    mresp_lf_phy              	= 0.d0
    gresp_lf_phy              	= 0.d0
    dw_ss_lf_phy              	= 0.d0
    sup_ratio_lf_phy          	= 0.d0
    supply_rate_lf_phy        	= 0.d0
    supply_used_lf_phy        	= 0.d0
    supply_used_mresp_lf_phy  	= 0.d0
    supply_used_gresp_lf_phy  	= 0.d0
    supply_used_dw_lf_phy     	= 0.d0
    reserves_used_mresp_lf_phy	= 0.d0
    maintenance_factor_lf_phy 	= 1.d0
    reduc_growth_factor_lf_phy	= 1.d0
    dtg_avail_it_phy          	= 0.d0
    subsres_avail_it_phy      	= 0.d0
    dtitss_phy                	= 0.d0
    mresp_it_phy              	= 0.d0
    gresp_it_phy              	= 0.d0
    dw_ss_it_phy              	= 0.d0
    sup_ratio_it_phy          	= 0.d0
    supply_rate_it_phy        	= 0.d0
    supply_used_it_phy        	= 0.d0
    supply_used_mresp_it_phy  	= 0.d0
    supply_used_gresp_it_phy  	= 0.d0
    supply_used_dw_it_phy     	= 0.d0
    reserves_used_mresp_lf_phy	= 0.d0
    maintenance_factor_it_phy 	= 1.d0
    reduc_growth_factor_it_phy	= 1.d0
    it_struc_pfac_rate			= 0.d0
    dstr_it_phy					= 0.d0
    dsug_it_phy					= 0.d0
    exc_dtg_lf      			= 0.d0
    exc_dtg_it      			= 0.d0
    exc_dtg_rt      			= 0.d0   
    dtot_str_dw_ref_till		= 0.d0
    suc_it_AG_ref_till	        = 0.d0
    hex_it_AG_ref_till	        = 0.d0                  
    suc_it_BG_ref_till	        = 0.d0
    hex_it_BG_ref_till	        = 0.d0 
    shared_it_sug_BG            = 0.d0

    !--- Crop Stress factors
    swfacp						= 1.d0
    swface						= 1.d0
    swfact						= 1.d0
    swfacf						= 1.d0    
    agefactor_per				= 1.d0
    agefactor_rue				= 1.d0
    pho_fac_co2					= 1.d0
    tempfac_h_per				= 1.d0
    tempfac_pho					= 1.d0
    tempfac_per					= 1.d0
    amaxfbfac					= 1.d0
    agefactor_amax              = 1.d0 

    !--- Phytomer Rates
    phprof(1: 200, 2) 		= 0.d0  ! Leaf Sink strenght
    phprof(1: 200, 3) 		= 0.d0  ! Allocated Leaf biomass
    phprof(1: 200, 4) 		= 0.d0  ! Leaf area rate
    phprof(1: 200, 7) 		= 0.d0  ! Internode Sink Strength dw rate g d-1
    phprof(1: 200,21) 		= 0.d0 	! Internode Growth Respiration
    phprof(1: 200,22) 		= 1.d0 	! Maintenance Respiration Factor (0-1) 1 =  is maintenance is ok
    phprof(1: 200,23) 		= 0.d0 	! dLength (cm)
    phprof(1: 200,25) 		= 0.d0 	! mresp leaf
    phprof(1: 200,26) 		= 0.d0 	! gresp leaf
    phprof(1: 200,27) 		= 0.d0 	! dw ss leaf
    phprof(1: 200,28) 		= 1.d0 	! sup_ratio_lf_phy
    phprof(1: 200,29) 		= 0.d0 	! supply_rate_lf
    phprof(1: 200,30) 		= 0.d0 	! sup_ratio_lf_phy
    phprof(1: 200,31) 		= 0.d0 	! supply_used_mresp_lf
    phprof(1: 200,32) 		= 0.d0 	! supply_used_gresp_lf
    phprof(1: 200,33) 		= 0.d0 	! supply_used_dw_lf
    phprof(1: 200,34) 		= 1.d0 	! maintenance_factor_lf
    phprof(1: 200,35) 		= 1.d0 	! reduc_growth_factor_lf
    phprof(1: 200,36) 		= 0.d0 	! mresp internode
    phprof(1: 200,37) 		= 0.d0 	! gresp internode
    phprof(1: 200,38) 		= 0.d0 	! dw ss internode
    phprof(1: 200,39) 		= 0.d0 	! supply_rate_it
    phprof(1: 200,40) 		= 1.d0 	! sup_ratio_it_phy
    phprof(1: 200,41) 		= 0.d0 	! supply_used_it
    phprof(1: 200,42) 		= 0.d0 	! supply_used_mresp_it
    phprof(1: 200,43) 		= 0.d0 	! supply_used_gresp_it
    phprof(1: 200,44) 		= 0.d0 	! supply_used_dw_it
    phprof(1: 200,45) 		= 1.d0 	! maintenance_factor_it
    phprof(1: 200,46) 		= 1.d0 	! reduc_growth_factor_it
    phprof(1: 200,47) 		= 0.d0 	! Internode dry weigth rate [g dt-1]
    phprof(1: 200,48) 		= 0.d0 	! Internode structural dry weigth rate [g dt-1]
    phprof(1: 200,49) 		= 0.d0 	! Internode total sugars rate [g dt-1]    
    phprof(1: 200,55)       = 0.d0  ! Leaf Age rate [dCdays]
    phprof(1: 200,56)       = 0.d0  ! Phytomer Age rate [dCdays]
    phprof(1: 200,57)       = 0.d0  ! Internode Age rate [dCdays]
    
    !------------------------------!
    !--- Check if crop is alive ---!
    !------------------------------!
    if(.not. flcropalive) return
    !------------------------------!
    
    !------------------------!
    !--- DEFINING FACTORS ---!
    !------------------------!
    
    !-----------------------!
    !--- Solar Radiation ---!
    !-----------------------!
    
    !--- Fraction of Solar Radiation Crop Can Actively Use
    !--- Photosynthetically Active Radiation (PAR) - Assume as 50% of total incoming radiation flux
    par_rad = srad * 0.5d0
    
    !--- Canopy light interception fraction (Beer Law)
    li      = 1.d0 - exp(-k * lai)
    
    !--- Canopy intercepted PAR [MJ]
    can_ipar = par_rad * li
    
    !-------------------!
    !--- Temperature ---!
    !-------------------!
    
    !--- Temperature stress on photosynthesis
    tempfac_pho      = temperature_factor(tmn, tb0pho, tb1pho, tb2pho, tbfpho) ! Photosynthesis
    tempfac_per      = min(1.,max(0.,tmn - tbper) / (tbMax_per - tbper))
    
    !--- Hourly Plant Extension Temperature Factor
    do hour = 1, 24
        tempfac_h_per(hour)   = min(1.d0, max(0.d0, thour(hour) - tbper) / (tbMax_per - tbper))
    enddo
    
    !-------------------------------------!
    !--- Atmospheric CO2 Concentration ---!
    !-------------------------------------!
    
    !--- Following Mathew Jones and Abraham Singels (https://doi.org/10.1016/j.eja.2017.12.009)
    !--- No effect on C4 Photosynthesis after 270 ppm - Higher Water Use Efficiency is believed to be the reason of increased biomass under increased CO2 (included in ptrans())
    if(co2 .gt. co2_pho_res_end)then
        !--- Optimun CO2 conditions
        pho_fac_co2 =   1.d0                
    else if(co2 .lt. co2_pho_res_ini)then
        !--- Extreme Low CO2 conditions (No photosynthesis)
        pho_fac_co2 =   0.d0                
    else
        !--- Transient concentration
        pho_fac_co2 = (co2_pho_res_end - co2) / (co2_pho_res_end - co2_pho_res_ini)            
    endif
    
    !-----------------------!
    !--- Genotype traits ---!
    !-----------------------!
    
    !--- Aging factor to account for reduced growth due to crop aging
    !--- Necessary to include effects of the Reduced Growth Phenomena (RGP) still not well understood,
    !--- or reduced turgor pressure on top parts in planting extension.
    agefactor_rue = exp(agefactor_fac_rue * diacem) 
    agefactor_rue = min(1.d0, agefactor_rue)
    
    !--- Max assimimilation reduction
    agefactor_amax = exp(agefactor_fac_amax * diacem) 
    agefactor_amax = min(1.d0, agefactor_amax)    
    
    !--- Age reduction factor for dper, based on N. G. Inman-Bamber et al. 2008 Australian Journal of Agricultural Research, Fig.3
    agefactor_per = exp(agefactor_fac_per * diacem)
    agefactor_per = min(1.d0, agefactor_per)
    
    !------------------------!
    !--- LIMITING FACTORS ---!
    !------------------------!
    
    if(.not. fl_potential)then
        
        !--- Water Stress ---!
        call sc_waterstress(2,            & 
                            ndws,         &
                            ndews,        &
                            eop,          &
                            trwup,        &
                            rwuep1,       &
                            rwuep2 ,      &
                            t_max_ws_pho, &
                            t_mid_ws_pho, &
                            t_min_ws_pho, &   
                            t_max_ws_exp, &
                            t_mid_ws_exp, &
                            t_min_ws_exp, &
                            t_max_ws_til, &
                            t_mid_ws_til, &
                            t_min_ws_til, &
                            t_max_ws_fpf, &
                            t_mid_ws_fpf, &
                            t_min_ws_fpf, &
                            threshews,    &
                            swfacp,       &
                            swface,       &
                            swfact,       &
                            swfacf)
        
        if(swfacp .lt. 1.d0 .and. flemerged)then
            fl_use_reserves = .false. ! do not use reserves when crop is stressed-out
        endif
        
    
        !--- Soil Nutrients ---!
    
        !--- (PIT) ---!
        
    endif
    
    !------------------------!
    !--- REDUCING FACTORS ---!
    !------------------------!    
    
    !--- (PIT) ---!
    
    
    !------------------------!
    !--- CROP DEVELOPMENT ---!
    !------------------------!
    
    !----------------!
    !--- Age Rate ---!
    !----------------!
    if(usetsoil)then
        
        !----------------------------!
        !--- Use Soil Temperature ---!
        !----------------------------!
        
        if(mulcheffect)then
            !--- Mulch effect when mulch is present (skip the first layer temperature (mulch))  
            !--- average soil temperature in plant depth soil layers (weighted mean)
            soiltemperature = 0.d0 
            do sl = 1, nlay  
                if(initcropdepth .gt. upper(sl)) then
                    
                    if(initcropdepth .gt. bottom(sl)) then
                        soiltemperature = soiltemperature + tsoil(sl+1) * slthickness(sl) / initcropdepth                
                    else           
                        soiltemperature = soiltemperature + tsoil(sl+1) * (initcropdepth - bottom(sl-1))  / initcropdepth                     
                    endif               
                endif       
            enddo        
        else
            !--- use soil temperature but without mulch effect (bare soil)
            !--- average soil temperature in plant depth soil layers (weighted mean)
            soiltemperature = 0.d0        
            do sl = 1, nlay        
                if(initcropdepth .gt. upper(sl)) then                
                    if(initcropdepth .gt. bottom(sl)) then
                        soiltemperature = soiltemperature + tsoil(sl) * slthickness(sl) / initcropdepth                
                    else           
                        soiltemperature = soiltemperature + tsoil(sl) * (initcropdepth - bottom(sl-1))  / initcropdepth                     
                    endif               
                endif       
            enddo           
        endif
        
        !--- Degree-days using soil and air temperatures        
        disoil  = min(max(0.d0, soiltemperature - tb)   , tbm - tb)
        diair   = min(max(0.d0, tmn - tb)               , tbm - tb)
    
        !--- Use soil temperature to compute crop age until stalks arises (Appical meristems is the sensor)
        if(.not. fl_stalk_emerged) then 
            
            !--- Computing crop age (degree-days) based on soil temperature 
	        di      = disoil
            
            if(flemerged)then                
                !--- Leaves emerges prior to stalks
                dileaf = diair                
            else                
                !--- Before emergence leaves (shoots) are below ground
                dileaf  = disoil
            endif
        else
            
            !--- Computing crop age (degree-days) based on air temperature
            di      = diair
            dileaf  = diair          
        endif
    
    else
        
        !--------------------------------!
        !--- Not Use Soil Temperature ---!
        !--------------------------------!
        
        !--- Considering soil temperature equal as air temperature
        soiltemperature = tmn
        
        !--- Computing crop age (degree-days) based on air temperature        
        di      = min(max(0.d0, tmn - tb), tbm - tb)
        diair   = di
        disoil  = di
        dileaf  = di
        
    endif  
    
    !------------------------!
    !--- Phytomer Stimuli ---!
    !------------------------!
    
    !--- Stimuli rate [phy/dt]
    dphy_stimuli    = (1.d0 / plastochron) * di
    
    
    if(flemerged)then
        
        !-----------------!
        !--- Tillering ---!
        !-----------------!
        
        select case(method_pop)
                    
            case(1)
                
                !------------------------------------!
                !--- Tillering Rate ~ Thermal Age ---!
                !------------------------------------!
            
                !--- Daily initial tillers numbers rate
                if(diacsoil .lt. (chupeak + diac_at_emergence)) then
                    !--- Initial tiller grow
				        dnstk = ((poppeak-ini_nstk)/(chupeak)) * disoil			   
                
                elseif(diacsoil .ge. (chupeak + diac_at_emergence) .and. diacsoil .lt. (chudec + diac_at_emergence)) then
                    !--- tillering peak
				        dnstk = 0.   
                
                elseif(diacsoil .ge. (chudec + diac_at_emergence) .and. diacsoil .lt. (chumat + diac_at_emergence)) then
                    !--- reduction phase to mature (abortion of tillers)
				        dnstk = (-(poppeak - popmat) / ((chumat)-(chudec))) * disoil 	   
                
                elseif(diacsoil .ge. (chumat+diac_at_emergence)) then
                    !--- late stable tiller population
				        dnstk = 0.		            						               
                endif
        
            case(2)       
                
                !------------------------------------------------------!
                !--- Tillering Rate ~ Thermal Age + Solar Radiation ---!
                !------------------------------------------------------!
                
                
                !--- Dead leaf position below the living leaves profile
                lf_dpos     = n_lf_alive_AG + 1
                
                !--- Dead LAI, where the fdeadlf is the fraction of blades of attached dead leaves 
                dead_lai    = sum(phprof(lf_dpos : (lf_dpos + aint(nsenesleaf_effect)), 5)) * nstk_now * tilleragefac * 1.e-4 * fdeadlf
                
                !--- Modified LAI (dry+green leaves)
                laimod = lai + dead_lai
                
                !--- Transmitted Light Through Canopy
                lt     = exp(-k * laimod)
                
                !--- Check Tillering States
                if(lt .lt. ltthreshold .and. .not. fl_tiller_peaked)then
                    !--- Fisrt time reaching tillering peak
                    fl_tiller_peaked    = .true.
                    fl_tiller_stop      = .true.
                                                            
                    !--- Store the peak of population
                    poppeak_lt          = nstk_now
                    chudec_lt           = diacsoil
                    chumat_lt           = chudec_lt + tt_chumat_lt
                    STGDOY(11)          = Control%YRDOY ! STGDOY(11) is peak population according to CANEGRO (file SC_Poplt3.for)
                    
                    !--- Tillering dead rate [till cdays-1]
                    dnstk_dead_rate     = (popmat - poppeak_lt) / (chumat_lt - chudec_lt)
                    
                else if(lt .lt. ltthreshold .and. fl_tiller_peaked)then
                    !--- Decrease tillering
                    fl_tiller_decrease  = .true.
                    fl_tiller_stop      = .false.
                    
                    !--- Update Tillering dead rate [till cdays-1]
                    if(diacsoil .ge. chumat_lt)then
                        dnstk_dead_rate = 0.d0
                    else                        
                        dnstk_dead_rate     = (popmat - nstk_now) / (chumat_lt - diacsoil)
                    endif                    
                    
                else if(lt .gt. ltthreshold .and. fl_tiller_peaked)then
                    !--- Don't let increase tillering after peak has reached (Unless lodging happens and resets fl_tiller_peaked)
                    fl_tiller_stop      = .true.       
                else
                    !--- Increase tillering
                    fl_tiller_increase  = .true.
                    fl_tiller_stop      = .false.
                endif
                
                !--- No change in tillering
                if(fl_tiller_stop)then
                    fl_tiller_increase  = .false.
                    fl_tiller_decrease  = .false.
                endif                
                    
                !--- Tillering rate
                if(fl_tiller_increase)then
                    !--- Increase number of tiller
                    dnstk = disoil / tillochron
                elseif(fl_tiller_decrease)then
                    !--- Decrease number of tiller
                    dnstk = dnstk_dead_rate * disoil
                else
                    !--- Stabilize plant population
                    dnstk = 0.d0                    
                endif
            
            end select
        
        !--- Dead Tissues rate due to tiller senescence
        if(dnstk .lt. 0.d0)then   
            
            !--- Dead Biomass rates [ton ha-1]
            ddw_lf_dead         =   tiller_senes(dw_lf,     nstk_now, dnstk, tilleragefac, tillerageprof, atln)
            ddw_it_dead         =   tiller_senes(dw_it,     nstk_now, dnstk, tilleragefac, tillerageprof, atln)
            ddw_it_AG_dead      =   tiller_senes(dw_it_AG,  nstk_now, dnstk, tilleragefac, tillerageprof, atln)
            dstr_it_AG_dead     =   tiller_senes(str_it_AG, nstk_now, dnstk, tilleragefac, tillerageprof, atln)
            dsug_it_AG_dead     =   tiller_senes(sug_it_AG, nstk_now, dnstk, tilleragefac, tillerageprof, atln)
            ddw_it_BG_dead      =   tiller_senes(dw_it_BG,  nstk_now, dnstk, tilleragefac, tillerageprof, atln)
            dstr_it_BG_dead     =   tiller_senes(str_it_BG, nstk_now, dnstk, tilleragefac, tillerageprof, atln)
            dsug_it_BG_dead     =   tiller_senes(sug_it_BG, nstk_now, dnstk, tilleragefac, tillerageprof, atln)            
            dwat_it_AG_dead     =   tiller_senes(wat_it_AG, nstk_now, dnstk, tilleragefac, tillerageprof, atln)
            
            !--- Dead Leaf Area Index [m2 m-2]
            dlai_dead = ddw_lf_dead * (1.e6/1.e4) * sla / 1.e4
            
        endif        
    endif
    
    !--- Leaf shedding rate
    if(fl_shed_leaf)then
        
        !--- Senesced leaf dry weight and area
        dw_lf_shed_phy      =   phprof(n_lf_alive + 1, 6)
        la_lf_shed_phy      =   phprof(n_lf_alive + 1, 5)
        nstk_at_appearance  =   phprof(n_lf_alive + 1, 10)
        fl_lf_AG_phy        =   fl_lf_AG(n_lf_alive + 1)
        
        !--- Shed DW Leaf rate [ton ha-1]
        ddw_lf_shed = dw_lf_shed_phy * nstk_now * tilleragefac * (1.e4/1.e6) * min(1.d0, nstk_at_appearance / nstk_now)
        
        !--- Shed Leaf Area Index [m2 m-2]
        dlai_shed   = la_lf_shed_phy * nstk_now * tilleragefac / 1.e4   * min(1.d0, nstk_at_appearance / nstk_now)
        
        !--- Update dead leaves counter
        n_lf_dead   =   n_lf_dead   + 1
        
        if(fl_lf_AG_phy)then
            !--- Above ground leaf
            n_lf_dead_AG    =   n_lf_dead_AG    + 1
        else
            !--- Below ground leaf
            n_lf_dead_BG    =   n_lf_dead_BG    + 1        
        endif
        
        !--- Leaf is shed, update flag
        fl_shed_leaf    =   .false.
        
        !--- Update leaf status
        fl_lf_alive(n_lf_alive + 1) = .false.
        
    endif
    
    !--- Leaf appearance
    if(fl_appear_leaf)then
    
        !--- Leaf Area Index Gain [m2 m-2]
        dlai_gain_appear    = phprof(1, 5) * nstk_now * tilleragefac / 1.e4 
        
        !--- Leaf Dry Weight Gain [ton ha-1]
        ddw_lf_appear       = phprof(1, 6) * nstk_now * tilleragefac * 1.e4/1.e6
        
        !--- Leaf appeared, update flag
        fl_appear_leaf = .false.
    
    endif
        
        !---------------------!
        !--- Sink Strength ---!
        !---------------------!
        
        !--- Phytomer Level
        do phy = 1, n_ph
            
            !--- Check which temperature to use (soil or air) based on phytomer position
            
            if(fl_it_AG(phy))then                
                !--- Above ground
                diphy   = di
                t_mresp = tmn
            else
                !--- Below ground
                diphy   = disoil
                t_mresp = soiltemperature
            endif
            
            !---------------------------!
            !--- Flag of active leaf ---!
            !---------------------------!
            if(phy .le. n_lf_alive)then
                fl_hasleaf  = .true.
            else
                fl_hasleaf  = .false.
            endif
            !---------------------------!
            
            !---------------------------------!
            !--- Flag of visible internode ---!
            !---------------------------------!
            if(phy .gt. n_lf_it_form)then
                fl_it_visible   = .true.
            else
                fl_it_visible   = .false.
            endif
            !---------------------------------!
                        
            if(fl_hasleaf) then
                
                !------------!
                !--- Leaf ---!
                !------------!
                
                !--- Leaf Age [Cdays]
                age_lf_phy      = phprof(phy, 1)
                
                !--- Leaf Dry Weight [g]
                dw_lf_phy       = phprof(phy, 6)
                
                !--- Leaf Initial DW [g]
                ini_dw_lf_phy   = phprof(phy, 9)
            
                !--- Leaf Maintenance respiration [gCH2O]
                mresp_lf    = dw_lf_phy * (kmr_leaf * q10_leaf ** ((tmn - tref_mr) / 10.))                
                
                !--- Leaf Dry Weight Sink Strength rate [gDW]
                dw_ss_lf    = fgrowth(1, max_lf_dw, ini_dw_lf_phy, 0., mid_tt_lf_growth, end_tt_lf_growth, age_lf_phy, 1.) * dileaf
            
                !--- Leaf Dry Weight Growth Respiration [gCH2O]
                gresp_lf    = (dw_ss_lf * (1.d0 / (1.d0 - gresp))) - dw_ss_lf
                
                !--- Age rate for each living leaf
                dage_lf_phy    = max(0.d0, dileaf)
                
                !--- Integrate leaf sink strength
                dtlfss = dtlfss + (gresp_lf + mresp_lf + dw_ss_lf)
                
                !--- Update Leaf sink strength rate [gCH2O]
                phprof(phy,2)   = gresp_lf + mresp_lf + dw_ss_lf
                phprof(phy,25)  = mresp_lf                          ! mresp leaf
                phprof(phy,26)  = gresp_lf                          ! gresp leaf
                phprof(phy,27)  = dw_ss_lf                          ! dw ss leaf
                phprof(phy,55)  = dage_lf_phy  
                
                !--- Total Growth/Maintenance Respiration and DW Sink Strength 
                tot_gresp_lf = tot_gresp_lf + gresp_lf  ! [gCH2O]
                tot_mresp_lf = tot_mresp_lf + mresp_lf  ! [gCH2O]
                tot_dw_ss_lf = tot_dw_ss_lf + dw_ss_lf  ! [gDW]
                
            endif
                        
            !-----------------!
            !--- Internode ---!
            !-----------------!
                
            !--- Internode Age [Cdays]
            age_it_phy  = phprof(phy,58)
                
            !--- Internode Dry Weight [g]
            dw_it_phy   = phprof(phy,50)
            
            !--- Maintenance Respiration
            kmr_it_phy      = phprof(phy,11)
            q10_it_phy      = phprof(phy,13)
            tref_mr_it_phy  = tref_mr
            
            !--- Internode Maintenance respiration [gCH2O]
            mresp_it    = dw_it_phy * (kmr_it_phy * q10_it_phy ** ((t_mresp - tref_mr_it_phy) / 10.))
            dw_ss_it    = 0.d0
            gresp_it    = 0.d0
            dage_it_phy = 0.d0
            
            if(fl_it_visible)then
                
                !--- Actively Growing Internode
                !--- Internode Dry Weight Sink Strength rate [gDW]                
                max_it_dw_phy   = phprof(phy, 14)                
                
                dw_ss_it    = fgrowth(1, max_it_dw_phy, 0., 0., mid_tt_it_growth, end_tt_it_growth, age_it_phy, 1.) * diphy
            
                !--- Internode Dry Weight Growth Respiration [gCH2O]
                gresp_it    = (dw_ss_it * (1.d0 / (1.d0 - gresp))) - dw_ss_it
            
                !--- Age rate for each internode
                dage_it_phy = max(0.d0, diphy)
                
            endif
                
            !--- Integrate Internode sink strength                
            if(fl_it_AG(phy))then
                    
                !--- Above Ground
                dtitss_AG = dtitss_AG + (gresp_it + mresp_it + dw_ss_it)
                    
                !--- Total Growth/Maintenance Respiration and DW Sink Strength 
                tot_gresp_it_AG = tot_gresp_it_AG + gresp_it ! [gCH2O]                 
                tot_mresp_it_AG = tot_mresp_it_AG + mresp_it ! [gCH2O]          
                tot_dw_ss_it_AG = tot_dw_ss_it_AG + dw_ss_it ! [gDW]       
                    
            else
                    
                !--- Below Ground
                dtitss_BG = dtitss_BG + (gresp_it + mresp_it + dw_ss_it)
                    
                !--- Total Growth/Maintenance Respiration and DW Sink Strength 
                tot_gresp_it_BG = tot_gresp_it_BG + gresp_it ! [gCH2O]
                tot_mresp_it_BG = tot_mresp_it_BG + mresp_it ! [gCH2O]
                tot_dw_ss_it_BG = tot_dw_ss_it_BG + dw_ss_it ! [gDW]
                    
            endif
            
            !--- Update phytomer profile
            phprof(phy,7)   = gresp_it + mresp_it + dw_ss_it
            phprof(phy,36)  = mresp_it                          ! mresp internode
            phprof(phy,37)  = gresp_it                          ! gresp internode
            phprof(phy,38)  = dw_ss_it                          ! dw ss internode
            phprof(phy,57)  = dage_it_phy                       ! age rate internode            
            phprof(phy,56)  = max(0.d0, diphy)                  ! age rate phytomer
            
        enddo
        
        !------------!
        !--- Root ---!
        !------------!
        
        !--- Roots Dry Weight Sink Strength rate [gDW m-2]
        dw_ss_rt    = fgrowth(1, max_rt_dw, ini_dw_rt, 0., mid_tt_rt_growth, end_tt_rt_growth, diacsoil, 1.) * disoil
        tot_dw_ss_rt= dw_ss_rt
        
        !--- Roots Dry Weight Growth Respiration [gCH2O m-2]
        gresp_rt     = (dw_ss_rt * (1.d0 / (1.d0 - gresp))) - dw_ss_rt
        tot_gresp_rt = gresp_rt
            
        !--- Roots Maintenance respiration [gCH2O m-2]
        mresp_rt     = (dw_rt * 1.e6/1.e4) * (kmr_root * q10_root ** ((soiltemperature - tref_mr) / 10.))
        tot_mresp_rt = mresp_rt
        
        !--- Total Roots sink strength [gCH2O m-2]
        dtrtss      = gresp_rt + mresp_rt + dw_ss_rt
                    
        !--- Use the initial plant population to scale
        !--- Upscale total substrates needed for leaf and internodes growth [gCH2O m-2] (Tiller Age factor added to account for different shoot ages in the same area)
        dtlfss      = dtlfss                    * nstk_now  * tilleragefac
        dtitss      = (dtitss_BG + dtitss_AG)   * nstk_now  * tilleragefac
        dtitss_BG   = dtitss_BG                 * nstk_now  * tilleragefac
        dtitss_AG   = dtitss_AG                 * nstk_now  * tilleragefac
        
        !--- Upscale total substrates needed for leaf and internodes growth/maintenance respiration and DW [gCH2O m-2, gDW m-2] (Tiller Age factor added to account for different shoot ages in the same area)
        tot_gresp_lf    = tot_gresp_lf          * nstk_now * tilleragefac
        tot_mresp_lf    = tot_mresp_lf          * nstk_now * tilleragefac
        tot_gresp_it_AG = tot_gresp_it_AG       * nstk_now * tilleragefac ! Above Ground
        tot_mresp_it_AG = tot_mresp_it_AG       * nstk_now * tilleragefac ! Above Ground       
        tot_gresp_it_BG = tot_gresp_it_BG       * nstk_now * tilleragefac ! Below Ground
        tot_mresp_it_BG = tot_mresp_it_BG       * nstk_now * tilleragefac ! Below Ground
        tot_dw_ss_lf    = tot_dw_ss_lf          * nstk_now * tilleragefac
        tot_dw_ss_it_AG = tot_dw_ss_it_AG       * nstk_now * tilleragefac
        tot_dw_ss_it_BG = tot_dw_ss_it_BG       * nstk_now * tilleragefac        
        
        !--- Total internodes sink strengths
        tot_gresp_it    = tot_gresp_it_AG + tot_gresp_it_BG
        tot_mresp_it    = tot_mresp_it_AG + tot_mresp_it_BG
        tot_dw_ss_it    = tot_dw_ss_it_AG + tot_dw_ss_it_BG
        
        !--------------------------!
        !--- Crop Sink Strength ---!
        !--------------------------!
        
        !--- Total substrates needed for crop growth [gCH2O m-2]
        dtcrss = dtitss + dtlfss + dtrtss        
        
        !--- Total substrates needed for crop growth/maintenance respiration and growth [gCH2O m-2]
        tot_gresp_crop = (tot_gresp_it_AG + tot_gresp_it_BG) + tot_gresp_lf + tot_gresp_rt
        tot_mresp_crop = (tot_mresp_it_AG + tot_mresp_it_BG) + tot_mresp_lf + tot_mresp_rt
        tot_dw_ss_crop = (tot_dw_ss_it_AG + tot_dw_ss_it_BG) + tot_dw_ss_lf + tot_dw_ss_rt
        
        !------------------------------!
        !--- Relative Sink Strength ---!
        !------------------------------!
        
        !--- Daily Relative Sink Strength by organ pool
        if(dtcrss .gt. z)then !To avoid NaN
            dr_rtss =   dtrtss / dtcrss
            dr_lfss =   dtlfss / dtcrss
            dr_itss =   dtitss / dtcrss
        else
            dr_rtss =   0.d0
            dr_lfss =   0.d0
            dr_itss =   0.d0
        endif
        
        !--- Carbon Balance Check
        if(dtcrss .gt. z)then
            if((dr_rtss + dr_lfss + dr_itss) .lt. 1.d0)then            
                !--- Send to roots
                dr_rtss = dr_rtss + 1.d0 - (dr_rtss + dr_lfss + dr_itss)            
            else if((dr_rtss + dr_lfss + dr_itss) .gt. 1.d0)then            
                !--- Reduce roots pf
                dr_rtss = dr_rtss - ((dr_rtss + dr_lfss + dr_itss) - 1.d0)
            endif
        endif
                                
        !--- Below ground fraction of growing internodes (considering leaf profile as the growing phytomers)
        if(dtitss_AG .gt. 0.d0) frac_AG = max(0.d0, min(1.d0, dtitss_AG/dtitss))
        if(dtitss_BG .gt. 0.d0) frac_BG = max(0.d0, min(1.d0, dtitss_BG/dtitss))
        
    !-------------------!
    !--- CROP GROWTH ---!
    !-------------------!    
    
    if(.not. flemerged)then
        
        !----------------------------------------------------------------!
        !--- Available CH2O for Growth & Maintenance Before Emergence ---!
        !----------------------------------------------------------------!
        
        !--- Available substrates for crop growth and maintenance [tonCH2O ha-1]
        subsres             = sug_it_BG             ! Use only below ground sugar pool      [ton ha-1]
        
        !--- CO2 Assimilation rate 
        dtg                 = 0.d0                  ! No CO2 assimilation before emergence  [ton ha-1]
                
    else
    
        !----------------------------------------------------------------!
        !--- Available CH2O for Growth & Maintenance After Emergence ---!
        !----------------------------------------------------------------!
        
        !--- Available substrates for crop growth and maintenance [tonCH2O ha-1]
        subsres             = sug_it_BG    ! Use only below ground sugar pool      [ton ha-1]        
        
        !----------------------!
        !--- Photosynthesis ---!
        !----------------------!
        
        !--- Select among photosynthesis methods
        select case(metpg)
        
        case(1)
            
            !------------------!
            !--- RUE method ---!
            !------------------!
            
            !--- WARNING: This method do not take into account Growth + Maintenance Respiration. The reason is because most (all to date) of RUE values found in literature
            !--- are computed based on dry biomass ~ IPAR. In other words, when dry biomass is weighted part of maintenance respiration is lost in the transition time of sampling-weighting
            !--- AND the most important: the growth respiration has already gone, otherwise we couldnt be weighting anything!
            
            !--- Reduced Radiation Use Efficiency [gDW/MJPAR/m2]
            rue_mod = rue * agefactor_rue * tempfac_pho * pho_fac_co2 * swfacp
            
            !--- Carbon Gain [gDW/m2]
            dtg     = rue_mod * li * par_rad
            
            !--- Convert to ton ha-1
            dtg     =   dtg * (1.e4/1.e6)
        
        case(2)
            
            !----------------------------------------!
            !--- CO2 Assimilation by Canopy Layer ---!
            !----------------------------------------!
            
            !--- Astrological calculations for difuse and direct PAR interception on hourly-step            
            call DAYLEN(doy, lat_sim, dayl, DEC, SNDN, SNUP)               
            call SOLAR(dayl, DEC, srad, lat_sim, CLOUDS, ISINB, S0N)
                        
            !--- Convert CO2 Assimilation rate to kgCO2 ha-1 h-1
            amax_conv   = amax / 1.e3 * 1.e4 * 3600 * 44.d0 / 1.e6
            
            !--- Convert Quantum Efficiency to kgCO2 ha-1 h-1 (J m-2 s-1)-1
            eff_conv    = eff / 1.e3 * 1.e4 * 3600 * 44.d0 / 1.e6 * 4.6
            
            !--- Reduced Maximum Assimilation Rate [mmol m-2 s-1]
            !--- Here we assume temperature, water stress and feedback response mainly affect the maximum assimilation rate, rather than quantum eff.
            amax_mod = amax_conv * tempfac_pho * swfacp * amaxfbfac * pho_fac_co2
            eff_mod  = eff_conv
            
            !--- LAI for CO2 assimilation
            lai_ass  = lai
             
            !--- get daily PAR in moles[quanta]/m2-d from WEATHER % PAR
            !--- Note: WEATHER % PAR = 0.42*SRAD, to use PAR = 0.50*SRAD use: par_sim = srad * 0.5 * 4.6  ! (moles[quanta]/m2-d) [MV 2022-08-02]
            par_sim = WEATHER % PAR     ! (moles[quanta]/m2-d)
            
            !--- Total assimilation for three canopy layers on hourly-step (Gaussian Integration) - Groudriaan
            if(dayl .gt. 0.d0) then
                call totass(                    &  !doy,
                            dayl,               &
                            lat_sim,            &
                            DEC,                &
                            SNDN,               &
                            SNUP,               &
                            CLOUDS,             &
                            ISINB,              &
                            S0N,                &
                            amax_mod,           &
                            eff_mod,            &
                            lai_ass,            &
                            kdif,               &
                            c_scattering,       &
                            srad,               & ! 
                            par_sim,            &                            
                            dtg,                & ! Output
                            Acanopy,            & ! Output
                            Qleaf,              & ! Output
                            incpar,             & ! Output
                            photo_layer_act,    & ! Output
                            frac_li)              ! Output
                            
            else
            
                !--- no radiation [polar zone]
                dtg             = 0.d0
                Acanopy         = 0.d0
                Qleaf           = 0.d0
                incpar          = 0.d0
                photo_layer_act = 0.d0
                frac_li         = 0.d0
            
            endif
            
            !--- Convert CO2 assimilation to CH2O assimilation rate [kgCH2O ha-1] (stoichiometric conversion)
            dtg             = dtg               * 30.d0/44.d0
            photo_layer_act = photo_layer_act   * 30.d0/44.d0
            
            !--- Convert to ton ha-1
            dtg             = dtg               * 1.e-3
            photo_layer_act = photo_layer_act   * 1.e-3
            
            !--- Daily PAR intercepted (MJ m-2)
            dacc_par        = frac_li * par_rad
            
        case(3)
            
            !-----------------------------------!
            !--- Canopy Gross Photosynthesis ---!
            !-----------------------------------!
            
            !--- Canopy gross photosysntesis rate
!           call PGS(swfacp,1.,1.,1.,chustk,par_rad,lai,dtg,resp,diac,tmn,dw_total,CCEFF,CCMAX,k,PHTMAX,CCMP,PARMAX)
            call PGS(swfacp,1.,1.,1.,       par_rad,lai,dtg,resp,diac,tmn,dw_total,CCEFF,CCMAX,k,PHTMAX,CCMP,PARMAX)
            
            !--- Growth and Maintenance respiration (computed on PGS subroutine)
            dtg = max(0.d0,dtg)
           
        end  select    
    endif
    
    !----------------------!
    !--- Carbon Balance ---!
    !----------------------!
    
    !--- Available CH2O for leaves, internodes and roots [g m-2]
    dtg_avail_rt        = dtg       * dr_rtss * (1.e6/1.e4)
    dtg_avail_lf        = dtg       * dr_lfss * (1.e6/1.e4)
    dtg_avail_it        = dtg       * dr_itss * (1.e6/1.e4)
    dtg_avail_it_BG     = dtg       * dr_itss * (1.e6/1.e4) * frac_BG
    dtg_avail_it_AG     = dtg       * dr_itss * (1.e6/1.e4) * frac_AG
        
    !--- Available CH2O reserves in case its needed [g m-2]
    subsres_avail_rt    = subsres   * dr_rtss * (1.e6/1.e4)
    subsres_avail_lf    = subsres   * dr_lfss * (1.e6/1.e4)
    subsres_avail_it    = subsres   * dr_itss * (1.e6/1.e4)
    subsres_avail_it_BG = subsres   * dr_itss * (1.e6/1.e4) * frac_BG
    subsres_avail_it_AG = subsres   * dr_itss * (1.e6/1.e4) * frac_AG
        
    !---  Crop will use its reserves to growth before emergence (searching for light!) [ton ha-1]        
    if(fl_use_reserves)then
            
        !------------------------------!
        !--- Use reserves to growth ---!
        !------------------------------!
        
        !--- Total substrates available to growth [g m-2]
        subs_avail_growth_crop  = (dtg + subsres) * (1.e6/1.e4)
        
        !--- This condition will happen before emergence [g m-2]
        subs_avail_growth_rt    = dtg_avail_rt  	+   subsres_avail_rt
        subs_avail_growth_lf    = dtg_avail_lf		+   subsres_avail_lf
        subs_avail_growth_it    = dtg_avail_it      +   subsres_avail_it
        subs_avail_growth_it_BG = dtg_avail_it_BG   +   subsres_avail_it_BG
        subs_avail_growth_it_AG = dtg_avail_it_AG   +   subsres_avail_it_AG
            
        !--- Carbon balance
        subsres_avail_rt    = 0.d0
        subsres_avail_lf    = 0.d0
        subsres_avail_it    = 0.d0
        subsres_avail_it_BG = 0.d0
        subsres_avail_it_AG = 0.d0
            
    else
            
        !-------------------------------------------!
        !--- Use only CO2 assimilation to growth ---!
        !-------------------------------------------!
        
        !--- Total substrates available to growth [g m-2]
        subs_avail_growth_crop  = dtg * (1.e6/1.e4)
        
        !--- Still the reserves will be used for maintenance when renecessary [g m-2]
        subs_avail_growth_rt    = dtg_avail_rt  
        subs_avail_growth_lf    = dtg_avail_lf	
        subs_avail_growth_it    = dtg_avail_it   
        subs_avail_growth_it_BG = dtg_avail_it_BG
        subs_avail_growth_it_AG = dtg_avail_it_AG
            
    endif
    
    !------------------------------!
    !--- Crop Source-Sink Ratio ---!
    !------------------------------!
    
    if(dtcrss .gt. 0.d0) cr_source_sink_ratio = subs_avail_growth_crop / dtcrss    
    
    !------------!
    !--- Leaf ---!
    !------------!
    call subs_balance(  subs_avail_growth_lf,   & ! Input
                        subsres_avail_lf,       & ! Input
                        dtlfss,                 & ! Input
                        tot_mresp_lf,           & ! Input
                        tot_gresp_lf,           & ! Input
                        tot_dw_ss_lf,           & ! Input
                        sup_ratio_lf,           & ! Output
                        supply_rate_lf,         & ! Output
                        supply_used_lf,         & ! Output
                        supply_used_mresp_lf,   & ! Output
                        supply_used_gresp_lf,   & ! Output
                        supply_used_dw_lf,      & ! Output
                        reserves_used_mresp_lf, & ! Output
                        maintenance_factor_lf,  & ! Output
                        reduc_growth_factor_lf)   ! Output
        
    !------------!
    !--- Stem ---!
    !------------!
    call subs_balance(  subs_avail_growth_it,   & ! Input
                        subsres_avail_it,       & ! Input        
                        dtitss,                 & ! Input
                        tot_mresp_it,           & ! Input
                        tot_gresp_it,           & ! Input
                        tot_dw_ss_it,           & ! Input
                        sup_ratio_it,           & ! Output
                        supply_rate_it,         & ! Output
                        supply_used_it,         & ! Output
                        supply_used_mresp_it,   & ! Output
                        supply_used_gresp_it,   & ! Output
                        supply_used_dw_it,      & ! Output
                        reserves_used_mresp_it, & ! Output
                        maintenance_factor_it,  & ! Output
                        reduc_growth_factor_it)   ! Output
        
    !-------------------------------!
    !--- Below ground Internodes ---!
    !-------------------------------!
    call subs_balance(subs_avail_growth_it_BG,      & ! Input
                        subsres_avail_it_BG,        & ! Input        
                        dtitss_BG,                  & ! Input
                        tot_mresp_it_BG,            & ! Input
                        tot_gresp_it_BG,            & ! Input
                        tot_dw_ss_it_BG,            & ! Input
                        sup_ratio_it_BG,            & ! Output
                        supply_rate_it_BG,          & ! Output
                        supply_used_it_BG,          & ! Output
                        supply_used_mresp_it_BG,    & ! Output
                        supply_used_gresp_it_BG,    & ! Output
                        supply_used_dw_it_BG,       & ! Output
                        reserves_used_mresp_it_BG,  & ! Output
                        maintenance_factor_it_BG,   & ! Output
                        reduc_growth_factor_it_BG)    ! Output
        
    !-------------------------------!
    !--- Above ground Internodes ---!
    !-------------------------------!
    call subs_balance(  subs_avail_growth_it_AG,    & ! Input
                        subsres_avail_it_AG,        & ! Input        
                        dtitss_AG,                  & ! Input
                        tot_mresp_it_AG,            & ! Input
                        tot_gresp_it_AG,            & ! Input
                        tot_dw_ss_it_AG,            & ! Input
                        sup_ratio_it_AG,            & ! Output
                        supply_rate_it_AG,          & ! Output
                        supply_used_it_AG,          & ! Output
                        supply_used_mresp_it_AG,    & ! Output
                        supply_used_gresp_it_AG,    & ! Output
                        supply_used_dw_it_AG,       & ! Output
                        reserves_used_mresp_it_AG,  & ! Output
                        maintenance_factor_it_AG,   & ! Output
                        reduc_growth_factor_it_AG)    ! Output
    !-------------!
    !--- Roots ---!
    !-------------!
    call subs_balance(  subs_avail_growth_rt,   & ! Input
                        subsres_avail_rt,       & ! Input        
                        dtrtss,                 & ! Input
                        tot_mresp_rt,           & ! Input
                        tot_gresp_rt,           & ! Input
                        tot_dw_ss_rt,           & ! Input
                        sup_ratio_rt,           & ! Output
                        supply_rate_rt,         & ! Output
                        supply_used_rt,         & ! Output
                        supply_used_mresp_rt,   & ! Output
                        supply_used_gresp_rt,   & ! Output
                        supply_used_dw_rt,      & ! Output
                        reserves_used_mresp_rt, & ! Output
                        maintenance_factor_rt,  & ! Output
                        reduc_growth_factor_rt)   ! Output
    
    !--- Overall Crop Carbon Balance
    supply_used_crop    	     = supply_used_rt   	   	+ 	supply_used_it   	   	+	 supply_used_lf   	 
    supply_used_mresp_crop	     = supply_used_mresp_rt	   	+ 	supply_used_mresp_it   	+	 supply_used_mresp_lf	   
    supply_used_gresp_crop	     = supply_used_gresp_rt	   	+ 	supply_used_gresp_it   	+	 supply_used_gresp_lf	   
    supply_used_dw_crop	         = supply_used_dw_rt	   	+ 	supply_used_dw_it	   	+	 supply_used_dw_lf	  
    reserves_used_mresp_crop	 = reserves_used_mresp_rt  	+ 	reserves_used_mresp_it 	+	 reserves_used_mresp_lf
    maintenance_factor_crop	     = (maintenance_factor_rt  * dw_rt 		+	maintenance_factor_it  * dw_it  + 	 maintenance_factor_lf  * dw_lf) / dw_total
    reduc_growth_factor_crop	 = (reduc_growth_factor_rt * dw_rt  	+	reduc_growth_factor_it * dw_it  + 	 reduc_growth_factor_lf * dw_lf) / dw_total
        
    !--- Downscale to organ level to find structural and sugar partitioning factors
    dstr_it_BG  = 0.d0
    dsug_it_BG  = 0.d0
    dstr_it_AG  = 0.d0
    dsug_it_AG  = 0.d0
        
    !--- Total substrates available for leaves and internodes at the "reference stalk" level
    !--- Note that at below ground conditions tilleragefac should be 1.d0 (keep it here for debugging purpose)
    subs_avail_growth_lf_ref_till       = subs_avail_growth_lf      / (nstk_now  * tilleragefac)
    subs_avail_growth_it_ref_till       = subs_avail_growth_it      / (nstk_now  * tilleragefac)
    subs_avail_growth_it_BG_ref_till    = subs_avail_growth_it_BG   / (nstk_now  * tilleragefac)
    subs_avail_growth_it_AG_ref_till    = subs_avail_growth_it_AG   / (nstk_now  * tilleragefac)
            
    !--- Total substrates reserves in case needed for leaves and internodes at the "reference stalk" level
    !--- Note that all reserves are available for growth and respiration in (avail_subs_crop)
    subsres_avail_lf_ref_till           = subsres_avail_lf          / (nstk_now  * tilleragefac)
    subsres_avail_it_ref_till           = subsres_avail_it          / (nstk_now  * tilleragefac)
    subsres_avail_it_BG_ref_till        = subsres_avail_it_BG       / (nstk_now  * tilleragefac)
    subsres_avail_it_AG_ref_till        = subsres_avail_it_AG       / (nstk_now  * tilleragefac)
        
    !--- Total sink strength of all leaves and internodes
    dtlfss_ref_till             = dtlfss        / (nstk_now  * tilleragefac)
    dtitss_ref_till             = dtitss        / (nstk_now  * tilleragefac)
    dtitss_BG_ref_till          = dtitss_BG     / (nstk_now  * tilleragefac)
    dtitss_AG_ref_till          = dtitss_AG     / (nstk_now  * tilleragefac)
    
    !----------------------!
    !--- Phytomer Level ---!
    !----------------------!
    do phy = 1, n_ph
            
        !--- Check which temperature to use (soil or air) based on phytomer position
        if(fl_it_AG(phy))then                
            !--- Above ground
            diphy   = di
            t_mresp = tmn
        else
            !--- Below ground
            diphy   = disoil
            t_mresp = soiltemperature
        endif
            
        !----------------------!
        !--- Phytomer Flags ---!
        !----------------------!
            
        !--- Has an active leaf        
        if(phy .le. n_lf_alive)then
            fl_hasleaf = .true.
        else
            fl_hasleaf = .false.
        endif
            
        !--- Has a growing internode
        if(phy .gt. n_lf_it_form)then
            fl_it_visible   = .true.
        else
            fl_it_visible   = .false.
        endif
                        
        if(fl_hasleaf)then
                
            !------------!
            !--- Leaf ---!
            !------------!
                
            !--- Initial sink strengths of leaf
            dtlfss_phy      = phprof(phy,2)
            mresp_lf_phy    = phprof(phy,25)
            gresp_lf_phy    = phprof(phy,26)
            dw_ss_lf_phy    = phprof(phy,27)
                
            !--- Relative Sink Strength                
            if(dtlfss_ref_till .gt. 0.d0) rel_ss_lf_phy    = dtlfss_phy / dtlfss_ref_till
            
            !--- Substrates available for this leaf
            subs_avail_growth_lf_phy    = subs_avail_growth_lf_ref_till * rel_ss_lf_phy
                
            !--- Reserves available for this leaf in case needed
            subsres_avail_lf_phy        = subsres_avail_lf_ref_till * rel_ss_lf_phy
                
            !--- Leaf Growth
            call subs_balance(  subs_avail_growth_lf_phy,   & ! Input
                                subsres_avail_lf_phy,       & ! Input
                                dtlfss_phy,                 & ! Input
                                mresp_lf_phy,               & ! Input
                                gresp_lf_phy,               & ! Input
                                dw_ss_lf_phy,               & ! Input
                                sup_ratio_lf_phy,           & ! Output
                                supply_rate_lf_phy,         & ! Output
                                supply_used_lf_phy,         & ! Output
                                supply_used_mresp_lf_phy,   & ! Output
                                supply_used_gresp_lf_phy,   & ! Output
                                supply_used_dw_lf_phy,      & ! Output
                                reserves_used_mresp_lf_phy, & ! Output
                                maintenance_factor_lf_phy,  & ! Output
                                reduc_growth_factor_lf_phy)   ! Output          
                
            !--- Update phytomer profile rates
            phprof(phy,28)  = sup_ratio_lf_phy
            phprof(phy,29)  = supply_rate_lf_phy
            phprof(phy,30)  = supply_used_lf_phy
            phprof(phy,31)  = supply_used_mresp_lf_phy
            phprof(phy,32)  = supply_used_gresp_lf_phy
            phprof(phy,33)  = supply_used_dw_lf_phy
            phprof(phy,34)  = maintenance_factor_lf_phy
            phprof(phy,35)  = reduc_growth_factor_lf_phy
                
            !--- Leaf Blade Area Gain (cm2)
            if(flemerged)then
                if(fl_lf_AG(phy))then
                    dla_phy             = supply_used_dw_lf_phy  * sla       
                    phprof(phy, 4)      = dla_phy
                    dla_gain_ref_till   = dla_gain_ref_till +  dla_phy
                endif
            endif                
        endif
                
        !-----------------!
        !--- Internode ---!
        !-----------------!
                
        !--- Initial sink strengths of internode [gCH2O]                
        dtitss_phy      = phprof(phy,7)
        mresp_it_phy    = phprof(phy,36)
        gresp_it_phy    = phprof(phy,37)
        dw_ss_it_phy    = phprof(phy,38)
                
        if(fl_it_AG(phy))then
                
            !--- Above Ground
            
            !--- Relative Sink Strength [0-1]
            if(dtitss_AG_ref_till .gt. 0.d0) rel_ss_it_phy    = dtitss_phy / dtitss_AG_ref_till
                 
            !--- Substrates available for internode growth [g]
            subs_avail_growth_it_phy = subs_avail_growth_it_AG_ref_till * rel_ss_it_phy
                
            !--- Reserves available for this leaf in case needed
            subsres_avail_it_phy    = subsres_avail_it_AG_ref_till * rel_ss_it_phy
                
        else
            
            !--- Below Ground
            
            !--- Relative Sink Strength [0-1]
            if(dtitss_BG_ref_till .gt. 0.d0) rel_ss_it_phy    = dtitss_phy / dtitss_BG_ref_till
                 
            !--- Substrates available for internode growth [g]
            subs_avail_growth_it_phy = subs_avail_growth_it_BG_ref_till * rel_ss_it_phy
                
            !--- Reserves available for this leaf in case needed
            subsres_avail_it_phy    = subsres_avail_it_BG_ref_till * rel_ss_it_phy
                
        endif            
            
        !--- Internode Growth [g]            
        call subs_balance(  subs_avail_growth_it_phy,   & ! Input
                            subsres_avail_it_phy,       & ! Input
                            dtitss_phy,                 & ! Input
                            mresp_it_phy,               & ! Input
                            gresp_it_phy,               & ! Input
                            dw_ss_it_phy,               & ! Input
                            sup_ratio_it_phy,           & ! Output
                            supply_rate_it_phy,         & ! Output
                            supply_used_it_phy,         & ! Output
                            supply_used_mresp_it_phy,   & ! Output
                            supply_used_gresp_it_phy,   & ! Output
                            supply_used_dw_it_phy,      & ! Output
                            reserves_used_mresp_it_phy, & ! Output
                            maintenance_factor_it_phy,  & ! Output
                            reduc_growth_factor_it_phy)   ! Output
            
        !--- Internode Age [Cdays]
        age_it_phy      = phprof(phy,58)
                
        !--- Structural partitioning factor [0-1]
        it_struc_pfac_rate = it_struc_pfac( it_struc_tb_ini,            &
                                            it_struc_to1,               &
                                            it_struc_to2,               &
                                            it_struc_tb_end,            &
                                            it_struc_pfac_temp_max_red, &
                                            it_struc_pfac_wate_max_red, &
                                            it_struc_pfac_max,          &
                                            it_struc_pfac_min,          &
                                            it_struc_pfac_tb,           &
                                            it_struc_pfac_tm,           &
                                            it_struc_pfac_te,           &
                                            it_struc_pfac_delta,        &
                                            age_it_phy,                 &
                                            t_mresp,                    &
                                            swfacf)                      ! From SOPLAT
            
        !--- Total Sugars and Structural rate [g]
        dstr_it_phy  = it_struc_pfac_rate           *   supply_used_dw_it_phy
        dsug_it_phy  = (1.d0 - it_struc_pfac_rate)  *   supply_used_dw_it_phy
            
        !--- Update phytomer profile
        phprof(phy,39)  = sup_ratio_it_phy
        phprof(phy,40)  = supply_rate_it_phy
        phprof(phy,41)  = supply_used_it_phy
        phprof(phy,42)  = supply_used_mresp_it_phy
        phprof(phy,43)  = supply_used_gresp_it_phy
        phprof(phy,44)  = supply_used_dw_it_phy
        phprof(phy,45)  = maintenance_factor_it_phy
        phprof(phy,46)  = reduc_growth_factor_it_phy
        phprof(phy,47)  = supply_used_dw_it_phy
        phprof(phy,48)  = dstr_it_phy
        phprof(phy,49)  = dsug_it_phy
        phprof(phy,15)  = it_struc_pfac_rate
                    
        !--- Integrate structural and sugar parts rates
        if(fl_it_AG(phy))then
                    
            !--- Above Ground
            dstr_it_AG  = dstr_it_AG    +   dstr_it_phy
            dsug_it_AG  = dsug_it_AG    +   dsug_it_phy    
                    
        else
                    
            !--- Below Ground
            dstr_it_BG  = dstr_it_BG    +   dstr_it_phy
            dsug_it_BG  = dsug_it_BG    +   dsug_it_phy
                    
        endif                   
    enddo
        
    !--- Upscale Structural and Sugar Rates to Field Level [g m-2]
    dstr_it_BG  = dstr_it_BG	* nstk_now * tilleragefac
    dsug_it_BG  = dsug_it_BG	* nstk_now * tilleragefac
    dstr_it_AG  = dstr_it_AG	* nstk_now * tilleragefac
    dsug_it_AG  = dsug_it_AG	* nstk_now * tilleragefac
            
    !--- Correction factor to meet carbon balance
    dsug_corr_fac_BG   = 1.d0
    dsug_corr_fac_AG   = 1.d0    
    if((dstr_it_BG + dsug_it_BG) .gt. z .and. supply_used_dw_it_BG .gt. 0.d0) dsug_corr_fac_BG   = supply_used_dw_it_BG / (dstr_it_BG + dsug_it_BG)
    if((dstr_it_AG + dsug_it_AG) .gt. z .and. supply_used_dw_it_AG .gt. 0.d0) dsug_corr_fac_AG   = supply_used_dw_it_AG / (dstr_it_AG + dsug_it_AG)
        
    !--- Check carbon balance discrepancy
    c_check_tol = 0.10
    if(abs(1.d0 - dsug_corr_fac_BG) .gt. c_check_tol)then
!        write(warn,*) 'More than 10% Carbon Balance Discrepancy on Internode Below Ground Sugar/Structural Partitioning at DAS: ',das, &
!            'Discrepancy of ', abs(1.d0 - dsug_corr_fac_BG) * 100.d0, '% was corrected.'
    endif
        
    c_check_tol = 0.10
    if(abs(1.d0 - dsug_corr_fac_AG) .gt. c_check_tol)then
!        write(warn,*) 'More than 10% Carbon Balance Discrepancy on Internode Above Ground Sugar/Structural Partitioning at DAS: ',das, &
!            'Discrepancy of ', abs(1.d0 - dsug_corr_fac_AG) * 100.d0, '% was corrected.'
    endif  
        
    !--- Correct to meet carbon balance
    dstr_it_BG  = dstr_it_BG    *   dsug_corr_fac_BG
    dsug_it_BG  = dsug_it_BG	*   dsug_corr_fac_BG
        
    !--- Biomass Rates [ton ha-1]
    ddw_rt      = supply_used_dw_rt     * (1.e4/1.e6)
    ddw_lf      = supply_used_dw_lf     * (1.e4/1.e6)
    ddw_it      = supply_used_dw_it     * (1.e4/1.e6)
    ddw_it_BG   = supply_used_dw_it_BG  * (1.e4/1.e6)        
    ddw_it_AG   = supply_used_dw_it_AG  * (1.e4/1.e6)
    dstr_it_BG  = dstr_it_BG            * (1.e4/1.e6)
    dsug_it_BG  = dsug_it_BG	        * (1.e4/1.e6)
    dstr_it_AG  = dstr_it_AG	        * (1.e4/1.e6)
    dsug_it_AG  = dsug_it_AG	        * (1.e4/1.e6)
    
    !--- Leaf Area Index Gain [m2 m-2]
    dlai_gain   = dla_gain_ref_till     * (nstk_now * tilleragefac) / 1.e4
        
    !--- Exceeded Substrates [tonCH2O ha-1]
    if(dtg_avail_lf .gt. dtlfss) exc_dtg_lf = (dtg_avail_lf - dtlfss) * (1.e4/1.e6)
    if(dtg_avail_it .gt. dtitss) exc_dtg_it = (dtg_avail_it - dtitss) * (1.e4/1.e6)
    if(dtg_avail_rt .gt. dtrtss) exc_dtg_rt = (dtg_avail_rt - dtrtss) * (1.e4/1.e6)
        
    !--- Net Rate of Reserves
    if(fl_use_reserves)then
            
        if(.not. dtg_avail_lf .gt. dtlfss) reserves_used_growth_lf = supply_used_lf - dtg_avail_lf
        if(.not. dtg_avail_it .gt. dtitss) reserves_used_growth_it = supply_used_it - dtg_avail_it
        if(.not. dtg_avail_rt .gt. dtrtss) reserves_used_growth_rt = supply_used_rt - dtg_avail_rt
            
    else
        reserves_used_growth_lf = 0.d0
        reserves_used_growth_it = 0.d0
        reserves_used_growth_rt = 0.d0               
    endif    
        
    !--- Substrate reserves balance
    dsubsres_lf     = exc_dtg_lf - reserves_used_mresp_lf * (1.e4/1.e6) - reserves_used_growth_lf * (1.e4/1.e6)
    dsubsres_it     = exc_dtg_it - reserves_used_mresp_it * (1.e4/1.e6) - reserves_used_growth_it * (1.e4/1.e6)
    dsubsres_rt     = exc_dtg_rt - reserves_used_mresp_rt * (1.e4/1.e6) - reserves_used_growth_rt * (1.e4/1.e6)
    dsubsres        = dsubsres_lf + dsubsres_it + dsubsres_rt
    
    !--- Overall Biomass Gain
    ddw     =    ddw_rt + (ddw_lf + ddw_lf_appear) + ddw_it + dsubsres
    
    !--- Ratio Reserves Variation    
    dsubsres_ratio  = 1.d0
    if(subsres .gt. 0.d0) dsubsres_ratio  = (subsres + dsubsres) / subsres
    
    !-------------------!
    !--- Stalk Rates ---!
    !-------------------!
    if(flemerged)then
        
        !--- Above ground conditions
        if(fl_stalk_emerged)then
            
            !-------------------------------------------!
            !--- Stalk has emerged from soil surface ---!
            !-------------------------------------------!
            
            !--- Plant extension rate
            do hour = 1, 24            
                !--- Hourly plant extension rate [mm h-1]
			    per_hour = dpercoeff * tempfac_h_per(hour) * swface * agefactor_per
			
                !--- Integrate to daily rate [mm day-1]
			    per = per + per_hour	        
            enddo
        
            !--- Fraction of stalk extension partitioned among internodes as function of structural gain        
            dtot_str_dw_ref_till = sum(phprof(1:n_ph,15))
            
            if(dtot_str_dw_ref_till .gt. 0.d0)then
                !--- Only extends when structural gain is positive
                do phy = 1, n_ph
                    !--- Internode extension rate [mm]
                    per_it_phy     = min(max_per_it, per * (phprof(phy,15) / dtot_str_dw_ref_till)) ! dLength [mm]      
                    phprof(phy,23) = per_it_phy ! dLength [mm]      
                enddo
            endif
                        
            !--------------------------------!
            !--- Water Fraction in Stalks ---!
            !--------------------------------!
            
            !--- Computing water fraction rate of Stalks for Stalk Fresh Mass [Fresh Cane Yield: ton ha-1]
            !--- Following Martines SASTA 2001 and Mathew Jones (CANEGRO)
            dwat_it_AG = (dswat_ddws * ddw_it_AG) - (dswat_dsuc * dsug_it_AG)              
        
        endif
        
    else
        
        !--- Crop is still below the ground surface        
        !--- Shoot extension towards soil surface [cm]
        dshootext_BG    =   dshootext_BG_rate     *   disoil
        
    endif
        
    !-------------------!
    !--- Root Growth ---!
    !-------------------!
        
    !--- Root Front velocity [cm day-1]
    drdepth = rootdrate * disoil 
        
    !--- Number of sublayers for root profile rate [1cm resolution]
    nsublay = aint(dep(nlay))
    
    !--- Root Profile Rate (RLD and Biomass)
    call root_profile(nlay			                , & ! Input
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
       
    !--- Root Senescence (PIT)
    ddw_rt_dead = 0.d0
    drld_dead   = 0.d0
    
    !------------------------!
    !--- Step Integration ---!
    !------------------------!
    
    !--- Phytomer Level Integration    
    do phy   = 1, n_ph
    
        !--- Integrate phytomer attributes
        phprof(phy, 1)  = phprof(phy, 1) + phprof(phy,55) ! Leaf Age                    [Cdays]
        phprof(phy,58)  = phprof(phy,58) + phprof(phy,57) ! Internode Age               [Cdays]
        phprof(phy,12)  = phprof(phy,12) + phprof(phy,56) ! Phytomer Age                [Cdays]
        phprof(phy, 6)  = phprof(phy, 6) + phprof(phy,33) ! Leaf Dry Weight             [g]
        phprof(phy, 5)  = phprof(phy, 5) + phprof(phy, 4) ! Leaf Area                   [cm2]
        phprof(phy,16)  = phprof(phy,16) + phprof(phy,23) ! Internode length            [mm]
                
        if(.not. fl_it_AG(phy))then
            
            !--- Carbon balance of growth
            dsug_it_phy_growth      = phprof(phy,49)
            dstr_it_phy_growth      = phprof(phy,48)
            ddw_it_phy_growth       = phprof(phy,47)
            
            !--- Carbon balance of reserves
            dsug_it_phy_reserves    = max(0.d0, (phprof(phy,52)  *   dsubsres_ratio) - phprof(phy,52))
            ddw_it_phy_reserves     = dsug_it_phy_reserves            
            
            !--- Current State
            dw_it_phy               = phprof(phy,50)
            str_it_phy              = phprof(phy,51)  
            sug_it_phy              = phprof(phy,52)
            
            !--- Integrate Total Dry Weight and Sugars [g]
            dw_it_phy      = dw_it_phy  + ddw_it_phy_growth     +   ddw_it_phy_reserves
            str_it_phy     = str_it_phy + dstr_it_phy_growth 
            sug_it_phy     = sug_it_phy + dsug_it_phy_growth    +   dsug_it_phy_reserves
            
            !--- Update internode profile
            phprof(phy,50)  = dw_it_phy  ! Internode Dry Weight        [g]
            phprof(phy,51)  = str_it_phy ! Internode Structural Weight [g]
            phprof(phy,52)  = sug_it_phy ! Internode Sugars Weight     [g]
            
            !--- Sucrose and Hexose Contents at below ground internodes [g]
            !--- 50% shared due to unkown
            suc_it_phy  =   frac_suc_BG   *   sug_it_phy   
            hex_it_phy  =   frac_hex_BG   *   sug_it_phy
            
             !--- Below Ground
            suc_it_BG_ref_till  = suc_it_BG_ref_till    +   suc_it_phy
            hex_it_BG_ref_till  = hex_it_BG_ref_till    +   hex_it_phy   
            
        else
            
            !--- Carbon balance of growth
            dsug_it_phy_growth      = phprof(phy,49)
            dstr_it_phy_growth      = phprof(phy,48)
            ddw_it_phy_growth       = phprof(phy,47)
            
            !--- Current State
            dw_it_phy               = phprof(phy,50)
            str_it_phy              = phprof(phy,51)  
            sug_it_phy              = phprof(phy,52)
            
            !--- Integrate Total Dry Weight and Sugars [g]                  
            dw_it_phy   = dw_it_phy  + ddw_it_phy_growth    ! Internode Dry Weight        [g]
            str_it_phy  = str_it_phy + dstr_it_phy_growth   ! Internode Structural Weight [g]
            sug_it_phy  = sug_it_phy + dsug_it_phy_growth   ! Internode Sugars Weight     [g]
            
            !--- Update internode profile
            phprof(phy,50)  = dw_it_phy  ! Internode Dry Weight        [g]
            phprof(phy,51)  = str_it_phy ! Internode Structural Weight [g]
            phprof(phy,52)  = sug_it_phy ! Internode Sugars Weight     [g]
            
            !--- Sucrose and Hexose Contents at above ground internodes
            call sucrose_content(   dw_it_phy,          &   ! Input Variable
                                    sug_it_phy,         &   ! Input Variable
                                    suc_min,            &   ! Input Parameter
                                    hex_min,            &   ! Input Parameter
                                    suc_acc_ini,        &   ! Input Parameter
                                    suc_frac_rate_ts,   &   ! Input Parameter
                                    suc_it_phy,         &   ! Output Variable
                                    hex_it_phy)             ! Output Variable
            
            !--- Above Ground
            suc_it_AG_ref_till  = suc_it_AG_ref_till    +   suc_it_phy
            hex_it_AG_ref_till  = hex_it_AG_ref_till    +   hex_it_phy                  
            
        endif
        
        !--- Store in phytomer profile
        phprof(phy,53)  = suc_it_phy ! Internode sucrose weight [g]
        phprof(phy,54)  = hex_it_phy ! Internode hexoses weight [g]
        
        !--- Fractions of Fiber/Sugars/Sucrose/Hexose
        if(phprof(phy,50) .gt. 0.d0)then
            phprof(phy,17)  = phprof(phy,51) / phprof(phy,50)
            phprof(phy,18)  = phprof(phy,52) / phprof(phy,50)
            phprof(phy,19)  = phprof(phy,53) / phprof(phy,50)
            phprof(phy,20)  = phprof(phy,54) / phprof(phy,50)
        endif
        
    enddo
      
    !--- Field Level Biomass Integration [ton ha-1]
    dw_rt       =   dw_rt       +   ddw_rt      -   ddw_rt_dead
    dw_lf       =   dw_lf       +   ddw_lf      -   ddw_lf_dead     -   ddw_lf_shed     +   ddw_lf_appear
    dw_it       =   dw_it       +   ddw_it      -   ddw_it_dead     +   dsubsres
    dw_it_AG    =   dw_it_AG    +   ddw_it_AG   -   ddw_it_AG_dead
    str_it_AG   =   str_it_AG   +   dstr_it_AG  -   dstr_it_AG_dead
    sug_it_AG   =   sug_it_AG   +   dsug_it_AG  -   dsug_it_AG_dead    
    dw_it_BG    =   dw_it_BG    +   ddw_it_BG   -   ddw_it_BG_dead  +   dsubsres
    str_it_BG   =   str_it_BG   +   dstr_it_BG  -   dstr_it_BG_dead
    sug_it_BG   =   sug_it_BG   +   dsug_it_BG  -   dsug_it_BG_dead +   dsubsres
    wat_it_AG   =   wat_it_AG   +   dwat_it_AG  -   dwat_it_AG_dead    
    
    !--- Dead parts
    dw_it_dead              =   dw_it_dead          +   ddw_it_dead
    dw_it_dead_AG           =   dw_it_dead_AG       +   ddw_it_AG_dead
    dw_it_dead_BG           =   dw_it_dead_BG       +   ddw_it_BG_dead
    dw_lf_dead              =   dw_lf_dead          +   ddw_lf_dead + ddw_lf_shed
    
    !--- Upscale Sucrose/Hexose to Field Level [ton ha-1]    
    suc_it_AG   =   suc_it_AG_ref_till * (nstk_now * tilleragefac) * (1.e4 / 1.e6)
    hex_it_AG   =   hex_it_AG_ref_till * (nstk_now * tilleragefac) * (1.e4 / 1.e6)
    suc_it_BG   =   suc_it_BG_ref_till * (nstk_now * tilleragefac) * (1.e4 / 1.e6) 
    hex_it_BG   =   hex_it_BG_ref_till * (nstk_now * tilleragefac) * (1.e4 / 1.e6)
        
    !--- Correction factor to meet carbon balance
    dsug_corr_fac_BG   = 1.d0
    dsug_corr_fac_AG   = 1.d0    
    if((suc_it_BG + hex_it_BG) .gt. 0.00001) dsug_corr_fac_BG   = sug_it_BG / (suc_it_BG + hex_it_BG)
    if((suc_it_AG + hex_it_AG) .gt. 0.00001) dsug_corr_fac_AG   = sug_it_AG / (suc_it_AG + hex_it_AG)     
    
    !--- Check carbon balance discrepancy
    c_check_tol = 0.10d0
    if(abs(1.d0 - dsug_corr_fac_BG) .gt. c_check_tol)then
!        write(warn,*) 'More than 10% Carbon Balance Discrepancy on Internode Below Ground Sugar/Structural Partitioning at DAS: ',das, &
!            'Discrepancy of ', abs(1.d0 - dsug_corr_fac_BG) * 100.d0, '% was corrected.'
    endif
        
    c_check_tol = 0.10d0
    if(abs(1.d0 - dsug_corr_fac_AG) .gt. c_check_tol)then
!        write(warn,*) 'More than 10% Carbon Balance Discrepancy on Internode Above Ground Sugar/Structural Partitioning at DAS: ',das, &
!            'Discrepancy of ', abs(1.d0 - dsug_corr_fac_AG) * 100.d0, '% was corrected.'
    endif        
    
    suc_it_AG   =   suc_it_AG	*	dsug_corr_fac_AG
    hex_it_AG   =   hex_it_AG	*	dsug_corr_fac_AG
    suc_it_BG   =   suc_it_BG	*	dsug_corr_fac_BG 
    hex_it_BG   =   hex_it_BG	*	dsug_corr_fac_BG    
    
    !--- Total Dry Biomass [ton ha-1]
    dw_total    =   dw_it       +   dw_lf   +   dw_rt
          
    !--- Stalk Fresh Mass [ton ha-1]
    fw_it_AG    =   dw_it_AG    +   max(0.d0, wat_it_AG) ! Avoid Negative Mass that can happen depending on parameters setup (dswat_dsuc & dswat_dstr)
    
    !--- Overall sugar content in dry mass basis
    if(dw_it_AG .gt. 0.d0) sug_cont    = sug_it_AG / dw_it_AG
    
    !--- Sucrose content on Fresh Stalk basis (POL%)
    if(sug_cont .gt. suc_acc_ini) then    
        pol          = suc_it_AG / fw_it_AG * 100.d0
        wat_con      = 1.d0 - (dw_it_AG / fw_it_AG)
    else
        pol      = 0.d0 ! To Avoid high pol values at early growth
        wat_con  = 0.85
    endif    
    
    !--- Aerial Dry Biomass [ton ha-1]
    if(flemerged)then        
        !--- After emergence
        dw_aerial   =   dw_it_AG    +   dw_lf        
    else        
        !--- Leaf Dry Weight is at below ground (Shoot)
        dw_aerial   =   dw_it_AG        
    endif    
        
    !--- Plant Population [tillers m-2]
    nstk    =   nstk    +   dnstk
    
    !--- Absolute number of tillers
    atln = aint(max(0.d0, nstk))        
    if(.not. (atln .eq. nstk))then
        !--- Increase one tiller to account for decimals
        atln    =   atln + 1
    endif
    
    if(flemerged) nstk_now = nstk ! nstk_now consider the below ground tillers while crop is not emrged
    if(flemerged) atln_now = atln ! atln_now consider the below ground tillers while crop is not emrged
    
    !--- Relative Age Difference Among Tillers [0-1]
    do tl = 1, atln            
        tillerageprof(tl,1) = tillerageprof(tl,1) + disoil
        tillerageprof(tl,2) = (tillerageprof(tl,1) / tillerageprof(1,1)) ** tilleragefac_adjust
    enddo
      
    !--- Update Tiller Age Factor
    !--- This factor is necessary to avoid overpredictions when upscaling all tillers to field level
    !--- This factor could be avoided if all tillers were simulated to achieve Field-Plant-Organ Scales (next version). 
    if(atln .lt. ini_nstk) then        
        !--- Only primary tillers
        tilleragefac = 1.d0
    else
        !--- Secondary/tertiary/four... emerged!
        tilleragefac = max(0.01, sum(tillerageprof(1:atln,2)) / atln) ! Do not let tilleragefac <= Zero (avoid zero division)
    endif
    
    !--- Reallocate below ground sugars among new tillers  
    if(abs(dnstk) .gt. 0.d0)then
        !--- Shared sugars [g] below ground sugars among below ground internodes            
        shared_it_sug_BG =  (sug_it_BG * (1.e6/1.e4) / (nstk_now * tilleragefac)) / n_it_BG                     
        
        !--- First Below Ground Internode position
        pos_it_BG        = n_ph - n_it_BG + 1        
        
        !--- Carbon balance including the new internode
        do phy = pos_it_BG, n_ph                
            !--- Carbon balance of reserves
            ts_it_phy       =   shared_it_sug_BG
            dw_it_phy       =   phprof(phy,51)  +   ts_it_phy
            
            !--- Update below ground internodes DW and Sugars
            phprof(phy,50)  =   dw_it_phy                           ! All weight are sugars at initial step
            phprof(phy,52)  =   ts_it_phy                           ! Total sugars
            phprof(phy,53)  =   shared_it_sug_BG * frac_suc_BG    ! 50% share sucrose/hexose
            phprof(phy,54)  =   shared_it_sug_BG * frac_hex_BG    ! 50% share sucrose/hexose                
        enddo        
    endif
    
    !--- Stalk Height [m]
    stk_h   =   stk_h   +   per / 1.e3
        
    !--- Stalk diameter 
    !--- Based on stalk population (Fabio Marin)       
    if(nstk_now .lt. 9.d0) then
        diam_stk = -.077 * nstk_now + 3.0443
    else
        diam_stk = -.0256 * nstk_now**2. + .4206 *  nstk_now + .7763
    endif
    
    !--- Leaf Area Index [m2 m-2]
    lai         =   lai     +   dlai_gain   -   dlai_dead   -   dlai_shed   +      dlai_gain_appear
    
    !--- Light intercepted for photosynthesis (only green leaves)
    acc_par     =   acc_par  +  dacc_par
    
    !--- Calculated RUE
    if((acc_par .gt. z) .and. (ddw .gt. z) .and. (dacc_par .gt. z))then
        drue_calc   = (ddw        * 1.e6/1.e4) / dacc_par ! RUE based on daily biomass gain and PAR intercepted [gDW/MJ]
        rue_calc    = (dw_total   * 1.e6/1.e4) / acc_par  ! RUE based on accumulated biomass and accumulated PAR intercepted [gDW/MJ] (the "typical" way)
    endif
    !--- Crop Coefficient (EORATIO)
    kc          = kc_min    + (eoratio - kc_min) * lai / (maxlai_eo)
    
    !--- Average Number of green leaves per tiller
    n_lf_tiller = n_lf_alive_AG * tilleragefac
    
    !--- Root Depth [cm]
    rd = min(rd + drdepth, rdm)
    
    !--- Effective Root Depth [cm]
    effective_rd = min(effective_rd + drdepth, rdm)
    if(.not. flemerged) then
        !--- Upward root growth
        rpup    = rpup + min(drdepth, dshootext_BG)
        rpup    = min(initcropdepth, rpup)
    endif
    
    !--- Root Length Density [cm.root cm-3.soil]
    rld      = rld  +   drld    -   drld_dead ! Arrays algebra    
        
    !--- Phytomer appearance stimulus [0-1]
    phy_stimuli     = phy_stimuli + dphy_stimuli
    
    !--- Crop Age [Cdays]
    diac        =   diac        +   di
    diacsoil    =   diacsoil    +   disoil
    
    !--- Crop Age After Emergence [Cdays]
    if(flemerged) then
        diacem        = diacem        + di
        diacsoilem    = diacsoilem    + disoil
        
        !--- Growth Stages After Emergence
        if(fl_tiller_increase)then
            
            !--- Increase number of tiller
            gstd    = 2.d0
            
            !--- Still tillering but stalk already emerged
            if(fl_stalk_emerged) gstd    = 3.d0
            
        elseif(fl_tiller_decrease)then
            
            !--- Decrease number of tiller        
            gstd    = 4.d0
            
        else
            !--- Stabilize plant population
            gstd    = 4.d0
        endif       
        
    endif    
    
    !--- Memorize how much substrates is needed for emergence
    if(.not. flemerged)then        
        !--- Learning how much substrates is needed
        res_used_emerg  =   res_used_emerg + supply_used_crop * (1.e4/1.e6)
    endif
    
    !--------------------!
    !--- Flags Update ---!
    !--------------------!
    
    if(cr_source_sink_ratio .lt. cr_source_sink_ratio_ruse .and. flemerged)then
        
        !--- Use reserves only for maintenance
        fl_use_reserves     = .false.
        
    else        
        if(sug_it_BG .gt. (res_used_emerg * res_used_emerg_fac))then
            
            !--- Use reserves to growth and maintenance
            fl_use_reserves     = .true.            
        else
            !--- Do not use reserves to growth if its not enough to the crop the emerge again (crop memory)
            fl_use_reserves     = .false.        
        endif
    endif
    
    !--- Check phytomer appearence stimulus 
    if(phy_stimuli .ge. 1.d0) then
        
        !-------------------------------!
        !--- Initiate a new phytomer ---!
        !-------------------------------!
        
        !--- Update Counters (leaf + internode + phytomers)
        n_ph                = n_ph              + 1
        n_it                = n_it              + 1
        n_lf                = n_lf              + 1
        n_lf_alive          = n_lf_alive        + 1
        n_lf_alive_juveni   = n_lf_alive_juveni + 1
                
        if(flemerged)then
            
            !--- Crop emerged
            n_ph_AG                 = n_ph_AG               + 1 ! Note: Here we consider that when leaf reach the surface the phytomer is above ground too. Depite the fact that most of its mass is below ground yet.        
            n_lf_AG                 = n_lf_AG               + 1
            n_lf_alive_AG           = n_lf_alive_AG         + 1
            n_lf_alive_juveni_AG    = n_lf_alive_juveni_AG  + 1
            
            if(fl_stalk_emerged)then            
                !--- Stalks Emerged
                n_it_AG =   n_it_AG + 1                
            else
                !--- Stalks are below the ground
                n_it_BG =   n_it_BG + 1                
            endif                            
        else
            
            !--- Crop is not emerged yet
            n_ph_BG                 = n_ph_BG               + 1
            n_it_BG                 = n_it_BG               + 1
            n_lf_BG                 = n_lf_BG               + 1
            n_lf_alive_BG           = n_lf_alive_BG         + 1
            n_lf_alive_juveni_BG    = n_lf_alive_juveni_BG  + 1                    
        endif                     
        
        !--- Update phytomer array
        do phy = n_ph, 1, -1
                
            !--- Update profile                
            phprof(phy+1,1)   = phprof(phy,1)  ! Leaf Age
            phprof(phy+1,2)   = phprof(phy,2)  ! Leaf Sink strenght
            phprof(phy+1,3)   = phprof(phy,3)  ! Allocated Leaf biomass
            phprof(phy+1,4)   = phprof(phy,4)  ! Leaf area rate
            phprof(phy+1,5)   = phprof(phy,5)  ! Leaf area
            phprof(phy+1,6)   = phprof(phy,6)  ! Leaf weight
            phprof(phy+1,7)   = phprof(phy,7)  ! Internode Sink Strength dw rate g d-1
            phprof(phy+1,8)   = phprof(phy,8)  ! Initial Leaf Area [cm2]
            phprof(phy+1,9)   = phprof(phy,9)  ! Initial Leaf DW [g]
            phprof(phy+1,10)  = phprof(phy,10) ! Plant population at appearance
            phprof(phy+1,11)  = phprof(phy,11) ! Kmr Internode, 
            phprof(phy+1,12)  = phprof(phy,12) ! Total phytomer Age
            phprof(phy+1,13)  = phprof(phy,13) ! Q10 Internode
            phprof(phy+1,14)  = phprof(phy,14) ! Internode Biomass at end of growth [g]
            phprof(phy+1,15)  = phprof(phy,15) ! Fiber Partitioning factor [0-1]
            phprof(phy+1,16)  = phprof(phy,16) ! Internode Length
            phprof(phy+1,17)  = phprof(phy,17) ! Fraction of Total Sugars
            phprof(phy+1,18)  = phprof(phy,18) ! Fraction of Fiber
            phprof(phy+1,19)  = phprof(phy,19) ! Fraction of Sucrose
            phprof(phy+1,20)  = phprof(phy,20) ! Fraction of Hexose
            phprof(phy+1,21)  = phprof(phy,21) ! Internode Growth Respiration
            phprof(phy+1,22)  = phprof(phy,22) ! Maintenance Respiration Factor (0-1) 1 =  is maintenance is ok
            phprof(phy+1,23)  = phprof(phy,23) ! dLength (cm)
            phprof(phy+1,24)  = phprof(phy,24) ! Lignin
            phprof(phy+1,25)  = phprof(phy,25) ! mresp leaf
            phprof(phy+1,26)  = phprof(phy,26) ! gresp leaf
            phprof(phy+1,27)  = phprof(phy,27) ! dw ss leaf
            phprof(phy+1,28)  = phprof(phy,28) ! sup_ratio_lf_phy
            phprof(phy+1,29)  = phprof(phy,29) ! supply_rate_lf
            phprof(phy+1,30)  = phprof(phy,30) ! supply_used_lf_phy
            phprof(phy+1,31)  = phprof(phy,31) ! supply_used_mresp_lf
            phprof(phy+1,32)  = phprof(phy,32) ! supply_used_gresp_lf
            phprof(phy+1,33)  = phprof(phy,33) ! supply_used_dw_lf
            phprof(phy+1,34)  = phprof(phy,34) ! maintenance_factor_lf
            phprof(phy+1,35)  = phprof(phy,35) ! reduc_growth_factor_lf
            phprof(phy+1,36)  = phprof(phy,36) ! mresp internode
            phprof(phy+1,37)  = phprof(phy,37) ! gresp internode
            phprof(phy+1,38)  = phprof(phy,38) ! dw ss internode
            phprof(phy+1,39)  = phprof(phy,39) ! sup_ratio_it_phy
            phprof(phy+1,40)  = phprof(phy,40) ! supply_rate_it_phy
            phprof(phy+1,41)  = phprof(phy,41) ! supply_used_it_phy
            phprof(phy+1,42)  = phprof(phy,42) ! supply_used_mresp_it_phy
            phprof(phy+1,43)  = phprof(phy,43) ! supply_used_gresp_it_phy
            phprof(phy+1,44)  = phprof(phy,44) ! supply_used_dw_it_phy
            phprof(phy+1,45)  = phprof(phy,45) ! maintenance_factor_it_phy
            phprof(phy+1,46)  = phprof(phy,46) ! reduc_growth_factor_it_phy
            phprof(phy+1,47)  = phprof(phy,47) ! Internode dry weigth rate [g dt-1]
            phprof(phy+1,48)  = phprof(phy,48) ! Internode structural dry weigth rate [g dt-1]
            phprof(phy+1,49)  = phprof(phy,49) ! Internode total sugars rate [g dt-1]
            phprof(phy+1,50)  = phprof(phy,50) ! Internode total dry weigth [g]
            phprof(phy+1,51)  = phprof(phy,51) ! Internode structural dry weigth [g]
            phprof(phy+1,52)  = phprof(phy,52) ! Internode total sugars [g]
            phprof(phy+1,53)  = phprof(phy,53) ! Internode sucrose weight [g]
            phprof(phy+1,54)  = phprof(phy,54) ! Internode hexoses weight [g]
            phprof(phy+1,55)  = phprof(phy,55) ! Leaf Age rate [dCdays]
            phprof(phy+1,56)  = phprof(phy,56) ! Phytomer Age rate [dCdays]
            phprof(phy+1,57)  = phprof(phy,57) ! Internode Age rate [dCdays]
            phprof(phy+1,58)  = phprof(phy,58) ! Internode Age [Cdays]
            
            !--- Flags Arrays
            fl_lf_AG(phy+1)   = fl_lf_AG(phy)
            fl_lf_alive(phy+1)= fl_lf_alive(phy)
            fl_it_AG(phy+1)   = fl_it_AG(phy)                
        enddo
        
        !--- Reset phytomer stimuli
        phy_stimuli    = phy_stimuli  - 1.d0                
        
        !-----------------------------------!
        !--- New phytomer initialization ---!
        !-----------------------------------!
        phprof(1, 1:60) = 0.d0
        
        !--- Leaf is alive
        fl_lf_alive(1)  = .true.
        
        !--- Initial leaf area depending on leaf number appearance to deal with leaf sheath size at different stages
        ini_la  = min(1.d0, max(0.d0, (n_lf_AG - 1) / (n_lf_max_ini_la))) * (max_ini_la - init_leaf_area) + init_leaf_area
        
        !--- Leaf initial area and dry Weight [cm2 and g]        
        if(flemerged) phprof(1,5)    = ini_la           ! [cm2]
        phprof(1,6)                  = ini_la / sla     ! [g]
        
        !--- Store initial State for SS
        if(flemerged) phprof(1,8)    = ini_la           ! [cm2]         
        phprof(1,9)                  = ini_la / sla     ! [g]
        phprof(1,10)                 = nstk_now         ! [tiller m-2] For upscaling
            
        !--- Initial age of the new phytomer [cDays]
        phprof(1,1)  =   phy_stimuli * phyllochron
        phprof(1,12) =   phy_stimuli * plastochron
        phprof(1,58) =   0.d0   ! Internode will have age zero until reach "Natural Break Point"
        
        
        if(flemerged)then
            
            !--- Crop Emerged
            fl_lf_AG(1) = .true.
            
            if(fl_stalk_emerged)then
                !--- Stalks emerged
                fl_it_AG(1) = .true.                
            else
                !--- Stalks below ground
                fl_it_AG(1) = .false.
            endif                        
        else
            !--- Crop not emerged
            fl_lf_AG(1) = .false.
            fl_it_AG(1) = .false.
        endif
            
        if(.not. fl_it_AG(1))then
            
            !--- New Below Ground Internode            
            !--- Shared sugars [g] below ground sugars among below ground internodes            
            shared_it_sug_BG =  (sug_it_BG * (1.e6/1.e4) / (nstk_now * tilleragefac)) / n_it_BG
                        
            !--- Initialize below ground internode 
            phprof(1,50) = shared_it_sug_BG                 ! All weight are sugars at initial step
            phprof(1,51) = 0.d0                             ! No structural
            phprof(1,52) = shared_it_sug_BG                 ! Total sugars
            phprof(1,53) = shared_it_sug_BG * frac_suc_BG   ! 50% share sucrose/hexose
            phprof(1,54) = shared_it_sug_BG * frac_hex_BG   ! 50% share sucrose/hexose
            phprof(1,14) = max_it_dw_BG   
            phprof(1,11) = kmr_stor   
            phprof(1,13) = q10_stor
            
            !--- Carbon balance including the new internode
            do phy = 2, n_it_BG    
                
                !--- Carbon balance of reserves
                ts_it_phy       =   shared_it_sug_BG
                dw_it_phy       =   phprof(phy,51)  +   ts_it_phy
            
                !--- Update below ground internodes DW and Sugars
                phprof(phy,50)  =   dw_it_phy                       ! All weight are sugars at initial step
                phprof(phy,52)  =   ts_it_phy                       ! Total sugars
                phprof(phy,53)  =   shared_it_sug_BG * frac_suc_BG  ! 50% share sucrose/hexose
                phprof(phy,54)  =   shared_it_sug_BG * frac_hex_BG  ! 50% share sucrose/hexose                
            enddo
        else
            phprof(1,14) = max_it_dw
            phprof(1,11) = kmr_stem   
            phprof(1,13) = q10_stem
        endif        
            
        !--- Appear a new young leaf
        fl_appear_leaf    = .true.
        
        !--- Leaf Maturity Counters
        dn_lf_alive_dewlap = 0
        
        !--- Amount of juvenile leaves
        if(n_lf_alive_juveni_AG .gt. (maxgl - maxdgl))then            
            n_lf_alive_juveni_AG   = (maxgl - maxdgl)
            dn_lf_alive_dewlap  = 1
        endif
        
        !--- Number of Leaves with formed dewlap 
        n_lf_alive_dewlap  = n_lf_alive_dewlap + dn_lf_alive_dewlap
        n_lf_AG_dewlap     = n_lf_AG_dewlap    + dn_lf_alive_dewlap
        
        !--- Check leaf spam
        if(n_lf_alive_dewlap .gt. maxdgl)then
            
            !--- Shed the oldest leaf                
            fl_shed_leaf    = .true.
            
            !--- Update living leaf number
            n_lf_alive        = maxgl
            n_lf_alive_AG     = maxgl
            n_lf_alive_BG     = maxgl
            n_lf_alive_dewlap = maxdgl
            
        endif
        
        !--- Check if stalk emerged
        if(n_lf_AG .ge. n_lf_when_stk_emerg)then
            fl_stalk_emerged    = .true.        
            STGDOY(4)           = Control%YRDOY ! Update devstage
        endif
        
    endif
    
    !--- Before emergence
    if(.not. flemerged)then
        
        !--- Shoot Depth [cm]
        shootdepth  = shootdepth - dshootext_BG
        
        !--- Update Counter
        nphy_BGround = nphy_BGround + 1
        
        !--- Critical level of Substrates reserves
        !--- Not even maintenace respiration can be sustained under this condition
        if(sug_it_BG .le. 0.d0 .and. shootdepth .gt. 0.d0) then                
            !--- Kill the crop before emergence
            flcropalive = .false.
            cropstatus  = '  Dead'     
            cstat       = 0.d0
        endif
        
        if(shootdepth .le. 0.d0)then
            
            !------------------------!
            !--- Crop has emerged ---!
            !------------------------!
                flemerged = .true.
            !------------------------!
            
            !------------------------------------------!
            !--- Initialize above ground conditions ---!
            !------------------------------------------!
		    shootdepth          = 0.d0
            phprof(n_ph,5)      = init_leaf_area                            ! Inital leaf area (cm2)
            nstk                = ini_nstk                                  ! Initial Tillering
            cropdstage          = 'Emergd'                                  ! Update Stage ID     
            diac_at_emergence   = diacsoil                                  ! Cdays at Emergence
            lai                 = init_leaf_area * ini_nstk / 1.e4          ! [m2 m-2]
            gstd                = 1.d0                                      ! Growth Stage (DSSAT)
            STGDOY(10)          = Control%YRDOY                             ! Date of emergence
        endif        
    endif
            
    !--- Store this day for output
    CaneCrop % dap  = dap
    
    !--- Update next-step
    dap = dap + 1
    
    !--- Linking with DSSAT globals
    CANHT       =   stk_h   ! Canopy height (m)    
    KCAN        =   k       ! Canopy light extinction coefficient for daily PAR, for equidistant plant spacing, modified when in-row and between row spacing are not equal 
    KTRANS      =   k       ! Light extinction coefficient used for computation of plant transpiration 
    NSTRES      =   1.      ! No Nitrogen Stress at this version
    RLV         =   rld     ! Root length density for soil layer L (cm[root] / cm3[soil])
    RWUMX       =   rwumax  ! Maximum water uptake per unit root length, constrained by soil water (cm3[water] / cm [root])        
    XHLAI       =   lai     ! Healthy leaf area index (m2[leaf] / m2[ground])
    XLAI        =   laimod  ! Leaf area (one side) per unit of ground area (m2[leaf] / m2[ground]) [NOTE: we are following canegro and considering as]
    
    !--- Total LAI must exceed or be equal to healthy LAI
    !--- Following MJ:
    XLAI = MAX(XLAI, XHLAI)
    
    !--- Maximum LAI so far at this season
    maxlai = max(lai,maxlai)
    
    !HARVRES     ! Composite variable containing harvest residue amounts for total dry matter, lignin, and N amounts.  Structure of variable is defined in ModuleDefs.for. 
    !MDATE       ! Harvest maturity date (YYYYDDD)
    !PORMIN      ! Read in SCSAM047.SPE            
    !SENESCE     ! Composite variable containing data about daily senesced plant matter. Structure of variable is defined in ModuleDefs.for
    !STGDOY      ! Day when plant stage I occurred (YYYYDDD)
    !UNH4        ! Not in use 
    !UNO3        ! Not in use
    !EORATIO     ! Read in SCSAM047.SPE    
    
    return
    
50  continue
        
    !--------------------------!
    !--- Write Output files ---!    
    !---  DYNAMIC = OUTPUT  ---!
    !--------------------------!
    
    !--- Passing variables to composite variables of SAMUCA
    CaneCrop % seqnow         		= seqnow        
    CaneCrop % pltype         		= pltype        
    CaneCrop % year           		= year          
    CaneCrop % doy            		= doy           
    CaneCrop % das            		= das           
    CaneCrop % diac           		= diac   
    CaneCrop % diacem               = diacem
    CaneCrop % dw_total       		= dw_total
    CaneCrop % dw_aerial      		= dw_aerial
    CaneCrop % dw_BG          		= max(0.d0, dw_total - dw_aerial)
    CaneCrop % dw_it_AG       		= dw_it_AG      
    CaneCrop % dw_lf          		= dw_lf         
    CaneCrop % dw_rt          		= dw_rt         
    CaneCrop % fw_it_AG       		= fw_it_AG      
    CaneCrop % suc_it_AG      		= suc_it_AG     
    CaneCrop % pol            		= pol           
    CaneCrop % lai            		= lai   
    CaneCrop % lai_ass              = lai_ass   
    CaneCrop % nstk           		= nstk          
    CaneCrop % stk_h          		= stk_h         
    CaneCrop % n_lf_AG_dewlap 		= n_lf
    CaneCrop % swface         		= swface        
    CaneCrop % swfacp         		= swfacp        
    CaneCrop % tempfac_pho          = tempfac_pho
    CaneCrop % tempfac_per          = tempfac_per
    CaneCrop % co2                  = co2
    CaneCrop % agefactor_amax       = agefactor_amax
    CaneCrop % agefactor_per        = agefactor_per
    CaneCrop % sug_it_BG            = sug_it_BG
    CaneCrop % amaxfbfac            = amaxfbfac
    CaneCrop % per                  = per
    CaneCrop % cropstatus     		= cropstatus    
    CaneCrop % cropdstage     		= cropdstage  
    CaneCrop % rd             		= rd
    CaneCrop % rld            		= rld
    CaneCrop % frac_li_pho    		= frac_li
    CaneCrop % frac_li_till   		= li
    CaneCrop % trwup          		= trwup * 10.d0 ! [mm]
    CaneCrop % eop            		= eop    
    CaneCrop % dtg            		= dtg
    CaneCrop % drue_calc      		= drue_calc 
    CaneCrop % rue_calc       		= rue_calc
    CaneCrop % tot_gresp_crop 		= tot_gresp_crop * (1.e4/1.e6) ! [ton ha-1]
    CaneCrop % tot_mresp_crop 		= tot_mresp_crop * (1.e4/1.e6) ! [ton ha-1]
    CaneCrop % gstd           		= gstd
    CaneCrop % nratoon        		= nratoon
    CaneCrop % cstat          		= cstat
    CaneCrop % dw_it_dead           = dw_it_dead
    CaneCrop % dw_it_dead_AG        = dw_it_dead_AG
    CaneCrop % dw_it_dead_BG        = dw_it_dead_BG
	CaneCrop % dw_lf_dead           = dw_lf_dead
    CaneCrop % tmn                  = tmn    
    CaneCrop % flemerged            = flemerged
    
    !--- Water demand
    if(eop .le. z)then
        CaneCrop % watdmd   = 1
    else
        CaneCrop % watdmd   = max((CaneCrop % trwup)/(CaneCrop % eop),0.d0)
    endif
    
    !--- Phytomer level
    CaneCrop % n_ph                 = n_ph
    CaneCrop % phprof               = phprof
    CaneCrop % tillerageprof        = tillerageprof
    CaneCrop % fl_it_AG             = fl_it_AG
    CaneCrop % fl_lf_AG             = fl_lf_AG
    CaneCrop % fl_lf_alive          = fl_lf_alive
    
    !--- Convert to micromol m-2 s-1 for output purpose
    CaneCrop % amax_out             = amax_mod * 1.e3 / 1.e4 / 3600.d0 / 44.d0 * 1.e6
    CaneCrop % eff_out              = eff_mod  * 1.e3 / 1.e4 / 3600.d0 / 44.d0 * 1.e6 / 4.6
        
    !--- Layered Canopy outputs
    CaneCrop % Acanopy              = Acanopy
    CaneCrop % Qleaf                = Qleaf
    CaneCrop % incpar               = incpar
    
    !--- Write output
    call sc_opgrow_sam( CONTROL,    &
                        CaneCrop,   &                        
                        YRPLT)
    
    !------------------------!
    !--- Detailed Outputs ---!
    !------------------------!    
    if(writedcrop) call SC_OPGROW_SAM_DETAILED (CONTROL, CaneCrop, YRPLT)    
    
    !--- Update STG in case of last day
    if(CONTROL%YRDOY == YREND)then
        STGDOY(16)  = YREND
        STGDOY(15)  = YREND
        MDATE       = YREND
    endif
    
    !--- Summary file outputs
    call SC_OPHARV_SAM(CONTROL, ISWITCH,        &
        CaneCrop, flemerged, maxlai,            &
        swfacp, swface, STGDOY, XLAI, YRPLT)
            
    return
    
60  continue
    
    !--- Summary file outputs
    call SC_OPHARV_SAM(CONTROL, ISWITCH,        &
        CaneCrop, flemerged, maxlai,            &
        swfacp, swface, STGDOY, XLAI, YRPLT)
        
    !--- Update cultivar/species file flags
    call get_cultivar_coeff(maxgl_r,'dummy', CONTROL, CF_ERR)
    call get_species_coeff(tb,      'dummy', CONTROL, SPC_ERROR)

    !--- Close outputs
    call sc_opgrow_sam( CONTROL,    &
                        CaneCrop,   &                        
                        YRPLT)
    
    return
    
    end subroutine SAMUCA
    
