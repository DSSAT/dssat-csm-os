!======================================================================
!  TF_GROSUB, Subroutine
!
!  WHAPS wheat growth subroutine
!----------------------------------------------------------------------
!  Revision history
!  06/27/2011 FSR created WH_GROSUB.for for APSIM NWheat (WHAPS) adaptation
!  01/11/2018 KEP converted WH_ sub-routines to TF_.
!  01/31/2018 KEP changed ce_tops = 3.8 * (SRAD**0.63 / SRAD) to ce_tops = 4.5 * (SRAD**0.63 / SRAD)
!  02/11/2018 KEP changed INGWTgrams = INGWT*0.001 to INGWTgrams = INGWT/100*0.001
!  02/24/2018 KEP changed GRNO to GRNO=GRNO*100
!  02/26/2018 KEP changed GRNO=GRNO*100 back to GRNO
!  04/05/2018 KEP changed transp_eff_coeff from transp_eff_coeff = 0.006 * ( 1+ (0.37/350)*(WEATHER % CO2 -350))
!                 to transp_eff_coeff = 0.009 * ( 1+ (0.28/350)*(WEATHER % CO2 -350))
!  04/05/2018 KEP changed transp_eff_coeff from transp_eff_coeff=0.006 to transp_eff_coeff=0.009
!  09/14/2018 KEP increased the base growth temperature from -4 C to 7.8 C for calculating prft.
!  01/21/2020 JG moved some CUL parameters to ECO file
!  07/24/2020 JG moved ozone parameters to ECO file
!  11/01/2021 FO Added missing CONTROL type for WH_temp.for subroutines
!  01/18/2022 TF Added statements to prevent divisions by zero
!----------------------------------------------------------------------
!  Called by : TF_APSIM
!
!  Calls  : NFACTO NUPTAK
!----------------------------------------------------------------------
C The statements begining with !*! are refer to APSIM source codes
! JZW rlv_nw is output of WH_Grosub and ourput of WH_APSIM, input of nwheats_rtlv
! How to get SNO3(NL) initial? from soil property? I add XStage to the argument of WH_GROSUB
! cnc is critical N  concentration? you need to call nwheats_set_nconc (cnc, mnc)
! duldep and lldep should beshold be DUL * DLAYR * 10.
! arguments duldep, lldep in nwheats_rtlv should be removed
! Need to output the ptnup and ctnup

      SUBROUTINE TF_GROSUB (CONTROL, ISWITCH, WEATHER, XSTAGE,
     &      ASMDOT, DLAYR, DS, DTT, EO, EOP, ES, FILEIO,      !Input
     &      FracRts, ISTAGE, zstage,                          !Input
     &      KG2PPM, LL, NLAYR, nh4ppm, no3ppm,                !Input
     &      nwheats_kvalue,                                   !Input
     &      pgdd, PLTPOP, PPLTD, rlv_nw, rtdep_nw,            !Input
     &      SAT, SDEPTH, SeedFrac, SHF, SLPF, SOILPROP,       !Input
     &      SPi_AVAIL, SRAD, stage_gpla, STGDOY, stgdur,      !Input
     &      sumstgdtt, SW, SWIDOT, TLNO, TMAX, TMIN,          !Input
     &      TRWUP, VegFrac, WLIDOT,                           !Input
     &      WRIDOT, WSIDOT, XNTI, xstag_nw,                   !Input
     &      YRDOY, YRPLT,                                     !Input
     &      EARS, GPP, MDATE,                                 !I/O
     &      AGEFAC, APTNUP, AREALF, CANHT, CANNAA, CANWAA,    !Output
     &      CANWH, CARBO, carbh, dlayr_nw, GNUP, GPSM, GRNWT, !Output
     &      GRORT, HI, HIP, LEAFNO, NSTRES,                   !Output
     &      nwheats_topsfr, PCNGRN, PCNL, PCNRT, PCNST,       !Output
     &      PCNVEG, PHINT, PODNO, PConc_Root, PConc_Seed,     !Output
     &      PConc_Shel, PConc_Shut, pl_la, plsc,              !Output
     &      PODWT, PORMIN, PSTRES1,                           !Output
     &      PSTRES2, PTF, PUptake, RLWR, ROOTN, RSTAGE, RTWT, !Output
     &      RTWTO, RWUMX, SATFAC, SDWT, SEEDNO,               !Output
     &      sen_la,  SENESCE, SHELPC, SI1,                    !Output
     &      SI3, SKERWT, SLA, STMWTO, STOVER,                 !Output
     &      STOVN, STOVWT, SUMP, SWFAC, tiln,                 !Output
     &      TOPWT, TURFAC, snup_nh4,                          !Output
     &      snup_no3, VSTAGE, WTLF, WTNCAN, pl_nit_leaf,      !Output
     &      pl_nit_grain, pl_nit_stem, cumph_nw,              !Output
     &      cumpnup, WTNVEG, XGNP, XHLAI, XLAI, XN, YIELD,    !Output
     &      KUptake, rwu_nw, swdef, nfact,                    !Output
     &      pl_nit_root, pl_nit_lfsheath, SLFT, GAD2)         !Output

! 2023-01-17 chp removed unused variables
!  CDAY, CO2, fstage, nwheats_dc_code, nwheats_vfac, P3, SUMDTT, 
!  TSEN, vd, vd1, vd2, SKi_Avail, KSTRES, RUE, DISLA

      USE ModuleDefs
      USE ModuleData
      USE TF_module
      USE Interface_SenLig_Ceres
      IMPLICIT  NONE
      EXTERNAL ADD_REAL_ARRAY, ALIN, COUNT_OF_REAL_VALS, ERROR, FIND, 
     &  GETLUN, GRN_PTL, HEADER, IGNORE, NWHEATS_ADD_SEN_ROOTS, 
     &  NWHEATS_CWDMD, NWHEATS_CWPOT, NWHEATS_GNDMD, NWHEATS_GRNIT, 
     &  NWHEATS_LEVEL, NWHEATS_NUPTK, NWHEATS_PLNIN, NWHEATS_RTDP, 
     &  NWHEATS_RTLV, NWHEATS_SET_ADF, ntefs_set_nconc, 
     &  ntefs_set_nfact, NWHEATS_WATUP_NEW, P_CERES, SET_SWDEF_NEW, 
     &  SUBTRACT_REAL_ARRAY, SUM_REAL_ARRAY, WARNING, TF_NFACTO
      SAVE
!----------------------------------------------------------------------
!                         Variable Declaration
!----------------------------------------------------------------------
      integer deepest_layer     ! deepest layer in which the roots are
                                ! growing
      real root_array(NL)    ! array to contain distributed material
      INTEGER NOUTXL, ERRNUM, RUN
      INTEGER nlayr_nw   ! Number of layers in specific soil profile
      CHARACTER*1 IDETR
      CHARACTER*1  MEEVP
      CHARACTER*5  temp_Char
      CHARACTER*12 OUTXL
      character*6 uptake_source ! add by JZW
      character*6 g_uptake_source ! add by JZW
      CHARACTER*78 MSG(2)
!     real       temp_c 
!     real     above_gnd_pcarb  ! potential increase in aboveground biom
      real uptake_water(NL)
      LOGICAL FEXIST, FIRST

!     FSR added coefficients from WHAPS SPECIES file
      real   dc_code(7) ! Growth stage code, similar to Zadocs scale
      real   istageno(7) ! istage input for interpolation of DC code
      real   p_adf(3)    ! aeration deficit (1 = no stress)
!     p_afs = crop sensitivity to aeration deficit, as a funct of phenol (1 = aeration deficit tolerant crop)
      real   p_afs(2)    
      real   p_fdsw(3)   ! fraction of drainable soil water in layer
      real   p_stage(2)  ! istage (growth stage): emerg.- grain fill
      real   rootfr(9)
      real   xs(6)  ! xstage input for interpolating Zadoks stage
      real   zs(6)  ! Zadoks growth stage number
      real  transp_eff_coeff, Tdepend  ! tec(10),rue_fac(10), XCO2(10),

!     FSR added coefficients from WHAPS CULTIVAR file
      REAL        ADLAI
      REAL        ADPHO
      REAL        ADTIL
      REAL        DTNP1
      REAL        EO, ep_nw, ES!JZW add for Canopy T calculation
      REAL        EXNH4
      REAL        EXNO3
      REAL        FOZ1  ! Added by JG for ozone calculation
      REAL        FREAR
      REAL        GPPES
      REAL        GPPSS
      REAL        GRNO
      REAL        INGNC
      REAL        INGWT
      real        INGWTgrams
      REAL        MNNCR
      REAL        MNNH4
      REAL        MNNO3
      REAL        MNRTN
      REAL        MXGWT
      REAL        MXNCR
      REAL        MXNUP
      REAL        NOMOB
      REAL        nwheats_kvalue
      REAL        P2AF
      REAL        P3AF
      REAL        P4AF
      REAL        P5
      REAL        P5AF
      REAL        P6AF
      REAL        PHINT
      REAL        PLGP1
      REAL        PLGP2
      REAL        PNUPR
      REAL        PPSEN
      REAL        MXFIL
      REAL        RTDP1
      REAL        RTDP2
      REAL        SFOZ1  ! Added by JG for ozone, formerly SLFOZ1
      REAL        SLA
      REAL        SLAP1
      REAL        SLAP2 ! nwheat cultivar parameter
      REAL        STEMN
      REAL        STMMX ! formerly STMWT
      REAL        TC1P1
      REAL        TC1P2
      REAL        VSEN
      REAL        WFNU
      REAL        WTDEP  ! Depth to water table (cm)

!*!   FSR added variables from nwheats.for
      real  rtsnr                 ! rate of senescense of root (0-1)
      parameter   (rtsnr = 0.01)
      REAL  gm2kg
      PARAMETER   (gm2kg = 0.001)   ! conversion factor from NWheat.
      REAL  sm2ha
      PARAMETER   (sm2ha = 0.0001)  ! conversion factor from NWheat.
      REAL  sm2smm
!*!   PARAMETER   (sm2smm = 10000) ! conversion factor from NWheat.
      PARAMETER   (sm2smm = 1000000) ! conversion factor from NWheat.
      REAL  smm2sm
!*!   PARAMETER   (smm2sm = 0.0001) ! conversion factor from NWheat.
      PARAMETER   (smm2sm = 0.000001) ! conversion factor from NWheat.
      real adf(NL)
      real afs
!     REAL af2_lai
      REAL af2_photo
!     real af2_tiller
      real aver_tsw
      real carbh    ! WHAPS ver of NWheat carb and carbo
      REAL ce       ! conversion efficiency under no stress(g biomas/mj)
      REAL ce_roots ! ce for roots (g biomass/MJ)
      REAL ce_tops  ! ce for tops  (g biomass/MJ)
      real Ctrans ! Carbohydrate avail for translocation
                  ! from seed reserves (g/plant)
      real cwdemand     ! crop water demand (mm) add by JZW
!     real gndmd                 ! (OUTPUT) grain N demand (g/plant) add by JZW
      Real tempmx, tempmn !add by JZW
      REAL zstage ! , xtag_nw !add by JZW
      REAL SNO3(NL), SNH4(NL) !add by JZW
      real cnc(mxpart) !it is pcnc, add by JZW
      real ptnup
      real cumpnup
      real snup_no3 (NL)   !actual plant NO3 uptake (kg/ha)
      real snup_nh4 (NL)   !actual plant NH4 uptake (kg/ha)
!     pnup = actual plant N uptake to ea plant part (g/plant part)
      real pnup (mxpart)   
      REAL cumph_nw(11) ! DSSSAT: "Cumulative phyllochron
                        ! intervals or fully expanded leaves"
      real dlayr_nw(NL) ! Soil thickness in layer L (mm) NWHEAT
!     real depth_nw  ! Depth below soil surface to the point of interest
!     real pesw_nw(NL)! available water in layer
      real potntl(NL)! Potential crop H2O uptake by soil layer (mm)
      real prwu (NL)! (nwheats_watup_new OUTPUT) root water uptake
                        !  (mm)
      real dtiln
      REAL DUL(NL)
      real duldep(NL)   ! drained upper limit by soil layer
      REAL fr_intc_radn
      REAL frlf ! New fraction of oldest expanding leaf
!     real fstage
      REAL g_obs_gpsm  ! Observed number of grains per plant
                              ! not yet implemented.  Set to zero.
      real g_water_table    ! water table depth (soil parameter; mm)
      REAL grain_nc_ratio ! N/C ratio in grain?
!     REAL grnrat        
      REAL grogrn   ! grain demand for carbohydrate (g/plant)
      REAL grolfsh  ! carbohydrate demand to grow new leaf sheath
      real grogrnmx ! max grain increase (g/plant)
      REAL gndmd_est   ! grain demand for N
      REAL gpp_stem_wt ! stem weight used in calc gpp (NWheats)
      REAL grdmd       ! grain demand for CH2O (g/plant)
      REAL grolf ! carbohydrate demand to grow new leaf (g/plant)
      REAL grpp        ! temporary variable used by NWheats
      REAL gro_wt(mxpart)  ! FSR: root, leaf, lfsheath, stem, grain
      REAL gtmaxfac, gtminfac
      Real GAD2  ! Grain# if there wwas no temperature effect
      real lai     ! from function nwheats_slan
      REAL leafgr  ! leaf growth before emergence (g/plant)
      real leafsen !leaf senescence variable from nwheats_lfsen function
      REAL lfipar !*! Radiation intercepted by leafs  (mj/m2)
      REAL lfshdmd ! carbohydrate demand to grow new leaf sheath
                          ! (g/plant)
      real LL(NL)
      real lldep(NL)
      real mass_balance  ! total translocation (g/plant)
      real temp ! temperary data
      REAL max_topsfr ! Maximum tops fraction?
      REAL mnc(mxpart)  ! Minimum N concentration, by plant part
!     REAL navl(mxpart) !N in each plant part available to grain?
      REAL navl_est(mxpart)  !*! est N available to grain
      REAL nfact(10) ! Nstress sensitivity by plant organ (0-1)
      real nfactor
      REAL nflow  !*! N mobilized to grain?
      REAL nitmn  ! nitrogen minimum level              (g/part)
      REAL npot   ! potential N available for transfer  (g/part)
!     real nwheats_dc_code
      REAL nwheats_gplag_emerg  ! growth in leaf area
                                ! during emerg to endjuv (mm^2)
      REAL nwheats_lfdmd ! CH2O needed for leaf growth (g/plant)
!     REAL nwheats_min_rootfr
      REAL nwheats_rtdmd 
      real nwheats_rtlos
      REAL nwheats_stdmd
      REAL nwheats_topsfr ! Senthold alternative to topsfr
!     real nwheats_vfac
      real optfr ! fraction of optimum conditions from stress (0-1)
      real part_shift     ! the shift in allocation from leaves to
      REAL plagtf !plant leaf area growth temperature function(?)
      real pot_growt_leaf ! a potential leaf growth before reallocation
!*!   real pcarb  ! same as pcarbo; only pcarbo used in WHAPS
      REAL pcarbo !Potential CH2O output (production) (gDM/plant)
      REAL pgdd(20)  ! TT required for each istage
      REAL pl_dmd(mxpart)  ! N demand by plant component nwheat
      REAL pl_la    ! Plant leaf area, mm2/plant         nwheat
      REAL pl_nit(mxpart) !plant part nitrogen (g/plant)  nwheat
      REAL plntp, pl_nit_leaf, pl_nit_grain
      REAL pl_nit_root, pl_nit_lfsheath, pl_nit_stem
!*!  REAL pl_wt(mxpart) !Replaced with "plantwt"        nwheat !JZW should remove
      REAL plag   ! plant leaf area growth for day   (mm^2/plant)
      REAL plagms
      REAL plantwt(mxpart)!plant part weights (g/plant)  nwheat
      real plsc(20)   ! accumulates leaf area in an array
                          ! for each phylochron interval
      REAL plwtmn(mxpart) !min wt of each plant part     nwheat
!     REAL plantwtmn(mxpart)!Replaced with "plwtmn"      nwheat
      real pntrans(mxpart) ! N translocated to grain.
      REAL ptcarb
      REAL ptfstem
      REAL ptgrn    ! potential grain growth (g/plant)
      REAL radfr    ! Fraction of radiation intercepted by leaves
      real rlvnew(NL)  ! potential new root-length-volume
                       ! (mm root/mm^3 soil)
      REAL rrgfill  !relative (0-1) rate of grainfill for the day
      REAL rootgr   ! root growth before emergence (g/plant)
      real rootnsen
      real rootsen  ! Senesced root
      real rootsenfr ! fraction of roots senescing(0-1)
      real rtdep_nw ! overall root depth in profile (mm) (?)
      real rtdnew   ! Increase in root depth (mm)
      real rtsw     ! not defined in NWheat code
      Integer TEFAC, RUEFAC !JZW add on Apr. 1, 2014
      ! Integer CNPYT !JZW add on May. 13, 2014
      real rue_factor  ! parameter for CO2 effect on pcarbo calculation
      real rwu_nw (NL)! (nwheats_watup_new OUTPUT) root water
                      !  uptake (mm)
      REAL seedrvNW ! seed carbohydrate reserves (g/plant)
      REAL sen_la   ! Senesced leaf area, mm2/plant      nwheat
      real senrt    ! Senesced root
      real sentil   ! Senesced tillers? (undocumented in NWheat)
      real sfactor  ! stress factor for leaf senescence(0-1)
      REAL sla_new  ! specific leaf area of new growth (mm2/g)
      real slan  ! leaf area senesced during normal development (mm^2 ?)
      real slfn     ! N effect on leaf senescence  (0-1)
      real slft     ! low temperature factor (0-1)
      real slfw     ! drought stress factor (0-1)
      real stage_gpla ! from function nwheats_slan
      REAL stemgr   ! stem growth before emergence (g/plant)
      real sum_real_array  ! Function in WH_SW_SUBS.for
      real sumcbo (mxpart)
      REAL sumstgdtt(20)
      REAL swdef(10)
      real swdep(NL) ! water content by soil layer (mm)
      real tc1
      real tc2
      REAL tf      ! Temperature function (?) nwheat
      real ti  ! Fraction of a phyllochron interval which occurred
               ! as a fraction of today's daily thermal time (DSSAT TI)
      REAL tiln    ! Number of tillers per plant (?)
      REAL TILSW   !DSSAT: Tiller standard weight (g/s)?
      REAL topsbiom
      REAL topsfr  ! Tops fraction
      REAL tot_navl_est
      real totprt  ! total of parts (g/plant)
      real tpsm
      real trans_wt (mxpart) ! (INPUT) actual change in plant part
                             ! weights due to translocation (g/plant)
!     REAL        vd
!     REAL        vd1
!     REAL        vd2
      real wfactor
      real WR(NL)    ! APSIM soil layer parameter (0-1?)
      real xstag_nw , XSTAGE!


      integer count_of_real_vals
!**!      integer NL
      integer nrlayr    ! no. of root layers
      integer dyingleaf ! from function nwheats_slan
      integer greenlfno ! from function nwheats_slan
      parameter (greenlfno = 4) !*! Included temporarily (FSR)
      integer  I
      INTEGER II !, TIMDIF
      integer nwheats_level ! Function from nwheats. Returns layer  
                            ! number of depth in profile dlayr_nw
      integer nwrlay        ! number of layers with root weighting
      INTEGER     part   ! plant part number
!     INTEGER     photo
      INTEGER     stgdur(20)
!     INTEGER     tiller
      PARAMETER   (leafgr = 0.006)  ! leaf growth before emergence
      PARAMETER   (rootgr = 0.2) ! root growth before emergence
      PARAMETER   (seedrvNW = 0.012) ! seed carbohydrate reserves
      PARAMETER   (stemgr = 0.4 - leafgr) ! stem growth before emergence

!*! End WHAPS coefficients

      REAL        AGEFAC
      REAL        APTNUP
      REAL        AREALF
      REAL        ASGDD
      REAL        ASMDOT
      REAL        BIOMAS
      REAL        BSGDD
      CHARACTER*80 C80
      REAL        CANHT
      REAL        CANHT_POT !added 26May04 RS
      REAL        CANNAA
      REAL        CANWAA
      REAL        CANWH
      REAL        CARBO
      REAL        CARBOT
      INTEGER     anth_date   !CDAY,  
      INTEGER     CMAT
!     REAL        CNSD1       
!     REAL        CNSD2        
!     REAL        CPSD1       
!     REAL        CPSD2        
      REAL        CO2X(10)
      REAL        CO2Y(10)
!     REAL        CO2         
!     REAL        CSD1        
!     REAL        CSD2        
      REAL        CUMDTTEG
!     REAL        CURV
      real        depmax   ! maximum depth to which roots can grow (mm)
      REAL        DLAYR(NL)
      REAL        DS(NL)

!     INTEGER     DOY         
      REAL        DTT
      REAL        DUMMY
      INTEGER     DYNAMIC
      REAL        EARS
      REAL        EARWT
      CHARACTER*6     ECONO
      INTEGER     EMAT
      REAL        EOP
      REAL        EP1
      INTEGER         ERR
      CHARACTER*6     ERRKEY
      PARAMETER       (ERRKEY='TF_GRO')
      CHARACTER*30    FILEIO
      CHARACTER*12    FILEC
      CHARACTER*92    FILECC
      CHARACTER*12    FILES
      CHARACTER*12    FILEE

      ! JG added for ecotype file
      CHARACTER*92    FILEGC
      CHARACTER*1     BLANK 
      PARAMETER (BLANK = ' ')
      CHARACTER*6     ECOTYP
      CHARACTER*355   C255  ! JG increased for large ecotype file
      CHARACTER*16    ECONAM
      INTEGER     PATHL
      INTEGER     LUNECO
      REAL        TBASE,TOPT,ROPT,TTOP, P2O,VREQ,GDDE,DSGFT,RUE1,RUE2
      REAL        KVAL1,KVAL2  ! JG added for ecotype file
      
      INTEGER         FOUND
      REAL        FSLFW
      REAL        FSLFN
!     REAL        G2    
!     REAL        G3          
      REAL        GNP
      REAL        GNUP
      REAL        GPP
      REAL        GPSM
      REAL        GRAINN
      REAL        GRF
      REAL        GRNWT
      REAL        GROEAR
      REAL        GRORT
      REAL        GROSTM
      REAL        HI
      REAL        HIP
      INTEGER     ICOLD
      INTEGER     ICSDUR
      CHARACTER   IDETO*1
      REAL        IPAR
      INTEGER     ISECT
      INTEGER     ISTAGE
      CHARACTER*1 ISWNIT
      CHARACTER*1 ISWWAT
      CHARACTER*1 ISWPOT
      REAL        KG2PPM(NL)
      INTEGER     L
      REAL        LAIDOT
      INTEGER     LEAFNO
      REAL        LEAFNOE
      REAL        CumLeafSenes    !today's cumul. leaf senescence
      REAL        CumLeafSenesY   !yesterday's cumul. leaf senescence
      REAL        CumLfNSenes     !cumul. N loss in senesced leaves
      INTEGER     LINC
      REAL        LFWT
      REAL        LFWTE
      REAL        LIFAC
      INTEGER     LNUM
      INTEGER     LUNCRP
      INTEGER     LUNIO
      INTEGER     MDATE
      REAL        MAXLAI
      REAL        NDEF3
      REAL        NFAC
      REAL        nh4ppm(NL) !REAL        NH4(NL)
      INTEGER     NLAYR
      REAL        no3ppm(NL) !     REAL        NO3(NL)

      INTEGER     NOUTDO
      REAL        NPOOL
      REAL        NPOOL1
      REAL        NPOOL2
      REAL        NSDR
      REAL        NSINK
      REAL        NSTRES
!     INTEGER     NWSD 
      real        p_min_pot_hi
      REAL        P1
!     REAL        P3      
!     REAL        PAR         
      REAL        PARSR
      CHARACTER*80    PATHCR
      CHARACTER*80    PATHSR
      CHARACTER*80    PATHER
!     REAL        PC          
!     REAL        PCARB
      REAL        PCNGRN
      REAL        PCNL
      REAL        PCNRT
      REAL        PCNSD
      REAL        PCNVEG
      REAL        PCNST
      REAL        PCO2
      REAL        PDWI
      ! Temperature effect of grain number
      REAL        GTMAX(4), GTMIN(4), GRDUH(4),  GRDUL(4)
!     DTT factor of Temperature effect of the Grain #
      REAL        DTTPT(3), DTTF(3) 
!     REAL        PRFTC(4)
      REAL        SENST(4),  SENSF(4), ALIN, TCSlope, TCInt ! TRLFG(4)
      REAL        SLPF
!     REAL        PGRORT      
      REAL        PLA
      REAL        PLAE
!*!   REAL        PLAG
      REAL        PLAS
      REAL        PLTPOP
      REAL        PODNO
      REAL        PODWT
      REAL        PORMIN
      REAL        PPLTD
      REAL        PRFT
      REAL        PTF
      REAL        RANC
      REAL        RANCE
      REAL        RCNP
!     REAL        RGFILL 
!     REAL        RGFIL(4) !not used    
      REAL        RLV(NL) !JZW should remove
      real        rlv_nw(NL)
      REAL        RLWR
      REAL        RMNC
      REAL        RNLAB
!     REAL        RNOUT
      REAL        ROOTN
      REAL        RSGR
      REAL        RSGRT
      INTEGER     RSTAGE
      REAL        RTDEP
      REAL        RTWT
      REAL        RTWTE
      REAL        RTWTO
!     REAL        RUE
      REAL        RWUEP1
      REAL        ROWSPC
      REAL        RWUMX
      REAL        SAT(NL)
      real        satdep(NL)
      REAL        SATFAC
      REAL        SDEPTH
      REAL        SDSZ
      REAL        SDWT
      CHARACTER*6 SECTION
      REAL        SEEDNO
      REAL        SEEDRV
      REAL        SEEDRVE
      REAL        SENLA   !, SENLAY
!     REAL        SFAC        
      REAL        SHF(NL)
      REAL        SHELPC
      REAL        SI1(6)
      REAL        SI2(6)
      REAL        SI3(6)
      REAL        SI4(6)
      REAL        SI5(6)
      REAL        SI6(6)
      REAL        SKERWT
      REAL        SLFC
      REAL        SRAD
      INTEGER     STGDOY(20)
!     REAL        Stg2CLS 
      REAL        STMWTO
      REAL        STMWT
      REAL        STMWTE
      REAL        STOVER
      REAL        STOVN
      REAL        STOVWT
!     REAL        SUMDTT      
      REAL        SUMEX
      REAL        SUMP
      REAL        SUMRL
!     FracRts  Fraction of soil volume in direct contact with roots
      REAL        FracRts(NL)   
      REAL        SW(NL)
      REAL        SWEXF
      REAL        SWFAC
      REAL        SWIDOT
      REAL        SWMAX
      REAL        SWMIN
!     REAL        TABEX       
      REAL        TANC
      REAL        TANCE
      REAL        TAVGD
      REAL        TCNP
      REAL        TEMPM
      REAL        TFAC
      REAL        TLNO
      REAL        TMAX, Tcnpy, VPD(4), FVPD(4), vpdf, vpd_transp
      REAL        TMNC
      REAL        TNLAB
      REAL        TMIN
      REAL        TOPWT
      REAL        TOTNUP
      REAL        TRNU
      REAL        TRWUP
!     REAL        TSEN
      REAL        TSS(NL)
      REAL        TURFAC
!     REAL        UNH4(NL)    
!     REAL        UNO3(NL)    
      REAL        VMNC
      REAL        VANC
      CHARACTER*6     VARNO
      CHARACTER*16    VRNAME
      REAL        VSTAGE
      REAL        WLIDOT
      REAL        WRIDOT
      REAL        WSIDOT
      REAL        WTLF
      REAL        WTNCAN
      REAL        WTNLF
!     REAL        WTNSD       
      REAL        WTNST
!     REAL        WTNUP       
      REAL        WTNVEG
      REAL        XANC
      REAL        XLAI
      REAL        XHLAI
      REAL        XLFWT
      REAL        XGNP
      REAL        XN
      REAL        XNF
      REAL        XNTI
      REAL        YIELD
      REAL        YIELDB
      INTEGER     YRDOY  !YR,   

!     Added to send messages to WARNING.OUT
!     CHARACTER*78 MESSAGE(10)

!     CHP added for P model 9/5/2004
      CHARACTER*1 ISWPHO
      INTEGER     YRPLT
      REAL        SPi_AVAIL(NL), PUptake(NL)    !, RWU(NL)
      REAL        FSLFP, PStres1
      REAL        PStres2, SeedFrac, VegFrac, SLFP
      REAL PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed

!     Transfer mass from stem to ear -- need to transfer P also
      REAL Stem2Ear

!     Added 04/19/07 US/CHP - Optional species coefficients for N conc
      REAL CTCNP1, CTCNP2

!     K model
      REAL, DIMENSION(NL) :: KUptake  !, SKi_Avail
!     REAL KSTRES

!     CHP 3/31/2006
!     Proportion of lignin in STOVER and Roots
      REAL PLIGLF, PLIGRT
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (ResidueType) SENESCE
      TYPE (SoilType) SOILPROP
      TYPE (SwitchType) ISWITCH
      TYPE (WeatherType) WEATHER

      DYNAMIC = CONTROL % DYNAMIC
      RUN     = CONTROL % RUN

      DLAYR  = SOILPROP % DLAYR
      DS     = SOILPROP % DS
      DUL    = SOILPROP % DUL
      KG2PPM = SOILPROP % KG2PPM
      LL     = SOILPROP % LL
      NLAYR  = SOILPROP % NLAYR
      SAT    = SOILPROP % SAT
      SHF    = SOILPROP % WR
      SLPF   = SOILPROP % SLPF  ! non-N soil fertility factor

      tempmx = weather % TMAX
      tempmn = weather % TMIN
      dlayr_nw = DLAYR * 10.0
      duldep   = DUL  ! JZW here shold be DUL * DLAYR * 10. !
      lldep    = LL
      satdep   = SAT
      WR       = SHF

!**!  DO L = 1, NLAYR
!**!     lldep(L) = LL(L)
!**!     duldep(L)= DUL(L)
!**!     dlayr_nw(L) = DLAYR(L) * 10.0
!**!  END DO

!----------------------------------------------------------------------
!                     DYNAMIC = RUNINIT
!----------------------------------------------------------------------
      IF(DYNAMIC.EQ.RUNINIT.OR.DYNAMIC.EQ.SEASINIT) THEN

          CALL GETLUN('OUTO', NOUTDO)
          IDETR  = ISWITCH % IDETR
          ISWNIT = ISWITCH % ISWNIT
          ISWPOT = ISWITCH % ISWPOT
          ISWPHO = ISWITCH % ISWPHO
          ISWWAT = ISWITCH % ISWWAT
          IDETO  = ISWITCH % IDETO
          MEEVP  = ISWITCH % MEEVP
          !-------------------------------------------------------
          !     Read input file name (ie. DSSAT45.INP) and path
          !-------------------------------------------------------
          CALL GETLUN('FILEIO', LUNIO)
          OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERR)
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
          REWIND (LUNIO)
          READ(LUNIO,50,IOSTAT=ERR) FILES, PATHSR; LNUM = 7
   50     FORMAT(//////,15X,A12,1X,A80)
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

          READ(LUNIO,51,IOSTAT=ERR) FILEE, PATHER; LNUM = LNUM + 1
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

          READ(LUNIO,51,IOSTAT=ERR) FILEC, PATHCR; LNUM = LNUM + 1
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
   51     FORMAT(15X,A12,1X,A80)
          !------------------------------------------------------
          !   Read Planting Details Section
          !------------------------------------------------------
          SECTION = '*PLANT'
          CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
          IF (FOUND .EQ. 0) THEN
            CALL ERROR(SECTION, 42, FILEIO, LNUM)
          ELSE
            READ(LUNIO,60,IOSTAT=ERR) PLTPOP,ROWSPC ; LNUM = LNUM + 1
C60         FORMAT(25X,F5.2,13X,F5.2,7X,F5.2)
 60         FORMAT(24X,F6.0,12X,F6.0,7X,F5.2)
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
          ENDIF
!     -----------------------------------------------------------------
!             Read crop cultivar coefficients
!     -----------------------------------------------------------------
          SECTION = '*CULTI'
          CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
          IF (FOUND .EQ. 0) THEN
              CALL ERROR(SECTION, 42, FILEIO, LNUM)
          ELSE
            READ (LUNIO,1800,IOSTAT=ERR)
     &          VARNO,VRNAME,ECONO,VSEN,PPSEN,P1,P5,PHINT,GRNO,MXFIL,
     &            STMMX,SLAP1
!------------------------------------------------------------------
! PNUPR = 0.45; APSIM pot_nuprate =  .45e-6 , g/mm root/day
! MNNCR=1.23: APSIM min_grain_nc_ratio = 0.0123
! EXNO3 = 6.74; APSIM enupf_no3 = 0.0675
! EXNH4 = 6.5 ; APSIM enupf_nh4 =  .065, exponent for NH4 supply factor
! MNRTN = 4.5 ; APSIM root_n_min = 0.0045
! MXNUP = 0.6 ; APSIM max_n_uptake = .6
! MNNO3 = 0; APSIM mnupf_no3
! MNNH4 = 0; APSIM mnupf_nh4, minimum for NH4 supply factor
! WFNU  = ;  APSIM wfnu_power, power term for water effect on N supply
! APSIM rmnpc= 0.45, root minimum N percent (0-100)
! p_max_grain_nc_ratio = 0.04
!-----------------------------------------------------------------
!*!1800        FORMAT (A6,1X,A16,1X,A6,1X,6F6.0)
1800        FORMAT (A6,1X,A16,1X,A6,1X,9F6.0)
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
          ENDIF

      VSEN = VSEN * 0.0054545 + 0.0003
      PPSEN = PPSEN *0.002

        CLOSE(LUNIO)

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
3100          FORMAT (A6,1X,A16,1X,10(1X,F5.1),2(1X,F5.2),3(1X,F5.1),
     &                1(1X,F5.3),1(1x,F5.0),11(1X,F5.2),1(1X,F5.3),
     &                1(1X,F5.2),1(1X,F5.3),5(1X,F5.2),3(1X,F5.3),
     &                2(1X,F5.2),1(1X,F5.1),1(1X,F5.2),1(1X,F5.3),
     &                2(1X,F5.0),2(1X,F5.2))
              IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEE,LNUM)
        
            ELSEIF (ISECT .EQ. 0) THEN
              CALL ERROR(ERRKEY,7,FILEE,LNUM)
            ENDIF
          ENDDO

      SLAP2 = SLAP2 * 100.          ! convert to mm2/g
          CLOSE (LUNECO)
        
!         ************************************************************
!         ************************************************************
!
!                 READ SPECIES FILE
!     Variable in *.spe          Variable in Fortran code
!      SENST                     SENST(1-4)
!      SENSF                     SENSF(1-4)
!      GTMAX                     GTMAX(1-4)
!      GRDUH                     GRDUH(1-4)
!      GTMIN                     GTMIN(1-4)
!      GRDUL                     GRDUL(1-4)
!      GTTPT                     GTTPT(1-3)
!      VPD                       VPD(1-4)
!      VPDF                      FVPD(1-4)
!      TCSLP                     TCSlope
!      TCINT                     TCInt
!         ************************************************************
!         ************************************************************


      FILECC =  TRIM(PATHSR) // FILES
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)

      !----------------------------------------------------------------
      !       Find and Read TEMPERATURE Section
      !----------------------------------------------------------------

      SECTION = '*TEMPE'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
       ! Effect of temperature on photosynthesis
       ! CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
       ! READ(C80,'(7X,4(1X,F5.2))',IOSTAT=ERR)
       ! &       PRFTC(1),PRFTC(2),PRFTC(3),PRFTC(4)
       ! IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

      ! Effect of temperature on relative grain filling rate
      !   CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
      !   READ(C80,'(7X,4(1X,F5.2))',IOSTAT=ERR) RGFIL(1),
      !&     RGFIL(2),RGFIL(3),RGFIL(4)
      !   IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        ! Temperatures effect for senescence of leaf
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!       SENST in *.spe
        READ(C80,'(7X, 1X, F5.0,3(1X,F5.0))',IOSTAT=ERR) SENST(1), 
     &     SENST(2), SENST(3), SENST(4)
        READ(C80,'(2X, A5)')temp_Char
        if (temp_Char(1:5) .ne. "SENST") then
            ERR =1
            WRITE(MSG(1),'(A,A,A)')'Read ',temp_Char,' instead of SENST'
            WRITE(MSG(2),'(A)') "Program will stop."
            CALL WARNING(2,ERRKEY,MSG)
        endif
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!       SENSF in  *.spe
        READ(C80,'(7X,4(1X,F5.0))',IOSTAT=ERR) SENSF(1), 
     &     SENSF(2), SENSF(3), SENSF(4)
        READ(C80,'(2X, A5)')temp_Char
        if (temp_Char(1:5) .ne. "SENSF") then
            ERR =1
            WRITE(MSG(1),'(A,A,A)')'Read ',temp_Char,' instead of SENSF'
            WRITE(MSG(2),'(A)') "Program will stop."
            CALL WARNING(2,ERRKEY,MSG)
        endif
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        ! Temperature effect for grain number
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(7X, 1X, F5.0,3(1X,F5.0))',IOSTAT=ERR) GTMAX(1), 
     &     GTMAX(2), GTMAX(3), GTMAX(4)
        READ(C80,'(2X, A5)')temp_Char
        if (temp_Char(1:5) .ne. "GTMAX") then
            ERR =1
            WRITE(MSG(1),'(A,A,A)')'Read ',temp_Char,' instead of GTMAX'
            WRITE(MSG(2),'(A)') "Program will stop."
            CALL WARNING(2,ERRKEY,MSG)
        endif
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(7X,4(1X,F5.0))',IOSTAT=ERR) GRDUH(1), 
     &     GRDUH(2), GRDUH(3), GRDUH(4)
         READ(C80,'(2X, A5)')temp_Char
        if (temp_Char(1:5) .ne. "GRDUH") then
            ERR =1
            WRITE(MSG(1),'(A,A,A)')'Read ',temp_Char,' instead of GRDUH'
            WRITE(MSG(2),'(A)') "Program will stop."
            CALL WARNING(2,ERRKEY,MSG)
        endif
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(7X, 1X, F5.0,3(1X,F5.0))',IOSTAT=ERR) GTMIN(1), 
     &     GTMIN(2), GTMIN(3), GTMIN(4)
        READ(C80,'(2X, A5)')temp_Char
        if (temp_Char(1:5) .ne. "GTMIN") then
            ERR =1
            WRITE(MSG(1),'(A,A,A)')'Read ',temp_Char,' instead of GTMIN'
            WRITE(MSG(2),'(A)') "Program will stop."
            CALL WARNING(2,ERRKEY,MSG)
        endif
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(7X,4(1X,F5.0))',IOSTAT=ERR) GRDUL(1), 
     &     GRDUL(2), GRDUL(3), GRDUL(4)
        READ(C80,'(2X, A5)')temp_Char
        if (temp_Char(1:5) .ne. "GRDUL") then
            ERR =1
            WRITE(MSG(1),'(A,A,A)')'Read ',temp_Char,' instead of GRDUL'
            WRITE(MSG(2),'(A)') "Program will stop."
            CALL WARNING(2,ERRKEY,MSG)
        endif
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(7X, 1X, F5.0,2(1X,F5.0))',IOSTAT=ERR) DTTPT(1), 
     &     DTTPT(2), DTTPT(3)
        READ(C80,'(2X, A5)')temp_Char
        if (temp_Char(1:5) .ne. "DTTPT") then
            ERR =1
            WRITE(MSG(1),'(A,A,A)')'Read ',temp_Char,' instead of DTTPT'
            WRITE(MSG(2),'(A)') "Program will stop."
            CALL WARNING(2,ERRKEY,MSG)
        endif
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(7X, 3(1X,F5.0))',IOSTAT=ERR) DTTF(1), 
     &     DTTF(2), DTTF(3)
        READ(C80,'(2X, A5)')temp_Char
        if (temp_Char(1:4) .ne. "DTTF") then
            ERR =1
            WRITE(MSG(1),'(A,A,A)')'Read ',temp_Char,' instead of DTTF'
            WRITE(MSG(2),'(A)') "Program will stop."
            CALL WARNING(2,ERRKEY,MSG)
        endif
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(7X, 1X, F5.0,3(1X,F5.0))',IOSTAT=ERR) VPD(1), 
     &     VPD(2), VPD(3), VPD(4)
        READ(C80,'(2X, A5)')temp_Char
        if (temp_Char(1:3) .ne. "VPD") then
            ERR =1
            WRITE(MSG(1),'(A,A,A)')'Read ',temp_Char,' instead of VPD'
            WRITE(MSG(2),'(A)') "Program will stop."
            CALL WARNING(2,ERRKEY,MSG)
        endif
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(7X, 4(1X,F5.0))',IOSTAT=ERR) FVPD(1), 
     &     FVPD(2), FVPD(3), FVPD(4)
        READ(C80,'(2X, A5)')temp_Char
        if (temp_Char(1:4) .ne. "VPDF") then
            ERR =1
            WRITE(MSG(1),'(A,A,A)')'Read ',temp_Char,' instead of VPDF'
            WRITE(MSG(2),'(A)') "Program will stop."
            CALL WARNING(2,ERRKEY,MSG)
        endif
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        ! Canopy temperature modification slop
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(7X, F5.0)',IOSTAT=ERR) TCSlope  !TCSLP in *.spe
        READ(C80,'(2X, A5)')temp_Char
        if (temp_Char(1:5) .ne. "TCSLP") then
            ERR =1
            WRITE(MSG(1),'(A,A,A)')'Read ',temp_Char,' instead of TCSLP'
            WRITE(MSG(2),'(A)') "Program will stop."
            CALL WARNING(2,ERRKEY,MSG)
        endif
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(7X, 1X,F4.0)',IOSTAT=ERR) TCInt ! TCINT in *.spe
        READ(C80,'(2X, A5)')temp_Char
        if (temp_Char(1:5) .ne. "TCINT") then
            ERR =1
            WRITE(MSG(1),'(A,A,A)')'Read ',temp_Char,' instead of TCINT'
            WRITE(MSG(2),'(A)') "Program will stop."
            CALL WARNING(2,ERRKEY,MSG)
        endif
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF
      REWIND(LUNCRP)
      !---------------------------------------------------------------
      !         Find and Read PHOTOSYNTHESIS section
      !---------------------------------------------------------------
      SECTION = '*PHOTO'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(8X,F6.3)',IOSTAT=ERR) PARSR
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(7X,10(1X,F5.0))',IOSTAT=ERR) CO2X(1),CO2X(2),
     &   CO2X(3), CO2X(4),CO2X(5), CO2X(6),CO2X(7),CO2X(8),
     &   CO2X(9),CO2X(10)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(7X,10(1X,F5.2))',IOSTAT=ERR) CO2Y(1),CO2Y(2),
     &   CO2Y(3), CO2Y(4),CO2Y(5), CO2Y(6),CO2Y(7),CO2Y(8),
     &   CO2Y(9),CO2Y(10)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

      ENDIF
      REWIND(LUNCRP)
!      ---------------------------------------------------------------
!               Find and Read APSIM CO2 section ! this section will not be used. It is replaced by model equation 
!      ---------------------------------------------------------------
      SECTION = '*APCO2'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(11X,I1)',IOSTAT=ERR)TEFAC 
!        READ(C80,'(9X,10(4x,F3.0))',IOSTAT=ERR) XCO2(1), XCO2(2),
!     &  XCO2(3), XCO2(4), XCO2(5), XCO2(6), XCO2(7), XCO2(8)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(11X,I1)',IOSTAT=ERR) RUEFAC 
!        READ(C80,'(9X,10(1X,F6.5))',IOSTAT=ERR) tec(1), tec(2),
!     &   tec(3), tec(4), tec(5), tec(6), tec(7), tec(8)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!       CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(9X,10(1X,F6.2))',IOSTAT=ERR) rue_fac(1),rue_fac(2),
!     &   rue_fac(3), rue_fac(4),rue_fac(5), rue_fac(6),rue_fac(7),
!     &   rue_fac(8)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!
      ENDIF
      REWIND(LUNCRP)
      !----------------------------------------------------------------
      !        Find and Read Stress Response
      !----------------------------------------------------------------
      SECTION = '*STRES'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F6.3)',IOSTAT=ERR) FSLFW
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F6.3)',IOSTAT=ERR) FSLFN
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        IF (ISWPHO .NE. 'N') THEN
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(9X,F6.3)',IOSTAT=ERR) FSLFP
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF
      ENDIF
      REWIND(LUNCRP)
      !----------------------------------------------------------------
      !        Find and Read Seed Growth Parameters
      !----------------------------------------------------------------
      SECTION = '*SEED '
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(8X,F6.4)',IOSTAT=ERR) SDSZ
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(8X,F6.3)',IOSTAT=ERR) RSGR
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(8X,F6.3)',IOSTAT=ERR) RSGRT
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(8X,F6.3)',IOSTAT=ERR) CARBOT
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

      ENDIF
      REWIND(LUNCRP)
      !----------------------------------------------------------------
      !        Find and Read Emergence Initial Conditions
      !----------------------------------------------------------------
      SECTION = '*EMERG'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F6.3)',IOSTAT=ERR) STMWTE
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F6.3)',IOSTAT=ERR) RTWTE
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F8.5)',IOSTAT=ERR) LFWTE
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F6.3)',IOSTAT=ERR) SEEDRVE
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F6.3)',IOSTAT=ERR) LEAFNOE
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F6.3)',IOSTAT=ERR) PLAE
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

      ENDIF
      REWIND(LUNCRP)

      !----------------------------------------------------------------
      !        Find and Read Plant Nitrogen Parameters
      !----------------------------------------------------------------
      SECTION = '*NITRO'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F8.4)',IOSTAT=ERR) TMNC
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F8.3)',IOSTAT=ERR) TANCE
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F8.4)',IOSTAT=ERR) RCNP
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F8.3)',IOSTAT=ERR) RANCE
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F8.3)',IOSTAT=ERR) CTCNP1
!       If error reading either value, use defaults
        IF (ERR /= 0 .OR. ISECT /= 1 .OR. CTCNP1 < 1.E-6) THEN
          CTCNP1 = 1.52
          CTCNP2 = 0.160
        ELSE
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(9X,F8.3)',IOSTAT=ERR) CTCNP2
          IF (ERR /= 0 .OR. ISECT /= 1 .OR. CTCNP2 < 1.E-6) THEN
            CTCNP1 = 1.52
            CTCNP2 = 0.160
          ENDIF
        ENDIF
      ENDIF
      REWIND(LUNCRP)

      !----------------------------------------------------------------
      !        Find and Read Root parameters
      !----------------------------------------------------------------
      SECTION = '*ROOT '
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F6.3)',IOSTAT=ERR) PORMIN
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F6.3)',IOSTAT=ERR) RWUMX
        ! Maximum water uptake per unit root length, constrained by soil
!             water (cm3[water] / cm [root])
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F6.3)',IOSTAT=ERR) RLWR
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F6.3)',IOSTAT=ERR) RWUEP1
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(8X,9(2X,F5.2))',IOSTAT=ERR) (rootfr(II),II=1,9)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

      ENDIF
      REWIND(LUNCRP)

      !---------------------------------------------------------------
      !       Find and Read AERATION DEFICIT PARAMETERS (Nwheat roots)
      !---------------------------------------------------------------
      SECTION = '*AERAT'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(10X,3(1X,F5.1))',IOSTAT=ERR) (p_fdsw(II),II=1,3)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(10X,3(1X,F5.1))',IOSTAT=ERR) (p_adf(II),II=1,3)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(10X,2(1X,F5.1))',IOSTAT=ERR) (p_stage(II),II=1,2)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(10X,2(1X,F5.1))',IOSTAT=ERR) (p_afs(II),II=1,2)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

      ENDIF
      REWIND(LUNCRP)

      !---------------------------------------------------------------
      !       Find and Read GROWTH STAGE (Nwheat)
      !**!       note: moved to StageFlags subroutine in WH_PHENOL.for
      !---------------------------------------------------------------
      SECTION = '*GROWT'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(10X,7(1X,F5.1))',IOSTAT=ERR) (istageno(II),II=1,7)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(10X,7(1X,F5.1))',IOSTAT=ERR) (dc_code(II),II=1,7)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(10X,6(1X,F5.1))',IOSTAT=ERR) (xs(II),II=1,6)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(10X,6(1X,F5.1))',IOSTAT=ERR) (zs(II),II=1,6)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

      ENDIF
      CLOSE (LUNCRP)

!** Initialize variables

      CALL SenLig_Ceres(PLIGLF=PLIGLF, PLIGRT=PLIGRT)

! JIL 08/01/2006 parameters for ear growth
!** Read in SPE file?

!*!       af2_lai    = 1.0  !Remove if nwheats_set_adf is implemented
!*!       af2_tiller = 1.0  !Remove if nwheats_set_adf is implemented
          af2_photo  = 1.0  !Remove if nwheats_set_adf is implemented
          ASGDD = 100.0     !Ending of ear growth (gdd)
          BSGDD = 250.0     !Beginning of ear growth (gdd)

          APTNUP = 0.0
          AREALF = 0.0
          ASMDOT = 0.0
          BIOMAS = 0.0
          CANHT  = 0
!**
!         set to 1.6 m, need to become user-input; RS 26May04
          CANHT_POT = 1.6
!**
          CANNAA = 0.0
          CANWAA = 0.0
          CANWH  = 0
          CARBO  = 0.0
          CMAT   = 0
          CUMDTTEG=0.0
          CumLeafSenes = 0.0
          CumLeafSenesY = 0.0
          CumLfNSenes = 0.0
          DUMMY  = 0.0
          EARS   = 0.0
          EARWT  = 0.0
          EMAT   = 0
          EP1    = 0.0
          g_obs_gpsm = 0.0  !*! NWheat observed grain #.
          GRAINN = 0.0
          GRF    = 0.0
          GNP = 0.0
          GNUP   = 0.0
          GRNWT  = 0.0
          gro_wt(1) = 0.0  ! can replaced by gro_wt = 0.
          gro_wt(2) = 0.0
          gro_wt(3) = 0.0
          gro_wt(4) = 0.0
          gro_wt(5) = 0.0
          gro_wt(6) = 0.0
          GROEAR = 0.0
          gpp_stem_wt = 0.0
          GPSM   = 0.0
          grogrn = 0.0
          grolf  = 0.0
          GRORT  = 0.0
          GROSTM = 0.0
          HI     = 0.0
          HIP    = 0.0
          ICOLD  = 0
          ICSDUR = 0
          IPAR   = 0.0
          lai    = 0.0
          LAIDOT = 0.0
          LEAFNO = 0
          LFWT   = 0.0
          LIFAC  = 0.0
          MAXLAI = 0.0
          nfact(1) = 1.0
          nfact(2) = 1.0
          nfact(3) = 1.0
          nfact(4) = 1.0
          NPOOL  = 0.0
          NPOOL1 = 0.0
          NPOOL2 = 0.0
          NSDR   = 0.0
          NSINK  = 0.0
          PCNVEG = 0.0
          PCO2   = 0.0
          PCNGRN = 0.0
          PCNL   = 0.0
          PCNRT  = 0.0
          PCNSD  = 0.0
          PCNST  = 0.0
          PDWI   = 0.0
          PLA = 0.0
          plag   = 0.0
          PLAS = 0.0
          PODNO  = 0.0
          PODWT  = 0.0
          PRFT   = 0.0
          PPLTD  = 0.0
          PTF    = 0.0
          ptgrn  = 0.0
          RANC   = 0.0
          rrgfill = 0.0
          RMNC   = 0.0
          RNLAB  = 0.0
          ROOTN  = 0.0
          RSTAGE = 0
          RTWTO  = 0.0
          RTWT   = 0.0
          SATFAC = 0.0
          SDWT = 0.0
          SEEDNO = 0.0
          SEEDRV = 0.0
          SENLA  = 0.0
          SENESCE % ResWt  = 0.0
          SENESCE % ResLig = 0.0
          SENESCE % ResE   = 0.0

          SHELPC = 0.0
          SLA    = 0.0
          SKERWT = 0.0
          SLAN = 0.0
          SLFC   = 0.0
          SLFN   = 0.0
          SLFP   = 0.0
          SLFT   = 0.0
          SLFW   = 0.0
          SI1 = 0.0
          SI2 = 0.0
          SI3 = 0.0
          SI4 = 0.0
          SI5 = 0.0
          SI6 = 0.0
          Stem2Ear = 0.0
          STMWT  = 0.0
          STMWTO = 0.0
          STOVER = 0.0
          STOVN  = 0.0
          STOVWT = 0.
          SUMP   = 0.0
          SUMRL  = 0.0
          SUMEX  = 0.0
          swdef(photo_nw)  = 1.0
          swdef(cellxp)    = 1.0
          swdef(tiller_nw) = 1.0
          SWEXF  = 0.0
          SWFAC  = 1.0
          SWIDOT = 0.0
          SWMAX = 0.0
          SWMIN = 0.0
          TANC   = TANCE
          TAVGD  = 0.0
          TCNP   = 0.0
          TEMPM  = 0.0
          TFAC   = 0.0
          TI     = 0.0
          TLNO   = 0.0
          TNLAB  = 0.0
          TOPWT  = 0.0
          TOTNUP = 0.0
          TRNU   = 0.0
          DO L=1,NLAYR
              TSS(L) = 0.0
          ENDDO
          TURFAC = 1.0
          VANC   = 0.0
          VMNC   = 0.0
          VSTAGE = 0.0
          WLIDOT = 0.0
          WRIDOT = 0.0
          WSIDOT = 0.0
          WTLF   = 0.0
          WTNCAN = 0.0
          WTNLF  = 0.0
          ! WTNSD  = 0.0 !which is pl_nit_grain
          WTNST  = 0.0
          !WTNUP  = 0.0 ! Which is WTNUP = cumpnup / 10.0 ,
          WTNVEG = 0.0
          XANC   = 0.0
          XGNP   = 0.0
          XHLAI  = 0.0
          XLAI   = 0.0
          XLFWT  = 0.0
          XN = 0.0
          XNF    = 0.0
          XNTI   = 0.0
          YIELD  = 0.0
          YIELDB = 0.0
          rlv_nw = 0. ! add by JZW
          plantwt = 0.0

          DO L=1,NL
              dlayr_nw(L) = DLAYR(L) * 10.0
          ENDDO

          IF (ISWNIT .NE. 'N') THEN
             CALL TF_NFACTO(DYNAMIC,TANC,TCNP,TMNC,
     %       AGEFAC,NDEF3,NFAC,NSTRES)
          ELSE
              AGEFAC = 1.0
              NSTRES = 1.0
              NDEF3 = 1.0
              NFAC = 1.0
          ENDIF
          gtmaxfac= 1.0
          gtminfac= 1.0
! The following 2 subroutine calls were removed for WHAPS (Nwheat)
! since no APSIM-nwheat code was transfered to WH_NUPTAK or WH_KUPTAK
          ! JZW recover the following two calls
!      CALL WH_NUPTAK(
!     %      RANC, ROOTN,RTWT,TANC,STOVN,STOVWT,TRNU,NLAYR,
!     %      RLV, no3ppm, nh4ppm, PDWI,TCNP,UNO3,UNH4,
!!**!     %      RLV,NO3,NH4,PDWI,TCNP,UNO3,UNH4,
!     %      XSTAGE,RCNP,PGRORT,PLTPOP,SW,LL,SAT,DLAYR,
!     %      SHF,PTF, SENESCE, KG2PPM, PLIGRT)

!**!       CALL WH_KUPTAK(
!**!  &       ISWPOT, NLAYR, SKi_Avail, UNH4, UNO3,        !Input
!**!  &       KUPTAKE, KSTRES)                             !Output

          CALL P_Ceres (DYNAMIC, ISWPHO,                    !Input
     &      CumLeafSenes, DLAYR, DS, FILECC, MDATE, NLAYR,  !Input
     &      PCNVEG, PLTPOP, PODWT, RLV, RTDEP, RTWTO,       !Input
     &      SDWT, SWIDOT, SeedFrac, SPi_AVAIL, Stem2Ear,    !Input
     &      STMWTO, VegFrac, WLIDOT, WRIDOT, WSIDOT,        !Input
     &      WTLF, YRPLT,                                    !Input
     &      SENESCE,                                        !I/O
     &      PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, !Output
     &      PStres1, PStres2, PUptake, FracRts)             !Output

!-----------------------------------------------------------------------

      IF (DYNAMIC.EQ.RUNINIT .AND. IDETR .EQ. 'Z') THEN
          OUTXL  = 'MultiVar.OUT'       ! IDETR switch set to 'Z' is un-
          CALL GETLUN('OUTXL',  NOUTXL) ! documented elsewhere. Purpose
                                        ! is to provide intermediate
                                        ! variables for use in Excel
          INQUIRE (FILE = OUTXL, EXIST = FEXIST)
          IF (FEXIST) THEN
            OPEN (UNIT=NOUTXL,FILE='MultiVar.out',STATUS='UNKNOWN')
            CLOSE (UNIT=NOUTXL, STATUS = 'DELETE')
          ENDIF
!          call nwheats_rtlv (CONTROL,
!     &  dlayr_nw, istage, nrlayr, p2af, p3af, PLTPOP,             !Input
!     &  nwheats_level, rtdep_nw, stgdur, wr,                      !Input
!     &  duldep, lldep, swdep, nlayr_nw, rlv_nw, gro_wt, adf,      !Input
!     &  rlvnew)                                                  !Output
      ENDIF

      IF (DYNAMIC .EQ. SEASINIT .AND. IDETR .EQ. 'Z') THEN
          !-------------------------------------------------------------
          !   Delete old and open new MultiVar.OUT
          !-------------------------------------------------------------
          INQUIRE (FILE = OUTXL, EXIST = FEXIST)
          IF (FEXIST) THEN
            OPEN (UNIT=NOUTXL, FILE=OUTXL, STATUS='OLD',
     &        IOSTAT=ERRNUM, POSITION='APPEND')
            FIRST = .FALSE.
          ELSE
            OPEN (UNIT=NOUTXL, FILE=OUTXL, STATUS='NEW',
     &        IOSTAT = ERRNUM)
              WRITE(NOUTXL,'("*INTERMEDIATE VARIABLE OUTPUT FILE")')
            FIRST = .TRUE.
          ENDIF
          !---------------------------------------------------------
          ! Generate variable heading for GROWTH.OUT
          !---------------------------------------------------------
          CALL HEADER(SEASINIT, NOUTXL, RUN)
          WRITE (NOUTXL,201)
  201     FORMAT('  YRDOY dieleaf     ',
     &   ' cumph_nw(1)      cumph_nw(2)      cumph_nw(3)     ',
     &   ' cumph_nw(4)            dtiln             frlf     ',
     &   '      fstage        gro_wt(1)        gro_wt(2)     ',
     &   '   gro_wt(3)        gro_wt(4)          leafsen     ',
     &   ' nwheat_vfac    nwheat_topsfr            optfr     ',
     &   '       pl_la       plantwt(2)       plantwt(3)     ',
     &   '     plsc(1)          plsc(2)          plsc(3)     ',
     &   '     plsc(4)          plsc(5)     ',
     &   '        prft             rtsw           sen_la     ',
     &   '     sfactor             slan             slft     ',
     &   '        slfw     sumstgdtt(1)     sumstgdtt(2)     ',
     &   'sumstgdtt(3)     sumstgdtt(4)     sumstgdtt(5)     ',
     &   '         tc1              tc2             tiln     ',
     &   ' trans_wt(1)      trans_wt(2)      trans_wt(3)     ',
     &   ' trans_wt(4)      trans_wt(5)      trans_wt(6)     ',
     &   '         vd1              vd2               vd     ',
     &   '    xstag_nw')
      ENDIF
      CALL nwheats_rtdp(CONTROL, SOILPROP,
     &  ADPHO, dlayr_nw, dtt, duldep, g_water_table, istage,  !Input
     &  lldep, nrlayr, p3af, rtdep_nw,                        !Input
     &  RTDP1, RTDP2, SDEPTH, stgdur, swdef, swdep,           !Input
     &  rtdnew)                                               !Output
      ! JZW add the following two lines
      rwu_nw = 0.
      sen_la = 0.
      tiln = 0 !tpsm = 0  !Jin Add on Feb 25, 2014
      sla_new = 0  !Jin Add on Feb 25, 2014
      rtdep_nw = SDEPTH * 10.0 !Jin Add on Feb 25, 2014
      pl_nit = 0 ! IT  IS WTNSD, Jin Add on Feb 26, 2014
      pl_nit_grain = 0
      pl_nit_stem = 0
      pl_nit_root = 0
      pl_nit_lfsheath = 0
      pl_nit_leaf = 0
      pntrans = 0 !Jin Add on Feb 26, 2014
      cumpnup = 0
      g_water_table = 0
      !trans_wt = 0
      !pl_dmd = 0
      cumph_nw = 0
      sumstgdtt = 0
      rtsw = 0
      aver_tsw = 0
      tilsw = 0
      IF(DYNAMIC.EQ. SEASINIT) THEN
         anth_date = 0
         GAD2 = 0.
      endif
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!                     DYNAMIC = RATE
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! The following if statement was previously in the DYNAMIC = INTEGR
! section, but istage never equaled sowing (8) there, and the
! initialization at sowing never took place.

      ELSEIF(DYNAMIC.EQ.RATE) THEN
      !transp_eff_coeff  =  TABEX (tec, XCO2, WEATHER % CO2 ,8)
      if (TEFAC .eq. 1) then
!        transp_eff_coeff = 0.006 * ( 1+ (0.37/350)*(WEATHER % CO2 -350))
!The standard transpiration efficiency coefficient was increased from from 0.006 to 0.009 based on
!Hammer and Muchow's (1994) sorghum model.
!The rate of change in response to increasing CO2 was decreased to 0.28 based on the rate of change
!calculated for sorghum by Pembleton et al. (2016).
        transp_eff_coeff = 0.009 * ( 1+ (0.28/350)*(WEATHER % CO2 -350))
      else
!        transp_eff_coeff = 0.006
        transp_eff_coeff = 0.009
      endif
!       transp_eff_coeff should be used in daily rate instaed of run ini, 
!       because the CO2 is not availabe in the runini or seasini
!       JZW replace rue_factor/transp_eff_coeff by model equation from David on Mar. 22, 2014
!       rue_factor  = TABEX (rue_fac, XCO2, WEATHER % CO2 ,8) 
!      Tdepend is Temperature dependent CO2 compensation point
      if (RUEFAC .eq. 1) then
        Tdepend = (163 - (Tmax+Tmin)/2)/(5.0 - 0.1* (Tmax+Tmin)/2)
        rue_factor = (WEATHER % CO2 - Tdepend) *(350 + 2* Tdepend)/
     &           ((WEATHER % CO2 + 2*Tdepend)*(350 - Tdepend))
      else
        rue_factor = 1
      Endif

      if (stgdur(sowing) .eq. 1  ! FSR - Changed from .eq. 1
     &    .and. istage .eq. sowing) then ! we have just planted the seed

         plantwt(seed_part) = seedrvNW

      endif

      CALL nwheats_rtdp(CONTROL, SOILPROP,
     &  ADPHO, dlayr_nw, dtt, duldep, g_water_table, istage,  !Input
     &  lldep, nrlayr, p3af, rtdep_nw,                        !Input
     &  RTDP1, RTDP2, SDEPTH, stgdur, swdef, swdep,           !Input
     &  rtdnew)                                               !Output
        nlayr = count_of_real_vals (dlayr_nw, NL)
         nwrlay = count_of_real_vals (wr, nlayr)
         depmax = sum_real_array (dlayr_nw, nwrlay)
!**!     rtdnew = bound (rtdnew, 0.0, depmax - rtdep)
         rtdnew = MAX(rtdnew, 0.0)
         rtdnew = MIN(rtdnew, depmax - rtdep_nw)

          ! update root depth pool
         rtdep_nw = rtdep_nw + rtdnew

          ! -------------- get new root length volume
!**!     nrlayr = nwheats_level(rtdep_nw)
         nrlayr = nwheats_level(rtdep_nw, dlayr_nw, NL)

        if (istage.eq.germ) then
        ! JZW move these setting from INTEGR section to here
         nwheats_topsfr= 0
         cwdemand= 0.0
        endif

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!                     DYNAMIC = INTEGR
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      ELSEIF(DYNAMIC.EQ.INTEGR) THEN

          SENESCE % ResWt  = 0.0
          SENESCE % ResLig = 0.0
          SENESCE % ResE   = 0.0
          KUPTAKE = 0.0

!         Keep yesterdays cumulative senescence
          CumLeafSenesY = CumLeafSenes

          swdep = SW  ! soil water from ROOTWU to WHAPS variable

!      Need to set these at the beg. of each new growth stage. From phasei
!      Initialize variables at the beginning of a new phenological stage
!      This code was moved from phenol or phasei in Generic CERES v 3.9

          IF (YRDOY == STGDOY(1) .OR. YRDOY == STGDOY(2) .OR.
     &        YRDOY == STGDOY(3) .OR. YRDOY == STGDOY(4) .OR.
     &        YRDOY == STGDOY(5) .OR. YRDOY == STGDOY(6) .OR.
     &        YRDOY == STGDOY(8) .OR. YRDOY == STGDOY(9)) THEN
              ICSDUR = 1
          ENDIF
!*! IF statement above was modified after MZ_IX_MAIZE, to include all
!*! stages except fallow (7).
          IF(YRDOY.EQ.STGDOY(3)) THEN
              SWMIN  = STMWT*0.85
              SUMP   = 0.0
              ICSDUR = 1
              CANNAA = STOVN*PLTPOP
              CANWAA = BIOMAS
          ENDIF

          IF(YRDOY.EQ.STGDOY(4)) THEN
              SWMAX  = STMWT
              VANC   = TANC
              VMNC   = TMNC
              EMAT   = 0
              ICSDUR = 1
          ENDIF

!*!       IF(YRDOY.EQ.STGDOY(9)) THEN
          IF(YRDOY.EQ.STGDOY(1)) THEN  !*! IF Emergence (1 in WHAPS)
              !Emergence
              STMWT   = STMWTE
              RTWT    = RTWTE !APSIM using seedrvNW0.012*0.5
              plantwt(leaf_part) = LFWTE
!**!              LFWT    = LFWTE
              STOVWT  = LFWT+STMWT
              SEEDRV  = SEEDRVE
              BIOMAS = STOVWT
              LEAFNO = INT(LEAFNOE)
              PLA     = PLAE
              SENLA  = 0.0
!*!           LAI    = PLTPOP*PLA*0.0001
              IF (ISWNIT .NE. 'N') THEN
                  GRAINN = 0.000
!*!               TANC = TANCE
                  RANC = RANCE   !Initialize root N (fraction) JIL
                  ROOTN = RANC   * RTWT
                  STOVN = STOVWT * TANC
              ENDIF

              WTLF = plantwt(leaf_part) * PLTPOP      !Leaf weight, g/m2
              STMWTO = STMWT * PLTPOP   !Stem weight, g/m2
              RTWTO = RTWT * PLTPOP     !Root weight, g/m2
              CALL P_Ceres (EMERG, ISWPHO,                      !Input
     &          CumLeafSenes, DLAYR, DS, FILECC, MDATE, NLAYR,  !Input
     &          PCNVEG, PLTPOP, PODWT, RLV, RTDEP, RTWTO,       !Input
     &          SDWT, SWIDOT, SeedFrac, SPi_AVAIL, Stem2Ear,    !Input
     &          STMWTO, VegFrac, WLIDOT, WRIDOT, WSIDOT,        !Input
     &          WTLF, YRPLT,                                    !Input
     &          SENESCE,                                        !I/O
     &          PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, !Output
     &          PStres1, PStres2, PUptake, FracRts)             !Output
          ENDIF

!         CHP 7/29/2008
          IF (MDATE == YRDOY) THEN
            RETURN
          ENDIF

          !-------------------------------------------------------------
          !           Begin Rate and Integration Calculations
          !-------------------------------------------------------------

          TEMPM = (TMAX + TMIN)*0.5   !Mean air temperature, C

          !-------------------------------------------------------------
          !           Compute critical nitrogen contents
          !-------------------------------------------------------------

          TCNP = EXP (CTCNP1 - CTCNP2 * XSTAGE) / 100.0

          IF (xstag_nw .LT. 4.0) THEN
              TMNC =  (1.25-0.200*xstag_nw)/100.0

              !JZW if this is copied from   MZ_GROSUB.for(1027)?
          ENDIF

          !-------------------------------------------------------------
          !        Compute Nitrogen Stress Factors
          !-------------------------------------------------------------
          IF (ISWNIT .NE. 'N' .AND. ISTAGE .LT. 7) THEN
              CALL TF_NFACTO(DYNAMIC,                 !Control
     %        TANC,TCNP,TMNC,                         !Inputs
     %        AGEFAC,NDEF3,NFAC,NSTRES)               !Outputs
          ELSE
              AGEFAC = 1.0
              NSTRES = 1.0
              NDEF3 = 1.0
              NFAC = 1.0
          ENDIF
          !-------------------------------------------------------------
          !      Compute Water Stress Factors
          ! ------------------------------------------------------------
          SWFAC  = 1.0
          TURFAC = 1.0
          IF(ISWWAT.NE.'N') THEN
             IF (EOP .GT. 0.0) THEN
                EP1 = EOP * 0.1
                IF (TRWUP / EP1 .LT. RWUEP1) THEN
                   TURFAC = (1./RWUEP1) * TRWUP / EP1
                ENDIF
                IF (EP1 .GE. TRWUP) THEN
                  SWFAC = TRWUP / EP1
                ENDIF
             ENDIF

          ENDIF
!-----------------------------------------------------------------
!*! NWheat code while calculating potential: no H2O stress.
       IF (ISWWAT .EQ. 'N') then

       ENDIF
!-----------------------------------------------------------------

          TURFAC = REAL(INT(TURFAC*1000))/1000


          !-------------------------------------------------------------
          !      Compute Water Saturation Factors
          ! ------------------------------------------------------------

          SATFAC = 0.0
          SUMEX = 0.0
          SUMRL = 0.0

          DO L = 1,NLAYR

          !------------------------------------------------------------
          !PORMIN = Minimum pore space required for supplying oxygen to
          !         roots for optimum growth and function
          !TSS(L) = Number of days soil layer L has been saturated
          !         above PORMIN
          !------------------------------------------------------------
              IF ((SAT(L)-SW(L)) .GE. PORMIN) THEN
                  TSS(L) = 0.
              ELSE
                  TSS(L) = TSS(L) + 1.
              ENDIF
          !------------------------------------------------------------
          ! Delay of 2 days after soil layer is saturated before root
          ! water uptake is affected
          !------------------------------------------------------------
              IF (TSS(L) .GT. 2.) THEN
                  SWEXF = (SAT(L)-SW(L))/PORMIN
                  SWEXF = MAX(SWEXF,0.0)
              ELSE
                  SWEXF = 1.0
              ENDIF

              SWEXF = MIN(SWEXF,1.0)
              SUMEX  = SUMEX + DLAYR(L)*RLV(L)*(1.0 - SWEXF)
              SUMRL  = SUMRL + DLAYR(L)*RLV(L)
          ENDDO

          IF (SUMRL .GT. 0.0) THEN
              SATFAC = SUMEX/SUMRL
          ELSE
              SATFAC = 0.0
          ENDIF
          SATFAC = AMAX1(SATFAC,0.0)
          SATFAC = AMIN1(SATFAC,1.0)

C         Calculate soil water table depth
!          CALL WTDEPT(
!     &      NLAYR, DLAYR, DS, DUL, SAT, SW,               !Input
!     &      WTDEP)                                        !Output
          CALL GET('WATER','WTDEP',WTDEP)

          g_water_table = WTDEP *10    ! for APSIM Nwheat model

!======================================================================
          call ntefs_set_nconc (xstag_nw, istage,              !Input
     &      zstage, VSEN,                                        !Input
     &      cnc, mnc)                                           !Output
         !*! APSIM code: call nwheats_set_nconc (cnc, mnc)
!======================================================================
!======================================================================
!*! Begin WHAPS calculation of grains (kernels) per plant
!*!       (from APSIM NWheat subroutine nwheats_gpp)
!----------------------------------------------------------------------
      if (stgdur(grnfil).eq.1 .and. istage.eq.grnfil) then

         if (g_obs_gpsm.ne.0) then
!*!         grpp = g_obs_gpsm/plants ! I assume plants =/= 0
        !Added IF statement to void divisions by zero (TF - 01/21/2022)
            IF(PLTPOP .GT. 0.0) THEN
               grpp = g_obs_gpsm/PLTPOP ! NWheat plants = DSSAT PLTPOP
            ELSE 
               grpp = 0 
            ! g_obs_gpsm  is Observed number of grains per plant
            ENDIF
!            g_obs_gpsm is currently set to 0, and therefore this if statement is irrelevant
         else
cnh Senthold
!**!        grpp = pl_wt(stem) * grnmx
!            grpp = gpp_stem_wt * GRNO
!            GRNO is reported in the CUL file in terms of 100 grains/g stem. Multiplying GRNO times 100 in the code resulted in
!            massive spikes in grain yield and unit grain weights so small that they were rounded to 0 g/grain. Therefore GRNO is kept
!            as is. The output for the final grain number is in terms of 0.01 grains/m2 and needs to be multiplied by 100 to get the actual
!            grains/m2. The output for the unit grain weight is reported in mg/100 grains and needs to be divided by 100 to get the actual mg/grain.
            GAD2 = gpp_stem_wt * GRNO
            grpp = gpp_stem_wt * GRNO* gtmaxfac * gtminfac
            gpp = grpp
         endif

      else
         grpp = gpp

      endif
!----------------------------------------------------------------------
!*! End WHAPS calculation of grains (kernels) per plant
!*!       (from APSIM NWheat subroutine nwheats_gpp)
!======================================================================
!*! Begin WHAPS initializations of plant weights
!*!      (from APSIM NWheat subroutine nwheats_plwin and nwheats_crppr)
!----------------------------------------------------------------------
          ! initialise plant weight
          ! initialisations - set up seed carbohydrate or leaf, stem
          ! and root
      call nwheats_set_adf (control, 
     &    dlayr_nw, duldep, g_water_table, nrlayr,       !Input 
     &    p3af, p4af, p5af, p6af, rtdep_nw,                      !Input
     &    satdep, swdep, xstag_nw, p_fdsw, p_adf, p_afs, p_stage, !Input
     &    nlayr_nw, adf, afs)                                    !Output
       ! JZW: Add nlayr_nw declaration and here
      CALL nwheats_rtdp(CONTROL, SOILPROP,
     &  ADPHO, dlayr_nw, dtt, duldep, g_water_table, istage,  !Input
     &  lldep, nrlayr, p3af, rtdep_nw,                        !Input
     &  RTDP1, RTDP2, SDEPTH, stgdur, swdef, swdep,           !Input
     &  rtdnew)                                               !Output

          ! get actual root growth and constrain it by the maximum
          ! depth that roots are allowed to grow.
         nlayr = count_of_real_vals (dlayr, NL)
         nwrlay = count_of_real_vals (wr, nlayr)
         depmax = sum_real_array (dlayr_nw, nwrlay)
!**!     rtdnew = bound (rtdnew, 0.0, depmax - rtdep)
         rtdnew = MAX(rtdnew, 0.0)
         rtdnew = MIN(rtdnew, depmax - rtdep_nw)

          ! update root depth pool
         rtdep_nw = rtdep_nw + rtdnew

          ! -------------- get new root length volume
!**!     nrlayr = nwheats_level(rtdep_nw)
         nrlayr = nwheats_level(rtdep_nw, dlayr_nw, NL)
      do L = 1, NLAYR
        sno3(L) = no3ppm(L)/KG2PPM(L)
        snh4(L) = nh4ppm(L)/KG2PPM(L)
      enddo

      call nwheats_rtlv (CONTROL, SOILPROP, sno3, snh4,
     &  dlayr_nw, istage, nrlayr, p2af, p3af, PLTPOP,             !Input
     &  nwheats_level, rtdep_nw, stgdur, wr,                      !Input
     &  duldep, lldep, swdep, rlv_nw, gro_wt, adf,               !Input
     &  rlvnew)                                                  !Output

!**!  call add_real_array (rlvnew, rlv, nrlayr)
        call add_real_array (rlvnew, rlv_nw, nrlayr)
      if (istage .lt. sowing
     &    .and. istage .gt. emergence) then
         ! the remaining seed disappears.
         plantwt(seed_part) = 0.0

      else
      endif

      if (stgdur(emergence) .eq. 1
     &        .and. istage .eq. emergence) then

          ! Seedling has just emerged. Initialise root, stem and leaf.

         plantwt(root_part) = seedrvNW * .5
         plantwt(leaf_part) = LFWTE
         plantwt(stem_part) = 0.0001 ! nh - in case zero causes trouble

!---------From nwheats_crppr-------------------------------------------
!*!      If((Istage.eq.1).and.(Stgdur(istage).eq.1)) then             !
cnh added 7-4-94 ???                                                  !
          pl_la = 40.0                                                !
!*!       pl_la = 48.0  ! temporary test - FSR                        !
          tiln = 1.0                                                  !
          tpsm = tiln*PLTPOP                                          !
!*!      else                                                         !
!*!      endif                                                        !
!---------From nwheats_crppr-------------------------------------------

      elseif (stgdur(grnfil) .eq. 1
     &        .and. istage .eq. grnfil) then ! start of grain filling
c Senthold
!*!      if (stemmn_p1 .eq. 0.0) then
         if (STEMN .eq. 0.0) then
            plwtmn(stem_part) = plwtmn(stem_part)
     &                     + (plantwt(stem_part) - plwtmn(stem_part))
!*!  &                     * p_fr_remob_part
     &                     * NOMOB ! p_fr_remob_part replaced w/ NOMOB
         else
            ! It is to be down below
         endif

c Senthold
c         pl_wt(grain) = 0.0035 * gpp
!*!      pl_wt(grain) = u_bound(p_init_grain_wt*gpp, plantwtmn(stem))
!        KEP 02/11/18: convert CUL parameter from mg/(100 grains) to g/grain
         INGWTgrams = INGWT/100*0.001 
!         INGWTgrams = INGWT*0.001 ! convert CUL parameter from mg to g
         plantwt(grain_part) = MIN(INGWTgrams * gpp, plwtmn(stem_part))


cnh need to account for this initial (and significant) biomass
cnh in stem grown BEFORE ear growth.
         plantwt(stem_part) = plantwt(stem_part)-plantwt(grain_part)
         plwtmn(stem_part) = plwtmn(stem_part)-plantwt(grain_part)

c Senthold
!*!      if (stemmn_p1.gt.0.0) then
!*!          plantwtmn(stem) = plantwt(stem) * stemmn_p1
         if (STEMN .gt. 0.0) then
            plwtmn(stem_part) = plantwt(stem_part) * STEMN
         else
            ! It is to be down below
         endif
c
      else    ! no changes
      endif
!----------------------------------------------------------------------
!*! Begin WHAPS Photosynthesis Rate calculation
!*!       (from APSIM NWheat subroutine nwheats_parin)
!----------------------------------------------------------------------
!*!   if (reals_are_equal (fr_intc_radn, 0.0)) then
      if (fr_intc_radn .eq. 0.0) then
      ! we need to calculate our own interception

         ! The equation below implies that leaf interception of
         ! photosynthetically active radiation obeys beer's law.
         ! extn_coeff (kvalue input) is extinction coefficient.
!*!      lai = (pl_la - sen_la) *plants/sm2smm
!*!      extn_coeff = nwheats_kvalue ()
!*!      radfr = 1.0 - exp (-extn_coeff*lai)
!*!      lfipar = radfr*solrad  (solrad same as SRAD)

         lai = (pl_la - sen_la) *PLTPOP/sm2smm
         XHLAI=LAI  ! XHLAI: Healthy leaf area index used to compute
                    ! transpiration in water balance routine

         radfr = 1.0 - exp (-nwheats_kvalue * lai)
         lfipar = radfr * SRAD

      else  ! interception has already been calculated for us
!*!      lfipar = fr_intc_radn * solrad
         lfipar = fr_intc_radn * SRAD
      endif
!----------------------------------------------------------------------
!*! End WHAPS Photosynthesis Rate calculation
!*!       (from APSIM NWheat subroutine nwheats_parin)
!======================================================================

c senthold
      if ((STEMN .eq. 0.0) .and. (istage .eq. endear)) then
!*!      if (sumdtt(istage) .le. pgdd(istage) * p_fr_endear_remob) then
         if (sumstgdtt(istage) .le. pgdd(istage) * FREAR) then
            ! set min stem wt
            plwtmn(stem_part) = plantwt(stem_part)
         else
         endif

      p_min_pot_hi = 0.0   !Senthold hard-wired this

      elseif ((istage .eq. grnfil) .and. (p_min_pot_hi .gt. 0.0)) then
         topsbiom = plantwt(grain_part) + plantwt(leaf_part)
     &            + plantwt(lfsheath_part) + plantwt(stem_part)
         plwtmn(stem_part) = plantwt(stem_part)
     &                - (               topsbiom - plantwt(grain_part))
         plwtmn(stem_part) = max(plwtmn(stem_part), 0.0)
      else
      endif
!----------------------------------------------------------------------
!*! End WHAPS initializations of plant weights
!*!      (from APSIM NWheat subroutine nwheats_plwin and nwheats_crppr)
*======================================================================
      !*! subroutine nwheats_plnin (plantn) initial plant N
      CALL nwheats_plnin (CONTROL, istage, stgdur, plantwt,      !input
     &    mnc, INGNC, MNRTN,                                     !input
     &    pl_nit )                                       !input & output
*======================================================================
!      call nwheats_add_sen_roots(CONTROL,
!     &  rootsen, rtdep_nw,                               !Input
!     &  deepest_layer, root_array)                      !Output
       !JZW, why we need to output  deepest_layer, root_array?
cjh temp fix to avoid any further development during maturity stage
   ! (from nwheats_crppr, "goto" end of routine; end development)
      if (istage.eq.mature  .or. istage.eq.fallow) then
!*!      goto 900
                   ! needs to go to harvest from here (FSR)
!*!900   continue
!*!      call nwheats_crtot ()
      else
      endif
!----------------------------------------------------------------------
!*! Begin WHAPS leaf emergence calculation
!*!       (from APSIM NWheat subroutine nwheats_lfemr)
!----------------------------------------------------------------------
      if (istage .ge. emergence .and. istage .le. endjuv) then
!*!      frlf = divide (dtt, PhInt, 0.0)
         frlf = dtt / PhInt
      else
         frlf = 0.0  ! frlf - New fraction of oldest expanding leaf
      endif

      ti = frlf   ! Fraction of a phyllochron interval
                  ! represented by today's daily thermal time
!----------------------------------------------------------------------
!*! End WHAPS leaf emergence calculation
!*!       (from APSIM NWheat subroutine nwheats_lfemr)
!----------------------------------------------------------------------
!           update the fully expanded leafs and constrain it to the maximum
!           number that leafs are allowed to grow.
       cumph_nw(istage) = cumph_nw(istage) + ti

!======================================================================
      IF (ISWNIT .NE. 'N') THEN
         !*! Call APSIM subroutine nwheats_set_nfact
         CALL  ntefs_set_nfact (xstag_nw, istage,              !Input
     &         cnc, mnc, pl_nit, plantwt,                      !Input
     &         nfact)                                          !Output
      ELSE
          nfact = 1.0
      ENDIF
!======================================================================

!       ADD BY jzw IN NOV 28:
!       RWUEP1 where use it???????????????????
!      sw_demand is input of set_swdef_new from respond2get_real_var
!       demand is output of nwheats_cwdmd

!       cwdemand shoulld be output of nwheats_watup_new
!       set_swdef_new should not have input cwdemand. nwheats.for does not have cwdemand here

!        we should move call nwheats_cwdmd out of call nwheats_watup_new. Because nwheats_cwdmd needs a lot arguments
!      pl_la is calculated in line 2494 also
!       potntl should be remover from input of nwheats_watup_new,  if keep call  nwheats_cwpot inside this subroutine

!       call nwheats_cwdmd (CONTROL,uptake_source,
!     &    carbh, nwheats_topsfr, PLTPOP,                   !Input
!       ! nwheats_topsfr is available in line 2101
!     &    pcarbo, TMAX, TMIN,                              !Input
!     &    cwdemand)
!       call nwheats_cwpot (CONTROL, SOILPROP, rlv_nw, swdep,
!     &    cumph_nw, cwdemand, istage,                        !Input
!     &    nrlayr, pl_la, PLTPOP, sen_la,                          !Input
!      ! sen_la is unknown in line 1646 ; pl_la is calculated in line 2494 also
!     &    potntl)                                                        !Output
!       Call nwheats_watup_new (CONTROL,     SOILPROP, swdep, uptake_water,
!     &    cwdemand, istage, nrlayr, potntl, uptake_source,     !Input
!    !&    rlv_nw, above_gnd_pcarb, carbh, nwheats_topsfr, PLTPOP, pcarbo, temp_c, TMAX, TMIN,
!    ! need above by call nwheats_cwdmd
!    !&  cumph_nw, cwdemand, istage,  nrlayr, pl_la, PLTPOP, sen_la,
!    ! Need above by call nwheats_cwpot
!     &    prwu, rwu_nw) !JZW: pesw_nw should be output. Actually, it is intermediate data, do not need to output
!       !!!! If cwdemand and rwu_nw is from yesterday, we need to initialize when Dynamic is run initial
!        moved the following to Rate section, by JZWU on Feb 25, 2014
!       if (istage.eq.germ) then
!       rwu_nw = 0.0 move to season initial
!         cwdemand= 0.0
!        if uptake_source is from calc, then cwdemand is not needed in set_swdef_nw
!       endif
       uptake_source = 'calc'

!======================================================================
      IF (ISWWAT .NE. 'N') THEN ! Call APSIM subroutine set_swdef_new
! jzw: Input here cwdemand should be sw_demand
! jzw: APSIM use SW_demand (cwdwmand) which is reading from respond2get_real_var
          CALL set_swdef_new (swdep, uptake_source,
     &         cwdemand, istage, rtdep_nw, rwu_nw, SOILPROP,
     &         swdef)                                           !Output
      ELSE
          swdef = 1.0
      ENDIF
!======================================================================



!----------------------------------------------------------------------
!*! Begin WHAPS potential dry matter production calculation
!*!       (from APSIM NWheat subroutine nwheats_ptcarb)
!----------------------------------------------------------------------
!*! Notes from NWheat code:
!           potential dry matter production at optimum temperature, soil
!           water, and N content is calculated.
!           this is g of dry biomass is produced per mj of intercepted
!           photosynthetically active radiation under nonstressed conditions.

!           note that assumptions are made as to the relative flows of carbo to
!           below ground biomass.  To keep above ground biomass correct and allo
!           assumed roots in biomass accumulation.

cbak    testing:
!*!     ce_tops = 3.8 * divide (solrad**0.63, solrad, 0.0)
!*!     ce_tops = 3.2 * divide (solrad**0.7, solrad, 0.0)
!*!     ce_tops = u_bound(ce_tops,2.0)

!       ce_tops = 3.8 * (SRAD**0.63 / SRAD)
!       KEP 01/31/18 Increased RUE by 18.5%, the difference between the CERES-Wheat and the CERES-Sorghum RUE
        ce_tops = 4.5 * (SRAD**0.63 / SRAD) 
        ce_tops = MIN(ce_tops,2.0)

!*!   if (nwheats_min_rootfr() .gt. 0.0) then
      if (rootfr(istage) .gt. 0.0) then
!          there is a carbon demand by roots - increase ce to allow for this
!*!      rootfr = nwheats_min_rootfr()  FSR removed function nwheats_min_rootfr()
!*!      ce_roots = ce_tops * divide (rootfr, 1. - rootfr, 0.0)
         ce_roots = ce_tops * (rootfr(istage) / (1. - rootfr(istage)))

      else
         ! no Carbon demand by roots
         ce_roots = 0.0

      endif

      ce = ce_tops + ce_roots

!*!   pcarbo = divide (ce*lfirad, plants, 0.0)
      IF(PLTPOP .GT. 0.0) THEN
        pcarbo = ce * (lfipar/PLTPOP) * rue_factor
      ELSE
        pcarbo = 0.0
      ENDIF
!----------------------------------------------------------------------
!*! End WHAPS potential dry matter production calculation
!*!       (from APSIM NWheat subroutine nwheats_ptcarb)
!======================================================================
!*! Begin WHAPS - actual & potential biomass (CH2O) production from
!*!       photosynthesis   (from APSIM NWheat subroutine nwheats_biomp)
!----------------------------------------------------------------------
      ptcarb = pcarbo

          ! now get the temperature stress factor that reduces
          ! photosynthesis (0-1)

cbak set the min temp for phs at -3oc

!*!     Increase the base growth temperature from -4 to 3.8 C for tef
!       if (TMIN .gt. -4.0) then  !*! TMIN replaces tempmn
       if (TMIN .gt. 3.8) then  
        ! photosynthetic rate decreases away from the optimum (18)

cbak optimum of 18oc for photosynthesis

         prft = 1.-0.0025*((0.25*TMIN+0.75*TMAX)-18.)**2
!*!      prft = bound (prft, 0.0, 1.0)
         prft = MAX(prft, 0.0)
         prft = MIN(prft, 1.0)

      else
         ! cold morning - too cold for plants to grow today
         prft = 0.0
      endif

!           --------------- actual -----------------

!           now get the actual dry matter (carbohydrate) production on the
!           day by discounting by temperature, water or N stress factors.
cnh senthold
cnh      optfr = min (swdef(photo), nfact(1)) * prft
      optfr = min (swdef(photo_nw), nfact(1), ADPHO) * prft
        !! threshold aeration deficit (AF2) affecting photosyn
      carbh = ptcarb*optfr
      carbh = MAX(carbh, 0.0)  !*! was: carbh = l_bound (carbh, 0.0)

      if (istage .eq. grnfil) then
         carbh =
     &    carbh * (1. - (1.2 - 0.8*plwtmn(stem_part)/plantwt(stem_part))
     &          * (sumstgdtt(istage) + 100.)/(pgdd(grnfil) + 100.) )
         carbh = MAX(carbh, 0.0)  !*! was: carbh = l_bound (carbh, 0.0)

      else if (istage.gt.grnfil) then
         carbh = 0.0

      else
      endif

      carbh = MAX(carbh, 0.0001) !*! was: carbh = l_bound(carbh, 0.0001)
!----------------------------------------------------------------------
!*! End WHAPS - actual & potential biomass (CH2O) production from
!*!       photosynthesis   (from APSIM NWheat subroutine nwheats_biomp)
!======================================================================
! JZW change on Feb 25, 2014, the day of istage=germ, may go to rate section, but not in integr section
 !     if (istage.eq.germ) then
 !        !nwheats_topsfr=? initialize here
 !      endif
      call nwheats_cwdmd (uptake_source,
     &    carbh, nwheats_topsfr, PLTPOP,                   !Input
       ! nwheats_topsfr is available in line 2100
     &    pcarbo, TMAX, TMIN, transp_eff_coeff,            !Input
     &    cwdemand, vpd_transp)

       call nwheats_cwpot (SOILPROP, rlv_nw, swdep,
     &    cumph_nw, istage,                                       !Input
     &    nrlayr, pl_la, PLTPOP, sen_la,                          !Input
!            sen_la is unknown in line 1646 ; pl_la is calculated in line 2494 also
     &    potntl)

      Call nwheats_watup_new (SOILPROP, swdep, uptake_water,
     &    cwdemand, nrlayr, potntl, uptake_source, !Input
!    &    rlv_nw, above_gnd_pcarb, carbh, nwheats_topsfr, PLTPOP, pcarbo, temp_c, TMAX, TMIN,
!     need above by call nwheats_cwdmd
!    &  cumph_nw, cwdemand, istage,  nrlayr, pl_la, PLTPOP, sen_la,
!     Need above by call nwheats_cwpot
     &    prwu, rwu_nw)                                    !Output
       rwu_nw = prwu    
! prwu(mm) is from calculation, rwu_nw is from input
!----------------------------------------------------------------------
!*! Begin WHAPS calculation of plant biomass production and partition
!*!             into various plant parts
!*!       (from APSIM NWheat subroutine nwheats_cdemand)
!----------------------------------------------------------------------
!=======================================================================
!*!   Find grain demand for carbohydrate (g/plant)
!----------------------------------------------------------------------
!  Revision history
!
!  12-22-2011 FSR transfered function nwheats_grdmd from APSIM nwheats
!----------------------------------------------------------------------
!*! grnrat   !
!*! grogrn   ! grain demand for carbohydrate (g/plant)
!*! ptgrn    ! potential grain growth (g/plant)
!*! rrgfill   ! relative (0-1) rate of grainfill for the day
                          ! due to temperature response (avg of periods)
* Implementation Section ----------------------------------

        if (istage.eq.grnfil) then ! effective grain filling period
          TEMPM = (TMAX + TMIN)/2.0
         if (TEMPM .gt. 10.) then
            rrgfill = 0.65 +
     :      (0.0787 - 0.00328*(TMAX-TMIN))*(TEMPM-10.)**0.8

         else
            rrgfill = 0.065 * TEMPM

         endif

         ! note that ceres-wheat did not bound rrgfill to above zero
         ! was that ok???
!*!      rgfill = bound (rgfill, 0.0, 1.0)
         rrgfill = MAX(rrgfill, 0.0)
         rrgfill = MIN(rrgfill, 1.0)

         ! now calculate the grain growth demand for the day in g/plant
         !*! ptgrn = gpp*(grnrat*mg2gm) where mg2gm is mg to gram
         ptgrn = gpp * (MXFIL * 0.001) ! grnrat now MXFIL; mg2gm=0.001?
         grogrn = ptgrn * rrgfill

      else  ! we are out of grain fill period
         grogrn = 0.0
      endif
cSenthold-1
!**!  grogrn = bound (grogrn,0.0,max(0.0,p_max_kernel_wt*mg2gm*gpp - pl_wt(grain)))
      grogrn = MAX(grogrn, 0.0)
      grogrn = MIN(grogrn,
     &             MAX(0.0,MXGWT * 0.001 * gpp - plantwt(grain_part)))
                         ! MXGWT was p_max_kernel_wt; mg2gm = 0.001?
      grdmd = grogrn
      pl_dmd(grain_part) = grdmd
!*!      call pop_routine (myname)
!*!      return
!*!      end
!=======================================================================
      if (pl_dmd(grain_part).gt.0.0) then
!----------------------------------------------------------------------
!*! Begin WHAPS calculation of grain nitrogen demand
!*!       (from APSIM NWheat subroutine nwheats_gndmd)
!----------------------------------------------------------------------

      CALL nwheats_gndmd (Istage, tempmx, tempmn, dtt, gpp, !Input
     &   gndmd_est)                                             !Output

!----------------------------------------------------------------------
!*! End WHAPS calculation of grain nitrogen demand
!*!       (from APSIM NWheat subroutine nwheats_gndmd)
!----------------------------------------------------------------------
!*!      call nwheats_gnptl (navl_est) ! nwheats_gnptl renamed GrN_Ptl

      call GrN_Ptl (CONTROL, 
     &   mnc, nfact, nitmn, npot, optfr, part, pl_la, pl_nit,    !INPUT
     &   plantwt, sen_la,                                        !INPUT
     &   navl_est)                                               !OUTPUT

      tot_navl_est  =  sum_real_array (navl_est, mxpart)
 !*!  create DO loop:
 !*!       tot_navl_est = 0.0  ! this may be unnecessary (FSR)
 !*!       DO L = 1,mxpart
 !*!          tot_navl_est  =  tot_navl_est + navl_est(L)
 !*!       ENDDO
CSenthold-2
         nflow = min(gndmd_est,tot_navl_est)

         IF (pl_dmd(grain_part) .gt. 1.e-6) THEN
            grain_nc_ratio = nflow / pl_dmd(grain_part)
         ENDIF
         grain_nc_ratio = MAX(grain_nc_ratio,
!*!  &  p_min_grain_nc_ratio)  ! assuming p_min_grain_nc_ratio = min_grain_nc_ratio
     &                        MNNCR/100)   !*! MNNCR is a .CUL parameter
         IF (grain_nc_ratio .gt. 1.e-6) THEN
           pl_dmd(grain_part) = nflow / grain_nc_ratio ! no nflow yet
         ENDIF

      else
         ! No grain demand - no N:C check required
      endif

!*! The following function calls are coded below
!*!   pl_dmd(leaf) = nwheats_lfdmd ()
!*!   pl_dmd(lfsheath) = nwheats_lfshdmd ()
!*!   pl_dmd(stem) = nwheats_stdmd ()
!*!   pl_dmd(root) = nwheats_rtdmd ()

!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!*! Begin WHAPS calculation of carbohydrate requirement
!*!       for leaf growth (g/plant)
!*!       (from APSIM NWheat function nwheats_lfdmd)

      if (istage.eq.emergence) then
             ! the plant is young
             ! get total plant leaf area and then new leaf weight
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!*!         nwheats_gplag_emerg ()  ! function begins here
!   returns the growth in leaf area during emergence to endjuv (mm^2)

!*!   TEMPM = (TMAX + TMIN)/2.0
      tf = 1.2 - 0.0042 * (TEMPM - 17.)**2.
!*!   nwheats_plagtf = bound (tf, 0.0, 1.0)
      plagtf = MAX(tf, 0.0)
      plagtf = MIN(plagtf, 1.0)

      optfr = MIN(swdef(cellxp), nfact(2), ADLAI, plagtf)
cnh senthold
cnh      plagms = 1400.*(cumph(istage)**.6)*ti*optfr
      plagms = PLGP1 * (cumph_nw(istage)**PLGP2) * ti * optfr

      plag = plagms * (0.3 + 0.7 * tiln)

      nwheats_gplag_emerg = plag  ! This may eventually be deleted:
                                  ! by just using plag.

!                  nwheats_gplag_emerg ()  ! function ends here
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!*!         sla_new = nwheats_sla ()    ! Nwheat function begins here

!*!        CUL input SLAP1 replaces sla_p1
!*!        CUL input SLAP2 replaces sla
!*!        note that SLAP2 was converted to mm2/g (multiplied by 100)
!*!        sla_new - specific leaf area of new growth (mm2/g)
      sla_new = SLAP2 + ((SLAP1*100.-SLAP2) * exp(-2.*(xstag_nw-1.)))
      sla = sla_new/100

!                     nwheats_sla ()    ! Nwheat function ends here
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!*!      grolf = divide (plag, sla_new, 0.0)
         if (sla_new .eq. 0.) then
             write(MSG(1),*) "sla_new is zero"
             CALL WARNING(1,"NWheat",MSG)
             CALL ERROR("NWheat",99," ",0)
         endif
         grolf = plag / sla_new

      else if (istage .eq. endjuv) then
         ! leaves will take what ever is left over
         grolf = 0.0

      else
!              we have no leaf growth in these stages.  Plag should be 0.
         grolf = 0.0

      endif

!*!   nwheats_lfdmd = l_bound (grolf, 0.0)
      nwheats_lfdmd = MAX(grolf, 0.0)

!*! End WHAPS calculation of carbohydrate requirement
!*!       for leaf growth (g/plant)
!*!       (from APSIM NWheat function nwheats_lfdmd)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!*! Begin WHAPS calculation of carbohydrate requirement
!*!       for leaf sheath growth (g/plant)
!*!       (from APSIM NWheat function nwheats_lfshdmd)

      if (istage.eq.emergence) then
         ! leaf sheath growth = leaf growth

!*!      plag = nwheats_gplag_emerg () ! already calculated above
!*!      sla_new = SLA                 ! already calculated above

cbak : set leaf sheath demand to 0.8 of leaf blade demand
!         grolfsh = divide (0.8*plag, sla_new, 0.0)
         if (sla_new .eq. 0.) then
             write(MSG(1),*) "sla_new is zero"
             CALL WARNING(1,"NWheat",MSG)
             CALL ERROR("NWheat",99," ",0)
         endif
         grolfsh = 0.8*plag/sla_new

      else if (istage.eq.endjuv) then  ! This IF-THEN clause has no
                                      ! purpose: grolfsh = 0 in any case

!          leaf sheaths will take what ever is left over
!          (Does this make sense? Both ELSE clauses have same result.) FSR
         grolfsh = 0.0

      else
!              we have no leaf growth in these stages.  Plag should be 0.
         grolfsh = 0.0

      endif

!*!      nwheats_lfshdmd = l_bound (grolfsh, 0.0)
         lfshdmd = MAX(grolfsh, 0.0)
!*! End WHAPS calculation of carbohydrate requirement
!*!       for leaf sheath growth (g/plant)
!*!       (from APSIM NWheat function nwheats_lfshdmd)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!*! Begin WHAPS calculation of carbohydrate requirement
!*!       for stem growth (g/plant)
!*!       (from APSIM NWheat function nwheats_stdmd)
!*! Note: must be called after leaf, ear and grain demands

!*! Begin WHAPS calculation of tops fraction, from
!*! Nwheats function nwheats_topsfr
!*!
!*!   max_topsfr = (1. - nwheats_min_rootfr()) ! rootfr(istage) substituted for
      max_topsfr = (1. - rootfr(istage))       ! nwheats_min_rootfr()
      if (istage .eq. emergence) then
         topsfr = max_topsfr
      else if (istage .eq. grnfil) then
         topsfr = 0.65 + 0.35 * (plwtmn(stem_part)/plantwt(stem_part))
      else
         topsfr =max_topsfr -0.1 *(1.0 -min (swdef(photo_nw),nfact(2)))
      endif
cnh changed for comparison
cnh      cw_topsfr = bound (topsfr, 0.0, max_topsfr)
      if (istage .eq. grnfil) then
!*!      nwheats_topsfr = bound (topsfr, 0.0, 1.0)
         nwheats_topsfr = MAX(topsfr, 0.0)
         nwheats_topsfr = MIN(nwheats_topsfr, 1.0)
      else
!*!      nwheats_topsfr = bound (topsfr, 0.0, max_topsfr)
         nwheats_topsfr = MAX(topsfr, 0.0)
         nwheats_topsfr = MIN(nwheats_topsfr, max_topsfr)
      endif
!*! End WHAPS calculation of tops fraction, from
!*! Nwheats function nwheats_topsfr
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
               !*! nwheats_min_rootfr = rootfr (istage)
      if (istage .eq. endjuv) then
cnh note that endjuv is only 3 phints long and so most c will still go into leaf

         ptfstem = (0.15 + 0.15 * sumstgdtt(endjuv)/phint)
!*!      ptfstem = u_bound (ptfstem, 0.85)
         ptfstem = MIN(ptfstem, 0.85)
         grostm = (carbh * topsfr) * ptfstem

      else if (istage .ge. endveg .and. istage .le. grnfil) then
         grostm = carbh * topsfr

      else
         grostm = 0.0
      endif

      nwheats_stdmd = grostm
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!*! End WHAPS calculation of carbohydrate requirement
!*!       for stem growth (g/plant)
!*!       (from APSIM NWheat function nwheats_stdmd)
!*! Note: must be called after leaf, ear and grain demands
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!*! Begin WHAPS calculation of carbohydrate requirement
!*!       for root growth (g/plant)
!*!       (from APSIM NWheat function nwheats_rtdmd)

! give roots carbohydrate they need  (g/plant)
!*!   grort = carbo * (1.0 - nwheats_topsfr())
!*!   nwheats_rtdmd = l_bound (grort, 0.0)
      grort = carbh * (1.0 - nwheats_topsfr)
      nwheats_rtdmd = MAX(grort, 0.0)
!*! End WHAPS calculation of carbohydrate requirement
!*!       for root growth (g/plant)
!*!       (from APSIM NWheat function nwheats_rtdmd)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      pl_dmd(root_part) = nwheats_rtdmd
      pl_dmd(leaf_part) = nwheats_lfdmd
      pl_dmd(lfsheath_part) = lfshdmd
      pl_dmd(stem_part) = nwheats_stdmd
!----------------------------------------------------------------------
!*! End WHAPS calculation of plant biomass production and partition
!*!             into various plant parts
!*!       (from APSIM NWheat subroutine nwheats_cdemand)
!======================================================================
!*! Begin WHAPS code for APSIM NWheat subroutine nwheats_partition
!*!                 (no description given in NWheat documentation)
!----------------------------------------------------------------------
!*!   call fill_real_array (gro_wt, 0.0, mxpart)
      do L=1, mxpart
         gro_wt(L) = 0.0
      enddo
          ! now translocate carbohydrate between plant components
          ! this is different for each stage
      part_shift = 0.0
      if (istage.eq.emergence) then
!           roots get carbon left after leaf demand but root demand must
!           be met. Excess carbon partitioned to roots is "respired".
!           This is not elegant, but other systems result in unstable root syste

          gro_wt(root_part) =
     &           carbh - pl_dmd(leaf_part) - pl_dmd(lfsheath_part)
          !jzw NEGATIVE VALUE HERE
          if (gro_wt(root_part) .le. 0) then
               continue
          endif
!           JZW: the following if statement is non-sense
!           gro_wt(root_part) = MAX(gro_wt(root_part), pl_dmd(root_part)) should replace if statement
          if (gro_wt(root_part) .gt. pl_dmd(root_part)) then
            part_shift = gro_wt(root_part) - pl_dmd(root_part)
            gro_wt(root_part) = pl_dmd(root_part)+ part_shift
            gro_wt(leaf_part) = (carbh - gro_wt(root_part))/2.0
            gro_wt(lfsheath_part) = gro_wt(leaf_part)
           else
            gro_wt(root_part) =
!*!  &            l_bound (gro_wt(root_part), pl_dmd(root_part))
     &            MAX(gro_wt(root_part), pl_dmd(root_part))
            gro_wt(leaf_part) = (carbh - gro_wt(root_part))/2.0
            gro_wt(lfsheath_part) = gro_wt(leaf_part)
          endif
         ! leaf and leaf sheath share equally any carbo left

       else if ( istage .eq. endjuv) then
          ! root and stem get what they demand
          gro_wt(root_part) = pl_dmd(root_part)
          gro_wt(stem_part) = pl_dmd(stem_part)

          ! leaf and leaf sheath share equally any carbo left
cbak       adjust partitioning to leaves if water or n stress is present
cbak       redirect additional c to roots

          pot_growt_leaf =
     &           (carbh - gro_wt(root_part) - gro_wt(stem_part))/2.0

          gro_wt(lfsheath_part) = pot_growt_leaf

cbak apply an additional reduction in leaf blade expansion with swdef(cellxp)
cnh senthold
cnh          gro_wt(leaf) = pot_growt_leaf * swdef(cellxp)
          gro_wt(leaf_part) =
     &           pot_growt_leaf * min(swdef(cellxp),ADLAI)
cbak allocate reduction in leaf area growth to root growth

          part_shift = pot_growt_leaf - gro_wt(leaf_part)
          gro_wt(root_part) = gro_wt(root_part) + part_shift

!------------------------------------------------------------------------------
cbak  part_shift is some carbon that has been redirected from leaves under stres
cbak  consider using it to reflect on sla under stress (ie. lower sla, thicker l
!-------------------------------------------------------------------------------

      else if (istage.eq.endveg
     :               .or.
     :         istage.eq.endear ) then
          gro_wt(root_part) = pl_dmd(root_part)
          gro_wt(stem_part) = pl_dmd(stem_part)
          if  (istage .eq. endear )then
            if ((anth_date .EQ. 0) .and. (Sumstgdtt(endear) .ge. 80)
     :         ) then
               anth_date = YRDOY
            endif
                gtmaxfac= gtmaxfac * (1+ ALIN (GTMAX, GRDUH, 4, TMAX)*
   !  :          0.01 * min (1., max(0., ((Sumstgdtt(endear)-20)/60))) )
     :           0.01 * min (1., max(0.,
     :             ALIN(DTTPT, DTTF, 3, Sumstgdtt(endear)))))
                gtminfac= gtminfac * (1+ ALIN (GTMIN, GRDUL, 4, TMIN)*
    ! :           0.01 * min (1., max(0., ((Sumstgdtt(endear)-20)/60))))
     :           0.01 * min (1., max(0.,
     :             ALIN(DTTPT, DTTF, 3, Sumstgdtt(endear)))))
!                ! II= MAX( 0, TIMDIF(anth_date, YRDOY))
!                 gtmaxfac= gtmaxfac * (1+ ALIN (GTMAX, GRDUH, 4, TMAX)*
!      :           0.01 * min (1., max(0., ((Sumstgdtt(endear)-80)/40))) )
!                 ! 0.01 is percentage
!                 gtminfac= gtminfac * (1+ ALIN (GTMIN, GRDUL, 4, TMIN)*
!      :           0.01 * min (1., max(0., ((Sumstgdtt(endear)-80)/40))) )
          endif
cbak
          gro_wt(leaf_part) = 0.0
          gro_wt(lfsheath_part) = 0.0

      else if (istage.eq.grnfil) then

         gro_wt(root_part) = pl_dmd(root_part)
         grogrnmx = carbh - gro_wt(root_part)
!*!      gro_wt(grain) = u_bound (pl_dmd(grain), grogrnmx)
         gro_wt(grain_part) = MIN(pl_dmd(grain_part), grogrnmx)
         gro_wt(stem_part) =
     &           carbh - gro_wt(root_part) - gro_wt(grain_part)

       else

             ! we have no change in carbohydrate allocation

       endif
!        JZW add the following: Dr. Senthold siad that APSIM has restriction:
!        root growing weight rate can not large than top growing rate
!        This may affect more for low Nitrogen case
       temp = gro_wt(lfsheath_part)+gro_wt(leaf_part)+
     :    gro_wt(stem_part) + gro_wt(grain_part)
       if  (gro_wt(root_part) .gt.  temp) then
          gro_wt(root_part) = temp
       endif
          ! now check that we have mass balance

      totprt = sum_real_array (gro_wt, mxpart)

!             the carbohydrate in the seed is available for uptake into the
!             rest of the plant.

!*! The following subroutine probably sends a message when fstage
!*! is "out of bounds".  I have not reproduced that here.  FSR
!*!   call bound_check_real_var(totprt, carb - 0.001, carb + 0.001, 'totprt')

!*!   call pop_routine (myname)
!*!   return
!*!   end
!----------------------------------------------------------------------
!*! End WHAPS code for APSIM NWheat subroutine nwheats_partition
!*!                 (no description given in NWheat documentation)
!======================================================================
!*! Begin WHAPS calculation of plant weight delta's due to
!*!       translocation (g/plant)
!*!       (from APSIM NWheat subroutine nwheats_translocate)
!----------------------------------------------------------------------
!*!   call fill_real_array (trans_wt, 0.0, mxpart)
            do L=1, mxpart
               trans_wt(L) = 0.0
            enddo

          ! now translocate carbohydrate between plant components
          ! this is different for each stage

      if (istage.eq.emergence) then
         if (gro_wt(leaf_part) .lt. pl_dmd(leaf_part)) then
            ! try using seed reserves
cnh do we need to try and satisfy leaf sheath demand as well???????????
!*!         Ctrans = u_bound (pl_dmd(leaf_part)
            Ctrans =     MIN (pl_dmd(leaf_part)
!**! &               - gro_wt(leaf_part),pl_wt(seed_part))
     &               - gro_wt(leaf_part),plantwt(seed_part))
            trans_wt(leaf_part) = Ctrans
            trans_wt(seed_part) = - Ctrans
         else
         endif

      else if (istage.eq.grnfil) then
         if (gro_wt(grain_part) .lt. pl_dmd(grain_part)) then
            ! try using stem reserves
!*!         Ctrans = u_bound (pl_dmd(grain_part)-gro_wt(grain_part),
            Ctrans =     MIN (pl_dmd(grain_part)-gro_wt(grain_part),
!**! &          pl_wt(stem_part)-plwtmn(stem_part)+gro_wt(stem_part))
     &          plantwt(stem_part)-plwtmn(stem_part)+gro_wt(stem_part))
!*!         Ctrans = l_bound (Ctrans,0.0)
            Ctrans =     MAX (Ctrans,0.0)
            trans_wt(grain_part) = Ctrans
            trans_wt(stem_part) = - Ctrans
         else
         endif

      else
        ! no translocation in this phase
      endif

          ! now check that we have mass balance

      mass_balance = sum_real_array (trans_wt, mxpart)
!             the carbohydrate in the seed is available for uptake into the
!             rest of the plant.

!*! The following subroutine probably sends a message when fstage
!*! is "out of bounds" (more than 0.001 from 0.0).
!*! I have not reproduced that here.  FSR
!*!   call bound_check_real_var (mass_balance, - 0.001, 0.001,
!*!  :     'mass balance')

!*!   call pop_routine (myname)
!*!   return
!*!   end
!----------------------------------------------------------------------
!*! End WHAPS calculation of plant weight delta's due to
!*!       translocation (g/plant)
!*!       (from APSIM NWheat subroutine nwheats_translocate)
!======================================================================
!*! Begin WHAPS tillering algorithm
!*!                   (from APSIM NWheat subroutine nwheats_tillering )
*+  Notes
*    if translocation from stem is to occur during stages for tillering
*    - it will have to effect this.
!----------------------------------------------------------------------

cbak chages from 1.5 to 2.5

      if (istage .eq. emergence) then
cnh senthold
cnh         If (cumph(istage) .gt. 2.5) then
         If (cumph_nw(istage) .gt. TC1P1) then
            ! get potential supply of tillers per phyllochron
cnh            tc1 = -2.5 + cumph (istage)
            tc1 = TC1P2*cumph_nw (istage) - TC1P1

            ! get potential supply of tillers allowable by competition
            tpsm = tiln * PLTPOP

            if (tpsm .ge. 800.) then
cbak            if (tpsm .ge. 600.) then
               tc2 = 0.0
            else
               tc2 = 2.5e-7 * (800. - tpsm)**3.
            endif

cbak replace swdef(cellxp) with swdef(tiller) in next equation

            wfactor = 1.4 * swdef(tiller_nw) - 0.4
            nfactor = 1.4 * nfact(3) - 0.4

            wfactor = MAX (wfactor, 0.0)
            nfactor = MAX (nfactor, 0.0)
cnh senthold
cnh            optfr = min (wfactor,nfactor)
            optfr = min (wfactor,nfactor,ADTIL)
            dtiln = ti * optfr * min (tc1,tc2)

         else
            ! too early for tillering
            dtiln = 0.0

         endif

      elseif (istage .eq. endjuv) then

cbak  tiller number is used in stage "emerg" to help determine plag
c     however, it does not appear to be used anywhere else.

cnh senthold
cnh         optfr = min (nfact(2), swdef(photo))
         optfr = min (nfact(2), swdef(photo_nw), ADTIL)
         tilsw = tilsw +
!*!  &         stmmx * 0.0889 * dtt * sumdtt(istage)/PhInt**2 * optfr
     &         stmmx * 0.0889 * dtt * sumstgdtt(istage)/PhInt**2 * optfr
!**!     aver_tsw = divide ((pl_wt(stem) + growt(stem)),tiln, 0.0)
         if (tiln .GT. 0.0) then
!**!        aver_tsw = (pl_wt(stem_part) + gro_wt(stem_part))/tiln
            aver_tsw = (plantwt(stem_part) + gro_wt(stem_part))/tiln
         else                             ! gro_wt was growt
            aver_tsw = 0.0
         endif

!*!      rtsw = divide (aver_tsw, tilsw, 1.0)

            if (tilsw .eq. 0.) then
                rtsw = 1.0
            else
                rtsw = aver_tsw / tilsw   ! NWheat "divide" function
                                          ! simplified
            endif
cnh senthold
cnh         dtiln = dtt * 0.005 * (rtsw - 1.)
         dtiln = dtt * DTNP1 * (rtsw - 1.)   ! DTNP1 substituted for


      else if (istage .eq. endveg) then
cnh senthold
cnh         optfr = min (nfact(2), swdef(photo))
         optfr = min (nfact(2), swdef(photo_nw), ADTIL)
         tilsw = tilsw + stmmx * dtt * 0.25/PhInt * optfr
!**!     aver_tsw = divide ((pl_wt(stem) + growt(stem)),tiln, 0.0)
         if (tiln .GT. 0.0) then
!**!        aver_tsw = (pl_wt(stem_part) + gro_wt(stem_part)) / tiln
            aver_tsw = (plantwt(stem_part) + gro_wt(stem_part)) / tiln
         else
            aver_tsw = 0.0           ! This interpretation of the
         endif                       ! NWheat "divide" function
                                     ! is a guess (FSR)
!*!      rtsw = divide (aver_tsw, tilsw, 1.0)

            if (tilsw .eq. 0.) then
               rtsw = 1.0
            else
               rtsw = aver_tsw / tilsw   ! NWheat "divide" function
                                         ! simplified
            endif
cnh senthold
cnh         dtiln = dtt * 0.005 * (rtsw - 1.)
         dtiln = dtt * DTNP1 * (rtsw - 1.)    ! DTNP1 for DTILN_P1
      else
         ! No new tillers in this growth stage
         dtiln = 0.0

      endif ! (istage .eq. )

      if ((dtiln .lt. 0.0) .and. (tiln + dtiln .lt. 1.0)) then
         ! this delta would drop tiln below 1 tiller/plant
         dtiln = -1.0*(tiln - 1.)
      else
      endif

      if (dtiln .lt. 0.0) then
         ! we are actually killing tillers - keep track of these
         sentil = sentil - dtiln  ! sentil = senesced tillers?
      else
      endif

      tiln = tiln + dtiln

!*!   call pop_routine (myname)
!*!   return
!*!   end

!----------------------------------------------------------------------
!*! End WHAPS tillering algorithm, from APSIM NWheat subroutine
!*! nwheats_tillering, called by nwheats_crppr
!======================================================================
!*! Begin N process from APSIM nwheats_crppr subroutines
!----------------------------------------------------------------------

            ! find actual grain uptake by translocation
      !JZW should not call grain before dtage grnfil ??
      !JG linked MXNCR in nwheats_grnit
*     ==================================================================

      call nwheats_grnit (CONTROL,                        !Input
     &        Istage, dtt, gpp, gro_wt, mnc, MXNCR, nfact,        !Input
     &        nitmn, npot, optfr, part, pl_la, pl_nit,            !Input
     &        plantwt, sen_la, tempmn, tempmx, trans_wt,          !Input
     &        pntrans)                                           !Output
*     ==================================================================
! translocate N -  update plant pools
      pl_nit(grain_part) = pl_nit(grain_part)
     &                   + sum_real_array (pntrans, mxpart)
      pl_nit(root_part)  = pl_nit(root_part) - pntrans(root_part)
      pl_nit(stem_part)  = pl_nit(stem_part) - pntrans(stem_part)
      pl_nit(leaf_part)  = pl_nit(leaf_part) - pntrans(leaf_part)
      pl_nit(lfsheath_part)  = pl_nit(lfsheath_part)
     &                       - pntrans(lfsheath_part)

            ! find actual plant uptake
      g_uptake_source = 'calc'
*     ==================================================================
      CALL nwheats_nuptk (CONTROL, SOILPROP,                      !Input
     &      carbh, cnc, EXNH4/100, EXNO3/100,                     !Input
     &      g_uptake_source, gro_wt, MNNH4, MNNO3, MXNUP,         !Input
     &      pcarbo, pl_nit,  plantwt, PLTPOP,                     !Input
     &      PNUPR/1000000, rlv_nw, snh4, sno3, swdep,             !Input
     &      WFNU, xstag_nw,                                       !Input
     &      pnup, snup_nh4, snup_no3)                            !Output
*     ==================================================================
       ptnup = sum_real_array (pnup, mxpart)
      if (g_uptake_source .eq. 'calc') then
         call subtract_real_array (snup_no3, sno3, nrlayr)
         call subtract_real_array (snup_nh4, snh4, nrlayr)
      else
         ! it is already taken out - dont do it again.
      endif
      ! Calculate total root uptake
      cumpnup = cumpnup + sum_real_array (snup_no3, nrlayr)
     :                  + sum_real_array (snup_nh4, nrlayr)
       if (cumpnup . GT. 0.000000001) then
        continue
!         on day 1989753, snup_no3(1)=0.0335, snup_no3(2) = 0.0713, snop_nh4(1)=0.00058, snop_nh4(2) =0.021
!        cumpnup = 0.127kg/ha
       endif
      call add_real_array (pnup, pl_nit, mxpart)
!----------------------------------------------------------------------
!*! End N process from APSIM nwheats_crppr subroutines
!======================================================================

!*! from Nwheats_crppr

           ! --------- root senescense (include nwheats_rtlos function)
      nrlayr = count_of_real_vals(rlv_nw,NL)

      if (istage.eq.fallow) then    ! ?? do we need to add sowing ????
         nwheats_rtlos = 0.0
      else
         senrt = rtsnr*plantwt(root_part)
         nwheats_rtlos = senrt
         rootsen = nwheats_rtlos
      endif
!**!  rootsenfr = divide(rootsen, pl_wt(root), 0.0)
!**!  IF(pl_wt(root_part) .GT. 0.0) THEN
!**!           rootsenfr = rootsen / pl_wt(root_part)
      IF(plantwt(root_part) .GT. 0.0) THEN
               rootsenfr = rootsen / plantwt(root_part)
            ELSE
               rootsenfr = 0.0
            ENDIF
      rootnsen =  rootsenfr * pl_nit(root_part) !pl_nit is array
!**!  pl_wt(root_part) = pl_wt(root_part) - rootsen
      plantwt(root_part) = plantwt(root_part) - rootsen
      pl_nit(root_part) = pl_nit(root_part) - rootnsen
      ! JZW add the following for output
      plntp = pl_nit(leaf_part)+pl_nit(lfsheath_part)+pl_nit(stem_part)
     :   + pl_nit(grain_part)
!       WTNCAN      Weight of nitrogen in above ground biomass (stem, leaf, grain), kg N/ha
      WTNCAN = plntp * PLTPOP
      pl_nit_leaf = pl_nit(leaf_part) * PLTPOP !pl_nit_leaf is  WTNLF
      !   g               g            plant
      !---------  = ----------------- *----
      !   m2              plant         m2
      pl_nit_stem = pl_nit(stem_part)* PLTPOP
      pl_nit_grain = pl_nit(grain_part)* PLTPOP
      pl_nit_root = pl_nit(root_part)* PLTPOP
      pl_nit_lfsheath = pl_nit(lfsheath_part)* PLTPOP
      do L=1,nrlayr   ! nwheats layer is now L
         rlv_nw(L) = rlv_nw(L) * (1.0 - rootsenfr)
      enddo
! ------- Add on today's growth -----

!**!  call add_real_array (growt, pl_wt, mxpart)

! JZW replace the following do loop by call add_real_array
      call add_real_array (gro_wt, plantwt, mxpart)
!      do L=1, mxpart
!!**!     pl_wt(L) = pl_wt(L) + gro_wt(L)
!         plantwt(L) = plantwt(L) + gro_wt(L)
!      enddo

!*!   call add_real_array (transwt, pl_wt, mxpart)
      call add_real_array (trans_wt, plantwt, mxpart)
!      do L=1, mxpart
!!**!     pl_wt(L) = pl_wt(L) + trans_wt(L)
!         plantwt(L) = plantwt(L) + trans_wt(L)
!      enddo

      sumcbo(istage) = sumcbo(istage) + carbh
!----------------------------------------------------------------------
!*! Begin WHAPS calculation of stem weight for grain number function
!*! APSIM NWheat subroutine nwheats_gpp_stemwt, called by nwheats_crppr)
!----------------------------------------------------------------------
!*!   call nwheats_gpp_stemwt (g_gpp_stem_wt) ! from nwheats_crppr

!*!   if ((xstage.ge.p_gpp_start_stage)     ! FSR: Not sure why this
!*!  :           .and.                      !  apparently convoluted
!*!  :  (xstage.lt.p_gpp_end_stage)) then   !  construction was used
! cultiva data GPPSS (e.g. 2.0)      gpp_start_stage - Grain per plant: 2=stem elong., 3=end leaf stage at which to start accumo. stem for gpp calc.
! cultiva data GPPES (e.g. 5.0)      gpp_end_stage - Start grainfilling stage at which to end accumulation stem for Grain per plant calc
! xstag_nw    !Non-integer growth stage indicator (NWheats)
      IF ((xstag_nw .ge. GPPSS)            ! GPPSS for p_gpp_start_stage
     &           .and.
     &  (xstag_nw.lt.GPPES)) then          ! GPPES for gpp_end_stage
!*!      IF (istage .eq. grnfil) THEN  ! Not sure where this came from??
! gpp_stem_wt : stem weight used in calc gpp (NWheats)
         gpp_stem_wt = gpp_stem_wt + gro_wt(stem_part) !*! was "growt"

      ELSE
!*!      ! Do nothing.
      ENDIF
!----------------------------------------------------------------------
!*! End WHAPS calculation of stem weight for grain number function
!*!       (from APSIM NWheat subroutine nwheats_gpp_stemwt)
!----------------------------------------------------------------------

           ! --------- calculate daily respiration
cbak   remove the respiration terms as rue is on a net carbon basis
cbak   at a latter date, we could look into including root respiration and
cbak   add the relevant c and n pools to the soil organic matter cycling
!*!   call nwheats_respiration (Cresp)
!*!   call subtract_real_array (Cresp, pl_wt, mxpart)

         ! ---------------- get new plant leaf area from new leaf weight

!*!   pl_la = pl_la + gro_wt(leaf)*nwheats_sla()
      pl_la = pl_la + gro_wt(leaf_part)*sla_new

      lai = (pl_la - sen_la) * PLTPOP / sm2smm
      XHLAI  = LAI  ! XHLAI: Healthy leaf area index used to compute
                    ! transpiration in water balance routine

cbak  plsc(mxleaf) accumulates leaf area in an array for each phylochron
!      interval

!      chp modified 1/8/2014
!      plsc(int(cumph_nw(istage))+2) = plsc(int(cumph_nw(istage))+2)
!    :                           + gro_wt(leaf_part)*sla_new
       i = min(int(cumph_nw(istage))+2,20)
       plsc(i) = plsc(i) + gro_wt(leaf_part)*sla_new

!----------------------------------------------------------------------
!*! Begin WHAPS calculation of leaf senescense
!*! returns the area of leaf that is senesced on a plant (mm^2/plant)
!*! (from APSIM NWheat real function nwheats_lfsen)
!----------------------------------------------------------------------
 ! now calculate the total leaf senescence
          ! due to normal phenological (phasic) development

!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!*! Begin code from real function nwheats_slan, called by
!*! nwheat_crppr:
!*!               slan = nwheats_slan ()
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!*! need to calculate ti before this section
!*! Fraction of a phyllochron interval which occurred as a fraction of today's daily thermal time
!*! same as frlf = dtt/PhInt ?    In DSSAT, TI = DTT/(PHINT*PC) (PC = correction factor)

      if (istage.eq.emergence .or. istage.eq.endjuv) then
         if (cumph_nw(istage).gt.greenlfno+1) then
            lai = (pl_la - sen_la) * PLTPOP * smm2sm ! PLTPOP for plants
            XHLAI  = LAI  ! XHLAI: Healthy leaf area index used to
!                         compute transpiration in water balance routine
            if (sen_la/pl_la .gt. 0.4 .and. lai .lt. 6.0) then
               slan = 0.0
            else
!              chp modified 1/8/2014
!              dyingleaf = cumph_nw(istage) + 2 - (greenlfno+1)
               dyingleaf = min(int(cumph_nw(istage) + 2 - (greenlfno+1))
     &                                                              ,20)
               slan = plsc(dyingleaf)*ti

!                do a bit of housekeeping - really this is not the place to do
!                this!!!!!!
cbak  adjust the green leaf ara of the leaf that is dying

               plsc(dyingleaf) = plsc(dyingleaf) - slan

            endif
         else
            ! to early for senescence
            slan = 0.0
         endif

      else if (istage .eq.endveg) then
         slan = 0.00037 * dtt * stage_gpla

      else if (istage .eq.endear) then
         slan = 0.00075 * dtt * stage_gpla

      else if (istage .eq.grnfil) then
         slan = 2. * sumstgdtt(istage) * dtt/(pgdd(grnfil)**2)
     :             * stage_gpla

      else
         slan = 0.0

      endif

!*!   nwheats_slan = bound (slan, 0.0, pl_la - sen_la)
      slan = MAX (slan, 0.0)
      slan = MIN (slan, pl_la - sen_la)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!*! End code from real function nwheats_slan, called by
!*! nwheat_crppr
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

          ! get senescense from stresses.

      slfw = 2.0 - swdef(photo_nw)/0.8
!*!   slfw = bound (slfw, 1.0, 2.0)
      slfw = MAX (slfw, 1.0)
      slfw = MIN (slfw, 2.0)

      slfn = 2.0 - nfact(4)/0.8
!*!   slfn = bound (slfn, 1.0, 2.0)
      slfn = MAX (slfn, 1.0)
      slfn = MIN (slfn, 2.0)
      vpdf = ALIN (VPD, FVPD, 4, vpd_transp)
      IF ((ISWWAT .EQ. 'Y') .and. (MEEVP .NE. 'Z')) THEN   
!           high temperature factor JZW changed on Mar. 24, 2014, add Tcnpy on May 14, 2014
         ep_nw = sum_real_array (rwu_nw, nrlayr) ! Actual transpiration
!          ES is actual soil evaporation
!         Tcnpy = (-12. * ( ES + ep_nw)/EO + 6.) +Tmax
         Tcnpy = vpdf*(TCSlope * ( ES + ep_nw)/EO + TCInt) +Tmax
      else
!        because EO is not availabe (there is no CALL PET in SPAM.for)
         Tcnpy = vpdf * (TCSlope + TCInt) + Tmax  
      endif
!        Average daily canopy temperature (°C)
         weather % TGROAV = Tcnpy 
         slft = ALIN (SENST, SENSF, 4, Tcnpy)
       Weather % VPD_TRANSP = vpd_transp
       Weather % VPDF = vpdf
!      if (TMAX .gt. 34.) then ! note that this factor is not continuous
!      ! REAL FUNCTION INTERPOLATE (ARRVAR, DEPTH, DS)
!         slft = 4. - (1.-(TMAX - 34.)/2.)
!      else
!         slft = 1.0
!      endif
      sfactor = max (slfw, slfn, slft)

cbak increase slan to account for stresses

      leafsen = slan * sfactor

      ! More Housekeeping needed here.!!!!!!!!!!!

cbak  adjust the plsc leaf area array to reflect leaf senesence

!     chp modified 1/8/2014
!     do I = 1, cumph_nw(istage)+2
      do I = 1, min(int(cumph_nw(istage))+2, 20)
         plsc(I) = plsc(I) - leafsen/real(greenlfno)
!*!      plsc(I) = l_bound (plsc(I), 0.0)
         plsc(I) = MAX (plsc(I), 0.0)
      enddo

!*!   sen_la = nwheats_lfsen ()
!*!   nwheats_lfsen = bound (sen_la + leafsen, 0.0, pl_la)
      sen_la = MAX (sen_la + leafsen, 0.0)
      sen_la = MIN (sen_la, pl_la)

      if (istage.eq.endveg) then
          lai = (pl_la - sen_la) *PLTPOP/sm2smm

          XHLAI  = LAI  ! XHLAI: Healthy leaf area index used to compute
                        ! transpiration in water balance routine, m2/m2
      endif
!----------------------------------------------------------------------
!*! End WHAPS calculation of leaf senescense
!*! (from APSIM NWheat real function nwheats_lfsen)
!----------------------------------------------------------------------

cjh quick fix for maturity stage

!*!   call nwheats_crtot ()

!*!   call pop_routine (myname)
!*!   return
!*!   end

!----------------------------------------------------------------------
!*! End  WHAPS code from APSIM NWheat subroutine nwheats_crppr
!======================================================================
      call nwheats_add_sen_roots (
     &  dlayr_nw, rootsen, rtdep_nw,                             !Input
     &  deepest_layer, root_array)                              !Output
!-----------------------------------------------------------------------
!                         DYNAMIC = OUTPUT
!-----------------------------------------------------------------------
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
       WTNVEG = WTNVEG
!-----------------------------------------------------------------------
!                         DYNAMIC = SEASEND
!-----------------------------------------------------------------------
      ELSEIF(DYNAMIC .EQ. SEASEND) THEN
!*!   IF (DYNAMIC .EQ. SEASEND) THEN
          !IF (IHARI .NE. 'M') then, seasend is not maturiy day
        ! !calculate Total grain N uptake, kg N/ha, i.e. at maturity
        !  GNUP = pl_nit(grain_part) * PLTPOP * 10
        !!  kg       g        kg       plants   10000m2
        !!  --- = ---------*---------*--------*---------
        !!  ha     plant     1000g        m2        ha
        !  XGNP = PCNGRN
          IF (IDETR .EQ. 'Z') THEN
         !Close daily output files.
         CLOSE (NOUTXL)
         ENDIF

      ENDIF !(DYNAMIC)
      if (YRDOY .EQ. MDATE) then
        !calculate Total grain N uptake, kg N/ha, i.e. at maturity
          GNUP = pl_nit(grain_part) * PLTPOP * 10
        !  kg       g        kg       plants   10000m2
        !  --- = ---------*---------*--------*---------
        !  ha     plant     1000g        m2        ha
          XGNP = PCNGRN
      endif

          IF (istage .LT. 7) THEN
              RSTAGE = istage           !R-stage
          ELSE
             RSTAGE = 0
          ENDIF

          SEEDNO = gpp                ! no. of grains per plant
          SLA    = sla_new / 100      ! mm2/g  to  cm2/g
          XLAI   = LAI                !Leaf area index, m2/m2
          XHLAI  = LAI  ! XHLAI: Healthy leaf area index used to compute
                        ! transpiration in water balance routine, m2/m2
          RTWT   = plantwt(root_part) ! * PLTPOP in TF_OPGROW   g/plant
          WTLF   = plantwt(leaf_part) * PLTPOP       !Leaf weight, g/m2
          PODWT  = plantwt(lfsheath_part) * PLTPOP !sheath weight, g/m2
          STMWTO = plantwt(stem_part) * PLTPOP    !stem weight, g/m2
          SDWT   = plantwt(grain_part) * PLTPOP    !grain weight, g/m2
          YIELD = SDWT * 10  ! to pass yield to Overview & Summary.out
          tpsm = tiln * PLTPOP
          SHELPC = tpsm               ! Tiller number (no/m2)
          TOPWT  = WTLF+PODWT+STMWTO+SDWT
          if (plantwt(grain_part)  .gt. 0.) then
             PCNGRN = pl_nit(grain_part)* 100./plantwt(grain_part)
          else
             PCNGRN = 0.
          Endif
          if (plantwt(stem_part) .gt. 0. ) then
             PCNVEG = (pl_nit(stem_part)+pl_nit(leaf_part)+
     %             pl_nit(lfsheath_part))/(plantwt(lfsheath_part)+
     %             plantwt(stem_part)+plantwt(leaf_part))*100.
          else
             PCNVEG = 0.
          endif
          if (plantwt(leaf_part) .GT. 0.) then
            PCNL = pl_nit(leaf_part)*100./plantwt(leaf_part)
          else
            PCNL = 0.
          endif
          if (plantwt(stem_part) .GT. 0.) then
            PCNST = pl_nit(stem_part)*100./plantwt(stem_part)
          else
            PCNST = 0.
          endif
          if (plantwt(root_part) .GT. 0.) then
            PCNRT = pl_nit(root_part)*100./plantwt(root_part)
          else
            PCNRT = 0.
          endif
          IF (TOPWT .GT. 0. .AND. SDWT .GE. 0.) THEN
              HI = SDWT/TOPWT
          ELSE
              HI = 0.
          ENDIF

!----------------------------------------------------------------------
!     Format Strings
!----------------------------------------------------------------------

      return
      END SUBROUTINE TF_GROSUB


!----------------------------------------------------------------------
!                         DEFINITIONS
!----------------------------------------------------------------------

! AGEFAC      Nitrogen stress factor affecting expansion (0-1)
! ANO3        Total extractable nitrate N in soil profile (kg N/ha)
! ANH4        Total extractable ammonium N in soil profile (kg N/ha)
! APTNUP      Nitrogen in stover (above ground biomass) kg N/ha
! AREALF      Leaf area index, cm2 leaf/m2 ground
! ASMDOT      Reduction in photosynthesis due to pests, %/day
! BIOMAS      Above ground biomass, g/m2
! CANHT       Canopy height, cm (not used)
! CANNAA      Stover N at anthesis, g N/M2
! CANWAA      Canopy weight at anthesis, g/m2
! CANWA       Canopy width, cm (not used)
! carbh       Actual dry matter production for the day (g/plant)  (NWheat output)
! CARBO       Daily biomass production, g/plant/day
! CARBOT      Consecutive days where CARBO is less than .001 before plant matures due to stress
! CMAT        Counts consecutive days that CARBO is less than 0.001
! CNSD1       Cumulative nitrogen deficit factor on photosynthesis in
!              each stage used to compute avg. stress during stage
! CNSD2       Cumulative nitrogen deficit on growth in each stage used
!              to compute avg. stress during stage
! cumph_nw    Cumulative phyllochron intervals or fully expanded leaves
! CO2X(10)    CO2 effect on photosynthesis, X axis is CO2 level, ppm
! CO2Y(10)    CO2 effect on photosynthesis, Y axis is relative effect
! CO2         Atmospheric CO2, ppm
! CSD1        Cumulative water stress on photosynthesis during growth stage
! CSD2        Cumulative water stress on growth during growth stage
! CURV        Function to interpolate a value from a series of curves
! DLAYR(L)    Soil thickness in layer L (cm)
! DM          Total above ground biomass, kg/ha
! DOY         Day of year
! DTT         Growing degree days today, C
! DYNAMIC     Main control variable to tell each module which section of code to run
! EARS        Ears per m2
! EARWT       Ear weight, g/ear JIL: should be g/plant
! ECONO       Ecotype number for the variety
! EMAT        Flag used in grosub to determine if relative grain fill is below 0.1 for 2 consecutive days
! EOP         Potential plant transpiration, mm/d
! EP1         Potential plant transpiration, cm/d
! SEASEND     Program control variable to execute code to complet model run (value=6)
! fr_intc_radn Fraction of intercepted radiation ? Nwheats optional input.
! frlf        New fraction of oldest expanding leaf (NWheats)
! FSLFW       Fraction of leaf area senesced due to 100% water stress, 1/day
! FSLFN       Fraction of leaf area senesced due to 100% nitrogen stress, 1/day
! G3          Potential kernel growth rate mg/kernel/day
! GNP         Nitrogen concentration in new grain growth, gN/g dry matter
! GNUP        Total grain N uptake, kg N/ha
! GPP         Grain number per plant, grains/plant
! gpp         NWheat version of grain number
! GPSM        Grain numbers, grains/m2
! GRAINN      Grain nitrogen content, g N/plant
! GRF         Fraction of today's carbon allocated to above ground biomass
! GRNO        Coefficient of kernel number per stem weight at the beginning of
!              grain filling [kernels (g stem)-1]   (NWheat)
! GRNWT       Grain weight, g/plant
! GROEAR      Daily growth of the ear - g
! GROGRN      Daily growth of the grain - g
! GROLF       Leaf growth rate, g/plant/day
! grolfsh     Carbohydrate demand to grow new leaf sheath (g/plant)
! GRORT       Root growth rate, g/plant/day
! GROSTM      Stem growth rate, g/plant/day
! grpp        Grain number per plant, grains/plant (NWheat)
! HI          Ratio of seed weight (SDWT) to weight of above-ground portion of
!              plant (TOPWT) (g[seed] / g[tops])
! HIP         Ratio of pod weight (PODWT) to weight of above-ground portion of
!              plant (TOPWT) (g[pods] / g[tops])
! I           Counter
! ICOLD       Cumulative number of days when TMIN is less than 6 C
! ICSDUR      Calendar day accumulator for each growth stage
! IDETO*1     Screen output switch, (Y/N)
! INTEGR      Program control variable to execute code to integrate daily rate variables (value=4)
! ISTAGE      Growth stage (integer)
! ISWNIT*1    Nitrogen balance switch (Y/N)
! ISWPHO      Phosphorus simulation (stress) switch
! ISWPOT      Potassium simulation (stress) switch
! ISWWAT      Soil water balance on/off switch (Y for yes, N for no)
! KG2PPM(L)   Factor that convergs mg elemental N/kg soil to kg N/ha for soil layer L
! L           Index counter
! lai         Leaf area index, m2/m2
! LAIDOT      Leaf area consumed by pests, cm2 leaf area/m2/day
! leafgr      Leaf growth before emergence (g/plant) (nwheats.for)
! LEAFNO      Number of oldest leaf per plant (same as XN)
! LEAFNOE     Number of oldest leaf at emergence
! lfipar      Radiation intercepted by leafs  (mj/m2)  (nwheats.for)
! lfshdmd     Carbohydrate demand to grow new leaf sheath (g/plant)
! LFWT        Leaf weight, g/plant
! LFWTE       Leaf weight at emergence, g/plant
! LIFAC       Effect of row spacing and plant population on light interception
! LL(L)       Lower limit of plant extractable water for soil layer L, cm3/cm3
! MAXLAI      Maximum leaf area index, m2/m2
! MXFIL       Maximum kernal growth rate:  Values between 2.0 and 2.5
! NDEF3       Nitrogen stress factor affecting grains per plant (0-1)
! NFAC        Nitrogen stress factor based on actual and critical nitrogen content in vegetative tisue
! NH4(L)      Ammonium in soil layer L, mg elemental N/kg soil
! NLAYR       Number of soil layer
! NO3(L)      Nitrate in soil layer L (mg elemental N/kg soil)
! NOMOB	      Fraction of accumulated stem weight that is not mobile (was fr_remob_part in nwheats.for)
! NOUTDO      File number
! NPOOL       Total plant N available for translocation to grain (g/plant)
! NPOOL1      Tops N available for translocation to grain (g/plant)
! NPOOL2      Root N available for translocation to grain (g/plant)
! NSDR        Plant N supply/demand ratio used to modify grain N content
! NSINK       Demand for N associated with grain filling (g/plant/day)
! NSTRES      Nitrogen stress factor affecting growth (0-1)
! NWSD        No. of consecutive days with severe water stress
! optfr       Fraction of optimum conditions from stress. (0-1)  (NWheat)
! OUTPUT      Program control variable to output state and rate variables to output file (value=5)
! P5          GDD from silking to physiological maturity, C
! PAR         Daily photosynthetically active radiation, calculated as half
! PARSR       Conversion of solar radiation to PAR, (MJ PAR/m2/d)/(MJ SRAD/m2/d)
! PC          Used to compute fraction of phyllochron interval occurring today
! PCARB       Potential dry matter production under optimum water, nitrogen and temperature, g/plant
! pcarbo      Potential dry matter (carbohydrate) production (g/plant)  (Nwheat)
! PCNGRN      Percent nitrogen in grain, %
! PCNL        Percent nitrogen in leaves, %
! PCNRT       Percent nitrogen in roots, %
! PCNST       Percent nitrogen in stems, %
! PCNVEG      Percent nitrogen in vegetative tissue (leaf and stem), kg N/ha
! PCO2        Effect of CO2 on plant growth rate
! PDWI        Potential increment in new shoot growth, g/plant
! SLPF        Relative reduction in growth due to poor soil fertility (0-1.0) that is
!             not related to nitrogen.
! PGRORT      Potential increment in new root growth, g/plant
! PHINT       Phyllochron interval. Number of GDD required for new leaf emergence, C.
! pl_la       Plant leaf area, mm2/plant (from nwheats.for)
! PLA         Plant leaf area, cm2/plant
! PLAE        Plant leaf area at emergence, cm2/plant
! PLAG        Leaf area growth, cm2/plant
! plantwt()   Array of plant component weights, g/plant (root_part, leaf_part, stem_part, etc)
! pl_nit_grain It is WTNSD
! PLAS        The rate of senescence of leaf area on one plant - sq. cm/day
! PLTPOP      Plant population, plants/m2
! PODWT       Leaf sheath dry weight, including C and N (g[leaf sheath]/m2[ground])
! PORMIN      Minimum pore space volume required for supplying oxygen to roots for optimum growth and function (0-100%)
! PPLTD       Percent of plants destroyed today, %/m2/d
! PRFTC not used (for ceres?)       Array containing base, optimum and maximum temperature for function reducing photosynthesis due to temperature.
! PRFT        Photosynthetic reduction factor for low and high temperatures  (DSSAT and Nwheat)
! ptcarb      Potential dry matter (carbohydrate) production (g/plant)  (NWheat output)
! PTF         Ratio of above ground biomass to total biomass
! radfr       Fraction of short wave radiation that is intercepted by leaves (calculated in nwheats.for)
! RANC        Root actual N concentration, g N/g root
! RATE        Program control variable to execute code to compute daily rate variables (value=3)
! RANCE       Root nitrogen concentration at emergence, g N/g root dry weight
! RCNP        Root critical nitrogen concentration, g N/g root dry weight
! RGFIL  not used (for ceres?)      Array containing base, optimum and maximum temperature to compute RGFILL function
! RGFILL      Rate of grain fill - mg/day
! RLV(L)      Root length density for soil layer L, cm root/cm2 soil
! RLWR        Root length weight ratio
! RMNC        Root minimum nitrogen concentration (g N/g root dry weight)
! RNLAB
! RNOUT
! rootgr      root growth before emergence (g/plant) (nwheats.for)
! ROOTN       Root nitrogen content, g N/plant
! rrgfill     Relative (0-1) rate of grainfill for the day (nwheats.for)
! RSGR        Relative seed growth rate below which early maturity may occur after RSGRT days
! RSGRT       Number of consecutive days where minimum relative seed growth rate (RSGR)is not
!              achieved. This results in early maturity due to slow seed growth. days
! RSTAGE      ISTAGE as a real number for output
! RTWT        Root weight, g/plant
! RTWTE       Root weight at emergence, g/plant
! RTWTO       Root weight, g/m2
! RUE         Radiation use efficiency, g CH2O/MJ Par
! ROWSPC      Row spacing, cm
! RWUEP1      Factor to modify water stress for cell expansion (in species file), mm/day
! RWUMX       Maximum water uptake per unit root length, constrained by soil
!             water (cm3[water] / cm [root])
! SAT(L)      Saturated water holding capacity for soil layer L, cm3/cm3
! SATFAC      Watterlogging stress factor (0-1.0)
! SDWT        Seed weight, g/m2
! SEEDRV      Carbohydrate reserve in seed, g/plant
! SEEDRVE     Carbohydrate reserve in seed at emergence, g/plant
! seedrvNW    Carbohydrate reserve in seed, g/plant (WHAPS-nwheats version)
! SENLA       Normal leaf senescence today, cm2/plant
! SFAC        Drought stress factor for grain nitrogen concentration
! SHF(L)      Relative root distribution in soil layer L (0-1)
! SHELPC      Shelling percent
! SI1(6)      Water stress during a growth stage used for output
! SI2(6)      Water stress during a growth stage used for output
! SI3(6)      Nitrogen stress during a growth stage used for output
! SI4(6)      Nitrogen stress during a growth stage used for output
! SKERWT      Weight per kernel, g/kernel
! SLA         Specific leaf area, cm2/g
! SLAN        Normal leaf senescence since emergence, cm2/plant
! sla_new     Calculated specific leaf area of new growth (mm2/g)
! SLAP1       ratio of leaf area to mass at emergence (CUL parameter)
! SLAP2       ratio of leaf area to mass at end of leaf growth (CUL parameter)
! SLFC        Leaf senescence factor due to competition for light(0-1)
! SLFN        Leaf senescence factor due to nitrogen stress (0-1)
! SLFP        Leaf senescence factor due to phosphorus stress (0-1)
! SLFT        Leaf senescence factor due to temperature (0-1)
! SLFW        Leaf senescence factor due to water sterss (0-1)
! SNH4(L)     Ammonium nitrogen in soil layer L, kg N/ha
! SNO3(L)     Nitrate content in soil layer L, kg N/ha
! SRAD        Daily solar radiation, MJ/M2/day
! stemgr      Stem growth before emergence (g/plant) (WHAPS-nwheats.for)
! STGDOY(20)  Year and day of year that a growth stage occurred on
! STMMX       Potential final dry weight of a single tiller (excluding grain) (g stem-1)
!             values 1.0 - 3.0 (NWheat cultivar parameter)
! STMWT       Stem weight, g/plant
! STMWTE      Stem weight at emergence, g/plant
! STOVER      Stover weight (leaf+stem+lfsheath), kg/ha
! STOVN       Nitrogen content in stover, g N/plant
! STOVWT      Stover weight (Stem + leaf ++lfsheath), g/plant
! SUMDTT      Sum of GDD for a given stage, C
! sumstgdtt(20) Sum of GDD for a given istage, deg days
!              (substituted for sumdtt from nwheats.for, since DSSAT uses SUMDTT already)
! SUMEX       Sum over all layers of water excess factor times depth
!              times root length density
! SUMP        Cumulative plant growth during ISTAGE 4, g/plant
! SUMRL       Sum of root length density (integral over depth)
! SW(L)       Soil water content in layer L, cm3 water/cm3 soil
! SWEXF       Soil water excess factor (0-1.0)
! SWFAC       Soil water stress effect on growth (0-1), 1 is no stress, 0 is full
! SWIDOT      Seed loss due to pests, g seed/m2/day
! SWMAX       Maximum stem weight (set in phasei) (move to grosub)
! SWMIN       Minimum value stem weight can reach durign linear grain filling g/plant
! TABEX       Table lookup function
! TANCE       Nitrogen content in above ground biomass at emergence, g N/g dry weight
! TANC        Nitrogen content in above ground biomass, g N/g dry weight
! TAVGD       Average temperature during daylight hours, C
! TCNP        Critical nitrogen concentration in tops, g N/g dry weight
! TEMPM       Mean daily temperature (°C)
! TFAC        Temperature stress factor for grain nitrogen concentration
! TI          Fraction of a phyllochron interval which occurred as a fraction of today's daily thermal time
! TLNO        Total number of leaves that the plant produces
! TMAX        Maximum daily temperture, C
! TMNC        Plant top minimum N concentration g N/g dry matter
! TNLAB       Total potential daily plant water uptake, cm/d
! TMIN        Minimum daily temperature
! topsbiom    Total aboveground biomass: grain + leaf + lfsheath + stem (g/plant) (WHAPS-nwheats.for)
! TOTNUP      Total shoot N uptake at maturity, kg N/ha
! TRNU        Total potential root nitrogen uptake, kg N/ha, converted to g N/pl
! TRWUP
! TSS(L)      Total days soil layer L has been saturated in excess of PORMIN
! TURFAC      Soil water stress effect on expansion (0-1), 1 is no stress, 0 is full stress
! UNH4(L)     Plant uptake of ammonium from layer (kg N/ha/day)
! UNO3(L)     Plant uptake of nitrate from a layer (kg N/ha/day)
! VMNC        Plant vegetative minimum nitrogen concentration, g N/g plant
! VANC        Plant vegetative actual N concentration, g N/g plant
! WLIDOT      Leaf loss due to pests, g/m2/day
! VSEN        sensitivity to vernalisation  (scale 1-5; NWheat)
! VSTAGE      Vegetative growth stage (real number) used for output
! WRIDOT      Root loss due to pests, g root/m2/day
! WSIDOT      Stem loss due to pests, g stem/m2/day


! WTNCAN      Weight of nitrogen in above ground biomass (stem, leaf, grain), kg N/ha
! WTNLF       Weight of nitrogen in leaf tissue, kg N/ha
! WTNSD       pl_nit_grain, whic is Weight of nitrogen in seed, g[N] / m2[ground]
! WTNST       Weight of nitrogen in stem tissue, kg N/ha
! WTNUP       Total N uptake, g/m2, WTNUP = cumpnup / 10.0 ,
! WTNVEG      Weight of nitrogen in vegetative tissue, kg N/ha
! XANC        Nitrogen concentration in above ground biomass %
! XHLAI       Healthy leaf area index used to compute transpiration in water balance routine, m2/m2
! XLFWT       New leaf weight today, g/plant
! XGNP        Nitrogen content of grain, %
! XN          Number of oldest expanding leaf
! XNF         Modified nitrogen factor based on critical N concentration in vegetative biomass
! XNTI        Number of leaves at tassel initiation
! XSTAGE      Non-integer growth stage indicator
! YIELD       Yield in kg/ha at 0% moisture content
! YIELDB      Yield, bu/ac at 0% moisture content
! YRDOY       Year and day of year
!///////////////////

! APSIM: nwheats_fac is convert from kg/ha to ppm (mg/kg)
!        nwheats_fac = 100.0 / bd(layer)*dlayr_nw(layer)
! DSSAT: kg2ppm.
!        In insoil.for KG2PPM(L) = 1.0/(BD(L) * 0.1 * DLAYR(L))
! APSIM: no3ppm is in ppm, sno3 is in kg/ha
!        no3ppm = sno3(L) * nwheats_fac (L, SOILPROP)
! DSSAT: NO3(L) is in ppm, SNO3(L) is in kg/ha
!        SNO3(L) = NO3(L) / KG2PPM(L)
! APSIM's no3ppm is DSSAT NO3
! no3ppm is only be used for calculate root length density.
! TF_GROSUB calculate Nitrogen uptake and passed to plant.for
! SOIL output no3 and pass to plant

!APSIM:
!      nitmn    nitrogen minimum level (g/part)
!      ndmd     plant nitrogen demand
!      fr_part   fraction of nitrogen to use (0-1) for plant part
!      n_demand  total nitrogen demand (g/plant)
!      n_supply  total nitrogen supply (g/plant)
!      ptnup    nitrogen uptake from soil (g/plant)
!   **   plntp    plant nitrogen of tops (g/plant)
!      xanc     grain nitrogen %
!      nupt is nitrogen uptake from soil by veg. not realy used
!      gnuptk is actual grain N uptake (g/plant) intermediate data
!      GNUP        Total grain N uptake, kg N/ha, intermediate data
!    ** snup_no3 (NL) (i.e. snuptk_no3)  It is snuptk_no3, actual (observed or calculated) plant N uptake from NO3 in each layer (kg/ha)
!      pntrans(mxpart) (i.e. pnout(mxpart) ) N translocated to grain.plant N taken out from plant parts ()
!***** cumpnup is [snup_no3 (NL)+snup_nh4(NL)] sum over layer and sum over day
!      pl_nit(mxpart)  g/plant
!plant nitrogen of tops (g/plant):plntp = pl_nit(leaf) + pl_nit(lfsheath) + pl_nit(stem) + pl_nit(grain)
!      tanc is plantNitrogen/plant weight
!      plantn (mxpart) (i.e. pl_nit) plant part nitrogen
! DSSAT: UNO3     Uptake of NO3 from soil (interim value) (kg N/ha)
!       WTNUP     Cumulative N uptake (g[N] / m2)
! NO3 or SNO3 is supply, and used for N demand
!//////////////////
! Bugs:
!1990 061 dap = 82, pl_nit = inifinity due to pnup = iniinity

! pgro(part) = pcarbo* (gro_wt(part)/ carbo) where carbo = 0.0
! fr_part = pndem(part) / n_demand = inifinity
