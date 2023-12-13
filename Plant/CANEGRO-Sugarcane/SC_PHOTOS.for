           
C********************END OF FUNCTION ROUTINES***************************

       SUBROUTINE SC_PHOTOS (
c     [I] Not sure
     - CWSI,
c     [I] Photosynthesis algorithm choice number (PRN file)
     - MDL,
c     [I] An indicator of anaerobic stress
     - ANAERF,
c     [I] Day of year
c     - IDOY,
c     [I] Latitude
!    - ALAT,
c     [I] A coefficient for solar radiation ?
c     - SCV,
c     [I] Not sure
     - IW,
c     [I] Allow neg growth? PRN file
     - NEGGRO,
c     [O] Change in dry weight today (i.e. photosynthate)
     - DWDT,
c     [O] Ken Boote's photos. technique: Light interception
c     - bootli,
c     [I] Important for choice of photos. tech.
     - coderd,
c     [I] The height of the canopy today
!    - canhgt,
c     [I] Rowspacing
!    - rowspc,
c     [I] Quantity of runoff today?
     - RUNOFF, 
c     [I] Allow lodging? (PRN file)
     - allowlodg,
c     [I] Sunlit LAI fraction
!    & F_SL,         
c     [O] Effect of lodging on light interception
     - LODGE_LI, 
c     [IO] The partitioning object
     - Part, 
c     [IO] The growth object
     - Growth, 
c     [I] The Climate object
     - Climate,
c     [I] soil photosythesis fertility factor (CHP 2/14/2011)
     - SLPF, 
c     [O] Output object
     - Out,
c     [I] Simulation switches
     - ISWITCH, 
c     [I] The DSSAT control variable
     -Control, 
c     [I] Atmospheric CO2 concentration
     & CO2,
c     [I] yesterday's mass partitioned to sucrose
     & DSUCyest,
c     [O] Average Rm fraction
     & RM_AVG, TMEAN_AVG)

!     2023-01-26 chp removed unused variables from argument list: 
!         ALAT, canhgt, rowspc, F_SL

c     Use the Canegro modules:
c     ::::::::::::::::::::::::
      USE CNG_ModuleDefs
c     Use DSSAT modules
c     :::::::::::::::::
      USE ModuleDefs
      IMPLICIT NONE
      EXTERNAL GET_SPECIES_COEFF, GET_CULTIVAR_COEFF, D_TT4, TABEX
      SAVE

c     Declare composite variables:
c     ::::::::::::::::::::::::::::
c     The DSSAT simulation control object
c     :::::::::::::::::::::::::::::::::::
      Type (SwitchType) ISWITCH

c     The Climate 'object'
c     :::::::::::::::::::
      TYPE(ClimateType) Climate

c     The Partitioning object:
c     :::::::::::::::::::::::
      TYPE(PartType) Part

c     The Growth object
c     :::::::::::::::::
      TYPE(GrothType) Growth

c     The output object:
c     ::::::::::::::::::
      TYPE (OutType) Out

c     The DSSAT control object:
c     :::::::::::::::::::::::::
      TYPE (ControlType) Control

c     Previously undeclared variables:
c     ::::::::::::::::::::::::::::::::
c     See variables.mdb
c     :::::::::::::::::
c         Climate vars:
c         :::::::::::::
          REAL     SOLRAD
          REAL     TEMPMN
          REAL     TEMPMX
          REAL     WINDSP

c         Growth vars:
c         ::::::::::::
          REAL     GRORT
          REAL     LAI
          REAL     LI

c         Part vars:
c         ::::::::::
          REAL     AERLDM
          REAL     FDWDT
          REAL     FLODGE
          REAL     PARCE
          REAL     RESPCF
          REAL     ROOTF
          REAL     STKDM
          REAL     STKWM
          REAL     TDMAS

c         Parameters
c         ::::::::::
!          REAL     ALAT
          REAL     ANAERF
c          REAL     BOOTLI
!          REAL     CANHGT
          REAL     CODERD
          REAL     CWSI
          REAL     DWDT
          INTEGER  IDOY
          INTEGER  IW
          INTEGER  MDL
          INTEGER  NEGGRO
       
!          REAL     ROWSPC
          REAL     RUNOFF
          REAL     SLPF   !CHP 2/14/2011
c          REAL     SCV
c     ::::::::::::::::::::::::::::::::

c     Local variables:
c     ::::::::::::::::


          REAL     GROSSP
          REAL     LODGE
          REAL     OLDFLODGE
          REAL     PAR
c         MJ, 2007-05-15: Respiration coeff
          REAL     RESPQ10
c         Conversion coeff:
          REAL     Respcon
          REAL     RESPGCF

c      Maintenance respiration fraction (t/t/d),
c      Sum of Rm fractions in this run (t/t)
       REAL RMFRAC, RMSUM
c      Sum of mean temperature on days that RMSUM was calculated
       REAL TMEAN_SUM
c      Number of days this season that Rm has been calculated
       INTEGER RMCOUNT
c      Average RM FRACtion and TMEAN so far this sim run
       REAL, INTENT(OUT) :: RM_AVG, TMEAN_AVG


c         For reading species/cult coeffs:
          LOGICAL SPE_ERR

c         Maximum photosynthetic conversion efficiency
c         Cultivar coeff
c         Units: g/MJ (grams of photosynthate per MJ solar radiation (per sq m??))
          REAL MX_PAR_CONV
          
c      Singels, Hoffman 2017: Assuming that leaf-level photosynthesis rate measurements,
c      taken relative to NCo376, can only be scaled up to canopy level for sunlit leaves.
c      Cultivar differences with shaded leaves are likely to be smaller.  So, NCo376 
c      MaxPARCE value is used for shaded leaves and the actual cultivar value is used
c      for sunlit leaves only.
c      The separation between sunlit and shaded leaves is based on a calculation from: 
c      Monteith and Unsworth, 2013.
c      :::::::::::::::
c      Sunlit leaf fraction
!      REAL, INTENT(IN) :: F_SL
c      Delta canopy- and leaf-level PARCE       
!      REAL D_PRC_OC, D_PRC_OL
c      Sunlit and shaded fraction efficiencies
c      Calculated from: de Silva and de Costa, 2009
       REAL EPS_SL, EPS_SH       
c      Reference PARCE for NCo376 [Species Parameter]
!      REAL PARCEMX376
c      Revised MAX_PAR_CONV equivalent
!      REAL NMX_PAR_CONV
c      :::::::::::::::


c         Base temperature for photosynthesis (plant does not 
c         photosynthesise if ambient temperature below this)
          REAL TBasePhotos


c     LODGING VARIABLES: 
c     ::::::::::::::::::::::::::::::::::    
c         aerial mass effect on lodging:
          REAL lodgeam
c         soil water effect on lodging:
	    REAL lodgeswc
c         wind effect on lodging
          REAL lodgewind
c         Allow lodging? (1 = yes, 0 = no)
          INTEGER allowlodg    
c         Severity of lodging (calculated here)
          REAL LODGE_LI
          REAL LG_RATING, LG_AMBASE, LG_AMRANGE, LG_GP_REDUC
c         Max reduction in fractional interception due to lodging
          REAL LG_MAX_RATING, LG_CRIT_WIND
          REAL PRECIPM
c         For reading cultivar values:
          LOGICAL C_ERR

c     MJ, July 2011.  CO2 'fertilisation effect'
c     ::::::::::::::::::::::::::::::::::::::::::          
c     Array of CO2 concentrations (ppm)
      REAL  CO2X(10)    
c     Array of corresponding relative photosynthesis rates
      REAL  CO2Y(10)    
c     Atmospheric CO2 content (ppm)
      REAL, INTENT(IN) ::  CO2            
c     Linear interpolation function
c     (defined in UTILS.FOR, provided by CSM)
      REAL  TABEX
c     Loop counter
      INTEGER I
c     Short name for species coeff name:
      CHARACTER*10 CO2_READ
c     Relative photosynthesis rate:
      REAL PCO2


c     Modification to calculation of delta thermal time to determine a
c     temperature effectiveness index for photosynthesis
c     based on ASA 2013 work:
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     Temperature effectiveness index (0-1) for photosynthesis
      REAL  EFFTEMPC
c     4-point thermal time function
      REAL D_TT4
c     Base, optimal start, optimal end, and final temperatures for photosynthesis
      REAL TOpt1, TOpt2, TFinalPH
c     Mean daily temperature (°C)
      REAL TMEAN

c     Yesterday's value of delta mass (t/ha/d)
      REAL DWDTyest
c     ...and sucrose mass:
c     t/ha/d
      REAL, INTENT(IN) :: DSUCyest

c     Growth respiration rate coefficient (g/g)
      REAL Rg_RESPCF
c     Daily growth, maintenance and total respiration rates (t/ha/d)
      REAL Rg, Rm, Rtot

c     Water/nutrient-stressed gross photosynthesis rate (t/ha/d)
      REAL PG_STRESSED
      
c     Effective temperature / thermal time for respiration (°Cd)
      REAL RmEffTT

c     Maintenance respiration rates (t/ha/d) for roots,
      REAL Rm_ROOTS
c     ...green leaves,
      REAL Rm_GREENLF
c     ...and sucrose
      REAL Rm_SUCROSE

c     Reference maintenance respiration rates (g/g) for roots,
      REAL RESPCN_RTS
c     ...green leaves,
      REAL RESPCN_LVS
c     ...and sucrose
      REAL RESPCN_SUC

c     Genetic coefficients:
      REAL TBaseRESP, TOptRESP, TFinRESP

c     Leftover respiration that could not be substracted from photosynthesis 
c     the previous day, carried over to today (t/ha/d)
c     (to avoid negative biomass increment)
      REAL Rleftovers

!         Unit number for output
!          INTEGER SCLUN   !CHP

c     ::::::::::::::::::::::::::::::::::

      PARAMETER(Respcon = 0.0033871)


c     *****************************************************************

c     Copy values from 'objects' to local variables:
c     ::::::::::::::::::::::::::::::::::::::::::::::
c         Climate vars:
c         :::::::::::::
          SOLRAD = Climate%SOLRAD
          TEMPMN = Climate%TEMPMN
          TEMPMX = Climate%TEMPMX
          WINDSP = Climate%WINDSP

c         Growth vars:
c         ::::::::::::
          GRORT  = Growth%GRORT
          LAI    = Growth%LAI
          LI     = Growth%LI   

c         Part vars:
c         ::::::::::
          AERLDM = Part%AERLDM
          FDWDT  = Part%FDWDT
          FLODGE = Part%FLODGE
          PARCE  = Part%PARCE
          RESPCF = Part%RESPCF
          ROOTF  = Part%ROOTF
          STKDM  = Part%STKDM
          STKWM  = Part%STKWM
          TDMAS  = Part%TDMAS

      IF (CONTROL%DYNAMIC .EQ. SEASINIT) THEN
c         :::::::::::::::::::::::::::::::::::

c     Cultivar / species coeff:
c     Species coefficients...
c     ::::::::::::::::::::::::
         
c          SCV    = 0.2
c         # remove SCV completely!
c         ARE THESE ACTUALLY USED???????????????????????????????????
c          CALL GET_SPECIES_COEFF(SCV,    'SCV',    Control, SPE_ERR)

c         Respiration coefficient: 
c         Increase in respiration rate per 10 C rise in temperature
          RESPQ10 = 1.68
          CALL GET_SPECIES_COEFF(RESPQ10,    'RespQ10', 
     &                           Control, SPE_ERR)


c     Ampthor, 2000
!      Rg_RESPCF = 0.19  ! 0.33 from Liu & Bull / Thornley and Johnson
c         
          ! RESPGCF  = .242
          RESPGCF  = 0.19
          CALL GET_SPECIES_COEFF(RESPGCF,    'RESPGCF', 
     &                           Control, SPE_ERR)
          Rg_RESPCF = RESPGCF

          ! WRITE(*, '(A, F8.3)') 'Rg_RESPCF is ', Rg_RESPCF

c     :::::::::::::::::::::::::

c     Get lodging values from cultivar file:
c     ::::::::::::::::::::::::::::::::::::::
c         First set default values:
          LG_RATING   =   8.
          LG_AMBASE   = 220.
          LG_AMRANGE  =  30.
          LG_GP_REDUC =   0.28

c         Then attempt to read from file:
c         :::::::::::::::::::::::::::::::
c         Lodge rating
c          CALL GET_CULTIVAR_COEFF(LG_RATING, 'LG_RATING', 
c     &                                         Control, C_ERR)

c         Aerial mass at which lodging starts:
          CALL GET_CULTIVAR_COEFF(LG_AMBASE, 'LG_AMBASE', 
     &                                         Control, C_ERR)

c         Range in Aerial mass in which lodging occurs
          CALL GET_CULTIVAR_COEFF(LG_AMRANGE, 'LG_AMRANGE', 
     &                                         Control, C_ERR)

c         Max photosynthesis reduction of fully-lodged cane
          CALL GET_CULTIVAR_COEFF(LG_GP_REDUC, 'LG_GP_REDUC', 
     &                                         Control, C_ERR)

c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     Now get lodging species variables:
c     ::::::::::::::::::::::::::::::::::
c         Set default values
          LG_MAX_RATING = 8.
          LG_CRIT_WIND  = 200.

c         Read from file
c         ::::::::::::::
c         Maximum lodging rating 
          CALL GET_SPECIES_COEFF(LG_MAX_RATING,'LG_RATING',
     &                           Control, SPE_ERR)

c         Critical wind speed that causes lodging
c     ## Error!!!
          CALL GET_SPECIES_COEFF(LG_CRIT_WIND,'LG_CRIT_WIND',
     &                           Control, SPE_ERR)

c      Photosynthesis variables:
c     :::::::::::::::::::::::::::::::
c         Maximum photosynthetic rate (g/MJ)
c         Default:
          MX_PAR_CONV = 9.9
c         Read from cultivar file:
          CALL GET_CULTIVAR_COEFF(MX_PAR_CONV, 'MaxPARCE', 
     &                                         Control, C_ERR)  
     
!c         #Todo: add read from species file (PARCE_oL376)
!          PARCEMX376 = 5.7
!c         Read from species file:
!          CALL GET_SPECIES_COEFF(PARCEMX376,'PARCEMAX_NCo376',
!     &                           Control, SPE_ERR)
               
c         #todo: make species params: Sunlit and shaded efficiency
          EPS_SL = 1.00
          EPS_SH = 0.31
                    

c         Thermal time parameters for photosynthesis:
c         :::::::::::::::::::::::::::::::::::::::::::::
c         Defaults:
          TOpt1  = 20.0 
          TOpt2  = 40.0 
          TFinalPH  = 47.0

          CALL GET_CULTIVAR_COEFF(TOpt1, 'TOPT_PHOT',  
     &       Control, C_ERR)
          CALL GET_CULTIVAR_COEFF(TOpt2, 'TOPT_PHO2',  
     &       Control, C_ERR)
          CALL GET_CULTIVAR_COEFF(TFinalPH, 'TFin_PHOT',  
     &       Control, C_ERR)            
c        Note: existing TBasePhotos parameter also used  (in SPECIES file)


c         Base temperature for photosynthesis:
c         Default:
          TBasePhotos = 10.0
          CALL GET_SPECIES_COEFF(TBasePhotos,'TBasePhotos',
     &                           Control, SPE_ERR)

c         MJ, July 2011
c         Read in the array of CO2 sensitivities:
c         This is cumbersome but is compatible with the existing
c         coefficient-reading subroutine.
c         X-values (CO2 PPM)
          DO I=1, 10
            WRITE(CO2_READ, '(5HCO2X_, I0)') I
c           Initialise default relative values to 1 always:
            CO2X(I) = I * 100.  ! 100, 200, etc ppm
c           Attempt to read from species file
            CALL GET_SPECIES_COEFF(CO2X(I), TRIM(CO2_READ), 
     &                           Control, SPE_ERR)
          ENDDO
c         Y-values (CO2 PPM)
          DO I=1, 10
            WRITE(CO2_READ, '(5HCO2Y_, I0)') I
c           Initialise default relative values to 1 always:
            CO2Y(I) = 1.  ! always 1 by default
c           Attempt to read from species file
            CALL GET_SPECIES_COEFF(CO2Y(I), TRIM(CO2_READ), 
     &                           Control, SPE_ERR)
          ENDDO



c     Read respiration rate coefficients from SPECIES file:
      ! roots
      RESPCN_RTS = 0.0036
      CALL GET_SPECIES_COEFF(RESPCN_RTS, 'RESPCON_ROOTS', 
     &                           Control, SPE_ERR)
c     ...green leaves,
      RESPCN_LVS = 0.0036
      CALL GET_SPECIES_COEFF(RESPCN_LVS, 'RESPCON_LEAVES', 
     &                           Control, SPE_ERR)

c     ...and sucrose
      RESPCN_SUC = 0.0015
      CALL GET_SPECIES_COEFF(RESPCN_SUC, 'RESPCON_SUCR', 
     &                           Control, SPE_ERR)


c     Respiration coeffs from CUL file
c         Defaults:
            TBaseRESP  = 0.0 
            TOptRESP  = 40.0 
            TFinRESP  = 47.0

          CALL GET_CULTIVAR_COEFF(TBaseRESP, 'TBASE_RESP',  
     &       Control, C_ERR)
          CALL GET_CULTIVAR_COEFF(TOptRESP, 'TOPT_RESP',  
     &       Control, C_ERR)
          CALL GET_CULTIVAR_COEFF(TFinRESP, 'TFin_RESP',  
     &       Control, C_ERR)     

!       WRITE(*, '(10(F10.3))') CO2X
!       WRITE(*, '(10(F10.3))') CO2Y

c     Initialisation:
c     :::::::::::::::
          GROSSP = 0.
          Out%GROSSP = 0.
          Out%DWDT   = 0.
          PARCE  = 0.
          TDMAS  = 0.
          Oldflodge = 0.
          aerldm = 0.
          stkdm  = 0.
          stkwm  = 0.
          Part%FLODGE = 0.
          Flodge = 0.

c         Allow negative growth = 0 for now
          NEGGRO = 0
c         Daily increment in biomass (I think)
c         Move to init section?
          DWDT = 0.
c         Ken Boote tech. LI
c          BOOTLI = 0.
c         Necessary for different solar radiation alg.
          CODERD = 2.

c         Initialise previous day's value of DWDT to 0
          DWDTyest = 0.0

c         No respiration leftovers one day 1
          Rleftovers = 0.0

c         growth and maintenance respiration:
          Rg = 0.0
          Rm = 0.0

c         Maintenance resp initialisation
          RMFRAC = 0.0
          RMSUM = 0.0 
          TMEAN_SUM = 0.0
          RMCOUNT = 0
          RM_AVG = 0.0
          TMEAN_AVG = -99.0
c     ::::::::::::::




      ELSEIF (CONTROL%DYNAMIC .EQ. RATE) THEN
c         :::::::::::::::::::::::::::::::::::

c     Work out the day of the year from YRDOY
      IDOY = CONTROL%YRDOY -  ((CONTROL%YRDOY / 1000)*1000.)

c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     Added 3/10/2003 - sdb 
c     MJ, Feb 2008: replaced the following line:
c     respcf = 1.68 **((((tempmx+tempmn)/2.0)-10.0)/ 10.0) * Respcon
c     with:
      !respcf = RESPQ10 **((((tempmx+tempmn)/2.0)-10.0)/ 10.0) * Respcon
c     And then modified for ASA 2013, MJ


c     Growth respiration
c     ASA, 2013.  MJ.  Algorithm by Singels.
c     ::::::::::::::::::::::::::::::::::::::
c     Ampthor, 2000
!      Rg_RESPCF = 0.19  ! 0.33 from Liu & Bull / Thornley and Johnson

c     Although growth respiration *ought* to operate on today's biomass
c     accumulation, for implementation here it is necessary to subtract
c     yesterday's growth respiration from today's photosynthesis.
c     Hence, DWDTyest and DSUCyest are the previous day's values.
c     This could be a problem when yesterday's photosynthesis was much
c     greater than today's...
      Rg = Rg_RESPCF * MAX(0., (DWDTyest - DSUCyest))

c     Maintenance respiration
c     ASA, 2013.  MJ.  Algorithm by Singels.
c     ::::::::::::::::::::::::::::::::::::::
c     Temperature effect on respiration:
!      RmEffTT = 0.0
!      IF (TMEAN .LT. TBaseRESP) THEN
!        RmEffTT = 0.0  
!      ELSEIF (TMEAN .LT. ToptRESP) THEN
!        RmEffTT = (TMEAN-TBaseResp)/10.0
!      ELSEIF (TMEAN .LT. TFinRESP) THEN
!c       Reduce effective temperature for respiration as the
!c       temperature increases beyond topt
!        RmEffTT = ((TOptRESP-TBaseResp)/10.0) * 
!     &    (1-(TMEAN-TOptRESP)/(TFinRESP-ToptRESP))
!      ELSE
!        RmEffTT = 0.0
!      ENDIF
      IF (TMEAN <= ToptRESP) THEN
        RmEffTT= (TMEAN - 10.0) / 10.0
      ELSE
        RmEffTT= ((ToptRESP - 10.0) / 10.0) * 
     &    (1.0 - (TMEAN - ToptRESP) / (TFinRESP - ToptRESP))
      ENDIF


!      WRITE(*, '(5F8.3)')  
!     &  TMEAN, TBaseRESP, ToptRESP, 
!     &  TFinRESP, RmEffTT


c     Calculated separately per biomass pool
      Rm_ROOTS = RESPCN_RTS * (RESPQ10 ** RmEffTT) * Out%ROOTDM
      Rm_GREENLF = RESPCN_LVS * (RESPQ10 ** RmEffTT) * Part%TOPDM
      Rm_SUCROSE = RESPCN_SUC * (RESPQ10 ** RmEffTT) * Part%SUCMAS

!      WRITE(*, '(8F8.3)')  
!     &  RESPCN_RTS, RESPCN_LVS, RESPCN_SUC, 
!     &  RESPQ10, RmEffTT, Out%ROOTDM, Part%TOPDM, Part%SUCMAS

c     Total maintenance respiration is the sum of the individual components      
      Rm = Rm_ROOTS + Rm_GREENLF + Rm_SUCROSE
     
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     MJ, Feb 2008: 
c     1. changed 9.9 (max photosynthetic conversion rate g/MJ)
c        to MX_PAR_CONV and made this a cultivar coeffcient:
c     2. Changed 7.0 (base temperature for photosynthesis) to TBasePhotos 
c	 parce = 9.9 *(1.0 - exp(-0.08 *(((tempmx+tempmn)/2.0) - 7.0)))
c     We may want to give some thought to what the 0.08 term refers to...
c     It affects the response of photosynthesis to temperature.

!     Changed for ASA 2013 runs by MJ	
!      parce = MX_PAR_CONV *
!     &        (1.0 - exp(-0.08 *
!     &        (((tempmx+tempmn)/2.0) - TBasePhotos)))


c    MJ, Aug 2017
c      Singels, Hoffman 2017: Assuming that leaf-level photosynthesis rate measurements,
c      taken relative to NCo376, can only be scaled up to canopy level for sunlit leaves.
c      Cultivar differences with shaded leaves are likely to be smaller.  So, NCo376 
c      MaxPARCE value is used for shaded leaves and the actual cultivar value is used
c      for sunlit leaves only.
c      The separation between sunlit and shaded leaves is based on a calculation from: 
c        Monteith and Unsworth, 2013.
c      with relative efficiencies calculated from:
c        de Silva and de Costa, 2009
c      MX_PAR_CONV now represents a rather awkward concept.  It is canopy-level PARCE
c      transformed from the NCo376 value to the cultivar-specific value by relative
c      leaf-level photos rates (or stomatal conductance).

!c     Difference between this cultivar's MaxPARCE and that of NCo376
!c     (leaf level)
!      D_PRC_OL = MX_PAR_CONV - PARCEMX376
!c     Scaled to canopy level via sunlit and shaded LAI fractions, taking into account
!c     the relative efficiencies of sunlit and shaded canopy fractions (EPS_SL and EPS_SH)
!      D_PRC_OC = (EPS_SL * D_PRC_OL * F_SL) + 
!     &           (EPS_SH * D_PRC_OL * (1.0 - F_SL))
!c     Actual MaxPARCE value for this cultivar, today:
!      NMX_PAR_CONV = PARCEMX376 + D_PRC_OC     
!!      WRITE(*, '(5(F8.3))')  MX_PAR_CONV, PARCEMX376, F_SL, D_PRC_OC, 
!!     & NMX_PAR_CONV

c     Calculate change in thermal time, then normalise to effective 
c     temperature index
      TMEAN = (tempmx+tempmn)/2.0 
      EFFTEMPC = (D_TT4(TMEAN, TBasePhotos, TOpt1, TOpt2, TFinalPH))/
     &  (TOpt1-TBasePhotos)
c     Calculate temperature-influenced radiation use efficiency
c     Revised for ASA 2013 work by MJ
      PARCE = MX_PAR_CONV * EFFTEMPC 
      ! PARCE = NMX_PAR_CONV * EFFTEMPC 


c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     MJ, Feb 2008: Michiel Smit (I think) noticed that PARCE is 
c     negative when temperature drops below TBasePhotos.  This means
c     that GROSSP is negative as well.  
          PARCE = MAX(0., PARCE)
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


       IF(MDL.EQ.1) THEN
c Simple Hesketh/McCree approach............................................
c Heat of combustion of Dm is assumed to be 4200 cal/g; PAR is in cal/cm2
       if(coderd.eq.1.) solrad=solrad/0.09454
C CNB JUN2002 - SWITCHED THE STATEMENT BELOW OFF SINCE PHOTOS NOW WORKS
C               ON MJ/m². THE CHANGE WAS VERIFIED AND WORKS CORRECT 
C       if(coderd.eq.2.) solrad=solrad/(4.187*0.01)

c CNB&AS Lodging factor Jan 2002
c We've included a reduction in RUE due to lodging here after some
c indication of the work by Singh 1998 ASSCT


c    CANESIM lodging (MJ, 2006/09/06)
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c

c     ** NOTE **
c     Add weight of intercepted rainwater to AERL Wet Mass for lodging
c     purposes.
c     Weight of intercepted rainwater
      PRECIPM = 0.

      IF (Climate%RAIN .GT. 0.0) THEN
c      Intercepted rainfall is max 5 mm of incident rainfall * fractional interception...
c      and its mass is ... * 1 litre = 1 kg / m2 * 10000 m2/ha/1000 kg/t = t/ha
        PRECIPM = LI * MIN(5.0, Climate%RAIN) * 10.0
      ENDIF


c     Cultivar variables:
c          LG_RATING   =   8.
c          LG_AMBASE   = 220.
c          LG_AMRANGE  =  30.
c          LG_GP_REDUC =   0.28

c          LG_MAX_RATING = 8.
c          LG_CRIT_WIND  = 200.

c         Previous value of lodging
      	oldflodge = flodge
c         Initialise lodging variables:
          flodge    =  0.
          lodgeswc  =  0.
          lodgewind =  0.
          lodgeam   =  0.
          LODGE_LI  =  0.

c     Override the allowlodge setting:
c     Assume that allowlodge is ON
c     Then is lodgerating is 0 (or less), do not
c     bother calculating lodging...

c     MJ, Mar 2008: lodgerating has been deprecated...
c     Following line changed:
c      IF (LG_RATING .LT. 0.01) ALLOWLODG = 0
      IF (LG_AMBASE .GT. 998.) ALLOWLODG = 0
c     Now, lodging is disabled entirely if LG_AMBASE = 999 or more


      IF (ALLOWLODG .EQ. 1) THEN
c         Soil water content effect on lodging:
c         :::::::::::::::::::::::::::::::::::::
c         Only simulate the water effect on lodging if water balance is
c         simulated:
          IF (ISWITCH%ISWWAT .EQ. 'Y') THEN
c             High runoff = high SWC = high lodge risk
                  IF(RUNOFF .gt. 0.) THEN 
                      lodgeswc = 2. 
c	                WRITE(*, '(A, F8.4)') 'LODGESWC: ', lodgeswc
	            ENDIF
          ELSE
              lodgeswc = 0. 
          ENDIF
c         :::::::::::::::::::::::::::::::::::::

c         Wind effect on lodging:
c         :::::::::::::::::::::::
c             lodgewind=amax1(0.0,amin1(2.0,(windsp-200.0)/100.0)) !risk factor for wind
c             ::::::::::::::::::::::::::::::::
              lodgewind = MAX(0., MIN(2., (WINDSP - LG_CRIT_WIND) 
     &                                    / LG_CRIT_WIND))
c         ::::::::::::::::::::::::::::::::

c         High aerial mass effect on lodging:
c         :::::::::::::::::::::::::::::::::::
c             To avoid div/0 error, and to make sense:
	        IF (STKDM .gt. 1.) THEN
c                 Note: 220 should be a variety parameter
                  lodgeam = LG_MAX_RATING * 
     &               (((aerldm/(stkdm/stkwm)+PRECIPM)
     &                      - LG_AMBASE) / LG_AMRANGE)
c                Combined effects on lodging:
c                ::::::::::::::::::::::::::::      	
                 lodge = lodgeam + lodgewind + lodgeswc

	        ENDIF
c         :::::::::::::::::::::::::::::::::::
      


              !lodge range 0..8
              if (lodge.lt.0.0) lodge=0.0
              if (lodge .gt. LG_MAX_RATING) lodge = LG_MAX_RATING
c         ::::::::::::::::::::::::::::

c         Effect of lodging on light interception:
c         These effects are applied in CANOP3.for
c         ::::::::::::::::::::::::::::::::::::::::
              LODGE_LI = lodge / LG_MAX_RATING
c         ::::::::::::::::::::::::::::::::::::::::

c         Reduce photosynthesis accordingly:
c         ::::::::::::::::::::::::::::::::::
      	    flodge=amax1(lodge/LG_MAX_RATING,oldflodge)

c          WRITE(*,*) 'Lodging: ', LODGE_LI, LODGE, LODGEAM
      ENDIF

          PAR=SOLRAD*0.5

c         MJ, July 2011
c         :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c         CO2 'fertilisation effect' - taken from CERES-Maize
c         CO2Y, CO2X are arrays defining relative photosynthesis rate
c         (Y) to atmospheric CO2 concentration (X, ppm).
c         TABEX interpolates a relative pg value for whatever the CO2
c         concentration is.  10 is the length of the datapoint array,
c         defined in the SPECIES file.
          PCO2 = 1.
          PCO2  = TABEX (CO2Y,CO2X,CO2,10) 
          
          
c         reduce photosynthesis by maximum 28% due to lodging. Based on Singh 1998 ASSCT
c         PARCE IS g/MJ
          GROSSP=PARCE*PAR*LI*(1.-(flodge * LG_GP_REDUC)) * PCO2

          Out%PAR = PAR
!      CALL GETLUN('WORK.OUT',SCLUN)
c      WRITE(SCLUN, '(I5, A, A, F10.5)') IDOY, ' SRAD is ',ACHAR(9), 
c     -                               Climate%SOLRAD
c     :::::::::::::::::::::::::::::::::

	!convert from g/m² to t/ha
	    GROSSP=GROSSP*(10000.0/1000000.0)
c         Changed by MJ for ASA 2013 runs
          !DWDT=(1.0 - RESPGCF)*(GROSSP-respcf*TDMAS)





      ! MJ#
      ! DWDT = 0.2

c     Some output:
c      WRITE(*, *) 'GROSSP', GROSSP, '; DWDT', DWDT



C 'TD' is mean daylight temperature used in a regression with LCA 
C  photosynthesis where a Pmax was at 33 deg C.

       ELSE

       ENDIF
C NET CARBON GAIN IS REDUCED BY WATER AND ANAEROBIC STRESS
c       RESPWT=PGROSS-DWDT
!      DWDT=AMIN1(DWDT*CWSI*ANAERF,DWDT)
!      14 Feb 2011 CHP added SLPF factor
!       DWDT=AMIN1(DWDT*CWSI*ANAERF*SLPF,DWDT)

!      MJ, modified for ASA 2013:
c      Rm and Rg are independent of photosynthesis, so should not be reduced by water stress
c      Hence apply stresses to reduce gross photosynthesis rate
       PG_STRESSED = GROSSP*CWSI*ANAERF*SLPF
c      And then subtract respiration.  But, because yesterday's respiration is being
c      substracted from today's photosynthesis, it is necessary to keep track of
c      situations where respiration rates exceed photosynthesis rate.  In this case, 
c      to avoid negative DWDT, the 'extra' respiration is carried over to the next day.
c      This is the Rleftovers variable
       Rtot = Rg + Rm + Rleftovers
c      and maybe calculate a new value for leftovers:
       IF (Rtot .GT. PG_STRESSED) THEN
         Rleftovers = Rtot - PG_STRESSED
         Rtot = PG_STRESSED
       ELSE
c        Reset leftovers to 0
         Rleftovers = 0.0
       ENDIF
c      So the biomass increment is stressed gross photosynthesis,
c      minus total respiration.
       DWDT = PG_STRESSED - Rtot



C If negative growth is allowed it cannot occur before a limited TDMAS has 
C accumulated
       IF(DWDT.LT.0.0) THEN
        IF(TDMAS.GT.5.0.AND.NEGGRO.EQ.1) THEN
              WRITE(*,*)'Growth is negative'  
        ELSE
              DWDT=0.0
        ENDIF
       ENDIF

c      Integrate TDMAS
       TDMAS=TDMAS+DWDT

c      Calculate the Maintenace respiration fraction today:
c      Start after TDMAS is 5 t/ha to avoid this spiralling to infinity
       IF (TDMAS .GT. 5.0) THEN
         RMFRAC = Rm / TDMAS
         RMSUM = RMFRAC + RMSUM
         TMEAN_SUM = TMEAN_SUM + TMEAN
         RMCOUNT = RMCOUNT + 1
         IF (RMCOUNT .GT. 0) THEN
           RM_AVG = RMSUM / RMCOUNT
           TMEAN_AVG = TMEAN_SUM / RMCOUNT
         ELSE
           RM_AVG = 0.0
           TMEAN_AVG = -99.0
         ENDIF
       ENDIF

c      Set FDWDT to net change in aboveground dry mass
	 FDWDT=DWDT

c      Update the 'yesterday' value of DWDT
c      For ASA 2013 work, MJ
       DWDTyest = DWDT

C      ALLOCATION OF GROWTH TO ROOTS * 100 TO CONVERT T/HA TO g/m^2
       ROOTF=AMIN1(ROOTF,0.7) 

c     GRORT now assigned in SC_Partit.for      
c       GRORT=DWDT*ROOTF*100.0

       if(coderd.eq.1.) solrad=solrad*0.09454
C SDB  3/6/2003 this gave incorrect solrad values replaced by below
C       if(coderd.eq.2.) solrad=solrad*(4.187*0.01)
       if(coderd.eq.2.) solrad=solrad

c     Copy values from local variables to 'objects':
c     ::::::::::::::::::::::::::::::::::::::::::::::
c         Climate vars:
c         :::::::::::::
          Climate%SOLRAD = SOLRAD
          Climate%TEMPMN = TEMPMN
          Climate%TEMPMX = TEMPMX
          Climate%WINDSP = WINDSP

c         Growth vars:
c         ::::::::::::
c          Growth%GRORT   = GRORT
          Growth%LAI     = LAI
          Growth%LI      = LI   

c         Part vars:
c         ::::::::::
          Part%AERLDM = AERLDM
          Part%FDWDT  = FDWDT
          Part%FLODGE = FLODGE
          Part%PARCE  = PARCE
          Part%RESPCF = RESPCF
          Part%ROOTF  = ROOTF
          Part%STKDM  = STKDM
          Part%STKWM  = STKWM
          Part%TDMAS  = TDMAS

c         Was GROSSP.
          Out%GROSSP = PG_STRESSED
          Out%DWDT   = DWDT 
          PAR = 0.

          Part%RESP_G = Rg
          Part%RESP_M = Rm


       IF(MDL.EQ.1) RETURN
       IF(IW.NE.1) RETURN
c       WRITE(16,100)DOY,LAI,bootLI,PMAXML,SOLRAD,DPAR,DAYKB,DAYINT,
c     -   QECAN, ENEFF2,PGROSS,RMCOEF,RESPWT,DWDT


c     End of RATE calculation
c     :::::::::::::::::::::::      
      ELSEIF (CONTROL%DYNAMIC .EQ. SEASEND) THEN
          oldflodge = 0.
          lodge     = 0.
          PAR = 0.
          Out%PAR = 0.
      ENDIF

c     Set Rg and Rm values in the Part object for output
      Part%RESP_G = Rg
      Part%RESP_M = Rm
      
c     ::::::::::
      

 100   FORMAT(F4.0,2F6.2,17F8.3)
       RETURN 
       END 


!>@brief A subroutine for calculating waterlogging
!>       stress in Canegro.  (Anaerobic conditions)
!> @param[out] ANAERF Anaerobic stress factor that limits photosynthesis
!> @param[in]  NLAYR  Number of soil layers
!> @param[in]  RLV    Root length density (cm/cm3)
!> @param[in]  SW     Soil moisture content (cm3/cm3)
!> @param[in]  DUL    Drained upper limit of soil moisture content (cm3/cm3)
!> @param[in]  SAT    Saturated limit of soil moisture content (cm3/cm3)
! Not used for now
       SUBROUTINE SC_ANAERF(
     &   NLAYR, RLV, SW, DUL, SAT, DLAYR,  ! Inputs  
     &   ANAERF)

!     2023-01-26 chp removed unused variables from argument list: CONTROL

       USE MODULEDEFS
       INTENT(IN) :: NLAYR, RLV, SW, DUL, SAT, DLAYR !, Control
       INTENT(OUT) :: ANAERF
       
!      TYPE (ControlType) Control
       
       REAL ANAERF
       REAL SWC_RTD, DUL_RTD, SAT_RTD
       REAL AERSTp
       INTEGER I, NLAYR
       REAL SWC_RTDi, DUL_RTDi, SAT_RTDi
       REAL WTRLG, WTRLG_7
       INTEGER NROOTD
       REAL, DIMENSION(NLAYR) :: RLV, SW, DUL, SAT, AERST, NSTLAYR, 
     &   DLAYR


c        Soil excess water stress
c        'Aeration stress' (Steduto, et al)
c        :::::::::::::::::::::::::::::::::::::::::::::
         SWC_RTD = 0.0
         DUL_RTD = 0.0
         SAT_RTD = 0.0
         AERSTp = 0.0
c        Get total SWC, SAT for the whole rooted profile
         DO I=1, NLAYR
           IF (RLV(I) .GT. 0.0001) THEN
           ! this layer is rooted
           NROOTD = I  
c          Rooted profile soil water content (cm)
           SWC_RTDi = SW(I) * DLAYR(I)
c          Rooted profile DUL (anything above is waterlogging)
           DUL_RTDi = DUL(I) * DLAYR(I)
c          Rooted profile SAT (anything above is runoff)
           SAT_RTDi = SAT(I) * DLAYR(I)

c          Aeration stress per layer (used to distribute whole-profile
c          aeration stress only):
c          # possibility to have a lower denominator for
c          # reducing expansive growth (e.g. ((SAT_RTDi - DUL_RTDi)/2.0) )
c          # more than photosynthesis
           AERST(I) = MAX(0.0, SWC_RTDi - DUL_RTDi) /
     &       (SAT_RTDi - DUL_RTDi)

c          Totals:
           SWC_RTD = SWC_RTD + SWC_RTDi
           DUL_RTD = DUL_RTD + DUL_RTDi
           SAT_RTD = SAT_RTD + SAT_RTDi
           AERSTp = AERSTp + AERST(I)
           
           ENDIF
         ENDDO     

c        Extent of aeration stress over the rooted profile
         ! WTRLG = MAX(0.0, SWC_RTD - DUL_RTD) / (SAT_RTD - DUL_RTD)
c        Use the lowest per-layer value to determine per-profile aeration stress:
c        Appears not to be working so well with the Ritchie / CERES WU model.
c        The basic thinking was that if any part of the root system 
c        has access to air, that will be enough for the whole plant.
c        But of course, some very thin layer with air will probably not be 
c        enough...  So a more robust model is required!
         ! WTRLG = MINVAL(AERST)
         WTRLG = DOT_PRODUCT(AERST(1:NROOTD),DLAYR(1:NROOTD))/NROOTD
         IF (WTRLG .GT. 1.0) THEN
           ! WRITE(*, '(2F10.5)') WTRLG, MAXVAL(AERST)
D           WRITE(*, '(I10)') CONTROL%YRDOY 
D           WRITE(*, '(5A10)') 'RLV', 'SW', 'DUL', 'SAT', 'DLAYR'
D           DO I=1, NLAYR
D           WRITE(*, '(5F10.5)') RLV(I), SW(I), DUL(I), SAT(I), DLAYR(I)
D           ENDDO
         ENDIF
c        Now calculate 7-day running average aeration stress:
         ! #todo
         WTRLG_7 = WTRLG
c        If there is aeration stress, distribute by layer in order
c        to limit transpiration according to per-layer relative 
c        aeration stress:
         IF (WTRLG_7 .GT. 0.00001) THEN
c        Consider rooted layers only
         DO I=1, NROOTD
c          This layer's SWDF1 'aeration stress' is weighted by the 
c          previously-calculated relative aeration stress and scaled 
c          to per-profile stress.
            NSTLAYR(I) = (AERST(I) / AERSTp) * (1.0 - WTRLG_7)
         ENDDO
c        And update the whole-profile water stress factor:
         ANAERF = (1.0 - WTRLG_7)
         ENDIF
       END