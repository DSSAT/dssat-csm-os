c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c     SASRI CANEGRO
c     &------------
c     Matthew Jones, September - December 2006
c     Gainesville, Florida, USA
c     &----------------------------------------------------
c     The SASRI Canegro was modularised according to the 
c     DSSAT structure and incorporated into the DSSAT
c     CSM.  This serves both as an update to the legacy
c     DSSAT sugarcane model (also a CANEGRO, from years
c     ago), as well as a restructured CANEGRO, a platform
c     upon which further development will hopefully be
c     easier and more effective.
c     ::::::::::::::::::::::::::::::::::::::::
c     This work was funded by the International Consortium 
c     for Sugarcane Modelling (ICSM):
c     https://sasri.sasa.org.za/agronomy/icsm/
c     ::::::::::::::::::::::::::::::::::::::::
c     Acknowledgements and thanks to:
c     Dr. Abraham Singels (SASRI), Dr. Maurits van den Berg 
c     (SASRI), Dr. Jim Jones (UF), Cheryl Porter (UF), 
c     Jim Shine (SGC / UF), Gerrit Hoogenboom (UGA) 
c     ... and Geoff Inman-Bamber (CSIRO?), for creating the 
c     Sugarcane infrastructure in DSSAT
c     -----------------------------------------------------
c     Changes for v4.7
c     Gainesville, Florida, January 2018.
c     Matthew Jones
c
c     1. Temperature responses (cardinal temperatures for 
c        many processes).
c     2. Shoot population (linear tillering rate per 
c        primary shoot with light interception-based
c        slowing).
c     3. Respiration (growth and maintenance)
c     4. CO2 impacts (lookup function now has better support
c        for low CO2, no direct benefit to high CO2.)
c     5. [optional: aquacrop water uptake] 
c     &----------------------------------------------------



c     SUBROUTINE: SC_CNGRO()
c     &---------------------
c     This sub interfaces with the rest of the DSSAT CSM.
c     Inputs and outputs are passed solely through this
c     interface.  This forms the PLANT GROWTH MODULE for
c     sugarcane.  Soil and atmospheric processes, along YRPLT
c     with external calculations like irrigation are
c     handled OUTSIDE this module.
c     [interface copied from MZ_CERES.for]
!  Added IRRAMT July 2015
!  2023-01-26 CHP Reduce compile warnings: add EXTERNAL stmts, remove 
!                 unused variables, shorten lines.
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SC_CNGRO (
     &    CONTROL, ISWITCH,                           !Input
     &    CO2, EOP, EP, EO, ES, HARVFRAC, NH4, NO3,   !Input
     &    SOILPROP, SW, TMAX, TMIN, TRWUP, EOS,       !Input
     &    RWUEP1, YREND, YRPLT, WEATHER, IRRAMT,      !Input
     &    CANHT, KCAN, KTRANS, MDATE, NSTRES,         !Output
     &    PORMIN, RLV, RWUMX,STGDOY, UNH4,            !Output
     &    UNO3, XLAI, XHLAI, EORATIO, SENESCE)        !Output
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::

!     2023-01-26 chp removed unused variables from argument list: 
!         DAYL, SNOW, SRAD, TRWU, TWILEN, 
!         HARVRES, SENESCE

c     Define DSSAT composite variables:
c     [Taken from MZ_CERES.for]
      USE ModuleDefs

c     Define CANEGRO composite variables:
      USE CNG_ModuleDefs
      USE N_TYPES  ! HBD (Jan 2023) after MvdL 2011
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::

      IMPLICIT NONE
      EXTERNAL SC_OPGROW, GETLUN, HEADER, SC_OPHARV, POPLT3, 
     &  SC_PARTIT, CANOP3, CARRYOVER_RATOON, GET_CULTIVAR_COEFF, 
     &  ROOTGROWTH, GET_SPECIES_COEFF, FIND_INP, WARNING, InitCaneCrop, 
     &  SC_PHENOLOGY, SC_NITRO
      EXTERNAL SC_PHOTOS, SC_CCOUT, SC_ETOUT
      SAVE
      
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c     DSSAT variable declarations (i.e. subroutine 
c     argument declarations)
c     Explanation of variables in Appendix 1 at the 
c     end of this subroutine.
c     :::::::::::::::::::::::
c     & Input variables -
c     :::::::::::::::::::
      REAL     CO2
!     REAL     DAYL
      REAL     EOP
      REAL     EP, EP1, RWUEP1, RWUEP2
      REAL     EO, EOS, ES
      REAL     HARVFRAC(2)
      REAL     NH4(NL)
      REAL     NO3(NL)
!     REAL     SNOW
!     REAL     SRAD
      REAL     SW(NL)
      REAL     TMAX
      REAL     TMIN
      REAL     TRWUP
!     REAL     TRWU
!     REAL     TWILEN
      INTEGER  YREND
      INTEGER	 YRPLT
c     Daily irrigation amount (mm)
      REAL IRRAMT
c     & Output variables: -
c     :::::::::::::::::::::
      REAL     CANHT
      REAL     KCAN
      REAL     KTRANS
      INTEGER  MDATE
      REAL     NSTRES
      REAL     PORMIN
      REAL     RLV(NL)
! c     Maximum rate of root water uptake per unit root length, RWUMX (today) and RWUMX_REF (limited by water stress)
      REAL     RWUMX ! , RWUMX_REF
      INTEGER  STGDOY(20)
      REAL     UNH4(NL)
      REAL     UNO3(NL)
      REAL     XLAI
      REAL     XHLAI 
      REAL     EORATIO
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c     DSSAT composite variables:
c     [Taken from MZ_CERES.for]
c     ::::::::::::::::::::::::::
      TYPE (ControlType) CONTROL
      TYPE (SoilType)    SOILPROP
      TYPE (SwitchType)  ISWITCH
!     Type (ResidueType) HARVRES 
      Type (ResidueType) SENESCE
      Type (WeatherType) WEATHER


c     & Local variables -
c     :::::::::::::::::::
c     local copy of CONTROL%DYNAMIC:
      INTEGER DYNAMIC
c     Output file unit:
c      INTEGER NOUTDG
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c     CANEGRO variable declarations (variables internal
c     to this subroutine and sub-subroutines)
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c     Heat units for today: base10, base16, cultivar-
c     defined heat units for leaf dynamics & emergence
c      & population
      REAL HU10, HU16, HUBaseLeaf, HUBaseEm, HUBasePop
c     Cumulative versions of above:
      REAL CHU10, CHU16, CHUBaseLeaf, CHUBaseEm, CHUBasePop

c     Thermal time measures for canesim canopy:
      REAL  HU_CANESIM, CHU_CANESIM

c     Average temperature for today:
c      REAL AVGTEMP

c     HBD (Jan 2023) after MvdL 2011
c     Nitrogen stress
c     NUM_COMPS is defined in N_TYPES module
c     Array of N-stress indices for different plant components
      REAL N_STRESS, ROOTNCONC, TOP_N      !MvdL

c     Base temperatures for leaf and emergence
      REAL TBaseLeaf, TBaseEm, TBasePop

c     CANEGRO local variables:
c     ::::::::::::::::::::::::
      TYPE (CNG_SoilType) Soil
      TYPE (WaterType)    WaterBal
      TYPE (CaneCropType) CaneCrop
      TYPE (GrothType)    Growth
      TYPE (ClimateType)  Climate
      TYPE (PartType)     Part
      TYPE (OutType)      Out

c     Yesterday's heat cum. thermal time, base16
      REAL T0

c     Canopy/population variables
      REAL    pplast
      INTEGER newbat
      REAL    rowspc, stdaye,stddiv, RATNUM

c     Local variables required for Photosynthesis
c      REAL SCV
      REAL CWSI
      REAL RECTIM 
      REAL SWDFMN
      REAL CRITSW
      REAL SLPF

c     Thermal time required to for photosynthesis to recover
c     from water stress (cultivar param):
      REAL HuRecover

      REAL ALAT
      INTEGER NEGGRO
      REAL DWDT
      REAL CODERD
      REAL RUNOFF
      INTEGER ALLOWLODG

c     Lodging variables (note: look in canop3 for lodge coeff.)
      REAL LODGE_LI

      INTEGER IWS
      REAL STDAYC, SENESF, EXPLA, PER, RESET, BOOTLI
c     BEST is a cultivar input
      REAL STRPOP, BEST
      INTEGER MDL

c      Average RM FRACtion and corres. mean temperature so far this sim run
       REAL RM_AVG, TMEAN_AVG

c     Cellulosic DM (t/ha)
      REAL  CELLSE_DM

c     Thermal time requirements for phenology
c     Germination:
!      REAL TT_GERM
c     Emergence
      REAL TT_EMERG
      LOGICAL EMERGED !, FILE_EXISTS

c     General local vars:
      INTEGER I

c     Choice of Canopy routine
      INTEGER CANOPY_ROUTINE !, CANESIM, CANEGRO

c     

c     Error variable for cultivar coefficient read
!      LOGICAL CF_ERR
c     Error variable for species coefficient read
      LOGICAL SPC_ERROR
!      REAL HI

c     temp output file (remove)
!      CHARACTER TFILENAME*25
      
c     Row-spacing warning text:
      CHARACTER*78 WARNINGS(2)      

c     REMOVE??
!      INTEGER INIDOY 
!      REAL INSWDF1, INSWDF2
!      INTEGER TDOY
!      REAL    TempLAI

!     CHP ADDED 2/13/2007
      REAL MAXLAI

c     Should the ratoon info be carried over?
c      LOGICAL CARRY - replaced by CaneCrop%CARRY_OVER

c     Partitioning:
c     To calc fresh mass
      REAL STKDMC, STKWAT

c     Temp:
!      REAL ES, ES_mm, ASWC_mm
      REAL cCOEFF
      LOGICAL CERROR

!     Unit number for output
!      INTEGER SCLUN   !CHP


c     Yesterday's value of delta sucrose mass:
c     t/ha/d
      REAL DSUCyest

c     Soil water stress for tillering (not used in this sub):
!     REAL SWDF30
      
c     Sunlit LAI fraction
      REAL F_SL      

c     Variables for calculating waterlogging stress
c     (saturation factor)      

      REAL  SATFAC
      REAL SUMEX, SUMRL, SWEXF
      INTEGER L
      REAL TSS(NL)
      
      !MvdL
!      REAL PLIGLF  ! HBD (Jan 2023) after MvdL 2011

c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c     ~~~~~~~~ SUBROUTINE CODE  ~~~~~~~~~~~~~~~~~~~~~~~~~~~
c     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c
c     Initialisation for DYNAMIC = all
c     ::::::::::::::::::::::::::::::::
c     Init DYNAMIC:
      DYNAMIC = CONTROL%DYNAMIC

c     Copy Soil properties from DSSAT object to CANEGRO
c     object:
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
      DO I=1,NL
          Soil%DUL(I)   = SOILPROP%DUL(I)
          Soil%LL(I)    = SOILPROP%LL(I)
          Soil%SW(I)    = SW(I)
          Soil%DLAYR(I) = SOILPROP%DLAYR(I)
          Soil%NLAYR    = SOILPROP%NLAYR
          Soil%SAT      = SOILPROP%SAT
      ENDDO
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c    
c              DYNAMIC = RUNINIT
c
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF (DYNAMIC.EQ.RUNINIT) THEN
       STGDOY  =  0
       ALLOWLODG  =  0 
       CANOPY_ROUTINE  =  0
       DYNAMIC  =  0
       I  =  0
       IWS  =  0
       MDL  =  0
       NEGGRO  =  0
       newbat  =  0
       CANHT  =  0.0
       NSTRES  =  1.
       PORMIN  =  0.0
       RLV  =  0.0
       UNH4  =  0.0
       UNO3  =  0.0
       XHLAI   =  0.0
       XLAI  =  0.0
       pplast  =  0.0
       rowspc  =  0.0
       stdaye  =  0.0
       stddiv  =  0.0
       RATNUM  =  0.0
       HU_CANESIM  =  0.0
       CHU_CANESIM  =  0.0
       ALAT  =  0.0
       cCOEFF  =  0.0
       CHU10    =  0.0
       CHU16  =  0.0
       CHUBaseLeaf  =  0.0
       CHUBaseEm  =  0.0
       CHUBasePop  =  0.0
       CODERD  =  0.0
       CRITSW  =  0.0
       CWSI  =  0.0
       DWDT  =  0.0
       HU10  =  0.0
       HU16  =  0.0
       HUBaseLeaf  =  0.0
       HUBaseEm  =  0.0
       HUBasePop  =  0.0
       HuRecover  =  0.0
       LODGE_LI  =  0.0
       MAXLAI  =  0.0
       RECTIM   =  0.0
       RUNOFF  =  0.0
       STDAYC  =  0.0
       SENESF    =  0.0
       EXPLA  =  0.0
       PER  =  0.0
       RESET  =  0.0
       BOOTLI  =  0.0
       STKDMC   =  0.0
       STKWAT  =  0.0
       STRPOP  =  0.0
       BEST  =  0.0
       SWDFMN  =  0.0
       T0  =  0.0
       TBaseLeaf  =  0.0
       TBaseEm  =  0.0
       TBasePop  =  0.0
       TT_EMERG  =  0.0

c     Call the output routine, to initialise output
c     ::::::::::
      CALL SC_OPGROW(CONTROL, CaneCrop, Growth,
     &  Part, Out, WaterBal, SoilProp,
     &  YRPLT, CELLSE_DM)

      IF (INDEX('YDA',ISWITCH % IDETL) > 0) THEN
!        TFILENAME = 'Work.OUT'
!        CALL GETLUN('WORK.OUT', SCLUN)
!        INQUIRE(FILE=TFILENAME, EXIST=FILE_EXISTS)

c       Open the file
!        IF (FILE_EXISTS) THEN
c         In append mode if the file already exists
!          OPEN(UNIT=SCLUN,FILE=TFILENAME,STATUS='OLD',POSITION='APPEND')
!        ELSE
c         A new file if not existing
!          OPEN (UNIT=SCLUN, FILE=TFILENAME, STATUS='NEW')
!          WRITE(SCLUN,'("*CaneGro Supplemental Output File")')
!        ENDIF

c       Output a header (treatment / run info, etc)
c       ::::::::::::::::::::::::
c        Use the predefined one:
!        CALL HEADER(SEASINIT, SCLUN, CONTROL%RUN)
      ENDIF

c     I don't know what this ought to be... 1 seems fine.
      KCAN = 1.

c     Ratoon number: it may be worth trying to keep track of ratoon
c     number; this will be initialised to 0. in runinit.  If the INP
c     file says 'S' (plant) then it will stay on 0.  If the INP
c     file says ratoon, and CaneCrop%RATOON is NOT 0., it will check for ratoon
c     likelihood and possibly increment this number.
      RATNUM = 0.
      CaneCrop%RATOON = 0

!     chp added 2/13/07 - prepare for summary outputs
      call SC_OPHARV(CONTROL, ISWITCH, 
     &    CaneCrop, EMERGED, Growth, Part, Out, MAXLAI,    !Input
     &    WaterBal%SWDF1, WaterBal%SWDF2, 
     &    STGDOY,
     &    XLAI, YRPLT,
     &    HARVFRAC, TOP_N)     !Input  ! HBD (Jan 2023) after MvdL 2011

c     :::::::::::::::::::::::::::

c     Initialise stalk population routine
c     :::::::::::::::::::::::::::::::::::
      CALL POPLT3 (CHUBasePop+HUBasePop,t0,pplast,newbat,rowspc,
     &  stdaye, stddiv,WATERBAL, CANECROP, SOIL, ISWITCH, 
     &  CONTROL, STGDOY, HUBaseEm, Growth%LI)
c     :::::::::::::::::::::::::::::::::::


c     Initialise Partitioning variables
c     ::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL SC_PARTIT (WaterBal, Part, Climate, CaneCrop, Growth, 
     &  Out, Control, 
     &  STGDOY, DSUCyest, CELLSE_DM, CHUBaseLeaf)
c     ::::::::::::::::::::::::::::::::::::::::::::::::::


c     Initialise canopy
c     :::::::::::::::::
      CALL CANOP3(CHUBaseEm,CHUBasePop,IWS,STDAYC,
     &  SENESF, expla,PER,KTRANS, F_SL, pplast,newbat,stdaye,
     &  stddiv, reset, rowspc,strpop,BEST, 
     &  LODGE_LI, CLIMATE, CaneCrop, Soil, Growth, WaterBal,
     &  CONTROL, ISWITCH)
c     ::::::::::::::::::::::::::::::::::::::::::::::::::

c     Initialise the Growth composite variables:
c     ::::::::::::::::::::::::::::::::::::::::::
      Growth%GRORT = 0.
      Growth%LAI = 0.
      Growth%TLAI = 0.
      Growth%LI = 0.
      Growth%GERMDY = 0.
      Growth%RTCMPG = 0.


c     Ratoon considerations
      CALL CARRYOVER_RATOON(Control, INT(RATNUM), 
     &   CaneCrop%CARRY_OVER)

c     Call the cultivar routine at least once:
      CALL GET_CULTIVAR_COEFF(cCOEFF, 'dummy', CONTROL, cERROR)

c     Set model number (CANEGRO dependency, to be removed eventually)
      MDL = 1
          
      RECTIM = 0.    
      CWSI = 0.    

c     Initialise root growth routine
      CALL ROOTGROWTH(Control, ISWITCH, Soil, WaterBal, 
     &  Growth, Climate, CaneCrop, RWUMX)

c HBD (Jan 2023) after MvdL 2011
c     Initialise Nitrogen module:
c         If N is simulated:
          IF (ISWITCH % ISWNIT .NE. 'N') THEN
              CALL SC_NITRO(ISWITCH, Part%TOPDM, Out%ROOTDM, Part%STKDM, 
     &                      Out%TRASDM, NO3, NH4,UNO3, UNH4, RLV,
     &                    SW, N_STRESS, SoilProp, Control,
     &                    CaneCrop, SENESCE, ROOTNCONC, TOP_N, YRPLT)
          ENDIF  

c     END of RUNINIT
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c
c     -----------------------------------------------------
c
c              DYNAMIC = SEASINIT
c
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
      ELSEIF (DYNAMIC.EQ.SEASINIT) THEN

c     EORATIO: this seems to be the closest thing to a crop coefficient
c     that DSSAT offers for the PENMON model at least.
c     ::::::::::::::::::::::::::::::::::::::::::::::::
      EORATIO = 1.15
      CALL GET_SPECIES_COEFF(EORATIO,'EORATIO', CONTROL, SPC_ERROR) 
c     ::::::::::::::::::::::::::::::::::::::::::::::::

c     Thermal time for photosythesis to recover from moisture stress
c     Should this be in the photosynthesis routine?
      HuRecover = 150.
      CALL GET_SPECIES_COEFF(HuRecover, 'HuRecover', 
     &                           CONTROL, SPC_ERROR)
        
c     Call the output routine, to initialise output files,
c     and so on:
c     ::::::::::
       CALL SC_OPGROW(CONTROL, CaneCrop, Growth,
     &   Part, Out, WaterBal, SoilProp,
     &   YRPLT, CELLSE_DM)

c     Until N is explcitly modeled, N stress will be 1 (no stress)
      NSTRES = 1.

c     Canopy height
      CANHT = 0.

c     Leaf area index:
      XLAI   = 0.
      XHLAI  = 0.

c     Init water stresses:
c     ::::::::::::::::::::
          WaterBal%SWDF1 = 1.
          WaterBal%SWDF2 = 1.

c     Init potential soil and plant evaporation:
c     ::::::::::::::::::::::::::::::::::::::::::
          WaterBal%EOS   = 0.
          WaterBal%EOP   = 0.
          WaterBal%TRWUP = 0.

c     Coefficients for SWDF1, SWDF2
c     From the species file
c     :::::::::::::::::
          RWUEP1         = 1.
          RWUEP2         = 2.

          CALL GET_SPECIES_COEFF(RWUEP1,'RWUEP1', CONTROL, SPC_ERROR)
          CALL GET_SPECIES_COEFF(RWUEP2,'RWUEP2', CONTROL, SPC_ERROR)

          WaterBal%RWUEP1 = RWUEP1
          WaterBal%RWUEP2 = RWUEP2

          RECTIM = 0.

c         Critical soil water content?
c         ::::::::::::::::::::::::::::
c         Read from species file:
          CRITSW = 0.2
          CALL GET_SPECIES_COEFF(CRITSW,'CRITSW', CONTROL, SPC_ERROR)

c     Initialise population variables:
c     :::::::::::::::::::::::::::::::::::::::
c         READ from fileX!!!      
c         Set the standard rowspacing
          rowspc = 1.4
          CALL FIND_INP(ROWSPC, 'ROWSPC', Control)
          
c         MJ, March 2010: check rowspacing
c         ::::::::::::::::::::::::::::::::
c         Issue a warning if row-spacing is less than 3 cm.  The reason
c         being that someone might inadvertantly enter 1.5, thinking the input
c         is metres.  Rowspacing is unlikely to exceed 3 m, hence this threshold.
c         Note: rowspc is converted as a special case from cm to m by the FIND_INP
c         routine.
          IF (ROWSPC .LT. .03) THEN
            WARNINGS(1) = "Warning: please check that " 
     &      // "row-spacing is specified in CENTIMETRES"
            WARNINGS(2) = "[Row-spacing appears to be very low]"
            CALL WARNING(2, "SCCAN"//ModelVerTxt//": SCCNGRO", WARNINGS)
          ENDIF          


c     Initialise CaneCrop object:
c     :::::::::::::::::::::::::::
c     Is this a plant/ratoon crop?
          CALL FIND_INP(RATNUM, 'RATOON', Control)
c         If it is a plant crop
          IF (RATNUM .LT. 1.) THEN 
              CaneCrop%RATOON = 0
          ELSE
              CaneCrop%RATOON = CaneCrop%RATOON + INT(RATNUM)
          ENDIF

c         Is this a carryover ratoon?
c         :::::::::::::::::::::::::::
              CALL CARRYOVER_RATOON(Control, CaneCrop%RATOON, 
     &                              CaneCrop%CARRY_OVER)



c         Matthew!  Remember to get it to respond somehow to this!
c          IF (CaneCrop%CARRY_OVER) 
c     &              WRITE(*, *) 'Carrying ratoon info over.'

c     This subroutine sets a number of initial values for parameters
c     relating to the canopy / stalk population
      CALL InitCaneCrop(CaneCrop)

c     Initialise phenology variables:
c     ::::::::::::::::::::::::::::::::::::::
      CALL SC_PHENOLOGY(CONTROL, CaneCrop, CHU10, HU10, 
     &                        CHU16, HU16, CHUBaseLeaf, HUBaseLeaf, 
     &                        CHUBaseEm, HUBaseEm, CHUBasePop, 
     &                        HUBasePop,  HU_CANESIM, CHU_CANESIM, Out, 
     &                        TT_EMERG, EMERGED, STGDOY,  
     &                        TBasePop, TBaseLeaf, TBaseEm, Climate)


c     Initialise stalk population variables:
c     ::::::::::::::::::::::::::::::::::::::
c     *** Should CHU16 be replaced with CHUBaseLeaf?? ***
      CALL POPLT3 (CHUBasePop+HUBasePop,t0,pplast,newbat,rowspc,
     &             stdaye, stddiv,WATERBAL, CANECROP, SOIL, ISWITCH, 
     &             CONTROL, STGDOY, HUBaseEm, Growth%LI)
c         ::::::::::::::::::::::::::::::::::::::


c     Init canopy variables:
c     ::::::::::::::::::::::
      CALL CANOP3(CHUBaseEm,CHUBasePop,IWS,STDAYC,
     &  SENESF, expla,PER,KTRANS,F_SL, pplast,newbat,stdaye, 
     &  stddiv, reset, rowspc,strpop,BEST, 
     &  LODGE_LI, CLIMATE, CaneCrop, Soil, Growth, WaterBal,
     &  CONTROL, ISWITCH)


c     Init Photosynthesis:
c     ::::::::::::::::::::
          Growth%GRORT  = 0.
c         Check this...
          Growth%GERMDY = 0.    
      
          SLPF = SOILPROP % SLPF  !CHP 2/14/2011
          CALL SC_PHOTOS(CWSI,MDL,
     &       WaterBal%ANAERF,
     &      IWS,NEGGRO,DWDT,coderd,
     &      RUNOFF, allowlodg, LODGE_LI, Part, Growth, 
     &      Climate, SLPF,
     &      Out, ISWITCH, Control, CO2, DSUCyest, RM_AVG, TMEAN_AVG)
          


c     Initialise Partitioning variables
c     ::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL SC_PARTIT (WaterBal, Part, Climate, CaneCrop, Growth, 
     &             Out, Control, 
     &             STGDOY, DSUCyest, CELLSE_DM, CHUBaseLeaf)
c     ::::::::::::::::::::::::::::::::::::::::::::::::::


c     Initialise root growth variables:
c     :::::::::::::::::::::::::::::::::
c         Init RLV:
          RLV = 0.
          Growth%RTCMPG = 0.
          Soil%WR = SoilProp%WR
          CALL ROOTGROWTH(Control, ISWITCH, Soil, WaterBal, 
     &                     Growth, Climate, CaneCrop, RWUMX)

c         Set RLV (subroutine output to DSSAT CSM):
          RLV = WaterBal%RLV
          WaterBal%ANAERF = 1.0
c     :::::::::::::::::::::::::::::::::

c HBD (Jan 2023) after MvdL 2011
c     Init Nitrogen variables
c         If N is simulated:
          IF (ISWITCH % ISWNIT .NE. 'N') THEN
              CALL SC_NITRO(ISWITCH, Part%TOPDM, Out%ROOTDM, Part%STKDM, 
     &                      Out%TRASDM, NO3, NH4,UNO3, UNH4, RLV,
     &                    SW, N_STRESS, SoilProp, Control,
     &                    CaneCrop, SENESCE, ROOTNCONC, TOP_N, YRPLT)
          ENDIF
c     :::::::::::::::::::::::

c     :::::::::::::::::::::::::::::::::

!     chp added 2/13/07
      call SC_OPHARV(CONTROL, ISWITCH, 
     &    CaneCrop, EMERGED, Growth, Part, Out, MAXLAI,    !Input
     &    WaterBal%SWDF1, WaterBal%SWDF2, 
     &    STGDOY,
     &    XLAI, YRPLT,
     &    HARVFRAC, TOP_N)     !Input  ! HBD (Jan 2023) after MvdL 2011


!            CALL SC_UM_WSTRESS(
!     &        Control, ISWITCH, SoilProp, Weather, ! DSSAT CSM inputs
!     &        SW, TRWUP, EO, Growth%LAI,          ! Plant and environ. inputs
!     &        WaterBal%SWDF1, WaterBal%SWDF2, SWDF30, ! Water stress outputs
!     &        CaneCrop%CWSI, STDAYC, RLV   
!     &     )


c     Call the climate change summary output file subroutine (module).
      CALL SC_CCOUT(CONTROL, Part%AERLDM, Part%STKDM, Part%SUCMAS, 
     &  IRRAMT, Weather%RAIN, WaterBal%SWDF1, TMIN, 
     &  Part%TOPDM, Out%TRASDM, Part%STKWM, Out%PAR, 
     &  Growth%Li, EP, ES)
     
c     Call the climate change summary output file subroutine (module).
      CALL SC_ETOUT(CONTROL, 
     &  Growth%Li, EP, ES, EO, YRPLT, XHLAI, EORATIO, YREND)

c	Water logging stress 
      PORMIN = 0.05 
      CALL GET_SPECIES_COEFF(PORMIN,'PORM', CONTROL, SPC_ERROR) 
      ! init waterlogging stress
      !WRITE(*, '(A, F10.5)') 'PORMIN is ', PORMIN
      DO I=1, NL
        TSS(I) = 0.0
      ENDDO
      SATFAC = 0.0

c     END of SEASINIT
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c
c     &----------------------------------------------------
c
c              DYNAMIC = RATE
c
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
      ELSEIF(DYNAMIC.EQ.RATE) THEN

c     DSSAT-Canegro init.:
c     ::::::::::::::::::::::
c         Solar radiation
          Climate%SOLRAD = Weather%SRAD
c         Max and min temperatures
          Climate%TEMPMX = TMAX
          Climate%TEMPMN = TMIN

c         Windspeed
          Climate%WINDSP = Weather%WINDSP

c         Rainfall:
          WaterBal%PRECIP = Weather%RAIN
          Climate%RAIN = Weather%RAIN

c     Calculate heat units, etc, with the phenology routine:
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL SC_PHENOLOGY(CONTROL, CaneCrop, CHU10, HU10, 
     &                        CHU16, HU16, CHUBaseLeaf, HUBaseLeaf, 
     &                        CHUBaseEm, HUBaseEm, CHUBasePop, 
     &                        HUBasePop,  HU_CANESIM, CHU_CANESIM, Out, 
     &                        TT_EMERG, EMERGED, STGDOY,  
     &                        TBasePop, TBaseLeaf, TBaseEm, Climate)


c     Calculate water stresses:
c     :::::::::::::::::::::::::::
c         Only call this sub if the crop has emerged:
c         :::::::::::::::::::::::::::::::::::::::::::
!          IF (EMERGED) THEN
!            CALL SC_UM_WSTRESS(
!     &        Control, ISWITCH, SoilProp, Weather, ! DSSAT CSM inputs
!     &        SW, TRWUP, EO, Growth%LAI,          ! Plant and environ. inputs
!     &        WaterBal%SWDF1, WaterBal%SWDF2, SWDF30, ! Water stress outputs
!     &        CWSI, STDAYC, RLV
!     &     )
!          CaneCrop%CWSI = CWSI
!         ENDIF

c     Calculate water stresses:
c     :::::::::::::::::::::::::::
c     Only call this sub if the crop has emerged:
c     :::::::::::::::::::::::::::::::::::::::::::
      IF (EMERGED) THEN
        IF (ISWITCH%ISWWAT .EQ. 'N') THEN
c         Water balance is NOT used
          WaterBal%SWDF1 = 1.0
          WaterBal%SWDF2 = 1.0
          SATFAC = 0.0
        ELSE
          WaterBal%EOP = EOP
          WaterBal%EOS = EOS
          WaterBal%TRWUP = TRWUP

c         If (1) the water balance is simulated
c            (2) Potential transpiration > 0.
c            (3) LAI > 1.E-4, calculate stresses:
c         ::::::::::::::::::::::::::::::::::::::::::::::::::::::
          IF ((EOP .LT. 0.00001) .OR. 
     &      .NOT.(Growth%LAI .GT. 0.0001)) THEN
c           If there is NO potential transp., there can be no water stress...
            WaterBal%SWDF1 = 1.0
            WaterBal%SWDF2 = 1.0
            WaterBal%ANAERF = 1.0
            SATFAC = 0.0
          ELSE
c           Init:
            WaterBal%SWDF1 = 1.0
            WaterBal%SWDF2 = 1.0
            EP1 = EOP * 0.1

c           SWDF1 
            WaterBal%SWDF1 = MIN(1., (1./RWUEP1* TRWUP/EP1))
c           SWDF2
            WaterBal%SWDF2 = MIN(1., (1./RWUEP2 * TRWUP/EP1))
            
c           Find an equivalent value from DSSAT!
            ! WaterBal%ANAERF = 1.
            ! MJ, Jan 2018: Calculate anaerobic stress factor
!            2023-03-31 HBD Removed hardwired PORM species coeff.
!            PORMIN = 0.20
!            CALL SC_ANAERF(
!     &        Soil%NLAYR, RLV, SW, Soil%DUL, Soil%SAT, 
!     &        Soil%DLAYR, CONTROL, WaterBal%ANAERF)
!c           SCV    = 0.2
!            WRITE(*, '(F10.5)') WaterBal%ANAERF

          !-------------------------------------------------------------
          !      Compute Water Saturation Factors       
          ! ------------------------------------------------------------
          ! taken from the CERES-Maize model, MZ_GROSUB.for
          ! Added by MJ, Jan 2018
          ! This is meant as a sort of fudge.  I will adjust PORMIN 
          ! until I get a similar fit to v4.5 with the AquaCrop 
          ! waterlogging stress routine.
          

          SATFAC = 0.0    
          SUMEX = 0.0
          SUMRL = 0.0
      
          DO L = 1, SoilProp%NLAYR

          !------------------------------------------------------------
          !PORMIN = Minimum pore space required for supplying oxygen to 
          !         roots for optimum growth and function    
          !TSS(L) = Number of days soil layer L has been saturated 
          !         above PORMIN
          !------------------------------------------------------------
              IF ((SOILPROP%SAT(L)-SW(L)) .GE. PORMIN) THEN
                  TSS(L) = 0.
              ELSE
                  TSS(L) = TSS(L) + 1.
              ENDIF
          !------------------------------------------------------------
          ! Delay of 2 days after soil layer is saturated before root
          ! water uptake is affected
          !------------------------------------------------------------
              IF (TSS(L) .GT. 2.) THEN
                  SWEXF = (SOILPROP%SAT(L)-SW(L))/PORMIN
                  SWEXF = MAX(SWEXF,0.0)
              ELSE
                  SWEXF = 1.0
              ENDIF

              SWEXF = MIN(SWEXF,1.0)
              SUMEX  = SUMEX + SOILPROP%DLAYR(L)*RLV(L)*(1.0 - SWEXF)
              SUMRL  = SUMRL + SOILPROP%DLAYR(L)*RLV(L)
          ENDDO

          IF (SUMRL .GT. 0.0) THEN
              SATFAC = SUMEX/SUMRL
          ELSE
              SATFAC = 0.0
          ENDIF
          SATFAC = AMAX1(SATFAC,0.0)
          SATFAC = AMIN1(SATFAC,1.0)
          ! WaterBal%ANAERF is passed to SC_PHOTOS and reduces
          ! photosynthesis rates.
          ! The same calculation is done in the ROOTWU routine
          ! (SATFAC reduces water uptake), and SWDF1 (and SWDF2)
          ! are calculated a function of total root water uptake
          ! and potential transp. - so this is maybe
          ! double-counting?  (Comment in MZ_GROSUB confirms this)
          ! WaterBal%ANAERF = 1.0 - SATFAC
          ! In CERES Maize, SATFAC is used in the calculation of
          ! leaf area growth, but not in photosynthesis.  Leaf
          ! area in growth in Canegro is limited by SWDF2, which
          ! includes some effect of SATFAC in ROOTWU... but there
          ! might be an argument for an additional effect of
          ! SATFAC on leaf elongation rate?
                      
          ENDIF
        ENDIF
      ENDIF

c HBD (Jan 2023) after MvdL 2011
c     Calculate Nitrogen stress
c         If N is simulated:
          IF (ISWITCH % ISWNIT .NE. 'N') THEN
              CALL SC_NITRO(ISWITCH, Part%TOPDM, Out%ROOTDM, Part%STKDM, 
     &                      Out%TRASDM, NO3, NH4,UNO3, UNH4, RLV,
     &                    SW, N_STRESS, SoilProp, Control,
     &                    CaneCrop, SENESCE, ROOTNCONC, TOP_N, YRPLT)
          ENDIF
c     :::::::::::::::::::::::

c     Calculate stalk population:
c     :::::::::::::::::::::::::::
      IF (EMERGED) THEN
c       T0 contains the previous day's thermal time value
c       *** Should CHU16 be replaced with CHUBaseLeaf?? ***
        CALL POPLT3 (CHUBasePop+HUBasePop,t0,pplast,newbat,rowspc,
     &             stdaye, stddiv,WATERBAL, CANECROP, SOIL, ISWITCH, 
     &             CONTROL, STGDOY, HUBaseEm, Growth%LI)
      ENDIF
c     :::::::::::::::::::::::::::


c     Do canopy calculations:
c     :::::::::::::::::::::::
c     Only call this sub if the crop has emerged:
c     :::::::::::::::::::::::::::::::::::::::::::
      IF (EMERGED) THEN
c  *          Should it be CHU10?? *
c             What is thermal time?
c             CHUBaseEm+HUBaseEm
c             Note to Matt: CHU10 is calculated AFTER emergence only; 
c             CHUBaseEm is calculated from start of planting. 
c             MJ, 2007-07-02: replaced CHU10+HU10 with CHUBaseLeaf + HUBaseLeaf
          CALL CANOP3(CHUBaseLeaf + HUBaseLeaf,CHUBasePop,IWS,STDAYC,
     &      SENESF, expla,PER,KTRANS,F_SL, pplast,newbat,stdaye,
     &      stddiv, reset, rowspc,strpop,BEST, 
     &      LODGE_LI, CLIMATE, CaneCrop, Soil, Growth, WaterBal,
     &      CONTROL, ISWITCH)

c         Set the canopy height for output:
c         :::::::::::::::::::::::::::::::::
          CANHT = CaneCrop%CANHEIGHT

c         Set the LAI for output:
c         :::::::::::::::::::::::
          XLAI = Growth%TLAI
          XHLAI = Growth%LAI
      
!         CHP added 2/13/07  I didnt see this elsewhere, but
!           replace if already exists.  Needed for Summary.out
            IF (Growth%LAI > MAXLAI) MAXLAI = Growth%LAI
      ENDIF      
c     :::::::::::::::::::::::    

c     Do photosynthesis calculations:
c     :::::::::::::::::::::::::::::::
c       Initialise PHOTOS variables
c       :::::::::::::::::::::::::::


c         For now...
c#        Read from XLAT?
          ALAT   = 30.
c         Amount of runoff today - for lodging calculations
          RUNOFF = 0.

c         Allow lodging
c         ::::::::::::::
c         Check for an 'implicit' switch (if lodgerating > 0)
c         Lodging is enabled unless cultivar indicates that
c         no lodging will happen (lodge rating is 0)
          ALLOWLODG = 1        

c       if there are leaves, call the photosynthesis routine:
c       :::::::::::::::::::::::::::::::::::::::::::::::::::::
        IF (Growth%LAI .GT. 0.001) THEN

C         Here we allow for a gradual return to nonstressed photosynthesis if 
c         stress has been severe.
          IF (ISWITCH%ISWWAT .EQ. 'Y') THEN
c             If water balance is used:          
              RECTIM = RECTIM + Out%HUPI
              IF((WaterBal%SWDF1 .LE. CRITSW) .AND. 
     &               (Growth%LAI .GT. 0.5)) THEN
                  RECTIM = 0.
                  SWDFMN = WaterBal%SWDF1
              ENDIF

c             CWSI is a photosynthesis reduction factor
              CWSI=AMIN1(SWDFMN+RECTIM/HuRecover,WaterBal%SWDF1)
              
              ! HBD (Jan 2023) after MvdL 2011   
              !MvdL: change made here April 2010
		  IF (ISWITCH%ISWNIT .EQ. 'Y') THEN                     
                  CWSI=AMIN1(SWDFMN+RECTIM/HuRecover,WaterBal%SWDF1)

! HBD May 2023: had to add this to avoid early null growth for some experiments
!               during tests with available default experiments
!               (i.e. in SATO8902)
                  if(Growth%LAI .LT. 0.01) N_STRESS=1

	            CWSI=CWSI * N_STRESS

! HBD Apr 2023: wonder if this should be evaluated (= CERES)
!               it may miss interactions of N x W
!                 CWSI=AMIN1(SWDFMN+RECTIM/HuRecover,
!     &               WaterBal%SWDF1,N_STRESS) 
	        ENDIF  

          ELSE
              CWSI = 1.
          ENDIF

c         Assign CWSI to canecrop variable:
          Canecrop%CWSI = CWSI

c        ... and calculate actual photosynthesis
         CALL SC_PHOTOS(CWSI,MDL,
     &    WaterBal%ANAERF,
     &   IWS,NEGGRO,DWDT,coderd,
     &   RUNOFF, allowlodg, LODGE_LI, Part, Growth, 
     &   Climate, SLPF,
     &   Out, ISWITCH, Control, CO2, DSUCyest, RM_AVG, TMEAN_AVG)


        ENDIF


c     :::::::::::::::::::::::::::::::  

c     And then calculate how this should be partitioned:
c     ::::::::::::::::::::::::::::::::::::::::::::::::::
          IF (EMERGED) THEN
          CALL SC_PARTIT (WaterBal, Part, Climate, CaneCrop, Growth, 
     &             Out, Control, 
     &             STGDOY, DSUCyest, CELLSE_DM, CHUBaseLeaf)
          ENDIF
c     ::::::::::::::::::::::::::::::::::::::::::::::::::


c     Calculate Stalk Wet mass
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     CNB&AS May2001 START taking a shot at cane moisture content 
c     after Martines SASTA 2001
      	STKWAT = (3.607 * Part%STKDM) - (2.078 * Part%SUCMAS)
	    STKDMC = 0.3
          IF (Part%STKDM.GT.0.0) THEN
              STKDMC = Part%STKDM / (STKWAT + Part%STKDM)
          ENDIF
	    Part%STKWM = Part%STKDM / STKDMC   !IT WASSTKWM=STKDM/(0.3)
c     CNB&AS May2001 END taking a shot at cane moisture content
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

c     Having partitioned biomass to the roots, calculate
c     Root growth:
c     ::::::::::::

          CALL ROOTGROWTH(Control, ISWITCH, Soil, WaterBal, 
     &                    Growth, Climate, CaneCrop, RWUMX)

c     Set RLV (subroutine output to DSSAT CSM):
      RLV = WaterBal%RLV
c     ::::::::::::::::::::::::::::::::::::::::::::::::::
          
!      IF (INDEX('YDA',ISWITCH%IDETL) > 0) THEN
!        WRITE(SCLUN, '(I10, 2X, 2(F10.5, 2X))') Control%YRDOY, CHU10, 
!     &                                      CHUBaseEm
!      ENDIF
      
c     Call the evaporation related output file subroutine (module).
      CALL SC_ETOUT(CONTROL, 
     &  Growth%Li, EP, ES, EO, YRPLT, XHLAI, EORATIO, YREND)

c     END of RATE
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c
c     -----------------------------------------------------
c
c              DYNAMIC = INTEGRATE
c
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
      ELSEIF(DYNAMIC.EQ.INTEGR) THEN

!            CALL SC_UM_WSTRESS(
!     &        Control, ISWITCH, SoilProp, Weather, ! DSSAT CSM inputs
!     &        SW, TRWUP, EO, Growth%LAI,          ! Plant and environ. inputs
!     &        WaterBal%SWDF1, WaterBal%SWDF2, SWDF30, ! Water stress outputs
!     &        CaneCrop%CWSI, STDAYC, RLV 
!     &     )

          CALL SC_PHENOLOGY(CONTROL, CaneCrop, CHU10, HU10, 
     &                        CHU16, HU16, CHUBaseLeaf, HUBaseLeaf, 
     &                        CHUBaseEm, HUBaseEm, CHUBasePop, 
     &                        HUBasePop,  HU_CANESIM, CHU_CANESIM, Out, 
     &                        TT_EMERG, EMERGED, STGDOY,  
     &                        TBasePop, TBaseLeaf, TBaseEm, Climate)

c HBD (Jan 2023) after MvdL 2011
c TODO check if here is the right place to insert this chunk of code
c         If N is simulated:
          IF (ISWITCH % ISWNIT .NE. 'N') THEN
              CALL SC_NITRO(ISWITCH, Part%TOPDM, Out%ROOTDM, Part%STKDM, 
     &                      Out%TRASDM, NO3, NH4,UNO3, UNH4, RLV,
     &                    SW, N_STRESS, SoilProp, Control,
     &                    CaneCrop, SENESCE, ROOTNCONC, TOP_N, YRPLT)
          ENDIF
c     :::::::::::::::::::::::


          CALL POPLT3 (CHUBasePop+HUBasePop,t0,pplast,newbat,rowspc,
     &             stdaye, stddiv,WATERBAL, CANECROP, SOIL, ISWITCH, 
     &             CONTROL, STGDOY, HUBaseEm, Growth%LI)

c       Call the climate change summary output file subroutine (module).
      CALL SC_CCOUT(CONTROL, Part%AERLDM, Part%STKDM, Part%SUCMAS, 
     &  IRRAMT, Weather%RAIN, WaterBal%SWDF1, TMIN, 
     &  Part%TOPDM, Out%TRASDM, Part%STKWM, Out%PAR, 
     &  Growth%Li, EP, ES)
     
     
c        Call the evaporation related output file subroutine (module).
         CALL SC_ETOUT(CONTROL, 
     &       Growth%Li, EP, ES, EO, YRPLT, XHLAI, EORATIO, YREND)

c     END of INTEGRATE
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c
c
c              DYNAMIC = OUTPUT
c
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
      ELSEIF(DYNAMIC.EQ.OUTPUT) THEN

c     Write output of daily values:
c     :::::::::::::::::::::::::::::
          CALL SC_OPGROW(CONTROL, CaneCrop, Growth,
     & Part, Out, WaterBal, SoilProp,
     & YRPLT, CELLSE_DM)

!         chp 4/7/2009
          IF (CONTROL%YRDOY == YREND) THEN
            STGDOY(16) = YREND
            MDATE = YREND

c HBD (Jan 2023) after MvdL 2011
c     Output Nitrogen variables
c     :::::::::::::::::::::::
c         If N is simulated:
            IF (ISWITCH % ISWNIT .NE. 'N') THEN
              CALL SC_NITRO(ISWITCH, Part%TOPDM, Out%ROOTDM, Part%STKDM, 
     &                      Out%TRASDM, NO3, NH4,UNO3, UNH4, RLV,
     &                    SW, N_STRESS, SoilProp, Control,
     &                    CaneCrop, SENESCE, ROOTNCONC, TOP_N, YRPLT)
            ENDIF

            
          ENDIF

!         chp added 2/13/07
          call SC_OPHARV(CONTROL, ISWITCH, 
     &      CaneCrop, EMERGED, Growth, Part, Out, MAXLAI,  !Input
     &      WaterBal%SWDF1, WaterBal%SWDF2, 
     &      STGDOY,
     &      XLAI, YRPLT,
     &      HARVFRAC, TOP_N)   !Input  ! HBD (Jan 2023) after MvdL 2011
     
     
c     Call the evaporation related output file subroutine (module).
      CALL SC_ETOUT(CONTROL, 
     &  Growth%Li, EP, ES, EO, YRPLT, XHLAI, EORATIO, YREND)

c     END of OUTPUT
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c
c     -----------------------------------------------------
c
c              DYNAMIC = FINAL 
c
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
      ELSEIF(DYNAMIC.EQ.SEASEND) THEN


         CALL SC_PHOTOS(CWSI,MDL,
     &    WaterBal%ANAERF,
     &   IWS,NEGGRO,DWDT,coderd,
     &   RUNOFF, allowlodg, LODGE_LI, Part, Growth, 
     &   Climate, SLPF,
     &   Out, ISWITCH, Control, CO2, DSUCyest, RM_AVG, TMEAN_AVG)


c     Call the output routine, to close output files,
c     ::::::::::::::::::::::::::::::::::::::::::::::
          CALL SC_OPGROW(CONTROL, CaneCrop, Growth,
     & Part, Out, WaterBal, SoilProp,
     & YRPLT, CELLSE_DM)

!         chp added 2/13/07
          call SC_OPHARV(CONTROL, ISWITCH, 
     &      CaneCrop, EMERGED, Growth, Part, Out, MAXLAI,  !Input
     &      WaterBal%SWDF1, WaterBal%SWDF2, 
     &      STGDOY,
     &      XLAI, YRPLT, 
     &      HARVFRAC, TOP_N)   !Input  ! HBD (Jan 2023) after MvdL 2011


c     :::::::::::::::::::::::::::
c     Ratoon considerations
          CALL CARRYOVER_RATOON(Control, CaneCrop%RATOON, 
     &                          CaneCrop%CARRY_OVER)

c     Actual root information is put in carryover variable
c     in this subroutine (for now)
          CALL ROOTGROWTH(Control, ISWITCH, Soil, WaterBal, 
     &                    Growth, Climate, CaneCrop, RWUMX)

c     Call the climate change summary output file subroutine (module).
      CALL SC_CCOUT(CONTROL, Part%AERLDM, Part%STKDM, Part%SUCMAS, 
     &  IRRAMT, Weather%RAIN, WaterBal%SWDF1, TMIN, 
     &  Part%TOPDM, Out%TRASDM, Part%STKWM, Out%PAR, 
     &  Growth%Li, EP, ES)




c     Temporary!!
!      CLOSE(SCLUN)



c     END of FINAL
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c
c     -----------------------------------------------------
c     END of DYNAMIC conditional statement
c     ::::::::::::::::::::::::::::::::::::
      ENDIF
c     ::::::::::::::::::::::::::::::::::::


      END
c     -----------------------------------------------------
c     END of SUBROUTINE SC_CNGRO
c     -----------------------------------------------------
c     APPENDIX 1: DSSAT interface variables:
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c     - Input variables -
c     :::::::::::::::::::
c      CONTROL:  init, rate, etc 
c      ISWITCH:  Indicates which processes (Soil, N, etc) are to be modelled 
c      CO2:      Atmospheric CO2, ppm 
c      DAYL:     Day length on day of simulation (from sunrise to sunset) (hr) 
c      EOP:      Potential plant transpiration, mm/day 
c      EP:       Actual plant transpiration mm/day
c      HARVFRAC: Two-element array containing fractions of (1) yield harvested 
c                and (2) by-product harvested (fraction) 
c      NH4:      Ammonium in soil layer L, mg elemental N/kg soil 
c      NO3:      Nitrate in soil layer L (mg elemental N/kg soil) 
c      SNOW:     Snow accumulation today, mm 
c      SOILPROP: Soil properties (DUL, LL, etc) 
c      SRAD:     Total solar radiation today, MJ/m2   
c      Sw:       Volumetric soil water content of soil layer, cm3 water/cm3 soil    
c      TMAX:     Max temp., degrees C 
c      TMIN:     Min temp., degrees C 
c      TRWUP:    Total potential root water uptake, cm/day 
c      TWILEN:   TWILEN is Twilight to Twilight daylength 
c      YREND:    Year and day of year for harvest 
c      YRPLT:    Year and day of year of planting 
c     :::::::::::::::::::
c     - Output variables -
c     :::::::::::::::::::
c      CANHT:    Canopy height (m)
c      HARVRES:  Composite variable containing harvest residue amounts for total 
c                dry matter, lignin, and N amounts (kg/ha)
c      KCAN:     Canopy light extinction coefficient for daily PAR, for equidistant 
c                plant spacing, modified when in-row and between  row spacing are not equal
c      KTRANS:   Energy extinction coefficient for partitioning EO (potential plant 
c                transpiration) to EP (actual plant transpiration) [previously KEP]
c      MDATE:    Year and day of year of maturity
c      NSTRES:   Nitrogen stress factor (1=no stress, 0=max stress)
c      PORMIN:   Minimum pore space required for supplying oxygen to roots for optimal 
c                growth and function (cm3/cm3)
c      RLV:      Root length density for soil layer L (cm[root] / cm3[soil])
c      RWUMX:    Maximum root water uptake parameter from species file
c      SENESCE:  Composite variable containing data about daily senesced plant matter. 
c      STGDOY:   Year and day of year that a growth stage occurred on
c      UNH4:     Plant uptake of ammonium from layer (kg N/ha/day)
c      UNO3:     Plant uptake of nitrate from a layer (kg N/ha/day)
c      XLAI:     Leaf area index, m2/m2
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
