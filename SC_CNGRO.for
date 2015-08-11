c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c     SASRI CANEGRO
c     -------------
c     Matthew Jones, September - December 2006
c     Gainesville, Florida, USA
c     -----------------------------------------------------
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
c     http://sasri.sasa.org.za/misc/icsm.html
c     ::::::::::::::::::::::::::::::::::::::::
c     Acknowledgements and thanks to:
c     Dr. Abraham Singels (SASRI), Dr. Maurits van den Berg 
c     (SASRI), Dr. Jim Jones (UF), Cheryl Porter (UF), 
c     Jim Shine (SGC / UF), Gerrit Hoogenboom (UGA) 
c     ... and Geoff Inman-Bamber (CSIRO?), for creating the 
c     Sugarcane infrastructure in DSSAT
c     -----------------------------------------------------
c     -----------------------------------------------------



c     SUBROUTINE: SC_CNGRO()
c     ----------------------
c     This sub interfaces with the rest of the DSSAT CSM.
c     Inputs and outputs are passed solely through this
c     interface.  This forms the PLANT GROWTH MODULE for
c     sugarcane.  Soil and atmospheric processes, along 
c     with external calculations like irrigation are
c     handled OUTSIDE of this module.
c     [interface copied from MZ_CERES.for]
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SC_CNGRO (
     &    CONTROL, ISWITCH,                                   !Input
     &    CO2, DAYL, EOP, EP, EO, HARVFRAC, NH4, NO3, SNOW,   !Input
     &    SOILPROP, SRAD, SW, TMAX, TMIN, TRWUP, TRWU, EOS,   !Input
     &    RWUEP1, TWILEN, YREND, YRPLT, WEATHER,              !Input
     $    CANHT, HARVRES, KCAN, KTRANS, MDATE, NSTRES,        !Output
     &    PORMIN, RLV, RWUMX,SENESCE, STGDOY, UNH4,           !Output
     &    UNO3, XLAI, XHLAI, EORATIO)                         !Output
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c     Define DSSAT composite variables:
c     [Taken from MZ_CERES.for]
      USE ModuleDefs

c     CANEGRO composite variables:
c      USE CNG_ModuleDefs

c     Define CANEGRO composite variables:
      USE CNG_ModuleDefs
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c     No implicit typing
      IMPLICIT NONE
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c     Maintain the value of internal variables between 
c     calls to this subroutine (supports the modular 
c     control structure)
      SAVE
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c     DSSAT variable declarations (i.e. subroutine 
c     argument declarations)
c     Explanation of variables in Appendix 1 at the 
c     end of this subroutine.
c     :::::::::::::::::::::::
c     - Input variables -
c     :::::::::::::::::::
      REAL     CO2
      REAL     DAYL
      REAL     EOP
      REAL     EP, EP1, RWUEP1, RWUEP2
      REAL     EO, EOS
      REAL     HARVFRAC(2)
      REAL     NH4(NL)
      REAL     NO3(NL)
      REAL     SNOW
      REAL     SRAD
      REAL     SW(NL)
      REAL     TMAX
      REAL     TMIN
      REAL     TRWUP
      REAL     TRWU
      REAL     TWILEN
      INTEGER  YREND
      INTEGER	 YRPLT
c     - Output variables: -
c     :::::::::::::::::::::
      REAL     CANHT
      REAL     KCAN
      REAL     KTRANS
      INTEGER  MDATE
      REAL     NSTRES
      REAL     PORMIN
      REAL     RLV(NL)
      REAL     RWUMX
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
      Type (ResidueType) HARVRES 
      Type (ResidueType) SENESCE
      Type (WeatherType) WEATHER


c     - Local variables -
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

c     Thermal time requirements for phenology
c     Germination:
!      REAL TT_GERM
c     Emergence
      REAL TT_EMERG
      LOGICAL EMERGED, FILE_EXISTS

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
      CHARACTER TFILENAME*25
      
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
      INTEGER SCLUN   !CHP

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
      ENDDO
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
      


c     -----------------------------------------------------
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
!       EORATIO  =  0.0
!       KCAN  =  0.0
!       KTRANS  =  0.0
       NSTRES  =  1.
       PORMIN  =  0.0
       RLV  =  0.0
!       RWUMX  =  0.0
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



c     Which canopy to use?  Default will be set to Canesim
c     because of the simpler inputs (2)
      CANOPY_ROUTINE = CANESIM


c     Call the output routine, to initialise output
c     ::::::::::
          CALL SC_OPGROW(CONTROL, CaneCrop, Growth,
     - Part, Out, WaterBal, SW, SoilProp,
     - YRPLT)


c     TEMPORARY!
c     file for output
c      PAUSE
!      WRITE(TFILENAME, '(A, I3, A)') 'dssatCNG_run', CONTROL%RUN,
!     -                               '.txt'
!      OPEN(FILE=TFILENAME, unit=SCLUN, ACTION='WRITE', 
!     -     STATUS='REPLACE')
c      WRITE(SCLUN, '(A, I3)') 'Run number', CONTROL%RUN

      IF (INDEX('YDA',ISWITCH % IDETL) > 0) THEN
        TFILENAME = 'Work.OUT'
        CALL GETLUN('WORK.OUT', SCLUN)
        INQUIRE(FILE=TFILENAME, EXIST=FILE_EXISTS)

c       Open the file
        IF (FILE_EXISTS) THEN
c         In append mode if the file already exists
          OPEN(UNIT=SCLUN,FILE=TFILENAME,STATUS='OLD',POSITION='APPEND')
        ELSE
c         A new file if not existing
          OPEN (UNIT=SCLUN, FILE=TFILENAME, STATUS='NEW')
          WRITE(SCLUN,'("*CaneGro Supplemental Output File")')
        ENDIF

c       Output a header (treatment / run info, etc)
c     ::::::::::::::::::::::::
c         Use the predefined one:
        CALL HEADER(SEASINIT, SCLUN, CONTROL%RUN)
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
     &    XLAI, YRPLT)     !Input

c     :::::::::::::::::::::::::::

c     Initialise stalk population routine
c     :::::::::::::::::::::::::::::::::::
          CALL POPLT3 (CHUBasePop+HUBasePop,t0,pplast,newbat,rowspc,
     -             stdaye, stddiv,WATERBAL, CANECROP, SOIL, ISWITCH, 
     -             CONTROL, STGDOY)
c     :::::::::::::::::::::::::::::::::::


c     Initialise Partitioning variables
c     ::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL PARTIT (WaterBal, Part, Climate, CaneCrop, Growth, 
     &             Out, Control, ISWITCH,
     -             STGDOY)
c     ::::::::::::::::::::::::::::::::::::::::::::::::::


c     Initialise canopy
c     :::::::::::::::::
            CALL CANOP3(CHUBaseEm,CHUBasePop,IWS,STDAYC,
     -        SENESF, expla,PER,KTRANS,pplast,newbat,stdaye,stddiv, 
     -        reset, rowspc,strpop,BEST, 
     -        LODGE_LI, CLIMATE, CaneCrop, Soil, Growth, WaterBal,
     -        CONTROL, ISWITCH)
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
     &                          CaneCrop%CARRY_OVER)

c     Call the cultivar routine at least once:
          CALL GET_CULTIVAR_COEFF(cCOEFF, 'dummy', CONTROL, cERROR)
c          WRITE(*, '(A, F10.5)') 'Just read SNWT: ', cCOEFF


      
c     Set model number (CANEGRO dependency, to be removed eventually)
          MDL = 1
          
      RECTIM = 0.    
      CWSI = 0.
c      WRITE(*, '(A, F10.4)') 'CWSI is ', CWSI      



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
     - Part, Out, WaterBal, SW, SoilProp,
     - YRPLT)

c          TMIN  = 0.
c          TMAX  = 0.

c         Until N is explcitly modeled, N stress will be 1 (no stress)
          NSTRES = 1.

c         Canopy height
          CANHT = 0.

c         Leaf area index:
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
            CALL WARNING(2, "SCCAN046: SCCNGRO", WARNINGS)
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
c     -              WRITE(*, *) 'Carrying ratoon info over.'

c     This subroutine sets a number of initial values for parameters
c     relating to the canopy / stalk population
          CALL InitCaneCrop(CaneCrop, CONTROL, ISWITCH)

c     Initialise phenology variables:
c     ::::::::::::::::::::::::::::::::::::::
          CALL SC_PHENOLOGY(CONTROL, ISWITCH, CaneCrop, CHU10, HU10, 
     -                        CHU16, HU16, CHUBaseLeaf, HUBaseLeaf, 
     -                        CHUBaseEm, HUBaseEm, CHUBasePop, 
     -                        HUBasePop,  HU_CANESIM, CHU_CANESIM, Out, 
     -                        TT_EMERG, EMERGED, STGDOY,  
     -                        TBasePop, TBaseLeaf, TBaseEm, Climate)


c     Initialise stalk population variables:
c     ::::::::::::::::::::::::::::::::::::::
c     *** Should CHU16 be replaced with CHUBaseLeaf?? ***
          CALL POPLT3 (CHUBasePop+HUBasePop,t0,pplast,newbat,rowspc,
     -             stdaye, stddiv,WATERBAL, CANECROP, SOIL, ISWITCH, 
     -             CONTROL, STGDOY)
c         ::::::::::::::::::::::::::::::::::::::


c     Init canopy variables:
c     ::::::::::::::::::::::
c         Which canopy routine should be used?  This is determined
c         by the availability of cultivar inputs
          CALL GET_CANOPY_ROUTINE(CANOPY_ROUTINE, Control)
          
c         Now initialise the appropriate canopy routine
          IF (CANOPY_ROUTINE .EQ. CANEGRO) THEN
            CALL CANOP3(CHUBaseEm,CHUBasePop,IWS,STDAYC,
     -        SENESF, expla,PER,KTRANS,pplast,newbat,stdaye,stddiv, 
     -        reset, rowspc,strpop,BEST, 
     -        LODGE_LI, CLIMATE, CaneCrop, Soil, Growth, WaterBal,
     -        CONTROL, ISWITCH)
          ELSEIF (CANOPY_ROUTINE .EQ. CANESIM) THEN
            CALL CANESIM_CANOPY (CHUBaseEm, CHUBasePop, IWS, STDAYC,
     -        PER, KTRANS, NEWBAT, STDAYE, STDDIV, RESET,
     -        ROWSPC, STRPOP, BEST, LODGE_LI, CLIMATE,  
     -        CaneCrop, Soil, Growth, WaterBal, CONTROL, 
     -        ISWITCH)
          ENDIF



c     Init Photosynthesis:
c     ::::::::::::::::::::
          Growth%GRORT  = 0.
c         Check this...
          Growth%GERMDY = 0.    
      
          SLPF = SOILPROP % SLPF  !CHP 2/14/2011
          CALL PHOTOS(CWSI,MDL,
     -       WaterBal%ANAERF,
     -      ALAT,IWS,NEGGRO,DWDT,coderd,CaneCrop%CANHEIGHT,
     -      rowspc,RUNOFF, allowlodg, LODGE_LI, Part, Growth, Climate, 
     -      SLPF,
     -      Out, ISWITCH, Control, CO2)
          


c     Initialise Partitioning variables
c     ::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL PARTIT (WaterBal, Part, Climate, CaneCrop, Growth, 
     &             Out, Control, ISWITCH,
     -             STGDOY)
c     ::::::::::::::::::::::::::::::::::::::::::::::::::


c     Initialise root growth variables:
c     :::::::::::::::::::::::::::::::::
c         Init RLV:
          RLV = 0.
          Growth%RTCMPG = 0.
          Soil%WR = SoilProp%WR
          CALL ROOTGROWTH(Control, ISWITCH, Soil, WaterBal, 
     -                     Growth, Climate, CaneCrop, RWUMX, HU16)

c         Set RLV (subroutine output to DSSAT CSM):
          RLV = WaterBal%RLV
c     :::::::::::::::::::::::::::::::::

c     :::::::::::::::::::::::::::::::::

!     chp added 2/13/07
      call SC_OPHARV(CONTROL, ISWITCH, 
     &    CaneCrop, EMERGED, Growth, Part, Out, MAXLAI,    !Input
     &    WaterBal%SWDF1, WaterBal%SWDF2, 
     &    STGDOY,
     &    XLAI, YRPLT)     !Input

c     END of SEASINIT
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c
c     -----------------------------------------------------
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

c     Calculate heat units, etc, with the phenology routine:
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL SC_PHENOLOGY(CONTROL, ISWITCH, CaneCrop, CHU10, HU10, 
     -                        CHU16, HU16, CHUBaseLeaf, HUBaseLeaf, 
     -                        CHUBaseEm, HUBaseEm, CHUBasePop, 
     -                        HUBasePop,  HU_CANESIM, CHU_CANESIM, Out, 
     -                        TT_EMERG, EMERGED, STGDOY,  
     -                        TBasePop, TBaseLeaf, TBaseEm, Climate)


c     Calculate water stresses:
c     :::::::::::::::::::::::::::
c         Only call this sub if the crop has emerged:
c         :::::::::::::::::::::::::::::::::::::::::::
          IF (EMERGED) THEN
              IF (ISWITCH%ISWWAT .EQ. 'N') THEN
c                 Water balance is NOT used
c                 :::::::::::::::::::::::::
                  WaterBal%SWDF1 = 1.
                  WaterBal%SWDF2 = 1.
              ELSE
                  WaterBal%EOP = EOP
                  WaterBal%EOS = EOS
                  WaterBal%TRWUP = TRWUP
c                 If (1) the water balance is simulated
c                    (2) Potential transpiration > 0.
c                    (3) LAI > 1.E-4, calculate stresses:
c                 [Point 3 is as result of a threshold value for LAI
c                 that limits the calculation of EP - see SPAM.for, 331]
c                 ::::::::::::::::::::::::::::::::::::::::::::::::::::::
                  IF ((EOP .LT. 0.00001) .OR. 
     -                 .NOT.(Growth%LAI .GT. 0.0001)) THEN
c                     If there is NO potential transp., there can be no
c                     water stress...
                      WaterBal%SWDF1 = 1.
                      WaterBal%SWDF2 = 1.
                  ELSE
c                     Init:
                      WaterBal%SWDF1 = 1.
                      WaterBal%SWDF2 = 1.
                      EP1 = EOP * .1

c                     SWDF1
c                     :::::
c                          WaterBal%SWDF1 = MIN(1., TRWUP/(1.* EOP*.1))
                          WaterBal%SWDF1 
     -                           = MIN(1., (1./RWUEP1* TRWUP/EP1))
c                     SWDF2
c                     :::::
c                         Matthew, move 2. to cultivar / species file!
c                          WaterBal%SWDF2 = MIN(1., TRWUP/(2.*EOP*.1))
                          WaterBal%SWDF2
     -                           = MIN(1., (1./RWUEP2 * TRWUP/EP1))
                  ENDIF
              ENDIF

          ENDIF

c     Calculate stalk population:
c     :::::::::::::::::::::::::::
c         Only call this sub if the crop has emerged:
c         :::::::::::::::::::::::::::::::::::::::::::
          IF (EMERGED) THEN
c             T0 contains the previous day's thermal time value
c             *** Should CHU16 be replaced with CHUBaseLeaf?? ***
          CALL POPLT3 (CHUBasePop+HUBasePop,t0,pplast,newbat,rowspc,
     -             stdaye, stddiv,WATERBAL, CANECROP, SOIL, ISWITCH, 
     -             CONTROL, STGDOY)
          ENDIF
c     :::::::::::::::::::::::::::


c     Do canopy calculations:
c     :::::::::::::::::::::::
c         Only call this sub if the crop has emerged:
c         :::::::::::::::::::::::::::::::::::::::::::
          IF (EMERGED) THEN
c  *          Should it be CHU10?? *
c             What is thermal time?
c             CHUBaseEm+HUBaseEm
c             Note to Matt: CHU10 is calculated AFTER emergence only; 
c             CHUBaseEm is calculated from start of planting. 
c             CALL CANOP3(CHU10+HU10, CHUBasePop, IWS, 
c     -         STDAYC, SENESF, expla, PER, KTRANS, pplast, newbat,  
c     -         stdaye,stddiv, reset, rowspc, strpop, BEST, 
c     -         LODGE_LI, CLIMATE, CaneCrop, Soil, Growth, WaterBal,
c     -         CONTROL, ISWITCH)

          IF (CANOPY_ROUTINE .EQ. CANEGRO) THEN
c             MJ, 2007-07-02: replaced CHU10+HU10 with CHUBaseLeaf + HUBaseLeaf
            CALL CANOP3(CHUBaseLeaf + HUBaseLeaf,CHUBasePop,IWS,STDAYC,
     -        SENESF, expla,PER,KTRANS,pplast,newbat,stdaye,stddiv, 
     -        reset, rowspc,strpop,BEST, 
     -        LODGE_LI, CLIMATE, CaneCrop, Soil, Growth, WaterBal,
     -        CONTROL, ISWITCH)
          ELSEIF (CANOPY_ROUTINE .EQ. CANESIM) THEN
c           MJ, 2007-07-17: replace CHUBASEPOP with CHU_CANESIM
            CALL CANESIM_CANOPY (CHUBaseLeaf + HUBaseLeaf, CHU_CANESIM, 
     -        IWS, STDAYC,
     -        PER, KTRANS, NEWBAT, STDAYE, STDDIV, RESET,
     -        ROWSPC, STRPOP, BEST, LODGE_LI, CLIMATE,  
     -        CaneCrop, Soil, Growth, WaterBal, CONTROL, 
     -        ISWITCH)
          ENDIF


c             Set the canopy height for output:
c             :::::::::::::::::::::::::::::::::
              CANHT = CaneCrop%CANHEIGHT

c             Set the LAI for output:
c             :::::::::::::::::::::::
              XLAI = Growth%TLAI
              XHLAI = Growth%LAI
      
!             CHP added 2/13/07  I didn't see this elsewhere, but
!                 replace if already exists.  Needed for Summary.out
C             IF (XLAI > MAXLAI) MAXLAI = XLAI
              IF (Growth%LAI > MAXLAI) MAXLAI = Growth%LAI
          ENDIF      
c     :::::::::::::::::::::::    

c     Do photosynthesis calculations:
c     :::::::::::::::::::::::::::::::
c       CAREL - to provide for coderd=3 the following two statements:
c	  if(coderd.eq.3.0.and.solrad.lt.-90.0)coderd=0.0
c	  if(coderd.eq.3.0.and.solrad.gt.-20.0)coderd=2.0

c       Initialise PHOTOS variables
c       :::::::::::::::::::::::::::
c         Find an equivalent value from DSSAT!
          WaterBal%ANAERF = 1.
c          SCV    = 0.2

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
     -               (Growth%LAI .GT. 0.5)) THEN
                  RECTIM = 0.
                  SWDFMN = WaterBal%SWDF1
              ENDIF

c             CWSI is a photosynthesis reduction factor
              CWSI=AMIN1(SWDFMN+RECTIM/HuRecover,WaterBal%SWDF1)
          ELSE
              CWSI = 1.
          ENDIF

c         Assign CWSI to canecrop variable:
          Canecrop%CWSI = CWSI

c        ... and calculate actual photosynthesis
         CALL PHOTOS(CWSI,MDL,
     -    WaterBal%ANAERF,
     -   ALAT,IWS,NEGGRO,DWDT,coderd,CaneCrop%CANHEIGHT,
     -   rowspc,RUNOFF, allowlodg, LODGE_LI, Part, Growth, Climate, 
     -   SLPF,
     -   Out, ISWITCH, Control, CO2)


        ENDIF


c     :::::::::::::::::::::::::::::::  

c     And then calculate how this should be partitioned:
c     ::::::::::::::::::::::::::::::::::::::::::::::::::
          IF (EMERGED) THEN
          CALL PARTIT (WaterBal, Part, Climate, CaneCrop, Growth, 
     &             Out, Control, ISWITCH,
     -             STGDOY)
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
     -                    Growth, Climate, CaneCrop, RWUMX, HU16)

c     Set RLV (subroutine output to DSSAT CSM):
      RLV = WaterBal%RLV
c     ::::::::::::::::::::::::::::::::::::::::::::::::::
          

c      WRITE(SCLUN, '(I10, 2X, 17(F10.5, 2X)))') Control%YRDOY, 
c     -       EO, EOP, EOS, WaterBal%SWDF1, WaterBal%SWDF2,
c     -       DOT_PRODUCT(SW(1:3), Soil%DLAYR(1:3)), 
c     -       DOT_PRODUCT(SW(1:20), Soil%DLAYR(1:20)),
c     -       SW(1:10)
      IF (INDEX('YDA',ISWITCH%IDETL) > 0) THEN
        WRITE(SCLUN, '(I10, 2X, 2(F10.5, 2X))') Control%YRDOY, CHU10, 
     -                                      CHUBaseEm
      ENDIF

c     END of RATE
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c
c     -----------------------------------------------------
c
c              DYNAMIC = INTEGRATE
c
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
      ELSEIF(DYNAMIC.EQ.INTEGR) THEN

          CALL SC_PHENOLOGY(CONTROL, ISWITCH, CaneCrop, CHU10, HU10, 
     -                        CHU16, HU16, CHUBaseLeaf, HUBaseLeaf, 
     -                        CHUBaseEm, HUBaseEm, CHUBasePop, 
     -                        HUBasePop,  HU_CANESIM, CHU_CANESIM, Out, 
     -                        TT_EMERG, EMERGED, STGDOY,  
     -                        TBasePop, TBaseLeaf, TBaseEm, Climate)


c     END of INTEGRATE
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c
c     -----------------------------------------------------
c
c              DYNAMIC = OUTPUT
c
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
      ELSEIF(DYNAMIC.EQ.OUTPUT) THEN

c     Write output of daily values:
c     :::::::::::::::::::::::::::::
          CALL SC_OPGROW(CONTROL, CaneCrop, Growth,
     - Part, Out, WaterBal, SW, SoilProp,
     - YRPLT)

!         chp 4/7/2009
          IF (CONTROL%YRDOY == YREND) THEN
            STGDOY(16) = YREND
            MDATE = YREND
          ENDIF

!         chp added 2/13/07
          call SC_OPHARV(CONTROL, ISWITCH, 
     &      CaneCrop, EMERGED, Growth, Part, Out, MAXLAI,  !Input
     &      WaterBal%SWDF1, WaterBal%SWDF2, 
     &      STGDOY,
     &      XLAI, YRPLT)   !Input

c     END of OUTPUT
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c
c     -----------------------------------------------------
c
c              DYNAMIC = FINAL 
c
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
      ELSEIF(DYNAMIC.EQ.SEASEND) THEN


         CALL PHOTOS(CWSI,MDL,
     -    WaterBal%ANAERF,
     -   ALAT,IWS,NEGGRO,DWDT,coderd,CaneCrop%CANHEIGHT,
     -   rowspc,RUNOFF, allowlodg, LODGE_LI, Part, Growth, Climate, 
     -   SLPF,
     -   Out, ISWITCH, Control, CO2)


c     Call the output routine, to close output files,
c     ::::::::::::::::::::::::::::::::::::::::::::::
          CALL SC_OPGROW(CONTROL, CaneCrop, Growth,
     - Part, Out, WaterBal, SW, SoilProp,
     - YRPLT)

!         chp added 2/13/07
          call SC_OPHARV(CONTROL, ISWITCH, 
     &      CaneCrop, EMERGED, Growth, Part, Out, MAXLAI,  !Input
     &      WaterBal%SWDF1, WaterBal%SWDF2, 
     &      STGDOY,
     &      XLAI, YRPLT)   !Input


c     :::::::::::::::::::::::::::
c     Ratoon considerations
          CALL CARRYOVER_RATOON(Control, CaneCrop%RATOON, 
     &                          CaneCrop%CARRY_OVER)

c     Actual root information is put in carryover variable
c     in this subroutine (for now)
          CALL ROOTGROWTH(Control, ISWITCH, Soil, WaterBal, 
     -                    Growth, Climate, CaneCrop, RWUMX, HU16)

c         Temporary!!
          CLOSE(SCLUN)
!          CLOSE(23)


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
c      CONTROL:  ‘init’, ‘rate’, etc 
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
c     -----------------------------------------------------