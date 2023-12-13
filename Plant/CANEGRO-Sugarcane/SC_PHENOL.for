c     ===============================================================
c     This subroutine performs all of the calculations necessary for
c     the implicit phenology calculations that go on throughout the
c     CANEGRO Plant Module.
c     The calculations include initialisations and thermal time
c     calculations.
c     ::::::::::::::::::::::::::::::::::::::::
c     Matthew Jones, September-November 2006
c     ::::::::::::::::::::::::::::::::::::::::
c     ----------------------------------------------------------

      SUBROUTINE SC_PHENOLOGY(CONTROL, CaneCrop, CHU10, HU10, 
     -                        CHU16, HU16, CHUBaseLeaf, HUBaseLeaf, 
     -                        CHUBaseEm, HUBaseEm, CHUBasePop, 
     -                        HUBasePop, HU_CANESIM, CHU_CANESIM, Out, 
     -                        TT_EMERG, EMERGED, STGDOY,  
     -                        TBasePop, TBaseLeaf, TBaseEm, Climate)

!     2023-01-26 chp removed unused variables from argument list: ISWITCH

      USE MODULEDEFS
      USE CNG_MODULEDEFS

      IMPLICIT NONE
      EXTERNAL GET_CULTIVAR_COEFF, D_TT
      SAVE

c     DSSAT control variable
      TYPE(CONTROLTYPE) CONTROL

c     DSSAT simulation control variable:
!     TYPE (SwitchType) ISWITCH  ! MJ: no longer used

c     CANEGRO crop type
      TYPE (CANECROPTYPE) CANECROP

c     Climate / weather type
      TYPE (ClimateType) Climate

c     Output type
      TYPE (OutType)      Out

c     Cultivar read-related variables:
      REAL T_MXLFARNO
      LOGICAL CF_ERR

c     Thermal time requirements for phenology
c     Germination:
!chp      REAL TT_GERM
c     Emergence
      REAL TT_EMERG
      LOGICAL EMERGED

c     Heat units for today: base10, base16, cultivar-
c     defined heat units for leaf dynamics & emergence
c      & population
      REAL HU10, HU16, HUBaseLeaf, HUBaseEm, HUBasePop
c     For canesim canopy:
      REAL HU_CANESIM
c     Cumulative versions of above:
      REAL CHU10, CHU16, CHUBaseLeaf, CHUBaseEm, CHUBasePop
c     For canesim canopy:
      REAL CHU_CANESIM

      REAL TBasePop, TBaseLeaf, TBaseEm
c     For canesim canopy:
      REAL TbaseCS
      REAL AVGTEMP

c     Phenological growth stage dates
c     (i.e. dates on which certain stages occurred).
c     Please see in SC_CNG_MODS.for for a list of
c     growth stage numbers and names
c     STGDOY(I) = 'YYYYDDD'
      INTEGER STGDOY(20)
      INTEGER I


c     Variables to support ASA 2013-style coefficient reads
c     and non-linear thermal time accumulation
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c     D_TT function defined in SC_CNG_MODS
      REAL D_TT
c     Array to store daily change in thermal time values for each
c     of leaves, shoots, emergence, °Cd
      REAL DTT(3)
      CHARACTER*20 TP_PRFIX
c     Set this to one of the PARAMETER values below
      INTEGER TP_CHOICE 
c     Trait parameter prefixes for different processes
      CHARACTER*20 TP_PRFICES(3)
c     Local storage arrays for trait params
      REAL, DIMENSION(3) :: TBase, TOpt, TFin
c     Code constants
      INTEGER GermEm, LeafApp, Shootpop
      PARAMETER (GermEm = 1,
     &           LeafApp  = 2,
     &           Shootpop = 3)
      DATA TP_PRFICES/'GE_EM','LFEM','TLREM'/

      
c         
    

c     ===============================================================
c     DYNAMIC = RUNINIT
c     ===============================================================
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF (CONTROL%DYNAMIC.EQ.RUNINIT) THEN
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c     END of RUNINIT
c     -----------------------------------------------------


c     ===============================================================
c     DYNAMIC = SEASINIT
c     ===============================================================
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
      ELSEIF (CONTROL%DYNAMIC.EQ.SEASINIT) THEN
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::

c     Initialise thermal time measures:
c     ::::::::::::::::::::::::::::::::::::::::::::
          CHU10 = 0.
          HU10  = 0.
          CHU16 = 0.
          HU16  = 0.
          CHUBaseLeaf = 0.
          HUBaseLeaf  = 0.
          CHUBaseEm   = 0.
          HUBaseEm    = 0.
          CHUBasePop  = 0.
          HUBasePop   = 0.
          Out%CHUPI   = 0.
          Out%HUPI    = 0.
          HU_CANESIM  = 0.
          CHU_CANESIM = 0.

c     Is this a plant or ratoon crop?
c     :::::::::::::::::::::::::::::::
          IF (CaneCrop%RATOON .LT. 1) THEN
c              For plant cane:
               TT_EMERG = 428.
               CALL GET_CULTIVAR_COEFF(TT_EMERG, 'TTPLNTEM', CONTROL, 
     -                            CF_ERR)
          ELSE
c             For ratoon cane:
              TT_EMERG = 203.
              CALL GET_CULTIVAR_COEFF(TT_EMERG, 'TTRATNEM', CONTROL, 
     -                            CF_ERR)
          ENDIF
c     :::::::::::::::::::::::::::::::

          EMERGED = .FALSE. 
c         Set base temperatures:
c         * READ from CULTIVAR file! *
c         ::::::::::::::::::::::
c         Base temperatures

c         READ VALUES FROM CULTIVAR FILE:
c         ===============================
c         Leaf number at which max. leaf size is reached:
c         :::::::::::::::::::::::::::::::::::::::::::::::
c             Set default:
              CaneCrop%MXLFARNO   = 14

c             Read from file:
              CALL GET_CULTIVAR_COEFF(T_MXLFARNO, 'MXLFARNO',
     -                             CONTROL,  CF_ERR)
              CaneCrop%MXLFARNO = INT(T_MXLFARNO)
c         :::::::::::::::::::::::::::::::::::::::::::::::

c         Number of leaves at which phyllocron interval switches:
c         :::::::::::::::::::::::::::::::::::::::::::::::::::::::
c             Default value:
              CaneCrop%PHYLSWTCH  = 18.

c             Read from file:
              CALL GET_CULTIVAR_COEFF(CaneCrop%PHYLSWTCH, 'PSWITCH', 
     -                             CONTROL,  CF_ERR)              
c         :::::::::::::::::::::::::::::::::::::::::::::::::::::::


c         Phyllocron intervals:
c         :::::::::::::::::::::
c             Set defaults
              CaneCrop%PHYLO(1) =  69.
              CaneCrop%PHYLO(2) = 169.

c             Read from file
              CALL GET_CULTIVAR_COEFF(CaneCrop%PHYLO(1), 'PI1', 
     -                                CONTROL, CF_ERR)
              CALL GET_CULTIVAR_COEFF(CaneCrop%PHYLO(2), 'PI2', 
     -                                CONTROL, CF_ERR)
c         :::::::::::::::::::::

c         Base temperatures:
c         ::::::::::::::::::
c             Set defaults:
              TBasePop   = 16.
              TBaseLeaf  = 16.
              TBaseEm    = 10.
              TBaseCS      = 16.

c             Read from file
!              CALL GET_CULTIVAR_COEFF(TBasePop, 'TTBASEPOP', 
!     -                                CONTROL, CF_ERR)
              CALL GET_CULTIVAR_COEFF(TBaseLeaf, 'TTBASELFEX', 
     -                                CONTROL, CF_ERR)              
              CALL GET_CULTIVAR_COEFF(TBaseEm, 'TTBASEEM', 
     -                                CONTROL, CF_ERR)    
! Removed by MJ, Feb 2018
!              CALL GET_CULTIVAR_COEFF(TBaseCS, 'TBase', 
!     -                                CONTROL, CF_ERR)   
     
     
c         New coefficient calls for ASA work:
c         :::::::::::::::::::::::::::::::::::

c         GERMINATION AND EMERGENCE
c         :::::::::::::::::::::::::
          TP_CHOICE = GermEm
c         Look up prefix 
          TP_PRFIX = TP_PRFICES(TP_CHOICE)
c         Read coefficients into array
c         Tbase_***
          CALL GET_CULTIVAR_COEFF(TBase(TP_CHOICE), 'TBASE_'//TP_PRFIX,
     &                                CONTROL, CF_ERR)
c         TOpt_***
          CALL GET_CULTIVAR_COEFF(TOpt(TP_CHOICE), 'TOPT_'//TP_PRFIX, 
     &                                CONTROL, CF_ERR)
c         TFin_***
          CALL GET_CULTIVAR_COEFF(TFin(TP_CHOICE), 'TFin_'//TP_PRFIX, 
     &                                CONTROL, CF_ERR)


c         LEAF APPEARANCE
c         :::::::::::::::::::::::::
          TP_CHOICE = LeafApp
c         Look up prefix 
          TP_PRFIX = TP_PRFICES(TP_CHOICE)
c         Read coefficients into array
c         Tbase_***
          CALL GET_CULTIVAR_COEFF(TBase(TP_CHOICE), 'TBASE_'//TP_PRFIX,
     &                                CONTROL, CF_ERR)
c         TOpt_***
          CALL GET_CULTIVAR_COEFF(TOpt(TP_CHOICE), 'TOPT_'//TP_PRFIX, 
     &                                CONTROL, CF_ERR)
c         TFin_***
          CALL GET_CULTIVAR_COEFF(TFin(TP_CHOICE), 'TFin_'//TP_PRFIX, 
     &                                CONTROL, CF_ERR)


c         SHOOT POPULATION
c         :::::::::::::::::::::::::
          TP_CHOICE = Shootpop
c         Look up prefix 
          TP_PRFIX = TP_PRFICES(TP_CHOICE)
c         Read coefficients into array
c         Tbase_***
          CALL GET_CULTIVAR_COEFF(TBase(TP_CHOICE), 'TBASE_'//TP_PRFIX,
     &                                CONTROL, CF_ERR)
c         TOpt_***
          CALL GET_CULTIVAR_COEFF(TOpt(TP_CHOICE), 'TOPT_'//TP_PRFIX, 
     &                                CONTROL, CF_ERR)
c         TFin_***
          CALL GET_CULTIVAR_COEFF(TFin(TP_CHOICE), 'TFin_'//TP_PRFIX, 
     &                                CONTROL, CF_ERR)


               
              CONTINUE         

c         ::::::::::::::::::

c         Phenological phases:
c         ::::::::::::::::::::  
c         Init STGDOY array:
          DO I=1,20
              STGDOY(I) = 0
          ENDDO
    
c         Current growth phase:
c         (Emerging - see GROWTHPHASES in SC_CNG_MODS.for)
          CaneCrop%GROPHASE = 1
c         Growth STAGE (start of simulation, today):
          STGDOY(14) = Control%YRSIM

c         Plant/ratoon date (ought to be today)
          STGDOY(8)  = Control%YRDOY
          


c         END READ VALUES FROM CULTIVAR FILE
c         ==================================
c     END of SEASINIT
c     -----------------------------------------------------


c     ===============================================================
c     DYNAMIC = RATE
c     ===============================================================
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
      ELSEIF(CONTROL%DYNAMIC.EQ.RATE) THEN
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::

c         Calculate heat units:
c         :::::::::::::::::::::
c         Average temperature:
              AVGTEMP = (Climate%TEMPMX + Climate%TEMPMN) / 2.
c         Heat units:

c         Calculate daily thermal time accumulation (°Cd)
c         Germination
          DTT(GermEm) = D_TT(AVGTEMP, TBase(GermEm), TOpt(GermEm), 
     &      TFin(GermEm))
c         Leaf appearance
          DTT(LeafApp) = D_TT(AVGTEMP, TBase(LeafApp), TOpt(LeafApp), 
     &      TFin(LeafApp))
c         Shoot population
          DTT(ShootPop) = D_TT(AVGTEMP, TBase(ShootPop), TOpt(ShootPop),
     &      TFin(ShootPop))

c         Today's thermal time:
c             Calculate
c             heat units for emergence:
          !HUBaseEm   = MAX(0., AVGTEMP - TBaseEm)
          HUBaseEm =  DTT(GermEm)
          IF ((CHUBaseEm + HUbaseEm) .GE. TT_EMERG) THEN
              
c             Calculate heat units for population, canopy,
c             etc (not emergence), after the plant has emerged
              HU10 = MAX(0., AVGTEMP - 10.)
              HU16 = MAX(0., AVGTEMP - 16.)
              !HUBaseLeaf = MAX(0., AVGTEMP - TBaseLeaf)
              HUBaseLeaf = DTT(LeafApp)
              !HUBasePop  = MAX(0., AVGTEMP - TBasePop)   
              HUBasePop  = DTT(Shootpop)

c             For canesim canopy:
              HU_CANESIM = MAX(0., AVGTEMP - TBaseCS)
              
c             Set date of emergence growth stage:
              IF (.NOT.(EMERGED)) THEN
                  STGDOY(10)  = Control%YRDOY
                  EMERGED = .TRUE.
c                 Set growth phase for output 
c                 (tillering comes after emergence)
                  CaneCrop%GROPHASE = 2
              ENDIF

c             Changed by MJ, ASA 2013
              Out%HUPI = HUBaseEm
              ! Out%HUPI = HUBaseLeaf

          ENDIF



c         *** Should CHU16 be replaced with CHUBaseLeaf?? ***
          Climate%DTT    = CHU16

c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c     END of RATE
c     -----------------------------------------------------


c     ===============================================================
c     DYNAMIC = INTEGRATE
c     ===============================================================
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
      ELSEIF(CONTROL%DYNAMIC.EQ.INTEGR) THEN
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::

c     Accumulate heat units:
c     ::::::::::::::::::::::
          CHU10 = CHU10 + HU10
          CHU16 = CHU16 + HU16

          ! modified for ASA 2013:
          CHUBaseEm   = CHUBaseEm   + HUBaseEm
          CHUBasePop  = CHUBasePop  + HUBasePop
          CHUBaseLeaf = CHUBaseLeaf + HUBaseLeaf
c         For canesim canopy:
          CHU_CANESIM = CHU_CANESIM + HU_CANESIM

c         Base emergence:
          
          IF (EMERGED) THEN 
              Out%CHUPI   = Out%CHUPI + Out%HUPI
          ENDIF


c         Allocate to composite variables if necessary
          Out%CHUPOP = CHUBasePop
          Out%CHU_EM = CHUBaseEm
          

c     END of INTEGRATE
c     -----------------------------------------------------

c
c     ===============================================================
c     DYNAMIC = OUTPUT
c     ===============================================================
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
      ELSEIF(CONTROL%DYNAMIC.EQ.OUTPUT) THEN
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c     END of OUTPUT
c     -----------------------------------------------------


c     ===============================================================
c              DYNAMIC = FINAL 
c     ===============================================================
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
      ELSEIF(CONTROL%DYNAMIC.EQ.SEASEND) THEN
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c     END of FINAL
c     -----------------------------------------------------


c     -----------------------------------------------------
c     END of DYNAMIC conditional statement
c     ::::::::::::::::::::::::::::::::::::
      ENDIF
c     -----------------------------------------------------------
      END SUBROUTINE SC_PHENOLOGY
