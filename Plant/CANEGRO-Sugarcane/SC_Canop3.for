c*****PREDICT LEAF TIP EMERGENCE (LT), LEAF EXTENSION, AREA EXPANSION 
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

! 3/29/09 CHP Note: possible problem with array size for LEN.  Under some
!     odd scenarios for cultivar parameters (such as for parameter estimation), 
!     the 2nd array index can exceed 70.  Ignore for now, but need to 
!     investigate before CaneGro can be used with GLUE parameter estimation
!     program, or possibly with GenCalc.

C ********LAI AND LI (LIGHT INTERCEPTION) *************************
      SUBROUTINE CANOP3 (
c         [I] Cum. heat units (base?)
     -        CHUPI,
c         [I] Cum. heat units (base?); only used for poplt (*remove?)
     -        chupop,
c         [I] Not sure...
     -        IW,
c         [O] Cum. stress days?
     -        STDAYC,
c         [I] Senescence?
     -        SENESF,
c         [O] Not sure - needs expla-ing :-)
     -        expla,
c         [O] Plant (ASA 2013 - LEAF) extension rate (cm/day)
     -        LER,
c         [O] Light extinction coefficient (req'd by TRANS)
     -        EXTINC,
c         [O] Sunlit Leaf area index fraction
     -        F_SL,    
c         [IO] Current stalk population, not used here  *
     -        pplast,
c         [I] Newbat signals a new stalk pop. cohort
     -        NEWBAT,
c         [I] Stress days?
     -        STDAYE,
c         [I] related to water stress and pop.
     -        STDDIV,
c         [I] SOmething to do with tiller cohorts and water stress
     -        RESET,
c         [I] Rowspacing
     -        ROWSPC,
c         [O] Water stress effect on stalk population  *
     -        STRPOP,
c         [I] Maximum number of green leaves (variety-spec.)
     -        BEST, 
c         [I] Captures effect of lodging on LI
     -        LODGE_LI,
c         [IO] CANEGRO composite variables
     -        CLIMATE, CaneCrop, Soil, Growth, WaterBal, 
c         [I] DSSAT composite control variable
     -        CONTROL, 
c         [I] DSSAT simulation control variable
     -        ISWITCH)

c     Instruct compiler to use CANEGRO module defns:
c     ::::::::::::::::::::::::::::::::::::::::::::::
      USE CNG_ModuleDefs
      USE ModuleDefs

c     Do not allow randomly-created variables...
c     ::::::::::::::::::::::::::::::::::::::::::
      IMPLICIT NONE
      EXTERNAL GET_SPECIES_COEFF, GET_CULTIVAR_COEFF, WARNING, D_TT
      SAVE

c     The DSSAT simulation control variable:
c     ::::::::::::::::::::::::::::::::::::::
      TYPE (SwitchType) ISWITCH

c     The Climate 'object'
c     ::::::::::::::::::::
      TYPE (ClimateType) Climate

c     The water properties 'object'
c     :::::::::::::::::::::::::::::
      TYPE (WaterType)   WaterBal

c     The CANCRP properties object:
c     :::::::::::::::::::::::::::::
      TYPE (CaneCropType) CaneCrop

c     The SOILI object:
c     :::::::::::::::::
      TYPE (CNG_SoilType) Soil

c     The Growth 'object'
c     ::::::::::::::::::::
      TYPE (GrothType) Growth


c     DSSAT control variable:
c     ::::::::::::::::::::::: 
      TYPE (ControlType) CONTROL


c     Sizes of variables:
c     :::::::::::::::::::
      DIMENSION SHOOTX(MAX_TILLER_COHORTS), 
     & ltill(MAX_TILLER_COHORTS), 
     & ttref(MAX_TILLER_COHORTS), 
     & istage(MAX_TILLER_COHORTS),
     & stdayd(MAX_TILLER_COHORTS,70)

c     Previously non-declared parameters:
c     :::::::::::::::::::::::::::::::::::
      INTEGER IW
      INTEGER MDL
      REAL    BEST
c      REAL    BOOTLI
      REAL    CHUPI
      REAL    CHUPOP
      REAL    EXPLA
c     ASA 2013 (MJ)
      !REAL    PER
      REAL    PPLAST
      REAL    RESET
      REAL    ROWSPC
      REAL    SENESF
      REAL    STDAYC
      REAL    STRPOP      

c     Local variable definition:
c     ::::::::::::::::::::::::::
c     MJ, 2006/09/22
c     ::::::::::::::
c     Previously non-declared local variables:
c     ::::::::::::::::::::::::::::::::::::::::
     	INTEGER I
     	INTEGER ISTAGE
     	INTEGER J
     	INTEGER L, LLL
     	INTEGER LAST
     	INTEGER LCOUNT
     	INTEGER LFNX
     	INTEGER LTILL
     	INTEGER LTX
     	INTEGER M
     	INTEGER N1
     	INTEGER NATLI6
     	INTEGER NEWBAT
     	INTEGER NTLX
     	REAL    ALT
     	REAL    AREDUC
     	REAL    B
     	REAL    DEPTH
     	REAL    ESW120
     	REAL    EXPA
     	REAL    EXTINC
     	REAL    FR
     	REAL    GLAHA
     	REAL    GLASHT
     	REAL    GLFMAT
     	REAL    GLFMAX
     	REAL    GLFMX1
     	REAL    GLFMX2
     	REAL    GLFN1
     	REAL    POP
     	REAL    SHOOTX
     	REAL    STDAYD
     	REAL    STDAYE
     	REAL    STDDIV
      REAL    SHGT
     	REAL    T0
     	REAL    TAPER
     	REAL    TLAHA
     	REAL    TLASHT
     	REAL    TLFN
     	REAL    TMEAN
     	REAL    TTEMPOP
     	REAL    TTREF
     	REAL    XLI

c     Delete:
!      REAL TEMPWID

c     Previously implicitly-typed common block variables:
c     :::::::::::::::::::::::::::::::::::::::::::::::::::
c     CLIMT (weather variables?)
c     :::::::::::::::::::::::::
c     Variables used in this subroutine
c     CHUPI and DTT are one and the same...
      REAL    DTT
      REAL    TEMPMN
      REAL    TEMPMX

c     ::::::::::::::::::::::::
c     Growth variables:
c     :::::::::::::::::
c     Used here:
c     Leaf area index, green and sunlit
      REAL    LAI, LAI_SL
c     Sunlit LAI fraction
      REAL, INTENT(OUT) :: F_SL
      REAL    LI
      REAL    TLAI

c     :::::::::::::::::
c     Soil Variables:
c     :::::::::::::::
c     Used here:
      REAL    LL(NL)
      REAL    DLAYR(NL)
      INTEGER NLAYR
      REAL    SW(NL)

c     Water variables
c     :::::::::::::::
c     in use here:
      REAL PRECIP
      REAL SWDF2
c      REAL TRWU

c     Canopy (?) variables:
c     :::::::::::::::::::::
c     In use here:
      REAL    AREAMX(70)

c      REAL    DEDLFN(30)
      REAL    DEDLFN(MAX_TILLER_COHORTS)

      REAL    EXTCFN
      REAL    EXTCFST

c      REAL    LEN(30,70)
      REAL    LEN(MAX_TILLER_COHORTS,70)

c      INTEGER LFN(30)
      INTEGER LFN(MAX_TILLER_COHORTS)

      INTEGER LFNMXEXT
      REAL    LMAX(70)
      INTEGER LOSING
      INTEGER LT
      INTEGER NTLGRP
      REAL    PHYLO(2)
      REAL    PHYLSWTCH
c      REAL    TEMPOP(30)
      REAL    TEMPOP(MAX_TILLER_COHORTS)
      REAL    TMEANDEDLF
      REAL    TMEANLF
      REAL    TOTPOP
      REAL    WIDCOR
      REAL    WIDTH(70)
      REAL    WMAX(70)


c     Coefficient relating PER to change in stalk height
c     I presume the fraction of plant extension that is stalk extension:
      REAL PERCoeff

c     Coefficient relating canopy height to first leaf maximum size
      REAL CHTCoeff

c     New derived variables for Plant Extension Rate
c     REAL dPERdT, TBASEPER
c     For testing
!      REAL oldPER, oldoldPER

c     Cultivar params for leaf characteristics
      REAL LMAX_CF(3), AREAMX_CF(3), WMAX_CF(3), MXLFARNO
      REAL MAXLFLENGTH, MAXLFWIDTH


c     END of common block variable definitions
c     ::::::::::::::::::::::::::::::::::::::::
c     ::::::::::::::
c     LODGING, MJ 2006/09/06:
c         The maximum percentage of light interception lost due to lodging
          REAL LDG_LI_EFF
c         Severity of lodging (calculated in PHOTOS)
          REAL LODGE_LI

c         Green leaf number?
	    INTEGER GLFN
c	    REAL GLFN

c         Not sure
c         Is this to do with cohorts?  30 previously
          REAL    EXPAL(MAX_TILLER_COHORTS)

c         Local variable for water balance (to be removed)
          CHARACTER*1 ISWWAT
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

c     Error status of cultivar/species file reads
      LOGICAL CF_ERR, SPE_ERR
c     Temp values read from file
      REAL T_LFNMXEXT, T_MXLFAREA, AL
!      REAL T_MXLFARNO


c       Modification to calculation of delta thermal time, based on ASA 2013 work:
c       ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c       Delta thermal time (°Cd) for stalk elongation
        REAL DTT_SER
c       Delta thermal time (°Cd) for leaf elongation
        REAL DTT_LER
c       Function for calculating delta thermal time
        REAL D_TT
c       Base, optimal and final temperatures for thermal time accum. (°C)
c       These are cultivar params.
c       For leaves
        REAL TBaseLFEX, ToptLFEX, TFinLFEX
c       For stalks
        REAL TBaseSTKEX, ToptSTKEX, TFinSTKEX
c       Leaf and stalk elongation rate
c       cm/d
        REAL SER, LER
        
c       New variables for initial leaf and stalk elongation rates (SK, 08/10/15)
        REAL LER0, SER0

c      Warning text string:
       CHARACTER*78 WRNING_TEXT(2)
       INTEGER WARNED

!     Unit number for output
!      INTEGER SCLUN   !CHP

c     ~~~~~~  CODE  ~~~~~~~~~
c     ~~~~~~~~~~~~~~~~~~~~~~~




c     ---------------------------------------------------------------------
      IF (CONTROL%DYNAMIC .EQ. RUNINIT) THEN
c         MJ, March 2010: zero composite variables:
c         :::::::::::::::::::::::::::::::::::::::::
         CaneCrop%GROPHASE  = 0
         CaneCrop%LFMAX  = 0
         CaneCrop%LFN(MAX_TILLER_COHORTS)  = 0
         CaneCrop%LFNMXEXT  = 0
         CaneCrop%LOSING  = 0
         CaneCrop%LT  = 0
         CaneCrop%MXLFAREA  = 0
         CaneCrop%MXLFARNO  = 0
         CaneCrop%NTLGRP  = 0
         CaneCrop%RATOON  = 0
         CaneCrop%AREAMX  = 0.
         CaneCrop%CANHEIGHT  = 0.
         CaneCrop%DEDLFN  = 0.
         CaneCrop%DEDSTK  = 0.
         CaneCrop%EXTCFN  = 0.
         CaneCrop%EXTCFST  = 0.
         CaneCrop%GLFN  = 0.
         CaneCrop%LEN  = 0.
         CaneCrop%LMAX  = 0.
         CaneCrop%PHYLO  = 0.
         CaneCrop%PHYLSWTCH  = 0.
         CaneCrop%popcf  = 0.
         CaneCrop%poplfn  = 0.
         CaneCrop%popmax = 0.
         CaneCrop%POPN  = 0.
         CaneCrop%SHGT  = 0.
         CaneCrop%TEMPOP  = 0.
         CaneCrop%TMEANDEDLF  = 0.
         CaneCrop%TMEANLF  = 0.
         CaneCrop%TOTPOP  = 0.
         CaneCrop%WIDCOR  = 0.
         CaneCrop%WIDTH  = 0.
         CaneCrop%WMAX  = 0.  

c     MJ, March 2010: zero local variables
c     ::::::::::::::::::::::::::::::::::::
         GLFN  =  0
         I  =  0
         ISTAGE  =  0
         IW  =  0
         J  =  0
         L  =  0
         LAST  =  0
         LCOUNT  =  0
         LFN  =  0
         LFNMXEXT  =  0
         LFNX  =  0
         LOSING  =  0
         LT  =  0
         LTILL  =  0
         LTX  =  0
         M  =  0
         MDL  =  0
         N1  =  0
         NATLI6  =  0
         NEWBAT  =  0
         NLAYR  =  0
         NTLGRP  =  0
         NTLX  =  0
  
           ALT  =  0.0
          AREAMX  =  0.0
          AREDUC  =  0.0
          B  =  0.0
          BEST  =  0.0
          CHTCoeff  =  0.0
          CHUPI  =  0.0
          CHUPOP  =  0.0
          DEDLFN  =  0.0
          DEPTH  =  0.0
          DLAYR  =  0.0
c         dPERdT = 0.
c         TBASEPER  =  0.0
          DTT  =  0.0
          ESW120  =  0.0
          EXPA  =  0.0
          EXPAL  =  0.0
          EXPLA  =  0.0
          EXTCFN  =  0.0
          EXTCFST  =  0.0
          EXTINC  =  0.0
          FR  =  0.0
          GLAHA  =  0.0
          GLASHT  =  0.0
          GLFMAT  =  0.0
          GLFMAX  =  0.0
          GLFMX1  =  0.0
          GLFMX2  =  0.0
          GLFN1  =  0.0
          LAI  =  0.0
          LDG_LI_EFF  =  0.0
          LEN  =  0.0
          LI  =  0.0
          LL  =  0.0
          LMAX  =  0.0
          MXLFARNO  =  0.0
          LODGE_LI  =  0.0
          MAXLFLENGTH  =  0.0
          MAXLFWIDTH  =  0.0
c         ASA 2013 (MJ)
          !PER  =  0.0
          SER = 0.0
          LER = 0.0

          PERCoeff  =  0.0
          PHYLO  =  0.0
          PHYLSWTCH  =  0.0
          POP  =  0.0
          PPLAST  =  0.0
          PRECIP  =  0.0
          RESET  =  0.0
          ROWSPC  =  0.0
          SENESF  =  0.0
          SHGT  =  0.0
          SHOOTX  =  0.0
          ! STDAYC  =  0.0
          STDAYD  =  0.0
          STDAYE  =  0.0
          STDDIV  =  0.0
          STRPOP   =  0.0
          SW  =  0.0
          SWDF2  =  1.
          T_LFNMXEXT  =  0.0 
          T_MXLFAREA  =  0.0
          AL  =  0.0
          T0  =  0.0
          TAPER  =  0.0
          TEMPMN  =  0.0
          TEMPMX  =  0.0
          TEMPOP  =  0.0
          TLAHA  =  0.0
          TLAI  =  0.0
          TLASHT  =  0.0
          TLFN  =  0.0
          TMEAN  =  0.0
          TMEANDEDLF  =  0.0
          TMEANLF  =  0.0
          TOTPOP  =  0.0
          TTEMPOP  =  0.0
          TTREF  =  0.0
          WIDCOR  =  0.0
          WIDTH  =  0.0
          WMAX  =  0.0
          XLI  =  0.0
c         Added 08/10/15(SK)
          SER0 = 0.0
          LER0 = 0.0         
 
      ENDIF


c     Copy values from modules:
c     Climate:
      DTT    = CLIMATE%DTT
      TEMPMN = CLIMATE%TEMPMN
      TEMPMX = CLIMATE%TEMPMX

c      ISWWAT = ISWITCH%ISWWAT
      ISWWAT = 'N'

c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     Initialise (once!)
c     ::::::::::::::::::
      IF (CONTROL%DYNAMIC .EQ. SEASINIT) THEN

c       Reset warning check
        WARNED = 0

c         An option to control which Light Interception method to use;
c         This is a throwback to old CANEGRO days.  This should be set
c         to 1 always.
          MDL      = 1


c         Some of these need to be read from cultivar file
c         :::::::::::::
          IW       = 0.
c         From species file
          SENESF   = 5.
          RESET    = 5.
c          BOOTLI   = 1.


c         Species coeffs
          CALL GET_SPECIES_COEFF(SENESF, 'SENESF', Control, SPE_ERR)
          CALL GET_SPECIES_COEFF(RESET,  'RESET',  Control, SPE_ERR)
c          CALL GET_SPECIES_COEFF(BOOTLI, 'BOOTLI', Control, SPE_ERR)



c         READ VALUES FROM CULTIVAR FILE:
c         ===============================
c         BEST: the maximum number of green leaves on a well-watered stalk
c               that is old enough to senesce leaves.
c         :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c             Default value:
              BEST     = 12.

c             Read from file
              CALL GET_CULTIVAR_COEFF(BEST, 'LFMAX', CONTROL,  
     -                            CF_ERR)

c         :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


c         Extinction coefficients:
c         ::::::::::::::::::::::::
c         Set defaults:
              CaneCrop%EXTCFN     = 0.840
              CaneCrop%EXTCFST    = 0.580

c         Read from file:
              CALL GET_CULTIVAR_COEFF(CaneCrop%EXTCFN, 'EXTCFN', 
     -                               CONTROL,  CF_ERR)
              CALL GET_CULTIVAR_COEFF(CaneCrop%EXTCFST, 'EXTCFST', 
     -                               CONTROL,  CF_ERR)
c         ::::::::::::::::::::::::

c         LFNMXEXT - number of leaves at which maximum
c         light extinction occurs...
c         ::::::::::::::::::::::::::::::::::::::::::::
c         Default value:
              CaneCrop%LFNMXEXT   = 20

c         Read from file:
c             Note: this value is read as a REAL and then converted to integer:
              CALL GET_CULTIVAR_COEFF(T_LFNMXEXT, 'LFNMXEXT', 
     -                               CONTROL,  CF_ERR)
              CaneCrop%LFNMXEXT = INT(T_LFNMXEXT)
c         ::::::::::::::::::::::::::::::::::::::::::::

c         Maximum leaf area, per leaf, cm2:
c         :::::::::::::::::::::::::::::::::
c             Set default:
              CaneCrop%MXLFAREA   = 360.

c             Read from file:
              CALL GET_CULTIVAR_COEFF(T_MXLFAREA, 'MXLFAREA',
     -                             CONTROL,  CF_ERR)
              CaneCrop%MXLFAREA = T_MXLFAREA
c         :::::::::::::::::::::::::::::::::

c         This is a variety-specific value
c         It seems to affect the width of the leaf
c         ::::::::::::::::::::::::::::::::::::::::
c             Set default
              CaneCrop%WIDCOR     = 1.

c             Read from file:
              CALL GET_CULTIVAR_COEFF(CaneCrop%WIDCOR, 'WIDCOR',
     -                             CONTROL,  CF_ERR)
c         ::::::::::::::::::::::::::::::::::::::::

c         MJ 2007-05-03: Some extra cultivar coefficients that
c         were previously hard-coded

c         Maximum leaf area:
             AREAMX_CF(1) =   0.
             AREAMX_CF(2) =  27.2
             AREAMX_CF(3) = -20.8

c             Read from file:
!  MJ, April 2018: this was always 0, so I removed it from ECO file
!              CALL GET_CULTIVAR_COEFF(AREAMX_CF(1), 'AREAMX_CF(1)',
!     -                             CONTROL,  CF_ERR)
c             Read from file:
              CALL GET_CULTIVAR_COEFF(AREAMX_CF(2), 'AREAMX_CF(2)',
     -                             CONTROL,  CF_ERR)
c             Read from file:
              CALL GET_CULTIVAR_COEFF(AREAMX_CF(3), 'AREAMX_CF(3)',
     -                             CONTROL,  CF_ERR)

c            Max leaf area reaches a certain plateau after a certain
c            number of leaves (14.)
              MXLFARNO = CaneCrop%MXLFARNO

              IF (MXLFARNO .LT. 1) THEN
c                 Read from file:
                  CALL GET_CULTIVAR_COEFF(MXLFARNO, 'MXLFARNO',
     -                             CONTROL,  CF_ERR)
              ENDIF


c         Maximum leaf length (I think)
             LMAX_CF(1) = -0.376
             LMAX_CF(2) = 12.2
             LMAX_CF(3) = 21.8

c             Read from file:
              CALL GET_CULTIVAR_COEFF(LMAX_CF(1) , 'LMAX_CF(1)',
     -                             CONTROL,  CF_ERR)
c             Read from file:
              CALL GET_CULTIVAR_COEFF(LMAX_CF(2), 'LMAX_CF(2)',
     -                             CONTROL,  CF_ERR)
c             Read from file:
              CALL GET_CULTIVAR_COEFF(LMAX_CF(3), 'LMAX_CF(3)',
     -                             CONTROL,  CF_ERR)


c         Maximum leaf width (I think)
             WMAX_CF(1) = -0.0345
             WMAX_CF(2) =  2.243
             WMAX_CF(3) =  7.75

c             Read from file:
              CALL GET_CULTIVAR_COEFF(WMAX_CF(1) , 'WMAX_CF(1)',
     -                             CONTROL,  CF_ERR)
c             Read from file:
              CALL GET_CULTIVAR_COEFF(WMAX_CF(2), 'WMAX_CF(2)',
     -                             CONTROL,  CF_ERR)
c             Read from file:
              CALL GET_CULTIVAR_COEFF(WMAX_CF(3), 'WMAX_CF(3)',
     -                             CONTROL,  CF_ERR)


c         Maximum leaf length after MXLFARNO leaves
             MAXLFLENGTH = 100.
c             Read from file:
              CALL GET_CULTIVAR_COEFF(MAXLFLENGTH, 'MAXLFLENGTH',
     -                             CONTROL,  CF_ERR)


c         Maximum leaf width:
             MAXLFWIDTH = 3.5
c             Read from file:
              CALL GET_CULTIVAR_COEFF(MAXLFWIDTH, 'MAXLFWIDTH',
     -                             CONTROL,  CF_ERR)

        
c         Cultivar parameters for leaf and stalk elongation rates (SK, 08/10/15)
c         ::::::::::::::::::::::::::::::::::::::::::::

c         Read from cult file:
              CALL GET_CULTIVAR_COEFF(SER0, 'SER0',
     -                                CONTROL, CF_ERR)

              CALL GET_CULTIVAR_COEFF(LER0, 'LER0',
     -                                CONTROL, CF_ERR)
     

      ! WRITE(*, '(A, F15.10)') 'SER0 = ', SER0
      ! WRITE(*, '(A, F15.10)') 'LER0 = ', LER0
      
c         Cultivar parameters for plant extension rate
c         ::::::::::::::::::::::::::::::::::::::::::::
c             dPERdT   =   .176
c             TBASEPER = 10.057

c         Read from cult file:
c             CALL GET_CULTIVAR_COEFF(dPERdT, 'dPERdT',
c    -                                CONTROL, CF_ERR)
c
c             CALL GET_CULTIVAR_COEFF(TBASEPER, 'TBASEPER',
c    -                                CONTROL, CF_ERR)
          

c         Coeffs for plant extension rate for
c         canopy height
          PERCoeff = 0.16
          CHTCoeff = 0.864
          CALL GET_SPECIES_COEFF(PERCoeff, 'PERCoeff', Control, 
     &                            SPE_ERR)
!         CALL GET_CULTIVAR_COEFF(CHTCoeff, 'CHTCoeff', Control, 
          CALL GET_SPECIES_COEFF (CHTCoeff, 'CHTCoeff', Control, 
     &                            SPE_ERR)



c         Init lodging variable, MJ 2006/09/06:
          LDG_LI_EFF = .01
          CALL GET_CULTIVAR_COEFF(LDG_LI_EFF, 'LDG_FI_REDUC',  
     &                            Control, SPE_ERR)



c         Thermal time parameters for stalk elongation:
c         :::::::::::::::::::::::::::::::::::::::::::::
c         Defaults:
            TBaseSTKEX = 16.0
            TOptSTKEX  = 35.0 
            TFinSTKEX  = 48.0
          CALL GET_CULTIVAR_COEFF(TBaseSTKEX, 'TBASE_STKEX',  
     &       Control, CF_ERR)
          CALL GET_CULTIVAR_COEFF(TOptSTKEX, 'TOPT_STKEX',  
     &       Control, CF_ERR)
          CALL GET_CULTIVAR_COEFF(TFinSTKEX, 'TFin_STKEX',  
     &       Control, CF_ERR)             


c         Thermal time parameters for leaf elongation:
c         :::::::::::::::::::::::::::::::::::::::::::::
c         Defaults:
            TBaseLFEX = 10.0
            TOptLFEX  = 30.0
            TFinLFEX  = 43.0
          CALL GET_CULTIVAR_COEFF(TBaseLFEX, 'TBASE_LFEX',  
     &       Control, CF_ERR)
          CALL GET_CULTIVAR_COEFF(TOptLFEX, 'TOPT_LFEX',  
     &       Control, CF_ERR)
          CALL GET_CULTIVAR_COEFF(TFinLFEX, 'TFin_LFEX', 
     &       Control, CF_ERR)           


c         END READ VALUES FROM CULTIVAR FILE
c         ==================================


c         Init some leaf characteristics:
c         :::::::::::::::::::::::::::::::
          DO I=1,70
              CaneCrop%AREAMX(I) = 0.
              CaneCrop%LMAX(I)   = 0.
              CaneCrop%WMAX(I)   = 0.
              CaneCrop%WIDTH(I)  = 0.
              DO J=1,MAX_TILLER_COHORTS
                  CaneCrop%LEN(J,I) = 0.
              ENDDO
          ENDDO


c         MJ, 2007-05-03: The quadratic form of the 
c         equations is the model; the coefficients are, alas, cultivar-
c         specific and so should be put in the cultivar file.
c         :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          DO L=1,70
             AL=REAL(L)
c            Taken from CANINT.for
C            This was replaced 15/6/92 by GIB AREAMX(L)=-54.6+36.88*AL-0.945*AL**2

c         MJ 2007-05-03:
c         Calculating leaf area and shape:
c         :::::::::::::::::::::::::::::::::::::
c         Maximum leaf area:
c         ::::::::::::::::::
c            In the quadratic equation of form ax^2 + bx + c, AREAMX_CF = {a,b,c}
c             AREAMX_CF(1) =   0.
c             AREAMX_CF(2) =  27.2
c             AREAMX_CF(3) = -20.8

             CaneCrop%AREAMX(L) = AREAMX_CF(3) + (AREAMX_CF(2) * AL) 
     &                            + (AREAMX_CF(1)*AL**2.)
c            Max leaf area reaches a certain plateau after a certain
c            number of leaves (15.)
c             MXLFARNO = 15.
             IF(AL .GT. MXLFARNO) THEN
                  CaneCrop%AREAMX(L) = AREAMX_CF(3) 
     &                              + (AREAMX_CF(2) * MXLFARNO)
     &                              + (AREAMX_CF(1)*MXLFARNO**2.)
             ENDIF

c            MJ: Assign max leaf area value to all leaves above leaf number
c            MXLFARNO
c             DO L=1,70
	          IF (L.GE.CaneCrop%MXLFARNO) 
     -                 CaneCrop%AREAMX(L) = CaneCrop%MXLFAREA
c             ENDDO

c         ::::::::::::::::::
c         Maximum leaf length (I think)
c         :::::::::::::::::::::::::::::
c            In the quadratic equation of form ax^2 + bx + c, LMAX_CF = {a,b,c}
c             LMAX_CF(1) = -0.376
c             LMAX_CF(2) = 12.2
c             LMAX_CF(3) = 21.8

             CaneCrop%LMAX(L)   = LMAX_CF(3)  + (LMAX_CF(2) * AL) 
     &                            + (LMAX_CF(1) * AL**2.)

c         Leaf length cannot exceed a certain value
c         after a certain number of leaves.  I suspect
c         that 
c         ::::::::::::::::::::::::::::::::::::::::::
c          MAXLFLENGTH = 100.
             IF(AL .GT. MXLFARNO) THEN
                CaneCrop%LMAX(L) = AMAX1 (CaneCrop%LMAX(L),MAXLFLENGTH) 
             ENDIF

c         :::::::::::::::::::::::::::::
c         Maximum leaf width (I think)
c         :::::::::::::::::::::::::::::
c            In the quadratic equation of form ax^2 + bx + c, WMAX_CF = {a,b,c}
c             WMAX_CF(1) = -0.0345
c             WMAX_CF(2) =  2.243
c             WMAX_CF(3) =  7.75

             CaneCrop%WMAX(L)   = (WMAX_CF(3) + (WMAX_CF(2) * AL) 
     &                          + (WMAX_CF(1) * AL**2)) / 10.0

c            Limit leaf width after a certain number of leaves
c            Maximum leaf width:
c             MAXLFWIDTH = 3.5
             IF(AL .GT. MXLFARNO) THEN
                CaneCrop%WMAX(L)   = AMAX1 (CaneCrop%WMAX(L),MAXLFWIDTH) 
             ENDIF      
          ENDDO


c         END of leaf characteristics init
c         ::::::::::::::::::::::::::::::::




c         Init Light interception effect of lodging
          LODGE_LI    = 0.

c         Init leaf area index
          Growth%LAI  = 0.
          Growth%TLAI = 0.

c         Init Light Interception
          Growth%LI  = 0.


c         Water balance introduced continuity errors...
c         This attempts to remove this effect:
c     ::::::::::::::::::::::::::::::::::::::::
          WIDTH = 0.
          LMAX = 0.
          LEN = 0.
          CHUPI = 0.

          L      = 0 
          LAST   = 0 
          LCOUNT = 0 
          LFNX   = 0
          LTILL  = 0
          LTX    = 0
          M      = 0 
          N1      = 0
          NATLI6 = 0
          NEWBAT = 0
          NTLX   = 0

          ALT    = 0.
          AREDUC = 0. 
          B      = 0. 
          DEPTH  = 0. 
          ESW120 = 0. 
          EXPA   = 0. 
          EXTINC = 0. 
          FR     = 0. 
          GLAHA  = 0. 
          GLASHT = 0. 
          GLFMAT = 0. 
          GLFMAX = 0. 
          GLFMX1 = 0. 
          GLFMX2 = 0. 
          GLFN1  = 0. 
          GLFN   = 0.
          POP    = 0. 

          STDAYE = 0. 
          STDDIV = 0. 
          SHGT   = 0. 
          T0     = 0. 
          TAPER  = 0. 
          TLAHA  = 0. 
          TLASHT = 0. 
          TLFN   = 0. 
          TMEAN  = 0. 
          TTEMPOP= 0. 
          TTREF  = 0.
          XLI    = 0.
 
          SHOOTX = 0. 
          STDAYD = 0. 
          TTREF  = 0.
          EXPAL  = 0. 


           
c             Set number of leaf tips to 1 (this sub should only be
c             called after TT-determined emergence)
              IF(LT.LE.0) LT = 1
c             Yesterday's thermal time
              T0 = 0.
c             Stress days?
              STDAYE = 0.
              STDDIV = 0.
              NATLI6 = 99
              DO I=1,MAX_TILLER_COHORTS
c                 Phenological stage?
                  ISTAGE(I) = 1
c                 ?
                  LTILL(I)  = 0
c                 Reference thermal time for each tiller cohort
                  TTREF(I)  = 0.
c                     Stress days for each tiller cohort
                      DO J=1,70
                          STDAYD(I,J) = 0.
                      ENDDO
              ENDDO
          

c         Canopy heights, etc
              SHGT = 0.
              SER = 0.0
              LER = 0.0

              ! PER = 0.
              CaneCrop%CANHEIGHT = 0.

c         Init tempop:
          TEMPOP = 0.

c     End of initialisation section
c    ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::




c     ---------------------------------------------------------------------
c     :::::::::::::::::::::::::::::::::::::::::::::
      ELSEIF (CONTROL%DYNAMIC .EQ. RATE) THEN
c     :::::::::::::::::::::::::::::::::::::::::::::
c         'pplast' is present population, 
c         'newbat'=1 signals a new cohort
c          CALL POPLT3 (chupop,t0,pplast,newbat,rowspc,stdaye,
c     1               stddiv, WATERBAL, CANECROP, SOIL)

c     MJ, 28/09/2006:
c     :::::::::::::::
c     POPLT3 used to be called here.  It is now called before this subroutine.
c     Variables used within Canop3 that are outputs from POPLT3 are:
c       *  newbat
c       *  stdaye
c       *  stddiv
c     These need to be passed in as parameters then.
c     ::::::::::::::::::::::::::::::::::::::::::::::


c     CaneCrop:
c     :::::::::
          AREAMX     = CaneCrop%AREAMX
          DEDLFN     = CaneCrop%DEDLFN
          EXTCFN     = CaneCrop%EXTCFN
          EXTCFST    = CaneCrop%EXTCFST
          LEN        = CaneCrop%LEN
          LFN        = CaneCrop%LFN
          LFNMXEXT   = CaneCrop%LFNMXEXT
          LMAX       = CaneCrop%LMAX
          LOSING     = CaneCrop%LOSING
          LT         = CaneCrop%LT
          NTLGRP     = CaneCrop%NTLGRP
          PHYLO      = CaneCrop%PHYLO
          PHYLSWTCH  = CaneCrop%PHYLSWTCH
          TEMPOP     = CaneCrop%TEMPOP
          TMEANDEDLF = CaneCrop%TMEANDEDLF
          TMEANLF    = CaneCrop%TMEANLF
          TOTPOP     = CaneCrop%TOTPOP
          WIDCOR     = CaneCrop%WIDCOR
          WIDTH      = CaneCrop%WIDTH
          WMAX       = CaneCrop%WMAX
c     ::::::::::

c     Soil
c     ::::
          LL    = Soil%LL
          DLAYR = Soil%DLAYR  
          NLAYR = Soil%NLAYR
          SW    = Soil%SW
c     ::::

c     Growth object:
c     ::::::::::::::
          LAI = Growth%LAI
          LI  = Growth%LI
          TLAI = Growth%TLAI
c     ::::::::::::::

c     Water:
c     ::::::
          PRECIP = WaterBal%PRECIP
          SWDF2  = WaterBal%SWDF2
c          TRWU   = WaterBal%TRWU
c          WRITE(SCLUN, '(A, 30(F10.0))') 'TEMPOP is ',TEMPOP
c     ::::::

        if(newbat.eq.1) ttref(ntlgrp)=chupi

c      WRITE(SCLUN, '(I5, I5, F10.0)') Newbat, ntlgrp, chupi

c     Use output from Stalk Population routine to calculate water stress effects:
c     (MJ, October 2006)
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF (ISWITCH%ISWWAT .EQ. 'Y') THEN
c         If water stress is included:
          strpop=stdaye/stddiv
      ELSE
c         If water stress excluded:
          strpop = 0.
      ENDIF
c     Note (MJ, October 2006):
c     ::::::::::::::::::::::::
c     None of these variables is used anyway.  Grumble.
c     :::::::::::::::::::::::::::::::::::::::::::::::::

      TMEAN=(TEMPMN+TEMPMX)/2.0

c *********
C The max. no green leaves on primary stalks is reduced according to 
C Stress days. Green leaf no. in later cohorts is simply leaf tips minus
c dead leavs on primary stalks
       
c CNB Jun2002: COMMENT OUT THE NEXT LINE AND READ MAX GREEN LF FROM VARIETY FILE	 
C	 BEST=12.0


c       IF(DTT.LT.2000.0) BEST=11.0
c       IF(DTT.LT.1000.0) BEST=12.0
       GLFMX1=max(BEST-(SENESF*STDAYC/100.0),0.0)
       GLFMX2=99.9
c 'GLFMX2' is used after 9 leaf stage and only if soil is deeper than 120cm
       if(lt.le.9) go to 6
        depth = 0.0
        esw120 = 0.0
        i=0
  5     i=i+1
         depth = depth + dlayr(i)
         if(i.gt.nlayr) go to 6
         if(depth.lt.120.0) then
          IF (ISWWAT .EQ. 'Y') THEN
c             If water balance is included:
              esw120=esw120+(sw(i)-ll(i))*dlayr(i)*10.0
          ELSE
c             MJ: added this fudged correction for no water balance
              esw120=esw120+(Soil%DUL(i)-LL(i))*dlayr(i)*10.0
          ENDIF
            go to 5
         endif

         IF (ISWWAT .EQ. 'Y') THEN
c        If water balance is included:
             esw120=esw120+(sw(i)-ll(i))*(120.0-(depth-dlayr(i)))*10.0
         ELSE
c            MJ: added this fudged correction for no water balance
             esw120=esw120+(Soil%DUL(i)-ll(i))*(120.0-(depth-dlayr(i)))
     -              *10.0
         ENDIF
         


cCNB Jan 2003 commented out this line: GLFMX2=(6.1+0.065*esw120)  
c GLFMX2=(3.95+0.106*esw45)-(lt-lfn(1))-(an-1)*0.3   
  6    glfmax=min(glfmx2,glfmx1)                    

     

c swdf2a is based on IB 1994 Field Crops Res. but makes no difference ie
c SWDF2 is doing a good enough job
c The quadratic funtion of esw30 applies only  when 4.0<esw30<32.0
c  6     esw30=min(esw30,31.0)
c        swdf2a = -0.27 + 0.06921*esw30 - 0.000904*esw30**2
c        swdf2a = max(swdf2a,0.0)


c     Matthew Jones, 2007-05-08
c     :::::::::::::::::::::::::
c     Changed the following line:
cC                  PER MM PER HOUR *24/10 FOR CM/DAY 
c                   oldPER= MAX(0., SWDF2*(-1.77+0.176*TMEAN)*24.0/10.0)
c     To:
c       1. Derived from this: base T = 10.57 degree days; dPER/dTEMP = 0.176
c          for NCo376
c       2. Prevent negative PER with MAX() function

c       Modification to calculation of delta thermal time, based on ASA 2013 work:
        DTT_SER = D_TT(TMEAN, TBaseSTKEX, ToptSTKEX, TFinSTKEX)
        DTT_LER = D_TT(TMEAN, TBaseLFEX, ToptLFEX, TFinLFEX)
        !PER = SWDF2 * dPERdT * MAX(0., TMEAN-TBASEPER) * 24./10.
        !LER = SWDF2 * dPERdT * DTT_LER * 24./10.
        !SER = SWDF2 * dPERdT * DTT_SER * 24./10.
        
        ! MJ, Jan 2018:
        ! Do we want to include water logging stress here, over and
        ! above the effects already included in SWDF2 via limited
        ! water uptake as calculated in ROOTWU?
        LER = SWDF2 * LER0 * DTT_LER * 24./10.
        SER = SWDF2 * SER0 * DTT_SER * 24./10.
        
c     For verification:
c        oldPER= MAX(0., SWDF2*(-1.77+0.176*TMEAN)*24.0/10.0)
c        oldoldPER= SWDF2*(-1.77+0.176*TMEAN)*24.0/10.0
c        WRITE(*, '(3(F10.5, 3H | ))') PER, oldPER, oldoldPER

c     ::::::::::::::::::::::::



C ONLY 4 LEAVES ARE ALLOWED TO ACCUMULATE IN THE WHORL. IF A NEW LEAF IS 
C INITIATED SO IS A NEW TILLER GROUP AND 'POPLTN' IS CALLED TO KEEP
C TRACK OF THE POPULATION STRUCTURE            

C----LOOP FOR UPTO 30 CATEGORIES OF SHOOTS STARTING AT PI HU INTERVALS--- 
C GLAHA, TLAHA are green and total leaf areas per ha
         GLAHA=0.0
         TLAHA=0.0
         do N1=1,MAX_TILLER_COHORTS
           EXPAL(N1)=0.0
         end do
         expla=0.0

c********************Start of cohort loop****************************
         DO 40 N1=1,NTLGRP
         ntlx=ntlgrp
         m=istage(N1)
c      WRITE(23, '(A, A, I5)') 'M is ', ACHAR(9), m
         if (phylo(m) > 1.e-6) then
           ltill(N1)=1+(chupi-ttref(N1))/phylo(m) + (m-1)*PHYLSWTCH
         else
           ltill(N1)=1
         endif
         if(ltill(N1).eq.PHYLSWTCH.and.istage(N1).eq.1) then
             istage(N1)=2 
             ttref(N1)=chupi
         endif
c no more than 4 leaves can expand at one time.
         if(ltill(N1)-lfn(N1).gt.4) ltill(N1)=lfn(N1)+4

c Leaf emergence is prevented in younger tillers when Li>.7
         if(N1.gt.natli6) go to 40
         ltx=ltill(N1)

c      WRITE(23, '(I10)') LTX

      

C --------CALCULATE THE AREA OF EXPANDING LEAVES 'EXPAL'OF ONE tiller cohort
        DO 20 LLL=1,LTx
c           MJ, Nov 2015: under certain conditions, more than 70 leaves are produced.  This causes
c           an array overflow, which bombs the simulation and can be a pain with long sequences.
c           This check prevents this.
            L = LLL
            IF (L .GT. 70) THEN
              L = 70
              IF (WARNED .EQ. 0) THEN
              WRNING_TEXT(1) = "More than 70 leaves simulated - " //
     &         "usually a sign that harvest age is too high."
              WRNING_TEXT(2) ="Please check dates and " // 
     &        " simulation outputs CAREFULLY."
              CALL WARNING(2, "SCCAN045: SCCNGRO", WRNING_TEXT)
              WARNED = 1
              ENDIF
            ENDIF

            LCOUNT=L
            IF(LEN(N1,L).EQ.-0.099) GOTO 20      


!            LEN(N1,L)=LEN(N1,L)+PER
c           ASA 2013 (MJ, Sept 2013)
            LEN(N1,L)=LEN(N1,L)+LER


c Final area of expanding leaves is reduced by as much as 50 % if water 
c stress is severe.
c DSSAT must be changed here

c           ::::::::::
            STDAYd(N1,L) =STDAYd(N1,L)  + (1.0-SWDF2)    
            areduc=max(1.0-STDAYD(N1,L)/30.0, 0.5) 

c      WRITE(SCLUN, '(F10.5)') Areduc

C-------CALCULATE THE AREA OF ONE EXPANDING LEAF  'EXPA'---------------------
             WIDTH(L)=AMIN1(WMAX(L)*LEN(N1,L)/(LMAX(L)*0.5) , WMAX(L))
             WIDTH(L)=WIDTH(L)*WIDCOR
c             MJ, Mar 2008: basically use the half base x height triangle area formula
c             for calculating area of the leaf
             TAPER=AMIN1(LEN(N1,L),0.5*LMAX(L))
             EXPA=TAPER*WIDTH(L)*0.5
             IF (LEN(N1,L).GT.0.5*LMAX(L)) 
     -            EXPA=EXPA+(LEN(N1,L)-0.5*LMAX(L))*WIDTH(L)
             IF (EXPA.GE.areamx(L)*areduc) THEN
                 
                     LFN(N1)=MIN(70, LFN(N1)+1)
                     LEN(N1,L)=-0.099
                     EXPA=0.0 
                     WIDTH(L)=0.0
                ENDIF
             EXPAL(N1)=EXPAL(N1)+EXPA
  20         CONTINUE   
C  GLASHT, TLASHT are green and total leaf areas per shoot.
        GLASHT=0.0
        TLASHT=0.0
c Calculate green leaf area per cohort (N1)
        IF (LFN(N1).GE.1) THEN                     
c 'glfmat' is number of fully expanded green leaves 
        glfmat=max(glfmax-(ltill(N1)-lfn(N1)),0.0)
       
c Here the oldest green expanding leaf is 'matured' then senesced if necessary. 
       if(glfmax.lt.ltill(N1)-lfn(N1)) then
            lfnx=MIN(70, lfn(N1)+1)
            len(N1,lfnx)=-0.099
            lfn(N1)=MIN(70, lfn(N1)+1)
       endif

c This assumes glfmax applies less as li tends to 1.0 at which time dead leaf 
c no is the same as for cohort #1
c change next for dssat3 GIB 6/9/95
       if(N1.eq.1) DEDLFN(1)=max(DEDLFN(1),LFN(1)-GLFMAt)
       if(N1.gt.1) 
     -   DEDLFN(N1)=max(dedlfn(N1),dedlfn(1)-(lfn(1)-lfn(N1))*(1.0-li),
     &            0.0)

c All stalks in this cohort die if they have fewer than 3 green leaves, but
c first 5 cohorts are not affected
       if(ltill(N1)-2.le.dedlfn(N1).and.dedlfn(N1).gt.0.0.and.N1.gt.5) 
     &            then
            tempop(N1)=0.0
            ntlx=ntlgrp-1
            losing=1
       endif

       alt=lt                                       
       fr=max((6.0-alt)/5.0,0.0)    

c      WRITE(23, '(A, A, F20.8)') 'GLASHT is ', ACHAR(9), GLASHT     
c      What determines if this is called? - DO 40 ...             

       GLFN=LFN(N1)-DEDLFN(N1)
c       WRITE(23, '(A, A, F5.1)') 'DEDLFN(N1) is ', ACHAR(9), DEDLFN(N1)

       IF (GLFN.LE.0.0) GO TO 34
           LAST=AMAX0(1,LFN(N1)-GLFN+1) 
           DO 30 L=LAST,LFN(N1)
            areduc=max(1.0-STDAYD(N1,L)/30.0, 0.5) 
  30       GLASHT=GLASHT+AREAMX(L)*areduc 
c          Here, glasht should be the same
c             but is not...
c          AreaMX IS the same though
c           WRITE(23, '(A, A, 70(F10.3))') 'AreaMX is ', ACHAR(9), AREAMX
c           WRITE(23, '(A, A, F20.8)') 'GLASHT is ', ACHAR(9), GLASHT   
c           WRITE(23, '(A, A, F20.8)') 'AREDUC is ', ACHAR(9), AREDUC  
c          WRITE(23, '(A, A, I5)') 'LAST is ', ACHAR(9), LAST   
c          :::::::::::::::::::::::::::::::
           DO 32 L=1,LFN(N1)
            areduc=max(1.0-STDAYD(N1,L)/30.0, 0.5) 
  32       TLASHT=TLASHT+AREAMX(L)*areduc  
        ENDIF
c          WRITE(23, '(A)') '---------'

  34    GLASHT=GLASHT+EXPAL(N1)      
        TLASHT=TLASHT+EXPAL(N1)  
c      WRITE(23, '(A, A, F20.8)') 'EXPAL(N1) is ', ACHAR(9), EXPAL(N1)  
c      MJ note: EXPAL N1 is different.
        GLAHA=GLAHA+GLASHT*TEMPOP(N1)
        TLAHA=TLAHA+TLASHT*TEMPOP(N1)

c  	  if (tempop(N1).gt.11000) then
c	   write(*,*)'TEMPOP LARGE'
c	endif
	     
	!inserted by cnb
c	ppp=0.
c	if (chupi.gt.282.and.chupi.le.283) ppp=1.
c      if (chupi.gt.483.and.chupi.le.484) ppp=1.
c	if (chupi.gt.1503.and.chupi.le.1504) ppp=1.
c	if (chupi.gt.2132.and.chupi.le.2133) ppp=1.
c	if (chupi.gt.3546.and.chupi.le.3547) ppp=1.
cc	if (ppp.gt.0.) then
c	 do 3335 nn=1, int(tempop(N1)/1000)
c	bab1=GLASHT*TEMPOP(N1) *0.0001*0.0001
c	bab2=TLASHT*TEMPOP(N1) *0.0001*0.0001
c3335	write(14,3336) chupi, bab1, bab2
c      endif
c3336  format(5f8.2)
	!cnb end



c  Expanding LA per ha
        expla=expla+EXPAL(N1)*TEMPOP(N1)
  40    SHOOTX(N1)=GLASHT
        ntlgrp=ntlx
        lt=ltill(1)

c      WRITE(23, '(A, A, F20.8)') 'GLASHT is ', ACHAR(9), GLASHT

	
C CNB/MARCH2000 CALCULATE THE MEAN LEAF NUMBER IN THE CROP
C THIS IS DONE BY TAKING THE POPULATION WEIGHED MEAN
	  TMEANLF=0.0
	  DO 440 N1=1,NTLGRP
!          This was LFN(N1) but was changed to include all leaves
	     TLFN=LTILL(N1) 
	     TTEMPOP=TEMPOP(N1)
	     TMEANLF=TLFN*TTEMPOP/(TOTPOP+1)+TMEANLF
440	  CONTINUE
C CNB/MARCH2000 END


C CNB JUL2000 {NEWPART} CALCULATE MEAN DEAD LFN SIMILARLY TO TMEANLF
 	  TMEANDEDLF=0.0
	  DO 445 N1=1,NTLGRP
	     TLFN=DEDLFN(N1)
	     TTEMPOP=TEMPOP(N1)
	     TMEANDEDLF=TLFN*TTEMPOP/(TOTPOP+1)+TMEANDEDLF
445	  CONTINUE
C CNB JUL2000 {NEWPART} END

c      WRITE(SCLUN, '(A, A, F20.8)') 'TAPER is ', ACHAR(9), TAPER

        LAI=GLAHA*0.0001*0.0001
C TLAI is total LAI including dead leaves. This is used in calculating
c soil evaporation. Perhaps the total number of leaves adhering to each 
c stalk should be considered. 
c        TLAI=AMAX1(TLAI,LAI)
        TLAI=TLAHA*0.0001*0.0001
c When the ligule of 15th leaf of 1st tiller cohort appears then the 
C extinction coefficient is increased to cater for the more homogeneous 
C distribution of leaves. This is not used in Bootes photos. routine
       
	 
c CNBJUN2002: EXTINC-COEFF AT START AND MATURITY TO BE READ IN BY VAR FILE	 
	!  fr=max((20.0-alt)/20.0,0.0) 
      !  EXTINC=0.58+(0.84-0.58)*(1.0-fr)
	fr=max((LFNMXEXT-alt)/LFNMXEXT,0.0) 
      EXTINC=EXTCFST+(EXTCFN-EXTCFST)*(1.0-fr)
c CNBJUN2002: EXTINC-COEFF END


c      Singels, Hoffman 2017: Assuming that leaf-level photosynthesis rate measurements,
c      taken relative to NCo376, can only be scaled up to canopy level for sunlit leaves.
c      Cultivar differences with shaded leaves are likely to be smaller.  So, NCo376 
c      MaxPARCE value is used for shaded leaves and the actual cultivar value is used
c      for sunlit leaves only.
c      The separation between sunlit and shaded leaves is based on a calculation from: 
c      Monteith and Unsworth, 2013.
c     F_SL is used in SC_PHOTOS subroutine
c     Calculate sunlit LAI:
      LAI_SL = -1.0/EXTINC * (EXP(-EXTINC*LAI) - 1.0)
c     Sunlit fraction:
      IF (LAI .GT. 0.0) THEN
        F_SL = LAI_SL / LAI
      ELSE
        F_SL = 1.0
      ENDIF


c  This is midday Li
        LI=1.0-EXP(-EXTINC*LAI)
c Correct this upward for integreted diurnal Li
        li=min(li*(1.389-1.05*li+0.728*li**2),0.99)

c     MJ, 2006/09/06
c     CANESIM lodging!
c     ::::::::::::::::
c     Canesim: Fi = Fi * (1 - Flodge * Par2)
! -- Removed by MJ,2012.  Some confusion.  Lodging is taken into account in
! photosynthesis routine.
          LI = LI * (1 - LODGE_LI * LDG_LI_EFF)
c     ::::::::::::::::	  
        xli=li
c Use li from Bootes photosynthesis if selected (mdl>1)
c        if(mdl.gt.1) xli=bootli
        if(xli.gt.0.6.and.natli6.eq.99) natLi6=ntlgrp

c     MJ: replaced this line to ensure that everything
c     returns from the same place:
c        IF(IW.NE.1) RETURN
c     with:
      IF (IW .EQ. 1) THEN
        POP   = TOTPOP / 1000.0
        GLFN1 = Lt - DEDLFN(1)
      ENDIF

 300    FORMAT(2i4,f8.2,3f8.1,10i3,10f6.1)


c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     MASS BALANCE LAI
c     ----------------
c     EXPERIMENTAL!  MJ, 2008: I am attempting a mass-balance based
c     LAI calc.  Using a dry specific leaf area of 10 m2/kg,
c     canopy leaf area can be determined by photosynthesis 
c     added to the canopy

c     Specific leaf area (m2/kg):
c      SLA = 10.

c     Change in top DM (t/ha)
c      DTOPDMDT = TOPDM - TOPDM_yest

c     Leaf area in m2 / ha:
c     ha/ha = t/ha * (m2/kg / 10000.) = ha / kg * 1000. --> ha/t
c      MLAI = DTOPDMDT * (SLA / 10000. * 1000.)

c     TOPDM_yest = TOPDM

c     There are two issues to be overcome:
c     1. Feedback - the MLAI value needs to be used to calculate
c        fractional interception and used for photosynthesis, 
c        thus determing TOPDM for the next day --> MLAI
c     2. Trash production -  Leaf senescence is determined by
c        this Canop3 routine and is important for biomass
c        partitioning.



c *********
c     MJ, September 2006:
c     Moved this from Mainnewc.for to this canopy subroutine:
c     Move 0.16 (PERCoeff) and 0.864 (CHTCoeff) to species file.
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      ! MJ, AUg 2012: only accumulate stalk height after stalks
      ! have appeared!
      IF (CaneCrop%GROPHASE .GE. 3) THEN
        SHGT = SHGT + (SER * PERCoeff)
      ELSE
        SHGT = 0.
      ENDIF
      CANECROP%SHGT = SHGT
      IF (LFN(1).GT.0) 
     -    CANECROP%CANHEIGHT = (SHGT + LMAX(LFN(1)) * CHTCoeff) / 100.
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::

c     Copy values to modules:
c     Climate:
c     ::::::::
          CLIMATE%DTT    = DTT
          CLIMATE%TEMPMN = TEMPMN
          CLIMATE%TEMPMX = TEMPMX

c     CaneCrop:
c     :::::::::
          CaneCrop%AREAMX     = AREAMX
          CaneCrop%DEDLFN     = DEDLFN
          CaneCrop%EXTCFN     = EXTCFN
          CaneCrop%EXTCFST    = EXTCFST
          CaneCrop%GLFN       = GLFN
          CaneCrop%LEN        = LEN
          CaneCrop%LFN        = LFN
          CaneCrop%LFNMXEXT   = LFNMXEXT
          CaneCrop%LMAX       = LMAX
          CaneCrop%LOSING     = LOSING
          CaneCrop%LT         = LT
          CaneCrop%NTLGRP     = NTLGRP
          CaneCrop%PHYLO      = PHYLO
          CaneCrop%PHYLSWTCH  = PHYLSWTCH
          CaneCrop%TEMPOP     = TEMPOP
          CaneCrop%TMEANDEDLF = TMEANDEDLF
          CaneCrop%TMEANLF    = TMEANLF
          CaneCrop%TOTPOP     = TOTPOP
          CaneCrop%WIDCOR     = WIDCOR
          CaneCrop%WIDTH      = WIDTH
          CaneCrop%WMAX       = WMAX
c     Soil
c     ::::
          Soil%LL    = LL
          Soil%DLAYR = DLAYR  
          Soil%NLAYR = NLAYR
          Soil%SW    = SW
c     ::::
c     Growth object:
c     ::::::::::::::
          Growth%LAI  = LAI
          Growth%LI   = LI
          Growth%TLAI = TLAI
c     ::::::::::::::

c     Water:
c     ::::::
          WaterBal%PRECIP = PRECIP
          WaterBal%SWDF2  = SWDF2
c          WaterBal%TRWU   = TRWU
c     ::::::

c     END of RATE calculation
c     :::::::::::::::::::::::
      ENDIF
c     :::::::::::::::::::::::

        RETURN
        END

