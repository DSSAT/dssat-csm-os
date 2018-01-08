c*****PREDICT LEAF TIP EMERGENCE (LT), LEAF EXTENSION, AREA EXPANSION 
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


C ********LAI AND LI (LIGHT INTERCEPTION) *************************
      SUBROUTINE CANESIM_CANOPY (
c         [I] Cum. heat units (base defined in cult file, TTBASELFEX)
     -        CHUPI,
c         [I] Cum. heat units (base defined in cult file, TTBASEPOP); 
     -        CHU_CANESIM,
c         [I] Not sure...
     -        IW,
c         [O] Cum. stress days?
     -        STDAYC,
c         [O] Plant extension rate (cm/day)
     -        PER,
c         [O] Light extinction coefficient (req'd by TRANS)
     -        EXTINC,
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
      DIMENSION SHOOTX(30), ltill(30), ttref(30), istage(30)
     -  ,stdayd(30,70)

c     Previously non-declared parameters:
c     :::::::::::::::::::::::::::::::::::
      INTEGER IW
      REAL    BEST
      REAL    CHUPI
      REAL    CHU_CANESIM
      REAL    PER
      REAL    RESET
      REAL    ROWSPC
      
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
     	INTEGER L
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
      REAL    LFMXEXT_TT
     	REAL    POP
      REAL    MATURITY
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
      REAL    LAI
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
      REAL    DUL(NL)
      REAL    SAT(NL)

c     Water variables
c     :::::::::::::::
c     in use here:
      REAL PRECIP
      REAL SWDF2
c      REAL TRWU

c     Canopy (?) variables:
c     :::::::::::::::::::::
c     In use here:
!      REAL    AREAMX(70)
!      REAL    DEDLFN(30)
      REAL    EXTCFN
      REAL    EXTCFST

c      REAL    LEN(30,70)
      REAL    LEN(100,70)
!      INTEGER LFN(30)
!      INTEGER LFNMXEXT
      REAL    LMAX(70)
!      INTEGER LOSING
      INTEGER LT
      INTEGER NTLGRP
      REAL    PHYLO(2)
      REAL    PHYLSWTCH

c      REAL    TEMPOP(30)
      REAL    TEMPOP(100)
!      REAL    TMEANDEDLF
!      REAL    TMEANLF
      REAL    TOTPOP
!      REAL    WIDCOR
      REAL    WIDTH(70)
!      REAL    WMAX(70)

c     Coefficient relating PER to change in stalk height
c     I presume the fraction of plant extension that is stalk extension:
      REAL PERCoeff

c     Coefficient relating canopy height to first leaf maximum size
      REAL CHTCoeff

c     New derived variables for Plant Extension Rate
      REAL dPERdT, TBASEPER
c     For testing
!      REAL oldPER, oldoldPER

c     Cultivar params for leaf characteristics
!      REAL LMAX_CF(3), AREAMX_CF(3), WMAX_CF(3), LFNUM_THRESH
!      REAL MAXLFLENGTH, MAXLFWIDTH

c     Minimum final population
      REAL    POPN

c     Thermal time at which population increase ceases
      REAL    TT_POPGROWTH



c     END of common block variable definitions
c     ::::::::::::::::::::::::::::::::::::::::
c     ::::::::::::::
c     LODGING, MJ 2006/09/06:
c         The maximum percentage of light interception lost due to lodging
          REAL LDG_LI_EFF
c         Severity of lodging (calculated in PHOTOS)
          REAL LODGE_LI

c         Green leaf number?
!	    INTEGER GLFN

c         Not sure
          REAL    EXPAL(30)

c         Local variable for water balance (to be removed)
          CHARACTER*1 ISWWAT
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

c     Canesim-specific water stress variables:
c     ::::::::::::::::::::::::::::::::::::::::
          REAL CS_CNREDUC, CS_CNPERIOD, ESTRESS, AWC, TAM
          REAL AFV, CUMFV, SATs
          REAL SCOUNT


c     Error status of cultivar/species file reads
      LOGICAL CF_ERR, SPE_ERR
c     Temp values read from file
!      REAL T_MXLFAREA, T_MXLFARNO, AL
      REAL T_LFNMXEXT

c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     MJ, 2006/09/08
c     CANESIM CANOPY variables
c     ::::::::::::::::::::::::::::::::::::::::::::
c         Thermal time to half canopy (degree days, ? base T)
          REAL Tthalfo, Tthalfa, Tthalf

c         Thermal time
          REAL CumHu10, CumHuVar, Tti

c         Universal shape constant for Hill equation
          REAL HillPar1

!         Unit number for output
!          INTEGER SCLUN   !CHP

c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::



c     ~~~~~~  CODE  ~~~~~~~~~
c     ~~~~~~~~~~~~~~~~~~~~~~~
c     Copy values from modules:
c     Climate:
      DTT    = CLIMATE%DTT
      TEMPMN = CLIMATE%TEMPMN
      TEMPMX = CLIMATE%TEMPMX

c      ISWWAT = ISWITCH%ISWWAT
      ISWWAT = 'N'

c     ---------------------------------------------------------------------
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     Initialise (once!)
c     ::::::::::::::::::
      IF (CONTROL%DYNAMIC .EQ. SEASINIT) THEN

c     Init lodging variable, MJ 2006/09/06:
c     Note, this should be provided in variety file
          LDG_LI_EFF = .01


c         Some of these need to be read from cultivar file
c         :::::::::::::
          IW       = 0.


c         SPECIES coeffs:
c         ==================================
c         Coeffs for plant extension rate for
c         canopy height
          PERCoeff = 0.16
          CHTCoeff = 0.864
          CALL GET_SPECIES_COEFF(PERCoeff, 'PERCoeff', Control, 
     &                            SPE_ERR)
          CALL GET_SPECIES_COEFF(CHTCoeff, 'CHTCoeff', Control, 
     &                            SPE_ERR)

c         Universal shape constant for Hill equation
          HillPar1 = 2.453
          CALL GET_SPECIES_COEFF(HillPar1,  'HillPar1',  
     &                           Control, SPE_ERR)

c         Stress days reset variable (perhaps used?)
          RESET    = 5.
          CALL GET_SPECIES_COEFF(RESET,  'RESET',  Control, SPE_ERR)

c         END of species COEFFS
c         ==================================



c         READ VALUES FROM CULTIVAR FILE:
c         ===============================

c         Tthalfo: Half canopy thermal time at  1.4 row spacing
c         Read from variety file?
          Tthalfo = 250.

c         Tthalfa: Half canopy thermal time adjustment for row width
c         Read from variety file?
          Tthalfa = 125.

c         Read from file:
              CALL GET_CULTIVAR_COEFF(Tthalfo, 'Tthalfo', 
     -                               CONTROL,  CF_ERR)
              CALL GET_CULTIVAR_COEFF(Tthalfa, 'Tthalfa', 
     -                               CONTROL,  CF_ERR)

c         Canopy reduction fraction
c         Equivalent to Canesim PAR30:
          CS_CNREDUC = 0.3

c         Canopy reduction period
c         MJ, 2007-07-02: changed 21. to 28.
c         Ref? - ???
c         Equivalent to Canesim PAR31:
          CS_CNPERIOD = 28.

c         Read from file:
              CALL GET_CULTIVAR_COEFF(CS_CNREDUC, 'CS_CNREDUC', 
     -                               CONTROL,  CF_ERR)
              CALL GET_CULTIVAR_COEFF(CS_CNPERIOD, 'CS_CNPERIOD', 
     -                               CONTROL,  CF_ERR)


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

c         LFNMXEXT - Leaf number at which maximum light extinction coeff of
c         canopy is reached.
c         ::::::::::::::::::::::::::::::::::::::::::::
c         Default value:
              CaneCrop%LFNMXEXT   = 20

c         Read from file:
c             Note: this value is read as a REAL and then converted to integer:
              CALL GET_CULTIVAR_COEFF(T_LFNMXEXT, 'LFNMXEXT', 
     -                               CONTROL,  CF_ERR)
              CaneCrop%LFNMXEXT = INT(T_LFNMXEXT)
c         ::::::::::::::::::::::::::::::::::::::::::::

c         Cultivar parameters for plant extension rate
c         ::::::::::::::::::::::::::::::::::::::::::::
              dPERdT   =   .176
              TBASEPER = 10.057

c         Read from cult file:
              CALL GET_CULTIVAR_COEFF(dPERdT, 'dPERdT',
     -                                CONTROL, CF_ERR)

              CALL GET_CULTIVAR_COEFF(TBASEPER, 'TBASEPER',
     -                                CONTROL, CF_ERR)

c         Init lodging variable, MJ 2006/09/06:
          LDG_LI_EFF = .01
          CALL GET_CULTIVAR_COEFF(LDG_LI_EFF, 'LDG_FI_REDUC',  
     &                            Control, SPE_ERR)



c         END READ VALUES FROM CULTIVAR FILE
c         ==================================          



c         These are read from file in the SC_PHENOL routine
          PHYLO      = CaneCrop%PHYLO
          PHYLSWTCH  = CaneCrop%PHYLSWTCH

c         Calculate thermal time age at which maximum extinction
c         coefficient is reached:
          LFMXEXT_TT = (MAX(0., REAL(CaneCrop%LFNMXEXT) - PHYLSWTCH)
     &               *  PHYLO(2))
     &               + (PHYLSWTCH*PHYLO(1))

c         Echo for verification:
c          WRITE(*,'(A, F10.1)') 'Thermal time of extinction coeff
c     & maturity is ', LFMXEXT_TT

c         Init Light interception effect of lodging
c         If lodging is not enabled, this will stay 0.
          LODGE_LI    = 0.

c         Init leaf area index
          Growth%LAI  = 0.
          Growth%TLAI = 0.

c         Init Light Interception
          Growth%LI  = 0.



c         Stress counter
          SCOUNT = 0.
          CumFV  = 0.

c         Relative soil water content for reduction in transpiration
          Estress = 1. / WaterBal%RWUEP2


c         Water balance introduced continuity errors...
c         This attempts to remove this effect:
c     ::::::::::::::::::::::::::::::::::::::::

c         MJ: many of these are not used.  Remove sometime.
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
              DO I=1,30
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
              PER = 0.
              CaneCrop%CANHEIGHT = 0.


c         Init tempop:
          TEMPOP = 0.

c     Minimum final population
          POPN   = CaneCrop%POPN
c         MJ, 2007/08/04:
c         Thermal time at which population increase ceases
          TT_POPGROWTH = 600.   
          CALL GET_CULTIVAR_COEFF(TT_POPGROWTH, 'TT_POPGROWTH',
     -                            CONTROL, CF_ERR)

c     End of initialisation section
c    ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::




c     ---------------------------------------------------------------------
c     :::::::::::::::::::::::::::::::::::::::::::::
      ELSEIF (CONTROL%DYNAMIC .EQ. RATE) THEN
c     :::::::::::::::::::::::::::::::::::::::::::::

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
          EXTCFN     = CaneCrop%EXTCFN
          EXTCFST    = CaneCrop%EXTCFST
          NTLGRP     = CaneCrop%NTLGRP
          TOTPOP     = CaneCrop%TOTPOP
c     ::::::::::

c     Soil
c     ::::
          LL    = Soil%LL
          DLAYR = Soil%DLAYR  
          NLAYR = Soil%NLAYR
          SW    = Soil%SW
          DUL   = Soil%DUL
          SAT   = Soil%SAT
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
c     ::::::

        if(newbat.eq.1) ttref(ntlgrp)=chupi

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





C Stress days for leaf size and number are accumulated STDAYG as in MAIN 
C but are reset when soil  profile is half full. The stress increment 
C is doubled every 5 deg C above Tmax = 25C. This is to cope with the 
c damage done to a canopy which can remain green in a dry soil but will 
c lose leaves rapidly under very hot conditions.

       TMEAN=(TEMPMN+TEMPMX)/2.0
       STDAYC =STDAYC  + (1.0-SWDF2)*AMAX1(1.0,TEMPMX/5.0-4.0)

      IF (ISWITCH%ISWWAT .EQ. 'Y') THEN
c         If water stress is included:
          IF(precip.GT.reset) THEN
              STDAYC=0.0
          ENDIF
c          WRITE(*,*) 'STDAYC is ', STDAYC
      ELSE
c         If water stress excluded:
          STDAYC=0.0
      ENDIF

!      CALL GETLUN('WORK.OUT',SCLUN)
c      WRITE(SCLUN, '(A, F10.0)') 'STDAYC is ', STDAYC


c     Matthew Jones, 2007-05-08
c     :::::::::::::::::::::::::
c     Changed the following line:
cC                  PER MM PER HOUR *24/10 FOR CM/DAY 
c                   oldPER= MAX(0., SWDF2*(-1.77+0.176*TMEAN)*24.0/10.0)
c     To:
c       1. Derived from this: base T = 10.57 degree days; dPER/dTEMP = 0.176
c          for NCo376
c       2. Prevent negative PER with MAX() function
        PER = SWDF2 * dPERdT * MAX(0., TMEAN-TBASEPER) * 24./10.


c     CNBJUN2002: EXTINC-COEFF AT START AND MATURITY TO BE READ IN BY 
c     VAR FILE	 
c     Matthew Jones (2007-05-14): There is no FR variable in the 
c     Canesim canopy; FR is (I think) a measure of maturity.  Use TT for 
c     each leaf emergence to calculate, in TT, when full-extinction-coeff
c     leaf number is reached.  This leaf number is LFNMXEXT from the 
c     cultivar file.  The calculation is performed in the init section.
      MATURITY = MIN(1., CHUPI/LFMXEXT_TT)

      EXTINC=EXTCFST+(EXTCFN-EXTCFST)*MATURITY
c     ::::::::::::::::::::::::::::::::::
c     CNBJUN2002: EXTINC-COEFF END


c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     MJ, 2006/09/08
c     CANESIM CANOPY!!!
c     ::::::::::::::::::::::::::::::::::::::::::::

c         Cumulative heat units (base 16 deg C):
          CumHuVar = CHU_CANESIM

c         Cumulative heat units, base 10 degrees C
          CumHu10 = CHUPI



c         Hill model
c         Rowspc is row spacing
          Tthalf = Tthalfo - Tthalfa * (1.4 - ROWSPC)  
          Tti = CumHuVar / (2. * Tthalf)
          LI = 1. * Tti ** HillPar1 / (.5 ** HillPar1 + Tti**HillPar1)
c     ::::::::::::::::::::::::::::::::::::::::::::
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


c     CANESIM water stress
c     Taken from the SASRI Crop Forecast Canesim code
c     :::::::::::::::::::::::::::::::::::::::::::::::

      IF (ISWITCH%ISWWAT .EQ. 'Y') THEN
c         If water stress is included:
c         Canesim uses a single-layered soil.  The original code
c         is as follows:
c
c         If AWC <= Tam Then aFv = AWC / (Estress * Tam)    'Drought
c         If AWC > Tam Then aFv = (Sat - AWC) / (Sat - Tam) ' Water logged
c         If aFv > 1 Then aFv = 1
c         
c         Where TAM is total available moisture, AWC is soil water content,
c         Sat is saturated AWC
c         ::::::::::::::::::::
c         Code modified to handle a multi-layered soil profile:
          AWC = DOT_PRODUCT(SW-LL, DLAYR)
          TAM = DOT_PRODUCT(DUL-LL, DLAYR)
          SATs = DOT_PRODUCT(SAT, DLAYR)

c         Stress due to drought:
          IF (AWC .LE. TAM) THEN
              aFv = AWC / (Estress * TAM)
c         Stress due to waterlogging:
          ELSE IF (AWC .GT. TAM) Then 
              aFv = (SATs - AWC) / (SATs - TAM)
          ENDIF

          IF (aFv .GT. 1.) aFv = 1.

    
c         Increase / decrease cumulative measure of stress
c         ::::::::::::::::::::::::::::::::::::::::::::::::
          IF (aFv .LT. 0.5) THEN 
c             Increment stressed days if too dry or too wet
              SCOUNT = SCOUNT + 1.

          ELSEIF (aFv .GT. 0.5) THEN
c             Decrement stress if good conditions abound
              SCOUNT = SCOUNT - 1.

          ENDIF
c         Implicit: if SCOUNT .EQ. 0.5, maintain current stress
c         but do not increase/decrease it

          IF (SCOUNT .GE. CS_CNPERIOD) THEN 
              SCOUNT = CS_CNPERIOD
          ELSEIF (SCOUNT .LT. 0.) THEN
              SCOUNT = 0.
          ENDIF

c         Calculate cumulative stress index:
c          ::::::::::::::::::::::::::::::::::
          CUMFV = SCOUNT / CS_CNPERIOD
      ELSE
          CUMFV = 0.
      ENDIF

c     Reduce canopy due to water stress:
c     ::::::::::::::::::::::::::::::::::
          LI = LI * (1. - CS_CNREDUC * CUMFV)
c     ::::::::::::::::::::::::::::::::::

 

c     MJ, 2006/09/06
c     CANESIM lodging!
c     Reduce LI due to lodging
c     ::::::::::::::::::::::::
c     Canesim: Fi = Fi * (1 - Flodge * Par2)
          LI = LI * (1 - LODGE_LI * LDG_LI_EFF)
c     ::::::::::::::::	  

      LI = MIN(LI, 0.999)
      LI = MAX(LI, 0.001)

c     MJ, 2007/05/14
c     Calculate LAI from LI and EXTINC
c     Some quality algebra went into deriving this baby!:
c     :::::::::::::::::::::::::::::::::::::::::::::::::::
          LAI = (LOG(1. - LI))/(-EXTINC)
          TLAI = LAI


c     :::::::::
        xli=li
        if(xli.gt.0.6.and.natli6.eq.99) natLi6=ntlgrp

c     MJ: replaced this line to ensure that everything
c     returns from the same place:
c        IF(IW.NE.1) RETURN
c     with:
      IF (IW .EQ. 1) THEN
        POP   = TOTPOP / 1000.0
c        GLFN1 = Lt - DEDLFN(1)
      ENDIF

 300    FORMAT(2i4,f8.2,3f8.1,10i3,10f6.1)


c     MJ, added 4 August 2007 to accommodate Canesim Canopy
c     Fix properly soon!
       IF (CHU_CANESIM .GE. TT_POPGROWTH) THEN
           TOTPOP = MAX(TOTPOP, POPN)
       ENDIF

c *********
c     MJ, September 2006:
c     Moved this from Mainnewc.for to this canopy subroutine:
c     Move 0.16 (PERCoeff) and 0.864 (CHTCoeff) to species file.
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SHGT = SHGT + (PER * PERCoeff)
      CANECROP%CANHEIGHT = SHGT
c      IF (LFN(1).GT.0) 
c     -    CANECROP%CANHEIGHT = (SHGT + LMAX(LFN(1)) * CHTCoeff) / 100.
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::

c     Copy values to modules:
c     Climate:
c     ::::::::
          CLIMATE%DTT    = DTT
          CLIMATE%TEMPMN = TEMPMN
          CLIMATE%TEMPMX = TEMPMX

c     CaneCrop:
c     :::::::::

          CaneCrop%EXTCFN     = EXTCFN
          CaneCrop%EXTCFST    = EXTCFST
          CaneCrop%NTLGRP     = NTLGRP
          CaneCrop%PHYLO      = PHYLO
          CaneCrop%PHYLSWTCH  = PHYLSWTCH
          CaneCrop%TEMPOP     = TEMPOP
          CaneCrop%TOTPOP     = TOTPOP
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
