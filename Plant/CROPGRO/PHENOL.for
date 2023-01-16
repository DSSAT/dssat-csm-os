C=======================================================================
C  PHENOL, Subroutine, J. W. Jones
C  Calculates phenological development.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1993 J.W. Jones, K.J. Boote, G. Hoogenboom
C  04/24/1994 NBP Replaced TAIRHR, TAVG with TGRO, TGROAV.
C  01/08/1997 GH  Add transplant effect
C  07/09/1997 CHP modified for CROPGRO restructuring
C             Added DYNAMIC variable for model control
!  07/13/2006 CHP Added P model
!  06/11/2007 CHP PStres2 affects growth
!  06/15/2022 CHP Added CropStatus
C-----------------------------------------------------------------------
!     Called from:    Main program
!     Calls:          IPPHENOL
!                     RSTAGES
!                     VSTAGES
!                     CURV
C=======================================================================

      SUBROUTINE PHENOL(CONTROL, ISWITCH, 
     &    DAYL, NSTRES, PStres2, SOILPROP, ST,            !Input
     &    SW, SWFAC, TGRO, TMIN, TURFAC, XPOD, YRPLT,     !Input
     &    CropStatus,                                     !Output
     &    DRPP, DTX, DXR57, FRACDN, MDATE, NDLEAF,        !Output
     &    NDSET, NR1, NR2, NR5, NR7, NVEG0, PHTHRS,       !Output
     &    RSTAGE, RVSTGE, STGDOY, SeedFrac, TDUMX,        !Output
     &    TDUMX2, VegFrac, VSTAGE, YREMRG, YRNR1,         !Output
     &    YRNR2, YRNR3, YRNR5, YRNR7)                     !Output

!----------------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL CURV, IPPHENOL, RSTAGES, VSTAGES
      SAVE
!----------------------------------------------------------------------------
      INTEGER NPHS
      PARAMETER (NPHS = 13)

      CHARACTER*1 ISIMI, ISWWAT, PLME
      CHARACTER*2 CROP
      CHARACTER*3 CTMP(20), DLTYP(20)

      INTEGER JPEND, NDVST, NVEG1, YREMRG
      INTEGER DAS, YRDOY, YRPLT, YRSIM, CropStatus
      INTEGER DYNAMIC
      INTEGER I, J, K, NLAYR
      INTEGER NDLEAF, NDSET, NR1, NR2, NR5, NR7, NVEG0, RSTAGE
      INTEGER YRNR1, YRNR2, YRNR3, YRNR5, YRNR7, MDATE
      INTEGER STGDOY(20)
      INTEGER NPRIOR(20), NVALPH(20), TSELC(20)

      REAL DAYL, NSTRES, SWFAC, TMIN, TURFAC, XPOD    !, TGROAV
      REAL DRPP, DTX, DXR57, FRACDN, RVSTGE, TDUMX, TDUMX2, VSTAGE
      REAL ATEMP, CLDVAR, CLDVRR, CSDVAR, CSDVRR, EVMODC
      REAL MNEMV1, MNFLLL, MNFLHM, OPTBI
      REAL SDEPTH, SDAGE, SLOBI, THVAR, TRIFOL
      REAL DTRY, FTHR, SWFEM, TNTFAC, TNTFC2
      REAL TSDEP, XDEP, XDEPL, ZMODTE

      REAL TB(5), TO1(5), TO2(5), TM(5)
      REAL PHTHRS(20)
      REAL LL(NL), DUL(NL), SAT(NL), DLAYR(NL)
      REAL WSENP(20), NSENP(20), PSENP(20), PHZACC(20)
      REAL SW(NL), ST(NL)
      REAL FNSTR(20), FPSTR(20), FSW(20), FT(20), FUDAY(20)
      REAL TGRO(TS)

      REAL  CURV  !Function subroutine

!     P Module
      REAL PStres2
      REAL SeedFrac, VegFrac

!-----------------------------------------------------------------------
      TYPE (ControlType) CONTROL
      TYPE (SoilType) SOILPROP
      TYPE (SwitchType) ISWITCH

!     Transfer values from constructed data types into local variables.
      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM

      DLAYR  = SOILPROP % DLAYR
      DUL    = SOILPROP % DUL
      LL     = SOILPROP % LL
      NLAYR  = SOILPROP % NLAYR
      SAT    = SOILPROP % SAT

      ISWWAT = ISWITCH % ISWWAT
      ISIMI  = ISWITCH % ISIMI

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN

!-----------------------------------------------------------------------
!     Subroutine IPPHENOL reads required phenology variables from input
!     files.  
!-----------------------------------------------------------------------
      CALL IPPHENOL(CONTROL,
     &    ATEMP, CLDVAR, CLDVRR, CSDVAR, CSDVRR, CROP,    !Output
     &    CTMP, DLTYP, EVMODC, NPRIOR, NSENP, OPTBI,      !Output
     &    PHTHRS, PLME, PSENP, SDAGE, SDEPTH, SLOBI,      !Output
     &    THVAR, TRIFOL, TSELC, TB, TO1, TO2, TM, WSENP)  !Output

C-----------------------------------------------------------------------
C     Set minimum days for phenological events under optimum conditions
C     (temperature and short photoperiod)
C-----------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
C       Minimum days from emergence to Vegetative Growth Stage 1:
        MNEMV1 = PHTHRS(2)

C       Minimum days from start of flowering to last leaf appearance:
        MNFLLL = PHTHRS(13)

C       Number of days from flowering to harvest maturity
        MNFLHM = PHTHRS(8) + PHTHRS(10) + PHTHRS(11)
      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!     Initialization variables from INPLNT
!-----------------------------------------------------------------------
      DRPP   = 0.0
      DTX    = 0.0
      DXR57  = 0.0
      FRACDN = 0.0
      TDUMX  = 0.0
      TDUMX2 = 0.0
      TNTFAC = 0.0
      TNTFC2 = 0.0
      DO J = 1, 20
        FNSTR(J) = 1.
        FPSTR(J) = 1.
        FSW(J)   = 1.
        FT(J)    = 0.
        FUDAY(J) = 0.
      ENDDO

      CALL RSTAGES(CONTROL,
     &    FNSTR, FPSTR, FSW, FT, FUDAY, ISIMI, NPRIOR,    !Input
     &    PHTHRS, PLME, SDEPTH, YRDOY, YRPLT, YRSIM,      !Input
     &    CropStatus,                                     !Output
     &    JPEND, MDATE, NDLEAF, NDSET, NDVST, NVALPH,     !Output
     &    NVEG0, NVEG1, NR1, NR2, NR5, NR7, PHZACC,       !Output
     &    RSTAGE, STGDOY, SeedFrac, VegFrac, YREMRG,      !Output
     &    YRNR1, YRNR2, YRNR3, YRNR5, YRNR7)              !Output

      CALL VSTAGES(
     &    DAS, DTX, EVMODC, MNEMV1, NDVST,                !Input
     &    NVEG0, NVEG1, PHZACC, PLME, TRIFOL,             !Input
     &    TURFAC, XPOD, YRDOY, YRPLT,                     !Input
     &    RVSTGE, VSTAGE,                                 !Output
     &    SEASINIT)                                       !Control

C***********************************************************************
C***********************************************************************
C     Daily Rate calculations
C***********************************************************************
      ELSE IF (DYNAMIC .EQ. RATE) THEN
C-----------------------------------------------------------------------
C     Compute temp, daylength, and water effects on development,
C-----------------------------------------------------------------------
C   EMERGENCE PHASE ONLY
C-----------------------------------------------------------------------
      IF(NVEG0 .GT. DAS) THEN
        FUDAY(1) = 1.
        FNSTR(1) = 1.
        FPSTR(1) = 1.
        K = TSELC(1)

        !IF(ISWWAT .EQ. 'Y') THEN
C-----------------------------------------------------------------------
C      Compute average soil temp, water in top 10 cm for emergence phase
C         SWFEM = Average soil water content of top 10 cm
C         TSDEP = Average temperature of top 10 cm
C-----------------------------------------------------------------------
        XDEP = 0.0
        SWFEM = 0.0
        TSDEP = 0.0

        DO I = 1, NLAYR
          XDEPL = XDEP
          XDEP = XDEP + DLAYR(I)
          DTRY = MIN(DLAYR(I),10. - XDEPL)

          IF (ISWWAT .EQ. 'Y') THEN
            IF(SW(I) .LE. DUL(I))THEN
              SWFEM = SWFEM + DTRY *
     &            (MAX(SW(I) - LL(I),0.0)) / (DUL(I) - LL(I))
            ELSE
              SWFEM = SWFEM + DTRY *
     &            (MAX(SAT(I) - SW(I),0.0)) / (SAT(I) - DUL(I))
            ENDIF
          ENDIF

          TSDEP = TSDEP + DTRY * ST(I)
          IF (XDEP .GE. 10.) GO TO 230
        ENDDO

  230   TSDEP = TSDEP / 10.
        
C-----------------------------------------------------------------------
C      Compute temperature and soil water effects for phase 1, emergence
C-----------------------------------------------------------------------
        FT(1) = CURV(CTMP(1),TB(K),TO1(K),TO2(K),TM(K),TSDEP)

        IF (ISWWAT .EQ. 'Y') THEN
          SWFEM = (SWFEM / 10.) * 100.0
          FSW(1) = CURV('LIN',0.0,20.0,100.,1000.,SWFEM)
        ELSE
          FSW(1) = 1.
          !FT(1) = CURV(CTMP(1),TB(K),TO1(K),TO2(K),TM(K),TGROAV)
        ENDIF
        FSW(1) = 1. + (1.-FSW(1))*WSENP(1)
      ENDIF
C-----------------------------------------------------------------------
C     Compute dev rates for all other phases, using hourly air temp
C-----------------------------------------------------------------------
      DO J = 2,NPHS
        K = TSELC(J)
        FT(J) = 0.0
        DO I = 1,TS
          FTHR = CURV(CTMP(J),TB(K),TO1(K),TO2(K),TM(K),TGRO(I))
          FT(J) = FT(J) + FTHR/REAL(TS)
        ENDDO
C 24 changed to TS by Bruce Kimball on 3Jul17

        IF (DAS .LT. NR1) THEN
          FUDAY(J) = CURV(DLTYP(J),1.0,CSDVAR,CLDVAR,THVAR,DAYL)
        ELSE
          FUDAY(J) = CURV(DLTYP(J),1.0,CSDVRR,CLDVRR,THVAR,DAYL)
        ENDIF

        FSW(J)   = 1. + (1. - SWFAC)  * WSENP(J)
        FNSTR(J) = 1. + (1. - NSTRES) * NSENP(J)
        FPSTR(J) = 1. + (1. - PStres2) * PSENP(J)
      ENDDO
C-----------------------------------------------------------------------
C     Transplants
C-----------------------------------------------------------------------
      IF (PLME .EQ. 'T' .AND. YRPLT .EQ. YRDOY) THEN
        K = TSELC(2)
        FT(2) = CURV(CTMP(2),TB(K),TO1(K),TO2(K),TM(K),ATEMP)
        PHZACC(2) = FT(2) * SDAGE
      ENDIF

C-----------------------------------------------------------------------
C     The effect of Tmin on rate of development from emergence to
C     flowering. Piper et al., (submitted to Field Crops Research, 1995)
C-----------------------------------------------------------------------
      ZMODTE = 1.0
      IF (TMIN .LT. OPTBI) THEN
        ZMODTE = 1. - (SLOBI * (OPTBI - TMIN))
        ZMODTE = AMAX1(0.0, ZMODTE)
        ZMODTE = AMIN1(1.0, ZMODTE)
      ENDIF
      FT(4) = FT(4) * ZMODTE
      FT(5) = FT(5) * ZMODTE
C-----------------------------------------------------------------------
C     Compute rates of development to be used in other parts of model
C     based on veg, early rep, and late rep temp sensitivities, respectively.
C     Physiological days during today for vegetative development (DTX),
C     physiological days during the day for reproductive development
C     (TNTFAC & TNTFC2), and photothermal days during the day (TDUMX & TDUMX2)
C-----------------------------------------------------------------------
      DTX    = FT(2)
      TNTFAC = FT(6)
      TNTFC2 = FT(10)
C-----------------------------------------------------------------------
C     DRPP affects seed & shell numbers set, seed and shell growth rates,
C     ACCAGE, PODADD, FLWADD, FLWADD.  Okay to use the "SEEDFILL"
C     photoperiod.  This change will make TDUMX2 sensitive to the R5-R7
C     period and affect N mobilization.
C-----------------------------------------------------------------------
      DRPP   = FUDAY(6)
      TDUMX  = TNTFAC * DRPP
      TDUMX2 = TNTFC2 * FUDAY(10)

C-----------------------------------------------------------------------
C    Calculate rate of V-stage change for height and width determination
C-----------------------------------------------------------------------
      CALL VSTAGES(
     &    DAS, DTX, EVMODC, MNEMV1, NDVST,                !Input
     &    NVEG0, NVEG1, PHZACC, PLME, TRIFOL,             !Input
     &    TURFAC, XPOD, YRDOY, YRPLT,                     !Input
     &    RVSTGE, VSTAGE,                                 !Output
     &    RATE)                                           !Control

C**********************************************************************
C**********************************************************************
C     Daily Integration 
C**********************************************************************
      ELSE IF (DYNAMIC .EQ. INTEGR) THEN

C----------------------------------------------------------------------
C     Check to see if stages occur today, if so set them in RSTAGES
C----------------------------------------------------------------------
      CALL RSTAGES(CONTROL,
     &    FNSTR, FPSTR, FSW, FT, FUDAY, ISIMI, NPRIOR,    !Input
     &    PHTHRS, PLME, SDEPTH, YRDOY, YRPLT, YRSIM,      !Input
     &    CropStatus,                                     !Output
     &    JPEND, MDATE, NDLEAF, NDSET, NDVST, NVALPH,     !Output
     &    NVEG0, NVEG1, NR1, NR2, NR5, NR7, PHZACC,       !Output
     &    RSTAGE, STGDOY, SeedFrac, VegFrac, YREMRG,      !Output
     &    YRNR1, YRNR2, YRNR3, YRNR5, YRNR7)              !Output

C-----------------------------------------------------------------------
C     Special accumulators used in other parts of the model
C-----------------------------------------------------------------------
C     Canopy age, flowering to harvest maturity, AGELF
C-----------------------------------------------------------------------
C     FRACDN is relative time from flowering to last leaf, modify leaf part
C-----------------------------------------------------------------------
      IF (DAS .GE. NR1) THEN
        FRACDN = PHZACC(13)/MNFLLL
        FRACDN = AMIN1(1.0,FRACDN)
      ENDIF
C-----------------------------------------------------------------------
C     DXR57-rel time from R5 to R7, modifies N mobilization
C-----------------------------------------------------------------------
      IF (DAS .GT. NR5) THEN
        DXR57 = PHZACC(10)/PHTHRS(10)
        DXR57 = MIN(DXR57,1.0)
      ELSE
        DXR57 = 0.0
      ENDIF

!-----------------------------------------------------------------------
!     Calculate V-stages
!-----------------------------------------------------------------------
      CALL VSTAGES(
     &    DAS, DTX, EVMODC, MNEMV1, NDVST,                !Input
     &    NVEG0, NVEG1, PHZACC, PLME, TRIFOL,             !Input
     &    TURFAC, XPOD, YRDOY, YRPLT,                     !Input
     &    RVSTGE, VSTAGE,                                 !Output
     &    INTEGR)                                         !Control

C***********************************************************************
!     End of DYNAMIC IF construct
C***********************************************************************
      END IF
!-----------------------------------------------------------------------
      RETURN
      END   !SUBROUTINE PHENOL

C-----------------------------------------------------------------------
C     End Subroutine PHENOL
C-----------------------------------------------------------------------



C=======================================================================
C  VSTAGES, Subroutine
C  Calculates V-stages
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  07/24/98 CHP Pulled code from PHENOL subroutine
C-----------------------------------------------------------------------
!     Called from:    PHENOL
!     Calls:          None
C=======================================================================

      SUBROUTINE VSTAGES(
     &    DAS, DTX, EVMODC, MNEMV1, NDVST,                !Input
     &    NVEG0, NVEG1, PHZACC, PLME, TRIFOL,             !Input
     &    TURFAC, XPOD, YRDOY, YRPLT,                     !Input
     &    RVSTGE, VSTAGE,                                 !Output
     &    DYNAMIC)                                        !Control

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      CHARACTER*1 PLME
      INTEGER DYNAMIC
      INTEGER DAS, NVEG0, NVEG1, NDVST
      INTEGER YRPLT, YRDOY
      REAL VSTAGE, RVSTGE, VSTGED, VSTAGP
      REAL MNEMV1, TRIFOL, EVMODC, EVMOD, DTX
      REAL TURFAC, XPOD
      REAL PHZACC(20)

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      VSTAGE = 0.0
      RVSTGE = 0.0
      VSTGED = 0.0
      VSTAGP = 0.0
      RVSTGE = 0.0

!***********************************************************************
!***********************************************************************
C     Daily Rate Calculations 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
!    Calculate rate of V-stage change for height and width determination
!-----------------------------------------------------------------------
      RVSTGE = 0.

      IF (DAS .GE. NVEG0 .AND. DAS .LE. NDVST + ANINT(VSTGED)) THEN
        IF (DAS .GT. NDVST .AND. VSTGED .GT. 0.001) THEN
          RVSTGE = 1. / VSTGED
        ELSE
          RVSTGE = VSTAGE - VSTAGP
        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     Daily Integration 
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
C     V-STAGE between emergence and unifoliate (V-1) is determined
C     by the physiological accumulator and the minimum number of
C     days between emergence and V-1 under optimum temperature (MNEMV1)
C     V-STAGE after V-1 is determined by the leaf appearance rate
C     (TRIFOL), a water stress factor (TURFAC) and physiological units
C     for today (DTX).
C-----------------------------------------------------------------------
      IF (RVSTGE .GT. 1.E-6) THEN
        VSTGED = 1. / RVSTGE
      ELSE
        VSTGED = 0.0
      ENDIF
      VSTAGP = VSTAGE
!-----------------------------------------------------------------------
!     V-Stage for transplants
!-----------------------------------------------------------------------
      IF (PLME .EQ. 'T' .AND. YRPLT .EQ. YRDOY) THEN
        VSTAGE = 1. + (PHZACC(2) - MNEMV1) * TRIFOL
      ENDIF

!-----------------------------------------------------------------------
      IF (DAS .GE. NVEG0 .AND. DAS .LE. NDVST) THEN
        IF (DAS .LT. NVEG1) THEN
          VSTAGE  = PHZACC(2)/MNEMV1
        ELSE
          IF (VSTAGE .LT. ABS(EVMODC) .AND. 
     &        ABS(EVMODC) .GT. 0.0001) THEN
            EVMOD = 1.0 + (ABS(EVMODC)- VSTAGE) / EVMODC
            EVMOD = AMIN1(2.0,EVMOD)
            EVMOD = AMAX1(0.0,EVMOD)
          ELSE
            EVMOD = 1.0
          ENDIF
          VSTAGE = VSTAGE + DTX * TRIFOL * EVMOD*TURFAC*(1.0-XPOD)
        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     End of DYNAMIC IF construct
!***********************************************************************
      END IF
!***********************************************************************
      RETURN
      END   !SUBROUTINE VSTAGES

!-----------------------------------------------------------------------
!     PHENOLOGY VARIABLES LIST
!-----------------------------------------------------------------------
! ATEMP     Temperature of transplant environment (°C)
! CLDVAR    Critical daylength above which development rate remains at min 
!             value (prior to flowering) (hours)
! CLDVRR    Critical daylength above which development rate remains at min 
!             value (after flowering) (hours)
! CROP      Crop identification code 
! CSDVAR    Critical daylength above which development rate decreases 
!             (prior to flowering) (hours)
! CSDVRR    Critical daylength above which development rate decreases 
!             (after flowering) (hours)
! CTMP(I)   Type of curve used for temp. function for phase I: LIN=linear, 
!             QDR=quadratic, SIN=sine function 
! CURV      Function subroutine 
! DAS       Days after start of simulation (days)
! DAYL      Day length on day of simulation (from sunrise to sunset) (hr)
! DLAYR(L)  Soil thickness in layer L (cm)
! DLTYP(I)  Type of curve used for daylength function for phase I:  NON=no 
!             photoperiod sensitivity, INL=inverse linear 
! DRPP      Photoperiod days which occur in a real day
!             (photoperiod days / day)
! DTRY      Effective depth of current soil layer (cm)
! DTX       Thermal time that occurs in a real day based on vegetative 
!             development temperature function (thermal days / day)
! DUL(L)    Volumetric soil water content at Drained Upper Limit in soil 
!             layer L (cm3 [H2O] /cm3 [soil])
! DXR57     Relative time between first seed (NR5) and physiological 
!             maturity (NR7) 
! EVMOD     Modifies rate of development - see EVMODC 
! EVMODC    Modifier of rate of vegetative node appearance for the first 
!             few nodes, primarily used for peanut 
! FNSTR(I)  Nitrogen stress function (0 to 1) for phase I 
! FPSTR(I)  Phosphorus stress function (0 to 1) for phase I 
! FRACDN    Relative time between flowering (NR1) and last leaf appearance 
!             (NDLEAF) 
! FSW(I)    Water stress function (0.0 to 1.0) for phase I 
! FT(I)     Temperature function (0-1) for phase I 
! FTHR      Used to calculate hourly air temperature (°C)
! FUDAY(I)  Effect of daylength on development progress (0-1) for phase I 
! ISIMI      Start of simulation code
!               E = On reported emergence day
!               I = When initial conditions measured
!               P = On reported planting date
!               S = On specified date
! ISWWAT     Simulation control switch
!               Y = Simulate water process
!               N = Don't simulate water process
! JPEND     Day when juvenile phase ends and plants first become sensitive 
!             to photoperiod (days)
! LL(L)     Volumetric soil water content in soil layer L at lower limit
!             (cm3/cm3)
! MNEMV1    Minimum time from emergence to unifoliate (V1) (thermal days)
! MNFLHM    Minimum time from flowering to harvest maturity (thermal days)
! MNFLLL    Minimum time from flowering to last leaf (thermal days)
! NDLEAF    Day when leaf expansion ceased (days)
! NDSET     Normal time by which a pod load (full number) should be 
!             achieved with no water stress (days)
! NDVST     Day on which last main stem node formed (days)
! NL        Maximum number of soil layers = 20 
! NPHS      Number of plant phases 
! NPRIOR(I) The phase of growth at which phase I accumulator can start 
! NR1       Day when 50% of plants have at least one flower (days)
! NR2       Day when 50% of plants have one peg (peanuts only) (days)
! NR5       Day when 50% of plants have pods with beginning seeds (days)
! NR7       Day when 50% of plants first have yellowing or maturing pods
!             (days)
! NSENP(I)  Sensitivity of phase I to Nitrogen stress. Varies from -1 
!             (slows dev) to +1 (hastens dev) 
! NSTRES    Nitrogen stress factor (1=no stress, 0=max stress) 
! NVALPH(I) Day when stage (I) occurred. (days)
! NVEG0     Day of emergence (days)
! NVEG1     1st day with 50% of plants w/ completely unrolled leaf at 
!             unifoliate node (days)
! OPTBI     Temperature below which growth rate is slowed from emergence to 
!             flowering (°C)
! PHTHRS      Time that must accumulate (by phase) for the next
!                 stage to occur (thermal or photo-thermal days)
!                 under optimal temp. and daylength
!                 (1): seed germination
!                 (2): emergence to 1st true leaf
!                 (3): emergence to end of juv. phase
!                 (4): floral induction
!                 (6): 1st flower to 1st peg > 0.5 cm
!                 (8): 1st flower to 1st seed
!                 (10): 1st seed to physiological maturity
!                 (11): physiological and harvest maturity
!                 (12): 1st flower to last leaf
!                 (13): 1st flower to end of leaf growth
! PHZACC(I) Cumulative. time of progression from the start of phase I
!             (thermal or photothermal days)
! PLME       Planting method (read from FILEX)
!               T = transplant
!               S = seed
!               P = pre-germinated seed
!               N = nursery
! RSTAGE    Number of RSTAGES which have occurred. 
! RVSTGE    Rate of VSTAGE change (nodes/day)
! SAT(L)    Volumetric soil water content in layer L at saturation
!             (cm3 [water] / cm3 [soil])
! SDAGE     Transplant age (days)
! SDEPTH    Planting depth (cm)
! SLOBI     Sensitivity of growth rate to minimum temperatures from 
!             emergence to flowering 
! ST(L)     Soil temperature in soil layer L (°C)
! STGDOY(I) Day when stage I occurred (YYDDD)
! STNAME    Output headings for specified crops 
! SW(L)     Volumetric soil water content in layer L
!             (cm3 [water] / cm3 [soil])
! SWFAC     Effect of soil-water stress on photosynthesis, 1.0=no stress, 
!             0.0=max stress 
! SWFEM     Average soil water content in top 10 cm of soil.  Used to 
!             modify emergence rate. (cm3/cm3)
!            TO2, and  TM Coefficients which define daily temperature distr
! TDUMX     Photo-thermal time that occurs in a real day based on early 
!             reproductive development temperature function
!             (photo-thermal days / day)
! TDUMX2    Photo-thermal time that occurs in a real day based on late 
!             reproductive development temperature function
!             (photo-thermal days / day)
! TGRO(I)   Hourly air temperature (°C)
! TGROAV    Average daily canopy temperature (°C)
! THVAR     Minimum relative rate of reproductive development under long 
!             days and optimal temperature 
! TIMDIF    Integer function which calculates the number of days between 
!             two Julian dates (da)
! TMIN      Minimum daily temperature (°C)
! TNTFAC    Thermal time that occurs in a single real day based on early 
!             reproductive development temperature function
!             (thermal days / day)
! TNTFC2    Thermal time that occurs in a single real day based on late 
!             reproductive development temperature function
!             (thermal days / day)
! TRIFOL    Rate of appearance on leaves on mainstem. Maximum rate of 
!             V-stage formation (leaves per thermal day)
! TSDEP     Average temperature in top 10 cm of soil. Used to modify 
!             emergence rate of development. (°C)
! TSELC      Number of temperature curve (by phase)
!                 1 = vegetative
!                 2 = early reproductive
!                 3 = late reproductive
! TURFAC    Water stress factor for expansion (0 - 1) 
! VSTAGE    Number of nodes on main stem of plant 
! VSTAGP    Previous VSTAGE 
! VSTGED    Duration of time for new node development (days/node)
! WSENP(I)  Sensitivity of phase I to water stress, varying from -1 (slows 
!             dev) to 1 (hastens dev) 
! XDEP      Depth to bottom of current soil layer (cm)
! XDEPL     Depth to top of current soil layer (cm)
! XPOD      Growth partitioning to pods which slows node appearance
!             (fraction)
! YRDOY     Current day of simulation (YYDDD)
! YREMRG    Day of emergence (YYDDD)
! YRNR1     Day when 50% of plants have at least one flower (YYDDD)
! YRNR2     Day when 50% of plants have one peg (peanuts only) (YYDDD)
! YRNR3     Day when 50% of plants have at least one beginning pod (YYDDD)
! YRNR5     Day when 50% of plants have pods with beginning seeds (YYDDD)
! YRNR7     Day when 50% of plants first have yellowing or maturing pods
!             (YYDDD)
! MDATE     Date of harvest maturity (YYDDD)
! YRPLT     Planting date (YYDDD)
! YRSIM     Start of simulation date (YYDDD)
! ZMODTE    Factor which accounts for effect of TMIN on rate of development 
!             from emergence to flowering 
C=======================================================================

