C=======================================================================
C  COPYRIGHT 1998-2010 The University of Georgia, Griffin, Georgia
C                      University of Florida, Gainesville, Florida
C                      Iowa State University, Ames, Iowa
C                      International Center for Soil Fertility and 
C                       Agricultural Development, Muscle Shoals, Alabama
C                      University of Guelph, Guelph, Ontario
C  ALL RIGHTS RESERVED
C=======================================================================
C=======================================================================
C  SoilNi, Subroutine
C
C  Determines inorganic N transformations
C  This routine was modified from NTRANS when the module was split into
C  organic and inorganic sections.
C-----------------------------------------------------------------------
C  Revision history
C  . . . . . .  Written
C  08-08-1992 WTB Modified for version 3 input/output (matches LeGRO).
C  02-08-1993 PWW Header revision and minor changes. 
C  06/09/1999 AJG Completely revised the soil N and SOM module
C               Also changed variable names:
C               OLD      NEW                OLD      NEW
C               ------   ------             ------   ------ 
C               AINO3    TNO3               HUM      HUMC   
C               ANH4     TNH4               INH4     TNH4   
C               ANO3     TNO3               MF       WFSOM  
C               B2       SNH4_AVAIL         RNTRF    NITRIF
C               BB       NITRIF             SWF      WFUREA
C               DNRATE   DENITRIF           TF       TFUREA
C               FT       TFDENIT            TFY      TFNITY
C               FW       WFDENIT            TSIN     TNH4NO3
C  06/21/1999 CHP Modular format.
C  03/29/2000 GH  Corrections for SARNC, CW, and DNRATE
C  01/01/2000 CHP/AJG Integrated the CENTURY-based and CERES-based SOM
C               modules with CHP's modular structure.
C  03/17/2000 GH  Incorporated in CROPGRO 
C  06/12/2002 UPS/CHP Added modifications for flooded conditions
C                   Added ammonia volatilization (OXLAYER) and replaced
C                     nitrification section based on Gilmour
C  11/07/2003 CHP Added tile drainage to NFLUX
C  11/18/2003 CHP Changed calculation of CW based on email from UPS
C  04/20/2004 US  Modified DLAG, removed IFDENIT
!  05/18/2004 AJG Renamed variables for P module:
!                         NAPNIT   to NAPFER
!                         AMTNIT   to AMTFER
!  01/14/2005 CHP/UPS Split NTRANS into separate organic and 
!                  inorganic routines.
!  02/25/2005 CHP changed HUMC to SSOMC to match Century variable name
!  04/13/2005 CHP changed subroutine name to SoilNi.FOR (was SoilN_inorg)
C-----------------------------------------------------------------------
C  Called : SOIL
C  Calls  : Fert_Place, IPSOIL, NCHECK, NFLUX, RPLACE,
C           SOILNI, YR_DOY, FLOOD_CHEM, OXLAYER
C=======================================================================

      SUBROUTINE SoilNi (CONTROL, ISWITCH, 
     &    DRN, ES, FERTDATA, FLOODWAT, IMM, LITC, MNR,    !Input
     &    SOILPROP, SSOMC, ST, SW, TDFC, TDLNO, TILLVALS, !Input
     &    UNH4, UNO3, UPFLOW, WEATHER, XHLAI,             !Input
     &    FLOODN,                                         !I/O
     &    NH4, NO3, UPPM)                                 !Output

!-----------------------------------------------------------------------
      USE ModuleDefs 
      USE ModuleData
      USE FloodModule
      USE ModSoilMix
      IMPLICIT  NONE
      SAVE
!-----------------------------------------------------------------------
      CHARACTER*1 ISWNIT

      LOGICAL IUON

      INTEGER DOY, DYNAMIC, INCDAT, IUYRDOY, IUOF, L
      INTEGER NLAYR
      INTEGER NSOURCE, YEAR, YRDOY   

      REAL AD, AK, ALGFIX, CW
      REAL NFAC, NITRIF, NNOM
      REAL SNH4_AVAIL, SNO3_AVAIL, SUMFERT
      REAL SWEF, TFDENIT, TFUREA
      REAL TMINERN, TIMMOBN, TLCH, TLCHD
      REAL TNH4, TNH4NO3, TNO3, TNOX, UHYDR, TNOXD
      REAL WFDENIT, WFSOM, WFUREA, XL, XMIN
      REAL WTNUP, TNITRIFY, TUREA
      
      REAL ADCOEF(NL), BD(NL), DENITRIF(NL), DLAYR(NL) 
      REAL DLTSNH4(NL), DLTSNO3(NL), DLTUREA(NL), DRN(NL), DUL(NL)
      REAL UPFLOW(NL)
      REAL KG2PPM(NL), LITC(0:NL), LL(NL) 
      REAL NH4(NL), NO3(NL), PH(NL), SAT(NL), SNH4(NL)
      REAL SNO3(NL), SSOMC(0:NL), ST(NL), SW(NL)
      REAL TFNITY(NL), UNH4(NL), UNO3(NL), UREA(NL), UPPM(NL)

      REAL IMM(0:NL,NELEM), MNR(0:NL,NELEM)

!     Variables added for flooded conditions analysis:
      TYPE (FloodWatType) FLOODWAT    
      TYPE (FloodNType)   FloodN        
      TYPE (OxLayerType)  OXLAYR   

      INTEGER NBUND, NSWITCH
      INTEGER FERTDAY
      INTEGER DLAG(NL), INCYD, LFD10  !REVISED-US
      REAL ALI, FLOOD, ES, TMAX, TMIN, SRAD, XHLAI
      REAL TKELVIN, TFACTOR, WFPL, WF2
      REAL PHFACT, T2, TLAG, ARNTRF   !, DNFRATE
      REAL CUMFNRO
      REAL BD1
      REAL TOTAML, TOTFLOODN

      REAL CNITRIFY           !Cumulative Nitrification 
      REAL CMINERN, CIMMOBN   !Cumul. mineralization, immobilization
      REAL CNETMINRN          !Cumul. net mineralization
      REAL CNUPTAKE

!     Added for tile drainage:
      REAL TDFC
      INTEGER TDLNO

!     Added for tillage
      INTEGER TILDATE
      REAL MIXPCT, TDEP

!     *** TEMP DEBUGGIN CHP
      REAL TNOM
      REAL NNOM_a, NNOM_b

!-----------------------------------------------------------------------
!     Constructed variables are defined in ModuleDefs.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (SoilType)    SOILPROP
      TYPE (FertType)    FERTDATA
      TYPE (TillType)    TILLVALS
      TYPE (WeatherType) WEATHER

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY

      ADCOEF = SOILPROP % ADCOEF 
      BD     = SOILPROP % BD     
      DLAYR  = SOILPROP % DLAYR  
      DUL    = SOILPROP % DUL    
      KG2PPM = SOILPROP % KG2PPM  
      LL     = SOILPROP % LL     
      NLAYR  = SOILPROP % NLAYR  
      PH     = SOILPROP % PH     
      SAT    = SOILPROP % SAT    

      NSWITCH = ISWITCH % NSWI
      ISWNIT  = ISWITCH % ISWNIT

      NBUND = FLOODWAT % NBUND
      FLOOD = FLOODWAT % FLOOD

      FERTDAY = FERTDATA % FERTDAY

      SRAD = WEATHER % SRAD
      TMAX = WEATHER % TMAX
      TMIN = WEATHER % TMIN

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!     ------------------------------------------------------------------
!       Today's values
        TMINERN  = 0.0  !mineralization
        TIMMOBN  = 0.0  !immobilization
        TNITRIFY = 0.0  !nitrification

        !*** temp debugging chp
        TNOM = 0.0

!       Seasonal cumulative vaules
        CMINERN  = 0.0  !mineralization
        CIMMOBN  = 0.0  !immobilization
        CNETMINRN= 0.0  !net mineralization
        CNITRIFY = 0.0  !nitrification
        CNUPTAKE = 0.0  !cumulative N uptake
        TNOX   = 0.0    !denitrification
        TLCH   = 0.0    !leaching
        WTNUP  = 0.0    !N uptake

        TFNITY = 0.0    !
        IUOF   = 0
        IUON   = .FALSE.

        DO L = 1, NLAYR
          DLTSNO3(L)    = 0.0     
          DLTSNH4(L)    = 0.0      
          DLTUREA(L)    = 0.0
          DLAG(L)       = 0   !REVISED-US
          !Initialize uptake variables here, or they will have residual
          !  value on first day of multi-season runs.
          UNH4(L)       = 0.0
          UNO3(L)       = 0.0
        ENDDO

!        IF (INDEX('N',ISWNIT) > 0) RETURN

!         Set initial SOM and nitrogen conditions for each soil layer.
          CALL SoilNi_init(CONTROL, ISWNIT,
     &      SOILPROP, ST,                                   !Input
     &      NH4, NO3, SNH4, SNO3, TFNITY, UPPM, UREA)       !Output

          CALL NCHECK_inorg(CONTROL, 
     &      NLAYR, NH4, NO3, SNH4, SNO3, UREA)              !Input

        SWEF = 0.9-0.00038*(DLAYR(1)-30.)**2

!       Initialize flooded N if flooding is a possibility.
        CALL FLOOD_CHEM(CONTROL, ISWITCH, 
     &    FLOODWat, LFD10, SOILPROP, SNO3, SNH4, UREA,    !Input
     &    SRAD, SW, TMAX, TMIN, XHLAI,                    !Input
     &    DLTSNH4, DLTSNO3, DLTUREA, FLOODN, OXLAYR,      !I/O
     &    ALGFIX, BD1, CUMFNRO, TOTAML, TOTFLOODN)        !Output

!       Initialize TOTAML
        CALL OXLAYER(CONTROL,
     &    BD1, ES, FERTDATA, FLOODWAT, LFD10,             !Input
     &    NSWITCH, SNH4, SNO3, SOILPROP, SRAD, ST,        !Input
     &    SW, TMAX, TMIN, UREA, XHLAI,                    !Input
     &    DLTSNH4, DLTSNO3, DLTUREA, OXLAYR,              !I/O
     &    ALI, TOTAML)                                    !Output

        LFD10 = CONTROL % YRSIM

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!     ------------------------------------------------------------------
      IF (INDEX('N',ISWNIT) > 0) RETURN

!     Initialize Soil N process rates for this time step.
      DLTUREA = 0.0
      DLTSNO3 = 0.0 
      DLTSNH4 = 0.0 

      DO L = 1, NLAYR
!       Update with yesterday's plant N uptake
        SNO3(L) = SNO3(L) - UNO3(L)
        SNH4(L) = SNH4(L) - UNH4(L)
        NO3(L)  = SNO3(L) * KG2PPM(L)
        NH4(L)  = SNH4(L) * KG2PPM(L)
      ENDDO

      IF (FERTDAY == YRDOY) THEN
!       Fertilizer was placed today
        SUMFERT = 0.0
        DO L = 1, NLAYR
          DLTSNO3(L) = DLTSNO3(L) + FERTDATA % ADDSNO3(L)
          DLTSNH4(L) = DLTSNH4(L) + FERTDATA % ADDSNH4(L)
          DLTUREA(L) = DLTUREA(L) + FERTDATA % ADDUREA(L)
          IF (FERTDATA % ADDUREA(L) > 1.E-4) THEN
            IUON = .TRUE.
            IUYRDOY = INCDAT(YRDOY, 21)
            CALL YR_DOY (IUYRDOY, YEAR, IUOF)
          ENDIF
          SUMFERT = SUMFERT + FERTDATA % ADDSNO3(L) + 
     &                   FERTDATA % ADDSNH4(L) + FERTDATA % ADDUREA(L)
        ENDDO

!       Fertilizer added directly to flooded field
        IF (FLOOD > 1.E-4) THEN
          FLOODN % DLTFNH4  = FLOODN % DLTFNH4  + FERTDATA % ADDFNH4
          FLOODN % DLTFNO3  = FLOODN % DLTFNO3  + FERTDATA % ADDFNO3
          FLOODN % DLTFUREA = FLOODN % DLTFUREA + FERTDATA % ADDFUREA
          IF (FERTDATA % ADDFUREA > 1.E-4) THEN 
            IUON = .TRUE.
            IUYRDOY = INCDAT(YRDOY, 21)
            CALL YR_DOY (IUYRDOY, YEAR, IUOF)
          ENDIF
        ENDIF

        OXLAYR % DailyCalc = .FALSE.
        LFD10 = INCYD(YRDOY, 10)  !Use YRDOY format, increment 10 days
      ENDIF

!     ------------------------------------------------------------------
!     Check if tillage occurred today -- if so, mix soil N within
!       tillage depth
      IF (TILLVALS % NTIL .GT. 0) THEN
        TILDATE = TILLVALS % TILDATE
        IF (YRDOY .EQ. TILDATE) THEN
          !Call mixing routine for soil properties
          MIXPCT = TILLVALS % TILMIX
          TDEP = TILLVALS % TILDEP
          CALL SoilMix (SNO3, DLTSNO3, 1, DLAYR, MIXPCT, NLAYR, TDEP)
          CALL SoilMix (SNH4, DLTSNH4, 1, DLAYR, MIXPCT, NLAYR, TDEP)
          CALL SoilMix (UREA, DLTUREA, 1, DLAYR, MIXPCT, NLAYR, TDEP)
        ENDIF
      ENDIF

!     ------------------------------------------------------------------
!     Calculate rates of change of flood N components and interaction
!         with top soil layer.
      IF (NBUND > 0) THEN
        CALL FLOOD_CHEM(CONTROL, ISWITCH, 
     &    FLOODWat, LFD10, SOILPROP, SNO3, SNH4, UREA,    !Input
     &    SRAD, SW, TMAX, TMIN, XHLAI,                    !Input
     &    DLTSNH4, DLTSNO3, DLTUREA, FLOODN, OXLAYR,      !I/O
     &    ALGFIX, BD1, CUMFNRO, TOTAML, TOTFLOODN)        !Output
      ENDIF
      
!     This needs to be called from NTRANS for upland cases, too.
      IF (FLOOD .LE. 0.0) THEN
        CALL OXLAYER(CONTROL,
     &    BD1, ES, FERTDATA, FLOODWAT, LFD10,             !Input
     &    NSWITCH, SNH4, SNO3, SOILPROP, SRAD, ST,        !Input
     &    SW, TMAX, TMIN, UREA, XHLAI,                    !Input
     &    DLTSNH4, DLTSNO3, DLTUREA, OXLAYR,              !I/O
     &    ALI, TOTAML)                                    !Output
      ENDIF

!     ----------------------------------------------------------------
!     If DOY=IUOF (has been set in Fert_Place), then all the urea has
!     hydrolyzed already.
      CALL YR_DOY (CONTROL%YRDOY, YEAR, DOY)
      IF (DOY .EQ. IUOF) THEN
        DO L = 1, NLAYR
          DLTSNH4(L) = DLTSNH4(L) + UREA(L)
          DLTUREA(L) = DLTUREA(L) - UREA(L)
        END DO
        IF (FLOOD > 1.E-4) THEN
          FLOODN % DLTFNH4  = FLOODN % DLTFNH4  + FLOODN % FLDU
          FLOODN % DLTFUREA = FLOODN % DLTFUREA - FLOODN % FLDU
        ENDIF
        IUON = .FALSE.
      ENDIF

!     ------------------------------------------------------------------
!     Loop through soil layers for rate calculations
!     ------------------------------------------------------------------
      NNOM = 0.0
      TMINERN  = 0.0
      TIMMOBN  = 0.0
      TNITRIFY = 0.0
      TNOXD = 0.0

      DO L = 1, NLAYR
!       ----------------------------------------------------------------
!       Environmental limitation factors for the soil processes.
!       ----------------------------------------------------------------
        IF (SW(L) .LE. DUL(L)) THEN
          IF (L .EQ. 1) THEN
!           For the topsoil layer, the lowest possible volumetric water
!           content (air-dry water content) may be lower than the lower
!           limit, because this layer may dry out due to water
!           evaporation at the soil surface.
            AD = LL(L) * SWEF
          ELSE
!           Set the air dry water content equal to the lower limit.
            AD  = LL(L)
          ENDIF

!         Soil water factor WFSOM.
          WFSOM = (SW(L) - AD) / (DUL(L) - AD)
        ELSE
!         If the soil water content is higher than the drained upper 
!         limit (field capacity), calculate the excess water as fraction
!         of the maximum excess (i.e. when saturated).
          XL = (SW(L) - DUL(L)) / (SAT(L) - DUL(L))

!         Soil water factor WFSOM.
          WFSOM = 1.0 - 0.5 * XL
        ENDIF   !End of IF block on SW vs. DUL.

!       PH factor (from RICE model)
        IF (FLOOD .GT. 0.0) THEN
           WFSOM    = 0.75
        ENDIF

!       Limit the soil water factors between 0 and 1.
        WFSOM = AMAX1 (AMIN1 (WFSOM, 1.), 0.)

!       Calculate the soil temperature factor for the urea hydrolysis.
        TFUREA = (ST(L) / 40.) + 0.20
        TFUREA = AMAX1 (AMIN1 (TFUREA, 1.), 0.)

!-----------------------------------------------------------------------
!       UREA hydrolysis
!-----------------------------------------------------------------------
        IF (IUON) THEN
!         Calculate the maximum hydrolysis rate of urea.
!         AK = -1.12 + 1.31 * OC(L) + 0.203 * PH(L) - 0.155 * OC(L) * PH(L)
          AK = -1.12 + 1.31 * SSOMC(L) * 1.E-4 * KG2PPM(L) + 0.203*PH(L)
     &              - 0.155 * SSOMC(L) * 1.E-4 * KG2PPM(L) * PH(L)
          AK = AMAX1 (AK, 0.25)

!         Calculate the soil water factor for the urea hydrolysis, and
!         limit it between 0 and 1.
          IF (FLOOD .GT. 0.0) THEN
             WFUREA = 1.0
          ELSE
            WFUREA = WFSOM + 0.20
            WFUREA = AMAX1 (AMIN1 (WFUREA, 1.), 0.)
          ENDIF

!         Calculate the amount of urea that hydrolyses.
          UHYDR = AK * AMIN1 (WFUREA, TFUREA) * (UREA(L) + DLTUREA(L))
          UHYDR = AMIN1 (UHYDR, UREA(L)+DLTUREA(L))

          DLTUREA(L) = DLTUREA(L) - UHYDR 
          DLTSNH4(L) = DLTSNH4(L) + UHYDR 
        ENDIF   !End of IF block on IUON.

!-----------------------------------------------------------------------
!       Net mineralization rate - Nitrogen.
!-----------------------------------------------------------------------
        IF (L == 1) THEN
!         First layer takes mineralization from surface also.
!         NNOM = MINERALIZE(0,N) + MINERALIZE(1,N)
          NNOM = MNR(0,N) + MNR(1,N) - IMM(0,N) - IMM(1,N)
        ELSE
!         Add in residual from previous layer to preserve N balance
!         NNOM = NNOM + MINERALIZE(L,N)
          NNOM = NNOM + MNR(L,N) - IMM(L,N)
        ENDIF

        !*** temp debugging chp
        TNOM = TNOM + NNOM

!       Mineralization
!       --------------
!       If the net N release from all SOM sources (NNOM) is positive,
!       add the mineralized N to the NH4 pool.
        IF (NNOM .GE. 0.0) THEN
          DLTSNH4(L) = DLTSNH4(L) + NNOM
          !TMINERALIZE(N) = TMINERALIZE(N) + NNOM
          NNOM = 0.

        ELSE
!       Immobilization
!       --------------
!         If NNOM is < 0, there is immobilization. If the N demand
!         of the immobilization is greater than the amount of NH4
!         available, take all NH4, leaving behind a minimum amount of
!         NH4 equal to XMIN (NNOM is negative!).

          IF (ABS(NNOM) .GT. (SNH4(L) - XMIN)) THEN
            NNOM_a = -(SNH4(L) - XMIN + DLTSNH4(L))
            NNOM_b = NNOM - NNOM_a

            DLTSNH4(L) = -(SNH4(L) - XMIN)
            !TIMMOBILIZE(N) = TIMMOBILIZE(N) + (SNH4(L) - XMIN)
            NNOM = NNOM_b

            SNO3_AVAIL = SNO3(L) + DLTSNO3(L)
            IF (ABS(NNOM) .GT. (SNO3_AVAIL - XMIN)) THEN
              !Not enough SNO3 to fill remaining NNOM, leave residual
              DLTSNO3(L) = DLTSNO3(L) + XMIN - SNO3_AVAIL
              !TIMMOBILIZE(N) = TIMMOBILIZE(N) + (SNO3_AVAIL - XMIN)
              NNOM = NNOM + SNO3_AVAIL - XMIN
            ELSE
!             Get the remainder of the immobilization from nitrate (NNOM
!             is negative!)
              DLTSNO3(L) = DLTSNO3(L) + NNOM
              !TIMMOBILIZE(N) = TIMMOBILIZE(N) - NNOM
              NNOM = 0.
            ENDIF

          ELSE
!           Reduce soil NH4 by the immobilization (NNOM is
!           negative!).
            DLTSNH4(L) = DLTSNH4(L) + NNOM
            !TIMMOBILIZE(N) = TIMMOBILIZE(N) - NNOM
            NNOM = 0.0
          ENDIF   !End of IF block on ABS(NNOM).
        ENDIF   !End of IF block on NNOM.

!-----------------------------------------------------------------------
!       Nitrification section
!-----------------------------------------------------------------------
!       Nitrification based on Gilmour
        TKELVIN = ST(L) + 273.0
        TFACTOR = EXP(-6572 / TKELVIN + 21.4)  !6602
!        WFPL    = WFP(L)    !not used
        WFPL = SW(L) / SAT(L)
        IF (SW(L) .GT. DUL(L)) THEN
          WF2 = -2.5 * WFPL + 2.55
        ELSE
          IF (WFPL .GT. 0.4) THEN
            WF2 = 1.0
          ELSE
            WF2 = 3.15 * WFPL - 0.1
          ENDIF
        ENDIF
        WF2 = AMAX1 (WF2, 0.0)
        WF2 = AMIN1 (1.0, WF2)

        PHFACT  = AMIN1 (1.0, 0.33 * PH(L) - 1.36)
!        NH4(L)  = SNH4(L)*KG2PPM(L)

        IF (FLOOD .GT. 0.0) THEN
          PHFACT = 1.0
          WF2    = 0.0     
          TFNITY(L) = 0.0
        ENDIF

        T2   = AMAX1 (0.0,(TFNITY(L) - 1.0))
        TLAG = 0.075 * T2**2
        TLAG = AMIN1 (TLAG,1.0)
        IF (NBUND <= 0.) THEN
          TLAG = 1.0
        ENDIF
        NFAC = AMAX1(0.0, AMIN1(1.0, TFACTOR * WF2 * PHFACT * TLAG))
        NITRIF = NFAC * NH4(L)

        IF (NSWITCH .EQ. 5) THEN
          ARNTRF = 0.0
        ELSE
!         chp (via Peter Grace) 8/17/2013
!         ARNTRF  = NITRIF * KG2PPM(L)
          ARNTRF  = NITRIF / KG2PPM(L)
        ENDIF

        IF (NH4(L).LE. 0.01) THEN
          TFNITY(L) = 0.0
        ELSE
          IF (SW(L) .LT. SAT(L)) THEN
            TFNITY(L) = TFNITY(L) + 1.0
          ENDIF
        ENDIF

        XMIN = 0.0
        SNH4_AVAIL = AMAX1(0.0, SNH4(L) + DLTSNH4(L) - XMIN)
        ARNTRF = AMIN1(ARNTRF, SNH4_AVAIL)

        DLTSNO3(L) = DLTSNO3(L) + ARNTRF
        DLTSNH4(L) = DLTSNH4(L) - ARNTRF
        TNITRIFY   = TNITRIFY   + ARNTRF

!-----------------------------------------------------------------------
!       Denitrification section
!-----------------------------------------------------------------------
!       Denitrification only occurs if there is nitrate, SW > DUL and
!       soil temperature > 5.
        IF (NO3(L) .GT. 0.01 .AND. SW(L) .GT. DUL(L) .AND.
     &       ST(L) .GE. 5.0) THEN

!         Water extractable soil carbon: estimated according to
!         Rolston et al. 1980, as cited in Godwin & Jones 1991 (ASA
!         monograph #31). Calculate carbohydrate carbon as 40% of the
!         carbohydrate pool.
C-UPS     Corrected per e-mail 03/29/00
!         CW = 24.5 + 0.0031 * (SSOMC(L) + 0.4 * FPOOL(L,1)) * KG2PPM(L)

!     ----------------------------------------------------------------
!11/18/2003 UPS: THE NEW NTRANS SHOULD READ: 
!         CW = 24.5 + {0.0031 * SSOMC(L) + 0.4 * FPOOL(L,1)} * KG2PPM(L) 
!            = 24.5 + AVAILABLE CARBON FROM HUMIC FRACTION + FRESH C from  CARBOHYDRATE POOL 

!NOTES: 1. ONLY THE HUMIC C IS MULTIPLIED BY 0.0031 
!       2. SOILC IN GODWIN&JONES INCLUDED BOTH HUMIC C AND FRESH (LITTER POOL C) 
!       3. WE ARE USING ONLY THE CARBOHYDRATE POOL (*0.4 TO C) = ALL AVAILABLE 
!       4. FPOOL is still kg of Organic matter/ha (and not kg C/ha)?? 

!     SO WE NEED TO FIX BOTH DSSAT4 AND GODWIN AND SINGH EQN TO THE ABOVE. 

!          CW = 24.5 + (0.0031 * SSOMC(L) + 0.4 * FPOOL(L,1)) * KG2PPM(L) 

!     The above removed on 1/14/2004 as per email from AJG and UPS

!     ----------------------------------------------------------------
!     DENITRIFICATION - CORRECTIONS - 13 Jan 2004 (US)
!         From NTRANS:
!          CW = 24.5 + 0.0031 * (HUMC(L) + 0.4 * FPOOL(L,1)) * KG2PPM(L)
!     ----------------------------------------------------------------

!        From Century:
         !CHP changed 1/14/2004 per email from UPS / AJG
          CW = 24.5 + 0.0031 * (SSOMC(L) + 0.2 * LITC(L)) * KG2PPM(L)
!     ----------------------------------------------------------------
!
!     The DENITRIF or DNRATE calculations are identical in NTRANS 
!             (DSSAT4) and Godwin and Singh:
!
!     DENITRIF = {6.0 * 1.E-04 * CW * NO3(L) * WFDENIT * TFDENIT * DLAG }/ KG2PPM(L)
!     -- AS in NTRANS (DSSAT4)
!
!         = {in concentration unit}/KG2PPM
!         = kg N (denitrified)
!         = {in conc unit }/(10/BD*DLAYR)
!
!     DENITRIF = 6.0*1.E-05 * CW * NO3(L) * WFDENIT * TFDENIT * DLAG*BD(L)*DLAYR(L)
!     -- AS in GODWIN & SINGH
!
!     NOTE: CW is in concentration unit!!  Extractable C concentration.
!
!     CW = (SOILC*KG2PPM(L))*0.0031 + 24.5
!
!       where SOILC = SSOMC(L) + 0.4 * FOM(L) in kg/ha - Origianl definition
!        Later corrected to:
!        SOILC = SSOMC(L) + 0.4 * FPOOL(L,1)  -- because only carbohydrate 
!             pool from the FOM is assumed to provide labile/extractable C.
! 
!     CW   = ({SSOMC(L) + 0.4 * FPOOL(L,1)} * KG2PPM(L))*0.0031 + 24.5
!
!     The equations in Godwin and Jones as well as Godwin and Singh are incorrect!!

!     ----------------------------------------------------------------
!         Temperature factor for denitrification.
          TFDENIT = 0.1 * EXP (0.046 * ST(L))
          TFDENIT = AMAX1 (AMIN1 (TFDENIT, 1.), 0.)

!         Water factor for denitrification: only if SW > DUL.
          WFDENIT = 1. - (SAT(L) - SW(L)) / (SAT(L) - DUL(L))
          WFDENIT = AMAX1 (AMIN1 (WFDENIT, 1.), 0.)

          IF (WFDENIT .GT. 0.0) THEN
            DLAG(L) = DLAG(L) + 1
          ELSE
            DLAG(L) = 0
          ENDIF

          IF (DLAG(L) .LT. 5) THEN
            WFDENIT = 0.0
          ENDIF

!         Denitrification rate
C-UPS     Corrected per e-mail 03/29/00
!         DLAG REMOVED REVISED-US 4/20/2004
          DENITRIF(L) = 6.0 * 1.E-04 * CW * NO3(L) * WFDENIT * 
     &                 TFDENIT / KG2PPM(L)       
          DENITRIF(L) = AMAX1 (DENITRIF(L), 0.0)

!         The minimum amount of NO3 that stays behind in the soil and 
!         cannot denitrify is XMIN.
!         XMIN    = 0.25 / KG2PPM(L)
          XMIN    = 0.       !AJG

!         Check that no more NO3 denitrifies than there is, taking
!         into account what has already been removed by other
!         processes (thus use only negative DLTSNO3 values). This is a
!         protection against negative values at the integration step.
          SNO3_AVAIL = SNO3(L) + AMIN1 (DLTSNO3(L), 0.) - XMIN

!         Take the minimum of the calculated denitrification and the
!         amount of NO3 available for denitrification. 
          DENITRIF(L)  = AMIN1 (DENITRIF(L), SNO3_AVAIL)

C         If flooded, lose all nitrate --------REVISED-US
!          IF (FLOOD .GT. 0.0) THEN
!            !DNFRATE = SNO3(L) - 0.5/KG2PPM(L)        !XMIN?, SNO3_AVAIL?
!            DNFRATE = SNO3_AVAIL - 0.5/KG2PPM(L)        !XMIN?, SNO3_AVAIL?
!          ELSE
!            DNFRATE = 0.0
!          ENDIF

!         chp/us 4/21/2006
          IF (FLOOD .GT. 0.0 .AND. WFDENIT > 0.0) THEN
!            DENITRIF(L) = SNO3_AVAIL
!           chp 9/6/2011 remove 50% NO3/d = 97% removed in 5 days
!           previously removed 100% NO3/d
            DENITRIF(L) = SNO3_AVAIL * 0.5
          ENDIF

!chp 4/20/2004   DENITRIF = AMAX1 (DENITRIF, DNFRATE)
          DENITRIF(L) = AMAX1 (DENITRIF(L), 0.0)
          IF (NSWITCH .EQ. 6) THEN
            DENITRIF(L) = 0.0
          ENDIF

!         Reduce soil NO3 by the amount denitrified and add this to
!         the NOx pool
          DLTSNO3(L) = DLTSNO3(L) - DENITRIF(L)
          TNOX       = TNOX       + DENITRIF(L)
          TNOXD      = TNOXD      + DENITRIF(L)

        ELSE
!         IF SW, ST OR NO3 FALL BELOW CRITICAL IN ANY LAYER RESET LAG EFFECT.
          DLAG(L) = 0      !REVISED-US
        ENDIF   !End of IF block on denitrification.

      END DO   !End of soil layer loop.

!     ------------------------------------------------------------------
!     Downward and upward N movement with the water flow.
!     ------------------------------------------------------------------
      TLCHD = 0.0

      IF (IUON) THEN
        NSOURCE = 1    !Urea.
        CALL NFLUX ( 
     &    ADCOEF, BD, DLAYR, DRN, DUL, UPFLOW, NLAYR,     !Input
     &    UREA, NSOURCE, SW, TDFC, TDLNO,                 !Input
     &    DLTUREA, TLCH, TLCHD)                           !Output
      ENDIF

      NSOURCE = 2   !NO3.
      CALL NFLUX ( 
     &  ADCOEF, BD, DLAYR, DRN, DUL, UPFLOW, NLAYR,       !Input
     &  SNO3, NSOURCE, SW, TDFC, TDLNO,                   !Input
     &  DLTSNO3, TLCH, TLCHD)                             !Output

      CALL PUT('NITR','TNOXD',ARNTRF)
      CALL PUT('NITR','TLCHD',TLCHD)

!***********************************************************************
!***********************************************************************
!     END OF FIRST DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF

!***********************************************************************
!***********************************************************************
!     DAILY INTEGRATION (also performed for seasonal initialization)
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT .OR. DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
      IF (INDEX('N',ISWNIT) > 0) RETURN

      IF (DYNAMIC .EQ. INTEGR) THEN
!       Update flood N components.
        IF (NBUND > 0) THEN
          CALL FLOOD_CHEM(CONTROL, ISWITCH, 
     &    FLOODWat, LFD10, SOILPROP, SNO3, SNH4, UREA,    !Input
     &    SRAD, SW, TMAX, TMIN, XHLAI,                    !Input
     &    DLTSNH4, DLTSNO3, DLTUREA, FLOODN, OXLAYR,      !I/O
     &    ALGFIX, BD1, CUMFNRO, TOTAML, TOTFLOODN)        !Output
        ENDIF
      
        !Update oxidation layer variables
        CALL OXLAYER(CONTROL,
     &    BD1, ES, FERTDATA, FLOODWAT, LFD10,              !Input
     &    NSWITCH, SNH4, SNO3, SOILPROP, SRAD, ST,        !Input
     &    SW, TMAX, TMIN, UREA, XHLAI,                    !Input
     &    DLTSNH4, DLTSNO3, DLTUREA, OXLAYR,              !I/O
     &    ALI, TOTAML)                                    !Output
      ENDIF

!     Loop through soil layers for integration
      DO L = 1, NLAYR
        SNO3(L) = SNO3(L) + DLTSNO3(L)    !plant uptake added to DLTSNO3
        SNH4(L) = SNH4(L) + DLTSNH4(L)    !plant uptake added to DLTSNH4
        UREA(L) = UREA(L) + DLTUREA(L)

!       Underflow trapping
        IF (ABS(SNO3(L)) .LT. 1.E-8) SNO3(L) = 0.0
        IF (ABS(SNH4(L)) .LT. 1.E-8) SNH4(L) = 0.0
        IF (ABS(UREA(L)) .LT. 1.E-8) UREA(L) = 0.0

!       Conversions.
        NO3(L)  = SNO3(L) * KG2PPM(L)
        NH4(L)  = SNH4(L) * KG2PPM(L)
        UPPM(L) = UREA(L) * KG2PPM(L)
      ENDDO

!     Call NCHECK to check for and fix negative values.
      CALL NCHECK_inorg(CONTROL, 
     &    NLAYR, NH4, NO3, SNH4, SNO3, UREA)              !Input

!     Soil profile accumulations.
      TNH4   = 0.0
      TNO3   = 0.0
      TUREA  = 0.0
      DO L = 1, NLAYR
        TNH4  = TNH4  + SNH4(L)
        TNO3  = TNO3  + SNO3(L)
        TUREA = TUREA + UREA(L)
        WTNUP = WTNUP + (UNO3(L) + UNH4(L)) / 10.    !g[N]/m2 cumul.
        IF (L ==1) THEN
          TMINERN = MNR(0,N) + MNR(1,N)
          TIMMOBN = IMM(0,N) + IMM(1,N)
        ELSE
          TMINERN = TMINERN + MNR(L,N)
          TIMMOBN = TIMMOBN + IMM(L,N)
        ENDIF
      ENDDO

      TNH4NO3 = TNH4 + TNO3

!     Seasonal cumulative values
      CMINERN  = CMINERN  + TMINERN 
      CIMMOBN  = CIMMOBN  + TIMMOBN 
      CNETMINRN= CMINERN  - CIMMOBN
      CNITRIFY = CNITRIFY + TNITRIFY
      CNUPTAKE = WTNUP * 10.

      IF (DYNAMIC .EQ. SEASINIT) THEN
        CALL SoilNiBal (CONTROL, ISWITCH,
     &    ALGFIX, CIMMOBN, CMINERN, CUMFNRO, FERTDATA, NBUND, TLCH,  
     &    TNH4, TNO3, TNOX, TOTAML, TOTFLOODN, TUREA, WTNUP) 

        CALL OpSoilNi(CONTROL, ISWITCH, SoilProp, 
     &    CIMMOBN, CMINERN, CNETMINRN, CNITRIFY, CNUPTAKE, 
     &    FertData, NH4, NO3, 
     &    TLCH, TNH4, TNH4NO3, TNO3, TNOX, TOTAML)
      ENDIF

!***********************************************************************
!***********************************************************************
!     OUTPUT
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
      IF (INDEX('N',ISWNIT) > 0) RETURN

C     Write daily output
      CALL OpSoilNi(CONTROL, ISWITCH, SoilProp, 
     &    CIMMOBN, CMINERN, CNETMINRN, CNITRIFY, CNUPTAKE, 
     &    FertData, NH4, NO3, 
     &    TLCH, TNH4, TNH4NO3, TNO3, TNOX, TOTAML)

      IF (NBUND > 0) THEN
        CALL FLOOD_CHEM(CONTROL, ISWITCH, 
     &    FLOODWat, LFD10, SOILPROP, SNO3, SNH4, UREA,    !Input
     &    SRAD, SW, TMAX, TMIN, XHLAI,                    !Input
     &    DLTSNH4, DLTSNO3, DLTUREA, FLOODN, OXLAYR,      !I/O
     &    ALGFIX, BD1, CUMFNRO, TOTAML, TOTFLOODN)        !Output
      ENDIF

      CALL SoilNiBal (CONTROL, ISWITCH,
     &    ALGFIX, CIMMOBN, CMINERN, CUMFNRO, FERTDATA, NBUND, TLCH,  
     &    TNH4, TNO3, TNOX, TOTAML, TOTFLOODN, TUREA, WTNUP) 

C***********************************************************************
C***********************************************************************
C     END OF SECOND DYNAMIC IF CONSTRUCT
C***********************************************************************
      ENDIF
C-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE SoilNi

!=======================================================================
! NTRANS Variables - updated 08/20/2003
!-----------------------------------------------------------------------
! AD            Lowest possible volumetric water content of a soil layer 
!                 when it is air dry. For the top soil layer AD may be 
!                 lower than the lower limit LL, because this layer may dry 
!                 out due to water evaporation at the soil surface.
!                 (cm3 [H2O]/ cm3 [soil])
! ADCOEF(L)     Anion adsorption coefficient for soil layer L;  for reduced 
!                 anion (nitrate) flow in variable-charge soils (ADCOEF = 0 
!                 implies no anion retention) (cm3 (H2O] / g [soil])
! AK            Maximum hydrolysis rate of urea (i.e. proportion of urea 
!                 that will hydrolyze in 1 day under optimum conditions). 
!                 AK >= 0.25. Also: Today's value of the nitrification 
!                 potential, calculated from the previous day’s value (d-1)
! ALGFIX        N in algae (kg [N] / ha)
! ALI            
! ARNTRF        Daily nitrification rate (kg [N] / ha / d)
! BD(L)         Bulk density, soil layer L (g [soil] / cm3 [soil])
! BD1           Bulk density of oxidized layer (g [soil] / cm3 [soil])
! CONTROL       Composite variable containing variables related to control 
!                 and/or timing of simulation.  The structure of the 
!                 variable (ControlType) is defined in ModuleDefs.for. 
! CUMFNRO       Cumulative N lost in runoff over bund (kg [N] / ha)
! CW            Water extractable SOM carbon (µg [C] / g [soil])
! DENITRIF      Denitrification rate (kg [N] / ha / d)
! DLAG(L)       Number of days with soil water content greater than the 
!                 drained upper limit for soil layer L.  For 
!                 denitrification to occur, DLAG must be greater than 4 (4 days
!                 lag period before denitrification begins). 
! DLAYR(L)      Thickness of soil layer L (cm)
! DLTFON(L)     Rate of change of N in fresh organic residue  in soil layer 
!                 L (kg [N] / ha / d)
! DLTFPOOL(L,J) Rate of change of FOM pool (FPOOL) in soil layer L 
!                 (J=1=carbohydrate, 2=cellulose, 3=lignin)
!                 (kg [residue pool] / ha / d)
! DLTSNH4(L)    Rate of change of ammonium in soil layer L
!                (kg [N] / ha / d)
! DLTSNO3(L)    Rate of change of nitrate in soil layer L (kg [N] / ha / d)
! DLTUREA(L)    Rate of change of urea content in soil layer L
!                (kg [N] / ha / d)
! DMINR         Maximum decomposition rate constant of stable organic 
!                 matter (d-1)
! DNFRATE       Maximum denitrification rate (kg [N] / ha / d)
! DOY           Current day of simulation (d)
! DRN(L)        Drainage rate through soil layer L (cm/d)
! DSNC          Depth to which C and N are integrated across all soil 
!                 layers for output in CARBON.OUT (cm)
! DUL(L)        Volumetric soil water content at Drained Upper Limit in 
!                 soil layer L (cm3[water]/cm3[soil])
! ES            Actual soil evaporation rate (mm/d)
! FLOOD         Current depth of flooding (mm)
! FLOODN        Composite variable which contains flood nitrogen mass and 
!                 concentrations. Structure of variable is defined in 
!                 ModuleDefs.for. (var.)
! FLOODWAT      Composite variable containing information related to bund 
!                 management. Structure of variable is defined in 
!                 ModuleDefs.for. 
! UPFLOW(L)     Movement of water between unsaturated soil layers due to 
!                 soil evaporation: + = upward, -- = downward (cm/d)
! FOM(L)        Fresh organic residue in soil layer L
!                (kg [dry matter] / ha)
! FON(L)        Nitrogen in fresh organic matter in soil layer L
!                (kg [N] / ha)
! FPOOL(L,J)    FOM pool in soil layer L: J=1:carbohydrate, 2:cellulose, 
!                 3:lignin (kg [residue pool] / ha)
! SSOMC(L)       Carbon in stable organic matter (humus) (kg [C] / ha)
! HUMFRAC       Humus fraction that decomposes (fraction)
! ISWITCH       Composite variable containing switches which control flow 
!                 of execution for model.  The structure of the variable 
!                 (SwitchType) is defined in ModuleDefs.for. 
! IUOF          Critical Julian day when all urea is assumed to be 
!                 hydrolyzed (this is assumed to occur 21 days after the 
!                 urea application) (d)
! IUON          Flag indicating presence of urea (true or false) 
! KG2PPM(L)     Conversion factor to switch from kg [N] / ha to µg [N] / g 
!                 [soil] for soil layer L 
! LFD10         Date, 10 days after last fertilization.  Used to determine 
!                 whether hourly flood chemistry computations will be done 
!                 (see DAILY variable). (YYYYDDD)
! LL(L)         Volumetric soil water content in soil layer L at lower 
!                 limit (cm3 [water] / cm3 [soil])
! NBUND         Number of bund height records 
! NH4(L)        Ammonium N in soil layer L (µg[N] / g[soil])
! NITRIF        Nitrification rate (kg [N] / ha - d)
! NL            Maximum number of soil layers = 20 
! NLAYR         Actual number of soil layers 
! NNOM          Net mineral N release from all SOM sources (kg [N] / ha)
! NO3(L)        Nitrate in soil layer L (µg[N] / g[soil])
! NSOURCE       Flag for N source (1 = urea, 2 = NO3) 
! NSWITCH       Nitrogen switch - can be used to control N processes (0-No 
!                 N simulated, 1-N simulated, 5-Nitrification off, 
!                 6-denitrification off, 7-floodwater loss off, 8-drainage 
!                 losses off, 9-leaching off, 10-runoff losses off 
! OXLAYR        Composite variable which contains data about oxidation 
!                 layer.  See ModuleDefs.for for structure. 
! PH(L)         pH in soil layer L 
! PHFACT        Effect of pH on various soil N processes; value varies with 
!                 routine 
! SAT(L)        Volumetric soil water content in layer L at saturation
!                (cm3 [water] / cm3 [soil])
! SNH4(L)       Total extractable ammonium N in soil layer L (kg [N] / ha)
! SNH4_AVAIL    Maximum amount of NH4 that may nitrify (kg [N] / (ha - d))
! SNO3(L)       Total extractable nitrate N in soil layer L (kg [N] / ha)
! SNO3_AVAIL    NO3 available for denitrification (kg [N] / ha)
! SOILPROP      Composite variable containing soil properties including 
!                 bulk density, drained upper limit, lower limit, pH, 
!                 saturation water content.  Structure defined in ModuleDefs. 
! SRAD          Solar radiation (MJ/m2-d)
! ST(L)         Soil temperature in soil layer L (°C)
! SW(L)         Volumetric soil water content in layer L
!                (cm3 [water] / cm3 [soil])
! SWEF          Soil water evaporation fraction; fraction of lower limit 
!                 content to which evaporation can reduce soil water 
!                 content in top layer (fraction)
! T2            Temperature factor for nitrification 
! TFACTOR       Temperature factor for nitrification 
! TFDENIT       Temperature factor for denitrification rate (range 0-1) 
! TFNITY(L)     Yesterday’s soil temperature factor for nitrification 
!                 (range 0-1) 
! TFUREA        Soil temperature factor for urea hydrolysis (range 0-1) 
! TIMMOBILIZE   Cumulative N immoblized (kg [N] / ha)
! TKELVIN       Soil temperature (oK)
! TLAG          Temperature factor for nitrification (0-1) 
! TLCH          Total N leached from soil (kg [N] / ha)
! TMAX          Maximum daily temperature (°C)
! TMIN          Minimum daily temperature (°C)
! TMINERALIZE   Cumulative mineralization (kg [N] / ha)
! TNH4          Total extractable ammonium N in soil profile (kg [N] / ha)
! TNH4NO3       Total amount of inorganic N (NH4 and NO3) across soil 
!                 profile (kg [N] / ha)
! CNITRIFY      Cumulative nitrification (kg [N] / ha)
! TNO3          Total extractable nitrate N in soil profile (kg [N] / ha)
! TNOX          Denitrification across the total soil profile  adding to 
!                 the nitrous oxide (NOx) pool of the air (kg [N] / ha)
! TOTAML        Cumulative ammonia volatilization (kg [N] / ha)
! TOTFLOODN     Current N in flood water (kg [N] / ha)
! TUREA         Total urea in soil profile (kg [N] / ha)
! UHYDR         Rate of urea hydrolysis (kg [N] / ha - d)
! UNH4(L)       Rate of root uptake of NH4, computed in NUPTAK
!                (kg [N] / ha - d)
! UNO3(L)       Rate of root uptake of NO3, computed in NUPTAK
!                (kg [N] / ha -d)
! UREA(L)       Amount of urea in soil layer L (kg [N] / ha)
! WF2           Water factor for nitrification 
! WFDENIT       Soil water factor for denitrification rate (range 0-1) 
! WFPL          Water factor for nitrification 
! WFSOM         Reduction factor for FOM decay based on soil water content 
!                 (no reduction for SW = DUL, 100% reduction for SW = LL, 
!                 50% reduction for SW = SAT) 
! WFUREA        Reduction of urea hydrolysis due to soil water content 
!                 (range = 0-1; no reduction for SW = DUL, 20% reduction 
!                 for SW = LL, 70% reduction for SW = SAT (fraction)
! WTNUP         Cumulative N uptake (g[N] / m2)
! XHLAI         Healthy leaf area index (m2[leaf] / m2[ground])
! XL            Excess water (above DUL) as a fraction of the maximum 
!                 amount of excess water (i.e. saturated). (fraction)
! XMIN          Amount of NH4 that cannot be immobilized but stays behind 
!                 in soil as NH4; Also, Amount of NO3 that cannot denitrify 
!                 but stays behind in the soil as NO3 (kg [N] / ha)
! YEAR          Year of current date of simulation 
!***********************************************************************
