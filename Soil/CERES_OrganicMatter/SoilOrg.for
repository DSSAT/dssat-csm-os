!=======================================================================
! COPYRIGHT 1998-2010 The University of Georgia, Griffin, Georgia
!                      University of Florida, Gainesville, Florida
!                      Iowa State University, Ames, Iowa
!                      International Center for Soil Fertility and 
!                       Agricultural Development, Muscle Shoals, Alabama
!                      University of Guelph, Guelph, Ontario
!  ALL RIGHTS RESERVED
!=======================================================================
!=======================================================================
!  SOILORG, Subroutine
!
!  Determines organic soil transformations.  Based on CERES NTRANS 
!     subroutine.  Organic and inorganic transformations were separated
!     into SoilOrg and SOILN_inorg subroutines.
!-----------------------------------------------------------------------
!  Revision history
!  . . . . . .  Written
!  08-08-1992 WTB Modified for version 3 input/output (matches LeGRO).
!  02-08-1993 PWW Header revision and minor changes. 
!  06/09/1999 AJG Completely revised the soil N and SOM module, and made
!               a new SOM module based on the CENTURY model.
!               Also changed variable names:
!               OLD      NEW                OLD      NEW
!               ------   ------             ------   ------ 
!               G1 * X   FOMFRAC            TF       TFSOM     
!               GRCOM    FOMFRAC * FOM      TIFOM    TFOM
!               GRNOM    FOMFRAC * FON      TIFON    TFON
!               HUM      HUMC               TOTIN    SNH4NO3(array!)
!               MF       WFSOM              TSOC     THUMC
!               NHUM     HUMN               TSON     THUMN
!               RHMIN    HUMFRAC * HUMN     X        FOMFRAC         
!               RNAC     IMMOB      
!  06/21/1999 CHP Modular format.
!  01/01/2000 CHP/AJG Integrated the CENTURY-based and CERES-based SOM
!               modules with CHP's modular structure.
!  03/17/2000 GH  Incorporated in CROPGRO 
!  03/12/2003 CHP Changed senescence variable to composite (SENESCE)
!                   as defined in ModuleDefs.for
!  01/24/2005 CHP/US Separated NTRANS into SoilOrg (this routine) and
!               SOILN_inorg
!  03/01/2005 CHP Changed variable names to match Century:
!                 HUMC to SSOMC     HUMN to SSOME
!                 THUMC to TSOMC    THUMN to TSOME
!                 TFON to TLITE     TFOM to TLITC
!  04/21/2005 CHP Added IMMOBP, IMMOBN (was IMMOB)
!  12/15/2005 CHP Modified to allow organic matter computation even 
!                   when N is not simulated.
!  02/11/2010 CHP No simulation of organic matter when water is not 
!                   simulated.
!  01/26/2023 CHP Reduce compile warnings: add EXTERNAL stmts, remove 
!                 unused variables, shorten lines. 
!-----------------------------------------------------------------------
!  Called : SOIL
!  Calls  : IPSOIL, MULCHLAYER, NCHECK_organic, OpSoilOrg, SoilOrg_init
!=======================================================================

      SUBROUTINE SoilOrg (CONTROL, ISWITCH, 
     &    DRAIN, FERTDATA, FLOODWAT, FLOODN, HARVRES,     !Input
     &    NH4, NO3, OMAData, RLV,                         !Input
     &    SENESCE, SOILPROP, SPi_Labile, ST, SW, TILLVALS,!Input
     &    CH4_data, IMM, LITC, MNR, MULCH, newCO2, SomLit,!Output
     &    SomLitC, SomLitE, SSOMC)                        !Output

!-----------------------------------------------------------------------
      USE ModuleDefs 
      USE ModuleData
      USE Interface_IpSoil
      USE FloodModule
      USE ModSoilMix
      USE GHG_MOD

      IMPLICIT  NONE
      EXTERNAL MethaneDynamics, SoilOrg_init, 
     &  NCHECK_organic, MULCHLAYER, SOILCBAL, OpSoilOrg, SoilNoPoBal
      SAVE
!-----------------------------------------------------------------------
      CHARACTER*1 ISWWAT
      CHARACTER(LEN=2) PREV_CROP

      INTEGER DYNAMIC, IEL, L, N_ELEMS
      INTEGER NLAYR, YRDOY, INDEX
      INTEGER, PARAMETER :: SRFC=0

      REAL AD, ADD, AM, CNR, CNRF
      REAL CUMSENWT, CUMSENN , CUMSENP
      REAL DECFACT, DLTNI1, DLTNI2, DLTPI1, DLTPI2, DLTSRF, DLTSRFN
      REAL DLTSRFP, DMINR, DltSRF_AM, DltSRF_EXTFAC, DltSRF_WATFAC
      REAL DMOD, DSNC, FOMFRAC, HUMFRAC, IMMOBN, IMMOBP, NNOM, PNOM
      REAL PRCEL, PRCHO, PRLIG, RDCEL, RDCHO, RDLIG
      REAL REDFACT
      REAL NI_AVAIL 
      REAL SWEF, TLITC, TLITE(NELEM), TFSOM
      REAL TSOMC, TSOME(NELEM)
      REAL WFSOM, XL, EXTFAC, WATFAC
      REAL TOMINFOM, TOMINSOM, TNIMBSOM
      REAL DECOMP1, DECOMP2, DECOMP3, DLTFOM
      
      REAL TFPOOL(3)

      REAL BD(NL), CNRAT(NL), DLAYR(NL) 
      REAL DLTFON(NL), DLTFOP(NL), DLTHUMC(0:NL), DLTHUME(0:NL,NELEM)
      REAL LITE(0:NL,NELEM)
      REAL DUL(NL), FOM(NL), FON(NL), FOP(NL)
      REAL KG2PPM(NL), LITC(0:NL), LL(NL)   !, LITE(NL,NELEM)
      REAL IMM(0:NL,NELEM), MNR(0:NL,NELEM)
      REAL NH4(NL), NO3(NL), PH(NL), SAT(NL), SNH4(NL)
      REAL SNH4NO3(NL), SNO3(NL), SomLitC(0:NL), SomLitE(0:NL,NELEM)
      REAL SPi_Labile(NL), SSOMC(0:NL), SSOME(0:NL,NELEM), SomLit(NL)
      REAL ST(NL), SW(NL)

      REAL DLTFPOOL(NL,3), FPOOL(NL,3)

!     Variables added for flooded conditions analysis:
      TYPE (FloodWatType) FLOODWAT    
      TYPE (FloodNType)   FLOODN    
      TYPE (MulchType)    MULCH

      REAL FLOOD
      REAL PHMIN, DMOD1, CMF, REQN

!     Plant senesced matter(surface-soil, carbon-lignin-nitrogen)
      REAL SenWt(0:NL)        !kg[dry matter]/ha
!     REAL SenLig(0:NL)       !kg[lignin]/ha
      REAL SenE(0:NL,NELEM)   !kg[E]/ha (E=N, P, S,...)
      REAL SEN_AM, SEN_PRCEL, SEN_PRCHO, SEN_PRLIG,SEN_EXTFAC,SEN_WATFAC
      REAL SENSUM

!     Added for tillage
      INTEGER I, TILDATE
      REAL DEPTH, FR, HOLD
      REAL MIXPCT, RESINC, RESINCN, RESINCP, TDEP, TILRESINC

!     Added for N2O
      REAL newCO2(0:NL), newCO2_HUM(0:NL), newCO2_FOM(0:NL)

!     Added for SCBal
      REAL ACCCO2(0:1)
      REAL SOM1C(0:NL)
      REAL CN_SOM(0:NL), CN_FOM(0:NL)

!     Methane variables:
      REAL Immob_OM
!     REAL CH4Consumption, CH4Emission, CH4Leaching, CH4Stored,
!    &    CO2emission, CumCH4Consumpt, CumCH4Emission, 
!    &    CumCH4Leaching, CumCO2Emission
      REAL RLV(NL), DRAIN
      TYPE (CH4_type) CH4_data

      REAL, PARAMETER :: FOMCFrac = 0.4
      REAL, PARAMETER :: HumusCFrac = 0.526 !(=1/1.9)
      REAL, PARAMETER :: HumusCNRatio = 10.0

!-----------------------------------------------------------------------
!     Constructed variables are defined in ModuleDefs.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (SoilType)    SOILPROP
      Type (ResidueType) HARVRES
      Type (ResidueType) SENESCE
      TYPE (OrgMatAppType) OMAData
      TYPE (TillType)    TILLVALS
      TYPE (FertType)    FERTDATA

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      N_ELEMS = CONTROL % N_ELEMS
      YRDOY   = CONTROL % YRDOY

      BD     = SOILPROP % BD  
      DLAYR  = SOILPROP % DLAYR  
      DMOD   = SOILPROP % DMOD    
      DUL    = SOILPROP % DUL    
      KG2PPM = SOILPROP % KG2PPM  
      LL     = SOILPROP % LL     
      NLAYR  = SOILPROP % NLAYR  
      PH     = SOILPROP % PH     
      SAT    = SOILPROP % SAT   

      FLOOD = FLOODWAT % FLOOD

      SenWt  = SENESCE % ResWt
!     SenLig = SENESCE % ResLig
      SenE   = SENESCE % ResE

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!     ------------------------------------------------------------------
!     --------------------------------------------------------------
!     Non-sequenced runs:
      IF (INDEX('QF',CONTROL%RNMODE) .GT. 0 .AND. CONTROL%RUN .GT. 1)
     &    THEN; RETURN
      ENDIF
!     --------------------------------------------------------------

!      For ORYZA, need estimate of initial soil carbon, even when N not simulated
!      ISWWAT = ISWITCH % ISWWAT 
!      IF (ISWITCH%ISWWAT == 'N') THEN
         IMM     = 0.0
         LITC    = 0.0
         MNR     = 0.0
         SomLit  = 0.0
         SomLitC = 0.0
         SomLitE = 0.0
         SSOMC   = 0.0
!        MULCH % MULCHMASS = 0.0
!        RETURN
!      ENDIF

!     These used to come from RESCH???.SDA, but were never varied
!     in that file.  File has been changed to provide Century data, so
!     hardwire here for now. 
      RDCHO = 0.2000     !Potential decomp. rate of carbohydrate
      RDCEL = 0.0500     !Potential decomp. rate of cellulose
      RDLIG = 0.0095     !Potential decomp. rate of lignin
      DMINR = 0.8300E-04 !Potential decomp. rate of SOM pool
      DSNC  = 20.0       !Depth for soil output (cm)     

      ACCCO2 = 0.0  !Cumulative CO2 released from SOM, FOM decomposition
      SOM1C  = 0.0  !Active SOM pool (not used in CERES SOM method)

      newCO2_FOM = 0.0
      newCO2_HUM = 0.0
      newCO2 = 0.0

      CALL MethaneDynamics(CONTROL, ISWITCH, SOILPROP,        !Input
     &    FERTDATA, FLOODWAT, SW, RLV, newCO2, DRAIN,         !Input
     &    CH4_data)                                           !Output

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!     ------------------------------------------------------------------
!      For ORYZA, need estimate of initial soil carbon, even when N not simulated
!      IF (ISWWAT == 'N') RETURN

!     Set initial SOM and nitrogen conditions for each soil layer.
!     Default characteristics for residue and senesced matter 
      CALL IPSOIL (CONTROL, CROP=CONTROL % CROP,          !Input
     &    DMINR=DMINR, DSNC=DSNC, PRCEL=PRCEL,            !Output
     &    PRCHO=PRCHO, PRLIG=PRLIG, AM=AM, EXTFAC=EXTFAC, !Output
     &    WATFAC=WATFAC)                                  !Output

      SEN_AM    = AM
      SEN_EXTFAC= EXTFAC
      SEN_WATFAC= WATFAC
      SEN_PRCEL = PRCEL
      SEN_PRCHO = PRCHO
      SEN_PRLIG = PRLIG

      Mulch % Mulch_AM   = AM
      Mulch % Mul_WATFAC = WATFAC
      Mulch % Mul_EXTFAC = EXTFAC
      Mulch % NewMulch   = 0.0

      SWEF = 0.9-0.00038*(DLAYR(1)-30.)**2

      CALL SoilOrg_init(CONTROL, 
     &    HARVRES, PREV_CROP, SOILPROP,                   !Input
     &    CNRAT, FOM, FON, FOP, FPOOL, SSOMC,             !Output
     &    SSOME, MULCH)                                   !Output

      CUMSENWT = 0.0
      CUMSENN  = 0.0
      CUMSENP  = 0.0
      SENESCE % CumResE(N) = 0.0
      SENESCE % CumResE(P) = 0.0
      SENESCE % CumResWt   = 0.0

!     Initialize
      DLTFON   = 0.0
      DLTFOM   = 0.0
      DLTFOP   = 0.0
      DLTFPOOL = 0.0
      DLTSRF   = 0.0
      DLTSRFN  = 0.0
      DLTSRFP  = 0.0
      DLTHUMC  = 0.0
      DLTHUME  = 0.0

      IMM = 0.0 
      MNR = 0.0

!     Soil profile accumulations.
      TLITE  = 0.0
      TSOME  = 0.0
      DO L = 1, NLAYR
        IF (N_ELEMS > 0) THEN
          TLITE(N) = TLITE(N) + FON(L)
          TSOME(N) = TSOME(N) + SSOME(L,N)
        ENDIF
        IF (N_ELEMS > 1) THEN
          TLITE(P) = TLITE(P) + FOP(L)
          TSOME(P) = TSOME(P) + SSOME(L,P)
        ENDIF
      ENDDO

      PREV_CROP = CONTROL % CROP  !Save crop name for harvest residue

      ACCCO2 = 0.0 !Cumulative CO2 released from SOM, FOM decomposition
      newCO2_FOM = 0.0
      newCO2_HUM = 0.0
      newCO2 = 0.0

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!     ------------------------------------------------------------------
      IF (ISWWAT == 'N') RETURN

!     Initialize
      DLTFON   = 0.0
      DLTFOM   = 0.0
      DLTFOP   = 0.0
      DLTFPOOL = 0.0
      DLTSRF   = 0.0
      DLTSRFN  = 0.0
      DLTSRFP  = 0.0
      DLTHUMC  = 0.0
      DLTHUME  = 0.0

      IMM = 0.0 
      MNR = 0.0

      newCO2_FOM = 0.0
      newCO2_HUM = 0.0
      newCO2 = 0.0

!     Add senesced matter to surface and soil
      SENSUM = SUM(SENWT)
      IF (SENSUM .GT. 0.0) THEN
!       senesced plant matter on surface kg/ha
        DltSRF = DltSRF + SenWt(SRFC)  !kg[dry weight]/ha
        DltSRF_AM = SEN_AM
        DltSRF_EXTFAC = SEN_EXTFAC
        DltSRF_WATFAC = SEN_WATFAC
!       N in senesced plant matter on surface
        IF (N_ELEMS > 0) DltSRFN = DltSRFN + SenE(SRFC,N) !kg[N]/ha
!       P in senesced plant matter on surface
        IF (N_ELEMS > 1) DltSRFP = DltSRFP + SenE(SRFC,P) !kg[P]/ha

        CUMSENWT = CUMSENWT + SenWt(SRFC)
        IF (N_ELEMS > 0) CUMSENN  = CUMSENN  + SenE(SRFC,N)
        IF (N_ELEMS > 1) CUMSENP  = CUMSENP  + SenE(SRFC,P)

!       Add senesced material to soil layers
        DO L = 1, NLAYR
          IF (N_ELEMS > 0) DltFON(L) = DltFON(L) + SenE(L,N)
          IF (N_ELEMS > 1) DltFOP(L) = DltFOP(L) + SenE(L,P)

          ADD = SenWt(L)              !kg[dry matter]/ha
          DltFPOOL(L,1) = DltFPOOL(L,1) + ADD * SEN_PRCHO 
          DltFPOOL(L,2) = DltFPOOL(L,2) + ADD * SEN_PRCEL 
          DltFPOOL(L,3) = DltFPOOL(L,3) + ADD * SEN_PRLIG 

          CUMSENWT = CUMSENWT + SenWt(L)
          IF (N_ELEMS > 0) CUMSENN  = CUMSENN  + SenE(L,N)
          IF (N_ELEMS > 1) CUMSENP  = CUMSENP  + SenE(L,P)
        ENDDO

        SENESCE % CumResWt   = CUMSENWT
        IF (N_ELEMS > 0) SENESCE % CumResE(N) = CUMSENN
        IF (N_ELEMS > 1) SENESCE % CumResE(P) = CUMSENP
      ENDIF

!     Add organic matter applications
      IF (YRDOY == OMAData % ResDAT) THEN
        CALL IPSOIL (CONTROL, OMAData % RESTYPE,          !Input
     &    PRCEL=PRCEL, PRCHO=PRCHO, PRLIG=PRLIG, AM=AM,   !Output
     &    RDCEL=RDCEL, RDCHO=RDCHO, RDLIG=RDLIG,          !Output
     &    WATFAC=WATFAC, EXTFAC=EXTFAC)                   !Output

!       CHP 8/15/08 
        IF (PRLIG < 0) PRLIG = 0.10

!       applied organic matter on surface kg/ha
        DltSRF = DltSRF + OMAData % ResWt(SRFC)  !kg[dry weight]/ha
        DltSRF_AM = AM
        DltSRF_WATFAC = WATFAC
        DltSRF_EXTFAC = EXTFAC

!       N in senesced plant matter on surface
        IF (N_ELEMS > 0) DltSRFN = DltSRFN + OMAData % ResE(SRFC,N) 
!       P in senesced plant matter on surface
        IF (N_ELEMS > 1) DltSRFP = DltSRFP + OMAData % ResE(SRFC,P) 

!       Add senesced material to soil layers
        DO L = 1, NLAYR
          IF (N_ELEMS > 0) DltFON(L) = DltFON(L) + OMAData % ResE(L,N)
          IF (N_ELEMS > 1) DltFOP(L) = DltFOP(L) + OMAData % ResE(L,P)

          ADD = OMAData % ResWt(L)              !kg[dry matter]/ha
          DltFPOOL(L,1) = DltFPOOL(L,1) + ADD * PRCHO 
          DltFPOOL(L,2) = DltFPOOL(L,2) + ADD * PRCEL 
          DltFPOOL(L,3) = DltFPOOL(L,3) + ADD * PRLIG 
        ENDDO
      ENDIF

!     Add organic matter from algae die-off or flood dry-up
      IF (N_ELEMS > 0) DltFON(1) = DltFON(1) + FLOODN % ALGFON

!     ------------------------------------------------------------------
!     Check if tillage occurred today -- if so, mix soil N & C in
!       tillage depth
      IF (TILLVALS % NTIL .GT. 0) THEN
        TILDATE = TILLVALS % TILDATE
        IF (YRDOY .EQ. TILDATE) THEN
!         Incorporate surface residue
          TDEP = TILLVALS % TILDEP
          TILRESINC = TILLVALS % TILRESINC

!         Amount of surface mulch to be incorporated
          RESINC  = MULCH % MULCHMASS * TILRESINC / 100.
          RESINCN = MULCH % MULCHN * TILRESINC / 100.
          RESINCP = MULCH % MULCHP * TILRESINC / 100.

          IF (RESINC .GT. 0.001 .AND. TDEP .GT. 0.001) THEN 
            DLTSRF  = DLTSRF  - RESINC
            DLTSRFN = DLTSRFN - RESINCN
            DLTSRFP = DLTSRFP - RESINCP

!           Distribute thru tillage depth
            DEPTH = 0.0
            DO L = 1, NLAYR
              HOLD  = DEPTH               !Depth to top of layer
              DEPTH = DEPTH + DLAYR(L)    !Depth to bottom of layer
              IF (TDEP .LE. DEPTH) THEN
!               FR = fraction of the residue added to this layer
                FR = (TDEP - HOLD) / TDEP
              ELSE
                FR = DLAYR(L) / TDEP
              ENDIF   

!             Add the residue to the carbohydrate, cellulose and lignin
!             pools of the layer.
              ADD = RESINC * FR
              DltFPOOL(L,1) = DltFPOOL(L,1) + ADD * PRCHO 
              DltFPOOL(L,2) = DltFPOOL(L,2) + ADD * PRCEL 
              DltFPOOL(L,3) = DltFPOOL(L,3) + ADD * PRLIG 

!             Add the residue to the rate variables.
              DLTFON(L) = DLTFON(L) + RESINCN * FR        !kg[N]/ha
              DLTFOP(L) = DLTFOP(L) + RESINCP * FR        !kg[P]/ha

!             If there are no more soil layers over which to distribute
!             the residue, jump out of the DO loop. 
              IF (DEPTH >= TDEP) EXIT
            ENDDO   !End of loop on soil layers.
          ENDIF   !End of IF block on RESSOL and RESDEPTH.

!         Call mixing routine for soil properties
          MIXPCT = TILLVALS % TILMIX

          CALL SoilMix (SSOMC, DLTHUMC, 0, DLAYR, MIXPCT, NLAYR, TDEP)
          CALL SoilMix (FON,   DLTFON,  1, DLAYR, MIXPCT, NLAYR, TDEP)
          CALL SoilMix (FOP,   DLTFOP,  1, DLAYR, MIXPCT, NLAYR, TDEP)

          DO IEL = 1, N_ELEMS
            CALL SoilMix (SSOME, DLTHUME, 0,IEL,DLAYR,MIXPCT,NLAYR,TDEP)
          ENDDO

          DO I = 1, 3
            CALL SoilMix (FPOOL, DLTFPOOL,1, I, DLAYR,MIXPCT,NLAYR,TDEP)
          ENDDO
        ENDIF
      ENDIF

      DMOD1 = DMOD   !used for alt. value of DMOD

!     ------------------------------------------------------------------
!     Loop through soil layers for rate calculations
!     ------------------------------------------------------------------

      TOMINFOM = 0.0
      TOMINSOM = 0.0
      TNIMBSOM = 0.0

      newCO2 = 0.0

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
          PHMIN = 1.00
        ELSE
          PHMIN = PH(L)/3.0 - 1.1
        ENDIF

!       Limit the soil water factors between 0 and 1.
        WFSOM = MAX (MIN (WFSOM, 1.), 0.)
        PHMIN = MAX (MIN (PHMIN, 1.), 0.)

!       Calculate the soil temperature factor for the SOM
!       decomposition model.
        TFSOM = (ST(L) - 5.) / 30.
        TFSOM = MAX (MIN (TFSOM, 1.), 0.)
        IF (ST(L) .LT. 5.) TFSOM = 0.

!-----------------------------------------------------------------------
!       FOM + SOM decay
!-----------------------------------------------------------------------
        IF (N_ELEMS > 0) THEN
!         -----------------
!         FOM decomposition
!         -----------------
          SNO3(L) = NO3(L) / KG2PPM(L)
          SNH4(L) = NH4(L) / KG2PPM(L)
          SNH4NO3(L) = SNO3(L) + SNH4(L)   !No XMIN needed
          SNH4NO3(L) = AMAX1 (SNH4NO3(L), 0.0)
        ENDIF

!       Only do FOM decomposition if there is something to decompose.
        FOM(L) = FPOOL(L,1) + FPOOL(L,2) + FPOOL(L,3)
        IF (FOM(L) >= 1.E-3) THEN

          CMF = WFSOM + 0.25                 !From RICE model
          CMF = MAX (MIN (CMF, 1.), 0.)

          IF (N_ELEMS > 0) THEN
!           C/N ratio of the fresh organic residue in the soil, including
!           the mineral N that is present in the soil.
            IF (FON(L) + SNH4NO3(L) > 1.E-6) THEN
                CNR = (FOMCFrac * FOM(L)) / (FON(L) + SNH4NO3(L))
            ELSE
                CNR = 25.
            ENDIF

!           C/N ratio factor with a critical C:N ratio of 25.
            CNRF = EXP (-0.693 * (CNR - 25.) / 25.0)
            CNRF = AMIN1 (CNRF, 1.0)
          ELSE
            CNRF = 1.0
          ENDIF

!         Decomposition factor as function of temperature, water and
!         C/N ratio.
          IF (FOM(L) .LT. 5.0) THEN
            DECFACT = 0.0
          ELSE
            DECFACT = TFSOM * WFSOM * CNRF
          ENDIF

!         FOM fraction that decomposes, summed across the three pools.
          FOMFRAC = DECFACT * (FPOOL(L,1) * RDCHO 
     &                      +  FPOOL(L,2) * RDCEL 
     &                      +  FPOOL(L,3) * RDLIG) / FOM(L)

C         Add different switch point for immobln under flooded conditions
C         REQN = N Req. ---> To decompose 1g of FOM (GRCOM) will have to
C         recruit (NREQ-N CONC) g of N
          IF (FLOOD .GT. 0.0) THEN
            REQN = 0.01
          ELSE
            REQN = 0.02
          ENDIF

          IF (N_ELEMS > 0) THEN
!           N immobilization by decomposing residue only occurs if its N%
!           (FON/FOM) is less than 2%. This value results from the carbon
!           conversion efficiency of microbes and their C:N ratio.
!           IMMOB = FOMFRAC * FOM(L) * (0.02 - FON(L) / FOM(L))
            IMMOBN = FOMFRAC * FOM(L) * (REQN - FON(L) / FOM(L))
            IMMOBN = AMAX1 (IMMOBN, 0.0)

!           Limit the immobilization to the available mineral N in the
!           soil. Also take into account the NH4 and NO3 that was already
!           removed by other processes (thus only take the negative value
!           of DLTSNH4 and DLTSNO3).
!            NI_AVAIL = AMAX1(0.0, SNH4NO3(L) + MIN (DLTSNH4(L), 0.) +
!     &        MIN (DLTSNO3(L), 0.))
!           CHP removed DLTSHN4 and DLTSNO3 on 2/28/2005 
!           No changes to snh4 or sno3 at this point in program.
!            NI_AVAIL = AMAX1(0.0, SNH4NO3(L) + DLTSNH4(L) + DLTSNO3(L))
            NI_AVAIL = AMAX1(0.0, SNH4NO3(L))

!           Limit the FOM decomposition if there is not enough mineral N.
            IF (IMMOBN .GT. NI_AVAIL) THEN
              REDFACT = NI_AVAIL / IMMOBN
              IMMOBN = NI_AVAIL
              FOMFRAC = FOMFRAC * REDFACT
              DECFACT = DECFACT * REDFACT
            ENDIF

!           The FOM fraction that decomposes. 
!           DLTFPOOL(L,1) = DLTFPOOL(L,1) - FPOOL(L,1) * DECFACT * RDCHO
!           DLTFPOOL(L,2) = DLTFPOOL(L,2) - FPOOL(L,2) * DECFACT * RDCEL
!           DLTFPOOL(L,3) = DLTFPOOL(L,3) - FPOOL(L,3) * DECFACT * RDLIG

            Decomp1 = FPOOL(L,1) * DECFACT * RDCHO
            Decomp2 = FPOOL(L,2) * DECFACT * RDCEL
            Decomp3 = FPOOL(L,3) * DECFACT * RDLIG

            DLTFPOOL(L,1) = DLTFPOOL(L,1) - Decomp1
            DLTFPOOL(L,2) = DLTFPOOL(L,2) - Decomp2
            DLTFPOOL(L,3) = DLTFPOOL(L,3) - Decomp3
            DLTFOM = Decomp1 + Decomp2 + Decomp3

!           80% of FOM decomposition goes to CO2 (the remainder to HUM pool)
!           Convert FOM to C units with 0.4 multiplier
            newCO2_FOM(L) = 0.8 * FOMCFrac * DLTFOM

!           Amount of OM associated with immobilized N
            Immob_OM = IMMOBN * HumusCNRatio / HumusCFrac
!           Change to FON (also, change to inorganic N)
            DLTNI1 = IMMOBN - FOMFRAC * FON(L)
            DLTFON(L) = DLTFON(L) + DLTNI1
            IF (DLTNI1.LT.0.0) THEN
              TOMINFOM = TOMINFOM + (-DLTNI1)
            ELSE
              TNIMBSOM = TNIMBSOM + DLTNI1
            ENDIF

          ENDIF     !N_ELEMS > 0, N simulation

          IF (N_ELEMS > 1) THEN
!           Change to P component at a N:P ratio of 10:1 if available
            IMMOBP = 0.10 * IMMOBN
            IMMOBP = AMIN1(IMMOBP, SPi_Labile(L))
            DLTPI1 = IMMOBP - FOMFRAC * FOP(L)
            DLTFOP(L) = DLTFOP(L) + DLTPI1
          ENDIF   !N_ELEMS > 1, P simulation
          
        ELSE   !If there is no FOM.
          DECFACT = 0.0
          FOMFRAC = 0.0
          IMMOBN  = 0.0
          IMMOBP  = 0.0
          CMF     = 1.0
          DLTNI1  = 0.0
          DLTFOM  = 0.0
        ENDIF   !End of IF block on FOM.

!       -------------------
!       Humus decomposition
!       -------------------
!       The humus fraction that decomposes only depends on the
!       environmental conditions and the decomposition constant; the C:N
!       ratio is not important.
        HUMFRAC = DMINR * TFSOM * CMF * DMOD1 * PHMIN
        DLTHUMC(L) = DLTHUMC(L) - HUMFRAC * SSOMC(L)

        newCO2_HUM(L) = HUMFRAC * SSOMC(L)
        newCO2(L) = newCO2_HUM(L) + newCO2_FOM(L)

!       chp 2019-03-07 Add 20% of C, regardless of N movement. Let C decomposition
!         drive the mass transfer.
        DLTHUMC(L) = DLTHUMC(L) + 0.2 * FOMCFrac * DLTFOM

        IF (N_ELEMS > 0) THEN
!          DLTHUMC(L) = DLTHUMC(L) + 0.2 * FOMFRAC * FON(L) * 10.0
!          DLTNI2 = - HUMFRAC * SSOME(L,N) + 0.2 * FOMFRAC * FON(L)

!         Change in humus N = reduction based on decomposition of humus
!           plus addition of 20% of fresh organic matter
          DLTNI2 = - HUMFRAC * SSOME(L,N) + 0.2 * FOMFRAC * FON(L)
          DLTHUME(L,N) = DLTHUME(L,N) + DLTNI2
          IF (DLTNI2.LT.0.0) THEN
            TOMINSOM = TOMINSOM + (-DLTNI2)
          ELSE
            TNIMBSOM = TNIMBSOM + DLTNI2
          ENDIF
        ENDIF

!       Phosphorus
        IF (N_ELEMS > 1) THEN
!         N:P ratio of 10 for new humus.
          DLTPI2 = -HUMFRAC*SSOME(L,P) + 0.2*FOMFRAC*FON(L)/HumusCNRatio
          IF (DLTPI2 + SSOME(L,P) < 1.E-5) THEN
            DLTPI2 = -SSOME(L,P)
          ENDIF
          DLTHUME(L,P) = DLTHUME(L,P) + DLTPI2
        ENDIF

        IF (N_ELEMS > 0) THEN
!         Net N release from all SOM and FOM sources
!         NNOM = HUMFRAC * SSOME(L,N) + 0.8 * FOMFRAC *FON(L) - IMMOBN 
          NNOM = - DLTNI1 - DLTNI2

!         8/25/2005 Separate immobilization from mineralization 
!         MINERALIZE(L,N) = NNOM   
          IF (NNOM >= 0.0) THEN 
            MNR(L,N) = NNOM
          ELSE
            IMM(L,N) = -NNOM
          ENDIF
        ELSE
          NNOM = 0.0
        ENDIF

        IF (N_ELEMS > 1) THEN
!         Net P release from all SOM and FOM sources
          PNOM = - DLTPI1 - DLTPI2
          IF (PNOM >= 0.0) THEN
            MNR(L,P) = PNOM
          ELSE
            IMM(L,P) = -PNOM
          ENDIF
        ELSE
          PNOM = 0.0
!         MINERALIZE(L,P) = PNOM
        ENDIF
      END DO   !End of soil layer loop.

      CALL MethaneDynamics(CONTROL, ISWITCH, SOILPROP,        !Input
     &    FERTDATA, FLOODWAT, SW, RLV, newCO2, DRAIN,         !Input
     &    CH4_data)                                           !Output

!     Transfer daily mineralization values for use by Cassava model
      CALL PUT('ORGC','TOMINFOM' ,TOMINFOM) !Miner from FOM (kg/ha)
      CALL PUT('ORGC','TOMINSOM' ,TOMINSOM) !Miner from SOM (kg/ha)
      CALL PUT('ORGC','TNIMBSOM', TNIMBSOM) !Immob (kg/ha)

!***********************************************************************
!***********************************************************************
!     END OF RATE SECTION
!***********************************************************************
      ENDIF

!***********************************************************************
!***********************************************************************
!     DAILY INTEGRATION (also performed for seasonal initialization)
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT .OR. DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
!      For ORYZA, need estimate of initial soil carbon, even when N not simulated
!      IF (ISWWAT == 'N') RETURN

!     Loop through soil layers for integration
      DO L = 1, NLAYR
        SSOMC(L)   = SSOMC(L)   + DLTHUMC(L)
        FPOOL(L,1) = FPOOL(L,1) + DLTFPOOL(L,1)
        FPOOL(L,2) = FPOOL(L,2) + DLTFPOOL(L,2)
        FPOOL(L,3) = FPOOL(L,3) + DLTFPOOL(L,3)
        FOM(L) = FPOOL(L,1) + FPOOL(L,2) + FPOOL(L,3)
!       FOM is units of kg[OM]/ha, HUMC in units of kg[C]/ha
!       Convert SSOMC at 1.9 kg[OM]/kg[C] (Adams, 1973)
!       SomLit(L)  = SSOMC(L) * 1.9 + FOM(L)       !kg[OM]/ha
        SomLit(L)  = SSOMC(L) / HumusCFrac + FOM(L)       !kg[OM]/ha

!       CHP 3/1/2005. LITC in kg[C]/ha, FOM in kg[dry matter]/ha
!       These are put into "Century" variable names to export to the
!       inorganic soil routines.
!       Convert FOM at 2.5 kg[OM]/kg[C] 
        LITC(L) = FOM(L) * FOMCFrac
        SomLitC(L) = SSOMC(L) + LITC(L) 

        IF (N_ELEMS > 0) THEN
          SSOME(L,N) = SSOME(L,N) + DLTHUME(L,N)
          FON(L)     = FON(L)     + DLTFON(L)
          LITE(L,N) = FON(L)
          SomLitE(L,N) = SSOME(L,N) + FON(L)
        ENDIF

        IF (N_ELEMS > 1) THEN
          SSOME(L,P) = SSOME(L,P) + DLTHUME(L,P)
          FOP(L)     = FOP(L)     + DLTFOP(L)
          LITE(L,P) = FOP(L)
          SomLitE(L,P) = SSOME(L,P) + FOP(L)
        ENDIF
        ACCCO2(1) = ACCCO2(1) + newCO2(L) 

!       C:N ratios
        IF (SSOME(L,N) .GT. 1.E-6) THEN
          CN_SOM(L) = SSOMC(L) / SSOME(L,N)
        ELSE
          CN_SOM(L) = HumusCNRatio
        ENDIF
        IF (LITE(L,N) .GT. 1.E-6) THEN
          CN_FOM(L) = LITC(L) / LITE(L,N)
        ELSE
          CN_FOM(L) = HumusCNRatio
        ENDIF
      ENDDO

!     Call NCHECK to check for and fix negative values.
      CALL NCHECK_organic(CONTROL, N_ELEMS, 
     &    FOM, FON, FOP, FPOOL, NLAYR, SSOMC, SSOME)      !Input

!     Soil profile accumulations.
      TLITE  = 0.0
      TLITC  = 0.0
      TSOMC  = 0.0
      TSOME  = 0.0
      TFPOOL(1) = 0.0
      TFPOOL(2) = 0.0
      TFPOOL(3) = 0.0
      DO L = 1, NLAYR
        TLITE(N) = TLITE(N) + FON(L)
        TLITE(P) = TLITE(P) + FOP(L)
        TLITC = TLITC + LITC(L)
        TSOMC = TSOMC + SSOMC(L)
        TSOME(N) = TSOME(N) + SSOME(L,N)
        TSOME(P) = TSOME(P) + SSOME(L,P)
        TFPOOL(1) = TFPOOL(1) + FPOOL(L,1)
        TFPOOL(2) = TFPOOL(2) + FPOOL(L,2)
        TFPOOL(3) = TFPOOL(3) + FPOOL(L,3)
      ENDDO

      CALL PUT('ORGC','TSOMC',TSOMC)

!     Sum up all the residues that were not incorporated 
      IF (DltSRF > 1.E-6) THEN
        Mulch % Mulch_AM = (Mulch % Mulch_AM * Mulch % MulchMass 
     &   + DltSRF_AM * DltSRF) / (Mulch%MulchMass + DltSRF)
        Mulch % Mul_EXTFAC = (Mulch % Mul_EXTFAC * Mulch % MulchMass 
     &   + DltSRF_EXTFAC * DltSRF) / (Mulch%MulchMass + DltSRF)
        Mulch % Mul_WATFAC = (Mulch % Mul_WATFAC * Mulch % MulchMass 
     &   + DltSRF_WATFAC * DltSRF) / (Mulch%MulchMass + DltSRF)
        Mulch % NewMulch = DltSRF
      ENDIF
      MULCH % MULCHMASS = MULCH % MULCHMASS + DltSRF
      IF (N_ELEMS > 0) MULCH % MULCHN = MULCH % MULCHN + DltSRFN !kg/ha
      IF (N_ELEMS > 1) MULCH % MULCHP = MULCH % MULCHP + DltSRFP !kg/ha

      SomLitC(0)   = MULCH % MULCHMASS * FOMCFrac
      LITC(0)      = MULCH % MULCHMASS * FOMCFrac
      LITE(0,1)    = MULCH % MULCHN
      LITE(0,2)    = MULCH % MULCHP
      SomLitE(0,N) = MULCH % MULCHN
      SomLitE(0,P) = MULCH % MULCHP

!     C:N ratios
      CN_SOM(0) = 0.0
      IF (LITE(0,N) .GT. 1.E-6) THEN
        CN_FOM(0) = LITC(0) / LITE(0,N)
      ELSE
        CN_FOM(0) = HumusCNRatio
      ENDIF

!     Compute mulch properties
      CALL MULCHLAYER (MULCH)

      IF (DYNAMIC .EQ. INTEGR) THEN
        CALL MethaneDynamics(CONTROL, ISWITCH, SOILPROP,      !Input
     &    FERTDATA, FLOODWAT, SW, RLV, newCO2, DRAIN,         !Input
     &    CH4_data)                                           !Output
      ENDIF

C***********************************************************************
C***********************************************************************
C     END OF INTEGR
C***********************************************************************
      ENDIF

!***********************************************************************
!***********************************************************************
!     OUTPUT
!***********************************************************************
      IF (DYNAMIC == SEASINIT .OR. DYNAMIC == OUTPUT .OR. 
     &        DYNAMIC == SEASEND) THEN
C-----------------------------------------------------------------------
      CALL SOILCBAL (CONTROL, ISWITCH, 
     &  CH4_data, HARVRES, LITC, OMAData, SENESCE,            !Input
     &  SSOMC, TLITC, TSOMC, YRDOY)                           !Input

      IF (ISWWAT == 'N') RETURN

C     Write seasonal output
      CALL OpSoilOrg(CONTROL, ISWITCH, 
     &  DSNC, NLAYR, OMADATA, SOILPROP, SomLitC, SomLitE, !Input
     &  SSOMC, SSOME, TLITC, TLITE, TSOMC, TSOME)         !Input

      CALL SoilNoPoBal (CONTROL, ISWITCH, 
     &    FON, HARVRES, IMM, LITE, MNR, MULCH, N_ELEMS,   !Input
     &    NLAYR, OMADATA, SENESCE, TLITE, TSOME)          !Input

      CALL MethaneDynamics(CONTROL, ISWITCH, SOILPROP,        !Input
     &    FERTDATA, FLOODWAT, SW, RLV, newCO2, DRAIN,         !Input
     &    CH4_data)                                           !Output

C***********************************************************************
C***********************************************************************
C     END OF OUTPUT
C***********************************************************************
      ENDIF
C-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE SoilOrg

!=======================================================================
! SoilOrg Variables 
!-----------------------------------------------------------------------
! AD            Lowest possible volumetric water content of a soil layer 
!                 when it is air dry. For the top soil layer AD may be 
!                 lower than the lower limit LL, because this layer may dry 
!                 out due to water evaporation at the soil surface.
!                 (cm3 [H2O]/ cm3 [soil])
! CMF           Affect of soil water content on decomposition of fresh 
!                 organic matter 
! CNR           C/N ratio of FOM (kg [C] / kg [N])
! CNRAT(L)      C/N ratio of humus or humus pool in soil layer L
!                (kg [C] / kg [N])
! CNRF          C/N ratio factor: the effect of C/N ratio on SOM 
!                 decomposition (range 0-1) 
! CONTROL       Composite variable containing variables related to control 
!                 and/or timing of simulation.  The structure of the 
!                 variable (ControlType) is defined in ModuleDefs.for. 
! CUMSENN       Cumulative N in senesced plant matter added to soil and 
!                 surface (kg [N] / ha)
! DLAYR(L)      Thickness of soil layer L (cm)
! DLTFON(L)     Rate of change of N in fresh organic residue  in soil layer 
!                 L (kg [N] / ha / d)
! DLTFPOOL(L,J) Rate of change of FOM pool (FPOOL) in soil layer L 
!                 (J=1=carbohydrate, 2=cellulose, 3=lignin)
!                 (kg [residue pool] / ha / d)
! DLTHUMC(L)    Rate of change of Carbon in stable organic matter (humus) 
!                 in soil layer L (kg [C] / ha - d)
! DLTHUME(L,N)    Change in nitrogen in stable organic matter (humus) in soil 
!                 layer L (kg[N]/ha-d)
! DMINR         Maximum decomposition rate constant of stable organic 
!                 matter (d-1)
! DMOD          Factor to adjust the mineralization rate for certain 
!                 atypical soils (range 0-1) 
! DSNC          Depth to which C and N are integrated across all soil 
!                 layers for output in CARBON.OUT (cm)
! DUL(L)        Volumetric soil water content at Drained Upper Limit in 
!                 soil layer L (cm3[water]/cm3[soil])
! FOM(L)        Fresh organic residue in soil layer L
!                (kg [dry matter] / ha)
! FOMFRAC       Fraction of carbohydrate, cellulose, or lignin in FOM, 
!                 before the decomposition is done (fraction)
! FON(L)        Nitrogen in fresh organic matter in soil layer L
!                (kg [N] / ha)
! FPOOL(L,J)    FOM pool in soil layer L: J=1:carbohydrate, 2:cellulose, 
!                 3:lignin (kg [residue pool] / ha)
! HARVRES       Composite variable containing harvest residue amounts for 
!                 total dry matter, lignin, and N amounts.  Structure of 
!                 variable is defined in ModuleDefs.for. 
! SSOMC(L)      Carbon in stable organic matter (humus) (kg [C] / ha)
! HUMFRAC       Humus fraction that decomposes (fraction)
! SSOME(L,N)    Nitrogen in stable organic matter (humus) (kg [N] / ha)
! SSOME(L,P)    Phosphorus in stable organic matter (humus) (kg [P] / ha)
! IMMOBN         N immobilization rate associated with residue decomposition
!                (kg [N] / (ha - d))
! ISWITCH       Composite variable containing switches which control flow 
!                 of execution for model.  The structure of the variable 
!                 (SwitchType) is defined in ModuleDefs.for. 
! KG2PPM(L)     Conversion factor to switch from kg [N] / ha to �g [N] / g 
!                 [soil] for soil layer L 
! LL(L)         Volumetric soil water content in soil layer L at lower 
!                 limit (cm3 [water] / cm3 [soil])
! NH4(L)        Ammonium N in soil layer L (�g[N] / g[soil])
! NI_AVAIL      N available for immobilization (kg [N] / ha)
! NL            Maximum number of soil layers = 20 
! NLAYR         Actual number of soil layers 
! NNOM          Net mineral N release from all SOM sources (kg [N] / ha)
! NO3(L)        Nitrate in soil layer L (�g[N] / g[soil])
! OXLAYR        Composite variable which contains data about oxidation 
!                 layer.  See ModuleDefs.for for structure. 
! PH(L)         pH in soil layer L 
! PHFACT        Effect of pH on various soil N processes; value varies with 
!                 routine 
! PHMIN         Effect of pH on decomposition of fresh organic matter in 
!                 soil 
! PHN(L)        Factor for pH effect on nitrification rate (range 0-1) for 
!                 soil layer L 
! PRCEL         Cellulose fraction of the residue (fraction)
! PRCHO         Carbohydrate fraction of the residue (fraction)
! PRLIG         Lignin fraction of the residue (fraction)
! RCN           N content of initial root residue (%)
! RDCEL         Maximum decomposition rate of cellulose (fraction / day)
! RDCHO         Maximum decomposition rate of carbohydrates
!                (fraction / day)
! RDLIG         Maximum decomposition rate of lignin (fraction / day)
! REDFACT       Reduction factor for FOM decomposition and for N leaching 
! REQN          Threshhold N content of fresh organic matter, below which N 
!                 immobilization will occur during decomposition 
! RESLEFT       Residue material which is left on top of soil and not 
!                 incorporated (kg[residue]/ha)
! RESNIT        N in current residue application (kg[N]/ha)
! SAT(L)        Volumetric soil water content in layer L at saturation
!                (cm3 [water] / cm3 [soil])
! SENESCE       Composite variable containing data about daily senesced 
!                 plant matter. Structure of variable is defined in 
!                 ModuleDefs.for 
! SENSUM        Mass of senesced plant matter today (kg / ha)
! SENWT         Leaf senescence due to N mobilization
!                (g[leaf] / m2[ground])
! SNH4(L)       Total extractable ammonium N in soil layer L (kg [N] / ha)
! SNH4NO3(L)    Total inorganic N in layer L (kg [N] / ha)
! SNO3(L)       Total extractable nitrate N in soil layer L (kg [N] / ha)
! SOILPROP      Composite variable containing soil properties including 
!                 bulk density, drained upper limit, lower limit, pH, 
!                 saturation water content.  Structure defined in ModuleDefs. 
! ST(L)         Soil temperature in soil layer L (�C)
! SW(L)         Volumetric soil water content in layer L
!                (cm3 [water] / cm3 [soil])
! SWEF          Soil water evaporation fraction; fraction of lower limit 
!                 content to which evaporation can reduce soil water 
!                 content in top layer (fraction)
! TLITC          Sum of FOM (fresh organic matter) across the total soil 
!                 profile (kg [dry matter] / ha)
! TLITE          Sum of FON (N in fresh organic matter) across the total 
!                 soil profile (kg [N] / ha)
! TFPOOL(J)     Total of FOM pool summed over soil profile: 
!                 J=1:carbohydrate, 2:cellulose, 3:lignin
!                 (kg [residue pool] / ha)
! TFSOM         Soil temperature factor for SOM decomposition (range 0-1) 
! TSOMC         Total C in stable organic matter (SSOMC) in soil profile
!                (kg [C] / ha)
! TSOME(N)      Total N in stable organic matter (SSOME) in soil profile
!                (kg [N] / ha)
! WFSOM         Reduction factor for FOM decay based on soil water content 
!                 (no reduction for SW = DUL, 100% reduction for SW = LL, 
!                 50% reduction for SW = SAT) 
! XL            Excess water (above DUL) as a fraction of the maximum 
!                 amount of excess water (i.e. saturated). (fraction)
!***********************************************************************
