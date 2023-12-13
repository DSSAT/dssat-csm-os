!=======================================================================
!  CENTURY, Subroutine for CENTURY-based SOM/residue module of DSSAT.
!
!  Determines N transformations
!-----------------------------------------------------------------------
!  Revision       History
!  .......... Parton et al.  Written for CENTURY model.
!  08/08/1992 WTB Modified for version 3 input/output (matches LeGRO).
!  02/08/1993 PWW Header revision and minor changes. 
!  06/09/1999 AJG Completely revised the soil N and SOM module, and made
!               a new SOM module based on the CENTURY model.
!  06/21/1999 CHP Modular format. 
!  01/01/2000 CHP/AJG Integrated the CENTURY-based and CERES-based SOM
!               modules with CHP's modular format.
!  04/20/2002 GH  Adjusted for crop rotations
!  04/25/2002 AJG Lots of small changes and linked SoilCBal.
!  11/11/2002 AJG Added DLAYR to LITDEC_C and SOMDEC_C.
!  07/17/2003 CHP Added call to TILLAGE for tillage data - removed
!                   direct reads of tillage info from FILEIO
!                 Added some initialization statements
!  11/04/2003 AJG/AG  Added P option, removed code indenting after DYNAMIC
!                 and rearranged the variable declarations.
!  11/07/2003 CHP Added tile drainage to NFLUX
!  11/11/2002 AJG Corrected the use of CES21T and CES3T.
!  11/14/2003 CHP Re-set value of DISTURBNUM for sequenced runs.
!  10/22/2003 AJG Added a soil-P option.
!  12/17/2003 AJG Renamed all the CH_ and CHEM_ variables to P_ variables.
!  01/12/2004 AJG Made the C:E ratio calculations a separate subroutine
!                 CE_RATIO_C.
!  08/30/2004 AJG Corrected some layout irregularities.
!  05/19.2005 AJG Rearranged the variable declarations.
!  07/14/2006 CHP Put revised DSSAT-Century with P model into CSM v4.6
!  02/11/2010 CHP No simulation of organic matter when water is not 
!                   simulated.
!  01/26/2023 CHP Reduce compile warnings: add EXTERNAL stmts, remove 
!                 unused variables, shorten lines. 
!-----------------------------------------------------------------------
!  Called: SOIL
!  Calls : CE_RATIO_C, DECRAT_C, IMMOBLIMIT_C, INCORPOR_C, LITDEC_C, 
!          NCHECK_C, OpSoilOrg, OPSOMLIT_C, RPLACE_C, 
!          SENESADD_C, SOILCBAL_C, SoilNoBal_C, SoilCNPinit_C, 
!          SOMDEC_C, SOMLITPRINT_C, TSOMLIT_C, YR_DOY
!=======================================================================

      SUBROUTINE CENTURY (CONTROL, ISWITCH, 
     &  DRAIN, FERTDATA, FLOODWAT, FLOODN, HARVRES,   !Input
     &  NH4, NO3, OMADATA, RLV, SENESCE,              !Input
     &  SOILPROP, SPi_Labile, ST, SW, TILLVALS,       !Input
     &  CH4_data, IMM, LITC, MNR, MULCH, newCO2,      !Output
     &  SomLit,SomLitC, SomLitE, SSOMC)               !Output

!     ------------------------------------------------------------------
      USE ModuleDefs
      USE FloodModule             
      USE ModSoilMix
      USE GHG_mod

      IMPLICIT  NONE
      EXTERNAL YR_DOY, OPSOMLIT_C, SoilCNPinit_C, TSOMLIT_C, 
     &  SOMLITPRINT_C, SENESADD_C, OpSoilOrg, MethaneDynamics, 
     &  SOILCBAL, SoilNoBal_C, SoilPoBal_C, RPLACE_C, INCORPOR_C, 
     &  DECRAT_C, CE_RATIO_C, LITDEC_C, SOMDEC_C, IMMOBLIMIT_C,
     &  NCHECK_C, INCYD
      SAVE
!     ------------------------------------------------------------------

      LOGICAL ADDMETABEFLAG, FRMETFLAG
      LOGICAL DOCULT(0:NL)
      CHARACTER*1  IDETL, RNMODE, ISWWAT
      CHARACTER*2  CROP

      INTEGER DISTURBENDMAX, DISTURBNUM, DNUM, DOY, DYNAMIC, 
     &  FERTDAY, RESDAT, I, IEL, INCYD, L, N_ELEMS, 
     &  NLAYR, RUN, YEAR, YRDOY
      INTEGER, DIMENSION(NAPPL*3) :: DEND, DISTURBEND
      INTEGER, PARAMETER :: NONLIG = 1, LIG = 2, SRFC = 0, SOIL = 1
      
      real newCO2(0:NL)      ! DayCent PG

      REAL CO2S2, CO2S3, CULMETQ, CULS1Q, CULS2Q,
     &  CULS3Q, CULSTRQ, DEPTH, DISTURBDEPTHMAX,
     &  DSNC, FERDEPTH, FERMIXPERC,FLOOD, FRMETI, FRMETS, 
     &  RESDAX, ResDepth, ResMixPerc, SENESSUMC, SENESSUMN, SENESSUMP,
     &  TILDEP, TILLMIXPERC, MIXDEP, TLITC, TMETABC, 
     &  TSOM1C, TSOM2C, TSOM3C, TSOMC, TSTRUCC
      REAL DECS2(1), DECS3(1)

      REAL, DIMENSION(NAPPL*3) :: DDEP, DISTURBDEPTH
       
      REAL, DIMENSION(0:1) :: ACCCO2, CO2MET, DECMET, DECS1, DECSTR 
      REAL, DIMENSION(0:1) :: LIGSTR
      
      REAL, DIMENSION(NELEM) :: CEDAM, CES21T, CES23T, CES3M, CES3T
      REAL, DIMENSION(NELEM) :: CES3X, CESTR, FRDAE
      REAL, DIMENSION(NELEM) :: TLITE, TMETABE, TSOM1E, TSOM23E
      REAL, DIMENSION(NELEM) :: TSOM2E, TSOM3E, TSOME, TSTRUCE
      
      REAL, DIMENSION(NL) :: BD, CFS1S3, CFS2S1, CFS2S3, CFS3S1, CLAY
      REAL, DIMENSION(NL) :: CO2FS2, CO2FS3, DLAYR, DLTSOM23C, DLTSOM2C
      REAL, DIMENSION(NL) :: DLTSOM3C, DUL, KG2PPM, LL, NH4, NO3, S1S3
      REAL, DIMENSION(NL) :: S2S3, SAT, SILT, SOM23C, SOM2C, SOM3C
      REAL, DIMENSION(NL) :: SomLit, SPi_Labile, ST, SW, TXS1
      
      REAL, DIMENSION(0:NL) :: CFMETS1, CFS1S2, CFSTRS1, CFSTRS2
      REAL, DIMENSION(0:NL) :: CFSTRS23, CO2FMET, CO2FS1, CO2S1, DEFAC
      REAL, DIMENSION(0:NL) :: DLTLIGC, DLTMETABC, DLTSOM1C, DLTSTRUCC
      REAL, DIMENSION(0:NL) :: FRLSTR, LIGC, LITC, METABC, SOM1C
      REAL, DIMENSION(0:NL) :: SSOMC, STRUCC
      
      REAL, DIMENSION(0:0,NELEM) :: CES21I
      
      REAL, DIMENSION(0:1,NELEM) :: CES1M, CES1T, CES1X, CES21M, CES21S
      REAL, DIMENSION(0:1,NELEM) :: CES21X, CES23LM
      REAL, DIMENSION(0:1,NELEM) :: CES23LX, CES23M, CES23X
      REAL, DIMENSION(0:1,NELEM) :: CES2LI, CES2LM, CES2LS, CES2LX
      
      REAL, DIMENSION(NL,NELEM) :: AMINRL, CES2, CES3, DLTSOM23E
      REAL, DIMENSION(NL,NELEM) :: DLTSOM2E, DLTSOM3E
      
      REAL, DIMENSION(0:1,2) :: CO2STR(0:1,2)
      REAL, DIMENSION(0:NL,2) :: CO2FSTR(0:NL,2)
      REAL, DIMENSION(0:NL,NELEM) :: ACCMNR, CES1, CES21, CES23, CES23L
      REAL, DIMENSION(0:NL,NELEM) :: CES2L,DLTMETABE, DLTSOM1E
      REAL, DIMENSION(0:NL,NELEM) :: DLTSTRUCE, EFMETS1, EFS1S2, EFS1S23
      REAL, DIMENSION(0:NL,NELEM) :: LITE
      
      REAL 
     &  EFS1S3(NL,NELEM), EFS23S1(NL,NELEM),
     &  EFS2S1(NL,NELEM), EFS2S3(NL,NELEM), EFS3S1(NL,NELEM),
     &  EFSTRS1(0:NL,NELEM), EFSTRS2(0:NL,NELEM),
     &  EFSTRS23(0:NL,NELEM), IMM(0:NL,NELEM), IMMMETS1(0:NL,NELEM),   
     &  IMMOB(0:NL,NELEM), IMMS1S2(0:NL,NELEM), IMMS1S23(0:NL,NELEM), 
     &  IMMS1S3(NL,NELEM),IMMS23S1(NL,NELEM), IMMS2S1(NL,NELEM), 
     &  IMMS2S3(NL,NELEM), IMMS3S1(NL,NELEM), IMMSTRS1(0:NL,NELEM), 
     &  IMMSTRS2(0:NL,NELEM), IMMSTRS23(0:NL,NELEM), 
     &  METABE(0:NL,NELEM), MINER(0:NL,NELEM), MNR(0:NL,NELEM),
     &  MNRMETS1(0:NL,NELEM), MNRS1S2(0:NL,NELEM), MNRS1S23(0:NL,NELEM),
     &  MNRS1S3(NL,NELEM), MNRS23S1(NL,NELEM), MNRS2S1(NL,NELEM), 
     &  MNRS2S3(NL,NELEM), MNRS3S1(NL,NELEM), MNRSTRS1(0:NL,NELEM), 
     &  MNRSTRS2(0:NL,NELEM), MNRSTRS23(0:NL,NELEM), SOM1E(0:NL,NELEM), 
     &  SOM23E(NL,NELEM), SOM2E(NL,NELEM), SOM3E(NL,NELEM), 
     &  SSOME(0:NL,NELEM), STRUCE(0:NL,NELEM)
      REAL SomLitC(0:NL), SomLitE(0:NL,NELEM) 

!     Added for tillage
      REAL MIXPCT

!     Methane variables:
!     REAL CH4Consumption, CH4Emission, CH4Leaching, CH4Stored,
!    &    CO2emission, CumCH4Consumpt, CumCH4Emission, 
!    &    CumCH4Leaching, CumCO2Emission
      REAL RLV(NL), DRAIN
      TYPE (CH4_type) CH4_data

      DATA ADDMETABEFLAG /.FALSE./
      DATA FRMETFLAG /.FALSE./

!-----------------------------------------------------------------------
!     Constructed variables are defined in ModuleDefs.
      TYPE (ControlType) CONTROL
      TYPE (MulchType)   MULCH
      Type (ResidueType) HARVRES
      TYPE (ResidueType) SENESCE      !Senescence
      TYPE (SoilType)    SOILPROP
      TYPE (SwitchType)  ISWITCH
      TYPE (TillType)    TILLVALS     !Tillage
      TYPE (FertType)    FERTDATA     !Fertilizer
      TYPE (OrgMatAppType) OMAData    !Organic matter application
      TYPE (FloodNType)  FloodN       !Flood N
      TYPE (FloodWatType) FloodWat    !Flooded field info

!     Transfer values from constructed data types into local variables.
      CROP    = CONTROL % CROP
      DYNAMIC = CONTROL % DYNAMIC
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE
      YRDOY   = CONTROL % YRDOY

!     Check that this list is complete and that all are needed.
      BD     = SOILPROP % BD     
      CLAY   = SOILPROP % CLAY    
      DLAYR  = SOILPROP % DLAYR  
!      DMOD   = SOILPROP % DMOD
!      IF (DMOD < -1.E-6) CALL ERROR("DMOD",0,"SOILPROP",0)   
      DUL    = SOILPROP % DUL    
      KG2PPM = SOILPROP % KG2PPM  
      LL     = SOILPROP % LL     
      NLAYR  = SOILPROP % NLAYR  
      SAT    = SOILPROP % SAT    
      SILT   = SOILPROP % SILT    
  
      IDETL  = ISWITCH % IDETL

      FERTDAY = FERTDATA % FERTDAY
      ResDat  = OMADATA  % ResDat

!     Need to modify decomposition rates for flooded conditions
!     This has not been done yet!! CHP 6/6/2006
      FLOOD   = FLOODWAT % FLOOD

!     ------------------------------------------------------------------
!     Split YRDOY into YR and DOY.
      CALL YR_DOY (YRDOY, YEAR, DOY)

!***********************************************************************
!***********************************************************************
!     RUN INITIALIZATION - CALLED ONCE PER SIMULATION
!***********************************************************************
      IF (DYNAMIC == RUNINIT) THEN
!     ------------------------------------------------------------------
      ISWWAT = ISWITCH % ISWWAT
      IF (ISWWAT == 'N') THEN
        IMM     = 0.0
        LITC    = 0.0
        MNR     = 0.0
        SomLit  = 0.0
        SomLitC = 0.0
        SomLitE = 0.0
        SSOMC   = 0.0
        MULCH % MULCHMASS = 0.0
        RETURN
      ENDIF
        
      N_ELEMS = CONTROL % N_ELEMS

      CALL OPSOMLIT_C (CONTROL, ISWITCH,
     &  ACCCO2, LITC, LITE, METABC, METABE,               !Input
     &  N_ELEMS,OMAData, SOILPROP, SOM1C, SOM1E, SOM2C,   !Input
     &  SOM2E, SOM23E, SOM3C, SOM3E, SomLitC, SomLitE,    !Input
     &  STRUCC, STRUCE, TLITC, TLITE, TMETABC, TMETABE,   !Input
     &  TSOM1C, TSOM1E, TSOM2C, TSOM2E, TSOM23E,          !Input
     &  TSOM3C, TSOM3E, TSOMC, TSOME, TSTRUCC, TSTRUCE)   !Input

!***********************************************************************
!***********************************************************************
!     SEASONAL INITIALIZATION: run once per season.
!***********************************************************************
      ELSEIF (DYNAMIC == SEASINIT) THEN
!     ------------------------------------------------------------------
      IF (ISWWAT == 'N') RETURN

      DLTMETABC = 0.0
      DLTSTRUCC = 0.0
      DLTLIGC   = 0.0
      DLTSOM1C  = 0.0
      DLTMETABE = 0.0
      DLTSTRUCE = 0.0
      DLTSOM1E  = 0.0
      DLTSOM2C  = 0.0
      DLTSOM3C  = 0.0
      DLTSOM2E  = 0.0
      DLTSOM3E  = 0.0

      IMMOB = 0.
      MINER = 0.
      IMM = 0.
      MNR = 0.

      ACCCO2 = 0.0
      newCO2 = 0.

!     Set initial SOM and nitrogen conditions for each soil layer.
      CALL SoilCNPinit_C (CONTROL, ISWITCH,               !Input 
     &  N_ELEMS, SOILPROP,                                !Input
     &  ACCCO2, ACCMNR, ADDMETABEFLAG, AMINRL, CEDAM,     !Output
     &  CES1, CES1M, CES1T, CES1X, CES2, CES21I, CES21M,  !Output
     &  CES21S, CES21T, CES21X, CES23LM, CES23LX, CES23M, !Output
     &  CES23T, CES23X, CES2LI, CES2LM, CES2LS, CES2LX,   !Output
     &  CES3, CES3M, CES3T, CES3X, CESTR, CO2MET, CO2S1,  !Output
     &  CO2S2, CO2S3, CO2STR, CULMETQ, CULS1Q, CULS2Q,    !Output
     &  CULS3Q, CULSTRQ, DECMET, DECS1, DECS2, DECS3,     !Output
     &  DECSTR, DISTURBNUM, DISTURBEND, DISTURBDEPTH,     !Output
     &  DSNC, FRDAE, FRMETFLAG, FRMETI,                   !Output
     &  FRMETS,HARVRES, LIGC, LIGSTR, METABC, METABE,     !Output
     &  MULCH, RESDAX, S1S3, S2S3, SOM1C, SOM1E, SOM2C,   !Output
     &  SOM2E, SOM23C, SOM23E, SOM3C, SOM3E, SSOMC,       !Output
     &  STRUCC, STRUCE, TLITE, TXS1)                      !Output

!     Get detailed SOM output. Do this here and not only after adding
!     root and shoot residue, because otherwise one cannot check the
!     SOM content against the SOIL.SOL values (if residues and
!     SOM1(SRFC) are incorporated). First update the total amount of
!     SOM (all pools together).
      CALL TSOMLIT_C ( 
     &  METABC, METABE, N_ELEMS, NLAYR, SOILPROP,         !Input
     &  SOM1C, SOM1E, SOM2C, SOM2E, SOM23E, SOM3C,        !Input
     &  SOM3E, STRUCC, STRUCE,                            !Input
     &  LITC, LITE, MULCH, SSOMC, SSOME, TLITC, TLITE,    !Output
     &  TMETABC, TMETABE, SomLit, SomLitC, SomLitE,       !Output
     &  TSOM1C, TSOM1E, TSOM2C, TSOM2E, TSOM23E, TSOM3C,  !Output
     &  TSOM3E, TSOMC, TSOME, TSTRUCC, TSTRUCE)           !Output

!     Get detailed SOM and litter output for checking the SOM
!     initialization (litter has not yet been set and will be printed
!     from CENTURY section SEASINIT). Use RUN to prevent it from printing
!     again with a SEASINIT of a follow-up year in a sequential run.
      IF (INDEX('AD',IDETL) > 0 .AND. RUN == 1) THEN
        CALL SOMLITPRINT_C (CONTROL,
     &    DLAYR, LITC, LITE, METABC, METABE, N_ELEMS,     !Input
     &    NLAYR, SOM1C, SOM1E, SOM2C, SOM2E, SOM23E,      !Input
     &    SOM3C, SOM3E, SSOMC, SSOME, STRUCC, STRUCE,     !Input
     &    TLITC, TLITE, TMETABC, TMETABE, TSOM1C,         !Input
     &    TSOM1E, TSOM2C, TSOM2E, TSOM23E, TSOM3C,        !Input
     &    TSOM3E, TSOMC, TSOME, TSTRUCC, TSTRUCE)         !Input
      ENDIF

!CHP 05/17/2004 Moved from SoilCNPinit_C
!     Write headers and explanation of abbreviations for SOMLIT.OUT
!     (output file with the standard DSSAT output format, i.e. each
!     line represents one day).
      CALL OPSOMLIT_C (CONTROL, ISWITCH,
     &  ACCCO2, LITC, LITE, METABC, METABE,               !Input
     &  N_ELEMS,OMAData, SOILPROP, SOM1C, SOM1E, SOM2C,   !Input
     &  SOM2E, SOM23E, SOM3C, SOM3E, SomLitC, SomLitE,    !Input
     &  STRUCC, STRUCE, TLITC, TLITE, TMETABC, TMETABE,   !Input
     &  TSOM1C, TSOM1E, TSOM2C, TSOM2E, TSOM23E,          !Input
     &  TSOM3C, TSOM3E, TSOMC, TSOME, TSTRUCC, TSTRUCE)   !Input

      CALL SENESADD_C (SEASINIT, 
     &  AMINRL, CEDAM, CESTR, CROP, DLAYR, FRDAE,         !Input
     &  FRMETI, FRMETS, N_ELEMS, NLAYR, RESDAX,           !Input
     &  SENESCE,                                          !Input
     &  ADDMETABEFLAG, DLTLIGC, DLTMETABC, DLTMETABE,     !Output
     &  DLTSTRUCC, DLTSTRUCE, FRMETFLAG, IMMOB, MULCH,    !Output
     &  SENESSUMC, SENESSUMN, SENESSUMP)                  !Output

      IF (RUN == 1 .OR. INDEX('QF',RNMODE) <= 0) THEN
!       Variables for setting the enhanced SOM/litter decomposition
!       rate after soil disturbance (DOCULT flag).
        DISTURBDEPTHMAX = 0.
        DISTURBNUM = 0
        DISTURBENDMAX = 0
        DISTURBEND = 0
        DO L = 1, NLAYR
          DOCULT(L) = .FALSE.
        ENDDO

      ELSE   !RUN /= 1 or INDEX('QF',RNMODE) > 0
!       For sequenced runs, need to purge the DISTURB arrays of old data
!       First copy data to temporary arrays, then go through and keep
!         only current dates.  This is needed because in long sequences 
!       the number of disturbances can get very large.
!         (potentially DISTURBNUM = # sequences * NAPPL * 3).

!       Copy data to temporary arrays.
        DNUM = DISTURBNUM
        DEND = DISTURBEND
        DDEP = DISTURBDEPTH

!       Zero out disturbance arrays.
        DISTURBNUM = 0
        DISTURBEND = 0
        DISTURBDEPTH = 0.0

!       Copy back to disturbance arrays only current data.
        DO I = 1, DNUM
          IF (DEND(I) > YRDOY) THEN
            DISTURBNUM = DISTURBNUM + 1
            DISTURBEND(DISTURBNUM) = DEND(I)
            DISTURBDEPTH(DISTURBNUM) = DDEP(I)
          ENDIF   !End of IF block on DEND>YRDOY.
        ENDDO   !End of DO loop on I=1,DNUM
      ENDIF   !End of IF block on RUN == 1 .OR. INDEX('QF',RNMODE) <= 0.

!     The SOM variables were initialized as state variables, but the
!     litter variables as rate variables (because of RPLACE call).
!     Integrate the rate variables for the initialization, so that
!     output on litter initialization can be obtained.
      DO L = 0, NLAYR
        METABC(L) = METABC(L) + DLTMETABC(L)
        STRUCC(L) = STRUCC(L) + DLTSTRUCC(L)
        LIGC(L)   = AMIN1(LIGC(L) + DLTLIGC(L), STRUCC(L))

        DO IEL = 1, N_ELEMS
          METABE(L,IEL) = METABE(L,IEL) + DLTMETABE(L,IEL)
          STRUCE(L,IEL) = STRUCE(L,IEL) + DLTSTRUCE(L,IEL) 
        END DO   !End of DO loop on IEL.

!         Calculate FRLSTR from LIGC. (NOT FOR SEQUENCED RUNS!!)
        IF (RUN == 1 .OR. INDEX('QF',RNMODE) <= 0) THEN
          IF (STRUCC(L) > 0.001) THEN
            FRLSTR(L) = LIGC(L) / STRUCC(L)
          ELSE
            FRLSTR(L) = 0.0
          ENDIF
        ENDIF   !End of IF block on RUN==1.
      END DO   !End of soil layer loop.

!###AJG May 2005 Is called twice from SEASINIT ==> TSOMxx too big.
!       Take the soil-profile sum of the SOM and litter variables.
      CALL TSOMLIT_C ( 
     &  METABC, METABE, N_ELEMS, NLAYR, SOILPROP,         !Input
     &  SOM1C, SOM1E, SOM2C, SOM2E, SOM23E, SOM3C,        !Input
     &  SOM3E, STRUCC, STRUCE,                            !Input
     &  LITC, LITE, MULCH, SSOMC, SSOME, TLITC, TLITE,    !Output
     &  TMETABC, TMETABE, SomLit, SomLitC, SomLitE,       !Output
     &  TSOM1C, TSOM1E, TSOM2C, TSOM2E, TSOM23E, TSOM3C,  !Output
     &  TSOM3E, TSOMC, TSOME, TSTRUCC, TSTRUCE)           !Output

!       Get detailed SOM and litter output for checking the litter
!     initialization (SOM initialization was printed from SoilCNPinit_C
      IF (INDEX('AD',IDETL) > 0) THEN
        CALL SOMLITPRINT_C (CONTROL,
     &    DLAYR, LITC, LITE, METABC, METABE, N_ELEMS,     !Input
     &    NLAYR, SOM1C, SOM1E, SOM2C, SOM2E, SOM23E,      !Input
     &    SOM3C, SOM3E, SSOMC, SSOME, STRUCC, STRUCE,     !Input
     &    TLITC, TLITE, TMETABC, TMETABE, TSOM1C,         !Input
     &    TSOM1E, TSOM2C, TSOM2E, TSOM23E, TSOM3C,        !Input
     &    TSOM3E, TSOMC, TSOME, TSTRUCC, TSTRUCE)         !Input
      ENDIF

      CALL OpSoilOrg(CONTROL, ISWITCH, 
     &  DSNC, NLAYR, OMADATA, SOILPROP, SomLitC, SomLitE, !Input
     &  SSOMC, SSOME, TLITC, TLITE, TSOMC, TSOME)         !Input

!     Soil carbon balance.
!      CALL SOILCBAL_C (CONTROL, ISWITCH, 
!     &  ACCCO2, LITC, OMAData, SENESCE,                   !Input
!     &    SOM1C, TLITC, TSOMC, YRDOY)                     !Input

      CALL MethaneDynamics(CONTROL, ISWITCH, SOILPROP,        !Input
     &    FERTDATA, FLOODWAT, SW, RLV, newCO2, DRAIN,         !Input
     &    CH4_data)                                           !Output

      CALL SOILCBAL (CONTROL, ISWITCH, 
     &  CH4_data, HARVRES, LITC, OMAData, SENESCE,            !Input
     &  SSOMC, TLITC, TSOMC, YRDOY)                           !Input

      CALL SoilNoBal_C (CONTROL, ISWITCH, 
     &  HARVRES, IMM, LITE, MNR, NLAYR, OMAData,          !Input
     &  SENESCE, SOM1E, TLITE, TSOME, TSOM1E,             !Input
     &  TSOM2E, TSOM3E)                                   !Input

      CALL SoilPoBal_C (CONTROL, ISWITCH,
     &  HARVRES, IMM, LITE, MNR, NLAYR, OMAData,          !Input
     &  SENESCE, SOM1E, TLITE, TSOME, TSOM1E, TSOM23E)    !Input

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS 
!***********************************************************************
      ELSEIF (DYNAMIC == RATE) THEN
!     ------------------------------------------------------------------
      IF (ISWWAT == 'N') RETURN

!     Set variables to zero at the start of a new day.
      MIXDEP = 0.
      IMMOB = 0.
      MINER = 0.

!     Initialize soil C and N process rates for this time step. First
!     the variables that exist for both SRFC and soil layers, then
!     those that exist only for the soil layers.
      DLTSOM1C  = 0.0
      DLTSOM2C  = 0.0
      DLTSOM3C  = 0.0
      DLTMETABC = 0.0
      DLTSTRUCC = 0.0
      DLTLIGC   = 0.0

      DLTSOM1E  = 0.0
      DLTSOM2E  = 0.0
      DLTSOM23E = 0.0
      DLTSOM3E  = 0.0
      DLTMETABE = 0.0
      DLTSTRUCE = 0.0

!     From flood N routines:  
      DLTMETABE(1,1) = FLOODN % ALGFON

      DO L = 1, NLAYR   
        AMINRL(L,N) = (NO3(L) + NH4(L)) / KG2PPM(L)
        AMINRL(L,P) = SPi_Labile(L)
      ENDDO

!     -----------------------------------------------------------
!     Add organic matter applications
      IF (YRDOY == ResDAT) THEN
!       This routine now places organic matter into appropriate
!       pools.  Organic matter applications are determined by 
!       OM_Place in management module.
        CALL RPLACE_C (CONTROL,
     &    AMINRL, CEDAM, CESTR, DLAYR, FRDAE,             !Input
     &    FRMETI, FRMETS, N_ELEMS, NLAYR, OMADATA,        !Input
     &    RESDAX,                                         !Input
     &    DLTLIGC, DLTMETABC, DLTMETABE, DLTSTRUCC,       !Input/Output
     &    DLTSTRUCE, IMMOB, MULCH,                        !Input/Output
     &    ADDMETABEFLAG, FRMETFLAG)                       !Output

!       If some of the newly applied residue is worked into the soil,
!       then the surface litter and SOM1 that were already present will
!       also be worked in. Set a flag to activate this in subroutine
!       NTRANS. For initialization, this is not needed, because there
!       is no surface litter yet; for carry-over residues, this should
!       be done by a tillage event.
        ResDepth = OMADATA % ResDepth
        ResMixPerc = OMADATA % ResMixPerc
      ELSE
        ResDepth = 0.
        ResMixPerc = 0.
      ENDIF   !End of IF block on ResDat == YRDOY

!     Add the senesced plant material to the soil as residues.
      CALL SENESADD_C (RATE,
     &  AMINRL, CEDAM, CESTR, CROP, DLAYR, FRDAE,         !Input
     &  FRMETI, FRMETS, N_ELEMS, NLAYR, RESDAX,           !Input
     &  SENESCE,                                          !Input
     &  ADDMETABEFLAG, DLTLIGC, DLTMETABC, DLTMETABE,     !Output
     &  DLTSTRUCC, DLTSTRUCE, FRMETFLAG, IMMOB, MULCH,    !Output
     &  SENESSUMC, SENESSUMN, SENESSUMP)                  !Output

      IF (FERTDAY == YRDOY) THEN
        FERDEPTH = FERTDATA % FERDEPTH
        FERMIXPERC = FERTDATA % FERMIXPERC
      ELSE
        FERDEPTH = 0.
        FERMIXPERC = 0.
      ENDIF   !End of IF block on FERDAY===YRDOY.

      IF (YRDOY == TILLVALS % TILDATE) THEN
        TILDEP   = TILLVALS % TILDEP
        TILLMIXPERC = TILLVALS % TILRESINC
      ELSE
        TILDEP = 0.
        TILLMIXPERC = 0.
      ENDIF   !End of IF block on YRDOY==TILLVALS

!     ----------------------------------------------------------------
!     Surface litter incorporation due to tillage or incorporation of 
!     residue or fertilizer.
!     12/13/2007 CHP remove accelerated decomposition due to fertilizer
!         placement.
!      MIXDEP = AMAX1 (TILDEP, ResDepth, FERDEPTH)
!      MIXPCT = AMAX1 (TILLMIXPERC, ResMixPerc, FERMIXPERC)
      MIXDEP = AMAX1 (TILDEP, ResDepth)
      MIXPCT = AMAX1 (TILLMIXPERC, ResMixPerc)
      IF (MIXDEP > 1.E-3) THEN
!       The decomposition rate is increased for 30 days as a result 
!       of soil disturbance. Set the start and end date. 
!       Increase the soil-disturbance number by one.
        DISTURBNUM = DISTURBNUM + 1
        DISTURBEND(DISTURBNUM) = INCYD (YRDOY, 30)
        DISTURBDEPTH(DISTURBNUM) = MIXDEP 

!       Take the maximum of the soil- and surface-residue mixing
!       variables (depth and percentage).
        CALL INCORPOR_C ( 
     &  DLAYR, LIGC, METABC, METABE, N_ELEMS, NLAYR,      !Input
     &  SOM1C, SOM1E, STRUCC, STRUCE, MIXDEP, MIXPCT,     !Input
     &  DLTLIGC, DLTMETABC, DLTMETABE, DLTSOM1C,          !Output
     &  DLTSOM1E, DLTSTRUCC, DLTSTRUCE)                   !Output

!       ------------
!       C variables:
        CALL SoilMix (SOM1C, DLTSOM1C, 0, DLAYR, MIXPCT, NLAYR, MIXDEP)
        CALL SoilMix (SOM2C, DLTSOM2C, 1, DLAYR, MIXPCT, NLAYR, MIXDEP)
        CALL SoilMix (SOM3C, DLTSOM3C, 1, DLAYR, MIXPCT, NLAYR, MIXDEP)
        CALL SoilMix (METABC,DLTMETABC,0, DLAYR, MIXPCT, NLAYR, MIXDEP)
        CALL SoilMix (STRUCC,DLTSTRUCC,0, DLAYR, MIXPCT, NLAYR, MIXDEP)
        CALL SoilMix (LIGC,  DLTLIGC,  0, DLAYR, MIXPCT, NLAYR, MIXDEP)

!       ------------
!       E variables:
        DO IEL = 1, N_ELEMS
          CALL SoilMix(SOM1E, DLTSOM1E, 0,IEL,DLAYR,MIXPCT,NLAYR,MIXDEP)
          IF (IEL == 1) THEN
            CALL SoilMix(SOM2E,DLTSOM2E,1,IEL,DLAYR,MIXPCT,NLAYR,MIXDEP)
            CALL SoilMix(SOM3E,DLTSOM3E,1,IEL,DLAYR,MIXPCT,NLAYR,MIXDEP)
          ELSEIF (IEL == 2) THEN
            CALL SoilMix(
     &                 SOM23E,DLTSOM23E,1,IEL,DLAYR,MIXPCT,NLAYR,MIXDEP)
          ENDIF
          CALL SoilMix(METABE,DLTMETABE,0,IEL,DLAYR,MIXPCT,NLAYR,MIXDEP)
          CALL SoilMix(STRUCE,DLTSTRUCE,0,IEL,DLAYR,MIXPCT,NLAYR,MIXDEP)
        ENDDO
      ENDIF

!     Determine the maximum soil depth for which the decomposition-
!     rate enhancement holds.
      DO I = 1, DISTURBNUM
        IF (YRDOY <= DISTURBEND(I)) THEN
!         If the new tillage event starts before the decomposition-
!         rate enhancing effect of a previous tillage event is
!         terminated, then take the deepest soil-disturbance depth
!         of the two. 
          DISTURBDEPTHMAX = AMAX1 (DISTURBDEPTHMAX, DISTURBDEPTH(I))

!         Take the highest DISTURBEND. This is going to determine
!         until when the decomposition-rate enhancing effect holds.
          DISTURBENDMAX = MAX0 (DISTURBENDMAX, DISTURBEND(I))

        ELSEIF (YRDOY > DISTURBEND(I) .AND. DISTURBEND(I) /= 0) THEN
!         If the decomposition-rate enhancing effect of a previous
!         tillage event is terminated, set the depth back to zero, so
!         that another - possibly shallower - event can take over.
!         DISTURBEND = 0 means that an empty array element is reached.
          DISTURBDEPTHMAX = 0.

        ELSEIF (DISTURBEND(I) == 0) THEN
!         If an empty DISTURBEND array element is reached, then
!         there are no further DISTURBDEPTH data anymore to compare,
!         so jump out of the DO loop.
          EXIT
        ENDIF   !End of IF block on DISTURBEND.
      END DO   !End of DISTURBNUM loop.

!     ----------------------------------------------------------------
!     Decomposition-rate enhancement after soil disturbance.
!     ----------------------------------------------------------------
      DO L = 0, NLAYR
!       Set a flag that speeds up the decomposition rate for thirty
!       days, from tomorrow onwards. Only applies to the layers that
!       were disturbed.
        IF (YRDOY <= DISTURBENDMAX) THEN
          IF (L == SRFC) THEN
            DEPTH = 0.
            DOCULT(SRFC) = .TRUE.
          ELSE
            DEPTH = DEPTH + DLAYR(L)

            IF (DEPTH <= DISTURBDEPTHMAX) THEN
              DOCULT(L) = .TRUE.
            ELSE
              DOCULT(L) = .FALSE.
            ENDIF
          ENDIF   !End of IF block on soil layers.
        ELSE
          DOCULT(L) = .FALSE.
        ENDIF   !End of IF block on YRDOY <= DISTURBENDMAX
      END DO   !End of soil layer loop.

!     ----------------------------------------------------------------
!     Loop through soil layers for rate calculations
!     ----------------------------------------------------------------
      DO L = 0, NLAYR   
!       --------------------------------------------------------------
!       Litter + SOM decay
!       --------------------------------------------------------------
!       Calculate the decomposition rate parameter, as a function
!       of the environmental conditions.
        CALL DECRAT_C (ISWITCH,
     &    DLAYR, DUL, L, LL, SAT, ST, SW,                 !Input
     &    DEFAC)                                          !Output

!       Set the C:E ratios of the SOM and litter pools.
        CALL CE_RATIO_C ( 
     &    AMINRL, CES1M, CES1T, CES1X, CES21I, CES21M,    !Input
     &    CES21S, CES21T, CES21X,                         !Input
     &    CES23LM, CES23LX, CES23M,                       !Input
     &    CES23T, CES23X, CES2LI, CES2LM, CES2LS,         !Input
     &    CES2LX, CES3M, CES3T, CES3X, DLAYR, L, METABC,  !Input
     &    METABE, N_ELEMS, SOM1C, SOM1E, STRUCC, STRUCE,  !Input
     &    CES1, CES21, CES23, CES23L, CES2L, CES3)        !Output

!       Though AMINRL is in the subroutine parameter string, the SRFC
!       layer does not use AMINRL in LITDEC and SOMDEC. Thus there is
!       no need to accommodate the fact that the SRFC layer does not
!       have mineral E and uses the AMINRL of soil layer 1.

************************************************************************
*       The layer index of the variables refers to the layer where     *
*       the decomposition occurs, not the layer where it ends up.      *
*       Thus CES2L(SRFC,IEL) is SRFC lignin of which SOM2 is formed    *
*       which ends up in the layer 1, and CES2L(1,IEL) is lignin in    *
*       layer 1 of which SOM2 is formed, also in layer 1.              *
************************************************************************

!       Do the litter decomposition with the CENTURY-based litter
!       decomposition model.
        CALL LITDEC_C ( 
     &    CES1, CES23L, CO2MET, CO2STR, CULMETQ, CULSTRQ, !Input
     &    DECMET, DECSTR, DEFAC, DOCULT, FRLSTR, L,       !Input
     &    LIGSTR, METABC, METABE, N_ELEMS, STRUCC, STRUCE,!Input
     &    CFMETS1, CFSTRS1, CFSTRS2, CFSTRS23, CO2FMET,   !Output
     &    CO2FSTR, EFMETS1, EFSTRS1, EFSTRS2, EFSTRS23,   !Output
     &    IMMMETS1, IMMSTRS1, IMMSTRS2, IMMSTRS23,        !Output
     &    MNRMETS1, MNRSTRS1, MNRSTRS2, MNRSTRS23)        !Output

!       Do the SOM decomposition with the CENTURY-based SOM model.
        CALL SOMDEC_C ( 
     &    CES1, CES21, CES23, CES3, CO2S1, CO2S2, CO2S3,  !Input
     &    CULS1Q, CULS2Q, CULS3Q, DECS1, DECS2, DECS3,    !Input
     &    DEFAC, DOCULT, L, N_ELEMS, S1S3, S2S3,          !Input
     &    SOM1C, SOM1E, SOM23C, SOM23E, SOM2C, SOM2E,     !Input
     &    SOM3C, SOM3E, TXS1,                             !Input
     &    CFS1S2, CFS1S3, CFS2S1, CFS2S3, CFS3S1, CO2FS1, !Output
     &    CO2FS2, CO2FS3, EFS1S2, EFS1S23, EFS1S3,        !Output
     &    EFS23S1, EFS2S1, EFS2S3, EFS3S1, IMMS1S2,       !Output
     &    IMMS1S23, IMMS1S3, IMMS23S1, IMMS2S1, IMMS2S3,  !Output
     &    IMMS3S1, MNRS1S2, MNRS1S3, MNRS23S1, MNRS2S1,   !Output
     &    MNRS2S3, MNRS3S1)                               !Output
      END DO   !End of soil layer loop

!     ----------------------------------------------------------------
!     Limit immobilization to the NH4+NO3 that is available.
!     ----------------------------------------------------------------
!     In LITDEC and SOMDEC, the decomposition was calculated under
!     unrestrained conditions concerning mineral E. This means that
!     the immobilization may take away more mineral E than there is
!     in a soil layer. The E immobilization thus has to be limited,
!     taking into account all other processes that affect mineral E
!     (thus, for N, that affect DLTSNH4 and DLTSNO3).
      CALL IMMOBLIMIT_C (
     &  AMINRL, CFMETS1, CFS1S2, CFS1S3, CFS2S1,          !Input
     &  CFS2S3, CFS3S1, CFSTRS1, CFSTRS2, CO2FMET,        !Input
     &  CO2FS1, CO2FS2, CO2FS3, CO2FSTR, EFMETS1,         !Input
     &  EFS1S2, EFS1S23, EFS1S3, EFS23S1, EFS2S1,         !Input
     &  EFS2S3, EFS3S1, EFSTRS1, EFSTRS2, EFSTRS23,       !Input
     &  IMMMETS1, IMMOB, IMMS1S2, IMMS1S23, IMMS1S3,      !Input
     &  IMMS23S1, IMMS2S1, IMMS2S3, IMMS3S1,              !Input
     &  IMMSTRS1, IMMSTRS2, IMMSTRS23, MINER, MNRMETS1,   !Input
     &  MNRS1S2, MNRS1S23, MNRS1S3, MNRS23S1,             !Input
     &  MNRS2S1, MNRS2S3, MNRS3S1, MNRSTRS1,              !Input
     &  MNRSTRS2, MNRSTRS23, N_ELEMS, NLAYR,              !Input
     &  DLTLIGC, DLTMETABC, DLTMETABE,                    !Output
     &  DLTSOM1C, DLTSOM1E, DLTSOM23C, DLTSOM23E,         !Output
     &  DLTSOM2C, DLTSOM2E, DLTSOM3C, DLTSOM3E,           !Output
     &  DLTSTRUCC, DLTSTRUCE, IMM, MNR)                   !Output

!     Check for impossible SOM/residue values and other warnings.
      CALL NCHECK_C (CONTROL, 
     &  ADDMETABEFLAG, FRLSTR, FRMETFLAG, METABC,         !Input
     &  METABE, NLAYR, SOM1C, SOM1E, SOM2C,               !Input
     &  SOM2E, SOM23E, SOM3C, SOM3E, STRUCC, STRUCE)      !Input

!     New CO2 produced from decomposition today
      DO L = 0, NLAYR
        IF (L == SRFC) THEN
          newCO2(SRFC) = CO2FMET(SRFC) + CO2FSTR(SRFC,LIG) + 
     &      CO2FSTR(SRFC,NONLIG) + CO2FS1(SRFC)
        ELSE
          newCO2(L) = CO2FMET(L) + CO2FSTR(L,LIG) + CO2FSTR(L,NONLIG) + 
     &      CO2FS1(L) + CO2FS2(L) + CO2FS3(L)
        ENDIF   
      ENDDO

      CALL MethaneDynamics(CONTROL, ISWITCH, SOILPROP,        !Input
     &    FERTDATA, FLOODWAT, SW, RLV, newCO2, DRAIN,         !Input
     &    CH4_data)                                           !Output

!***********************************************************************
!***********************************************************************
!     DAILY INTEGRATION
!     First limit immobilization to what is available in the soil.
!***********************************************************************
      ELSEIF (DYNAMIC == INTEGR) THEN
!-----------------------------------------------------------------------
      IF (ISWWAT == 'N') RETURN

!-----------------------------------------------------------------------
!     Loop through soil layers for integration.
!     -----------------------------------------
!     Integrate soil C and E process rates for this time step. First
!     the variables that exist for both SRFC and soil layers, then
!     those that exist only for the soil layers.
!     ======
!     SOIL C
!     ======
      DO L = 0, NLAYR
        METABC(L) = METABC(L) + DLTMETABC(L)
        STRUCC(L) = STRUCC(L) + DLTSTRUCC(L)
        LIGC(L)   = AMIN1(LIGC(L) + DLTLIGC(L), STRUCC(L))
        SOM1C(L)  = SOM1C(L)  + DLTSOM1C(L) 

        IF (L /= SRFC) THEN
          SOM2C(L) = SOM2C(L) + DLTSOM2C(L)
          SOM3C(L) = SOM3C(L) + DLTSOM3C(L)
          SOM23C(L) = SOM23C(L) + DLTSOM23C(L)

!         Calculate FRLSTR from LIGC: only if new residue was added.
          IF (STRUCC(L) > 0.0001) THEN
            FRLSTR(L) = LIGC(L) / STRUCC(L)
          ELSE
            FRLSTR(L) = 0.0
          ENDIF
        ENDIF

        DO IEL = 1, N_ELEMS
          METABE(L,IEL) = METABE(L,IEL) + DLTMETABE(L,IEL)
          STRUCE(L,IEL) = STRUCE(L,IEL) + DLTSTRUCE(L,IEL) 
          SOM1E(L,IEL)  = SOM1E(L,IEL)  + DLTSOM1E(L,IEL)

!         Only for the soil layers.
          IF (L /= SRFC) THEN
            IF (IEL == N) THEN
              SOM2E(L,N) = SOM2E(L,N) + DLTSOM2E(L,N)
              SOM3E(L,N) = SOM3E(L,N) + DLTSOM3E(L,N)
            ELSEIF (IEL == P) THEN
              SOM23E(L,P) = SOM23E(L,P) + DLTSOM23E(L,P)
            ENDIF   !End of IF block on IEL== N or P
          ENDIF   !End of IF block on L/=SRFC.
        ENDDO   !End of DO loop on IEL.
      ENDDO   !End of soil layer loop.

!     ----------------------------------------
!     Accumulators for mineralization and CO2.
!     ----------------------------------------
      DO L = 0, NLAYR
        IF (L == SRFC) THEN
          DO IEL = 1, N_ELEMS
!           Accumulate mineralized E of the whole simulation period.
            ACCMNR(SRFC,IEL) = ACCMNR(SRFC,IEL) + MNR(SRFC,IEL)
          END DO   !End of IEL loop

        ELSEIF (L == 1) THEN
          DO IEL = 1, N_ELEMS
!           For layer 1, subtract the E mineralization by the SRFC
!           layer, because this E also adds to the topsoil layer but
!           is accumulated separately.
            ACCMNR(1,IEL) = ACCMNR(1,IEL) + MNR(1,IEL) - MNR(SRFC,IEL)
          END DO   !End of DO loop on IEL.

        ELSE
          DO IEL = 1, N_ELEMS
            ACCMNR(L,IEL) = ACCMNR(L,IEL) + MNR(L,IEL)
          END DO   !End of IEL loop
        ENDIF   !End of IF block on L == SRFC.

!       Accumulate CO2 of the whole simulation period.
        IF (L == SRFC) THEN
          ACCCO2(SRFC) = ACCCO2(SRFC) + CO2FMET(SRFC) +
     &        CO2FSTR(SRFC,LIG) + CO2FSTR(SRFC,NONLIG) + CO2FS1(SRFC)

!!         microbial respiration for N2O/N2 loss in DayCent   PG/chp
!          newCO2(SRFC) = CO2FMET(SRFC) + CO2FSTR(SRFC,LIG) + 
!     &        CO2FSTR(SRFC,NONLIG) + CO2FS1(SRFC)

        ELSE
          ACCCO2(SOIL) = ACCCO2(SOIL) + CO2FMET(L) + CO2FSTR(L,LIG) +
     &        CO2FSTR(L,NONLIG) + CO2FS1(L) + CO2FS2(L) + CO2FS3(L)

!!         microbial respiration for N2O/N2 loss in DayCent   PG
!          newCO2(L) = CO2FMET(L) + CO2FSTR(L,LIG) + CO2FSTR(L,NONLIG) + 
!     &        CO2FS1(L) + CO2FS2(L) + CO2FS3(L)

        ENDIF   !End of IF block on L == SRFC.
      END DO   !End of DO loop on L.

!     Update total amount of SOM and litter (all pools together).
      CALL TSOMLIT_C ( 
     &  METABC, METABE, N_ELEMS, NLAYR, SOILPROP,         !Input
     &  SOM1C, SOM1E, SOM2C, SOM2E, SOM23E, SOM3C,        !Input
     &  SOM3E, STRUCC, STRUCE,                            !Input
     &  LITC, LITE, MULCH, SSOMC, SSOME, TLITC, TLITE,    !Output
     &  TMETABC, TMETABE, SomLit, SomLitC, SomLitE,       !Output
     &  TSOM1C, TSOM1E, TSOM2C, TSOM2E, TSOM23E, TSOM3C,  !Output
     &  TSOM3E, TSOMC, TSOME, TSTRUCC, TSTRUCE)           !Output

!     Check for impossible SOM/residue values and other warnings.
      CALL NCHECK_C (CONTROL, 
     &  ADDMETABEFLAG, FRLSTR, FRMETFLAG, METABC,         !Input
     &  METABE, NLAYR, SOM1C, SOM1E, SOM2C,               !Input
     &  SOM2E, SOM23E, SOM3C, SOM3E, STRUCC, STRUCE)      !Input

      CALL MethaneDynamics(CONTROL, ISWITCH, SOILPROP,        !Input
     &    FERTDATA, FLOODWAT, SW, RLV, newCO2, DRAIN,         !Input
     &    CH4_data)                                           !Output

!     Soil carbon balance.
!      CALL SOILCBAL_C (CONTROL, ISWITCH, 
!     &  ACCCO2, LITC, OMAData, SENESCE,                   !Input
!     &    SOM1C, TLITC, TSOMC, YRDOY)                     !Input

      CALL SOILCBAL (CONTROL, ISWITCH, 
     &  CH4_data, HARVRES, LITC, OMAData, SENESCE,            !Input
     &  SSOMC, TLITC, TSOMC, YRDOY)                           !Input

!***********************************************************************
!***********************************************************************
!     OUTPUT
!***********************************************************************
      ELSEIF (DYNAMIC == OUTPUT) THEN
!---------------------------------------------------------------------
      IF (ISWWAT == 'N') RETURN

      CALL MethaneDynamics(CONTROL, ISWITCH, SOILPROP,        !Input
     &    FERTDATA, FLOODWAT, SW, RLV, newCO2, DRAIN,         !Input
     &    CH4_data)                                           !Output

!       Get detailed SOM and litter output.
      IF (INDEX('AD',IDETL) > 0) THEN
        CALL SOMLITPRINT_C (CONTROL,
     &    DLAYR, LITC, LITE, METABC, METABE, N_ELEMS,     !Input
     &    NLAYR, SOM1C, SOM1E, SOM2C, SOM2E, SOM23E,      !Input
     &    SOM3C, SOM3E, SSOMC, SSOME, STRUCC, STRUCE,     !Input
     &    TLITC, TLITE, TMETABC, TMETABE, TSOM1C,         !Input
     &    TSOM1E, TSOM2C, TSOM2E, TSOM23E, TSOM3C,        !Input
     &    TSOM3E, TSOMC, TSOME, TSTRUCC, TSTRUCE)         !Input
      ENDIF   !End of IF block on IDETL.

      CALL OPSOMLIT_C (CONTROL, ISWITCH,
     &  ACCCO2, LITC, LITE, METABC, METABE,               !Input
     &  N_ELEMS,OMAData, SOILPROP, SOM1C, SOM1E, SOM2C,   !Input
     &  SOM2E, SOM23E, SOM3C, SOM3E, SomLitC, SomLitE,    !Input
     &  STRUCC, STRUCE, TLITC, TLITE, TMETABC, TMETABE,   !Input
     &  TSOM1C, TSOM1E, TSOM2C, TSOM2E, TSOM23E,          !Input
     &  TSOM3C, TSOM3E, TSOMC, TSOME, TSTRUCC, TSTRUCE)   !Input

      CALL OpSoilOrg(CONTROL, ISWITCH, 
     &  DSNC, NLAYR, OMADATA, SOILPROP, SomLitC, SomLitE, !Input
     &  SSOMC, SSOME, TLITC, TLITE, TSOMC, TSOME)         !Input

      CALL SoilNoBal_C (CONTROL, ISWITCH, 
     &  HARVRES, IMM, LITE, MNR, NLAYR, OMAData,          !Input
     &  SENESCE, SOM1E, TLITE, TSOME, TSOM1E,             !Input
     &  TSOM2E, TSOM3E)                                   !Input

      CALL SoilPoBal_C (CONTROL, ISWITCH,
     &  HARVRES, IMM, LITE, MNR, NLAYR, OMAData,          !Input
     &  SENESCE, SOM1E, TLITE, TSOME, TSOM1E, TSOM23E)    !Input

      CALL SOILCBAL (CONTROL, ISWITCH, 
     &  CH4_data, HARVRES, LITC, OMAData, SENESCE,            !Input
     &  SSOMC, TLITC, TSOMC, YRDOY)                           !Input

!***********************************************************************
!***********************************************************************
!     SEASEND
!***********************************************************************
      ELSEIF (DYNAMIC == SEASEND) THEN
!     ------------------------------------------------------------------
      IF (ISWWAT == 'N') RETURN

      CALL MethaneDynamics(CONTROL, ISWITCH, SOILPROP,        !Input
     &    FERTDATA, FLOODWAT, SW, RLV, newCO2, DRAIN,         !Input
     &    CH4_data)                                           !Output

!     Close output files.
      IF (INDEX('AD',IDETL) > 0) THEN
        CALL SOMLITPRINT_C (CONTROL,
     &    DLAYR, LITC, LITE, METABC, METABE, N_ELEMS,     !Input
     &    NLAYR, SOM1C, SOM1E, SOM2C, SOM2E, SOM23E,      !Input
     &    SOM3C, SOM3E, SSOMC, SSOME, STRUCC, STRUCE,     !Input
     &    TLITC, TLITE, TMETABC, TMETABE, TSOM1C,         !Input
     &    TSOM1E, TSOM2C, TSOM2E, TSOM23E, TSOM3C,        !Input
     &    TSOM3E, TSOMC, TSOME, TSTRUCC, TSTRUCE)         !Input
      ENDIF   !End of IF block on IDETL.

      CALL OPSOMLIT_C (CONTROL, ISWITCH,
     &  ACCCO2, LITC, LITE, METABC, METABE,               !Input
     &  N_ELEMS,OMAData, SOILPROP, SOM1C, SOM1E, SOM2C,   !Input
     &  SOM2E, SOM23E, SOM3C, SOM3E, SomLitC, SomLitE,    !Input
     &  STRUCC, STRUCE, TLITC, TLITE, TMETABC, TMETABE,   !Input
     &  TSOM1C, TSOM1E, TSOM2C, TSOM2E, TSOM23E,          !Input
     &  TSOM3C, TSOM3E, TSOMC, TSOME, TSTRUCC, TSTRUCE)   !Input

      CALL OpSoilOrg(CONTROL, ISWITCH, 
     &  DSNC, NLAYR, OMADATA, SOILPROP, SomLitC, SomLitE, !Input
     &  SSOMC, SSOME, TLITC, TLITE, TSOMC, TSOME)         !Input

      CALL SoilNoBal_C (CONTROL, ISWITCH, 
     &  HARVRES, IMM, LITE, MNR, NLAYR, OMAData,          !Input
     &  SENESCE, SOM1E, TLITE, TSOME, TSOM1E,             !Input
     &  TSOM2E, TSOM3E)                                   !Input

      CALL SoilPoBal_C (CONTROL, ISWITCH,
     &  HARVRES, IMM, LITE, MNR, NLAYR, OMAData,          !Input
     &  SENESCE, SOM1E, TLITE, TSOME, TSOM1E, TSOM23E)    !Input

      CALL SOILCBAL (CONTROL, ISWITCH, 
     &  CH4_data, HARVRES, LITC, OMAData, SENESCE,            !Input
     &  SSOMC, TLITC, TSOMC, YRDOY)                           !Input

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------

      RETURN
      END   SUBROUTINE Century

!***********************************************************************
! NTRANS Variables
!
! BD(L)       Bulk density (g[soil] / cm3[soil])
! CUMRES      Cumulative amount of residue application (kg [res] / ha)
! DLAYR(L)    Soil thickness in layer L (cm)
! DMOD        Factor to adjust the mineralization rate RHMIN for certain  
!               atypical soils (range 0-1)
! DOY         Current day of simulation (d)
! DSNC        Depth to which C and N are integrated across all soil 
!               layers for output in CARBON.OUT (cm) 
! DUL(L)      Volumetric soil water content at drained upper limit in 
!               layer L (cm3/cm3)
! LL(L)       Volumetric soil water content in layer L at lower limit 
!               (cm3/cm3)
! NL          Maximum number of soil layers       
! NLAYR       Number of soil layers               
! SAT(L)      Volumetric soil water content in layer L at saturation
!             (cm3[H2O] / cm3[soil])
! SNH4(L)     Total extractable ammonium N in soil layer L (kg [N] / ha)
! SNO3(L)     Total extractable nitrate N in soil layer L (kg [N] / ha)
! ST(L)       Soil temperature by soil layer (oC)
! SW(L)       Today's Volumetric Soil Water Content in layer L (cm3/cm3)
! YEAR        Year of current date of simulation            
! YRDOY       Current day of simulation (YYDDD)
!***********************************************************************
