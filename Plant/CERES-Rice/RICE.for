C=======================================================================
C COPYRIGHT 1998-2021 
C                     DSSAT Foundation
C                     International Fertilizer Development Center
C                     University of Florida, Gainesville, Florida
C
C ALL RIGHTS RESERVED
C=======================================================================
C=======================================================================
C  CERES RICE UPLAND and LOWLAND N MODEL
C
C  May 1, 2003 CERES Rice Model V4.0
C
C  Developed by Singh, Ritchie, Alocilja and others
C  Nitrogen routines developed by Godwin, Jones, et al.
C  Lowland nitrogen routines developed by Singh and Godwin, IFDC
C  
C       Version 4.0
C
C         Upland and lowland water balance
C         Upland and lowland nitrogen balance
C         Effects of extreme temperatures on phenological development
C         Growth subroutine completely redone
C         Including tiller dynamic function and grain filling
C         Labile N Pool added to GROSUB
C         Modifications to NFAC, Nitrification and Nuptake routines
C         Support for multi-year and treatment runs
C         DSSAT strategy application
C         Climate Change Effect
C
C-----------------------------------------------------------------------
C  Revision history
C
C             Written - See above
C  08/07/1993 PWW Header revision and minor changes
C  08/29/2002 CHP/MUS Converted rice model to modular subroutine of CSM.
C  02/19/2003 CHP Converted dates to YRDOY format
C  03/12/2003 CHP Changed senescence variable to composite (SENESCE)
C                   as defined in ModuleDefs.for
C  04/01/2004 CHP/US New PHEFAC calculation
!  12/17/2004 CHP Modified HRESCeres call for harvest residue
!  04/02/2008 CHP/US Added P model
!  04/02/2008 US Added simple K model
!  04/24/2019 US/JF/CHP Replace G4, G5 with THOT, TCLDP, TCLDF
!  06/15/2022 CHP Added CropStatus
!  01/26/2023 CHP Reduce compile warnings: add EXTERNAL stmts, remove 
!                 unused variables, shorten lines. 
C=======================================================================

      SUBROUTINE RICE(CONTROL, ISWITCH,
     &    CO2, DAYL, EOP, FLOODWAT, HARVFRAC, NH4, NO3,   !Input
     &    SKi_Avail, SPi_AVAIL,                           !Input
     &    SOILPROP, SRAD, ST, SW, TMAX, TMIN, TRWUP,      !Input
     &    TWILEN, YRPLT,                                  !Input
     &    FLOODN,                                         !I/O
     &    CANHT, HARVRES, LAI, KUptake, MDATE, NSTRES,    !Output
     &    PORMIN, PUptake, RWUEP1, RWUMX, CropStatus,     !Output
     &    RLV, SENESCE, STGDOY, FracRts, UNH4, UNO3)      !Output

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
      USE FloodModule    ! which contain control information, soil
                         ! parameters, hourly weather data and flooded
                         ! conditions.
      IMPLICIT NONE
      EXTERNAL YR_DOY, RI_OPHARV, RI_PHENOL, RI_ROOTGR, RI_GROSUB, 
     &  RI_OPGROW, GNURSE, HRes_Ceres
      SAVE

      CHARACTER*1  ISWWAT, ISWNIT
      CHARACTER*2  CROP
      CHARACTER*10 STNAME(20)

      INTEGER DOY, DYNAMIC, EDATE, EMAT, CropStatus
      INTEGER ISDATE, ISTAGE, ITRANS
      INTEGER LEAFNO, MDATE, NDAT, NLAYR
      INTEGER YRDOY, YRNURSE, YEAR, YRPLT, YRSIM, YRSOW
      INTEGER STGDOY(20)

      REAL CANNAA,CANWAA,SEEDNI, CDTT_TP, CANHT
      REAL AGEFAC, APTNUP, BIOMAS, CO2, CUMDTT, DAYL, DTT, DYIELD
      REAL EOP, EP1, FERTILE, FLOOD
      REAL G2, GNUP, GRAINN, GRORT
      REAL LAI, MAXLAI, NSTRES
      REAL P1, P1T, P3, P4, PBIOMS, PHEFAC, PHINT
      REAL PLANTS, PLTPOP, PODWT, PORMIN, RLWR, ROOTN
      REAL RTDEP, RWUEP1, RWUMX
      REAL SDEPTH, SDTT_TP, SEEDRV
      REAL SKERWT, SRAD, STMWT, STOVER, STOVN, STOVWT
      REAL STRCOLD, STRESSW, STRHEAT, SUMDTT, SWFAC
      REAL TAGE, TANC, TBASE, TGROGRN, TILNO
      REAL TMAX, TMIN, TOTNUP, TRWUP, TSGRWT, TURFAC, CumNUptake
      REAL TWILEN, WSTRES, XGNP, XSTAGE, XST_TP

      REAL BWAH, SDWT, SDWTAH, TOPWT, WTNSD

      REAL GPP, GPSM, GRNWT, LFWT, PANWT, RTWT
      REAL HARVFRAC(2)

      REAL  SI3(6)
      REAL, DIMENSION(NL) :: DLAYR, DUL, DS, LL, NH4, NO3
      REAL, DIMENSION(NL) :: RLV, ST, SW, UNH4, UNO3

      LOGICAL FIELD, LTRANS, NEW_PHASE, TF_GRO, BUNDED
      INTEGER DAS

!     P variables!    
      REAL PUptake(NL), SPi_AVAIL(NL), FracRts(NL)       
      REAL SeedFrac, VegFrac, PSTRES1, PSTRES2
      REAL PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed
!     g/plant to g/m2 
	REAL RTWTO, STMWTO, WTLF                     

!     Added for K model   
      REAL KUptake(NL), SKi_AVAIL(NL), KSTRES


 
C-----------------------------------------------------------------------
C     Initialize
C-----------------------------------------------------------------------

!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType)  CONTROL
      TYPE (SoilType)     SOILPROP
      TYPE (SwitchType)   ISWITCH
      TYPE (FloodWatType) FLOODWAT
      TYPE (FloodNType)   FloodN
      Type (ResidueType)  HARVRES
      Type (ResidueType)  SENESCE

!     Transfer values from constructed data types into local variables.
      CROP    = CONTROL % CROP
      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM
      
      DLAYR  = SOILPROP % DLAYR  
      DS     = SOILPROP % DS     
      DUL    = SOILPROP % DUL 
      LL     = SOILPROP % LL     
      NLAYR  = SOILPROP % NLAYR  
   
      ISWWAT = ISWITCH % ISWWAT
      ISWNIT = ISWITCH % ISWNIT

      FLOOD  = FLOODWAT % FLOOD
      BUNDED = FLOODWAT % BUNDED
      CALL YR_DOY(YRDOY, YEAR, DOY)

!***********************************************************************
!***********************************************************************
!     Run initialization - run once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN

      CALL RI_OPHARV (CONTROL, ISWITCH, 
     &    AGEFAC, APTNUP, BIOMAS, CANNAA, CANWAA,         !Input
     &    DYIELD, G2, GNUP, GPP, GPSM, GRAINN, GRNWT,     !Input
     &    HARVFRAC, ISDATE, ISTAGE, LAI, LEAFNO, MAXLAI,  !Input
     &    MDATE, NSTRES, PANWT, PBIOMS, PLANTS, PLTPOP,   !Input
     &    PSTRES1, PSTRES2,                               !Input
     &    SKERWT, STGDOY, STNAME, STOVER, STOVN, SWFAC,   !Input
     &    TILNO, TOTNUP, TURFAC, XGNP, YRPLT,             !Input
     &    BWAH, PODWT, SDWT, SDWTAH, TOPWT, WTNSD)        !Output

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
C     Call PLANT initialization routine to set variables to 0
C-----------------------------------------------------------------------
      SWFAC  = 1.0
      TURFAC = 1.0
      PHEFAC = 1.0

!     CHP 5/18/2011
      MDATE      = -99      

!     Temporary -- until P is integrated with this crop module  US 4/2/08
!     PConc_Shut = 0.0
!      PConc_Root = 0.0
!      PConc_Shel = 0.0
!      PConc_Seed = 0.0

C-----------------------------------------------------------------------
C     Determine Seedling Nursery period
C-----------------------------------------------------------------------
!      CALL YR_DOY (YRPLT, YR, ISOW)
      YRSOW = YRPLT

      !Initialize phenology
      CALL RI_PHENOL (CONTROL, ISWITCH, 
     &    AGEFAC, BIOMAS, DAYL, LEAFNO, NSTRES, PHEFAC,   !Input
     &    PHINT, SDEPTH, SOILPROP, SRAD, SW, SWFAC,       !Input
     &    TGROGRN, TILNO, TMAX, TMIN, TWILEN, TURFAC,     !Input
     &    YRPLT,FLOODWAT, LAI,                            !Input
     &    CUMDTT, EMAT, ISDATE, PLANTS, RTDEP, YRSOW,     !I/O
     &    CDTT_TP, DTT, FERTILE, FIELD, ISTAGE,           !Output
     &    ITRANS, LTRANS, MDATE, NDAT, NEW_PHASE, P1, P1T,!Output
     &    P3, P4, SDTT_TP, SEEDNI, SI3, STGDOY, STNAME,   !Output
     &    STRCOLD, STRESSW, STRHEAT, SUMDTT, TAGE,        !Output
     &    TBASE, TF_GRO, TSGRWT, WSTRES, XSTAGE, XST_TP,  !Output
     &    SeedFrac, VegFrac, CropStatus)                  !Output

      CALL RI_ROOTGR (CONTROL, 
     &    DTT, FLOOD, GRORT, ISWNIT, ISWWAT,              !Input
     &    ITRANS, NH4, NO3, PLANTS, RLWR, SOILPROP,       !Input
     &    SUMDTT, SW, SWFAC, YRDOY, YRPLT,                !Input
     &    FLOODN, RTWT,                                   !I/O
     &    RLV, RTDEP, SDEPTH)                             !Output

      CALL RI_GROSUB (CONTROL, ISWITCH,                  !Input 
     &    CO2, CDTT_TP, CUMDTT, DTT, FERTILE, FIELD,      !Input
     &    FLOOD, FracRts, ISTAGE, ITRANS, LTRANS,         !Input
     &    NEW_PHASE, NH4, NO3, P1, P1T, P3, P4, PHEFAC,   !Input     
     &    RLV, RTDEP, SDEPTH, SDTT_TP, SeedFrac,          !Input
     &    SI3, SOILPROP, SKi_AVAIL, SPi_AVAIL, SRAD, ST,  !Input
     &    STRCOLD, STRESSW, STRHEAT, SUMDTT, SW, SWFAC,   !Input
     &    TAGE, TBASE, TF_GRO, TMAX, TMIN, TSGRWT,        !Input
     &    TURFAC, VegFrac, WSTRES, XSTAGE, XST_TP, YRPLT, !Input
     &    YRSOW,                                          !Input
     &    EMAT, FLOODN, PLANTS, RTWT,                     !I/O
     &    AGEFAC, APTNUP, BIOMAS, CANNAA, CANWAA, DYIELD, !Output
     &    GNUP, GPP, GPSM, GRAINN, GRNWT, GRORT,          !Output
     &    KUptake, KSTRES, LAI,                           !Output
     &    LEAFNO, LFWT, MAXLAI, NSTRES, PANWT, PBIOMS,    !Output
     &    PHINT, PLTPOP, PConc_Root, PConc_Seed,          !Output
     &    PConc_Shel, PConc_Shut, PORMIN, PSTRES1,        !Output
     &    PSTRES2, PUptake, RLWR, ROOTN, RTWTO, RWUEP1,   !Output
     &    RWUMX, SEEDNI, SEEDRV, SENESCE,                 !Output
     &    SKERWT, STMWT, STMWTO,                          !Output
     &    STOVER, STOVN, TANC, TGROGRN, TILNO, TOTNUP,    !Output
     &    CumNUptake, UNH4, UNO3, WTLF, XGNP)             !Output
     
      CALL RI_OPGROW (CONTROL, ISWITCH, SOILPROP,
     &    BIOMAS, GPP, GPSM, GRAINN, GRNWT, ISTAGE, LAI,  
     &    LEAFNO, LFWT, MDATE, NLAYR, NSTRES, PANWT, PLANTS,
     &    PLTPOP, RLV, ROOTN, RTDEP, RTWT, SENESCE,       
     &    STMWT, STOVN, SWFAC, TILNO, CumNUptake, TURFAC, YRPLT,      
     &    CANHT, KSTRES, DTT)                                  

      CALL RI_OPHARV (CONTROL, ISWITCH, 
     &    AGEFAC, APTNUP, BIOMAS, CANNAA, CANWAA,         !Input
     &    DYIELD, G2, GNUP, GPP, GPSM, GRAINN, GRNWT,     !Input
     &    HARVFRAC, ISDATE, ISTAGE, LAI, LEAFNO, MAXLAI,  !Input
     &    MDATE, NSTRES, PANWT, PBIOMS, PLANTS, PLTPOP,   !Input
     &    PSTRES1, PSTRES2,                               !Input
     &    SKERWT, STGDOY, STNAME, STOVER, STOVN, SWFAC,   !Input
     &    TILNO, TOTNUP, TURFAC, XGNP, YRPLT,             !Input
     &    BWAH, PODWT, SDWT, SDWTAH, TOPWT, WTNSD)        !Output

!     NOTE: TAGE is initialized in PHENOL
      CALL GNURSE (ITRANS, TAGE, YRPLT, YRSIM,            !Input
     &             YRSOW, YRNURSE)                        !Output

!***********************************************************************
!***********************************************************************
!     Daily rate / integration calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
C-----------------------------------------------------------------------
C        Define dates for water balance calculations
C        YRSIM = Start of Simulation Date
C        YRPLT = Planting Date
C        EDATE = Emergence Date
C        MDATE = Maturity Date
C        YRDOY = Year - Day of Year (Dynamic Variable)
C-----------------------------------------------------------------------
!     Zero N, P and K uptake arrays (NUPTAK not always called)
        UNO3 = 0.0
        UNH4 = 0.0
	  PUptake = 0.0
	  KUptake = 0.0

         SELECT CASE (ITRANS)
           CASE (1);    EDATE = STGDOY (09)
           CASE (2, 3); EDATE = STGDOY (11)
           CASE (4);    EDATE = STGDOY (10)
         END SELECT

!     Calculate daily stresses (from SWFACS)
      IF (ISWWAT .EQ. 'Y') THEN
        SWFAC  = 1.0
        TURFAC  = 1.0
        PHEFAC = 1.0
        IF (EOP .GT. 0.0 .AND. YRDOY .GT. EDATE .AND. EDATE .GT. 0) THEN
          EP1 = EOP * 0.1
          IF (TRWUP / EP1 .LT. 3.25 .AND. FLOOD .LE. 0.0) THEN
            !PHEFAC = (1.0 / 3.25)*(TRWUP / EP1) - 0.14
            !PHEFAC = AMAX1 (PHEFAC, 0.001)
             PHEFAC = 1.1/(1+EXP(-(TRWUP/EP1-2.1)/0.5))  !Mar 2004 -- US
             PHEFAC = AMIN1(AMAX1 (PHEFAC, 0.001), 1.0)
          ENDIF
          IF (TRWUP / EP1 .LT. RWUEP1) THEN
            TURFAC = (1. / RWUEP1) * TRWUP / EP1
          ENDIF
          IF (EP1 .GE. TRWUP) THEN
            SWFAC = TRWUP / EP1
          ENDIF
        ENDIF

        !PHEFAC = 1.0

       !WRESR growth and depth routine
        CALL RI_ROOTGR (CONTROL, 
     &    DTT, FLOOD, GRORT, ISWNIT, ISWWAT,              !Input
     &    ITRANS, NH4, NO3, PLANTS, RLWR, SOILPROP,       !Input
     &    SUMDTT, SW, SWFAC, YRDOY, YRPLT,                !Input
     &    FLOODN, RTWT,                                   !I/O
     &    RLV, RTDEP, SDEPTH)                             !Output
      ENDIF

C-----------------------------------------------------------------------
C        Call Phenology routine unless in fallow
C-----------------------------------------------------------------------

      IF (YRPLT .NE. 9999999) THEN
        IF (YRDOY .EQ. YRPLT .OR. ISTAGE .NE. 7) THEN
          CALL RI_PHENOL (CONTROL, ISWITCH, 
     &    AGEFAC, BIOMAS, DAYL, LEAFNO, NSTRES, PHEFAC,   !Input
     &    PHINT, SDEPTH, SOILPROP, SRAD, SW, SWFAC,       !Input
     &    TGROGRN, TILNO, TMAX, TMIN, TWILEN, TURFAC,     !Input
     &    YRPLT,FLOODWAT, LAI,                            !Input
     &    CUMDTT, EMAT, ISDATE, PLANTS, RTDEP, YRSOW,     !I/O
     &    CDTT_TP, DTT, FERTILE, FIELD, ISTAGE,           !Output
     &    ITRANS, LTRANS, MDATE, NDAT, NEW_PHASE, P1, P1T,!Output
     &    P3, P4, SDTT_TP, SEEDNI, SI3, STGDOY, STNAME,   !Output
     &    STRCOLD, STRESSW, STRHEAT, SUMDTT, TAGE,        !Output
     &    TBASE, TF_GRO, TSGRWT, WSTRES, XSTAGE, XST_TP,  !Output
     &    SeedFrac, VegFrac, CropStatus)                  !Output
         ENDIF
!	WRITE(999,*) ' DAY=',YRDOY, 'VEG=',VEGFRAC, 'SEED=',SEEDFRAC
      ENDIF

C--------------------------------------------------------------
C        Call Growth and Root Growth Routines
C--------------------------------------------------------------

      CALL RI_GROSUB (CONTROL, ISWITCH,                   !Input  
     &    CO2, CDTT_TP, CUMDTT, DTT, FERTILE, FIELD,      !Input
     &    FLOOD, FracRts, ISTAGE, ITRANS, LTRANS,         !Input
     &    NEW_PHASE, NH4, NO3, P1, P1T, P3, P4, PHEFAC,   !Input     
     &    RLV, RTDEP, SDEPTH, SDTT_TP, SeedFrac,          !Input
     &    SI3, SOILPROP, SKi_AVAIL, SPi_AVAIL, SRAD, ST,  !Input
     &    STRCOLD, STRESSW, STRHEAT, SUMDTT, SW, SWFAC,   !Input
     &    TAGE, TBASE, TF_GRO, TMAX, TMIN, TSGRWT,        !Input
     &    TURFAC, VegFrac, WSTRES, XSTAGE, XST_TP, YRPLT, !Input
     &    YRSOW,                                          !Input
     &    EMAT, FLOODN, PLANTS, RTWT,                     !I/O
     &    AGEFAC, APTNUP, BIOMAS, CANNAA, CANWAA, DYIELD, !Output
     &    GNUP, GPP, GPSM, GRAINN, GRNWT, GRORT,          !Output
     &    KUptake, KSTRES, LAI,                           !Output
     &    LEAFNO, LFWT, MAXLAI, NSTRES, PANWT, PBIOMS,    !Output
     &    PHINT, PLTPOP, PConc_Root, PConc_Seed,          !Output
     &    PConc_Shel, PConc_Shut, PORMIN, PSTRES1,        !Output
     &    PSTRES2, PUptake, RLWR, ROOTN, RTWTO, RWUEP1,   !Output
     &    RWUMX, SEEDNI, SEEDRV, SENESCE,                 !Output
     &    SKERWT, STMWT, STMWTO,                          !Output
     &    STOVER, STOVN, TANC, TGROGRN, TILNO, TOTNUP,    !Output
     &    CumNUptake, UNH4, UNO3, WTLF, XGNP)             !Output


      FLOODN % NDAT   = NDAT

!***********************************************************************
!***********************************************************************
!     Daily output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
       CALL RI_GROSUB (CONTROL, ISWITCH,                   !Input  
     &    CO2, CDTT_TP, CUMDTT, DTT, FERTILE, FIELD,      !Input
     &    FLOOD, FracRts, ISTAGE, ITRANS, LTRANS,         !Input
     &    NEW_PHASE, NH4, NO3, P1, P1T, P3, P4, PHEFAC,   !Input     
     &    RLV, RTDEP, SDEPTH, SDTT_TP, SeedFrac,          !Input
     &    SI3, SOILPROP, SKi_AVAIL, SPi_AVAIL, SRAD, ST,  !Input
     &    STRCOLD, STRESSW, STRHEAT, SUMDTT, SW, SWFAC,   !Input
     &    TAGE, TBASE, TF_GRO, TMAX, TMIN, TSGRWT,        !Input
     &    TURFAC, VegFrac, WSTRES, XSTAGE, XST_TP, YRPLT, !Input
     &    YRSOW,                                          !Input
     &    EMAT, FLOODN, PLANTS, RTWT,                     !I/O
     &    AGEFAC, APTNUP, BIOMAS, CANNAA, CANWAA, DYIELD, !Output
     &    GNUP, GPP, GPSM, GRAINN, GRNWT, GRORT,          !Output
     &    KUptake, KSTRES, LAI,                           !Output
     &    LEAFNO, LFWT, MAXLAI, NSTRES, PANWT, PBIOMS,    !Output
     &    PHINT, PLTPOP, PConc_Root, PConc_Seed,          !Output
     &    PConc_Shel, PConc_Shut, PORMIN, PSTRES1,        !Output
     &    PSTRES2, PUptake, RLWR, ROOTN, RTWTO, RWUEP1,   !Output
     &    RWUMX, SEEDNI, SEEDRV, SENESCE,                 !Output
     &    SKERWT, STMWT, STMWTO,                          !Output
     &    STOVER, STOVN, TANC, TGROGRN, TILNO, TOTNUP,    !Output
     &    CumNUptake, UNH4, UNO3, WTLF, XGNP)             !Output

      CALL RI_OPGROW (CONTROL, ISWITCH, SOILPROP,
     &    BIOMAS, GPP, GPSM, GRAINN, GRNWT, ISTAGE, LAI,  
     &    LEAFNO, LFWT, MDATE, NLAYR, NSTRES, PANWT, PLANTS,
     &    PLTPOP, RLV, ROOTN, RTDEP, RTWT, SENESCE,       
     &    STMWT, STOVN, SWFAC, TILNO, CumNUptake, TURFAC, YRPLT,      
     &    CANHT, KSTRES, DTT)                                  

      CALL RI_OPHARV (CONTROL, ISWITCH, 
     &    AGEFAC, APTNUP, BIOMAS, CANNAA, CANWAA,         !Input
     &    DYIELD, G2, GNUP, GPP, GPSM, GRAINN, GRNWT,     !Input
     &    HARVFRAC, ISDATE, ISTAGE, LAI, LEAFNO, MAXLAI,  !Input
     &    MDATE, NSTRES, PANWT, PBIOMS, PLANTS, PLTPOP,   !Input
     &    PSTRES1, PSTRES2,                               !Input
     &    SKERWT, STGDOY, STNAME, STOVER, STOVN, SWFAC,   !Input
     &    TILNO, TOTNUP, TURFAC, XGNP, YRPLT,             !Input
     &    BWAH, PODWT, SDWT, SDWTAH, TOPWT, WTNSD)        !Output

!***********************************************************************
!***********************************************************************
!     Seasonal output
!***********************************************************************
        IF (DYNAMIC .EQ. SEASEND) THEN
          IF (PLTPOP .GT. .00001) THEN
            STOVWT = STOVER / PLTPOP / 10.0 
!           g/pl     kg/ha   pl/m2
          ELSE
            STOVWT = 0.0
          ENDIF

          CALL HRes_Ceres(CONTROL,
     &    CROP, DLAYR, GRNWT, HARVFRAC, NLAYR,            !Input
     &    PConc_Shut, PConc_Root, PConc_Shel,             !Input
     &    PConc_Seed, PLTPOP, PODWT, RLV, ROOTN,          !Input
     &    RTWT, SENESCE, STOVN, STOVWT, WTNSD,            !Input
     &    HARVRES)                                        !Output

          !Set senescence variable to zero for next season
          SENESCE % ResWt  = 0.0
          SENESCE % ResLig = 0.0
          SENESCE % ResE   = 0.0
        ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF

!***********************************************************************
      RETURN
      END SUBROUTINE RICE
