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
C

C  S U B S T O R   TARO    M O D E L
C
C  Jan 5, 2004 Substor Taro Model V4.0
C
C  Developed by U.Singh, R. Goenaga, R. Ogoshi, and H.K. Prasad

C  Nitrogen routines developed by Godwin, Jones, et al.
C  Lowland nitrogen routines developed by Singh and Godwin, IFDC

C  A COLLABORATIVE EFFORT AMONG IBSNAT, SPRAD-UH, USP, MPI (Fiji)
C  USDA-ARS (Puerto Rico), IFDC and MSU.
C
C       Version 4.0
C
C         Crop establishment options for taro
C         Upland and lowland water and nitrogen balance
C         Growth subroutine completely redone
C         Including tiller/sucker dynamic function and corm and cormel growth
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
!  06/15/2022 CHP Added CropStatus
!  01/26/2023 CHP Reduce compile warnings: add EXTERNAL stmts, remove 
!                 unused variables, shorten lines. 
C=======================================================================

      SUBROUTINE TR_SUBSTOR(CONTROL, ISWITCH,
     &    CO2, DAYL, EOP, FLOODWAT, HARVFRAC, NH4, NO3,   !Input
     &    SOILPROP, SRAD, ST, SW, TMAX, TMIN, TRWUP,      !Input
     &    YRPLT,                                          !Input
     &    FLOODN,                                         !I/O
     &    CropStatus,                                     !Output
     &    CANHT, HARVRES, LAI, MDATE, NSTRES, PORMIN,     !Output
     &    RWUEP1, RWUMX, RLV, SENESCE, STGDOY, UNH4, UNO3)!Output

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
      USE FloodModule    ! which contain control information, soil
                         ! parameters, hourly weather data and flooded
                         ! conditions.
      IMPLICIT NONE
      EXTERNAL YR_DOY, TR_OPGROW, TR_OPHARV, TR_PHENOL, TR_ROOTGR, 
     &  TR_GROSUB, GNURSE, HRes_Ceres
      SAVE

      CHARACTER*1  ISWWAT, ISWNIT
      CHARACTER*2  CROP
      CHARACTER*10 STNAME(20)

      INTEGER DOY, DYNAMIC, EDATE, EMAT, CropStatus
      INTEGER ISDATE, ISTAGE, ITRANS
      INTEGER LEAFNO, MDATE, NDAT, NLAYR
      INTEGER YRDOY, YRNURSE, YEAR, YRPLT, YRSIM, YRSOW
      INTEGER STGDOY(20)
      INTEGER LEAF1, LEAF3, LEAFT

      REAL CANNAA,CANWAA,SEEDNI, CDTT_TP, CANHT
      REAL AGEFAC, APTNUP, BIOMAS, CO2, CUMDTT, DAYL, DTT, DYIELD
      REAL EOP, EP1, FLOOD
      REAL G2, GRORT
      REAL LAI, MAXLAI, NSTRES
      REAL P1, P1T, P3, P4, PBIOMS, PHINT
      REAL PLANTS, PLTPOP, PORMIN, RLWR, ROOTN
      REAL RTDEP, RWUEP1, RWUMX
      REAL SDEPTH, SDTT_TP, SEEDRV
      REAL SKERWT, SRAD, STOVER, STOVN
      REAL SUMDTT, SWFAC
      REAL TAGE, TANC, TBASE, TILNO
      REAL TMAX, TMIN, TOTNUP, TRWUP, TURFAC
      REAL WSTRES, XSTAGE, XST_TP

      REAL BWAH, SDWT, SDWTAH, TOPWT, WTNSD

      REAL LFWT, RTWT, CARBO
      REAL HARVFRAC(2)

      REAL  SI3(6)
      REAL, DIMENSION(NL) :: DLAYR, NH4, NO3
      REAL, DIMENSION(NL) :: RLV, ST, SW, UNH4, UNO3

      REAL BF, CARBC1, CARBC2, CARBC3, CARBR1, CARBR2, CARBR3, CARBR4
      REAL CARBL0, CARBL1, CARBL2, CARBL3, CARBL4, CMAT2, CMAT5, G3, G4
      REAL KLITE, MCORMWT, MLFWT, MPETWT, PCGRD
      REAL PCINT, PETGR1, PETGR2, PETGR5, PLAMAX, RCONST, RCTFAC, RESPF
      REAL RGCORM2, RNINT, RPET, RSENCE, RUEA, SLA3, SLA4, SLA5, SLAIFC
      REAL SLFCGRD, SLFNGRD, SLFNINT, SLFTGRD, SLFTINT, SLFWGRD, SLFWINT
      REAL SLGRD1, SLGRD2, SUFAC2, SUFAC3, TCARBC2, TGRAD, TIPGRAD
      REAL TIPGRAD3, TIPINT, XNFGRAD, XNFINT, XTNMAX
!     REAL TCORMWT    ! addition RMO

      REAL CORMNUP, CORMLNO, CORMN, CORMWT, PCORMN, TGROCOM
      REAL PODWT, GRNWT

!     For output to PlantGro.OUT
      REAL DEADLF, SATFAC, PETWT, STOVWT, WTNCAN   !TUBN, TUBWT, 
      REAL WTNUP, XLAI

      LOGICAL FIELD, LTRANS, NEW_PHASE, TF_GRO

!     P variables
      REAL PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed

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
      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM

      DLAYR  = SOILPROP % DLAYR  
      NLAYR  = SOILPROP % NLAYR  
   
      ISWWAT = ISWITCH % ISWWAT
      ISWNIT = ISWITCH % ISWNIT

      FLOOD  = FLOODWAT % FLOOD

      CALL YR_DOY(YRDOY, YEAR, DOY)

!***********************************************************************
!***********************************************************************
!     Run initialization - run once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN

      CALL TR_OPGROW (CONTROL, ISWITCH, SOILPROP, 
     &    BIOMAS, CORMWT, DEADLF, DTT, ISTAGE,            !Input
     &    LAI, LFWT, MDATE, NLAYR, NSTRES, PLTPOP,        !Input
     &    RLV, ROOTN, RTDEP, RTWT, SATFAC, SENESCE,       !Input
     &    PETWT, STOVN, STOVWT, SWFAC, CORMN      ,       !Input
     &    MCORMWT, CORMLNO,                               !Input
     &    TURFAC, WTNCAN, WTNUP, XLAI, YRPLT)             !Input

      CALL TR_OPHARV (CONTROL, ISWITCH, 
     &    AGEFAC, APTNUP, BIOMAS, CANNAA, CANWAA,         !Input
     &    CORMLNO, CORMN, CORMNUP, CORMWT, DYIELD, G2,    !Input
     &    HARVFRAC, ISDATE, ISTAGE, LAI, LEAFNO, MAXLAI,  !Input
     &    MDATE, NSTRES, PBIOMS, PCORMN, PLANTS, PLTPOP,  !Input
     &    SKERWT, STGDOY, STNAME, STOVER, STOVN, SWFAC,   !Input
     &    TOTNUP, TURFAC, YRPLT,                          !Input
     &    BWAH, SDWT, SDWTAH, TOPWT, WTNSD,               !Output
     &    MCORMWT)    !, TCORMWT ) !additions RMO

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

!     Temporary -- until P is integrated with this crop module
      PConc_Shut = 0.0
      PConc_Root = 0.0
      PConc_Shel = 0.0
      PConc_Seed = 0.0

C-----------------------------------------------------------------------
C     Determine Seedling Nursery period
C-----------------------------------------------------------------------
!      CALL YR_DOY (YRPLT, YR, ISOW)
      YRSOW = YRPLT

      !Initialize phenology
      CALL TR_PHENOL (CONTROL, ISWITCH, 
     &    AGEFAC, DAYL, LEAFNO, NSTRES, SDEPTH, SOILPROP, !Input
     &    SRAD, SW, SWFAC, TGROCOM, TILNO, TMAX, TMIN,    !Input
     &    TURFAC, YRPLT,                                  !Input
     &    CUMDTT, EMAT, PLANTS, RTDEP, YRSOW,             !I/O
     &    CropStatus,                                     !Output
     &    CDTT_TP, DTT, FIELD, ISTAGE, ITRANS, LTRANS,    !Output
     &    MDATE, NDAT, NEW_PHASE, P1, P1T, P3, P4,        !Output
     &    SDTT_TP,SEEDNI, SI3, STGDOY, STNAME, SUMDTT,    !Output
     &    TAGE, TBASE, TF_GRO, WSTRES, XSTAGE, XST_TP)    !Output

      CALL TR_ROOTGR (CONTROL, 
     &    DTT, FLOOD, GRORT, ISWNIT, ISWWAT,              !Input
     &    ITRANS, NH4, NO3, PLANTS, RLWR, SOILPROP,       !Input
     &    SUMDTT, SW, SWFAC, YRDOY, YRPLT,                !Input
     &    FLOODN, RTWT,                                   !I/O
     &    RLV, RTDEP, SDEPTH)                             !Output

      CALL TR_GROSUB (CONTROL, ISWITCH, 
     &    BF, CARBC1, CARBC2, CARBC3, 
     &    CARBR1, CARBR2, CARBR3, CARBR4, CARBL0,
     &    CARBL1, CARBL2, CARBL3, CARBL4, CMAT2, CMAT5,  !Input 
     &    CO2, CDTT_TP, CUMDTT, DTT, EMAT, FIELD, FLOOD, 
     &    FLOODN, G2, G3, G4, ISTAGE, ITRANS, KLITE,
     &    LEAF1, LEAF3, LEAFT, LTRANS, MCORMWT, MLFWT, 
     &    MPETWT, NEW_PHASE, NH4, NO3, P1, P1T, P3,  
     &    P4, PCGRD, PCINT, PETGR1, PETGR2, PETGR5, 
     &    PHINT, PLAMAX, PLANTS, PLTPOP, PORMIN,
     &    RCONST, RCTFAC, RESPF, RGCORM2, RLV, RNINT,
     &    RPET, RSENCE, RTDEP, RUEA,
     &    SDEPTH, SDTT_TP, SEEDRV, SEEDNI, SLA3, SLA4,  !slan1
     &    SLA5,  SLAIFC, SLFCGRD, SLFNGRD, SLFNINT, 
     &    SLFTGRD,SLFTINT, SLFWGRD, SLFWINT,  
     &    SLGRD1, SLGRD2, SOILPROP, SRAD, ST,  
     &    SUFAC2, SUFAC3, SUMDTT, SW, SWFAC, 
     &    TBASE, TCARBC2, TF_GRO, TGRAD, TIPGRAD,  !TAGE, 
     &    TIPGRAD3, TIPINT, TMAX, TMIN, TURFAC, !WSTRES, 
     &    XNFGRAD, XNFINT, XTNMAX, XSTAGE, XST_TP,        !Input
     &    YRPLT, YRSOW,                  
     &    AGEFAC, APTNUP, BIOMAS, CANHT, CANNAA, CANWAA,  !Output
     &    CORMNUP, CORMLNO, CORMN, CORMWT, DYIELD, GRORT, !Output
     &    LAI, LEAFNO, LFWT, MAXLAI, NSTRES, PBIOMS,      !Output
     &    PCORMN, RLWR, ROOTN, RTWT, RWUEP1, RWUMX,       !Output
     &    STOVER, STOVN, TANC, TGROCOM, TILNO, TOTNUP,    !Output
     &    UNH4, UNO3, MDATE, WTNUP,                      !Output
     &    CARBO, CropStatus,                              !Output
     &    PETWT)                              !addition RMO

      CALL TR_OPGROW (CONTROL, ISWITCH, SOILPROP, 
     &    BIOMAS, CORMWT, DEADLF, DTT, ISTAGE,            !Input
     &    LAI, LFWT, MDATE, NLAYR, NSTRES, PLTPOP,        !Input
     &    RLV, ROOTN, RTDEP, RTWT, SATFAC, SENESCE,       !Input
     &    PETWT, STOVN, STOVWT, SWFAC, CORMN,       !Input
     &    MCORMWT, CORMLNO,                               !Input
     &    TURFAC, WTNCAN, WTNUP, XLAI, YRPLT)             !Input

      CALL TR_OPHARV (CONTROL, ISWITCH, 
     &    AGEFAC, APTNUP, BIOMAS, CANNAA, CANWAA,         !Input
     &    CORMLNO, CORMN, CORMNUP, CORMWT, DYIELD, G2,    !Input
     &    HARVFRAC, ISDATE, ISTAGE, LAI, LEAFNO, MAXLAI,  !Input
     &    MDATE, NSTRES, PBIOMS, PCORMN, PLANTS, PLTPOP,  !Input
     &    SKERWT, STGDOY, STNAME, STOVER, STOVN, SWFAC,   !Input
     &    TOTNUP, TURFAC, YRPLT,                          !Input
     &    BWAH, SDWT, SDWTAH, TOPWT, WTNSD,               !Output
     &    MCORMWT)    !, TCORMWT ) !additions RMO

!     NOTE: TAGE is initialized in PHENOL
      CALL GNURSE (ITRANS, TAGE, YRPLT, YRSIM,            !Input
     &             YRSOW, YRNURSE)                        !Output

!***********************************************************************
!***********************************************************************
!     Daily rate / integration calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
C-----------------------------------------------------------------------
C     Define dates for water balance calculations
C     YRSIM = Start of Simulation Date
C     YRPLT = Planting Date
C     EDATE = Emergence Date
C     MDATE = Maturity Date
C     YRDOY = Year - Day of Year (Dynamic Variable)
C-----------------------------------------------------------------------
!     Zero N uptake arrays (NUPTAK not always called)
      UNO3 = 0.0
      UNH4 = 0.0

       SELECT CASE (ITRANS)
         CASE (1);    EDATE = STGDOY (09)
         CASE (2, 3); EDATE = STGDOY (11)
         CASE (4);    EDATE = STGDOY (10)
       END SELECT

!     Calculate daily stresses (from SWFACS)
      IF (ISWWAT .EQ. 'Y') THEN
        SWFAC  = 1.0
        TURFAC  = 1.0
        !PHEFAC = 1.0
        IF (EOP .GT. 0.0 .AND. YRDOY .GT. EDATE .AND. EDATE .GT. 0) THEN
          EP1 = EOP * 0.1
          IF (TRWUP / EP1 .LT. RWUEP1) THEN
            TURFAC = (1. / RWUEP1) * TRWUP / EP1
          ENDIF
          IF (EP1 .GE. TRWUP) THEN
            SWFAC = TRWUP / EP1
          ENDIF
        ENDIF

       !WRESR growth and depth routine
        CALL TR_ROOTGR (CONTROL, 
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
          CALL TR_PHENOL (CONTROL, ISWITCH, 
     &    AGEFAC, DAYL, LEAFNO, NSTRES, SDEPTH, SOILPROP, !Input
     &    SRAD, SW, SWFAC, TGROCOM, TILNO, TMAX, TMIN,    !Input
     &    TURFAC, YRPLT,                                  !Input
     &    CUMDTT, EMAT, PLANTS, RTDEP, YRSOW,             !I/O
     &    CropStatus,                                     !Output
     &    CDTT_TP, DTT, FIELD, ISTAGE, ITRANS, LTRANS,    !Output
     &    MDATE, NDAT, NEW_PHASE, P1, P1T,P3, P4,         !Output
     &    SDTT_TP,SEEDNI, SI3, STGDOY, STNAME, SUMDTT,    !Output
     &    TAGE, TBASE, TF_GRO, WSTRES, XSTAGE, XST_TP)    !Output
         ENDIF
      ENDIF

C--------------------------------------------------------------
C        Call Growth and Root Growth Routines
C--------------------------------------------------------------

      CALL TR_GROSUB (CONTROL, ISWITCH, 
     &    BF, CARBC1, CARBC2, CARBC3, 
     &    CARBR1, CARBR2, CARBR3, CARBR4, CARBL0,
     &    CARBL1, CARBL2, CARBL3, CARBL4, CMAT2, CMAT5,  !Input 
     &    CO2, CDTT_TP, CUMDTT, DTT, EMAT, FIELD, FLOOD, 
     &    FLOODN, G2, G3, G4, ISTAGE, ITRANS, KLITE,
     &    LEAF1, LEAF3, LEAFT, LTRANS, MCORMWT, MLFWT, 
     &    MPETWT, NEW_PHASE, NH4, NO3, P1, P1T, P3,  
     &    P4, PCGRD, PCINT, PETGR1, PETGR2, PETGR5, 
     &    PHINT, PLAMAX, PLANTS, PLTPOP, PORMIN,
     &    RCONST, RCTFAC, RESPF, RGCORM2, RLV, RNINT,
     &    RPET, RSENCE, RTDEP, RUEA,
     &    SDEPTH, SDTT_TP, SEEDRV, SEEDNI, SLA3, SLA4,  !slan1
     &    SLA5,  SLAIFC, SLFCGRD, SLFNGRD, SLFNINT, 
     &    SLFTGRD,SLFTINT, SLFWGRD, SLFWINT,  
     &    SLGRD1, SLGRD2, SOILPROP, SRAD, ST,  
     &    SUFAC2, SUFAC3, SUMDTT, SW, SWFAC, 
     &    TBASE, TCARBC2, TF_GRO, TGRAD, TIPGRAD,  !TAGE, 
     &    TIPGRAD3, TIPINT, TMAX, TMIN, TURFAC, !WSTRES, 
     &    XNFGRAD, XNFINT, XTNMAX, XSTAGE, XST_TP,        !Input
     &    YRPLT, YRSOW,                  
     &    AGEFAC, APTNUP, BIOMAS, CANHT, CANNAA, CANWAA,  !Output
     &    CORMNUP, CORMLNO, CORMN, CORMWT, DYIELD, GRORT, !Output
     &    LAI, LEAFNO, LFWT, MAXLAI, NSTRES, PBIOMS,      !Output
     &    PCORMN, RLWR, ROOTN, RTWT, RWUEP1, RWUMX,       !Output
     &    STOVER, STOVN, TANC, TGROCOM, TILNO, TOTNUP,    !Output
     &    UNH4, UNO3, MDATE, WTNUP,                       !Output
     &    CARBO, CropStatus,                              !Output
     &    PETWT)                              !addition RMO

      FLOODN % NDAT   = NDAT

!***********************************************************************
!***********************************************************************
!     Daily output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
      CALL TR_OPGROW (CONTROL, ISWITCH, SOILPROP, 
     &    BIOMAS, CORMWT, DEADLF, DTT, ISTAGE,           !Input
     &    LAI, LFWT, MDATE, NLAYR, NSTRES, PLTPOP,        !Input
     &    RLV, ROOTN, RTDEP, RTWT, SATFAC, SENESCE,       !Input
     &    PETWT, STOVN, STOVWT, SWFAC, CORMN,       !Input
     &    MCORMWT, CORMLNO,                               !Input
     &    TURFAC, WTNCAN, WTNUP, XLAI, YRPLT)             !Input

      CALL TR_OPHARV (CONTROL, ISWITCH, 
     &    AGEFAC, APTNUP, BIOMAS, CANNAA, CANWAA,         !Input
     &    CORMLNO, CORMN, CORMNUP, CORMWT, DYIELD, G2,    !Input
     &    HARVFRAC, ISDATE, ISTAGE, LAI, LEAFNO, MAXLAI,  !Input
     &    MDATE, NSTRES, PBIOMS, PCORMN, PLANTS, PLTPOP,  !Input
     &    SKERWT, STGDOY, STNAME, STOVER, STOVN, SWFAC,   !Input
     &    TOTNUP, TURFAC, YRPLT,                          !Input
     &    BWAH, SDWT, SDWTAH, TOPWT, WTNSD,               !Output
     &    MCORMWT)    !, TCORMWT ) !additions RMO
!***********************************************************************
!***********************************************************************
!     Seasonal output
!***********************************************************************
      IF (DYNAMIC .EQ. SEASEND) THEN
        PODWT = 0.0
        GRNWT = 0.0
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
      END SUBROUTINE TR_SUBSTOR
