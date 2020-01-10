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
C  PT_SUBSTOR_2D, Subroutine
C
C  Main routine for potato growth module.
C-----------------------------------------------------------------------
C  Revision history
C
C  08/29/2001 CHP Written for modular pototo model to be incorporated 
C                   into CROPGRO.
C  03/12/2003 CHP Changed senescence variable to composite (SENESCE)
C                   as defined in ModuleDefs.for
!  12/17/2004 CHP Modified HRESCeres call for harvest residue
!  08/17/2005 CHP Renamed to PT_SUBSTOR to accomodate TN, TR SUBSTOR
!                 routines.
C=======================================================================

      SUBROUTINE PT_SUBSTOR_2D(CONTROL, ISWITCH,
     &    CO2, EOP, CELLS, HARVFRAC, NH4, NO3, SOILPROP, SRAD,   !Input 
    ! &    CO2, EOP, HARVFRAC, NH4, NO3, SOILPROP, SRAD,   !Input
     &    ST, SW, TMAX, TMIN,SWFAC, TURFAC, TRWUP, TWILEN, YREND, YRPLT,!Input  
   !   &    ST, SW, TMAX, TMIN, TRWUP, TWILEN, YREND, YRPLT,!Input  
     &    CANHT, HARVRES, MDATE, NSTRES, PORMIN, RLV,     !Output 1D does not has RLV2
    ! &    CANHT, HARVRES, MDATE, NSTRES, PORMIN, RLV2,     !Output JZW change RLV to RLV2
     &    RWUMX, SENESCE, STGDOY, UNH4, UNO3, XLAI)       !Output
!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      USE Cells_2D
      USE ModuleData
      IMPLICIT NONE
      SAVE

      CHARACTER*1  IDETG, ISWNIT, ISWWAT
      CHARACTER*2  CROP
      CHARACTER*30 FILEIO

      INTEGER DYNAMIC, YREND, ISDATE, ISTAGE, I
      INTEGER MDATE, NLAYR, RUN, YRDOY, YREMRG, YRPLT, YRSIM
      INTEGER STGDOY(20)

      REAL AGEFAC, APTNUP, BIOMAS, BWAH, CANNAA, CANWAA
      REAL CANHT, CNSD1, CNSD2, CO2
      REAL CUMDEP, CUMDTT, DEADLF, DTT, EOP, EP1, GNUP
      REAL GRAINN, GRNWT, GRORT, LFWT, MAXLAI, NSTRES
      REAL PLANTS, PLTPOP, PODWT, ROOTN, RTDEP, RTF
      REAL RTWT, SDWTAH, SDWTPL, SEEDNI, SEEDRV, SRAD, STMWT, STOVN
      REAL SLPF
      REAL STOVWT, STT, SWFAC, TMAX, TMIN, TOPSN
      REAL TOPWT, TOTNUP, TRNU, TUBN, TUBWT, TURFAC, TWILEN
      REAL WTNCAN, WTNLO, XLAI, XSTAGE
      REAL SDWT, SEEDNO, TRWUP, WTNSD, WTNUP, YIELD

      REAL SATFAC !SATFAC imported only for PT_OPGROW
      REAL RWUEP1, PORMIN, RWUMX

      REAL, DIMENSION(2)  :: HARVFRAC

      REAL, DIMENSION(NL) :: DLAYR, DUL, DS, LL, KG2PPM
      REAL, DIMENSION(NL) :: NH4, NO3, RLV, SAT, SHF
      REAL, DIMENSION(NL) :: ST, SW, UNO3, UNH4

!     P variables
      REAL PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed

!-----------------------------------------------------------------------
!     Constructed variable types defined in ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SoilType)    SOILPROP
      TYPE (SwitchType)  ISWITCH
      Type (ResidueType) HARVRES
      Type (ResidueType) SENESCE
      Type (CellType)    CELLS(MaxRows,MaxCols)

!     Transfer values from constructed data types into local variables.
      CROP    = CONTROL % CROP
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM

      DLAYR  = SOILPROP % DLAYR  
      DS     = SOILPROP % DS     
      DUL    = SOILPROP % DUL 
      KG2PPM = SOILPROP % KG2PPM     
      LL     = SOILPROP % LL     
      NLAYR  = SOILPROP % NLAYR  
      SAT    = SOILPROP % SAT    
      SHF    = SOILPROP % WR
      SLPF   = SOILPROP % SLPF   
   
      IDETG  = ISWITCH % IDETG
      ISWNIT = ISWITCH % ISWNIT
      ISWWAT = ISWITCH % ISWWAT

!***********************************************************************
!***********************************************************************
!     Run initialization - run once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
      CALL PT_OPGROW(CONTROL, ISWITCH, 
     &    BIOMAS, DEADLF, GRAINN, ISTAGE, LFWT, MDATE,    !Input
     &    NLAYR, NSTRES, PLTPOP, RLV, ROOTN, RTDEP, RTWT, !Input
     &    SATFAC, SENESCE, STMWT, STOVN, STOVWT, SWFAC,   !Input
     &    TUBN, TUBWT, TURFAC, WTNCAN, WTNUP, XLAI, YRPLT)!Input

      CALL PT_OPHARV(CONTROL, ISWITCH, 
     &    AGEFAC, APTNUP, BIOMAS, GNUP, HARVFRAC, ISDATE, !Input
     &    ISTAGE, MAXLAI, MDATE, NSTRES, PLTPOP, SDWT,    !Input
     &    SDWTPL, SEEDNO, STGDOY, STOVWT, SWFAC, TOTNUP,  !Input
     &    TUBN, TUBWT, TURFAC, WTNCAN, WTNUP, XLAI,       !Input
     &    YIELD, YRPLT,                                   !Input
     &    BWAH, SDWTAH, WTNSD)                            !Output
      ! JZW for debug, comparing the results of 1D & 2D
!      Write(93,*)",,,,,1D Model,,,,,  , , ,,,, 2D Model"
!      Write(93,*)"DAS,,DS1,DS2,DS3,DS4,DS5,DS6,DS7,DS8,"
!     &      ,",,DS1,DS2,DS3,DS4,DS5,DS6,DS7,DS8,DS9"
!      Write(93,930)",",DS(1),DS(2),DS(3),DS(4),DS(5), DS(6),DS(7),DS(8),
!     &   ",,",DS(1),DS(2),DS(3),DS(4),DS(5), DS(6), DS(7),DS(8),DS(9)
!  930  format (A1,8(",",F6.2),A2,9(",",F6.2))
!      Write(93,*)" ,RTDEP,RLV1,RLV2,RLV3,RLV4,RLV5,RLV6,RLV7,RLV8,,
!     &  RTDEP2,RLV1,RLV2,RLV3,RLV4,RLV5,RLV6,RLV7,RLV8,RLV9" 
!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!     from SWFACS
      TURFAC = 1.0
      SWFAC  = 1.0
      SEEDNO = 0.0
      CANHT  = 0.0

      CALL PT_IPSPE(FILEIO, RWUEP1, PORMIN, RWUMX)
!      RWUEP1 = 1.5
!      PORMIN = 0.02                              ! Minimum pore space
!      RWUMX  = 0.09                              ! Max root water uptake
!      RLWR   = 0.75

!     Temporary -- until P is integrated with this crop module
      PConc_Shut = 0.0
      PConc_Root = 0.0
      PConc_Shel = 0.0
      PConc_Seed = 0.0

      CALL PT_ROOTGR_2D(SEASINIT, ISWWAT, CELLS, YRDOY,
     &    DLAYR, DS, DTT, FILEIO, GRORT, ISWNIT,          !Input
     &    NH4, NLAYR, NO3, PLTPOP, SHF, SWFAC,            !Input
     &    CUMDEP, RLV, RTDEP)                             !Output

      CALL PT_PHENOL (
     &    DLAYR, FILEIO, GRAINN, ISWWAT, LL, MDATE, NLAYR,!Input
     &    NSTRES, PLTPOP, RTWT, ST, SW, SWFAC, TMAX, TMIN,!Input
     &    TOPSN, TWILEN, XLAI, YRDOY, YRPLT, YRSIM,       !Input
     &    APTNUP, CUMDTT, DTT, GNUP, GRORT, ISDATE,       !Output
     &    ISTAGE, MAXLAI, PLANTS, RTF, SEEDRV,            !Output
     &    STGDOY, STT, TOTNUP, XSTAGE, YREMRG,            !Output
     &    SEASINIT)

      CALL PT_GROSUB_2D (SEASINIT, CELLS,
     &    CO2, CUMDTT, DLAYR, DTT, DUL, FILEIO,           !Input
     &    ISTAGE, ISWNIT, KG2PPM, LL, NLAYR,              !Input
     &    RTF, SAT, SLPF, SRAD, STGDOY, STT,              !Input
     &    SWFAC, TMAX, TMIN, TURFAC, XSTAGE, YRDOY,       !Input
     &    GRORT, SEEDRV,                                  !I/O
     &    AGEFAC, BIOMAS, CANNAA, CANWAA, CNSD1, CNSD2,   !Output
     &    DEADLF, GRAINN, LFWT, NSTRES, PLTPOP, ROOTN,    !Output
     &    RTWT, SDWTPL, SEEDNI, SENESCE, STMWT, STOVN,    !Output
     &    STOVWT, TOPSN, TOPWT, TRNU, TUBN, TUBWT,        !Output
     &    UNH4, UNO3, WTNCAN, WTNLO, WTNUP, XLAI)         !Output

      STGDOY(14) = YRSIM

      CALL PT_OPGROW(CONTROL, ISWITCH, 
     &    BIOMAS, DEADLF, GRAINN, ISTAGE, LFWT, MDATE,    !Input
     &    NLAYR, NSTRES, PLTPOP, RLV, ROOTN, RTDEP, RTWT, !Input
     &    SATFAC, SENESCE, STMWT, STOVN, STOVWT, SWFAC,   !Input
     &    TUBN, TUBWT, TURFAC, WTNCAN, WTNUP, XLAI, YRPLT)!Input

      CALL PT_OPHARV(CONTROL, ISWITCH, 
     &    AGEFAC, APTNUP, BIOMAS, GNUP, HARVFRAC, ISDATE, !Input
     &    ISTAGE, MAXLAI, MDATE, NSTRES, PLTPOP, SDWT,    !Input
     &    SDWTPL, SEEDNO, STGDOY, STOVWT, SWFAC, TOTNUP,  !Input
     &    TUBN, TUBWT, TURFAC, WTNCAN, WTNUP, XLAI,       !Input
     &    YIELD, YRPLT,                                   !Input
     &    BWAH, SDWTAH, WTNSD)                            !Output
!       CALL ArrayHandler(CELLS, CONTROL, SOILPROP, CELLS%STATE%RLV, 
!     &    "RLV",0.0,20.0)

!***********************************************************************
!***********************************************************************
!     Daily rate calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
!     Calculate daily water stess factors (from SWFACS)
      SWFAC  = 1.0
      TURFAC = 1.0
      IF (ISWWAT .EQ. 'Y' .AND. EOP .GT. 0.0) THEN
        EP1 = EOP * 0.1
        IF (TRWUP / EP1 .LT. RWUEP1) THEN
          TURFAC = (1./RWUEP1) * TRWUP / EP1
        ENDIF
        IF (EP1 .GE. TRWUP) THEN
          SWFAC = TRWUP / EP1
        ENDIF
      ENDIF

!     ROOTGR was called from main program between WATBAL and NTRANS
      IF (ISWWAT .EQ. 'Y') THEN
        !
        ! WRESR growth and depth routine
        !
        ! In the plant subroutine, initialize the RLV=0 for the days RLV are not calculated
        !IF (DTT .GT. 0.0) THEN 
        ! root growth routine should not be called until emergence day.
        IF (GRORT .GT. 0.0) THEN
          CALL PT_ROOTGR_2D(RATE, ISWWAT, CELLS, YRDOY,
     &    DLAYR, DS, DTT, FILEIO, GRORT, ISWNIT,          !Input
     &    NH4, NLAYR, NO3, PLTPOP, SHF, SWFAC,            !Input
     &    CUMDEP, RLV, RTDEP)                             !Output
        ENDIF
        ! JZW for debug, comparing the results of 1D & 2D
!        Write(92,*)"PT_RootGR: RLV(L)=,", RLV
!        Write(92,*)"PT_RootGR_2D: RLV(ROW)=,",RLV2 
!        Write(93,931)CONTROL % DAS,RTDEP,(RLV(I),I=1,8),",",RTDEP2,
!     &      (RLV2(I),I=1,9)
      ENDIF
! 931  format (I3,9(",",F7.4),A2,10(",",F7.4))
      IF (YRDOY .EQ. YRPLT .OR. ISTAGE .NE. 5) THEN
        CALL PT_PHENOL (
     &    DLAYR, FILEIO, GRAINN, ISWWAT, LL, MDATE, NLAYR,!Input
     &    NSTRES, PLTPOP, RTWT, ST, SW, SWFAC, TMAX, TMIN,!Input
     &    TOPSN, TWILEN, XLAI, YRDOY, YRPLT, YRSIM,       !Input
     &    APTNUP, CUMDTT, DTT, GNUP, GRORT, ISDATE,       !Output
     &    ISTAGE, MAXLAI, PLANTS, RTF, SEEDRV,            !Output
     &    STGDOY, STT, TOTNUP, XSTAGE, YREMRG,            !Output
     &    RATE)
      ENDIF

      IF (ISTAGE .LT. 5) THEN
        CALL PT_GROSUB_2D (RATE, CELLS,
     &    CO2, CUMDTT, DLAYR, DTT, DUL, FILEIO,           !Input
     &    ISTAGE, ISWNIT, KG2PPM, LL, NLAYR,              !Input
     &    RTF, SAT, SLPF, SRAD, STGDOY, STT,              !Input
     &    SWFAC, TMAX, TMIN, TURFAC, XSTAGE, YRDOY,       !Input
     &    GRORT, SEEDRV,                                  !I/O
     &    AGEFAC, BIOMAS, CANNAA, CANWAA, CNSD1, CNSD2,   !Output
     &    DEADLF, GRAINN, LFWT, NSTRES, PLTPOP, ROOTN,    !Output
     &    RTWT, SDWTPL, SEEDNI, SENESCE, STMWT, STOVN,    !Output
     &    STOVWT, TOPSN, TOPWT, TRNU, TUBN, TUBWT,        !Output
     &    UNH4, UNO3, WTNCAN, WTNLO, WTNUP, XLAI)         !Output
      ELSE
        UNO3 = 0.0
        UNH4 = 0.0
      ENDIF
!      CALL ArrayHandler(CELLS, CONTROL, SOILPROP, CELLS%STATE%RLV, 
!     &    "RLV",0.0,20.0)
!***********************************************************************
!***********************************************************************
!     Daily output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
!-----------------------------------------------------------------------
      IF (YRDOY .EQ. YREND) THEN
        STGDOY(20) = YREND
      ENDIF

      CALL PT_OPGROW(CONTROL, ISWITCH, 
     &    BIOMAS, DEADLF, GRAINN, ISTAGE, LFWT, MDATE,    !Input
     &    NLAYR, NSTRES, PLTPOP, RLV, ROOTN, RTDEP, RTWT, !Input
     &    SATFAC, SENESCE, STMWT, STOVN, STOVWT, SWFAC,   !Input
     &    TUBN, TUBWT, TURFAC, WTNCAN, WTNUP, XLAI, YRPLT)!Input

      CALL PT_OPHARV(CONTROL, ISWITCH, 
     &    AGEFAC, APTNUP, BIOMAS, GNUP, HARVFRAC, ISDATE, !Input
     &    ISTAGE, MAXLAI, MDATE, NSTRES, PLTPOP, SDWT,    !Input
     &    SDWTPL, SEEDNO, STGDOY, STOVWT, SWFAC, TOTNUP,  !Input
     &    TUBN, TUBWT, TURFAC, WTNCAN, WTNUP, XLAI,       !Input
     &    YIELD, YRPLT,                                   !Input
     &    BWAH, SDWTAH, WTNSD)                            !Output
     
      ! CALL PT_OPRoots_2D in PT_ROOTGR_2D when DYNAMIC .EQ. OUTPUT
      CALL PT_ROOTGR_2D(DYNAMIC, ISWWAT, CELLS, YRDOY,
     &    DLAYR, DS, DTT, FILEIO, GRORT, ISWNIT,          !Input
     &    NH4, NLAYR, NO3, PLTPOP, SHF, SWFAC,            !Input
     &    CUMDEP, RLV, RTDEP)                             !Output    
 
!***********************************************************************
!***********************************************************************
!     Seasonal output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
!     YIELD  = TUBWT*10.*PLANTS   !YIELD used by OPAHRV, OPOPS
      YIELD  = TUBWT*10.*PLTPOP   !CHP changed 

      CALL PT_OPGROW(CONTROL, ISWITCH, 
     &    BIOMAS, DEADLF, GRAINN, ISTAGE, LFWT, MDATE,    !Input
     &    NLAYR, NSTRES, PLTPOP, RLV, ROOTN, RTDEP, RTWT, !Input
     &    SATFAC, SENESCE, STMWT, STOVN, STOVWT, SWFAC,   !Input
     &    TUBN, TUBWT, TURFAC, WTNCAN, WTNUP, XLAI, YRPLT)!Input

      CALL PT_OPHARV(CONTROL, ISWITCH, 
     &    AGEFAC, APTNUP, BIOMAS, GNUP, HARVFRAC, ISDATE, !Input
     &    ISTAGE, MAXLAI, MDATE, NSTRES, PLTPOP, SDWT,    !Input
     &    SDWTPL, SEEDNO, STGDOY, STOVWT, SWFAC, TOTNUP,  !Input
     &    TUBN, TUBWT, TURFAC, WTNCAN, WTNUP, XLAI,       !Input
     &    YIELD, YRPLT,                                   !Input
     &    BWAH, SDWTAH, WTNSD)                            !Output

      PODWT = 0.0
      GRNWT = YIELD
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

      CALL PT_ROOTGR_2D(DYNAMIC, ISWWAT, CELLS, YRDOY,
     &    DLAYR, DS, DTT, FILEIO, GRORT, ISWNIT,          !Input
     &    NH4, NLAYR, NO3, PLTPOP, SHF, SWFAC,            !Input
     &    CUMDEP, RLV, RTDEP)                             !Output    

!       CALL ArrayHandler(CELLS, CONTROL, SOILPROP, CELLS%STATE%RLV, 
!     &    "RLV",0.0,20.0)
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF

!***********************************************************************
      RETURN
      END SUBROUTINE PT_SUBSTOR_2D
C=======================================================================



