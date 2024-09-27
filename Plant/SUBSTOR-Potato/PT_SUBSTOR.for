C=======================================================================
C  COPYRIGHT 1998-2023 DSSAT Foundation
C                      University of Florida, Gainesville, Florida
C                      International Fertilizer Development Center
C                      Universidad Politechnica de Madrid
C  ALL RIGHTS RESERVED
C=======================================================================
C=======================================================================
C  PT_SUBSTOR, Subroutine
C
C  Main routine for potato growth module.
C-----------------------------------------------------------------------
C  Revision history
C
C  08/29/2001 CHP Written for modular pototo model to be incorporated 
C                   into CROPGRO.
C  03/12/2003 CHP Changed senescence variable to composite (SENESCE)
C                   as defined in ModuleDefs.for
C  12/17/2004 CHP Modified HRESCeres call for harvest residue
C  08/17/2005 CHP Renamed to PT_SUBSTOR to accomodate TN, TR SUBSTOR
C                 routines.
C  08/23/2011 GH Added CO2 response for tuber growth
!  01/26/2023 CHP Reduce compile warnings: add EXTERNAL stmts, remove 
!                 unused variables, shorten lines. 
C=======================================================================

      SUBROUTINE PT_SUBSTOR(CONTROL, ISWITCH,
     &    CO2, EOP, HARVFRAC, NH4, NO3, SOILPROP, SRAD,   !Input
     &    ST, SW, TMAX, TMIN, TRWUP, TWILEN, YREND, YRPLT,!Input
     &    CANHT, HARVRES, MDATE, NSTRES, PORMIN, RLV,     !Output
     &    RWUMX, SENESCE, STGDOY, UNH4, UNO3, XLAI)       !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL PT_OPGROW, PT_OPHARV, PT_IPSPE, PT_ROOTGR, PT_PHENOL, 
     &  PT_GROSUB, HRes_Ceres
      SAVE

      CHARACTER*1  IDETG, ISWNIT, ISWWAT
      CHARACTER*2  CROP
      CHARACTER*30 FILEIO

      INTEGER DYNAMIC, YREND, ISDATE, ISTAGE
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

      CALL PT_ROOTGR (SEASINIT,
     &    DLAYR, DS, DTT, DUL, FILEIO, GRORT, ISWNIT,     !Input
     &    LL, NH4, NLAYR, NO3, PLTPOP, SHF, SW, SWFAC,    !Input
     &    CUMDEP, RLV, RTDEP)                             !Output

      CALL PT_PHENOL (
     &    DLAYR, FILEIO, GRAINN, ISWWAT, LL, MDATE, NLAYR,!Input
     &    NSTRES, PLTPOP, RTWT, ST, SW, SWFAC, TMAX, TMIN,!Input
     &    TOPSN, TWILEN, XLAI, YRDOY, YRPLT, YRSIM,       !Input
     &    APTNUP, CUMDTT, DTT, GNUP, GRORT, ISDATE,       !Output
     &    ISTAGE, MAXLAI, PLANTS, RTF, SEEDRV,            !Output
     &    STGDOY, STT, TOTNUP, XSTAGE, YREMRG,            !Output
     &    SEASINIT)

      CALL PT_GROSUB (SEASINIT,
     &    CO2, CUMDTT, DLAYR, DTT, DUL, FILEIO,           !Input
     &    ISTAGE, ISWNIT, KG2PPM, LL, NH4, NLAYR, NO3,    !Input
     &    RLV, RTF, SAT, SLPF, SRAD, STGDOY, STT, SW,     !Input
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

!       WRESR growth and depth routine
        IF (GRORT .GT. 0.0) THEN
          CALL PT_ROOTGR (RATE, 
     &    DLAYR, DS, DTT, DUL, FILEIO, GRORT, ISWNIT,     !Input
     &    LL, NH4, NLAYR, NO3, PLTPOP, SHF, SW, SWFAC,    !Input
     &    CUMDEP, RLV, RTDEP)                             !Output
        ENDIF
      ENDIF

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
        CALL PT_GROSUB (RATE,
     &    CO2, CUMDTT, DLAYR, DTT, DUL, FILEIO,           !Input
     &    ISTAGE, ISWNIT, KG2PPM, LL, NH4, NLAYR, NO3,    !Input
     &    RLV, RTF, SAT, SLPF, SRAD, STGDOY, STT, SW,     !Input
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

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF

!***********************************************************************
      RETURN
      END SUBROUTINE PT_SUBSTOR
C=======================================================================


C=======================================================================
C  PT_IPSPE, Subroutine
C
C  Input data for potato module
C-----------------------------------------------------------------------
C  Revision history
C
C  08/23/2001 CHP Written
C  10/25/2002 CHP Modified read format for Y2K
C  08/12/2003 CHP Added I/O error checking
C                 Read harvest section to set MDATE (potato only)
C-----------------------------------------------------------------------

      SUBROUTINE PT_IPSPE(FILEIO, RWUEP1, PORMIN, RWUMX)

!     ------------------------------------------------------------------

      IMPLICIT NONE
      EXTERNAL ERROR, GETLUN, IGNORE

      CHARACTER*1, PARAMETER :: BLANK = ' '
      CHARACTER*6, PARAMETER :: ERRKEY = 'ROOTGR'

      CHARACTER*4   C4
      CHARACTER*12  FILEC
      CHARACTER*30  FILEIO
      CHARACTER*80  PATHCR
      CHARACTER*92  FILECC
      CHARACTER*180 CHAR

      INTEGER LUNIO, LUNCRP
      INTEGER ERR, ISECT, LNUM, PATHL

      REAL RWUEP1, PORMIN, RWUMX
      
!     LOGICAL EOF
!-----------------------------------------------------------------------
!     Read data from FILEIO for use in ROOTGR module
      CALL GETLUN('FILEIO', LUNIO)
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)

      READ(LUNIO,'(6(/),15X,A12,1X,A80)') FILEC, PATHCR
      LNUM = 7
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

!C-----------------------------------------------------------------------
!C    Read Harvest Section for maturity date 
!C-----------------------------------------------------------------------
!     CHP 6/26/09 harvest date is handled by management routine.
!      SECTION = '*HARVE'
!      CALL FIND(LUNIO, SECTION, LINC, FOUND); LNUM = LNUM + LINC
!      IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEIO, LNUM)
!      READ(LUNIO,'(3X,I7)',IOSTAT=ERR) MDATE
!      LNUM = LNUM + 1
!      IF (ERR .NE. 0) CALL ERROR(ERRKEY, ERR, FILEIO, LNUM)

      CLOSE (LUNIO)

C-----------------------------------------------------------------------
C     Read Crop Parameters from FILEC
C-----------------------------------------------------------------------
      LNUM   = 0
      PATHL  = INDEX (PATHCR,BLANK)
      IF (PATHL .LE. 1) THEN
         FILECC = FILEC
       ELSE
         FILECC = PATHCR(1:(PATHL-1)) // FILEC
      ENDIF
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEC,0)

      LNUM = 0
!     EOF not portable. CHP 7/24/2007
!     DO WHILE (.NOT. EOF (LUNCRP))
      DO WHILE (ERR == 0)
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
!       IF (ISECT .EQ. 0) CALL ERROR(ERRKEY,33,FILECC,LNUM)
        IF (ISECT .EQ. 0) EXIT
        IF (ISECT .EQ. 2) CYCLE
        C4 = CHAR(10:13)

        SELECT CASE(C4)
          CASE('RWEP'); READ(CHAR,'(14X,F6.0)',IOSTAT=ERR) RWUEP1
          CASE('PORM'); READ(CHAR,'(14X,F6.0)',IOSTAT=ERR) PORMIN
          CASE('RWMX'); READ(CHAR,'(14X,F6.0)',IOSTAT=ERR) RWUMX
        END SELECT
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEC,LNUM)
      END DO

      CLOSE (LUNCRP)

C-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE PT_IPSPE
C=======================================================================

