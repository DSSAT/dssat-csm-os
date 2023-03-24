C=======================================================================
C  COPYRIGHT 1998-2003 The University of Georgia, Griffin, Georgia
C                      University of Florida, Gainesville, Florida
C                      Iowa State University, Ames, Iowa
C                      International Center for Soil Fertility and 
C                       Agricultural Development, Muscle Shoals, Alabama
C                      University of Guelph, Guelph, Ontario
C  ALL RIGHTS RESERVED
C=======================================================================
C=======================================================================
C  PEST, Subroutine
C  Calculates pest damage.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  02/23/1998 CHP Written based on Pest damage code in PLANT subroutine
C  01/12/1999 GH  Incorporated into CROPGRO
C  06/19/2001 GH  Added mowing option
C  01/28/2002 GH  Expanded number of pests to 100 (from 40)
C  05/09/2003 CHP Expanded number of pests to 200 (from 100)
C-----------------------------------------------------------------------
C  Called by: CROPGRO
C                  ML_CERES
C                  MZCERES
C                  SGCERES
C  Calls:     FOR_ASMDM
C             FOR_IPPARM
C             FOR_IPPEST
C             FOR_IPPROG
C             FOR_LINDM
C             FOR_OPPEST
C             FOR_PESTCP
C             FOR_ROOTDM
C             FOR_SEEDDM
C             FOR_VEGDM
C=======================================================================
      SUBROUTINE FOR_PEST(CONTROL, ISWITCH, 
     &    AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, PGAVL,    !Input
     &    PHTIM, PLTPOP, RTWT, SLA, SLDOT, SOILPROP,      !Input
     &    SSDOT, STMWT, TOPWT, WLFDOT, WTLF, YRPLT,       !Input
     &    RLV, SDNO, SHELN, SWIDOT,                       !Input/Output
     &    VSTAGE, WSHIDT, WTSD, WTSHE,                    !Input/Output
     &    ASMDOT, DISLA, HPDAM, NPLTD, PPLTD,             !Output
     &    SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT,             !Output
     &  CSRW, SSRDOT, STRWT, WSFDOT, WSRFDOT,             !Input
     &  WSRIDOT,                                          !Output

     &  CSFRZ, CSRFRZ, CSTRM, DSTOR, SRDAM)               !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
        ! which contain control information, soil
        ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL FOR_IPPEST, FOR_IPPARM, FOR_IPPROG, FOR_PESTCP, 
     &  FOR_SEEDDM, FOR_VEGDM, FOR_ROOTDM, FOR_OPPEST, 
     &  FOR_LINDM, TIMDIF, ASMDM
      SAVE
!-----------------------------------------------------------------------
      CHARACTER*1   IDETD
      CHARACTER*5   PSTHD(6)
      CHARACTER*5   PID(200)
      CHARACTER*5   PCPID(200,6)
      CHARACTER*12  FILEP, FILET
      CHARACTER*30  FILEIO
      CHARACTER*80  PATHPE

      INTEGER DYNAMIC, LUNIO, MULTI, PCN

      INTEGER TRTNO, NR2
      INTEGER YRDOY, YRPLT, YRSIM, DAP, DAS, NPEST
      INTEGER TIMDIF
      INTEGER PNO(6), POBS(6)
      INTEGER PCTID(200)
      INTEGER IDAP(6,200)

      REAL PHTHRS8, SLA
      REAL PL(6)
      REAL PHTIM(365)
      REAL PDCF1(200,6)
      REAL YPL(6,200)

C     Leaf Variables
      REAL TLFAD, PLFAD, TLFMD, PLFMD, PCLMT, PCLMA
      REAL CLFM, CLAI, LAIDOT, WLIDOT
      REAL WTLF, SLDOT
      REAL DISLA, DISLAP, AREALF
      REAL CLW, WLFDOT
C     Diseased Leaf Area
      REAL PDLA, TDLA
C     Stem Variables
      REAL WSTMD, PSTMD, PCSTMD, CSTEM
      REAL WSIDOT
      REAL STMWT
      REAL VSTAGE, PVSTGD, VSTGD
      REAL CSW, SSDOT
      REAL CSFRZ, WSFDOT
C     Root Variables
      REAL WRTMD, PRTMD, TRTLV, PRTLV, PRTLF, TRTLF, WRIDOT
      REAL RLVDOT, RLFDOT, CRLV, CRLF, CRTM
      REAL RTWT
      REAL RLV(NL)
      REAL PRLV
C     Seed Variables
      REAL NSDDS, NSDDL, NSDDM, PSDDS, PSDDL, PSDDM, WSDDS, WSDDL, WSDDM
      REAL CSDM, CSDN
      REAL SDDES(NCOHORTS), SDIDOT, SWIDOT
      REAL TSDNOS, TSDNOL, TSDNOM, TSDWTS, TSDWTL, TSDWTM
      REAL LAGSD, LNGPEG
      REAL WSDD,PSDD,SDWT
!     REAL SEEDNO
      REAL WTSD(NCOHORTS), SDNO(NCOHORTS)
C     Shell Variables
      REAL NSHDS, NSHDL, NSHDM, PSHDS, PSHDL, PSHDM, WSHDS, WSHDL, WSHDM
      REAL CSHM, CSHN
      REAL SHIDOT, WSHIDT
      REAL TSHNOS, TSHNOL, TSHNOM, TSHWTS, TSHWTL, TSHWTM
      REAL  WTSHE(NCOHORTS), SHELN(NCOHORTS)
C     Plant Variables
      REAL NPLTD, PPLTD, CPPLTD, TOPWT
      REAL PLTPOP
C     Photosynthesis Variables
      REAL TPSR, PPSR, CASM, ASMDOT
      REAL PGAVL

C     Storage organ Variables
      REAL CSRW, CSTRM, SSRDOT, STRWT, WSRFDOT, WSRIDOT

      REAL CSRFRZ, DSTOR, SRDAM
      REAL PCSTRD, PSTRD, WSTRD     

      
!      SAVE CASM, CRLV, CRLF, CRTM
!      SAVE CSDM, CSDN, CSHM, CSHN
!      SAVE CLAI, CLFM, CSTEM

C     Harvest damage/removal Variables
      REAL HPDAM

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

!     The variable "SOILPROP" is of type "SoilType".
      TYPE (SoilType) SOILPROP

!     The variable "ISWITCH" is of type "SwitchType".
      TYPE (SwitchType) ISWITCH

!     Transfer values from constructed data types into local variables.
      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO
      MULTI   = CONTROL % MULTI
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM

      IDETD = ISWITCH % IDETD

C***********************************************************************
C***********************************************************************
!     Run Initialization - Called once per simulation
C***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN


C-----------------------------------------------------------------------
C     Call FOR_IPPEST to read data from FILEIO
C-----------------------------------------------------------------------
      CALL FOR_IPPEST(
     &    FILEIO, LUNIO,                                  !Input
     &    FILEP, FILET, PATHPE, PHTHRS8, TRTNO)           !Output

C-----------------------------------------------------------------------
C     Subroutine FOR_IPPARM reads FILEP, the PEST progress file.
C-----------------------------------------------------------------------
      CALL FOR_IPPARM(
     &    FILEP, PATHPE,                                  !Input
     &    NPEST, PCPID, PCTID, PDCF1, PID)                !Output

!      IF (IDETD .EQ. 'Y') THEN
!!     Initialize (or delete existing) output file.
!        CALL FOR_OPPEST(CONTROL, ISWITCH, 
!     &    ASMDOT, CASM, CLAI, CLFM, CPPLTD, CRLF, CRLV,      
!     &    CRTM, CSDM, CSDN, CSHM, CSHN, CSTEM, DISLA, DISLAP,   
!     &    LAIDOT, PPLTD, RLFDOT, RLVDOT, SDIDOT, SHIDOT, 
!     &    SWIDOT, WLIDOT, WRIDOT, WSIDOT, WSHIDT, YRPLT)     
!      ENDIF

C***********************************************************************
C***********************************************************************
!     Seasonal initialization - run once per season
C***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
C     Subroutine FOR_IPPROG reads FILET, the pest time series file.
C-----------------------------------------------------------------------
      CALL FOR_IPPROG(CONTROL, 
     &    FILET, NPEST, PID, YRPLT, TRTNO,                !Input
     &    IDAP, PCN, PNO, POBS, PSTHD, YPL)               !Output
C-----------------------------------------------------------------------
C  Initialize whole plant factors
C-----------------------------------------------------------------------
      CALL FOR_PESTCP(
     &    PCN, PCPID, PCTID, PDCF1,                       !Input
     &    PL, PLTPOP, PNO, RTWT, SLA, STMWT, TOPWT,        !Input

     &    TSDNOL, TSDNOM, TSDNOS, TSDWTL, TSDWTM, TSDWTS, !Input
     &    TSHNOL, TSHNOM, TSHNOS, TSHWTL, TSHWTM, TSHWTS, !Input
     &    VSTAGE, WTLF,                                   !Input
     &    NSDDL, NSDDM, NSDDS, NSHDL, NSHDM, NSHDS,       !Input/Output
     &    PPLTD, TLFAD, TLFMD, TRTLV,                     !Input/Output
     &    WRTMD, WSDDL, WSDDM, WSDDS,                     !Input/Output
     &    WSHDL, WSHDM, WSHDS,                            !Input/Output
     &    CPPLTD, HPDAM, NPLTD, PCLMA, PCLMT,             !Output
     &    PCSTMD, PDLA, PLFAD, PLFMD, PPSR,               !Output
     &    PRTLF, PRTLV, PRTMD, PSDDL, PSDDM, PSDDS,       !Output
     &    PSHDL, PSHDM, PSHDS, PSTMD, PVSTGD,             !Output
     &    TDLA, TPSR, TRTLF, VSTGD, WSTMD,                !Output

     &  PCSTRD, PSTRD, WSTRD,                             !Output

     &    SEASINIT,WSDD,PSDD,PRLV)
C-----------------------------------------------------------------------
C  Initialize assimilate, seed, vegetative and root pest damage factors
C-----------------------------------------------------------------------
      CALL ASMDM(
     &    PGAVL, PPSR, TPSR,                              !Input
     &    ASMDOT, CASM,                                   !Output
     &    SEASINIT)                                       !Control

      CALL FOR_SEEDDM(
     &    DAS, LAGSD, LNGPEG, NR2, PHTIM, PHTHRS8,        !Input
     &    PSDDL, PSDDM, PSDDS, PSHDL, PSHDM, PSHDS,       !Input
     &    NSDDL, NSDDM, NSDDS, NSHDL, NSHDM, NSHDS,       !Input/Output
     &    SDNO, SHELN, SWIDOT,                            !Input/Output
     &    WSDDL, WSDDM, WSDDS,                            !Input/Output
     &    WSHDL, WSHDM, WSHDS, WTSD, WTSHE, WSHIDT,       !Input/Output
     &    CSDM, CSDN, CSHM, CSHN, SDDES, SDIDOT,          !Output
     &    SHIDOT, TSDNOL, TSDNOM, TSDNOS, TSDWTL,         !Output
     &    TSDWTM, TSDWTS, TSHNOL, TSHNOM, TSHNOS,         !Output
     &    TSHWTL, TSHWTM, TSHWTS,                         !Output
     &    SEASINIT,WSDD,PSDD,SDWT)                             !Control

      CALL FOR_VEGDM(
     &    AREALF, CLW, CSW, PCLMT,                        !Input
     &    PCSTMD, PDLA, PLFAD, PLFMD, PSTMD,              !Input
     &    PVSTGD, SLA, SLDOT, SSDOT, STMWT,               !Input
     &    TDLA, VSTGD, WLFDOT, WSTMD, WTLF,               !Input
     &    TLFAD, TLFMD, VSTAGE, WLIDOT,                   !Input/Output
     &    CLAI, CLFM, CSTEM, DISLA, DISLAP,               !Output
     &    LAIDOT, WSIDOT,                                 !Output
     &    CSRW, PCSTRD, PSTRD, SSRDOT, STRWT,             !Input
     &    WSFDOT, WSRFDOT, WSTRD,                         !Input
     &    CSTRM, WSRIDOT,                                 !Output
     &    CSFRZ, CSRFRZ, DSTOR, SRDAM,                    !Output
     &    SEASINIT)                                       !Control

      CALL FOR_ROOTDM(
     &    PRLV,PRTLF, PRTLV, PRTMD, RTWT, SOILPROP, TRTLF,    !Input
     &    RLV, TRTLV, WRTMD,                              !Input/Output
     &    CRLF, CRLV, CRTM, RLFDOT, RLVDOT, WRIDOT,       !Output
     &    SEASINIT)                                       !Control

      !IF (IDETD .EQ. 'Y') THEN
        CALL FOR_OPPEST(CONTROL, ISWITCH, 
     &    ASMDOT, CASM, CLAI, CLFM, CPPLTD, CRLF, CRLV,      
     &    CRTM, CSDM, CSDN, CSHM, CSHN, CSTEM, DISLA, DISLAP,   
     &    LAIDOT, PPLTD, RLFDOT, RLVDOT, SDIDOT, SHIDOT, 
     &    SWIDOT, WLIDOT, WRIDOT, WSIDOT, WSHIDT, YRPLT)     
      !ENDIF

!***********************************************************************
!***********************************************************************
!     Daily rate calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
C-----------------------------------------------------------------------
C     Interpolate between pest damage factors to get today's pest level.
C-----------------------------------------------------------------------
      DAP   = MAX(0,TIMDIF(YRPLT,YRDOY))
      CALL FOR_LINDM(
     &    DAP, IDAP, PCN, YPL,                            !Input
     &    PL)                                             !Output

C-----------------------------------------------------------------------
C     Compute damage applied to each coupling point.
C-----------------------------------------------------------------------
      CALL FOR_PESTCP(
     &    PCN, PCPID, PCTID, PDCF1,                       !Input
     &    PL, PLTPOP, PNO, RTWT, SLA, STMWT, TOPWT,       !Input
     &    TSDNOL, TSDNOM, TSDNOS, TSDWTL, TSDWTM, TSDWTS, !Input
     &    TSHNOL, TSHNOM, TSHNOS, TSHWTL, TSHWTM, TSHWTS, !Input
     &    VSTAGE, WTLF,                                   !Input
     &    NSDDL, NSDDM, NSDDS, NSHDL, NSHDM, NSHDS,       !Input/Output
     &    PPLTD, TLFAD, TLFMD, TRTLV,                     !Input/Output
     &    WRTMD, WSDDL, WSDDM, WSDDS,                     !Input/Output
     &    WSHDL, WSHDM, WSHDS,                            !Input/Output
     &    CPPLTD, HPDAM, NPLTD, PCLMA, PCLMT,             !Output
     &    PCSTMD, PDLA, PLFAD, PLFMD, PPSR,               !Output
     &    PRTLF, PRTLV, PRTMD, PSDDL, PSDDM, PSDDS,       !Output
     &    PSHDL, PSHDM, PSHDS, PSTMD, PVSTGD,             !Output
     &    TDLA, TPSR, TRTLF, VSTGD, WSTMD,                !Output

     &  PCSTRD, PSTRD, WSTRD,                             !Output

     &    RATE,WSDD,PSDD,PRLV)

      CALL FOR_SEEDDM(
     &    DAS, LAGSD, LNGPEG, NR2, PHTIM, PHTHRS8,        !Input
     &    PSDDL, PSDDM, PSDDS, PSHDL, PSHDM, PSHDS,       !Input
     &    NSDDL, NSDDM, NSDDS, NSHDL, NSHDM, NSHDS,       !Input/Output
     &    SDNO, SHELN, SWIDOT,                            !Input/Output
     &    WSDDL, WSDDM, WSDDS,                            !Input/Output
     &    WSHDL, WSHDM, WSHDS, WTSD, WTSHE, WSHIDT,       !Input/Output
     &    CSDM, CSDN, CSHM, CSHN, SDDES, SDIDOT,          !Output
     &    SHIDOT, TSDNOL, TSDNOM, TSDNOS, TSDWTL,         !Output
     &    TSDWTM, TSDWTS, TSHNOL, TSHNOM, TSHNOS,         !Output
     &    TSHWTL, TSHWTM, TSHWTS,                         !Output
     &    RATE, WSDD,PSDD,SDWT)                           !Control
C-----------------------------------------------------------------------
C     Call vegetative pest damage routine and compute damage rates
C-----------------------------------------------------------------------
      CALL FOR_VEGDM(
     &    AREALF, CLW, CSW, PCLMT,                        !Input
     &    PCSTMD, PDLA, PLFAD, PLFMD, PSTMD,              !Input
     &    PVSTGD, SLA, SLDOT, SSDOT, STMWT,               !Input
     &    TDLA, VSTGD, WLFDOT, WSTMD, WTLF,               !Input
     &    TLFAD, TLFMD, VSTAGE, WLIDOT,                   !Input/Output
     &    CLAI, CLFM, CSTEM, DISLA, DISLAP,               !Output
     &    LAIDOT, WSIDOT,                                 !Output
     &    CSRW, PCSTRD, PSTRD, SSRDOT, STRWT,             !Input
     &    WSFDOT, WSRFDOT, WSTRD,                         !Input
     &    CSTRM, WSRIDOT,                                 !Output
     &    CSFRZ, CSRFRZ, DSTOR, SRDAM,                    !Output
     &    RATE)                                           !Control
C-----------------------------------------------------------------------
C     Call root pest damage routine and compute damage rates
C-----------------------------------------------------------------------
      CALL FOR_ROOTDM(
     &    PRLV,PRTLF, PRTLV, PRTMD, RTWT, SOILPROP, TRTLF,   !Input
     &    RLV, TRTLV, WRTMD,                              !Input/Output
     &    CRLF, CRLV, CRTM, RLFDOT, RLVDOT, WRIDOT,       !Output
     &    RATE)                                           !Control

!***********************************************************************
!***********************************************************************
!     Daily integration
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
C-----------------------------------------------------------------------
C     Call assimilative damage routine to update assimilative damage
!          variables.
C-----------------------------------------------------------------------
      CALL ASMDM(
     &    PGAVL, PPSR, TPSR,                              !Input
     &    ASMDOT, CASM,                                   !Output
     &    INTEGR)                                         !Control
C-----------------------------------------------------------------------
C     Call routine to apply damage to seed and shell
C-----------------------------------------------------------------------
      CALL FOR_SEEDDM(
     &    DAS, LAGSD, LNGPEG, NR2, PHTIM, PHTHRS8,        !Input
     &    PSDDL, PSDDM, PSDDS, PSHDL, PSHDM, PSHDS,       !Input
     &    NSDDL, NSDDM, NSDDS, NSHDL, NSHDM, NSHDS,       !Input/Output
     &    SDNO, SHELN, SWIDOT,                            !Input/Output
     &    WSDDL, WSDDM, WSDDS,                            !Input/Output
     &    WSHDL, WSHDM, WSHDS, WTSD, WTSHE, WSHIDT,       !Input/Output
     &    CSDM, CSDN, CSHM, CSHN, SDDES, SDIDOT,          !Output
     &    SHIDOT, TSDNOL, TSDNOM, TSDNOS, TSDWTL,         !Output
     &    TSDWTM, TSDWTS, TSHNOL, TSHNOM, TSHNOS,         !Output
     &    TSHWTL, TSHWTM, TSHWTS,                         !Output
     &    INTEGR, WSDD,PSDD,SDWT)                         !Control
C-----------------------------------------------------------------------
C     Call routine to apply damage to leaf and stem
C-----------------------------------------------------------------------
      CALL FOR_VEGDM(
     &    AREALF, CLW, CSW, PCLMT,                        !Input
     &    PCSTMD, PDLA, PLFAD, PLFMD, PSTMD,              !Input
     &    PVSTGD, SLA, SLDOT, SSDOT, STMWT,               !Input
     &    TDLA, VSTGD, WLFDOT, WSTMD, WTLF,               !Input
     &    TLFAD, TLFMD, VSTAGE, WLIDOT,                   !Input/Output
     &    CLAI, CLFM, CSTEM, DISLA, DISLAP,               !Output
     &    LAIDOT, WSIDOT,                                 !Output
     &    CSRW, PCSTRD, PSTRD, SSRDOT, STRWT,             !Input
     &    WSFDOT, WSRFDOT, WSTRD,                         !Input
     &    CSTRM, WSRIDOT,                                 !Output
     &    CSFRZ, CSRFRZ, DSTOR, SRDAM,                    !Output
     &    INTEGR)                                         !Control
C-----------------------------------------------------------------------
C     Call root pest damage routine and compute damage factors
C-----------------------------------------------------------------------
      CALL FOR_ROOTDM(
     &    PRLV, PRTLF, PRTLV, PRTMD, RTWT, SOILPROP, TRTLF,     !Input
     &    RLV, TRTLV, WRTMD,                              !Input/Output
     &    CRLF, CRLV, CRTM, RLFDOT, RLVDOT, WRIDOT,       !Output
     &    INTEGR)                                         !Control

!***********************************************************************
!***********************************************************************
!     OUTPUT/SEASEND
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
      !IF (IDETD .EQ. 'Y') THEN
        CALL FOR_OPPEST(CONTROL, ISWITCH, 
     &    ASMDOT, CASM, CLAI, CLFM, CPPLTD, CRLF, CRLV,      
     &    CRTM, CSDM, CSDN, CSHM, CSHN, CSTEM, DISLA, DISLAP,   
     &    LAIDOT, PPLTD, RLFDOT, RLVDOT, SDIDOT, SHIDOT, 
     &    SWIDOT, WLIDOT, WRIDOT, WSIDOT, WSHIDT, YRPLT)     
      !ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      END  ! SUBROUTINE PEST

!-----------------------------------------------------------------------
!     PEST Variable Definitions
!-----------------------------------------------------------------------
! AREALF     Area of leaves (one side) per unit ground area
!              (cm2[leaf] / m2[ground])
! ASMDOT     Daily assimilative damage (g[CH2O] /m2 / d)
! CASM       Cumulative assimilate damage (g[CH2O] / m2)
! CLAI       Cumulative leaf area index destroyed (m2/m2)
! CLFM       Cumulative leaf mass destroyed  (g/m2)
! CLW        Cumulative leaf growth (g[leaf]/m2)
! CPPLTD     Cumulative percent of plants destroyed (%)
! CRLF       Cumulative root length flux (cm root / cm2 ground)
! CRLV       Cumulative root length density (cm root / cm3 soil)
! CROPD      Name of crop 
! CRTM       Cumulative root mass (g/m2)
! CSDM       Cumulative seed mass destroyed (g/m2)
! CSDN       Cumulative number of seeds destroyed (#/m2)
! CSFRZ      Cumulative frozen stem tissue (g[stem]/m2)
! CSHM       Cumulative shell mass destroyed (g/m2)
! CSHN       Cumulative number of shells destroyed (#/m2)
! CSRW         Cumulative storage organ growth (g[storage]/m2)
! CSTEM      Cumulative stem mass destroyed (g/m2)
! CSTRM         Cumulative storage organ mass destroyed (g/m2)      
! CSW        Cumulative stem growth (g[stem]/m2)
! DAP        Number of days after planting (d)
! DAS        Days after start of simulation (d)
! DISLA      Diseased leaf area (cm2[leaf]/m2[ground]/d)
! DISLAP     Percent diseased leaf area (%/d)
! DLAYR(L)   Soil Depth in layer L (cm)
! DYNAMIC    Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
!              INTEGR, OUTPUT, or SEASEND 
! FILEIO     Filename for input file (e.g., IBSNAT35.INP) 
! FILEP      Filename for pest coefficient file 
! FILET      Pest time series file 
! HPDAM         Harvest removal (proportion of TOPWT)
! IDAP(I,J)  Day of pest damage for pest I, observation J
!              (days after planting)
! IDETD      Switch to generate PEST.OUT file (Y or N) 
! LAGSD      Time required between shell growth and seed growth, per cohort
!              (Photo-thermal days)
! LAIDOT     Daily change in leaf area index due to pest damage (m2/m2/d)
! LNGPEG     Time between start of peg (full flower) and shell formation 
!              (for peanuts only).  Defines slow growth period.
!              (Photo-thermal days)
! LUNIO      Logical unit number for FILEIO 
! MULTI      Current simulation year (=1 for first or single simulation, 
!              =NYRS for last seasonal simulation) 
! NL         Maximum number of soil layers = 20 
! NLAYR      Actual number of soil layers 
! NPEST      Number of pest or damage types in FILEP 
! NPLTD      Number of plants destroyed (#/m2/d)
! NR2        Day when 50% of plants have one peg (peanuts only) (d)
! NSDDL      Daily number of large seeds damaged (#/m2/d)
! NSDDM      Daily number of mature seeds damaged (#/m2/d)
! NSDDS      Daily number of small seeds damaged (#/m2/d)
! NSHDL      Daily number of large shells damaged (#/m2/d)
! NSHDM      Daily number of mature shells damaged (#/m2/d)
! NSHDS      Daily number of small shells damaged (#/m2/d)
! OUTD       File name for pest damage output file (e.g., PEST.OUT) 
! PATHPE     Path name for pest information file 
! PCLMA      Percent observed leaf mass (WTLF) damage (%)
! PCLMT      Percent of total leaf mass (WTLF + senescence) destroyed (%)
! PCN        Number of pests in FILET 
! PCPID(I,J) Pest coupling points identification code for pest I, coupling 
!              point J 
! PCSTMD     Observed cumulative percentage stem mass damage (%)
! PCSTRD  Observed cumulative percentage storage organ mass damage (%)
! PCTID(I)   Pest damage characterization method for pest I: (1) absolute 
!              daily damage rate, (2) percent observed damage, (3) daily 
!              percent damage rate, (4) absolute daily damage rate w/ 
!              preference and competition. 
! PDCF1(I,J) Pest damage coefficient associated with pest I, coupling point 
!              J 
! PDLA       Percent diseased leaf area (%)
! PGAVL      Total available CH2O available for growth & respiration
!              (g[CH2O] / m2)
! PHTHRS8    Threshold time that must accumulate in phase 8 for the next 
!              stage to occur.  Equivalent to PHTHRS(8) in Subroutine 
!              PHENOLOG. (photothermal days)
! PHTIM      Cumulative photothermal time ages of seeds and shells 
! PID(I)     Pest identification header from FILEP (max 200) 
! PL(I)      Pest level from pest progress file (FILET) for pest I (varies)
! PLFAD      Daily percent leaf area damage (%/d)
! PLFMD      Percent leaf mass damage (%/d)
! PLTPOP     Plant population (# plants / m2)
! PNO(I)     Pest number from File P for pest I 
! POBS(I)    Number of observations for pest I 
! PPLTD      Percent plants destroyed  (%/m2/d)
! PPSR       Assimilate damage in daily percent photosynthesis reduction
!              (%/d)
! PRLV       Percent of root length volume destroyed, %
! PRTLF      Percent of root flux destroyed (%/d)
! PRTLV      Daily percent reduction in root length volume  (%/d)
! PRTMD      Daily percent root mass damage (%/d)
! PSDD       Percent of seed mass destroyed (%/d)
! PSDDL      Percent large seed mass damage (%/d)
! PSDDM      Percent mature seed mass damage (%/d)
! PSDDS      Percent small seed mass damage (%/d)
! PSHDL      Percent large shell mass damage (%/d)
! PSHDM      Percent mature shell mass damage (%/d)
! PSHDS      Percent small shell mass damage (%/d)
! PSTHD(I)   Column heading for each pest progress curve in FILET (max 6) 
! PSTMD      Daily percent stem mass damage (%)
! PVSTGD     Percent V-stage damage (%)
! RLFDOT     Daily root length flux damage (cm root / cm2 ground)
! RLV(L)     Root length density for soil layer L (cm[root] / cm3[soil])
! RLVDOT     Daily root length damage (cm root / cm3 soil)
! RTWT       Dry mass of root tissue, including C and N
!              (g[root] / m2[ground])
! SDDES(J)   Number of seeds destroyed today in cohort J when shells are 
!              not destroyed (#/m2/day)
! SDIDOT     Number of seeds destroyed on the current day (#/m2/d)
! SDNO(J)    Number of seeds for cohort J (#/m2)
! SDWT       Seed weight, g/m2
! SHELN(J)   Number of shells for cohort J (#/m2)
! SHIDOT     Number of shells destroyed on current day (#/m2/d)
! SLA        Specific leaf area (cm2[leaf] / m2[ground])
! SLDOT      Defoliation due to daily leaf senescence (g/m2/day)
! SSDOT      Daily senescence of petioles (g / m2 / d)
! SSRDOT         Daily senescence of storage organ (g / m2 / d)
! STRWT      Dry mass of storage organ tissue, including C and N
!              (g[storage] / m2[ground)
! STMWT      Dry mass of stem tissue, including C and N
!              (g[stem] / m2[ground)
! SWIDOT     Daily seed mass damage (g/m2/day)
! TDLA       Total diseased leaf area (cm2/m2)
! TIMDIF     Integer function which calculates the number of days between 
!              two Julian dates (da)
! TITLET     Description of treatment for this simulation 
! TLFAD      Total leaf area damage (cm2/cm2/d)
! TLFMD      Total leaf mass damage (g/m2/day)
! TOPWT      Total above ground biomass (g/m2)
! TPSR       Daily absolute assimilate damage (g[CH2O]/m2/d)
! TRTLF      Total root flux destroyed (cm/cm2/d)
! TRTLV      Daily absolute reduction in root length volume  (cm/cm3/d)
! TRTNO      Treatment number being simulated (from FILEX) 
! TSDNOL     Total number of large seeds (#/m2)
! TSDNOM     Total number of mature seeds (#/m2)
! TSDNOS     Total number of small seeds (#/m2)
! TSDWTL     Seed mass for large seeds (g/m2)
! TSDWTM     Seed mass for mature seeds (g/m2)
! TSDWTS     Seed mass for small seeds (g/m2)
! TSHNOL     Number shells with large seeds (#/m2)
! TSHNOM     Number shells with mature seeds (#/m2)
! TSHNOS     Number shells with small seeds (#/m2)
! TSHWTL     Shell mass with large seeds (g/m2)
! TSHWTM     Shell mass with mature seeds (g/m2)
! TSHWTS     Shell mass with small seeds (g/m2)
! VSTAGE     Number of nodes on main stem of plant (nodes)
! VSTGD      Absolute daily V-stage damage (nodes/day)
! WLFDOT     Leaf weight losses due to freezing (g[leaf]/m2-d)
! WLIDOT     Daily pest or freeze damage to leaf mass (g/m2/day)
! WRIDOT     Daily pest damage to root mass (g/m2/day)
! WRTMD      Daily absolute root mass reduction  (g/m2/day)
! WSDD       Daily weight of seeds destroyed (g/m2/d)
! WSDDL      Daily percent of large seed damaged (g/m2/day)
! WSDDM      Daily percent of mature seed damaged (g/m2/day)
! WSDDS      Daily percent of small seed damaged (g/m2/day)
! WSFDOT        Stem weight losses due to freezing (g[stem]/m2-d)
! WSHDL      Daily mass of large shell damaged (g/m2/day)
! WSHDM      Daily mass of mature shell damaged (g/m2/day)
! WSHDS      Daily mass of small shell damaged (g/m2/day)
! WSHIDT     Weight of shell tissue consumed by pests today (g[shell]/m2-d)
! WSIDOT     Daily pest damage to stem mass (g/m2/day)
! WSRFDOT    Storage organ weight losses due to freezing (g[storage]/m2-d)
! WSRIDOT    Daily pest damage to storage organ mass (g/m2/day)
! WSTMD      Daily absolute stem damage (g/m2/day)
! WTLF       Dry mass of leaf tissue including C and N
!              (g[leaf] / m2[ground])
! WTSD(J)    Seed mass  for cohort J (g/m2)
! WTSHE(J)   Shell mass  for cohort J (g/m2)
! YPL(I,J)   Array for storage of pest data for pest or damage type I, 
!              observation J 
! YRDOY      Current day of simulation (YYDDD)
! YRPLT      Planting date (YYDDD)
! YRSIM      Start of simulation date (YYDDD)
!-----------------------------------------------------------------------
!     End Subroutine PEST
!-----------------------------------------------------------------------

