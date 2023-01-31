C=======================================================================
C  FOR_PESTCP, Subroutine
C-----------------------------------------------------------------------
C     This subroutine computes damage to be applied to each
C     coupling point in the model.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  01/01/1991 WDB Written
C  02/23/1998 CHP Modified for PEST Module
C  01/12/1999 GH  Incorporated into CROPGRO
C  06/20/2001 GH  Added mowing option
C  04/15/2002 GH  Modified number of pests to 100
C  05/09/2003 CHP Modified number of pests to 200
C-----------------------------------------------------------------------
C  Called by: PEST
C  Calls:
C=======================================================================
      SUBROUTINE FOR_PESTCP(
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

     &    DYNAMIC,WSDD,PSDD,PRLV)

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
        ! which contain control information, soil
        ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      CHARACTER*5 PCPID(200,6)

      INTEGER DYNAMIC
      INTEGER I,J,K
      INTEGER PCN
      INTEGER PNO(6)
      INTEGER PCTID(200)

      REAL FSM
      REAL PDAM,DAM,DDAM,REMDAM
      REAL TSDNOS,TSDNOL,TSDNOM,TSDWTS,TSDWTL,TSDWTM
      REAL TSHNOS,TSHNOL,TSHNOM,TSHWTS,TSHWTL,TSHWTM
      REAL WTLF, SLA, STMWT, RTWT, PLTPOP
      REAL PL(6)
      REAL PDCF1(200,6)

C     Leaf Variables
      REAL TLFAD,PLFAD,TLFMD,PLFMD,PCLMT,PCLMA
C     Diseased Leaf Area
      REAL PDLA,TDLA
C     Stem Variables
      REAL WSTMD,PSTMD,PCSTMD
C     Vstage variables
      REAL VSTGD, PVSTGD, VSTAGE
C     Root Variables
      REAL WRTMD,PRTMD,TRTLV,PRTLV,PRTLF,TRTLF,PRLV
C     Seed Variables
      REAL NSDDS,PSDDS,NSDDL,PSDDL,NSDDM,PSDDM,WSDDS,WSDDL,WSDDM
      REAL PSDD,WSDD
C     Shell Variables
      REAL NSHDS,NSHDL,NSHDM,PSHDS,PSHDL,PSHDM,WSHDS,WSHDL,WSHDM
C     Plant Variables
      REAL NPLTD,PPLTD,CPPLTD,TOPWT
C     Photosynthesis Variables
      REAL TPSR,PPSR
C     Weed Leaf Area Variables (Light Competition)
      REAL LAIW,PLAIW
C     Storage Variables
      REAL PCSTRD, PSTRD, WSTRD
C      Special forage harvest variables
      REAL HPDAM

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      CPPLTD = 0.0
      PL     = 0.0
C-----------------------------------------------------------------------
C     Initialization added by CHP in newer versions
C-----------------------------------------------------------------------
C -- Leaf Variables --
      TLFAD  = 0.0
      TLFMD  = 0.0
      PLFAD  = 0.0
      PLFMD  = 0.0
      PCLMT  = 0.0
      PCLMA  = 0.0
      PDLA   = 0.0
      TDLA   = 0.0
      PVSTGD = 0.0
      VSTGD  = 0.0
C -- Stem Variables --
      WSTMD  = 0.0
      PSTMD  = 0.0
      PCSTMD = 0.0
C -- Root Variables --
      WRTMD  = 0.0
      PRTMD  = 0.0
      TRTLV  = 0.0
      PRTLV  = 0.0
      PRTLF  = 0.0
      TRTLF  = 0.0
      PRLV   = 0.0
C -- Seed Variables --
      NSDDS  = 0.0
      PSDDS  = 0.0
      NSDDM  = 0.0
      PSDDM  = 0.0
      NSDDL  = 0.0
      PSDDL  = 0.0
      WSDDS  = 0.0
      WSDDM  = 0.0
      WSDDL  = 0.0
      WSDD   = 0.0
      PSDD   = 0.0
C -- Shell Variables --
      NSHDS  = 0.0
      NSHDM  = 0.0
      NSHDL  = 0.0
      PSHDS  = 0.0
      PSHDM  = 0.0
      PSHDL  = 0.0
      WSHDS  = 0.0
      WSHDM  = 0.0
      WSHDL  = 0.0
C -- Whole Plant Variables --
      NPLTD  = 0.0
      PPLTD  = 0.0
C -- Weed Leaf Area Variables --
      LAIW   = 0.0
      PLAIW  = 0.0
C -- Photosynthesis Variables --
      TPSR   = 0.0
      PPSR   = 0.0
C -- Other Variables --
      PDAM   = 0.0
      FSM    = 0.0
      HPDAM  = 0.0
!***********************************************************************
!***********************************************************************
!     Daily rate calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
C-----------------------------------------------------------------------
C     Initialize coupling point damage variables each day of simulation
C-----------------------------------------------------------------------
C -- Leaf Variables --
      TLFAD  = 0.0
      TLFMD  = 0.0
      PLFAD  = 0.0
      PLFMD  = 0.0
      PCLMT  = 0.0
      PCLMA  = 0.0
      PDLA   = 0.0
      TDLA   = 0.0
      PVSTGD = 0.0
      VSTGD  = 0.0
C -- Stem Variables --
      WSTMD  = 0.0
      PSTMD  = 0.0
      PCSTMD = 0.0
C -- Root Variables --
      WRTMD  = 0.0
      PRTMD  = 0.0
      TRTLV  = 0.0
      PRTLV  = 0.0
      PRTLF  = 0.0
      TRTLF  = 0.0
      PRLV   = 0.0
C -- Seed Variables --
      NSDDS  = 0.0
      PSDDS  = 0.0
      NSDDM  = 0.0
      PSDDM  = 0.0
      NSDDL  = 0.0
      PSDDL  = 0.0
      WSDDS  = 0.0
      WSDDM  = 0.0
      WSDDL  = 0.0
      WSDD   = 0.0
      PSDD   = 0.0
C -- Shell Variables --
      NSHDS  = 0.0
      NSHDM  = 0.0
      NSHDL  = 0.0
      PSHDS  = 0.0
      PSHDM  = 0.0
      PSHDL  = 0.0
      WSHDS  = 0.0
      WSHDM  = 0.0
      WSHDL  = 0.0
C -- Whole Plant Variables --
      NPLTD  = 0.0
      PPLTD  = 0.0
C -- Weed Leaf Area Variables --
      LAIW   = 0.0
      PLAIW  = 0.0
C -- Photosynthesis Variables --
      TPSR   = 0.0
      PPSR   = 0.0
C -- Other Variables --
      PDAM   = 0.0
      FSM    = 0.0
      HPDAM  = 0.0

C -- Storage Variables --
      WSTRD  = 0.0
      PSTRD  = 0.0
      PCSTRD = 0.0

!***********************************************************************
!     PEST LOOP
!     K increments through pests in FILET.
!-----------------------------------------------------------------------
      DO 8000 K=1,PCN
C--------------------------------------------------------------------
!     Variable PNO(K) refers to the pest number from FILEP.
C--------------------------------------------------------------------
        I = PNO(K)
!-----------------------------------------------------------------------
!     Bypass pest coupling point loop if pest damage characterization
!         method is invalid (PCTID>4 or PCTID<1).
!-----------------------------------------------------------------------
        IF (PCTID(I) .GT. 0 .AND. PCTID(I) .LE. 4) THEN
        REMDAM = 0.0
!***********************************************************************
!     COUPLING POINT LOOP
C     J increments through coupling points in pest coefficient file.
!-----------------------------------------------------------------------
        DO 7000 J=1,6
        IF(PCPID(I,J) .EQ. 'xxxxx') GO TO 8000
!-----------------------------------------------------------------------
!   Begin PCTID "IF" construct
C   Values of PCTID:
C   1 = Absolute daily damage rate
C   2 = Percent Apparent observed damage
C   3 = daily percent damage rate
C   4 = absolute damage rate with pref and competition
!***********************************************************************
        IF (PCTID(I) .EQ. 1) THEN
C-----------------------------------------------------------------------
C     Compute damage without preference and competition
C-----------------------------------------------------------------------
        DAM = PL(K) * PDCF1(I,J)
!***********************************************************************
        ELSEIF (PCTID(I) .EQ. 2) THEN
C-----------------------------------------------------------------------
C     Observed percent leaf or stem damage selected
C-----------------------------------------------------------------
        DAM = PL(K) * PDCF1(I,J)
C     ----------------------------------------------------------------
C     If leaf damage is specified as percent of total leaf mass produced
C     (WTLF + SENESENCE), then the internal damage variable is PCLMT
C     -----------------------------------------------------------------
        IF (INDEX (PCPID(I,J),'LMD') .GT. 0 .OR. 
     &    INDEX(PCPID(I,J),'LAD') .GT. 0) THEN
        PCLMT = PCLMT + DAM
        ENDIF

C     -----------------------------------------------------------------
C     Observed percent stem damage
C     -----------------------------------------------------------------
        IF (INDEX (PCPID(I,J),'SMD') .GT. 0) THEN
        PCSTMD = PCSTMD + DAM
        ENDIF

C     ----------------------------------------------------------------
C     If leaf damage is specified as percent of present leaf mass, WTLF,
C     then the internal damage variable is PCLMA
C     ----------------------------------------------------------------
C     PERCENT TOTAL LEAF IN FIELD (WTLF)
!              IF (INDEX (PCPID(I,J),'LMD1') .GT. 0 .OR. 
!     &        INDEX (PCPID(I,J),'LAID1') .GT. 0)  THEN
!                PCLMA = PCLMA + DAM
!              ENDIF

!************************************************************************
        ELSEIF (PCTID(I) .EQ. 3) THEN
C------------------------------------------------------------------------
C     Apply damage as percent damage rate per day
!     Note:  these daily percent damages should be accumulated using:
!         (1 - a) * (1 - b) * (1 - c) where a, b and c are the individual
!         daily percent damages.
C-------------------------------------------------------------------------
        DAM = PL(K) * PDCF1(I,J)

        IF (INDEX (PCPID(I,J),'LAD') .GT. 0) PLFAD = PLFAD + DAM
        IF (INDEX (PCPID(I,J),'LMD') .GT. 0) PLFMD = PLFMD + DAM
        IF (INDEX (PCPID(I,J),'SMD') .GT. 0) PSTMD = PSTMD + DAM
        IF (INDEX (PCPID(I,J),'RMD') .GT. 0) PRTMD = PRTMD + DAM
        IF (INDEX (PCPID(I,J),'RLV') .GT. 0) PRTLV = PRTLV + DAM
        IF (INDEX (PCPID(I,J),'RLF') .GT. 0) PRTLF = PRTLF + DAM
        IF (INDEX (PCPID(I,J),'SDNS') .GT. 0) PSDDS = PSDDS + DAM
        IF (INDEX (PCPID(I,J),'SDNL') .GT. 0) PSDDL = PSDDL + DAM
        IF (INDEX (PCPID(I,J),'SDNM') .GT. 0) PSDDM = PSDDM + DAM
        IF (INDEX (PCPID(I,J),'SDNL') .GT. 0) PSDDL = PSDDL + DAM

        IF (INDEX (PCPID(I,J),'SDM')  .GT. 0) PSDD = PSDD+DAM

        IF (INDEX (PCPID(I,J),'SDMS') .GT. 0) PSDDS = PSDDS + DAM
        IF (INDEX (PCPID(I,J),'SDMM') .GT. 0) PSDDM = PSDDM + DAM
        IF (INDEX (PCPID(I,J),'SDML') .GT. 0) PSDDL = PSDDL + DAM
        IF (INDEX (PCPID(I,J),'SHNS') .GT. 0) PSHDS = PSHDS + DAM
        IF (INDEX (PCPID(I,J),'SHNM') .GT. 0) PSHDM = PSHDM + DAM
        IF (INDEX (PCPID(I,J),'SHNL') .GT. 0) PSHDL = PSHDL + DAM
        IF (INDEX (PCPID(I,J),'SHMS') .GT. 0) PSHDS = PSHDS + DAM
        IF (INDEX (PCPID(I,J),'SHMM') .GT. 0) PSHDM = PSHDM + DAM
        IF (INDEX (PCPID(I,J),'SHML') .GT. 0) PSHDL = PSHDL + DAM

        IF (INDEX (PCPID(I,J),'VSTG') .GT. 0)PVSTGD = PVSTGD + DAM
        IF (INDEX (PCPID(I,J),'WPD') .GT. 0) THEN
        PPLTD = PPLTD + DAM
        PPLTD = MIN(100.,PPLTD)
        CPPLTD = CPPLTD + PPLTD
        PLFMD = PLFMD + DAM
        PSTMD = PSTMD + DAM
        PRTMD = PRTMD + DAM

        PSDD = PSDD + DAM

        PSDDS = PSDDS + DAM
        PSDDL = PSDDL + DAM
        PSDDM = PSDDM + DAM
        PSHDS = PSHDS + DAM
        PSHDL = PSHDL + DAM
        PSHDM = PSHDM + DAM
        ENDIF


        IF (INDEX (PCPID(I,J),'PPLD') .GT. 0) THEN
        PPLTD = PPLTD + DAM
        PPLTD = MIN(100.,PPLTD)
        CPPLTD = CPPLTD + PPLTD
        PRLV = PRLV + DAM
C                PLFMD = PLFMD + DAM
C                PSTMD = PSTMD + DAM
C                PRTMD = PRTMD + DAM
C                PSDD = PSDD + DAM
        ENDIF

        IF (INDEX (PCPID(I,J),'PPDN') .GT. 0) THEN
        PSDDS = PSDDS + DAM
        PSDDL = PSDDL + DAM
        PSDDM = PSDDM + DAM
        PSHDS = PSHDS + DAM
        PSHDL = PSHDL + DAM
        PSHDM = PSHDM + DAM
        ENDIF

        IF (INDEX (PCPID(I,J),'WLA') .GT. 0) THEN 
C             Note:  PLAIW is calculated here, but not used anywhere.
        PLAIW = PLAIW + DAM
        ENDIF

        IF (INDEX (PCPID(I,J),'PDLA').GT.0) THEN
        PDLA = PDLA + DAM
        ENDIF

        IF (INDEX (PCPID(I,J),'ASM') .GT. 0) THEN
        !Accumulate daily percentages
        PPSR = (1.0 - (1.0 - PPSR) * (1.0 - DAM))
        ENDIF
C-----------------------------------------------------------------------
C           Mowing option
C-----------------------------------------------------------------------
        IF (INDEX( PCPID(I,J),'TOPWT') .GT. 0.0) THEN
C-----------------------------------------------------------------------
C  SJR 3/16/04 Divide PL(K) by 10 to allow MOW input as kg/ha
C                        instead of old g/m2
C-----------------------------------------------------------------------
        IF (TOPWT .GT. 0.0 .AND. TOPWT .GT. PL(K)/10.) THEN
C-----------------------------------------------------------------------                
C              Determine the fraction of the top weight that will be
C              removed by mowing. Include an IF statement to protect
C              against the condition that PL=0 (fileT with grazing or
C              mowing has a sequence of zero damage - real damage -
C              zero damage), would otherwise result in 100% damage.
C-----------------------------------------------------------------------
C  SJR 9/18/05 Add new variable to "remember" harvest removal for 
C             two age-class senescence scheme - establishes "old" tissue
C                  pool size.
C-----------------------------------------------------------------------
        IF (PL(K) .GT. 0.0) THEN
        DAM = (TOPWT - PL(K)/10.) / TOPWT * 100. * PDCF1(I,J)
        HPDAM = DAM
        ELSE
        DAM = 0.0
        ENDIF
        ELSE
        DAM = 0.0
        ENDIF
C-----------------------------------------------------------------------            
C              Set the top weight to the desired value after mowing.
C              This also corrects the LAI in subroutine VEGDM.
C-----------------------------------------------------------------------
        PLFMD = PLFMD + DAM
        PSTMD = PSTMD + DAM
!               PVSTGD = PVSTGD + DAM
        ENDIF

C-----------------------------------------------------------------------
C  SJR 5/19/04
C      MVS Leaf Number reduction option - used in conjunction with MOW
C-----------------------------------------------------------------------
        IF (INDEX( PCPID(I,J),'NOLF') .GT. 0.0) THEN
C-----------------------------------------------------------------------
C  SJR 5/19/04 PL(K) input as number of leaves left after mowing
C-----------------------------------------------------------------------
        IF (VSTAGE .GT. 0.0 .AND. VSTAGE .GT. PL(K)) THEN
C-----------------------------------------------------------------------                
C              Determine the fraction of leaves that will be
C              removed by mowing. Include an IF statement to protect
C              against the condition that PL=0 (fileT with grazing or
C              mowing has a sequence of zero damage - real damage -
C              zero damage), would otherwise result in 100% damage.
C-----------------------------------------------------------------------
        IF (PL(K) .GT. 0.0) THEN
        DAM = (VSTAGE - PL(K)) / VSTAGE * 100. * PDCF1(I,J)
        ELSE
        DAM = 0.0
        ENDIF
        ELSE
        DAM = 0.0
        ENDIF
C-----------------------------------------------------------------------            
C              Set the VStage to the desired value after mowing.
C-----------------------------------------------------------------------
!               PLFMD = PLFMD + DAM
!               PSTMD = PSTMD + DAM
        PVSTGD = PVSTGD + DAM
        ENDIF



C***********************************************************************
        ELSEIF (PCTID(I) .EQ. 4) THEN
C-----------------------------------------------------------------------
C  Compute preference and competition
C  If there is not a coupling point following this one, then
C  there is not a subsequent preference.  Thus, do not apply
C  the competition algorithm.  Apply remaining damage to the
C  current coupling point.
C-----------------------------------------------------------------------
        DAM = 0.
        PDAM = 0.
        DDAM = 0.
        IF(PL(K).LT.0.0001) GOTO 7000
C-----------------------------------------------------------------------
C     Compute food source mass or number available for consumption
C-----------------------------------------------------------------------
        FSM = 0
        IF (INDEX (PCPID(I,J),'LAD') .GT. 0) FSM = WTLF*SLA/10000.
        IF (INDEX (PCPID(I,J),'LMD') .GT. 0) FSM = WTLF
        IF (INDEX (PCPID(I,J),'SMD') .GT. 0) FSM = STMWT
        IF (INDEX (PCPID(I,J),'RMD') .GT. 0) FSM = RTWT
        IF (INDEX (PCPID(I,J),'SDNS') .GT. 0) FSM = TSDNOS
        IF (INDEX (PCPID(I,J),'SDNL') .GT. 0) FSM = TSDNOL
        IF (INDEX (PCPID(I,J),'SDNM') .GT. 0) FSM = TSDNOM
        IF (INDEX (PCPID(I,J),'SDMS') .GT. 0) FSM = TSDWTS
        IF (INDEX (PCPID(I,J),'SDML') .GT. 0) FSM = TSDWTL
        IF (INDEX (PCPID(I,J),'SDMM') .GT. 0) FSM = TSDWTM
        IF (INDEX (PCPID(I,J),'SHNS') .GT. 0) FSM = TSHNOS
        IF (INDEX (PCPID(I,J),'SHNL') .GT. 0) FSM = TSHNOL
        IF (INDEX (PCPID(I,J),'SHNM') .GT. 0) FSM = TSHNOM
        IF (INDEX (PCPID(I,J),'SHMS') .GT. 0) FSM = TSHWTS
        IF (INDEX (PCPID(I,J),'SHML') .GT. 0) FSM = TSHWTL
        IF (INDEX (PCPID(I,J),'SHMM') .GT. 0) FSM = TSHWTM
        IF (INDEX (PCPID(I,J),'VSTG') .GT. 0) FSM = VSTAGE

C-----------------------------------------------------------------------
C     For first coupling point, potential damage (DDAM) equals entire
C     pest level times the feeding rate.  For subsequent coupling
C     points, potential damage equals remaining damage available.
C-----------------------------------------------------------------------
        IF (J .EQ. 1) THEN
        DDAM = PL(K) * PDCF1(I,J)
        ELSE
        DDAM = REMDAM
        ENDIF
C-----------------------------------------------------------------------
C    Compute actual damage to be applied (DAM)
C-----------------------------------------------------------------------
        IF (FSM .GT. 0.0001) THEN
        PDAM = EXP(-DDAM/FSM)
        DAM = FSM * (1. - PDAM)
        ENDIF
C-----------------------------------------------------------------------
C   REMDAM = Remaining potential damage for next coupling point
C-----------------------------------------------------------------------
        IF (PCPID(I,J+1) .NE. 'xxxxx') THEN
        REMDAM = (DDAM-DAM)*PDCF1(I,J+1)/PDCF1(I,J)
        ELSE
        REMDAM = 0
        ENDIF
!***********************************************************************
        ENDIF     !End of PCTID "IF" construct
!***********************************************************************

!***********************************************************************
        IF (PCTID(I) .EQ. 1 .OR. PCTID(I) .EQ. 4) THEN
C-----------------------------------------------------------------------
C     Apply damage in absolute units with or without pest preference
C     and competition effects.
C-----------------------------------------------------------------------
        IF (INDEX (PCPID(I,J),'LMD')  .GT. 0) TLFMD = TLFMD + DAM
        IF (INDEX (PCPID(I,J),'LAD')  .GT. 0) TLFAD = TLFAD + DAM
        IF (INDEX (PCPID(I,J),'PDLA') .GT. 0) TDLA = TDLA + DAM
        IF (INDEX (PCPID(I,J),'SMD')  .GT. 0) WSTMD = WSTMD + DAM
        IF (INDEX (PCPID(I,J),'RMD')  .GT. 0) WRTMD = WRTMD + DAM
        IF (INDEX (PCPID(I,J),'RLV')  .GT. 0) TRTLV = TRTLV + DAM
        IF (INDEX (PCPID(I,J),'RLF')  .GT. 0) TRTLF = TRTLF + DAM
        IF (INDEX (PCPID(I,J),'SDNS') .GT. 0) NSDDS = NSDDS + DAM
        IF (INDEX (PCPID(I,J),'SDNM') .GT. 0) NSDDM = NSDDM + DAM
        IF (INDEX (PCPID(I,J),'SDNL') .GT. 0) NSDDL = NSDDL + DAM

        IF (INDEX (PCPID(I,J),'SDM')  .GT. 0) WSDD  = WSDD  + DAM

        IF (INDEX (PCPID(I,J),'SDMS') .GT. 0) WSDDS = WSDDS + DAM
        IF (INDEX (PCPID(I,J),'SDMM') .GT. 0) WSDDM = WSDDM + DAM
        IF (INDEX (PCPID(I,J),'SDML') .GT. 0) WSDDL = WSDDL + DAM
        IF (INDEX (PCPID(I,J),'SHNS') .GT. 0) NSHDS = NSHDS + DAM
        IF (INDEX (PCPID(I,J),'SHNM') .GT. 0) NSHDM = NSHDM + DAM
        IF (INDEX (PCPID(I,J),'SHNL') .GT. 0) NSHDL = NSHDL + DAM
        IF (INDEX (PCPID(I,J),'SHMS') .GT. 0) WSHDS = WSHDS + DAM
        IF (INDEX (PCPID(I,J),'SHMM') .GT. 0) WSHDM = WSHDM + DAM
        IF (INDEX (PCPID(I,J),'SHML') .GT. 0) WSHDL = WSHDL + DAM
!        Note:  LAIW is calculated here, but not used anywhere.
        IF (INDEX (PCPID(I,J),'WLA')  .GT. 0) LAIW  = LAIW + DAM

        IF (INDEX (PCPID(I,J),'VSTG') .GT. 0) VSTGD = VSTGD + DAM
        IF (INDEX (PCPID(I,J),'ASM')  .GT. 0) TPSR=TPSR + DAM
        IF (INDEX (PCPID(I,J),'WPD')  .GT. 0) THEN
        NPLTD = DAM
        IF (PLTPOP .GT. 0.0) THEN
        PPLTD = PPLTD + 100.0 * NPLTD/PLTPOP
        ENDIF
        PPLTD = MIN(PPLTD,100.0)
        CPPLTD = CPPLTD + PPLTD
        PLFMD = PLFMD + PPLTD
        PSTMD = PSTMD + PPLTD
        PRTMD = PRTMD + PPLTD

C                PSDD = PSDD + PPLTD

        PSDDS = PSDDS + PPLTD
        PSDDL = PSDDL + PPLTD
        PSDDM = PSDDM + PPLTD
        PSHDS = PSHDS + PPLTD
        PSHDL = PSHDL + PPLTD
        PSHDM = PSHDM + PPLTD
        ENDIF
        IF (INDEX (PCPID(I,J),'WPDM')  .GT. 0) THEN
        NPLTD = DAM
        IF (PLTPOP .GT. 0.0) THEN
        PPLTD = PPLTD + 100.0 * NPLTD/PLTPOP
        ENDIF
        PPLTD = MIN(PPLTD,100.0)
        CPPLTD = CPPLTD + PPLTD
C                PLFMD = PLFMD + PPLTD
C                PSTMD = PSTMD + PPLTD
C                PRTMD = PRTMD + PPLTD
C                PSDD = PSDD + PPLTD

        ENDIF


        ENDIF     !End of PCTID = 1 or 4 if-construct
!-----------------------------------------------------------------------
 7000     ENDDO       !End of Coupling Points Loop
        ENDIF         !End of bypass for invalid pest damage method
 8000 ENDDO           !End of Pest loop

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF

!***********************************************************************
      RETURN
      END   ! SUBROUTINE FOR_PESTCP

!-----------------------------------------------------------------------
!     Variable Definitions
!-----------------------------------------------------------------------
! CPPLTD     Cumulative percent of plants destroyed (%)
! DAM        Daily absolute damage 
! DDAM       Potential pest damage for damage types with competition 
! DYNAMIC    Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
!              INTEGR, OUTPUT, or SEASEND 
! FSM        Food source mass or number  
! HPDAM         Harvest removal (proportion of TOPWT)
! LAIW       Weed leaf area (m2/m2)
! NPLTD      Number of plants destroyed (#/m2/d)
! NSDDL      Daily number of large seeds damaged (#/m2/d)
! NSDDM      Daily number of mature seeds damaged (#/m2/d)
! NSDDS      Daily number of small seeds damaged (#/m2/d)
! NSHDL      Daily number of large shells damaged (#/m2/d)
! NSHDM      Daily number of mature shells damaged (#/m2/d)
! NSHDS      Daily number of small shells damaged (#/m2/d)
! PCLMA      Percent observed leaf mass (WTLF) damage (%)
! PCLMT      Percent of total leaf mass (WTLF + senescence) destroyed (%)
! PCN        Number of pests in FILET 
! PCPID(I,J) Pest coupling points identification code for pest I, coupling 
!              point J 
! PCSTMD     Observed cumulative percentage stem mass damage (%)
! PCSTRD     Observed cumulative percentage storage organ mass damage (%)
! PCTID(I)   Pest damage characterization method for pest I: (1) absolute 
!              daily damage rate, (2) percent observed damage, (3) daily 
!              percent damage rate, (4) absolute daily damage rate w/ 
!              preference and competition. 
! PDAM       Pest damage which may be applied to subsequent coupling points
! PDCF1(I,J) Pest damage coefficient associated with pest I, coupling point 
!              J 
! PDLA       Percent diseased leaf area (%)
! PL(I)      Pest level from pest progress file (FILET) for pest I (varies)
! PLAIW      Percent weed leaf area = LAIW/(LAIW+LAI crop).  Calculated in 
!              FOR_PESTCP, but not used anywhere. (%)
! PLFAD      Daily percent leaf area damage (%/d)
! PLFMD      Percent leaf mass damage (%/d)
! PLTPOP     Plant population (# plants / m2)
! PNO(I)     Pest number from File P for pest I 
! PPLTD      Percent plants destroyed  (%/m2/d)
! PPSR       Assimilate damage in daily percent photosynthesis reduction
!              (%/d)
! PRLV       Percent of root length volume destroyed
! PRTLF      Percent of root flux destroyed (%/d)
! PRTLV      Daily percent reduction in root length volume  (%/d)
! PRTMD      Daily percent root mass damage (%/d)
! PSDD       Percent of seed mass destroyed
! PSDDL      Percent large seed mass damage (%/d)
! PSDDM      Percent mature seed mass damage (%/d)
! PSDDS      Percent small seed mass damage (%/d)
! PSHDL      Percent large shell mass damage (%/d)
! PSHDM      Percent mature shell mass damage (%/d)
! PSHDS      Percent small shell mass damage (%/d)
! PSTMD      Daily percent stem mass damage (%)
! PSTRD      Daily percent storage organ mass damage (%)
! PVSTGD     Percent V-stage damage (%)
! REMDAM     Remainder of damage for multiple pest coupling points 
! RTWT       Dry mass of root tissue, including C and N
!              (g[root] / m2[ground])
! SLA        Specific leaf area (cm2[leaf] / m2[ground])
! STMWT      Dry mass of stem tissue, including C and N
!              (g[stem] / m2[ground)
! TDLA       Total diseased leaf area (cm2/m2)
! TLFAD      Total leaf area damage (cm2/cm2/d)
! TLFMD      Total leaf mass damage (g/m2/day)
! TOPWT      Total above ground biomass (g/m2)
! TPSR       Daily absolute assimilate damage (g[CH2O]/m2/d)
! TRTLF      Total root flux destroyed (cm/cm2/d)
! TRTLV      Daily absolute reduction in root length volume  (cm/cm3/d)
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
! WRTMD      Daily absolute root mass reduction  (g/m2/day)
! WSDD       Daily mass of seeds damaged (g/m2/d)
! WSDDL      Daily mass of large seed damaged (g/m2/day)
! WSDDM      Daily mass of mature seed damaged (g/m2/day)
! WSDDS      Daily mass of small seed damaged (g/m2/day)
! WSHDL      Daily mass of large shell damaged (g/m2/day)
! WSHDM      Daily mass of mature shell damaged (g/m2/day)
! WSHDS      Daily mass of small shell damaged (g/m2/day)
! WSTMD      Daily absolute stem damage (g/m2/day)
! WSTRD      Daily absolute storage mass damage (g/m2/day)
! WTLF       Dry mass of leaf tissue including C and N
!              (g[leaf] / m2[ground])
!-----------------------------------------------------------------------
!     End Subroutine FOR_PESTCP
!-----------------------------------------------------------------------
