C======================================================================
C  SG_PHASEI, Subroutine
C
C  Determines phase initialization
C-----------------------------------------------------------------------
C  Revision history
C
C  12/31/1996 GH  Deleted phenology statements.
C  02/07/1993 PWW Header revision and minor changes                   
C  02/07/1993 PWW Added switch block, code cleanup                    
C  06/21/1994 WDB Removed Y1 and Y2 variables
C  07/31/2002 WDB Converted to modular format   
C  04/20/2007 GH  Removed PP3, P4 and P5 calculations
C-----------------------------------------------------------------------
C  INPUT  : ISWWAT,ISWNIT
C
C  LOCAL  : PP3,AG2,GROPAN,PANDW,L,L1
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SG_PHENOL
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  AG2    :
C  GROPAN : Daily panicle growth - g
C  PANDW  :
C  PP3    :
C=======================================================================

      SUBROUTINE SG_PHASEI (
     & AGEFAC, BIOMAS, BIOMS1, BIOMS2, CNSD1, CNSD2, 
     & CSD2, CSD1, CUMDEP, CUMDTT, CUMPH, CUMP2, CUMP4, DLAYR,
     & DTT, EMAT, G1, G2, GPP, GRAINN, GRNWT,
     & GROLF, GRORT, GROSTM, ICSDUR, IDUR1, 
     & ISTAGE, ISWWAT,ISWNIT,LAI, LEAFNO, LFWT, LWMIN, MGROLF, 
     & MGROPAN,MGROSTM, MLFWT, MPANWT, MSTMWT, NLAYR,
     & NSTRES, P3, P4, P9, PAF, PANWT, PGC, PHINT, PLA, PLAN, PLAO,
     & PLATO, PLAY, PLAMX, PLTPOP, PTF, RANC, PANTH, PFLOWR,
     & RLV, ROOTN, RTDEP, RTWT, RWU, SDEPTH, SEEDRV, SENLA, 
     & SIND, SLAN, STMWT, STOVN, STOVWT, SUMRTR, SWMAX, 
     & SWMIN, TANC, TCARBO, TCNP, TDUR, TGROLF, TGROPAN,
     & TGROSTM, TILN, TLFWT, TLNO, TMNC, TPANWT,
     & TPSM, TSIZE, TSTMWT, SUMDTT, VANC,  
     & VMNC, XNTI,P2O, P2R, TWILEN)

      USE MODULEDEFS

      IMPLICIT  NONE

      SAVE
C-------------------------------------------------------
C VARIABLES IN COMMON BLOCKS
C-------------------------------------------------------
      REAL AGEFAC
      REAL BIOMAS
      REAL BIOMS1
      REAL BIOMS2
      REAL CNSD1
      REAL CNSD2
      REAL CSD2
      REAL CSD1
      REAL CUMDEP
      REAL CUMDTT
      REAL CUMPH
	REAL CUMP2, CUMP4
      REAL DLAYR(NL)
      REAL DTT
      REAL EMAT
      REAL G1
      REAL G2
      REAL GPP
      REAL GRAINN
      REAL GRNWT
      REAL GROLF
      REAL GRORT
      REAL GROSTM

      INTEGER ICSDUR
      INTEGER IDUR1
      INTEGER ISTAGE

      REAL LAI

      INTEGER LEAFNO

      REAL LFWT
      REAL LWMIN
      REAL MGROLF
      REAL MGROPAN
      REAL MGROSTM
      REAL MLFWT
      REAL MPANWT
      REAL MSTMWT

      INTEGER NLAYR

      REAL NSTRES
	REAL P3, P4, P9, PAF, PANTH, PANWT, PFLOWR
	REAL TWILEN, P2O, P2R
      REAL PGC
      REAL PHINT
      REAL PLA
      REAL PLAN
      REAL PLAO
      REAL PLATO
      REAL PLAY
      REAL PLAMX
      REAL PLTPOP
      REAL PTF
      REAL RANC
      REAL RLV(NL)
      REAL ROOTN
      REAL RTDEP
      REAL RTWT
      REAL RWU(NL)
      REAL SDEPTH
      REAL SEEDRV
      REAL SENLA
      REAL SIND
      REAL SLAN
      REAL STMWT
      REAL STOVN
      REAL STOVWT
      REAL SUMRTR
      REAL SWMAX
      REAL SWMIN
      REAL TANC
      REAL TCARBO
      REAL TCNP
      REAL TDUR
      REAL TGROLF
      REAL TGROPAN
      REAL TGROSTM
      REAL TILN
      REAL TLFWT
      REAL TLNO
      REAL TMNC
      REAL TPANWT
      REAL TPSM
      REAL TSIZE
      REAL TSTMWT
      REAL SUMDTT
      REAL VANC
      REAL VMNC
      REAL XNTI
C------------------------------------------------------------------------
C  LOCAL VARIABLES NOT IN OLD COMMON BLOCKS
C------------------------------------------------------------------------
      CHARACTER ISWWAT*1,ISWNIT*1
      REAL      AG2,GROPAN,PANDW
C-GH REAL       PP3,AG2,GROPAN,PANDW
      INTEGER   L,L1

C--------------------------------------------------------------------
C              Main Code
C--------------------------------------------------------------------
      CNSD1  = 0.0
      CNSD2  = 0.0
      CSD1   = 0.0
      CSD2   = 0.0
      ICSDUR = 1

      !--------------------------------------------------------------
      !     Definition of Stages
      !--------------------------------------------------------------
      ! 7 - Sowing date
      ! 8 - Germination
      ! 9 - Emergence
      ! 1 - End juvenile
      ! 2 - Pannicle initiation
      ! 3 - End of flag leaf expansion
      ! 4 - Beginning grain fill
      ! 5 - Physiological maturity
      ! 6 - Harvest
      !

      !--------------------------------------------------------------
      !   ISTAGE = 1: End of Juvenile period
      !--------------------------------------------------------------
      IF (ISTAGE .EQ. 1) THEN
          ISTAGE = 2
          SIND   = 0.0
          BIOMS1 = 0.1
          BIOMS2 = 0.1
	    CUMP2  = 0.0

         RETURN
      !--------------------------------------------------------------
      !  ISTAGE = 2: Panicle Initiation
      !--------------------------------------------------------------
       ELSEIF (ISTAGE .EQ. 2) THEN
          ISTAGE = 3
          TLNO   = (CUMDTT/35.0+6.0)
          BIOMS1 = BIOMAS/PLTPOP
          XNTI   = SUMDTT/43.0
C-GH      PP3   = 275.0     + 0.19*SUMDTT
C-GH      P3    = 6.5*PHINT + 0.25*SUMDTT
C-GH      P4    = 4.0*PHINT + 38.0
          IDUR1 = 0
          PLAMX  = PLA
          TDUR   = 0.0
          SUMRTR = 0.0
          SUMDTT = 0.0
          RETURN

      !--------------------------------------------------------------
      !  ISTAGE = 3: End of flag leaf expansion
      !--------------------------------------------------------------

       ELSEIF (ISTAGE .EQ. 3) THEN
          ISTAGE = 4
          PGC    = 0.0
          PAF    = 0.1
          TSIZE  = EXP(-0.15*(PLTPOP-2.0))
          PTF    = 1.0
	    SUMDTT = SUMDTT - (PANTH - P3)
	    IF (TWILEN .GT. P2O) THEN
	      PFLOWR = P3 +  P2R * (TWILEN - P2O)
          ELSE
            PFLOWR = P3
          ENDIF
	    CUMP4 = 0.0
	    RETURN
      !--------------------------------------------------------------
      !  ISTAGE = 4: Beginning grain filling
      !--------------------------------------------------------------
       ELSEIF (ISTAGE .EQ. 4) THEN
          ISTAGE = 5
          VANC   = TANC
          VMNC   = TMNC
          AG2    = G2     * 0.05
          PANWT  = STMWT  * AG2

          !Added by CHP 08/15/2003
          STMWT  = STMWT - PANWT

          GRAINN = PANWT  * TCNP
          STOVN  = STOVN  - GRAINN
          STOVWT = STOVWT - PANWT
          SWMIN = STMWT*0.80
          LWMIN = LFWT *0.95
          SWMAX = STMWT
          PGC   = PANWT*AG2
          GPP   = 1340.0*((BIOMS2-BIOMS1)/IDUR1)
	    SUMDTT = SUMDTT - (PFLOWR + P4)
	    EMAT   = 0.0
          RETURN

      !--------------------------------------------------------------
      !  ISTAGE = 5: End of Grain Filling
      !--------------------------------------------------------------
       ELSEIF (ISTAGE .EQ. 5) THEN
          ISTAGE = 6
          RETURN
      !--------------------------------------------------------------
      !  ISTAGE = 6: Physiological Maturity
      !--------------------------------------------------------------
       ELSEIF (ISTAGE .EQ. 6) THEN
          ISTAGE = 7
          CUMDTT = 0.0
          DTT    = 0.0
          RETURN

      !--------------------------------------------------------------
      !  ISTAGE = 7: Sowing Date
      !--------------------------------------------------------------
       ELSEIF (ISTAGE .EQ. 7) THEN
          ISTAGE = 8
          RTDEP  = SDEPTH
          SUMDTT = 0.0
          RETURN

      !--------------------------------------------------------------
      !  ISTAGE = 8: Germination Date
      !--------------------------------------------------------------
       ELSEIF (ISTAGE .EQ. 8) THEN
          ISTAGE =    9
          CUMDTT =  0.0
          NSTRES =  1.0
          AGEFAC =  1.0
          SUMDTT =  0.0
          RETURN

      !--------------------------------------------------------------
      !  ISTAGE = 9: Seedling Emergence
      !--------------------------------------------------------------
       ELSEIF (ISTAGE .EQ. 9) THEN
          ISTAGE = 1
          SUMDTT = SUMDTT - P9
          GROSTM = 0.0
          GRNWT  = 0.0
          SENLA  = 0.0
          GRORT  = 0.0
          TILN   = 1.0
          GROPAN = 0.0
          GROLF  = 0.0
          SLAN   =  0.0
          TLNO   = 30.0
          LEAFNO =  1
          PLAY   =  1.0
          CUMDTT  = CUMDTT - P9
          TPSM    = 1.0
          PLAN    = 0.0
          PLATO   = 0.0
          PLAO    = 0.5
          PLA     = 0.5
          LFWT    = 0.01
          MLFWT   = 0.01
          TLFWT   = 0.005
          RTWT    = 0.006
          STMWT   = 0.006
          MSTMWT  = 0.006
          TSTMWT  = 0.001
          PANDW   = 0.0
          PANWT   = 0.0
          MPANWT  = 0.0
          TPANWT  = 0.0
          MGROSTM = 0.0
          GROSTM  = 0.0
          TGROSTM = 0.0
          TGROLF  = 0.0
          MGROLF  = 0.0
          GROLF   = 0.0
          MGROPAN = 0.0
          GROPAN  = 0.0
          TGROPAN = 0.0
          TCARBO  = 0.0
          TDUR    = 0.0
          SUMRTR  = 0.0
          CUMPH  = SUMDTT/PHINT
          STOVWT = 0.016
          PLAY   = 0.130 + 0.003*G1
          SEEDRV = 0.020
          LAI    = PLTPOP*PLA*0.0001
          BIOMAS = STOVWT
      ENDIF


C--------------------------------------------------------------------
C       This code is run for on the day ISTAGE goes from 9 to 1
C--------------------------------------------------------------------
      CUMDEP = 0.0
      IF (ISWWAT .EQ. 'N') RETURN

      DO L = 1, NLAYR
         CUMDEP = CUMDEP + DLAYR(L)
         RLV(L) = 0.20*PLTPOP/DLAYR (L)
         IF (CUMDEP .GT. RTDEP) GO TO 100            ! Was EXIT
      END DO

  100 CONTINUE                                       ! Sun Fix

      RLV(L) = RLV(L)*(1.0-(CUMDEP-RTDEP)/DLAYR(L))
      L1 = L + 1
      IF (L1 .LT. NLAYR) THEN
         DO L = L1, NLAYR
            RLV(L) = 0.0
         END DO
      ENDIF

      DO L = 1, NLAYR
         RWU(L) = 0.0
      END DO

      IF (ISWNIT .NE. 'N') THEN
         RANC   = 0.025
         TANC   = 0.050
         GRAINN = 0.000
         ROOTN = RANC   * RTWT
         STOVN = STOVWT * TANC
      ENDIF

      RETURN
      END SUBROUTINE SG_PHASEI
