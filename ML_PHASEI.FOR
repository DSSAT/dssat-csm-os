C======================================================================
!  ML_PHASEI, Subroutine
!
!  Determines phase initialization
!-----------------------------------------------------------------------
!  Revision history
!
!  1. Written
!  2. Header revision and minor changes           P.W.W.      2-7-93
!  3. Added switch common block, restructured     P.W.W.      2-7-93
!  4. Removed Y1 and Y2 variables                 B.D.B.      6-21-1994
!  5. Converted to modular format                 W.D.B.      7-31-02
!-----------------------------------------------------------------------
!  INPUT  : ISWWAT,ISWNIT
!
!  LOCAL  : PP3,AG2,GROPAN,PANDW,L,L1
!
!  OUTPUT :
!-----------------------------------------------------------------------
!  Called : ML_PHENOL
!
!  Calls  : None
!-----------------------------------------------------------------------
!                         DEFINITIONS
!
!  AG2    :
!  GROPAN : Daily pannicle growth - g
!  PANDW  :
!  PP3    :
!=======================================================================

      SUBROUTINE ML_PHASEI (
     & AGEFAC, BIOMAS, BIOMS1, BIOMS2, CNSD1, CNSD2, 
     & CSD2, CSD1, CUMDEP, CUMDTT, CUMPH, DLAYR,
     & DTT, EMAT, G4, GPP, GRAINN, GRNWT,
     & GROLF, GRORT, GROSTM, ICSDUR, IDUR1, ISM,
     & ISTAGE, ISWWAT,ISWNIT,LAI, LEAFNO, LFWT, MGROLF, 
     & MGROPAN,MGROSTM,MLAG1, MLFWT, MPANWT, MPLAG, MPLA, MSTMWT, NLAYR,
     & NSTRES, P3, P4, P9, PAF, PANWT, PGC, PHINT, PLA, PLAN, PLAO,
     & PLATO, PLAY, PLAMX, PLTPOP, PTF, PWA, RANC, RESERVE,
     & RLV, ROOTN, RTDEP, RTWT, RWU, SDEPTH, SEEDRV, SENLA, 
     & SIND, SLAN, STMWT, STOVWT,    
     & TANC, TCARBO, TGROLF, TGROPAN,
     & TGROSTM, TILN, TILSW, TLAG1, TLFWT, TLNO, TMNC, TPANWT,
     & TPLA, TPLAG, TPSM, TSIZE, TSTMWT, SUMDTT, VANC,  
     & VMNC, WSTR1, XNTI)

      USE MODULEDEFS
      IMPLICIT  NONE
      SAVE

!-------------------------------------------------------
! VARIABLES IN COMMON BLOCKS
!-------------------------------------------------------
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
      REAL DLAYR(NL)
      REAL DTT
      REAL EMAT
      REAL G4
      REAL GPP
      REAL GRAINN
      REAL GRNWT
      REAL GROLF
      REAL GRORT
      REAL GROSTM
      INTEGER ICSDUR
      INTEGER IDUR1
      INTEGER ISM
      INTEGER ISTAGE
      REAL LAI
      INTEGER LEAFNO
      REAL LFWT
      REAL MGROLF
      REAL MGROPAN
      REAL MGROSTM
      REAL MLAG1
      REAL MLFWT
      REAL MPANWT
      REAL MPLAG
      REAL MPLA
      REAL MSTMWT
      INTEGER NLAYR
      REAL NSTRES
      REAL P3
      REAL P4
      REAL P9
      REAL PAF
      REAL PANWT
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
      REAL PWA
      REAL RANC
      REAL RESERVE
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
      REAL STOVWT
      REAL SWMIN
      REAL TANC
      REAL TCARBO
      REAL TGROLF
      REAL TGROPAN
      REAL TGROSTM
      REAL TILN
      REAL TILSW
      REAL TLAG1
      REAL TLFWT
      REAL TLNO
      REAL TMNC
      REAL TPANWT
      REAL TPLA
      REAL TPLAG
      REAL TPSM
      REAL TSIZE
      REAL TSTMWT
      REAL SUMDTT
      REAL VANC
      REAL VMNC
      REAL WSTR1
      REAL XNTI
!------------------------------------------------------------------------
!  LOCAL VARIABLES NOT IN OLD COMMON BLOCKS
!------------------------------------------------------------------------
      CHARACTER ISWWAT*1,ISWNIT*1
      REAL      GROPAN,PANDW
      INTEGER   L,L1

!--------------------------------------------------------------------
!              Main Code
!--------------------------------------------------------------------

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
      ! 3 - End leaf growth
      ! 4 - End pannicle growth
      ! 5 - Grain fill
      ! 6 - Maturity
      !

      !--------------------------------------------------------------
      !   ISTAGE = 1: End of Juvenile period
      !--------------------------------------------------------------
      IF (ISTAGE .EQ. 1) THEN
          ISTAGE = 2
          SIND   = 0.0
          BIOMS1 = 0.1
          BIOMS2 = 0.1
          RETURN
      !--------------------------------------------------------------
      !  ISTAGE = 2: Panicle Initiation
      !--------------------------------------------------------------
       ELSEIF (ISTAGE .EQ. 2) THEN
          ISTAGE = 3
          TLNO   = (CUMDTT/35.0+6.0)
          BIOMS1 = BIOMAS/PLTPOP
          XNTI   = SUMDTT/43.0
          P3     = 370.0 + 0.135*SUMDTT
          P4     = 150.0
          SUMDTT = DTT
          PLAMX  = PLA
          RETURN

      !--------------------------------------------------------------
      !  ISTAGE = 3: End of Leaf Growth
      !--------------------------------------------------------------

       ELSEIF (ISTAGE .EQ. 3) THEN
          ISTAGE = 4
          PGC    = 0.0
          PAF    = 0.1
          MPLAG  = 0.0
          !
          ! SWMIN is set equal to STMWT in version 2.1 at ISTAGE 4
          !
          SWMIN = STMWT
          TSIZE  = EXP(-0.15*(PLTPOP-2.0))
          PTF    = 1.0
          SUMDTT = SUMDTT - P3
          RETURN
      !--------------------------------------------------------------
      !  ISTAGE = 4: End of Panicle Growth
      !--------------------------------------------------------------
       ELSEIF (ISTAGE .EQ. 4) THEN
          ISTAGE = 5
          VANC   = TANC
          VMNC   = TMNC
          TGROLF  = 0.0
          TGROSTM = 0.0
          MGROLF  = 0.0
          MGROSTM = 0.0
          TPLAG   = 0.0
          IF (ISWWAT .EQ. 'N') THEN
              WSTR1 = 0.0
          ELSE
             WSTR1 = CSD1/ICSDUR
          ENDIF
          !
          ! G4 IS GRAIN PART. COEFF DIFF FOR HYV(1.0) & VARS(0.5).
          !
          PANWT = (STMWT + LFWT) * 0.15 * (1.0 - 0.5 * WSTR1) * G4

!         CHP 12/14/2006 Limit PANWT <= STMWT
          PANWT = AMIN1(PANWT, STMWT)

          PWA   = PANWT
          STMWT = STMWT - PANWT
          PGC   = PANWT * 0.15
          RESERVE = PGC
          ISM   = 0
          GPP   = 3130.0 * (BIOMS2/IDUR1)
          SUMDTT = SUMDTT - P4
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
          P9    = 45.0 +  6.0*SDEPTH
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
          MPLA    = 0.5
          TPLA    = 0.5
          MPLAG   = 0.0
          TPLAG   = 0.0
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
          TILSW   = 0.5
          CUMPH  = SUMDTT/PHINT
          IDUR1  = 0
          STOVWT = LFWT
          SEEDRV =  0.006
          MLAG1  =  0.5
          TLAG1  =  0.5
          LAI    = PLTPOP*PLA*0.0001
          BIOMAS = STOVWT
      ENDIF


!--------------------------------------------------------------------
!       This code is run for on the day ISTAGE goes from 9 to 1
!--------------------------------------------------------------------
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
      ENDIF

      RETURN
      END SUBROUTINE ML_PHASEI
