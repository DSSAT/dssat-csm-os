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
!  6. April-May 2015 - KJB major revisions, to set PANWT and GPP as
!     50:50 function of cumulative biomass and CGR during ISTAGE4
!     Also, to realistically simulate single seed growth (size) over time
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
     & CSD2, CSD1, CUMDEP, CUMDTT, CUMPH, DLAYR, NCOUNT,
     & DTT, EMAT, P5, G4, G5, XRGSET, GPP, GRAINN, GRNWT,
     & GROLF, GRORT, GROSTM, ICSDUR, IDUR1, ISM,
     & ISTAGE, ISWWAT,ISWNIT,LAI, LEAFNO, LFWT, MGROLF, 
     & MGROPAN,MGROSTM, MLFWT, MPANWT, MPLAG, MPLA, MSTMWT, NLAYR,
     & NSTRES, P3, P4, P9, PAF, PANWT, PGC, PHINT, PLA, PLAN, PLAO,
     & PLATO, PLAY, PLAMX, PLTPOP, PTF, PWA, RANC, RESERVE,
     & RLV, ROOTN, RTDEP, RTWT, RWU, SDEPTH, SEEDRV, SENLA, 
     & SIND, SLAN, STMWT, STOVWT,    
     & TANC, TCARBO, TGROLF, TGROPAN,
     & TGROSTM, TILN, TILSW, TLAG1, TLFWT, TLNO, TMNC, TPANWT,
     & TPLA, TPLAG, TPSM, TSIZE, TSTMWT, SUMDTT, VANC,  
     & VMNC, XNTI)

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
      REAL BIOMS3
      REAL BIOMS4
      REAL EFFBIO
      REAL CGRP4
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
      REAL P5
      REAL G4
      REAL G5
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
      INTEGER   NCOUNT
      REAL LAI
      INTEGER LEAFNO
      REAL LFWT
      REAL MGROLF
      REAL MGROPAN
      REAL MGROSTM
!     REAL MLAG1
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
!     REAL WSTR1
      REAL XNTI

!------------------------------------------------------------------------
!  LOCAL VARIABLES NOT IN OLD COMMON BLOCKS
!------------------------------------------------------------------------
      CHARACTER ISWWAT*1,ISWNIT*1
      REAL      GROPAN,PANDW
      REAL      XRGSET
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
          BIOMS3 = 0.1
          BIOMS4 = 0.1
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
          BIOMS3 = BIOMAS/PLTPOP
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
          BIOMS4 = BIOMAS/PLTPOP
          CGRP4 = (BIOMS4-BIOMS3)/NCOUNT
C  KJB
C  Previous code tried, but failed to compute WSTR1 in PHASEI, always csd1=0 and icsdur=1
C  WSTR1 is now passed to it, and it works.
!          IF (ISWWAT .EQ. 'N') THEN
!              WSTR1 = 0.0
!          ELSE
!             WSTR1 = CSD1/ICSDUR
!          ENDIF
          !
          ! G4 IS GRAIN PART. COEFF DIFF FOR HYV(1.0) & VARS(0.5).
! KJB 
!     Using 50:50 ratio of STMWT+LEWT and CGRP4 to reduce effect of
!     long daylength to set too much panwt and GPP.  Used on both
!     In one case example, the BIOMS2/IDUR was 0.391 of the CGRP4
!     The BIOMS2/IDUR was 1.27 and the PGC later was 1.242 from .15 *.15 * (LFWT+STMWT)
!         EFFBIO = CGRP4 * 0.391 / (0.15 * 0.15)
         EFFBIO = CGRP4 * 0.410/ (0.15 * 0.15)
!  KJB - XRGSET IS Tave effect on grain # and partitioning
!        computed during end leaf to end panicle (ISTAGE 4)
!          PANWT = (STMWT + LFWT) * 0.15 * (1.0 - 0.5 * WSTR1) * G4 
!last          PANWT = XRGSET * (STMWT + LFWT) * 0.15 * (1.0-0.1*WSTR1) * G4
!          PANWT = XRGSET * (0.5*(STMWT + LFWT) + 0.5 * EFFBIO) 
!     &          * 0.15 * (1.0-0.05*WSTR1) * G4
!KJB, with the EFFBIO (from CGR during ISTAGE4, and cumul Biomass) we do not
!  need to double count water stress during ISTAGE4.  It already reduces CARBO in EFFBIO
!  in addition, that WSTR1 is not in GPP equation.
          PANWT = XRGSET * (0.50 * (STMWT + LFWT) + 0.50 * EFFBIO) 
     &          * 0.15 * G4
!         CHP 12/14/2006 Limit PANWT <= STMWT
          PANWT = AMIN1(PANWT, STMWT)

          PWA   = PANWT
          STMWT = STMWT - PANWT
          PGC   = PANWT * 0.15
          RESERVE = PGC
          ISM   = 0

!   BIOMAS2/IDURI is CGR. 31.3 IS ALMOST DAYS OF GRAIN FILL, 1000 IS mg/g
!   Divide by G5 (size in mg) causes fewer grain if large grain
!   Multiply by G4, adds number proportional to PANWT above, but
!   with a scalar because higher G4 partitioning, runs into assim supply
!   0.8 is sort of a scalar, initially reference G4.  Is it needed?
!   P5/300 is to increase seed number (lower ISGR) for long P5, to hold 
!   seed size as definable with long or short P5. KJBOOTE, 3-7-2015
!   XRGSET is Tave effect on grain # and partitioning, computed during ISTAGE 4
          
!          GPP   = (BIOMS2/IDUR1)*1000*31.3/G5*(G4**0.75)/0.8
!     &             *340/340
!          GPP   = (BIOMS2/IDUR1)*1000./G5*31.3*(G4+0.1*(1.-G4))/0.8
!     &             *P5/300.
!last          GPP   = XRGSET * (BIOMS2/IDUR1)*1000./G5*31.3
!last     &             *(G4+0.1*(1.-G4))/0.8 * P5/300.
!         GPP  = XRGSET*(0.50*(BIOMS2/IDUR1)+0.50*(LFWT+STMWT)*0.15*0.15)
      GPP  = XRGSET*(0.50 *(CGRP4*0.410)+0.50*(LFWT+STMWT)*0.15*0.15)
     &    *1000./G5*31.3*(G4+0.1*(1.-G4))/0.8 * P5/300.           
!  original        GPP   = 3130.0 * (BIOMS2/IDUR1)
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
! KJB  MLAG1 AND TLAG1 INITIALIZED AT 0.0 IN GROSUB AND TILLSUB, WHY HERE?
!          MLAG1  =  0.5
!          TLAG1  =  0.5
          TLAG1  =  0.0
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
