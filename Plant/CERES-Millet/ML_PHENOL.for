C=======================================================================
C  ML_PHENOL, Subroutine
C
C  Determines phenological stage
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2. Header revision and minor changes           P.W.W.      2-7-93
C  3. Added switch block, code cleanup            P.W.W.      2-7-93
C  4. Modified TT calculations to reduce line #'s P.W.W.      2-7-93
C  5. Modified for MILLET model                   W.T.B.      MAY 94
C  6. Converted to modular format                 W.D.B.      7-31-02
C  7. Major revisions to millet model             K.J.B April-May 2015
!  06/15/2022 CHP Added CropStatus
C-----------------------------------------------------------------------
C  INPUT  : YRDOY
C
C  LOCAL  : I,NDAS,L,L0,XANC,TEMPCN,TEMPCX,XS,TDIF,TCOR,
C           TTMP,YDL,DEC,DLV,CHGDL,SWSD,PDTT,RATEIN,PFLOWR,PSKER,
C           YIELDB,TWILEN,BARFAC,ABSTRES
C
C  OUTPUT : STGDOY
C-----------------------------------------------------------------------
C  Called : ML_CERES
C
C  Calls  : ML_PHASEI ML_COLD
C-----------------------------------------------------------------------
C
C                         DEFINITIONS
C
C  STGDOY :
C  YRDOY  :
C  NOUTDO :
C=======================================================================

      SUBROUTINE ML_PHENOL (APTNUP,BIOMAS,BIOMS2,
     & CSD1, 
     & CNSD1, CNSD2, CTYPE, CUMDEP, DLAYR, DTT, SETTC, SETTYP,  
     & EMAT, GNUP, GPP, GPSM, GRAINN, GRNWT, IDETO,
     & IDUR1, IPRINT, ISDATE, ISM, ISTAGE, ISWNIT, ISWWAT,
     & LAI, LEAFNO, LL, MAXLAI, MDATE, NOUTDO, NLAYR, 
     & P1, P2O, P2R, P3, P4, P5, P9, PANWT, PGRNWT, PHINT, 
     & PLTPOP, PWA, ROPT, RTDEP, SDEPTH, SI1, SI2, SI3,
     & SI4, SIND, SKERWT, SNOW, SRAD, STGDOY,STMWT, STOVER, STOVN, 
     & SUMDTT, SW, TANC, TBASE, TEMPCR, TMAX, TMIN, 
     & TOPT,TOTNUP, TPSM, YIELD, YRDOY,XGNP, XSTAGE,  
C      Variables passed through PHENOL to phasei but not used in phenol
     & AGEFAC, BIOMS1, CUMDTT, CUMPH, G4, G5, GROLF,
     & GRORT, GROSTM, LFWT, MGROLF, MGROPAN, MGROSTM, 
     & MLFWT, MPANWT, MPLAG, MPLA, MSTMWT, NSTRES, PAF, 
     & PGC, PLA, PLAN, PLAO, PLATO, PLAMX, PTF, RANC, 
     & RESERVE, RLV, ROOTN, RTWT, RWU, SEEDRV, SENLA, SLAN, 
     & STOVWT, TCARBO, TGROLF,
     & TGROPAN, TGROSTM, TILN, TILSW, TLAG1, TLFWT, TLNO, TMNC,
     & TPANWT, TPLA, TPLAG, TSIZE, TSTMWT, VANC, VMNC, WSTR1, 
     & XNTI,SWFAC,TURFAC,DGET,SWCG,DJTI,
     &    DAYL, TWILEN, CANWAA, CANNAA, CropStatus)

      USE MODULEDEFS
      IMPLICIT  NONE
      EXTERNAL DAYLEN, CURV, ML_PHASEI, WARNING
      SAVE
C -----------------------------------------------------------------------
C VARIABLES ONLY USED IN PHASEI. THEY ARE PASSED THROUGH PHENOL TO PHASEI
C------------------------------------------------------------------------
      REAL AGEFAC
      REAL BIOMS1
      INTEGER CropStatus
      REAL CUMDTT
      REAL CUMPH
      REAL DGET
      REAL DJTI
      REAL G4
      REAL G5
      REAL GROLF
      REAL GRORT
      REAL GROSTM
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
      REAL NSTRES
      REAL PAF
      REAL PGC
      REAL PLA
      REAL PLAN
      REAL PLAO
      REAL PLATO
      REAL PLAY
      REAL PLAMX
      REAL PTF
      REAL RANC
      REAL RESERVE
      REAL RLV(NL)
      REAL ROOTN
      REAL RTWT
      REAL RWU(NL)
      REAL SEEDRV
      REAL SENLA
      REAL SLAN
      REAL STOVWT
      REAL SWCG
      REAL SWFAC
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
      REAL TSIZE
      REAL TSTMWT
      REAL TURFAC
      REAL VANC
      REAL VMNC
      REAL WSTR1
      REAL XNTI
      REAL RGSET
      REAL CRGSET
      REAL CURV
      REAL SETTC(4)
      CHARACTER*3  SETTYP
      REAL      TEMPST
      REAL      XRGSET
C------------------------------------------------------------------------
C   VARIABLES IN OLD COMMON BLOCKS
C------------------------------------------------------------------------

      REAL APTNUP
      REAL BIOMAS
      REAL BIOMS2
      REAL CANNAA
      REAL CANWAA
      REAL CSD1
      REAL CSD2
      REAL CNSD1
      REAL CNSD2
      INTEGER CTYPE
      REAL CUMDEP
      REAL DLAYR(NL)
      REAL DTT
      REAL EMAT
      REAL GNUP
      REAL GPP
      REAL GPSM
      REAL GRAINN
      REAL GRNWT
      INTEGER ICSDUR
      CHARACTER*1 IDETO
      INTEGER IDUR1
      INTEGER IPRINT
      INTEGER ISDATE
      INTEGER ISM
      INTEGER ISTAGE
      CHARACTER*1 ISWNIT
      CHARACTER*1 ISWWAT
      REAL LAI
      INTEGER LEAFNO
      REAL LL(NL)
      REAL MAXLAI
      INTEGER MDATE
      INTEGER NOUTDO
      INTEGER NLAYR
      REAL P1
      REAL P2O
      REAL P2R
      REAL P3
      REAL P4
      REAL P5
      REAL P9
      REAL PANWT
      REAL PGRNWT
      REAL PHINT
      REAL PLTPOP
      REAL PWA
      REAL ROPT
      REAL RTDEP
      REAL SDEPTH
      REAL SI1(6)
      REAL SI2(6)
      REAL SI3(6)
      REAL SI4(6)
      REAL SIND
      REAL SKERWT
      REAL SNOW
      REAL SRAD
      REAL STMWT
      REAL STOVER
      REAL STOVN
      REAL SUMDTT
      REAL SW(NL)
      REAL TANC
      REAL TBASE
      REAL TEMPCR
      REAL TMAX
      REAL TMIN
      REAL TOPT
      REAL TOTNUP
      REAL TPSM
      REAL YIELD
      REAL XGNP
      REAL XSTAGE
      

C------------------------------------------------------------------------
C     LOCAL VARIABLES
C------------------------------------------------------------------------
      INTEGER   STGDOY(20),YRDOY,I,NDAS,L,L0
      INTEGER   NCOUNT
      REAL      XANC,TEMPCN,TEMPCX,XS,
     +          SWSD,PDTT,RATEIN,PFLOWR,
     +          YIELDB,TWILEN
      REAL      MAXLAI2,MAXLAI3
      REAL      DAYL,ACOEF,TNSOIL,TDSOIL,TMSOIL
      REAL      TH,DOPT


!     Variables needed to send message to WARNING.OUT file
      CHARACTER*78 MESSAGE(10)

C--------------------------------------------------------------------
C                         MAIN CODE
C--------------------------------------------------------------------
      XANC   = TANC*100.0
      APTNUP = STOVN*10.0*PLTPOP
      TOTNUP = APTNUP      

      !--------------------------------------------------------------
      !Compute thermal time based on new method developed by J.T.R
      !at CYMMIT, 5/5/98.  TBASE, TOPT, and ROPT are read in from 
      ! the species file, as is CTYPE (Cereal type).
      !--------------------------------------------------------------

      !Initially, set TEMPCN and TEMPCS to TMAX and TMIN

      TEMPCN = TMIN
      TEMPCX = TMAX
      XS     = SNOW
      XS     = AMIN1 (XS,15.0)

      !--------------------------------------------------------------
      ! Calculate crown temperature based on temperature and snow cover
      !--------------------------------------------------------------
      IF (TMIN .LT. 0.0) THEN
         TEMPCN = 2.0 + TMIN*(0.4+0.0018*(XS-15.0)**2)
      ENDIF
      IF (TMAX .LT. 0.0) THEN
         TEMPCX = 2.0 + TMAX*(0.4+0.0018*(XS-15.0)**2)
      ENDIF
      TEMPCR = (TEMPCX + TEMPCN)/2.0
      !
      ! DOPT, Devlopment optimum temperature, is set to TOPT during 
      ! vegetative growth and to ROPT after anthesis
      !
      DOPT = TOPT
      IF ((ISTAGE .GT. 3) .AND. (ISTAGE .LE. 6)) THEN
         DOPT = ROPT
      ENDIF
      !
      ! Check basic temperature ranges and calculate DTT for 
      ! develpment based on PC with JTR
      !
      IF (TMAX .LT. TBASE) THEN
         DTT = 0.0
       ELSEIF (TMIN .GT. DOPT) THEN
         !
         ! This statement replaces DTT = TOPT 
         ! ... GoL and LAH, CIMMYT, 1999
         DTT = DOPT - TBASE
         !
         ! Now, modify TEMPCN, TEMPCX based on soil conditions or snow
         ! If wheat and barley is before terminal spiklett stage
         ! Or if corn and sorghum are before 10 leaves
         !
       ELSEIF ((CTYPE .EQ. 1 .AND. LEAFNO .LE. 10) .OR.
     &         (CTYPE .EQ. 2 .AND. ISTAGE .LT. 2)) THEN
         !
         ! Check for snow  (should following be GT.0 or GT.15 ?).  
         ! Based on snow cover, calculate DTT for the day
         !
         IF (XS .GT. 0.0) THEN
            !
            ! Snow on the ground
            !
            DTT    = (TEMPCN + TEMPCX)/2.0 - TBASE
          ELSE
            !
            ! No snow, compute soil temperature
            !
            ACOEF  = 0.01061 * SRAD + 0.5902
            TDSOIL = ACOEF * TMAX + (1.0 - ACOEF) * TMIN
            TNSOIL = 0.36354 * TMAX + 0.63646 * TMIN
            IF (TDSOIL .LT. TBASE) THEN
               DTT = 0.0
             ELSE
               IF (TNSOIL .LT. TBASE) THEN
                  TNSOIL = TBASE
               ENDIF
               IF (TDSOIL .GT. DOPT) THEN
                  TDSOIL = DOPT
               ENDIF
!     chp - import DAYL from Weather module instead
               !CALL DAYLEN (DOY,XLAT,DAYL,DEC,SNDN,SNUP)
               TMSOIL = TDSOIL * (DAYL/24.) + TNSOIL * ((24.-DAYL)/24.)
               IF (TMSOIL .LT. TBASE) THEN
                  DTT = (TBASE+TDSOIL)/2.0 - TBASE
                ELSE
                  DTT = (TNSOIL+TDSOIL)/2.0 - TBASE
               ENDIF
               !
               ! Statement added ... GoL and LAH, CIMMYT, 1999
               !
               DTT = AMIN1 (DTT,DOPT-TBASE)
            ENDIF
         ENDIF
         !
         ! Now, compute DTT for when Tmax or Tmin out of range
         !
       ELSEIF (TMIN .LT. TBASE .OR. TMAX .GT. DOPT) THEN
          DTT = 0.0
          DO I = 1, 24
             TH = (TMAX+TMIN)/2. + (TMAX-TMIN)/2. * SIN(3.14/12.*I)
             IF (TH .LT. TBASE) THEN
                TH = TBASE
             ENDIF
             IF (TH .GT. DOPT) THEN
                TH = DOPT
             ENDIF
             DTT = DTT + (TH-TBASE)/24.0
          END DO
       ELSE
          DTT = (TMAX+TMIN)/2.0 - TBASE
      ENDIF

      SUMDTT  = SUMDTT  + DTT 
      CUMDTT = CUMDTT + DTT
      CSD1   = CSD1 + 1.0 - SWFAC
      CSD2   = CSD2 + 1.0 - TURFAC
      CNSD1 = CNSD1 + 1.0-NSTRES
      CNSD2 = CNSD2 + 1.0-AGEFAC
      ICSDUR = ICSDUR + 1

C     KJB added
      TEMPST = (TMAX + TMIN)*0.5   !Mean air temperature, C


C--------------------------------------------------------------------
C       Definition of Stages
C
C     7 - Sowing date
C     8 - Germination
C     9 - Emergence
C     1 - End juvenile
C     2 - Pannicle initiation
C     3 - End leaf growth
C     4 - End pannicle growth
C     5 - Grain fill
C     6 - Maturity
C--------------------------------------------------------------------

C--------------------------------------------------------------------
C         ISTAGE = 7: Determine when sowing date occurs
C--------------------------------------------------------------------
      IF (ISTAGE .EQ. 7) THEN
          STGDOY(ISTAGE) = YRDOY
          NDAS           = 0.0
          CRGSET          = 0.0
          XRGSET = 1.0

          CALL ML_PHASEI (
     &      AGEFAC, BIOMAS, BIOMS1, BIOMS2, CNSD1, CNSD2, 
     &      CSD2, CSD1, CUMDEP, CUMDTT, CUMPH, DLAYR, NCOUNT,
     &      DTT, EMAT, P5, G4, G5, XRGSET, GPP, GRAINN, GRNWT,
     &      GROLF, GRORT, GROSTM, ICSDUR, IDUR1, ISM,
     &      ISTAGE, ISWWAT,ISWNIT,LAI, LEAFNO, LFWT, MGROLF, 
     &      MGROPAN,MGROSTM, MLFWT, MPANWT, MPLAG, MPLA, MSTMWT, NLAYR,
     &      NSTRES, P3, P4, P9, PAF, PANWT, PGC, PHINT, PLA, PLAN, PLAO,
     &      PLATO, PLAY, PLAMX, PLTPOP, PTF, PWA, RANC, RESERVE,
     &      RLV, ROOTN, RTDEP, RTWT, RWU, SDEPTH, SEEDRV, SENLA, 
     &      SIND, SLAN, STMWT, STOVWT,    
     &      TANC, TCARBO, TGROLF, TGROPAN,
     &      TGROSTM, TILN, TILSW, TLAG1, TLFWT, TLNO, TMNC, TPANWT,
     &      TPLA, TPLAG, TPSM, TSIZE, TSTMWT, SUMDTT, VANC,  
     &      VMNC, XNTI)

          IF (ISWWAT .EQ. 'N') RETURN

          CUMDEP         = 0.0
          DO L = 1, NLAYR
             CUMDEP = CUMDEP + DLAYR(L)
             IF (SDEPTH .LT. CUMDEP) GO TO 100       ! Was EXIT
          END DO

  100     CONTINUE                                   ! Sun Fix

          L0 = L
          RETURN

C--------------------------------------------------------------------
C      ISTAGE = 8: Determine Germination Date
C--------------------------------------------------------------------
       ELSEIF (ISTAGE .EQ. 8) THEN
          IF (ISWWAT .NE. 'N') THEN
             IF (SW(L0) .LE. LL(L0)) THEN
                 SWSD = (SW(L0)-LL(L0))*0.65 + (SW(L0+1)-LL(L0+1))*0.35
                 NDAS = NDAS + 1

                 IF (SWSD .LT. SWCG) RETURN
             ENDIF
          ENDIF

          STGDOY(ISTAGE) = YRDOY
          IPRINT = 0

          CALL ML_PHASEI (
     &      AGEFAC, BIOMAS, BIOMS1, BIOMS2, CNSD1, CNSD2, 
     &      CSD2, CSD1, CUMDEP, CUMDTT, CUMPH, DLAYR, NCOUNT,
     &      DTT, EMAT, P5, G4, G5, XRGSET, GPP, GRAINN, GRNWT,
     &      GROLF, GRORT, GROSTM, ICSDUR, IDUR1, ISM,
     &      ISTAGE, ISWWAT,ISWNIT,LAI, LEAFNO, LFWT, MGROLF, 
     &      MGROPAN,MGROSTM, MLFWT, MPANWT, MPLAG, MPLA, MSTMWT, NLAYR,
     &      NSTRES, P3, P4, P9, PAF, PANWT, PGC, PHINT, PLA, PLAN, PLAO,
     &      PLATO, PLAY, PLAMX, PLTPOP, PTF, PWA, RANC, RESERVE,
     &      RLV, ROOTN, RTDEP, RTWT, RWU, SDEPTH, SEEDRV, SENLA, 
     &      SIND, SLAN, STMWT, STOVWT,    
     &      TANC, TCARBO, TGROLF, TGROPAN,
     &      TGROSTM, TILN, TILSW, TLAG1, TLFWT, TLNO, TMNC, TPANWT,
     &      TPLA, TPLAG, TPSM, TSIZE, TSTMWT, SUMDTT, VANC,  
     &      VMNC, XNTI)

          RETURN


C--------------------------------------------------------------------
C      ISTAGE = 9: Determine seedling emergence date
C--------------------------------------------------------------------
       ELSEIF (ISTAGE .EQ. 9) THEN
          NDAS = NDAS + 1
          RTDEP  = RTDEP + 0.15*DTT
          IDUR1  = IDUR1 + 1

          IF (SUMDTT .LT. P9) RETURN
          IF (P9 .GT. DGET) THEN
             ISTAGE = 6
             PLTPOP = 0.00
             GPP    = 1.0
             GRNWT  = 0.0

             WRITE(MESSAGE(1),1399)
             CALL WARNING(1, 'MLPHEN', MESSAGE)

             WRITE (     *,1399)
             IF (IDETO .EQ. 'Y') THEN
                WRITE (NOUTDO,1399)
             ENDIF
             MDATE = YRDOY
             CropStatus = 12
             RETURN
          ENDIF

          STGDOY(ISTAGE) = YRDOY

          CALL ML_PHASEI (
     &      AGEFAC, BIOMAS, BIOMS1, BIOMS2, CNSD1, CNSD2, 
     &      CSD2, CSD1, CUMDEP, CUMDTT, CUMPH, DLAYR, NCOUNT,
     &      DTT, EMAT, P5, G4, G5, XRGSET, GPP, GRAINN, GRNWT,
     &      GROLF, GRORT, GROSTM, ICSDUR, IDUR1, ISM,
     &      ISTAGE, ISWWAT,ISWNIT,LAI, LEAFNO, LFWT, MGROLF, 
     &      MGROPAN,MGROSTM, MLFWT, MPANWT, MPLAG, MPLA, MSTMWT, NLAYR,
     &      NSTRES, P3, P4, P9, PAF, PANWT, PGC, PHINT, PLA, PLAN, PLAO,
     &      PLATO, PLAY, PLAMX, PLTPOP, PTF, PWA, RANC, RESERVE,
     &      RLV, ROOTN, RTDEP, RTWT, RWU, SDEPTH, SEEDRV, SENLA, 
     &      SIND, SLAN, STMWT, STOVWT,    
     &      TANC, TCARBO, TGROLF, TGROPAN,
     &      TGROSTM, TILN, TILSW, TLAG1, TLFWT, TLNO, TMNC, TPANWT,
     &      TPLA, TPLAG, TPSM, TSIZE, TSTMWT, SUMDTT, VANC,  
     &      VMNC, XNTI)
          RETURN

C--------------------------------------------------------------------
C        ISTAGE = 1: Determine end of juvenile stage
C--------------------------------------------------------------------
       ELSEIF (ISTAGE .EQ. 1) THEN
          NDAS   = NDAS + 1
          XSTAGE = 2. * SUMDTT / P1
          IDUR1  = IDUR1 + 1
          IF (SUMDTT .LT. P1) RETURN
          STGDOY(ISTAGE) = YRDOY


C -------------------------------------------------------------------
C         ISTAGE = 2: Determine date of panicle initiation
C--------------------------------------------------------------------
       ELSEIF (ISTAGE .EQ. 2) THEN
          NDAS   = NDAS + 1
          XSTAGE = 2.0 + SIND
          IDUR1  = IDUR1 + 1
          PDTT   = DTT
          
          IF (ISWWAT .EQ. 'N') THEN
             ICSDUR = ICSDUR + 1
             ENDIF

          IF (ICSDUR .EQ. 1) THEN
             PDTT = SUMDTT - P1
          ENDIF

          !TWILEN = AMAX1 (TWILEN,P2O)
          IF (TWILEN .GT. P2O) THEN
            !RATEIN = 1.0/(68.0+P2R*(TWILEN-P2O))
            RATEIN = 1.0/(DJTI+P2R*(TWILEN-P2O))
          ELSE
            !RATEIN = 1.0/68.0
            RATEIN = 1.0/DJTI
          ENDIF

          SIND = SIND + RATEIN*PDTT
          IF (SIND .LT. 1.0) RETURN
          STGDOY(ISTAGE) = YRDOY

C--------------------------------------------------------------------
C         ISTAGE = 3: Determine end of leaf growth
C--------------------------------------------------------------------
       ELSEIF (ISTAGE .EQ. 3) THEN
          NDAS   = NDAS + 1
          IDUR1 = IDUR1 + 1
          XSTAGE = 3.0 + 2.0*SUMDTT/P3
          IF (SUMDTT .LT. P3) RETURN
          STGDOY(ISTAGE) = YRDOY
          MAXLAI = LAI

          !chp added these - Bill - check!!!! 07/02/03
              CANNAA = STOVN*PLTPOP
              CANWAA = BIOMAS

! KJB - to add temp effect on seed set
              NCOUNT = 0
              CRGSET = 0.0
C--------------------------------------------------------------------
C         ISTAGE = 4: Determine end of panicle growth
C--------------------------------------------------------------------
       ELSEIF (ISTAGE .EQ. 4) THEN
          NDAS = NDAS + 1
          PFLOWR = 50.0
          XSTAGE = 5.0 + SUMDTT/PFLOWR
! KJB - to add temp effect on seed set
          NCOUNT = NCOUNT + 1
          
C  KJB:  ADD TEMP EFFECT ON GRAIN-SET, AVERAGE TEMP PRIOR days of ISTAGE4

      RGSET = CURV(SETTYP,SETTC(1),SETTC(2),SETTC(3),SETTC(4),TEMPST)
      RGSET  = AMAX1(RGSET,0.0)
      RGSET = AMIN1(RGSET,1.0)
          CRGSET = CRGSET + RGSET
          XRGSET = CRGSET/NCOUNT
C  KJB
C  Previous code tried, but failed to compute WSTR1 in PHASEI, always csd1=0 and icsdur=1
C  WSTR1 was not passed in initial code.  No longer use it, as PCARB during ISTAGE4
C  carries water stress effect now via CARBO
          IF (ISWWAT .EQ. 'N') THEN
              WSTR1 = 0.0
          ELSE
             WSTR1 = CSD1/ICSDUR
          ENDIF
C  KJB
          IF (SUMDTT .LE. PFLOWR) THEN
             IDUR1 = IDUR1 + 1
             ENDIF

          IF (SUMDTT .GE. PFLOWR .AND. IPRINT .EQ. 0) THEN
             STGDOY(ISTAGE) = YRDOY
             BIOMS2 = BIOMAS/PLTPOP
             ISDATE = YRDOY
             MAXLAI2 = LAI
             IPRINT = 1
             ENDIF

          IF (SUMDTT .LT. P4) RETURN
             STGDOY(16) = YRDOY
             XSTAGE     = 6.0 + 0.5 * (SUMDTT - 50.0) / (P4 - 50.0)
          IF (GPP .LE. 0.0) THEN
             GPP   = 1.0
             ENDIF

          MAXLAI3 = LAI
!chp          MAXLAI  = AMAX1(MAXLAI1,MAXLAI2,MAXLAI3)
          MAXLAI  = AMAX1(MAXLAI2,MAXLAI3)

C--------------------------------------------------------------------
C        ISTAGE = 5: Determine end of effective filling period
C--------------------------------------------------------------------
       ELSEIF (ISTAGE .EQ. 5) THEN
          NDAS = NDAS + 1

C
C            Determine end of grain filling for millet and sorghum
C
          XSTAGE = 6.5 + 2.5*SUMDTT/P5
          IF (SUMDTT .LT. P5) RETURN

          IF (ISM .GT. 0) THEN
             IF (SUMDTT .LT. P5 + 90.0) RETURN
                 EMAT = EMAT + 1
                 IF (EMAT .LT. 2) RETURN
                 STGDOY (ISTAGE) = YRDOY
             ELSE
                 ISM         = 1
                 STGDOY (17) = YRDOY
             RETURN
             ENDIF


C--------------------------------------------------------------------
C       ISTAGE = 6: Determine physiological maturity
C--------------------------------------------------------------------
       ELSEIF (ISTAGE .EQ. 6) THEN
          IF (DTT    .LT. 2.0) RETURN
          IF (SUMDTT .LT. 2.0) RETURN

          STGDOY(ISTAGE) = YRDOY
          IPRINT         = 0
          MDATE          = YRDOY
          CropStatus     = 1
          GRNWT  = PANWT - 1.1 * PWA
          GRNWT  = AMAX1(GRNWT,0.0)
          YIELD  = GRNWT*10.0*PLTPOP

          IF (PLTPOP .NE. 0.0) THEN
              IF (GPP .LE. 0.0) THEN
                  GPP = 1.0
                  ENDIF
              SKERWT = GRNWT/GPP
              GPSM = GPP*PLTPOP
              STOVER  = BIOMAS*10.0-YIELD
 
              YIELDB  = YIELD/62.8
              PGRNWT = SKERWT*1000.0
              IF (ISWNIT .EQ. 'N') THEN
                  XGNP = 0.0
                  GNUP = 0.0
                ELSE
                  IF (GRNWT .GT. 0.0) THEN
                   XGNP = (GRAINN/GRNWT)*100.0
                   GNUP = GRAINN*PLTPOP*10.0
                   ENDIF
              ENDIF

             TOTNUP      = GNUP + APTNUP

             SI1(ISTAGE) = 0.0
             SI2(ISTAGE) = 0.0
             SI3(ISTAGE) = 0.0
             SI4(ISTAGE) = 0.0
          ENDIF
      ENDIF       !End of main ISTAGE Logic

C--------------------------------------------------------------------
C     This code is run at the beginning of ISTAGE 1, 2, 3, 4, 5, 6
C
C     This code is NOT run for ISTAGE 7, 8, or 9 due to the RETURN
C     statements in the above sections of code for ISTAGE 7, 8, and 9
C--------------------------------------------------------------------

      IF (ISWWAT .NE. 'N'.AND.ISTAGE.GT.0) THEN
         SI1(ISTAGE) = CSD1  / ICSDUR
         SI2(ISTAGE) = CSD2  / ICSDUR
         SI3(ISTAGE) = CNSD1 / ICSDUR
         SI4(ISTAGE) = CNSD2 / ICSDUR
      ENDIF


      CALL ML_PHASEI (
     &  AGEFAC, BIOMAS, BIOMS1, BIOMS2, CNSD1, CNSD2, 
     &  CSD2, CSD1, CUMDEP, CUMDTT, CUMPH, DLAYR, NCOUNT,
     &  DTT, EMAT, P5, G4, G5, XRGSET, GPP, GRAINN, GRNWT,
     &  GROLF, GRORT, GROSTM, ICSDUR, IDUR1, ISM,
     &  ISTAGE, ISWWAT,ISWNIT,LAI, LEAFNO, LFWT, MGROLF, 
     &  MGROPAN,MGROSTM, MLFWT, MPANWT, MPLAG, MPLA, MSTMWT, NLAYR,
     &  NSTRES, P3, P4, P9, PAF, PANWT, PGC, PHINT, PLA, PLAN, PLAO,
     &  PLATO, PLAY, PLAMX, PLTPOP, PTF, PWA, RANC, RESERVE,
     &  RLV, ROOTN, RTDEP, RTWT, RWU, SDEPTH, SEEDRV, SENLA, 
     &  SIND, SLAN, STMWT, STOVWT,    
     &  TANC, TCARBO, TGROLF, TGROPAN,
     &  TGROSTM, TILN, TILSW, TLAG1, TLFWT, TLNO, TMNC, TPANWT,
     &  TPLA, TPLAG, TPSM, TSIZE, TSTMWT, SUMDTT, VANC,  
     &  VMNC, XNTI)

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

1399  FORMAT (10X,'Seed ran out of metabolite due to deep planting')

      END SUBROUTINE ML_PHENOL
