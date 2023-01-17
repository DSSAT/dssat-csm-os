C=======================================================================
C  SG_PHENOL, Subroutine
C
C  Determines phenological stage for sorghum
C-----------------------------------------------------------------------
C  Revision history
C
C  12/31/1996 GH  Deleted phenology statements.
C  02/07/1993 PWW Header revision and minor changes                   
C  02/07/1993 PWW Added switch block, code cleanup                    
C  02/07/1993 PWW Modified TT calculations to reduce line #'s         
C  07/31/2002 WDB Converted to modular format   
C  12/14/2005 CHP Added phenology revisions from P.S.Traore  
C                 Added alternate cultivar coefficients for Traore method
C  02/07/2007 JWW Added P9 calculation
C  01/15/2008 GH  Include GDDE for P9 calculation
C  01/16/2008 GH  Rename DJTI to P2 and move to cultivar file
C  01/16/2008 GH  Add cultivar coefficient PANTH
C  06/01/2007 GH  Add P-model (unfinished)
C  01/06/2014 MA  Change to couple the P model     
!  06/15/2022 CHP Added CropStatus
C-----------------------------------------------------------------------
C  INPUT  : YRDOY
C
C  LOCAL  : I, L,L0,XANC,TEMPCN,TEMPCX,XS,TDIF,TCOR,
C           TTMP,YDL,DEC,DLV,CHGDL,SWSD,PDTT,RATEIN,PFLOWR,PSKER,
C           YIELDB,TWILEN,BARFAC,ABSTRES
C
C  OUTPUT : STGDOY
C-----------------------------------------------------------------------
C  Called : SG_CERES
C
C  Calls  : SG_PHASEI SG_COLD
C-----------------------------------------------------------------------
C
C                         DEFINITIONS
C
C  STGDOY :
C  YRDOY  :
C  NOUTDO :
C=======================================================================

      SUBROUTINE SG_PHENOL (DYNAMIC,
     & APTNUP,BIOMAS,BIOMS2,CSD1, CSD2,
     & CNSD1, CNSD2, CTYPE, CUMDEP, DLAYR, DTT, 
     & EMAT, G1, G2, GDDE, GNUP, GPP, GPSM, GRAINN, GRNWT, ICSDUR,
     & IDETO, IDUR1, IPRINT, ISDATE, ISTAGE, ISWNIT, ISWWAT,
     & LAI, LEAFNO, LL, MAXLAI, MDATE, NOUTDO, NLAYR, 
     & P1, P2O, P2R, P3, P4, P5, P9, PANTH, PANWT, PGRNWT, PHINT,
     & PBASE, PSAT, PFLOWR, 
     & PLTPOP, ROPT, RTDEP, SDEPTH, SI1, SI2, SI3,
     & SI4, SIND, SKERWT, SNOW, SRAD, STGDOY,STMWT, STOVER, STOVN, 
     & SUMDTT, SW, TANC, TBASE, TEMPCR, TMAX, TMIN, 
     & TOPT,TOTNUP, TPSM, YIELD, YRDOY,XGNP, XSTAGE,  
C      Variables passed through PHENOL to phasei but not used in phenol
     & AGEFAC, BIOMS1, CUMDTT, CUMPH, GROLF,
     & GRORT, GROSTM, LFWT, LWMIN, MGROLF, MGROPAN, MGROSTM, 
     & MLFWT, MPANWT, MSTMWT, NSTRES, PAF, 
     & PGC, PLA, PLAN, PLAO, PLATO, PLAY, PLAMX, PTF, RANC, 
     & RLV, ROOTN, RTWT, RWU, SEEDRV, SENLA, SLAN, 
     & STOVWT, SUMRTR, SWMAX, SWMIN, TCARBO, TCNP, TDUR, TGROLF,
     & TGROPAN, TGROSTM, TILN, TLFWT, TLNO, TMNC,
     & TPANWT, TSIZE, TSTMWT, VANC, VMNC,  
     & XNTI,SWFAC,TURFAC,DGET,SWCG,P2, 
     & DAYL, TWILEN, CANWAA, CANNAA,CUMP4,
     & SeedFrac, VegFrac, CropStatus)                                 

      USE ModuleDefs
      IMPLICIT  NONE
      EXTERNAL SG_PHASEI, WARNING
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
      REAL GROLF
      REAL GRORT
      REAL GROSTM
      REAL LFWT
      REAL LWMIN
      REAL MGROLF
      REAL MGROPAN
      REAL MGROSTM
      REAL MLFWT
      REAL MPANWT
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
      REAL RLV(NL)
      REAL ROOTN
      REAL RTWT
      REAL RWU(NL)
      REAL SEEDRV
      REAL SENLA
      REAL SLAN
      REAL STOVWT
      REAL SUMRTR
      REAL SWCG
      REAL SWFAC
      REAL SWMAX
      REAL SWMIN
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
      REAL TSIZE
      REAL TSTMWT
      REAL TURFAC
      REAL VANC
      REAL VMNC
      REAL XNTI
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
C-GH  REAL DJTI
      REAL P2, CUMP2, CUMP4, PANTH
      REAL DLAYR(NL)
      REAL DTT
      REAL EMAT
      REAL G1
      REAL G2
      REAL GDDE
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
      INTEGER   STGDOY(20),YRDOY,I, L,L0
      REAL      XANC,TEMPCN,TEMPCX,XS,
     +          SWSD,PDTT,RATEIN,PFLOWR,
     +          YIELDB,TWILEN
      REAL      DAYL,ACOEF,TNSOIL,TDSOIL,TMSOIL
      REAL      TH,DOPT

!     Variables needed to send messages to WARNING.OUT
      CHARACTER*78 MESSAGE(10)

!     Variables added for sorghum methods PST/CHP
      REAL PBASE, PSAT
!     Added for P model
      REAL SeedFrac, VegFrac
      REAL SUMDTT_2, SUMDTT_3

      INTEGER DYNAMIC
!----------------------------------------------------------------------
!         DYNAMIC = RUNINIT OR DYNAMIC = SEASINIT
! ---------------------------------------------------------------------
      IF (DYNAMIC.EQ.RUNINIT.OR.DYNAMIC.EQ.SEASINIT) THEN
      SeedFrac = 0.0
      VegFrac  = 0.0
      SUMDTT_2 = 0.0
      SUMDTT_3 = 0.0

      STGDOY = -99

!----------------------------------------------------------------------
!         DYNAMIC = RATE OR INTEGRATE
! ---------------------------------------------------------------------

      ELSE    

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
         ! This statement replaces DTT = TOPT ... 
         !    GoL and LAH, CIMMYT, 1999
         !
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
      
C--------------------------------------------------------------------
C       Definition of Stages
C
C     7 - Sowing date
C     8 - Germination
C     9 - Emergence
C     1 - End juvenile
C     2 - Pannicle initiation
C     3 - End of flag leaf expansion
C     4 - Beginning of grain filling
C     5 - Grain fill
C     6 - Maturity
C--------------------------------------------------------------------

C--------------------------------------------------------------------
C         ISTAGE = 7: Determine when sowing date occurs
C--------------------------------------------------------------------
      IF (ISTAGE .EQ. 7) THEN
          STGDOY(ISTAGE) = YRDOY

          CALL SG_PHASEI (
     &    AGEFAC, BIOMAS, BIOMS1, BIOMS2, CNSD1, CNSD2, 
     &    CSD2, CSD1, CUMDEP, CUMDTT, CUMPH, CUMP2, CUMP4, DLAYR,
     &    DTT, EMAT, G1, G2, GPP, GRAINN, GRNWT,
     &    GROLF, GRORT, GROSTM, ICSDUR, IDUR1, 
     &    ISTAGE, ISWWAT,ISWNIT,LAI, LEAFNO, LFWT, LWMIN, MGROLF, 
     &    MGROPAN,MGROSTM, MLFWT, MPANWT, MSTMWT, NLAYR,
     &    NSTRES, P3, P4, P9, PAF, PANWT, PGC, PHINT, PLA, PLAN, PLAO,
     &    PLATO, PLAY, PLAMX, PLTPOP, PTF, RANC, PANTH, PFLOWR,
     &    RLV, ROOTN, RTDEP, RTWT, RWU, SDEPTH, SEEDRV, SENLA, 
     &    SIND, SLAN, STMWT, STOVN, STOVWT, SUMRTR, SWMAX, 
     &    SWMIN, TANC, TCARBO, TCNP, TDUR, TGROLF, TGROPAN,
     &    TGROSTM, TILN, TLFWT, TLNO, TMNC, TPANWT,
     &    TPSM, TSIZE, TSTMWT, SUMDTT, VANC,  
     &    VMNC, XNTI,P2O, P2R, TWILEN)

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
                 IF (SWSD .LT. SWCG) RETURN
             ENDIF
          ENDIF

          STGDOY(ISTAGE) = YRDOY
          IPRINT = 0

          CALL SG_PHASEI (
     &    AGEFAC, BIOMAS, BIOMS1, BIOMS2, CNSD1, CNSD2, 
     &    CSD2, CSD1, CUMDEP, CUMDTT, CUMPH, CUMP2, CUMP4, DLAYR,
     &    DTT, EMAT, G1, G2, GPP, GRAINN, GRNWT,
     &    GROLF, GRORT, GROSTM, ICSDUR, IDUR1, 
     &    ISTAGE, ISWWAT,ISWNIT,LAI, LEAFNO, LFWT, LWMIN, MGROLF, 
     &    MGROPAN,MGROSTM, MLFWT, MPANWT, MSTMWT, NLAYR,
     &    NSTRES, P3, P4, P9, PAF, PANWT, PGC, PHINT, PLA, PLAN, PLAO,
     &    PLATO, PLAY, PLAMX, PLTPOP, PTF, RANC, PANTH, PFLOWR,
     &    RLV, ROOTN, RTDEP, RTWT, RWU, SDEPTH, SEEDRV, SENLA, 
     &    SIND, SLAN, STMWT, STOVN, STOVWT, SUMRTR, SWMAX, 
     &    SWMIN, TANC, TCARBO, TCNP, TDUR, TGROLF, TGROPAN,
     &    TGROSTM, TILN, TLFWT, TLNO, TMNC, TPANWT,
     &    TPSM, TSIZE, TSTMWT, SUMDTT, VANC,  
     &    VMNC, XNTI,P2O, P2R, TWILEN)

!         JWW 2/7/07
          P9    = 50.0 +  6.0*SDEPTH
C-GH
          P9    = 50.0 +  GDDE * SDEPTH
          RETURN

C--------------------------------------------------------------------
C      ISTAGE = 9: Determine seedling emergence date
C--------------------------------------------------------------------
       ELSEIF (ISTAGE .EQ. 9) THEN
          RTDEP  = RTDEP + 0.15*DTT
          IDUR1  = IDUR1 + 1
          IF (SUMDTT .LT. P9) RETURN
          IF (P9 .GT. DGET) THEN
             ISTAGE = 6
             PLTPOP = 0.00
             GPP    = 1.0
             GRNWT  = 0.0

             WRITE(MESSAGE(1),1399)
             CALL WARNING(1, 'SGPHEN', MESSAGE)
             WRITE (     *,1399)
             IF (IDETO .EQ. 'Y') THEN
                WRITE (NOUTDO,1399)
             ENDIF
             MDATE = YRDOY
             CropStatus = 12
             RETURN
          ENDIF

          STGDOY(ISTAGE) = YRDOY

          CALL SG_PHASEI (
     &    AGEFAC, BIOMAS, BIOMS1, BIOMS2, CNSD1, CNSD2, 
     &    CSD2, CSD1, CUMDEP, CUMDTT, CUMPH, CUMP2, CUMP4, DLAYR,
     &    DTT, EMAT, G1, G2, GPP, GRAINN, GRNWT,
     &    GROLF, GRORT, GROSTM, ICSDUR, IDUR1, 
     &    ISTAGE, ISWWAT,ISWNIT,LAI, LEAFNO, LFWT, LWMIN, MGROLF, 
     &    MGROPAN,MGROSTM, MLFWT, MPANWT, MSTMWT, NLAYR,
     &    NSTRES, P3, P4, P9, PAF, PANWT, PGC, PHINT, PLA, PLAN, PLAO,
     &    PLATO, PLAY, PLAMX, PLTPOP, PTF, RANC, PANTH, PFLOWR,
     &    RLV, ROOTN, RTDEP, RTWT, RWU, SDEPTH, SEEDRV, SENLA, 
     &    SIND, SLAN, STMWT, STOVN, STOVWT, SUMRTR, SWMAX, 
     &    SWMIN, TANC, TCARBO, TCNP, TDUR, TGROLF, TGROPAN,
     &    TGROSTM, TILN, TLFWT, TLNO, TMNC, TPANWT,
     &    TPSM, TSIZE, TSTMWT, SUMDTT, VANC,  
     &    VMNC, XNTI,P2O, P2R, TWILEN)
          RETURN

C--------------------------------------------------------------------
C        ISTAGE = 1: Determine end of juvenile stage
C--------------------------------------------------------------------
       ELSEIF (ISTAGE .EQ. 1) THEN
          XSTAGE = 2. * SUMDTT / P1
          IDUR1  = IDUR1 + 1

C     For P model, we need to estimate the fraction of time completed 
C     between emergence and tassel initiation, VegFrac.  Because stage 2 
C     (end of juvenile stage to tassel initiation) completion is not 
C     based on physiological time, but rather on daylight hours, we 
C     will make the assumption that the physical duration of that phase 
C     is 10 days at optimum temperature.

!  VegFrac = SUMDTT / (P1 + 10. * (DOPT - TBASE) + P3 + P4)
! MA 06-07jan2014, change to 20 days ( 10 is too few)
! VegFrac is now defined from emergence to beginning of grain filling
! VegFrac includes P1, P2 adapted according to P2R with should includes  PANTH( with P3)
         VegFrac = SUMDTT /(P1 +P2R + PANTH +P4) 

        IF (SUMDTT .LT. P1) RETURN
        STGDOY(ISTAGE) = YRDOY

C -------------------------------------------------------------------
C         ISTAGE = 2: Determine date of panicle initiation
C-MA from end of juvenile to tassel inititiation (P2 adapted to PP)
C--------------------------------------------------------------------
       ELSEIF (ISTAGE .EQ. 2) THEN
          XSTAGE = 2.0 + SIND
          IDUR1  = IDUR1 + 1
          PDTT   = DTT
          IF (ISWWAT .EQ. 'N') THEN
             ICSDUR = ICSDUR + 1
             ENDIF
          IF (ICSDUR .EQ. 1) THEN
             PDTT = SUMDTT - P1
          ENDIF
          
          
!        Add option for using PBASE, PSAT.  If these parameters 
!           are present (non-zero) then use alternate method.
         IF (PBASE < 1.E-2 .OR. PSAT < 1.E-2) THEN

C-GH-2008
C        Revised calculation of degree days to panicle initiation
C          Total degree days to panicle initiation is a function of
C          the basic degree days under optimum conditions and short
C          daylengths (P2) plus the delay due the number of hours 
C          above the critical daylength (P2R * (TWILEN - P2O))
C          Panicle initation occurs when the total number of 
C          degree days since the end of juvenile phase (CUMP2)
C          is greater than the minimum number of required
C          degree days (P2 + P2R * (TWILEN - P2O))
C
C          Original method
C
           IF (TWILEN > P2O) THEN
C-GH         RATEIN = 1. / (P2 +  P2R * (TWILEN - P2O))
             RATEIN = P2 +  P2R * (TWILEN - P2O)
           ELSE
C-GH         RATEIN = 1. / P2
             RATEIN = P2
           ENDIF
C-GH       SIND = SIND + RATEIN * PDTT
           CUMP2 = CUMP2 + PDTT
           SIND = AMAX1(SIND,CUMP2/RATEIN)

!         -----------------------------------------------------------
!        Threshold hyperbolic option
         ELSE     !
           IF (TWILEN > PSAT) THEN
             RATEIN = (TWILEN - PBASE) / (P1 *(PSAT - PBASE))
           ELSE
             RATEIN = 1. / P1
           ENDIF
           SIND = RATEIN * SUMDTT
         ENDIF

!------------------------------------------------------------------------
         
C-MA         IF (SUMDTT .GT. (P1 + 10. * (DOPT - TBASE))) THEN
C-MA              VegFrac = SUMDTT / (SUMDTT + P3 + P4)
C-MA           ELSE
C-MA              VegFrac = SUMDTT / (P1 + 10. * (DOPT - TBASE) + P3 + P4)
C-MA          ENDIF

         ! MA 06jan2014 !
          VegFrac = SUMDTT /(P1 + P2R+ PANTH +P4)
          
C-GH     IF (SIND .LT. 1.0) RETURN
         IF (CUMP2 .LT. RATEIN) RETURN
         
         IF (CUMP2 .LT. RATEIN) THEN
           RETURN
        ELSE
              STGDOY(ISTAGE) = YRDOY
!             SUMDTT_2 = p1 + adapted P2 without PANTH --MA
              SUMDTT_2 = SUMDTT  
              VegFrac= max (VegFrac,SUMDTT_2/(SUMDTT_2+PANTH+P4))
        ENDIF

     



C--------------------------------------------------------------------
C         ISTAGE = 3: Determine end of flag leaf expansion
C                     [formerly end of leaf growth]
C-MA from tassel initiation to end of leaf growth (PANTH-P3)
C--------------------------------------------------------------------
        
          ELSEIF (ISTAGE .EQ. 3) THEN
          IDUR1 = IDUR1 + 1
C-GH      XSTAGE = 3.0 + 2.0*SUMDTT/P3
          XSTAGE = 3.0 + 2.0*SUMDTT/(PANTH - P3)
C-MA      VegFrac = (SUMDTT + SUMDTT_2) / (SUMDTT_2 + P3 + P4)
          VegFrac = max (VegFrac,(SUMDTT_2+ SUMDTT)/
     &              (SUMDTT_2 + PANTH+P4))
C-MA          SeedFrac = SUMDTT / (P3 + P4 + P5)
 
C-GH      IF (SUMDTT .LT. P3) THEN
          IF (SUMDTT .LT. (PANTH - P3)) THEN
            RETURN
          ELSE
            STGDOY(ISTAGE) = YRDOY
            MAXLAI = LAI
!           SUMDDT_3 = P1 +P2 adapted + (PANTH -P3)
            SUMDTT_3 = SUMDTT_2 + SUMDTT 
            VegFrac = max (VegFrac,SUMDTT_3 /
     &                       (SUMDTT_3 + P3+P4)) 
          ENDIF

C--------------------------------------------------------------------
C         ISTAGE = 4: Determine beginning grain filling
C                     [formerly end of panicle growth]
C-MA from end leaf growth to beginning of grain filling, including P3+P4
C--------------------------------------------------------------------
       ELSEIF (ISTAGE .EQ. 4) THEN
C--------------------------------------------------------------------
C                     Determine beginning anthesis
C                     No ISTAGE
C--------------------------------------------------------------------
C-GH      PFLOWR = 2.5*PHINT+30.0
          IF (TWILEN .GT. P2O) THEN
            PFLOWR = P3 +  P2R * (TWILEN - P2O)
          ELSE
            PFLOWR = P3
          ENDIF

          XSTAGE = 5.0 + SUMDTT/PFLOWR
          IF (SUMDTT .LE. PFLOWR) THEN
             IDUR1 = IDUR1 + 1
          ENDIF
 
          IF (SUMDTT .GE. PFLOWR .AND. IPRINT .EQ. 0) THEN
             STGDOY(16)     = YRDOY
             CANNAA = STOVN*PLTPOP
             CANWAA = BIOMAS
             BIOMS2 = BIOMAS/PLTPOP
             ISDATE = YRDOY
             IPRINT = 1
          ENDIF

          CUMP4 = CUMP4 + DTT

!      VegFrac = MAX(VegFrac,(SUMDTT + SUMDTT_2) /
!     & (SUMDTT_2 + (PANTH)+P4))
       VegFrac = max(VegFrac,(SUMDTT + SUMDTT_3)/(SUMDTT_3+ PFLOWR+P4))
C-MA           SeedFrac = (SUMDTT + P3) / (P3 + P4 + P5)
     

          IF (SUMDTT .LT. (PFLOWR + P4)) THEN
            RETURN
          ELSE
            STGDOY(ISTAGE) = YRDOY
            VegFrac = 1.0
            

            IF (GPP .LE. 0.0) THEN
              GPP   = 1.0
            ENDIF
          ENDIF
C--------------------------------------------------------------------
C        ISTAGE = 5: Determine end of effective grain filling period
C--------------------------------------------------------------------
       ELSEIF (ISTAGE .EQ. 5) THEN
	       CUMP4 = CUMP4 + DTT
             XSTAGE = 6.5 + 2.5*SUMDTT/P5
C-MA             SeedFrac = (SUMDTT + P3 + P4) / (P3 + P4 + P5)
             SeedFrac = SUMDTT / (P5)

             IF (SUMDTT .LT. P5) RETURN
             SeedFrac = 1.0
  
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
          GRNWT  = PANWT  * 0.8
          GRAINN = GRAINN - 0.2  * PANWT*TANC
          STOVN  = STOVN  + 0.2  * PANWT*TANC
          APTNUP = STOVN  * 10.0 * PLTPOP
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
             PGRNWT = GRNWT/GPP*1000.0
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


      CALL SG_PHASEI (
     &    AGEFAC, BIOMAS, BIOMS1, BIOMS2, CNSD1, CNSD2, 
     &    CSD2, CSD1, CUMDEP, CUMDTT, CUMPH, CUMP2, CUMP4, DLAYR,
     &    DTT, EMAT, G1, G2, GPP, GRAINN, GRNWT,
     &    GROLF, GRORT, GROSTM, ICSDUR, IDUR1, 
     &    ISTAGE, ISWWAT,ISWNIT,LAI, LEAFNO, LFWT, LWMIN, MGROLF, 
     &    MGROPAN,MGROSTM, MLFWT, MPANWT, MSTMWT, NLAYR,
     &    NSTRES, P3, P4, P9, PAF, PANWT, PGC, PHINT, PLA, PLAN, PLAO,
     &    PLATO, PLAY, PLAMX, PLTPOP, PTF, RANC, PANTH, PFLOWR,
     &    RLV, ROOTN, RTDEP, RTWT, RWU, SDEPTH, SEEDRV, SENLA, 
     &    SIND, SLAN, STMWT, STOVN, STOVWT, SUMRTR, SWMAX, 
     &    SWMIN, TANC, TCARBO, TCNP, TDUR, TGROLF, TGROPAN,
     &    TGROSTM, TILN, TLFWT, TLNO, TMNC, TPANWT,
     &    TPSM, TSIZE, TSTMWT, SUMDTT, VANC,  
     &    VMNC, XNTI,P2O, P2R, TWILEN)

! ----------------------------------------------------------------------
!          ENDIF            ! End Dynamic Loop
! ----------------------------------------------------------------------

      ENDIF  ! End DYNAMIC STRUCTURE
      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

1399  FORMAT (10X,'Seed ran out of metabolite due to deep planting')

      END SUBROUTINE SG_PHENOL
