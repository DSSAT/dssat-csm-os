C=======================================================================
C  ROOTS, Subroutine, G. Hoogenboom, J.W. Jones
C-----------------------------------------------------------------------
C
C  Calculates root growth, extension, respiration, and senescence
C
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/09/1989 GH  Written.
C  09/29/1995 KJB Changed to lessen effect of water deficit on root depth
C                 increase.
C  01/19/1996 JWJ Added effects of excess water.
C  01/20/1996 KJB Increase root extension rate under drought stress
C  09/13/1998 CHP Modified for modular format
C  09/14/1998 CHP Changed TROOT to TRLV to match same variable in ROOTDM
C                 Changed SWDF1 to SWFAC to match variable name in WATBAL
C  05/11/1999 GH  Incorporated in CROPGRO
!  02/21/2005 SJR Moved ISWWAT condition here to allow computation of 
!                 root senescence even when water not simulated.
!  10/04/2005 SJR Include senescence due to water stress in total 
!                 daily senescence.
!  10/20/2005 CHP Added optional minimum root mass for senescence, 
!                 RTWTMIN, to species file
!  01/19/2006 CHP Fixed discrepancies between plant root senescence  
!                 calculated and that sent to soil routines for addition
!                 to organic matter.  
!-----------------------------------------------------------------------
!  Called by  :  PLANT
!  Calls      :  IPROOT, INROOT
!=======================================================================

      SUBROUTINE ROOTS(DYNAMIC,
     &    AGRRT, CROP, DLAYR, DS, DTX, DUL, FILECC, FRRT, !Input
     &    ISWWAT, LL, NLAYR, PG, PLTPOP, RO, RP, RTWT,    !Input
     &    SAT, SW, SWFAC, VSTAGE, WR, WRDOTN, WTNEW,      !Input
     &    RLV, RTDEP, SATFAC, SENRT, SRDOT)               !Output

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL IPROOT, INROOT, TABEX
      SAVE

      CHARACTER*1 ISWWAT
      CHARACTER*2 CROP
      CHARACTER*92 FILECC

      INTEGER L, L1, NLAYR

      INTEGER DYNAMIC

      REAL CUMDEP, DEPMAX, DTX, FRRT,
     &  PG, RFAC1, RFAC2, RFAC3,
     &  RLDSM, RLNEW, RO, RP,
     &  RTDEP, RTSDF, RTSEN, RTWT, SRDOT, SWDF, SWFAC,
     &  TRLDF, TRLV, WRDOTN   !, TRTDY
      REAL CGRRT, AGRRT
      REAL SWEXF, PORMIN, RTEXF, RTSURV
      REAL RTDEPI, SUMEX, SUMRL, SATFAC
      REAL PLTPOP, WTNEW, VSTAGE
      REAL TABEX              !Function subroutine located in UTILS.for
      REAL XRTFAC(4), YRTFAC(4)
      REAL DLAYR(NL), DS(NL), DUL(NL), ESW(NL), LL(NL), RLDF(NL)
      REAL RLGRW(NL), RLSEN(NL), RLV(NL), RLV_WS(NL), RRLF(NL)
      REAL SW(NL), SAT(NL), WR(NL)
      REAL GRESPR(NL), MRESPR(NL), RESPS(NL)
      REAL SENRT(NL)

!     Added 10/20/2005 for minimum RLV calculations
!     RTWTMIN = minimum root mass per layer; used to limit senescence
!                 (g/m2) (species file parameter)
!     TRLV_MIN  = conversion of RTWTMIN to RLV units per layer
      REAL TRLV_MIN, RLSENTOT, FACTOR, RTWTMIN
      REAL TotRootMass, CumRootMass

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
      CALL IPROOT(FILECC,                                 !Input
     &  PORMIN, RFAC1, RLDSM, RTDEPI, RTEXF,              !Output
     &  RTSEN, RTSDF, RTWTMIN, XRTFAC, YRTFAC)            !Output

      DEPMAX = DS(NLAYR)

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      SRDOT = 0.0       
      RLV   = 0.0
      RTDEP = 0.0       
      SENRT = 0.0
      SUMEX = 0.0
      SUMRL = 0.0
      SATFAC = 0.0

!-----------------------------------------------------------------------
C     ROOT DEPTH INCREASE RATE WITH TIME, cm/physiological day
C-----------------------------------------------------------------------
!      IF (CROP .NE. 'FA' .AND. ISWWAT .NE. 'N') THEN
      IF (CROP .NE. 'FA') THEN
        RFAC2 = TABEX(YRTFAC,XRTFAC,0.0,4)
      ENDIF
      
      CumRootMass = 0.0

!***********************************************************************
!***********************************************************************
!     EMERGENCE CALCULATIONS - Performed once per season upon emergence
!         or transplanting of plants
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. EMERG) THEN
!-----------------------------------------------------------------------
!       Call INROOT for initialization of root variables on
!       day of emergence.  (GROW emergence initialization
!       must preceed call to INROOT.)
!-----------------------------------------------------------------------
      CALL INROOT(
     &  DLAYR, FRRT, NLAYR, PLTPOP, RFAC1, RTDEPI, WTNEW, !Input
     &  RLV, RTDEP)                                       !Output

      RFAC3 = RFAC1

      TRLV = 0.0
      DO L = 1,NLAYR
        TRLV = TRLV + RLV(L) * DLAYR(L) ! cm[root] / cm2[ground]
      ENDDO

      CumRootMass = WTNEW * FRRT * PLTPOP * 10. 
!        kg[root]  g[tissue] g[root]    plants   kg/ha
!        -------- = ----- * --------- * ------ * ----- 
!           ha      plant   g[tissue]     m2      g/m2

!***********************************************************************
!***********************************************************************
!     DAILY RATE/INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
C     Calculate Root Depth Rate of Increase, Physiological Day (RFAC2)
C-----------------------------------------------------------------------
      RFAC2 = TABEX(YRTFAC, XRTFAC, VSTAGE, 4)
      RLNEW = WRDOTN * RFAC1 / 10000.
      CGRRT = AGRRT * WRDOTN

C-----------------------------------------------------------------------
C     Calculate root length per cm2 soil and initiate growth,
C     respiration and senescence by layer
C-----------------------------------------------------------------------
!      TRTDY = 0.0
!     1/19/2006 Remove TRTDY and replace with TRLV -- RLV is only updated
!     once, so yesterday's value is stored in TRLV here.
      DO L = 1,NLAYR
!       TRTDY = TRTDY + RLV(L) * DLAYR(L) ! cm[root] / cm2[ground]
        RRLF(L)   = 0.0
        RLSEN(L)  = 0.0
        RLGRW(L)  = 0.0
        MRESPR(L) = 0.0
        GRESPR(L) = 0.0
        RESPS(L)  = 0.0
      ENDDO

!     Update RFAC3 based on yesterday's RTWT and TRLV
      IF (RTWT - WRDOTN .GE. 0.0001 .AND. TRLV .GE. 0.00001) THEN
!       RFAC3 = TRTDY * 10000.0 / (RTWT - WRDOTN)
!       RTWT has not yet been updated today, so use yesterday's
!       value and don't subtract out today's growth - chp 11/13/00
        RFAC3 = TRLV * 10000.0 / RTWT
      ELSE
        RFAC3 = RFAC1
      ENDIF

!     10/20/2005 Limit RLV decrease due to senscence to 
!       a minimum resulting root weight 
      IF (RTWTMIN > 0.0) THEN
        TRLV_MIN = RTWTMIN * RFAC3 / 1.E4   !same units as TRLV
!        cm/cm2  =  (g/m2) *(cm/g) / (cm2/m2)
      ELSE
!       Set TRLV_MIN to zero -- no minimum root mass
        TRLV_MIN = 0.0
      ENDIF
      
!-----------------------------------------------------------------------
      TRLDF  = 0.0
      CUMDEP = 0.0
      SUMEX  = 0.0
      SUMRL  = 0.0
      RLV_WS = 0.0
      RLSEN  = 0.0

      DO L = 1,NLAYR
        L1 = L
        CUMDEP = CUMDEP + DLAYR(L)
        SWDF = 1.0
        SWEXF = 1.0

C-----------------------------------------------------------------------
C     2/21/05 - SJR - move conditional call for water stress from CROPGRO 
C     to ROOTS.  Allows root senescence when Water dynamics option is 
C     turned off.  Water stress options set to no stress levels.  This 
C     also allows output of root growth dynamics without limimiting 
C     water or N uptake. 
C-----------------------------------------------------------------------
        IF (ISWWAT .EQ. 'Y') THEN
          IF (SAT(L)-SW(L) .LT. PORMIN) THEN
            SWEXF = (SAT(L) - SW(L)) / PORMIN
            SWEXF = MIN(SWEXF, 1.0)
          ENDIF

          SUMEX = SUMEX + DLAYR(L) * RLV(L) * (1.0 - SWEXF)
          SUMRL = SUMRL + DLAYR(L) * RLV(L)

          ESW(L) = DUL(L) - LL(L)
          IF (SW(L) - LL(L) .LT. 0.25*ESW(L)) THEN
            SWDF = (SW(L) - LL(L)) / (0.25*ESW(L))
            SWDF = MAX(SWDF, 0.0)
          ENDIF
        ENDIF
C-----------------------------------------------------------------------

        RTSURV = MIN(1.0,(1.-RTSDF*(1.-SWDF)),(1.-RTEXF*(1.-SWEXF)))
        IF (RLV(L) .GT. RLDSM .AND. TRLV + RLNEW > TRLV_MIN) THEN
!         1/14/2005 CHP Don't subtract water stress senescence 
!           yet - combine with natural senescence and check to see if 
!           enough RLV for senescence to occur (TRLV > TRLV_MIN)
          !RLV(L) = RLV(L) * RTSURV
          RLV_WS(L) = RLV(L) * (1.0 - RTSURV)
        ELSE
          RLV_WS(L) = 0.0
        ENDIF

C-----------------------------------------------------------------------
        RLDF(L) = WR(L) * DLAYR(L) * MIN(SWDF,SWEXF)
        IF (CUMDEP .LT. RTDEP) THEN
          TRLDF = TRLDF + RLDF(L)
        ELSE
          IF (WR(L) .GT. 0.0 .AND. RLNEW .GT. 0.0) THEN
            IF (L .EQ. 1) THEN
              RTDEP = RTDEP + DTX * RFAC2
            ELSE
              RTDEP = RTDEP + DTX * RFAC2 * MIN(SWDF,SWEXF) *
     &                (1. + 0.25 * (1. - MAX(SWFAC,0.40)))
C-----------------------------------------------------------------------
C-KJB  DO NOT WANT TO DECREASE ROOT DEPTH WITH STRESS.  IF PG TO ROOTS
C IS LOW BECAUSE OF SEED GROWTH OR IF WATER DEFICIT CAUSES LOW PG TO ROOTS
C DESPITE INCREASED PARTITIONING TO ROOTS, THEN RLV WILL NOT INCREASE
C SO THERE WILL BE NO EFFECTIVE INCREASE IN WATER EXTRACTION.  IDEALLY THE
C DECISION SHOULD BE BASED ON AMOUNT OF ROOT GROWTH VS NORMAL UNSTRESS.
C ACCELERATE FROM 1.0 TO 0.5, STAY FLAT, SHOULD DROP AGAIN, 0.5 TO 0.0
C AS THE OTHER FUNCTION ACTS.  NOTE:  SWFAC*2.0 WAS NOT USED IN ALL 40 CASES
C EXCEPT 1985-RAINFED WHERE DELETING INCREASED YIELD 2764 TO 2770 KG/HA.
C NOW ACCELERATING ROOT GROWTH BY ABOUT 12-13% AT SWFAC=0.50.  THIS
C HELPS IOWA 88 AND VEG STRESS TRTS IN 1981 AND 1985. INCR SEED AND BIO.
C-----------------------------------------------------------------------
            ENDIF
            IF (RTDEP .GT. DEPMAX) THEN
               RTDEP = DEPMAX
            ENDIF
          ENDIF
          RLDF(L) = RLDF(L) * (1. - (CUMDEP - RTDEP) / DLAYR(L))
          TRLDF = TRLDF + RLDF(L)
          GO TO 2900
        ENDIF
      ENDDO
C-----------------------------------------------------------------------
C     Calculate root senescence, growth, maintenance and growth
C     respiration, and update root length density for each layer.
!-----------------------------------------------------------------------
 2900 CONTINUE

      IF (SUMRL .GT. 0.0) THEN
         SATFAC = SUMEX/SUMRL
      ELSE
         SATFAC = 0.0
      ENDIF

      SRDOT = 0.0
      RLSENTOT = 0.0

      DO L = 1,L1
        IF (TRLDF .LT. 0.00001) THEN
          RRLF(L) = 1.0
        ELSE
          RRLF(L) = RLDF(L)/TRLDF
        ENDIF
!-----------------------------------------------------------------------
!       MRESPR, GRESPR, and RESPS are not used anywhere
!                       chp 9/22/98
!-----------------------------------------------------------------------
        MRESPR(L) = (RLV(L)/RFAC1*RO*DLAYR(L)*100.0
     &    +RRLF(L)*FRRT*PG*RP) * 44.0 / 30.0
        GRESPR(L) = RRLF(L) * (CGRRT-WRDOTN) * 44.0 /30.0
        RESPS(L) = MRESPR(L) + GRESPR(L)
!-----------------------------------------------------------------------
        RLGRW(L) = RRLF(L) * RLNEW / DLAYR(L) !cm[root]/cm3[ground]

        IF (TRLV + RLNEW > TRLV_MIN) THEN
          RLSEN(L) = RLV(L) * RTSEN * DTX
        ELSE
          RLSEN(L) = 0.0
        ENDIF

!       Limit total senescence in each layer to existing RLV
        IF (RLV(L) - RLSEN(L) - RLV_WS(L) + RLGRW(L) < 0.0) THEN
          RLSEN(L) = RLV(L) + RLGRW(L) - RLV_WS(L)
        ENDIF 

!       RLSENTOT is profile senescence, water stress and natural cm/cm2
        RLSENTOT = RLSENTOT + (RLSEN(L) + RLV_WS(L)) * DLAYR(L)
      ENDDO

!     If senescence too high (results in TRLV < TRLV_MIN) then
!       reduce senescence in each layer by factor.
      IF (RLSENTOT > 1.E-6 .AND. TRLV + RLNEW - RLSENTOT < TRLV_MIN)THEN
        FACTOR = (TRLV + RLNEW - TRLV_MIN) / RLSENTOT
        FACTOR = MAX(0.0, MIN(1.0, FACTOR))
        RLSEN  = RLSEN  * FACTOR
        RLV_WS = RLV_WS * FACTOR
      ENDIF

!     Update RLV and TRLV based on today's growth and senescence
      TRLV = 0.0
      DO L = 1, NLAYR
        RLV(L) = RLV(L) + RLGRW(L) - RLSEN(L) - RLV_WS(L)
        TRLV = TRLV + RLV(L) * DLAYR(L)

!       Keep senescence in each layer for adding C and N to soil
        !SENRT(L) = RLSEN(L) * DLAYR(L) / RFAC1 * 10000. * 10. !kg/ha
!       1/14/2005 CHP - water stress senesence needs to be inlcuded.
        SENRT(L) = (RLSEN(L) + RLV_WS(L)) * DLAYR(L) / RFAC3 * 1.E5 
!                   cm[root]              g[root]   1000 cm2   10(kg/ha)
!         kg/ha  =  -------- * cm[soil] * ------- * -------- * ---------
!                  cm3[soil]             cm[root]     m2         (g/m2)

        SENRT(L) = AMAX1(SENRT(L), 0.0)
        SRDOT = SRDOT + SENRT(L)/10.        !g/m2

!       Not used:
        !TRLGRW = TRLGRW + RLGRW(L) * DLAYR(L)
        !TRLSEN = TRLSEN + RLSEN(L) * DLAYR(L)
      ENDDO

!     11/13/2000 CHP Sum RLSEN for total root senescence today.  
!     SRDOT = TRLSEN / RFAC3 * 10000.     !g/m2

!     Total root senescence = water stress + natural senescence
!     10/3/2005 SJR
!      SRDOT = (TRTDY + RLNEW - TRLV) * 10000.0 / RFAC3    !g/m2
      SRDOT = AMAX1(SRDOT, 0.0)

      TotRootMass = TRLV / RFAC3 * 1.E5
!                   cm[root]   g[root]   10000 cm2   10(kg/ha)
!          kg/ha  = -------- * ------- * -------- * ---------
!                  cm2[soil]   cm[root]     m2         (g/m2)

      CumRootMass = CumRootMass + WRDOTN * 10. - SRDOT * 10. 
           
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE ROOTS
!=======================================================================


!=======================================================================
!  IPROOT Subroutine
!  Reads root parameters from input files.
!----------------------------------------------------------------------
!  REVISION HISTORY
!  09/13/1998 CHP Written
C  08/12/2003 CHP Added I/O error checking
!-----------------------------------------------------------------------
!  Called : ROOTS
!  Calls  : FIND, ERROR, IGNORE
C=======================================================================
      SUBROUTINE IPROOT(
     &  FILECC,                                           !Input
     &  PORMIN, RFAC1, RLDSM, RTDEPI, RTEXF,              !Output
     &  RTSEN, RTSDF, RTWTMIN, XRTFAC, YRTFAC)            !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
!     NL defined in ModuleDefs.for

      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, ERROR, IGNORE

      CHARACTER*6 ERRKEY
      PARAMETER (ERRKEY = 'ROOTS')

      CHARACTER*6 SECTION
      CHARACTER*80 CHAR
      CHARACTER*92 FILECC

      INTEGER LUNCRP, ERR, LNUM, ISECT, FOUND, II

      REAL RTDEPI, RLDSM, PORMIN
      REAL RFAC1, RTSEN, RTSDF, RTEXF
      REAL XRTFAC(4), YRTFAC(4)

!     Added 10/20/2005 for minimum root mass for senescence
      CHARACTER (len=7) RWMTXT
      REAL RTWTMIN

!-----------------------------------------------------------------------
!     ***** READ ROOT GROWTH PARAMETERS *****************
!-----------------------------------------------------------------------
!     Read in values from input file, which were previously input
!       in Subroutine IPCROP.
!-----------------------------------------------------------------------
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)

!-----------------------------------------------------------------------
!    Find and Read Photosynthesis Section
!-----------------------------------------------------------------------
      SECTION = '!*ROOT'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(5F6.0)',IOSTAT=ERR) RTDEPI,RFAC1,RTSEN,RLDSM,RTSDF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(8F6.0)',IOSTAT=ERR)(XRTFAC(II),YRTFAC(II),II = 1,4)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(12X,2F6.0)',IOSTAT=ERR) PORMIN, RTEXF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(F6.0,T45,A7)',IOSTAT=ERR) RTWTMIN, RWMTXT
        IF (ERR .NE. 0 .OR. RWMTXT .NE. 'RTWTMIN') THEN
          RTWTMIN = 0.0
        ENDIF
      ENDIF

      CLOSE (LUNCRP)

!***********************************************************************
      RETURN
      END SUBROUTINE IPROOT
!=======================================================================

C=======================================================================
C  INROOT Subroutine
C  Initializes root variables at emergence.
C----------------------------------------------------------------------
C  REVISION HISTORY
C  04/01/91 GH  Adapted for CROPGRO
C  06/17/98 CHP Modified for modular format
C  05/11/99 GH  Incorporated in CROPGRO
C-----------------------------------------------------------------------
C  Called : CROPGRO
C  Calls  : None
C=======================================================================
      SUBROUTINE INROOT(
     &  DLAYR, FRRT, NLAYR, PLTPOP, RFAC1, RTDEPI, WTNEW, !Input
     &  RLV, RTDEP)                                       !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
!     NL defined in ModuleDefs.for

      IMPLICIT NONE

      INTEGER L, NLAYR

      REAL DEP,RLINIT
      REAL RTDEP,RTDEPI,CUMDEP
      REAL RFAC1
      REAL WTNEW, FRRT, PLTPOP
      REAL RLV(NL), DLAYR(NL)

!***********************************************************************
C     INITIALIZE ROOT DEPTH AT EMERGENCE
C-----------------------------------------------------------------------
      RTDEP = RTDEPI
C-----------------------------------------------------------------------
C     DISTRIBUTE ROOT LENGTH EVENLY IN ALL LAYERS TO A DEPTH OF
C     RTDEPTI (ROOT DEPTH AT EMERGENCE)
C-----------------------------------------------------------------------
      CUMDEP = 0.

      DO L = 1,NLAYR
        RLV(L) = 0.0
      ENDDO

      DO L = 1,NLAYR
           DEP = MIN(RTDEP - CUMDEP, DLAYR(L))
           RLINIT = WTNEW * FRRT * PLTPOP * RFAC1 * DEP / ( RTDEP *
     &          10000 )
!        cm[root]   g[root]    plants  cm[root]   m2
!        -------- = -------- * ------ * ------- * ---
!      cm2[ground]   plant       m2     g[root]   cm2

           CUMDEP = CUMDEP + DEP
           RLV(L) = RLINIT / DLAYR(L)
           IF (CUMDEP .GE. RTDEP) GO TO 300
      ENDDO

  300 CONTINUE
!***********************************************************************
      RETURN
      END SUBROUTINE INROOT
!=======================================================================

!-----------------------------------------------------------------------
!       Variable definitions
!-----------------------------------------------------------------------
! AGRRT     Mass of CH2O required for new root growth (g[CH2O] / g[root])
! CGRRT     Carbon demand for new root growth (g[CH2O] / m2 / d)
! CROP      Crop identification code 
! CUMDEP    Cumulative depth of soil profile (cm)
! DEP       Cumulative soil depth (cm)
! DEPMAX    Maximum depth of reported soil layers (cm)
! DLAYR(L)  Soil thickness in layer L (cm)
! DS(L)     Cumulative depth in soil layer L (cm)
! DTX       Thermal time that occurs in a real day based on vegetative 
!             development temperature function (thermal days / day)
! DUL(L)    Volumetric soil water content at Drained Upper Limit in soil 
!             layer L (cm3 [H2O] /cm3 [soil])
! ESW(L)    Plant extractable soil water by layer (= DUL - LL) (cm3/cm3)
! FILECC    Path plus filename for species file (*.spe) 
! RFAC3     Ratio of root length to root weight at the current time (cm/g)
! FRRT      Fraction of vegetative tissue growth that goes to roots on a 
!             day (g[root] / g[veg])
! GRESPR(L) Growth respiration for new root growth in layer L 
! LL(L)     Volumetric soil water content in soil layer L at lower limit
!             ( cm3/cm3)
! LUNCRP    Logical unit number for FILEC (*.spe file) 
! LUNIO     Logical unit number for FILEIO 
! MRESPR(L) Maintenance respiration for new root growth in layer L 
! NL        Maximum number of soil layers = 20 
! NLAYR     Number of soil layers 
! PG        Daily gross photosynthesis (g[CH2O] / m2 / d)
! PLTPOP    Plant population (# plants / m2)
! PORMIN    Minimum pore space required for supplying oxygen to roots for 
!             optimal growth and function (cm3/cm3)
! RESPS(L)  Total respiration for new root growth in layer L 
! RFAC1     Root length per unit  root weight. (cm/g)
! RFAC2     Root depth increase rate with time (cm / physiol. day)
! RFAC3     Ratio of root length to root weight at the current time (cm/g)
! RLDF(L)   Combined weighting factor to determine root growth distribution
! RLDSM     Minimum root length density in a given layer, below which 
!             drought-induced senescence is not allowed.
!             (cm [root ]/ cm3 [soil])
! RLGRW(L)  Incremental root length density in soil layer L
!             (cm[root] / cm3[soil])
! RLINIT    Initial root density (cm[root]/cm2[ground])
! RLNEW     New root growth added (cm[root]/cm2[ground]/d)
! RLSEN(L)  Root length density senesced today (cm[root]/ cm3[soil])
! RLV(L)    Root length density for soil layer L (cm[root] / cm3[soil])
! RO        Respiration coefficient that depends on total plant mass
!             (g[CH2O] / g[tissue])
! RP        proportion of the day's photosynthesis which is respired in the 
!             maintenance process 
! RRLF(L)   Root length density factor ratio (RLDF(L) / TRLDF) 
! RTDEP     Root depth (cm)
! RTDEPI    Depth of roots on day of plant emergence. (cm)
! RTEXF     Fraction root death per day under oxygen depleted soil 
! RTSDF     Maximum fraction of root length senesced in a given layer per 
!             physiological day when water content in a given layer falls 
!             below 25 % of extractable soil water. 
! RTSEN     Fraction of existing root length which can be senesced per 
!             physiological day. (fraction / ptd)
! RTSURV(L) Fraction survival of roots on a given day, taking into account 
!             death due to excess or deficit water conditions 
! RTWT      Dry mass of root tissue, including C and N
!             (g[root] / m2[ground])
! SAT(L)    Volumetric soil water content in layer L at saturation
!             (cm3 [water] / cm3 [soil])
! SRDOT     Daily root senescence (g / m2 / d)
! SW(L)     Volumetric soil water content in layer L
!             (cm3 [water] / cm3 [soil])
! SWDF      Soil water deficit factor for layer with deepest roots (0-1) 
! SWEXF     Excess water stress factor for layer with deepest roots (0-1) 
! SWFAC     Effect of soil-water stress on photosynthesis, 1.0=no stress, 
!             0.0=max stress 
! TABEX     Function subroutine - Lookup utility 
! TRLDF     Total root length density factor for root depth (cm)
! TRLGRW    Total new root length density in soil layer L
!             (cm[root] / cm2[soil])
! TRLSEN    Total root length density senesced today (cm[root]/ cm2[soil])
! TRLV      Total root length per square cm soil today 
!             (cm[root]/cm2[soil])
! TRTDY     Total root length per square cm soil yesterday 
!             (cm[root]/cm2[soil])
! VSTAGE    Number of nodes on main stem of plant 
! WR(L)     Root hospitality factor, used to compute root growth 
! WRDOTN    Dry weight growth rate of new root tissue including N but not C 
!             reserves (g[root] / m2[ground]-d)
! WTNEW     Initial mass of seedling or seed (g / plant)
! XRTFAC(I) V-stage at which rate of increase in root depth per 
!             physiological day is YRTFAC(I). (# leaf nodes)
! YRTFAC(I) Rate of increase in root depth per degree day at V-stage 
!             XRTFAC(I). (cm / (physiol. day))
!***********************************************************************
!      END SUBROUTINES ROOTS, IPROOT, and INROOT
!=======================================================================
