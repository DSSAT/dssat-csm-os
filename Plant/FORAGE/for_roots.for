C=======================================================================
C  FOR_ROOTS, Subroutine, G. Hoogenboom, J.W. Jones
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
!-----------------------------------------------------------------------
!  Called by  :  PLANT
!  Calls      :  FOR_IPROOT, FOR_INROOT
!=======================================================================

      SUBROUTINE FOR_ROOTS(DYNAMIC,
     &    AGRRT, CROP, DLAYR, DS, DTX, DUL, FILECC,       !Input
     &    FILEIO, FRRT, ISWWAT, LL, NLAYR, PG,            !Input
     &    RLSEN, RO, RP, RTWT, SAT, SW,                   !Input
     &    SWFAC, VSTAGE, WR, WRDOTN, WTNEW,               !Input
     &    CUMDEP, RLV, RTDEP, SATFAC, SENRT,              !Output
     &    SRCADDOT, SRNADDOT)                             !Output

!     2023-01-20 CHP Removed unused variables in argument list
!     CADRT, NADRT, SRMDOT, SRDOT, SRNDOT, 

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
        ! which contain control information, soil
        ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL FOR_IPROOT, TABEX, FOR_INROOT
      SAVE

      CHARACTER*1 PLME
      CHARACTER*1 ISWWAT
      CHARACTER*2 CROP
      CHARACTER*30 FILEIO
      CHARACTER*92 FILECC

      INTEGER L, L1, NLAYR

      INTEGER DYNAMIC

      REAL CUMDEP, DEPMAX, DTX, FRRT,
     &  PG, RFAC1, RFAC2, RFAC3,
     &  RLDSM, RLNEW, RO, RP,
     &  RTDEP, RTSDF, RTSEN, RTWT, SWDF, SWFAC, !SRDOT, 
     &  TRLDF, TRLV, TRTDY, WRDOTN
        REAL CGRRT, AGRRT
      REAL SRCADDOT, SRNADDOT  !CADRT, NADRT, 
      REAL SWEXF, PORMIN, RTEXF, RTSURV
      REAL RTDEPI, SUMEX, SUMRL, SATFAC
      REAL PLTPOP, WTNEW, VSTAGE
      REAL TABEX              !Function subroutine located in UTILS.FOR
      REAL XRTFAC(4), YRTFAC(4)
      REAL DLAYR(NL), DS(NL), DUL(NL), ESW(NL), LL(NL), RLDF(NL)
      REAL RLGRW(NL), RLSEN(NL), RLV(NL), RRLF(NL)
      REAL SW(NL), SAT(NL), WR(NL)
      REAL GRESPR(NL), MRESPR(NL), RESPS(NL)
!       REAL YNRLT(NL), YNRL(5,NL)
      REAL RLNSEN(NL), TRLNSEN, TRLSEN, TRLGRW, SENRT(NL)  !RNDOT(NL), 
!     REAL SRMDOT, SRNDOT

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
      CALL FOR_IPROOT(
     &  FILECC, FILEIO,                                   !Input
     &  PLME, PLTPOP, PORMIN, RFAC1, RLDSM, RTDEPI,       !Output
     &  RTEXF, RTSEN, RTSDF, XRTFAC, YRTFAC)              !Output

      DEPMAX = DS(NLAYR)

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!      SRDOT = 0.0    Already initialized in for_senmob.for PDA
!      SRNDOT = 0.0   Already initialized in for_senmob.for PDA
      RLV   = 0.0
      RTDEP = 0.0       
      SENRT = 0.0
      SUMEX = 0.0
      SUMRL = 0.0
      SRCADDOT = 0.0
      SRNADDOT = 0.0
      SATFAC = 0.0
!-----------------------------------------------------------------------
C     ROOT DEPTH INCREASE RATE WITH TIME, cm/physiological day
C-----------------------------------------------------------------------
      IF (CROP .NE. 'FA' .AND. ISWWAT .NE. 'N') THEN
        RFAC2 = TABEX(YRTFAC,XRTFAC,0.0,4)
      ENDIF

!***********************************************************************
!***********************************************************************
!     EMERGENCE CALCULATIONS - Performed once per season upon emergence
!         or transplanting of plants
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. EMERG) THEN
!-----------------------------------------------------------------------
!       Call FOR_INROOT for initialization of root variables on
!       day of emergence.  (GROW emergence initialization
!       must preceed call to FOR_INROOT.)
!-----------------------------------------------------------------------
      CALL FOR_INROOT(
     &  DLAYR, FRRT, NLAYR, PLME, PLTPOP, RFAC1,                 !Input
     &  RLDSM, RTDEPI, RTWT, WTNEW,                               !Input
     &  RLV, RTDEP)                                              !Output

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
      TRTDY = 0.0
      DO L = 1,NLAYR
        TRTDY = TRTDY + RLV(L) * DLAYR(L)
        RRLF(L)   = 0.0
!        RLSEN(L)  = 0.0
        RLGRW(L)  = 0.0
        MRESPR(L) = 0.0
        GRESPR(L) = 0.0
        RESPS(L)  = 0.0
!        RNDOT(L) = 0.0
!        RLNSEN(L) = 0.0

      ENDDO

C-----------------------------------------------------------------------
C     Move calculation of "yesterday's" RFAC here so have RFAC3 to use 
C in calculating SENWT(L) - had been using RFAC1.
C-----------------------------------------------------------------------

      IF (RTWT - WRDOTN .GE. 0.0001) THEN
!       RFAC3 = TRTDY * 10000.0 / (RTWT - WRDOTN)
!       RTWT has not yet been updated today, so use yesterday's
!       value and don't subtract out today's growth - chp 11/13/00
        RFAC3 = TRTDY * 10000.0 / RTWT
      ELSE
      RFAC3 = RFAC1
      ENDIF

!-----------------------------------------------------------------------
!      SRNDOT = 0.0 Already calculated in for_senmob.for
      TRLDF  = 0.0
      CUMDEP = 0.0
      SUMEX = 0.0
      SUMRL = 0.0

      DO L = 1,NLAYR
        L1 = L
        CUMDEP = CUMDEP + DLAYR(L)
        SWDF = 1.0
        SWEXF = 1.0



C-----------------------------------------------------------------------
C      2/21/05 - SJR - move conditional call for water stress from CROPGRO 
C      to FOR_ROOTS.  Allows root senescence when Water dynamics option is 
C      turned off.  Water stress options set to no stress levels.  This 
C      also allows output of root growth dynamics without limimiting 
C      water or N uptake. Moved calculation of SUMEX and SUMRL after 
C      calculation of SWDF so that both excess soil water stress (SWEXF) 
C      and water deficit stress (SWDF) could be included in a single 
C      conditional clause.
C-----------------------------------------------------------------------
      
      IF (ISWWAT .EQ. 'Y') THEN

        IF (SAT(L)-SW(L) .LT. PORMIN) THEN
        SWEXF = (SAT(L) - SW(L)) / PORMIN
        SWEXF = MIN(SWEXF, 1.0)
        ENDIF

!        SUMEX  = SUMEX + DLAYR(L)*RLV(L)*(1.0 - SWEXF)
        SUMEX  = SUMEX + DLAYR(L)*(RLV(L) - RLSEN(L))*(1.0 - SWEXF)
!        SUMRL  = SUMRL + DLAYR(L)*RLV(L)
        SUMRL  = SUMRL + DLAYR(L)*(RLV(L) - RLSEN(L))

!     Need to calculate ESW where used. CHP 10/15/01
        ESW(L) = DUL(L) - LL(L)
        IF (SW(L) - LL(L) .LT. 0.25*ESW(L)) THEN
        SWDF = (SW(L) - LL(L)) / (0.25*ESW(L))
        SWDF = MAX(SWDF, 0.0)
        ENDIF
      ENDIF

C-----------------------------------------------------------------------

        RTSURV = MIN(1.0,(1.-RTSDF*(1.-SWDF)),(1.-RTEXF*(1.-SWEXF)))
C-----------------------------------------------------------------------
C      10/3/05 SJR
C      Calculate losses to water-stress senescence but do not update RLV.
C      Instead update at end of routine after calculating natural 
C      senescence and maintenance respiration.
C      6/21/06 SJR Moved water-stress senescence to SENMOB along with 
C      all other types of senescence
C-----------------------------------------------------------------------
!        IF (RLV(L) .GT. RLDSM) THEN
!        IF ((RLV(L) - RLSEN(L)) .GT. RLDSM) THEN
!            RNDOT(L) = RLV(L) * (1 - RTSURV)
!            RNDOT(L) = (RLV(L) - RLSEN(L)) * (1 - RTSURV)
!          RLV(L) = RLV(L) * RTSURV
!        ENDIF

        RLDF(L) = WR(L) * DLAYR(L) * MIN(SWDF,SWEXF)
        IF (CUMDEP .LT. RTDEP) THEN
        TRLDF = TRLDF + RLDF(L)
        ELSE
        IF (WR(L) .GT. 0.0 .AND. RLNEW .GT. 0.0) THEN
        IF (L .EQ. 1) THEN
        RTDEP = RTDEP + DTX * RFAC2
        ELSE
        RTDEP = RTDEP + DTX * RFAC2 * MIN(SWDF,SWEXF) *
     &    (1. + 0.25 * (1. - MAX(SWFAC,0.40)))
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

      TRLV = 0.0
      TRLSEN = 0.0
      TRLNSEN = 0.0
      TRLGRW = 0.0

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
!        MRESPR(L) = (RLV(L)/RFAC1*RO*DLAYR(L)*100.0
        MRESPR(L) = ((RLV(L) - RLSEN(L))/RFAC1*RO*DLAYR(L)*100.0
     &    +RRLF(L)*FRRT*PG*RP) * 44.0 / 30.0
        GRESPR(L) = RRLF(L) * (CGRRT-WRDOTN) * 44.0 /30.0
        RESPS(L) = MRESPR(L) + GRESPR(L)
!-----------------------------------------------------------------------
        RLGRW(L) = RRLF(L) * RLNEW / DLAYR(L)
C-----------------------------------------------------------------------
C      10/3/05 SJR Track water-stress senescence (RLNSEN, TRLNSEN, SRNDOT)
C       separately from natural senescence so each can be lost at
C        proper N concentration.
C      10/4/05 SJR moved calculation of RFAC 3 to top of integration step
C      and replaced RFAC1 in calculation of SENRT with RFAC3.
C-----------------------------------------------------------------------

!        RLNSEN(L) = RLV(L) * RNDOT(L)
!        RLNSEN(L) = RNDOT(L)
!        RLSEN(L) = RLV(L) * RTSEN * DTX
        SENRT(L) = RLSEN(L) * DLAYR(L) / RFAC3 * 10000. * 10. +
     &    RLNSEN(L) * DLAYR(L) / RFAC3 * 10000. * 10. !kg/ha
        RLV(L) = RLV(L) - RLNSEN(L) - RLSEN(L) + RLGRW(L)

        TRLGRW = TRLGRW + RLGRW(L) * DLAYR(L)
        TRLNSEN = TRLNSEN + RLNSEN(L) * DLAYR(L)
        TRLSEN = TRLSEN + RLSEN(L) * DLAYR(L)
        TRLV = TRLV + RLV(L) * DLAYR(L)
      ENDDO


!     SRDOT = (TRTDY + RLNEW - TRLV) * 10000.0 / RFAC3
!     Sum RLSEN for total root senescence today. chp 11/13/00  
!      SRNDOT = TRLNSEN / RFAC3 * 10000. !g/m2
!      SRDOT = SRMDOT + SRNDOT     
C-----------------------------------------------------------------------
C     7/27/05 SJR Calculate today's NADRT lost to senescence.
C     8/2/05 SJR SRDOT is lost at minimum composition/concentration
C                        so all CADRT and NADRT are left in the N & CH2O pools
C      10/04/05 SJR - Required  only for N and C lost with roots senesced
C                        due to water stress. 
C      6/21/06 SJR No longer required after moving senescence to SENMOB
C-----------------------------------------------------------------------
!      SRCADDOT = CADRT * MIN(1.0,(SRNDOT) / (RTWT - SRMDOT))
!      SRNADDOT = NADRT * MIN(1.0,(SRNDOT) / (RTWT - SRMDOT))
!      SRDOT = SRDOT + SRNADDOT / 0.16 + SRCADDOT
      
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END !SUBROUTINE FOR_ROOTS
!=======================================================================


!=======================================================================
!  FOR_IPROOT Subroutine
!  Reads root parameters from input files.
!----------------------------------------------------------------------
!  REVISION HISTORY
!  09/13/98 CHP wrote
!-----------------------------------------------------------------------
!  Called : FOR_ROOTS
!  Calls  : FIND, ERROR, IGNORE
C=======================================================================
      SUBROUTINE FOR_IPROOT(
     &  FILECC, FILEIO,                                   !Input
     &  PLME, PLTPOP, PORMIN, RFAC1, RLDSM, RTDEPI,       !Output
     &  RTEXF, RTSEN, RTSDF, XRTFAC, YRTFAC)              !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
        ! which contain control information, soil
        ! parameters, hourly weather data.
!     NL defined in ModuleDefs.for

      IMPLICIT NONE
      EXTERNAL GETLUN, ERROR, FIND, IGNORE

      CHARACTER*1 PLME
      CHARACTER*6 ERRKEY
      PARAMETER (ERRKEY = 'ROOTS')

      CHARACTER*6 SECTION
      CHARACTER*80 CHAR
      CHARACTER*30 FILEIO
      CHARACTER*92 FILECC

      INTEGER LUNCRP, LUNIO, ERR, LNUM, ISECT, FOUND, II

      REAL PLTPOP, RTDEPI, RLDSM, PORMIN
      REAL RFAC1, RTSEN, RTSDF, RTEXF
      REAL XRTFAC(4), YRTFAC(4)


!-----------------------------------------------------------------------
      CALL GETLUN('FILEIO', LUNIO)
      OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)

!-----------------------------------------------------------------------
!    Read Planting Details Section
!-----------------------------------------------------------------------
        SECTION = '*PLANT'
        CALL FIND(LUNIO, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILEIO, LNUM)
        ELSE
        READ(LUNIO,'(24X,F6.1,5X,A1)')
     &    PLTPOP, PLME
        ENDIF
  
      CLOSE (LUNIO)

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
      LNUM = 1
      SECTION = '!*ROOT'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(5F6.0)') RTDEPI, RFAC1, RTSEN, RLDSM, RTSDF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(8F6.0)',IOSTAT=ERR)(XRTFAC(II),YRTFAC(II),II = 1,4)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(12X,2F6.0)',IOSTAT=ERR) PORMIN, RTEXF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF

      CLOSE (LUNCRP)

!***********************************************************************
      RETURN
      END !SUBROUTINE FOR_IPROOT
!=======================================================================

C=======================================================================
C  FOR_INROOT Subroutine
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
      SUBROUTINE FOR_INROOT(
     &  DLAYR, FRRT, NLAYR, PLME, PLTPOP, RFAC1,                 !Input
     &  RLDSM, RTDEPI, RTWT, WTNEW,                              !Input
     &  RLV, RTDEP)                                              !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
        ! which contain control information, soil
        ! parameters, hourly weather data.
!     NL defined in ModuleDefs.for

      IMPLICIT NONE

      CHARACTER*1 PLME
      INTEGER L, NLAYR

      REAL DEP,RLINIT
      REAL RTDEP,RTDEPI,CUMDEP
        REAL RFAC1
      REAL WTNEW, FRRT, PLTPOP
        REAL RLV(NL), DLAYR(NL), RLCAP(NL)
      REAL RLDSM, RLRATIO, RTWT, TRLCAP

!***********************************************************************
C     INITIALIZE ROOT DEPTH AT EMERGENCE
C-----------------------------------------------------------------------
      CUMDEP = 0.
      TRLCAP = 0.0

      DO L = 1,NLAYR
        RLV(L) = 0.0
        RLCAP(L) = 0.0
      ENDDO

      
      IF (PLME .EQ. 'T') THEN

      RLINIT = RTWT / 10000 * RFAC1

        DO L=1, NLAYR
        CUMDEP = CUMDEP + DLAYR(L)
        RLCAP(L) = RLDSM * DLAYR(L)
        TRLCAP = TRLCAP + RLCAP(L)
        ENDDO

        IF (RLINIT .LE. TRLCAP) THEN
        RLRATIO = 1.0
        RTDEP = RLINIT / RLDSM
        ELSE
        RLRATIO = RLINIT / TRLCAP
        RTDEP = CUMDEP
        ENDIF

        DO L=1,NLAYR
        IF (RLINIT .GE. RLCAP(L)) THEN
        RLV(L) = RLCAP(L) * RLRATIO /DLAYR(L)
        RLINIT = RLINIT - (RLCAP(L) * RLRATIO)
        ELSE
        RLV(L) = RLINIT / DLAYR(L)
        RLINIT = 0.0
        ENDIF
        ENDDO
      ELSE


      RTDEP = RTDEPI
C-----------------------------------------------------------------------
C     DISTRIBUTE ROOT LENGTH EVENLY IN ALL LAYERS TO A DEPTH OF
C     RTDEPTI (ROOT DEPTH AT EMERGENCE)
C-----------------------------------------------------------------------

      DO L = 1,NLAYR
        DEP = MIN(RTDEP - CUMDEP, DLAYR(L))
        RLINIT = WTNEW * FRRT * PLTPOP * RFAC1 * DEP / ( RTDEP *
     &    10000 )
        CUMDEP = CUMDEP + DEP
        RLV(L) = RLINIT / DLAYR(L)
        IF (CUMDEP .GE. RTDEP) GO TO 300
      ENDDO

300   CONTINUE
      ENDIF
!***********************************************************************
      RETURN
      END !SUBROUTINE FOR_INROOT
!=======================================================================

!-----------------------------------------------------------------------
!       Variable definitions
!-----------------------------------------------------------------------
! AGRRT     Mass of CH2O required for new root growth (g[CH2O] / g[root])
! CADRT        CH2O added to root CH2O reserves (g[CH2O] / m2 / d)
! CGRRT     Carbon demand for new root growth (g[CH2O] / m2 / d)
! CROP      Crop identification code 
! CUMDEP    Cumulative depth of soil profile (cm)
! DEP       Cumulative soil depth (cm)
! DEPMAX    Maximum depth of reported soil layers (cm)
! DLAYR(L)  Soil Depth in layer L (cm)
! DS(L)     Cumulative depth in soil layer L (cm)
! DTX       Thermal time that occurs in a real day based on vegetative 
!             development temperature function (thermal days / day)
! DUL(L)    Volumetric soil water content at Drained Upper Limit in soil 
!             layer L (cm3 [H2O] /cm3 [soil])
! DYNAMIC   Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
!             INTEGR, OUTPUT, or SEASEND 
! ESW(L)    Plant extractable soil water by layer (= DUL - LL) (cm3/cm3)
! FILECC    Path plus filename for species file (*.spe) 
! FRRT      Fraction of vegetative tissue growth that goes to roots on a 
!             day (g[root] / g[veg])
! GRESPR(L) Growth respiration for new root growth in layer L 
! LL(L)     Volumetric soil water content in soil layer L at lower limit
!             ( cm3/cm3)
! LUNCRP    Logical unit number for FILEC (*.spe file) 
! LUNIO     Logical unit number for FILEIO 
! MRESPR(L) Maintenance respiration for new root growth in layer L 
! NADRT        N added to root N reserves (g[N] / m2 / d)
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
! RLCAP(L)  Root Length capacity of soil layer L (cm [root] / cm2[soil])
! RLDF(L)   Combined weighting factor to determine root growth distribution
! RLDSM     Minimum root length density in a given layer, below which 
!             drought-induced senescence is not allowed.
!             (cm [root ]/ cm3 [soil])
! RLGRW(L)  Incremental root length density in soil layer L
!             (cm[root] / cm3[soil])
! RLINIT    Initial root density (cm[root]/cm2[ground])
! RLNEW     New root growth added (cm[root]/cm2[ground]/d)
! RLRATIO   For transplants - ratio of initial root length (at RFAC1) to total 
!                  soil profile root length capacity
! RLSEN(L)  Root length density senesced today (cm[root]/ cm3[soil])
! RLNSEN(L) Root length density senesced today due to water-stress (cm[root]/ cm3[soil])
! RLV(L)    Root length density for soil layer L (cm[root] / cm3[soil])
! RNDOT(L)  Proportion of RLV lost in layer L to water-stress (cm[root]/ cm3[soil])
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
! SRCADDOT  Today's CADRT lost with senescing root tissue (g [CH2O]/m2/d)
! SRDOT     Daily root senescence (g / m2 / d)
! SRMDOT    Daily root senescence due to natural senescence that is lost at PRORTF, 
!              hence, some fraction of the N content is subject to mobilization (g/m2/day)
! SRNADDOT  Today's NADRT lost with senescing root tissue (g [N]/m2/d)
! SRNDOT    Daily root senescence that is lost to water -stress (lost at current N%) 
! SW(L)     Volumetric soil water content in layer L
!             (cm3 [water] / cm3 [soil])
! SWDF      Soil water deficit factor for layer with deepest roots (0-1) 
! SWEXF     Excess water stress factor for layer with deepest roots (0-1) 
! SWFAC     Effect of soil-water stress on photosynthesis, 1.0=no stress, 
!             0.0=max stress 
! TABEX     Function subroutine - Lookup utility 
! TRLCAP        Root Length capacity of entire soil profile to CUMDEP (cm [root] / cm2[soil])
! TRLDF     Total root length density factor for root depth (cm)
! TRLGRW    Total new root length density in soil layer L
!             (cm[root] / cm2[soil])
! TRLSEN    Total root length density senesced today (cm[root]/ cm2[soil])
! TRLNSEN   Total root length density senesced today due to water-stress (cm[root]/ cm2[soil])
! TRLV      Total root length per square cm soil today 
!             (cm[root]/cm2[soil])
! TRTDY     Total root length per square cm soil yesterday 
!             (cm[root]/cm2[soil])
! VSTAGE    Number of nodes on main stem of plant 
! WR(L)     Root hospitality factor, used to computer root water uptake 
! WRDOTN    Dry weight growth rate of new root tissue including N but not C 
!             reserves (g[root] / m2[ground]-d)
! WTNEW     Initial mass of seedling or seed (g / plant)
! XRTFAC(I) V-stage at which rate of increase in root depth per 
!             physiological day is YRTFAC(I). (# leaf nodes)
! YRTFAC(I) Rate of increase in root depth per degree day at V-stage 
!             XRTFAC(I). (cm / (physiol. day))
!***********************************************************************
!      END SUBROUTINES FOR_ROOTS, FOR_IPROOT, and FOR_INROOT
!=======================================================================
