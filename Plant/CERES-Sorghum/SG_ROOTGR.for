C=======================================================================
C  SG_ROOTGR, Subroutine
C
C  Determines root growth
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by                           E. Alocilja & B. Baer 9-88
C  3  Modified by                           T. Jou                4-89
C  4. Header revision and minor changes             P.W.W.      2-8-93
C  5. Added switch block, etc.                      P.W.W.      2-8-93
C  6. Simplified the RLNEW calculation and slowed
C     the growth of roots in deeper soils.   J.T.R. & B.D.B.   6-20-94
C  7. Converted to modular format                    W.D.B.    7-31-02
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  : RLDF,RNFAC,RLNEW,SWDF,TRLDF,RNLF,L,L1
C
C  OUTPUT : None
C-----------------------------------------------------------------------
C  Called : SG_CERES
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  L,L1   : Loop counter
C  RLDF() : A root length density factor for soil layer L used to calculate
C           new root growth distribution - unitless
C  RLNEW  : New root length to be added to the total root system length -
C           cm.  root per sq. cm. ground
C  RNFAC  : Zero to unity factor describing mineral N availability effect on
C           root growth in Layer L
C  RNLF   : Intermediate factor used to calculate distribution of new root
C           growth in the soil - unitless value between 0 and 1
C  SWDF   : Soil water deficit factor for Layer L used to calculate root
C           growth and water uptake - unitless value between 0 and 1
C  TRLDF  : An intermediate calculation used to calculate distribution of
C           new root growth in soil
C=======================================================================

      SUBROUTINE SG_ROOTGR (
     & CUMDEP, CUMDTT, DEPMAX, DLAYR, DTT,
     & ESW, GRORT, ISWNIT, LL, NH4, NLAYR, NO3, 
     & PLTPOP, PORMIN,RLV, RLWR, RTDEP, SAT,SHF, SW, SWFAC)
     
      USE MODULEDEFS
      IMPLICIT  NONE
      SAVE
C---------------------------------------------------------------------------
C  VARIABLES THAT WERE IN COMMON BLOCK
C---------------------------------------------------------------------------
      
      REAL CUMDEP
      REAL CUMDTT
      REAL DEPMAX
      REAL DLAYR(NL)
      REAL DTT
      REAL ESW(NL)
      REAL LL(NL)
      REAL NH4(NL)
      INTEGER NLAYR
      REAL NO3(NL)
      REAL PLTPOP
      REAL RLV(NL)
      REAL RLWR
      REAL RTDEP
      REAL SHF(NL)
      REAL SW(NL)
      REAL SWFAC
	REAL RTSURV
	REAL RTEXF
	REAL SWEXF
	REAL SAT(NL)
	REAL PORMIN
      

C--------------------------------------------------------------------
C   LOCAL VARIABLES
C--------------------------------------------------------------------
      CHARACTER ISWNIT*1
      REAL      RLDF(NL),RNFAC,RLNEW,SWDF,TRLDF,RNLF,GRORT
      INTEGER   L,L1




C--------------------------------------------------------------------
C            MAIN CODE
C--------------------------------------------------------------------
C
C     The small differences between root length/weight ratios used in earlier
C     models were insignificant considering the uncertainty of the value
C     and the uncertainty of loss of assimilate by exudation and respiration.
C     A compromise value of 0.98 was choosen for all crops.
C
C     RLNEW  = GRORT * 0.98 * PLTPOP     ! A compromise value -- JTR
      RLNEW  = GRORT * RLWR * PLTPOP     ! A compromise value -- JTR
      CUMDEP = 0.0
      RNFAC  = 1.0
      RLDF   = 0.0
      L      = 0

      DO WHILE ((CUMDEP .LT. RTDEP) .AND. (L .LT. NLAYR))
         L = L + 1
         CUMDEP = CUMDEP + DLAYR(L)
         IF (SW(L)-LL(L) .LT. 0.25*ESW(L)) THEN
            SWDF = 4.0*(SW(L)-LL(L))/ESW(L)
            IF (SWDF .LT. 0.0) THEN
               SWDF = 0.0
            ENDIF
          ELSE
            SWDF = 1.0
         ENDIF
C
C        Made all crops so that RNFAC is constrained between 0.01 and 1.0;
C        on page 94 of Jones & Kiniry book the minimum is 0.01. - WTB
C
         IF (ISWNIT .NE. 'N') THEN
            RNFAC = 1.0 - (1.17*EXP(-0.15*(NO3(L)+NH4(L))))
            RNFAC = AMAX1 (RNFAC,0.01)
         ENDIF

         RLDF(L) = AMIN1(SWDF,RNFAC)*SHF(L)*DLAYR (L)
      END DO

      L1 = L
C
C     The following changes were made to simplify the code and make the model
C     more generic. It also takes into account some newer data provided by
C     Julio Dardenelli of Argentina.  For the first time the ceres model
C     restricts the rate of downward movement of roots with the soil property
C     -- root weighting factor -- to account for greater difficulty in growing
C     downward in hard soil. Changes made by JTR 6/16/94.
C

c** wdb 10/22/03  
          RTEXF = 0.1
          SWEXF = 1.0
          IF (SAT(L)-SW(L) .LT. PORMIN) THEN
            SWEXF = (SAT(L) - SW(L)) / PORMIN
            SWEXF = MIN(SWEXF, 1.0)
          ENDIF

          RTSURV = MIN(1.0,(1.-RTEXF*(1.-SWEXF)))

c** wdb 10/22/03
      IF (CUMDTT .LT. 275.0) THEN                ! JTR 6/17/94
         RTDEP = RTDEP + DTT*0.1*SQRT(SHF(L)*AMIN1(SWFAC*2.0,SWDF))
       ELSE
         RTDEP = RTDEP + DTT*0.2*SQRT(SHF(L)*AMIN1(SWFAC*2.0,SWDF))
      ENDIF

      RTDEP    = AMIN1 (RTDEP,DEPMAX)                            
      RLDF(L1) = RLDF(L1)*(1.0-(CUMDEP-RTDEP)/DLAYR(L1))
      TRLDF    = 0.0

      DO  L = 1, L1
         TRLDF = TRLDF + RLDF(L)
      END DO
      IF (TRLDF .GE. RLNEW*0.00001 .AND. TRLDF .GT. 0.0) THEN
         RNLF = RLNEW/TRLDF
         DO L = 1, L1
            RLV(L) = RLV(L) + RLDF(L)*RNLF/DLAYR(L)-0.005*RLV(L)
	      RLV(L) = RLV(L) * RTSURV
            RLV(L) = AMAX1 (RLV(L),0.0)
            RLV(L) = AMIN1 (RLV(L),4.0)
         END DO
      ENDIF

      RETURN
      END SUBROUTINE SG_ROOTGR

