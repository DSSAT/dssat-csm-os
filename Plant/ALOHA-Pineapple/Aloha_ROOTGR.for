!=======================================================================
!  Aloha_ROOTGR, Subroutine
!
!  Determines root growth
!-----------------------------------------------------------------------
!  Revision history
!
!  Written
!  09/1988 Modified by E. Alocilja & B. Baer 
!  04/1989 Modified by T. Jou  
!  02/08/1993 PWW Added switch block,  Header revision and minor changes
!  06/20/1994 JTR, BDB Simplified the RLNEW calculation and slowed
!                  the growth of roots in deeper soils.   
!  06/25/2017 CHP Adpated for CSM v4.6
!-----------------------------------------------------------------------
!                         DEFINITIONS
!
!  L,L1   : Loop counter
!  RLDF() : A root length density factor for soil layer L used to calculate
!           new root growth distribution - unitless
!  RLNEW  : New root length to be added to the total root system length -
!           cm.  root per sq. cm. ground
!  RNFAC  : Zero to unity factor describing mineral N availability effect on
!           root growth in Layer L
!  RNLF   : Intermediate factor used to calculate distribution of new root
!           growth in the soil - unitless value between 0 and 1
!  SWDF   : Soil water deficit factor for Layer L used to calculate root
!           growth and water uptake - unitless value between 0 and 1
!  TRLDF  : An intermediate calculation used to calculate distribution of
!           new root growth in soil
!=======================================================================

      SUBROUTINE Aloha_ROOTGR (CONTROL,
     &     CUMDTT, DTT, GRORT, ISTAGE, ISWITCH, NO3, NH4,     !Input
     &     SOILPROP, SW, SWFAC,                               !Input
     &     RLV, RTDEP, RTWT)                                  !Output

      USE Aloha_mod
      IMPLICIT  NONE
      SAVE

      INTENT(IN) :: CONTROL, 
     &     CUMDTT, DTT, GRORT, ISTAGE, ISWITCH, NO3, NH4,     !Input
     &     SOILPROP, SW, SWFAC                                !Input
      INTENT(OUT):: RLV, RTDEP, RTWT

      CHARACTER ISWWAT*1
      REAL      RNFAC,RLNEW,SWDF,TRLDF,RNLF,GRORT, RTWT, RTDEP, CUMDEP
      REAL  PLTPOP, DTT, CUMDTT, DEPFAC, SWFAC
      REAL, DIMENSION(NL) :: RLDF, RLV, DLAYR, ESW, SW, NO3, NH4
      INTEGER   L,L1, ISTAGE, DYNAMIC, NLAYR
      LOGICAL FIRST

      TYPE (ControlType) CONTROL
      TYPE (SoilType) SOILPROP
      TYPE (SwitchType) ISWITCH

      DLAYR = SOILPROP % DLAYR
      DYNAMIC = CONTROL % DYNAMIC

!=======================================================================
      SELECT CASE (DYNAMIC)
!=======================================================================
      CASE (RUNINIT, SEASINIT)
!=======================================================================
      RTWT   =  0.0
      RTDEP  = Planting % SDEPTH    ! Rooting depth = seeding depth (cm)

      PLTPOP = Planting % PLTPOP
      NLAYR  = SOILPROP % NLAYR

      FIRST = .TRUE.

      ISWWAT = ISWITCH % ISWWAT
      IF (ISWWAT .NE. 'Y') RETURN
      DO L = 1, NLAYR
        ESW(L) = SOILPROP % DUL(L) - SOILPROP % LL(L)
      ENDDO

!=======================================================================
      CASE (RATE)
!=======================================================================
!     from phenology
      SELECT CASE (ISTAGE)
        CASE (1)
          IF (ISWWAT .NE. 'Y') RETURN
          RTDEP  = RTDEP + 0.01*DTT     ! Depth of root (f) DTT
        CASE (9)
          RTDEP  = RTDEP + 0.01*DTT     ! Depth of root (f) DTT
      END SELECT

      IF (ISWWAT .NE. 'Y') RETURN
      IF (GRORT < 0.0001) RETURN
C
C     The small differences between root length/weight ratios used in earlier
C     models were insignificant considering the uncertainty of the value
C     and the uncertainty of loss of assimilate by exudation and respiration.
C     A compromise value of 0.98 was choosen for all crops.
C
!     Read in from species value
      RLNEW  = GRORT * Species % RLWR * PLTPOP

      CUMDEP = 0.0
      RNFAC  = 1.0
      SWDF = 1.0

      L = 0
      DO WHILE ((CUMDEP .LT. RTDEP) .AND. (L .LT. NLAYR))
        L = L + 1
        CUMDEP = CUMDEP + DLAYR(L)
        IF (ISWITCH % ISWWAT .NE. 'N') THEN
          IF (SW(L) - SOILPROP % LL(L) .LT. 0.25*ESW(L)) THEN
            SWDF = 4.0 * (SW(L) - SOILPROP % LL(L)) / ESW(L)
            IF (SWDF .LT. 0.0) THEN
              SWDF = 0.0
            ENDIF
          ELSE
            SWDF = 1.0
          ENDIF
C
C         Made all crops so that RNFAC is constrained between 0.01 and 1.0;
C         on page 94 of Jones & Kiniry book the minimum is 0.01. - WTB
C
          IF (ISWITCH % ISWNIT .NE. 'N') THEN
            RNFAC = 1.0 - (1.17*EXP(-0.15*(NO3(L)+NH4(L))))
            RNFAC = AMAX1 (RNFAC,0.01)
          ENDIF
        ENDIF
        RLDF(L) = AMIN1(SWDF,RNFAC) * SOILPROP % WR(L) * DLAYR(L)
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
      DEPFAC = SQRT(SOILPROP % WR(L) * AMIN1(SWFAC * 2.0, SWDF))
      IF (CUMDTT .LT. 275.0) THEN                       ! JTR 6/17/94
         RTDEP = RTDEP + DTT * 0.1 * DEPFAC
       ELSE
         RTDEP = RTDEP + DTT * 0.2 * DEPFAC
      ENDIF

      RTDEP    = AMIN1 (RTDEP, SOILPROP % DS(NLAYR))                            
      RLDF(L1) = RLDF(L1)*(1.0-(CUMDEP-RTDEP)/DLAYR(L1))
      TRLDF    = 0.0

      DO  L = 1, L1
         TRLDF = TRLDF+RLDF(L)
      END DO

      IF (TRLDF .GE. RLNEW*0.00001) THEN
         RNLF = RLNEW/TRLDF
         DO L = 1, L1
            RLV(L) = RLV(L) + RLDF(L)*RNLF/DLAYR(L)-0.005*RLV(L)
            RLV(L) = AMAX1 (RLV(L),0.0)
            RLV(L) = AMIN1 (RLV(L),4.0)
         END DO
      ENDIF

!=======================================================================
!     Integration
!-----------------------------------------------------------------------
      CASE (INTEGR)
!=======================================================================
      IF (ISWWAT .NE. 'Y') RETURN

!     Initialize RLV at stage 1
      IF (ISTAGE == 1 .AND. FIRST) THEN
        FIRST = .FALSE.
        CUMDEP = 0.0
        DO L = 1, NLAYR
           CUMDEP = CUMDEP + SOILPROP % DLAYR(L)
           RLV(L) = 0.20*PLTPOP/DLAYR (L)
           IF (CUMDEP .GT. RTDEP) EXIT
        END DO

        RLV(L) = RLV(L)*(1.0-(CUMDEP-RTDEP)/DLAYR(L))
        L1 = L + 1
        IF (L1 .LT. NLAYR) THEN
           DO L = L1, NLAYR
              RLV(L) = 0.0
           END DO
        ENDIF
      ENDIF
!=======================================================================
      END SELECT
!=======================================================================
      RETURN
      END SUBROUTINE Aloha_ROOTGR
!=======================================================================
!=======================================================================
