C=======================================================================
C  INFIL, Subroutine, J.T. Ritchie
C  Calculates infiltration rate.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1989 JR  Written
C  01/11/1987 GH  Infiltration distr. according to capacity of each horz.
C  12/05/1993 NBP Made into subroutine
C  06/13/1997 WDB Added tile drainage
!  10/13/1997 CHP Modified for modular format
!  01/05/1999 CHP Removed tile drainage
!  10/29/2001 CHP Removed read stmts for soil info.  These variables
!                 now come from soil module.
!  08/12/2003 CHP Reset EXCS to zero daily.
!  06/29/2011 CHP Suleiman-Ritchie changes to daily drainage from paper:
!     Suleiman, A.A., J.T. Ritchie. 2004. Modifications to the DSSAT vertical 
!       drainage model for more accurate soil water dynamics estimation. 
!       Soil Science 169(11):745-757.
!  06/29/2011 CHP Combine INFIL and SATFLO - no need for two separate routines.
!-----------------------------------------------------------------------
!  Called by: WATBAL
!  Calls:     None
!-----------------------------------------------------------------------
C  Notes  : Infiltration is distributed according to the holding
C           capacity of each horizon (SAT(L)-DUL(L)).  If the soil
C           water content of a layer reaches saturation, the excess
C           amount of infiltration is added to the next soil layer(s),
C           depending on their water holding capacity and actual soil
C           water content.  Then saturated flow is calculated.

!     Ksat = cm/hr;  SWCN = cm/d
C=======================================================================
      SUBROUTINE INFIL(
     &    DYNAMIC, DLAYR, DUL, KSAT, NLAYR, PINF, SAT,    !Input
     &    SW, SWCON,                                      !Input
     &    DRAIN, DRN, EXCS, SWDELTS)                      !Output

!     ------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE
      SAVE

      INTEGER DYNAMIC, L, LK, NLAYR

      REAL DRAIN, DRCM, EXCS, HOLD, PINF, SWCON, TMPEXCS
      REAL DLAYR(NL), DRN(NL), DUL(NL), SAT(NL), SW(NL)
      REAL KSAT(NL), SWDELTS(NL), SWTEMP(NL)
      REAL DrainC(NL), F

!***********************************************************************
!***********************************************************************
!     Initialization
!***********************************************************************
      IF (DYNAMIC .EQ. INIT) THEN
!-----------------------------------------------------------------------
      DO L = 1, NLAYR
!       Eqns from Suleiman & Ritchie, 2004
        DrainC(L) = 3.*DUL(L)**2. - 2.6*DUL(L) + 0.85  !Eqn. 23
      ENDDO

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
      DO L = 1, NLAYR
        DRN(L) = 0.0
        SWDELTS(L) = 0.0
        SWTEMP(L) = SW(L)
      ENDDO
      EXCS    = 0.0
      TMPEXCS = 0.0

      DO L = 1,NLAYR
        HOLD = (SAT(L) - SWTEMP(L)) * DLAYR(L)      !cm

!       Equation 25, PINF and KSAT both in cm/d
!       F modifies drainage every day, each layer based on current drainage from above
        F = MAX(0.0, 1.0 - (LOG(PINF + 1.0) / LOG(KSAT(L) + 1.0)))
        SWCON = F * DrainC(L)

        IF (PINF .GT. 1.E-4 .AND. PINF .GT. HOLD) THEN

! 11/30/2006 JTR/CHP reduce SWCON in top layer to allow for
!     increased evaporation for wet soils
          IF (L == 1) THEN                                        !JTR
            DRCM = 0.9 * SWCON * (SAT(L) - DUL(L)) * DLAYR(L)     !JTR
          ELSE                                                    !JTR
            DRCM = SWCON * (SAT(L) - DUL(L)) * DLAYR(L)
          ENDIF                                                   !JTR

          DRN(L) = PINF - HOLD + DRCM

!         Failed experiment -- too many problems with zero KSAT and 
!           historic soil profiles
!         IF (KSAT(L) .GE. 0.0 .AND. DRN(L) .GT. KSAT(L)*24.0) THEN
          IF (KSAT(L) .GT. 0.0 .AND. DRN(L) .GT. KSAT(L)*24.0) THEN
            DRN(L) = KSAT(L) * 24.0
            DRCM = DRN(L) + HOLD - PINF
          ENDIF

          SWTEMP(L) = SWTEMP(L) + (PINF - DRN(L)) / DLAYR(L)
          IF (SWTEMP(L) .GT. SAT(L)) THEN
            TMPEXCS = (SWTEMP(L) - SAT(L)) * DLAYR(L)     ! US FEB 2004
            SWTEMP(L) = SAT(L)
            IF (L .EQ. 1 .AND. TMPEXCS .GT. 0.0) THEN
               EXCS = EXCS + TMPEXCS     
       !Allow for ponding of water on surface and flooding with 
       !  presence of bund -- US Feb04
            ENDIF 
C           If there is excess water, redistribute it in layers above.
            IF (L .GT. 1) THEN
                 
              DO LK = L-1,1,-1
                IF (TMPEXCS .LT. 0.0001) GOTO 760
                HOLD = MIN((SAT(LK) - SWTEMP(LK)) * DLAYR(LK),TMPEXCS)
                SWTEMP(LK) = SWTEMP(LK) + HOLD / DLAYR(LK)
                DRN(LK) = MAX((DRN(LK) - TMPEXCS), 0.0)
                TMPEXCS = TMPEXCS - HOLD
                IF (LK. EQ. 1 .AND. TMPEXCS .GT. 0.0) THEN
                   EXCS = EXCS + TMPEXCS
                ENDIF   
              ENDDO

            ENDIF

  760       CONTINUE

          ENDIF
          PINF = DRN(L)

        ELSE
          SWTEMP(L) = SWTEMP(L) + PINF / DLAYR(L)
          IF (SWTEMP(L) .GE. DUL(L) + 0.003) THEN

! 11/30/2006 JTR/CHP reduce SWCON in top layer to allow for
!     increased evaporation for wet soils

            IF (L == 1) THEN                                        !JTR
              DRCM = 0.9 * (SWTEMP(L) - DUL(L)) * SWCON * DLAYR(L)  !JTR
            ELSE                                                    !JTR
              DRCM = (SWTEMP(L) - DUL(L)) * SWCON * DLAYR(L)
            ENDIF                                                   !JTR

            DRN(L) = DRCM

!           IF (KSAT(L) .GE. 0.0 .AND. DRN(L) .GT. KSAT(L)*24.0) THEN
            IF (KSAT(L) .GT. 0.0 .AND. DRN(L) .GT. KSAT(L)*24.0) THEN
               DRN(L) = KSAT(L) * 24.0
               DRCM = DRN(L)
            ENDIF

            SWTEMP(L) = SWTEMP(L) - DRCM / DLAYR(L)
            PINF = DRCM

          ELSE
            PINF = 0.0
            DRN(L) = 0.0
          ENDIF
        ENDIF
      ENDDO

      DRAIN = PINF * 10.0
!-----------------------------------------------------------------------
      DO L = 1, NLAYR
          SWDELTS(L) = SWTEMP(L) - SW(L)
      ENDDO

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE INFIL
!-----------------------------------------------------------------------
!     INFIL VARIABLE DEFINITIONS: (updated 2/12/2004)
!-----------------------------------------------------------------------
! DLAYR(L)   Thickness of soil layer L (cm)
! DRAIN       Drainage rate from soil profile (mm/d)
! DRCM        Drainage rate from soil profile (cm/d)
! DRN(L)      Drainage rate through soil layer L (cm/d)
! DUL(L)      Volumetric soil water content at Drained Upper Limit in soil 
!              layer L (cm3[water]/cm3[soil])
! EXCS       Excess water to be added to runoff (cm/d)
! HOLD        Amount of water a soil layer will hold above it's present 
!               level, used to calculate downward flow; also, temporary 
!               variable/ intermediate calculation (cm)
! NLAYR       Actual number of soil layers 
! PINF       Potential water available for infiltration (cm)
! SAT(L)      Volumetric soil water content in layer L at saturation
!               (cm3 [water] / cm3 [soil])
! SW(L)       Volumetric soil water content in layer L
!               (cm3 [water] / cm3 [soil])
! KSAT(L)     Saturated hydraulic conductivity in layer L (cm/hr)
! SWCON       Soil water conductivity constant; whole profile drainage rate 
!               coefficient (1/d)
! SWDELTS(L) Change in soil water content due to drainage in layer L
!             (cm3 [water] / cm3 [soil])
! SWTEMP(L)   Soil water content in layer L (temporary value to be modified 
!               based on drainage, root uptake and upward flow through soil 
!               layers). (cm3/cm3)
!-----------------------------------------------------------------------
!     END SUBROUTINE INFIL
!-----------------------------------------------------------------------
