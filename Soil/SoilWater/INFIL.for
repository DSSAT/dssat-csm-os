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
C=======================================================================
      SUBROUTINE INFIL(
     &    DLAYR, DS, DUL, NLAYR, PINF, SAT, SW,           !Input
     &    SWCN, SWCON, MgmtWTD,                           !Input
     &    DRAIN, DRN, EXCS, SWDELTS)                      !Output

!     ------------------------------------------------------------------
      USE ModuleDefs

      IMPLICIT NONE
      SAVE

      INTEGER L, LK, NLAYR

      REAL DRAIN, DRCM, EXCS, HOLD, PINF, SWCON, TMPEXCS
      REAL MgmtWTD
      REAL DLAYR(NL), DRN(NL), DS(NL), DUL(NL), SAT(NL), SW(NL)
      REAL SWCN(NL), SWDELTS(NL), SWTEMP(NL)

!-----------------------------------------------------------------------
      DO L = 1, NLAYR
        DRN(L) = 0.0
        SWDELTS(L) = 0.0
        SWTEMP(L) = SW(L)
      ENDDO
      EXCS    = 0.0
      TMPEXCS = 0.0

      DO L = 1,NLAYR
        HOLD = (SAT(L) - SWTEMP(L)) * DLAYR(L)
        IF (PINF .GT. 1.E-4 .AND. PINF .GT. HOLD) THEN

! 11/30/2006 JTR/CHP reduce SWCON in top layer to allow for
!     increased evaporation for wet soils

          IF (DS(L) > MgmtWTD) THEN
            DRN(L) = 0.0
            DRCM = 0.0
          ELSE
            IF (L == 1) THEN                                        !JTR
              DRCM = 0.9 * SWCON * (SAT(L) - DUL(L)) * DLAYR(L)     !JTR
            ELSE                                                    !JTR
              DRCM = SWCON * (SAT(L) - DUL(L)) * DLAYR(L)
            ENDIF                                                   !JTR
        
            DRN(L) = PINF - HOLD + DRCM
        
!           Failed experiment -- too many problems with zero SWCN and 
!             historic soil profiles
!           IF (SWCN(L) .GE. 0.0 .AND. DRN(L) .GT. SWCN(L)*24.0) THEN
            IF (SWCN(L) .GT. 0.0 .AND. DRN(L) .GT. SWCN(L)*24.0) THEN
              DRN(L) = SWCN(L) * 24.0
              DRCM = DRN(L) + HOLD - PINF
            ENDIF
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
          IF (SWTEMP(L) .GE. DUL(L) + 0.003 .AND.
     &          DS(L) < MgmtWTD) THEN

! 11/30/2006 JTR/CHP reduce SWCON in top layer to allow for
!     increased evaporation for wet soils

            IF (L == 1) THEN                                        !JTR
              DRCM = 0.9 * (SWTEMP(L) - DUL(L)) * SWCON * DLAYR(L)  !JTR
            ELSE                                                    !JTR
              DRCM = (SWTEMP(L) - DUL(L)) * SWCON * DLAYR(L)
            ENDIF                                                   !JTR

            DRN(L) = DRCM

!           IF (SWCN(L) .GE. 0.0 .AND. DRN(L) .GT. SWCN(L)*24.0) THEN
            IF (SWCN(L) .GT. 0.0 .AND. DRN(L) .GT. SWCN(L)*24.0) THEN
               DRN(L) = SWCN(L) * 24.0
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

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE INFIL
!-----------------------------------------------------------------------
!     INFIL VARIABLE DEFINITIONS: (updated 2/12/2004)
!-----------------------------------------------------------------------
! DLAYR(L)   Thickness of soil layer L (cm)
! DRAIN      Drainage rate from soil profile (mm/d)
! DRCM       Drainage rate from soil profile (cm/d)
! DRN(L)     Drainage rate through soil layer L (cm/d)
! DUL(L)     Volumetric soil water content at Drained Upper Limit in soil 
!              layer L (cm3[water]/cm3[soil])
! EXCS       Excess water to be added to runoff (cm/d)
! HOLD       Amount of water a soil layer will hold above it's present 
!               level, used to calculate downward flow; also, temporary 
!               variable/ intermediate calculation (cm)
! MgmtWTD    Managed depth to water table (cm)
! NLAYR      Actual number of soil layers 
! PINF       Potential water available for infiltration (cm)
! SAT(L)     Volumetric soil water content in layer L at saturation
!               (cm3 [water] / cm3 [soil])
! SW(L)      Volumetric soil water content in layer L
!               (cm3 [water] / cm3 [soil])
! SWCN(L)    Saturated hydraulic conductivity in layer L (cm/hr)
! SWCON      Soil water conductivity constant; whole profile drainage rate 
!               coefficient (1/d)
! SWDELTS(L) Change in soil water content due to drainage in layer L
!             (cm3 [water] / cm3 [soil])
! SWTEMP(L)  Soil water content in layer L (temporary value to be modified 
!               based on drainage, root uptake and upward flow through soil 
!               layers). (cm3/cm3)
!-----------------------------------------------------------------------
!     END SUBROUTINE INFIL
!-----------------------------------------------------------------------
