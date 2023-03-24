C=======================================================================
C  SATFLO, Subroutine, J.T. Ritchie
C  Calculates saturated flow on days with no rain or irrigation.
!-----------------------------------------------------------------------
C  Notes  : Saturated flow is calculated for days with no irr or rain.
C           Drainage is reduced when the flux exceeds the rate allowed
C           by the saturated soil hydraulic conductivity, assuming unit
C           gradient.  This allows for perched water tables in profile.
C           Prevents flux from exceeding the most limiting layer below
C           it.  If the sat. hyd. cond values are missing (neg) assume
C           no perching of water table.
!-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1989 JR  Written
C  11/01/1987 GH
C  12/05/1993 NBP Made into subroutine
C  07/02/1997 WDB ADDED TILE DRAIN FLOW
!  10/13/1997 CHP Modified for modular format.
!  10/29/2001 CHP Removed read stmts for soil info.  These variables
!                 now come from soil module.
!-----------------------------------------------------------------------
!  Called by: WATBAL
!  Calls:     None
C=======================================================================

      SUBROUTINE SATFLO(
     &    DLAYR, DUL, NLAYR, SAT, SW, SWCN, SWCON,        !Input
     &    DRAIN, DRN, SWDELTS)                            !Output

!     2023-02-07 CHP removed unused variables from argument list:
!     DS, MgmtWTD
!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
!     NL defined in ModuleDefs.for

      IMPLICIT NONE

      INTEGER L, NLAYR
      REAL DRAIN, HOLD, SWCON, SWOLD !, MgmtWTD
      REAL DLAYR(NL), DRMX(NL), DRN(NL), DUL(NL), SAT(NL), !DS(NL), 
     &        SW(NL), SWCN(NL), SWDELTS(NL), SWTEMP(NL)

!-----------------------------------------------------------------------
      DO L = 1, NLAYR
        DRN(L) = 0.0
        SWDELTS(L) = 0.0
        SWTEMP(L) = SW(L)
      ENDDO
!-----------------------------------------------------------------------

      DO L = 1,NLAYR
        DRN(L) = 0.0
        DRMX(L) = 0.0

!        IF (DS(L) > MgmtWTD) THEN
!          EXIT
!        ENDIF

        IF (SWTEMP(L) .GE. (DUL(L) + 0.003)) THEN
          DRMX(L) = (SWTEMP(L) - DUL(L)) * SWCON * DLAYR(L)
          DRMX(L) = MAX(0.0, DRMX(L))
        ENDIF

        IF (L .EQ. 1) THEN
          DRN(L) = DRMX(L)
        ELSE
          HOLD = 0.0
          IF (SWTEMP(L) .LT. DUL(L)) 
     &        HOLD = (DUL(L) - SWTEMP(L)) * DLAYR(L)
          DRN(L) = MAX(DRN(L-1) + DRMX(L) - HOLD,0.0)
        ENDIF

!       Failed experiment -- too many problems with zero SWCN and 
!         historic soil profiles
!       IF (SWCN(L) .GE. 0.0 .AND. DRN(L) .GT. SWCN(L)*24.0)
        IF (SWCN(L) .GT. 0.0 .AND. DRN(L) .GT. SWCN(L)*24.0)
     &    DRN(L) = SWCN(L) * 24.0

!        IF (DRN(L) .GT. 0.0) IDRSW = 1
      ENDDO

C     Compute volumetric water contents after drainage in a day.
C     Prevent water content in any layer from exceeding saturation
C     as water drains down in the profile.

      DO L = NLAYR, 2, -1
        SWOLD = SWTEMP(L)
        SWTEMP(L) = SWTEMP(L) + (DRN(L-1) - DRN(L)) / DLAYR(L)
        IF (SWTEMP(L) .GT. SAT(L)) THEN
          DRN(L-1) = MAX(0.0,(SAT(L) - SWOLD) * DLAYR(L) + DRN(L))
          SWTEMP(L) = SAT(L)
        ENDIF
      ENDDO

      SWTEMP(1) = SWTEMP(1) - DRN(1) / DLAYR(1)
      DRAIN = DRN(NLAYR) * 10.0
!-----------------------------------------------------------------------
!     Calculate flux in soil water content due to infiltration and
!         saturated flow.
!-----------------------------------------------------------------------
      DO L = 1, NLAYR
        SWDELTS(L) = SWTEMP(L) - SW(L)
      ENDDO

      RETURN
      END SUBROUTINE SATFLO

!-----------------------------------------------------------------------
!     SATFLO VARIABLE DEFINITIONS:  updated 2/19/2004
!-----------------------------------------------------------------------
! DLAYR(L)   Thickness of soil layer L (cm)
! DRAIN      Drainage rate from soil profile (mm/d)
! DRMX(L)    Calculated maximum saturated flow from layer L (cm/d)
! DRN(L)     Drainage rate through soil layer L (cm/d)
! DUL(L)     Volumetric soil water content at Drained Upper Limit in soil 
!              layer L (cm3[water]/cm3[soil])
! HOLD       Amount of water a soil layer will hold above it's present 
!              level, used to calculate downward flow; also, temporary 
!              variable/ intermediate calculation (cm)
! NLAYR      Actual number of soil layers 
! SAT(L)     Volumetric soil water content in layer L at saturation
!             (cm3 [water] / cm3 [soil])
! SW(L)      Volumetric soil water content in layer L
!             (cm3 [water] / cm3 [soil])
! SWCN(L)    Saturated hydraulic conductivity in layer L (cm/hr)
! SWCON      Soil water conductivity constant; whole profile drainage rate 
!              coefficient (1/d)
! SWDELTS(L) Change in soil water content due to drainage in layer L
!             (cm3 [water] / cm3 [soil])
! SWOLD      Previous soil water content prior to capillary flow from layer
!             (cm3/cm3)
! SWTEMP(L)  Soil water content in layer L (temporary value to be modified 
!              based on drainage, root uptake and upward flow through soil 
!              layers). (cm3/cm3)
!----------------------------------------------------------------------
!     END SUBROUTINE SATFLO
!=======================================================================



