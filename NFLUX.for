!***********************************************************************
!  NFLUX, subroutine
!
!  Purpose: Determines upward and downward movement of nitrogen with
!           the water flow.
!
!  Revision history:
!  ....     ...  Written
!  02/10/1993 PWW  Minor changes and header revision.
!  04/02/1995 WTB  Added adsorption coefficient (ADCOEF) for retarded NO3
!                movement
!  10/18/1995 GH   Modified for CERES and CROPGRO
!  03/28/2000 AJG/WTB  Completely restructured and added explanatory text.
!                Removed IFLAG, KPFLG, SPPM. Renamed:
!                OLD       NEW
!                ---       ---
!                ISFLAG    NSOURCE
!                MU        NLAYR
!                NISFAC    FRAC_SOLN
!                SKGN      NORIG
!                OUTN      NDOWN_SAT, NDOWN_UNSAT, NUP
!  11/07/2003 CHP Added tile drain N loss.
!  11/21/2017 HJ  Modified tile drain N loss.

!  Called: NTRANS_inorg
!  Calls : --
!***********************************************************************

      SUBROUTINE NFLUX (
     &  ADCOEF, BD, DLAYR, DRN, DUL, UPFLOW, NLAYR,       !Input
     &  NORIG, NSOURCE, SW, TDFC, TDLNO,                  !Input
     &  DLTN, CLeach, TLeachD, CNTILEDR, NTILEDR)         !Output !HJ

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
!     NL defined in ModuleDefs.for
      IMPLICIT  NONE
      SAVE
!     ------------------------------------------------------------------

      INTEGER L, NLAYR, NSOURCE

      REAL REDFACT, NORIG_AVAIL, NTEMP, CLeach, TLeachD

      REAL ADCOEF(NL), BD(NL), DLAYR(NL), DLTN(NL), DRN(NL),
     &  DUL(NL), NDOWN(NL), NDOWN_SAT(NL), NDOWN_UNSAT(NL),
     &  NORIG(NL), NUP(NL), UPFLOW(NL), FRAC_SOLN(NL), SW(NL)

!CHP - Tile drain variables
      INTEGER TDLNO
      REAL NTILEDR, CNTILEDR, TDFC           !HJ added CNTILEDR

!     ------------------------------------------------------------------
      DO L = 1, NLAYR
!       Daily initialization of the N flow.
        NUP(L) = 0. 
        NDOWN_SAT(L) = 0.
        NDOWN_UNSAT(L) = 0.
!       NTILEDR = 0.  !HJ modified and initialized in SOILNI.for (L.820)

!       FRAC_SOLN is the fraction of NO3 in a layer that is in solution
!       and can move with the water flux. ADCOEF is the anion adsorption
!       coefficient of the layer (0.0 = no adsorption, hence no 
!       retarded movement of NO3.
        IF (NSOURCE .EQ. 1) THEN   !Urea
          FRAC_SOLN(L) = 1.
        ELSE                       !NO3
          FRAC_SOLN(L) = 1. / (1. + BD(L) * ADCOEF(L) / DUL (L)) 
        ENDIF

!       ---------------------------------------------------
!       Nutrient flow due to downward saturated water flow.
!       ---------------------------------------------------
!       To allow N movement across several soil layers within one time
!       step after a big rain event, a temporary integration of the
!       incoming flow from above is done for the saturated flow, so that
!       the updated state variable applies for the flow out of this
!       layer. This does not apply to the top layer.
        IF (L .EQ. 1) THEN
          NTEMP = NORIG(L)
        ELSE 
          NTEMP = NORIG(L) + NDOWN_SAT(L-1)
        ENDIF

!       Downward N flow out of this layer (though downward water
!       flow is negative, DRN is always positive!).
        IF ((SW(L) * DLAYR(L) + DRN(L)) .GT. 0.001) THEN
          NDOWN_SAT(L) = AMAX1 (NTEMP * FRAC_SOLN(L), 0.) *
     &      DRN(L) / (SW(L) * DLAYR(L) + DRN(L))
!         kjb/chp note: why the DRN in the denominator?
        ELSE
          NDOWN_SAT(L) = 0.0
        ENDIF

!       ----------------------------------------------------------------
!       Nutrient flow due to unsaturated downward or upward water flow.
!       ----------------------------------------------------------------
!       Downward unsaturated N flow out of this layer. Take the ABS to
!       get a positive value of NDOWN, because UPFLOW is negative (thus
!       take only the size, not the direction); in the calculation of
!       DLTN it will be given a sign.
        IF (UPFLOW(L) .LT. 0.) THEN
          NDOWN_UNSAT(L) = AMAX1 (NORIG(L) * FRAC_SOLN(L), 0.) *
     &      ABS (UPFLOW(L)) / (SW(L) * DLAYR(L) + ABS (UPFLOW(L)))

!       Upward N flow out of this layer (does not apply to layer 1).
        ELSEIF (UPFLOW(L) .GT. 0 .AND. L .GT. 1) THEN
          NUP(L) = AMAX1 (NORIG(L) * FRAC_SOLN(L), 0.) *
     &      UPFLOW(L) / (SW(L) * DLAYR(L) + UPFLOW(L))
        ENDIF   !End of IF block on UPFLOW.

!       Combine the saturated and unsaturated downward N flow.
        NDOWN(L) = NDOWN_SAT(L) + NDOWN_UNSAT(L)


        IF (L .NE. TDLNO) THEN
!       Limit the N flow out of the layer to the available amount,
!       corrected for what was already removed by other processes (thus
!       take only negative DLTN values).
        NORIG_AVAIL = (NORIG(L) + AMIN1 (DLTN(L),0.))
        IF (NDOWN(L) + NUP(L) .GT. NORIG_AVAIL .AND. 
     &      NDOWN(L) + NUP(L) .GT. 1.E-6) THEN
          REDFACT = NORIG_AVAIL / (NDOWN(L) + NUP(L))
          NDOWN(L) = NDOWN(L) * REDFACT
          NUP(L) = NUP(L) * REDFACT
        ENDIF

        ELSE
!       If this is tile drain layer, then calculate N loss to drain.
          NTILEDR = AMAX1(NORIG(L) * FRAC_SOLN(L), 0.) * 
     &        TDFC / (SW(L) * DLAYR(L) + TDFC)

!         Limit the N flow out to the available amount, including tile
!         drainage.
          NORIG_AVAIL = (NORIG(L) + AMIN1 (DLTN(L),0.))
          IF (NDOWN(L) + NUP(L) + NTILEDR .GT. NORIG_AVAIL) THEN
            REDFACT = NORIG_AVAIL / (NDOWN(L) + NUP(L) + NTILEDR)
            NDOWN(L) = NDOWN(L) * REDFACT
            NUP(L) = NUP(L) * REDFACT
            NTILEDR = NTILEDR * REDFACT
          ENDIF
        ENDIF

      END DO   !End of soil layer loop.

!     Accumulate the N lost by leaching below profile depth.
      CLeach  = CLeach  + NDOWN(NLAYR)    !Season cumulative
      TLeachD = TLeachD + NDOWN(NLAYR)    !Today's leached N
      CNTILEDR = CNTILEDR + NTILEDR !HJ N loss to TD, season cumulative
!     ------------------------------------------------------------------
!     DLTxxx section.
!     ------------------------------------------------------------------
!     Set the DLTN rate variable.
      DO L = 1, NLAYR
        IF (L .EQ. 1) THEN   !There is no NUP(1).
          DLTN(L) = DLTN(L) - NDOWN(L) + NUP(L+1) 

        ELSEIF (L .EQ. NLAYR) THEN   !There is no NUP(NLAYR+1).
          DLTN(L) = DLTN(L) - NDOWN(L) - NUP(L) + NDOWN(L-1)

        ELSE   !Other layers.
          DLTN(L) = DLTN(L) - NDOWN(L) - NUP(L) + NDOWN(L-1) + NUP(L+1)

        ENDIF   !End of IF block on layers.

        IF (L .EQ. TDLNO) THEN
          DLTN(L) = DLTN(L) - NTILEDR
        ENDIF

      END DO   !End of layer loop.

      RETURN
      END SUBROUTINE NFLUX

!==========================================================================
! NFLUX Variable List
!==========================================================================
! ADCOEF(L)      Anion adsorption coefficient for soil layer L;  for 
!                  reduced anion (nitrate) flow in variable-charge soils 
!                  (ADCOEF = 0 implies no anion retention)
!                  (cm3 (H2O] / g [soil])
! BD(L)          Bulk density, soil layer L (g [soil] / cm3 [soil])
! DLAYR(L)       Thickness of soil layer L (cm)
! DLTN(L)        Rate of change of N in soil layer L (kg [N] / ha)
! DRN(L)         Drainage rate through soil layer L (cm/d)
! DUL(L)         Volumetric soil water content at Drained Upper Limit in 
!                  soil layer L (cm3[water]/cm3[soil])
! UPFLOW(L)        Volume of water moving from soil layer L by unsaturated 
!                  flow: + = upward, -- = downward (cm/d)
! FRAC_SOLN(L)   Fraction of NO3 in a layer that is in solution and can 
!                  move with the water flux 
! NDOWN(L)       N moving downward in soil layer L (kg [N] / ha)
! NDOWN_SAT(L)   N moving downward with saturated flow in soil layer L
!                  (kg [N] / ha)
! NDOWN_UNSAT(L) N moving downward with unsaturated flow in soil layer L
!                  (kg [N] / ha)
! NL             Maximum number of soil layers = 20 
! NLAYR          Actual number of soil layers 
! NORIG(L)       N content in soil layer L before drainage (Urea or NO3)
!                  (kg [N] / ha)
! NORIG_AVAIL    N available for leaching (kg [N] / ha)
! NSOURCE        Flag for N source (1 = urea, 2 = NO3) 
! NTEMP          N variable for temporary integration step (kg [N] / ha)
! NUP(L)         N moving upward in soil layer L (kg [N] / ha)
! REDFACT        Reduction factor for FOM decomposition and for N leaching 
! SW(L)          Volumetric soil water content in layer L
!                  (cm3 [water] / cm3 [soil])
! TDFC           Flow through tile drain (cm / d)
! TDLNO          Layer number containing tile drain
! CLeach         Total N leached from soil (kg [N] / ha)
! CNTILEDR       Total N loss to tile drain (kg [N] / ha)
!==========================================================================
