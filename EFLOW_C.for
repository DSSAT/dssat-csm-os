!***********************************************************************
!  EFLOW_C, subroutine for CENTURY-based SOM/residue module of DSSAT.
!
!  Purpose: This subroutine calculates the amount of E that flows
!           from pool A to pool B, and also calculates the E
!           immobilization or mineralization that goes with this flow.
!
!  Revision history:
!  ........ Parton et al.  Written for CENTURY model.
!  01/01/99 AJG Revised and linked to DSSAT.
!  01/01/00 AJG Integrated the CENTURY-based and CERES-based SOM
!               modules with CHP's modular structure.
!
!  Called: LITDEC_C, PARTIT_C, SOMDEC_C
!  Calls : --
!***********************************************************************

      SUBROUTINE EFLOW_C (
     &  AC, AE, CEB, CFAB, CO2FA,                         !Input
     &  EFAB, IMMOB, MINER)                               !Output

!     ------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
!     ------------------------------------------------------------------
      REAL AC, AE, CEB, CFAB, CO2FA, EFAB, EFCO2, IMMOB,
     &  MINER

!     Initialize to zero. 
      EFAB = 0.
      EFCO2 = 0.
      MINER = 0.
      IMMOB = 0.

!     Crash prevention against division by zero (in case AE or CFAB
!     equals zero. EFAB then becomes zero).
      IF (AE < 1.E-6 .OR. CFAB < 1.E-6 .OR. AC < 1.E-6 .OR.
     &  CEB < 1.E-6) RETURN

!     The amount of E flowing from pool A to pool B is proportional to
!     the C flow from pool A to B.
      EFAB = AE * (CFAB / AC)

!     E mineralization due to CO2 respiration that accompanies the C
!     flow from pool A to B. For pools that flow to two pools (eg. SOM1
!     flows to SOM2 and SOM3), do this only once; CO2FA is then set
!     to -99 when it is not needed.
      IF (CO2FA < -98.) THEN
        EFCO2 = 0.
      ELSE
        EFCO2 = AE * (CO2FA / AC)
      ENDIF

!     ******************************************************************
!     Though immobilization and mineralization could be summed up here,
!     they are kept separately, because that is needed for subroutine
!     IMMOBLIMIT. The mineralization affects what leaves pool A and what
!     enters into pool B, while the immobilization only affects B.
!     ******************************************************************
!     If the material flowing from pool A to pool B has a C/E ratio
!     that is wider than the C/E ratio of the material that is
!     allowed to enter pool B, then immobilization is needed.
      IF (CEB > 1.E-6) THEN
        IF (CFAB / EFAB > CEB) THEN
!         Calculate the amount of E immobilized, based on the
!         relationship CEB = CFAB / (EFAB + IMMOB).
          IMMOB = CFAB / CEB - EFAB

        ELSE
!         The amount of E that flows from pool A to pool B is enough to
!         satisfy the condition of the C/E ratio that is allowed to enter
!         pool B. The rest of the E coming from pool A is mineralized.
          MINER = EFAB - CFAB / CEB

!         Correct the E flow from pool A to pool B for the E
!         mineralization.
          EFAB = EFAB - MINER
        ENDIF

      ELSE
        IMMOB = 0.
        MINER = 0.
      ENDIF

!     Sum the E released due to CO2 respiration with the E release
!     related to the flow from pool A to B.
      MINER = MINER + EFCO2

!***********************************************************************
!***********************************************************************
!     END
!***********************************************************************

      RETURN
      END    !Subroutine EFLOW_C

!***********************************************************************
! EFLOW_C variables:
!
! AC          Amount of carbon in pool A, from where the flow originates
!               (kg[C] / ha)
! AE          Amount of carbon in pool B, the receiving pool (kg[C] / ha)
! CEB         C:E ratio (i.e., C:N or C:P) of the material allowed to enter
!               the receiving pool B (-)
! CFAB        C flow from pool A to pool B (kg[C] / ha)
! CO2FA       CO2 flow out of pool A, which is the pool from where the
!               C flow originates (kg[C] / ha)
! EFAB        E from pool A to B (kg[E] / ha)
! EFCO2       E mineralization due to CO2 respiration that accompanies the
!               C flow from pool A to B (kg[E] / ha)
! IMMOB         Immobilization of E  (kg[E] / ha)
! MINER         Mineralization of E  (kg[E] / ha)
!***********************************************************************
