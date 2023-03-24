!***********************************************************************
!     TRANSPIRATION MODULE - File TRANS.for
!***********************************************************************
!     Includes subroutines:
!         TRANS - Calculates actual transpiration rate.
!         TRATIO - Calculates relative transpiration rate.
!         BLRRES - Computes boundary layer resistance.
!***********************************************************************
C=======================================================================
C  TRANS, Subroutine, J.T. Ritchie
C  Calculates potential transpiration rate, EOP (mm/d).
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1989 JR  Written
C  01/01/1989 JWJ Modified for climate change using ETRATIO subroutine.
C  12/05/1993 NBP Made into subroutine and changed to TRATIO function.
C  10/13/1997 CHP Modified for modular format.
C  11/25/1997 CHP Put in file TRANS.for w/ TRATIO and BLRRES
C  09/01/1999 GH  Incorporated into CROPGRO
C  01/13/2000 NBP Added DYNAMIC contruct to input KCAN and calc. FDINT
C  02/06/2003 KJB/CHP Replaced KCAN with KEP
!  06/19/2003 JWJ/CHP Replaced KEP with KTRANS
!  10/31/2011 CHP Use EVAP instead of ES (can include flood and mulch evap)
!  01/18/2018 KRT Added KCB and REFET for ASCE dual Kc ET method
!-----------------------------------------------------------------------
!  Called by: WATBAL
!  Calls:     None
C=======================================================================
      SUBROUTINE TRANS_old(DYNAMIC, 
     &    CO2, CROP, EO, EVAP, KTRANS, TAVG,              !Input
     &    WINDSP, XHLAI,                                  !Input
     &    EOP)                                            !Output

!-----------------------------------------------------------------------
      USE ModuleDefs
      USE ModuleData
      IMPLICIT NONE
      EXTERNAL TRATIO

      CHARACTER*2  CROP

      INTEGER DYNAMIC

      REAL CO2, EO, EVAP, FDINT, KTRANS, TAVG, WINDSP, XHLAI
      REAL EOP, TRAT, EOP_reduc, EOP_max
      REAL KCB, REFET

!     FUNCTION SUBROUTINES:
      REAL TRATIO
      
      CALL GET('SPAM', 'KCB', KCB)
      CALL GET('SPAM', 'REFET', REFET)

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
        EOP = 0.0

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
        EOP = 0.0
        TRAT = TRATIO(CROP, CO2, TAVG, WINDSP, XHLAI)
!-----------------------------------------------------------------------
C       Estimate light interception.  NOTE 01/15/03 We don't want PAR
C       Estimate ENERGY interception.  NOTE 01/15/03 We don't want PAR
C       interception.  Changed to reflect energy interception which
C       correctly considers both PAR and the infrared.  Villalobos
C       measured 0.52 for Transpiration component.  Theory of Goudriaan
C       supports combined interception coefficient of 0.5 for PAR and NIR

C       01/15/03 - Work of Sau et al, shows that a K of 0.5 was better in
C       all cases, for PT form as well as the Dynamic form for predicting
C       soil water balance and predicting measured ET.

        IF (KCB .GE. 0.0) THEN
          EOP = KCB * REFET !KRT added for ASCE dual Kc ET approach
        ELSE  
          FDINT = 1.0 - EXP(-(KTRANS) * XHLAI)  
          EOP = EO * FDINT
          EOP_reduc = EOP * (1. - TRAT)  
          EOP = EOP * TRAT

C         01/15/03 KJB  I think the change to "Same" K for EOS and EOP
C         may cause next function to be less driving, but below still
C         will depend on whether actual soil evapo (EVAP) meets EOS

!         IF ((EOP + EVAP) .GT. (EO * TRAT)) EOP = EO * TRAT - EVAP

!         Need to limit EOP to no more than EO (reduced by TRAT effect on EOP) 
!         minus actual evaporation from soil, mulch and flood
          EOP_max = EO - EOP_reduc - EVAP
          EOP = MIN(EOP, EOP_max)
        ENDIF  

        EOP = MAX(EOP,0.0)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE TRANS_old

!-----------------------------------------------------------------------
!     TRANS VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! CO2     Atmospheric carbon dioxide concentration (ppm)
! EO      Potential evapotranspiration rate (mm/d)
! EOP     Potential plant transpiration  (mm/d)
! EVAP    Actual soil + mulch + flood evaporation rate (mm/d)
! FDINT   Fraction of direct solar radiation captured by canopy
! KCAN    Canopy light extinction coefficient for daily PAR, for
!           equidistant plant spacing, modified when in-row and between row
!           spacings are not equal
! KCB     Basal crop coefficient for ASCE dual Kc ET method
! LNUM    Current line number of input file
! REFET   ASCE Standardized Reference Evapotranspiration (alfalfa or grass)
! TAVG    Average daily temperature (°C)
! TRAT    Relative transpiration rate for CO2 values other than 330 ppm
! TRATIO  Function subroutine which calculates relative transpiration rate.
!
! WINDSP  Wind speed (km/d)
! XHLAI   Leaf area index (m2[leaf] / m2[ground])
!-----------------------------------------------------------------------
!     END SUBROUTINE TRANS
!-----------------------------------------------------------------------


! XDEN
! XHLAI    Leaf area index (m2[leaf] / m2[ground])
! XNUM
!-----------------------------------------------------------------------
!     END FUNCTION TRATIO
!-----------------------------------------------------------------------

!C=======================================================================
!C  BLRRES, Subroutine, J.W. Jones
!C  Computes boundary layer resistance (Jagtap and Jones, 1990).
!C-----------------------------------------------------------------------
!C  REVISION HISTORY
!C  ??/??/?? JWJ Written
!!  10/17/97 CHP Modified for modular format.
!!-----------------------------------------------------------------------
!!  Called by: TRATIO
!!  Calls:     None
!C=======================================================================
!      SUBROUTINE BLRRES(
!     &    CHIGHT, UAVG,                   !Input
!     &    RS1, RS2, RC1, RC2)             !Output
!
!      IMPLICIT NONE
!!-----------------------------------------------------------------------
!      REAL CHIGHT, UAVG
!      REAL RC1, RC2, RS1, RS2
!      REAL D, E1, E2, E3, E4, E5, E6, EN, X, Z0, Z1
!
!!-----------------------------------------------------------------------
!      EN = 3.0
!      Z1 = 0.01
!      X = 2.0
!      D = 0.7 * CHIGHT**0.979
!      Z0 = 0.13 * CHIGHT**0.997
!
!      E1 = EXP(EN*(1. - (D + Z0) / CHIGHT))
!      E2 = EXP(EN)
!      E3 = CHIGHT/(EN*(CHIGHT-D))
!      E4 = ALOG((X - D)/Z0)
!      E5 = 0.4 * 0.4 * UAVG
!      E6 = ALOG((X - D) / (CHIGHT - D))
!
!      RS2 = E4 * E3 * (E2 - E1) / E5
!      RC2 = E4 * (E6 + E3 * (E1 - 1.)) / E5
!      RS1 = ALOG(X/Z1) * ALOG((D + Z0)/Z1) / E5
!      RC1 = (ALOG(X/Z1)**2) / E5
!      RC1 = RC1 - RS1
!
!      RETURN
!      END SUBROUTINE BLRRES
!
!!-----------------------------------------------------------------------
!!     BLRRES VARIABLE DEFINITIONS:
!!-----------------------------------------------------------------------
!! CHIGHT   Reference height for crop (m)
!! D
!! E1, E2, E3, E4, E5, E6
!! EN
!! RC1, RC2
!! RS1, RS2
!! UAVG     Average wind speed (m/s)
!! X
!! Z0
!! Z1
!!-----------------------------------------------------------------------
!!     END SUBROUTINE BLRRES
!!-----------------------------------------------------------------------
!***********************************************************************
!     END OF TRANSPIRATION MODULE
!***********************************************************************
