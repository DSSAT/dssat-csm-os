!***********************************************************************
!     SOIL EVAPORATION MODULE - File SOILEV.for
!***********************************************************************
!     Includes subroutines:
!         SOILEV - Calculates actual soil evaporation (ES, mm/d).
!         ESUP   - Supplementary calculations for SOILEV.
!         PSE    - Calculates potential soil evaporation from total PET
!                   and LAI.
!***********************************************************************

C=======================================================================
C  SOILEV, Subroutine, J. Ritchie, B. Baer
C  Calculates actual soil evaporation (ES, mm/d).
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1989 JR  Written
C  08/15/1993 BB  Revised into IF-THEN-ELSE structure
C  11/30/1993 NBP Revised header and I/O order, removed ES1 from arg. list
C  12/10/1993 NBP Changes to SW in UPFLOW, returns ES with SW unchanged.
C               Added variable AWEV1 to simplify limit on ES.
C  08/20/1995 GH  Removed stage 1 limitations
C  07/12/1996 GH  Rechecked; there are still some concept problems
C  10/13/1997 CHP Modified for modular format.
C  09/01/1999 GH  Incorporated into CROPGRO
C  03/30/2000 CHP Keep original value of WINF for export to soil N module
!-----------------------------------------------------------------------
!  Called by: SPAM
!  Calls:     ESUP
C=======================================================================
      SUBROUTINE SOILEV(DYNAMIC,
     &    DLAYR, DUL, EOS, LL, SW, SW_AVAIL, U, WINF,     !Input
     &    ES)                                             !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types,
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      USE ModuleData
      IMPLICIT NONE
      EXTERNAL ESUP
      SAVE

      !CHARACTER*6 ERRKEY
      !PARAMETER (ERRKEY = 'SOILEV')

      INTEGER DYNAMIC

      REAL EOS, SWEF, U, SW_AVAIL, SWMIN
      REAL SUMES1, SUMES2, WINF, WINFMOD
      REAL ES, T
      REAL AWEV1, ESX, SWR, USOIL
      REAL DLAYR(NL), DUL(NL), LL(NL), SW(NL)
      REAL PMFRACTION

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
C  Calculate initial soil water content with respect to evaporation sums
!     From INSOIL
C-----------------------------------------------------------------------
          SWR = MAX(0.0,(SW(1) - LL(1)) / (DUL(1) - LL(1)))
          USOIL = (DUL(1) - SW(1)) * DLAYR(1) * 10.

          IF (SWR .GE. 1.0) THEN
              SUMES1 = 0.
              SUMES2 = 0.
              T = 0.
          ELSE IF (USOIL .LE. U) THEN
              SUMES2 = 0.
              T = 0.
              SUMES1 = USOIL
          ELSE
              SUMES2 = USOIL - U
              SUMES1 = U
              T= (SUMES2/3.5)**2
          ENDIF

          CALL GET("PM", "PMFRACTION", PMFRACTION)

!-----------------------------------------------------------------------
!     Set air dry water content for top soil layer
          SWEF = 0.9-0.00038*(DLAYR(1)-30.)**2

!***********************************************************************
!     RATE CALCULATIONS
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
C  Adjust soil evaporation, and the sum of stage 1 (SUMES1) and stage 2
C    (SUMES2) evaporation based on infiltration (WINF), potential
C    soil evaporation (EOS), and stage 1 evaporation (U).
!-----------------------------------------------------------------------
      IF ((SUMES1 .GE. U) .AND. (WINF .GE. SUMES2)) THEN

!       Stage 1 Evaporation
        WINFMOD = WINF - SUMES2
        SUMES1 = U - WINFMOD
        SUMES2 = 0
        T = 0.0
        IF (WINFMOD .GT. U) SUMES1 = 0.0
        CALL ESUP(EOS, SUMES1, SUMES2, U, ES, T)  !Supplementary calcs

      ELSE IF ((SUMES1 .GE. U) .AND. (WINF .LT. SUMES2)) THEN
!       Stage 2 Evaporation
        T = T + 1.0
        ES = 3.5 * T**0.5 - SUMES2
        IF (WINF .GT. 0.0) THEN
          ESX = 0.8 * WINF
          IF (ESX .LE. ES) ESX = ES + WINF
          IF (ESX .GT. EOS) ESX = EOS
          ES = ESX
        ELSE IF (ES .GT. EOS) THEN
          ES = EOS
        ENDIF
        SUMES2 = SUMES2 + ES - WINF
        T = (SUMES2/3.5)**2

      ELSE IF (WINF .GE. SUMES1) THEN
!       Stage 1 evaporation
        SUMES1 = 0.0
        CALL ESUP(EOS, SUMES1, SUMES2, U, ES, T)  !Supplementary calcs

      ELSE
!       Stage 1 evaporation
        SUMES1 = SUMES1 - WINF
        CALL ESUP(EOS, SUMES1, SUMES2, U, ES, T)  !Supplementary calcs

      ENDIF

C-----------------------------------------------------------------------
C  Soil evaporation can not be larger than the current extractable soil
C    water in the top layer.
C    If available soil water is less than soil evaporation, adjust first
C    and second stage evaporation and soil evaporation accordingly
C-----------------------------------------------------------------------
      AWEV1 = (SW(1) - LL(1) * SWEF) * DLAYR(1) * 10.0
      AWEV1 = MAX(0.0,AWEV1)

      IF (AWEV1 .LT. ES) THEN
         IF (SUMES1 .GE. U .AND. SUMES2 .GT. ES) THEN
            SUMES2 = SUMES2 - ES + AWEV1
            T = (SUMES2/3.5)**2
            ES = AWEV1

         ELSE IF (SUMES1 .GE. U .AND. SUMES2 .LT. ES .AND.
     &            SUMES2 .GT. 0) THEN
            SUMES1 = SUMES1 - (ES - SUMES2)
            SUMES2 = MAX(SUMES1 + AWEV1 - U,0.0)
            SUMES1 = MIN(SUMES1 + AWEV1, U)
            T = (SUMES2/3.5)**2
            ES = AWEV1
         ELSE
            SUMES1 = SUMES1 - ES + AWEV1
            ES = AWEV1
         ENDIF
      ENDIF

!     Apply the fraction of plastic mulch coverage
      IF (PMFRACTION .GT. 1.E-6) THEN
        ES = ES * (1.0 - PMFRACTION)
      ENDIF

!-----------------------------------------------------------------------
!     Available water = SW - air dry limit + infil. or sat. flow
      SWMIN = MAX(0.0, SW_AVAIL - SWEF * LL(1))

!     Limit ES to between zero and avail water in soil layer 1
      IF (ES .GT. SWMIN * DLAYR(1) * 10.) THEN
        ES = SWMIN * DLAYR(1) * 10.
      ENDIF
      ES = MAX(ES, 0.0)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE SOILEV
!-----------------------------------------------------------------------

C=======================================================================
C  ESUP, Subroutine, J. Ritchie, B. Baer
C  Supplementary calculations for SOILEV
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/89 JR  Written
C  08/15/93 BB? Revised into IF-THEN-ELSE structure
C  11/30/93 NBP Revised header and I/O order
C  08/21/95 GH  Added calculations for SUMES2, second stage evaporation
!  10/13/97 CHP Modified for modular format.
!-----------------------------------------------------------------------
!  Called by: SOILEV
!  Calls:     None
C=======================================================================
      SUBROUTINE ESUP(EOS, SUMES1, SUMES2, U, ES, T)
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
!     INPUT VARIABLES:
      REAL EOS, U
!-----------------------------------------------------------------------
!     INPUT/OUTPUT VARIABLES:
      REAL SUMES1
!-----------------------------------------------------------------------
!     OUTPUT VARIABLES:
      REAL ES, SUMES2, T
!-----------------------------------------------------------------------
C  Calculate stage 1 soil evaporation
C    If the sum for stage 1 soil evaporation (SUMES1) is larger than
C    stage 1 evaporation limit (U), start stage 2 soil evaporation (SUMES2)
C    and adjust soil evaporation (ES)
C-----------------------------------------------------------------------
      SUMES1 = SUMES1 + EOS
      IF (SUMES1 .GT. U) THEN
        ES = EOS - 0.4 * (SUMES1 - U)
        SUMES2 = 0.6 * (SUMES1 - U)
        T = (SUMES2/3.5)**2
        SUMES1 = U
      ELSE
        ES = EOS
      ENDIF

      END SUBROUTINE ESUP
!
!-----------------------------------------------------------------------
!     SOILEV, ESUP VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! AWEV1       Available water for soil evaporation (mm/d)
! DLAYR(L)    Soil thickness in layer L (cm)
! DUL(L)      Volumetric soil water content at Drained Upper Limit in soil
!               layer L (cm3 [H2O] /cm3 [soil])
! EOS         Potential rate of soil evaporation (mm/d)
! ERRKEY      Subroutine name for error file
! ES          Actual soil evaporation rate (mm/d)
! ESX         Interim value of evaporation rate for Stage 2 evaporation
!               calculations
! LL(L)       Volumetric soil water content in soil layer L at lower limit
!               (cm3/cm3)
! LNUM        Current line number of input file
! NL          Maximum number of soil layers = 20
! SUMES1      Cumulative soil evaporation in stage 1 (mm)
! SUMES2      Cumulative soil evaporation in stage 2 (mm)
! SW(L)       Volumetric soil water content in layer L
!               (cm3 [water] / cm3 [soil])
! SW_AVAIL(L) Soil water content in layer L available for evaporation,
!               plant extraction, or movement through soil
!               (cm3 [water] / cm3 [soil])
! SWEF        Soil water evaporation fraction; fraction of lower limit
!               content to which evaporation can reduce soil water content
!               in top layer (fraction)
! SWR         Soil water content in top layer expressed as fractional value
!               between lower limit and drained upper limit (fraction)
! T           Number of days into Stage 2 evaporation (WATBAL); or time
!               factor for hourly temperature calculations
! U           Evaporation limit (cm)
! USOIL       Soil water deficit below the drained upper limit
!               (cm3 [water] / cm3 [soil])
! WINF        Potential Precipitation for infiltration (mm)
! WINFMOD     Interrim value of WINF, water available for infiltration (mm)
!-----------------------------------------------------------------------

