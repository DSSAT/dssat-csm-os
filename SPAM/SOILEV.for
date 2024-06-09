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
!  01/24/2024 chp Integrated 2D process into 1D model
!-----------------------------------------------------------------------
!  Called by: SPAM
!  Calls:     ESUP
C=======================================================================
      SUBROUTINE SOILEV(CONTROL,
     &    CELLS, EOS, U, WINF, SWAVAIL,         !Input
     &    SOILPROP, SOILPROP_FURROW,            !Input
     &    ES, ES_LYR)                           !Output

! GET FROM SOILPROP: DLAYR, DUL, LL, 
!-----------------------------------------------------------------------
      USE Cells_2D
      USE ModuleData
      IMPLICIT NONE
      EXTERNAL ESUP
      SAVE

!     ------------------------------------------------
!     Interface Variables:
      TYPE (ControlType), INTENT(IN) :: CONTROL
      TYPE(CellType), DIMENSION(MaxRows,MaxCols), INTENT(INOUT) :: CELLS
      TYPE (SoilType), INTENT(IN) :: SOILPROP, SOILPROP_FURROW 
      REAL, INTENT(IN) :: EOS          !Potential soil evap (mm/d)
      REAL, INTENT(IN) :: U, WINF
      REAL, DIMENSION(MaxCols), INTENT(IN) :: SWAVAIL
      REAL, INTENT(OUT) :: ES
      REAL, DIMENSION(NL), INTENT(OUT) :: ES_LYR

      !CHARACTER*6 ERRKEY
      !PARAMETER (ERRKEY = 'SOILEV')

      INTEGER DYNAMIC, L, NLAYR, Row
      REAL SWMIN, WINFMOD, AWEV1, ESX, Infilt
      REAL, DIMENSION(NL) :: DLAYR, DUL, LL

      REAL, DIMENSION(MaxCols) :: SWR, USOIL, SUMES1, SUMES2, T, SWEF
      REAL, DIMENSION(MaxCols) :: ES_col
      REAL, DIMENSION(0:MaxCols) :: PMFRACTION
      REAL, DIMENSION(MaxRows, MaxCols) :: CellEvap

!     2D additions:
      TYPE (SoilType) USE_SOILPROP
      INTEGER Col, FurRow1, FurCol1
      REAL, DIMENSION(MaxRows, MaxCols) :: mm_2_vf, Cell_Type
      REAL, DIMENSION(MaxRows, MaxCols) :: SWV, ES_mm, ColFrac
      REAL, DIMENSION(MaxCols) :: ESc

      DYNAMIC = CONTROL % DYNAMIC

      SWV       = CELLS % STATE % SWV
!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      FurRow1 = BedDimension % FurRow1
      FurCol1 = BedDimension % FurCol1
      mm_2_vf = BedDimension % mm_2_vf
      ColFrac = BedDimension % ColFrac

      ES = 0.0
      ES_mm = 0.0
      ES_LYR = 0.0
      ES_col = 0.0
      CellEvap = 0.0

      Cell_Type = CELLS % STRUC % Cell_Type

!     PMFraction is the fraction of the soil covered by plastic mulch
!     PMFraction(0) is the entire row. PMFraction(J) is for each column of soil.
      CALL GET("PM", "PMFRACTION", PMFRACTION, MaxCols+1)

!     Loop through columns and initialize for each column separately
      DO Col = 1, NColsTot

        IF (.NOT. CONTROL % SIM2D .OR. PMFraction(col) < 0.001) THEN
!         This is either a 1D simulation or a bed with no plastic mulch or a flat system.
          Use_SOILPROP = SOILPROP
          Row = 1
        ELSE
!         This is a 2D furrow layer with no plastic mulch
          Use_SOILPROP = SOILPROP_FURROW
          Row = FurRow1
        ENDIF

        DLAYR = Use_SOILPROP % DLAYR
        DUL   = Use_SOILPROP % DUL
        LL    = Use_SOILPROP % LL
        NLAYR = Use_SOILPROP % NLAYR

!       Calculate initial soil water content with respect to evaporation sums
        SWR(Col) = MAX(0.0,(SWV(Row,Col) - LL(1)) / (DUL(1) - LL(1)))
        USOIL(Col) = (DUL(1) - SWv(Row,Col)) * DLAYR(1) * 10.
        
        IF (SWR(Col) .GE. 1.0) THEN
            SUMES1(Col) = 0.
            SUMES2(Col) = 0.
            T(Col) = 0.
        ELSE IF (USOIL(Col) .LE. U) THEN
            SUMES2(Col) = 0.
            T(Col) = 0.
            SUMES1(Col) = USOIL(Col)
        ELSE
            SUMES2(Col) = USOIl(Col) - U
            SUMES1(Col) = U
            T(Col)= (SUMES2(Col)/3.5)**2
        ENDIF

!       Set air dry water content for top soil layer
        SWEF(Col) = 0.9-0.00038*(DLAYR(1)-30.)**2
      ENDDO

!***********************************************************************
!     RATE CALCULATIONS
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
      ES = 0.0
      ESc = 0.0
      ES_mm = 0.0
      ES_LYR = 0.0
      ES_col = 0.0
      CellEvap = 0.0

!     Increase the infiltration amount (from rainfall and irrig) to account
!       for partial coverage of plastic mulch. Uncovered soil recieves additional
!       infiltration.
      IF (PMFraction(0) < 1.0) THEN
        Infilt = WINF / (1.0 - PMFraction(0))
      ELSE
        Infilt = 0.0
      ENDIF

!     Loop through columns and calculate soil evaporation for each column separately
      DO Col = 1, NColsTot

        IF (PMFraction(col) > 0.999) THEN
!         Full plastic mulch cover - no evaporation from this column. Move on.
          CYCLE
        ENDIF

        IF (.NOT. CONTROL % SIM2D .OR. Cell_Type(1,Col) == 3) THEN
!         This is either a 1D simulation in the 2D bed, start ES at top
          Use_SOILPROP = SOILPROP
          Row = 1
        ELSE
!         This is a 2D furrow column, ES is at top of furrow
          Use_SOILPROP = SOILPROP_FURROW
          Row = FurRow1
        ENDIF

        DLAYR = Use_SOILPROP % DLAYR
        DUL   = Use_SOILPROP % DUL
        LL    = Use_SOILPROP % LL
        NLAYR = Use_SOILPROP % NLAYR

C       Adjust soil evaporation, and the sum of stage 1 (SUMES1) and stage 2
C       (SUMES2) evaporation based on infiltration (Infilt), potential
C       soil evaporation (EOS), and stage 1 evaporation (U).
!-----------------------------------------------------------------------
        IF ((SUMES1(Col) .GE. U) .AND. (Infilt .GE. SUMES2(Col))) THEN

!         Stage 1 Evaporation
          WINFMOD = Infilt - SUMES2(Col)
          SUMES1(Col) = U - WINFMOD
          SUMES2(Col) = 0.0
          T(Col) = 0.0
          IF (WINFMOD .GT. U) SUMES1(Col) = 0.0
!         Supplementary calcs
          CALL ESUP(EOS, SUMES1(Col), SUMES2(Col), U, 
     &      ESc(col), T(Col))

        ELSEIF ((SUMES1(Col) >= U) .AND. (Infilt < SUMES2(Col))) THEN
!         Stage 2 Evaporation
          T(Col) = T(Col) + 1.0
          ESc(col) = 3.5 * T(Col)**0.5 - SUMES2(Col)
          IF (Infilt .GT. 0.0) THEN
            ESX = 0.8 * Infilt
            IF (ESX .LE. ESc(col)) ESX = ESc(col) + Infilt
            IF (ESX .GT. EOS) ESX = EOS
            ESc(col) = ESX
          ELSE IF (ESc(col) .GT. EOS) THEN
            ESc(col) = EOS
          ENDIF
          SUMES2(Col) = SUMES2(Col) + ESc(col) - Infilt
          T(Col) = (SUMES2(Col)/3.5)**2

        ELSE IF (Infilt .GE. SUMES1(Col)) THEN
!         Stage 1 evaporation
          SUMES1(Col) = 0.0
          CALL ESUP(EOS, SUMES1(Col), SUMES2(Col), U, 
     &      ESc(col), T(Col))

        ELSE
!         Stage 1 evaporation
          SUMES1(Col) = SUMES1(Col) - Infilt
          CALL ESUP(EOS, SUMES1(Col), SUMES2(Col), U, 
     &      ESc(col), T(Col))
        ENDIF

C-----------------------------------------------------------------------
C  Soil evaporation can not be larger than the current extractable soil
C    water in the top layer.
C    If available soil water is less than soil evaporation, adjust first
C    and second stage evaporation and soil evaporation accordingly
C-----------------------------------------------------------------------
        AWEV1 = (SWV(Row,col) - LL(1) * SWEF(col)) * DLAYR(1) * 10. !mm
        AWEV1 = MAX(0.0,AWEV1)

        IF (AWEV1 .LT. ESc(col)) THEN
           IF (SUMES1(Col) .GE. U .AND. SUMES2(Col) .GT. ESc(col)) THEN
              SUMES2(Col) = SUMES2(Col) - ESc(col) + AWEV1
              T(col) = (SUMES2(Col)/3.5)**2
              ESc(col) = AWEV1

           ELSE IF (SUMES1(Col) .GE. U .AND. 
     &              SUMES2(Col) .LT. ESc(col) .AND.
     &              SUMES2(Col) .GT. 0) THEN
              SUMES1(Col) = SUMES1(Col) - (ESc(Col) - SUMES2(Col))
              SUMES2(Col) = MAX(SUMES1(Col) + AWEV1 - U,0.0)
              SUMES1(Col) = MIN(SUMES1(Col) + AWEV1, U)
              T(Col) = (SUMES2(Col)/3.5)**2
              ESc(col) = AWEV1
           ELSE
              SUMES1(Col) = SUMES1(Col) - ESc(col) + AWEV1
              ESc(col) = AWEV1
           ENDIF
        ENDIF

!       Apply the fraction of plastic mulch coverage
        IF (PMFRACTION(Col) .GT. 1.E-6) THEN
          ESc(col) = ESc(col) *(1.0 - PMFRACTION(Col))
        END IF

!-----------------------------------------------------------------------
!       Available water = SW - air dry limit + infil. or sat. flow
        SWMIN = MAX(0.0, SWAVAIL(Col) - SWEF(col) * LL(1))

!       Limit ES to between zero and avail water in soil layer 1
        IF (ESc(col) .GT. SWMIN * DLAYR(1) * 10.) THEN
          ESc(col) = SWMIN * DLAYR(1) * 10.
        ENDIF
        ESc(col) = MAX(ESc(col), 0.0)

!        IF (CONTROL % Sim2D) THEN
!          ES_mm(Row,Col) = ESc(col) / mm_2_vf(Row,Col)
!        ELSE
!          ES_mm(Row,Col) = ESc(col) * DLAYR(1) * 10.
!        ENDIF
        ES_mm(Row,Col) = ESc(col)
!       ES_LYR(L) = ES_LYR(L) + ES_mm(Row,Col) * ColFrac(Row,Col)
        ES_col(col) = ES_col(col) + ES_mm(Row,Col)

        ES = ES + ESc(col) * ColFrac(Row,Col)  !profile sum (mm)
        CellEvap(Row,col) = -ESc(col)

      ENDDO

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF

      CELLS % RATE % ES_Rate = ES_mm
!-----------------------------------------------------------------------
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

