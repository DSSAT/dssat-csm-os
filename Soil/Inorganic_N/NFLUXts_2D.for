!***********************************************************************
!  NFLUX_2D, subroutine
!
!  Purpose: Determines movement of nitrogen with water flow
!     for 2D model. 
!  Two subroutines are used with some shared variables.
!  - NFLUX_2D is called daily from SoilNi.for and sends back the daily
!       accumulated values of DeltaSNO3 and DeltaUREA due to soil
!       water fluxes.
!  - NFLUXts_2D is called from WatBal_ts on a sub-daily time step. At each
!       time step, N fluxes are computed as a proportion of water fluxes to
!       soil water content (i.e, if 20% of water moves in a time step, then
!       20% of soluble N moves with it). 

!  Revision history:
!  ....     ...  Written
!  05/31/2010 CHP/JZW  2D written based on NFLUX
!  08/15/2011 Add arguments NFlux_L, NFlux_R, NFlux_D, NFlux_U
!  09/10/2024 chp Modified to move N on a sub-daily time step to eliminate
!                 instabilities.
!***********************************************************************

      MODULE NFLUXts

      USE Cells_2D

!     These variables are shared between NFLUX_2D (called daily by SoilNi) 
!       and NFLUXts_2D (called sub-daily by WatBal_2D)
      REAL, PROTECTED :: DayNLeach
      REAL, DIMENSION(NL), PROTECTED :: FRAC_SOLN_urea, FRAC_SOLN_no3
      REAL, DIMENSION(MaxRows,MaxCols) :: CellArea
      REAL, DIMENSION(MaxRows,MaxCols) :: SNO3ts, UREAts, 
     &  SNO3init, UREAinit
      INTEGER, DIMENSION(MaxRows,MaxCols) :: Cell_Type
      LOGICAL, PROTECTED :: First_ts

      contains

!==========================================================================
      SUBROUTINE NFLUX_2D (DYNAMIC,
     &  CELLS, SNO3_2D, SOILPROP, UREA_2D,              !Input
     &  CLeach, TLeachD, DLTSNO3_2D, DLTUREA_2D)        !Output

!     ------------------------------------------------------------------
      IMPLICIT  NONE
      SAVE
!     ------------------------------------------------------------------

      INTEGER, INTENT(IN) :: DYNAMIC
      TYPE (CellType), INTENT(IN) :: CELLS(MaxRows,MaxCols)
      REAL, DIMENSION(MaxRows,MaxCols), INTENT(IN) :: SNO3_2D, UREA_2D
      TYPE (SoilType), INTENT(IN) :: SOILPROP
      REAL, DIMENSION(MaxRows,MaxCols), INTENT(OUT) :: 
     &       DLTSNO3_2D, DLTUREA_2D
      REAL, INTENT(OUT) :: CLeach, TLeachD

      INTEGER L, i, j
      REAL NewSNO3, NewUREA
      REAL, DIMENSION(MaxRows) :: ADCOEF, BD, DUL
      REAL, DIMENSION(MaxRows,MaxCols) :: DeltaSNO3, DeltaUrea

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!       These two variables are sent out to SoilNi daily
        CLeach = 0.0
        TLeachD = 0.0

!       Indicator that the next time NFLUXts_2D is called it will be
!         the first call of the day.  
        First_ts = .TRUE.

!       DayNLeach is equivalent to TLeachD, but it is "saved" in the 
!       module for sub-daily updates. The module allows the values 
!       to be shared between the daily and sub-daily subroutines while
!       protecting the export variable.
        DayNLeach = 0.0

        CellArea  = CELLS % STRUC % CellArea
        Cell_Type = CELLS % STRUC % Cell_Type

!     ------------------------------------------------------------------
!       FRAC_SOLN is the fraction of N in a layer that is in solution
!       and can move with the water flux. ADCOEF is the anion adsorption
!       coefficient of the layer (0.0 = no adsorption, hence no 
!       retarded movement of N)
        DUL = SOILPROP % DUL
        BD  = SOILPROP % BD
        ADCOEF = SOILPROP % ADCOEF

        FRAC_SOLN_urea = 1.  !urea
        DO L = 1, NRowsTot
          FRAC_SOLN_no3(L) = 1. / (1. + BD(L) * ADCOEF(L) / DUL(L)) 
        ENDDO

!***********************************************************************
!***********************************************************************
!     DAILY RATE
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!     ------------------------------------------------------------------
!     This call from SoilNi is done after WatBal_2D has computed water and
!       N fluxes at a sub-daily time step. The integration of sub-daily fluxes 
!       is used to compute daily DeltaSNO3 and DeltaUrea.

!     Update Delta N variables to send back
      DeltaSNO3 = SNO3ts - SNO3init
      DeltaUrea = UREAts - UREAinit

      DO i = 1, NRowsTot
        DO j = 1, NColsTot
          IF (Cell_Type(i,j) < 3 .OR. Cell_Type(i,j) > 5) CYCLE

          DLTSNO3_2D(i,j) = DLTSNO3_2D(i,j) + DeltaSNO3(i,j)
          NewSNO3 = SNO3_2D(i,j) + DLTSNO3_2D(i,j)
          IF (NewSNO3 < 0.0) THEN 
            DLTSNO3_2D(i,j) = -SNO3_2D(i,j)
            IF (i == NRowsTot) THEN
              TLeachD = TLeachD + NewSNO3
            ENDIF
          ENDIF

          DLTUREA_2D(i,j) = DLTUREA_2D(i,j) + DeltaUrea(i,j)
          NewUREA = UREA_2D(i,j) + DLTUREA_2D(i,j)
          IF (NewUREA < 0.0) THEN 
            DLTUREA_2D(i,j) = -UREA_2D(i,j)
            IF (i == NRowsTot) THEN
              TLeachD = TLeachD + NewUREA
            ENDIF
          ENDIF

        ENDDO
      ENDDO

!     At the end of the day, send back leached amounts and Delta N values
      TLeachD = DayNLeach * 2.0 !Double for the entire field
      CLeach = CLeach + TLeachD

      First_ts = .TRUE.

!     ------------------------------------------------------------------
!     End of DYNAMIC IF block
      ENDIF
!     ------------------------------------------------------------------
      END SUBROUTINE NFLUX_2D
!==========================================================================

!==========================================================================
!     This subroutine is called every time step by the WatBal_2D subroutine,
!     after movement of water in cells is calculated. Compute N fluxes based
!     on water fluxes.
!==========================================================================
      SUBROUTINE NFLUXts_2D (
     &  CELLS, SWV_ts, SWFh_ts, SWFv_ts)       !Input
!     ------------------------------------------------------------------

      IMPLICIT  NONE
      SAVE

      TYPE (CellType), INTENT(IN) :: CELLS(MaxRows,MaxCols)
      Double Precision, DIMENSION(MaxRows,MaxCols), INTENT(IN) ::SWV_ts
      REAL, DIMENSION(MaxRows,MaxCols), INTENT(IN) :: SWFh_ts, SWFv_ts

      INTEGER i, j
      REAL FracSWVh, FracSWVv
      REAL ResidualNO3, ResidualUREA
      REAL, DIMENSION(MaxRows,MaxCols) :: NO3Fh, UreaFh, NO3Fv, UreaFv

!***********************************************************************
!***********************************************************************
!     Sub-daily time step rate calculations 
!***********************************************************************
      IF (First_ts) THEN

!       Calculate available N at the beginning of the day
!       Plant uptake of N was done at the end of the day yesterday. Subtract
!         this from SNO3 to ensure that we don't get negative values after
!         N fluxes are calculated.
        SNO3ts = CELLS % State % SNO3 - Cells % Rate % NO3Uptake
        UREAts = CELLS % State % UREA

!       Accumulated leached N
        DayNLeach = 0.0

!       Check for N leaks
        ResidualNO3 = 0.0
        ResidualUREA = 0.0

        First_ts = .FALSE.

        SNO3init = SNO3ts
        UREAinit = UREAts

      ENDIF

!     --------------------------------------------------------------
!     N flux (kg[N]/ha) is proportional to water flux (cm2)
      DO i = 1, NRowsTot
        DO j = 1, NColsTot
          IF (Cell_Type(i,j) > 5 .OR. Cell_Type(i,j) < 3) CYCLE

!         --------------------------------------------------------------
!         Vertical fluxes, all cells
!         SWFv_ts is the vertical flux at the boundary of cell(i,j) and cell(i+1,j)
          IF (ABS(SWFv_ts(i,j)) .LT. 1.E-10) THEN
!           No water flux, no N flux
            NO3Fv(i,j) = 0.0
            UREAFv(i,j) = 0.0

          ELSEIF (SWFv_ts(i,j) > 1.E-10) THEN
!           Positive vertical fluxes from cell(i,j) to cell(i+1,j)
!           Calculate the fraction of water that moves out of cell(i,j) with this flux
            FracSWVv = SWFv_ts(i,j) / CellArea(i,j) / SWV_ts(i,j)
!           N fluxes are proportional to water fluxes
            NO3Fv(i,j) = MAX(0.0, SNO3ts(i,j) * FRAC_SOLN_NO3(i))
     &        * FracSWVv
            UreaFv(i,j) = MAX(0.0, UREAts(i,j) * FRAC_SOLN_urea(i))
     &        * FracSWVv

!           ****************************
!           N leaching
            IF (i == NRowsTot) THEN
              DayNLeach= DayNLeach + NO3Fv(i,j) + UreaFv(i,j)
            ENDIF
!           ****************************

          ELSE
!           Negative vertical fluxes from cell(i+1,j) to cell(i,j)
!           Calculate the fraction of water that moves out of cell(i+1,j) with this flux
            FracSWVv = SWFv_ts(i,j) / CellArea(i,j) / SWV_ts(i,j)
            NO3Fv(i,j) = MAX(0.0, SNO3ts(i+1,j) * FRAC_SOLN_NO3(i))
     &        * FracSWVv
            UreaFv(i,j) = MAX(0.0, UREAts(i+1,j) * FRAC_SOLN_urea(i))
     &        * FracSWVv
          ENDIF

!         Pseudo-integration of N time step variables prevents negative values
          SNO3ts(i,j)   = SNO3ts(i,j)   - NO3Fv(i,j)
          SNO3ts(i+1,j) = SNO3ts(i+1,j) + NO3Fv(i,j)
          UREAts(i,j)   = UREAts(i,j)   - UREAFv(i,j)
          UREAts(i+1,j) = UREAts(i+1,j) + UREAFv(i,j)

!         --------------------------------------------------------------
!         Horizontal N Flux (all soil cells except right boundary)
          IF ((Cell_Type(i,j) == 3 .AND. j < BedDimension % FurCol1 - 1)
     &      .OR. (Cell_Type(i,j) == 4 .AND. j < NColsTot) 
     &      .OR. (Cell_Type(i,j) == 5 .AND. j < NColsTot)) THEN

!           SWFh_ts is the horizontal flux at the boundary of cell(i,j) and cell(i,j+1)
            IF (ABS(SWFh_ts(i,j)) .LT. 1.E-10) THEN
!             No water flux, no N flux
              NO3Fh(i,j) = 0.0
              UREAFh(i,j) = 0.0

            ELSEIF (SWFh_ts(i,j) > 1.E-10) THEN

!             Positive horizontal fluxes from cell(i,j) to cell(i,j+1)
!             Calculate the fraction of water that moves out of cell(i,j) with this flux
              FracSWVh = SWFh_ts(i,j) / CellArea(i,j) / SWV_ts(i,j)
              NO3Fh(i,j) = FracSWVh * SNO3ts(i,j) * FRAC_SOLN_NO3(i)
              UreaFh(i,j) = FracSWVh * UREAts(i,j) * FRAC_SOLN_urea(i)

            ELSE
!             Negative horizontal fluxes from cell(i,j+1) to cell(i,j)
!             Calculate the fraction of water that moves out of cell(i,j+1) with this flux
              FracSWVh = SWFh_ts(i,j) / CellArea(i,j+1) / SWV_ts(i,j+1)
              NO3Fh(i,j) = FracSWVh * SNO3ts(i,j+1) * FRAC_SOLN_NO3(i)
              UreaFh(i,j) = FracSWVh * UREAts(i,j+1) *FRAC_SOLN_urea(i)
            ENDIF
          ENDIF

!         Pseudo-integration of N time step variables prevents negative values
          SNO3ts(i,j)   = SNO3ts(i,j)   - NO3Fh(i,j)
          SNO3ts(i,j+1) = SNO3ts(i,j+1) + NO3Fh(i,j)
          UREAts(i,j)   = UREAts(i,j)   - UREAFh(i,j)
          UREAts(i,j+1) = UREAts(i,j+1) + UREAFh(i,j)

        ENDDO
      ENDDO

!     Check for negative state values
      DO i = 1, NRowsTot
        DO j = 1, NColsTot
          IF (SNO3ts(i,j) < -1.E-10) THEN
            ResidualNO3 = ResidualNO3 + SNO3ts(i,j)
            SNO3ts(i,j) = 0.0
          ENDIF
          IF (UREAts(i,j) < -1.E-10) THEN
            ResidualUREA = ResidualUREA + SNO3ts(i,j)
            UREAts(i,j) = 0.0
          ENDIF
        ENDDO
      ENDDO

      RETURN
      END SUBROUTINE NFLUXts_2D

!==========================================================================
      END MODULE NFLUXts
!==========================================================================

!==========================================================================
! NFLUX_2D Variable List
!==========================================================================
! ADCOEF(L)      Anion adsorption coefficient for soil layer L;  for 
!                  reduced anion (nitrate) flow in variable-charge soils 
!                  (ADCOEF = 0 implies no anion retention)
!                  (cm3 (H2O] / g [soil])
! BD(L)          Bulk density, soil layer L (g [soil] / cm3 [soil])
! CellArea       Area of cell (cm2)
! Cell_Type      Cell type, see definitions in SoilCellUtils_2D.f90
! CLeach         Cumulative N leached from soil (kg[N]/ha) - sent back to SoilNi
! DayNLeach      Total N leached from soil today (kg[N]/ha) - local variable
! DeltaNO3       Change in NO3 state today (kg[N]/ha)
! DeltaUrea      Change in urea state today (kg[N]/ha)
! DUL(L)         Volumetric soil water content at Drained Upper Limit in 
!                  soil layer L (cm3[water]/cm3[soil])
! FRAC_SOLN_no3  Fraction of NO3 in solution and able to move with water
! FRAC_SOLN_urea Fraction of urea in solution and able to move with water
! NO3Fh          Horizontal NO3 flux for this time step (kg[N]/ha)
! NO3Fv          Vertical NO3 flux for this time step (kg[N]/ha)
! ResidualNO3    Check for N loss (kg[N]/ha)
! ResidualUREA   Check for N loss (kg[N]/ha)
! SNO3init       Initial NO3 at beginning of the day (kg[N]/ha)
! SNO3ts         NO3 updated on sub-daily time step (kg[N]/ha)
! SWFh_ts        Horizontal soil water flux for this time step (cm2)
! SWFv_ts        Vertical soil water flux for this time step (cm2)
! SWV_ts         Soil water content at this sub-daily time step (cm3/cm3)
! TLeachD        Total N leached from soil today (kg[N]/ha) - sent back to SoilNi
! UreaFh         Horizontal Urea flux for this time step (kg[N]/ha)
! UreaFv         Vertical urea flux for this time step (kg[N]/ha)
! UREAinit       Inital urea at the beginning of the day (kg[N]/ha)
! UREAts         Urea updated on sub-daily time step (kg[N]/ha)
!==========================================================================
!==========================================================================

