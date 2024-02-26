!=======================================================================
!  SALUS SOIL EVAPORATION MODULE - File ESR_SoilEvap.for
!=======================================================================
!  ESR_SoilEvap, Subroutine, J. Ritchie, C. Porter
!
!  Calculates actual soil evaporation (ES, mm/d) based on method
!  described in:
!
!  Ritchie, J.T., C.H. Porter, J.Judge, J.W.Jones, A.A. Suleiman. 2009.
!    Application of a functional model for simulation of soil evaporation
!    and water redistribution.  Division S-1 -- soil Physics;
!    Soil Science Society of America. in review.
!
!  and
!
!  Suleiman, A.A., J.T.Ritchie. Modeling Soil Water Redistribution
!    during Second-Stage Evaporation. Division S-1 -- soil Physics;
!    Soil Science Society of America. Vol. 67, No. 2. March-apr 2003.
!
!  This routine takes the place of SOILEV and UPFLOW.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  05/03/2005 JTR/CHP Written
!  07/05/2006 JTR/CHP Modified calculation of C_dry, function of depth
!                         only.  Eliminate A and B coefficients.
!  04/08/2008 JTR modification for wet profile
!  05/29/2008 JTR added intermediate profile case
!  10/02/2008 CHP/JTR changed depth for determining evaporation case
!                     from 50 cm to 100 cm.
!  02/27/2009 CHP Modified for 2D model
!  01/24/2024 chp Integrated 2D process into 1D model
!-----------------------------------------------------------------------
!  Called by: SPAM
!=======================================================================
      SUBROUTINE ESR_SoilEvap(CONTROL,
     &   CELLS, EOS, SOILPROP, SOILPROP_FURROW, SWDELTS, WINF,  !Input
     &   ES, ES_LYR, SWDELTU, UPFLOW)                           !Output

!-----------------------------------------------------------------------
      USE Cells_2D; USE ModuleData
      IMPLICIT NONE
      SAVE

!     ------------------------------------------------
!     Interface Variables:
      TYPE (ControlType), INTENT(IN) :: CONTROL
      TYPE(CellType), DIMENSION(MaxRows,MaxCols), INTENT(INOUT) :: CELLS
      TYPE (SoilType), INTENT(IN) :: SOILPROP, SOILPROP_FURROW 
      REAL, INTENT(IN) :: EOS          !Potential soil evap (mm/d)
      REAL, INTENT(IN) :: WINF
      REAL, INTENT(IN) :: SWDELTS(NL)  !Rate of drainage (cm3/cm3)

      REAL, INTENT(OUT):: ES           !Actual soil evaporation (mm/d)
      REAL, DIMENSION(NL), INTENT(OUT) :: ES_LYR   !Actual ES (mm/d)
      REAL, DIMENSION(NL), INTENT(OUT) :: SWDELTU
!     REAL, DIMENSION(MaxRows, MaxCols), INTENT(OUT) :: SWDELTU
      REAL, INTENT(OUT):: UPFLOW(NL)   !Flow or N transport (cm/d)
!     UPFLOW(1:NL) refers to water which moves up from layer L to
!       layer L-1, and includes upflow from lower layers.
!     ------------------------------------------------

!      CHARACTER*12, PARAMETER :: ERRKEY = 'SAL_SoilEvap'
      INTEGER DYNAMIC, L, NLAYR, ProfileType, StartRow
      REAL A, B, RedFac, SW_threshold, Infilt
      REAL, DIMENSION(NL) :: DLAYR, DS, DUL, LL, MEANDEP
      REAL, DIMENSION(NL) :: SWAD, SWTEMP, SW_AVAIL, ES_Coef
      REAL, DIMENSION(MaxCols) :: ES_col
      REAL, DIMENSION(0:MaxCols) :: PMFRACTION, EOS_factor, EOS_max
      REAL, DIMENSION(MaxRows, MaxCols) :: CellEvap

!     2D additions:
      TYPE (SoilType) USE_SOILPROP
      INTEGER Col, FurRow1, FurCol1, Row
      REAL, DIMENSION(MaxRows, MaxCols) :: mm_2_vf, Cell_Type
      REAL, DIMENSION(MaxRows, MaxCols) :: SWV, ES_mm, ColFrac
      REAL SimWidth

      DYNAMIC = CONTROL % DYNAMIC

      SWV = CELLS % STATE % SWV
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
      UPFLOW = 0.0
      CellEvap = 0.0

      IF (CONTROL % Sim2D) THEN
        SimWidth = Row
      ENDIF

      Cell_Type = CELLS % STRUC % Cell_Type

!     PMFraction is the fraction of the soil covered by plastic mulch
!     PMFraction(0) is the entire row. PMFraction(J) is for each column of soil.
      CALL GET("PM", "PMFRACTION", PMFRACTION, MaxCols+1)
      CALL GET("PM", "EOS_factor", EOS_factor, MaxCols+1)

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
!     ProfileType:
!     1 = Wet: SW > DUL in at least one layer in top 100 cm and
!           SW > SW_threshold in top layer
!     2 = Intermediate: wet, but SW < SW_threshold in top layer
!     3 = Dry: SW < DUL in all layers in top 100 cm
!-----------------------------------------------------------------------
      ES = 0
      ES_mm = 0.0
      ES_LYR = 0.0
      ES_col = 0.0
      UPFLOW = 0.0
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
!       Maximum potential soil evaporation by column accounts for partial 
!         coverage with plastic mulch. To maintain the overall field 
!         potential EOS, increase EOS for columns not covered by plastic. 
!         This does not necessarily increase the actual soil evaporation 
!         which is limited by available soil water.
        EOS_max(col) = EOS * EOS_factor(col)

        IF (.NOT. CONTROL % SIM2D .OR. Cell_Type(1,Col) > 2) THEN
!         This is either a 1D simulation or a bed with no plastic mulch or a flat system.
          Use_SOILPROP = SOILPROP
          StartRow = 1
        ELSE
!         This is a 2D furrow layer with no plastic mulch
          Use_SOILPROP = SOILPROP_FURROW
          StartRow = FurRow1
        ENDIF

        DLAYR = Use_SOILPROP % DLAYR
        DS    = Use_SOILPROP % DS
        DUL   = Use_SOILPROP % DUL
        LL    = Use_SOILPROP % LL
        NLAYR = Use_SOILPROP % NLAYR

!**********************************************************************
        ProfileType = 3   !assume dry profile until proven wet
        DO L = 1, NLAYR
!         2D row location (if furrow, the top layer is not the top cell
          Row = L+StartRow-1  
          SWTEMP(L) = CELLS(Row,Col)%State%SWV

!         Air dry water content
          SWAD(L) = 0.30 * LL(L) !JTR 11/28/2006

!         Mean depth for each soil layer
          MEANDEP(L) = DS(L) - DLAYR(L) / 2.  !cm

          IF (.NOT. CONTROL % Sim2D) THEN
!           Pseudo-integraton step
!           If increase in SW due to rain or irrigation, include half
            IF (SWDELTS(L) > 0.0) THEN
              SWTEMP(L) = SWV(Row,Col) + 0.5 * SWDELTS(L)
            ELSE
!             If decrease in SW due to drainage, include all
              SWTEMP(L) = SWV(Row,Col) + SWDELTS(L)
            ENDIF
          ELSE
!           Use SWV with no pseudo-integration for 2D
            SWTEMP(L) = SWV(Row,Col) + 0.5 * Infilt
          ENDIF

!         If any layer in top 100 cm is wet, use wet profile method
          IF (MEANDEP(L) < 100. .AND. SWTEMP(L) > DUL(L)) THEN
            ProfileType = 1
          ENDIF

!         If wet profile, check for top layer SW below threshold.
          IF (ProfileType == 1) THEN
!           SW_threshold = DUL(1) - 0.05 !/ 0.13 * (DUL(1) - LL(1))
!           JTR 6/4/2008
!           Threshold WC = 0.275*DUL +1.165*DUL^2 + (1.2*DUL^3.75)*depth (center)
            SW_threshold = 0.275*DUL(1) + 1.165*DUL(1)*DUL(1) +
     &              (1.2*DUL(1)**3.75)*MEANDEP(1)
!           chp 6/4/2008 use DUL - 0.05, like before, but limit to air dry
!            SW_threshold = MAX(SWAD(1), DUL(1) - 0.05)
            IF (SWTEMP(1) < SW_threshold) THEN
              ProfileType = 2
            ENDIF
          ENDIF

!-----  ------------------------------------------------------------------
          SELECT CASE (ProfileType)

!         Dry profile
          CASE (3)
!           Depth-dependant coefficients based on Ritchie spreadsheet 11/29/2006
            A =  0.5  + 0.24 * DUL(L)
            B = -2.04 + 0.20 * DUL(L)
            ES_Coef(L) = A * MEANDEP(L) ** B

!         Equilibrium profile
          CASE (2)
            ES_Coef(L) = 0.011   !for all depths

!         Wet profile
          CASE (1)
!           Ritchie spreadsheet of 5/28/08
            A = 0.26  !6/20/08  A = 0.14  !6/2/08  A = 0.42   !4/18/08
            B = -0.70 !6/20/08  B = -0.46 !6/2/08  B = -0.73  !4/18/08
            ES_Coef(L) = A * MEANDEP(L) ** B !function, no integration

          END SELECT
!-----  ------------------------------------------------------------------

!         CellEvap in mm3/mm3
          CellEvap(Row,Col) = -(SWTEMP(L) - SWAD(L)) * ES_Coef(L) 

!         Apply the fraction of plastic mulch coverage
          CellEvap(Row,Col) = CellEvap(Row,Col) *
     &      (1.0 - PMFRACTION(Col))
        
!         Limit to available water
!         SW_AVAIL(L) = SW(L) + SWDELTS(L) - SWAD(L)
          SW_AVAIL(L) = SWV(Row,Col) - SWAD(L)
          IF (-CellEvap(Row,Col) > SW_AVAIL(L)) THEN
            CellEvap(Row,Col) = -SW_AVAIL(L)                   !mm3/mm3
          ENDIF

!         Limit to negative values (decrease SW)
          CellEvap(Row,Col) = AMIN1(0.0, CellEvap(Row,Col))

!         Aggregate soil evaporation from each cell.  
!         Scale with half row spacing for 2D simulations.

          IF (CONTROL % Sim2D) THEN
            ES_mm(Row,Col) = -CellEvap(Row,Col) / mm_2_vf(Row,Col)
          ELSE
            ES_mm(Row,Col) = -CellEvap(Row,Col) * DLAYR(L) * 10.
          ENDIF
!         ES_LYR(L) = ES_LYR(L) + ES_mm(Row,Col) * ColFrac(Row,Col)
          ES_col(col) = ES_col(col) + ES_mm(Row,Col)
        ENDDO

!       Limit total profile soil evaporation to potential soil evaporation
        RedFac = 1.0
        IF (ES_col(col) > EOS_max(col)) THEN
          RedFac = EOS_max(col) / ES_col(col)
        ENDIF

        DO L = 1, NLAYR
          Row = L+StartRow-1  
          CellEvap(Row,Col) = CellEvap(Row,Col) * RedFac
          ES_mm(Row,Col) = ES_mm(Row,Col) * RedFac
          ES_LYR(L) = ES_LYR(L) + ES_mm(Row,Col) * ColFrac(Row,Col)
        ENDDO

        ES = ES + ES_col(col) * ColFrac(Row,Col)  !profile sum (mm)
      ENDDO

!     UPFLOW calcs are only for 1D simulations
      IF (.NOT. CONTROL % SIM2D) THEN
        UPFLOW = 0.0
        UPFLOW(NLAYR) = ES_LYR(NLAYR) / 10.
        DO L = NLAYR-1, 1, -1
          UPFLOW(L) = UPFLOW(L+1) + ES_LYR(L) / 10. !cm/d
          SWDELTU(l) = CellEvap(L,1)
        ENDDO
      ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF

      CELLS % RATE % ES_Rate = ES_mm
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE ESR_SoilEvap
!=======================================================================

