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
!-----------------------------------------------------------------------
!  Called by: SPAM
!=======================================================================
      SUBROUTINE ESR_SoilEvap(DYNAMIC,
     &   CELLS, EOS, SOILPROP, SW, SWDELTS,               !Input
     &   ES, ES_LYR, SWDELTU, UPFLOW)                     !Output
!      SUBROUTINE ESR_SoilEvap(DYNAMIC, 
!     &   CELLS, EOS, SOILPROP, SW, SWDELTS,                      !Input
!     &   ES, ES_LYR, ES_mm, SWDELTU, UPFLOW)                     !Output

!-----------------------------------------------------------------------
      USE Cells_2D; USE ModuleData
      IMPLICIT NONE
      SAVE

!     ------------------------------------------------
!     Interface Variables:
      INTEGER, INTENT(IN) :: DYNAMIC
      TYPE(CellType), DIMENSION(MaxRows,MaxCols), INTENT(IN) :: CELLS
      TYPE (SoilType), INTENT(IN) :: SOILPROP !Soil properties
      REAL, INTENT(IN) :: EOS          !Potential soil evap (mm/d)
      REAL, INTENT(IN) :: SW(NL)       !Soil water content (cm3/cm3)
      REAL, INTENT(IN) :: SWDELTS(NL)  !Rate of drainage (cm3/cm3)

      REAL, INTENT(OUT):: ES           !Actual soil evaporation (mm/d)
      REAL, DIMENSION(NL), INTENT(OUT) :: ES_LYR   !Actual ES (mm/d)
!     REAL, DIMENSION(NL), INTENT(OUT) :: SWDELTU
      REAL, DIMENSION(MaxRows, MaxCols), INTENT(OUT) :: SWDELTU
      REAL, INTENT(OUT):: UPFLOW(NL)   !Flow or N transport (cm/d)
      REAL, DIMENSION(MaxRows, MaxCols) :: ES_mm
!     UPFLOW(1:NL) refers to water which moves up from layer L to
!       layer L-1, and includes upflow from lower layers.
!     ------------------------------------------------

!      CHARACTER*12, PARAMETER :: ERRKEY = 'SAL_SoilEvap'
      INTEGER L, NLAYR, ProfileType, jj
      REAL A, B, RedFac, SW_threshold
      REAL, DIMENSION(NL) :: DLAYR, DS, DUL, LL, MEANDEP
      REAL, DIMENSION(NL) :: SWAD, SWTEMP, SW_AVAIL, ES_Coef
      REAL, DIMENSION(0:MaxCols) :: PMFRACTION

!     2D additions:
      INTEGER Col, FurRow1, FurCol1, Row
      REAL, DIMENSION(MaxRows, MaxCols) :: mm_2_vf

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      FurRow1 = BedDimension % FurRow1
      FurCol1 = BedDimension % FurCol1
      mm_2_vf = BedDimension % mm_2_vf

      ES = 0.0
      ES_mm = 0.0
      ES_LYR = 0.0

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
      ES = 0.0
      ES_mm = 0.0
      ES_LYR = 0.0

      CALL GET("PM", "PMFRACTION", PMFRACTION, MaxCols+1)

!     Loop through columns and calculate soil evaporation for each column separately
      DO Col = 1, NColsTot
!       If this column is covered by plastic mulch, no evaporation
        IF (PMFraction(Col) >= 1.0) THEN
          DO L = 1, NLAYR
            SWDELTU(L,Col) = 0.0
          ENDDO
          CYCLE
        ENDIF

        IF (.NOT. SIM2D .OR. CELLS(1,Col)%Cell_Type >2) THEN
!         This is either a 1D simulation or a bed with no plastic mulch or a flat system.
          Use_SOILPROP = SOILPROP
          J = 1
        ELSE
!         This is a furrow layer with no plastic mulch
          Use_SOILPROP = SOILPROP_FURROW
          J = FurRow1
        ENDIF

        DLAYR = SOILPROP % DLAYR
        DS    = SOILPROP % DS
        DUL   = SOILPROP % DUL
        LL    = SOILPROP % LL
        NLAYR = SOILPROP % NLAYR

!**********************************************************************
        ProfileType = 3   !assume dry profile until proven wet
        DO L = 1, NLAYR

!         Air dry water content
          SWAD(L) = 0.30 * LL(L) !JTR 11/28/2006

!         Mean depth for each soil layer
          MEANDEP(L) = DS(L) - DLAYR(L) / 2.  !cm

!         Pseudo-integraton step
!         If increase in SW due to rain or irrigation, include half
          IF (SWDELTS(L) > 0.0) THEN
            SWTEMP(L) = SW(L) + 0.5 * SWDELTS(L)
          ELSE
!           If decrease in SW due to drainage, include all
            SWTEMP(L) = SW(L) + SWDELTS(L)
          ENDIF

!         If any layer in top 100 cm is wet, use wet profile method
          IF (MEANDEP(L) < 100. .AND. SWTEMP(L) > DUL(L)) THEN
            ProfileType = 1
          ENDIF
        ENDDO

!       If wet profile, check for top layer SW below threshold.
        IF (ProfileType == 1) THEN
!         SW_threshold = DUL(1) - 0.05 !/ 0.13 * (DUL(1) - LL(1))
!         JTR 6/4/2008
!         Threshold WC = 0.275*DUL +1.165*DUL^2 + (1.2*DUL^3.75)*depth (center)
          SW_threshold = 0.275*DUL(1) + 1.165*DUL(1)*DUL(1) +
     &            (1.2*DUL(1)**3.75)*MEANDEP(1)
!         chp 6/4/2008 use DUL - 0.05, like before, but limit to air dry
!          SW_threshold = MAX(SWAD(1), DUL(1) - 0.05)
          IF (SWTEMP(1) < SW_threshold) THEN
            ProfileType = 2
          ENDIF
        ENDIF

      DO L = 1, NLAYR
!-----------------------------------------------------------------------
        SELECT CASE (ProfileType)

!       Dry profile
        CASE (3)
!         Depth-dependant coefficients based on Ritchie spreadsheet 11/29/2006
          A =  0.5  + 0.24 * DUL(L)
          B = -2.04 + 0.20 * DUL(L)
          ES_Coef(L) = A * MEANDEP(L) ** B

!       Equilibrium profile
        CASE (2)
          ES_Coef(L) = 0.011   !for all depths

!       Wet profile
        CASE (1)
!         Ritchie spreadsheet of 5/28/08
          A = 0.26  !6/20/08  A = 0.14  !6/2/08  A = 0.42   !4/18/08
          B = -0.70 !6/20/08  B = -0.46 !6/2/08  B = -0.73  !4/18/08
          ES_Coef(L) = A * MEANDEP(L) ** B !function, no integration

        END SELECT
!-----------------------------------------------------------------------

        SWDELTU(L) = -(SWTEMP(L) - SWAD(L)) * ES_Coef(L) !mm3/mm3

!       Apply the fraction of plastic mulch coverage
        IF (PMFRACTION(0) .GT. 1.E-6) THEN
          SWDELTU(L) = SWDELTU(L) * (1.0 - PMFRACTION(0))
        END IF

!       Limit to available water
        SW_AVAIL(L) = SW(L) + SWDELTS(L) - SWAD(L)
        IF (-SWDELTU(L) > SW_AVAIL(L)) THEN
          SWDELTU(L) = -SW_AVAIL(L)                   !mm3/mm3
        ENDIF

!       Limit to negative values (decrease SW)
        SWDELTU(L) = AMIN1(0.0, SWDELTU(L))

!         Aggregate soil evaporation from each cell.  Scale with half row
!           spacing.
          ES_mm(Row,Col) = -SWDELTU(Row,Col) / mm_2_vf(Row,Col)
          ES_LYR(L) = ES_LYR(L) + ES_mm(Row,Col)
          ES = ES + ES_mm(Row,Col)         !profile sum (mm)
      ENDDO

!     Limit total profile soil evaporation to potential soil evaporation
      RedFac = 1.0
      If (ES > EOS) Then
        RedFac = EOS / ES
        ES_mm = ES_mm * RedFac
        ES_LYR = ES_LYR * RedFac
        SWDELTU = SWDELTU * RedFac
        ES = EOS
      End If

      UPFLOW = 0.0
      ES = 0.0
      UPFLOW(NLAYR) = ES_LYR(NLAYR) / 10.
      DO L = NLAYR-1, 1, -1
        UPFLOW(L) = UPFLOW(L+1) + ES_LYR(L) / 10.     !cm/d
        ES = ES + ES_LYR(L)                           !profile sum (mm)
      ENDDO

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE ESR_SoilEvap
!=======================================================================

