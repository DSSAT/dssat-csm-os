!=======================================================================
!  SALUS SOIL EVAPORATION MODULE - File ESR_SoilEvap.FOR
!=======================================================================
!  ESR_SoilEvap_2D, Subroutine, J. Ritchie, C. Porter
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
      SUBROUTINE ESR_SoilEvap_2D(DYNAMIC,   
     &   CELLS, EOS, SOILPROP_FURROW,                     !Input
     &   ES, ES_mm)                                       !Output

!-----------------------------------------------------------------------
      USE Cells_2D; USE ModuleData
      IMPLICIT NONE
      SAVE

!     ------------------------------------------------
!     Interface Variables:
      TYPE(CellType), DIMENSION(MaxRows,MaxCols) :: CELLS
      REAL, INTENT(IN) :: EOS          !Potential soil evap (mm/d)
      TYPE (SoilType), INTENT(IN) :: SOILPROP_FURROW !Soil properties
      
      REAL, INTENT(OUT):: ES           !Actual soil evaporation (mm/d)
!     ------------------------------------------------

      INTEGER L, NLAYR, ProfileType, jj
      REAL A, B, RedFac, SW_threshold
      REAL, DIMENSION(NL) :: DLAYR, DS, DUL, LL, MEANDEP, SWAD
      REAL, DIMENSION(NL) :: SWTEMP, SW_AVAIL, ES_Coef
      
!     2D additions:
      INTEGER Col, FurRow1, FurCol1, Row, DYNAMIC
      REAL, DIMENSION(MaxRows, MaxCols) :: ES_mm, mm_2_vf, SWDELTU
      CHARACTER*6, PARAMETER :: ERRKEY = 'EVAP2D'

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      DLAYR = SOILPROP_FURROW % DLAYR
      DS    = SOILPROP_FURROW % DS
      DUL   = SOILPROP_FURROW % DUL
      LL    = SOILPROP_FURROW % LL
      NLAYR = SOILPROP_FURROW % NLAYR
      SWAD  = SOILPROP_Furrow % WCR

      FurRow1 = BedDimension % FurRow1
      FurCol1 = BedDimension % FurCol1
      mm_2_vf = BedDimension % mm_2_vf

      ES = 0.0
      ES_mm = 0.0
      
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

!-----------------------------------------------------------------------
      ES = 0.0
      ES_mm = 0.0
      
!     Compute separate soil evaporation for each soil column
      IF (BedDimension % PMCover) then
!       If there is plastic cover, the infiltration is in the furrow
        jj = FurCol1 
      else
        jj = 1
      endif
      ! DO Col = FurCol1, NColsTot
      DO Col = jj, NColsTot
        
!**********************************************************************
        ProfileType = 3   !assume dry profile until proven wet
        DO L = 1, NLAYR
          Row = L + FurRow1 - 1
          SWTEMP(L) = CELLS(Row,Col)%State%SWV
          
!         Air dry water content 
!         01/23/2010 CHP replace with residual water content
!         SWAD(L) = 0.30 * LL(L) !JTR 11/28/2006
      
!         Mean depth for each soil layer
          MEANDEP(L) = DS(L) - DLAYR(L) / 2.               !cm
      
!         If any layer in top 100 cm is wet, use wet profile method
          IF (MEANDEP(L) < 100. .AND. SWTEMP(L) > DUL(L)) THEN
            ProfileType = 1
          ENDIF
        ENDDO
      
!       If wet profile, check for top layer SW below threshold.
        IF (ProfileType == 1) THEN
          SW_threshold = 0.275*DUL(1) + 1.165*DUL(1)*DUL(1) + 
     &            (1.2*DUL(1)**3.75)*MEANDEP(1)
          IF (SWTEMP(1) < SW_threshold) THEN
            ProfileType = 2
          ENDIF
        ENDIF
      
        DO L = 1, NLAYR
          Row = L + FurRow1 - 1
!-------------------------------------------------------------------------
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
!-------------------------------------------------------------------------
      
          SWDELTU(Row,Col) = -(SWTEMP(L) - SWAD(L)) * ES_Coef(L) 
          !mm3/mm3
      
!         Limit to available water
          SW_AVAIL(L) = SWTEMP(L) - SWAD(L)
          IF (-SWDELTU(Row,Col) > SW_AVAIL(L)) THEN
            SWDELTU(Row,Col) = -SW_AVAIL(L)                   !mm3/mm3
          ENDIF
      
!         Limit to negative values (decrease SW)
          SWDELTU(Row,Col) = AMIN1(0.0, SWDELTU(Row,Col))
      
!         Aggregate soil evaporation from each cell.  Scale with half row
!           spacing.
          ES_mm(Row,Col) = -SWDELTU(Row,Col) / mm_2_vf(Row,Col)
          ES = ES + ES_mm(Row,Col)         !profile sum (mm)
        ENDDO
      ENDDO
      
!     Limit total profile soil evaporation to potential soil evaporation
      RedFac = 1.0
      If (ES > EOS) Then
        RedFac = EOS / ES
        ES_mm = ES_mm * RedFac
        SWDELTU = SWDELTU * RedFac
        ES = EOS
      End If
      
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE ESR_SoilEvap_2D
!=======================================================================

