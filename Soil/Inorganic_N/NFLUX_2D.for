!***********************************************************************
!  NFLUX_2D, subroutine
!
!  Purpose: Determines movement of nitrogen with water flow.
!
!  Revision history:
!  ....     ...  Written
!  05/31/2010 CHP/JZW  Written based on NFLUX
!  08/15/2011 Add arguments NFlux_L, NFlux_R, NFlux_D, NFlux_U
!***********************************************************************

      SUBROUTINE NFLUX_2D (DYNAMIC,
     &  ADCOEF, BD, CELLS, ColFrac, DUL, NORIG,                 !Input
     &  NSOURCE, SWV,                                           !Input
     &  DLTN, TLCH, TLCHD, NFlux_L, NFlux_R, NFlux_D, NFlux_U)  !Output

!     ------------------------------------------------------------------
      USE Cells_2D
      IMPLICIT  NONE
      SAVE
!     ------------------------------------------------------------------

      INTEGER L, DYNAMIC, NSOURCE

      REAL TLCH, TLCHD, WFluxFrac !, NTEMP_AVG
      REAL, DIMENSION(NL) :: ADCOEF, BD, DUL, FRAC_SOLN
      REAL, DIMENSION(MaxCols) :: NLeach
      REAL, DIMENSION(MaxRows,MaxCols) :: CellArea, DLTN, ColFrac,
     &     NFlux_L, NFlux_R, NFlux_D, NFlux_U, NORIG, NTEMP,
     &     SWFlux_L, SWFlux_R, SWFlux_D, SWFlux_U, SWV
      INTEGER, DIMENSION(MaxRows,MaxCols) :: Cell_Type 

!     2D variables:
      INTEGER j, FurRow1, FurCol1

      TYPE (CellType) CELLS(MaxRows,MaxCols)

!     TEMP CHP
!      REAL TotN, Sum_n2, TotNLast

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      CellArea = CELLS % STRUC % CellArea
      FurRow1 = BedDimension % FurRow1
      FurCol1 = BedDimension % FurCol1
      TLCH = 0.0

      Cell_Type = CELLS%STRUC%Cell_Type

!     temp chp
!      TotN = 0.0

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!     ------------------------------------------------------------------
!     Daily initialization of the N flow.
      NFlux_L = 0.0
      NFlux_R = 0.0
      NFlux_D = 0.0
      NFlux_U = 0.0

      SWFlux_L = CELLS % RATE % SWFlux_L
      SWFlux_R = CELLS % RATE % SWFlux_R
      SWFlux_D = CELLS % RATE % SWFlux_D
      SWFlux_U = CELLS % RATE % SWFlux_U

!     TEMP CHP
!      TotNLast = TotN
!      TOTN = SUM_N2(Cell_Type, NORIG, ColFrac)

!     ------------------------------------------------------------------
!     FRAC_SOLN is the fraction of N in a layer that is in solution
!     and can move with the water flux. ADCOEF is the anion adsorption
!     coefficient of the layer (0.0 = no adsorption, hence no 
!     retarded movement of N)
      IF (NSOURCE == 1) THEN 
        FRAC_SOLN = 1.  !urea
      ELSE              !NO3
        DO L = 1, NRowsTot
          FRAC_SOLN(L) = 1. / (1. + BD(L) * ADCOEF(L) / DUL(L)) 
        ENDDO
      ENDIF

!     ------------------------------------------------------------------
!     Include DeltaN in calculations to prevent negatives
      DO L = 1, NRowsTot
        DO j = 1, NColsTot
          NTEMP(L,j) = MAX(0.0, NORIG(L,j) + DLTN(L,j))    !kg[N]/ha
        ENDDO
      ENDDO

!     TEMP CHP
!      TotNLast = TotN
!      TOTN = SUM_N2(Cell_Type, NTEMP, ColFrac)

!     ------------------------------------------------------------------
!     Move N down and right - start from the top, left
      DO L = 1, NRowsTot
        DO j = 1, NColsTot
          IF (Cell_Type(L,j) > 5 .OR. Cell_Type(L,j) < 3) CYCLE

!     ------------------------------------------------------------------
!         To allow N movement across several soil layers within one time
!         step after a big rain event, a temporary integration of the
!         incoming flow from above and from the left is done for the
!         water flow, so that the updated state variable applies for L,j
!         the flow out of this layer to the right and downward. 
!     ------------------------------------------------------------------
!         N flux (kg[N]/ha) is proportional to water flux (cm2)
!         --------------------------------------------------------------
!         N Flux to the right (all soil cells except right boundary)
          IF ((Cell_Type(L,j) == 3 .AND. j < FurCol1 - 1)  .OR. 
     &        (Cell_Type(L,j) == 4 .AND. j < NColsTot) .OR. 
     &        (Cell_Type(L,j) == 5 .AND. j < NColsTot)) THEN
 
          IF (SWFlux_R(L,j) .EQ. 0) then
            Nflux_R(L,j) = 0
          ELSE
            WFluxFrac = MAX(0.0, MIN(1.0,
     &        SWFlux_R(L,j) / (SWV(L,j) * CellArea(L,j))))
            Nflux_R(L,j) = MAX(0.0, NTEMP(L,j) * FRAC_SOLN(L))*WFluxFrac
     &        * SWFlux_R(L,j) / 
     &   (SWFlux_R(L,j) + SWFlux_L(L,j) + SWFlux_D(L,j) + SWFlux_U(L,j))

            NTEMP(L,j)   = NTEMP(L,j)   - Nflux_R(L,j)
            NTEMP(L,j+1) = NTEMP(L,j+1) + Nflux_R(L,j) 
!     &                      * ColFrac(L, j) / ColFrac(L, j+1)
          ENDIF

!!           Check that we are not concentrating N at the boundaries
!            IF (Nflux_R(L,j) > 1.E-6 .AND. NTEMP(L,j+1) > NTEMP(L,j)) 
!     &                                                              THEN
!              NTEMP_AVG = (NTEMP(L,j)   * ColFrac(j)   + 
!     &                     NTEMP(L,j+1) * ColFrac(j+1))
!     &                  / (ColFrac(j) + ColFrac(j+1))
!              NTEMP(L,j)   = NTEMP_AVG
!              NTEMP(L,j+1) = NTEMP_AVG
!            ENDIF

           IF (ABS(NTEMP(L,j))   < 1.E-20) NTEMP(L,j)   = 0.0
           IF (ABS(NTEMP(L,j+1)) < 1.E-20) NTEMP(L,j+1) = 0.0
          ENDIF

!     TEMP CHP
!      TotNLast = TotN
!      TOTN = SUM_N2(Cell_Type, NTEMP, ColFrac)
!      continue
!         --------------------------------------------------------------
!         N Flux downward (all soil cells)
          IF (SWFlux_D(L,j) .EQ. 0) THEN
            Nflux_D(L,j) = 0
          ELSE
            WFluxFrac = MAX(0.0, MIN(1.0,
     &        SWFlux_D(L,j) / (SWV(L,j) * CellArea(L,j))))
            Nflux_D(L,j) = MAX(0.0, NTEMP(L,j) * FRAC_SOLN(L)) 
     &          * WFluxFrac * SWFlux_D(L,j) / 
     &         (SWFlux_R(L,j) + SWFlux_L(L,j) + 
     &          SWFlux_D(L,j) + SWFlux_U(L,j))

            NTEMP(L,j)   = NTEMP(L,j)   - Nflux_D(L,j)
            IF (L < NRowsTot) THEN
              NTEMP(L+1,j) = NTEMP(L+1,j) + Nflux_D(L,j) 
!     &            * ColFrac(L, j) / ColFrac(L + 1, j)
            ELSE
!             Accumulate the N lost by leaching below profile depth.
              NLeach(j) = NFlux_D(NRowsTot,j)
!             Today's leached N
              TLCHD = TLCHD + NLeach(j) * 2.0
            ENDIF
          ENDIF

!     TEMP CHP
!      TotNLast = TotN
!      TOTN = SUM_N2(Cell_Type, NTEMP, ColFrac)
!      continue

        ENDDO
      ENDDO

!     ------------------------------------------------------------------
!     Move N up and left - start from the bottom, right
      DO L = NRowsTot, 1, -1
        DO j = NColsTot, 1, -1
          IF (Cell_Type(L,j) > 5 .OR. Cell_Type(L,j) < 3) CYCLE
!         --------------------------------------------------------------
!         Calculate movement to the left (all soil cells except left boundary)
          IF (j > 1) THEN
            IF (SWFlux_L(L,j) .EQ. 0) THEN
              Nflux_L(L,j) = 0
            ELSE
              WFluxFrac = MAX(0.0, MIN(1.0,
     &          SWFlux_L(L,j) / (SWV(L,j) * CellArea(L,j))))
              Nflux_L(L,j) = MAX(0.0, NTEMP(L,j) * FRAC_SOLN(L))*
     &            WFluxFrac
     &          * SWFlux_R(L,j) / (SWFlux_R(L,j) + SWFlux_L(L,j) +
     &            SWFlux_D(L,j) + SWFlux_U(L,j))
            
              NTEMP(L,j)   = NTEMP(L,j)   - Nflux_L(L,j)
              NTEMP(L,j-1) = NTEMP(L,j-1) + Nflux_L(L,j)
!     &                                  * ColFrac(L,j) / ColFrac(L,j-1)

!           Check that we are not moving N from low to high concentration
!            IF (NFlux_L(L,j) > 1.E-6 .AND. NTEMP(L,j-1) > NTEMP(L,j)) 
!     &                                                              THEN
!              NTEMP_AVG = (NTEMP(L,j)   * ColFrac(L,j)   + 
!     &                     NTEMP(L,j-1) * ColFrac(L,j-1))
!     &                  / (ColFrac(L,j) + ColFrac(L,j-1))
!              NTEMP(L,j)   = NTEMP_AVG
!              NTEMP(L,j-1) = NTEMP_AVG
!            ENDIF

              IF (ABS(NTEMP(L,j))   < 1.E-20) NTEMP(L,j)   = 0.0
              IF (ABS(NTEMP(L,j-1)) < 1.E-20) NTEMP(L,j-1) = 0.0
            ENDIF
          ENDIF


!     TEMP CHP
!      TotNLast = TotN
!      TOTN = SUM_N2(Cell_Type, NTEMP, ColFrac)
!      continue


!         --------------------------------------------------------------
!         Calculate movement upward (all soil cells except top boundary)
          IF (L > 1) THEN
            IF (Cell_Type(L-1, j) > 2 .AND. Cell_Type(L-1, j) < 6) THEN
              IF (SWFlux_U(L,j) .EQ. 0) THEN
                Nflux_U(L,j) = 0
              ELSE
                  WFluxFrac = MAX(0.0, MIN(1.0,
     &                SWFlux_U(L,j) / (SWV(L,j) * CellArea(L,j))))
                  Nflux_U(L,j) = MAX(0.0, NTEMP(L,j) * FRAC_SOLN(L))
     &              *WFluxFrac * SWFlux_R(L,j) / 
     &              (SWFlux_R(L,j) + SWFlux_L(L,j) + 
     &              SWFlux_D(L,j) + SWFlux_U(L,j))

                  NTEMP(L,j)   = NTEMP(L,j)   - Nflux_U(L,j)
                  NTEMP(L-1,j) = NTEMP(L-1,j) + Nflux_U(L,j) !* 
!     &              ColFrac(L, j) / ColFrac(L - 1, j)
              ENDIF
            ENDIF
          ENDIF

!     TEMP CHP
!      TotNLast = TotN
!      TOTN = SUM_N2(Cell_Type, NTEMP, ColFrac)
!      continue

        ENDDO
      ENDDO

!     ------------------------------------------------------------------
!     DLTxxx section.
!     ------------------------------------------------------------------
!     Set the DLTN rate variable.
      DO j = 1, NColsTot
        DO L = 1, NRowsTot
          DLTN(L,j) = NTEMP(L,j) - NORIG(L,j)    !kg[N]/ha
        ENDDO
      ENDDO   
      TLCH = TLCH + TLCHD  !Season cumulative

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE NFLUX_2D

!==========================================================================
! NFLUX_2D Variable List
!==========================================================================
! ADCOEF(L)      Anion adsorption coefficient for soil layer L;  for 
!                  reduced anion (nitrate) flow in variable-charge soils 
!                  (ADCOEF = 0 implies no anion retention)
!                  (cm3 (H2O] / g [soil])
! BD(L)          Bulk density, soil layer L (g [soil] / cm3 [soil])
! DLTN(L)        Rate of change of N in soil layer L (kg [N] / ha)
! DRN(L)         Drainage rate through soil layer L (cm/d)
! DUL(L)         Volumetric soil water content at Drained Upper Limit in 
!                  soil layer L (cm3[water]/cm3[soil])
! FRAC_SOLN(L)   Fraction of NO3 in a layer that is in solution and can 
!                  move with the water flux 
! NORIG(L)       N content in soil layer L before drainage (Urea or NO3)
!                  (kg [N] / ha)
! NSOURCE        Flag for N source (1 = urea, 2 = NO3) 
! NTEMP          N variable for temporary integration step (kg [N] / ha)
! SWV(L,j)       Volumetric soil water content in layer L
!                  (cm3 [water] / cm3 [soil])
! TLCH           Total N leached from soil (kg [N] / ha)
!==========================================================================

