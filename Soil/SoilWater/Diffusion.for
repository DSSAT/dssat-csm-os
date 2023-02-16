C=======================================================================
C  Diffusion, Subroutine, C.H.Porter
C  Calculates diffusion of soil water - upward and downward flow. 
C-----------------------------------------------------------------------
C  REVISION       HISTORY
!  2023-02-16 CHP extracted the vertical diffusion code from the 2D 
!                 Drainage.for module.
C=======================================================================

      SUBROUTINE Drainage_2D(DYNAMIC, 
     &    ActWTD, Diffus, Kunsat,             !Input
     &    SOILPROP, SWV_D, WCr,             !Input
     &    LatFlow_ts, SWV_ts, SWFh_ts, SWFv_ts)       !Output

!-----------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE
      SAVE

!-----------------------------------------------------------------------
!     Interface variables:
!-----------------------------------------------------------------------
!     Input:
      REAL, INTENT(IN) :: ActWTD
      TYPE (SoilType), INTENT(IN) :: SOILPROP
      Double Precision, DIMENSION(MaxRows,MaxCols), INTENT(IN) :: SWV_D
      TYPE(CellType), DIMENSION(MaxRows,MaxCols), INTENT(IN) :: CELLS
      Double Precision, DIMENSION(MaxRows,MaxCols), INTENT(OUT) ::SWV_ts
      REAL, DIMENSION(MaxRows,MaxCols), INTENT(OUT) :: SWFh_ts, SWFv_ts
      REAL, INTENT(OUT) :: LatFlow_ts

!-----------------------------------------------------------------------
      CHARACTER*6, PARAMETER :: ERRKEY = 'Diffus' 

      INTEGER i, j, NLAYR, LIMIT_2D
      INTEGER, DIMENSION(MaxRows,MaxCols) :: Cell_type
      REAL DiffusH, DiffusV, HalfRow
      REAL Se1, Se2, KunsatV
      Double Precision Diff_limit, DeltaTheta, Grav_limit, V_diff,V_grav
      REAL, DIMENSION(NL) :: DLAYR, DUL, DS, SAT, WCr
      REAL, DIMENSION(MaxRows,MaxCols) :: CellArea, Diffus
      REAL, DIMENSION(MaxRows,MaxCols) :: Kunsat, Thick
      REAL, DIMENSION(MaxRows,MaxCols) :: Width
      Double Precision, Dimension(MaxRows,MaxCols+1) :: H_in, H_out
      Double Precision, Dimension(MaxRows+1,MaxCols) :: V_in, V_out
!     REAL, DIMENSION(NL) :: Ksat

!     Water table
      Logical WatTable
!     REAL FracWT, TargetSWV !, Deficit
!     REAL Bottom, Top
      
!***********************************************************************
!***********************************************************************
!     SEASONAL INITIALIZATION
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!     Transfer values from constructed data types into local variables.
      DUL   = SOILPROP % DUL 
      DLAYR = SOILPROP % DLAYR
      NLAYR = SOILPROP % NLAYR
      SAT   = SOILPROP % SAT
      DS    = SOILPROP % DS
      LIMIT_2D = BedDimension % LIMIT_2D
!     Set process rates to zero.
      SWFh_ts = 0.0
      SWFv_ts = 0.0
      
      HalfRow = BedDimension % ROWSPC_cm / 2.
      
      Thick = CELLS % Struc % Thick
      Width = CELLS % Struc % Width
      CellArea = CELLS % Struc % CellArea
      Cell_Type = CELLS % Struc % CellType
       
      LatFlow_ts = 0.0
      WatTable = .FALSE.

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
      LatFlow_ts = 0.0

      SWFh_ts = 0.0
      SWFv_ts = 0.0

      H_in  = 0.0
      H_out = 0.0
      V_in  = 0.0
      V_out = 0.0
      WatTable = .FALSE.
!     At the beginning of the day, watertable_2D was called, thus LIMIT_2D has been defined  
      LIMIT_2D = BedDimension % LIMIT_2D  !Instead of Limit_2D, use ActWTD
!      DO i = LIMIT_2D + 1,  NLayr
!        if (LIMIT_2D .GE. NLayr) exit 
!       Check for water table in this layer
!       IF (DS(i) > MgmtWTD) THEN 
!          WatTable = .TRUE.
!          Bottom = DS(i)
!          IF (i == 1) THEN
!            Top = 0.
!          ELSE
!            Top = DS(i-1)
!          ENDIF
!          IF (MgmtWTD > Top) THEN
            ! Water table surface is in this layer
            !iWatTable = i
!           FracWT = (Bottom - MgmtWTD) / DLAYR(i)
!            TargetSWV = FracWT * SAT(i) + (1. - FracWT) * ThetaCap(i)
!          ELSE
            !For the layers below water table
!            TargetSWV = SAT(i)
!          ENDIF

!        ELSE
!          WatTable = .FALSE.
!          if (i .GT. LIMIT_2D) TargetSWV =  ThetaCap(i)
!        ENDIF
!      End do
      
      DO i = 1, min(LIMIT_2D,  NLayr)
        DO j = 1, NColsTot

          SELECT CASE(CELLS(i,j)%STRUC%CellType)
          CASE (3,4,5);CONTINUE
          CASE DEFAULT; CYCLE
          END SELECT

!         -------------------------------------------------------------
!         Horizontal flow - diffusion
          IF (j == 1) THEN
!           No lateral flow from center (symmetry)
            H_in(i,j) = 0.0
          ENDIF
  
          IF ((Cell_Type(i,j) == 3 .AND. j == FurCol1-1) .OR.
     &        (Cell_Type(i,j) == 5 .AND. j == NColsTot)) THEN
!           No lateral flow into furrow from bed (plastic cover) and
!           no lateral flow at centerline of furrow (symmetry)
            H_out(i,j) = 0.0
          ELSE
!!           Use arithmetic mean of diffusion coefficients in adjacent cells
!            DiffusH = 0.5 * (Diffus(i,j) + Diffus(i,j+1)) 
!           Use only diffus from cell (i,j)
!           DiffusH =Diffus(i,j) 
            DiffusH = SQRT(Diffus(i,j) * Diffus(i,j+1))

!           Horizontal flow in cm3/cm[row length], total for this time step
!           Driving force for homogeneous cells is difference in water content:
            DeltaTheta = SWV_D(i,j) - SWV_D(i,j+1)

            H_out(i,j) =  DiffusH / ((Width(i,j) + Width(i,j+1)) * 0.5)
!               cm2    =   cm2/hr /     cm             
     &                   * DeltaTheta  *  Thick(i,j) * (TimeIncr / 60.)
!                        *   mm3/mm3   *     cm      *       hr

!           Limit diffusion to available water
            IF (H_out(i,j) > 1.E-10) THEN
!             movement from column j to column j+1
              Diff_limit = MAX(0.0, (SWV_D(i,j) - WCr(i))*CellArea(i,j))
              H_out(i,j) = MIN (H_out(i,j), Diff_limit)
            ELSEIF (H_out(i,j) < -1.E-10) THEN
!             movement from column j+1 to column j
              Diff_limit = MAX(0.0, 
     &                    (SWV_D(i,j+1) - WCr(i)) * CellArea(i,j+1))
              H_out(i,j) = MAX(H_out(i,j), -Diff_limit)
            ELSE
              H_out(i,j) = 0.0
            ENDIF

            H_in(i,j+1) = H_out(i,j)
          ENDIF ! End of if Cell_Type

!         -------------------------------------------------------------
!         Vertical flow - diffusion and gravity 
          
          IF (i == 1) THEN
!           No vertical flow from top (irrig and infiltration already added)
            V_in(i,j) = 0.0
          ENDIF

!         Gravity flow from layer bottom to i+1 layer
!         KunsatV = Kunsat(i,j)
          IF (i < NRowsTot) THEN
            KunsatV = SQRT(Kunsat(i,j) * Kunsat(i+1,j))
          ELSE
            KunsatV = Kunsat(i,j)
          ENDIF

          V_grav = KunsatV * Width(i,j) * (TimeIncr / 60.) 
!          V_grav = Ksat(i) * Width(i,j) * (TimeIncr / 60.) 
!           cm2  =   cm/hr *     cm     *         hr  

!         Limit gravity flow to amount > DUL
          Grav_limit = MAX (0.0, (SWV_D(i,j) - DUL(i)) * CellArea(i,j))
!     &          + H_in(i,j) + V_in(i,j))
          V_grav = MIN(V_grav, Grav_limit)

!         If this layer is within the water table, then no vertical drainage
          !IF (WatTable) THEN
          !  V_grav = 0.0
          !ENDIF
          
!         Vertical diffusion flow in cm3/cm[row length], total for this time step
!         For water table, use LIMIT_2D+1 to allow diffusion across 2D boundary.
          IF (i < min(LIMIT_2D+1, NRowsTot)) THEN  
!           DiffusV = Diffus(i,j) 
            DiffusV = SQRT(Diffus(i,j) * Diffus(i+1,j))

!           Driving force for non-homogeneous soils based on normalized water content
            Se1 = (SWV_D(i,j) - WCr(i)) / (SAT(i) - WCr(i))
            Se2 = (SWV_D(i+1,j) - WCr(i+1)) / (SAT(i+1) - WCr(i+1))
            DeltaTheta = (Se1 - Se2) * 
     &                 0.5 * (SAT(i) - WCr(i) + SAT(i+1) - WCr(i+1))

            V_diff =  DiffusV / ((Thick(i,j) + Thick(i+1,j)) * 0.5)
!             cm2  =   cm2/hr /         cm               
     &                * DeltaTheta  *  Width(i,j) * (TimeIncr / 60.)
!                     *   mm3/mm3   *     cm      *         hr

!           Limit diffusion to available water, after gravity flow
            IF (V_diff > 1.E-10) THEN
!             downward flow to cell below
!             limit of flow includes existing available water 
!               + inflow from above - gravity flow to cell below.
              Diff_limit = MAX (0.0, 
     &          (SWV_D(i,j) - WCr(i)) * CellArea(i,j) !avail water 
     &          + V_in(i,j) - V_grav)               ! + inflow - outflow
              V_diff = MIN(V_diff, Diff_limit)
            ELSEIF (V_diff < -1.E-10) THEN
!             upward flow from cell below
!             limit to available water in cell below
              Diff_limit = MAX(0.0, 
     &                  (SWV_D(i+1,j) - WCr(i+1)) * CellArea(i+1,j))
              V_diff = MAX(V_diff, -Diff_limit)
!             Should set upper flow limit (or negative H_out limit) as SAT-SWV_D ?

            ELSE
!             Zero vertical diffusion rate
              V_diff = 0.0
            ENDIF

            V_out(i,j) = V_diff + V_grav

!           Check for water table in cell below
            !IF (DS(i+1) > MgmtWTD) THEN
            IF (i .eq. LIMIT_2D) THEN ! If there is water table
!             Water table found - compute lateral flows
              V_in(i+1,j) = 0.0  !no net change to cell below
!             Lateral inflow is positive, outflow is negative
              IF (V_out(i,j) > 1.E-10) THEN
!               Flow into water table from above - lateral outflow to balance
                LatFlow_ts = LatFlow_ts - V_out(i,j)   !cm2
              ELSEIF (V_out(i,j) < -1.E-10) THEN
!               Flow up from water table cell - lateral inflow to balance
!               What is the difference for this two cases?
                LatFlow_ts = LatFlow_ts - V_out(i,j)   !cm2 
              ENDIF
            Endif

          ELSE
!           If there is no water table, this is Last row - gravity only, no diffusion. 
!           If there is water table, this is layer LIMIT_2D+1 
            V_diff = 0.0
            V_out(i,j) = V_grav
          ENDIF !for Row condition
          
           SWV_ts(i,j) = SWV_D(i,j) + 
     &                (H_in(i,j) - H_out(i,j) + V_in(i,j) - V_out(i,j)) 
     &                 / CellArea(i,j)
          
!         Limit vertical Flow not making next layer above saturation
          if ((SWV_ts(i,j).GT. SAT(i)).and.(i.GT.1).and.(j.GT.1)) then 
             if (i.LT. LIMIT_2D) then 
!              If water is down flow shall we set V_in_hold? How to send the extra water back to above layer?
!              Do not assume to flow out horizentally
!            if (V_out(i,j) .LE. 0.) then
               V_out(i,j) = V_out(i,j) + 
     &                     (SWV_ts(i,j) - SAT(i)) * CellArea(i,j)
               SWV_ts(i,j) = SAT(i)
             elseif (i.eq.LIMIT_2D) then 
!              if today's water table is lower then yesterday, to avoid the SW in layer LIMIT_2D to go to above SAT 
               LatFlow_ts = LatFlow_ts -
     &          (SWV_ts(LIMIT_2D,j)-SAT(LIMIT_2D))* CellArea(LIMIT_2D,j)
               SWV_ts(LIMIT_2D,j) = SAT(LIMIT_2D)
               SWFv_ts(LIMIT_2D,j) = SWFv_ts(LIMIT_2D,j) +
     &          (SWV_ts(LIMIT_2D,j)-SAT(LIMIT_2D))* CellArea(LIMIT_2D,j)
             Endif
        !    Endif
          Endif
          V_in(i+1,j) = V_out(i,j)
         
         
!         Cells within water table should remain saturated
! This subroutine does not calculate SWV_ts(i,j) below LIMIT_2D, thus remove the following
        !  IF (WatTable) THEN
        !    Deficit = (TargetSWV - SWV_ts(i,j)) * CellArea(i,j)
        !    LatFlow_ts = LatFlow_ts + Deficit
        !    SWV_ts(i,j) = TargetSWV
        !  ENDIF

          SWFh_ts(i,j) = H_out(i,j) !cm2
          SWFv_ts(i,j) = V_out(i,j) !cm2

!         temp chp
          Cell_detail%H_in(i,j)  = H_in(i,j)
          Cell_detail%H_out(i,j) = H_out(i,j)
          Cell_detail%V_in(i,j)  = V_in(i,j)
          Cell_detail%V_out(i,j) = V_out(i,j)
          Cell_detail%Vdiff(i,j) = V_diff
          Cell_detail%Vgrav(i,j) = V_grav
        ENDDO ! Drainage column by column
      ENDDO ! Drainage row by row

!     Below LIMIT_2D, set subroutine output water content equal to subroutine input water content 
      if (LIMIT_2D .LT. NLayr) then 
        DO i = LIMIT_2D+1, NLAYR
          DO j = 1, NColsTot
            SWV_ts(i,j) = SWV_D(i,j)
          Enddo
        Enddo
      endif
      
      LatFlow_ts = LatFlow_ts / HalfRow * 10.   !mm

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------

      RETURN
      END SUBROUTINE Drainage_2D
!=====================================================================

C=====================================================================
!     Drainage_2D VARIABLE DEFINITIONS: (updated Oct 2009)
!-----------------------------------------------------------------------
! DiffusH    Horizontal diffusivity in cm2/hr
! DiffusV    Vertical diffusivity in cm2/hr
! Diff_limit Maximum water which can be removed from cell by diffusion cm2
! DS(i)      Depth of buttom of ith layer
! Grav_limit Maximum water which can be removed from cell by gravity flow cm2
! H_in(Row,Col)    2D Horizontal water amount into cell in current time step in cm2
! H_out(Row,Col)   2D Horizontal water amount out of cell in current time step in cm2
! LatFlow_ts    Total inward lat flow for all cells for current time step. It is in mm finally
! K_unsat(Row,Col) Un-saturated hydraulic conductivity in cm/hr
! SWFh_ts(Row,Col) 2D Horizontal water amount out of cell in current time step in cm2
! SWFv_ts(Row,Col) 2D Vertical water amount out of cell in current time step in cm2
! SWV_D(Row,Col)   Double precision soil water content at the beginning of time step in cm2
! SWV_ts(Row,Col)  Soil water content at the end of time step in cm2
! TargetSWV  Balanced soil water content at given hight
! ThetaCap An array of volumetric soil water contents at the midpoint of each soil layer.
!          Calculated from the water characteristic curve at the height above the
!          water table. At equilibrium state, soil matric potential h = water table depth z
! Thick(Row,Col) Cell thickness in cm[soil]
! TimeIncr       Dynamic time step within a day in min.
! V_diff  in cm2 Vertical water amount into cell due to diffusivity in current time step in cm2
! V_grav in cm2  Vertical water amount into cell due to gravity in current time step in cm2
! V_in(Row,Col)  2D Vertical water amount into cell in current time step in cm2
! V_out(Row,Col) 2D Vertical water amount out of cell in current time step in cm2
!-----------------------------------------------------------------------
!     END SUBROUTINE Drainage_2D
!=======================================================================
