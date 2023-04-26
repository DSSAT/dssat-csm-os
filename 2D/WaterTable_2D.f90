!=======================================================================
!  WaterTable_2D, Subroutine, C.H.Porter
!  Computes lateral flows necessary to maintain a managed water table depth.
!  Actual computed water table may rise above or fall below managed water
!  table. 
!  The 2D version collapses 2D soil water into 1D layers with average soil water content.
!  All water table calculations are done on 1D basis because there is little variation
!    across a row for very wet conditions.
!  The daily 1D soil water flux due to a change in water table is then converted to 2D cells.
!-----------------------------------------------------------------------
!  REVISION       HISTORY
!  01/23/2010 CHP Written
!  08/01/2011 CHP Add LIMIT_2D for water table handling
!  04/26/2023 CHP Use 1D WaterTable.f90, but modify to expand 1D fluxes to 2D cells.
!=======================================================================

  Subroutine WaterTable_2D(DYNAMIC,           &
    CELLS, SOILPROP, SoilProp_row, SWV,       &   !Input                                              
    ActWTD, LatInflow, LatOutflow,            &   !Output
    MgmtWTD, SWVDELTW)                            !Output

!-----------------------------------------------------------------------
  USE Cells_2D     
  USE ModuleData
  IMPLICIT NONE
  EXTERNAL CapFringe
  SAVE
!-----------------------------------------------------------------------
! Interface:
  INTEGER, INTENT(IN) :: DYNAMIC
  Type(CellType), DIMENSION(MaxRows,MaxCols), INTENT(IN) :: CELLS
  Type(SoilType), INTENT(IN) :: SOILPROP, SoilProp_row
  REAL, DIMENSION(MaxRows,MaxCols), INTENT(IN) :: SWV
  REAL, DIMENSION(MaxRows,MaxCols), INTENT(OUT) :: SWVDELTW
  REAL, INTENT(OUT) :: ActWTD, MgmtWTD, LatInflow, LatOutflow

! Local
  INTEGER i, j, L, NLAYR, LIMIT_2D 
  REAL Bottom, Top, Thick, TargetWTD, AdjustWTD
  REAL, DIMENSION(NL) :: DLAYR, DS, DUL, SAT, SW, WCR  
  REAL, DIMENSION(NL) :: ThetaCap, SW_temp, SWDELTW, DeltaSW
  REAL, DIMENSION(MaxRows,MaxCols) :: SWV_temp

    REAL, PARAMETER :: TOL = 0.5  !tolerance for target water table level (cm)
    REAL, PARAMETER :: Kd = 0.5   !drawdown coefficient (fraction/day)
!   Kd should be a user specified input in the Fields section of FILEX
!   based on field measurement. Or it could be calculated from soil 
!   properties. For now assume a constant value.  

!***********************************************************************
!***********************************************************************
    LatInflow  = 0.0
    LatOutflow = 0.0
    SWDELTW    = 0.0
    SWVDELTW   = 0.0
    SW_temp    = SW

!   Use cell water content to calculate layer sol water content 
    CALL Interpolate2Layers_2D(                         &
        SWV, CELLS%Struc, SOILPROP % NLAYR,             & !Input
        SW)                                               !Output

!   If modeling a raised bed, SOILPROP contains soil properties for raised bed plus below bed.
    DLAYR = SOILPROP % DLAYR
    DS    = SOILPROP % DS
    DUL   = SOILPROP % DUL
    NLAYR = SOILPROP % NLAYR
    SAT   = SOILPROP % SAT
    WCR   = SOILPROP % WCR

!***********************************************************************
!***********************************************************************
! Seasonal initialization - run once per season
!***********************************************************************
  IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!   Get initial depth to water table
    CALL GET('MGMT','ICWD',MgmtWTD)

!   Negative or zero value means no managed water table
    IF (MgmtWTD < 1.E-6) THEN
      MgmtWTD = 1000.
    ENDIF

!   If there is a raised bed, then need to adjust water table depths 
!   Assumptions:
!   - Recorded water table depths are relative to the soil surface prior to making a raised bed.
!   - The difference between the surface of the bed and the original surface is BEDHT - DIGDEP
    IF (BedDimension % RaisedBed) THEN
      AdjustWTD = BedDimension % BedHt - BedDimension % DigDep
    ELSE
      AdjustWTD = 0.0
    ENDIF

    MgmtWTD = MgmtWTD + AdjustWTD
    ActWTD    = MgmtWTD
    TargetWTD = MgmtWTD

!   Actual water table depth will equal either the managed water table depth or
!     the target water table depth. When user water table depth records change
!     a lot from one day to the next, it may take a few days for the actual 
!     water table depth to reach the managed depth.
    CALL PUT('WATER','WTDEP' ,ActWTD)  !Actual current water table depth
    CALL PUT('MGMT' ,'WATTAB',MgmtWTD) !Managed water table depth (i.e., user records)

    IF (MgmtWTD < DS(NLAYR)) THEN
!     Initialize soil water content with water table depth, starting at bottom of profile
      DO L = NLAYR, 1, -1
        IF (L == 1) THEN 
          Top = 0.
        ELSE
          Top = DS(L-1)
        ENDIF
        Bottom = DS(L)
        IF (MgmtWTD > Bottom) THEN
!         This layer is entirely above the managed water table; done.
!         Leave water content as read from initial conditions.
          EXIT
        ELSEIF (MgmtWTD > Top) THEN
!         This layer is partially in managed water table.  
!         Assume saturated water content below water table, DUL above.
          Thick = Bottom - MgmtWTD
          SW_TEMP(L) = (SAT(L) * Thick + DUL(L) * (DLAYR(L) - Thick)) / DLAYR(L)
          SW_TEMP(L) = MAX(SW_TEMP(L), SW(L))
        ELSE
!         This layer is entirely within managed water table.
          SW_TEMP(L) = SAT(L)
        ENDIF 
      ENDDO
    ENDIF

!***********************************************************************
!***********************************************************************
! DAILY RATE CALCULATIONS
!***********************************************************************
  ELSEIF (DYNAMIC .EQ. RATE) THEN

!   Get management depth to water table
    CALL GET('MGMT','WATTAB',MgmtWTD)
    MgmtWTD = MgmtWTD + AdjustWTD

    IF (MgmtWTD > DS(NLayr) .AND. ActWTD > DS(NLayr)) THEN
      RETURN
    ENDIF

!-----------------------------------------------------------------------
!   Compute lateral flow to maintain managed water table depth
!   Use tolerance in cm to get target water table depth close to management 
!   depth.  If the exact management depth is used, instabilities occur due
!   to soil water changes with other simultaneous processes.

!   Actual water table lower than management.  Lateral Inflow calculations
    IF (ActWTD - MgmtWTD > TOL) THEN

!     initial guess at today's water table depth
      TargetWTD = (ActWTD - MgmtWTD) * Kd + MgmtWTD

!     Water content below the target water table will be set to SAT. 
!     Calculate lateral inflow needed to raise water table to target depth
      DO L = NLAYR, 1, -1
        IF (L == 1) THEN 
          Top = 0.
        ELSE
          Top = DS(L-1)
        ENDIF
        Bottom = DS(L)
        IF (TargetWTD > Bottom) THEN
!         This layer is entirely above the target water table; done.
          EXIT
        ELSEIF (TargetWTD > Top) THEN
!         This layer is partially in target water table
          Thick = Bottom - TargetWTD
        ELSE
!         This layer is entirely within target water table.
          Thick = DLAYR(L)
        ENDIF 
        SWDELTW(L) = MAX(0.0,(SAT(L) - SW(L)) * Thick / DLAYR(L))
        LatInflow = LatInflow + SWDELTW(L) * DLAYR(L) * 10. !mm
      ENDDO

!-------------------------------------------------------------------------
!   Actual water table higher than management - drawdown using Kd
    ELSEIF (MgmtWTD - ActWTD > TOL .AND. MgmtWTD < 9999.) THEN
!     Calculate lateral outflow needed to draw water table down to specified depth
!     Water content above the target water table but within the old water table 
!       will be set to DUL. 
!     The capillary rise routine will then reset theta values just above the water table.

!     initial guess at today's water table depth
      TargetWTD = (ActWTD - MgmtWTD) * Kd + MgmtWTD

!     Calculate lateral outflow needed to lower water table to target depth
      DO L = 1, NLAYR
        IF (L == 1) THEN 
          Top = 0.
        ELSE
          Top = DS(L-1)
        ENDIF
        Bottom = DS(L)
        IF (TargetWTD > Bottom) THEN
!         This layer is entirely above the target water table. Set the 
!           water content to DUL in the entire layer.
          Thick = DLAYR(L)
        ELSEIF (TargetWTD > Top) THEN
!         This layer is partially in target water table. The top portion
!           of the layer (above the TargetWTD) is set to DUL.
          Thick = TargetWTD - Top
        ELSE
!         This layer is entirely within target water table. Do nothing 
!           because it was in the water table yesterday, too.
          EXIT
        ENDIF 
        SWDELTW(L) = MAX(0.0,-(SW(L) - DUL(L)) * Thick / DLAYR(L))
!       Lateral outflow values are negative (so are SWDELTW here)
        LatOutflow = LatOutflow + SWDELTW(L) * DLAYR(L) * 10. !mm 
      ENDDO

    ELSE
      TargetWTD = MgmtWTD
    ENDIF 

!-----------------------------------------------------------------------
!   Set actual water table depth equal to target calculated above.
!   Probably need a better way to do this, i.e., calculate the actual
!   water table depth based on water table and drawdown dynamics, but
!   previous attempts resulted in instability for daily model.
    ActWTD = TargetWTD
    CALL PUT('WATER','WTDEP',ActWTD)

!-------------------------------------------------------------------------
!   Update soil water content for 1D soil profile
!   Pseudo-integration, the real integration occurs in WATBAL
    DO L = 1, NLAYR
      SW_temp(L) = SW_temp(L) + SWDELTW(L)
    ENDDO

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
  ENDIF
!-----------------------------------------------------------------------

!   No effect of water table if it is below the bottom of the soil profile.
    IF (ActWTD .GE. DS(NLAYR)) THEN
      LIMIT_2D = NRowsTot

    ELSE
!     Calculate water content within capillary fringe, ThetaCap
      CALL CapFringe(           &
        ActWTD,  SOILPROP,      &   !Input
        ThetaCap)                   !Output

!     Update temporary soil water content with ThetaCap
      DO L = 1, NLAYR
!       ThetaCap(L) = MAX(SW_TEMP(L), ThetaCap(L))
        DeltaSW(L) = MAX(0.0, ThetaCap(L) - SW_TEMP(L))
        SW_TEMP(L) = SW_TEMP(L) + DeltaSW(L)
        LatInflow = LatInflow + DeltaSW(L) * DLAYR(L) * 10.
      ENDDO
      
!     Flux in soil water content due to changes in water table and capillary flow
      DO L = 1, NLAYR
        SWDELTW(L) = SW_TEMP(L) - SW(L)
      ENDDO
    ENDIF

!-----------------------------------------------------------------------
!   Convert the soil water flux due to water table into 2D variable 
    CALL Interpolate2Cells_2D(                    &
        CELLS%STRUC, SOILPROP, SWDELTW, 0.0,      &     !Input
        SWVDeltW)                                       !Output

!   The 2D model is not needed in the vicinity of the water table.
!   Calculate the limits of the 2D model. 
    IF (ActWTD > DS(L)) THEN
!     Water table is below profile depth
      LIMIT_2D = NRowsTot    
    Else          
!     Set LIMIT_2D to be the layer above ThetaCap = .9 * SAT
      LIMIT_2D = NLAYR
      DO L = NLAYR, 1, -1
        IF ((ThetaCap(L) - DUL(L)) > (0.9 * (SAT(L) - DUL(L)))) then
          LIMIT_2D = L - 1
        ELSE 
          EXIT
        ENDIF
      ENDDO
    ENDIF 
    
    BedDimension % LIMIT_2D = LIMIT_2D

!-----------------------------------------------------------------------
    RETURN
    End Subroutine WaterTable_2D
!=======================================================================
!     WaterTable_2D VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! ActWTD   The actual depth which may vary from the MgmtWTD if there has been rainfall.  
!          Calculated from soil water content at the end of the day.  Not be
!          used for anything, but just reported as output.
! Bottom   Depth of the bottom for current layer 
! LatFlow  Daily LatFlow in cm
! LIMIT_2D From LIMIT_2D+1 to LNYR using 1D model. Daily variable. It is allowed to be in bed area
! MgmtWTD  User inputed fixed management water table depth. Counted from top to down
! SWV(L,j) Cell soil water content
! ThetaCap An array of volumetric soil water contents at the midpoint of each soil layer.
!          Calculated from the water characteristic curve at the height above the
!          water table. 
! Top      Depth of the top for current layer
!-----------------------------------------------------------------------
!     END SUBROUTINE WaterTable_2D
!=======================================================================


