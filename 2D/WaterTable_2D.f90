!=======================================================================
!  WaterTable_2D, Subroutine, C.H.Porter
!  Computes lateral flows necessary to maintain a managed water table depth.
!  Collapses 2D soil water into 1D layers with average soil water content.
!  Then the 1D water table routine is used to calculate lateral flow.
!-----------------------------------------------------------------------
!  REVISION       HISTORY
!  01/23/2010 CHP Written
!  Aug. 2011 Add LIMIT_2D for water table handling
!            Set LIMIT_2D to be the layer above ThetaCap = .9 * SAT 
!            The maximum of LIMIT_2D is bottom layer of the bed or first layer of the non bed system
!            Change the argument of subroutine. SWV is output, removerAtcWTD
!            Add MgmtWTD_Y; Calculate the LatFlow due to the change of water table depth
!            Reorganize the if statement of dynamic sections
!=======================================================================

  Subroutine WaterTable_2D(DYNAMIC,                     &
    CELLS, SOILPROP, SWV,                               &   !Input                                              
    ActWTD, LatInflow, LatOutflow,            &   !Output
    MgmtWTD, ThetaCap)                             !Output
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
  Type(SoilType), INTENT(IN) :: SOILPROP
  REAL, DIMENSION(MaxRows,MaxCols), INTENT(INOUT) :: SWV
  REAL, INTENT(OUT):: ActWTD, MgmtWTD
    REAL               , INTENT(OUT):: LatInflow, LatOutflow
  REAL, DIMENSION(NL), INTENT(OUT) :: ThetaCap

! Local
  INTEGER L, NLAYR, LIMIT_2D 
! REAL Deficit, LatFlow, MgmtWTD_Y
  REAL Bottom, Top, Thick, TargetWTD
  REAL, DIMENSION(NL) :: DLAYR, DS, DUL, SAT, SW, WCR  
  
!  Data MgmtWTD_Y /-99/ ! it should be 10000.?

    REAL, PARAMETER :: TOL = 0.5  !tolerance for target water table level (cm)
    
    REAL, PARAMETER :: Kd = 0.5   !drawdown coefficient (fraction/day)
!   Kd should be a user specified input in the Fields section of FILEX
!   based on field measurement. Or it could be calculated from soil 
!   properties. For now assume a constant value.  

!  Deficit = 0.
  
    LatInflow  = 0.0
    LatOutflow = 0.0
! LatFlow = 0.
  
!! Get management depth to water table
!  CALL GET('MGMT','WATTAB',MgmtWTD)
!! Negative or zero value means no managed water table
!  IF (MgmtWTD < 1.E-6) THEN
!    MgmtWTD = 10000.
!  ENDIF
!  IF (MgmtWTD_Y < 1.E-6) THEN
!    MgmtWTD_Y = 10000. !JZW add
!  ENDIF

!***********************************************************************
!***********************************************************************
! Seasonal initialization - run once per season
!***********************************************************************
  IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      DLAYR = SOILPROP % DLAYR
      DS    = SOILPROP % DS 
      DUL   = SOILPROP % DUL
      NLAYR = SOILPROP % NLAYR
      SAT   = SOILPROP % SAT
      WCR   = SOILPROP % WCR

!     Get initial depth to water table
      CALL GET('MGMT','ICWD',MgmtWTD)
    
!   Negative or zero value means no managed water table
    IF (MgmtWTD < 1.E-6) THEN
      MgmtWTD = 1000.
    ENDIF
    ActWTD    = MgmtWTD
    TargetWTD = MgmtWTD
!   Actual water table depth will equal either the managed water table depth or
!     the target water table depth. When user water table depth records change
!     a lot from one day to the next, it may take a few days for the actual 
!     water table depth to reach the managed depth.
    CALL PUT('WATER','WTDEP' ,ActWTD)  !Actual current water table depth
    CALL PUT('MGMT' ,'WATTAB',MgmtWTD) !Managed water table depth (i.e., user records)

!   Use cell water content to calculate layer sol water content which will be used to clculate the LatFlow due to water table depth changes
    CALL Interpolate2Layers_2D(                         &
        SWV, CELLS%Struc, SOILPROP % NLAYR,             & !Input
        SW)                                               !Output

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

    CALL PUT('WATER','WTDEP',ActWTD)

!***********************************************************************
!***********************************************************************
! DAILY RATE CALCULATIONS
!***********************************************************************
  ELSEIF (DYNAMIC .EQ. RATE) THEN

!   Get management depth to water table
    CALL GET('MGMT','WATTAB',MgmtWTD)

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

!     initial guess at today's water table depth
      TargetWTD = (ActWTD - MgmtWTD) * Kd + MgmtWTD

!     Water content above the target water table will be set to DUL. 
!       The capillary rise routine will then reset theta values 
!       just above the water table.

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

    ActWTD = TargetWTD
    CALL PUT('WATER','WTDEP',ActWTD)

!-------------------------------------------------------------------------
!     Water table is below profile depth
      IF (MgmtWTD > 9999.) THEN
        LIMIT_2D = NRowsTot    
      Else                       
!       Calculate water content within capillary fringe (for water table)
!       Set LIMIT_2D to be the layer above ThetaCap = .9 * SAT
!       Don't let LIMIT_2D go above bottom of bed   
        LIMIT_2D = NLAYR
        Do L = NLAYR, Max(2,BedDimension % FurRow1), -1
          if ((ThetaCap(L) - DUL(L)) > (0.9 * (SAT(L) - DUL(L)))) then
            LIMIT_2D = L -1
          else 
            Exit
          Endif
        EndDo
      ENDIF !end if there is water table
      
      BedDimension % LIMIT_2D= LIMIT_2D

!   Need to do this in 2D
!   Update soil water content 
!   (Pseudo-integration, the real integration occurs in WATBAL)
    DO L = 1, NLAYR
      SW_temp(L) = SW_temp(L) + SWDELTW(L)
    ENDDO
    
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
  ENDIF
        
!!   Determine actual (average) depth to water table
!   CALL WTDEPT(                                &
!         NLAYR, DLAYR, DS, DUL, SAT, SW,        &         !Input
!         ActWTD)                                          !Output                                           

!-----------------------------------------------------------------------
!   Set actual water table depth equal to target calculated above.
!   Probably need a better way to do this, i.e., calculate the actual
!   water table depth based on water table and drawdown dynamics, but
!   previous attempts resulted in instability for daily model.

!   No effect of water table if it is below the bottom of the soil profile.
    IF (ActWTD > DS(NLAYR)) THEN
      RETURN
    ENDIF

    CALL CapFringe(           &
      ActWTD,  SOILPROP,      &   !Input
      ThetaCap)                   !Output

    DO L = 1, NLAYR
!     ThetaCap(L) = MAX(SW_TEMP(L), ThetaCap(L))
      DeltaSW(L) = MAX(0.0, ThetaCap(L) - SW_TEMP(L))
      SW_TEMP(L) = SW_TEMP(L) + DeltaSW(L)
      LatInflow = LatInflow + DeltaSW(L) * DLAYR(L) * 10.
    ENDDO

!   flux in soil water content due to changes in water table and capillary flow
    DO L = 1, NLAYR
      SWDELTW(L) = SW_TEMP(L) - SW(L)
    ENDDO

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


