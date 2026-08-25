!=======================================================================
!  WaterTable, Subroutine, C.H.Porter
!  Computes lateral flows necessary to maintain a managed water table depth
!    (i.e., daily water table depth user inputs).
!  The actual computed water table may rise above or fall below managed 
!    water table due to rainfall and rapid changes in user input depths.
!
!-----------------------------------------------------------------------
!  REVISION       HISTORY
!  01/23/2010 CHP Written
!  02/10/2023 chp move SW integration to WATBAL
!  04/15/2025 CHP integrated 1D and 2D water table routines.
!=======================================================================

    Subroutine WaterTable(DYNAMIC,              &
      SOILPROP, SW,                             &   !Input
      ActWTD, netLatFlow,                       &   !Output
      MgmtWTD, SWDELTW)                             !Output

!-----------------------------------------------------------------------
    USE ModuleDefs
    USE ModuleData
    IMPLICIT NONE
    EXTERNAL CapFringe
    SAVE
!-----------------------------------------------------------------------
!   Interface:
    INTEGER            , INTENT(IN) :: DYNAMIC
    TYPE(SoilType)     , INTENT(IN) :: SOILPROP
    REAL, DIMENSION(NL), INTENT(IN) :: SW
    REAL               , INTENT(OUT):: ActWTD, MgmtWTD
    REAL               , INTENT(OUT):: netLatFlow
    REAL, DIMENSION(NL), INTENT(OUT):: SWDELTW

!   Local
    INTEGER L, NLAYR
    REAL Bottom, Top, Thick, TargetWTD, BedAdjust, MinWTD, AdjWTD
    REAL, DIMENSION(NL) :: DLAYR, DS, DUL, SAT, WCR
    REAL, DIMENSION(NL) :: ThetaCap, SW_temp, DeltaSW

    REAL, PARAMETER :: TOL = 0.5  !tolerance for target water table level (cm)
    REAL, PARAMETER :: Kd = 0.5   !drawdown coefficient (fraction/day)
!   Kd should be a user specified input in the Fields section of FILEX
!     based on field measurement. Or it could be calculated from soil 
!     properties. For now assume a constant value.  

!***********************************************************************
!***********************************************************************
    netLatFlow = 0.0
    SWDELTW    = 0.0
    SW_temp    = SW

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

!   For 2D raised bed systems, we need to adjust the user-input 
!     water table depths to be relative to top of the constructed bed.
!   This is not needed for 1D simulations, so this is a placeholder 
!     until we merge the 2D model into this code.

!   Water table adjustment for constructed bed
    BedAdjust = 0.0
!   Limit the water table depth to bottom of raised bed
    MinWTD    = 0.0
!   Negative or zero value means no managed water table
    IF (MgmtWTD < 1.E-6) THEN
      MgmtWTD = 1000.
      BedAdjust = 0.0
    ENDIF

    AdjWTD = MAX(MgmtWTD + BedAdjust, MinWTD)
    ActWTD    = AdjWTD
    TargetWTD = AdjWTD

!   Actual water table depth will equal either the managed water table depth or
!     the target water table depth. When user water table depth records change
!     a lot from one day to the next, it may take a few days for the actual 
!     water table depth to reach the managed depth. The target depth allows a slow 
!     transition from one managed depth to another. 
    CALL PUT('WATER','WTDEP' ,ActWTD)  !Actual current water table depth
!   Managed water table depth (i.e., user records) adjusted for bed construction
    CALL PUT('MGMT' ,'ADJWTD',AdjWTD)  

    IF (AdjWTD < DS(NLAYR)) THEN
!     Initialize soil water content with water table depth, starting at bottom of profile
      DO L = NLAYR, 1, -1
        IF (L == 1) THEN 
          Top = 0.
        ELSE
          Top = DS(L-1)
        ENDIF
        Bottom = DS(L)
        IF (AdjWTD > Bottom) THEN
!         This layer is entirely above the managed water table; done.
!         Leave water content as read from initial conditions.
          EXIT
        ELSEIF (AdjWTD > Top) THEN
!         This layer is partially in managed water table.  
!         Assume saturated water content below water table, DUL above.
          Thick = Bottom - AdjWTD
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

!   Get management depth to water table (user input)
    CALL GET('MGMT','WATTAB',MgmtWTD)

!   Adjust depth to water table for bed height contruction
!   i.e., measure from top of bed instead of original ground level
    IF (MgmtWTD > 0 .AND. MgmtWTD < 999.) THEN
      AdjWTD = MAX(MgmtWTD + BedAdjust, MinWTD)
    ELSE
       AdjWTD = 1000.
    ENDIF
     
    CALL PUT('MGMT','ADJWTD',AdjWTD)

    IF (AdjWTD > DS(NLayr) .AND. ActWTD > DS(NLayr)) THEN
      RETURN
    ENDIF

!-----------------------------------------------------------------------
!   Compute updated soil water content with today's water table depth
!   Use tolerance in cm to get target water table depth close to management 
!   depth.  If the exact management depth is used, instabilities occur due
!   to soil water changes with other simultaneous processes.

!   Actual water table lower than management. Water table has been raised.
    IF (ActWTD - AdjWTD > TOL) THEN

!     initial guess at today's water table depth
      TargetWTD = (ActWTD - AdjWTD) * Kd + AdjWTD

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

!       Set water content below the water table depth to SAT
        IF (SW(L) < SAT(L)) THEN
          SWDELTW(L) = (SAT(L) - SW(L)) * Thick / DLAYR(L)
        ELSE
          SWDELTW(L) = 0.0
        ENDIF
      ENDDO

!-------------------------------------------------------------------------
!   Actual water table higher than management - drawdown using Kd
    ELSEIF (AdjWTD - ActWTD > TOL .AND. AdjWTD < 999.) THEN
!     Calculate lateral outflow needed to draw water table down to specified depth

!     initial guess at today's water table depth
      TargetWTD = (ActWTD - AdjWTD) * Kd + AdjWTD

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
!           water content to DUL or below in the entire layer.
          Thick = DLAYR(L)
        ELSEIF (TargetWTD > Top) THEN
!         This layer is partially in target water table. The top portion
!           of the layer (above the TargetWTD) is set to DUL (or below).
          Thick = TargetWTD - Top
        ELSE
!         This layer is entirely within target water table. Do nothing 
!           because it was in the water table yesterday, too.
          EXIT
        ENDIF 

!       Set water content that is now above the water table depth to DUL
        IF (SW(L) > DUL(L)) THEN
          SWDELTW(L) = -(SW(L) - DUL(L)) * Thick / DLAYR(L)
        ELSE
          SWDELTW(L) = 0.0
        ENDIF
      ENDDO

    ELSE
      TargetWTD = AdjWTD
    ENDIF 

    ActWTD = TargetWTD
    CALL PUT('WATER','WTDEP',ActWTD)

!-------------------------------------------------------------------------
!   Update soil water content 
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
!   Set actual water table depth equal to target calculated above.
!   Probably need a better way to do this, i.e., calculate the actual
!   water table depth based on water table and drawdown dynamics, but
!   previous attempts resulted in instability for daily model.

!   No effect of water table if it is below the bottom of the soil profile.
    IF (ActWTD .GE. DS(NLAYR)) THEN
      RETURN
    ENDIF

!   Calculate water content within capillary fringe, ThetaCap
    CALL CapFringe(           &
      ActWTD,  SOILPROP,      &   !Input
      ThetaCap)                   !Output

!   Update temporary soil water content with ThetaCap
    DO L = 1, NLAYR
      DeltaSW(L) = MAX(0.0, ThetaCap(L) - SW_TEMP(L))
      SW_TEMP(L) = SW_TEMP(L) + DeltaSW(L)
      SWDELTW(L) = SW_TEMP(L) - SW(L)
!     net lateral flow (+ inflow, - outflow)
      netLatFlow = netLatFlow + SWDELTW(L) * DLAYR(L) * 10.
    ENDDO

!-----------------------------------------------------------------------
    RETURN
    End Subroutine WaterTable
!=======================================================================
!     WaterTable VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! ActWTD   The actual water table depth which may vary from the managed water table depth
!          if there has been rainfall. ActWTD is calculated from soil water content  
!          at the end of the day (cm)
! MgmtWTD  User input managed water table depth below surface (cm)
! AdjWTD   Adjusted water table depth = MgmtWTD + adustment factor based on bed geometry
! ThetaCap An array of volumetric soil water contents at the midpoint of each soil layer.
!          Calculated from the water characteristic curve at the height above the
!          water table. 
!=======================================================================

