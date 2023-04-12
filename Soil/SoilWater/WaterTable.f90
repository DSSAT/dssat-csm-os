!=======================================================================
!  WaterTable, Subroutine, C.H.Porter
!  Computes lateral flows necessary to maintain a managed water table depth.
!  Actual computed water table may rise above or fall below managed water
!  table. 
!-----------------------------------------------------------------------
!  REVISION       HISTORY
!  01/23/2010 CHP Written
!  02/10/2023 chp move SW integration to WATBAL, update for incorporation
!                 into next release
!=======================================================================

    Subroutine WaterTable(DYNAMIC,              &
      SOILPROP, SW,                             &   !Input
      ActWTD, LatInflow, LatOutflow,            &   !Output
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
    REAL               , INTENT(OUT):: LatInflow, LatOutflow
!   SWDELT_WT = Change in SW due to water table changes
    REAL, DIMENSION(NL), INTENT(OUT):: SWDELTW

!   REAL, DIMENSION(NL), INTENT(IN) :: SWDELTU, SWDELTX
!   REAL, DIMENSION(NL), INTENT(OUT):: CAPRI

!   Local
    INTEGER L, NLAYR !, K, L1, L2
    REAL Bottom, Top, Thick, TargetWTD
!   REAL Excess1, Excess2, TotExcess, Residual
    REAL, DIMENSION(NL) :: DLAYR, DS, DUL, SAT, WCR 
!   REAL, DIMENSION(NL) :: Excess, MaxRise
    REAL, DIMENSION(NL) :: ThetaCap, SW_temp, DeltaSW

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
!   CAPRI      = 0.0
    SW_temp    = SW

!***********************************************************************
!***********************************************************************
! Seasonal initialization - run once per season
!***********************************************************************
  IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
    DLAYR = SOILPROP % DLAYR
    DUL   = SOILPROP % DUL
    DS    = SOILPROP % DS
    NLAYR = SOILPROP % NLAYR
    SAT   = SOILPROP % SAT
    WCR   = SOILPROP % WCR
    
!   Get initial depth to water table
    CALL GET('MGMT','ICWD',MgmtWTD)
    
!   Negative or zero value means no managed water table
    IF (MgmtWTD < 1.E-6) THEN
      MgmtWTD = 10000.
    ENDIF
    ActWTD    = MgmtWTD
    TargetWTD = MgmtWTD

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

!   Negative or zero value means no managed water table
    IF (MgmtWTD < 1.E-6) THEN
      MgmtWTD = 10000.
    ENDIF

    IF (MgmtWTD > DS(NLAYR)) RETURN

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

! OLD METHOD
!!     First determine how much water (SAT-DUL) is between actual and 
!!       managed water table levels
!      DO L = 1, NLAYR
!        IF (L == 1) THEN 
!          Top = 0.
!        ELSE
!          Top = DS(L-1)
!        ENDIF
!        Bottom = DS(L)
!        IF (ActWTD > Bottom) THEN
!!         This layer is entirely above the actual water table; do nothing.
!          CYCLE
!        ELSEIF (ActWTD > Top) THEN
!!         This layer is partially in water table
!          L1 = L
!          TotExcess = 0.0
!!         Work thru layers until management water table depth is reached.
!          DO K = L1, NLAYR
!            Bottom = DS(K)
!            IF (K == L) THEN
!              Top = ActWTD
!            ELSE
!              Top = DS(K-1)
!            ENDIF
!
!            IF (MgmtWTD < Bottom) THEN
!              Bottom = MgmtWTD
!              L2 = K
!            ENDIF
!            Thick = Bottom - Top
!
!!           Actual water table assumes a layer is divided into
!!           a saturated portion at the bottom and the remainder at DUL.
!!           (Wrong, I know!, but expedient for now -- fix later)
!!           Removal of water will act on the average water content of
!!           the layer.
!            Excess1 = Thick * (SAT(K) - DUL(K))   !saturated portion of layer
!            Excess2 = DLAYR(K) * (SW(K) - DUL(K)) !average water content above DUL for entire layer 
!            Excess(K) = MIN(Excess1, Excess2)     !cm
!            TotExcess = TotExcess + Excess(K)     !cm
!            IF (ABS(MgmtWTD - Bottom) < 1.) EXIT 
!          ENDDO     
!        ELSE
!!         This layer is entirely within water table; done.
!          EXIT
!        ENDIF 
!      ENDDO     
!
!!     Drawdown volume of water based on Kd
!      LatOutflow = -TotExcess * Kd * 10. !mm
!      Residual = -LatOutflow / 10. !cm
!      DO L = L1, L2
!        IF (Excess(L) < Residual) THEN
!          SWDELTW(L) = -Excess(L) / DLAYR(L)
!          Residual = Residual - Excess(L)
!        ELSE
!          SWDELTW(L) = -Residual / DLAYR(L)
!          Residual = 0.0
!          EXIT
!        ENDIF
!      ENDDO
    ENDIF 

!-------------------------------------------------------------------------
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
!-----------------------------------------------------------------------

!   Set actual water table depth equal to target calculated above.
!   Probably need a better way to do this, i.e., calculate the actual
!   water table depth based on water table and drawdown dynamics, but
!   previous attempts resulted in instability for daily model.
    ActWTD = TargetWTD

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
    End Subroutine WaterTable
!=======================================================================

