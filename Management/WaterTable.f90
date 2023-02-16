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
    EXTERNAL WTDEPT, CAPILLARY, CapFringe
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

!!   Update actual depth to water table
!    CALL WTDEPT(                                &
!         NLAYR, DLAYR, DS, DUL, SAT, SW,        &         !Input
!         ActWTD)                                          !Output

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

!   Negative or zero value means no managed water table
    IF (MgmtWTD < 1.E-6) THEN
      MgmtWTD = 10000.
    ENDIF

!!   Update actual depth to water table
!    CALL WTDEPT(                                &
!         NLAYR, DLAYR, DS, DUL, SAT, SW,        &         !Input
!         ActWTD)                                          !Output
!
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

!!==================================
!!   Capillary rise - Method 1
!   This method resulted in very unstable water table depths in the top layers.
!!==================================
!!   Set a limit on how much capillary rise can occur
!!   Plant uptake and soil evap are from yesterday.
!    DO L = 1, NLAYR
!!     MaxRise    =   DUL   - SW    + uptake     + evap 
!      MaxRise(L) = (SAT(L) - SW(L) - SWDELTX(L) - SWDELTU(L)) * DLAYR(L) *10 !mm
!      MaxRise(L) = MAX(0.0, MaxRise(L))
!    ENDDO 
!
!!   Capillary flow
!    CALL Capillary(DYNAMIC,                   &
!         ActWTD, SOILPROP, SW_TEMP, MaxRise,  &     !Input
!         Capri)                                     !Output
!
!    DO L = 1, NLAYR
!      LatInflow = LatInflow + CAPRI(L)
!      SW_TEMP(L) = SW_TEMP(L) + CAPRI(L) / (DLAYR(L) * 10.)
!    ENDDO
!!-----------------------------------------------------------------------

!==================================
!   Capillary rise - Method 2
!==================================
    CALL CapFringe(           &
      ActWTD,  SOILPROP,      &   !Input
      ThetaCap)                   !Output

    DO L = 1, NLAYR
!     ThetaCap(L) = MAX(SW_TEMP(L), ThetaCap(L))
      DeltaSW(L) = MAX(0.0, ThetaCap(L) - SW_TEMP(L))
      SW_TEMP(L) = SW_TEMP(L) + DeltaSW(L)
      LatInflow = LatInflow + DeltaSW(L) * DLAYR(L) * 10.
    ENDDO

!-----------------------------------------------------------------------
!!   Update actual depth to water table
!    CALL WTDEPT(                                &
!         NLAYR, DLAYR, DS, DUL, SAT, SW_TEMP,   &         !Input
!         ActWTD)                                          !Output
!   Set the actual water table today to the Target WT

!   flux in soil water content due to changes in water table and capillary flow
    DO L = 1, NLAYR
      SWDELTW(L) = SW_TEMP(L) - SW(L)
    ENDDO

!-----------------------------------------------------------------------
    RETURN
    End Subroutine WaterTable
!=======================================================================

!=======================================================================
!  WTDEPT, Subroutine
!  Determines actual water table depth
!  Allows perched water table (uses highest free water surface in profile)
!   This subroutine, as written still results in step-wise increments to water table.
!   Maybe need to go back to original WTDEPT for non-managed water tables (i.e., perched)
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  01/06/1997 GH  Written
!  10/20/1997 CHP Modified for modular format.
!  03/17/2001 CHP Allows water table between top and bottom of layer
!                 (no longer a step function).  Uses highest free water
!                 surface.
!  05/23/2007 CHP Start at bottom of profile and determine 1st unsaturated
!                   layer.  Add factor to decrease step function appearance.
!-----------------------------------------------------------------------
!  Called by: WATBAL
!  Calls    : None
!=======================================================================
      SUBROUTINE WTDEPT(                        &
         NLAYR, DLAYR, DS, DUL, SAT, SW,        &         !Input
         ActWTD)                                          !Output

!     ------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE
      SAVE

!     Interface variables:
      INTEGER, INTENT(IN) :: NLAYR


      INTEGER L
      REAL ActWTD, FACTOR             !Depth to Water table (cm)
      REAL, DIMENSION(NL) ::  DLAYR, DS, DUL, SAT, SW
      REAL, DIMENSION(NL) :: SATFRAC
      REAL, PARAMETER :: TOL = 0.95

!-----------------------------------------------------------------------
      DO L = NLAYR, 1, -1
        SATFRAC(L) = (SW(L) - DUL(L)) / (SAT(L) - DUL(L))
        SATFRAC(L) = MIN(MAX(0.0, SATFRAC(L)), 1.0)
        IF (SATFRAC(L) > TOL) THEN
!         Layer is saturated, continue up to next layer
          CYCLE
        ELSEIF (L == NLAYR) THEN
!         Bottom layer is unsaturated
          ActWTD = DS(NLAYR) - DLAYR(NLAYR) * SATFRAC(NLAYR)
          EXIT
        ELSE
!         Layer is unsaturated.  Interpolate water table depth.
!         FACTOR prevents a step function when transitioning between
!           layers.
          FACTOR = MIN(MAX(0.0, (SATFRAC(L+1) - TOL) / (1.0 - TOL)),1.0)
          ActWTD = DS(L) - DLAYR(L) * SATFRAC(L) * FACTOR
          EXIT
        ENDIF
      ENDDO

      RETURN
      END SUBROUTINE WTDEPT

!-----------------------------------------------------------------------
!     WTDEPT VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! DLAYR(L) Soil thickness in layer L (cm)
! DS(L)    Cumulative depth in soil layer L (cm)
! NL       Maximum number of soil layers = 20 
! NLAYR    Actual number of soil layers 
! SAT(L)   Volumetric soil water content in layer L at saturation
!            (cm3 [water] / cm3 [soil])
! SW(L)    Volumetric soil water content in layer L (cm3[water]/cm3[soil])
! ActWTD    Water table depth  (cm)
! SATFRAC   Fraction of layer L which is saturated
!-----------------------------------------------------------------------
!     END SUBROUTINE WTDEPT
!=======================================================================
