!=======================================================================
!  WaterTable, Subroutine, C.H.Porter
!  Computes lateral flows necessary to maintain a managed water table depth.
!  Actual computed water table may rise above or fall below managed water
!  table. 
!-----------------------------------------------------------------------
!  REVISION       HISTORY
!  01/23/2010 CHP Written
!=======================================================================

    Subroutine WaterTable(DYNAMIC,                    &
      SOILPROP,  SWDELTU,                             &   !Input
      SW,                                             &   !I/O
      ActWTD, CAPRI, LatInflow, LatOutflow, MgmtWTD)      !Output

!-----------------------------------------------------------------------
    USE ModuleDefs     
    USE ModuleData
    IMPLICIT NONE
    SAVE
!-----------------------------------------------------------------------
!   Interface:
    INTEGER            , INTENT(IN) :: DYNAMIC
    TYPE(SoilType)     , INTENT(IN) :: SOILPROP
    REAL, DIMENSION(NL), INTENT(IN) :: SWDELTU
    REAL, DIMENSION(NL), INTENT(INOUT) :: SW
    REAL, DIMENSION(NL), INTENT(OUT):: CAPRI
    REAL               , INTENT(OUT):: ActWTD, MgmtWTD
    REAL               , INTENT(OUT):: LatInflow, LatOutflow

!   Local
    INTEGER L, K, L1, L2, NLAYR
    REAL Bottom, Top, Thick, Residual, TargetWTD
    REAL Excess1, Excess2, TotExcess
    REAL, DIMENSION(NL) :: DLAYR, DS, DUL, SAT, WCR
    REAL, DIMENSION(NL) :: Excess, SWDELTW, RWU

    REAL, PARAMETER :: TOL = 5.0    !tolerance for water table level (cm)
    REAL, PARAMETER :: Kd = 0.5     !drawdown coefficient (1/day)
!   Kd should be a user specified input in the Fields section of FILEX
!   based on field measurement.  For now assume a constant value.  

!***********************************************************************
!***********************************************************************
    LatInflow  = 0.0
    LatOutflow = 0.0
    SWDELTW    = 0.0
    CAPRI      = 0.0

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

!   Update actual depth to water table
    CALL WTDEPT(                                &
         NLAYR, DLAYR, DS, DUL, SAT, SW,        &         !Input
         ActWTD)                                          !Output

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
          SW(L) = (SAT(L) * Thick + DUL(L) * (DLAYR(L) - Thick)) / DLAYR(L)
        ELSE
!         This layer is entirely within managed water table.
          SW(L) = SAT(L)
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

!   Update actual depth to water table
    CALL WTDEPT(                                &
         NLAYR, DLAYR, DS, DUL, SAT, SW,        &         !Input
         ActWTD)                                          !Output

!-----------------------------------------------------------------------
!   Compute lateral flow to maintain managed water table depth
!   Use tolerance in cm to get target water table depth close to management 
!   depth.  If the exact management depth is used, instabilities occur due
!   to soil water changes with other simultaneous processes.

!   Actual water table lower than management.  Lateral Inflow calculations
    IF (ActWTD - MgmtWTD > TOL) THEN
!     Calculate lateral inflow needed to raise water table to specified depth
      TargetWTD = (ActWTD - MgmtWTD) * Kd + MgmtWTD
      DO L = NLAYR, 1, -1
        IF (L == 1) THEN 
          Top = 0.
        ELSE
          Top = DS(L-1)
        ENDIF
        Bottom = DS(L)
        IF (TargetWTD > Bottom) THEN
!         This layer is entirely above the managed water table; done.
          EXIT
        ELSEIF (TargetWTD > Top) THEN
!         This layer is partially in managed water table
          Thick = Bottom - TargetWTD
        ELSE
!         This layer is entirely within managed water table.
          Thick = DLAYR(L)
        ENDIF 
        SWDELTW(L) = MAX(0.0,(SAT(L) - SW(L)) * Thick / DLAYR(L))
        LatInflow = LatInflow + SWDELTW(L) * DLAYR(L) * 10. !mm
      ENDDO

!-------------------------------------------------------------------------
!   Actual water table higher than management - drawdown using Kd
    ELSEIF (MgmtWTD - ActWTD > TOL .AND. MgmtWTD < 9999.) THEN
!     Calculate lateral outflow needed to draw water table down to specified depth
!     Gradual drawdown using Kd

!     First determine how much water (SAT-DUL) is between actual and 
!       managed water table levels
      DO L = 1, NLAYR
        IF (L == 1) THEN 
          Top = 0.
        ELSE
          Top = DS(L-1)
        ENDIF
        Bottom = DS(L)
        IF (ActWTD > Bottom) THEN
!         This layer is entirely above the actual water table; do nothing.
          CYCLE
        ELSEIF (ActWTD > Top) THEN
!         This layer is partially in water table
          L1 = L
          TotExcess = 0.0
!         Work thru layers until management water table depth is reached.
          DO K = L1, NLAYR
            Bottom = DS(K)
            IF (K == L) THEN
              Top = ActWTD
            ELSE
              Top = DS(K-1)
            ENDIF

            IF (MgmtWTD < Bottom) THEN
              Bottom = MgmtWTD
              L2 = K
            ENDIF
            Thick = Bottom - Top

!           Actual water table assumes a layer is divided into
!           a saturated portion at the bottom and the remainder at DUL.
!           (Wrong, I know!, but expedient for now -- fix later)
!           Removal of water will act on the average water content of
!           the layer.
            Excess1 = Thick * (SAT(K) - DUL(K))   !saturated portion of layer
            Excess2 = DLAYR(K) * (SW(K) - DUL(K)) !average water content above DUL for entire layer 
            Excess(K) = MIN(Excess1, Excess2)     !cm
            TotExcess = TotExcess + Excess(K)     !cm
            IF (ABS(MgmtWTD - Bottom) < 1.) EXIT 
          ENDDO     
        ELSE
!         This layer is entirely within water table; done.
          EXIT
        ENDIF 
      ENDDO     

!     Drawdown volume of water based on Kd
      LatOutflow = -TotExcess * Kd * 10. !mm
      Residual = -LatOutflow / 10. !cm
      DO L = L1, L2
        IF (Excess(L) < Residual) THEN
          SWDELTW(L) = -Excess(L) / DLAYR(L)
          Residual = Residual - Excess(L)
        ELSE
          SWDELTW(L) = -Residual / DLAYR(L)
          Residual = 0.0
          EXIT
        ENDIF
      ENDDO
    ENDIF 

!-------------------------------------------------------------------------
!   Update soil water content
    DO L = 1, NLAYR
      SW(L) = SW(L) + SWDELTW(L)
    ENDDO

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
  ENDIF
!-----------------------------------------------------------------------

    IF (MgmtWTD > 9999.) RETURN

!!   Calculate water content within capillary fringe (for water table)
!    CALL CapFringe(              & 
!        MgmtWTD,  SOILPROP,      &    !Input
!        ThetaCap)                     !Output

    CALL GET('SPAM','UH2O',RWU)

!   Capillary flow    
    CALL Capillary(DYNAMIC,                    &
         SWDELTU, MgmtWTD, RWU, SOILPROP, SW,  &        !Input
         Capri)                                         !Output

    DO L = 1, NLAYR
      LatInflow = LatInflow + CAPRI(L)
      IF (L < NLAYR) THEN
        SW(L) = SW(L) + (CAPRI(L+1)-CAPRI(L)) / (DLAYR(L) * 10.)
      ELSE
        SW(L) = SW(L) - CAPRI(L) / (DLAYR(L) * 10.)
      ENDIF
    ENDDO
!
!   Update actual depth to water table
    CALL WTDEPT(                                &
         NLAYR, DLAYR, DS, DUL, SAT, SW,        &         !Input
         ActWTD)                                          !Output

!-----------------------------------------------------------------------
    RETURN
    End Subroutine WaterTable
!=======================================================================

!=======================================================================
!  WTDEPT, Subroutine
!  Determines actual water table depth
!  Allows perched water table (uses highest free water surface in profile)
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

      INTEGER L, NLAYR
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
