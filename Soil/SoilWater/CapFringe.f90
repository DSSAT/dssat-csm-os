!=======================================================================
!  CapFringe, Subroutine, C.H.Porter
!  Computes capillary fringe above water table.
!  Sends back ThetaCap, an array of volumetric soil water contents at 
!  the midpoint of each soil layer.  These are computed based on the 
!  water characteristic curve and height above the managed water table.
!-----------------------------------------------------------------------
!  REVISION       HISTORY
!  02/16/2010 CHP Written
!=======================================================================

    Subroutine CapFringe(     &
      ActWTD,  SOILPROP,      &   !Input
      ThetaCap)                   !Output

!-----------------------------------------------------------------------
    USE ModuleDefs
    IMPLICIT NONE
    SAVE
!-----------------------------------------------------------------------
!   Interface:
    REAL, INTENT(IN) :: ActWTD
    Type(SoilType), INTENT(IN) :: SOILPROP
    REAL, DIMENSION(NL), INTENT(OUT):: ThetaCap

!   Local
    INTEGER L
    INTEGER NLAYR
    REAL Bottom, Top, MidPt
    REAL H_mid, H_top, H_equiv, Se_boundary
    REAL, DIMENSION(NL) :: DLAYR, DS, DUL, SAT, Se_mid, Se_top
    REAL, DIMENSION(NL) :: alphaVG, mVG, nVG, WCR   

    NLAYR = SOILPROP % NLAYR
    DLAYR = SOILPROP % DLAYR
    DS    = SOILPROP % DS 
    DUL   = SOILPROP % DUL
    SAT   = SOILPROP % SAT

    WCR   = SOILPROP % WCR
    alphaVG=SOILPROP % alphaVG
    mVG   = SOILPROP % mVG
    nVG   = SOILPROP % nVG

!-----------------------------------------------------------------------
!   ThetaCap is the water content from the water characteristic curve at 
!   the height above the water table, using the midpoint of the layer as 
!   the height for any given layer.  
!-----------------------------------------------------------------------
!   Because soil layers have different properties and different water
!   characteristic curves, it is necessary to compute the capillary fringe
!   piecewise, maintaining continuity of the normailized water content, Se, 
!   at soil layer boundaries.
!   Working from the water table, up to the surface, the normalized water 
!   content, Se, must be the same for the soil layers above and below each  
!   soil layer boundary. So at the top of each layer, calculate Se_top. 
!   Then back-calculate the equivalent H for the layer above.
!-----------------------------------------------------------------------
!   Existing soil water content - 
!-----------------------------------------------------------------------

    ThetaCap = DUL
    ! If there is no water table, return
    IF (ActWTD < 1.E-6 .OR. ActWTD > 9999.) RETURN
    Se_boundary = -99.
    !JZW: need to set Se_boundary for L=NLAYR:
    ! If there is water table, calculate theta for all layers
    DO L = NLAYR, 1, -1
      IF (L == 1) THEN 
        Top = 0.
      ELSE
        Top = DS(L-1)
      ENDIF
      Bottom = DS(L)
      MidPt = (Top + Bottom) / 2.0

!   ----------------------------------------------------------
      IF (ActWTD > Bottom) THEN
!       This layer is entirely above the water table.
        IF (Se_boundary < 0.) THEN !lowest layer only
!         Height above water table to midpoint of layer
          H_mid = ActWTD - MidPt
!         JZW need to calculate H_equiv or initialize in the case of ActWTD > DS(NLAYR)
          H_equiv = H_mid - (Bottom - Midpt)
        ELSE
!         At equilibrium state, soil matric potential h = water table depth z
!         Equivalent height above water table for homogeneous soil
          H_equiv = (((1.0 / Se_boundary ** (1./mVG(L)) - 1.0)) ** (1./nVG(L))) / alphaVG(L)
          H_mid = H_equiv + (Bottom - Midpt)
        ENDIF
        Se_mid(L) = (1.0 / (1.0 + (alphaVG(L) * H_mid) ** nVG(L))) ** mVG(L)
        ThetaCap(L) = WCR(L) + Se_mid(L) * (SAT(L) - WCR(L))

        H_top = H_equiv + DLAYR(L)
        Se_top(L) = (1.0 / (1.0 + (alphaVG(L) * H_top) ** nVG(L))) ** mVG(L)
        Se_boundary = Se_top(L)
        
!   ----------------------------------------------------------
      ELSEIF (ActWTD > Top) THEN
!       This layer is partially within the managed water table.  
        IF (ActWTD <= MidPt) THEN
          ThetaCap(L) = SAT(L)
          Se_mid(L) = 1.0
        ELSE
          H_mid = ActWTD - MidPt
!         The relation between normalized SW and Matric potential based on Van Genuchten model
          Se_mid(L) = (1.0 / (1.0 + (alphaVG(L) * H_mid) ** nVG(L))) ** mVG(L)
          ThetaCap(L) = WCR(L) + Se_mid(L) * (SAT(L) - WCR(L))
        ENDIF
        H_top = ActWTD - Top
        Se_top(L) = (1.0 / (1.0 + (alphaVG(L) * H_top) ** nVG(L))) ** mVG(L)
        Se_boundary = Se_top(L)

!   ----------------------------------------------------------
      ELSE
!       This layer is entirely within the water table
        Se_mid(L) = 1.0
        Se_top(L) = 1.0
        ThetaCap(L) = SAT(L)
      ENDIF 
    ENDDO

!-----------------------------------------------------------------------
    RETURN
    End Subroutine CapFringe
!=======================================================================
!=======================================================================
!     CapFringe VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! Bottom      Depth to bottom of the current layer 
! H_equiv     Equivalent height for the bottom of the layer
!             Assume: homogeneous soil; the normalized soil water content is Se_boundary
!             Measured from the water table surface 
! ActWTD      Water table depth (cm) below surface.
! MidPt       Distance between midpoint of current layer to water table surface
! Se_boundary Normalized soil water content at layer boundary. 
!             Assume the Se at top of layer L+1 as the boundary of layer L and layer L+1
! Se_mid(L)   Normalized soil water content at middle of given layer
! Se_top(L)   Normalized soil water content at the top of given layer
! ThetaCap    An array of volumetric soil water contents at the midpoint of each soil layer.
!             Calculated from the water characteristic curve at the height above the
!             water table. At equilibrium state, soil matric potential h = water table depth z
! Top         Depth to top of the current layer
!-----------------------------------------------------------------------
!     END SUBROUTINE CapFringe
!=======================================================================
