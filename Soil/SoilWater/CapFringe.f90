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

!!=======================================================================
!!  ThetaCapOp, Subroutine
!!  Output daily info for ThetaCap 
!!-----------------------------------------------------------------------
!!  REVISION       HISTORY
!!  
!!=======================================================================
!
!    Subroutine ThetaCapOp(DYNAMIC,CONTROL,ISWITCH,MgmtWTD, SOILPROP)
!!-----------------------------------------------------------------------
!    USE ModuleDefs
!    USE Cells_2D
!    IMPLICIT NONE
!    EXTERNAL GETLUN, HEADER, CAPFRINGE, YR_DOY, INCDAT
!    SAVE
!!-----------------------------------------------------------------------
!    integer DYNAMIC, YR2, DY2, DAS, LUNThetaCap, LIMIT_2D, L
!    INTEGER INCDAT, N1_LYR, N2_LYR
!    REAL, INTENT(IN) :: MgmtWTD
!    TYPE (ControlType), INTENT(IN) :: CONTROL
!    Type(SoilType) , INTENT(INOUT):: SOILPROP
!    TYPE (SwitchType)   ISWITCH
!
!    REAL, DIMENSION(NL):: ThetaCap, DUL, LL, SAT, LrTop, DS !NLAYR
!!   REAL ThetaCa(25)
!    CHARACTER*17, PARAMETER :: ThetaCapOut = 'ThetaCap.OUT'
!    LOGICAL FEXIST, DOPRINT
!    !    CALL GET('MGMT','WATTAB',MgmtWTD)
!    LIMIT_2D = BedDimension % LIMIT_2D 
!    N1_LYR = Min (11, SOILPROP % NLAYR)
!    N2_LYR = Min (25, SOILPROP % NLAYR) 
!!***********************************************************************
!!     Seasonal initialization - run once per season
!!***********************************************************************
!    IF (DYNAMIC .EQ. SEASINIT) THEN
!!-----------------------------------------------------------------------
!      DOPRINT=.TRUE.
!      IF (ISWITCH % IDETW .EQ. 'N') THEN
!        DOPRINT=.FALSE.
!      ENDIF
!      IF (ISWITCH % ISWWAT .EQ. 'N') THEN
!        DOPRINT=.FALSE.
!      ENDIF
!      IF (ISWITCH % IDETL /= 'D') THEN
!        DOPRINT=.FALSE.
!      ENDIF
!      IF (.NOT. DOPRINT) RETURN
!      DO L = 1, min(25, SOILPROP % NLAYR)
!        DS(L) = SOILPROP % DS(L)
!        LrTop(L) = DS(L) - SOILPROP % DLAYR(L)
!        LL(L)   = SOILPROP % LL(L)
!        DUL(L)  = SOILPROP % DUL(L)
!        SAT(L)  = SOILPROP % SAT(L)
!      EndDo
!!       Open output file
!        CALL GETLUN('ThetaCapOut', LUNThetaCap)
!        INQUIRE (FILE = ThetaCapOut, EXIST = FEXIST)
!        IF (FEXIST) THEN
!          OPEN (UNIT = LUNThetaCap, FILE = ThetaCapOut, STATUS = 'OLD', &
!          POSITION = 'APPEND')
!        ELSE
!          OPEN (UNIT = LUNThetaCap, FILE = ThetaCapOut, STATUS = 'NEW')
!          WRITE(LUNThetaCap,'("*Daily ThetaCap for each Soil layer")')
!        ENDIF
!      CALL HEADER(SEASINIT, LUNThetaCap, CONTROL % RUN)
!!      if (CONTROL % RUN .eq. 1) then
!      !  WRITE (LUNThetaCap, '(I2)') &
!      !      ('! LIMIT_2D is ', LIMIT_2D) 
!  !    Endif
! 
!      WRITE (LUNThetaCap,1115, ADVANCE='NO')'!    Variable ' 
!      WRITE (LUNThetaCap,1116) ("LYR",L, L=1,N1_LYR), ("LYR",L, L=13,N2_LYR, 2)
!      WRITE (LUNThetaCap,1115, ADVANCE='NO')'!   Top Layer'
!      WRITE (LUNThetaCap,1118) (LrTop(L),L=1,N1_LYR), (LrTop(L),L=13,N2_LYR,2)
!      WRITE (LUNThetaCap,1115, ADVANCE='NO')'!Bottom Layer'
!      WRITE (LUNThetaCap,1118) (DS(L),L=1,N1_LYR), (DS(L),L=13,N2_LYR,2)
!      WRITE (LUNThetaCap,1115, ADVANCE='NO')'!          LL'
!      WRITE (LUNThetaCap,1119) (LL(L),L=1,N1_LYR), (LL(L),L=13,N2_LYR,2)
!      WRITE (LUNThetaCap,1115, ADVANCE='NO')'!         DUL'
!      WRITE (LUNThetaCap,1119) (DUL(L),L=1,N1_LYR), (DUL(L),L=13,N2_LYR,2)
!      WRITE (LUNThetaCap,1115, ADVANCE='NO')'!         SAT'
!      WRITE (LUNThetaCap,1119) (SAT(L),L=1,N1_LYR), (SAT(L),L=13,N2_LYR,2)
!      WRITE (LUNThetaCap,1115)
! 
!      WRITE (LUNThetaCap,1120, ADVANCE='NO')'@YEAR DOY DAS '
!      WRITE (LUNThetaCap,1116, ADVANCE='NO') ("LYR",L, L=1,N1_LYR), &
!         ("LYR",L, L=13,N2_LYR, 2)
!      WRITE (LUNThetaCap,1120)'  mgWTD LIMIT_2D'
!       CALL CapFringe(            & 
!          MgmtWTD,  SOILPROP,   &    !Input
!          ThetaCap)                  !Output
!      
!      CALL YR_DOY(INCDAT(CONTROL % YRDOY,-1),YR2,DY2)
!      WRITE (LUNThetaCap,1300, ADVANCE='NO') YR2, DY2, DAS, &
!           (ThetaCap(L),L=1,N1_LYR), (ThetaCap(L),L=13,N2_LYR,2)
!      WRITE (LUNThetaCap,1400) MgmtWTD, LIMIT_2D   
!      
!!***********************************************************************
!!     DAILY RATE CALCULATIONS
!!***********************************************************************
!    ELSEIF (DYNAMIC .EQ. RATE) THEN        
!      CALL CapFringe(            & 
!          MgmtWTD,  SOILPROP,   &    !Input
!          ThetaCap)                  !Output
!      
!      CALL YR_DOY(CONTROL % YRDOY, YR2, DY2)
!      DAS = CONTROL % DAS
!      IF (.NOT. DOPRINT) RETURN
!      WRITE (LUNThetaCap,1300, ADVANCE='NO') YR2, DY2, DAS, &
!           (ThetaCap(L),L=1,N1_LYR), (ThetaCap(L),L=13,N2_LYR,2)
!      WRITE (LUNThetaCap,1400) MgmtWTD, LIMIT_2D   
! 1115 FORMAT(13A)
! 1116 FORMAT(9(2X,A3,I1), 9(1X,A3,I2)) 
! 1118 FORMAT(1x, 18F6.1) 
! 1119 FORMAT(1x, 18F6.2) 
! 1120 FORMAT(14A)  
! 1121 FORMAT(A1, 7X, A5, 1X, 18F6.1, F6.1,3X)    
! 1122 FORMAT(A1, 7X, A5, 1X, 18F6.3, F6.1,3X)   
! 1300 FORMAT(1X,I4,1X,I3,1X,I3, 1X, 18F6.3, F6.1,3X, I2) 
! 1400 FORMAT(F7.0,3X, I2) 
!!***********************************************************************
!!     END OF DYNAMIC IF CONSTRUCT
!!***********************************************************************  
!    Endif  
!    RETURN
!  End Subroutine ThetaCapOp
!!=======================================================================