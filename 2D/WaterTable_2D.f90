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

  Subroutine WaterTable_2D(DYNAMIC,                    &
    CELLS, SOILPROP, SWV,                               &   !Input                                              
    LatFlow, MgmtWTD, ThetaCap)                             !Output
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
  REAL, INTENT(OUT):: MgmtWTD
  REAL, DIMENSION(NL), INTENT(OUT) :: ThetaCap

! Local
  INTEGER L, LIMIT_2D 
  INTEGER NLAYR
  REAL Deficit, LatFlow, MgmtWTD_Y
  REAL, DIMENSION(NL) :: DLAYR, DS, DUL, SAT, SW  
  
  Data MgmtWTD_Y /-99/ ! it should be 10000.?

!-----------------------------------------------------------------------
!   Update actual depth to water table
!   CALL WTDEPT(                                &
!        NLAYR, DLAYR, DS, DUL, SAT, SW,        &         !Input
!       ActWTD)                                          !Output
  Deficit = 0.
  LatFlow = 0.
  
! Get management depth to water table
  CALL GET('MGMT','WATTAB',MgmtWTD)
! Negative or zero value means no managed water table
  IF (MgmtWTD < 1.E-6) THEN
    MgmtWTD = 10000.
  ENDIF
  IF (MgmtWTD_Y < 1.E-6) THEN
    MgmtWTD_Y = 10000. !JZW add
  ENDIF
!***********************************************************************
!***********************************************************************
! Seasonal and rate sections
!***********************************************************************
  IF (DYNAMIC == SEASINIT .OR. DYNAMIC == RATE) THEN
!-----------------------------------------------------------------------
    IF (DYNAMIC == SEASINIT) then
      NLAYR = SOILPROP % NLAYR
      DLAYR = SOILPROP % DLAYR
      DS    = SOILPROP % DS 
      DUL   = SOILPROP % DUL
      SAT   = SOILPROP % SAT
      MgmtWTD_Y = 10000. ! required for multiple run
      LIMIT_2D = NRowsTot
      
    Endif
!   There is change to water table from yesterday
    IF (ABS(MgmtWTD - MgmtWTD_Y) > 0.1) THEN
!     Water table is below profile depth
      IF (MgmtWTD > 9999.) THEN
        LIMIT_2D = NRowsTot    
      Else                       
!   Calculate water content within capillary fringe (for water table)
        CALL CapFringe(            & 
          MgmtWTD,  SOILPROP,   &    !Input
          ThetaCap)                  !Output
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
    !  IF (DYNAMIC == SEASINIT) then
     !   Do L =1, NLAYR
     !     DO j = 1, NColsTot
      !      SELECT CASE(CELLS(L,j)%Struc%CellType)
      !        CASE (3,4,5)
       !         SWV(L,j) = ThetaCap(L)
      !          SWA(L,j) = ThetaCap(L) - SOILPROP%LL(L)
      !        CASE DEFAULT
       !         SWV(L,j) = 0.0
      !          SWA(L,j) = 0.0
       !     END SELECT
       !   Enddo
       ! enddo
     ! Endif
    ENDIF !end if there is change for water table depth from yesterday
    
    ! use cell water content to calculate layer sol water content which will be used to clculate the LatFlow due to water table depth changes
    CALL Interpolate2Layers_2D(                         &
        SWV, CELLS%Struc, SOILPROP % NLAYR,             & !Input
        SW)                                               !Output

!   Calculate lateral flow needed to maintain this water table depth
!   If water table surface raise, LatFlow to fill SW to ThetaCap
!   If water surface drop, 2D drainage will solve the soil water change 
    if (dynamic == rate) then
      Do L = NLAYR, LIMIT_2D+1, -1         
        ! if (Ytd_LIMIT_2D .GT.LIMIT_2D) then
        ! else
        Deficit = (ThetaCap(L)- SW(L)) * DLAYR(L)
        LatFlow = LatFlow + Deficit ! cm ! LatFlow in is positive
        ! endif 
      Enddo
      LatFlow = LatFlow * 10.   !mm

!     Save water table depth for comparison tomorrow
     ! if (dynamic == rate) 
      MgmtWTD_Y = MgmtWTD
     
    Endif

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

 !------------------------------------------

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
      MgmtWTD,  SOILPROP,     &   !Input
      ThetaCap)                   !Output

!-----------------------------------------------------------------------
    USE ModuleDefs
    IMPLICIT NONE
    SAVE
!-----------------------------------------------------------------------
!   Interface:
    REAL, INTENT(IN) :: MgmtWTD
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
    ThetaCap = DUL
    ! If there is no water table, return
    IF (MgmtWTD < 1.E-6 .OR. MgmtWTD > 9999.) RETURN
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
      IF (MgmtWTD > Bottom) THEN
!       This layer is entirely above the managed water table.
        IF (Se_boundary < 0.) THEN
!         Height above water table for lowest layer, if completely 
!           above managed water table
          H_mid = MgmtWTD - MidPt
! JZW need to calculate H_equiv or initialize in the case of MgmtWTD > DS(NLAYR)
          H_equiv = H_mid - (Bottom - Midpt)
!Other it is arbrary value
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
      ELSEIF (MgmtWTD > Top) THEN
!       This layer is partially within the managed water table.  
        IF (MgmtWTD <= MidPt) THEN
          ThetaCap(L) = SAT(L)
          Se_mid(L) = 1.0
        ELSE
          H_mid = MgmtWTD - MidPt
          ! The relation between normalized SW and Metric potential based on Van Genuchte model
          Se_mid(L) = (1.0 / (1.0 + (alphaVG(L) * H_mid) ** nVG(L))) ** mVG(L)
          ThetaCap(L) = WCR(L) + Se_mid(L) * (SAT(L) - WCR(L))
        ENDIF
        H_top = MgmtWTD - Top
        Se_top(L) = (1.0 / (1.0 + (alphaVG(L) * H_top) ** nVG(L))) ** mVG(L)
        Se_boundary = Se_top(L)

!   ----------------------------------------------------------
      ELSE
!       This layer is entirely within the managed water table
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
! Bottom   Depth of the bottom for current layer 
! H_equiv  Equivalent height for the bottom of the layer
!          Assume: homogeneous soil; the normalized soil water content is Se_boundary
!          Measured from the water table surface 
! MgmtWTD  User inputed fixed management water table depth (cm). Counted from top to down
! MidPt    Depth of the middle point for current layer 
!          Measured from the water table surface
! Se_boundary Normalized soil water content at layer boundary. 
!          Assume the Se at top of layer L+1 as the boundary of layer L and layer L+1
! Se_mid(L)Normalized soil water content at middle of given layer
!          Measured from soil surface
! Se_top(L)Normalized soil water content at the top of given layer
!          Measured from soil surface
! ThetaCap An array of volumetric soil water contents at the midpoint of each soil layer.
!          Calculated from the water characteristic curve at the height above the
!          water table. At equilibrium state, soil matric potential h = water table depth z
! Top      Depth of the top for current layer
!-----------------------------------------------------------------------
!     END SUBROUTINE CapFringe
!=======================================================================

!=======================================================================
!  ThetaCapOp, Subroutine
!  Output daily info for ThetaCap 
!-----------------------------------------------------------------------
!  REVISION       HISTORY
!  
!=======================================================================

    Subroutine ThetaCapOp(DYNAMIC,CONTROL,ISWITCH,MgmtWTD, SOILPROP)
!-----------------------------------------------------------------------
    USE ModuleDefs
    USE Cells_2D
    IMPLICIT NONE
    EXTERNAL GETLUN, HEADER, CAPFRINGE, YR_DOY, INCDAT
    SAVE
!-----------------------------------------------------------------------
    integer DYNAMIC, YR2, DY2, DAS, LUNThetaCap, LIMIT_2D, L
    INTEGER INCDAT, N1_LYR, N2_LYR
    REAL, INTENT(IN) :: MgmtWTD
    TYPE (ControlType), INTENT(IN) :: CONTROL
    Type(SoilType) , INTENT(INOUT):: SOILPROP
    TYPE (SwitchType)   ISWITCH

    REAL, DIMENSION(NL):: ThetaCap, DUL, LL, SAT, LrTop, DS !NLAYR
!   REAL ThetaCa(25)
    CHARACTER*17, PARAMETER :: ThetaCapOut = 'ThetaCap.OUT'
    LOGICAL FEXIST, DOPRINT
    !    CALL GET('MGMT','WATTAB',MgmtWTD)
    LIMIT_2D = BedDimension % LIMIT_2D 
    N1_LYR = Min (11, SOILPROP % NLAYR)
    N2_LYR = Min (25, SOILPROP % NLAYR) 
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
    IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      DOPRINT=.TRUE.
      IF (ISWITCH % IDETW .EQ. 'N') THEN
        DOPRINT=.FALSE.
      ENDIF
      IF (ISWITCH % ISWWAT .EQ. 'N') THEN
        DOPRINT=.FALSE.
      ENDIF
      IF (ISWITCH % IDETL /= 'D') THEN
        DOPRINT=.FALSE.
      ENDIF
      IF (.NOT. DOPRINT) RETURN
      DO L = 1, min(25, SOILPROP % NLAYR)
        DS(L) = SOILPROP % DS(L)
        LrTop(L) = DS(L) - SOILPROP % DLAYR(L)
        LL(L)   = SOILPROP % LL(L)
        DUL(L)  = SOILPROP % DUL(L)
        SAT(L)  = SOILPROP % SAT(L)
      EndDo
!       Open output file
        CALL GETLUN('ThetaCapOut', LUNThetaCap)
        INQUIRE (FILE = ThetaCapOut, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = LUNThetaCap, FILE = ThetaCapOut, STATUS = 'OLD', &
          POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = LUNThetaCap, FILE = ThetaCapOut, STATUS = 'NEW')
          WRITE(LUNThetaCap,'("*Daily ThetaCap for each Soil layer")')
        ENDIF
      CALL HEADER(SEASINIT, LUNThetaCap, CONTROL % RUN)
!      if (CONTROL % RUN .eq. 1) then
      !  WRITE (LUNThetaCap, '(I2)') &
      !      ('! LIMIT_2D is ', LIMIT_2D) 
  !    Endif
 
      WRITE (LUNThetaCap,1115, ADVANCE='NO')'!    Variable ' 
      WRITE (LUNThetaCap,1116) ("LYR",L, L=1,N1_LYR), ("LYR",L, L=13,N2_LYR, 2)
      WRITE (LUNThetaCap,1115, ADVANCE='NO')'!   Top Layer'
      WRITE (LUNThetaCap,1118) (LrTop(L),L=1,N1_LYR), (LrTop(L),L=13,N2_LYR,2)
      WRITE (LUNThetaCap,1115, ADVANCE='NO')'!Bottom Layer'
      WRITE (LUNThetaCap,1118) (DS(L),L=1,N1_LYR), (DS(L),L=13,N2_LYR,2)
      WRITE (LUNThetaCap,1115, ADVANCE='NO')'!          LL'
      WRITE (LUNThetaCap,1119) (LL(L),L=1,N1_LYR), (LL(L),L=13,N2_LYR,2)
      WRITE (LUNThetaCap,1115, ADVANCE='NO')'!         DUL'
      WRITE (LUNThetaCap,1119) (DUL(L),L=1,N1_LYR), (DUL(L),L=13,N2_LYR,2)
      WRITE (LUNThetaCap,1115, ADVANCE='NO')'!         SAT'
      WRITE (LUNThetaCap,1119) (SAT(L),L=1,N1_LYR), (SAT(L),L=13,N2_LYR,2)
      WRITE (LUNThetaCap,1115)
 
      WRITE (LUNThetaCap,1120, ADVANCE='NO')'@YEAR DOY DAS '
      WRITE (LUNThetaCap,1116, ADVANCE='NO') ("LYR",L, L=1,N1_LYR), &
         ("LYR",L, L=13,N2_LYR, 2)
      WRITE (LUNThetaCap,1120)'  mgWTD LIMIT_2D'
       CALL CapFringe(            & 
          MgmtWTD,  SOILPROP,   &    !Input
          ThetaCap)                  !Output
      
      CALL YR_DOY(INCDAT(CONTROL % YRDOY,-1),YR2,DY2)
      WRITE (LUNThetaCap,1300, ADVANCE='NO') YR2, DY2, DAS, &
           (ThetaCap(L),L=1,N1_LYR), (ThetaCap(L),L=13,N2_LYR,2)
      WRITE (LUNThetaCap,1400) MgmtWTD, LIMIT_2D   
      
!***********************************************************************
!     DAILY RATE CALCULATIONS
!***********************************************************************
    ELSEIF (DYNAMIC .EQ. RATE) THEN        
      CALL CapFringe(            & 
          MgmtWTD,  SOILPROP,   &    !Input
          ThetaCap)                  !Output
      
      CALL YR_DOY(CONTROL % YRDOY, YR2, DY2)
      DAS = CONTROL % DAS
      IF (.NOT. DOPRINT) RETURN
      WRITE (LUNThetaCap,1300, ADVANCE='NO') YR2, DY2, DAS, &
           (ThetaCap(L),L=1,N1_LYR), (ThetaCap(L),L=13,N2_LYR,2)
      WRITE (LUNThetaCap,1400) MgmtWTD, LIMIT_2D   
 1115 FORMAT(13A)
 1116 FORMAT(9(2X,A3,I1), 9(1X,A3,I2)) 
 1118 FORMAT(1x, 18F6.1) 
 1119 FORMAT(1x, 18F6.2) 
 1120 FORMAT(14A)  
 1121 FORMAT(A1, 7X, A5, 1X, 18F6.1, F6.1,3X)    
 1122 FORMAT(A1, 7X, A5, 1X, 18F6.3, F6.1,3X)   
 1300 FORMAT(1X,I4,1X,I3,1X,I3, 1X, 18F6.3, F6.1,3X, I2) 
 1400 FORMAT(F7.0,3X, I2) 
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************  
    Endif  
    RETURN
  End Subroutine ThetaCapOp
!=======================================================================
!=======================================================================
!     ThetaCapOp VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!     END SUBROUTINE ThetaCapOp
!=======================================================================