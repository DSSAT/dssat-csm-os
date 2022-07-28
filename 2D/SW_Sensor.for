!=======================================================================
!  SW_SensorD, Subroutine
!  Work with PraxSoft sensor depth
!  2D water balance for drip irrigation bed with
!     plastic mulch cover.   
! SUBROUTINE READASensor(FILEA, PATHEX, OLAB, TRTNUM, X) read file A for sensor location
!-----------------------------------------------------------------------
!=======================================================================

      SUBROUTINE SW_SensorD(SOILPROP, CONTROL, Cells, SWV)       !Input
!-----------------------------------------------------------------------
      USE Cells_2D
      USE ModuleData
      IMPLICIT NONE
      SAVE

      TYPE (ControlType), INTENT(IN) :: CONTROL
      Type (CellType) Cells(MaxRows,MaxCols)
      TYPE (SoilType) SOILPROP
      
      REAL, DIMENSION(MaxRows,MaxCols) :: SWV
      !CHARACTER*8, PARAMETER :: ERRKEY = 'SW_SensorD'
!     CHARACTER*78 MSG(30) 
      INTEGER i, j, L, DYNAMIC    !, NLAYR, LIMIT_2D, iPDAT, iHDAT
      INTEGER LUNIO
!     INTEGER, DIMENSION(MaxRows,MaxCols) :: Cell_Type

!     REAL, DIMENSION(NL) :: BD, DLAYR, DS
!     REAL, DIMENSION(MaxRows,MaxCols) :: CellArea
!     REAL, DIMENSION(MaxRows,MaxCols) :: Thick, Width
      LOGICAL FEXIST
      
      REAL, DIMENSION(8):: SS_SW
      Integer LUNWBLxmlD, YR2, DY2, FurCol1
      Integer RowIndx(10,2), ColIndx(10,2)    !CellDTLx, CellDTLy, 
!     CHARACTER*6  SSLayerText(4)
      CHARACTER*30 FILEIO
      CHARACTER*6 X(EvaluateNum)
      INTEGER TRTNUM
      
	CHARACTER*17, PARAMETER :: SWBALxmlD = 'SWSensorD.OUT'
	Real SSDep(10), SSOfset(10) !, SSDS(4)
 
    !  Common / SensorDepth/SSDS
      DYNAMIC = CONTROL % DYNAMIC
      TRTNUM =  CONTROL % TRTNUM
      FILEIO  = CONTROL % FILEIO
!***********************************************************************      
!     Seasonal initialization - run once per season
!***********************************************************************
      !IF (DYNAMIC .EQ. SEASINIT) THEN
      IF (DYNAMIC .EQ. RUNINIT) THEN
C-----------------------------------------------------------------------
!     Read Measured (measured) data from FILEA
!-----------------------------------------------------------------------
      CALL READASensor (FILEIO, LUNIO, TRTNUM, X)
       !READ(X(1),'(I7)')iPDAT
	 !READ(X(2),'(I7)')iHDAT
	 READ(X(1),'(F6.1)') SSDep(1) ! soil sensor 1 
	 READ(X(2),'(F6.1)') SSOfset(1) 
	 READ(X(3),'(F6.1)') SSDep(2) ! Soil sensor 2
       READ(X(4),'(F6.1)') SSOfset(2) 
       READ(X(5),'(F6.1)') SSDep(3) ! soil sensor 3
	 READ(X(6),'(F6.1)') SSOfset(3)
	 READ(X(7),'(F6.1)') SSDep(4) ! soil sensor4
       READ(X(8),'(F6.1)') SSOfset(4)
       READ(X(9),'(F6.1)') SSDep(5) ! soil sensor 1 
	 READ(X(10),'(F6.1)') SSOfset(5)
	 READ(X(11),'(F6.1)') SSDep(6) ! Soil sensor 2
       READ(X(12),'(F6.1)') SSOfset(6) 
       READ(X(13),'(F6.1)') SSDep(7) ! soil sensor 3
	 READ(X(14),'(F6.1)') SSOfset(7)
	 READ(X(15),'(F6.1)') SSDep(8) ! soil sensor4
       READ(X(16),'(F6.1)') SSOfset(8)
       READ(X(17),'(I10)') Cell_detail%row
       if (Cell_detail%row .EQ. -99) Cell_detail%row = 1
       READ(X(18),'(I10)') Cell_detail%col
       if (Cell_detail%col .EQ. -99) Cell_detail%col = 1
!      SSDS(1) = ss1x
!      SSDS(2) = ss2x
!      SSDS(3) = ss3x
!      SSDS(4) = ss4x
       Call CalSS_CellIndex(SOILPROP, Cells, SSDep, SSOfset, RowIndx, 
     &       ColIndx )
      FurCol1 = BedDimension % FurCol1
!***********************************************************************
!     Open output file
!-----------------------------------------------------------------------------
      CALL GETLUN('SWBALxmlD', LUNWBLxmlD)
      INQUIRE (FILE = SWBALxmlD, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = LUNWBLxmlD, FILE = SWBALxmlD, STATUS = 'OLD',
     &    POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = LUNWBLxmlD, FILE = SWBALxmlD, STATUS = 'NEW')
  !      OPEN (UNIT = LUNWBLxmlD, FILE = SWBALxmlD, STATUS = 'REPLACE')
        WRITE(LUNWBLxmlD,'("*Daily Soil Water BALANCE OUTPUT FILE")')
      ENDIF
      CALL HEADER(SEASINIT, LUNWBLxmlD, CONTROL % RUN)
      if (CONTROL % RUN .eq. 1) then
!        WRITE (LUNWBLxmlD, '(4(A,I1,A4,F7.2,/))')
!     &       (('! Depth of Sensor',L, ' is ', SSDep(L),
!     &         '! Offset of Sensor',L, ' is ', SSOfset(L),
!     &         '! Row Index of Sensor',L, ' is ', RowIndx(L),
!     &         '! Col index of Sensor',L, ' is ', ColIndx(L) ), L=1,8) 
      Endif
      WRITE (LUNWBLxmlD, '("!",T23,"Sensor 1", T35,"Sensor 2", T47, 
     &      "Sensor 3", T59, "Sensor 4", T71, "Sensor 5", T83, 
     &      "Sensor 6", T95, "Sensor 7", T107, "Sensor 8")') 
      WRITE (LUNWBLxmlD, '(A, 8(F6.2, 6X))')
     &      '!Depth of Sensor (cm) ', (SSDep(L), L=1,8)  
      WRITE (LUNWBLxmlD, '(A, 8(F6.2, 6X))')
     &      '!Offset of Sensor (cm)', (SSOfset(L), L=1,8)
      WRITE (LUNWBLxmlD, '(A, 8(I2, A,I2,6X))') 
     &       '!Row Index of Sensor  ', 
     &        (RowIndx(L,1), ' &', RowIndx(L,2), L=1,8)
      WRITE (LUNWBLxmlD, '(A, 8(I2, A,I2,6X))') 
     &       '!Col Index of Sensor  ', 
     &       (ColIndx(J, 1), ' &', ColIndx(J, 2), J=1,8)
      WRITE (LUNWBLxmlD, '(A)') '!'
      WRITE (LUNWBLxmlD,1123)
! 1123   FORMAT('@YEAR DOY   DAS',
!     &    '   LYR1max LYR2max LYR3max LYR4max  LYR1min LYR2min '
!     &    'LYR3min LYR4min  LYR1avg LYR2avg LYR3avg LYR4avg'
!     &    '  LYR1ctr LYR2ctr LYR3ctr LYR4ctr')
 1123   FORMAT('@YEAR DOY   DAS   ',
    ! &    '    SW1Max  SW1Min   SW2Max  SW2Min   SW3Max  SW3Min   '
     &    '    SW_SS1      SW_SS2      SW_SS3      '
    ! &    'SW4Max  SW4Min   SW5Max  SW5Min   SW6Max  SW6Min'
     &    'SW_SS4      SW_SS5      SW_SS6   '
    ! &    '   SW7Max  SW7Min   SW8Max  SW8Min')   
     &    '   SW_SS7      SW_SS8   ')  


!***********************************************************************
!     OUTPUT - Daily output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
C-----------------------------------------------------------------------
!-----------------------------------------------------------------
!        SWV    = CELLS % State % SWV
!        Call CalSS_SW(SOILPROP, SWV, SSDS, SS_SW )
!       Call CalSS_CellIndex(SOILPROP, SSDep, SSOfset, RowIndx, ColIndx )
        DO i = 1, 8
          SS_SW(i) =( SWV(RowIndx(i,1),ColIndx(i,1)) + 
     &               SWV(RowIndx(i,2),ColIndx(i,2)) )/2
        Enddo

1300   FORMAT(1X,I4,1X,I3.3,1X,I5, 5X, 8(F8.3,4x))       !Inflows

        CALL YR_DOY(CONTROL % YRDOY, YR2, DY2)

         WRITE (LUNWBLxmlD,1300) YR2, DY2, CONTROL % DAS, 
     &    SS_SW(1), SS_SW(2), SS_SW(3), SS_SW(4),
     &    SS_SW(5), SS_SW(6), SS_SW(7), SS_SW(8)    
    
!------------------------------------------------------------
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE SW_SensorD

!=======================================================================
C=====================================================================
!     SW_SensorD VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! EvaluateNum Number of evaluation variables. It equals 40 in ModuleDefs
! SWV         Single precision cell soil water content in mm3/mm3  

! Width(Row,Col)Cell width in cm[soil]
!-----------------------------------------------------------------------
!     END SUBROUTINE SW_SensorD
!=======================================================================
!=======================================================================
!  SW_SensorH, Subroutine
!  Work with PraxSoft sensor depth
!  2D water balance for drip irrigation bed with
!     plastic mulch cover.  
!-----------------------------------------------------------------------
!=======================================================================

      SUBROUTINE SW_SensorH(SOILPROP, CONTROL, Cells,SWV,iHr)  
!-----------------------------------------------------------------------
      USE Cells_2D
      USE ModuleData
      IMPLICIT NONE
      SAVE

      TYPE (ControlType), INTENT(IN) :: CONTROL
      Type (CellType) Cells(MaxRows,MaxCols)
      TYPE (SoilType) SOILPROP
      
      REAL, DIMENSION(MaxRows,MaxCols) :: SWV
      CHARACTER*8, PARAMETER :: ERRKEY = 'SW_SensorH'
!     CHARACTER*78 MSG(30)
      INTEGER i, j, L, DYNAMIC, iHr, iHDAT !, NLAYR, LIMIT_2D, iPDAT
      INTEGER LUNIO
!     INTEGER, DIMENSION(MaxRows,MaxCols) :: Cell_Type
      Integer RowIndx(10,2), ColIndx(10,2)    !CellDTLx, CellDTLy, 

!     REAL, DIMENSION(NL) :: BD, DLAYR, DS
!     REAL, DIMENSION(MaxRows,MaxCols) :: CellArea
!     REAL, DIMENSION(MaxRows,MaxCols) :: Thick, Width
      LOGICAL FEXIST
      
      REAL, DIMENSION(8) :: SS_SW
      !REAL, DIMENSION(4):: SS_SWmax, SS_SWmin, SS_SWavg 
      Integer LUNWBLxmlH, YR2, DY2, FurCol1
!     CHARACTER*6  SSLayerText(4)
      !CHARACTER*12 FILEA
      CHARACTER*30 FILEIO
	!CHARACTER*80 PATHEX
	!CHARACTER*6, DIMENSION(EvaluateNum) :: OLABS
      CHARACTER*6 X(EvaluateNum)
      INTEGER TRTNUM
      
	CHARACTER*17, PARAMETER :: SWBALxmlH = 'SWSensorH.OUT'
	! Real ssd1, ssd2, ssd3, ssd4, SSDS(4)
	Real SSDep(10), SSOfset(10)
!     Define headings for observed data file (FILEA)
!      DATA OLABS / !Pred.          Obs.   Definition 
!     & 'SDEP1  ', !1  Depth of first soil water sensor (cm)   
!     & 'SOFS1  ', !2 Offset of first soil water sensor (cm)         
!     & 'SDEP2  ', !3 Depth of second soil water sensor (cm)   
!     & 'SOFS2  ', !4 Offset of second soil water sensor (cm)        
!     & 'SDEP3  ', !5 Depth of third soil water sensor (cm)          
!     & 'SOFS3  ', !6 Offset of third soil water sensor (cm)  
!     & 'SDEP4  ', !7 Depth of 4th soil water sensor (cm) 
!     & 'SOFS4  ', !8 Offset of 4th soil water sensor (cm) 
!     & 'SDEP5  ', !9  Depth of 5th soil water sensor (cm)   
!     & 'SOFS5  ', !10 Offset of 5th soil water sensor (cm)         
!     & 'SDEP6  ', !11 Depth of 6th soil water sensor (cm)   
!     & 'SOFS6  ', !12 Offset of 6th soil water sensor (cm)        
!     & 'SDEP7  ', !13 Depth of 7th soil water sensor (cm)          
!     & 'SOFS7  ', !14 Offset of 7th soil water sensor (cm)  
!     & 'SDEP8  ', !15 Depth of 8th soil water sensor (cm) 
!     & 'SOFS8  ', !16 Offset of 8th soil water sensor (cm) 
!     & 'DTLRW  ', !17 Row index for detail output cll (integer)
!     & 'DTLCL  ', !18 Col index for detail output cll (integer)
!     &  22 * '      '/   
  !    Common / SensorDepth/SSDS
      DYNAMIC = CONTROL % DYNAMIC
      TRTNUM =  CONTROL % TRTNUM
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO
     
!***********************************************************************      
!     Seasonal initialization - run once per season
!***********************************************************************
      !IF (DYNAMIC .EQ. SEASINIT) THEN
      IF (DYNAMIC .EQ. RUNINIT) THEN
C-----------------------------------------------------------------------
!     Read Measured (measured) data from FILEA
!-----------------------------------------------------------------------
      ! CALL READA (FILEA, PATHEX, OLABS, TRT_ROT, YRSIM, X)
      CALL READASensor (FILEIO, LUNIO, TRTNUM, X)
   !    READ(X,'(4F10.0)') SSDS(1), SSDS(2), SSDS(3), SSDS(4)
	 READ(X(1),'(F6.1)') SSDep(1) ! soil sensor 1 
	 READ(X(2),'(F6.1)') SSOfset(1) 
	 READ(X(3),'(F6.1)') SSDep(2) ! Soil sensor 2
       READ(X(4),'(F6.1)') SSOfset(2) 
       READ(X(5),'(F6.1)') SSDep(3) ! soil sensor 3
	 READ(X(6),'(F6.1)') SSOfset(3)
	 READ(X(7),'(F6.1)') SSDep(4) ! soil sensor4
       READ(X(8),'(F6.1)') SSOfset(4)
       READ(X(9),'(F6.1)') SSDep(5) ! soil sensor 1 
	 READ(X(10),'(F6.1)') SSOfset(5)
	 READ(X(11),'(F6.1)') SSDep(6) ! Soil sensor 2
       READ(X(12),'(F6.1)') SSOfset(6) 
       READ(X(13),'(F6.1)') SSDep(7) ! soil sensor 3
	 READ(X(14),'(F6.1)') SSOfset(7)
	 READ(X(15),'(F6.1)') SSDep(8) ! soil sensor4
       READ(X(16),'(F6.1)') SSOfset(8)
       READ(X(17),'(I10)') Cell_detail%row 
       if (Cell_detail%row .EQ. -99) Cell_detail%row = 1
       READ(X(18),'(I10)') Cell_detail%col
       if (Cell_detail%col .EQ. -99) Cell_detail%col = 1
      FurCol1 = BedDimension % FurCol1
       Call CalSS_CellIndex(SOILPROP, Cells, SSDep, SSOfset, RowIndx, 
     &       ColIndx )
!***********************************************************************
!     Open output file
!-----------------------------------------------------------------------------
      CALL GETLUN('SWBALxmlH', LUNWBLxmlH)
      INQUIRE (FILE = SWBALxmlH, EXIST = FEXIST)
      IF (FEXIST) THEN
       OPEN (UNIT = LUNWBLxmlH, FILE = SWBALxmlH, STATUS = 'OLD',
     &    POSITION = 'APPEND')
      ELSE
  !      OPEN (UNIT = LUNWBLxmlH, FILE = SWBALxmlH, STATUS = 'REPLACE')
        OPEN (UNIT = LUNWBLxmlH, FILE = SWBALxmlH, STATUS = 'NEW')
        WRITE(LUNWBLxmlH,'("*Hourly and daily Soil WATER BALANCE OUTPUT 
     &FILE")')
      ENDIF
      CALL HEADER(SEASINIT, LUNWBLxmlH, CONTROL % RUN)
      if (CONTROL % RUN .EQ.1) then
        WRITE (LUNWBLxmlH, '(A, I7,/)')
     &  '!Farmers predicted harvest date is ',iHDAT
      endif
      WRITE (LUNWBLxmlH, '("!",T23,"Sensor 1", T35,"Sensor 2", T47, 
     &      "Sensor 3", T59, "Sensor 4", T71, "Sensor 5", T83, 
     &      "Sensor 6", T95, "Sensor 7", T107, "Sensor 8")') 
      WRITE (LUNWBLxmlH, '(A, 8(F6.2, 6X))')
     &      '!Depth of Sensor (cm) ', (SSDep(L), L=1,8)  
      WRITE (LUNWBLxmlH, '(A, 8(F6.2, 6X))')
     &      '!Offset of Sensor (cm)', (SSOfset(L), L=1,8)
      WRITE (LUNWBLxmlH, '(A, 8(I2, A,I2,6X))') 
     &       '!Row Index of Sensor  ', 
     &        (RowIndx(L,1), ' &', RowIndx(L,2), L=1,8)
      WRITE (LUNWBLxmlH, '(A, 8(I2, A,I2,6X))') 
     &       '!Col Index of Sensor  ', 
     &       (ColIndx(J, 1), ' &', ColIndx(J, 2), J=1,8)
      WRITE (LUNWBLxmlH, '(A)') '!'
      WRITE (LUNWBLxmlH,1123)
! 1123   FORMAT('@YEAR DOY   DAS',
!     &    '   LYR1max LYR2max LYR3max LYR4max  LYR1min LYR2min '
!     &    'LYR3min LYR4min  LYR1avg LYR2avg LYR3avg LYR4avg'
!     &    '  LYR1ctr LYR2ctr LYR3ctr LYR4ctr')
1123   FORMAT('@YEAR DOY   DAS  Hour ',
     &    ' SW_SS1      SW_SS2      SW_SS3      '
     &    'SW_SS4      SW_SS5      SW_SS6   ' 
     &    '   SW_SS7      SW_SS8   ')  


!***********************************************************************
!     DAILY RATE CALCULATIONS
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
C-----------------------------------------------------------------------
!-----------------------------------------------------------------
 !       Call CalSS_SW(SOILPROP, SWV, SSDS, SS_SW )
        DO i = 1, 8
          SS_SW(i) =( SWV(RowIndx(i,1),ColIndx(i,1)) + 
     &               SWV(RowIndx(i,2),ColIndx(i,2)) )/2
        Enddo
        !     IF (IDETL .EQ. 'D') THEN
        !Write header for daily output
   !     WRITE (LUNWBLxmlH, '("!",T7,"Soil Layer depths (cm):",7A8,
   !  &      " Soil Layer depths (cm):")') (" ",L=1,N_LYR-3)
        

 
!          WRITE (LUNWBLxmlH,1122,ADVANCE='NO') ("SW",L,"D",L=1,9),"    SW10"
! 1122     FORMAT(9("    ",A2,I1,A1),A8)
!          WRITE(LUNWBLxmlH,'(9("    ",A2,I1,A1),A8)')("TCSW",L,"D",L=1,9)
!     &              ,"    TCSW10"
  !    ENDIF
1300   FORMAT(1X,I4,1X,I3.3,1X,I5, 2X,I2,1X, 8(F8.3,4x))       !Inflows
        CALL YR_DOY(CONTROL % YRDOY, YR2, DY2)
        !if (iHr. eq. 0) iHr=24
        WRITE (LUNWBLxmlH,1300) YR2, DY2, CONTROL % DAS, iHr,
     &    SS_SW(1), SS_SW(2), SS_SW(3), SS_SW(4),
     &    SS_SW(5), SS_SW(6), SS_SW(7), SS_SW(8)    
!------------------------------------------------------------
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE SW_SensorH

!=======================================================================
C=====================================================================
!     SW_SensorH VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! EvaluateNum Number of evaluation variables. It equals 40 in ModuleDefs
! SWV         Single precision cell soil water content in mm3/mm3  

! Width(Row,Col)Cell width in cm[soil]
!-----------------------------------------------------------------------
!     END SUBROUTINE SW_SensorH
!=======================================================================


!=======================================================================
!  CalSS_SW, Subroutine
!  Work with PraxSoft sensor depth
!  Calculate the soil water content for sensor layers
!=======================================================================
      Subroutine CalSS_SW(SOILPROP, SWV, SSDS, SS_SW )
      Use Cells_2D
      USE ModuleData
      Implicit None
      
      TYPE (SoilType) SOILPROP
      Real SSDS(4), SS_SW(4, NColsTot), Top, Bottom !, Thick
      INTEGER i, j, L, NLAYR, FurCol1 
!     REAL HalfFurrow, HalfRow
      Real, Dimension(MaxRows,MaxCols) :: SWV !CellArea, 
      REAL, DIMENSION(NL) :: DS   !DLAYR, 
      
!     CellArea = CELLS % Struc % CellArea !CELLS % Struc % Width , CELLS % Struc % Thick
      DS    = SOILPROP % DS 
      NLAYR = SOILPROP % NLAYR 
      FurCol1 = BedDimension % FurCol1
!     DLAYR = SOILPROP % DLAYR
     
      DO i = 1, 4
         DO j = 1, Furcol1-1
           DO L = 1, NLAYR 
              IF (L == 1) THEN 
                Top = 0.
              ELSE
                Top = DS(L-1)
              ENDIF
              Bottom = DS(L)
              IF ((SSDS(i) > TOP) .and. (SSDS(i) < Bottom) )THEN
!               This sensor depth is within a simulation layer layer; done.
                SS_SW(i, j) = SWV(L, j)
              ELSEIF (SSDS(i) == Bottom) THEN
!               This sensor layer is in between of two simulation layers;  
                SS_SW(i,j) = (SWV(L, j) + SWV(L+1, j))/2
              ELSE
                Cycle
!               This sensor layer is out of current simulation layer;
              Endif
           Enddo
        ENDDO
      ENDDO

      RETURN
      END Subroutine CalSS_SW
!=======================================================================
C=====================================================================
!     CalSS_SW VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!     END SUBROUTINE CalSS_SW
!=======================================================================
!=======================================================================
!  CalSS_SW, Subroutine
!  Work with PraxSoft sensor depth
!  Calculate the soil water content for sensor layers
!=======================================================================
      Subroutine CalSS_CellIndex(SOILPROP, Cells, SSDep, SSOfset,
     &            RowIndx,ColIndx)
      Use Cells_2D
      USE ModuleData
      Implicit None
      
      TYPE (SoilType) SOILPROP
      Real Top, Bottom, Left, Right !Thick, SSDS(4), SS_SW(4, NColsTot), 
      INTEGER i, j, L, NLAYR, FurCol1, RowIndx(10,2), ColIndx(10,2)
      REAL SSDep(10), SSOfset(10) !HalfFurrow, HalfRow, 
      REAL, DIMENSION(MaxRows,MaxCols) :: Width
      !Real, Dimension(MaxRows,MaxCols) :: CellArea, SWV
      REAL, DIMENSION(NL) :: DS   !DLAYR, 
      TYPE(CellType), DIMENSION(MaxRows,MaxCols), INTENT(IN) :: CELLS
!     INTEGER, DIMENSION(MaxRows,MaxCols) :: Cell_type
      
!      CellArea = CELLS % Struc % CellArea !CELLS % Struc % Width , CELLS % Struc % Thick
      DS    = SOILPROP % DS 
      NLAYR = SOILPROP % NLAYR 
      FurCol1 = BedDimension % FurCol1
      Width = CELLS % Struc % Width
      RowIndx = 1
      ColIndx = 1 ! set default value if there is no A file available
!      DLAYR = SOILPROP % DLAYR
     
      DO i = 1, 8
           DO L = 1, NLAYR 
              IF (L == 1) THEN 
                Top = 0.
              ELSE
                Top = DS(L-1)
              ENDIF
              Bottom = DS(L)
              IF ((SSDep(i) > TOP) .and. (SSDep(i) < Bottom) )THEN
!               This sensor depth is within a simulation layer layer; done.
                RowIndx(i,1) = L
                RowIndx(i,2) = L
              ELSEIF (SSDep(i) == Bottom) THEN
!               This sensor layer is in between of L and (L+1) simulation layers, row index will be L+1;  
                RowIndx(i,1) = L
                RowIndx(i,2) = L+1
              ELSE
                Cycle
!               This sensor layer is out of current simulation layer;
              Endif
           Enddo
      ENDDO
      DO j = 1, 8
        if (SSOfset(j) == 0.0) then 
           ColIndx(j,1) = 1
           ColIndx(j,2) = 1
        else
           Right = 0
           DO L = 1, MaxCols 
              Left = Right
              Right = Left + Width(1, L)
              IF ((SSOfset(j) > Left) .and. (SSOfset(j) < Right) )THEN
!               This sensor depth is within a simulation layer layer; done.
                ColIndx(j,1) = L
                ColIndx(j,2) = L
                exit
              ELSEIF (SSOfset(j) == Right) THEN
!               This sensor layer is in between of two simulation layers; 
                ColIndx(j,1) = L 
                ! if (L == (FurCol1-1) ) then
!               bug, need handle if last col of Furrow or last row of bed
                if (L == MaxCols ) then 
                  ColIndx(j,2) = L
                else 
                  ColIndx(j,2) = L+1
                endif 
                exit
              ELSE
                Cycle
!               This sensor layer is out of current simulation layer;
              Endif
           Enddo
        endif
      ENDDO
     
      RETURN
      END Subroutine CalSS_CellIndex
!=======================================================================
C=====================================================================
!     CalSS_CellIndex VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!     END SUBROUTINE CalSS_CellIndex
!=======================================================================
!=======================================================================
!  Subroutine READASeasor
!   Reads measured development and final harvest data from FILEA 
!   and maps measured data to appropriate headers for output to 
!   OVERVEIW.OUT
!-----------------------------------------------------------------------
!  Revision history:
!  08/12/2005 CHP Modified to read "alias" headers for some variables
C  02/09/2007 GH  Add path for fileA
!=======================================================================
      SUBROUTINE READASensor(FILEIO, LUNIO, TRTNUM, X)

!-----------------------------------------------------------------------
!     READ DEVELOPMENT AND FINAL HARVEST DATA FROM  FILEA
!-----------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE

      INTEGER TRTNUM,ERRNUM,LUNA,LINEXP,ISECT,NTR,I, J
!     INTEGER YR,ISIM
      INTEGER COUNT, LUNIO, ISENS, LNUM
!     Headers with aliases -- save column
!     INTEGER HWAM, HWAH, BWAM, BWAH, PDFT, R5AT  

!     REAL TESTVAL

      CHARACTER*6   OLABS(EvaluateNum), HD
      CHARACTER*6   HEAD(EvaluateNum)
      CHARACTER*6   DAT(EvaluateNum), X(EvaluateNum)  !, ERRKEY
      CHARACTER*10, PARAMETER :: ERRKEY = 'ReadSensor'
      CHARACTER*12  FILEA
      CHARACTER*30 FILEIO
      CHARACTER*78  MSG(10)
	CHARACTER*80  PATHEX
	CHARACTER*92  FILEA_P
      CHARACTER*255 C255

      LOGICAL FEXIST
      DATA OLABS / !Pred.          Obs.   Definition 
     & 'SDEP1  ', !1  Depth of first soil water sensor (cm)   
     & 'SOFS1  ', !2 Offset of first soil water sensor (cm)         
     & 'SDEP2  ', !3 Depth of second soil water sensor (cm)   
     & 'SOFS2  ', !4 Offset of second soil water sensor (cm)        
     & 'SDEP3  ', !5 Depth of third soil water sensor (cm)          
     & 'SOFS3  ', !6 Offset of third soil water sensor (cm)  
     & 'SDEP4  ', !7 Depth of 4th soil water sensor (cm) 
     & 'SOFS4  ', !8 Offset of 4th soil water sensor (cm) 
     & 'SDEP5  ', !9  Depth of 5th soil water sensor (cm)   
     & 'SOFS5  ', !10 Offset of 5th soil water sensor (cm)         
     & 'SDEP6  ', !11 Depth of 6th soil water sensor (cm)   
     & 'SOFS6  ', !12 Offset of 6th soil water sensor (cm)        
     & 'SDEP7  ', !13 Depth of 7th soil water sensor (cm)          
     & 'SOFS7  ', !14 Offset of 7th soil water sensor (cm)  
     & 'SDEP8  ', !15 Depth of 8th soil water sensor (cm) 
     & 'SOFS8  ', !16 Offset of 8th soil water sensor (cm) 
     & 'DTLRW  ', !17 Row index for detail output cll (integer)
     & 'DTLCL  ', !18 Col index for detail output cll (integer)
     &  22 * '      '/   

!     INQUIRE (FILE = FILEIO,OPENED = FOPEN) ! check if the file was not closed?
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)

      READ (LUNIO,'(55X,I5)',IOSTAT=ERRNUM) ISENS; LNUM = 1   
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
      READ (LUNIO,'(3(/),15X,A12,1X,A80)',IOSTAT=ERRNUM) FILEA,
     &     PATHEX
      ! Why above statement make program crash?
      ! If remove SW_SensorD, then above statement will has no problem
      CLOSE (LUNIO)
      !CALL GETDESC(ACOUNT, OLABS, DESCRIP)
      
      
      FILEA_P = TRIM(PATHEX)//FILEA

C-----------------------------------------------------------------------
C     Initialize measured values to -99 before reading values
C
      X = '   -99'

      CALL GETLUN('FILEA', LUNA)
      LINEXP = 0

      INQUIRE (FILE = FILEA_P, EXIST = FEXIST)

      IF (FEXIST) THEN
        OPEN (LUNA,FILE = FILEA_P,STATUS = 'OLD',IOSTAT=ERRNUM)
        IF (ERRNUM .NE. 0) GOTO 5000
      !  CALL YR_DOY(YRSIM,YR,ISIM)

C       FIND THE HEADER LINE, DESIGNATED BY @TRNO
        DO WHILE (.TRUE.)
          READ(LUNA,'(A)',END=5000) C255
          LINEXP = LINEXP + 1
          IF (C255(1:1) .EQ. '@') EXIT    
        ENDDO

C       FOUND HEADER LINE, SAVE IT IN HEAD AND SEARCH FOR TREATMENT
!@TRNO   PDAT  HDAT SWSD1 SWSD2 SWSD3 SWSD4
        DO I = 1,EvaluateNum
          READ(C255,'(1X,A5)') HEAD(I)
          IF (HEAD(I) .EQ. '     ') THEN
            COUNT = I - 1
            EXIT
          ENDIF
          C255 = C255(7:255)
        ENDDO

C       FIND THE RIGHT TREATMENT LINE OF DATA
        DO I = 1,1000
          CALL IGNORE(LUNA,LINEXP,ISECT,C255)
C
C    Return if no matching treatment is found in file FILA
C    No field measured data are necessary to be able to run the
C    model

          IF (ISECT .EQ. 0) GO TO 100
  50        READ(C255(1:6),'(2X,I4)',IOSTAT=ERRNUM) NTR
          IF (ERRNUM .NE. 0) GOTO 5000
          IF(NTR .EQ. TRTNUM) GO TO 60
        ENDDO
  
  60    CONTINUE
  
C       READ DATA LINE
        DO I = 1,COUNT
          READ(C255,'(A6)',IOSTAT=ERRNUM) DAT(I)
          IF (ERRNUM .NE. 0) GOTO 5000

!          !Test for numeric value -- set non-numeric values to -99
!          READ(C255,'(F6.0)',IOSTAT=ERRNUM) TESTVAL
!          IF (ERRNUM .NE. 0 .AND. 
!     &        TRIM(ADJUSTL(HEAD(I))) .NE. 'TNAM') THEN
!            DAT(I) = '   -99'
!          ENDIF 

          C255 = C255(7:255)
        ENDDO


C       MATCH HEADER WITH DATA
        DO I = 2, COUNT   !Loop thru FILEA headers
          HD = ADJUSTL(HEAD(I))

!         
            DO J = 1, EvaluateNum    !Loop thru crop-specific headers
              IF (OLABS(J) == HD) THEN
                X(J) = DAT(I)
                EXIT
              ENDIF
            ENDDO

        ENDDO
  
 100    CONTINUE
        CLOSE(LUNA)
        RETURN

!       Error handling
 5000   CONTINUE
        X = '   -99'
        WRITE (MSG(1),'(" Error in FILEA - Measured data not used")')
        CALL INFO(1, "READA ", MSG)
      ENDIF

      CLOSE (LUNA)
      RETURN
      END SUBROUTINE READASensor
C=====================================================================
!     READASensor VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! ISECT   Data record code (0 - End of file encountered, 1 - Found a good 
!           line to read, 2 - End of Section in file encountered, denoted 
!           by * in column 1
! LNUM    Current line number of input file 
!-----------------------------------------------------------------------
!     END SUBROUTINE READASensor
!=======================================================================