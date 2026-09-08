!=====================================================================
!  OpSWxmin, Subroutine, Cheryl Porter
!  x-minute output interval for soil water content, all cells.
!  This file can be modified to change the value of "x", the print interval.
!  For time steps greater than 15 minutes, less frequent output is reported.
!  Each output includes instantaneous, average, minimum, and maximum values 
!    since the last report.
!    - The instantaneous value is reported at the end of the print interval.
!    - Max, min, and mean values are over the previous x minutes.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  06/17/2025 CHP written
!-----------------------------------------------------------------------
!  Called by: WATBAL_2D
!=====================================================================
      SUBROUTINE OpSWxmin(CONTROL, ISWITCH, 
     &    CELLS, Time, TimeIncr, SWV_D)  !Input
!     ------------------------------------------------------------------
      USE Cells_2D
      USE ModuleData
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, YR_DOY
      SAVE

      TYPE (ControlType), INTENT(IN) :: CONTROL
      TYPE (SwitchType), INTENT(IN) :: ISWITCH
      Type (CellType), INTENT(IN) :: CELLS(MaxRows,MaxCols)
      REAL, INTENT(IN) :: Time, TimeIncr
      Double Precision, DIMENSION(MaxRows,MaxCols), INTENT(IN) :: SWV_D 

      CHARACTER*17, PARAMETER :: SWV15 = 'SoilWat_15min.csv'
      INTEGER, PARAMETER :: MaxCount = 1000
      INTEGER DYNAMIC, LUNW15, I, count, row, col
      INTEGER YRDOY, YEAR, DOY, DAS

      REAL, DIMENSION(MaxRows,MaxCols) :: SWV_inst, SWV_max, SWV_min, 
     &                                    SWV_avg, SWV_ts
      REAL Last_print_time, Target_print_time, Last_clock_time, Sum_time
      REAL ThisTS, SWcell, Clock_ratio

!     Save up to 1000 values of SWV between reporting intervals
!     900 seconds per 15 minutes, so 1000 should be enough (?)
      TYPE Save_type
        REAL, DIMENSION(MaxRows,MaxCols) :: SWV
        REAL ts
      END TYPE Save_type
      TYPE (Save_type), Dimension(0:MaxCount) :: SW_save

      LOGICAL FEXIST, DOPRINT

!!     temp chp - print info for one cell
!      integer r1,c1
!      r1 = 2
!      c1 = 3

!     ------------------------------------------------------------------
      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY
      DAS     = CONTROL % DAS

      SWV_ts = SNGL(SWV_D)

!***********************************************************************
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
!     ------------------------------------------------------------------

!     ------------------------------------------------------------------
!     Open output file SoilWat_15min.csv
      CALL GETLUN(SWV15, LUNW15)
      INQUIRE (FILE = SWV15, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = LUNW15, FILE = SWV15, STATUS = 'OLD',
     &    POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = LUNW15, FILE = SWV15, STATUS = 'NEW')
        WRITE(LUNW15,'("*2D Soil Water, 15-minute interval")')
      ENDIF

!     Write header for daily output
      WRITE (LUNW15,'(A)')
     &  'YEAR,DOY,DAS,TIME,Row,Col,SWV_inst,SWV_min,SWV_avg,SWV_max'

!     Write initial value for instantaneous soil water
      DO row = 1, nRowsTot
        DO col = 1, nColsTot
          SW_save % SWV(row,col) = 0.0 

!         Process cell types 3, 4, and 5 only
          SELECT CASE(CELLS(row,col)%STRUC%Cell_Type)
          CASE (3,4,5);CONTINUE
          CASE DEFAULT; CYCLE
          END SELECT
          
          CALL YR_DOY(YRDOY, YEAR, DOY)
          WRITE (LUNW15,'(6(g0,","),g0)') YEAR, DOY, DAS+1, 0.0,
     &           SWV_ts(row,col) 
        ENDDO
      ENDDO

!     These times are in hours on a 24 hour clock. 
      Last_print_time = 0.0
      Target_print_time = 0.25
      count = 0
      Last_clock_time = 0.0
      Sum_time = 0.0  !duration of time since last printout

      SW_save(0) % ts  = 0.0
      SW_save(0) % SWV = SWV_ts

!!     temp chp
!!     for one cell, print at every time step (unit 6123) and at every print interval (unit 6124)
!      write(6123,'(A,/,7(g0,","),g0)') 
!     &  "YEAR,DOY,DAS,TIME,DeltaT,ROW,COL,SWV_ts", 
!     &  year, doy, das+1, 0.0, 0.0, r1, c1, swv_ts(r1,c1)
!
!      write(6124,'(A,A,/,8(g0,","),g0)') "YEAR,DOY,DAS,TIME,DeltaT,",
!     &  "count,ROW,COL,SWV_inst,SWV_min,SWV_avg,SWV_max",
!     &  year, doy, das+1, 0.0, 0.0, 0, r1, c1, swv_ts(r1,c1)

!***********************************************************************
!***********************************************************************
!     Time step OUTPUT 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
      IF (.NOT. DOPRINT) RETURN
!     ------------------------------------------------------------------
!!     temp chp
!      write(6123,'(7(g0,","),g0)') 
!     &  year, doy, das, time, TimeIncr, r1, c1, swv_ts(r1,c1)

      IF (count == MaxCount - 1) THEN
        Target_print_time = TIME
      ENDIF

!     15-minute SWV output for all cells
      IF (TIME - Target_print_time >= -1E-5) THEN

!       Handle time steps larger than 15 minutes 
!       Skip some print steps rather than interpolate between values.
        DO WHILE (TIME - Target_print_time >= 0.25)
          Target_print_time = Target_print_time + 0.25
        ENDDO

!       It's time to print, save last value for aggregation
        count = count + 1
!       Partial time step ending in Target_print_time
        SW_save(count) % ts  = Target_print_time - Last_clock_time


        IF (ABS(Target_print_time - TIME) < 0.001) THEN
!         Print time and clock time are in synch
          SW_save(count) % SWV = SWV_ts
        ELSE
!         Interpolate last SWV value at the target print time
          Clock_ratio = SW_save(count) % ts / (TIME - Last_clock_time)
          DO row = 1, nRowsTot
            DO col = 1, nColsTot
              SW_save(count) % SWV(row,col) = 
     &          SW_save(count-1) % SWV(row,col)
     &          + (SWV_ts(row,col) - SW_save(count-1) % SWV(row,col))
     &          * Clock_ratio
            ENDDO
          ENDDO
        ENDIF

        SWV_min = SW_save(0) % SWV
        SWV_max = SW_save(0) % SWV
        SWV_avg = 0.0

        DO i = 1, count
!         To make it easy to read
          ThisTS = SW_save(i) % ts  !current time step in hours
          Sum_time = Sum_time + ThisTS  !hours

          DO row = 1, nRowsTot
            DO col = 1, nColsTot
!             Process cell types 3, 4, and 5 only
              SELECT CASE(CELLS(row,col)%STRUC%Cell_Type)
              CASE (3,4,5);CONTINUE
              CASE DEFAULT; CYCLE
              END SELECT
          
!             Soil water content for this cell, this time step
              SWcell = SW_save(i) % SWV(row,col)

!             Minimum over the print interval
              IF (SWcell < SWV_min(row,col)) THEN
                SWV_min(row,col) = SWcell
              ENDIF

!             Maximum over the print interval
              IF (SWcell > SWV_max(row,col)) THEN
                SWV_max(row,col) = SWcell
              ENDIF

!             Average is weighted average over the 15-minute print interval
              SWV_avg(row,col) = SWV_avg(row,col) + 
     &          (SWcell + SW_save(i-1) % SWV(row,col)) / 2.0 * ThisTS
!               (current SWV + last time step SWV) / 2.0 * time step

!             The instantaneous value is the SWV calculated at the print time
              IF (i == count) THEN
                SWV_inst(row,col) = SW_save(count) % SWV(row,col)
              ENDIF
            ENDDO
          ENDDO
        ENDDO

        SWV_avg = SWV_avg / Sum_time

!       Write initial value for instantaneous soil water
        DO row = 1, nRowsTot
          DO col = 1, nColsTot
            SELECT CASE(CELLS(row,col)%STRUC%Cell_Type)
            CASE (3,4,5);CONTINUE
            CASE DEFAULT; CYCLE
            END SELECT

            CALL YR_DOY(YRDOY, YEAR, DOY)
            WRITE (LUNW15,'(10(g0,","),g0)') 
     &        YEAR, DOY, DAS, Target_print_time, Sum_time, row, col, 
     &        SWV_inst(row,col), SWV_min(row,col), 
     &        SWV_avg(row,col), SWV_max(row,col) 
          ENDDO
        ENDDO

!!       temp chp
!        write(6124,'(11(g0,","),g0)')year, doy, das, Target_print_time,
!     &    Sum_time, count, r1, c1, 
!     &    SWV_inst(r1,c1), SWV_min(r1,c1), 
!     &    SWV_avg(r1,c1), SWV_max(r1,c1) 

!       Initialize arrays for next print interval
        count = 0  
        Sum_time = 0.0
        SW_save % ts  = 0.0
        DO row = 1, nRowsTot
          DO col = 1, nColsTot
            SW_save % SWV(row,col) = 0.0 
          ENDDO
        ENDDO

        SW_save(0) % SWV = SWV_inst   !SWV at last print time
        IF (ABS(Last_print_time - Last_clock_time) > 0.001) THEN
!         Print time and clock time are not in synch. Need to save the extra
!           bit of time beyond the last print time as the first element of the array.
          count = 1
          SW_save(1) % SWV = SWV_ts     !SWV at current clock time
          SW_save(1) % ts  = TIME - Last_print_time 
        ENDIF

        Last_clock_time = TIME                      !hours
        Last_print_time = Target_print_time
        Target_print_time = Target_print_time + 0.25

!       Is this the last time interval of the day?
        IF (ABS(TIME - 24.) < 0.01) THEN
!         End of day
          count = 0
          Last_clock_time = 24. - Last_clock_time
          Last_print_time = 0.0
          Target_print_time = 0.25
        ENDIF

      ELSE
!       Save values for later aggregation
        count = count + 1
        SW_save(count) % ts  = TimeIncr / 60.    !hours
        SW_save(count) % SWV = SWV_ts
        Last_clock_time = TIME                      !hours
      ENDIF

!***********************************************************************
!***********************************************************************
!     SEASEND - Seasonal output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
      IF (.NOT. DOPRINT) RETURN
      CLOSE(LUNW15)    

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE OpSW15min
C=======================================================================
C=====================================================================
!     OpSW15min VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! Values represent soil water content (mm3/mm3) for the time period 
!   since the last print.
! SWV_inst Instantaneous value at time of output
! SWV_max  Maximum SW over interval
! SWV_min  Minimum SW over interval
! SWV_avg  Mean SW over interval
!-----------------------------------------------------------------------
!     END SUBROUTINE OpSWxmin
!=======================================================================
