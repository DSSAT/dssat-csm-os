!=====================================================================
!  OpSW15min, Subroutine, Cheryl Porter
!  15-minute output interval for soil water content, all cells
!  Where time steps are greater than 15 minutes, less frequent output is reported.
!  Each output includes instantaneous, average, minimum, and maximum values since last report.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  06/17/2025 CHP written
!-----------------------------------------------------------------------
!  Called by: WATBAL_2D
!=====================================================================
      SUBROUTINE OpSW15min(CONTROL, ISWITCH, 
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
      INTEGER DYNAMIC, LUNW15, I, count, row, col
      INTEGER YRDOY, YEAR, DOY, DAS

      REAL, DIMENSION(MaxRows,MaxCols) :: SWV_inst, SWV_max, SWV_min, 
     &                                    SWV_avg, SWV_ts
      REAL Last_print_time, Target_print_time, Last_clock_time, Sum_time
      REAL ThisTS, SWcell

!     Save up to 1000 values of SWV between reporting intervals
!     900 seconds per 15 minutes, so 1000 should be enough (?)
      TYPE Save_type
        REAL, DIMENSION(MaxRows,MaxCols) :: SWV
        REAL ts
      END TYPE Save_type
      TYPE (Save_type), Dimension(0:1000) :: SW_save

      LOGICAL FEXIST, DOPRINT

!     temp chp - print info for one cell
      integer r1,c1
      r1 = 2
      c1 = 3

!     ------------------------------------------------------------------
      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY
      DAS     = CONTROL % DAS

      SWV_inst = SNGL(SWV_D)

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
          WRITE (LUNW15,'(6(g0,","),g0)') YEAR, DOY+1, DAS+1, 0.0,
     &           SWV_inst(row,col) 
        ENDDO
      ENDDO

!     These times are in hours on a 24 hour clock. 
      Last_print_time = 0.0
      Target_print_time = Last_print_time + 0.25
      count = 0
      Last_clock_time = 0.0
      Sum_time = 0.0  !duration of time since last printout

      SW_save(0) % ts  = 0.0
      SW_save(0) % SWV = SWV_inst

!     temp chp
!     for one cell, print at every time step (unit 6123) and at every print interval (unit 6124)
      write(6123,'(A,/,3(g0,","),g0)') 
     &  "YEAR,DOY,TIME,SWV", 
     &  year, doy+1, 0.0, swv_inst(r1,c1)

      write(6124,'(A,/,3(g0,","),g0)')
     &  "YEAR,DOY,TIME,SWV_inst,SWV_min,SWV_avg,SWV_max",
     &  year, doy+1, 0.0, swv_inst(r1,c1)

!***********************************************************************
!***********************************************************************
!     Time step OUTPUT 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
      IF (.NOT. DOPRINT) RETURN
!     ------------------------------------------------------------------
!     temp chp
      write(6123,'(3(g0,","),g0)') 
     &  year, doy, time, swv_inst(r1,c1)

!     15-minute SWV output for all cells
      IF (TIME - Target_print_time >= -0.01) THEN

!       Handle time steps larger than 15 minutes 
!       Skip some print steps rather than interpolate between values.
        DO WHILE (.TRUE.)
          IF (TIME - Target_print_time > 0.25) THEN   !hours
            Target_print_time = Target_print_time + 0.25
          ELSE
            EXIT
          ENDIF
        ENDDO

!       It's time to print, save last value for aggregation
        count = count + 1
!       Partial time step ending in Target_print_time
        SW_save(count) % ts  = Target_print_time - Last_clock_time
        Sum_time = Sum_time + SW_save(count) % ts   !hours

!       Interpolate last SWV value at the target time
        SW_save(count) % SWV = (SWV_inst - SW_save(count-1) % SWV) 
     &      * SW_save(count) % ts / (TimeIncr / 60.)
     &      + SW_save(count-1) % SWV

        SWV_min = SW_save(0) % SWV
        SWV_max = SW_save(0) % SWV
        SWV_avg = 0.0

        DO i = 1, count
!         To make it easy to read
          ThisTS = SW_save(i) % ts  !current time step in hours

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
                SWV_ts(row,col) = SW_save(count) % SWV(row,col)
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
            WRITE (LUNW15,'(7(g0,","),g0)') YEAR, DOY, DAS, TIME,
     &             SWV_ts(row,col), SWV_min(row,col), 
     &             SWV_avg(row,col), SWV_max(row,col) 
          ENDDO
        ENDDO

!       temp chp
        write(6124,'(3(g0,","),g0)')year, doy, time,  
     &    SWV_ts(r1,c1), SWV_min(r1,c1), 
     &    SWV_avg(r1,c1), SWV_max(r1,c1) 

!       First time increment includes the partial time step which was 
!         beyond the target print time
        count = 0
        SW_save(0) % SWV = SW_save(count) % SWV

!       Initialize arrays for next print interval
        SW_save % ts  = 0.0
        DO row = 1, nRowsTot
          DO col = 1, nColsTot
            SW_save % SWV(row,col) = 0.0 
          ENDDO
        ENDDO

        Last_clock_time = TIME                      !hours
        Last_print_time = Target_print_time
        Target_print_time = Target_print_time + 0.25
        Sum_time = 0.0

      ELSE
!       Save values for later aggregation
        count = count + 1

        SW_save(count) % SWV = SWV_inst
        IF (count == 1) THEN
          SW_save(count) % ts  = TIME - Last_print_time !hours
        ELSE
          SW_save(count) % ts  = TimeIncr / 60.    !hours
        ENDIF

!       Duration since last print
        Sum_time = Sum_time + SW_save(count) % ts  !hours
        Last_clock_time = TIME                      !hours
      ENDIF

!     Is this the last time interval of the day?
      IF (ABS(TIME - 24.) < 0.01) THEN
!       End of day
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
!     SWV_inst
!     SWV_max
!     SWV_min
!     SWV_avg
!-----------------------------------------------------------------------
!     END SUBROUTINE OpSW15min
!=======================================================================
