!=====================================================================
!  OpSWxmin, Subroutine, Cheryl Porter
!  x-minute output interval for soil water content, all cells.
!
!  - File is created only for the first run of a batch.
!  - This file can be modified to change the value of "x", the print 
!    interval, in minutes. To change the print interval, change the value
!    of parameter INTERVAL.
!  - For time steps greater than x minutes, less frequent output is reported.
!  - Each output includes instantaneous, average, minimum, and maximum values 
!    since the last report.
!    * The instantaneous value is at the reported time.
!    * Max, min, and mean values are computed over the previous x minutes.
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

!     ****************************************************************
!     Change this value to get printout in different time intervals
!     Use 60 minutes or smaller
!     INTEGER, PARAMETER :: INTERVAL = 15  !minutes
!     INTEGER, PARAMETER :: INTERVAL = 30  !minutes
      INTEGER, PARAMETER :: INTERVAL = 60  !minutes
!     ****************************************************************

      CHARACTER*13 SWVXFile

      INTEGER DYNAMIC, LUNWX, I, count, row, col
      INTEGER YRDOY, YEAR, DOY, DAS

      REAL, DIMENSION(MaxRows,MaxCols) :: SWV_inst, SWV_max, SWV_min, 
     &                                    SWV_avg, SWV_ts
      REAL Last_print_time, Target_print_time, Last_clock_time, Sum_time
      REAL ThisTS, SWcell, Clock_ratio, INT_hour

!     Assume 1 second as minimum average time step for storing intermediate values
      INTEGER, PARAMETER :: MaxCount = INTERVAL * 60  
!     Save up to MaxCount values of SWV between reporting intervals
      TYPE Save_type
        REAL, DIMENSION(MaxRows,MaxCols) :: SWV
        REAL ts
      END TYPE Save_type
      TYPE (Save_type), Dimension(0:MaxCount) :: SW_save

      LOGICAL DOPRINT, FEXIST

!!     ------------------------------------------------------------------
!!     Detailed printout for one cell - currently disabled
!!     ------------------------------------------------------------------
!      integer lunts,lunpi        !unit numbers
!      character*14 CellTS, CellPI  !file names
!      integer r1,c1  !row and column for detailed printout
!      r1 = 2  !cell row for detailed printout
!      c1 = 3  !cell column for detailed printout
!!     ------------------------------------------------------------------

!     ------------------------------------------------------------------
      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY
      DAS     = CONTROL % DAS
      CALL YR_DOY(YRDOY, YEAR, DOY)

      SWV_ts = SNGL(SWV_D)

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      DOPRINT=.TRUE.
!     Print only for first run of a batch
      IF (CONTROL%RUN /= 1) THEN
        DOPRINT=.FALSE.
      ENDIF
!     Print only when water output is requested
      IF (ISWITCH % IDETW .EQ. 'N') THEN
        DOPRINT=.FALSE.
      ENDIF
!     Print only when detailed output is requested
      IF (ISWITCH % IDETL /= 'D') THEN
        DOPRINT=.FALSE.
      ENDIF
      IF (.NOT. DOPRINT) RETURN
!     ------------------------------------------------------------------

!     ------------------------------------------------------------------
!     Open output file 
      WRITE(SWVXFile,'(A,I2.2,A)') "SWcell_", INTERVAL, ".csv"
      
      CALL GETLUN(SWVXFile, LUNWX)
      INQUIRE (FILE = SWVXFile, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = LUNWX, FILE = SWVXFile, STATUS = "REPLACE")
      ELSE
        OPEN (UNIT = LUNWX, FILE = SWVXFile, STATUS = 'NEW')
      ENDIF

!     Write header for daily output
      WRITE (LUNWX,'(A)')
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
          
          WRITE (LUNWX,'(9(g0,","),g0)') 
     &      YEAR, DOY, DAS+1, 0.0, row, col, 
     &      SWV_ts(row,col), SWV_ts(row,col),
     &      SWV_ts(row,col),SWV_ts(row,col)
        ENDDO
      ENDDO

!     Convert print interval time to hours
      INT_hour = INTERVAL / 60.

!     These times are in hours on a 24 hour clock. 
      Last_print_time = 0.0
      Target_print_time = INT_hour
      count = 0
      Last_clock_time = 0.0
      Sum_time = 0.0  !duration of time since last printout

      SW_save(0) % ts  = 0.0
      SW_save(0) % SWV = SWV_ts

!!     ------------------------------------------------------------------
!!     Detailed printout for one cell - currently disabled
!!     ------------------------------------------------------------------
!!     For one cell, print at every time step (unit lunts)
!      WRITE(CellTS,'(A,I2.2,A,I2.2,A)') "SWts-",r1,"-",c1,".csv"
!      
!      CALL GETLUN(CellTS,lunts)
!      INQUIRE (FILE = CellTS, EXIST = FEXIST)
!      IF (FEXIST) THEN
!        OPEN (UNIT = lunts, FILE = CellTS, STATUS = "REPLACE")
!      ELSE
!        OPEN (UNIT = lunts, FILE = CellTS, STATUS = 'NEW')
!      ENDIF
!
!      write(lunts,'(A,/,7(g0,","),g0)') 
!     &  "YEAR,DOY,DAS,TIME,DeltaT,ROW,COL,SWV_ts", 
!     &  year, doy, das+1, 0.0, 0.0, r1, c1, swv_ts(r1,c1)
!
!!     ------------------------------------------------------------------
!!     For one cell, print at every print interval (unit lunpi)
!      WRITE(CellPI,'(A,I2.2,A,I2.2,A,I2.2,A)') 
!     &  "SW",INTERVAL,"-",r1,"-",c1,".csv"
!      
!      CALL GETLUN(CellPI,lunpi)
!      INQUIRE (FILE = CellPI, EXIST = FEXIST)
!      IF (FEXIST) THEN
!        OPEN (UNIT = lunpi, FILE = CellPI, STATUS = "REPLACE")
!      ELSE
!        OPEN (UNIT = lunpi, FILE = CellPI, STATUS = 'NEW')
!      ENDIF
!
!      write(lunpi,'(A,A,/,8(g0,","),g0)') "YEAR,DOY,DAS,TIME,DeltaT,",
!     &  "count,ROW,COL,SWV_inst,SWV_min,SWV_avg,SWV_max",
!     &  year, doy, das+1, 0.0, 0.0, 0, r1, c1, swv_ts(r1,c1)
!!     ------------------------------------------------------------------

!***********************************************************************
!***********************************************************************
!     Time step OUTPUT 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
      IF (.NOT. DOPRINT) RETURN
!     ------------------------------------------------------------------
!!     ------------------------------------------------------------------
!!     Detailed printout for one cell - currently disabled
!!     ------------------------------------------------------------------
!!     For one cell, print at every time step (unit lunts)
!      write(lunts,'(7(g0,","),g0)') 
!     &  year, doy, das, time, TimeIncr, r1, c1, swv_ts(r1,c1)
!!     ------------------------------------------------------------------

!     If the array size is at maximum, print now.
      IF (count == MaxCount - 1) THEN
        Target_print_time = TIME
      ENDIF

!     X-minute SWV output for all cells
      IF (TIME - Target_print_time >= -1E-5) THEN

!       Handle time steps larger than X minutes 
!       Skip some print steps rather than interpolate between values.
        DO WHILE (TIME - Target_print_time >= INT_hour)
          Target_print_time = Target_print_time + INT_hour
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

!             Average is weighted average over the X-minute print interval
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

            WRITE (LUNWX,'(9(g0,","),g0)') 
     &        YEAR, DOY, DAS, Target_print_time, row, col, 
     &        SWV_inst(row,col), SWV_min(row,col), 
     &        SWV_avg(row,col), SWV_max(row,col) 
          ENDDO
        ENDDO

!!     ------------------------------------------------------------------
!!     Detailed printout for one cell - currently disabled
!!     ------------------------------------------------------------------
!!     For one cell, print at every print interval (unit lunpi)
!        write(lunpi,'(11(g0,","),g0)')year, doy, das, Target_print_time,
!     &    Sum_time, count, r1, c1, 
!     &    SWV_inst(r1,c1), SWV_min(r1,c1), 
!     &    SWV_avg(r1,c1), SWV_max(r1,c1) 
!!     ------------------------------------------------------------------

!       Initialize arrays for next print interval
        count = 0  
        Sum_time = 0.0
        SW_save % ts  = 0.0
        DO row = 1, nRowsTot
          DO col = 1, nColsTot
            SW_save % SWV(row,col) = 0.0 
          ENDDO
        ENDDO

        Last_clock_time = TIME                      !hours
        Last_print_time = Target_print_time
        Target_print_time = Target_print_time + INT_hour

        SW_save(0) % SWV = SWV_inst   !SWV at last print time
        IF (ABS(Last_print_time - Last_clock_time) > 0.001) THEN
!         Print time and clock time are not in synch. Need to save the extra
!           bit of time beyond the last print time as the first element of the array.
          count = 1
          SW_save(1) % SWV = SWV_ts     !SWV at current clock time
          SW_save(1) % ts  = TIME - Last_print_time 
        ENDIF


!       Is this the last time interval of the day?
        IF (ABS(TIME - 24.) < 0.01) THEN
!         End of day
          count = 0
          Last_clock_time = 24. - Last_clock_time
          Last_print_time = 0.0
          Target_print_time = INT_hour
        ENDIF

      ELSE
!       Save values for later aggregation
        count = count + 1
        SW_save(count) % ts  = TimeIncr / 60.    !hours
        SW_save(count) % SWV = SWV_ts
        Last_clock_time = TIME                   !hours
      ENDIF

!***********************************************************************
!***********************************************************************
!     SEASEND - Seasonal output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
      IF (.NOT. DOPRINT) RETURN
      CLOSE(LUNWX)    

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE OpSWxmin
C=======================================================================
C=====================================================================
!     OpSWxmin VARIABLE DEFINITIONS:
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
