!=================================================================
!  CO2VAL, Subroutine, CHPorter
!  Determines daily CO2 value
!  Switch ICO2:
!  ICO2 = 'D' use static value (CO2BAS from CO2*.WDA)
!  ICO2 = 'W' use values (daily and/or annual) read from weather file
!  ICO2 = 'M' use time series values read from CO2*.WDA file (default)

!-----------------------------------------------------------------------
!  REVISION HISTORY
!  04/29/2008 CHP Written
!  07/07/2008 CHP Added error handling routine
!  08/31/2009 CHP Stop program if CO2 file not found or errors reading.
!  07/25/2014 CHP Added daily CO2 read from weather file (DCO2)
!-----------------------------------------------------------------------
!  Called by: WEATHR
!=======================================================================

      SUBROUTINE CO2VAL(CONTROL, ISWITCH, CCO2, DCO2, CO2)
!-----------------------------------------------------------------------
      USE ModuleDefs   
      IMPLICIT NONE
      EXTERNAL YR_DOY, INFO, GETLUN, WARNING, IGNORE, PATH, FIND2, 
     &  CO2_ERROR, PARSE_HEADERS, LEAP, CO2HEADER
      SAVE

      TYPE (ControlType), INTENT(IN) :: CONTROL
      TYPE (SwitchType),  INTENT(IN) :: ISWITCH
      REAL,               INTENT(IN) :: CCO2
      REAL,               INTENT(IN) :: DCO2
      REAL,               INTENT(OUT):: CO2

      CHARACTER*1 ICO2
      CHARACTER*10 TEXT
      CHARACTER*12 NAMEF
      CHARACTER*78 MSG(3)
      CHARACTER*80 CHARTEST, PATHSD
      CHARACTER*92 FILECO2

      INTEGER DOY, DYNAMIC, ERR, FOUND, I, INDX
      INTEGER LINE, LNUM, LUNCO2, NVals, PFLAG, YEAR

      LOGICAL FEXIST, LEAP

      CHARACTER*6, PARAMETER :: ERRKEY = 'CO2VAL'
      INTEGER, PARAMETER :: MaxNVals = 5000  !Max number of records
      INTEGER, DIMENSION(MaxNVals) :: FYEAR, FDOY
      REAL,    DIMENSION(MaxNVals) :: FCO2
      REAL CO2BAS

!     For free-format reads
!     Up to MAXCOL headers per line, up to 10 characters long each
      INTEGER, PARAMETER :: MAXCOL = 25
      CHARACTER*15  HEADER(MAXCOL)
!     COL keeps beginning and ending column for each header
      INTEGER COL(MAXCOL,2), COUNT, C1, C2, ISECT

      DYNAMIC = CONTROL % DYNAMIC

!-----------------------------------------------------------------------
      IF (DYNAMIC == SEASINIT) THEN
!-----------------------------------------------------------------------

      ICO2 = ISWITCH % ICO2

!     Use CO2 from weather file if ICO2 = 'W'
      IF (ICO2 == 'W' .AND. CCO2 > 1) THEN
        CO2    = CCO2
        CO2BAS = CCO2
        RETURN
      ELSE
        CO2    = 380.
        CO2BAS = 380.
      ENDIF

!-----------------------------------------------------------------------
!     Read from CO2???.WDA file
      FILECO2 = 'CO2' // ModelVerTxt // '.WDA'
      LNUM = 0
      INQUIRE (FILE = FILECO2, EXIST = FEXIST)

      IF (.NOT. FEXIST) THEN
        CALL PATH('STD',CONTROL%DSSATP,PATHSD,PFLAG,NAMEF)
        FILECO2 = TRIM(PATHSD) // FILECO2
      ENDIF

      INQUIRE (FILE = FILECO2, EXIST = FEXIST)
      IF (.NOT. FEXIST) THEN
        CALL CO2_ERROR(29, CO2, ERRKEY, FILECO2, LNUM, LUNCO2)
        RETURN
      ENDIF

      CALL GETLUN('FILECO2', LUNCO2)
      OPEN (LUNCO2,FILE=FILECO2,STATUS='OLD',IOSTAT=ERR)
      IF (ERR > 0) THEN
        CALL CO2_ERROR(10, CO2, ERRKEY, FILECO2, LNUM, LUNCO2)
        RETURN
      ENDIF

!     Read base CO2 (default static value) from file
      REWIND (LUNCO2)
      CALL FIND2(LUNCO2,'@CO2BAS',LINE,FOUND); LNUM = LNUM + LINE
      IF (FOUND == 1) THEN
        CALL IGNORE(LUNCO2,LNUM,ISECT,CHARTEST)
        IF (ISECT == 1) THEN
          READ(CHARTEST,*) CO2BAS
          IF (CO2BAS > 1) THEN
            CO2 = CO2BAS
          ELSE
            CO2 = 380.
          ENDIF
          IF (ICO2 /= 'M') THEN
            CLOSE (LUNCO2)
            RETURN  !ICO2 = 'D' -- Use default CO2
          ENDIF
        ELSE
          CALL CO2_ERROR(64, CO2, ERRKEY, FILECO2, LNUM, LUNCO2)
          RETURN
       ENDIF
      ELSE
        CALL CO2_ERROR(42, CO2, ERRKEY, FILECO2, LNUM, LUNCO2)
        RETURN
      ENDIF

!     No need to read data again if it's already been read.
      IF (NVals == 0) THEN
!       Read time series CO2 values from file
        CALL FIND2(LUNCO2,'@ YEAR',LINE,FOUND); LNUM = LNUM + LINE
        IF (FOUND == 1) THEN
          BACKSPACE(LUNCO2)
          READ(LUNCO2,'(A)') CHARTEST
          IF (ISECT == 1) THEN
            CALL PARSE_HEADERS(CHARTEST, MAXCOL, HEADER, COUNT, COL)
          ELSE
            CALL CO2_ERROR(50, CO2, ERRKEY, FILECO2, LNUM, LUNCO2)
          ENDIF
        ELSE
          CALL CO2_ERROR(50, CO2, ERRKEY, FILECO2, LNUM, LUNCO2)
          RETURN 
        ENDIF    

        NVals = 1
        ReadLoop: DO WHILE (NVals <= MaxNVals)
!         Read a line of data 
          CALL IGNORE (LUNCO2, LNUM, ISECT, CHARTEST)
          IF (ISECT .NE. 1) EXIT

          HeaderLoop: DO I = 1, COUNT
            C1 = COL(I,1)
            C2 = COL(I,2)
            READ(CHARTEST(C1:C2),'(A)',IOSTAT=ERR) TEXT
            IF (TEXT .NE. '          ') THEN
              SELECT CASE (TRIM(HEADER(I)))
                CASE('YEAR')
                    READ(CHARTEST(C1:C2),*,IOSTAT=ERR)FYEAR(NVals)
                CASE('DOY')
                    READ(CHARTEST(C1:C2),*,IOSTAT=ERR)FDOY(NVals)
                CASE('CO2')
                    READ(CHARTEST(C1:C2),*,IOSTAT=ERR)FCO2(NVals)
              END SELECT
              IF (ERR /= 0) CYCLE ReadLoop
            ENDIF
          ENDDO HeaderLoop
          IF (FCO2(NVals) < 1.) CYCLE ReadLoop
          IF (LEAP(FYEAR(NVals))) THEN
            IF (FDOY(NVals) > 366) CYCLE ReadLoop
          ELSE
            IF (FDOY(NVals) > 365) CYCLE ReadLoop
          ENDIF
          NVals = NVals + 1
        ENDDO ReadLoop
        NVals = NVals - 1
        CLOSE (LUNCO2)

        WRITE(MSG(1),'(I4,A)') 
     &          NVals," CO2 data points read from file."
        MSG(2) = FILECO2(1:78)
        IF (NVals < MaxNVals) THEN
          CALL INFO(2,ERRKEY,MSG)
        ELSE
          MSG(3) = "Remaining data will be ignored."
          CALL WARNING(3,ERRKEY,MSG)
        ENDIF
      ENDIF

!     Get value to use for first day of simulation
      CALL YR_DOY(CONTROL % YRSIM, YEAR, DOY)
      INDX = 1
      CO2 = FCO2(1)
      DO I = 1, NVALS
        INDX = I
        IF (YEAR < FYEAR(I)) THEN
          EXIT
        ELSEIF (YEAR == FYEAR(I)) THEN
          IF (DOY < FDOY(I)) THEN
            EXIT
          ELSEIF (DOY == FDOY(I)) THEN
            CO2 = FCO2(I)
            EXIT
          ELSE
            CO2 = FCO2(I)
            CYCLE
          ENDIF
        ELSE
          CO2 = FCO2(I)
          CYCLE
        ENDIF
      ENDDO
      IF (CO2 < 1.) CO2 = CO2BAS
      CO2BAS = CO2

      CALL CO2Header(CO2)

!-----------------------------------------------------------------------
      ELSEIF (DYNAMIC == RATE) THEN
!-----------------------------------------------------------------------

      CO2 = CO2BAS
      IF (ICO2 == 'M' .AND. NVals > 0) THEN
!       Get daily CO2 from CO2???.WDA file
        CALL YR_DOY(CONTROL % YRDOY, YEAR, DOY)

        DO I = INDX, NVALS
          INDX = I
          IF (YEAR < FYEAR(I)) THEN
            EXIT
          ELSEIF (YEAR == FYEAR(I)) THEN
            IF (DOY < FDOY(I)) THEN
              EXIT
            ELSEIF (DOY == FDOY(I)) THEN
              CO2 = FCO2(I)
              IF (CO2 < 1.) CO2 = CO2BAS
              EXIT
            ENDIF
          ENDIF
        ENDDO

      ELSEIF (ICO2 == 'W') THEN
!       Use daily CO2 read from weather file
        IF (DCO2 > 1.) THEN
          CO2 = DCO2
        ENDIF
      ENDIF
 
      CO2BAS = CO2
!-----------------------------------------------------------------------
      ENDIF   !END OF DYNAMIC BLOCK
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE CO2VAL

!=======================================================================
!     CO2VAL variable definitions
!-----------------------------------------------------------------------

! CCO2     Atmospheric CO2 concentration read from weather file (ppm)
! CO2      Atmospheric carbon dioxide concentration (ppm)
! CO2BAS   Default carbon dioxide base level (ppm)

!=======================================================================

      SUBROUTINE CO2_ERROR(IERROR, CO2, ERRKEY, FILECO2, LNUM, LUNCO2)
!     Error handling routine for CO2 input 

      IMPLICIT NONE
      EXTERNAL WARNING, ERROR

      CHARACTER* 6 ERRKEY
      CHARACTER*78 MSG(4)
      CHARACTER*92 FILECO2
      INTEGER IERROR, LNUM, LUNCO2
      REAL CO2

      CLOSE (LUNCO2)

      IF (CO2 < 1.) THEN
        CO2 = 380.
      ENDIF

      MSG(2) = FILECO2(1:78)
      WRITE(MSG(3),'(A,I4)') "Line number ", LNUM

      SELECT CASE(IERROR)

      CASE (10)
        MSG(1) ='Error opening CO2 input file.'
        WRITE(MSG(4),'(A)') "Program will stop."
        CALL WARNING(4,ERRKEY,MSG)
        CALL ERROR(ERRKEY,IERROR,FILECO2,LNUM)

      CASE (29)
        MSG(1) ='CO2 input file does not exist or has invalid format.'
        WRITE(MSG(4),'(A)') "Program will stop."
        CALL WARNING(4,ERRKEY,MSG)
        CALL ERROR(ERRKEY,IERROR,FILECO2,LNUM)

      CASE (42)
        MSG(1) ='Default CO2 section not found.'
        WRITE(MSG(4),'(A,F7.1,A)') 'CO2 =',CO2,' ppm will be used.'
        CALL WARNING(4,ERRKEY,MSG)

      CASE (50)
        MSG(1) ='CO2 time series section not found.'
        WRITE(MSG(4),'(A,F7.1,A)') 'CO2 =',CO2,' ppm will be used.'
        CALL WARNING(4,ERRKEY,MSG)

      CASE (64)
        MSG(1) ='Invalid format in CO2 file.'
        WRITE(MSG(4),'(A,F7.1,A)') 'CO2 =',CO2,' ppm will be used.'
        CALL WARNING(4,ERRKEY,MSG)

      END SELECT

      RETURN
      END SUBROUTINE CO2_ERROR
!=======================================================================
