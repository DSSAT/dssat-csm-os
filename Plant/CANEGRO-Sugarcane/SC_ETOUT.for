c     Climate change study summary output
c     ----------------------------------------
c     MJ & SK, Jun 2015.  Work done for SASRI project 08RE14.
c     This module generates a single line of output per
c     season, relevant to climate change studies,
c     specifically the requirements of 08RE14.
c     ---------------------------------------
c     Variables to output:
c      - Aerial dry mass (t/ha)
c      - Stalk dry mass (t/ha)
c      - Sucrose mass (t/ha)
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SC_ETOUT(CONTROL, Fi, EP, ES, EO, YRPLT, XHLAI, 
     & EORATIO, YREND)

!     2023-01-26 chp removed unused variables from argument list: PAR, MDATE, 

c     ***************************************************************      
c     Instruct compiler to use module definitions:
c     DSSAT
      USE ModuleDefs
      USE ModuleData
c     Canegro
      USE CNG_ModuleDefs      

      IMPLICIT NONE
      EXTERNAL GETLUN, YR_DOY, NAILUJ, ETAD_NAILUJ, FIND_HDATE
      EXTERNAL TIMDIF, MTHEND
      SAVE

c     Control variable:
      TYPE (ControlType), INTENT(IN) :: CONTROL        

c     Bio-physical variables to output in summary file
c     CMDMD is cellulose DM.
c     LGDMD, LDDMD are green tops DM and trash DM
c     RDMD is root dry mass
      REAL, INTENT(IN) :: Fi, EP, ES, EO, XHLAI, EORATIO !PAR, 
     
      INTEGER, INTENT(IN) :: YRPLT, YREND !, MDATE

c     Local state variables
c     :::::::::::::::::::::
c     Cumulative transpiration + evaporation
      REAL EPT, ETV, EPV, CumFi, AvgFi, KC, ET0

      INTEGER COUNTER

c     The file unit number for the summary output file.
!      INTEGER ETOUT
c     Filename
!      CHARACTER*20 OFILE
c     Does the file exist, is this the first run?:
!      LOGICAL FILE_EXISTS !, FIRST
c     Error status:
!      INTEGER ERRNUM
c     Days after planting
      INTEGER DAP
      INTEGER DOY, YEAR, YRDOY, NDAY, iMON, DAYMON !, YR, YEARPLT
      CHARACTER*3  RMON
      !For the why
      INTEGER TIMDIF
      INTEGER MTHEND
      
      INTEGER PLNTYR
      INTEGER GRYRNO
      INTEGER GRMONNO
      
      INTEGER YEARHV(3)
      CHARACTER*3 MONHV
      INTEGER DOYHV, NDAYHV
      
      INTEGER YRHV
      
      REAL EToc, ETo, ETc_ETo, ETc_EToc, ETc_ETo14 
      ! Runoff and drainage
      REAL cRUNOFF, cDRAIN, dRUNOFF, dDRAIN
      
      ! MJ, Jan 2018: should output be produced?
      LOGICAL GENOUTP

c     ===============================================================
c     RUN INITIALISATION
c     ===============================================================
c     Code to be called at the beginning of the simulation run
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF (Control%DYNAMIC .EQ. RUNINIT) THEN
c     Nothing for now...

c     ===============================================================
c     RUN INITIALISATION
c     ===============================================================
c     Code to be called at the beginning of each simulated crop
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      ELSEIF (Control%DYNAMIC .EQ. SEASINIT) THEN
c     Open the output file
c     Write a file header

c     Open growth aspects output file:
c     ::::::::::::::::::::::::::::::::
c       Set file name:
!        OFILE = 'ETSUMMRY.OUT'
!
!c       Get file unit number:
!        CALL GETLUN('ETSUMMRY', ETOUT)
!
!c       Check that the file exists:
!        FILE_EXISTS = .FALSE.
!        INQUIRE(FILE=OFILE, EXIST=FILE_EXISTS)
!
!c       Open the file
!        IF (FILE_EXISTS) THEN
!c         In append mode if the file already exists
!          OPEN (UNIT=ETOUT, FILE=OFILE, STATUS='OLD',
!     &      IOSTAT=ERRNUM, POSITION='APPEND')
!!         WRITE(*,*) 'Appending to existing file.'
!        ELSE
!c         A new file if not existing
!          OPEN (UNIT=ETOUT, FILE=OFILE, STATUS='NEW',
!     &      IOSTAT = ERRNUM)
!     
!!         WRITE(*,*) 'Writing to new file.' 
!          
!          WRITE(ETOUT,'("*MONTHLY EVAPORATION SUMMARY OUTPUT FILE")')
!          WRITE(ETOUT,'("! RUNNO - Cumulative irrigation (mm)")')
!          WRITE(ETOUT,'("! TRNO-the DSSAT CSM internal TRNO variable")')
!          WRITE(ETOUT,'("! HV_YEAR � harvest year")')
!          WRITE(ETOUT,'("! HV_MON � harvest month")')
!          WRITE(ETOUT,'(A)') "! GRYR_No � the year of growth for this " 
!     &                  //   "month's output "
!          WRITE(ETOUT,'("! GRMON_No � the growth month number, from 1 
!     & to n where n is the number of months at harvest.
!     &    ")')
!          WRITE(ETOUT,'("! MONTH � month number, 1-12 for Jan- Dec")')
!          WRITE(ETOUT,'("! FI � average canopy cover for that month.")')
!          WRITE(ETOUT,'("! ETc � sum of plant (EP) and soil (ET) 
!     & evaporation over the course of the month")')
!          WRITE(ETOUT,'("! EToc � sum of potential evaporation, i.e. EO,
!     &     over the course of the month")')
!          WRITE(ETOUT,'("! ETo � sum of FAO-56 shortgrass potential
!     & evaporation, over the course of the month")')
!          WRITE(ETOUT,'("! ETc_ETo � monthly average ETc / ETo")')
!          WRITE(ETOUT,'("! ETc_EToc � monthly average ETc / EToc")')
!          WRITE(ETOUT,'("! RNOFF - Monthly cumulative runoff (mm)")')
!          WRITE(ETOUT,'("! DRAIN - Monthly cumulative drainage (mm)")')
!          WRITE(ETOUT,'("")')
!c         Write column headings
!          WRITE(ETOUT, '(A8, 1H , A5, 1H , A4, 1H , 13(A8, 1H ))') 
!     &      '@EXPCODE', 'RUNNO', 'TRNO', 'HV_YEAR', 'HV_MON','GRYR_NO',
!     &      'GRMON_NO','MONTH','FI','ETc', 'EToc', 'ETc_ETo', 'ETo', 
!     &      'ETc_EToc', 'RNOFF', 'DRAIN'
!        ENDIF

c       Initialised total irrigation to 0.0
        COUNTER = 0
        EPT = 0.0
        ETV = 0.0
        YRHV = 0
        
        EToc = 0.0
        ETo = 0.0
        ETc_ETo = 0.0
        ETc_EToc = 0.0
        AvgFi = 0
        CumFi = 0
        GRMONNO = 0
        
        ! Runoff and drainage
        cRUNOFF = 0.0
        cDRAIN = 0.0


c     ===============================================================
c     RATE
c     ===============================================================
      ELSEIF (Control%DYNAMIC .EQ. RATE) THEN
        
             

c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c
c     -----------------------------------------------------
c
c              DYNAMIC = INTEGRATE
c
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
      ELSEIF(Control%DYNAMIC .EQ. INTEGR) THEN
        
        DAP = MAX(0, TIMDIF(YRPLT, Control%YRDOY))
        ! MJ, Jan 2018: Only accumulate values after crop has started
        ! (simulation can start earlier)
        IF (DAP .GT. 0) THEN
        
        COUNTER = COUNTER + 1
      
c       DAP
        YRDOY   = CONTROL % YRDOY

c       YEAR        
        CALL YR_DOY(YRDOY, YEAR, DOY)

c       RMON
        CALL NAILUJ(DOY,YEAR,RMON,NDAY)
        
c       iMON        
        CALL ETAD_NAILUJ (DOY, YEAR, iMON, NDAY)
        
        EPV = EPV + EP
        
        ETV = ETV + EP + ES
        
        EToc = EToc + EO
        
        CumFi = CumFi + Fi
        
        KC=1.0+(EORATIO-1.0)*XHLAI/6.0
              
        ET0 = EO/KC
        
        ETo = ETo + ET0
        
        ! MJ 30/12/2017 access runoff
        CALL GET('WATER', 'RUNOFF', dRUNOFF)
        ! and drainage
        CALL GET('WATER', 'DRAIN', dDRAIN)
        ! Accumulate values:
        ! (reset after each monthly output in OUTPUT section)
        cRUNOFF = cRUNOFF + dRUNOFF
        cDRAIN = cDRAIN + dDRAIN
        
        END IF
      

c     END of INTEGRATE
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c
c     -----------------------------------------------------
c
c              DYNAMIC = OUTPUT
c
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
      ELSEIF(Control%DYNAMIC .EQ. OUTPUT) THEN
      
      ! MJ, Jan 2018: Problem:
      ! Monthly output is only provided for whole months
      ! This is because output is only produced when DOY == DAYMON
      ! If the crop starts mid-month, then the first month's data
      ! values will not reflect a full month, so the end month 
      ! should also be shown even if it is partial.
      ! However, care must be exercised when using the values.
      ! Possibility: output daily averages calculated over the month?
      !   Still dicey I think.
      
      
      ! Logic is as follows:
      ! Produce output if
      ! DAYMON == DOY (SK's standard logic)
      ! DOY == Harvest DOY and year = harvest year
      ! if GENOUTP == TRUE, generate output
      GENOUTP = .FALSE.
      
      IF (DAP .GT. 0) THEN
      
        ! Test for standard output condition
        ! (day of year == end of current month)
          ! Get day of year of the end of the current month 
        DAYMON = MTHEND(YEAR,iMON)
        IF(DOY .EQ. DAYMON) THEN
          GENOUTP = .TRUE.
        ENDIF
        
        ! Test for end of season condition
        ! year and doy == harvest year and doy:
        ! YRDOY is the current date of the simulation
        ! YREND appears to be the date of harvest / sim end
        IF (YRDOY .EQ. YREND) THEN
          GENOUTP = .TRUE.
        ENDIF
        
        ! Produce output if either condition has 
        ! been met
        IF (GENOUTP) THEN
c         Growth month number
          GRMONNO = GRMONNO + 1
          
c         YEAR        
          CALL YR_DOY(YRPLT, PLNTYR, DOY)
          
c         YEAR        
          CALL YR_DOY(YRDOY, YEAR, DOY)

c         Harvest year
c          CALL YR_DOY(YREND, YEARHV, DOYHV)

c         RMON
          CALL NAILUJ(DOY,YEAR,RMON,NDAY)
        
c         iMON        
          CALL ETAD_NAILUJ (DOY, YEAR, iMON, NDAY)
          
c         Growth year number
          GRYRNO = YEAR - PLNTYR + 1
          
c         Average monthly fractional interception
          if (COUNTER > 0) then
            AvgFi = CumFi/COUNTER
          else
            AvgFi = 0.0
          endif
          
          if (ETo > 1.E-6) then
            ETc_ETo = ETV/ETo
          else
            ETc_ETo = 0.0
          endif
    
c         MONTH END
c         CALL MTHEND(YR,MTH)

          CALL FIND_HDATE(CONTROL, YEARHV)
          
          YRHV = YEARHV(1)/1000
          
          DOYHV = YEARHV(1) - (YRHV*1000)
          
          CALL NAILUJ(DOYHV,YRHV,MONHV,NDAYHV)
          
          YRHV = YEARHV(1)/1000

          if (EToc > 1.E-6) then
            ETc_ETo14 = ETV/EToc
          else
            ETc_ETo14 = 0.0
          endif
      
c         Write summary data to file.
!		  WRITE(ETOUT, '(A8, 1H , ' // !File name1
!     &                 'I5, 1X , '//  !Run number2
!     &                 'I4, 1H , ' // !Treatment number3
!     &      'I8, 1H '//      !HV_YEAR4
!     &      'A8, 1H '//      !HV_MON5
!     &      'I8, 1H '//      !Growth year num6
!     &      'I8, 1H '//      !Growth month num7
!     &      'I8, 1H '//      !Current month num8
!     &      'F8.3, 1H '//    !FI9
!     &      'F8.3, 1H '//    !ETc10
!     &      'F8.3, 1H '//    !EToc11
!     &      'F8.3, 1H '//    !ETo12
!     &      'F8.3, 1H '//    !ETc_ETo13
!     &      'F8.3, 1H '//    !ETc_EToc14
!     &      'F8.3, 1H '//    !Runoff
!     &      'F8.3, 1H )')    !Drainage
!     &      Control%FILEX,   !File1
!     &      Control%RUN,     !Run number2
!     &      Control%TRTNUM,  !Treatment number3
!     &      YRHV,            !Harvest year4
!     &      MONHV,           !Harvest month5
!     &      GRYRNO,          !Growth year number6
!     &      GRMONNO,         !Growth month number7
!     &      iMON,            !Current month number8
!     &      AvgFi,           !average canopy cover for the month9
!     &      ETV,             !Sum of EP and ET evaporation for the mon10
!     &      EToc,            !Sum of EO for the month11
!     &      ETc_ETo,         !Sum of EO for the month12
!     &      ETo,             !Monthly average ETc_ETo13
!!    &      (ETV/EToc),      !Monthly average ETc_ETo14
!     &      ETc_ETo14,       !Monthly average ETc_ETo14
!     &      cRUNOFF,         ! Monthly cumulative runoff
!     &      cDRAIN           ! Monthly cumulative drainage
     
            COUNTER = 0
            ETV = 0
            EPV = 0
            AvgFi = 0
            CumFi = 0
            EToc = 0
            ETo = 0
            
            ! Runoff and drainage
            cRUNOFF = 0.0
            cDRAIN = 0.0
     
          END IF
      END IF

c     End of DYNAMIC conditional statement


c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

c     ===============================================================
c     END OF SEASON
c     ===============================================================
      ELSEIF (Control%DYNAMIC .EQ. SEASEND) THEN
      
c       Close the output file
!        CLOSE(UNIT=ETOUT)

c     End of DYNAMIC conditional statement
      ENDIF
      
      END  
c     =============================================================== 

C=======================================================================
C  IPAHAR, Subroutine, C. Porter
C-----------------------------------------------------------------------
C  Reads input variables from temporary data file for use by automatic
!      harvest routine (AUTHAR)
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  11/23/1999 CHP Written for modular version of AUTHAR.
C  08/20/2002 GH  Modified for Y2K
C  08/12/2003 CHP Added error checking
C========================================================================

      SUBROUTINE FIND_HDATE(CONTROL,
     &    HDATE)                  !Output

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL FIND, ERROR

      CHARACTER*6 SECTION, ERRKEY 
      PARAMETER (ERRKEY = 'FIND_HDATE')
      CHARACTER*30 FILEIO 
      CHARACTER*90 CHAR

      INTEGER ERRNUM
      INTEGER I, HDATE(3)
      INTEGER LINC, LNUM, FOUND
      INTEGER LUNIO

!     The variable "CONTROL" is of constructed type "ControlType" as 
!     defined in ModuleDefs.for, and contains the following variables.
!     The components are copied into local variables for use here.
      TYPE (ControlType) CONTROL
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO

C-----------------------------------------------------------------------
C    Open Temporary File
C-----------------------------------------------------------------------
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)
      LNUM = 0

C-----------------------------------------------------------------------
C    Read Harvest Section
C-----------------------------------------------------------------------
      SECTION = '*HARVE'
      CALL FIND(LUNIO, SECTION, LINC, FOUND); LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
      
        DO I = 1,3
          READ(LUNIO,'(3X,I7,4X,A90)',ERR=4102,END=4102) HDATE(I), CHAR 
          LNUM = LNUM + 1

          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)

        ENDDO
 4102   CONTINUE

      ENDIF

      CLOSE (LUNIO)

      RETURN
      END !SUBROUTINE FIND_HDATE 