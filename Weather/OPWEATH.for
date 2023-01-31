C=======================================================================
C  OpWeath, Subroutine, C.H.Porter from weather portions of OPDAY
C  Generates output for daily weather data.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  02/08/2002 CHP Written
C  06/06/2002 GH  Modified for crop rotations
C  08/20/2002 GH  Modified for Y2K
C-----------------------------------------------------------------------
C  Called from:   WEATHR
C  Calls:         None
C=======================================================================
      SUBROUTINE OpWeath(CONTROL, ISWITCH, 
     &    CLOUDS, CO2, DAYL, FYRDOY, OZON7, PAR, RAIN,    !Daily values
     &    SRAD, TAVG, TDAY, TDEW, TGROAV, TGRODY,         !Daily values
     &    TMAX, TMIN, TWILEN, WINDSP, WEATHER)            !Daily values

!     Daily values:
!     SRAD,TMAX,TMIN,RAIN,TDEW,WINDSP,PAR,RHUM
!-----------------------------------------------------------------------
      USE ModuleDefs
      USE CsvOutput 
      USE Linklist
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, YR_DOY
      SAVE

      CHARACTER*1  IDETG, IDETL, RNMODE
      CHARACTER*6,  PARAMETER :: ERRKEY = 'OPWTHR'
      CHARACTER*11, PARAMETER :: OUTWTH = 'Weather.OUT'

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, LUN
      INTEGER RUN, YEAR, YRDOY, REPNO, FYRDOY, WDATE

      REAL
     &  CLOUDS, CO2, DAYL, OZON7, PAR, RAIN, SRAD, 
     &  TAVG, TDAY, TDEW, TGROAV, TGRODY,
     &  TMAX, TMIN, TWILEN, WINDSP, VPDF, vpd_transp

      LOGICAL FEXIST
      TYPE (WeatherType) WEATHER

C-----------------------------------------------------------------------
C     Define constructed variable types based on definitions in
C     ModuleDefs.for.
C-----------------------------------------------------------------------
      TYPE (ControlType) CONTROL
C-----------------------------------------------------------------------
C     The variable "ISWITCH" is of type "SwitchType".
C-----------------------------------------------------------------------
      TYPE (SwitchType) ISWITCH

      IDETG   = ISWITCH % IDETG
      IDETL   = ISWITCH % IDETL
      IF (IDETG .NE. 'Y' .OR. IDETL == '0') RETURN

      DAS     = CONTROL % DAS 
      DYNAMIC = CONTROL % DYNAMIC 
      FROP    = CONTROL % FROP  
      RNMODE  = CONTROL % RNMODE
      REPNO   = CONTROL % REPNO
      RUN     = CONTROL % RUN    
      YRDOY   = CONTROL % YRDOY   

      FMOPT   = ISWITCH % FMOPT   ! VSH
!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
          CALL GETLUN('OUTWTH', LUN)
          INQUIRE (FILE = OUTWTH, EXIST = FEXIST)
          IF (FEXIST) THEN
            OPEN (UNIT = LUN, FILE = OUTWTH, STATUS = 'OLD',
     &        IOSTAT = ERRNUM, POSITION = 'APPEND')
          ELSE
            OPEN (UNIT = LUN, FILE = OUTWTH, STATUS = 'NEW',
     &        IOSTAT = ERRNUM)
            WRITE(LUN,'("*WEATHER MODULE DAILY OUTPUT FILE")')
          ENDIF
        END IF   ! VSH
        
        IF (RNMODE .NE. 'Q' .OR. RUN .EQ. 1) THEN
          IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
            !For first run of a sequenced run, use replicate
            ! number instead of run number in header.
            IF (RNMODE .EQ. 'Q') THEN
              CALL HEADER(SEASINIT, LUN, REPNO)
            ELSE
              CALL HEADER(SEASINIT, LUN, RUN)
            ENDIF

C-----------------------------------------------------------------------
C           Variable heading for Weather.OUT
C-----------------------------------------------------------------------
            WRITE (LUN,120)
  120       FORMAT('@YEAR DOY   DAS',
     &'   PRED  DAYLD   TWLD   SRAD   PARD   CLDD   TMXD   TMND   TAVD',
     &'   TDYD   TDWD   TGAD   TGRD   WDSD   CO2D   VPDF    VPD  OZON7',
     &'   WDATE')
          END IF   ! VSH
        ENDIF

!***********************************************************************
!***********************************************************************
!     Daily Output - OUTPUT and SEASEND calls
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
C       Generate output for file Weather.OUT
        IF ((DYNAMIC .EQ. OUTPUT .AND. MOD(DAS,FROP) .EQ. 0) .OR.
     &      (DYNAMIC .EQ. SEASEND  .AND. MOD(DAS,FROP) .NE. 0) .OR. 
     &       DAS == 1) THEN 
          VPDF = WEATHER % VPDF
          vpd_transp = Weather % VPD_TRANSP
!         These can be modified by ETPHOT during hourly energy balance
          TGROAV = WEATHER % TGROAV
          TGRODY = WEATHER % TGRODY

      
          CALL YR_DOY(YRDOY, YEAR, DOY)
          IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
            !Daily printout

            IF (FYRDOY > 0) THEN
              WDATE = FYRDOY  !Date for weather forecasting ensembles
            ELSE
              WDATE = YRDOY
            ENDIF

            WRITE (LUN,300) YEAR, DOY, DAS, 
     &        RAIN, DAYL, TWILEN, SRAD, PAR, CLOUDS, 
             !TMXD  TMND  TAVD  TDYD   TDWD   TGAD   TGRD   
     &        TMAX, TMIN, TAVG, TDAY, TDEW, TGROAV, TGRODY,
           !  WDSD   CO2D  VPDF  VPD
     &        WINDSP, CO2, VPDF, vpd_transp, OZON7, WDATE
  300       FORMAT(1X,I4,1X,I3.3,1X,I5,
     &        5(1X,F6.1),1X,F6.2,
     &        8(1X,F6.1),F7.1, 1x, F6.2, 1X, F6.2, F7.2, I8)
          END IF   ! VSH
          
          IF (FMOPT == 'C') THEN 
            CALL CsvOutWth(EXPNAME, RUN, CONTROL%TRTNUM, 
     &        CONTROL%ROTNUM, CONTROL%REPNO, YEAR, DOY, DAS,  
     &        RAIN, DAYL, TWILEN, SRAD, PAR, CLOUDS, TMAX,  
     &        TMIN, TAVG, TDAY, TDEW, TGROAV, TGRODY, WINDSP, CO2,
     &        VPDF, vpd_transp,   
     &        vCsvlineWth, vpCsvlineWth, vlngthWth)
     
             CALL LinklstWth(vCsvlineWth)
          END IF
        ENDIF

        IF ((DYNAMIC .EQ. SEASEND)
     &    .AND. (FMOPT == 'A'.OR. FMOPT == ' '))THEN
          !Close daily output files.
          CLOSE (LUN)
        ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OpWeath
!***********************************************************************
