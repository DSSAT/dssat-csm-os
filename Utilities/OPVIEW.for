C=======================================================================
C  OPVIEW, Subroutine C.H. Porter
C  Prints overview file based on data provided by various crop models.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/01/2002 CHP Written based on OPHARV subroutine.
C  04/16/2002 GH  Added control block 
C  04/18/2002 GH  Modified DAP variable
C  08/12/2003 CHP Added I/O error checking
C  03/24/2004 CHP Added P stresses
!  10/24/2005 CHP Added environmental & stress factors to Overview.OUT
C=======================================================================

      SUBROUTINE OPVIEW(CONTROL,  
     &    BIOMAS, ICOUNT, DESCRIP, IDETO, LFNUM, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, XLAI, YIELD, YRPLT, RSTAGE)

!-----------------------------------------------------------------------
      USE HeaderMod
!      USE ModuleDefs     !Definitions of constructed variable types, 
!                         ! which contain control information, soil
!                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL CLEAR, ERROR, FIND, GET_CROPD, GETLUN, HEADER, 
     &  LENSTRING, NAILUJ, TIMDIF, YR_DOY
      SAVE

      CHARACTER*1 IDETO, RNMODE, ANS
      CHARACTER*2 CROP
      CHARACTER*3 RMM
      CHARACTER*6 SECTION
      CHARACTER*6, PARAMETER :: ERRKEY = 'OPVIEW'
!      CHARACTER*8 FNAME
      CHARACTER*10 STNAME(20)
      CHARACTER*16 CROPD
      CHARACTER*12 FILEA, OUTO
      CHARACTER*25 TITLET
      CHARACTER*30 FILEIO
      CHARACTER*84 SimText

      INTEGER DAP, DOY, DSTRES, DYNAMIC, ERRNUM, FOUND
      INTEGER I, IPX, ISENS, LINC, LNUM, LUNIO, NameLength
      INTEGER NOUTDO, NYRS, RUN, TIMDIF, TRTNUM
      INTEGER YIELD, YR, YRDOY, YRPLT, RSTAGE
      INTEGER STGDOY(20)
      INTEGER Length, LenString

      REAL AVDSTR, AVNSTR, BIOMAS, LFNUM, N_grow
      REAL STRESN, STRESS, W_grow, WTN, WTNCAN, XLAI

      LOGICAL FEXIST, PRINT_STAGES

!     Describe observed and Simulated data as character strings.
      INTEGER ICOUNT                           !Number of values
      CHARACTER*8 Simulated(40), Measured(40) !Values
      CHARACTER*50 DESCRIP(EvaluateNum)       !Descriptions

!     Added with P module
      REAL AVPSTR1, P_phot, STRESP1
      REAL AVPSTR2, P_grow, STRESP2

      CHARACTER*64 TITLE(3), TITLE2(3)
      DATA TITLE /
     &'        CROP GROWTH     BIOMASS         LEAF   CROP N  ',
     &'   DATE  AGE STAGE        kg/ha    LAI   NUM  kg/ha  % ',
     &' ------  --- ----------   -----  -----  ----  ---  --- '
!               DAP STNAME(I)   BIOMAS   XLAI LFNUM WTNCAN WTN
     &/

      DATA TITLE2 /
     &'   STRESS      STRESS ',
     &'  H2O  Nitr Phos1 Phos2  RSTG',
     &' ----  ----  ----  ----  ----'/
!     AVDSTR AVNSTR AVP1STR AVP2STR RSTAGE

      INTERFACE 
        SUBROUTINE OPSTRESS(CONTROL, IDETO,   
     &    PlantStres, WEATHER, YIELD, BIOMAS)
          USE ModuleDefs 
          TYPE (ControlType), Intent(IN)           :: CONTROL
          CHARACTER*1,        Intent(IN), Optional :: IDETO
          TYPE (PlStresType), Intent(IN), Optional :: PlantStres
          TYPE (WeatherType), Intent(IN), Optional :: WEATHER
          INTEGER,            Intent(IN), Optional :: YIELD
          REAL,               Intent(IN), Optional :: BIOMAS
        END SUBROUTINE OPSTRESS
      END INTERFACE

      TYPE (ControlType) CONTROL
      TYPE (PlStresType) PlantStres

!-----------------------------------------------------------------------
!     Don't print for fallow
      CROP    = CONTROL % CROP
!      IF (CROP .EQ. 'FA') RETURN

      NYRS    = CONTROL % NYRS
      RNMODE  = CONTROL % RNMODE
      FILEIO  = CONTROL % FILEIO
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY
      DYNAMIC = CONTROL % DYNAMIC

      N_grow = PlantStres % N_grow
      W_grow = PlantStres % W_grow
      P_phot = PlantStres % P_phot
      P_grow = PlantStres % P_grow

C***********************************************************************
C***********************************************************************
C     RUN INITIALIZATION
C***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
C-----------------------------------------------------------------------
!     Read FILEIO
      CALL GETLUN('FILEIO', LUNIO)
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)
      LNUM = 0
C-----------------------------------------------------------------------
C    Read FILE names and paths
C-----------------------------------------------------------------------
      READ (LUNIO,'(55X,I5)', IOSTAT=ERRNUM) ISENS ; LNUM = LNUM + 1
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
      READ (LUNIO,'(2/,15X,A12,1X,A80)', IOSTAT=ERRNUM) FILEA
      LNUM = LNUM + 3    
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)

C-----------------------------------------------------------------------
C       Find and Read TREATMENTS Section
C-----------------------------------------------------------------------
      SECTION = '*TREAT'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ(LUNIO, '(I3,6X,A25)', IOSTAT=ERRNUM) TRTNUM, TITLET
        LNUM = LNUM + 1    
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
      ENDIF
      
      CLOSE (LUNIO)

C-----------------------------------------------------------------------
      IF (IDETO .EQ. 'Y') THEN
!       Get unit number for OVERVIEW.OUT
        OUTO  = 'OVERVIEW.OUT'
        CALL GETLUN('OUTO', NOUTDO)
      ENDIF

      SIMULATED = '     -99'
      MEASURED =  '     -99'

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      IF (CROP /= 'FA') THEN
        IF (IDETO .EQ. 'Y') THEN
          INQUIRE (FILE = OUTO, EXIST = FEXIST)
          IF (FEXIST) THEN
            OPEN (UNIT = NOUTDO, FILE = OUTO, STATUS = 'OLD',
     &        IOSTAT = ERRNUM, POSITION = 'APPEND')
          ELSE
            OPEN (UNIT = NOUTDO, FILE = OUTO, STATUS = 'NEW',
     &        IOSTAT = ERRNUM)
            WRITE(NOUTDO,'("*SIMULATION OVERVIEW FILE")')
          ENDIF
        ENDIF

        PRINT_STAGES = .FALSE.
        DO I = 1, 20
          IF (LenString(STNAME(I)) > 0) THEN
            PRINT_STAGES = .TRUE.
            EXIT
          ENDIF
        ENDDO
      ENDIF
      
!     Initialize simulated values
      SIMULATED = '     -99'
      MEASURED =  '     -99'

!     Initialize average stresses
      DSTRES = 0
      STRESS = 0.0
      STRESN = 0.0
      STRESP1 = 0.0
      STRESP2 = 0.0

      WTN = 0.0
      AVDSTR = 0.0
      AVNSTR = 0.0

      SimText = REPEAT(" ",84)

      CALL OPSTRESS(CONTROL, IDETO=IDETO, PlantStres=PlantStres)

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
C-----------------------------------------------------------------------
      IF (CROP == 'FA') RETURN

      IF (YRDOY == YRPLT) THEN
        IF (CONTROL%MULTI > 1 .OR. INDEX('NQFY',CONTROL%RNMODE) > 0)THEN
          CALL MULTIRUN(RUN,YRPLT)
        ENDIF
C-----------------------------------------------------------------------
C    WRITE HEADER TO SCREEN and overview.out file
C-----------------------------------------------------------------------
        IF (INDEX('IE',RNMODE) .GT. 0 .AND. NYRS .LE. 1 
     &       .AND. PRINT_STAGES) THEN
          WRITE(*,3050) RUN, TITLET, TITLE
 3050     FORMAT(//,
     &     '*SIMULATED CROP AND SOIL STATUS AT MAIN DEVELOPMENT STAGES',
     &      //,' RUN NO.',I6,4X,A25,/,3(/,A64))
        ENDIF
      
        IF (IDETO .EQ. 'Y') THEN
          CALL HEADER(RUNINIT, NOUTDO, RUN)
        ENDIF

        IF (IDETO .EQ. 'Y' .AND. PRINT_STAGES) THEN
          WRITE (NOUTDO,3055) RUN,TITLET, (TITLE(I), TITLE2(I), I=1,3)
 3055     FORMAT(//,
     &     '*SIMULATED CROP AND SOIL STATUS AT MAIN DEVELOPMENT STAGES',
     &      //,' RUN NO.',I6,4X,A25,/,3(/,A55,A))
          IF (YRPLT > CONTROL%YRSIM) THEN
            WRITE(NOUTDO,'(A)') SimText
          ENDIF
        ENDIF
      ENDIF

!     Accumulate water and N and P stresses since last stage printout
      DSTRES = DSTRES + 1
      STRESS = STRESS + W_grow
      STRESN = STRESN + N_grow      
      STRESP1 = STRESP1 + P_phot 
      STRESP2 = STRESP2 + P_grow  

!     At each plant stage, print growth data.
      IF (PRINT_STAGES) THEN
        DO I = 1, 20
          NameLength = LenString(STNAME(I))
          IF (YRDOY .EQ. STGDOY(I) .AND. NameLength > 0) THEN

            !Store current Stage, STGDOY and STNAME
            CALL PUT('PLANT','iSTAGE', I)
            CALL PUT('PLANT','iSTGDOY', STGDOY(I))
            CALL PUT('PLANT','iSTNAME', STNAME(I))

            !Compute average stresses
            IF (DSTRES .GE. 1) THEN
              AVDSTR = 1.0 - STRESS / DSTRES
              AVNSTR = 1.0 - STRESN / DSTRES
              AVPSTR1 = 1.0 - STRESP1 / DSTRES
              AVPSTR2 = 1.0 - STRESP2 / DSTRES

              DSTRES = 0
              STRESS = 0.0
              STRESN = 0.0
              STRESP1 = 0.0
              STRESP2 = 0.0
            ELSE
              AVDSTR = 0.0
              AVNSTR = 0.0
              AVPSTR1 = 0.0
              AVPSTR2 = 0.0
            ENDIF

            IF (BIOMAS .GT. 1.E-4 .AND. WTNCAN > 1.E-8) THEN
              WTN = WTNCAN*10. / BIOMAS * 100.
            ELSE
              WTN = 0.0
            ENDIF

            IF (YRPLT .GT. 0) THEN
              DAP = MAX(0,TIMDIF(YRPLT, YRDOY))
            ELSE
              DAP = 0
            ENDIF
            CALL YR_DOY (YRDOY, YR, DOY)
            CALL NAILUJ (DOY, YR, RMM, IPX)
!            IF (YRDOY >= YRPLT) THEN
              IF (INDEX('IE',RNMODE) .GT. 0 .AND. NYRS .LE. 1) THEN
                WRITE(*,4600) IPX, RMM, DAP, STNAME(I), NINT(BIOMAS), 
     &            XLAI, LFNUM, NINT(WTNCAN*10.), WTN, AVDSTR, AVNSTR
 4600           FORMAT (1X,I2,1X,A3,1X,I4,1X,A10,I8,1X,F6.2,1X,F5.1,
     &                  1X,I4,1X,F4.1,1X,F5.2,1X,F5.2)
              ENDIF

              IF (IDETO .EQ. 'Y') THEN
                WRITE(SimText,4605)IPX, RMM, DAP,STNAME(I),NINT(BIOMAS),
     &            XLAI, LFNUM, NINT(WTNCAN*10.), WTN, AVDSTR, AVNSTR,
     &            AVPSTR1, AVPSTR2, RSTAGE
 4605           FORMAT (1X,I2,1X,A3,1X,I4,1X,A10,I8,1X,F6.2,1X,F5.1,
     &                  1X,I4,1X,F4.1,1X,F5.2,1X,F5.2,2(1X,F5.2),I6)
                IF (YRDOY >= YRPLT) WRITE(NOUTDO,'(A)') SimText
              ENDIF
            ENDIF
!          ENDIF
        ENDDO
      ENDIF

      CALL OPSTRESS(CONTROL, PlantStres=PlantStres)
      CALL PUT('PLANT','BIOMAS',BIOMAS)

!***********************************************************************
!***********************************************************************
!     Seasonal Output 
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
      IF (CROP /= 'FA') THEN

        CALL GET_CROPD(CROP, CROPD)
        CROPD = ADJUSTR(CROPD)

!       Check for simulation errors -- all values to '-99'
        IF (CONTROL % ERRCODE > 0) THEN
          SIMULATED = '     -99'
          WRITE(NOUTDO,90) CONTROL % ERRCODE
   90     FORMAT(//,"*** SIMULATION ABORTED, ERROR CODE ",I3," ***",
     &           /,"See Warning.OUT file for additional information.",/)
        ENDIF
      
!       Plant overview data:
          IF (INDEX('IE',RNMODE) .GT. 0 .AND. NYRS .LE. 1) THEN  
            WRITE(*,
     &          '(/,1X,"Please press < ENTER > key to continue ",2X,$)')
            READ  (*, *)
            CALL CLEAR
      
            WRITE(*,100)
            DO I = 1, ICOUNT
      
                  !Don't allow blank Simulated value,
              IF (((Simulated(I) .NE. ' ') .AND.          !<----------!
                                                                    ! !
                !or -99 Simulated value,                            !A!
C-GH &          (INDEX(Simulated(I),'-99') .EQ. 0) .AND.            !L!
C Need to keep -99 values                                           !L!
     &          (INDEX(Simulated(I),'-99') .EQ. 0) .AND.            ! !
c-chp If Simulated values are -99, then value doesnt exist and      !O!
c      we shouldnt print it. Maize has some variables that are      !F!
c      included in OLAB array (like pod date, etc.) that            ! !
c      really dont make sense for maize.  We should probably        !T!
c      modify the OLAB array to only include values that are        !H!
c      needed. - chp 4/11/03                                        !E!
c      Values of -99 for Measured data are still printed.           !S!
                                                                    !E!
                !or blank description.                              ! !
     &          (DESCRIP(I)(1:1) .NE. ' '))    !<---------------------!

     &        .OR.                             !<---------------------!
C-CHP  OK - compromise - keep Simulated=-99 for physiological maturity!
!                                               and anthesis date     !
     &          ((INDEX(Simulated(I),'-99') .EQ. 0) .AND.    ! OR THIS!
     &           ((INDEX(DESCRIP(I),'Phys') .NE. 0) .OR.   !AND EITHER!
     &            (INDEX(DESCRIP(I),'Anthesis') .NE. 0))))   !OF THESE!
     &          THEN                           !<---------------------!
                                               
                WRITE(*,200) DESCRIP(I), Simulated(I), Measured(I)
              ENDIF
            ENDDO
            WRITE(*,250) RUN,TITLET
            READ  (5,'(A1)') ANS
            CALL CLEAR
            WRITE(*,300) CROPD, YIELD
          ENDIF

  100     FORMAT(
     &    //,"*MAIN GROWTH AND DEVELOPMENT VARIABLES",//,   
     &       "@",5X,"VARIABLE",T56,"SIMULATED     MEASURED",/,  
     &           6X,"--------",T56,"---------     --------")  
  200     FORMAT(6X,A50,A8,5X,A8)
  250     FORMAT ('*RUN ',I6,4X,': ',A22,
     &          '... Press < ENTER > key to continue',$)

          IF (IDETO .EQ. 'Y' .AND. CONTROL % ErrCode == 0) THEN
            WRITE(NOUTDO,100)
            DO I = 1, ICOUNT

                !Don't allow blank Simulated value,
              IF (((Simulated(I) .NE. ' ') .AND.          !<----------!
                !or -99 predicted value,                            !T!
C-GH &          (INDEX(Simulated(I),'-99') .EQ. 0) .AND.            !H!
C Need to keep -99 values                                           !E!
                                                                    !S!
     &          (INDEX(Simulated(I),'-99') .EQ. 0) .AND.            !E!
                                                                    ! !
                !or blank description.                              !3!
     &          (DESCRIP(I)(1:1) .NE. ' '))    !<---------------------!
                  
     &          .OR.                                               !OR!
C-CHP                                          !<---------------------!
c OK - compromise - keep Simulated=-99 for physiological maturity     !
!                                               and anthesis date     !
     &          ((INDEX(Simulated(I),'-99') .NE. 0) .AND.     !OR THIS!
     &          ((INDEX(DESCRIP(I),'Phys')  .NE. 0) .OR.   !AND EITHER!
     &           (INDEX(DESCRIP(I),'Anthesis').NE. 0))))   !  OF THESE!
     &        THEN                             !<---------------------!
                                               
                WRITE(NOUTDO,200) DESCRIP(I), Simulated(I), Measured(I)
              ENDIF
            ENDDO
          ENDIF
        ENDIF

        CALL OPSTRESS(CONTROL, PlantStres=PlantStres, YIELD=YIELD, 
     &                Biomas=Biomas)

        IF (CROP /= 'FA') THEN
          IF (IDETO .EQ. 'Y') THEN
            IF (CONTROL % ErrCode == 0) THEN
              Length = LenString(CROPD)
              WRITE(NOUTDO,270) 
              WRITE(NOUTDO,300) CROPD(1:Length), YIELD
            ENDIF
            WRITE(NOUTDO,'(110("*"))')
          ENDIF
        ENDIF
        CLOSE (NOUTDO)
  270 FORMAT(/,'------------------------------------------------------',
     &'--------------------------------------------------------')
  300 FORMAT(/,10X,A," YIELD : ",I8," kg/ha    [Dry weight] ",/)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OPVIEW
C=======================================================================
