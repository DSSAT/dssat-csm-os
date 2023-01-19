C=======================================================================
C  OPVIEW, Subroutine C.H. Porter
C  Prints overview file based on data provided by various crop models.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/01/2002 CHP Written based on OPHARV subroutine.
C  04/16/2002 GH  Added control block 
C  04/18/2002 GH  Modified DAP variable

! 09/25/2016 - why does this have to be separate from OPVIEW, which is 
!             generic for all other crops? I think this should be 
!             folded into OPVIEW.for.
C=======================================================================

      SUBROUTINE FOR_OPVIEW(CONTROL,  
     &    BIOMAS, ACOUNT, DESCRIP, IDETO, LFNUM, NSTRES, 
     &    OBSERVED, PREDICTED, STGDOY, STNAME, TURFAC, 
     &    WTNCAN, XLAI, YIELD, YRPLT)

C-----------------------------------------------------------------------
C      USE DFLIB
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, ERROR, HEADER, YR_DOY, NAILUJ, GET_CROPD,
     &  TIMDIF, CLEAR
      SAVE

      CHARACTER*1 IDETO, RNMODE, ANS
      CHARACTER*2 CROP
      CHARACTER*3 RMM
      CHARACTER*6 SECTION
      CHARACTER*6, PARAMETER :: ERRKEY = 'OPVIEW'
!      CHARACTER*8 FNAME
      CHARACTER*16 CROPD
      character*10STNAME(20)
      CHARACTER*12 FILEA, OUTO
      CHARACTER*25 TITLET
      CHARACTER*30 FILEIO

      INTEGER DAP, DOY, DSTRES, DYNAMIC, ERRNUM, FOUND
      INTEGER I, IPX, ISENS, LNUM, LUNIO
      INTEGER NOUTDO, NYRS, RUN, TIMDIF, TRTNO
      INTEGER YIELD, YR, YRDOY, YRPLT
      INTEGER STGDOY(20)

      REAL AVDSTR, AVNSTR, BIOMAS, LFNUM, NSTRES
      REAL STRESN, STRESS, TURFAC, WTN, WTNCAN, XLAI

      LOGICAL FEXIST

!     Describe observed and predicted data as character strings.
      INTEGER ACOUNT                           !Number of values
      CHARACTER*8 PREDICTED(40), OBSERVED(40) !Values
      CHARACTER*35 DESCRIP(40)                !Descriptions

      CHARACTER*64 TITLE(3)
      DATA TITLE /
     &'        CROP GROWTH   BIOMASS         LEAF   CROP N     STRESS',
     &'   DATE  AGE STAGE      kg/ha    LAI   NUM  kg/ha  %   H2O    N',
     &' ------  --- ---------- -----  -----  ----  ---  ---  ----  ----'
!               DAP STNAME(I) BIOMAS   XLAI LFNUM WTNCAN WTN AVDSTR AVNSTR
     &/

C     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

!     Don't print for fallow
      CROP    = CONTROL % CROP
      IF (CROP .EQ. 'FA') RETURN

      NYRS    = CONTROL % NYRS
      RNMODE  = CONTROL % RNMODE
      FILEIO  = CONTROL % FILEIO
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY
      DYNAMIC = CONTROL % DYNAMIC

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

C-----------------------------------------------------------------------
C    Read FILE names and paths
C-----------------------------------------------------------------------
      READ (LUNIO,'(55X,I5)') ISENS     !used only in OPHARV
      READ (LUNIO,'(2/,15X,A12,1X,A80)') FILEA     

C-----------------------------------------------------------------------
C       Find and Read TREATMENTS Section
C-----------------------------------------------------------------------
      SECTION = '*TREAT'
      CALL FIND(LUNIO, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILEIO, LNUM)
      ELSE
        READ(LUNIO, '(I3,6X,A25)') TRTNO, TITLET
      ENDIF
      
      CLOSE (LUNIO)

C-----------------------------------------------------------------------
      IF (IDETO .EQ. 'Y') THEN
!       Get unit number for OVERVIEW.OUT
        OUTO  = 'OVERVIEW.OUT'
        CALL GETLUN('OUTO', NOUTDO)
      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      IF (IDETO .EQ. 'Y') THEN
        INQUIRE (FILE = OUTO, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTDO, FILE = OUTO, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, ACCESS = 'APPEND')
        ELSE
          OPEN (UNIT = NOUTDO, FILE = OUTO, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(NOUTDO,'("*SIMULATION OVERVIEW FILE")')
        ENDIF
!        CALL HEADER(RUNINIT, FILEIO, NOUTDO, RUN)
        CALL HEADER(RUNINIT, NOUTDO, RUN)
      ENDIF

C-----------------------------------------------------------------------
C    WRITE HEADER TO SCREEN and overview.out file
C-----------------------------------------------------------------------
      IF (INDEX('IDE',RNMODE) .GT. 0 .AND. NYRS .LE. 1) THEN
        WRITE(*,3050) RUN, TITLET, TITLE
 3050   FORMAT(
     &    '*SIMULATED CROP AND SOIL STATUS AT MAIN DEVELOPMENT STAGES',
     &    //,' RUN NO.',I6,4X,A25,/,3(/,A64))
      ENDIF

      IF (IDETO .EQ. 'Y') THEN
         WRITE (NOUTDO,3050) RUN,TITLET, TITLE
      ENDIF

!     Initialize average stresses
      DSTRES = 0
      STRESS = 0.0
      STRESN = 0.0

      WTN = 0.0
      AVDSTR = 0.0
      AVNSTR = 0.0

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
C-----------------------------------------------------------------------
!     Accumulate water and N stresses since last stage printout
      DSTRES = DSTRES + 1
      STRESS = STRESS + TURFAC
      STRESN = STRESN + NSTRES      

!     At each plant stage, print growth data.
      DO I = 1, 20
        IF (YRDOY .EQ. STGDOY(I)) THEN

          !Compute average stresses
          IF (DSTRES .GE. 1) THEN
            AVDSTR = 1.0 - STRESS / DSTRES
            AVNSTR = 1.0 - STRESN / DSTRES
            DSTRES = 0
            STRESS = 0.0
            STRESN = 0.0
          ELSE
            AVDSTR = 0.0
            AVNSTR = 0.0
          ENDIF

          IF (BIOMAS .GT. 0.0) THEN
            WTN = WTNCAN / BIOMAS * 100.
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

          IF (INDEX('IDE',RNMODE) .GT. 0 .AND. NYRS .LE. 1) THEN
            WRITE(*,4600) IPX, RMM, DAP, STNAME(I), NINT(BIOMAS), 
     &        XLAI, LFNUM, NINT(WTNCAN), WTN, AVDSTR, AVNSTR
 4600       FORMAT (1X,I2,1X,A3,1X,I4,1X,A10,I6,1X,F6.2,1X,F5.1,
     &                1X,I4,1X,F4.1,1X,F5.2,1X,F5.2)
          ENDIF

          IF (IDETO .EQ. 'Y') THEN
            WRITE(NOUTDO,4600) IPX, RMM, DAP, STNAME(I), NINT(BIOMAS), 
     &        XLAI, LFNUM, NINT(WTNCAN), WTN, AVDSTR, AVNSTR
          ENDIF
        ENDIF
      ENDDO

!***********************************************************************
!***********************************************************************
!     Seasonal Output 
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
      CALL GET_CROPD(CROP, CROPD)
      CROPD = ADJUSTR(CROPD)

!     Plant overview data:
        IF (INDEX('IDE',RNMODE) .GT. 0 .AND. NYRS .LE. 1) THEN  
          WRITE(*,
     &          '(/,1X,"Please press < ENTER > key to continue ",2X,$)')
          READ  (*, *)
          CALL CLEAR

          WRITE(*,100)
          DO I = 1, ACOUNT

                !Don't allow blank predicted value,
            IF ((PREDICTED(I) .NE. ' ') .AND.
                !or -99 predicted value,
C-GH &          (INDEX(PREDICTED(I),'-99') .EQ. 0) .AND.
C Need to keep -99 values
     &          (INDEX(PREDICTED(I),'-99') .EQ. 0) .AND.
c-chp If PREDICTED values are -99, then value doesn't exist and
c      we shouldn't print it. Maize has some variables that are
c      included in OLAB array (like pod date, etc.) that
c      really don't make sense for maize.  We should probably
c      modify the OLAB array to only include values that are
c      needed. - chp 4/11/03
c      Values of -99 for OBSERVED data are still printed.

                !or blank description.
     &          (DESCRIP(I)(1:1) .NE. ' ')) THEN
              WRITE(*,200) DESCRIP(I), PREDICTED(I), OBSERVED(I)
            ENDIF
          ENDDO
          WRITE(*,250) RUN,TITLET
          READ  (5,'(A1)') ANS
          CALL CLEAR
          WRITE(*,300) CROPD, YIELD
        ENDIF

  100     FORMAT(
     &    //,"*MAIN GROWTH AND DEVELOPMENT VARIABLES",//,   
     &       "@",5X,"VARIABLE",T41,"PREDICTED     MEASURED",/,  
     &           6X,"--------",T41,"---------     --------")  
  200     FORMAT(6X,A35,A8,5X,A8)
  250     FORMAT ('*RUN ',I6,4X,': ',A22,
     &          '... Press < ENTER > key to continue',$)
  300     FORMAT(/,1X,A10," YIELD : ",I8," kg/ha    [DRY WEIGHT] ",/)

        IF (IDETO .EQ. 'Y') THEN
          WRITE(NOUTDO,100)
          DO I = 1, ACOUNT

                !Don't allow blank predicted value,
            IF ((PREDICTED(I) .NE. ' ') .AND.
                !or -99 predicted value,
     &          (INDEX(PREDICTED(I),'-99') .EQ. 0) .AND.
C-GH &          (INDEX(PREDICTED(I),'-99') .EQ. 0) .AND.
C Need to keep -99 values
                !or blank description.
     &          (DESCRIP(I)(1:1) .NE. ' ')) THEN
              WRITE(NOUTDO,200) DESCRIP(I), PREDICTED(I), OBSERVED(I)
            ENDIF
          ENDDO
          WRITE(NOUTDO,300) CROPD, YIELD
          WRITE(NOUTDO,'(78("*"))')
        ENDIF

        CLOSE (NOUTDO)
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE FOR_OPVIEW
C=======================================================================
