C=======================================================================
C  OpFlood, Subroutine, C.H.Porter
C  Generates daily output on days when flooding occurs.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  11/16/2001 CHP Written
C-----------------------------------------------------------------------
C  Called from:   Paddy_Mgmt
C  Calls:         None
C=======================================================================
      SUBROUTINE OpFlood(CONTROL, ISWITCH, 
     &     ABUND, EF, FLOOD, FRUNOFF, INFILT, IRRAMT, RAIN, RUNOFF)
!-----------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, YR_DOY
      SAVE

      CHARACTER*1 IDETW, ISWWAT, RNMODE
      CHARACTER*10, PARAMETER :: OUTFLD = 'FloodW.OUT'

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, LUN
      INTEGER RUN, YEAR, YRDOY, REPNO

      REAL ABUND, EF, FLOOD, FRUNOFF, IRRAMT, RAIN, RUNOFF, INFILT

      LOGICAL FEXIST, FIRST

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

!     The variable "ISWITCH" is of type "SwitchType".
      TYPE (SwitchType) ISWITCH

!     Transfer values from constructed data types into local variables.
      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FROP    = CONTROL % FROP
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE
      REPNO   = CONTROL % REPNO
      YRDOY   = CONTROL % YRDOY

      IDETW   = ISWITCH % IDETW
      ISWWAT  = ISWITCH % ISWWAT

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      IF (ISWWAT .EQ. 'Y' .AND. IDETW .NE. 'N') THEN
        INQUIRE (FILE = OUTFLD, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = LUN, FILE = OUTFLD, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
          FIRST = .FALSE.  
        ELSE
          CALL GETLUN('OUTFLD', LUN)
          OPEN (UNIT = LUN, FILE = OUTFLD, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(LUN,'("*FLOOD WATER DAILY OUTPUT FILE")')
          FIRST = .TRUE.  
        ENDIF

        !Write headers
        IF (RNMODE .NE. 'Q' .OR. RUN .EQ. 1 .OR. FIRST) THEN

          !For first run of a sequenced run, use replicate
          ! number instead of run number in header.
          IF (RNMODE .EQ. 'Q') THEN
            CALL HEADER(SEASINIT, LUN, REPNO)
          ELSE
            CALL HEADER(SEASINIT, LUN, RUN)
          ENDIF

          WRITE (LUN,120)
  120     FORMAT('@YEAR DOY   DAS ABUND  FLOD',
     &        '  PRED  IRRD  ROFD  EFAD  INFD  FROD')
        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     Daily Output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
      IF (IDETW .NE .'N' .AND. ISWWAT .EQ. 'Y') THEN
        IF ((DYNAMIC .EQ. OUTPUT .AND. MOD(DAS,FROP) .EQ. 0) .OR.
     &      (DYNAMIC .EQ. SEASEND  .AND. MOD(DAS,FROP) .NE. 0)) THEN 
          CALL YR_DOY(YRDOY, YEAR, DOY) 
          !Daily printout
          WRITE (LUN,300) YEAR, DOY, DAS, ABUND, FLOOD, RAIN, IRRAMT, 
     &            RUNOFF, EF, INFILT, FRUNOFF
  300     FORMAT(1X,I4,1X,I3.3,1X,I5,8(1X,F5.1))
        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal Output
!***********************************************************************
        IF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
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
      END SUBROUTINE OpFlood
!***********************************************************************
