!=======================================================================
!  RunList, Subroutine
!
!  This routine writes a list of simulations to RUNList.OUT file
!
!-----------------------------------------------------------------------
!  Revision history
!
!  01/30/2006 CHP Written 
!=======================================================================

      SUBROUTINE RunList(CONTROL)

      USE ModuleDefs 
      USE ModuleData
      IMPLICIT NONE
      EXTERNAL GETLUN
      SAVE

      CHARACTER*1  RNMODE
      CHARACTER*12 FILEX, FILEX_LAST

      INTEGER DYNAMIC, EXPNO, LUN, NYRS, RLUN, RUN, TRTNUM

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH	

      LOGICAL FIRST, FOPEN

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      FILEX   = CONTROL % FILEX
      NYRS    = CONTROL % NYRS
      RNMODE  = CONTROL % RNMODE
      RUN     = CONTROL % RUN
      TRTNUM  = CONTROL % TRTNUM

      DATA FIRST /.TRUE./
!-----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL GETLUN('RUNLST',RLUN)
        FIRST = .FALSE.
        OPEN(UNIT=RLUN,FILE='RunList.OUT',STATUS='REPLACE')
        WRITE(RLUN,100)
  100   FORMAT(' NREP  EXP  TRT  NYR FILEX        DESCRIPTION')
        FILEX_LAST = '99999999.999'
        EXPNO = 0
      ENDIF

      IF (DYNAMIC == 1) THEN
        IF (FILEX /= FILEX_LAST) THEN
          EXPNO = EXPNO + 1
          FILEX_LAST = FILEX
        ENDIF

        INQUIRE (FILE='RunList.OUT', OPENED = FOPEN)
        IF (.NOT. FOPEN) THEN
          OPEN(UNIT=RLUN,FILE='RunList.OUT',ACCESS='APPEND')
        ENDIF

        WRITE(RLUN,200) RUN, EXPNO, TRTNUM, NYRS, FILEX 
  200   FORMAT(4I5,1X,A12)

      ELSEIF (DYNAMIC > 5) THEN
        CALL GET(ISWITCH)
        IF (INDEX('0N', ISWITCH%IDETL) < 1) THEN
          CLOSE(RLUN)
        ELSE
          CLOSE(RLUN, STATUS='DELETE')

          !CALL GETLUN('LUN.LST',LUN)
          !CLOSE(LUN, STATUS='DELETE')

          CALL GETLUN('FINPUT', LUN)
          OPEN (UNIT=LUN, FILE='HEADER.OUT')
          CLOSE(LUN,STATUS='DELETE')

          CALL GETLUN('OUTINFO', LUN)
          CLOSE(LUN,STATUS='DELETE')


        ENDIF
      ENDIF

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE RunList

!===========================================================================
