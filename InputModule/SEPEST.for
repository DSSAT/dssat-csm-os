C=======================================================================
C  SEPEST, Subroutine
C
C  Pest and disease sensitivity analysis options
C-----------------------------------------------------------------------
C  Revision history
C
C  01/09/1989 GH  Written
C  05/28/1993 PWW Header revision and minor changes
C  02/02/1998 GH  Rename routine to SEPEST
C-----------------------------------------------------------------------
C  INPUT  : IDETD,RNMODE,ISWDIS
C
C  LOCAL  : ERRKEY,MENU,NLOOP
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SENS
C
C  Calls  : CLEAR ERROR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SEPEST (IDETD,RNMODE,ISWDIS)

      IMPLICIT    NONE
      EXTERNAL CLEAR, ERROR

      CHARACTER*1 RNMODE,IDETD,ISWDIS
      CHARACTER*6 ERRKEY

      INTEGER     MENU,NLOOP

      PARAMETER (ERRKEY = 'SEPEST')

      NLOOP = 0
  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)

      IF (INDEX('IE',RNMODE) .GT. 0)  THEN
         CALL CLEAR
         WRITE (*,200) ISWDIS,IDETD
         WRITE (*,290)
      ENDIF
C
C     Get menu choice
C
      READ (5,'(I2)',ERR = 100) MENU
C
C     Branch to menu choice
C
      IF (MENU .EQ. 0) THEN
          RETURN
      ELSE IF (MENU .EQ. 1) THEN
          IF (ISWDIS .EQ. 'Y') THEN
             ISWDIS = 'N'
           ELSE
             ISWDIS = 'Y'
          ENDIF
      ELSE IF (MENU .EQ. 2) THEN
          WRITE (*,400)
          PAUSE
      ELSE IF (MENU .EQ. 3) THEN
          IF (IDETD .EQ. 'Y') THEN
             IDETD = 'N'
           ELSE
             IDETD = 'Y'
          ENDIF
      ENDIF

      GO TO 100
C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

  200 FORMAT (//,15X,'PEST AND DISEASE SELECTION AND MODIFICATION',/,
     &           15X,'===========================================',//,
     &  5X,' 0. Return to Main Menu ',//,
     &  5X,' 1. Pest & Disease Simulation .........]',1X,A1,/,
     &  5X,' 2. Enter Pest Population or Damage ...|',/,
     &  5X,' 3. Pest Output File ..................]',1X,A1,/)
  290 FORMAT (//,9X,'SELECTION ? [ Default = 0 ] ===> ',$)
  400 FORMAT (9X,'Option Currently Not Available')

      END SUBROUTINE SEPEST
