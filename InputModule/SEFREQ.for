C=======================================================================
C  SEFREQ, Subroutine
C
C  Determines output file options and control
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1991 GH  Written
C  05/28/1993 PWW Header revision and minor changes
C-----------------------------------------------------------------------
C  INPUT  : IDETO,IDETS,IDETG,IDETC,IDETW,IDETN,IDETP,IDETD,
C           IOX,FROP,NRUN,IDETL
C
C  LOCAL  : LINE,ANS,UPCASE,ERRKEY,MENU,NLOOP,FLAG,FREQ
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SENS
C
C  Calls  : ERROR CLEAR VERIFY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SEFREQ (RNMODE,IDETO,IDETS,IDETG,IDETC,IDETW,IDETN,
     &                   IDETP,IDETD,IOX,FROP,RUN,IDETL)

      IMPLICIT    NONE
      EXTERNAL CLEAR, ERROR, UPCASE, VERIFY

      CHARACTER*1 RNMODE,IDETO,IDETS,IDETG,IDETC,IDETW,IDETN
      CHARACTER*1 IDETP,IDETD,IDETL,IOX,LINE(80),ANS,UPCASE
      CHARACTER*6 ERRKEY

      INTEGER     MENU,FROP,NLOOP,RUN
      REAL        FLAG,FREQ

      PARAMETER (ERRKEY = 'SEFREQ')

      MENU  = 0
      NLOOP = 0

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 50) CALL ERROR (ERRKEY,1,' ',0)

      IF (INDEX('IE',RNMODE) .GT. 0)  THEN
         CALL CLEAR
         WRITE (*,200) RNMODE,IDETO,IDETS,IDETG,IDETC,IDETW,
     &                 IDETN,IDETP,IDETD,FROP,IOX,IDETL
         WRITE (*,275)
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
          IF (INDEX('IE',RNMODE) .GT. 0) THEN
             RNMODE = 'N'
             CALL CLEAR
          ELSE
             RNMODE = 'I'
          ENDIF
      ELSE IF (MENU .EQ. 2) THEN
          IF (IDETO .EQ. 'Y') THEN
             IDETO = 'N'
           ELSE
             IDETO = 'Y'
          ENDIF
      ELSE IF (MENU .EQ. 3) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,1000) IDETS
          READ (5,'(A1)') ANS
          IF (UPCASE (ANS) .EQ. 'A') THEN
             IDETS = 'A'
           ELSE IF (UPCASE (ANS) .EQ. 'N') THEN
             IDETS = 'N'
           ELSE IF (UPCASE (ANS) .EQ. 'Y') THEN
             IDETS = 'Y'
          ENDIF
      ELSE IF (MENU .EQ. 4) THEN
          IF (IDETG .EQ. 'Y') THEN
             IDETG = 'N'
           ELSE
             IDETG = 'Y'
          ENDIF
      ELSE IF (MENU .EQ. 5) THEN
          IF (IDETC .EQ. 'Y') THEN
             IDETC = 'N'
           ELSE
             IDETC = 'Y'
          ENDIF
      ELSE IF (MENU .EQ. 6) THEN
          IF (IDETW .EQ. 'Y') THEN
             IDETW = 'N'
           ELSE
             IDETW = 'Y'
          ENDIF
      ELSE IF (MENU .EQ. 7) THEN
          IF (IDETN .EQ. 'Y') THEN
             IDETN = 'N'
           ELSE
             IDETN = 'Y'
          ENDIF
      ELSE IF (MENU .EQ. 8) THEN
          IF (IDETP .EQ. 'Y') THEN
             IDETP = 'N'
           ELSE
             IDETP = 'Y'
          ENDIF
      ELSE IF (MENU .EQ. 9) THEN
          IF (IDETD .EQ. 'Y') THEN
             IDETD = 'N'
           ELSE
             IDETD = 'Y'
          ENDIF
      ELSE IF (MENU .EQ. 10) THEN
  400     CONTINUE
          LINE(1) = ' '
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,500) FROP
          READ (5,'(80A1)') LINE

          CALL VERIFY (LINE,FREQ,FLAG)

          IF (FREQ .LE. 0) THEN
             GO TO 100
           ELSE IF ((FLAG .GT. 0.0) .OR. (FREQ .GT. 99.0)) THEN
             WRITE (*,700)
             GO TO 400
           ELSE IF (FREQ .NE. NINT(FREQ)) THEN
             WRITE (*,800)
             GO TO 400
           ELSE IF (FREQ .GT. 0.0) THEN
             FROP = NINT(FREQ)
           ELSE
             CALL ERROR (ERRKEY,2,' ',0)
          ENDIF
      ELSE IF (MENU .EQ. 11) THEN
          IF (RUN .EQ. 1) THEN
             IF (IOX   .EQ. 'Y') THEN
                IOX   = 'N'
              ELSE
                IOX   = 'Y'
             ENDIF
          ENDIF
      ELSE IF (MENU .EQ. 12) THEN
          IF (RUN .EQ. 1) THEN
             IF (IDETL .EQ. 'A') THEN
                IDETL = '0'
              ELSE
                IDETL = 'A'
             ENDIF
          ENDIF
      ENDIF

      GO TO 100

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

  200 FORMAT (//,
     &           15X,'OUTPUT FILE OPTIONS AND CONTROLS',/,
     &           15X,'================================',//,
     & 5X,' 0. Return to Main Menu ',//,
     & 5X,' 1. Video Output ..................................',3X,A1,/,
     & 5X,' 2. Overview Output File ..........................',3X,A1,/,
     & 5X,' 3. Summary Output File ...........................',3X,A1,/,
     & 5X,' 4. Growth Output File ............................',3X,A1,/,
     & 5X,' 5. Carbon Balance Output File ....................',3X,A1,/,
     & 5X,' 6. Water Balance Output File .....................',3X,A1,/,
     & 5X,' 7. Nitrogen Balance Output File ..................',3X,A1,/,
     & 5X,' 8. Mineral Nutrients Output File .................',3X,A1,/,
     & 5X,' 9. Pest Output File ..............................',3X,A1,/,
     & 5X,'10. Frequency of Simulation Output (in days).......',1X,I3,/,
     & 5X,'11. Save Output Files with Experiment Code ........',3X,A1,/
     & 5X,'12. Toggle all output on or off ...................',3X,A1)
  275 FORMAT (/, 8X,'SELECTION ? [ Default = 0 ] ===> ',$)
  500 FORMAT (//,8X,'OPTION SELECTED       ===>',1X,I3,/,
     &           8X,'FREQUENCY OF OUTPUT ? --->',2X,' ',$)
  700 FORMAT (8X,'Output frequency must be a number between 1 and 99')
  800 FORMAT (8X,'ERROR-Frequency selection must be an INTEGER value',/)
 1000 FORMAT (//,8X,'Please enter :',/,22X,'Y for YES',/,22X,'N for NO',
     &                               /,22X,'A for APPEND',
     &        //,8X,'OPTION SELECTED ===> ',1X,A1,
     &         /,8X,'OVERVIEW FILE ? ---> ',1X,' ',$)

      END SUBROUTINE SEFREQ
