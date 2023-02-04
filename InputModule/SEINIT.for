C=======================================================================
C  SEINIT, Subroutine
C
C  Determines initilization sensitivity analysis options
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1990 GH  Written
C  05/28/1999 PWW Header revision and minor changes 
C  12/14/2000 GH  Input revised
C  06/09/2002 GH  Modified for Y2K
C-----------------------------------------------------------------------
C  INPUT  : NLAYR,SWINIT,PRCROP,WRESR,WRESND,DLAYR,SLPF,DS,SWSPRF,
C           INO3,INH4
C
C  LOCAL  : LINE,ERRKEY,DESUN,DESMOD,MENU,NLOOP,EFF,FLAG
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SENS SESOIL
C
C  Calls  : ERROR CLEAR SEPLYR VERIFY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SEINIT (RNMODE,NLAYR,SWINIT,PRCROP,WRESR,WRESND,DLAYR,
     &                   SLPF,DS,SWSPRF,INO3,INH4,ICWD,ICRES,ICREN,
     &                   ICREP,ICRIP,ICRID)

      USE ModuleDefs
      IMPLICIT     NONE
      EXTERNAL CLEAR, ERROR, SEPLYR, VERIFY

      CHARACTER*1  RNMODE,LINE(80),SWSPRF
      CHARACTER*2  PRCROP
      CHARACTER*6  ERRKEY
      CHARACTER*10 DESUN
      CHARACTER*30 DESMOD

      INTEGER      MENU,NLOOP,NLAYR

      REAL         DLAYR(NL),SWINIT(NL),DS(NL),INO3(NL),INH4(NL)
      REAL         WRESR,WRESND,SLPF,EFF,FLAG
      REAL         ICWD,ICRES,ICREN,ICREP,ICRIP,ICRID

      PARAMETER (ERRKEY = 'SEINIT')

      NLOOP = 0

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)

      IF (INDEX('IE',RNMODE) .GT. 0)  THEN
         CALL CLEAR
         WRITE (*,200) ICWD,PRCROP,ICRES,ICREN,
     &                 ICREP,ICRIP,ICRID,WRESR,WRESND,SLPF
         IF (SWSPRF .EQ. 'Y') WRITE (*,250)
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
          DESUN  = 'cm3/cm3  '
          DESMOD = 'INITIAL SOIL WATER CONTENT   '
          CALL SEPLYR (DLAYR,NLAYR,RNMODE,DS,SWINIT,DESMOD,DESUN)
      ELSE IF (MENU .EQ. 2) THEN
          DESUN  = 'ug N/g   '
          DESMOD = 'INITIAL SOIL NO3 CONTENT     '
          CALL SEPLYR (DLAYR,NLAYR,RNMODE,DS,INO3,DESMOD,DESUN)
      ELSE IF (MENU .EQ. 3) THEN
          DESUN  = 'ug N/g   '
          DESMOD = 'INITIAL SOIL NH4 CONTENT     '
          CALL SEPLYR (DLAYR,NLAYR,RNMODE,DS,INH4,DESMOD,DESUN)
      ELSE IF (MENU .EQ. 4) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,1100) ICWD
          READ (5,1000) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GT. 0. .AND. EFF .LE. 1000. .AND. FLAG .LE. 0) THEN
             ICWD = EFF
          ENDIF
      ELSE IF (MENU .EQ. 5) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,900) PRCROP
          READ (5,'(A2)') PRCROP
      ELSE IF (MENU .EQ. 6) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,1200) ICRES
          READ (5,1000) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GT. 0. .AND. EFF .LE. 100000. .AND. FLAG .LE. 0) THEN
             ICRES = EFF
          ENDIF
      ELSE IF (MENU .EQ. 7) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,1300) ICREN
          READ (5,1000) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GE. 0. .AND. EFF .LE. 100. .AND. FLAG .LE. 0) THEN
             ICREN = EFF
          ENDIF
      ELSE IF (MENU .EQ. 8) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,1400) ICREP
          READ (5,1000) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GE. 0. .AND. EFF .LE. 100. .AND. FLAG .LE. 0) THEN
             ICREP = EFF
          ENDIF
      ELSE IF (MENU .EQ. 9) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,1500) ICRIP
          READ (5,1000) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GE. 0. .AND. EFF .LE. 100. .AND. FLAG .LE. 0) THEN
             ICRIP = EFF
          ENDIF
      ELSE IF (MENU .EQ. 10) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,1600) ICRID
          READ (5,1000) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GE. 0. .AND. EFF .LE. 1000. .AND. FLAG .LE. 0) THEN
             ICRID = EFF
          ENDIF
      ELSE IF (MENU .EQ. 11) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,700) WRESR
          READ (5,1000) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GE. 0. .AND. EFF .LE. 100000. .AND. FLAG .LE. 0) THEN
C-GH         WRESR = MAX(EFF,1.0)
             WRESR = EFF

          ENDIF
      ELSE IF (MENU .EQ. 12) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,800) WRESND
          READ (5,1000) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GE. 0. .AND. EFF .LE. 100000. .AND. FLAG .LE. 0) THEN
             WRESND = EFF
          ENDIF
      ELSE IF (MENU .EQ. 13) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,500) SLPF
          READ (5,1000) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GT. 0.0 .AND. EFF .LE. 2.0 .AND. FLAG .LE. 0) THEN
             SLPF = EFF
          ENDIF
      ENDIF

      GO TO 100

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

  200 FORMAT(//,15X,'INITIAL CONDITIONS AT START OF SIMULATION',
     &        /,15X,'=========================================',//,
     & 7X,'0. Return to Main Menu ',//,
     & 7X,'1. Initial Soil Water Content ..........................',/,
     & 7X,'2. Initial Soil NO3 ....................................',/,
     & 7X,'3. Initial Soil NH4 ....................................',/,
     & 7X,'4. Initial Water Table Depth ...........................]',
     & 1X,F10.2,/,
     & 7X,'5. Previous Crop .......................................]',
     & 9X,A2,/,
     & 7X,'6. Initial Crop Residue Applied/Carried Over............]',
     & 1X,F10.2,/,
     & 7X,'7. Initial Residue N % .................................]',
     & 1X,F10.2,/,
     & 7X,'8. Initial Residue P % .................................]',
     & 1X,F10.2,/,
     & 7X,'9. Initial Residue Incorporation %......................]',
     & 1X,F10.2,/,
     & 6X,'10. Initial Residue Incorporation depth..................]',
     & 1X,F10.2,/
     & 6X,'11. Root Weight from Previous Crop ......................]',
     & 1X,F10.2,/,
     & 6X,'12. Nodule Weight from Previous Crop ....................]',
     & 1X,F10.2,/,
     & 6X,'13. Growth Reduction Factor Due to Poor Soil Fertility ..]',
     & 6X,F5.3)
  250 FORMAT (9X,
     & 'You chose another soil profile. Redefine initial conditions!',/)
  290 FORMAT (//,9X,'SELECTION ? [ Default = 0 ] ===> ',$)
  500 FORMAT (//,
     &  9X,'GROWTH REDUCTION FACTOR           ===>',1X,F5.3,/,
     &  9X,'[RELATIVE - VALUE BETWEEN 0 and 1 ]',/,
     &  9X,'NEW NUMBER ?                      --->  ',$)
  700 FORMAT (//,
     &  9X,'ROOT WEIGHT FROM PREVIOUS CROP    ===>',1X,F10.2,' kg/ha',/,
     &  9X,'NEW ROOT WEIGHT ?                 --->  ',$)
  800 FORMAT (//,
     &  9X,'NODULE WEIGHT FROM PREVIOUS CROP  ===>',1X,F10.2,' kg/ha',/,
     &  9X,'NEW NODULE WEIGHT ?               --->  ',$)
  900 FORMAT (//,
     &  9X,'PREVIOUS CROP                     ===>',1X,A2,/,
     &  9X,'NEW PREVIOUS CROP ?               --->  ',$)
 1000 FORMAT (80A1)
 1100 FORMAT (//,
     &  9X,'INITIAL WATER TABLE DEPTH         ===>',1X,F10.2,' cm',/,
     &  9X,'NEW WATER TABLE DEPTH ?           --->  ',$)
 1200 FORMAT (//,
     &  9X,'INITIAL CROP RESIDUE APPLIED      ===>',1X,F10.2,' kg/ha',/,
     &  9X,'NEW CROP RESIDUE ?                --->  ',$)
 1300 FORMAT (//,
     &  9X,'INITIAL RESIDUE N CONTENT         ===>',1X,F10.2,' %',/,
     &  9X,'NEW RESIDUE N CONTENT ?           --->  ',$)
 1400 FORMAT (//,
     &  9X,'INITIAL RESIDUE P CONTENT         ===>',1X,F10.2,' %',/,
     &  9X,'NEW RESIDUE P CONTENT ?           --->  ',$)
 1500 FORMAT (//,
     &  9X,'INITIAL RESIDUE INCORPORATION     ===>',1X,F10.2,' %',/,
     &  9X,'NEW INCORPORATION PERCENTAGE ?    --->  ',$)
 1600 FORMAT (//,
     &  9X,'INITIAL RESIDUE INCORPORATION DEPTH ===>',1X,F10.2,' cm',/,
     &  9X,'NEW INCORPORATION DEPTH ?           --->  ',$)

      END SUBROUTINE SEINIT
