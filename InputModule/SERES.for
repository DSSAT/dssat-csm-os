C=======================================================================
C  SERES, Subroutine
C
C  Determines residue management sensitivity analysis options
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1991 GH  Written
C  05/28/1993 PWW Header revision and minor changes 
C  01/18/1996 GH  Multiple residue applications
C  08/19/2002 GH  Modified for Y2K
C  08/23/2002 GH  Expanded array for organic material applications to NAPPL
C-----------------------------------------------------------------------
C  INPUT  : PRCROP,WRESR,WRESND,RESIDUE,DEPRES,RESN,RESP,
C           RESCOD,RINP,RESDAY,NARES
C
C  LOCAL  : ERRKEY,MENU,NLOOP
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SENS
C
C  Calls  : CLEAR VERIFY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SERES (RNMODE,CROP,RESIDUE,DEPRES,ISWWAT,ISWNIT,
     &                  RESN,RESP,RESDAY,NARES,RESAMT,RESCOD,RINP,RESK,
     &                  IRESI,NSWITCH,IDETN,REMANT)

      USE ModuleDefs
      IMPLICIT    NONE
      EXTERNAL CLEAR, ERROR, ENTRES, SENIT, SWRES

      CHARACTER*1  RNMODE,IRESI,IDETN,ISWWAT,ISWNIT
      CHARACTER*2  CROP
      CHARACTER*5  RESCOD(NAPPL)
      CHARACTER*6  ERRKEY
      CHARACTER*25 RTYPE(NAPPL),REMANT

      INTEGER     MENU,NLOOP,NARES,RESDAY(NAPPL),I
      INTEGER     RESTYPE(NAPPL),NSWITCH

      REAL        RESAMT
      REAL        RESN(NAPPL),RESIDUE(NAPPL),RINP(NAPPL),DEPRES(NAPPL)
      REAL        RESP(NAPPL), RESK(NAPPL)

      PARAMETER (ERRKEY = 'SERES ')

      RTYPE (1) = 'Crop residue            '
      RTYPE (2) = 'Green manure            '
      RTYPE (3) = 'Barnyard manure         '
      RTYPE (4) = 'Liquid manure           '

      NLOOP = 0

  100 CONTINUE
      DO I = 1, NAPPL
	 READ (RESCOD(I)(4:5),'(I2)') RESTYPE(I)
      END DO
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)

      IF (INDEX('IE',RNMODE) .GT. 0)  THEN
	 CALL CLEAR
	 IF (ISWNIT .NE. 'Y') THEN
	    REMANT = 'NO N-BALANCE SIMULATION  '
	  ELSE IF (IRESI .EQ. 'A') THEN
	    REMANT = 'AUTOMATIC RESIDUE APPLIC.'
	  ELSE IF (IRESI .EQ. 'F') THEN
	    REMANT = 'AUTOMATIC RESIDUE APPLIC.'
	  ELSE IF (IRESI .EQ. 'N') THEN
	    REMANT = 'NO RESIDUE APPLICATION   '
	  ELSE IF (IRESI .EQ. 'R') THEN
	    REMANT = 'FIELD SCHEDULE-YRDOY     '
	  ELSEIF (IRESI .EQ. 'D') THEN
	    REMANT = 'FIELD SCHEDULE-DAP       '
	  ELSE
	    REMANT = '                         '
	 ENDIF

	 WRITE (*,200) ISWNIT,REMANT,IDETN
	 IF (ISWWAT .EQ. 'N') THEN
	    IF (ISWNIT .EQ. 'Y') WRITE (*,280)
	 ENDIF
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
	  IF (ISWNIT .EQ. 'Y') THEN
	     ISWNIT  = 'N'
	     NSWITCH = 0
	     IF (CROP .EQ. 'RI') THEN
		CALL SENIT (RNMODE,ISWNIT,NSWITCH)
	     ENDIF
	   ELSE
	     ISWNIT  = 'Y'
	     NSWITCH = 1
	  ENDIF
      ELSE IF (MENU .EQ. 2) THEN
	  CALL SWRES (RNMODE,IRESI,ISWNIT,ISWWAT,REMANT)
      ELSE IF (MENU .EQ. 3) THEN
	  WRITE(*,650)
	  PAUSE
      ELSE IF (MENU .EQ. 4) THEN
	  CALL ENTRES (NARES,RESDAY,RESCOD,RESIDUE,RESN,RESP,RESK,
     &                 RINP,DEPRES,RNMODE,IRESI,RESTYPE,RTYPE,RESAMT)

      ELSE IF (MENU .EQ. 5) THEN
	  IF (IDETN .EQ. 'Y') THEN
	     IDETN = 'N'
	   ELSE
	     IDETN = 'Y'
	  ENDIF
      END IF

      GO TO 100

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

  200 FORMAT (//,
     &     9X,'RESIDUE MANAGEMENT AND MODIFICATION',/,
     &     9X,'===================================',//,
     & 5X,' 0. Return to Main Menu ',//,
     & 5X,' 1. Nitrogen Balance Simulation .......] ',A1,/,
     & 5X,' 2. Residue Management ................] ',A25,/,
     & 5X,' 3. Automatic Residue Management ......] ',/,
     & 5X,' 4. Enter Residue Interactive .........| ',/,
     & 5X,' 5. Nitrogen Output File ..............] ',A1,/)
  280 FORMAT (/,9X,'To be able to simulate the nitrogen balance,',
     &        /,9X,'Please turn on the water balance simulation!',/)
  290 FORMAT (//,9X,'SELECTION ? [ Default = 0 ] ===> ',$)

  650 FORMAT (5X,'Option Currently Not Available')

      END SUBROUTINE SERES

C=======================================================================
C  SWRES, Subroutine
C
C  Determines residue management sensitivity analysis options
C-----------------------------------------------------------------------
C  Revision history
C
C  01/18/1996 GH Written
C  
C-----------------------------------------------------------------------
C  INPUT  : RNMODE,IRESI,ISWNIT,ISWWAT,REMANT
C
C  LOCAL  : ERRKEY
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SERES
C
C  Calls  : CLEAR ERROR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SWRES (RNMODE,IRESI,ISWNIT,ISWWAT,REMANT)

      IMPLICIT     NONE
      EXTERNAL CLEAR, ERROR

      CHARACTER*1  RNMODE,IRESI,ISWNIT,ISWWAT
      CHARACTER*6  ERRKEY
      CHARACTER*25 REMANT

      INTEGER      NLOOP,MENU

      PARAMETER (ERRKEY = 'SWRES ')

      NLOOP = 0

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)
      IF (INDEX('IE',RNMODE) .GT. 0) THEN
	 CALL CLEAR
	 WRITE (*,3400)
	 WRITE (*, 290) REMANT
      ENDIF
C
C     Get menu choice
C
      READ (5,'(I2)',ERR = 100) MENU
C
C     Branch to menu choice
C
      IF (MENU .EQ. 1) THEN
	  IRESI  = 'N'
	  ISWWAT = 'Y'
	  ISWNIT = 'Y'
      ELSE IF (MENU .EQ. 2) THEN
	  IRESI  = 'R'
	  ISWWAT = 'Y'
	  ISWNIT = 'Y'
      ELSE IF (MENU .EQ. 3) THEN
	  IRESI  = 'D'
	  ISWWAT = 'Y'
	  ISWNIT = 'Y'
      ELSE IF (MENU .EQ. 4) THEN
	  IRESI  = 'A'
	  ISWWAT = 'Y'
	  ISWNIT = 'Y'
      ELSE IF (MENU .EQ. 5) THEN
	  ISWNIT = 'N'
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

  290 FORMAT (//,9X,'CURRENT RESIDUE MANAGEMENT      ===> ',A25,/
     &           9X,'SELECTION (#) ? [ Default = 0 ] ---> ',$)
 3400 FORMAT (
     &     9X,'ORGANIC FERTILIZER MANAGEMENT STRATEGY',/,
     &     9X,'======================================',//,
     & 5X,' 0. Return to Previous Menu ',//
     & 5X,' 1. No Residue Application .............................|',/,
     & 5X,' 2. Apply Residue According Field Schedule.(YRDOY)......|',/,
     & 5X,' 3. Apply Residue According To Field Schedule (DAP).....|',/,
     & 5X,' 4. Automatic Residue Application ......................|',/,
     & 5X,' 5. No Nitrogen Stress / No N-Balance Simulation .......|')

      END SUBROUTINE SWRES


C=======================================================================
C  ENTRES, Subroutine
C
C  Determines residue data entry
C-----------------------------------------------------------------------
C  Revision history
C
C  11/20/1995 PWW Written
C  07/16/2002 CHP Increased number of applications to 200 (NAPPL)
C  05/07/2020 FO  Added new Y4K subroutine call to convert YRDOY
C-----------------------------------------------------------------------
C  INPUT  : NARES,RESDAY,RESCOD,RESIDUE,RESN,RESP,RESK,RINP,DEPRES,
C           RNMODE,IRESI
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SERES
C
C  Calls  : CLEAR VERIFY YR_DOY SORT
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE ENTRES (NARES,RESDAY,RESCOD,RESIDUE,RESN,RESP,RESK,
     &                 RINP,DEPRES,RNMODE,IRESI,RESTYPE,RTYPE,RESAMT)

      USE ModuleDefs
      IMPLICIT     NONE
      EXTERNAL CLEAR, ERROR, UPCASE, VERIFY, Y4K_DOY, YR_DOY

      CHARACTER*1  RNMODE,LINE(80),IMENU,UPCASE,IRESI
      CHARACTER*6  ERRKEY
      CHARACTER*5  RESCOD(*)
      CHARACTER*25 RTYPE(NAPPL)

      INTEGER      NARES,I,MENU,YRTEMP,YRT,DOYT,NLOOP
      INTEGER      RESDAY(*),RESTYPE(NAPPL)
      REAL         RINP(*),RESP(*),RESK(*),RESN(*),DEPRES(*)
      REAL         RESIDUE(*),EFF,FLAG,RESAMT
      
      PARAMETER (ERRKEY = 'ENTRES')

      NLOOP = 0
  100 CONTINUE
      NLOOP = NLOOP + 1

      IF (INDEX('IE',RNMODE).GT. 0)  THEN
C	 CALL HOME
	 IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)
	 WRITE (*,200)
	 WRITE (*,250)
	 IF (NARES .GT. 0) THEN
	    DO I = 1, NARES
		CALL YR_DOY (RESDAY(I),YRT,DOYT)
	      WRITE(*,285) I,YRT,DOYT,RESIDUE(I),DEPRES(I),
     &                     RTYPE(RESTYPE(I)),RINP(I),RESN(I),RESP(I),
     &                     RESK(I)
	    END DO
	 ENDIF
	 WRITE (*,275)
	 WRITE (*,295)
      ENDIF
      IF (IRESI .NE. 'D' .AND. IRESI .NE. 'R') WRITE (*,296)
C
C     Get menu choice
C
      READ (5,'(A1)',ERR = 100) IMENU
      IF (IRESI .NE. 'D' .AND. IRESI .NE. 'R') RETURN
C
C     Branch to menu choice
C
      IMENU = UPCASE(IMENU)
      IF (IMENU .EQ. 'E') THEN
	  IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,850)
	  READ (5,'(I5)',ERR = 100) MENU
	  IF (MENU .GT. NARES .OR. MENU .LE. 0) GOTO 100
      ELSE IF (IMENU .EQ. 'D') THEN
	  IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,800)
	  READ (5,450) LINE
	  CALL VERIFY (LINE,EFF,FLAG)
	  IF (EFF .GT. 0. .AND. EFF .LE. NARES .AND. FLAG .LE. 0) THEN
	     MENU          = NINT(EFF)
	     RESDAY(MENU)  = 9999999
	     RESIDUE(MENU) = 0.0
	     DEPRES(MENU)  = 0.0
	     RESCOD(MENU)  = 'RE001'
	     RINP(MENU)    = 0.0
	     RESN(MENU)    = 0.0
	     RESP(MENU)    = 0.0
	     RESK(MENU)    = 0.0
	     GO TO 700
	  ENDIF
      ELSE IF (IMENU .EQ. 'A') THEN
	  NARES = NARES + 1
	  MENU   = NARES
  380     IF (INDEX('IE',RNMODE) .GT. 0) THEN
	    IF (IRESI .EQ. 'R') THEN
	      WRITE (*,400)
	    ELSE IF (IRESI .EQ. 'D') THEN
	      WRITE (*,401)
	    ENDIF
	  ENDIF
	  READ (5,450) LINE
	  CALL VERIFY (LINE,EFF,FLAG)
	  IF (EFF .GT. -0.0001 .AND. EFF .LE. 9999999. .AND.
     &    FLAG .LE. 0) THEN
	    YRTEMP = NINT(EFF)
C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY
	    !CALL Y2K_DOY(YRTEMP)
      CALL Y4K_DOY(YRTEMP,'ENTRES',0,ERRKEY,1)
	    IF (IRESI .EQ. 'R') THEN
	      CALL YR_DOY (YRTEMP,YRT,DOYT)
	      IF (DOYT .GT. 0 .AND. DOYT .LE. 366 .AND. 
     &        YRT .LT. 3000) THEN
	        RESDAY(NARES) = YRTEMP
	      ELSE
	        WRITE (*,600)
              GO TO 380
	      ENDIF
	    ELSE IF (IRESI .EQ. 'D') THEN
		  RESDAY(NARES) = YRTEMP
	    ENDIF
	  ELSE
	    WRITE (*,600)
	    GO TO 380
	  ENDIF
      ELSE IF (IMENU .EQ. 'Q') THEN
	  DO I = 1, NAPPL
	     WRITE (RESCOD(I),'(A2,I3.3)') 'RE',RESTYPE(I)
	  END DO
	  RETURN
      ELSE
	  DO I = 1, NAPPL
	     WRITE (RESCOD(I),'(A2,I3.3)') 'RE',RESTYPE(I)
	  END DO
	  RETURN
      ENDIF
      !
      ! Allow user to change the date of Residue application, if editing
      !
      IF (IMENU .EQ. 'E') THEN
 390     IF (INDEX('IE',RNMODE) .GT. 0) THEN
	    IF (IRESI .EQ. 'R') THEN
	       WRITE (*,410) RESDAY(MENU)
	    ELSE IF (IRESI .EQ. 'D') THEN
	       WRITE (*,411) RESDAY(MENU)
	    ENDIF
	 ENDIF
	 READ (5,450) LINE
	 CALL VERIFY (LINE,EFF,FLAG)
	 IF (EFF .GT. -0.0001 .AND. EFF .LE. 9999999. .AND. FLAG .LE. 0)
     &   THEN
      YRTEMP = NINT(EFF)
C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY
      !CALL Y2K_DOY(YRTEMP)
      CALL Y4K_DOY(YRTEMP,'ENTRES',0,ERRKEY,1)
	   IF (IRESI .EQ. 'R') THEN
	     CALL YR_DOY (YRTEMP,YRT,DOYT)
	     IF (DOYT .GT. 0 .AND. DOYT .LE. 366 .AND.YRT .LT. 3000) THEN
	       RESDAY(MENU) = YRTEMP
	     ELSE
	       WRITE (*,600)
	       GO TO 390
	     ENDIF
	   ELSE IF (IRESI .EQ. 'D') THEN
	     RESDAY(MENU) = YRTEMP
	   ENDIF
        ENDIF
      ENDIF
      !
      ! How much Residue?
      !
  550 IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,500) RESIDUE(MENU)
      READ (5,450) LINE
      CALL VERIFY (LINE,EFF,FLAG)
      IF (EFF .GE. 0.0 .AND. EFF .LE. 10000.0 .AND. FLAG .LE. 0) THEN
	 RESIDUE(MENU) = EFF
       ELSE IF (FLAG .EQ. 1.0) THEN
	 RESIDUE(MENU) = RESIDUE(MENU)
       ELSE
	 WRITE (*,600)
	 GO TO 550
      ENDIF
      !
      ! Depth of application?
      !
  950 IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,900) DEPRES(MENU)
      READ (5,450) LINE
      CALL VERIFY (LINE,EFF,FLAG)
      IF (EFF .GE. 0.0 .AND. EFF .LE. 200.0 .AND. FLAG .LE. 0) THEN
	 DEPRES(MENU) = EFF
       ELSE IF (FLAG .EQ. 1.0) THEN
	 DEPRES(MENU) = DEPRES(MENU)
       ELSE
	 WRITE (*,600)
	 GO TO 950
      ENDIF
      !
      ! What type of Residue?
      !
      IF (INDEX('IE',RNMODE) .GT. 0) CALL CLEAR
 5000 IF (INDEX('IE',RNMODE) .GT. 0) THEN
	 WRITE (*,4900) (I,RTYPE(I),I=1,4),RESTYPE(MENU)
      ENDIF

      READ (5,450) LINE
      CALL VERIFY (LINE,EFF,FLAG)

      IF (EFF .GT. 0.0 .AND. EFF .LE. 4.0 .AND. FLAG .LE. 0) THEN
	 RESTYPE(MENU) = NINT(EFF)
       ELSE IF (FLAG .EQ. 1.0 .AND. RESTYPE(MENU) .GT. 0) THEN
	 RESTYPE(MENU) = RESTYPE(MENU)
       ELSE
	 WRITE (*,600)
	 GO TO 5000
      ENDIF
      !
      ! % Incorporation of residue application?
      !
  960 IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,910) RINP(MENU)
      READ (5,450) LINE
      CALL VERIFY (LINE,EFF,FLAG)
      IF (EFF .GE. 0.0 .AND. EFF .LE. 100.0 .AND. FLAG .LE. 0) THEN
	 RINP(MENU) = EFF
       ELSE IF (FLAG .EQ. 1.0) THEN
	 RINP(MENU) = RINP(MENU)
       ELSE
	 WRITE (*,600)
	 GO TO 960
      ENDIF
      !
      ! % N of residue application?
      !
  970 IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,920) RESN(MENU)
      READ (5,450) LINE
      CALL VERIFY (LINE,EFF,FLAG)
      IF (EFF .GE. 0.0 .AND. EFF .LE. 100.0 .AND. FLAG .LE. 0) THEN
	 RESN(MENU) = EFF
       ELSE IF (FLAG .EQ. 1.0) THEN
	 RESN(MENU) = RESN(MENU)
       ELSE
	 WRITE (*,600)
	 GO TO 970
      ENDIF
      !
      ! % P of residue application?
      !
  980 IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,930) RESP(MENU)
      READ (5,450) LINE
      CALL VERIFY (LINE,EFF,FLAG)
      IF (EFF .GE. 0.0 .AND. EFF .LE. 100.0 .AND. FLAG .LE. 0) THEN
	 RESP(MENU) = EFF
       ELSE IF (FLAG .EQ. 1.0) THEN
	 RESP(MENU) = RESP(MENU)
       ELSE
	 WRITE (*,600)
	 GO TO 980
      ENDIF
      !
      ! % K of residue application?
      !
  990 IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,940) RESK(MENU)
      READ (5,450) LINE
      CALL VERIFY (LINE,EFF,FLAG)
      IF (EFF .GE. 0.0 .AND. EFF .LE. 100.0 .AND. FLAG .LE. 0) THEN
	 RESK(MENU) = EFF
       ELSE IF (FLAG .EQ. 1.0) THEN
	 RESK(MENU) = RESK(MENU)
       ELSE
	 WRITE (*,600)
	 GO TO 990
      ENDIF
      !
      ! Sort entries by date of application
      !
 700  CONTINUE
CCC   CALL SORT (NARES,RESDAY,RESIDUE,DEPRES,ETYPE,MTYPE)

      IF (RESDAY(NARES) .EQ. 9999999) THEN
	 RESDAY(MENU)  = 0
	 RESIDUE(MENU) = 0.0
	 DEPRES(MENU)  = 0.0
	 RESCOD(MENU)  = 'RE001'
	 RINP(MENU)    = 0.0
	 RESN(MENU)    = 0.0
	 RESP(MENU)    = 0.0
	 RESK(MENU)    = 0.0
	 NARES         = NARES - 1
      ENDIF
      RESAMT = 0.0
      DO I = 1, NARES
	 RESAMT = RESAMT + RESIDUE(I)
      END DO

      GO TO 100

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

  200 FORMAT (  10X,'INTERACTIVE DATA ENTRY FOR RESIDUE',
     &        /,10X,'==================================')
  250 FORMAT (
     & 1X,'|---|---------|--------|-------|---------------|----------',
     &    '|-----|-----|-----|',/,
     & 1X,'| # | Date    | Amount | Depth | Residue Mater.| Incorp.% ',
     &    '|  N% |  P% |  K% |',/,
     & 1X,'|   |         |  Kg/Ha |   cm  |               |          ',
     &    '|     |     |     |',/,
     & 1X,'|---|---------|--------|-------|---------------|----------',
     &    '|-----|-----|-----|')
  275 FORMAT (
     & 1X,'|---|---------|--------|-------|---------------|----------',
     &    '|-----|-----|-----|',/)

  285 FORMAT (
     & 1X,'|',I2,1X,'|',I4,1X,I3,1X,'|',F7.1,1X,'|',F6.1,1X,'| ',A14,
     &    '| ',F5.1,4X,'|',F5.1,'|',F5.1,'|',F5.1,'|')
  295 FORMAT (/9X,
     &     '(E)dit, (A)dd an event, (D)elete, (Q)uit (Enter = Done)',$)
  296 FORMAT (//,9X,
     &    'This option is not available for the current Residue',/,
     & 9X,'management selection.  Please change selection first.', $)
  400 FORMAT (/,9X,'Residue application date [YRDOY]',/,
     &          9X,'New application date ?   --->',1X,' ',$)
  401 FORMAT (/,9X,'Residue application day [DAP]',/,
     &          9X,'New application date ?   --->',1X,' ',$)
  410 FORMAT (/,9X,'Residue application date [YRDOY] ===>',2X,I7,
     &        /,9X,'New application date ?           --->',1X,' ',$)
  411 FORMAT (/,9X,'Residue application day [DAP]    ===>',2X,I5,
     &        /,9X,'New application date ?           --->',1X,' ',$)
  450 FORMAT (80A1)
  500 FORMAT (/,9X,'Residue amount            ===>',1X,F10.3,' kg/ha',
     &        /,9X,'New Residue amount ?      --->',3X,' ',$)
  600 FORMAT (/,9X,'ERROR! Values are out of range',/)
  800 FORMAT (/,9X,'Please enter entry number to delete ',$)
  850 FORMAT (/,9X,'Please enter entry number to edit   ',$)
  900 FORMAT (/,9X,'Residue application depth ===>',1X,F7.3,' cm',
     &        /,9X,'New Residue depth  ?      --->',3X,' ',$)
  910 FORMAT (/,9X,'Residue incorporation     ===>',1X,F7.3,' %',
     &        /,9X,'New incorporation % ?     --->',3X,' ',$)
  920 FORMAT (/,9X,'Residue % nitrogen        ===>',1X,F7.3,' %',
     &        /,9X,'New nitrogen % ?          --->',3X,' ',$)
  930 FORMAT (/,9X,'Residue % phosphorus      ===>',1X,F7.3,' %',
     &        /,9X,'New phosphorus % ?        --->',3X,' ',$)
  940 FORMAT (/,9X,'Residue % potassium       ===>',1X,F7.3,' %',
     &        /,9X,'New potassium % ?         --->',3X,' ',$)
 4900 FORMAT (//,9X,'|---|-----------------------------|',/,
     &           9X,'| # |   Residue Type              |',/,
     &           9X,'|---|-----------------------------|',/,
     &           4(9X,'|',I2,' |',1X,A17,10X,' |',/),
     &           9X,'|---|-----------------------------|',//,
     &           9X,'Residue type selected     ===>',1X,I3,
     &         /,9X,'New type ?                --->  ',$)

      END SUBROUTINE ENTRES
