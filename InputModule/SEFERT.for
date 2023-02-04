C=======================================================================
C  SEFERT, Subroutine
C
C  Determines N fertilizer management sensitivity analysis options
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1990 GH  Written
C  05/28/1993 PWW Header revision and minor changes  
C  12/14/2000 GH  Write revision
C  06/09/2002 GH  Modified for Y2K
C  08/23/2002 GH  Expanded array for fertilizer applications to 200
C-----------------------------------------------------------------------
C  INPUT  : IDETN,ISWNIT,ISWWAT,NIMANT,NFMANT,IFERI,ISWSYM,EFINOC,
C           EFNFIX,DSOILN,SOILNC,SOILNX,FTYPEN,NFERT,IFTYPE,ANFER,DFERT,
C           TOTNAP,FDAY
C
C  LOCAL  : ERRKEY,FTYPE,MENU,NLOOP
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SENS
C
C  Calls  : CLEAR SWFERT AUTFER ENTFER SWFIX SEFIX
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SEFERT (IDETN,RNMODE,ISWNIT,ISWWAT,NIMANT,NFMANT,
     &    IFERI,ISWSYM,EFINOC,EFNFIX,DSOILN,SOILNC,SOILNX,FTYPEN,
     &    NFERT,IFTYPE,ANFER,DFERT,TOTNAP,FDAY,NSWITCH,CROP,NCODE,
     &    FERCOD)

      USE ModuleDefs
      IMPLICIT     NONE
      EXTERNAL ERROR, CLEAR, SENIT, SWFERT, AUTFER, ENTFER, SWFIX, SEFIX

      CHARACTER*1  RNMODE,IDETN,ISWWAT,ISWNIT,IFERI,ISWSYM
      CHARACTER*2  CROP
      CHARACTER*5  IFTYPE(NAPPL),FERCOD(NAPPL),NCODE
      CHARACTER*6  ERRKEY
      CHARACTER*15 NFMANT
      CHARACTER*25 FTYPE(22),NIMANT
      CHARACTER*34 ATYPE(20)

      INTEGER      MENU,NLOOP,FTYPEN,NFERT,FDAY(NAPPL),IFFTYP(NAPPL)
      INTEGER      IFFCOD(NAPPL),I,NSWITCH
      REAL         EFINOC,EFNFIX,DSOILN,SOILNC,SOILNX,DFERT(NAPPL),
     &             ANFER(NAPPL),TOTNAP

      PARAMETER (ERRKEY = 'SEFERT')

!     CHP 2/19/2008 - Match values in Fert_Place
C     FTYPE ( 0) = 'None applied             '
      FTYPE ( 1) = 'Ammonium nitrate         '
      FTYPE ( 2) = 'Ammonium sulphate        '
      FTYPE ( 3) = 'Ammonium nitrate-sulphate'
      FTYPE ( 4) = 'Anhydrous ammonia        '
      FTYPE ( 5) = 'Urea                     '
      FTYPE ( 6) = 'Diammonium phosphate     '
      FTYPE ( 7) = 'Monoammonium phosphate   '
      FTYPE ( 8) = 'Calcium nitrate          '
      FTYPE ( 9) = 'Aqua ammonia             '
      FTYPE (10) = 'Urea Ammonium nitrate    '
      FTYPE (11) = 'Calcium ammonium nitrate '
      FTYPE (12) = 'Ammonium poly-phosphate  '
      FTYPE (13) = 'Single super phosphate   '
      FTYPE (14) = 'Triple super phosphate   '
      FTYPE (15) = 'Liquid phosphoric acid   '
      FTYPE (16) = 'Potassium chloride       '
      FTYPE (17) = 'Potassium Nitrate        '
      FTYPE (18) = 'Potassium sulfate        '
      FTYPE (19) = 'Urea super granules      '
      FTYPE (20) = 'Dolomitic limestone      '
      FTYPE (21) = 'Rock phosphate           '
      FTYPE (22) = 'Calcitic limestone       '
!      FTYPE (24) = 'Rhizobium                '
!      FTYPE (26) = 'Calcium hydroxide        '
!      19-22      = Reserved for control release fert. 
!             51  = Urea Super Granule

C     ATYPE ( 0) = 'Applied when required             '
      ATYPE ( 1) = 'Broadcast, not incorporated       '
      ATYPE ( 2) = 'Broadcast, incorporated           '
      ATYPE ( 3) = 'Banded on surface                 '
      ATYPE ( 4) = 'Banded below surface              '
      ATYPE ( 5) = 'Applied in irr. water             '
      ATYPE ( 6) = 'Foliar spray                      '
      ATYPE ( 7) = 'Bottom of hole                    '
      ATYPE ( 8) = 'On the seed                       '
      ATYPE ( 9) = 'Injected                          '
      ATYPE (11) = 'Broadcast, unincorporated         '      
      ATYPE (12) = 'Broadcast, 15% in soil            '
      ATYPE (13) = 'Broadcast, 30% in soil            '
      ATYPE (14) = 'Broadcast, 45% in soil            '
      ATYPE (15) = 'Broadcast, 60% in soil            '
      ATYPE (16) = 'Broadcast, 75% in soil            '
      ATYPE (17) = 'Broadcast, 90% in soil            '
      ATYPE (18) = 'Band on saturated soil,92% in soil'        
      ATYPE (19) = 'Deeply placed pellets, 95% in soil'
      ATYPE (20) = 'Deeply placed pellets,100% in soil'

      DO I = 1, NFERT
       READ (IFTYPE(I)(4:5),'(I2)') IFFTYP(I)
       READ (FERCOD(I)(4:5),'(I2)') IFFCOD(I)
        IF (ABS(IFFCOD(I)) .GT. 20) THEN
          IFFCOD(I) = 2                            ! Fully incorporated
       ENDIF
      END DO

      NLOOP = 0
  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. NAPPL) CALL ERROR (ERRKEY,1,' ',0)

      IF (INDEX('IE',RNMODE) .GT. 0)  THEN
       CALL CLEAR
       IF (ISWNIT .NE. 'Y') THEN
          NIMANT = 'NO N-BALANCE SIMULATION  '
        ELSE IF (IFERI .EQ. 'A') THEN
          NIMANT = 'AUTOMATIC N-FERTILIZER AP'
        ELSE IF (IFERI .EQ. 'F') THEN
          NIMANT = 'AUTOMATIC N-FERTILIZER AP'
        ELSE IF (IFERI .EQ. 'N') THEN
          NIMANT = 'NO N-FERTILIZER APPLIC.  '
        ELSE IF (IFERI .EQ. 'R') THEN
          NIMANT = 'FIELD SCHEDULE-YRDOY     '
        ELSEIF (IFERI .EQ. 'D') THEN
          NIMANT = 'FIELD SCHEDULE-DAP       '
        ELSE
          NIMANT = '                         '
       ENDIF

       IF (ISWNIT .EQ. 'Y') THEN
          ISWWAT = 'Y'
          IF (ISWSYM .EQ. 'Y') THEN
          NFMANT = 'N-FIX SIMULAT. '
          ELSE IF (ISWSYM .EQ. 'N') THEN
          NFMANT = 'NO N-FIX SIMUL.'
          ELSE IF (ISWSYM .EQ. 'U') THEN
          NFMANT = 'UNLIMITED N-FIX'
          ELSE
          NFMANT = 'N-FIX NON LIMIT'
          ENDIF
        ELSE
          NFMANT = '               '
       ENDIF

       WRITE (*,200) ISWNIT,NIMANT,NFMANT,IDETN
       IF (ISWWAT .NE. 'Y') THEN
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
        CALL SWFERT (RNMODE,IFERI,ISWNIT,ISWWAT,NIMANT)
      ELSE IF (MENU .EQ. 3) THEN
        CALL AUTFER (DSOILN,SOILNC,SOILNX,FTYPEN,RNMODE,FTYPE)
        WRITE (NCODE,'(A2,I3.3)') 'IB',FTYPEN
      ELSE IF (MENU .EQ. 4) THEN
        CALL ENTFER (NFERT,FDAY,ANFER,RNMODE,TOTNAP,DFERT,IFFTYP,
     &                 FTYPE,IFFCOD,ATYPE,IFERI)
          DO I = 1, NAPPL
           WRITE (IFTYPE(I),'(A2,I3.3)') 'FE',IFFTYP(I)
           WRITE (FERCOD(I),'(A2,I3.3)') 'AP',IFFCOD(I)
        END DO
      ELSE IF (MENU .EQ. 5) THEN
        CALL SWFIX (RNMODE,ISWNIT,ISWWAT,ISWSYM,NFMANT)
      ELSE IF (MENU .EQ. 6) THEN
        CALL SEFIX (EFNFIX,EFINOC,RNMODE)
      ELSE IF (MENU .EQ. 7) THEN
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
     &     9X,'NITROGEN MANAGEMENT AND MODIFICATION',/,
     &     9X,'====================================',//,
     & 5X,' 0. Return to Main Menu ',//,
     & 5X,' 1. Nitrogen Balance Simulation .......] ',A1,/,
     & 5X,' 2. N-Fertilizer Management ...........] ',A25,/,
     & 5X,' 3. Automatic N-Fertilizer Management .] ',/,
     & 5X,' 4. Enter N-Fertilizer Interactive ....| ',/,
     & 5X,' 5. Nitrogen Fixation .................] ',A15,/,
     & 5X,' 6. Nitrogen Fixation Characteristics..| ',/,
     & 5X,' 7. Nitrogen Output File ..............] ',A1,/)
  280 FORMAT (/,9X,'To be able to simulate the nitrogen balance,',
     &        /,9X,'Please turn on the water balance simulation!',/)
  290 FORMAT (//,9X,'SELECTION ? [ Default = 0 ] ===> ',$)

      END SUBROUTINE SEFERT

C=======================================================================
C  SWFERT, Subroutine
C
C  Determines N-fertilizer management sensitivity analysis options
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1991 GH  Written
C  05/28/1993 PWW Header revision and minor changes 
C  07/16/2002 CHP Increased number of applications to 200 (NAPPL)
C-----------------------------------------------------------------------
C  INPUT  : RNMODE,IFERI,ISWNIT,ISWWAT,NIMANT
C
C  LOCAL  : ERRKEY
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SEFERT
C
C  Calls  : CLEAR ERROR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SWFERT (RNMODE,IFERI,ISWNIT,ISWWAT,NIMANT)

      USE ModuleDefs
      IMPLICIT     NONE
      EXTERNAL ERROR, CLEAR

      CHARACTER*1  RNMODE,IFERI,ISWNIT,ISWWAT
      CHARACTER*6  ERRKEY
      CHARACTER*25 NIMANT

      INTEGER      NLOOP,MENU

      PARAMETER (ERRKEY = 'SWFERT')

      NLOOP = 0

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. NAPPL) CALL ERROR (ERRKEY,1,' ',0)
      IF (INDEX('IE',RNMODE) .GT. 0) THEN
       CALL CLEAR
       WRITE (*,3400)
       WRITE (*, 290) NIMANT
      ENDIF
C
C     Get menu choice
C
      READ (5,'(I2)',ERR = 100) MENU
C
C     Branch to menu choice
C
      IF (MENU .EQ. 1) THEN
        IFERI  = 'N'
        ISWWAT = 'Y'
        ISWNIT = 'Y'
      ELSE IF (MENU .EQ. 2) THEN
        IFERI  = 'R'
        ISWWAT = 'Y'
        ISWNIT = 'Y'
      ELSE IF (MENU .EQ. 3) THEN
        IFERI  = 'D'
        ISWWAT = 'Y'
        ISWNIT = 'Y'
      ELSE IF (MENU .EQ. 4) THEN
        IFERI  = 'A'
        ISWWAT = 'Y'
        ISWNIT = 'Y'
      ELSE IF (MENU .EQ. 5) THEN
        ISWNIT = 'N'
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

  290 FORMAT (//,9X,'CURRENT N-FERTILIZER MANAGEMENT ===> ',A25,/
     &           9X,'SELECTION (#) ? [ Default = 0 ] ---> ',$)
 3400 FORMAT (
     &     9X,'NITROGEN FERTILIZER MANAGEMENT STRATEGY',/,
     &     9X,'=======================================',//,
     & 5X,' 0. Return to Previous Menu ',//
     & 5X,' 1. Not Fertilized .....................................|',/,
     & 5X,' 2. Apply N-Fertilizer According Field Schedule.(YRDOY).|',/,
     & 5X,' 3. Apply N-Fertilizer According To Field Schedule (DAP)|',/,
     & 5X,' 4. Automatic N-Fertilizer Application .................|',/,
     & 5X,' 5. No Nitrogen Stress / No N-Balance Simulation .......|')

      END SUBROUTINE SWFERT

C=======================================================================
C  AUTFER, Subroutine
C
C  Determines automatic N-fertilizer management options
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      5-28-93
C  07/16/2002 CHP Increased number of applications to 200 (NAPPL)
C-----------------------------------------------------------------------
C  INPUT  : DSOILN,SOILNC,SOILNX,FTYPEN,RNMODE,FTYPE
C
C  LOCAL  : LINE,ERRKEY,NLOOP,MENU,I,FLAG,EFF
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SEFERT
C
C  Calls  : CLEAR ERROR VERIFY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE AUTFER (DSOILN,SOILNC,SOILNX,FTYPEN,RNMODE,FTYPE)

      USE ModuleDefs
      IMPLICIT     NONE
      EXTERNAL CLEAR, ERROR, VERIFY

      CHARACTER*1  RNMODE,LINE(80)
      CHARACTER*6  ERRKEY
      CHARACTER*25 FTYPE(20)

      INTEGER      NLOOP,MENU,I,FTYPEN
      REAL         FLAG,DSOILN,SOILNC,SOILNX,EFF

      PARAMETER (ERRKEY = 'AUTFER')

      NLOOP = 0

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. NAPPL) CALL ERROR (ERRKEY,1,' ',0)
      IF (INDEX('IE',RNMODE) .GT. 0) THEN
       CALL CLEAR
       WRITE (*,3900) SOILNC,SOILNX,DSOILN,FTYPE(FTYPEN)
       WRITE (*,4050)
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
        IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,4500) SOILNC
        READ (5,4600) LINE
        CALL VERIFY (LINE,EFF,FLAG)
        IF (EFF .GT. 0.0 .AND. EFF .LE. 100.0 .AND. FLAG .LE. 0) THEN
           SOILNC = EFF
        ENDIF
      ELSE IF (MENU .EQ. 2) THEN
        IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,4700) SOILNX
        READ (5,4600) LINE
        CALL VERIFY (LINE,EFF,FLAG)
        IF (EFF .GE. 0.0 .AND. FLAG .LE. 0.0) THEN
           SOILNX = EFF
        ENDIF
      ELSE IF (MENU .EQ. 3) THEN
        IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,4800) DSOILN
        READ (5,4600) LINE
        CALL VERIFY (LINE,EFF,FLAG)
        IF (EFF .GE. 0.0 .AND. EFF .LT. 200.0 .AND. FLAG .LE. 0) THEN
           DSOILN = EFF
        ENDIF
      ELSE IF (MENU .EQ. 4) THEN
        IF (INDEX('IE',RNMODE) .GT. 0) THEN
           CALL CLEAR
           WRITE (*,4900) (I,FTYPE(I),I=1,17), FTYPEN
        ENDIF
        READ (5,4600) LINE
        CALL VERIFY (LINE,EFF,FLAG)
        IF (EFF .GT. 0.0 .AND. EFF .LE. 17.0 .AND. FLAG .LE. 0) THEN
           FTYPEN = NINT(EFF)
        ENDIF
      ENDIF

      GO TO 100

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

 3900 FORMAT (
     &     9X,'AUTOMATIC NITROGEN FERTILIZER APPLICATION',/,
     &     9X,'=========================================',//,
     & 5X,' 0. Return to Previous Menu ',//
     & 5X,' 1. Threshold - Nitrogen Stress Factor ........(%)] ',F8.3,/,
     & 5X,' 2. Amount of N-Fertilizer Per Application .kg/ha)] ',F8.3,/,
     & 5X,' 3. Application Depth ........................(cm)] ',F8.3,/,
     & 5X,' 4. Material Code ............................ (#)] ',A25,/)
 4050 FORMAT (//,9X,'SELECTION (#) ? [ Default = 0 ] ---> ',$)
 4500 FORMAT (//,9X,'THRESHOLD N-STRESS FACTOR ===>',1X,F8.3,/,
     &           9X,'[PERCENT - VALUE BETWEEN 0 and 100]',/,
     &           9X,'NEW THRESHOLD FACTOR ?    --->  ',$)
 4600 FORMAT (80A1)
 4700 FORMAT (//,9X,'N-FERTILIZER PER APPLICATION  ===>',1X,F8.3,
     &' kg/ha',/,9X,'NEW AMOUNT ?                  --->  ',$)
 4800 FORMAT (//,9X,'FERTILIZER APPLICATION DEPTH  ===>',1X,F8.3,' cm',
     &         /,9X,'NEW DEPTH ?                   --->  ',$)
 4900 FORMAT (//,9X,'  #',5X,'N-FERTILIZER TYPE',/
     &           9X,' ==',5X,'=================',/,
     &           17(9X,I3,5X,A25,/),/,
     &              9X,'FERTILIZER TYPE SELECTED ===>',1X,I3,
     &            /,9X,'NEW TYPE ?               --->  ',$)

      END

C=======================================================================
C  SWFIX, Subroutine
C
C  Determines N fixation strategy options
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      5-28-93
C  07/16/2002 CHP Increased number of applications to 200 (NAPPL)
C-----------------------------------------------------------------------
C  INPUT  : RNMODE,ISWNIT,ISWWAT,ISWSYM,NFMANT
C
C  LOCAL  : ERRKEY,NLOOP,MENU
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SEFERT
C
C  Calls  : ERROR CLEAR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SWFIX (RNMODE,ISWNIT,ISWWAT,ISWSYM,NFMANT)

      USE ModuleDefs
      IMPLICIT     NONE
      EXTERNAL ERROR, CLEAR

      CHARACTER*1  RNMODE,ISWNIT,ISWSYM,ISWWAT
      CHARACTER*6  ERRKEY
      CHARACTER*15 NFMANT

      INTEGER      NLOOP,MENU

      PARAMETER (ERRKEY = 'SWFIX ')

      NLOOP = 0

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. NAPPL) CALL ERROR (ERRKEY,1,' ',0)
      IF (INDEX('IE',RNMODE) .GT. 0) THEN
       CALL CLEAR
       WRITE (*,3400)
       WRITE (*, 290) NFMANT
      ENDIF
C
C     Get menu choice
C
      READ (5,'(I2)',ERR = 100) MENU
C
C     Branch to menu choice
C
      IF (MENU .EQ. 1) THEN
        ISWSYM = 'N'
        ISWNIT = 'Y'
        ISWWAT = 'Y'
      ELSE IF (MENU .EQ. 2) THEN
        ISWSYM = 'Y'
        ISWNIT = 'Y'
        ISWWAT = 'Y'
      ELSE IF (MENU .EQ. 3) THEN
        ISWSYM = 'U'
        ISWNIT = 'Y'
        ISWWAT = 'Y'
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

  290 FORMAT (//,9X,'CURRENT NITROGEN FIXATION SELECTION ===> ',A15,/
     &           9X,'SELECTION (#) ? [ Default = 0 ]     ---> ',$)
 3400 FORMAT (   9X,'NITROGEN FIXATION STRATEGY',/,
     &           9X,'==========================',//,
     & 5X,' 0. Return to Previous Menu ',//
     & 5X,' 1. No Nitrogen Fixation Simulation ..................|',/,
     & 5X,' 2. Dynamic Nitrogen Fixation Simulation .............|',/,
     & 5X,' 3. Unlimited Nitrogen Fixation Simulation ...........|',/)

      END SUBROUTINE SWFIX

C=======================================================================
C  SEFIX, Subroutine
C
C  Determines N fixation characteristics options
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      5-28-93
C  07/16/2002 CHP Increased number of applications to 200 (NAPPL)
C-----------------------------------------------------------------------
C  INPUT  : EFNFIX,EFINOC,RNMODE
C
C  LOCAL  : LINE,ERRKEY,NLOOP,MENU,FLAG,EFF
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SEFERT
C
C  Calls  : ERROR CLEAR VERIFY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SEFIX (EFNFIX,EFINOC,RNMODE)

      USE ModuleDefs
      IMPLICIT    NONE
      EXTERNAL CLEAR, ERROR, VERIFY

      CHARACTER*1 RNMODE,LINE(80)
      CHARACTER*6 ERRKEY

      INTEGER     NLOOP,MENU
      REAL        FLAG,EFF,EFNFIX,EFINOC

      PARAMETER (ERRKEY = 'SEFIX ')

      NLOOP = 0

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. NAPPL) CALL ERROR (ERRKEY,1,' ',0)

      IF (INDEX('IE',RNMODE) .GT. 0) THEN
       CALL CLEAR
       WRITE (*,3900) EFNFIX,EFINOC
       WRITE (*,4050)
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
        IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,4500) EFNFIX
        READ (5,4600) LINE
        CALL VERIFY (LINE,EFF,FLAG)
        IF (EFF .GE. 0.0 .AND. EFF .LE. 1.0 .AND. FLAG .LE. 0) THEN
           EFNFIX = EFF
        ENDIF
      ELSE IF (MENU .EQ. 2) THEN
        IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,4700) EFINOC
        READ (5,4600) LINE
        CALL VERIFY (LINE,EFF,FLAG)
        IF (EFF .GE. 0.0 .AND. EFF .LE. 1.0 .AND. FLAG .LE. 0) THEN
           EFINOC = EFF
        ENDIF
      ENDIF

      GO TO 100

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

 3900 FORMAT (9X,'NITROGEN FIXATION CHARACTERISTICS',/,
     &        9X,'=================================',//,
     &    5X,' 0. Return to Previous Menu ',//
     &    5X,' 1. Rhizobia Effectiveness ........] ',F6.3,/,
     &    5X,' 2. Rhizobia Number (Relative .....] ',F6.3,/)
 4050 FORMAT (//,9X,'SELECTION (#) ? [ Default = 0 ] ---> ',$)
 4500 FORMAT (//,9X,'RHIZOBIA EFFECTIVENESS    ===>',1X,F6.3,/,
     &           9X,'[FRACTION - VALUE BETWEEN 0 and 1 ]',/,
     &           9X,'NEW FACTOR ?              --->  ',$)
 4600 FORMAT (80A1)
 4700 FORMAT (//,9X,'RHIZOBIA NUMBER           ===>',1X,F6.3,/,
     &           9X,'[RELATIVE - VALUE BETWEEN 0 and 1 ]',/,
     &           9X,'NEW NUMBER ?              --->  ',$)

      END SUBROUTINE SEFIX

C=======================================================================
C  ENTFER, Subroutine
C
C  Determines fertilizer data entry
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1991 GH  Written
C  05/28/1993 PWW Header revision and minor changes  
C  07/16/2002 CHP Increased number of applications to 200 (NAPPL)
C  05/07/2020 FO  Added new Y4K subroutine call to convert YRDOY
C-----------------------------------------------------------------------
C  INPUT  : NENTRY,EDAY,EMOUNT,TOTAP,EDEPTH,ETYPE,FTYPE
C
C  LOCAL  : LINE,ERRKEY,NENTRY,I,MENU,YRTEMP,YRT,DOYT,NLOOP,EFF,FLAG,TOTAP
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SENS
C
C  Calls  : CLEAR VERIFY YR_DOY SORT
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE ENTFER (NENTRY,EDAY,EMOUNT,RNMODE,TOTAP,EDEPTH,ETYPE,
     &                   FTYPE,MTYPE,ATYPE,IFERI)

      USE ModuleDefs
      IMPLICIT     NONE
      EXTERNAL YR_DOY, ERROR, CLEAR, UPCASE, VERIFY, HOME, Y4K_DOY, SORT

      CHARACTER*1  RNMODE,LINE(80),IMENU,UPCASE,ANS,IFERI
      CHARACTER*6  ERRKEY
      CHARACTER*25 FTYPE(20)
      CHARACTER*34 ATYPE(18)

      INTEGER      NENTRY,I,MENU,YRTEMP,YRT,DOYT,NLOOP
      INTEGER      EDAY(*),ETYPE(*),COUNT,MTYPE(*)
      REAL         EMOUNT(*),EFF,FLAG,TOTAP,EDEPTH(*)
      
      PARAMETER (ERRKEY = 'ENTFER')

      NLOOP = 0
  100 CONTINUE
      NLOOP = NLOOP + 1

      IF (INDEX('IE',RNMODE) .GT. 0)  THEN
         CALL HOME
         IF (NLOOP .GT. NAPPL) CALL ERROR (ERRKEY,1,' ',0)
       WRITE (*,200)
       WRITE (*,250)
       COUNT = 0
       IF (NENTRY .GT. 0) THEN
          DO I = 1, NENTRY
            CALL YR_DOY (EDAY(I),YRT,DOYT)
            WRITE(*,285) I,YRT,DOYT,EMOUNT(I),EDEPTH(I),
     &                     FTYPE(ETYPE(I)),ATYPE(MTYPE(I))
             COUNT = COUNT + 1
             IF (COUNT .EQ. 15) THEN
            COUNT = 0
            WRITE (*,275)
            WRITE (*,1700)
            READ  (5,'(A1)') ANS
            CALL HOME
            WRITE (*,200)
            WRITE (*,250)
             ENDIF
          END DO
       ENDIF
       WRITE (*,275)
       WRITE (*,295)
      ENDIF
      IF (IFERI .NE. 'D' .AND. IFERI .NE. 'R') WRITE(*,296)
C
C     Get menu choice
C
      READ (5,'(A1)',ERR = 100) IMENU
      IF (IFERI .NE. 'D' .AND. IFERI .NE. 'R') RETURN

C
C     Branch to menu choice
C
      IMENU = UPCASE(IMENU)
      IF (IMENU .EQ. 'E') THEN
        IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,850)
        READ (5,'(I5)',ERR = 100) MENU
        IF (MENU .GT. NENTRY .OR. MENU .LE. 0) GOTO 100
      ELSE IF (IMENU .EQ. 'D') THEN
        IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,800)
        READ (5,450) LINE
        CALL VERIFY (LINE,EFF,FLAG)
        IF (EFF .GT. 0. .AND. EFF .LE. NENTRY .AND. FLAG .LE. 0) THEN
           MENU         = NINT(EFF)
           EDAY(MENU)   = 9999999
           EMOUNT(MENU) = 0.0
           ETYPE(MENU)  = 0
           MTYPE(MENU)  = 0
           GO TO 700
        ENDIF
      ELSE IF (IMENU .EQ. 'A') THEN
        NENTRY = NENTRY + 1
        MENU   = NENTRY
  380     IF (INDEX('IE',RNMODE) .GT. 0) THEN
          IF (IFERI .EQ. 'R') THEN
            WRITE (*,400)
          ELSE IF (IFERI .EQ. 'D') THEN
            WRITE (*,401)
          ENDIF
        ENDIF
        READ (5,450) LINE
        CALL VERIFY (LINE,EFF,FLAG)
        IF (EFF .GT. -0.0001 .AND. EFF .LE. 9999999. .AND.FLAG .LE. 0)
     &    THEN
          YRTEMP = NINT(EFF)
C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY
          !CALL Y2K_DOY(YRTEMP)
          CALL Y4K_DOY(YRTEMP,'ENTFER',0,ERRKEY,1)
          IF (IFERI .EQ. 'R') THEN
            CALL YR_DOY (YRTEMP,YRT,DOYT)
            IF (DOYT > 0 .AND. DOYT <= 366 .AND. YRT < 3000) THEN
              EDAY(NENTRY) = YRTEMP
            ELSE
              WRITE (*,600)
              GO TO 380
            ENDIF
          ELSE IF (IFERI .EQ. 'D') THEN
            EDAY(NENTRY) = YRTEMP
            ENDIF
          ELSE
            WRITE (*,600)
            GO TO 380
          ENDIF
        ELSE IF (IMENU .EQ. 'Q') THEN
          RETURN
        ELSE
         RETURN
      ENDIF
      !
      ! Allow user to change the date of fertilizer application,
      ! if editing
      !
      IF (IMENU .EQ. 'E') THEN
 390     IF (INDEX('IE',RNMODE) .GT. 0) THEN
          IF (IFERI .EQ. 'R') THEN
             WRITE (*,410) EDAY(MENU)
          ELSE IF (IFERI .EQ. 'D') THEN
             WRITE (*,411) EDAY(MENU)
          ENDIF
        ENDIF
        READ (5,450) LINE
        CALL VERIFY (LINE,EFF,FLAG)
        IF (EFF .GT. -0.0001 .AND. EFF .LE. 9999999. .AND. FLAG .LE. 0)
     &    THEN
          YRTEMP = NINT(EFF)
C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY
          !CALL Y2K_DOY(YRTEMP)
          CALL Y4K_DOY(YRTEMP,'ENTFER',0,ERRKEY,1)
          IF (IFERI .EQ. 'R') THEN
            CALL YR_DOY (YRTEMP,YRT,DOYT)
            IF (DOYT > 0 .AND. DOYT <= 366 .AND.YRT < 3000) THEN
              EDAY(MENU) = YRTEMP
            ELSE
             WRITE (*,600)
             GO TO 390
            ENDIF
          ELSE IF (IFERI .EQ. 'D') THEN
            EDAY(MENU) = YRTEMP
          ENDIF
        ENDIF
      ENDIF
      !
      ! How much fertilizer?
      !
  550 IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,500) EMOUNT(MENU)
      READ (5,450) LINE
      CALL VERIFY (LINE,EFF,FLAG)
      IF (EFF .GE. 0.0 .AND. EFF .LE. 1000.0 .AND. FLAG .LE. 0) THEN
        EMOUNT(MENU) = EFF
      ELSE IF (FLAG .EQ. 1.0) THEN
        EMOUNT(MENU) = EMOUNT(MENU)
      ELSE
        WRITE (*,600)
        GO TO 550
      ENDIF
      !
      ! Depth of application?
      !
  950 IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,900) EDEPTH(MENU)
      READ (5,450) LINE
      CALL VERIFY (LINE,EFF,FLAG)
      IF (EFF .GE. 0.0 .AND. EFF .LE. 200.0 .AND. FLAG .LE. 0) THEN
        EDEPTH(MENU) = EFF
      ELSE IF (FLAG .EQ. 1.0) THEN
        EDEPTH(MENU) = EDEPTH(MENU)
      ELSE
        WRITE (*,600)
        GO TO 950
      ENDIF
      !
      ! What type of fertilizer?
      !
      IF (INDEX('IE',RNMODE) .GT. 0) CALL CLEAR
 5000 IF (INDEX('IE',RNMODE) .GT. 0) THEN
        WRITE (*,4900) (I,FTYPE(I),I=1,17),ETYPE(MENU)
      ENDIF

      READ (5,450) LINE
      CALL VERIFY (LINE,EFF,FLAG)

      IF (EFF .GT. 0.0 .AND. EFF .LE. 17.0 .AND. FLAG .LE. 0) THEN
        ETYPE(MENU) = NINT(EFF)
      ELSE IF (FLAG .EQ. 1.0 .AND. ETYPE(MENU) .GT. 0) THEN
        ETYPE(MENU) = ETYPE(MENU)
      ELSE
        WRITE (*,600)
        GO TO 5000
      ENDIF
      !
      ! Method of application?
      !

      IF (INDEX('IE',RNMODE) .GT. 0) CALL CLEAR
 6000 IF (INDEX('IE',RNMODE) .GT. 0) THEN
        WRITE (*,4950) (I,ATYPE(I),I=1,18),MTYPE(MENU)
      ENDIF

      READ (5,450) LINE
      CALL VERIFY (LINE,EFF,FLAG)

      IF (EFF .GT. 0.0 .AND. EFF .LE. 18.0 .AND. FLAG .LE. 0) THEN
        MTYPE(MENU) = NINT(EFF)
      ELSE IF (FLAG .EQ. 1.0 .AND. MTYPE(MENU) .GT. 0) THEN
        MTYPE(MENU) = MTYPE(MENU)
      ELSE
        WRITE (*,600)
        GO TO 6000
      ENDIF
      !
      ! Sort entries by date of application
      !
 700  CONTINUE
      CALL SORT (NENTRY,EDAY,EMOUNT,EDEPTH,ETYPE,MTYPE)

      IF (EDAY(NENTRY) .EQ. 9999999) THEN
       EDAY(NENTRY)    = 0
       ETYPE(NENTRY)   = 0
       EDEPTH(NENTRY)  = 0
       NENTRY = NENTRY - 1
      ENDIF

      TOTAP = 0.0

      DO I = 1, NENTRY
       TOTAP = TOTAP + EMOUNT(I)
      END DO

      GO TO 100

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

  200 FORMAT (  9X,'INTERACTIVE DATA ENTRY FOR FERTILIZER',
     &        /,9X,'=====================================')
  250 FORMAT (
     & 1X,'|---|---------|--------|-------|------------------------|',
     &    '--------------------|',/,
     & 1X,'| # | Date    | Amount | Depth |    Fertilizer Type     |',
     &    ' Application Method |',/,
     & 1X,'|   |         |  Kg/Ha |   cm  |                        |',
     &    '                    |',/,
     & 1X,'|---|---------|--------|-------|------------------------|',
     &    '-------------------|')
  275 FORMAT (
     & 1X,'|---|---------|--------|-------|------------------------|',
     &    '--------------------|')
      

  285 FORMAT (
     & 1X,'|',I2,1X,'|',I4,1X,I3,1X,'|',F7.1,1X,'|',F6.1,1X,'| ',
     &          A23,'| ',A19,'|')
  295 FORMAT (/9X,
     &     '(E)dit, (A)dd an event, (D)elete, (Q)uit (Enter = Done)',$)
  296 FORMAT (//,9X,
     &    'This option is not available for the current N-fertilizer',/,
     & 9X,'management selection.  Please change selection first.', $)
  400 FORMAT (/,9X,'Fertilizer application date [YRDOY]',/,
     &          9X,'New application date ?   --->',1X,' ',$)
  401 FORMAT (/,9X,'Fertilizer application day [DAP]',/,
     &          9X,'New application date ?   --->',1X,' ',$)
  410 FORMAT (/,9X,'Fertilizer application date [YRDOY] ===>',2X,I7,
     &        /,9X,'New application date ?              --->',1X,' ',$)
  411 FORMAT (/,9X,'Fertilizer application day [DAP]    ===>',2X,I5,
     &        /,9X,'New application date ?              --->',1X,' ',$)
  450 FORMAT (80A1)
  500 FORMAT (/,9X,'Fertilizer amount           ===>',1X,F10.3,' kg/ha',
     &        /,9X,'New fertilizer amount ?     --->',3X,' ',$)
  600 FORMAT (/,9X,'ERROR! Values are out of range',/)
  800 FORMAT (/,9X,'Please enter entry number to delete ',$)
  850 FORMAT (/,9X,'Please enter entry number to edit   ',$)
  900 FORMAT (/,9X,'Fertilizer application depth ===>',1X,F10.3,' cm',
     &        /,9X,'New fertilizer depth  ?      --->',3X,' ',$)
 1700 FORMAT (/,15X,'There are more entries .. Press any key ..')
 4900 FORMAT (//,9X,'|---|---------------------------|',/,
     &           9X,'| # |   Fertilizer Type         |',/,
     &           9X,'|---|---------------------------|',/,
     &           17(9X,'|',I2,' |',1X,A25,' |',/),
     &           9X,'|---|---------------------------|',//,
     &              9X,'Fertilizer type selected ===>',1X,I3,
     &            /,9X,'New type ?               --->  ',$)
 4950 FORMAT (//,9X,'|---|',35('─'),'|',/,
     &           9X,'| # | Application Method',16X,'|',/,
     &           9X,'|---|',35('─'),'|',/,
     &           18(9X,'|',I2,' |',1X,A34,'|',/),
     &           9X,'|---|',35('─'),'|',//,
     &              9X,'Application method selected ===>',1X,I3,
     &            /,9X,'New method ?                --->  ',$)

      END SUBROUTINE ENTFER
