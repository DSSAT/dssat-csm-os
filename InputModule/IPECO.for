C=======================================================================
C  IPECO, Subroutine
C
C  Determines variety selection
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1990 GH  Written
C  05/28/1993 PWW Header revision and minor changes
C-----------------------------------------------------------------------
C  INPUT  : FILEE,NSENS,IVRGRP,IVRTEM,CSDVAR,CLDVAR,THVAR,PHTHRS,
C           SDPRO,TRIFOL,SIZELF,THRESH,LNGSH,RWIDTH,RHGHT,PATHEC,PPSEN,
C           PM06,PM09,PH2T5,ECOTYP,ECONAM,ECONO
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SEVAR SENS INPUT
C
C  Calls  : ERROR CLEAR IGNORE VERIFY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IPECO (FILEE,NSENS,RNMODE,PATHEC,ECOTYP,ECONAM,
     &                  ECONO,IVRGRP,MODEL)

      IMPLICIT      NONE
      EXTERNAL CLEAR, ERROR, IGNORE, VERIFY

      CHARACTER*1   LINE(80),RNMODE,BLANK,ANS
      CHARACTER*6   ERRKEY,ECOTYP,ECONO
	CHARACTER*8   MODEL
      CHARACTER*12  FILEE
      CHARACTER*16  ECONAM
      CHARACTER*80  PATHEC
      CHARACTER*92  FILEGC
      CHARACTER*255 C255

      INTEGER       I,IVRGRP,IVRTEM,NSENS,NLECO
      INTEGER       LUNECO,LINECO,ISECT,NLOOP,ERRNUM,PATHL

      REAL          FLAG,ECO

      PARAMETER (LUNECO = 19)
      PARAMETER (ERRKEY = 'IPECO ')
      PARAMETER (BLANK  = ' ')

      NLECO = 0

      PATHL  = INDEX(PATHEC,BLANK)
      IF (PATHL .LE. 1) THEN
         FILEGC = FILEE
       ELSE
         FILEGC = PATHEC(1:(PATHL-1)) // FILEE
      ENDIF

C-----------------------------------------------------------------------
C    Read Ecotype Parameter File
C-----------------------------------------------------------------------

      OPEN (LUNECO,FILE = FILEGC,STATUS = 'OLD',IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEE,0)

      IF (NSENS .EQ. 1) THEN
         I  = 1
         NLOOP = 0
         IF (INDEX('IE',RNMODE) .GT. 0) THEN
            CALL CLEAR
            WRITE (*,100)
         ENDIF

  200    CONTINUE
         CALL IGNORE (LUNECO, LINECO, ISECT, C255)
         IF (ISECT .EQ. 0) GO TO 211
         IF (ISECT .EQ. 2) GO TO 200
         IF (C255(1:1) .EQ. ' ' .OR. C255(1:1) .EQ. '*' ) GO TO 200
	   IF (MODEL(3:5) .EQ. 'GRO') THEN
           READ (C255,3100,IOSTAT=ERRNUM) ECOTYP,ECONAM,IVRGRP,IVRTEM
	   ELSE
	     READ (C255,3100,IOSTAT=ERRNUM) ECOTYP,ECONAM
           IVRGRP = 1
           IVRTEM = 1
	   ENDIF
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEE,LINECO)
         IF (INDEX('IE',RNMODE) .GT. 0) THEN
            WRITE(*,750) I,ECOTYP,ECONAM,IVRGRP,IVRTEM
         ENDIF
         IF (ECOTYP .EQ. ECONO) NLECO = I
         IF (IVRGRP .LE. 0) IVRGRP = -99
         IF (IVRTEM .LE. 0) IVRTEM = -99
C
C        Write Pause Statement Every 15 lines
C
         IF (MOD(I,15) .EQ. 0 .AND. INDEX('IE',RNMODE) .GT. 0) THEN
            WRITE (*,300)
            READ  (5,'(A1)') ANS
         ENDIF
         I  = I + 1
         GOTO 200

  211    CONTINUE
         NLOOP = NLOOP + 1
         IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,FILEE,LINECO)
         LINE(1) = ' '
         IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,1300) NLECO
         READ (5, '(80A1)') LINE
         CALL VERIFY (LINE,ECO,FLAG)

         IF (ECO .LE. 0) THEN
            ECO = NLECO
          ELSE IF ((FLAG .GT. 0) .OR. (ECO .GT. (I-1))) THEN
            WRITE (*,1200) I -1
            GO TO 211
          ELSE IF (ECO .NE. NINT(ECO)) THEN
            WRITE (*,1201)
            GO TO 211
          ELSE IF (ECO .GT. 0) THEN
            NLECO = NINT(ECO)
          ELSE
            GO TO 211
         ENDIF

         REWIND (LUNECO)
      ENDIF

      I = 0
 2010 CONTINUE
      I = I + 1
 2000 CONTINUE
      CALL IGNORE (LUNECO, LINECO, ISECT, C255)
      IF (ISECT .EQ. 0) GO TO 3200
      IF (ISECT .EQ. 2) GO TO 2000
      IF (C255(1:1) .EQ. ' ' .OR. C255(1:1) .EQ. '*') GO TO 2000
	IF (MODEL(3:5) .EQ. 'GRO') THEN
        READ (C255,3100,IOSTAT=ERRNUM) ECOTYP,ECONAM,IVRGRP,IVRTEM
	ELSE
	  READ (C255,3100,IOSTAT=ERRNUM) ECOTYP,ECONAM
        IVRGRP = 1
	  IVRTEM = 1
	ENDIF
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEE,LINECO)
      IF (((ECOTYP .EQ. ECONO) .AND. (NSENS .EQ. 0)) .OR.
     &         ((I .EQ. NLECO) .AND. (NSENS .EQ. 1))) THEN
         ECONO = ECOTYP
         CLOSE (LUNECO)
         RETURN
       ELSE
         GO TO 2010
      ENDIF

 3200 CONTINUE
      IF (ECONO .EQ. 'DFAULT') CALL ERROR (ERRKEY,3,FILEE,LINECO)
      ECONO = 'DFAULT'
      REWIND (LUNECO)
      I = 0
      GO TO 2010

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

  100 FORMAT (T30,'ECOTYPE SELECTION',/,T30,'=================',
     &     //,T43,'PHOTOPERIOD',2X,'TEMPERATURE',
     &      /,2X,'NO.',1X,'ENTRY',3X,'VARIETY',24X,'GROUP',8X,'GROUP',
     &      /,2X,'---',1X,'------',2X,20('-'),8X,11('-'),2X,
     &        11('-'))
  300 FORMAT (/,'  More.... press < ENTER > key')
  750 FORMAT (I4,') ',A6,2X,A16,15X,I3,10X,I3)
 1200 FORMAT (6X,'ERROR! Ecotype Selection must be between 1 and ',I3,/)
 1201 FORMAT (6X,'ERROR! Ecotype Selection must be an INTEGER value',/)
 1300 FORMAT (/,6X,'ECOTYPE SELECTED ===>',1X,I4,
     &        /,6X,'NEW SELECTION ?  --->',3X,' ',$)
 3100 FORMAT (A6,1X,A16,1X,2(1X,I2))

      END SUBROUTINE IPECO
