C=======================================================================
C  IPTILL, Subroutine
C
C  Determines tillage operations for a simulation
C-----------------------------------------------------------------------
C  Revision history
C
C  07/18/1995 GPF Written 
C  04/01/1996 GH  Modified and included in DSSAT v3.1
C  08/19/2002 GH  Modified for Y2K
C  08/23/2002 GH  Expanded array tillage applications to 200
C  02/03/2005 GH  Corrected error checking for missing levels
C  05/07/2020 FO  Added new Y4K subroutine call to convert YRDOY
C  06/24/2022 GH  Check for missing tillage implement
C-----------------------------------------------------------------------
C  INPUT  : LUNEXP,FILEX,LNCHE,CDATE,CHCOD,CHAMT,CHMET,CHDEP,CHT
C           YRSIM,ISWWAT,NCHEM,FOLFR,SOLFR
C
C  LOCAL  : ERRKEY,CHARTEST,ISECT,LINEXP,ERRNUM,J,IFIND,LN
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : IPEXP
C
C  Calls  : FIND IGNORE ERROR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C=======================================================================

      SUBROUTINE IPTILL (LUNEXP,FILEX,LNTIL,YRSIM,ISWTIL,NTIL,TDATE,
     &    TIMPL,TDEP,LNSIM)

      USE ModuleDefs
      IMPLICIT     NONE
      EXTERNAL ERROR, FIND, IGNORE, Y4K_DOY

      CHARACTER*5  TIMPL(NAPPL)
      CHARACTER*6  ERRKEY,FINDCH
      CHARACTER*12 FILEX
      CHARACTER*1  ISWTIL
      CHARACTER*80 CHARTEST

      INTEGER      LNTIL,LUNEXP,ISECT,LINEXP,TDATE(NAPPL),NTIL
      INTEGER      ERRNUM,J,IFIND,LN,YRSIM,LNSIM,TIMPLN
      REAL         TDEP(NAPPL)

      PARAMETER   (ERRKEY ='IPTILL')
      FINDCH ='*TILLA'

      DO J = 1, NAPPL
         TIMPL(J)  = '     '
         TDATE(J)  = 0
         TDEP(J) = 0.0
      END DO

      NTIL = 0
      IF (LNTIL .GT. 0) THEN
         IF (ISWTIL .EQ. 'N' .AND. LNSIM .EQ. 0) THEN
            ISWTIL = 'Y'
         ENDIF
         NTIL = 1
         CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
         IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)
 50      CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)

         IF (ISECT .EQ. 1) THEN
            READ (CHARTEST,60,IOSTAT=ERRNUM) LN
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            IF (LN .NE. LNTIL) GO TO 50
C
C           Read tillage operations
C
            READ (CHARTEST,60,IOSTAT=ERRNUM) LN,TDATE(NTIL),
     &            TIMPL(NTIL),TDEP(NTIL)
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            
C-GH 6/24/2022 Check for missing tillage implement
            READ (TIMPL(NTIL)(3:5),'(I3)',IOSTAT=ERRNUM) TIMPLN
            IF (TIMPLN .LE. 0 .OR. TIMPLN .GE. 999 .OR.
     &          ERRNUM .NE. 0) THEN
               CALL ERROR (ERRKEY,4,FILEX,LINEXP)
            ENDIF 

            IF ((TDATE(NTIL) .LT. 1)  .OR.
     &         (MOD(TDATE(NTIL),1000) .GT. 366)) THEN
               CALL ERROR (ERRKEY,10,FILEX,LINEXP)
            ENDIF
            
C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY
            !CALL Y2K_DOY(TDATE(NTIL))
            CALL Y4K_DOY(TDATE(NTIL),FILEX,LINEXP,ERRKEY,3)

            IF (TDATE(NTIL) .LT. YRSIM) THEN
	             CALL ERROR (ERRKEY,3,FILEX,LINEXP)
            ENDIF

            IF (TDEP(NTIL) .LT. 0) CALL ERROR (ERRKEY,11,FILEX,LINEXP)
            NTIL = NTIL + 1
            IF (NTIL .GT. NAPPL) GO TO 120
          ELSE
	      IF (NTIL .EQ. 1) THEN
	        CALL ERROR (ERRKEY,2,FILEX,LINEXP)
	      ENDIF

            GO TO 120
         ENDIF
         GO TO 50
      ENDIF

 120  REWIND (LUNEXP)
      NTIL = MAX((NTIL-1),0)

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 60   FORMAT (I3,I5,1X,A5,1X,F5.0)

      END SUBROUTINE IPTILL

