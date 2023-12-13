C=======================================================================
C  IPCHEM, Subroutine
C
C  Determines chemical application for a simulation
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1995 GPF Written                                    
C  01/01/1996 GH  Accepted and included in DSSAT v3.1        
C  08/19/2002 GH  Modified for Y2K
C  08/23/2002 GH  Expanded array for chemical applications to 200
C  02/03/2005 GH  Corrected error checking for missing levels
C  05/07/2020 FO  Added new Y4K subroutine call to convert YRDOY
C-----------------------------------------------------------------------
C  INPUT  : LUNEXP,FILEX,LNCHE,CDATE,CHCOD,CHAMT,CHMET,CHDEP,CHT
C           YRSIM,ISWWAT,NCHEM
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
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IPCHEM(LUNEXP,FILEX,LNCHE,YRSIM,NCHEM,CDATE,
     &    CHCOD,CHAMT,CHMET,CHDEP,CHT,ISWCHE,CHEXTR)
     
!     2023-01-26 chp removed unused variables in argument list:
!       ISWWAT, LNSIM,

      USE ModuleDefs
      IMPLICIT     NONE
      EXTERNAL ERROR, FIND, IGNORE, Y4K_DOY

      CHARACTER*1  ISWCHE !ISWWAT,
      CHARACTER*5  CHCOD(NAPPL),CHMET(NAPPL),CHT(NAPPL)
      CHARACTER*6  ERRKEY,FINDCH
      CHARACTER*12 FILEX
      CHARACTER*42 CHEXTR(NAPPL)
      CHARACTER*80 CHARTEST

      INTEGER      LNCHE,LUNEXP,ISECT,LINEXP,CDATE(NAPPL),NCHEM
      INTEGER      ERRNUM,J,IFIND,LN,YRSIM,ICHCOD !,LNSIM
      REAL         CHAMT(NAPPL),CHDEP(NAPPL)

      PARAMETER   (ERRKEY ='IPCHEM')
      FINDCH ='*CHEMI'


      NCHEM = 0
      IF (ISWCHE .EQ. 'N') RETURN

      DO J = 1, NAPPL
         CHCOD(J)  = '     '
         CDATE(J)  = 0
         CHAMT(J)  = 0.0
         CHDEP(J)  = 0.0
      END DO

!      IF ( ISWWAT .NE. 'N' .AND. LNCHE .GT. 0) THEN
!         IF (ISWCHE .EQ. 'N' .AND. LNSIM .EQ. 0) THEN
!            ISWCHE = 'Y'
!         ENDIF
         NCHEM = 1
         CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
         IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)
 50      CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)

         IF (ISECT .EQ. 1) THEN
            READ (CHARTEST,60,IOSTAT=ERRNUM) LN
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            IF (LN .NE. LNCHE) GO TO 50
C
C           Read different chemical types and amounts
C
            READ (CHARTEST,60,IOSTAT=ERRNUM) LN,CDATE(NCHEM),
     &            CHCOD(NCHEM),CHAMT(NCHEM),CHMET(NCHEM),
     &            CHDEP(NCHEM),CHT(NCHEM),CHEXTR(NCHEM)
C    &            CHDEP(NCHEM),CHT(NCHEM)

            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            IF ((CDATE(NCHEM) .LT. 1)  .OR.
     &         (MOD(CDATE(NCHEM),1000) .GT. 366)) THEN
               CALL ERROR (ERRKEY,10,FILEX,LINEXP)
            ENDIF
            
C  FO 05/07/2020 - Add new Y4K subroutine call to convert YRDOY
	          !CALL Y2K_DOY (CDATE(NCHEM))
            CALL Y4K_DOY (CDATE(NCHEM),FILEX,LINEXP,ERRKEY,3)
            IF (CDATE(NCHEM) .LT. YRSIM) THEN
	             CALL ERROR (ERRKEY,3,FILEX,LINEXP)
            ENDIF
            
            IF ((CHAMT(NCHEM) .LT. 0.0) .OR.
     &          (CHAMT(NCHEM) .GT. 9999999.)) THEN
               CALL ERROR (ERRKEY,11,FILEX,LINEXP)
            ENDIF
            
C-GH 7/25/2022 Check for missing chemical code
            READ (CHCOD(NCHEM)(3:5),'(I3)',IOSTAT=ERRNUM) ICHCOD
            IF (ICHCOD .LE. 0 .OR. ICHCOD .GE. 999 .OR.
     &          ERRNUM .NE. 0) THEN
               CALL ERROR (ERRKEY,4,FILEX,LINEXP)
            ENDIF 
            
            NCHEM = NCHEM + 1
            IF (NCHEM .GT. NAPPL) GO TO 120
          ELSE
	      !IF (NCHEM .EQ. 1) THEN
	      !  CALL ERROR (ERRKEY,2,FILEX,LINEXP)
	      !ENDIF
            GO TO 120
         ENDIF
         GO TO 50
!      ENDIF

 120  REWIND (LUNEXP)
      NCHEM = MAX((NCHEM-1),0)

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 60   FORMAT (I3,I5,1X,A5,1X,F5.0,1X,A5,1X,F5.0,1X,A5,A42)
      END SUBROUTINE IPCHEM
