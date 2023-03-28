C=======================================================================
C  IPENV, Subroutine
C
C  Input environmental data and check for errors.
C-----------------------------------------------------------------------
C  Revision history
C
C  09/15/1991 NBP Written                             
C  05/28/1993 PWW Header revision and minor changes   
C  10/23/2002 CHP Modified for Y2K
C  02/03/2005 GH  Corrected error checking for missing levels
C  05/07/2020 FO  Added new Y4K subroutine call to convert YRDOY
C-----------------------------------------------------------------------
C  INPUT  : FILEX,LNENV,LUNEXP
C
C  LOCAL  : ERRKEY,ERRNUM,FOUND,LINE,LN
C
C  OUTPUT : CO2ADJ,CO2FAC,DAYADJ,DAYFAC,DPTADJ,DPTFAC,NEV,PRCADJ,PRCFAC,
C           RADADJ,RADFAC,TMADJ,TMFAC,TXADJ,TXFAC,WMDATE,WNDADJ,WNDFAC
C
C  Fn/Sub : EOSECT,ERROR,FIND,IGNORE,VALSTR
C
C  Ifile  : *.SBX
C-----------------------------------------------------------------------
C  Called : IPEXP
C
C  Calls  : FIND IGNORE ERROR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IPENV(FILEX,LNENV,LUNEXP,CO2ADJ,CO2FAC,DAYADJ,
     &  DAYFAC,DPTADJ,DPTFAC,NEV,PRCADJ,PRCFAC,RADADJ,RADFAC,
     &  TMADJ,TMFAC,TXADJ,TXFAC,WMDATE,WMODI,WNDADJ,WNDFAC,
     &  WTHADJ)   !,YRSIM)

!     2023-01-26 chp removed unused variables in argument list:
!       YRSIM

      USE ModuleDefs
      IMPLICIT    NONE
      EXTERNAL FIND, ERROR, IGNORE, Y4K_DOY

      CHARACTER   ERRKEY*6,LINE*80,FILEX*12,WMODI*1,FINDCH*6
      CHARACTER*1 DAYFAC(NAPPL),RADFAC(NAPPL),TXFAC (NAPPL)
      CHARACTER*1 TMFAC(NAPPL), WNDFAC(NAPPL)
      CHARACTER*1 PRCFAC(NAPPL),CO2FAC(NAPPL),DPTFAC(NAPPL)
      INTEGER     ERRNUM,FOUND,I,LN,LNENV,LUNEXP,LINEXP,NEV
      INTEGER     WMDATE(NAPPL)   !,YRSIM

      REAL        DAYADJ(NAPPL),RADADJ(NAPPL),TXADJ (NAPPL),TMADJ(NAPPL)
      REAL        PRCADJ(NAPPL)
      REAL        CO2ADJ(NAPPL),DPTADJ(NAPPL),WNDADJ(NAPPL),WTHADJ(2,8)

      PARAMETER  (ERRKEY = 'IPENV ')

      FINDCH = '*ENVIR'
C
C     Initialize
C
      DO I = 1, 8
        WTHADJ(1,I) = 0.0
        WTHADJ(2,I) = 1.0
      END DO

	DO I = 1, NAPPL
        WMDATE(I)  = 0
        DAYADJ(I)  = 0.0
        RADADJ(I)  = 0.0
        TXADJ(I)   = 0.0
        TMADJ(I)   = 0.0
        PRCADJ(I)  = 0.0
        CO2ADJ(I)  = 0.0
        DPTADJ(I)  = 0.0
        WNDADJ(I)  = 0.0
      END DO

      WMODI  = 'N'
      NEV    =  0
      LINEXP =  0

      IF (LNENV .EQ. 0) GO TO 30
C
C     Find environmental section
C
      CALL FIND (LUNEXP,FINDCH,LINEXP,FOUND)
      IF (FOUND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)
C
C     If found, skip to next valid data line containing the level number.
C     Blank or comment lines are ignored.  NEV=0 if no valid lines are found.
C
      IF (FOUND .EQ. 1) THEN
         NEV = 1
   20    CALL IGNORE (LUNEXP,LINEXP,FOUND,LINE)
         IF (FOUND .EQ. 1) THEN
            READ (LINE,'(I2)',IOSTAT=ERRNUM) LN
            IF (ERRNUM.NE.0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)

            IF (LN .EQ. LNENV) THEN
               READ (LINE,1000,IOSTAT=ERRNUM)   LN,WMDATE(NEV),
     &         DAYFAC(NEV),DAYADJ(NEV),RADFAC(NEV),RADADJ(NEV),
     &         TXFAC (NEV),TXADJ (NEV),TMFAC (NEV),TMADJ (NEV),
     &         PRCFAC(NEV),PRCADJ(NEV),CO2FAC(NEV),CO2ADJ(NEV),
     &         DPTFAC(NEV),DPTADJ(NEV),WNDFAC(NEV),WNDADJ(NEV)
           
C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY
	             !CALL Y2K_DOY (WMDATE(NEV))
               CALL Y4K_DOY (WMDATE(NEV),FILEX,LINEXP,ERRKEY,3)

               IF (DAYADJ(NEV) .LE. -90.) DAYADJ(NEV) = 0.0
               IF (RADADJ(NEV) .LE. -90.) RADADJ(NEV) = 0.0
               IF (TXADJ(NEV)  .LE. -90.) TXADJ(NEV)  = 0.0
               IF (TMADJ(NEV)  .LE. -90.) TMADJ(NEV)  = 0.0
               IF (PRCADJ(NEV) .LE. -90.) PRCADJ(NEV) = 0.0
               IF (CO2ADJ(NEV) .LE. -90.) CO2ADJ(NEV) = 0.0
               IF (DPTADJ(NEV) .LE. -90.) DPTADJ(NEV) = 0.0
               IF (WNDADJ(NEV) .LE. -90.) WNDADJ(NEV) = 0.0

               IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)
               NEV = NEV + 1
            ENDIF

            GOTO 20
          ELSE
	    IF (NEV .EQ. 1) THEN
	        CALL ERROR (ERRKEY,2,FILEX,LINEXP)
	      ENDIF
            GOTO 30
         ENDIF
      ENDIF

   30 REWIND (LUNEXP)
      NEV = MAX((NEV-1),0)  

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 1000 FORMAT (I3,I5,5(1X,A1,F4.0),1X,A1,F4.0,2(1X,A1,F4.0))

      END SUBROUTINE IPENV
