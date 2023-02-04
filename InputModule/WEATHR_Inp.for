C=======================================================================
C  WEATHR, Subroutine
C
C  Weather handling routine: input, weather modification and
C  generation of hourly values.
C-----------------------------------------------------------------------
C  Revision history
C
C  08/30/1991 NBP Written 
C  05/28/1993 PWW Header revision and minor changes 
!  05/18/2006 CHP Modifications for input subroutine module
C-----------------------------------------------------------------------
C  INPUT  : CO2ADJ,CO2FAC,DAYADJ,DAYFAC,DPTADJ,DPTFAC,PRCADJ,PRCFAC,
C           RADADJ,RADFAC,TMADJ,TMFAC,TXADJ,TXFAC,WMODB,WMODI,WNDADJ,
C           WNDFAC,WTHADJ,CO2
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : INPUT
C
C  Calls  : ERROR WTHMDB WTHMOD
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE WEATHR_Inp (CO2ADJ,CO2FAC,DAYADJ,DAYFAC,DPTADJ,DPTFAC,
     &           PRCADJ,PRCFAC,RADADJ,RADFAC,TMADJ,TMFAC,
     &           TXADJ,TXFAC,WMODI,WNDADJ,WNDFAC,WTHADJ,
     &           CO2,WTHSTR,NEV)

      IMPLICIT NONE
      EXTERNAL WTHSUM

      CHARACTER*1   CO2FAC(10),DAYFAC(10),DPTFAC(10),WNDFAC(10)
      CHARACTER*1   PRCFAC(10),RADFAC(10),TMFAC(10),TXFAC(10)
      CHARACTER*1   WMODI
      CHARACTER*120 WTHSTR

      INTEGER NEV

      REAL    CO2,CO2ADJ(10),DAYADJ(10),DPTADJ(10)
      REAL    PRCADJ(10),RADADJ(10),TMADJ(10)
      REAL    TXADJ(10),WNDADJ(10),WTHADJ(2,8)

!      PARAMETER (CO2BAS = 330.)

!      CO2 = CO2BAS
C
C     Adjustment of CO2
C
      IF (WMODI .EQ. 'Y' .AND. WTHADJ(1,6) .NE. 0. .OR.
     &    WMODI .EQ. 'Y' .AND. WTHADJ(2,6) .NE. 1.) THEN
!          CO2 = WTHADJ(1,6) + CO2BAS*WTHADJ(2,6)
          CO2 = WTHADJ(1,6) + CO2*WTHADJ(2,6)
      ENDIF

      CALL WTHSUM (WTHADJ,WTHSTR)

      IF (WMODI .EQ. 'Y') THEN
         READ(WTHSTR(1:120),75) DAYFAC(1),DAYADJ(1),
     &   RADFAC(1),RADADJ(1),TXFAC(1),TXADJ(1),TMFAC(1),TMADJ(1),
     &   PRCFAC(1),PRCADJ(1),CO2FAC(1),CO2ADJ(1),DPTFAC(1),DPTADJ(1),
     &   WNDFAC(1),WNDADJ(1)
      ENDIF


      IF (NEV .GE. 1) THEN
!         IF (CO2FAC(1) .EQ. 'A' .AND. CO2ADJ(1) .EQ. 0.) THEN
!             CO2FAC(1) = 'R'
!             CO2ADJ(1) = CO2
!         ENDIF
         WRITE (WTHSTR(1:120),80)
     &  'DAYL= ',DAYFAC(1),DAYADJ(1),'SRAD= ',RADFAC(1),RADADJ(1),
     &  'TMAX= ',TXFAC(1),TXADJ(1),  'TMIN= ',TMFAC(1),TMADJ(1),
     &  'RAIN= ',PRCFAC(1),PRCADJ(1),'CO2 = ',CO2FAC(1),CO2ADJ(1),
     &  'DEW = ',DPTFAC(1),DPTADJ(1),'WIND= ',WNDFAC(1),WNDADJ(1)
  80  FORMAT(5(A6,A1,F6.2,2X),1(A6,A1,F6.1,2X),2(A5,A1,F6.2,2X))
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

  75  FORMAT (8(6X,A1,F6.0,2X))
!  80  FORMAT (8(A6,A1,F6.2,2X))

      END SUBROUTINE WEATHR_Inp
