C=======================================================================
C  WTHSET, Subroutine, N.B. Pickering
C  Update WTHADJ array.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/04/1991 NBP Written
C  02/02/1998 GH  Added WTHMDI
C  01/15/2003 GH  Modified screen interactive function
C-----------------------------------------------------------------------
C  Input : AMOUNT,WTYPE,VNUM,WTHADJ
C  Output: WTHADJ
C=======================================================================

      SUBROUTINE WTHSET_Inp (AMOUNT,WTYPE,VNUM,WTHADJ)

      IMPLICIT NONE

      CHARACTER WTYPE*1
      INTEGER   VNUM
      REAL      AMOUNT,WTHADJ(2,8)

C     Change: additive <> 0, multiplicative <> 1, constant > 0.

      IF (WTYPE .EQ. 'A' .AND. AMOUNT .NE. 0.) THEN
         WTHADJ(1,VNUM) = AMOUNT
         WTHADJ(2,VNUM) = 1.0
       ELSE IF (WTYPE .EQ. 'S' .AND. AMOUNT .NE. 0.) THEN
         WTHADJ(1,VNUM) = -AMOUNT
         WTHADJ(2,VNUM) = 1.0
       ELSE IF (WTYPE .EQ. 'M' .AND. AMOUNT .NE. 1.) THEN
         WTHADJ(1,VNUM) = 0.0
         WTHADJ(2,VNUM) = AMOUNT
       ELSE IF (WTYPE .EQ. 'R' .AND. AMOUNT .GE. 0.) THEN
         WTHADJ(1,VNUM) = AMOUNT
         WTHADJ(2,VNUM) = 0.0
       ELSE
         WTHADJ(1,VNUM) = 0.0
         WTHADJ(2,VNUM) = 1.0
      ENDIF

      END SUBROUTINE WTHSET_Inp

C=======================================================================
C  WTHSUM, Subroutine, N.B. Pickering
C  Calculate WTYPE and AMOUNT of weather modification from WTHADJ for
C  display.  Creates summary string for output of all 8 variables.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/17/1992 NBP Written
C-----------------------------------------------------------------------
C  Input : WTHADJ
C  Output: WTHSTR, CO2
C=======================================================================

      SUBROUTINE WTHSUM (WTHADJ,WTHSTR)

      IMPLICIT NONE

      CHARACTER*1   WTYPE(8)
      CHARACTER*120 WTHSTR
      INTEGER       VNUM
      REAL          AMOUNT(8),WTHADJ(2,8)
C
C     Change from WTHADJ array to WTYPE and AMOUNT for display.  All
C     displayed as relative changes except CO2.  The resulting value
C     of CO2 is computed and diplayed as a replacement (R) value.
C
      DO VNUM = 1, 8
!        IF (VNUM .EQ. 6) THEN
!           WTYPE(VNUM)   = 'R'
!           AMOUNT(VNUM) = CO2
!         ELSE
           IF (WTHADJ(1,VNUM).GT.0. .AND. WTHADJ(2,VNUM).EQ.1.) THEN
              WTYPE(VNUM)   = 'A'
              AMOUNT(VNUM) = WTHADJ(1,VNUM)
           ELSEIF (WTHADJ(1,VNUM).LT.0. .AND. WTHADJ(2,VNUM).EQ.1.) THEN
              WTYPE(VNUM)   = 'S'
              AMOUNT(VNUM) = -WTHADJ(1,VNUM)
           ELSEIF (WTHADJ(1,VNUM).EQ.0. .AND. WTHADJ(2,VNUM).NE.1.) THEN
              WTYPE(VNUM)   = 'M'
              AMOUNT(VNUM) = WTHADJ(2,VNUM)
           ELSEIF (WTHADJ(1,VNUM).NE.0. .AND. WTHADJ(2,VNUM).EQ.0.) THEN
              WTYPE(VNUM)   = 'R'
              AMOUNT(VNUM) = WTHADJ(1,VNUM)
           ELSE
              WTYPE(VNUM)   = ' '
              AMOUNT(VNUM) = WTHADJ(1,VNUM)
           ENDIF
!        ENDIF
      END DO

c     WRITE (WTHSTR,'(8(A,A,F6.1,2X))')
c    &  'DAYL= ',WTYPE(1),AMOUNT(1),'SRAD= ',WTYPE(2),AMOUNT(2),
c    &  'TMAX= ',WTYPE(3),AMOUNT(3),'TMIN= ',WTYPE(4),AMOUNT(4),
c    &  'RAIN= ',WTYPE(5),AMOUNT(5),'CO2 = ',WTYPE(6),AMOUNT(6),
c    &  'DEW = ',WTYPE(7),AMOUNT(7),'WIND= ',WTYPE(8),AMOUNT(8)
      WRITE (WTHSTR,'(8(A,A,F6.2,2X))')
     &  'DAYL= ',WTYPE(1),AMOUNT(1),'SRAD= ',WTYPE(2),AMOUNT(2),
     &  'TMAX= ',WTYPE(3),AMOUNT(3),'TMIN= ',WTYPE(4),AMOUNT(4),
     &  'RAIN= ',WTYPE(5),AMOUNT(5),'CO2 = ',WTYPE(6),AMOUNT(6),
     &  'DEW = ',WTYPE(7),AMOUNT(7),'WIND= ',WTYPE(8),AMOUNT(8)

      END SUBROUTINE WTHSUM

C=======================================================================
C  WTHMDI, Subroutine, N.B. Pickering
C  Initialize weather modification array INTERACTIVELY.  Used to update
C  daily weather record. All variables are adjusted on input.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/04/1991 NBP Written
C  08/10/1993 GH  Modified for new separate I/O
C-----------------------------------------------------------------------
C  Input : OUT0OP,WMODI
C  Output: WMODI,WTHADJ
C  Local : A,AMOUNT,B,CO2,CO2BAS,ERRNUM,I,J,MENU1,MENU2,MORE,
C          SCREEN,REPEAT
C  Notes : WTHADJ is weather modification array (2x8) containing offset
C          and multiplier for each variable:
C          1=DAYL, 2=SRAD, 3=TMAX, 4=TMIN, 5=RAIN, 6=RH, 7=WIND, 8=CO2
C=======================================================================

      SUBROUTINE WTHMDI (RNMODE,WMODI,WTHADJ)

      IMPLICIT  NONE
      EXTERNAL CLEAR, WTHSET_Inp

      CHARACTER*1  RNMODE, WTYPE,WMODI
      INTEGER      ERRNUM,I,J,MENU1,MENU2
      LOGICAL      A,B,MORE,REPEAT
      REAL         AMOUNT,WTHADJ(2,8)

!      PARAMETER   (CO2BAS=330.0)

C     Initialization: CO2 every time for display.

!      CO2    = WTHADJ(1,6) + CO2BAS*WTHADJ(2,6)
!      CO2    = WTHADJ(1,6) + CO2*WTHADJ(2,6)

C     Repeat loop until valid entry or exit code (0).

      REPEAT = .TRUE.
   20 IF (REPEAT) THEN
        ERRNUM = 0
        MENU1  = 0

C       Write menu to choose weather variable.

        IF (INDEX('IE',RNMODE) .GT. 0)  THEN
          CALL CLEAR
          WRITE(*,'(A/A//A/5(A,2F7.2/),A,2F7.2/2(A,2F7.2/))')
     &      ' SELECT/REVISE WEATHER VARIABLES:',
     &      ' ================================',
     &      ' 0)  RETURN                  OFFSET  MULT.  VALUE',
     &      ' 1)  Photoperiod (Daylength)',(WTHADJ(J,1),J=1,2),
     &      ' 2)  Solar Radiation        ',(WTHADJ(J,2),J=1,2),
     &      ' 3)  Maximum Temperature    ',(WTHADJ(J,3),J=1,2),
     &      ' 4)  Minimum Temperature    ',(WTHADJ(J,4),J=1,2),
     &      ' 5)  Rainfall               ',(WTHADJ(J,5),J=1,2),
     &      ' 6)  Carbon Dioxide         ',(WTHADJ(J,6),J=1,2),
     &      ' 7)  Humidity (dew point)   ',(WTHADJ(J,7),J=1,2),
     &      ' 8)  Wind speed             ',(WTHADJ(J,8),J=1,2)

C         Warning for CO2 and radiation adjustments.

!          WRITE(*,'(A,F6.0,A//A/)')
!     &      ' Relative adjustments of CO2 from a base value of ',
!!     &      CO2BAS,' ppm.',
!     &      CO2,' ppm.',
     &
          WRITE(*,'(A/)')
     &      ' PFD and Solar Radiation automatically changed together.'

C         Warning if only one of TMAX and TMIN is modified.

          A = WTHADJ(1,3) .NE. 0. .OR. WTHADJ(2,3) .NE. 1.
          B = WTHADJ(1,4) .NE. 0. .OR. WTHADJ(2,4) .NE. 1.
          IF (.NOT. A .AND. B .OR. A .AND. .NOT. B) WRITE(*,'(A/)')
     &      ' Maximum and Minimum Temperature usually changed together.'

          WRITE(*,'(/A,$)')
     &
     &      ' CHOICE ? < Default = 0 > ===> '
        ENDIF
        READ(5,'(I2)',IOSTAT=ERRNUM) MENU1

C       Write menu to screen to enter offset and multiplier.  This gives
C       a choice of type of weather modification: additive, multiplicative
C       or constant value.

        MORE = MENU1 .GE. 1 .AND. MENU1 .LE. 8 .AND. ERRNUM .EQ. 0

        IF (MORE) THEN
          WTHADJ(1,MENU1) = 0.0
          WTHADJ(2,MENU1) = 1.0
          ERRNUM          = -1

C         Repeat loop until valid choice (0-3).

          REPEAT = .TRUE.
   30     IF (REPEAT) THEN
            ERRNUM = 0
            MENU2  = 0
            IF (INDEX('IE',RNMODE) .GT. 0)  THEN
              CALL CLEAR
              WRITE(*,'(A//A/A/A/A/A//A)')
     &          ' Select modification option, then enter amount:',
     &          ' 0)  NO CHANGE             ( ambient conditions )',
     &          ' 1)  Additive Change       ( 3.0 = 3 higher )',
     &          ' 2)  Subtractive Change    ( 3.0 = 3 lower )',
     &          ' 3)  Multiplicative Change ( 1.2 = 20% higher )',
     &          ' 4)  Constant Value        ( 100 = constant of 100 )',
     &          ' <=== CHOICE? < Default = 0 >'
            ENDIF
            READ (5,'(I2)',IOSTAT=ERRNUM) MENU2

C           Choose amount and update WTHADJ if MENU2 = 1-4.

            MORE = MENU2 .GE. 1 .AND. MENU2 .LE. 4 .AND. ERRNUM .EQ. 0
            IF (MORE) THEN

C           Write to screen to get amount to change weather variable
            IF (INDEX('IE',RNMODE) .GT. 0)  THEN 
			WRITE(*,'(/A)') ' <=== Amount'
	      ENDIF
              READ (5,'(F5.0)',IOSTAT=ERRNUM) AMOUNT

C             Update WTHADJ.

              IF (MENU2 .EQ. 1) THEN
                WTYPE = 'A'
              ELSE IF (MENU2 .EQ. 2) THEN
                WTYPE = 'S'
              ELSE IF (MENU2 .EQ. 3) THEN
                WTYPE = 'M'
              ELSE IF (MENU2 .EQ. 4) THEN
                WTYPE = 'R'
              ENDIF
              CALL WTHSET_Inp(AMOUNT,WTYPE,MENU1,WTHADJ)

            ENDIF
            REPEAT = MENU2 .LT. 0 .OR. MENU2 .GT. 4 .OR. ERRNUM .NE. 0
          GOTO 30
          ENDIF
        ENDIF

C       Adjust CO2 value for display.

!        CO2 = WTHADJ(1,6) + CO2BAS*WTHADJ(2,6)
!        CO2 = WTHADJ(1,6) + CO2*WTHADJ(2,6)

        REPEAT = MENU1 .NE. 0 .OR. ERRNUM .NE. 0

      GOTO 20
      ENDIF

C     Set WMODI if weather modification is requested.

      WMODI = 'N'
      DO I = 1, 8
        IF (WTHADJ(1,I) .NE. 0.0 .OR. WTHADJ(2,I) .NE. 1.0) THEN
           WMODI = 'Y'
        ENDIF
      END DO

      END SUBROUTINE WTHMDI
