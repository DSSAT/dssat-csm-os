C=======================================================================
C  FOR_LINDM, Subroutine
C----------------------------------------------------------------------
C  This subroutine linearly interpolates the between pest observations
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/90 WDB
C  02/25/98 CHP Modified for PEST Module
C  01/12/99 GH  Incorporated into CROPGRO
C-----------------------------------------------------------------------
C  Called by: PEST
C  Calls:     None
C=======================================================================
      SUBROUTINE FOR_LINDM(
     &    DAP, IDAP, PCN, YPL,                            !Input
     &    PL)                                             !Output
C-----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE

      INTEGER DAP, I, PCN
      INTEGER ROW(6)
      INTEGER IDAP(6,200)

      REAL RISE, RUN, SLOPE
      REAL PL(6)
      REAL YPL(6,200)
C-----------------------------------------------------------------------
C     Initialize variables
C-----------------------------------------------------------------------
      IF (DAP .LE. 1) THEN
        DO I = 1, 6
        PL(I)  = 0
        ROW(I) = 1
        ENDDO
      ENDIF
C-----------------------------------------------------------------------
C     Linearly interpolate pest levels daily
C-----------------------------------------------------------------------
      DO I = 1, PCN
C-----------------------------------------------------------------------
C     Increment Row Counter
C-----------------------------------------------------------------------
450     CONTINUE
        IF (IDAP(I,ROW(I)) .LT. 0) THEN
        ROW(I) = ROW(I) + 1
        GOTO 450
        ENDIF

        IF (DAP .GT. IDAP(I,ROW(I)) .AND. IDAP(I,ROW(I)) .GT. 0)
     &  ROW(I) = ROW(I) + 1
C-----------------------------------------------------------------------
C    Set beginning of interpolation to zero
C-----------------------------------------------------------------------
        IF (DAP .LT. IDAP(I,1) ) THEN
        PL(I) = 0
C-----------------------------------------------------------------------
C     Set First Interpolated Value if idap(I,1) = 1
C-----------------------------------------------------------------------
        ELSEIF (DAP .EQ. IDAP(I,1)) THEN
        PL(I) = YPL(I,ROW(I))
        ROW(I) = ROW(I) + 1
C-----------------------------------------------------------------------
C   Set first interpolated value if idap < 0, that is
C   there is some pest data before planting date
C-----------------------------------------------------------------------
        ELSEIF (ROW(I) .GT. 1 .AND. IDAP(I,ROW(I)-1) .LT. 0
     &    .AND. DAP .LE. 1) THEN
        RISE = YPL(I,ROW(I)) - YPL(I,ROW(I)-1)
        RUN = IDAP(I,ROW(I)) - IDAP(I,ROW(I)-1)
        SLOPE = RISE/RUN
        PL(I) = YPL(I,ROW(I)-1) + SLOPE * (DAP - IDAP(I,ROW(I)-1))
C-----------------------------------------------------------------------
C Linear Interpolation
C-----------------------------------------------------------------------
        ELSEIF(DAP .LE. IDAP(I,ROW(I))) THEN
        RISE = YPL(I,ROW(I)) - YPL(I,ROW(I)-1)
        RUN = IDAP(I,ROW(I)) - IDAP(I,ROW(I)-1)
        SLOPE = RISE/RUN
        PL(I) = PL(I) + SLOPE * 1
C-----------------------------------------------------------------------
C Set all damage to zero after last user entered
c value for damage
C-----------------------------------------------------------------------
!        ELSEIF (DAP .GT. IDAP(I,ROW(I))) THEN
!          PL(I) = 0
        ENDIF

        IF (DAP .GT. IDAP(I,ROW(I))) THEN
        PL(I) = 0
        ENDIF
      ENDDO

      RETURN
      END   ! SUBROUTINE FOR_LINDM

!-----------------------------------------------------------------------
!     Variables definitions
!-----------------------------------------------------------------------
! DAP       Number of days after planting (d)
! IDAP(I,J) Day of pest damage for pest I, observation J
!             (days after planting)
! PCN       Number of pests in FILET 
! PL(I)     Pest level from pest progress file (FILET) for pest I (varies)
! RISE      Change in pest damage level between two observations for linear 
!             interpolation 
! ROW(I)    Counter which points to current row of data for each pest 
!             progress curve (I) 
! RUN       Change in date between two observations for linear 
!             interpolation 
! SLOPE     Slope of pest level curve (RISE/RUN) between two observations 
! YPL(I,J)  Array for storage of pest data for pest or damage type I, 
!             observation J 
!-----------------------------------------------------------------------
!     End Subroutine FOR_LINDM
!-----------------------------------------------------------------------
