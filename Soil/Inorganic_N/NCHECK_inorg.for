C=======================================================================
C  NCHECK_inorg, Subroutine
C
C  Checks for negative values of soil N and prints report if found.
C-----------------------------------------------------------------------
C  Revision history
C  12/22/1999 CHP written
C  03/16/2000 GH  Incorporated in CROPGRO
C               Note: File names etc. should be dynamically created 
C               Check time stamp
!  02/25/2005 CHP Split NCHECK into organic and inorganic.
C-----------------------------------------------------------------------
      SUBROUTINE NCHECK_inorg(CONTROL, 
     &    NLAYR, NH4, NO3, SNH4, SNO3, UREA)              !Input

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL WARNING
!-----------------------------------------------------------------------
      CHARACTER*78  MSG(10)
      INTEGER L, NLAYR, YRDOY
      REAL, DIMENSION(NL) :: NH4, NO3, SNH4, SNO3, UREA
      REAL, PARAMETER :: TOL = -1.E-6

      TYPE (ControlType) CONTROL
      YRDOY   = CONTROL % YRDOY

!-----------------------------------------------------------------------
      WRITE(MSG(3),"('Value will be set to zero')")

!     Check for negative soil N values
      DO L = 1, NLAYR

        WRITE(MSG(1),100) YRDOY, L
  100   FORMAT('Negative soil N value on day ',I7,' in layer ',I3)

        IF (SNO3(L).LT. 0.0) THEN
          SNO3(L) = 0.0
          NO3(L) = 0.0
          IF (SNO3(L) .LT. TOL) THEN
            WRITE(MSG(2),"('Nitrate (SNO3) =',F10.3,'kg[N]/ha')")SNO3(L)
            CALL WARNING(3, "NCHECK", MSG)
          ENDIF
        ENDIF

        IF (SNH4(L).LT. 0.0) THEN
          SNH4(L) = 0.0
          NH4(L) = 0.0
          IF (SNH4(L) .LT. TOL) THEN
           WRITE(MSG(2),"('Ammonium (SNH4) =',F10.3,'kg[N]/ha')")SNH4(L)
           CALL WARNING(3, "NCHECK", MSG)
          ENDIF
        ENDIF

        IF (UREA(L).LT. 0.0) THEN
          UREA(L) = 0.0
          IF (UREA(L) .LT. TOL) THEN
            WRITE(MSG(2),"('Urea =',F10.3,'kg[N]/ha')") UREA(L)
            CALL WARNING(3, "NCHECK", MSG)
          ENDIF
        ENDIF
      ENDDO

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE NCHECK_inorg

! NCHECK, NWRITE Variable list
!==========================================================================
! CODE       Code for negative soil nitrogen values to be written to 
!              warning file 
! LUNWARN    Logical unit number for Warning.OUT file 
! NL         Maximum number of soil layers = 20 
! NLAYR      Actual number of soil layers 
! SNH4(L)    Total extractable ammonium N in soil layer L (kg [N] / ha)
! SNO3(L)    Total extractable nitrate N in soil layer L (kg [N] / ha)
! UREA(L)    Amount of urea in soil layer L (kg [N] / ha)
! VALUE      Value of variable written to warning file 
! YRDOY      Current day of simulation (YYDDD)
!==========================================================================
