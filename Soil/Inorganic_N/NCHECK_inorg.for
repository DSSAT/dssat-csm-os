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
     &    NH4_2D, NO3_2D, SNH4_2D, SNO3_2D, UREA_2D) !Input  

!-----------------------------------------------------------------------
      USE Cells_2D
!     USE ModuleDefs !already USED by Cells_2D
      IMPLICIT NONE 
      EXTERNAL WARNING
!-----------------------------------------------------------------------
      TYPE (ControlType), INTENT(IN) :: CONTROL
      REAL, DIMENSION(MaxRows, MaxCols), INTENT(INOUT) ::
     &     NH4_2D, NO3_2D, SNH4_2D,SNO3_2D, UREA_2D

      CHARACTER*78 MSG(10)
      INTEGER L, J, YRDOY
      REAL, PARAMETER :: TOL = -1.E-6

      IF (CONTROL % DYNAMIC == SEASINIT) THEN
        WRITE(MSG(3),"('Value will be set to zero')")
      ENDIF

!-----------------------------------------------------------------------
      YRDOY   = CONTROL % YRDOY

!     Check for negative soil N values
      DO L = 1, NRowsTot
        DO J = 1, NColsTot
          IF (Sim2D) THEN
            WRITE(MSG(1),100) YRDOY, L, J
  100       FORMAT('Negative soil N value on day ',I7,' in cell ',I3,I3)
          ELSE
            WRITE(MSG(1),200) YRDOY, L
  200       FORMAT('Negative soil N value on day ',I7,' in layer ',I3)
          ENDIF

          IF (SNO3_2D(L, J).LT. 0.0) THEN
            SNO3_2D(L,J) = 0.0
            NO3_2D(L,J) = 0.0
            IF (SNO3_2D(L,J) .LT. TOL) THEN
              WRITE(MSG(2),"('Nitrate =',F10.3,'kg[N]/ha')")
     &                    SNO3_2D(L, J)
              CALL WARNING(3, "NCHECK", MSG)
            ENDIF
          ENDIF

          IF (SNH4_2D(L, J).LT. 0.0) THEN
            SNH4_2D(L,J) = 0.0
            NH4_2D(L,J) = 0.0
            IF (SNH4_2D(L,J) .LT. TOL) THEN
              WRITE(MSG(2),"('Ammonium =',F10.3,'kg[N]/ha')")
     &                   SNH4_2D(L, J)
              CALL WARNING(3, "NCHECK", MSG)
            ENDIF
          ENDIF

          IF (UREA_2D(L, J).LT. 0.0) THEN
            UREA_2D(L,J) = 0.0
            IF (UREA_2D(L,J) .LT. TOL) THEN
              WRITE(MSG(2),"('Urea =',F10.3,'kg[N]/ha')") UREA_2D(L, J)
              CALL WARNING(3, "NCHECK", MSG)
            ENDIF
          ENDIF
        ENDDO
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
! SNH4(L)    1D Total extractable ammonium N in soil layer L (kg [N] / ha)
! SNO3(L)    1D Total extractable nitrate N in soil layer L (kg [N] / ha)
! UREA(L)    1D Amount of urea in soil layer L (kg [N] / ha)
! SNH4_2D(L,J) 2D Total extractable ammonium N in soil cell L,J (kg [N] / ha)
! SNO3_2D(L,J) 2D Total extractable nitrate N in soil cell L,J (kg [N] / ha)
! UREA_2D(L,J) 2D Amount of urea in soil cell L,J (kg [N] / ha)
! VALUE      Value of variable written to warning file 
! YRDOY      Current day of simulation (YYDDD)
!==========================================================================
