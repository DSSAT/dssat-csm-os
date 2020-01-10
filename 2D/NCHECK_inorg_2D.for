C=======================================================================
C  NCHECK_inorg_2D, Subroutine
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
      SUBROUTINE NCHECK_inorg_2D(CONTROL, 
     &    NH4_2D, NO3_2D, SNH4_2D, SNO3_2D, UREA_2D) !Input  
    !&    NLAYR, NH4, NO3, SNH4, SNO3, UREA)              !Input

!-----------------------------------------------------------------------
      USE Cells_2D
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE 
!-----------------------------------------------------------------------
      CHARACTER*78  MSG(10)
      INTEGER L, J, YRDOY !, NLAYR
      !INTEGER L, NLAYR, YRDOY
      REAL, DIMENSION(MaxRows, MaxCols) :: NH4_2D, NO3_2D, SNH4_2D, 
     &   SNO3_2D, UREA_2D
    ! REAL, DIMENSION(NL) :: NH4, NO3, SNH4, SNO3, UREA
      TYPE (ControlType) CONTROL
      YRDOY   = CONTROL % YRDOY

!-----------------------------------------------------------------------
      WRITE(MSG(3),"('Value will be set to zero')")

!     Check for negative soil N values
      DO L = 1, NRowsTot
        DO J = 1, NColsTot
          WRITE(MSG(1),100) YRDOY, L, J
  100     FORMAT('Negative soil N value on day ',I7,' in cell ',I3, I3)

          IF (SNO3_2D(L, J).LT. -0.1) THEN
            WRITE(MSG(2),"('Nitrate (SNO3_2D) =',F10.3,'kg[N]/ha')")
     &                    SNO3_2D(L, J)
            CALL WARNING(3, "NCHECK", MSG)
            SNO3_2D(L, J) = 0.0
            NO3_2D(L, J) = 0.0
          ENDIF

          IF (SNH4_2D(L, J).LT. -0.1) THEN
            WRITE(MSG(2),"('Ammonium (SNH4_2D) =',F10.3,'kg[N]/ha')")
     &                   SNH4_2D(L, J)
            CALL WARNING(3, "NCHECK", MSG)
            SNH4_2D(L, J) = 0.0
            NH4_2D(L, J) = 0.0
          ENDIF

          IF (UREA_2D(L, J).LT. -0.1) THEN
            WRITE(MSG(2),"('UREA_2D =',F10.3,'kg[N]/ha')") UREA_2D(L, J)
            CALL WARNING(3, "NCHECK", MSG)
            UREA_2D(L, J) = 0.0
          ENDIF
        ENDDO
      ENDDO

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE NCHECK_inorg_2D

! NCHECK_2D, NWRITE Variable list
!==========================================================================
! CODE       Code for negative soil nitrogen values to be written to 
!              warning file 
! LUNWARN    Logical unit number for Warning.OUT file 
! NL         Maximum number of soil layers = 20 
! NLAYR      Actual number of soil layers 
! SNH4_2D(L,J) Total extractable ammonium N in soil layer L (kg [N] / ha)
! SNO3_2D(L,J) Total extractable nitrate N in soil layer L (kg [N] / ha)
! UREA_2D(L)    Amount of urea in soil layer L (kg [N] / ha)
! VALUE      Value of variable written to warning file 
! YRDOY      Current day of simulation (YYDDD)
!==========================================================================
