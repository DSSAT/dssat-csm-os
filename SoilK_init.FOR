!=======================================================================
!  SoilKi_init 
!
!  Initalize inorganic soil K variables using extractable K from
!     soil file (EXK).
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  10/31/2007 CHP Written 
!-----------------------------------------------------------------------
!  Called by: SoilKi
!=======================================================================

      SUBROUTINE SoilKi_init (ISWPOT, SOILPROP,           !Input 
     &    Ki_Avail, SKi_Avail, SKiAvlProf)                !Output

!     ------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT  NONE
      SAVE
!     ------------------------------------------------------------------
      CHARACTER*1  ISWPOT
      CHARACTER*6, PARAMETER :: ERRKEY = 'SKINIT'
      CHARACTER*78 MSG(3)

      INTEGER L
      INTEGER NLAYR    

!     Soil properties:
      REAL SKiAvlProf
      REAL, DIMENSION(NL) :: Ki_Avail, SKi_Avail, KG2PPM
      TYPE (SoilType)    SOILPROP

      IF (ISWPOT .NE. 'Y') RETURN

      KG2PPM   = SOILPROP % KG2PPM
      Ki_Avail = SOILPROP % EXK * 391.
      NLAYR    = SOILPROP % NLAYR

      SKiAvlProf = 0.
      DO L = 1, NLAYR
        SKi_Avail(L) = Ki_Avail(L) / KG2PPM(L) 
        SKiAvlProf = SKiAvlProf + SKi_Avail(L)
      ENDDO

      DO L = 1, NLAYR
        IF (Ki_Avail(L) < 0.0) THEN
          MSG(1) = "Negative value found in soil file for potassium."
          MSG(2) = "Cannot run potassium model for this data."
          MSG(3) = "Model will stop."
          CALL WARNING(3,ERRKEY,MSG)
          CALL ERROR(ERRKEY,0," ",0)
        ENDIF
      ENDDO

      RETURN
      END SUBROUTINE SoilKi_init

!=======================================================================
