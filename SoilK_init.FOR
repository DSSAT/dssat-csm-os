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
     &    KFertIndex, Ki_Avail, SKi_Avail, SKiAvlProf)    !Output

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
      REAL, DIMENSION(NL) :: Ki_Avail, Ki_Tot, SKi_Avail, KG2PPM
      REAL, DIMENSION(NL) :: KFertIndex, KExchgIndex, KAvailIndex
      TYPE (SoilType)    SOILPROP

      IF (ISWPOT .NE. 'Y') RETURN

      KG2PPM   = SOILPROP % KG2PPM
      NLAYR    = SOILPROP % NLAYR

      SKiAvlProf = 0.
      DO L = 1, NLAYR

        IF (SOILPROP % EXK(L) < 0.0) THEN
          MSG(1) = "Negative value found in soil file for potassium."
          MSG(2) = "Cannot run potassium model for this data."
          MSG(3) = "Model will stop."
          CALL WARNING(3,ERRKEY,MSG)
          CALL ERROR(ERRKEY,0," ",0)
        ENDIF

        Ki_Tot(L) = SOILPROP % EXK(L) * 391.

        IF (SOILPROP % OC(L) > 1.E-5) THEN
           KFertIndex(L) = 1.05 + 0.5 * LOG(SOILPROP % OC(L))
        ELSE
           KFertIndex(L) = 0.05   !Value of function at 1.E-6
        ENDIF

        IF (SOILPROP % CEC(L) > 1.E-5) THEN
         KExchgIndex(L) = MIN(1.0, 17.5* SOILPROP % EXK(L) /
     &                      SOILPROP % CEC(L))
        ELSE
          KExchgIndex(L) = 1.00   
        ENDIF

        KAvailIndex(L) = MIN(KFertIndex(L), KExchgIndex(L), 1.0)

        Ki_Avail(L) = Ki_Tot(L) * KAvailIndex(L)
        SKi_Avail(L) = Ki_Avail(L) / KG2PPM(L)
        SKiAvlProf = SKiAvlProf + SKi_Avail(L)
      ENDDO

      RETURN
      END SUBROUTINE SoilKi_init

!=======================================================================
