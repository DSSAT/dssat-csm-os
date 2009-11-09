C=======================================================================
C  SoilKi, Subroutine, U. Singh and C.H.Porter
C-----------------------------------------------------------------------
C  Soil Potassium Model
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  10/31/2007 US/CHP Written
C=====================================================================

      SUBROUTINE SoilKi(CONTROL, ISWITCH, 
     &    FERTDATA, KUptake, SOILPROP, TILLVALS,          !Input
     &    SKi_Avail)                                      !Output

C-----------------------------------------------------------------------
      USE ModuleDefs
      USE ModSoilMix
      IMPLICIT NONE
      SAVE

      CHARACTER*1  ISWPOT, RNMODE
      CHARACTER*6, PARAMETER :: ERRKEY = 'SOILKi'
      CHARACTER*78 MSG(4)

      INTEGER DYNAMIC, FERTDAY, L, NLAYR, RUN, YRDOY

      REAL SKiAvlProf
      REAL MIXPCT, TDEP   !Tillage mixing percent and depth

      REAL, DIMENSION(NL) :: DLAYR, KG2PPM
      REAL, DIMENSION(NL) :: DLTSKiAvail
      REAL, DIMENSION(NL) :: ADDSKi, KUptake

!     K pools (kg/ha):
      REAL, DIMENSION(NL) :: SKi_Avail
!     ppm
      REAL, DIMENSION(NL) :: Ki_Avail
      REAL, DIMENSION(NL) :: KAvailIndex, KFertIndex, KExchgIndex

!==========================================
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (SoilType)    SOILPROP
      TYPE (FertType)    FERTDATA
      TYPE (TillType)    TILLVALS

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      RNMODE  = CONTROL % RNMODE
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY

      ISWPOT  = ISWITCH % ISWPOT

      DLAYR   = SOILPROP % DLAYR
      KG2PPM  = SOILPROP % KG2PPM
      NLAYR   = SOILPROP % NLAYR

      FERTDAY = FERTDATA % FERTDAY

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC == SEASINIT) THEN
!-----------------------------------------------------------------------
        IF (RUN .EQ. 1 .OR. INDEX('QF',RNMODE) .LE. 0) THEN
          CALL SoilKi_init (ISWPOT, SOILPROP,             !Input 
     &    Ki_Avail, SKi_Avail, SKiAvlProf)                !Output

          DO L = 1, NLAYR
            IF (SOILPROP % OC(L) > 1.E-5) THEN
               KFertIndex(L) = 1.05 + 0.5 * LOG(SOILPROP % OC(L))
            ELSE
               KFertIndex(L) = 0.05   !Value of function at 1.E-6
            ENDIF
            IF (SOILPROP % CEC(L) > 1.E-5) THEN
             KExchgIndex(L) = MIN(1.0, 17.5* SOILPROP % EXK(L) /
     &                          SOILPROP % CEC(L))
            ELSE
               KExchgIndex(L) = 1.00   
            ENDIF

            KAvailIndex(L) = MIN(KFertIndex(L), KExchgIndex(L), 1.0)
            Ki_Avail(L) = Ki_Avail(L) * KAvailIndex(L)
            SKi_Avail(L) = Ki_Avail(L) / KG2PPM(L)
          ENDDO
        ENDIF

        KUptake = 0.0
        DLTSKiAvail = 0.0

        CALL OpSOILKi(CONTROL, ISWITCH, 
     &    FertData, KUptake,      
     &    SOILPROP, Ki_AVAIL, SKiAvlProf)  

!***********************************************************************
!***********************************************************************
!     Rate Calculations 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
      IF (ISWPOT .EQ. 'N') RETURN

!     Set Dlt variables to zero every day
      DLTSKiAvail   = 0.

!-----------------------------------------------------------------------
!     K Uptake
      DO L = 1, NLAYR
        IF (KUptake(L) > 1.E-5) THEN
          IF (SKi_Avail(L) < KUptake(L)) THEN
            WRITE(MSG(1),'(A,I2)') "Negative soil K, layer ", L
            WRITE(MSG(2),'(A,8X,F7.3)')"Avail K (kg/ha)",SKi_Avail(L)
            WRITE(MSG(3),'(A,F7.3)')"Computed uptake (kg/ha)",KUptake(L)
            WRITE(MSG(4),'(A)') "Soil K set to zero"
            CALL WARNING(4,ERRKEY,MSG)
            SKi_Avail(L) = 0.0
          ELSE 
            SKi_Avail(L) =  SKi_Avail(L) - KUptake(L)
          ENDIF
        ENDIF
      ENDDO

!-----------------------------------------------------------------------
!     Fertilizer added today:
      IF (YRDOY == FERTDAY) THEN
        ADDSKi  = FERTDATA % ADDSKi
        DO L = 1, NLAYR
          IF (ADDSKi(L) > 1.E-6) THEN
!           Add fertilizer to available pool
            DLTSKiAvail(L) = DLTSKiAvail(L) + ADDSKi(L) 
          ENDIF
        ENDDO
      ENDIF

!     ------------------------------------------------------------------
!     Check if tillage occurred today.  Mix soil K within tillage depth.  
      IF (TILLVALS % NTIL .GT. 0) THEN
        IF (YRDOY .EQ. TILLVALS % TILDATE) THEN
          MIXPCT = TILLVALS % TILMIX
          TDEP = TILLVALS % TILDEP
          CALL SoilMix(SKi_Avail,DLTSKiAvail,1,DLAYR,MIXPCT,NLAYR,TDEP)
        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     Daily integration
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
C-----------------------------------------------------------------------
      IF (ISWPOT .EQ. 'N') RETURN

      SKiAvlProf = 0.

      DO L = 1, NLAYR
!       Update soil K content
        SKi_Avail(L) = SKi_Avail(L) + DLTSKiAvail(L)
        Ki_Avail(L) = SKi_Avail(L) * KG2PPM(L) 
        SKiAvlProf = SKiAvlProf + SKi_Avail(L)
      ENDDO

      DLTSKiAvail = 0.

!***********************************************************************
!***********************************************************************
!     OUTPUT
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
      IF (ISWPOT .EQ. 'N') RETURN

      CALL OpSOILKi(CONTROL, ISWITCH, 
     &    FertData, KUptake,      
     &    SOILPROP, Ki_AVAIL, SKiAvlProf)  

C***********************************************************************
C***********************************************************************
C     END OF DYNAMIC IF CONSTRUCT
C***********************************************************************
      ENDIF
C***********************************************************************

      RETURN
      END SUBROUTINE SoilKi

C***********************************************************************
