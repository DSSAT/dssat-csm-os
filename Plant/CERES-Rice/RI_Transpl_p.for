C=======================================================================
C  TRNSPL_PHENOL, Subroutine
C
C  Allows for transplanting shock and estimates new values for
C  P1, BIOMAS, RTWT and PLA
C-----------------------------------------------------------------------
C  Revision history
C
C  08/07/1993 PWW Header revision and minor changes
C  08/29/2002 CHP/MUS Converted to modular format for inclusion in CSM.
C=======================================================================

      SUBROUTINE TRNSPL_PHENOL ( 
     &    ATEMP, ITRANS, P1T, SDEPTH, TAGE, TBASE,        !Input
     &    CDTT_TP, CUMDTT, ISTAGE, P1, P8, P9, SDTT_TP,   !Output
     &    SUMDTT, XSTAGE, XST_TP)                         !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT  NONE
      EXTERNAL WARNING
      SAVE

      CHARACTER*78 MSG(10)
      INTEGER ISTAGE, ITRANS, TAGEFAC

      REAL ATEMP, CDTT_TP, CUMDTT
      REAL P_AGE, P1, P1T, P8, P9
      REAL SDEPTH, SDTT_TP, SUMDTT
      REAL TAGE, TBASE, TPHEN, XSTAGE, XST_TP

C-----------------------------------------------------------------------
      IF (ITRANS .EQ. 1 .OR. ITRANS .EQ. 4) RETURN

      TAGEFAC = 0
      !
      ! Cummulative thermal time
      !
      IF (ITRANS .EQ. 3) THEN

         ! Estimate CUMDTT from mean temp and age. 
         ! Resets CUMDTT to 0 at ISOW
         CUMDTT  = (ATEMP-TBASE)*(TAGE-1.0)

         ! Estimate phenological age of transplant
         P_AGE = TAGE*(ATEMP-TBASE)
!         IF (P_AGE .LT. 240.0 .OR. TAGE .LE. 14.0) THEN  
         IF (P_AGE .LT. 260.0 .OR. TAGE .LE. 14.0) THEN   !US
            MSG(1) = 
     &        'Transplanted using seedling tray or daPog seedlings.'
            CALL WARNING(1, "RIPHEN", MSG)
            TAGEFAC = 1
         ENDIF

         P8 = 150.0*EXP(-0.055*ATEMP)
         P8 = AMIN1 (P8,80.0)
         P8 = AMAX1 (P8,28.0)

         P9     = 10.0*SDEPTH + 20.0
         SUMDTT = CUMDTT - P8 - P9

         ! Initialize Plant N
         XSTAGE = SUMDTT/P1T
         ISTAGE = 1
      ENDIF

! Applies to ITRANS = 2,3 i.e. transplanted whether start simulation
! at transplanting (=3) or start simulation at seeding (=2)
! Calculate transPlant effect on duration (even when simulation begins
! at seeding for transplanted exp.)  ... Plant pop. in the field
!
!      PLANTS = TPLANTS*NPPH  !already calculated in PHENOL
      TPHEN  = P1T + 25.0 + 0.4*SUMDTT
      TPHEN  = AMIN1 (TPHEN,(1.75*P1T))
      P1     = TPHEN
      IF (TAGEFAC .EQ. 1) THEN
!         P1 = P1T + 25.0
         P1 = P1T + 25.0 * (P_AGE/185.)   !US
      ENDIF

      !Save value of CUMDTT and SUMDTT for use by TRANSPL_GROSUB
      CDTT_TP = CUMDTT
      SDTT_TP = SUMDTT
      XST_TP  = XSTAGE

      RETURN
      END SUBROUTINE TRNSPL_PHENOL
