C=======================================================================
C  TR_TRNSPL_PHENOL, Subroutine
C
C  Allows for transplanting shock and estimates new values for
C  P1, BIOMAS, RTWT and PLA
C-----------------------------------------------------------------------
C  Revision history
C
C  08/07/1993 PWW Header revision and minor changes
C  08/29/2002 CHP/MUS Converted to modular format for inclusion in CSM.
C=======================================================================

      SUBROUTINE TR_TRNSPL_PHENOL ( 
     &    ATEMP, ITRANS, P1T, SDEPTH, TAGE, TBASE,        !Input
     &    CDTT_TP, CUMDTT, ISTAGE, P1, P8, P9, SDTT_TP,   !Output
     &    SUMDTT, XSTAGE, XST_TP)                         !Output

!-----------------------------------------------------------------------
      IMPLICIT  NONE
      SAVE

      INTEGER ISTAGE, ITRANS    !, TAGEFAC

      REAL ATEMP, CDTT_TP, CUMDTT
      REAL P1, P1T, P8, P9
      REAL SDEPTH, SDTT_TP, SUMDTT
      REAL TAGE, TBASE, TPHEN, XSTAGE, XST_TP

C-----------------------------------------------------------------------
      IF (ITRANS .EQ. 1 .OR. ITRANS .EQ. 4) RETURN

      !TAGEFAC = 0
      !
      ! Cummulative thermal time
      !
      IF (ITRANS .EQ. 3) THEN

         ! Estimate CUMDTT from mean temp and age. 
         ! Resets CUMDTT to 0 at ISOW
         CUMDTT  = (ATEMP-TBASE)*(TAGE-1.0)
         !
         ! Estimate cumdtt from mean temp and age. 
         ! Resets cumddt to 0 at Isow
         !
         !
         SUMDTT = CUMDTT - P8 - P9
         !
         ! Initialize plant n
         !
         XSTAGE = SUMDTT / P1T
         ISTAGE = 1

         IF (SDEPTH .LE. 0.0) THEN
            SDEPTH = 10.0
         ENDIF

      ENDIF
      ! Applies to Itrans=2,3 i.e. transplanted whether start simulation
      ! at transplanting (=3) or start simulation at seeding (=2)
      ! Calculate transplant effect on duration (even when simulation 
      ! begins at seeding for transplanted exp.)
      !
      !PLANTS = TPLANTS
      TPHEN  = P1T + 25.0 + 0.4*SUMDTT
      TPHEN  = AMIN1 (TPHEN,(P1T+200.0))
      P1     = TPHEN
      !
      ! Min. delay is 20 C
      !
      !
 
      !Save value of CUMDTT and SUMDTT for use by TRANSPL_GROSUB
      CDTT_TP = CUMDTT
      SDTT_TP = SUMDTT
      XST_TP  = XSTAGE

      RETURN
      END SUBROUTINE TR_TRNSPL_PHENOL
