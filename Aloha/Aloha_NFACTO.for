C=======================================================================
C  Aloha_NFACTO, Subroutine
C
C  Determines N deficit in Maize
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      2-8-93
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  :
C
C  OUTPUT : None
C-----------------------------------------------------------------------
C  Called : GROSUB
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  None   :
C=======================================================================

      SUBROUTINE Aloha_NFACTO (DYNAMIC, ISTAGE,

      IMPLICIT NONE
      SAVE

      INTEGER ISTAGE
      REAL AGEFAC, CNSD1, CNSD2, NDEF3, NFAC, NSTRES
      REAL RCNP, TANC, TCNP, TMNC, XSTAGE

      TCNP = EXP (1.52-0.160*XSTAGE)/100.0
      TMNC = 0.0045
      IF (XSTAGE .LT. 4.0) THEN
         TMNC =  (1.25-0.200*XSTAGE)/100.0
      ENDIF
      RCNP   = 1.06/100.0
      NFAC   = 1.0 - (TCNP-TANC)/(TCNP-TMNC)
      NFAC   = AMIN1 (NFAC,1.0)

      IF (ISTAGE .EQ. 3 .OR. ISTAGE .EQ. 4) THEN
          NFAC = 1.0 - 1.80*EXP(-3.5*NFAC)
      ENDIF

      NFAC   = AMAX1 (NFAC,0.001)
      NSTRES = 1.0
      AGEFAC = 1.0
      NDEF3  = 1.0
      AGEFAC = NFAC
      IF (NFAC .LT. 0.8) THEN
         NDEF3 = 0.2 + NFAC
      ENDIF
      NSTRES = NFAC*1.2 + 0.2
      IF (NFAC .GT. 0.5) THEN
         NSTRES = NFAC*0.4 + 0.6
      ENDIF
      AGEFAC = AMIN1 (AGEFAC,1.0)
      NDEF3  = AMIN1 (NDEF3, 1.0)
      CNSD1  = CNSD1 + 1.0 - NSTRES
      CNSD2  = CNSD2 + 1.0 - AGEFAC

      RETURN

      END SUBROUTINE Aloha_NFACTO
