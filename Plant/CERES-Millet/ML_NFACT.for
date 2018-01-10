C=======================================================================
C  ML_NFACT, Subroutine
C
C  Determines N deficiency factor for Pearl Millet
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      2-8-93
C  4. Modified                                      W.T.B.     June 94
C  5. Converted to modular format                   W.D.B.     7-31-02
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : ML_GROSUB
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C         :
C=======================================================================

      SUBROUTINE ML_NFACT (
     & AGEFAC, CNSD1, CNSD2, ISTAGE, NDEF3, NFAC,
     & NSTRES, RANC, RCNP, TANC, TCNP, TMNC, XSTAGE)
  
      IMPLICIT  NONE
      SAVE

      REAL AGEFAC
      REAL CNSD1
      REAL CNSD2
      INTEGER ISTAGE
      REAL NDEF3
      REAL NFAC
      REAL NSTRES
      REAL RANC
      REAL RCNP
      REAL TANC
      REAL TCNP
      REAL TMNC
      REAL XSTAGE


      TCNP = 8.276 - 1.485*XSTAGE+0.0711*XSTAGE*XSTAGE
      TCNP = AMIN1 (TCNP,5.0)
      !
      ! TMNC as calculated in Version 2.1 ... WTB 8-1-94
      !
      TMNC = 1.2 - 0.05 * XSTAGE
      IF (XSTAGE .LT. 5.0) THEN
         TMNC = 2.0 - 0.28 * XSTAGE
      ENDIF

      IF (XSTAGE .GE. 8.0) THEN
         TMNC = TCNP * 0.65
      ENDIF
      TMNC = TMNC*0.01
      TCNP = TCNP*0.01
      RCNP = 2.5 - 0.125*XSTAGE
      RCNP = RCNP*0.01
      IF (XSTAGE .LT. 0.2) THEN
         TANC = TCNP
         RANC = RCNP
      ENDIF
      NFAC = 1.0 - (TCNP-TANC)/(TCNP-TMNC)
      NFAC = AMIN1 (NFAC,1.0)
      IF (ISTAGE .EQ. 3 .OR. ISTAGE .EQ. 4) THEN
         NFAC = 1.0 - 2.0*EXP(-3.5*NFAC)
      ENDIF
      NFAC   = AMIN1 (NFAC,1.0)
      NFAC   = AMAX1 (NFAC,0.001)
      NSTRES = 1.0
      AGEFAC = 1.0
      NDEF3  = 1.0
      AGEFAC = NFAC
      NSTRES = NFAC * 1.2 + 0.2
      IF (NFAC .LT. 0.3) THEN
         NDEF3 = 2.33*NFAC + 0.3
      ENDIF
      NSTRES = AMIN1 (NSTRES,1.0)
      AGEFAC = AMIN1 (AGEFAC,1.0)
      NDEF3  = AMIN1 (NDEF3, 1.0)
      CNSD1  = CNSD1 + 1.0 - NSTRES
      CNSD2  = CNSD2 + 1.0 - AGEFAC

      RETURN
      END SUBROUTINE ML_NFACT
