!=======================================================================
!  Aloha_NFACTO, Subroutine
!
!  Determines N deficit in Maize
!-----------------------------------------------------------------------
!  Revision history
!
!  1. Written
!  2  Modified by
!  3. Header revision and minor changes             P.W.W.      2-8-93
!=======================================================================

      SUBROUTINE Aloha_NFACTO (DYNAMIC, 
     &    ISTAGE, TANC, XSTAGE,                           !Input
     &    AGEFAC, NDEF3, NFAC, NSTRES, RCNP, TCNP, TMNC)  !Output

      USE ModuleDefs
      IMPLICIT NONE
      SAVE

      INTEGER ISTAGE, DYNAMIC
      REAL AGEFAC, NDEF3, NFAC, NSTRES    !, CNSD1, CNSD2
      REAL RCNP, TANC, TCNP, TMNC, XSTAGE

!=======================================================================
      SELECT CASE (DYNAMIC)
!=======================================================================
      CASE (RUNINIT)
!=======================================================================
      TCNP   = 0.0
      TMNC   = 0.0
      RCNP   = 0.0
      NFAC   = 1.0
      NSTRES = 1.0
      AGEFAC = 1.0
      NDEF3  = 1.0

!=======================================================================
      CASE (SEASINIT)
!=======================================================================

      NDEF3  =  1.0
      NSTRES =  1.0
      AGEFAC =  1.0


!=======================================================================
      CASE (RATE)
!=======================================================================

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

      AGEFAC = 1.0
      NDEF3  = 1.0
      NSTRES = 1.0

      AGEFAC = NFAC
      AGEFAC = AMIN1 (AGEFAC,1.0)

      IF (NFAC .LT. 0.8) THEN
         NDEF3 = 0.2 + NFAC
      ENDIF
      NDEF3  = AMIN1 (NDEF3, 1.0)

      NSTRES = NFAC*1.2 + 0.2
      IF (NFAC .GT. 0.5) THEN
         NSTRES = NFAC*0.4 + 0.6
      ENDIF

!=======================================================================
      END SELECT
!=======================================================================
      RETURN
      END SUBROUTINE Aloha_NFACTO
