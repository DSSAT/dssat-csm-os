C=======================================================================
C  PT_NFACTO, Subroutine
C
C  Determines N deficit in Potato
C-----------------------------------------------------------------------
C  Revision history
C
C                 Written
C  02/08/1993 PWW Header revision and minor changes
C  08/24/2001 CHP Modified for modular format
C-----------------------------------------------------------------------
C  Called : GROSUB
C
C  Calls  : None
C=======================================================================

      SUBROUTINE PT_NFACTO (
     &    ISTAGE, TANC, XSTAGE,                           !Input
     &    AGEFAC, CNSD1, CNSD2, NFAC, NSTRES,             !Output
     &    RCNP, TCNP, TMNC)                               !Output

!----------------------------------------------------------------------
      IMPLICIT  NONE
      SAVE

      REAL AGEFAC, CNSD1, CNSD2, NFAC, NSTRES
      REAL RCNP, TANC, TCNP, TMNC, XSTAGE

      INTEGER  ISTAGE
!----------------------------------------------------------------------
      !
      ! Calculation of critical and minimum N concentrations
      !
      IF (ISTAGE .EQ. 1) THEN                    !Calculate TCNP, TMNC
         TCNP = (4.5 - 0.5*(XSTAGE - 1.0))/100.0 !ISTAGE=1 (vegetative)
         TMNC = TCNP - 0.02
       ELSE
         TCNP = (4.0 - 1.0*(XSTAGE - 2.0))/100.0 !TCNP, TMNC when tubers
         TMNC = TCNP - 0.02                      !are present (ISTAGE=2)
      END IF

      RCNP = (2.15 - 0.5*(XSTAGE - 1.0))/100.0   !Root critical N (RCNP)
      !
      ! Calculation of NFAC, basis for N deficit factors
      !
      IF (TANC .GE. 1.1*TCNP) THEN               !NFAC: actual N conc.
          NFAC = TANC/TCNP                       !relative to critical 
       ELSE                                      !and minimum conc.
          NFAC = AMIN1 (1.0 - (TCNP-TANC)/(TCNP-TMNC),1.0)
      END IF
      !
      ! N deficit factors, NSTRES and AGEFAC
      !
      NSTRES = AMIN1 (NFAC, 1.0)              !NSTRES > photosynthesis
      IF (XSTAGE .LT. 1.1) THEN
         NSTRES = 1.0
      ENDIF
      NSTRES = AMAX1 (NSTRES,0.1)
      AGEFAC = AMIN1 (0.95*NFAC, 1.0)         !AGEFAC > expansion growth
      IF (XSTAGE .LT. 1.1) THEN
         AGEFAC = 1.0
      ENDIF
      AGEFAC = AMAX1 (AGEFAC,0.1)

      CNSD1 = CNSD1 + 1.0 - NSTRES            !Cumulative N stresses
      CNSD2 = CNSD2 + 1.0 - AGEFAC

      RETURN
      END SUBROUTINE PT_NFACTO
