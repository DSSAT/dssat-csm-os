C=======================================================================
C  SG_NFACT, Subroutine
C
C  Determines N deficiency factor for sorghum
C-----------------------------------------------------------------------
C  Revision history
C  xx/xx/19xx     Written
C  02/08/1993 PWW Header revision and minor changes              
C  03/29/2001 WDB Converted to modular form                      
C  12/01/2001 WDB Further modular conversion  
C  04/31/2007 GH  Added dynamic components
C  03/27/2014 GH  Corrected cumulative N Stress factors

C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SGROSUB
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C         :
C=======================================================================

      SUBROUTINE SG_NFACT (DYNAMIC,
     & AGEFAC, ISTAGE, NDEF3, NFAC, !CNSD1, CNSD2, 
     & NSTRES, RANC, RCNP, TANC, TCNP, TMNC, XSTAGE)

      USE ModuleDefs 
      IMPLICIT  NONE
      SAVE

      REAL        AGEFAC
!      REAL        CNSD1
!      REAL        CNSD2
      INTEGER     ISTAGE
      REAL        NDEF3
      REAL        NFAC
      REAL        NSTRES
      REAL        RANC
      REAL        RCNP
      REAL        TANC
      REAL        TCNP
      REAL        TMNC
      REAL        XSTAGE
      INTEGER     DYNAMIC     
C     ----------------------------------------------------------------

      IF(DYNAMIC.EQ.RUNINIT) THEN
          NSTRES = 1.0    
          AGEFAC = 1.0    
          NDEF3 = 1.0     
          NFAC = 1.0      

      ELSEIF(DYNAMIC.EQ.SEASINIT) THEN
          NSTRES = 1.0
          AGEFAC = 1.0
          NDEF3 = 1.0     
          NFAC = 1.0  

      ELSEIF(DYNAMIC.EQ.INTEGR) THEN

         TCNP = 5.05 - 0.62*XSTAGE
         IF (XSTAGE .GE. 4.0) THEN
           TCNP = 5.05 - 0.7*XSTAGE
         ENDIF
         IF (XSTAGE .GE. 6.0) THEN
           TCNP = 1.78 - 0.158*XSTAGE
         ENDIF
         TMNC = 1.62-0.22*XSTAGE
         IF (XSTAGE .GE. 4.0) THEN
           TMNC = 0.8 - 0.05*XSTAGE
         ENDIF
         IF (TCNP .LE. TMNC) THEN
           TCNP = TMNC + 0.05
         ENDIF
         TCNP = TCNP*0.01
         TMNC = TMNC*0.01
         RCNP = 1.06/100.0
         IF (XSTAGE .LT. 0.15) THEN
           TANC = TCNP
           RANC = RCNP
         ENDIF
         NFAC = 1.0-(TCNP-TANC)/(TCNP-TMNC)
         NFAC = AMIN1 (NFAC,1.0)
         IF (ISTAGE .EQ. 3 .OR. ISTAGE .EQ. 4) THEN
           NFAC = 1.0-2.0*EXP(-3.5*NFAC)
         ENDIF
         NFAC   = AMIN1 (NFAC,1.0)
         NFAC   = AMAX1 (NFAC,0.001)
         NSTRES = 1.0
         AGEFAC = 1.0
         NDEF3  = 1.0
         AGEFAC = NFAC
         NSTRES = NFAC * 1.2 + 0.2
         NDEF3  = 0.15 + NFAC*0.85
         NSTRES = AMIN1 (NSTRES,1.0)
         AGEFAC = AMIN1 (AGEFAC,1.0)
         NDEF3  = AMIN1 (NDEF3, 1.0)
C-GH Stress factors also accumulate in SG_Phenol         
C-GH     CNSD1  = CNSD1 + 1.0 - NSTRES
C-GH     CNSD2  = CNSD2 + 1.0 - AGEFAC

      ENDIF

      RETURN
      END SUBROUTINE SG_NFACT
