C=======================================================================
C  TEFF_NFACTO, Subroutine
C
C  Determines N deficiency factor
C
C-----------------------------------------------------------------------
C  Revision history
C
C                 Written
C  08/07/1993 PWW Header revision and minor changes
C  08/29/2002 CHP/MUS Converted to modular format for inclusion in CSM.
C  12/16/2004 US  Changed dimension for NPER, XST to 16
C  12/12/2019 MB/US Copyed from Rice model and modified for Teff 
C  03/29/2021 MB/WP Addapted to Teff based on CERES-Rice
C-----------------------------------------------------------------------
C  Called : TRNSP GROSUB
C  Calls  : None
C=======================================================================

      SUBROUTINE TEFF_NFACTO(DYNAMIC, FIELD, XSTAGE,              !Input
     &    AGEFAC, NDEF3, NFAC, NSTRES, RCNP, TANC, TCNP, TMNC)  !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL ALIN
      SAVE

      INTEGER DYNAMIC
      REAL AGEFAC, ALIN, NDEF3, NFAC, NSTRES, RCNP
      REAL TANC, TCNP, TMNC, XSTAGE
      LOGICAL FIELD

      INTEGER, PARAMETER :: NDATA = 16
      REAL NPER(NDATA), XST(NDATA)
C
C     Data from Ishizuka, SINGH-(IBSNAT)
C
!  03/03/2007 US - Put these in species file.
!  TODO 10/31/2016 chp - still not in species file!!
!  N concentration (NPER) as function of crop stage (XST)
      DATA XST /0.80,0.82,0.85,0.90,1.00,1.50,2.00,2.50,3.00,4.00,
     &          5.00,5.20,5.50,6.00,7.50,9.0/
      !DATA NPER/4.30,4.00,3.80,3.40,3.00,2.90,2.70,2.30,2.00,1.75,
      !&          1.45,1.00,0.70,0.65,0.50,0.45/  
      DATA NPER/4.40,4.20,4.10,3.90,3.41,2.50,2.00,1.70,1.50,1.10,
     &          0.65,0.60,0.55,0.50,0.40,0.20/

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!     FROM INPLNT:
      NSTRES   = 1.0
      AGEFAC   = 1.0
      NDEF3    = 1.0
      NFAC     = 1.0 

      TCNP = 0.044
      RCNP = 0.01                                    ! 1.0/100.0
      TANC = 0.044
      TMNC = 0.0

!***********************************************************************
!***********************************************************************
!     Daily rate / integration calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
      TCNP = ALIN (XST,NPER,NDATA,XSTAGE)*0.01 
!      TMNC = 0.0045
      IF (XSTAGE .GE. 6.0) THEN
         TMNC = 0.0035 - (XSTAGE-6.0)*0.0006
       ELSE
         TMNC = (1.275-0.10*XSTAGE)/100.0            !.20
      ENDIF
      
      IF (.NOT. FIELD) THEN
         NSTRES = 1.0
         AGEFAC = 1.0
         NDEF3  = 1.0
         TANC   = TCNP
         RETURN                                                                 
      ENDIF

      NFAC = 1.0 - (TCNP-TANC)/(TCNP-TMNC)
C
C     Allow for adjustment in NFAC following transplanting
C
      NDEF3  = 1.0
      NFAC   = AMIN1 (NFAC,1.0)
      NFAC   = AMAX1 (NFAC,0.0)
      NSTRES = 1.0
      AGEFAC = 1.0
      AGEFAC = NFAC**1.65
      IF (AGEFAC .LT. 0.1) THEN
         AGEFAC = 0.5*NFAC
      ENDIF
      IF (NFAC .LT. 0.8) THEN
         NSTRES = NFAC
      ENDIF
      NDEF3  = NSTRES*1.25
      NSTRES = AMIN1 (NSTRES,1.0)
      AGEFAC = AMIN1 (AGEFAC,1.0)
      NDEF3  = AMIN1 (NDEF3 ,1.0)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN                                                                    
      END SUBROUTINE TEFF_NFACTO                                                                
