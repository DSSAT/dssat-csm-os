C=======================================================================
C  TR_NFACTO, Subroutine
C
C  Determines N deficiency factor
C
C-----------------------------------------------------------------------
C  Revision history
C
C                 Written
C  08/07/1993 PWW Header revision and minor changes
C  08/29/2002 CHP/MUS Converted to modular format for inclusion in CSM.
C  03/27/2014 US,RO Initialization fixed
C-----------------------------------------------------------------------
C  Called : TRNSP GROSUB
C  Calls  : None
C=======================================================================

      SUBROUTINE TR_NFACTO(DYNAMIC, FIELD, XSTAGE,              !Input
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
      REAL NPER(13), XST(13)
      LOGICAL FIELD

C
C     Singh-(IBSNAT), Kabeerathumma etal 1984. J Root Crops,10:29-32
C     Silva (personal comm)
      !
      DATA XST /0.80,1.0,1.5,2.00,2.50,3.00,4.00,
     &          5.00,5.20,5.50,6.00,7.50,9.0/
      DATA NPER/3.6,3.4,3.2,3.15,3.10,3.00,3.00,
     &          3.00,3.00,2.90,2.70,2.60,2.50/
                              

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

      TCNP = 0.040
      RCNP = 0.015                                   
      TANC = 0.040
      TMNC = 0.0

!***********************************************************************
!***********************************************************************
!     Daily rate / integration calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------


      TCNP = ALIN (XST,NPER,13,XSTAGE)*0.01
      TMNC =(1.55-0.01*XSTAGE)/100.0
                                

      IF (.NOT. FIELD) THEN
         NSTRES = 1.0
         AGEFAC = 1.0
         NDEF3  = 1.0
         TANC   = TCNP
         RETURN                                                                 
      ENDIF

      IF (XSTAGE .LE. 0.02) THEN
         TANC = TCNP
      ENDIF
      NFAC = 1.0 - (TCNP-TANC)/(TCNP-TMNC)
c
C
C     Allow for adjustment in NFAC following transplanting
C
      NFAC   = AMIN1 (NFAC,1.0)
      NFAC   = AMAX1 (NFAC,0.0)
      NSTRES = 1.0
      AGEFAC = 1.0
      AGEFAC = NFAC
      IF (NFAC .LT. 0.5) THEN
         NSTRES = 1.8*NFAC
      ENDIF
      NDEF3  = NFAC
      NSTRES = AMIN1 (NSTRES,1.0)
      AGEFAC = AMIN1 (AGEFAC,1.0)
      NDEF3  = AMIN1 (NDEF3 ,1.0)
      NSTRES = AMAX1 (NSTRES,0.001)
      AGEFAC = AMAX1 (AGEFAC,0.001)
      NDEF3  = AMAX1 (NDEF3 ,0.001)
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN                                                                    
      END SUBROUTINE TR_NFACTO                                                                
