C=======================================================================
C  CSP_MOBIL, Subroutine for CASUPRO sugarcane model, based on 
C  MOBIL, Subroutine, G. Hoogenboom, J.W. Jones, and K.J.Boote
C-----------------------------------------------------------------------
C  Calculating of the Mobilization of N
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  01/09/1989 GH  Written
C  04/02/1996 KJB Mobilization modified
C  08/15/1998 CHP Modified for modular format
C  05/10/1990 GH  Incorporated in CROPGRO
!  11/12/2001 O.H. Daza modified for the sugarcane model
C  08/28/2003 FSR Incorporated in CASUPRO for DSSAT 4.0
C-----------------------------------------------------------------------
!  Called by:  CASUPRO
!  Calls:      None
C=======================================================================
      SUBROUTINE CSP_MOBIL(DYNAMIC,
     &  NDMNEW, NMINEP, NMOBR, RPRO, TRNU,                !Input
     &  WNRLF, WNRRT, WNRSU, WNRST,                       !Input
     &  NMINEA, NRUSLF, NRUSRT, NRUSSU, NRUSST)           !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE
!-----------------------------------------------------------------------	
      INTEGER DYNAMIC
     
      REAL CNMINE, NDMNEW, NMINEA, NMINEP, NMINER, NMOBR
      REAL NRUSLF, NRUSRT, NRUSST, RPRO
      REAL TRNU, WNRLF, WNRRT, WNRST

! NEW
      REAL WNRSU, NRUSSU

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      CNMINE = 0.0         !
      NMINEA = 0.0         !
      NRUSLF = 0.0         !moved from INPLNT
      NRUSST = 0.0         !
      NRUSRT = 0.0         !

!Sugars      
	NRUSSU = 0.0
	WNRSU  = 0.0

!***********************************************************************
!***********************************************************************
!     DAILY RATE/INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
      CNMINE = 0.0
      NMINEA = 0.0
      NRUSLF = 0.0
      NRUSST = 0.0
      NRUSRT = 0.0

!Sugars      
	NRUSSU = 0.0
	WNRSU  = 0.0

C-----------------------------------------------------------------------
!    Leave MOBIL with N Mined from Leaf, Stem,Root, Sugars, and
!    Total Plant Tissue, and CH2O used in the Re-synthesis of Protein
!-----------------------------------------------------------------------
      IF (TRNU .LT. NDMNEW .AND. NMINEP .GT. 0.0) THEN
         NMINEA = NDMNEW - TRNU
         IF (NMINEA .GT. NMINEP) NMINEA = NMINEP
         NMINER = NMINEA / NMINEP * NMOBR
         NRUSLF = NMINER * WNRLF
         NRUSST = NMINER * WNRST
         NRUSRT = NMINER * WNRRT
!Sugars      
         NRUSSU = NMINER * WNRSU

         CNMINE = NMINEA / 0.16 * RPRO        !Not used
      ENDIF
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END ! SUBROUTINE CSP_MOBIL

!-----------------------------------------------------------------------
! CNMINE  Protein re-synthesis cost (g[CH2O] / m2)
! NDMNEW  Total N demand for new growth (g[N] / m2 / d)
! NMINEA  Actual Nitrogen mined from existing tissue (g[N] / m2 / d)
! NMINEP  Potential N mobilization from storage (g[N] / m2 / d)
! NMINER  Total N actually mobilized from plant in a day (g[N]/m2-d)
! NMOBR   Stage dependent N mining rate 
! NRUSLF  N actually mobilized from leaves in a day (g[N]/m2-d)
! NRUSRT  N actually mobilized from roots in a day (g[N]/m2-d)
! NRUSST  N actually mobilized from stems in a day (g[N]/m2-d)
! RPRO    Respiration required for re-synthesizing protein from mobilized N
!           (g[CH2O] / g[protein])
! TRNU    Total N uptake in a day (g[N] / m2 / d)
! WNRLF   N available for mobilization from leaves above lower limit of 
!           mining (g[N] / m2)
! WNRRT   N available for mobilization from roots above lower limit of 
!           mining (g[N] / m2)
! WNRST   N available for mobilization from stems above lower limit of 
!           mining (g[N] / m2)

!NEW
! NRUSSU  N actually mobilized from sugars in a day (g [N] / m2 - d)
! WNRSU   N available for mobilization from sugars above lower limit of 
!           mining (g [N] / m2)
!-----------------------------------------------------------------------
!      END SUBROUTINE CSP_MOBIL
!-----------------------------------------------------------------------
