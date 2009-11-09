C=======================================================================
C  ROOTDM, Subroutine
C  Calculates root damage due to pests.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/90 WDB Written
C  03/02/98 CHP Modified for PEST Module
C  01/12/99 GH  Incorporated into CROPGRO
C-----------------------------------------------------------------------
C  Called by: PEST
C  Calls:     None
C=======================================================================
      SUBROUTINE ROOTDM(
     &    PRLV,PRTLF, PRTLV, PRTMD, RTWT, SOILPROP, TRTLF, !Input
     &    RLV, TRTLV, WRTMD,                              !Input/Output
     &    CRLF, CRLV, CRTM, RLFDOT, RLVDOT, WRIDOT,       !Output
     &    DYNAMIC)                                        !Control

C-----------------------------------------------------------------------
C     Variable declaration
C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE
      
      INTEGER L
      INTEGER NLAYR
      INTEGER DYNAMIC
      

      REAL RLVDOT,CRLV,RLFDOT,CRLF,CRTM
      REAL WRTMD,PRTMD,TRTLV,PRTLV,PRTLF,TRTLF,WRIDOT
      REAL TRLV,RTWT, CUMDEP
      REAL DLAYR(NL), RLV(NL), RTFRAC(NL)
      REAL PRLV

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

!     The variable "SOILPROP" is of type "SoilType".
      TYPE (SoilType) SOILPROP

      DLAYR  = SOILPROP % DLAYR  
      NLAYR  = SOILPROP % NLAYR  

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      CRLV = 0
      CRLF = 0
      CRTM = 0

      RLVDOT = 0.0
      RLFDOT = 0.0
      WRIDOT = 0.0

C-----------------------------------------------------------------------
C     Compute soil profile depth (cm)
C-----------------------------------------------------------------------
      CUMDEP = 0.0
      DO L = 1,NLAYR
        CUMDEP = CUMDEP + DLAYR(L)
      ENDDO

!***********************************************************************
!***********************************************************************
!     Daily rate calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
C-----------------------------------------------------------------------
      RLVDOT = 0.0
      RLFDOT = 0.0
      WRIDOT = 0.0
C-----------------------------------------------------------------------
C     Compute total root length volume
C-----------------------------------------------------------------------
      TRLV = 0.0
      DO L = 1,NLAYR                            
        TRLV = TRLV + RLV(L)* DLAYR(L)          
      ENDDO                                     
      IF (TRLV .LE. 0.0001) RETURN  

!     Calculate fraction of root mass in each soil layer
!     This will be used to distribute RLV changes
      DO L=1,NLAYR
        RTFRAC(L) = RLV(L)*DLAYR(L)/TRLV
      ENDDO

C-----------------------------------------------------------------------
C     Percent root mass destroyed
C-----------------------------------------------------------------------
      IF (PRTMD .GT. 0.0) THEN
        WRIDOT = WRIDOT + (PRTMD/100.) * RTWT 
        RLVDOT = RLVDOT + (PRTMD/100.) * (TRLV/CUMDEP)
      ENDIF
C-----------------------------------------------------------------------
C     Totol root mass destroyed  g/m2/day
C-----------------------------------------------------------------------
      IF (WRTMD .GT. 0.0) THEN
        WRTMD = MIN(WRTMD,RTWT)
        WRIDOT = WRIDOT + WRTMD 
        RLVDOT = RLVDOT + (WRTMD/RTWT) * (TRLV/CUMDEP) 
      ENDIF
C-----------------------------------------------------------------------
C     Percent root length volume damaged (cm/cm3)
C-----------------------------------------------------------------------
      IF (PRTLV .GT. 0.0) THEN
        WRIDOT = WRIDOT + (PRTLV/100.) * RTWT 
        RLVDOT = RLVDOT + (PRTLV/100.) * (TRLV/CUMDEP)
      ENDIF
C-----------------------------------------------------------------------
C     Total root length volume damaged (cm/cm3)
C-----------------------------------------------------------------------
      IF (TRTLV .GT. 0.0) THEN
        TRTLV = MIN(TRTLV,TRLV)
        WRIDOT = WRIDOT + RTWT * TRTLV/(TRLV/CUMDEP)
        RLVDOT = RLVDOT + TRTLV 
      ENDIF
C-----------------------------------------------------------------------
C   Percent root length flux (cm/cm2) destroyed
C-----------------------------------------------------------------------
      IF (PRTLF .GT. 0.0) THEN
        WRIDOT = WRIDOT + (PRTLF/100.) * RTWT 
        RLVDOT = RLVDOT + (PRTLF/100.) * (TRLV/CUMDEP)
      ENDIF
C-----------------------------------------------------------------------
C   Total root length flux, cm/cm2 destroyed
C-----------------------------------------------------------------------
      IF (TRTLF .GT. 0.0 .AND. TRLV .GT. 0.0) THEN
        WRIDOT = WRIDOT + (TRTLF/TRLV) * RTWT
        RLVDOT = RLVDOT + (TRTLF/CUMDEP) 
      ENDIF

      WRIDOT = MAX(0.0, MIN(WRIDOT, RTWT))
      RLVDOT = MAX(0.0, MIN(RLVDOT, TRLV/CUMDEP))
      RLFDOT = RLVDOT * CUMDEP

C-----------------------------------------------------------------------
C     When population is reduced in ceres-maize, root mass/m2 is reduced
C     through reduction in population directly. The following allows
C     root length volume to be adjusted separately

C     Percent root length volume damaged (cm/cm3)
C-----------------------------------------------------------------------
      IF (PRLV .GT. 0.0) THEN
c        WRIDOT = WRIDOT + (PRTLV/100.) * RTWT 
        RLVDOT = RLVDOT + (PRLV/100.) * (TRLV/CUMDEP)
      ENDIF

!***********************************************************************
!***********************************************************************
!     Daily integration
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
C-----------------------------------------------------------------------
!     RLVDOT is distributed to layers by root mass fraction
!     (RTFRAC) regardless of pest type or method.
      IF (RLVDOT .GT. 0.0) THEN
        DO L = 1,NLAYR
          RLV(L) = RLV(L) - RLVDOT * CUMDEP * RTFRAC(L) / DLAYR(L)
          !CUMDEP converts from per volume to per unit area 
          !RTFRAC(L) proportions among layers
          !Division by DLAYR(L) converts back to per volume basis for
          !  each soil layer
          RLV(L) = MAX(0.0, RLV(L))
        ENDDO
      ENDIF

C-----------------------------------------------------------------------
C     Compute cumulative root length mass, voluem, and flux
C     and daily root length flux (cm root / cm2 ground) damage
C-----------------------------------------------------------------------
      CRTM = CRTM + WRIDOT
      CRLV = CRLV + RLVDOT
      CRLF = CRLF + RLFDOT

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END   ! SUBROUTINE ROOTDM

C-----------------------------------------------------------------------
C     Variable definitions
C-----------------------------------------------------------------------
! CRLF      Cumulative root length flux (cm [root] / cm2 [ground])
! CRLV      Cumulative root length density (cm [root] / cm3 [soil])
! CRTM      Cumulative root mass (g/m2)
! CUMDEP    Cumulative depth of soil profile (cm)
! DLAYR(L)  Soil thickness in layer L (cm)
! NL        Maximum number of soil layers = 20 
! NLAYR     Actual number of soil layers 
! PRLV      Percent reductin in root length volume in each layer, %
! PRTLF     Percent of root flux destroyed (%/d)
! PRTLV     Daily percent reduction in root length volume  (%/d)
! PRTMD     Daily percent root mass damage (%/d)
! RLFDOT    Daily root length flux damage (cm root / cm2 ground)
! RLV(L)    Root length density for soil layer L (cm[root] / cm3[soil])
! RLVDOT    Daily root length damage (cm root / cm3 soil)
! RTFRAC(L) Fraction of total root mass in soil layer L 
! RTWT      Dry mass of root tissue, including C and N
!             (g[root] / m2[ground])
! TRLV      Total root length per square cm soil today
!             (cm[root] / cm2[ground)
! TRTLF     Total root flux destroyed (cm/cm2/d)
! TRTLV     Daily absolute reduction in root length volume  (cm/cm3/d)
! WRIDOT    Daily pest damage to root mass (g/m2/day)
! WRTMD     Daily absolute root mass reduction  (g/m2/day)
C-----------------------------------------------------------------------
C     End Subroutine ROOTDM
C-----------------------------------------------------------------------


