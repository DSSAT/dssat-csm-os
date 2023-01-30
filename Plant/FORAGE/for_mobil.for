C=======================================================================
C  FOR_MOBIL, Subroutine, G. Hoogenboom, J.W. Jones, and K.J.Boote
C-----------------------------------------------------------------------
C  Calculating of the Mobilization of N
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  01/09/1989 GH  Written
C  04/02/1996 KJB Mobilization modified
C  08/15/1998 CHP Modified for modular format
C  05/10/1990 GH  Incorporated in CROPGRO
C-----------------------------------------------------------------------
!  Called by:  CROPGRO
!  Calls:      None
C=======================================================================
      SUBROUTINE FOR_MOBIL(
     &    LFSNMOB, RPRO, RTSNMOB, SRSNMOB, STSNMOB,       !Input
     &    TSNMOB, ANMINELF, ANMINERT, ANMINESH,           !Output
     &    ANMINESR, ANMINEST, NMINEA, NRUSLF,             !Output
     &    NRUSRT, NRUSSH, NRUSST, NRUSSR,                 !Output
     &    PNMLF, PNMST, PNMRT, PNMSR, PNMSH,              !Output
     &    DYNAMIC)                                        !Control

! 2023-01-19 CHP Remove unused variables from argument list
!  NDMNEW, NMINELF, NMINEP, NMINERT, NMINESR, NMINEST, NMOBR, SHNMINE, 
!  TRNU, WNRLF, WNRRT, WNRSH, WNRST, NMOBSR, PPMFAC, WNRSR, 
!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
        ! which contain control information, soil
        ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      INTEGER DYNAMIC

      REAL CNMINE, NMINEA   !NDMNEW, NMINEP, NMINER, NMOBR
      REAL NRUSLF, NRUSRT, NRUSSH, NRUSST, RPRO
!     REAL TRNU, WNRLF, WNRRT, WNRSH, WNRST
      REAL NRUSSR  !, WNRSR, NMOBSR, PPMFAC
      REAL NRUSTOT, PNMLF, PNMST, PNMRT, PNMSR, PNMSH
      REAL ANMINELF, ANMINERT, ANMINESH, ANMINESR, ANMINEST, LFSNMOB, 
     &   RTSNMOB, !NMINELF, NMINERT, NMINESR, NMINEST, SHNMINE,
     &   SRSNMOB, STSNMOB, TSNMOB  

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
      NRUSSH = 0.0         !
      NRUSSR = 0.0
      PNMLF=0.0
      PNMST=0.0
      PNMRT=0.0
      PNMSR=0.0
      PNMSH=0.0

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
      NRUSSH = 0.0
      NRUSSR = 0.0

      ANMINELF = 0.0
      ANMINEST = 0.0
      ANMINERT = 0.0
      ANMINESR = 0.0
      ANMINESH = 0.0

C-----------------------------------------------------------------------
!    Leave FOR_MOBIL with N Mined from Leaf, Stem,Root, Shell, and
!    Total Plant Tissue, and CH2O used in the Re-synthesis of Protein
!-----------------------------------------------------------------------
!      IF (TRNU .LT. NDMNEW .AND. NMINEP .GT. 0.0) THEN
!         NMINEA = NDMNEW - TRNU
!         IF (NMINEA .GT. NMINEP) NMINEA = NMINEP


!         NMINER = NMINEA/NMINEP * NMOBR
!         NRUSLF = NMINER * WNRLF
!         NRUSST = NMINER * WNRST
!         NRUSSH = NMINER * WNRSH
!         NRUSRT = NMINER * WNRRT * (PPMFAC)
!           NRUSSR = NMINEA * WNRSR * NMOBSR / NMINEP

!         CNMINE = NMINEA / 0.16 * RPRO        !Not used
!      ENDIF
      NMINEA = TSNMOB

C     NMINELF SHOULD HAVE BEEN LFNMINE, ETC. IN EARLIER VERSION, NOW NMINEP=TSNMOB=NMINEA

!      IF (TRNU .LT. NDMNEW .AND. NMINEP.GT. 0.0) THEN
!         NMINEA = MAX(TSNMOB,NDMNEW - TRNU)
!         IF (NMINEA .GT. NMINEP) NMINEA = NMINEP
!
!            IF (NMINEA .GT. TSNMOB) THEN
!                  NMINER = (NMINEA - TSNMOB) / (NMINEP - TSNMOB)
!                  ANMINESH = SHNMINE * NMINER
!                  ANMINELF = (NMINELF - LFSNMOB) * NMINER
!                  ANMINEST = (NMINEST - STSNMOB) * NMINER
!                  ANMINERT = (NMINERT - RTSNMOB) * NMINER
!                  ANMINESR = (NMINESR - SRSNMOB) * NMINER
!            ENDIF
!      ENDIF
        
        NRUSLF = LFSNMOB + ANMINELF
        NRUSST = STSNMOB + ANMINEST
        NRUSRT = RTSNMOB + ANMINERT
        NRUSSR = SRSNMOB + ANMINESR
        NRUSSH = ANMINESH

        CNMINE = NMINEA / 0.16 * RPRO        !Not used
  

C-----------------------------------------------------------------------
!    Calculate proportion of N Mined from Leaf, Stem,Root, Shell, and
!      Storage - Use to put back excess mobilized N
C-----------------------------------------------------------------------

      NRUSTOT = NRUSLF+NRUSST+NRUSSH+NRUSRT+NRUSSR
      IF (NRUSTOT .GT. 0.0) THEN
      PNMLF=NRUSLF/NRUSTOT
      PNMST=NRUSST/NRUSTOT
      PNMRT=NRUSRT/NRUSTOT
      PNMSR=NRUSSR/NRUSTOT
      PNMSH=NRUSSH/NRUSTOT
      ELSE
      PNMLF=0.0
      PNMST=0.0
      PNMRT=0.0
      PNMSR=0.0
      PNMSH=0.0
      ENDIF
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END ! SUBROUTINE FOR_MOBIL

!-----------------------------------------------------------------------
! ANMINELF Actual leaf N mobilized in excess of that from natural senescence
!                  (g[N] / m2 / d)
! ANMINERT Actual root N mobilized in excess of that from natural senescence
!                  (g[N] / m2 / d)
! ANMINESH Actual shell N mobilized 
!                  (g[N] / m2 / d)                  
! ANMINESR Actual STOR N mobilized in excess of that from natural senescence
!                  (g[N] / m2 / d)
! ANMINEST Actual stem N mobilized in excess of that from natural senescence
!                  (g[N] / m2 / d)
! CNMINE  Protein re-synthesis cost (g[CH2O] / m2)
! DYNAMIC Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, INTEGR, 
!           OUTPUT, or SEASEND 
! LFSNMOB Leaf N mobilization from natural senescence (g[N] / m2 / d)
! NDMNEW  Total N demand for new growth (g[N] / m2 / d)
! NMINEA  Actual Nitrogen mined from existing tissue (g[N] / m2 / d)
! NMINEP  Potential N mobilization from storage (g[N] / m2 / d)
! NMINER  Total N actually mobilized from plant in a day (g[N]/m2-d)
! NMOBR   Stage dependent N mining rate 
! NMOBSR  Stage dependent N mining rate for storage organ 
! NRUSLF  N actually mobilized from leaves in a day (g[N]/m2-d)
! NRUSRT  N actually mobilized from roots in a day (g[N]/m2-d)
! NRUSSH  N actually mobilized from shells in a day (g[N]/m2-d)
! NRUSSR  N actually mobilized from storageorgan in a day (g[N]/m2-d)
! NRUSST  N actually mobilized from stems in a day (g[N]/m2-d)
! NRUSTOT Total N actually mobilized in a day (g[N]/m2-d)
! PPMFAC  Reduction in mobilization from storage organ due to photoperiod 
!              induced dormancy
! PNMLF      Proportion of actually mobilized N mobilized from leaves in a day
! PNMST      Proportion of actually mobilized N mobilized from stems in a day
! PNMRT      Proportion of actually mobilized N mobilized from roots in a day
! PNMSR      Proportion of actually mobilized N mobilized from storage organ in a day
! PNMSH      Proportion of actually mobilized N mobilized from shells in a day
! RPRO    Respiration required for re-synthesizing protein from mobilized N
!           (g[CH2O] / g[protein])
! RTSNMOB Root N mobilization from natural senescence (g[N] / m2 / d)
! SRSNMOB STOR N mobilization from natural senescence (g[N] / m2 / d)
! STSNMOB Stem N mobilization from natural senescence (g[N] / m2 / d)
! TRNU    Total N uptake in a day (g[N] / m2 / d)
! TSNMOB  Total plant N mobilization from natural senescence (g[N] / m2 / d)
! WNRLF   N available for mobilization from leaves above lower limit of 
!           mining (g[N] / m2)
! WNRRT   N available for mobilization from roots above lower limit of 
!           mining (g[N] / m2)
! WNRSH   N available for mobilization from shells above lower limit of 
!           mining (g[N] / m2)
! WNRSR   N available for mobilization from storage organ above lower 
!           limit of mining (g[N] / m2)
! WNRST   N available for mobilization from stems above lower limit of 
!           mining (g[N] / m2)
!-----------------------------------------------------------------------
!      END SUBROUTINE FOR_MOBIL
!-----------------------------------------------------------------------
