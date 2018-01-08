C= FILE: CSP_INPHENOL.for===============================================
C  CSP_INPHENOL - Adapted by O.H. Daza
C  INPHENOL  - CHP
C  Initializes phenological parameters.
C----------------------------------------------------------------------
C  REVISION HISTORY
C  07/30/1998 CHP moved from PHENOL
C  12/11/2000 GH  added cowpea code
C  09/26/2001 OHD adapted for the sugarcane model CASUPRO 
C  07/26/2004 CHP Removed variables which were not being used
C-----------------------------------------------------------------------
C  Called by: CSP_PHENOL  in CSP_PHENOL.for
C  Calls    : None
C=======================================================================
      SUBROUTINE CSP_INPHENOL(
     &    CROP, PHTHRS, TB, TO1,                          !Input
     &    CumOptStageDur, OptStageDur, STNAME)            !Output
  
!     &    CROP, PHTHRS, PLME, TB, TO1, TO2,     !Input
C-----------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE
C-----------------------------------------------------------------------
      CHARACTER*2 CROP
!      CHARACTER*1 PLME
      CHARACTER*10 STNAME(20)

      INTEGER I, J, NPHS
      PARAMETER (NPHS = 4)
      INTEGER, PARAMETER :: NumOfPhases = 4

	REAL PHTHRS(NumOfPhases)
	REAL Tb(5), To1(5)  ! , To2(5)

! Dopt              optimum thermal time, °C-d
! OptStageDur(4)    Optimum stage duration, days
! CumOptStageDur(4) Cumulative optimum stage duration, days
      REAL Dopt, OptStageDur(NPHS), CumOptStageDur(NPHS)

C-----------------------------------------------------------------------
C     This section computes factors which vary with cultivar
C     at the beginning of each run
C-----------------------------------------------------------------------
C     Set minimum days for phenological events under optimum conditions
C     (temperature and short photoperiod)
C-----------------------------------------------------------------------
      DO I = 1,NPHS
	  OptStageDur(I) = 0
	  CumOptStageDur(I) = 0
      END DO
	 
      IF (CROP .NE. 'FA') THEN
	  Dopt = To1(1) - Tb(1) 
        DO I = 1,NPHS
	    OptStageDur(I) = PHTHRS(I) / Dopt
	    IF (I == 1) THEN
            CumOptStageDur(I) = OptStageDur(I)
          ELSE
	      CumOptStageDur(I) = CumOptStageDur(I - 1) + OptStageDur(I)
          END IF
	  END DO
      END IF

C-----------------------------------------------------------------------
C     Define names of stages
C-----------------------------------------------------------------------
      DO J = 1,20  ! Initialize array to blanks
        STNAME(J) = '          '
      END DO

! These are the phenological stages defined in the model.
! More stages can be added if needed.
!chp      IF (CROP .EQ. 'SC') THEN    
        STNAME(1)  = 'Planting  '
        STNAME(2)  = 'Sprouting '
        STNAME(3)  = 'Emergence '
        STNAME(4)  = 'Beg sk grw' ! Begining of stalk growth
	  STNAME(5)  = 'Flowering '
! Positions 15 to 20 are reserved to other quasi-stages like 
! Start of Simulation and End of Simulation
        STNAME(15) = 'Start Sim ' 
        STNAME(16) = 'Harvest   '

!chp 2/13/07 'FA' crop handled by CROPGRO
!chp      ELSE IF (CROP .EQ. 'FA') THEN
!chp        STNAME(15) = 'Start Sim '
!chp        STNAME(16) = 'End Sim   '
!chp      END IF

!      IF (PLME == 'T') THEN
!        STNAME(1) = 'Transplant'

! Check this again. It is required to simulate plant and ratoon crops. 
! However R is an input in the IBSNAT35.INP file
!      IF (PLME == 'R') THEN 
!        STNAME(1) = 'Ratooning '
!        STNAME(2) = 'Ratooning ' ! 
!      ENDIF

!***********************************************************************
      RETURN

      END  !SUBROUTINE CSP_INPHENOL

!-----------------------------------------------------------------------
!     INPHENOL LOCAL VARIABLES:
!-----------------------------------------------------------------------
! VARIABLE           DEFINITION, UNITS

! CROP               Crop identification code
! CumOptStageDur(I)  Cumulative optimum stage duration, days
! Davg               Average optimum thermal time, °C-d
! OptStageDur(I)     Optimum stage duration, days
! PHTHRS(I)          Threshold time that must accumulate in phase I for 
!                      the next stage to occur  (thermal or photothermal 
!                      days), °C-d
! PLME               Planting method (read from FILEX)
!                      T = transplant
!                      S = seed
!                      P = pre-germinated seed
!                      N = nursery,
!                      R = ratooning (for sugarcane)
! STNAME             Output headings for specified crops
! Tavg               Average optimum temperature, °C
! TB                 Base temperature, °C
! TM                 Maximum Temperature, °C
! TO1                Lower optimum temperature, °C
! TO2                Upper optimum temperature, °C
!-----------------------------------------------------------------------
!     END SUBROUTINE INPHENOL
!-----------------------------------------------------------------------
