C=======================================================================
C  CSP_RESPIR Subroutine, O.H. Daza
C  Based on RESPIR Subroutine, K.J.Boote, J.W.Jones, G. Hoogenboom
C  Calculates maintenance respiration by stalk.  Net photosynthate
C  available, PGAVAL(j) is computed in the CSP_CASUPRO module.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1989 KJB Written.
C  10/15/1992 NBP Made into subroutine.
C  09/15/1998 CHP Modified for modular format
C  05/11/1999 GH  Incorporated in CROPGRO
C  11/12/2001 OHD Modified for the CASUPRO sugarcane model 
C  08/25/2003 FSR Incorporated into DSSAT 4.0 CASUPRO
C  07/26/2004 CHP Removed variables which were not being used
C  10/20/2005 FSR Revised for stalk-based output
C-----------------------------------------------------------------------
C  Called from:   CSP_CASUPRO
C  Calls:         None
C=======================================================================
      SUBROUTINE CSP_RESPIR(CONTROL,
     &    LeafArea, LeafDist, LFWT, R30C2,            !Input
     &	RES30C, RLF30C, SLAREF, StalkState,         !Input
     &    StkPG, SUWT, TGRO, TOTALWT,                 !Input
     &    RO, RP,                                     !Input/Output
     &    LeafMaint, MAINTR)                          !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
!     TS defined in ModuleDefs.for

      IMPLICIT NONE
      SAVE

      CHARACTER*4 StalkState(NumOfStalks,10)

      INTEGER DAS, H, Stalk, Zone
	INTEGER, PARAMETER :: CanopyLayers=3

	REAL R30C2, RES30C, RLF30C, RL, RO, RP, SLAREF, SLASTK, SCLTS !LMF, 
C             SCLTS added on 4 July 2017 by Bruce Kimball

      REAL TGRO(TS), TRSFAC !TGROAV, 

	REAL, DIMENSION(1:NumOfStalks) :: MAINTR,TOTALWT 
	REAL, DIMENSION(0:NumOfDays,NumOfStalks) :: LeafArea, LFWT, SUWT
	REAL, DIMENSION(1:NumOfStalks,CanopyLayers) :: LeafDist, 
     &                LeafMaint, StkPG

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL

!     Transfer values from constructed data types into local variables.
      DAS     = CONTROL % DAS

C-----------------------------------------------------------------------
C     Initialize output variables

	DO Stalk = 1, NumOfStalks
	  
	   MAINTR(Stalk) = 0

	   DO Zone = 1,3 

	       LeafMaint(Stalk,Zone) = 0

	   END DO ! Zone = 1,Gro

	END DO ! 

C-----------------------------------------------------------------------
C     Hourly temperature effect on maintenance respiration (McCree, 1974)

      TRSFAC = 0.0
      SCLTS = 24./TS
      DO H = 1,TS
C        TRSFAC = TRSFAC + 0.044+0.0019*TGRO(H)+0.001*TGRO(H)**2
         TRSFAC = TRSFAC + (0.044+0.0019*TGRO(H)+0.001*TGRO(H)**2)*SCLTS
C    scaling factor of 24/TS added on 4 July 2017 by Bruce Kimball
      ENDDO
C 24 changed to TS on 3 Jul 17 by Bruce Kimball
C Look suspicious because TRSFAC will accumulate according to number of time steps
      
C-----------------------------------------------------------------------
C     Convert hourly maintenance respiration to daily, temperature-based 
C     value. RES30C and RLF30C are the g CH2O/g DW/hr used in maintenance 
C     respiration for tiller and root DW, and leaf DW respectively; 30 C.
	
	RL = (RLF30C * TRSFAC) / 1000  ! convert units from Cultivar file
      RO = (RES30C * TRSFAC) / 1000  ! convert units from Cultivar file
      RP = (R30C2  * TRSFAC) / 1000  ! convert units from Cultivar file
C-----------------------------------------------------------------------
!      Maintenance respiration for each stalk.  Sucrose is removed from
!      maintenance costs, and an explicit respiration cost for leaves 
!      is now included (RLF30C).

!      Note: Use DAS-1 since daily values not yet calculated when 
!	 CSP_RESPIR is called.

	DO Stalk = 1, NumOfStalks

        IF (StalkState(Stalk,1) .EQ. 'LIVE') THEN 

		MAINTR(Stalk) = RO * (TOTALWT(Stalk) 
     &                  - (LFWT(DAS-1,Stalk)) + SUWT(DAS-1,Stalk))

	    ! The following  calculates a whole-stalk SLA, which is then 
		! applied to all canopy zones on the stalk.  This is a more 
		! accurate representation of instantanious leaf DM than SLAREF.
		   
           IF (LFWT(DAS-1,Stalk) .GT. 0.0) THEN 
              SLASTK = LeafArea(DAS-1,Stalk)/LFWT(DAS-1,Stalk)
		    ELSE 
		    SLASTK = SLAREF
           END IF

           DO Zone = 1,3 

              LeafMaint(Stalk,Zone) = RP * StkPG(Stalk,Zone) 
     &		                + RL * LeafDist(Stalk,Zone) / SLASTK 

		 END DO ! Zone = 1,Ground

         ELSE

         MAINTR(Stalk) = 0

           DO Zone = 1,3 
	       LeafMaint(Stalk,Zone) = 0
		 END DO ! Zone = 1,Ground

        END IF
	
	END DO  
!-----------------------------------------------------------------------
      RETURN
      END ! SUBROUTINE RESPIR
!-----------------------------------------------------------------------
!     RESPIR variables:
!-----------------------------------------------------------------------
! LeafMaint(j)  Maintenance respiration for leaves only 
!                 (g[CH2O] / stalk j / d)
! MAINTR(j) Maintenance respiration for entire stalk (g[CH2O] / stalk j / d)
! PG      Daily gross photosynthesis (g[CH2O] / m2 / d)
! R30C2   Respiration coefficient that depends on gross photosynthesis, 
!           value at 30C (g[CH2O] used / g[CH2O] fixed / hr)
! RES30C  Respiration coefficient that depends on total plant mass,
!           value at 30C (g CH2O/g DW/hr)
! RO      Respiration coefficient that depends on total plant mass
!           (g[CH2O] / g[tissue])
! RP      proportion of the day's photosynthesis which is respired in the 
!           maintenance process 
! StalkState(j,k)  Condition k of stalk j.  Currently, conditions are:
!                  k = 1 (LIVE or DEAD); 2 (PRIM or TILR); 3-10 unused          
! StkPG(j) Daily gross photosynthesis for stalk j (g[CH2O] / stalk j / d) 
! TOTALWT(j) Total weight of sugarcane stalk j, including a proportion of 
!            root tissue   (g[tissue] / stalk j)
! TOTWT   Mass of tissue assumed to require maintenance (g[tissue] / m2)
! TRSFAC  Temperature effect on maintenance respiration 
! TS      Number of intermediate time steps (=24) 
!-----------------------------------------------------------------------
!     END SUBROUTINE RESPIR
!-----------------------------------------------------------------------
