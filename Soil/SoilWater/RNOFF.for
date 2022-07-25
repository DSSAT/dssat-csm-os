C=======================================================================
C  RNOFF, Subroutine, J.T. Ritchie
C  Calculate runoff by Williams-SCS curve number (CN) technique.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1989 JR  Written
C  10/01/1987 GH  Correction of RUNOFF calc. according to SCS curve no.
C  12/05/1993 NBP Made into subroutine
C  07/12/1996 GH  Changed Precipitation to RAIN
C  07/01/1997 BDB Changed CN and SMX initialization (removed CN1, CN2,
C                 CN3, WF, WX, XX) from old INSOIL code
C  07/01/1997 BDB Simplified CN calculations (Removed C1, C2, C3, WF, DUL
C                  and made SMX input variables) (old WBSUBS code)
C  10/11/1997 CHP Updated for modular format.
C  09/01/1999  GH Incorporated into CROPGRO
!  06/12/2007 CHP Increase initial abstraction if mulch layer present.
!  07/22/2022  FO Updated 'PMFRAC' to 'PMFRACTION' to avoid get warning
!-----------------------------------------------------------------------
!  Called by: WATBAL
!  Calls:     None
C=======================================================================
      SUBROUTINE RNOFF( 
     &    CN, LL, MEINF, MULCH, SAT, SW, WATAVL,          !Input
     &    RUNOFF)                                         !Output

C-----------------------------------------------------------------------
      USE ModuleDefs
      USE ModuleData
      IMPLICIT NONE
      SAVE

      CHARACTER*1 MEINF
      CHARACTER*6 ERRKEY
      PARAMETER (ERRKEY = 'RNOFF')

      REAL CN, IABS
      REAL PB, WATAVL, SMX
      REAL RUNOFF, SWABI

!     Maximum initial abstraction ratio
      REAL, PARAMETER :: MAXIABS = 0.6

!     Max number of soil layers defined in ModuleDefs.for
      REAL LL(NL), SAT(NL), SW(NL)

!     Mulch layer
      Type (MulchType) MULCH
      
!     Plastic Mulch
      REAL PMFRACTION

!!     Temporary for printing
!      INTEGER DOY, YEAR, LUN
!      REAL CUMRO
!      Type (ControlType) CONTROL
!
!      CALL GET(CONTROL)
!      CALL YR_DOY(CONTROL.YRDOY, YEAR, DOY)
!      if (control.das == 0) then
!        if (control.run == 1) then
!          CALL GETLUN('OUTRO', LUN)
!          OPEN (UNIT = LUN, FILE = 'Runnoft.OUT', STATUS = 'replace')
!          WRITE(LUN,'("*Surface Water Runoff Daily output (temp)")')
!        endif
!        call header(SEASINIT, LUN, control.RUN)
!        WRITE (LUN,1120)
! 1120   FORMAT('@YEAR DOY   DAS     SMX  WATAVL   SWABI',
!     &  '   COVER    IABS      PB   RNOFF   CUMRO')
!        CUMRO = 0.0
!      endif

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS
!***********************************************************************
!     ----------------------------------------------
!     Soil storage 
      SMX = 254.0 * (100.0/CN - 1.0)

!     ----------------------------------------------
!     Initial abstraction ratio
!     Runoff is related to the average soil water content of the top
!     two layers of soil
      SWABI = 0.15 * ((SAT(1) - SW(1)) / (SAT(1) - LL(1) * 0.5) +
     &              (SAT(2) - SW(2)) / (SAT(2) - LL(2) * 0.5))
      SWABI = MAX(0.0, SWABI)

!     05/08/2006 CHP increase initial abstraction if surface mulch is
!     present. Initial abstraction ratio increases from SWABI
!     (no-mulch conditions) to 0.6 (MAXIABS) at full mulch coverage. 
!     IF (INDEX('RSN',MEINF) .LE. 0) THEN
      IF (INDEX('RSM',MEINF) > 0) THEN   
!       Model effects of mulch on runoff
        IABS = SWABI + (MAXIABS - SWABI) * MULCH % MULCHCOVER
        IABS = MAX(SWABI, IABS)
      ELSE
!       No mulch effects on runoff
        IABS = SWABI
      ENDIF
      
      PB = WATAVL - IABS * SMX
      
      IF (WATAVL .GT. 0.001) THEN
        IF (PB .GT. 0) THEN
          RUNOFF = PB**2/(WATAVL + (1.0-IABS) * SMX)
        ELSE
          RUNOFF = 0.0
        END IF
      ELSE
        RUNOFF = 0.0
      ENDIF
      
      CALL GET("PM","PMFRACTION",PMFRACTION)
      IF (PMFRACTION .GT. 1.E-6) THEN
          RUNOFF = WATAVL * PMFRACTION + RUNOFF * (1 - PMFRACTION)
      ENDIF

!!     Temporary
!      CUMRO = CUMRO + RUNOFF
!      WRITE (LUN,1300)YEAR, DOY, CONTROL.DAS, 
!     &  SMX, WATAVL, SWABI, MULCH.MULCHCOVER, IABS, PB, RUNOFF, CUMRO
! 1300 FORMAT(1X,I4,1X,I3.3,1X,I5,2F8.1,3F8.3,3F8.2)

!***********************************************************************
      RETURN
      END SUBROUTINE RNOFF

!-----------------------------------------------------------------------
!     Variable definitions for RNOFF Module (updated 2/12/2004)
!-----------------------------------------------------------------------
! CN       Runoff Curve Number - measure of runoff potential based on soil 
!            type and current soil water content. 
! IABS     Initial abstraction ratio, modified for surface mulch layer effects.
! LL(L)    Volumetric soil water content in soil layer L at lower limit
!           (cm3 [water] / cm3 [soil])
! PB       Determines threshold amount of rainfall that will occur before 
!            runoff starts (mm/d)
! RUNOFF   Calculated runoff (mm/d)
! SAT(L)   Volumetric soil water content in layer L at saturation
!           (cm3 [water] / cm3 [soil])
! SMX      Soil storage available for surface water based on CN formula
!           (mm)
! SW(L)    Volumetric soil water content in layer L
!           (cm3 [water] / cm3 [soil])
! SWABI(L) A soil water abstraction index, a unitless indicator of the soil 
!            water condition at the time of a rainfall event.  This affects 
!            the intercept of the runoff axis when runoff starts to 
!            occur--later when drier and sooner when wetter.
! WATAVL   Water available for infiltration or runoff (rainfall plus 
!            irrigation) (mm/d)
!-----------------------------------------------------------------------
!     END SUBROUTINE RNOFF
!-----------------------------------------------------------------------


