!=======================================================================
!  P_Uptake
!  Generic P-uptake routine.
!  Calculates the P uptake and P stress by a crop.
!-----------------------------------------------------------------------
!  Revision History
!  ../../.... AG  Written as part of subroutine CHEM_PAR for the P model
!                 of Daroub et al, 2003.
!  10/20/2003 AJG Brought into modular format and linked to the 
!                 CENTURY-based SOM/residue module.
!  12/17/2003 AJG Renamed all the CH_ and CHEM_ variables to P_ variables.
!  03/23/2004 CHP Separated out the uptake portion of the code to make
!                   generic routine for all crops.
!  01/24/2005 CHP Revised for Root-volume approach
!-----------------------------------------------------------------------
!  Called by: P_CGRO, P_CERES, .....
!  Calls:     None
!=======================================================================
      SUBROUTINE P_Uptake (DYNAMIC,  
     &    N2P_min, PCNVeg, PConc_Veg, PTotDem,            !Input
     &    RLV, SPi_AVAIL,                                 !Input
     &    N2P, PUptake, PUptakeProf)                      !Output

!     ------------------------------------------------------------------
      USE ModuleDefs 
      USE ModuleData
      IMPLICIT  NONE
      EXTERNAL WARNING
      SAVE
!     ------------------------------------------------------------------
!      CHARACTER*1  ISWPHO
      CHARACTER*78 MSG(3)
      INTEGER DYNAMIC, L, NLAYR   !, LUN

      REAL PTotDem, P_SUPPLYprof, PUptakeProf, WF_PHOS    !FracPUptake, 
!      REAL P_SUPPLYnew, P_SUPPLYprof_old, P_SUPPLYprof_new
      REAL N2P, N2P_min, PCNVeg, PConc_Veg, PUp_reduce
      REAL, DIMENSION(NL) :: DLAYR, DS, DUL, LL, P_SUPPLY, PUptake
      REAL, DIMENSION(NL) :: RLV, SAT, SPi_AVAIL 

!     TEMPORARY FOR PRINTOUT
!      TYPE (ControlType) CONTROL
      TYPE (SoilType) SOILPROP
!      LOGICAL FEXIST 
!      INTEGER ERRNUM, YEAR, DOY 
      REAL RLVTOT               

!***********************************************************************
!***********************************************************************
!     SEASONAL INITIALIZATION: run once per season.
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!     ------------------------------------------------------------------
!     Initialize local variables.
      PUptakeProf = 0.0
      PUptake    = 0.0
      WF_PHOS    = 0.0   

!      IF (ISWPHO == 'N') RETURN 
!
!!     Temporary output file
!!     If this is made permanent, need to tie to IDETP
!      INQUIRE (FILE = 'PUPTAKE.OUT', EXIST = FEXIST)
!      CALL GETLUN('PUPTAKE.OUT',LUN)
!      IF (FEXIST) THEN
!        OPEN (UNIT = LUN, FILE = 'PUPTAKE.OUT', STATUS = 'OLD',
!     &    IOSTAT = ERRNUM, POSITION = 'APPEND')
!      ELSE
!        OPEN (UNIT = LUN, FILE = 'PUPTAKE.OUT', STATUS = 'NEW',
!     &    IOSTAT = ERRNUM)
!        WRITE(LUN,'(
!     & "*Plant phosphorus uptake output (TEMPORARY)")')
!      ENDIF
!      CALL GET(CONTROL)
!      CALL HEADER(SEASINIT, LUN, CONTROL%RUN)
!      WRITE(LUN,'(/,A)') 
!     &    '@YEAR DOY   DAS      PDEM      PUPT      RLVT'

!***********************************************************************
!***********************************************************************
!     DAILY INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
!     P supply.
!     ------------------------------------------------------------------
      PUptake = 0.0
      P_SUPPLYprof = 0.0
!      P_SUPPLYprof_old = 0.0
!      P_SUPPLYprof_new = 0.0

!     If there is no demand, then return
      IF (PTotDem < 1.E-4) RETURN

      CALL GET(SOILPROP)
      DS    = SOILPROP % DS
      DUL   = SOILPROP % DUL
      DLAYR = SOILPROP % DLAYR
      NLAYR = SOILPROP % NLAYR
      LL    = SOILPROP % LL
      SAT   = SOILPROP % SAT
      
!!*******************************************************************
!!     CHP 03/16/2005
!!     This is the old algorithm for comparison.  This section will
!!     be eliminated eventually.  At that time, P_Labile will no longer
!!     be needed as an input to the plant P module.
!                                                                       !
!      DO L = 1, NLAYR
!
!!  This algorithm could be replaced with a Michaelis-Menton type       !
!!      equation. Something like:  (check N uptake routine)             !
!!    P_supply(L) = Kmax * Pi_avail(L) / (K50 + Pi_avail(L)) * RLV * RWU!
!!                                                                      !
!!     The DLAYR(1) / DLAYR(L) ratio in the following WEIGHT eqn        !
!!     could erroneously impact uptake at lower layers.                 !
!                                                                       !
!!       Water factor to limit P uptake.                                !
!!was:         chp 12/6/2004                                            !
!!        WF_PHOS = AMIN1 (1.0, 1.0 / 0.005) * RWU(L) / DLAYR(L)        !
!        WF_PHOS = AMIN1 (1.0, 1.0 / 0.005 * RWU(L) / DLAYR(L))          
!                                                                       !
!!       Root density factor.                                           !
!! WAS        RDF(L) = DLAYR(1) / DLAYR(L) * (1.0 - EXP(-6.0 * RLV(L))) !
!!        RDF_OLD(L) = DLAYR(1) / DLAYR(L) * (1.0 - EXP(-6.0 * RLV(L))) !      
!        RDF(L) = DS(1) / DS(L) * (1.0 - EXP(-6.0 * RLV(L)))          !  
!!     According to Samira's paper, this weighting factor should be     !
!!     using DS instead of DLAYR (chp 10/19/2004):                      !
!!     "A root density factor (RDF) is calculated that is inversely     !
!!     proportional to the depth to the bottom of the layer (cm)        !
!!     and increases with RLV (cm / cm3)"                               !
!                                                                       !
!!       Weighting factor.                                              !
!        WEIGHT_1(L) = AMIN1 (WF_PHOS, RDF(L))       
!                                                                       !
!!       P supply of this soil layer. kg/ha                             !
!!WAS        P_SUPPLY(L) = Pi_AVAIL(L) * WEIGHT_1(L) / KG2PPM(L)        !
!             ! kg/ha           ppm         non-dim     conversion      !
!        P_SUPPLYold(L) = SPi_Labile(L) * WEIGHT_1(L)                    
!                                                                       !
!!       Sum the supply by all the layers to get the total supply.       
!        P_SUPPLYprof_old = P_SUPPLYprof_old + P_SUPPLYold(L) 
!      ENDDO
!
!!     End of old algorithm
!!*******************************************************************

!-----------------------------------------------------------------------
!     Revised uptake -- all of labile P in soil adjacent to roots is
!     available for uptake.  1/22/2005
!     5/10/2005 Use FracPUptake to limit uptake to this fraction of available
!      P_SUPPLYprof_new = 0.0
      P_SUPPLY = 0.0
      P_SUPPLYprof = 0.0
      DO L = 1, NLAYR
        IF (RLV(L) > 1.E-6) THEN
          P_SUPPLY(L) = SPi_AVAIL(L)     !kg/ha by layers
          P_SUPPLYprof = P_SUPPLYprof + P_SUPPLY(L)
        ENDIF
      ENDDO

!-----------------------------------------------------------------------
!       P uptake.
!-----------------------------------------------------------------------
!     Actual uptake. kg/ha
      PUptakeProf  = AMIN1(P_SUPPLYprof, PTotDem)

!     Set uptake from each layer to zero every day.
      PUptake = 0.0

      IF (PUptakeProf > 1.E-5) THEN
        IF (ABS(PUptakeProf - P_SUPPLYprof) < 1.E-5) THEN
          PUptake = P_SUPPLY
        ELSE
          DO L = 1, NLAYR
            PUptake(L) = P_SUPPLY(L) * PUptakeProf / P_SUPPLYprof
          END DO
        ENDIF
      ENDIF

!     Temporary print of old and new methods:
      RLVTOT = 0.0
      DO L = 1, NLAYR
        RLVTOT = RLVTOT + RLV(L) * DLAYR(L)
      ENDDO
      RLVTOT = RLVTOT / DS(NLAYR)

!!     Temporary print
!      CALL GET(CONTROL)
!      CALL YR_DOY(CONTROL%YRDOY, YEAR, DOY) 
!      WRITE(LUN,'(1X,I4,1X,I3.3,1X,I5,4F10.4)') YEAR, DOY, CONTROL%DAS, 
!     &        PTotDem, PUptakeProf, RLVTOT

!-----------------------------------------------------------------------
!     Reduce uptake if N:P ratio is below minimum
      IF (PConc_Veg > 1.E-6) THEN     !PConc_Veg from yesterday
        N2P = PCNVEG / PConc_Veg      !PCNVeg from today     
      ELSE
        N2P = -99.
      ENDIF                           

!     Calculate P uptake reduction factor:
      IF (N2P > 1.E-6 .AND. N2P < N2P_min) THEN
        PUP_Reduce = N2P / N2P_min
        WRITE(MSG(1),100) N2P, N2P_min
        WRITE(MSG(2),110) PUP_Reduce
  100   FORMAT('N:P ratio of ',F5.1,' is below minimum of ',F5.1,'.')
  110   FORMAT
     &    ('Daily P uptake will be reduced by a factor of ',F5.2,'.')
        CALL WARNING(2, 'PUPTAK', MSG)
        PUptake     = PUP_Reduce * PUptake
        PUptakeProf = PUP_Reduce * PUptakeProf
      ENDIF

!     Eventually, calculate N uptake reduction factor:
!     IF (N2P > N2P_max) THEN
!       NUPTRF = NP2_max / N2P
!     ELSE
!       NUPTRF = 1.0
!     ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------

      RETURN
      END SUBROUTINE P_Uptake

!-----------------------------------------------------------------------
! P_Uptake Variables - updated 9/29/2004
!-----------------------------------------------------------------------
! DLAYR(L)     Thickness of soil layer L (cm)
! KG2PPM(L)    Conversion factor to switch from kg [N] / ha to µg [N] / g 
!                [soil] for soil layer L 
! NL           Maximum number of soil layers = 20 
! NLAYR        Actual number of soil layers 
! P_SUPPLY(L)  Potential uptake of P in soil layer, L (kg[P]/ha)
! P_Supply_Tot Total P in soil profile available for uptake by roots
!               (kg[P]/ha)
! Pi_AVAIL(L)  Soil P which is available for uptake by plants. (ppm)
! PTotDem      Total daily plant demand for P (kg[P]/ha)
! PUptake(L)   Plant uptake of P in soil layer L (kg[P]/ha/d)
! PUptakeProf   Plant uptake of P over whole soil profile (kg[P]/ha/d)
! RLV(L)       Root length density for soil layer L (cm[root] / cm3[soil])
! RWU(L)       Root water uptake from soil layer L (cm/d)
! WEIGHT_1     Weighting factor for uptake of P by roots by soil layer 
! WF_PHOS      Water factor to limit P uptake by roots 
!-----------------------------------------------------------------------
