C=======================================================================
C  CSP_HRes, Subroutine
C-----------------------------------------------------------------------
C  Determines harvest residue at end of season for CROPGRO crops.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  08/12/2002 CHP Written based on old OPSEQ and OPSEQ_C routines
C  03/12/2003 CHP Changed senescence variable to composite (SENESCE)
C                   as defined in ModuleDefs.for
C  12/16/2004 CHP/KJB/LAH changed residue to account for harvest
C                   fractions.
!  01/06/2005 CHP Added N_ELEMS to CONTROL variable to make it available here.
!  03/01/2005 CHP Only compute for sequenced runs.  Set to zero otherwise.
C  09/09/2005 FSR Renamed Hres_CGRO to CSP_Hres for use with CASUPRO.
C  09/09/2005 FSR Substituted RTWTHa(DAS) for RTWT.
!  11/04/2005 CHP Include P in harvest residue.
C  06/30/2006 FSR Updated to CSM v5
C=======================================================================

      SUBROUTINE CSP_HRes(CONTROL,
     &    CROP, DLAYR, DWNOD, HARVFRAC, NLAYR,            !Input
     &    PConc_Root, PConc_Shut, PLIGLF, PLIGNO, PLIGRT, !Input
     &    PLIGSD, PLIGSH, PLIGST, RLV, RTWTHa,            !Input
     &    SDWT, SENESCE, SHELWT, STMWT, WTLF,             !Input
     &    WTNLF,WTNNOD, WTNRT, WTNSD, WTNSH, WTNST,       !Input
     &    HARVRES)                                        !Output

!-------------------------------------------------------------------------
      USE ModuleDefs

      IMPLICIT NONE
      SAVE

!     Input variables
      CHARACTER*2 CROP
      INTEGER DAS, NLAYR, N_ELEMS
      REAL DWNOD, PLIGLF, PLIGNO, PLIGRT, PLIGSD, PLIGSH,     
     &     PLIGST, SDWT, SHELWT, WTLF, STMWT,
     &     WTNLF, WTNNOD, WTNRT, WTNSD, WTNSH, WTNST
      REAL LFRES, STMRES, SDRES, SHLRES
      REAL HARVFRAC(2)
      REAL, DIMENSION(NL) :: DLAYR, RLV
      REAL PConc_Shut, PConc_Root
      REAL, DIMENSION(0:NumOfDays) :: RTWTHa


!     Output variables
      Type (ResidueType) HARVRES
      Type (ResidueType) SENESCE
      TYPE (ControlType) CONTROL

!     Local Variables
      INTEGER IEL, L
      REAL TRTRES, TRTRESE(3), TRLV
      INTEGER, PARAMETER :: SRFC = 0   !, N = 1 

!     Harvest residue variables 0 = surface
      REAL HResWt(0:NL)       !Residue mass (kg[dry matter]/ha)
      REAL HResLig(0:NL)      !Residue lignin (kg[lignin]/ha)
      REAL HResE(0:NL,NELEM)  !Residue element components (kg[E]/ha)

!     Transfer values from constructed data types into local variables.
      DAS     = CONTROL % DAS
      N_ELEMS = CONTROL % N_ELEMS

!-------------------------------------------------------------------------
      HResWt  = 0.0   
      HResLig = 0.0
      HResE   = 0.0

!     No residue left in fallow field.  
!     Residue only computed for sequenced runs.
      IF (CROP /= 'FA' .AND. INDEX('QF',CONTROL%RNMODE) > 0) THEN

!-------------------------------------------------------------------------
!       Shoot residues.
!       ---------------
!       Residue weight equals top weight minus seed and by-product.
!        HResWt(SRFC) = TOPWT - SDWT - BWAH
!                      g/m2    g/m2   g/m2

!       By-product not harvested:
!       TOPRES = (TOPWT - SDWT) * 10. * (1. - HARVFRAC(2)) 
!       08/11/2005 GH/CHP
        LFRES  = AMAX1(0.0, WTLF  * 10. * (1. - HARVFRAC(2)))
        STMRES = AMAX1(0.0, STMWT * 10. * (1. - HARVFRAC(2)))

!       Add weight of seeds not harvested
        SDRES = AMAX1(0.0, SDWT * 10. * (1. - HARVFRAC(1)))

!       Add weight of shells (100% assumed to remain in field)
        SHLRES = AMAX1(0.0, SHELWT * 10.)

        HResWt(SRFC) = LFRES + STMRES + SDRES + SHLRES

!       N in residue 
        HResE(SRFC,N) = (WTNLF + WTNST) * 10. * (1. - HARVFRAC(2)) 
     &                + WTNSD * 10. * (1.- HARVFRAC(1))
     &                + WTNSH * 10.
        IF (N_ELEMS > 1) THEN
!         P in residue 
          HResE(SRFC,P) = PConc_Shut * (LFRES + STMRES)
!     &                + PConc_Seed * SDRES   removed from Sugarcane 
!     &                + PConc_Shel * SHLRES  (FSR) 
        ENDIF

!       Senescence has been added daily (subroutine SENESADD), so no need
!       to add it here as WTLO, WTSO, WTSHO, WTSDO, as in the CERES-based
!       module).

        IF (HResWt(SRFC) .GT. 1.E-4) THEN
!         The lignin concentration of the various plant parts varies
!         widely: usually stem and leaf are similar, and so are seed and
!         shell. So separate those here. Correct VEGRES for non-harvested
!         seeds and shells, and add those separately with their own lignin
!         concentration.
          HResLig(SRFC) = (LFRES * PLIGLF + STMRES * PLIGST +
     &      SDRES * PLIGSD + SHLRES * PLIGSH) / HResWt(SRFC)
        ENDIF 

!-------------------------------------------------------------------------
!       Distribute root+nodule residues by layer, according to the root
!       length distribution (NB: this may give very wrong results if the
!       roots vary in thickness by layer, but root weight is not available
!       by layer).
        TRLV = 0.
        DO L = 1, NLAYR
          TRLV = TRLV + RLV(L) * DLAYR(L)
        END DO

!       Root + nodule residues.
!       -----------------------
!       Total root residues (whole profile) equal root weight plus nodule
!       weight.
!       TRTRES = RTWT + DWNOD    Replaced with following by FSR 09-09-05  
        TRTRES = RTWTHa(DAS)    ! 

!       N in root residues is N in roots plus N in nodules.
        TRTRESE(N) = WTNRT + WTNNOD
        TRTRESE(P) = PConc_Root * RTWTHa(DAS) !Check whether this
                   ! substitute for RTWT works as-is (FSR).

!       Senescence has been added daily (subroutine SENESADD), so no need
!       to add it here as WTRO and WTNOO, as in the CERES-based module.
        IF (TRLV > 1.E-6 .AND. TRTRES > 1.E-6) THEN
          DO L = 1, NLAYR
            HResWt(L)  = 10. * TRTRES * RLV(L) * DLAYR(L) / TRLV
            HResLig(L) = (RTWTHa(DAS) * PLIGRT + DWNOD * PLIGNO)/ TRTRES 
     &                            * RLV(L) * DLAYR(L) / TRLV
            DO IEL = 1, N_ELEMS
              HResE(L,IEL) = 10. * TRTRESE(IEL) * RLV(L) * DLAYR(L)/TRLV
            ENDDO
          END DO   !End of soil layer loop.
        ELSE
          DO L = 1, NLAYR
            HResWt(L)  = 0.
            HResLig(L) = 0.
            HResE(L,N) = 0.
            HResE(L,P) = 0.
          ENDDO
        ENDIF

C-------------------------------------------------------------------------
        !Add in last day of senesced plant material (not added in soil
        !  module because it is computed after soil integration.
        HResWt(SRFC)  = HResWt(SRFC)  + SENESCE % ResWt(SRFC)
        HResLig(SRFC) = HResLig(SRFC) + SENESCE % ResLig(SRFC)
        HResE(SRFC,N) = HResE(SRFC,N) + SENESCE % ResE(SRFC,N)
        HResE(SRFC,P) = HResE(SRFC,P) + SENESCE % ResE(SRFC,P)

        DO L = 1, NLAYR
          HResWt(L)  = HResWt(L)  + SENESCE % ResWt(L)
          HResLig(L) = HResLig(L) + SENESCE % ResLig(L)
          HResE(L,N) = HResE(L,N) + SENESCE % ResE(L,N)
          HResE(L,P) = HResE(L,P) + SENESCE % ResE(L,P)
        ENDDO

      ENDIF   !Crop .NE. 'FA'
C-----------------------------------------------------------------------
!     Transfer results to constructed variable
      HARVRES % ResWt  = HResWt
      HARVRES % ResLig = HResLig
      HARVRES % ResE   = HResE

      RETURN
      END SUBROUTINE CSP_HRes

C-----------------------------------------------------------------------
! Variable Definitions (11 March 2004)
C-----------------------------------------------------------------------
! BWAH       Weight of by-product not harvested (top weight minus seed 
!              weight) (g/m2)
! CONTROL    Composite variable containing variables related to control 
!              and/or timing of simulation.    See Appendix A. 
! CROP       Crop identification code 
! DWNOD      Current nodule mass (g[nodule] / m2)
! HARVRES    Composite variable containing harvest residue amounts for 
!              total dry matter, lignin, and N amounts.  Structure of 
!              variable is defined in ModuleDefs.for. 
! HRESE(L,E) Amount of element E in plant residue left in field after 
!              harvest in soil/surface layer L (E=1 for nitrogen; E=2 for 
!              phosphorus; . . .) (kg[E]/ha (E=N, P, S,...))
! HRESLIG(L) Amount of lignin in plant residue left in field after harvest 
!              in soil/surface layer L (kg[lignin]/ha)
! HRESWT(L)  Amount of plant residue left in field after harvest in 
!              soil/surface layer L (kg[dry matter]/ha)
! NLAYR      Actual number of soil layers 
! PLIGLF     Proportion of leaf tissue that is lignin (fraction)
! PLIGNO     Proportion of nodule tissue that is lignin (fraction)
! PLIGRT     Proportion of root tissue that is lignin (fraction)
! PLIGSD     Proportion of seed tissue that is lignin (fraction)
! PLIGSH     Proportion of shell tissue that is lignin (fraction)
! PLIGST     Proportion of stem tissue that is lignin (fraction)
! PRCEL      Cellulose fraction of the residue (fraction)
! PRCHO      Carbohydrate fraction of the residue (fraction)
! RLV(L)     Root length density for soil layer L (cm[root] / cm3[soil])
! RTWTHa(i)  Root weight (including C and N) per ground area on day i     
!              kg[root tissue] / ha
! RTWT       Dry mass of root tissue, including C and N
!             (g[root] / m2[ground])
! SDWT       Dry mass of seed tissue, including C and N
!             (g[seed] / m2[ground])
! SDWTAH     Actual seed weight harvested (g[seed] / m2[ground])
! SENESCE    Composite variable containing data about daily senesced plant 
!              matter. Structure of variable is defined in ModuleDefs.for 
! SHELWT     Total mass of all shells (g / m2)
! TRLV       Total root length per square cm soil today
!             (cm[root] / cm2[ground)
! TRTRES     Weight of root residues from previous crop
!             (kg [dry matter] / ha)
! TRTRESE(E) Amount of element E in root/nodule residue left in field after 
!              harvest for soil profile (E=1 for nitrogen; E=2 for 
!              phosphorus; . . .) (g[E]/m2)
! WTNLF      Mass of N in leaves (g[leaf N] / m2[ground])
! WTNNOD     Mass of N in nodules (g[N] / m2[ground])
! WTNRT      Mass of N in roots (g[root N] / m2[ground])
! WTNSD      Mass of N in seeds (g[N] / m2[ground])
! WTNSH      Mass of N in shells (g[N] / m2[ground])
! WTNST      Mass of N in stems (g[stem N] / m2[ground])
C=======================================================================
