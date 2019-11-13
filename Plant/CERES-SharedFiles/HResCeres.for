C=======================================================================
C  HRes_Ceres, Subroutine
C-----------------------------------------------------------------------
C  Determines harvest residue at end of season for Ceres crops.
C     Maize, millet, sorghum, rice, wheat(?).
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  08/16/2002 CHP Written based on old OPSEQ routine
C  03/12/2003 CHP Changed senescence variable to composite (SENESCE)
C                   as defined in ModuleDefs.for
C  12/16/2004 CHP/KJB/LAH changed residue to account for harvest
C                   fractions.
!  07/21/2005 CHP Use PODWT instead of SDWT to calculated unharvested
!                   plant mass.
!  08/02/2005 CHP Only compute for sequenced runs.  Set to zero otherwise.
!  08/11/2005 CHP Further revise harvest residue computations - all
!                 cob/chaff assumed to be left in field.  Byproduct 
!                 harvested is STOVWT * Harvest %.
!  03/31/2006 CHP Lignin content from species file.
!  07/13/2006 CHP P model added.
C=======================================================================

      SUBROUTINE HRes_Ceres(CONTROL,
     &    CROP, DLAYR, GRNWT, HARVFRAC, NLAYR,            !Input
     &    PConc_Shut, PConc_Root, PConc_Shel,             !Input
     &    PConc_Seed, PLTPOP, PODWT, RLV, ROOTN,          !Input
     &    RTWT, SENESCE, STOVN, STOVWT, WTNSD,            !Input
     &    HARVRES)                                        !Output

      USE ModuleDefs
      USE Interface_SenLig_Ceres
      IMPLICIT NONE
      SAVE

!     Input variables
      TYPE (ControlType) CONTROL
      Type (ResidueType) SENESCE
      CHARACTER*2 CROP
      INTEGER NLAYR
      REAL GRNWT, PLTPOP, PODWT, ROOTN, RTWT, STOVN, STOVWT, WTNSD
      REAL TOPRES, GRNRES, COBRES
      REAL HARVFRAC(2)
      REAL, DIMENSION(NL) :: DLAYR, RLV

!     Output variables
      Type (ResidueType) HARVRES

!     Local Variables
      INTEGER IEL, L, N_ELEMS
      REAL TRTRES, TRTRESE(3), TRLV
      INTEGER, PARAMETER :: SRFC = 0

!     Harvest residue variables 0 = surface
      REAL HResWt(0:NL)   !Residue mass (kg[dry matter]/ha)
      REAL HResLig(0:NL)  !Residue lignin (kg[lignin]/ha)
      REAL HResE(0:NL,NELEM)  !Residue element components (kg[E]/ha)

!     Lignin content
      REAL PLIGLF, PLIGST, PLIGRT, PLIGSH, PLIGSD

!     P variables
      REAL PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed

      N_ELEMS = CONTROL % N_ELEMS

C-----------------------------------------------------------------------
      HResWt  = 0.0   
      HResLig = 0.0
      HResE   = 0.0

!     No residue left in fallow field.  
!     Residue only computed for sequenced runs.
      IF (CROP /= 'FA' .AND. INDEX('QF',CONTROL%RNMODE) > 0) THEN

      CALL SenLig_Ceres(PLIGLF, PLIGST, PLIGRT, PLIGSH, PLIGSD)
C-----------------------------------------------------------------------
!       By-product not harvested:
        TOPRES = AMAX1(0.0, STOVWT * PLTPOP * 10. * (1. - HARVFRAC(2)))
!        kg/ha               g/pl    pl/m2              

!       Grain not harvested
        GRNRES = AMAX1(0.0, GRNWT * PLTPOP * 10. * (1. - HARVFRAC(1)))
!        kg/ha               g/pl   pl/m2  
            
!       Add cob (chaff) weight -- 100%.
        COBRES = AMAX1(0.0, (PODWT - GRNWT * PLTPOP)  * 10.)
!        kg/ha                g/m2    g/pl    pl/m2
!       chp added 12/21/2007 -- want to be able to remove ALL surface res.
        COBRES = COBRES * (1. - HARVFRAC(2))

        HResWt(SRFC)  = TOPRES + GRNRES + COBRES
        HResLig(SRFC) = TOPRES*PLIGLF + GRNRES*PLIGSD + COBRES*PLIGSH

        IF (STOVWT > 1.E-5) THEN
          HResE(SRFC,N) = STOVN * TOPRES / STOVWT
!           kg[N]/ha   =(g[N]/pl) * (kg[DW]/ha) / (g[DW]/pl)         
     &                  + WTNSD * 10. * (1. - HARVFRAC(1))
!                        g[N]/m2        
        ELSE
          HResE(SRFC,N) = WTNSD * 10. * (1. - HARVFRAC(1))
!           kg[N]/ha     g[N]/m2    
        ENDIF

        IF (N_ELEMS > 1) THEN
          HResE(SRFC,P) = PConc_Shut * TOPRES 
     &                  + PConc_Seed * GRNRES 
     &                  + PConc_Shel * COBRES
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
!       Total root residues (whole profile) equal root weight 
        TRTRES = RTWT * PLTPOP * 10.              !kg/ha
!             g/plant * plants/m2

!       N in root residues.
        TRTRESE(N) = ROOTN * PLTPOP * 10.       !kg/ha
!                 g[N]/plant * plants/m2  
        TRTRESE(P) = PConc_Root * TRTRES

!       Senescence has been added daily (subroutine SENESADD), so no need
!       to add it here as WTRO and WTNOO, as in the CERES-based module.
        IF (TRLV > 1.E-6) THEN
          DO L = 1, NLAYR
            HResWt(L) = TRTRES * RLV(L) * DLAYR(L) / TRLV
            HResLig(L) = HResWt(L) * PLIGRT
            DO IEL = 1, CONTROL % N_ELEMS
              HResE(L,IEL) = TRTRESE(IEL) * RLV(L) * DLAYR(L) / TRLV
            END DO   !End of IEL loop.
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

! chp 08/11/2005
!       Senesced leaf and stem may be harvested for byproduct.
        HResWt(SRFC)  = HResWt(SRFC)  + SENESCE % ResWt(SRFC) 
     &                    * (1. - HARVFRAC(2))
        HResLig(SRFC) = HResLig(SRFC) + SENESCE % ResLig(SRFC)
     &                    * (1. - HARVFRAC(2))
        HResE(SRFC,N) = HResE(SRFC,N) + SENESCE % ResE(SRFC,N)
     &                    * (1. - HARVFRAC(2))
        HResE(SRFC,P) = HResE(SRFC,P) + SENESCE % ResE(SRFC,P)
     &                    * (1. - HARVFRAC(2))

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
      END SUBROUTINE HRes_Ceres

