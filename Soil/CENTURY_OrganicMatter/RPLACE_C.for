!***********************************************************************
!  RPLACE_C, Subroutine for CENTURY-based SOM/residue module.
!
!  Purpose: Deals with residue placement.  Actual residue placement
!     amounts are computed in OM_Place.
!
!  Revision       History
!  11/21/1995 PWW Written.
!  06/08/1999 CHP Modified for modular format
!  03/01/2004 CHP Modified to accept organic matter placement information 
!                 from OM_Place and to distribute to correct pools.
!
!  Called : Century
!  Calls  : PARTIT_C
!=======================================================================

      SUBROUTINE RPLACE_C (CONTROL,
     &  AMINRL, CEDAM, CESTR, DLAYR, FRDAE,               !Input
     &  FRMETI, FRMETS, N_ELEMS, NLAYR, OMADATA,          !Input
     &  RESDAX,                                           !Input
     &  DLTLIGC, DLTMETABC, DLTMETABE, DLTSTRUCC,         !Input/Output
     &  DLTSTRUCE, IMMOB, MULCH,                          !Input/Output
     &  ADDMETABEFLAG, FRMETFLAG)                         !Output

!     ------------------------------------------------------------------
      USE ModuleDefs
      USE Interface_IpSoil

      IMPLICIT  NONE
      EXTERNAL PARTIT_C
      SAVE
!     ------------------------------------------------------------------

      LOGICAL ADDMETABEFLAG, FRMETFLAG

      CHARACTER*5 RESTYPE
!      CHARACTER*6 ERRKEY
!      PARAMETER (ERRKEY = 'RPLACE')

      INTEGER IEL, L, LAYER, N_ELEMS, NLAYR
      INTEGER, PARAMETER :: SRFC = 0

      REAL FRMETI, FRMETS, RESDAX, 
     &  RESSOL, RESSRF, TotResWt, Res_AM, EXTFAC, RES_WATFAC

      REAL CEDAM(NELEM), CESTR(NELEM), DLAYR(NL), FRDAE(NELEM)

      REAL, DIMENSION(0:NL) :: DLTLIGC, DLTMETABC, DLTSTRUCC, FRLRES, 
     &    RESC, ResLig, ResWt
      REAL, DIMENSION(NL,NELEM) :: AMINRL
      REAL, DIMENSION(0:NL,NELEM) :: RESE, DLTMETABE, DLTSTRUCE,
     &  RESCE, IMMOB

!     Constructed variables are defined in ModuleDefs.
      TYPE (OrgMatAppType) OMAData    !Organic matter application
      TYPE (MulchType)     MULCH
      TYPE (ControlType)   CONTROL

!!======================================================================
!!     Organic Matter Application data
!      TYPE OrgMatAppType
!        INTEGER NAPRes, ResDat, ResDepth
!        CHARACTER (len=5) RESTYPE
!        REAL ResMixPerc   !Percent mixing rate for SOM module
!
!        REAL, DIMENSION(0:NL) :: ResWt        !kg[dry matter]/ha/d
!        REAL, DIMENSION(0:NL) :: ResLig       !kg[lignin]/ha/d
!        REAL, DIMENSION(0:NL,NELEM) :: ResE   !kg[E]/ha/d (E=N, P, ..)
!        REAL  CumResWt                        !cumul. kg[dry matter]/ha
!        REAL, DIMENSION(NELEM) :: CumResE     !cumulative kg[E]/ha
!      END TYPE OrgMatAppType
!!======================================================================

      ResType  = OMAData % ResType
      ResE     = OMAData % ResE
      ResWt    = OMAData % ResWt
      ResLig   = OMAData % ResLig

      RESSRF = ResWt(SRFC)

!     Get surface coverage for this residue type
      CALL IPSOIL (CONTROL, RESTYPE=ResType,AM=RES_AM,WATFAC=RES_WATFAC,
     &  EXTFAC=EXTFAC)
      IF (Mulch%MulchMass + RESSRF > 1.E-6) THEN
        Mulch % Mulch_AM = (Mulch % Mulch_AM * Mulch % MulchMass 
     &   + RES_AM * RESSRF) / (Mulch%MulchMass + RESSRF)
        Mulch % Mul_EXTFAC = (Mulch % Mul_EXTFAC * Mulch % MulchMass 
     &   + EXTFAC * RESSRF) / (Mulch%MulchMass + RESSRF)
        Mulch % Mul_WATFAC = (Mulch % Mul_WATFAC * Mulch % MulchMass 
     &   + RES_WATFAC * RESSRF) / (Mulch%MulchMass + RESSRF)
        Mulch % NewMulch = RESSRF
      ELSE
        Mulch % Mulch_AM   = RES_AM
        Mulch % Mul_WATFAC = RES_WATFAC
        Mulch % Mul_EXTFAC = EXTFAC
        Mulch % NewMulch   = 0.0
      ENDIF

      RESSOL = 0.0
      DO L = 1, NLAYR
        RESSOL = RESSOL + ResWt(L)
      ENDDO
      TotResWt = RESSOL + RESSRF

!======================================================================
!***********************************************************************
!***********************************************************************
!     ----------------------------------------------------------------
!     C and N contribution of the applied residue.
!     ----------------------------------------------------------------
!     Surface-applied residue
!     -----------------------
!     Calculate the C and N contribution of the residue that is newly
!     added to the surface layer. Set the residue's C content (= 40% of
!     the residue amount), C/E ratio and lignin concentration.
      IF (RESSRF > 0.) THEN
        RESC(SRFC) = RESSRF * 0.40

        DO IEL = 1, N_ELEMS
          IF (RESE(SRFC,IEL) .GT. 1.E-6) THEN   
            RESCE(SRFC,IEL) = RESC(SRFC) / RESE(SRFC,IEL)
          ELSE
            RESCE(SRFC,IEL) = 0.0
          ENDIF
        ENDDO

!       Lignin fraction 
        FRLRES(SRFC) = ResLig(SRFC) / ResWt(SRFC)

!       If L is transferred via the subroutine's parameter
!       string, it goes wrong, because L is the DO loop counter.
!       Therefore, L is first copied to LAYER.
        LAYER = SRFC

!       Distribute the newly added residues over the SRFC structural
!       and metabolic residue pools.
        CALL PARTIT_C (
     &    AMINRL, CEDAM, CESTR, DLAYR, FRDAE,             !Input
     &    FRLRES, FRMETI, FRMETS, LAYER, N_ELEMS,         !Input
     &    RESC, RESCE, RESDAX,                            !Input
     &    DLTLIGC, DLTMETABC, DLTMETABE, DLTSTRUCC,       !Input/Output
     &    DLTSTRUCE, IMMOB, RESE,                         !Input/Output
     &    ADDMETABEFLAG, FRMETFLAG)                       !Output
      ENDIF   !End of IF block on RESSRF>0.001

!     ------------
!     Soil residue
!     ------------
!     If there are soil-deposited residues, which are incorporated (and
!     not left on top of the soil, which would make them not soil-
!     deposited).
      IF (RESSOL > 0.001) THEN 
        DO L = 1, NLAYR
          RESC(L) = ResWt(L) * 0.40

          DO IEL = 1, N_ELEMS
            IF (RESE(L,IEL) > 0.) THEN  
              RESCE(L,IEL) = RESC(L) / RESE(L,IEL)
           ELSE
              RESCE(L,IEL) = 0.0
            ENDIF
          ENDDO

!         Lignin fraction 
          IF (ResWt(L) > 1.E-5) THEN
            FRLRES(L) = ResLig(L) / ResWt(L)
          ELSE
            FRLRES(L) = 0.
          ENDIF

!         If L is transferred via the subroutine's parameter
!         string, it goes wrong, because L is the DO loop counter.
!         Therefore, L is first copied to LAYER.
          LAYER = L

!         Distribute the newly added residues over the soil 
!         structural and metabolic residue pools.
          CALL PARTIT_C (
     &      AMINRL, CEDAM, CESTR, DLAYR, FRDAE,           !Input
     &      FRLRES, FRMETI, FRMETS, LAYER, N_ELEMS,       !Input
     &      RESC, RESCE, RESDAX,                          !Input
     &      DLTLIGC, DLTMETABC, DLTMETABE, DLTSTRUCC,     !Input/Output
     &      DLTSTRUCE, IMMOB, RESE,                       !Input/Output
     &      ADDMETABEFLAG, FRMETFLAG)                     !Output
        ENDDO   !End of loop on soil layers.
      ENDIF   !End of IF block on RESSOL and RESDEPTH

!     ------------------------------------------------------------------

      RETURN
      END SUBROUTINE RPLACE_C

!=======================================================================
