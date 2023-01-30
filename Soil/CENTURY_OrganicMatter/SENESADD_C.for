!***********************************************************************
!  SENESADD_C, Subroutine for CENTURY-based SOM/residue module of DSSAT.
!
!  Purpose: Add the senesced material daily to the residue pools for
!           the CENTURY-based SOM model.
!
!  REVISION HISTORY
!  01/01/1999 AJG Written.
!  03/26/2000 CHP/AJG Integrated the CENTURY-based and CERES-based SOM
!               modules with CHP's modular format.
!  03-12-2003 CHP Changed senescence variable to composite (SENESCE)
!                   as defined in ModuleDefs.for
!  10/17/2003 AJG Linked to the P module of Daroub et al.and removed the
!                 KEEPxxx variables.
!  11/17/2003 AJG Renamed all the CH_ variables to P_ variables.
!
!  Called: NTRANS_C
!  Calls : PARTIT_C
!***********************************************************************

      SUBROUTINE SENESADD_C (DYNAMIC, 
     &  AMINRL, CEDAM, CESTR, CROP, DLAYR, FRDAE, FRMETI, !Input
     &  FRMETS, N_ELEMS, NLAYR, RESDAX, SENESCE,          !Input
     &  ADDMETABEFLAG, DLTLIGC, DLTMETABC, DLTMETABE,     !Output
     &  DLTSTRUCC, DLTSTRUCE, FRMETFLAG, IMMOB, MULCH,    !Output
     &  SENESSUMC, SENESSUMN, SENESSUMP)                  !Output

!     ------------------------------------------------------------------
      USE ModuleDefs 
      USE ModuleData
      USE Interface_IpSoil

      IMPLICIT NONE
      EXTERNAL PARTIT_C
      SAVE

      LOGICAL ADDMETABEFLAG, FRMETFLAG

      CHARACTER*2  CROP

      INTEGER DYNAMIC, L, LAYER, N_ELEMS, NLAYR 
      INTEGER, PARAMETER :: SRFC = 0  !, N = 1, P = 2

      REAL ADDC, ADDLIG, ADDN, ADDP, FRMETI, FRMETS, RESDAX, SENSUM,
     &  SENESSUMC, SENESSUMN, SENESSUMP, SEN_AM, SEN_EXTFAC, SEN_WATFAC
      REAL CEDAM(NELEM), CESTR(NELEM), DLAYR(NL), DLTLIGC(0:NL),
     &  DLTMETABC(0:NL), DLTMETABE(0:NL,NELEM), 
     &  DLTSTRUCC(0:NL), FRDAE(NELEM),
     &  FRLRES(0:NL), RESC(0:NL), RESE(0:NL,NELEM)
      REAL AMINRL(NL,NELEM), DLTSTRUCE(0:NL,NELEM), RESCE(0:NL,NELEM)
      REAL IMMOB(0:NL,NELEM)
!-----------------------------------------------------------------------
!     Constructed variables are defined in ModuleDefs.
      TYPE (ResidueType) SENESCE  
      TYPE (ControlType) CONTROL
      TYPE (MulchType)   MULCH

!***********************************************************************
!***********************************************************************
!     RUN INITIALIZATION - CALLED ONCE PER SIMULATION
!***********************************************************************
      IF (DYNAMIC == SEASINIT) THEN
!     ------------------------------------------------------------------
!       Initialize the cumulative senescence for the new season at zero.
        SENESSUMC = 0.
        SENESSUMN = 0.
        SENESSUMP = 0.

!     Surface coverage characteristics for this crop residue 
      CALL GET(CONTROL)
      CALL IPSOIL(CONTROL,CROP=CONTROL%CROP,AM=SEN_AM,WATFAC=SEN_WATFAC,
     &   EXTFAC = SEN_EXTFAC)

!***********************************************************************
!***********************************************************************
!     RATE CALCULATIONS 
!***********************************************************************
      ELSEIF (DYNAMIC == RATE) THEN
!     ------------------------------------------------------------------
!       If it is a fallow treatment, there is no senescence.
        IF (CROP == 'FA') RETURN

        SENSUM = SUM(SENESCE % ResWt)
        IF (SENSUM < 1.E-10) RETURN

!       -----------------
!       Shoot senescence.
!       -----------------
!       Senesced leaves+stems+shells+seeds. The organ damage may be due
!       age senescence, water-stress damage, freeze damage and pest
!       damage; for some organs all of these hold, but for others not.
!       All of these, minus the pest damage (this does not add to the
!       soil), should be included.
        RESE = SENESCE % RESE
!       The senescence variables are calculated in the plant growth modules.
        ADDC   = SENESCE % ResWt(0) * 0.40            !kg[C]/ha
        ADDLIG = SENESCE % ResLig(0)                  !kg[lignin]/ha
        IF (N_ELEMS > 0) ADDN   = SENESCE % ResE(0,N) !kg[N]/ha
        IF (N_ELEMS > 1) ADDP   = SENESCE % ResE(0,P) !kg[P]/ha

!       Residue C to be added.
        RESC(SRFC) = ADDC

!       Average surface coverage parameter
        IF (Mulch%MulchMass > 1.E-6) THEN
          Mulch % Mulch_AM = (Mulch % Mulch_AM * Mulch % MulchMass 
     &      + SEN_AM * SENESCE % ResWt(0)) 
     &      / (Mulch%MulchMass + SENESCE % ResWt(0))
          Mulch % Mul_EXTFAC = (Mulch % Mul_EXTFAC * Mulch % MulchMass 
     &      + SEN_EXTFAC * SENESCE % ResWt(0)) 
     &      / (Mulch%MulchMass + SENESCE % ResWt(0))
          Mulch % Mul_WATFAC = (Mulch % Mul_WATFAC * Mulch % MulchMass 
     &      + SEN_WATFAC * SENESCE % ResWt(0)) 
     &      / (Mulch%MulchMass + SENESCE % ResWt(0))
        ELSE
          Mulch % Mulch_AM = SEN_AM
          Mulch % Mul_EXTFAC = SEN_EXTFAC
          Mulch % Mul_WATFAC = SEN_WATFAC
        ENDIF

!       Calculate senesced-leaf C/E ratio.
        IF (ADDN > 1.E-5) THEN
          RESCE(SRFC,N) = ADDC / ADDN
        ELSE
          RESCE(SRFC,N) = 0.0
        ENDIF

        IF (ADDP > 1.E-5) THEN
          RESCE(SRFC,P) = ADDC / ADDP
        ELSE
          RESCE(SRFC,P) = 0.0
        ENDIF

!       Set the lignin concentration of the senesced material.
        IF (ADDC > 1.E-5) THEN
          FRLRES(SRFC) = ADDLIG / (ADDC * 2.5)
        ELSE
          FRLRES(SRFC) = 0.0
        ENDIF

!       If L is transferred to PARTIT via the subroutine's parameter
!       string, it may go wrong because L is the DO loop counter.
!       Therefore, L is first copied to LAYER.
        LAYER = SRFC

!       Add the senesced material to the structural and metabolic
!       residue pools.
        CALL PARTIT_C (
     &    AMINRL, CEDAM, CESTR, DLAYR, FRDAE,             !Input
     &    FRLRES, FRMETI, FRMETS, LAYER, N_ELEMS,         !Input
     &    RESC, RESCE, RESDAX,                            !Input
     &    DLTLIGC, DLTMETABC, DLTMETABE, DLTSTRUCC,       !Input/Output
     &    DLTSTRUCE, IMMOB, RESE,                         !Input/Output
     &    ADDMETABEFLAG, FRMETFLAG)                       !Output

!       Sum up all C, N and P in senesced parts added to the soil for
!       the C, N and P balance.
        SENESSUMC = SENESSUMC + ADDC
        IF (N_ELEMS > 0) SENESSUMN = SENESSUMN + ADDN
        IF (N_ELEMS > 1) SENESSUMP = SENESSUMP + ADDP

!       Set back to zero after the residue has been added.
        ADDC = 0.
        ADDLIG = 0.
        ADDN = 0.
        ADDP = 0.

!       -------------------------
!       Root + nodule senescence.
!       -------------------------
!       Total root length is only calculated in ROOTDM (a pest routine),
!       which is not used if the pest option is not selected. So do it
!       here also.
        DO L = 1, NLAYR
!         Senesced roots and nodules are not calculated by soil layer,
!         so distribute them according to the root distribution.
          ADDC   = SENESCE % ResWt(L) * 0.40            !kg[C]/ha
          ADDLIG = SENESCE % ResLig(L)                  !kg[lignin]/ha
          IF (N_ELEMS > 0) ADDN   = SENESCE % ResE(L,N) !kg[N]/ha
          IF (N_ELEMS > 1) ADDP   = SENESCE % ResE(L,P) !kg[P]/ha

!         Multiply by 10 to convert from g/m2 (plant) to
!         kg/ha (soil).
          RESC(L)    = ADDC
          IF (ADDN > 1.E-5) THEN
            RESCE(L,N) = RESC(L) / ADDN
          ELSE
            RESCE(L,N) = 0.0
          ENDIF

          IF (ADDP > 1.E-5) THEN
            RESCE(L,P) = RESC(L) / ADDP
          ELSE
            RESCE(L,P) = 0.0
          ENDIF

!         Set the lignin concentration of the senesced material.
          IF (ADDC > 1.E-5) THEN
            FRLRES(L) = ADDLIG / (ADDC * 2.5)
          ELSE
            FRLRES(L) = 0.0
          ENDIF

!         If L is transferred via the subroutine's 
!         parameter string, it may go wrong, because L is the
!         DO loop counter. Therefore, L is first copied to LAYER.
          LAYER = L

!         Add the senesced material to the structural and metabolic
!         residue pools.
          CALL PARTIT_C (
     &      AMINRL, CEDAM, CESTR, DLAYR, FRDAE,           !Input
     &      FRLRES, FRMETI, FRMETS, LAYER, N_ELEMS,       !Input
     &      RESC, RESCE, RESDAX,                          !Input
     &      DLTLIGC, DLTMETABC, DLTMETABE, DLTSTRUCC,     !Input/Output
     &      DLTSTRUCE, IMMOB, RESE,                       !Input/Output
     &      ADDMETABEFLAG, FRMETFLAG)                     !Output

!         Sum up all C, N and P in senesced parts added to the soil for
!         the C, N and P balance.
          SENESSUMC = SENESSUMC + ADDC
          IF (N_ELEMS > 0) SENESSUMN = SENESSUMN + ADDN
          IF (N_ELEMS > 1) SENESSUMP = SENESSUMP + ADDP

!         Set back to zero after the residue has been added.
          ADDC = 0.
          ADDN = 0.
          ADDP = 0.
        END DO   !End of soil layer loop.

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF

      SENESCE % CumResWt   = SENESSUMC / 0.40
      IF (N_ELEMS > 0) SENESCE % CumResE(N) = SENESSUMN
      IF (N_ELEMS > 1) SENESCE % CumResE(P) = SENESSUMP

      RETURN
      END Subroutine SENESADD_C

