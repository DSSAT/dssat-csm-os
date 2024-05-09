!***********************************************************************
!  TSOMLIT_C, Subroutine for CENTURY-based SOM/residue module of DSSAT.
!
!  Purpose: Sum all SOM pools and litter pools for the whole soil
!           profile.
!
!  REVISION HISTORY
!  06/09/1999 AJG Written
!  01/01/2000 AJG Integrated the CENTURY-based and CERES-based SOM
!               modules with CHP's modular structure.
!  09/08/2004 AJG Adapted for the P module.
!  05/23/2006 CHP Added mulch layer effects on evaporation and infiltration.
!
!  Called : CENTURY, SOILNI_C
!  Calls  : MULCHLAYER
!***********************************************************************

      SUBROUTINE TSOMLIT_C ( 
     &  METABC, METABE, N_ELEMS, NLAYR, SOILPROP,         !Input
     &  SOM1C, SOM1E, SOM2C, SOM2E, SOM23E, SOM3C,        !Input
     &  SOM3E, STRUCC, STRUCE,                            !Input
     &  LITC, LITE, MULCH, SSOMC, SSOME, TLITC, TLITE,    !Output
     &  TMETABC, TMETABE, SomLit, SomLitC, SomLitE,       !Output
     &  TSOM1C, TSOM1E, TSOM2C, TSOM2E, TSOM23E, TSOM3C,  !Output
     &  TSOM3E, TSOMC, TSOME, TSTRUCC, TSTRUCE)           !Output

!     ------------------------------------------------------------------
      USE ModuleDefs
      USE ModuleData
      IMPLICIT NONE
      EXTERNAL MULCHLAYER
      SAVE
!     ------------------------------------------------------------------
      INTEGER IEL, L, N_ELEMS, NLAYR
      INTEGER, PARAMETER :: SRFC = 0    !, SOIL = 1, N = 1, P = 2

      REAL TLITC, TMETABC, TSOM1C, TSOM2C, TSOM3C,
     &  TSOMC, TSTRUCC

      REAL LITC(0:NL), METABC(0:NL), SOM1C(0:NL), SOM2C(NL), 
     &  SOM3C(NL), SSOMC(0:NL), STRUCC(0:NL), TLITE(NELEM),
     &  TMETABE(NELEM), TSOM1E(NELEM), TSOM2E(NELEM),
     &  TSOM23E(NELEM), TSOM3E(NELEM), TSOME(NELEM), TSTRUCE(NELEM)
      REAL SomLitC(0:NL), SomLitE(0:NL,NELEM), SomLit(NL)
      REAL LITE(0:NL,3), METABE(0:NL,3), SOM1E(0:NL,3), SOM2E(NL,3),
     &  SOM23E(NL,3), SOM3E(NL,3), SSOME(0:NL,3), STRUCE(0:NL,3)

      TYPE (MulchType)   MULCH

!     Add soil properties to convert organic C to %
      TYPE (SoilType)    SOILPROP
      REAL, DIMENSION(NL) :: BD, DLAYR
      BD    = SOILPROP % BD
      DLAYR = SOILPROP % DLAYR

!     ------------------------------------------------------------------
!     Initialize to zero before summing up across all soil layers.
      TLITC   = 0.
      TMETABC = 0.
      TSTRUCC = 0.

      TSOMC   = 0.
      TSOM1C  = 0.
      TSOM2C  = 0.
      TSOM3C  = 0.

      TLITE   = 0.
      TMETABE = 0.
      TSTRUCE = 0.

      TSOME   = 0.
      TSOM1E  = 0.
      TSOM2E  = 0.
      TSOM3E  = 0.
      TSOM23E = 0.

!     ---------------------------------------------------------------
!     Compute surface totals
      LITC(SRFC)  = STRUCC(SRFC) + METABC(SRFC)
      SSOMC(SRFC) = SOM1C(SRFC)
      SomLitC(SRFC) = SSOMC(SRFC) + LITC(SRFC)

      DO IEL = 1, N_ELEMS
        LITE(SRFC,IEL) = STRUCE(SRFC,IEL) + METABE(SRFC,IEL)
        SSOME(SRFC,IEL) = SOM1E(SRFC,IEL)
        SomLitE(SRFC,IEL) = SSOME(SRFC,IEL) + LITE(SRFC,IEL)
      END DO

!     ---------------------------------------------------------------
!     Calculate the combined structural-plus-metabolic C and E, and
!     sum across the whole profile (not SRFC layer).
      DO L = 1, NLAYR
        LITC(L) = STRUCC(L) + METABC(L)
        TMETABC = TMETABC + METABC(L)
        TSTRUCC = TSTRUCC + STRUCC(L)
        TLITC   = TLITC   + LITC(L)

        DO IEL = 1, N_ELEMS
          LITE(L,IEL)  = STRUCE(L,IEL) + METABE(L,IEL)
          TMETABE(IEL) = TMETABE(IEL)  + METABE(L,IEL)
          TSTRUCE(IEL) = TSTRUCE(IEL)  + STRUCE(L,IEL)
          TLITE(IEL)   = TLITE(IEL)    + LITE(L,IEL)
        END DO   !End of DO loop on IEL
      END DO   !End of DO loop on L

!     Calculate the total SOM C and E in the soil layers, and sum
!     across the whole profile (not SRFC layer).
      DO L = 1, NLAYR
        SSOMC(L) = SOM1C(L) + SOM2C(L) + SOM3C(L)
        TSOMC    = TSOMC    + SSOMC(L)
        TSOM1C   = TSOM1C   + SOM1C(L)
        TSOM2C   = TSOM2C   + SOM2C(L)
        TSOM3C   = TSOM3C   + SOM3C(L)
        SomLitC(L) = SSOMC(L) + LITC(L)

!       kg[Organic matter]/ha
!       SomLit is units of kg[OM]/ha, 
!       SSOMC and LITC are in units of kg[C]/ha
!       Convert LITC at 2.5 kg[OM]/kg[C] 
!       Convert SSOMC at 1.9 kg[OM]/kg[C] (Adams, 1973)
        SomLit(L) = SSOMC(L) * 1.9 + LITC(L) * 2.5

        DO IEL = 1, N_ELEMS
          IF (IEL == N) THEN
            SSOME(L,N) = SOM1E(L,N) + SOM2E(L,N) + SOM3E(L,N)
            TSOM1E(N)  = TSOM1E(N)  + SOM1E(L,N)
            TSOM2E(N)  = TSOM2E(N)  + SOM2E(L,N)
            TSOM3E(N)  = TSOM3E(N)  + SOM3E(L,N)
          ELSEIF (IEL == P) THEN 
            SSOME(L,P) = SOM1E(L,P) + SOM23E(L,P)
            TSOM1E(P)  = TSOM1E(P)  + SOM1E(L,P)
            TSOM23E(P) = TSOM23E(P) + SOM23E(L,P)
          ENDIF   !End of IF block on IEL.
          TSOME(IEL)  = TSOME(IEL)  + SSOME(L,IEL)
          SomLitE(L,IEL) = SSOME(L,IEL) + LITE(L,IEL)
        ENDDO   !End of DO loop on IEL.
      ENDDO   !End of DO loop on L.

!     Transfer surface residue to mulch variable
!     Mulch mass kg[dry matter]/ha
      MULCH % MULCHMASS = SSOMC(SRFC) * 1.9 + LITC(SRFC) * 2.5 
      MULCH % MULCHN    = SomLitE(SRFC,1)      !kg[N]/ha
      MULCH % MULCHP    = SomLitE(SRFC,P)      !kg[P]/ha
      CALL MULCHLAYER (MULCH) 

      CALL PUT('ORGC','TSOMC', TSOMC)

!     ------------------------------------------------------------------
      RETURN
      END SUBROUTINE TSOMLIT_C
