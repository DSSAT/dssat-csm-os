!***********************************************************************
!  CE_RATIO_C, subroutine for CENTURY-based SOM/residue module of DSSAT.
!
!  Purpose: This subroutine calculates the C:N and C:P ratio of newly
!           formed SOM.
!
!  Revision history:
!  01/12/2004 AJG Made a separate subroutine by splitting this off from
!                 LITDEC_C and SOMDEC_C.
!  08/30/2004 AJG Corrected layout irregularities.
!  09/11/2006 AJG Changed equations for C:P ratios.
!
!  Called: CENTURY
!  Calls : --
************************************************************************

      SUBROUTINE CE_RATIO_C ( 
     &  AMINRL, CES1M, CES1T, CES1X, CES21I, CES21M,      !Input
     &  CES21S, CES21T, CES21X,                           !Input
     &  CES23LM, CES23LX, CES23M,                         !Input
     &  CES23T, CES23X, CES2LI, CES2LM, CES2LS,           !Input
     &  CES2LX, CES3M, CES3T, CES3X, DLAYR, L, METABC,    !Input
     &  METABE, N_ELEMS, SOM1C, SOM1E, STRUCC, STRUCE,    !Input
     &  CES1, CES21, CES23, CES23L, CES2L, CES3)          !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
      IMPLICIT NONE
      SAVE
!     ------------------------------------------------------------------
      INTEGER IEL, L, N_ELEMS
!      INTEGER, PARAMETER :: NONLIG = 1, LIG = 2
      INTEGER, PARAMETER :: SRFC = 0, SOIL = 1

      REAL CEADD, CES1S, FREMET, FRESTR

      REAL DLAYR(NL), METABC(0:NL), SOM1C(0:NL), STRUCC(0:NL)

      REAL AMINRL(NL,3), CES1(0:NL,3), CES1M(0:1,3), CES1T(0:1,3),
     &  CES1X(0:1,3), CES21(0:NL,3), CES21I(0:0,3), CES21M(0:1,3),
     &  CES21S(0:1,3), CES21T(1,3), CES21X(0:1,3), CES23(0:NL,3),
     &  CES23L(0:NL,3), CES23LM(0:1,3),
     &  CES23LX(0:1,3), CES23M(0:1,3), CES23S(0:1,3),
     &  CES23T(1:1,3), CES23X(0:1,3), CES2L(0:NL,3), CES2LI(0:1,3),
     &  CES2LM(0:1,3), CES2LS(0:1,3), CES2LX(0:1,3), CES3(NL,3),
     &  CES3M(1,3),CES3S(1,3), CES3T(1,3), CES3X(1,3), METABE(0:NL,3),
     &  SOM1E(0:NL,3), STRUCE(0:NL,3)

!     ------------------------------------------------------------------
!#######################################################################
!     FOR LITDEC_C
!#######################################################################
!     ------------------------------------------------------------------
!     C/E ratio of surface SOM1 from surface metabolic.
!     ------------------------------------------------------------------
      DO IEL = 1, N_ELEMS
        IF (IEL == N .AND. L == SRFC) THEN
!         Fraction N in metabolic pool.
          IF (METABC(SRFC) > 1.E-5) THEN
            FREMET = METABE(SRFC,N) / (METABC(SRFC) * 2.5)

!           If the N concentration of the decomposing surface metabolic
!           residue is >= a threshold value, set the C/N ratio of the
!           new surface SOM1 to the minimum value.
            IF (FREMET >= CES1T(SRFC,N)) THEN
              CES1(SRFC,N) = CES1M(SRFC,N)
            ELSE
!             Otherwise, interpolate between the minimum and maximum
!             C/E ratio, depending on the E concentration of the
!             decomposing surface metabolic residue.
              CES1S = (CES1M(SRFC,N) - CES1X(SRFC,N)) / CES1T(SRFC,N)
              CES1(SRFC,N) = CES1X(SRFC,N) + FREMET * CES1S

!             Make sure that the new C/E ratio is >= the minimum and <= 
!             the maximum.
              CES1(SRFC,N) = AMAX1 (CES1(SRFC,N), CES1M(SRFC,N))
              CES1(SRFC,N) = AMIN1 (CES1(SRFC,N), CES1X(SRFC,N))
            ENDIF   !End of IF block on FREMET
          ELSE
            FREMET = 0.0
          ENDIF   !End of IF block on METABC(SRFC) > 1.E-5

        ELSEIF (IEL == P .AND. L == SRFC) THEN
!         Surface microorganisms are much more limited by C than by P.
!         Thus, only if C is abundant (e.g. >200 kg/ha per 1-cm soil
!         depth -- but the SRFC layer has no depth), P may become limiting.
          IF (METABC(SRFC) > 0. .AND. METABC(SRFC) < 200.) THEN
!           P is not limiting.
            CES1(SRFC,P) = CES1M(SRFC,P)
          ELSEIF (METABC(SRFC) >= 200.) THEN
!           P may be limiting.
!           If the P concentration of the decomposing surface metabolic
!           residue is >= a threshold value, set the C/P ratio of the 
!           new surface SOM1 to the minimum value.
            FREMET = METABE(SRFC,P) / (METABC(SRFC) * 2.5)
      
            IF (FREMET >= CES1T(SRFC,P)) THEN
              CES1(SRFC,P) = CES1M(SRFC,P)
            ELSE
!             Otherwise, interpolate between the minimum and maximum
!             C/P ratio, depending on the P concentration of the
!             decomposing surface metabolic residue.
              CES1S = (CES1M(SRFC,P) - CES1X(SRFC,P)) / CES1T(SRFC,P)
              CES1(SRFC,P) = CES1X(SRFC,P) + FREMET * CES1S

!             Make sure that the new C/P ratio is >= the minimum value
!             and <= the maximum.
              CES1(SRFC,P) = AMAX1 (CES1(SRFC,P), CES1M(SRFC,P))
              CES1(SRFC,P) = AMIN1 (CES1(SRFC,P), CES1X(SRFC,P))
            ENDIF   !End if IF block on FREMET.
          ENDIF   !End of IF block on METABC.
        ENDIF   !End of IF block on IEL

!       -----------------------------------------------------------------
!       C/E ratio of surface SOM1 and SOM2(N) or SOM23(P) of layer 1 from
!       surface structural.
!       -----------------------------------------------------------------
        IF (IEL == N .AND. L == SRFC) THEN
!         =====================
!         nonlignin(N) --> SOM1
!         =====================
!         E concentration of the decomposing surface structural
!         residue
          IF (STRUCC(SRFC) > 1.E-5) THEN
            FRESTR = STRUCE(SRFC,N) / (STRUCC(SRFC) * 2.5)

!           If the N concentration of the decomposing surface structural
!           residue is >= a threshold value, set the C/N ratio of the new
!           surface SOM1 to the minimum value.
            IF (FRESTR >= CES1T(SRFC,N)) THEN
              CES1(SRFC,N) = CES1M(SRFC,N)
            ELSE
!             Otherwise, interpolate between the minimum and maximum 
!             C/N ratio, depending on the E concentration of the 
!             decomposing surface structural residue.
              CES1S = (CES1M(SRFC,N) - CES1X(SRFC,N)) /
     &          CES1T(SRFC,N)
              CES1(SRFC,N) = CES1X(SRFC,N) + FRESTR * CES1S

!             Make sure that the new C/N ratio is >= the minimum value
!             and <= the maximum.
              CES1(SRFC,N) = AMAX1 (CES1(SRFC,N), CES1M(SRFC,N))
              CES1(SRFC,N) = AMIN1 (CES1(SRFC,N), CES1X(SRFC,N))
            ENDIF   !End of IF block on FRESTR.
          ELSE
            FRESTR = 0.0
          ENDIF   !End of IF block on STRUCC(SRFC) > 1.E-5

!         ==================
!         lignin(N) --> SOM2
!         ==================
!         Calculate the C/N ratio for new SOM2 from decomposing
!         lignin surface structural residue. This is set equal to
!         the C/N of the surface SOM1 that is newly formed from the
!         non-lignin structural residue, plus a factor that depends
!         on the C/N of SOM1.
          CEADD = CES2LI(SRFC,N) + CES2LS(SRFC,N) *
     &      (CES1(SRFC,N) - CES1M(SRFC,N))
          CES2L(SRFC,N) = CES1(SRFC,N) + CEADD

!         Make sure that the C/E ratio of the new SOM2 is >= the minimum
!         value and <= the maximum.
          CES2L(SRFC,N) = AMAX1 (CES2L(SRFC,N), CES2LM(SRFC,N))
          CES2L(SRFC,N) = AMIN1 (CES2L(SRFC,N), CES2LX(SRFC,N))

        ELSEIF (IEL == P .AND. L == SRFC) THEN
!         =====================
!         nonlignin(P) --> SOM1
!         =====================
!         Soil microorganisms are much more limited by C than by P.
!         Thus, only if C is abundant (e.g. >200 kg[C]/ha per 1-cm soil
!         depth -- but the SRFC layer has no depth), P may become limiting.
          IF (STRUCC(SRFC) < 200.) THEN
!           P is not limiting.
            CES1(SRFC,P) = CES1M(SRFC,P)
          ELSEIF (STRUCC(SRFC) >= 200.) THEN
!           P may be limiting.
!           If the P concentration of the decomposing surface structural
!           residue is >= a threshold value, set the C/P ratio of the 
!           new surface SOM1 to the minimum value.
            FRESTR = STRUCE(SRFC,P) / (STRUCC(SRFC) * 2.5)
      
            IF (FRESTR >= CES1T(SRFC,P)) THEN
              CES1(SRFC,P) = CES1M(SRFC,P)
            ELSEIF (FRESTR > 0. .AND. FRESTR < CES1T(SRFC,P)) THEN
!             Otherwise, interpolate between the minimum and maximum
!             C/P ratio, depending on the P concentration of the
!             decomposing surface structural residue.
              CES1S = (CES1M(SRFC,P) - CES1X(SRFC,P)) / CES1T(SRFC,P)
              CES1(SRFC,P) = CES1X(SRFC,P) + FRESTR * CES1S

!             Make sure that the new C/P ratio is >= the minimum value
!             and <= the maximum.
              CES1(SRFC,P) = AMAX1 (CES1(SRFC,P), CES1M(SRFC,P))
              CES1(SRFC,P) = AMIN1 (CES1(SRFC,P), CES1X(SRFC,P))
            ENDIF   !End of IF block on FRESTR
          ENDIF   !End of IF block on STRUCC.

!         ========================
!         SRFC lignin(P) --> SOM23
!         ========================
!         Calculate the C/P ratio for new SOM23 from decomposing
!         soil structural lignin residue. This is set equal to
!         the C/P of the soil SOM1 that is newly formed from the
!         non-lignin soil residue, plus a factor.

!CENTURY (PREDEC) == SRFC lignin --> SOM2 of L=1
!radds1 = rad1p(1,iel) + rad1p(2,iel) * (rnewas(iel,1)-pcemic(2,iel))
!rnewas(iel,2) = rnewas(iel,1) + radds1
!rnewas(iel,2) = max(rnewas(iel,2), rad1p(3,iel))

!rnewas(iel,2) is new SOM2 from SRFC lignin; rad1p(1,iel) is intercept (220), rad1p(2,iel) is slope (5);
!pcemic(2,2) is minimum C/P ratio for surface microbes (99)

!         From CENTURY's sub PREDEC:
!         CEADD = 220 + 5 * SOM1C(L) / SOM1E(L,P) - 99
          CEADD = 220 + 5 * CES1(SRFC,P) - 99

          CES23L(SRFC,P) = CES1(SRFC,P) + CEADD

!         Make sure that the C/E ratio of the new SOM23 is >= the minimum
!         value and <= the maximum.
          CES23L(SRFC,P) = AMAX1 (CES23L(SRFC,P), CES23LM(SRFC,P))
          CES23L(SRFC,P) = AMIN1 (CES23L(SRFC,P), CES23LX(SRFC,P))
        ENDIF   !End of IF block on IEL

!       ----------------------------------------------------------------
!       C/E ratio of soil SOM1 from soil metabolic.
!       ----------------------------------------------------------------
        IF (IEL == N .AND. L /= SRFC) THEN
!         NB: The original CENTURY model only deals with a 20-cm-thick 
!         soil layer, while DSSAT has many layers of variable thickness. 
!         The parameter CES1T (a 'fixed' parameter) is in kg[N]/ha
!         and the comparison it is used for depends on the layer
!         thickness. It is therefore recalculated to a 1-cm-thick layer.
          IF (AMINRL(L,N) <= 1.E-06) THEN
!           If no E available in the soil for immobilization by the
!           residue, set the C/E ratio to the maximum value.
            CES1(L,N) = CES1X(SOIL,N)
          ELSEIF (AMINRL(L,N) / DLAYR(L) >= 
     &      CES1T(SOIL,N) / 20.) THEN
!           If the amount of E available in the soil for immobilization
!           by the residue is more than the critical value, set the C/E
!           ratio to the minimum value.
            CES1(L,N) = CES1M(SOIL,N)
          ELSE
!           Otherwise, interpolate between the minimum and maximum
!           C/E ratio, depending on the amount of E available in the
!           soil for absorption by the residue.
            CES1S = -(CES1X(SOIL,N) - CES1M(SOIL,N)) /
     &        (CES1T(SOIL,N) / 20.)
            CES1(L,N) = CES1X(SOIL,N) + CES1S *
     &        AMINRL(L,N) / DLAYR(L)

!           Make sure that the new C/E ratio is >= the minimum value
!           and <= the maximum.
            CES1(L,N) = AMAX1 (CES1(L,N), CES1M(SOIL,N))
            CES1(L,N) = AMIN1 (CES1(L,N), CES1X(SOIL,N))
          ENDIF   !End of IF block on AMINRL

        ELSEIF (IEL == P .AND. L /= SRFC) THEN
!         Soil microorganisms are much more limited by C than by P.
!         Thus, only if C is abundant (e.g. >200 kg[C] per 1-cm soil depth),
!         P may become limiting.
          IF (METABC(L) / DLAYR(L) < 200.) THEN
!           P is not limiting.
            CES1(L,P) = CES1M(SOIL,P)
          ELSE   !METABC(L) / DLAYR(L) >= 200.
!           P may be limiting.
!           If the P concentration of the decomposing surface metabolic
!           residue is >= a threshold value, set the C/P ratio of the  
!           new surface SOM1 to the minimum value.
            FREMET = METABE(L,P) / (METABC(L) * 2.5)

            IF (FREMET >= CES1T(SOIL,P)) THEN
              CES1(L,P) = CES1M(SOIL,P)
            ELSE
!             Otherwise, interpolate between the minimum and maximum
!             C/P ratio, depending on the P concentration of the
!             decomposing surface metabolic residue.
              CES1S = (CES1M(SOIL,P) - CES1X(SOIL,P)) /
     &          (CES1T(SOIL,P) / 20.)
              CES1(L,P) = CES1X(SOIL,P) + FREMET * CES1S

!             Make sure that the new C/P ratio is >= the minimum value
!             and <= the maximum.
              CES1(L,P) = AMAX1 (CES1(L,P), CES1M(SOIL,P))
              CES1(L,P) = AMIN1 (CES1(L,P), CES1X(SOIL,P))
            ENDIF   !End of IF block on FREMET.
          ENDIF   !End of IF block on METABC
        ENDIF   !End of IF block on IEL.

!       ----------------------------------------------------------------
!       C/E ratio of soil SOM1 and SOM2 or SOM23 from soil structural.
!       ----------------------------------------------------------------
        IF (IEL == N .AND. L /= SRFC) THEN
!         =====================
!         nonlignin(N) --> SOM1
!         =====================
!         NB: The original CENTURY model only deals with a 20-cm-thick 
!         soil layer, while DSSAT has many layers of variable thickness. 
!         The parameter CES1T (a 'fixed' parameter) is in kg[N]/ha
!         and the comparison it is used for depends on the layer
!         thickness. It is therefore recalculated to a 1-cm-thick layer.

!         If the amount of E available in the soil for
!         immobilization by the residue is more than the critical
!         value, set the C/E ratio to the minimum value.
          IF (AMINRL(L,N) / DLAYR(L) >=
     &      CES1T(SOIL,N) / 20.) THEN
            CES1(L,N) = CES1M(SOIL,N)
          ELSE
!           Otherwise, interpolate between the minimum and maximum
!           C/E ratio, depending on the amount of E available in the
!           soil for absorption by the residue.
            CES1S = -(CES1X(SOIL,N) - CES1M(SOIL,N)) /
     &        (CES1T(SOIL,N) / 20.)
            CES1(L,N) = CES1X(SOIL,N) + CES1S *
     &        AMINRL(L,N) / DLAYR(L)

!           Make sure that the new C/E ratio is >= the minimum value
!           and <= the maximum.
            CES1(L,N) = AMAX1 (CES1(L,N), CES1M(SOIL,N))
            CES1(L,N) = AMIN1 (CES1(L,N), CES1X(SOIL,N))
          ENDIF

!         ==================
!         soil lignin(N) --> SOM2
!         ==================
!         Calculate the C/E ratio for new SOM2 from decomposing
!         soil structural lignin residue. This is set equal to
!         the C/E of the soil SOM1 that is newly formed from the
!         non-lignin soil residue, plus a factor that depends
!         on the C/E of SOM1.
          CEADD = CES2LI(SOIL,N) + CES2LS(SOIL,N) *
     &      (CES1(L,N) - CES1M(SOIL,N))
          CES2L(L,N) = CES1(L,N) + CEADD

!         Make sure that the new C/E ratio is >= the minimum value
!         and <= the maximum.
          CES2L(L,N) = AMAX1 (CES2L(L,N), CES2LM(SOIL,N))
          CES2L(L,N) = AMIN1 (CES2L(L,N), CES2LX(SOIL,N))

        ELSEIF (IEL == P .AND. L /= SRFC) THEN
!         =====================
!         nonlignin(P) --> SOM1
!         =====================
!         Soil microorganisms are much more limited by C than by P.
!         Thus, only if C is abundant (e.g. >200 kg[C] per 1-cm soil depth),
!         P may become limiting.
          IF (STRUCC(L) / DLAYR(L) < 200.) THEN
!           P is not limiting.
            CES1(L,P) = CES1M(SOIL,P)
          ELSE   !STRUCC(L) / DLAYR(L) >= 200.
!           P may be limiting.
!           If the P concentration of the decomposing soil structural
!           material is >= than a threshold value, set the C/P ratio of
!           the new surface SOM1 to the minimum value.
            FRESTR = STRUCE(L,P) / (STRUCC(L) * 2.5)
      
            IF (FRESTR >= CES1T(SOIL,P)) THEN
              CES1(L,P) = CES1M(SOIL,P)
            ELSE
!             Otherwise, interpolate between the minimum and maximum
!             C/P ratio, depending on the P concentration of the
!             decomposing soil structural residue.
              CES1S = (CES1M(SOIL,P) - CES1X(SOIL,P)) /
     &          (CES1T(SOIL,P) / 20.)
              CES1(L,P) = CES1X(SOIL,P) + FREMET * CES1S

!             Make sure that the new C/P ratio is >= the minimum value
!             and <= the maximum.
              CES1(L,P) = AMAX1 (CES1(L,P), CES1M(SOIL,P))
              CES1(L,P) = AMIN1 (CES1(L,P), CES1X(SOIL,P))
            ENDIF   !End of IF block on FRESTR.
          ENDIF   !End of IF block on STRUCC.

!         ========================
!         soil lignin(P) --> SOM23
!         ========================
!         Set the C/P ratio for new SOM23 from decomposing
!         soil structural lignin residue. 

!         Set CES23L from soil lignin to CES23X, as in CENTURY's sub PREDEC (param VARAT2(1,2)).
          CES23L(L,P) = CES23X(SOIL,P)

!         Make sure that the C/E ratio of the new SOM2 is >= the minimum
!         value and <= the maximum.
          CES23L(L,P) = AMAX1 (CES23L(L,P), CES23LM(SOIL,P))
          CES23L(L,P) = AMIN1 (CES23L(L,P), CES23LX(SOIL,P))
        ENDIF   !End of IF block on IEL.

!#######################################################################
!       FOR SOMDEC_C
!#######################################################################
!       ----------------------------------------------------------------
!       C/E ratio of new SOM2(N) or SOM23(P) from decomposing surface SOM1.
!       ----------------------------------------------------------------
        IF (IEL == N .AND. L == SRFC) THEN
!         Calculate C/E ratios for the flow from surface SOM1 to
!         SOM2 (layer 1). The C/E of new SOM2 equals C/E of old
!         surface SOM1 plus a factor that depends on the C/E of
!         SOM1.
          IF (SOM1E(SRFC,N) > 1.E-06) THEN
            CEADD = CES21I(SRFC,N) + CES21S(SRFC,N) *
     &        (SOM1C(SRFC) / SOM1E(SRFC,N) - CES1M(SRFC,N))
            CES21(SRFC,N) = SOM1C(SRFC) / SOM1E(SRFC,N) + CEADD
          ENDIF

!         Make sure that the new C/E ratio is >= the minimum value
!         allowed and =< the maximum value allowed.
          CES21(SRFC,N) = AMAX1 (CES21(SRFC,N), CES21M(SRFC,N))
          CES21(SRFC,N) = AMIN1 (CES21(SRFC,N), CES21X(SRFC,N))

        ELSEIF (IEL == P .AND. L == SRFC) THEN
          IF (SOM1E(SRFC,P) > 1.E-06) THEN

!CENTURY = SRFC SOM1:
!radds1 = rad1p(1,iel) + rad1p(2,iel) * ((som1c(1)/som1e(1,iel)) - pcemic(2,iel))   !!SOM1C(1) is SRFC layer
!       =    220       +    5         * ((som1c(1)/som1e(1,iel)) -    99
!rceto2(iel) = som1c(SRFC)/som1e(SRFC,iel) + radds1

!rad1p(1,1)  is CES21I(0,2) = 220
!rad1p(2,1)  is CES21S(0,2) = 5
!pcemic(2,2) is CES1M(0,2)  = 99

            CEADD = CES21I(SRFC,P) + CES21S(SRFC,P) * 
     &        SOM1C(SRFC) / SOM1E(SRFC,P) - CES1M(0,P)
            CES23(SRFC,P) = SOM1C(SRFC) / SOM1E(SRFC,P) + CEADD
          ENDIF

!         Make sure that the new C/E ratio is >= the minimum value
!         allowed and =< the maximum value allowed.
          CES23(SRFC,P) = AMAX1 (CES23(SRFC,P), CES23M(SRFC,P))
          CES23(SRFC,P) = AMIN1 (CES23(SRFC,P), CES23X(SRFC,P))
        ENDIF   !End of IF block on IEL.

!       ----------------------------------------------------------------
!       C/E ratio of new soil SOM1 from decomposing SOM2, SOM3 or SOM23.
!       ----------------------------------------------------------------
        IF (IEL == N .AND. L /= SRFC) THEN
          IF (AMINRL(L,N) <= 1.E-06) THEN
!           If there is no mineral N available in the soil for
!           absorption by the decomposing SOM2 or SOM3, set the C/N
!           ratio of the new soil SOM1 to the maximum value allowed.
            CES1(L,N) = CES1X(SOIL,N)
  
          ELSEIF ((AMINRL(L,N) / DLAYR(L)) >= CES1T(SOIL,N)) THEN
!           If the amount of mineral N available in the soil for
!           absorption by the decomposing SOM2 or SOM3 is greater than
!           the critical value, set the C/N ratio of the new soil SOM1
!           to the minimum value allowed.
            CES1(L,N) = CES1M(SOIL,N)

          ELSE
!           Otherwise, interpolate between the minimum and maximum C/N
!           ratio depending on the amount of mineral N available in
!           the soil for absorption by the decomposing SOM2 or SOM3.
            CES1S = -(CES1X(SOIL,N) - CES1M(SOIL,N)) / CES1T(SOIL,N)
            CES1(L,N) = CES1X(SOIL,N) + CES1S * AMINRL(L,N)

!           Make sure that the new C/N ratio is >= the minimum value
!           allowed and =< the maximum value allowed.
            CES1(L,N) = AMAX1 (CES1(L,N), CES1M(SOIL,N))
            CES1(L,N) = AMIN1 (CES1(L,N), CES1X(SOIL,N))
          ENDIF  !End of IF block on AMINRL.

        ELSEIF (IEL == P .AND. L /= SRFC) THEN
          IF (AMINRL(L,P) <= 1.E-06) THEN
!           If there is no mineral P available in the soil for
!           absorption by the decomposing SOM23, set the C/P
!           ratio of the new soil SOM1 to the maximum value allowed.
            CES1(L,P) = CES1X(SOIL,P)

          ELSEIF ((AMINRL(L,P) / DLAYR(L)) >= CES1T(SOIL,P)) THEN
!           If the amount of mineral P available in the soil per 1-cm soil
!           depth for absorption by the decomposing SOM23 is greater than
!           the critical value, set the C/P ratio of the new soil SOM1
!           to the minimum value allowed.
            CES1(L,P) = CES1M(SOIL,P)

          ELSE
!           Otherwise, interpolate between the minimum and maximum C/P
!           ratio depending on the amount of mineral P available in
!           the soil for absorption by the decomposing SOM2 or SOM3.
            CES1S = -(CES1X(SOIL,P) - CES1M(SOIL,P)) / CES1T(SOIL,P)
            CES1(L,P) = CES1X(SOIL,P) + CES1S * AMINRL(L,P)

!           Make sure that the new C/P ratio is >= the minimum value
!           allowed and =< the maximum value allowed.
            CES1(L,P) = AMAX1 (CES1(L,P), CES1M(SOIL,P))
            CES1(L,P) = AMIN1 (CES1(L,P), CES1X(SOIL,P))
          ENDIF   !End of IF block on AMINRL.
        ENDIF   !End of IF block on IEL.

!       ----------------------------------------------------------------
!       C/E ratio of new SOM2(N) or SOM23(P) from decomposing soil SOM1.
!       ----------------------------------------------------------------
!       NB: The original CENTURY model only deals with a 20-cm-thick 
!       soil layer, while DSSAT has many layers of variable thickness. 
!       The parameter CES21T (a 'fixed' parameter) is in kg[N]/ha
!       and the comparison it is used for depends on the layer
!       thickness. It is therefore recalculated to a 1-cm-thick layer.
        IF (IEL == N .AND. L /= SRFC) THEN
          IF (AMINRL(L,N) <= 1.E-06) THEN
!           If there is no mineral N available in the layer for
!           absorption by the decomposing soil SOM1, set the C/N ratio
!           of the new SOM2 to the maximum value allowed.
            CES21(L,N) = CES21X(SOIL,N)

          ELSEIF (AMINRL(L,N) / DLAYR(L) >  CES21T(SOIL,N) / 20.) THEN
!           If the amount of mineral N available in the layer for
!           absorption by the decomposing soil SOM1 is greater than
!           the critical value, set the C/N ratio of the new SOM2 to
!           the minimum value allowed.
            CES21(L,N) = CES21M(SOIL,N)

          ELSE
!           Otherwise, interpolate between the minimum and maximum
!           C/E, depending on the amount of mineral N available in 
!           the layer for absorption by the decomposing soil SOM1.
            CES21S(SOIL,N) = -(CES21X(SOIL,N) - CES21M(SOIL,N))
     &        / (CES21T(SOIL,N) / 20.)
            CES21(L,N) = CES21X(SOIL,N) + CES21S(SOIL,N) *
     &        AMINRL(L,N) / DLAYR(L)

!           Make sure that the new C/N ratio is >= the minimum value
!           allowed and =< the maximum value allowed.
            CES21(L,N) = AMAX1 (CES21(L,N), CES21M(SOIL,N))
            CES21(L,N) = AMIN1 (CES21(L,N), CES21X(SOIL,N))
          ENDIF  !End of IF block on CES21.

        ELSEIF (IEL == P .AND. L /= SRFC) THEN
!================
!CENTURY soil SOM1:
!rceto2(iel) = bgdrat(aminrl,varat2,iel)
!bgdrat = (1.-aminrl(P)/varat(3,P)) * (varat(1,P)-varat(2,P))+varat(2,P)
!bgdrat = (1.-aminrl(P) / 2) * (400. - 100.) + 100.

!varat2(1,P) = maximum C/E ratio for material entering SOM2, thus SOM23X(1,2) = 400
!varat2(2,P) = minimum C/E ratio for material, thus SOM23M(1,2) = 100
!varat2(3,P) = amount of E present when minimum ratio applies, thus SOM23T(1,2) = 2 g/m2 or 20 kg/ha
!================

          CES23(L,P) = (1. - AMINRL(L,P) / CES23T(1,P)) *
     &      (CES23X(1,P) - CES23M(1,P)) + CES23M(1,P)
          CES23(L,P) = (1. - AMINRL(L,P) / 20.) * (400. - 100.) + 100.

          IF (AMINRL(L,P) < 1.E-06) THEN
!           If there is no mineral P available in the layer for
!           absorption by the decomposing soil SOM1, set the C/P ratio
!           of the new SOM23 to the maximum value allowed.
!================
!CENTURY:
!rceto2(iel) = bgdrat(aminrl,varat2,iel) 
!bgdrat = varat(1,iel)
!================
            CES23(L,P) = CES23X(SOIL,P)

          ELSEIF (AMINRL(L,P) / DLAYR(L) >  CES23T(SOIL,P) / 20.) THEN
!           If the amount of mineral P available in the layer for
!           absorption by the decomposing soil SOM1 is greater than
!           the critical value, set the C/P ratio of the new SOM23 to
!           the minimum value allowed.
!================
!CENTURY:
!elseif (aminrl(iel) .gt. varat(3,iel))
!bgdrat = varat(2,iel) 
!================
            CES23(L,P) = CES23M(SOIL,P)

          ELSE
!           Otherwise, interpolate between the minimum and maximum
!           C/P, depending on the amount of mineral P available in 
!           the layer for absorption by the decomposing soil SOM1.
!================
!CENTURY:
!aminrl(iel) > 0 and <= varat(3,iel)
!bgdrat = (1.-aminrl(iel)/varat(3,iel))*(varat(1,iel)-varat(2,iel))+varat(2,iel)
!================
            CES23S(SOIL,P) = -(CES23X(SOIL,P) - CES23M(SOIL,P))
     &        / (CES23T(SOIL,P) / 20.)
            CES23(L,P) = CES23X(SOIL,P) + CES23S(SOIL,P) *
     &        AMINRL(L,P) / DLAYR(L)

!           Make sure that the new C/P ratio is >= the minimum value
!           allowed and =< the maximum value allowed.
            CES23(L,P) = AMAX1 (CES23(L,P), CES23M(SOIL,P))
            CES23(L,P) = AMIN1 (CES23(L,P), CES23X(SOIL,P))
          ENDIF   !End of IF block on AMINRL.
        ENDIF   !End of IF block on IEL.

!       ----------------------------------------------------------------
!       C/E ratio of new SOM3 from decomposing soil SOM1 or SOM2.
!       ----------------------------------------------------------------
!       NB: The original CENTURY model only deals with a 20-cm-thick 
!       soil layer, while DSSAT has many layers of variable thickness. 
!       The parameter CES3T (a 'fixed' parameter) is in kg[N]/ha
!       and the comparison it is used for depends on the layer
!       thickness. It is therefore recalculated to a 1-cm-thick layer.
        IF (IEL == N .AND. L /= SRFC) THEN
          IF (AMINRL(L,N) <= 1.E-06) THEN
!           If there is no mineral E available in the layer for
!           absorption by the decomposing soil SOM1 or SOM2, set the
!           C/E ratio of the new SOM3 to the maximum value allowed.
            CES3(L,N) = CES3X(SOIL,N)

          ELSEIF (AMINRL(L,N) / DLAYR(L) > CES3T(SOIL,N) / 20.) THEN
!           If the amount of mineral E available in the layer for
!           absorption by the decomposing soil SOM1 or SOM2 is greater
!           than the critical value, set the C/E ratio of the new SOM3
!           to the minimum value allowed.
            CES3(L,N) = CES3M(SOIL,N)

          ELSE
!           Otherwise, interpolate between the minimum and maximum C/E
!           ratio depending on the amount of mineral E available in
!           the layer for absorption by the decomposing soil SOM1 or
!           SOM2.
            CES3S(SOIL,N) = -(CES3X(SOIL,N) - CES3M(SOIL,N)) /
     &        (CES3T(SOIL,N) / 20.)
            CES3(L,N) = CES3X(SOIL,N) + CES3S(SOIL,N) *
     &        AMINRL(L,N) / DLAYR(L)

!           Make sure that the new C/E ratio is >= the minimum value
!           allowed and =< the maximum value allowed.
            CES3(L,N) = AMAX1 (CES3(L,N), CES3M(SOIL,N))
            CES3(L,N) = AMIN1 (CES3(L,N), CES3X(SOIL,N))
          ENDIF   !End of IF block on CES3.

        ELSEIF (IEL == P .AND. L /= SRFC) THEN
!         Nothing needed, as CE23(L,P) has already been calculated.
        ENDIF   !End of IF block on IEL.
      ENDDO   !End of loop on IEL.

      RETURN
      END Subroutine CE_RATIO_C
