!***********************************************************************
!  INCORPOR_C, subroutine for CENTURY-based SOM/residue module of DSSAT.
!
!  Purpose: This subroutine takes care of incorporation of surface
!           litter into the soil, both for situations in which
!           incorporation is done immediately when residue is applied
!           or when incorporation is done at a later time. This
!           subroutine only works when the CENTURY-based SOM model has
!           been chosen, because otherwise surface litter is not being
!           simulated.
!
!  Revision history:
!  01/01/1999 AJG  Written
!  09/06/2004 AJG  Changed .LE. into <= etc.
!
!  Called: CENTURY
!  Calls : --
!***********************************************************************

      SUBROUTINE INCORPOR_C (
     &  DLAYR, LIGC, METABC, METABE, N_ELEMS, NLAYR,      !Input
     &  SOM1C, SOM1E, STRUCC, STRUCE, MIXDEP, MIXPCT,     !Input

     &  DLTLIGC, DLTMETABC, DLTMETABE, DLTSOM1C,          !Output
     &  DLTSOM1E, DLTSTRUCC, DLTSTRUCE)                   !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
!     NL defined in ModuleDefs.for

      IMPLICIT NONE
      SAVE

      INTEGER IEL, IOUT, L, N_ELEMS, NLAYR, SRFC
      PARAMETER (SRFC = 0)

      REAL DEPTH, ERRAMT, FR, HOLD, MIXDEP, MIXPCT

      REAL, DIMENSION(NL) :: DLAYR
      REAL, DIMENSION(0:NL) :: DLTLIGC, DLTMETABC, DLTSOM1C, DLTSTRUCC
      REAL, DIMENSION(0:NL) :: LIGC, METABC, SOM1C, STRUCC

      REAL, DIMENSION(0:NL,3) :: DLTMETABE, DLTSOM1E, DLTSTRUCE, METABE
      REAL, DIMENSION(0:NL,3) :: SOM1E, STRUCE

!***********************************************************************
      ERRAMT = 0.0

!     ----------------------------------------------------------------
!     If residues had been applied earlier on top of the soil surface,
!     but are only now incorporated into the soil, then they have been
!     divided already in metabolic and structural material. These are
!     now thus distributed over the soil layers.

!     If the tillage depth is zero, the incorporation percentage
!     should also be zero.
      IF (MIXDEP < 0.001) RETURN
      IF (MIXPCT < 0.001) RETURN

!     Set the starting depth for counting the layers to zero.
      DEPTH = 0.

!     Initialize a flag that determines when to jump out of the DO
!     loop.
      IOUT = 1

      DO L = 1, NLAYR
!       Set the depth of the upper limit of the layer.
        HOLD = DEPTH

!       Calculate the depth of the bottom of the layer.
        DEPTH = DEPTH + DLAYR(L)

!       If the residue application depth is less than the depth of
!       the bottom of the layer.
        IF (MIXDEP <= DEPTH) THEN

!         Calculate the fraction of the residue that is to be added
!         to this layer.
          IF (MIXDEP > 1.E-4) FR = (MIXDEP - HOLD) / MIXDEP

!         Set a flag to jump out of the DO loop.
          IOUT = 2
        ELSE
!         If the residue application depth is greater than the
!         depth of bottom of the layer, calculate the fraction of
!         the residue that is to be added to this layer.
          IF (MIXDEP > 1.E-4) FR = DLAYR(L) / MIXDEP
        ENDIF

!       Add the newly incorporated material to the litter pools of
!       the layer. 
        DLTMETABC(L) = DLTMETABC(L) + METABC(SRFC)* FR * MIXPCT / 100.
        DLTSTRUCC(L) = DLTSTRUCC(L) + STRUCC(SRFC)* FR * MIXPCT / 100.
        DLTLIGC(L)   = DLTLIGC(L)   + LIGC(SRFC)  * FR * MIXPCT / 100.
        DLTSOM1C(L)  = DLTSOM1C(L)  + SOM1C(SRFC) * FR * MIXPCT / 100.

        DO IEL = 1, N_ELEMS
          DLTMETABE(L,IEL) = DLTMETABE(L,IEL) + METABE(SRFC,IEL) 
     &                       * FR * MIXPCT / 100.
          IF (DLTMETABE(SRFC,IEL) + METABE(SRFC,IEL) < -1.E-4) THEN
            ERRAMT = ERRAMT + DLTMETABE(L,IEL) + METABE(L,IEL)
            DLTMETABE(L,IEL) = -METABE(L,IEL)
          ENDIF

          DLTSTRUCE(L,IEL) = DLTSTRUCE(L,IEL) + STRUCE(SRFC,IEL) 
     &                       * FR * MIXPCT / 100.
          IF (DLTSTRUCE(L,IEL) + STRUCE(L,IEL) < -1.E-4) THEN
            ERRAMT = ERRAMT + DLTSTRUCE(L,IEL) + STRUCE(L,IEL)
            DLTSTRUCE(L,IEL) = -STRUCE(L,IEL)
          ENDIF

          DLTSOM1E(L,IEL) = DLTSOM1E(L,IEL) + SOM1E(SRFC,IEL) 
     &                      * FR * MIXPCT / 100.
          IF (DLTSOM1E(L,IEL) + SOM1E(L,IEL) < -1.E-4) THEN
            ERRAMT = ERRAMT + DLTSOM1E(L,IEL) + SOM1E(L,IEL)
            DLTSOM1E(L,IEL) = -SOM1E(L,IEL)
          ENDIF
        END DO   !End of IEL loop.

!       If there are no more soil layers over which to distribute
!       the residue, jump out of the DO loop. 
        IF (IOUT == 2) GOTO 100
      END DO   !End of layer loop.

!     Continue here after jumping out of the DO loop.
100   CONTINUE

!     Correct the SRFC litter pools.
      DLTMETABC(SRFC) = DLTMETABC(SRFC) - METABC(SRFC) * MIXPCT / 100.
      DLTSTRUCC(SRFC) = DLTSTRUCC(SRFC) - STRUCC(SRFC) * MIXPCT / 100.
      DLTLIGC(SRFC)   = DLTLIGC(SRFC)   - LIGC(SRFC)   * MIXPCT / 100.
      DLTSOM1C(SRFC)  = DLTSOM1C(SRFC)  - SOM1C(SRFC)  * MIXPCT / 100.

      DO IEL = 1, N_ELEMS
        DLTMETABE(SRFC,IEL) = DLTMETABE(SRFC,IEL) - METABE(SRFC,IEL) 
     &                        * MIXPCT / 100.
        IF (DLTMETABE(SRFC,IEL) + METABE(SRFC,IEL) < -1.E-4) THEN
          ERRAMT = ERRAMT + DLTMETABE(SRFC,IEL) + METABE(SRFC,IEL)
          DLTMETABE(SRFC,IEL) = -METABE(SRFC,IEL)
        ENDIF

        DLTSTRUCE(SRFC,IEL) = DLTSTRUCE(SRFC,IEL) - STRUCE(SRFC,IEL)
     &                        * MIXPCT / 100.
        IF (DLTSTRUCE(SRFC,IEL) + STRUCE(SRFC,IEL) < -1.E-4) THEN
          ERRAMT = ERRAMT + DLTSTRUCE(SRFC,IEL) + STRUCE(SRFC,IEL)
          DLTSTRUCE(SRFC,IEL) = -STRUCE(SRFC,IEL)
        ENDIF

        DLTSOM1E(SRFC,IEL)  = DLTSOM1E(SRFC,IEL) - SOM1E(SRFC,IEL)
     &                        * MIXPCT / 100.
        IF (DLTSOM1E(SRFC,IEL) + SOM1E(SRFC,IEL) < -1.E-4) THEN
          ERRAMT = ERRAMT + DLTSOM1E(SRFC,IEL) + SOM1E(SRFC,IEL)
          DLTSOM1E(SRFC,IEL) = -SOM1E(SRFC,IEL)
        ENDIF
      END DO   !End of IEL loop.

!     ------------------------------------------------------------------
      RETURN
      END SUBROUTINE INCORPOR_C

!***********************************************************************
! INCORPOR_C variables:
!
! CFMETS1     C flow from the metabolic pool to SOM1 (-)
! CFS1S2      C flow from SOM1 to SOM2  (kg[C] / ha)
! CFSTRS1     C flow from the structural pool to SOM1 (kg[C] / ha)
! CFSTRS2     C flow from the structural pool to SOM2 (kg[C] / ha)
! CO2FMET     CO2 flow that accompanies the C flow out of the metabolic pool
!               (kg[C] / ha)
! CO2FS1      CO2 flow that accompanies the C flow out of SOM1l (kg[C] / ha)
! CO2FSTR     CO2 flow that accompanies the C flow out of the structural pool
!               (kg[C] / ha)
! DLAYR       Thickness of soil layer L (cm)
! DLTLIGC     Rate variable for the change in lignin C (kg[C] / ha)
! DLTMETABC   Rate variable for the change in metabolic C (kg[C] / ha)
! DLTMETABE   Rate variable for the change in metabolic E (kg[E] / ha)
! DLTSOM1C    Rate variable for the change in SOM1 C (kg[C] / ha)
! DLTSOM1E    Rate variable for the change in SOM1 E (kg[E] / ha)
! DLTSTRUCC   Rate variable for the change in structural C (kg[C] / ha)
! DLTSTRUCE   Rate variable for the change in structural E (kg[E] / ha)
! EFMETS1     E flow from soil or soil or surface metabolic residue to soil
!               or surface SOM1 (kg[E] / ha)
! EFS1S2      E flow from soil or surface SOM1 to SOM2 (kg[E] / ha)
! EFSTRS1     E flow from soil or surface structural residue to soil or
!               surface SOM1 (kg[E] / ha)
! EFSTRS2     E flow from soil or soil or surface structural residue to SOM2
!               (kg[E] / ha)
! IMMMETS1    Immobilization of E during the flow from soil or surface metabolic
!               residue to soil or surface SOM1  (kg[E] / ha)
! IMMSTRS1    Immobilization of E during the flow from soil or surface structural
!               residue to soil or surface SOM1  (kg[E] / ha)
! IMMSTRS2    Immobilization of E during the flow from soil or surface structural
!               residue to SOM2  (kg[E] / ha)
! METABC      Metabolic litter C pool (kg[C] / ha)
! METABE      Metabolic litter E pool (kg[E] / ha)
! MNRMETS1    Mineralization of E during the flow from soil or surface metabolic
!               residue to soil or surface SOM1  (kg[E] / ha)
! MNRS1S2     Mineralization of E during the flow from SOM1 to SOM2 (kg[E] / ha)
! MNRSTRS1    Mineralization of E during the flow from soil or surface structural
!               to soil or surface SOM1  (kg[E] / ha)
! MNRSTRS2    Mineralization of E during the flow from soil or surface structural
!               residue to SOM2  (kg[E] / ha)
! N_ELEMS       Number of elements: 1 = N, 2 = N+P, 3 = N+P+S (-)
! NLAYR       Number of soil layers (-)
! METABC      Soil or surface metabolic residue carbon content (units: kg[C] / ha)
! METABE      Soil or surface metabolic residue E content (units: kg[E] / ha)
! SOM1C       Soil or surface SOM1 carbon content (units: kg[C] / ha)
! SOM1E       Soil or surface SOM1 E content (units: kg[E] / ha)
! SOM2C       SOM2 carbon content (units: kg[C] / ha)
! SOM2E       SOM2 E content (units: kg[E] / ha)
! SOM3C       SOM3 carbon content (units: kg[C] / ha)
! SOM3E       SOM3 E content (units: kg[E] / ha)
! STRUCC      Soil or surface structural residue carbon content (units: kg[C] / ha)
! STRUCE      Soil or surface structural residue E content (units: kg[E] / ha)
! MIXDEP      Tillage depth of current tillage event.(cm)
! MIXPCT      Percentage of the surface residues that will be incorporated with the
!               current tillage event (%)
!***********************************************************************
