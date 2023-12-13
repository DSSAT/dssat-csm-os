!***********************************************************************
!  LITDEC_C, subroutine for CENTURY-based SOM/residue module of DSSAT.
!
!  Purpose: This subroutine calculates the decomposition of soil- 
!           and surface-deposited residues.
!
!  Revision history:
!  .......... Parton et al.  Written for CENTURY model.
!  01/01/1999 AJG  Revised, added explanatory text and linked to DSSAT.
!  01/01/2000 CHP/AJG Integrated the CENTURY-based and CERES-based SOM
!               modules with CHP's modular format.
!  11/11/2002 AJG Corrected the use of CES1T for layer thickness.
!  06/23/2003 AJG Limit the amount of lignin that decomposes.
!  01/12/2004 AJG Made the C:E ratio calculations a separate subroutine
!                 CE_RATIO_C.
!  01/19/2004 AJG Added a P option,
!  08/30/2004 AJG Corrected some layout irregularities.
!  05/04/2011 JW  Change DOCULT(0:20) to DOCULT(0:NL)
!
!  Called: CENTURY
!  Calls : EFLOW_C
************************************************************************

      SUBROUTINE LITDEC_C (
     &  CES1, CES23L, CO2MET, CO2STR, CULMETQ, CULSTRQ,   !Input
     &  DECMET, DECSTR, DEFAC, DOCULT, FRLSTR, L,         !Input
     &  LIGSTR, METABC, METABE, N_ELEMS, STRUCC,          !Input
     &  STRUCE,                                           !Input
     &  CFMETS1, CFSTRS1, CFSTRS2, CFSTRS23, CO2FMET,     !Output
     &  CO2FSTR, EFMETS1, EFSTRS1, EFSTRS2, EFSTRS23,     !Output
     &  IMMMETS1, IMMSTRS1, IMMSTRS2, IMMSTRS23,          !Output
     &  MNRMETS1, MNRSTRS1, MNRSTRS2, MNRSTRS23)          !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL EFLOW_C
      SAVE
!     ------------------------------------------------------------------
      LOGICAL DOCULT(0:NL)

      INTEGER L, N_ELEMS

      INTEGER, PARAMETER :: NONLIG = 1, LIG = 2
      INTEGER, PARAMETER :: SRFC = 0, SOIL = 1
!     INTEGER, PARAMETER :: N = 1, P = 2

      REAL CULMETQ, CULSTRQ, KEEPOLD, TCFLOW

      REAL CFMETS1(0:NL), CFSTRS1(0:NL), CFSTRS2(0:NL), 
     &  CFSTRS23(0:NL), CO2FMET(0:NL), CO2MET(0:1), 
     &  CULMET(0:NL), CULSTR(0:NL), DECMET(0:1),
     &  DECSTR(0:1), DEFAC(0:NL), FRLSTR(0:NL),
     &  LIGSTR(0:1), METABC(0:NL), STRUCC(0:NL)

      REAL CES1(0:NL,3), CES23L(0:NL,3), CES2L(0:NL,3),
     &  CO2FSTR(0:NL,2), CO2STR(0:1,2), EFMETS1(0:NL,3),
     &  EFSTRS1(0:NL,3), EFSTRS2(0:NL,3), EFSTRS23(0:NL,3),
     &  METABE(0:NL,3), IMMMETS1(0:NL,3), IMMSTRS1(0:NL,3),
     &  IMMSTRS2(0:NL,3), IMMSTRS23(0:NL,3), MNRMETS1(0:NL,3),
     &  MNRSTRS1(0:NL,3), MNRSTRS2(0:NL,3), MNRSTRS23(0:NL,3),
     &  STRUCE(0:NL,3) 

!     The SRFC layer has array index '0' and the soil layers '1' to
!     '20'. If there is no distinction between the different soil
!     layers, array index SOIL (which equals '1') is used. AMINRL,
!     SOM2, SOM3 and the parameters related to these pools do not
!     exist for the surface layer. The SRFC layer uses the mineral N
!     from the topsoil layer.

************************************************************************
*     The layer index of the variables refers to the layer where the   *
*     decomposition takes place and not the layer where the OM ends up.*
*     Thus the SRFC structural lignin decomposes in L=SRFC, but the    *
*     newly formed SOM2 goes to L=1. The E flow is EFSTRS2(SRFC,IEL)   * 
*     and the C/E ratio of the SOM2 is CES2L(SRFC,IEL).                *
*     The immobilization needed is IMMSTRS2(SRFC,IEL) and the          *
*     mineralization is MNRSTRS2(SRFC,IEL), though the SRFC layer has  *
*     no mineral nutrients.                                            *
*     In IMMOBLIMIT, the nutrients involved in the SRFC layer are      *
*     dealt with by layer L=1                                          *
************************************************************************

!     ------------------------------------------------------------------
!     Daily initializations.
!     ------------------------------------------------------------------
!     Set all the flows to zero. 
!     ================
!     Surface and Soil
!     ================
      CFMETS1(L) = 0.
      CFSTRS1(L) = 0.
      CFSTRS2(L) = 0.

      CO2FMET(L)        = 0.
      CO2FSTR(L,LIG)    = 0.
      CO2FSTR(L,NONLIG) = 0.

!     Set the E flows and related immobilizations / mineralizations to zero.
!     ================
!     Surface and Soil
!     ================
!     N and P flows.
      EFMETS1(L,N)  = 0. ; EFMETS1(L,P)   = 0.
      EFSTRS1(L,N)  = 0. ; EFSTRS1(L,P)   = 0.
      EFSTRS2(L,N)  = 0. ; EFSTRS23(L,P)  = 0.
      IMMMETS1(L,N) = 0. ; IMMMETS1(L,P)  = 0.
      IMMSTRS1(L,N) = 0. ; IMMSTRS1(L,P)  = 0.
      IMMSTRS2(L,N) = 0. ; IMMSTRS23(L,P) = 0.
      MNRMETS1(L,N) = 0. ; MNRMETS1(L,P)  = 0.
      MNRSTRS1(L,N) = 0. ; MNRSTRS1(L,P)  = 0.
      MNRSTRS2(L,N) = 0. ; MNRSTRS23(L,P) = 0.

!     Set effect of cultivation on decomposition rate.
!     ================
!     Surface and Soil
!     ================
      IF (DOCULT(L)) THEN
        CULMET(L) = CULMETQ
        CULSTR(L) = CULSTRQ
      ELSE
        CULMET(L) = 1.
        CULSTR(L) = 1.
      ENDIF

!     ##################################################################
!     >>> Section on SOM-C and SOM-N <<<
!     ##################################################################

!     ----------------------------------------------------------------
!     Decomposition of surface metabolic residue.
!     *** C + N ***
!     ----------------------------------------------------------------
      IF (L == SRFC .AND. METABC(SRFC) > 1.E-06) THEN
!       --------------------------------------------------------------
!       C flow from surface metabolic to surface SOM1 and CO2.
!       --------------------------------------------------------------
!       Calculate the total C flow out of the surface metabolic
!       residue.
!        TCFLOW = METABC(SRFC) * DEFAC(SRFC) * CULMET(SRFC) *
!     &  DECMET(SRFC) 

!       1/29/08 CHP use factors for layer 1 for all decomposition rate
!         modifiers
        TCFLOW = METABC(SRFC) * DEFAC(SRFC) * CULMET(SRFC) *DECMET(SRFC)

        IF (TCFLOW > METABC(SRFC)) TCFLOW = METABC(SRFC)

!       Calculate the CO2 respiration associated with the C flow
!       from surface metabolic residue to surface SOM1.
        CO2FMET(SRFC) = TCFLOW * CO2MET(SRFC)

!       Correct the C flow from surface metabolic residue to
!       surface SOM1 for the CO2 lost to respiration.
        CFMETS1(SRFC) = TCFLOW - CO2FMET(SRFC)

!       --------------------------------------------------------------
!       N flow from surface metabolic.
!       --------------------------------------------------------------
        IF (N_ELEMS > 0) THEN
!         Do the N flow associated with the C flow from surface metabolic
!         to surface SOM1 and also do the N mineralization or immobilization
!         that goes with this flow and with the CO2 respiration.
          CALL EFLOW_C (
     &      METABC(SRFC), METABE(SRFC,N), CES1(SRFC,N),   !Input
     &      CFMETS1(SRFC), CO2FMET(SRFC),                 !Input
     &      EFMETS1(SRFC,N), IMMMETS1(SRFC,N),            !Output
     &      MNRMETS1(SRFC,N))                             !Output
        ENDIF   !End of IF block on N_ELEMS

!       --------------------------------------------------------------
!       P flow from surface metabolic.
!       --------------------------------------------------------------
        IF (N_ELEMS > 1) THEN
!         Do the P flow associated with the C flow from surface metabolic
!         to surface SOM1 and also do the P mineralization or immobilization
!         that goes with this flow and with the CO2 respiration.
          CALL EFLOW_C (
     &      METABC(SRFC), METABE(SRFC,P), CES1(SRFC,P),   !Input
     &      CFMETS1(SRFC), CO2FMET(SRFC),                 !Input
     &      EFMETS1(SRFC,P), IMMMETS1(SRFC,P),            !Output
     &      MNRMETS1(SRFC,P))                             !Output
        ENDIF   !End of IF block on N_ELEMS
      ENDIF     !End of L == SRFC .AND. METABC(SRFC) > 1.E-06

!     ----------------------------------------------------------------
!     Decomposition of surface structural residue to surface SOM1
!     and SOM2 of layer 1.
!     ----------------------------------------------------------------
      IF (L == SRFC .AND. STRUCC(SRFC) > 1.E-06) THEN
!       --------------------------------------------------------------
!       C flow from surface structural to surface SOM1, SOM2, SOM23 
!       and CO2.
!       --------------------------------------------------------------
!       Calculate the total C flow out of the surface structural
!       residue. The lignin fraction flows to SOM2; non-lignin flows
!       to surface SOM1.
!       TCFLOW = STRUCC(SRFC) * DEFAC(SRFC) * CULSTR(SRFC) *
!    &    DECSTR(SRFC) *EXP(-LIGSTR(SRFC) * FRLSTR(SRFC))

!       1/29/08 CHP use factors for layer 1 for all decomposition rate
!         modifiers
        TCFLOW = STRUCC(SRFC) * DEFAC(SRFC) * CULSTR(SRFC) *
     &    DECSTR(SRFC) *EXP(-LIGSTR(SRFC) * FRLSTR(SRFC))

        IF (TCFLOW > STRUCC(SRFC)) TCFLOW = STRUCC(SRFC)

!       Let the surface structural lignin flow to SOM2.
        CFSTRS2(SRFC) = TCFLOW * FRLSTR(SRFC)

!       Check that no more lignin decomposes than there is.
        IF (CFSTRS2(SRFC) > FRLSTR(SRFC) * STRUCC(SRFC)) THEN
!         Keep the old value for correcting TCFLOW.
          KEEPOLD = CFSTRS2(SRFC)

!         Limit CFSTRS2 and redo calculations of CO2FSTR.
          CFSTRS2(SRFC)     = FRLSTR(SRFC)  * STRUCC(SRFC)
          CO2FSTR(SRFC,LIG) = CFSTRS2(SRFC) * CO2STR(SRFC,LIG)
          CFSTRS2(SRFC)     = CFSTRS2(SRFC) - CO2FSTR(SRFC,LIG)

!         Limit TCFLOW with the lignin correction.
          TCFLOW = TCFLOW - KEEPOLD + CFSTRS2(SRFC)
        ELSE
!         Calculate the respiration associated with the surface
!         structural lignin flow to SOM2.
          CO2FSTR(SRFC,LIG) = CFSTRS2(SRFC) * CO2STR(SRFC,LIG)

!         Correct the C flow from surface structural lignin to SOM2
!         for the CO2 lost to respiration.
          CFSTRS2(SRFC) = CFSTRS2(SRFC) - CO2FSTR(SRFC,LIG)
        ENDIF

!       Calculate the gross C flow from surface structural non-
!       lignin into surface SOM1.
        CFSTRS1(SRFC) = TCFLOW - CFSTRS2(SRFC) - CO2FSTR(SRFC,LIG)

!       Calculate the respiration associated with the flow from
!       surface structural non-lignin into surface SOM1.
        CO2FSTR(SRFC,NONLIG) = CFSTRS1(SRFC) * CO2STR(SRFC,NONLIG)

!       Correct the C flow from surface structural non-lignin to
!       surface SOM1 for the CO2 lost to respiration.
        CFSTRS1(SRFC) = CFSTRS1(SRFC) - CO2FSTR(SRFC,NONLIG)

!       --------------------------------------------------------------
!       N flow from SRFC structural lignin and non-lignin.
!       --------------------------------------------------------------
        IF (N_ELEMS > 0) THEN
!         Do the N flow associated with the C flow from surface structur
!         non-lignin to surface SOM1 and also do the N mineralization or
!         immobilization that goes with this flow and with the CO2
!         respiration.
          CALL EFLOW_C (
     &      STRUCC(SRFC), STRUCE(SRFC,N), CES1(SRFC,N),   !Input
     &      CFSTRS1(SRFC), CO2FSTR(SRFC,NONLIG),          !Input
     &      EFSTRS1(SRFC,N), IMMSTRS1(SRFC,N),            !Output
     &      MNRSTRS1(SRFC,N))                             !Output

!         Do the same for surface structural lignin to SOM2 of layer 1.
          CALL EFLOW_C (
     &      STRUCC(SRFC), STRUCE(SRFC,N), CES2L(SRFC,N),  !Input
     &      CFSTRS2(SRFC), CO2FSTR(SRFC,LIG),             !Input
     &      EFSTRS2(SRFC,N), IMMSTRS2(SRFC,N),            !Output
     &      MNRSTRS2(SRFC,N))                             !Output
        ENDIF   !End of N_ELEMS

!       ----------------------------------------------------------------
!       P flow from SRFC structural lignin and non-lignin.
!       ----------------------------------------------------------------
!       If there is surface structural residue P.
        IF (N_ELEMS > 1) THEN
!         Do the P flow associated with the C flow from surface structural
!         non-lignin to surface SOM1 and also do the P mineralization or
!         immobilization that goes with this flow and with the CO2
!         respiration.
          CALL EFLOW_C (
     &      STRUCC(SRFC), STRUCE(SRFC,P), CES1(SRFC,P),   !Input
     &      CFSTRS1(SRFC), CO2FSTR(SRFC,NONLIG),          !Input
     &      EFSTRS1(SRFC,P), IMMSTRS1(SRFC,P),            !Output
     &      MNRSTRS1(SRFC,P))                             !Output

!         Do the same for surface structural lignin to SOM23 of layer 1.
!         Note: For the SRFC layer the C that flows to SOM23 is the
!         same as what goes to SOM2 with N.
          CFSTRS23(SRFC) = CFSTRS2(SRFC)

          CALL EFLOW_C (
     &      STRUCC(SRFC), STRUCE(SRFC,P), CES23L(SRFC,P), !Input
     &      CFSTRS23(SRFC), CO2FSTR(SRFC,LIG),            !Input
     &      EFSTRS23(SRFC,P), IMMSTRS23(SRFC,P),          !Output
     &      MNRSTRS23(SRFC,P))                            !Output
        ENDIF   !End of IF block on N_ELEMS
      ENDIF     !End of L == SRFC .AND. STRUCC(SRFC) > 1.E-06

!     ----------------------------------------------------------------
!     Decomposition of soil metabolic residue
!     *** C + N ***
!     ----------------------------------------------------------------
      IF (L /= SRFC .AND. METABC(L) > 1.E-06) THEN

!       --------------------------------------------------------------
!       C flow from soil metabolic to soil SOM1, and CO2.
!       --------------------------------------------------------------
!       Calculate the total C flow out of the soil metabolic
!       residue.
        TCFLOW = METABC(L) * DEFAC(L) * DECMET(SOIL) * CULMET(L)
        IF (TCFLOW > METABC(L)) TCFLOW = METABC(L)

!       Calculate the CO2 respiration associated with the C flow
!       from soil metabolic residue to soil SOM1.
        CO2FMET(L) = TCFLOW * CO2MET(SOIL)

!       Correct the C flow from soil metabolic residue to soil SOM1
!       for the CO2 lost to respiration.
        CFMETS1(L) = TCFLOW - CO2FMET(L)

!       --------------------------------------------------------------
!       N flow from soil metabolic.
!       --------------------------------------------------------------
        IF (N_ELEMS > 0) THEN
!         Do the N flow associated with the C flow from soil metabolic
!         to surface SOM1 and also do the N mineralization or immobilization
!         that goes with this flow and with the CO2 respiration.
          CALL EFLOW_C (
     &      METABC(L), METABE(L,N), CES1(L,N),            !Input
     &      CFMETS1(L), CO2FMET(L),                       !Input
     &      EFMETS1(L,N), IMMMETS1(L,N), MNRMETS1(L,N))   !Output
        ENDIF   !End of N_ELEMS

!       ----------------------------------------------------------------
!       P flow from soil metabolic.
!       ----------------------------------------------------------------
        IF (N_ELEMS > 1) THEN
!         Do the P flow associated with the C flow from soil metabolic
!         to soil SOM1 and also do the P mineralization or immobilization
!         that goes with this flow and with the CO2 respiration.
          CALL EFLOW_C (
     &      METABC(L), METABE(L,P), CES1(L,P),            !Input
     &      CFMETS1(L), CO2FMET(L),                       !Input
     &      EFMETS1(L,P), IMMMETS1(L,P), MNRMETS1(L,P))   !Output
        ENDIF   !End of IF block on METABE(L,P)
      ENDIF     !End of L /= SRFC .AND. METABC(L) > 1.E-06
  
!     ----------------------------------------------------------------
!     Decomposition of soil structural residue.
!     ----------------------------------------------------------------
      IF (L /= SRFC .AND. STRUCC(L) > 1.E-06) THEN

!       --------------------------------------------------------------
!       C flow from soil structural to soil SOM1, SOM2 and CO2.
!       --------------------------------------------------------------
!       Calculate the total C flow out of the soil structural
!       residue.
        TCFLOW = STRUCC(L) * DEFAC(L) * DECSTR(SOIL) *
     &    EXP(-LIGSTR(SOIL) * FRLSTR(L)) * CULSTR(L)
        IF (TCFLOW > STRUCC(L)) TCFLOW = STRUCC(L)

!       --------------------------------------------------------------
!       Flow of soil structural lignin to SOM2; non-lignin soil
!       structural residue goes to soil SOM1.
!       --------------------------------------------------------------
!       Let the soil structural lignin flow to SOM2.
        CFSTRS2(L) = TCFLOW * FRLSTR(L)

!       Check that no more lignin decomposes than there is.
        IF (CFSTRS2(L) > FRLSTR(L) * STRUCC(L)) THEN
!         Keep the old value for correcting TCFLOW.
          KEEPOLD = CFSTRS2(L)

!         Limit CFSTRS2 and redo calculations of CO2FSTR.
          CFSTRS2(L)     = FRLSTR(L) * STRUCC(L)
          CO2FSTR(L,LIG) = CFSTRS2(L) * CO2STR(SOIL,LIG)
          CFSTRS2(L)     = CFSTRS2(L) - CO2FSTR(L,LIG)

!         Limit TCFLOW with the lignin correction.
          TCFLOW = TCFLOW - KEEPOLD + CFSTRS2(L)
        ELSE
!         Calculate the respiration associated with the soil
!         structural lignin flow to SOM2.
          CO2FSTR(L,LIG) = CFSTRS2(L) * CO2STR(SOIL,LIG)

!         Correct the C flow from soil structural lignin to SOM2 for
!         the CO2 lost to respiration.
          CFSTRS2(L) = CFSTRS2(L) - CO2FSTR(L,LIG)
        ENDIF

!       Calculate the gross C flow from soil structural non-lignin
!       into soil SOM1.
        CFSTRS1(L) = TCFLOW - CFSTRS2(L) - CO2FSTR(L,LIG)

!       Calculate the respiration associated with the flow from
!       soil structural non-lignin into soil SOM1.
        CO2FSTR(L,NONLIG) = CFSTRS1(L) * CO2STR(SOIL,NONLIG)

!       Correct the C flow from soil structural non-lignin to soil
!       SOM1 for the CO2 lost to respiration.
        CFSTRS1(L) = CFSTRS1(L) - CO2FSTR(L,NONLIG)

!       --------------------------------------------------------------
!       N flow from soil structural lignin and non-lignin
!       --------------------------------------------------------------
        IF (N_ELEMS > 0) THEN
!         Do the N flow associated with the C flow from soil structural 
!         non-lignin to soil SOM1 and also do the N mineralization or 
!         immobilization that goes with this flow and with the CO2
!         respiration.
          CALL EFLOW_C (
     &      STRUCC(L), STRUCE(L,N), CES1(L,N),            !Input
     &      CFSTRS1(L), CO2FSTR(L,NONLIG),                !Input
     &      EFSTRS1(L,N), IMMSTRS1(L,N), MNRSTRS1(L,N))   !Output

          CALL EFLOW_C (
     &      STRUCC(L), STRUCE(L,N), CES2L(L,N),           !Input
     &      CFSTRS2(L), CO2FSTR(L,LIG),                   !Input
     &      EFSTRS2(L,N), IMMSTRS2(L,N), MNRSTRS2(L,N))   !Output

        ENDIF   !End of N_ELEMS

!       ----------------------------------------------------------------
!       P flow from surface metabolic.
!       ----------------------------------------------------------------
        IF (N_ELEMS > 1) THEN
!         Do the P flow associated with the C flow from soil structural
!         non-lignin to soil SOM1 and also do the P mineralization or
!         immobilization that goes with this flow and with the CO2
!         respiration.
          CALL EFLOW_C (
     &      STRUCC(L), STRUCE(L,P), CES1(L,P),            !Input
     &      CFSTRS1(L), CO2FSTR(L,NONLIG),                !Input
     &      EFSTRS1(L,P), IMMSTRS1(L,P), MNRSTRS1(L,P))   !Output

!         Note: The C that flows to SOM23 is the same as what goes to
!         SOM2 with N.
          CFSTRS23(L) = CFSTRS2(L)

!         Do the same for soil structural lignin to SOM2 of layer L.
          CALL EFLOW_C (
     &      STRUCC(L), STRUCE(L,P), CES23L(L,P),          !Input
     &      CFSTRS23(L), CO2FSTR(L,LIG),                  !Input
     &      EFSTRS23(L,P), IMMSTRS23(L,P),                !Output
     &      MNRSTRS23(L,P))                               !Output
        ENDIF   !End of IF block on N_ELEMS
      ENDIF   !End of L /= SRFC .AND. STRUCC(L) > 1.E-06

      RETURN
      END SUBROUTINE LITDEC_C

!***********************************************************************
! LITDEC_C variables:
!
! AMINRL(LYR,IEL)       Available mineral E in a soil layer (kg[E] / ha)
! CEADD(IEL)            Addition factor for the C/E ratio of newly formed SOM2, coming
!                         from surface structural material or surface SOM1
!                         (kg[C] / kg[E])
! CES1(LYR,IEL)         C/E ratio for newly formed soil or surface SOM1, coming from
!                         soil or surface residue (kg[C] / kg[E])
!                         Also: initial value of the C/E ratio of soil or surface SOM1
!                         (kg[C] / kg[E])
! CES1M(SOIL/SRFC,IEL)  Minimum allowable C/E ratio for newly formed soil or 
!                         surface SOM1, coming from soil or surface residue
!                         (kg[C] / kg[E])
! CES1S(SOIL/SRFC,IEL)  Slope value for the calculation of the C/E ratio of newly
!                         formed SOM1, coming from soil or surface residue
!                         (SOIL: kg[C].ha / kg2[E]; SRFC: kg[C].kg[DM] / kg2[E])
! CES1T(SOIL,IEL)       Threshold value of the soil mineral E content, above which
!                         the C/E ratio of newly formed soil SOM1 equals
!                         CES1M(SOIL,IEL) (kg[E] / ha)
! CES1T(SRFC,IEL)       Threshold value of the E concentration of decomposing surface
!                         residue, above which the C/E ratio of newly formed surface
!                         SOM1 equals CES1M(SRFC,IEL) (kg[E] / kg[DM])
! CES1X(SOIL/SRFC,IEL)  Maximum allowable C/E ratio for newly formed soil or surface
!                          SOM1, coming from soil or surface residue (kg[C] / kg[E])
! CES2L(LYR,IEL)        C/E ratio for newly formed SOM2, coming from soil or surface
!                          structural lignin residue (kg[C] / kg[E])
! CES2LI(SRFC,IEL)      Intercept value for the calculation of the C/E ratio of newly
!                          formed SOM2, coming from surface structural lignin residue 
!                          (kg[C] / kg[E])
! CES2LM(SRFC,IEL)      Minimum allowable C/E ratio for newly formed SOM2, coming from
!                          surface structural lignin residue (kg[C] / kg[E])
! CES2LS(SRFC,IEL)      Slope value for the calculation of the C/E ratio of newly formed
!                         SOM2, coming from surface structural lignin residue (units?)
! CES2LX(SOIL/SRFC,IEL) Maximum allowable C/E ratio for newly formed SOM2, coming from
!                         soil or surface structural lignin residue (kg[C] / kg[E])
! CFMETS1               C flow from the metabolic pool to SOM1 (-)
! CFSTRS1               C flow from the structural pool to SOM1 (kg[C] / ha)
! CFSTRS2               C flow from the structural pool to SOM2 (kg[C] / ha)
! CO2FMET               CO2 flow that accompanies the C flow out of the metabolic pool
!                         (kg[C] / ha)
! CO2FSTR               CO2 flow that accompanies the C flow out of the structural pool
!                         (kg[C] / ha)
! CO2MET(SOIL/SRFC)     C fraction lost to CO2 respiration when soil or surface metabolic
!                         residue decomposes to soil or surface SOM1 (units?)
! CO2STR(SOIL/SRFC)     C fraction lost to CO2 when soil or surface structural non-lignin
!                         residue decomposes to soil of surface SOM1 (units?)
! CULMET(LYR)           Effect of cultivation on the decomposition rate of soil or surface
!                         metabolic residue. Is set to CULME$ when cultivation occurs and
!                         functions as a multiplier on the decomposition rate (-)
! CULMETQ               Effect of cultivation on the decomposition rate of soil or surface
!                         metabolic residue (-)
! CULSTR(LYR)           Effect of cultivation on decomposition rate of soil structural
!                         residue. Is set to CULST$ when cultivation occurs and functions
!                         as a multiplier on the decomposition rate (-)
! CULSTRQ               Effect of cultivation on the decomposition rate of soil structural
!                         residue (-)
! DECMET(SOIL/SRFC)     Maximum decomposition rate of soil or surface metabolic residue
!                         under optimal conditions (but without increased decomposition 
!                         due to soil disturbance; see CULME) (1/d)
! DECSTR(SOIL/SRFC)     Maximum decomposition rate of soil or surface structural residue
!                         under optimal conditions (but without increased decomposition
!                         due to soil disturbance; see CULST) (1/d)
! DEFAC(LYR)            Decomposition factor that represents the effect of temperature
!                         and low soil water conditions on the decomposition rate parameter
!                         functions as a multiplier on the maximum decomposition rate
!                         (DECMET, DECSTR, DECS1, DECS2, DECS3) (range 0-1) (-)
! EFMETS1               E flow from soil or soil or surface metabolic residue to soil
!                         or surface SOM1 (kg[E] / ha)
! EFSTRS1               E flow from soil or surface structural residue to soil or
!                         surface SOM1 (kg[E] / ha)
! EFSTRS2               E flow from soil or soil or surface structural residue to SOM2
!                         (kg[E] / ha)
! FREMET                E concentration (fraction) of decomposing metabolic residue
!                         (kg[E] / kg[DM])
! FRESTR                E concentration (fraction) of decomposing structural residue
!                         (kg[E] / kg[DM])
! FRLSTR(LYR)           Lignin concentration (fraction) of the structural soil or
!                         surface residue that was already in the system (kg[lignin] / kg[DM])
! IEL                   Element number. 1 = N; 2 = P; 3 = S (-)
! IMMMETS1              Immobilization of E during the flow from soil or surface metabolic
!                         residue to soil or surface SOM1  (kg[E] / ha)
! IMMSTRS1              Immobilization of E during the flow from soil or surface structural
!                         residue to soil or surface SOM1  (kg[E] / ha)
! IMMSTRS2              Immobilization of E during the flow from soil or surface structural
!                         residue to SOM2  (kg[E] / ha)
! LIG                   Array index indicating whether the variable refers to the lignin
!                         component of the soil or surface structural residue pool or
!                         the non-lignin component (see NONLIG). LIG = 1 (-)
! LIGSTR(SOIL/SRFC)     Effect of lignin on the decomposition rate of soil or surface
!                         structural residue (-)
! METABC                Soil or surface metabolic residue carbon content (kg[C] / ha)
! METABE                Soil or surface metabolic residue E content (kg[E] / ha)
! MNRMETS1              Mineralization of E during the flow from soil or surface metabolic
!                         residue to soil or surface SOM1  (kg[E] / ha)
! MNRSTRS1              Mineralization of E during the flow from soil or surface structural
!                         to soil or surface SOM1  (kg[E] / ha)
! MNRSTRS2              Mineralization of E during the flow from soil or surface structural
!                         residue to SOM2  (kg[E] / ha)
! NL                    Maximum number of soil layers used in the array definitions (-)
! NONLIG                Array index indicating whether the variable refers to the lignin
!                         component of the soil or surface structural residue pool (see LIG)
!                         or the non-lignin component. NONLIG = 2 (-)
! STRUCC                Soil or surface structural residue carbon content (kg[C] / ha)
! STRUCE                Soil or surface structural residue E content (kg[E] / ha)
! SOIL                  Identifier for the soil layer as a whole (i.e. no individual layers)
!                       . SOIL = 1 (-)
! SRFC                  Identifier for the surface layer. SRFC = 0 (-)
! TCFLOW                Total amount of C flowing from one residue or SOM pool to another
!                         (kg[N] / ha)
!***********************************************************************
