!***********************************************************************
!  SOMDEC_C, subroutine
!
!  Purpose: This subroutine deals with the carbon and nutrient flows
!          between the different SOM pools (not: the flow out
!          of the litter; see LITDEC), as a consequence of SOM
!          decomposition.
!
!  Revision history:
!  .......... Parton et al.  Written for CENTURY model.
!  01/01/1999 AJG  Revised, added explanatory text and linked to DSSAT.
!  01/01/2000 CHP/AJG Integrated the CENTURY-based and CERES-based SOM
!               modules with CHP's modular format.
!  11/11/2002 AJG Corrected the use of CES21T and CES3T.
!  10/31/2003 AJG Fixed some remaining comparisons that were done with
!                 the amount of E instead of its concentration and added
!                 some explanatory text.
!  01/12/2004 AJG Made the C:E ratio calculations a separate subroutine
!                 CE_RATIO_C.
!  01/15/2004 AJG Made new SOM P pool structure with SOM23
!  05/16/2004 AJG Added a DO IEL = 1, N_ELEMS loop.
!  11/12/2004 AJG Caught many errors in the P code and got it functioning
!                 for the first time, but calibration still needed.
!  02/10/2009 CHP Remove effects of DMOD for CENTURY model
!
!  Called: CENTURY
!  Calls : EFLOW_C
!***********************************************************************

      SUBROUTINE SOMDEC_C ( 
     &    CES1, CES21, CES23, CES3, CO2S1, CO2S2, CO2S3,  !Input
     &    CULS1Q, CULS2Q, CULS3Q, DECS1, DECS2, DECS3,    !Input
     &    DEFAC, DOCULT, L, N_ELEMS, S1S3,                !Input
     &    S2S3, SOM1C, SOM1E, SOM23C, SOM23E, SOM2C,      !Input
     &    SOM2E, SOM3C, SOM3E, TXS1,                      !Input

     &    CFS1S2, CFS1S3, CFS2S1, CFS2S3, CFS3S1, CO2FS1, !Output
     &    CO2FS2, CO2FS3, EFS1S2, EFS1S23, EFS1S3,        !Output
     &    EFS23S1, EFS2S1, EFS2S3, EFS3S1, IMMS1S2,       !Output
     &    IMMS1S23, IMMS1S3, IMMS23S1, IMMS2S1, IMMS2S3,  !Output
     &    IMMS3S1, MNRS1S2, MNRS1S3, MNRS23S1, MNRS2S1,   !Output
     &    MNRS2S3, MNRS3S1)                               !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL EFLOW_C
      SAVE
!     ------------------------------------------------------------------
!     The SRFC layer has array index '0' and the soil layers '1' to
!     'NL'. If there is no distinction between the different soil
!     layers, array index SOIL (which equals '1') is used. AMINRL,
!     SOM2, SOM3 and the parameters related to these pools do not
!     exist for the surface layer. The SRFC layer uses the AMINRL
!     from the topsoil layer.

      LOGICAL DOCULT(0:NL)

      INTEGER L, N_ELEMS 

      INTEGER, PARAMETER :: SRFC = 0, SOIL = 1 

      REAL CO2S2, CO2S3, CULS1Q, CULS2Q, CULS3Q,
     &  TCFLOW
      REAL, PARAMETER :: RDUMMY = -99.

      REAL CFS1S2(0:NL), CFS1S23(0:NL), CFS1S3(NL), CFS23S1(NL),
     &  CFS2S1(NL), CFS2S3(NL), CFS3S1(NL), CO2FS1(0:NL), CO2FS2(NL),
     &  CO2FS23(NL), CO2FS3(NL), CO2S1(0:NL), CULS1(0:NL), CULS2(NL),
     &  CULS3(NL), DECS1(0:1), DECS2(1), DECS3(1),
     &  DEFAC(0:NL),
     &  S1S3(NL), S2S3(NL), SOM1C(0:NL), SOM2C(NL), SOM23C(NL),
     &  SOM3C(NL),  TXS1(NL)

      REAL CES1(0:NL,3), CES21(0:NL,3),
     &  CES23(0:NL,3), CES3(NL,3),    
     &  EFS1S2(0:NL,3), EFS1S3(NL,3),
     &  EFS1S23(0:NL,3), EFS2S1(NL,3), EFS2S3(NL,3), EFS23S1(NL,3),
     &  EFS3S1(NL,3), IMMS1S2(0:NL,3), IMMS1S23(0:NL,3), IMMS1S3(NL,3),
     &  IMMS23S1(NL,3), IMMS2S1(NL,3), IMMS2S3(NL,3),
     &  IMMS3S1(NL,3), MNRS1S2(0:NL,3), MNRS1S23(0:NL,3), MNRS1S3(NL,3),
     &  MNRS23S1(NL,3), MNRS2S1(NL,3), MNRS2S3(NL,3), MNRS3S1(NL,3),
     &  SOM1E(0:NL,3), SOM2E(NL,3), SOM23E(NL,3), SOM3E(NL,3)

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

!***********************************************************************
!     ----------------------------------------------------------------
!     Daily initializations.
!     ----------------------------------------------------------------
!     Carbon flows
      CFS1S2(L)  = 0.
      CFS1S23(L) = 0.
      CO2FS1(L)  = 0.
      IF (L > SRFC) THEN
        CFS1S3(L)  = 0.
        CFS2S1(L)  = 0.
        CFS2S3(L)  = 0.
        CFS3S1(L)  = 0.
        CFS23S1(L) = 0.
        CO2FS2(L)  = 0.
        CO2FS3(L)  = 0.
        CO2FS23(L) = 0.
      ENDIF

!     Nitrogen flows
      EFS1S2(L,N)  = 0.
      IMMS1S2(L,N) = 0.
      MNRS1S2(L,N) = 0.
      IF (L > SRFC) THEN
        EFS1S3(L,N)  = 0.
        EFS2S1(L,N)  = 0.
        EFS2S3(L,N)  = 0.
        EFS3S1(L,N)  = 0. 
        IMMS1S3(L,N) = 0.
        IMMS2S1(L,N) = 0.
        IMMS2S3(L,N) = 0.
        IMMS3S1(L,N) = 0.
        MNRS1S3(L,N) = 0.
        MNRS2S1(L,N) = 0.
        MNRS2S3(L,N) = 0.
        MNRS3S1(L,N) = 0.
      ENDIF

!     Phosphorus flows
      EFS1S23(L,P)  = 0.
      IMMS1S23(L,P) = 0.
      MNRS1S23(L,P) = 0.
      IF (L > SRFC) THEN
        EFS23S1(L,P)  = 0. 
        IMMS23S1(L,P) = 0.
        MNRS23S1(L,P) = 0.
      ENDIF

!     ------------------------------------------------------------------
!     Set effect of cultivation on decomposition rate.
      IF (DOCULT(L)) THEN
        CULS1(L)  = CULS1Q
        IF (L > SRFC) THEN
          CULS2(L)  = CULS2Q
          CULS3(L)  = CULS3Q
          !CULS23(L) = CULS3Q
        ENDIF   !End of IF bock on L.
      ELSE
        CULS1(L)  = 1.
        IF (L > SRFC) THEN
          CULS2(L)  = 1.
          CULS3(L)  = 1.
          !CULS23(L) = 1.
        ENDIF   !End of IF bock on L.
      ENDIF   !End of IF block on DOCULT.

!     ################################################################
!     >>> Section on SOM-C and SOM-N <<<
!     ################################################################

!     ----------------------------------------------------------------
!     Decomposition of surface SOM1 to SOM2 and to CO2.
!     *** C + N ***
!     ----------------------------------------------------------------
!      IF (L == SRFC .AND. IEL == N) THEN
!        IF (SOM1C(SRFC) > 1.E-06) THEN
! 6/26/07 CHP Decompose C even if N_ELEMS < 1

!     If there is surface SOM1.
      IF (L == SRFC .AND. SOM1C(SRFC) > 1.E-06) THEN

!       Calculate the total C flow out of the surface SOM1 (the SRFC
!       layer is not affected by DMOD).
!        TCFLOW = SOM1C(SRFC) * DEFAC(SRFC) * DECS1(SRFC) *
!     &    CULS1(SRFC)

!       1/29/08 CHP use factors for layer 1 for all decomposition rate
!         modifiers
        TCFLOW = SOM1C(SRFC) * DEFAC(1) * DECS1(SRFC) *
     &    CULS1(SRFC) * TXS1(1) !* DMOD

        IF (TCFLOW > SOM1C(SRFC)) TCFLOW = SOM1C(SRFC)

!       Calculate the CO2 respiration associated with the C flow
!       from surface SOM1 to SOM2 (layer 1).
!       CO2FS1(SRFC) = TCFLOW * CO2S1(SRFC)

!       1/29/08 CHP use factors for layer 1 for all resp. rates
        CO2FS1(SRFC) = TCFLOW * CO2S1(1)

!       Correct the C flow from surface SOM1 to SOM2 for the CO2
!       lost to respiration.
        CFS1S2(SRFC) = TCFLOW - CO2FS1(SRFC)

        IF (N_ELEMS > 0) THEN
!         Do the N flow associated with the C flow from surface SOM1 to SOM2,
!         and also do the N mineralization or immobilization that goes
!         with this flow and with the CO2 respiration.
          CALL EFLOW_C (
     &      SOM1C(SRFC), SOM1E(SRFC,N), CES21(SRFC,N),  !Input
     &      CFS1S2(SRFC), CO2FS1(SRFC),                 !Input
     &      EFS1S2(SRFC,N), IMMS1S2(SRFC,N),            !Output
     &      MNRS1S2(SRFC,N))                            !Output
        ENDIF

        IF (N_ELEMS > 1) THEN
!         For EFLOW, the amount of C flowing to SOM23 is needed. This
!         is no different from what goes to SOM2 and SOM3 for N.
          CFS1S23(SRFC) = CFS1S2(SRFC)

!         Do the P flow associated with the C flow from surface SOM1
!         to SOM23, and also do the P mineralization that goes with
!         this flow and with the CO2 respiration.
          CALL EFLOW_C (
     &      SOM1C(SRFC), SOM1E(SRFC,P), CES23(SRFC,P),  !Input
     &      CFS1S23(SRFC), CO2FS1(SRFC),                !Input
     &      EFS1S23(SRFC,P), IMMS1S23(SRFC,P),          !Output
     &      MNRS1S23(SRFC,P))                           !Output
        ENDIF   !End of IF block on IEL
      ENDIF   !End of IF block on L and SOM1C.

!       ----------------------------------------------------------------
!       Decomposition of soil SOM1 to SOM2 + SOM3, and to CO2.
!       *** C + N ***
!       ----------------------------------------------------------------
!        IF (L /= SRFC .AND. IEL == N) THEN
!         If there is soil SOM1.
      IF (L /= SRFC .AND. SOM1C(L) > 1.E-06) THEN

!       Calculate the total C flow out of soil SOM1.
        TCFLOW = SOM1C(L) * DEFAC(L) * DECS1(SOIL) * CULS1(L) * TXS1(L)
     &     !* DMOD
        IF (TCFLOW > SOM1C(L)) TCFLOW = SOM1C(L)

!       Calculate the CO2 respiration associated with the C flow
!       from soil SOM1 to SOM2 and SOM3.
        CO2FS1(L) = TCFLOW * CO2S1(L)

!       Calculate the C flow from soil SOM1 to SOM3 (the fraction
!       to SOM3 is texture dependent).
!       NB: In contrast to other C flows, TCFLOW is not first
!       reduced by the CO2 loss before dividing it over CFS1S2 and
!       CFS1S3, because the CO2 loss only refers to microbial
!       processes.
        CFS1S3(L) = TCFLOW * S1S3(L)

!       Calculate the C flow from soil SOM1 to SOM2: SOM2 gets
!       what is left over of TCFLOW.
        CFS1S2(L) = TCFLOW - CO2FS1(L) - CFS1S3(L)

        IF (N_ELEMS > 0) THEN
!         Do the N flow associated with the C flow from soil SOM1 to
!         SOM2, and also do the N mineralization or immobilization that
!         goes with this flow and with the CO2 respiration.
          CALL EFLOW_C (
     &      SOM1C(L), SOM1E(L,N), CES21(L,N),           !Input
     &      CFS1S2(L), CO2FS1(L),                       !Input
     &      EFS1S2(L,N), IMMS1S2(L,N), MNRS1S2(L,N))    !Output

!         Do the N flow associated with the C flow from soil SOM1 to
!         SOM3, and also do the N mineralization or immobilization that
!         goes with this flow.
!         NB: The N mineralization that goes with the CO2 flow has
!         already been dealt with when handling the flow from SOM1 to
!         SOM2, so put here a dummy with value -99.
          CALL EFLOW_C (
     &      SOM1C(L), SOM1E(L,N), CES3(L,N),            !Input
     &      CFS1S3(L), RDUMMY,                          !Input
     &      EFS1S3(L,N), IMMS1S3(L,N), MNRS1S3(L,N))    !Output
        ENDIF  !End of IF block on soil N_ELEMS.

        IF (N_ELEMS > 1) THEN
!         For EFLOW, the amount of C flowing to SOM23 is needed. This
!         is no different from the C that goes to SOM2 and SOM3.
          CFS1S23(L) = CFS1S2(L) + CFS1S3(L)

!         Do the P flow associated with the C flow from soil SOM1 to
!         SOM23, and also do the P mineralization that goes with this
!         flow and with the CO2 respiration. CO2FS1 has already been
!         calculated for C. 
          CALL EFLOW_C (
     &      SOM1C(L), SOM1E(L,P), CES23(L,P),           !Input
     &      CFS1S23(L), CO2FS1(L),                      !Input
     &      EFS1S23(L,P), IMMS1S23(L,P), MNRS1S23(L,P)) !Output
        ENDIF   !End of IF block on 
      ENDIF

!     --------------------------------------------------------------
!     Decomposition of SOM2 to SOM3 + soil SOM1 and to CO2.
!     *** C + N ***
!     --------------------------------------------------------------
!     If there is SOM2.
      IF (L /= SRFC) THEN
        IF (SOM2C(L) > 1.E-06) THEN

!         Calculate the total C flow out of SOM2.
          TCFLOW = SOM2C(L) * DEFAC(L) * DECS2(SOIL) * CULS2(L) !* DMOD
          IF (TCFLOW > SOM2C(L)) TCFLOW = SOM2C(L)
        
!         Calculate the CO2 respiration associated with the C flow
!         from SOM2 to soil SOM1 + SOM3.
          CO2FS2(L) = TCFLOW * CO2S2
        
!         Calculate the C flow from SOM2 to SOM3.
!         NB: In contrast to other C flows, TCFLOW is not first
!         reduced by the CO2 loss before dividing it over CFS2S3 and
!         CFS2S1, because the CO2 loss only refers to microbial
!         processes.
          CFS2S3(L) = TCFLOW * S2S3(L)
        
!         Calculate the C flow from SOM2 to soil SOM1.
          CFS2S1(L) = TCFLOW - CO2FS2(L) - CFS2S3(L)
        
          IF (N_ELEMS > 0) THEN
!           Do the N flow associated with the C flow from soil SOM2 to
!           soil SOM1 and also do the N mineralization or immobilization
!           that  goes with this flow and with the CO2 respiration.
            CALL EFLOW_C (
     &        SOM2C(L), SOM2E(L,N), CES1(L,N),            !Input
     &        CFS2S1(L), CO2FS2(L),                       !Input
     &        EFS2S1(L,N), IMMS2S1(L,N), MNRS2S1(L,N))    !Output
        
!           Ditto for the flow from SOM2 to SOM3.
!           NB: The N mineralization that goes with the CO2 flow has 
!           already been dealt with when handling the flow from SOM2 to
!           soil SOM1,  so put here a dummy with value -99.
            CALL EFLOW_C (
     &        SOM2C(L), SOM2E(L,N), CES3(L,N),            !Input
     &        CFS2S3(L), RDUMMY,                          !Input
     &        EFS2S3(L,N), IMMS2S3(L,N), MNRS2S3(L,N))    !Output
          ENDIF  !End of IF block on N_ELEMS.
        ENDIF
      ENDIF

!     --------------------------------------------------------------
!     Decomposition of SOM3 to soil SOM1, and to CO2.
!     *** C + N ***
!     --------------------------------------------------------------
!     If there is SOM3.
      IF (L /= SRFC) THEN
        IF (SOM3C(L) > 1.E-06) THEN

!         Calculate the total C flow out of SOM3C.
          TCFLOW = SOM3C(L) * DEFAC(L) * DECS3(SOIL) * CULS3(L) !* DMOD
          IF (TCFLOW > SOM3C(L)) TCFLOW = SOM3C(L)
        
!         Calculate the CO2 respiration associated with the C flow
!         from SOM3 to soil SOM1.
          CO2FS3(L) = TCFLOW * CO2S3
        
!         Correct the C flow from SOM3 to soil SOM1 for the CO2 lost
!         to respiration.
          CFS3S1(L) = TCFLOW - CO2FS3(L)
        
          IF (N_ELEMS > 0) THEN
!           Do the N flow associated with the C flow from soil SOM3 to
!           soil SOM1, and also do the N mineralization or immobilization
!           that goes with this flow and with the CO2 respiration.
            CALL EFLOW_C (
     &        SOM3C(L), SOM3E(L,N), CES1(L,N),            !Input
     &        CFS3S1(L), CO2FS3(L),                       !Input
     &        EFS3S1(L,N), IMMS3S1(L,N), MNRS3S1(L,N))    !Output
          ENDIF  !End of IF block on SOM3C.
        ENDIF

!     ################################################################
!     >>> Section on SOM-P <<<
!     The decomposition of the various SOM pools goes for P largely
!     the same as it is for N; it only becomes a different pool: for N
!     it goes to SOM2+SOM3, but for P there is one combined SOM23 pool.
!     ################################################################


!     --------------------------------------------------------------
!     Decomposition of SOM23 to SOM1.
!     *** only P ***
!     --------------------------------------------------------------

!       If there is SOM23-P.
        IF (N_ELEMS > 1 .AND. SOM23E(L,P) > 1.E-06) THEN
!         For EFLOW, the amount of C flowing to SOM1 is needed. This is
!         no different from what C comes from SOM2 and SOM3 for N. The
!         same for the CO2 flow.
          CFS23S1(L) = CFS2S1(L) + CFS3S1(L)
          CO2FS23(L) = CO2FS2(L) + CO2FS3(L)
      
!         Do the P flow associated with the C flow from SOM23 to soil
!         SOM1, and also do the P mineralization that goes with this
!         flow and with the CO2 respiration.
          CALL EFLOW_C (
     &      SOM23C(L), SOM23E(L,P), CES1(L,P),          !Input
     &      CFS23S1(L), CO2FS23(L),                     !Input
     &      EFS23S1(L,P), IMMS23S1(L,P), MNRS23S1(L,P)) !Output
        ENDIF   !End of IF block on SOM23-P.
      ENDIF

!***********************************************************************
      RETURN
      END Subroutine SOMDEC_C
