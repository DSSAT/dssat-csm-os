!***********************************************************************
! IMMOBLIMIT_C, subroutine for CENTURY-based SOM/residue module of DSSAT
!
!  Purpose: Limit the immobilization, when summing up all the E flows
!           would take away more mineral E than there is in a soil
!           layer.
! 
!  Revision history:
!  01/01/2000 AJG  Written.
!  06/23/2003 AJG  Included a 'fake' integration and a reduction factor
!                  to ensure that no state variables go negative in the
!                  real integration.
!  11/04/2003 AJG  Added P option.
!  11/14/2003 CHP  Added checks for zero divides
!  05/18/2004 AJG Completely reshuffled to get it ready for N and P with
!                 a SOM23 pool for P.
!
!  Called: CENTURY
!  Calls : --
!***********************************************************************

      SUBROUTINE IMMOBLIMIT_C (
     &  AMINRL, CFMETS1, CFS1S2, CFS1S3, CFS2S1,          !Input
     &  CFS2S3, CFS3S1, CFSTRS1, CFSTRS2, CO2FMET,        !Input
     &  CO2FS1, CO2FS2, CO2FS3, CO2FSTR, EFMETS1,         !Input
     &  EFS1S2, EFS1S23, EFS1S3, EFS23S1, EFS2S1,         !Input
     &  EFS2S3, EFS3S1, EFSTRS1, EFSTRS2, EFSTRS23,       !Input
     &  IMMMETS1, IMMOB, IMMS1S2, IMMS1S23, IMMS1S3,      !Input
     &  IMMS23S1, IMMS2S1, IMMS2S3, IMMS3S1,              !Input
     &  IMMSTRS1, IMMSTRS2, IMMSTRS23, MINER, MNRMETS1,   !Input
     &  MNRS1S2, MNRS1S23, MNRS1S3, MNRS23S1,             !Input
     &  MNRS2S1, MNRS2S3, MNRS3S1, MNRSTRS1,              !Input
     &  MNRSTRS2, MNRSTRS23, N_ELEMS, NLAYR,              !Input

     &  DLTLIGC, DLTMETABC, DLTMETABE,                    !Output
     &  DLTSOM1C, DLTSOM1E, DLTSOM23C, DLTSOM23E,         !Output
     &  DLTSOM2C, DLTSOM2E, DLTSOM3C, DLTSOM3E,           !Output
     &  DLTSTRUCC, DLTSTRUCE, IMM, MNR)                   !Output

!     ------------------------------------------------------------------
      USE ModuleDefs
      USE ModuleData
      IMPLICIT NONE
      SAVE
!     ------------------------------------------------------------------
      INTEGER IEL, L, LIG, N_ELEMS, NLAYR, NONLIG 

      PARAMETER (NONLIG = 1, LIG = 2) 
      INTEGER, PARAMETER :: SRFC = 0 

      REAL XMIN 

      REAL CFMETS1(0:NL), CFS1S2(0:NL), CFS1S3(NL),
     &  CFS2S1(NL), CFS2S3(NL), CFS3S1(NL),
     &  CFSTRS1(0:NL), CFSTRS2(0:NL), CO2FMET(0:NL),
     &  CO2FS1(0:NL), CO2FS2(NL), CO2FS3(NL), DLTLIGC(0:NL), 
     &  DLTMETABC(0:NL),  
     &  DLTSOM1C(0:NL), DLTSOM23C(NL), DLTSOM2C(NL), DLTSOM3C(NL),
     &  DLTSTRUCC(0:NL), REDUCFACTMIN(NL)

      REAL AMINRL(NL,3), CO2FSTR(0:NL,2), DLTMETABE(0:NL,3),
     &  DLTSOM1E(0:NL,3), DLTSOM23E(NL,3), DLTSOM2E(NL,3),
     &  DLTSOM3E(NL,3), DLTSTRUCE(0:NL,3), EFMETS1(0:NL,3),
     &  EFS1S2(0:NL,3), EFS1S23(0:NL,3), EFS1S3(NL,3), EFS23S1(NL,3),
     &  EFS2S1(NL,3), EFS2S3(NL,3), EFS3S1(NL,3), EFSTRS1(0:NL,3),
     &  EFSTRS2(0:NL,3), EFSTRS23(0:NL,3), IMM(0:NL,3),
     &  IMMMETS1(0:NL,3), IMMOB(0:NL,NELEM),
     &  IMMS1S2(0:NL,3), IMMS1S23(0:NL,3),
     &  IMMS1S3(NL,3), IMMS23S1(NL,3), IMMS2S1(NL,3),
     &  IMMS2S3(NL,3),IMMS3S1(NL,3), IMMSTRS1(0:NL,3), IMMSTRS2(0:NL,3),
     &  IMMSTRS23(0:NL,3), IMMSUMNET(0:NL,3), MINER(0:NL,NELEM),
     &  MNR(0:NL,3), MNRMETS1(0:NL,3), MNRS1S2(0:NL,3),
     &  MNRS1S23(0:NL,3), MNRS1S3(NL,3), MNRS23S1(NL,3), MNRS2S1(NL,3),
     &  MNRS2S3(NL,3), MNRS3S1(NL,3), MNRSTRS1(0:NL,3),
     &  MNRSTRS2(0:NL,3),MNRSTRS23(0:NL,3), REDUCFACT(NL,3)

       REAL TOMINSOM, TOMINSOM1, TOMINSOM2, TOMINSOM3, TNIMBSOM,TOMINFOM

!     CHP 8/28/2007
      INTEGER LIM_EL
      REAL Net_immob, Avail_Min

!***********************************************************************
      REDUCFACT    = 1.0
      REDUCFACTMIN = 1.0
      TOMINFOM  = 0.0
      TOMINSOM  = 0.0
      TOMINSOM1 = 0.0
      TOMINSOM2 = 0.0
      TOMINSOM3 = 0.0
      TNIMBSOM  = 0.0

      DO L = 0, NLAYR   !Including SRFC layer.
!       ---------------------------------------
!       Total N, P demand by various processes.
!       ---------------------------------------
        DO IEL = 1, N_ELEMS
!         Determine the sum of all net-immobilizing flows, because it
!         are only those pools for which the flows may have to be
!         limited. This sum is different from the overall net
!         immobilization of the layer. Take the flow from the
!         structural pool to SOM1 and SOM2 together, because these
!         flows are linked.
!         ======
!         SRFC N
!         ======
          IF (L == SRFC .AND. IEL == N) THEN
            IMMSUMNET(SRFC,N) = 
     &        MAX (IMMMETS1(SRFC,N) - MNRMETS1(SRFC,N),0.) 
     &      + MAX (IMMSTRS1(SRFC,N) - MNRSTRS1(SRFC,N)
     &           + IMMSTRS2(SRFC,N) - MNRSTRS2(SRFC,N),0.) 
     &      + MAX (IMMS1S2(SRFC,N)  - MNRS1S2(SRFC,N), 0.)

!         ======
!         SRFC P
!         ======
          ELSEIF (L == SRFC .AND. IEL == P) THEN
            IMMSUMNET(SRFC,P) = 
     &        MAX (IMMMETS1(SRFC,P) - MNRMETS1(SRFC,P), 0.) 
     &      + MAX (IMMSTRS1(SRFC,P) - MNRSTRS1(SRFC,P)
     &           + IMMSTRS23(SRFC,P)- MNRSTRS23(SRFC,P),0.) 
     &      + MAX (IMMS1S23(SRFC,P) - MNRS1S23(SRFC,P), 0.)

!         ======
!         SOIL N
!         ======
          ELSEIF (L /= SRFC .AND. IEL == N) THEN
!           Ditto for each of the soil layers.
            IMMSUMNET(L,N)  =  
     &        MAX (IMMMETS1(L,N) - MNRMETS1(L,N), 0.) 
     &      + MAX (IMMSTRS1(L,N) - MNRSTRS1(L,N) 
     &           + IMMSTRS2(L,N) - MNRSTRS2(L,N), 0.) 
     &      + MAX (IMMS1S2(L,N) - MNRS1S2(L,N) 
     &           + IMMS1S3(L,N) - MNRS1S3(L,N), 0.) 
     &      + MAX (IMMS2S1(L,N) - MNRS2S1(L,N) 
     &           + IMMS2S3(L,N) - MNRS2S3(L,N), 0.) 
     &      + MAX (IMMS3S1(L,N) - MNRS3S1(L,N), 0.)

!         ======
!         SOIL P
!         ======
          ELSEIF (L /= SRFC .AND. IEL == P) THEN
!           Ditto for each of the soil layers.
            IMMSUMNET(L,P)  =  
     &        MAX (IMMMETS1(L,P) - MNRMETS1(L,P), 0.) 
     &      + MAX (IMMSTRS1(L,P) - MNRSTRS1(L,P) 
     &           + IMMSTRS23(L,P)- MNRSTRS23(L,P), 0.) 
     &      + MAX (IMMS1S23(L,P) - MNRS1S23(L,P), 0.) 
     &      + MAX (IMMS23S1(L,P) - MNRS23S1(L,P), 0.)
          ENDIF   !End of IF block on SRFC layer vs. soil layers & IEL=N
        ENDDO   !End of IEL loop.
      ENDDO   !End of layer loop.

!     ------------------------------------------------------------------
!     Reduction factor for immobilization.
!     ------------------------------------------------------------------
!     If the total E immobilization is greater than
!     the amount of E available in the soil, reduce the SOM and litter
!     decomposition by a reduction factor, so that:
!     Amount of E needed for immobilization = amount of E available.
!     Or phrased differently:
!     REDUCFACT * (net immobilization) = (AMINRL - XMIN) 
!     A minimum amount of XMIN has to stay behind to prevent slightly
!     negative AMINRL due to inaccuracies with REAL variables.
!     All immobilizing flows in the layer are then reduced by the same
!     reduction factor, without making a distinction which flow may be
!     more important. The mineralization flows remain untouched: they
!     don't remove mineral E, but only add to the soil.

!     ------------------------------------------------------------------
!     Because the reduction factor only applies to immobilizing flows
!     and not to mineralizing flows, it has to be determined whether a
!     flow is immobilizing for either the N or P. It could be that N is
!     not immobilizing while P is (or vice versa), but because they
!     affect the same decomposition process, both the N and P flow then
!     have to be corrected by the reduction factor. Therefore, set reduction
!     factor based on the most limiting nutrient. Then apply the reduction 
!     factor to the C, N and P flows from that pool.

      DO L = 1, NLAYR  !SRFC layer not needed: uses AMINRL from layer 1.
        DO IEL = 1, N_ELEMS
!         The SRFC layer uses the mineral E of soil layer 1. Calculate
!         the reduction factor to limit the immobilization from layer 1;
!         this factor holds for both the SRFC layer and layer 1.
!         Due to inaccuracy in real variables, REDUCFACT may still take
!         more AMINRL than there is if AMINRL is very small (though the
!         difference is very, very small). This could result in negative
!         AMINRL. Keep therefore a minimum amount of AMINRL: XMIN.

          IF (IEL == N) THEN
            XMIN = 0.1
          ELSEIF (IEL == P) THEN
!           No need for XMIN with P, as Pi_AVAIL is already based on
!           a minimum Pi_LABILE_0.
            XMIN = 0.
          ENDIF   !End of IF block on IEL==N.

          IF (L == 1) THEN
!           For top soil layer, include surface immobilization 
            Net_immob = IMMSUMNET(SRFC,IEL) + IMMSUMNET(1,IEL)
          ELSE
            Net_immob = IMMSUMNET(L,IEL)
          ENDIF

          Avail_Min = MAX(0.0, AMINRL(1,IEL) - XMIN)

          IF (Net_immob > Avail_Min .AND. Net_immob > 1.E-7) THEN
            REDUCFACT(L,IEL) = Avail_Min / Net_immob
          ELSE
            REDUCFACT(L,IEL) = 1.0
          ENDIF

!         Limit between >= 0 and =<1.
          REDUCFACT(L,IEL) = AMAX1 (REDUCFACT(L,IEL), 0.) 
          REDUCFACT(L,IEL) = AMIN1 (REDUCFACT(L,IEL), 1.)
         ENDDO   !End of IEL loop.

!       Determine the minimum of the reduction factors for N and P. Note that
!       there is no REDUCFACT for the SRFC layer, as surface layer uses
!       nutrients from layer 1.
!       Specify which element is limiting transformation due to inadequate
!         mineral form avaliable.
        REDUCFACTMIN(L) = REDUCFACT(L,N)
        LIM_EL = N
        IF (N_ELEMS == 2 .AND. REDUCFACT(L,P) < REDUCFACT(L,N)) THEN
          REDUCFACTMIN(L) = REDUCFACT(L,P)
          LIM_EL = P
        ENDIF   !End of IF block on L/=SRFC and N_ELEMS==1.
      ENDDO   !End of DO loop on L.

!     ------------------------------------------------------------------
!     Limit the immobilization.
      DO L = 0, NLAYR   !Including SRFC layer.
        IF (L == SRFC .AND. REDUCFACTMIN(1) < 1.0) THEN
!         ----------
!         SRFC LAYER
!         --------------------------------------------------
!         Surface flow from metabolic litter to SOM1.
          Net_immob = IMMMETS1(SRFC,LIM_EL) - MNRMETS1(SRFC,LIM_EL)
          IF (Net_immob > 1.E-6)THEN
!           Carbon
            CFMETS1(SRFC)   = CFMETS1(SRFC)    * REDUCFACTMIN(1)
            CO2FMET(SRFC)   = CO2FMET(SRFC)    * REDUCFACTMIN(1)

!           Nitrogen
            EFMETS1(SRFC,N) = EFMETS1(SRFC,N)  * REDUCFACTMIN(1) 
            IMMMETS1(SRFC,N)= IMMMETS1(SRFC,N) * REDUCFACTMIN(1) 
            MNRMETS1(SRFC,N)= MNRMETS1(SRFC,N) * REDUCFACTMIN(1)

!           Phosphorus
            IF (N_ELEMS > 1) THEN
              EFMETS1(SRFC,P)  = EFMETS1(SRFC,P)  * REDUCFACTMIN(1) 
              IMMMETS1(SRFC,P) = IMMMETS1(SRFC,P) * REDUCFACTMIN(1) 
              MNRMETS1(SRFC,P) = MNRMETS1(SRFC,P) * REDUCFACTMIN(1)
            ENDIF
          ENDIF !End of Net immobilization > 0 block

!         --------------------------------------------------
!         Surface flow from structural litter to SOM1 and SOM2.
!         The lignin and non-lignin fraction of the structural litter
!         decompose together. So if one has to be reduced, the other
!         one has to go also.  
!         First calculate net immobilization from structural decomposition
!             for the limiting element
          IF (LIM_EL == N) THEN
            Net_immob = IMMSTRS1(SRFC,N) - MNRSTRS1(SRFC,N)
     &                + IMMSTRS2(SRFC,N) - MNRSTRS2(SRFC,N)
          ELSEIF (LIM_EL == P) THEN
            Net_immob = IMMSTRS1(SRFC,P) - MNRSTRS1(SRFC,P)
     &                + IMMSTRS23(SRFC,P)- MNRSTRS23(SRFC,P)
          ENDIF

          IF (Net_immob > 1.E-6) THEN
!           Carbon
            CFSTRS1(SRFC)        = CFSTRS1(SRFC) * REDUCFACTMIN(1)
            CFSTRS2(SRFC)        = CFSTRS2(SRFC) * REDUCFACTMIN(1)
            CO2FSTR(SRFC,NONLIG) = CO2FSTR(SRFC,NONLIG) *REDUCFACTMIN(1)
            CO2FSTR(SRFC,LIG)    = CO2FSTR(SRFC,LIG) * REDUCFACTMIN(1)

!           Nitrogen
            EFSTRS1(SRFC,N)  = EFSTRS1(SRFC,N)  * REDUCFACTMIN(1)
            EFSTRS2(SRFC,N)  = EFSTRS2(SRFC,N)  * REDUCFACTMIN(1) 
            IMMSTRS1(SRFC,N) = IMMSTRS1(SRFC,N) * REDUCFACTMIN(1)
            IMMSTRS2(SRFC,N) = IMMSTRS2(SRFC,N) * REDUCFACTMIN(1) 
            MNRSTRS1(SRFC,N) = MNRSTRS1(SRFC,N) * REDUCFACTMIN(1)
            MNRSTRS2(SRFC,N) = MNRSTRS2(SRFC,N) * REDUCFACTMIN(1)

!           Phosphorus
            IF (N_ELEMS > 1) THEN
              EFSTRS1(SRFC,P)   = EFSTRS1(SRFC,P)  * REDUCFACTMIN(1)
              IMMSTRS1(SRFC,P)  = IMMSTRS1(SRFC,P) * REDUCFACTMIN(1)
              MNRSTRS1(SRFC,P)  = MNRSTRS1(SRFC,P) * REDUCFACTMIN(1)
              EFSTRS23(SRFC,P)  = EFSTRS2(SRFC,P)  * REDUCFACTMIN(1) 
              IMMSTRS23(SRFC,P) = IMMSTRS2(SRFC,P) * REDUCFACTMIN(1) 
              MNRSTRS23(SRFC,P) = MNRSTRS2(SRFC,P) * REDUCFACTMIN(1)
            ENDIF
          ENDIF !End of Net immobilization > 0 block

!         --------------------------------------------------
!         Surface flow from SOM1 to SOM2 and SOM3
!         First calculate net immobilization from structural decomposition
!             for the limiting element
          IF (LIM_EL == N) THEN
            Net_immob = IMMS1S2(SRFC,N)  - MNRS1S2(SRFC,N)
          ELSEIF (LIM_EL == P) THEN
            Net_immob = IMMS1S23(SRFC,P) - MNRS1S23(SRFC,P)
          ENDIF

          IF (Net_immob > 1.E-6) THEN
!           Carbon
            CFS1S2(SRFC) = CFS1S2(SRFC) * REDUCFACTMIN(1)
            CO2FS1(SRFC) = CO2FS1(SRFC) * REDUCFACTMIN(L)

!           Nitrogen.
            EFS1S2(SRFC,N)  = EFS1S2(SRFC,N)  * REDUCFACTMIN(1) 
            IMMS1S2(SRFC,N) = IMMS1S2(SRFC,N) * REDUCFACTMIN(1)
            MNRS1S2(SRFC,N) = MNRS1S2(SRFC,N) * REDUCFACTMIN(1)

!           Phosphorus.
            IF (N_ELEMS > 1) THEN
              EFS1S23(SRFC,P)  = EFS1S23(SRFC,P)  * REDUCFACTMIN(1) 
              IMMS1S23(SRFC,P) = IMMS1S23(SRFC,P) * REDUCFACTMIN(1)
              MNRS1S23(SRFC,P) = MNRS1S23(SRFC,P) * REDUCFACTMIN(1)
            ENDIF    !End of IF block on P
          ENDIF !End of Net immobilization > 0 block

!========================================================================
        ELSEIF (L /= SRFC) THEN
          IF (REDUCFACTMIN(L) < 1.0) THEN
!           -----------
!           SOIL LAYERS
!           --------------------------------------------------
!           Soil flow from metabolic litter to SOM1.
            Net_immob = IMMMETS1(L,LIM_EL) - MNRMETS1(L,LIM_EL)
            IF (Net_immob > 1.E-6) THEN
!             Carbon.
              CFMETS1(L) = CFMETS1(L) * REDUCFACTMIN(L)
              CO2FMET(L) = CO2FMET(L) * REDUCFACTMIN(L)
          
!             Nitrogen
              EFMETS1(L,N)  = EFMETS1(L,N)  * REDUCFACTMIN(L) 
              IMMMETS1(L,N) = IMMMETS1(L,N) * REDUCFACTMIN(L)
              MNRMETS1(L,N) = MNRMETS1(L,N) * REDUCFACTMIN(L)
          
!             Phosphorus
              IF (N_ELEMS > 1) THEN
                EFMETS1(L,P)   = EFMETS1(L,P)  * REDUCFACTMIN(L) 
                IMMMETS1(L,P)  = IMMMETS1(L,P) * REDUCFACTMIN(L)
                MNRMETS1(L,P)  = MNRMETS1(L,P) * REDUCFACTMIN(L)
              ENDIF    !End of IF block on P
            ENDIF !End of Net immobilization > 0 block
          
!           --------------------------------------------------
!           Soil flow from structural litter to SOM1 and SOM2.
            IF (LIM_EL == N) THEN
              Net_immob = IMMSTRS1(L,N) - MNRSTRS1(L,N)
     &                  + IMMSTRS2(L,N) - MNRSTRS2(L,N)
            ELSEIF (LIM_EL == P) THEN
              Net_immob = IMMSTRS1(L,P) - MNRSTRS1(L,P)
     &                  + IMMSTRS23(L,P)- MNRSTRS23(L,P)
            ENDIF
          
            IF (Net_immob > 1.E-6) THEN
!             Carbon.
              CFSTRS1(L) = CFSTRS1(L) * REDUCFACTMIN(L)
              CFSTRS2(L) = CFSTRS2(L) * REDUCFACTMIN(L)
              CO2FSTR(L,NONLIG) = CO2FSTR(L,NONLIG) * REDUCFACTMIN(L)
              CO2FSTR(L,LIG)    = CO2FSTR(L,LIG)    * REDUCFACTMIN(L)
          
!             Nitrogen
              EFSTRS1(L,N)  = EFSTRS1(L,N)  * REDUCFACTMIN(L)
              IMMSTRS1(L,N) = IMMSTRS1(L,N) * REDUCFACTMIN(L)
              MNRSTRS1(L,N) = MNRSTRS1(L,N) * REDUCFACTMIN(L)
              EFSTRS2(L,N)  = EFSTRS2(L,N)  * REDUCFACTMIN(L) 
              IMMSTRS2(L,N) = IMMSTRS2(L,N) * REDUCFACTMIN(L)
              MNRSTRS2(L,N) = MNRSTRS2(L,N) * REDUCFACTMIN(L)
          
!             Phosphorus
              IF (N_ELEMS > 1) THEN
                EFSTRS1(L,P)   = EFSTRS1(L,P)  * REDUCFACTMIN(L)
                IMMSTRS1(L,P)  = IMMSTRS1(L,P) * REDUCFACTMIN(L)
                MNRSTRS1(L,P)  = MNRSTRS1(L,P) * REDUCFACTMIN(L)
                EFSTRS23(L,P)  = EFSTRS23(L,P) * REDUCFACTMIN(L) 
                IMMSTRS23(L,P) = IMMSTRS2(L,P) * REDUCFACTMIN(L)
                MNRSTRS23(L,P) = MNRSTRS2(L,P) * REDUCFACTMIN(L)
              ENDIF    !End of IF block on P
            ENDIF !End of Net immobilization > 0 block
          
!           --------------------------------------------------
!           Soil flow from SOM1 to SOM2 and SOM3
!           First calculate net immobilization from structural decomposition
!               for the limiting element
            IF (LIM_EL == N) THEN
              Net_immob = IMMS1S2(L,N) - MNRS1S2(L,N)
     &                  + IMMS1S3(L,N) - MNRS1S3(L,N) 
            ELSEIF (LIM_EL == P) THEN
              Net_immob = IMMS1S23(L,P) - MNRS1S23(L,P)
            ENDIF
          
            IF (Net_immob > 1.E-6) THEN
!             Carbon.
              CFS1S2(L) = CFS1S2(L) * REDUCFACTMIN(L)
              CFS1S3(L) = CFS1S3(L) * REDUCFACTMIN(L)
              CO2FS1(L) = CO2FS1(L) * REDUCFACTMIN(L)
          
!             Nitrogen
              EFS1S2(L,N)  = EFS1S2(L,N)  * REDUCFACTMIN(L) 
              IMMS1S2(L,N) = IMMS1S2(L,N) * REDUCFACTMIN(L)
              MNRS1S2(L,N) = MNRS1S2(L,N) * REDUCFACTMIN(L)
              EFS1S3(L,N)  = EFS1S3(L,N)  * REDUCFACTMIN(L) 
              IMMS1S3(L,N) = IMMS1S3(L,N) * REDUCFACTMIN(L) 
              MNRS1S3(L,N) = MNRS1S3(L,N) * REDUCFACTMIN(L)
          
!             Phosphorus
              IF (N_ELEMS > 1) THEN
                EFS1S23(L,P)  = EFS1S23(L,P)  * REDUCFACTMIN(L) 
                IMMS1S23(L,P) = IMMS1S23(L,P) * REDUCFACTMIN(L)
                MNRS1S23(L,P) = MNRS1S23(L,P) * REDUCFACTMIN(L)
              ENDIF    !End of IF block on P
            ENDIF !End of Net immobilization > 0 block
          
!           --------------------------------------------------
!           Soil flow from SOM2 to SOM1 and SOM3
!           First calculate net immobilization from structural decomposition
!               for the limiting element
            IF (LIM_EL == N) THEN
              Net_immob = IMMS2S1(L,N) - MNRS2S1(L,N)
     &                  + IMMS2S3(L,N) - MNRS2S3(L,N) 
            ELSEIF (LIM_EL == P) THEN
              Net_immob = IMMS23S1(L,P) - MNRS23S1(L,P)
            ENDIF
          
            IF (Net_immob > 1.E-6) THEN
!             Carbon.
              CFS2S1(L) = CFS2S1(L) * REDUCFACTMIN(L)
              CFS2S3(L) = CFS2S3(L) * REDUCFACTMIN(L)
              CO2FS2(L) = CO2FS2(L) * REDUCFACTMIN(L)
          
!             Nitrogen
              EFS2S1(L,N)  = EFS2S1(L,N)  * REDUCFACTMIN(L) 
              IMMS2S1(L,N) = IMMS2S1(L,N) * REDUCFACTMIN(L) 
              MNRS2S1(L,N) = MNRS2S1(L,N) * REDUCFACTMIN(L)
              EFS2S3(L,N)  = EFS2S3(L,N)  * REDUCFACTMIN(L) 
              IMMS2S3(L,N) = IMMS2S3(L,N) * REDUCFACTMIN(L) 
              MNRS2S3(L,N) = MNRS2S3(L,N) * REDUCFACTMIN(L)
          
!             Phosphorus
              IF (N_ELEMS > 1) THEN
                EFS23S1(L,P)  = EFS23S1(L,P)  * REDUCFACTMIN(L) 
                IMMS23S1(L,P) = IMMS23S1(L,P) * REDUCFACTMIN(L) 
                MNRS23S1(L,P) = MNRS23S1(L,P) * REDUCFACTMIN(L)
              ENDIF    !End of IF block on P
            ENDIF !End of Net immobilization > 0 block
          
!           --------------------------------------------------
!           Soil flow from SOM3 to SOM1
            IF (LIM_EL == N) THEN
              Net_immob = IMMS3S1(L,N) - MNRS3S1(L,N)
            ELSEIF (LIM_EL == P) THEN
              Net_immob = 0.0
            ENDIF
          
            IF (Net_immob > 1.E-6) THEN
!             Carbon.
              CFS3S1(L) = CFS3S1(L) * REDUCFACTMIN(L)
              CO2FS3(L) = CO2FS3(L) * REDUCFACTMIN(L)
          
!             Nitrogen
              EFS3S1(L,N)  = EFS3S1(L,N)  * REDUCFACTMIN(L) 
              IMMS3S1(L,N) = IMMS3S1(L,N) * REDUCFACTMIN(L)
              MNRS3S1(L,N) = MNRS3S1(L,N) * REDUCFACTMIN(L)
            ENDIF !End of Net immobilization > 0 block
          ENDIF
        ENDIF   !End of IF block on SRFC layer vs. other layers.

!       --------------------------------------------------------------
!       Combine all the fluxes in DLTxxx rate variables.
!       --------------------------------------------------------------
!       --------------------------------
!       Metabolic and structural litter.
!       --------------------------------
!       ** Carbon.
        DLTMETABC(L) = DLTMETABC(L) - CFMETS1(L) - CO2FMET(L)
        DLTSTRUCC(L) = DLTSTRUCC(L) - CFSTRS1(L) - CFSTRS2(L) -
     &    CO2FSTR(L,NONLIG) - CO2FSTR(L,LIG)

!       The material that flows from structural to SOM2 is lignin.
        DLTLIGC(L) = DLTLIGC(L) - CFSTRS2(L) - CO2FSTR(L,LIG)

!       ** Nitrogen.
!       Include the E flow from pool A to B, plus the E that is
!       mineralized. E from immobilization adds to the receiving pool;
!       the providing pool has nothing to do with the immobilization.
        DLTMETABE(L,N) = DLTMETABE(L,N) - EFMETS1(L,N)
!         Subtract mineralization.
     &    - MNRMETS1(L,N)

        DLTSTRUCE(L,N) = DLTSTRUCE(L,N) - EFSTRS1(L,N) -
     &    EFSTRS2(L,N)
!         Subtract mineralization.
     &    - MNRSTRS1(L,N) - MNRSTRS2(L,N)

!       ** Phosphorus.
        IF (N_ELEMS > 1) THEN
          DLTMETABE(L,P) = DLTMETABE(L,P) - EFMETS1(L,P)
!           Subtract mineralization.
     &      - MNRMETS1(L,P)

          DLTSTRUCE(L,P) = DLTSTRUCE(L,P) - EFSTRS1(L,P) -
     &      EFSTRS23(L,P)
!           Subtract mineralization.
     &      - MNRSTRS1(L,P) - MNRSTRS23(L,P)
        ENDIF

!       ----
!       SOM1
!       ----
        IF (L == SRFC) THEN
!         ** Carbon.
          DLTSOM1C(SRFC) = DLTSOM1C(SRFC) + CFMETS1(SRFC) +
     &      CFSTRS1(SRFC) - CFS1S2(SRFC) - CO2FS1(SRFC)

!         ** Nitrogen.
          DLTSOM1E(SRFC,N) = DLTSOM1E(SRFC,N) + EFMETS1(SRFC,N)
     &      + EFSTRS1(SRFC,N) - EFS1S2(SRFC,N)
!           Subtract mineralization.
     &      - MNRS1S2(SRFC,N)
!           Add immobilization.
     &      + IMMMETS1(SRFC,N) + IMMSTRS1(SRFC,N)

!         ** Phosphorus.
          IF (N_ELEMS > 1) THEN
            DLTSOM1E(SRFC,P) = DLTSOM1E(SRFC,P) + EFMETS1(SRFC,P)
     &        + EFSTRS1(SRFC,P) - EFS1S23(SRFC,P)
!             Subtract mineralization.
     &        - MNRS1S23(SRFC,P)
!             Add immobilization.
     &        + IMMMETS1(SRFC,P) + IMMSTRS1(SRFC,P)
          ENDIF

        ELSE   !Soil layers.
!         ----
!         SOM1
!         ----
!         ** Carbon.
          DLTSOM1C(L) = DLTSOM1C(L) + CFMETS1(L) + CFSTRS1(L) -
     &      CFS1S2(L) - CFS1S3(L) - CO2FS1(L) + CFS2S1(L) + CFS3S1(L)

!         ** Nitrogen.
          DLTSOM1E(L,N) = DLTSOM1E(L,N) + EFMETS1(L,N) +
     &      EFSTRS1(L,N) - EFS1S2(L,N) - EFS1S3(L,N) +
     &      EFS2S1(L,N) + EFS3S1(L,N)
!           Subtract mineralization.
     &      - MNRS1S2(L,N) - MNRS1S3(L,N)
!           Add immobilization.
     &      + IMMMETS1(L,N) + IMMSTRS1(L,N) + IMMS2S1(L,N) 
     &      + IMMS3S1(L,N)

!         ** Phosphorus.
          IF (N_ELEMS > 1) THEN
            DLTSOM1E(L,P) = DLTSOM1E(L,P) + EFMETS1(L,P) +
     &        EFSTRS1(L,P) - EFS1S23(L,P) + EFS23S1(L,P)
!             Subtract mineralization.
     &        - MNRS1S23(L,P) 
!             Add immobilization.
     &        + IMMMETS1(L,P) + IMMSTRS1(L,P) + IMMS23S1(L,P) 
          ENDIF

!         ----
!         SOM2
!         ----
!         SOM2 of layer 1 gets input from the SRFC layer also.
!         ** Carbon.
          IF (L == 1) THEN
            DLTSOM2C(L) = DLTSOM2C(L) + CFSTRS2(SRFC) + CFSTRS2(L) +
     &        CFS1S2(SRFC) + CFS1S2(L) - CFS2S1(L) - CFS2S3(L) -
     &         CO2FS2(L)
          ELSE
            DLTSOM2C(L) = DLTSOM2C(L) + CFSTRS2(L) + CFS1S2(L) -
     &         CFS2S1(L) - CFS2S3(L) - CO2FS2(L)
          ENDIF   !End of IF block on L==1

!         ** Nitrogen.
          IF (L == 1) THEN
            DLTSOM2E(1,N) = DLTSOM2E(1,N) + EFSTRS2(SRFC,N) +
     &        EFSTRS2(1,N) + EFS1S2(SRFC,N) + EFS1S2(1,N) -
     &        EFS2S1(1,N) - EFS2S3(1,N)
!             Subtract mineralization.
     &        - MNRS2S1(1,N) - MNRS2S3(1,N)
!             Add immobilization.
     &        + IMMSTRS2(SRFC,N) + IMMSTRS2(1,N)
     &        + IMMS1S2(SRFC,N) + IMMS1S2(1,N)
          ELSE
            DLTSOM2E(L,N) = DLTSOM2E(L,N) + EFSTRS2(L,N) +
     &        EFS1S2(L,N) - EFS2S1(L,N) - EFS2S3(L,N)
!             Subtract mineralization.
     &        - MNRS2S1(L,N) - MNRS2S3(L,N)
!             Add immobilization.
     &        + IMMSTRS2(L,N) + IMMS1S2(L,N)
          ENDIF   !End of IF block on L==1

!         ----
!         SOM3
!         ----
!         ** Carbon.
          DLTSOM3C(L) = DLTSOM3C(L) + CFS1S3(L) + CFS2S3(L) -
     &      CFS3S1(L) - CO2FS3(L)

!         ** Nitrogen.
          DLTSOM3E(L,N) = DLTSOM3E(L,N) + EFS1S3(L,N) +
     &      EFS2S3(L,N) - EFS3S1(L,N)
!           Subtract mineralization.
     &      - MNRS3S1(L,N)
!           Add immobilization.
     &      + IMMS1S3(L,N) + IMMS2S3(L,N)

!         -----
!         SOM23
!         -----
!         ** Carbon.
          DLTSOM23C(L) = DLTSOM2C(L) + DLTSOM3C(L)

!         ** Phosphorus.
          IF (N_ELEMS > 1) THEN
            IF (L == 1) THEN
              DLTSOM23E(1,P) = DLTSOM23E(1,P)  
     &                       + EFSTRS23(SRFC,P) + EFSTRS23(1,P) 
     &                       + EFS1S23(SRFC,P)  + EFS1S23(1,P) 
     &                       - EFS23S1(1,P)
!               Subtract mineralization.
     &          - MNRS23S1(1,P)
!               Add immobilization.
     &          + IMMSTRS23(SRFC,P)+ IMMSTRS23(1,P)  
     &          + IMMS1S23(SRFC,P)+ IMMS1S23(1,P) 
            ELSE
              DLTSOM23E(L,P) = DLTSOM23E(L,P) + EFSTRS23(L,P) +
     &          EFS1S23(L,P) - EFS23S1(L,P)
!               Subtract mineralization.
     &          - MNRS23S1(L,P)
!               Add immobilization.
     &          + IMMSTRS23(L,P) + IMMS1S23(L,P)
            ENDIF   !End of IF block on L==1.
          ENDIF   !End of IF block on N_ELEMS>1..
        ENDIF   !End of IF block on L=SRFC vs. other soil layers.

!       -------------------------------------------------------
!       Total E mineralization or immobilization in a layer.
!       -------------------------------------------------------
!       The MINERALIZE variable used here sums up the mineralization
!       from where it originated, not where it ended up. Thus for the
!       SRFC layer, it goes to MINERALIZE(SRFC,IEL) and not to the
!       layer-1 variable. In the NUPDATE subroutine (called from
!       NTRANS), it is handled where the E goes to.
        IF (L == SRFC) THEN
          MNR(SRFC,N) = MINER(SRFC,N) + MNRMETS1(SRFC,N) +
     &      MNRSTRS1(SRFC,N) + MNRSTRS2(SRFC,N) + MNRS1S2(SRFC,N)

          TOMINFOM = TOMINFOM + MNRMETS1(SRFC,N) + MNRSTRS1(SRFC,N)
     &                         + MNRSTRS2(SRFC,N)
          TOMINSOM1 = TOMINSOM1 + MNRS1S2(SRFC,N)    

          IMM(SRFC,N) = IMMOB(SRFC,N) + IMMMETS1(SRFC,N) + 
     &      IMMSTRS1(SRFC,N) + IMMSTRS2(SRFC,N) + IMMS1S2(SRFC,N)

          TNIMBSOM = TNIMBSOM + IMMMETS1(SRFC,N) +
     &       IMMSTRS1(SRFC,N) + IMMSTRS2(SRFC,N) + IMMS1S2(SRFC,N)

          IF (N_ELEMS > 1) THEN
            MNR(SRFC,P) = MINER(SRFC,P) + MNRMETS1(SRFC,P) +
     &        MNRSTRS1(SRFC,P) + MNRSTRS23(SRFC,P) + MNRS1S23(SRFC,P)

            IMM(SRFC,P) = IMMOB(SRFC,P) + IMMMETS1(SRFC,P) +
     &        IMMSTRS1(SRFC,P) + IMMSTRS23(SRFC,P) + IMMS1S23(SRFC,P)
          ENDIF

        ELSE
            MNR(L,N) = MINER(L,N) + MNRMETS1(L,N) + MNRSTRS1(L,N) +
     &                 MNRSTRS2(L,N) + MNRS1S2(L,N) + MNRS1S3(L,N) +
     &                 MNRS2S1(L,N) + MNRS2S3(L,N) + MNRS3S1(L,N)

            TOMINFOM = TOMINFOM + MNRMETS1(L,N) + MNRSTRS1(L,N)
     &                          + MNRSTRS2(L,N)
            TOMINSOM1 = TOMINSOM1 + MNRS1S2(L,N) + MNRS1S3(L,N)    
            TOMINSOM2 = TOMINSOM2 + MNRS2S1(L,N) + MNRS2S3(L,N)
            TOMINSOM3 = TOMINSOM3 + MNRS3S1(L,N)

            IMM(L,N) = IMMOB(L,N) + IMMMETS1(L,N) + IMMSTRS1(L,N) +
     &        IMMSTRS2(L,N) + IMMS1S2(L,N) + IMMS1S3(L,N) +
     &        IMMS2S1(L,N) + IMMS2S3(L,N) + IMMS3S1(L,N)

            TNIMBSOM = TNIMBSOM + IMMMETS1(L,N) +
     &       IMMSTRS1(L,N) + IMMSTRS2(L,N) + IMMS1S2(L,N) +
     &       IMMS2S1(L,N) + IMMS1S3(L,N) + IMMS2S3(L,N)

          IF (N_ELEMS > 1) THEN
            MNR(L,P) = MINER(L,P) + MNRMETS1(L,P) + MNRSTRS1(L,P) +
     &        MNRSTRS23(L,P) + MNRS1S23(L,P) + MNRS23S1(L,P)
            IMM(L,P) = IMMOB(L,P) + IMMMETS1(L,P) + IMMSTRS1(L,P) +
     &        IMMSTRS23(L,P) + IMMS1S23(L,P) + IMMS23S1(L,P)
          ENDIF   !End of IF block on N_ELEMS>1.
        ENDIF   !End of IF block on L==SRFC.

      ENDDO   !End of layer loop

      TOMINSOM = TOMINSOM + TOMINSOM1 + TOMINSOM2 + TOMINSOM3

!     Transfer daily mineralization values for use by Cassava model
      CALL PUT('ORGC','TOMINFOM' ,TOMINFOM) !Miner from FOM (kg/ha)
      CALL PUT('ORGC','TOMINSOM' ,TOMINSOM) !Miner from SOM (kg/ha)
      CALL PUT('ORGC','TOMINSOM1',TOMINSOM1)!Miner from SOM1(kg/ha)
      CALL PUT('ORGC','TOMINSOM2',TOMINSOM2)!Miner from SOM2(kg/ha)
      CALL PUT('ORGC','TOMINSOM3',TOMINSOM3)!Miner from SOM3(kg/ha)
      CALL PUT('ORGC','TNIMBSOM', TNIMBSOM) !Immob (kg/ha)

!***********************************************************************
!***********************************************************************
!     END
!***********************************************************************
      RETURN
      END Subroutine IMMOBLIMIT_C

!***********************************************************************
! IMMOBLIMIT_C variables:
!
! CFMETS1     C flow from the metabolic pool to SOM1 (-)
! CFS1S2      C flow from SOM1 to SOM2  (kg[C] / ha)
! CFS1S2OLD   C flow from SOM1 to SOM2 before limiting it because of
!               shortage of E available for immobilization. (kg[C] / ha)
! CFS1S3      C flow from SOM1 to SOM3 (kg[C] / ha)
! CFS1S3OLD   C flow from SOM1 to SOM3 before limiting it because of
!               shortage of E available for immobilization. (kg[C] / ha)
! CFS2S1      C flow from SOM2 to SOM1 (kg[C] / ha)
! CFS2S1OLD   C flow from SOM2 to SOM1 before limiting it because of
!               shortage of E available for immobilization. (kg[C] / ha)
! CFS2S3      C flow from SOM2 to SOM3 (kg[C] / ha)
! CFS2S3OLD   C flow from SOM2 to SOM3 before limiting it because of
!               shortage of E available for immobilization. (kg[C] / ha)
! CFSTRS1     C flow from the structural pool to SOM1 (kg[C] / ha)
! CFSTRS2     C flow from the structural pool to SOM2 (kg[C] / ha)
! CO2FMET     CO2 flow that accompanies the C flow out of the metabolic pool
!               (kg[C] / ha)
! CO2FS1      CO2 flow that accompanies the C flow out of SOM1l (kg[C] / ha)
! CO2FS2      CO2 flow that accompanies the C flow out of SOM12 (kg[C] / ha)
! CO2FS3      CO2 flow that accompanies the C flow out of SOM3l (kg[C] / ha)
! CO2FSTR     CO2 flow that accompanies the C flow out of the structural pool
!               (kg[C] / ha)
! DLTLIGC     Rate variable for the change in lignin C (kg[C] / ha)
! DLTMETABC   Rate variable for the change in metabolic C (kg[C] / ha)
! DLTMETABE   Rate variable for the change in metabolic E (kg[E] / ha)
! DLTSNH4     Rate variable for the change in SNH4 (kg[N] / ha)
! DLTSNO3     Rate variable for the change in SNO3 (kg[N] / ha)
! DLTSOM1C    Rate variable for the change in SOM1 C (kg[C] / ha)
! DLTSOM1E    Rate variable for the change in SOM1 E (kg[E] / ha)
! DLTSOM2C    Rate variable for the change in SOM2 C (kg[C] / ha)
! DLTSOM2E    Rate variable for the change in SOM2 E (kg[E] / ha)
! DLTSOM3C    Rate variable for the change in SOM3 C (kg[C] / ha)
! DLTSOM3E    Rate variable for the change in SOM3 E (kg[E] / ha)
! DLTSTRUCC   Rate variable for the change in structural C (kg[C] / ha)
! DLTSTRUCE   Rate variable for the change in structural E (kg[E] / ha)
! DLTUREA     Rate variable for the change in urea N  (kg[N] / ha)
! EFMETS1     E flow from soil or soil or surface metabolic residue to soil
!               or surface SOM1 (kg[E] / ha)
! EFS1S2      E flow from soil or surface SOM1 to SOM2 (kg[E] / ha)
! EFS1S3      E flow from soil SOM1 to SOM3 (kg[E] / ha)
! EFS2S1      E flow from SOM2 to SOM1 (kg[E] / ha)
! EFS2S3      E flow from SOM2 to SOM3 (kg[E] / ha)
! EFS3S1      E flow from SOM3 to SOM1 (kg[E] / ha)
! EFSTRS1     E flow from soil or surface structural residue to soil or
!               surface SOM1 (kg[E] / ha)
! EFSTRS2     E flow from soil or soil or surface structural residue to SOM2
!               (kg[E] / ha)
! IMMMETS1    Immobilization of E during the flow from soil or surface metabolic
!               residue to soil or surface SOM1  (kg[E] / ha)
! IMMOB       Immobilization of E  (kg[E] / ha)
! IMMS1S2     Immobilization of E during the flow from soil or surface SOM1 to
!               SOM2 (kg[E] / ha)
! IMMS1S3     Immobilization of E during the flow from SOM1 to SOM3 (kg[E] / ha)
! IMMS2S1     Immobilization of E during the flow from SOM2 to SOM1 (kg[E] / ha)
! IMMS2S3     Immobilization of E during the flow from SOM2 to SOM3 (kg[E] / ha)
! IMMS3S1     Immobilization of E during the flow from SOM3 to SOM1 (kg[E] / ha)
! IMMSTRS1    Immobilization of E during the flow from soil or surface structural
!               residue to soil or surface SOM1  (kg[E] / ha)
! IMMSTRS2    Immobilization of E during the flow from soil or surface structural
!               residue to SOM2  (kg[E] / ha)
! IMMSUMNET   Net immobilization of E by all flows in a layer (kg[E] / ha)
! MINERALIZE  Mineralization of E  (kg[E] / ha)
! MNRMETS1    Mineralization of E during the flow from soil or surface metabolic
!               residue to soil or surface SOM1  (kg[E] / ha)
! MNRS1S2     Mineralization of E during the flow from SOM1 to SOM2 (kg[E] / ha)
! MNRS1S3     Mineralization of E during the flow from SOM1 to SOM3 (kg[E] / ha)
! MNRS2S1     Mineralization of E during the flow from SOM2 to SOM1 (kg[E] / ha)
! MNRS2S3     Mineralization of E during the flow from SOM2 to SOM3 (kg[E] / ha)
! MNRS3S1     Mineralization of E during the flow from SOM3 to SOM1 (kg[E] / ha)
! MNRSTRS1    Mineralization of E during the flow from soil or surface structural
!               to soil or surface SOM1  (kg[E] / ha)
! MNRSTRS2    Mineralization of E during the flow from soil or surface structural
!               residue to SOM2  (kg[E] / ha)
! N_ELEMS       Number of elements: 1 = N, 2 = N+P, 3 = N+P+S (-)
! NLAYR       Number of soil layers (-)
!***********************************************************************

