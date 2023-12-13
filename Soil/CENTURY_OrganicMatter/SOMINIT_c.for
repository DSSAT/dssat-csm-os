!***********************************************************************
!  SOMINIT_C, Subroutine for CENTURY-based SOM/residue module of DSSAT.
!
!  Purpose: This subroutine initializes variables and does
!           calculations that only have to be done once at the start
!           of a cycle. Only for CENTURY-based SOM model.
!
!  Revision history:
!  ........       Parton et al.  Written for CENTURY model.
!  01/01/1999 AJG Revised and linked to DSSAT.
!  02/25/2003 CHP Revised initialization when TOTN is limiting.
!  03/26/2003 GH  Modified file name
!  08/12/2003 CHP Added I/O error checking
!                 Write to Warning.out file in addition to screen.
!  09/01/2003 CHP Further revised SOM initialization when TOTN value
!                   is limiting.
!  10/21/2003 AJG Linked to the P model of Daroub et al.
!  11/02/2003 AJG Corrected some errors and added explanatory text.
!  12/17/2003 AJG Renamed all the CH_ and CHEM_ variables to P_ variables.
!  01/15/2004 AJG Made new SOM P pool structure with SOM23
!  05/13/2004 AJG Changed the SOM1C:SOM2C:SOM3C for grassland.
!  11/27/2005 AJG Restructured the various options for estimating SOM23
!  02/06/2006 CHP Read SOM1, SOM2, and SOM3 initial fractions from FILEX.
!  09/11/2006 CHP Use total organic N instead of TOTN to initialize
!  10/25/2006 AJG Corrected SOM23E calculation with Hedley data.
!  10/25/2007 CHP/GH Changed name of SOMFRACTIONS file to SOMFR045.SDA
!  04/21/2008 CHP changed methodology for initial SOM fractions 
!
!  Called: SoilCNPinit_C
!  Calls : UPCASE
!***********************************************************************

      SUBROUTINE SOMINIT_C (CONTROL, ISWITCH,
     &  CES1, CES1M, CES1X, CES2, CES21M, CES21X, CES23M, !Input
     &  CES23X, CES3, CES3M, CES3X, CO2S1I, CO2S1S,       !Input
     &  N_ELEMS, S1S3I, S1S3S, S2S3I, S2S3S, SOILPROP,    !Input
     &  SSOMC, TXS1I, TXS1S,                              !Input

     &  ACCCO2, ACCMNR, CO2S1, S1S3, S2S3, SOM1C,         !Output
     &  SOM1E, SOM2C, SOM2E, SOM23C, SOM23E, SOM3C,       !Output
     &  SOM3E, TXS1)                                      !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
      IMPLICIT  NONE
      EXTERNAL ERROR, FIND, SOMFRAC_INIT, IPHedley_C, WARNING, INFO
      SAVE
!     ------------------------------------------------------------------

      CHARACTER*1   ISWPHO
      CHARACTER*2   PCR
      CHARACTER*6   SECTION
      CHARACTER*6,  PARAMETER :: ERRKEY  = 'SOMINI'
      CHARACTER*17  SOILLAYERTYPE(NL)
      CHARACTER*30  FILEIO
      CHARACTER*78 MSG(NL+4)

      INTEGER ERRNUM, FOUND, L, MULTI,
     &  LINC, LNUM, LUNIO, MNUM, N_ELEMS, NLAYR
      INTEGER, PARAMETER :: SRFC = 0, SOIL = 1

      REAL CO2S1I, CO2S1S, DIFF
      REAL MAX_SOM1E, MAX_SOM2E, MAX_SOM23E, MAX_SOM3E, MAX_SSOME
      REAL MIN_SOM1E, MIN_SOM2E, MIN_SOM23E, MIN_SOM3E, MIN_SSOME
      REAL NCONC, S1S3I, S1S3S, S2S3I, S2S3S
      REAL TXS1I, TXS1S

!     Variable NL defined in ModuleDefs.for
      REAL ACCCO2(0:1), BD(NL), CLAY(NL), CO2S1(0:NL),
     &  DLAYR(NL), KG2PPM(NL), OC(NL), ORGP(NL), PH(NL),
     &  P_FUMIG(NL), P_RESIDUAL(NL),P_TOTAL(NL), 
     &  Po_BICARB(NL), Po_HCl(NL), Po_HClhot(NL), Po_NAOH(NL), 
     &  Po_NaOHsonic(NL), PPM2KG(NL), S1S3(NL), S2S3(NL), SAND(NL),
     &  SOM1C(0:NL), SOM1FRAC(NL), SOM23C(NL), SOM2C(NL),
     &  SOM2FRAC(NL), SOM3C(NL), SOM3FRAC(NL), 
     &  SSOMC(0:NL), TOTN(NL), TotOrgN(NL), TOTP(NL), TXS1(NL)

      REAL ACCMNR(0:NL,3), 
     &  CES1(0:NL,3), CES1M(0:1,3), CES1X(0:1,3), 
     &  CES2(NL,3), CES21M(0:1,3),  CES21X(0:1,3), 
     &  CES3(NL,3),   CES3M(1,3),   CES3X(1,3), 
     &  SOM1E(0:NL,3), SOM1E_PPM(0:NL,3), SOM2E(NL,3), SOM23E(NL,3),
     &  SOM3E(NL,3), SSOME(0:NL,3)
      REAL SSOMN_DEFAULT(NL)
      REAL SOM1N_DEFAULT(NL), SOM2N_DEFAULT(NL), SOM3N_DEFAULT(NL)
      REAL Ratio
      REAL, DIMENSION(NL,NELEM) :: CES23
      REAL, DIMENSION(0:1,NELEM) :: CES23M, CES23X

      LOGICAL UseHedley, UseDefaultN 

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL
      TYPE (SoilType)   SOILPROP
      TYPE (SwitchType)  ISWITCH

!     Transfer values from constructed data types into local variables.
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO
      MULTI   = CONTROL % MULTI
      ISWPHO  = ISWITCH % ISWPHO

      BD      = SOILPROP % BD
      CLAY    = SOILPROP % CLAY
      DLAYR   = SOILPROP % DLAYR
      KG2PPM  = SOILPROP % KG2PPM 
      NLAYR   = SOILPROP % NLAYR
      OC      = SOILPROP % OC
      ORGP    = SOILPROP % ORGP  
      PH      = SOILPROP % PH  
      SAND    = SOILPROP % SAND
      TOTN    = SOILPROP % TOTN
      TOTP    = SOILPROP % TOTP
      TotOrgN = SOILPROP % TotOrgN
      SOILLAYERTYPE = SOILPROP % SOILLAYERTYPE

      PPM2KG(1:NLAYR)  = 1.0 / KG2PPM(1:NLAYR)

!***********************************************************************
!     Open FILEIO to get path to soils directory
!         and user input SOM1, SOM2 and SOM3 initial fractions
      OPEN (UNIT = LUNIO, FILE = FILEIO, STATUS='OLD', IOSTAT=ERRNUM)
      IF (ERRNUM /= 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)

!     Find INITIAL CONDITIONS Section for the previous crop (PCR).
      REWIND(LUNIO)
      SECTION = '*INITI'
      CALL FIND (LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND == 0) CALL ERROR (SECTION, 42, FILEIO, LNUM)
      READ (LUNIO, '(3X, A2)', IOSTAT = ERRNUM) PCR
      LNUM = LNUM + 1
      IF (ERRNUM /= 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, LNUM)

      CLOSE(LUNIO)

!     ------------------------------------------------------------------
!     Initialize SOM fractions
      CALL SOMFRAC_INIT(CONTROL, SOILPROP, SSOMC, 
     &    SOM1FRAC, SOM2FRAC, SOM3FRAC) 

!***********************************************************************
!     Read Hedley fractionation data if specified by ISWPHO = 'H'
      IF (ISWPHO == 'H') THEN
        CALL IPHedley_C (CONTROL, NLAYR,                  !Input
     &      P_FUMIG, P_RESIDUAL, P_TOTAL, Po_BICARB,      !Output
     &      Po_NaOH, Po_NaOHsonic, Po_HClhot, UseHedley)  !Output
      ELSE
        UseHedley = .False.
      ENDIF

!***********************************************************************
!     Initialize E mineralization accumulators.
      ACCMNR = 0.

!     Initialize accumulators for microbially respired CO2.
      ACCCO2 = 0.

!***********************************************************************
!     Surface:
!     Burke's equations only apply to soil layers, so the
!     surface SOM1C is just set to a default value. The various
!     SOM pools are calculated from SSOMC(L).
!     CHP, JWJ 07/12/2007 Remove the initial 100 kg/ha SOM1. 
!     It is too much for some deficient soils. 
!     SOM1C(SRFC) = 100.
!     IF (N_ELEMS > 0) SOM1E(SRFC,N) = SOM1C(SRFC) / CES1(SRFC,N)
!     IF (N_ELEMS > 1) SOM1E(SRFC,P) = SOM1C(SRFC) / CES1(SRFC,P)
      SOM1C = 0.0
      SOM1E = 0.0
      SOM2E = 0.0
      SOM3E = 0.0

      SOM1N_DEFAULT = 0.0
      SOM2N_DEFAULT = 0.0
      SOM3N_DEFAULT = 0.0
      SSOMN_DEFAULT = 0.0

!***********************************************************************
!     Soil
      LayerLoopC1: DO L = 1, NLAYR
!       ----------------------------------------------------------------
!       Initialize the SOM pools using Burke's equations.
!       ----------------------------------------------------------------
!       Burke's equation for estimating the SOM pool distribution
!       depends on the previous long-term use of the soil. This value
!       is set in Initial Conditions of fileX under Previous Crop.

!       --------------------------------------------------------------
!       SOM C
!       --------------------------------------------------------------
!       Calculate the total SOMC.
!        SSOMC(L) = OC(L) * BD(L) * DLAYR(L) * 1000.

!       Give each SOM pool it share of the total SOMC.
        SOM1C(L) = SSOMC(L) * SOM1FRAC(L)
        SOM2C(L) = SSOMC(L) * SOM2FRAC(L)
        SOM3C(L) = SSOMC(L) * SOM3FRAC(L)

!       --------------------------------------------------------------
!       Some soil-texture-dependent variables that affect the SOM
!       decomposition; the SRFC layer does not have texture.
!       ----------------------------------------------------------------
!       Calculate the effect of soil texture on the soil-microbe
!       (i.e. soil SOM1) decomposition rate.
        TXS1(L)  = TXS1I + TXS1S * SAND(L) / 100.

!       Calculate the C fraction lost as CO2, when soil SOM1
!       decomposes into SOM2.
        CO2S1(L) = CO2S1I + CO2S1S * SAND(L) / 100.

!       Calculate the C fraction transferred to SOM3 when soil SOM1
!       or SOM2 decomposes.
        S1S3(L)  = S1S3I + S1S3S * CLAY(L) / 100.
        S2S3(L)  = S2S3I + S2S3S * CLAY(L) / 100.

!       --------------------------------------------------------------
        IF (N_ELEMS > 0) THEN
!         ----------------------------------------------------------------
!         Calculate the N content of surface SOM1, soil SOM1, SOM2, SOM3.
!         ----------------------------------------------------------------
!         SOM N
!         ------------------------------------------------------------
!         Soil layers: before calculating the SOME content of the
!         three  SOM pools with the TotOrgN values in SOIL.SOL, first
!         calculate what they would be with the default C:E ratios.
          SOM1N_DEFAULT(L) = SOM1C(L) / CES1(L,N)   
          SOM2N_DEFAULT(L) = SOM2C(L) / CES2(L,N)
          SOM3N_DEFAULT(L) = SOM3C(L) / CES3(L,N)
          SSOMN_DEFAULT(L) = SOM1N_DEFAULT(L) + SOM2N_DEFAULT(L) 
     &                     + SOM3N_DEFAULT(L)
        ENDIF
      ENDDO LayerLoopC1

!***********************************************************************
      IF (N_ELEMS > 0) THEN
        LayerLoopN1: DO L = 1, NLAYR
!         If there are data on the total soil N content of the layer,
!         use these to initialize SOM N.
          IF (TotOrgN(L) >= 0.001) THEN
!           If there are data on the total soil N content, use these
!           to initialize SOM N. Calculate the E content of SOM3 as
!           done above, because this is relatively stable. Then give
!           soil SOM1E a fraction of the remaining SSOME in accordance
!           with its carbon share, and limit its C/E ratio to the
!           maximum and minimum ratios specified in SOMFX???.SDA. The
!           rest of the E goes to SOM2E.
          
!           Total SOM N available based on TotOrgN:
!           SSOME(L,N) = TOTN(L) * BD(L) * DLAYR(L) * 1000.
            SSOME(L,N) = TotOrgN(L) !kg/ha
            IF (SSOMN_DEFAULT(L) > 1.E-6) THEN
              Ratio = TotOrgN(L) / SSOMN_DEFAULT(L)
            ELSE
              Ratio = 0.0
            ENDIF
            SOM1E(L,N) = SOM1N_DEFAULT(L) * Ratio
            SOM2E(L,N) = SOM2N_DEFAULT(L) * Ratio
            SOM3E(L,N) = SOM3N_DEFAULT(L) * Ratio

!           Calculate maximum and minimum SOME values based on max and min
!             ratios in SOMFIX file.
            MAX_SOM1E = SOM1C(L) / CES1M(SOIL,N)
            MAX_SOM2E = SOM2C(L) / CES21M(SOIL,N)
            MAX_SOM3E = SOM3C(L) / CES3M(SOIL,N)
            MAX_SSOME = MAX_SOM1E + MAX_SOM2E + MAX_SOM3E

            MIN_SOM1E = SOM1C(L) / CES1X(SOIL,N)
            MIN_SOM2E = SOM2C(L) / CES21X(SOIL,N)
            MIN_SOM3E = SOM3C(L) / CES3X(SOIL,N)
            MIN_SSOME = MIN_SOM1E + MIN_SOM2E + MIN_SOM3E

!           Check that total organic N is within limits
            IF (SSOME(L,N) > MAX_SSOME .OR. SSOME(L,N) < MIN_SSOME) THEN
!             Total N > Max. -- use defaults
              UseDefaultN = .TRUE.
              EXIT LayerLoopN1
            ELSE
!             Total organic N OK, check each pool separately
              IF (SOM1E(L,N) < MIN_SOM1E) THEN    
!               Do not let SOM1 fall below minimum
                DIFF = SOM1E(L,N) - MIN_SOM1E
                SOM1E(L,N) = MIN_SOM1E
              ELSEIF (SOM1E(L,N) > MAX_SOM1E) THEN
!               Do not let SOM1 go above maximum
                DIFF = SOM1E(L,N) - MAX_SOM1E
                SOM1E(L,N) = MAX_SOM1E
              ELSE
                DIFF = 0.0
              ENDIF !SOM1E

!             Check SOM2E next, after adjusting with SOM1E excess or deficit
              SOM2E(L,N) = SOM2E(L,N) + DIFF
              IF (SOM2E(L,N) < MIN_SOM2E) THEN    
!               Do not let SOM1 fall below minimum
                DIFF = SOM2E(L,N) - MIN_SOM2E
                SOM2E(L,N) = MIN_SOM2E
              ELSEIF (SOM2E(L,N) > MAX_SOM2E) THEN
!               Do not let SOM1 go above maximum
                DIFF = SOM2E(L,N) - MAX_SOM2E
                SOM2E(L,N) = MAX_SOM2E
              ELSE
                DIFF = 0.0
              ENDIF !SOM2E

!             Check SOM3E last, after adjusting with SOM2E excess or deficit
              SOM3E(L,N) = SOM3E(L,N) + DIFF
              IF (SOM3E(L,N) < MIN_SOM3E) THEN    
!               Do not let SOM1 fall below minimum
                DIFF = SOM3E(L,N) - MIN_SOM3E
                SOM3E(L,N) = MIN_SOM3E
              ELSEIF (SOM3E(L,N) > MAX_SOM3E) THEN
!               Do not let SOM1 go above maximum
                DIFF = SOM3E(L,N) - MAX_SOM3E
                SOM3E(L,N) = MAX_SOM3E
              ELSE
                DIFF = 0.0
              ENDIF !SOM3D
              SSOME(L,N) = SOM1E(L,N) + SOM2E(L,N) + SOM3E(L,N)
            ENDIF   !SSOME(L,N) comparison with MAX_SSOME and MIN_SSOME

!           ---------------------------------------------------------
!           Write a warning message if total SOM N had to be adjusted
!           from the TotOrgN value.
            IF (ABS(DIFF) > 0.001) THEN
              WRITE(MSG(1),100) TOTN(L), L
              WRITE(MSG(2),101) 
100           FORMAT("Reported initial value of total N (",F5.2,
     &          "% in layer", I2,") was not used ")
101           FORMAT("because resulting C:N ratios were not within",
     &          " range of acceptable values.")
              CALL WARNING(2, ERRKEY, MSG)
            ENDIF
          ELSE
            UseDefaultN = .TRUE.
          ENDIF
        ENDDO LayerLoopN1

        LayerLoopN2: DO L = 1, NLAYR
          IF (UseDefaultN) THEN
            SOM1E(L,N) = SOM1N_DEFAULT(L)
            SOM2E(L,N) = SOM2N_DEFAULT(L)
            SOM3E(L,N) = SOM3N_DEFAULT(L)
          ELSE
!           Recalculate C:E ratios and sum of SOME pools,
            CES1(L,N) = SOM1C(L) / SOM1E(L,N)
            CES2(L,N) = SOM2C(L) / SOM2E(L,N)
            CES3(L,N) = SOM3C(L) / SOM3E(L,N)
          ENDIF
          SSOME(L,N) = SOM1E(L,N) + SOM2E(L,N) + SOM3E(L,N)
  
          IF (SOM1E(L,N) < 0. .OR. SOM2E(L,N) < 0. .OR.
     &        SOM3E(L,N) < 0.) THEN
!           Print a warning.
            WRITE(MSG(1),
     &        '("SOM1/2/3 < 0, probably due to an imbalance")')
            WRITE(MSG(2),
     &        '("between SLOC and SLNI in the SOIL.SOL file.")')
            CALL WARNING(2, ERRKEY, MSG)
          ENDIF   !End of IF block on SOM1E, SOM2E, SOM3E
        ENDDO LayerLoopN2
      ENDIF   !End of IF block on N_ELEMS > 0

!***********************************************************************
!       ==============================================================
!       Organic Phosphorus
!       ==============================================================
      IF (N_ELEMS > 1) THEN
        LayerLoopP1: DO L = 1, NLAYR
!         For P, maintain a different SOM pool structure than for C and N:
!         SOM1 is the same, but SOM2 and SOM3 are combined in SOM23.

          WRITE(MSG(1),'(A,I3)') "Soil layer",L

!         **********************************************************
!         ----------------------------------------------------------
!         Option #1: Hedley data is available. 
!         ----------------------------------------------------------
          IF (UseHedley) THEN
!           =========================
!           Soil microbial P (SOM1-P)
!           =========================
!           SOM1-P depends on whether there are fumigation-P data or
!           not. These are in ug/g (= mg/kg) --> convert to kg/ha  
!           with PPM2KG.

!           Fumigation data available.
            IF (P_FUMIG(L) > 0.0001) THEN
              SOM1E(L,P) = P_FUMIG(L) * PPM2KG(L)
            ELSE

              SELECT CASE(PCR)
!             If previous crop was grassland, microbes hold about 2.5 % of
!             total P; for cropped land with organic amendents it is 1% and
!             for continuous cropping it is 0.5%. (A. Oberson, pers.comm.).
              CASE ('BA','MZ','ML','RI','SG','WH',
     &              'CH','CP','BN','FB','PN','SB',
     &              'VB','CS','PT','TN','TR','LT','PE')
                SOM1E(L,P) = 0.005 * P_TOTAL(L) * PPM2KG(L)

              CASE ('BM','BH','BR','G0','G1','GW')
                SOM1E(L,P) = 0.025 * P_TOTAL(L) * PPM2KG(L)
                  
              CASE DEFAULT
                SOM1E(L,P) = 0.01 * P_TOTAL(L) * PPM2KG(L)
              END SELECT
            ENDIF   !End of IF block on P_FUMIG

!           Convert SOM1E(L,P) to ug/g (ppm) units, as this is needed for
!           Sharpley's equations and others.
            SOM1E_PPM(L,P) = SOM1E(L,P) * KG2PPM(L)

!           ----------------------------------------------------------------
!           Residual P and stable inorganic Pi, modified according to SOM1-P.
!           ----------------------------------------------------------------
!           Deduct the microbial P from the residual P, or the P balance
!           would not be correct, because microbial P is not part of the
!           Hedley fractionation.
            P_residual(L) = P_residual(L) - SOM1E_PPM(L,P)

!           =======
!           SOM23-P
!           =======
!           SOM P has a combined SOM2+3 (SOM23) pool; SOM2E(L,P) and
!           SOM3E(L,P) don't exist. If there are data on Po fractions
!           from a Hedley fractionation, use these.
!AJG 061025: must include both Po_HCl and Po_HClhot
            IF (Po_BICARB(L) > 0. .AND. Po_NaOH(L) > 0. .AND.
     &        Po_NaOHsonic(L) > 0. .AND. Po_HCl(L) > 0. .AND.
     &        Po_HClhot(L) > 0.) THEN

!CHP 10/23/2007 Po_HCl is not supplied from IPHedley_C!!!
!     Remove it from the equation
              SOM23E(L,P) = (Po_BICARB(L) + Po_NAOH(L) +
!     &          Po_NaOHsonic(L) + Po_HCl(L) + Po_HClhot(L) +
     &          Po_NaOHsonic(L) + Po_HClhot(L) +
     &          0.5 * P_RESIDUAL(L)) * PPM2KG(L)

            ELSE
!             If sufficient Hedley data not available, estimate SOM23E
!               from SOM1E (already calculated using Hedley P_FUMIG or 
!               P_TOTAL).  This is still a Hedley data estimation!
              SELECT CASE (SOILLAYERTYPE(L))

!             Calcareous soils.-- Sharpley et al. (1989, Table 4)
              CASE('CALCAREOUS       ')
                SOM23E(L,P) = (53.5 * OC(L) - 3.3 - SOM1E_PPM(L,P))
     &             * PPM2KG(L)

!             Slightly weathered soils. This is not included in Sharpley
!             et al. (1989), so take Sharpley et al. (1984, Table 3).
              CASE('SLIGHTLYWEATHERED')
!               Calculate the overall soil N conc (%), because TotOrgN may not
!               have a value or the SOM-N may have been adjusted.
                NCONC = SSOME(L,N) / (BD(L) * DLAYR(L) * 1000.)

!               Reduce SOM1E (converted to ug/g units) from the Po calculated according to
!               Sharpley et al. (1984, Table 3), so that what is left is SOM23E 
!               (also in ug/g units) and then convert all to kg/ha units.
                SOM23E(L,P) = (44.4 + 1130 * NCONC - SOM1E_PPM(L,P))
     &            * PPM2KG(L)

!             Highly weathered soils. -- Sharpley et al. (1989, Table 4), 
              CASE('HIGHLYWEATHERED  ')
                SOM23E(L,P) = (62.1 * OC(L) + 63.4 - SOM1E_PPM(L,P)) 
     &            * PPM2KG(L)

!             ANDISOLS
              CASE('ANDISOL          ')
!###AJG to be finished***

              CASE DEFAULT
!###AJG to be finished*** ==> Same as in line 675 ???

              END SELECT
            ENDIF   !End of IF block on Po_BICARB, Po_NAOH, etc.

            SSOME(L,P) = SOM1E(L,P) + SOM23E(L,P)
            ORGP(L) = SSOME(L,P) * KG2PPM(L)
            MSG(2) = "Organic P from Hedley fractionation data."
            WRITE(MSG(3),'(A,F6.2,A)') "Organic P: ",ORGP(L), " ppm"
            MNUM = 3

!         **********************************************************
!         ----------------------------------------------------------
!         Option #2: No Hedley data available. Take Po value from
!         the second-tier soils data.  Use value of organic P if
!         available, otherwise, estimate from other soil data.
!         ----------------------------------------------------------
          ELSE    !No Hedley data

            IF (ORGP(L) > -1.E-5) THEN
!             Organic P read directly from soils file:
              ORGP(L) = MAX(0.0, ORGP(L))
              WRITE(MSG(2),'(A,F6.2,A)') 
     &            "Organic P read from soil file: ",ORGP(L), " ppm"
              MNUM = 2

            ELSE
!             According to the PhD thesis of Upendra Singh 
!             (Univ. Hawaii, 1985) and also Sharpley et al. (1984, 
!             Table 3 or 1989, Table 4), the Po conc.can be set as a 
!             function of the soil pH, orgC or Ntot conc.
!             This depends on the soil type.
              SELECT CASE(SOILLAYERTYPE(L))
              CASE('CALCAREOUS       ')   !Thesis of U. Singh.
                ORGP(L) = 
     &            (200. * EXP(-1.8 * ((pH(L) - 3.) / 6.) ** 2.)) 
     &                          * (1. - EXP(-0.55 * OC(L)))

              CASE('SLIGHTLYWEATHERED')   !Thesis of U. Singh.
                ORGP(L) = 
     &            (900. * EXP(-1.5 * ((pH(L) - 10.) / 12.) ** 2.))
     &                          * (1. - EXP(-0.1 * OC(L)))

              CASE('HIGHLYWEATHERED  ')   !Thesis of U. Singh.
                ORGP(L) = 
     &            (200. * EXP(-1.85 * ((pH(L) - 3.) / 6.) ** 2.)) 
     &                          * (1. - EXP(-0.35 * OC(L)))

!             ANDISOLS
!             CASE('ANDISOL          ')
!###AJG ****to be finished***

              CASE DEFAULT
                ORGP(L) = 
     &            (520. * EXP(-1.5 * ((pH(L) - 7.) / 8.) ** 2.)) 
     &                          * (1. - EXP(-0.135 * OC(L)))

              END SELECT

              WRITE(MSG(2),'(A,F6.2,A)') 
     &            "Organic P estimated: ",ORGP(L)," ppm"
              WRITE(MSG(3),'(2(A,F6.2))') 
     &            "Organic C: ", OC(L), " ppm;  pH: ", pH(L)
              MNUM = 3

            ENDIF   !End of IF block on ORGP(L) > -1.E-5.

!           Limit organic P to be no greater than measured total P
            IF (TOTP(L) > 1.E-5 .AND. ORGP(L) > TOTP(L)) THEN
              ORGP(L) = TOTP(L)
              MSG(MNUM+1) = "Organic P cannot be greater than " //
     &            "Total P read from soil file."
              WRITE(MSG(MNUM+2),'(3(A,F6.2))') "Total P: ",TOTP(L),
     &            " ppm; Organic P: ",ORGP(L), " ppm"
              MNUM = MNUM + 2
            ENDIF

!           Use ORGP, no matter how it was estimated, to set SOM23E and SOM1E(L,P).
            SSOME(L,P) = ORGP(L) * PPM2KG(L)

            SELECT CASE(PCR)
              CASE('BM','BH','BR','G0','G1','GW')
                SOM1E(L,P) = 0.06 * SSOME(L,P)
              CASE DEFAULT
                SOM1E(L,P) = 0.03 * SSOME(L,P)
            END SELECT

!           Deduct the microbial SOM to get the SOM23E.
            SOM23E(L,P) = SSOME(L,P) - SOM1E(L,P)
          ENDIF  !End of IF block on UseHedley

!         Calculate a combined SOM2C and SOM3C, needed for determining
!         the P mineralization due to CO2 production in EFLOW.
          SOM23C(L) = SOM2C(L) + SOM3C(L)
          SSOME(L,P) = SOM1E(L,P) + SOM23E(L,P)

!         ------------------------------------------
!         Check C:E ratios for SOM pools

!         Calculate maximum and minimum SOME values based on max and min
!           ratios in SOMFIX file.
          MAX_SOM1E  = SOM1C(L)  / CES1M(SOIL,P)
          MAX_SOM23E = SOM23C(L) / CES23M(SOIL,P)
          MAX_SSOME  = MAX_SOM1E + MAX_SOM23E

          MIN_SOM1E  = SOM1C(L)  / CES1X(SOIL,P)
          MIN_SOM23E = SOM23C(L) / CES23X(SOIL,P)
          MIN_SSOME = MIN_SOM1E + MIN_SOM23E

!         Check that total organic P is within limits
          IF (SSOME(L,P) > MAX_SSOME) THEN
!           Total P > Max. -- reset all pools to max
            SOM1E(L,P)  = MAX_SOM1E
            SOM23E(L,P) = MAX_SOM23E
            DIFF = SSOME(L,P) - MAX_SSOME
            SSOME(L,P) = MAX_SSOME

            ORGP(L) = SSOME(L,P) * KG2PPM(L)
            MSG(MNUM+1) = "C:P ratio below acceptable range. "
            WRITE(MSG(MNUM+2),'(A,F6.2)') "Revised Organic P: ", ORGP(L)
            MNUM=MNUM+2

          ELSEIF (SSOME(L,P) < MIN_SSOME) THEN
!           Total P < Min. -- reset all pools to min
            SOM1E(L,P)  = MIN_SOM1E
            SOM23E(L,P) = MIN_SOM23E
            DIFF = SSOME(L,P) - MIN_SSOME
            SSOME(L,P) = MIN_SSOME

            ORGP(L) = SSOME(L,P) * KG2PPM(L)
            MSG(MNUM+1) = "C:P ratio above acceptable range. "
            WRITE(MSG(MNUM+2),'(A,F6.2)') "Revised Organic P: ", ORGP(L)
            MNUM=MNUM+2

          ELSE
!           Total organic P OK, check each pool separately
            IF (SOM1E(L,P) < MIN_SOM1E) THEN    
!             Do not let SOM1 fall below minimum
              DIFF = SOM1E(L,P) - MIN_SOM1E
              SOM1E(L,P) = MIN_SOM1E
              MSG(MNUM+1)=
     &          "C:P ratio for microbial SOM P above acceptable range."
              MNUM=MNUM+1
            ELSEIF (SOM1E(L,P) > MAX_SOM1E) THEN
!             Do not let SOM1 go above maximum
              DIFF = SOM1E(L,P) - MAX_SOM1E
              SOM1E(L,P) = MAX_SOM1E
              MSG(MNUM+1)=
     &          "C:P ratio for microbial SOM P below acceptable range."
              MNUM=MNUM+1
            ELSE
              DIFF = 0.0
            ENDIF !SOM1E

!           Check SOM23E next, after adjusting with SOM1E excess or deficit
            SOM23E(L,P) = SOM23E(L,P) + DIFF
            IF (SOM23E(L,P) < MIN_SOM23E) THEN    
!             Do not let SOM23 fall below minimum
              DIFF = SOM23E(L,P) - MIN_SOM23E
              SOM23E(L,P) = MIN_SOM23E
              MSG(MNUM+1) = 
     &          "C:P ratio for stable SOM P above acceptable range. "
              MNUM=MNUM+1
            ELSEIF (SOM23E(L,P) > MAX_SOM23E) THEN
!             Do not let SOM23 go above maximum
              DIFF = SOM23E(L,P) - MAX_SOM23E
              SOM23E(L,P) = MAX_SOM23E
              MSG(MNUM+1) = 
     &          "C:P ratio for stable SOM P below acceptable range. "
              MNUM=MNUM+1
            ELSE
              DIFF = 0.0
            ENDIF !SOM23E

            SSOME(L,P) = SOM1E(L,P) + SOM23E(L,P)

            ORGP(L) = SSOME(L,P) * KG2PPM(L)
            WRITE(MSG(MNUM+1),'(A,F6.2)') "Organic P: ", ORGP(L)
            MNUM=MNUM+1

          ENDIF   !SSOME(L,P) comparison with MAX_SSOME and MIN_SSOME

          CALL INFO(MNUM, ERRKEY, MSG)

!         Recalculate C:E ratios and sum of SOME pools,
          IF (SOM1E(L,P) > 1.E-7) THEN
            CES1(L,P)  = SOM1C(L)  / SOM1E(L,P)
          ELSE
            CES1(L,P) = CES1X(SOIL,P)
          ENDIF
          IF (SOM23E(L,P) > 1.E-7) THEN
            CES23(L,P) = SOM23C(L) / SOM23E(L,P)
          ELSE
            CES23(L,P) = CES23X(SOIL,P)
          ENDIF
          SSOME(L,P) = SOM1E(L,P) + SOM23E(L,P)

        ENDDO LayerLoopP1
      ENDIF  !P section
!***********************************************************************
!     Transfer back into soil data revised organic P
      SOILPROP % ORGP = ORGP

      RETURN
      END SUBROUTINE SOMINIT_C
!=======================================================================



!=======================================================================
!  SOMFRAC_INIT, Subroutine

!  Sets default initial SOM fractions based on (in this order of precedence)
!     1. Measured data from soil analysis section of FILEX
!     2. Field management history and duration
!     3. regression equation

!  Revision history:
!  08/11/2006 CHP Written
!  04/21/2008 CHP Rewritten for modified file format, soil analysis section
!                 Adiku equation
!  04/30/2008 CHP Path for SDA files set in DSSATPRO file
!
!  Called: SOMINIT_C
!=======================================================================
      SUBROUTINE SOMFRAC_INIT(CONTROL, SOILPROP, SSOMC, 
     &    SOM1FRAC, SOM2FRAC, SOM3FRAC) 

      Use ModuleDefs
      EXTERNAL ERROR, FIND, GETLUN, IGNORE, INFO, PATH
      SAVE

      TYPE (ControlType), INTENT(IN) :: CONTROL
      TYPE (SoilType),    INTENT(IN) :: SOILPROP
      REAL,DIMENSION(0:NL),INTENT(IN):: SSOMC
      REAL,DIMENSION(NL), INTENT(OUT):: SOM1FRAC, SOM2FRAC, SOM3FRAC

      CHARACTER*5 FldHist   
      INTEGER FHDur, LUNIO, NLAYR   !, Nsoils

!     Local variables:
      CHARACTER*1,  PARAMETER :: BLANK = ' '
      CHARACTER*6   SECTION
      CHARACTER*6,  PARAMETER :: ERRKEY  = 'SOMINI'
      CHARACTER (len=12) TEXTURE(NL), TEX(12), NAMEF
      CHARACTER*14 Method(NL)
      CHARACTER*30  FILEIO
      CHARACTER*50 PrevMgmtDesc
      CHARACTER*78 MSG(NL+3)
      CHARACTER*80 PATHSD
      CHARACTER*92 SOMPF
      CHARACTER*120 CHAR
      INTEGER ERRNUM, FOUND, I, ISECT, J, L, LINC
      INTEGER LNUM, LUN, NTEX, PFLAG
      LOGICAL FEXIST

      REAL Frac, FHDur_real, StableC
      REAL, DIMENSION(12) :: S3A, S3B  !0-20 cm and 0-40 cm layers
!     SOM3 values from file for 12 textures, 6 durations, 2 depths
      REAL, DIMENSION(12,6,2) :: S3V
      REAL, DIMENSION(NL) :: CLAY, DLAYR, DS, FOMC, KG2PPM, OC
      REAL, DIMENSION(NL) :: SASC, SILT, SOM_TOT

      LOGICAL DONE
      CHARACTER*12 SOMFILE 

      SOMFILE = 'SOMFR' // ModelVerTxt // '.SDA'

!     ------------------------------------------------------------------
      DS      = SOILPROP % DS
      DLAYR   = SOILPROP % DLAYR
      KG2PPM  = SOILPROP % KG2PPM
      NLAYR   = SOILPROP % NLAYR
      CLAY    = SOILPROP % CLAY
      OC      = SOILPROP % OC
      SASC    = SOILPROP % SASC
      SILT    = SOILPROP % SILT
      TEXTURE = SOILPROP % TEXTURE

!     ************************************************************
!     1.  MEASURED DATA
!     ************************************************************
      SOM3FRAC = -99.
      DONE = .TRUE.

      DO L = 1, NLAYR
        SOM_TOT(L) = SSOMC(L) * KG2PPM(L) * 1.E-4 !g/100g
        IF (SASC(L) > 0.0) THEN
!         Use measured data from soil analysis section
          StableC = SASC(L)                      !g/100g
          SOM3FRAC(L) = StableC / SOM_TOT(L)
          SOM3FRAC(L) = AMIN1(SOM3FRAC(L), 0.98)
          Method(L) = 'Measured data '    
        ELSE
          DONE = .FALSE.
        ENDIF
      ENDDO
      IF (DONE) GOTO 5000

!     ************************************************************
!     2.  FIELD HISTORY
!     ************************************************************
!     Open FILEIO to get field history and duration
      LUNIO  = CONTROL % LUNIO
      FILEIO = CONTROL % FILEIO
      OPEN (UNIT = LUNIO, FILE = FILEIO, STATUS='OLD', IOSTAT=ERRNUM)
      IF (ERRNUM /= 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)

!     Read previous management look-up code from FIELDS section
      REWIND(LUNIO)
      SECTION = '*FIELD'
      CALL FIND(LUNIO, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0)  THEN
        FldHist = 'ERROR'
        FHDUR = -99
      ELSE
        READ(LUNIO,'(/,A)', IOSTAT=ERRNUM) CHAR
        IF (ERRNUM == 0) THEN
          READ(CHAR,'(81X,A5)', IOSTAT=ERRNUM) FldHist
          IF (ERRNUM /= 0) FldHist = 'ERROR'
          READ(CHAR,'(86X,F6.0)', IOSTAT=ERRNUM) FHDur_real
          IF (ERRNUM /= 0) FHDur = -99
          IF (FHDur_real < 0) FldHist = 'ERROR'
        ELSE
          FldHist = 'ERROR'
          FHDur = -99
        ENDIF
      ENDIF

      CLOSE(LUNIO)

!     ------------------------------------------------------------------
!     Read SOM3 values for this field history
!     If problems, set FldHist = 'ERROR'
      IF (.NOT. FldHist == 'ERROR') THEN
        FHDur = NINT(FHDur_real)
        MSG(1) = "Field history for initial soil organic " 
     &            // "matter composition."

!       If file is available, read initial SOM values from file.

        SOMPF = SOMFILE
        INQUIRE (FILE = SOMPF, EXIST = FEXIST)
        IF (.NOT. FEXIST) THEN
          CALL PATH('STD',CONTROL%DSSATP,PATHSD,PFLAG,NAMEF)
          SOMPF = TRIM(PATHSD) // SOMFILE
        ENDIF

        INQUIRE(FILE = SOMPF, EXIST = FEXIST)
        IF (FEXIST) THEN
!         Open the SOMFR???.SDA file to read the fractions of SOM3C
          CALL GETLUN('FINPUT', LUN)
          LNUM = 0
          OPEN (UNIT = LUN, FILE = SOMPF, STATUS = 'OLD', IOSTAT=ERRNUM)
          IF (ERRNUM /= 0) THEN
            FldHist = 'ERROR'
            MSG(2) = 'Error opening file: '
            GOTO 100
          ENDIF  

!         Find appropriate section from previous management code
   50     SECTION = "@" // FldHist
          REWIND(LUN)
          CALL FIND(LUN, SECTION, LINC, FOUND)
          IF (FOUND /= 0) THEN
            BACKSPACE(LUN)
            READ(LUN,'(7X,A50)',IOSTAT=ERRNUM) PrevMgmtDesc
            IF (ERRNUM /= 0) WRITE(PrevMgmtDesc,"(A50)") " "
!           Read SOM fractions for up to 12 soil textures
            DO I = 1, 12
              CALL IGNORE(LUN, LINC, ISECT, CHAR)
              IF (ISECT == 1) THEN
                READ (CHAR, '(5X,A12,1X,12F7.0)',IOSTAT=ERRNUM) 
     &            TEX(I), (S3V(I,J,1), S3V(I,J,2),J=1,6)
                SELECT CASE(FHDur)
                CASE(:0)    !Use column 1
                  S3A(I) = S3V(I,1,1) 
                  S3B(I) = S3V(I,1,2)
                CASE(1:5)   !Interpolate between columns 1 & 2
                  Frac = FLOAT(FHDur) / 5.
                  S3A(I) = S3V(I,1,1) + Frac * (S3V(I,2,1)-S3V(I,1,1))
                  S3B(I) = S3V(I,1,2) + Frac * (S3V(I,2,2)-S3V(I,1,2))
                CASE(6:10)  !Interpolate between columns 2 & 3
                  Frac = (FLOAT(FHDur) - 5.) / 5.
                  S3A(I) = S3V(I,2,1) + Frac * (S3V(I,3,1)-S3V(I,2,1))
                  S3B(I) = S3V(I,2,2) + Frac * (S3V(I,3,2)-S3V(I,2,2))
                CASE(11:20) !Interpolate between columns 3 & 4
                  Frac = (FLOAT(FHDur) - 10.) / 10.
                  S3A(I) = S3V(I,3,1) + Frac * (S3V(I,4,1)-S3V(I,3,1))
                  S3B(I) = S3V(I,3,2) + Frac * (S3V(I,4,2)-S3V(I,3,2))
                CASE(21:60) !Interpolate between columns 4 & 5
                  Frac = (FLOAT(FHDur) - 20.) / 40.
                  S3A(I) = S3V(I,4,1) + Frac * (S3V(I,5,1)-S3V(I,4,1))
                  S3B(I) = S3V(I,4,2) + Frac * (S3V(I,5,2)-S3V(I,4,2))
                CASE(61:100)    !Interpolate between columns 5 & 6
                  Frac = (FLOAT(FHDur) - 60.) / 40.
                  S3A(I) = S3V(I,5,1) + Frac * (S3V(I,6,1)-S3V(I,5,1))
                  S3B(I) = S3V(I,5,2) + Frac * (S3V(I,6,2)-S3V(I,5,2))
                CASE(101:)  !Use column 6
                  S3A(I) = S3V(I,6,1) 
                  S3B(I) = S3V(I,6,2)
                END SELECT
                IF (ERRNUM /= 0) THEN
                  WRITE(MSG(2),'(A,A)') "Error reading data for ",
     &            "Field history code, ",  FldHist
                  FldHist = 'ERROR'
                  GOTO 100
                ENDIF
              ENDIF
            ENDDO
            NTEX = I - 1
          ELSE
            WRITE(MSG(2),'(A,A,A)') "Field history code, ", FldHist, 
     &            " not found in file:"
            FldHist = 'ERROR'
            GOTO 100
          ENDIF

        ELSE
          FldHist = 'ERROR'
          MSG(2) = 'File not found:'
          GOTO 100
        ENDIF

  100   IF (FldHist == 'ERROR') THEN
          WRITE(MSG(3),'(A,A)') "File: ",SOMPF(1:72)
          MSG(4) = "Default intial SOM fractions will be used."
          CALL INFO(4, ERRKEY, MSG)
        ELSE

          WRITE(MSG(2),'(A,A)') "File: ",SOMPF(1:72)
          WRITE(MSG(3),'(A,A,1X,A)')"Field history code: ",FldHist,
     &        PrevMgmtDesc
          WRITE(MSG(4),'(A,I3,A)') "Duration: ", FHDur, " years"
          CALL INFO(4, ERRKEY, MSG)
        ENDIF
      ENDIF

!     ------------------------------------------------------------------
!     Loop thru soil layers to set initial SOM fractions
!     1. If soil analysis data are available, use them
!     2. Next, use Field history
!     3. Next, use Adiku equation

      DO L = 1, NLAYR
        IF (SOM3FRAC(L) < 0) THEN
          IF (.NOT. FldHist == 'ERROR' .AND. DS(L) <= 40.) THEN
!           Use field history and duration
            DO I = 1, NTEX
              IF (TEXTURE(L) == TEX(I)) THEN
                IF (DS(L) <= 20.) THEN
!                 Entire layer < 20 cm
                  SOM3FRAC(L) = S3A(I)
                ELSE
                  IF(L > 1) THEN
                    IF (DS(L-1) < 20.) THEN
!                     Layer divided 0-20 and 20-40 cm
                      SOM3FRAC(L) = (S3A(I) * (20. - DS(L-1)) + 
     &                        S3B(I) * (DS(L) - 20.)) / DLAYR(L)
                    ELSE
!                     Entire layer in 20-40 cm
                      SOM3FRAC(L) = S3B(I)
                    ENDIF
                  ELSE
                      SOM3FRAC(L) = (S3A(I) * 20. + 
     &                        S3B(I) * (DS(L) - 20.)) / DLAYR(L)
                  ENDIF
                ENDIF   
                EXIT  !Found values for this layer, go on to next layer
              ENDIF    
            ENDDO     !Texture loop
            Method(L) = 'Field history ' 
          ENDIF
        ENDIF

!     ************************************************************
!     3.  REGRESSION EQUATION
!     ************************************************************
        IF (SOM3FRAC(L) < 0.) THEN
          IF (SOM_TOT(L) > 1.E-6) THEN
!           Adiku equation
            StableC = 0.15 * (CLAY(L) + SILT(L)) + 0.69    !g/kg
            StableC = StableC / 10.                        !g/100g
!           Gargiulo's regression
!           y = 0.0093x + 0.1829
!           StableC = 0.0093 * (CLAY(L) + SILT(L)) + 0.1829   !g/100g
            SOM3FRAC(L) = StableC / SOM_TOT(L)
            Method(L) = 'Regression eqn'
          ELSE
            SOM3FRAC(L) = 0.98
            Method(L) = 'Zero Organic C'
          ENDIF
        ENDIF
      ENDDO

!     ------------------------------------------------------------------
 5000 CONTINUE
      MSG(1) = 
     & "Initial SOM fractions (fraction of total soil organic matter):"
      WRITE(MSG(2),'(21X,A)') "SAND   TOC  FOMC  SOMC  SOM1  SOM2  SOM3"
      MSG(3) = " Lyr Dep Texture       %     %     %     %" // 
     &      "   frac  frac  frac Method"

      DO L = 1, NLAYR
!       Limit SOM3 to 98% of total SOM
        SOM3FRAC(L) = AMIN1(SOM3FRAC(L), 0.98)

!       SOM1 is volatile and a small portion of total, assume 5% of non-SOM3
        SOM1FRAC(L) = 0.05 * (1.0 - SOM3FRAC(L))

!       SOM2 is remaining fraction of total SOM
        SOM2FRAC(L) = 1.0 - SOM1FRAC(L) - SOM3FRAC(L)

!!***********************************************************************
!!       TEMP CHP Use Tirol-Padre equation for SOM2 g/100g
!        SOM2_p = (7.71 - 0.07 * (CLAY(L) + SILT(L)) / OC(L)) / 10. 
!        SOM2_P = MIN(SOM2_P, 0.20 * OC(L))
!        SOM2FRAC(L) = SOM2_P / SOM_TOT(L)
!        SOM2FRAC(L) = MIN(SOM2FRAC(L), 0.95 * (1.0 - SOM3FRAC(L)))
!        SOM1FRAC(L) = 1.0 - SOM3FRAC(L) - SOM2FRAC(L)
!!***********************************************************************

        FOMC(L) = OC(L) - SOM_TOT(L)
        WRITE(MSG(L+3),'(I3,I4,1X,A,F5.1,6F6.3,1X,A)') 
     &      L, NINT(DS(L)), TEXTURE(L), SOILPROP%SAND(L), OC(L), 
     &      FOMC(L), SOM_TOT(L), SOM1FRAC(L), SOM2FRAC(L), 
     &      SOM3FRAC(L), METHOD(L)
      ENDDO
      CALL INFO(NLAYR+3,ERRKEY,MSG)
!!***********************************************************************
        
      RETURN
      END SUBROUTINE SOMFRAC_INIT
