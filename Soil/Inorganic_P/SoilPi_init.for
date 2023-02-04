!=======================================================================
!  SoilPi_init USING SOIL TEST P
!
!  Initalize inorganic soil P variables.
!  1. Use Hedley fractionation data if available (indicated by 
!     ISWPHO = 'H') to estimate labile, active and stable inorganic P.
!  2. Otherwise, use measured P data as indicated by soil type and P 
!     extraction method.
!  3. Otherwise, estimate labile P using other available soil data
!     depending on soil type.  If the required data is not available, 
!     the program will stop.
!  4. Estimate active and stable soil P from labile P (unless already
!     done in Hedley section).

!-----------------------------------------------------------------------
!  REVISION HISTORY
!  08/../1997 AG  Written as CHEM_INI.
!  12/../1998 AG  Added CH_UPT_LAYER
!  09/19/2003 AJG Brought into modular format and linked to the 
!                 CENTURY-based SOM/residue module.
!  12/17/2003 AJG Renamed all the CH_ and CHEM_ variables to P_ variables.
!  02/12/2004 AJG Renamed the subroutine from CHEM_INI to P_INI.
!  08/30/2004 AJG Added default values for Hedley fractions.
!  03/25/2005 UPS Modified initializations
!  04/13/2005 CHP Renamed SoilPi_init
!  01/13/2006 CHP Added warning and error messages for input data.
!-----------------------------------------------------------------------
!  Called by: SoilPi
!=======================================================================

      SUBROUTINE SoilPi_init (CONTROL, 
     &    ISWPHO, SOILPROP,                               !Input
     &    K_ACT2LAB, K_ACT2STA, K_LAB2ACT, K_STA2ACT,     !Output
     &    PiActive, PiLabile, PiStable)                   !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
      IMPLICIT  NONE
      EXTERNAL IPHedley_inorg, WARNING, ErrorCode, ERROR, INFO
!     ------------------------------------------------------------------
      CHARACTER*1  ISWPHO
      CHARACTER*6, PARAMETER :: ERRKEY = 'SPINIT'
      CHARACTER*17 SOILLAYERTYPE(NL)
      CHARACTER*78 MSG(20), WMSG(10)

      INTEGER I, L, NMSG
      INTEGER NLAYR !, YREND  

!     Soil properties:
      REAL, DIMENSION(NL) :: CaCO3, CEC, CLAY, EXK, KG2PPM 
!     REAL, DIMENSION(NL) :: EXCA, EXNA, TOTP 
      REAL, DIMENSION(NL) :: P_AVAILINDEX, PH, TOTBAS
      REAL, DIMENSION(NL) :: PiActive, PiLabile, PiStable, PiTotal

!     Rate coefficients:
      REAL, DIMENSION(NL) :: K_ACT2LAB, K_ACT2STA, K_LAB2ACT, K_STA2ACT

!     Soil analysis data:
      CHARACTER*5  PMethod, Method
      REAL, DIMENSION(NL) :: PMeasured
      REAL PMIN, PFunc

!     Hedley fractionation data:
      LOGICAL UseHedley

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL
      TYPE (SoilType)    SOILPROP

      CaCO3  = SOILPROP % CaCO3
      CEC    = SOILPROP % CEC
      CLAY   = SOILPROP % CLAY
!     EXCA   = SOILPROP % EXCA
      EXK    = SOILPROP % EXK 
!     EXNA   = SOILPROP % EXNA
      KG2PPM = SOILPROP % KG2PPM
      NLAYR  = SOILPROP % NLAYR
      PH     = SOILPROP % PH
      TOTBAS = SOILPROP % TOTBAS
!     TOTP   = SOILPROP % TOTP
      SOILLAYERTYPE = SOILPROP % SOILLAYERTYPE

!     ------------------------------------------------------------------
!     Initialize.
      PiActive   = 0.0
      PiStable   = 0.0
      PiLabile   = -99.

      IF (ISWPHO == 'N') RETURN

!     ------------------------------------------------------------------
!     USE HEDLEY DATA
!     ------------------------------------------------------------------
!     If soil phosphorus switch is set to 'H', use Hedley fractionation data.
      IF (ISWPHO == 'H') THEN
        CALL IPHedley_inorg (CONTROL, SOILPROP,           !Input    
     &    PiActive, PiLabile, PiStable, UseHedley)        !Output
      ELSE
        UseHedley = .False.
      ENDIF

      IF (.NOT. UseHedley) THEN
!     ------------------------------------------------------------------
!     USE SOIL ANALYSIS DATA
!     ------------------------------------------------------------------
        PMeasured = SOILPROP % EXTP   !Extractable P
        PMethod   = SOILPROP % SMPX   !Method of extraction
      ENDIF
!     ------------------------------------------------------------------
!     Jones et al. (1984) and Sharpley et al. (1984, 1989) 
!       SSSAJ 48: 800-805 and 805-809; SSSAJ 53:153-158.
!     Singh, U. (1985). PhD Thesis, University of Hawaii. Includes all data 
!       from Sharpley et al. + Mauinet + SCS Lincoln + MOA, Fiji.

!     Brief description of methods:
!     -----------------------------
!     OLSEN P (Olsen and Sommers, 1982)
!       5g of soil shaken with 100 ml 0.5 M NaHCO3 (pH=8.5) for 30 min.
!     BRAY1 P (Bray and Kurtz, 1945)
!       2 g soil shaken in 20 ml of 0.03 M NH4F and 0.025 M HCl for 1 minute.
!     Double Acid (Sabbe and Breland, 1974)
!       5g of soil shaken with 20 ml of 0.05 M HCl and 0.0125 M H2SO4 for 
!       5 minutes
!     Hydroxide P (Dalal, 1973)
!        1 g soil shaken for 16 h with 0.25M NaOH and 0.1 M Na2CO3
!     Modified Truog (Ayres and Hagihara, 1952)
!       1 g soil equilibrated with 20 ml water for 24 h. Then extracted
!       with 100 ml 0.02 N H2SO4 containing 3 g of (NH4)2SO4.
!     Water P (adapted from Olsen and Sommers, 1982)
!       2.5g soil shaken with 25 ml water for 1 h then centrifuged at 10,000 rpm
!       for 20 mins
!     Solution P  Phosphate Sorption (Fox and Kamprath, 1970)
!       3g soil equilibrated for 6 days in 30 ml 0.01 M CaCl2. The samples 
!       (over 6-day period) were shaken longitudinally for 30 min period twice
!       daily.
!     Sulfuric Acid P (unignited sample corresponds to inorganic P)
!     Modified - use 0.5 N H2SO4 using procedure of Walker and Adams, 1958)
!
!     For all methods P was determined colorimetrically after filtering
!       Whatman’s #42) using ascorbic acid method (Olsen and Sommers, 1982).
!     ----------------------------------------------------------------

!     If there are data on which soil type a soil layer has (calcareous,
!       slightly or highly weathered), then estimating PiLabile from the
!       soil type. 
!     Estimate PiLabile from Singh (1985) unless stated otherwise:

!     Need to add lookup in Detail.CDE table for these descriptions and
!       include this in messages to WARNING.OUT
!     From Soil Analysis Methods for P extraction
!     PMethod  Description
!     -------  -----------
!     SA001    Olsen                        
!     SA002    Bray No. 1                   
!     SA003    Bray No. 2                   
!     SA004    Mehlich I (double acid, 1:5) 
!     SA005    Anion exchange resin         
!     SA006    Truog                        
!     SA007    Mehlich I (double acid, 1:10)
!     SA008    Colwell                      
!     SA009    Water                        
!     SA010    IFDC Pi strip                
!     SA011    pH in Water                  
!     SA012    pH in KCl                    
!     SA013    Mehlich 3                    
!     SA014    Morgan's solution            
!     SA015    NH4OAc                       

!  *****************************************************************
!     Need to put limits on acceptable values of PMeasured. 
!     For now just limit to positive values. 
!  *****************************************************************

      DO L = 1, NLAYR
        NMSG = 0 
        IF (.NOT. UseHedley) THEN
          Method = PMethod
          PiLabile(L) = -99.
          WRITE(MSG(1),'("Soil layer: ",I2)') L
          WRITE(MSG(2),'("Soil type: ",A)') TRIM(SOILLAYERTYPE(L))
          WRITE(MSG(3),'("P extraction method: ",A)') PMETHOD
          NMSG = 3

          IF (PMeasured(L) < -1.E-5) THEN
!           No measured P -- model must stop
            MSG(3) = "Cannot run phosphorus model without" // 
     &            " some soil P measurement."
            CALL WARNING(3,ERRKEY,MSG)
           !CALL ErrorCode(CONTROL, 101, ERRKEY, YREND)
            CALL ERROR(ERRKEY,1,' ',0)
            RETURN
          ELSEIF (PMethod == 'SA005' .OR. PMethod == 'SA010') THEN
!           Labile P measured directly with resin extraction or Pi Strip
            PiLabile(L) = PMeasured(L) 
            WRITE(MSG(4),'("Measured P =",F8.2," ppm")') PMeasured(L)
            WRITE(MSG(5),'("Labile P   =",F8.2," ppm")') PiLabile(L)
            NMSG = 5
            GOTO 1000
          ENDIF  

!         Check valid method for pH
          IF (pH(L) >= 7.5 .OR. 
     &        SOILLAYERTYPE(L) == 'CALCAREOUS       ') THEN
            IF (Method /= 'SA001') THEN
              MSG(NMSG+1) = 
     &  "Olsen P extraction method is recommended for calcareous soils"
              MSG(NMSG+2) = "  or for pH values above 7.5."
              WRITE(MSG(NMSG+3),'(A,F7.2)') "  Reported pH = ", pH(L)
              MSG(NMSG+4) = "Please double check your data."
              NMSG=NMSG+4
              DO I = 1, NMSG
                WMSG(I) = MSG(I)
              ENDDO
              CALL WARNING(NMSG,ERRKEY,WMSG)
            ENDIF
          ELSEIF (pH(L) <= 4.5) THEN
            IF (Method == 'SA001') THEN
              MSG(NMSG+1) = 
     & "Olsen P extraction method is not valid for pH values below 4.5."
              WRITE(MSG(NMSG+2),'(A,F7.2)') "  Reported pH = ", pH(L)
              MSG(NMSG+3) = "Please double check your data."
              NMSG=NMSG+3
              DO I = 1, NMSG
                WMSG(I) = MSG(I)
              ENDDO
              CALL WARNING(NMSG,ERRKEY,WMSG)
            ENDIF
          ENDIF

          SELECT CASE (SOILLAYERTYPE(L))
!         -----------------------------------------------
!         Calcareous soils (number of samples = 22).
          CASE('CALCAREOUS       ')
            SELECT CASE(Method)
            CASE('SA001')   !Olsen
              PMin = 2.0
              IF (PMeasured(L) < PMin) THEN
!               Force the function to zero as PMeasured approaches 0.
                PiLabile(L) = (0.85 * PMin + 0.18) * PMeasured(L)/PMin
              ELSE
                PiLabile(L) = 0.85 * PMeasured(L) + 0.18
!               Sharpley et al. (1989) combined with IFDC/UH data -US.
!               PiLabile(L) = 0.96 * P_OLSEN(L) - 0.19
              ENDIF

            CASE('SA002')   !Bray No. 1                   
              PMin = 2.0
              IF (PMeasured(L) < PMin) THEN
!               Force the function to zero as PMeasured approaches 0.
                PiLabile(L) = (1.81 * PMin + 1.88) * PMeasured(L)/PMin
              ELSE
                PiLabile(L) = 1.81 * PMeasured(L) + 1.88
!               Sharpley et al. (1984), Table 2.
!               PiLabile(L) = 0.55 * P_BRAY1(L) + 6.1
              ENDIF

            CASE('SA009')   !Water                   
              PMin = 0.1
              IF (PMeasured(L) < PMin) THEN
!               Force the function to zero as PMeasured approaches 0.
                PiLabile(L) = (5.92 * PMin + 0.09) * PMeasured(L)/PMin
              ELSE
                PiLabile(L) = 5.92 * PMeasured(L) + 0.09   
              ENDIF

            END SELECT  !P extraction methods for Calcareous soils

!         -----------------------------------------------
!         Slightly weathered soils (CEC/%CLAY .GT. 16)
!         (number of samples = 120)
          CASE('SLIGHTLYWEATHERED')
!           For slightly weathered soils, have option to estimate PiLabile
!             using both measured P and exchangeable K, if available.
            SELECT CASE(Method)

            CASE('SA001')   !Olsen
             PMin = 2.0
             IF (PMeasured(L) < PMin) THEN
!                Force the function to zero as PMeasured approaches 0.
                 PiLabile(L) = (0.76 * PMin + 0.53) * PMeasured(L)/PMin
              ELSE
                 PiLabile(L) = 0.76 * PMeasured(L) + 0.53
!                Sharpley et al. (1984), Table 2.
!                PiLabile(L) = 1.07 * P_OLSEN(L) + 4.1
             ENDIF
             IF (EXK(L) > 1.E-6) THEN
                 PiLabile(L) = AMIN1(PiLabile(L), 
     &               0.62 * PMeasured(L) + 10.09* EXK(L) + 2.62)
        
             ENDIF

            CASE('SA002')   !Bray No. 1                  
            PMin = 2.0
!           PMin = 4.0   !US Nov08
              IF (PMeasured(L) < PMin) THEN
!                 Force the function to zero as PMeasured approaches 0.
                  PiLabile(L) = (1.37 * PMin + 1.55) * PMeasured(L)/PMin
!                 PiLabile(L) = (1.37 * PMin + 6.77) * PMeasured(L)/PMin
              ELSE
                  PiLabile(L) = 1.37 * PMeasured(L) + 1.55   !US Nov08
!                 PiLabile(L) = 1.37 * PMeasured(L) + 6.77
!                 Sharpley et al. (1984), Table 2.
!                 PiLabile(L) = 0.56 * P_BRAY1(L) + 5.1
              ENDIF
              IF (EXK(L) > 1.E-6) THEN
                PiLabile(L) = AMIN1(PiLabile(L), 
     &                    1.09 * PMeasured(L) + 10.59 * EXK(L) +2.71)
              ENDIF

            CASE('SA004')   !Mehlich I (double acid, 1:5)
              PMin = 10.0
              IF (PMeasured(L) < PMin) THEN
!                 Force the function to zero as PMeasured approaches 0.
                  PiLabile(L) = (0.13 * PMin + 11.4) * PMeasured(L)/PMin
              ELSE
!                 PiLabile(L) = 2.71 * PMeasured(L) + 5.82
!                 3/25/08 US Go back to Sharpley -- this method likely to be
!                     used for temperate soils.
!                 Sharpley et al. (1984), Table 2.
                  PiLabile(L) = 0.13 * PMeasured(L) + 11.4
              ENDIF
              IF (EXK(L) > 1.E-6) THEN
                  PiLabile(L) = AMIN1(PiLabile(L), 
     &                     2.16 * PMeasured(L) + 9.58 * EXK(L) +2.42)
              ENDIF

            CASE('SA006')   !Truog
              
              PMin = 2.0
              IF (PMeasured(L) < PMin) THEN
!                 Force the function to zero as PMeasured approaches 0.
                  PiLabile(L) = (0.34 * PMin + 1.35) * PMeasured(L)/PMin
              ELSE
                  PiLabile(L) = 0.34 * PMeasured(L) + 1.35
              ENDIF
              IF (EXK(L) > 1.E-6) THEN
                PiLabile(L) = AMIN1(PiLabile(L), 
     &                0.30 * PMeasured(L) + 5.85 * EXK(L) +1.48)
              ENDIF

            CASE('SA014')   !Morgan's solution                  
              PMin = 0.1
              IF (PMeasured(L) < PMin) THEN
!               Force the function to zero as PMeasured approaches 0.
                PiLabile(L) = (187.3 * PMin + 11.87)*PMeasured(L)/PMin
              ELSE
                PiLabile(L) = 187.3 * PMeasured(L) + 11.87
              ENDIF

            END SELECT  !P ext methods for slightly weathered soils

            IF (EXK(L) >= 1.E-6) THEN
              WRITE(MSG(NMSG+1),'("Exch K     =",F8.2," ppm")') EXK(L)
              NMSG = NMSG+1
            ENDIF

!         -----------------------------------------------
!         Highly weathered soils (number of samples = 70).
          CASE('HIGHLYWEATHERED  ')
            SELECT CASE(Method)

            CASE('SA001')   !Olsen
              PiLabile(L) = MAX(0.0, 1.50 * PMeasured(L) - 1.39)
!             Sharpley et al. (1984), Table 2.
!             PiLabile(L) = 0.55 * P_OLSEN(L) + 2.1

            CASE('SA002')   !Bray No. 1  
              PMin = 1.0
              IF (PMeasured(L) > 6.29) THEN  
!               US Table 3. IFDC (2008)             
                PiLabile(L) = MAX(0.0, 1.34 * PMeasured(L) - 0.30)
              ELSEIF (PMeasured(L) > PMin) THEN
!               Sharpley et al. (1989) 
                PiLabile(L) = 0.41 * PMeasured(L) + 5.55
              ELSE
!               Extension thru origin 
                PiLabile(L) = (0.41 * PMin + 5.55) * PMeasured(L) / PMin
              ENDIF

            CASE('SA004')   !Mehlich I (double acid, 1:5)
!             PiLabile(L) = MAX(0.0, 5.97 * PMeasured(L) - 0.21)
              PMin = 10.0   ! need to lower to 5.0 
              IF (PMeasured(L) < PMin) THEN
                PiLabile(L)=(0.24* PMeasured(L) + 2.9)*PMeasured(L)/PMin
              ELSE
!               Sharpley et al. (1984), Table 2.
                PiLabile(L) = 0.24 * PMeasured(L) + 2.9
              ENDIF

            CASE('SA006')   !Truog
              PiLabile(L) = 1.07 * PMeasured(L) - 1.49
!             Sharpley et al. (1989), Table 3.
!             PiLabile(L) = 0.20 * P_TRUOG(L) + 5.62

            CASE('SA007')   !Mehlich I (double acid, 1:10)
              PMin = 2.0
              IF (PMeasured(L) < PMin) THEN
                PiLabile(L) = (0.64 * PMin + 5.72) * PMeasured(L)/PMin
              ELSE
!               Sharpley et al. (1989), Table 3.
                PiLabile(L) = 0.64 * PMeasured(L) + 5.72
!               PiLabile(L) = 0.64 * P_MEHLICH(L) + 5.72
              ENDIF

            CASE('SA008')   !Colwell                 
              PMin = 2.0
              IF (PMeasured(L) < PMin) THEN
                PiLabile(L) = (0.43 * PMin + 4.21) * PMeasured(L)/PMin
              ELSE
!               Sharpley et al. (1989), Table 3.
                PiLabile(L) = 0.43 * PMeasured(L) + 4.21
!               PiLabile(L) = 0.43 * P_COLWELL(L) + 4.21
              ENDIF

            END SELECT  !P ext methods for highly weathered soils

!         -----------------------------------------------
!         ANDISOLS - Volcanic ash soils (number of samples = 21).
          CASE('ANDISOL          ')
            SELECT CASE(Method)

            CASE('SA001')   !Olsen
              PiLabile(L) = MAX(0.01, 1.41 * PMeasured(L) - 2.56)

            CASE('SA002')   !Bray No. 1                   
              PiLabile(L) = MAX(0.01, 1.88 * PMeasured(L) - 2.11)

            CASE('SA004')   !Mehlich I (double acid, 1:5)
              PMin = 4.0
              IF (PMeasured(L) < PMin) THEN
                PiLabile(L) = (4.52 * PMin + 6.67) * PMeasured(L)/PMin
              ELSE
                PiLabile(L) = 4.52 * PMeasured(L) + 6.67
              ENDIF

            CASE('SA006')   !Truog
              PiLabile(L) = MAX(0.0, 0.27 * PMeasured(L) - 0.73)

            END SELECT  !P extraction method for andisols

!         -----------------------------------------------
!         Separate relationship for top vs subsoils is used when 
!           soil type not known
          CASE DEFAULT    !Default soil type or none specified
            IF (L < 3) THEN  !(TOP SOIL LAYER)
              SELECT CASE(Method)

              CASE('SA001')   !Olsen
                PMin = 5.0
                IF (PMeasured(L) < PMin) THEN
                  PiLabile(L) = (0.74 * PMin + 1.39) *PMeasured(L)/PMin
                ELSE
                  PiLabile(L) = 0.74 * PMeasured(L) + 1.39
                ENDIF

              CASE('SA002')   !Bray No. 1                   
                PMin = 5.0
                IF (PMeasured(L) < PMin) THEN
                  PiLabile(L) = (1.35 * PMin + 1.02) *PMeasured(L)/PMin
                ELSE
                  PiLabile(L) = 1.35 * PMeasured(L) + 1.02
                ENDIF

!              CASE('SA004')   !Mehlich I (double acid, 1:5)
!                PMin = 0.5
!                IF (PMeasured(L) < PMin) THEN
!                  PiLabile(L) = (2.65 * PMin + 9.39) * PMeasured(L)/PMin
!                ELSE
!                  PiLabile(L) = 2.65 * PMeasured(L) + 9.39
!                ENDIF

              CASE('SA006')   !Truog
                PMin = 4.0
                IF (PMeasured(L) < PMin) THEN
                  PiLabile(L) = (0.28 * PMin + 0.62) * PMeasured(L)/PMin
                ELSE
                  PiLabile(L) = 0.28 * PMeasured(L) + 0.62
                ENDIF

              END SELECT  !P extraction method

            ELSE  ! (SUBSOIL)
              SELECT CASE(Method)
              CASE('SA001')   !Olsen
                PMin = 0.5
                IF (PMeasured(L) < PMin) THEN
                  PiLabile(L) = (0.78 * PMin + 1.11) *PMeasured(L)/PMin
                ELSE
                  PiLabile(L) = 0.78 * PMeasured(L) + 1.11
                ENDIF

              CASE('SA002')   !Bray No. 1                   
                PMin = 0.5
                IF (PMeasured(L) < PMin) THEN
                  PiLabile(L) = (1.66 * PMin + 0.73) *PMeasured(L)/PMin
                ELSE
                  PiLabile(L) = 1.66 * PMeasured(L) + 0.73
                ENDIF

!              CASE('SA004')   !Mehlich I (double acid, 1:5)
!                PMin = 0.05
!                IF (PMeasured(L) < PMin) THEN
!                  PiLabile(L) = (4.66 * PMin + 1.64) *PMeasured(L)/PMin
!                ELSE
!                  PiLabile(L) = 4.66 * PMeasured(L) + 1.64
!                ENDIF

              CASE('SA006')   !Truog
                PMin = 2.0
                IF (PMeasured(L) < PMin) THEN
                  PiLabile(L) = (0.16 * PMin + 0.38) *PMeasured(L)/PMin
                ELSE
                  PiLabile(L) = 0.16 * PMeasured(L) + 0.38
                ENDIF

              END SELECT  !P extraction method for unknown soil type
            ENDIF       !Surface or subsoil
          END SELECT    !Soil type

!         Check for uninitialized data:
          IF (PiLabile(L) > -1.E-6) THEN
           WRITE(MSG(NMSG+1),'("Measured P =",F8.2," ppm")')PMeasured(L)
           WRITE(MSG(NMSG+2),'("Labile P   =",F8.2," ppm")') PiLabile(L)
           NMSG = NMSG + 2
          ELSE
           MSG(NMSG+1) ="Initialization of soil P not possible with " //  
     &                 "data provided."
           MSG(NMSG+2) ="Program will stop."
           CALL WARNING(NMSG+2, ERRKEY, MSG)
           CALL ERROR(ERRKEY,1,' ',0)
          ENDIF
        ELSE    !Hedley data will be used
          MSG(1) = 
     &      "Hedley fractionation data used for inorganic P estimation."
          NMSG = 1
        ENDIF   !Soil Analysis section (UseHedley = .false.)

 1000   CONTINUE !with analysis based on Hedley or TOTP data

!       ----------------------------------------------------------------
!       Set the P availability index according to Sharpley et al. (1984),
!       Table 4. P_AVAILINDEX (fraction) uses TOTBAS (%), CaCO3 (%), CLAY (%),
!       pH, PiLabile (mg/kg).

!       Default value, unknown soil type:
        IF (PiLabile(L) < 0.2) THEN
          P_AVAILINDEX(L) = 0.9
        ELSEIF (PiLabile(L) < 1.5) THEN
!         Singh, 1985; Montage & Zapata, 2002 (IAEA), IFDC (2008)
          P_AVAILINDEX(L) = AMIN1(0.75 + 0.00023**PiLabile(L),0.9)  
        ELSE
!         Should never get here!
          P_AVAILINDEX(L) = 0.75
        ENDIF

        SELECT CASE (SOILLAYERTYPE(L))

!         Calcareous soils.
          CASE('CALCAREOUS       ')
!           Sharpley et al (1989) Table 4.
            IF (CaCO3(L) > -1.E-6) THEN
              P_AVAILINDEX(L) = -0.0058 * CaCO3(L) + 0.60
            ENDIF

!         Slightly weathered soils.
          CASE('SLIGHTLYWEATHERED')
!           Sharpley et al (1984) Table 4. 
            IF (TOTBAS(L) > -1.E-6 .AND. PH(L) > 0. .AND. 
     &            PiLabile(L) > 1.E-3) THEN
              P_AVAILINDEX(L) = AMIN1(0.90, 0.0043 * TOTBAS(L) 
     &           + 0.0034 * PiLabile(L) + 0.11 * PH(L) - 0.70)
            ENDIF

!         Highly weathered soils.
          CASE('HIGHLYWEATHERED  ')
!           Sharpley et al (1989) Table 4.
            IF (CLAY(L) > 1.E-6) THEN   !Protection from LOG(0).
              P_AVAILINDEX(L) = -0.30 * ALOG10(CLAY(L)) + 0.68 
            ENDIF

        END SELECT  !soil layer type

        P_AVAILINDEX(L) = MIN(P_AVAILINDEX(L), 0.99)
        P_AVAILINDEX(L) = MAX(P_AVAILINDEX(L), 0.1)

!       ----------------------------------------------------------------
!       P rate coefficients.  
        K_LAB2ACT(L) = (((1. - P_AVAILINDEX(L)) / 
     &                      P_AVAILINDEX(L)) ** 0.5) * 0.03
        K_ACT2LAB(L) = K_LAB2ACT(L) / 3. * P_AVAILINDEX(L)
        K_ACT2STA(L) = EXP(-1.77 * P_AVAILINDEX(L) - 7.05)
        K_STA2ACT(L) = 0.0001 !(independent of layer)

!       ----------------------------------------------------------------
!       Active and stable P 
        IF (.NOT. UseHedley) THEN
          PFunc = (1. - P_AvailIndex(L)) / P_AvailIndex(L)

!         If active Pi was not calculated from the Hedley fractions, 
!         estimate it now from the labile Pi and P_AvailIndex according
!         to Jones et al. (1984), eq. 2.
          PiActive(L) = PiLABILE(L) * PFunc

!         If PiActive and PiLabile are in equilibrium:
          PiActive(L) = PiLABILE(L) * K_LAB2ACT(L) / K_ACT2LAB(L)
!
!         According to Jones et al. (1984, above eq. 6) the stable Pi 
!         pool is four times as large as the active pool.
          PiStable(L) = 4. * PiActive(L)
        ENDIF       !Not UseHedley

        PiTotal(L) = PiLabile(L) + PiActive(L) + PiStable(L)    
        WRITE(MSG(NMSG+1),'(A,F8.2,A)')"Active P   =",PiActive(L)," ppm"
        WRITE(MSG(NMSG+2),'(A,F8.2,A)')"Stable P   =",PiStable(L)," ppm"
        WRITE(MSG(NMSG+3),'("P Avail Index   =",F8.2)')P_AvailIndex(L)
        WRITE(MSG(NMSG+4),'("Rate PLab->PAct =",F8.5,"/d")')K_LAB2ACT(L)
        WRITE(MSG(NMSG+5),'("Rate PAct->PLab =",F8.5,"/d")')K_ACT2LAB(L)
        WRITE(MSG(NMSG+6),'("Rate PAct->PSta =",F8.5,"/d")')K_ACT2STA(L)
        WRITE(MSG(NMSG+7),'("Rate PSta->PAct =",F8.5,"/d")')K_STA2ACT(L)
        NMSG = NMSG + 7
        CALL INFO(NMSG, ERRKEY, MSG)

      ENDDO         !Soil layer loop

!-----------------------------------------------------------------------

      RETURN
      END SUBROUTINE SoilPi_init

!=======================================================================
