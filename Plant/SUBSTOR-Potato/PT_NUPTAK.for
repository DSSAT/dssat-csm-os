C=======================================================================
C  PT_NUPTAK, Subroutine
C
C  Determines N uptake
C-----------------------------------------------------------------------
C  Revision history
C
C  06/  /1994     Written
C  02/08/1993 WTB Modified 
C  02/08/1993 PWW Header revision and minor changes
C  12/  /1994 WTB Adapted for SUBSTOR model
C  08/28/2001 CHP Modified for modular format.
!  11/07/2005 CHP Replaced FAC with SOILPROP variable KG2PPM
!  02/25/2018 MZ  Adapted for 2D
!  03/27-2024 CHP 2D integration into 1D code
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  NUF    : Plant N supply/demand ratio used to modify uptake
C  NDEM   : Plant nitrogen demand (g/plant)
C  L,L1   : Loop counters
C  J      : Loop counters for 2D
C  THUMN  :
C  RNH4U  : Potential ammonium uptake from Layer L (kg N/ha)
C  RNO3U  : Potential nitrate uptake from Layer L (kg N/ha)
C  ANO3   : Total extractable nitrate N in soil profile (kg N/ha)
C  ANH4   : Total extractable ammonium N in soil profile (kg N/ha)
C  DNG    : N demand of potential new growth of tops (g N/plant)
C  TNDEM  : Plant tops demand for nitrogen (g N/plant)
C  RNDEM  : Plant root demand for nitrogen (g/plant)
C  ANDEM  : Crop N demand (kg N/ha)
C  DROOTN : Daily change in plant root nitrogen content (g N/plant)
C  DSTOVN :
C  FNH4   : Unitless soil ammonium supply index
C  FNO3   : Unitless soil nitrate supply index
C  SMDFR  : Soil moisture deficit factor affecting N uptake
C  RFAC   : Interim variable describing the effects of root length density
C           on potential N uptake from a layer
C  UNO3   : Plant uptake of nitrate from a layer (kg N/ha)
C  UNH4   : Plant uptake of ammonium from a layer (kg N/ha)
C  NH4_2D(L,J)   : Ammonium N in soil cell (µg[N] / g[soil])
C  NO3_2D(L,J)   : Nitrate in soil cell (µg[N] / g[soil])
C  RLV_2D(L,J)   : Root length density for soil cell ((cm root / cm3 soil))
C  RNH4U_2D(L,J) : Ammonium uptake (kg N/ha)
C  RNO3U_2D(L,J) : Nitrate uptake (kg N/ha)
C  UNH4_2D(L,J)  : Uptake of NH4 from cell
C  UNO3_2D(L,J)  : Uptake of NO3 from cell
C  XMIN   :
C  XNDEM  :
C  FACTOR : Relative weighting to distribute crop root residues at the beginning
C           of a simulation
C=======================================================================

      SUBROUTINE PT_NUPTAK (CONTROL, CELLS,
     &    DUL, KG2PPM, LL, NLAYR, SAT,                    !Input
     &    GRORT, GROTUB, ISTAGE,                          !Input
     &    PLTPOP, RCNP, RTWT, TCNP, TMNC,                 !Input
     &    TOPWT, TUBCNP, TUBWT,                           !Input
     &    GROTOP, ROOTN, TOPSN, TUBANC,                   !I/O
     &    ARVCHO, RANC, TANC, TRNU, TUBN, UNH4, UNO3,     !Output
     &    WTNUP)                                          !Output

!-----------------------------------------------------------------------
      USE Cells_2D
      USE ModuleDefs
      IMPLICIT  NONE
      SAVE

!     Subroutine interface variables
      Type (ControlType), INTENT(IN) :: CONTROL
      Type (CellType), INTENT(INOUT) :: Cells(MaxRows,MaxCols)

      REAL, INTENT(IN) :: PLTPOP, RCNP, RTWT, 
     &  TCNP, TMNC, TOPWT, TUBCNP, TUBWT
      REAL, DIMENSION(NL), INTENT(IN) :: DUL, KG2PPM, LL, SAT

      INTEGER, INTENT(IN) :: ISTAGE, NLAYR
      REAL, INTENT(INOUT) :: GRORT, GROTOP, GROTUB, ROOTN, TOPSN, TUBANC

      REAL, DIMENSION(NL), INTENT(OUT) :: UNO3(NL), UNH4(NL)
      REAL, INTENT(OUT) :: ARVCHO, RANC, TANC, TRNU, TUBN, WTNUP
      REAL, DIMENSION(MaxRows, MaxCols) :: ColFrac
      INTEGER DYNAMIC, L

      REAL ANDEM, AVAILN, EXTRAN, FACTOR 
      REAL FNH4, FNO3, GRFN
      REAL NDEM, NPART, NUF, PGROW 
      REAL RATIO, RNDEM   !, RFAC
      REAL SMDFR, STOPSN, TNDEM
      REAL TOPNUSD, TUBDEM, TUBMNC, TUBSINK, TUBSN
      REAL XMIN, XNDEM
      REAL SurfaceVal
      REAL TRNO3U, TRNH4U

      REAL, DIMENSION(NL) :: ESW
      REAL MXNH4U, MXNO3U

      INTEGER J
      INTEGER, DIMENSION(MaxRows,MaxCols) :: Cell_type
      REAL HalfRow, BEDWD
      REAL, DIMENSION(MaxRows,MaxCols) :: NO3_2D, NH4_2D, RLV_2D, 
     &    RTLEN_2D, RNH4U_2D, RNO3U_2D, CellArea
      REAL, DIMENSION(MaxRows,MaxCols) :: SNO3_2D, SNH4_2D, SWV
      REAL, DIMENSION(MaxRows,MaxCols) :: UNO3_2D, UNH4_2D
      Real FieldFac, ROWSPC_cm

!     debug CHP
      REAL DayLostN, CumLostN

      DYNAMIC = CONTROL % DYNAMIC

      SWV    = CELLS % State % SWV
      RLV_2D = CELLS % State % RLV
      SNO3_2D = CELLS % State % SNO3
      SNH4_2D = CELLS % State % SNH4
      Cell_type = CELLS % Struc % Cell_type

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      TUBMNC  = 0.007
      TUBSINK = 0.0   !from PHASEI
      WTNUP   = 0.0   !Seasonal total N uptake (kg[N]/ha)
      TRNO3U = 0.0 
      TRNH4U = 0.0 
      TRNU   = 0.0 
      UNH4   = 0.0
      UNO3   = 0.0
      UNH4_2D = 0.0
      UNO3_2D = 0.0
      RLV_2D  = 0.0
      CumLostN = 0.0

      CELLS % RATE % NH4Uptake = UNH4_2D    !kg[N]/ha
      CELLS % RATE % NO3Uptake = UNO3_2D    !kg[N]/ha

      HalfRow = BedDimension % ROWSPC_cm / 2
      BEDWD   = BedDimension % BEDWD
      ColFrac = BedDimension % ColFrac
      ROWSPC_cm = BedDimension % ROWSPC_cm
      CellArea = CELLS % STRUC % CellArea

      IF (CONTROL % SIM2D) THEN
        FieldFac = 2.0
      ELSE
        FieldFac = 1.0
      ENDIF

!***********************************************************************
!***********************************************************************
!     Daily rate calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
C   Initialize variables
C-----------------------------------------------------------------------
      ARVCHO = 0.0
      NUF    = 0.0
      TRNU   = 0.0
!     RNO3U    = 0.0
!     RNH4U    = 0.0
      RNO3U_2D = 0.0
      RNH4U_2D = 0.0
      UNH4_2D  = 0.0
      UNO3_2D  = 0.0
      UNH4     = 0.0
      UNO3     = 0.0
      TRNO3U   = 0.0
      TRNH4U   = 0.0

      DO L = 1, NRowsTot
        DO J = 1, NColsTot
!         Concentration
          SELECT CASE(Cell_type(L,J))
          CASE(3,4,5)
             NO3_2D(L,J) = SNO3_2D(L,J) * KG2PPM(L) / ColFrac(L,J)
     &                     * FieldFac
             NH4_2D(L,J) = SNH4_2D(L,J) * KG2PPM(L) / ColFrac(L,J)
     &                     * FieldFac
          END SELECT
        ENDDO
      ENDDO

C-----------------------------------------------------------------------
C   Calculate potential N supply in soil layers with roots (TRNU)
C-----------------------------------------------------------------------
      DO L = 1, NRowsTot 
        DO J = 1, NColsTot
          SELECT CASE(Cell_type(L,J))
          CASE(3,4,5)
            IF (RLV_2D(L,J) .GT. 1.E-6) THEN
              IF (NH4_2D(L,J) > 1.E-6) THEN
                FNH4 = 1.0 - EXP(-0.0250*NH4_2D(L,J))
              ELSE
                FNH4 = 0.0
              ENDIF

              IF (NO3_2D(L,J) > 1.E-6) THEN
                FNO3 = 1.0 - EXP(-0.0275*NO3_2D(L,J))
              ELSE
                FNO3 = 0.0
              ENDIF
!             CHP 2024-10-23 Not sure why the lower limit of 0.03, but it has a big 
!               impact on results, so keep it as in the 1D model.
              IF (FNO3 .LT. 0.03) FNO3 = 0.0  
              IF (FNO3 .GT. 1.0)  FNO3 = 1.0
              IF (FNH4 .LT. 0.03) FNH4 = 0.0  
              IF (FNH4 .GT. 1.0)  FNH4 = 1.0

!             SMDFR = relative drought factor
              ESW(L) = DUL(L) - LL(L)
              SMDFR = (SWV(L,J) - LL(L)) / ESW(L)
              SMDFR = AMAX1 (SMDFR, 0.0)

              IF (SMDFR .GT. 1.0) THEN
                 SMDFR = (SAT(L) - SWV(L, J)) / (SAT(L) - DUL(L))
              ENDIF

!             RFAC = RLV_2D(L, J) * SMDFR * SMDFR * DLAYR (L) * 100.0
!              RNO3U_2D(L,J) = RFAC * FNO3 * 0.006
!              RNH4U_2D(L,J) = RFAC * FNH4 * 0.006

!             Convert RLV to root length per area
              RTLEN_2D(L,J) = RLV_2D(L,J) * CellArea(L,J) / RowSpc_cm
              FACTOR = RTLEN_2D(L,J) * 0.006 * (SMDFR ** 2.0) * 100.
!           kg[N]          cm[root]     mg[N]     100 kg/ha
!           -----    =     --------- * -------- * ---------
!             ha           cm2[soil]   cm[root]     mg/cm2

              RNO3U_2D(L,J) = MAX(0.0, FACTOR * FNO3)
              RNH4U_2D(L,J) = MAX(0.0, FACTOR * FNH4)

!             kg[N]/ha
              TRNU = TRNU + (RNO3U_2D(L,J) + RNH4U_2D(L,J)) * FieldFac
            ENDIF
          END SELECT
        ENDDO
      ENDDO

C-----------------------------------------------------------------------
C   Calculate N demand
C-----------------------------------------------------------------------

      AVAILN = TRNU/(PLTPOP * 10.0)                   !g[N]/plant
      PGROW  = GROTOP + GRORT + GROTUB                !Potential growth
      !
      ! Update N concentration in each growth component
      ! Initial calculation of N demand, based on calculated growth
      ! For each component, N demand is reduced if _ANC > _CNP
      ! New growth GRO___ is added at critical concentration
      !
      TANC = TOPSN/TOPWT                     ! Define TANC, TNDEM:

      IF (TANC .GE. TCNP) THEN               ! All N demand function
        TNDEM = GROTOP*TCNP                 ! depend on actual vs.
      ELSE                                  ! critical N conc.
        TNDEM = TOPWT*(TCNP - TANC) + GROTOP*TCNP
      END IF

      TNDEM = AMAX1 (TNDEM, 0.0)

      IF (RTWT .GT. 0.0) THEN
        RANC = ROOTN/RTWT                    ! Define RANC, RNDEM
      ELSE
        RANC = 0.
      ENDIF

      IF (RANC .GE. RCNP) THEN
        RNDEM = GRORT*RCNP
      ELSE
        RNDEM = RTWT*(RCNP - RANC) + GRORT*RCNP
      END IF

      RNDEM = AMAX1 (RNDEM, 0.0)

      IF (TUBWT .GT. 0) THEN                 ! Define TUBANC, TUBDEM
        TUBANC = TUBN/TUBWT
        IF (TUBANC .GE. TUBCNP) THEN
          TUBDEM = GROTUB*TUBCNP
        ELSE
          TUBDEM = TUBWT*(TUBCNP - TUBANC) + GROTUB*TUBCNP
        END IF
      ELSE
        TUBDEM = 0.0
      END IF

      TUBDEM = AMAX1 (TUBDEM, 0.0)

      SELECT CASE (ISTAGE)
      CASE (1)                               ! Vegetative development

        NDEM = TNDEM + RNDEM                 !g[N]/plant

        IF (AVAILN .GE. NDEM) THEN           ! N sufficient for all
          IF (NDEM .NE. 0.0) THEN            ! N demands.
            RATIO = AMIN1 (AVAILN/NDEM, 1.3) ! EXTRAN partitioned
          ELSE                               ! on demand RATIO
            RATIO = 0.0
          END IF        
          TNDEM = TNDEM * RATIO              ! Luxury uptake of N
        ELSE
          TNDEM = GROTOP* TCNP               ! Re-define demand as only
          RNDEM = GRORT * RCNP               ! new growth at critical N
          NDEM  = TNDEM + RNDEM

          IF (AVAILN .GE. TNDEM+RNDEM) THEN
            IF (TNDEM + RNDEM .GT. 0.0) THEN
              TNDEM = AVAILN * TNDEM/NDEM
              RNDEM = AVAILN - TNDEM
            ELSE
              TNDEM = 0.0
              RNDEM = 0.0
            ENDIF
          ELSE
!              
!           STOPSN allows for dilution of haulm N
            STOPSN = AMIN1 (0.05*TOPWT*(TANC - TMNC), 
     +                 (TNDEM+RNDEM)-AVAILN)
            IF ((AVAILN + STOPSN) .GE. (TNDEM + RNDEM)) THEN
              TNDEM = AVAILN * TNDEM / NDEM
              RNDEM = AVAILN - TNDEM
            ELSE
              IF (TNDEM+RNDEM .NE. 0.0) THEN
                GRFN = (AVAILN + STOPSN)/(TNDEM + RNDEM)
              ELSE
                GRFN = 0.0
              END IF

              ARVCHO = (GROTOP + GRORT) - (GROTOP + GRORT) * GRFN
              GROTOP =  GROTOP * GRFN     ! Growth reduced due to
              GRORT  =  GRORT  * GRFN     ! insufficient N
              GRFN   =  AVAILN /(TNDEM + RNDEM)
              TNDEM  =  TNDEM  * GRFN
              RNDEM  =  RNDEM  * GRFN
            ENDIF
          ENDIF
        ENDIF

        NDEM   = TNDEM + RNDEM

      CASE (2)                             ! PARTTN, with Tubers

        NDEM    = TUBDEM + TNDEM + RNDEM
        TUBSINK = TUBWT / (TOPWT + RTWT)
        TOPNUSD = 0.0

        IF (AVAILN .GE. NDEM) THEN           ! AVAILN > NDEM
                                             ! All demands met
          IF (NDEM .GT. 0.0) THEN           ! Luxury uptake.
            RATIO = AMIN1 (AVAILN/NDEM, 1.25)
            TNDEM = TNDEM * RATIO

!           Accumulate less N in the tubers under excess N conditions.
!           Don't allow any excess N to accumulate in the roots unless 
!           a provision is added to re-allocate excess N in the roots 
!           to other plant parts.
            TUBDEM = TUBDEM * (1.0 + (RATIO - 1.0) * 0.5)
          ELSE
            TNDEM  = 0.0
            TUBDEM = 0.0
            RNDEM  = 0.0
          END IF

        ELSE IF (AVAILN .LT. NDEM) THEN     ! AVAILN < NDEM:

          NPART  = AMIN1 ((0.5 + 0.5*TUBSINK),1.0)
          ARVCHO = GROTUB - GROTUB*NPART
          GROTUB = GROTUB * NPART

          TUBDEM = GROTUB * TUBCNP          ! N demand only new growth
          TNDEM  = GROTOP * TCNP            ! at critical N conc.
          RNDEM  = GRORT  * RCNP
          NDEM   = TUBDEM + TNDEM + RNDEM

          IF (AVAILN .GE. NDEM) THEN
            IF (NDEM .GT. 0.) THEN
              RATIO  = AMIN1 (AVAILN/NDEM,1.25)
              TNDEM  = TNDEM*RATIO
              TUBDEM = TUBDEM * (1.0 + (RATIO - 1.0)*0.5)
            ELSE
              TNDEM  = 0.0
              TUBDEM = 0.0
              RNDEM  = 0.0
            END IF

          ELSEIF (AVAILN .GE. TUBDEM) THEN

            EXTRAN = AVAILN - TUBDEM       ! Fill tuber demand 1st,
            STOPSN = AMIN1(0.05*TOPWT*(TANC-TMNC),TNDEM+RNDEM-EXTRAN)
            IF (EXTRAN+STOPSN .GE. TNDEM+RNDEM) THEN
              TNDEM = EXTRAN * TNDEM/(TNDEM+RNDEM)
              RNDEM = EXTRAN - TNDEM
            ELSE
              IF (TNDEM+RNDEM .GT. 0.0) THEN
                GRFN = (EXTRAN + STOPSN)/(TNDEM + RNDEM)
              ELSE
                GRFN = 0.0
              END IF

              ARVCHO = ARVCHO + (GROTOP+GRORT)-(GROTOP+GRORT)*GRFN
              GROTOP = GROTOP * GRFN
              GRORT  = GRORT  * GRFN
              GRFN   = EXTRAN /(TNDEM + RNDEM)
              TNDEM  = TNDEM  * GRFN
              RNDEM  = RNDEM  * GRFN
            END IF

          ELSE IF (AVAILN .LT. TUBDEM) THEN ! Use N from haulm

            ARVCHO = ARVCHO + GROTOP + GRORT
            TNDEM  = 0.0                    ! Only tubers grow
            RNDEM  = 0.0
            GROTOP = 0.0
            GRORT  = 0.0

            STOPSN = TOPWT *0.05*(TANC - TMNC)
            IF (AVAILN + STOPSN .LT. TUBDEM) THEN
              TOPNUSD = STOPSN
              TOPSN   = TOPSN  - STOPSN
              AVAILN  = AVAILN + STOPSN
              TUBSN   = TUBWT*0.10*(TUBANC - TUBMNC)
              IF (AVAILN+TUBSN .LT. TUBDEM) THEN
                GRFN   = (AVAILN+TUBSN)/TUBDEM
                ARVCHO = ARVCHO + GROTUB - GROTUB*GRFN
                GROTUB = GROTUB * GRFN
              END IF
                 TUBDEM  = AVAILN
              ELSE
              TOPNUSD = TUBDEM - AVAILN
              TOPSN   = TOPSN - (TUBDEM - AVAILN)
            ENDIF
          ENDIF
        ENDIF
        NDEM = TUBDEM + TNDEM + RNDEM - TOPNUSD  ! Total N demand, rev.
      END SELECT

C-----------------------------------------------------------------------
C   Convert total N demand from g N/plt to kg N/ha (ANDEM)
C-----------------------------------------------------------------------
      !Convert ANDEM, AVAILN to area basis
      ANDEM  = NDEM   * PLTPOP * 10.0    !kg[N]/ha
    !      g[N]/plant * plt/m2 * (kg/ha)/(g/m2)
      AVAILN = AVAILN * PLTPOP * 10.0
C     GRFN   = (GROTOP + GRORT + GROTUB)/PGROW ! Indicator of N stress

C-----------------------------------------------------------------------
C   Calculate factor (NUF) to reduce N uptake to level of demand
C-----------------------------------------------------------------------
      IF (ANDEM .LE. 0.0) THEN
         TRNU  = 0.0
         NUF   = 0.0
       ELSE
         ANDEM = AMIN1 (ANDEM,TRNU)    !kg[N]/ha
         IF (TRNU .EQ. 0.0) RETURN
         NUF   = ANDEM/TRNU
         TRNU  = 0.0
      ENDIF

C-----------------------------------------------------------------------
C   Calculate N uptake in soil layers with roots based on demand (kg/ha)
!   Scale from field scale to cell using ColFrac after checking XMIN
C-----------------------------------------------------------------------
      DO L = 1, NRowsTot
        DO J = 1, NColsTot
          SELECT CASE(Cell_type(L,J))
          CASE(3,4,5)
            IF (RLV_2D(L,J) .GT. 0.0) THEN
!             Proportion by demand : supply ratio
              UNO3_2D(L,J) = RNO3U_2D(L,J) * NUF
              UNH4_2D(L,J) = RNH4U_2D(L,J) * NUF

!             XMIN = minimum amount NO3 left after uptake (kg[N]/ha)
              XMIN    = 0.25 / KG2PPM(L) * ColFrac(L,J) / FieldFac
              MXNO3U  = MAX(0.0,(SNO3_2D(L,J) - XMIN))
              IF (UNO3_2D(L,J) .GT. MXNO3U) THEN
                DayLostN = DayLostN + (UNO3_2D(L,J) - MXNO3U)
                UNO3_2D(L,J) = MXNO3U
              ENDIF

!             XMIN = minimum amount NH4 left after uptake (kg[N]/ha)
              XMIN = 0.5 / KG2PPM(L) * ColFrac(L,J) / FieldFac
              MXNH4U  = MAX(0.0,(SNH4_2D(L, J) - XMIN))
              IF (UNH4_2D(L,J) .GT. MXNH4U) THEN
                DayLostN = DayLostN + (UNH4_2D(L,J) - MXNH4U)
                UNH4_2D(L,J) = MXNH4U
              ENDIF

!             For 2D simulations, multiply by 2.0 because we are modeling only half a field.
              TRNO3U  = TRNO3U + UNO3_2D(L,J) * FieldFac
              TRNH4U  = TRNH4U + UNH4_2D(L,J) * FieldFac
              UNO3(L) = UNO3(L) + UNO3_2D(L,J) * FieldFac
              UNH4(L) = UNH4(L) + UNH4_2D(L,J) * FieldFac
            ENDIF
          END SELECT
        ENDDO
      ENDDO

!     debug chp
      CumLostN = CumLostN + DayLostN

!     Convert uptake to g/m^2 for plant routines
      TRNO3U = TRNO3U / 10.0
      TRNH4U = TRNH4U / 10.0
      TRNU   = TRNO3U + TRNH4U

      CELLS % RATE % NH4Uptake = UNH4_2D    !kg[N]/ha
      CELLS % RATE % NO3Uptake = UNO3_2D    !kg[N]/ha

!     Use Cell2Layer_2D for mass variables
      CALL Cell2Layer_2D(
     &  UNO3_2D, Cells%Struc, NLAYR,                  !Input
     &  UNO3, SurfaceVal)                             !Output
      CALL Cell2Layer_2D(
     &  UNH4_2D, Cells%Struc, NLAYR,                  !Input
     &  UNH4, SurfaceVal)                             !Output

!-----------------------------------------------------------------------
C   Update stover and root N
C-----------------------------------------------------------------------
      IF (NDEM .GT. TRNU / PLTPOP) THEN
        XNDEM  = TRNU / PLTPOP
        FACTOR = XNDEM / NDEM
        NDEM   = XNDEM
        TNDEM  = TNDEM  * FACTOR
        RNDEM  = RNDEM  * FACTOR
        TUBDEM = TUBDEM * FACTOR
      ENDIF

!     g[N]/plant
      TOPSN  = TOPSN + TNDEM
      ROOTN  = ROOTN + RNDEM
      TUBN   = TUBN  + TUBDEM

      WTNUP = WTNUP + TRNU * 10.0        !kg[N]/ha

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE PT_NUPTAK
