C=======================================================================
C  PT_NUPTAK_2D, Subroutine
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
!  02/25/2018 MZ  Converted to 2D
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  NUF    : Plant N supply/demand ratio used to modify uptake
C  NDEM   : Plant nitrogen demand (g/plant)
C  L      : Loop counters
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

      SUBROUTINE PT_NUPTAK_2D (DYNAMIC, CELLS,
     &    ISTAGE, DLAYR, DUL, KG2PPM, LL, NLAYR,          !Input
     &    PLTPOP, RCNP, RTWT, SAT, TCNP, TMNC,            !Input
     &    TOPWT, TUBCNP, TUBWT,                           !Input
     &    GRORT, GROTOP, GROTUB, ROOTN, TOPSN, TUBANC,    !I/O
     &    ARVCHO, RANC, TANC, TRNU, TUBN, UNH4, UNO3,     !Output
     &    WTNUP)                                          !Output

!-----------------------------------------------------------------------
      USE Cells_2D
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT  NONE
      SAVE
      
      Type (CellType) Cells(MaxRows,MaxCols)
      INTEGER, DIMENSION(MaxRows,MaxCols) :: Cell_Type 
      REAL, DIMENSION(MaxRows, MaxCols) :: ColFrac

      INTEGER DYNAMIC, ISTAGE, L, NLAYR

      REAL ANDEM, ARVCHO, AVAILN, EXTRAN, FACTOR 
      REAL FNH4, FNO3, GRFN, GRORT, GROTOP, GROTUB 
      REAL NDEM, NPART, NUF, PGROW, PLTPOP, RANC 
      REAL RATIO, RCNP, RFAC, RNDEM, ROOTN, RTWT
      REAL SMDFR, STOPSN, TANC, TCNP, TMNC, TNDEM
      REAL TOPNUSD, TOPSN, TOPWT, TRNU 
      REAL TUBANC, TUBCNP, TUBDEM, TUBN, TUBMNC
      REAL TUBSINK, TUBSN, TUBWT
      REAL WTNUP, XMIN, XNDEM

      REAL, DIMENSION(NL) :: DLAYR, DUL, ESW, KG2PPM, LL, NH4, NO3
      REAL, DIMENSION(NL) :: SAT, UNO3, UNH4
      
      INTEGER J, FurCol1
      REAL HalfRow, BEDWD
      REAL, DIMENSION(MaxRows,MaxCols) :: NO3_2D, NH4_2D, RLV_2D
      REAL, DIMENSION(MaxRows,MaxCols) :: SNO3_2D, SNH4_2D, SWV,RNH4U_2D
      REAL, DIMENSION(MaxRows,MaxCols) :: UNO3_2D, UNH4_2D, RNO3U_2D

      SWV    = CELLS % State % SWV
      RLV_2D = CELLS % State % RLV
      SNO3_2D = CELLS % State % SNO3
      SNH4_2D = CELLS % State % SNH4

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      TUBMNC  = 0.007
      TUBSINK = 0.0   !from PHASEI
      WTNUP   = 0.0   !Seasonal total N uptake (kg[N]/ha)
      UNH4_2D = 0.0
      UNO3_2D = 0.0
      RLV_2D  = 0.0
      
      CELLS % RATE % NH4Uptake = UNH4_2D    !kg[N]/ha
      CELLS % RATE % NO3Uptake = UNO3_2D    !kg[N]/ha
      
      HalfRow = BedDimension % ROWSPC_cm / 2
      BEDWD   = BedDimension % BEDWD
      FurCol1 = BedDimension % FurCol1 
      ColFrac = BedDimension % ColFrac

!***********************************************************************
!***********************************************************************
!     Daily rate calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
C   Initialize variables
C-----------------------------------------------------------------------
      ARVCHO = 0.0
      NUF    = 0.0
      TRNU   = 0.0
      RNO3U_2D = 0.0
      RNH4U_2D = 0.0
      UNH4_2D  = 0.0
      UNO3_2D  = 0.0

      DO L = 1, NRowsTot
        DO J = 1, NColsTot
          NO3_2D(L, J) = SNO3_2D(L, J) * KG2PPM(L)
          NH4_2D(L, J) = SNH4_2D(L, J) * KG2PPM(L)
        ENDDO
      ENDDO

C-----------------------------------------------------------------------
C   Calculate potential N supply in soil layers with roots (TRNU)
C-----------------------------------------------------------------------

      DO L = 1, NRowsTot 
        DO J = 1, NColsTot
          IF (RLV_2D(L, J) .NE. 0.0) THEN
             ESW(L) = DUL(L) - LL(L)
             FNO3 = 1.0 - EXP(-0.0275*NO3_2D(L, J))
             FNH4 = 1.0 - EXP(-0.0250*NH4_2D(L, J))
             IF (FNO3 .LT. 0.03) THEN
                FNO3 = 0.0
             ENDIF
             IF (FNH4 .LT. 0.03) THEN
                FNH4 = 0.0
             ENDIF
             FNO3  = AMIN1 (FNO3, 1.0)
             FNH4  = AMIN1 (FNH4, 1.0)

             SMDFR = (SWV(L, J) - LL(L)) / ESW(L)
             SMDFR = AMAX1 (SMDFR, 0.0)

             IF (SMDFR .GT. 1.0) THEN
                SMDFR = (SAT(L) - SWV(L, J)) / (SAT(L) - DUL(L))
             ENDIF

             RFAC = RLV_2D(L, J) * SMDFR * SMDFR * DLAYR (L) * 100.0
             RNO3U_2D(L, J) = RFAC * FNO3 * 0.006
             RNH4U_2D(L, J) = RFAC * FNH4 * 0.006
             RNO3U_2D(L, J) = MAX(0.0, RNO3U_2D(L, J))
             RNH4U_2D(L, J) = MAX(0.0, RNH4U_2D(L, J))
             TRNU = TRNU + (RNO3U_2D(L, J) + RNH4U_2D(L, J)) * 
     &              ColFrac(L, J) !kg[N]/ha

          ENDIF
        END DO
      END DO

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
        CASE (1)                              ! Vegetative development

        NDEM = TNDEM + RNDEM                  !g[N]/plant

        IF (AVAILN .GE. NDEM) THEN            ! N sufficient for all
           IF (NDEM .NE. 0.0) THEN            ! N demands.
              RATIO = AMIN1 (AVAILN/NDEM, 1.3)! EXTRAN partitioned
            ELSE                              ! on demand RATIO
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
              ! STOPSN allows for dilution of haulm N
              !
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
           !
           ! Accumulate less N in the tubers under excess N conditions.
           ! Don't allow any excess N to accumulate in the roots unless 
           ! a provision is added to re-allocate excess N in the roots 
           ! to other plant parts.
           !
              TUBDEM = TUBDEM * (1.0 + (RATIO - 1.0)*0.5)
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
C-----------------------------------------------------------------------


      DO L = 1, NRowsTot
        DO J = 1, NColsTot
          UNO3_2D(L, J) = RNO3U_2D(L, J) * NUF
          UNH4_2D(L, J) = RNH4U_2D(L, J) * NUF
          XMIN          = 0.25 / KG2PPM(L)
          UNO3_2D(L, J) = AMIN1 (UNO3_2D(L, J), SNO3_2D(L, J) - XMIN)
          UNO3_2D(L, J) = MAX(0.0, UNO3_2D(L, J))
          XMIN          = 0.5 / KG2PPM(L)
          UNH4_2D(L, J) = AMIN1 (UNH4_2D(L, J),SNH4_2D(L, J) - XMIN)
          UNH4_2D(L, J) = MAX(0.0, UNH4_2D(L, J))
          TRNU          = TRNU + UNO3_2D(L, J) + UNH4_2D(L, J)       !kg[N]/ha
        END DO
      END DO

      TRNU = TRNU/(PLTPOP*10.0)                       !g[N]/plant

      CELLS % RATE % NH4Uptake = UNH4_2D    !kg[N]/ha
      CELLS % RATE % NO3Uptake = UNO3_2D    !kg[N]/ha
      CAll Interpolate2Layers_2D(UNO3_2D, Cells%Struc, NLAYR,  !input
     &         UNO3)                                           !Output
      CAll Interpolate2Layers_2D(UNH4_2D, Cells%Struc, NLAYR,  !input
     &         UNH4)                                           !Output

!-----------------------------------------------------------------------
C   Update stover and root N
C-----------------------------------------------------------------------

      IF (NDEM .GT. TRNU) THEN
         XNDEM  = TRNU
         FACTOR = XNDEM / NDEM
         NDEM   = XNDEM
         TNDEM  = TNDEM  * FACTOR
         RNDEM  = RNDEM  * FACTOR
         TUBDEM = TUBDEM * FACTOR
      ENDIF

      !g[N]/plant
      TOPSN  = TOPSN + TNDEM      
      ROOTN  = ROOTN + RNDEM      
      TUBN   = TUBN  + TUBDEM    

      WTNUP = WTNUP + TRNU * PLTPOP * 10.0        !kg[N]/ha
!      WTNUP = WTNUP + TRNU * PLTPOP                !g[N]/m2

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN         
      END SUBROUTINE PT_NUPTAK_2D
