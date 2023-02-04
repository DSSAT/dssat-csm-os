C=======================================================================
C  RI_NUPTAK, Subroutine
C
C  Determines N uptake
C
C-----------------------------------------------------------------------
C  Revision history
C  08/07/1993 PWW Header revision and minor changes
C  08/29/2002 CHP/MUS Converted to modular format for inclusion in CSM.
!  11/07/2005 CHP Replaced FAC with SOILPROP variable KG2PPM
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  ANDEM  : Crop N demand (kg N/ha)
C  ANH4   : Total extractable ammonium N in soil profile (kg N/ha)
C  ANO3   : Total extractable nitrate N in soil profile (kg N/ha)
C  DNG    : N demand of potential new growth of tops (g N/plant)
C  DROOTN : Daily change in plant root nitrogen content (g N/plant)
C  DSTOVN :
C  FACTOR : Relative weighting to distribute crop root residues at the beginning
C           of a simulation
C  FNH4   : Unitless soil ammonium supply index
C  FNO3   : Unitless soil nitrate supply index
C  L,L1   : Loop counters
C  NDEM   : Plant nitrogen demand (g/plant)
C  NUF    : Plant N supply/demand ratio used to modify uptake
C  RFAC   : Interim variable describing the effects of root length density
C           on potential N uptake from a layer
C  RNDEM  : Plant root demand for nitrogen (g/plant)
C  RNH4U  : Potential ammonium uptake from Layer L (kg N/ha)
C  RNLOSS : Loss of N from the plant via root exudation in one layer (g N/plant)
C  RNO3U  : Potential nitrate uptake from Layer L (kg N/ha)
C  SMDFR  : Soil moisture deficit factor affecting N uptake
C  THUMN  :
C  TNDEM  : Plant tops demand for nitrogen (g N/plant)
C  TRLV   : Total root length density variable
C  TRNLOS : Total plant N lost by root exudation (g N/plant)
C  UNH4   : Plant uptake of ammonium from a layer (kg N/ha)
C  UNO3   : Plant uptake of nitrate from a layer (kg N/ha)
C  XMIN   :
C  XNDEM  :
C=======================================================================

      SUBROUTINE RI_NUPTAK (DYNAMIC, 
     &    FLOOD, NH4, NO3, PDWI, PGRORT, PLANTS, PTF,     !Input
     &    RCNP, RLV, RTWT, SOILPROP, ST, STOVWT, SW, TCNP,!Input
     &    FLOODN, STOVN, RANC, ROOTN, TANC,               !I/O
     &    RNLOSS, SENESCE, TRNLOS, UNH4, UNO3, PLIGRT,    !Output
     &    CumNUptake)                                     !Output
!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
      USE FloodModule    ! parameters, hourly weather data.

      IMPLICIT  NONE
      SAVE

      REAL     PLANTS,RTWT,PTF

      REAL     ANDEM,DNG,DROOTN,DSTOVN,FACTOR,FNH4,FNO3,
     &         FRNH4U,FRNO3U,NUF,RFAC,RNO3U(NL),RNH4U(NL), RNDEM,
     &         SMDFR,TNDEM,TRNLOS,TRLV,
     &         XMIN,XNDEM,NDEM,TEMPRU(NL)

      INTEGER DYNAMIC, L, L1, NLAYR

      REAL FLDH4, FLDH4C, FLDN3, FLDN3C, FLOOD, PDWI, PGRORT
      REAL RANC, RCNP, ROOTN, STOVN, STOVWT, TANC, TCNP, TRNU
      REAL CUMFNU, CumNUptake !Cum flood N uptake, Cum N uptake total
      REAL, DIMENSION(NL) :: DLAYR, DUL, ESW, KG2PPM, LL, NH4, NO3
      REAL, DIMENSION(NL) :: RLV, RNLOSS, SNH4, SNO3, ST, SW, UNO3, UNH4

!     Proportion of lignin in roots
      REAL PLIGRT

      TYPE (ResidueType) SENESCE

!     The variable "SOILPROP" is of type "SoilType".
      TYPE (SoilType) SOILPROP
      TYPE (FloodNType) FLOODN

!     Transfer values from constructed data types into local variables.
      DLAYR = SOILPROP % DLAYR
      DUL   = SOILPROP % DUL
      KG2PPM= SOILPROP % KG2PPM
      LL    = SOILPROP % LL
      NLAYR = SOILPROP % NLAYR

!     Flood N
      FLDH4C = FLOODN % FLDH4C    !ppm
      FLDN3C = FLOODN % FLDN3C    !ppm
      FLDN3  = FLOODN % FLDN3     !kg/ha
      FLDH4  = FLOODN % FLDH4     !kg/ha

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      UNH4   = 0.0
      UNO3   = 0.0
      TRNLOS = 0.0
      RNLOSS = 0.0
      FRNH4U = 0.0
      FRNO3U = 0.0
      CUMFNU = 0.0
      
      CumNUptake = 0.0

      CumNUptake = 0.0              !kg[N]/ha

!***********************************************************************
!***********************************************************************
!     Daily rate / integration calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
C     Initialize daily variables
C-----------------------------------------------------------------------
      DO L = 1, NLAYR
        SNO3(L) = NO3(L) / KG2PPM(L)
        SNH4(L) = NH4(L) / KG2PPM(L)
        ESW(L) = DUL(L) - LL(L)
        UNO3(L) = 0.0
        UNH4(L) = 0.0
      ENDDO

      TRNLOS = 0.0
      IF (RTWT   .GT. 0.0) RANC = ROOTN / RTWT
      IF (STOVWT .GT. 0.0) TANC = STOVN / STOVWT
      TRLV   = 0.0
      DROOTN = 0.0
      DSTOVN = 0.0
      TRNU   = 0.0
      NUF    = 0.0
      XNDEM  = 0.0
      XMIN   = 0.0
      TNDEM  = 0.0
      RNLOSS = 0.0
      RNDEM  = 0.0
      RFAC   = 0.0
      NDEM   = 0.0
      FNO3   = 0.0
      FNH4   = 0.0
      FACTOR = 0.0
      DNG    = 0.0
      ANDEM  = 0.0

      DO L = 1, NLAYR
         RNO3U(L) = 0.0
         RNH4U(L) = 0.0
         TRLV     = TRLV    + RLV(L)

         IF (ST(L) .LT. 9.) THEN
           TEMPRU(L) = 0.0
         ELSEIF (ST(L) .GT. 27.) THEN
           TEMPRU(L) = 1.0
         ELSE
           TEMPRU(L) = 1.0 -0.055*(27.0 -ST(L))
         ENDIF
      END DO

C-----------------------------------------------------------------------
C     Calculate N demand (DNG=g N/PLT) for new growth (PDWI=g/plt)
C-----------------------------------------------------------------------
      DNG = PDWI*TCNP

C-----------------------------------------------------------------------
C     Calculate total N demand (NDEM) of tops (TNDEM) and roots (RNDEM),
C     all expressed in g N/plt Convert total N demand to kg N/ha (ANDEM)
C-----------------------------------------------------------------------
      TNDEM  = AMAX1((STOVWT*(TCNP-TANC) + DNG),0.0)
      RNDEM  = AMAX1((RTWT  *(RCNP-RANC) + PGRORT*RCNP),0.0)
      NDEM   = TNDEM  + RNDEM

C-----------------------------------------------------------------------
C     Convert total N demand from g N/plt to kg N/ha (ANDEM)
C-----------------------------------------------------------------------
      ANDEM  = NDEM*PLANTS*10.0

C-----------------------------------------------------------------------
C     Calculate potential N supply in soil layers with roots (TRNU)
C-----------------------------------------------------------------------
      DO L = 1, NLAYR
         IF (RLV(L) .LT. 0.001) THEN
            EXIT
         ENDIF
            L1 = L

         FNH4     = 1.0 - EXP(-0.030*(NH4(L)-0.5))
         FNO3     = 1.0 - EXP(-0.030*NO3(L))
         IF (FNO3 .LT. 0.03) THEN
            FNO3  = 0.0
         ENDIF
         FNO3  = AMIN1 (FNO3,1.0)
         IF (FNH4 .LT. 0.03) THEN
            FNH4  = 0.0
         ENDIF
         FNH4     = AMIN1 (FNH4,1.0)

         SMDFR    = (SW(L) - LL(L)) / ESW(L)
            SMDFR    = AMAX1 (SMDFR,0.0)
            SMDFR    = AMIN1 (SMDFR,1.0)
         IF (FLOOD .GT. 0.0) THEN
            SMDFR = 1.0
         ENDIF

         RFAC     = RLV(L)*SMDFR*SMDFR*DLAYR(L)*100.0*TEMPRU(L)
         RNO3U(L) = RFAC * FNO3 * 0.008
         RNH4U(L) = RFAC * FNH4 * 0.008
         TRNU     = TRNU + RNO3U(L) + RNH4U(L)    !kg[N]/ha
      END DO

      FRNH4U = 0.0
      FRNO3U = 0.0

      IF (FLOOD .GT. 0.0 .AND. RLV(1) .GT. 1.0) THEN
         !For very low FLOOD levels, if fertilizer is added, the
         !   concentrations can be very high.  Check to prevent
         !   underflows.
         IF (FLDH4C .LT. 500.) THEN
           FNH4 = 1.0 - EXP(-0.030*FLDH4C)
         ELSE
           FNH4 = 1.0
         ENDIF
         IF (FLDN3C .LT. 500.) THEN
           FNO3 = 1.0 - EXP(-0.030*FLDN3C)
         ELSE
           FNO3 = 1.0
         ENDIF
         IF (FNO3 .LT. 0.03) THEN
            FNO3 = 0.0
         ENDIF
         FNO3 = AMIN1 (FNO3,1.0)
         IF (FNH4 .LT. 0.03) THEN
            FNH4 = 0.0
         ENDIF
         FNH4   = AMIN1 (FNH4,1.0)

         RFAC   = 0.005* RLV(1)*FLOOD*10.0*TEMPRU(1)
         FRNO3U = RFAC * FNO3*0.008
         FRNH4U = RFAC * FNH4*0.008
         TRNU   = TRNU + FRNO3U + FRNH4U          !kg[N]/ha
      ENDIF

C-----------------------------------------------------------------------
C     Calculate factor (NUF) to reduce N uptake to level of demand
C-----------------------------------------------------------------------
      IF (ANDEM .LE. 0.0) THEN
         NUF   = 0.0
       ELSE
         ANDEM = AMIN1 (ANDEM,TRNU)
         IF (TRNU .LT. 0.0001) THEN
            RETURN
         ENDIF
         NUF   = ANDEM/TRNU
      ENDIF

C-------------------------------------------------------------------------
C     Calculate N uptake in soil layers with roots based on demand (kg/ha)
C-------------------------------------------------------------------------
      TRNU = 0.0
      XMIN    = 0.0

      DO L = 1, L1
         UNO3(L)    = RNO3U(L) * NUF !* TEMPRU(L)
         UNO3(L)    = AMIN1 (UNO3(L),SNO3(L)-XMIN)

         UNH4(L)    = RNH4U(L) * NUF !* TEMPRU(L)
         UNH4(L)    = AMIN1 (UNH4(L),SNH4(L)-XMIN)

         RNLOSS(L)  = 0.0
         IF (TANC .GT. TCNP .AND. TRLV .GT. 0.0) THEN
            RNLOSS(L) = RANC*RTWT*0.075*PLANTS*RLV(L)/TRLV
         ENDIF
         !Calculate N in senesced roots (kg/ha)
         SENESCE % ResE(L,1) = RNLOSS(L) * 10.0
!                   kg[N]/ha =  g/m2  * 10.

         !Back calculate senesced root mass from N senesced.
         IF (RANC .GT. 0.0) THEN
              SENESCE % ResWt(L) = SENESCE % ResE(L,1) / RANC   
         ELSE                               !kg[dry matter]/ha
              SENESCE % ResWt(L) = SENESCE % ResE(L,1) * 10.0 / 0.40   
!              kg[dry matter]/ha =       kg[N]/ha * kg[C]/kg[N]         
!                                                 / kg[C]/kg[dry matter]
         ENDIF
            !Compute lignin, cellulose and carbohydrate portions
         SENESCE % ResLig(L) = SENESCE % ResWt(L) * PLIGRT


         TRNLOS  = TRNLOS + RNLOSS(L)
         TRNU    = TRNU + UNO3(L) + UNH4(L)       !kg[N]/ha
      END DO

      IF (FLOOD .GT. 0.0 .AND. RLV(1) .GT. 1.0) THEN
         FRNO3U  = FRNO3U * NUF    !*TEMPRU(1)
         FRNO3U  = AMIN1 (FRNO3U,FLDN3)

         FRNH4U  = FRNH4U * NUF    !*TEMPRU(1)
         FRNH4U  = AMIN1 (FRNH4U, FLDH4)

         TRNU  = TRNU  + FRNO3U  + FRNH4U         !kg[N]/ha
      ENDIF
      CumNUptake = CumNUptake + TRNU              !kg[N]/ha
      TRNU = TRNU/(PLANTS*10.0)                   !g[N]/plant
      CUMFNU = CUMFNU + (FRNH4U + FRNO3U) / (PLANTS * 10.0)

C-----------------------------------------------------------------------
C     Update stover and root N
C-----------------------------------------------------------------------

      IF (NDEM .GT. TRNU .AND. NDEM > 1.E-6) THEN
          XNDEM  = TRNU
          FACTOR = XNDEM / NDEM
          NDEM   = XNDEM
          TNDEM  = TNDEM * FACTOR
          RNDEM  = RNDEM * FACTOR
      ENDIF

      DSTOVN = 0.0
      DROOTN = 0.0
      IF (NDEM .GT. 0) THEN
         DSTOVN = TNDEM/NDEM*TRNU-     PTF *TRNLOS/(PLANTS*10.0)
         DROOTN = RNDEM/NDEM*TRNU-(1.0-PTF)*TRNLOS/(PLANTS*10.0)
      ENDIF

      STOVN = STOVN + DSTOVN
      ROOTN = ROOTN + DROOTN
      TANC  = STOVN/STOVWT
      RANC  = ROOTN/RTWT


!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
!     Transfer values of N uptake from floodwaters to FLOODN variable.
      FLOODN % FRNH4U = FRNH4U
      FLOODN % FRNO3U = FRNO3U

      RETURN
      END SUBROUTINE RI_NUPTAK
