C=======================================================================
C  Aloha_NUPTAK, Subroutine
C
C  Determines N uptake
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes               P.W.W.      2-8-93
C  4  Modified by                                   W.T.B.     JUNE 94
C  5. Changed water content dependent factor J.T.R. & B.D.B. 28-Jun-1994
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  : NUF,NDEM,L,L1,THUMN,RNH4U,RNO3U,TRNLOS,TRLV,TOTN,DNG,
C           TNDEM,RNDEM,ANDEM,DROOTN,DSTOVN,FNH4,FNO3,SMDFR,RFAC,UNO3,UNH4,
C           XMIN,RNLOSS,XNDEM,FACTOR
C
C  OUTPUT : None
C-----------------------------------------------------------------------
C  Called : GROSUB SGROSUB MGROSUB WGROSUB
C
C  Calls  : None
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
C  TOTN   : Total mineral N in a layer (kg N/ha)
C  TRLV   : Total root length density variable
C  TRNLOS : Total plant N lost by root exudation (g N/plant)
C  UNH4   : Plant uptake of ammonium from a layer (kg N/ha)
C  UNO3   : Plant uptake of nitrate from a layer (kg N/ha)
C  XMIN   :
C  XNDEM  :
C=======================================================================

      SUBROUTINE Aloha_NUPTAK

      IMPLICIT  NONE

      INCLUDE  'GEN1.BLK'
      INCLUDE  'GEN3.BLK'
      INCLUDE  'GEN4.BLK'
      INCLUDE  'NTRC1.BLK'
      INCLUDE  'NTRC2.BLK'

      REAL      NUF,NDEM
      DIMENSION RNO3U(20),RNH4U(20)

      INTEGER   L,L1
      REAL      RNH4U,RNO3U,TRNLOS,TRLV,DNG,
     +          TNDEM,RNDEM,ANDEM,DROOTN,DSTOVN,FNH4,FNO3,SMDFR,RFAC,
     +          UNO3,UNH4,XMIN,RNLOSS,XNDEM,FACTOR

      SAVE

C-----------------------------------------------------------------------
C     Initialize variables
C-----------------------------------------------------------------------

      TRNLOS = 0.0
      RANC   = ROOTN / RTWT
      TANC   = STOVN / STOVWT
      TRLV   = 0.0
      ANO3   = 0.0
      ANH4   = 0.0
      DROOTN = 0.0
      DSTOVN = 0.0
      TRNU   = 0.0
      NUF    = 0.0

      DO L = 1, NLAYR
         RNO3U(L) = 0.0
         RNH4U(L) = 0.0
         TRLV     = TRLV    + RLV(L)
         NO3(L)   = SNO3(L) * FAC(L)
         NH4(L)   = SNH4(L) * FAC(L)
      END DO

C-----------------------------------------------------------------------
C     Calculate N demand (DNG=g N/PLT) for new growth (PDWI=g/plt)
C-----------------------------------------------------------------------

      IF (PDWI .EQ. 0.0) THEN
         PDWI = 1.0
      ENDIF
      DNG = PDWI*TCNP
      IF (XSTAGE .LE. 1.2) THEN
         DNG = 0.0
      ENDIF

C-----------------------------------------------------------------------
C     Calculate total N demand (NDEM) of tops (TNDEM) and roots (RNDEM),
C     all expressed in g N/plt Convert total N demand to kg N/ha (ANDEM)
C-----------------------------------------------------------------------

      TNDEM  = STOVWT * (TCNP-TANC) + DNG
      RNDEM  = RTWT   * (RCNP-RANC) + PGRORT*RCNP
      NDEM   = TNDEM  + RNDEM

C-----------------------------------------------------------------------
C     Convert total N demand from g N/plt to kg N/ha (ANDEM)
C-----------------------------------------------------------------------

      ANDEM  = NDEM   * PLTPOP*10.0

C-----------------------------------------------------------------------
C     Calculate potential N supply in soil layers with roots (TRNU)
C-----------------------------------------------------------------------

      DO L = 1, NLAYR
         IF (RLV(L) .NE. 0.0) THEN
            L1 = L
C
C           The following code was written by JTR to simplify the code for the
C           generic model and to make the functions more like the water uptake
C           functions.  Done on June 28, 1994.
C
C           New water content dependent factor for uptake and new uptake
C           methods that work like the soil water uptake  -- JTR 6/94
C
            SMDFR    = 1.5-6.0*((SW(L)-LL(L))/(SAT(L)-LL(L))-0.5)**2
            SMDFR    = AMAX1 (SMDFR,0.0)
            SMDFR    = AMIN1 (SMDFR,1.0)
            RFAC     = 1.0-EXP(-8.0*RLV(L))
            FNH4     = SHF(L)*0.075
            FNO3     = SHF(L)*0.075
            RNH4U(L) = RFAC*SMDFR*FNH4*(NH4(L)-0.5)*DLAYR(L)
            RNO3U(L) = RFAC*SMDFR*FNO3*NO3(L)*DLAYR(L)
            TRNU     = TRNU + RNO3U(L) + RNH4U(L)
         ENDIF
      END DO

C-----------------------------------------------------------------------
C     Calculate factor (NUF) to reduce N uptake to level of demand
C-----------------------------------------------------------------------

      IF (ANDEM .LE. 0.0) THEN
         TRNU  = 0.0
         NUF   = 0.0
       ELSE
         ANDEM = AMIN1 (ANDEM,TRNU)
         IF (TRNU .EQ. 0.0) RETURN
         NUF   = ANDEM/TRNU
         TRNU  = 0.0
      ENDIF

C-------------------------------------------------------------------------
C     Calculate N uptake in soil layers with roots based on demand (kg/ha)
C-------------------------------------------------------------------------

      DO L = 1, L1
         UNO3    = RNO3U(L)*NUF
         UNH4    = RNH4U(L)*NUF
         XMIN    = 0.25/FAC(L)
         UNO3    = MIN (UNO3,SNO3(L) - XMIN)
         SNO3(L) = MAX (SNO3(L) - UNO3, 0.0)
         XMIN    = 0.5/FAC(L)
         UNH4    = MIN (UNH4,SNH4(L) - XMIN)
         SNH4(L) = MAX (SNH4(L) - UNH4, 0.0)
         NO3(L)  = SNO3(L) * FAC(L)
         NH4(L)  = SNH4(L) * FAC(L)
         TRNU    = TRNU + UNO3 + UNH4
      END DO

      TRNU = TRNU/(PLTPOP*10.0)

C-----------------------------------------------------------------------
C     Update stover and root N
C-----------------------------------------------------------------------

      IF (NDEM .GT. TRNU) THEN
          XNDEM  = TRNU
          FACTOR = XNDEM / NDEM
          NDEM   = XNDEM
          TNDEM  = TNDEM * FACTOR
          RNDEM  = RNDEM * FACTOR
      ENDIF

      IF (NDEM .LE. 0.0 .OR. TRNU .LE. 0.0) THEN
         DSTOVN = 0.0
         DROOTN = 0.0
       ELSE
         !
         ! Calculate root exudation losses (if any)
         !
         DO L = 1, L1
            RNLOSS = 0.0
            IF (TANC .GT. TCNP) THEN
               RNLOSS = RANC*RTWT*0.05*PLTPOP*RLV(L)/TRLV
            ENDIF
            TRNLOS  = TRNLOS + RNLOSS
            FON(L)  = FON(L) + RNLOSS
         END DO
         !
         ! Adjust DSTOVN and DROOTN to compensate for N lost to FON
         !
         DSTOVN = TNDEM / NDEM*TRNU-      PTF*TRNLOS/(PLTPOP*10.0)
         DROOTN = RNDEM / NDEM*TRNU-(1.0-PTF)*TRNLOS/(PLTPOP*10.0)
      ENDIF

      STOVN = STOVN + DSTOVN
      TANC  = STOVN / STOVWT
      ROOTN = ROOTN + DROOTN
      RANC  = ROOTN / (RTWT-0.01*RTWT)

      RETURN
      END SUBROUTINE Aloha_NUPTAK
