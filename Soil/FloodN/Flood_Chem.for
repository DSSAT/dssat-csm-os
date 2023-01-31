C=======================================================================
C  FLOOD_CHEM, Subroutine, U. Singh, C.H.Porter
C  Flooded conditions chemistry - main routine.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  03/13/2002 CHP Written based on flooded conditions portions of NTRANS
!  02/21/2006 CHP Limit flood N species to non-negative values.
!  03/02/2006 CHP ALGFON instead of DLTFON for N content of algae
!  01/26/2023 CHP Reduce compile warnings: add EXTERNAL stmts, remove 
!                 unused variables, shorten lines. 
C-----------------------------------------------------------------------
! Called by: NTRANS_inorg
! Calls:     FCHEM, PERC_N, NRUNOFF, DRYUP, FLOODI
C=======================================================================
      SUBROUTINE FLOOD_CHEM(CONTROL, ISWITCH, 
     &    FLOODWat, LFD10, SOILPROP, SNO3, SNH4, UREA,    !Input
     &    SRAD, SW, TMAX, TMIN, XHLAI,                    !Input
     &    DLTSNH4, DLTSNO3, DLTUREA, FLOODN, OXLAYR,      !I/O
     &    ALGFIX, BD1, CUMFNRO, TOTAML, TOTFLOODN)        !Output
 
      USE ModuleDefs
      USE FloodModule
      IMPLICIT NONE
      EXTERNAL FCHEM, PERC_N, NRUNOFF, DRYUP, FLOODI
      SAVE

      CHARACTER*1 ISWNIT
      INTEGER DYNAMIC, L, LFD10, IBP
      INTEGER NDAT, NLAYR, NSWITCH, NBUND
      INTEGER YRDOY, YRDRY, YRWET
      REAL ALI, ALGACT, ALGFIX, ALGFON, BD1, BDP, BD2, BDP2, DLAYR1, EF
      REAL FLDH4, FLDN3, FLDU, FLOOD
      REAL INFILT
      REAL OXFAC, OXLT, OXMIN3, OXMIN4
      REAL SRAD
      REAL SURCEC, SW1, TMIN, TMAX, XHLAI
      REAL DLTFUREA, DLTFNO3, DLTFNH4 
      REAL DLTOXU, DLTOXH4, DLTOXN3
      REAL FLDH4C, FLDN3C
      REAL FRUNOFF, FNI
      REAL, DIMENSION(NL) :: BD, DLAYR, 
     &    DLTSNH4, DLTSNO3, DLTUREA, 
     &    DUL, LL, OC, PH, PO, 
     &    SAT, SNO3, SNH4, SW, 
     &    UREA, WFP
      REAL CUMFNRO, CUMPERCN, FNRO, PERCN, TOTFLOODN, TOTAML
      REAL FRNH4U, FRNO3U, CUMFNU
      REAL OXU, OXH4, OXN3
      LOGICAL AKILL, DailyCalc, IMMOBIL, PUDDLED

!-----------------------------------------------------------------------
      TYPE (ControlType)  CONTROL
      TYPE (SwitchType)   ISWITCH
      TYPE (SoilType)     SOILPROP
      TYPE (FloodNType)   FloodN
      TYPE (FloodWatType) FloodWat
      TYPE (OxLayerType)  OxLayr

      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY

      ISWNIT  = ISWITCH % ISWNIT
      NSWITCH = ISWITCH % NSWI

      EF      = FLOODWAT % EF       !From PaddyMgmt
      FLOOD   = FLOODWAT % FLOOD    !From PaddyMgmt
      NBUND   = FLOODWAT % NBUND    !From PaddyMgmt
      FRUNOFF = FLOODWAT % FRUNOFF  !From PaddyMgmt
      INFILT  = FLOODWAT % INFILT   !From SPAM
      PUDDLED = FLOODWAT % PUDDLED  !from PaddyMgmt
      YRDRY   = FLOODWAT % YRDRY    !From PaddyMgmt
      YRWET   = FLOODWAT % YRWET    !From PaddyMgmt

      NDAT     = FLOODN % NDAT       !from Rice phenology
      FRNH4U   = FLOODN % FRNH4U     !from Rice NUPTAK
      FRNO3U   = FLOODN % FRNO3U     !from Rice NUPTAK
      DLTFUREA = FLOODN % DLTFUREA   
      DLTFNO3  = FLOODN % DLTFNO3    
      DLTFNH4  = FLOODN % DLTFNH4    

      BD     = SOILPROP % BD
      SURCEC = SOILPROP % CEC(1)
!     Keep value of DLAYR(1), DLAYR1 can be modified
      DLAYR  = SOILPROP % DLAYR    
      DUL    = SOILPROP % DUL
      LL     = SOILPROP % LL
      OC     = SOILPROP % OC
      PH     = SOILPROP % PH
      NLAYR  = SOILPROP % NLAYR
      SAT    = SOILPROP % SAT

      SW1 = SW(1)

      DailyCalc= OXLAYR % DailyCalc
      OXLT     = OXLAYR % OXLT
      DLTOXU   = OXLAYR % DLTOXU 
      DLTOXH4  = OXLAYR % DLTOXH4
      DLTOXN3  = OXLAYR % DLTOXN3
      IBP      = OXLAYR % IBP
      OXU      = OXLAYR % OXU 
      OXH4     = OXLAYR % OXH4
      OXN3     = OXLAYR % OXN3
      OXLT     = OXLAYR % OXLT

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!      SMIN4 = 0.0
!      SMIN3 = 0.0

!     Don't do this for sequenced runs!!!!
C  FLDU   : Floodwater urea (kg N/ha)
      FLDU = 0.0
      FLDH4 = 0.0
      FLDN3 = 0.0

      FLDH4C = 0.0
      FLDN3C = 0.0

      TOTAML  = 0.0  !from INPLNT
      FNI     = 0.1
      IMMOBIL = .FALSE.    
      ALGFIX  = 0.0
      ALGACT  = 0.1
      ALGFON  = 0.0

      BD1 = BD(1)
      BD2 = BD(2)
      OXLT  = MAX(0.50 - 0.1*OC(1), 0.01)
      OXFAC = 1.0/(BD1*OXLT*1.0E-01)

      CUMFNRO  = 0.0      !Cumulative N in flood runoff over bund
      CUMPERCN = 0.0      !Cumulative N in flood water which percs
      CUMFNU   = 0.0      !Cumulative flood N uptake by plant
      TOTFLOODN = 0.0

!     Zero flood N flux variables
      DLTFUREA = 0.0
      DLTFNO3  = 0.0
      DLTFNH4  = 0.0

      IF (NBUND > 0) THEN
        CALL FCHEM(CONTROL, ISWITCH,
     &    BD1, SURCEC, DLAYR, EF, FLDH4, FLDN3, FLDU,     !Input
     &    FLOOD, LFD10, NSWITCH, OC, OXMIN3, OXMIN4,      !Input
     &    OXU, OXH4, OXN3, OXLT, PH, SNH4, SNO3, SRAD,    !Input
     &    SW1, TMAX, TMIN, UREA, XHLAI, YRDOY, YRDRY,     !Input
     &    AKILL, ALGFON, DailyCalc, DLTSNH4, DLTSNO3,     !I/O
     &    DLTUREA, DLTFUREA, DLTFNO3, DLTFNH4, DLTOXU,    !I/O
     &    DLTOXH4, DLTOXN3, IBP, IMMOBIL,                 !I/O
     &    ALGFIX, ALI, FNI, TOTAML)                       !Output
      ENDIF

!***********************************************************************
!***********************************************************************
!     Daily rate calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
      ALGFON  = 0.0    !FON added to layer 1 due to algae kill or dry-up
      DLTFNH4  = DLTFNH4 + FRNH4U
      DLTFNO3  = DLTFNO3 + FRNO3U
      !DLTFUREA = 0.0

!      IF (CEC(1) .GT. 0.0) THEN
!         SURCEC = CEC(1)  
!      ENDIF
      SURCEC = MAX(SURCEC, 0.0)

      DO L = 1, NLAYR
        PO(L)  = 1.0 - BD(L) / 2.65
      ENDDO

!     Eventually this needs to be put in tillage routine.
      IF (PUDDLED) THEN
         BDP   = BD(1)*BD(1)/(PO(1)+BD(1))    
         BDP2  = BD(2)*BD(2)/(PO(2)+BD(2))    
         BDP   = AMAX1 (BDP ,0.40)
         BDP2  = AMAX1 (BDP2,0.40)
      ENDIF

!     From POROSITY subroutine
      IF (PUDDLED .AND. NDAT .GT. 0) THEN
         BD1 = BDP  + (BD(1)-BDP )*NDAT/65.0
         BD2 = BDP2 + (BD(2)-BDP2)*NDAT/65.0
         BD1 = AMAX1(0.0,BD1)
         BD2 = AMAX1(0.0,BD2)
         BD1 = AMIN1 (BD1,BD(1))      
         BD2 = AMIN1 (BD2,BD(2))      
      ELSE
         BD1 = BD(1)
         BD2 = BD(2)
      ENDIF

      IF (FLOOD .GT. 0.0) THEN
        CALL PERC_N(
     &    FLOOD, FLDH4, FLDN3, FLDU, INFILT, ISWNIT,      !Input
     &    DLTFUREA, DLTFNO3, DLTFNH4, DLTSNH4,            !I/O
     &    DLTSNO3, DLTUREA, DLTOXU, DLTOXN3, DLTOXH4,     !I/O
     &    CUMPERCN, PERCN)                                !Output

        IF (FRUNOFF .GT. 0.0 .AND. NSWITCH .NE. 10) THEN
          CALL NRUNOFF (
     &    FLDH4, FLDN3, FLDU, FLOOD, FRUNOFF,             !Input
     &    DLTFNO3, DLTFNH4, DLTFUREA,                     !I/O
     &    CUMFNRO, FNRO)                                  !Output
        ENDIF
      ENDIF

      !KG2PPM(1) = 1.0/(BD1*1.E-01*DLAYR(1))
      !KG2PPM(2) = 1.0/(BD2*1.E-01*DLAYR(2))

!    OXFAC Conversion factor - used only in PaddyN routines. (local)
      OXFAC  = 1.0/(BD1*OXLT*1.0E-01)
      OXMIN4 = 0.0 !0.01 / OXFAC  TEMP 3/22/03
      OXMIN3 = 0.0 !0.01 / OXFAC

      DLAYR1 = DLAYR(1)

      DO L = 1, NLAYR
        IF (FLOOD .GT. 0.0) THEN
          IF (L .LE. 2) THEN
             WFP(L) = SAT(L)/PO(L)    !NTRANS, OXLAYER, POROSITY
          ELSE
             WFP(L) = 1.0
          ENDIF
        ELSE
          WFP(L) = SW(L) / PO(L)
        ENDIF
        WFP(L) = AMIN1 (WFP(L), 1.0)
      END DO
!--------------------------------------------

!     If flood waters just dryed up, perform initialization
      IF (YRDOY .EQ. YRDRY) THEN
        CALL DRYUP(
     &    LFD10, YRDOY,                                   !Input
     &    AKILL, ALGFIX, ALGFON, DLTOXN3, DLTOXH4,        !I/O
     &    DLTOXU, DLTSNH4, DLTSNO3, DLTUREA, FLDN3,       !I/O
     &    FLDH4, FLDU, IMMOBIL,                           !I/O
     &    ALGACT, DailyCalc)                              !Output

!     If newly flooded conditions exist, perform initialization
      ELSEIF (YRDOY .EQ. YRWET) THEN
        CALL FLOODI (
     &    BD1, FLOODWAT, OXMIN3, OXMIN4, SNH4,            !Input  
     &    SNO3, SOILPROP, SW1, UREA, YRDOY, YRDRY,        !Input
     &    DailyCalc, DLTUREA,  DLTSNH4, DLTSNO3, DLTFUREA,!I/O
     &    DLTFNO3, DLTFNH4, IBP,                          !I/O
     &    ALGACT, AKILL, ALGFIX, DLTOXU, DLTOXH4, DLTOXN3,!Output
     &    FNI, IMMOBIL, OXLT)                             !Output
      ENDIF

      IF (FLOOD .GT. 0.0) THEN
        CALL FCHEM(CONTROL, ISWITCH,
     &    BD1, SURCEC, DLAYR, EF, FLDH4, FLDN3, FLDU,     !Input
     &    FLOOD, LFD10, NSWITCH, OC, OXMIN3, OXMIN4,      !Input
     &    OXU, OXH4, OXN3, OXLT, PH, SNH4, SNO3, SRAD,    !Input
     &    SW1, TMAX, TMIN, UREA, XHLAI, YRDOY, YRDRY,     !Input
     &    AKILL, ALGFON, DailyCalc, DLTSNH4, DLTSNO3,     !I/O
     &    DLTUREA, DLTFUREA, DLTFNO3, DLTFNH4, DLTOXU,    !I/O
     &    DLTOXH4, DLTOXN3, IBP, IMMOBIL,                 !I/O
     &    ALGFIX, ALI, FNI, TOTAML)                       !Output
      ENDIF

!***********************************************************************
!***********************************************************************
!     Daily integration
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
      IF (FLOOD .GT. 0.0) THEN
        FLDU  = MAX(0.0, FLDU  + DLTFUREA)
        FLDN3 = MAX(0.0, FLDN3 + DLTFNO3)
        FLDH4 = MAX(0.0, FLDH4 + DLTFNH4)
        TOTFLOODN = FLDU + FLDN3 + FLDH4

!       Compute concentrations of N in floodwater.
        FLDH4C = FLDH4  * 100.0 / FLOOD   !ppm
        FLDN3C = FLDN3  * 100.0 / FLOOD

      ELSE
        FLDU  = 0.0
        FLDN3 = 0.0
        FLDH4 = 0.0
        TOTFLOODN = 0.0
        FLDH4C = 0.0
        FLDN3C = 0.0
      ENDIF

      DLTFUREA = 0.0
      DLTFNO3  = 0.0
      DLTFNH4  = 0.0

      CUMFNU = CUMFNU + FRNH4U + FRNO3U

!***********************************************************************
!***********************************************************************
!     Daily OUTPUT
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
!-----------------------------------------------------------------------
      CALL FCHEM(CONTROL, ISWITCH,
     &    BD1, SURCEC, DLAYR, EF, FLDH4, FLDN3, FLDU,     !Input
     &    FLOOD, LFD10, NSWITCH, OC, OXMIN3, OXMIN4,      !Input
     &    OXU, OXH4, OXN3, OXLT, PH, SNH4, SNO3, SRAD,    !Input
     &    SW1, TMAX, TMIN, UREA, XHLAI, YRDOY, YRDRY,     !Input
     &    AKILL, ALGFON, DailyCalc, DLTSNH4, DLTSNO3,     !I/O
     &    DLTUREA, DLTFUREA, DLTFNO3, DLTFNH4, DLTOXU,    !I/O
     &    DLTOXH4, DLTOXN3, IBP, IMMOBIL,                 !I/O
     &    ALGFIX, ALI, FNI, TOTAML)                       !Output

!***********************************************************************
!***********************************************************************
!     SEASONAL OUTPUT
!***********************************************************************
!      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
!      WRITE(*,500) CUMFNU
!  500 FORMAT('Cumulative flood N uptake', F10.2)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
      FLOODN % ALGFON = ALGFON
      FLOODN % FLDH4C = FLDH4C
      FLOODN % FLDN3C = FLDN3C
      FLOODN % FLDU   = FLDU 
      FLOODN % FLDH4  = FLDH4
      FLOODN % FLDN3  = FLDN3
      FLOODN % DLTFUREA = DLTFUREA
      FLOODN % DLTFNO3  = DLTFNO3 
      FLOODN % DLTFNH4  = DLTFNH4 

      OXLAYR % ALGACT   = ALGACT
      OXLAYR % OXMIN4   = OXMIN4
      OXLAYR % OXMIN3   = OXMIN3
      OXLAYR % DailyCalc= DailyCalc
      OXLAYR % DLTOXU   = DLTOXU 
      OXLAYR % DLTOXH4  = DLTOXH4
      OXLAYR % DLTOXN3  = DLTOXN3
      OXLAYR % OXU      = OXU 
      OXLAYR % OXH4     = OXH4
      OXLAYR % OXN3     = OXN3
      OXLAYR % OXLT     = OXLT

!***********************************************************************
      RETURN
      END SUBROUTINE FLOOD_CHEM
C=======================================================================


C=======================================================================
C  DRYUP, Subroutine, U. Singh
C  Determines floodwater dryup
C-----------------------------------------------------------------------
C  REVISION HISTORY
C             US  Written
C  03/13/2002 CHP added to modular model.
C                 changed IMMOB to IMMOBIL because name taken in NTRANS.
C=======================================================================

      SUBROUTINE DRYUP (
     &    LFD10, YRDOY,                                   !Input
     &    AKILL, ALGFIX, ALGFON, DLTOXN3, DLTOXH4,        !I/O
     &    DLTOXU, DLTSNH4, DLTSNO3, DLTUREA, FLDN3,       !I/O
     &    FLDH4, FLDU, IMMOBIL,                           !I/O
     &    ALGACT, DailyCalc)                              !Output

      USE ModuleDefs
      IMPLICIT  NONE
      SAVE

      INTEGER LFD10, YRDOY
      REAL    FLDH4, FLDN3, FLDU
      REAL    ALGACT
      REAL    DLTOXU, DLTOXN3, DLTOXH4
      REAL    ALGFIX, ALGFON
      REAL, DIMENSION(NL) :: DLTSNH4, DLTSNO3, DLTUREA
      LOGICAL AKILL, DailyCalc, IMMOBIL

      !
      ! Do the N stuff here
      !
!      IF (ISWNIT .EQ. 'Y') THEN
         !
         ! Return N tied up in algae to the soil
         !
!         IF (AKILL .EQ. 0 .AND. IMMOBIL .EQ. 1) THEN
         IF (.NOT. AKILL .AND. IMMOBIL) THEN
            ALGFON = ALGFON + ALGFIX
         ENDIF

         DLTSNO3(1) = DLTSNO3(1) + FLDN3
         DLTSNH4(1) = DLTSNH4(1) + FLDH4
         DLTUREA(1) = DLTUREA(1) + FLDU
         DLTOXN3    = DLTOXN3 + FLDN3   
         DLTOXH4    = DLTOXH4 + FLDH4   
         DLTOXU     = DLTOXU  + FLDU    
         FLDH4   = 0.0  
         FLDN3   = 0.0  
         FLDU    = 0.0   
         ALGACT  = 0.1 
         ALGFIX  = 0.0 
         AKILL   = .FALSE.
         IMMOBIL = .FALSE.

         IF (YRDOY .GE. LFD10) THEN
           DailyCalc = .TRUE.
         ENDIF
!      ENDIF

      RETURN
      END SUBROUTINE DRYUP

C=======================================================================
C  NRUNOF, Subroutine, U. Singh
C  Determines N loss with water over the bund
C-----------------------------------------------------------------------
C  REVISION HISTORY
C             US  Written
C  03/29/2002 CHP modular format
C=======================================================================

      SUBROUTINE NRUNOFF (
     &    FLDH4, FLDN3, FLDU, FLOOD, FRUNOFF,             !Input
     &    DLTFNO3, DLTFNH4, DLTFUREA,                     !I/O
     &    CUMFNRO, FNRO)                                  !Output

      IMPLICIT NONE

      REAL FLDU, FLDH4, FLDN3
      REAL FLOOD, FRUNOFF, FRACLOSS
      REAL DLTFUREA, DLTFNO3, DLTFNH4
      REAL FNRO, CUMFNRO

!     FRUNOFF is the loss of floodwater over the bund.
!     Floodwater N is lost from the system.
      FRACLOSS = MAX(0.0,MIN(1.0, FRUNOFF / FLOOD))
      DLTFUREA = MAX(-FLDU,  DLTFUREA - FRACLOSS * FLDU)
      DLTFNO3  = MAX(-FLDN3, DLTFNO3  - FRACLOSS * FLDN3)
      DLTFNH4  = MAX(-FLDH4, DLTFNH4  - FRACLOSS * FLDH4)

      FNRO    = FRACLOSS * (FLDU + FLDN3 + FLDH4)
      CUMFNRO = CUMFNRO + FNRO

      RETURN
      END SUBROUTINE NRUNOFF

C=======================================================================
C  PERC_N, Subroutine, U. Singh
C  Determines N fluxes with flood percolation (INFILT in mm)
C-----------------------------------------------------------------------
C  REVISION HISTORY
C             US  Written
C  02/26/2002 CHP Split into PERCOL and PERC_N to separate flood volume
C                 and chemistry.
C=======================================================================

      SUBROUTINE PERC_N (
     &    FLOOD, FLDH4, FLDN3, FLDU, INFILT, ISWNIT,      !Input
     &    DLTFUREA, DLTFNO3, DLTFNH4, DLTSNH4,            !I/O
     &    DLTSNO3, DLTUREA, DLTOXU, DLTOXN3, DLTOXH4,     !I/O
     &    CUMPERCN, PERCN)                                !Output

      USE ModuleDefs
      IMPLICIT  NONE
      SAVE

      REAL    FLDU,FLDH4,FLDN3
      REAL    DLTOXU,DLTOXN3,DLTOXH4
      REAL    FLOOD
      REAL DLTFUREA, DLTFNO3, DLTFNH4
      REAL PERCN, CUMPERCN, PERCFRAC
      REAL, DIMENSION(NL) :: DLTUREA, DLTSNO3, DLTSNH4

      CHARACTER ISWNIT*1
      REAL      PERCH4,PERCN3,PERCU,INFILT

      IF (INFILT .GT. 0.0 .AND. ISWNIT .EQ. 'Y') THEN
         PERCFRAC = INFILT / FLOOD
         PERCFRAC = MAX(0.0, MIN(1.0, PERCFRAC))

         PERCH4  = PERCFRAC * FLDH4
         PERCN3  = PERCFRAC * FLDN3
         PERCU   = PERCFRAC * FLDU

         DLTFNH4  = DLTFNH4  - PERCH4
         DLTFNO3  = DLTFNO3  - PERCN3
         DLTFUREA = DLTFUREA - PERCU

         DLTSNH4(1) = DLTSNH4(1) + PERCH4  
         DLTSNO3(1) = DLTSNO3(1) + PERCN3
         DLTUREA(1) = DLTUREA(1) + PERCU

         DLTOXU  = DLTOXU  + PERCU
         DLTOXN3 = DLTOXN3 + PERCN3
         DLTOXH4 = DLTOXH4 + PERCH4

         PERCN = PERCU + PERCN3 + PERCH4
         CUMPERCN = CUMPERCN + PERCN
      ENDIF

      END SUBROUTINE PERC_N
C=======================================================================


