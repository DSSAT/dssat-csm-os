!=======================================================================
!   Subroutine Aloha_Pineapple
!
!   ALOHA-PINEAPPLE MODEL (formerly PIALO980.EXE)
!
!   August 1997
!
!
!   Aloha-Pineapple model developed by Jingbo Zhang and Duane Bartholomew
!   using some routines of ceres-maize model developed by Ritchie,Kiniry,
!   Jones,Kneivel,Singh and others in July, 1988. 
!   IBSNAT DSSAT I/O structures adapted from Soygro by C. Zickos
!   and D. Godwin, ifdc.
!
!   Version 3.5 has :
!
!   1. Population effects on leaf growth, fruit development,
!      and fruit yield.
!   2. Weather effects on growth and development.
!   3. Initial plant size effect on growth.
!   4. Plant size at the time of forcing on fruit size and fruit yield.
!   5. Water balance and nitrogen balance have not been tested.
!
!----------------------------------------------------------------------
!  Revision history
!  (see history above)
!  02/07/1993 PWW Header revision and minor changes   
!  02/24/1993 BDB Changed call to WATBAL (Added AIRAMT)
!  03/22/2017 CHP Adpated for CSM v4.6
C=======================================================================

      Subroutine Aloha_Pineapple(CONTROL, ISWITCH, 
     &    EOP, HARVFRAC, NH4, NO3, SOILPROP, SW, TRWUP,   !Input
     &    WEATHER, YRPLT,                                 !Input
     &    LAI, MDATE, RLV, SENESCE, STGDOY, UNH4, UNO3)   !Output

      USE Aloha_mod
      IMPLICIT NONE
      SAVE

!      REAL      TOPWT,WTNUP,SDWTAM
      REAL      FDINT
!      REAL      SEEDNI,WTNLF,WTNST,WTNSH,WTNRT,WTNLO

      CHARACTER*1 ISWWAT
      INTEGER EDATE, ISTAGE, YRDOY, YRPLT, MDATE, ISDATE, PMDATE
!      INTEGER ICSDUR, 
      INTEGER, DIMENSION(20) :: STGDOY
      REAL HARVFRAC(2)
      REAL    LN, CRWNWT, FRTWT  
!      REAL    FLRWT, GROSK, YIELD, SENLA, SLAN
      REAL    LFWT, STMWT, GPSM, GPP
!      REAL    CARBO, PTF, LEAFNO, XN, AGEFAC, NDEF3, NDEF4
      REAL    BASLFWT, BIOMAS, LAI, NSTRES, STOVN, RTDEP, RTWT, SKWT
!      REAL    ANFAC, NFAC, ATANC, TANC, RANC, VANC, VMNC, TMNC, RCNP
!      REAL    TCNP, GRAINN, GNP, XGNP, APTNUP, GNUP, TOTNUP
      REAL    GRORT, XSTAGE, STOVWT, ROOTN, WTNUP
!     REAL, DIMENSION(6)  :: SI1, SI2, SI3, SI4
      REAL, DIMENSION(NL) :: NH4, NO3, RLV, SW, UNH4, UNO3

      REAL      CANNAA,CANWAA
      REAL      DTT
      REAL    CUMDTT, SUMDTT      !,SDWTAH,BWAH, 
      REAL      XLAT
      REAL      EOP, EP1, TRWUP, RWUEP1
      REAL      SWFAC, TURFAC, TRNU, TEMPM

      REAL      PLTPOP,   TBASE, STOVER, WTINITIAL, YIELD
      REAL    WTNCAN, WTNGRN

!     ------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SoilType)    SOILPROP
      TYPE (SwitchType)  ISWITCH
!      Type (ResidueType) HARVRES
      Type (ResidueType) SENESCE
      TYPE (WeatherType) WEATHER

!      CHARACTER*1 RNMODE
      INTEGER DYNAMIC !, NLAYR, FROP, RUN
!      REAL, DIMENSION(NL) :: DLAYR, DUL, LL, SAT

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
!      CROP    = CONTROL % CROP
!      MODEL   = CONTROL % MODEL
!      FROP    = CONTROL % FROP
!      RUN     = CONTROL % RUN
!      RNMODE  = CONTROL % RNMODE
!      FILEIO  = CONTROL % FILEIO
      YRDOY   = CONTROL % YRDOY
!      YRSIM   = CONTROL % YRSIM

      !DLAYR  = SOILPROP % DLAYR  
      !DS     = SOILPROP % DS     
      !DUL    = SOILPROP % DUL    
      !KG2PPM = SOILPROP % KG2PPM    
      !LL     = SOILPROP % LL     
      !NLAYR  = SOILPROP % NLAYR  
      !SAT    = SOILPROP % SAT    
      !SHF    = SOILPROP % WR
      !SLPF   = SOILPROP % SLPF   
      !
      !AMTRH  = WEATHER % AMTRH
      !CO2    = WEATHER % CO2
      !DAYL   = WEATHER % DAYL
      !SRAD   = WEATHER % SRAD
      !TMAX   = WEATHER % TMAX
      !TMIN   = WEATHER % TMIN
      !TWILEN = WEATHER % TWILEN

!=======================================================================
      SELECT CASE (DYNAMIC)
!=======================================================================
C     Initialize
C-----------------------------------------------------------------------
      CASE (RUNINIT, SEASINIT)
!=======================================================================
C     Call PLANT initialization routine to set variables to 0
C-----------------------------------------------------------------------
      XLAT   = WEATHER % XLAT

      ISWWAT = ISWITCH % ISWWAT

      CUMDTT = 0.0

      !!
      !! Initialze stress indices
      !!
      !DO I = 1, 6
      !   SI1(I) = 0.0
      !   SI2(I) = 0.0
      !   SI3(I) = 0.0
      !   SI4(I) = 0.0
      !END DO

      SWFAC = 1.0
      TURFAC = 1.0

C-----------------------------------------------------------------------
C     Call IPIBS .. Read in IBSNAT31.INP file
C-----------------------------------------------------------------------

      CALL Aloha_IPPlant (CONTROL) !formerly call to IPIBS
      
      CALL Aloha_IPCROP ()

      PLTPOP = PLANTING % PLTPOP
      RWUEP1 = SPECIES % RWEP

C-----------------------------------------------------------------------
C     Call PHENOLOGY initialization routine
C-----------------------------------------------------------------------

      CALL Aloha_PHENOL (CONTROL, ISWITCH,
     &    SW, WEATHER, SOILPROP,                              !Input
     &    DTT, EDATE, ISDATE, ISTAGE, MDATE, PMDATE,          !Output
     &    STGDOY, SUMDTT, TBASE, TEMPM, XSTAGE)               !Output

      CALL Aloha_GROSUB  (CONTROL, ISWITCH, 
     &    DTT, ISTAGE, NH4, NO3, SOILPROP, SW, SWFAC,         !Input
     &    SUMDTT, TBASE, TURFAC, WEATHER, XSTAGE,             !Input
     &    BASLFWT, BIOMAS, CRWNWT, FRTWT, GPP, GPSM, GRORT,   !Output
     &    LAI, LFWT, LN, NSTRES, RLV, ROOTN, RTWT, SENESCE,   !Output
     &    SKWT, STMWT, STOVER, STOVN, STOVWT, TEMPM,          !Output
     &    UNH4, UNO3, WTNUP, WTINITIAL, YIELD)                !Output

      CALL Aloha_ROOTGR (CONTROL,
     &     CUMDTT, DTT, GRORT, ISTAGE, ISWITCH, NO3, NH4,     !Input
     &     SOILPROP, SW, SWFAC,                               !Input
     &     RLV, RTDEP, RTWT)                                  !Output

      CALL Aloha_OpGrow (CONTROL, ISWITCH,  
     &   BASLFWT, BIOMAS, CRWNWT, FRTWT, GPP, GPSM, ISTAGE, 
     &   LAI, LFWT, LN, MDATE, NSTRES, PLTPOP, RLV, ROOTN,  
     &   RTDEP, RTWT, SKWT, STMWT, STOVN, STOVWT, SWFAC,    
     &   TRNU, TURFAC, WTNCAN, WTNGRN, WTNUP, YRPLT)     

      CALL Aloha_OPHARV(CONTROL, ISWITCH, 
     &   BIOMAS, CRWNWT, GPSM, GPP, HARVFRAC, ISDATE,     !Input
     &   LAI, LN, MDATE, PMDATE, STGDOY, STOVER,          !Input
     &   WTINITIAL, WTNCAN, WTNGRN, WTNUP, YIELD,         !Input
     &   YRDOY, YRPLT)                                    !Input

!=======================================================================
C     Beginning of daily simulation loop
C-----------------------------------------------------------------------
      CASE (RATE)
!=======================================================================
C        Define dates for water balance calculations
C
C        YRSIM = Start of Simulation Date
C        YRPLT = Planting Date
C        EDATE = Emergence Date
C        MDATE = Maturity Date
C        YRDOY = Year - Day of Year (Dynamic Variable)
C-----------------------------------------------------------------------
!         EDATE = STGDOY(9)

!temp chp
!!     Calculate daily SW stress factors.
          SWFAC  = 1.0
          TURFAC = 1.0
          IF(ISWWAT.NE.'N' .and. YRDOY >= EDATE) THEN
             IF (EOP .GT. 0.0) THEN
                EP1 = EOP * 0.1
                IF (TRWUP / EP1 .LT. RWUEP1) THEN
                   TURFAC = (1./RWUEP1) * TRWUP / EP1
                ENDIF
                IF (EP1 .GE. TRWUP) THEN
                  SWFAC = TRWUP / EP1
                ENDIF
             ENDIF
          ENDIF

!      CSD1   = CSD1 + 1.0 - SWFAC
!      CSD2   = CSD2 + 1.0 - TURFAC
!      ICSDUR = ICSDUR + 1
!     IF (ISWWAT .NE. 'N') THEN
!         SI1(ISTAGE) = CSD1  / ICSDUR
!         SI2(ISTAGE) = CSD2  / ICSDUR
!         SI3(ISTAGE) = CNSD1 / ICSDUR
!         SI4(ISTAGE) = CNSD2 / ICSDUR
!      ENDIF

C-----------------------------------------------------------------------
C        Calculate light interception for transpiration.
C        Note: this is discontinues function at LAI = 3.
C        Consider CROPGRO approach with KCAN later.
C-----------------------------------------------------------------------

         IF (LAI .LE. 3.0) THEN
           FDINT = 1.0 - EXP(-LAI)
         ELSE
           FDINT = 1.0
         ENDIF
C        FDINT = 1.0 - EXP(-(KCAN+0.1)*XHLAI)

C-----------------------------------------------------------------------

!         IF (ISWWAT .EQ. 'Y') THEN
            !
            !  WRESR growth and depth routine
            !
   !         IF (GRORT .GT. 0.0) THEN
              CALL Aloha_ROOTGR (CONTROL,
     &     CUMDTT, DTT, GRORT, ISTAGE, ISWITCH, NO3, NH4,     !Input
     &     SOILPROP, SW, SWFAC,                               !Input
     &     RLV, RTDEP, RTWT)                                  !Output
   !         ENDIF
!         ENDIF

C-----------------------------------------------------------------------
C        Call GROSUB
C----------------------------------------------------------------------
      
!        IF (ISTAGE .LT. 6) THEN
           CALL Aloha_GROSUB  (CONTROL, ISWITCH, 
     &    DTT, ISTAGE, NH4, NO3, SOILPROP, SW, SWFAC,         !Input
     &    SUMDTT, TBASE, TURFAC, WEATHER, XSTAGE,             !Input
     &    BASLFWT, BIOMAS, CRWNWT, FRTWT, GPP, GPSM, GRORT,   !Output
     &    LAI, LFWT, LN, NSTRES, RLV, ROOTN, RTWT, SENESCE,   !Output
     &    SKWT, STMWT, STOVER, STOVN, STOVWT, TEMPM,          !Output
     &    UNH4, UNO3, WTNUP, WTINITIAL, YIELD)                !Output
!        ENDIF

         IF (YRDOY .EQ. STGDOY(3)) THEN
            CANNAA = STOVN*PLTPOP
            CANWAA = BIOMAS
         ENDIF

C-----------------------------------------------------------------------
C        Call PHENOL
C-----------------------------------------------------------------------

        IF (YRDOY .EQ. YRPLT .OR. ISTAGE .NE. 7) THEN
           CALL Aloha_PHENOL (CONTROL, ISWITCH, 
     &    SW, WEATHER, SOILPROP,                              !Input
     &    DTT, EDATE, ISDATE, ISTAGE, MDATE, PMDATE,          !Output
     &    STGDOY, SUMDTT, TBASE, TEMPM, XSTAGE)               !Output
        ENDIF

        CUMDTT = CUMDTT + DTT

!=======================================================================
C     Integration
C-----------------------------------------------------------------------
      CASE (INTEGR)
!=======================================================================
        CALL Aloha_ROOTGR (CONTROL,
     &     CUMDTT, DTT, GRORT, ISTAGE, ISWITCH, NO3, NH4,     !Input
     &     SOILPROP, SW, SWFAC,                               !Input
     &     RLV, RTDEP, RTWT)                                  !Output
     
        CALL Aloha_GROSUB  (CONTROL, ISWITCH, 
     &    DTT, ISTAGE, NH4, NO3, SOILPROP, SW, SWFAC,         !Input
     &    SUMDTT, TBASE, TURFAC, WEATHER, XSTAGE,             !Input
     &    BASLFWT, BIOMAS, CRWNWT, FRTWT, GPP, GPSM, GRORT,   !Output
     &    LAI, LFWT, LN, NSTRES, RLV, ROOTN, RTWT, SENESCE,   !Output
     &    SKWT, STMWT, STOVER, STOVN, STOVWT, TEMPM,          !Output
     &    UNH4, UNO3, WTNUP, WTINITIAL, YIELD)                !Output

!=======================================================================
C        Call daily output subroutine
C-----------------------------------------------------------------------
      CASE (OUTPUT)
!=======================================================================

      CALL Aloha_OpGrow (CONTROL, ISWITCH,  
     &   BASLFWT, BIOMAS, CRWNWT, FRTWT, GPP, GPSM, ISTAGE, 
     &   LAI, LFWT, LN, MDATE, NSTRES, PLTPOP, RLV, ROOTN,  
     &   RTDEP, RTWT, SKWT, STMWT, STOVN, STOVWT, SWFAC,    
     &   TRNU, TURFAC, WTNCAN, WTNGRN, WTNUP, YRPLT)     

      CALL Aloha_OPHARV(CONTROL, ISWITCH, 
     &   BIOMAS, CRWNWT, GPSM, GPP, HARVFRAC, ISDATE,     !Input
     &   LAI, LN, MDATE, PMDATE, STGDOY, STOVER,          !Input
     &   WTINITIAL, WTNCAN, WTNGRN, WTNUP, YIELD,         !Input
     &   YRDOY, YRPLT)                                    !Input

!=======================================================================
C     Call end of season output routine
C-----------------------------------------------------------------------
      CASE (SEASEND)
!=======================================================================
      CALL Aloha_OpGrow (CONTROL, ISWITCH,  
     &   BASLFWT, BIOMAS, CRWNWT, FRTWT, GPP, GPSM, ISTAGE, 
     &   LAI, LFWT, LN, MDATE, NSTRES, PLTPOP, RLV, ROOTN,  
     &   RTDEP, RTWT, SKWT, STMWT, STOVN, STOVWT, SWFAC,    
     &   TRNU, TURFAC, WTNCAN, WTNGRN, WTNUP, YRPLT)     

      CALL Aloha_OPHARV(CONTROL, ISWITCH,
     &   BIOMAS, CRWNWT, GPSM, GPP, HARVFRAC, ISDATE,     !Input
     &   LAI, LN, MDATE, PMDATE, STGDOY, STOVER,          !Input
     &   WTINITIAL, WTNCAN, WTNGRN, WTNUP, YIELD,         !Input
     &   YRDOY, YRPLT)                                    !Input

!=======================================================================
      END SELECT
!=======================================================================
      END Subroutine Aloha_Pineapple
!=======================================================================
