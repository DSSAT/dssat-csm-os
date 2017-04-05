C=======================================================================
C   Subroutine Aloha_Pineapple
C
C   ALOHA-PINEAPPLE MODEL (formerly PIALO980.EXE)
C
C   August 1997
C
C
C   Aloha-Pineapple model developed by Jingbo Zhang and Duane Bartholomew
C   using some routines of ceres-maize model developed by Ritchie,Kiniry,
C   Jones,Kneivel,Singh and others in July, 1988. 
C   IBSNAT DSSAT I/O structures adapted from Soygro by C. Zickos
C   and D. Godwin, ifdc.
C
C   Version 3.5 has :
C
C   1. Population effects on leaf growth, fruit development,
C      and fruit yield.
C   2. Weather effects on growth and development.
C   3. Initial plant size effect on growth.
C   4. Plant size at the time of forcing on fruit size and fruit yield.
C   5. Water balance and nitrogen balance have not been tested.
C
!----------------------------------------------------------------------
!  Revision history
!  (see history above)
!  02/07/1993 PWW Header revision and minor changes   
!  02/24/1993 BDB Changed call to WATBAL (Added AIRAMT)
!  03/22/2017 CHP Adpated for CSM v4.6
C=======================================================================

      Subroutine Aloha_Pineapple(CONTROL, ISWITCH, 
     &    SOILPROP, SW, WEATHER, YRPLT,   !Input
     &    MDATE)      !Output
      USE Aloha_mod

      IMPLICIT NONE
      SAVE

!      REAL      TOPWT,WTNUP,SDWTAM
      REAL      FDINT
!      REAL      SEEDNI,WTNLF,WTNST,WTNSH,WTNRT,WTNLO

      CHARACTER*1 ISWWAT, ISWNIT
      INTEGER EDATE, ISTAGE, YRDOY, YRPLT, I, MDATE
!      INTEGER ICSDUR, 
      INTEGER, DIMENSION(20) :: STGDOY
      REAL    LN, CRWNWT, FRTWT  
!      REAL    FLRWT, GROSK, YIELD, SENLA, SLAN
      REAL    LFWT, STMWT, GPSM, GPP
!      REAL    CARBO, PTF, LEAFNO, XN, AGEFAC, NDEF3, NDEF4
      REAL    BASLFWT, BIOMAS, LAI, NSTRES, STOVN, RTDEP, RTWT, SKWT
!      REAL    ANFAC, NFAC, ATANC, TANC, RANC, VANC, VMNC, TMNC, RCNP
!      REAL    TCNP, ROOTN, GRAINN, GNP, XGNP, APTNUP, GNUP, TOTNUP
      REAL    GRORT, XSTAGE
      REAL, DIMENSION(6)  :: SI1, SI2, SI3, SI4
      REAL, DIMENSION(NL) :: SW
      REAL, DIMENSION(NL) :: RLV

      REAL      CANNAA,CANWAA
      REAL      DTT
!      REAL    CUMDTT,SUMDTT,SDWTAH,BWAH, 
      REAL      XLAT
!temp      REAL      EP, EP1, TRWU, RWUEP1
      REAL      SWFAC, TURFAC, TRNU

      REAL      PLTPOP,   TBASE

!     ------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SoilType)    SOILPROP
      TYPE (SwitchType)  ISWITCH
!      Type (ResidueType) HARVRES
!      Type (ResidueType) SENESCE
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
      ISWNIT = ISWITCH % ISWNIT


      !
      ! Initialze stress indices
      !
      DO I = 1, 6
         SI1(I) = 0.0
         SI2(I) = 0.0
         SI3(I) = 0.0
         SI4(I) = 0.0
      END DO

C-----------------------------------------------------------------------
C     Call IPIBS .. Read in IBSNAT31.INP file
C-----------------------------------------------------------------------

      CALL Aloha_IPPlant (CONTROL) !formerly call to IPIBS
      
      CALL Aloha_IPCROP ()

      PLTPOP = PLANTING % PLTPOP

C-----------------------------------------------------------------------
C     Call PHENOLGY initialization routine
C-----------------------------------------------------------------------

      CALL Aloha_GROSUB  (CONTROL, 
     &    DTT, ISTAGE, SWFAC, TBASE, TURFAC, WEATHER,        !Input
     &    BASLFWT, BIOMAS, CRWNWT, FRTWT, GPP, GPSM, LAI,    !Output
     &    LFWT, LN, NSTRES, RLV, RTWT, SKWT, STMWT, TRNU)    !Output

      CALL Aloha_PHENOL (CONTROL, ISWITCH,
     &    SW, WEATHER, SOILPROP,          !Input
     &    DTT, ISTAGE, MDATE, STGDOY, TBASE, XSTAGE)   !Output

      CALL Aloha_OpGrow (CONTROL, ISWITCH,  
     &   BASLFWT, BIOMAS, CRWNWT, FRTWT, GPP, GPSM, ISTAGE, 
     &   LAI, LFWT, LN, MDATE, NSTRES, PLTPOP, RLV, RTDEP,  
     &   RTWT, SKWT, STMWT, SWFAC, TRNU, TURFAC, YRPLT)     



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

         EDATE = STGDOY(9)

!temp chp
!!     Calculate daily SW stress factors.
      SWFAC = 1.0
      TURFAC = 1.0
!      IF (EP .GT. 0.0) THEN
!        EP1 = EP*0.1
!        IF (TRWU/EP1 .LT. RWUEP1) TURFAC = (1./RWUEP1)*TRWU/EP1
!        IF (EP1 .GE. TRWU) THEN
!          SWFAC = TRWU / EP1
!          EP = TRWU * 10.0
!        ENDIF
!      ENDIF
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
C        Generic WATer BALance routine implemented in CERES and CROPGRO
C-----------------------------------------------------------------------

         IF (ISWWAT .EQ. 'Y') THEN
            !
            !  WRESR growth and depth routine
            !
            IF (GRORT .GT. 0.0) THEN
   !            CALL ROOTGR (ISWNIT,GRORT)
              RTDEP = 0.0  !TEMPORARY UNTIL THIS COMES FROM ROOTGR
            ENDIF
         ENDIF

C-----------------------------------------------------------------------
C        Call PHENOL
C-----------------------------------------------------------------------

        IF (YRDOY .EQ. YRPLT .OR. ISTAGE .NE. 7) THEN
           CALL Aloha_PHENOL (CONTROL, ISWITCH, 
     &    SW, WEATHER, SOILPROP,          !Input
     &    DTT, ISTAGE, MDATE, STGDOY, TBASE, XSTAGE)  !Output
        ENDIF

C-----------------------------------------------------------------------
C        Call GROSUB
C----------------------------------------------------------------------
      
!        IF (ISTAGE .LT. 6) THEN
           CALL Aloha_GROSUB  (CONTROL, 
     &    DTT, ISTAGE, SWFAC, TBASE, TURFAC, WEATHER,        !Input
     &    BASLFWT, BIOMAS, CRWNWT, FRTWT, GPP, GPSM, LAI,    !Output
     &    LFWT, LN, NSTRES, RLV, RTWT, SKWT, STMWT, TRNU)    !Output
!        ENDIF

         IF (YRDOY .EQ. STGDOY(3)) THEN
            CANNAA = STOVN*PLTPOP
            CANWAA = BIOMAS
         ENDIF

!=======================================================================
C        Call daily output subroutine
C-----------------------------------------------------------------------
      CASE (OUTPUT)
!=======================================================================

      CALL Aloha_OpGrow (CONTROL, ISWITCH,  
     &   BASLFWT, BIOMAS, CRWNWT, FRTWT, GPP, GPSM, ISTAGE, 
     &   LAI, LFWT, LN, MDATE, NSTRES, PLTPOP, RLV, RTDEP,  
     &   RTWT, SKWT, STMWT, SWFAC, TRNU, TURFAC, YRPLT)     

!=======================================================================
C     Call end of season output routine
C-----------------------------------------------------------------------
      CASE (SEASEND)
!=======================================================================
      CALL Aloha_OpGrow (CONTROL, ISWITCH,  
     &   BASLFWT, BIOMAS, CRWNWT, FRTWT, GPP, GPSM, ISTAGE, 
     &   LAI, LFWT, LN, MDATE, NSTRES, PLTPOP, RLV, RTDEP,  
     &   RTWT, SKWT, STMWT, SWFAC, TRNU, TURFAC, YRPLT)     


!      CALL OPHARV (TRTNO,YRDOY,YRSIM,YRPLT,CROP,CROPD,
!     &   WTNSD,NAP,TOTIR,CRAIN,CET,TRUNOF,PESW,TDRAIN,TSON,TSOC,TLCH,
!     &   NAPNIT,ISDATE,MDATE,YIELD,SKERWT,GPSM,GPP,MAXLAI,
!     &   PBIOMS,STOVER,XGNP,TOTNUP,APTNUP,GNUP,BIOMAS,
!     &   NYRS,FLDNAM,EXPER,WTNFX,WTNCAN,TSIN,WTNUP,NREP,AMTNIT,
!     &   SDWTAM,TITLET,STGDOY,ENAME,SDWT,
!     &   ROTNO,ROTOPT,CRPNO,SDRATE,TOPWT,AMTRES,HBPC,FBIOM,EYEWT,
!     &   PMDATE,FHDATE,WTINITIAL,BWAH,SDWTAH,
!     &   TSEDM,TRON,TOTPST,H2OLOS,SEPLOS,CHMCOD,ISENS)


!=======================================================================
      END SELECT
!=======================================================================
      END Subroutine Aloha_Pineapple
!=======================================================================
