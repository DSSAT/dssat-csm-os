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

      Subroutine Aloha_Pineapple(DYNAMIC)
      USE AlohaMod

      IMPLICIT NONE
      SAVE

      REAL      PGFAC3,EFINOC,EFNFIX,STHETA(3),XLONG,
     +          SDSOIL(3),AIRAMT,XLAT,SNDN,SNUP,SDRATE,TOPWT,
     +          AINO3,AINH4,WTNSD,PBIOMS,WTNFX,WTNCAN,WTNUP,SDWTAM
      REAL      FDINT,HBPC(3),TDAY,RHUM,XELEV,WTHADJ(2,8)
      REAL      SEEDNI,WTNLF,WTNST,WTNSH,WTNRT,WTNLO
      LOGICAL   FEXIST

      INTEGER*2 IP

      INTEGER   NAPNIT,I,NAP,NAPRES
      REAL      TOTIR,CANNAA,CANWAA,TRUNOF,TDRAIN,AMTNIT,TLCH,TNOX
      REAL      CUMDTT,SUMDTT,DTT
      REAL      SDWTAH,BWAH

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
      CHARACTER*6, PARAMETER :: ERRKEY='ALOHA' 
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
!      YRDOY   = CONTROL % YRDOY
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
      !ISWWAT = ISWITCH % ISWWAT
      !ISWNIT = ISWITCH % ISWNIT
      !ISWDIS = ISWITCH % ISWDIS
      !IPLTI  = ISWITCH % IPLTI
      !IDETO  = ISWITCH % IDETO
      !IDETS  = ISWITCH % IDETS
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
      LN     = 0.0
      FLRWT  = 0.0
      FRTWT  = 0.0
      CRWNWT = 0.0
      SKWT   = 0.0
      GROSK  = 0.0
      YIELD  = 0.0
      SENLA  = 0.0
      SLAN   = 0.0
      CARBO  = 0.0
      GRNWT  = 0.0
      RTWT   = 0.0
      LFWT   = 0.0
      STMWT  = 0.0
      SDWTAH = 0.0
      SDWTAM = 0.0
      WTNUP  = 0.0
      TOPWT  = 0.0
      BWAH   = 0.0
      WTNLF  = 0.0
      WTNST  = 0.0
      WTNSH  = 0.0
      WTNRT  = 0.0
      WTNLO  = 0.0
      GPSM   = 0.0
      GPP    = 0.0
      PTF    = 0.0

      DO I = 1, 8
         TMFAC1(I) = 0.931 + 0.114*I-0.0703*I**2+0.0053*I**3
      END DO

      DO I = 1, 20
         RLV(I) = 0.0
      END DO

      BIOMAS = 0.0
      LEAFNO = 0.0
      LAI    = 0.0
      XN     = 0.0
      ICSDUR = 0
      NSTRES = 1.0
      AGEFAC = 1.0
      NDEF3  = 1.0
      NDEF4  = 1.0
      ANFAC  = 0.0
      NFAC   = 1.0
      ATANC  = 0.0
      TANC   = 0.044
      RANC   = 0.0
      VANC   = 0.0
      VMNC   = 0.0
      TMNC   = 0.0
      RCNP   = 0.0
      TCNP   = 0.0
      TLCH   = 0.0
      TNOX   = 0.0
      SEEDNI = 0.0
      STOVN  = 0.0
      ROOTN  = 0.0
      GRAINN = 0.0
      GNP    = 0.0
      XGNP   = 0.0
      APTNUP = 0.0
      GNUP   = 0.0
      TOTNUP = 0.0
      CUMDTT = 0.0
      SUMDTT = 0.0
      DTT    = 0.0
      CANNAA = 0.0
      CANWAA = 0.0
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

      CALL Aloha_IPPlant () !formerly call to IPIBS
      
      WTINITIAL = SDWTPL/(PLTPOP*10.0)        ! kg/ha  --> g/plt

      CALL Aloha_IPCROP (FILEC,PATHCR)

C-----------------------------------------------------------------------
C     Call PHENOLGY initialization routine
C-----------------------------------------------------------------------

      CALL Aloha_PHENOL (DYNAMIC, STGDOY,YRDOY,XLAT)


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
               CALL ROOTGR (ISWNIT,GRORT)
            ENDIF
         ENDIF

C-----------------------------------------------------------------------
C        Call GROSUB
C----------------------------------------------------------------------
      
         IF (ISTAGE .LT. 6) THEN
            IF (CROP .EQ. 'PI') THEN
               CALL GROSUB  (NOUTDO,ISWNIT,IDETO)
            ENDIF
         ENDIF

         IF (YRDOY .EQ. STGDOY(3)) THEN
            CANNAA = STOVN*PLTPOP
            CANWAA = BIOMAS
         ENDIF

C-----------------------------------------------------------------------
C        Call PHENOL
C-----------------------------------------------------------------------

         IF (YRDOY .EQ. YRPLT .OR. ISTAGE .NE. 7) THEN
            CALL PHENOL (DYNAMIC, STGDOY,YRDOY,XLAT)
         ENDIF

!=======================================================================
C        Call daily output subroutine
C-----------------------------------------------------------------------
      CASE (OUTPUT)
!=======================================================================

         CALL OPDAY (MODEL,TRTNO,DAP,YRSIM,YRPLT,NREP,TITLET,EXPER,
     &               ENAME,MULTI,CROPD,STGDOY,YRDOY,TOTIR,NYRS,DAS,
     &               DAYL,TRUNOF,TDRAIN,AMTNIT,NAP,HAREND,NPSTAP,
     &               STNAME,WTNLF,WTNST,WTNSD,WTNSH,WTNRT)



!=======================================================================
C     Call end of season output routine
C-----------------------------------------------------------------------
      CASE (SEASEND)
!=======================================================================

      CALL OPHARV (TRTNO,YRDOY,YRSIM,YRPLT,CROP,CROPD,
     &   WTNSD,NAP,TOTIR,CRAIN,CET,TRUNOF,PESW,TDRAIN,TSON,TSOC,TLCH,
     &   NAPNIT,ISDATE,MDATE,YIELD,SKERWT,GPSM,GPP,MAXLAI,
     &   PBIOMS,STOVER,XGNP,TOTNUP,APTNUP,GNUP,BIOMAS,
     &   NYRS,FLDNAM,EXPER,WTNFX,WTNCAN,TSIN,WTNUP,NREP,AMTNIT,
     &   SDWTAM,TITLET,STGDOY,ENAME,SDWT,
     &   ROTNO,ROTOPT,CRPNO,SDRATE,TOPWT,AMTRES,HBPC,FBIOM,EYEWT,
     &   PMDATE,FHDATE,WTINITIAL,BWAH,SDWTAH,
     &   TSEDM,TRON,TOTPST,H2OLOS,SEPLOS,CHMCOD,ISENS)


!=======================================================================
      END SELECT
!=======================================================================
      END Subroutine Aloha_Pineapple
!=======================================================================
