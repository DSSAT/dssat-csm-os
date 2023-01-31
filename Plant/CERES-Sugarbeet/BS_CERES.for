!=======================================================================
!  COPYRIGHT 1998-2021 DSSAT Foundation
!                      University of Florida, Gainesville, Florida                  
!                      International Fertilizer Development Center
!                     
!  ALL RIGHTS RESERVED
!=======================================================================
!======================================================================
!  Various Sugarbeet Subroutines
!
!  Sugarbeet growth routine that coordinates calling of GROSUB, PHENOL and
!  ROOTGR. Prepared for the modular version of Generic CERES.
!
!----------------------------------------------------------------------
!  Revision history
!  06/15/2022 CHP Added CropStatus
!  01/26/2023 CHP Reduce compile warnings: add EXTERNAL stmts, remove 
!                 unused variables, shorten lines. 
!----------------------------------------------------------------------
!  Called : Alt_Plant
!----------------------------------------------------------------------


      SUBROUTINE BS_CERES (CONTROL, ISWITCH,              !Input
     &     EOP, HARVFRAC, NH4, NO3, SPi_AVAIL, SNOW,      !Input
     &     SOILPROP, SW, TRWUP, WEATHER, YREND, YRPLT,    !Input
     &     CANHT, CropStatus, HARVRES, KCAN, KEP, MDATE,  !Output
     &     NSTRES, PORMIN, PUptake, RLV, RWUMX, SENESCE,  !Output
     &     STGDOY, FracRts, XLAI, XHLAI)                  !Output

      USE ModuleDefs

      IMPLICIT NONE
      EXTERNAL GETLUN, HRES_CERES, PEST, BS_PHENOL, BS_GROSUB, 
     &  BS_ROOTGR, BS_OPGROW, BS_OPNIT, BS_OPHARV
      SAVE

!----------------------------------------------------------------------

      REAL            AGEFAC    
      REAL            AMTRH(TS)
      REAL            APTNUP       
      REAL            BWAH  
      REAL            CANHT        
      REAL            CANNAA    
      REAL            CANWAA   
      REAL            CANWH   
      REAL            CARBO      
      INTEGER         CDAY   
!      REAL            CNSD1    
!      REAL            CNSD2    
      REAL            CO2      
      CHARACTER*2     CROP  
      INTEGER         CropStatus 
      REAL            CUMDEP      
      REAL            CUMDTT    
      REAL            DAYL 
      REAL            DEPMAX     
      REAL            DLAYR(NL) 
      REAL            DS(NL)     
      REAL            DTT      
      REAL            DUL(NL)    
      INTEGER         DYNAMIC    
      REAL            EARS    
      REAL            EOP           
      CHARACTER*6     ERRKEY  
      REAL            ESW(NL)     
      CHARACTER*30    FILEIO
      INTEGER         FROP    
!     REAL            GDDAE
      REAL            GNUP      
      REAL            GPP    
      REAL            GPSM     
      REAL            GRNWT     
      REAL            GRORT    
      INTEGER         YREND   
      REAL            HARVFRAC(2)
      REAL            HI               
      REAL            HIP            
      INTEGER         I          
      CHARACTER*1     IDETO 
      CHARACTER*1     IPLTI     
      INTEGER         ISDATE        
      INTEGER         ISTAGE  
      CHARACTER*1     ISWWAT
      CHARACTER*1     ISWNIT  
      CHARACTER*1     ISWDIS
      REAL            KG2PPM(NL)
      INTEGER         LEAFNO 
      CHARACTER*1     IDETS    
      REAL            LL(NL)       
      INTEGER         LUNIO  
      REAL            KCAN
      REAL            KEP
      INTEGER         MDATE 
      CHARACTER*8     MODEL
      CHARACTER*10    BSSTGNAM(20) 
      REAL            NH4(NL)    
      INTEGER         NLAYR  
      REAL            NO3(NL)  
      REAL            NSTRES 
      REAL            P3          
      REAL            PCNGRN   
      REAL            PCNVEG     
      REAL            PCNL   
      REAL            PCNST     
      REAL            PCNRT
      REAL            SLPF    
      REAL            PHINT    
      REAL            PODNO      
      REAL            PODWT   
      REAL            PORMIN  
      REAL            PLTPOP    
!     REAL            PEAR
C      REAL            PRLF
!     REAL            PSTM
      REAL            PTF        
      REAL            RLV(NL)   
      REAL            RLWR      
      CHARACTER*1     RNMODE   
      REAL            ROOTN          
      INTEGER         RSTAGE   
      REAL            RTDEP      
      REAL            RTWT     
      REAL            RTWTO     
      REAL            RUE
      INTEGER         RUN  
      REAL            RWUMX   
      REAL            SAT(NL)  
      REAL            SATFAC    
      REAL            SDEPTH    
      REAL            SDWT 
      REAL            SEEDNO   
      REAL            SDWTAH  
      REAL            SHELPC    
      REAL            SHF(NL)  
      REAL            SI1(6)     
      REAL            SI3(6)   
      REAL            SKERWT    
      REAL            SLA      
      REAL            SNOW          
      REAL            SRAD     
      REAL            STOVER    
      REAL            STOVN      
      REAL            STOVWT  
      INTEGER         STGDOY(20)   
      CHARACTER*10    STNAME(20)      
      REAL            STMWTO       
      REAL            SUMDTT     
      REAL            SUMP        
      REAL            SW(NL)     
      REAL            SWFAC    
      REAL            TLNO 
      REAL            TMIN    
      REAL            TMAX        
      REAL            TOPWT      
      REAL            TRWUP     
      REAL            TSEN     
      REAL            TURFAC 
      REAL            TWILEN
      REAL            UNH4(NL)      
      REAL            UNO3(NL)    
      REAL            VSTAGE   
      REAL            WTCO    
      REAL            WTLF    
      REAL            WTLO   
      REAL            WTSO     
      REAL            WTNCAN      
      REAL            WTNVEG     
      REAL            WTNLF 
      REAL            WTNSD 
      REAL            WTNST    
      REAL            WTNUP   
      REAL            XHLAI     
      REAL            XGNP      
      REAL            XLAI          
      REAL            XN        
      REAL            XNTI     
      REAL            XSTAGE   
      REAL            YIELD    
      INTEGER         YRDOY  
      INTEGER         YREMRG   
      INTEGER         YRPLT 
      INTEGER         YRSIM    
!     REAL            Z2STAGE

!     Added by W.D.B. for pest damage at CIMMYT 4/14/2001

      REAL    AREALF,CLW,CSW,LAGSD,LNGPEG
      REAL    SLDOT,SSDOT,WLFDOT
      REAL    PHTIM(NCOHORTS)
      REAL    WTSD(NCOHORTS), SDNO(NCOHORTS)
      REAL    WTSHE(NCOHORTS), SHELN(NCOHORTS)
      REAL    SDDES(NCOHORTS)
      REAL    SWIDOT,WSHIDT,ASMDOT,DISLA,NPLTD,PPLTD
      REAL    WLIDOT,WRIDOT,WSIDOT
      INTEGER NR2

!     CHP added for P model 
      REAL PUptake(NL), SPi_AVAIL(NL), FracRts(NL)       
      REAL SeedFrac, VegFrac, PSTRES1, PSTRES2
      REAL PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed

!     Added for K model   
!     REAL SKi_AVAIL(NL)

!     ------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SoilType)    SOILPROP
      TYPE (SwitchType)  ISWITCH
      Type (ResidueType) HARVRES
      Type (ResidueType) SENESCE
      TYPE (WeatherType) WEATHER

      PARAMETER       (ERRKEY='SUGARBEET') 

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      CROP    = CONTROL % CROP
      MODEL   = CONTROL % MODEL
      FROP    = CONTROL % FROP
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE
      FILEIO  = CONTROL % FILEIO
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM

      DLAYR  = SOILPROP % DLAYR  
      DS     = SOILPROP % DS     
      DUL    = SOILPROP % DUL    
      KG2PPM = SOILPROP % KG2PPM    
      LL     = SOILPROP % LL     
      NLAYR  = SOILPROP % NLAYR  
      SAT    = SOILPROP % SAT    
      SHF    = SOILPROP % WR
      SLPF   = SOILPROP % SLPF   

      ISWWAT = ISWITCH % ISWWAT
      ISWNIT = ISWITCH % ISWNIT
      ISWDIS = ISWITCH % ISWDIS
      IPLTI  = ISWITCH % IPLTI
      IDETO  = ISWITCH % IDETO
      IDETS  = ISWITCH % IDETS

      AMTRH  = WEATHER % AMTRH
      CO2    = WEATHER % CO2
      DAYL   = WEATHER % DAYL
      SRAD   = WEATHER % SRAD
      TMAX   = WEATHER % TMAX
      TMIN   = WEATHER % TMIN
      TWILEN = WEATHER % TWILEN

      DATA BSSTGNAM /
     &  'End Juveni',   !1
     &  'Leaf  Grow',   !2
     &  'End Lf Gro',   !3
     &  'Beg Ef Gro',   !4
     &  'End Ef Gro',   !5
     &  'Maturity  ',   !6
     &  'Sowing    ',   !7
     &  'Germinate ',   !8
     &  'Emergence ',   !9
     &  '          ',   !10
     &  '          ',   !11
     &  '          ',   !12
     &  '          ',   !13
     &  'Start Sim ',   !14
     &  'End Sim   ',   !15
     &  'Harvest   ',   !16
     &  '          ',   !17
     &  '          ',   !18
     &  '          ',   !19
     &  'Harvest   '/   !20


C----------------------------------------------------------------------
C
C              Code for all Dynamic Variables
C
C----------------------------------------------------------------------

C----------------------------------------------------------------------
C
C              DYNAMIC = RUNINIT
C
C----------------------------------------------------------------------

      IF(DYNAMIC.EQ.RUNINIT) THEN
          !Define names for growth stages for screen output in CROPGRO. 
          !Not used anywhere else in CERES
          DO I = 1, 20
              STNAME(I) = '          '
              STNAME(I) = BSSTGNAM (I)    
          END DO
          
          STGDOY(14) = YRSIM

          CALL GETLUN('FILEIO', LUNIO)


          !-------------------------------------------------------------
          !Call phenology routine
          !-------------------------------------------------------------
            CALL BS_PHENOL(DYNAMIC,ISWWAT,FILEIO,IDETO,              !C
     &        CUMDEP,DAYL,DLAYR,LEAFNO,LL,NLAYR,PLTPOP,SDEPTH,        !I
     &        SNOW, SRAD,SW,TMAX,TMIN, TWILEN,                        !I
     &        XN,YRDOY,YRSIM,                                         !I
     &        CUMDTT,DTT,GPP,ISDATE, ISTAGE,MDATE,STGDOY,SUMDTT,      !O
     &        XNTI,TLNO,XSTAGE,YREMRG,RUE,KCAN,KEP, P3, TSEN, CDAY,   !O
     &        SeedFrac, VegFrac, CropStatus)                          !O


          !-------------------------------------------------------------
          !Call growth routine
          !-------------------------------------------------------------
          CALL BS_GROSUB (DYNAMIC, ISWITCH, 
     &      ASMDOT, CO2, DLAYR, DS, DTT, EOP, FILEIO,         !Input
     &      FracRts, ISTAGE, KG2PPM, LL, NLAYR, NH4, NO3,     !Input
     &      PLTPOP, PPLTD, RLV, RTDEP, RUE, SAT, SeedFrac,    !Input
     &      SHF, SLPF, SPi_AVAIL, SRAD, STGDOY, SUMDTT, SW,   !Input
     &      SWIDOT, TLNO, TMAX, TMIN, TRWUP, VegFrac,         !Input
     &      WLIDOT, WRIDOT, WSIDOT, XNTI, XSTAGE,             !Input
     &      YRDOY, YRPLT,                                     !Input
     &      EARS, GPP, MDATE,                                 !I/O
     &      CropStatus,                                       !Output
     &      AGEFAC, APTNUP, AREALF, CANHT, CANNAA, CANWAA,    !Output
     &      CANWH, CARBO, GNUP, GPSM, GRNWT, GRORT, HI, HIP,  !Output
     &      LEAFNO, NSTRES, PCNGRN, PCNL, PCNRT, PCNST,       !Output
     &      PCNVEG, PHINT, PODNO, PConc_Root, PConc_Seed,     !Output
     &      PConc_Shel, PConc_Shut, PODWT, PORMIN, PSTRES1,   !Output
     &      PSTRES2, PTF, PUptake, RLWR, ROOTN, RSTAGE, RTWT, !Output
     &      RTWTO, RWUMX, SATFAC, SDWT, SEEDNO, SENESCE,      !Output
     &      SHELPC, SI1, SI3, SKERWT, SLA, STMWTO, STOVER,    !Output
     &      STOVN, STOVWT, SUMP, SWFAC, TOPWT, TURFAC, UNH4,  !Output
     &      UNO3, VSTAGE, WTLF, WTNCAN, WTNLF, WTNSD, WTNST,  !Output
     &      WTNUP, WTNVEG, XGNP, XHLAI, XLAI, XN, YIELD)      !Output
 
          !-------------------------------------------------------------
          !Call Root routine
          !-------------------------------------------------------------

          CALL BS_ROOTGR (DYNAMIC,ISWNIT,                         !C
     &        CUMDEP,CUMDTT,DEPMAX,DLAYR,DTT,ESW,GRORT,ISTAGE,    !I
     %        LL,DUL,NO3,NH4,NLAYR,PLTPOP,PORMIN,RLWR,SAT,SDEPTH, !I
     %        SHF,STGDOY,SW,SWFAC,YRDOY,                          !I
     %        RTDEP,RLV)            

          CALL BS_OPGROW(CONTROL, ISWITCH,  
     &    CANHT, CANWH, DTT, HI, HIP, MDATE, NLAYR, NSTRES, 
     &    PCNL, PLTPOP, PODNO, PODWT, RLV, RSTAGE, !PSTRES1, PSTRES2, 
     &    RTDEP, RTWT, SATFAC, SDWT, SEEDNO, SENESCE, SHELPC, SLA, 
     &    STMWTO, SWFAC, TOPWT, TURFAC, VSTAGE, WTCO, WTLF, WTLO, 
     &    WTSO, XLAI, YRPLT)

          CALL BS_OPNIT(CONTROL, ISWITCH, 
     &    YRPLT, MDATE, NLAYR, SENESCE,
     &    WTNCAN,WTNSD,WTNVEG,PCNGRN,PCNVEG,
     &    WTNUP,WTNLF,WTNST,PCNL,PCNST,PCNRT)
       
          CALL BS_OPHARV(CONTROL, 
     &    AGEFAC, APTNUP, CANNAA, CANWAA, GNUP, GPP,      !Input
     &    GPSM,HARVFRAC, IDETO, IDETS, IPLTI, ISDATE,     !Input
     &    ISTAGE, MDATE, NSTRES, PODWT,                   !Input
     &    SEEDNO, SENESCE, SKERWT, STGDOY, STOVER, SWFAC, !Input
     &    TOPWT, TURFAC,WTNCAN, WTNUP, XLAI, XN,          !Input
     &    YIELD, YREMRG, YRPLT,                           !Input
     &    BWAH, SDWTAH)                                   !Output

          CALL PEST(CONTROL, ISWITCH, 
     &    AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, CARBO,    !Input
     &    PHTIM, PLTPOP, RTWTO, SLA, SLDOT, SOILPROP,     !Input
     &    SSDOT, STMWTO, TOPWT, WLFDOT, WTLF, YRPLT,      !Input
     &    RLV, SDNO, SHELN, SWIDOT,                       !Input/Output
     &    VSTAGE, WSHIDT, WTSD, WTSHE,                    !Input/Output
     &    ASMDOT, DISLA, NPLTD, PPLTD,                    !Output
     &    SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT)             !Output
C-----------------------------------------------------------------------
C                     DYNAMIC = SEASINIT
C-----------------------------------------------------------------------

      ELSEIF(DYNAMIC.EQ.SEASINIT) THEN

C-----------------------------------------------------------------------
C     Subroutine IPPARM reads FILEP, the PEST progress file.
C-----------------------------------------------------------------------
          IF (ISWDIS.EQ.'Y') THEN
          CALL PEST(CONTROL, ISWITCH, 
     &    AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, CARBO,    !Input
     &    PHTIM, PLTPOP, RTWTO, SLA, SLDOT, SOILPROP,     !Input
     &    SSDOT, STMWTO, TOPWT, WLFDOT, WTLF, YRPLT,      !Input
     &    RLV, SDNO, SHELN, SWIDOT,                       !Input/Output
     &    VSTAGE, WSHIDT, WTSD, WTSHE,                    !Input/Output
     &    ASMDOT, DISLA, NPLTD, PPLTD,                    !Output
     &    SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT)             !Output
          ENDIF
      
          DO I = 1, 20
              STNAME(I) = '          '
              STNAME(I) = BSSTGNAM (I)    
          END DO
          
          STGDOY(14) = YRSIM

!         CHP 5/18/2011
          MDATE      = -99      

            CALL BS_PHENOL(DYNAMIC,ISWWAT,FILEIO,IDETO,               !C
     &        CUMDEP,DAYL,DLAYR,LEAFNO,LL,NLAYR,PLTPOP,SDEPTH,        !I
     &        SNOW, SRAD,SW,TMAX,TMIN, TWILEN,                        !I
     &        XN,YRDOY,YRSIM,                                         !I
     &        CUMDTT,DTT,GPP,ISDATE, ISTAGE,MDATE,STGDOY,SUMDTT,      !O
     &        XNTI,TLNO,XSTAGE,YREMRG,RUE,KCAN,KEP, P3, TSEN, CDAY,   !O
     &        SeedFrac, VegFrac, CropStatus)                          !O

          CALL BS_GROSUB (DYNAMIC, ISWITCH, 
     &      ASMDOT, CO2, DLAYR, DS, DTT, EOP, FILEIO,         !Input
     &      FracRts, ISTAGE, KG2PPM, LL, NLAYR, NH4, NO3,     !Input
     &      PLTPOP, PPLTD, RLV, RTDEP, RUE, SAT, SeedFrac,    !Input
     &      SHF, SLPF, SPi_AVAIL, SRAD, STGDOY, SUMDTT, SW,   !Input
     &      SWIDOT, TLNO, TMAX, TMIN, TRWUP, VegFrac,         !Input
     &      WLIDOT, WRIDOT, WSIDOT, XNTI, XSTAGE,             !Input
     &      YRDOY, YRPLT,                                     !Input
     &      EARS, GPP, MDATE,                                 !I/O
     &      CropStatus,                                       !Output
     &      AGEFAC, APTNUP, AREALF, CANHT, CANNAA, CANWAA,    !Output
     &      CANWH, CARBO, GNUP, GPSM, GRNWT, GRORT, HI, HIP,  !Output
     &      LEAFNO, NSTRES, PCNGRN, PCNL, PCNRT, PCNST,       !Output
     &      PCNVEG, PHINT, PODNO, PConc_Root, PConc_Seed,     !Output
     &      PConc_Shel, PConc_Shut, PODWT, PORMIN, PSTRES1,   !Output
     &      PSTRES2, PTF, PUptake, RLWR, ROOTN, RSTAGE, RTWT, !Output
     &      RTWTO, RWUMX, SATFAC, SDWT, SEEDNO, SENESCE,      !Output
     &      SHELPC, SI1, SI3, SKERWT, SLA, STMWTO, STOVER,    !Output
     &      STOVN, STOVWT, SUMP, SWFAC, TOPWT, TURFAC, UNH4,  !Output
     &      UNO3, VSTAGE, WTLF, WTNCAN, WTNLF, WTNSD, WTNST,  !Output
     &      WTNUP, WTNVEG, XGNP, XHLAI, XLAI, XN, YIELD)      !Output
 
                         
          CALL BS_ROOTGR (DYNAMIC,ISWNIT,                         !C
     &        CUMDEP,CUMDTT,DEPMAX,DLAYR,DTT,ESW,GRORT,ISTAGE,    !I
     %        LL,DUL,NO3,NH4,NLAYR,PLTPOP,PORMIN,RLWR,SAT,SDEPTH, !I
     %        SHF,STGDOY,SW,SWFAC,YRDOY,                          !I
     %        RTDEP,RLV)            

          CALL BS_OPGROW(CONTROL, ISWITCH,  
     &    CANHT, CANWH, DTT, HI, HIP, MDATE, NLAYR, NSTRES, 
     &    PCNL, PLTPOP, PODNO, PODWT, RLV, RSTAGE, !PSTRES1, PSTRES2, 
     &    RTDEP, RTWT, SATFAC, SDWT, SEEDNO, SENESCE, SHELPC, SLA, 
     &    STMWTO, SWFAC, TOPWT, TURFAC, VSTAGE, WTCO, WTLF, WTLO, 
     &    WTSO, XLAI, YRPLT)

          CALL BS_OPNIT(CONTROL, ISWITCH, 
     &    YRPLT, MDATE, NLAYR, SENESCE,
     &    WTNCAN,WTNSD,WTNVEG,PCNGRN,PCNVEG,
     &    WTNUP,WTNLF,WTNST,PCNL,PCNST,PCNRT)


          CALL BS_OPHARV(CONTROL, 
     &    AGEFAC, APTNUP, CANNAA, CANWAA, GNUP, GPP,      !Input
     &    GPSM,HARVFRAC, IDETO, IDETS, IPLTI, ISDATE,     !Input
     &    ISTAGE, MDATE, NSTRES, PODWT,                   !Input
     &    SEEDNO, SENESCE, SKERWT, STGDOY, STOVER, SWFAC, !Input
     &    TOPWT, TURFAC,WTNCAN, WTNUP, XLAI, XN,          !Input
     &    YIELD, YREMRG, YRPLT,                           !Input
     &    BWAH, SDWTAH)                                   !Output

C----------------------------------------------------------------------
C----------------------------------------------------------------------
C
C                 DYNAMIC = RATE
C
C----------------------------------------------------------------------
C----------------------------------------------------------------------

      ELSEIF(DYNAMIC.EQ.RATE) THEN


        IF (ISWDIS.EQ.'Y') THEN
          CALL PEST(CONTROL, ISWITCH, 
     &    AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, CARBO,    !Input
     &    PHTIM, PLTPOP, RTWTO, SLA, SLDOT, SOILPROP,     !Input
     &    SSDOT, STMWTO, TOPWT, WLFDOT, WTLF, YRPLT,      !Input
     &    RLV, SDNO, SHELN, SWIDOT,                       !Input/Output
     &    VSTAGE, WSHIDT, WTSD, WTSHE,                    !Input/Output
     &    ASMDOT, DISLA, NPLTD, PPLTD,                    !Output
     &    SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT)             !Output
        ENDIF
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C
C                 DYNAMIC = INTEGRATE
C
C----------------------------------------------------------------------
C----------------------------------------------------------------------

      ELSEIF(DYNAMIC.EQ.INTEGR) THEN

        IF (ISWDIS.EQ.'Y') THEN
          CALL PEST(CONTROL, ISWITCH, 
     &    AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, CARBO,    !Input
     &    PHTIM, PLTPOP, RTWTO, SLA, SLDOT, SOILPROP,     !Input
     &    SSDOT, STMWTO, TOPWT, WLFDOT, WTLF, YRPLT,      !Input
     &    RLV, SDNO, SHELN, SWIDOT,                       !Input/Output
     &    VSTAGE, WSHIDT, WTSD, WTSHE,                    !Input/Output
     &    ASMDOT, DISLA, NPLTD, PPLTD,                    !Output
     &    SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT)             !Output
        ENDIF

          !------------------------------------------------------------
          !        Call BS_PHENOL
          !------------------------------------------------------------
        IF (YRDOY .EQ. YRPLT .OR. ISTAGE .NE. 7) THEN               
          IF (CROP .NE. 'FA') THEN
              CALL BS_PHENOL(DYNAMIC,ISWWAT,FILEIO,IDETO,             !C
     &        CUMDEP,DAYL,DLAYR,LEAFNO,LL,NLAYR,PLTPOP,SDEPTH,        !I
     &        SNOW, SRAD,SW,TMAX,TMIN, TWILEN,                        !I
     &        XN,YRDOY,YRSIM,                                         !I
     &        CUMDTT,DTT,GPP,ISDATE, ISTAGE,MDATE,STGDOY,SUMDTT,      !O
     &        XNTI,TLNO,XSTAGE,YREMRG,RUE,KCAN,KEP, P3, TSEN, CDAY,   !O
     &        SeedFrac, VegFrac, CropStatus)                          !O

	      ENDIF
          ENDIF
          !------------------------------------------------------------
          !Call BS_GROSUB
          !------------------------------------------------------------
        IF (ISTAGE .GT. 0 .AND. ISTAGE .LE. 6) THEN  

          CALL BS_GROSUB (DYNAMIC, ISWITCH, 
     &      ASMDOT, CO2, DLAYR, DS, DTT, EOP, FILEIO,         !Input
     &      FracRts, ISTAGE, KG2PPM, LL, NLAYR, NH4, NO3,     !Input
     &      PLTPOP, PPLTD, RLV, RTDEP, RUE, SAT, SeedFrac,    !Input
     &      SHF, SLPF, SPi_AVAIL, SRAD, STGDOY, SUMDTT, SW,   !Input
     &      SWIDOT, TLNO, TMAX, TMIN, TRWUP, VegFrac,         !Input
     &      WLIDOT, WRIDOT, WSIDOT, XNTI, XSTAGE,             !Input
     &      YRDOY, YRPLT,                                     !Input
     &      EARS, GPP, MDATE,                                 !I/O
     &      CropStatus,                                       !Output
     &      AGEFAC, APTNUP, AREALF, CANHT, CANNAA, CANWAA,    !Output
     &      CANWH, CARBO, GNUP, GPSM, GRNWT, GRORT, HI, HIP,  !Output
     &      LEAFNO, NSTRES, PCNGRN, PCNL, PCNRT, PCNST,       !Output
     &      PCNVEG, PHINT, PODNO, PConc_Root, PConc_Seed,     !Output
     &      PConc_Shel, PConc_Shut, PODWT, PORMIN, PSTRES1,   !Output
     &      PSTRES2, PTF, PUptake, RLWR, ROOTN, RSTAGE, RTWT, !Output
     &      RTWTO, RWUMX, SATFAC, SDWT, SEEDNO, SENESCE,      !Output
     &      SHELPC, SI1, SI3, SKERWT, SLA, STMWTO, STOVER,    !Output
     &      STOVN, STOVWT, SUMP, SWFAC, TOPWT, TURFAC, UNH4,  !Output
     &      UNO3, VSTAGE, WTLF, WTNCAN, WTNLF, WTNSD, WTNST,  !Output
     &      WTNUP, WTNVEG, XGNP, XHLAI, XLAI, XN, YIELD)      !Output

 
        ELSE
          SWFAC = 1.0
        ENDIF

          !------------------------------------------------------------
          !        Call BS_ROOTGR
          !------------------------------------------------------------
          IF (ISWWAT .EQ. 'Y') THEN

            DEPMAX = DS(NLAYR)
            CALL BS_ROOTGR (DYNAMIC,ISWNIT,                       !C
     &        CUMDEP,CUMDTT,DEPMAX,DLAYR,DTT,ESW,GRORT,ISTAGE,    !I
     %        LL,DUL,NO3,NH4,NLAYR,PLTPOP,PORMIN,RLWR,SAT,SDEPTH, !I
     %        SHF,STGDOY,SW,SWFAC,YRDOY,                          !I
     %        RTDEP,RLV)            
          ENDIF

C----------------------------------------------------------------------
C ---------------------------------------------------------------------
C
C                         DYNAMIC = OUTPUT 
C
C----------------------------------------------------------------------
C----------------------------------------------------------------------
      ELSEIF(DYNAMIC.EQ.OUTPUT) THEN
        IF (YRDOY .EQ. YREND) THEN
          STGDOY(16) = YREND
        ENDIF

        IF (YRDOY .GE. YRPLT) THEN
          CALL BS_GROSUB (DYNAMIC, ISWITCH, 
     &      ASMDOT, CO2, DLAYR, DS, DTT, EOP, FILEIO,         !Input
     &      FracRts, ISTAGE, KG2PPM, LL, NLAYR, NH4, NO3,     !Input
     &      PLTPOP, PPLTD, RLV, RTDEP, RUE, SAT, SeedFrac,    !Input
     &      SHF, SLPF, SPi_AVAIL, SRAD, STGDOY, SUMDTT, SW,   !Input
     &      SWIDOT, TLNO, TMAX, TMIN, TRWUP, VegFrac,         !Input
     &      WLIDOT, WRIDOT, WSIDOT, XNTI, XSTAGE,             !Input
     &      YRDOY, YRPLT,                                     !Input
     &      EARS, GPP, MDATE,                                 !I/O
     &      CropStatus,                                       !Output
     &      AGEFAC, APTNUP, AREALF, CANHT, CANNAA, CANWAA,    !Output
     &      CANWH, CARBO, GNUP, GPSM, GRNWT, GRORT, HI, HIP,  !Output
     &      LEAFNO, NSTRES, PCNGRN, PCNL, PCNRT, PCNST,       !Output
     &      PCNVEG, PHINT, PODNO, PConc_Root, PConc_Seed,     !Output
     &      PConc_Shel, PConc_Shut, PODWT, PORMIN, PSTRES1,   !Output
     &      PSTRES2, PTF, PUptake, RLWR, ROOTN, RSTAGE, RTWT, !Output
     &      RTWTO, RWUMX, SATFAC, SDWT, SEEDNO, SENESCE,      !Output
     &      SHELPC, SI1, SI3, SKERWT, SLA, STMWTO, STOVER,    !Output
     &      STOVN, STOVWT, SUMP, SWFAC, TOPWT, TURFAC, UNH4,  !Output
     &      UNO3, VSTAGE, WTLF, WTNCAN, WTNLF, WTNSD, WTNST,  !Output
     &      WTNUP, WTNVEG, XGNP, XHLAI, XLAI, XN, YIELD)      !Output


        ENDIF   

      CALL BS_OPGROW(CONTROL, ISWITCH,  
     &    CANHT, CANWH, DTT, HI, HIP, MDATE, NLAYR, NSTRES, 
     &    PCNL, PLTPOP, PODNO, PODWT, RLV, RSTAGE, !PSTRES1, PSTRES2, 
     &    RTDEP, RTWT, SATFAC, SDWT, SEEDNO, SENESCE, SHELPC, SLA, 
     &    STMWTO, SWFAC, TOPWT, TURFAC, VSTAGE, WTCO, WTLF, WTLO, 
     &    WTSO, XLAI, YRPLT)

      CALL BS_OPNIT(CONTROL, ISWITCH, 
     &    YRPLT, MDATE, NLAYR, SENESCE,
     &    WTNCAN,WTNSD,WTNVEG,PCNGRN,PCNVEG,
     &    WTNUP,WTNLF,WTNST,PCNL,PCNST,PCNRT)

      CALL BS_OPHARV(CONTROL, 
     &    AGEFAC, APTNUP, CANNAA, CANWAA, GNUP, GPP,      !Input
     &    GPSM,HARVFRAC, IDETO, IDETS, IPLTI, ISDATE,     !Input
     &    ISTAGE, MDATE, NSTRES, PODWT,                   !Input
     &    SEEDNO, SENESCE, SKERWT, STGDOY, STOVER, SWFAC, !Input
     &    TOPWT, TURFAC,WTNCAN, WTNUP, XLAI, XN,          !Input
     &    YIELD, YREMRG, YRPLT,                           !Input
     &    BWAH, SDWTAH)                                   !Output

      IF (ISWDIS.EQ.'Y') THEN
        CALL PEST(CONTROL, ISWITCH, 
     &    AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, CARBO,    !Input
     &    PHTIM, PLTPOP, RTWTO, SLA, SLDOT, SOILPROP,     !Input
     &    SSDOT, STMWTO, TOPWT, WLFDOT, WTLF, YRPLT,      !Input
     &    RLV, SDNO, SHELN, SWIDOT,                       !Input/Output
     &    VSTAGE, WSHIDT, WTSD, WTSHE,                    !Input/Output
     &    ASMDOT, DISLA, NPLTD, PPLTD,                    !Output
     &    SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT)             !Output
      ENDIF
C----------------------------------------------------------------------
C ---------------------------------------------------------------------
C
C                         DYNAMIC = SEASEND 
C
C----------------------------------------------------------------------
C----------------------------------------------------------------------
      ELSEIF(DYNAMIC.EQ.SEASEND) THEN

          CALL BS_GROSUB (DYNAMIC, ISWITCH, 
     &      ASMDOT, CO2, DLAYR, DS, DTT, EOP, FILEIO,         !Input
     &      FracRts, ISTAGE, KG2PPM, LL, NLAYR, NH4, NO3,     !Input
     &      PLTPOP, PPLTD, RLV, RTDEP, RUE, SAT, SeedFrac,    !Input
     &      SHF, SLPF, SPi_AVAIL, SRAD, STGDOY, SUMDTT, SW,   !Input
     &      SWIDOT, TLNO, TMAX, TMIN, TRWUP, VegFrac,         !Input
     &      WLIDOT, WRIDOT, WSIDOT, XNTI, XSTAGE,             !Input
     &      YRDOY, YRPLT,                                     !Input
     &      EARS, GPP, MDATE,                                 !I/O
     &      CropStatus,                                       !Output
     &      AGEFAC, APTNUP, AREALF, CANHT, CANNAA, CANWAA,    !Output
     &      CANWH, CARBO, GNUP, GPSM, GRNWT, GRORT, HI, HIP,  !Output
     &      LEAFNO, NSTRES, PCNGRN, PCNL, PCNRT, PCNST,       !Output
     &      PCNVEG, PHINT, PODNO, PConc_Root, PConc_Seed,     !Output
     &      PConc_Shel, PConc_Shut, PODWT, PORMIN, PSTRES1,   !Output
     &      PSTRES2, PTF, PUptake, RLWR, ROOTN, RSTAGE, RTWT, !Output
     &      RTWTO, RWUMX, SATFAC, SDWT, SEEDNO, SENESCE,      !Output
     &      SHELPC, SI1, SI3, SKERWT, SLA, STMWTO, STOVER,    !Output
     &      STOVN, STOVWT, SUMP, SWFAC, TOPWT, TURFAC, UNH4,  !Output
     &      UNO3, VSTAGE, WTLF, WTNCAN, WTNLF, WTNSD, WTNST,  !Output
     &      WTNUP, WTNVEG, XGNP, XHLAI, XLAI, XN, YIELD)      !Output

  
        CALL BS_OPGROW(CONTROL, ISWITCH,  
     &    CANHT, CANWH, DTT, HI, HIP, MDATE, NLAYR, NSTRES, 
     &    PCNL, PLTPOP, PODNO, PODWT, RLV, RSTAGE, !PSTRES1, PSTRES2, 
     &    RTDEP, RTWT, SATFAC, SDWT, SEEDNO, SENESCE, SHELPC, SLA, 
     &    STMWTO, SWFAC, TOPWT, TURFAC, VSTAGE, WTCO, WTLF, WTLO, 
     &    WTSO, XLAI, YRPLT)

        CALL BS_OPNIT(CONTROL, ISWITCH, 
     &    YRPLT, MDATE, NLAYR, SENESCE,
     &    WTNCAN,WTNSD,WTNVEG,PCNGRN,PCNVEG,
     &    WTNUP,WTNLF,WTNST,PCNL,PCNST,PCNRT)

        CALL BS_OPHARV(CONTROL, 
     &    AGEFAC, APTNUP, CANNAA, CANWAA, GNUP, GPP,      !Input
     &    GPSM,HARVFRAC, IDETO, IDETS, IPLTI, ISDATE,     !Input
     &    ISTAGE, MDATE, NSTRES, PODWT,                   !Input
     &    SEEDNO, SENESCE, SKERWT, STGDOY, STOVER, SWFAC, !Input
     &    TOPWT, TURFAC,WTNCAN, WTNUP, XLAI, XN,          !Input
     &    YIELD, YREMRG, YRPLT,                           !Input
     &    BWAH, SDWTAH)                                   !Output

        CALL HRes_Ceres(CONTROL,
     &    CROP, DLAYR, GRNWT, HARVFRAC, NLAYR,            !Input
     &    PConc_Shut, PConc_Root, PConc_Shel,             !Input
     &    PConc_Seed, PLTPOP, PODWT, RLV, ROOTN,          !Input
     &    RTWT, SENESCE, STOVN, STOVWT, WTNSD,            !Input
     &    HARVRES)                                        !Output

        !Set senescence variable to zero for next season
        SENESCE % ResWt    = 0.0
        SENESCE % ResLig = 0.0
        SENESCE % ResE     = 0.0

      ENDIF

      RETURN
      END SUBROUTINE BS_CERES

C----------------------------------------------------------------------
C
C           Variable Definitions
C
C----------------------------------------------------------------------

! AGEFAC      !Nitrogen stress factor affecting cell expansion 
! ANO3(20)    !Total extractable nitrate N in soil profile (kg N/ha)    
! ANH4(20)    !Total extractable ammonium N in soil profile (kg N/ha) 
! APTNUP      !Nitrogen in stover (above ground biomass) kg N/ha
! AREALF     Area of leaves (one side) per unit ground area
!              (cm2[leaf] / m2[ground])
! ASMDOT     Daily assimilative damage (g[CH2O] /m2 / d)
! CANHT       !Canopy height, m (currently not calculated)
! CANNAA      !Stover N at anthesis, g N/M2    
! CANWAA      !Canopy weight at anthesis, g/m2  
! CANWH            !Canopy width, m (currently not calculated)
! CARBO       !Daily biomass production, g/plant/day  
! CLW        Cumulative leaf growth (g[leaf]/m2)
! CNSD1       !Cum. water stress on photosynthesis during growth stage    
! CNSD2       !Cumulative water stress on growth during growth stage  
! CO2         !Atmospheric CO2, ppm  
! CO2X(10)    !CO2 effect on photosynthesis, X axis is CO2 level, ppm
! CO2Y(10)    !CO2 effect on photosynthesis, Y axis is relative effect 
! CROP        !Two character representation of crop (ie. BS)
! CSW        Cumulative stem growth (g[stem]/m2)
! CTYPE       !Crop type (Always 1 for maize)
! CUMDEP      !Cum. soil depth, cm 
! CUMDTT      !Cum. growing degree days, degrees C    
! DAP         !Days after planting
! DEPMAX      !Depth of soil, cm
! DISLA       Diseased leaf area (cm2[leaf]/m2[ground]/d)
! DLAYR(L)    Soil thickness in layer L (cm)
! DS(20)      !Depth of soil in layer, cm  
! DTT         !Growing degree days today, degrees C    
! DUL(20)     !Drained upper limite, cm3/cm3 
! DYNAMIC     !Main control variable to tell each module which section of code to run
! EARS        !Ears per plant, computed here and used in grosub.  
! EOP         !Potential plant transpiration, mm/day
! ERRKEY      !Key or error messages
! ERRNUM      !Error number to print to screen in case of error
! ESW(20)     !Extractable soil water, cm3 water/cm3 soil  
! FILEC       !Path and name of species file (ie. *.spe)
! FILEIO      !Name if input file (ie. DSSAT45.INP)               
! FOUND        !Flag used to warn user if information in an input file does not exist 
! G2          !Potential leaf growth rate(cm2/cm2)
! GNUP        !Total grain N uptake, kg N/ha
! GPP         !Grain number per plant, grains/plant 
! GPSM        !Grain numbers, grains/m2 
! GRNWT       !Grain weight, g/plan
! GRORT       !Root growth rate, g/plant/day
! YREND       !Year and day of year for harvest
! HI          !Harvest index, Seed weight/above ground plant weight
! HIP         !Harvest index of ear, (g ear + seed weight)/(g above ground plant weight)
! I           !Index counter
! IDETO       !Switch for printing overview.out file
! IPLTI       !
! ISDATE      !Year and day of year for end of leaf growth
! ISTAGE      !Growth stage
! ISWDIS      !Disease switch
! ISWWAT      !Water balance switch
! ISWNIT      !Nitrogen balance switch 
! KG2PPM(20)  !Factor that convergs mg elemental N/kg soil to kg N/ha for soil layer L
! LAGSD      Not used in CERES except to pass 0 into pest.for
! LAI         !Leaf area index, cm2/cm2 
! LEAFNO      !Number of oldest leaf per plant (same as XN)
! IDETS       !Code to generate several output files
! L           !Index counter
! LINC        !Line number of input file
! LNGPEG      Not used in CERES except to pass 0 into pest.for
! LL(20)      !Volumetric lower limit of soil water holding capacity, cm3 water/cm3 soil
! LNUM        !Line number in an input datafile
! LUNIO       !Logical unit number for file
! MDATE       !Maturity data, year and day of year
! BSSTGNAM(20)!Array containing names of various growth stages
! NH4(20)     !Ammonium in soil layer L, mg elemental N/kg soil 
! NLAYR       !Number of soil layers
! NPEST       !Number of pests defined in the crop pest file
! NPLTD      Number of plants destroyed (#/m2/d)
! NO3(20)     !Nitrate in soil layer L (mg elemental N/kg soil) 
! NSTRES      !Nitrogen stress factor affecting growth (0-1) 
! P1          !Growing degree days (base 8C) from seedling emergence to end of Juvenile phase    
! P2          !Photoperiod sensitivity coefficient, 1/hr    
! PATHCR  !Path to species filee
! PCNGRN      !Nitrogen content in grain, %
! PCNVEG      !Percent nitrogen in vegetative tissue (leaf and stem), kg N/ha  
! PCNL        !Percent nitrogen in leaf tissue, %
! PCNST       !Percent of nitrogen in stems, %
! PCNRT       !Percent of nitrogen in roots, %
! PCPID(40,6) !Pest coupling point identification code for pest i and couping point j
! PCTID(I)   Pest damage characterization method for pest I: (1) absolute 
!              daily damage rate, (2) percent observed damage, (3) daily 
!              percent damage rate
! PDCF1(I,J) Pest damage coefficient asscociated with pest I, coupling 
!              point J 
! PID(I)     Pest identification header from FILE
! PL(I)             Pest level for pest I today
! PHINT       !Phylochron interval. Number of GDD for new leaf emergence, degrees C 
! PHTIM      Not used in CERES except to pass 0 into pest.for
! PLTPOP      !Plant Population, Pl/m  
! PODNO       !Ear number, #/m2
! PODWT       !Pod (ear) weight, g/m2
! PORMIN      !Minimum pore volume before soil water saturation effects growth (not used), cm3/cm3    
! PPLTD      Percent plants destroyed  (%/m2/d)
! PTF         !Ratio of above ground biomass to total biomass 
! RLV(20)     !Root length volume of soil layer, cm3 root/cm3 soil
! RLWR        !Root length to weight ration, cm/g   
! RNMODE    !Run mode
! ROPT        !Second optimum temperature for development from species file, 
! RSTAGE        !Growth stage (same as ISTAGE), 1-6.
! RTDEP       !Root depth, cm 
! RTWT        !Root weight, g/plant 
! RTWTO       !Root weight, g/m2
! RUN      !Run number
! RWUMX       !Maximum root water uptake parameter from species file  
! SAT(20)     !Saturated water holding capacity for soil layer, cm3 water/cm3 soil
! SATFAC      !Reduction of growth due to water logging (0-1.0)
! SDDES(J)   Number of seeds destroyed today in cohort J when shells are 
!              not destroyed (#/m2/day)
! SDEPTH      !Sowing depth, cm        
! SDNO(J)    Need to figure out how to use this array to pass in seed number???
! SDWT        !Seed (grain) weight, g/m2
! SECTION !Variable indicating which section to look for in an input file
! SEEDNO           !Seed number, #/m2
! SHELPC           !Shelling percentage (grain weight/ear weight)
! SHELN(J)   Number of shells for cohort J (#/m2)
! SHF(20)     !Soil hospitality factor, unitless    
! SI1(6)      !Water stress during a growth stage used for output           
! SI3(6)      !Nitrogen stress during a growth stage used for output
! SKERWT      !Weight per kernel, g/kernel 
! SLA              !Specific leaf area, cm2/g
! SLDOT      Defoliation due to daily leaf senescence (g/m2/day)
! SNOW             !Snow accumulation today, mm
! SRAD        !Total solar radiation today, MJ/m2  
! SSDOT      Not used in CERES except to pass 0 into pest.for
! STOVER           !Stover weight (leaf+stem), kg/ha
! STGDOY(20)   !Array storing the dates that different growth stages occurred
! STNAME(20)      !Array containing names of various growth stages
! STMWTO           !Stem weight, g/m2
! SUMDTT      !Sum of growing degree days since stage initiation, C  
! SUMP        !Cumulative plant growth during ISTAGE 4, g/plant 
! SW(20)      !Volumetric soil water content of soil layer, cm3 water/cm3 soil   
! SWFAC       !Soil water stress effect on growth (0-1), 1 is no stress, 0 is full stress
! SWIDOT     Daily seed mass damage (g/m2/day)
! TBASE       !Base temperature below which no development occurs, C    
! TLNO        !Total number of leaves that the plant produces
! TMIN        !Minimum temperature today, C    
! TMAX        !Maximum temperature today, C 
! TOPT        !Optimum temperature for development (from species file), C
! TOPWT            !Total above ground biomass, g/m2
! TRWUP            !Total root water uptake, mm/day
! TURFAC      !Soil water stress effecting cell expansion
! UNH4(20)         !Plant uptake of ammonium from layer (kg N/ha/day)
! UNO3(20)         !Plant uptake of nitrate from a layer (kg N/ha/day)
! VSTAGE           !Vegetative growth stage (number of leaves)
! WLFDOT     Leaf weight losses due to freezing (g[leaf]/m2-d)
! WLIDOT     Daily pest or freeze damage to leaf mass (g/m2/day)
! WMODB*1 !Switch to indicate if weather has been modified in file X (Warning****** not passed in***)
! WRIDOT     Daily pest damage to root mass (g/m2/day)
! WSHIDT     Weight of shell tissue consumed by pests today (g[shell]/m2-d)
! WSIDOT     Daily pest damage to stem mass (g/m2/day)
! WTCO        !Cumulative loss of plant tissue, g/m2  (not computed)  
! WTLF        !Leaf weight, g/m2
! WTLO        !Cumulative loss of leaf tissue, g/m2 (not computed)    
! WTSD(J)    Seed mass  for cohort J (g/m2)
! WTSO        !Cumulative stem loss of stem tissue, g/m2 (not computed) 
! WTHADJ(2,8) !Used on BS_phenol but not passed into maize.for ???????  
! WTNCAN      !Weight of nitrogen in above ground biomass (stem, leaf, grain), kg N/ha    
! WTNVEG      !Weight of nitrogen in vegetative tissue, kg N/ha    
! WTNLF       !Weight of nitrogen in leaf tissue, kg N/h    
! WTNSD       !Weight of nitrogen in seed, g[N] / m2[ground]
! WTNST       !Weight of nitrogen in stem tissue, kg N/ha     
! WTNUP       !Total nitrogen uptake, g/m2  
! WTSHE(J)   Shell mass  for cohort J (g/m2)
! XHLAI       !Healthy leaf area, cm2/cm2  
! XGNP        !Nitrogen content of grain, %
! XLAI             !Leaf area index, m2/m2
! XLAT        !Latutude  
! XN          !Number of oldest expanding leaf    
! XNTI        !Number of leaves at tassel initiation
! XSTAGE      !Non-integer growth stage indicator    
! YR       !Year
! YIELD       !Yield in kg/ha at 0% moisture content
! YRDOY    !Current year and day of year
! YREMRG   !Year and day of year of emergence 
! YRPLT    !Year and day of year of planting
! YRSIM    !Year and day of year of first day of simulation


