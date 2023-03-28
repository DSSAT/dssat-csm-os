C=======================================================================
C  TR_GROSUB, Subroutine
C
C  Determines taro growth
C-----------------------------------------------------------------------
C  Revision history
C
C  
C  12/16/2004 CHP/US/RO Converted to modular format for inclusion in CSM.
!  06/15/2022 CHP Added CropStatus
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  FIELD  : Switch to indicate field simulation of all processes but
C           when false then growth without stress as in seedbed
C  LTRANS : True if transplanted (itrans=2 or 3)
C=======================================================================

      SUBROUTINE TR_GROSUB (CONTROL, ISWITCH, 
     &    BF, CARBC1, CARBC2, CARBC3, 
     &    CARBR1, CARBR2, CARBR3, CARBR4, CARBL0,
     &    CARBL1, CARBL2, CARBL3, CARBL4, CMAT2, CMAT5,     !Input 
     &    CO2, CDTT_TP, CUMDTT, DTT, EMAT, FIELD, FLOOD, 
     &    FLOODN, G2, G3, G4, ISTAGE, ITRANS, KLITE,
     &    LEAF1, LEAF3, LEAFT, LTRANS, MCORMWT, MLFWT, 
     &    MPETWT, NEW_PHASE, NH4, NO3, P1, P1T, P3,  
     &    P4, PCGRD, PCINT, PETGR1, PETGR2, PETGR5, 
     &    PHINT, PLAMAX, PLANTS, PLTPOP, PORMIN,
     &    RCONST, RCTFAC, RESPF, RGCORM2, RLV, RNINT,
     &    RPET, RSENCE, RTDEP, RUEA,
     &    SDEPTH, SDTT_TP, SEEDRV, SEEDNI, SLA3, SLA4,  
     &    SLA5,  SLAIFC, SLFCGRD, SLFNGRD, SLFNINT, 
     &    SLFTGRD,SLFTINT, SLFWGRD, SLFWINT,  
     &    SLGRD1, SLGRD2, SOILPROP, SRAD, ST,  
     &    SUFAC2, SUFAC3, SUMDTT, SW, SWFAC, 
     &    TBASE, TCARBC2, TF_GRO, TGRAD, TIPGRAD,  !TAGE, 
     &    TIPGRAD3, TIPINT, TMAX, TMIN, TURFAC, !WSTRES, 
     &    XNFGRAD, XNFINT, XTNMAX, XSTAGE, XST_TP,        !Input
     &    YRPLT, YRSOW,                  
     &    AGEFAC, APTNUP, BIOMAS, CANHT, CANNAA, CANWAA,  !Output
     &    CORMNUP, CORMLNO, CORMN, CORMWT, DYIELD, GRORT, !Output
     &    LAI, LEAFNO, LFWT, MAXLAI, NSTRES, PBIOMS,      !Output
     &    PCORMN, RLWR, ROOTN, RTWT, RWUEP1, RWUMX,       !Output
     &    STOVER, STOVN, TANC, TGROCOM, TILNO, TOTNUP,    !Output
     &    UNH4, UNO3, MDATE, WTNUP,                       !Output
     &    CARBO, CropStatus,                              !Output
     &    PETWT)                              !addition RMO

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
      USE FloodModule    ! parameters, hourly weather data.

      IMPLICIT  NONE
      EXTERNAL YR_DOY, TR_IPGROSUB, TR_IPCROP, TR_NFACTO, TR_CALCSHK, 
     &  TR_TILLSUB, TR_NUPTAK, TR_PlantInit, TR_TRNSPL_GROSUB
      EXTERNAL TABEX, ERROR
      SAVE

      CHARACTER*1 ISWWAT, ISWNIT
      CHARACTER*2 CROP
      CHARACTER*12 FILEC
      CHARACTER*80 PATHCR
      INTEGER   I,YEAR, CropStatus
!     Variable conversion RUEA = ceff
       
      REAL      SWFAC,TURFAC
      REAL      NSINK,NPOOL1,NPOOL2,NPOOL,NSDR,TEMF
      REAL      TTMP,PC,TI,XTN,A
      REAL      SLFW,SLFT,SLFN,GRF,TCARB1,TILCAR
      REAL      RCNFIL,RMNC
      REAL      XNF,TNLAB,RNLAB,RNOUT,SLFC,PLAS
      REAL      TABEX,PCO2,Y1,YSTOVWT,YRTWT
      REAL      MGROPET, MGROCOM, MGROTOP, PCORMN   !, PLAC
      REAL      PLAMAX, RCONST, RCTFAC, RESPF, RGCORM2, RGCORMT, RNINT
      REAL      RGCORM, RPET, RSENCE, RUEA, RUEX, RMPLA, RMPLA1 !, RMAT
      REAL      SENLAM, SENLAT !, SENLAMY, SENLATY
      REAL      SLA3, SLA4, SLA5, SLAMP, SLA1  !slan1
      REAL      SLAIFC, SLAT1, SLAT2, SLAT5, SUCINT, SUCX1, SUCX2, SUCX3
      REAL      SLFCGRD, SLFNGRD, SLFNINT, SLFTGRD
      REAL      SLFTINT, SLFWGRD, SLFWINT 
      REAL      SLGRD1, SLGRD2  !, SLWFIN, SLWMAIN
      REAL      SUFAC2, SUFAC3
      REAL      TCARBC1, TCARBC2, TCARBC3, TCARBL2, TCARBL5, TCLRAT1
      REAL      TCMAT, TCORMWT, TOPSBIO, TOTROOT, TPETWT
      REAL      TGROPET, TGROCOM, TTOP
      REAL      TGRAD, TIPGRAD, TIPGRAD3, TIPINT, TROOT, XNFGRAD 
      REAL      XNFINT, XTNMAX, YCUMNLOS, YSLAN 
      REAL      CANHT

      INTEGER DOY, DYNAMIC, EMAT, MDATE
      INTEGER ISTAGE, ITRANS
      INTEGER L, LEAFNO, MODELVER, NLAYR, YRDOY, YRPLT, YRSOW
      INTEGER IPERIOD, NOWSTDAY, ISTOP 
      INTEGER LEAF1, LEAF3, LEAFT 

      REAL    ASSIM
      REAL    BF, BFG4, BGRAD, BGRAD2, BINT, BINT2, CARBC1, CARBC2
      REAL    CARBC, CARBC3, CARBL, CARBR, CLEAF 
      REAL    CARBR1, CARBR2, CARBR3, CARBR4, CARBL0
      REAL    CARBL1, CARBL2, CARBL3, CARBL4, CMAT2, CMAT5  
      REAL    CMPART, CMFAC5, CORMLNO, CORMN, CORMNUP, CORMWT
      REAL    CUMNLOS, DEADLF, DEADPT
      REAL    DSTOVNLOS, GROCOM, GRFMIN
      REAL    KLITE, KSHADE
      REAL    PCGRD, PCINT, PETGR1, PETGR2, PETGR5 
      REAL    PETWT, PETGR
 

      REAL ABIOMS, AGEFAC, APTNUP, ATEMP, BIOMAS
      REAL CANNAA, CANWAA, CARBO, CO2, CDTT_TP, CUMDEP, CUMDTT, CUMPH
      REAL DTT, DYIELD, FLOOD
      REAL G2, G3, G4
      REAL GRORT, LAI, LFWT
      REAL MAXLAI, MCORMWT, MGROLF, MLFWT, MPLA, MPLAG
      REAL MPETWT, NDEF3, NFAC, NPPH, NSTRES, P1, P1T, P3, P4, P5
      REAL PAR, PBIOMS, PCARB, PDWI, PGRORT
      REAL PHINT, PLA, PLANTS, PLPH, PLTPOP, PORMIN, PRFT, PTF
      REAL RANC, RCNP, RESERVE, RLWR
      REAL ROOTN, ROWSPC, RTDEP, RTR, RTWT, RWUEP1, RWUMX , WTNUP
      REAL SDEPTH, SDTT_TP
      REAL SDWTPL, SEEDNI, SEEDRV, SENLA
      REAL SLAN, SRAD, STOVER, STOVN !, SNLFWT, 
      REAL STOVWT, SUMDTT
      REAL TANC, TBASE, TCARBO, TCNP, TEMPM !, TAGE
      REAL TGROLF, TPLA
      REAL TILNO, TLFWT, TMAX, TMIN, TMNC, TOTNUP
      REAL TPLAG, TPLANTS, TRLOS, TRNLOS, TSHOCK
      REAL VANC, VMNC !, WSTRES
      REAL XANC, XN, XSTAGE, XST_TP
      REAL AVNSTRS, AVWSTRS
      REAL P8, P9, KROW, SROW, HARVMAT, BMIN, BMIN2, SLA2, CARBC4
      REAL SLINT, SLGRD5, SUFAC1, TREDUCE, YIELD    !,SLAM, SLAT

      REAL TMFAC1(8)
      REAL  CO2X(10), CO2Y(10)
      REAL, DIMENSION(NL) :: DLAYR, RLV, RNLOSS, NH4, NO3
      REAL, DIMENSION(NL) :: ST, SW, UNH4, UNO3

      LOGICAL FIELD, LTRANS, NEW_PHASE, TF_GRO, FIRST

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

!     The variable "ISWITCH" is of type "SwitchType".
      TYPE (SwitchType) ISWITCH

!     The variable "SOILPROP" is of type "SoilType".
      TYPE (SoilType) SOILPROP
      TYPE (FloodNType) FLOODN

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY

      ISWWAT = ISWITCH % ISWWAT
      ISWNIT = ISWITCH % ISWNIT

      DLAYR = SOILPROP % DLAYR
      NLAYR = SOILPROP % NLAYR

      CALL YR_DOY(YRDOY, YEAR, DOY)

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      CALL TR_IPGROSUB (CONTROL, 
     &    ATEMP, CROP, FILEC, P1,P3,P4,P5,
     &    G2,G3,G4,PHINT,PCINT,PCGRD, PATHCR, 
     &    PLANTS, PLPH, PLTPOP, ROWSPC, SDWTPL)

      CALL TR_IPCROP (FILEC, PATHCR,  
     &    CO2X, CO2Y, MODELVER, PORMIN, 
     &    RWUEP1, RWUMX, P1, P1T,
     &    TBASE,P8,P9,PHINT,PCGRD,PCINT,
     &    KLITE,KROW,SROW,RUEA,LEAF1,LEAF3,
     &    LEAFT,KSHADE,HARVMAT,
     &    BF,TIPINT,TIPGRAD,TIPGRAD3,XTNMAX,
     &    PLAMAX,BMIN,BMIN2,BINT,BINT2,BGRAD,BGRAD2,
     &    SLA1,SLA2,SLA3,SLA4,SLA5,SLAT1,SLAT2, SLAT5,
     &    CARBR1,CARBR2,CARBR3,CARBR4,CARBL0,
     &    CARBL1,CARBL2,CARBL3,CARBL4,CARBC1,
     &    CARBC2,CARBC3,CARBC4,TCLRAT1,TCARBL2,TCARBL5,
     &    PETGR1,PETGR2,PETGR5,RGCORM,RGCORM2,
     &    RGCORMT,CMAT2,CMAT5,TCMAT,
     &    SUFAC1,SUFAC2,SUFAC3,SLGRD1,SLGRD2,
     &    SLINT,SLGRD5,SUCINT,SUCX1,SUCX2,SUCX3,
     &    RESPF,RSENCE,RPET,TCARBC1,TCARBC2,TCARBC3,
     &    RNINT,RCTFAC,TGRAD,RCONST,XNFINT,XNFGRAD,
     &    SLFWINT,SLFWGRD,SLFNINT,SLFNGRD,SLAIFC,
     &    SLFCGRD,SLFTINT,SLFTGRD,
     &    SEEDRV,MCORMWT,MPETWT,MLFWT,RLWR)

      SEEDRV   = AMAX1 (SEEDRV,0.0)
      RTWT     = 0.001
      MLFWT    = AMAX1 (MLFWT,0.001)
      TLFWT    = 0.0
      LFWT     = MLFWT + TLFWT
      MPETWT   = AMAX1 (MPETWT,0.1) 
      TPETWT   = 0.0
      PETWT    = MPETWT + TPETWT
      MCORMWT  = AMAX1 (MCORMWT,0.0)
      TCORMWT  = 0.0
      CORMWT   = MCORMWT + TCORMWT
      STOVWT   = LFWT    + PETWT
      BIOMAS   = STOVWT  + CORMWT
      LEAFNO   = 0

      LAI      = 0.0  !chp 8/12/2003 - prevents early N stress
      MAXLAI   = 0.0
      PTF      = 0.0
      TILNO    = 0.0
      PLA      = 0.01
      MPLA     = 0.01
      TPLA     = 0.0
      MPLAG    = 0.0
      TPLAG    = 0.0
      SLAN     = 0.0    
      !SNLFWT   = 0.0    
      SENLA    = 0.0 
      DEADLF   = 0.0
      !SLAM     = 0.0
      !SLAT     = 0.0
      SENLAM   = 0.0
      SENLAT   = 0.0

      MGROPET  = 0.0
      TGROPET  = 0.0
      TGROLF   = 0.0
      MGROLF   = 0.0
      GRORT    = 0.0
      MGROCOM  = 0.0
      TGROCOM  = 0.0
      TCARBO   = 0.0
      CARBO    = 0.0

      CUMPH    = 0.333

      CANHT    = 0.0
      CORMN    = 0.0
      CORMLNO  = 0.0
     

      DYIELD   = 0.0 
      STOVER   = 0.0 
      SEEDNI   = 0.0 
      CANNAA   = 0.0 
      CANWAA   = 0.0 
      TOTNUP   = 0.0 
      CORMNUP  = 0.0 
      APTNUP   = 0.0 
      PCORMN   = 0.0 
      IPERIOD  = 0
!     ADDED TO PREVENT PREMATURE END IN TRT FOLLOWING ABORTED RUN
      ISTOP    = 0          
C
C     RMNC INITIALIZED TO PREVENT GROWTH DIFFERENCES IN SIMULATIONS RAN IN MULTI- AND SINGLE-TREATMENT MODE US/RO MAR 26 2014
C
      RMNC = 0.0
C

      CALL TR_NFACTO(DYNAMIC, FIELD, XSTAGE, 
     &    AGEFAC, NDEF3, NFAC, NSTRES, RCNP, TANC, TCNP, TMNC)  !Output

      IF (ISWNIT .EQ. 'Y') THEN
        TANC  = 0.040
        RANC  = 0.015
        ROOTN = RANC * RTWT
        STOVN = STOVWT * TANC
      ELSE
        TANC  = 0.0
        STOVN = 0.0
        ROOTN = 0.0
        XANC  = 0.0
      ENDIF


      DO I = 1, 8
         TMFAC1(I) = 0.931+0.114*I-0.0703*I**2+0.0053*I**3
      END DO

      CALL TR_CALCSHK (DYNAMIC, ISTAGE, ISWWAT, LTRANS,  !Input
     &    YRDOY, YRSOW, TROOT, TMAX, TSHOCK)             !Output 

      CALL TR_TILLSUB (DYNAMIC, ISTAGE, 
     &    BGRAD, BGRAD2, BINT, BINT2, 
     &    DTT, FLOOD, G2, G3, KSHADE, LEAFT, LAI,         !Input
     &    NSTRES, P3, P4, P5, PETGR2, PETGR5, RGCORMT, 
     &    RTR, SLAT1, SLAT2, SLAT5, SRAD, SUCINT, SUCX1, 
     &    SUCX2, SUCX3, SUMDTT, TCARBC1, TCARBC2, TCARBC3, 
     &    TCARBL2, TCARBL5, TCARBO, TCLRAT1, TCMAT, 
     &    TCORMWT, TGROCOM, TILNO, TLFWT, TMIN, TMAX,     !Output
     &    TPETWT,  TPLA, TURFAC, XN)                      !Output

      CALL TR_NUPTAK (DYNAMIC, 
     &    FLOOD, NH4, NO3, PDWI, PGRORT, PLANTS, PTF,     !Input
     &    RCNP, RLV, RTWT, SOILPROP, ST, STOVWT, SW, TCNP,!Input
     &    FLOODN, STOVN, RANC, ROOTN, TANC,               !I/O
     &    RNLOSS, TRNLOS, UNH4, UNO3 , WTNUP)             !Output

      FIRST = .TRUE.
      CUMNLOS = 0.0
      TOPSBIO = 0.0
      TOTROOT = 0.0
      
!***********************************************************************
!***********************************************************************
!     Daily rate / integration calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------

      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL TR_PlantInit(
     &    CORMN, NPPH, PLPH, ROOTN, STOVN, PLANTS, PLTPOP, 
     &    ITRANS, SEEDNI, TPLANTS)
      ENDIF

!     MAKE SURE THAT GROSUB IS CALLED UNDER THESE CONDITIONS! - CHP
      IF (YRDOY .EQ. YRPLT .AND.(ITRANS .EQ. 2 .OR. ITRANS .EQ. 3)) THEN
        CALL TR_TRNSPL_GROSUB (DYNAMIC, SOILPROP,  
     &    CDTT_TP, FIELD, ISWWAT, ISWNIT, ITRANS, P1T,    !Input
     &    PHINT, PLANTS, SDTT_TP, SDWTPL, TPLANTS, XST_TP,!Input
     &    AGEFAC, BIOMAS, CUMPH, ISTAGE, LAI, MLFWT,      !Output
     &    MPLA, MPETWT, NDEF3, NFAC, NSTRES, RCNP, ROOTN, !Output
     &    RTDEP, RTWT, SDEPTH, SEEDRV, STOVN, STOVWT,     !Output
     &    TANC, TCNP, TLFWT, TMNC, TPLA, TRLOS, TREDUCE,  !Output
     &    MCORMWT)                                        !Output

     
       IF (ISWNIT .EQ. 'Y') THEN
          APTNUP     = STOVN*10.0*PLANTS   
          XANC       = STOVN/STOVWT*100.0  
        ENDIF
      ENDIF

      !Initialize for new stage set today
      IF (NEW_PHASE) THEN

        SELECT CASE (ISTAGE)
        CASE (1)
          LEAFNO = 1      !GROSUB
!CHP 1/11/2005 - Default canopy height for soil temperature routine
          CANHT = 0.5 

          IF (ISWWAT .EQ. 'Y') THEN
            DO L = 1, NLAYR
              RLV(L) = 0.0 
!              RWU(L) = 0.0   ROOTWU, not here.
            END DO

            CUMDEP = 0.0
            DO L = 1, NLAYR
              CUMDEP = CUMDEP + DLAYR(L)
              RLV(L) = AMIN1(0.20*PLANTS/DLAYR(L),0.5) 
              IF (CUMDEP .GT. RTDEP) then
                EXIT
              ELSE
                RLV(L) = RLV(L)*(1.0-(CUMDEP-RTDEP)/DLAYR(L))  
              ENDIF          
              IF (L .EQ. NLAYR) THEN
                IF (CUMDEP .LE. RTDEP) THEN
                  RTDEP = CUMDEP  !ROOTGR, TRANSPL
                  EXIT
                ENDIF
              ENDIF
            END DO
          ENDIF
        
        CASE (2,3)
            IF (LEAFNO .GT. LEAFT) THEN    
             LEAFT = LEAFNO
          ENDIF

        CASE (4)  
!     For taro determined at end of stage 3 (2,3) 
!     For tanier end of stage 4 so must be in CASE (5)

          PTF    = 1.0    
          MAXLAI = LAI
          ABIOMS = BIOMAS*PLANTS
          CANWAA = ABIOMS
          CANNAA = ABIOMS*TANC
C-------------------------------

        CASE (5)
C

          VANC    = TANC
          VMNC    = TMNC

        CASE (6)

        CASE(8)       !germination
          RTDEP  = SDEPTH     !FROM PHENOL

        CASE (20)
          DYIELD  = CORMWT*10.0*PLANTS
          YIELD   = DYIELD/0.33
          STOVER  = (BIOMAS*PLANTS*10.)-DYIELD
          CORMLNO = (TILNO + 1.0)*PLANTS
          IF (ISWNIT .EQ. 'Y') THEN
             IF (CORMWT .GT. 0.0) THEN
                PCORMN     =(CORMN/CORMWT)*100.0
                CORMNUP  = CORMN*10.0*PLANTS
             ENDIF
             TOTNUP = CORMNUP + STOVN*10.0*PLANTS
          ENDIF
        END SELECT
      ENDIF

      IF (ISWNIT .EQ. 'Y') THEN
         XANC   = TANC*100.0           
         APTNUP = STOVN*10.0*PLANTS    
      ENDIF

      IF (PLANTS .LE. 0.0 .OR. ISTAGE .GE. 6 .OR. .NOT. TF_GRO) THEN
        AGEFAC = 1.0
        NSTRES = 1.0
        UNO3 = 0.0
        UNH4 = 0.0
        RETURN
      ENDIF

      IF (ISWNIT .EQ. 'Y' .AND. ISTAGE .LT. 7) THEN
        CALL TR_NFACTO(DYNAMIC, FIELD, XSTAGE, 
     &    AGEFAC, NDEF3, NFAC, NSTRES, RCNP, TANC, TCNP, TMNC)  !Output
      ENDIF

C     TARO

      IPERIOD  = IPERIOD + 1
      AVNSTRS = (AVNSTRS*(IPERIOD-1)+AGEFAC)/IPERIOD
      AVWSTRS = (AVWSTRS*(IPERIOD-1)+TURFAC)/IPERIOD
      IF (ABS(AGEFAC - 1.0) < 1.E-4) THEN
          NOWSTDAY = NOWSTDAY + 1
          IF (NOWSTDAY .GE. 15) THEN
             AVNSTRS = 1.0
          ENDIF
       ELSE
          NOWSTDAY = 0
      ENDIF    
      IF (ABS(TURFAC - 1.0) < 1.E-4) THEN
         NOWSTDAY = NOWSTDAY + 1
         IF (NOWSTDAY .GE. 15) THEN
            AVWSTRS = 1.0
         ENDIF
       ELSE
         NOWSTDAY = 0
      ENDIF    

      PAR     = 0.5*SRAD     !PAR local variable
      Y1      = EXP(-KLITE*LAI)
      RUEX    = 0.65 
      PCARB   = RUEA*PAR**RUEX/PLANTS*(1.0-Y1) 
      !
      ! Calculate Photosynthetic Response to CO2

      PCO2  = TABEX (CO2Y,CO2X,CO2,10)
      PCARB = PCARB*PCO2
      PRFT  = 1.02-0.003*((0.25*TMIN+0.75*TMAX)-28.0)**2   
      !VERIFIED JAPAN J. CROP SCI 47(3):425-430. 1978  
      PRFT  = AMAX1 (PRFT,0.0)
      IF (PRFT .GT. 1.0) THEN
          PRFT = 1.0                                
      ENDIF

      !
      ! Calculate Transplanting Shock
      !
      CALL TR_CALCSHK (DYNAMIC, ISTAGE, ISWWAT, LTRANS,  !Input
     &    YRDOY, YRSOW, TROOT, TMAX, TSHOCK)             !Output

      CARBO = PCARB*AMIN1(PRFT,SWFAC,NSTRES,AVNSTRS,AVWSTRS,1.0)

      IF (CARBO .GT. 0.0) THEN 
          ISTOP = 0
       ELSE
          IF (SEEDRV .LE. 0.0 .AND. ISTAGE .GT. 7) THEN 
!            STOPS ONLY BEFORE EMERGE
             ISTOP = ISTOP + 1
          ENDIF
      ENDIF

    
      TEMF  = 1.0
      TEMPM = (TMAX + TMIN)*0.5

      IF (TMIN .LT. 14.0 .OR. TMAX .GT. 32.0) THEN
         IF (TMAX .LT. TBASE) THEN
            TEMF = 0.0
         ENDIF
         IF (ABS(TEMF) > 1.E-4) THEN
            TEMF = 0.0
            DO I = 1, 8
               TTMP = TMIN + TMFAC1(I)*(TMAX-TMIN)
               IF (TTMP .GT. 14.0  .AND. TTMP .LE. 32.0) THEN
                   TEMF = TEMF + 1.0/8.0
               ENDIF
               IF (TTMP .GE. TBASE .AND. TTMP .LT. 14.0) THEN
                   TEMF = TEMF + 0.0210*(TTMP-TBASE)
               ENDIF
               IF (TTMP .GT. 32.0  .AND. TTMP .LT. 42.0) THEN
                   TEMF = TEMF + 0.0125*(42.0-TTMP)
               ENDIF
            END DO
         ENDIF
      ENDIF

!     Calculate radiation to temperature ratio for tillering and senescence
      IF (DTT .GT. 0) THEN
        RTR = SRAD / DTT
      ELSE
        RTR = SRAD
      ENDIF


      PC    =  1.0

      IF (CUMPH .LE. LEAF1) THEN
          PC = AMIN1((TIPINT+TIPGRAD*CUMPH),1.0)
      ENDIF
      IF (CUMPH .GT. LEAF3) THEN
          PC = AMIN1((1.0+(CUMPH-LEAF3)*TIPGRAD3),1.25)
      ENDIF


      TI     = DTT/(PHINT*PC)
      CUMPH  = CUMPH + DTT/(PHINT*PC)
      XN     = CUMPH
      LEAFNO = NINT(CUMPH)
      XTN    = XTNMAX*EXP(-BF*CUMPH)
      A      = PLAMAX*G4
      BFG4   = BF*G4

      IF (CARBO .LE. 0.0) THEN
        IF (ISTAGE .LT. 5) THEN
          MGROLF  = 0.0
          MGROPET = 0.0
          MGROCOM = 0.0
          GRORT   = 0.0
          MPLAG   = 0.0
          TPLAG   = 0.0
          TGROLF  = 0.0
          TGROPET = 0.0
          TGROCOM = 0.0
          MPLA    = MPLA + MPLAG
          TPLA    = TPLA + TPLAG
          PLA     = MPLA + TPLA
          GROCOM  = TGROCOM + MGROCOM
          MPETWT  = MPETWT + MGROPET
          TPETWT  = TPETWT + TGROPET
          PETWT   = MPETWT  + TPETWT
          MLFWT   = MLFWT   + MGROLF
          TLFWT   = TLFWT  + TGROLF
          LFWT    = MLFWT   + TLFWT
          MCORMWT = MCORMWT + MGROCOM
          TCORMWT = TCORMWT + TGROCOM
          CORMWT  = CORMWT  + GROCOM
          IF (ISTOP .GT. 5) THEN
             WRITE(*,*) 'CROP GROWTH TERMINATED DUE TO SEVERE STRESS'
             SUMDTT = P5 + 10  ! FORCED HARVEST 
!            CHP 5/14/2008
             MDATE = YRDOY
             CropStatus = 39
             RETURN
          ENDIF
        ENDIF
      ELSE
      SELECT CASE (ISTAGE)
        CASE (1)
          !
          ! Growth stage 1
          !
          SLAMP   = SLA1 
          CARBC   = CARBC1/G2 !*AMIN1(TEMF,SWFAC,NSTRES)
          CARBL   = CARBL1    !*AMIN1(TEMF,SWFAC,NSTRES)
          PETGR   = PETGR1*G2 !*AMIN1(SWFAC,NSTRES,TEMF)
          ! 
          MPLAG   = A*(-BFG4)*XTN*EXP(XTN)*TI
     &              *AMIN1(TURFAC,TEMF,AGEFAC,TSHOCK)
          MGROLF  = MPLAG * SLAMP                           
          MGROPET = MGROLF*PETGR

          IF (SEEDRV .GT. 0.0) THEN
             ASSIM = SEEDRV + CARBO
             GRORT = ASSIM*CARBR1*TROOT
             IF (MGROLF .LT. (ASSIM*CARBL0*TSHOCK)) THEN
                MGROLF  = ASSIM*CARBL0*TSHOCK
                MPLAG   = MGROLF/SLAMP
                MGROPET = MGROLF
             ENDIF
             MGROCOM = CARBO - GRORT - MGROLF - MGROPET
             IF (MGROCOM .LT. CARBC1*TSHOCK*CARBO) THEN
                MGROCOM = 0.0
             ENDIF
             SEEDRV = ASSIM - GRORT - MGROLF - MGROPET - MGROCOM
             IF (SEEDRV .LE. 0.0) THEN
                SEEDRV  = 0.0
                MGROCOM = 0.0
                GRORT   = CARBO*CARBR1
                MGROLF  = CARBO*CARBL1*TSHOCK
                MGROPET = CARBO - MGROLF - GRORT
                MPLAG   = MGROLF/SLAMP
             ENDIF
           ELSE
             GRORT = CARBO*(CARBR1 - (0.05*SUMDTT/P1))
     &               *AMAX1(2.0-TURFAC,1.8-AGEFAC*0.8,1.0)
             IF (MGROLF .GT. CARBL*CARBO) THEN
                MGROLF = CARBO*CARBL
             ENDIF
             MGROPET = MGROLF*PETGR
             MGROCOM = CARBO - GRORT - MGROLF - MGROPET
             IF (MGROCOM .LT. CARBO * CARBC) THEN  
                MGROCOM = CARBO * CARBC
                MGROTOP = CARBO - GRORT - MGROCOM
                MGROPET = MGROTOP - MGROLF
             ENDIF
             MPLAG = MGROLF/SLAMP
          ENDIF
 
          MPLA     = MPLA    + MPLAG
          MLFWT    = MLFWT   + MGROLF
          MPETWT   = MPETWT  + MGROPET
          MCORMWT  = MCORMWT + MGROCOM
          PLA      = MPLA
          LFWT     = MLFWT
          PETWT    = MPETWT
          CORMWT   = MCORMWT
          GROCOM   = MGROCOM
          RMPLA    = 1.0

        CASE (2,3,4)
          RMPLA   = 1.0
          SLAMP   = SLA3 
          CARBR   = CARBR2 - (0.10*SUMDTT/(P3+P4))
          CARBC   = CARBC3/G2  
          PETGR   = PETGR2*G2
          CLEAF   = CARBL3*CARBO
          GRF     = G3*SUFAC2 
          GRFMIN  = 0.05 + 0.80*(TPLA-TPLA*SENLA/PLA)/(PLA-SENLA)      
          IF (GRF .LT. GRFMIN ) THEN
             GRF = GRFMIN
          ENDIF
          IF (SUMDTT .GE. P3) THEN
             RMPLA = RSENCE*(1.0-RESPF*(SUMDTT-P3)/(P3+P4+P5))
          ENDIF

          MPLAG   = A*(-BF)*XTN*EXP(XTN)*TI
     &              *AMIN1(TURFAC,TEMF,AGEFAC)+TCARB1/SLA3
          MPLAG   = MPLAG  * RMPLA
          GRORT   = CARBO  * CARBR
     &             *AMAX1(1.0,2.0-TURFAC,1.5-AGEFAC*0.5)
          MGROLF  = MPLAG  * SLAMP               
          MGROPET = MGROLF * PETGR
          MGROTOP = MGROLF + MGROPET 
          MGROCOM = RGCORM/G2*(1.0+CMAT2*SUMDTT/(P3+P4))
     &                *DTT*AMIN1(TURFAC,TEMF,NSTRES,AVWSTRS,AVNSTRS)
          IF (MGROCOM .GT. CARBO*CARBC) THEN
                MGROCOM = CARBO*CARBC
          ENDIF
          TILCAR  = CARBO - MGROLF - MGROPET - GRORT - MGROCOM
          IF (CUMPH .GT. LEAFT) THEN     !TILLERING
               IF (TILCAR .GT. GRF * (CARBO-GRORT)) THEN
                  TILCAR  = GRF * (CARBO-GRORT)
                  MGROCOM = CARBO-MGROLF-MGROPET-GRORT-TILCAR
               ENDIF 
               IF (TILCAR .LT. GRFMIN * (CARBO-GRORT)) THEN
                  TILCAR = GRFMIN * (CARBO-GRORT)
                  IF (MGROLF .GT. CLEAF) THEN
                      MGROLF  = CLEAF
                      MGROPET = MGROLF*PETGR
                      MGROTOP = MGROLF + MGROPET
                      MPLAG   = MGROLF/SLAMP
                  ENDIF
                  MGROCOM = CARBO- MGROLF- MGROPET- GRORT- TILCAR
                  IF (MGROCOM .GT. CARBO*CARBC) THEN
                      TILCAR  = TILCAR + (MGROCOM - (CARBO*CARBC))
                      MGROCOM = CARBO*CARBC
                  ENDIF
                  IF (MGROCOM .LT. 0.0 .OR. TILCAR .LT. 0.0) THEN
                     IF (MGROCOM+TILCAR .GT. 0.0) THEN
                        TILCAR  = (TILCAR + MGROCOM) * 0.5
                        MGROCOM  = TILCAR
                      ELSE
                        TILCAR  = GRFMIN * (CARBO - GRORT)
                        MGROTOP = MGROLF + MGROPET + MGROCOM  !-VE CORM
                        MGROCOM = 0.0
                        MGROPET = MGROPET*MGROTOP/(MGROPET+MGROLF)
                        MGROLF  = MGROTOP - MGROPET
                        MPLAG   = MGROLF/SLAMP
                     ENDIF
                  ENDIF
               ENDIF
               TCARBO  = TILCAR
               CALL TR_TILLSUB (DYNAMIC, ISTAGE, 
     &    BGRAD, BGRAD2, BINT, BINT2, 
     &    DTT, FLOOD, G2, G3, KSHADE, LEAFT, LAI,         !Input
     &    NSTRES, P3, P4, P5, PETGR2, PETGR5, RGCORMT, 
     &    RTR, SLAT1, SLAT2, SLAT5, SRAD, SUCINT, SUCX1, 
     &    SUCX2, SUCX3, SUMDTT, TCARBC1, TCARBC2, TCARBC3, 
     &    TCARBL2, TCARBL5, TCARBO, TCLRAT1, TCMAT, 
     &    TCORMWT, TGROCOM, TILNO, TLFWT, TMIN, TMAX,     !Output
     &    TPETWT,  TPLA, TURFAC, XN)                      !Output
          ELSE
               MGROTOP = MGROTOP + TILCAR    ! NO TILLER GROWS
               MGROLF  = MGROLF*MGROTOP/(MGROLF+MGROPET)
               MGROPET = MGROTOP - MGROLF
               MPLAG   = MGROLF/SLAMP
               CARBO   = 0.0
               TGROLF  = 0.0
               TGROPET = 0.0
               TGROCOM = 0.0
               TPLAG   = 0.0
          ENDIF
          MLFWT   = MLFWT   + MGROLF
          MPETWT  = MPETWT  + MGROPET
          MCORMWT = MCORMWT + MGROCOM
          MPLA    = MPLA    + MPLAG
          TCARB1  = TILCAR  - TCARBO
          TCARB1  = AMAX1 (TCARB1,0.0)
          CORMWT  = MCORMWT + TCORMWT
          LFWT    = MLFWT   + TLFWT
          PETWT   = MPETWT  + TPETWT
          PLA     = MPLA    + TPLA
          GROCOM  = MGROCOM + TGROCOM
         
        CASE (5)
          IF (ABS(PLANTS - 0.01) < 1.E-4) RETURN
          RMPLA = RSENCE*(1.0-RESPF*1.075*(SUMDTT+P3+P4)/(P3+P4+P5))
          RMPLA   = AMIN1(RMPLA,1.0)
          RMPLA   = AMAX1(RMPLA,0.0)
          SLAMP   = SLA4 
          CARBL   = CARBL4 
          PETGR   = PETGR5*G2
          GRF     = G3*(SUFAC3- 1.0) ! stress
          GRFMIN  = 0.05 + 0.80*(TPLA-TPLA*SENLA/PLA)/(PLA-SENLA)
          GRF     = AMAX1 (GRF,GRFMIN) 
          GRORT   = CARBO*CARBR4
     &              *AMAX1(1.0,2.0-TURFAC,1.25-NSTRES*0.25)       
          TCARBO  = AMIN1(GRF,1.0-GRORT/CARBO)*CARBO
          CMPART  = 0.65/G2*AMIN1(SWFAC,NSTRES,TEMF)/G2
          CMFAC5  = 0.5
          MPLAG   = A*(-BF)*XTN*EXP(XTN)*TI
     &              *AMIN1(TURFAC,TEMF,AGEFAC)
          MPLAG   = MPLAG * RMPLA
          MGROLF  = MPLAG * SLAMP                     
          RESERVE = CARBO - TCARBO - GRORT
          IF (MGROLF .GT. CARBL*RESERVE) THEN
             MGROLF = RESERVE*CARBL                
             MPLAG  = MGROLF/SLAMP
          ENDIF
          IF (RESERVE .LT. 0.0) THEN
             TCARBO  = CARBO - GRORT
             RESERVE = 0.0
             MGROLF  = 0.0
             MPLAG   = 0.0
             MGROCOM = 0.0
          ENDIF
          MGROPET = MGROLF * PETGR 

          IF (SUMDTT .LE. P5) THEN
             IF (RESERVE .GT. 0.0) THEN
                MGROCOM = (RESERVE -MGROLF -MGROPET)
             ENDIF 
             IF (MGROCOM .GT. CMPART * CARBO)THEN
                MGROCOM = CMPART * CARBO 
             ENDIF 
             IF (MGROCOM .LT. 0.0) THEN
                MGROCOM = 0.0
                MGROPET = RESERVE *(RPET-1.0)
                MGROLF  = RESERVE - MGROPET
                MPLAG   = MGROLF  / SLAMP
             ENDIF
             TCARBO  = CARBO - MGROCOM - MGROPET - MGROLF - GRORT
             IF (TCARBO .GT. 0.0) THEN
                CALL TR_TILLSUB (DYNAMIC, ISTAGE, 
     &    BGRAD, BGRAD2, BINT, BINT2, 
     &    DTT, FLOOD, G2, G3, KSHADE, LEAFT, LAI,         !Input
     &    NSTRES, P3, P4, P5, PETGR2, PETGR5, RGCORMT, 
     &    RTR, SLAT1, SLAT2, SLAT5, SRAD, SUCINT, SUCX1, 
     &    SUCX2, SUCX3, SUMDTT, TCARBC1, TCARBC2, TCARBC3, 
     &    TCARBL2, TCARBL5, TCARBO, TCLRAT1, TCMAT, 
     &    TCORMWT, TGROCOM, TILNO, TLFWT, TMIN, TMAX,     !Output
     &    TPETWT,  TPLA, TURFAC, XN)                      !Output
              ELSE
                TCARBO  = 0.0
                TGROLF  = 0.0
                TGROPET = 0.0
                TGROCOM = 0.0
                TPLAG   = 0.0
             ENDIF
             MCORMWT = MCORMWT + MGROCOM
             MLFWT   = MLFWT   + MGROLF
             MPETWT  = MPETWT  + MGROPET
             CORMWT  = MCORMWT + TCORMWT
             PETWT   = MPETWT  + TPETWT
             LFWT    = MLFWT   + TLFWT
             GROCOM  = MGROCOM + TGROCOM
           ELSE
             MGROCOM = MGROLF*CMFAC5
             TCARBO  = CARBO - GRORT - MGROCOM - MGROLF - MGROPET
             TGROCOM = 0.0
             TGROPET = 0.0
             TGROLF  = 0.0
             TPLAG   = 0.0
             TLFWT   = TLFWT  + TGROLF
             TPETWT  = TPETWT + TGROPET
             IF (TCARBO .GT. 0.0) THEN
                CALL TR_TILLSUB (DYNAMIC, ISTAGE, 
     &    BGRAD, BGRAD2, BINT, BINT2, 
     &    DTT, FLOOD, G2, G3, KSHADE, LEAFT, LAI,         !Input
     &    NSTRES, P3, P4, P5, PETGR2, PETGR5, RGCORMT, 
     &    RTR, SLAT1, SLAT2, SLAT5, SRAD, SUCINT, SUCX1, 
     &    SUCX2, SUCX3, SUMDTT, TCARBC1, TCARBC2, TCARBC3, 
     &    TCARBL2, TCARBL5, TCARBO, TCLRAT1, TCMAT, 
     &    TCORMWT, TGROCOM, TILNO, TLFWT, TMIN, TMAX,     !Output
     &    TPETWT,  TPLA, TURFAC, XN)                      !Output
             ENDIF
             PETWT   = MPETWT  + TPETWT
             MLFWT   = MLFWT   + MGROLF
             LFWT    = MLFWT   + TLFWT
             GROCOM  = TGROCOM + MGROCOM
             MCORMWT = MCORMWT + MGROCOM
             CORMWT  = CORMWT  + GROCOM
          ENDIF
          MPLA    = MPLA + MPLAG
          PLA     = MPLA + TPLA
          IF (GROCOM > 1.E-4 .AND. ISWNIT .EQ. 'Y') THEN
              RCNFIL = RNINT - RCTFAC*DTT+0.75*(TMAX-TMIN) + TGRAD*TEMPM
              RCNFIL = RCNFIL*0.001                   
              NSINK  = RCNFIL*GROCOM
              NSINK  = NSINK*AGEFAC
              IF (ABS(NSINK) < 1.E-4) GO TO 1200
              RMNC   = TMNC*RCONST                    
              IF (STOVWT .GT. 0.0) THEN
                 TANC = STOVN/STOVWT
               ELSE
                 TANC = 0.0
               ENDIF
               TANC   = AMAX1 (TANC,TMNC)
               NPOOL1 = STOVWT*(TANC-TMNC)
               NPOOL2 = RTWT*(RANC-RMNC)
               NPOOL2 = AMAX1 (NPOOL2,0.0)
               XNF    = XNFINT + XNFGRAD*NFAC          
               TNLAB  = XNF*NPOOL1
               RNLAB  = XNF*NPOOL2
               NPOOL  = TNLAB + RNLAB
!               IF (NFAC .EQ. 0.0) THEN
               IF (ABS(NFAC) < 1.E-4) THEN
                  EMAT = EMAT + 1
               ENDIF
               NSDR   = NPOOL/NSINK
               IF (NSDR .LT. 1.0) THEN
                  NSINK = NSINK*NSDR
               ENDIF
            !
            ! Now if enough N in shoots supply corms
            ! if not enough get it from roots
            !
               IF (NSINK .GT. TNLAB) THEN
                  STOVN = STOVN - TNLAB
                  RNOUT = NSINK - TNLAB
                  RNLAB = RNLAB - RNOUT
                  TNLAB = 0.0
                  ROOTN = ROOTN - RNOUT
                  RANC  = ROOTN / RTWT
               ELSE
                  TNLAB = TNLAB - NSINK
                  STOVN = STOVN - NSINK
                  IF (STOVWT .GT. 0.0) THEN
                     TANC = STOVN/STOVWT
                  ELSE
                     TANC = 0.0
                  ENDIF
               !
               ! Here there is enough in shoot pool - just getting
               ! enough to satisfy corm N demand
               !
               ENDIF
          ENDIF
1200      CORMN  = CORMN + NSINK         


        CASE (6)
          !
          ! Physiological maturity
          !
          RETURN
       END SELECT
      ENDIF

      CARBO  = AMAX1 (CARBO,0.0001)
      IF (SEEDRV .LE. 0.0) THEN
          PDWI   = PCARB*(1.0-GRORT/CARBO)
          PGRORT = PCARB - PDWI
       ELSE
          PDWI   = MGROCOM+MGROLF+MGROPET
          PGRORT = GRORT
      ENDIF
      YRTWT  = RTWT
      YSTOVWT= STOVWT
      ! PTF DEFINED AS NON-ROOT PARTS/TOTAL (CORM IS NON-ROOT)
      PTF    = (LFWT + PETWT + CORMWT)/(LFWT + PETWT+CORMWT + 
     &         (RTWT + 0.5*GRORT - 0.005*RTWT))
      ! TODAY'S PTF FOR NUPTAKE CALCULATION

      IF (CUMPH .GE. 5.0) THEN
         SLFW = SLFWINT + SLFWGRD*TURFAC             
         SLFN = SLFNINT + SLFNGRD*NSTRES 
       ELSE
         SLFN = 1.0
         SLFW = 1.0
      ENDIF
      SLFC = 1.0
      IF (LAI .GT. SLAIFC) THEN
         SLFC = 1.0 - SLFCGRD*(LAI-SLAIFC)          
      ENDIF
      SLFT = 1.0
      IF (TEMPM .LE. 12.0) THEN       !   MIGHT NEED MORE SENSITIVITY
         SLFT = 1.0 -(SLFTINT-TEMPM)/SLFTGRD        
      ENDIF
      IF (TMIN .LE. 0.0) THEN
         SLFT = 0.0
      ENDIF
      SLFT   = AMAX1 (SLFT,0.0)
      IF (CUMDTT .LT. 5.0*PHINT) THEN
          SLAN    = 0.0
          YSLAN   = SLAN
          RMPLA1  = 1.0
        ELSE     
         ! SENESCENCE STARTS AFTER 5 LEAVES HAVE EMERGED
         YSLAN  = SLAN
         IF (ISTAGE .GE. 2) THEN
            RMPLA1 = AMAX1 (SLGRD2,
     &               RSENCE*(1.0-RESPF*(CUMDTT-P1)/(P1+P3+P4+P5)))
         ENDIF
         SLAN   = PLA*(SLGRD1-RMPLA1)**2*AMIN1(0.95,(CUMDTT-5.0*PHINT)/
     &            (P1+P3+P4+P5-5.0*PHINT))
      ENDIF
      PLAS   = (PLA-SENLA)*(1.0-AMIN1(SLFW,SLFT,SLFN,SLFC))
      SENLA  = SENLA + AMAX1((SLAN-YSLAN),PLAS,0.0)
      SENLA  = AMIN1 (SENLA,PLA)
      IF (SENLA .LE. 0.0) THEN 
         DEADPT = 0.0
         DEADLF = 0.0
      ENDIF    
      LAI    = (PLA-SENLA)*PLANTS*0.0001
      DEADLF = SENLA * SLA5 
      IF (DEADLF .GT. 0.0 .AND. ISTAGE .GE. 2) THEN
         DEADPT = DEADLF*G2*AMIN1 (0.8,(CUMDTT-P1)/
     &   (P1+P3+P4+P5))
      ENDIF
      YCUMNLOS  = AMAX1 (0.0,CUMNLOS)
      CUMNLOS   = AMAX1 (0.0,(DEADLF+DEADPT)*TMNC*0.80)
      DSTOVNLOS = AMAX1 (0.0,CUMNLOS - YCUMNLOS)

      YRTWT  = RTWT
      YSTOVWT= STOVWT
      PTF    =(LFWT + PETWT + CORMWT)/((LFWT + PETWT + CORMWT)
     &         + (RTWT + 0.5*GRORT - 0.005*RTWT))
      ! TODAY'S PTF FOR NUPTAKE CALCULATION
      ! PTF DEFINED AS NON-ROOT PARTS/TOTAL (CORM IS NON-ROOT)

      IF (ISWNIT .EQ. 'Y' .AND. XSTAGE .LT. 10.0 .AND. FIELD) THEN
        CALL TR_NUPTAK (DYNAMIC, 
     &    FLOOD, NH4, NO3, PDWI, PGRORT, PLANTS, PTF,     !Input
     &    RCNP, RLV, RTWT, SOILPROP, ST, STOVWT, SW, TCNP,!Input
     &    FLOODN, STOVN, RANC, ROOTN, TANC,               !I/O
     &    RNLOSS, TRNLOS, UNH4, UNO3, WTNUP)              !Output

      ENDIF
      !SENLAMY = SENLAM
      !SENLATY = SENLAT
      TTOP    = TCARBO - TGROCOM
      IF (CORMWT .GT. 0.0) THEN
         PCORMN = CORMN/CORMWT*100.0
       ELSE
         PCORMN = 0.0
      ENDIF
      
      LFWT   = AMAX1 (LFWT  - DEADLF,0.0)
      PETWT  = AMAX1 (PETWT - DEADPT,0.0)
      RTWT   = RTWT  + 0.5*GRORT-0.005*RTWT
      BIOMAS = LFWT  + PETWT + CORMWT
      STOVWT = LFWT  + PETWT
      STOVN  = AMAX1 (STOVN -  DSTOVNLOS,0.0)
      ROOTN  = AMAX1 (ROOTN -(RMNC*0.005*RTWT),0.0)
      IF (STOVWT > 1.E-6) THEN
        TANC = STOVN/STOVWT
      ENDIF
      IF (RTWT > 1.E-6) THEN
        RANC = ROOTN/RTWT
      ENDIF
      TOPSBIO = AMAX1(0.0,TOPSBIO)
      TOTROOT = AMAX1(0.0,TOTROOT)
      IF (SEEDRV .LT. 0.) THEN
          TOPSBIO = TOPSBIO + (CARBO - GRORT)
          TOTROOT = TOTROOT + GRORT
       ELSE 
          TOPSBIO = BIOMAS
          TOTROOT = RTWT
      ENDIF
      MAXLAI = AMAX1 (LAI,MAXLAI)
      PBIOMS  = BIOMAS*PLANTS*10.0  

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
      RETURN
      END SUBROUTINE TR_GROSUB
C=======================================================================

C=======================================================================
C  TR_PlantInit, Subroutine
C
C  Initialization at the beginning of planting
C-----------------------------------------------------------------------
C  Revision history
C
C  05-07-2002 CHP Written
C-----------------------------------------------------------------------
C  Called : TR_GROSUB
C=======================================================================
      SUBROUTINE TR_PlantInit(
     &    CORMN, NPPH, PLPH, ROOTN, STOVN, PLANTS, PLTPOP, 
     &    ITRANS, SEEDNI, TPLANTS)         !Output

      IMPLICIT NONE

      INTEGER ITRANS
      REAL CORMN, NPPH, PLANTN, PLANTS, PLPH, PLTPOP, ROOTN
      REAL SEEDNI, STOVN, TPLANTS, XPLANT

      TPLANTS = PLTPOP                            !FROM IPFLOD

C     Set up hill population from plant population
      NPPH = PLPH
      IF (NPPH .LT. 0.001) THEN  
         NPPH = 1.0                               !FROM IPFLOD
      ENDIF                    

      SELECT CASE(ITRANS)
      CASE (1)    
        IF (PLANTS .LE. 0.0) THEN
          PLANTS = TPLANTS*NPPH                   !FROM IPFLOD
          PLTPOP = PLANTS                         !FROM IPFLOD
        ENDIF
        XPLANT = PLANTS
        PLANTN = (ROOTN + STOVN + CORMN) / 10.0  !FROM INPHEN
        SEEDNI = PLANTN*PLANTS*10.0               !FROM INPHEN

      CASE (2)
        PLANTS = TPLANTS * NPPH                   !FROM IPFLOD
        PLTPOP = PLANTS                           !FROM IPFLOD
        XPLANT = PLANTS
        !PLANTS = 300.0                            !FROM INGROW
        PLANTN = 6.6E-6                           !FROM INPHEN
        SEEDNI = PLANTN*10.0*TPLANTS*NPPH         !FROM INPHEN

      CASE (3)
        PLANTS = TPLANTS * NPPH                   !FROM IPFLOD
        PLTPOP = PLANTS                           !FROM IPFLOD
        XPLANT = PLANTS
        PLANTN = (ROOTN + STOVN + CORMN) / 10.0  !FROM INPHEN
        SEEDNI = PLANTN*PLANTS*10.0               !FROM INPHEN

      CASE (4)
        IF (PLANTS .LE. 0.0) THEN
          PLANTS = TPLANTS*NPPH                   !FROM IPFLOD
          PLTPOP = PLANTS                         !FROM IPFLOD
        ENDIF
        XPLANT = PLANTS
        PLANTN = (ROOTN + STOVN + CORMN) / 10.0  !FROM INPHEN
        SEEDNI = PLANTN*PLANTS*10.0               !FROM INPHEN

      END SELECT


!     CHP added this in 7/31/2003 as per email from UPS
      PLTPOP = PLANTS

      RETURN
      END SUBROUTINE TR_PlantInit

C=======================================================================

C=======================================================================
C  TR_IPGROSUB, Subroutine
C
C  Reads FILEIO for GROSUB routine
C  05/07/2002 CHP Written
C  08/12/2003 CHP Added I/O error checking
C=======================================================================
      SUBROUTINE TR_IPGROSUB (CONTROL, 
     &    ATEMP, CROP, FILEC, P1,P3,P4,P5,
     &    G2,G3,G4,PHINT,PCINT,PCGRD, PATHCR, 
     &    PLANTS, PLPH, PLTPOP, ROWSPC, SDWTPL)

      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT     NONE
      EXTERNAL ERROR, FIND

      CHARACTER*2 CROP
      CHARACTER*6  ERRKEY, SECTION
      PARAMETER (ERRKEY = 'IPRICE')
      CHARACTER*12 FILEC
      CHARACTER*30 FILEIO
      CHARACTER*80 PATHCR

      INTEGER LINC, LNUM, LUNIO, ERR, FOUND

      REAL ATEMP, G2, G3, G4, P1, P3, P4, P5, PHINT, PCINT, PCGRD
      REAL PLANTS, PLPH, PLTPOP, ROWSPC, SDWTPL

C     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL
      FILEIO = CONTROL % FILEIO
      LUNIO  = CONTROL % LUNIO

!-----------------------------------------------------------------------
!       Read data from FILEIO for use in PLANT module
!-----------------------------------------------------------------------
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)

C-----------------------------------------------------------------------
C     Read FILE names and paths
C-----------------------------------------------------------------------
      READ(LUNIO,'(6(/),15X,A12,1X,A80)', IOSTAT=ERR) FILEC, PATHCR
      LNUM = 7
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

!-----------------------------------------------------------------------
!    Read Experiment Details, Treatments, and Cultivars Sections
!-----------------------------------------------------------------------
      SECTION = '*EXP.D'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEIO, LNUM)
      READ(LUNIO,'(////,3X,A2)', IOSTAT=ERR) CROP ; LNUM = LNUM + 5
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

C-----------------------------------------------------------------------
C    Read Planting Details Section
C-----------------------------------------------------------------------
      SECTION = '*PLANT'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEIO, LNUM)
      READ (LUNIO,70, IOSTAT=ERR) 
     &        PLANTS, PLTPOP, ROWSPC, SDWTPL, ATEMP, PLPH
   70 FORMAT (18X,2(1X,F5.1),13X,F5.0,12X,1X,F5.0,6X,2(1X,F5.0))
      LNUM = LNUM + 1
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

      ROWSPC = ROWSPC / 100.0

C-----------------------------------------------------------------------
C     Read crop genetic information
C-----------------------------------------------------------------------
      SECTION = '*CULTI'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEIO, LNUM)
      READ (LUNIO,100, IOSTAT=ERR) P1,P3,P4,P5,
     &                 G2,G3,G4,PHINT,PCINT,PCGRD
  100 FORMAT  (30X,4(F6.0),3(F6.2),3(F6.1))
      LNUM = LNUM + 1
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

      CLOSE (LUNIO)
      RETURN
      END SUBROUTINE TR_IPGROSUB

C=======================================================================
