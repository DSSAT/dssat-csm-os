C=======================================================================
C  PT_GROSUB, Subroutine
C
C  Potato growth routine
C-----------------------------------------------------------------------
C  Revision history
C
C  02/07/1993 PWW Header revision and minor changes
C  02/07/1993 PWW Switch block added, etc
C  12/13/1994 WTB Restructured N routines
C  08/27/2001 CHP Modified for modular structure.
C                 Changed LAI to XLAI to match CROPGRO variable.
C  06/11/2002 GH  Modified for Y2K
C  10/02/2002 CHP/WM Added bedwidth correction when PLME = 'B'
C  03/12/2003 CHP Changed senescence variable to composite (SENESCE)
C                   as defined in ModuleDefs.for
C  08/12/2003 CHP Added Walter Bowen's changes to GROLF from 1/2000
C  08/23/2011 GH/JIL Added CO2 response to tuber growth
!  04/01/2012 CHP Added two RUE parameters to new ecotype file
!  07/28/2023 HBD/FO Protection for SLFT not kill the canopy.
C=======================================================================

      SUBROUTINE PT_GROSUB (DYNAMIC,
     &    CO2, CUMDTT, DLAYR, DTT, DUL, FILEIO,           !Input
     &    ISTAGE, ISWNIT, KG2PPM, LL, NH4, NLAYR, NO3,    !Input
     &    RLV, RTF, SAT, SLPF, SRAD, STGDOY, STT, SW,     !Input
!     &    SWFAC, TGROAV,TMAX, TMIN, TURFAC, XSTAGE, YRDOY,!Input
     &    SWFAC, TMAX, TMIN, TURFAC, XSTAGE, YRDOY,!Input

     &    GRORT, SEEDRV,                                  !I/O

     &    AGEFAC, BIOMAS, CANNAA, CANWAA, CNSD1, CNSD2,   !Output
     &    DEADLF, GRAINN, LFWT, NSTRES, PLTPOP, ROOTN,    !Output
     &    RTWT, SDWTPL, SEEDNI, SENESCE, STMWT, STOVN,    !Output
     &    STOVWT, TOPSN, TOPWT, TRNU, TUBN, TUBWT,        !Output
     &    UNH4, UNO3, WTNCAN, WTNLO, WTNUP, XLAI)         !Output

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT  NONE
      EXTERNAL PT_IPGRO, PT_NUPTAK, PT_NFACTO, ALIN, TABEX
      SAVE

      LOGICAL FIRST

      CHARACTER*1  ISWNIT, PLME
      CHARACTER*30 FILEIO

      INTEGER DYNAMIC, ISTAGE
      INTEGER NLAYR, YRDOY
      INTEGER STGDOY(20)

      REAL AGEFAC, ARVCHO, BIOMAS, CANNAA, CANWAA
      REAL BWRATIO, RUE1, RUE2
      REAL CARBO, CNSD1, CNSD2, CO2
      REAL CUMDTT, DEVEFF, DDEADLF, DEADLF, DEADLN, DTT, ETGT
      REAL G2, G3, GRAINN, GRF, GROLF, GROPLNT, GRORT
      REAL GROSTM, GROTOP, GROTUB
      REAL LALWR, LFWT, NFAC, NSTRES, PT_PAR, PCARB
      REAL PCO2, PD, PGRTUB, PLA, PLAG
      REAL PLAS, PLTPOP, PRFT, PTF, PTUBGR
      REAL RANC, RCNP, RLGR, ROOTN, RTF, RTPAR, RTWT
      REAL RVCAV, RVCHO, RVCMAX, RVCUSD
      REAL SDWTPL, SEEDAV, SEEDN, SEEDNI, SEEDRV
      REAL SLAN, SLFC, SLFN, SLFT, SLFW, SLPF, SRAD, STMWT
      REAL STOVN, STOVWT, STT, SWFAC
      REAL TABEX, TANC, TCNP, TEMPM, TIND, TMAX, TMIN
      REAL TMNC, TOPSN, TOPWT, TRNU, TUBANC, TUBCNP, TUBN
      REAL TUBWT, TURFAC, WTNCAN, WTNLO, WTNUP, XLAI, XSTAGE
      !REAL SLFT_TMAX, SLFT_TMIN, TGROAV, ALIN, MIN
      REAL SLFT_TMAX, SLFT_TMIN, ALIN, MIN

      REAL DTII(3)
      TYPE (ResidueType) SENESCE 

      REAL, DIMENSION(4)  :: SENST, SENSF
      REAL, DIMENSION(10) :: CO2X, CO2Y
      REAL, DIMENSION(NL) :: DLAYR, DUL, KG2PPM, LL, 
     &    NH4, NO3, RLV, SAT, SW, UNO3, UNH4  

!      DATA  LALWR, SLAN /270.,0./
      DATA  LALWR /270./      !leaf area:leaf wt. ratio (cm2/g)
     
!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      CALL PT_IPGRO(
     &    FILEIO,                                         !Input
     &    CO2X, CO2Y, G2, G3, PD, PLME, PLTPOP,           !Output
     &    SDWTPL, RUE1, RUE2, SENSF, SENST)               !Output

      IF (PLME .EQ. 'B') THEN
        !Bed width ratio = Bed width / Row Spacing
        !For now, this ratio will be coded in (could use the ROWSPC
        !  variable in FILEX in the future). CHP
        BWRATIO = 0.5
      ELSE
        BWRATIO = 1.0
      ENDIF

      FIRST = .TRUE.

!     From INPLNT
      AGEFAC = 1.0
      BIOMAS = 0.0
      CANNAA = 0.0
      CANWAA = 0.0
      DEADLF = 0.0
      DEADLN = 0.0
      GRAINN = 0.0
      XLAI   = 0.0
      LFWT   = 0.0
      NFAC   = 1.0
      NSTRES = 1.0
      PTF    = 0.0
      RANC   = 0.0215 !set to zero in PHASEI - see below
      RCNP   = 0.0
      ROOTN  = 0.0
      RTWT   = 0.0
      SEEDNI = 0.0
      STMWT  = 0.0
      STOVN  = 0.0
      STOVWT = 0.0
      TANC   = 0.045  !set to zero in PHASEI - see below
      TCNP   = 0.0
      TMNC   = 0.0
      TOPSN  = 0.0
      TOPWT  = 0.0
      TUBANC = 0.014  !set to zero in PHASEI - see below
      TUBWT  = 0.0

!     This initialization was in INPHEN
      IF (ISWNIT .NE. 'Y') THEN
         TANC = 0.0
      ENDIF

!-----------------------------------------------------------------------
!     These stmts from main program
      !
      ! Only 80% of the CHO in the seed piece is assumed to be available
      ! for growth.
      !
      SEEDRV = SDWTPL/(PLTPOP * 10.0)          ! convert kg/ha to g/plt
      SEEDRV = SEEDRV*0.8

!-----------------------------------------------------------------------
!These initializations were done in PHASEI, cases 1, 2, 5 and 7
      BIOMAS  = 0.0
      CNSD1   = 0.0
      CNSD2   = 0.0
      DDEADLF = 0.0
      DTII    = 0.0   !array
      XLAI    = 0.0
      LFWT    = 0.0
      GRORT   = 0.0
      GROTOP  = 0.0
      PLA     = 0.0
      RANC    = 0.0  !Note conflict with IPLNT initialization (above)
      RTWT    = 0.0
      SEEDN   = 0.0
      SEEDNI  = 0.0
      STMWT   = 0.0
      TANC    = 0.0
      TIND    = 0.0
      TOPWT   = 0.0
      TUBANC  = 0.0  !Note conflict with IPLNT initialization (above)
      TUBN    = 0.0
      TUBWT   = 0.0
      
      CALL PT_NUPTAK (SEASINIT, 
     &    ISTAGE, DLAYR, DUL, KG2PPM, LL, NH4, NLAYR, NO3,!Input
     &    PLTPOP, RCNP, RLV, RTWT, SAT, SW, TCNP, TMNC,   !Input
     &    TOPWT, TUBCNP, TUBWT,                           !Input
     &    GRORT, GROTOP, GROTUB, ROOTN, TOPSN, TUBANC,    !I/O
     &    ARVCHO, RANC, TANC, TRNU, TUBN, UNH4, UNO3,     !Output
     &    WTNUP)                                          !Output

      CARBO  = 0.0
      PCARB  = 0.0
      STT    = 0.0
      TRNU   = 0.0
      UNH4   = 0.0
      UNO3   = 0.0
      WTNLO  = 0.0
      WTNCAN = 0.0

!***********************************************************************
!***********************************************************************
!     Daily rate calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------

      IF (FIRST) THEN     !Initializations from PHASEI, all Case(7), 
                          ! except where noted.
        FIRST   = .FALSE.
!        XLAI    = PLA * PLANTS * 0.0001
        XLAI    = PLA * PLTPOP * 0.0001       !CHP
        IF (PLME .EQ. 'B') THEN               !WM
          XLAI    = XLAI * BWRATIO            !WM
        ENDIF                                 !WM
        LFWT    = 0.093                
        PLA     = 25.0        !cm2/plant             
        STMWT   = LFWT                 
        TOPWT   = 0.186                
        IF (ISWNIT .EQ. 'Y') THEN
          RANC  = 0.015
          TANC  = 0.045
          TUBN  = 0.0
          ROOTN = RANC * RTWT
          TOPSN = TOPWT * TANC
          SEEDN = (RANC * RTWT) + (TOPWT * TANC)
          SEEDNI = SEEDN * PLTPOP 
          TUBANC = 0.014              !from PHASEI, Case(1) 
          TUBWT = 0.010               !from PHASEI, Case(1)
          TUBN = TUBANC * TUBWT       !from PHASEI, Case(1)
          SEEDN  = SEEDN + TUBN       !from PHASEI, Case(1)
        ENDIF
      ENDIF

      IF (ISWNIT .NE. 'N') THEN
         CALL PT_NFACTO (
     &    ISTAGE, TANC, XSTAGE,                           !Input
     &    AGEFAC, CNSD1, CNSD2, NFAC, NSTRES,             !Output
     &    RCNP, TCNP, TMNC)                               !Output

         TUBCNP = 0.014
!         TUBMNC = 0.007      !moved to PT_NUPTAK
       ELSE
         NSTRES = 1.0
         AGEFAC = 1.0
         NFAC   = 1.0
      END IF   

      TEMPM = (TMAX + TMIN)/2.0         ! Mean temp. calculation
      !PRFT  = 1.2 - 0.0035*(TEMPM - 22.5)**2 !original funtion
!      --------Begin----effect of Tmean on PRFT, modified by RR 02/15/2016
      IF (TEMPM .LE. 14) THEN
          PRFT  = 1.2 - 0.0035*(TEMPM - 22.5)**2
      ! The next 2 lines set the bounds of PRFT at 0.0 and 1.0
      !
          PRFT  = AMAX1 (PRFT,0.0)
          PRFT  = AMIN1 (PRFT,1.0)

      ELSEIF (TEMPM .GT. 14 .AND. TEMPM .LE. 24) THEN
          PRFT = 1.0
      ELSEIF (TEMPM .GT. 24 .AND. TEMPM .LE. 35) THEN
!         RR linear function from 24 to 40  y = -0.0909x + 3.1818      
          PRFT = -0.0909*(TEMPM) + 3.1818 
      ELSE
          PRFT = 0
      END IF
!      --------End-----effect of Tmean on PRFT, modified by RR 02/15/2016
!      
    
!       Calculation of daily leaf senescence, begin
!      
      SELECT CASE (ISTAGE)
        CASE (1)                        ! Natural senescence, SLAN
          SLAN = CUMDTT*PLA/10000.
        CASE (2)
          SLAN = (CUMDTT*PLA/10000.)*EXP(-1.60 + XSTAGE)*(PD**0.5)
     &           *(1./NFAC)
      END SELECT

      IF (ISTAGE .EQ. 2) THEN              ! Senescence from stress
         SLFW = 0.95 + 0.05*TURFAC         ! ...Water stress
C        SLFN = 0.95 + 0.05*AGEFAC         ! ...Nitrogen stress
         SLFN = 1.0
         SLFC = 1.0                        ! ...Competition
         IF (XLAI .GT. 4.) THEN
            SLFC = 1. - 0.008*(XLAI - 4.)
         ENDIF

!         SLFT = 1.0                        ! ...Temperature
!         IF (TMIN .LE. 0.0) THEN
!             SLFT = 0
         !ENDIF
         !
         ! The following was causing the plant to die; modified 
         ! temporarily, so that T factor SLFT will be very small 
         ! but at least not ZERO.
         ! This NEEDS to be FIXED!!!  WTB, Lima, 28/06/96
         !
         ! With SLFT = 0, means all leaf area senesced!
         ! SLFT = 1.0 - 0.02*TMIN**2 was taken from SIMPOTATO V1.53
         !
!        SA/CHP 9/2/2015. Introduce a high temperature function 
!          based on that in NWheat model.
!         IF (TMIN .LE. 0.0) THEN
!           SLFT = 1.0 - (6.0 - TEMPM)/6.0
!         ENDIF
         SLFT_TMIN = 1.0 - (6.0 - TEMPM)/6.0

         SLFT_TMAX = (ALIN(SENST,SENSF,4,TMAX)) ! RR 01/14/2016

!        Use the maximum here, but in the next equation, it's minimum!
         SLFT = MAX(SLFT_TMIN, SLFT_TMAX) !changed to MIN RR
         SLFT = MIN(SLFT_TMIN, SLFT_TMAX)
         SLFT = MAX (SLFT, 0.0)
!        20230-07-28 HBD, FO - Protection for SLFT not kill the canopy.
!         IF (TMIN <= 0.0) SLFT = 0.0

         PLAS = PLA*(1.0 - AMIN1(SLFW,SLFC,SLFT,SLFN))
       ELSE
         PLAS = 0.0
       END IF

      DDEADLF = AMAX1(SLAN,PLAS)/LALWR               ! Daily leaf loss

      IF (DDEADLF .GE. LFWT) THEN
          DDEADLF = LFWT
      END IF   

!     Senesced matter to surface residue
      SENESCE % ResWt(0) = DDEADLF * PLTPOP * 10. !/ 0.40
!                kg/ha   = g/plant * pl/m2 * (kg/ha)/(g/m2)
      SENESCE % ResE(0,1) = SENESCE % ResWt(0) * TMNC
!               kg[N]/ha  =            kg/ha   * g[N]/g[plant]

      ! Senescence calculation, end
      !
      ! Update of haulm after senescence

      LFWT   = LFWT   - DDEADLF
      PLA    = LFWT   * LALWR
!      XLAI   = PLA    * PLANTS * 0.0001
      XLAI   = PLA    * PLTPOP * 0.0001       !CHP
      IF (PLME .EQ. 'B') THEN                 !WM
        XLAI   = XLAI * BWRATIO               !WM
      ENDIF                                   !WM
      TOPWT  = LFWT   + STMWT
      DEADLF = DEADLF + DDEADLF
      IF (ISWNIT .NE. 'N') THEN
         TOPSN  = TOPSN  - (DDEADLF*TMNC)
         DEADLN = DEADLN + (DDEADLF*TMNC)
      ENDIF
      
!      This unction was merged with PRFT
!      --------beggin----RR effect of Tmean on RUE 02/15/2016
!      IF (TEMPM .LE. 24) THEN
!          TX_RUE = 1.0
!      ELSEIF (TEMPM .GT. 24 .AND. TEMPM .LE. 35) THEN
!          TX_RUE = -0.0909*(TEMPM) + 3.1818 !RR linear function from 24 to 40  y = -0.0909x + 3.1818      
!      ELSE
!         TX_RUE = 0
!      END IF
!      --------end----------

!       Potential carbon fixation
      PT_PAR = SRAD*0.5               ! PAR = SRAD*.02092
      IF (ISTAGE .LT. 2) THEN
         !PCARB = 3.5*PT_PAR/PLTPOP*(1.0 - EXP(-0.55*XLAI))    !CHP
         PCARB = RUE1*PT_PAR/PLTPOP*(1.0 - EXP(-0.55*XLAI))    !CHP
       ELSE
!        PCARB = 4.0*PT_PAR/PLTPOP*(1.0 - EXP(-0.55*XLAI))    !CHP
!        PCARB = RUE2*TX_RUE*PT_PAR/PLTPOP*(1.0 - EXP(-0.55*XLAI))    !CHP
         PCARB = RUE2*PT_PAR/PLTPOP*(1.0 - EXP(-0.55*XLAI))    !CHP
      END IF

      IF (PLME .EQ. 'B') THEN                 !WM
        PCARB = PCARB / BWRATIO               !WM
      ENDIF                                   !WM
      !
      ! Calculate Photosynthetic Response to CO2
      !
      PCO2   = TABEX (CO2Y,CO2X,CO2,10)
!     PCARB  = PCARB*PCO2 ! original function 02/15/2016
      PCARB  = PCARB*PCO2*PRFT
!     Modified by RR 02/15/2016
!     CARBO  = PCARB*AMIN1(PRFT, SWFAC, NSTRES)*SLPF + 0.5*DDEADLF ! original function 02/15/2016
      CARBO  = PCARB*AMIN1(SWFAC, NSTRES)*SLPF + 0.5*DDEADLF 
      
      RVCUSD = 0.0                                   ! Reserve C used

      SWFAC  = AMAX1 (SWFAC, 0.1)
      TURFAC = AMAX1 (TURFAC,0.1)

      SELECT CASE (ISTAGE)
        CASE (1)
          !
          ! Vegetative Growth
          !
          ! Potential growth as function of temperature
          ! Primary varaible is leaf expansion; other growth
          ! parameters calculated from PLAG
          !
          GROTUB = 0.0
          PGRTUB = 0.0
          RLGR   = 0.50*DTT             ! From Ingram & McCloud (1984)
          PLAG   = EXP(RLGR)*PLA - PLA
          PLAG   = PLAG*AMIN1 (TURFAC, AGEFAC, 1.0)
          GROLF  = PLAG/LALWR
          GROSTM = GROLF*0.75
          !
          ! Following is a temporary fix to assure adequate
          ! root growth after emergence
          !
          IF (XSTAGE .LT. 1.5) THEN      ! RTPAR: proportion to roots
             RTPAR = 0.5 - 0.5*(XSTAGE - 1.0)
           ELSE
             RTPAR = 0.25
          END IF

          GRORT   = (GROLF + GROSTM)* RTPAR
          GROPLNT =  GROLF + GROSTM + GRORT
          !
          ! Use of seed piece carbon between emergence and
          ! PLA=400, as in Ng and Loomis
          !
          IF (PLA .LT. 100.0) THEN
             SEEDAV = 1.5*STT
             RVCHO  = 0.0
           ELSEIF (PLA .GE. 100.0 .AND. PLA .LE. 400.0) THEN
             SEEDAV = (1.5 - 0.005*(PLA - 100.0))*STT
             RVCHO  = 0.0
           ELSE
             SEEDAV = 0.0
             SEEDRV = 0.0
          END IF

          SEEDAV = AMIN1 (SEEDAV,SEEDRV)
          RVCAV  = SEEDAV + RVCHO

          IF (CARBO .LT. GROPLNT) THEN
              CARBO = CARBO + RVCAV
              IF (CARBO .LT. GROPLNT) THEN
                  GRF    = CARBO  / GROPLNT
                  GROLF  = GROLF  * GRF
                  PLAG   = GROLF  * LALWR
                  GROSTM = GROSTM * GRF
                  GRORT  = GRORT  * GRF
                  RVCUSD = RVCAV
                  RVCHO  = 0.0
               ELSE
                  RVCHO  = CARBO  - GROPLNT
                  RVCUSD = RVCAV  - RVCHO
              END IF

              IF (PLA .LE. 400.) THEN
                 SEEDRV = AMAX1 (SEEDRV - RVCUSD,0.0)
                 RVCHO  = 0.0
              END IF
              !
              ! A reserve carbohydrate pool, limited to 10 % 
              !       of haulm weight
           ELSE
              RVCUSD = 0.0
              IF (PLA .GT. 400.0) THEN
                  RVCMAX = 0.1*(LFWT+GROLF+STMWT+GROSTM)
                  RVCHO  = RVCHO + CARBO - GROPLNT
                  RVCHO  = AMIN1 (RVCHO,RVCMAX)
               ELSE
                  RVCHO = 0.0
              END IF
          END IF
        CASE (2)
          !
          ! Tuber Growth
          !
          ! Calculation of TIND: proportion of tuber demand
          ! to be filled under prevailing temperature
          !
          DTII(1) = DTII(2)
          DTII(2) = DTII(3)
          DTII(3) = RTF + 0.5 * (1.0 - AMIN1(SWFAC, NSTRES,1.0))
!         DTII(3) = RTF + 0.5 * (1.0 - AMIN1(SWFAC, NSTRES, LFT,1.0)) !added tmax stress
          DTII(3) = AMIN1 (DTII(3),1.0)
          !
          ! DEVEFF is used to limit carbon demand of tubers
          ! immediately after initiation; eg. if PD=1.0,
          ! DEVEFF defaults to 1.0 at XSTAGE=2.1
          !
          DEVEFF = AMIN1 ((XSTAGE - 2.0) * 10. * PD, 1.0)
          IF (NFAC .GT. 1.0) THEN
             TIND = (DTII(1)+DTII(2)+DTII(3)/3.0)*(1./NFAC)*DEVEFF
           ELSE
             TIND = (DTII(1)+DTII(2)+DTII(3)/3.0)*DEVEFF
          END IF

          TIND = AMAX1 (TIND,0.0)
          TIND = AMIN1 (TIND,1.0)

          IF (TEMPM .GT. 2.0 .AND. TEMPM .LT. 15.0) THEN
             ETGT = 0.0769*(TEMPM-2.0)
           ELSEIF (TEMPM .GE. 15.0 .AND. TEMPM .LE. 23.0) THEN
             ETGT = 1.0
           ELSEIF (TEMPM .GT. 23.0 .AND. TEMPM .LT. 33.0) THEN
             ETGT = 1.0 - 0.1*(TEMPM-23.0) 
           ELSE
             ETGT = 0.0
           ENDIF
               
          ! Calculation of potential growth .. Set priorities for carbon
          !
!          PTUBGR  = G3*ETGT/PLTPOP    !CHP
           PTUBGR  = G3 * PCO2 * ETGT/PLTPOP          !JIL   (Modified)
          
          
          IF (PLME .EQ. 'B') THEN                     !WM
            PTUBGR  = PTUBGR / BWRATIO                !WM
          ENDIF                                       !WM
!          SLFT = (ALIN(SENST,SENSF,4,TMAX))
          GROTUB  = PTUBGR*AMIN1 (TURFAC, AGEFAC, 1.0)*TIND
!          PLAG    = G2*DTT/PLANTS
          PLAG    = G2*DTT/PLTPOP     !CHP
          IF (PLME .EQ. 'B') THEN                     !WM
            PLAG    = PLAG / BWRATIO                  !WM
          ENDIF                                       !WM
          PLAG    = PLAG  *AMIN1 (TURFAC, AGEFAC, 1.0)
          GROLF   = PLAG/LALWR
          GROSTM  = GROLF*0.75
          RTPAR   = 0.2
          GRORT   = (GROLF + GROSTM)* RTPAR
          GROPLNT =  GROLF + GROSTM + GRORT
          RVCAV   = RVCHO                  
          CARBO   = CARBO + RVCAV
          !
          ! Partitions C based on tuber being first priority; 
          ! C becomes progressively more limiting in this routine, 
          ! and growth reduction factor (GRF) is implemented to 
          ! reduce potential growth
          !
          IF (CARBO .GE. GROTUB) THEN
              CARBO = CARBO - GROTUB
              IF (CARBO .LT. GROPLNT) THEN
                  GRF    = CARBO  / GROPLNT
                  GROLF  = GROLF  * GRF
                  PLAG   = GROLF  * LALWR
                  GROSTM = GROSTM * GRF
                  GRORT  = GRORT  * GRF
                  RVCUSD = RVCAV
                  RVCHO  = RVCHO  - RVCUSD
               ELSE
                  RVCHO  = CARBO  - GROPLNT
                  IF (RVCHO .GT. RVCAV) THEN
                     RVCUSD = 0.0
                     RVCMAX = 0.1*(LFWT+GROLF+STMWT+GROSTM)
                     RVCHO  = AMIN1 (RVCHO,RVCMAX)
                   ELSE
                     RVCUSD = RVCAV - RVCHO
                  END IF
              END IF    
           ELSE
              GROTUB = CARBO
              GROLF  = 0.0
              GROSTM = 0.0
              GRORT  = 0.0
              RVCUSD = RVCAV
              RVCHO  = RVCHO - RVCUSD
          END IF

          PGRTUB = GROTUB
        CASE DEFAULT
          STOP 'Illegal ISTAGE in GROSUB module'
      END SELECT

      GROTOP = GROLF + GROSTM
      CARBO  = AMAX1 (CARBO, 0.0001)
      !
      ! N subroutines are called;  compare potential
      ! growth (carbon-driven) with available N and
      ! adjusts growth if necessary
      !                             
      IF (ISWNIT .NE. 'N') THEN
          ! IF (SEEDAV .GT. 0.0) THEN
          !    SRVNU = RVCUSD*0.014       ! Seed reserve N used
          ! END IF
          ! SRVNU  = AMAX1 (SRVNU, 0.0)
          ! AVAILN = (SRVNU)+(0.5*DDEADLF*TMNC)

        CALL PT_NUPTAK (RATE,
     &    ISTAGE, DLAYR, DUL, KG2PPM, LL, NH4, NLAYR, NO3,!Input
     &    PLTPOP, RCNP, RLV, RTWT, SAT, SW, TCNP, TMNC,   !Input
     &    TOPWT, TUBCNP, TUBWT,                           !Input
     &    GRORT, GROTOP, GROTUB, ROOTN, TOPSN, TUBANC,    !I/O
     &    ARVCHO, RANC, TANC, TRNU, TUBN, UNH4, UNO3,     !Output
     &    WTNUP)                                          !Output

!-----------------------------------------------------------------------
! Jan 2000, Walter Bowen 
! Before going into NUPTAK, GROTOP was set equal to GROLF+GROSTM, 
! with GROSTM = GROLF * 0.75. As shown below, after coming out of 
! NUPTAK, GROLF was then set to GROTOP*0.5, which always results 
! in a loss of C. The equation was corrected as shown: 
! GROTOP = GROLF + GROSTM 
!       = GROLF + (GROLF*0.75) 
!       = GROLF*(1 + (1*0.75)) 
!       = GROLF * 1.75 
! thus, GROLF = GROTOP * (1/1.75) 
! was...   GROLF   = GROTOP * 0.50 
! should be: 
          GROLF   = GROTOP * (1/1.75) 
          GROSTM  = GROLF  * 0.75                    ! Added 0.75 (WTB) 
          GROPLNT = GROLF  + GROSTM + GRORT 
!-----------------------------------------------------------------------

          IF (ARVCHO .GT. 0.) THEN
             IF (PLA .LE. 400.) THEN
                SEEDRV = SEEDRV + AMIN1(ARVCHO,RVCUSD)
              ELSE
                RVCMAX = 0.1*(LFWT+GROLF+STMWT+GROSTM)
                RVCHO = RVCHO + ARVCHO
                RVCHO = AMIN1 (RVCHO,RVCMAX)
             END IF
          END IF
       ELSE
         NFAC = 1.0
      END IF
      !
      ! Daily Summary of Plant Parts
      !
      LFWT   = LFWT  + GROLF
      STMWT  = STMWT + GROSTM
      RTWT   = RTWT  + 0.8*GRORT
      TOPWT  = LFWT  + STMWT
      TUBWT  = TUBWT + GROTUB
      STOVWT = LFWT  + STMWT
      !
      ! Equivalancies for reporting
      !
      GRAINN = TUBN
      STOVN  = TOPSN

      IF (GROTOP .LT. 0.0) THEN
         DEADLF = DEADLF - GROTOP*0.5
      END IF

      PLAG = GROLF * LALWR
      PLA  = PLA   + PLAG
!      XLAI = PLA   * PLANTS * 0.0001
      XLAI = PLA   * PLTPOP * 0.0001  !CHP
      IF (PLME .EQ. 'B') THEN                 !WM
        XLAI = XLAI * BWRATIO                 !WM
      ENDIF                                   !WM

      IF (TOPWT .LE. 0.0) THEN
         TOPWT = 0.0
         XLAI  = 0.0
         TANC  = 0.0
       ELSE
         TANC  = TOPSN/TOPWT
      END IF

!      BIOMAS = (LFWT + STMWT + TUBWT)*PLANTS
      BIOMAS = (LFWT + STMWT + TUBWT)*PLTPOP  !CHP
      !IF (PLME .EQ. 'B') THEN                 !WM
      !  BIOMAS = BIOMAS / BWRATIO             !WM
      !ENDIF                                   !WM

      IF (GROTUB + GROPLNT .GT. 0.0) THEN
          PTF = GROTUB/(GROTUB + GROPLNT)  ! Proportion of daily growth
       ELSE                                ! due to tuber growth
          PTF = 0.0
      END IF

!     Moved from main program
      WTNLO  = DEADLN * PLTPOP

!     Moved from main program
      IF (YRDOY .EQ. STGDOY(3)) THEN
        CANNAA = STOVN * PLTPOP
        CANWAA = BIOMAS
      ENDIF

!     Moved from OPGROW
      WTNCAN = (STOVN + GRAINN) * PLTPOP

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE PT_GROSUB
C=======================================================================


C=======================================================================
C  PT_IPGRO, Subroutine
C
C  Data input for potato growth module
C-----------------------------------------------------------------------
C  Revision history
C
C  08/27/2001 CHP Written
C  08/12/2003 CHP Added I/O error checking
C=======================================================================
      SUBROUTINE PT_IPGRO(
     &    FILEIO,                                         !Input
     &    CO2X, CO2Y, G2, G3, PD, PLME, PLTPOP,           !Output
     &    SDWTPL, RUE1, RUE2, SENSF, SENST)               !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, ERROR, IGNORE

      INTEGER LUNIO, LUNCRP, LUNECO
      CHARACTER*1 PLME
      CHARACTER*1, PARAMETER :: BLANK = ' '
      CHARACTER*6, PARAMETER :: ERRKEY = 'GROSUB'

      CHARACTER*2   CROP
      CHARACTER*5   ACRO(4)  
      CHARACTER*6   SECTION, ECONO, ECOTYP
      CHARACTER*12  FILEC, FILEE
      CHARACTER*16  ECONAM
      CHARACTER*30  FILEIO
      CHARACTER*80  PATHCR, PATHEC
      CHARACTER*92  FILECC, FILEGC
      CHARACTER*180 CHAR
      CHARACTER*255 C255

      INTEGER ERR, FOUND, I, J, LINC, LNUM, PATHL, ISECT

      REAL G2, G3, PD, PLTPOP, SDWTPL, RUE1, RUE2
      REAL, DIMENSION(4) :: SENST, SENSF
      REAL CO2X(10), CO2Y(10)
      
!      LOGICAL EOF
!-----------------------------------------------------------------------
!     Read data from FILEIO for use in GROSUB module
      CALL GETLUN('FILEIO', LUNIO)
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)

!     Read file and path name for species file
      READ(LUNIO,'(6(/),15X,A12,1X,A80)', IOSTAT=ERR) FILEC, PATHCR
      LNUM = 7
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
      READ (LUNIO,105,IOSTAT=ERR) FILEE, PATHEC; LNUM = LNUM + 1
  105 FORMAT(15X,A12,1X,A80)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

C    Read Crop Code from Cultivars Section
      SECTION = '*CULTI'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ (LUNIO,'(3X,A2)', IOSTAT=ERR) CROP ; LNUM = LNUM + 1
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
      ENDIF

C    Read Planting Details Section
      SECTION = '*PLANT'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ (LUNIO,'(25X,F5.1,5X,A1,25X,F5.0)', IOSTAT=ERR) 
     &        PLTPOP, PLME, SDWTPL ; LNUM = LNUM + 1
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
      ENDIF

C     Read crop genetic information
      SECTION = '*CULTI'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        IF (INDEX ('PT',CROP) .GT. 0) THEN
          READ (LUNIO,'(24X,A6,1X,3F6.0)',IOSTAT=ERR) 
     &          ECONO, G2, G3, PD
          LNUM = LNUM + 1
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
        ENDIF
      ENDIF

      CLOSE (LUNIO)

C-----------------------------------------------------------------------
C     Read Crop Parameters from FILEC
      LNUM   = 0
      PATHL  = INDEX (PATHCR,BLANK)
      IF (PATHL .LE. 1) THEN
         FILECC = FILEC
       ELSE
         FILECC = PATHCR(1:(PATHL-1)) // FILEC
      ENDIF
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEC,0)

      ACRO(1) = 'CO2X'
      ACRO(2) = 'CO2Y'
      ACRO(3) = 'SENST'
      ACRO(4) = 'SENSF'
      LNUM = 0

      DO WHILE (ERR == 0)
!       READ (LUNCRP,'(A180)',IOSTAT=ERR, END=200) CHAR
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        IF (ISECT == 0) EXIT
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEC,LNUM)
        DO J = 1, 4
          IF (INDEX(CHAR(1:15),ACRO(J)) > 0) THEN
            SELECT CASE (J)
              CASE (1)
                READ (CHAR(16:66),'(10F5.0)',IOSTAT=ERR)(CO2X(I),I=1,10)
              CASE (2)
                READ (CHAR(16:66),'(10F5.2)',IOSTAT=ERR)(CO2Y(I),I=1,10)
              CASE (3)
                READ (CHAR(16:39),'(10F6.0)',IOSTAT=ERR)(SENST(I),I=1,4)
              CASE (4)
                READ (CHAR(16:39),'(10F6.0)',IOSTAT=ERR)(SENSF(I),I=1,4)
            END SELECT
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEC,LNUM)
          ENDIF
        END DO
      END DO

  200 CLOSE (LUNCRP)
C-----------------------------------------------------------------------
C     Read ECO Parameters from FILEE
C-----------------------------------------------------------------------
C     Open FILEE Ecotype coefficients (.ECO File)
C-----------------------------------------------------------------------
        LNUM = 0
        PATHL  = INDEX(PATHEC,BLANK)
        IF (PATHL .LE. 1) THEN
          FILEGC = FILEE
        ELSE
          FILEGC = PATHEC(1:(PATHL-1)) // FILEE
        ENDIF

C-----------------------------------------------------------------------
C    Read Ecotype Parameter File
C-----------------------------------------------------------------------
!RUE1 - Radiation use efficiency, ISTAGE=1, g plant dry matter/MJ PAR
!RUE2 - Radiation use efficiency, ISTAGE>1, g plant dry matter/MJ PAR
C-----------------------------------------------------------------------
        CALL GETLUN('FILEE', LUNECO)
        OPEN (LUNECO,FILE = FILEGC,STATUS = 'OLD',IOSTAT=ERR)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEE,0)
        ECOTYP = '      '
        LNUM = 0
        DO WHILE (ECOTYP .NE. ECONO)
          CALL IGNORE(LUNECO, LNUM, ISECT, C255)
          IF (ISECT .EQ. 1 .AND. C255(1:1) .NE. ' ' .AND.
     &          C255(1:1) .NE. '*') THEN  
            READ(C255,3100,IOSTAT=ERR) ECOTYP, ECONAM
3100        FORMAT (A6,1X,A16)
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEE,LNUM)
            IF (ECOTYP .EQ. ECONO) THEN
              READ(C255(26:37),'(F5.0, 1X, F5.0)',IOSTAT=ERR) RUE1, RUE2
              IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEE,LNUM)
              EXIT
            ENDIF
          ELSEIF (ISECT .EQ. 0) THEN
            CALL ERROR(ERRKEY,7,FILEE,LNUM)
          ENDIF
        ENDDO

        CLOSE (LUNECO)

C-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE PT_IPGRO
C=======================================================================
