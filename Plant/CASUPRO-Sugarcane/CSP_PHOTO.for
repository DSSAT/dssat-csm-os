C  CSP_PHOTO, Subroutine, based on PHOTO Subroutine by 
C  K.J.Boote, J.W.Jones, G. Hoogenboom
C  Compute daily photosynthetic using canopy (C) method.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1989 KJB Written.
C  06/02/1993 NBP Made into subroutine.
C  02/11/1994 NBP Corrected dimension (15) for XPGSLW, YPGSLW
C  04/24/1994 NBP Replaced TAIRHR, TAVG with TGRO, TGROAV.
C  09/25/1995 KJB Light interception for ET the same for canopy and leaf
C                 models
C  11/11/1999 CHP Modified for modular format
C  01/13/2000 NBP Added SWFAC effect on PG
C  06/11/2002 GH  Modified for Y2K
C  08/12/2003 CHP Added I/O error checking
C  10/09/2003 FSR Modified for O.H. Daza CASUPRO sugarcane model
C  08/17/2004 FV  Modified PTSMAX photosynthesis calculation
C  09/02/2004 CHP Added KC_SLOPE to SPE file and KC_ECO to ECO file.
C  10/04/2005 FSR Implemented stalk-level phosynthesis.
!-----------------------------------------------------------------------
!  Called from:   CSP_CASUPRO
!  Calls:         CSP_PHOTIP
C=======================================================================

      SUBROUTINE CSP_PHOTO(CONTROL,FILEIO, 
     &    CANHT, CanLmtFac, CANWH, CO2, ECONO, EXCESS,    !Input
     &    DeltaLeafArea, DeltaLeafNum, FILECC,            !Input 
     &    DeltaLeafArealost, FILEGC, FRACSH, FRSHV,       !Input
     &    KCAN, LeafAreaPlant, LeafNum, LfShdFac, PAR,    !Input 
     &    PStres2, RNITP, SLAREF, SLAAD,                  !Input
     &    SLPF, Smax, StalkState, StkHt, SWFAC, TAVG,     !Input 
     &    TURFAC, WEATHER, XHLAI,                         !Input
     &    AGEFAC, LeafDist, LI, LITOTAL, PGT, PgRatio,    !Output
     &    PLTPOP, StkPG)                                  !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
      USE ModuleData

      IMPLICIT NONE
      EXTERNAL CSP_PHOTIP, CSP_CANOPY, CURV, GETLUN, TABEX
      SAVE

      CHARACTER*3 TYPPGN, TYPPGT
      CHARACTER*4 StalkState(NumOfStalks,10)
      CHARACTER*6 ECONO
      CHARACTER*30 FILEIO
      CHARACTER*92 FILECC, FILEGC

      INTEGER DAS, DYNAMIC, H, PLUN, Smax, YRDOY, YRSIM, Zone

      REAL AGEFAC, AGEFCC, A0, CANHT, CanLmtFac, CANWH, !, BETN
     &  CCEFF, CCK, CCMAX, CCMP, CO2, COLDSTR, CUMSTR, CURV, 
     &  E_FAC, FRACSH, FRSHAV, FRSHV, KCAN, LAITH1,  
     &  LAITH2, LAsun, LAshd, LAgnd, LIsun, LaiAllZone, LAI2, LIalt, 
     &  LaiSunZone, LaiShdZone, LaiGndZone, LfShdFac, LIshd, 
     &  LIsunAvl, LIshdAvl, LIgndAvl, LNREF, PAR, PARMAX, PG,      
     &  LI, LIallZone, LITOTAL, SLPF, PGT,
     &  PGLFMX, PGREF, PGSLW, PHTMAX, PLTPOP, PRATIO,   
     &  PStres2, PTSMAX, RNITP, ROWSPC, SHLAI, SLAREF, SLAAD, 
     &  SLW, SNDN, SNUP, SWFAC, TABEX, 
     &  TGRO(TS), TLI, TURFAC, XHLAI, XLAI    
      
      REAL PlantLeafAreaTmp, temp 

      INTEGER OpenStatus, Stalk  
      INTEGER GP1, GP2, GP3, Sun, Shade, Ground

!	INTEGER, PARAMETER :: Sun=1, Shade=2, Ground=3, CanopyLayers=3
      INTEGER, PARAMETER :: CanopyLayers=3
      REAL FNPGN(4), FNPGT(4) 
      REAL TAVG, PG_b, XPGSLW(15), YPGSLW(15) 

      REAL, DIMENSION(0:NumOfDays) :: LeafAreaPlant, LAtmp, LAItmp 
      REAL, DIMENSION(1:NumOfStalks) :: EXCESS, PgRatio, 
     &           StkHt, TotalPG
      REAL, DIMENSION(0:NumOfDays,NumOfStalks) :: LeafNum, !LeafArea, 
     &           DeltaLeafArea, DeltaLeafArealost, DeltaLeafNum

      REAL, DIMENSION(1:NumOfStalks,CanopyLayers) :: LeafDist, StkLI,
     &               StkPG

      TYPE (WeatherType) WEATHER
!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH

!     Transfer values from constructed data types into local variables.
      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM   
      SNDN    = WEATHER % SNDN   
      SNUP    = WEATHER % SNUP  

C***********************************************************************
C***********************************************************************
C     Run Initialization - Called once per simulation
C***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
C-----------------------------------------------------------------------
      CALL CSP_PHOTIP(FILEIO,
     &  CCEFF, CCMAX, CCMP, FNPGN, FNPGT, LIsun,  !, KCAN
     &  LIshd, LNREF, PARMAX, PGREF,    !, LMXSTD, PHTHRS10
     &  PHTMAX, PLTPOP, ROWSPC, TYPPGN, TYPPGT, XPGSLW, 
     &  YPGSLW)

C-----------------------------------------------------------------------
C     Adjust canopy photosynthesis for GENETIC input value of
C     maximum leaf photosynthesis (LMXSTD). Exponential curve from
C     from the hedgerow photosynthesis model (Boote et al, 199?).

! This needs be revised in case of use.
C-----------------------------------------------------------------------
!       Copied from CSP_GROW     FSR
!-----------------------------------------------------------------------
!!        ROWSPC = ROWSPC / 100.
!!        IF (ROWSPC .GT. 0.0 .AND. PLTPOP .GT. 0.0) THEN
!!          BETN = 1./(ROWSPC*PLTPOP)
!!        ELSE
!!          BETN = 0.
!!        ENDIF
      Sun=1
      Shade=2
      Ground=3
C-----------------------------------------------------------------------
      CALL CSP_CANOPY(CONTROL, 
     &    DAS, DeltaLeafNum, ECONO, FILECC,               !Input
     &    FILEGC, FRACSH, FRSHV, KCAN,                    !Input
     &    LeafNum, PAR, ROWSPC, Smax,                     !Input 
     &    StalkState, TGRO, TURFAC, WEATHER, XHLAI,       !Input
     &    CANHT, CANWH, FRSHAV, H, StkHt)                 !Output
C-----------------------------------------------------------------------
! LMXSTD   Maximum leaf photosyntheses for standard cultivar.  No apparent
!          source for this in CASUPRO files.  Had been using M1 from 
!          CUL file, but that was an apparent legacy from CropGro. 
! PGREF    Reference value for leaf level photosynthesis used in canopy 
!            light response curve (µmol[CO2] / m2-s)

! chp 2/9/2007 remove LMXSTD, no value available.
!      IF (PGREF .GT. 0.0 .AND. LMXSTD .GT. 0.0) THEN
!        PGLFMX = (1. - EXP(-1.6 * LMXSTD)) / (1. - EXP(-1.6 * PGREF))
!      ELSE
        PGLFMX = 1.0
!      ENDIF

C***********************************************************************
C***********************************************************************
C     Seasonal initialization - run once per season
C***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
! Open file to write results from CSP_PHOTO.for
      CALL GET(ISWITCH)
      IF (INDEX('Y'  ,ISWITCH%IDETG) > 0 .AND. 
     &    INDEX('YDA',ISWITCH%IDETL) > 0) THEN

        CALL GETLUN('CSP_PHOTO',PLUN)
        OPEN(UNIT = PLUN, FILE = "CSP_PHOTO.OUT", STATUS = "UNKNOWN", 
     &     ACTION = "WRITE", POSITION = "REWIND", IOSTAT = OpenStatus)

        WRITE(PLUN,'(1X,"RESULTS FROM CSP_PHOTO.for")')
        WRITE(PLUN,'(90X,"LeafArea m2")')
        WRITE(PLUN,'(1X,"  YRDOY  DAS XHLAI LAIAZ SHLAI FRSHA 
     &LIalt    LI LITOT LIsun LIshd LIgnd     PG  PGTOT  GP1  GP2
     &  GP3 EXCESS 
     &   PAR RNITP SLAAD SWFAC  TAVG AGEFAC PTSMAX   SLPF    
     &TLI AGEFCC  PGSLW PRATIO PGLFMX")',
     &  ADVANCE="NO")
     
!!!      DO Stalk = 1, Smax
!!!        WRITE(PLUN,'("  LI",I2)',ADVANCE="NO") Stalk
!!!      END DO
     
        DO Stalk = 1,Smax
          WRITE(PLUN,'("  PG",I2.2)',ADVANCE="NO") Stalk
        END DO
      ENDIF

C-----------------------------------------------------------------------
      AGEFAC   = 0.0
      CUMSTR   = 0.0
      COLDSTR  = 0.0

      LaiSunZone = 0.0 
      LaiShdZone = 0.0
      LaiGndZone = 0.0
      LaiAllZone  = 0.0

      LIsunAvl = 0.0  
      LIshdAvl = 0.0
      LIgndAvl = 0.0
!************ new ************

      FRACSH = 0.0
      PGT     = 0.0
      LAsun   = 0.0  ! Daily leaf area, stalks in pseudo-zone 1 
      LAshd   = 0.0  ! Daily leaf area, stalks in pseudo-zone 2
      LAgnd   = 0.0  ! Daily leaf area, stalks in pseudo-zone 3
      LITOTAL = 0.0  ! Sum of stalk Light Interception
      GP1 = 0 
      GP2 = 0 
      GP3 = 0
      LAI2 = 0. 
          
      DO Stalk = 1,Smax  
        DO Zone  = 1,3
          LeafDist(Stalk,Zone)   = 0.0
          StkPG(Stalk,Zone)      = 0.0
          StkLI(Stalk,Zone)      = 0.0
        END DO  ! Zone
      END DO  ! Stalk
!************ end ************

!	LAItmp   = 0.0
      PG       = 0.0

      DO Stalk = 1,Smax
!!!        DO Zone  = 1,3
!!!          StkPG(Stalk,Zone)      = 0.0
!!!          StkLI(Stalk,Zone)      = 0.0
!!!        END DO  ! Zone
        PgRatio(Stalk)    = 0.0
        TotalPG(Stalk)    = 0.0
        StalkState(Stalk,3) = '    '
      END DO  ! Stalk

C-----------------------------------------------------------------------
! Calculate the fraction of ground area shaded prior to canopy closing.

      CALL CSP_CANOPY(CONTROL, 
     &    DAS, DeltaLeafNum, ECONO, FILECC,               !Input
     &    FILEGC, FRACSH, FRSHV, KCAN,                    !Input
     &    LeafNum, PAR, ROWSPC, Smax,                     !Input 
     &    StalkState, TGRO, TURFAC, WEATHER, XHLAI,       !Input
     &    CANHT, CANWH, FRSHAV, H, StkHt)                 !Output
C-----------------------------------------------------------------------
      LAITH1 = (1/(-KCAN)) * log(1-LIsun)      !chp for portability
      
      LAITH2 = (1/(-KCAN)) * log(1-LIshd)
C***********************************************************************
C***********************************************************************
C     Daily rate calculations
C***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
C-----------------------------------------------------------------------
      GP1 = 0 
      GP2 = 0 
      GP3 = 0 
          
      DO Stalk = 1,Smax
        DO Zone  = 1,3
          StkPG(Stalk,Zone)      = 0.0
          StkLI(Stalk,Zone)      = 0.0
        END DO  ! Zone
      END DO  ! Stalk

! CanopyLayers
!	Sun    = 1  
!	Shade  = 2
!	Ground = 3
C-----------------------------------------------------------------------
C     Calculate maximum photosynthesis as function of PAR, g CH2O/m2
C-----------------------------------------------------------------------
      PGT     = 0.0
      LAsun   = 0.0  ! Daily leaf area, stalks in pseudo-zone 1 
      LAshd   = 0.0  ! Daily leaf area, stalks in pseudo-zone 2
      LAgnd   = 0.0  ! Daily leaf area, stalks in pseudo-zone 3
      LITOTAL = 0.0  ! Sum of stalk Light Interception
      PTSMAX = PHTMAX * (1.0 - EXP(-(1.0 / PARMAX) * PAR))

! Use this equation for the time being. Maximum photosynthesis without 
! any correction (See correction factors in equation above).
! Calculations taken from paper of Goudriaan and van Laar ()

!Fernando Villegas changed 8/17/2004 
!      PTSMAX = -0.0035 * PAR ** 2 + 1.068 * PAR

      PG_b = PTSMAX

C-----------------------------------------------------------------------
C     Calculate reduction in photosynthesis due to incomplete canopy.
C-----------------------------------------------------------------------
! LI - Light Interception  
!
!  Ignore KCANR for stalk-based photosynthesis - JWJ
!!      IF (BETN .LE. ROWSPC) THEN
!!        SPACNG = BETN / ROWSPC
!!      ELSE
!!        SPACNG = ROWSPC / BETN
!!      ENDIF
!!    PGFAC = 1. - EXP(-KCANR * XHLAI)

      LI    = 1. - EXP(-KCAN * XHLAI)

C-----------------------------------------------------------------------
C     Compute reduction in PG based on the average daylight temperature.
!
      TLI = CURV(TYPPGT,FNPGT(1),FNPGT(2),FNPGT(3),FNPGT(4),TAVG)
C-----------------------------------------------------------------------
C     Compute reduction in PG as a function of leaf N concentration.
!     Until N stress is restored, temporarily replaced following lines so 
!     that AGEFCC = approx 1  FSR
!
!      AGEFAC = CURV(TYPPGN,FNPGN(1),FNPGN(2),FNPGN(3),FNPGN(4),RNITP)
!      AGEREF = CURV(TYPPGN,FNPGN(1),FNPGN(2),FNPGN(3),FNPGN(4),LNREF)
!      AGEFAC = AGEFAC / AGEREF
      AGEFAC = 1.028571  
C-----------------------------------------------------------------------
C     9/24/95 KJB,JWJ Decrease sensitivity of canopy PG to N function
C     to mimic behavior of leaf version.  AGEFAC corresponds to leaf
C     PG vs. leaf N. Function does not act strongly on QE, thus, we
C     need to scale effect back on daily canopy version.
C-----------------------------------------------------------------------
      AGEFCC = (1.0 - EXP(-2.0 * AGEFAC)) / (1. - EXP(-2.0 * 1.0))

C-----------------------------------------------------------------------
C     Compute canopy PG response to changes in specific leaf weight.
C     (Dornhoff and Shibles, 197?).
C-----------------------------------------------------------------------
      IF (SLAAD .GT. 0.0) THEN
        SLW = 1. / SLAAD
      ELSE IF (SLAREF .GT. 0.0) THEN
        SLW = 1. / SLAREF
      ELSE
        SLW = 0.0099
      ENDIF
      PGSLW = TABEX(YPGSLW, XPGSLW, SLW, 10)

C-----------------------------------------------------------------------
C     Adjust canopy photosynthesis for CO2 concentration assuming a
C     reference value of CO2 of 330 ppmv.
C-----------------------------------------------------------------------
      CCK = CCEFF / CCMAX
      A0 = -CCMAX * (1. - EXP(-CCK * CCMP))
      PRATIO = A0 + CCMAX * (1. - EXP(-CCK * CO2))

!***********************************************************************
!***********************************************************************
!      Daily integration
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
!     09-14-2007 FSR restored the INTEGR section which had been commented
!     out in the CSP_PHOTO module.  This was done to be more consistent 
!     the CSP_GROW_CANE module, since CSP_PHOTO shares several variables 
!     with CSP_GROW_CANE in order to distribute each stalk's leaf area  
!     across three canopy zones.  These functions of CSP_PHOTO are more
!     integrative than rate-calculating.
!
C     Effect of daylength on daily gross PG, computed by KJB with
C     stand alone model, and used by Ernie Piper in his dissertation
C     see page 127 for the equation and conditions. Nornalized to
C     about 13 hours where the function is 1.00.  Actually
C     normalized to a bit higher between 13 and 14 hours.
C
C     DLFAC = 1.0 + 0.6128 - 0.01786*DAYL + 0.006875*DAYL*DAYL
C    &        - 0.000247*DAYL*DAYL*DAYL
C
C-----------------------------------------------------------------------
! AGEFAC   Relative effect of current leaf N on canopy photosynthesis (0-1)
!            (fraction)
! AGEFCC   Effect of AGEFAC on photosynthesis 
! AGEREF   Reference value calculated at reference leaf N value to 
!            normalize the effect of N for different plant species 
! LI    Multiplier to compute daily canopy PG as a function of leaf area 
!            index (LAI) 
! PGLFMX   Multiplier for daily canopy photosynthesis to account for 
!          cultivar differences in leaf photosynthesis capabilities 
! PGREF    Reference value for leaf level photosynthesis used in canopy 
!            light response curve (µmol[CO2] / m2-s)
! PGSLW    Relative effect of leaf thickness (SLW) on daily canopy PG 
! PRATIO   Relative effect of CO2 on canopy PG, for multiplying by PG 
!            computed for 330 vpm 
! PTSMAX   Potential amount of CH20 which can be produced at the specified 
!            PAR for full canopy (LAI>8), all other factors optimal
!            (g[CH2O]/m2-d)
! SLPF   Empirical multiplier to adjust daily canopy PG due to unknown 
!        soil or environmental factors that persist in a given location 
! SWFAC    Effect of soil-water stress on photosynthesis, 1.0=no stress, 
!            0.0=max stress 
! TLI   Reduction in specific leaf area due to daytime temperature 
!          being less than optimal (0-1) 
! --------------------- temp for checking output -----------------------
C  Compute daily gross photosynthesis (g CH2O/m2/d)

!     Phosphorus stress
!     Following CropGro photo.for (CHP 05/07/2004) 
!     AGEFCC can be > 1.0, so don't want to use minimum of PSTRES2 and AGEFCC
!     (PSTRES2 is always 1.0 or below).
      IF (AGEFCC .GE. 1.0) THEN
        E_FAC = AGEFCC * PSTRES2
      ELSE
        E_FAC = MIN(AGEFCC, PSTRES2)
      ENDIF

!-----------------------------------------------------------------------
! Calculate the fraction of ground area shaded prior to canopy closing.

      CALL CSP_CANOPY(CONTROL, 
     &    DAS, DeltaLeafNum, ECONO, FILECC,               !Input
     &    FILEGC, FRACSH, FRSHV, KCAN,                    !Input
     &    LeafNum, PAR, ROWSPC, Smax,                     !Input 
     &    StalkState, TGRO, TURFAC, WEATHER, XHLAI,       !Input
     &    CANHT, CANWH, FRSHAV, H, StkHt)                 !Output
C-----------------------------------------------------------------------
!  Compute daily gross canopy photosynthesis (g CH2O/stalk/d) 
!!
!!      PG = PTSMAX * SLPF * LI * TLI * E_FAC * PGSLW
!!     &   * PRATIO * PGLFMX * SWFAC 
!-----------------------------------------------------------------------
!	FRSHAV - Avg of FRACSH; hourly daylight shaded area
!     SHLAI  - Shaded LAI; > XHLAI prior to canopy closing
!     FRSHV  - Vertical sun shadow fraction 
!	FRACSH - Hourly daylight shadow fraction of ground area 
      FRSHAV = MAX(FRSHAV, 3.0E-05)
      
      SHLAI  = XHLAI * 1 / FRSHAV


      LIalt = (1. - EXP(-KCAN * (XHLAI/FRSHAV))) * FRSHAV 

!-----------------------------------------------------------------------
!  Compute daily gross canopy photosynthesis (g[CH2O]/m2-d) 
!  (this function is now used only for checking against sum of stalk PG)

      PG = PTSMAX * SLPF * LIalt * TLI * E_FAC * PGSLW
     &   * PRATIO * PGLFMX * SWFAC 

!-----------------------------------------------------------------------
!**********************************************************************
!----------------BEGIN STALK-BASED PHOTOSYNTHESIS METHOD----------------
!-----------------------------------------------------------------------
! Remove DEAD stalk leaf area.	
      
      DO Stalk = 1,Smax
        IF (StalkState(Stalk,1) .EQ. 'DEAD') THEN

          LAsun = LAsun - LeafDist(Stalk,Sun)
          LeafDist(Stalk,Sun) = 0.
          LAsun = MAX(LAsun,0.)
         
          LAshd = LAshd - LeafDist(Stalk,Shade)
          LeafDist(Stalk,Shade) = 0.
          LAshd = MAX(LAshd,0.)

          LAgnd = LAgnd - LeafDist(Stalk,Ground)
          LeafDist(Stalk,Ground) = 0.
          LAgnd = MAX(LAgnd,0.)

        END IF ! (StalkState(Stalk,1) .EQ. 'DEAD')
           
      END DO ! Stalk = 1,Smax 
!-----------------------------------------------------------------------
! Remove senesced leaf area beginning with lowest leaves.

      DO Stalk = 1,Smax
           
        IF (StalkState(Stalk,1) .EQ. 'LIVE' .AND. 
     &        DeltaLeafArealost(DAS,Stalk) > 0.) THEN

            temp = DeltaLeafArealost(DAS,Stalk)

            IF (LeafDist(Stalk,Ground) > 0.) THEN
                  LeafDist(Stalk,Ground) = LeafDist(Stalk,Ground)- temp 
                temp = 0.

                IF (LeafDist(Stalk,Ground) < 0.) THEN
                    temp = -LeafDist(Stalk,Ground)
                    LeafDist(Stalk,Ground) = 0.
                END IF ! (LeafDist(Stalk,Ground) < 0.)
            END IF ! (LeafDist(Stalk,Ground) > 0.)

            IF (LeafDist(Stalk,Shade) > 0. .AND. temp > 0.) THEN
                LeafDist(Stalk,Shade) = LeafDist(Stalk,Shade)- temp 
                temp = 0.

                IF (LeafDist(Stalk,Shade) < 0.) THEN
                    temp = -LeafDist(Stalk,Shade)
                    LeafDist(Stalk,Shade) = 0.
                END IF ! (LeafDist(Stalk,Shade) < 0.)
            END IF ! (LeafDist(Stalk,Shade) > 0.)


            IF (LeafDist(Stalk,Sun) > 0. .AND. temp > 0.) THEN
                LeafDist(Stalk,Sun) = LeafDist(Stalk,Sun) - temp 
                temp = 0.

                IF (LeafDist(Stalk,Sun) < 0.) THEN
                    temp = -LeafDist(Stalk,Sun)
                    LeafDist(Stalk,Sun) = 0.
                    LaiSunZone = 0.
                END IF ! (LeafDist(Stalk,Sun) < 0.)

            END IF ! (LeafDist(Stalk,Ground) > 0.)

        END IF ! (StalkState(Stalk,1) .EQ. 'LIVE')
        
        temp = 0.

      END DO !Stalk = 1,Smax 
!-----------------------------------------------------------------------
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!-----------------------------------------------------------------------
! Assign leaf area on each stalk to canopy zones based on available space 
! in each canopy zone. Each stalk may have leaf area in each zone.  
! This section corresponds to the period before the leaf canopy closes.

      PlantLeafAreaTmp = 0.   ! for de-bugging  FSR

      IF (FRSHAV < 1.0)  THEN ! Canopy has not closed

        DO Stalk = 1,Smax 

          IF (StalkState(Stalk,1) .EQ. 'LIVE') THEN

            IF (LaiSunZone <= LAITH1) THEN

              LeafDist(Stalk, Sun) = LeafDist(Stalk,Sun)
     &                        + DeltaLeafArea(DAS,Stalk) 


            ELSE IF (LaiSunZone + LaiShdZone <= LAITH2) THEN

              LeafDist(Stalk,Shade) = LeafDist(Stalk,Shade) 
     &		                + DeltaLeafArea(DAS,Stalk) !DAS-1 
     
      
            ELSE
              LeafDist(Stalk,Ground) = LeafDist(Stalk,Ground) 
     &		                + DeltaLeafArea(DAS,Stalk) !DAS-1

            END IF !   

          END IF !   

        END DO !Stalk = 1,Smax  

      ELSE ! (FRSHAV = 1.0 means canopy has closed) 
      
! After closing, new leaf area is allocated to highest canopy zone where
! a stalk has previously-existing leaf area.  Since the canopy has closed, 
! the canopy zone is generally filled (LaiSunZone >= LAITH1), and new
! leaf area displaces existing leaf area, both same-stalk and other stalk,
! which are moved down a zone.

        DO Stalk = 1,Smax 
        
          IF (LeafDist(Stalk,Sun) > 0.)   THEN

            LeafDist(Stalk,Sun) = LeafDist(Stalk,Sun)
     &                        + LfShdFac * DeltaLeafArea(DAS,Stalk)
     
            LeafDist(Stalk,Shade) = LeafDist(Stalk,Shade) 
     &		              + (1 - LfShdFac) * DeltaLeafArea(DAS,Stalk) 
        
        
          ELSE IF (LeafDist(Stalk,Shade) > 0.)   THEN

            LeafDist(Stalk,Shade) = LeafDist(Stalk,Shade) 
     &		                + LfShdFac * DeltaLeafArea(DAS,Stalk)
            
            LeafDist(Stalk,Ground) = LeafDist(Stalk,Ground) 
     &		              + (1 - LfShdFac) * DeltaLeafArea(DAS,Stalk)
          ELSE 

            LeafDist(Stalk,Ground) = LeafDist(Stalk,Ground) 
     &		                       + DeltaLeafArea(DAS,Stalk)
          END IF

        END DO

      END IF !(FRSHAV < 1.0) 
!-----------------------------------------------------------------------
! Calculate cumulative leaf area in each canopy pseudo-zone

!     IF (LeafAreaPlant(DAS-1) > 0) THEN

      DO Stalk = 1,Smax 

            LAsun = LAsun + LeafDist(Stalk,Sun)
          
            LAshd = LAshd + LeafDist(Stalk,Shade) 
          
            LAgnd = LAgnd + LeafDist(Stalk,Ground) 
      
            PlantLeafAreaTmp = PlantLeafAreaTmp  !for debugging FSR
     &                       + LeafDist(Stalk,Sun) 
     &                       + LeafDist(Stalk,Shade)
     &                       + LeafDist(Stalk,Ground)

      END DO !Stalk = 1,Smax

      LAtmp(DAS) = LAsun + LAshd + LAgnd !FSR-temporary debug check  

!-----------------------------------------------------------------------
! Calculate LAI for each canopy pseudo-zone

      LaiSunZone =  LAsun * PLTPOP * 0.0001 * 1 / FRSHAV

      LaiShdZone =  LAshd * PLTPOP * 0.0001 * 1 / FRSHAV
      
      LaiGndZone =  LAgnd * PLTPOP * 0.0001 * 1 / FRSHAV
      
!-----------------------------------------------------------------------
! Check whether canopy zones are over-filled, and if so move leaf area
! down a zone, beginning with newer (higher number) stalks.
     
      Stalk = Smax + 1  
      
      DO WHILE ((LaiSunZone > LAITH1) .AND. (Stalk > 1.))
        
        Stalk = Stalk - 1

        IF (LeafDist(Stalk,Sun) > 0.)   THEN
            
            LeafDist(Stalk,Shade) = LeafDist(Stalk,Shade)
     &                              + CanLmtFac * LeafDist(Stalk,Sun)  

            LeafDist(Stalk,Sun) = (1 - CanLmtFac) * LeafDist(Stalk,Sun) 

            LAsun = LAsun - CanLmtFac * LeafDist(Stalk,Sun)
            LAshd = LAshd + CanLmtFac * LeafDist(Stalk,Sun)

            LaiSunZone =  LAsun * PLTPOP * 0.0001 * 1 / FRSHAV
            LaiShdZone =  LAshd * PLTPOP * 0.0001 * 1 / FRSHAV

        END IF

      END DO	
!------------------------
      Stalk = Smax +1

      DO WHILE (LaiSunZone + LaiShdZone > LAITH2 .AND. Stalk > 1.)

        Stalk = Stalk - 1

        IF (LeafDist(Stalk,Shade) > 0.)   THEN
            
            LeafDist(Stalk,Ground) = LeafDist(Stalk,Ground)
     &                              + CanLmtFac * LeafDist(Stalk,Shade) 

            LeafDist(Stalk,Shade)=(1-CanLmtFac) * LeafDist(Stalk,Shade) 

            LAshd = LAshd - CanLmtFac * LeafDist(Stalk,Shade)

            LaiShdZone =  LAshd * PLTPOP * 0.0001 * 1 / FRSHAV

        END IF

      END DO	
!-----------------------------------------------------------------------
! Re-calculate cumulative leaf area in each canopy pseudo-zone

!     IF (LeafAreaPlant(DAS-1) > 0) THEN
      
      LAsun = 0.
      LAshd = 0. 
      LAgnd = 0.
      PlantLeafAreaTmp = 0.

      DO Stalk = 1,Smax 

            LAsun = LAsun + LeafDist(Stalk,Sun)
          
            LAshd = LAshd + LeafDist(Stalk,Shade) 
          
            LAgnd = LAgnd + LeafDist(Stalk,Ground) 
      
            PlantLeafAreaTmp = PlantLeafAreaTmp  !for debugging FSR
     &                       + LeafDist(Stalk,Sun) 
     &                       + LeafDist(Stalk,Shade)
     &                       + LeafDist(Stalk,Ground)

      END DO !Stalk = 1,Smax

!-----------------------------------------------------------------------
! Re-calculate LAI for each canopy pseudo-zone

!!      DO Stalk = 1,Smax 

           LaiSunZone =  LAsun * PLTPOP * 0.0001 * 1 / FRSHAV

           LaiShdZone =  LAshd * PLTPOP * 0.0001 * 1 / FRSHAV
      
           LaiGndZone =  LAgnd * PLTPOP * 0.0001 * 1 / FRSHAV
      
!! 	END DO ! Stalk = 1,Smax 
!-----------------------------------------------------------------------
! Check whether canopy zones are under-filled, and if so move leaf area
! up a zone, beginning with older (lower number) stalks.

      DO Stalk = 1,Smax 

        IF (LaiSunZone <= LAITH1 .AND. 
     &			LeafDist(Stalk,Shade) > 0.)   THEN
            
          LeafDist(Stalk,Sun) = LeafDist(Stalk,Sun)
     &                          + CanLmtFac * LeafDist(Stalk,Shade)  

          LeafDist(Stalk,Shade) = (1 - CanLmtFac)  
     &                            * LeafDist(Stalk,Shade)

          LAsun = LAsun + CanLmtFac * LeafDist(Stalk,Shade)
          LAshd = LAshd - CanLmtFac * LeafDist(Stalk,Shade)

          LaiSunZone =  LAsun * PLTPOP * 0.0001 * 1 / FRSHAV
          LaiShdZone =  LAshd * PLTPOP * 0.0001 * 1 / FRSHAV

        ELSE IF (LaiSunZone + LaiShdZone <= LAITH2 .AND. 
     &			LeafDist(Stalk,Ground) > 0.)   THEN
            
          LeafDist(Stalk,Shade) = LeafDist(Stalk,Shade)
     &                          + CanLmtFac * LeafDist(Stalk,Ground)  

          LeafDist(Stalk,Ground) = (1 - CanLmtFac)  
     &                            * LeafDist(Stalk,Ground) 

          LAshd = LAshd + CanLmtFac * LeafDist(Stalk,Ground)
          LAgnd = LAgnd - CanLmtFac * LeafDist(Stalk,Ground)

          LaiShdZone =  LAshd * PLTPOP * 0.0001 * 1 / FRSHAV


        END IF
      
      END DO

      LAtmp(DAS) = LAsun + LAshd + LAgnd !FSR-temporary debug check  

!-----------------------------------------------------------------------
!            FSR-temporary debug check Was here

!-----------------------------------------------------------------------
! Calculate LI for each canopy pseudo-zone

      IF (LAsun > 0) THEN
        LIsunAvl = (1. - EXP(-KCAN * LaiSunZone))* FRSHAV
      ELSE 
          LIsunAvl = 0.
      END IF ! (LAsun > 0)

!--------
      IF (LAshd > 0) THEN
        LIshdAvl = ((1. - EXP(-KCAN*(LaiSunZone+LaiShdZone)))*FRSHAV)
     &               - LIsunAvl
      ELSE 
          LIshdAvl = 0.
      END IF ! (LAshd > 0)

!--------
      IF (LAgnd > 0) THEN
!!!		LIgndAvl = (LI - (LIsunAvl + LIshdAvl))* FRSHAV

        LIgndAvl 
     &           = ((1.- EXP(-KCAN*(LaiSunZone+LaiShdZone+LaiGndZone)))
     &           *  FRSHAV)
     &           - (LIsunAvl+LIshdAvl)

!!		LIgndAvl = (LIalt - (LIsunAvl + LIshdAvl))  
        LIgndAvl = MAX(LIgndAvl,0.) 

      ELSE 
           LIgndAvl = 0.
      END IF ! (LAgnd > 0)

!-----------------------------------------------------------------------
!     Distribute LI among stalks according to canopy zone and stalk
!     leaf area.  

      IF (LeafAreaPlant(DAS-1) > 0) THEN !DAS-1 since DAS not calculated
                                         !until CSP_Grow_Cane is called
        
        DO Stalk = 1,Smax 
!          StkLI(Stalk) = 0.0  !Initialize daily to zero
!          StkPG(Stalk) = 0.0  


          IF (LeafDist(Stalk,Sun) > 0) THEN


            StkLI(Stalk,Sun) = LIsunAvl  * 1/PLTPOP 
     &	                   * LeafDist(Stalk,Sun)/LAsun

          END IF !(LeafDist(Stalk,Sun) .GT. 0)
                 

          IF (LeafDist(Stalk,Shade) > 0) THEN

            StkLI(Stalk,Shade) = LIshdAvl * 1/PLTPOP 
     &	                    * LeafDist(Stalk,Shade)/LAshd
          END IF !(LeafDist(Stalk,Shade) .GT. 0)


          IF (LeafDist(Stalk,Ground) > 0) THEN
            StkLI(Stalk,Ground) = LIgndAvl * 1/PLTPOP 
     &	                     * LeafDist(Stalk,Ground)/LAgnd

          END IF !(LeafDist(Stalk,Ground) .GT. 0)


          DO Zone = 1,3 

            StkPG(Stalk,Zone) = PTSMAX * SLPF * StkLI(Stalk,Zone)
     &                 * TLI * E_FAC * PGSLW * PRATIO * PGLFMX * SWFAC
 
            LITOTAL = LITOTAL + StkLI(Stalk,Zone) * PLTPOP!Tmp for debug
            PGT     = PGT     + StkPG(Stalk,Zone) * PLTPOP 

          END DO ! Zone = 1,Ground

        END DO ! Stalk = 1,Smax 

        LIallZone = LIsunAvl + LIshdAvl + LIgndAvl 

!	PGT = 0.
      END IF ! (LeafAreaPlant(DAS-1) > 0)
!-----------------------------------------------------------------------
!            FSR-temporary debug check
!      The following two LAI calculations will not be equal:
!      XHLAI is equal to LAI2 at this point in the program, but   
!      equals LAI after a call to CSP_GROW_CANE).  LI is
!      calculated from XHLAI (input) and LITOTAL from StkLI
!      (output)

      LAI2 = PLTPOP * LeafAreaPlant(DAS-1) / 10000 
      LaiAllZone  = (LaiSunZone + LaiShdZone + LaiGndZone)
      LAItmp(DAS) =  LAtmp(DAS) * PLTPOP * 0.0001  !  LaiAllZone
!	LAItmp(DAS) =  LaiAllZone
      XLAI = LAtmp(DAS-1) * PLTPOP * 0.0001 / FRSHAV 

!-----------------------------------------------------------------------
! Eliminate the production of CH2O while stalk is very small.

!!!                  IF (LeafNum(DAS,Stalk) <= 2) THEN !DAS-1
!!!                      StkPG(Stalk) = 0.0  !Not applied to PG calculation
!!!                  END IF !(LeafNum(DAS,Stalk) <= 2)

! For PG to equal sum of StkPG(Stalk), must eliminate EXCESS factor 
! (below), as well as the code above which stops production of CH2O when 
! stalks are very young.  Neither of these are applied to PG, since  
! EXCESS is now a (Stalk) array, and StkPG(j) = 0.0 was not applied 
! to PG. To test the equivalence of PG and PGT, these two lines must be 
! deactiviated. (Compare PG to PGT in CSP_PHOTO.OUT.)  FSR
! 
!!!       StkPG(Stalk) = StkPG(Stalk) * EXCESS(Stalk)!Not applied to PG
! EXCESS deactivated.  If it is to be restored, it needs to accomodate 
! canopy zones.  This may involve applying it only to the zone which is
! most likely to cause excess production, Zone 1.  On the other hand, maybe
! the photosynthesis calculation should be refined, instead of EXCESS.
! 
!-----------------------------------------------------------------------
C-----------------------------------------------------------------------
!    Calculate ratio for assigning shoot carbon costs to existing stalks
      DO Stalk = 1,Smax 

        TotalPG(stalk) = 0.

          DO Zone = 1,3 

               TotalPG(stalk) = TotalPG(stalk) + StkPG(Stalk,Zone)
             
          END DO ! Zone = 1,Ground 

          IF (TotalPG(stalk) > 0 .AND. PGT > 0)  THEN

               PgRatio(stalk) = TotalPG(Stalk) / (PGT/PLTPOP)

          ELSE

               PgRatio(Stalk) = 0.0 
             
          ENDIF ! (StkPG(Stalk) > 0 .AND. PGT >0)
        END DO ! Stalk = 1,Smax 

!---------------END STALK-BASED PHOTOSYNTHESIS METHOD---------------
!***********************************************************************
      IF (INDEX('Y'  ,ISWITCH%IDETG) > 0 .AND. 
     &    INDEX('YDA',ISWITCH%IDETL) > 0) THEN

        WRITE(PLUN,'(/1X,I7,1X,I4,10(1X,F5.2),2(1X,F6.2),3(1X,F4.1),
     &  2(1X,F6.2),4(1X,F5.2),1X,F6.2,7(1X,F6.2))',
     &  ADVANCE="NO"),  
     &  YRDOY, DAS, XHLAI, LaiAllZone, SHLAI, FRSHAV, LIalt, 
     &  LI, LITOTAL, LIsunAvl, LIshdAvl,
     &  LIgndAvl,PG, PGT, LAsun/10000, LAshd/10000, LAgnd/10000, 
     &  EXCESS(1), PAR, RNITP, SLAAD, SWFAC, TAVG, AGEFAC, PTSMAX, 
     &  SLPF, TLI, AGEFCC, PGSLW, PRATIO, PGLFMX

!!!      DO Stalk = 1,Smax
!!!        WRITE(PLUN,'(2X,F4.2)', ADVANCE="NO") StkLI(Stalk)
!!!      END DO

        DO Stalk = 1,Smax
          WRITE(PLUN,'(2X,F4.1)', ADVANCE="NO") TotalPG(Stalk)
        END DO
      ENDIF

!     PG = PG * EXCESS ! EXCESS is now an array (Stalk)
!      ENDIF     !(DYNAMIC .EQ. INTEGR)
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
!!!      LITOTAL = 0.0 ! Temporary for debugging
      ENDIF
!***********************************************************************
      RETURN
      END !SUBROUTINE CSP_PHOTO

C=======================================================================
C  CSP_PHOTIP, Subroutine, N.B. Pickering
C  Read input parameters for daily photosynthesis.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/25/99     Written.
!-----------------------------------------------------------------------
C  Output:
C  Local :
!-----------------------------------------------------------------------
!  Called from: CSP_PHOTO
!  Calls : None
C=======================================================================

      SUBROUTINE CSP_PHOTIP(FILEIO,
     &  CCEFF, CCMAX, CCMP, FNPGN, FNPGT, LIsun,  !, KCAN
     &  LIshd, LNREF, PARMAX, PGREF,    !, LMXSTD, PHTHRS10
     &  PHTMAX, PLTPOP, ROWSPC, TYPPGN, TYPPGT, XPGSLW, 
     &  YPGSLW)

!-----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL ERROR, FIND, GETLUN, IGNORE

      CHARACTER*1  BLANK
      CHARACTER*3  TYPPGN, TYPPGT
      CHARACTER*6  ERRKEY, SECTION
      CHARACTER*12 FILEC
      CHARACTER*30 FILEIO
      CHARACTER*80 PATHCR,CHAR
      CHARACTER*92 FILECC

      INTEGER LUNIO, LINC, LNUM, FOUND
      INTEGER II, PATHL, LUNCRP, ERR, ISECT

      REAL CCEFF, CCMAX, CCMP, LIshd, LIsun,  !, KCAN
     &  LNREF, PARMAX, PGREF, PHTMAX, !LMXSTD, PHTHRS10, 
     &  PLTPOP, ROWSPC, XPGSLW(15)   !, SLPF

      REAL FNPGN(4),FNPGT(4)
      REAL YPGSLW(15)

      PARAMETER (BLANK  = ' ')
      PARAMETER (ERRKEY = 'PHOTIP')

!-----------------------------------------------------------------------
!     Open and read FILEIO (DSSAT45.INP)
!-----------------------------------------------------------------------
      CALL GETLUN('FILEIO', LUNIO)
      OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)

!-----------------------------------------------------------------------
      READ(LUNIO,50,IOSTAT=ERR) FILEC, PATHCR; LNUM = 7
   50 FORMAT(//////,15X,A12,1X,A80)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

!-----------------------------------------------------------------------
C    Read Planting Details Section
!-----------------------------------------------------------------------
      SECTION = '*PLANT'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ(LUNIO,'(24X,F6.1,12X,F6.0)',IOSTAT=ERR)
     &       PLTPOP, ROWSPC ; LNUM = LNUM + 1
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
      ENDIF

!-----------------------------------------------------------------------
C    Read Cultivars Section
!-----------------------------------------------------------------------
        SECTION = '*CULTI'
        CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILEIO, LNUM)
        ELSE

          READ(LUNIO,'(36X,F6.0,60X,2F6.3)',IOSTAT=ERR) PHTMAX, LIsun, 
     &                                                  LIshd 
		LNUM = LNUM + 1

          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
        ENDIF

      CLOSE (LUNIO)
C-----------------------------------------------------------------------
!     Open FILEC (.SPE or Species) Input file
C-----------------------------------------------------------------------
      LNUM = 0
      PATHL  = INDEX(PATHCR,BLANK)
      IF (PATHL .LE. 1) THEN
        FILECC = FILEC   ! species file
      ELSE
        FILECC = PATHCR(1:(PATHL-1)) // FILEC
      ENDIF
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)
        LNUM = 0
C-----------------------------------------------------------------------
C READ PHOTOSYNTHESIS PARAMETERS *******************  
C-----------------------------------------------------------------------
        SECTION = '*#PHOT'
        CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILECC, LNUM)
        ELSE      
        
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR) !KCAN

          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          READ(CHAR,'(12X,F6.3)',IOSTAT=ERR) PARMAX 
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          READ(CHAR,'(12X,F6.3)',IOSTAT=ERR) CCMP
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          READ(CHAR,'(12X,F6.3)',IOSTAT=ERR) CCMAX
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          READ(CHAR,'(12X,F6.4)',IOSTAT=ERR) CCEFF
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          READ(CHAR,'(12X,4F6.2)',IOSTAT=ERR) (FNPGN(II),II=1,4)
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          READ(CHAR,'(14X,A3)',IOSTAT=ERR) TYPPGN
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          READ(CHAR,'(12X,4F6.2)',IOSTAT=ERR) (FNPGT(II),II=1,4)
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          READ(CHAR,'(14X,A3)',IOSTAT=ERR) TYPPGT
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR) !XLMAXT
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR) 
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR) !NSLOPE

          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          READ(CHAR,'(14X,F6.3)',IOSTAT=ERR) LNREF
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          READ(CHAR,'(14X,F6.3)',IOSTAT=ERR) PGREF
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          READ(CHAR,'(13X,10F6.3)',IOSTAT=ERR) (XPGSLW(II),II=1,10)
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          READ(CHAR,'(13X,10F6.3)',IOSTAT=ERR) (YPGSLW(II),II=1,10)
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        ENDIF
C-----------------------------------------------------------------------
      CLOSE (LUNCRP)

      END !SUBROUTINE PHOTIP
!=======================================================================
! Variable definitions for PHOTO and PHOTIP
!=======================================================================
! AGEFAC   Relative effect of current leaf N on canopy photosynthesis (0-1)
!            (fraction)
! AGEFCC   Effect of AGEFAC on photosynthesis 
! AGEREF   Reference value calculated at reference leaf N value to 
!            normalize the effect of N for different plant species 
! BETN     Spacing between plants along a row (m / plant)
! CanLmtFac Proportion of stalk leaf area that can be moved UP into a 
!           de-populated canopy light interception zone.   
! BLANK    Blank character 
! CCEFF    Relative efficiency of CO2 assimilation used in equation to 
!            adjust canopy photosynthesis with CO2 concentrations 
! CCK      Computed exponent for relationship between CO2 and canopy 
!            photosynthesis (=CCEFF / CCMAX) 
! CCMAX    Maximum daily canopy photosynthesis relative to photosynthesis 
!            at a CO2 concentration of 330 vpm 
! CCMP     Canopy CO2 compensation point (CO2 at which daily PG is 0.0) 
! CHAR     Contains the contents of last record read 
! CO2      Atmospheric carbon dioxide concentration (µmol[CO2] / mol[air])
! COLDSTR  Cold weather stress factor for photosynthesis (not currently 
!            used) 
! CUMSTR   Cumulative stress factor for photosynthesis after start of seed 
!            production 
! CURV     Function subroutine 
! DAS      Days after start of simulation (d)
! DXR57    Relative time between first seed (NR5) and physiological 
!            maturity (NR7) 
! DYNAMIC  Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
!            INTEGR, OUTPUT, or FINAL 
! ERR      Error code for file operation 
! ERRKEY   Subroutine name for error file 
! EXCESS(j) Factor based on excess PG used to affect tomorrow's PG 
!            calculation for stalk j 
! FILEC    Filename for SPE file (e.g., SBGRO980.SPE) 
! FILECC   Path plus filename for species file (*.spe) 
! FILEIO   Filename for input file (e.g., IBSNAT35.INP) 
! FNPGN(I) Critical leaf N concentration for function to reduce 
!            photosynthesis due to low leaf N levels (4 values for function 
!            CURV) 
! FNPGT(I) Critical values of temperature for the functions to reduce 
!            canopy PG under non-optimal temperatures (in function CURV) 
! FOUND    Indicator that good data was read from file by subroutine FIND 
!            (0 - End-of-file encountered, 1 - NAME was found)
! GP1      Number of stalks in pseudo-zone 1 (2,3)  
! ISECT    Indicator of completion of IGNORE routine: 0 - End of file 
!            encountered, 1 - Found a good line to read, 2 - End of Section 
!            in file encountered denoted by * in column 1.  
! KCAN     Canopy light extinction coefficient for daily PAR, for 
!            equidistant plant spacing, modified when in-row and between 
!            row spacing are not equal 
! KCANR    Canopy light extinction coefficient, reduced for incomplete 
!            canopy 
! KC_SLOPE Slope of relationship between reduced Kcan (KCANR) and Kcan.
!           If no value is specified in the species file, then 0.1 is used.
! LAsun    Leaf area in light-interception zone 1 
! LAshd    Leaf area in light-interception zone 2
! LAgnd    Leaf area in light-interception zone 3
! LFMAX   Maximum leaf photosynthesis rate at 30 C, 350 vpm CO2, and high light
!         (mg CO2/m2-s) Related to LMXSTD below. 
! LIgndAvl Light Interception avail. at the inner-most canopy pseudo-zone
! LIshdAvl Light Interception avail. at the middle-level canopy pseudo-zone
! LIsunAvl Light Interception avail. at the outer-most canopy pseudo-zone
! LIshd    Parameter read from SPE file, used to calculate LAITH2   
! LIsun    Parameter read from SPE file, used to calculate LAITH1  
! LAI      Leaf Area Index calculated from sum of stalks; compare to XHLAI
! LITOTAL  Sum of Light Interception of each stalk
! LAITH1   LAI target corresponding to LIsun; the outer-most canopy zone 
! LAITH2   LAI target corresponding to LIshd; the middle canopy zone 
! LaiSunZone  LAI in the outer-most canopy zone 
! LaiShdZone  LAI in the mid canopy zone have LAI = LaiShdZone-LaiSunZone     
! LINC     Line number of input file. 
! LMXSTD   Maximum leaf photosyntheses for standard cultivar.  See LFMAX above. 
! LNREF    Value of leaf N above which canopy PG is maximum (for standard 
!            cultivar) 
! LNUM     Current line number of input file 
! LUNCRP   Logical unit number for FILEC (*.spe file) 
! LUNIO    Logical unit number for FILEIO 
! NR5      Day when 50% of plants have pods with beginning seeds (days)
! PAR      Daily photosynthetically active radiation or photon flux density
!            (moles [quanta] / m2-d)
! PARMAX   Value of PAR at which photosynthesis is 63% of the maximum value 
!            (PHTMAX). Used in daily canopy photosynthesis calculations
!            (moles[quanta]/m2-d)
! PATHCR   Pathname for SPE file or FILEE. 
! PATHL    Number of characters in path name (path plus filename for FILEC)
!            
! PG       Daily gross photosynthesis from LIalt (g[CH2O] / m2 / d)
! PGT      Daily gross photosynthesis from sum of stalk PG (g[CH2O] / m2 / d)
! LI    Multiplier to compute daily canopy PG as a function of leaf area 
!            index (LAI) 
! PGLFMX   Multiplier for daily canopy photosynthesis to account for 
!            cultivar differences in leaf photosynthesis capabilities 
! PGREF    Reference value for leaf level photosynthesis used in canopy 
!            light response curve (µmol[CO2] / m2-s)
! PGSLW    Relative effect of leaf thickness (SLW) on daily canopy PG 
! PHTHRS10 Threshold time that must accumulate in phase 10 for the next 
!            stage to occur.  Equivalent to PHTHRS(10) in Subroutine 
!            PHENOLOG. 
! PHTMAX   Maximum amount of CH20 which can be produced if 
!            photosynthetically active radiation (PAR) is very high (3 
!            times PARMAX) and all other factors are optimal (g[CH2O]/m2-d)
! PRATIO   Relative effect of CO2 on canopy PG, for multiplying by PG 
!            computed for 330 vpm 
! PTSMAX   Potential amount of CH20 which can be produced at the specified 
!            PAR for full canopy (LAI>8), all other factors optimal
!            (g[CH2O]/m2-d)
! RNITP    True nitrogen concentration in leaf tissue for photosynthesis 
!            reduction. (%)
! ROWSPC   Row spacing (m)
! SECTION  Section name in input file 
! SHLAI    Shaded area LAI (after canopy closes, equal to XHLAI)
! SLAAD    Specific leaf area, excluding weight of C stored in leaves
!            (cm2[leaf] / g[leaf])
! SLPF     Empirical multiplier to adjust daily canopy PG due to unknown 
!            soil or environmental factors that persist in a given location 
! SLW      Specific leaf weight (g[leaf] / m2[leaf])
! SPACNG   Ratio of distance between plants in a row to distance between 
!            rows (or vice versa - always < 1)
! StkPG(j) Daily gross photosynthesis by stalk j (g[CH2O] / stalk / d) 
! SWFAC    Effect of soil-water stress on photosynthesis, 1.0=no stress, 
!            0.0=max stress 
! TABEX    Function subroutine - Lookup utility 
! NR TDAY  Average temperature during daylight hours (°C)
! TAVG     Average daily temperature (°C)
! TIMDIF   Integer function which calculates the number of days between two 
!            Julian dates (da)
! TLI   Reduction in specific leaf area due to daytime temperature being 
!            less than optimal (0-1) 
! TYPPGN   Type of function for the leaf N effects on PG 
! TYPPGT   Character variable specifying the type of function to use for 
!            the relationship between temperature and PG (for use in 
!            function subroutine CURV) 
! XHLAI    Green leaf area index (m2[leaf] / m2[ground])
! XLAI     Same as XHLAI
! XPGSLW(I) Array of SLW values for table look-up function, used with YPGSLW
!            (g[leaf] / m2[leaf])
! XPOD     Growth partitioning to pods which slows node appearance
!            (fraction)
! YPGSLW(I) Array of PG values corresponding to SLW values in array XPGSLW
!            (g[CH2O] / m2 / d)
! YRDOY    Current day of simulation (YYDDD)
! YRSIM    Start of simulation date (YYDDD)
!=======================================================================
! END SUBROUTINE CSP_PHOTO
!=======================================================================
