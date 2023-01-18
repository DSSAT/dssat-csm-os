C=======================================================================
C  OpPlantP, Subroutine, C.H.Porter
C-----------------------------------------------------------------------
C  Generates output file for daily plant phosphorus data
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  03/25/2004 CHP Written
C-----------------------------------------------------------------------
C  Called by: P_PLANT
C  Calls:     None
!=======================================================================
      SUBROUTINE OpPlantP(DYNAMIC, MDATE, YRPLT, 
     &  PConc_Shut_opt, PConc_Root_opt, PConc_Shel_opt, PConc_Seed_opt,
     &  PConc_Shut_min, PConc_Root_min, PConc_Shel_min, PConc_Seed_min,
     &  PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, PConc_Plant,
     &  PShut_kg, PRoot_kg, PShel_kg, PSeed_kg, PPlant_kg, 
     &  Shut_kg, Root_kg, Shel_kg, Seed_kg, N2P, PTotDem,  
     &  SenSoilP, SenSurfP, PhFrac1, PhFrac2, 
     &  PStres1, PStres2, PSTRESS_RATIO, PUptakeProf,
     &  PestShutP, PestRootP, PestShelP, PestSeedP)

!-----------------------------------------------------------------------
      USE ModuleDefs 
      USE ModuleData
!     VSH
      USE CsvOutput 
      USE Linklist
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, TIMDIF, YR_DOY
      SAVE
!-----------------------------------------------------------------------
      CHARACTER*1  IDETL, IDETP, ISWPHO
      CHARACTER*2  CROP
      CHARACTER*12 OUTP
      CHARACTER*13 PPBAL

      INTEGER COUNT, DAP, DAS, DOY, DYNAMIC, ERRNUM, FROP
      INTEGER LUNPPC, NOUTDP,RUN
      INTEGER TIMDIF, YEAR, YRDOY, MDATE, YRPLT

      REAL PConc_Shut_opt, PConc_Root_opt, PConc_Shel_opt,PConc_Seed_opt
      REAL PConc_Shut_min, PConc_Root_min, PConc_Shel_min,PConc_Seed_min
      REAL PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, PConc_Plant
      REAL PShut_kg, PRoot_kg, PShel_kg, PSeed_kg, PPlant_kg 
      REAL Shut_kg, Root_kg, Shel_kg, Seed_kg 
      REAL SenSoilP, SenSurfP, PStres1, PStres2, PUptakeProf
      REAL PSTRESS_RATIO
      REAL CumSenSurfP, CumSenSoilP !cumul. senes. P soil and surface
      REAL DayPBal, CumBal
      REAL InitPlusAdds, FinalPlusSubs
      REAL PPLANTinit, PPlant_Y
      REAL PS1_AV, PS2_AV
      REAL PUptake_Cum
      REAL PhFrac1, PhFrac2
      REAL N2P, PTotDem
      REAL PestShutP, PestRootP, PestShelP, PestSeedP
      REAL PestShutCumP, PestRootCumP, PestShelCumP, PestSeedCumP
      REAL CumSenTot, CumPestTot, FinalP, PBalance, DayPestP

      LOGICAL FEXIST, FIRST

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH

!     Get CONTROL and ISWITCH info
      CALL GET(CONTROL)
      CALL GET(ISWITCH)
           
      FMOPT = ISWITCH % FMOPT   ! VSH

!     No output for fallow crop
      CROP    = CONTROL % CROP
      IDETL   = ISWITCH % IDETL
      IDETP   = ISWITCH % IDETP
      ISWPHO  = ISWITCH % ISWPHO
      IF (CROP   .EQ. 'FA' .OR. 
     &    IDETP  .EQ. 'N'  .OR.  
     &    IDETL  .EQ. 'N'  .OR.
     &    IDETL  .EQ. '0'  .OR.
     &    ISWPHO .EQ. 'N') RETURN

!     Transfer values from constructed data types into local variables.
!     NOTE: DYNAMIC variable is passed as argument -- needed to get value at
!       emergence (DYNAMIC = 'EMERG')
      DAS     = CONTROL % DAS
      FROP    = CONTROL % FROP
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
!     Initialize daily growth output file
      OUTP  = 'PlantP.OUT'
      CALL GETLUN(OUTP, NOUTDP)
      INQUIRE (FILE = OUTP, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = NOUTDP, FILE = OUTP, STATUS = 'OLD',
     &    IOSTAT = ERRNUM, POSITION = 'APPEND')
        FIRST = .FALSE.
      ELSE
        OPEN (UNIT = NOUTDP, FILE = OUTP, STATUS = 'NEW',
     &    IOSTAT = ERRNUM)
        WRITE(NOUTDP,'("*Plant Phosphorus Daily Output")')
        FIRST = .TRUE.
      ENDIF

      !Write headers
      CALL HEADER(SEASINIT, NOUTDP, RUN)
      WRITE (NOUTDP,50)
   50 FORMAT('@YEAR DOY   DAS   DAP',
!       Optimum P conc. (shoot, root, shell, seed)
     &  '   PSHOD   PRTOD   PSLOD   PSDOD', 
!       Minimum P conc. (shoot, root, shell, seed) 
     &  '   PSHMD   PRTMD   PSLMD   PSDMD', 
!       P Conc. (shoot, root, shell, seed)
     &  '   SHPPD   RTPPD   SLPPD   SDPPD   PLPPD'
!       P Mass (shoot, root, shell, seed)
     &  '   SHPAD   RTPAD   SLPAD   SDPAD    PPAD',
!       P stresses, uptake, senescence
     &  '   PST1A   PST2A    PUPD    PUPC   SNP0C   SNP1C',
!       Fraction of vegetative and reproductive phases
     &  '   PHFR1   PHFR2',
!       Plant weights (shoot, root, shell, seed)
     &  '   SHWAD    RWAD    SHAD    GWAD    PSTRAT   NTOPD',
!       Total P demand
     &  '    PTDD')
      END IF   ! VSH

      CumSenSurfP = 0.0
      CumSenSoilP = 0.0   
      PUptake_Cum = 0.0

      PestShutCumP = 0.0
      PestRootCumP = 0.0
      PestShelCumP = 0.0
      PestSeedCumP = 0.0

      PPlantInit = PPlant_kg

!     ------------------------------------------------------------------
!     Seasonal Plant P balance.
      PPBAL = 'PlantPBal.OUT'
      CALL GETLUN(PPBAL, LUNPPC)
      INQUIRE (FILE = PPBAL, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = LUNPPC, FILE = PPBAL, STATUS = 'OLD',
     &    IOSTAT = ERRNUM, POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = LUNPPC, FILE = PPBAL, STATUS = 'NEW',
     &    IOSTAT = ERRNUM)
        WRITE(LUNPPC,'("*PLANT P BALANCE")')
      ENDIF

      CALL HEADER(SEASINIT, LUNPPC, RUN)

!     ------------------------------------------------------------------
!     Optional daily P balance (if IDETL = 'D' or 'A')
      IF (INDEX('AD',IDETL) > 0) THEN
        PPLANT_Y = PPLANTinit
        WRITE(LUNPPC, 80)
   80   FORMAT('@YEAR DOY   DAS   DAP',
     &  '   TOTPAD    SHPAD    RTPAD    SLPAD    SDPAD',
     &  '     PUPD    SNP0D    SNP1D    PPest     SPBAL    CUMBAL')
      ENDIF

      PS1_AV = 0.0
      PS2_AV = 0.0
      COUNT = 0

!***********************************************************************
!***********************************************************************
!     EMERGENCE
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. EMERG) THEN
C-----------------------------------------------------------------------
      PPlantInit = PPlant_kg

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
C-----------------------------------------------------------------------
C   CHECK FOR OUTPUT FREQUENCY
C-----------------------------------------------------------------------
!     Don't print prior to planting date
      IF (YRDOY .LT. YRPLT .OR. YRPLT .LT. 0) RETURN

!     Accumulate seasonal P uptake
      PUptake_Cum = PUptake_Cum + PUptakeProf

!     Compute average stress factors since last printout
      PS1_AV = PS1_AV + (1.0 - PSTRES1)
      PS2_AV = PS2_AV + (1.0 - PSTRES2)
      COUNT = COUNT + 1

!     Accumulate P in senesced matter for surface and soil.
      CumSenSurfP = CumSenSurfP + SenSurfP
      CumSenSoilP = CumSenSoilP + SenSoilP

!     Accumulate P lost to pest damage.
      PestShutCumP = PestShutCumP + PestShutP
      PestRootCumP = PestRootCumP + PestRootP
      PestShelCumP = PestShelCumP + PestShelP
      PestSeedCumP = PestSeedCumP + PestSeedP

      IF ((MOD(DAS,FROP) .EQ. 0)          !Daily output every FROP days,
     &  .OR. (YRDOY .EQ. YRPLT)           !on planting date, and
     &  .OR. (YRDOY .EQ. MDATE)) THEN     !at harvest maturity 
!       Print 
        DAP = MAX(0,TIMDIF(YRPLT,YRDOY))
        CALL YR_DOY(YRDOY, YEAR, DOY) 

!       Compute average stress factors since last printout
        IF (COUNT > 0) THEN
          PS1_AV = PS1_AV / COUNT
          PS2_AV = PS2_AV / COUNT
          COUNT = 0
        ENDIF
      
      IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
        WRITE (NOUTDP,100) YEAR, DOY, DAS, DAP, 
     &    PConc_Shut_opt*100., PConc_Root_opt*100., 
     &    PConc_Shel_opt*100., PConc_Seed_opt*100., 
     &    PConc_Shut_min*100., PConc_Root_min*100., 
     &    PConc_Shel_min*100., PConc_Seed_min*100., 
     &    PConc_Shut*100., PConc_Root*100., 
     &    PConc_Shel*100., PConc_Seed*100., PConc_Plant*100.,  
     &    PShut_kg, PRoot_kg, PShel_kg, PSeed_kg, PPlant_kg, 
     &    PS1_AV, PS2_AV, 
     &    PUptakeProf, PUptake_Cum, CumSenSurfP, CumSenSoilP,
C-GH     &    Min(1.0,PhFrac1), Min(1.0,PhFrac2),
     &    PhFrac1,PhFrac2,
     &    NINT(Shut_kg), NINT(Root_kg), NINT(Shel_kg), NINT(Seed_kg)
     &    , PSTRESS_RATIO, N2P, PTotDem
  100   FORMAT(1X,I4,1X,I3.3,2(1X,I5),
     &       20(F8.3),F8.4,5F8.3,4I8  !
     &       , F10.3, 2F8.2)
      END IF   ! VSH

!       Set average stress factors since last printout back to zero
        PS1_AV = 0.0
        PS2_AV = 0.0
      
      ENDIF

!     VSH
      IF (FMOPT == 'C') THEN   ! VSH 
         CALL CsvOutPlantP(EXPNAME, CONTROL%RUN, CONTROL%TRTNUM, 
     &CONTROL%ROTNUM, CONTROL%REPNO, YEAR, DOY, DAS, DAP, 
     &PConc_Shut_opt, PConc_Root_opt, PConc_Shel_opt, PConc_Seed_opt, 
     &PConc_Shut_min, PConc_Root_min, PConc_Shel_min, PConc_Seed_min, 
     &PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, PConc_Plant, 
     &PShut_kg, PRoot_kg, PShel_kg, PSeed_kg, PPlant_kg, PS1_AV, PS2_AV,
     &PUptakeProf, PUptake_Cum, CumSenSurfP, CumSenSoilP, PhFrac1, 
     &PhFrac2, Shut_kg, Root_kg, Shel_kg, Seed_kg, PSTRESS_RATIO, N2P, 
     &PTotDem,    
     &vCsvlinePlantP, vpCsvlinePlantP, vlngthPlantP)
     
         CALL LinklstPlantP(vCsvlinePlantP)
      END IF
      
!     -------------------------------------------------------------
!     Daily P balance
      IF (INDEX('AD',IDETL) > 0) THEN

        CumSenTot = CumSenSurfP + CumSenSoilP
        CumPestTot = PestShutCumP + PestRootCumP + PestShelCumP 
     &                + PestSeedCumP
        DayPestP = PestShutP + PestRootP + PestShelP + PestSeedP

        DayPBal = PPlant_kg - PPlant_Y        !Change
     &    - PUptakeProf                       !Additions
     &    + SenSurfP + SenSoilP + DayPestP    !Subtractions

        IF ((PPlant_kg - PPLANTinit) .LT. 1.E-6) THEN
          DayPBal = DayPBal - PPLANTinit
        ENDIF

        InitPlusAdds = PPLANTinit + PUptake_Cum
        FinalPlusSubs = PPlant_kg + CumSenTot + CumPestTot
        CumBal = FinalPlusSubs - InitPlusAdds

        WRITE(LUNPPC, 300)  YEAR, DOY, DAS, DAP, 
     &    PPlant_kg, PShut_kg, PRoot_kg, PShel_kg, PSeed_kg, 
     &    PUptakeProf, SenSurfP, SenSoilP, DayPestP, DayPBal, CumBal
  300   FORMAT(1X,I4,1X,I3.3,2(1X,I5),9F9.5, 2F10.6)

        PPlant_Y = PPlant_kg
      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal Output 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
      !Close daily output file.
      CLOSE (NOUTDP)

!     ------------------------------------------------------------------
      CumSenTot = CumSenSurfP + CumSenSoilP
      CumPestTot = PestShutCumP +PestRootCumP +PestShelCumP+PestSeedCumP
      FinalP = PShut_kg + PRoot_kg + PShel_kg + PSeed_kg
      PBalance = PPlantInit + PUptake_Cum -CumSenTot -CumPestTot -FinalP
      PBalance = FinalP - PPlantInit - PUptake_Cum 
     &                    + CumSenTot + CumPestTot

      WRITE (LUNPPC,510) PPlantInit, PUptake_Cum,      !Initial & Uptake
     &    CumSenTot, CumSenSurfP, CumSenSoilP,                !Senesced
     &    CumPestTot, PestShutCumP, PestRootCumP, 
     &        PestShelCumP, PestSeedCumP,                     !Pest
     &    FinalP, PShut_kg, PRoot_kg, PShel_kg, PSeed_kg,     !Final
     &    PBalance                                            !Balance

  510 FORMAT (//,' Seasonal Plant P balance (kg[P]/ha)',/,
     &    T36,'   Total   Shoot    Root   Shell    Seed',/,
     &    ' Initial plant P (at emergence)    ',F8.3,/,       !Initial
     &    ' P uptake from soil               +',F8.3,/,       !Uptake
     &    ' P losses:',/,
     &    '   Senesced / freeze damage       -',3F8.3,/,      !Senesced
     &    '   Pest / disease damage          -',5F8.3,/,      !Pest
     &    ' P in plant tissue at harvest     =',5F8.3,//,     !Final
     &    ' Total P balance                   ',F8.3)         !Balance
    
      WRITE (LUNPPC,'(/,80("*"))')

!     Close seasonal output file.
      CLOSE (UNIT = LUNPPC)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE OpPlantP
!=======================================================================


!=======================================================================
! OpPlantP variables -- updated 9/29/2004
!=======================================================================
! CONTROL        Composite variable containing variables related to control 
!                  and/or timing of simulation.    See Appendix A. 
! COUNT          Number of pest columns found in FILET 
! CROP           Two-character crop identification code 
! CUMBAL         Cumulative seasonal balance (var.)
! CumSenSoilP    Seasonal cumulative loss of P in senesced roots (kg[P]/ha)
! CumSenSurfP    Seasonal cumulative loss of P in senesced shoots
!                 (kg[P]/ha)
! DAP            Number of days after planting (d)
! DAS            Days after start of simulation (d)
! DAYPBAL        Daily P balance (kg[P]/ha)
! DOY            Current day of simulation (d)
! ERRNUM         Error number for input 
! FEXIST         Logical variable 
! FinalPlusSubs  Right hand side of seasonal balance (var)
! FIRST          Indicates first call to subroutine (true or false) 
! FROP           Frequency of output (d)
! IDETL          Switch for detailed printout (Y or N) 
! IDETP          Code to generate OUTP file (e.g., SoilP.OUT), Y or N 
! InitPlusAdds   Left hand side of seasonal balance (var)
! ISWITCH        Composite variable containing switches which control flow 
!                  of execution for model.  See Appendix A. 
! ISWPHO         Phosphorus simulation switch (Y or N) 
! LUNPPC         Logical output unit for phosphorus report 
! MDATE          Harvest maturity date (YYYYDDD)
! NOUTDP         Logical unit number for output file OUTP (e.g., 
!                  PHOSPHORUS.OUT, not currently used) 
! OUTP           Plant phosphorus daily output filename 
! PConc_Plant    P concentration in whole plant (g[P]/g[plant])
! Pconc_Root     P concentration in root (g[P]/g[root])
! PConc_Root_min Minimum P concentration in root (g[P]/g[root])
! Pconc_Root_opt Optimum P concentration in root (g[P]/g[root])
! PConc_Seed     Concentration of P in seed (g[P]/g[seed])
! PConc_Seed_min Minimum P concentration in seed (g[P]/g[seed])
! PConc_Seed_opt Optimum P concentration in seed (g[P]/g[seed])
! PConc_Shel     Concentration of P in shell (g[P]/g[shell])
! PConc_Shel_min Minimum P concentration in shell (g[P]/g[shell])
! PConc_Shel_opt Optimum P concentration in shell (g[P]/g[shell])
! Pconc_Shut     P concentration in shoots (leaf and stem) (g[P]/g[shoot])
! PConc_Shut_min Minimum P concentration in leaf and stem (g[P]/g[shoot])
! PConc_Shut_opt Optimum P concentration in leaf and stem (g[P]/g[shoot])
! PhFrac1      Fraction of physiological time which has occurred between 
!                  first and second critical phases for computation of 
!                  optimum and minimum plant P concentrations 
! PhFrac2      Fraction of physiological time which has occurred between 
!                  second and third critical phases for computation of 
!                  optimum and minimum plant P concentrations 
! PPBAL          Plant phosphorus balance output filename 
! PPlant_kg      P content in whole plant (kg[P]/ha)
! PPlant_Y       Yesterday's summation of P in plant parts (kg[P]/ha)
! PPLANTinit     Phosphorus content in whole plant at emergence or 
!                  transplant (kg[P]/ha)
! PRoot_kg       P in roots (kg[P]/ha)
! PS1_AV         Average P stress since last printout 
! PS2_AV         Average P stress since last printout 
! PSeed_kg       P content in seed (kg[P]/ha)
! PShel_kg       P content in shell (kg[P]/ha)
! PShut_kg       P in shoots (leaf and stem) (kg[P]/ha)
! PStres1        P stress which affects vegetative partitioning   
!                  (1=no stress, 0=max stress) 
! PStres2        P stress factor for reducing photosynthate (1=no stress, 
!                  0=max stress) 
! PUptake_cum    Seasaonal cumulative P uptake by plant (kg[P]/ha)
! PUptakeProf     Plant uptake of P over whole soil profile (kg[P]/ha/d)
! RUN            Change in date between two observations for linear 
!                  interpolation 
! SenSoilP       P in senesced root and nodule tissue (kg[P]/ha/d)
! SenSurfP       P in senesced canopy tissue (kg[P]/ha/d)
! YEAR           Year of current date of simulation 
! YRDOY          Current day of simulation (YYYYDDD)
! YRPLT          Planting date (YYYYDDD)
!=======================================================================
