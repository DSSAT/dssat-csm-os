!***********************************************************************
!  SOILCBAL   Subroutine for SOM/residue modules of DSSAT.
!
!  Purpose: Calculate the carbon balance of the crop and of SOM/litter.
!
!  REVISION HISTORY
!  03/10/2001 AJG Written.
!  03/09/2002 AJG Removed the code on crop carbon (now a separate routine).
!  11/01/2002 AJG Adapted for seasonal/sequential runs with output for 
!                 both an individual season and a complete sequential run.
!  08/22/2018 CHP Revamped to better represent a complete soil C balance for
!                 daily and seasonal, soil and surface, Century and Ceres-OM.
!                 
!  Called: CENTURY
!  Calls : ERROR, GETLUN, HEADER, YR_DOY
!***********************************************************************

      SUBROUTINE SOILCBAL (CONTROL, ISWITCH, 
     &  ACCCO2, HARVRES, LITC, OMAData, SENESCE,          !Input
     &  SSOMC, TLITC, TSOMC, YRDOY)                       !Input

!     ------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT  NONE
      SAVE
!     ------------------------------------------------------------------
      LOGICAL FEXIST

      CHARACTER*1  IDETC, IDETL, RNMODE
      CHARACTER*11 ERRKEY
      CHARACTER*12 SCBAL
      CHARACTER*15 SCBALSUM
      PARAMETER (ERRKEY = 'SCBAL')

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, INCDAT, LUNSNC, LUNSNC2, Num, 
     &  RUN, SOIL, SRFC, YR, YRDOY, FROP
      INTEGER InitialYr, InitialDOY

      PARAMETER (SRFC = 0, SOIL = 1)

!      REAL CBALANCE, CBALANCE1, CUMRES, END_AND_OUTPUT, END_AND_OUTPUT1,
!     & oldCUMRES, oldSENESSUMC, oriTLITCI_SOILSRFC, oriTSOMCI_SOILSRFC,
!     & SENESSUMC, START_AND_INPUT, START_AND_INPUT1, TLITC,
!     & TLITC_SOILSRFC, TLITCI_SOILSRFC, TSOMC, TSOMC_SOILSRFC,
!     & TSOMCI_SOILSRFC

      REAL ACCCO2(0:1), ACCCO2Y(0:1), ACCCO2_LastSeason(0:1)
      REAL ACCCO2_init(0:1)
      REAL, DIMENSION(0:NL) :: LITC, SSOMC

      REAL TSOMC, TLITC
      REAL RESC0D, RESC0D_init, RESC1D, RESC1D_init
      REAL SNCLD, SNCLD_init, SNCLDY, SNCSD, SNCSD_init, SNCSDY
      REAL SOMC0T_Y, SOMCT_Y, LC0D_Y, LCTD_Y
      REAL HarvCTot

      REAL SOMC0T_init, SOMCT_init
      REAL LC0D_init, LCTD_init
      REAL THRC0D, THRC1D

      REAL CumResC, CumSenC   !, HarvResC

      REAL TotalC, TotalCY, TotalC_init
      REAL TotalAdd, TotalSub
      REAL DayBal, CumBal, TotBal

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType)   CONTROL
      TYPE (SwitchType)    ISWITCH
      TYPE (OrgMatAppType) OMAData    !Organic matter application
      TYPE (ResidueType)   SENESCE
      TYPE (ResidueType)   HARVRES

!     ------------------------------------------------------------------
      !Don't print unless C output requested.
      IDETC   = ISWITCH % IDETC
      IDETL   = ISWITCH % IDETL
!     IF (IDETC == 'N' .OR. INDEX('AD',IDETL) < 1) RETURN
      IF (IDETC == 'N' .OR. INDEX('0',IDETL) > 0) RETURN

!     ------------------------------------------------------------------
!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      DAS     = CONTROL % DAS
      FROP    = CONTROL % FROP
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE

!***********************************************************************
!***********************************************************************
!     SEASONAL INITIALIZATION
!***********************************************************************
      IF (DYNAMIC == SEASINIT) THEN
!***********************************************************************
        SCBAL = 'SoilCBal.OUT'
        CALL GETLUN ('SCBAL', LUNSNC)

        INQUIRE (FILE = SCBAL, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = LUNSNC, FILE = SCBAL, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = LUNSNC, FILE = SCBAL, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE (LUNSNC,'("*Soil Carbon Balance")')
        ENDIF

!     -------------------------------------------------------------------
        SCBALSUM = 'SoilCBalSum.OUT'
        CALL GETLUN ('SCBAL2', LUNSNC2)

        INQUIRE (FILE = SCBALSUM, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = LUNSNC2, FILE = SCBALSUM, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = LUNSNC2, FILE = SCBALSUM, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE (LUNSNC2,'("*Soil Carbon Balance Summary")')
!     -------------------------------------------------------------------
!         Write headers for summary output file.
          CALL HEADER (SEASINIT, LUNSNC2, RUN)

          WRITE (LUNSNC2, '(4A,/,4A,/,4A,/,4A)') 
     &"!                             ",
     &"<----------- Initial State Variables ---------->",
     &"<------------ Final State Variables ----------->",
     &"<------------ Added -----------><-Lost-><-Balance>",

     &"!                             ",
     &"<-------- SOM-C -------><-------- FOM-C ------->",
     &"<-------- SOM-C -------><-------- FOM-C ------->",
     &"<---OM---><Senesced><--HarvRes-><--CO2->",

     &"!                             ",
     &"        Surf        Soil        Surf        Soil",
     &"        Surf        Soil        Surf        Soil",
     &" ",

     &"@Run FILEX               TN CR",
     &"       SC0Di      SOMCTi       LC0Di       LCTDi",
     &"        SC0D       SOMCT        LC0D        LCTD",
     &"      OMAC     SENCC      HRCH     CO2TC    CUMBAL"

        ENDIF

!       If the file can't be found, call an error.
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, 'SCBALSUM', 0)

!     -------------------------------------------------------------------
!       Get initial values of organic matter placement and senescence
        RESC0D_init = OMAData % ResWt(0) * 0.4
        RESC1D_init = SUM(OMAData % ResWt) * 0.4 - RESC0D

        SNCLD_init = 0.0
        SNCSD_init = 0.0

!       Senesced material is added to FOM on the next day, so keep yesterday's 
!       value to use in the balance.
        SNCLDY = SNCLD_init
        SNCSDY = SNCSD_init

!       For single season runs, make sure harvest residue is zero.
        IF (RUN .EQ. 1 .OR. INDEX('QF',RNMODE) .LE. 0) THEN
          HARVRES % RESWT  = 0.0
          HARVRES % RESLig = 0.0
          HARVRES % RESE   = 0.0
!          HARVRES % CumResWt= 0.0
!          HARVRES % CumResE = 0.0
        ENDIF

!       Harvest residue from previous crop has already been added to soil
!       Subtract it out to report initial value.
        HarvCTot = SUM(HARVRES % ResWt) * 0.4
        THRC0D = HARVRES % ResWt(0) * 0.4   !surface
        THRC1D = HarvCTot - THRC0D          !soil

!       Set the initial amount of carbon in SOM+litter at the beginning
!       of the new season, as a reference point for the seasonal balance.
!        TSOMCI_SOILSRFC = TSOMC + SOM1C(SRFC)
!        TLITCI_SOILSRFC = TLITC + LITC(SRFC)
        SOMC0T_init = SSOMC(0)
        SOMCT_init  = TSOMC

        LC0D_init = LITC(0) - THRC0D
        LCTD_init = TLITC - THRC1D

        TotalC_init = SSOMC(0) + TSOMC + LITC(0) + TLITC - HarvCTot

        SOMC0T_Y = SOMC0T_init
        SOMCT_Y  = SOMCT_init
        LC0D_Y   = LC0D_init
        LCTD_Y   = LCTD_init
        TotalCY  = TotalC_init

        CumBal = 0.0

        CALL YR_DOY(INCDAT(YRDOY,-1), YR, DOY)
        InitialYr   = YR
        InitialDOY = DOY
        ACCCO2_LastSeason = ACCCO2
        ACCCO2Y = 0.0
        ACCCO2_init = 0.0
      
!     -------------------------------------------------------------------
!       Write headers for seasonal balance output file.
        CALL HEADER (SEASINIT, LUNSNC, RUN)

!       Daily output is for "D" detailed printout only
        IF (INDEX('D',IDETL) < 1) RETURN

        WRITE (LUNSNC, '(5A,/,5A,/,5A,/,5A)') 
     &"!               ",
     &"<----------- Initial State Variables ---------->",
     &"<------------ Final State Variables ----------->",
     &"<-------------------------- Added ------------------------->",
     &"<------ Lost ------>",


     &"!               ",
     &"<-------- SOM-C -------><-------- FOM-C ------->",
     &"<-------- SOM-C -------><-------- FOM-C ------->",
     &"<------- OM -------><---- Senesced ----><----- HarvRes ---->",
     &"<------- CO2 ------><---- Balances ---->",

     &"!               ",
     &"        Surf        Soil        Surf        Soil",
     &"        Surf        Soil        Surf        Soil",
     &"      Surf      Soil      Surf      Soil      Surf      Soil",
     &"      Surf      Soil     Daily     Cumul",

     &"@YEAR DOY    DAS",
     &"     SOMC0Ti      SOMCTi       LC0Di       LCTDi",
     &"      SOMC0T       SOMCT        LC0D        LCTD",
     &"    RESC0D    RESC1D     SNCLD     SNCSD    THRC0D    THRC1D",
     &"     CO20D     CO2SD    DAYBAL    CUMBAL"

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT 
!***********************************************************************
      ELSEIF (DYNAMIC == OUTPUT) THEN
!***********************************************************************
        RESC0D = OMAData % ResWt(0) * 0.4
        RESC1D = SUM(OMAData % ResWt) * 0.4 - RESC0D

        SNCLD = SENESCE % ResWt(0) * 0.4
        SNCSD = SUM(SENESCE % ResWt) * 0.4 - SNCLD

        TotalC = SSOMC(0) + TSOMC + LITC(0) + TLITC
        TotalAdd = RESC0D + RESC1D + SNCLDY + SNCSDY + THRC0D + THRC1D
        TotalSub = (ACCCO2(0) + ACCCO2(1)) - (ACCCO2Y(0) + ACCCO2Y(1))

        DayBal = TotalC - TotalCY - TotalAdd + TotalSub
        CumBal = CumBal + DayBal

!       Daily output is for "D" detailed printout only
        IF (INDEX('D',IDETL) > 0) THEN

!         ****************************************************************
!         Carbon balance of a single-season run, seasonal run.
!         ****************************************************************
          CALL YR_DOY(YRDOY, YR, DOY)

!         IF (INDEX ('QF', RNMODE) <= 0) THEN
!         Check for output frequency
          IF (MOD(DAS,FROP) == 0) THEN  
            WRITE (LUNSNC,'(1X,I4,1X,I3.3,I7, 8F12.2, 8F10.2, 2F10.4)') 
     &        YR, DOY, DAS, 
     &        SOMC0T_Y, SOMCT_Y,      !SOM-C yesterday
     &        LC0D_Y, LCTD_Y,         !Litter-C yesterday
     &        SSOMC(0), TSOMC,        !SOM-C today
     &        LITC(0), TLITC,         !Litter-C today
     &        RESC0D, RESC1D,         !Organic matter-C applied today
     &        SNCLD, SNCSD,           !Senesced C today
     &        THRC0D, THRC1D,         !Harvest residue-C added today
     &        ACCCO2(0) - ACCCO2Y(0), !CO2 emissions from surface today
     &        ACCCO2(1) - ACCCO2Y(1), !CO2 emissions from soil today   
     &        DAYBAL, CUMBAL          !Balances
          ENDIF
        ENDIF

        SOMC0T_Y = SSOMC(0)
        SOMCT_Y  = TSOMC
        LC0D_Y   = LITC(0)
        LCTD_Y   = TLITC
        TotalCY = TotalC

        ACCCO2Y = ACCCO2
        SNCLDY = SNCLD
        SNCSDY = SNCSD

!       Set harvest residue back to zero after first day
        THRC0D = 0.0      !surface
        THRC1D = 0.0      !soil

!***********************************************************************
!***********************************************************************
!     SEASONAL OUTPUT 
!***********************************************************************
      ELSEIF (DYNAMIC == SEASEND) THEN
!***********************************************************************
!     Get final values of organic matter placement and senescence
      CumResC = OMAData % CumResWt * 0.4

!     Subtract last senescence from the balance. This one is added
!     to harvest residue for next season.
      CumSenC = SENESCE % CumResWt * 0.4      ! - SNCLD - SNCSD
!      HarvResC = HARVRES % CumResWt * 0.4

      TotalAdd = CumResC + CumSenC + HarvCTot
      TotalSub = ACCCO2(0) + ACCCO2(1)
     &         - ACCCO2_LastSeason(0) - ACCCO2_LastSeason(1)

      TotBal = TotalC - TotalC_init - TotalAdd + TotalSub

      WRITE(LUNSNC,'(/,"!",T50,A,T70,A)') " Initial", "   Final"
      WRITE(LUNSNC,'(  "!",T50,A,T70,A)') "Year/DOY", "Year/DOY"
      WRITE(LUNSNC,'(  "!",T50,I4,"-",I3.3,T70,I4,"-",I3.3)') 
     &    InitialYr, InitialDOY, YR, DOY
      WRITE(LUNSNC,'("!",A,T50,A)') " SOIL C BALANCE", 
     &   "--------- kg[C]/ha ---------"
      WRITE(LUNSNC,'("!",A)') "  SOIL & SURFACE ORGANIC C"

      WRITE(LUNSNC,'("!",A,T40,F18.2,T60,F18.2)') 
     &   "   Soil Organic C", SOMCT_init, TSOMC
      WRITE(LUNSNC,'("!",A,T40,F18.2,T60,F18.2)') 
     &   "   Surface Organic C ", SOMC0T_init, SSOMC(0)

      WRITE(LUNSNC,'("!",A,T40,F18.2,T60,F18.2)') 
     &   "   Soil Litter C", LCTD_init, TLITC
      WRITE(LUNSNC,'("!",A,T40,F18.2,T60,F18.2)') 
     &   "   Surface Litter C", LC0D_init, LITC(0)

      WRITE(LUNSNC,'("!",T50,A,T70,A)') "--------","--------"
      WRITE(LUNSNC,'("!",A,T40,F18.2,T60,F18.2)') 
     &   "   Total C in Soil and Surface Layers", TotalC_init, TotalC 

      WRITE(LUNSNC,'("!",A)') "  ADDITIONS AND REMOVALS:"
      WRITE(LUNSNC,'("!",A,T40,F18.2)') 
     &   "   C in Harvest Residues from Previous Crop", HarvCTot
      WRITE(LUNSNC,'("!",A,T40,F18.2)')  
     &   "   C from Organic Applications", CumResC
      WRITE(LUNSNC,'("!",A,T40,F18.2)')  
     &   "   C in returned senesced material", CumSenC
      WRITE(LUNSNC,'("!",A,T60,F18.2)')  
     &   "   CO2-C emitted", ACCCO2(0) + ACCCO2(1)  
     &                  - ACCCO2_init(0) - ACCCO2_init(1)

      WRITE(LUNSNC,'("!",T50,A,T70,A)')  "--------", "--------"
      WRITE(LUNSNC,'("!",A,T40,F18.2,T60,F18.2)')" TOTAL C BALANCE",
     &   TotalC_init + TotalAdd, TotalC + TotalSub
      WRITE(LUNSNC,'("!",A,T60,F18.3)') "   Balance", TotBal

!     -------------------------------------------------------------------
!     Write seasonal balance summary file
        IF (CONTROL % RNMODE == 'Q') THEN
          Num = CONTROL % ROTNUM
        ELSE
          Num = CONTROL % TRTNUM
        ENDIF

      WRITE(LUNSNC2,'(I4,1X,A12,I10,1X,A2,8F12.2, 11F10.2)')
     &  CONTROL%RUN, CONTROL%FILEX, Num, CONTROL%CROP,
     &  SOMC0T_init, SOMCT_init,      !SOM-C initial
     &  LC0D_init, LCTD_init,         !Litter-C initial
     &  SSOMC(0), TSOMC,              !SOM-C final
     &  LITC(0), TLITC,               !Litter-C final
     &  CumResC,                      !Organic matter-C applied cumul
     &  CumSenC,                      !Senesced C cumul
     &  HarvCTot,                     !Harvest residue-C cumul
!       CO2 emissions cumul
     &  ACCCO2(0) - ACCCO2_init(0) + ACCCO2(1) - ACCCO2_init(1),
     &  CUMBAL                        !Balances

!     Set the initial amount of carbon in SOM+litter for the next season.
      SOMC0T_init = SSOMC(0)
      SOMCT_init  = TSOMC

      LC0D_init = LITC(0)
      LCTD_init = TLITC

      TotalC_init = SSOMC(0) + TSOMC + LITC(0) + TLITC
      TotalCY = TotalC_init

      CumBal = 0.0

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF

!***********************************************************************
      RETURN
      END SUBROUTINE SOILCBAL
