!======================================================================
!
!  CSP_PHENOL Subroutine, based on PHENOL_SC, Subroutine, O.H. Daza
!
!  PURPOSE: Simualte phenological development of the sugarcane crop
!----------------------------------------------------------------------
!  REVISION HISTORY
!  03/10/01 OHD wrote it
!  07/27/03 FSR reformatted to FORTRAN 77
!  08/22/03 FSR integrated into DSSAT 4.0 
!  07/26/2004 CHP Removed variables which were not being used
!  07/07/2005 FSR Modified
!----------------------------------------------------------------------
!  Called from:  CASUPRO
!  Calls:        CSP_INPHENOL  in CSP_INPHENOL.for
!                CSP_IPPHENOL  in CSP_IPPHENOL.for
!                CURV          in UTILS.for
!----------------------------------------------------------------------
!
      SUBROUTINE CSP_PHENOL(CONTROL, ISWITCH, FILECC,
     &    DTPI, Kill, LI, M, SOILPROP, TGROAV,               !Input
     &    TURFAC, XLFNUM, XStkNum, YLfFac, YLFSZ, YRDOY,     !Input
     &    YRPLT, YRSIM,                                      !Input
     &    CropTypeCode, DeltaLeafArea, DeltaLeafNum,         !Output
     &    DLFN, DPLARF, DRPP, DTX, TillerCount,              !Output 
     &    LeafNum, MinGr, MDATE,                             !Output
     &    NVEG0, PhenoStage, PHTHRS, Ph1P, PI1, PI2, ROWSPC, !Output
     &    Smax, StalkState, STNAME, STGDOY, StkHrNO,         !Output
     &    SumTTD, SumTTG, SumTTStalk, TDUMX, VSTAGE,         !Output
     &    XLAI, YREMRG)                                      !Output
!----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL CSP_IPPHENOL, CSP_INPHENOL, CURV, GETLUN, HEADER, TABEX, 
     &  TIMDIF, YR_DOY
      SAVE
!----------------------------------------------------------------------

! Variable specification

      CHARACTER*1 ISIMI, ISWWAT, IDETG, PLME
      CHARACTER*2 CROP
      !CHARACTER*3 CTMP(4), DLTYP(4)
      CHARACTER*4 StalkState(NumOfStalks,10)
      CHARACTER*10 STNAME(20)
      CHARACTER(11) :: CropTypeName
      CHARACTER*13 OUTPHNL
      CHARACTER*30 FILEIO
      CHARACTER*92  FILECC

      INTEGER Cond, EndStalk, ERRNUM, FROP, DYNAMIC, DOY, DAS   
      INTEGER DAP, TillerCount, MDATE, NLAYR, NOUTPHN, NVEG0, RUN 
      INTEGER Smax, TIMDIF, YEAR, YREMRG, YRPLT, YRSIM, YRDOY   
      INTEGER livecount, Day, Phase, Stalk    
      INTEGER CropTypeCode
      INTEGER PhenoStage, NewStalk
!      INTEGER Temp, WLUN
      INTEGER, PARAMETER :: NumOfTemp = 5, NumOfLeaves = 40 
      INTEGER, PARAMETER :: NumOfStages = 5, NumOfPhases = 4
      INTEGER, DIMENSION(NumOfStages) :: DayOfStage, 
     &                   DaysAfterPlantOfStage, STGDOY
      INTEGER, DIMENSION(1:NumOfStalks) :: Kill    
      REAL LI, LI1, RTR, XLI(7), YVTR(7)  !, VT   
      REAL XLFNUM(7), YLFSZ(7), XStkNum(9), YLfFac(9)  
      REAL TABEX  ! Function subroutine - Lookup utility
  
      REAL TURFAC, DRPP, DTX  
      REAL SDEPTH 
      REAL PLANTS, PLTPOP, ROWSPC
      REAL TDUMX, ExcsTTD
      REAL CURV  ! Function subroutine
      REAL Tbase, TbaseStalk
      REAL M, MinGr
      REAL Ph1P, Ph1R, Ph2, Ph3, Ph4
      REAL DTPI, PI1, PI2
!      REAL So, Go, Gmax
      REAL DepthRateOfEmer, DeltaDepthOfEmer, DepthToEmer
      REAL TGROAV, DeltaTTD, SumTTD, DeltaTTG 
      REAL RTNFAC, StkHrNO, SumTTG, TELOM 
      REAL XLAI
      REAL VSTAGE, EnviroFactor
      REAL GrowFrac, GrowTime, RipeFrac, RipeTime

      LOGICAL FEXIST  !, FIRST

      REAL :: DPLARF, DLFN, NewTiller, TillerExcess
      
      REAL, DIMENSION(NL) :: LL, DUL, SAT, DLAYR  ! , SW, ST
      REAL, DIMENSION(0:NumOfPhases) :: FNSTR, FPSTR, FSW, FT, FUDAY
!     &                                WSENP, NSENP
      REAL, DIMENSION(1:NumOfStalks) :: DeltaTillerNum
                                
      REAL, DIMENSION(0:NumOfDays,NumOfStalks) :: DeltaLeafNum, LeafNum, 
     &                DeltaLeafArea
      REAL, DIMENSION(0:NumOfDays,NumOfStalks) :: SumTTStalk

      REAL, DIMENSION(NumOfPhases) :: PHTHRS, PHZACC
      REAL, DIMENSION(NumOfPhases) :: OptStageDur, CumOptStageDur
      REAL, DIMENSION(NumOfTemp) :: TB, TO1, TO2, TM

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

!     The variable "SOILPROP" is of type "SoilType".
      TYPE (SoilType) SOILPROP

!     The variable "ISWITCH" is of type "SwitchType".
      TYPE (SwitchType) ISWITCH

!     No output for fallow crop (or if IDETG = N? FSR)
      CROP    = CONTROL % CROP
      IDETG   = ISWITCH % IDETG

!     Transfer values from constructed data types into local variables.
      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      FROP    = CONTROL % FROP
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM

      DLAYR  = SOILPROP % DLAYR
      DUL    = SOILPROP % DUL
      LL     = SOILPROP % LL
      NLAYR  = SOILPROP % NLAYR
      SAT    = SOILPROP % SAT

      ISWWAT = ISWITCH % ISWWAT
!-----------------------------------------------------------------------
! Start execution part
!-----------------------------------------------------------------------

! Days after start of simulation 
!     DAS = MAX(0,TIMDIF(YRSIM,YRDOY))

! Days after planting
      DAP = MAX(0,TIMDIF(YRPLT,YRDOY))

! Bud planting density is read in FILEX as PLANTS; may want to use this
!     method in the future, since it corresponds to production practice

!    BudPlantDensity = ((1 / BudSpacing) + 1) / RowSpacing

! Right 
!    BudPlantDensity = 1 / (BudSpacing * RowSpacing)

!**********************************************************************
!**********************************************************************
!  Run Initialization - Called once per simulation
!**********************************************************************

      IF (DYNAMIC .EQ. RUNINIT) THEN

      DPLARF = 0     !Leaf area for the whole plant 
      EndStalk = 0 
      NewTiller = 0.0
	RTR = 0.0
	LI = 0.0 ! Initialize here since Phenol is called before Photo (FSR)
	TillerCount = 0.0

!----------------------------------------------------------------------
!FSR - Introduced another output file that is readable by G-Build.  
!      Included the following line in UTILS.for:

!     CASE ('OUTPHNL'); LUN = 48  !PHENOLOGY.OUT - FSR 
  
!     IF (IDETG .EQ. 'Y') THEN !Whenever PlantGrow.Out is selected
      IF (INDEX('Y'  ,ISWITCH%IDETG) > 0 .AND. 
     &    INDEX('YDA',ISWITCH%IDETL) > 0) THEN

        OUTPHNL  = 'CSP_PHEN.OUT'
        CALL GETLUN('OUTPHNL',  NOUTPHN)
      ENDIF
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Subroutine CSP_IPPHENOL reads required phenology variables from 
! input files. Subroutine INPHENOL initializes data for beginning 
! of run (Once only initialization).
!----------------------------------------------------------------------
      CALL CSP_IPPHENOL(CONTROL, FILECC,
     &           CROP,  DTPI, ISIMI,                      !Output
     &           LI1, MinGr, Ph1P, Ph1R, Ph2, Ph3,        !Output 
     &           Ph4, PI1, PI2, PLANTS, PLME, PLTPOP,     !Output
     &           RTNFAC, ROWSPC, SDEPTH, Smax,            !Output
     &           StkHrNO, TB, TELOM, TM, TO1,             !Output
     &           TO2, XLFNUM, XLI, XStkNum, YLfFac,       !Output
     &           YLFSZ, YVTR)                             !Output

! PHTHRS calculate this here and make it input in CSP_INPHENOL

! It will become 2 or greater after the first run of simulation
! see MESIC and others
! CropTypeCode    Crop type code (1 : plant crop; >=2 : ratoon crop)
      IF (PLME .EQ. 'R') THEN !PLME Planting Method, is set in FILEX.
        CropTypeCode = 2  ! 1 indicates plant cane, and 2 is the 
                          ! first ratoon option.  Second and higher  
                          ! ratoons will require further work
        ELSE
        CropTypeCode = 1
      END IF  

      IF (CropTypeCode == 1) THEN
        PHTHRS(1) = Ph1P
        CropTypeName = "Plant crop"
!      END IF

      ELSE IF (CropTypeCode >= 2) THEN
          PHTHRS(1) = Ph1R
          CropTypeName = "Ratoon crop"
      END IF
!     PHTHRS from "phase threshold" (FSR) 
      PHTHRS(2) = SDEPTH * 10 / Ph2 ! Determine PhenoStages
      PHTHRS(3) = Ph3               ! Ph1, 2, 3 & 4 parameters in .ECO
      PHTHRS(4) = Ph4

!      CALL GETLUN('WORK.OUT', WLUN)
!      OPEN(UNIT = WLUN, FILE = "WORK.OUT", STATUS = "UNKNOWN",
!     &   ACTION = "WRITE", POSITION = "APPEND")

!      WRITE(WLUN,'(1X,"RESULTS FROM CSP_IPPHENOL.for")')
!      WRITE(WLUN,*)
!      WRITE(WLUN,'(1X,"Input file: ",A)') FILEIO
!      WRITE(WLUN,'(1X,"Crop  : ",A2)') CROP
!      WRITE(WLUN,'(1X,"ISIMI : ",A1)') ISIMI
!      WRITE(WLUN,'(1X,"ISWWAT: ",A1)') ISWWAT
!      WRITE(WLUN,*)
!      WRITE(WLUN,'(1x,"Plant population at seeding:  
!     &             ",F8.1," plants/m�")') PLANTS
!      WRITE(WLUN,'(1x,"Plant population at emergence:
!     &               ",F8.1," plants/m�")') PLTPOP
!      WRITE(WLUN,'(1X,"PLME                         : ",A1)') PLME
!      WRITE(WLUN,'(1x,"Row spacing                  :
!     &               ",F8.1," cm")') ROWSPC
!      WRITE(WLUN,'(1x,"Planting depth               :
!     &             ",F8.1," cm")') SDEPTH
!      WRITE(WLUN,*)
!      WRITE(WLUN,'(1X,"Output from cultivar file .CUL")')
!      WRITE(WLUN,*)
!      WRITE(WLUN,'(1X,"Phyllochron interval 1:",F8.1," �C-day")') PI1
!      WRITE(WLUN,'(1X,"Phyllochron interval 2:",F8.1," �C-day")') PI2
!      WRITE(WLUN,'(1X,"Phyllochron interval  :",F8.1," �C-day")') DTPI
!      WRITE(WLUN,*)
   !!!WRITE(WLUN,'(1X,"Smax:",F8.1," # stalks/stubble")') Smax
!      WRITE(WLUN,'(1X,"Smax:",I8," # stalks/stubble")') Smax
!      WRITE(WLUN,'(1X,"So  :",F8.1," # stalks/stubble")') So    
!      WRITE(WLUN,'(1X,"Gmax:",F8.1," �C-day")') Gmax    
!      WRITE(WLUN,'(1X,"Go  :",F8.1," �C-day")') Go
!      WRITE(WLUN,*)
!      WRITE(WLUN,*)
!      WRITE(WLUN,'(1X,"Phase 1 - plant crop :", F8.1, " �C-day")') Ph1P
!      WRITE(WLUN,'(1X,"Phase 1 - ratoon crop:", F8.1, " �C-day")') Ph1R
!      WRITE(WLUN,'(1X,"Phase 2              :
!     &             ", F8.1, " mm/(�C-day)")') Ph2
!      WRITE(WLUN,'(1X,"Phase 3              :", F8.1, " �C-day")') Ph3
!      WRITE(WLUN,'(1X,"Phase 4              :", F8.1, " �C-day")') Ph4
!      WRITE(WLUN,*)
!      WRITE(WLUN,'(1X,"Phase  PHTHRS  �C-day")')
!      DO Phase = 1, NumOfPhases
!        WRITE(WLUN,'(1X,I5,1X,F7.1)') Phase, PHTHRS(Phase)
!      END DO
!      WRITE(WLUN,*)
!      WRITE(WLUN,'(1X,"   TB  TO1  TO2   TM  �C")')
!      DO Temp = 1,NumOfTemp
!        WRITE(WLUN,'(1X, 4F5.1)') TB(Temp), TO1(Temp), TO2(Temp),
!     &        TM(Temp)
!      END DO
!      WRITE(WLUN,*)
!      WRITE(WLUN,'(1X,"Phase WSENP NSENP")')
!      DO Phase = 1, NumOfPhases
!        WRITE(WLUN,'(1X,I5,2(1X,F5.1))') Phase, WSENP(Phase), 
!     &        NSENP(Phase)
!      END DO


!      WRITE(WLUN,'(1X,"Crop: ", A)') CropTypeName(:6)

!----------------------------------------------------------------------
! Subroutine CSP_INPHENOL calculates required phenology variables 
! from values read in input files. Subroutine CSP_INPHENOL initializes 
! data for beginning of run (Once only initialization).
!----------------------------------------------------------------------
      CALL CSP_INPHENOL( 
     &    CROP, PHTHRS, TB, TO1,                          !Input
     &    CumOptStageDur, OptStageDur, STNAME)            !Output

! TEMPORARY: Statements to test output from CSP_INPHENOL above
!      WRITE(WLUN,*)
!      WRITE(WLUN,'(1X,"RESULTS FROM CSP_INPHENOL.for")')
!      WRITE(WLUN,*)
!      WRITE(WLUN,'(1X,"Phase OptStageDur CumOptStageDur  STNAME")')
!      WRITE(WLUN,'(1X,"         (days)       (days)")')
!      DO Phase = 1, NumOfPhases
!        WRITE(WLUN,'(1X,I5,1X,F11.1,1X,F14.1,2X,A)') Phase,
!     &           OptStageDur(Phase), CumOptStageDur(Phase), 
!     &           STNAME(Phase)
!      END DO

! Base temperatures
      Tbase = TB(1)
      TbaseStalk = TB(2)

! This is an input value from file .CUL (read by CSP_IPPHENOL)
      DepthRateOfEmer = Ph2   ! e.g., 0.8 mm (soil depth) / (�C-day)

! For P module (FSR):
      GrowFrac = 0.0
      RipeFrac = 0.0
      GrowTime = PHTHRS(1) + PHTHRS(2) + PHTHRS(3) 
      RipeTime = PHTHRS(4)

!**********************************************************************
!**********************************************************************
!     Seasonal initialization - run once per season
!**********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------

      IF (INDEX('Y'  ,ISWITCH%IDETG) > 0 .AND. 
     &    INDEX('YDA',ISWITCH%IDETL) > 0) THEN

!FSR - Initialize daily phenology.out file
        INQUIRE (FILE = OUTPHNL, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTPHN, FILE = OUTPHNL, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND') ! Was 'APPEND' FSR
!!!          FIRST = .FALSE.
        ELSE
          OPEN (UNIT = NOUTPHN, FILE = OUTPHNL, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(NOUTPHN,'("*PHENOLOGY ASPECTS OUTPUT FILE")')
!!!          FIRST = .TRUE.
        ENDIF

        !Write headers
! - - - - - New CASUPRO header v v v v - - - - - - -
        CALL HEADER(SEASINIT, NOUTPHN, RUN)  
        WRITE(NOUTPHN,'("@YEAR DOY DAS DAP PHSTG NSKST    STTD",  
     &  "  DTTG   STTG  XLAI")', ADVANCE="NO")

!  Header line is complicated to keep column headings same length
!  by inserting underscore where stalk number < 10.  FSR     
        IF (Smax <= 9) THEN 
          DO Stalk = 1,Smax
            WRITE(NOUTPHN,'(3X"LN_",I1)',ADVANCE="NO") Stalk
          END DO
        ELSE
          DO Stalk = 1,9
            WRITE(NOUTPHN,'(3X"LN_",I1)',ADVANCE="NO") Stalk
          END DO
          
          DO Stalk = 10,Smax
            WRITE(NOUTPHN,'(3X"LN",I2)',ADVANCE="NO") Stalk
          END DO
        ENDIF

        IF (Smax <= 9) THEN 
          DO Stalk = 1,Smax
            WRITE(NOUTPHN,'(2X"DLAR_",I1)',ADVANCE="NO") Stalk
          END DO
        ELSE
          DO Stalk = 1,9
            WRITE(NOUTPHN,'(2X"DLAR_",I1)',ADVANCE="NO") Stalk
          END DO
          
          DO Stalk = 10,Smax
            WRITE(NOUTPHN,'(2X"DLAR",I2)',ADVANCE="NO") Stalk
          END DO
        ENDIF
      ENDIF
!-----------------------------------------------------------------------
! Ph1P    Threshold to sprouting - Plant cane,  �C-day
! Ph1R    Threshold to sprouting - Ratoon cane, �C-day
!-----------------------------------------------------------------------

      IF (CropTypeCode >= 2) THEN
        PHTHRS(1) = Ph1R
        CropTypeName = "Ratoon crop"
!       YRPLT = YRRAT  ! Date of ratooning
      END IF

      PhenoStage = 0  ! Added by CHP and FSR to solve initialization
      VSTAGE = 0.0    ! problems, leading to emergence a day earlier 
      DeltaTTD = 0.0  ! in batch runs.  10-08-2003
      DeltaTTG = 0.0

      ExcsTTD = 0 
      Stalk = 0
      NewStalk = 0

      SumTTD = 0  ! Summation of thermal time for leaves
      SumTTG = 0  ! Summation of thermal time for stalks

      DepthToEmer = 0

      NVEG0 = 10000

! These can be incorporated later on to affect development.  OHD

      DRPP   = 0.0  ! Photoperiod days which occur in a real day
                    ! (photoperiod days / day)

      DTX    = 0.0  ! Thermal time that occurs in a real day based on 
                    ! vegetative development temperature function 
                    ! (thermal days / day)

      TDUMX  = 0.0  ! Photo-thermal time that occurs in a real day 
                    ! based on early reproductive development 
                    ! temperature function

      XLAI = 0.0    ! Leaf Area Index

    ! Initialization of arrays for each phase
      DO Phase = 1, NumOfPhases    
        FNSTR(Phase)  = 1. ! stress-by-phase not yet implemented 
        FPSTR(Phase)  = 1. ! check definitions at end of file - FSR
        FSW(Phase)    = 1.
        FT(Phase)     = 0.
        FUDAY(Phase)  = 0.
        PHZACC(Phase) = 0. ! Of this list, only this is currently used
      END DO

      DO Stalk = 1, Smax
         DeltaTillerNum(Stalk) = 0.  
      END DO  

! Cond 1 - LIVE or DEAD; 2 - PRIM or TILR; 3 - 10 not yet used
      DO Stalk = 1, Smax
        DO Cond = 1, 10
          StalkState(Stalk,Cond) = '    '
        END DO  
      END DO  

      DO Day = 0, NumOfDays
!      DO Day = 1, NumOfDays
        ! Initialization of variables for stalk appearance and leaf area
        DO Stalk = 1, Smax
          LeafNum(Day,Stalk)      = 0.
          SumTTStalk(Day,Stalk)   = 0.!Thermal time for stalk appearance
          DeltaLeafArea(Day,Stalk)= 0.
          DeltaLeafNum(Day,Stalk) = 0.
        END DO  ! Stalk
      END DO ! Day


! TEMPORARY: Statements to test output from PHENOL_SC
!      WRITE(WLUN,*)
!      WRITE(WLUN,'(1X,"RESULTS FROM CSP_PHENOL.for - SEASINIT")')
!      WRITE(WLUN,*)
!      WRITE(WLUN,'(1X,"YRSIM:",I7)') YRSIM
!      WRITE(WLUN,'(1X,"YRPLT:",I7)') YRPLT
!      WRITE(WLUN,*)
!      WRITE(WLUN,'(1X,"CropTypeName: ",A11)') CropTypeName
!      WRITE(WLUN,*)
!      WRITE(WLUN,'(1X,"RESULTS FROM CSP_PHENOL.for - INTEGR")')
!      WRITE(WLUN,*)

!      WRITE(WLUN,'(1X,"  YRDOY DAS DAP PHSTG NSKST    STTD  
!     &DTTG   STTG  XLAI")', ADVANCE="NO")

!      DO Stalk = 1, Smax
!        WRITE(WLUN,'(" LN(",I2,")")',ADVANCE="NO") Stalk
!      END DO
      
!      DO Stalk = 1,Smax
!        WRITE(WLUN,'(3X"LN",I2)',ADVANCE="NO") Stalk
!      END DO

!      DO Stalk = 1, Smax
!        WRITE(WLUN,'(2X"DLAR",I2)',ADVANCE="NO") Stalk
!      END DO

!***********************************************************************
!***********************************************************************
!  Daily Rate calculations
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
! Compute temperature, daylength, and water effects on development.
! Daylength and water effects on development not included yet.
!-----------------------------------------------------------------------

!************************************************************************
!     CODE per Fernando Villegas FVT / CHP 9/16/2004
!************************************************************************
      IF (YRDOY > YRPLT) THEN
! Calculate thermal time in the day for primary stalk & leaf appearance
        IF (TGROAV < TB(1) .OR. TGROAV > TM(1)) THEN
          DeltaTTD = 0.
        ELSEIF (TGROAV >= TB(1) .AND. TGROAV < TO1(1)) THEN
          DeltaTTD = TGROAV - TB(1)
        ELSEIF (TGROAV >= TO1(1) .AND. TGROAV <= TO2(1)) THEN
          DeltaTTD = TO1(1) - TB(1)
        ELSEIF (TGROAV > TO2(1) .AND. TGROAV <= TM(1)) THEN
          DeltaTTD = (TO1(1) - TB(1))*(TM(1) - TGROAV)/(TM(1) - TO2(1))
        ENDIF

! Calculate thermal time in the day for tiller appearance
        IF (TGROAV < TB(2) .OR. TGROAV > TM(2)) THEN
          DeltaTTG = 0.
        ELSEIF (TGROAV >= TB(2) .AND. TGROAV < TO1(2)) THEN
          DeltaTTG = TGROAV - TB(2)
        ELSEIF (TGROAV >= TO1(2) .AND. TGROAV <= TO2(2)) THEN
          DeltaTTG = TO1(2) - TB(2)
        ELSEIF (TGROAV > TO2(2) .AND. TGROAV <= TM(2)) THEN
          DeltaTTG = (TO1(2) - TB(2))*(TM(2) - TGROAV)/(TM(2) - TO2(2))
        ENDIF
      ENDIF

! DTX     Thermal time that occurs in a real day based on vegetative 
!         development temperature function (thermal days / day)
!         It could be substituted in this module.  (FSR)
! TURFAC    Water stress factor for expansion (0 - 1) 
! DTX     could be substituted in 

      DTX = CURV('lin', Tb(1), To1(1), To2(1), Tm(1), TGROAV)

!************************************************************************
! Environmental factor as a function of water stress factor
! This factor is taken from the equation below defined in PHENOL.for and adapted 
! for the sugarcane model.  Will P-stress be added here?  FSR

! VSTAGE = VSTAGE + DTX * TRIFOL * EVMOD * TURFAC * (1.0 - XPOD)
    
      EnviroFactor = TURFAC 
!     EnviroFactor = TURFAC*DTX  !Use DTX = DeltaTTD/(TO1(1)-TB(1))

!     Note: DTX    is replaced with DeltaTTD. 
!           TRIFOL not used in CASUPRO, is the rate of appearance of leaves 
!                  on mainstem; replaced with DeltaLeafNum(DAS, Stalk). 
!           EVMOD  Modifies rate of development; not used in CASUPRO. 
!           XPOD   Growth partitioning to pods. 
!
!      So only TURFAC remains, & use of EnviroFactor needs to be evaluated.       
!************************************************************************
    !FSR added for Plant P - 08/25/2006
        IF (GrowTime > 0) THEN
          GrowFrac =  (PHZACC(1) + PHZACC(2) + PHZACC(3)) / GrowTime
        ELSE
          GrowFrac = 0.0
        ENDIF

        IF (RipeTime > 0) THEN
          RipeFrac =  PHZACC(4) / RipeTime
        ELSE
          RipeFrac = 0.0
        ENDIF
!************************************************************************

      SELECT CASE (PhenoStage) ! Checks PhenoStage value
!=============
!        CASE (1)  ! planting - sprouting phase
!=============

!=============
        CASE (2)   ! sprouting - emergence phase
!=============
        IF (CropTypeCode == 1) THEN
          ! Bud planting depth in cm
          IF (DeltaTTD > 0) THEN  
            DeltaDepthOfEmer = DepthRateOfEmer * DeltaTTD / 10 !mm to cm
          END IF
        END IF  

!=============
        CASE (3)   ! emergence - stalk growth phase
!=============
!******************* Stalk development code (FSR) **********************
      IF (LI < LI1)  THEN !Low competition among tillers   
	 
!!        Use TELOM for tiller emergence thermal time (�C-day):. 
	    RTR = 1/TELOM

	ELSE ! Calculate Relative Tillering Rate following 
              ! Bezuidenhout et al 2003

		RTR = MAX(0.0,TABEX(YVTR, XLI, LI, 6)) 

      END IF  ! (LI < LI1)

	DO Stalk = 1, Smax

		IF (StalkState(Stalk,1) .EQ. 'LIVE') THEN  

              DeltaTillerNum(Stalk) =  DeltaTTG * RTR

          ELSE ! Stalk > NewStalk (not alive) 

			DeltaTillerNum(Stalk) = 0

		END IF ! (StalkState(Stalk,1) .EQ. 'LIVE')

	END DO         

!***********************************************************************
! Section for leaf development
!-----------------------------------------------------------------------
!  Calculates the rate of increase of leaf number on each stalk

        DO Stalk = 1, Smax

         IF (StalkState(Stalk,1) .EQ. 'LIVE') THEN   

            IF (SumTTStalk(DAS - 1, Stalk) <= DTPI) THEN
              DeltaLeafNum(DAS, Stalk) = DeltaTTD / PI1
            ELSE
              DeltaLeafNum(DAS, Stalk) = DeltaTTD / PI2
            END IF
          ELSE
            DeltaLeafNum(DAS, Stalk) = 0
          END IF

          !Correction due to temperature and water deficit
          DeltaLeafNum(DAS, Stalk) = DeltaLeafNum(DAS, Stalk)
     &                             * EnviroFactor 
              
!--- New look-up code replaces former M1,M2 leaf area parameters ------

        M = MAX(TABEX(YLFSZ, XLFNUM, LeafNum(DAS-1, Stalk), 7), 1.E-06)
     &      * MAX(TABEX(YLfFac, XStkNum, Float(Stalk), 9), 1.E-06)

          DeltaLeafArea(DAS,Stalk) = M * DeltaLeafNum(DAS,Stalk)

!         NOTE: MAX function avoids dividing by zero in CSP_Grow_Cane 
!               when LeafNum = 0, M = 0
!-----------------------------------------------------------------------
        END DO

!=============
        CASE (4)   ! stalk growth - flowering phase
!=============

!******************* Stalk development code (FSR) **********************

! Calculate Relative Tillering Rate based on Bezuidenhout et al 2003

		RTR = MAX(0.0,TABEX(YVTR, XLI, LI, 6)) 

		DO Stalk = 1, Smax

			IF (StalkState(Stalk,1) .EQ. 'LIVE') THEN  

                 DeltaTillerNum(Stalk) =  DeltaTTG * RTR

          	ELSE ! Stalk > NewStalk (not alive) 

				  DeltaTillerNum(Stalk) = 0

			END IF ! (StalkState(Stalk,1) .EQ. 'LIVE')

		END DO         

!***********************************************************************
      ! Section for leaf development
        DO Stalk = 1, Smax
!-----------------------------------------------------------------------
!  Calculates the rate of increase of leaf number in each stalk

         IF (StalkState(Stalk,1) .EQ. 'LIVE') THEN   

            IF (SumTTStalk(DAS - 1, Stalk) <= DTPI) THEN
              DeltaLeafNum(DAS, Stalk) = DeltaTTD / PI1
            ELSE
              DeltaLeafNum(DAS, Stalk) = DeltaTTD / PI2
            END IF
          ELSE
            DeltaLeafNum(DAS, Stalk) = 0
          END IF

          !Correction due to temperature and water deficit
          DeltaLeafNum(DAS, Stalk) = DeltaLeafNum(DAS, Stalk)
     &                             * EnviroFactor 

!--- New look-up code replaces former M1,M2 leaf area parameters ------

        M = TABEX(YLFSZ, XLFNUM, LeafNum(DAS-1, Stalk), 7)
     &      * TABEX(YLfFac, XStkNum, float(Stalk), 9)

          DeltaLeafArea(DAS,Stalk) = M * DeltaLeafNum(DAS,Stalk)

!         NOTE: MAX function avoids dividing by zero in CSP_Grow_Cane 
!               when LeafNum = 0, M = 0
!-----------------------------------------------------------------------
        END DO
      END SELECT

!----------------------------------------------------------------
! Increment of leaf number in main stalk
      DLFN = DeltaLeafNum(DAS,1)

!**********************************************************************
!**********************************************************************
!  Daily Integration 
!**********************************************************************

      ELSE IF (DYNAMIC .EQ. INTEGR) THEN

!**********************************************************************

      DTX = CURV('lin', Tb(1), To1(1), To2(1), Tm(1), TGROAV)
      ! DTX calculation repeated here since RATE section does not exist
      ! in CSP_ROOTS, and value of DTX was not being passed.

      IF (YRDOY .EQ. YRPLT) THEN  ! It will be the same for ratooning 
        PhenoStage = 1          ! Planting or ratooning is set
        STGDOY(1) = YRDOY
        DayOfStage(1) = YRDOY   ! Sets the day  
        DaysAfterPlantOfStage(1) = DAP  ! Sets de number of days
!        DeltaTTD = 0 ! Thermal time is not accounted the first day of simulation
      END IF


! Integrates thermal time for days greater than day of planting
! For ratooning YRPLT = YRRAT -  Make this later!
      IF (YRDOY > YRPLT) THEN  
        !Integrates thermal time only for DeltaTTD > 0
        IF (DeltaTTD > 0) SumTTD = SumTTD + DeltaTTD

        IF ((PhenoStage == 3) .OR. (PhenoStage == 4)) THEN 
          !Integrates thermal time only for DeltaTTG > 0
          IF (DeltaTTG > 0) SumTTG = SumTTG + DeltaTTG
        END IF 
      END IF


      SELECT CASE (PhenoStage) ! Checks PhenoStage value
!=============
        CASE (1)
!=============
        IF (CropTypeCode == 1) THEN
          PHZACC(1) = PHZACC(1) + DeltaTTD
          IF (PHZACC(1) > PHTHRS(1)) THEN
            PhenoStage = 2
            STGDOY(2) = YRDOY
            DayOfStage(2) = YRDOY
            DaysAfterPlantOfStage(2) = DAP

            ExcsTTD = PHZACC(1) - PHTHRS(1) ! Excess thermal time 
            PHZACC(2) = ExcsTTD  ! Initializes Phase 2
            ExcsTTD = 0  ! Voids excess thermal time 
          END IF

        ELSE IF (CropTypeCode >= 2) THEN
          PHZACC(1) = PHZACC(1) + DeltaTTD
          IF (PHZACC(1) > PHTHRS(1)) THEN
            ! Stage 2 is skipped
            PhenoStage = 3  ! Stage 3 is set right away
!-----------------------------------------------------------------------
!           Emergence, next stage, occurs on day DAS
!-----------------------------------------------------------------------
            NVEG0 = DAS
            YREMRG = YRDOY
            STGDOY(2) = YRDOY
            STGDOY(3) = YRDOY
            DayOfStage(2) = YRDOY  ! Phases 2 and 3 occur the same day
            DayOfStage(3) = YRDOY  ! Phases 2 and 3 occur the same day
!-----------------------------------------------------------------------
            DaysAfterPlantOfStage(2) = DAP  ! Idem
            DaysAfterPlantOfStage(3) = DAP  ! Idem

            ExcsTTD = PHZACC(1) - PHTHRS(1) ! Excess thermal time 
! Initializes thermal time accumulation for Phase 3
            PHZACC(3) = ExcsTTD  
            ExcsTTD = 0  ! Voids excess thermal time 

            PHZACC(2) = 0  ! Second phase does not take place


! Primary stalk number for ratoon depends on mature stalks harvested
! during the previous season.

            ! Starts development of stalks; NewStalk becomes 1 now

            NewStalk = StkHrNO * RTNFAC

            DO Stalk = 1, NewStalk  

                StalkState(Stalk,1) = 'LIVE'
                StalkState(Stalk,2) = 'PRIM'

            END DO 

            NewStalk = StkHrNO * RTNFAC !(temp for debugging)

          END IF
        END IF

!=============
        CASE (2) 
!=============
        IF (CropTypeCode == 1) THEN
          PHZACC(2) = PHZACC(2) + DeltaTTD
          DepthToEmer = DepthToEmer + DeltaDepthOfEmer

          IF (DepthToEmer > SDEPTH) THEN
            PhenoStage = 3
!-----------------------------------------------------------------------
!           Emergence, next stage, occurs on day DAS
!-----------------------------------------------------------------------
            NVEG0 = DAS
            YREMRG = YRDOY
            STGDOY(3) = YRDOY
            DayOfStage(3) = YRDOY
!-----------------------------------------------------------------------
            DaysAfterPlantOfStage(3) = DAP

            ExcsTTD = PHZACC(2) - PHTHRS(2) ! Excess thermal time 
! Initializes thermal time accumulation for Phase 3
            PHZACC(3) = ExcsTTD  
            ExcsTTD = 0  ! Voids excess thermal time 

! Starts development of stalks; NewStalk must become 1 now
            NewStalk = NewStalk + 1
            StalkState(NewStalk,1) = 'LIVE'
            StalkState(NewStalk,2) = 'PRIM' 
                  !Only 1st stalk is counted here 
          END IF

!!        ELSE IF (CropTypeCode >= 2) THEN  ! Ratoon crop
!!          PHZACC(2) = 0  ! Second phase does not take place
!!          PhenoStage = 3
!-----------------------------------------------------------------------
!         Emergence, next stage, occurs on day DAS
!-----------------------------------------------------------------------
!!          NVEG0 = DAS
!!          YREMRG = YRDOY
!!          STGDOY(3) = YRDOY
!!          DayOfStage(3) = YRDOY
!-----------------------------------------------------------------------

! Primary stalk number for ratoon depends on mature stalks harvested
! during the previous season.

!!          DaysAfterPlantOfStage(3) = DAP
!************************************************************************
! Check this section: is it ever used?
          ! Starts development of stalks; NewStalk must become 1 now
!!          NewStalk = NewStalk + 1
!!        StalkState(NewStalk,1) = 'LIVE'
        END IF  
!************************************************************************

!=============
        CASE (3)
!=============
        PHZACC(3) = PHZACC(3) + DeltaTTD

!************* Stalk-based stalk development code 3(FSR) ****************
      DO Stalk = 1, Smax
          IF (StalkState(Stalk,1) .EQ. 'LIVE') THEN   

                  NewTiller = NewTiller 
     &                        + DeltaTillerNum(Stalk)
     &                        + TillerExcess
                  TillerExcess = 0.

          ELSE

          END IF  ! (StalkState(Stalk,1) .EQ. 'LIVE')
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          IF (INT(NewTiller) >= 1.0)  THEN 

            NewStalk = NewStalk + INT(NewTiller)
            StalkState(NewStalk,1) = 'LIVE'
            StalkState(NewStalk,2) = 'TILR'
            TillerExcess = NewTiller - INT(NewTiller) 
		  TillerCount = Tillercount + 1

            NewTiller = 0                             

          END IF  ! ((INT(NewTiller) >= 1.0)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     Temporary limit 
         IF (NewStalk > Smax) THEN
           StalkState(NewStalk,1) = '    '
           NewStalk = Smax
         END IF  
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!     Remove stalks not receiving sufficient CH2O for maint respiration

        IF (Kill(Stalk) .EQ. 1) THEN

            StalkState(Stalk,1) = 'DEAD'

            Kill(Stalk) = -1 

          EXIT ! Exits DO Loop to avoid senescing more than one stalk 
         ELSE  ! per day
                
        END IF

      END DO
!************* Stalk-based stalk development code 3(FSR) ****************

! Section for leaf development
! Integrates thermal time for each day and each stalk that has been set
        DO Stalk = 1, Smax
          IF (StalkState(Stalk,1) .EQ. 'LIVE') THEN

            SumTTStalk(DAS, Stalk) = 
     &            SumTTStalk(DAS - 1, Stalk) + DeltaTTD
			IF (LeafNum(DAS-1, Stalk) .EQ. 0) THEN !to start stalk with
				LeafNum(DAS-1, Stalk) = 1          ! a leaf
			END IF
          ELSE
            SumTTStalk(DAS, Stalk) = 0
          END IF

! Integrates number of leaves in each day and each stalk
          LeafNum(DAS, Stalk) = 
     &         LeafNum(DAS - 1, Stalk) + DeltaLeafNum(DAS, Stalk)
        END DO

        IF (PHZACC(3) > PHTHRS(3)) THEN
          PhenoStage = 4
          STGDOY(4) = YRDOY
          DayOfStage(4) = YRDOY
          DaysAfterPlantOfStage(4) = DAP

          ExcsTTD = PHZACC(3) - PHTHRS(3) ! Excess thermal time 
! Initializes thermal time accumulation for Phase 3
          PHZACC(4) = ExcsTTD  
          ExcsTTD = 0  ! Voids excess thermal time 
        END IF

!=============
        CASE (4)
!=============
        PHZACC(4) = PHZACC(4) + DeltaTTD

!************* Stalk-based stalk development code 4(FSR) ****************
      DO Stalk = 1, Smax
          IF (StalkState(Stalk,1) .EQ. 'LIVE') THEN   

                  NewTiller = NewTiller 
     &                        + DeltaTillerNum(Stalk)
     &                        + TillerExcess
                  TillerExcess = 0.

          ELSE

          END IF  ! (StalkState(Stalk,1) .EQ. 'LIVE')
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
          IF (INT(NewTiller) >= 1.0)  THEN 

            NewStalk = NewStalk + INT(NewTiller)
            StalkState(NewStalk,1) = 'LIVE'
            StalkState(NewStalk,2) = 'TILR'
            TillerExcess = NewTiller - INT(NewTiller) 
		  TillerCount = Tillercount + 1

            NewTiller = 0                             

          END IF  ! ((INT(NewTiller) >= 1.0)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     Temporary limit 
         IF (NewStalk > Smax) THEN
           StalkState(NewStalk,1) = '    '
           NewStalk = Smax
         END IF  
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!     Remove stalks not receiving sufficient CH2O for maint respiration   

        IF (Kill(Stalk) .EQ. 1) THEN

            StalkState(Stalk,1) = 'DEAD'

            Kill(Stalk) = -1 

          EXIT ! Exits DO Loop to avoid senescing more than one stalk 
         ELSE  ! per day

        END IF

      END DO

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!     Check for live stalks. If none are found, season (run) ends. 

      
	livecount = 0

	DO Stalk = 1,Smax
	 IF (StalkState(Stalk,1) .EQ. 'LIVE') THEN

		livecount = livecount +1 
	  
	 END IF !(StalkState(Stalk,1) .EQ. 'LIVE') etc
      END DO  ! Stalk

	 IF (livecount .LT. 1) THEN
!!!		MDATE = YRDOY   ! punch line goes here
		livecount = livecount    ! temp for debugging
	 END IF !

!************* Stalk-based stalk development code 4(FSR) ****************

!-----------------------------------------------------------------------
! Section for leaf development
! Integrates thermal time for each day and each stalk that has been set

        DO Stalk = 1, Smax
         IF (StalkState(Stalk,1) .EQ. 'LIVE') THEN

            SumTTStalk(DAS, Stalk) = 
     &            SumTTStalk(DAS - 1, Stalk) + DeltaTTD
			IF (LeafNum(DAS-1, Stalk) .EQ. 0) THEN !to start stalk with
				LeafNum(DAS-1, Stalk) = 1          ! a leaf
			END IF
          ELSE
            SumTTStalk(DAS, Stalk) = 0
          END IF

! Integrates number of leaves in each day and each stalk
          LeafNum(DAS, Stalk) = 
     &         LeafNum(DAS - 1, Stalk) + DeltaLeafNum(DAS, Stalk) 
        END DO

        IF (PHZACC(4) > PHTHRS(4)) THEN
          PhenoStage = 5
          STGDOY(5) = YRDOY
          DayOfStage(5) = YRDOY
          DaysAfterPlantOfStage(5) = DAP

          ExcsTTD = PHZACC(4) - PHTHRS(4) ! Excess thermal time 
! Initializes thermal time accumulation for Phase 3
          PHZACC(4) = ExcsTTD  
          ExcsTTD = 0  ! Voids excess thermal time 
        END IF

!=============
!        CASE (5)   !  flowering - senescence 
!=============

      END SELECT

      IF (PhenoStage >= 3) THEN
        VSTAGE = LeafNum(DAS, 1)  ! Leaf number of stalk 1 (main stalk)
      END IF 
! The above is a legacy conversion, since VSTAGE is not particularly 
!  relevant to sugarcane.  This makes sure a useful number is available.
  
!-----------------CSP_PHENOL.OUT-----------------------------------------

!      WRITE(WLUN,'(/1X,I7,1X,I3,1X,I3,4X,I2,3X,I3,1X,F7.1,1X,
!     &    F5.2,1X,F6.1,1X,F5.1)', ADVANCE="NO"),
!     &    YRDOY, DAS, DAP, PhenoStage, NewStalk,   
!     &    SumTTD, DeltaTTG, SumTTG, XLAI

!      DO Stalk = 1, Smax
!        WRITE(WLUN,'(1X,F6.2)', ADVANCE="NO") LeafNum(DAS,Stalk)
!      END DO
 

!      DO Stalk = 1, Smax
!        WRITE(WLUN,'(1X,F7.2)', ADVANCE="NO") DeltaLeafArea(DAS,Stalk)
!      END DO
!-----------------CSP_PHENOL.OUT-----------------------------------------

      IF ((MOD(DAS,FROP) .EQ. 0)          !Daily output every FROP days,
     &  .OR. (YRDOY .EQ. YRPLT)           !on planting date, and
     &  .OR. (YRDOY .EQ. MDATE)) THEN     !at harvest maturity 

!       Print 
        DAP = MAX(0,TIMDIF(YRPLT,YRDOY))
        CALL YR_DOY(YRDOY, YEAR, DOY) 

!        IF (IDETG .EQ. 'Y') THEN !Whenever PlantGrow.Out is selected

!-----------------Phenology.Out-----------------------------------------
      IF (INDEX('Y'  ,ISWITCH%IDETG) > 0 .AND. 
     &    INDEX('YDA',ISWITCH%IDETL) > 0) THEN

        WRITE(NOUTPHN,'(/1X,I4,1X, I3, 1X,I3,1X,I3,4X,I2,3X,I3,1X,F7.1,
     &      1X,F5.2,1X,F6.1,1X,F5.1)', ADVANCE="NO"),
     &      YEAR, DOY, DAS, DAP, PhenoStage, NewStalk,   
     &      SumTTD, DeltaTTG, SumTTG, XLAI

        DO Stalk = 1, Smax
         WRITE(NOUTPHN,'(1X,F6.2)', ADVANCE="NO") LeafNum(DAS,Stalk)
        END DO
 

        DO Stalk = 1, Smax
       WRITE(NOUTPHN,'(1X,F7.2)', ADVANCE="NO") DeltaLeafArea(DAS,Stalk)
        END DO
!-----------------Phenology.Out-----------------------------------------
  
        ENDIF
      ENDIF
!-----------------------------------------------------------------------

!**********************************************************************
      END IF
!**********************************************************************
      RETURN
       
!       CLOSE (WLUN)
       CLOSE (NOUTPHN)

      END  !SUBROUTINE PHENOL_SC
! Equivalences of variable names of this module with the names used in
! other modules
! INPUT
!======
! DAYL
! DUL
! FILEIO  InFileName        Filename for input file (e.g., IBSNAT35.INP)
! LL
! NSTRES  
! SAT
! ST
! SW
! TGROAV                    Average daily air temperature (°C)

! Tmin
! TURFAC  Water stress factor for expansion (0 - 1) 
! YRDOY   DayOfSim          Current day of simulation (YYDDD)
! YRPLT   DayOfPlant        Planting date (YYDDD)
! YRSIM   StartOfSimDate    Start of simulation date (YYDDD)

! PLANTS  BudPlantDensity   Plant population at seeding = Bud planting density
! PLTPOP  BudDensityAtEmer  Plant population at emergence = Bud density at emergence

! OUTPUT
!=======
! DRPP    Photoperiod days which occur in a real day
! DTX     Thermal time that occurs in a real day based on vegetative development
!          temperature function (thermal days / day)
! PHTHRS(I) Threshold time that must accumulate in phase I for the next stage to occur
! STNAME  OutHeading        Output headings for specified crops
! STGDOY   
! TDUMX                     Photo-thermal time that occurs in a real day based on early 
!                             reproductive development temperature function
!                             (photo-thermal days / day)
! VSTAGE                    Number of nodes on main stem of plant 
! YREMRG                    Day of emergence (YRDDD)
! YRNR8

! CONTROL
!========
! DYNAMIC

! DaysAfterPlant     DAP     Days after planting
! DayOfPlant         YRPLT   Planting date (YYDDD)
! DaysAfterStartSim  DAS     Days after start of simulation
! NumOfPhases        NPHS    Number of phenological stages
! NumOfLayers        NL      Number of soil layers
! RowSpacing         ROWSPC  Row spacing
!----------------------------------------------------------------------
!  PHENOLOGY VARIABLES LIST
!----------------------------------------------------------------------
! SYMBOL   VARIABLE NAME         DESCRIPTION   
!                               UNITS
! Cond     Used with StalkState array to determine state of interest:
!          1 - LIVE or DEAD; 2 - PRIM or TILR; 3 - 10 not yet used
! DayOfStage         day of occurrence of stage I
! DAi,j    DeltaLeafArea(i,j) increment of leaf area in period i and 
!                             stalk j                                   cm2
! DDi      DeltaTTD(i)        thermal time increment for leaf 
!                             appearance in period i                    °C-day
! DGi      DeltaTTG(i)        thermal time increment for stalk 
!                             appearance in period i                    °C-day
!          [DTTG in CSP_PHENOL.OUT]
! Dli,j    DeltaLeafNum(i,j)  leaf number increase at period i in 
!                             stalk j                                   leaves (°C-day)-1
! Bd       BudPlantDensity    bud planting density                      # buds m-2
! Bs       BudDistance        bud spacing along the row of plants       m
! Di       SumTTD(i)          accumulated thermal time up to period i      °C-day
! di,j     SumTTStalk(i,j)    accumulated thermal time in period i for 
!                             stalk j                                   °C-day
! DLAR      DeltaLeafArea(DAS,Stalk) 
! DTPI      Thermal time threshold (Tb = 9 °C) corresponding to a 
!             given leaf number at which phyllochron interval changes 
!             from Ph1 to Ph2, °C-day
! FNSTR(I) Nitrogen stress function (0 to 1) for phase I
! FPSTR(I) Phosphorus stress function (0 to 1) for phase I 
! FSW(I)   Water stress function (0.0 to 1.0) for phase I 
! FT(I)    Temperature function (0-1) for phase I 
! FTR      First Tiller Rate of growth (tiller/deg-days).
! FUDAY(I) Effect of daylength on development progress (0-1) for phase I 
! FROP     Frequency of output (d)
! Gi       SumTTGStalk(i)     accumulated thermal time for stalk 
!                             appearance up to period i                 °C-day
! Gmax     Gmax               threshold thermal time at which the 
!                             maximum stalk number is reached           °C-day
! Go       Go                 threshold thermal time at which the 
!                             stable stalk number is set                °C-day
! i        i                  index denoting period      
! j        j                  index denoting stalk number      
! K        KmxStk             maximum number of stalks a stubble can 
!                             yield                                     # stalks stubble-1

!             INTEGR, OUTPUT, or FINAL 
! MESIC     Code for sequenced runs: 
!            'S'=second or subsequent run in sequenced simulation, 
!            'M'=single run or first run of sequenced simulation. 
! NewStalk Number of stalks produced per plant (including senesced) 
! NVEG0    Day of emergence (days)
!                             number                                    cm2 leaf-1
! P1       Ph1P               threshold to sprouting (Tb = 9 °C) 
!                             - Plant cane                              °C-day
! P1       Ph1R               threshold to sprouting (Tb = 9 °C) 
!                             - Ratoon cane                             °C-day
! P2       Ph2                threshold to emergence - Plant and 
!                             ratoon cane                               mm (°C-day)-1 
! PI1, PI2 inverse slopes of the linear relationship for phyllochron
!          interval 1 and 2           # leaves (°C-day)-1
! PHZACC   cumulative thermal time for phase I
! RTNFAC   Number of primary shoots to develop from each mature stalk 
!           at previous harvest.
! rs       RowsSpacing        row distance between rows of plants       m
! Smax     Smax               maximum number of stalks a variety can 
!                             yield                                     # stalks stubble-1
!          NumOfStalks        maximum size of the array for stalks
! So       So                 stable stalk number in a plant            # stalks stubble-1 
! StkHrNO  Number of stalks last harvested from existing stubble.
! StalkState(j,k)  Condition k of stalk j.  Currently, conditions are:
!                  k = 1 (LIVE or DEAD); 2 (PRIM or TILR); 
!                  3 (LI zone: TOP, MID or GND);  4-10 unused  
! TELOM    Telomechron interval, or thermal period required for an existing tiller
!                             to produce one higher order tiller.
! Tb       Tbase              base temperature                          °C 
! Ti       Tmean, TGROAV        mean daily temperature in period i        °C
! Tm       Tm                 maximum critical temperature              °C
! Tmax i   Tmax(i)            maximum daily temperature                 °C
! Tmin i   Tmin(i)            minimum daily temperature                 °C
! To1      To1                lower optimum temperature                 °C
! To2      To2                upper optimum temperature                 °C
! Tbs      TbaseStalk         base temperature for stalk appearance     °C
! VT       Variable Telomechron (tiller emergence)   °C-day
!-----------------------------------------------------------------------

