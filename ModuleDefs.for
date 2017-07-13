!=======================================================================
C  MODULE ModuleDefs
C  11/01/2001 CHP Written
C  06/15/2002 CHP Added flood-related data constructs 
C  03/12/2003 CHP Added residue data construct
C  05/08/2003 CHP Added version information
C  09/03/2004 CHP Modified GetPut_Control routine to store entire
C                   CONTROL variable. 
C             CHP Added GETPUT_ISWITCH routine to store ISWITCH.
C             CHP Added TRTNUM to CONTROL variable.
!  06/14/2005 CHP Added FILEX to CONTROL variable.
!  10/24/2005 CHP Put weather variables in constructed variable. 
!             CHP Added PlantStres environmental stress variable
!  11/07/2005 CHP Added KG2PPM conversion variable to SoilType
!  03/03/2006 CHP Tillage variables added to SOILPROP
!                 Added N_ELEMS to CONTROL variable.
!  03/06/2006 CHP Added mulch variable
!  07/13/2006 CHP Add P variables to SwitchType and SoilType TYPES
!  07/14/2006 CHP Added Fertilizer type, Organic Matter Application type
!  07/24/2006 CHP Added mulch/soil albedo (MSALB) and canopy/mulch/soil
!                   albedo (CMSALB) to SOILPROP variable
!  01/12/2007 CHP Changed TRTNO to TRTNUM to avoid conflict with same
!                 variable name (but different meaning) in input module.
!  01/24/2007 CHP Changed GET & PUT routines to more extensible format.
!  01/22/2008 CHP Moved data exchange (GET & PUT routines) to ModuleData
!  04/28/2008 CHP Added option to read CO2 from file 
!  08/08/2008 CHP Use OPSYS to define variables dependant on operating system
!  08/08/2008 CHP Compiler directives for system call library
!  08/21/2008 CHP Add ROTNUM to CONTROL variable
!  11/25/2008 CHP Mauna Loa CO2 is default
!  12/09/2008 CHP Remove METMP
!  11/19/2010 CHP Added "branch" to version to keep track of non-release branches
!=======================================================================

      MODULE ModuleDefs
!     Contains defintion of derived data types and constants which are 
!     used throughout the model.
      SAVE

!=======================================================================
!     Change this line to switch between Windows and Linux compilers
!     Operating system
!=======================================================================
! By Willingthon Pavan (2017-04-24):
! Intel ifort understands the C-style preprocessor directives, so it might
! be easiest to convert the files to that style. Then we would have a
! single code base that would work with both compilers (Intel & gfortran).
! * Using the fpp Preprocessor(INTEL): https://software.intel.com/en-us/node/694581
! * Microsoft Visual Studio IDE: set the Preprocess Source File option to Yes in 
!   the Fortran Preprocessor Option Category.
!=======================================================================

!#if defined WIN32 || defined _WIN32 || defined WIN64 || defined _WIN64
      CHARACTER(LEN=5), PARAMETER :: OPSYS = 'WINDO'   !DOS, Windows
!#else
!      CHARACTER(LEN=5), PARAMETER :: OPSYS = 'LINUX'   !Linux, UNIX
!#endif

!=======================================================================
!     Compiler directives used to set library for system calls
!     Compiler pre-processor definitions can be set in:
!     Project Settings - FORTRAN - General - Preprocessor definitions

!     OR -- set them here
!cDEC$ DEFINE COMPILER=0   !Compaq Visual Fortran compiler
!cDEC$ DEFINE COMPILER=1   !Intel compilers

!     OR -- search code for IFPORT and replace with library required 
!       for your compiler
!=======================================================================

!     Global CSM Version Number
      TYPE VersionType
        INTEGER :: Major = 4
        INTEGER :: Minor = 6
        INTEGER :: Model = 5
        INTEGER :: Build = 4
      END TYPE VersionType
      TYPE (VersionType) Version
      CHARACTER(len=10) :: VBranch = '-Engy_Bal '
!     CHARACTER(len=10) :: VBranch = '-develop  '
!     CHARACTER(len=10) :: VBranch = '-release  '

!     Version history:  
!       4.6.5.04 chp 07/12/2017 ET-based irrigation, minor fixes 
!       4.6.5.03 chp 07/08/2017 Cross-platform compatibility, 
!                    potato temperature sensitivity (R.Raymundo)
!       4.6.5.02 chp 07/06/2017 Y2K-2025, EXNAME in Summary.OUT, data updates
!       4.6.5.01 chp 05/10/2017 Workshop 2017 version. Remove SALUS. 
!       4.6.1.14 chp 05/09/2017 CSV output updates, minor sunflower changes, 
!                               remove auto forage variables
!       4.6.1.13 chp 05/05/2017 Forage model, cross-platform compatibility
!       4.6.1.12 chp 04/17/2017 Growth stage, supply-limited irrigation added
!       4.6.1.11 chp 04/07/2017 CSV format output, fix stage 2 rice longevity issue
!                               NWheat max N uptake from CUL file.
!       4.6.1.10 chp 12/15/2016 Bugfixes tillage, rice, SALUS
!       4.6.1.09 chp 11/18/2016 NWheat grain N bug fixed.
!                               Rice photosynth now uses SLPF.
!       4.6.1.08 chp 10/18/2016 CSCER compatibility with sequence, NWheat species file update
!                               Rice N uptake initialization, taro N uptake calculation 
!       4.6.1.07 chp 07/29/2016 Millet changes from KJB, Puddled field deactivates after 30 days dry
!       4.6.1.06 chp 07/21/2016 DSSAT soil temperature is default method, per GH.
!       4.6.1.05 chp 07/21/2016 EPIC soil temperature is default method.
!                               4-character Weather file in data directory recognized.
!                               SNOW variable correctly passed.
!                               Fatal error messaging improved.
!       4.6.1.04 chp 03/03/2016 Removed some screen messages in GROW and CSP_GROW. Remove make file. 
!       4.6.1.03 chp 08/28/2015 NWheat added 
!       4.6.1.02 chp 09/10/2015 Major changes to millet model (KJB) 
!       4.6.1.01 chp 08/28/2015 NWheat added 
!       4.6.1.00 GH  07/01/2015 DSSAT Version 4.6.1 Release
!       4.6.0.49 GH  06/19/2015 CERES-Rice drought stress issue
!       4.6.0.48 GH  06/18/2015 Harvest fix CERES & minor data file updates
!       4.6.0.47 GH  06/18/2015 Added MaxPest to ModuleDefs
!       4.6.0.46 chp 06/17/2015 Summary.OUT - fixed problems with environmental summary outputs
!       4.6.0.45 vsh 06/16/2015 added Tony's files; Added Taro files
!       4.6.0.44 chp 01/12/2015 CSCER Evaluate.OUT remove headers
!                               OPSUM improved handling of "-99" values
!                               Rice - Ceres & ORYZA - output N uptake
!       4.6.0.43 chp 01/12/2015 Portability - file exension consistency
!       4.6.0.42 chp 12/05/2014 DSSAT sprint -1st batch of changes
!                               Dates and headers, JDATE.CDE fix, portability fixes, 
!                               "F" forced auto-planting date option, minor changes
!       4.6.0.41 chp 11/04/2014 KThorp changes for portability
!       4.6.0.40 chp 09/19/2014 Minor changes
!       4.6.0.39 chp 07/25/2014 Allow daily input of CO2 in weather file (header CO2 or DCO2)
!                               Move PI and RAD to global constants
!       4.6.0.38 chp 07/17/2014 Fixed output switches
!       4.6.0.37 chp 06/05/2014 ORYZA code updated to ORYZA3 (Tao Li)
!       4.6.0.36 chp 05/07/2014 SALUS model updates, ESCP, EPCP added to Summary.OUT
!       4.6.0.35 chp 03/28/2014 Taro initialization fixed - RO
!       4.6.0.34 chp 03/28/2014 Minor changes Weather, CSCER, sorghum
!       4.6.0.33 chp 03/26/2014 Y2K crossover at 2020
!       4.6.0.32 chp 03/20/2014 Minor bug fixes, millet and auto-irrig
!       4.6.0.31 chp 03/10/2014 Sorghum P linkage
!       4.6.0.30 chp 02/21/2014 CSCER, CSCRP, CSCAS updates
!       4.6.0.29 chp 01/23/2014 Fixed bug in auto planting when IHARI = "R"
!       4.6.0.28 chp 01/16/2014 Suppress screen output for VBOSE=zero.
!       4.6.0.27 chp 01/12/2014 Added abiotic stresses to overview.out
!       4.6.0.26 chp 12/22/2013 Fixed issue with tillage routine for crop rotations
!       4.6.0.25 chp 12/03/2013 
!       4.6.0.24 chp 08/30/2013 Add in Tony's code from email 6/11/2013
!       4.6.0.23 chp 08/26/2013 EPIC Soil Temp added as option (METMP = "E")
!       4.6.0.22 chp 08/18/2013 Bugfix - nitrification units conversion
!       4.6.0.21 chp 05/25/2013 Fixed problem with crop-model compatibility check.
!       4.6.0.20 gh  04/27/2013 Cassava module added.
!       4.6.0.19 chp 04/19/2013 Salus generic crop model added.
!       4.6.0.18 chp 10/25/2012 Sugarcane CO2 response to photosynthesis.
!       4.6.0.17 chp 07/01/2012 Minor changes to match v4.5.2.1 release version.
!       4.6.0.16 chp 04/06/2012 Rollback CSCER and CSCRP. ORYZA minor changes.
!       4.6.0.15 chp 04/05/2012 Format changes for rice cultivar input
!                    potato & rice models OK, 
!                    need to rollback CSCER & CSCRP next build
!       4.6.0.14 chp 03/14/2012  
!                US  rice temperature responses, PHINT moved from SPE to CUL
!                GH  ecotype file for potato, RUE1 & RUE2
!                LAH revise CSCRP and CSCER
!       4.6.0.13 chp 03/13/2012 CHP / TL Add ORYZA rice model, synch w/ 4.5.1.27
!       4.6.0.12 chp 01/03/2012 JIL fix potato read stmts 
!       4.6.0.11 chp 12/15/2011 JIL remove P4 from potato 
!       4.6.0.10 chp 12/09/2011 Remove ksat estimation
!       4.6.0.9  chp 12/08/2011 All codes changed to 046
!       4.6.0.8  chp 11/17/2011 GFF version - equivalent to v4.5.1.22
!       4.6.0.7  chp 11/10/2011 Revert to old drainage routines.
!                               Denitrification rate for flooded field = 50% NO3/d
!                               Fixed discontinuity in potential soil evap routine
!       4.6.0.6  chp 10/29/2011 Modified CO2 effects to transpiration (SPAM, TRANS)
!       4.6.0.5  chp 09/22/2011 Drainage modifications JTR
!                               Enabled Canola
!                               CO2 response for potato
!       4.6.0.4  chp 08/30/2011 Sorghum changes GH, CSCER, CSCRP changes, LAH.
!       4.6.0.3  chp 08/30/2011 Added vapor pressure as optional weather input.
!       4.6.0.2  gh  06/29/2011 Sorghum cul file re-order.
!       4.6.0.1  chp 06/28/2011 v4.6
!                               Changes to CSCER, CSCRP, incl. spe, eco, cul formats
!       4.5.1.0  chp 10/10/2010 V4.5 Release version
!       4.0.2.0  chp 08/11/2005 Release
!       4.0.1.0  chp 01/28/2004 Release Version 

!=======================================================================

!     Global constants
      INTEGER, PARAMETER :: 
     &    NL       = 20,  !Maximum number of soil layers 
     &    TS       = 24,  !Number of hourly time steps per day
     &    NAPPL    = 9000,!Maximum number of applications or operations
     &    NCOHORTS = 300, !Maximum number of cohorts
     &    NELEM    = 3,   !Number of elements modeled (currently N & P)
!            Note: set NELEM to 3 for now so Century arrays will match
     &    NumOfDays = 1000, !Maximum days in sugarcane run (FSR)
     &    NumOfStalks = 42, !Maximum stalks per sugarcane stubble (FSR)
     &    EvaluateNum = 40, !Number of evaluation variables
     &    MaxFiles = 200,   !Maximum number of output files
     &    MaxPest = 500    !Maximum number of pest operations

      REAL, PARAMETER :: 
     &    PI = 3.14159265,
     &    RAD=PI/180.0

      INTEGER, PARAMETER :: 
         !Dynamic variable values
     &    RUNINIT  = 1, 
     &    INIT     = 2,  !Will take the place of RUNINIT & SEASINIT
                         !     (not fully implemented)
     &    SEASINIT = 2, 
     &    RATE     = 3,
     &    EMERG    = 3,  !Used for some plant processes.  
     &    INTEGR   = 4,  
     &    OUTPUT   = 5,  
     &    SEASEND  = 6,
     &    ENDRUN   = 7 

      INTEGER, PARAMETER :: 
         !Nutrient array positions:
     &    N = 1,          !Nitrogen
     &    P = 2,          !Phosphorus
     &    Kel = 3         !Potassium

      CHARACTER(LEN=1)  SLASH  
      character(len=3)  exe_string
      CHARACTER(LEN=3)  ModelVerTxt
      CHARACTER(LEN=12) DSSATPRO 
      CHARACTER(LEN=11) STDPATH 
      CHARACTER(LEN=6)  LIBRARY    !library required for system calls

      CHARACTER*3 MonthTxt(12)
      DATA MonthTxt /'JAN','FEB','MAR','APR','MAY','JUN',
     &               'JUL','AUG','SEP','OCT','NOV','DEC'/

!=======================================================================

!     Data construct for control variables
      TYPE ControlType
        CHARACTER (len=1)  MESIC, RNMODE
        CHARACTER (len=2)  CROP
        CHARACTER (len=8)  MODEL, ENAME
        CHARACTER (len=12) FILEX
        CHARACTER (len=30) FILEIO
        CHARACTER (len=102)DSSATP
        CHARACTER (len=120) :: SimControl = 
     &  "                                                            "//
     &  "                                                            "
        INTEGER   DAS, DYNAMIC, FROP, ErrCode, LUNIO, MULTI, N_ELEMS
        INTEGER   NYRS, REPNO, ROTNUM, RUN, TRTNUM
        INTEGER   YRDIF, YRDOY, YRSIM
      END TYPE ControlType

!=======================================================================
!     Data construct for control switches
      TYPE SwitchType
        CHARACTER (len=1) FNAME
        CHARACTER (len=1) IDETC, IDETD, IDETG, IDETH, IDETL, IDETN
        CHARACTER (len=1) IDETO, IDETP, IDETR, IDETS, IDETW
        CHARACTER (len=1) IHARI, IPLTI, IIRRI, ISIMI
        CHARACTER (len=1) ISWCHE, ISWDIS, ISWNIT
        CHARACTER (len=1) ISWPHO, ISWPOT, ISWSYM, ISWTIL, ISWWAT
        CHARACTER (len=1) MEEVP, MEHYD, MEINF, MELI, MEPHO
        CHARACTER (len=1) MESOM, MESOL, MESEV, MEWTH
        CHARACTER (len=1) METMP !Temperature, EPIC
        CHARACTER (len=1) IFERI, IRESI, ICO2
        CHARACTER (len=1) FMOPT   ! VSH
        INTEGER NSWI
      END TYPE SwitchType

!Other switches and methods used by model:
! MELI, IOX - not used
! IDETH - only used in MgmtOps
! MEWTH - only used in WEATHR

!=======================================================================
!     Data construct for weather variables
      TYPE WeatherType
        SEQUENCE

!       Weather station information
        REAL REFHT, WINDHT, XLAT

!       Daily weather data.
        REAL CLOUDS, CO2, DAYL, DCO2, PAR, RAIN, RHUM, SNDN, SNUP, 
     &    SRAD, TAMP, TA, TAV, TAVG, TDAY, TDEW, TGROAV, TGRODY,      
     &    TMAX, TMIN, TWILEN, VAPR, WINDSP, VPDF, VPD_TRANSP

!       Hourly weather data
        REAL, DIMENSION(TS) :: AMTRH, AZZON, BETA, FRDIFP, FRDIFR, PARHR
        REAL, DIMENSION(TS) :: RADHR, RHUMHR, TAIRHR, TGRO, WINDHR

      END TYPE WeatherType

!=======================================================================
!     Data construct for soil variables
      TYPE SoilType
        INTEGER NLAYR
        CHARACTER (len=5) SMPX
        CHARACTER (len=10) SLNO
        CHARACTER (len=12) TEXTURE(NL)
        CHARACTER (len=17) SOILLAYERTYPE(NL)
        CHARACTER*50 SLDESC, TAXON
        
        LOGICAL COARSE(NL)
        
        REAL ALES, DMOD, SLPF         !DMOD was SLNF
        REAL CMSALB, MSALB, SWALB, SALB      !Albedo 
        REAL, DIMENSION(NL) :: BD, CEC, CLAY, DLAYR, DS, DUL
        REAL, DIMENSION(NL) :: KG2PPM, LL, OC, PH, PHKCL
        REAL, DIMENSION(NL) :: SAND, SAT, SILT, STONES, SWCN
        
      !Residual water content
        REAL, DIMENSION(NL) :: WCR

      !vanGenuchten parameters
        REAL, DIMENSION(NL) :: alphaVG, mVG, nVG

      !Second tier soils data:
        REAL, DIMENSION(NL) :: CACO3, EXTP, ORGP, PTERMA, PTERMB
        REAL, DIMENSION(NL) :: TOTP, TOTBAS, EXCA, EXK, EXNA

      !Soil analysis data 
        REAL, DIMENSION(NL) :: SASC   !stable organic C

      !Variables added with new soil format:
        REAL ETDR, PONDMAX, SLDN, SLOPE
!       REAL, DIMENSION(NL) :: RCLPF, RGIMPF

      !Variables deleted with new soil format:
      !Still needed for Ritchie hydrology
        REAL CN, SWCON, U
        REAL, DIMENSION(NL) :: ADCOEF, TOTN, TotOrgN, WR

      !Text describing soil layer depth data
      !1-9 describe depths for layers 1-9
      !10 depths for layers 10 thru NLAYR (if NLAYR > 9)
      !11 depths for layers 5 thru NLAYR (if NLAYR > 4)
        CHARACTER*8 LayerText(11)

      !These variables could be made available if needed elsewhere.
      !  They are currently read by SOILDYN module.
      !  CHARACTER*5 SLTXS
      !  CHARACTER*11 SLSOUR
      !  CHARACTER*50 SLDESC, TAXON

      !Second tier soils data that could be used:
!        REAL, DIMENSION(NL) :: EXTAL, EXTFE, EXTMN, 
!        REAL, DIMENSION(NL) :: EXMG, EXTS, SLEC

      END TYPE SoilType

!=======================================================================
!     Data construct for mulch layer
      TYPE MulchType
        REAL MULCHMASS    !Mass of surface mulch layer (kg[dry mat.]/ha)
        REAL MULCHALB     !Albedo of mulch layer
        REAL MULCHCOVER   !Coverage of mulch layer (frac. of surface)
        REAL MULCHTHICK   !Thickness of mulch layer (mm)
        REAL MULCHWAT     !Water content of mulch (mm3/mm3)
        REAL MULCHEVAP    !Evaporation from mulch layer (mm/d)
        REAL MULCHSAT     !Saturation water content of mulch (mm3/mm3)
        REAL MULCHN       !N content of mulch layer (kg[N]/ha)
        REAL MULCHP       !P content of mulch layer (kg[P]/ha)
        REAL NEWMULCH     !Mass of new surface mulch (kg[dry mat.]/ha)
        REAL NEWMULCHWAT  !Water content of new mulch ((mm3/mm3)
        REAL MULCH_AM     !Area covered / dry weight of residue (ha/kg)
        REAL MUL_EXTFAC   !Light extinction coef. for mulch layer
        REAL MUL_WATFAC   !Saturation water content (mm[water] ha kg-1)
      END TYPE MulchType

!=======================================================================
!     Data construct for tillage operations
      TYPE TillType
        INTEGER NTIL      !Total number of tillage events in FILEX
        INTEGER TILDATE   !Date of current tillage event

!       Maximum values for multiple events in a single day
        REAL TILDEP, TILMIX, TILRESINC

!       Irrigation amount which affects tilled soil properties 
!          expressed in units of equivalent rainfall depth
        REAL TIL_IRR   

!       Allows multiple tillage passes in a day
        INTEGER NTil_today !number of tillage events today (max 3)
        INTEGER, DIMENSION(NAPPL) :: NLYRS
        REAL, DIMENSION(NAPPL) :: CNP, TDEP
        REAL, DIMENSION(NAPPL,NL) :: BDP, DEP, SWCNP
      END TYPE TillType

!=======================================================================
!     Data construct for oxidation layer
      TYPE OxLayerType
        INTEGER IBP
        REAL    OXU, OXH4, OXN3   
        REAL    OXLT, OXMIN4, OXMIN3
        REAL    DLTOXU, DLTOXH4, DLTOXN3
        REAL    ALGACT
        LOGICAL DailyCalc
      END TYPE OxLayerType

!======================================================================
!     Fertilizer application data
      TYPE FertType
        CHARACTER*7 AppType != 'UNIFORM', 'BANDED ' or 'HILL   '
        INTEGER FERTDAY, FERTYPE
        INTEGER, DIMENSION(NELEM) :: NAPFER
        REAL FERDEPTH, FERMIXPERC
        REAL ADDFNH4, ADDFNO3, ADDFUREA
        REAL ADDOXU, ADDOXH4, ADDOXN3
        REAL, DIMENSION(NELEM) :: AMTFER
        REAL, DIMENSION(NL) :: ADDSNH4, ADDSNO3, ADDUREA
        REAL, DIMENSION(NL) :: ADDSPi
        REAL, DIMENSION(NL) :: ADDSKi
        LOGICAL UNINCO
      END TYPE FertType

!=======================================================================
!   Data construct for residue (harvest residue, senesced matter, etc.)
      TYPE ResidueType
        REAL, DIMENSION(0:NL) :: ResWt        !kg[dry matter]/ha/d
        REAL, DIMENSION(0:NL) :: ResLig       !kg[lignin]/ha/d
        REAL, DIMENSION(0:NL,NELEM) :: ResE   !kg[E]/ha/d (E=N,P,K,..)
        REAL  CumResWt                        !cumul. kg[dry matter]/ha
        REAL, DIMENSION(NELEM) :: CumResE     !cumulative kg[E]/ha
      END TYPE ResidueType

!======================================================================
!     Organic Matter Application data
      TYPE OrgMatAppType
        INTEGER NAPRes, ResDat, ResDepth
        CHARACTER (len=5) RESTYPE
        REAL ResMixPerc   !Percent mixing rate for SOM module

        REAL, DIMENSION(0:NL) :: ResWt        !kg[dry matter]/ha/d
        REAL, DIMENSION(0:NL) :: ResLig       !kg[lignin]/ha/d
        REAL, DIMENSION(0:NL,NELEM) :: ResE   !kg[E]/ha/d (E=N, P, ..)
        REAL  CumResWt                        !cumul. kg[dry matter]/ha
        REAL, DIMENSION(NELEM) :: CumResE     !cumulative kg[E]/ha
      END TYPE OrgMatAppType

!======================================================================
!     Plant stresses for environmental stress summary
      Type PlStresType
        INTEGER NSTAGES   !# of stages (max 5)
        CHARACTER(len=23) StageName(0:5)
        REAL W_grow, W_phot, N_grow, N_phot
        REAL P_grow, P_phot
        LOGICAL ACTIVE(0:5)
      End Type PlStresType

!======================================================================
!     Array of output files, aliases, unit numbers, etc.
      Type OutputType
        INTEGER NumFiles
        CHARACTER*16, DIMENSION(MaxFiles) :: FileName
        CHARACTER*2,  DIMENSION(MaxFiles) :: OPCODE
        CHARACTER*50, DIMENSION(MaxFiles) :: Description
        CHARACTER*10, DIMENSION(MaxFiles) :: ALIAS
        INTEGER, DIMENSION(MaxFiles) :: LUN
      End Type


!======================================================================
      CONTAINS

!----------------------------------------------------------------------
      SUBROUTINE SETOP ()
!     Set variables for current operating system
      IMPLICIT NONE

      WRITE(ModelVerTxt,'(I2.2,I1)') Version%Major, Version%Minor

!      call op_sys(slash,dssatpro,stdpath)
      SELECT CASE (OPSYS)
      CASE ('WINDO','DOS  ')
!       DOS, Windows
        SLASH = '\' 
        DSSATPRO = 'DSSATPRO.V46'
!       Note: Use DSSAT45 directory for now. 
C-GH    Set to DSSAT46
        STDPATH = 'C:\DSSAT46\' 
        exe_string = 'EXE'

      CASE ('LINUX','UNIX ')
!       Linux, Unix
        SLASH = '/' 
        DSSATPRO = 'DSSATPRO.L46'
        STDPATH = '../DSSAT46/'
        exe_string = '.so'
      END SELECT

      END SUBROUTINE SETOP

!======================================================================
      END MODULE ModuleDefs
!======================================================================



!======================================================================
!     Paddy Managment routines.
!======================================================================
      MODULE FloodModule
!=======================================================================
!     Data construct for flood data. 
      Type FloodWatType
        !From IRRIG
        LOGICAL BUNDED        
        INTEGER NBUND         
        REAL ABUND            
        REAL PUDPERC, PERC
        REAL PLOWPAN    !Depth of puddling (m) (ORYZA)

        !From Paddy_Mgmt
        INTEGER YRDRY, YRWET  
        REAL FLOOD, FRUNOFF   
        REAL TOTBUNDRO        
        LOGICAL PUDDLED       

        REAL CEF, EF          !From SPAM
        REAL INFILT, RUNOFF   !From WATBAL
      End Type FloodWatType

      Type FloodNType
        INTEGER NDAT
        REAL    ALGFON                        !Algae kill or dry-up
        REAL    FLDH4C, FLDN3C                !Flood N concentrations
        REAL    FLDU, FLDN3, FLDH4            !Flood N mass (kg/ha)
        REAL    FRNH4U, FRNO3U                !Flood N uptake (kg/ha)
        REAL    DLTFUREA, DLTFNH4, DLTFNO3    !Flood N flux (kg/ha)
      End Type FloodNType

      END MODULE FloodModule
!======================================================================

!=======================================================================
!  MODULE ModuleData
!  01/22/2008 CHP Written
!=======================================================================

      MODULE ModuleData
!     Data storage and retrieval module.
!     Defines data structures that hold information that can be 
!       stored or accessed by query.  

!     A call to the GET routine will return the value of variable 
!       requested.  The procedure is "overloaded", i.e., the procedure 
!       executed will depend on the type of variable(s) in the argument 
!       list.  A request for a "real" data type will invoke the GET_Real
!       procedure, for example.  

!     Similarly, a call to the PUT routine will store the data sent.
!       It is also an overloaded procedure including several different
!       types of data which can be stored.

!     The SAVE_data variable is used to store all information.

!     To add a variable for storage and retrieval: 
!     1.  Add the variable to one of the Type constructs based on the 
!         module that "owns" the variable, for example SPAMType, Planttype 
!         or MgmtType.
!     2.  For a real data type, add a line of code in both the GET_Real and
!         PUT_Real subroutines.  
!     3.  For an integer data type, add a line of code in both the 
!         GET_Integer and PUT_Integer subroutines.  
!     4.  All routines accessing GET or PUT procedures must include a 
!         "USE ModuleData" statement.
!     5.  A call to the PUT routine must be used to store data prior to
!         a call to the GET routine to retrive the data.

      USE ModuleDefs
      SAVE

!======================================================================
!     Data transferred from hourly energy balance 
      Type SPAMType
        REAL AGEFAC, PG                   !photosynthese
        REAL CEF, CEM, CEO, CEP, CES, CET !Cumulative ET - mm
        REAL  EF,  EM,  EO,  EP,  ES,  ET !Daily ET - mm/d
        REAL  EOP, EVAP                   !Daily mm/d
        REAL, DIMENSION(NL) :: UH2O       !Root water uptake
      End Type SPAMType

!     Data transferred from CROPGRO routine 
      TYPE PlantType
        REAL CANHT, CANWH, DXR57, EXCESS,
     &    PLTPOP, RNITP, SLAAD, XPOD
        REAL BIOMAS
        INTEGER NR5, iSTAGE, iSTGDOY
        CHARACTER*10 iSTNAME
      END TYPE PlantType

!     Data transferred from management routine 
      Type MgmtType
        REAL DEPIR, EFFIRR, FERNIT, IRRAMT, TOTIR, TOTEFFIRR
        REAL V_AVWAT(20)    ! Create vectors to save growth stage based irrigation
        REAL V_IMDEP(20)
        REAL V_ITHRL(20)
        REAL V_ITHRU(20)
        INTEGER V_IRON(20)
        REAL V_IRAMT(20)
        REAL V_IREFF(20)
        REAL V_IFREQ(20)
        INTEGER GSIRRIG
        CHARACTER*5 V_IRONC(20)
      End Type MgmtType

!     Data transferred from Soil water routine
      Type WatType
        REAL DRAIN, RUNOFF, SNOW
      End Type WatType

!     Data transferred from Soil Inorganic Nitrogen routine
      Type NiType
        REAL TNOXD, TLCHD
      End Type NiType

!     Data transferred from Organic C routines
      Type OrgCType
        REAL TOMINFOM, TOMINSOM, TOMINSOM1, TOMINSOM2
        REAL TOMINSOM3, TNIMBSOM
        REAL MULCHMASS
      End Type OrgCType

!     Data from weather
      Type WeathType
        Character*8 WSTAT
      End Type WeathType

      TYPE PDLABETATYPE
        REAL PDLA
        REAL BETALS
      END TYPE

!     Data which can be transferred between modules
      Type TransferType
        Type (ControlType) CONTROL
        Type (SwitchType)  ISWITCH
        Type (OutputType)  OUTPUT
        Type (PlantType)   PLANT
        Type (MgmtType)    MGMT
        Type (NiType)      NITR
        Type (OrgCType)    ORGC
        Type (SoilType)    SOILPROP
        Type (SPAMType)    SPAM
        Type (WatType)     WATER
        Type (WeathType)   WEATHER
        TYPE (PDLABETATYPE) PDLABETA
      End Type TransferType

!     The variable SAVE_data contains all of the components to be 
!     stored and retrieved.
      Type (TransferType) SAVE_data

!======================================================================
!     GET and PUT routines are differentiated by argument type.  All of 
!       these procedures can be accessed with a CALL GET(...)
      INTERFACE GET
         MODULE PROCEDURE GET_Control
     &                  , GET_ISWITCH 
     &                  , GET_Output 
     &                  , GET_SOILPROP
!     &                  , GET_Weather
     &                  , GET_Real 
     &                  , GET_Real_Array_NL
     &                  , GET_Integer
     &                  , GET_Char
      END INTERFACE

      INTERFACE PUT
         MODULE PROCEDURE PUT_Control
     &                  , PUT_ISWITCH 
     &                  , PUT_Output 
     &                  , PUT_SOILPROP
!     &                  , PUT_Weather
     &                  , PUT_Real 
     &                  , PUT_Real_Array_NL
     &                  , PUT_Integer
     &                  , PUT_Char
      END INTERFACE

      CONTAINS

!----------------------------------------------------------------------
      Subroutine Get_CONTROL (CONTROL_arg)
!     Retrieves CONTROL variable
      IMPLICIT NONE
      Type (ControlType) Control_arg
      Control_arg = SAVE_data % Control
      Return
      End Subroutine Get_CONTROL

!----------------------------------------------------------------------
      Subroutine Put_CONTROL (CONTROL_arg)
!     Stores CONTROL variable
      IMPLICIT NONE
      Type (ControlType) Control_arg
      SAVE_data % Control = Control_arg
      Return
      End Subroutine Put_CONTROL

!----------------------------------------------------------------------
      Subroutine Get_ISWITCH (ISWITCH_arg)
!     Retrieves ISWITCH variable
      IMPLICIT NONE
      Type (SwitchType) ISWITCH_arg
      ISWITCH_arg = SAVE_data % ISWITCH
      Return
      End Subroutine Get_ISWITCH

!----------------------------------------------------------------------
      Subroutine Put_ISWITCH (ISWITCH_arg)
!     Stores ISWITCH variable
      IMPLICIT NONE
      Type (SwitchType) ISWITCH_arg
      SAVE_data % ISWITCH = ISWITCH_arg
      Return
      End Subroutine Put_ISWITCH

!----------------------------------------------------------------------
      SUBROUTINE GET_OUTPUT(OUTPUT_ARG)
!     Retrieves OUTPUT variable as needed
      IMPLICIT NONE
      TYPE (OutputType) OUTPUT_ARG
      OUTPUT_ARG = SAVE_data % OUTPUT
      RETURN
      END SUBROUTINE GET_OUTPUT

!----------------------------------------------------------------------
      SUBROUTINE PUT_OUTPUT(OUTPUT_ARG)
!     Stores OUTPUT variable 
      IMPLICIT NONE
      TYPE (OutputType) OUTPUT_ARG
      SAVE_data % OUTPUT = OUTPUT_ARG
      RETURN
      END SUBROUTINE PUT_OUTPUT

!----------------------------------------------------------------------
      SUBROUTINE GET_SOILPROP(SOIL_ARG)
!     Retrieves SOILPROP variable as needed
      IMPLICIT NONE
      TYPE (SoilType) SOIL_ARG
      SOIL_ARG = SAVE_data % SOILPROP
      RETURN
      END SUBROUTINE GET_SOILPROP

!----------------------------------------------------------------------
      SUBROUTINE PUT_SOILPROP(SOIL_ARG)
!     Stores SOILPROP variable 
      IMPLICIT NONE
      TYPE (SoilType) SOIL_ARG
      SAVE_data % SOILPROP = SOIL_ARG
      RETURN
      END SUBROUTINE PUT_SOILPROP

!!----------------------------------------------------------------------
!      SUBROUTINE GET_WEATHER(WEATHER_ARG)
!!     Retrieves WEATHER variable as needed
!      IMPLICIT NONE
!      TYPE (WeathType) WEATHER_ARG
!      WEATHER_ARG = SAVE_data % WEATHER
!      RETURN
!      END SUBROUTINE GET_WEATHER
!
!!----------------------------------------------------------------------
!      SUBROUTINE PUT_WEATHER(WEATHER_ARG)
!!     Stores WEATHER variable 
!      IMPLICIT NONE
!      TYPE (WeathType) WEATHER_ARG
!      SAVE_data % WEATHER = WEATHER_ARG
!      RETURN
!      END SUBROUTINE PUT_WEATHER

!----------------------------------------------------------------------
      Subroutine GET_Real(ModuleName, VarName, Value)
!     Retrieves real variable from SAVE_data.  Variable must be
!         included as a component of SAVE_data. 
      IMPLICIT NONE
      Character*(*) ModuleName, VarName
      Character*78 MSG(2)
      Real Value
      Logical ERR

      Value = 0.0
      ERR = .FALSE.

      SELECT CASE (ModuleName)
      Case ('SPAM')
        SELECT CASE (VarName)
        Case ('AGEFAC'); Value = SAVE_data % SPAM % AGEFAC
        Case ('PG');     Value = SAVE_data % SPAM % PG
        Case ('CEF');    Value = SAVE_data % SPAM % CEF
        Case ('CEM');    Value = SAVE_data % SPAM % CEM
        Case ('CEO');    Value = SAVE_data % SPAM % CEO
        Case ('CEP');    Value = SAVE_data % SPAM % CEP
        Case ('CES');    Value = SAVE_data % SPAM % CES
        Case ('CET');    Value = SAVE_data % SPAM % CET
        Case ('EF');     Value = SAVE_data % SPAM % EF
        Case ('EM');     Value = SAVE_data % SPAM % EM
        Case ('EO');     Value = SAVE_data % SPAM % EO
        Case ('EP');     Value = SAVE_data % SPAM % EP
        Case ('ES');     Value = SAVE_data % SPAM % ES
        Case ('ET');     Value = SAVE_data % SPAM % ET
        Case ('EOP');    Value = SAVE_data % SPAM % EOP
        Case ('EVAP');   Value = SAVE_data % SPAM % EVAP
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('PLANT')
        SELECT CASE (VarName)
        Case ('BIOMAS'); Value = SAVE_data % PLANT % BIOMAS
        Case ('CANHT') ; Value = SAVE_data % PLANT % CANHT
        Case ('CANWH') ; Value = SAVE_data % PLANT % CANWH
        Case ('DXR57') ; Value = SAVE_data % PLANT % DXR57
        Case ('EXCESS'); Value = SAVE_data % PLANT % EXCESS
        Case ('PLTPOP'); Value = SAVE_data % PLANT % PLTPOP
        Case ('RNITP') ; Value = SAVE_data % PLANT % RNITP
        Case ('SLAAD') ; Value = SAVE_data % PLANT % SLAAD
        Case ('XPOD')  ; Value = SAVE_data % PLANT % XPOD
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('MGMT')
        SELECT CASE (VarName)
        Case ('EFFIRR'); Value = SAVE_data % MGMT % EFFIRR
        Case ('TOTIR');  Value = SAVE_data % MGMT % TOTIR
        Case ('TOTEFFIRR');Value=SAVE_data % MGMT % TOTEFFIRR
        Case ('DEPIR');  Value = SAVE_data % MGMT % DEPIR
        Case ('IRRAMT'); Value = SAVE_data % MGMT % IRRAMT
        Case ('FERNIT'); Value = SAVE_data % MGMT % FERNIT
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('WATER')
        SELECT CASE (VarName)
        Case ('DRAIN'); Value = SAVE_data % WATER % DRAIN
        Case ('RUNOFF');Value = SAVE_data % WATER % RUNOFF
        Case ('SNOW');  Value = SAVE_data % WATER % SNOW
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('NITR')
        SELECT CASE (VarName)
        Case ('TNOXD'); Value = SAVE_data % NITR % TNOXD
        Case ('TLCHD'); Value = SAVE_data % NITR % TLCHD
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('ORGC')
        SELECT CASE (VarName)
        Case ('MULCHMASS');Value = SAVE_data % ORGC % MULCHMASS
        Case ('TOMINFOM'); Value = SAVE_data % ORGC % TOMINFOM
        Case ('TOMINSOM'); Value = SAVE_data % ORGC % TOMINSOM
        Case ('TOMINSOM1');Value = SAVE_data % ORGC % TOMINSOM1
        Case ('TOMINSOM2');Value = SAVE_data % ORGC % TOMINSOM2
        Case ('TOMINSOM3');Value = SAVE_data % ORGC % TOMINSOM3
        Case ('TNIMBSOM'); Value = SAVE_data % ORGC % TNIMBSOM
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('SOIL')
        SELECT CASE (VarName)
        Case ('TOMINFOM'); Value = SAVE_data % ORGC % TOMINFOM
        Case ('TOMINSOM'); Value = SAVE_data % ORGC % TOMINSOM
        Case ('TOMINSOM1');Value = SAVE_data % ORGC % TOMINSOM1
        Case ('TOMINSOM2');Value = SAVE_data % ORGC % TOMINSOM2
        Case ('TOMINSOM3');Value = SAVE_data % ORGC % TOMINSOM3
        Case ('TNIMBSOM'); Value = SAVE_data % ORGC % TNIMBSOM
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      CASE ('PDLABETA')
        SELECT CASE(VarName)
        CASE('PDLA'); Value = SAVE_data % PDLABETA % PDLA
        CASE('BETA'); Value = SAVE_data % PDLABETA % BETALS
        CASE DEFAULT; ERR = .TRUE.
        END SELECT
            
      Case DEFAULT; ERR = .TRUE.
      END SELECT

      IF (ERR) THEN
        WRITE(MSG(1),'("Error transferring variable: ",A, " in ",A)') 
     &      Trim(VarName), Trim(ModuleName)
        MSG(2) = 'Value set to zero.'
        CALL WARNING(2,'GET_REAL',MSG)
      ENDIF

      RETURN
      END SUBROUTINE GET_Real

!----------------------------------------------------------------------
      SUBROUTINE PUT_Real(ModuleName, VarName, Value)
!     Stores real variable SAVE_data.  
      IMPLICIT NONE
      Character*(*) ModuleName, VarName
      Character*78 MSG(2)
      Real Value
      Logical ERR

      ERR = .FALSE.

      SELECT CASE (ModuleName)
      Case ('SPAM')
        SELECT CASE (VarName)
        Case ('AGEFAC'); SAVE_data % SPAM % AGEFAC = Value
        Case ('PG');     SAVE_data % SPAM % PG     = Value
        Case ('CEF');    SAVE_data % SPAM % CEF    = Value
        Case ('CEM');    SAVE_data % SPAM % CEM    = Value
        Case ('CEO');    SAVE_data % SPAM % CEO    = Value
        Case ('CEP');    SAVE_data % SPAM % CEP    = Value
        Case ('CES');    SAVE_data % SPAM % CES    = Value
        Case ('CET');    SAVE_data % SPAM % CET    = Value
        Case ('EF');     SAVE_data % SPAM % EF     = Value
        Case ('EM');     SAVE_data % SPAM % EM     = Value
        Case ('EO');     SAVE_data % SPAM % EO     = Value
        Case ('EP');     SAVE_data % SPAM % EP     = Value
        Case ('ES');     SAVE_data % SPAM % ES     = Value
        Case ('ET');     SAVE_data % SPAM % ET     = Value
        Case ('EOP');    SAVE_data % SPAM % EOP    = Value
        Case ('EVAP');   SAVE_data % SPAM % EVAP   = Value
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('PLANT')
        SELECT CASE (VarName)
        Case ('BIOMAS'); SAVE_data % PLANT % BIOMAS = Value
        Case ('CANHT');  SAVE_data % PLANT % CANHT  = Value
        Case ('CANWH');  SAVE_data % PLANT % CANWH  = Value
        Case ('DXR57');  SAVE_data % PLANT % DXR57  = Value
        Case ('EXCESS'); SAVE_data % PLANT % EXCESS = Value
        Case ('PLTPOP'); SAVE_data % PLANT % PLTPOP = Value
        Case ('RNITP');  SAVE_data % PLANT % RNITP  = Value
        Case ('SLAAD');  SAVE_data % PLANT % SLAAD  = Value
        Case ('XPOD');   SAVE_data % PLANT % XPOD   = Value
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('MGMT')
        SELECT CASE (VarName)
        Case ('EFFIRR'); SAVE_data % MGMT % EFFIRR = Value
        Case ('TOTIR');  SAVE_data % MGMT % TOTIR  = Value
        Case ('TOTEFFIRR');SAVE_data%MGMT % TOTEFFIRR=Value
        Case ('DEPIR');  SAVE_data % MGMT % DEPIR  = Value
        Case ('IRRAMT'); SAVE_data % MGMT % IRRAMT = Value
        Case ('FERNIT'); SAVE_data % MGMT % FERNIT = Value
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('WATER')
        SELECT CASE (VarName)
        Case ('DRAIN'); SAVE_data % WATER % DRAIN  = Value
        Case ('RUNOFF');SAVE_data % WATER % RUNOFF = Value
        Case ('SNOW');  SAVE_data % WATER % SNOW   = Value
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('NITR')
        SELECT CASE (VarName)
        Case ('TNOXD'); SAVE_data % NITR % TNOXD = Value
        Case ('TLCHD'); SAVE_data % NITR % TLCHD = Value
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('ORGC')
        SELECT CASE (VarName)
        Case ('MULCHMASS');SAVE_data % ORGC % MULCHMASS = Value
        Case ('TOMINFOM'); SAVE_data % ORGC % TOMINFOM  = Value
        Case ('TOMINSOM'); SAVE_data % ORGC % TOMINSOM  = Value
        Case ('TOMINSOM1');SAVE_data % ORGC % TOMINSOM1 = Value
        Case ('TOMINSOM2');SAVE_data % ORGC % TOMINSOM2 = Value
        Case ('TOMINSOM3');SAVE_data % ORGC % TOMINSOM3 = Value
        Case ('TNIMBSOM'); SAVE_data % ORGC % TNIMBSOM  = Value
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      CASE ('PDLABETA')
        SELECT CASE(VarName)
        CASE('PDLA'); SAVE_data % PDLABETA % PDLA = Value
        CASE('BETA'); SAVE_data % PDLABETA % BETALS = Value
        CASE DEFAULT; ERR = .TRUE.
        END SELECT
            
      Case DEFAULT; ERR = .TRUE.
      END SELECT

      IF (ERR) THEN
        WRITE(MSG(1),'("Error transferring variable: ",A, "in ",A)') 
     &      Trim(VarName), Trim(ModuleName)
        MSG(2) = 'Value not saved! Errors may result.'
        CALL WARNING(2,'PUT_REAL',MSG)
      ENDIF

      RETURN
      END SUBROUTINE PUT_Real

!----------------------------------------------------------------------
      SUBROUTINE GET_Real_Array_NL(ModuleName, VarName, Value)
!     Retrieves array of dimension(NL) 
      IMPLICIT NONE
      Character*(*) ModuleName, VarName
      Character*78 MSG(2)
      REAL, DIMENSION(NL) :: Value
      Logical ERR

      Value = 0.0
      ERR = .FALSE.

      SELECT CASE (ModuleName)

      CASE ('SPAM')
        SELECT CASE (VarName)
          CASE ('UH2O'); ; Value = SAVE_data % SPAM % UH2O
          CASE DEFAULT; ERR = .TRUE.
        END SELECT

        CASE DEFAULT; ERR = .TRUE.
      END SELECT

      IF (ERR) THEN
        WRITE(MSG(1),'("Error transferring variable: ",A, "in ",A)') 
     &      Trim(VarName), Trim(ModuleName)
        MSG(2) = 'Value set to zero.'
        CALL WARNING(2,'GET_Real_Array_NL',MSG)
      ENDIF

      RETURN
      END SUBROUTINE GET_Real_Array_NL

!----------------------------------------------------------------------
      SUBROUTINE PUT_Real_Array_NL(ModuleName, VarName, Value)
!     Stores array of dimension NL
      IMPLICIT NONE
      Character*(*) ModuleName, VarName
      Character*78 MSG(2)
      REAL, DIMENSION(NL) :: Value
      Logical ERR

      ERR = .FALSE.

      SELECT CASE (ModuleName)
      Case ('SPAM')
        SELECT CASE (VarName)
        Case ('UH2O'); SAVE_data % SPAM % UH2O = Value
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case DEFAULT; ERR = .TRUE.
      END SELECT

      IF (ERR) THEN
        WRITE(MSG(1),'("Error transferring variable: ",A, "in ",A)') 
     &      Trim(VarName), Trim(ModuleName)
        MSG(2) = 'Value not saved! Errors may result.'
        CALL WARNING(2,'PUT_Real_Array_NL',MSG)
      ENDIF

      RETURN
      END SUBROUTINE PUT_Real_Array_NL

!----------------------------------------------------------------------
      Subroutine GET_Integer(ModuleName, VarName, Value)
!     Retrieves Integer variable as needed
      IMPLICIT NONE
      Character*(*) ModuleName, VarName
      Character*78  MSG(2)
      Integer Value
      Logical ERR

      Value = 0
      ERR = .FALSE.

      SELECT CASE (ModuleName)
      Case ('PLANT')
        SELECT CASE (VarName)
        Case ('NR5');  Value = SAVE_data % PLANT % NR5
        Case ('iSTAGE');  Value = SAVE_data % PLANT % iSTAGE
        Case ('iSTGDOY'); Value = SAVE_data % PLANT % iSTGDOY
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case Default; ERR = .TRUE.
      END SELECT

      IF (ERR) THEN
        WRITE(MSG(1),'("Error transferring variable: ",A, "in ",A)') 
     &      Trim(VarName), Trim(ModuleName)
        MSG(2) = 'Value set to zero.'
        CALL WARNING(2,'GET_INTEGER',MSG)
      ENDIF

      RETURN
      END SUBROUTINE GET_Integer

!----------------------------------------------------------------------
      SUBROUTINE PUT_Integer(ModuleName, VarName, Value)
!     Stores Integer variable
      IMPLICIT NONE
      Character*(*) ModuleName, VarName
      Character*78 MSG(2)
      Integer Value
      Logical ERR

      ERR = .FALSE.

      SELECT CASE (ModuleName)
      Case ('PLANT')
        SELECT CASE (VarName)
        Case ('NR5');  SAVE_data % PLANT % NR5  = Value
        Case ('iSTAGE');  SAVE_data % PLANT % iSTAGE  = Value
        Case ('iSTGDOY'); SAVE_data % PLANT % iSTGDOY = Value
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case DEFAULT; ERR = .TRUE.
      END SELECT

      IF (ERR) THEN
        WRITE(MSG(1),'("Error transferring variable: ",A, "in ",A)') 
     &      Trim(VarName), Trim(ModuleName)
        MSG(2) = 'Value not saved! Errors may result.'
        CALL WARNING(2,'PUT_Integer',MSG)
      ENDIF

      RETURN
      END SUBROUTINE PUT_Integer

!----------------------------------------------------------------------
      Subroutine GET_Char(ModuleName, VarName, Value)
!     Retrieves Integer variable as needed
      IMPLICIT NONE
      Character*(*) ModuleName, VarName, Value
      Character*78  MSG(2)
      Logical ERR

      Value = ' '
      ERR = .FALSE.

      SELECT CASE (ModuleName)
      Case ('WEATHER')
        SELECT CASE (VarName)
        Case ('WSTA');  Value = SAVE_data % WEATHER % WSTAT
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('PLANT')
        SELECT CASE (VarName)
        Case ('iSTNAME');  Value = SAVE_data % PLANT % iSTNAME
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case Default; ERR = .TRUE.
      END SELECT

      IF (ERR) THEN
        WRITE(MSG(1),'("Error transferring variable: ",A, "in ",A)') 
     &      Trim(VarName), Trim(ModuleName)
        MSG(2) = 'Value set to zero.'
        CALL WARNING(2,'GET_INTEGER',MSG)
      ENDIF

      RETURN
      END SUBROUTINE GET_Char

!----------------------------------------------------------------------
      SUBROUTINE PUT_Char(ModuleName, VarName, Value)
!     Stores Character variable
      IMPLICIT NONE
      Character*(*) ModuleName, VarName, Value
      Character*78 MSG(2)
      Logical ERR

      ERR = .FALSE.

      SELECT CASE (ModuleName)
      Case ('WEATHER')
        SELECT CASE (VarName)
        Case ('WSTA');  SAVE_data % WEATHER % WSTAT  = Value
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('PLANT')
        SELECT CASE (VarName)
        Case ('iSTNAME');  SAVE_data % PLANT % iSTNAME = Value
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case DEFAULT; ERR = .TRUE.
      END SELECT

      IF (ERR) THEN
        WRITE(MSG(1),'("Error transferring variable: ",A, "in ",A)') 
     &      Trim(VarName), Trim(ModuleName)
        MSG(2) = 'Value not saved! Errors may result.'
        CALL WARNING(2,'PUT_Integer',MSG)
      ENDIF

      RETURN
      END SUBROUTINE PUT_Char

!======================================================================
      END MODULE ModuleData
!======================================================================
