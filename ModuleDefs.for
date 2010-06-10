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
!=======================================================================

      MODULE ModuleDefs
!     Contains defintion of derived data types and constants which are 
!     used throughout the model.
      SAVE

!=======================================================================
!     Change this line to switch between Windows and Linux compilers
!     Operating system
      CHARACTER(LEN=5), PARAMETER :: OPSYS = 'WINDO'   !DOS, Windows
!     CHARACTER(LEN=5), PARAMETER :: OPSYS = 'LINUX'   !Linux, UNIX

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
        INTEGER :: Minor = 5
        INTEGER :: Model = 0
        INTEGER :: Build =48
      END TYPE VersionType
      TYPE (VersionType) Version

!     Version history:
!       4.5.0.48 chp 06/09/2010 Post-Griffin Workshop.
!       4.5.0.47 chp 05/06/2010 Griffin Workshop.
!       4.5.0.46 chp 04/22/2010 Further changes to CSCRP genotype files.
!       4.5.0.45 chp 04/16/2010 Changes to CSCER, CSCRP genotype files.
!       4.5.0.44 chp 03/19/2010 Canegro cultivar file has EXPNO.
!       4.5.0.43 chp 03/19/2010 Sensitivity analysis parameters updated or disabled.
!       4.5.0.42 chp 02/09/2010 Error handling, batch continues.
!       4.5.0.41 chp 02/08/2010 Resource productivity.
!       4.5.0.40 chp 01/12/2010 Ammonia volatilization bug fixed.
!       4.5.0.39 chp 01/08/2010 Changes to default switch settings, output options
!       4.5.0.38 chp 11/02/2009 GIT source control starting version
!       4.5.0.37 chp 09/09/2009 Gainesville meeting version
!       4.5.0.36 chp 06/18/2009 Pre-release version
!       4.5.0.35 chp 03/23/2009 Pre-release workshops
!       4.5.0.34 chp 01/29/2009 Post-workshop version
!       4.5.0.33 chp 12/07/2008 Manoa workshop version, Pre-release
!       4.5.0.32 chp 11/25/2008 Mauna Loa CO2 default
!       4.5.0.31 chp 10/15/2008 Ksat issues 
!       4.5.0.30 chp 09/03/2008 Minor changes 
!       4.5.0.29 chp 08/08/2008 Compiler directives and OPSYS for Linux
!       4.5.0.28 chp 07/10/2008 Pre-release version -- no more major changes!
!       4.5.0.27 chp 05/29/2008 Post-workshop version
!       4.5.0.26 chp 04/28/2008 K model added
!       4.5.0.25 chp 03/27/2008 P, SOM model modifications
!       4.5.0.24 chp 02/12/2008 Changes to Century decomposition rates
!       4.5.0.23 chp 01/23/2008 Added ModuleData -- need to transfer data to Tony's routines.
!       4.5.0.22 chp 01/02/2008 Happy New Year version
!       4.5.0.21 chp 11/27/2007 Cultivar file changes
!       4.5.0.20 chp 10/25/2007 Argentina workshop version (not actually used)
!       4.5.0.19 chp 10/10/2007 Pre-DSSAT meeting Gainesville
!       4.5.0.18 chp 09/30/2007 Added simulation control file.
!       4.5.0.17 chp 08/19/2007 Minor changes.
!       4.5.0.16 chp 07/24/2007 South Africa Workshop model.
!       4.5.0.15 chp 07/17/2007 Pre-South Africa Workshop model.
!       4.5.0.14 chp 07/05/2007 Default simulation control file added.
!       4.5.0.13 chp 06/29/2007 ISWITCH as input module argument
!       4.5.0.12 chp 06/27/2007 Revisions to SA-Canegro, OC decomp.
!       4.5.0.11 chp 06/08/2007 Mulch modeling is default, sweet corn complete
!       4.5.0.10 chp 05/26/2007 Revised Canegro-SA, added sweet corn (partial)
!       4.5.0.9  chp 05/09/2007 Added fresh weight option
!       4.5.0.8  chp 03/20/2007 Changes at IFDC 
!       4.5.0.7  chp 02/06/2007 Rename pre-release model to v4.5 (was 4.6) 

!       4.0.2.0  chp 08/11/2005 Release
!       4.0.1.0  chp 01/28/2004 Release Version 

!=======================================================================

!     Global constants
      INTEGER, PARAMETER :: 
     &    NL       = 20,  !Maximum number of soil layers 
     &    TS       = 24,  !Number of hourly time steps per day
     &    NAPPL    = 300, !Maximum number of applications or operations
     &    NCOHORTS = 300, !Maximum number of cohorts
     &    NELEM    = 3,   !Number of elements modeled (currently N & P)
!            Note: set NELEM to 3 for now so Century arrays will match
     &    NumOfDays = 1000, !Maximum days in sugarcane run (FSR)
     &    NumOfStalks = 42, !Maximum stalks per sugarcane stubble (FSR)
     &    EvaluateNum = 40, !Number of evaluation variables
     &    MaxFiles = 100    !Maximum number of output files

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
        CHARACTER (len=8)  MODEL
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
        CHARACTER (len=1) IFERI, IRESI, ICO2
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
        REAL CLOUDS, CO2, DAYL, PAR, RAIN, RHUM, SNDN, SNUP, SRAD, 
     &    TAMP, TA, TAV, TAVG, TDAY, TDEW, TGROAV, TGRODY,      
     &    TMAX, TMIN, TWILEN, WINDSP

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
        REAL ALES, DMOD, SLPF         !DMOD was SLNF
        REAL CMSALB, MSALB, SWALB, SALB      !Albedo 
        REAL, DIMENSION(NL) :: BD, CEC, CLAY, DLAYR, DS, DUL
        REAL, DIMENSION(NL) :: KG2PPM, LL, OC, PH, PHKCL
        REAL, DIMENSION(NL) :: SAND, SAT, SILT, STONES, SWCN

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

      SELECT CASE (OPSYS)
      CASE ('WINDO','DOS  ')
!       DOS, Windows
        SLASH = '\' 
        DSSATPRO = 'DSSATPRO.V45'
        STDPATH = 'C:\DSSAT45\'

      CASE ('LINUX','UNIX ')
!       Linux, Unix
        SLASH = '/' 
        DSSATPRO = 'DSSATPRO.L45'
        STDPATH = './DSSAT45/'
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
        REAL, DIMENSION(NL) :: UH2O       !Root water uptake
      End Type SPAMType

!     Data transferred from CROPGRO routine 
      TYPE PlantType
        REAL CANHT, CANWH, DXR57, EXCESS,
     &    PLTPOP, RNITP, SLAAD, XPOD
        INTEGER NR5
      END TYPE PlantType

!     Data transferred from management routine 
      Type MgmtType
        REAL DEPIR, EFFIRR, FERNIT, IRRAMT, TOTIR
      End Type MgmtType

!     Data transferred from Soil water routine
      Type WatType
        REAL DRAIN, RUNOFF
      End Type WatType

!     Data transferred from Soil Inorganic Nitrogen routine
      Type NiType
        REAL TNOXD, TLCHD
      End Type NiType

!     Data transferred from Organic C routines
      Type OrgCType
        REAL TOMINFOM, TOMINSOM, TOMINSOM1, TOMINSOM2
        REAL TOMINSOM3, TNIMBSOM
      End Type OrgCType

!     Data from weather
      Type WeathType
        Character*8 WSTAT
      End Type WeathType

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
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('PLANT')
        SELECT CASE (VarName)
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
        Case ('DEPIR');  Value = SAVE_data % MGMT % DEPIR
        Case ('IRRAMT'); Value = SAVE_data % MGMT % IRRAMT
        Case ('FERNIT'); Value = SAVE_data % MGMT % FERNIT
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('WATER')
        SELECT CASE (VarName)
        Case ('DRAIN'); Value = SAVE_data % WATER % DRAIN
        Case ('RUNOFF');Value = SAVE_data % WATER % RUNOFF
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
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('PLANT')
        SELECT CASE (VarName)
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
        Case ('DEPIR');  SAVE_data % MGMT % DEPIR  = Value
        Case ('IRRAMT'); SAVE_data % MGMT % IRRAMT = Value
        Case ('FERNIT'); SAVE_data % MGMT % FERNIT = Value
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('WATER')
        SELECT CASE (VarName)
        Case ('DRAIN'); SAVE_data % WATER % DRAIN  = Value
        Case ('RUNOFF');SAVE_data % WATER % RUNOFF = Value
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
        Case ('TOMINFOM'); SAVE_data % ORGC % TOMINFOM  = Value
        Case ('TOMINSOM'); SAVE_data % ORGC % TOMINSOM  = Value
        Case ('TOMINSOM1');SAVE_data % ORGC % TOMINSOM1 = Value
        Case ('TOMINSOM2');SAVE_data % ORGC % TOMINSOM2 = Value
        Case ('TOMINSOM3');SAVE_data % ORGC % TOMINSOM3 = Value
        Case ('TNIMBSOM'); SAVE_data % ORGC % TNIMBSOM  = Value
        Case DEFAULT; ERR = .TRUE.
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
