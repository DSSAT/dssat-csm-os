c     -----------------------------------------------------
c     -----------------------------------------------------

c     SUBROUTINE: SC_OPGROW()
c     ----------------------
c     This subroutine handles the output of growth
c     aspects (yields, roots, etc)

c     Matthew Jones, Feb 2008:
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     1. I have made a number of changes to this routine.
c     Output is now controlled by variable (runtime)
c     format strings.  Set column width with the 
c     VAR_WIDTH variable.  The code will automagically
c     adjust formatting (do not let VAR_WIDTH go below
c     4, otherwise this will clash with the floating
c     point decimals).  This might not work so well in 
c     a different compiler, because there is more 
c     whitespace in the format strings than strictly
c     necessary (or, um, correct).  It works with the
c     CVF compiler; if this does not work with Intel,
c     modify the code to output the format string, then
c     copy and paste this from the output window into the
c     code.  Then clean this up and use it as a hard-
c     coded format string.
c
c     2. I have also included a routine for right-aligning
c     text strings so that it still works with GBuild
c     
c     3. I have changed many of the ouput variables from
c     DSSAT-standard kg/ha measures to sugarcane-
c     conventional t/ha values.  I have updated the 
c     list of data codes in DATA.CDE with new variables
c     for these measures.
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SC_OPGROW (CONTROL, CaneCrop, Growth,
     - Part, Out, WaterBal, SoilProp,
     - YRPLT, CELLSE_DM)
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::

!     2023-01-26 chp removed unused variables from argument list: SW, 


c     Define DSSAT composite variables:
c     [Taken from MZ_CERES.for]
      USE ModuleDefs
      USE ModuleData

c     Define CANEGRO composite variables:
      USE CNG_ModuleDefs

c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c     No implicit typing - yuck!
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, YR_DOY, TIMDIF
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c     Maintain the value of internal variables between 
c     calls to this subroutine (supports the modular 
c     control structure)
      SAVE
c     DSSAT composite variables:
c     [Taken from MZ_CERES.for]
c     ::::::::::::::::::::::::::
      TYPE (ControlType) CONTROL
      TYPE (SoilType)    SOILPROP !chp not actually used yet
!      TYPE (SwitchType)  ISWITCH  !chp not actually used yet
!      Type (ResidueType) HARVRES  !chp not actually used yet
!      Type (ResidueType) SENESCE  !chp not actually used yet

c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c     DSSAT inputs:
c     :::::::::::::
c     The file unit number for the growth output file.
      INTEGER NOUTDG
c     local run number
      INTEGER RUN
c     local ?
      CHARACTER*20 FILEIO

c     Soil water content
!     REAL SW(NL)

c     CANEGRO variables:
c     ::::::::::::::::::
c     The CaneCrop 'object', an input parameter
c     :::::::::::::::::::::::::::::::::::::::::
      TYPE (CaneCropType) CaneCrop
c     The growth 'object'
      TYPE (GrothType)    Growth
c     The Partitioning object
      TYPE (PartType) Part
c     The output object:
      TYPE (OutType) Out
c     Water balance object:
      TYPE (WaterType) WaterBal

c     General inputs:
c     :::::::::::::::
c     General variables:
c     :::::::::::::::::::
c     local copy of CONTROL%DYNAMIC:
      INTEGER DYNAMIC
c     Filename
      CHARACTER*20 OFILE
c     Does the file exist, is this the first run?:
      LOGICAL FILE_EXISTS !, FIRST
c     Error status:
      INTEGER ERRNUM

c     Date/planting variables (local):
c     ::::::::::::::::::::::::::::::::
      INTEGER DAP, DAS, FROP, YRDOY, YRPLT, YEAR, DOY
      INTEGER TIMDIF  !, YRSIM
!      INTEGER MDATE, 
c     Intermediate output variables (to accom.
c     different units, etc)
c     ::::::::::::::::::::::
c     Stalk population per m2
      REAL STKPOPm2 

c     Cellulosic DM (t/ha)
      REAL, INTENT(IN) ::  CELLSE_DM

c     CANEGRO 3.5 variables:
c     ::::::::::::::::::::::
c     GROHEAD - this is an array variable that stores the 
c     text headings in the plantgro.out file.
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     MJ, Feb 2008: Grohead was previously an array of four long
c     strings.  However, this was a pain to maintain, so it is
c     now a 2-dimensional array with each column header stored
c     separately.
      CHARACTER*15 GROHEAD(4, 50)
      CHARACTER*15 GRO_OUT(50)


c     Loop counter variables
      INTEGER I, J, NUM_OVARS

!     CHP added 08Jan10
!     Print only if output switch (IDETG) is 'Y'
      Type (SwitchType) ISWITCH
      CHARACTER*1 IDETG

c     OUTPUT variables (as they are in PlantGro.out)
c     ::::::::::::::::::::::::::::::::::::::::::::::
!      INTEGER GSTD
c     Leaf area index, DM, leafDM, stalkDM, sucroseDM, rootDM, crop DM  
c     The DM values are in kg/ha
      REAL LAID, LGDMD,  SMDMD, SUCMD,  RWAD, RDMD, BADMD
c     Stalk fresh mass, t/ha
      REAL SMFMD
c     Number of leaf tips, number of green leaves:
      REAL T_SD, L_SD
c     Harvest index
      REAL SUID
c     Trash
      REAL LDDMD  
c     Total (living + dead) LAI
      REAL LAITD  
c     Water stress
      REAL WSPD
      REAL WSGD
c     Specific leaf area
      REAL SLAD  
c     Canopy height
      REAL CHTD
c     Root depth
      REAL  RDPD
c     Potential soil evaporation
      REAL EOSA
c     Total root length density (weighted by layer thickness)
      REAL RTLD
c     Fractional light interception
      REAL LIPD
c     Water stress in the top 30 cm of soil, affecting tiller popn
      REAL SWDF30
c     Photosynthesis
      REAL GROSSP, BRDMD, PARCE
c     Thermal time / heat units for emergence, stalk population and
c     Leaf extension respectively (deg C[ > Tbase].day)
      REAL TTEBC, TTSPC, TTLEC
c     Sucrose content (%) of dry and fresh mass respectively
      REAL SUDMD, SUFMD
!----------------------------------------------------------------------
c             DATA (for headings) - peviously from DSSAT 3.5, modified
c             to be like DSSAT 4, and then changed to individual
c             variable approach
!----------------------------------------------------------------------
c     Length of variable name string (excluding leading and 
c     trailing whitespace)
      INTEGER VLEN

c     How many spaces need to be skipped?
      INTEGER SKIP

c     How wide should the columns be spaced?
      INTEGER VAR_WIDTH

c     String equivalents of VLEN and SKIP
      CHARACTER*10 SKIP_STR, VLEN_STR, WIDTH_STR

c     Runtime format statement:
      CHARACTER*1024 FMT_STR, T_FMT_STR

c     General format statments for outputting heading comments
c     and daily values
      CHARACTER*100 G_FMT_STR, D_FMT_STR

c     String lengths:
      INTEGER TLEN, FLEN, ENDI

c     SUGARCANE-specific output variables:
c     (See C:\DSSAT45\DATA.CDE)
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     @CDE   LABEL           DESCRIPTION............................................  SYNONYMS
c     EOSA   Pot. soil evap. Average potential soil evaporation per day  (mm/day)     .
c     SMFMD  Stalk fresh massStalk (millable) fresh mass (t/ha)                                 .
c     WSTD   Tiller stress   Water stress affecting tiller population                 .
c     SHTD   Stalk height m  Stalk height (m)                                         .
c     RTLD   Total RLV (cm2) Total root length per cm2 (cm/cm2)                       .
c     LI%D   % intercpt      Canopy light interception (%)                            .
c     PGRD   Gross photos.   Gross photosynthesis rate (t/ha/day)                     .
c     BRDMD  Bioms increase  Biomass accumulation rate (kg/ha/day)                     .
c     LDGFD  Lodging         Extent of lodging                                        .
c     ! New codes, MJ Feb 2008 (mainly t/ha measures as conv. for sugarcane)
c     LGDMD  Green tops DM   Green leaf canopy + meristem dry mass(t/ha)['green tops'].
c     SMDMD  Stalk DM t/ha   Stalk (millable) dry mass, t/ha)                         .
c     SUCMD  Sucrose DM t/ha Sucrose dry mass (t/ha)                                  .
c     LDDMD  Trash DM  t/ha  Trash (residue) dry mass (t/ha)                          .
c     BADMD  Aerial DM t/ha  Aerial dry biomass (t/ha)                                .
c     LAIGD  Green LAI       Green leaf area index (m2/m2)                            .
c     RDMD   Root DM t/ha    Root dry mass (t/ha)                                     .
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


c      Headings for output variables:
c      ::::::::::::::::::::::::::::::
c      Date:
           DATA GROHEAD(1, 1) /'!DATE'/
           DATA GROHEAD(2, 1) /'! '/
           DATA GROHEAD(3, 1) /'!YEAR'/
           DATA GROHEAD(4, 1) /'@YEAR'/

c      Day of year:
           DATA GROHEAD(1, 2) /'Day '/
           DATA GROHEAD(2, 2) /'of'/
           DATA GROHEAD(3, 2) /'year'/
           DATA GROHEAD(4, 2) /'DOY'/

c      Days after 'simulation start' (I think)
           DATA GROHEAD(1, 3) /'Days'/
           DATA GROHEAD(2, 3) /'after'/
           DATA GROHEAD(3, 3) /'sim.start'/
           DATA GROHEAD(4, 3) /'DAS'/

c      Days after 'planting' (/harvest?)
           DATA GROHEAD(1, 4) /'Days'/
           DATA GROHEAD(2, 4) /'after'/
           DATA GROHEAD(3, 4) /'plant'/
           DATA GROHEAD(4, 4) /'DAP'/

c      Number of leaf tips 
           DATA GROHEAD(1, 5) /'Leaf-'/
           DATA GROHEAD(2, 5) /'tip'/
           DATA GROHEAD(3, 5) /'number'/
           DATA GROHEAD(4, 5) /'T#SD'/

c      Growth stage
           DATA GROHEAD(1, 6) /'Growth'/
           DATA GROHEAD(2, 6) /'stage'/
           DATA GROHEAD(3, 6) /'number'/
           DATA GROHEAD(4, 6) /'GSTD'/

c      Leaf area index
           DATA GROHEAD(1, 7) /'Leaf'/
           DATA GROHEAD(2, 7) /'area'/
           DATA GROHEAD(3, 7) /'index'/
           DATA GROHEAD(4, 7) /'LAIGD'/

c      Dry mass of green leaves + meristem
           DATA GROHEAD(1, 8) /'Green'/
           DATA GROHEAD(2, 8) /'leaf'/
           DATA GROHEAD(3, 8) /'DM (t/ha)'/
c          * Note: define a new Canegro-specific variable here??? *
           DATA GROHEAD(4, 8) /'LGDMD'/

c      Stalk fresh mass
           DATA GROHEAD(1, 9) /'Stalk'/
           DATA GROHEAD(2, 9) /'fresh'/
           DATA GROHEAD(3, 9) /'mass (t/ha)'/
           DATA GROHEAD(4, 9) /'SMFMD'/

c      Stalk dry mass
           DATA GROHEAD(1, 10) /'Stalk'/
           DATA GROHEAD(2, 10) /'dry'/
           DATA GROHEAD(3, 10) /'mass (t/ha)'/
           DATA GROHEAD(4, 10) /'SMDMD'/

c      Stalk dry mass
           DATA GROHEAD(1, 11) /'Sucrose'/
           DATA GROHEAD(2, 11) /'mass'/
           DATA GROHEAD(3, 11) /'(t/ha)'/
           DATA GROHEAD(4, 11) /'SUCMD'/

c      Root dry mass
          DATA GROHEAD(1, 12) /'Root'/
          DATA GROHEAD(2, 12) /'Dry mass'/
          DATA GROHEAD(3, 12) /'t/ha'/
          DATA GROHEAD(4, 12) /'RDMD'/

c      Aerial (above-ground) biomass
          DATA GROHEAD(1, 13) /'Above-'/
          DATA GROHEAD(2, 13) /'ground'/
          DATA GROHEAD(3, 13) /'DM (t/ha)'/
          DATA GROHEAD(4, 13) /'BADMD'/

c     Stalk dry mass
          DATA GROHEAD(1, 14) /'Stalks'/
          DATA GROHEAD(2, 14) /'per'/
          DATA GROHEAD(3, 14) /'ha'/
          DATA GROHEAD(4, 14) /'T#AD'/

c     Green leaf number per tiller
          DATA GROHEAD(1, 15) /'Green'/
          DATA GROHEAD(2, 15) /'leaf'/
          DATA GROHEAD(3, 15) /'No.'/
          DATA GROHEAD(4, 15) /'L#SD'/

c     Harvest index (t sucrose / t above ground biomass)
          DATA GROHEAD(1, 16) /'Harvest'/
          DATA GROHEAD(2, 16) /'Index'/
          DATA GROHEAD(3, 16) /'HI (t/t)'/
          DATA GROHEAD(4, 16) /'SUID'/

c     Trash dry mass
          DATA GROHEAD(1, 17) /'Trash'/
          DATA GROHEAD(2, 17) /'(dead leaf)'/
          DATA GROHEAD(3, 17) /'DM (t/ha)'/
          DATA GROHEAD(4, 17) /'LDDMD'/

c     Total (living + dead) leaf area index
          DATA GROHEAD(1, 18) /'Lv+Dd'/
          DATA GROHEAD(2, 18) /'leaf area'/
          DATA GROHEAD(3, 18) /'index'/
          DATA GROHEAD(4, 18) /'LAITD'/

c     Water stress affecting photosynthesis
          DATA GROHEAD(1, 19) /'Phot.'/
          DATA GROHEAD(2, 19) /'water'/
          DATA GROHEAD(3, 19) /'Stress(0-1)'/
          DATA GROHEAD(4, 19) /'WSPD'/

c     Water stress affecting growth
          DATA GROHEAD(1, 20) /'Growth'/
          DATA GROHEAD(2, 20) /'Water'/
          DATA GROHEAD(3, 20) /'stress'/
          DATA GROHEAD(4, 20) /'WSGD'/

c     Water stress affecting tillering
          DATA GROHEAD(1, 21) /'WaterStress'/
          DATA GROHEAD(2, 21) /'Tillering'/
          DATA GROHEAD(3, 21) /'(0-1)'/
          DATA GROHEAD(4, 21) /'WSTD'/


c     Stalk height (metres)
          DATA GROHEAD(1, 22) /'Stalk'/
          DATA GROHEAD(2, 22) /'Hght'/
          DATA GROHEAD(3, 22) /'m'/
          DATA GROHEAD(4, 22) /'SHTD'/

c     Root depth (m)
          DATA GROHEAD(1, 23) /'Root'/
          DATA GROHEAD(2, 23) /'Depth'/
          DATA GROHEAD(3, 23) /'m'/
          DATA GROHEAD(4, 23) /'RDPD'/

c     Root length density (cm3/cm3)
c     Layer 1
          DATA GROHEAD(1, 24) /'Root_Len.'/
          DATA GROHEAD(2, 24) /'density(1)'/
          DATA GROHEAD(3, 24) /'cm3/cm3'/
          DATA GROHEAD(4, 24) /'RL1D'/

c     Root length density (cm3/cm3)
c     Layer 2
          DATA GROHEAD(1, 25) /'Root_Len.'/
          DATA GROHEAD(2, 25) /'density(2)'/
          DATA GROHEAD(3, 25) /'cm/cm3'/
          DATA GROHEAD(4, 25) /'RL2D'/
c     Root length density (cm3/cm3)
c     Layer 3
          DATA GROHEAD(1, 26) /'Root_Len.'/
          DATA GROHEAD(2, 26) /'density(3)'/
          DATA GROHEAD(3, 26) /'cm/cm3'/
          DATA GROHEAD(4, 26) /'RL3D'/
c     Root length density (cm3/cm3)
c     Layer 4
          DATA GROHEAD(1, 27) /'Root_Len.'/
          DATA GROHEAD(2, 27) /'density(4)'/
          DATA GROHEAD(3, 27) /'cm/cm3'/
          DATA GROHEAD(4, 27) /'RL4D'/
c     Root length density (cm3/cm3)
c     Layer 5
          DATA GROHEAD(1, 28) /'Root_Len.'/
          DATA GROHEAD(2, 28) /'density(5)'/
          DATA GROHEAD(3, 28) /'cm/cm3'/
          DATA GROHEAD(4, 28) /'RL5D'/
c     Root length density (cm3/cm3)
c     Layer 6
          DATA GROHEAD(1, 29) /'Root_Len.'/
          DATA GROHEAD(2, 29) /'density(6)'/
          DATA GROHEAD(3, 29) /'cm/cm3'/
          DATA GROHEAD(4, 29) /'RL6D'/
c     Root length density (cm3/cm3)
c     Layer 7
          DATA GROHEAD(1, 30) /'Root_Len.'/
          DATA GROHEAD(2, 30) /'density(7)'/
          DATA GROHEAD(3, 30) /'cm/cm3'/
          DATA GROHEAD(4, 30) /'RL7D'/
c     Root length density (cm3/cm3)
c     Layer 8
          DATA GROHEAD(1, 31) /'Root_Len.'/
          DATA GROHEAD(2, 31) /'density(8)'/
          DATA GROHEAD(3, 31) /'cm/cm3'/
          DATA GROHEAD(4, 31) /'RL8D'/
c     Root length density (cm3/cm3)
c     Layer 9
          DATA GROHEAD(1, 32) /'Root_Len.'/
          DATA GROHEAD(2, 32) /'density(9)'/
          DATA GROHEAD(3, 32) /'cm/cm3'/
          DATA GROHEAD(4, 32) /'RL9D'/
c     Root length density (cm3/cm3)
c     Layer 10
          DATA GROHEAD(1, 33) /'Root_Len.'/
          DATA GROHEAD(2, 33) /'density(10)'/
          DATA GROHEAD(3, 33) /'cm/cm3'/
          DATA GROHEAD(4, 33) /'RL10D'/

c     Potential soil evaporation (mm/day)
c     This is in here because ET.OUT does not output it...
          DATA GROHEAD(1, 34) /'Pot.'/
          DATA GROHEAD(2, 34) /'soil'/
          DATA GROHEAD(3, 34) /'evap(mm)'/
          DATA GROHEAD(4, 34) /'EOSA'/

c     Total root length density
c     This is a fairly meaningless variable that
c     assisted with model comparison
          DATA GROHEAD(1, 35) /'Total'/
          DATA GROHEAD(2, 35) /'Root'/
          DATA GROHEAD(3, 35) /'Length'/
          DATA GROHEAD(4, 35) /'RTLD'/

c     Percent light interception
c     0-100%
          DATA GROHEAD(1, 36) /'Percent'/
          DATA GROHEAD(2, 36) /'light'/
          DATA GROHEAD(3, 36) /'int.'/
          DATA GROHEAD(4, 36) /'LI%D'/

c     Specific leaf area
          DATA GROHEAD(1, 37) /'Spec'/
          DATA GROHEAD(2, 37) /'Leaf'/
          DATA GROHEAD(3, 37) /'Area'/
          DATA GROHEAD(4, 37) /'SLAD'/

c     Gross photosynthetic rate
c     t/ha/day
          DATA GROHEAD(1, 38) /'Gross'/
          DATA GROHEAD(2, 38) /'photos'/
          DATA GROHEAD(3, 38) /'rate(t/ha/d)'/
          DATA GROHEAD(4, 38) /'PGRD'/

c     Biomass accumulation rate (change per day)
c     t/ha/day
          DATA GROHEAD(1, 39) /'Biomass'/
          DATA GROHEAD(2, 39) /'accum.'/
          DATA GROHEAD(3, 39) /'(t/ha/d)'/
          DATA GROHEAD(4, 39) /'BRDMD'/

c     Severity of lodging
          DATA GROHEAD(1, 40) /'lodging'/
          DATA GROHEAD(2, 40) /'severity'/
          DATA GROHEAD(3, 40) /'(0-1)'/
          DATA GROHEAD(4, 40) /'LDGFD'/

c     Thermal time (emergence)
          DATA GROHEAD(1, 41) /'Heat units'/
          DATA GROHEAD(2, 41) /'TBase Emerg'/
          DATA GROHEAD(3, 41) /'deg C.day'/
          DATA GROHEAD(4, 41) /'TTEBC'/

c     Thermal time (tiller population)
          DATA GROHEAD(1, 42) /'Heat units'/
          DATA GROHEAD(2, 42) /'TBase Popn'/
          DATA GROHEAD(3, 42) /'deg C.day'/
          DATA GROHEAD(4, 42) /'TTSPC'/

c     Thermal time (Leaf extension)
          DATA GROHEAD(1, 43) /'Heat units'/
          DATA GROHEAD(2, 43) /'TbaseLeafEx'/
          DATA GROHEAD(3, 43) /'deg C.day'/
          DATA GROHEAD(4, 43) /'TTLEC'/

c     Sucrose % of dry stalk mass
          DATA GROHEAD(1, 44) /'Sucrose %'/
          DATA GROHEAD(2, 44) /'dry mass'/
          DATA GROHEAD(3, 44) /'t/t * 100'/
          DATA GROHEAD(4, 44) /'SU%DMD'/

c     Sucrose % of wet/fresh stalk mass
          DATA GROHEAD(1, 45) /'Sucrose %'/
          DATA GROHEAD(2, 45) /'wet mass'/
          DATA GROHEAD(3, 45) /'t/t * 100'/
          DATA GROHEAD(4, 45) /'SU%FMD'/

c     Water stress (CWSI)
          DATA GROHEAD(1, 46) /'CWSI'/
          DATA GROHEAD(2, 46) /'water'/
          DATA GROHEAD(3, 46) /'stress'/
          DATA GROHEAD(4, 46) /'CWSI'/

c     Respiration rate
          DATA GROHEAD(1, 47) /'PARCE'/
          DATA GROHEAD(2, 47) /'RUE'/
          DATA GROHEAD(3, 47) /'Rate'/
          DATA GROHEAD(4, 47) /'PARCE'/

c     Growth respiration rate
          DATA GROHEAD(1, 48) /'G_RESP'/
          DATA GROHEAD(2, 48) /'Growth '/
          DATA GROHEAD(3, 48) /'Resp.'/
          DATA GROHEAD(4, 48) /'G_RESP'/

c     Maintenance respiration rate
          DATA GROHEAD(1, 49) /'M_RESP'/
          DATA GROHEAD(2, 49) /'Resp.'/
          DATA GROHEAD(3, 49) /'Maint.'/
          DATA GROHEAD(4, 49) /'M_RESP'/

c     Maintenance respiration rate
          DATA GROHEAD(1, 50) /'Cell-'/
          DATA GROHEAD(2, 50) /'ulosic'/
          DATA GROHEAD(3, 50) /'DM yld'/
          DATA GROHEAD(4, 50) /'CMDMD'/


c          ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c          Number of output variables (important for output!)
           DATA NUM_OVARS /50/

c          Width of output columns:
           DATA VAR_WIDTH /12/
c         :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c     ~~~~~~~~ SUBROUTINE CODE  ~~~~~~~~~~~~~~~~~~~~~~~~~~~
c     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c
c     Initialisation for DYNAMIC = all
c     ::::::::::::::::::::::::::::::::
c     Init DYNAMIC:
      DYNAMIC = CONTROL%DYNAMIC
c     Init RUN
      RUN     = CONTROL%RUN
c     Fileio
      FILEIO  = CONTROL%FILEIO

c     Alert that sugarcane model is running
c      WRITE(*,*) 'Called the sugarcane output module'


c     -----------------------------------------------------
c
c              DYNAMIC = RUNINIT
c
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF (DYNAMIC.EQ.RUNINIT) THEN


c     END of RUNINIT
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c
c     -----------------------------------------------------
c
c              DYNAMIC = SEASINIT
c
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
      ELSEIF (DYNAMIC.EQ.SEASINIT) THEN

          CALL GET(ISWITCH)
          IDETG = ISWITCH % IDETG
          IF (IDETG .NE. 'Y') RETURN

c     Open growth aspects output file:
c     ::::::::::::::::::::::::::::::::
c         Set file name:
          OFILE = 'PlantGro.OUT'

c         Get file unit number:
          CALL GETLUN('OUTG', NOUTDG)

c         Check that the file exists:
          FILE_EXISTS = .FALSE.
          INQUIRE(FILE=OFILE, EXIST=FILE_EXISTS)

c         Open the file
          IF (FILE_EXISTS) THEN
c             In append mode if the file already exists
              OPEN (UNIT=NOUTDG, FILE=OFILE, STATUS='OLD',
     &        IOSTAT=ERRNUM, POSITION='APPEND')
          ELSE
c             A new file if not existing
              OPEN (UNIT=NOUTDG, FILE=OFILE, STATUS='NEW',
     &        IOSTAT = ERRNUM)
              WRITE(NOUTDG,'("*GROWTH ASPECTS OUTPUT FILE")')
          ENDIF

c     Output a header (treatment / run info, etc)
c     ::::::::::::::::::::::::
c         Use the predefined one:
          CALL HEADER(SEASINIT, NOUTDG, RUN)

c     Now write the variable names:
c     :::::::::::::::::::::::::::::
c         (taken from DSSAT CANEGRO 3.5)
c         ::::::::::::::::::::::::::::::::::::::::::::
c         MJ, Feb 2008: changed from simply outputting the 
c         4 array elements (long strings) of GROHEAD to the
c         'finer-grained' one element per column and row
c         GROHEAD:
c         old:
c          WRITE (NOUTDG,2190) GROHEAD(1)
c          WRITE (NOUTDG,2190) GROHEAD(2)
c          WRITE (NOUTDG,2190) GROHEAD(3)
c          WRITE (NOUTDG,2190) GROHEAD(4)
c 2190     FORMAT (A)

c         new:
c              DO J=1,NUM_OVARS    
c                  GROHEAD(5, J) = '!--------------'  
c              ENDDO

c         Now, there is a problem.  GBuild expects column labels
c         to be right-aligned. Fortran, so far as I can find out,
c         only supports left-aligned text, BUT always right-
c         aligns Fx.x floating point numbers.  I do not know
c         why.  I have to then create a runtime format string
c         to correctly right-align the text in the headings:
          FMT_STR = '(A5, 1X, A3, '

          DO J=3, NUM_OVARS
c             Get length of variable name
              VLEN = LEN_TRIM(GROHEAD(4, J))
c             How many spaces need to be skipped?
              SKIP = VAR_WIDTH - VLEN
c             Create strings of these:
              WRITE(SKIP_STR, '(I3)') SKIP
              WRITE(VLEN_STR, '(I3)') VLEN
c              WRITE(*, '(A)') SKIP_STR, VLEN_STR

c             Add to format statement:
              IF (J .EQ. NUM_OVARS) THEN
c                 If this is format info for the last variable:
                  T_FMT_STR = TRIM(SKIP_STR) // 'X,'
     &                      // 'A' // TRIM(VLEN_STR)
     &                      // ')'
              ELSE
c                 For any variable
                  T_FMT_STR = TRIM(SKIP_STR) // 'X,'
     &                      // 'A' // TRIM(VLEN_STR)
     &                      // ','
              ENDIF

              TLEN = LEN_TRIM(T_FMT_STR) + 1
              FLEN = LEN_TRIM(FMT_STR) + 1
              ENDI = FLEN + TLEN - 1

c              WRITE(*, '(A)') T_FMT_STR
c              WRITE(*, '(3(I3, 2H, ))') TLEN, FLEN, ENDI
              WRITE(FMT_STR(FLEN:ENDI), '(A)') T_FMT_STR(1:TLEN)
              
          ENDDO

c         Proudly output the format string
c              WRITE(*, '(A)') FMT_STR

c         Format string for general headings:
          WRITE(WIDTH_STR, '(I3)') VAR_WIDTH-1
          G_FMT_STR = '(A5,1X,A3,1X,50(1H|, A' 
     &                 // TRIM(WIDTH_STR) // '))'


c         Loop through each row of GROHEAD, outputting
c         each column value per row
          DO I=1, 4
              DO J=1,NUM_OVARS    
                  GRO_OUT(J) = GROHEAD(I, J)  
              ENDDO
               IF (I .LT. 4) THEN
c                 Write any old column headings
c                  WRITE (NOUTDG,'(A5,1X,A3,1X,4(A5,1X), 44(1X,A12))') 
c     &                       GRO_OUT(1:NUM_OVARS)
c                  WRITE (NOUTDG,'(A5,1X,A3,1X,50(1H|, A11))') 
c     &                       GRO_OUT(1:NUM_OVARS)
                  WRITE (NOUTDG,FMT=G_FMT_STR) 
     &                       GRO_OUT(1:NUM_OVARS)
               ELSE
c                   Write right-aligned column headings
                    WRITE (NOUTDG,FMT=FMT_STR) 
     &                       GRO_OUT(1:NUM_OVARS)
               ENDIF 
          ENDDO


c         Output format string for daily values
          D_FMT_STR = '(1X, I4, 1X, I3, 4(1X, I' 
     &                // TRIM(WIDTH_STR) // 
     &                '), 95(1X, F' // 
     &                TRIM(WIDTH_STR) // '.3))'

c          WRITE(*, '(A)') D_FMT_STR

c         ::::::::::::::::::::::::::::::::::::::::::::

 
c     Initialise some date variables:
c     :::::::::::::::::::::::::::::::
      FROP   = CONTROL%FROP
!      YRSIM  = CONTROL%YRSIM


c     END of SEASINIT
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c
c     -----------------------------------------------------
c
c              DYNAMIC = RATE
c
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
      ELSEIF(DYNAMIC.EQ.RATE) THEN
c     END of RATE
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c
c     -----------------------------------------------------
c
c              DYNAMIC = INTEGRATE
c
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
      ELSEIF(DYNAMIC.EQ.INTEGR) THEN

c     END of INTEGRATE
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c
c     -----------------------------------------------------
c
c              DYNAMIC = OUTPUT
c
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
      ELSEIF(DYNAMIC.EQ.OUTPUT) THEN
      
          IF (IDETG .NE. 'Y') RETURN

c     Print daily output:
c     :::::::::::::::::::
      YRDOY  = CONTROL%YRDOY

      IF (YRDOY .GE. YRPLT) THEN
          DAP = MAX(0, TIMDIF(YRPLT, YRDOY))
!         DAS = MAX(0, TIMDIF(YRSIM, YRDOY))
          DAS = CONTROL % DAS

          !-------------------------------------------------------------
          !  Write output based on user specified frequency 
          !-------------------------------------------------------------
          IF ((MOD(DAS,FROP) .EQ. 0)      !Daily output every FROP days,
     &      .OR. (YRDOY .EQ. YRPLT)) THEN !on planting date

            CALL YR_DOY(YRDOY, YEAR, DOY)

            
            T_SD     = CaneCrop%LT
            LAID     = Growth%LAI
            LGDMD    = Part%TOPDM
            SMDMD    = Part%STKDM
            SMFMD    = Part%STKWM
            SUCMD    = Part%SUCMAS  
            RWAD     = Out%ROOTDM * 1000.
            RDMD     = Out%ROOTDM
            BADMD    = Part%AERLDM
            STKPOPm2 = CaneCrop%TOTPOP / 10000.
            L_SD     = CaneCrop%TMEANLF - CaneCrop%TMEANDEDLF
c           Harvest index = product / biomass
            IF (Part%STKDM .GT. 0.) THEN
               SUID     = Part%SUCMAS / Part%STKDM
            ELSE 
               SUID     = 0.
            ENDIF
            LDDMD    = Out%TRASDM
            LAITD    = Growth%TLAI
            WSPD     = WaterBal%SWDF1
            WSGD     = WaterBal%SWDF2
            
c           Calculate sucrose %, on a dry and fresh mass basis
c           STKWM is a function of STKDM, so it is only
c           necessary to check one.
            IF (Part%STKDM .GT. 0.00001) THEN
              SUDMD    = Part%SUCMAS / Part%STKDM * 100.
              SUFMD    = Part%SUCMAS / Part%STKWM * 100.
            ELSE
              SUDMD = 0.
              SUFMD = 0.  
            ENDIF

c           Specific leaf area (cm2/gram)
c           [calculated from TOPS, i.e. including meristem]
c           [assuming LI is an indicator of field coverage]
c                     ((area (m2) * LI * number of leaves/area) / total leaf DM)
            SLAD     = 0.
            IF (LGDMD .GT. 0.) SLAD = ((10000. * Growth%LI * 
     -                                Growth%LAI) / LGDMD)
!           HBD May 2023:
!           - It seems that this equation above has been here since v4.5
!           - Test (1)
!           - Divided Growth%LI above in L826 to 100 to convert % to fraction
!           - Multiplied LGDMD above in L827 to 100 to convert t/ha to g/m2
!           - Values still does not seem ok.
!           - Test (2) SLAD calculation as below. Simply LAI/Green leaf DM
!           - It produces really high values that do not appear to be right...
!           -    in both calculation formats!
!           - At least outputs are being read in DSSAT R package now
!            IF (LGDMD .GT. 0.5) SLAD = (Growth%LAI*10000)/(LGDMD*100)

!           HBD May 2023: uncommented the division here to export in meters            
            CHTD     = CaneCrop%SHGT  / 100.
            
            
            RDPD     = WaterBal%RTDEP / 100.
            EOSA     = WaterBal%EOS
            
            LIPD     = Growth%LI * 100.
c           Total root length density
            RTLD = DOT_PRODUCT(SoilProp%DLAYR(1:SoilProp%NLAYR),
     -                         WaterBal%RLV(1:SoilProp%NLAYR))

            SWDF30 = WaterBal%SWDF30
            GROSSP = Out%GROSSP
            BRDMD  = Out%DWDT * 1000.
            PARCE  = Part%PARCE

c         Thermal time / heat units
            TTEBC= Out%CHU_EM
            TTSPC= Out%CHUPOP
            TTLEC= Out%CHUPI

 
c         MJ, Feb 2008:
c         Previously:
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c            WRITE(NOUTDG,400)YEAR, DOY,  DAS, DAP, T_SD, 0,
c     -                       LAID, LGDMD, SMFMD, SMDMD, SUCMD, RWAD, 
c     -                       BADMD, STKPOPm2, L_SD, SUID,
c     -                       NINT(LDDMD), LAITD, WSPD, WSGD, NSTD, LN_D,
c     -                       SH_D, SLAD, CHTD, CWID, NWAD, RDPD,
c     -                       WaterBal%RLV(1:10), EOSA, EOPA, TWUP, RTLD,
c     -                       LIPD, SWDF30, GROSSP, BRDMD, PARCE, Out%PAR,
c     -                       Part%FLODGE


c 400        FORMAT (1X, I4,1X, I3,2(1X, I5),1X, F5.0,1X, I5,1X, F5.3,1X,
c     -              5(F6.0,1X), F6.0,1X, F5.1, 1X, F5.2, 1X, F5.3, 
c     -              1X, I5,1X, F5.2,1X, F5.3, 9(1X, F5.3), 10(1X, F5.3),
c     -              1X, F5.2, 1X, F5.2, 1X, F5.2, 1X, F5.2, 1X, F5.2, 
c     -              1X, F5.2, 1X, F6.4, 1X, F6.4, 1X, F6.4, 1X, F6.2,
c     -              1X, F6.2)
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c         NOW:
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

!       chp 9/26/2008 changed stress output to (1.0 - stress)

          WRITE(NOUTDG,FMT=D_FMT_STR) 
     -                     YEAR, DOY,  DAS, DAP, NINT(T_SD), 
     -                     CaneCrop%GROPHASE,
     -                     LAID, LGDMD, SMFMD, SMDMD, SUCMD, RDMD, 
     -                     BADMD, STKPOPm2, L_SD,   SUID,
     -                     LDDMD, LAITD, 1.0-WSPD, 1.0-WSGD, 1.0-SWDF30,
     -                     CHTD, RDPD,
     -                     WaterBal%RLV(1:10), EOSA, RTLD,
     -                     LIPD, SLAD, GROSSP, BRDMD, 
     -                     Part%FLODGE, TTEBC, TTSPC, TTLEC, SUDMD,
     &              SUFMD, Canecrop%CWSI, PARCE, Part%RESP_G,
     &              Part%RESP_M, CELLSE_DM

c         Format statement
c         Original one
c         Dates, etc
c  500     FORMAT(I5,1X,I3,4(1X, I5),1X,
c         Up to 97 (total 100 vars) 12-character floating point nums
c     &           95(F12.3, 1X))

c         Latest one:
c  500     FORMAT(1X, I4, 1X, I3, 4(1X, I11),
c         Up to 95 (total 100 vars) 11-character floating point nums
c     &           95(1X, F11.3))


c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          ENDIF
      ENDIF
c     END of OUTPUT
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c
c     -----------------------------------------------------
c
c              DYNAMIC = FINAL 
c
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
      ELSEIF(DYNAMIC.EQ.SEASEND) THEN
          IF (IDETG .NE. 'Y') RETURN

c     Close the output file
          CLOSE(UNIT=NOUTDG)
c     END of FINAL
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::
c
c     -----------------------------------------------------
c     END of DYNAMIC conditional statement
c     ::::::::::::::::::::::::::::::::::::
      ENDIF
c     ::::::::::::::::::::::::::::::::::::

      END
c     -----------------------------------------------------
c     END of SUBROUTINE SC_OPGROW
c     -----------------------------------------------------