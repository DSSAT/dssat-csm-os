C=======================================================================
C  OPSUM, Subroutine G. Hoogenboom, J. W. Jones
C  Generates output for seasonal data.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1990 GH  Written
C  11/02/1999 CHP Changed TSOC to THUMC, TSIN to THUMN, AMTRES to CUMRES 
C  07/01/2000 GH  Eliminated common block statements
C  01/09/2002 CHP Added SumModule to store SUMMARY.OUT variables
C  05/08/2003 CHP Added version and date-time stamp.
C  08/12/2003 CHP Added I/O error checking
C  09/10/2003 CHP Changed Evaluate.out headers to have "S" (simulated) 
C                    and "M" (measured) suffixes rather than "P" (predicted) 
C                    and "O" (observed)
C  01/04/2005 CHP Changed screen output to HWAH instead of HWAM
C  02/04/2005 CHP Added new variables to Summary.out: WSTAT, SLNO, PWAM, LAIX,
C                   HIAM, EPCM, ESCM
!  08/12/2005 CHP Changed P variable headers
!  12/12/2005 CHP Add OCTAM, ONTAM, OPAM, and OPTAM variables
!  06/27/2007 CHP Add water productivity to Summary.OUT
!  06/17/2009 CHP Update weather station in SeasInit
!  01/07/2010 CHP Add irrigation water productivity to Overview and Summary
!  01/08/2010 CHP Separate switch for Evaluate, but not Overview (IDETO=E)
!  02/10/2010 CHP Added EDAT.
!  02/23/2011 CHP Added seasonal average environmental values
!  03/27/2012 CHP Fixed format bug for very large HWUM
!  07/19/2016 CHP Add cumulative N2O emissions in Nitrogen section
!  09/09/2016 CHP Add cumulative CO2 emissions from OC decomposition
!  06/20/2024  FO Added Economic Yield
C=======================================================================

      MODULE SumModule
      USE ModuleDefs
!     This module defines variables which are printed to SUMMARY.OUT file.

!     Data construct for summary.out data. Used only by SUMVAL and OPSUM.
      Type SummaryType
        INTEGER ADAT, EDAT, MDAT, DWAP, CWAM
        INTEGER HWAM
        INTEGER HNUMAM, NFXM, NUCM, CNAM, GNAM
        INTEGER IRNUM, IRCM, ETCM
        INTEGER PRCM, ROCM, DRCM, SWXM
        INTEGER NINUMM, NICM, NLCM, NIAM, RECM, ONAM, OCAM
        INTEGER PINUMM, PICM, PUPC, SPAM
        INTEGER KINUMM, KICM, KUPC, SKAM
        REAL HWAH, HWUM, BWAH, HNUMUM 

!       Added 2/6/2005 for v4.0.2.0
        REAL LAIX, HIAM
        INTEGER PWAM, EPCM, ESCM

!       Added 12/12/2005 Organic Matter
        INTEGER OCTAM, ONTAM, OPAM, OPTAM

!       Added 06/27/2007 Water productivity
        REAL DMPEM, DMPPM, DMPTM, YPEM, YPPM, YPTM
!       Added 01/07/2010 Irrigation productivity
        REAL DMPIM, YPIM
!       Added 01/27/2010 N productivity
        REAL DPNAM, DPNUM, YPNAM, YPNUM

!       Added 02/23/2011 Seasonal average environmental data
        INTEGER NDCH
        REAL TMINA, TMAXA, SRADA, DAYLA, CO2A, PRCP, ETCP, ESCP, EPCP
        
!       Added 7/19/2016 N2O emissions
        REAL N2OEM  !kg/ha
        REAL CO2EM
        REAL CH4EM  !kg[C]/ha chp 2021-07-28

!       Added 2019-19-17 CHP Cumulative net mineralization
        REAL NMINC
        
!       Added 05/28/2021 Latitude, Longitude and elevation
        REAL XCRD, YCRD, ELEV

!       Added 2021-04-14 CHP End of season crop status
        INTEGER CRST

!       Added 2021-20-04 LPM Fresh weight variables
        INTEGER FCWAM, FHWAM, FPWAM
        REAL HWAHF, FBWAH

!       Added 2024-06-20 FO Economic Yield
        REAL EYLDH

      End Type SummaryType

      Type EvaluateType
        INTEGER ICOUNT
        CHARACTER*6,  DIMENSION(EvaluateNum) :: OLAP
        CHARACTER*8,  DIMENSION(EvaluateNum) :: Simulated, Measured
        CHARACTER*50, DIMENSION(EvaluateNum) :: DESCRIP
      End Type EvaluateType

      Type (SummaryType) SUMDAT
      Type (EvaluateType) EvaluateData

      End Module SumModule
C=======================================================================


C=======================================================================
      SUBROUTINE OPSUM (CONTROL, ISWITCH, YRPLT) 

C-----------------------------------------------------------------------
      USE SumModule     
      USE ModuleDefs
      USE ModuleData
!     VSH
      USE CsvOutput
      USE Linklist
      IMPLICIT NONE
      EXTERNAL ERROR, FIND, TIMDIF, GETLUN, LENSTRING, PrintText,  
     &  PrintTxtNeg, CLEAR, ROUND
      SAVE

      CHARACTER*1  IDETL, IDETO, IDETS, RNMODE
      CHARACTER*2  CROP, CG
      CHARACTER*6  SECTION
      CHARACTER*6, PARAMETER :: ERRKEY = 'OPSUM '
      CHARACTER*8  EXPER, FLDNAM, MODEL, MODEL_LAST, CO2EM_TXT
      CHARACTER*12 OUTS, SEVAL, FMT
!     PARAMETER (OUTS = 'Summary.OUT')
      CHARACTER*25 TITLET
      CHARACTER*30 FILEIO
      CHARACTER*60 ENAME

      INTEGER ADAT, CNAM, CRPNO, CWAM, DNR1, DNR7, DRCM, DWAP, DYNAMIC
      INTEGER EDAT, ERRNUM, ETCM, FOUND, GNAM, HNUMAM, HWAM
      INTEGER I, IRCM, LUNIO, LINC, LNUM, MDAT, IRNUM, NINUMM
      INTEGER NFXM, NIAM, NICM, NLCM, NLINES, NMINC  !, NNAPHO
      INTEGER NOUTDS, NUCM, NYRS, PRCM, RECM, ROCM, ONAM, OCAM
      INTEGER REPNO  !CHP 3/15/2018
      INTEGER ROTNO, ROTOPT, RUN, SLUN, SWXM, TIMDIF, TRTNUM, YRPLT
      INTEGER YRSIM, YRDOY
      INTEGER RUN2, SimLen, LenString
      INTEGER PINUMM, PICM, PUPC, SPAM    !P data
      INTEGER KINUMM, KICM, KUPC, SKAM    !K data

      REAL BWAH, HNUMUM, HWAH, HWUM   !, HBPC, HPC

!     Added 2/6/2005 for v4.0.2.0
      REAL LAIX, HIAM
      INTEGER PWAM, EPCM, ESCM
      CHARACTER* 8 WSTAT, WSTATION
      CHARACTER*10 SLNO

!     Added 12/12/2005 
      INTEGER OCTAM, ONTAM, OPAM, OPTAM

!     Added 06/27/2007 Water productivity
      REAL DMPEM, DMPPM, DMPTM, YPEM, YPPM, YPTM
!     Added 01/07/2010 Irrigation productivity
      REAL DMPIM, YPIM
!     Added 01/27/2010 N productivity
      REAL DPNAM, DPNUM, YPNAM, YPNUM
!     Added 02/23/2011 Seasonal average environmental data
      INTEGER NDCH
      REAL TMINA, TMAXA, SRADA, DAYLA, CO2A, PRCP, ETCP, ESCP, EPCP
      INTEGER CRST
      REAL CO2EM, N2OEM, CH4EM  !kg/ha
!     Added 05/28/2021 Latitude, Longitude and elevation data
      CHARACTER*9  ELEV 
      CHARACTER*15 LATI, LONG
      INTEGER HYEAR

!     2020-12-30 CHP added WYEAR - weather year corresponding to YRSIM date
!     For forecast mode may be different than simulation year
      INTEGER WYEAR
      
!     Added 2021-20-04 LPM Fresh weight variables
      INTEGER FCWAM, FHWAM, FPWAM
      REAL HWAHF, FBWAH
      
!     Added 2024-06-20 FO Economic Yield
      REAL EYLDH, ROUND
      INTEGER iEYLDH

      LOGICAL FEXIST

!     Text values for some variables that get overflow with "-99" values
!     CHARACTER*9 PRINT_TXT, PRINT_TXT_neg 
      CHARACTER*9 DMPPM_TXT, DMPEM_TXT, DMPTM_TXT, DMPIM_TXT
      CHARACTER*9 YPPM_TXT, YPEM_TXT, YPTM_TXT, YPIM_TXT
      CHARACTER*9 DPNAM_TXT, DPNUM_TXT, YPNAM_TXT, YPNUM_TXT
      CHARACTER*6 TMINA_TXT, TMAXA_TXT, SRADA_TXT, DAYLA_TXT
      CHARACTER*7 CO2A_TXT, PRCP_TXT, ETCP_TXT, ESCP_TXT, EPCP_TXT
      CHARACTER*6 N2OEC_TXT  !, N2OGC_TXT

!     Evaluate.OUT variables:
      INTEGER ICOUNT   !Number of observations for this crop
      CHARACTER*6,  DIMENSION(EvaluateNum) :: OLAP    !Labels
      CHARACTER*8,  DIMENSION(EvaluateNum) :: Measured, Simulated
      CHARACTER*50, DIMENSION(EvaluateNum) :: DESCRIP !Descriptions

!     Date and time stamp variables
      INTEGER       DATE_TIME(8)
!      CHARACTER*3   MonthTxt(12)
!      DATA MonthTxt /'Jan','Feb','Mar','Apr','May','Jun','Jul'
!     &         ,'Aug','Sep','Oct','Nov','Dec'/
      DATA MODEL_LAST /'        '/
!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

!     The variable "ISWITCH" is of type "SwitchType".
      TYPE (SwitchType) ISWITCH

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE
      REPNO   = CONTROL % REPNO
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM
      NYRS    = CONTROL % NYRS

      IDETS   = ISWITCH % IDETS
      IDETO   = ISWITCH % IDETO
      IDETL   = ISWITCH % IDETL
      FMOPT   = ISWITCH % FMOPT   ! VSH
C***********************************************************************
C***********************************************************************
C     Run initialization - run once per simulation
C***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
C-----------------------------------------------------------------------
      CALL DATE_AND_TIME (VALUES=DATE_TIME)
!      date_time(1)  The 4-digit year  
!      date_time(2)  The month of the year  
!      date_time(3)  The day of the month  
!      date_time(4)  The time difference with respect to Coordinated Universal Time (UTC) in minutes  
!      date_time(5)  The hour of the day (range 0 to 23) - local time  
!      date_time(6)  The minutes of the hour (range 0 to 59) - local time  
!      date_time(7)  The seconds of the minute (range 0 to 59) - local time  
!      date_time(8)  The milliseconds of the second (range 0 to 999) - local time  
      
      OPEN (UNIT=LUNIO, FILE = FILEIO, STATUS = 'OLD', 
     &  IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)
      LNUM = 0
C-----------------------------------------------------------------------
C      Find and Read Experimental Details Section
C-----------------------------------------------------------------------
      SECTION = '*EXP.D'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ(LUNIO,'(3X,A8,1X,A2,1X,A60)',IOSTAT=ERRNUM) EXPER,CG,ENAME
        LNUM = LNUM + 1
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
      ENDIF
C-----------------------------------------------------------------------
C       Find and Read TREATMENTS Section
C-----------------------------------------------------------------------
      SECTION = '*TREAT'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ (LUNIO,'(I3,I2,2(1X,I1),1X,A25)',IOSTAT=ERRNUM) 
     &      TRTNUM, ROTNO, ROTOPT, CRPNO, TITLET
        LNUM = LNUM + 1
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
      ENDIF

C-----------------------------------------------------------------------
C       Find and read Cultivar Section
C-----------------------------------------------------------------------
      SECTION = '*CULTI'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ(LUNIO,'(3X,A2)',IOSTAT=ERRNUM) CROP
        LNUM = LNUM + 1
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
      ENDIF

C-----------------------------------------------------------------------
C    Find and read Field Section
C-----------------------------------------------------------------------
      SECTION = '*FIELD'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ(LUNIO,'(3X,A8,1X,A8,49X,A10)',IOSTAT=ERRNUM) 
     &        FLDNAM, WSTATION, SLNO
        LNUM = LNUM + 1
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
      ENDIF

      CLOSE (LUNIO)

C     Get unit number for SUMMARY.OUT file
      CALL GETLUN('OUTS', NOUTDS)

!-----------------------------------------------------------------------
!     Get unit number for EVALUATE.OUT file
      SEVAL = 'Evaluate.OUT'
      CALL GETLUN('SEVAL', SLUN)

      EvaluateData % Simulated = '     -99'
      EvaluateData % Measured  = '     -99'

C***********************************************************************
C***********************************************************************
C     Seasonal initialization - run once per season
C***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
C     Initialize temporary file which will store variables needed by
C       OPSUM.  This file will be written to by various modules and
C       deleted upon closing.
C-----------------------------------------------------------------------
C     Initialize OPSUM variables.
      SUMDAT % EDAT   = -99
      SUMDAT % ADAT   = -99
      SUMDAT % MDAT   = -99
      SUMDAT % DWAP   = -99
      SUMDAT % CWAM   = -99
      SUMDAT % HWAM   = -99
      SUMDAT % HWAH   = -99.0
      SUMDAT % BWAH   = -9.9
      SUMDAT % PWAM   = -99
      SUMDAT % HWUM   = -99.0
      SUMDAT % HNUMAM = -99
      SUMDAT % HNUMUM = -99.0
      SUMDAT % HIAM   = -99.0
      SUMDAT % LAIX   = -99.0
      SUMDAT % IRNUM  = -99
      SUMDAT % IRCM   = -99
      SUMDAT % PRCM   = -99
      SUMDAT % ETCM   = -99
      SUMDAT % EPCM   = -99
      SUMDAT % ESCM   = -99
      SUMDAT % ROCM   = -99
      SUMDAT % DRCM   = -99
      SUMDAT % SWXM   = -99
      SUMDAT % NINUMM = -99
      SUMDAT % NICM   = -99
      SUMDAT % NFXM   = -99
      SUMDAT % NUCM   = -99
      SUMDAT % NLCM   = -99
      SUMDAT % NIAM   = -99
      SUMDAT % NMINC  = -99
      SUMDAT % CNAM   = -99
      SUMDAT % GNAM   = -99
      
!     N2O emissions
      SUMDAT % N2OEM  = -99. !N2O emissions (kg[N]/ha)
      SUMDAT % CO2EM  = -99  !CO2 emissions from OM decomp (kg[C]/ha)
      SUMDAT % CH4EM  = -99. !CH4 emissions (kg[C]/ha)
      
      SUMDAT % RECM   = -99
      SUMDAT % ONTAM  = -99
      SUMDAT % ONAM   = -99
      SUMDAT % OPTAM  = -99
      SUMDAT % OPAM   = -99
      SUMDAT % OCTAM  = -99
      SUMDAT % OCAM   = -99

!     P data - CHP added 8/12/2005
      SUMDAT % PINUMM = -99
      SUMDAT % PICM   = -99
      SUMDAT % PUPC   = -99
      SUMDAT % SPAM   = -99

!     K data - CHP added 04/14/2008
      SUMDAT % KINUMM = -99
      SUMDAT % KICM   = -99
      SUMDAT % KUPC   = -99
      SUMDAT % SKAM   = -99
      
!     Fresh weight values -LPM added 04/20/2021
      SUMDAT % FCWAM  = -99   !Fresh tops weight at maturity (kg/ha)
      SUMDAT % FHWAM  = -99   !Harvest prod fresh wt at maturity (kg/ha)
      SUMDAT % HWAHF  = -99.0 !Harvested yield (fresh weight) (kg/ha)
      SUMDAT % FBWAH  = -99.0 !By-prod fresh harvested (kg/ha)
      SUMDAT % FPWAM  = -99   !Fresh pod (ear) wt at maturity (kg/ha)
      SUMDAT % EYLDH  = -99.0 !Economic Yield

      SUMDAT % DMPPM  = -99.0 !Dry matter-rain productivity(kg[DM]/m3[P]
      SUMDAT % DMPEM  = -99.0 !Dry matter-ET productivity(kg[DM]/m3[ET]
      SUMDAT % DMPTM  = -99.0 !Dry matter-EP productivity(kg[DM]/m3[EP]
      SUMDAT % DMPIM  = -99.0 !Dry matter-irr productivity(kg[DM]/m3[I]
      SUMDAT % DPNAM  = -99.0 !Dry matter : N applied
      SUMDAT % DPNUM  = -99.0 !Dry matter : N uptake

      SUMDAT % YPPM   = -99.0 !Yield-rain productivity(kg[yield]/m3[P]
      SUMDAT % YPEM   = -99.0 !Yield-ET productivity(kg[yield]/m3[ET]
      SUMDAT % YPTM   = -99.0 !Yield-EP productivity(kg[yield]/m3[EP]
      SUMDAT % YPIM   = -99.0 !Yield-irr productivity(kg[yield]/m3[I]
      SUMDAT % YPNAM  = -99.0 !Yield : N applied
      SUMDAT % YPNUM  = -99.0 !Yield : N uptake

!     Average or cumulative environmental data, planting to harvest
      SUMDAT % NDCH   = -99   !Number days
      SUMDAT % TMINA  = -99.9 !Avg min daily temp (C) 
      SUMDAT % TMAXA  = -99.9 !Avg max daily temp (C) 
      SUMDAT % SRADA  = -99.9 !Avg solar rad (MJ/m2/d)
      SUMDAT % DAYLA  = -99.9 !Avg daylength (hr/d) 
      SUMDAT % CO2A   = -99.9 !Avg atm. CO2 (ppm) 
      SUMDAT % PRCP   = -99.9 !Cumul rainfall (mm), planting to harvest
      SUMDAT % ETCP   = -99.9 !Cumul ET (mm), planting to harvest
      SUMDAT % ESCP   = -99.9 !Cumul soil evap (mm), planting to harvest
      SUMDAT % EPCP   = -99.9 !Cumul transp (mm), planting to harvest

      SUMDAT % CRST   = -99   !End of season crop status code

      CALL GET('WEATHER','WSTA',WSTAT)
!      IF (LenString(WSTAT) < 1) THEN
!        WSTAT = WSTATION
!      ENDIF

!***********************************************************************
!***********************************************************************
!    Seasonal output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
      EDAT = SUMDAT % EDAT    !Emergence Date
      ADAT = SUMDAT % ADAT    !Anthesis Date
      MDAT = SUMDAT % MDAT    !Physiological Maturity Date
      
      DWAP = SUMDAT % DWAP    !Planting Material Weight (kg/ha)
      CWAM = SUMDAT % CWAM    !Tops Weight at Maturity (kg/ha)
      HWAM = SUMDAT % HWAM    !Yield at Maturity (kg/ha)
      HWAH = SUMDAT % HWAH    !Yield at Harvest (kg/ha)
      BWAH = SUMDAT % BWAH    !By-product (kg/ha)
      PWAM = SUMDAT % PWAM    !Pod weight at maturity (kg/ha)
      HWUM = SUMDAT % HWUM    !Unit Weight at Maturity (mg/unit)
      HNUMAM=SUMDAT % HNUMAM  !Number at Maturity (no./m2)
      HNUMUM=SUMDAT % HNUMUM  !Number at Maturity (no./unit)
      HIAM = SUMDAT % HIAM    !Harvest index
      LAIX = SUMDAT % LAIX    !Leaf area index (mm2/mm2)
      FCWAM= SUMDAT % FCWAM   !Fresh tops weight at maturity (kg/ha)
      FHWAM= SUMDAT % FHWAM   !Harvest prod fresh wt at maturity (kg/ha)
      HWAHF= SUMDAT % HWAHF   !Harvested yield (fresh weight) (kg/ha)
      FBWAH= SUMDAT % FBWAH   !By-prod harvested fresh wt (kg/ha)
      FPWAM= SUMDAT % FPWAM   !Fresh pod (ear) weight @ maturity (kg/ha)
      EYLDH= SUMDAT % EYLDH   !Economic Yield

      IRNUM= SUMDAT % IRNUM   !Irrigation Applications (no.)
      IRCM = SUMDAT % IRCM    !Season Irrigation (mm)
      PRCM = SUMDAT % PRCM    !Season Precipitation (mm)
      ETCM = SUMDAT % ETCM    !Season Evapo-transpiration (mm)
      EPCM = SUMDAT % EPCM    !Season Transpiration (mm)
      ESCM = SUMDAT % ESCM    !Season Soil Evaporation (mm)
      ROCM = SUMDAT % ROCM    !Season Surface Runoff (mm)
      DRCM = SUMDAT % DRCM    !Season Vertical Drainage (mm)
      SWXM = SUMDAT % SWXM    !Extractable Water at Maturity (mm)

      NINUMM=SUMDAT % NINUMM  !Nitrogen Applications (no.)
      NICM = SUMDAT % NICM    !Inorganic N applied (kg N/ha)
      NFXM = SUMDAT % NFXM    !N Fixed (kg/ha)
      NUCM = SUMDAT % NUCM    !N uptake (kg/ha)
      NLCM = SUMDAT % NLCM    !N leached (kg/ha)
      NIAM = SUMDAT % NIAM    !Inorganic N at maturity (kg N/ha)
      NMINC= SUMDAT % NMINC   !Net mineralized N (kg N/ha)
      CNAM = SUMDAT % CNAM    !Tops N at Maturity (kg/ha)
      GNAM = SUMDAT % GNAM    !Grain N at Maturity (kg/ha)
      N2OEM= SUMDAT % N2OEM   !N2O emissions (kg[N]/ha)
      CO2EM= SUMDAT % CO2EM   !CO2 emissions (kg[C]/ha)
      CH4EM= SUMDAT % CH4EM   !CH4 emissions (kg[C]/ha)

      RECM = SUMDAT % RECM    !Residue Applied (kg/ha)
      ONTAM= SUMDAT % ONTAM   !Organic N at maturity, soil & surf (kg/h)
      ONAM = SUMDAT % ONAM    !Organic soil N at maturity (kg/ha)
      OPTAM= SUMDAT % OPTAM   !Organic P at maturity, soil & surf (kg/h)
      OPAM = SUMDAT % OPAM    !Organic soil P at maturity (kg/ha)
      OCTAM= SUMDAT % OCTAM   !Organic C at maturity, soil & surf (kg/h)
      OCAM = SUMDAT % OCAM    !Organic soil C at maturity (kg/ha)

      PINUMM=SUMDAT % PINUMM  !Number of P applications   !PI#M
      PICM = SUMDAT % PICM    !P applied (kg/ha)          !PICM
      PUPC = SUMDAT % PUPC    !Cumul P uptake (kg[P]/ha)  !PUPC
      SPAM = SUMDAT % SPAM    !Soil P at maturity (kg/ha) !SPAM

      KINUMM=SUMDAT % KINUMM  !Number of K applications   !KI#M
      KICM = SUMDAT % KICM    !K applied (kg/ha)          !KICM
      KUPC = SUMDAT % KUPC    !Cumul K uptake (kg[P]/ha)  !KUPC
      SKAM = SUMDAT % SKAM    !Soil K at maturity (kg/ha) !SKAM

      DMPPM= SUMDAT % DMPPM   !Dry matter-rain productivity(kg[DM]/m3[P]
      DMPEM= SUMDAT % DMPEM   !Dry matter-ET productivity(kg[DM]/m3[ET]
      DMPTM= SUMDAT % DMPTM   !Dry matter-EP productivity(kg[DM]/m3[EP]
      DMPIM= SUMDAT % DMPIM   !Dry matter-irr productivity(kg[DM]/m3[I]
      YPPM = SUMDAT % YPPM    !Yield-rain productivity(kg[yield]/m3[P]
      YPEM = SUMDAT % YPEM    !Yield-ET productivity(kg[yield]/m3[ET]
      YPTM = SUMDAT % YPTM    !Yield-EP productivity(kg[yield]/m3[EP]
      YPIM = SUMDAT % YPIM    !Yield-irr productivity(kg[yield]/m3[I]

      DPNAM  = SUMDAT % DPNAM !Dry matter : N applied
      DPNUM  = SUMDAT % DPNUM !Dry matter : N uptake
      YPNAM  = SUMDAT % YPNAM !Yield : N applied
      YPNUM  = SUMDAT % YPNUM !Yield : N uptake

      NDCH   = SUMDAT % NDCH  !Number days 
      TMINA  = SUMDAT % TMINA  !Avg min daily temp (C) 
      TMAXA  = SUMDAT % TMAXA  !Avg max daily temp (C) 
      SRADA  = SUMDAT % SRADA !Avg solar rad (MJ/m2/d)
      DAYLA  = SUMDAT % DAYLA !Avg daylength (hr/d) 
      CO2A   = SUMDAT % CO2A  !Avg atm. CO2 (ppm) 
      PRCP   = SUMDAT % PRCP  !Cumulative rainfall (mm) 
      ETCP   = SUMDAT % ETCP  !Cumul ET (mm), planting to harvest
      ESCP   = SUMDAT % ESCP  !Cumul soil evap (mm), planting to harvest
      EPCP   = SUMDAT % EPCP  !Cumul transp (mm), planting to harvest

      CRST   = SUMDAT % CRST  !End of season crop status code

      CALL GET('WEATHER','WYEAR',WYEAR)
      CALL GET('FIELD','CYCRD',LATI)
      CALL GET('FIELD','CXCRD',LONG)
      CALL GET('FIELD','CELEV',ELEV)
      LATI = ADJUSTR(LATI)
      LONG = ADJUSTR(LONG)
      ELEV = ADJUSTR(ELEV)
      HYEAR= INT(YRDOY/1000)
C-------------------------------------------------------------------
C
C  Simulation Summary File
C
C-------------------------------------------------------------------
!     For csv output, include a text file with the version information
      SELECT CASE(FMOPT)
        CASE('C');    OUTS = "Version.OUT"
        CASE DEFAULT; OUTS = "Summary.OUT"
      END SELECT

      IF (INDEX('ADY',IDETS) .GT. 0) THEN
        INQUIRE (FILE = OUTS, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTDS, FILE = OUTS, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = NOUTDS, FILE = OUTS, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)

!         Check for simulation control file -- note in header if used:
          SimLen = LenString(CONTROL % SimControl)
          
          SELECT CASE(FMOPT)
!         For CSV output, write a text file with version and date inforamtion
          CASE('C')
!           Version information stored in ModuleDefs.for
            WRITE (NOUTDS,200) Version, VBranch,
     &        MonthTxt(DATE_TIME(2)), DATE_TIME(3), DATE_TIME(1), 
     &             DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
  200       FORMAT ('*VERSION',/,
     &       'DSSAT Cropping System Model Ver. ',    
     &       I1,'.',I1,'.',I1,'.',I3.3,1X,A,/,
     &       'Simulation date ',
     &       A3," ",I2.2,", ",I4,"; ",I2.2,":",I2.2,":",I2.2)
     &         
            IF (SimLen < 1) THEN
              WRITE(NOUTDS,'(A,A)') 
     &         "Simulation Control file: ", CONTROL%SimControl(1:SimLen)
            ENDIF

            CLOSE(NOUTDS)

!         If not CSV output, print the standard Summary.OUT header
          CASE DEFAULT
            IF (SimLen < 1) THEN
!             Version information stored in ModuleDefs.for
              WRITE (NOUTDS,300) EXPER, CG, ENAME, Version, VBranch,
     &          MonthTxt(DATE_TIME(2)), DATE_TIME(3), DATE_TIME(1), 
     &               DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
  300         FORMAT (
     &        '*SUMMARY : ',A8,A2,1X,A60,1X,
     &        'DSSAT Cropping System Model Ver. ',I1,'.',I1,'.',I1,'.',
     &         I3.3,1X,A10,4X,
     &         A3," ",I2.2,", ",I4,"; ",I2.2,":",I2.2,":",I2.2)
            
            ELSE
              WRITE (NOUTDS,305) EXPER, CG, ENAME, 
     &         "Simulation Control file: ",CONTROL%SimControl(1:SimLen),
     &         Version, VBranch, MonthTxt(DATE_TIME(2)), DATE_TIME(3), 
     &         DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
  305         FORMAT (
     &        '*SUMMARY : ',A8,A2,1X,A60,1X,A,A,5X,
     &        'DSSAT Cropping System Model Ver. ',I1,'.',I1,'.',I1,'.',
     &        I3.3,1X,A10,4X,
     &        A3," ",I2.2,", ",I4,"; ",I2.2,":",I2.2,":",I2.2)
            ENDIF
          
            WRITE(NOUTDS,310)
  310       FORMAT(/,
     &'!IDENTIFIERS......................... ',
     &'EXPERIMENT AND TREATMENT.......... ', 
     &'SITE INFORMATION.....................',
     &'........................................ ',
     &'DATES..................................................  ',
     &'DRY WEIGHT, YIELD AND YIELD COMPONENTS, AND ECONOMIC YIELD',
     &'.............................  ',
     &'FRESH WEIGHT..........................  ',
     &'WATER...............................................  ',
     &'NITROGEN..................................................  ',
     &'PHOSPHORUS............  ',
     &'POTASSIUM.............  ',
     &'ORGANIC MATTER.................................................',
     &'    ',
     &'WATER PRODUCTIVITY..................................',
     &'................    ',
     &'NITROGEN PRODUCTIVITY...........  ',
     &'SEASONAL ENVIRONMENTAL DATA (Planting to harvest)..............',
     &'STATUS')

            WRITE (NOUTDS,400)
! CHP 3/14/2018 USE P# for REPNO instead of C# for CRPNO, which isn't used.
  400       FORMAT ('@   RUNNO   TRNO R# O# P# CR MODEL... ',
     &   'EXNAME.. TNAM..................... ',
     &   'FNAM.... WSTA.... WYEAR SOIL_ID... ',
     &   '            XLAT            LONG      ELEV  ',
     &   '  SDAT    PDAT    EDAT    ADAT    MDAT    HDAT   HYEAR',
     &   '  DWAP    CWAM    HWAM    HWAH    BWAH  PWAM',
!    &   '    HWUM  H#AM    H#UM  HIAM  LAIX',
     &   '    HWUM    H#AM    H#UM  HIAM  LAIX    EYLDH',
     &   '   FCWAM   FHWAM   HWAHF   FBWAH   FPWAM',
     &   '  IR#M  IRCM  PRCM  ETCM  EPCM  ESCM  ROCM  DRCM  SWXM',
     &   '  NI#M  NICM  NFXM  NUCM  NLCM  NIAM NMINC  CNAM  GNAM N2OEM',
!    &   '  NI#M  NICM  NFXM  NUCM  NLCM  NIAM  CNAM  GNAM N2OGC',
     &   '  PI#M  PICM  PUPC  SPAM',
     &   '  KI#M  KICM  KUPC  SKAM',
     &   '  RECM  ONTAM   ONAM  OPTAM   OPAM   OCTAM    OCAM',
     &   '   CO2EM  CH4EM',
     &   '    DMPPM    DMPEM    DMPTM    DMPIM     YPPM     YPEM',
     &   '     YPTM     YPIM',
     &   '    DPNAM    DPNUM    YPNAM    YPNUM',
     &   '  NDCH TMAXA TMINA SRADA DAYLA   CO2A   PRCP   ETCP',
     &   '   ESCP   EPCP',
     &   '  CRST')
          END SELECT
        ENDIF

        IF (BWAH < -1) BWAH = -9.9

        MODEL = CONTROL % MODEL

        IF (FMOPT == 'A' .OR. FMOPT == ' ' .OR. FMOPT == '') THEN
        WRITE (NOUTDS,500,ADVANCE='NO') 
     &    RUN, TRTNUM, ROTNO, ROTOPT, REPNO, 
     &    CROP, MODEL, CONTROL%FILEX(1:8), TITLET, FLDNAM, WSTAT, WYEAR,
     &    SLNO,LATI, LONG, ELEV,
     &    YRSIM, YRPLT, EDAT, ADAT, MDAT, YRDOY, HYEAR, 
     &    DWAP, CWAM, HWAM, NINT(HWAH), NINT(BWAH*10.), PWAM

!       RUN, TRTNUM, ROTNO, ROTOPT, REPNO (was CRPNO), 
  500   FORMAT (I9,1X,I6,3(I3),               

!       CROP, MODEL, FILEX, TITLET, FLDNAM, WSTAT, WYEAR, SLNO,
     &  1X,A2,1X,A8,1X,A8,1X,A25,1X,A8,1X,A8,1X,I5,1X,A10,      

!       LATI, LONG, ELEV
     &  1X,2(1X,A15),1X,A9,
     
!       YRSIM, YRPLT, EDAT, ADAT, MDAT, YRDOY, 
     &  7(1X,I7),

!       DWAP, CWAM, HWAM, NINT(HWAH), NINT(BWAH*10.), PWAM,
     &  1X,I5,4(1X,I7),1X,I5)

        IF     (HWUM < -.01)  THEN; FMT = '(1X,F7.0)'
        ELSEIF (HWUM < 1.)    THEN; FMT = '(1X,F7.4)'
        ELSEIF (HWUM < 10.)   THEN; FMT = '(1X,F7.3)'
        ELSEIF (HWUM < 100.)  THEN; FMT = '(1X,F7.2)'
        ELSEIF (HWUM < 1000.) THEN; FMT = '(1X,F7.1)'
        ELSE                      ; FMT = '(1X,F7.0)'
        ENDIF
        WRITE (NOUTDS,FMT,ADVANCE="NO") HWUM

!        WRITE (NOUTDS,'(1X,I5,1X,F7.1)',ADVANCE="NO") HNUMAM, HNUMUM
        WRITE (NOUTDS,'(1X,I7,1X,F7.1)',ADVANCE="NO") HNUMAM, HNUMUM

        IF (HIAM < -.01)  THEN; FMT = '(1X,F5.0)'
        ELSE                  ; FMT = '(1X,F5.3)'
        ENDIF
        WRITE (NOUTDS,FMT,ADVANCE='NO') HIAM

!       Handle formatting for real numbers which may have value of "-99"
        CALL PrintText(DMPPM, "(F9.1)", DMPPM_TXT)
        CALL PrintText(DMPPM, "(F9.1)", DMPPM_TXT)
        CALL PrintText(DMPEM, "(F9.1)", DMPEM_TXT)
        CALL PrintText(DMPTM, "(F9.1)", DMPTM_TXT)
        CALL PrintText(DMPIM, "(F9.1)", DMPIM_TXT)
        CALL PrintText(YPPM,  "(F9.1)", YPPM_TXT )
        CALL PrintText(YPEM,  "(F9.1)", YPEM_TXT )
        CALL PrintText(YPTM,  "(F9.1)", YPTM_TXT )
        CALL PrintText(YPIM,  "(F9.1)", YPIM_TXT )
        CALL PrintText(DPNAM, "(F9.1)", DPNAM_TXT)
        CALL PrintText(DPNUM, "(F9.1)", DPNUM_TXT)
        CALL PrintText(YPNAM, "(F9.1)", YPNAM_TXT)
        CALL PrintText(YPNUM, "(F9.1)", YPNUM_TXT)
        CALL PrintText(SRADA, "(F6.1)", SRADA_TXT)
        CALL PrintText(DAYLA, "(F6.1)", DAYLA_TXT)
        CALL PrintText(CO2A,  "(F7.1)", CO2A_TXT )
        CALL PrintText(PRCP,  "(F7.1)", PRCP_TXT )
        CALL PrintText(ETCP,  "(F7.1)", ETCP_TXT )
        CALL PrintText(ESCP,  "(F7.1)", ESCP_TXT )
        CALL PrintText(EPCP,  "(F7.1)", EPCP_TXT )

!       Allow negative values for TMAX, TMIN, and net CO2 emissions
        CALL PrintTxtNeg(TMINA, 6, 1, TMINA_TXT)
        CALL PrintTxtNeg(TMAXA, 6, 1, TMAXA_TXT)
        CALL PrintTxtNeg(CO2EM, 8, 1, CO2EM_TXT)

!       N2O emissions
        IF (N2OEM .LT. -0.00001) THEN
          N2OEC_TXT = "   -99"
        ELSEIF (N2OEM .LT. 1) THEN
          CALL PrintText(N2OEM, "(F6.3)", N2OEC_TXT) !kg/ha
        ELSEIF (N2OEM .LT. 10) THEN
          CALL PrintText(N2OEM, "(F6.2)", N2OEC_TXT) !kg/ha
        ELSEIF (N2OEM .LT. 100) THEN
          CALL PrintText(N2OEM, "(F6.1)", N2OEC_TXT) !kg/ha
        ELSE
          CALL PrintText(N2OEM, "(F6.0)", N2OEC_TXT) !kg/ha
        ENDIF

!       CALL PrintText(N2OEM*1000.,"(F6.1)",N2OGC_TXT)   !g/ha

        IF (FBWAH .GT. 1.E-3) THEN
          FBWAH = FBWAH * 10.
        ENDIF

        WRITE (NOUTDS,510,ADVANCE='NO') LAIX
510     FORMAT(                                
!       LAIX,
     &  F6.1)

        ! 2024-07-11 FO - Economic standard output format
        IF    (EYLDH < 0.999) THEN; FMT = '(1X,F8.3)'
        ELSEIF(EYLDH < 10.0)  THEN; FMT = '(1X,F8.2)'
        ELSEIF(EYLDH < 100.0) THEN; FMT = '(1X,F8.1)'
        ELSEIF(EYLDH < 1000.0)THEN
          iEYLDH = INT(EYLDH)
          FMT = '(1X,I8)'
        ELSEIF(EYLDH < 10000.0)THEN
          EYLDH = ROUND(EYLDH, -1)
          iEYLDH = INT(EYLDH)
          FMT = '(1X,I8)'
        ELSEIF(EYLDH < 100000.0)THEN
          EYLDH = ROUND(EYLDH, -2)
          iEYLDH = INT(EYLDH)
          FMT = '(1X,I8)'
        ELSE
          EYLDH = ROUND(EYLDH, -2)
          iEYLDH = INT(EYLDH)
          FMT = '(1X,I8)'
        ENDIF
        
        IF(EYLDH < 100.0) THEN
            WRITE (NOUTDS,FMT,ADVANCE='NO') EYLDH
        ELSE
            WRITE (NOUTDS,FMT,ADVANCE='NO') iEYLDH
        ENDIF

        WRITE (NOUTDS,520,ADVANCE='NO')
     &    FCWAM, FHWAM, NINT(HWAHF), NINT(FBWAH), FPWAM 
520     FORMAT(
!       FCWAM, FHWAM, NINT(HWAHF), NINT(FBWAH*10.), FPWAM
     &  5(1X,I7))
     
        WRITE (NOUTDS,530)
     &    IRNUM, IRCM, PRCM, ETCM, EPCM, ESCM, ROCM, DRCM, SWXM, 
     &    NINUMM, NICM, NFXM, NUCM, NLCM, NIAM, NMINC, CNAM, GNAM, 
     &    N2OEC_TXT,
!    &    N2OGC_TXT,
     &    PINUMM, PICM, PUPC, SPAM,        !P data
     &    KINUMM, KICM, KUPC, SKAM,        !K data
     &    RECM, ONTAM, ONAM, OPTAM, OPAM, OCTAM, OCAM, CO2EM_TXT, CH4EM,
!         Water productivity
     &    DMPPM_TXT, DMPEM_TXT, DMPTM_TXT, DMPIM_TXT, 
     &                 YPPM_TXT, YPEM_TXT, YPTM_TXT, YPIM_TXT,
     &    DPNAM_TXT, DPNUM_TXT, YPNAM_TXT, YPNUM_TXT,
     &    NDCH, TMAXA_TXT, TMINA_TXT, SRADA_TXT, DAYLA_TXT, 
     &                 CO2A_TXT, PRCP_TXT, ETCP_TXT, ESCP_TXT, EPCP_TXT,
     &    CRST

  530   FORMAT(                                            
!       IRNUM, IRCM, PRCM, ETCM, EPCM, ESCM, ROCM, DRCM, SWXM, 
!       NINUMM, NICM, NFXM, NUCM, NLCM, NIAM, NMINC, CNAM, GNAM, 
     &  18(1X,I5),

!        N2OEC_TXT,
     &  A,

!       PINUMM, PICM, PUPC, SPAM, 
!       KINUMM, KICM, KUPC, SKAM, RECM, 
     &  9(1X,I5),
       
!       ONTAM, ONAM, OPTAM, OPAM, OCTAM, OCAM, CO2EM, CH4EM,
     &  4(1X,I6),2(1X,I7), A, F7.1,      
   
!       DMPPM, DMPEM, DMPTM, DMPIM, YPPM, YPEM, YPTM, YPIM
!    &  4F9.1,4F9.2,
     &  8A,

!       DPNAM, DPNUM, YPNAM, YPNUM
!    &  4F9.1,
     &  4A,

!       NDCH, TMINA, TMAXA, SRADA, DAYLA, CO2A, PRCP, ETCP, ESCP, EPCP
!    &  I6,3F6.1,F6.2,5F7.1)
     &  I6,9A,

!       CRST
     &  I6)

        CLOSE (NOUTDS)
        END IF   ! VSH
        
!       VSH summary.csv header
        IF (FMOPT == 'C') THEN  

!           CALL CsvOutSumOpsum(RUN, TRTNUM, ROTNO, ROTOPT, CRPNO, CROP,
            CALL CsvOutSumOpsum(RUN, TRTNUM, ROTNO, ROTOPT, REPNO, CROP,
     &MODEL, CONTROL%FILEX(1:8), TITLET, FLDNAM, WSTAT,WYEAR,SLNO,
     &LATI,LONG,ELEV,YRSIM,YRPLT, EDAT, ADAT, MDAT, YRDOY, HYEAR, DWAP, 
     &CWAM, HWAM, HWAH, BWAH,
!     &PWAM, HWUM, HNUMUM, HIAM, LAIX, HNUMAM, IRNUM, IRCM, PRCM, ETCM,
     &PWAM, HWUM, HNUMUM, HIAM, LAIX,EYLDH,HNUMAM,FCWAM, FHWAM,HWAHF, 
     &FBWAH, FPWAM, IRNUM, IRCM, PRCM, ETCM,
     &EPCM, ESCM, ROCM, DRCM, SWXM, NINUMM, NICM, NFXM, NUCM, NLCM, 
     &NIAM, NMINC, CNAM, GNAM, N2OEM, PINUMM, PICM, PUPC, SPAM, KINUMM, 
     &KICM, KUPC, SKAM, RECM, ONTAM, ONAM, OPTAM, OPAM, OCTAM, OCAM, 
     &CO2EM, CH4EM, DMPPM, DMPEM, DMPTM, DMPIM, YPPM, YPEM, YPTM, YPIM, 
     &DPNAM, DPNUM, YPNAM, YPNUM, NDCH, TMAXA, TMINA, SRADA, DAYLA, 
     &CO2A, PRCP, ETCP, ESCP, EPCP, CRST,   
     &vCsvlineSumOpsum, vpCsvlineSumOpsum, vlngthSumOpsum) 

            CALL LinklstSumOpsum(vCsvlineSumOpsum) 
        END IF
                
      ENDIF
C-------------------------------------------------------------------
C     Console output for multi-season runs:
C     Was OPBAT subroutine
C-------------------------------------------------------------------
!      IF (INDEX('NQSABCGF',RNMODE) .GT. 0 .OR. NYRS .GT. 1) THEN
      IF ((INDEX('NQSABCGFY',RNMODE) .GT. 0 .OR. NYRS .GT. 1) .AND.
     &    (IDETL .NE. "0")) THEN
          NLINES = RUN - 1
        IF (RUN .EQ. 1) THEN
          CALL CLEAR
        ENDIF
        NLINES = MOD(NLINES,20)
        IF (NLINES .EQ. 0) THEN
          IF (RUN .LT. 1000) THEN
            IF (INDEX('Q',RNMODE) .GT. 0) THEN
              WRITE(*,601)
            ELSE
              WRITE(*,600)
            ENDIF
            WRITE(*,610)
          ELSE
            RUN2 = (RUN / 1000) * 1000
            IF (INDEX('Q',RNMODE) .GT. 0) THEN
              WRITE(*,603)
            ELSE
              WRITE(*,602)
            ENDIF
            WRITE(*,612) RUN2
          ENDIF
        ENDIF
  600 FORMAT('RUN    TRT FLO MAT TOPWT HARWT  RAIN  TIRR',
     &                    '   CET  PESW  TNUP  TNLF   TSON TSOC')
  601 FORMAT('RUN    ROT FLO MAT TOPWT HARWT  RAIN  TIRR',
     &                    '   CET  PESW  TNUP  TNLF   TSON TSOC')
  602 FORMAT('RUN+   TRT FLO MAT TOPWT HARWT  RAIN  TIRR',
     &                    '   CET  PESW  TNUP  TNLF   TSON TSOC')
  603 FORMAT('RUN+   ROT FLO MAT TOPWT HARWT  RAIN  TIRR',
     &                    '   CET  PESW  TNUP  TNLF   TSON TSOC')
  610 FORMAT('           dap dap kg/ha kg/ha    mm    mm    mm    mm',
     &                                     ' kg/ha kg/ha  kg/ha t/ha')
  612 FORMAT (I5, '      dap dap kg/ha kg/ha    mm    mm    mm    mm',
     &                                     ' kg/ha kg/ha  kg/ha t/ha')

        DNR1 = TIMDIF(YRPLT, ADAT)
        IF (DNR1 .LT. 0 .OR. YRPLT .LT. 0 .OR. DNR1 .GT. 999) THEN
          DNR1 = -99
        ENDIF
        DNR7 = TIMDIF(YRPLT, MDAT)
        IF (DNR7 .LT. 0 .OR. YRPLT .LT. 0 .OR. DNR7 .GT. 999) THEN
          DNR7 = -99
        ENDIF

        IF (INDEX('Q',RNMODE) > 0) THEN
          WRITE (*,'(I3,1X,A2,I4,2(1X,I3))',ADVANCE='NO') 
     &      MOD(RUN,1000), CROP, MOD(ROTNO,1000), DNR1, DNR7
        ELSE
          WRITE (*,'(I3,1X,A2,I4,2(1X,I3))',ADVANCE='NO') 
     &      MOD(RUN,1000), CROP, MOD(TRTNUM,1000), DNR1, DNR7
        ENDIF

        IF (CWAM > 99999. .OR. HWAM > 99999. .OR. HWAH > 99999.) THEN
          WRITE(*,'(I6,"T",I5,"T",I5)',ADVANCE='NO') 
     &      CWAM/1000, NINT(HWAH)/1000, PRCM
        ELSE
          WRITE(*,'(3I6)',ADVANCE='NO') CWAM, NINT(HWAH), PRCM
        ENDIF

        WRITE(*,'(5I6,I7,I5)') IRCM, ETCM, SWXM, NUCM, NIAM, 
     &      ONAM, NINT(OCAM/1000.)
        NLINES=NLINES+1
      ENDIF

C-------------------------------------------------------------------
!     Write Evaluate.OUT file
!     IF((INDEX('0',IDETL) < 1 .AND. INDEX('IAEBCGDT',RNMODE) > 0) .AND.
!     Evaluate.OUT printed whenever Overview.OUT is printed (i.e., switch
!         with IDETO -- CHP 8/31/2007
!     Add a separate switch for Evaluate.OUT, but not Overview.OUT
!     IDETO = Y - both Overview and Evaluate are printed
!     IDETO = N - neither Overview nor Evaluate are printed
!     IDETO = E - only Evaluate is printed.
!      IF (INDEX('YE',IDETO) > 0 .AND. 
      IF (INDEX('YE',IDETO) > 0) THEN
!     &        CROP .NE. 'WH' .AND. CROP .NE. 'BA' .AND.
!     &        CROP .NE. 'BA' .AND. !JZW changed
!     &        CROP .NE. 'CS') THEN
!     CHP 18 Aug 2015 Exclude by model, not crop
!        SELECT CASE(MODEL)
        SELECT CASE(MODEL(1:5))
        CASE('CSCER', 'CSCRP', 'CSCAS', 'CSYCA')
!         These models write out Evaluate.OUT using separate routines
!         CSCER    BA   CROPSIM-CERES-Barley
!         CSCER    WH   CROPSIM-CERES-Wheat
!         CSCRP    BA   CSCRP-Barley
!         CSCAS    CS   CSCAS-Cassava
!         CSYCA    CS   CSYCA-Cassava
!         CSCRP    WH   CSCRP-Wheat
          CONTINUE 

!       All other models, print out Evaluate.OUT
        CASE DEFAULT
          ICOUNT    = EvaluateData % ICOUNT
          OLAP      = EvaluateData % OLAP
          DESCRIP   = EvaluateData % DESCRIP
          Simulated = EvaluateData % Simulated
          Measured  = EvaluateData % Measured

!         Check for simulation aborted (YRSIM == YRDOY)
!         Print out "-99"s
          IF (CONTROL % ERRCODE > 0) THEN
            Simulated = "     -99"
          ENDIF

          IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
!         Open or create Evaluate.out file
          INQUIRE (FILE = SEVAL, EXIST = FEXIST)
          IF (FEXIST) THEN
            OPEN (UNIT = SLUN, FILE = SEVAL, STATUS = 'OLD',
     &        IOSTAT = ERRNUM, POSITION = 'APPEND')
          ELSE
            OPEN (UNIT = SLUN, FILE = SEVAL, STATUS = 'NEW',
     &        IOSTAT = ERRNUM)
            CALL DATE_AND_TIME (VALUES=DATE_TIME)
            WRITE (SLUN,700) EXPER, CG, ENAME, Version, VBranch,
!           WRITE (SLUN,700) MODEL, EXPER, CG, ENAME, Version,
     &        MonthTxt(DATE_TIME(2)), DATE_TIME(3), DATE_TIME(1), 
     &        DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
  700       FORMAT ('*EVALUATION : ',A8,A2,1X,A60,1X,
! 700       FORMAT ('*EVALUATION : ',A8,1X,A8,A2,1X,A60,1X,
     &    'DSSAT Cropping System Model Ver. ',I1,'.',I1,'.',I1,'.',I3.3,
     &     1X,A10,4X,A3," ",I2.2,", ",I4,"; ",I2.2,":",I2.2,":",I2.2)
          ENDIF

!         Write headers if new crop is being processed
          IF (MODEL .NE. MODEL_LAST) THEN
            WRITE(SLUN,
     &       '(/,"@RUN EXCODE        TN RN CR",80(1X,A7))')   
     &       (ADJUSTR(OLAP(I))//"S",ADJUSTR(OLAP(I))//"M",I = 1, ICOUNT)
            MODEL_LAST = MODEL
          ENDIF

!         Write evaluation data
          WRITE(SLUN,750) RUN, EXPER, CG, TRTNUM, ROTNO, CROP, 
     &            (Simulated(I), Measured(I), I= 1,ICOUNT)
  750     FORMAT(I4,1X,A8,A2,I6,I3,1X,A2,80A8)
          CLOSE(SLUN)
          END IF   ! VSH
         
!        VSH  for evaluate.csv 
         IF (FMOPT == 'C') THEN 
            csvICOUNT = ICOUNT
            csvOLAP = OLAP
            CALL CsvOutEvOpsum(EXPER, RUN, CG, TRTNUM, ROTNO,  CROP, 
     &Simulated, Measured, ICOUNT,   
     &vCsvlineEvOpsum, vpCsvlineEvOpsum, vlngthEvOpsum) 
            
            CALL LinklstEvOpsum(vCsvlineEvOpsum)
         END IF
         
        END SELECT
      ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OPSUM
!=======================================================================

!=======================================================================
!  PrintText, Subroutine, C.H.Porter
!     Sends back a text string for a real value with format provided.
!     Negative values return a "-99" string.
!   Input:  
!     VALUE = real value
!     FTXT  = format for real value
!   Output:
!     PRINT_TXT = text string for real value
!=======================================================================
      Subroutine PrintText(VALUE, FTXT, PRINT_TXT)

      REAL, INTENT(IN) :: VALUE
      CHARACTER(LEN=*), INTENT(IN) :: FTXT      
      CHARACTER(LEN=*), INTENT(OUT) :: PRINT_TXT

      CHARACTER(LEN=6) FTXT1     !modified format for real value
      CHARACTER(LEN=7) FTXT2     !format for "-99"
      INTEGER I, ERRNUM

      READ (FTXT,'(2X,I1)',IOSTAT=ERRNUM) I   !width of field
      IF (ERRNUM == 0 .AND. I > 0) THEN
        FTXT1 = FTXT
        WRITE(FTXT2,'("(",I1,"X,A3)")') I-3   
      ELSE
        FTXT1 = "(F6.1)"
        FTXT2 = "(3X,A3)"
      ENDIF

      IF (VALUE > 0.0) THEN
        WRITE(PRINT_TXT,FTXT1) VALUE
      ELSE
        WRITE(PRINT_TXT,FTXT2) "-99"
      ENDIF

      End Subroutine PrintText
!=======================================================================

!=======================================================================
!  PrintTxtNeg, Subroutine, C.H.Porter
!     Sends back a text string for a real value with format provided.
!     Allows real negative values.
!   Input:  
!     VALUE = real value
!     FTXT  = format for real value
!   Output:
!     PRINT_TXT_neg = text string for real value
!
!   2024-05-27 chp - handle missing values (-99). If the value is  exactly
!    equal to -99.00000 then handle as missing (low risk of a false 
!    missing value). Missing values are reported as "-99" integer values,
!     Actual values are floating point.
!=======================================================================
      Subroutine PrintTxtNeg(VALUE, FWID, FDEC, PRINT_TXT_neg)

      REAL, INTENT(IN) :: VALUE
      INTEGER, INTENT(IN) :: FWID, FDEC
      CHARACTER(LEN=*), INTENT(OUT) :: PRINT_TXT_neg 
      CHARACTER(LEN=10) :: FTXT 

!     Handle missing values (exactly equal to -99)
      IF (ABS(VALUE + 99.) .LT. 1E-6) THEN
!       assume missing value, change format to integer
        IF (FWID < 9) THEN
          WRITE(FTXT,'(A,I1,A)') "(I",FWID,")"
        ELSE
          WRITE(FTXT,'(A,I2,A)') "(I",FWID,")"
        ENDIF
        WRITE (PRINT_TXT_neg, FTXT) NINT(VALUE)
      ELSE
        IF (FWID < 9) THEN
          WRITE(FTXT,'(A,I1,A,I1,A)') "(F", FWID, ".", FDEC, ")"
        ELSE
          WRITE(FTXT,'(A,I2,A,I1,A)') "(F", FWID, ".", FDEC, ")"
        ENDIF
        WRITE (PRINT_TXT_neg, FTXT) VALUE
      ENDIF

      RETURN
      End Subroutine PrintTxtNeg
!=======================================================================

!=======================================================================
!  SUMVALS, Subroutine C. H. Porter
!  Obtains and stores Summary.out values
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  01/09/2002 CHP Written
!=======================================================================
      SUBROUTINE SUMVALS (NUMBER, LABEL, VALUE) 

!-----------------------------------------------------------------------
      USE SumModule
      USE ModuleDefs
      USE ModuleData
      IMPLICIT NONE

      INTEGER I, NUMBER
      CHARACTER*(*), DIMENSION(NUMBER) :: LABEL
      REAL, DIMENSION(NUMBER) :: VALUE

!-----------------------------------------------------------------------
      TYPE (ControlType) CONTROL
      CALL GET(CONTROL)
      IF (CONTROL % ERRCODE > 0) THEN
!       Simulation terminated abnormally, computed values are meaningless
        VALUE = -99.
      ENDIF

!-----------------------------------------------------------------------
      DO I = 1, NUMBER
        SELECT CASE(TRIM(LABEL(I)))

        !From OPHARV:
        CASE ('EDAT'); SUMDAT % EDAT   = NINT(VALUE(I))
        CASE ('ADAT'); SUMDAT % ADAT   = NINT(VALUE(I))
        CASE ('MDAT'); SUMDAT % MDAT   = NINT(VALUE(I))
        CASE ('DWAP'); SUMDAT % DWAP   = NINT(VALUE(I))
        CASE ('CWAM'); SUMDAT % CWAM   = NINT(VALUE(I))
        CASE ('HWAM'); SUMDAT % HWAM   = NINT(VALUE(I))
        CASE ('HWAH'); SUMDAT % HWAH   = VALUE(I) !Float
        CASE ('BWAH'); SUMDAT % BWAH   = VALUE(I) !Float
        CASE ('HWUM'); SUMDAT % HWUM   = VALUE(I) !Float
        CASE ('H#AM'); SUMDAT % HNUMAM = NINT(VALUE(I))
        CASE ('H#UM'); SUMDAT % HNUMUM = VALUE(I) !Float
        CASE ('NFXM'); SUMDAT % NFXM   = NINT(VALUE(I))
        CASE ('NUCM'); SUMDAT % NUCM   = NINT(VALUE(I))
        CASE ('CNAM'); SUMDAT % CNAM   = NINT(VALUE(I))
        CASE ('GNAM'); SUMDAT % GNAM   = NINT(VALUE(I))
        CASE ('PWAM'); SUMDAT % PWAM   = NINT(VALUE(I)) 
        CASE ('LAIX'); SUMDAT % LAIX   = VALUE(I) !Float
        CASE ('HIAM'); SUMDAT % HIAM   = VALUE(I) !Float
        !LPM 2021-04-20 Add fresh weight variables
        CASE ('FCWAM'); SUMDAT % FCWAM   = NINT(VALUE(I))
        CASE ('FHWAM'); SUMDAT % FHWAM   = NINT(VALUE(I))
        CASE ('HWAHF'); SUMDAT % HWAHF   = VALUE(I) !Float
        CASE ('FBWAH'); SUMDAT % FBWAH   = VALUE(I) !Float
        CASE ('FPWAM'); SUMDAT % FPWAM   = NINT(VALUE(I))

        !From MgmtOps:
        CASE ('IR#M'); SUMDAT % IRNUM    = NINT(VALUE(I))
        CASE ('IRCM'); SUMDAT % IRCM   = NINT(VALUE(I))

        !From OPSPAM:
        CASE ('ETCM'); SUMDAT % ETCM = NINT(VALUE(I))
        CASE ('EPCM'); SUMDAT % EPCM = NINT(VALUE(I)) 
        CASE ('ESCM'); SUMDAT % ESCM = NINT(VALUE(I)) 

        !From OPWBAL:
        CASE ('PRCM'); SUMDAT % PRCM = NINT(VALUE(I))
        CASE ('ROCM'); SUMDAT % ROCM = NINT(VALUE(I))
        CASE ('DRCM'); SUMDAT % DRCM = NINT(VALUE(I))
        CASE ('SWXM'); SUMDAT % SWXM = NINT(VALUE(I))

        !From OpSoilNC:
        CASE ('NI#M'); SUMDAT % NINUMM = NINT(VALUE(I))
        CASE ('NICM'); SUMDAT % NICM   = NINT(VALUE(I))
        CASE ('NLCM'); SUMDAT % NLCM   = NINT(VALUE(I))
        CASE ('NIAM'); SUMDAT % NIAM   = NINT(VALUE(I))
        CASE ('NMINC');SUMDAT % NMINC  = NINT(VALUE(I))
        CASE ('RECM'); SUMDAT % RECM   = NINT(VALUE(I))

        CASE ('OCTAM');SUMDAT % OCTAM  = NINT(VALUE(I))
        CASE ('OCAM'); SUMDAT % OCAM   = NINT(VALUE(I))
        CASE ('ONTAM');SUMDAT % ONTAM  = NINT(VALUE(I))
        CASE ('ONAM'); SUMDAT % ONAM   = NINT(VALUE(I))
        CASE ('OPTAM');SUMDAT % OPTAM  = NINT(VALUE(I))
        CASE ('OPAM'); SUMDAT % OPAM   = NINT(VALUE(I))

        !From OpSoilP
        CASE ('PI#M'); SUMDAT % PINUMM = NINT(VALUE(I))
        CASE ('PICM'); SUMDAT % PICM   = NINT(VALUE(I))
        CASE ('PUPC'); SUMDAT % PUPC   = NINT(VALUE(I))
        CASE ('SPAM'); SUMDAT % SPAM   = NINT(VALUE(I))

        !From OpSoilK
        CASE ('KI#M'); SUMDAT % KINUMM = NINT(VALUE(I))
        CASE ('KICM'); SUMDAT % KICM   = NINT(VALUE(I))
        CASE ('KUPC'); SUMDAT % KUPC   = NINT(VALUE(I))
        CASE ('SKAM'); SUMDAT % SKAM   = NINT(VALUE(I))

        !From OpStress
        CASE ('DMPPM');SUMDAT % DMPPM  = VALUE(I)
        CASE ('DMPEM');SUMDAT % DMPEM  = VALUE(I)
        CASE ('DMPTM');SUMDAT % DMPTM  = VALUE(I)
        CASE ('DMPIM');SUMDAT % DMPIM  = VALUE(I)
        CASE ('DPNAM');SUMDAT % DPNAM  = VALUE(I)
        CASE ('DPNUM');SUMDAT % DPNUM  = VALUE(I)

        CASE ('YPPM'); SUMDAT % YPPM   = VALUE(I)
        CASE ('YPEM'); SUMDAT % YPEM   = VALUE(I)
        CASE ('YPTM'); SUMDAT % YPTM   = VALUE(I)
        CASE ('YPIM'); SUMDAT % YPIM   = VALUE(I)
        CASE ('YPNAM');SUMDAT % YPNAM  = VALUE(I)
        CASE ('YPNUM');SUMDAT % YPNUM  = VALUE(I)

        CASE ('NDCH'); SUMDAT % NDCH   = NINT(VALUE(I))
        CASE ('TMINA');SUMDAT % TMINA  = VALUE(I)
        CASE ('TMAXA');SUMDAT % TMAXA  = VALUE(I)
        CASE ('SRADA');SUMDAT % SRADA  = VALUE(I)
        CASE ('DAYLA');SUMDAT % DAYLA  = VALUE(I)
        CASE ('CO2A'); SUMDAT % CO2A   = VALUE(I)
        CASE ('PRCP'); SUMDAT % PRCP   = VALUE(I)
        CASE ('ETCP'); SUMDAT % ETCP   = VALUE(I)
        CASE ('ESCP'); SUMDAT % ESCP   = VALUE(I)
        CASE ('EPCP'); SUMDAT % EPCP   = VALUE(I)

!       From GHG_Mod
        CASE ('N2OEM');SUMDAT % N2OEM  = VALUE(I)
        CASE ('CO2EM');SUMDAT % CO2EM  = VALUE(I)
        CASE ('CH4EM');SUMDAT % CH4EM  = VALUE(I)
               
        !From Ipexp or Ipwth:
        CASE ('YCRD'); SUMDAT % YCRD  = VALUE(I)
        CASE ('XCRD'); SUMDAT % XCRD  = VALUE(I)
        CASE ('ELEV'); SUMDAT % ELEV  = VALUE(I)

!       Crop status
        CASE ('CRST') ;SUMDAT % CRST   = VALUE(I)
            
!       Economic Yield
        CASE ('EYLDH') ;SUMDAT % EYLDH = VALUE(I)


        END SELECT
      ENDDO

      RETURN
      END SUBROUTINE SUMVALS

C=======================================================================


C=======================================================================
C  EvaluateDat, Subroutine C. H. Porter
C  Obtains and stores Evaluate.out values
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  03/21/2002 CHP Written
C=======================================================================
      SUBROUTINE EvaluateDat(ICOUNT, Measured, Simulated, DESCRIP, OLAP)

C-----------------------------------------------------------------------
      USE SumModule     
      IMPLICIT NONE

      INTEGER I, ICOUNT
      CHARACTER*6,  DIMENSION(EvaluateNum) :: OLAP
      CHARACTER*8,  DIMENSION(EvaluateNum) :: Measured, Simulated
      CHARACTER*50, DIMENSION(EvaluateNum) :: DESCRIP

C-----------------------------------------------------------------------
      EvaluateData % ICOUNT = MIN(ICOUNT, EvaluateNum)
      DO I = 1, ICOUNT
        EvaluateData % Measured(I)  = Measured(I)
        EvaluateData % Simulated(I) = Simulated(I)
        EvaluateData % DESCRIP(I)   = DESCRIP(I)
        EvaluateData % OLAP(I)      = OLAP(I)
      ENDDO

      RETURN
      END SUBROUTINE EvaluateDat

C=======================================================================
