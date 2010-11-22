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
      IMPLICIT NONE
      SAVE

      CHARACTER*1  IDETL, IDETO, IDETS, RNMODE
      CHARACTER*2  CROP, CG
      CHARACTER*6  SECTION
      CHARACTER*6, PARAMETER :: ERRKEY = 'OPSUM '
      CHARACTER*8  EXPER, FLDNAM, MODEL, MODEL_LAST
      CHARACTER*12 OUTS, SEVAL, FMT
      PARAMETER (OUTS = 'Summary.OUT')
      CHARACTER*25 TITLET
      CHARACTER*30 FILEIO
      CHARACTER*60 ENAME

      INTEGER ADAT, CNAM, CRPNO, CWAM, DNR1, DNR7, DRCM, DWAP, DYNAMIC
      INTEGER EDAT, ERRNUM, ETCM, FOUND, GNAM, HNUMAM, HWAM
      INTEGER I, IRCM, LUNIO, LINC, LNUM, MDAT, IRNUM, NINUMM
      INTEGER NFXM, NIAM, NICM, NLCM, NLINES  !, NNAPHO
      INTEGER NOUTDS, NUCM, NYRS, PRCM, RECM, ROCM, ONAM, OCAM
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

      LOGICAL FEXIST

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
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM
      NYRS    = CONTROL % NYRS

      IDETS   = ISWITCH % IDETS
      IDETO   = ISWITCH % IDETO
      IDETL   = ISWITCH % IDETL

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
      SUMDAT % CNAM   = -99
      SUMDAT % GNAM   = -99
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
      CNAM = SUMDAT % CNAM    !Tops N at Maturity (kg/ha)
      GNAM = SUMDAT % GNAM    !Grain N at Maturity (kg/ha)

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

C-------------------------------------------------------------------
C
C  Simulation Summary File
C
C-------------------------------------------------------------------
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
          IF (SimLen < 1) THEN
      
!           Version information stored in ModuleDefs.for
            WRITE (NOUTDS,300) EXPER, CG, ENAME, Version, VBranch,
     &        MonthTxt(DATE_TIME(2)), DATE_TIME(3), DATE_TIME(1), 
     &             DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
  300       FORMAT (
     &      '*SUMMARY : ',A8,A2,1X,A60,1X,
     &      'DSSAT Cropping System Model Ver. ',I1,'.',I1,'.',I1,'.',
     &       I3.3,1X,A10,4X,
     &       A3," ",I2.2,", ",I4,"; ",I2.2,":",I2.2,":",I2.2)

          ELSE
            WRITE (NOUTDS,305) EXPER, CG, ENAME, 
     &        "Simulation Control file: ", CONTROL%SimControl(1:SimLen),
     &        Version, VBranch, MonthTxt(DATE_TIME(2)), DATE_TIME(3), 
     &        DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
  305       FORMAT (
     &      '*SUMMARY : ',A8,A2,1X,A60,1X,A,A,5X,
     &      'DSSAT Cropping System Model Ver. ',I1,'.',I1,'.',I1,'.',
     &      I3.3,1X,A10,4X,
     &      A3," ",I2.2,", ",I4,"; ",I2.2,":",I2.2,":",I2.2)
          ENDIF

          WRITE(NOUTDS,310)
  310     FORMAT(/,
     &    '!IDENTIFIERS......................... ',
     &    'TREATMENT................ SITE INFORMATION............ ',
     &    'DATES..........................................  ',
     &    'DRY WEIGHT, YIELD AND YIELD COMPONENTS....................',
     &    '..................  ',
     &    'WATER...............................................  ',
     &    'NITROGEN......................................  ',
     &    'PHOSPHORUS............  ',
     &    'POTASSIUM.............  ',
     &    'ORGANIC MATTER..................................    ',
     &    'WATER PRODUCTIVITY..................................',
     &    '................    ',
     &    'NITROGEN PRODUCTIVITY...........')

          WRITE (NOUTDS,400)
  400     FORMAT ('@   RUNNO   TRNO R# O# C# CR MODEL    ',
     &    'TNAM                      FNAM     WSTA.... SOIL_ID...  ',
     &    '  SDAT    PDAT    EDAT    ADAT    MDAT    HDAT',
     &    '  DWAP    CWAM    HWAM    HWAH    BWAH  PWAM',
     &    '    HWUM  H#AM    H#UM  HIAM  LAIX',
     &    '  IR#M  IRCM  PRCM  ETCM  EPCM  ESCM  ROCM  DRCM  SWXM',
     &    '  NI#M  NICM  NFXM  NUCM  NLCM  NIAM  CNAM  GNAM',
     &    '  PI#M  PICM  PUPC  SPAM',
     &    '  KI#M  KICM  KUPC  SKAM',
     &    '  RECM  ONTAM   ONAM  OPTAM   OPAM   OCTAM    OCAM',
     &    '    DMPPM    DMPEM    DMPTM    DMPIM     YPPM     YPEM',
     &    '     YPTM     YPIM',
     &    '    DPNAM    DPNUM    YPNAM    YPNUM')
        ENDIF

        IF (BWAH < -1) BWAH = -9.9

        MODEL = CONTROL % MODEL

        WRITE (NOUTDS,500,ADVANCE='NO') 
     &    RUN, TRTNUM, ROTNO, ROTOPT, CRPNO, 
     &    CROP, MODEL, TITLET, FLDNAM, WSTAT, SLNO,
     &    YRSIM, YRPLT, EDAT, ADAT, MDAT, YRDOY, 
     &    DWAP, CWAM, HWAM, NINT(HWAH), NINT(BWAH*10.), PWAM

!       RUN, TRTNUM, ROTNO, ROTOPT, CRPNO, 
  500   FORMAT (I9,1X,I6,3(I3),               

!       CROP, MODEL, TITLET, FLDNAM, WSTAT, SLNO,
     &  1X,A2,1X,A8,1X,A25,1X,A8,1X,A8,1X,A10,      

!       YRSIM, YRPLT, EDAT, ADAT, MDAT, YRDOY, 
     &  6(1X,I7),

!       DWAP, CWAM, HWAM, NINT(HWAH), NINT(BWAH*10.), PWAM,
     &  1X,I5,4(1X,I7),1X,I5)

        IF     (HWUM < -.01)  THEN; FMT = '(1X,F7.0)'
        ELSEIF (HWUM < 1.)    THEN; FMT = '(1X,F7.4)'
        ELSEIF (HWUM < 10.)   THEN; FMT = '(1X,F7.3)'
        ELSEIF (HWUM < 100.)  THEN; FMT = '(1X,F7.2)'
        ELSEIF (HWUM < 1000.) THEN; FMT = '(1X,F7.1)'
        ENDIF
        WRITE (NOUTDS,FMT,ADVANCE="NO") HWUM

        WRITE (NOUTDS,'(1X,I5,1X,F7.1)',ADVANCE="NO") HNUMAM, HNUMUM

        IF (HIAM < -.01)  THEN; FMT = '(1X,F5.0)'
        ELSE                  ; FMT = '(1X,F5.3)'
        ENDIF
        WRITE (NOUTDS,FMT,ADVANCE='NO') HIAM

        WRITE (NOUTDS,503) LAIX, 
     &    IRNUM, IRCM, PRCM, ETCM, EPCM, ESCM, ROCM, DRCM, SWXM, 
     &    NINUMM, NICM, NFXM, NUCM, NLCM, NIAM, CNAM, GNAM, 
     &    PINUMM, PICM, PUPC, SPAM,        !P data
     &    KINUMM, KICM, KUPC, SKAM,        !K data
     &    RECM, ONTAM, ONAM, OPTAM, OPAM, OCTAM, OCAM,
!         Water productivity
     &    DMPPM, DMPEM, DMPTM, DMPIM, YPPM, YPEM, YPTM, YPIM,
     &    DPNAM, DPNUM, YPNAM, YPNUM

  503   FORMAT(     
                                              
!!       HNUMAM, HNUMUM, HIAM, LAIX,
!     &  1X,I5,1X,F7.1, F6.2, F6.1,    
!       LAIX,
     &  F6.1,    

!       IRNUM, IRCM, PRCM, ETCM, EPCM, ESCM, ROCM, DRCM, SWXM, 
!       NINUMM, NICM, NFXM, NUCM, NLCM, NIAM, CNAM, GNAM, 
!       PINUMM, PICM, PUPC, SPAM, 
!       KINUMM, KICM, KUPC, SKAM, RECM, 
     &  26(1X,I5),
       
!       ONTAM, ONAM, OPTAM, OPAM, OCTAM, OCAM,
     &  4(1X,I6),2(1X,I7),       
   
!       DMPPM, DMPEM, DMPTM, DMPIM, YPPM, YPEM, YPTM, YPIM
     &  4F9.1,4F9.2,

!       DPNAM, DPNUM, YPNAM, YPNUM
     &  4F9.1)
                          

        CLOSE (NOUTDS)
      ENDIF
C-------------------------------------------------------------------
C     Console output for multi-season runs:
C     Was OPBAT subroutine
C-------------------------------------------------------------------
      IF (INDEX('NQSABCGF',RNMODE) .GT. 0 .OR. NYRS .GT. 1) THEN
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
      IF (INDEX('YE',IDETO) > 0 .AND. 
     &        CROP .NE. 'WH' .AND. CROP .NE. 'BA' .AND.
     &        CROP .NE. 'CS') THEN

        ICOUNT    = EvaluateData % ICOUNT
        OLAP      = EvaluateData % OLAP
        DESCRIP   = EvaluateData % DESCRIP
        Simulated = EvaluateData % Simulated
        Measured  = EvaluateData % Measured

!       Check for simulation aborted (YRSIM == YRDOY)
!       Print out "-99"s
        IF (CONTROL % ERRCODE > 0) THEN
          Simulated = "     -99"
        ENDIF

!       Open or create Evaluate.out file
        INQUIRE (FILE = SEVAL, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = SLUN, FILE = SEVAL, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = SLUN, FILE = SEVAL, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          CALL DATE_AND_TIME (VALUES=DATE_TIME)
          WRITE (SLUN,700) EXPER, CG, ENAME, Version, VBranch,
!         WRITE (SLUN,700) MODEL, EXPER, CG, ENAME, Version,
     &        MonthTxt(DATE_TIME(2)), DATE_TIME(3), DATE_TIME(1), 
     &        DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
  700     FORMAT ('*EVALUATION : ',A8,A2,1X,A60,1X,
! 700     FORMAT ('*EVALUATION : ',A8,1X,A8,A2,1X,A60,1X,
     &    'DSSAT Cropping System Model Ver. ',I1,'.',I1,'.',I1,'.',I3.3,
     &     1X,A10,4X,A3," ",I2.2,", ",I4,"; ",I2.2,":",I2.2,":",I2.2)
        ENDIF

!       Write headers if new crop is being processed
        IF (MODEL .NE. MODEL_LAST) THEN
          WRITE(SLUN,
     &       '(/,"@RUN EXCODE        TN RN CR",80(1X,A7))')   
     &       (ADJUSTR(OLAP(I))//"S",ADJUSTR(OLAP(I))//"M",I = 1, ICOUNT)
          MODEL_LAST = MODEL
        ENDIF

!       Write evaluation data
        WRITE(SLUN,750) RUN, EXPER, CG, TRTNUM, ROTNO, CROP, 
     &            (Simulated(I), Measured(I), I= 1,ICOUNT)
  750   FORMAT(I4,1X,A8,A2,I6,I3,1X,A2,80A8)
        CLOSE(SLUN)
      ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OPSUM
C=======================================================================


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
