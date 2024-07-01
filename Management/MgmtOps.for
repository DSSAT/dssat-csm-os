C=======================================================================
C  COPYRIGHT 1998-2021 DSSAT Foundation
C                      University of Florida, Gainesville, Florida
C                      International Fertilizer Development Center
C    
C  ALL RIGHTS RESERVED
C=======================================================================
C=====================================================================
C  MgmtOps, Subroutine
C-----------------------------------------------------------------------
C  Operations Management subroutine.  Calls all operations modules:
C     AUTPLT   - automatic planting
C     AUTHAR   - automatic harvest
C     TILLAGE  - tillage
C     Chemical - Chemical applications
C     IRRIG    - irrigation applications
C  Eventually, move fertilizer placement and residue placement modules here.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  10/15/2001 CHP Written
C  04/16/2002 GH  Adjustment for sequence analysis
C  08/01/2002 CHP Merged RUNINIT and SEASINIT into INIT section
!  10/24/2005 CHP Put weather variables in constructed variable. 
!  03/03/2006 CHP Added tillage (A.Andales & WDBatchelor).
!  06/06/2006 CHP Export TIL_IRR, the irrigation amount which affects 
!                 soil dynamics (excludes drip irrigation).
!  07/14/2006 CHP Added Fert_Place (replaces FPLACE in NTRANS and 
!                 FPLACE_C in Century)
!  07/14/2006 CHP Added OM_Place (replaces RPLACE in NTRANS and 
!                 RPLACE_C in Century)
!  01/26/2023 CHP Reduce compile warnings: add EXTERNAL stmts, remove 
!                 unused variables, shorten lines. 
C=====================================================================

      SUBROUTINE MGMTOPS(CONTROL, ISWITCH, 
     &    FLOODWAT, HARVRES, SOILPROP, ST,                !Input 
     &    STGDOY, SW, WEATHER,                            !Input
     &    YREND, FERTDATA, HARVFRAC, IRRAMT,              !Output
     &    MDATE, OMADATA, TILLVALS, YRPLT)                !Output

C-----------------------------------------------------------------------
      USE ModuleDefs
      USE FloodModule
      IMPLICIT NONE
      EXTERNAL AUTHAR, AUTPLT, CHEMICAL, FERT_PLACE, IPAHAR, IRRIG, 
     &  OM_PLACE, OPMGMT, PADDY_MGMT, SUMVALS, TILLAGE, TIMDIF, YR_DOY
      SAVE

      CHARACTER*1  IHARI, IIRRI, IPLTI, ISWCHE, RNMODE
      CHARACTER*1  IDETO, ISWTIL, ISWWAT
      CHARACTER*2  CROP
      CHARACTER*6, PARAMETER :: ERRKEY = 'MGMTOP'

      INTEGER DAP, DYNAMIC   
      INTEGER YREND, IDATE, ISIM          
      INTEGER NAP, NCHEM, NLAYR, TILLNO          
      INTEGER NHAR, NTIL, RUN, TIMDIF
      INTEGER YRDIF, YRDOY, MDATE, YRO, YRPLT, YRS, YRSIM
      INTEGER HDATE(NAPPL)
      INTEGER STGDOY(20)

      REAL IRRAMT, TOTIR, TIL_IRR
      REAL HPC(NAPPL), HBPC(NAPPL), HARVFRAC(2)
      REAL, DIMENSION(NL) :: DLAYR, DUL, DS, LL, ST, SW

!     Variables added for flooded conditions
      INTEGER NBUND
      REAL FLOOD, RAIN

      !Variables needed to call IPAHAR for sequenced runs:
      INTEGER HDLAY, HLATE, HSTG(NAPPL)
      REAL    SWPLTL, SWPLTH, SWPLTD

!     Arrays which contain data for printing in SUMMARY.OUT file
      INTEGER, PARAMETER :: SUMNUM = 2
      CHARACTER*4, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

!-----------------------------------------------------------------------
      TYPE (ControlType)  CONTROL
      TYPE (SoilType)     SOILPROP
      TYPE (SwitchType)   ISWITCH
      TYPE (FloodWatType) FLOODWAT
      TYPE (FloodNType)   FLOODN
      TYPE (TillType)     TILLVALS
      TYPE (FertType)     FERTDATA
      TYPE (OrgMatAppType)OMAData
      TYPE (WeatherType)  WEATHER
      Type (ResidueType)  HARVRES  

!     Transfer values from constructed data types into local variables.
      CROP    = CONTROL % CROP  
      DYNAMIC = CONTROL % DYNAMIC 
      RUN     = CONTROL % RUN   
      YRDOY   = CONTROL % YRDOY   
      YRSIM   = CONTROL % YRSIM   
      YRDIF   = CONTROL % YRDIF
      RNMODE  = CONTROL % RNMODE

      DLAYR  = SOILPROP % DLAYR  
      DUL    = SOILPROP % DUL    
      DS     = SOILPROP % DS    
      LL     = SOILPROP % LL     
      NLAYR  = SOILPROP % NLAYR  
      
      ISWWAT = ISWITCH % ISWWAT
      ISWCHE = ISWITCH % ISWCHE
      ISWTIL = ISWITCH % ISWTIL
      IPLTI  = ISWITCH % IPLTI
      IIRRI  = ISWITCH % IIRRI
      IHARI  = ISWITCH % IHARI
      IDETO  = ISWITCH % IDETO

      FLOOD = FLOODWAT % FLOOD
      RAIN  = WEATHER  % RAIN

C***********************************************************************
C***********************************************************************
C    Input and Initialization 
C***********************************************************************
      IF (DYNAMIC .EQ. INIT) THEN
C-----------------------------------------------------------------------
C     For sequenced runs, YRDIF (the time difference in years to adjust
C       dates for the new season), is calculated based on HDATE (if 
C       IHARI = 'D') or on YRPLT
C-----------------------------------------------------------------------
      IF (RUN .EQ. 1) THEN
        YRDIF = 0
      ELSEIF (RNMODE .EQ. 'Q') THEN  
        IF (CROP .EQ. 'FA' .AND. IHARI .NE. 'D') THEN
          CALL YR_DOY(YRSIM, YRS, ISIM)
C         YRDIF based on HDATE
!         Call IPAHAR to read HDATE for YRDIF calculation.
          CALL IPAHAR(CONTROL,
     &      HPC, HBPC, HDATE, HDLAY, HLATE, HSTG,
     &      NHAR, SWPLTL, SWPLTH, SWPLTD)
          CALL YR_DOY(HDATE(1), YRO, IDATE)
          YRDIF    =  YRS - YRO
          HDATE(1) = (YRO + YRDIF) * 1000 + IDATE
          IF (HDATE(1) .LT. YRSIM) THEN
            YRDIF = YRDIF + 1
            HDATE(1) = (YRO + YRDIF) * 1000 + IDATE
          ENDIF
          CONTROL % YRDIF   = YRDIF
        ENDIF 
      ENDIF       

C-----------------------------------------------------------------------
C     Call modules to modify dates for seasonal or sequenced runs.
C-----------------------------------------------------------------------
      CALL AUTPLT (CONTROL, ISWWAT,
     &    DLAYR, DUL, FLOOD, IDETO, IPLTI, LL, ST, SW,    !Input
     &    MDATE, YRPLT)                                   !Output
C-----------------------------------------------------------------------
C     Adjust harvest dates for seasonal or sequenced runs.
C     For potato, sets harvest date.
C-----------------------------------------------------------------------
      CALL AUTHAR(CONTROL, ISWWAT,
     &    DLAYR, DUL, IDETO, IHARI, LL, STGDOY,           !Input
     &    SW, MDATE, YRPLT,                               !Input
     &    YREND, HARVFRAC, HDATE, NHAR)                   !Output
    
      IF (ISWCHE .EQ. 'Y') THEN
        CALL Chemical(CONTROL, ISWITCH, NCHEM)
      ENDIF

      CALL Fert_Place (CONTROL, ISWITCH, 
     &  DLAYR, DS, FLOOD, NLAYR, YRPLT,           !Input
     &  FERTDATA)                                 !Output

      CALL OM_Place (CONTROL, ISWITCH, 
     &    DLAYR, NLAYR, YRPLT,                            !Input
     &    OMAData)                                        !Output

      IF (INDEX('YR',ISWTIL) > 0) THEN
        CALL TILLAGE(CONTROL, ISWITCH, SOILPROP,          !Input
     &    TILLVALS, TILLNO)                               !Output
        NTIL = TILLVALS % NTIL
      ENDIF

      CALL IRRIG(CONTROL, ISWITCH,
     &    RAIN, SOILPROP, SW, MDATE, YRPLT, STGDOY,       !Input
     &    FLOODWAT, IIRRI, IRRAMT, NAP, TIL_IRR, TOTIR)   !Output

      NBUND = FLOODWAT % NBUND
!     initialize flood management variables.
      CALL PADDY_MGMT (CONTROL, ISWITCH,
     &    IRRAMT, RAIN,                                   !Input
     &    FLOOD, FLOODWAT, FLOODN)                        !Output 

      CALL OpMgmt(CONTROL, ISWITCH,
     &    FERTDATA, HARVFRAC, HARVRES, IIRRI, IRRAMT, NAP, OMADATA, 
     &    SOILPROP, TILLNO, TILLVALS, TOTIR, YRPLT)

C***********************************************************************
C***********************************************************************
C     Rate Calculations 
C***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
C-----------------------------------------------------------------------
C  Call AUTPLT subroutine if automatic planting on, determine YRPLT
C-----------------------------------------------------------------------
      IF (YRPLT < 0) THEN
        DAP = 0
      ELSE
        DAP = MAX0 (0, TIMDIF (YRPLT, YRDOY))
      ENDIF

      IF (DAP .EQ. 0 .AND. INDEX('AF', IPLTI) /= 0 .AND. CROP .NE. 'FA')
     & THEN
        CALL AUTPLT (CONTROL, ISWWAT,
     &    DLAYR, DUL, FLOOD, IDETO, IPLTI, LL, ST, SW,    !Input
     &    MDATE, YRPLT)                                   !Output
      ENDIF

      CALL Fert_Place (CONTROL, ISWITCH, 
     &  DLAYR, DS, FLOOD, NLAYR, YRPLT,           !Input
     &  FERTDATA)                                 !Output

      CALL OM_Place (CONTROL, ISWITCH, 
     &    DLAYR, NLAYR, YRPLT,                            !Input
     &    OMAData)                                        !Output

      IF (INDEX('YR',ISWTIL) > 0 .AND. NTIL .GT. 0) THEN
        CALL TILLAGE(CONTROL, ISWITCH, SOILPROP,          !Input
     &    TILLVALS, TILLNO)                               !Output
      ENDIF

      IF (INDEX('AFRDPWET',IIRRI) .GT. 0 .AND. ISWWAT .EQ. 'Y') THEN
!       Calculate irrigation depth for today
        CALL IRRIG(CONTROL, ISWITCH,
     &    RAIN, SOILPROP, SW, MDATE, YRPLT, STGDOY,       !Input
     &    FLOODWAT, IIRRI, IRRAMT, NAP, TIL_IRR, TOTIR)   !Output
        TILLVALS % TIL_IRR = TIL_IRR
      ELSE
          IRRAMT = 0.0
      ENDIF

      IF (ISWCHE .EQ. 'Y' .AND. NCHEM .GT. 0 .) THEN
        CALL Chemical(CONTROL, ISWITCH, NCHEM)
      ENDIF

C***********************************************************************
C***********************************************************************
C     Daily integration
C***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN

C-----------------------------------------------------------------------
C     Call AUTHAR subroutine to check harvest switch and
C     determine YREND
C-----------------------------------------------------------------------
!     Calculate cumulative irrigation
      IF (INDEX('AFRDPWET',IIRRI) .GT. 0 .AND. ISWWAT .EQ. 'Y') THEN
        CALL IRRIG(CONTROL, ISWITCH,
     &    RAIN, SOILPROP, SW, MDATE, YRPLT, STGDOY,       !Input
     &    FLOODWAT, IIRRI, IRRAMT, NAP, TIL_IRR, TOTIR)   !Output
      ENDIF

      IF (NBUND .GT. 0) THEN
!       Determine flood depth today.
        CALL PADDY_MGMT (CONTROL, ISWITCH,
     &    IRRAMT, RAIN,                            !Input
     &    FLOOD, FLOODWAT, FLOODN)                        !Output 
      ENDIF

      CALL AUTHAR(CONTROL, ISWWAT, 
     &    DLAYR, DUL, IDETO, IHARI, LL, STGDOY,           !Input
     &    SW, MDATE, YRPLT,                               !Input
     &    YREND, HARVFRAC, HDATE, NHAR)                   !Output

!***********************************************************************
!***********************************************************************
!     OUTPUT
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
!-----------------------------------------------------------------------
      CALL OpMgmt(CONTROL, ISWITCH,
     &    FERTDATA, HARVFRAC, HARVRES, IIRRI, IRRAMT, NAP, OMADATA, 
     &    SOILPROP, TILLNO, TILLVALS, TOTIR, YRPLT)

      IF (NBUND .GT. 0) THEN
        CALL PADDY_MGMT (CONTROL, ISWITCH,
     &    IRRAMT, RAIN,                                   !Input
     &    FLOOD, FLOODWAT, FLOODN)                        !Output 
      ENDIF

!***********************************************************************
!***********************************************************************
!     SEASEND - seasonal output and close files
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------

      CALL OM_Place (CONTROL, ISWITCH, 
     &    DLAYR, NLAYR, YRPLT,                    !Input
     &    OMAData)                                !Output

      CALL OpMgmt(CONTROL, ISWITCH,
     &    FERTDATA, HARVFRAC, HARVRES, IIRRI, IRRAMT, NAP, OMADATA, 
     &    SOILPROP, TILLNO, TILLVALS, TOTIR, YRPLT)

      IF (NBUND .GT. 0) THEN
        CALL PADDY_MGMT (CONTROL, ISWITCH,
     &    IRRAMT, RAIN,                                   !Input
     &    FLOOD, FLOODWAT, FLOODN)                        !Output 
      ENDIF

!       Store Summary.out labels and values in arrays to send to
!       OPSUM routines for printing.  Integers are temporarily 
!       saved as real numbers for placement in real array.
        LABEL(1)  = 'IR#M'; VALUE(1)  = FLOAT(NAP)
        LABEL(2)  = 'IRCM'; VALUE(2)  = TOTIR

!       Send labels and values to OPSUM
        CALL SUMVALS (SUMNUM, LABEL, VALUE) 

C***********************************************************************
C***********************************************************************
C     END OF DYNAMIC IF CONSTRUCT
C***********************************************************************
      ENDIF
C***********************************************************************

      RETURN
      END SUBROUTINE MGMTOPS

C=======================================================================

C=======================================================================
C  OpMgmt, Subroutine, C.H.Porter 
C  Generates output for Management Operations Module.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  08/01/2002 CHP Written
!  03/04/2003 CHP Disable routine for DS4 release - to be enabled when
!                   additional management operations can be added to
!                   output file (fertilizer, residue, tillage, etc.)
!  06/01/2006 CHP Include alternate file format which prints a summary
!                   line of output on the day of any management event.
!                 Daily output file:   MgmtOps.OUT
!                 Summary output file: MgmtEvent.OUT
C-----------------------------------------------------------------------
C  Called from:   MgmtOps
C  Calls:         None
C=======================================================================
      SUBROUTINE OpMgmt(CONTROL, ISWITCH,
     &    FERTDATA, HARVFRAC, HARVRES, IIRRI, IRRAMT, NAP, OMADATA, 
     &    SOILPROP, TILLNO, TILLVALS, TOTIR, YRPLT)

!-----------------------------------------------------------------------
      USE ModuleDefs
      USE ModuleData
      USE SumModule
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, NAILUJ, TIMDIF, YR_DOY
      SAVE

      CHARACTER*1  ISWCHE, IDETW, ISWTIL, IIRRI, RNMODE
      CHARACTER*1  IDETR, IFERI, IRESI, ISWNIT, ISWPHO, ISWPOT
      CHARACTER*2  CROP
      CHARACTER*3  RMON
      CHARACTER*6,  PARAMETER :: ERRKEY = 'OPMGMT'
      CHARACTER*10  iSTNAME
      CHARACTER*11, PARAMETER :: OUTM = 'MgmtOps.OUT'
      CHARACTER*12, Date_Txt
      CHARACTER*13, PARAMETER :: OUTM2= 'MgmtEvent.OUT'

      INTEGER DAP, DAS, DLUN, DLUN2, DOY, DYNAMIC, ERRNUM, FROP, I
      INTEGER NDAY, RUN, YEAR, YRDOY, YRPLT
      INTEGER L, NAP, TIMDIF, NAPFER(NELEM), NAPRes
      INTEGER TILLNO, TILDATE, ISH_date
      INTEGER iSTAGE, iSTGDOY


      REAL BDAVG3, CUMDEP, IRRAMT, TILDEP, DEPIR, ISH_wt
      REAL TotAmtN, TotAmtP, TotAmtK, TOTIR, TotResWt, SurfRes, RootRes
      REAL HARVFRAC(2)
      REAL, DIMENSION(NELEM) :: AMTFER, CumRESE
      REAL, DIMENSION(NL) :: BD, DLAYR

      LOGICAL DPRINT, FEXIST

C-----------------------------------------------------------------------
C     Define constructed variable types based on definitions in
C     ModuleDefs.for.
      TYPE (ControlType)  CONTROL
      TYPE (SwitchType)   ISWITCH
      TYPE (TillType)     TILLVALS
      TYPE (SoilType)     SOILPROP
      TYPE (FertType)     FERTDATA
      TYPE (OrgMatAppType)OMAData
      Type (ResidueType)  HARVRES

      CROP    = CONTROL % CROP 
      DAS     = CONTROL % DAS 
      DYNAMIC = CONTROL % DYNAMIC 
      FROP    = CONTROL % FROP  
      RNMODE  = CONTROL % RNMODE   
      RUN     = CONTROL % RUN    
      YRDOY   = CONTROL % YRDOY   

      IDETR   = ISWITCH % IDETR
      IFERI   = ISWITCH % IFERI
      IRESI   = ISWITCH % IRESI
      ISWCHE  = ISWITCH % ISWCHE
      IDETW   = ISWITCH % IDETW
      ISWTIL  = ISWITCH % ISWTIL

      ISWNIT = ISWITCH % ISWNIT
      ISWPHO = ISWITCH % ISWPHO
      ISWPOT = ISWITCH % ISWPOT

!***********************************************************************
!***********************************************************************
!     Input and Initialization
!***********************************************************************
      IF (DYNAMIC .EQ. INIT) THEN
!-----------------------------------------------------------------------
C     Determine whether there will be anything to print out in
C         daily 'Managmnt.out' file.
C-----------------------------------------------------------------------
      DPRINT = .FALSE.

      IF (IDETR == 'Y') THEN
!       If irrigation apps, trigger printout  
        IF ((INDEX('AFPWRDET',IIRRI) .GT. 0) .OR.

!       If Fertilizer applications are activated
     &    (INDEX('AFRFD',IFERI) .GT. 0) .OR.

!       If Organic matter applications are activated
     &    (INDEX('ARFD',IRESI) .GT. 0) .OR.

!       If Tillage apps, trigger printout  
     &    (INDEX('YR',ISWTIL) > 0) .OR. 

!       If Chemical apps, trigger printout      
     &    (ISWCHE .EQ. 'Y')) THEN 

          DPRINT = .TRUE.
        ENDIF
      ENDIF

!     If daily printout is needed, open file.
      IF (DPRINT) THEN
        CALL GETLUN('OUTM', DLUN)
        INQUIRE (FILE = OUTM, EXIST = FEXIST)
        IF (FEXIST) THEN      
!         MgmtOps.out file has already been created for this run.
          OPEN (UNIT=DLUN, FILE=OUTM, STATUS='OLD',
     &      IOSTAT = ERRNUM, POSITION='APPEND')
        ELSE                  
!         Get unit number for daily and seasonal irrigation output files
          OPEN (UNIT=DLUN, FILE=OUTM, STATUS='NEW',
     &      IOSTAT = ERRNUM)
          WRITE(DLUN,'("*MANAGEMENT OPERATIONS DAILY OUTPUT FILE")')
        ENDIF

        CALL HEADER(SEASINIT, DLUN, RUN)
        WRITE(DLUN,"(/,'@YEAR DOY   DAS   DAP')",ADVANCE='NO')
      
!!       If Chemical apps, add headers      
!        IF (ISWCHE .EQ. 'Y' .AND. NCHEM .GT. 0) THEN
!          WRITE(DLUN,"('  CH#C  CHAC')", ADVANCE=NO) 
!        ENDIF 

!       If irrigation apps, add headers  
        IF (INDEX('AFPWRDET',IIRRI) .GT. 0) THEN
          WRITE(DLUN,'(A)',ADVANCE='NO') '  IR#C  IRRC'
        ENDIF 

!       If Tillage apps, add headers  
        IF (INDEX('YR',ISWTIL) > 0) THEN
          WRITE(DLUN,'(A)',ADVANCE='NO') '  TL#C  TLDD DLYR3   TLBD'
        ENDIF

!       If Fertilizer apps, add headers  
        IF (INDEX('ARFD',IFERI) .GT. 0) THEN
          DO I = 1, NELEM
            SELECT CASE (I)
              CASE (1)
                IF (ISWNIT=='Y') THEN 
                  WRITE(DLUN,"(A)",ADVANCE='NO') '  NAPC  NICM'
                 ENDIF
              CASE (2)
                IF (ISWPHO .NE. 'N') THEN
                  WRITE(DLUN,"(A)",ADVANCE='NO') '  PAPC  PICM'
                ENDIF
              CASE (3)
                IF (ISWPOT == 'Y') THEN
                  WRITE(DLUN,"(A)",ADVANCE='NO') '  KAPC  KICM'
                ENDIF
            END SELECT
          ENDDO
        ENDIF

!       If Organic Matter apps, add headers  
        IF (INDEX('ANRFD',IRESI) .GT. 0) THEN
          WRITE(DLUN,'(A)',ADVANCE='NO') '  R#AC RESNC RESPC'
        ENDIF
      ENDIF

!       ----------------------------------------------------------------
      IF (IDETR == 'Y') THEN
!       Management event output file
        CALL GETLUN('OUTM2', DLUN2)
        INQUIRE (FILE = OUTM2, EXIST = FEXIST)
        IF (FEXIST) THEN      
          !MgmtOps.out file has already been created for this run.
          OPEN (UNIT=DLUN2, FILE=OUTM2, STATUS='OLD',
     &      IOSTAT = ERRNUM, POSITION='APPEND')
        ELSE                  
!         Get unit number for daily and seasonal irrigation output files
          OPEN (UNIT=DLUN2, FILE=OUTM2, STATUS='NEW',
     &      IOSTAT = ERRNUM)
          WRITE(DLUN2,'("*MANAGEMENT OPERATIONS EVENT SUMMARY")')
        ENDIF

        IF (INDEX('FQNY',RNMODE) <= 0 .OR. RUN == 1) THEN
          CALL HEADER(SEASINIT, DLUN2, RUN)  
        ENDIF

        WRITE(DLUN2,'(A,A)')
     &    "!----------------------------------------------",
     &    "-----------------------------------------------"

        WRITE(DLUN2,50)
   50   FORMAT("@RUN Date........  DOY",
     &   "    DAS    DAP  CR  Stage         Operation       Quantities")
        WRITE(DLUN2,'(A,A)')
     &    "!----------------------------------------------",
     &    "-----------------------------------------------"
      ENDIF

!***********************************************************************
!***********************************************************************
!     Daily Output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN  
C-----------------------------------------------------------------------
C  Generate output for file MgmtOps.OUT
C-----------------------------------------------------------------------
      IF (DPRINT) THEN

        IF (YRPLT .GT. 0) THEN
          DAP = MAX(0,TIMDIF(YRPLT,YRDOY))
        ELSE
          DAP = 0
        ENDIF
        CALL YR_DOY(YRDOY, YEAR, DOY) 

        IF ((DYNAMIC .EQ. OUTPUT .AND. MOD(DAS,FROP) .EQ. 0) .OR.
     &       DAS == 1) THEN 
          WRITE(DLUN,'(/,1X,I4,1X,I3.3,2(1X,I5))',ADVANCE='NO') 
     &                    YEAR, DOY, DAS, DAP
      
C-----------------------------------------------------------------------
!!         If Chemical apps, print data      
!          IF (ISWCHE .EQ. 'Y' .AND. NCHEM .GT. 0) THEN
!            WRITE(DLUN,"('  CH#C  CHAC')", ADVANCE=NO) 
!          ENDIF 

!-----------------------------------------------------------------------
!         If irrigation apps, print data  
          IF (INDEX('AFPWRDET',IIRRI) .GT. 0) THEN
            WRITE(DLUN,"(2(1X,I5))",ADVANCE='NO') NAP, NINT(TOTIR)
          ENDIF     

!-----------------------------------------------------------------------
!         If Tillage apps, print data   
          IF (INDEX('YR',ISWTIL) > 0) THEN
            TILDATE = TILLVALS % TILDATE
            IF (YRDOY .EQ. TILDATE)  THEN
              TILDEP  = TILLVALS % TILDEP
            ELSE
              TILDEP = 0.0
            ENDIF

!           Calculate alternate layer depths corresponding to tilled BD's
            BD    = SOILPROP % BD
            DLAYR = SOILPROP % DLAYR
            BDAVG3 = 0.0
            CUMDEP = 0.0
            DO L = 1, 3
              BDAVG3 = BDAVG3 + BD(L) * DLAYR(L)
              CUMDEP = CUMDEP + DLAYR(L)
            ENDDO
            BDAVG3 = BDAVG3 / CUMDEP
            WRITE(DLUN,"(3(1X,I5),F7.3)",ADVANCE='NO') 
     &            TILLNO, NINT(TILDEP), NINT(CUMDEP), BDAVG3
          ENDIF !End Tillage printout

C-----------------------------------------------------------------------
!         If Fertilizer apps, print data   
          IF (INDEX('ARFD',IFERI) .GT. 0) THEN
            AMTFER  = FERTDATA % AMTFER
            NAPFER  = FERTDATA % NAPFER
            DO I = 1, NELEM
              SELECT CASE (I)
                CASE (1)
                  IF (ISWNIT == 'Y') WRITE(DLUN,"(2I6)",ADVANCE='NO') 
     &                NAPFER(I), NINT(AMTFER(I))
                CASE (2)
                  IF (ISWPHO .NE. 'N') WRITE(DLUN,"(2I6)",ADVANCE='NO') 
     &                NAPFER(I), NINT(AMTFER(I))
                CASE (3)
                  IF (ISWPOT == 'Y') WRITE(DLUN,"(2I6)",ADVANCE='NO') 
     &                NAPFER(I), NINT(AMTFER(I))
              END SELECT
            ENDDO
          ENDIF 

!-----------------------------------------------------------------------
!         If Organic matter apps, print data   
          IF (INDEX('ANRFD',IRESI) .GT. 0) THEN
            CumRESE = OMADATA % CumRESE
            NAPRes  = OMADATA % NAPRes
            WRITE(DLUN,'(3I6)',ADVANCE='NO') 
     &              NAPRes, NINT(CumRESE(N)), NINT(CumRESE(P))
          ENDIF
        ENDIF
      ENDIF

!-----------------------------------------------------------------------
!       Event summary printout MgmtEvent.OUT
!-----------------------------------------------------------------------
      IF (IDETR == 'Y') THEN
        CALL NAILUJ(DOY,YEAR,RMON,NDAY)
        Write(Date_Txt,'(A3,I3,", ",I4)') RMON, NDAY, YEAR 

!       Planting
        IF (YRDOY == YRPLT .AND. CROP .NE. 'FA') THEN
          WRITE(DLUN2,98) RUN, Date_Txt, DOY, DAS, DAP, CROP, 
     &          "Planting       "
   98     FORMAT(I4,1X,A12,2X,I3.3,2(1X,I6),2X,A2,T57,A)
        ENDIF

!     Remove this - it gives some strange results for some crops
!     chp 2018-12-18
!!       Physiological Maturity
!        IF (MDATE == YRDOY) THEN
!          WRITE(DLUN2,100) RUN, Date_Txt, DOY, DAS, DAP, CROP, 
!     &          "Phys. Maturity "
!        ENDIF

!       Retrieve current Stage, STGDOY and STNAME
        CALL GET('PLANT','iSTAGE', iSTAGE)
        CALL GET('PLANT','iSTGDOY', iSTGDOY)
        CALL GET('PLANT','iSTNAME', iSTNAME)

        IF (YRDOY == iSTGDOY) THEN
          WRITE(DLUN2,102) RUN, Date_Txt, DOY, DAS, DAP, CROP, 
     &         iSTAGE, iSTNAME
  102     FORMAT(I4,1X,A12,2X,I3.3,2(1X,I6),2X,A2,2X,I2.2,1X,A)
        ENDIF

!       Fertilizer application
        IF (YRDOY == FertData % FertDay) THEN
          WRITE(DLUN2,104,ADVANCE='NO') RUN, Date_Txt, DOY, DAS, DAP, 
     &         CROP, "Fertilizer     "
  104     FORMAT(I4,1X,A12,2X,I3.3,2(1X,I6),2X,A2,T57,A)
          IF (ISWNIT == 'Y') THEN
            TotAmtN = SUM(FertData%ADDSNH4) 
     &              + SUM(FertData%ADDSNO3) 
     &              + SUM(FertData%ADDUREA)
            WRITE(DLUN2,'(F7.0,A)',ADVANCE='NO') TotAmtN, " kg[N]/ha"
          ENDIF
          IF (ISWPHO .NE. 'N') THEN
            TotAmtP = SUM(FertData%ADDSPi)
            WRITE(DLUN2,'(A,F7.0,A)',ADVANCE='NO')";",TotAmtP,
     &                " kg[P]/ha"
          ENDIF
          IF (ISWPOT == 'Y') THEN
            TotAmtK = SUM(FertData%ADDSKi)
            WRITE(DLUN2,'(A,F7.0,A)',ADVANCE='NO')";",TotAmtK, 
     &                " kg[K]/ha"
          ENDIF
          WRITE(DLUN2,'(" ")')
        ENDIF

!       Organic matter application
        IF (YRDOY == OMAData % ResDat) THEN
          TotResWt = SUM(OMAData%ResWt)
          WRITE(DLUN2,105) RUN, Date_Txt, DOY, DAS, DAP, CROP, 
     &          "Organic matter ", 
     &      NINT(TotResWt), " kg[DM]/ha"
  105     FORMAT(I4,1X,A12,2X,I3.3,2(1X,I6),2X,A2,T57,A,I7,A)
        ENDIF

!       Tillage
        IF (YRDOY == TILLVALS % TILDATE) THEN
          WRITE(DLUN2,106) RUN, Date_Txt, DOY, DAS, DAP, CROP, 
     &          "Tillage        ",
     &        TILLVALS % TILDEP, " cm"
  106     FORMAT(I4,1X,A12,2X,I3.3,2(1X,I6),2X,A2,T57,A,F7.1,A,F7.1,A)
        ENDIF

!       Irrigation
        IF (IRRAMT > 1.E-6) THEN
!         Total irrig amt today (mm) (includes losses)
          CALL Get('MGMT','DEPIR', DEPIR)   
          WRITE(DLUN2,110) RUN, Date_Txt, DOY, DAS, DAP, CROP, 
     &          "Irrigation     ",
     &        DEPIR, " mm"
  110     FORMAT(I4,1X,A12,2X,I3.3,2(1X,I6),2X,A2,T57,A,F7.1,A)
        ENDIF

!       In-season harvest or mow operation
        CALL GET('MHARVEST','ISH_date',ISH_date)
        CALL GET('MHARVEST','ISH_wt',  ISH_wt)
        IF (YRDOY == ISH_date) THEN
!         In-season harvest
          WRITE(DLUN2,120) RUN, Date_Txt, DOY, DAS, DAP, CROP, 
     &          "Harvest        ",
     &        ISH_wt, " kg/ha"
  120     FORMAT(I4,1X,A12,2X,I3.3,2(1X,I6),2X,A2,T57,A,F7.0,A)
        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
      IF (IDETR == 'Y') THEN

!       End of simulation-season harvest or mow operation
        CALL GET('MHARVEST','ISH_date',ISH_date)
        CALL GET('MHARVEST','ISH_wt',  ISH_wt)
        IF (YRDOY == ISH_date) THEN
!         In-season harvest
          WRITE(DLUN2,220) RUN, Date_Txt, DOY, DAS, DAP, CROP, 
     &          "Harvest        ",
     &        ISH_wt, " kg/ha"
  220     FORMAT(I4,1X,A12,2X,I3.3,2(1X,I6),2X,A2,T57,A,F7.0,A)
        ENDIF

          WRITE(DLUN2,220) RUN, Date_Txt, DOY, DAS, DAP, CROP, 
     &          "End simulation "

!       Harvested amounts
        IF (CROP .NE. 'FA') THEN
          WRITE(DLUN2,312) HARVFRAC(1)*100., " % yield harvested",
     &            SumDat % HWAH, " kg/ha"
          WRITE(DLUN2,312) HARVFRAC(2)*100., " % by-product harv",
     &            SumDat % BWAH, " kg/ha"
  312     FORMAT(T45,F6.1,A,T72,F7.0,A)

!         Harvest residues
          IF (INDEX('FQ',RNMODE) > 0 .AND. CROP /= 'FA') THEN
            SurfRes = HARVRES % ResWt(0)
            IF (SurfRes > 0) THEN
              WRITE(DLUN2,400) "Surface residue carryover", SurfRes,
     &            " kg/ha"
  400         FORMAT(T46,A,T72,F7.0,A)
            ENDIF
        
            RootRes = SUM(HARVRES % ResWt) - HARVRES % ResWt(0)
            IF (RootRes > 0) THEN
              WRITE(DLUN2,400) "Root residue carryover", RootRes, 
     &            " kg/ha"
            ENDIF
          ENDIF
!        ELSE
!          WRITE(DLUN2,500) RUN, Date_Txt, DOY, DAS, DAP, CROP, 
!     &            "End Sim        "
!  500     FORMAT(I4,1X,A12,2X,I3.3,2(1X,I6),2X,A2,2X,A,3(F7.0,A))
        ENDIF

        WRITE(DLUN2,'(" ")')
        CLOSE (DLUN)
        CLOSE (DLUN2)
      ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OpMgmt
!***********************************************************************
