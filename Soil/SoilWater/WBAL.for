C=====================================================================
C  WBAL, Subroutine, Gerrit Hoogenboom
C  Seasonally: Provides output Water balance.  Prints file SoilWatBal.OUT
!  Data is obtained from WATBAL, SPAM and IRRIG modules daily.  
!  Data from SPAM and IRRIG are sent via GETPUT routines.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/20/1996 GH  Written
C  09/02/1999 CHP Added daily soil water content check
C  08/20/2002 GH  Modified for Y2K
!  02/22/2006 CHP Added tiledrain.
!  03/06/2006 CHP Added mulch layer effects on evaporation and infiltration.
!  01/11/2007 CHP Changed GETPUT calls to GET and PUT
!  04/02/2025 chp add SWBalSum.OUT
!  04/10/2025 chp merge 1D and 2D versions of this file
!-----------------------------------------------------------------------
!  Called by: WATBAL
C=====================================================================
      SUBROUTINE Wbal(CONTROL, ISWITCH, 
     &    CRAIN, DRAIN, FLOODWAT, netLatFlow,
     &    IRRAMT, MULCH, RAIN, RUNOFF, SNOW,  
     &    TDFC, TDFD, TDRAIN, TRUNOF, TSW)
!     ------------------------------------------------------------------
      USE ModuleDefs 
      USE ModuleData
      USE FloodModule
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, YR_DOY, INCDAT
      SAVE

      TYPE (ControlType), INTENT(IN) :: CONTROL
      TYPE (SwitchType),  INTENT(IN) :: ISWITCH
      TYPE (FloodWatType),INTENT(IN) :: FLOODWAT
      TYPE (MulchType),   INTENT(IN) :: MULCH
      REAL, INTENT(IN) :: CRAIN, DRAIN, netLatFlow, IRRAMT, RAIN, 
     &  RUNOFF, SNOW, TDFC, TDFD, TDRAIN, TRUNOF, TSW

      CHARACTER*1 IDETL, IDETW, ISWWAT, MEINF
      CHARACTER*14, PARAMETER :: SWBAL = 'SoilWatBal.OUT'
      CHARACTER*12, PARAMETER :: SWBSUM = 'SWBalSum.OUT'
      INTEGER DAS, DOY, DYNAMIC, INCDAT, LUNWBL, LUNWBLS
      INTEGER RUN, YEAR, YRSIM, YRDOY, NBUND
      INTEGER YR1, DY1, YR2, DY2

      REAL CEO, CEP, CES, CEVAP, EFFIRR
      REAL TOTIR, CumNetLatFlow, WBALAN, TSWINI

!     Daily balance
      REAL ES, EF, TOTEFFIRR
      REAL EP, TSWY    !, INFILT
      REAL CEF, FLOOD, FLOODI, TOTBUNDRO, FRUNOFF, FLOODY
      REAL SNOWI, SNOWY, CUMWBAL
      REAL MULCHWAT, MWI, MWY
      REAL CUMRESWATADD, RESWATADD_T
      REAL CUMMULEVAP, MULCHEVAP

      LOGICAL FEXIST, Sim2D

!     ------------------------------------------------------------------
      IDETW   = ISWITCH % IDETW
      IDETL   = ISWITCH % IDETL
      ISWWAT  = ISWITCH % ISWWAT

      IF (IDETW .EQ. 'N' .OR. ISWWAT .EQ. 'N') RETURN
!     ------------------------------------------------------------------
      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM
      DAS     = CONTROL % DAS
      MEINF   = ISWITCH % MEINF

      EF        = FLOODWAT % EF    
      CEF       = FLOODWAT % CEF    
      FLOOD     = FLOODWAT % FLOOD    
      IF (FLOOD .LT. 0.0) FLOOD = 0.0
      NBUND     = FLOODWAT % NBUND    
      TOTBUNDRO = FLOODWAT % TOTBUNDRO
      FRUNOFF   = FLOODWAT % FRUNOFF

      MULCHWAT  = MULCH % MULCHWAT
      MULCHEVAP = MULCH % MULCHEVAP
      RESWATADD_T = MULCH % NEWMULCHWAT

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      RUN     = CONTROL % RUN
      Sim2D   = .FALSE.

      TSWINI = TSW
      TSWY   = TSWINI
      FLOODI = FLOOD
      FLOODY = FLOOD

      SNOWI = SNOW
      SNOWY = SNOW
      MWI   = MULCHWAT
      MWY   = MULCHWAT

      CUMWBAL = 0.0
      CUMRESWATADD = 0.0
      CUMMULEVAP = 0.0
      CumNetLatFlow = 0.0

!--------------------------------------------------------------
!     Initialize SoilWatBal.OUT file
      CALL GETLUN('SWBAL', LUNWBL)
      INQUIRE (FILE = SWBAL, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = LUNWBL, FILE = SWBAL, STATUS = 'OLD',
     &    POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = LUNWBL, FILE = SWBAL, STATUS = 'NEW')
        WRITE(LUNWBL,'("*WATER BALANCE OUTPUT FILE")')
      ENDIF

      CALL HEADER(SEASINIT, LUNWBL, RUN)

      IF (INDEX('AD',IDETL) > 0) THEN
!       Write header for daily output
        WRITE (LUNWBL,1120)
 1120   FORMAT(/,'@YEAR DOY   DAS',
     & '    SWTD    FWTD   SNOWD   MWTD',                   !State vars
     & '   IRRD   PRED',                                    !Inflows
     & '  RESAD   LFLOD',                                   !Inflows
     & '  MEVAP',                                           !Outflows
     & '   DRND   ROFD   FROD   ESAD   EPAD   EFAD   TDFD', !Outflows
     & '    WBAL   CUMWBAL')                                !Balance

        CALL YR_DOY(INCDAT(YRDOY,-1), YEAR, DOY) 
        WRITE (LUNWBL,1300) YEAR, DOY, DAS, 
     &    (TSWINI * 10.), FLOOD, SNOW, MULCHWAT,      !State variables
     &    0.0, 0.0,                                   !Inflows
     &    0.0,                                        !Inflows
     &    0.0,MULCHEVAP,                              !Outflows
!     &    INFILT,                 !Exchange between flood and soil water
     &    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,          !Outflows
     &    0.0, 0.0                                    !Balance

      ENDIF

!--------------------------------------------------------------
!     Initialize SWBalSum.OUT file
!     IF (INDEX('AD',IDETL) > 0) THEN
        CALL GETLUN('SWBSUM', LUNWBLS)
        INQUIRE (FILE = SWBSUM, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = LUNWBLS, FILE = SWBSUM, STATUS = 'OLD',
     &      POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = LUNWBLS, FILE = SWBSUM, STATUS = 'NEW')
          WRITE(LUNWBLS,'("*WATER BALANCE SUMMARY OUTPUT FILE")')
        
          CALL HEADER(SEASINIT, LUNWBLS, RUN)

!         Write header for daily output
          WRITE (LUNWBLS,1130)
 1130     FORMAT(/,'!                           ',
     &    '|----- Initial Water Content (mm) -----',
     &    '|------ Final Water Content (mm) -------',
     &    '|-------- Water Additions (mm) ---------',
     &    '|--------------- Water Losses (mm) ---------------',
     &    '|-----------|',/,

     &    '!                           ',
     &    '|    Soil     Mulch     Flood      Snow',
     &    '      Soil     Mulch     Flood      Snow',
     &    '    Precip     Irrig   LatFlow   Residue',
     &    '      Evap  Transpir    Runoff  Drainage   Tile Dr',
     &    '     Balance',/,

     &    '@  Run FILEX          TN CR',
     &    '     SWTDi     MWTDi     FWTDi    SNOWDi',
     &    '      SWTD      MWTD      FWTD     SNOWD',
     &    '      PRCM      IRRC     LATFC     RESAC',
     &    '      ESCM      EPCM      ROCM      DRCM      TDFC',
     &    '     CUMWBAL')

        ENDIF
!     ENDIF

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
!-----------------------------------------------------------------------
      IF (INDEX('AD',IDETL) > 0) THEN

!       Transfer data from constructed variable to local variables
        CALL Get('SPAM','CEO',CEO)
        CALL Get('SPAM','CEP',CEP)
        CALL Get('SPAM','CES',CES)
        CALL Get('SPAM','EP', EP)
        CALL Get('SPAM','ES', ES)
        CALL Get('MGMT','TOTIR', TOTIR)
        CALL Get('MGMT','EFFIRR',EFFIRR)
        CALL YR_DOY(YRDOY, YEAR, DOY) 

!       Change in storage = Inflows - Outflows
!       Balance = Inflows - Outflows - Change in storage
        WBALAN = 
     &         + IRRAMT + RAIN                !Inflows
     &         + RESWATADD_T                  !Inflows
     &         + netLatFlow   !Net lateral flow (could be negative)
     &         - MULCHEVAP                    !Outflows
     &         - DRAIN - RUNOFF - FRUNOFF     !Outflows
     &         - ES - EP - EF - (TDFD*10.)    !Outflows
     &         - (TSW * 10.) + (TSWY * 10.)   !Change in soil water 
     &         - FLOOD       + FLOODY         !Change in flood water 
     &         - SNOW        + SNOWY          !Change in snow accum.
     &         - MULCHWAT    + MWY            !Change in mulch water

        CUMWBAL = CUMWBAL + WBALAN

        WRITE (LUNWBL,1300) YEAR, DOY, DAS
     &    ,(TSW * 10.), FLOOD, SNOW, MULCHWAT         !State variables
     &    ,IRRAMT, RAIN                               !Inflows
     &    ,RESWATADD_T                                !Inflows
     &    , netLatFlow                      !net lateral flow
     &    ,MULCHEVAP                                  !Outflows
!!     &    ,INFILT                 !Exchange between flood and soil water
     &    ,DRAIN, RUNOFF, FRUNOFF, ES, EP, EF, TDFD*10. !Outflows
     &    ,WBALAN, CUMWBAL                             !Balance

 1300   FORMAT(1X,I4,1X,I3.3,1X,I5
     &      ,3F8.2,F7.2
     &      ,3F7.2
     &      ,F8.2       !Lateral flow
     &      ,8F7.2
     &      ,F8.3,F10.3 !Balances
     &      )

        !Save values for comparison tomorrow
        TSWY   = TSW
        FLOODY = FLOOD
        SNOWY  = SNOW
        MWY    = MULCHWAT
      ENDIF
      
      CUMMULEVAP   = CUMMULEVAP + MULCHEVAP
      CUMRESWATADD = CUMRESWATADD + RESWATADD_T
      CumNetLatFlow = CumNetLatFlow + netLatFlow

!***********************************************************************
!***********************************************************************
!     SEASEND - Seasonal output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
      IF (INDEX('0',IDETL) > 0) RETURN

      CALL YR_DOY(YRSIM, YR1, DY1)
      CALL YR_DOY(YRDOY, YR2, DY2)

      CALL Get('SPAM','CEO',CEO)
      CALL Get('SPAM','CEP',CEP)
      CALL Get('SPAM','CES',CES)
      CALL Get('SPAM','EP', EP)
      CALL Get('SPAM','ES', ES)

      CALL Get('MGMT','TOTEFFIRR',TOTEFFIRR)  !Total effective irrig

      WRITE (LUNWBL,320)
  320 FORMAT(/,'!',5X,'WATER BALANCE PARAMETERS',
     &       /,'!',5X,'========================',T48,'--mm--')
      WRITE (LUNWBL,400)
     &                   YR1, DY1, TSWINI*10,
     &                   YR2, DY2, TSW*10, 
     &                   TOTEFFIRR,
     &                   CRAIN, CUMRESWATADD, 
     &                   CumNetLatFlow,
     &                   TDRAIN, TDFC*10., TRUNOF, CUMMULEVAP,
     &                   CES, CEP, CES+CEP, CEO
  400 FORMAT(
     &    /,'!',5X,'Soil H20 (start) on Year/day',I5,'/',I3.3,T44,F10.2,
     &    /,'!',5X,'Soil H20 (final) on Year/day',I5,'/',I3.3,T44,F10.2,
     &    /,'!',5X,'Effective Irrigation',                    T44,F10.2,
     &    /,'!',5X,'Precipitation',                           T44,F10.2,
     &    /,'!',5X,'Water added with new mulch',              T44,F10.2,
     &    /,'!',5X,'Net lateral flow',                        T44,F10.2,
     &    /,'!',5X,'Drainage',                                T44,F10.2,
     &    /,'!',5X,'Tiledrain flow',                          T44,F10.2,
     &    /,'!',5X,'Runoff',                                  T44,F10.2,
     &    /,'!',5X,'Mulch evaporation',                       T44,F10.2,
     &    /,'!',5X,'Soil Evaporation',                        T44,F10.2,
     &    /,'!',5X,'Transpiration',                           T44,F10.2,
     &    /,'!',5X,'Total evapotranspiration',                T44,F10.2,
     &    /,'!',5X,'Total potential evapotranspiration',      T44,F10.2)

      WBALAN = (TSWINI * 10.) - (TSW * 10.) !Change in water content
     &       + TOTEFFIRR + CRAIN + CUMRESWATADD           !Inflows
     &       + CumNetLatFlow                  !Lateral flow
     &       - CUMMULEVAP                                 !Outflows
     &       - TDRAIN - TRUNOF - CES - CEP - (TDFC*10.)   !Outflows

      IF (SNOW > 0.0 .OR. SNOWI > 0.0) THEN
        WRITE(LUNWBL, 420) SNOWI, SNOW
  420   FORMAT(/,'!',5X,'Initial snow accumulation ', T44, F10.2,
     &         /,'!',5X,'Final snow accumulation ',   T44, F10.2)
        WBALAN = WBALAN - SNOW + SNOWI
      ENDIF

      IF (INDEX('RSM',MEINF) > 0) THEN   
        WRITE(LUNWBL, 430) MWI, MULCHWAT
  430   FORMAT(/,'!',5X,'Initial mulch water content ', T44, F10.2,
     &       /,'!',5X,'Final mulch water content ',   T44, F10.2)
        WBALAN = WBALAN - MULCHWAT + MWI
      ENDIF

      IF (NBUND .GT. 0) THEN
!        WRITE (LUNWBL,450) FLOODI, FLOOD, CINF, CEF, TOTBUNDRO
        WRITE (LUNWBL,450) FLOODI, FLOOD, CEF, TOTBUNDRO
  450   FORMAT(
     &       /,'!',5X,'Initial flood depth     ',T44,F10.2,
     &       /,'!',5X,'Final flood depth       ',T44,F10.2,
!     &       /,'!',5X,'Cumulative infiltration ',T44,F10.2,
     &       /,'!',5X,'Flood pool evaporation  ',T44,F10.2,
     &       /,'!',5X,'Runoff over bund        ',T44,F10.2)
        WBALAN = WBALAN - FLOOD + FLOODI - CEF - TOTBUNDRO
      ENDIF

      WRITE  (LUNWBL,500) WBALAN
  500 FORMAT(/,'!',5X,'Final Balance ',T42,F12.3,/)

      CLOSE(LUNWBL)

!--------------------------------------------------------------
!     IF (INDEX('AD',IDETL) > 0) THEN
!       Write seasonal summary to SWBalSum.OUT
        CALL GET('SPAM', 'CEVAP', CEVAP)
        
        WRITE (LUNWBLS,'(I6,1X,A12,1X,I4,1X,A2,17F10.2,F12.4)')
     &    RUN, CONTROL % FILEX, CONTROL % TRTNUM, CONTROL % CROP, 
     &    TSWINI*10, MWI, FLOODI, SNOWI,
     &    TSW*10, MULCHWAT, FLOOD, SNOW,  
     &    CRAIN, TOTEFFIRR, CumNetLatFlow, CUMRESWATADD, 
     &    CEVAP, CEP, TRUNOF, TDRAIN, TDFC*10., 
     &    WBALAN
!     ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE Wbal
C=======================================================================


C=====================================================================
!     WBAL VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! CEO      Cumulative potential evapotranspiration (mm)
! CEP      Cumulative transpiration (mm)
! CES      Cumulative evaporation (mm)
! CRAIN    Cumulative precipitation (mm)
! DEFICIT  Amount by which the allowable minimum soil water content in top 
!            layer exceeds the actual calculated soil water content (cm3/cm3)
! EFFIRR   Irrigation application efficiency (cm/cm)
! ES       Actual soil evaporation rate (mm/d)
! EXPER    Experiment code (prefix of input files) 
! FIRST    Indicates first call to subroutine (true or false)
! LL(L)    Volumetric soil water content in soil layer L at lower limit
!            (cm3/cm3)
! LUNWARN  Logical unit number for Warning.OUT file 
! LUNWBL   Logical unit number for WBAL.OUT file 
! NL       Maximum number of soil layers = 20 
! SAT(L)   Volumetric soil water content in layer L at saturation
!            (cm3 [water] / cm3 [soil])
! SW(L)    Volumetric soil water content in layer L
!            (cm3 [water] / cm3 [soil])
! SWEF     Soil water evaporation fraction; fraction of lower limit content 
!            to which evaporation can reduce soil water content in top layer
!            (fraction)
! TDRAIN   Cumulative daily drainage from profile (mm)
! TOTIR    Total seasonal irrigation (mm)
! TRUNOF   Cumulative runoff (mm)
! TSW      Total soil water in profile (cm)
! TSWINI   Initial soil water content (cm)
! WBALAN   Seasonal water balance (should equal 0.0) (mm)
! YRDOY    Current day of simulation (YYDDD)
! YRSIM    Start of simulation date (YYDDD)
!-----------------------------------------------------------------------
!     END SUBROUTINE WBAL
C=======================================================================
