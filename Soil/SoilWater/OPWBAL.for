C=======================================================================
C  OPWBAL, Subroutine, C.H.Porter from Soil Water portions of OPDAY
C  Generates output for daily soil water data
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  04/13/1998 CHP Written
C  06/19/2001 GH  Modified output format
C  08/20/2002 GH  Modified for Y2K
C  03/10/2006 CHP Added mulch routine triggered by MEINF.
C-----------------------------------------------------------------------
C  Called from:   WATBAL
C  Calls:         None
C=======================================================================
      SUBROUTINE OPWBAL(CONTROL, ISWITCH, 
     &    CRAIN, DLAYR, FLOODWAT, IRRAMT, LL, MULCH,      !Input
     &    NLAYR, RUNOFF, SOILPROP, SW, TDFC, TDFD,        !Input
     &    TDRAIN, TRUNOF, ActWTD, LatInflow, LatOutflow,  !Input
     &    EXCS, WTDEP)                                    !Input

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
      USE FloodModule    ! which contain control information, soil
                         ! parameters, hourly weather data.
!     VSH
      USE CsvOutput 
      USE Linklist
      IMPLICIT NONE
      EXTERNAL YR_DOY, GETLUN, HEADER, SUMSW, SUMVALS, INCDAT
     &   , SoilLayerText2
      SAVE

      CHARACTER*1 IDETW, IDETL, ISWWAT, MEINF, RNMODE
      CHARACTER*12 OUTWAT
      PARAMETER (OUTWAT = 'SoilWat.OUT ')

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, L
      INTEGER NAP, NLAYR, N_LYR, NOUTDW, RUN
      INTEGER YEAR, YRDOY, REPNO, YRSTART, INCDAT

      REAL CRAIN, IRRAMT, PESW, TDFC, TDFD, TDRAIN, TLL
      REAL TOTBUNDRO, TOTIR, TRUNOF, TSW, MULCHWAT
      REAL, DIMENSION(NL) :: DLAYR, LL, SW
      REAL RUNOFF
      REAL EXCS

      LOGICAL FEXIST, DOPRINT

!     Water table
      INTEGER NAVWB
      REAL LatInflow, LatOutflow, ActWTD, WTDEP, WaterTable
      REAL CumLatInflow, CumLatOutflow
      REAL AVWTD, AVMWTD

!     Arrays which contain data for printing in SUMMARY.OUT file
      INTEGER, PARAMETER :: SUMNUM = 4
      CHARACTER*4, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

      CHARACTER*8, DIMENSION(NL) :: SW_txt, LayerText

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (FloodWatType) FLOODWAT
      TYPE (MulchType)    MULCH
      TYPE (SoilType)     SoilProp

      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FROP    = CONTROL % FROP
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE
      REPNO   = CONTROL % REPNO
      YRDOY   = CONTROL % YRDOY

      IDETL   = ISWITCH % IDETL
      IDETW   = ISWITCH % IDETW
      ISWWAT  = ISWITCH % ISWWAT
      MEINF   = ISWITCH % MEINF    
      FMOPT   = ISWITCH % FMOPT   ! VSH

      TOTBUNDRO = FLOODWAT % TOTBUNDRO
      MULCHWAT  = MULCH % MULCHWAT

      CALL YR_DOY(YRDOY, YEAR, DOY) 

      IF (ActWTD .LT. WTDEP) THEN
        WaterTable = ActWTD
      ELSE
        WaterTable = WTDEP
      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC == SEASINIT) THEN
C-----------------------------------------------------------------------
C   Set initial values to calculate average values
C-----------------------------------------------------------------------
      NAP   = 0
      PESW  = 0.
      NAVWB = 0
      AVWTD = 0.
      AVMWTD = 0.
      TOTIR = 0.
      TRUNOF= 0.
      CRAIN = 0.
      CumLatInflow = 0.
      CumLatOutflow = 0.

      IF (IDETW == 'N' .OR. ISWWAT == 'N' .OR. IDETL == '0') THEN
        DOPRINT = .FALSE.
      ELSE
        DOPRINT = .TRUE.
      ENDIF

      IF (.NOT. DOPRINT) RETURN
!-----------------------------------------------------------------------
C   Generate headings for output file
C-----------------------------------------------------------------------
      IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
      CALL GETLUN('OUTWAT', NOUTDW)
      INQUIRE (FILE = OUTWAT, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = NOUTDW, FILE = OUTWAT, STATUS = 'OLD',
     &    IOSTAT = ERRNUM, POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = NOUTDW, FILE = OUTWAT, STATUS = 'NEW',
     &    IOSTAT = ERRNUM)
        WRITE(NOUTDW,'("*SOIL WATER DAILY OUTPUT FILE")')
      ENDIF
      END IF   ! VSH
C-----------------------------------------------------------------------
C     Variable heading for WATER.OUT
C-----------------------------------------------------------------------
      IF (RNMODE .NE. 'Q' .OR. RUN .EQ. 1) THEN
        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
        IF (RNMODE .EQ. 'Q') THEN
          CALL HEADER(SEASINIT, NOUTDW, REPNO)
        ELSE
          CALL HEADER(SEASINIT, NOUTDW, RUN)
        ENDIF
        END IF   ! VSH

!       Print all layers
        N_LYR = MAX(4,SOILPROP%NLAYR)

!     Use revised Soil layer text which uses all soil layers
      CALL SoilLayerText2(SOILPROP%DS, NLAYR,        !Input
     &    LayerText)                             !Output 

        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
!         New print format includes mulch, tiledrain and runoff info
          WRITE (NOUTDW, '("!",T133,
     &    "Soil water content (mm3/mm3) by soil depth (cm):",
     &    /,"!",T128,20A8)') (LayerText(L), L=1,NLAYR)
          WRITE (NOUTDW,1120, ADVANCE='NO')
 1120     FORMAT('@YEAR DOY   DAS',
     &    '    SWTD    SWXD    ROFC    DRNC    PREC    IR#C',
     &    '    IRRC   LATFC    DTWT',
     &    '    MWTD    TDFD    TDFC    ROFD    ROSD')

!       print SW for all layers
 !    &    '  SWTD  SWXD   ROFC   DRNC   PREC  IR#C  IRRC  DTWT')
 !       ENDIF

!        IF (N_LYR < 10) THEN
!          WRITE (NOUTDW,1121) ("SW",L,"D",L=1,N_LYR)
! 1121     FORMAT(9("    ",A2,I1,A1))
!        ELSE
!!         WRITE (NOUTDW,1122) ("SW",L,"D",L=1,9), "    SW10"
        DO L = 1, NLAYR
          IF (L < 10) THEN
            WRITE(SW_txt(L),'("    SW",I1,"D")') L
          ELSE
            WRITE(SW_txt(L),'("   SW",I2,"D")') L
          ENDIF
        ENDDO
        WRITE(NOUTDW,'(20(A8))') (SW_txt(L), L=1,NLAYR)

        END IF   ! VSH

        YRSTART = YRDOY
        CALL YR_DOY(INCDAT(YRSTART,-1),YEAR,DOY)

        IF (ISWWAT .EQ. 'Y') THEN
          CALL SUMSW(NLAYR, DLAYR, SW, TSW)
          CALL SUMSW(NLAYR, DLAYR, LL, TLL)
          PESW = MAX(0.0, TSW - TLL) / 10.
        ELSE
          PESW = 0.0
        ENDIF

        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
!         New print format includes mulch, tiledrain and runoff info
          WRITE (NOUTDW,1300)YEAR,DOY,DAS, NINT(TSW), 
     &    NINT(PESW*10.),0,0,0,0,
     &      0, 0, NINT(WaterTable), 
     &      MULCHWAT, 0.0, 0.0, 0.0, 0.0,
     &      (SW(L),L=1,NLAYR)
 1300     FORMAT(1X,I4,1X,I3.3,1X,I5,9(1X,I7),  !was 10(1X,I7)
     &      F8.2,2F8.1,F8.2,
     &      F8.2,   !EXCS
     &      20(F8.3))

        END IF   ! VSH
         
      IF (FMOPT == 'C') THEN
         N_LYR = MIN(10, MAX(4,SOILPROP%NLAYR)) 
         CALL CsvOutSW_crgro(EXPNAME,CONTROL%RUN, CONTROL%TRTNUM,
     &CONTROL%ROTNUM,CONTROL%REPNO, YEAR, DOY, DAS, TSW, PESW, TRUNOF,
     &TDRAIN, CRAIN, NAP, TOTIR, AVWTD, MULCHWAT, TDFD*10., TDFC*10.,
     &RUNOFF, N_LYR, SW, vCsvlineSW, vpCsvlineSW, vlngthSW)
     
         CALL LinklstSW(vCsvlineSW)
      END IF     
      ENDIF

!***********************************************************************
!***********************************************************************
      ENDIF !DYNAMIC CONTROL
!***********************************************************************
!***********************************************************************
!     Daily Output
!***********************************************************************
      IF (DYNAMIC == SEASINIT .OR. DYNAMIC == OUTPUT .OR. 
     &      DYNAMIC == SEASEND) THEN
C-----------------------------------------------------------------------
C   Calculate average values as a function of the output interval
C-----------------------------------------------------------------------
      IF (DOPRINT) THEN
        IF (ActWTD > 1.E-6) THEN
          NAVWB  = NAVWB  + 1
          AVWTD  = AVWTD  + WaterTable
        ENDIF

        IF (IRRAMT .GT. 1.E-4) THEN
          TOTIR = TOTIR + IRRAMT
          NAP  = NAP + 1
        ENDIF

        IF (ISWWAT .EQ. 'Y') THEN
          CALL SUMSW(NLAYR, DLAYR, SW, TSW)
          CALL SUMSW(NLAYR, DLAYR, LL, TLL)
          PESW = MAX(0.0, TSW - TLL) / 10.
        ELSE
          PESW = 0.0
        ENDIF

        CumLatInflow = CumLatInflow + LatInflow
        CumLatOutflow = CumLatOutflow + LatOutflow

C-----------------------------------------------------------------------
C  Generate output for file WATER.OUT
C-----------------------------------------------------------------------
!       Print every FROP days, and
        IF ((DYNAMIC .EQ. OUTPUT .AND. MOD(DAS, FROP) .EQ. 0) .OR. 
!           Print on last day if not already done.
     &      (DYNAMIC .EQ. SEASEND  .AND. MOD(DAS, FROP) .NE. 0)) THEN

          IF (NAVWB > 0) THEN
            AVWTD = AVWTD / NAVWB
            AVMWTD= AVMWTD / NAVWB
          ELSE
            AVWTD = WaterTable
!           AVMWTD= MgmtWTD
          ENDIF

          IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
!           New print format includes mulch, tiledrain and runoff info
            WRITE (NOUTDW,1300)YEAR,DOY,MOD(DAS,100000), NINT(TSW), 
     &        NINT(PESW*10),NINT(TRUNOF),NINT(TDRAIN),NINT(CRAIN),
     &        NAP, NINT(TOTIR),
     &        NINT(CumLatInflow+CumLatOutflow), 
     &        NINT(AVWTD),
     &        MULCHWAT, TDFD*10., TDFC*10., RUNOFF, EXCS,
     &        (SW(L),L=1,NLAYR)

          END IF   ! VSH 

!     VSH CSV output corresponding to SoilWat.OUT
      IF (FMOPT == 'C') THEN
         N_LYR = MIN(10, MAX(4,SOILPROP%NLAYR)) 
         CALL CsvOutSW_crgro(EXPNAME,CONTROL%RUN, CONTROL%TRTNUM,
     &CONTROL%ROTNUM,CONTROL%REPNO, YEAR, DOY, DAS, TSW, PESW, TRUNOF,
     &TDRAIN, CRAIN, NAP, TOTIR, AVWTD, MULCHWAT, TDFD*10., TDFC*10.,
     &RUNOFF, N_LYR, SW, vCsvlineSW, vpCsvlineSW, vlngthSW)
     
         CALL LinklstSW(vCsvlineSW)
      END IF         
      
          NAVWB = 0
          AVWTD = 0.
          AVMWTD= 0.
        ENDIF
      ENDIF

      IF (DYNAMIC .EQ. SEASINIT) THEN
          NAVWB = 0
          AVWTD = 0.        
          AVMWTD= 0.
      ENDIF

!***********************************************************************
!***********************************************************************
!     SEASEND - Sesaonal Output
!***********************************************************************
        IF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
!         Need to calculate end of season PESW always (regardless of IDETL)
          IF (ISWWAT .EQ. 'Y') THEN
            CALL SUMSW(NLAYR, DLAYR, SW, TSW)
            CALL SUMSW(NLAYR, DLAYR, LL, TLL)
            PESW = MAX(0.0, TSW - TLL) / 10.
          ELSE
            PESW = 0.0
          ENDIF

!           Store Summary.out labels and values in arrays to send to
!           OPSUM routines for printing.  Integers are temporarily 
!           saved aS real numbers for placement in real array.
            LABEL(1)  = 'PRCM'; VALUE(1)  = CRAIN
            LABEL(2)  = 'ROCM'; VALUE(2)  = TRUNOF + TOTBUNDRO
            LABEL(3)  = 'DRCM'; VALUE(3)  = TDRAIN
            LABEL(4)  = 'SWXM'; VALUE(4)  = PESW*10.

            !Send labels and values to OPSUM
            CALL SUMVALS (SUMNUM, LABEL, VALUE) 

            !Close daily output files.
            CLOSE (NOUTDW)
        ENDIF
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OPWBAL
!***********************************************************************
!-----------------------------------------------------------------------
!     OPWBAL VARIABLE DEFINITIONS:  updated 2/19/2004
!-----------------------------------------------------------------------
! AVWTD     Average water table depth since last printout (cm)
! AVMWTD    Average managed water table depth since last printout (cm)
! CONTROL   Composite variable containing variables related to control 
!             and/or timing of simulation.    See Appendix A. 
! CRAIN     Cumulative precipitation (mm)
! DAS       Days after start of simulation (d)
! DLAYR(L)  Thickness of soil layer L (cm)
! DOY       Current day of simulation (d)
! ERRNUM    Error number for input 
! FEXIST    Logical variable 
! FLOODWAT  Composite variable containing information related to bund 
!             management. Structure of variable is defined in ModuleDefs.for. 
! FROP      Frequency of output (d)
! IDETW     Y=detailed water balance output, N=no detailed output 
! IRRAMT    Irrigation amount for today (mm / d)
! ISWITCH   Composite variable containing switches which control flow of 
!             execution for model.  The structure of the variable 
!             (SwitchType) is defined in ModuleDefs.for. 
! ISWWAT    Water simulation control switch (Y or N) 
! LABEL(I)  Array of labels for variables sent to OPSUM for summary 
!             printout; corresponds to VALUE array which stores values of 
!             variables being sent. 
! LL(L)     Volumetric soil water content in soil layer L at lower limit
!            (cm3 [water] / cm3 [soil])
! NAP       Number of irrigation applications 
! NAVWB     Number of days since last printout (d)
! NLAYR     Actual number of soil layers 
! NOUTDW    Unit number for water balance output file 
! PESW      Potential extractable soil water (= SW - LL) summed over root 
!             depth (cm)
! REPNO     Replication number for current simulation 
! RNMODE    Simulation run mode (I=Interactive, A=All treatments, 
!             B=Batch mode, E=Sensitivity, D=Debug, N=Seasonal, Q=Sequence)
! RUN       Change in date between two observations for linear 
!             interpolation 
! SW(L)     Volumetric soil water content in layer L
!            (cm3 [water] / cm3 [soil])
! TDRAIN    Cumulative daily drainage from profile (mm)
! TLL       Total soil water in the profile at the lower limit of 
!             plant-extractable water (cm)
! TOTBUNDRO Cumulative seasonal flood runoff over bund (mm)
! TOTIR     Total seasonal irrigation (mm)
! TRUNOF    Cumulative runoff (mm)
! TSW       Total soil water in profile (cm)
! VALUE(I)  Array of values of variables sent to OPSUM for summary 
!             printout; corresponds to LABEL array which identifies 
!             variables being sent. (varies)
! ActWTD    Depth to water table (cm)
! WTDEP     Depth to calculated perched water table (cm)
! WaterTable Depth to lesser of ActWTD or WTDEP (cm)
! YEAR      Year of current date of simulation 
! YRDOY     Current day of simulation (YYYYDDD)
!-----------------------------------------------------------------------
!     END OPWBAL Subroutine
!-----------------------------------------------------------------------

!=======================================================================
!  The following subroutine is probably temporary, copied from 
!     SoilLayerText in the SOILDYN.for file.
!   This version prints ALL layers, and does not compress the final 
!     layers above 9 into a single large bottom layer.

!=======================================================================
!   SoilLayerText2, Subroutine
!-----------------------------------------------------------------------
!     Labels for soil layer depth info
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  2023-02-13 chp

!-----------------------------------------------------------------------
!  Called by: OPWBAL
!  Calls    : 
!=======================================================================
      SUBROUTINE SoilLayerText2(DS, NLAYR,        !Input
     &    LayerText)                             !Output 
 
      USE ModuleDefs 
      INTEGER NLAYR, L
      REAL, DIMENSION(NL) :: DS
      CHARACTER*8 LayerText(NL)
      INTEGER, DIMENSION(NL) :: ZB, ZT
      CHARACTER*14 FMT  
        
!     Establish soil layer depths for headers
!     Text describing soil layer depth data
!     1-NLAYR describe depths for layers 1-9
      LayerText = '        '
      ZT = -99
      ZB = -99
 
      DO L = 1, NLAYR
        IF (L == 1) THEN 
          ZT(1) = 0
          ZB(1) = NINT(DS(1))
        ELSE
          ZT(L) = ZB(L-1)
          ZB(L) = NINT(DS(L))
        ENDIF

!       Format dependant on # digits
        IF (ZB(L) > 0) THEN
          SELECT CASE (ZT(L))
          CASE (:9)
            SELECT CASE (ZB(L))
            CASE(:9);    FMT = '(5X,I1,"-",I1)'
            CASE(10:99); FMT = '(4X,I1,"-",I2)'
            CASE(100:);  FMT = '(3X,I1,"-",I3)'
            END SELECT
          CASE (10:99)
            SELECT CASE (ZB(L))
            CASE(10:99); FMT = '(3X,I2,"-",I2)'
            CASE(100:);  FMT = '(2X,I2,"-",I3)'
            END SELECT
          CASE (100:);   FMT = '(1X,I3,"-",I3)'
          END SELECT
          WRITE(LayerText(L),FMT) ZT(L), ZB(L)
        ENDIF
      ENDDO

      RETURN      
      END SUBROUTINE SoilLayerText2
C=======================================================================



