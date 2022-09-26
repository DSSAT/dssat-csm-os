C=======================================================================
C  OPWBAL_2D, Subroutine, C.H.Porter from Soil Water portions of OPDAY
C  Generates output for daily soil water data
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  09/05/2008 CHP Written
C-----------------------------------------------------------------------
C  Called from:   WatBal2D
C  Calls:         None
C=======================================================================
      SUBROUTINE OPWBAL_2D(CONTROL, ISWITCH, 
     &    CRAIN, DLAYR, IRRAMT, LL, NLAYR, RUNOFF,        !Input
     &    SOILPROP, StdIrrig, SW, TDRAIN, TRUNOF,         !Input
     &    ActWTD, LatFlow, MgmtWTD, ThetaCap)             !Input 

!-----------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE
      EXTERNAL YR_DOY, GETLUN, HEADER, INCDAT, SUMSW, SUMVALS
      SAVE

      CHARACTER*1 IDETW, IDETL, ISWWAT, RNMODE
      CHARACTER*12 OUTWAT
      PARAMETER (OUTWAT = 'SoilWat.OUT ')

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, L
      INTEGER NAP, NLAYR, N_LYR, NOUTDW, RUN
      INTEGER YEAR, YRDOY, REPNO, YRSTART, INCDAT

      REAL CRAIN, IRRAMT, PESW, TDRAIN, TLL
      REAL TOTIR, TRUNOF, TSW, StdIrrig
      REAL, DIMENSION(NL) :: DLAYR, LL, SW
      REAL RUNOFF, SW10        

      LOGICAL FEXIST, DOPRINT

!     Water table
      INTEGER NAVWB
      REAL LatFlow, ActWTD, MgmtWTD
      REAL CumLatFlow
      REAL AVWTD, AVMWTD, TCSW10
      REAL, DIMENSION(NL) :: ThetaCap

!     Arrays which contain data for printing in SUMMARY.OUT file
      INTEGER, PARAMETER :: SUMNUM = 4
      CHARACTER*4, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (SoilType)    SoilProp

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

      CALL YR_DOY(YRDOY, YEAR, DOY) 

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
      CumLatFlow = 0.

      IF (IDETW == 'N' .OR. ISWWAT == 'N' .OR. IDETL == '0') THEN
        DOPRINT = .FALSE.
      ELSE
        DOPRINT = .TRUE.
      ENDIF

      IF (.NOT. DOPRINT) RETURN
!-----------------------------------------------------------------------
C   Generate headings for output file
C-----------------------------------------------------------------------
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

C-----------------------------------------------------------------------
C     Variable heading for WATER.OUT
C-----------------------------------------------------------------------
      IF (RNMODE .NE. 'Q' .OR. RUN .EQ. 1) THEN
        IF (RNMODE .EQ. 'Q') THEN
          CALL HEADER(SEASINIT, NOUTDW, REPNO)
        ELSE
          CALL HEADER(SEASINIT, NOUTDW, RUN)
        ENDIF

        N_LYR = MIN(10, MAX(4,SOILPROP%NLAYR))

        WRITE (NOUTDW, '("!",T97,"Soil Layer depths (cm):",7A8,
     &      " Soil Layer depths (cm):")') (" ",L=1,N_LYR-3)
        WRITE (NOUTDW, '(T92,20A8)')
     &       (SoilProp%LayerText(L), L=1,N_LYR), 
     &       (SoilProp%LayerText(L), L=1,N_LYR)  

        WRITE (NOUTDW,1123,ADVANCE='NO')
 1123   FORMAT('@YEAR DOY   DAS',
     &    '  SWTD  SWXD   ROFC   DRNC   PREC  IR#C  IRRC   ROFD',
     &    '   LATFC   MDTWT    DTWT')

        IF (N_LYR < 10) THEN
          WRITE (NOUTDW,1121,ADVANCE='NO') ("SW",L,"D",L=1,N_LYR)
 1121     FORMAT(9("    ",A2,I1,A1))
          WRITE (NOUTDW,'(9("  ",A2,I1,A1))') ("TCSW",L,"D",L=1,N_LYR)
        ELSE
          WRITE (NOUTDW,1122,ADVANCE='NO') ("SW",L,"D",L=1,9),"    SW10"
 1122     FORMAT(9("    ",A2,I1,A1),A8)
          WRITE(NOUTDW,'(9("    ",A2,I1,A1),A8)')("TCSW",L,"D",L=1,9)
     &              ,"    TCSW10"
        ENDIF

        YRSTART = YRDOY
        CALL YR_DOY(INCDAT(YRSTART,-1),YEAR,DOY)

        IF (ISWWAT .EQ. 'Y') THEN
          CALL SUMSW(NLAYR, DLAYR, SW, TSW)
          CALL SUMSW(NLAYR, DLAYR, LL, TLL)
          PESW = MAX(0.0, TSW - TLL) / 10.
        ELSE
          PESW = 0.0
        ENDIF
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
          AVWTD  = AVWTD  + ActWTD
          AVMWTD = AVMWTD + MgmtWTD
        ENDIF

!       Drip Irrig
        IF (IRRAMT .GT. 1.E-4) THEN
          TOTIR = TOTIR + IRRAMT
          NAP  = NAP + 1
        !ENDIF

!       Standard Irrig
        ELSE IF (StdIrrig > 1.E-4) THEN
          TOTIR = TOTIR + StdIrrig
          NAP = NAP + 1
        ENDIF

        CALL SUMSW(NLAYR, DLAYR, SW, TSW)
        CALL SUMSW(NLAYR, DLAYR, LL, TLL)
        PESW = MAX(0.0, TSW - TLL) / 10.
      ENDIF

      SW10 = 0.0
      TCSW10 = 0.0
      IF (NLAYR > N_LYR) THEN
        DO L = 10, NLAYR
          SW10 = SW10 + SW(L) * DLAYR(L)
          TCSW10 = TCSW10 + ThetaCap(L) * DLAYR(L)
        ENDDO
        SW10 = SW10 / (SOILPROP%DS(NLAYR) - SOILPROP%DS(9))
        TCSW10 = TCSW10 / (SOILPROP%DS(NLAYR) - SOILPROP%DS(9))
      ENDIF

      CumLatFlow = CumLatFlow + LatFlow

C-----------------------------------------------------------------------
C  Generate output for file WATER.OUT
C-----------------------------------------------------------------------
      IF (DOPRINT) THEN
!           Print initial conditions, 
        IF (DYNAMIC == SEASINIT .OR.
!           Print every FROP days, and
     &     (DYNAMIC .EQ. OUTPUT .AND. MOD(DAS, FROP) .EQ. 0) .OR. 
!           Print on last day if not already done.
     &     (DYNAMIC .EQ. SEASEND  .AND. MOD(DAS, FROP) .NE. 0)) THEN

          IF (NAVWB > 0) THEN
            AVWTD = AVWTD / NAVWB
            AVMWTD= AVMWTD / NAVWB
          ELSE
            AVWTD = ActWTD
            AVMWTD= MgmtWTD
          ENDIF

          WRITE (NOUTDW,1302,ADVANCE='NO') YEAR, DOY, MOD(DAS,100000),
     &        NINT(TSW), NINT(PESW*10), NINT(TRUNOF), NINT(TDRAIN), 
     &        NINT(CRAIN), NAP, NINT(TOTIR), NINT(RUNOFF), 
     &        NINT(CumLatFlow), 
     &        NINT(MgmtWTD), NINT(AVWTD) 
 1302     FORMAT(1X,I4,1X,I3.3,I6,
     &       2(1X,I5),3(1X,I6),2(1X,I5),I7,3I8,10F8.3)

          IF (NLAYR < 11) THEN
            WRITE(NOUTDW,'(20F8.3)') (SW(L),L=1,N_LYR) 
     &                        ,(ThetaCap(L),L=1,N_LYR)
          ELSE
            WRITE(NOUTDW,'(20F8.3)') (SW(L),L=1,9), SW10, 
     &                         (ThetaCap(L),L=1,9), TCSW10
          ENDIF

          NAVWB = 0
          AVWTD = 0.
          AVMWTD= 0.
        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     SEASEND - Sesaonal Output
!***********************************************************************
        IF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
!           Store Summary.out labels and values in arrays to send to
!           OPSUM routines for printing.  Integers are temporarily 
!           saved aS real numbers for placement in real array.
            LABEL(1)  = 'PRCM'; VALUE(1)  = CRAIN
            LABEL(2)  = 'ROCM'; VALUE(2)  = TRUNOF 
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
      END SUBROUTINE OPWBAL_2D
!***********************************************************************
!-----------------------------------------------------------------------
!     OPWBAL_2D VARIABLE DEFINITIONS:  updated 2/19/2004
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
! TOTIR     Total seasonal irrigation (mm)
! TRUNOF    Cumulative runoff (mm)
! TSW       Total soil water in profile (cm)
! VALUE(I)  Array of values of variables sent to OPSUM for summary 
!             printout; corresponds to LABEL array which identifies 
!             variables being sent. (varies)
! ActWTD    Depth to water table (cm)
! YEAR      Year of current date of simulation 
! YRDOY     Current day of simulation (YYYYDDD)
!-----------------------------------------------------------------------
!     END OPWBAL_2D Subroutine
!-----------------------------------------------------------------------
