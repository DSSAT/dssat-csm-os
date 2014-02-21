C======================================================================
C  SA_OPGROW, Subroutine, G. Hoogenboom, J.W. Jones
C  Modified MZ_OPGROW by Kofikuma Dzotsi for Salus crop model
C----------------------------------------------------------------------
C  Generates output file for daily growth variables
C     This routine is used for maize, sorghum and millet.
C----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1990     Written
C  09/21/1998 CHP Split off from OPDAY.FOR file
C  05/11/1999 GH  Incorporated in CROPGRO
C  12/18/2001 WDB Revised for modular CERES
C  08/20/2002 GH  Modified for Y2K
C  07/08/2003 CHP Changed senescence output.
!  05/31/2006 CHP Output DTT, growing degree days.
!  09/24/2009 KAD Modified for Salus
C----------------------------------------------------------------------
C  Called by: SALUS
C  Calls:     None
!======================================================================
      SUBROUTINE SALUS_OPGROW(CONTROL, ISWITCH, 
     &  DTT, MDATE,  
     &  RTWT,  
     &  TOPWT,  
     &  XHLAI,YRPLT,DROUGHTFAC)

!----------------------------------------------------------------------
      USE ModuleDefs 
      USE ModuleData
      IMPLICIT NONE
      SAVE
!----------------------------------------------------------------------
      CHARACTER*1  RNMODE
      CHARACTER*12 OUTG

      INTEGER TIMDIF, COUNTDAP, DAS, DOY, I, N_LYR, RSTAGE, RUN
      INTEGER DYNAMIC, NOUTDG, DAP, MDATE, YEAR, YRDOY, YRPLT, YRSIM

      REAL VSTAGE, XLAI, STMWTO, SDWT, WTLF, TOPWT, RTWT, PODWT, SEEDNO
      REAL SLA, PCNL, TURFAC, CANHT, CANWH, HI, SHELPC, SATFAC, KSTRES
      REAL SDSIZE, PODNO, PSTRES1, PSTRES2, RTDEP, NSTRES, SWFAC, HIP
      REAL PLTPOP, PODWTD, DTT, WTLO, WTSO, WTCO
      REAL RLV(NL), XHLAI, DROUGHTFAC

!     Average stresses since last printout
      REAL SWF_AV, TUR_AV, NST_AV, EXW_AV, PS1_AV, PS2_AV, KST_AV

      CHARACTER*1     IDETG
      CHARACTER*6, PARAMETER :: ERRKEY = 'MZ_OPG'
      INTEGER         ERRNUM, FROP, NLAYR, L
      LOGICAL         FEXIST, FIRST

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (ResidueType) SENESCE
      TYPE (SoilType)    SOILPROP

      IDETG   = ISWITCH % IDETG
      IF (IDETG .EQ. 'N') RETURN

      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FROP    = CONTROL % FROP
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM

!-----------------------------------------------------------------------
! DYNAMIC = SEASINIT -  Seasonal initialization - run once per season
!-----------------------------------------------------------------------
       IF (DYNAMIC .EQ. SEASINIT) THEN
          OUTG  = 'PlantGro.OUT'
          CALL GETLUN('OUTG',  NOUTDG)
          
          !-------------------------------------------------------------
          !   Open PlantGro.OUT as new or append
          !-------------------------------------------------------------
          INQUIRE (FILE = OUTG, EXIST = FEXIST)
          IF (FEXIST) THEN
            OPEN (UNIT=NOUTDG, FILE=OUTG, STATUS='OLD',
     &        IOSTAT=ERRNUM, POSITION='APPEND')
            FIRST = .FALSE.  
          ELSE
            OPEN (UNIT=NOUTDG, FILE=OUTG, STATUS='NEW',
     &        IOSTAT = ERRNUM)
              WRITE(NOUTDG,'("*GROWTH ASPECTS OUTPUT FILE")')
            FIRST = .TRUE.  
          ENDIF

          !---------------------------------------------------------
          ! Generate variable heading for GROWTH.OUT
          !---------------------------------------------------------
          CALL HEADER(SEASINIT, NOUTDG, RUN)

          WRITE (NOUTDG, 201)
  201     FORMAT('@YEAR DOY   DAS   DAP   LAID   RWAD   CWAD   WSPD')

!-----------------------------------------------------------------------
!                         DYNAMIC = OUTPUT
!-----------------------------------------------------------------------
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
      
        IF (YRDOY .GE. YRPLT) THEN
          DAP = MAX(0, TIMDIF(YRPLT, YRDOY))

          IF (DAP > DAS) DAP = 0

          !-------------------------------------------------------------
          !  Write output based on user specified frequency 
          !-------------------------------------------------------------
          IF ((MOD(DAS,FROP) .EQ. 0)    !Daily output every FROP days,
     &      .OR. (YRDOY .EQ. YRPLT)         !on planting date, and
     &      .OR. (YRDOY .EQ. MDATE)) THEN   !at harvest maturity 

            CALL YR_DOY(YRDOY, YEAR, DOY)

		  WRITE(NOUTDG,400)
     &        YEAR, DOY, DAS, DAP,XHLAI,NINT(RTWT),NINT(TOPWT),
     &        (1-DROUGHTFAC)
             
 400        FORMAT (1X,I4,1X,I3.3,2(1X,I5),1X,F6.2,2(1X,I6),1X,F6.3)
  
          ENDIF

        ENDIF 

!-----------------------------------------------------------------------
!                 DYNAMIC = SEASEND
!-----------------------------------------------------------------------
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
      
        !Close daily output files.
        CLOSE (NOUTDG)

      ENDIF

!***********************************************************************
      RETURN
      END SUBROUTINE SALUS_OPGROW
!=======================================================================


!=======================================================================
!       Variable definitions for OPGROW
!-----------------------------------------------------------------------
! CANHT   Canopy height (m)
! CANWH   Canopy width normal to row (m)
! CROP    Crop identification code 
! ENAME   Experiment description 
! EXPER   Experiment code (prefix of input files) 
! HI      Ratio of seed weight (SDWT) to weight of above-ground portion of 
!           plant (TOPWT) (g[seed] / g[tops])
! HIP     Ratio of pod weight (PODWT) to weight of above-ground portion of 
!           plant (TOPWT) (g[pods] / g[tops])
! MODEL   Name of CROPGRO executable file 
! NL      maximum number of soil layers = 20 
! NOUTDG  Unit number for growth output file 
! RUN    Report number for sequenced or multi-season runs 
! NSTRES  Nitrogen stress factor (1=no stress, 0=max stress) 
! OUTG    Growth output file name (typically 'GROWTH.OUT') 
! PCNL    Percentage of N in leaf tissue (100 g[N] / g[leaf])
! PODNO   Total number of pods (#/m2)
! PODWT   Dry mass of seeds plus shells, including C and N
!           (g[pods] / m2[ground])
! PODWTD  Mass of detached pods (g[pods] / m2[ground])
! RLV(L)  Root length density for soil layer L ((cm root / cm3 soil))
! RSTAGE  Number of RSTAGES which have occurred. 
! RTDEP   Root depth (cm)
! RTWT    Dry mass of root tissue, including C and N (g[root] / plant)
! SATFAC  Root length weighted soil water excess stress factor ( 0 = no 
!           stress; 1 = saturated stress ) 
! SDSIZE  Average mass of seeds (mg / seed)
! SEEDNO  Total number of seeds (#/m2)
! SHELLW  Shell mass (g[shell] / m2)
! SHELPC  Percentage of pod mass that is seeds (g[seed]/g[pods] * 100%)
! SLA     Specific leaf area (cm2[leaf] / m2[ground])
! STMWTO   Dry mass of stem tissue, including C and N (g[stem] / m2[ground)
! SWFAC   Effect of soil-water stress on photosynthesis, 1.0=no stress, 
!           0.0=max stress 
! TITLET  Description of treatment for this simulation 
! TOPWT   Total weight of above-ground portion of crop, including pods
!           (g[tissue] / m2)
! TURFAC  Water stress factor for expansion (0 - 1) 
! VSTAGE  Number of nodes on main stem of plant 
! WTCO    Cumulative losses of plant tissue (g[tissue] / m2)
! WTLF    Dry mass of leaf tissue including C and N (g[leaf] / m2[ground])
! WTLO    Cumulative leaf losses (g[leaf] / m2)
! WTSO    Cumulative stem losses (g[stem] / m2)
! XLAI    Leaf area (one side) per unit of ground area
!           (m2[leaf] / m2[ground])
! YRDOY   Current day of simulation (YYDDD)
! YRPLT   Planting date (YYDDD)
!***********************************************************************
!       END SUBROUTINE MZ_OPGROW
!=======================================================================

