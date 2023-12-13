C======================================================================
C  BS_OPGROW, Subroutine
C----------------------------------------------------------------------
C  Generates output file for daily growth variables
C     This routine is used for Sugarbeet.
C----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1990     Written based on MZ_OPGROW
C----------------------------------------------------------------------
C  Called by: Sugarbeet
C  Calls:     None
!======================================================================
      SUBROUTINE BS_OPGROW(CONTROL, ISWITCH, 
     &  CANHT, CANWH, DTT, HI, HIP, MDATE, NLAYR, NSTRES, 
     &  PCNL, PLTPOP, PODNO, PODWT, RLV, RSTAGE, !PSTRES1, PSTRES2, 
     &  RTDEP, RTWT, SATFAC, SDWT, SEEDNO, SENESCE, SHELPC, SLA, 
     &  STMWTO, SWFAC, TOPWT, TURFAC, VSTAGE, WTCO, WTLF, WTLO, 
     &  WTSO, XLAI, YRPLT)

!----------------------------------------------------------------------
      USE ModuleDefs 
      USE ModuleData
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, TIMDIF, YR_DOY
      SAVE
!----------------------------------------------------------------------
      INTEGER NOUTDG
      INTEGER DYNAMIC

      CHARACTER*1  RNMODE
      CHARACTER*12 OUTG

      INTEGER TIMDIF, COUNT
      INTEGER DAP, DAS, DOY, I, N_LYR, RSTAGE, RUN
      INTEGER MDATE, YEAR, YRDOY, YRPLT, YRSIM, VWAD

      REAL VSTAGE, XLAI, STMWTO, SDWT, WTLF, TOPWT, RTWT, PODWT, SEEDNO
      REAL SLA, PCNL, TURFAC, CANHT, CANWH, HI, SHELPC, SATFAC
      REAL SDSIZE, PODNO, RTDEP, NSTRES, SWFAC, HIP   !PSTRES1, PSTRES2, 
      REAL PLTPOP, PODWTD, DTT
      REAL WTLO, WTSO, WTCO
      REAL RLV(NL)
      REAL CUMSENSURF, CUMSENSOIL     !cumul. senes. soil and surface

!     Average stresses since last printout
      REAL SWF_AV, TUR_AV, NST_AV, EXW_AV, PS1_AV, PS2_AV, KST_AV

      CHARACTER*1     IDETG
      CHARACTER*6, PARAMETER :: ERRKEY = 'BS_OPG'
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
!                                 DYNAMIC = RUNINIT
!-----------------------------------------------------------------------
      IF(DYNAMIC.EQ.RUNINIT) THEN
          OUTG  = 'PlantGro.OUT'
          CALL GETLUN('OUTG',  NOUTDG)

!**********************************************************************
!     Seasonal initialization - run once per season
!**********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
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

          CALL GET(SOILPROP)
          N_LYR = MIN(10, MAX(4,SOILPROP%NLAYR))

          WRITE (NOUTDG, '("!",230(" ") ,"Root Dens. (cm/cm3)",
     &      " by soil depth (cm):",
     &    /,"!",T227,10A8)') (SoilProp%LayerText(L), L=1,N_LYR)

          WRITE (NOUTDG,201, ADVANCE='NO')
  201     FORMAT('@YEAR DOY   DAS   DAP',
     &   '   L#SD   GSTD   LAID   LWAD   SWAD   GWAD',
     &   '   RWAD   VWAD   CWAD   G#AD   GWGD   HIAD   PWAD',
     &   '   P#AD   WSPD   WSGD   NSTD   EWSD  PST1A  PST2A',
     &   '   KSTD   LN%D   SH%D   HIPD   PWDD   PWTD',
     &   '     SLAD   CHTD   CWID   RDPD') 

          DO L = 1, N_LYR
            IF (L < 10) THEN
              WRITE (NOUTDG,'("    ",A2,I1,A1)',ADVANCE='NO') "RL",L,"D"
            ELSE
              WRITE (NOUTDG,'("   ",A2,I2,A1)', ADVANCE='NO') "RL",L,"D"
            ENDIF
          ENDDO

          WRITE (NOUTDG,207)
  207     FORMAT('   CDAD   LDAD   SDAD   SNW0C   SNW1C  DTTD') 

        CUMSENSURF = 0.0
        CUMSENSOIL = 0.0
        SWF_AV = 0.0
        TUR_AV = 0.0
        NST_AV = 0.0
        EXW_AV = 0.0
!        PS1_AV = 0.0
!        PS2_AV = 0.0
        KST_AV = 0.0
!-----------------------------------------------------------------------
!                         DYNAMIC = OUTPUT
!-----------------------------------------------------------------------
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
        IF (YRDOY .GE. YRPLT) THEN

          PODWTD = 0.0
!         DAS = MAX(0, TIMDIF(YRSIM, YRDOY))
          DAP = MAX(0, TIMDIF(YRPLT, YRDOY))

          IF (DAP > DAS) DAP = 0

!         Calculate cumulative senesence
          CUMSENSURF = CUMSENSURF + SENESCE % ResWt(0) 
          DO L = 1, NLAYR
            CUMSENSOIL = CUMSENSOIL + SENESCE % ResWt(L)
          ENDDO

          IF (SEEDNO .GT. 0.0) THEN
              SDSIZE = SDWT/SEEDNO*1000.
          ELSE
              SDSIZE = 0.0
          ENDIF

!         Compute average stress factors since last printout
          SWF_AV = SWF_AV + (1.0 - SWFAC)
          TUR_AV = TUR_AV + (1.0 - TURFAC)
          NST_AV = NST_AV + (1.0 - NSTRES)
          EXW_AV = EXW_AV + SATFAC
!          PS1_AV = PS1_AV + (1.0 - PSTRES1)
!          PS2_AV = PS2_AV + (1.0 - PSTRES2)
          COUNT = COUNT + 1

          !-------------------------------------------------------------
          !  Write output based on user specified frequency 
          !-------------------------------------------------------------
          IF ((MOD(DAS,FROP) .EQ. 0)    !Daily output every FROP days,
     &      .OR. (YRDOY .EQ. YRPLT)         !on planting date, and
     &      .OR. (YRDOY .EQ. MDATE)) THEN   !at harvest maturity 

            CALL YR_DOY(YRDOY, YEAR, DOY)

!           Compute average stress factors since last printout
            IF (COUNT > 0) THEN
              SWF_AV = SWF_AV / COUNT
              TUR_AV = TUR_AV / COUNT
              NST_AV = NST_AV / COUNT
              EXW_AV = EXW_AV / COUNT
!              PS1_AV = PS1_AV / COUNT
!              PS2_AV = PS2_AV / COUNT
              COUNT = 0
            ENDIF

            VWAD = NINT(WTLF*10. + STMWTO*10.)
      
            WRITE(NOUTDG,400,ADVANCE='NO')
     &        YEAR, DOY, DAS, DAP,VSTAGE,RSTAGE,XLAI,
     &        NINT(WTLF*10.),NINT(STMWTO*10.),NINT(SDWT*10.),
     &        NINT(RTWT*10.*PLTPOP), VWAD, NINT(TOPWT*10.),
     &        NINT(SEEDNO),SDSIZE,HI,NINT(PODWT*10.),NINT(PODNO),
     &        SWF_AV, TUR_AV, NST_AV, EXW_AV, PS1_AV, PS2_AV, KST_AV,
     &        PCNL,SHELPC,HIP, NINT(PODWTD*10.),
     &        NINT((PODWTD+PODWT)*10.),SLA,CANHT,CANWH, (RTDEP/100.)
 400        FORMAT (1X,I4,1X,I3.3,2(1X,I5),1X,F6.1,1X,I6,1X,F6.2,
     &        7(1X,I6),1X,F6.1,1X,F6.3,2(1X,I6),7(1X,F6.3),3(1X,F6.2),
     &        2(1X,I6),1X,F8.1,2(1X,F6.2),1X,F6.2)

            WRITE(NOUTDG,402,ADVANCE='NO')(RLV(I),I=1,N_LYR)
 402          FORMAT (10F8.2)

            WRITE(NOUTDG,404)
     &        NINT(WTCO*10.),NINT(WTLO*10.),NINT(WTSO*10.),
     &         NINT(CUMSENSURF), NINT(CUMSENSOIL), DTT
 404          FORMAT (3(1X,I6), 2I8, F6.2)
          ENDIF

!         Set average stress factors since last printout back to zero
          SWF_AV = 0.0
          TUR_AV = 0.0
          NST_AV = 0.0
          EXW_AV = 0.0
!          PS1_AV = 0.0
!          PS2_AV = 0.0

        ENDIF 

!-----------------------------------------------------------------------
!                 DYNAMIC = SEASEND
!-----------------------------------------------------------------------
C     Simulation Summary File
C-------------------------------------------------------------------
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
        !Close daily output files.
        CLOSE (NOUTDG)

      ENDIF

!***********************************************************************
      RETURN
      END SUBROUTINE BS_OPGROW
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
!       END SUBROUTINE BS_OPGROW
!=======================================================================

