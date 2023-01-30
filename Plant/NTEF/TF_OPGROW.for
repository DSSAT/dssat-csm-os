C======================================================================
C  TF_OPGROW, Subroutine, G. Hoogenboom, J.W. Jones
C----------------------------------------------------------------------
C  Generates output file for daily growth variables
C     This routine is used for maize, sorghum, millet and wheat (APSIM).
C----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1990     Written
C  09/21/1998 CHP Split off from OPDAY.for file
C  05/11/1999 GH  Incorporated in CROPGRO
C  12/18/2001 WDB Revised for modular CERES
C  08/20/2002 GH  Modified for Y2K
C  07/08/2003 CHP Changed senescence output.
C  05/31/2006 CHP Output DTT, growing degree days.
C  06/27/2011 FSR created WH_OPGROW.for for APSIM NWheat (WHAPS) adaptation
!  01/11/2018 KEP converted WH_ sub-routines to TF_.
C----------------------------------------------------------------------
C  Called by: TF_APSIM
C  Calls:     None
!======================================================================
      SUBROUTINE TF_OPGROW(CONTROL, ISWITCH,
     &  CANHT, CANWH, DTT, HI, HIP, istage, KSTRES, MDATE, NLAYR,
     &  nfact, nwheats_dc_code, PCNL, PLTPOP,  PODWT,
     &  PSTRES1, PSTRES2, RLV, rtdep_nw, RTWT, SATFAC, SDWT,
     &  SEEDNO, SENESCE, SHELPC, SLA, STMWTO, sumstgdtt, SWFAC,
     &  TOPWT, SWDEF2, WTCO, WTLF, WTLO,
     &  WTSO, XLAI, YRPLT, SLFT, GAD2)

! 2023-01-26 chp removed unused variables in argument list:
!         PODNO, RSTAGE, VSTAGE, 

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
      INTEGER DAP, DAS, DOY, I, istage, N_LYR, RUN  !, RSTAGE
      INTEGER MDATE, YEAR, YRDOY, YRPLT, YRSIM, VWAD

      REAL XLAI, STMWTO, SDWT, WTLF, TOPWT, RTWT, PODWT, SEEDNO !VSTAGE, 
    ! REAL SLA, PCNL, TURFAC, CANHT, CANWH, HI, SHELPC, SATFAC, KSTRES
      REAL SLA, PCNL, SWDEF2, CANHT, CANWH, HI, SHELPC, SATFAC, KSTRES
      REAL SDSIZE, PSTRES1, PSTRES2, SWFAC, HIP  !, PODNO, RTDEP, NSTRES
      REAL rtdep_nw, nwheats_dc_code, PLTPOP, PODWTD, DTT
      REAL WTLO, WTSO, WTCO, GAD2
      REAL nfact(10) ! Nstress sensitivity by plant organ (0-1)
      REAL RLV(NL)
      REAL CUMSENSURF, CUMSENSOIL     !cumul. senes. soil and surface
      REAL sumstgdtt(20)
      real slft     ! low temperature factor (0-1)

!     Average stresses since last printout
      REAL SWF_AV, TUR_AV, NST_AV, EXW_AV, PS1_AV, PS2_AV, KST_AV

      CHARACTER*1     IDETG
      CHARACTER*6, PARAMETER :: ERRKEY = 'TF_OPG'
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

          WRITE (NOUTDG, '("!",237(" ") ,"Root Dens. (cm/cm3)",
     &      " by soil depth (cm):",
     &    /,"!",T234,10A8)') (SoilProp%LayerText(L), L=1,N_LYR)

          WRITE (NOUTDG,201, ADVANCE='NO')
  201     FORMAT('@YEAR DOY   DAS   DAP',
     &   '    DCCD  GSTD   LAID   LWAD   SWAD   GWAD',
     &   '   RWAD   VWAD   CWAD   G#AD   G#AD2  GWGD   HIAD   SHAD',
     &   '   WSPD   WSGD   SLFT   NSTD   EWSD  PST1A  PST2A',
!    &   '   P#AD   WSPD   WSGD   NSTD   EWSD  PST1A  PST2A',
     &   '   KSTD   LN%D   TPSM   HIPD   PWDD   PWTD',
     &   '     SLAD   CHTD   CWID   RDPD')

          DO L = 1, N_LYR
            IF (L < 10) THEN
              WRITE (NOUTDG,'("    ",A2,I1,A1)',ADVANCE='NO') "RL",L,"D"
            ELSE
              WRITE (NOUTDG,'("   ",A2,I2,A1)', ADVANCE='NO') "RL",L,"D"
            ENDIF
          ENDDO

          WRITE (NOUTDG,207)
  207   FORMAT('   CDAD   LDAD   SDAD   SNW0C   SNW1C    DTTD   SUMDTT')

        CUMSENSURF = 0.0
        CUMSENSOIL = 0.0
        SWF_AV = 0.0
        TUR_AV = 0.0
        NST_AV = 0.0
        EXW_AV = 0.0
        PS1_AV = 0.0
        PS2_AV = 0.0
        KST_AV = 0.0

!-----------------------------------------------------------------------
!                         DYNAMIC = OUTPUT
!-----------------------------------------------------------------------
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
        IF (YRDOY .GE. YRPLT) THEN

          PODWTD = 0
!         DAS = MAX(0, TIMDIF(YRSIM, YRDOY))
          DAP = MAX(0, TIMDIF(YRPLT, YRDOY))

          IF (DAP > DAS) DAP = 0

!         Calculate cumulative senesence
          CUMSENSURF = CUMSENSURF + SENESCE % ResWt(0)
          DO L = 1, NLAYR
            CUMSENSOIL = CUMSENSOIL + SENESCE % ResWt(L)
          ENDDO
!          Added IF statement to void divisions by zero (TF - 01/21/2022)
          IF (SEEDNO .GT. 0.0 .AND. PLTPOP .GT. 0.0) THEN
              !JZW changed
              SDSIZE = SDWT / (SEEDNO*PLTPOP) *1000.
              !SDSIZE = SDWT/SEEDNO*1000.
          ELSE
              SDSIZE = 0.0
          ENDIF

!         Compute average stress factors since last printout
          SWF_AV = SWF_AV + (1.0 - SWFAC)
          TUR_AV =  1 - SWDEF2
!          TUR_AV = TUR_AV + (1.0 - TURFAC) !JZW, TUR_AV is initialed as zero every day
!          TUR_AV = 1.0 - TURFAC
!*!       NST_AV = NST_AV + (1.0 - NSTRES)
        !Avg N stress:
!**!      NST_AV = (1.0 - (Nfact(1)+Nfact(2)+Nfact(3)+Nfact(4))/4)
! N stress index changed to enable comparison to APSIM nwheat output
! n_stress = n_stress + (1 - nfact2)
           NST_AV = (1.0 - Nfact(2))
          !NST_AV = Nfact(2)    ! JZW changed
          EXW_AV = EXW_AV + SATFAC
          PS1_AV = PS1_AV + (1.0 - PSTRES1)
          PS2_AV = PS2_AV + (1.0 - PSTRES2)
          KST_AV = KST_AV + (1.0 - KSTRES)
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
              TUR_AV = TUR_AV / COUNT !TUR_AV is not summation of days
              NST_AV = NST_AV / COUNT
              EXW_AV = EXW_AV / COUNT
              PS1_AV = PS1_AV / COUNT
              PS2_AV = PS2_AV / COUNT
              KST_AV = KST_AV / COUNT
              COUNT = 0
            ENDIF

            VWAD = NINT((WTLF + STMWTO + PODWT) * 10)

            WRITE(NOUTDG,400,ADVANCE='NO')
            !'@YEAR DOY  DAS  DAP            DCCD   GSTD LAID
     &        YEAR, DOY, DAS, DAP,nwheats_dc_code,istage,XLAI,
            !      LWAD             SWAD           GWAD
     &        NINT(WTLF*10.),NINT(STMWTO*10.),NINT(SDWT*10.),
            !      RWAD              VWAD        CWAD
     &        NINT(RTWT*10.*PLTPOP), VWAD, NINT(TOPWT*10.),
            !        G#AD            G#AD2                  GWGD  HIAD
     &        NINT(SEEDNO*PLTPOP), NINT(GAD2*PLTPOP), NINT(SDSIZE), HI,
            !  SHAD
     &     NINT(PODWT*10.),
            !   P#AD(int)
 !    &      NINT(PODNO),
            !  WSPD    WSGD   SLFT  NSTD   EWSD   PST1A   PST2A  KSTD
     &        SWF_AV, TUR_AV, SLFT,NST_AV,EXW_AV, PS1_AV, PS2_AV,KST_AV,
            ! LN%D   TPSM HIPD         PWDD
     &        PCNL,SHELPC, HIP, NINT(PODWTD*10.),
     &      !               PWTD      SLAD  CHTD  CWID    RDPD
     &        NINT((PODWTD+PODWT)*10.),SLA,CANHT,CANWH, (rtdep_nw/1000.)

!                   @YEAR   DOY    DAS DAP    DCCD  GSTD    LAID
 400        FORMAT (1X,I4,1X,I3.3,2(1X,I5),2X,F6.3,1X,I5,1X,F6.3,
!           LWAD SWAD GWAD RWAD VWAD CWAD G#AD G#AD2 GWGD HIAD(real) SHAD,
     &        8(1X,I6),                           1X,I6,1X,F6.3, 1X, I6,
!          &        7(1X,I6),                         1X,I6,1X,F6.3, 2(1X,I6),
!            WSPD WSGD SLFT NSTD EWSD PST1A PST2A KSTD  LN%D   TPSM   HIPD
     &        8(1X,F6.2),                      1X,F6.2,1X,F6.1,1x,F6.2,
!            PWDD PWTD     SLAD  CHTD CWID    RDPD
     &         2(1X,I6),1X,F8.1,2(1X,F6.2),1X,F6.4)

            WRITE(NOUTDG,402,ADVANCE='NO')(RLV(I),I=1,N_LYR)
 402          FORMAT (10F8.3)

            WRITE(NOUTDG,404)
     &        NINT(WTCO*10.),NINT(WTLO*10.),NINT(WTSO*10.),
     &         NINT(CUMSENSURF), NINT(CUMSENSOIL), DTT,
     &         sumstgdtt(istage)
 404        FORMAT (3(1X,I6), 2I8, 1X, F7.3, 1X, F8.3)

          ENDIF

!         Set average stress factors since last printout back to zero
          SWF_AV = 0.0
          TUR_AV = 0.0
          NST_AV = 0.0
          EXW_AV = 0.0
          PS1_AV = 0.0
          PS2_AV = 0.0
          KST_AV = 0.0

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
      END SUBROUTINE TF_OPGROW
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
! RUN     Report number for sequenced or multi-season runs
! nfact() Nitrogen stress factors (1=no stress, 0=max stress)  (Nwheat)
! OUTG    Growth output file name (typically 'GROWTH.OUT')
! PCNL    Percentage of N in leaf tissue (100 g[N] / g[leaf])
! PODNO   Total number of pods (#/m2)
! PODWT   Leaf sheath dry weight, including C and N (g[leaf sheath]/m2[ground])
! PODWTD  Mass of detached pods (g[pods] / m2[ground])
! RLV(L)  Root length density for soil layer L ((cm root / cm3 soil))
! RSTAGE  Number of RSTAGES which have occurred.
! RTDEP   Root depth (cm)
! RTWT    Dry mass of root tissue, including C and N (g[root] / plant)
! SATFAC  Root length weighted soil water excess stress factor ( 0 = no
!           stress; 1 = saturated stress )
! SDSIZE  Average mass of seeds (mg / seed)
! SEEDNO  Total number of seeds (#/m2), JWZ found that it is #/plant
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
!       END SUBROUTINE TF_OPGROW
!=======================================================================

