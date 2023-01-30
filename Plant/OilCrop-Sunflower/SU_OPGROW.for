C======================================================================
C  SU_OPGROW, Subroutine, G. Hoogenboom, J.W. Jones
C----------------------------------------------------------------------
C  Generates output file for daily growth variables
C     This routine is used for maize, sorghum and millet.
C----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1990     Written
C  09/21/1998 CHP Split off from OPDAY.for file
C  05/11/1999 GH  Incorporated in CROPGRO
C  12/18/2001 WDB Revised for modular CERES
C  08/20/2002 GH  Modified for Y2K
C  07/08/2003 CHP Changed senescence output.
!  05/31/2006 CHP Output DTT, growing degree days.
C----------------------------------------------------------------------
C  Called by: MAIZE, SG_CERES, ML_CERES
C  Calls:     None
!======================================================================
      SUBROUTINE SU_OPGROW(CONTROL, ISWITCH, 
     &  CANHT, CANWH, DTT, HI, HIO, KSTRES, MDATE, NLAYR, NSTRES, 
     &  PCNL, PLTPOP, OILPC, OILWT, PSTRES1, PSTRES2, RLV, RSTAGE, 
     &  RTDEP, RTWT, SATFAC, SDWT, SEEDNO, SENESCE, SHELPC, SLA, 
     &  STMWTO, SWFAC, TOPWT, TURFAC, VSTAGE, WTCO, WTLF, WTLO, 
     &  WTSO, XLAI, YRPLT)

!----------------------------------------------------------------------
      USE ModuleDefs 
      USE ModuleData
      USE CsvOutput   ! VSH
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

      REAL VSTAGE, XLAI, STMWTO, SDWT, WTLF, TOPWT, RTWT, OILWT, SEEDNO
      REAL SLA, PCNL, TURFAC, CANHT, CANWH, HI, SHELPC, SATFAC, KSTRES
      REAL SDSIZE,  PSTRES1, PSTRES2, RTDEP, NSTRES, SWFAC, HIO
      REAL PLTPOP, DTT !, PODWTD
      REAL WTLO, WTSO, WTCO,OILPC
      REAL RLV(NL)
      REAL CUMSENSURF, CUMSENSOIL     !cumul. senes. soil and surface
      
!     Average stresses since last printout
      REAL SWF_AV, TUR_AV, NST_AV, EXW_AV, PS1_AV, PS2_AV, KST_AV

      CHARACTER*1     IDETG
      CHARACTER*6, PARAMETER :: ERRKEY = 'SU_OPG'
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
      
      FMOPT   = ISWITCH % FMOPT   ! VSH
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
          IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
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
          END IF   ! VSH

          CALL GET(SOILPROP)
          N_LYR = MIN(10, MAX(4,SOILPROP%NLAYR))

          IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
          WRITE (NOUTDG, '("!",222(" ") ,"Root Dens. (cm/cm3)",
     &      " by soil depth (cm):",
     &    /,"!",T220,10A8)') (SoilProp%LayerText(L), L=1,N_LYR)

          WRITE (NOUTDG,201, ADVANCE='NO')
  201     FORMAT('@YEAR DOY   DAS   DAP',
     &   '   L#SD   GSTD   LAID   LWAD   SWAD   GWAD',
     &   '   RWAD   VWAD   CWAD   G#AD   GWGD   HIAD   COAD',
     &   '   XO%D   WSPD   WSGD   NSTD   EWSD  PST1A  PST2A',
     &   '   KSTD   LN%D   SH%D   HIOD',
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
          END IF ! VSH
          
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
              TUR_AV = TUR_AV / COUNT
              NST_AV = NST_AV / COUNT
              EXW_AV = EXW_AV / COUNT
              PS1_AV = PS1_AV / COUNT
              PS2_AV = PS2_AV / COUNT
              KST_AV = KST_AV / COUNT
              COUNT = 0
            ENDIF

            VWAD = NINT(WTLF*10. + STMWTO*10.)
            IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
            WRITE(NOUTDG,400,ADVANCE='NO')
     &        YEAR, DOY, DAS, DAP,VSTAGE,RSTAGE,XLAI,
     &        NINT(WTLF*10.),NINT(STMWTO*10.),NINT(SDWT*10.),
     &        NINT(RTWT*10.*PLTPOP), VWAD, NINT(TOPWT*10.),
     &        NINT(SEEDNO),SDSIZE,HI,NINT(OILWT*10.),NINT(OILPC),
     &        SWF_AV, TUR_AV, NST_AV, EXW_AV, PS1_AV, PS2_AV, KST_AV,
     &        PCNL,SHELPC,HIO, 
     &        SLA,CANHT,CANWH, (RTDEP/100.)
 400        FORMAT (1X,I4,1X,I3.3,2(1X,I5),1X,F6.1,1X,I6,1X,F6.2,
     &        7(1X,I6),1X,F6.1,1X,F6.3,2(1X,I6),7(1X,F6.3),3(1X,F6.2),
     &        1X,F8.1,2(1X,F6.2),1X,F6.2)

            WRITE(NOUTDG,402,ADVANCE='NO')(RLV(I),I=1,N_LYR)
 402          FORMAT (10F8.2)
            WRITE(NOUTDG,404)
     &        NINT(WTCO*10.),NINT(WTLO*10.),NINT(WTSO*10.),
     &         NINT(CUMSENSURF), NINT(CUMSENSOIL), DTT
 404          FORMAT (3(1X,I6), 2I8, F6.2)
 
            END IF   ! VSH
            
 !    VSH CSV output corresponding to PlantGro.OUT
      IF (FMOPT == 'C') THEN    
         CALL CsvOut_SUOIL(EXPNAME,CONTROL%RUN,CONTROL%TRTNUM, 
     &CONTROL%ROTNUM,CONTROL%REPNO, YEAR, DOY, DAS, DAP, 
     &VSTAGE, RSTAGE, XLAI, WTLF, STMWTO, SDWT, RTWT, PLTPOP, VWAD, 
     &TOPWT, SEEDNO, SDSIZE, HI, OILWT, OILPC, SWF_AV, TUR_AV, NST_AV, 
     &EXW_AV, PS1_AV, PS2_AV, KST_AV, PCNL, SHELPC, HIO, SLA,
     &CANHT, CANWH, RTDEP, N_LYR, RLV, WTCO, WTLO, WTSO, CUMSENSURF, 
     &CUMSENSOIL, DTT, vCsvlineSUOIL, vpCsvlineSUOIL, vlngthSUOIL)
 
          CALL LinklstSUOIL(vCsvlineSUOIL)
      END IF
  
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
      ELSEIF ((DYNAMIC .EQ. SEASEND) 
     & .AND. (FMOPT == 'A' .OR. FMOPT == ' ')) THEN
        !Close daily output files.
        CLOSE (NOUTDG)

      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE SU_OPGROW
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
! HIO     Ratio of OIL weight (OILWT) to weight of above-ground portion of 
!           plant (TOPWT) (g[oil] / g[tops])
! MODEL   Name of CROPGRO executable file 
! NL      maximum number of soil layers = 20 
! NOUTDG  Unit number for growth output file 
! RUN    Report number for sequenced or multi-season runs 
! NSTRES  Nitrogen stress factor (1=no stress, 0=max stress) 
! OUTG    Growth output file name (typically 'GROWTH.OUT') 
! PCNL    Percentage of N in leaf tissue (100 g[N] / g[leaf])
! OILPC   Percent oil in grains
! OILWT   Oil mass (g/m2)
! RLV(L)  Root length density for soil layer L ((cm root / cm3 soil))
! RSTAGE  Number of RSTAGES which have occurred. 
! RTDEP   Root depth (cm)
! RTWT    Dry mass of root tissue, including C and N (g[root] / plant)
! SATFAC  Root length weighted soil water excess stress factor ( 0 = no 
!           stress; 1 = saturated stress ) 
! SDSIZE  Average mass of seeds (mg / seed)
! SEEDNO  Total number of seeds (#/m2)
! SHELLW  Shell mass (g[shell] / m2) !!! In OilcropSun this refers to pericarp mass
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
!       END SUBROUTINE SU_OPGROW
!=======================================================================

