C=======================================================================
C  OPGROW, Subroutine, G. Hoogenboom, J.W. Jones
C-----------------------------------------------------------------------
C  Generates output file for daily growth variables
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  01/01/1990 GH  Written
C  09/21/1998 CHP Split off from OPDAY.for file
C  05/11/1999 GH  Incorporated in CROPGRO
C  06/19/2001 GH  Modified output format
C  08/20/2002 GH  Modified for Y2K
C  07/08/2003 CHP Changed senescence output.
C  03/24/2004 CHP Added P stresses to PlantGro.out
C  11/08/2023  FO Added LINTW and LINTP for lint growth outputs.
C-----------------------------------------------------------------------
C  Called by: PLANT
C  Calls:     None
!=======================================================================
      SUBROUTINE OPGROW(CONTROL, ISWITCH, SoilProp, 
     &    CADLF, CADST, CANHT, CANWH, CMINEA, DWNOD, GROWTH,  
     &    GRWRES, KSTRES, MAINR, MDATE, NFIXN, NLAYR, NSTRES, 
     &    PCLSD, PCCSD, PCNL, PCNRT, PCNSD, PCNSH, PCNST, PG, 
     &    PODNO, PODWT, PODWTD, PSTRES1, PSTRES2, RHOL, RHOS, 
     &    RLV, RSTAGE, RTDEP, RTWT, SATFAC, SDWT, SEEDNO, 
     &    SENESCE, SLA, STMWT, SWFAC, TGRO, TGROAV, TOPWT, 
     &    TOTWT, TURFAC, VSTAGE, WTLF, WTNCAN, WTNLF, WTNST, 
     &    WTNSD, WTNUP, WTNFX, XLAI, YRPLT, LINTW, LINTP)
!    &    EOP, TRWUP, WRDOTN)

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
!     VSH
      USE CsvOutput 
      USE Linklist
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, TIMDIF, YR_DOY
      SAVE
!-----------------------------------------------------------------------
      CHARACTER*1  IDETG, ISWPHO, ISWPOT
      CHARACTER*2  CROP
      CHARACTER*6, PARAMETER :: ERRKEY = 'OPGROW'
!      CHARACTER*8  FNAME
      CHARACTER*12 OUTG, OUTPC, OUTPN

      INTEGER COUNT, DAP, DAS, DOY, DYNAMIC, ERRNUM, FROP, I, L
      INTEGER N_LYR, NLAYR, NOUTDG, NOUTPC, NOUTPN, RUN, RSTAGE
      INTEGER TIMDIF, YEAR, YRDOY, MDATE, YRPLT, VWAD

      REAL CADLF, CADST, CANHT, CANWH, CMINEA, DWNOD
      REAL GROWTH, GRWRES, HI, HIP, MAINR, NSTRES
      REAL PCLSD, PCCSD, PCNL, PG, PODNO, PODWT, PODWTD
      REAL RHOL, RHOS, RTDEP, RTWT, STMWT, SDWT, SEEDNO
      REAL SDSIZE, SHELLW, SHELPC, SLA, LINTW, LINTP
      REAL SATFAC, SWFAC, TGROAV, TOPWT, TOTWT, TURFAC
      REAL VSTAGE, WTLF, XLAI
!     REAL EOP, TRWUP, WRDOTN

      REAL PCCSDP, PCLSDP, PCNLP, PCNRTP, PCNSDP
      REAL PCNSHP, PCNSTP, RHOLP, RHOSP, SLAP

      REAL RLV(NL)
      REAL TGRO(TS)

      REAL WTNCAN,WTNLF,WTNST,WTNSD,WTNUP,WTNFX
      REAL WTNVEG,PCNVEG,NFIXN    
      REAL PCNST,PCNRT,PCNSH,PCNSD

      REAL CUMSENSURF,  CUMSENSOIL  !cumul. senesced matter, soil & surf
      REAL CUMSENSURFN, CUMSENSOILN !cumul. senes. N soil and surface

!     Average stresses since last printout
      REAL SWF_AV, TUR_AV, NST_AV, EXW_AV, PS1_AV, PS2_AV, KST_AV

      LOGICAL FEXIST, FIRST

!     P, K modules
      REAL PStres1, PStres2, KSTRES

!     Temp
      REAL SENSURFT, SENSOILT

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (ResidueType) SENESCE
      TYPE (SoilType)    SOILPROP

!     No output for fallow crop
      CROP    = CONTROL % CROP
      IDETG   = ISWITCH % IDETG
      IF (CROP .EQ. 'FA' .OR. IDETG .EQ. 'N') RETURN

!     Transfer values from constructed data types into local variables.
      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FROP    = CONTROL % FROP
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY

      ISWPHO = ISWITCH % ISWPHO
      ISWPOT = ISWITCH % ISWPOT
          
      FMOPT  = ISWITCH % FMOPT    ! VSH

!***********************************************************************
!***********************************************************************
!     Run initialization - run once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN    ! VSH
        OUTG  = 'PlantGro.OUT'
        CALL GETLUN('OUTG',  NOUTDG)

        OUTPN  = 'PlantN.OUT  '
        CALL GETLUN('OUTPN', NOUTPN)

        OUTPC  = 'PlantC.OUT  '
        CALL GETLUN('OUTPC', NOUTPC)
        END IF    ! VSH

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN    ! VSH
!       Initialize daily growth output file      
        INQUIRE (FILE = OUTG, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTDG, FILE = OUTG, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
          FIRST = .FALSE.
        ELSE
          OPEN (UNIT = NOUTDG, FILE = OUTG, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(NOUTDG,'("*GROWTH ASPECTS OUTPUT FILE")')
          FIRST = .TRUE.
        ENDIF

        !Write headers
        CALL HEADER(SEASINIT, NOUTDG, RUN)
        END IF    ! VSH
        
        N_LYR = MIN(10, MAX(4,SOILPROP%NLAYR))

        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN    ! VSH
!        IF (ISWPHO .NE. 'N') THEN
          WRITE (NOUTDG, 100) "Root Dens. (cm/cm3) by soil ",
     &      "depth (cm):",(SoilProp%LayerText(L), L=1,N_LYR)
  100     FORMAT("!",244X,A,A,/,"!",239X,10A8) 
!        ELSE
!          WRITE (NOUTDG,102) (SoilProp%LayerText(L), L=1,N_LYR)
!  102     FORMAT("!",216X,"Soil Layer depths (cm):",/,"!",211X,10A8)
!        ENDIF

        WRITE (NOUTDG,200, ADVANCE='NO')
  200   FORMAT('@YEAR DOY   DAS   DAP',
     &         '   L#SD   GSTD   LAID   LWAD   SWAD   GWAD')

        IF(CROP .EQ. 'CO') THEN
         WRITE (NOUTDG,'(A)', ADVANCE='NO') '  LIWAM  LINTP'
        ENDIF

        WRITE (NOUTDG,205, ADVANCE='NO')
  205   FORMAT('   RWAD   VWAD   CWAD   G#AD    GWGD   HIAD   PWAD',
     &         '   P#AD   WSPD   WSGD   NSTD')

!        IF (ISWPHO .NE. 'N') THEN
          WRITE (NOUTDG,"('  PST1A  PST2A')", ADVANCE='NO')
!        ENDIF
!        IF (ISWPOT .EQ. 'Y') THEN
          WRITE (NOUTDG,"('   KSTD')", ADVANCE='NO')
!        ENDIF

        WRITE (NOUTDG,214, ADVANCE='NO')
  214   FORMAT('   EWSD    LN%D',
     &         '   SH%D   HIPD   PWDD   PWTD   SLAD   CHTD',
     &         '   CWID   NWAD   RDPD')

          DO L = 1, N_LYR
            IF (L < 10) THEN
              WRITE (NOUTDG,'("    ",A2,I1,A1)',ADVANCE='NO') "RL",L,"D"
            ELSE
              WRITE (NOUTDG,'("   ",A2,I2,A1)', ADVANCE='NO') "RL",L,"D"
            ENDIF
          ENDDO

        WRITE (NOUTDG,220)
  220   FORMAT('    SNW0C   SNW1C') !   SNW0D   SNW1D',
!     &         '     EOP   TRWUP  WRDOTN')

!-----------------------------------------------------------------------
!       Initialize daily plant nitrogen output file
        INQUIRE (FILE = OUTPN, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTPN, FILE = OUTPN, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
          FIRST = .FALSE.
        ELSE
          OPEN (UNIT = NOUTPN, FILE = OUTPN, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(NOUTPN,'("*PLANT N OUTPUT FILE")')
          FIRST = .TRUE.
        ENDIF

        !Write headers
        CALL HEADER(SEASINIT, NOUTPN, RUN)

        WRITE (NOUTPN,230)
  230   FORMAT('@YEAR DOY   DAS   DAP',
     &      '    CNAD    GNAD    VNAD    GN%D    VN%D     NFXC    NUPC',
     &      '    LNAD    SNAD    LN%D    SN%D    SHND',
!CHP &      '  RN%D  NFXD')
     &      '   RN%D   NFXD   SNN0C   SNN1C')

!-----------------------------------------------------------------------
!       Initialize daily plant carbon output file
        INQUIRE (FILE = OUTPC, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTPC, FILE = OUTPC, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
          FIRST = .FALSE.
        ELSE
          OPEN (UNIT = NOUTPC, FILE = OUTPC, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(NOUTPC,'("*PLANT C OUTPUT FILE")')
          FIRST = .TRUE.
        ENDIF

        !Write headers
        CALL HEADER(SEASINIT, NOUTPC, RUN)

        WRITE (NOUTPC,250)
  250   FORMAT('@YEAR DOY   DAS   DAP   TWAD    PHAD',
     &      '    CMAD    CGRD    GRAD    MRAD    CHAD   CL%D   CS%D',
     &      '   TGNN   TGAV    GN%D    GL%D    GC%D')
        END IF ! VSH
        
        CUMSENSURF  = 0.0
        CUMSENSOIL  = 0.0
        CUMSENSURFN = 0.0
        CUMSENSOILN = 0.0 
        SWF_AV = 0.0
        TUR_AV = 0.0
        NST_AV = 0.0
        EXW_AV = 0.0
        PS1_AV = 0.0
        PS2_AV = 0.0
        KST_AV = 0.0
        LINTP  = 0.0

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
C-----------------------------------------------------------------------
C   CHECK FOR OUTPUT FREQUENCY
C-----------------------------------------------------------------------
        IF (YRDOY .LT. YRPLT .OR. YRPLT .LT. 0) RETURN

!       Compute average stress factors since last printout
        SWF_AV = SWF_AV + (1.0 - SWFAC)
        TUR_AV = TUR_AV + (1.0 - TURFAC)
        NST_AV = NST_AV + (1.0 - NSTRES)
        EXW_AV = EXW_AV + SATFAC
        PS1_AV = PS1_AV + (1.0 - PSTRES1)
        PS2_AV = PS2_AV + (1.0 - PSTRES2)
        KST_AV = KST_AV + (1.0 - KSTRES)
        COUNT = COUNT + 1

!       Accumulate senesced matter for surface and soil.
        SENSURFT = SENESCE % ResWt(0)
        CUMSENSURF  = CUMSENSURF  + SENESCE % ResWt(0)
        CUMSENSURFN = CUMSENSURFN + SENESCE % ResE(0,1) 
      
        SENSOILT = 0.0
        DO L = 1, NLAYR
          SENSOILT    = SENSOILT    + SENESCE % ResWt(L)
          CUMSENSOIL  = CUMSENSOIL  + SENESCE % ResWt(L)
          CUMSENSOILN = CUMSENSOILN + SENESCE % ResE(L,1)
        ENDDO

        IF ((MOD(DAS,FROP) .EQ. 0)        !Daily output every FROP days,
     &    .OR. (YRDOY .EQ. YRPLT)         !on planting date, and
     &    .OR. (YRDOY .EQ. MDATE)) THEN   !at harvest maturity 

          DAP = MAX(0,TIMDIF(YRPLT,YRDOY))
          IF (DAP > DAS) DAP = 0
          CALL YR_DOY(YRDOY, YEAR, DOY) 

          !Prior to emergence, do not report %N, %C values
!Hold off on this until GBuild can handle -99 values
!          IF (DAS .GE. NVEG0) THEN
            RHOLP  = RHOL * 100.
            RHOSP  = RHOS * 100.
            SLAP   = SLA
            PCNLP  = PCNL
            PCNSTP = PCNST
            PCNRTP = PCNRT
!          ELSE
!            RHOLP  = -99.
!            RHOSP  = -99.
!            SLAP   = -99.
!            PCNLP  = -99.
!            PCNSTP = -99.
!            PCNRTP = -99.
!          ENDIF

          !Prior to grain growth, do not report % compositions of seed
!Hold off on this until GBuild can handle -99 values
!          IF (SDWT .GT. 0.0001) THEN
            PCNSDP = PCNSD
            PCLSDP = PCLSD
            PCCSDP = PCCSD
            PCNSHP = PCNSH
!          ELSE
!            PCNSDP = -99.
!            PCLSDP = -99.
!            PCCSDP = -99.
!            PCNSHP = -99.
!          ENDIF

          IF (PODWT .GT. 0.1) THEN
            SHELPC = SDWT*100./PODWT
          ELSE
            SHELPC = 0.0
          ENDIF

          SHELPC = MIN(SHELPC,99.99)
          SHELLW = PODWT - SDWT       !Not used

          IF (SEEDNO .GT. 1.E-4) THEN
            SDSIZE = SDWT/SEEDNO*1000
          ELSE
            SDSIZE = 0.0
          ENDIF

          IF (TOPWT .GT. 1.E-4 .AND. SDWT .GE. 1.E-4) THEN
            HI = SDWT/TOPWT
          ELSE
            HI = 0.
          ENDIF

          IF (TOPWT .GT. 1.E-4 .AND. PODWT .GE. 1.E-4) THEN
            HIP = PODWT/TOPWT
          ELSE
            HIP = 0.
          ENDIF

!         Compute average stress factors since last printout
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

          VWAD = NINT(WTLF*10. + STMWT*10.)
          
          IF(CROP .EQ. 'CO' .AND. SDWT .GT. 0.0) THEN
            LINTP = (LINTW*100.)/SDWT
          ENDIF

          IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
          WRITE (NOUTDG,300, ADVANCE='NO')
     &        YEAR, DOY, DAS, DAP, VSTAGE, RSTAGE, XLAI,
     &        NINT(WTLF*10.), NINT(STMWT*10.), NINT(SDWT*10.)
     
  300     FORMAT (1X,I4,1X,I3.3,2(1X,I5),
     &        1X,F6.1,1X,I6,1X,F6.3,3(1X,I6))

          IF(CROP .EQ. 'CO') THEN
            WRITE (NOUTDG,305, ADVANCE='NO')
     &        NINT(LINTW*10.), LINTP
  305       FORMAT (1X,I6,1X,F6.2)
          ENDIF

          WRITE (NOUTDG,310, ADVANCE='NO')
     &        NINT(RTWT*10.), VWAD, NINT(TOPWT*10.), NINT(SEEDNO), 
     &        SDSIZE, HI, NINT(PODWT*10.), NINT(PODNO), SWF_AV, TUR_AV,
     &        NST_AV
  310     FORMAT (4(1X,I6),
     &        1X,F7.1,1X,F6.3,2(1X,I6),2(1X,F6.3),
     &        1X,F6.3)

!          IF (ISWPHO .NE. 'N') THEN
            WRITE (NOUTDG,'(2(1X,F6.3))', ADVANCE='NO') PS1_AV, PS2_AV
!          ENDIF

!          IF (ISWPOT .EQ. 'Y') THEN
            WRITE (NOUTDG,'(1X,F6.3)', ADVANCE='NO') KST_AV
!          ENDIF

          WRITE (NOUTDG,314, ADVANCE='NO') 
     &        EXW_AV, PCNLP, SHELPC, HIP, NINT(PODWTD*10.),
     &        NINT((PODWTD+PODWT)*10.), SLAP, CANHT, CANWH, (DWNOD*10.),
     &        (RTDEP/100.), (RLV(I),I=1,N_LYR)
  314     FORMAT (1X,F6.3,1X,F7.2,2(1X,F6.2),
     &        2(1X,I6),1X,F6.1,2(1X,F6.2),1X,F6.1,1X,F6.2,11(1X,F7.2))

          WRITE (NOUTDG,316) 
     &        NINT(CUMSENSURF), NINT(CUMSENSOIL)   !, SENSURFT, SENSOILT
!     &        , EOP, TRWUP, WRDOTN
  316     FORMAT (I8,1X,I7, 2F8.3, 3F8.4)
          END IF   ! VSH
!-----------------------------------------------------------------------
!     VSH CSV output corresponding to PlantGro.OUT
      IF (FMOPT == 'C') THEN
         CALL CsvOut(EXPNAME,CONTROL%RUN, CONTROL%TRTNUM,CONTROL%ROTNUM
     &,CONTROL%REPNO, YEAR, DOY, DAS, DAP, VSTAGE, RSTAGE, XLAI, 
     &WTLF, STMWT, SDWT, LINTW, LINTP,
     &RTWT, VWAD, TOPWT, SEEDNO, SDSIZE, HI, PODWT,
     &PODNO, SWF_AV, TUR_AV, NST_AV, PS1_AV, PS2_AV, KST_AV, EXW_AV,
     &PCNLP, SHELPC, HIP, PODWTD, SLAP, CANHT, CANWH,
     &DWNOD, RTDEP, N_LYR, RLV, CUMSENSURF, CUMSENSOIL,
     &vCsvline, vpCsvline, vlngth)
     
         CALL Linklst(vCsvline)
      END IF
      
!         Set average stress factors since last printout back to zero
          SWF_AV = 0.0
          TUR_AV = 0.0
          NST_AV = 0.0
          EXW_AV = 0.0
          PS1_AV = 0.0
          PS2_AV = 0.0
          KST_AV = 0.0

!            WRITE (NOUTDG,312)YEAR, DOY, DAS, DAP, VSTAGE, RSTAGE, XLAI,
!  312       FORMAT (1X,I4,1X,I3.3,2(1X,I5),1X,F6.1,1X,I6,1X,F6.2,

!     &        NINT(WTLF*10.), NINT(STMWT*10.), NINT(SDWT*10.),
!     &        NINT(RTWT*10.), NINT(TOPWT*10.), NINT(SEEDNO), 
!     &        6(1X,I6),

!    &         SDSIZE, HI,
!     &        1X,F7.1,1X,F6.3,

!     &        NINT(PODWT*10.), NINT(PODNO), (1.-SWFAC), (1.-TURFAC),
!     &        (1.-NSTRES), (1.-PSTRES1), (1.-PSTRES2), SATFAC, 
!     &        2(1X,I6),6(1X,F6.3),

!     &        PCNLP, SHELPC, HIP, 
!     &        1X,F7.2,2(1X,F6.2),

!     &        NINT(PODWTD*10.),NINT((PODWTD+PODWT)*10.), SLAP, CANHT, CANWH, 
!     &        2(1X,I6),1X,F6.1,2(1X,F6.2),

!     &        (DWNOD*10.),(RTDEP/100.), (RLV(I),I=1,10),
!     &        1X,F6.1,11(1X,F6.2),

!     &        NINT(CUMSENSURF), NINT(CUMSENSOIL), SENSURFT, SENSOILT
!     &        , EOP, TRWUP, WRDOTN
!     &        2(1X,I7), 2F8.3, 3F8.3)
C-----------------------------------------------------------------------
          WTNVEG  = (WTNLF + WTNST)
      
          IF ((WTLF+STMWT).GT. 0.0001) THEN
            PCNVEG = (WTNLF+WTNST)/(WTLF+STMWT)*100.
          ELSE
            !PCNVEG = -99.    !Wait for GBuild fix for -99's
            PCNVEG = 0.
          ENDIF

          IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN       ! VSH
          WRITE (NOUTPN,410) YEAR, DOY, DAS, DAP, (WTNCAN*10), 
     &       (WTNSD*10), (WTNVEG*10), PCNSDP, PCNVEG, (WTNFX*10),
     &       (WTNUP*10), (WTNLF*10), (WTNST*10), PCNLP, PCNSTP,
     &       PCNSHP, PCNRTP, NFIXN*10, CUMSENSURFN, CUMSENSOILN
  410     FORMAT(1X,I4,1X,I3.3,2(1X,I5),3(1X,F7.1),2(1X,F7.2),1X,
     &       2(1X,F7.1),2(1X,F7.1),2(1X,F7.2),1X,F7.1,2(1X,F6.1),
     &       2(1X,F7.2))
          END IF    ! VSH
          
          !     VSH
      IF (FMOPT == 'C') THEN
         CALL CsvOutPlNCrGro(EXPNAME, CONTROL%RUN, CONTROL%TRTNUM, 
     &CONTROL%ROTNUM, CONTROL%REPNO, YEAR, DOY, DAS, DAP, 
     &WTNCAN, WTNSD, WTNVEG, PCNSDP, PCNVEG, WTNFX, WTNUP, 
     &WTNLF, WTNST, PCNLP, PCNSTP, PCNSHP, PCNRTP, NFIXN, 
     &CUMSENSURFN, CUMSENSOILN,
     &vCsvlinePlNCrGro, vpCsvlinePlNCrGro, vlngthPlNCrGro)
     
         CALL LinklstPlNCrGro(vCsvlinePlNCrGro)
      
         CALL CsvOutPlCCrGro(EXPNAME, CONTROL%RUN, CONTROL%TRTNUM, 
     &CONTROL%ROTNUM, CONTROL%REPNO, YEAR, DOY, DAS, DAP, 
     &TOTWT, PG, CMINEA, GROWTH, GRWRES, MAINR, CADLF, 
     &CADST, RHOLP, RHOSP, TGRO, TGROAV, PCNSDP, PCLSDP, 
     &PCCSDP, TS, 
     &vCsvlinePlCCrGro, vpCsvlinePlCCrGro, vlngthPlCCrGro)
      
         CALL LinklstPlCCrGro(vCsvlinePlCCrGro)    
      END IF
C-----------------------------------------------------------------------
          IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN ! VSH
          WRITE (NOUTPC,510) YEAR, DOY, DAS, DAP,
     &        NINT(TOTWT*10), PG, CMINEA, GROWTH,
     &        GRWRES, MAINR, (CADLF + CADST), RHOLP, RHOSP,
     &        TGRO(TS/2), TGROAV, PCNSDP, PCLSDP, PCCSDP
C       changed from 12 to TS/2 on 9Jul17 by Bruce Kimball
  510     FORMAT(1X,I4,1X,I3.3,2(1X,I5),1X,I6,5(F8.4),F8.5,2(F7.3),
     &        2(1X,F6.3),3(1X,F7.4))
!  510     FORMAT(1X,I4,1X,I3.3,2(1X,I5),1X,I6,6(1X,F7.2),2(1X,F6.1),
!     &        2(1X,F6.1),3(1X,F7.2))
          END IF   ! VSH
        ENDIF       

!***********************************************************************
!***********************************************************************
!     Seasonal Output 
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
        !Close daily output files.
        CLOSE (NOUTDG)
        CLOSE (NOUTPN)
        CLOSE (NOUTPC)
        END IF   ! VSH

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END ! SUBROUTINE OPGROW
!=======================================================================


!=======================================================================
!       Variable definitions for OPGROW
!-----------------------------------------------------------------------
! CANHT   Canopy height (m)
! CANWH   Canopy width normal to row (m)
! DAP     Number of days after planting (d)
! DWNOD   Current nodule mass (g[nodule] / m2)
! HI      Ratio of seed weight (SDWT) to weight of above-ground portion of 
!           plant (TOPWT) (g[seed] / g[tops])
! HIP     Ratio of pod weight (PODWT) to weight of above-ground portion of 
!           plant (TOPWT) (g[pods] / g[tops])
! LINTP   Lint Percetange (%)
! MODEL   Name of CROPGRO executable file 
! MULTI   Current seasonal simulation (=1 for first or only simulation, 
!           =NYRS for last simulation 
! NL      maximum number of soil layers = 20 
! NOUTDG  Unit number for growth output file 
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
! RTWT    Dry mass of root tissue, including C and N (g[root] / m2[ground])
! SDSIZE  Average mass of seeds (mg / seed)
! SDWT    Dry mass of seed tissue, including C and N (g[seed] / m2[ground])
! SEEDNO  Total number of seeds (#/m2)
! SHELLW  Shell mass (g[shell] / m2)
! SHELPC  Percentage of pod mass that is seeds (g[seed]/g[pods] * 100%)
! SLA     Specific leaf area (cm2[leaf] / m2[ground])
! STMWT   Dry mass of stem tissue, including C and N (g[stem] / m2[ground)
! SWFAC   Effect of soil-water stress on photosynthesis, 1.0=no stress, 
!           0.0=max stress 
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
!       END SUBROUTINE OPGROW
!=======================================================================

