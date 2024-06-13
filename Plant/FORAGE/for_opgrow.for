C=======================================================================
C  FOR_OPGROW, Subroutine, G. Hoogenboom, J.W. Jones
C-----------------------------------------------------------------------
C  Generates output file for daily growth variables
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  01/01/1990 GH  Written
C  09/21/1998 CHP Split off from OPDAY.FOR file
C  05/11/1999 GH  Incorporated in CROPGRO
C  06/19/2001 GH  Modified output format
C  08/20/2002 GH  Modified for Y2K
C-----------------------------------------------------------------------
C  Called by: PLANT
C  Calls:     None
!=======================================================================
      SUBROUTINE FOR_OPGROW(CONTROL, ISWITCH, 
     &    CADLF, CADST, CANHT, CANWH, CMINEA, DWNOD,  
     &    GROWTH, GRWRES, MAINR, MDATE, NFIXN, NSTRES, 
     &    PCLSD, PCCSD, PCNL, PCNRT, PCNSD, PCNSH, PCNST, 
     &    PG, PODNO, PODWT, PODWTD, RHOL, RHOS, RLV, RSTAGE, 
     &    RTDEP, RTWT, SATFAC, SDWT, SEEDNO, SLA, STMWT, SWFAC, 
     &    TGRO, TGROAV, TOPWT, TOTWT, TURFAC, VSTAGE, WTCO, 
     &    WTLF, WTLO, WTNCAN, WTNLF, WTNST, WTNSD, WTNUP, 
     &    WTNFX, WTSO, XLAI, YRPLT,
     &    DRMST, PPGFAC, PPMFAC, PPTFAC, SRFTEMP, ST, FREEZ2,
     &    AGRSTR, CADSR, CMOBSR, CPFSTR, CRUSSR, CSRFRZ, CSRW, 
     &    CSTRM, DSTOR, FNINSR, FNINSRG, FRSTR, FRSTRM, NADSR, 
     &    NGRSR, NGRSRG, NMOBSR, NRUSSR, NSRALL, NSRDOT, NSROFF, 
     &    NVSTSR, PCNSR, PCSTRD, PROSRT, PSRSRFD, PSRLYRD, 
     &    PSRSRFL, PSRLYR1, RHOSR, SRDAM, SRSRFD, SRLYRD, SSRDOT, 
     &    SSRNDOT, STRWT, TPSRSRFL, TPSRLYR1, WCRSR, WNRSR, 
     &    WRCSRDT, WSRDOT, WSRDOTN, WSRFDOT, WSRI, WSRIDOT, 
     &    WTNSR, WTNSRA, WTNSRO, WTSRO, XSTR,
     &    FRLF, FRSTM, FRRT,
     &    FHWAH, FHLPH, DWTCO, DWTLO, DWTSO,fhpctn,
     &    RHOR)

!     2023-01-19 CHP Removed unused variables in argument list:
!    &  PROSRF, PROSRG, PROSRI, PCARSR, PLIGSR, 
!    &  PLIPSR, POASR, PMINSR, ALPHSR, CMOBSRX, CADSRF, NMOBSRX, CLAIT,
!    &  YSTOR, FRSTRF, FRSTRMX, STRSRFL, STRLYR1, SENSR,
!    &  FNPTD, TYPPTD, FNPMD, TYPPMD, FNPGD, TYPPGD, HARD1, HARD2,
!    &  FRZDC, FRZHRD, TYPHRD, FRZDHD, TYPDHD, RDRMG, RDRMM, RDRMT, 
!    &  RCHDP, MOWC, RSPLC
!     
!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
        ! which contain control information, soil
        ! parameters, hourly weather data.
      !     VSH
      USE CsvOutput 
      USE Linklist
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, YR_DOY, TIMDIF
      SAVE
!-----------------------------------------------------------------------
      CHARACTER*1  IDETG, IDETL, RNMODE
      CHARACTER*2  CROP
      CHARACTER*6, PARAMETER :: ERRKEY = 'OPGROW'
!      CHARACTER*8  FNAME
      CHARACTER*12 OUTG, OUTPC, OUTPN, OUTDRM, OUTSTOR  !, OUTINSTR

      CHARACTER*30 FILEIO

      INTEGER DAP, DAS, DOY, DYNAMIC, ERRNUM, FROP, I
      INTEGER NOUTDG, NOUTPC, NOUTPN, RUN, RSTAGE
      INTEGER NOUTDRM, NOUTSTOR  !, NOUTINSTR 
      INTEGER TIMDIF, YEAR, YRDOY, MDATE, YRPLT, YRSIM
      
      INTEGER :: N_LYR = 10   ! VSH

      REAL CADLF, CADST, CANHT, CANWH, CMINEA, DWNOD
      REAL GROWTH, GRWRES, HI, HIP, MAINR, NSTRES
      REAL PCLSD, PCCSD, PCNL, PG, PODNO, PODWT, PODWTD
      REAL RHOL, RHOS, RTDEP, RTWT, STMWT, SDWT, SEEDNO
      REAL SDSIZE, SHELLW, SHELPC, SLA
      REAL SATFAC, SWFAC, TGROAV, TOPWT, TOTWT, TURFAC
      REAL VSTAGE, WTCO, WTLF, WTLO, WTSO, XLAI
      REAL RLV(NL)
      REAL TGRO(TS)

      REAL FRLF, FRSTM, FRRT, FHWAH, FHLPH, PELF
      
      REAL WTNCAN,WTNLF,WTNST,WTNSD,WTNUP,WTNFX
      REAL WTNVEG,PCNVEG,NFIXN
      REAL PCNST,PCNRT,PCNSH,PCNSD
!      REAL CADSR, PCNSR, PSRSRFD, PSRSRFL, RHOSR, STRWT,
!     &  WTNSR, WTSRO
      REAL DWTCO, DWTLO, DWTSO
      
      CHARACTER*6 DRMST
!     CHARACTER*3   TYPPGD,TYPPTD, TYPPMD, TYPHRD, TYPDHD

      REAL PPGFAC, PPMFAC, PPTFAC, SRFTEMP, FREEZ2

      REAL AGRSTR, CADSR, CMOBSR, CPFSTR, CRUSSR, CSRFRZ, CSRW, 
     &  CSTRM, DSTOR, FNINSR, FNINSRG, FRSTR, FRSTRM, NADSR, 
     &  NGRSR, NGRSRG, NMOBSR, NRUSSR, NSRALL, NSRDOT, NSROFF, 
     &  NVSTSR, PCNSR, PCSTRD, PROSRT, PSRSRFD, PSRLYRD, 
     &  PSRSRFL, PSRLYR1, RHOSR,RHOR,SRDAM, SRSRFD, SRLYRD, SSRDOT, 
     &  SSRNDOT, STRWT, TPSRSRFL, TPSRLYR1, WCRSR, WNRSR, 
     &  WRCSRDT, WSRDOT, WSRDOTN, WSRFDOT, WSRI, WSRIDOT, 
     &  WTNSR, WTNSRA, WTNSRO, WTSRO, XSTR



!     REAL PROSRF, PROSRG, PROSRI, PCARSR, PLIGSR, 
!    &  PLIPSR, POASR, PMINSR, ALPHSR, CMOBSRX, CADSRF, NMOBSRX, CLAIT,
!    &  YSTOR(8), FRSTRF, FRSTRMX, STRSRFL, STRLYR1, SENSR,
!    &  FNPTD(4), FNPMD(4), FNPGD(4), HARD1, HARD2, 
!    &  FRZDC, FRZHRD(4), 
!    &  FRZDHD(4), RDRMG, RDRMM, RDRMT, RCHDP
      REAL fhpctn!,MOWC,RSPLC

      REAL, DIMENSION(NL) :: ST
C-------------------------------------------

      LOGICAL FEXIST, FIRST

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

!     The variable "ISWITCH" is of type "SwitchType".
      TYPE (SwitchType) ISWITCH

!     No output for fallow crop
      CROP    = CONTROL % CROP
      IDETG   = ISWITCH % IDETG
      IDETL   = ISWITCH % IDETL
      IF (CROP .EQ. 'FA' .OR. IDETG .EQ. 'N' .OR. IDETL .EQ. "0") RETURN

!     Transfer values from constructed data types into local variables.
      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      FROP    = CONTROL % FROP
!      LUNIO   = CONTROL % LUNIO
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM

      IDETG = ISWITCH % IDETG
      
      FMOPT  = ISWITCH % FMOPT    ! VSH

!***********************************************************************
!***********************************************************************
!     Run initialization - run once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
!      IF (IDETG .EQ. 'Y') THEN
        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN    ! VSH
        OUTG  = 'PlantGro.OUT'
        CALL GETLUN('OUTG', NOUTDG)

        OUTPN  = 'PlantN.OUT  '
        CALL GETLUN('OUTPN', NOUTPN)

        OUTPC  = 'PlantC.OUT  '
        CALL GETLUN('OUTPC', NOUTPC)

        OUTDRM  = 'Dormancy.OUT'
        CALL GETLUN('OUTDRM', NOUTDRM)

        OUTSTOR  = 'Storage.OUT '
        CALL GETLUN('OUTSTOR', NOUTSTOR)
        END IF    ! VSH
!        OUTINSTR  = 'StorSpIn.OUT'
!        CALL GETLUN('OUTINSTR', NOUTINSTR)

!      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!       Initialize daily growth output file
        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN    ! VSH
        INQUIRE (FILE = OUTG, EXIST = FEXIST)
        IF (FEXIST) THEN
        OPEN (UNIT = NOUTDG, FILE = OUTG, STATUS = 'OLD',
     &  IOSTAT = ERRNUM, ACCESS = 'APPEND')
        FIRST = .FALSE.
        ELSE
        OPEN (UNIT = NOUTDG, FILE = OUTG, STATUS = 'NEW',
     &  IOSTAT = ERRNUM)
        WRITE(NOUTDG,'("*GROWTH ASPECTS OUTPUT FILE")')
        FIRST = .TRUE.
        ENDIF

        !Write headers
!        CALL HEADER(SEASINIT, FILEIO, NOUTDG, RUN)
        CALL HEADER(SEASINIT, NOUTDG, RUN)
        WRITE (NOUTDG,210)
210   FORMAT('@YEAR DOY   DAS   DAP',
     &    '  L#SD  GSTD  LAID  LWAD  SWAD  QWAD  QS%D  Q1%D  GWAD',
     &    '  RWAD  CWAD  G#AD   GWGD  HIAD  PWAD',
     &    '  P#AD  WSPD  WSGD  NSTD  EWSD  LN%D',
     &    '  SH%D  HIPD  PWDD  PWTD  SLAD  CHTD',
     &    '  CWID  NWAD  RDPD  RL1D  RL2D  RL3D',
     &    '  RL4D  RL5D  RL6D  RL7D  RL8D  RL9D',
     &    '  RL10  CDAD  LDAD  SDAD  QDAD  HERB  FHL%  LF%D'
!    &    ' DWTCO DWTLO DWTSO CHTCM CPROT  MOWC  RSPLC')
     &    ' DWTCO DWTLO DWTSO CHTCM CPROT')
!-----------------------------------------------------------------------
!       Initialize daily plant nitrogen output file
        INQUIRE (FILE = OUTPN, EXIST = FEXIST)
        IF (FEXIST) THEN
        OPEN (UNIT = NOUTPN, FILE = OUTPN, STATUS = 'OLD',
     &  IOSTAT = ERRNUM, ACCESS = 'APPEND')
        FIRST = .FALSE.
        ELSE
        OPEN (UNIT = NOUTPN, FILE = OUTPN, STATUS = 'NEW',
     &  IOSTAT = ERRNUM)
        WRITE(NOUTPN,'("*PLANT N OUTPUT FILE")')
        FIRST = .TRUE.
        ENDIF

        !Write headers
!        CALL HEADER(SEASINIT, FILEIO, NOUTPN, RUN)
        CALL HEADER(SEASINIT, NOUTPN, RUN)

        WRITE (NOUTPN,230)
230   FORMAT('@YEAR DOY   DAS   DAP',
     &  '  CNAD  GNAD  VNAD  GN%D  VN%D    NFXC   NUPC',
     &  '  LNAD  SNAD  QNAD  LN%D  SN%D  QN%D  SHND',
     &  '  RN%D  NFXD')

!-----------------------------------------------------------------------
!       Initialize daily plant carbon output file
        INQUIRE (FILE = OUTPC, EXIST = FEXIST)
        IF (FEXIST) THEN
        OPEN (UNIT = NOUTPC, FILE = OUTPC, STATUS = 'OLD',
     &  IOSTAT = ERRNUM, ACCESS = 'APPEND')
        FIRST = .FALSE.
        ELSE
        OPEN (UNIT = NOUTPC, FILE = OUTPC, STATUS = 'NEW',
     &  IOSTAT = ERRNUM)
        WRITE(NOUTPC,'("*PLANT C OUTPUT FILE")')
        FIRST = .TRUE.
        ENDIF

        !Write headers
!        CALL HEADER(SEASINIT, FILEIO, NOUTPC, RUN)
        CALL HEADER(SEASINIT, NOUTPC, RUN)

        WRITE (NOUTPC,250)
250   FORMAT('@YEAR DOY   DAS   DAP  TWAD   PHAD',
     &  '   CMAD   CGRD   GRAD   MRAD   CHAD  QHAD  CL%D  CS%D',
     &  '  QC%D  CR%D  TGNN  TGAV  GN%D  GL%D  GC%D')


!-----------------------------------------------------------------------
!       Initialize daily Dormancy output file
        INQUIRE (FILE = OUTDRM, EXIST = FEXIST)
        IF (FEXIST) THEN
        OPEN (UNIT = NOUTDRM, FILE = OUTDRM, STATUS = 'OLD',
     &  IOSTAT = ERRNUM, ACCESS = 'APPEND')
        FIRST = .FALSE.
        ELSE
        OPEN (UNIT = NOUTDRM, FILE = OUTDRM, STATUS = 'NEW',
     &  IOSTAT = ERRNUM)
        WRITE(NOUTDRM,'("*PLANT DORMANCY OUTPUT FILE")')
        FIRST = .TRUE.
        ENDIF

        !Write headers
!        CALL HEADER(SEASINIT, FILEIO, NOUTDRM, RUN)
        CALL HEADER(SEASINIT, NOUTDRM, RUN)

        WRITE (NOUTDRM,270)
270   FORMAT('@YEAR DOY   DAS   DAP',
     &  '   QDSD  PPGF  PPMF  PPTF  TSRD  TS1D  FRZ2  LV%D  SV%D',
     &    '  QV%D  RV%D')

!-----------------------------------------------------------------------
!       Initialize daily Storage output file
        INQUIRE (FILE = OUTSTOR, EXIST = FEXIST)
        IF (FEXIST) THEN
        OPEN (UNIT = NOUTSTOR, FILE = OUTSTOR, STATUS = 'OLD',
     &  IOSTAT = ERRNUM, ACCESS = 'APPEND')
        FIRST = .FALSE.
        ELSE
        OPEN (UNIT = NOUTSTOR, FILE = OUTSTOR, STATUS = 'NEW',
     &  IOSTAT = ERRNUM)
        WRITE(NOUTSTOR,'("*PLANT STORAGE OUTPUT FILE")')
        FIRST = .TRUE.
        ENDIF

        !Write headers
!        CALL HEADER(SEASINIT, FILEIO, NOUTSTOR, RUN)
        CALL HEADER(SEASINIT, NOUTSTOR, RUN)

        WRITE (NOUTSTOR,280)
280   FORMAT('@YEAR DOY   DAS   DAP',
     &   '  QCQD  QHAD  QC%M  QRAD  QMAD  QCFD  QCAD  QCDD  QDTD',
     &   '  QN%X  QN%I  QV%D  QV%T  QNAA  QNRX  QNRN  QNAR  QNAM',
     &   '  QNAG  QNAN  QNAL  QN%N  QN%D  QW%C  QP%W  QL%S  QL%1',
     &   '  QS%D  Q1%D  QC%D  QCAM  QFDS  QFD1  QEAD  QEWD  QWAD',
     &   '  QT%S  QT%1  QCRD  QNMD  QCAG  QWNG  QWND  QFAD  QWAI',
     &   '  QMAM  QNAD  QNAC  QNLC  QDAD  XSTR')
         END IF    ! VSH
!-----------------------------------------------------------------------
!       Initialize Storage inputs output file
!        INQUIRE (FILE = OUTINSTR, EXIST = FEXIST)
!        IF (FEXIST) THEN
!          OPEN (UNIT = NOUTINSTR, FILE = OUTINSTR, STATUS = 'OLD',
!     &  IOSTAT = ERRNUM, ACCESS = 'APPEND')
!          FIRST = .FALSE.
!        ELSE
!          OPEN (UNIT = NOUTINSTR, FILE = OUTINSTR, STATUS = 'NEW',
!     &  IOSTAT = ERRNUM)
!          WRITE(NOUTINSTR,'("*PLANT STORAGE INPUTS OUTPUT FILE")')
!          FIRST = .TRUE.
!        ENDIF

        !Write headers
!        CALL HEADER(SEASINIT, FILEIO, NOUTINSTR, RUN)

!        WRITE (NOUTINSTR,290)
!  290   FORMAT('  SP%F  SP%G  SP%I  SRC%  SRG%  SRL%  SRO%  SRM%',
!     &  '  SR%L  SC%M  C%SR  SMNX  LAIT  SP%1  SP%2  SP%3  SP%4',
!     &    '  SP%5  SP%6  SP%7  SP%8  FRSR  FSRX  SRSF  SRL1  SNSR',
!     &  '  SRT1  SRT2  SRT3  SRT4  TSPT  SRM1  SRM2  SRM3  SRM4',
!     &    '  TSPM  SRG1  SRG2  SRG3  SRG4  TSPG  HRD1  HRD2  FZDC',
!     &  '  ZHD1  ZHD2  ZHD3  ZHD4  THRD  ZDH1  ZDH2  ZDH3  ZDH4',  
!     &    '  TDHD  RDMG  RDMM  RDMT  RCHD')



!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN

!-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C   CHECK FOR OUTPUT FREQUENCY
C-----------------------------------------------------------------------
      IF (YRDOY .LT. YRPLT .OR. YRPLT .LT. 0) RETURN
      IF ((MOD(DAS,FROP) .EQ. 0)          !Daily output every FROP days,
     &  .OR. (YRDOY .EQ. YRPLT)           !on planting date, and
     &  .OR. (YRDOY .EQ. MDATE)) THEN     !at harvest maturity 

!       Print 
        DAP = MAX(0,TIMDIF(YRPLT,YRDOY))
        CALL YR_DOY(YRDOY, YEAR, DOY) 

        IF (IDETG .EQ. 'Y') THEN
        IF (PODWT .GT. 0.1) THEN
        SHELPC = SDWT*100./PODWT
        ELSE
        SHELPC = 0.0
        ENDIF

        SHELPC = MIN(SHELPC,99.99)
        SHELLW = PODWT - SDWT       !Not used

        IF (SEEDNO .GT. 0.0) THEN
        SDSIZE = SDWT/SEEDNO*1000
        ELSE
        SDSIZE = 0.0
        ENDIF

!        IF (WTLF .GT. 0. .AND. SDWT .GE. 0.) THEN
        IF (WTLF .GT. 0.) THEN
        PELF = 100.*WTLF/(WTLF+STMWT)
        ELSE
        PELF = 0.0
        ENDIF
        
        IF (FHWAH .eq. 0.0) THEN
        fhpctn = 0.0
        endif

        IF (TOPWT .GT. 0. .AND. SDWT .GE. 0.) THEN
        HI = SDWT/TOPWT
        ELSE
        HI = 0.
        ENDIF
        
        IF (TOPWT .GT. 0. .AND. PODWT .GE. 0.) THEN
        HIP = PODWT/TOPWT
        ELSE
        HIP = 0.
        ENDIF
        
        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
        WRITE (NOUTDG,310)YEAR, DOY, DAS, DAP,VSTAGE,RSTAGE,XLAI,
     &    NINT(WTLF*10.),NINT(STMWT*10.),NINT(STRWT*10.), 
     &    PSRSRFL, PSRLYR1, NINT(SDWT*10.),
     &    NINT(RTWT*10.),NINT(TOPWT*10.),NINT(SEEDNO),SDSIZE,HI,
     &    NINT(PODWT*10.),NINT(PODNO),(1.-SWFAC),(1.-TURFAC),
     &    (1.-NSTRES),SATFAC,PCNL,SHELPC,HIP,NINT(PODWTD*10.),
     &    NINT((PODWTD+PODWT)*10.),SLA,CANHT,CANWH,(DWNOD*10.),
     &    (RTDEP/100.),(RLV(I),I=1,10),
     &    NINT(WTCO*10.),NINT(WTLO*10.),NINT(WTSO*10.), 
     &    NINT(WTSRO*10.),NINT(FHWAH*10.),FHLPH,
     &    PELF,NINT(DWTCO*10.),NINT(DWTLO*10.),NINT(DWTSO*10.),
     &    NINT(CANHT*100.),fhpctn*6.25    !,MOWC,RSPLC
310     FORMAT (1X,I4,1X,I3.3,2(1X,I5),
     &    1X,F5.1,1X,I5,1X,F5.2,3(1X,I5),2(1X,F5.2),4(1X,I5),
     &    1X,F6.1,1X,F5.3,2(1X,I5),4(1X,F5.3),3(1X,F5.2),
     &    2(1X,I5),1X,F5.1,2(1X,F5.2),1X,F5.1,11(1X,F5.2),
     &    4(I6),1x,I5,2(1X,F5.1),3(I6),I6,F6.2,F6.0,F6.2)
        END IF   ! VSH
        
!     VSH CSV output corresponding to PlantGro.OUT
      IF (FMOPT == 'C') THEN
       Call CsvOut_PlGroPrFrm(EXPNAME, RUN, CONTROL%TRTNUM,
     & CONTROL%ROTNUM, CONTROL%REPNO, YEAR,
     & DOY, DAS, DAP, VSTAGE, RSTAGE, XLAI, WTLF, STMWT, STRWT, PSRSRFL,
     & PSRLYR1, SDWT, RTWT, TOPWT, SEEDNO, SDSIZE, HI, PODWT, PODNO,    
     & SWFAC, TURFAC, NSTRES, SATFAC, PCNL, SHELPC, HIP, PODWTD, SLA,   
     & CANHT, CANWH, DWNOD, RTDEP, WTCO, WTLO, WTSO, WTSRO, FHWAH, 
     & FHLPH, PELF, DWTCO, DWTLO, DWTSO, fhpctn, N_LYR, RLV, 
     & vCsvlinePlGroPrFrm, vpCsvlinePlGroPrFrm, vlngthPlGroPrFrm)
       CALL LinklstPlGroPrFrm(vCsvlinePlGroPrFrm)
      END IF
C-----------------------------------------------------------------------
        WTNVEG  = (WTNLF + WTNST)
      
        IF ((WTLF+STMWT).GT. 0.0) THEN
        PCNVEG = (WTNLF+WTNST)/(WTLF+STMWT)*100.
        ELSE
        PCNVEG = 0.0
        ENDIF
        
        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN       ! VSH
        WRITE (NOUTPN,410) YEAR, DOY, DAS, DAP, (WTNCAN*10), 
     &   (WTNSD*10), (WTNVEG*10), PCNSD, PCNVEG, (WTNFX*10),
     &   (WTNUP*10), (WTNLF*10), (WTNST*10), (WTNSR*10.), PCNL, 
     &   PCNST, PCNSR, PCNSH, PCNRT, NFIXN*10
410     FORMAT(1X,I4,1X,I3.3,2(1X,I5),3(1X,F5.1),2(1X,F5.2),1X,
     &   2(1X,F6.1),3(1X,F5.1),3(1X,F5.2),2(1X,F5.2),1X,F5.1)
!CHP TEMP     &   (1X,F6.1),1X,F8.3,2(1X,F5.1),2(1X,F5.2),3(1X,F5.1))

        WRITE (NOUTPC,510) YEAR, DOY, DAS, DAP,
     &    NINT(TOTWT*10), PG, CMINEA, GROWTH,
     &    GRWRES, MAINR, (CADLF + CADST), CADSR, RHOL*100., 
     &    RHOS*100., RHOSR*100., RHOR*100., TGRO(12), TGROAV, PCNSD, 
     &    PCLSD, PCCSD
510     FORMAT(1X,I4,1X,I3.3,3(1X,I5),6(1X,F6.2),F6.2,4(1X,F5.1),
     &   2(1X,F5.1),3(1X,F5.2))


        WRITE (NOUTDRM,610) YEAR, DOY, DAS, DAP,
     &    DRMST, PPGFAC, PPMFAC, PPTFAC, SRFTEMP, ST(1), FREEZ2,
     &    FRLF, FRSTM, FRSTR, FRRT
610     FORMAT(1X,I4,1X,I3.3,2(1X,I5),1X,A6,3(1X,F5.3),
     &    3(1X,F5.1),4(1X,F5.3))
        

        WRITE (NOUTSTOR,710) YEAR, DOY, DAS, DAP,
     &    AGRSTR, CADSR, CMOBSR, CPFSTR, CRUSSR, CSRFRZ,  
     &    NINT(CSRW), CSTRM, DSTOR, FNINSR, FNINSRG, FRSTR,  
     &    FRSTRM, NADSR, NGRSR, NGRSRG, NMOBSR, NRUSSR, NSRALL, 
     &    NSRDOT, NSROFF, NVSTSR, PCNSR, PCSTRD, PROSRT, PSRSRFD, 
     &    PSRLYRD, PSRSRFL, PSRLYR1, RHOSR, SRDAM, SRSRFD, 
     &    SRLYRD, SSRDOT, SSRNDOT, NINT (STRWT*10), TPSRSRFL,  
     &    TPSRLYR1, WCRSR, WNRSR, WRCSRDT, WSRDOT, WSRDOTN,  
     &    WSRFDOT, WSRI, WSRIDOT, WTNSR, NINT(WTNSRA*10), 
     &    WTNSRO, NINT(WTSRO*10.), XSTR

710    FORMAT(1X,I4,1X,I3.3,2(1X,I5),2(1X,F5.2),1X,F5.4,3(1X,F5.2),
     &    1X,I5,2(1X,F5.2),4(1X,F5.3),9(1X,F5.2),7(1X,F5.2),1X, 
     &    F5.3,5(1X,F5.2),1X,I5,2(1X,F5.3),1X,F5.1,5(1X,F5.2),
     &    1X,F5.0,2(1X,F5.2),1X,I5,1X,F5.2,1X,I6,1X,F5.2)
        END IF    ! VSH
        
        IF (FMOPT == 'C') THEN   ! VSH
          Call CsvOutPlNPrFrm(EXPNAME, RUN, CONTROL%TRTNUM, 
     & CONTROL%ROTNUM, CONTROL%REPNO, YEAR, DOY, DAS, DAP, WTNCAN, 
     & WTNSD, WTNVEG, PCNSD, PCNVEG, WTNFX, WTNUP, WTNLF, WTNST, WTNSR, 
     & PCNL, PCNST, PCNSR, PCNSH, PCNRT, NFIXN,
     & vCsvlinePlNPrFrm, vpCsvlinePlNPrFrm, vlngthPlNPrFrm)
           
          CALL LinklstPlNPrFrm(vCsvlinePlNPrFrm)
          
          CALL CsvOutPlCPrFrm(EXPNAME, CONTROL%RUN, CONTROL%TRTNUM, 
     &CONTROL%ROTNUM, CONTROL%REPNO, YEAR, DOY, DAS, DAP, 
     &TOTWT, PG, CMINEA, GROWTH, GRWRES, MAINR, CADLF, 
     &CADST, CADSR, RHOL, RHOS, RHOSR, RHOR, TGRO, TGROAV, PCNSD, 
     &PCLSD, PCCSD, TS, 
     &vCsvlinePlCPrFrm, vpCsvlinePlCPrFrm, vlngthPlCPrFrm)
      
         CALL LinklstPlCPrFrm(vCsvlinePlCPrFrm)
         
         CALL CsvOutDormPrFrm(EXPNAME, CONTROL%RUN, CONTROL%TRTNUM, 
     &CONTROL%ROTNUM, CONTROL%REPNO, YEAR, DOY, DAS, DAP, 
     &DRMST, PPGFAC, PPMFAC, PPTFAC, SRFTEMP, ST, FREEZ2, FRLF, FRSTM,  
     &FRSTR, FRRT, TS, 
     &vCsvlineDormPrFrm, vpCsvlineDormPrFrm, vlngthDormPrFrm)

         CALL LinklstDormPrFrm(vCsvlineDormPrFrm)
         
         CALL CsvOutStorPrFrm(EXPNAME, CONTROL%RUN, CONTROL%TRTNUM, 
     &CONTROL%ROTNUM, CONTROL%REPNO, YEAR, DOY, DAS, DAP, AGRSTR, 
     &CADSR, CMOBSR, CPFSTR, CRUSSR, CSRFRZ, CSRW, CSTRM, DSTOR, FNINSR,
     &FNINSRG, FRSTR, FRSTRM, NADSR, NGRSR, NGRSRG, NMOBSR, NRUSSR, 
     &NSRALL, NSRDOT, NSROFF, NVSTSR, PCNSR, PCSTRD, PROSRT, PSRSRFD,
     &PSRLYRD, PSRSRFL, PSRLYR1, RHOSR, SRDAM, SRSRFD, SRLYRD, SSRDOT,
     &SSRNDOT, STRWT, TPSRSRFL, TPSRLYR1, WCRSR, WNRSR, WRCSRDT, WSRDOT,
     &WSRDOTN, WSRFDOT, WSRI, WSRIDOT, WTNSR, WTNSRA,WTNSRO, WTSRO,XSTR,
     &vCsvlineStorPrFrm, vpCsvlineStorPrFrm, vlngthStorPrFrm)

         CALL LinklstStorPrFrm(vCsvlineStorPrFrm)
        END IF ! VSH 
        
        ENDIF       
      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal Output 
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------

!       Write Storage inputs output file

!          WRITE (NOUTINSTR,810) PROSRF, PROSRG, PROSRI, PCARSR, PLIGSR,  
!     &  PLIPSR, POASR, PMINSR, ALPHSR, CMOBSRX, CADSRF, NMOBSRX, CLAIT,      
!     &  YSTOR(1), YSTOR(2), YSTOR(3), YSTOR(4), YSTOR(5), YSTOR(6), 
!     &  YSTOR(7), YSTOR(8), FRSTRF, FRSTRMX, STRSRFL, STRLYR1, SENSR,
!     &  FNPTD(1), FNPTD(2), FNPTD(3),FNPTD(4), TYPPTD, FNPMD(1), 
!     &  FNPMD(2), FNPMD(3), FNPMD(4), TYPPMD, FNPGD(1), FNPGD(2), 
!     &  FNPGD(3), FNPGD(4), TYPPGD, HARD1, HARD2, FRZDC, FRZHRD(1), 
!     &  FRZHRD(2), FRZHRD(3), FRZHRD(4), TYPHRD, FRZDHD(1), FRZDHD(2), 
!     &  FRZDHD(3), FRZDHD(4), TYPDHD, RDRMG, RDRMM, RDRMT, RCHDP

!  810     FORMAT(8(1X,F5.3),15(1X,F5.2),3(1X,F5.3),4(1X,F5.1),3X,A3, 
!     &              4(1X,F5.1),3X,A3,4(1X,F5.1),3X,A3,2(1X,F5.1),1X,F5.3,
!     &              4(1X,F5.1),3X,A3,4(1X,F5.1),3X,A3,4(1X,F5.3))

!            WRITE(NOUTINSTR,'("*DUMMY LINE FOR DEBUGGING")')

 
 
 
        !Close daily output files.
        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
        CLOSE (NOUTDG)
        CLOSE (NOUTPN)
        CLOSE (NOUTPC)
        CLOSE (NOUTDRM)
        CLOSE (NOUTSTOR)
!        CLOSE (NOUTINSTR)
        END IF   ! VSH
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END ! SUBROUTINE FOR_OPGROW
!=======================================================================


!=======================================================================
!       Variable definitions for FOR_OPGROW
!-----------------------------------------------------------------------
! CANHT   Canopy height (m)
! CANWH   Canopy width normal to row (m)
! CROPD   Name of crop 
! DAP     Number of days after planting (d)
! DWNOD   Current nodule mass (g[nodule] / m2)
! DYNAMIC Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, INTEGR, 
!           OUTPUT, or SEASEND 
! ENAME   Experiment description 
! EXPER   Experiment code (prefix of input files) 
! HI      Ratio of seed weight (SDWT) to weight of above-ground portion of 
!           plant (TOPWT) (g[seed] / g[tops])
! HIP     Ratio of pod weight (PODWT) to weight of above-ground portion of 
!           plant (TOPWT) (g[pods] / g[tops])
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
!       END SUBROUTINE FOR_OPGROW
!=======================================================================

