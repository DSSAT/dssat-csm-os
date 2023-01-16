C=======================================================================
C  PT_OPGROW, Subroutine
C
C  Generates output for growth data
C-----------------------------------------------------------------------
C  Revision history
C                 Written
C  02/08/1993 PWW Header revision and minor changes 
C  02/08/1993 PWW Added switch block, etc. 
C  09/05/2001 CHP Modified for modular format
C  08/20/2002 GH  Modified for Y2K
C  07/08/2003 CHP Added senescence output to conform to other plant routines.
C
C=======================================================================

      SUBROUTINE PT_OPGROW (CONTROL, ISWITCH, 
     &    BIOMAS, DEADLF, GRAINN, ISTAGE, LFWT, MDATE,    !Input
     &    NLAYR, NSTRES, PLTPOP, RLV, ROOTN, RTDEP, RTWT, !Input
     &    SATFAC, SENESCE, STMWT, STOVN, STOVWT, SWFAC,   !Input
     &    TUBN, TUBWT, TURFAC, WTNCAN, WTNUP, XLAI, YRPLT)!Input

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT  NONE
      EXTERNAL GETLUN, HEADER, TIMDIF, YR_DOY
      SAVE

      CHARACTER*1   IDETG, ISWNIT
      CHARACTER*12  OUTG, OUTPN
      CHARACTER*120 NITHEAD(4)
      CHARACTER*220 GROHEAD(4)
      CHARACTER*6, PARAMETER :: ERRKEY = 'OPGROW'

      INTEGER DAP, DAS, DOY, DYNAMIC, ERRNUM, FROP
      INTEGER I, ISTAGE, L, LUNIO, NLAYR
      INTEGER NOUTPN, NOUTDG, RUN, RSTAGE, TIMDIF
      INTEGER YEAR, YRDOY, MDATE, YRPLT
 
      REAL XLAI,STMWT,SDWT,WTLF,BIOMAS,RTWT,PODWT,SEEDNO
      REAL SLA,PCNL,TURFAC,CANHT,CANWH,RLV(20),HI,SHELPC,SHELLW
      REAL SDSIZE,PODNO,RTDEP,NSTRES,SWFAC,SATFAC,PLTPOP,GM2KG
      REAL FRYLD,DEADLF, GRAINN

      REAL LFWT, GPP, PCNGRN, PCNRT
      REAL PCNST, PCNVEG, ROOTN
      REAL STOVN, STOVWT
      REAL TUBN, TUBWT, WTNCAN
      REAL WTNGRN, WTNLF, WTNRT, WTNSD, WTNSH, WTNST
      REAL WTNUP, WTNVEG

      REAL CUMSENSURF, CUMSENSOIL, CUMSENSURFN, CUMSENSOILN  

      LOGICAL FEXIST, FIRST

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

!     The variable "ISWITCH" is of type "SwitchType".
      TYPE (SwitchType) ISWITCH
      TYPE (ResidueType) SENESCE

!     Transfer values from constructed data types into local variables.
      IDETG   = ISWITCH % IDETG
      IF (IDETG .NE. 'Y') RETURN

      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FROP    = CONTROL % FROP
      LUNIO   = CONTROL % LUNIO
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY

      ISWNIT  = ISWITCH % ISWNIT

C-----------------------------------------------------------------------
      DATA GROHEAD /
!      DATA GROHEAD(1)/
     &'! YR       Days  Days  Grow       Fresh          
     &      Dry Weight                           Pod      Phot. Grow    
     &   Leaf Shell   Spec    Canopy          Root  ³    Root Length Den
     &sity   ³ Senesced mass              ',

!      DATA GROHEAD(2)/
     &'!   and   after after Stage  LAI  Yield  Leaf  St
     &em Tuber  Root  Crop  Tops DLeaf   HI   Wgt.   No.    Water     Ni
     &t.   Nit -ing   Leaf  Hght  Brdth      Depth  ³     cm3/cm3   of 
     &soil    ³    (kg/ha)                ',

!      DATA GROHEAD(3)/
     &'!     DOY   sim plant             Mg/Ha  ³<------
     &--------- kg/Ha --------------->³      Kg/Ha        ³<Stress (0-1)
     &>³    %     %   Area    m     m           m   ³<------------------
     &------>³ Surface  Soil              ',

!      DATA GROHEAD(4) / 
     &'@YEAR DOY   DAS   DAP  GSTD  LAID  UYAD  LWAD  
     &SWAD  UWAD  RWAD  TWAD  CWAD  DWAD  HIAD  EWAD  E#AD  WSPD  WSGD  
     &NSTD  LN%D  SH%D  SLAD  CHTD  CWID  EWSD  RDPD  RL1D  RL2D  RL3D  
!     &RL4D  RL5D              '/
     &RL4D  RL5D  SNW0C  SNW1C'/

C-----------------------------------------------------------------------
      DATA NITHEAD /
!      DATA NITHEAD(1)/
     &'!YEAR      Days  Days      Nitrogen       Nitroge
     &n     Up-  Leaf  Stem  Leaf  Stem  Root   Senesced N',

!      DATA NITHEAD(2)/
     &'!   and     Aft.  Aft. Crop Tuber  Veg. Tuber  Ve
     &g.   take    N     N     N     N     N     (kg/ha) ',

!      DATA NITHEAD(3)/
     &'!     DOY   Sim Plant  ³<--- Kg/Ha -->³ ³<-- % --
     &>³  ³<--- kg/ha --->³ ³<----- % ----->³  Surface Soil',

!      DATA NITHEAD(4)/  
     &'@YEAR DOY   DAS   DAP  TUNA  UNAD  VNAD  UN%D  
     &VN%D   NUPC  LNAD  SNAD  LN%D  SN%D  RN%D  SNN0C  SNN1C'/

!***********************************************************************
!***********************************************************************
!     Run initialization - run once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
        OUTG  = 'PlantGro.OUT'
        CALL GETLUN('OUTG',  NOUTDG)

        OUTPN  = 'PlantN.OUT  '
        CALL GETLUN('OUTPN', NOUTPN)

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!     Initialize daily growth output file
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

C       Variable heading for GROWTH.OUT
        WRITE (NOUTDG,2192) GROHEAD(1)
        WRITE (NOUTDG,2192) GROHEAD(2)
        WRITE (NOUTDG,2192) GROHEAD(3)
        WRITE (NOUTDG,2192) GROHEAD(4)
 2192   FORMAT (A219)

        SEEDNO = 0.0
        GPP   = 0.0
        WTNUP = 0.0
        CANHT = 0.0
        CANWH = 0.0

!-----------------------------------------------------------------------
!     Initialize daily plant nitrogen output file
      IF (ISWNIT .EQ. 'Y') THEN
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

        CALL HEADER(SEASINIT, NOUTPN, RUN)

        WRITE (NOUTPN,2240) NITHEAD(1)
        WRITE (NOUTPN,2240) NITHEAD(2)
        WRITE (NOUTPN,2240) NITHEAD(3)
        WRITE (NOUTPN,2240) NITHEAD(4)
 2240   FORMAT (A110)
      ENDIF

      CUMSENSURF  = 0.0
      CUMSENSOIL  = 0.0
      CUMSENSURFN = 0.0
      CUMSENSOILN = 0.0   

!***********************************************************************
!***********************************************************************
!     Daily Output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
C-----------------------------------------------------------------------
      IF (YRDOY .LT. YRPLT .AND. YRPLT .GT. 0) RETURN

!     Accumulate senesced matter for surface and soil.
      CUMSENSURF  = CUMSENSURF  + SENESCE % ResWt(0) 
      CUMSENSURFN = CUMSENSURFN + SENESCE % ResE(0,1) 
      DO L = 1, NLAYR
        CUMSENSOIL  = CUMSENSOIL  + SENESCE % ResWt(L)
        CUMSENSOILN = CUMSENSOILN + SENESCE % ResE(L,1)
      ENDDO

!     Compute reported growth variables
      RSTAGE = ISTAGE
      SDWT   = TUBWT           !SDWT used by OPHARV, OPPHO and OPSEQ
      WTLF   = LFWT   * PLTPOP !WTLF used by OPNIT, OPPHO and OPOPS

      IF (WTLF .GT. 0.0) THEN
        SLA  = XLAI * 10000 / WTLF
      ELSE
        SLA  = 0.0
      ENDIF

!     SEEDNO = GPSM               !SEEDNO used by OPHARV
      PODWT  = 0.0
      PODNO = 0.0

      IF ((LFWT+STMWT) .GT. 0.0) THEN
        WTNLF = STOVN * (LFWT  / STOVWT) * PLTPOP
      ELSE
        WTNLF = 0.0
      ENDIF

      IF (LFWT .GT. 0.0) THEN
        PCNL = WTNLF /( LFWT * PLTPOP) * 100.0
      ELSE
        PCNL = 0.0
      ENDIF

!      WTNUP = WTNUP + TRNU * PLTPOP       !g[N]/m2 
!   g[N]/m2 =   g[N]/plant * plant/m2
C
C     GM2KG converts gm/plant to kg/ha
C
      GM2KG  = PLTPOP * 10.0
      SHELPC = 0.0
      IF (PODWT .GT. 0.1) THEN
        SHELPC = SDWT*100.0/PODWT
      ENDIF
      SHELLW = PODWT - SDWT
      SDSIZE = 0.0
      IF (SEEDNO .GT. 0.0) THEN
        SDSIZE = SDWT*PLTPOP/SEEDNO*1000.0
      ENDIF

!     Local variable HI used in OPHARV with different formula
      HI = 0.0
      IF (BIOMAS .GT. 0.0 .AND. SDWT .GE. 0.0) THEN
        HI = SDWT*PLTPOP/BIOMAS
      ENDIF
!      YIELD  = TUBWT*10.*PLTPOP   
!      FRYLD = (YIELD/1000.)/0.2    
      FRYLD = (TUBWT*10.*PLTPOP/1000.)/0.2   ! Fresh yield

!---------------------------------------------------------------------------
!     Compute reported plant N variables
!      WTNCAN = (STOVN + GRAINN) * PLTPOP
      IF ((LFWT+STMWT) .GT. 0.0) THEN
        WTNLF = STOVN * (LFWT  / STOVWT) * PLTPOP
        WTNST = STOVN * (STMWT / (LFWT + STMWT)) * PLTPOP
      ELSE
        WTNLF = 0.0
        WTNST = 0.0
      ENDIF

      WTNSD = GRAINN * PLTPOP
      WTNRT = ROOTN * PLTPOP        ! Is this right?
      WTNSH = 0.0
      IF (LFWT .GT. 0.0) THEN
        PCNL = WTNLF /( LFWT * PLTPOP) * 100.0
      ELSE
        PCNL = 0.0
      ENDIF
      IF (STMWT .GT. 0.0) THEN
        PCNST = WTNST/(STMWT * PLTPOP) * 100.0
      ELSE
        PCNST = 0.0
      ENDIF
      IF (RTWT .GT. 0.0) THEN
        PCNRT = ROOTN/RTWT * 100.0
      ELSE
        PCNRT = 0.0
      ENDIF

      WTNVEG  = (WTNLF + WTNST)
      WTNGRN  = (WTNSH + WTNSD)
      IF ((WTLF+STMWT) .GT. 0.0) THEN
        PCNVEG = (WTNLF+WTNST)/(WTLF+(STMWT*PLTPOP))*100.0
      ELSE
        PCNVEG = 0.0
      ENDIF

      IF (TUBWT .GT. 0.0) THEN
        PCNGRN = TUBN*100.0/TUBWT
      ELSE
        PCNGRN = 0.0
      ENDIF

!---------------------------------------------------------------------------
      IF ((MOD(DAS,FROP) .EQ. 0)          !Daily output every FROP days,
     &  .OR. (YRDOY .EQ. YRPLT)           !on planting date, and
     &  .OR. (YRDOY .EQ. MDATE)) THEN     !at harvest maturity 

        DAP = MAX(0,TIMDIF(YRPLT,YRDOY))
        IF (DAP > DAS) DAP = 0
        CALL YR_DOY(YRDOY, YEAR, DOY)

!       PlantGro.out file
        IF (IDETG .EQ. 'Y') THEN
          WRITE (NOUTDG,400)YEAR, DOY, DAS, DAP,RSTAGE,XLAI,FRYLD,
     &        NINT(WTLF*10.0),NINT(STMWT*GM2KG),NINT(SDWT*GM2KG),
     &        NINT(RTWT*GM2KG),NINT(BIOMAS*10.0),
     &        NINT(WTLF*10.0)+NINT(STMWT*GM2KG),NINT(DEADLF*GM2KG),HI,
     &        NINT(PODWT*GM2KG),NINT(PODNO),1.0-SWFAC,1.0-TURFAC,
     &        1.0-NSTRES,PCNL,SHELPC,SLA,CANHT,CANWH,SATFAC,
     &        (RTDEP/100),(RLV(I),I=1,5)
     &       ,NINT(CUMSENSURF), NINT(CUMSENSOIL)
 400      FORMAT (1X,I4,1X,I3.3,3(1X,I5),1X,F5.2,1X,F5.1,7(1X,I5),
     &          1X,F5.3,2(1X,I5),3(1X,F5.3),2(1X,F5.2),1X,F5.1,
     &          2(1X,F5.2),1X,F5.3,6(1X,F5.2), 2I6)
        ENDIF

C-----------------------------------------------------------------------
!       From OPNIT.OUT
        IF (ISWNIT .EQ. 'Y') THEN
          WRITE (NOUTPN,300) YEAR, DOY, DAS, DAP, (WTNCAN*10.0),
     &            (WTNSD*10.0), (WTNVEG*10.0), PCNGRN, PCNVEG,
!    &            (WTNUP*10.0), (WTNLF*10.0), (WTNST*10.0),  !WTNUP g/m2
     &            WTNUP, (WTNLF*10.0), (WTNST*10.0), !WTNUP kg/ha
     &            PCNL, PCNST, PCNRT
     &    ,CUMSENSURFN, CUMSENSOILN     
 300      FORMAT (1X,I4,1X,I3.3,2(1X,I5),3(1X,F5.1),2(1X,F5.2),1X,F6.1,
     &        2(1X,F5.1),3(1X,F5.2)  !)
     &        ,2(1X,F6.2))
        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal Output
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
        !Close daily output files.
        CLOSE (NOUTDG)
        CLOSE (NOUTPN)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE PT_OPGROW
!=======================================================================

