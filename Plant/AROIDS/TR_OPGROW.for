C=======================================================================
C  TR_OPGROW, Subroutine
C
C  Generates output for growth data for Taro
C-----------------------------------------------------------------------
C  Revision history
C  08/18/2005 CHP Written based on PT_OPGROW
C=======================================================================

      SUBROUTINE TR_OPGROW (CONTROL, ISWITCH, SOILPROP,   !Input
     &    BIOMAS, CORMWT, DEADLF, DTT, ISTAGE,            !Input
     &    LAI, LFWT, MDATE, NLAYR, NSTRES, PLTPOP,        !Input
     &    RLV, ROOTN, RTDEP, RTWT, SATFAC, SENESCE,       !Input
     &    PETWT, STOVN, STOVWT, SWFAC, CORMN,             !Input
     &    MCORMWT, CORMLNO,                               !Input
     &    TURFAC, WTNCAN, WTNUP, XLAI, YRPLT)             !Input

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT  NONE
      EXTERNAL GETLUN, HEADER, TIMDIF, YR_DOY
      SAVE

      CHARACTER*1   IDETG, ISWNIT
      CHARACTER*12  OUTG, OUTPN
      CHARACTER*30 FILEIO
      CHARACTER*120 NITHEAD(4)
      CHARACTER*250 GROHEAD(4), TEXT

      INTEGER DAP, DAS, DOY, DYNAMIC, FROP
      INTEGER I, ISTAGE, L, NLAYR
      INTEGER NOUTPN, NOUTDG, RUN, TIMDIF
      INTEGER YEAR, YRDOY, MDATE, YRPLT 
 
      REAL LAI, XLAI,PETWT,SDWT,WTLF,BIOMAS,RTWT,PODWT,SEEDNO
      REAL SLA,PCNL,TURFAC,CANHT,CANWH,RLV(NL),HI,SHELPC
      REAL PODNO,RTDEP,NSTRES,SWFAC,SATFAC,PLTPOP,GM2KG 
      REAL FRYLD,DEADLF, DTT, CORMWT, MCORMWT, CORMLNO

      REAL LFWT, PCNGRN, PCNRT 
      REAL PCNST, PCNVEG, ROOTN
      REAL STOVN, STOVWT
      REAL CORMN, WTNCAN
      REAL WTNLF, WTNSD, WTNSH, WTNST 
      REAL WTNUP, WTNVEG

      REAL CUMSENSURF, CUMSENSOIL, CUMSENSURFN, CUMSENSOILN  

      LOGICAL FEXIST  

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

!     The variable "ISWITCH" is of type "SwitchType".
      TYPE (SwitchType) ISWITCH
      TYPE (ResidueType)SENESCE
      TYPE (SoilType)   SOILPROP

!     Transfer values from constructed data types into local variables.
      IDETG   = ISWITCH % IDETG
      IF (IDETG .NE. 'Y') RETURN

      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      FROP    = CONTROL % FROP
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY

      ISWNIT  = ISWITCH % ISWNIT

C-----------------------------------------------------------------------
      DATA GROHEAD /
!      DATA GROHEAD(1)/
     &'!          Days  Days        Leaf Fresh                          
     &                                    <----Stress (0-1)---->  Leaf S
     &hell  Spec  <-Canopy->  Root    <---Root Length Density by layer--
     &->   Grow',

!      DATA GROHEAD(2)/
     &'!         after after  Grow  Area Yield  <--------- Dry Weight (k
     &g/ha) --------->  Harv <---Corm-->  <-----Water---->  Nitr   Nit  
     &-ing  Leaf  Hght Width   Dep    <--------cm[root]/cm3[soil]-------
     &-> Degree',

!      DATA GROHEAD(3)/
     &'!<-Date->   sim plant Stage Index Mg/Ha  Leaf Petiole Corm Root  
     &Crop  Tops DLeaf Index kg/ha   No.  Phot  Grow Exces           %  
     &   %  Area     m     m     m                                      
     &     Days',

!      DATA GROHEAD(4) / 
     &'@YEAR DOY   DAS   DAP  GSTD  LAID  UYAD  LWAD  SWAD  UWAD  RWAD  
     &TWAD  CWAD  DWAD  HIAD  EWAD  E#AD  WSPD  WSGD  EWSD  NSTD  LN%D  
     &SH%D  SLAD  CHTD  CWID  RDPD    RL1D    RL2D    RL3D    RL4D    RL
     &5D   DTTD'/

C-----------------------------------------------------------------------
      DATA NITHEAD /
!      DATA NITHEAD(1)/
     &'!          Days  Days',

!      DATA NITHEAD(2)/
     &'!         after after <--------- N mass (kg/ha) -------->  <-----
     &------ N % ---------->',

!      DATA NITHEAD(3)/
     &'!<-Date->   sim plant Uptak  Crop   Veg  Leaf  Stem  Corm  Corm  
     & Veg  Leaf  Stem  Root',

!      DATA NITHEAD(4)/  
     &'@YEAR DOY   DAS   DAP  NUPC  TUNA  VNAD  LNAD  SNAD  UNAD  UN%D  
     &VN%D  LN%D  SN%D  RN%D'/

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
     &      POSITION = 'APPEND')
          !FIRST = .FALSE.  
        ELSE
          OPEN (UNIT = NOUTDG, FILE = OUTG, STATUS = 'NEW')
          WRITE(NOUTDG,'("*GROWTH ASPECTS OUTPUT FILE")')
          !FIRST = .TRUE.  
        ENDIF

        !Write headers
        CALL HEADER(SEASINIT, NOUTDG, RUN)

!       Fill in layer depths in GROHEAD(3)
        TEXT = GROHEAD(3)
        WRITE (GROHEAD(3),'(A159,5A8,A21)') 
     &      TEXT(1:159),(SoilProp%LayerText(L),L=1,5),TEXT(200:220)

C       Variable heading for GROWTH.OUT
        WRITE (NOUTDG,2192) GROHEAD(1)
        WRITE (NOUTDG,2192) GROHEAD(2)
        WRITE (NOUTDG,2192) GROHEAD(3)
        WRITE (NOUTDG,2192) GROHEAD(4)
 2192   FORMAT (A227)

        SEEDNO = 0.0
        !GPP   = 0.0
        !WTNUP = 0.0
        CANHT = 0.0
        CANWH = 0.0

!-----------------------------------------------------------------------
!     Initialize daily plant nitrogen output file
      IF (ISWNIT .EQ. 'Y') THEN
        INQUIRE (FILE = OUTPN, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTPN, FILE = OUTPN, STATUS = 'OLD',
     &      POSITION = 'APPEND')
          !FIRST = .FALSE.
        ELSE
          OPEN (UNIT = NOUTPN, FILE = OUTPN, STATUS = 'NEW')
          WRITE(NOUTPN,'("*PLANT N OUTPUT FILE")')
          !FIRST = .TRUE.
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
      SDWT   = CORMWT           !SDWT used by OPHARV, OPPHO and OPSEQ
      WTLF   = LFWT   * PLTPOP !WTLF used by OPNIT, OPPHO and OPOPS

      IF (WTLF .GT. 0.0) THEN
        SLA  = XLAI * 10000 / WTLF
      ELSE
        SLA  = 0.0
      ENDIF

      PODWT  = 0.0
      PODNO = 0.0

      IF (STOVWT .GT. 0.0) THEN
        WTNLF = STOVN * (LFWT  / STOVWT) * PLTPOP
      ELSE
        WTNLF = 0.0
      ENDIF

      IF (LFWT*PLTPOP .GT. 0.0) THEN
        PCNL = WTNLF /( LFWT * PLTPOP) * 100.0
      ELSE
        PCNL = 0.0
      ENDIF


C
C     GM2KG converts gm/plant to kg/ha
C
      GM2KG  = PLTPOP * 10.0
      SHELPC = 0.0
      IF (PODWT .GT. 0.1) THEN
        SHELPC = SDWT*100.0/PODWT
      ENDIF


!     Local variable HI used in OPHARV with different formula
      HI = 0.0
      IF (BIOMAS .GT. 0.0 .AND. SDWT .GE. 0.0) THEN
        HI = CORMWT/BIOMAS       ! 12/5/14 BIOMAS AND CORMWT IN G/PLANT
      ENDIF
!      YIELD  = CORMWT*10.*PLTPOP   
!      FRYLD = (YIELD/1000.)/0.2    
      FRYLD = (CORMWT*GM2KG/1000.)* 3  
! 12/5/14 Fresh yield with 200% moisture

!---------------------------------------------------------------------------
!     Compute reported plant N variables
      WTNCAN = (STOVN + CORMN) * PLTPOP
      IF (LFWT + PETWT > 1.E-6) THEN
        WTNST = STOVN * (PETWT  / (LFWT + PETWT)) * PLTPOP
        WTNLF = STOVN *  (LFWT /  (LFWT + PETWT)) * PLTPOP
      ELSE
        WTNST = 0.0
        WTNLF = 0.0
      ENDIF

      WTNSD = CORMN * PLTPOP
      WTNSH = 0.0
      IF (LFWT*PLTPOP .GT. 0.0) THEN
        PCNL = WTNLF /( LFWT * PLTPOP) * 100.0
      ELSE
        PCNL = 0.0
      ENDIF
      IF (PETWT*PLTPOP .GT. 0.0) THEN
        PCNST = WTNST/(PETWT * PLTPOP) * 100.0
      ELSE
        PCNST = 0.0
      ENDIF
      IF (RTWT .GT. 0.0) THEN
        PCNRT = ROOTN/RTWT * 100.0
      ELSE
        PCNRT = 0.0
      ENDIF

      WTNVEG  = (WTNLF + WTNST)
      IF ((WTLF+PETWT) .GT. 0.0) THEN
        PCNVEG = (WTNLF+WTNST)/(WTLF+(PETWT*PLTPOP))*100.0
      ELSE
        PCNVEG = 0.0
      ENDIF

      IF (CORMWT .GT. 0.0) THEN
        PCNGRN = CORMN*100.0/CORMWT
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
          WRITE (NOUTDG,400)YEAR, DOY, DAS, DAP, ISTAGE, LAI, FRYLD,
     &        NINT(LFWT*GM2KG), NINT(PETWT*GM2KG), NINT(CORMWT*GM2KG),
     &        NINT(RTWT*GM2KG), NINT(BIOMAS*GM2KG),
     &        NINT(WTLF*10.0)+NINT(PETWT*GM2KG), NINT(DEADLF*GM2KG), HI,
     &        NINT(MCORMWT*GM2KG), NINT(CORMLNO), 1.0-SWFAC, 1.0-TURFAC,
     &        SATFAC, 1.0-NSTRES, PCNL, SHELPC, SLA, CANHT, CANWH, 
     &        (RTDEP/100), (RLV(I),I=1,5),DTT
 400      FORMAT (1X,I4,1X,I3.3,3(1X,I5),1X,F5.3,1X,F5.1,7(1X,I5),
     &          1X,F5.3,2(1X,I5),4(1X,F5.3),2(1X,F5.2),1X,F5.1,
     &          3(1X,F5.2),5(1X,F7.2), F7.2)
        ENDIF

C-----------------------------------------------------------------------
!       From OPNIT.OUT
        IF (ISWNIT .EQ. 'Y') THEN
          WRITE (NOUTPN,300) YEAR, DOY, DAS, DAP, WTNUP, (WTNCAN*10.0),
     &            (WTNVEG*10.0), (WTNLF*10.0), (WTNST*10.0), 
     &            (WTNSD*10.0), PCNGRN, PCNVEG,
     &            PCNL, PCNST, PCNRT
 300      FORMAT (1X,I4,1X,I3.3,2(1X,I5),6(1X,F5.1),5(1X,F5.2))
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
      END SUBROUTINE TR_OPGROW
!=======================================================================

