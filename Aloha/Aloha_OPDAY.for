C=======================================================================
C  Aloha_OPDAY, Subroutine
C
C  Generates output for simulated data
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P. Wilkens  2-8-93
C  3. Added switch block, etc.                      P. Wilkens  2-8-93
C-----------------------------------------------------------------------
C  INPUT  : MODEL,TRTNO,DAP,YRSIM,YRPLT,CROPD,STGDOY,
C           YRDOY,TOTIR,NYRS,DAS,DAYL,TRUNOF,TDRAIN,AMTNIT,NAP
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : PINE
C
C  Calls  : CLEAR OPWBAL OPNIT OPGROW OPPEST OPCARB OPVIEW
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE Aloha_OPDAY (MODEL,TRTNO,DAP,YRSIM,YRPLT,NREP,TITLET,
     &         EXPER,ENAME,MULTI,CROPD,STGDOY,YRDOY,TOTIR,NYRS,DAS,
     &         DAYL,TRUNOF,TDRAIN,AMTNIT,NAP,HAREND,NPSTAP,STNAME,
     &         WTNLF,WTNST,WTNSD,WTNSH,WTNRT)

      IMPLICIT  NONE

      INCLUDE  'GEN1.BLK'
      INCLUDE  'GEN2.BLK'
      INCLUDE  'GEN3.BLK'
      INCLUDE  'GEN4.BLK'
      INCLUDE  'NTRC1.BLK'
      INCLUDE  'NTRC2.BLK'
      INCLUDE  'PREDOB.BLK'
      INCLUDE  'SWITCH.BLK'
      INCLUDE  'PEST.BLK'
      INCLUDE  'PHOS.BLK'

      CHARACTER*8   EXPER
      CHARACTER*10  CROPD,STNAME(20)
      CHARACTER*25  MODEL,TITLET
      CHARACTER*60  ENAME
      CHARACTER*210 GROHEAD(4)
      CHARACTER*240 WATHEAD(4)
      CHARACTER*252 NITHEAD(4)
      CHARACTER*252 PHOHEAD(4),PSTHEAD(1),CARHEAD(4)
      CHARACTER*512 HEAD

      INTEGER LUNOV,NAVWB,I,M,HAREND,NREP,NPSTAP
      INTEGER TRTNO,DAP,YRSIM,YRPLT,YRDOY,MULTI
      INTEGER NYRS,RSTAGE,DAS,STGDOY(20),NAP

      REAL    DSTRES,STRESP,STRESS,NFIXN,MAINR
      REAL    DAYL,TRUNOF,TDRAIN,WTNCAN,AMTNIT,WTNUP,TSIN
      REAL    SLA,PODWT,PODNO,TOTIR,STRESN,CWSV,CWSR
      REAL    WTPSD,AVEP,AVES,AVET,AVEO,AVTMX,AVTMN,AVWTD
      REAL    WTLF,SEEDNO,WTNLF,WTNST,WTNSD,WTNSH,WTNFX,WTNRT
      REAL    PCNL,PCNST,PCNRT,PCNSH,VSTAGE,CANHT,CANWH
      REAL    DISLA,DISLAP,WLIDOT,WSIDOT,WRIDOT,SWIDOT,WSHIDT,SDIDOT
      REAL    SHIDOT,CASM,PG,GROWTH,GRWRES,CADLF,CADST,CMINEA
      REAL    TOTWT,RHOL,PCINPN,XANC,AVSRAD,PPLTD,LAIDT
      REAL    CPPLTD,CLFM,CSTEM,CSDM,CSDN,CSHM,CSHN,RLVDOT,ASMDOT
      REAL    RHOS,PGN,CRLV,RLFDOT,CRLF,CRTM
      REAL    PCINPD,SLWSLN,SLWSHN,PNLSLN,PNLSHN,LMXSLN,LMXSHN

      SAVE

      PARAMETER (LUNOV  =  6)

      DATA GROHEAD(1)/'!YR    Days   Leaf  Grow                      Dry
     1 Weight                           Pod      Phot. Grow       Leaf  
     2Shell  Spec    Canopy          Root  ³                Root Length 
     3Density                     ³'/
      DATA GROHEAD(2)/'! and  after  Num  Stage  LAI   Leaf  Stem Fruit
     1 Root Basal Crown  Suck   HI   Wgt.   No.    Water     Nit.   Nit 
     2  -ing  Leaf  Hght Brdth       Depth  ³                cm3/cm3  of
     3 soil                        ³'/
      DATA GROHEAD(3)/'!  DOY plant                    ³<---------------
     1 kg/Ha -------------->³       Kg/Ha        ³<Stress (0-1)>³    %  
     2   %   Area    m     m           m   ³<---------------------------
     3--------------------------->³'/
      DATA GROHEAD(4)/'@DATE   CDAY  L#SD  GSTD  LAID  LWAD  SWAD  FWAD 
     1 RWAD  BWAD  CRAD  SUGD  HIAD  EWAD  E#AD  WSPD  WSGD  NSTD  LN%D 
     2 SH%D  SLAD  CHTD  CWID  EWSD  RDPD  RL1D  RL2D  RL3D  RL4D  RL5D 
     3 RL6D  RL7D  RL8D  RL9D  RL10'/

      DATA NITHEAD(1)/'!YR     Days      Nitrogen      Nitrogen    Inorg
     1  Fix  Up-  leach  Soil  Soil  Leaf  Stem  Leaf  Stem  Shell Root 
     2 Nfix  Total Soil  ³<------------- Nitrate for Soil Layer --------
     3--------->³  ³<--------------- Ammonium for Soil Layer -----------
     4--->³'/
      DATA NITHEAD(2)/'! and   After Crop  Grain Veg. Grain  Veg.  N Fer
     1t      take       Inorg   Org    N     N     N     N     N     N  
     2 Rate   NO3   NH4    1     2     3     4     5     6     7     8  
     3   9    10     1     2     3     4     5     6     7     8     9  
     4  10'/
      DATA NITHEAD(3)/'!  DOY  Plant ³<--- Kg/Ha -->³ ³<-- % -->³  ³<---
     1--------- kg/ha ----------->³  ³<-kg/ha->³ ³<-------- % ------->³k
     2g/ha/d ³<-kg/ha->³ ³<---------------------------------------------
     3------- ug N/g soil ----------------------------------------------
     4--->³'/
      DATA NITHEAD(4)/'@DATE   CDAY  CNAD  GNAD  VNAD  GN%D  VN%D  NAPC 
     1 NFXC  NUPC  NLCC  NIAD  NOAD  LNAD  SNAD  LN%D  SN%D  SHND  RN%D 
     2 NFXD  NITD  NHTD  NI1D  NI2D  NI3D  NI4D  NI5D  NI6D  NI7D  NI8D 
     3 NI9D  NI10  NH1D  NH2D  NH3D  NH4D  NH5D  NH6D  NH7D  NH8D  NH9D 
     4 NH10'/

C-----------------------------------------------------------------------
C     Initialize counters, averages
C-----------------------------------------------------------------------

      IF (YRDOY .EQ. YRSIM) THEN
         DSTRES = 0.0
         STRESP = 0.0
         STRESS = 0.0
         STRESN = 0.0
         CWSV   = 0.0
         CWSR   = 0.0
         WTNUP  = 0.0

C-----------------------------------------------------------------------
C        Generate headings for output files
C-----------------------------------------------------------------------

         IF (IDETO .EQ. 'Y') THEN
            OPEN (UNIT=NOUTDO, FILE=OUTO, STATUS='UNKNOWN',
     &            ACCESS='APPEND')
         ENDIF

         IF (IDETG .EQ. 'Y') THEN
            OPEN (UNIT=NOUTDG, FILE=OUTG, STATUS='UNKNOWN',
     &            ACCESS='APPEND')
         ENDIF
         IF (IDETN .EQ. 'Y') THEN
            OPEN (UNIT=NOUTDN, FILE=OUTN, STATUS='UNKNOWN',
     &            ACCESS='APPEND')
         ENDIF
C-----------------------------------------------------------------------
C        Variable heading for GROWTH.OUT
C-----------------------------------------------------------------------

         IF (IDETG .EQ. 'Y') THEN
            IF (IDETL .EQ. 'Y') THEN
               WRITE (NOUTDG,2192) GROHEAD(1)
               WRITE (NOUTDG,2192) GROHEAD(2)
               WRITE (NOUTDG,2192) GROHEAD(3)
               WRITE (NOUTDG,2192) GROHEAD(4)
             ELSE
               WRITE (NOUTDG,2190) GROHEAD(1)
               WRITE (NOUTDG,2190) GROHEAD(2)
               WRITE (NOUTDG,2190) GROHEAD(3)
               WRITE (NOUTDG,2190) GROHEAD(4)
            ENDIF
         ENDIF

C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C        Variable heading for NITROGEN.OUT
C-----------------------------------------------------------------------

         IF (IDETN .EQ. 'Y') THEN
            IF (IDETL .EQ. 'Y') THEN
               WRITE (NOUTDN,2240) NITHEAD(1)
               WRITE (NOUTDN,2240) NITHEAD(2)
               WRITE (NOUTDN,2240) NITHEAD(3)
               WRITE (NOUTDN,2240) NITHEAD(4)
             ELSE
               WRITE (NOUTDN,2190) NITHEAD(1)
               WRITE (NOUTDN,2190) NITHEAD(2)
               WRITE (NOUTDN,2190) NITHEAD(3)
               WRITE (NOUTDN,2190) NITHEAD(4)
            ENDIF
         ENDIF

C   Calculate average values as a function of the output interval
C-----------------------------------------------------------------------

      WTLF   = LFWT   * PLTPOP
      SDWT   = GRNWT  * PLTPOP
      XLAI   = LAI
      IF (WTLF .GT. 0.0) THEN
         SLA  = LAI * 10000 / WTLF
       ELSE
         SLA = 0.0
      ENDIF
      SEEDNO = GPSM
      PODWT  = CRWNWT
      IF (GPP .GT. 0.0) THEN
         PODNO = SEEDNO/GPP
       ELSE
         PODNO = 0.0
      ENDIF

      WTNUP = WTNUP + TRNU * PLTPOP
C
C     Check for output frequency
C
      IF (YRDOY .EQ. HAREND) FROP = 1

      IF ((MOD(DAS,FROP) .EQ. 0) .OR. YRDOY .EQ. STGDOY(6)
     &                           .OR. YRDOY .EQ. YRPLT) THEN

C-----------------------------------------------------------------------
C        The following generates output for file NITROGEN.OUT
C-----------------------------------------------------------------------

         IF (IDETN .EQ. 'Y' .AND. ISWNIT .EQ. 'Y') THEN
            WTNCAN = (STOVN + GRAINN) * PLTPOP
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
            WTNFX = 0.0
            NFIXN = 0.0
            ANO3  = 0.0
            ANH4  = 0.0
            DO I = 1, NLAYR
               !
               ! Changed * to / -- 12-27-93 ... WTB & PWW
               !
               ANO3 = ANO3 + NO3(I) / FAC(I)
               ANH4 = ANH4 + NH4(I) / FAC(I)
            END DO
            TSIN = ANO3 + ANH4

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
            CALL OPNIT (NOUTDN,YRDOY,DAP,WTNCAN,WTNLF,WTNST,
     &          WTNSD,WTNSH,WTNUP,TLCH,TSIN,WTNFX,
     &          NO3,NH4,WTLF,STMWT,IDETL,SDWT,TSON,NFIXN,
     &          ANO3,ANH4,AMTNIT,PCNL,PCNST,PCNRT,PCNSH,PLTPOP)
         ENDIF

C-----------------------------------------------------------------------
C        The following generates output for GROWTH.OUT
C-----------------------------------------------------------------------

         IF (YRDOY .GE. YRPLT) THEN
            IF (IDETG .EQ. 'Y') THEN
               LEAFNO = LN
               VSTAGE = REAL (LEAFNO)
               IF (LEAFNO .GT. 0.0) THEN
                  RSTAGE = REAL(ISTAGE)
                ELSE
                  RSTAGE = 0.0
               ENDIF
               SDWT = GRNWT

               CALL OPGROW (NOUTDG,YRDOY,DAP,VSTAGE,XLAI,STMWT,
     &              SDWT,WTLF,BIOMAS,RTWT,PODWT,PODNO,SEEDNO,SLA,
     &              PCNL,TURFAC,CANHT,CANWH,RLV,IDETL,RTDEP,RSTAGE,
     &              SWFAC,NSTRES,PLTPOP,FRTWT,BASLFWT,CRWNWT,
     &              SKWT,SATFAC)
            ENDIF

         ENDIF
      ENDIF

C-----------------------------------------------------------------------
C     Call OPVIEW
C-----------------------------------------------------------------------

      CALL OPVIEW (NREP,TITLET,RNMODE,NOUTDO,IDETO,NYRS,DAP,BIOMAS,LAI,
     &    XN,CET,CRAIN,TOTIR,DOY,PESW,TURFAC,SWFAC,NSTRES,CWSV,CWSR,
     &    STGDOY,YRDOY,YRSIM,XANC,STOVN,GRAINN,PLTPOP,LUNOV,STNAME)

      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

 100  FORMAT (/,'*RUN ',I3,8X,': ',A25,/,
     &       1X,'MODEL',10X,':'1X,A8,' - ',A10,/,
     &       1X,'EXPERIMENT',5X,':',1X,A8,1X,A2,4X,A47,/,
     &       1X,'TREATMENT',1X,I2, 3X,':',1X,A25,/)
 2190 FORMAT (A80)
 2192 FORMAT (A210)
 2196 FORMAT (4(A5,I1))
 2197 FORMAT (10(A5,I1))
 2090 FORMAT (A240)
 2240 FORMAT (A252)
 2340 FORMAT (A252)

      END SUBROUTINE Aloha_OPDAY

C=======================================================================
C  Aloha_OPGROW, Subroutine
C
C  Generates output for growth data
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P. Wilkens  2-8-93
C  3. Added switch block, etc.                      P. Wilkens  2-8-93
C-----------------------------------------------------------------------
C  INPUT  : NOUTDG,YRDOY,DAP,VSTAGE,XLAI,STMWT,SDWT,WTLF,BIOMAS,RTWT,PODWT,
C           PODNO,SEEDNO,SLA,PCNL,TURFAC,CANHT,CANWH,RLV,IDETL,RTDEP,RSTAGE,
C           SWFAC,NSTRES,PLTPOP
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : OPDAY
C
C  Calls  :
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

       SUBROUTINE Aloha_OPGROW (NOUTDG,YRDOY,DAP,VSTAGE,XLAI,STMWT,
     &    SDWT,WTLF,BIOMAS,RTWT,PODWT,PODNO,SEEDNO,
     &    SLA,PCNL,TURFAC,CANHT,CANWH,RLV,IDETL,RTDEP,RSTAGE,
     &    SWFAC,NSTRES,PLTPOP,FRTWT,BASLFWT,CRWNWT,SKWT,SATFAC)

      IMPLICIT  NONE

      CHARACTER IDETL*1

      INTEGER   DAP,NOUTDG,YRDOY,I,RSTAGE

      REAL      VSTAGE,XLAI,STMWT,SDWT,WTLF,BIOMAS,RTWT,PODWT,SEEDNO
      REAL      SLA,PCNL,TURFAC,CANHT,CANWH,RLV(20),HI,SHELPC,SHELLW
      REAL      SDSIZE,PODNO,RTDEP,NSTRES,SWFAC,PLTPOP,GM2KG
      REAL      FRTWT,BASLFWT,CRWNWT,SKWT,SATFAC
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
      HI     = 0.0
      IF (BIOMAS .GT. 0.0 .AND. SDWT .GE. 0.0) THEN
         HI = SDWT*PLTPOP/BIOMAS
      ENDIF

      IF (IDETL .EQ. 'N') THEN
         WRITE (NOUTDG,300) MOD(YRDOY,100000),DAP,VSTAGE,RSTAGE,XLAI,
     &      NINT(WTLF*10.0),NINT(STMWT*GM2KG),NINT(FRTWT*GM2KG),
     &      NINT(RTWT*GM2KG),NINT(BASLFWT*10.0),NINT(CRWNWT*GM2KG),
     &      NINT(SKWT*GM2KG),HI
       ELSE IF (IDETL .EQ. 'Y') THEN
         WRITE (NOUTDG,400)MOD(YRDOY,100000),DAP,VSTAGE,RSTAGE,XLAI,
     &      NINT(WTLF*10.0),NINT(STMWT*GM2KG),NINT(FRTWT*GM2KG),
     &      NINT(RTWT*GM2KG),NINT(BASLFWT*10.0),NINT(CRWNWT*GM2KG),
     &      NINT(SKWT*GM2KG),HI,
     &      NINT(PODWT*GM2KG),NINT(PODNO),(1.0-SWFAC),(1.0-TURFAC),
     &      (1.0-NSTRES),PCNL,SHELPC,SLA,CANHT,CANWH,SATFAC,
     &      (RTDEP/100),(RLV(I),I=1,10)
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

 300  FORMAT (2(1X,I5),1X,F5.1,1X,I5,1X,F5.2,7(1X,I5),1X,F5.3)
 400  FORMAT (2(1X,I5),1X,F5.1,1X,I5,1X,F5.2,7(1X,I5),
     &        1X,F5.3,2(1X,I5),3(1X,F5.3),2(1X,F5.2),1X,F5.1,
     &        2(1X,F5.2),1X,F5.3,11(1X,F5.2))

      END SUBROUTINE Aloha_OPGROW

C=======================================================================
C  Aloha_OPNIT, Subroutine
C
C  Generates nitrogen aspects output file
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P. Wilkens  2-8-93
C  3. Added switch block, etc.                      P. Wilkens  2-8-93
C-----------------------------------------------------------------------
C  INPUT  : NOUTDN,YRDOY,DAP,WTNCAN,WTNLF,WTNST,WTNSD,WTNSH,WTNUP,TLCH,
C           TSIN,WTNFX,NO3,NH4,WTLF,STMWT,IDETL,SDWT,TSON,NFIXN,ANO3,ANH4,
C           AMTNIT,PCNL,PCNST,PCNRT,PCNSH,PLTPOP
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : OPDAY
C
C  Calls  :
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE Aloha_OPNIT (NOUTDN,YRDOY,DAP,WTNCAN,WTNLF,WTNST,
     &       WTNSD,WTNSH,WTNUP,TLCH,TSIN,WTNFX,
     &       NO3,NH4,WTLF,STMWT,IDETL,SDWT,TSON,NFIXN,
     &       ANO3,ANH4,AMTNIT,PCNL,PCNST,PCNRT,PCNSH,PLTPOP)

      IMPLICIT  NONE

      CHARACTER IDETL*1

      INTEGER   NOUTDN,YRDOY,DAP,I
      REAL      WTNCAN,WTNLF,WTNST,WTNSD,WTNSH,WTNUP,WTNFX
      REAL      TLCH,NO3(20),NH4(20)
      REAL      WTLF,STMWT,TSIN,TSON,ANO3,ANH4
      REAL      WTNVEG,WTNGRN,PCNVEG,PCNGRN,SDWT,NFIXN,AMTNIT
      REAL      PCNL,PCNST,PCNRT,PCNSH,PLTPOP

      WTNVEG  = (WTNLF + WTNST)
      WTNGRN  = (WTNSH + WTNSD)
      IF ((WTLF+STMWT) .GT. 0.0) THEN
         PCNVEG = (WTNLF+WTNST)/(WTLF+(STMWT*PLTPOP))*100.0
       ELSE
         PCNVEG = 0.0
      ENDIF
      IF (SDWT .GT. 0.0) THEN
         PCNGRN = WTNSD/SDWT*100
       ELSE
         PCNGRN = 0.0
      ENDIF

      IF (IDETL .EQ. 'N') THEN
        WRITE (NOUTDN,200) MOD(YRDOY,100000),DAP,(WTNCAN*10.0),
     &        (WTNSD*10.0),(WTNVEG*10.0),PCNGRN,PCNVEG,NINT(AMTNIT),
     &        (WTNFX*10.0),(WTNUP*10.0),TLCH,TSIN,NINT(TSON)

       ELSE IF (IDETL .EQ. 'Y') THEN
        WRITE (NOUTDN,300) MOD(YRDOY,100000),DAP,(WTNCAN*10.0),
     &        (WTNSD*10.0),(WTNVEG*10.0),PCNGRN,PCNVEG,NINT(AMTNIT),
     &        (WTNFX*10.0),(WTNUP*10.0),TLCH,TSIN,NINT(TSON),
     &        (WTNLF*10.0),(WTNST*10.0),PCNL,PCNST,PCNSH,PCNRT,NFIXN*10,
     &         ANO3,ANH4,(NO3(I),I=1,10),(NH4(I),I=1,10)
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

 200  FORMAT (2(1X,I5),3(1X,F5.1),2(1X,F5.2),1X,I5,4(1X,F5.1),1X,I5)
 300  FORMAT (2(1X,I5),3(1X,F5.1),2(1X,F5.2),1X,I5,4(1X,F5.1),1X,I5,
     &        2(1X,F5.1),4(1X,F5.2),23(1X,F5.1))

      END SUBROUTINE Aloha_OPNIT


!C=======================================================================
!C  OPVIEW, Subroutine
!C
!C  Generates stage specific summary outputs to screen and NOUTDO file
!C-----------------------------------------------------------------------
!C  Revision history
!C
!C  1. Written
!C  2  Modified by
!C  3. Header revision and minor changes             P.W.W.      2-8-93
!C-----------------------------------------------------------------------
!C  INPUT  : NREP,TITLET,IDETV,NOUTDO,IDETO,NYRS,DAP,BIOMAS,LAI,XN,CET,CRAIN,
!C           TOTIR,DOY,PESW,TURFAC,SWFAC,NSTRES,CWSV,CWSR,STGDOY,YRDOY,
!C           YRSIM,XANC,STOVN,GRAINN,PLTPOP,LUNOV
!C
!C  LOCAL  :
!C
!C  OUTPUT :
!C-----------------------------------------------------------------------
!C  Called : OPDAY
!C
!C  Calls  : YR_DOY NAILUJ
!C-----------------------------------------------------------------------
!C                         DEFINITIONS
!C
!C  HDLAY  :
!C=======================================================================
!
!      SUBROUTINE OPVIEW (NREP,TITLET,RNMODE,NOUTDO,IDETO,NYRS,DAP,
!     &    BIOMAS,LAI,XN,CET,CRAIN,TOTIR,DOY,PESW,TURFAC,SWFAC,NSTRES,
!     &    CWSV,CWSR,STGDOY,YRDOY,YRSIM,XANC,STOVN,GRAINN,PLTPOP,
!     &    LUNOV,STNAME)
!
!      IMPLICIT     NONE
!
!      CHARACTER*1  IDETO,RNMODE
!      CHARACTER*3  RMM
!      CHARACTER*25 TITLET
!      CHARACTER*10 STNAME(20)
!
!      INTEGER      I,NREP,YR,DAP,NOUTDO,IPX,DOY,NYRS
!      INTEGER      STGDOY(20),YRDOY,YRSIM,LUNOV
!
!      REAL         BIOMAS,LAI,XN,CET,CRAIN,TOTIR,PESW,TURFAC
!      REAL         SWFAC,NSTRES,CWSV,CWSR,DSTRES,STRESS,STRESP,STRESN
!      REAL         XANC,AVDSTR,AVNSTR,STOVN,GRAINN,PLTPOP,WTNCAN
!
!C-----------------------------------------------------------------------
!C     Initialize counters, averages
!C-----------------------------------------------------------------------
!
!      IF (YRDOY .EQ. YRSIM) THEN
!         DSTRES = 0.0
!         STRESP = 0.0
!         STRESS = 0.0
!         STRESN = 0.0
!         CWSV   = 0.0
!         CWSR   = 0.0
!      ENDIF
!
!      DSTRES = DSTRES + 1.0
!      STRESS = STRESS + TURFAC
!      STRESP = STRESP + SWFAC
!      STRESN = STRESN + NSTRES
!
!      IF (DSTRES .GE. 1.0) THEN
!         AVDSTR = 1.0 - (STRESS/DSTRES)
!         AVNSTR = 1.0 - (STRESN/DSTRES)
!      ENDIF
!
!C-----------------------------------------------------------------------
!C     Write to OUTO output file at defined phenological stages
!C-----------------------------------------------------------------------
!
!      IF (RNMODE .EQ. 'I' .AND. YRDOY .EQ. YRSIM .AND. NYRS .LE. 1) THEN
!         CALL CLEAR
!         WRITE (LUNOV,3050) NREP,TITLET              ! Write header to screen
!         WRITE (LUNOV,3200)
!      ENDIF
!
!      IF (IDETO .EQ. 'Y' .AND. YRDOY .EQ. YRSIM) THEN
!         WRITE (NOUTDO,3050) NREP,TITLET
!         WRITE (NOUTDO,3200)
!      ENDIF
!
!      DO I = 1, 20
!         IF (YRDOY .EQ. STGDOY(I)) THEN
!            DSTRES  = 0.0
!            STRESS  = 0.0
!            STRESP  = 0.0
!            STRESN  = 0.0
!
!C-----------------------------------------------------------------------
!C           DAP is days after planting, DAP = 0 on day of planting
!C-----------------------------------------------------------------------
!
!            WTNCAN = (STOVN+GRAINN)*PLTPOP*10.0
!            IF (BIOMAS .GT. 0.0) THEN
!               XANC = (WTNCAN/(BIOMAS*10.0))*100.0
!             ELSE
!               XANC = 0.0
!            ENDIF
!            IF (YRDOY .EQ. STGDOY(8)) THEN
!               XANC = 0.0
!            ENDIF
!            CALL YR_DOY (YRDOY,YR,DOY)
!            CALL NAILUJ (DOY,YR,RMM,IPX)
!
!           IF (RNMODE .EQ. 'I' .AND. NYRS .LE. 1) THEN
!              WRITE (LUNOV,4600) IPX,RMM,DAP,STNAME(I),
!     &        NINT(BIOMAS*10.0),LAI,XN,NINT(CET),NINT(CRAIN),
!     &        NINT(TOTIR),NINT(PESW*10.0),NINT(WTNCAN),
!     &        XANC,AVDSTR,AVNSTR
!            ENDIF
!
!            IF (IDETO .EQ. 'Y') THEN
!               WRITE (NOUTDO,4600) IPX,RMM,DAP,STNAME(I),
!     &         NINT(BIOMAS*10.0),LAI,XN,NINT(CET),NINT(CRAIN),
!     &         NINT(TOTIR),NINT(PESW*10.0),NINT(WTNCAN),
!     &         XANC,AVDSTR,AVNSTR
!            ENDIF
!         ENDIF
!      END DO
!
!      RETURN
!
!C-----------------------------------------------------------------------
!C     Format Strings
!C-----------------------------------------------------------------------
!
! 3050 FORMAT (//,'*SIMULATED CROP AND SOIL STATUS AT MAIN',
!     &        ' DEVELOPMENT STAGES',//,' RUN NO. ',I2,4X,A25,/)
! 3200 FORMAT (3X,'DATE',1X,'CROP',1X,'GROWTH',3X,
!     &       'BIOMASS',2X,'LAI ',1X,'LEAF',3X,'ET',2X,'RAIN',1X,'IRRIG',
!     &       1X,'SWATER',1X,'CROP  N',2X,'STRESS',/,8X,'AGE',2X,'STAGE',
!     &       5X,'kg/ha',9X,'NUM.',2X,'mm',3X,'mm',4X,'mm',4X,'mm',3X,
!     &       'kg/ha %',1X,' H2O  N',/,80('-'))
! 4600 FORMAT (1X,I2,1X,A3,2X,I3,1X,A10,I6,1X,F5.2,1X,F4.1,I5,
!     &        3I6,I5,F4.1,2F4.2)
!
!      END
