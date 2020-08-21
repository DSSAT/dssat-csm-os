C=======================================================================
C  ETPHR, Subroutine, N.B. Pickering
C  Computes canopy ET (mm/h) and gross photosynthesis (mmol CO2/m2/s)
C  for each hour.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  12/05/90 NBP Written
C  11/05/93 KJB Added layers for SLW and leaf N
C  11/23/93 NBP Removed SLW effect from INVEG, all in PGLFEQ
C  12/11/93 NBP Removed AWES1 from calc. of Rsoil.
C  04/21/94 NBP Check in CANPET prevents small trans. amounts for LAI=0.
C  01/10/00 NBP Modular version
C-----------------------------------------------------------------------
C  Called from: ETPHOT
C  Calls:       CANPET,CANOPG,HSOILT
C=======================================================================

      SUBROUTINE ETPHR(CONTROL,H,
     &  CANHT, CEC, CEN, CLOUDS, CO2HR, DAYTIM,           !Input
     &  DLAYR2, DULE, FNPGL, FNPGN, FRACSH, FRSHV,        !Input
     &  KDIRBL, LAISH, LAISHV, LAISL, LAISLV, LLE,        !Input
     &  LMXREF, LNREF, LWIDTH, MEEVP, MEPHO, NLAYR,       !Input
     &  NSLOPE, PARSH, PARSUN, QEREF, RABS, RCUTIC,       !Input
     &  REFHT, RHUMHR, RNITP, RWUH, SHCAP, SLAAD,         !Input
     &  SLWREF, SLWSLO, STCOND, SWE,SWTD,TAIRHR, TA,          !Input
     &  TMIN, TYPPGL, TYPPGN, WINDHR, XLAI,               !Input
     &  XLMAXT, YLMAXT, XSW,YSCOND,YSHCAP,                !Input (CSV CXSW,YSCOND,YSHCAP)
     &  AGEFAC, EHR, LFMXSH, LFMXSL, PCNLSH, PCNLSL,      !Output
     &  PGHR,PGSL,PGSH, SLWSH, SLWSL, T0HR, TCAN,         !Output  
     &  TCAN1, THR, TSHR, TSURF,SWEH,                        !Output
     &  CONDSH, CONDSL, RA, RB, RSURF, RNET,              !Output
     &  G, LH, LHEAT, RSSH, RSSL, RSSS, SH, SHEAT,        !Output
!     Added by BAK on 10DEC15
     &  RBSH, RBSL,RBSS,EMISAV,TK4CAN,TK4SKY,             !Output
     &  CCNEFF, CICAD, CMXSF, CQESF, PGPATH,DIFPR,        !Input
     &  AGEQESL, CO2QESL, QEFFSL,CISH,CISL,VPDSL,VPDSH)   !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types,
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      CHARACTER MEEVP*1,MEPHO*1,TYPPGN*3,TYPPGL*3

      INTEGER ITER,NLAYR,H,J,YRDOY,DAS

      LOGICAL DAYTIM,REPEAT,STRESS

      REAL AGEFAC,CANHT,CEC,CEN,CLOUDS,CO2HR,CONDSH,
     &  CONDSL,CSHPRV,CSHSTR,CSLPRV,CSLSTR,DLAYR2(NL),DULE,EHR,EMAXTC,
     &  EMAXTR,ERRBND,ERRTC,ERRTR,FNPGN(4),FNPGL(4),FRACSH,FRSHV,HOLD,
     &  KDIRBL,LAISH,LAISHV,LAISL,LAISLV,LFMXSH,LFMXSL,LLE,LNREF,
     &  LWIDTH,LMXREF,NSLOPE,PARSH,PARSUN(3),PCNLSH,PCNLSL,PGHR,QEREF,
     &  RA,RABS(3),RCUTIC,REFHT,RHUMHR,RNITP,RWUH,SHCAP(NL),SLAAD,
     &  SLWREF,SLWSH,SLWSL,SLWSLO,STCOND(NL),SWE,SWEH,SWEH2,SWTD,T0HR,
     &  TAIRHR,
     &  TA,TMIN,TCAN,TCPREV,THR,TPREV,TSHR(NL),TSUM,TSURF(3,1),USTAR,
     &  WINDHR,XLAI,XLMAXT(6),YLMAXT(6),EMISAV,TK4CAN,TK4SKY,
C       following lines added by CSVC
     & TABEX,SW2,YHC(3),YTC(3),XSW(NL,3),YSCOND(NL,3),DIFPR, UPRATE,SSH,
     & YSHCAP(NL,3),XC(3),TCAN1 ,VPSAT,PGSL,PGSH,CISH,CISL,VPDSL,VPDSH

!     Added by BAK
      REAL RB(3),RSURF(3),RNET(3,1),
     &  G, LH, LHEAT(3,1), RSSH, RSSL, RSSS, SH, SHEAT(3,1),
     &  RBSH, RBSL, RBSS,STCONDSS,TW
      CHARACTER PGPATH*2
      REAL CCNEFF, CICAD, CMXSF, CQESF
      REAL AGEQESL, CO2QESL, QEFFSL

      TYPE (ControlType) CONTROL

      PARAMETER (ERRBND=0.01)

C     Initialize.

      YRDOY   = CONTROL % YRDOY
      DAS   = CONTROL % DAS

      EMAXTC = ERRBND * 30.0
      EMAXTR = ERRBND * 1.0

      AGEFAC = 1.0
      EHR = 0.0
      LFMXSH = 0.0
      LFMXSL = 0.0
      PCNLSH = 0.0
      PCNLSL = 0.0
      PGHR = 0.0
      RA = 0.0
      USTAR = 0.0
      SLWSH = 0.0
      SLWSL = 0.0
      T0HR = 0.0
      TCAN = TAIRHR
      THR = 0.0
      TSURF(1,1) = TAIRHR
      TSURF(2,1) = TAIRHR
      TSURF(3,1) = TAIRHR
      STRESS = .FALSE.


C     Daylight hours with canopy.

      IF (DAYTIM .AND. XLAI .GT. 0.0) THEN
        REPEAT = .TRUE.
        ITER = 1
        TSUM = 0.0

C       Loop until evapotranspiration and photosynthesis are stable.

        IF (MEEVP.EQ.'Z' .AND. MEPHO.EQ.'L') THEN

          DO WHILE (REPEAT .AND. ITER .LE. 5) !Adjust NET radiation to update surface temp
            TCPREV = TCAN
            CALL CANOPG(
     &        CO2HR, FNPGL, FNPGN, LAISH, LAISL, LMXREF,  !Input
     &        LNREF, NSLOPE, PARSH, PARSUN, QEREF, RNITP, !Input
     &        SLAAD, SLWSLO, TMIN, TSURF, TYPPGL, TYPPGN, !Input
     &        XLMAXT, YLMAXT,RHUMHR,WINDHR,               !Input
     &        AGEFAC, CONDSH, CONDSL, CSHSTR, CSLSTR,     !Output
     &   LFMXSH, LFMXSL, PCNLSH, PCNLSL,PGHR,PGSL,PGSH,   !Output
     &        SLWREF, SLWSH, SLWSL, STRESS,               !Output
     &        CCNEFF,CICAD,CMXSF,CQESF,PGPATH,MEEVP,      !Input
     &        AGEQESL,CO2QESL,QEFFSL,CISH,CISL,VPDSL,VPDSH)        !Output

            CALL CANPET(
     &        CANHT, CEC, CEN, CLOUDS, CONDSH, CONDSL,    !Input
     &        DLAYR2, FRACSH, FRSHV, KDIRBL, LAISH,       !Input
     &        LAISHV, LAISL, LAISLV, LWIDTH, RABS,        !Input
     &        RCUTIC, REFHT, RHUMHR, STCOND, TAIRHR,      !Input
     &        WINDHR, DIFPR,STCONDSS,SHCAP,NLAYR,         !Input
     &        EHR, RA, TCAN, THR, TSHR, TSURF, USTAR,     !Output
     &        RB, RSURF, RNET,EMISAV,TK4CAN,TK4SKY,       !Output
     &        G, LH, LHEAT, RSSH, RSSL, RSSS, SH, SHEAT,  !Output
C        G, LH, LHEAT, RSSH, RSSL, RSSS, SH, SHEAT
C         RB, RSURF RNET output added DEC2014 by Bruce Kimball
     &        RBSL, RBSL, RBSS)                           !Output
C          added by BAK on 10DEC2015

             IF(ITER.EQ.1) TCAN1=TCAN

            TSUM = TSUM + TCAN
            IF (ITER .GT. 5) THEN
              TCAN = TSUM / ITER
            ELSE
              TCAN = (TCAN+TCPREV) / 2.0
            ENDIF
            ERRTC = ABS(TCAN-TCPREV)
            REPEAT = ERRTC .GT. EMAXTC
            ITER = ITER + 1
          ENDDO
          T0HR = THR

C         Water stress loop.  Evapotranspiration limited to by soil water
C         supply by adjusting leaf conductances.  Photosynthesis recalulated.

          IF (THR .GT. RWUH .or. EHR.GT.SWEH) THEN

       
            IF(THR .GT. RWUH) CONDSL = CONDSL * RWUH/THR
            IF(THR .GT. RWUH) CONDSH = CONDSH * RWUH/THR

            IF (EHR.GT.SWEH) CEN = 100

C CSVC - necessary if not use Gs control by VPD (i.e.,  Gs_code='OFF') 
c CSVC         IF (THR .GT. RWUH.OR.THR.GT.0.5) THEN
c CSVC           CONDSL = CONDSL * MIN(0.5,RWUH)/THR
c            CONDSH = CONDSH * MIN(0.5,RWUH)/THR

            CSLPRV = CONDSL
            CSHPRV = CONDSH
            REPEAT = .TRUE.
            ITER = 1
            TSUM = 0.0
            DO WHILE (REPEAT .AND. ITER .LE. 5)
              TCPREV = TCAN
              TPREV = THR
            CALL CANPET(
     &        CANHT, CEC, CEN, CLOUDS, CONDSH, CONDSL,  !Input
     &        DLAYR2, FRACSH, FRSHV, KDIRBL, LAISH,     !Input
     &        LAISHV, LAISL, LAISLV, LWIDTH, RABS,      !Input
     &        RCUTIC, REFHT, RHUMHR, STCOND, TAIRHR,    !Input
     &        WINDHR,DIFPR,STCONDSS,SHCAP,NLAYR,        !Input
     &        EHR, RA, TCAN, THR, TSHR, TSURF, USTAR,   !Output
     &        RB, RSURF, RNET,EMISAV,TK4CAN,TK4SKY,     !Output
     &        G, LH, LHEAT, RSSH, RSSL, RSSS, SH, SHEAT,!Output
     &        RBSH, RBSL, RBSS)                         !Output
              TSUM = TSUM + TCAN
              IF (ITER .GT. 5) THEN
                TCAN = TSUM / ITER
              ELSE
                TCAN = (TCAN+TCPREV) / 2.0
              ENDIF
              ERRTC = ABS(TCAN-TCPREV)
              ERRTR = ABS(THR-RWUH)
CSVC   HOLD = CSLPRV+(CONDSL-CSLPRV)/(THR-TPREV)*(RWUH-TPREV)
       HOLD = CSLPRV+(CONDSL-CSLPRV)*MAX((RWUH-TPREV)/(THR-TPREV),.05) !CSVC ADD 07/13/2018
              CSLPRV = CONDSL
              CONDSL = MAX(1.0/RCUTIC,HOLD)
CSVC   HOLD = CSHPRV+(CONDSH-CSHPRV)/(THR-TPREV)*(RWUH-TPREV)
       HOLD = CSHPRV+(CONDSH-CSHPRV)*MAX((RWUH-TPREV)/(THR-TPREV),.05) !CSVC ADD 07/13/2018
              CSHPRV = CONDSH
              CONDSH = MAX(1.0/RCUTIC,HOLD)
              REPEAT = ERRTR .GT. EMAXTR .AND. ERRTC .GT. EMAXTC
              ITER = ITER + 1
            ENDDO
            CSLSTR = CONDSL
            CSHSTR = CONDSH
            STRESS = .TRUE.
            CALL CANOPG(
     &        CO2HR, FNPGL, FNPGN, LAISH, LAISL, LMXREF,  !Input
     &        LNREF, NSLOPE, PARSH, PARSUN, QEREF, RNITP, !Input
     &        SLAAD, SLWSLO, TMIN, TSURF, TYPPGL, TYPPGN, !Input
     &        XLMAXT, YLMAXT, RHUMHR,WINDHR,              !Input
     &        AGEFAC, CONDSH, CONDSL, CSHSTR, CSLSTR,     !Output
     &     LFMXSH, LFMXSL,PCNLSH,PCNLSL,PGHR,PGSL,PGSH,   !Output
     &        SLWREF, SLWSH, SLWSL, STRESS,               !Output
     &        CCNEFF,CICAD,CMXSF,CQESF,PGPATH,MEEVP,      !Input
     &        AGEQESL,CO2QESL,QEFFSL,CISH,CISL,VPDSL,VPDSH)   !Output
     
            STRESS = .FALSE.
          ENDIF
        ELSE
          CALL CANOPG(
     &      CO2HR, FNPGL, FNPGN, LAISH, LAISL, LMXREF,    !Input
     &      LNREF, NSLOPE, PARSH, PARSUN, QEREF, RNITP,   !Input
     &      SLAAD, SLWSLO, TMIN, TSURF, TYPPGL, TYPPGN,   !Input
     &      XLMAXT, YLMAXT, RHUMHR,WINDHR,                !Input
     &      AGEFAC, CONDSH, CONDSL, CSHSTR, CSLSTR,       !Output
     &   LFMXSH, LFMXSL, PCNLSH, PCNLSL,PGHR,PGSL,PGSH,   !Output
     &      SLWREF, SLWSH, SLWSL, STRESS,                 !Output
     &      CCNEFF,CICAD,CMXSF,CQESF,PGPATH,MEEVP,        !Input
     &      AGEQESL,CO2QESL,QEFFSL,CISH,CISL,VPDSL,VPDSH) !Output
        ENDIF

C     Night hours or bare soil.

      ELSE
        IF (MEEVP .EQ. 'Z') THEN
          CONDSH = 0.0
          CONDSL = 0.0
          REPEAT = .TRUE.
          ITER = 1
          TSUM = 0.0
          DO WHILE (REPEAT .AND. ITER .LE. 5)
            TCPREV = TCAN
            CALL CANPET(
     &        CANHT, CEC, CEN, CLOUDS, CONDSH, CONDSL,    !Input
     &        DLAYR2, FRACSH, FRSHV, KDIRBL, LAISH,       !Input
     &        LAISHV, LAISL, LAISLV, LWIDTH, RABS,        !Input
     &        RCUTIC, REFHT, RHUMHR, STCOND, TAIRHR,      !Input
     &        WINDHR, DIFPR,STCONDSS,SHCAP,NLAYR,               !Input
     &        EHR, RA, TCAN, THR, TSHR, TSURF, USTAR,     !Output
     &        RB, RSURF, RNET,EMISAV,TK4CAN,TK4SKY,       !Output
     &        G, LH, LHEAT, RSSH, RSSL, RSSS, SH, SHEAT,  !Output
C        G, LH, LHEAT, RSSH, RSSL, RSSS, SH, SHEAT
C         RB, RSURF RNET output added on 1DEC2014 by Bruce Kimball
     &        RBSL, RBSL, RBSS)                           !Output
C             added by BAK on 10DEC2015

            TSUM = TSUM + TCAN
            IF (ITER .GT. 5) THEN
              TCAN = TSUM / ITER
            ELSE
              TCAN = (TCAN+TCPREV) / 2.0
            ENDIF
            ERRTC = ABS(TCAN-TCPREV)
            REPEAT = ERRTC .GT. EMAXTC
            ITER = ITER + 1
          ENDDO
          T0HR = THR
        ENDIF
      ENDIF
      PGHR = MAX(PGHR,0.0)
      T0HR = MAX(T0HR,0.0)
      THR = MAX(THR,0.0)
      EHR = MAX(EHR,0.0)

C     Update soil moisture in upper layer, soil and canopy-air temperature
C     difference.

      IF (MEEVP .EQ. 'Z') THEN

        CALL HSOILT(CONTROL,H,G,
     &    DLAYR2, NLAYR, SHCAP, STCOND,STCONDSS, TA, TSURF(3,1),   !Input
     &    TSHR)                                           !Output


C SVC - Original
C SVC                 SWEH = MAX(SWEH-EHR,0.0)      
C SVC        CEN = MIN(MAX(0.0, 100.0*(DULE-SWE) / (DULE-LLE) ),100.0)
C SVC

C CSVC - New implementation

c           UPRATE = 1./2.
        SSH = 2.0 !Soil Skin hight
        TW  = 1./4.  !fraction soil water homogenization with the rest of the soil (1st and 2 layers) per time step

      
      IF(SSH.LT.SWTD)THEN !SWTD is the total deepth of SWE

           IF (H.EQ.1) THEN
            SWEH = SWE * (SSH/SWTD) !E can access only the first SSH cm's
            SWE  = SWE - SWEH 
            SWEH2 = SWEH 
           ELSE
             IF ( SWEH.LT. (SWE*SSH/SWTD) ) THEN
              SWEH = SWEH + TW*( ((SWE/SWTD)-(SWEH/SSH)) * SSH )
              SWE  = SWE  - TW*( ((SWE/SWTD)-(SWEH/SSH)) * SSH )
             ENDIF
          ENDIF ! (H.EQ.1) 

          SWEH = MAX(SWEH-EHR,0.0)
          CEN  = MIN(MAX(0.0, 100.0*( (DULE*SSH/SWTD)- SWEH ) / 
     &                ((DULE*SSH/SWTD)-(LLE*SSH/SWTD)) ),100.0)

          SW2  = (SWE+SWEH)/(SWTD*10.0)

          SWEH2 = 0.5*SWEH2 + 0.5*SWEH

       ELSE !IF(SSH.LT.SWTD)THEN (i.e., do not use SWEH)
         SWE = MAX(SWE-EHR,0.0)
         CEN = MIN(MAX(0.0, 100.0*(DULE-SWE) / (DULE-LLE) ),100.0)
         SWEH= SWE
         SW2=SWE/(SWTD*10.0) !SVC - original -> (DLAYR2(1)*10.0)

       ENDIF        



        DO J=1,3
          XC(J) = XSW(1,J)
          YHC(J) = YSHCAP(1,J)
          YTC(J) = YSCOND(1,J)
        ENDDO
        SHCAP(1) = TABEX(YHC,XC,SW2,3)
        STCOND(1) = TABEX(YTC,XC,SW2,3)
        STCONDSS  =  TABEX(YTC,XC,SWEH2/(SSH*10.),3)*0.5
C previous two lines uncommented by BK and KB on 11Jul17
C per version 3.5 of DSSAT

c      print*,YRDOY,DAS,H,SWEH,SWE,EHR,CEN

      ENDIF

      RETURN
      END SUBROUTINE ETPHR

C========================================================================
C  CANOPG, Subroutine, K.J. Boote, J.W. Jones, G. Hoogenboom
C  Computes instantaneous canopy photosynthesis (mmol CO2/m2/s) and leaf
C  CO2 conductance (cm/s) of shaded and sunlit leaves.  Uses shaded
C  and sunlit leaf areas, light intensity on each, and integrates
C  over leaf angle classes.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  05/01/89     Written
C  11/15/90 NBP Reorganized
C  11/05/93 KJB Added layers for SLW and leaf N
C-----------------------------------------------------------------------
C  Called from: ETPHR
C  Calls:       PGLFEQ,PGLEAF
C=======================================================================

      SUBROUTINE CANOPG(
     &  CO2HR, FNPGL, FNPGN, LAISH, LAISL, LMXREF,        !Input
     &  LNREF, NSLOPE, PARSH, PARSUN, QEREF, RNITP,       !Input
     &  SLAAD, SLWSLO, TMIN, TSURF, TYPPGL, TYPPGN,       !Input
     &  XLMAXT, YLMAXT, RHUMHR,WINDHR,                    !Input
     &  AGEFAC, CONDSH, CONDSL, CSHSTR, CSLSTR,           !Output
     &  LFMXSH, LFMXSL, PCNLSH, PCNLSL, PGHR,PGSL,PGSH,   !Output
     &  SLWREF, SLWSH, SLWSL, STRESS,                     !Output
     &  CCNEFF,CICAD,CMXSF,CQESF,PGPATH,MEEVP,            !Input
     &  AGEQESL,CO2QESL,QEFFSL,CISH,CISL,VPDSL,VPDSH)     !Output

      IMPLICIT  NONE
      SAVE

      CHARACTER TYPPGN*3,TYPPGL*3,MEEVP*1,Gs_code*3
      INTEGER I,CIEFFECT,NINT
      LOGICAL STRESS
      REAL AGEFAC,AGMXSH,AGMXSL,CO2HR,CONDSH,CONDSL,CONSUM,CONSUN,
     &  LAISH,LAISL,LMXREF,LFMXSH,LFMXSL,LNREF,NSLOPE,PARSH,
     &  PARSUN(3),PARSL,PGHR,PGSUM,PGSUN,PGSH,PGSL,QEREF,QEFFSH,
     &  QEFFSL,RNITP,SLAAD,TEMPSH,TEMPSL,TSURF(3,1),FNPGN(4),
     &  FNPGL(4),XLMAXT(6),XLAI,YLMAXT(6),CSLSTR,CSHSTR,SLWSL,
     &  SLWSH,PCNLSL,PCNLSH,SLWSLO,SLWREF,TMIN,
     &  VPDSL,VPDSH,RHUMHR,VPSAT,WINDHR

      CHARACTER PGPATH*2
      REAL CCNEFF, CICAD, CMXSF, CQESF
      REAL AGEQESH, AGEQESL, CO2QESH, CO2QESL
      REAL  CICA,CISL,CISH,CISUN,CISUM,CINT

C     Initialize.

      TEMPSL = TSURF(1,1)
      TEMPSH = TSURF(2,1)
      XLAI = LAISL + LAISH
      PARSL = PARSUN(2)
CSVC
       VPDSL = (VPSAT(TEMPSL)/1000)*(1- (RHUMHR / 100.0))
       VPDSH = (VPSAT(TEMPSH)/1000)*(1- (RHUMHR / 100.0))
CSVC
C     Calculate leaf photosynthesis parameters with separate layering
C     of SLW and leaf N.  SLW and leaf N decrease linearly with
C     increasing LAI (Sinclair et al., 1993).  Assume sunlit (SL)
C     leaves are physically above shaded (SH) leaves.

      SLWSL = 1./SLAAD + 0.5*SLWSLO*LAISH
      SLWSH = 1./SLAAD - 0.5*SLWSLO*LAISL
      PCNLSL = RNITP + 0.5*NSLOPE*LAISH
      PCNLSH = RNITP - 0.5*NSLOPE*LAISL


        Gs_code='ON'

        IF (Gs_code.eq.'OFF') then


        IF (PGPATH .EQ. "C4" .OR. PGPATH .EQ. "c4") THEN
           CICA = CICAD
           CINT = CICA*CO2HR 
        ELSE
           CICA = 0.7
           CINT = CICA*CO2HR 
        ENDIF

         CISL = CINT
         CISH = CINT

C     Calulate leaf photosynthesis for SH and SL leaves.

      CALL PGLFEQ_ORIGINAL(
     &  CO2HR, FNPGL, FNPGN, LMXREF, LNREF, QEREF,        !Input
     &  PCNLSH, SLWSH, SLWREF, TEMPSH, TMIN, TYPPGL,      !Input
     &  TYPPGN, XLMAXT, YLMAXT,                           !Input
     &  AGMXSH, LFMXSH, QEFFSH,                           !Output
     &  CCNEFF, CICAD, CMXSF,CQESF,PGPATH,                !Input
     &  CO2QESH,AGEQESH)                                  !Output
      CALL PGLFEQ_ORIGINAL(
     &  CO2HR, FNPGL, FNPGN, LMXREF, LNREF, QEREF,        !Input
     &  PCNLSL, SLWSL, SLWREF, TEMPSL, TMIN, TYPPGL,      !Input
     &  TYPPGN, XLMAXT, YLMAXT,                           !Input
     &  AGMXSL, LFMXSL, QEFFSL,                           !Output
     &  CCNEFF, CICAD, CMXSF,CQESF,PGPATH,                !Input
     &  CO2QESL,AGEQESL)                                  !Output

C     Gaussian integration of photosynthesis and leaf CO2 conductance
C     over three leaf classes for sunlit leaves.

      PGSUM = 0.0
      CONSUM = 0.0
      DO I=1,3
        CALL PGLEAF_ORIGINAL(
     &    CO2HR, LFMXSL, PARSUN(I), QEFFSL, TEMPSL,       !Input
     &    CONSUN, PGSUN,                                  !Output
     &    CCNEFF,CICAD,PGPATH,VPDSL)                            !Input
        IF (I .EQ. 2) THEN
          PGSUM = PGSUM + PGSUN*1.6
          CONSUM = CONSUM + CONSUN*1.6
        ELSE
          PGSUM = PGSUM + PGSUN
          CONSUM = CONSUM + CONSUN
        ENDIF
      ENDDO
      PGSL = PGSUM / 3.6
      CONDSL = CONSUM / 3.6

C     Compute photosynthesis and leaf CO2 conductance for shaded leaves

       CALL PGLEAF_ORIGINAL(
     &  CO2HR, LFMXSH, PARSH, QEFFSH, TEMPSH,             !Input
     &  CONDSH, PGSH,                                     !Output
     &  CCNEFF,CICAD,PGPATH,VPDSH)                              !Input



        ELSE !Gs_code.eq.'ON'

C     Calulate leaf photosynthesis for SL and SH leaves.

        IF (PGPATH .EQ. "C4" .OR. PGPATH .EQ. "c4") THEN
           CICA = CICAD
           CINT = CICA*CO2HR 
        ELSE
           CICA = 0.7
           CINT = CICA*CO2HR 
        ENDIF

       CISL = CINT
       CISH = CINT

        IF (MEEVP .EQ. 'Z') THEN
        NINT=2
        ELSE
        NINT=1
        ENDIF

       DO CIEFFECT=1,NINT


      CALL PGLFEQ(
     &  CO2HR, FNPGL, FNPGN, LMXREF, LNREF, QEREF,        !Input
     &  PCNLSL, SLWSL, SLWREF, TEMPSL, TMIN, TYPPGL,      !Input
     &  TYPPGN, XLMAXT, YLMAXT,CISL,                      !Input
     &  CCNEFF, CICAD, CMXSF,CQESF,PGPATH,MEEVP,          !Input
     &  AGMXSL, LFMXSL, QEFFSL,CO2QESL,AGEQESL)           !Output

      CALL PGLFEQ(
     &  CO2HR, FNPGL, FNPGN, LMXREF, LNREF, QEREF,        !Input
     &  PCNLSH, SLWSH, SLWREF, TEMPSH, TMIN, TYPPGL,      !Input
     &  TYPPGN, XLMAXT, YLMAXT,CISH,                      !Input
     &  CCNEFF, CICAD, CMXSF,CQESF,PGPATH,MEEVP,          !Input
     &  AGMXSH, LFMXSH, QEFFSH,CO2QESH,AGEQESH)           !Output


C     Gaussian integration of photosynthesis and leaf CO2 conductance
C     over three leaf classes for sunlit leaves.

      PGSUM = 0.0
      CONSUM = 0.0
      CISUM =  0.0

      DO I=1,3
        CALL PGLEAF(
     &   CO2HR, LFMXSL, PARSUN(I), QEFFSL, TEMPSL,WINDHR, !Input
     &   CCNEFF,CICAD,PGPATH,VPDSL,MEEVP,CIEFFECT,        !Input
     &   CONSUN, PGSUN,CISUN)                             !Output

        IF (I .EQ. 2) THEN
          PGSUM = PGSUM + PGSUN*1.6
          CONSUM = CONSUM + CONSUN*1.6
          CISUM = CISUM + CISUN*1.6
        ELSE
          PGSUM = PGSUM + PGSUN
          CONSUM = CONSUM + CONSUN
          CISUM = CISUM + CISUN
        ENDIF
      ENDDO

      PGSL   = PGSUM / 3.6
      CONDSL = CONSUM / 3.6
      CISL   = CISUM / 3.6

C     Compute photosynthesis and leaf CO2 conductance for shaded leaves

      CALL PGLEAF(
     &  CO2HR, LFMXSH, PARSH, QEFFSH, TEMPSH,WINDHR,      !Input
     &  CCNEFF,CICAD,PGPATH,VPDSH,MEEVP,CIEFFECT,         !Input
     &  CONDSH, PGSH,CISH)                                !Output


      ENDDO 
!CSVC - need to do it in future     
!CSVC - for now soil water is not send signal to stomata 
!CSVC in the future, send this conductance increase effect under stress to impact CI
!CSVC when implment that we have to remove the impact bellow

      ENDIF !for new Gs code or ORIGINAL



C     Compute canopy photosynthesis (mmol CO2/m2/s).

      IF (STRESS) THEN
        IF (CONDSL .GT. 0.0) THEN
            IF(CSLSTR.GT.CONDSL) CSLSTR=CONDSL !CSVC ADD 07/13/2018
          PGSL = PGSL * CSLSTR/CONDSL
          CONDSL=CSLSTR
        ELSE
          PGSL = 0.0
        ENDIF
        IF (CONDSH .GT. 0.0) THEN
            IF(CSHSTR.GT.CONDSH) CSHSTR=CONDSH !CSVC ADD 07/13/2018
          PGSH = PGSH * CSHSTR/CONDSH
          CONDSH=CSHSTR
        ELSE
          PGSH = 0.0
        ENDIF
      ENDIF
      PGHR = PGSL*LAISL + PGSH*LAISH
      AGEFAC = (LAISL*AGMXSL+LAISH*AGMXSH) / XLAI

      RETURN
      END SUBROUTINE CANOPG

C=======================================================================
C  PGLFEQ, Subroutine, K.J. Boote, N.B. Pickering
C  Calculates leaf quantum efficiency and maximum photosynthesis for
C  leaf photosynthesis equation as a function of temp, CO2, leaf N,
C  and specific leaf area.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  05/01/89 KJB Written
C  12/01/90 NBP Modified
C  11/05/93 KJB Added layers for SLW and leaf N
C-----------------------------------------------------------------------
C  Called from: CANOPG
C  Calls:       CURV,TABEX
C  Notes : Standard conditions and suggested values for QEREF and LXREF.
C          LXREF is for upper sunlit leaves in the canopy.  Lower
C          leaves will have a lower rate because of less SLW and N.
C          QEREF : 30 oC, 350 �L/L CO2, 2% O2, <100 �mol/m2/S PFD
C                 0.0541 �mol/�mol (Ehleringer and Bjorkman, 1977)
C                 (converted from 30 oC, 325 �L/L CO2)
C          LXREF: 30 oC, 350 �L/L CO2, 2% O2, 2000 �mol/m2/S PFD
C                 measured at SLWREF and LNREF
C                 BEAN=28, PEANUT=28, SOYBEAN=28 �mol/m2/s
C========================================================================

      SUBROUTINE PGLFEQ(
     &  CO2HR, FNPGL, FNPGN, LMXREF, LNREF, QEREF,        !Input
     &  RNITP, SLW, SLWREF, TEMPHR, TMIN, TYPPGL,         !Input
     &  TYPPGN, XLMAXT, YLMAXT,CINTSC,                    !Input
     &  CCNEFF, CICAD, CMXSF, CQESF, PGPATH,MEEVP,        !Input
     &  AGEMXL, LFMAX, QEFF, CO2QE, AGEQE)                !Output

      USE MODULEDATA

      IMPLICIT NONE
      SAVE

      CHARACTER TYPPGN*3,TYPPGL*3,MEEVP*1
      REAL AGEMXL,AGEQE,CICA,CINT,CO2HR,CO2MAX,CO2QE,
     &  CURV,FNPGN(4),FNPGL(4),GAMST,LFMAX,LNREF,LMXREF,LXREF,
     &  O2,QEFF, QEREF,RGAS,RNITP,RT,SLW,SLWMAX,SLWREF,TABEX,TAU,
     &  TEMPHR,TEMPMX,TK,XLMAXT(6),YLMAXT(6),TMIN,CHILL,CINTSC

      CHARACTER PGPATH*2
      REAL CCNEFF, CICAD, CMXSF, CQESF

      PARAMETER (O2=210000.0,RGAS=8.314)

      REAL BETALS,PDLA,BETAMX

C     Initialization.  Convert LMXREF from mgCO2/m2/s to �mol/m2/s.

      TK = TEMPHR + 273.
      RT = RGAS * TK
      LXREF = LMXREF * 1000.0 / 44.0

C     Temperature and CO2 effects on QEFF AND LMXREF are both modeled using
C     Farquhar and Caemmerer's (1982) equation (16.60 a,b) for limiting RuBP,
C     combined with the temperature effect on the specificity factor (TAU)
C     and the compensation point in the absence of dark respiration (GAMST).
      !CHP 4/15/03 Prevent overflow
       IF (RT .GT. 1000.) THEN
         if(pgpath .eq. "C4" .or. pgpath .eq. 'c4')then
            tau = exp(-3.949 + 28990.0/RT)*CCNEFF
         else
            TAU = EXP(-3.949 + 28990.0/RT)
         end if
        GAMST = 0.5 * O2 / TAU
       ELSE
        TAU = 1E10
        GAMST = 0.0
       ENDIF

C     EFFECTS ON MAXIMUM LEAF PHOTOSYNTHESIS (LMXREF).

C     SLW effect on LMXREF assumed linear (Dornhoff and Shibles, 1970).

      SLWMAX = SLW / SLWREF

C     Temperature and non-saturating CO2.

C     For the computation of LMXREF, Ci/Ca = 0.7 for CO2=350 �L/L.  The factor
C     7.179 scales CO2MAX to 1.0 at 30 oC and 350 �L/L CO2.

C     CICA = 0.4+0.6*EXP(-0.002*CO2HR)

        IF (MEEVP .EQ. 'Z') THEN
        CINT = MAX(CINTSC,GAMST) !Why summing + (1.0-CICA)*GAMST??
        ELSE
            IF (PGPATH .EQ. "C4" .OR. PGPATH .EQ. "c4") THEN
            CICA = CICAD
            CINT = CICA*CO2HR + (1.0-CICA)*GAMST
            CINT = MAX(CINT,GAMST)
            ELSE
            CICA = 0.7
            CINT = CICA*CO2HR + (1.0-CICA)*GAMST
            CINT = MAX(CINT,GAMST)
            ENDIF
        ENDIF


        IF (PGPATH .EQ. "C4" .OR. PGPATH .EQ. "c4") THEN
           CO2MAX = CMXSF * (CINT-GAMST) / (4.0*CINT+8.0*GAMST)
        ELSE
           CO2MAX = 7.179 * (CINT-GAMST) / (4.0*CINT+8.0*GAMST)
        ENDIF

C     Temperature and saturating CO2.

C     Temperature effect on LMXREF at saturating CO2 via lookup table.
C     Sawtooth shape i.e. linear increase to peak, then linear decrease.
C     Based on analysis of LMXREF using Tenhunen's (1976) data with QEFF
C     from Ehleringer and Bjorkman (1977).  TEMPMX scaled to 1.0 at 30 oC.

      TEMPMX = TABEX(YLMAXT,XLMAXT,TEMPHR,6)/TABEX(YLMAXT,XLMAXT,30.0,6)

C     Minimum night temp effect on Pmax next day, quadratic function
C     after Mike Bell, peanut.  ONLY the first two numbers are used.
C     KJB, 9/7/94.  OKAY TO USE NIGHT MINIMUM CANOPY TEMPERATURE LATER

      CHILL = CURV(TYPPGL,FNPGL(1),FNPGL(2),FNPGL(3),FNPGL(4),TMIN)

C     Nitrogen effects on LMXREF.  Photosynthesis is affected both by
C     plant nutrition and age.  Quadratic from (1) to (2).  AGEMXL scaled
C     to 1.0 at LNREF.

      AGEMXL = CURV(TYPPGN,FNPGN(1),FNPGN(2),FNPGN(3),FNPGN(4),RNITP) /
     &   CURV(TYPPGN,FNPGN(1),FNPGN(2),FNPGN(3),FNPGN(4),LNREF)

C     EFFECTS ON QUANTUM EFFICIENCY (QEFF).

C     Temperature and non-saturating CO2.

      IF (MEEVP .EQ. 'Z') THEN
       CINT = MAX(CO2HR,GAMST)
      ELSE
C     For the computation of QEFF, Ci/Ca = 1.0.  The factor 6.225 scales CO2QE
C     to 1.0 at 30 oC and 350 µL/L CO2.
         CINT = MAX(CO2HR,GAMST)
       ENDIF

       IF (PGPATH .EQ. "C4" .OR. PGPATH .EQ. "c4") THEN
         CO2QE = CQESF * (CINT-GAMST) / (4.*CINT+8.*GAMST)  !CSVC - why do not have 6.225?
       ELSE
         CO2QE = 6.225 * (CINT-GAMST) / (4.*CINT+8.*GAMST)
       ENDIF

C     Nitrogen effects on QEFF.  Photosynthesis is affected both by
C     plant nutrition (small in legumes) and age.
C      AGEQE = 0.0094 + (1.0-EXP(-4.4*AGEMAX))
C     7/17/94.  KJB INCREASED EXTENT OF N EFFECT ON QEFF.  COEFFICIENT
C     CHANGED FROM -4.4 TO -3.0,  NOW 0.97 AT AGEMAX=0.75, 0.819 AT
C     0.50, AND 0.560 AT 0.25.  ALSO, NORMALIZED.  THE 0.0094 VALUE
C     NOW KEEPS THE VALUE AT 0.01
C     CHANGE AGAIN 9/24/95 KJB, CHANGE FROM -2.5 TO -2.0, NOW 0.923
C     AT 0.8, 0.734 AT 0.5, 0.388 AT 0.2, 1.0 AT 1, 0.01 AT 0.
C
      AGEQE =  (0.0094 + (1.0-EXP(-2.0*AGEMXL))) /
     &  (0.0094 + (1.0-EXP(-2.0*1.0)))
      AGEQE = MIN(MAX(AGEQE,0.0),1.0)

C    25 Apr 2011 KJB,PDA,MPS added code for beta function: PDLA effects on lfmax and QE
      CALL GET('PDLABETA','BETA',BETALS)
      CALL GET('PDLABETA','PDLA',PDLA)
      BETAMX = (1.0-PDLA/100.)**BETALS

C     Calculate QEFF and LFMAX at ambient conditions.

      QEFF = QEREF * CO2QE * AGEQE * BETAMX
      LFMAX = LXREF * SLWMAX * TEMPMX * AGEMXL * CO2MAX * CHILL * BETAMX

      RETURN
      END SUBROUTINE PGLFEQ

C=======================================================================
C  PGLEAF, Subroutine, K.J.Boote, J.W.Jones, G.Hoogenboom
C  Calculate instantaneous leaf photosynthesis as a function of PAR
C  and leaf characteristics (�mol/m2/s).  Leaf conductance calculated
C  as a function of net photosynthesis and Ci/Ca ratio (cm/s)
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  05/01/89 GH  Written
C  12/10/90 NBP Modified to calculated leaf conductance to H2O
C  11/23/93 NBP Modified for layer input of SLW
C-----------------------------------------------------------------------
C  Called from: CANOPG
C  Calls:
C=======================================================================

      SUBROUTINE PGLEAF(
     &  CO2HR, LFMAX, PARLF, QEFF, TEMPHR,WINDHR,         !Input
     &  CCNEFF, CICAD, PGPATH,VPDATM,MEEVP,CIEFFECT,      !Input
     &  CONDLF, PGLF,CINT)                                !Output

      IMPLICIT NONE
      SAVE

      REAL A,B,C,CICA,CINT,CO2HR,CCO2LF,CONDLF,CVTURE,GAMST,LFMAX,QEFF,
     &  PARLF,PATM,PGLF,PNLF,RGAS,RT,TAU,TEMPHR,WINDHR
     
       REAL VPDATM,gsmin,VPDSLP,VPDMIN,gbco2

      CHARACTER PGPATH*2,MEEVP*1
      REAL CCNEFF, CICAD,ASLP,CSHR,VPDFACTOR

      INTEGER CIEFFECT
      PARAMETER (CVTURE=0.8, PATM=101300.0, RGAS=8.314)

C     Initialization.


      RT = RGAS * (TEMPHR+273.0)

C     Calculate leaf photosynthesis (�mol CO2/m2/s) using a non-rectangular
C     hyperbola (Rabinowitch, 1951; Lommen et al, 1971; Evans and Farquhar,
C     Norman and Arkebauer, Gutschick, In: Boote and Loomis, 1991)

      A = CVTURE
      B = (QEFF*PARLF) + LFMAX
      C = QEFF * PARLF * LFMAX
!     CHP Added checks for floating underflow 1/16/03
!      IF (LFMAX .GT. 0.0) THEN
      IF (LFMAX .GT. 0.0 .AND. (QEFF*PARLF/LFMAX) .LT. 20.) THEN
C       PGLF = (B - SQRT(B**2-4.*A*C)) / (2.*A)
        PGLF = LFMAX * (1.0 - EXP(-QEFF*PARLF/LFMAX))
      ELSE
        PGLF = MAX(LFMAX, 0.0)
      ENDIF
      PNLF = PGLF

C     Calculate leaf CO2 conductance (mol/m2/s) using assumption of constant
C     CI/CA ratio.  Compensation point (GAMST) is temperature dependent.
C     Leaf respiration neglected so PNLF=PGLF.

      !CHP 4/15/03 Prevent overflow
      IF (RT .GT. 1000.) THEN
         IF (PGPATH .EQ. "C4" .OR. PGPATH .EQ. "c4") THEN
            TAU = EXP(-3.9489 + 28990.0/RT) * CCNEFF
         ELSE
            TAU = EXP(-3.9489 + 28990.0/RT)
         ENDIF
        GAMST = 1.0E6 * 0.5 * 0.21 / TAU
      ELSE
        TAU = 1E10
        GAMST = 0.0
      ENDIF


C     CICA = 0.4+0.6*EXP(-0.002*CO2HR)
CSVC - the CO2 concentration that goes into conductance is the CS, CO2 at the leaf surface
        if (pgpath .eq. 'C4' .or. pgpath .eq. 'c4') then
           cica=cicad
        else
           CICA = 0.7
        end if
        CINT = CICA*CO2HR + (1.0-CICA)*GAMST
CSVC - in future implement equation to compute CS, for now assume it is equal to CA
      CINT = MAX(CINT,GAMST)


       IF (MEEVP .EQ. 'Z') THEN


C       gbCO2 from Pollard et al. (1995), eq. A39,  l = 0.07, assuming Wind Speed at leaf ~ 0.5 of  WINDHR
        gbco2 = MIN(10., MAX(0.1, (0.01* SQRT((WINDHR/2)/0.07)) *
     &           (1 / 0.029) )) 
C Leaf boundary layer co2 concentration
        CSHR = CO2HR - (PNLF/ gbco2)

C CSVC - when use gsmin equal to 0.01 model gets instabel for some experiments
C CSVC - we need more investigations about model instability when resistences goes high!!!
        gsmin  = 0.05   !KJB, please take these to .SPE file
        VPDSLP = -0.32  !KJB, please take these to .SPE file
        VPDMIN = 0.5    !KJB, please take these to .SPE file
        ASLP = 7.7      !KJB, please take these to .SPE file

       IF (VPDATM.GT.VPDMIN) THEN
       VPDFACTOR=MAX(0.3,(1+VPDSLP*(VPDATM-VPDMIN)))

C                    !Ball–Berry–Leuning (BBL) original
        CCO2LF = MAX(ASLP*PNLF*VPDFACTOR /(CSHR-GAMST), gsmin )

       ELSE
        CCO2LF = MAX(ASLP*PNLF /( (CSHR-GAMST) ), gsmin )
       ENDIF

CSVC - Ball–Berry–Leuning ORIGINAL
c        CCO2LF = MAX(ASLP*PNLF /( (CSHR-GAMST)*(1+VPDATM/3.0) ), gsmin )


!See Folley et al. (1996) or Sperry et al. (2016)
! Here as we are using PNLF in micro mol and CCO2LF in mol we can use CO2 in ppm 
! (if both PNLF and CCO2LF wer in the unit CINT and CSHR should be in mol/mol, Foley approach)
        CINT = MAX( CSHR - (1.6*PNLF/ CCO2LF), 1.05*GAMST)

c       print*,'PGLEAF ',CIEFFECT,CO2HR,CSHR,CINT,CINT/CO2HR,PNLF,
c     & gbco2,CCO2LF,(1.6*PNLF/ CCO2LF)

       IF(CIEFFECT.eq.2) WRITE(883, 3018)CO2HR,CINT,CINT/CO2HR,PNLF,
     &                   CCO2LF,VPDATM,(1+VPDSLP*(VPDATM-VPDMIN))
3018      FORMAT (6(F7.3,';'),F7.3)   

       ELSE ! if is not 'Z' delet next line      
       CCO2LF = MAX(PNLF/(CO2HR-CINT),0.0)  !(ORIGINAL)
       ENDIF

c       if(CIEFFECT.eq.2.and.CCO2LF.eq.0.0.and.PNLF.gt.1)  
c     &  print*,PNLF,CCO2LF,VPDFACTOR,(CSHR-GAMST)


C     Convert units from mol/m2/s CO2 to m/s H20.

CSVC       CONDLF = 1.6 * CCO2LF * RT / PATM   !ORIGINAL 
       CONDLF = CCO2LF* 1/(44.6*(273/(273+TEMPHR))*(PATM/101300))


      RETURN
      END SUBROUTINE PGLEAF



C=======================================================================
C  PGLFEQ, Subroutine, K.J. Boote, N.B. Pickering
C  Calculates leaf quantum efficiency and maximum photosynthesis for
C  leaf photosynthesis equation as a function of temp, CO2, leaf N,
C  and specific leaf area.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  05/01/89 KJB Written
C  12/01/90 NBP Modified
C  11/05/93 KJB Added layers for SLW and leaf N
C-----------------------------------------------------------------------
C  Called from: CANOPG
C  Calls:       CURV,TABEX
C  Notes : Standard conditions and suggested values for QEREF and LXREF.
C          LXREF is for upper sunlit leaves in the canopy.  Lower
C          leaves will have a lower rate because of less SLW and N.
C          QEREF : 30 oC, 350 �L/L CO2, 2% O2, <100 �mol/m2/S PFD
C                 0.0541 �mol/�mol (Ehleringer and Bjorkman, 1977)
C                 (converted from 30 oC, 325 �L/L CO2)
C          LXREF: 30 oC, 350 �L/L CO2, 2% O2, 2000 �mol/m2/S PFD
C                 measured at SLWREF and LNREF
C                 BEAN=28, PEANUT=28, SOYBEAN=28 �mol/m2/s
C========================================================================

      SUBROUTINE PGLFEQ_ORIGINAL(
     &  CO2HR, FNPGL, FNPGN, LMXREF, LNREF, QEREF,        !Input
     &  RNITP, SLW, SLWREF, TEMPHR, TMIN, TYPPGL,         !Input
     &  TYPPGN, XLMAXT, YLMAXT,                           !Input
     &  AGEMXL, LFMAX, QEFF,                              !Output
     &  CCNEFF, CICAD, CMXSF, CQESF, PGPATH,              !Input
     &  CO2QE, AGEQE)                                     !Output

      USE MODULEDATA

      IMPLICIT NONE
      SAVE

      CHARACTER TYPPGN*3,TYPPGL*3
      REAL AGEMXL,AGEQE,CICA,CINT,CO2HR,CO2MAX,CO2QE,
     &  CURV,FNPGN(4),FNPGL(4),GAMST,LFMAX,LNREF,LMXREF,LXREF,
     &  O2,QEFF, QEREF,RGAS,RNITP,RT,SLW,SLWMAX,SLWREF,TABEX,TAU,
     &  TEMPHR,TEMPMX,TK,XLMAXT(6),YLMAXT(6),TMIN,CHILL

      CHARACTER PGPATH*2
      REAL CCNEFF, CICAD, CMXSF, CQESF

      PARAMETER (O2=210000.0,RGAS=8.314)

      REAL BETALS,PDLA,BETAMX

C     Initialization.  Convert LMXREF from mgCO2/m2/s to �mol/m2/s.

      TK = TEMPHR + 273.
      RT = RGAS * TK
      LXREF = LMXREF * 1000.0 / 44.0

C     Temperature and CO2 effects on QEFF AND LMXREF are both modeled using
C     Farquhar and Caemmerer's (1982) equation (16.60 a,b) for limiting RuBP,
C     combined with the temperature effect on the specificity factor (TAU)
C     and the compensation point in the absence of dark respiration (GAMST).
      !CHP 4/15/03 Prevent overflow
      IF (RT .GT. 1000.) THEN
         if(pgpath .eq. "C4" .or. pgpath .eq. 'c4')then
            tau = exp(-3.949 + 28990.0/RT)*CCNEFF
         else
            TAU = EXP(-3.949 + 28990.0/RT)
         end if
        GAMST = 0.5 * O2 / TAU
      ELSE
        TAU = 1E10
        GAMST = 0.0
      ENDIF

C     EFFECTS ON MAXIMUM LEAF PHOTOSYNTHESIS (LMXREF).

C     SLW effect on LMXREF assumed linear (Dornhoff and Shibles, 1970).

      SLWMAX = SLW / SLWREF

C     Temperature and non-saturating CO2.

C     For the computation of LMXREF, Ci/Ca = 0.7 for CO2=350 �L/L.  The factor
C     7.179 scales CO2MAX to 1.0 at 30 oC and 350 �L/L CO2.

C     CICA = 0.4+0.6*EXP(-0.002*CO2HR)
        IF (PGPATH .EQ. "C4" .OR. PGPATH .EQ. "c4") THEN
           CICA = CICAD
           CINT = CICA*CO2HR + (1.0-CICA)*GAMST
           CINT = MAX(CINT,GAMST)
           CO2MAX = CMXSF * (CINT-GAMST) / (4.0*CINT+8.0*GAMST)
        ELSE
           CICA = 0.7
           CINT = CICA*CO2HR + (1.0-CICA)*GAMST
           CINT = MAX(CINT,GAMST)
           CO2MAX = 7.179 * (CINT-GAMST) / (4.0*CINT+8.0*GAMST)
        ENDIF

C     Temperature and saturating CO2.

C     Temperature effect on LMXREF at saturating CO2 via lookup table.
C     Sawtooth shape i.e. linear increase to peak, then linear decrease.
C     Based on analysis of LMXREF using Tenhunen's (1976) data with QEFF
C     from Ehleringer and Bjorkman (1977).  TEMPMX scaled to 1.0 at 30 oC.

      TEMPMX = TABEX(YLMAXT,XLMAXT,TEMPHR,6)/TABEX(YLMAXT,XLMAXT,30.0,6)

C     Minimum night temp effect on Pmax next day, quadratic function
C     after Mike Bell, peanut.  ONLY the first two numbers are used.
C     KJB, 9/7/94.  OKAY TO USE NIGHT MINIMUM CANOPY TEMPERATURE LATER

      CHILL = CURV(TYPPGL,FNPGL(1),FNPGL(2),FNPGL(3),FNPGL(4),TMIN)

C     Nitrogen effects on LMXREF.  Photosynthesis is affected both by
C     plant nutrition and age.  Quadratic from (1) to (2).  AGEMXL scaled
C     to 1.0 at LNREF.

      AGEMXL = CURV(TYPPGN,FNPGN(1),FNPGN(2),FNPGN(3),FNPGN(4),RNITP) /
     &   CURV(TYPPGN,FNPGN(1),FNPGN(2),FNPGN(3),FNPGN(4),LNREF)

C     EFFECTS ON QUANTUM EFFICIENCY (QEFF).

C     Temperature and non-saturating CO2.

C     For the computation of QEFF, Ci/Ca = 1.0.  The factor 6.225 scales CO2QE
C     to 1.0 at 30 oC and 350 �L/L CO2.

      CINT = MAX(CO2HR,GAMST)
      IF (PGPATH .EQ. "C4" .OR. PGPATH .EQ. "c4") THEN
         CO2QE = CQESF * (CINT-GAMST) / (4.*CINT+8.*GAMST)
      ELSE
         CO2QE = 6.225 * (CINT-GAMST) / (4.*CINT+8.*GAMST)
      ENDIF

C     Nitrogen effects on QEFF.  Photosynthesis is affected both by
C     plant nutrition (small in legumes) and age.
C      AGEQE = 0.0094 + (1.0-EXP(-4.4*AGEMAX))
C     7/17/94.  KJB INCREASED EXTENT OF N EFFECT ON QEFF.  COEFFICIENT
C     CHANGED FROM -4.4 TO -3.0,  NOW 0.97 AT AGEMAX=0.75, 0.819 AT
C     0.50, AND 0.560 AT 0.25.  ALSO, NORMALIZED.  THE 0.0094 VALUE
C     NOW KEEPS THE VALUE AT 0.01
C     CHANGE AGAIN 9/24/95 KJB, CHANGE FROM -2.5 TO -2.0, NOW 0.923
C     AT 0.8, 0.734 AT 0.5, 0.388 AT 0.2, 1.0 AT 1, 0.01 AT 0.
C
      AGEQE =  (0.0094 + (1.0-EXP(-2.0*AGEMXL))) /
     &  (0.0094 + (1.0-EXP(-2.0*1.0)))
      AGEQE = MIN(MAX(AGEQE,0.0),1.0)

C    25 Apr 2011 KJB,PDA,MPS added code for beta function: PDLA effects on lfmax and QE
      CALL GET('PDLABETA','BETA',BETALS)
      CALL GET('PDLABETA','PDLA',PDLA)
      BETAMX = (1.0-PDLA/100.)**BETALS

C     Calculate QEFF and LFMAX at ambient conditions.

      QEFF = QEREF * CO2QE * AGEQE * BETAMX
      LFMAX = LXREF * SLWMAX * TEMPMX * AGEMXL * CO2MAX * CHILL * BETAMX

      RETURN
      END SUBROUTINE PGLFEQ_ORIGINAL

C=======================================================================
C  PGLEAF, Subroutine, K.J.Boote, J.W.Jones, G.Hoogenboom
C  Calculate instantaneous leaf photosynthesis as a function of PAR
C  and leaf characteristics (�mol/m2/s).  Leaf conductance calculated
C  as a function of net photosynthesis and Ci/Ca ratio (cm/s)
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  05/01/89 GH  Written
C  12/10/90 NBP Modified to calculated leaf conductance to H2O
C  11/23/93 NBP Modified for layer input of SLW
C-----------------------------------------------------------------------
C  Called from: CANOPG
C  Calls:
C=======================================================================

      SUBROUTINE PGLEAF_ORIGINAL(
     &  CO2HR, LFMAX, PARLF, QEFF, TEMPHR,                !Input
     &  CONDLF, PGLF,                                     !Output
     &  CCNEFF, CICAD, PGPATH,VPDATM)                            !Input

      IMPLICIT NONE
      SAVE

      REAL A,B,C,CICA,CINT,CO2HR,CCO2LF,CONDLF,CVTURE,GAMST,LFMAX,QEFF,
     &  PARLF,PATM,PGLF,PNLF,RGAS,RT,TAU,TEMPHR,VPDATM

      CHARACTER PGPATH*2
      REAL CCNEFF, CICAD

      PARAMETER (CVTURE=0.8, PATM=101300.0, RGAS=8.314)

C     Initialization.

      RT = RGAS * (TEMPHR+273.0)

C     Calculate leaf photosynthesis (�mol CO2/m2/s) using a non-rectangular
C     hyperbola (Rabinowitch, 1951; Lommen et al, 1971; Evans and Farquhar,
C     Norman and Arkebauer, Gutschick, In: Boote and Loomis, 1991)

      A = CVTURE
      B = (QEFF*PARLF) + LFMAX
      C = QEFF * PARLF * LFMAX
!     CHP Added checks for floating underflow 1/16/03
!      IF (LFMAX .GT. 0.0) THEN
      IF (LFMAX .GT. 0.0 .AND. (QEFF*PARLF/LFMAX) .LT. 20.) THEN
C       PGLF = (B - SQRT(B**2-4.*A*C)) / (2.*A)
        PGLF = LFMAX * (1.0 - EXP(-QEFF*PARLF/LFMAX))
      ELSE
        PGLF = MAX(LFMAX, 0.0)
      ENDIF
      PNLF = PGLF

C     Calculate leaf CO2 conductance (mol/m2/s) using assumption of constant
C     CI/CA ratio.  Compensation point (GAMST) is temperature dependent.
C     Leaf respiration neglected so PNLF=PGLF.

      !CHP 4/15/03 Prevent overflow
      IF (RT .GT. 1000.) THEN
         IF (PGPATH .EQ. "C4" .OR. PGPATH .EQ. "c4") THEN
            TAU = EXP(-3.9489 + 28990.0/RT) * CCNEFF
         ELSE
            TAU = EXP(-3.9489 + 28990.0/RT)
         ENDIF
        GAMST = 1.0E6 * 0.5 * 0.21 / TAU
      ELSE
        TAU = 1E10
        GAMST = 0.0
      ENDIF


C     CICA = 0.4+0.6*EXP(-0.002*CO2HR)
      if (pgpath .eq. 'C4' .or. pgpath .eq. 'c4') then
         cica=cicad
      else
         CICA = 0.7
      end if
      CINT = CICA*CO2HR + (1.0-CICA)*GAMST
      CCO2LF = MAX(PNLF/(CO2HR-CINT),0.0)

        WRITE(883, 3018)CO2HR,CINT,CINT/CO2HR,PNLF,
     &                   CCO2LF,VPDATM,0.0

3018      FORMAT (6(F7.3,';'),F7.3)    

C     Convert units from mol/m2/s CO2 to m/s H20.

      CONDLF = 1.6 * CCO2LF * RT / PATM

      RETURN
      END SUBROUTINE PGLEAF_ORIGINAL


C=======================================================================
C  CANPET, Subroutine, N.B. Pickering
C  Computes instantaneous canopy evapotranspiration (mm/h) and
C  surface temperatures of soil and shaded and sunlit leaves (C).
C  Soil temperatures also computed.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  12/12/90 NBP Written
C  04/21/94 NBP Check in CANPET prevents small trans. amounts for LAI=0.
C-----------------------------------------------------------------------
C  Called from: ETPHR
C  Calls:       ETRES,ETSOLV,RADB,VPSAT
C=======================================================================

      SUBROUTINE CANPET(
     &  CANHT, CEC, CEN, CLOUDS, CONDSH, CONDSL,          !Input
     &  DLAYR2, FRACSH, FRSHV, KDIRBL, LAISH,             !Input
     &  LAISHV, LAISL, LAISLV, LWIDTH, RABS,              !Input
     &  RCUTIC, REFHT, RHUMHR, STCOND, TAIRHR,            !Input
     &  WINDHR, DIFPR, STCONDSS,SHCAP,NLAYR,                    !Input
     &  EHR, RA, TCAN, THR, TSHR, TSURF, USTAR,           !Output
     &  RB, RSURF, RNET,EMISAV,TK4CAN,TK4SKY,             !Output
     &  G, LH, LHEAT, RSSH, RSSL, RSSS, SH, SHEAT,        !Output
C        G, LH, LHEAT, RSSH, RSSL, RSSS, SH, SHEAT
C         RB, RSURF RNET output added DEC2014 by Bruce Kimball
     &    RBSH, RBSL, RBSS)                               !Output
C       added by BAK on 10DEC15

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types,
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      INTEGER I,NLAYR
      REAL CANHT,CONDSH,CONDSL,CEC,CEN,DLAYR1,DLAYR2(NL),
     &  EAIRHR,ECAN,ESATHR,EHR,ETHR,FRACSH,FRSHV,G,KDIRBL,LAISH,
     &  LAISL,LH,LHEAT(3,1),LHVAP,LWIDTH,PATM,PSYCON,RA,REFHT,RHUMHR,
     &  RCUTIC,RL(3,3),RABS(3),RNTOT,RNET(3,1),RS(3,3),SHAIR,STCND1,
     &  STCOND(NL),TAIRHR,TCAN,THR,TSHR1,TSHR(NL),SHCAP(NL),TSURF(3,1),
     &  VHCAIR,
     &  VPD(3,1),VPSAT,WINDHR,CLOUDS,DAIR,DAIRD,DVAPOR,Q,SH,SHEAT(3,1),
     &  SHAIRD,TK,MWATER,RGAS,MAIR,LAISHV,LAISLV,RADBK(3),
     &  USTAR,XLAI,ZERO,DIFPR,EMISAV,TK4CAN,TK4SKY,STCONDSS

      REAL RB(3), RSURF(3),RSSH,RSSL,RSSS,RBSH,RBSL,RBSS
C         RB, RSURF RSSH RSSL RSSS added DEC2014 by Bruce Kimball
C         RBSH,RBSL,RBSS added by BAK on 10DEC15

      PARAMETER (RGAS=8.314,MWATER=0.01802,MAIR=0.02897,PATM=101300.0,
     &  SHAIRD=1005.0, ZERO=1.0E-6)

C     Initialize.

      XLAI = LAISH + LAISL
      STCND1 = STCOND(1)
      TSHR1 = TSHR(1)
      DLAYR1 = DLAYR2(1) / 100.0

C     Calculate air/water properties as a function of temperature.

      ESATHR = VPSAT(TAIRHR)                              ! Pa
      EAIRHR = ESATHR * RHUMHR / 100.0                    ! Pa
      TK = TAIRHR + 273.0                                 ! K
      DVAPOR = MWATER * EAIRHR / (RGAS * TK)              ! kg/m3
      DAIRD = MAIR * (PATM-EAIRHR) / (RGAS * TK)          ! kg/m3
      DAIR = DVAPOR + DAIRD                               ! kg/m3
      Q = DVAPOR / DAIR
      SHAIR = SHAIRD * (1.0+0.84*Q)                       ! J/kg/K
      VHCAIR = DAIR * SHAIR                               ! J/m3/K
      LHVAP = (2501.0-2.373*TAIRHR) * 1000.0              ! J/kg
      PSYCON = SHAIR * PATM / (0.622*LHVAP)               ! Pa/K

C     Create vpd and resistance matrices.

      DO I=1,3
        VPD(I,1) = ESATHR - EAIRHR
      ENDDO
      CALL ETRES(
     &  CANHT, CEC, CEN, CONDSH, CONDSL, FRACSH, FRSHV,   !Input
     &  KDIRBL, LAISH, LAISL, LWIDTH, RCUTIC, REFHT,      !Input
     &  TAIRHR, TCAN, WINDHR,VHCAIR,TSURF,DIFPR,          !Input
     &  RA, RL, RS, USTAR,                                !Output
     &  RB,RSURF)

C       Obtain the resistances of sunlit, shaded leaves and
C         soil surface. Added by BAK on 18MAR15

C     Calculate NET total by subtracting net (back) longwave radiation.
       CALL RADB(
     &  CLOUDS, EAIRHR, FRSHV, LAISHV,                    !Input
     &  LAISLV, TAIRHR, TCAN, TSHR1 ,DIFPR,                     !Input
     &  RADBK,EMISAV,TK4CAN,TK4SKY)                       !Output
      RNET(1,1) = RABS(1) - RADBK(1)
      RNET(2,1) = RABS(2) - RADBK(2)
      RNET(3,1) = RABS(3) - RADBK(3)
      RNTOT = RNET(1,1) + RNET(2,1) + RNET(3,1)

C     Solve 3-zone model for ET and E (mm/h).

      CALL ETSOLV2(
     &  DLAYR1, EAIRHR, PSYCON, RL, RNET, RS, STCND1,     !Input
     &  TAIRHR, TSHR1, VHCAIR, VPD,STCONDSS,              !Input
     &  ECAN, G, LH, LHEAT, SH, SHEAT, TCAN, TSURF)       !Output

      ETHR = LH / LHVAP * 3600.0
      EHR = LHEAT(3,1) / LHVAP * 3600.0
      IF (XLAI .LE. ZERO) THEN
        TSURF(1,1) = 0.0
        TSURF(2,1) = 0.0
        THR = 0.0
        EHR = ETHR
      ELSE
        THR = ETHR - EHR
      ENDIF
      RETURN
      END SUBROUTINE CANPET

C========================================================================
C  ETRES, Subroutine, N.B. Pickering
C  Computes resistances to latent and sensible heat and inserts them
C  into matrices for ETSOLV.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  12/12/90 NBP Written
C------------------------------------------------------------------------
C  Called from: CANPET
C  Calls:       RESBLR
C========================================================================

      SUBROUTINE ETRES(
     &  CANHT, CEC, CEN, CONDSH, CONDSL, FRACSH, FRSHV,   !Input
     &  KDIRBL, LAISH, LAISL, LWIDTH, RCUTIC, REFHT,      !Input
     &  TAIRHR, TCAN, WINDHR,VHCAIR,TSURF,DIFPR,                      !Input
     &  RA, RL, RS, USTAR, RB, RSURF)               !Output
C        added RB and RSURF to output on 1DEC2014 by Bruce Kimball

      IMPLICIT NONE
      SAVE

      INTEGER I,J
      REAL CANHT,CEC,CEN,CONDSH,CONDSL,FRACSH,FRSHV,KDIRBL,
     &  LAISH,LAISL,LWIDTH,RCUTIC,RA,RB(3),REFHT,RL(3,3),
     &  RMAX,RS(3,3),RSSH,RSSL,RSSS,RSURF(3),TAIRHR,TCAN,
     &  WINDHR,XLAI,USTAR,VHCAIR,TSURF(3,1),DIFPR
      PARAMETER (RMAX=50000)

C     Initialization.

      XLAI = LAISH + LAISL

C     Calculate canopy and soil boundary layer resistances.

      CALL RESBLR(
     &CANHT, FRACSH, FRSHV, KDIRBL, LAISH, LAISL,DIFPR,       !Input
     &LWIDTH,REFHT,TAIRHR,TCAN,WINDHR,VHCAIR,TSURF,     !Input
     &  RA, RB, USTAR)                                   !Output

C     Calculate leaf surface resistances.

      IF (XLAI .GT. 0.0) THEN
        IF (CONDSL .GT. 0.0) THEN
          RSSL = 1.0/(CONDSL*LAISL) !CSVC - RB(1) !Total boundary resistence is 'RSSL + RB(1)'
          RSSL = MAX(RSSL,1.0)
        ELSE
          RSSL = MAX(RCUTIC / LAISL,1.)
        ENDIF
        IF (CONDSH .GT. 0.0) THEN
          RSSH = 1.0/(CONDSH*LAISH) !CSVC - RB(2) !Total boundary resistence is 'RSSL + RB(2)'
          RSSH = MAX(RSSH,1.)
        ELSE
          RSSH = MAX(RCUTIC/LAISL,1.)
        ENDIF
      ELSE
C       For XLAI=0.0 set RSURF = 0.0  RBLYR = RMAX in RESBLR, so
C       this means RLEAF = RMAX for both heat and vapor.
        RSSL = 0.0
        RSSH = 0.0
      ENDIF
      RSURF(1) = MIN(RSSL,RMAX)
      RSURF(2) = MIN(RSSH,RMAX)

C     Calculate soil surface resistance. Radiation component (a+b*rna)
C     reduced to constant 0.0117 using the average RNA from Jagtap (1976).
C     If RNET is included again, may need a RNMIN too.


      IF (CEN .LE. CEC) THEN
          RSSS = 50.0
      ELSE
c        RSSS = 100.0 + 154.0*(EXP(0.0117*(CEN-CEC)**1.37)-1.0)
         RSSS = 50.0 + 100*EXP(0.06*(CEN-CEC))
      ENDIF
      RSURF(3) = MIN(RSSS,RMAX)
C     Create resistance matrices (RS, RL).

      DO I=1,3
        DO J=1,3
          IF (I .EQ. J) THEN
            RS(I,J) = RA + RB(I)
            RL(I,J) = RA + RB(I) + RSURF(I)
          ELSE
            RS(I,J) = RA
            RL(I,J) = RA
          ENDIF
        ENDDO
      ENDDO

      RETURN
      END SUBROUTINE ETRES

C=======================================================================
C  RESBLR, Subroutine, N.B. Pickering
C  Calculates canopy and soil boundary-layer resistances to heat and
C  vapor.  Based on Choudhury and Monteith (1988).
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/09/93 NBP Written
C  04/24/94 NBP Added check to prevent -ve wind speed at top of canopy.
C-----------------------------------------------------------------------
C  Called from: ETRES
C  Calls:
C=======================================================================

      SUBROUTINE RESBLR(
     &  CANHT, FRACSH, FRSHV, KDIRBL, LAISH, LAISL,DIFPR,       !Input
     &  LWIDTH, REFHT, TAIRHR, TCAN, WINDHR,VHCAIR,TSURF,       !Input
     &  RA, RB, USTAR)                              !Output

      IMPLICIT NONE
      SAVE

      REAL CANHT,D,ETAK,ETAKMX,ETAW,ETAWMX,FRSHV,FRACSH,H,KDIRBL,
     &  KH,LAISH,LAISL,ZMD,HMD,K1,K2,LWIDTH,PSIM,PSIH,RA,VONK,DIFPR,
     &  RBLF,RBSH,RBSL,RB(3),REFHT,RMAX,TAIRHR,TCAN,TKAIR,TSURF(3,1),
     &  WINDHR,WINDSP,XLAI,Z0H,Z0M,ZS0H,ZS0M,LHZ0M,LZZ0H,LZZ0M,DT,WINDC,
     &  MO,USTAR,X,A,B,PI,RATIO,WINDH,RBSS,ZERO,VHCAIR,RASAC,STC,RIN,KS

       REAL xl,dvegl,cvegl,bvegl,bdl,exphl,expl,alog4,alogv,yl,cl,dl
       REAL RBSHb,WINDSS,RBSHd,dzl

      PARAMETER (ETAKMX=2.0, ETAWMX=3.0, PI=3.14159, RMAX=1.0E4,
     &  ZERO=1.0E-6 , VONK  = 0.4)

C     Initialization and calculation of zero plane displacement height and
C     canopy surface roughness (Brutsaert, 1982).

      XLAI = LAISH + LAISL
      WINDSP = MAX(WINDHR,0.1)
C     changed on 1Dec2014 by Bruce Kimball. 1.0 m/s is too high a
C       wind speed to be the minimum.
C     WINDSP = MAX(WINDHR,1.0)
      H = CANHT
      ZS0M = 0.03                                                  ! m
      Z0M = MAX(ZS0M,0.13*H)                                       ! m #height above d where momentum is complete absorbed
      IF (FRSHV .GT. 0.0 .AND. FRSHV.LT. 1.0) THEN
        Z0M = 4.0 * Z0M
      ENDIF
      D = 0.77 * H                                                 ! m #displacement height (where momentum is complete absorbed)
      Z0H = Z0M/5.0                                                ! m
      ZS0H = ZS0M/5.0                                              ! m
      HMD = H - D                                                  ! m
      ZMD = REFHT - D

CSVC   Reference height, taked from WTH, can't be <= Canopy height
c      IF(ZMD.LE.0.5)PRINT*,'WARNING: Canopy height < Ref. height !CSVC ADD 07/13/2018
      ZMD=max(ZMD,0.5)                                   !CSVC ADD 07/13/2018

      LZZ0M = LOG(ZMD/Z0M)
      LZZ0H = LOG(ZMD/Z0H)
      TKAIR = TAIRHR + 273.
      DT = TCAN - TAIRHR
      IF (ABS(DT) .LE. ZERO) THEN
        USTAR = 0.4 * WINDSP / LZZ0M
        RA = LZZ0H / (0.4 * USTAR)
      ENDIF
C     ETAK = ETAKMX*FRSHV
C     ETAW = ETAWMX*FRSHV
      ETAK = ETAKMX
      ETAW = ETAWMX

C     Stability correction for RA (Choudhury et al, 1986).

      MO = -0.4*9.81*DT*ZMD / (RA*TKAIR*USTAR**3)
      IF (ABS(MO) .LE. ZERO) THEN                         ! neutral
        PSIM = 0.0
        PSIH = 0.0
      ELSE IF (MO .GT. ZERO) THEN                         ! stable
        PSIM = -5.0 * MO
        PSIM = MAX(PSIM,-5.0)
        PSIH = PSIM
      ELSE IF (MO .LT. -ZERO) THEN                        ! unstable
        X = (1.0-16.0*MO)**0.25
        A = ((1.0+X)/2.0)**2
        B = (1.0+X**2)/2.0
        PSIM = ALOG(A*B) - 2.0*ATAN(X) + PI/2.0
        PSIH = 2.0 * ALOG(B)
        RATIO = PSIH / PSIM
        PSIM = MIN(PSIM,LZZ0M-0.1)
        PSIH = RATIO * PSIM
      ENDIF

C     Aerodynamic resistance.

      USTAR = 0.4 * WINDSP / (LZZ0M-PSIM)
      RA = (LZZ0H-PSIH) / (0.4 * USTAR)                   ! s/m



C SVC - compute a alternative aerodynamic resistance
C  Kimball et al. 2015

C       DT = TCAN - TAIRHR


        RIN=9.81*(-DT)*ZMD / (TKAIR*WINDSP**2)

        IF(DT.LE.0.0) THEN                !UNDER STABLE CONDITION
        STC=(1+15*RIN)*sqrt(1+5*RIN)
        ELSE                              !UNDER UNSTABLE CONDITION
        KS=(75*(0.4**2)*sqrt((ZMD+Z0M)/Z0M))/(LOG((ZMD+Z0M)/Z0M)**2)
        STC=1/( 1  - (15*RIN/(1+ KS*SQRT(-RIN))) )
        ENDIF


        RASAC=(1./WINDSP)*( ( (1./0.4) * LOG((ZMD+Z0M)/Z0M))**2 ) * STC

        IF(WINDSP.LT.1.0 .AND. ABS(DT).GT.0.01) THEN
         RASAC= MIN(RASAC,VHCAIR/( 1.52 * ABS(DT)**(1./3.)  ) )
         RASAC=MIN(RASAC,VHCAIR/2.32)
         RA=RASAC          
        ENDIF

        RA=RASAC !Uses Kimball et al. 2015 all the times
        RA=MIN(RA,100.0)


C     Canopy calculations.

      IF (XLAI .GT. 0.0) THEN

C       Calculate wind speed at the top of the canopy.

        LHZ0M = LOG(HMD/Z0M)
        WINDH = MAX(WINDSP*LHZ0M/LZZ0M,0.1)               ! m/s

C       Calculate leaf boundary layer resistances (extension of Choudhury
C       and Montieth, 1988).

        RBLF = ETAW * SQRT(LWIDTH/WINDH)
     &    / (2.0*0.01*XLAI*(1.0-EXP(-ETAW/2.0)))         ! s/m
        K1 = ETAW/2.0+KDIRBL*XLAI/FRACSH

        RBSL = SQRT(LWIDTH/WINDH) * K1 / (0.01*XLAI*(1.0-EXP(-K1)))
        RBSL = MIN(RBSL,RMAX)
        RBSH = 1.0 / (1.0/RBLF-1.0/RBSL)
        RBSH = MAX(RBSH,1.0)

c      RBSL =  RBSL
c      RBSH =  RBSL
      

C We should consider a correction based on thermal instability, applyed to all 3 RB's 


C       Calculate soil aerodynamic resistance (extension of Choudhury
C       and Montieth, 1988).

        KH = 0.32 * WINDSP * HMD / LZZ0M                           ! m/s
        K2 = EXP(-ETAK*ZS0H/H) - EXP(-ETAK*(D+Z0H)/H)
        RBSS = H*EXP(ETAK)*K2 / (ETAK*KH)                          ! s/m
        RBSS =  RBSS  ! ORIGINAL

CSVC - Implemented by Santiago Cuadra 12 MAR 2020
CSVC - Pollard et al. 1995

CSVC compute wind speed at the center of canopy

       xl =  MIN(1., 2.0 * XLAI / 4.0)  !8.0 normalization constant for lower canopy aerodynamics

        cvegl = (0.4 / MAX(0.9*H,0.01))*MAX(0.001,xl)  !Assumin that ztop-zbot is 0.9*H

        dvegl = (0.1 * MAX(0.9*H,0.01)) / MAX(0.001,xl,xl**2) !diffusion coefficient, Assumin that ztop-zbot is 0.9*H

        bvegl  = SQRT(2.0 * cvegl * (1. / dvegl))
        bdl = 0.5 * bvegl * dvegl

         dzl= MAX(0.01,(xl*H + (1.0-xl)*(ZS0M+0.01))-(xl*0.1*H + 
     &         (1.0-xl)*(ZS0M+0.01)))

         exphl = exp (0.5*bvegl*dzl) !Assumin that ztop-zbot is 0.9*H
         expl  = exphl**2

          alog4 = LOG(MAX(0.1*H,1.1*ZS0M))   !dlog(z4, ZS0M) 
          alogv = LOG(ZS0M)
           xl = ((alog4-alogv)/VONK)**2 * bdl
c
        yl = ((xl+1)*expl + (xl-1)*(1./expl))
     >        / ((xl+1)*expl - (xl-1)*(1./expl))
c

c        taul = (ua(i) * VONK/(aloga(i)-alogu(i)))**2 * stramu(i) !taul = USTAR**2

        cl = 0.5 * (USTAR**2) * (yl+1)/bdl
        dl = 0.5 * (USTAR**2) * (yl-1)/bdl
       WINDC =  MAX(0.01, SQRT(MAX(0.0, (cl/exphl+dl*exphl) )))  !Wind at the center of the Canopy


       RBSL = 1./ (0.01* SQRT((0.5*(WINDHR + WINDC))/0.07)) !boundary considering wind at top of canopy

       RBSHb = 1./ (0.01* SQRT(WINDC/0.07)) !boundary considering wind at top of canopy
       RBSHd = 1./(dvegl*0.5*(WINDHR+WINDC)/(H*0.5))

       RBSH = RBSHb + RBSHd


       WINDSS  = MAX(0.01, SQRT(MAX(0.0, (cl/expl+dl*expl) )))  !Wind at soil level

        RBSS =  DIFPR*(1./(dvegl*WINDC/H)) +
     &           (1./ (0.01* SQRT(WINDSS/ZS0M))) 

c        print*,WINDHR,WINDH,WINDC,WINDSS,DIFPR

CSVC    Applying Correction for Thermal instability

       IF(WINDSP.LT.1.0) THEN

       IF((TSURF(1,1)-TCAN).GT.0.1) 
     &  RBSL=MIN(RBSL,VHCAIR/(1.52*ABS(TSURF(1,1)-TCAN)**(1./3.))) 

       IF((TSURF(2,1)-TCAN).GT.0.1) 
     &  RBSH=MIN(RBSH,VHCAIR/(1.52*ABS(TSURF(2,1)-TCAN)**(1./3.))) 

       IF((TSURF(3,1)-TCAN).GT.0.1) 
     &  RBSS=MIN(RBSS,VHCAIR/(1.52*ABS(TSURF(3,1)-TCAN)**(1./3.))) 

        ENDIF

C     Bare soil.

      ELSE

        RBSL = RMAX
        RBSH = RMAX
        RBSS = 0.0

      ENDIF

C     Set boundary layer array.

      RB(1) = RBSL
      RB(2) = RBSH
      RB(3) = RBSS

      RETURN
      END SUBROUTINE RESBLR
C=======================================================================
C  ETSOLV, Subroutine, S. S. Jagtap, N.B. Pickering
C  Computes latent and sensible heat, and temperatures for zonal ET
C  model. Solves system of 10 equations using previous time step's
C  top-layer soil temperature.  Modified from Jagtap (1976), Jagtap
C  and Jones (1989).  Small errors in papers; equations here correct.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  ??/??/89 SSJ Written
C  01/15/91 NBP Modified
C-----------------------------------------------------------------------
C  Called from: ETPHR
C  Calls:       GAUSSJ,MATADD,MATCON,MATPRO,VPSLOP
C=======================================================================

      SUBROUTINE ETSOLV(
     &  DLAYR1, EAIRHR, PSYCON, RL, RNET, RS, STCND1,     !Input
     &  TAIRHR, TSHR1, VHCAIR, VPD,STCONDSS,              !Input
     &  ECAN, G, LH, LHEAT, SH, SHEAT, TCAN, TSURF)       !Output
  
      IMPLICIT NONE
      SAVE 

      REAL STCND1,DLAYR1,DZ1,EAIRHR,ECAN,G,PSYCON,RBLCN,SH,LH,
     &  TAIRHR,TCAN,TSHR1,VHCAIR,VP,VPSLOP,VSP,HOLD,STCONDSS
      REAL CMAT(3,1),IRL(3,3),IRS(3,3),LMAT(1,1),LHEAT(3,1),ONE(1,3),
     &  RL(3,3),RNET(3,1),RS(3,3),RSL(3,3),RSLRN(3,1),SMAT(1,1),
     &  SHEAT(3,1),TDIFF(3,1),TSURF(3,1),VHAIRS(3,3),VPD(3,1),
     &  VPIRLD(3,1),VSPIRL(3,3),XMAT(3,3),YMAT(3,1)
      DATA ONE/1.0,1.0,1.0/

C     Initialize

      VSP = VHCAIR * VPSLOP(TAIRHR) / PSYCON
      VP = VHCAIR / PSYCON
      DZ1 = DLAYR1 / 2.0

C     Invert latent and sensible heat matrices to get [iRL] and [iRS].
C     Create temporary [XMAT] = [iRSL] = [C4] then adjust for G estimated from
C     last E-zone temperature.  [XMAT] inverted to get [RSL].
C     VHAIRS and VSPIRL saved for later use.

      CALL GAUSSJ(
     &  RL,3,                                             !Input
     &  IRL)                                              !Output
      CALL GAUSSJ(
     &  RS,3,                                             !Input
     &  IRS)                                              !Output
      CALL MATCON(
     &  VSP,IRL,3,3,'*',                                  !Input
     &  VSPIRL)                                           !Output
      CALL MATCON(
     &  VHCAIR,IRS,3,3,'*',                               !Input
     &  VHAIRS)                                           !Output
      CALL MATADD(
     &  VHAIRS,VSPIRL,3,3,'+',                            !Input
     &  XMAT)                                             !Output
      XMAT(3,3) = XMAT(3,3) + STCND1/DZ1
      CALL GAUSSJ(
     &  XMAT,3,                                           !Input
     &  RSL)                                              !Output

C     Create [CMAT] = VP*[RSL]*[IRL]*[VPD].  Adjust [VPiRLD] = [C3] for G
C     estimated from last E-zone temp.  Original matrix [VPIRLD] restored
C     for later use.

      CALL MATPRO(IRL,VPD,3,3,1,YMAT)
      CALL MATCON(VP,YMAT,3,1,'*',VPIRLD)
      HOLD = VPIRLD(3,1)
      VPIRLD(3,1) = HOLD - STCND1/DZ1*(TSHR1-TAIRHR)
      CALL MATPRO(RSL,VPIRLD,3,3,1,CMAT)
      VPIRLD(3,1) = HOLD

C     Solve for TDIFF matrix.

      CALL MATPRO(RSL,RNET,3,3,1,RSLRN)
      CALL MATADD(VHAIRS,VSPIRL,3,3,'+',XMAT) ! Check this--XMAT used?
      CALL MATADD(RSLRN,CMAT,3,1,'-',TDIFF)
      CALL MATCON(TAIRHR,TDIFF,3,1,'+',TSURF)

C     Solve remaining 6 equations for latent and sensible heat fluxes and
C     sum up latent and sensible heats for 3 zones.

      CALL MATPRO(VHAIRS,TDIFF,3,3,1,SHEAT)
      CALL MATPRO(VSPIRL,TDIFF,3,3,1,YMAT)
      CALL MATADD(YMAT,VPIRLD,3,1,'+',LHEAT)
      CALL MATPRO(ONE,LHEAT,1,3,1,LMAT)
      CALL MATPRO(ONE,SHEAT,1,3,1,SMAT)
      LH = LMAT(1,1)
      SH = SMAT(1,1)

C     Calculate other values.  RBLCN is any off-diagonal element of RS.

      RBLCN = RS(1,2)
      ECAN = (LH*RBLCN/VP) + EAIRHR
      TCAN = (SH*RBLCN/VHCAIR) + TAIRHR
      G = STCND1 / DZ1 * (TSURF(3,1)-TSHR1)


      RETURN
      END SUBROUTINE ETSOLV


C=======================================================================
C  ETSOLV, Subroutine, S. S. Jagtap, N.B. Pickering
C  Computes latent and sensible heat, and temperatures for zonal ET
C  model. Solves system of 10 equations using previous time step's
C  top-layer soil temperature.  Modified from Jagtap (1976), Jagtap
C  and Jones (1989).  Small errors in papers; equations here correct.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  ??/??/89 SSJ Written
C  01/15/91 NBP Modified
C  01/07/2020 SVC Modified
C-----------------------------------------------------------------------
C  Called from: ETPHR
C  Calls:       GAUSSJ,MATADD,MATCON,MATPRO,VPSLOP
C=======================================================================
      SUBROUTINE ETSOLV2(
     &  DLAYR1, EAIRHR, PSYCON, RL, RNET, RS, STCND1,     !Input
     &  TAIRHR, TSHR1, VHCAIR, VPD,STCONDSS,              !Input
     &  ECAN, G, LH, LHEAT, SH, SHEAT, TCAN, TSURF)       !Output

      USE ModuleDefs     !Definitions of constructed variable types,
                         ! which contain control information, soil
                         ! parameters, hourly weather data.

      IMPLICIT NONE
      SAVE

        INTEGER NLAYR,I,ITER
        LOGICAL REPEAT

       REAL STCND1,DLAYR1,DZ1,EAIRHR,ECAN,G,PSYCON,RBLCN,SH,LH,
     &  TAIRHR,TCAN,TSHR1,VHCAIR,VP,VPSLOP,VSP,HOLD,RAUC,
     &  DTssaDG,DTssbDG,DG,STCONDSS,LHi,SHi,VPSAT, TSS

       REAL TSHR(NL),TSHR_ETSOLV(NL),GDIFF,GRES


       REAL CMAT(3,1),IRL(3,3),IRS(3,3),LMAT(1,1),LHEAT(3,1),ONE(1,3),
     &  RL(3,3),RNET(3,1),RMAT(3,1),RS(3,3),RSL(3,3),RSLRN(3,1),
     &    SMAT(1,1),
     &  SHEAT(3,1),TDIFF(3,1),TSURF(3,1),VHAIRS(3,3),VPD(3,1),
     &  VPIRLD(3,1),VSPIRL(3,3),XMAT(3,3),YMAT(3,1)
      DATA ONE/1.0,1.0,1.0/

       TYPE (ControlType) CONTROL


C     Initialize

      VSP = VHCAIR * VPSLOP(TAIRHR) / PSYCON
      VP = VHCAIR / PSYCON
      DZ1 = DLAYR1 / 2.0


C     Invert latent and sensible heat matrices to get [iRL] and [iRS].
C     Create temporary [XMAT] = [iRSL] = [C4] then adjust for G estimated from
C     last E-zone temperature.  [XMAT] inverted to get [RSL].
C     VHAIRS and VSPIRL saved for later use.


      CALL GAUSSJ(
     &  RL,3,                                             !Input
     &  IRL)                                              !Output (1)

      CALL GAUSSJ(
     &  RS,3,                                             !Input
     &  IRS)                                              !Output (2)
      CALL MATCON(
     &  VSP,IRL,3,3,'*',                                  !Input
     &  VSPIRL)                                           !Output (3)
      CALL MATCON(
     &  VHCAIR,IRS,3,3,'*',                               !Input
     &  VHAIRS)                                           !Output (4)
      CALL MATADD(
     &  VHAIRS,VSPIRL,3,3,'+',                            !Input
     &  XMAT)                                             !Output (5)
      XMAT(3,3) = XMAT(3,3)  !+ STCND1/DZ1 !SVC where this STCND1/DZ1 came from???
      CALL GAUSSJ(
     &  XMAT,3,                                           !Input
     &  RSL)                                              !Output (6)

C     Create [CMAT] = VP*[RSL]*[IRL]*[VPD].  Adjust [VPiRLD] = [C3] for G
C     estimated from last E-zone temp.  Original matrix [VPIRLD] restored
C     for later use.

      CALL MATPRO(IRL,VPD,3,3,1,YMAT)                     ! Output (7)
      CALL MATCON(VP,YMAT,3,1,'*',VPIRLD)                 ! Output (8)
      HOLD = VPIRLD(3,1)
  
C SVC - If the the flux from other sources do not affect the flux of each source
C SVC - The fluxes are computed between each source and the free atmosphere
c         TSURF(1,1)= (1./( VHCAIR/RS(1,1)+VSP/RL(1,1)))
c     & * (RNET(1,1)+TAIRHR*(VHCAIR/RS(1,1)+VSP/RL(1,1))
c     &       - VPD(1,1)*VP/RL(1,1) )
c      SHEAT(1,1) =  (VHCAIR/RS(1,1))*(TSURF(1,1)-TAIRHR) 
c      LHEAT(1,1) =  (VSP/RL(1,1))*(TSURF(1,1)-TAIRHR) + VPD(1,1)*VP/RL(1,1)
c         TSURF(2,1)= (1./( VHCAIR/RS(2,2)+VSP/RL(2,2)))
c     & * (RNET(2,1)+TAIRHR*(VHCAIR/RS(2,2)+VSP/RL(2,2))
c     &       - VPD(2,1)*VP/RL(2,2) )      
c      SHEAT(2,1) =  (VHCAIR/RS(2,2))*(TSURF(2,1)-TAIRHR) 
c      LHEAT(2,1) =  (VSP/RL(2,2))*(TSURF(2,1)-TAIRHR) + VPD(2,1)*VP/RL(2,2)


         TSS = (1./( VHCAIR/RS(3,3)+VSP/RL(3,3)+STCONDSS/DZ1))
     & * (RNET(3,1)+TAIRHR*(VHCAIR/RS(3,3)+VSP/RL(3,3))
     &       + TSHR1*STCONDSS/DZ1 - VPD(3,1)*VP/RL(3,3) )

      G = (TSS-TSHR1)*(STCONDSS/DZ1)

         VPIRLD(3,1) = HOLD + G ! (TSURF(3,1)-TSHR1)*(STCONDSS/DZ1)!*(1./RS(3,3))  !*(TAIRHR-TSHR1)*(STCND1/DZ1)  ! Output (9)
        
       CALL MATPRO(RSL,VPIRLD,3,3,1,CMAT)               ! Output (10) 
            VPIRLD(3,1) = HOLD

C     Solve for TDIFF matrix.
      CALL MATPRO(RSL,RNET,3,3,1,RSLRN)                  ! Output (11)
      CALL MATADD(RSLRN,CMAT,3,1,'-',TDIFF)              ! Output (12)
      CALL MATCON(TAIRHR,TDIFF,3,1,'+',TSURF)            ! Outuput (13)
        
c        TSURF(3,1) = TSS
    
C     Solve remaining 6 equations for latent and sensible heat fluxes and
C     sum up latent and sensible heats for 3 zones.

      CALL MATPRO(VHAIRS,TDIFF,3,3,1,SHEAT)             ! Output (14)
      CALL MATPRO(VSPIRL,TDIFF,3,3,1,YMAT)              ! Output (14)
      CALL MATADD(YMAT,VPIRLD,3,1,'+',LHEAT)

c      SHEAT(3,1) =  (VHCAIR/RS(3,3))*(TSURF(3,1)-TAIRHR) 
c      LHEAT(3,1) =  (VSP/RL(3,3))*(TSURF(3,1)-TAIRHR) + VPD(3,1)*VP/RL(3,3)



!C SVC - 2nd Solving system WITH the impact of leaves fluxes on TSURF(3,1)
c         TSS = (1./( VHCAIR/RS(3,3)+VSP/RL(3,3)+STCONDSS/DZ1))
c     & * (RNET(3,1)+TAIRHR*(VHCAIR/RS(3,3)+VSP/RL(3,3))
c     &       + TSHR1*STCONDSS/DZ1 - VPD(3,1)*VP/RL(3,3)  
c     &       - (SHEAT(1,1)+SHEAT(2,1))*RS(1,2)/RS(3,3) 
c     &       - (LHEAT(1,1)+LHEAT(2,1))*RS(1,2)/RL(3,3)     )
c
c      SHEAT(3,1) = (VHCAIR/RS(3,3))*(TSURF(3,1)-TAIRHR) 
c    & - (SHEAT(1,1)+SHEAT(2,1))*RS(1,2)/RS(3,3) 
c
c      LHEAT(3,1) = (VSP/RL(3,3))*(TSURF(3,1)-TAIRHR) + VPD(3,1)*VP/RL(3,3)
c    & - (LHEAT(1,1)+LHEAT(2,1))*RS(1,2)/RL(3,3) 
c
         TSS = TSURF(3,1)
      G = (TSS-TSHR1)*(STCONDSS/DZ1)

         VPIRLD(3,1) = HOLD + G                         ! Output (9)
        
       CALL MATPRO(RSL,VPIRLD,3,3,1,CMAT)               ! Output (10) 
            VPIRLD(3,1) = HOLD

C     Solve for TDIFF matrix.
      CALL MATPRO(RSL,RNET,3,3,1,RSLRN)                  ! Output (11)
      CALL MATADD(RSLRN,CMAT,3,1,'-',TDIFF)              ! Output (12)
      CALL MATCON(TAIRHR,TDIFF,3,1,'+',TSURF)            ! Outuput (13)
        
        TSURF(3,1) = TSS
    
C     Solve remaining 6 equations for latent and sensible heat fluxes and
C     sum up latent and sensible heats for 3 zones.

      CALL MATPRO(VHAIRS,TDIFF,3,3,1,SHEAT)             ! Output (14)
      CALL MATPRO(VSPIRL,TDIFF,3,3,1,YMAT)              ! Output (14)
      CALL MATADD(YMAT,VPIRLD,3,1,'+',LHEAT)
      CALL MATPRO(ONE,LHEAT,1,3,1,LMAT)
      CALL MATPRO(ONE,SHEAT,1,3,1,SMAT)
      CALL MATPRO(ONE,RNET,1,3,1,RMAT)
      LH = LMAT(1,1)
      SH = SMAT(1,1)

c      print*,RBLCN,RS(1,1),RL(1,1),TAIRHR,TCAN,TSURF(1,1),TSURF(2,1),TSURF(3,1)


C     Calculate other values.  RBLCN is any off-diagonal element of RS, i.e. RA.
      RBLCN = RS(1,2)
      ECAN = (LH*RBLCN/VP) + EAIRHR
      TCAN = (SH*RBLCN/VHCAIR) + TAIRHR
       GRES = RMAT(1,1) - LH - SH - G


       
C SVC - Equations from Agro-IBIS, neet to compute loss from Runoff and Rainfall
C SVC  fhtop(i) = heatg(i)
C SVC     >           + raing(i)*ch2o*(traing(i)-tsoi(i,1))
C SVC     >           - grunof(i)*ch2o*max(tmelt-tsoi(i,1), 0.)

      RETURN
      END SUBROUTINE ETSOLV2


C=======================================================================
C  GAUSSJ, Subroutine, Numerical Recipes, N.B. Pickering
C  Inverts N x N matrix using Gauss-Jordan elimination with full
C  pivoting (Numerical Recipes, Press et al., 1986, pg. 28).
C  The original matrix is AMAT and the inverse AINV.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  ??/??/??     Written
C  01/10/91 NBP Modified
C-----------------------------------------------------------------------
C  Called from: ETSOLV
C  Calls:
C=======================================================================

      SUBROUTINE GAUSSJ(
     &  AMAT,N,                                           !Input
     &  AINV)                                             !Output

      IMPLICIT NONE
      SAVE

      INTEGER NMAX
      PARAMETER (NMAX = 10)
      INTEGER I,ICOL,INDXC(NMAX),INDXR(NMAX),IPIV(NMAX),IROW,J,K,L,LL,N
      REAL AMAT(N,N),AINV(N,N),BIG,DUM,PIVINV,ZERO
      PARAMETER (ZERO = 1.0E-6)
      CHARACTER*78 MSG(1)

C     Initialize.

      DO J=1,N
        DO K=1,N
          AINV(J,K) = AMAT(J,K)
        ENDDO
      ENDDO
      DO J=1,N
        IPIV(J) = 0
      ENDDO

C     Main loop over columns to be reduced.

      DO I=1,N
        BIG = 0.0

C       Search for pivot (or largest) element and store position.

        DO J=1,N
          IF(IPIV(J) .NE. 1) THEN
            DO K=1,N
              IF (IPIV(K) .EQ. 0) THEN
                IF (ABS(AINV(J,K)) .GE. BIG) THEN
                  BIG = ABS(AINV(J,K))
                  IROW = J
                  ICOL = K
                ENDIF
              ELSE IF (IPIV(K) .GT. 1) THEN
                MSG(1) = "SINGULAR MATRIX IN GAUSSJ."
                CALL WARNING(1,"GAUSSJ",MSG)
                WRITE(*,*) MSG(1)
                !PAUSE 'SINGULAR MATRIX IN GAUSSJ.'
              ENDIF
            ENDDO
          ENDIF
        ENDDO
        IPIV(ICOL) = IPIV(ICOL)+1

C       Interchange rows by relabeling.  INDXC(I) = col of i'th pivot
C       element, INDXR(I) = original row where pivot element was located.

        IF (IROW .NE. ICOL) THEN
          DO L=1,N
            DUM = AINV(IROW,L)
            AINV(IROW,L) = AINV(ICOL,L)
            AINV(ICOL,L) = DUM
          ENDDO
        ENDIF

C       Divide pivot row by pivot element at (IROW,ICOL).

        INDXR(I) = IROW
        INDXC(I) = ICOL
        IF(ABS(AINV(ICOL,ICOL)).LE.ZERO) THEN
          MSG(1) = "SINGULAR MATRIX IN GAUSSJ."
          CALL WARNING(1,"GAUSSJ",MSG)
          WRITE(*,*) MSG(1)
          !PAUSE 'SINGULAR MATRIX - GAUSSJ'
        ENDIF
        PIVINV = 1.0/AINV(ICOL,ICOL)
        AINV(ICOL,ICOL) = 1.0
        DO L=1,N
          AINV(ICOL,L) = AINV(ICOL,L)*PIVINV
        ENDDO

C       Reduce pivot rows except for pivot one.

        DO LL=1,N
          IF(LL .NE. ICOL) THEN
            DUM = AINV(LL,ICOL)
            AINV(LL,ICOL) = 0.0
            DO L=1,N
              AINV(LL,L) = AINV(LL,L)-AINV(ICOL,L)*DUM
            ENDDO
          ENDIF
        ENDDO
      ENDDO

C     Unscramble AINV by interchanging col pairs in the reverse order
C     that the permutation was created.  INDXR(I)<>INDXR(I) ==> column
C     interchange needed.

      DO L=N,1,-1
        IF(INDXR(L) .NE. INDXC(L)) THEN
          DO K=1,N
            DUM = AINV(K,INDXR(L))
            AINV(K,INDXR(L)) = AINV(K,INDXC(L))
            AINV(K,INDXC(L)) = DUM
          ENDDO
        ENDIF
      ENDDO

      RETURN
      END SUBROUTINE GAUSSJ

C=======================================================================
C  MATADD, Subroutine, S.S. Jagtap
C  Matrix addition.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  ??/??/89 SSJ Written
C  01/14/91 NBP Modified
C-----------------------------------------------------------------------
C  Called from: ETSOLV
C  Calls:
C=======================================================================

      SUBROUTINE MATADD(
     &  AMAT,BMAT,NROW,NCOL,OPERND,                     !Input
     &  CMAT)                                           !Output

      IMPLICIT NONE
      SAVE

      CHARACTER*1 OPERND
      INTEGER I,J,NCOL,NROW
      REAL AMAT(NROW,NCOL),BMAT(NROW,NCOL),CMAT(NROW,NCOL)

C     Addition of two matrices.

      DO I=1,NROW
        DO J=1,NCOL
        IF (OPERND .EQ. '+') THEN
          CMAT(I,J) = AMAT(I,J) + BMAT(I,J)
        ELSE IF (OPERND .EQ. '-') THEN
          CMAT(I,J) = AMAT(I,J) - BMAT(I,J)
        ENDIF
        ENDDO
      ENDDO

      RETURN
      END SUBROUTINE MATADD

C=======================================================================
C  MATCON, Subroutine, N.B. Pickering
C  Matrix multiplication/addition of a constant (CONST).
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/14/91 NBP Written
C-----------------------------------------------------------------------
C  Called from: ETSOLV
C  Calls:
C=======================================================================

      SUBROUTINE MATCON(
     &  CONST,BMAT,NROW,NCOL,OPERND,                    !Input
     &  CMAT)                                           !Output

      IMPLICIT NONE
      SAVE

      CHARACTER*1 OPERND
      INTEGER I,J,NROW,NCOL
      REAL CONST,BMAT(NROW,NCOL),CMAT(NROW,NCOL)

C     Loop for all elements of [CMAT].

      DO I=1,NROW
        DO J=1,NCOL
          IF (OPERND .EQ. '+') THEN
            CMAT(I,J) = CONST + BMAT(I,J)
          ELSE IF (OPERND .EQ. '*') THEN
            CMAT(I,J) = CONST * BMAT(I,J)
          ENDIF
        ENDDO
      ENDDO

      RETURN
      END SUBROUTINE MATCON

C=======================================================================
C  MATPRO, Subroutine, S.S. Jagtap
C  Matrix product or multiplication.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  ??/??/89 SSJ Written
C  01/14/91 NBP Modified
C-----------------------------------------------------------------------
C  Called from: ETSOLV
C  Calls:
C=======================================================================

      SUBROUTINE MATPRO(
     &  AMAT,BMAT,NROWA,NCOM,NCOLB,                     !Input
     &  CMAT)                                           !Output

      IMPLICIT NONE
      SAVE

      INTEGER I,J,K,NROWA,NCOM,NCOLB
      REAL AMAT(NROWA,NCOM),BMAT(NCOM,NCOLB),CMAT(NROWA,NCOLB),SUM

C     Multiplication of two matrices.  Loop for all elements of [CMAT].

      DO I=1,NROWA
        DO J=1,NCOLB
          SUM = 0.0

C         Sum products for each element of [CMAT]--along [AMAT] and down [BMAT]

          DO K=1,NCOM
            SUM = SUM + AMAT(I,K) * BMAT(K,J)
          ENDDO

C         Set each element of [CMAT] equal to the sum.

          CMAT(I,J) = SUM

        ENDDO
      ENDDO

      RETURN
      END SUBROUTINE MATPRO

C=======================================================================
C  RADB, Subroutine, N.B. Pickering
C  Calculates net/back long wave or thermal radiation from leaves
C  and soil.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  12/10/90 NBP Written
C  02/09/93 NBP Modified
C-----------------------------------------------------------------------
C  Called from: CANPET
C  Calls:
C=======================================================================

      SUBROUTINE RADB(
     &  CLOUDS, EAIRHR, FRSHV, LAISHV,                    !Input
     &  LAISLV, TAIRHR, TCAN, TSHR1,DIFPR,                            !Input
     &  RADBK,EMISAV,TK4CAN,TK4SKY)                       !Output

      IMPLICIT NONE
      SAVE

      REAL CLOUDS,DELT,EMISA,EMISA0,EMISL,EMISS,EMISS0,FRSHV,
     &  LAISHV,LAISLV,RADBK(3),EAIRHR,SBZCON,TAIRHR,TKAIR,TK4SKY,
     &  TSKY,XLAI,EMISAV,RBKLF,RBACK,TCAN,TK4CAN,ZERO, TSHR1,DIFPR

      PARAMETER (SBZCON=5.675E-8, DELT=11.0, EMISL=0.97, EMISS0=0.96,
     &  ZERO=1.0E-6)

C     Initialize.  Apparent atmospheric emissivity from Brutsaert (1982)
C     and Monteith and Undsworth (1990)

      XLAI = LAISLV + LAISHV
      TKAIR = TAIRHR + 273.0

c         TK4CAN = (TCAN+273.0)**4
CSVC IMPLEMENT, BUT IT HAS SMALL EFFECT
CSVC      TK4CAN = ((FRSHV*(TCAN+273)) +((1.0-FRSHV)*(TSHR1+273)))**4
      TK4CAN = ((DIFPR*(TCAN+273)) +((1.0-DIFPR)*(TSHR1+273)))**4

      EMISA0 = 1.24 * (EAIRHR/100.0/TKAIR)**(1.0/7.0)     !EAIRHR in Pa
      EMISA = CLOUDS*(1.0-(1.0-EMISA0)*4*DELT/TKAIR) +
     &  (1.0-CLOUDS)*EMISA0
      TK4SKY = EMISA * TKAIR**4
      TSKY = TK4SKY**0.25 - 273.0

C CSVC - remove      EMISS = MIN(EMISS0 + 0.07,1.0)

C     Calculate thermal losses from sunlit leaves and soil.  Soil surface
C     assumed to have view factor of FRSHV with TEMPSH and (1.-FRSHV)
C     with sky temperature.  Energy loss from leaf is porportionally
C     weighted according to leaf area index.  NEED VIEW FACTOR FOR LEAVES!

      EMISAV = DIFPR*EMISL + (1.0-DIFPR)*EMISS0
      RBACK =  EMISAV * SBZCON * (TK4CAN-TK4SKY)

      IF (XLAI .LE. ZERO) THEN
        RADBK(1) = 0.0
        RADBK(2) = 0.0
      ELSE
        RBKLF    = DIFPR * RBACK
        RADBK(1) = 0.7 * RBKLF   !sunlit leaves are above shaded leaves
        RADBK(2) = 0.3 * RBKLF
      ENDIF
      RADBK(3) = (1.0-DIFPR) * RBACK

      RETURN
      END SUBROUTINE RADB

C=======================================================================
C  HSOILT, Subroutine, S. S. Jagtap
C  Updates soil temperatures by layer (from Hillel, 1975) using
C  soil surface temperature calculated in ETSOLV.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  ??/??/89 SSJ Written
C  01/14/91 NBP Modified
C-----------------------------------------------------------------------
C  Called from: ETPHR
C  Calls:
C=======================================================================

        SUBROUTINE HSOILT(CONTROL,H,G,
     &  DLAYR2, NLAYR, SHCAP, STCOND,STCONDSS, TA, TEMPSS,         !Input
     &  TSHR)                                             !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types,
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      INTEGER I,NLAYR,YRDOY,TRTNUM,TIMED,TIMEI,H,DAS,RUN
      REAL DT,DZ,DLAYR2(NL),INFLOW,OUTFLO,SCOND,STCOND(NL),SHCAP(NL),
     &  TA,TINCR,TSHR(NL),TEMPSS,VHCAP(NL),TIMEMIN,G,STCONDSS,GC,GCSS
CSVC----------------------------------------
      PARAMETER (TINCR=24.0/TS*3600.0)

      TYPE (ControlType) CONTROL


      YRDOY   = CONTROL % YRDOY
      DAS     = CONTROL % DAS
      TRTNUM  = CONTROL%TRTNUM
      RUN     = CONTROL%RUN

       TIMEMIN = 0.0
       OUTFLO  = 0.0

       
      DO I=1,NLAYR
         IF (I .EQ. 1) THEN
          DZ = 0.5 * DLAYR2(1)/100.0
         ELSEIF (I .EQ. NLAYR) THEN
          DZ = 0.5 * DLAYR2(I)/100.0
         ELSE
          DZ = 0.5 * (DLAYR2(I)+DLAYR2(I+1))/100.0
         ENDIF

C     Calculate volumetric heat capacity at each node.
         VHCAP(I) = TSHR(I) * SHCAP(I) * DLAYR2(I)/100.0

         SCOND = STCOND(I)
         TIMEMIN=MAX(TIMEMIN,1./ABS((SHCAP(I)*(DZ**2)/SCOND)/(3600.)))
      ENDDO


        TIMEI=INT(TIMEMIN)+2

      DO TIMED=1,TIMEI


C     Loop for flux between soil layers.  Influx at top and bottom of profile
C     passes through half the layer thickness.  Bottom of profile is assumed to be
C     a slowly-varying temperature boundary condition (TA).

  
      DO I = 1,NLAYR
        IF (I .EQ. 1) THEN
          DT = TEMPSS - TSHR(1)
          DZ = 0.5 * DLAYR2(1)/100.0
          SCOND = STCOND(I)          
 
          INFLOW = G !(DT/DZ)*(STCONDSS) !G !(DT/DZ) * (STCONDSS)
          GC   = (DT/DZ)*(SCOND)
          GCSS =(DT/DZ)*(STCONDSS)
        ELSE
          INFLOW = OUTFLO
        ENDIF
        IF (I .EQ. NLAYR) THEN
          DT = TSHR(I) - TA
          DZ = 0.5 * DLAYR2(I)/100.0
          SCOND = STCOND(I)
          OUTFLO = DT / DZ * (SCOND)
        ELSE
          DT = TSHR(I) - TSHR(I+1)
          DZ = 0.5 * (DLAYR2(I)+DLAYR2(I+1))/100.0
          SCOND = 0.5 * (STCOND(I)+STCOND(I+1))    ! need to correct, must weight the size of layers
          OUTFLO = DT / DZ * (SCOND)
        ENDIF
CSVC                WRITE (881,*)TRTNUM,YRDOY,I,TSHR(I) ,VHCAP(I),DZ**2,SCOND,
CSVC     &   (VHCAP(I)*(DZ**2)/SCOND)/60.

        VHCAP(I) = VHCAP(I) + (INFLOW-OUTFLO)*TINCR/REAL(TIMEI)
      ENDDO

C     Update soil temperatures.

      DO I = 1,NLAYR
        TSHR(I) = VHCAP(I) / (DLAYR2(I)/100.0*SHCAP(I))
      ENDDO

        ENDDO


	     WRITE (881, 3015)TRTNUM,TIMED,YRDOY,H,0.0,DLAYR2(1),DLAYR2(2),
     &   DLAYR2(3),DLAYR2(4),DLAYR2(5),DLAYR2(6),DLAYR2(7),
     &   DLAYR2(8),DLAYR2(9),DLAYR2(10),DLAYR2(11),DLAYR2(12),
     & DLAYR2(13),TEMPSS,TSHR(1),TSHR(2),TSHR(3),
     &TSHR(4),TSHR(5),TSHR(6),TSHR(7),TSHR(8),TSHR(9),TSHR(10),TSHR(11),
     &TSHR(12),TA



3015      FORMAT (2(I2,';'),I7,';', I7, 28(';', F6.1))
CSVC ----------------------


      RETURN
      END SUBROUTINE HSOILT
