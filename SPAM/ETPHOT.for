C=======================================================================
C  ETPHOT, Subroutine, N.B. Pickering
C  Computes canopy daily ET (mm/d) and gross photosynthesis (g CO2/d)
C  by integrating over hourly timesteps in the day.
C-----------------------------------------------------------------------
C  Zonal ET calculations (MEEVP = 'Z') are for research purposes only
C  and are not supported in the CSM v3.9 distribution version
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  12/05/1990     NBP Written.
C  11/05/1993 KJB Added layers for SLW and leaf N.
C  11/20/1993 NBP Error checks in SHADOW.
C  11/23/1993 NBP Removed SLW effect from INVEG, all in PGLFEQ
C  12/11/1993 NBP Made calc. of Rsoil larger, removed AWES1, and made
C                 RWUH greater i.e. less E and more T.
C  12/12/1993 NBP Added avg. soil and surface temps.--passed back.
C  01/27/1994 NBP Added hourly TCAN array and TCANAV--passed back to GPHEN.
C  02/11/1994 NBP Corrected initialization for photo. to NVEG0+1
C  03/13/1994 NBP Added soil layer conversion: 5,15.. cm to 10,10.. cm.
C                 Switched ET and PG fixed and daily init. routines.
C  04/24/1994 NBP Added TCANDY. Used DAYTIM for FRSHAV. Repl. 24 with TS.
C  03/20/1996 KJB Minimum default LAI decreased from 0.05 to 0.01.  This
C                 artificially held up canopy rate, and used
C                 in conjunction with VSSINK effects on F, gave very low
C                 SLA and messed up ability to do low density plantings.
C  02/10/1999 chp changed SWDF1 to SWFAC
C  01/10/2000 NBP Modular version
C  01/13/2000 NBP Added calls to ROOTWU and SWFACS
C  01/13/2000 NBP Added SWFAC effect on PG for MEPHO='L' and MEEVP<>'Z'
C  07/02/2000 GH  Eliminate SWFACS section
C  03/29/2001 CHP Added PLTPOP to input variable list (can be modified in
C                 plant routines).  Removed TURFAC (not used)
C  06/21/2001 GH  Add seasonal initialization section
C  09/17/2001 CHP PORMIN, RWUMX input from Plant Modules.
C  01/09/2002 CHP SWFAC calculated here.
C  06/11/2002 GH  Modified for Y2K
!  10/24/2005 CHP Put weather variables in constructed variable.
!                 Removed GETPUT_Weather subroutine.
!  01/11/2007 CHP Changed GETPUT calls to GET and PUT
!  01/10/2019 CHP Remove KRT changes introduced with pull request #201
!                 These cause major differences in some CROPGRO experiments
!                 Roll back for now, need to investigate!
!  11/14/2020 FO  ETPHOT - First part of code protections for divisions by zero
!                 and negative values.
!                 These changes were supervised by GH, KJB, SC and NBP.
!  01/15/2021 FO  ETPHOT - Second part of code protections for divisions by zero
!                 and negative values. Removed XLAI, CANHT, CANWH initializations.
C-----------------------------------------------------------------------
C  Called from: SPAM
C  Calls:       ETIND,ETINP,PGINP,PGIND,RADABS,ETPHR,ROOTWU,SOIL05,SWFACS
C========================================================================

      SUBROUTINE ETPHOT (CONTROL, ISWITCH,
     &    PORMIN, PSTRES1, RLV, RWUMX, SOILPROP, ST, SW,  !Input
     &    WEATHER, XLAI,                                  !Input
     &    EOP, EP, ES, RWU, TRWUP)                        !Output
C     &    Enoon, Tnoon, WINDN, TCANn, CSHnn, CSLnn,        !Output
C     &    LSHnn, LSLnn, ETnit, TEMnit, Enit, Tnit, WINnit,!Output
C     &    TCnit, TSRnit, CSHnit, CSLnit, LSHnit, LSLnit)  !Output
C         previous three output lines added by Bruce Kimball on 2DEC14

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types,
      USE ModuleData

      IMPLICIT NONE
      EXTERNAL ETIND, ETINP, ETPHR, OPETPHOT, OPSTEMP, PGIND, PGINP, 
     &  RADABS, ROOTWU, SOIL05, YR_DOY
      SAVE

      CHARACTER FILEIO*30,ISWWAT*1,MEEVP*1,MEPHO*1,METEMP*1,
     &  TYPPGN*3,TYPPGL*3, CROP*2
      INTEGER DAS,DYNAMIC,H,I,NELAYR,NHOUR, DOY, YRDOY, YEAR,
     &  NLAYR,NR5, LUNIO, TSV2
!         TSV2 = index for mid-day hour added by Bruce Kimball on 9JAN17
      LOGICAL DAYTIM
      REAL AGEFAC,AZIR,AZZON(TS),BETA(TS),BETN,   !AWEV1,
     &  CANHT,CANWH,CEC,CEN,CLOUDS,CO2,CO2HR,DAYKP,DAYKR,DAYPAR,
     &  DAYRAD,DLAYR(NL),DLAYR2(NL),DULE,DYABSP,DYABSR,DYINTP,
     &  DYINTR,EDAY,EHR,EOP,EP,ES,ETNOON,FNPGN(4),FNPGL(4),FRACSH,
     &  FRDFPN,FRDFRN,FRDIFP(TS),FRDIFR(TS),FRSHAV,FRSHV,
     &  HS,HOLDHT,KDIRBL,KDRBLV,HOLDLA,LAISH,LAISHV,LAISL,
     &  LAISLV,LFANGD(3),LFMXSH,LFMXSL,LL(NL),LL2(NL),LLE,LMXSHN,
     &  LMXSLN,LMXREF,LNREF,LWIDTH,NSLOPE,PALB,PARHR(TS),PARN,PARSH,
     &  PARSUN(3),PCABPD,PCABPN,PCABRD,PCABRN,PCABSP,PCABSR,PCINPD,
     &  PCINPN,PCINRD,PCINRN,PCINTP,PCINTR,PCNLSH,PCNLSL,PG,PGCO2,
     &  PGDAY,SLPF,PGHR,PGNOON,PNLSHN,PNLSLN,QEREF,RABS(3),
     &  RADHR(TS),RADN,RCUTIC,REFHT,RHUMHR(TS),RLV(NL),RNITP,ROWSPC,
     &  RWU(NL),RWUH,SALB,SCVIR,SCVP,SHCAP(NL),SLAAD,SLWREF,
     &  SLWSH,SLWSHN,
     &  SLWSL,SLWSLN,SLWSLO,SNDN,SNUP,ST(NL),STn(NL),ST2(NL),STCOND(NL),
     &  SW(NL),SW2(NL),SWFAC,SWE,SWEF,T0HR,TAIRHR(TS),TA,
     &  TCAN(TS),TCANAV,TCANDY,TDAY,TEMPN,THR,TINCR,TRWUP,
     &  TSHR(NL), TSRF(3),TSRFN(3),TSURF(3,1),HOLDWH,WINDHR(TS),
     &  XLAI, TSHRn(NL),
     &  XLMAXT(6),XSW(NL,3),YLMAXT(6),YSCOND(NL,3),YSHCAP(NL,3),TMIN
      REAL SAT(NL),TGRO(TS),TGROAV,TGRODY,TAV,TAMP
      REAL PGXX,DXR57,EXCESS,XPOD,CUMSTR,COLDSTR
!      PARAMETER (TINCR=24.0/TS)
!                TINCR is the lenfth of a time increment in hours
      REAL PHTHRS10, PLTPOP
      REAL PORMIN, RWUMX
      REAL PALBW, SALBW, SRAD, DayRatio

      REAL CONDSH, CONDSL, RA, RB(3), RSURF(3), Rnet(3,1)
      REAL Enoon, Tnoon, WINDN, TCANn, CSHnn, CSLnn,
     &    LSHnn, LSLnn, ETnit, TEMnit, Enit, Tnit, WINnit, TCnit,
     &    TSRnit(3), CSHnit, CSLnit, LSHnit, LSLnit, SRFTEMP,
     &    G, LH, LHEAT(3,1), RSSH, RSSL, RSSS, SH, SHEAT(3,1),
     &     GN, LHN, LHEATN(3), RSSHN, RSSLN, RSSSN, SHN, SHEATN(3),
     &     GMT, LHT, LHEATT(3), RSSHT, RSSLT, RSSST, SHT, SHEATT(3),
     &     RNETN(3),RNETT(3),
     &     TAnn, TAnit, TGROnn, TGROnit,
C         previous 7 lines added by Bruce Kimball on 2DEC14
     &     RBSH,RBSL,RBSS,RBSHN,RBSLN,RBSSN,RBSHT,RBSLT,RBSST
C         added by BAK on 10DEC2015

      REAL, DIMENSION(NL) :: BD, DUL, SAT2, DUL2, RLV2

      CHARACTER(len=2) PGPATH
      character(len=8) model
      REAL CCNEFF, CICAD, CMXSF, CQESF
      REAL AGEQESL, AGEQESLN, CO2QESL, CO2QESLN, QEFFSL, QEFFSLN

      REAL PSTRES1  !3/22/2011

!      SAVE AZIR,BETN,CEC,DLAYR,DLAYR2,DULE,FNPGL,FNPGN,LFANGD,
!     &  LL,LL2,LLE,LMXREF,LNREF,LWIDTH,NELAYR,NLAYR,NSLOPE,PALB,
!     &  SLPF,QEREF,RCUTIC,ROWSPC,RWU,SALB,SAT,SCVIR,SCVP,SLWREF,
!     &  SLWSLO,SWEF,TYPPGN,TYPPGL,XLMAXT,XSW,YLMAXT,YSCOND,YSHCAP

!-----------------------------------------------------------------------
      TYPE (ControlType) CONTROL
      TYPE (SoilType)    SOILPROP
      TYPE (SwitchType)  ISWITCH
      TYPE (WeatherType) WEATHER

!      PARAMETER (TINCR=24.0/TS)
        TINCR =24.0/REAL(TS)
!                TINCR is the lenfth of a time increment in hours
!     Transfer values from constructed data types into local variables.
      CROP    = CONTROL % CROP
      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO
      model   = control % model

      BD     = SOILPROP % BD
      DLAYR  = SOILPROP % DLAYR
      DUL    = SOILPROP % DUL
      LL     = SOILPROP % LL
      NLAYR  = SOILPROP % NLAYR

!     CHP 7/24/2006
!     SOILPROP now includes MSALB (with soil water and mulch effects
!     on soil albedo) and CMSALB (also includes canopy cover effects)
      SALBW  = SOILPROP % SALB

      SLPF = SOILPROP % SLPF
      SAT  = SOILPROP % SAT

      ISWWAT = ISWITCH % ISWWAT
      MEEVP  = ISWITCH % MEEVP
      MEPHO  = ISWITCH % MEPHO

      AZZON  = WEATHER % AZZON
      BETA   = WEATHER % BETA
      CLOUDS = WEATHER % CLOUDS
      CO2    = WEATHER % CO2
      FRDIFP = WEATHER % FRDIFP
      FRDIFR = WEATHER % FRDIFR
      PARHR  = WEATHER % PARHR
      RADHR  = WEATHER % RADHR
      REFHT  = WEATHER % REFHT
      RHUMHR = WEATHER % RHUMHR
      SNDN   = WEATHER % SNDN
      SNUP   = WEATHER % SNUP
      SRAD   = WEATHER % SRAD
      TA     = WEATHER % TA
      TAIRHR = WEATHER % TAIRHR
      TGRO   = WEATHER % TGRO     !I/O
      TGROAV = WEATHER % TGROAV   !I/O
      TGRODY = WEATHER % TGRODY
      TMIN   = WEATHER % TMIN
      WINDHR = WEATHER % WINDHR

!     Retrieve plant module data for use here.
      Call GET('PLANT', 'CANHT',  CANHT)
      Call GET('PLANT', 'CANWH',  CANWH)
      Call GET('PLANT', 'DXR57',  DXR57)
      Call GET('PLANT', 'EXCESS', EXCESS)
      Call GET('PLANT', 'NR5',    NR5)
      Call GET('PLANT', 'PLTPOP', PLTPOP)
      Call GET('PLANT', 'RNITP',  RNITP)
      Call GET('PLANT', 'SLAAD',  SLAAD)
      Call GET('PLANT', 'XPOD',   XPOD)

      CALL YR_DOY(YRDOY, YEAR, DOY) !LPM 04DEC12 for OPSTEMP
C========================================================================
C MEPHO  MEEVP
C -----  -----
C  'C'   /='Z' PHOTO used for photosynthesis, WATBAL used for ET
C  'L'   /='Z' ETPHOT used for photosynthesis, WATBAL used for ET
C  'L'    'Z'  ETPHOT used for both photosynthesis and ET
C========================================================================

C     Set MEEVP='N' temporarily if the water balance is not performed.
C     MEEVP reset on exit from ETPHOT to maintain input settings.

      IF (ISWWAT .EQ. 'N') THEN
        METEMP = MEEVP
        MEEVP = 'N'
      ENDIF

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
        IF (MEEVP .EQ. 'Z') THEN
          CALL ETINP(
     &    BD, DLAYR, DUL, FILEIO, LL, LUNIO, NLAYR,       !Input
     &    SALBW, SAT,                                     !Input
     &    AZIR, BETN, CEC, DLAYR2, DUL2, DULE, LFANGD,    !Output
     &    LL2, LLE, LWIDTH, NELAYR, PALBW, PHTHRS10,      !Output
     &    RCUTIC, ROWSPC, SAT2, SCVIR,SCVP, SWEF, XSW,    !Output
     &    YSCOND, YSHCAP)                                 !Output
        ENDIF

        IF (MEPHO .EQ. 'L' .AND. CROP .NE. 'FA') THEN
          CALL PGINP(
     &      model,FILEIO, LUNIO, SALB,                    !Input
     &      AZIR, BETN, FNPGL, FNPGN, LFANGD, LMXREF,     !Output
     &      LNREF, NSLOPE, PALBW, QEREF, ROWSPC,          !Output
     &      SCVP, SLWREF, SLWSLO, TYPPGL, TYPPGN,         !Output
     &      XLMAXT, YLMAXT, PHTHRS10,                     !Output
     &      CCNEFF, CICAD, cmxsf,cqesf,pgpath)            !Output

          CALL OpETPhot(CONTROL, ISWITCH,
     &   PCINPD, PG, PGNOON, PCINPN, SLWSLN, SLWSHN,
     &   PNLSLN, PNLSHN, LMXSLN, LMXSHN, TGRO, TGROAV)
        ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
        IF (MEEVP .EQ. 'Z') THEN
          DO I=1,NLAYR
!           TSHR(I) = TAV
            TSHR(I) = TA
           ST(I) = TSHR(I)
          ENDDO
          DO I = 1, TS
            TGRO(I) = TA
          ENDDO
            EP = 0.0
            ES = 0.0
            EOP = 0.0
            TGROAV = TA
            TGRODY = TA
            TEMPN = 0.0
            DO I = 1,3
               TSRF(I) = TA
               TSRFN(I) = TA
            ENDDO
!           LPM 04DEC14 to include the surface temperature as output
            SRFTEMP = TSRFN(3)
            CALL OPSTEMP(CONTROL,ISWITCH,DOY,SRFTEMP,ST,TAV,TAMP)  !LPM

          CALL ROOTWU(SEASINIT,
     &      DLAYR, LL, NLAYR, PORMIN, RLV, RWUMX, SAT, SW,!Input
     &      RWU,  TRWUP)                           !Output
        ENDIF

        AGEFAC  = 1.0
        CUMSTR  = 0.0
        COLDSTR = 0.0
        LMXSLN  = 0.0
        LMXSHN  = 0.0
        PCINPD  = 0.0
        PCINPN  = 0.0
        PG      = 0.0
        PGNOON  = 0.0
        PNLSLN  = 0.0
        PNLSHN  = 0.0
        SLWSLN  = 0.0
        SLWSHN  = 0.0
        SWFAC   = 1.0

        IF (MEPHO .EQ. 'L') THEN
          CALL OpETPhot(CONTROL, ISWITCH,
     &   PCINPD, PG, PGNOON, PCINPN, SLWSLN, SLWSHN,
     &   PNLSLN, PNLSHN, LMXSLN, LMXSHN, TGRO, TGROAV)
        ENDIF

C***********************************************************************
C***********************************************************************
C     COMPUTE DAILY RATES
C***********************************************************************

      ELSE IF (DYNAMIC.EQ.RATE) THEN

C     Initialize DAILY parameters.
!     PLTPOP can change due to pest damage. CHP
      IF (ROWSPC.GT.0.0 .AND. PLTPOP.GT.0.0) THEN
        BETN = 1.0 / (ROWSPC*PLTPOP)
      ELSE
        BETN = 0.0
      ENDIF

        HOLDLA = XLAI
        HOLDHT = CANHT
        HOLDWH = CANWH
        FRSHAV = 0.0
        NHOUR = 0
        TCANAV = 0.0
        TCANDY = 0.0

        IF (MEEVP .EQ. 'Z') THEN
          CALL ETIND(
     &      DUL2, RLV, SALBW, SW,                         !Input
     &      CEN,DAYRAD,DLAYR2,DULE,DYABSR,DYINTR,EDAY,    !Output
     &      EOP,ETNOON,FRDFRN,LLE,NELAYR,NLAYR,PCABRN,    !Output
     &      PCINRN,RADN,RLV2, SALB, SHCAP,ST2,STCOND,SW2, !Output
     &      SWE, TDAY,TEMPN,TSRF,TSRFN,XSW,YSCOND,YSHCAP) !Output

          CALL ROOTWU(RATE,
     &      DLAYR, LL, NLAYR, PORMIN, RLV, RWUMX, SAT, SW,!Input
     &      RWU, TRWUP)                           !Output

C  KJB NOTE TO CP.  NEED TO DELETE RWUH HERE, PUT IT INTO HOURLY
C  KJB CALL TO ROOTWU, DO WE INITIATE HOURLY? PRIOR TO HOURLY RATE?
C         Increase root water uptake rate 5-fold to account for
C         instantaeous vs. daily rates and convert to mm/h.
          !RWUH = 5.0 * TRWUP * 10.0/24.0
        ENDIF

        IF (MEPHO .EQ. 'L') THEN
          CALL PGIND(
     &      NLAYR, PALBW, DUL2, SW,                   !Input
     &      DAYPAR, DYABSP, DYINTP, FRDFPN, LMXSLN,   !Output
     &      LMXSHN, PALB, PARN, PGCO2, PGDAY, PGNOON, !Output
     &      PCABPN, PCINPN, PNLSLN, PNLSHN, SLWSLN,   !Output
     &      SLWSHN, SW2)                              !Output
        ENDIF

C       Compute hourly rates of canopy photosynthesis and evapotranspiration
C       and sum for day (TS=24 for hourly).

!       Accounting for hours of high transpiration compared to 24 hours which
!       is used by daily model.  The 2.0 accounts for lower transpiration
!       during early morning hours, with high relative humidity.
        DayRatio = 24.0 / (WEATHER % DAYL - 2.0)
!                 Note that DayRatio will blow up when DAYL is two hours, but this would only occur at high and low lattitudes
!                 on each side of winter when temperatures likely are too cold for crop growth. BAK.

!       Conpute index for mid-day time step added by Bruce Kimball on 9JAN17
        TSV2 = INT(TS/2)
        DO H=1,TS

C         Calculate real and solar time.
          HS = REAL(H) * TINCR
          IF (HS.GT.SNUP .AND. HS.LT.SNDN) THEN
            DAYTIM = .TRUE.
          ELSE
            DAYTIM = .FALSE.
          ENDIF
          CO2HR = CO2

C         Calculate hourly radiation absorption by canopy/soil.

          CALL RADABS(
     &      AZIR, AZZON(H), BETA(H), BETN, CANHT, CANWH,  !Input
     &      DAYTIM, FRDIFP(H), FRDIFR(H), H, LFANGD,      !Input
     &      MEEVP, MEPHO, PALB, PARHR(H), RADHR(H),       !Input
     &      ROWSPC, SALB, SCVIR, SCVP, XLAI,             !Input
     &      FRACSH, FRSHV, KDIRBL, KDRBLV, LAISH, LAISHV, !Output
     &      LAISL, LAISLV, PARSH, PARSUN, PCABSP, PCABSR, !Output
     &      PCINTP, PCINTR, RABS)                         !Output

C         Calculate canopy ET/photosynthesis.

C  KJB NOTE TO CP.  WE NEED TO CALL ROOTWU INSIDE ETPHR, PASS MODIFIER
C  KJB OF RWUMX HERE?
C  KJB Possibly call ROOTWU here hourly, so the water content decreases
C  KJB progressively with time of day to impact RWUH
C  KJB We would need the saved TRWUP from "iterated" transp to be used
C  KJB to compute actual water loss, as RWUH is hypothetical until the
C  KJB transpiration rate is solved in the loop.
C  KJB That would be the option to calling it from within ETPHR.  IF
C  KJB  inside ETPHR, you could use the relative stress of "Now" to
C  KJB impact the computed RWUH.  Which is safer?
C  KJB If inside the loop, we could let the EO have a small influence
C  KJB on RWUH, like water potential demand
C
C  KJB Present ROOTWU will not work.  We need a "psuedo-water content"
C  KJB update, so the SW(L) decreases each hour.  Can we call EXTRACT
C  KJB and SPSUM on an hourly basis?  Or put a dummy one in an hourly
C  KJB place inside ROOTWU that is called only if we have Z version
C  KJB The problem is the need to come back to this after iterative to
C  KJB create the actual water extracted.  So, need to call EXTRACT
C  KJB and SPSUM hourly.

!         RWUH = TRWUP * RADHR(H) / SRAD * 3600. / 1.E6 * 10.
!         mm/h = cm/d *  J/m2-s  / MJ/m2-d  * s/hr / J/MJ * mm/cm
          RWUH = (TRWUP*10.) * ((RADHR(H)*TINCR*3600.) / (SRAD * 1.E6))
! mm/time step =  cm/d * mm/cm    J/s-m2* h/timestep * s/h / (MJ/m2-d * J/MJ)
!        changed by Bruce Kimball on 10Jan17

!         Need multiplier to account for hourly : daily uptake rate
          RWUH = RWUH * DayRatio

          CALL ETPHR(
     &      CANHT, CEC, CEN, CLOUDS, CO2HR, DAYTIM,       !Input
     &      DLAYR2, DULE, FNPGL, FNPGN, FRACSH, FRSHV,    !Input
     &      KDIRBL, LAISH, LAISHV, LAISL, LAISLV, LLE,    !Input
     &      LMXREF, LNREF, LWIDTH, MEEVP, MEPHO, NLAYR,   !Input
     &      NSLOPE, PARSH, PARSUN, QEREF, RABS, RCUTIC,   !Input
     &      REFHT, RHUMHR(H), RNITP, RWUH, SHCAP, SLAAD,  !Input
     &      SLWREF, SLWSLO, STCOND, SWE, TAIRHR(H), TA,   !Input
     &      TMIN, TYPPGL, TYPPGN, WINDHR(H), XLAI,        !Input
     &      XLMAXT, YLMAXT,                               !Input
     &      AGEFAC, EHR, LFMXSH, LFMXSL, PCNLSH, PCNLSL,  !Output
     &      PGHR, SLWSH, SLWSL, T0HR, TCAN(H), THR, TSHR, !Output
     &      TSURF,                                        !Output
     &      CONDSH, CONDSL, RA, RB, RSURF, Rnet,          !Output
     &      G, LH, LHEAT, SH, SHEAT,                      !Output
C       CONDSH, CONDSL, RA, RB, RSURF, RNET output added by
C           Bruce Kimball on 2DEC14
     &     RBSH, RBSL, RBSS,                              !Output
C            added by BAK on 10DEC2015
     &      CCNEFF, CICAD, CMXSF, CQESF, PGPATH,          !Input
     &      AGEQESL, CO2QESL, QEFFSL)                     !Output

C         Integrate instantaneous canopy photoynthesis (µmol CO2/m2/s)
C         and evapotranspiration (mm/h) to get daily values (g CO2/m2/d
C         and mm/d).


          IF (MEPHO .EQ. 'L') THEN
            PGDAY = PGDAY + TINCR*PGHR*44.0*0.0036
          ENDIF

          IF (MEEVP .EQ. 'Z') THEN
            EDAY= EDAY + TINCR*EHR
            TDAY = TDAY + TINCR*THR
            EOP = EOP + TINCR*T0HR
            DO I=1,NLAYR
              ST2(I) = ST2(I) + TSHR(I)
            ENDDO
            DO I=1,3
              TSRF(I) = TSRF(I) + TSURF(I,1)
            ENDDO
            TCANAV = TCANAV + TCAN(H)
            IF (DAYTIM) TCANDY = TCANDY + TCAN(H)
          ENDIF

          DAYPAR = DAYPAR + TINCR*PARHR(H)*0.0036
          DYABSP = DYABSP + TINCR*PCABSP*PARHR(H)*0.000036
          DYINTP = DYINTP + TINCR*PCINTP*PARHR(H)*0.000036
          DAYRAD = DAYRAD + TINCR*RADHR(H)*0.0036
          DYINTR = DYINTR + TINCR*PCINTR*RADHR(H)*0.000036
          DYABSR = DYABSR + TINCR*PCABSR*RADHR(H)*0.000036

          IF (DAYTIM) THEN
            FRSHAV = FRSHAV + FRACSH
            NHOUR = NHOUR + 1
          ENDIF

C         Remember noon values (ET in mm/h; PG in mg CO2/m2/s).

C KJB WE COULD, BUT DON'T NEED, TO REMEMBER A MID-DAY WATER STRESS FACTOR?
!            Get mid-day values
          IF (H .EQ. TSV2) THEN
            IF (MEPHO .EQ. 'L') THEN
              FRDFPN = FRDIFP(H)
              LMXSLN = LFMXSL * 0.044
              LMXSHN = LFMXSH * 0.044
              PARN = PARHR(H)
              PCINPN = PCINTP
              PCABPN = PCABSP
              PGNOON = PGHR * 0.044
              PNLSLN = PCNLSL
              PNLSHN = PCNLSH
              SLWSLN = SLWSL * 1000.
              SLWSHN = SLWSH * 1000.
              QEFFSLN = QEFFSL
              CO2QESLN = CO2QESL
              AGEQESLN = AGEQESL
            ENDIF
            IF (MEEVP .EQ. 'Z') THEN
              ETNOON = EHR + THR
              RADN = RADHR(H)
              FRDFRN = FRDIFR(H)
              PCINRN = PCINTR
              PCABRN = PCABSR
              TEMPN = TAIRHR(H)

              DO I=1,3
                TSRFN(I) = TSURF(I,1)
              ENDDO
!             LPM 04DEC14 to include the surface temperature as output
              SRFTEMP = TSRFN(3)
              DO I=1,NLAYR
                  TSHRn(I) = TSHR(I)
              ENDDO
              CALL SOIL05(
     &          TSHRn,0,NLAYR,                                  !Input
     &          STn)                                           !Output
!             LPM 04DEC14 to include the temperature as output (OPSTEMP)
              ST = STn
C       The following 8 variales added by Bruce Kimball on 1Dec2014
              Enoon = EHR
              Tnoon = THR
              WINDN = WINDHR(H)
              TCANn = TCAN(H)
              CSHnn = CONDSH
              CSLnn = CONDSL
              LSHnn = LAISH
              LSLnn = LAISL
              GN = G
              LHN = LH
              SHN = SH
              RSSHN = RSSH
              RSSLN = RSSL
              RSSSN = RSSS
C     Next 3 lines added by BAK on 10DEC2015
              RBSHN = RBSH
              RBSLN = RBSL
              RBSSN = RBSS
              DO I=1,3
                LHEATN(I)=LHEAT(I,1)
                SHEATN(I)=SHEAT(I,1)
                RNETN(I)=RNET(I,1)
                ENDDO

            ENDIF
!            Print soil temperature data in STEMP.OUT
!            CALL OPSTEMP(CONTROL, ISWITCH, DOY, SRFTEMP, ST)
!            BAK 8Jun15 above line commented out because soil output seems to be called too much
          ENDIF

C       Remember midnight values
          IF(H.EQ.TS .AND. MEEVP .EQ. "Z") THEN
            ETnit = EHR + THR
            TEMnit = TAIRHR(H)
            Enit = EHR
            Tnit = THR
            WINnit = WINDHR(H)
            TCnit = TCAN(H)
            DO I = 1,3
                TSRnit(I) = TSURF(I,1)
            ENDDO
            CSHnit = CONDSH
            CSLnit = CONDSL
            LSHnit = LAISH
            LSLnit = LAISL
            GMT = G
              LHT = LH
              SHT = SH
              RSSHT = RSSH
              RSSLT = RSSL
              RSSST = RSSS
C next 3 lines added by BAK on 10DEC2015
              RBSHT = RBSH
              RBSLT = RBSL
              RBSST = RBSS
              DO I=1,3
                LHEATT(I)=LHEAT(I,1)
                SHEATT(I)=SHEAT(I,1)
                RNETT(I)=RNET(I,1)
                ENDDO
          ENDIF
        ENDDO

C       Assign daily values.

        XLAI = HOLDLA
        CANHT = HOLDHT
        CANWH = HOLDWH

        IF (MEEVP .EQ. 'Z') THEN
! FO - 06/30/2022 - Protections on DAYRAD for higher XLATs
          IF (XLAI .GT. 0.0 .AND. DAYRAD .GT. 0.0) THEN
            DAYKR = -LOG((DAYRAD-DYINTR)/DAYRAD) / XLAI
          ELSE
            DAYKR = 0.0
          ENDIF
          IF(DAYRAD .GT. 0.0) THEN
            PCABRD = DYABSR / DAYRAD * 100.0
            PCINRD = DYINTR / DAYRAD * 100.0
          ELSE
            PCABRD = 0.0
            PCINRD = 0.0
          ENDIF

          DO I=1,NLAYR
            ST2(I) = ST2(I) / TS
          ENDDO
          DO I=1,3
            TSRF(I) = TSRF(I) / TS
          ENDDO
          TCANAV = TCANAV / TS
          IF(NHOUR .GT. 0) THEN
            TCANDY = TCANDY / NHOUR
          ELSE
            TCANDY = 0.0
          ENDIF
          TGRODY = TCANDY
          TGROAV = TCANAV
          DO  I=1,TS
            TGRO(I) = TCAN(I)
          ENDDO

C           Save noon and midnight growth and air temperatures
C           add by Bruce Kimball on 9MAR15
          TGROnn = TGRO(TS/2)
          TGROnit = TGRO(TS)
          TAnn = TAIRHR(TS/2)
          TAnit = TAIRHR(TS)

          CALL SOIL05(
     &      ST2,0,NLAYR,                                  !Input
     &      ST)                                           !Output

!          CALL SOIL05(
!     &      RWU2, 0, NLAYR,
!     &      RWU)
!CHP
!          AWEV1 = (SW2(1)-LL2(1)*SWEF) * DLAYR2(1) * 10.0
C         AWEV1 = (SW(1)-LL(1)*SWEF) * DLAYR(1) * 10.0
          EP = MAX(TDAY,0.0)
C WHY SHOULD AWEV1 LIMIT IF SWE ALREADY SETS LIMIT. REMOVE AWEV1, KJB & BK 10 JULY 2017
C          ES = MAX(MIN(EDAY,AWEV1),0.0)
          ES = MAX(EDAY,0.0)
        ENDIF

        IF (MEPHO .EQ. 'L') THEN
! FO - 06/30/2022 - Protections on DAYRAD for higher XLATs
          IF (XLAI .GT. 0.0 .AND. DAYPAR .GT. 0.0) THEN
            DAYKP = -LOG((DAYPAR-DYINTP)/DAYPAR) / XLAI
          ELSE
            DAYKP = 0.0
          ENDIF

          IF(DAYPAR .GT. 0.0) THEN
            PCABPD = DYABSP / DAYPAR * 100.0
            PCINPD = DYINTP / DAYPAR * 100.0
          ELSE
            PCABPD = 0.0
            PCINPD = 0.0
          ENDIF
          IF(NHOUR .GT. 0) THEN
            FRSHAV = FRSHAV / NHOUR
          ELSE
            FRSHAV = 0.0
          ENDIF
          PG = PGDAY/44.0*30.0 * SLPF
          PGCO2 = PGDAY * SLPF

!*****************************************
!         Calculate daily water stess factors (from SWFACS)
          SWFAC = 1.0
          IF (EOP .GT. 1.E-4 .AND. ISWWAT .EQ. 'Y') THEN
            IF ((EOP * 0.1) .GE. TRWUP) THEN
              SWFAC = TRWUP / (EOP * 0.1)
            ENDIF
          ENDIF
!*****************************************

          IF (MEEVP .NE. 'Z') THEN
C
C KJB USE THE REAL MID-DAY WATER STRESS FACTOR HERE, NOT THE DAILY ONE?
C KJB AT LEAST FOR THE PGNOON?
C
!     CHP 3/22/2011 - multiply by P stress here.
            PG = PG * SWFAC * PSTRES1
            PGCO2 = PGCO2 * SWFAC * PSTRES1
            LMXSLN = LMXSLN * SWFAC * PSTRES1
            LMXSHN = LMXSHN * SWFAC * PSTRES1
            PGNOON = PGNOON * SWFAC * PSTRES1
          ENDIF

C         Post-processing for some stress effects (duplicated in PHOTO).
          PGXX = PG
          IF (DAS .GT. NR5) THEN
            CUMSTR =  CUMSTR + DXR57 * (1.0-SWFAC)*XPOD / PHTHRS10
            COLDSTR = 0.0
              PG = PG * (1.0 - 0.3*CUMSTR)
          ELSE
            CUMSTR = 0.0
            COLDSTR = 0.0
          ENDIF
          PG = PG * EXCESS

          CALL OpETPhot(CONTROL, ISWITCH,
     &   PCINPD, PG, PGNOON, PCINPN, SLWSLN, SLWSHN,
     &   PNLSLN, PNLSHN, LMXSLN, LMXSHN, TGRO, TGROAV)
        ENDIF
!***********************************************************************
!***********************************************************************
!     SEASEND
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND .OR. DYNAMIC .EQ. OUTPUT) THEN
!-----------------------------------------------------------------------
        IF (MEPHO .EQ. 'L') THEN
                CALL OpETPhot(CONTROL, ISWITCH,
     &   PCINPD, PG, PGNOON, PCINPN, SLWSLN, SLWSHN,
     &   PNLSLN, PNLSHN, LMXSLN, LMXSHN, TGRO, TGROAV)
        ENDIF

        IF(MEEVP .EQ. "Z") THEN
            CALL OPSTEMP(CONTROL, ISWITCH, DOY, SRFTEMP, ST, TAV, TAMP)
            ENDIF
!      BAK 8Jun15 Added the IF statement for call to OPSTEMP

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************

      IF (ISWWAT .EQ. 'N') THEN
        MEEVP = METEMP
        METEMP = ' '
      ENDIF

!     Store PG and AGEFAC for use by PLANT routine.
      CALL PUT('SPAM', 'AGEFAC', AGEFAC)
      CALL PUT('SPAM', 'PG'    , PG)

C       If the method to compute ET is energy balance, then
C       grow the plants at canopy temperature. Else grow them
C       at air temperature (TGRO initialized to TA in HMET.)
C       IF added by Bruce Kimball on 9MAR15
      IF(MEEVP .EQ. 'Z') THEN
        WEATHER % TGROAV = TCANAV
        WEATHER % TGRO   = TCAN
        WEATHER % TGRODY = TCANDY
        ELSE
        WEATHER % TGROAV = TGROAV   !I/O
        WEATHER % TGRO   = TGRO     !I/O
        WEATHER % TGRODY = TGRODY
        ENDIF

      RETURN
      END SUBROUTINE ETPHOT

C=======================================================================
C  ETINP, Subroutine, N.B. Pickering, 12/12/90
C  Reads in and initializes fixed ET parameters.
C------------------------------------------------------------------------
C  REVISION HISTORY
C  12/05/1990 NBP Written.
C  05/10/1994 NBP Added PGFAC3,SLWSLO,LNREF,SLWREF
C  09/21/1999 NBP Moved weather parameters to ETPSIM.
C  09/25/1999 NBP Separate ET and PHOT routines.
C  01/20/2000 NBP Put ETINF in ETINP.
C  08/12/2003 CHP  Added I/O error checking
C-----------------------------------------------------------------------
C  Called from: ETPHOT
C  Calls:       ERROR,FIND,SOIL10
C=======================================================================

      SUBROUTINE ETINP(
     &    BD, DLAYR, DUL, FILEIO, LL, LUNIO, NLAYR,       !Input
     &    SALBW, SAT,                                     !Input
     &    AZIR, BETN, CEC, DLAYR2, DUL2, DULE, LFANGD,    !Output
     &    LL2, LLE, LWIDTH, NELAYR, PALBW, PHTHRS10,      !Output
     &    RCUTIC, ROWSPC, SAT2, SCVIR,SCVP, SWEF, XSW,    !Output
     &    YSCOND, YSHCAP)                                 !Output

!     ------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE
      EXTERNAL FIND, GETLUN, IGNORE, SOIL10, ERROR
      SAVE

      CHARACTER BLANK*1,ERRKEY*6,FILEC*12,FILECC*92,FILEIO*30,
     &  PATHCR*80,SECTION*6
      CHARACTER*80 C80
      INTEGER ERRNUM,FOUND,I,J,LNUM,LUNCRP,LUNIO,NELAYR,NLAYR,
     &  PATHL, ISECT
      PARAMETER (BLANK=' ', ERRKEY='ETPINP')
      REAL AZIR,BD(NL),BD2(NL),BETN,CEC,CORRN, DLAYR(NL),
     &  DLAYR2(NL),DUL(NL),DUL2(NL),DULE,GA,GC,HCAPS(NL),HCAPS2(NL),
     &  KSOIL,LFANGB,LFANGD(3),LL(NL),LL2(NL),LLE,LWIDTH,N1,PALBW,
     &  PLTPOP,RCUTIC,ROWSPC,SALBW, SCVIR,SCVP,
     &  SWEF,TCAIR,TCNDS2(NL),TCONDS(NL),TCWATR,VHCWAT,XAIR,
     &  XPORE,XSOIL,XSW(NL,3),XWATER,YSCOND(NL,3),YSHCAP(NL,3),ZERO
      PARAMETER (VHCWAT=4.18E+6, TCAIR=0.025, TCWATR=0.57, ZERO=1.0E-6)
      REAL PHTHRS10
      REAL, DIMENSION(NL) :: SAT, SAT2

C     Read IBSNAT35.INP file.

      OPEN(LUNIO,FILE=FILEIO,STATUS='OLD',IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)

      SECTION = '*FILES'
      CALL FIND(LUNIO,SECTION,LNUM,FOUND)
      IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEIO,LNUM)

      READ(LUNIO,'(////,15X,A,1X,A)',IOSTAT=ERRNUM) FILEC,PATHCR
      LNUM = LNUM + 5
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)

      PATHL  = INDEX(PATHCR,BLANK)
      IF (PATHL .LE. 1) THEN
        FILECC = FILEC
      ELSE
        FILECC = PATHCR(1:(PATHL-1)) // FILEC
      ENDIF

      REWIND(LUNIO)
      SECTION = '*PLANT'
      CALL FIND(LUNIO,SECTION,LNUM,FOUND)
      IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEIO,LNUM)

      READ(LUNIO,'(24X,F6.0,12X,2F6.0)',IOSTAT=ERRNUM)PLTPOP,ROWSPC,AZIR
      LNUM = LNUM + 1
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM+1)

      DO I = 1,NL
        HCAPS(I) = 0.0
        TCONDS(I) = 0.0
      ENDDO

!     PHTHRS(10) needed for calculation of CUMSTR
      REWIND(LUNIO)
      SECTION = '*CULTI'
      CALL FIND(LUNIO, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) CALL ERROR (SECTION, 42, FILEIO,LNUM)
      READ(LUNIO,'(60X,F6.0)',IOSTAT=ERRNUM) PHTHRS10; LNUM = LNUM + 1
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)

      CLOSE(LUNIO)

C     Read species file.
      CALL GETLUN('FILEC', LUNCRP)
      OPEN(LUNCRP,FILE=FILECC,STATUS='OLD',IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILECC,0)

      SECTION = '!*PHOT'
      CALL FIND(LUNCRP,SECTION,LNUM,FOUND)
      DO I=1,8    !Read 8th line of photosynthesis section
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
      ENDDO
      READ(C80,'(6X,F6.0,6X,F6.0)',IOSTAT=ERRNUM) SCVP,LFANGB
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILECC,LNUM)

      CLOSE(LUNCRP)

C     Initialize some parameters.

      SCVIR = 4.0 * SCVP
      PALBW = 0.6 * SALBW

      ROWSPC = ROWSPC / 100.0
      IF (ROWSPC.GT.0.0 .AND. PLTPOP.GT.0.0) THEN
        BETN = 1.0 / (ROWSPC*PLTPOP)
      ELSE
        BETN = 0.0
      ENDIF

      NELAYR = 1
      LWIDTH = 0.02
      RCUTIC = 5000.0
      DO I = 1,NLAYR
        hcaps(i) = 2.17e6
        tconds(i) = 7.8
      ENDDO

      SWEF = 0.9-0.00038*(DLAYR(1)-30.)**2

C     Transform soil layers from 5,15,etc. to 10,10,etc. (10 in top layer
C     is necessary to prevent instability in ETPHOT).

      CALL SOIL10(
     &  DLAYR,                                            !Input
     &  1,NLAYR,DLAYR2)                                   !Output
      CALL SOIL10(
     &  LL,                                               !Input
     &  0,NLAYR,LL2)                                      !Output
      CALL SOIL10(
     &  DUL,                                              !Input
     &  0,NLAYR,DUL2)                                     !Output
      CALL SOIL10(
     &  SAT,                                              !Input
     &  0,NLAYR,SAT2)                                     !Output
      CALL SOIL10(
     &  BD,                                               !Input
     &  0,NLAYR,BD2)                                      !Output
      CALL SOIL10(
     &  HCAPS,                                            !Input
     &  0,NLAYR,HCAPS2)                                   !Output
      CALL SOIL10(
     &  TCONDS,                                           !Input
     &  0,NLAYR,TCNDS2)                                   !Output

C     Calculate parameters for E depth (DULE and LLE in mm). CEC starts
C     at 0.1 * the 1st stage evaporation amount.

      DULE = 0.0
      LLE = 0.0
      DO I = 1,NELAYR
        DULE = DULE + DUL2(I)*DLAYR2(I)*10.0
        LLE = LLE + LL2(I)*DLAYR2(I)*10.0
      ENDDO
C     CEC = 0.45 * U / (DULE-LLE) * 100.0
      CEC = 0.0

C     Calculate soil thermal properties.  Arrays YSHCAP and YSCOND store
C     results for daily table lookup as a function of moisture content.

      N1 = 5.0
      GA = 1.0 / (2.0+N1)
      GC = 1.0 - 2.0*GA

      DO I=1,NL
        DO J=1,3
          XSW(I,J) = 0.0
          YSHCAP(I,J) = 0.0
          YSCOND(I,J) = 0.0
        ENDDO
      ENDDO

      DO I=1,NLAYR
        XSOIL = BD2(I)/2.65
        XPORE = 1.0 - XSOIL

C       Calculate soil heat capacity and thermal conductivity parameters
C       using De Vries (1963) for dry, field capacity and saturated
C       moisture contents (XSW).

        DO J=1,3

C         Dry soil.

          IF (J .EQ. 1) THEN
            XAIR = XPORE
            XWATER = 0.0
            KSOIL = (2.0 / (1.+(TCNDS2(I)/TCAIR-1.0)*GA)
     &        + 1.0 / (1.+(TCNDS2(I)/TCAIR-1.0)*GC)) / 3.0
            CORRN = 1.25

C         Field capacity soil.

          ELSE IF (J .EQ. 2) THEN
            XAIR = XPORE - DUL2(I)
            XWATER = DUL2(I)
            KSOIL = (2.0 / (1.+(TCNDS2(I)/TCWATR-1.0)*GA)
     &        + 1.0 / (1.+(TCNDS2(I)/TCWATR-1.0)*GC)) / 3.0
            CORRN = 1.0

C         Saturated soil.

          ELSE
            XAIR = 0.0
            XWATER = XPORE
            KSOIL = (2.0 / (1.+(TCNDS2(I)/TCWATR-1.0)*GA)
     &        + 1.0 / (1.+(TCNDS2(I)/TCWATR-1.0)*GC)) / 3.0
            CORRN = 1.0
          ENDIF
          XSW(I,J) = XWATER
          YSHCAP(I,J) = HCAPS2(I)*XSOIL + VHCWAT*XWATER
          YSCOND(I,J) = (KSOIL*XSOIL*TCNDS2(I)+XWATER*TCWATR+XAIR*TCAIR)
     &      / (KSOIL*XSOIL+XWATER+XAIR) * CORRN

        ENDDO
      ENDDO

C     Compute leaf angles in three classes (0-30, 30-60, 60-90) using
C     the ellipsoidal distribution.  Approx. eqn. for CDF at 30 and 60 deg.

      LFANGD(1) = 0.936 * (1.0-0.630*EXP(-0.719*LFANGB))**4.950
      LFANGD(2) = 0.974 * (1.0-1.109*EXP(-1.037*LFANGB))**1.408
      LFANGD(3) = 1.0 - LFANGD(2)
      LFANGD(2) = LFANGD(2) - LFANGD(1)

      RETURN
      END SUBROUTINE ETINP

C=======================================================================
C  PGINP, Subroutine, N.B. Pickering, 12/12/90
C  Reads and initializes fixed PG parameters.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  12/05/1990 NBP Written.
C  05/10/1994 NBP Added PGFAC3,SLWSLO,LNREF,SLWREF
C  09/21/1999 NBP Moved weather parameters to ETPSIM.
C  09/25/1999 NBP Separate ET and PHOT routines.
C  01/20/2000 NBP Put PGINF in PGINP.
C  07/03/2000 GH  Included  PHTHRS10
C  08/12/2003 CHP Added I/O error checking
C-----------------------------------------------------------------------
C  Called from: ETPHOT
C  Calls:       ERROR,FIND
C=======================================================================

      SUBROUTINE PGINP(
     &  model,FILEIO, LUNIO, SALBW,                       !Input
     &  AZIR, BETN, FNPGL, FNPGN, LFANGD, LMXREF,         !Output
     &  LNREF, NSLOPE, PALBW, QEREF, ROWSPC,              !Output
     &  SCVP, SLWREF, SLWSLO, TYPPGL, TYPPGN,             !Output
     &  XLMAXT, YLMAXT, PHTHRS10,                         !Output
     &  ccneff, cicad, cmxsf, cqesf, pgpath)              !Output

      IMPLICIT NONE
      EXTERNAL FIND, GETLUN, IGNORE, ERROR
      SAVE

      CHARACTER BLANK*1,ERRKEY*6,FILEC*12,FILECC*92,FILEIO*30,
     &  PATHCR*80,SECTION*6,TYPPGL*3,TYPPGN*3
      CHARACTER*80 C80

      INTEGER ERRNUM,FOUND,I,LINC,LNUM,LUNCRP,LUNIO,PATHL
      INTEGER ISECT

      PARAMETER (BLANK=' ', ERRKEY='PGINP ')

      REAL AZIR,BETN,FNPGL(4),LFANGB,LFANGD(3),LMXREF,LNREF,NSLOPE,
     &  PALBW,PLTPOP,QEREF,ROWSPC,SALBW,SCVP,SLWREF,
     &  SLWSLO,FNPGN(4),XLMAXT(6),YLMAXT(6),PHTHRS10

      character(len=2) pgpath
      character(len=8) model
      real ccneff, cicad, cmxsf, cqesf

C     Read IBSNAT35.INP file.

      OPEN(LUNIO,FILE=FILEIO,STATUS='OLD',IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)

      SECTION = '*FILES'
      CALL FIND(LUNIO,SECTION,LNUM,FOUND)
      IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEIO,LNUM)

      READ (LUNIO,'(////15X,A,1X,A)',IOSTAT=ERRNUM) FILEC,PATHCR
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM+5)
      PATHL  = INDEX(PATHCR,BLANK)
      IF (PATHL .LE. 1) THEN
        FILECC = FILEC
      ELSE
        FILECC = PATHCR(1:(PATHL-1)) // FILEC
      ENDIF

      REWIND(LUNIO)
      SECTION = '*PLANT'
      CALL FIND(LUNIO,SECTION,LNUM,FOUND)
      IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEIO,LNUM)

      READ(LUNIO,'(24X,F6.0,12X,2F6.0)',IOSTAT=ERRNUM)PLTPOP,ROWSPC,AZIR
      LNUM = LNUM + 1
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM+1)

      REWIND(LUNIO)
      SECTION = '*CULTI'
      CALL FIND(LUNIO,SECTION,LNUM,FOUND)
      CALL FIND(LUNIO,SECTION,LINC,FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEIO,LNUM)

C-GH  READ(LUNIO,'(72X,F6.0)') LMXREF
      READ(LUNIO,'(60X,F6.0,6X,F6.0)',IOSTAT=ERRNUM) PHTHRS10,LMXREF
      LNUM = LNUM + 1
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM+1)

      CLOSE(LUNIO)

C     Read species file.
      CALL GETLUN('FILEC', LUNCRP)
      OPEN(LUNCRP,FILE=FILECC,STATUS='OLD',IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILECC,0)

      SECTION = '!*PHOT'
      CALL FIND(LUNCRP,SECTION,LNUM,FOUND)

!     Read 3rd line of photosynthesis section of species file
      CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
      CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
      CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
      READ(C80,'(4F6.0,3X,A)',IOSTAT=ERRNUM) (FNPGN(I),I=1,4), TYPPGN
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILECC,LNUM)

      CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
      CALL IGNORE(LUNCRP,LNUM,ISECT,C80)  !5th line
      READ(C80,'(6F6.0)',IOSTAT=ERRNUM) (XLMAXT(I),I=1,6)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILECC,LNUM)

      CALL IGNORE(LUNCRP,LNUM,ISECT,C80)  !6th line
      READ(C80,'(6F6.0)',IOSTAT=ERRNUM) (YLMAXT(I),I=1,6)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILECC,LNUM)

      CALL IGNORE(LUNCRP,LNUM,ISECT,C80)  !7th line
      READ(C80,'(4F6.0,3X,A)',IOSTAT=ERRNUM) (FNPGL(I),I=1,4),TYPPGL
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILECC,LNUM+7)

      CALL IGNORE(LUNCRP,LNUM,ISECT,C80)  !8th line
      READ(C80,'(2F6.0,6X,F6.0)',IOSTAT=ERRNUM) QEREF,SCVP,LFANGB
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILECC,LNUM)

      CALL IGNORE(LUNCRP,LNUM,ISECT,C80)  !9th line
      READ(C80,'(4F6.0)',IOSTAT=ERRNUM) SLWREF,SLWSLO,NSLOPE,LNREF
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILECC,LNUM)

      if( model(1:5) == 'PRFRM' ) then
         CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
         CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
         CALL IGNORE(LUNCRP,LNUM,ISECT,C80) !12th line
         READ(C80,'(4F6.0,2X,A)',IOSTAT=ERRNUM) CICAD,CCNEFF,
     &        CMXSF,CQESF,PGPATH
         IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILECC,LNUM)
      else
         pgpath='  '
         cicad = -99
         ccneff = -99
         cmxsf = -99
         cqesf = -99
      end if


      CLOSE(LUNCRP)

C     Initialize some parameters.

      PALBW = 0.6 * SALBW
!      PGFAC3 = SLPF

      ROWSPC = ROWSPC / 100.0
      IF (ROWSPC.GT.0.0 .AND. PLTPOP.GT.0.0) THEN
        BETN = 1.0 / (ROWSPC*PLTPOP)
      ELSE
        BETN = 0.0
      ENDIF

C     Compute leaf angles in three classes (0-30, 30-60, 60-90) using
C     the ellipsoidal distribution.  Approx. eqn. for CDF at 30 and 60 deg.

      LFANGD(1) = 0.936 * (1.0-0.630*EXP(-0.719*LFANGB))**4.950
      LFANGD(2) = 0.974 * (1.0-1.109*EXP(-1.037*LFANGB))**1.408
      LFANGD(3) = 1.0 - LFANGD(2)
      LFANGD(2) = LFANGD(2) - LFANGD(1)

      RETURN
      END SUBROUTINE PGINP

C=======================================================================
C  ETIND, Subroutine, N.B. Pickering
C  Initializes daily input variables for ET.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  10/14/92 NBP Written
C  11/23/93 NBP SWE returned to ETPHOT.
C  12/11/93 NBP Made calc. of Rsoil larger, removed AWES1, RWUH greater.
C               Added noon variable initialization.
C  03/13/94 NBP Added soil layer conversion: 5,15.. cm to 10,10.. cm.
C  04/24/94 NBP Replaced 24.0 with TS.
C-----------------------------------------------------------------------
C  Called from: ETPHOT
C  Calls:       SOIL10,TABEX
C=======================================================================

      SUBROUTINE ETIND(
     &  DUL2, RLV, SALBW, SW,                             !Input
     &  CEN,DAYRAD,DLAYR2,DULE,DYABSR,DYINTR,EDAY,        !Output
     &  EOP,ETNOON,FRDFRN,LLE,NELAYR,NLAYR,PCABRN,        !Output
     &  PCINRN,RADN,RLV2, SALB, SHCAP,ST2,STCOND,SW2,     !Output
     &  SWE, TDAY,TEMPN,TSRF,TSRFN,XSW,YSCOND,YSHCAP)     !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types,
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
!     NL, TS defined in ModuleDefs.for

      IMPLICIT NONE
      EXTERNAL SOIL10, TABEX
      SAVE

      INTEGER I,J,NELAYR,NLAYR
      REAL CEN,DAYRAD,DLAYR2(NL),DULE,DYABSR,DYINTR,EDAY,ETNOON,FRDFRN,
     &  LLE,PCINRN,PCABRN,RADN,SHCAP(NL),ST2(NL),STCOND(NL),
     &  SW(NL),SW2(NL),SWE,EOP,TABEX,TDAY,TEMPN,TSRF(3),
     &  TSRFN(3),XC(3),XSW(NL,3),YHC(3),YTC(3),YSCOND(NL,3),
     &  YSHCAP(NL,3)
      REAL SALB, SALBW, SALBD
      REAL, DIMENSION(NL) :: DUL2, RLV, RLV2

C     Initialize.
      EDAY   = 0.0
      ETNOON = 0.0
      FRDFRN = 0.0
      EOP  = 0.0
      TDAY   = 0.0
      DAYRAD = 0.0
      DYABSR = 0.0
      DYINTR = 0.0
      PCINRN = 0.0
      PCABRN = 0.0
      RADN   = 0.0
      DO I=1,NLAYR
        ST2(I) = 0.0
      ENDDO
      TEMPN = 0.0
      DO I=1,3
        TSRF(I)  = 0.0
        TSRFN(I) = 0.0
      ENDDO

C     Transform soil layers for SW and RLV.

      CALL SOIL10(
     &  SW,                                               !Input
     &  0,NLAYR,SW2)                                      !Output
      CALL SOIL10(
     &  RLV,                                              !Input
     &  0,NLAYR,RLV2)                                     !Output

C     Calculate SW in the evaporation zone, soil heat capacity,
C     and soil thermal conductivity by layer.

      SWE = 0.0
      DO I = 1,NELAYR
        SWE = SWE + SW2(I)*DLAYR2(I)*10.0
      ENDDO
      CEN = MIN(MAX((DULE-SWE)/(DULE-LLE)*100.0,0.0),100.0)
      DO I=1,NLAYR
        DO J=1,3
          XC(J) = XSW(I,J)
          YHC(J) = YSHCAP(I,J)
          YTC(J) = YSCOND(I,J)
        ENDDO
        SHCAP(I) = TABEX(YHC,XC,SW2(I),3)
        STCOND(I) = TABEX(YTC,XC,SW2(I),3)
      ENDDO

C     Calulation of albedo as a function of SW.

!     CHP 7/24/2006
!     SOILPROP Now includes MSALB (with soil water and mulch effects
!     on soil albedo) and CMSALB (also includes canopy cover effects)
      IF (SW2(1) .GE. DUL2(1)) THEN
        SALB = SALBW
      ELSE
        SALBD = SALBW * 1.25
        SALB = SALBD - (SALBD-SALBW)/DUL2(1)*SW2(1)
      ENDIF

      RETURN
      END SUBROUTINE ETIND

C=======================================================================
C  PGIND, Subroutine, N.B. Pickering
C  Initializes daily input variables for PG.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  10/14/92 NBP Written
C  12/11/93 NBP Added noon variable initialization.
C-----------------------------------------------------------------------
C  Called from: ETPHOT
C  Calls:
C=======================================================================

      SUBROUTINE PGIND(
     &  NLAYR, PALBW, DUL2, SW,                       !Input
     &  DAYPAR, DYABSP, DYINTP, FRDFPN, LMXSLN,       !Output
     &  LMXSHN, PALB, PARN, PGCO2, PGDAY, PGNOON,     !Output
     &  PCABPN, PCINPN, PNLSLN, PNLSHN, SLWSLN,       !Output
     &  SLWSHN, SW2)                                  !Output

      USE MODULEDEFS
      IMPLICIT NONE
      EXTERNAL SOIL10
      SAVE

      INTEGER NLAYR
      REAL DAYPAR,DYABSP,DYINTP,DUL2(NL),FRDFPN,LMXSLN,LMXSHN,PALB,
     &  PALBD,PALBW,PARN,PGCO2,PGDAY,PGNOON,PCABPN,PCINPN,PNLSLN,
     &  PNLSHN,SLWSLN,SLWSHN,SW(NL),SW2(NL)

C     Initialize.

      DAYPAR = 0.0
      DYABSP = 0.0
      DYINTP = 0.0
      FRDFPN = 0.0
      LMXSLN = 0.0
      LMXSHN = 0.0
      PARN   = 0.0
      PGCO2  = 0.0
      PGDAY  = 0.0
      PGNOON = 0.0
      PCABPN = 0.0
      PCINPN = 0.0
      PNLSLN = 0.0
      PNLSHN = 0.0
      SLWSLN = 0.0
      SLWSHN = 0.0

C     Calculate SW in the transformed layers.

      CALL SOIL10(
     &  SW,                                               !Input
     &  0,NLAYR,SW2)                                      !Output

C     Calulation of albedo as a function of SW.

      IF (SW2(1) .GE. DUL2(1)) THEN
        PALB = PALBW
      ELSE
        PALBD = PALBW * 1.25
        PALB = PALBD - (PALBD-PALBW)/DUL2(1)*SW2(1)
      ENDIF

      RETURN
      END SUBROUTINE PGIND

C=======================================================================
C  RADABS, Subroutine, N.B. Pickering
C  Calculates hourly canopy absorption of PAR and IR radiation.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  03/31/91 NBP Written
C-----------------------------------------------------------------------
C  Called from: ETPHOT
C  Calls:       CANABS,LFEXTN,SHADOW
C=======================================================================

      SUBROUTINE RADABS(
     &  AZIR, AZZON, BETA, BETN, CANHT, CANWH,            !Input
     &  DAYTIM, FRDIFP, FRDIFR, H, LFANGD,                !Input
     &  MEEVP, MEPHO, PALB, PARHR, RADHR,                 !Input
     &  ROWSPC, SALB, SCVIR, SCVP, XLAI,                 !Input
     &  FRACSH, FRSHV, KDIRBL, KDRBLV, LAISH, LAISHV,     !Output
     &  LAISL, LAISLV, PARSH, PARSUN, PCABSP, PCABSR,     !Output
     &  PCINTP, PCINTR, RABS)                             !Output

      IMPLICIT NONE
      EXTERNAL WARNING, SHADOW, LFEXTN, CANABS
      SAVE

      CHARACTER MEEVP*1,MEPHO*1
      CHARACTER*78  MESSAGE(10)
      INTEGER H
      LOGICAL DAYTIM
      REAL AZIR,AZZON,BETA,BETN,CANHT,CANWH,FRACSH,FRDIFP,FRDIFR,
     &  FRSHV,KDIFBL,KDIRBL,KDRBLV,LAISH,LAISL,LAISHV,LAISLV,
     &  LFANGD(3),PALB,PARHR,PARSH,PARSUN(3),PARSL,PARSS,PCINTP,
     &  PCABSP,PCINTR,PCABSR,PCREFP,PCREFR,RADHR,RABS(3),RNG,
     &  PTOT,RTOT,ROWSPC,SALB,SCVP,SCVIR,XLAI,PARQC,PARW,IRHR,
     &  IRALB,FRDIFI,PCABSI,PCINTI,PCREFI,IRSH,IRSUN(3),IRSL,IRSS
      PARAMETER (PARQC=4.6)

C     Initialize.

      IF (H .EQ. 1) THEN
        FRSHV = 0.0
        LAISHV = 0.0
        LAISLV = 0.0
      ENDIF
      FRACSH = 0.0
      KDIRBL = 0.0
      LAISH = 0.0
      LAISL = 0.0
      PCABSP = 0.0
      PCINTP = 0.0
      PCREFP = 0.0
      PARSUN(1) = 0.0
      PARSUN(2) = 0.0
      PARSUN(3) = 0.0
      PARSL = 0.0
      PARSH = 0.0
      PARSS = 0.0
      PCABSR = 0.0
      PCINTR = 0.0
      PCREFR = 0.0
      RABS(1) = 0.0
      RABS(2) = 0.0
      RABS(3) = 0.0

      IF (XLAI .GT. 0.0) THEN

C       Calculate fraction shaded and LAI's for vertical sun position.

        IF (H .EQ. 1) THEN
          CALL SHADOW(
     &      AZIR, AZZON, 90.0, BETN, CANHT, CANWH, ROWSPC,!Input
     &      FRSHV)                                        !Output
          CALL LFEXTN(
     &      90.0, FRSHV, LFANGD, XLAI,                   !Input
     &      KDIFBL, KDRBLV, LAISHV, LAISLV, RNG)          !Output
        ENDIF

        IF (DAYTIM) THEN

C         Calculate fraction shaded, leaf extinction and LAI's.

          CALL SHADOW(
     &      AZIR, AZZON, BETA, BETN, CANHT, CANWH, ROWSPC,!Input
     &      FRACSH)                                       !Output
          CALL LFEXTN(
     &      BETA, FRACSH, LFANGD, XLAI,                  !Input
     &      KDIFBL, KDIRBL, LAISH, LAISL, RNG)            !Output

C         Calculate PAR absorbed by canopy during day.

          IF (MEPHO .EQ. 'L' .OR. MEEVP .EQ. 'Z') THEN
            CALL CANABS(
     &        PALB, BETA, BETN, CANHT, CANWH, FRACSH,     !Input
     &        FRDIFP, KDIFBL, KDIRBL, LAISH, LAISL, PARHR,!Input
     &        RNG, ROWSPC, SCVP,                          !Input
     &        PCABSP, PCINTP, PCREFP, PARSH, PARSS,       !Output
     &        PARSUN)                                     !Output
            PARSL = PARSUN(2)
          ENDIF

C         Calculate infrared radiation absorbed by canopy during day.

          IF (MEEVP .EQ. 'Z') THEN
            PARW = PARHR / PARQC
            IRHR = RADHR - PARW
            IF (IRHR .GT. 0.0) THEN
              IRALB = MIN( (SALB*RADHR-PALB*PARW)/IRHR, 1.0)
              FRDIFI = MIN( (FRDIFR*RADHR-FRDIFP*PARW)/IRHR, 1.0)
            ELSE
              WRITE(MESSAGE(1),100) H
              WRITE(MESSAGE(2),101)
  100         FORMAT('Error in RADHR or PARHR for hour ',I2,'.')
  101         FORMAT('Program will stop.')
              CALL WARNING(2, 'RADABS', MESSAGE)
              WRITE(*,'(A78)') MESSAGE(1)
              STOP
            ENDIF
            CALL CANABS(
     &        IRALB, BETA, BETN, CANHT, CANWH, FRACSH,    !Input
     &        FRDIFI, KDIFBL, KDIRBL, LAISH, LAISL, IRHR, !Input
     &        RNG, ROWSPC, SCVIR,                         !Input
     &        PCABSI, PCINTI, PCREFI, IRSH, IRSS,         !Output
     &        IRSUN)                                      !Output
            IRSL = IRSUN(2)

C           Convert total radiation to land area basis.

            RABS(1) = (IRSL+PARSL/PARQC) * LAISL
            RABS(2) = (IRSH+PARSH/PARQC) * LAISH
            RABS(3) =  IRSS + PARSS/PARQC
            IF (RADHR .GT. 0.) THEN
              PCABSR = (PCABSI*IRHR+PCABSP*PARW) / RADHR
              PCINTR = (PCINTI*IRHR+PCINTP*PARW) / RADHR
              PCREFR = (PCREFI*IRHR+PCREFP*PARW) / RADHR
            ENDIF
          ENDIF
        ELSE

C         Night time with canopy.

          FRACSH = FRSHV
          KDIRBL = KDRBLV
          LAISH = LAISHV
          LAISL = LAISLV
        ENDIF
      ELSE

C       Bare soil (day or night).

        IF (MEPHO .EQ. 'L' .OR. MEEVP .EQ. 'Z') THEN
          PCABSP = (1.0-PALB) * 100.0
          PCREFP = PALB * 100.0
          PARSS = (1.0-PALB) * PARHR
        ENDIF
        IF (MEEVP .EQ. 'Z') THEN
          PCABSR = (1.0-SALB) * 100.0
          PCREFR = SALB * 100.0
          RABS(3) = (1.0-SALB) * RADHR
        ENDIF
      ENDIF

C     Energy balance check.

      PTOT = PARSL*LAISL + PARSH*LAISH + PARSS + PARHR*PCREFP/100.0
      RTOT = RABS(1) + RABS(2) + RABS(3) + RADHR*PCREFR/100.0

      RETURN
      END SUBROUTINE RADABS

C=======================================================================
C  SHADOW, Subroutine, N.B. Pickering, J.W. Jones
C  Calculates fraction shaded for sun and row geometry using ellipses.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  03/14/91 NBP Written
C  11/15/91 NBP Modified
C  11/23/93 NBP Included more error checks and limits
C  08/20/21 CHP Added error protection
C  09/13/21 FO  Updated error call because of compiler issue.
C-----------------------------------------------------------------------
C  Called from: RADABS
C  Calls:
C=======================================================================

      SUBROUTINE SHADOW(
     &  AZIR, AZZON, BETA, BETN, CANHT, CANWH, ROWSPC,    !Input
     &  FRACSH)                                           !Output

      IMPLICIT NONE
      EXTERNAL ERROR
      SAVE

      CHARACTER*6 ERRKEY
      REAL A,B,C1,C2,C3,C4,AZIMD,AZIR,AZZON,BETA,BETN,CANHT,CANWH,ETA,
     &  FRACSH,GAMMA,PI,RAD,RBETA,ROWSPC,SHADE,SHLEN,SHPERP,STOUCH,ZERO
      PARAMETER (PI=3.14159, RAD=PI/180.0, ZERO=1.0E-6)
      PARAMETER (ERRKEY = 'SHADOW')

C     Set fraction shaded to 0.0 for zero width or height.

      IF (CANWH .LE. 0.0 .OR. CANHT .LE. 0.0) THEN
        FRACSH = 0.0

C     Set fraction shaded to 1.0 for full cover.

      ELSE IF (CANWH .GE. ROWSPC) THEN
        FRACSH = 1.0

C     Calculate fraction shaded.

      ELSE

C       Adjust BETA if sun exactly overhead or at horizon.  Calculate
C       acute positive angle between sun and row azimuths. Initialize
C       other constants.

        RBETA = MIN(MAX(BETA*RAD,1.0E-6),PI/2.0-1.0E-6)
        AZIMD = ABS(AZZON-AZIR)*RAD
        IF (AZIMD .GT. PI/2.0) AZIMD = PI - AZIMD
        A = (CANWH/CANHT)**2
        GAMMA = ATAN(A*TAN(RBETA))
        C1 = A*(TAN(RBETA))**2
        C2 = (A*TAN(RBETA))**2

C       Calculate shadow length assuming elliptical plant.

        SHLEN = CANHT * COS(RBETA-GAMMA) / SIN(RBETA) *
     &    SQRT((1.0+C2)/(1.0+C1))

!       CHP 2021-08-20
        SHLEN = MAX (0.0, SHLEN)

        B = (SHLEN/CANWH)**2
        C3 = B*(TAN(AZIMD))**2
        C4 = (B*TAN(AZIMD))**2
        STOUCH = SHLEN / (COS(AZIMD) * SQRT(1.+C3))

C       CALCULATE FRACTION SHADED.

C       Sun parallel to row.  Shadow limited to BETN.

        IF (AZIMD .LE. ZERO) THEN
          SHLEN = MIN(SHLEN,BETN)
          SHADE = 0.25 * PI * SHLEN * CANWH

C       Sun not parallel to row.

        ELSE

C         Calculate perpendicular shadow length.

          AZIMD = MAX(AZIMD,1.0E-6)
          ETA = ATAN(1.0/(B*TAN(AZIMD)))
          SHPERP = CANWH*SIN(AZIMD+ETA)*SQRT((1.0+C4)/(1.0+C3))

C         Hedgerow (plant shadows overlap).

          IF (STOUCH .GE. BETN) THEN

C           Shadow length is perpendicular and limited to ROWSPC.

            SHLEN = MIN(SHPERP,ROWSPC)
            SHADE = SHLEN * BETN

C         Individual plants.

          ELSE

C           Limit shadow length to within one ROWSPC.
C FO/GH 11/14/2020 Code protections for divisions by zero.
            IF (SHPERP .GT. 0.0 .AND. SHPERP .GT. ROWSPC) THEN
              SHLEN = SHLEN * ROWSPC/SHPERP
!            ELSE
!              SHLEN = 0.0
            ENDIF

            SHADE = 0.25 * PI * SHLEN * CANWH

          ENDIF
        ENDIF

C FO/GH 11/14/2020 Code protections for divisions by zero.
        IF(ROWSPC .GT. 0.0 .AND. BETN .GT. 0.0) THEN
          FRACSH = MIN(SHADE/(ROWSPC*BETN),1.0)
        ELSE
!         chp 2021-08-20 Added error protection.
          CALL ERROR(ERRKEY,1,'SHADOW',0)
        ENDIF

      ENDIF

!     FRACSH = MIN(MAX(FRACSH,1.0E-6),1.0)
      FRACSH = MIN(MAX(FRACSH,0.0),1.0)

      RETURN
      END SUBROUTINE SHADOW

C=======================================================================
C  LFEXTN, Subroutine, N.B. Pickering, K.J. Boote
C  Computes leaf extinction coefficients based on leaf angle distribution
C  (Goudriaan, 1988) and sunlit and shaded leaf area indices.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  ??/??/?? KJB Written
C  05/14/91 NBP Removed COMMON and reorganized.
C  11/14/20 FO/GH Code protections for divisions by zero.
C  12/03/21 FO/GH/CHP Protections to avoid negative leaf are index (LAI)
C-----------------------------------------------------------------------
C  Called from: RADABS
C  Calls:
C=======================================================================

      SUBROUTINE LFEXTN(
     &  BETA, FRACSH, LFANGD, XLAI,                      !Input
     &  KDIFBL, KDIRBL, LAISH, LAISL, RNG)                !Output

      IMPLICIT NONE
      SAVE

      REAL BETA,F15,F45,F75,FRACSH,K15,K45,K75,KDIRBL,KDIFBL,LAISH,
     &  LAISL,LFANGD(3),O15,O45,O75,OAV,PI,RAD,RNG,SINB,VARSIN,XLAI,
     &  FRAKDI
      PARAMETER (PI = 3.14159, RAD = PI/180.0)

C     Initialization.  F15, F45, and F75 are the proportion of leaves
C     in the three leaf classes: 0-30, 30-60 and 60-90 degrees.

C FO/GH 11/14/2020 Code protections for divisions by zero.
      !SINB = MAX(0.00001,SIN(BETA*RAD))
      SINB = SIN(BETA*RAD)

      F15 = LFANGD(1)
      F45 = LFANGD(2)
      F75 = LFANGD(3)

C     Calculate direct light extinction coefficient for black leaves and
C     the range of sine of incidence (used in Gaussian integration).

      O15 = MAX(0.26,0.93*SINB)
      O45 = MAX(0.47,0.68*SINB)
      O75 = 1.0 - 0.268*O15 - 0.732*O45
      OAV = F15*O15 + F45*O45 + F75*O75

C FO/GH 11/14/2020 Code protections for divisions by zero.
      IF(SINB .GT. 0.0) THEN
        KDIRBL = OAV / SINB
      ELSE
        KDIRBL = 0.0
      ENDIF

      VARSIN = 0.06*F15+0.25*F45+0.467*F75 +
     +  (0.81*F15 + 0.25*F45 - 0.4*F75)*SINB**2 - OAV**2
      VARSIN = MAX(VARSIN,0.0)
      RNG = SQRT(12.0*VARSIN)

C     Calculate diffuse light extinction coefficient for black leaves.

      K15 = 1.00*F15 + 1.82*F45 + 2.26*F75
      K45 = 0.93*F15 + 0.68*F45 + 0.67*F75
      K75 = 0.93*F15 + 0.68*F45 + 0.29*F75

      IF(XLAI .GT. 0.0) THEN
        KDIFBL = -ALOG(0.25*EXP(-K15*XLAI)+0.5*EXP(-K45*XLAI)
     &           +0.25*EXP(-K75*XLAI)) / XLAI
      ELSE
        KDIFBL = 0.0
      ENDIF

C     Calculate sunlit and shaded leaf area indices.
!CHP added check to prevent underflow 1/16/03
C FO/GH 11/14/2020 Code protections for divisions by zero.
      IF (KDIRBL .GT. 0.0 .AND. FRACSH .GT. 0.0) THEN
        !LAISL = (FRACSH/KDIRBL) * (1.0-EXP(-KDIRBL*XLAI/FRACSH))
        FRAKDI = FRACSH/KDIRBL
        LAISL = FRAKDI * (1.0-EXP(-XLAI/FRAKDI))
      ELSE
        LAISL = 0.0
      ENDIF

      LAISL = MIN(LAISL,XLAI)
C-KRT*******************************
C-KRT  LAISH = XLAI - LAISL
!-CHP  LAISH = MAX(0.02,XLAI - LAISL)
C FO/GH 11/14/2020 Code protections for divisions by zero.
      LAISH = MAX(0.0, XLAI - LAISL)

C-KRT*******************************
      RETURN
      END SUBROUTINE LFEXTN

C=======================================================================
C  CANABS, Subroutine, K.J. Boote, N.B. Pickering
C  Computes radiation absorbed by soil, sunlit and shaded leaves.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  ??/??/?? KJB Written
C  05/14/91 NBP Removed COMMON and reorganized.
C  12/03/21 FO/GH/CHP Protections to compute diffuse/scattered
C               components of the direct beam.
C-----------------------------------------------------------------------
C  Called from: RADABS
C  Calls:
C=======================================================================

      SUBROUTINE CANABS(
     &  ALBEDO, BETA, BETN, CANHT, CANWH, FRACSH,
     &  FRDIF, KDIFBL, KDIRBL, LAISH, LAISL, RADHR,
     &  RNG, ROWSPC, SCVR,
     &  PCTABS, PCTINT, PCTREF, RADSH, RADSS, RADSUN)

      IMPLICIT NONE
      SAVE

      INTEGER I
      REAL ADDF,ADDR,ADIF,ADIR,AREF,ATOT,ADIRSL,ADDRSL,ADDFSL,
     &  ADDFSH,ADIFSL,ADIFSH,AREFSL,AREFSH,RDIFSL,ALBEDO,BETA,
     &  BETN,CANHT,CANWH,DELWP,DELWR,DIFP,DIFPR,DIFR,FRACSH,FRDIF,
     &  INCSOI,INTCAN,KDIFBL,KDIRBL,LAISH,LAISL,O,OAV,PATHP,PATHR,
     &  PCTABS,PCTINT,PCTREF,RADDIF,RADDIR,RADHR,     PI,RAD,
     &  RADSH,RADSS,RADSUN(3),REFDF,REFDIF,REFDIR,REFDR,REFH,
     &  REFSOI,REFTOT,RNG,ROWSPC,SCVR,SINB,SQV,XLAI,RADTOT

      PARAMETER (PI=3.14159, RAD=PI/180.0)

C     Initialization.

      SQV = SQRT(1.0-SCVR)
      SINB = SIN(BETA*RAD)
      XLAI = LAISH + LAISL

C     Compute reflection coefficients for direct and diffuse light.

      REFH = (1.0-SQV) / (1.0+SQV)
      REFDR = 2.0*KDIRBL/(1.0+KDIRBL) * REFH
      REFDF = REFH

C     Split total radiation into direct and diffuse components.

      RADDIF = FRDIF * RADHR
      RADDIR = RADHR - RADDIF

C     Direct radiation absorbed in shaded zone by considering the direct
C     (ADDR) and diffuse/scattered (ADDF) components of the direct beam.

!     CHP - Added checks to prevent underflows 1/16/03
      IF (FRACSH .GT. 0.0) THEN
!        IF ((KDIRBL*SQV*XLAI/FRACSH) .LT. 20.) THEN
          ADIR = FRACSH * (1.0-REFDR) * RADDIR *
     &    (1.0-EXP(-KDIRBL*SQV*XLAI/FRACSH))
!        ELSE
!          ADIR = 0.0
!        ENDIF

!        IF ((KDIRBL*XLAI/FRACSH) .LT. 20.) THEN
          ADDR = FRACSH * (1.0-SCVR) * RADDIR *
     &    (1.0-EXP(-KDIRBL*XLAI/FRACSH))
!        ELSE
!          ADDR = 0.0
!        ENDIF
C-KRT****************************
C-KRT   ADDF = ADIR - ADDR
!-CHP   ADDF = MAX(0.0,ADIR-ADDR)
!-FO        ADDF = ADIR - ADDR

        ADDR = MIN(ADDR, ADIR)
        ADDF = MAX(0.0,ADIR-ADDR)

C-KRT****************************
!        IF ((KDIRBL*SQV*LAISL/FRACSH) .LT. 20.) THEN
          ADIRSL = FRACSH * (1.0-REFDR) * RADDIR *
     &    (1.0-EXP(-KDIRBL*SQV*LAISL/FRACSH))
!        ELSE
!          ADIRSL = 0.0
!        ENDIF

!        IF ((KDIRBL*LAISL/FRACSH) .LT. 20.) THEN
          ADDRSL = FRACSH * (1.0-SCVR) * RADDIR *
     &    (1.0-EXP(-KDIRBL*LAISL/FRACSH))
!        ELSE
!          ADDRSL = 0.0
!        ENDIF
C-KRT************************************
C-KRT   ADDFSL = ADIRSL - ADDRSL
C-KRT   ADDFSH = ADDF - ADDFSL
!-CHP   ADDFSL = MAX(0.0,ADIRSL - ADDRSL)
!-CHP   ADDFSH = MAX(0.0,ADDF - ADDFSL)
!-FO        ADDFSL = ADIRSL - ADDRSL

        ADDRSL = MIN(ADDRSL, ADIRSL)
        ADDFSL = MAX(0.0,ADIRSL - ADDRSL)


!-FO        ADDFSH = ADDF - ADDFSL

        ADDFSL = MIN(ADDFSL, ADDF)
        ADDFSH = MAX(0.0,ADDF - ADDFSL)

C-KRT************************************
      ELSE
        ADIR   = 0.0
        ADDR   = 0.0
        ADDF   = 0.0
        ADIRSL = 0.0
        ADDRSL = 0.0
        ADDFSL = 0.0
        ADDFSH = 0.0
      ENDIF

C     Diffuse skylight is absorbed over an effective area equal to the
C     canopy height plus width for an isolated row.  For interfering rows,
C     Eqns. 2.130 and 2.128 from Goudriaan (1977) are used.  Concept
C     extended for both between plants (P) and rows (R).

      IF (CANWH .LT. BETN) THEN
        PATHP = BETN - CANWH
        DELWP = PATHP + CANHT - SQRT(PATHP**2+CANHT**2)
        DIFP = MIN((CANWH+DELWP)/BETN,1.0)
      ELSE
        DIFP = 1.0
      ENDIF
      IF (CANWH .LT. ROWSPC) THEN
        PATHR = ROWSPC - CANWH
        DELWR = PATHR + CANHT - SQRT(PATHR**2+CANHT**2)
        DIFR = MIN((CANWH+DELWR)/ROWSPC,1.0)
      ELSE
        DIFR = 1.0
      ENDIF
      DIFPR = MIN(MAX(DIFP*DIFR,1.0E-6),1.0)
      ADIF = DIFPR * (1.0-REFDF) * RADDIF *
     &  (1.0-EXP(-KDIFBL*SQV*XLAI/DIFPR))
      ADIFSL = DIFPR * (1.0-REFDF) * RADDIF *
     &  (1.0-EXP(-KDIFBL*SQV*LAISL/DIFPR))
C-KRT********************************
C-KRT ADIFSH = ADIF - ADIFSL
!-CHP ADIFSH = MAX(0.0,ADIF - ADIFSL)
!-FO      ADIFSH = ADIF - ADIFSL

      ADIFSL = MIN(ADIFSL, ADIF)
      ADIFSH = MAX(0.0,ADIF - ADIFSL)

C-KRT********************************

C     Light reflected from the soil assumed to be isotropic and diffuse.
C     Absorption handled in the same manner as diffuse skylight.

      REFDIR = FRACSH * REFDR * RADDIR
      REFDIF = DIFPR * REFDF * RADDIF
      INTCAN = REFDIR + REFDIF + ADIR + ADIF
      INCSOI = RADHR - INTCAN
      REFSOI = ALBEDO * INCSOI
      AREF = DIFPR * (1.0-REFDF) * REFSOI *
     &  (1.0-EXP(-KDIFBL*SQV*XLAI/DIFPR))
      AREFSH = DIFPR * (1.0-REFDF) * REFSOI *
     &  (1.0-EXP(-KDIFBL*SQV*LAISH/DIFPR))
C-KRT********************************
C-KRT AREFSL = AREF - AREFSH
!-CHP AREFSL = MAX(0.0,AREF - AREFSH)
!-FO      AREFSL = AREF - AREFSH

      AREFSH = MIN(AREFSH, AREF)
      AREFSL = MAX(0.0,AREF - AREFSH)

C-KRT********************************
      ATOT = ADIR + ADIF + AREF
      REFTOT = REFDIR + REFDIF + REFSOI - AREF

C     Determine sunlit radiation for each sunlit leaf class,
C     shaded leaves and soil.  The average sunlit radiation =RABS(2).

      RADSS = INCSOI * (1.0-ALBEDO)
C     RADSH = (ADDF+ADIF+AREF) / XLAI
C FO/GH 11/14/2020 Code protections for divisions by zero.
      IF(LAISH .GT. 0.0) THEN
          RADSH = (ADDFSH+ADIFSH+AREFSH) / LAISH
      ELSE
          RADSH = 0.0
      ENDIF

C FO/GH 11/14/2020 Code protections for divisions by zero.
      IF(LAISL .GT. 0.0) THEN
          RDIFSL = (ADDFSL+ADIFSL+AREFSL) / LAISL
      ELSE
          RDIFSL = 0.0
      ENDIF

      OAV = KDIRBL * SINB
C FO/GH 11/14/2020 Code protections for divisions by zero.
      IF(SINB .GT. 0.0) THEN
        DO I=1,3
          O = OAV + SQRT(0.15)*RNG*(2-I)
C       RADSUN(I) = RADSH + (1.-SCVR)*RADDIR*O/SINB
          RADSUN(I) = RDIFSL + (1.-SCVR)*RADDIR*O/SINB
        ENDDO
      ELSE
          RADSUN = 0.0
      ENDIF

C     Set radiation array and calculate ratios of components.

      IF (RADHR .GT. 0.0) THEN
        PCTINT = 100.0 * INTCAN / RADHR
        PCTABS = 100.0 * ATOT / RADHR
        PCTREF = 100.0 * REFTOT / RADHR
      ELSE
        PCTINT = 0.0
        PCTABS = 0.0
        PCTREF = 0.0
      ENDIF

C     Energy balance check (RADTOT=RADHR).

      RADTOT = ATOT + REFTOT + RADSS
      RADTOT = RADSH*LAISH + RADSUN(2)*LAISL + REFTOT + RADSS

C FO/GH 11/14/2020 Code protections for divisions by zero.
      IF(SINB .GT. 0.0) THEN
        RADTOT = RADSH*LAISH + RDIFSL*LAISL + REFTOT + RADSS +
     &  (1.0-SCVR)*RADDIR*OAV/SINB*LAISL
      ENDIF


      RETURN
      END SUBROUTINE CANABS

C=======================================================================
C  SOIL10, Subroutine, N.B. Pickering
C  Converts from soil layers of 5,15.. cm to 10,10.. cm.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  03/13/94 NBP Written
C-----------------------------------------------------------------------
C  Called from: ETIND,ETINP
C  Calls:
C=======================================================================

      SUBROUTINE SOIL10(
     &  ARRAY, ICODE, NLAYR,                              !Input
     &  ARRAY2)                                           !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types,
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      INTEGER I,ICODE,L,NLAYR
      REAL ARRAY(NL),ARRAY2(NL)

      DO I=1,NL
        ARRAY2(I) = 0.0
      ENDDO
      IF (ICODE .EQ. 1) THEN
        ARRAY2(1) = 10.0
        ARRAY2(2) = 10.0
      ELSE
        ARRAY2(1) = 0.5 * (ARRAY(1)+ARRAY(2))
        ARRAY2(2) = ARRAY(2)
      ENDIF
      DO L=3,NLAYR
        ARRAY2(L) = ARRAY(L)
      ENDDO

      RETURN
      END SUBROUTINE SOIL10

C=======================================================================
C  SOIL05, Subroutine, N.B. Pickering
C  Converts from soil layers of 10,10.. cm to 5,15.. cm.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  03/13/94 NBP Written
C-----------------------------------------------------------------------
C  Called from: ETPHOT
C  Calls:
C=======================================================================

      SUBROUTINE SOIL05(
     &  ARRAY2,ICODE, NLAYR,                              !Input
     &  ARRAY)                                            !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types,
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      INTEGER I,ICODE,L,NLAYR
      REAL ARRAY(NL),ARRAY2(NL)

      DO I=1,NL
        ARRAY(I) = 0.0
      ENDDO
      IF (ICODE .EQ. 1) THEN
        ARRAY(1) =  5.0
        ARRAY(2) = 15.0
      ELSE
        ARRAY(1) = ARRAY2(1)
        ARRAY(2) = (ARRAY2(1)+2.0*ARRAY2(2)) / 3.0
      ENDIF
      DO L=3,NLAYR
        ARRAY(L) = ARRAY2(L)
      ENDDO

      RETURN
      END SUBROUTINE SOIL05

C=======================================================================



