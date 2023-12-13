C=======================================================================
C  OXLAYER, Subroutine, U. Singh
C  Determines Oxidised Layer chemistry
C-----------------------------------------------------------------------
C  REVISION HISTORY
C             US  Written
C  03/29/2002 CHP modular format
!  03/07/2005 CHP Fixed check for LDF10
!  07/24/2006 CHP Use MSALB instead of SALB (includes mulch and soil 
!                 water effects on albedo)
C=======================================================================

      SUBROUTINE OXLAYER (CONTROL,
     &    BD1, ES, FERTDATA, FLOODWAT, LFD10,             !Input
     &    NSWITCH, SNH4, SNO3, SOILPROP, SRAD, ST,        !Input
     &    SW, TMAX, TMIN, UREA, XHLAI,                    !Input
     &    DLTSNH4, DLTSNO3, DLTUREA, OXLAYR,              !I/O
     &    ALI, TOTAML)                                    !Output

      USE ModuleDefs
      USE FloodModule
      IMPLICIT NONE
      EXTERNAL EQUIL2
      SAVE

      CHARACTER*1 RNMODE

      INTEGER  NSWITCH, YRDOY, YRDRY, DYNAMIC
      INTEGER  RUN

      REAL    FPI, BD1, SW1
      REAL    ALGACT,AMLOSS,OXRNTRF
      REAL    ALI
      REAL    OXLT,OXFAC,OXU,OXN3,OXN3C,OXH4,OXH4C
      REAL    OXN,OXNI
      REAL    TOTAML
      INTEGER IHDAY,LFD10, IBP

!     UNINCO: Fertilizer not fully incorporated; if true, ignore oxlayer
      LOGICAL DailyCalc,UNINCO
      REAL    WFP
      REAL    OXMIN3, OXMIN4
      REAL    KG2PPM(NL)
      REAL    SURCEC

!     Local Variables
      INTEGER  IST,IHR,I,K
      REAL     SURAD,OXALI,STI,SWI,AMPES,AK,HTMFAC,MF,TK
      REAL     STEMP,HES,PHSHIFT,TEMPFU,XL,XL2,SWF,UALGCT,OXUHYDR,OXPH1
      REAL     OXUHYDC,OXUHYDM,PHOXUHY,OXPH,OXMINC,OXH3C,OXH3,WIND
      REAL     ALOGHK,HK,OXH3M,OXH3P,AMLOS1,GLOS1,TFACTOR,OXNC,WF2
      REAL     PHFACT,RNTRFI,HOXRNT,ELAG

!     Passed Variables
      INTEGER  FERTYPE
      REAL     UREA(NL),SNH4(NL),SNO3(NL),SRAD,MSALB, XHLAI
      REAL     SW(NL),SAT(NL),ST(NL),DUL(NL),ES,TMIN,TMAX
      REAL     LL(NL),PH(NL),DLAYR(NL)
      REAL     OC(NL)

!     Additions to oxidation layer from today's fertilizer:
      REAL ADDOXH4, ADDOXN3, ADDOXU

      TYPE (ControlType)  CONTROL
      TYPE (SoilType)     SOILPROP
      TYPE (FloodWatType) FloodWat
      TYPE (OxLayerType)  OXLAYR
      TYPE (FertType)     FERTDATA

      REAL DLTUREA(NL),  DLTSNO3(NL),  DLTSNH4(NL)
      REAL DLTOXU, DLTOXH4, DLTOXN3
      REAL TMPUREA, TMPNH4,  TMPNO3
      REAL TMPOXU, TMPOXH4, TMPOXN3

!-----------------------------------------------------------------------
      DYNAMIC = CONTROL % DYNAMIC
      RNMODE  = CONTROL % RNMODE 
      RUN     = CONTROL % RUN    
      YRDOY   = CONTROL % YRDOY  

      YRDRY = FLOODWAT % YRDRY

      DLAYR = SOILPROP % DLAYR
      SURCEC= SOILPROP % CEC(1)
      DUL   = SOILPROP % DUL
      KG2PPM= SOILPROP % KG2PPM
      LL    = SOILPROP % LL
      OC    = SOILPROP % OC
      PH    = SOILPROP % PH
!     SALB  = SOILPROP % SALB
      MSALB  = SOILPROP % MSALB
      SAT   = SOILPROP % SAT

      SW1 = SW(1)

      DailyCalc   = OXLAYR % DailyCalc
      DLTOXH4 = OXLAYR % DLTOXH4
      DLTOXN3 = OXLAYR % DLTOXN3
      DLTOXU  = OXLAYR % DLTOXU
      OXLT    = OXLAYR % OXLT
      OXH4    = OXLAYR % OXH4
      OXN3    = OXLAYR % OXN3
      OXU     = OXLAYR % OXU
      OXMIN3  = OXLAYR % OXMIN3
      OXMIN4  = OXLAYR % OXMIN4
      IBP     = OXLAYR % IBP

      ADDOXH4 = FERTDATA % ADDOXH4
      ADDOXN3 = FERTDATA % ADDOXN3
      ADDOXU  = FERTDATA % ADDOXU
      FERTYPE = FERTDATA % FERTYPE
!      LFD10   = FERTDATA % LFD10
      UNINCO  = FERTDATA % UNINCO

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      FPI = 0.50

      !Initialize for non-sequenced runs
      IF (INDEX('FQ',RNMODE) .LE. 0 .OR. RUN .EQ. 1) THEN

        OXLT  = 0.50 - 0.1*OC(1)
        OXLT  = MAX(0.001,OXLT)
        OXLAYR % OXLT = OXLT
        OXFAC = 1.0/(BD1*OXLT*1.0E-01)

        OXU   = UREA(1) * KG2PPM(1) / OXFAC
        OXN3  = SNO3(1) * KG2PPM(1) / OXFAC
        OXH4  = SNH4(1) * KG2PPM(1) / OXFAC
        OXNI  = 0.05

        TOTAML = 0.0
        DailyCalc = .TRUE.

        OXPH = PH(1)      !chp 1/27/2004
      ENDIF

      ALGACT = 0.1

!***********************************************************************
!***********************************************************************
!     Daily rate calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
      OXFAC = 1.0/(BD1*OXLT*1.0E-01)

      TMPUREA = UREA(1) + DLTUREA(1)
      TMPNO3  = SNO3(1) + DLTSNO3(1)
      TMPNH4  = SNH4(1) + DLTSNH4(1)

      TMPOXU  = OXU  + DLTOXU  + ADDOXU
      TMPOXN3 = OXN3 + DLTOXN3 + ADDOXN3 
      TMPOXH4 = OXH4 + DLTOXH4 + ADDOXH4

      !FAC1 = 1.0 / (BD1 * 1.E-01 * DLAYR(1))
      !NO3_1  = SNO3(1) * FAC1
      !NH4_1  = SNH4(1) * FAC1
      !UPPM_1 = UREA(1) * FAC1

      OXH4C   = TMPOXH4   * OXFAC
      OXN3C   = TMPOXN3   * OXFAC

!     If fertilizer incorporated, ignore oxidation layer 
!         (UNINCO set in FPLACE)
      IF (.NOT. UNINCO) RETURN

!     LFD10 - 10 days after last fertilizer application (set in FPLACE)
      IF (YRDOY < LFD10) THEN
         DailyCalc = .FALSE.
      ELSE
         DailyCalc = .TRUE.
      ENDIF

      TMPOXU    = AMIN1 (TMPOXU , TMPUREA)
      TMPOXH4   = AMIN1 (TMPOXH4, TMPNH4)
      TMPOXN3   = AMIN1 (TMPOXN3, TMPNO3)

!     Compute algal activity - for saturated soil
!      SURAD = SRAD*(1.0-SALB)*EXP(-0.85*XHLAI)
      SURAD = SRAD*(1.0-MSALB)*EXP(-0.85*XHLAI)
      OXALI = 1.0 - EXP(-SURAD/10.0)

      IF (SW(1) .EQ. SAT(1)) THEN
         ALI = 1.0 - EXP(-SURAD/10.0)
      ELSE
         ALI = 1.0
      ENDIF

      IF (ST(1) .LT. 30.0)  THEN
         STI = (ST(1)-15.0)*0.1
      ELSEIF (ST(1) .GE. 30.0) THEN
         STI = 1.0-(ST(1)-30.0)*0.05
      ENDIF

      STI = AMIN1 (STI,1.0)
      SWI = 1.0
      IF (SW(1) .LT. DUL(1)) THEN
         SWI = (SAT(1)-DUL(1))/(SAT(1)-SW(1))
      ENDIF

!     Biological and chemical activity of "OXIDIZED" layer
      ELAG   = AMIN1(OXNI,FPI,ALI,STI,SWI)
      ALGACT = ELAG*(4.0-ALGACT)*ALGACT           ! 4->3.5
      IF (XHLAI .GT. 1.5) THEN
         ALGACT = AMIN1(ALGACT,ALI)
      ENDIF
      ALGACT = AMIN1 (ALGACT,1.00)
      ALGACT = AMAX1 (ALGACT,0.05)

!     Partition soil evaporation
!     AMPES(hr) is 0.35*ES(daily) chamged to 0.45
      AMPES   = 0.38*ES                           ! 0.35->0.38
      AMLOSS  = 0.0
      OXRNTRF = 0.0
      IF (ALGACT .GE. 0.2 .OR. YRDOY .LT. LFD10) THEN
        DailyCalc = .FALSE.
      ENDIF

      IF (DailyCalc) THEN
         IHDAY = 1
         IST   = 6
         IHR   = 6
      ELSE
         IHDAY = 12
         IST   = 1
         IHR   = 12
      ENDIF

!     Potential Hydrolysis
!     AK = -1.12+0.31*OC(1)+0.203*PH(1)-0.0355*OC(1)*PH(1)
      AK = 0.25+0.075*OC(1)
      AK = AMAX1 (AK,0.25)
      AK = AMIN1 (AK,1.00)

      DO I = IST, IHR
         K      = 7 - I
         IF (I .GT. 6) THEN
            K = I - 6
         ENDIF
         HTMFAC = 0.931+0.114*K-0.0703*K**2+0.0053*K**3

!        Hourly soil surface temp
         STEMP  = TMIN + HTMFAC*(TMAX+2.0-TMIN)
         IF (I .EQ. 13) THEN
            STEMP = TMIN
         ENDIF

!        Calculate hourly soil evaporation (HES)
         IF (I .LE. 6 .OR. I .GT. 9) THEN
            HES = AMPES*SIN(3.141593*FLOAT(I)/12.0)+0.08    ! 10->9
         ENDIF

         HES = AMAX1 (ES/24.0,ABS(HES))

         ! (1)  Calculate indices for bio-chemical activity
         IF (I. EQ. 6) THEN
             OXNI = (OXH4C+OXN3C)/10.0+0.10
         ENDIF
         OXNI = AMIN1 (OXNI,1.0)

         ! BIOACT = AMIN1(1.0,5.0*ALGACT)
         PHSHIFT = 0.75 + 2.0*ALGACT

         SELECT CASE (FERTYPE)
           CASE (0:3,8,11:50)
             IF (OXH4C .LE. OXN3C) THEN
                PHSHIFT = 0.6
             ENDIF
           CASE DEFAULT
             ! Just go on to UREA hydrolysis
         END SELECT

!        UREA hydrolysis function of surface layer OC or
!        biological activity, whichever is greater.
         TEMPFU  = 0.04*STEMP - 0.2
         TEMPFU  = AMIN1 (TEMPFU,1.0)
         TEMPFU  = AMAX1 (TEMPFU,0.0)
         XL      = DUL(1) - LL(1)*0.5
         XL2     = SW(1)  - LL(1)*0.5
         MF      = XL2/XL
         MF      = AMAX1 (MF,0.0)
         SWF     = MF + 0.20
         IF (SW(1) .GE. DUL(1)) THEN
            SWF = 1.0
         ENDIF
         SWF     = AMIN1 (SWF,1.0)
         SWF     = AMAX1 (SWF,0.0)
         UALGCT  = 0.25*ALGACT
         OXUHYDR = AMAX1 (AK,UALGCT)*AMIN1 (SWF,TEMPFU)*TMPOXU

!        CHP added this check, but still get negative urea 
!        need to overhaul this routine - prevent negative values for 
!        all N species.
         OXUHYDR = AMIN1(TMPUREA, OXUHYDR)  !CHP 5/4/2010

         TMPOXU     = TMPOXU     - OXUHYDR
         TMPOXH4    = TMPOXH4    + OXUHYDR
         TMPUREA = TMPUREA - OXUHYDR
         TMPNH4  = TMPNH4  + OXUHYDR

         IF (I .LE. 6) THEN
            OXPH1 = PH(1)+PHSHIFT*SIN(3.1412*FLOAT(I)/12.0)   ! 8->11
         ENDIF

!        Add effects of UREA hydrolysis on surface layer pH here
         IF (MF .GT. 1.5 .AND. OXPH1 .LT. 7.2) THEN
            OXPH1 = 7.2
         ENDIF
         IF (OXUHYDR .GT. 0.001) THEN
            OXUHYDC = OXUHYDR*OXFAC
            OXUHYDM = OXUHYDC*0.001/14.0          ! (Molar conc.)
            PHOXUHY = AMIN1 (10.0,-LOG10(OXUHYDM))
            OXPH    = OXPH1 + OXALI*(10.0-PHOXUHY)/10.0
         ENDIF
         OXPH = AMIN1 (OXPH,9.0)
         OXPH = AMAX1 (OXPH,PH(1))

!        AMMONIA loss routine ... calculate surface layer NH3
         TK     = STEMP + 273.15
         OXH4C  = TMPOXH4  * OXFAC
         OXMINC = OXMIN4* OXFAC

!        CALL AMTHERM (OXH4C,OXH4SC,BD(1),CEC,2,BPOXL,OXMINC)
!        OXH4SC = AMIN1(OXH4SC/DUL(1),OXH4C)
         IF (I .LE. 6 .OR. OXH3C .GE. OXH4C) THEN
            OXH3C = OXH4C/(1.0+10.0**(0.09018+2729.92/TK-OXPH))
         ENDIF

!        Calculate ammonia (Mass) using inverse of (KG2PPM) OXLT in cm)
         IF (OXH3C .LE. 0.00001 .AND. OXH3C .GT. 0.0) THEN
            OXH3C = 0.0
         ENDIF
         OXH3    = OXH3C/OXFAC
         IF (OXH3 .GT. (TMPOXH4-OXMIN4)) THEN
            OXH3  = TMPOXH4 - OXMIN4
            OXH3C = OXH3 * OXFAC
         ENDIF

         WIND    = 7.15*HES           ! 7.15 -> 5.75
         ALOGHK  = 158.559-8621.06/TK-25.6767*ALOG(TK)+0.035388*TK 
         HK      = EXP(ALOGHK)
         OXH3M   = OXH3C*0.001/14.0
         OXH3P   = 10.0*OXH3M/HK                 
         IF (OXH3P .GT. 0.0) THEN
            AMLOS1  = 0.0012*OXH3P+0.0014*WIND+0.00086*OXH3P**2*WIND
         ENDIF
         IF (NSWITCH .EQ. 8) THEN
            AMLOS1 = 0.0
         ENDIF
         IF (OXH3P .LE. 0.0) THEN
            AMLOS1 = 0.0
         ENDIF
         GLOS1   = AMLOS1
         AMLOS1  = AMIN1 (AMLOS1,TMPOXH4-OXMIN4)
         AMLOSS  = AMLOSS + AMLOS1
         TOTAML  = TOTAML + AMLOS1
         TMPOXH4    = TMPOXH4   - AMLOS1
         OXH4C   = TMPOXH4   * OXFAC
         TMPNH4  = TMPNH4 - AMLOS1

!        Nitrification section
         OXN     = TMPOXH4 + TMPOXU + TMPOXN3
         OXNC    = OXN*OXFAC
         TFACTOR = EXP(-6572 / TK + 21.4)

         WFP = SW(1) / SAT(1)
         IF (SW(1) .GT. DUL(1)) THEN
             WF2 = -2.5 * WFP + 2.55
         ELSE
             IF (WFP .GT. 0.4) THEN
                WF2 = 1.0
             ELSE
                WF2 = 3.15 * WFP - 0.1
             ENDIF
         ENDIF
         WF2     = AMAX1 (WF2, 0.0)
         WF2     = AMIN1 (1.0, WF2)
         PHFACT  = 0.3*OXPH - 1.8
         PHFACT  = AMIN1 (PHFACT,1.0)
         PHFACT  = AMAX1 (PHFACT,0.0)
         RNTRFI  = TFACTOR*WF2*PHFACT/13.0
         RNTRFI  = AMIN1 (RNTRFI,0.90)
         HOXRNT  = RNTRFI*TMPOXH4*OXLT/DLAYR(1)
         IF (OXMIN4 .GT. TMPOXH4) THEN
            TMPOXH4   = OXMIN4
         ENDIF
         IF (TMPOXH4-HOXRNT .LE. OXMIN4) THEN
            HOXRNT = TMPOXH4 - OXMIN4
         ENDIF
         IF (HOXRNT .GT. -0.00001 .AND. HOXRNT .LT. 0.00001) THEN
            HOXRNT = 0.0
         ENDIF

!        HOXRNT  = AMIN1 (HOXRNT,SNH4(1)-SMIN4(1))
         HOXRNT  = AMIN1 (HOXRNT,TMPOXH4)
         TMPNH4  = TMPNH4  - HOXRNT
         TMPNO3  = TMPNO3  + HOXRNT
         TMPOXH4    = TMPOXH4    - HOXRNT
         TMPOXN3    = TMPOXN3    + HOXRNT
         OXRNTRF = OXRNTRF + HOXRNT

!        Recompute equilibria after nitrification/denitrification
         CALL EQUIL2 (
     &     BD1, SURCEC, DLAYR, 0.0, IHDAY, 1,          !Input
     &     OXLT, OXMIN3, OXMIN4, SW1, YRDOY, YRDRY,    !Input
     &     IBP, 0.0, TMPOXU, TMPUREA)                  !I/O 

         CALL EQUIL2 (
     &     BD1, SURCEC, DLAYR, 0.0, IHDAY, 2,          !Input
     &     OXLT, OXMIN3, OXMIN4, SW1, YRDOY, YRDRY,    !Input
     &     IBP, 0.0, TMPOXN3, TMPNO3)                  !I/O 

         CALL EQUIL2 (
     &     BD1, SURCEC, DLAYR, 0.0, IHDAY, 3,          !Input
     &     OXLT, OXMIN3, OXMIN4, SW1, YRDOY, YRDRY,    !Input
     &     IBP, 0.0, TMPOXH4, TMPNH4)                  !I/O 
      END DO

!     Surface variables
      DLTUREA(1) = MAX(0.0, TMPUREA) - UREA(1)
      DLTSNO3(1) = MAX(0.0, TMPNO3)  - SNO3(1)
      DLTSNH4(1) = MAX(0.0, TMPNH4)  - SNH4(1)

      DLTOXU  = MAX(0.0, TMPOXU)  - OXU
      DLTOXH4 = MAX(0.0, TMPOXH4) - OXH4
      DLTOXN3 = MAX(0.0, TMPOXN3) - OXN3

!***********************************************************************
!***********************************************************************
!     DAILY INTEGRATION (also performed for seasonal initialization)
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
!     Oxidation layer integration
      OXU  = OXU  + DLTOXU
      OXH4 = OXH4 + DLTOXH4
      OXN3 = OXN3 + DLTOXN3

      OXLAYR % OXU  = OXU 
      OXLAYR % OXH4 = OXH4
      OXLAYR % OXN3 = OXN3

      DLTOXU  = 0.0
      DLTOXH4 = 0.0
      DLTOXN3 = 0.0

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      OXLAYR % DailyCalc   = DailyCalc
      OXLAYR % DLTOXH4 = DLTOXH4
      OXLAYR % DLTOXN3 = DLTOXN3
      OXLAYR % DLTOXU  = DLTOXU
      OXLAYR % OXU  = OXU 
      OXLAYR % OXH4 = OXH4
      OXLAYR % OXN3 = OXN3
      OXLAYR % IBP  = IBP

      RETURN
      END SUBROUTINE OXLAYER

