C=======================================================================
C  FCHEM, Subroutine, U. Singh
C  Determines floodwater chemistry
C-----------------------------------------------------------------------
C  REVISION HISTORY
C             US  Written
C  03/13/2002 CHP added to modular model.
C                 changed IMMOB to IMMOBIL because name taken in NTRANS.
C  01/14/2005 CHP Added initialization for ALI, FTI, FPH
!  02/21/2006 CHP Limit flood N species to non-negative values.
!  03/02/2006 CHP ALGFON instead of DLTFON for N content of algae
C-----------------------------------------------------------------------
! Called by: Flood_chem
! Calls:     OPFLOODN, EQUIL2
C=======================================================================

      SUBROUTINE FCHEM (CONTROL, ISWITCH,
     &    BD1, SURCEC, DLAYR, EF, FLDH4, FLDN3, FLDU,     !Input
     &    FLOOD, LFD10, NSWITCH, OC, OXMIN3, OXMIN4,      !Input
     &    OXU, OXH4, OXN3, OXLT, PH, SNH4, SNO3, SRAD,    !Input
     &    SW1, TMAX, TMIN, UREA, XHLAI, YRDOY, YRDRY,     !Input
     &    AKILL, ALGFON, DailyCalc, DLTSNH4, DLTSNO3,     !I/O
     &    DLTUREA, DLTFUREA, DLTFNO3, DLTFNH4, DLTOXU,    !I/O
     &    DLTOXH4, DLTOXN3, IBP, IMMOBIL,                 !I/O
     &    ALGFIX, ALI, FNI, TOTAML)                       !Output

      USE ModuleDefs
      USE FloodModule
      IMPLICIT NONE
      EXTERNAL OPFLOODN, EQUIL2
      SAVE

      INTEGER NSWITCH, YRDRY, DYNAMIC
      REAL    BD1, FLDU, FLDH4,FLDH4C,FLDN3,FLDN3C
      REAL    FLDNI,FLDNC,FPI,FLDH3C
      REAL    ALGACT,FPH,AMLOSS,OXRNTRF
      REAL    FUHYDR, ALI, FNI, FTI, SURCEC
      REAL    OXLT,OXFAC,OXU,OXUC,OXN3,OXN3C,OXH4,OXH4C
      REAL    ALGFIX
      REAL    TOTAML
      INTEGER IHDAY,LFD10, IBP
      LOGICAL AKILL, DailyCalc, IMMOBIL
      REAL    EF,FLOOD
      REAL    OXMIN3, OXMIN4

      INTEGER YRDOY, KILL,IST,IHR,I,K
      REAL    ALGALN,WALB,FRAD,AK,AMPEF,FTEMP,TK
      REAL    YALGA,TOTUH,HTMFAC,HEF,PHSHIFT,TEMPFU,UALGCT,FUHYDRC
      REAL    FUHYDRM,PHFUHYD,FPH1,FLDH3,WIND,ALOGHK,HK,FNH3M,FNH3P
      REAL    AMLOS1,GLOS1,TFACTOR,WF2,PHFACT,RNTRFI,HOXRNT,XMIN
      REAL    ELAG, SW1
      REAL    OC(NL)
      REAL    XHLAI, PH(NL), SNH4(NL), DLAYR(NL)
      REAL    SNO3(NL), SRAD, UREA(NL), TMIN, TMAX

      REAL DLTFUREA, DLTFNO3, DLTFNH4
      REAL DLTOXU, DLTOXH4, DLTOXN3
      REAL TMPFUREA, TMPFNO3, TMPFNH4
      REAL TMPOXU, TMPOXH4, TMPOXN3
      REAL DLTUREA(NL),  DLTSNO3(NL),  DLTSNH4(NL)
      REAL TMPUREA,  TMPNO3,  TMPNH4
      REAL FAC1 
      REAL ALGFON 

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH

      DYNAMIC = CONTROL % DYNAMIC

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!     Need initialization section here 
      TOTAML  = 0.0
      FNI     = 0.1
      FPI     = 0.5
      IMMOBIL = .FALSE.
      YALGA   = 0.1
      ALGFIX  = 0.0
      ALGACT  = 0.1

      ALI     = 1.0
      FTI     = 1.0
      FPH     = 7.0

      CALL OPFLOODN (CONTROL, ISWITCH, 
     &    ALGACT, ALI, AMLOSS, BD1, EF, FLDH3C, FLDH4, 
     &    FLDH4C, FLDN3C, FLDU, FLOOD, FNI, FPH, FTI, 
     &    FUHYDR, OXRNTRF, YRDOY)

!***********************************************************************
!***********************************************************************
!     Daily rate calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
!     Copy flood N variables into temporary variables to compute
!       changes for today.
      TMPFUREA = FLDU  + DLTFUREA
      TMPFNO3  = FLDN3 + DLTFNO3
      TMPFNH4  = FLDH4 + DLTFNH4

      TMPUREA = UREA(1) + DLTUREA(1)
      TMPNO3  = SNO3(1) + DLTSNO3(1)
      TMPNH4  = SNH4(1) + DLTSNH4(1)

      TMPOXU  = OXU  + DLTOXU
      TMPOXN3 = OXN3 + DLTOXN3
      TMPOXH4 = OXH4 + DLTOXH4

!     Compute concentrations of N in floodwater.
      FLDH4C = TMPFNH4  * 100.0 / FLOOD
      FLDN3C = TMPFNO3  * 100.0 / FLOOD

      FAC1 = 1.0 / (BD1 * 1.E-01 * DLAYR(1))
      OXFAC  = 1.00/(BD1*OXLT*1.0E-01)
      ALGALN = 0.0
      WALB     = 0.1
      FRAD     = SRAD*(1.0-WALB)*EXP(-0.65*XHLAI)      ! .85->.65
      ALI      = 1.0 - EXP(-FRAD/5.0)
      !
      ! Potential Hydrolysis
      !
      AK    = 0.008 + 0.005*OC(1)
      AK    = AMIN1 (AK,1.0)
      !
      ! Floodwater Evaporation (EF) initialization at begin of day
      !
      ! AMPEF(hr) is 0.2*EF(daily)  Inc to .38
      !
      AMPEF   = 0.38*EF    
      AMLOSS  = 0.0
      OXRNTRF = 0.0
      FTEMP   = (TMAX + TMIN)*0.5

      IF (FTEMP .LT. 30.0) THEN
         FTI = (FTEMP-15.0)*0.1
       ELSEIF (FTEMP .GE. 30.0) THEN
         FTI = 1.0-(FTEMP-30.0)*0.05
      ENDIF

      FTI = AMIN1 (FTI,1.0)
      !
      ! Biological activity of floodwater
      !
      ELAG   = AMIN1 (FNI,FPI,ALI,FTI)
      ALGACT = ELAG*(3.0-ALGACT)*ALGACT       ! 3->3.5
      IF (XHLAI .GT. 1.0) THEN
         ALGACT = AMIN1 (ALGACT,ALI)
      ENDIF
      ALGACT = AMIN1 (ALGACT,1.00)
      ALGACT = AMAX1 (ALGACT,0.10)
      FLDNI = TMPFNH4 + TMPFNO3

      IF (.NOT. AKILL) THEN
         IF (.NOT. IMMOBIL) THEN
            IF (ALGALN .GT. FLDNI) THEN
                ALGFIX = ALGALN
            ENDIF
            ALGALN = FLDNI
            ALGALN = AMAX1 (ALGALN,ALGFIX)
            !
      ! Trying to determine amount of N in algae at peak algal growth
      ! Maximum N fixed by algae = 5 kg N/ha
            !
            IF (ALGALN .GE. ALGFIX .AND. ALGALN .GT. 0.8) THEN
               ALGALN = AMIN1  (ALGALN,8.0)
               TMPFNO3  = TMPFNO3 - ALGALN * TMPFNO3 / FLDNI
               TMPFNH4  = TMPFNH4 - ALGALN * TMPFNH4 / FLDNI
               IMMOBIL  = .TRUE.
               ALGFIX = ALGALN
             ELSE
               IF (KILL .EQ. 1 .AND. FLDNI .GT. 0.0) THEN
                  TMPFNO3  = TMPFNO3 - ALGALN * TMPFNO3 / FLDNI
                  TMPFNH4  = TMPFNH4 - ALGALN * TMPFNH4 / FLDNI
                  IMMOBIL  = .TRUE.
                  ALGFIX = ALGALN
               ENDIF
            ENDIF
         ENDIF
         IF (YALGA .LE. ALGACT) THEN
            KILL = 0
          ELSE
            KILL = KILL + 1
            IF (KILL .EQ. 2) THEN
               AKILL  = .TRUE.
               ALGFON = ALGFON + ALGFIX
               ALGFIX = 0.0
             ENDIF
         ENDIF
      ENDIF

      YALGA = ALGACT

      IF (ALGACT .GE. 0.2 .OR. YRDOY .LT. LFD10) THEN
        DailyCalc = .FALSE.
        IHDAY = 12
        IST   = 1
        IHR   = 12
      ELSE
        DailyCalc = .TRUE.
        IHDAY = 1
        IST   = 6
        IHR   = 6
      ENDIF

      TOTUH = 0.0

      DO I = IST, IHR
         K    = 7 - I
         IF (I .GT. 6) THEN
            K = I - 6
         ENDIF
         HTMFAC = 0.931 + 0.114*K-0.0703*K**2+0.0053*K**3
         !
         ! First approximation of floodwater temperature
         !
         FTEMP = TMIN + HTMFAC*(TMAX + 2.0 - TMIN)
         IF (I .EQ. 13) THEN
            FTEMP = TMIN
         ENDIF
         !
         ! Calculate Hourly Floodwater Evaporation (HEF)
         !
         IF (I .LE. 6 .OR. I .GT. 9) THEN
            HEF = AMPEF*SIN(3.141593*FLOAT(I)/12.0) + 0.08  ! 10->9
         ENDIF

         HEF = ABS (HEF)
         !
         ! Calculate indices for biological activity
         !
         IF (I .EQ. 6) THEN
            FNI = (FLDH4C+FLDN3C)/15.0+0.10          ! 15->10
         ENDIF
         FNI     = AMIN1 (FNI,1.0)
         PHSHIFT = 0.5 + 2.0*ALGACT                  ! 2->2.5
         !
         ! Urea hydrolysis function of surface layer OC or
         ! biological activity, whichever is greater.
         !
         TEMPFU = 0.04 * FTEMP - 0.2
         TEMPFU = AMIN1 (TEMPFU, 0.9)
         UALGCT = 0.1*ALGACT
         FUHYDR = AMAX1 (AK, UALGCT) * TEMPFU * TMPFUREA
         TMPFUREA   = TMPFUREA  - FUHYDR
         TMPFNH4  = TMPFNH4 + FUHYDR
         TOTUH  = TOTUH + FUHYDR

         IF (I .LE. 6 .OR. I .GT. 8) THEN
            FPH = 7.0 + PHSHIFT*SIN(3.1412*FLOAT(I)/12.0)
            !
            ! Add effects of UREA hydrolysis on floodwater pH here
            !
            IF (FUHYDR .GT. 0.05) THEN
               FUHYDRC = FUHYDR*100.0/FLOOD
               FUHYDRM = FUHYDRC*0.001/14.0
               PHFUHYD = AMIN1(10.0,-LOG10(FUHYDRM))
               FPH1    = FPH
               FPH     = FPH + ALI*(10.0-PHFUHYD)/10.0
            ENDIF
         ENDIF

         FPH  = AMAX1 (FPH,7.0)
         !
         ! Ammonia loss routine ... calculate floodwater NH3
         !
         TK     = FTEMP + 273.
         FLDH4C = TMPFNH4 * 100.0 / FLOOD
         FLDN3C = TMPFNO3 * 100.0 / FLOOD
         IF (I .LE. 6 .OR. I .GT. 9 .OR. FLDH3C .GE. FLDH4C) THEN
            FLDH3C = FLDH4C/(1.0+10.0**(0.09018+2729.92/TK-FPH))
            FLDH3C = MAX(0.0, FLDH3C)
         ENDIF
         FLDH3  = FLDH3C*FLOOD*0.01
         WIND   = 7.15*HEF                           ! 7.15->5.75
         IF (XHLAI .GT. 1.0) THEN
            WIND = 5.75*HEF/(XHLAI*1.5)
         ENDIF
         ALOGHK = 155.559-8621.06/TK-25.6767*ALOG(TK)+0.035388*TK
         HK     = EXP (ALOGHK)
         FNH3M  = FLDH3C*0.001/14.0
         FNH3P  = AMAX1 (0.0,10.*FNH3M/HK)       ! 1.552
         IF (FNH3P .GT. 0.0) THEN
         AMLOS1 = 0.036 * FNH3P + 0.0082 * WIND +
     &            0.000036 * FNH3P**2 * WIND * FLOOD
         ENDIF
         IF (NSWITCH .EQ. 7) THEN
            AMLOS1 = 0.0
         ENDIF
         IF (FNH3P .LE. 0.0) THEN
            AMLOS1 = 0.0
         ENDIF
         GLOS1  = AMLOS1
         IF (FLDH3C .LE. 0.0 .OR. AMLOS1 .LE. 0.0) THEN
            AMLOS1 = 0.0
         ENDIF
         FLDH3  = AMIN1 (FLDH3,TMPFNH4)
         AMLOS1 = AMIN1 (AMLOS1,FLDH3)
         TMPFNH4  = TMPFNH4  - AMLOS1
         AMLOSS = AMLOSS + AMLOS1
         TOTAML = TOTAML + AMLOS1
         FLDH4C = TMPFNH4*100.0/FLOOD
         !
      ! Now do some nitrification in the oxidised layer - thickness from
      ! flooding.  Denitrification in NTRANS routine should take care of
      ! any nitrate accumulation
        ! TFACTOR = EXP(-6602/TK+24.1)
         TFACTOR = EXP(-6572/TK+21.4)             !UPS UH 03/21/03
         FLDNI  = TMPFNH4 + TMPFUREA + TMPFNO3
         FLDNC   = FLDNI*100.0/FLOOD
         WF2     = 0.15                              ! .85 --> .15
         PHFACT  = 0.3*PH(1)-1.65
         PHFACT  = AMIN1 (PHFACT,1.0)
         PHFACT  = AMAX1 (PHFACT,0.0)
         RNTRFI  = TFACTOR*WF2*PHFACT/13.0
         RNTRFI  = AMIN1 (RNTRFI,0.90)
         HOXRNT  = RNTRFI*TMPNH4*OXLT/DLAYR(1)
!         XMIN    = SMIN4(1)
         XMIN    = 0.001
         HOXRNT  = AMAX1(0.0, AMIN1 (HOXRNT,TMPNH4-XMIN))
         TMPNH4 = TMPNH4 - HOXRNT
         TMPNO3 = TMPNO3 + HOXRNT
         OXRNTRF = OXRNTRF + HOXRNT

         OXH4C = TMPNH4
         OXN3C = TMPNO3
         OXUC  = TMPUREA
         TMPOXN3  = OXN3C/OXFAC
         TMPOXU   = OXUC /OXFAC
         TMPOXH4  = OXH4C/OXFAC
         !
         ! Recompute equilibria after nitrification/denitrification
         !
         CALL EQUIL2 (
     &    BD1, SURCEC, DLAYR, FLOOD, IHDAY, 1,            !Input
     &    OXLT, OXMIN3, OXMIN4, SW1, YRDOY, YRDRY,        !Input
     &    IBP, TMPFUREA, TMPOXU, TMPUREA)                      !I/O

         CALL EQUIL2 (
     &    BD1, SURCEC, DLAYR, FLOOD, IHDAY, 2,            !Input
     &    OXLT, OXMIN3, OXMIN4, SW1, YRDOY, YRDRY,        !Input
     &    IBP, TMPFNO3, TMPOXN3, TMPNO3)                       !I/O

         CALL EQUIL2 (
     &    BD1, SURCEC, DLAYR, FLOOD, IHDAY, 3,            !Input
     &    OXLT, OXMIN3, OXMIN4, SW1, YRDOY, YRDRY,        !Input
     &    IBP, TMPFNH4, TMPOXH4, TMPNH4)                       !I/O

      END DO

!     Changes to urea, NO3 and NH4 due to flood initialization processes
      !Flood variables
      DLTFUREA = MAX(0.0, TMPFUREA) - FLDU
      IF (DLTFUREA + FLDU < 1.E-9 .AND. FLDU > 1.E-5) THEN
        DLTFUREA = -FLDU
      ENDIF
      DLTFNO3  = MAX(0.0, TMPFNO3)  - FLDN3
      IF (DLTFNO3 + FLDN3 < 1.E-9 .AND. FLDN3 > 1.E-5) THEN
        DLTFNO3 = -FLDN3
      ENDIF
      DLTFNH4  = MAX(0.0, TMPFNH4)  - FLDH4
      IF (DLTFNH4 + FLDH4 < 1.E-9 .AND. FLDH4 > 1.E-5) THEN
        DLTFNH4 = -FLDH4
      ENDIF

      !Oxidation layers
      DLTOXU  = MAX(0.0, TMPOXU)  - OXU
      DLTOXH4 = MAX(0.0, TMPOXH4) - OXH4
      DLTOXN3 = MAX(0.0, TMPOXN3) - OXN3

      !Surface variables
      DLTUREA(1) = MAX(0.0, TMPUREA) - UREA(1)
      DLTSNO3(1) = MAX(0.0, TMPNO3)  - SNO3(1)
      DLTSNH4(1) = MAX(0.0, TMPNH4)  - SNH4(1)

!***********************************************************************
!***********************************************************************
!     Daily OUTPUT
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
      CALL OPFLOODN (CONTROL, ISWITCH, 
     &    ALGACT, ALI, AMLOSS, BD1, EF, FLDH3C, FLDH4, 
     &    FLDH4C, FLDN3C, FLDU, FLOOD, FNI, FPH, FTI, 
     &    FUHYDR, OXRNTRF, YRDOY)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE FCHEM

