C=======================================================================
C  CSCERES_Interface, Subroutine
C  DSSAT interface for CSCERES Wheat growth routine.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  04/16/2002 LAH/CHP Written.
C  03-12-2003 CHP Changed senescence variable to composite (SENESCE)
C                   as defined in ModuleDefs.for
C=======================================================================
      SUBROUTINE CSCERES_Interface (CONTROL, ISWITCH,      !Input
     &     EOP, YREND, NH4, NO3, SNOW, SOILPROP,           !Input
     &     SRFTEMP, ST, SW, TRWUP, WEATHER, YRPLT, HARVFRAC,!Input
     &     CANHT, HARVRES, KCAN, KEP, MDATE, NSTRES,PORMIN,!Output
     &     RLV, RWUMX, SENESCE, STGDOY, UNH4, UNO3, XLAI)  !Output

      USE ModuleDefs
      USE ModuleData
      USE CsvOutput   ! VSH
      USE CER_First_Trans_m

      IMPLICIT NONE
      EXTERNAL YR_DOY, CSCER
      SAVE

      CHARACTER*1   IDETG, IDETL, IDETO, IDETS, ISWWAT, ISWNIT, RNMODE
      !CHARACTER*2   CROP
      !CHARACTER*30  FILEIO
      CHARACTER*250 FILEIOCS

      INTEGER DYNAMIC, RUN, TN, RUNI, RN, ON
      INTEGER REP, STEP, CN, YRHAR, YREND, YRDOY
      INTEGER MDATE, NLAYR !, L
      INTEGER MULTI, FROP, SN, YEAR, DOY
      INTEGER STGDOY(20), YRPLT

      REAL WUPT, EOP, EP, ET, TRWUP, SRAD, TMAX, TMIN, CO2
      REAL SNOW, KCAN, KEP, DEPMAX
      REAL NSTRES, XLAI, LAI, NFP, SLPF
      REAL DAYL, TWILEN, PORMIN, RAIN, RWUMX, SRFTEMP
      REAL CANHT, EO, TOTIR, WINDSP

      REAL, DIMENSION(NL) :: BD, DLAYR, DS, DUL, LL
      REAL, DIMENSION(NL) :: NH4, NO3, RLV, SAT, SHF
      REAL, DIMENSION(NL) :: ST, SW, UNO3, UNH4, UH2O
      REAL, DIMENSION(0:NL) :: SENC, SENN, SENLIG
      REAL, DIMENSION(0:NL) :: CRESC, CRESN, CRESLIG
      REAL, DIMENSION(0:NL) :: SOILTEMP
      REAL, DIMENSION(2)  :: HARVFRAC

      TYPE (ControlType) CONTROL
      TYPE (SoilType)    SOILPROP
      TYPE (SwitchType)  ISWITCH
      Type (ResidueType) HARVRES
      Type (ResidueType) SENESCE
      TYPE (WeatherType) WEATHER

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      CROP    = CONTROL % CROP
      FROP    = CONTROL % FROP
      MULTI   = CONTROL % MULTI
      RNMODE  = CONTROL % RNMODE
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY

      ISWWAT  = ISWITCH % ISWWAT
      ISWNIT  = ISWITCH % ISWNIT
      IDETG   = ISWITCH % IDETG
      IDETL   = ISWITCH % IDETL
      IDETO   = ISWITCH % IDETO
      IDETS   = ISWITCH % IDETS
      FMOPT   = ISWITCH % FMOPT   ! VSH

      BD     = SOILPROP % BD     
      DLAYR  = SOILPROP % DLAYR  
      DS     = SOILPROP % DS    
      DUL    = SOILPROP % DUL    
      LL     = SOILPROP % LL     
      NLAYR  = SOILPROP % NLAYR  
      SAT    = SOILPROP % SAT    
      SHF    = SOILPROP % WR
      SLPF   = SOILPROP % SLPF

      YRHAR = YREND
      WUPT  = TRWUP

      DEPMAX = DS(NLAYR)

      FILEIOCS(1:30) = FILEIO

      CALL YR_DOY(YRDOY, YEAR, DOY)

      IF (DYNAMIC .EQ. RUNINIT .OR. DYNAMIC .EQ. SEASINIT) THEN
        TN = 0
        RN = 0
        SN = 0
        ON = 0
        CN = 0  !Crop component
        REP = 1
        STEP = 1
        RUNI = 1
        TOTIR = 0.0
      ELSEIF (DYNAMIC == RATE) THEN
        CALL GET('SPAM','EO',  EO)
        CALL GET('SPAM','EP',  EP)
        CALL GET('SPAM','UH2O',UH2O)
      ELSEIF (DYNAMIC == INTEGR) THEN
        CALL GET('SPAM','ET',  ET)
        CALL Get('MGMT','TOTIR', TOTIR)
      ENDIF

      SOILTEMP(0) = SRFTEMP
      DO L = 1, NLAYR
        SOILTEMP(L) = ST(L)
      ENDDO

      CO2   = WEATHER % CO2
      DAYL  = WEATHER % DAYL 
      RAIN  = WEATHER % RAIN
      SRAD  = WEATHER % SRAD
      TMAX  = WEATHER % TMAX
      TMIN  = WEATHER % TMIN
      TWILEN= WEATHER % TWILEN
      WINDSP= WEATHER % WINDSP

C-----------------------------------------------------------------------
      CALL CSCER (FILEIOCS, RUN, TN, RN, RNMODE,           !Command line
     & ISWWAT, ISWNIT, IDETS, IDETO, IDETG, IDETL, FROP,   !Controls
     & SN, ON, RUNI, REP, YEAR, DOY, STEP, CN,             !Run+loop
     & SRAD, TMAX, TMIN, CO2, RAIN, TOTIR,                 !Weather
     & TWILEN, WINDSP, SOILTEMP, EO,                       !Weather
     & NLAYR, DLAYR, DEPMAX, LL, DUL, SAT, BD, SHF, SLPF,  !Soil states
     & SNOW, SW, NO3, NH4,                                 !H2o,N states
     & YRPLT, HARVFRAC,                                    !Pl.date
     & EOP, EP, ET, TRWUP,                                 !Resources
     & LAI, KCAN, KEP,                                     !States
     & RLV, NFP, PORMIN, RWUMX, CANHT,                     !States
     & UNO3, UNH4, UH2O,                                   !Uptake
     & SENC, SENN, SENLIG,                                 !Senescence
     & CRESC, CRESN, CRESLIG,                              !Residue
     & STGDOY,                                             !Stage dates
     & DYNAMIC)                                            !Control 

      XLAI   = LAI
      NSTRES = NFP

      IF (STGDOY(11).EQ.YRDOY) THEN
        MDATE = YRDOY
        YREND = YRDOY
      ENDIF 

      if (dynamic .eq. integr) then
        DO L=0, NLAYR
          SENESCE % ResWt(L)  = (SENC(L) + CRESC(L)) / 0.40
          SENESCE % ResLig(L) = SENLIG(L) + CRESLIG(L)
          SENESCE % ResE(L,1) = SENN(L) + CRESN(L)
        ENDDO
      endif
                      
      IF (YREND.EQ.YRDOY .AND. DYNAMIC.EQ.INTEGR) THEN 
        !Transfer harvest residue from senescence variable to 
        !harvest residue variable on day of harvest.
        HARVRES = SENESCE
        SENESCE % ResWt  = 0.0
        SENESCE % ResLig = 0.0
        SENESCE % ResE   = 0.0
      ELSE
        MDATE = -99
      ENDIF

      RETURN
      END SUBROUTINE CSCERES_Interface
