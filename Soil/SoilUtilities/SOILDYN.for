C=======================================================================
C  COPYRIGHT 1998-2020 DSSAT Foundation
C                      University of Florida, Gainesville, Florida
C                      Inernational Fertilizer Development Center
C  
C  ALL RIGHTS RESERVED
C=======================================================================
C=======================================================================
C  SOILDYN, Subroutine, C.H. Porter
C  Soil dynamics routine computes and distributes soil parameters.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  10/29/2001 CHP Written
C  08/12/2003 CHP Added I/O error checking
!  10/29/2004 CHP Program to halt for missing WR data (per GH).
!  02/01/2005 CHP Added initializations and revised checks.
!  11/07/2005 CHP Added KG2PPM conversion variable to SoilType
!  02/22/2006 CHP Added tiledrain, ETDR is hardwired for now.
!  03/03/2006 CHP Added dynamic soil properties and tillage operations.
!  04/13/2006 CHP Added TextureClass subroutine
!  04/18/2006 CHP Modify BD, DLAYR, DUL, LL based on SOM changes.
!  07/07/2006 CHP Update OC daily
!  07/14/2006 CHP Added 2nd tier soils data for P model
!  07/24/2006 CHP Added mulch/soil albedo (MSALB) and canopy/mulch/soil
!                   albedo (CMSALB) to SOILPROP variable
!  09/11/2006 CHP Compute total organic N, TotOrgN, & store in SOILPROP
!  03/26/2007 CHP Soil layer depth labels added to SoilProp variable 
!  02/11/2009 CHP Do not run SoilDyn when ISWWAT = 'N'
!                 Changed condition for missing or zero OC.
!  01/24/2023 chp added SAEA to soil analysis in FileX for methane
!  08/30/2024  FO Added WARNING message if LL > DUL > SAT.
C-----------------------------------------------------------------------
C  Called : Main
C  Calls  : 
C=======================================================================

      SUBROUTINE SOILDYN(CONTROL, ISWITCH, 
     &    KTRANS, MULCH, SomLit, SomLitC, SW, TILLVALS,   !Input
     &    WEATHER, XHLAI,                                 !Input
     &    SOILPROP)                                       !Output

C-----------------------------------------------------------------------
      USE ModuleDefs 
      USE ModuleData

      IMPLICIT NONE
      EXTERNAL ERROR, FIND, WARNING, INFO, TEXTURECLASS, SOILLAYERCLASS,
     &  CALBROKCRYPARA, RETC_VG, SOILLAYERTEXT, PRINT_SOILPROP, 
     &  SETPM, OPSOILDYN, ALBEDO_avg, TILLEVENT, SOILMIXING
      SAVE

      LOGICAL NOTEXTURE, PHFLAG, FIRST, NO_OC
      CHARACTER*1 ISWTIL, ISWWAT, MEINF, MESOM, RNMODE
      CHARACTER*6 SECTION
      CHARACTER*7, PARAMETER :: ERRKEY = 'SOILDYN'
      CHARACTER*30 FILEIO
      CHARACTER*78 MSG(NL+10)
      CHARACTER*200 CHAR

      INTEGER DAS, DYNAMIC, ERRNUM, FOUND, I, L, Length 
      INTEGER LNUM, LUNIO, MULTI, REPNO, RUN, YRDOY
!     ---------------------------------------------------------------
!     Soil properties:
      CHARACTER*5 SLTXS, SMPX
      CHARACTER*10 SLNO
      CHARACTER*11 SLSOUR
      CHARACTER*12 TEXTURE(NL)
      CHARACTER*17 SOILLAYERTYPE(NL)
      CHARACTER*50 SLDESC, TAXON
      INTEGER NLAYR
      REAL CN, DMOD, KTRANS, SALB, SLDP, SLPF, SWCON, TEMP, TOTAW, U
      REAL SWAD, SWnew
      REAL, DIMENSION(NL) :: ADCOEF, BD, CEC, CLAY, DLAYR, DS, DUL
      REAL, DIMENSION(NL) :: KG2PPM, LL, OC, PH, POROS, SAND, SAT, SILT
      REAL, DIMENSION(NL) :: SW, SWCN, TOTN, TotOrgN, WCR, WR
!     REAL, DIMENSION(NL) :: RGIMPF
      LOGICAL, DIMENSION(NL) :: COARSE

!     Initial conditions (used to calculate TotOrgN from TOTN)
      REAL, DIMENSION(NL) :: NO3, NH4

!     Second tier soils data:
      REAL, DIMENSION(NL) :: EXTP, TOTP, ORGP, CACO, CACO3
      REAL, DIMENSION(NL) :: EXTAL, EXTFE, EXTMN, TOTBAS, PTERMA
      REAL, DIMENSION(NL) :: PTERMB, EXK, EXMG, EXNA, EXTS, SLEC, EXCA

!     vanGenuchten parameters
      REAL, DIMENSION(NL) :: alphaVG, mVG, nVG
!     Brook & Corey model parameters
      Double Precision, DIMENSION(NL) :: hb, lambda

!     From soil analysis section 
!     Stable organic C, read from 2nd tier soil data in INP file.
!     Value comes from soil analysis section of FILEX, but stored w/ soil profile data
      REAL, DIMENSION(NL) :: SASC
!     Soil alternate electron acceptors (mol Ceq/m3)
      REAL, DIMENSION(NL) :: SAEA

!From DSSAT3.0 manual (second tier):
! EXTP    Extractable phosphorus (mg/kg)
! TOTP    Total phosphorus (mg/kg)
! ORGP    Organic phosphorus (mg/kg)
! CACO    CaCO3 content (g/kg) -- called CACO in Input module
!           Save as % in variable CaCO3.
! EXTAL   Aluminum
! EXTFE   Iron
! EXTMN   Manganese
! TOTBAS  Base saturation (cmol/kg)
! PTERMA  Phosphorus isotherm A (mmol/kg)
! PTERMB  Phosphorus isotherm B (mmol/kg)
! EXK     Exchangeable potassium (cmol/kg)
! EXMG    Magnesium (cmol/kg)
! EXNA    Sodium (cmol/kg)
! EXTS    Sulfur
! SLEC    Electrical conductivity (seimen)

!     Not currently used:
      REAL, DIMENSION(NL) :: PHKCL, STONES

!     Added for Tiledrain:
      REAL ETDR

!     ---------------------------------------------------------------
!     Soil dynamics variables
      INTEGER NTIL, TILDATE, NMSG
      REAL AS, CRAIN, CUMDEP   !, FF, CANCOV
      REAL LCRAIN, MCUMDEP, MIXPCT, MULCHALB, MULCHCOVER
      REAL RAIN, RSTL, SOILCOV, SRATE
      REAL SUMKE, SUMKEL, SUMKET, TDEP, TIL_IRR, XHLAI
      REAL CN_TILLED
      REAL, DIMENSION(NL) :: CLAY_PREMIX, CLAY_MIX, SAND_PREMIX
      REAL, DIMENSION(NL) :: SAND_MIX, SILT_PREMIX, SILT_MIX
      REAL, DIMENSION(NL) :: BD_TILLED, DL_TILLED, DS_TILLED
      REAL, DIMENSION(NL) :: SAT_TILLED, SC_TILLED
!     REAL, DIMENSION(NL) :: RG_TILLED, RGIF_INIT
      LOGICAL TILLED

!     Keep initial values of soil properties -- these are the "baseline"
!     values for comparison with effects of tillage and organic C content.
      REAL CN_INIT
      REAL, DIMENSION(NL) :: BD_INIT, DLAYR_INIT, DS_INIT, DUL_INIT
      REAL, DIMENSION(NL) :: LL_INIT, SWCN_INIT, SAT_INIT, SW_INIT

!     Base soil values modified by soil organic matter
      REAL dBD_SOM, dDLAYR_SOM, dDUL_SOM, dLL_SOM, dSOM, dOC
      REAL, DIMENSION(NL) :: BD_SOM, DLAYR_SOM, DS_SOM, DUL_SOM, LL_SOM
      REAL, DIMENSION(NL) :: SomLit, SomLit_INIT, SOM_PCT, SOM_PCT_init
      REAL, DIMENSION(NL) :: OC_INIT, TOTN_INIT, TotOrgN_init
      REAL, DIMENSION(0:NL) :: SomLitC, KECHGE

!     Izaurralde method
      INTEGER, PARAMETER :: METHOD = 2
      REAL, DIMENSION(NL) :: BD_calc, BD_calc_init  !, BD_mineral

      REAL CN_BASE
      REAL, DIMENSION(NL) :: BD_BASE, DL_BASE, DS_BASE, SAT_BASE,SC_BASE    !, RG_BASE

!     Labels for soil layer depth info
      CHARACTER*8 LayerText(11)

      LOGICAL PRINT_TODAY, VG_ok
      REAL WCR_TEMP

!     ---------------------------------------------------------------
!     Composite variables
      TYPE (SoilType)   , INTENT(OUT):: SOILPROP !Soil properties
      TYPE (MulchType)  , INTENT(IN) :: MULCH    !Surface mulch propert.
      TYPE (SwitchType) , INTENT(IN) :: ISWITCH  !Simulation options 
      TYPE (ControlType), INTENT(IN) :: CONTROL  !Control variables
      TYPE (TillType)   , INTENT(IN) :: TILLVALS !Tillage operation vars
      TYPE (WeatherType), INTENT(IN) :: WEATHER  !Weather variables

      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO
      MULTI   = CONTROL % MULTI
      REPNO   = CONTROL % REPNO
      RNMODE  = CONTROL % RNMODE
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY

      MEINF   = ISWITCH % MEINF
      MESOM   = ISWITCH % MESOM
      
      MULCHALB = MULCH % MULCHALB

      RAIN = WEATHER % RAIN

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
!     IF (DYNAMIC .EQ. RUNINIT .OR. DYNAMIC .EQ. SEASINIT) THEN
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
!     Skip initialization for sequenced runs:
      IF (INDEX('FQ',RNMODE) > 0 .AND. RUN /= 1) RETURN

!     Initialize soils variables
      NLAYR  = 0
      DLAYR  = -99.
      DS     = -99.
      SLTXS  = '     '
      SLSOUR = '           '
      SLDESC = '                                                 '
      TAXON  = '                                                 '
      SLNO   = '-99.      '
      LayerText = '        '

      SLDP   = -99.
      SALB   = -99.
      DMOD   = -99.
      SLPF   = -99.
      ETDR   = -99.
      CLAY   = -99.
      SILT   = -99.
      SAND   = 0.
      STONES = -99.
      OC     = -99.
      SASC   = -99.
      SAEA   = -99.
      PH     = -99.
      BD     = -99.
      LL     = -99.
      DUL    = -99.
      SAT    = -99.
      SWCN   = -99.
      PHKCL  = -99.
      CEC    = -99.
!     RGIMPF = -99.

      U      = -99.
      SWCON  = -99.
      CN     = -99.
      WR     = -99.
      TOTN   = -99.
      TotOrgN= -99.
      ADCOEF = 0.

      SMPX   = "-99  "
      CACO3  = -99.
      EXTP   = -99.
      ORGP   = -99.
      PTERMA = -99.
      PTERMB = -99.
      TOTP   = -99.
      TOTBAS = -99.
      EXCA   = -99.
      EXK    = -99.
      EXNA   = -99.
      
!-----------------------------------------------------------------------
!     Should not need to run this unless soil water is being simulated.
!     However, currently roots are grown even with no soil water simulation.
!     Need to fix this in the future
      ISWWAT = ISWITCH % ISWWAT

!-----------------------------------------------------------------------
!     Read FILEIO
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)

!-----------------------------------------------------------------------
!     Find and Read Soil Profile Section.
      SECTION = '*SOIL '
      CALL FIND(LUNIO, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEIO, LNUM)

      READ(LUNIO,60,IOSTAT=ERRNUM,ERR=1000)SLNO,SLSOUR,SLTXS,SLDP,SLDESC
   60 FORMAT (1X,A10, 2X, A11,1X,A5,1X,F5.0,1X,A50)
      LNUM = LNUM + 1
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LNUM)
      READ(LUNIO, 70, IOSTAT=ERRNUM,ERR=1000) TAXON ; LNUM = LNUM + 1
   70 FORMAT (41X,A50)
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LNUM)
      
!-----------------------------------------------------------------------
      READ(LUNIO, 80, IOSTAT=ERRNUM,ERR=1000) 
     &           SALB, U, SWCON, CN, DMOD, SLPF, SMPX
   80 FORMAT(7X,F5.2,1X,F5.1,1X,F5.2,1X,F5.0,2(1X,F5.2),
     &       7X,A5,12X,F6.0)
     
      LNUM = LNUM + 1
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LNUM)

!     First tier soils data:
      NLAYR = 0
      DO L = 1, NL
        READ(LUNIO,'(F6.0,6X,A200)',IOSTAT=ERRNUM,ERR=1000) DS(L), CHAR
        IF (ERRNUM .NE. 0) EXIT
        IF (L .GT. 1) THEN
          IF (DS(L) .LT. DS(L-1)) EXIT
        ENDIF

        READ(CHAR,'(15F6.0)',IOSTAT=ERRNUM,ERR=1000) 
     &      LL(L), DUL(L), SAT(L), WR(L), SWCN(L), BD(L),
     &      OC(L), CLAY(L), SILT(L), STONES(L), TOTN(L),
     &      PH(L), PHKCL(L), CEC(L), ADCOEF(L)
        LNUM = LNUM + 1

        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LNUM)
        NLAYR = NLAYR + 1
      ENDDO

!     Second tier soils data:
      DO L = 1, NLAYR
        IF (L == 1 .AND. NLAYR == NL) THEN
!         If number of layers = NL, need to read the blank line between soil tiers
          READ(LUNIO,'(/,F6.0,A200)',IOSTAT=ERRNUM,ERR=1000) DS(L), CHAR
        ELSE
          READ(LUNIO,'(F6.0,A200)',IOSTAT=ERRNUM,ERR=1000) DS(L), CHAR
        ENDIF
        IF (ERRNUM .NE. 0) EXIT
        IF (L .GT. 1) THEN
          IF (DS(L) .LT. DS(L-1)) EXIT
        ENDIF
        LNUM = LNUM + 1

!       Calcium carbonate is read as CACO (units: g/kg) and converted 
!       to CACO3 (units: %).
        READ (CHAR,'(20(F6.0))',IOSTAT=ERRNUM,ERR=1000) 
     &      EXTP(L), TOTP(L), ORGP(L), CACO(L), 
     &      EXTAL(L), EXTFE(L), EXTMN(L), TOTBAS(L), PTERMA(L),
     &      PTERMB(L), EXK(L), EXMG(L), EXNA(L), EXTS(L), SLEC(L),
     &      EXCA(L), SASC(L), SAEA(L)
        LNUM = LNUM + 1
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LNUM)
      ENDDO

!     Third tier soils data (van Genuchten parameters):
      DO L = 1, NLAYR
!       Need to read the blank line between soil tiers
        IF (L == 1) THEN
!         If number of layers = NL, need to read the blank line between soil tiers
          READ(LUNIO,'(/,F6.0,A200)',IOSTAT=ERRNUM,ERR=1000) DS(L), CHAR
        ELSE
          READ(LUNIO,'(F6.0,A200)',IOSTAT=ERRNUM,ERR=1000) DS(L), CHAR
        ENDIF
        IF (ERRNUM .NE. 0) EXIT
        IF (L .GT. 1) THEN
          IF (DS(L) .LT. DS(L-1)) EXIT
        ENDIF
        LNUM = LNUM + 1

        READ (CHAR,'(20(F6.0))',IOSTAT=ERRNUM,ERR=1000) 
     &      alphaVG(L), mVG(L), nVG(L), WCR(L)
        LNUM = LNUM + 1
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LNUM)
      ENDDO

!-----------------------------------------------------------------------
!     Find and read Initial Conditions section -- get initial inorganic
!       N for calculation of total organic N.
      REWIND(LUNIO)
      SECTION = '*INITI'
      CALL FIND (LUNIO, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) CALL ERROR (SECTION, 42, FILEIO, LNUM)

!     Read line before layer data -- ignore
      READ (LUNIO, '(A)', IOSTAT = ERRNUM,ERR=1000) CHAR
      LNUM = LNUM + 1
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, LNUM)

      NMSG = 0
      DO L = 1, NLAYR
        READ(LUNIO, 100, IOSTAT=ERRNUM,ERR=1000)SW(L), NH4(L),NO3(L)
100     FORMAT (8X, 3 (1X, F5.1))
        LNUM = LNUM + 1
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, LNUM)

        IF (SW(L) .LT. LL(L)) THEN
          IF (NMSG == 0) THEN
            MSG(1) = "Initial soil water content < LL."
            MSG(2) = " "
            MSG(3) = "         FileX              SW @  Revised"
            MSG(4) = "Layer  Init SW       LL   AirDry  Init SW"
            NMSG = 4
          ENDIF
          IF (L == 1) THEN
!           Layer 1 - check for SW < air dry
            SWAD = 0.30 * LL(L)
            IF (SW(L) < SWAD) THEN
              SWnew = SWAD
              NMSG = NMSG + 1
              WRITE(MSG(NMSG),'(I5,4F9.3)') L, SW(L), LL(L), SWAD, SWnew
              SW(L) = SWnew
            ENDIF
          ELSE
!           Layers 2 thru NLAYR
            SWnew = LL(L)
            NMSG = NMSG + 1
            WRITE(MSG(NMSG),'(I5,2F9.3,9X,F9.3)') L, SW(L), LL(L), SWnew
            SW(L) = SWnew
          ENDIF
        ENDIF
      ENDDO
      IF (NMSG > 0) THEN
        CALL WARNING(NMSG, ERRKEY, MSG)
      ENDIF

      NMSG = 0
      DO L = 1, NLAYR
        IF (SW(L) > SAT(L)) THEN
          IF (NMSG == 0) THEN
            MSG(1) = "Initial soil water content > SAT."
            MSG(2) = " "
            MSG(3) = "         FileX           Revised"
            MSG(4) = "Layer  Init SW      SAT  Init SW"
            NMSG = 4
          ENDIF
          SWnew = SAT(L)
          NMSG = NMSG + 1
          WRITE(MSG(NMSG),'(I5,3F9.3)') L, SW(L), SAT(L), SWnew
          SW(L) = SWnew
        ENDIF
      ENDDO
      IF (NMSG > 0) THEN
        CALL WARNING(NMSG, ERRKEY, MSG)
      ENDIF

      CLOSE (LUNIO)

      GOTO 2000
!     Error trap
 1000 MSG(1) = "Error reading file.  Check soil inputs."
      WRITE(MSG(2),'(A,A)') "File:    ", FILEIO
      WRITE(MSG(3),'(A,I3)')"Line:   ",  LNUM+1
      WRITE(MSG(4),'(A,A)') "Section: ", SECTION
      CALL WARNING(4, ERRKEY, MSG)
      CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM+1)

!     ------------------------------------------------------------------
 2000 CONTINUE
C-----------------------------------------------------------------------
!     Check validity of input values:
      IF (SALB < 1.E-4) THEN
        SALB = 0.15
        IF (MULTI < 2) THEN
          WRITE(MSG(1),400) SALB
  400     FORMAT('Soil albedo has been set to ',F5.1,'.')
          CALL WARNING(1, ERRKEY, MSG)
        ENDIF
      ENDIF

      IF (SLPF < 1.E-4) THEN
        SLPF = 1.0
      ELSEIF (SLPF < 0.9999) THEN
        WRITE(MSG(1),'("Soil photosynthesis factor (SLPF) =",F5.2)')SLPF
        CALL WARNING(1,ERRKEY,MSG)
      ENDIF

C     Initialize curve number (according to J.T. Ritchie) 1-JUL-97 BDB
      IF (CN .LE. 25.0 .OR. CN .GT. 98.0 .AND. ISWWAT == 'Y') THEN
        TEMP = CN
        IF (CN .LE. 0.0) THEN
          CN = 80.
        ENDIF
        CN = AMIN1 (CN,98.0)
        CN = AMAX1 (CN,25.0)
        IF (MULTI < 2) THEN
          WRITE(MSG(1),500) TEMP, CN
  500     FORMAT('Surface runoff curve number has been changed from ',
     &    F5.1,' to ',F5.1,'.')
          CALL WARNING(1, ERRKEY, MSG)
        ENDIF
      ENDIF

      IF (U < 1.E-4) THEN
        U = 6.0
        IF (MULTI < 2) THEN
          WRITE(MSG(1),505) U
  505     FORMAT('Stage 1 soil evaporation limit has been set to ',
     &       F5.1,'.')
          CALL WARNING(1, ERRKEY, MSG)
        ENDIF
      ENDIF

      IF (SWCON < 1.E-4) THEN
        SWCON = 0.25
        IF (MULTI < 2) THEN
          MSG(1)=
     &      'A default soil water conductivity constant will be used:'
          WRITE(MSG(2),510) SWCON
  510     FORMAT("SWCON = ",F5.2,' (fraction/day).')
          CALL WARNING(2, ERRKEY, MSG)
        ENDIF
      ENDIF

      PHFLAG = .FALSE.

!-----------------------------------------------------------------------
      MSG(3) = ' '
      DO L = 1, NLAYR
!       Calculate layer thicknesses
        IF (L .EQ. 1) THEN
          DLAYR(1) = DS(1)
        ELSE
          DLAYR(L) = DS(L) - DS(L-1)
        ENDIF

        !10/29/2004 CHP changed from .LE. to .LT.
        IF (WR(L) .LT. 0.0) THEN
          !10/29/2004 CHP per GH
          !Do not allow missing data.
          CALL ERROR(ERRKEY,11,FILEIO,0)
        ENDIF

!       Set bulk density to an average value, if not given.
        IF (BD(L) .LT. 0.001) THEN
          BD(L) = 1.2             
          Length = LEN(TRIM(MSG(3)))
          IF (Length < 2) THEN
            MSG(3) = '  BD'
          ELSE
            IF (INDEX(MSG(3),'BD') < 1) THEN
              MSG(3) = TRIM(MSG(3)) // ', BD'
            ENDIF
          ENDIF 
        ENDIF

!       Set the soil pH to an average value, if not given.
        IF (PH(L) .LE. 1.0 .OR. PH(L) .GT. 10.0) THEN
          PH(L) = 7.0
          IF (Length < 2) THEN
            MSG(3) = '  pH'
          ELSE
            IF (INDEX(MSG(3),'pH') < 1) THEN
              MSG(3) = TRIM(MSG(3)) // ', pH'
            ENDIF
          ENDIF 
          PHFLAG = .TRUE.
        ENDIF

        IF (CEC(L) .LE. 0.0) THEN
          CEC(L) = 15.5
          Length = LEN(TRIM(MSG(3)))
          IF (Length < 2) THEN
            MSG(3) = '  CEC'
          ELSE
            IF (INDEX(MSG(3),'CEC') < 1) THEN
              MSG(3) = TRIM(MSG(3)) // ', CEC'
            ENDIF
          ENDIF
        ENDIF
         
        IF (STONES(L) .GT. 100.0 .OR. STONES(L) .LT. 0.0) THEN
          STONES(L) = 0.0
          Length = LEN(TRIM(MSG(3)))
          IF (Length < 2) THEN
            MSG(3) = '  STONES'
          ELSE
            IF (INDEX(MSG(3),'STONES') < 1) THEN
              MSG(3) = TRIM(MSG(3)) // ', STONES'
            ENDIF
          ENDIF
        ENDIF

        IF (ADCOEF(L) .LE. 0.0) THEN
          ADCOEF(L) = 0.0
          Length = LEN(TRIM(MSG(3)))
          IF (Length < 2) THEN
            MSG(3) = '  ADCOEF'
          ELSE
            IF (INDEX(MSG(3),'ADCOEF') < 1) THEN
              MSG(3) = TRIM(MSG(3)) // ', ADCOEF'
            ENDIF
          ENDIF
        ENDIF

        IF (SAEA(L) .LT. 0.0) THEN
          SAEA(L) = 26.5
          Length = LEN(TRIM(MSG(3)))
          IF (Length < 2) THEN
            MSG(3) = '   SAEA'
          ELSE
            IF (INDEX(MSG(3),'SAEA') < 1) THEN
              MSG(3) = TRIM(MSG(3)) // ', SAEA'
            ENDIF
          ENDIF
        ENDIF
      ENDDO   !End of soil layer loop.

!     Warning message for non-sequenced runs or any first run
      IF (INDEX('QFNY',RNMODE) .LE. 0 .OR. 
     &            (RUN .EQ. 1 .AND. REPNO .EQ. 1)) THEN
        IF (LEN(TRIM(MSG(3))) > 1) THEN
!         Print message for missing or invalid data
          MSG(1)='The soil file has missing or invalid data. '
          MSG(2)='Default values have been used for these variables:'
          MSG(4)='These defaults may or may not be important to your' //
     &      ' results.'
          CALL INFO(4, ERRKEY, MSG)
        ENDIF

!       Warning that the pH was set to a different value.
        IF (PHFLAG) THEN
          WRITE (MSG(1), 600)
          WRITE (MSG(2), 601)
          WRITE (MSG(3), 602)
  600     FORMAT('The pH of one or more soil layers at initialization')
  601     FORMAT('was outside the range pH=1-10, which is unlikely.')
  602     FORMAT('It was set by DSSAT to pH=7.') 
          CALL WARNING (3, ERRKEY, MSG)
        ENDIF   !End of IF block on PHFLAG.

!!       Check for zero SWCN in any layer
!     10/14/2008 Move this check to input module and set SWCN to -99.
!        ERRNUM = 0
!        DO L = 1, NLAYR
!          IF (ABS(SWCN(L) < 1.E-6)) ERRNUM = 1     !Zero values
!        ENDDO
!        IF (ERRNUM > 0) THEN
!          MSG(1) = "Saturated hydraulic conductivity equal to " // 
!     &          "zero for one or more soil layers."
!          MSG(2) = "Data will be treated as missing."
!          CALL WARNING(2,ERRKEY,MSG)
!        ENDIF 
      ENDIF

!     Add additional checks:
      NOTEXTURE = .FALSE.

!     Check for soil texture validity.
      COARSE = .FALSE.
      DO L = 1, NLAYR
!       Soil texture check for invalid soil texture. Treat as if 
!       no texture has been provided.
        IF (CLAY(L) + SILT(L) + SAND(L) .GT. 100.001) THEN
          CLAY(L) = -99.
          SILT(L) = -99.
        ENDIF

!       If no texture data is provided, set to average values.
        IF (CLAY(L) .LE. -0.001 .OR. CLAY(L) .GT. 100.001) THEN
          CLAY(L) = 30.

!         If silt is given, but clay not, the default clay value may
!         sum to >100%.
          IF (CLAY(L) + SILT(L) .GT. 100.) CLAY(L) = 100. - SILT(L)

          NOTEXTURE = .TRUE.
        ENDIF

        IF (SILT(L) .LE. -0.001 .OR. SILT(L) .GT. 100.001) THEN
          SILT(L) = 30.

!         If clay is given, but silt not, the default silt value may
!         sum to >100%.
          IF (CLAY(L) + SILT(L) .GT. 100.) SILT(L) = 100. - CLAY(L)
          NOTEXTURE = .TRUE.
        ENDIF

!       Clay and silt data are given, but sand not.
        SAND(L) = 100. - CLAY(L) - SILT(L)
        SAND(L) = AMAX1 (SAND(L), 0.)

!       Calcium carbonate - convert from g/kg to %
        IF (CACO(L) .LT. 1.E-4) THEN
          CACO3 = -99.
        ELSE
          CACO3 = CACO / 10.0
        ENDIF

!       4/13/2006 CHP/AJG added soil texture
!       Get texture info -- these are currently local variables, but
!         could be made available through SOILPROP if they are 
!         needed elsewhere.
        CALL TEXTURECLASS (CLAY(L), SAND(L), SILT(L),     !Input
     &    TEXTURE(L), COARSE(L))                          !Output

      ENDDO   !End of soil layer loop.

      NO_OC = .FALSE.
      DO L = 1, NLAYR
        IF (OC(L) .LT. 1.E-3 .AND. ISWWAT == 'Y') THEN
!       Organic C data missing - write message to WARNING.OUT file 
!       10/15/2008 Allow zero OC values.
!       02/19/2010 Estimate using Adiku eqn.
!       Ultimately, we want to require OC in the top layers.  But some of our DSSAT
!       soils do not currently include OC, so estimate with Adiku eqn. and give
!       warning.
          IF (.NOT. NO_OC) THEN
!           First layer with missing data - write general message and header
            MSG(1) = "Organic C data in one or more soil " //
     &        "layers are zero or missing."
            MSG(2) = "A minimum value of OC in this layer " // 
     &        " will be estimated using soil texture."
            MSG(3) = "      Depth  Clay  Silt    OC"
            MSG(4) = "Layer  (cm)   (%)   (%)   (%)"
!                         1    42  63.2  22.1  0.97
            NMSG = 4
            IF (L > 1) THEN
              DO i = 1, L-1
                NMSG = NMSG + 1
                WRITE(MSG(NMSG),'(I5,I6,2F6.1,F6.2)') 
     &              i, NINT(DS(i)), CLAY(i), SILT(i), OC(i)
              ENDDO
            ENDIF
          ENDIF

          OC(L) = (0.15 * (CLAY(L) + SILT(L)) + 0.69) / 10.    !g/100g
          NMSG = NMSG+1
          WRITE(MSG(NMSG),'(I5,I6,2F6.1,F6.2," (estimated)")') 
     &          L, NINT(DS(L)), CLAY(L), SILT(L), OC(L)
          NO_OC = .TRUE.
        ELSEIF (NO_OC) THEN
          NMSG = NMSG + 1
          WRITE(MSG(NMSG),'(I5,I6,2F6.2,F6.3)') 
     &              L, NINT(DS(L)), CLAY(L), SILT(L), OC(L)
        ENDIF
      ENDDO
      IF (NO_OC) THEN
        CALL WARNING(NMSG,ERRKEY,MSG)
      ENDIF

!     Define the type of soil layer. 
      Call SoilLayerClass(ISWITCH, 
     &    MULTI, DS, NLAYR, SLDESC, TAXON,                !Input
     &    CaCO3, PH, CEC, Clay, SOILLAYERTYPE)            !Output 

!     Warning message for Century
!      (non-sequenced runs or any first run)
      IF (MESOM == 'P' .AND. 
     &   (INDEX('QFNY',RNMODE) .LE. 0 .OR. 
     &            (RUN .EQ. 1 .AND. REPNO .EQ. 1))) THEN
!       Texture data missing - write message to WARNING.OUT file.
        IF (NOTEXTURE) THEN
            MSG(1)='The CENTURY-based soil-organic-matter module needs'
            MSG(2)='soil texture data, but the specified soil profile '
            MSG(3)='does not have those for some layers. The model will'
            MSG(4)='run with the default 30% clay, 30% silt, 40% sand. '
            MSG(5)='This may give incorrect results.'
        CALL WARNING(5, ERRKEY, MSG)
        ENDIF

      ENDIF     !End of IF block on MESOM & RNMODE

!-----------------------------------------------------------------------
      DO L = 1, NLAYR
!       Conversion from kg/ha to ppm (or mg/l).  Recalculate daily.
        KG2PPM(L) = 10.0 / (BD(L) * DLAYR(L))   
        POROS(L)  = 1.0 - BD(L) / 2.65
        IF (POROS(L) < DUL(L)) POROS(L) = SAT(L)

        IF (TOTN(L) > 1.E-5) THEN
!         Use inorganic N values to calculate organic N in kg/ha
          NO3(L) = AMAX1 (NO3(L), 0.01) !ppm
          NH4(L) = AMAX1 (NH4(L), 0.01) !ppm
!         TOTN in %
          TotOrgN(L) = (TOTN(L)*1.E4 - NO3(L) - NH4(L)) / KG2PPM(L)
          TotOrgN(L) = MAX(0.0, TotOrgN(L))
        ELSE
          TotOrgN(L) = -99.
        ENDIF

!       Remove this ksat estimation 
!       It causes problems when SAT and DUL are close. (KJB/JWJ - India workshop 2011)
!!       Calculate Ksat (SWCN) if not provided
!        IF (SWCN(L) < -1.E-6) THEN
!!         Eqn. 10 from 
!!         Suleiman, A.A., J.T. Ritchie. 2004. Modifications to the DSSAT vertical 
!!           drainage model for more accurate soil water dynamics estimation. 
!!           Soil Science 169(11):745-757.
!          SWCN(L) = 75. * ((SAT(L) - DUL(L)) / DUL(L))**2. / 24. !cm/h
!        ENDIF
      ENDDO

!-----------------------------------------------------------------------
!     Compute vanGenuchten parameters which describe water retention curve
!     Calculate all VG parameters if any one is missing
      VG_OK  = .TRUE.
      DO L = 1, NLAYR
        IF (alphaVG(L) < -1.E-6) VG_OK = .FALSE.
        IF (mVG(L)     < -90.)   VG_OK = .FALSE.   !Negative values OK
        IF (nVG(L)     < -1.E-6) VG_OK = .FALSE.
      ENDDO

      NMSG = 2
      DO L = 1, NLayr
!       Brooks & Corey parameters
	  call calBrokCryPara(TEXTURE(L), SAT(L), LL(L),    !Input
     &     DUL(L),                                        !Input
     +     wcr_temp, Hb(L), lambda(L))                 !output

        IF (WCR(L) < -1.E-6) THEN
!         Replace individual missing residual water content values.
          WCR(L) = wcr_temp
        ENDIF

!!       Calculate Ksat (SWCN) if not provided
!        IF (SWCN(L) <= -1.E-6) THEN 
!!         Ksat calculation based on paper "Soil Water Characteristic Estimates 
!!         by Texture and Organic Matter for Hydrologic Solutions" by K. E. Saxton 
!!         and W. J. Rawls, Aug. 2006.
!!         Ksat in cm/h
!!         SWCN(L) = 1930. * ((SAT(L) - DUL(L))**(3.-Lambda(L)))/10.0
!
!!         Eqn. 10 from 
!!         Suleiman, A.A., J.T. Ritchie. 2004. Modifications to the DSSAT vertical 
!!           drainage model for more accurate soil water dynamics estimation. 
!!           Soil Science 169(11):745-757.
!          SWCN(L) = 75. * ((SAT(L) - DUL(L)) / DUL(L))**2. / 24. !cm/h
!
!          NMSG = NMSG + 1
!          WRITE(MSG(NMSG),'(I5,F8.3,3F6.3)') 
!     &        L, SWCN(L), LL(L), DUL(L), SAT(L)
!        ENDIF

        IF (.NOT. VG_OK) THEN
!         van Genuchten parameters fit from known soil properties based on RETC code
          CALL RETC_VG(TEXTURE(L), SWCN(L), LL(L), DUL(L),  !Input
     &      SAT(L), wcr(L), Hb(L),                          !Input
     &      alphaVG(L), mVG(L), nVG(L))                     !Output
        ENDIF
      ENDDO

      IF (NMsg > 2) THEN
        MSG(1)='Ksat value(s) calculated from known soil parameters'
        MSG(2)='Layer    KSat    LL   DUL   SAT'
        CALL INFO(NMSG, ERRKEY, MSG)
      ENDIF

!     Write RETC info to info.out
      MSG(1) = "Soil RETC parameters: "
      MSG(2) = 
     &  " LYR DS  SWCN   WCR    Hb Lambd  Alpha     m     n Texture"
      DO L = 1, NLAYR
        WRITE(MSG(2+L),'(I3,I4,F6.2,F6.3,F6.1, F6.3, 1x, 3F6.3, 1x,A)')
     &      L, NINT(DS(L)), SWCN(L), wcr(L), Hb(L), lambda(L),
     &      alphaVG(L),mVG(L), nVG(L), TEXTURE(L) 
      ENDDO
      NMSG = NLAYR + 2
      CALL INFO(NMSG, "RETC", MSG)

!-----------------------------------------------------------------------
      CALL SoilLayerText(DS, NLAYR,                       !Input
     &    LayerText)                                      !Output

!-----------------------------------------------------------------------
!!     Get root growth impedance factor
!      CALL IPSAUX(CONTROL, RGIMPF) 

!-----------------------------------------------------------------------
!     Copy soil parameters to soil variable type, which is defined in
!     ModuleDefs.for.  
      SOILPROP % ADCOEF = ADCOEF 
      SOILPROP % BD     = BD     
      SOILPROP % CACO3  = CACO3     
      SOILPROP % CEC    = CEC    
      SOILPROP % CLAY   = CLAY   
      SOILPROP % CN     = CN     
      SOILPROP % DLAYR  = DLAYR  
      SOILPROP % DMOD   = DMOD        !formerly SLNF   
      SOILPROP % DS     = DS     
      SOILPROP % DUL    = DUL    
      SOILPROP % ETDR   = 0.2     !tile drainage rate   
      SOILPROP % EXTP   = EXTP          
      SOILPROP % KG2PPM = KG2PPM  !conversion factor 
      SOILPROP % LL     = LL     
      SOILPROP % NLAYR  = NLAYR
      SOILPROP % OC     = OC     
      SOILPROP % PH     = PH     
      SOILPROP % PHKCL  = PHKCL  
      SOILPROP % PTERMA = PTERMA  
      SOILPROP % PTERMB = PTERMB  
      SOILPROP % POROS  = POROS
!     SOILPROP % RGIMPF = RGIMPF  !Root growth impedance factor    
      SOILPROP % SALB   = SALB  
      SOILPROP % MSALB  = SALB
      SOILPROP % CMSALB = SALB 

      SOILPROP % SAND   = SAND   
      SOILPROP % SASC   = SASC   
      SOILPROP % SAEA   = SAEA   
      SOILPROP % SAT    = SAT    
      SOILPROP % SILT   = SILT   
      SOILPROP % SLNO   = SLNO  
      SOILPROP % SLPF   = SLPF  
      SOILPROP % SMPX   = SMPX  
      SOILPROP % STONES = STONES 
      SOILPROP % SWCN   = SWCN   
      SOILPROP % SWCON  = SWCON  
      SOILPROP % TOTBAS = TOTBAS   
      SOILPROP % TEXTURE= TEXTURE
      SOILPROP % TOTN   = TOTN   
      SOILPROP % TotOrgN= TotOrgN   
      SOILPROP % U      = U     
      SOILPROP % WR     = WR      !SHF
      SOILPROP % WCR    = WCR     !residual water content

      SOILPROP % TOTP   = TOTP
      SOILPROP % ORGP   = ORGP
      SOILPROP % EXCA   = EXCA
      SOILPROP % EXNA   = EXNA
      SOILPROP % EXK    = EXK 

      SOILPROP % alphaVG= alphaVG
      SOILPROP % mVG    = mVG
      SOILPROP % nVG    = nVG
      SOILPROP % WCR    = WCR   !residual water content

      SOILPROP % SOILLAYERTYPE = SOILLAYERTYPE
      SOILPROP % LayerText     = LayerText
      SOILPROP % SLDESC        = SLDESC
      SOILPROP % TAXON         = TAXON

      SOILPROP % COARSE = COARSE
      
      CALL SETPM(SOILPROP)

      CALL PUT(SOILPROP)

      IF (ISWWAT == 'N') RETURN

      CALL PRINT_SOILPROP(SOILPROP)
      
!-----------------------------------------------------------------------
!     Initialization
C  Designate the CN2, BD, and SWCN values from CROPGRO as the settled
C  values to be used in changing curve number, bulk density, and 
C  hydraulic conductivity of the top 3 soil layers as affected by 
C  tillage and rainfall kinetic energy
      BD_INIT   = BD
      CN_INIT   = CN
      DLAYR_INIT= DLAYR
      DS_INIT   = DS
      DUL_INIT  = DUL
      LL_INIT   = LL
      OC_INIT   = OC
!     RGIF_INIT = RGIMPF
      SAT_INIT  = SAT
      TOTN_INIT = TOTN
      TotOrgN_INIT = TotOrgN
      SWCN_INIT = SWCN
      SW_INIT   = SW

      BD_SOM   = BD
      DUL_SOM  = DUL
      DS_SOM   = DS
      LL_SOM   = LL
      DLAYR_SOM= DLAYR

      TILLED    =.FALSE.
      CRAIN     = 0.
      LCRAIN    = 0.

!***********************************************************************
!***********************************************************************
!     Seasonal initialization
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      IF (ISWWAT == 'N') RETURN

      ISWTIL = ISWITCH % ISWTIL
      NTIL = TILLVALS % NTIL

      CALL OPSOILDYN(CONTROL, DYNAMIC, ISWITCH, 
     &  BD, BD_SOM, CN, CRAIN, DLAYR, DUL, KECHGE, LL, PRINT_TODAY, SAT,
     &  SOILCOV, SUMKE, SWCN, TOTAW)

!     Skip initialization for sequenced runs:
      IF (INDEX('FQ',RNMODE) > 0 .AND. RUN /= 1) RETURN

      FIRST = .TRUE.
      TILLED    =.FALSE.

      SUMKE  = 0.0
      KECHGE = 0.0
      CRAIN  = 0.0
      LCRAIN = 0.0

      BD    = BD_INIT  
      CN    = CN_INIT  
      DLAYR = DLAYR_INIT
      DS    = DS_INIT  
      DUL   = DUL_INIT 
      LL    = LL_INIT  
      OC    = OC_INIT
      SAT   = SAT_INIT 
      SWCN  = SWCN_INIT
      TOTN  = TOTN_INIT
      TotOrgN = TotOrgN_INIT

      SW    = SW_INIT

      BD_SOM   = BD
      DUL_SOM  = DUL
      DS_SOM   = DS
      LL_SOM   = LL
      DLAYR_SOM= DLAYR

!-----------------------------------------------------------------------
      DO L = 1, NLAYR
!       Conversion from kg/ha to ppm (or mg/l).  Recalculate daily.
        KG2PPM(L) = 10.0 / (BD(L) * DLAYR(L))   
        POROS(L)  = 1.0 - BD(L) / 2.65
        IF (POROS(L) < DUL(L)) POROS(L) = SAT(L)
      ENDDO

      SOILPROP % BD    = BD
      SOILPROP % CN    = CN
      SOILPROP % DLAYR = DLAYR
      SOILPROP % DS    = DS    
      SOILPROP % DUL   = DUL   
      SOILPROP % LL    = LL    
      SOILPROP % KG2PPM= KG2PPM    
      SOILPROP % OC    = OC
      SOILPROP % POROS = POROS
      SOILPROP % SAT   = SAT   
      SOILPROP % SWCN  = SWCN  
      SOILPROP % TOTN  = TOTN
      SOILPROP % TotOrgN=TotOrgN

!***********************************************************************
!***********************************************************************
!     Daily rate calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
      IF (ISWWAT == 'N') RETURN

!     Initial SOM not established until end of SEASINIT section so 
!     remember initial values here.  Units are kg[Organic matter]/ha
      IF (FIRST) THEN
!       Save initial value of SOM
        SomLit_init = SomLit
        DO L = 1, NLAYR
!         (See conversion explanation below)
          SOM_PCT(L) = SomLit(L) * 1.E-5 / (BD(L) * DLAYR(L)) *100.

!         This can result in negative BD_mineral for very high organic
!           matter content.  
!!         For Izaurralde method, calculate initial mineral BD
!          BD_Mineral(L) =(100.-SOM_PCT(L))/(100./BD(L)-SOM_PCT(L)/0.224)

!         Instead, calculate a BD_calc. Use the 
!           change to BD_calc to scale BD, which was input by user.
          BD_calc(L) = 100./(SOM_PCT(L)/0.224 + (100.-SOM_PCT(L))/2.65)
        ENDDO

!       Set initial array
        SOM_PCT_init = SOM_PCT
        BD_calc_init = BD_calc

!       Print initial values
        Print_today = .TRUE.
        FIRST = .FALSE.
      ENDIF

!     ------------------------------------------------------------------
      CALL ALBEDO_avg(KTRANS, MEINF, MULCH, SOILPROP, SW(1), XHLAI)

!     IF (INDEX('RSN',MEINF) .LE. 0) THEN
      IF (INDEX('RSM',MEINF) > 0) THEN 

!       ---------------------------------------------------
!       Update combined soil/mulch albedo
!       Transfer local values from constructed variables
        MULCHCOVER = MULCH % MULCHCOVER
        MULCHALB   = MULCH % MULCHALB

!       ---------------------------------------------------
!       Update BD, DLAYR, DUL, LL based on changes to soil organic matter 
!       CHP 4/11/2006
!       These SOM-revised values will be the new "base" values to which
!       tilled soil properties will return.
!       Update soil water holding capacity daily due to changes in bulk
!       density and organic carbon content of soil.
!       These equations based on Gupta & Larson 1979, Adams 1973, Izaurralde 2006
        DO L = 1, NLAYR
!         Change to SOM since initialization
!         SOM units have already been converted to OM (not C)
          dSOM = SomLit(L) - SomLit_init(L) !kg[OM]/ha
          
          IF (dSOM < 0.01) THEN
!           No changes to soil properties due to organic matter
            BD_SOM(L)   = BD_INIT(L)
            DLAYR_SOM(L)= DLAYR_INIT(L)
            DS_SOM(L)   = DS_INIT(L)
            DUL_SOM(L)  = DUL_INIT(L)
            LL_SOM(L)   = LL_INIT(L)

          ELSE
!           Need to update soil properties based on changes to SOM

!           First -- modify layer thickness
!           dDlayr = change in layer thickness due to addition (or depletion)
!             of organic matter.
!           Mean bulk density of organic matter is 0.224 g/cm3 (from Adams,1973)

!                    kg[om]   10^3 g     ha        m2           cm3
!           dDlayr = ------ * ------ * ------- * -------- * -----------  
!                      ha       kg     10^4 m2   10^4 cm2   0.224 g[om]

            dDLAYR_SOM = dSOM * 4.46E-5
!              cm      =kg/ha * 4.46E-5

!           New base layer thickness and depths
            DLAYR_SOM(L) = DLAYR_INIT(L) + dDLAYR_SOM

!           -------------------------------------------------------
!           Change SOM from kg/ha to percent
            SOM_PCT(L) = SomLit(L) * 1.E-5/(BD_SOM(L)*DLAYR_SOM(L))*100.

!            BD_SOM(L) = 100.0 / 
!     &        (SOM_PCT(L) / 0.224 + (100. - SOM_PCT(L)) / BD_mineral(L))
!
            BD_calc(L) = 100.0 / 
     &        (SOM_PCT(L) / 0.224 + (100. - SOM_PCT(L)) / 2.65)

            BD_SOM(L) = BD_calc(L) / BD_calc_init(L) * BD_init(L)

!           Limit BD to realistic values
!           BD_SOM(L) = MIN(BD_SOM(L), 1.8)
!           BD_SOM(L) = MAX(BD_SOM(L), 0.25)
!           Upper limit for BD_SOM
            BD_SOM(L) = MIN(BD_SOM(L), BD_INIT(L)*1.2, 1.80) 
!           Lower limit for BD_SOM
            BD_SOM(L) = MAX(BD_SOM(L), BD_INIT(L)*0.8, 0.95) 
            dBD_SOM = BD_SOM(L) - BD_INIT(L)

!           -------------------------------------------------------
!           Update DS
            IF (L == 1) THEN
              DS_SOM(1) = DLAYR_SOM(1)
            ELSE
              DS_SOM(L) = DS(L-1) + DLAYR_SOM(L)
            ENDIF

!           Change SOM from kg/ha to percent
            SOM_PCT(L) = SomLit(L) * 1.E-5/(BD_SOM(L)*DLAYR_SOM(L))*100.
!                         kg[OM]    g[OM]/cm2     cm3       1
!                      = -------- * --------- * -------  * ---- * 100%
!                           ha      kg[OM]/ha   g[soil]     cm
!
!                      = g[OM]/g[soil] * 100%

!           Change in %SOM
            dOC = SOM_PCT(L) - SOM_PCT_init(L)

!           Equation to modify DUL depends on soil texture (Gupta & Larson, 1979)
            IF (COARSE(L)) THEN
!             Coarse soils  --  use DUL10
              dDUL_SOM = 0.004966 * dOC - 0.2423 * dBD_SOM 
            ELSE
!             Other soils -- use DUL33
              dDUL_SOM = 0.002208 * dOC - 0.1434 * dBD_SOM 
            ENDIF
            DUL_SOM(L) = DUL_INIT(L) + dDUL_SOM

!           Lower limit
            dLL_SOM = 0.002228 * dOC + 0.02671 * dBD_SOM
            LL_SOM(L)  = LL_INIT(L) + dLL_SOM

!            IF (L==1) WRITE(1000,*)dOC, dBD_SOM, dLL_SOM, LL_SOM(1)
          ENDIF
        ENDDO
      ENDIF

!     Tillage effects applied to SOM modified values
      IF (INDEX('YR',ISWTIL) > 0 .AND. NTIL .GT. 0 .AND.
     &    YRDOY .EQ. TILLVALS % TILDATE) THEN
        BD_BASE   = BD_SOM
        CN_BASE   = CN_INIT
        DS_BASE   = DS_SOM
!       RG_BASE   = RGIF_INIT
        SAT_BASE  = SAT_INIT
        SC_BASE   = SWCN_INIT
        DL_BASE   = DLAYR_INIT

        CRAIN = 0.0
        LCRAIN = 0.0

!       TillEvent computes the effects of today's tillage event
!         on soil properties -- effects are applied in integr section.
        CALL TillEvent(CONTROL, 
     &    BD, BD_BASE, CN, CN_BASE, DLAYR, DL_BASE,       !Input
     &    DS_BASE, SAT, SAT_BASE, SWCN, SC_BASE,          !Input
     &    NLAYR, TILLVALS,                                !Input
     &    BD_TILLED, CN_TILLED, DL_TILLED,                !Output
     &    DS_TILLED, SAT_TILLED, SC_TILLED)               !OutpuT

      ENDIF

      CALL PUT(SOILPROP)

!***********************************************************************
!***********************************************************************
!     Daily integration
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
      IF (ISWWAT == 'N') RETURN

!-----------------------------------------------------------------------
!  Initialize surface soil properties 
!-----------------------------------------------------------------------
!     Update Organic C variable OC (%):
      DO L = 1, NLAYR
        OC(L) = SomLitC(L) * 1.E-5 / (BD(L) * DLAYR(L)) * 100.
!                  kg[C]     g/cm2     cm3       1
!             =  --------  * ----- * ------- * ----- * 100%
!                   ha       kg/ha   g[soil]     cm
!
!             = g[C]/g[soil] * 100%
      ENDDO

C     Calculate fraction of soil covered by canopy and residue.
      !CANCOV = 1.0 - EXP(-0.75 * XHLAI)
      !SOILCOV = CANCOV + MULCHCOVER * (1.0 - CANCOV)
      SOILCOV = MULCHCOVER 

      IF ((INDEX('YR',ISWTIL) > 0 .AND. NTIL .GT. 0) .OR. TILLED) THEN

        TILDATE = TILLVALS % TILDATE
        TIL_IRR = TILLVALS % TIL_IRR
        CRAIN = CRAIN + RAIN + TIL_IRR

        IF (YRDOY .EQ. TILDATE) THEN
!         Update soil properties
          BD     = BD_TILLED   
          CN     = CN_TILLED 
          DLAYR  = DL_TILLED
          DS     = DS_TILLED
!         RGIMPF = RG_TILLED  
          SAT    = SAT_TILLED 
          SWCN   = SC_TILLED 

          CUMDEP = 0.0
          SUMKE  = 0.0
          SUMKET = 0.00217 * CRAIN 
C** WDB
C LRAIN is cumulative rainfall from beginning of simulation until the last rainfall event        
          LCRAIN = CRAIN
C** WDB
          TILLED = .TRUE.

C-----------------------------------------------------------------------
!         Call mixing routine for soil properties
          MIXPCT = TILLVALS % TILMIX
          TDEP = TILLVALS % TILDEP

          DO L = 1, NLAYR
!           Depths of soil components (cm) - use yesterday's DLAYR
            SAND_PREMIX(L) = SOILPROP % SAND(L) * DLAYR(L) / 100.
            CLAY_PREMIX(L) = SOILPROP % CLAY(L) * DLAYR(L) / 100.
            SILT_PREMIX(L) = SOILPROP % SILT(L) * DLAYR(L) / 100.
          ENDDO

          CALL SoilMixing(DLAYR, MIXPCT,NLAYR,TDEP,SAND_PREMIX,SAND_MIX)
          CALL SoilMixing(DLAYR, MIXPCT,NLAYR,TDEP,CLAY_PREMIX,CLAY_MIX)
          CALL SoilMixing(DLAYR, MIXPCT,NLAYR,TDEP,SILT_PREMIX,SILT_MIX)
!         Will also need to call SoilMixing for organic C, N, water 

!         Calculate alternate layer depths corresponding to tilled BD's
          DO L = 1, NLAYR
            IF (ABS(BD(L) - BD_SOM(L)) .GE. 0.01) THEN
              DLAYR(L) = DLAYR_SOM(L) * BD_SOM(L) / BD(L)
            ELSE
              DLAYR(L) = DLAYR_SOM(L)
            ENDIF

!           Depths to % 
            SOILPROP % SAND(L) = SAND_MIX(L) * 100. / 
     &        (SAND_MIX(L) + CLAY_MIX(L) + SILT_MIX(L))
            SOILPROP % CLAY(L) = CLAY_MIX(L) * 100. / 
     &        (SAND_MIX(L) + CLAY_MIX(L) + SILT_MIX(L))
            SOILPROP % SILT(L) = SILT_MIX(L) * 100. / 
     &        (SAND_MIX(L) + CLAY_MIX(L) + SILT_MIX(L))

!           Organic C in %
            OC(L) = SomLitC(L) * 1.E-5 / (BD(L) * DLAYR(L)) * 100.
          ENDDO
        END IF

C-----------------------------------------------------------------------
C       Calculate changes in bulk density and hydraulic conductivity due 
C       to rainfall kinetic energy.
        IF ((RAIN > 0.0 .OR. TIL_IRR > 0.0) .AND. TILLED) THEN
          CUMDEP  = 0.0
          TILLED = .FALSE.      !Set back to true in soil layer loop

C** WDB ORIG   SUMKE = 0.00217 * CRAIN - SUMKET
C** WDB ORIG   SUMKE = (1.0 - SOILCOV) * SUMKE
C Problem was that we were allowing todays soilcov to affect
C sumke for all previous rainfall events. This would drive
C sumke lower rather than higher for some rainfall events, 
c depending upon soil cover. 
          IF (ABS(CRAIN - LCRAIN) > 1.E-3) THEN
            SUMKE = SUMKE + 0.00217*(CRAIN-LCRAIN)*(1-SOILCOV)
          ELSE
!           Allows for rain or tillage on day of tillage
            SUMKE = SUMKE + 0.00217*(RAIN + TIL_IRR)*(1-SOILCOV)
          ENDIF
          LCRAIN = CRAIN 
  
C         Calculate changes in runoff curve number to account for 
C         changes in surface roughness.
          AS = 0.205 * OC(1)
          SRate = 30.
          RSTL = SRate * (1.0 - AS)
          RSTL = MAX(0.0, RSTL)   !CHP 12/15/2016
C**        RSTL = 5.0 * (1.0 - AS)
          KECHGE(0) = EXP(-RSTL * SUMKE) 
          CN = CN_INIT + (CN_TILLED - CN_INIT) * KECHGE(0)
c         IF (CN .GT. CN_INIT ) CN = CN_INIT

          DO L = 1, NLAYR
            MCUMDEP = CUMDEP + DLAYR(L) / 2.0
            CUMDEP  = CUMDEP + DLAYR(L)

            AS = 0.205 * OC(L)
C** WDB          RSTL = 5.0 * (1.0 - AS)
            RSTL = SRate * (1.0 - AS)
            RSTL = MAX(0.0, RSTL)   !CHP 12/15/2016
c** wdb orig          SUMKEL = SUMKE * EXP(-0.15*MCUMDEP)
            SUMKEL = SUMKE * EXP(-0.05*MCUMDEP)
            KECHGE(L) = EXP(-RSTL * SUMKEL)

            BD(L)    =BD_SOM(L)   +(BD_TILLED(L) -  BD_SOM(L))*KECHGE(L)
            SWCN(L)  =SWCN_INIT(L)+(SC_TILLED(L)-SWCN_INIT(L))*KECHGE(L)
            SAT (L)  =SAT_INIT(L) +(SAT_TILLED(L)-SAT_INIT(L))*KECHGE(L)
!           RGIMPF(L)=RGIF_INIT(L)+(RG_TILLED(L)-RGIF_INIT(L))*KECHGE(L)

           !If bulk density is not back to original value for any layer,
           !  soil is still in "tilled" condition.
            IF (ABS(BD(L) - BD_SOM(L)) .GE. 0.01) THEN
              TILLED = .TRUE.
              DLAYR(L) = DLAYR_SOM(L) * BD_SOM(L) / BD(L)
            ELSE
              BD(L)     = BD_SOM(L)
              DLAYR(L)  = DLAYR_SOM(L)
!             RGIMPF(L) = RGIF_INIT(L)
              SAT(L)    = SAT_INIT(L)
              SWCN(L)   = SWCN_INIT(L)
              PRINT_TODAY = .TRUE. !Override print rules and print today
             ENDIF
           ENDDO
        ENDIF

        IF (.NOT. TILLED) THEN
          !Bulk density is now back to original values for each layer
          !Set all soils properties back to original values
          BD     = BD_SOM
          CN     = CN_INIT
          DLAYR  = DLAYR_SOM
!         RGIMPF = RGIF_INIT
          SAT    = SAT_INIT
          SWCN   = SWCN_INIT
          SUMKE  = 0.0
        ENDIF

      ELSE
        BD     = BD_SOM
        DLAYR  = DLAYR_SOM
      ENDIF

      DUL = DUL_SOM
      LL  = LL_SOM

!     Doesn't actually change daily if both BD and DLAYR are updated
!     simultaneously.
      TOTAW = 0.0
      DO L = 1, NLAYR
!       Conversion from kg/ha to ppm (or mg/l).  Recalculate daily.
        KG2PPM(L) = 10.0 / (BD(L) * DLAYR(L))

!       Recompute DS for updated DLAYR
        IF (L == 1) THEN
          DS(1) = DLAYR(1)
        ELSE  
          DS(L) = DS(L-1) + DLAYR(L)
        ENDIF 

!       Available water capacity (mm)
        TOTAW = TOTAW + (DUL(L) - LL(L)) * DLAYR(L) * 10.
        POROS(L)  = 1.0 - BD(L) / 2.65
        IF (POROS(L) < DUL(L)) POROS(L) = SAT(L)
      
!       2024-08-30 FO - Protection for LL < DUL < SAT
        IF ((DUL(L) - SAT(L)) .GT. 0.0) THEN
          SAT(L) = DUL(L) + 0.01
          MSG(1) = 'DUL greater than SAT due to Soil Organic Matter'
          WRITE(MSG(2),'(A,I1,A,I1,A)') 'Setting: SAT(Layer = ',L,
     &    ') = DUL(Layer = ',L,') + 0.01'
          CALL WARNING(2,ERRKEY,MSG)
        ENDIF
        IF ((LL(L) - DUL(L)) .GT. 0.0) THEN
          LL(L) = DUL(L) - 0.01
          MSG(1) = 'LL greater than DUL due to Soil Organic Matter'
          WRITE(MSG(2),'(A,I1,A,I1,A)') 'Setting: LL(Layer = ',L,
     &    ') = DUL(Layer = ',L,') + 0.01'
          CALL WARNING(2,ERRKEY,MSG)
        ENDIF 
      ENDDO

      SOILPROP % BD     = BD     
      SOILPROP % CN     = CN     
      SOILPROP % DLAYR  = DLAYR  !thickness of tilled soil layers 
      SOILPROP % DS     = DS     !depth of tilled soil layers
      SOILPROP % DUL    = DUL    
      SOILPROP % KG2PPM = KG2PPM
      SOILPROP % LL     = LL    
      SOILPROP % OC     = OC
      SOILPROP % SAT    = SAT    
      SOILPROP % SWCN   = SWCN 
      SOILPROP % POROS  = POROS  

      CALL PUT(SOILPROP)

!***********************************************************************
!***********************************************************************
!     Daily output
!***********************************************************************
      ELSEIF (DYNAMIC == OUTPUT .OR. DYNAMIC == SEASEND) THEN
!-----------------------------------------------------------------------
      IF (ISWWAT == 'N') RETURN

      CALL OPSOILDYN(CONTROL, DYNAMIC, ISWITCH, 
     &  BD, BD_SOM, CN, CRAIN, DLAYR, DUL, KECHGE, LL, PRINT_TODAY, SAT, 
     &  SOILCOV, SUMKE, SWCN, TOTAW)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************

      RETURN
      END SUBROUTINE SOILDYN

!========================================================================
! SoilDyn variable definitiions - updated 08/20/2003
!========================================================================
! ADCOEF(L) Anion adsorption coefficient for soil layer L;  for reduced 
!             anion (nitrate) flow in variable-charge soils (ADCOEF = 0 
!             implies no anion retention) (cm3 (H2O] / g [soil])
! BD(L)     Bulk density, soil layer L (g [soil] / cm3 [soil])
! CEC(L)    Cation exchange capacity, soil layer L (cmol kg-1)
! CHAR      Contains the contents of last record read 
! CLAY(L)   Percentage of clay in soil layer L 
! CN        Runoff Curve Number - measure of runoff potential based on soil 
!             type and current soil water content. 
! CONTROL   Composite variable containing variables related to control 
!             and/or timing of simulation.  The structure of the variable 
!             (ControlType) is defined in ModuleDefs.for. 
! DLAYR(L)  Thickness of soil layer L (cm)
! DMOD      Factor to adjust the mineralization rate for certain atypical 
!             soils (range 0-1) 
! DS(L)     Cumulative depth in soil layer L (cm)
! DUL(L)    Volumetric soil water content at Drained Upper Limit in soil 
!             layer L (cm3[water]/cm3[soil])
! ERRKEY    Subroutine name for error file 
! ERRNUM    Error number for input 
! FILEIO    Filename for input file (e.g., IBSNAT35.INP) 
! FOUND     Indicator that good data was read from file by subroutine FIND 
!             (0 - End-of-file encountered, 1 - NAME was found) 
! KG2PPM(L) Conversion factor to switch from kg [N] / ha to ug [N] / g 
! LL(L)     Volumetric soil water content in soil layer L at lower limit
!            (cm3 [water] / cm3 [soil])
! LNUM      Current line number of input file 
! LUNIO     Logical unit number for FILEIO 
! MESOM     Method for soil N computations ('G'=Godwin or Ceres-based, 
!             'P'=Parton or Century-based (future)) 
! MSG       Text array containing information to be written to WARNING.OUT 
!             file. 
! NL        Maximum number of soil layers = 20 
! NLAYR     Actual number of soil layers 
! NOTEXTURE Logical variable which indicates whether soil texture data is 
!             available 
! OC(L)     Organic carbon content of layer (%)
! PH(L)     pH in soil layer L 
! PHKCL(L)  pH in buffer, soil layer L 
! SALB      Bare soil albedo (fraction)
! SAT(L)    Volumetric soil water content in layer L at saturation
!            (cm3 [water] / cm3 [soil])
! SECTION   Section name in input file 
! SILT(L)   Percentage of silt in soil layer L 
! SLDESC    Soil description or local classification 
! SLDP      Soil profile depth (cm)
! SLNO      Soil identifier (Institute + Site + Year + Soil) 
! SLPF      Soil photosynthesis factor, 0 to 1 scale 
! SLSOUR    Souce of soil information 
! SLTXS     Soil texture code 
! SOILPROP  Composite variable containing soil properties including bulk 
!             density, drained upper limit, lower limit, pH, saturation 
!             water content.  Structure defined in ModuleDefs. 
! STONES(L) Coarse fraction (>2 mm) (%)
! SWCN(L)   Saturated hydraulic conductivity in layer L (cm/hr)
! SWCON     Soil water conductivity constant; whole profile drainage rate 
!             coefficient (1/d)
! TAXON     Soil family, SCS system 
! TOTN(L)   Total N in soil (%)
! U         Evaporation limit (cm)
! WR(L)     Root hospitality factor, used to compute root distribution 
!========================================================================
!     End SOILDYN module
!========================================================================


!=======================================================================
      SUBROUTINE ALBEDO_avg(KTRANS, MEINF, MULCH, SOILPROP, SW1, XHLAI)
      !Update soil albedo based on mulch cover and soil water content in
      !top layer

!-----------------------------------------------------------------------
!  REVISION HISTORY
!  07/24/2006 CHP Written
!  10/23/2007 CHP Use species-dependant KTRANS for canopy 
!                    light interception
!-----------------------------------------------------------------------
 
      USE ModuleDefs
      IMPLICIT NONE

      TYPE (MulchType) MULCH    !Surface mulch properties
      TYPE (SoilType)  SOILPROP !Soil properties

      CHARACTER*1 MEINF
      REAL CANCOV, CMSALB, FF, KTRANS, MSALB, MULCHALB, MULCHCOVER
      REAL SW1, SWALB, XHLAI

!     ---------------------------------------------------
!     Calculate albedo changes with soil water content
      FF = (SW1 - 0.03) / (SOILPROP % DUL(1) - 0.03)
      FF = MAX(0.0, MIN(2.0, FF))
      SWALB = SOILPROP % SALB * (1.0 - 0.45 * FF)

!     1/18/2008 chp change albedo calculations back to original at GH's request.
!     Probably temporary-- temp chp
!!     chp 12/21/2007
!!     Based on Idso, Jackson et al., 1975. The dependence of bare soil 
!!     albedo on soil water content. Journal of Applied Meteorology 14, 109-113. 
!!     Max albedo ~0.30 at air dry, Min albedo ~DUL
!      SWAD = 0.30 * LL1 !JTR 11/28/2006
!      FF = (SW1 - SWAD) / (SOILPROP % DUL(1) - SWAD)
!      FF = MAX(0.0,MIN(1.0,FF))
!!     SWALB = SOILPROP % SALB * FF + 0.30 * (1. - FF)
!
!!     Dry soil albedo as function of wet soil albedo
!!     Relationship from data of Post, et al.,  Soil Sci. Soc. Am. J. 64:1027-1034 (2000).
!      Wet_alb = SOILPROP % SALB
!      Dry_alb = Wet_alb * 1.7
!      SWALB = Wet_alb * FF + Dry_alb * (1. - FF)
!
!!     Lobell and Asner, Moisture Effects on Soil Reflectance. Soil Sci. Soc. Am. J. 66:722-727 (2002).
!!     Uses SAT to represent wet soils instead of DUL
!!     R = Rsat + (Rdry - Rsat) * exp(-c*SW)
!!     No data to parameterize, so don't use this method.

!     IF (INDEX('RSN',MEINF) .LE. 0) THEN
      IF (INDEX('RSM',MEINF) > 0) THEN   
!       Update combined soil/mulch albedo
!       Transfer local values from constructed variables
        MULCHCOVER = MULCH % MULCHCOVER
        MULCHALB   = MULCH % MULCHALB

!       Albedo with mulch cover
        MSALB = MULCHCOVER * MULCHALB + (1.0 - MULCHCOVER) * SWALB
      ELSE
        MSALB = SWALB
      ENDIF

!     Albedo with canopy cover
!     CANCOV = 1.0 - EXP(-0.75 * XHLAI)
      CANCOV = 1.0 - EXP(-KTRANS * XHLAI)
      CMSALB = CANCOV * 0.23 + (1.0 - CANCOV) * MSALB

      SOILPROP % CMSALB = CMSALB
      SOILPROP % MSALB  = MSALB
      SOILPROP % SWALB  = SWALB

!!    Temporary -- print soil albedo stuff
!     GET (CONTROL)
!     CALL YR_DOY(CONTROL.YRDOY, YEAR, DOY)
!     WRITE(2250,'(1X,I4,1X,I3.3,1X,I5,8F8.3)') YEAR, DOY, CONTROL.DAS, SOILPROP.SALB, 
!     &      FF, SWALB, MULCHCOVER, MSALB, CANCOV, CMSALB

      RETURN
      END SUBROUTINE ALBEDO_avg
!=======================================================================

!=======================================================================
! Subroutine PRINT_SOILPROP
! Prints soil properties data to INFO.OUT file
      SUBROUTINE PRINT_SOILPROP(SOILPROP)
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  08/20/2008 CHP Written
!-----------------------------------------------------------------------
      USE MODULEDEFS
      USE MODULEDATA
      IMPLICIT NONE
      EXTERNAL INFO
      TYPE (SwitchType) ISWITCH
      TYPE (SoilType) SOILPROP

      CHARACTER (len= 5) SMPX
      CHARACTER (len= 6) TEXT
      CHARACTER (len=10) SLNO
      CHARACTER (len=12) TEXTURE(NL)
      CHARACTER (len=17) SOILLAYERTYPE(NL)
      CHARACTER (len=50) SLDESC, TAXON
      CHARACTER (len=78) MSG(NL+8)

      INTEGER COUNT, I, L, NLAYR, NMSG
      REAL DMOD, SLPF
      REAL SALB

      REAL, DIMENSION(NL) :: BD, CEC, CLAY, DLAYR, DS, DUL
      REAL, DIMENSION(NL) :: KG2PPM, LL, OC, PH, PHKCL
      REAL, DIMENSION(NL) :: SAND, SAT, SILT, STONES, SWCN
      REAL, DIMENSION(NL) :: CACO3, EXTP, ORGP, PTERMA, PTERMB
      REAL, DIMENSION(NL) :: TOTP, TOTBAS, EXCA, EXK, EXNA
      REAL, DIMENSION(NL) :: SASC   !stable organic C
!     SAEA = soil alternate electron acceptors (mol Ceq/m3)
      REAL, DIMENSION(NL) :: SAEA   
      REAL, DIMENSION(NL) :: WCR, alphaVG, mVG, nVG

      REAL CN, SWCON
      REAL, DIMENSION(NL) :: ADCOEF, TOTN, TotOrgN, WR

!     Print column info
      INTEGER, PARAMETER :: MaxParams = 13
      CHARACTER*6, DIMENSION(MaxParams) :: HEADER, UNITS, FMT
      REAL        VALUE(MaxParams,NL)
      LOGICAL     OK(0:MaxParams)

      CALL GET(ISWITCH)

      ADCOEF  = SOILPROP % ADCOEF
      BD      = SOILPROP % BD    
      CACO3   = SOILPROP % CACO3    
      CEC     = SOILPROP % CEC   
      CLAY    = SOILPROP % CLAY  
      CN      = SOILPROP % CN    
      DLAYR   = SOILPROP % DLAYR 
      DMOD    = SOILPROP % DMOD       !formerly SLNF   
      DS      = SOILPROP % DS    
      DUL     = SOILPROP % DUL   
      EXTP    = SOILPROP % EXTP         
      KG2PPM  = SOILPROP % KG2PPM !conversion factor 
      LL      = SOILPROP % LL    
      NLAYR   = SOILPROP % NLAYR 
      OC      = SOILPROP % OC    
      PH      = SOILPROP % PH    
      PHKCL   = SOILPROP % PHKCL 
      PTERMA  = SOILPROP % PTERMA 
      PTERMB  = SOILPROP % PTERMB 
      SALB    = SOILPROP % SALB  
      SALB    = SOILPROP % MSALB 
      SALB    = SOILPROP % CMSALB

      SAND    = SOILPROP % SAND   
      SASC    = SOILPROP % SASC   
      SAEA    = SOILPROP % SAEA   
      SAT     = SOILPROP % SAT    
      SILT    = SOILPROP % SILT   
      SLNO    = SOILPROP % SLNO   
      SLPF    = SOILPROP % SLPF   
      SMPX    = SOILPROP % SMPX   
      STONES  = SOILPROP % STONES 
      SWCN    = SOILPROP % SWCN   
      SWCON   = SOILPROP % SWCON  
      TOTBAS  = SOILPROP % TOTBAS   
      TEXTURE = SOILPROP % TEXTURE
      TOTN    = SOILPROP % TOTN   
      TotOrgN = SOILPROP % TotOrgN   
      WR      = SOILPROP % WR      !SHF
      WCR     = SOILPROP % WCR     !residual water content

      alphaVG = SOILPROP % alphaVG
      mVG     = SOILPROP % mVG    
      nVG     = SOILPROP % nVG    

      TOTP    = SOILPROP % TOTP  
      ORGP    = SOILPROP % ORGP  
      EXCA    = SOILPROP % EXCA  
      EXNA    = SOILPROP % EXNA  
      EXK     = SOILPROP % EXK   

      SOILLAYERTYPE = SOILPROP % SOILLAYERTYPE
      SLDESC        = SOILPROP % SLDESC       
      TAXON         = SOILPROP % TAXON        

!     General profile data:
      MSG(1) = "Soil ID: " // SLNO
      MSG(2) = SLDESC
      MSG(3) = TAXON
      MSG(4) = "  SALB SWCON    CN  DMOD  SLPF SMPX"
      WRITE(MSG(5),'(2F6.2,F6.1,2F6.2,1X,A5)') 
     &      SALB, SWCON, CN,DMOD,SLPF, SMPX
      WRITE(MSG(6),'(A,A)') 
     &      "Soil layer distribution method: ",ISWITCH%MESOL 
      
!     Soil layer data
!     Always print these
      MSG(7) = "     DS DLAYR    LL   DUL   SAT" // 
     &  "  Root    BD    OC  CLAY  SILT  SAND"
      MSG(8) = "LYR  cm    cm  frac  frac  frac" // 
     &  "  Grow g/cm3     %     %     %     %    pH"
      NMSG = 8

      DO L = 1, NLAYR
        WRITE(MSG(NMSG+L),'(I3,I4,I6,4F6.3,2F6.2,4F6.1)') 
     &      L, NINT(DS(L)), NINT(DLAYR(L)), LL(L), DUL(L), SAT(L), 
     &      WR(L), BD(L), OC(L), CLAY(L), SILT(L), SAND(L), PH(L)
      ENDDO

      NMSG = NMSG + NLAYR
      CALL INFO (NMSG, 'SOILDYN', MSG)

!     Print only relevent data here
      HEADER( 1) = '  KSAT'; UNITS( 1) = '  cm/h'; FMT( 1)='(F6.1)'
      HEADER( 2) = '  SLCF'; UNITS( 2) = '     %'; FMT( 2)='(F6.1)'
      HEADER( 3) = '  TOTN'; UNITS( 3) = '     %'; FMT( 3)='(F6.2)'
      HEADER( 4) = '  OrgN'; UNITS( 4) = ' kg/ha'; FMT( 4)='(I6)  '
      HEADER( 6) = '  SASC'; UNITS( 6) = '     %'; FMT( 6)='(F6.2)'
      HEADER( 5) = ' CACO3'; UNITS( 5) = '     %'; FMT( 5)='(F6.1)'
      HEADER( 7) = '  EXTP'; UNITS( 7) = ' mg/kg'; FMT( 7)='(F6.1)'
      HEADER( 8) = '  TOTP'; UNITS( 8) = ' mg/kg'; FMT( 8)='(F6.1)'
      HEADER( 9) = '  ORGP'; UNITS( 9) = ' mg/kg'; FMT( 9)='(F6.0)'
      HEADER(10) = '  EXCA'; UNITS(10) = ' cmo/k'; FMT(10)='(F6.2)'
      HEADER(11) = '   EXK'; UNITS(11) = ' cmo/k'; FMT(11)='(F6.2)'
      HEADER(12) = '  EXNA'; UNITS(12) = ' cmo/k'; FMT(12)='(F6.2)'
      HEADER(13) = '  TBAS'; UNITS(13) = ' cmo/k'; FMT(13)='(F6.1)'

!     If there is at least one non-negative value, then print the column of data
      OK = .FALSE.
      OK(0) = .TRUE.
      DO L = 1, NLAYR
        IF (SWCN(L)   >= 0.0) OK( 1) = .TRUE.; VALUE( 1,L) = SWCN(L)   
        IF (STONES(L) >= 0.0) OK( 2) = .TRUE.; VALUE( 2,L) = STONES(L) 
        IF (TOTN(L)   >= 0.0) OK( 3) = .TRUE.; VALUE( 3,L) = TOTN(L)   
        IF (TotOrgN(L)>= 0.0) OK( 4) = .TRUE.; VALUE( 4,L) = TotOrgN(L)
        IF (SASC(L)   >= 0.0) OK( 6) = .TRUE.; VALUE( 6,L) = SASC(L)   
        IF (CACO3(L)  >= 0.0) OK( 5) = .TRUE.; VALUE( 5,L) = CACO3(L)  
        IF (EXTP(L)   >= 0.0) OK( 7) = .TRUE.; VALUE( 7,L) = EXTP(L)   
        IF (TOTP(L)   >= 0.0) OK( 8) = .TRUE.; VALUE( 8,L) = TOTP(L)   
        IF (ORGP(L)   >= 0.0) OK( 9) = .TRUE.; VALUE( 9,L) = ORGP(L)   
        IF (EXCA(L)   >= 0.0) OK(10) = .TRUE.; VALUE(10,L) = EXCA(L)   
        IF (EXK(L)    >= 0.0) OK(11) = .TRUE.; VALUE(11,L) = EXK(L)    
        IF (EXNA(L)   >= 0.0) OK(12) = .TRUE.; VALUE(12,L) = EXNA(L)   
        IF (TOTBAS(L) >= 0.0) OK(13) = .TRUE.; VALUE(13,L) = TOTBAS(L) 
      ENDDO

!     Can print up to 8 more columns of data
      COUNT = -1
      DO I = 0, MaxParams
        IF (OK(I)) THEN
          COUNT = COUNT + 1
          IF (COUNT > 8) EXIT
          DO L = 1, NLAYR
            IF (COUNT == 0) THEN
!             Always print
              IF (L == 1) THEN
                MSG(1) = "Additional soil properties"
                MSG(2) = "               Convert"
                MSG(3) = "   Cation Adsrp  ppm /"
                MSG(4) = "  L  Exch  Coef  kg/ha"
              ENDIF
              WRITE(MSG(4+L),'(I3,2F6.1,F7.2)') 
     &              L, CEC(L), ADCOEF(L), KG2PPM(L)
            ELSE
!             Print only if data provided
              IF (L == 1) THEN
                MSG(3) = TRIM(MSG(3)) // HEADER(I)
                MSG(4) = TRIM(MSG(4)) // UNITS(I)
              ENDIF
              SELECT CASE(I)
              CASE (4);     WRITE(TEXT,FMT(I)) NINT(VALUE(I,L)) !integer
              CASE DEFAULT; WRITE(TEXT,FMT(I)) VALUE(I,L)       !real
              END SELECT
              MSG(4+L) = TRIM(MSG(4+L)) // TEXT
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      CALL INFO(4+NLAYR,'SOILDYN',MSG)

      RETURN
      END SUBROUTINE PRINT_SOILPROP
!=======================================================================

!=======================================================================
!     SUBROUTINE OPSOILDYN -- output dynamic soil properties
      SUBROUTINE OPSOILDYN(CONTROL, DYNAMIC, ISWITCH, 
     &  BD, BD_SOM, CN, CRAIN, DLAYR, DUL, KECHGE, LL, PRINT_TODAY, SAT,
     &  SOILCOV, SUMKE, SWCN, TOTAW)

      USE ModuleDefs
      IMPLICIT NONE
      EXTERNAL YR_DOY, GETLUN, HEADER
      SAVE

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH

      CHARACTER*11, PARAMETER :: OUTSOL = 'SoilDyn.OUT'
      INTEGER DLUN, DOY, DYNAMIC, YEAR   
      LOGICAL FEXIST, PrintDyn
      LOGICAL Print_today !, TILLED
      REAL CN, CRAIN, SOILCOV, SUMKE, TOTAW
      REAL, DIMENSION(NL) :: BD, BD_SOM, DLAYR, DUL, LL, SAT, SWCN
      REAL, DIMENSION(0:NL) :: KECHGE

!***********************************************************************
!***********************************************************************
!     Seasonal initialization
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      IF (INDEX('AD',ISWITCH % IDETL) > 0 .AND. ISWITCH % IDETW == 'Y' 
     &   .AND.  INDEX('YR',ISWITCH % ISWTIL) > 0) THEN
        PrintDyn = .TRUE. 
        CALL GETLUN('OUTSOL',DLUN)
!       Temporary output file for debugging:
        INQUIRE (FILE = OUTSOL, EXIST = FEXIST)
        IF (FEXIST) THEN      
          !SoilDyn.out file has already been created for this run.
          OPEN (UNIT=DLUN, FILE=OUTSOL, STATUS='OLD', POSITION='APPEND')
        ELSE                  
          CALL GETLUN('OUTSOL', DLUN)
          OPEN (UNIT=DLUN, FILE=OUTSOL, STATUS='NEW')
          WRITE(DLUN,'("*SOIL DYNAMICS OUTPUT FILE")')
        ENDIF

       IF (INDEX('FQ',CONTROL%RNMODE) > 0 .AND. CONTROL%RUN /= 1)RETURN
        CALL HEADER(SEASINIT, DLUN, CONTROL % RUN)
        WRITE(DLUN,"(/,
     &     '@YEAR DOY   DAS   CRAIN  SOLCOV   SUMKE    ROCN   TOTAW',
     &  '  KECHG1  KECHG2  KECHG3  KECHG4',
     &  '  DLAYR1  DLAYR2  DLAYR3  DLAYR4',
     &  '     BD1     BD2     BD3     BD4',
     &  '    BDS1    BDS2    BDS3    BDS4',
     &  '   SWCN1   SWCN2   SWCN3   SWCN4',
     &  '    SAT1    SAT2    SAT3    SAT4',
     &  '    DUL1    DUL2    DUL3    DUL4',
     &  '     LL1     LL2     LL3     LL4')")
      ELSE
        PrintDyn = .FALSE.
      ENDIF

!***********************************************************************
!***********************************************************************
!     Daily output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
!-----------------------------------------------------------------------
      IF (PrintDyn .AND.                        !Print on AND
     &   (MOD(CONTROL%DAS,CONTROL%FROP) .EQ. 0  !(Every FROP days
!    &    .OR. TILLED                           !OR Tilled recently
     &    .OR. Print_today)) THEN    !OR back to normal after tillage)
        CALL YR_DOY(CONTROL % YRDOY, YEAR, DOY) 
        WRITE(DLUN,'(1X,I4,1X,I3.3,1X,I5,
     &    F8.1,2F8.3,F8.1,F8.2,
     &    4F8.3,4F8.2,8F8.3,4F8.3,4F8.2,8F8.5)') 
     &    YEAR, DOY, CONTROL % DAS, CRAIN, SOILCOV, SUMKE, CN, TOTAW, 
     &    KECHGE(1),KECHGE(2),KECHGE(3),KECHGE(4),
     &    DLAYR (1), DLAYR(2), DLAYR(3), DLAYR(4),
     &    BD    (1),    BD(2),    BD(3),    BD(4),
     &    BD_SOM(1),BD_SOM(2),BD_SOM(3),BD_SOM(4),
     &    SWCN  (1),  SWCN(2),  SWCN(3),  SWCN(4),
     &    SAT   (1),   SAT(2),   SAT(3),   SAT(4),
     &    DUL   (1),   DUL(2),   DUL(3),   DUL(4),
     &    LL    (1),    LL(2),    LL(3),    LL(4)
        Print_today =  .FALSE.
      ENDIF

!***********************************************************************
!***********************************************************************
!     End of season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
      CLOSE (DLUN)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************

      RETURN
      END SUBROUTINE OPSOILDYN
!=======================================================================


!C=======================================================================
!C  READ_SOILAUX, Subroutine, C.H. Porter
!C
!C  Soil dynamics routine computes and distributes soil parameters.
!C-----------------------------------------------------------------------
!C  REVISION HISTORY
!C  10/29/2001 CHP  Written
!C  05/15/2003 CHP  Added dynamic soil properties and tillage operations.
!C  08/12/2003 CHP  Added I/O error checking
!C-----------------------------------------------------------------------
!C  Called : Main
!C  Calls  : 
!C=======================================================================
!
!      SUBROUTINE IPSAUX(CONTROL, RGIMPF) 
!
!C-----------------------------------------------------------------------
!      USE ModuleDefs     !Definitions of constructed variable types, 
!                         ! which contain control information, soil
!                         ! parameters, hourly weather data.
!      IMPLICIT NONE
!
!      REAL RGIMPF(NL)
!
!      CHARACTER*1   BLANK
!      CHARACTER*6   SECTION
!      CHARACTER*6,  PARAMETER :: ERRKEY = 'IPSAUX'
!      CHARACTER*10  SLNO
!      CHARACTER*11, PARAMETER :: FILESX = 'Hardpan.sol'
!      CHARACTER*30  FILEIO
!      CHARACTER*80  PATHSL
!      CHARACTER*92  FILESS
!      CHARACTER*255 C255
!      INTEGER ERRNUM, FOUND, LN, LNUM, LUNIO, PATHL
!      INTEGER SDEP(NL)
!      INTEGER LUNSL, LINSOL, ISECT
!      PARAMETER (BLANK = ' ')
!      LOGICAL FEXIST
!
!      TYPE (ControlType) CONTROL
!      FILEIO = CONTROL % FILEIO
!      LUNIO  = CONTROL % LUNIO
!
!C-----------------------------------------------------------------------
!C         Read parameters from input file
!C-----------------------------------------------------------------------
!      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
!      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)
!      REWIND (LUNIO)
!!     Read path to soils directory
!      READ (LUNIO,'(10/,28X,A80)', IOSTAT=ERRNUM) PATHSL
!
!C-----------------------------------------------------------------------
!C    Read Field Section
!C-----------------------------------------------------------------------
!      SECTION = '*FIELD'
!      CALL FIND(LUNIO, SECTION, LNUM, FOUND)
!      IF (FOUND .EQ. 0) THEN
!        CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
!      ELSE
!        READ(LUNIO,'(69X,A10)', IOSTAT=ERRNUM) SLNO
!      ENDIF
!
!      CLOSE (LUNIO)
!
!!-----------------------------------------------------------------------
!C      Set default values for all hard pan factors
!C      Note, by default, we set RGIMPF of layer 4 to 0.3 for any soil.
!C      This can be overridden if the root growth impedance factor is actually
!C      defined in the SOIL.AUX file. If the user does not create an entry
!C      for their soil in SOIL.AUX, the default values will be used.
!
!C     -----------------------------------------
!C     Set filename and path
!C     -----------------------------------------
!      PATHL  = INDEX(PATHSL,BLANK)
!      IF (PATHL .LE. 1) THEN
!        FILESS = FILESX
!      ELSE
!        FILESS = TRIM(PATHSL) // TRIM(FILESX)
!      ENDIF
!
!      INQUIRE (FILE = FILESS, EXIST = FEXIST)
!      IF (FEXIST) THEN
!        CALL GETLUN('FINPUT', LUNSL)
!C       -----------------------------------------
!C       Open soil.aux file
!C       -----------------------------------------
!        OPEN (LUNSL, FILE = FILESS,STATUS = 'OLD')
!
!C       Loop to search for soil id matching soiltype from soil.sol
!        FileLoop: DO WHILE (.TRUE.)
!          CALL IGNORE(LUNSL, LINSOL, ISECT, C255)
!          IF (ISECT .EQ. 0 ) EXIT FileLoop
!          IF (C255(1:1) .NE. '*') CYCLE FileLoop
!          IF (C255(2:11) .EQ. SLNO) THEN
!c           match found, read in hard pan properties
!            LN = 0
!C           read first line of data, skip blanks/comments
!            LayerLoop: DO WHILE (.TRUE.)
!              CALL IGNORE(LUNSL,LINSOL,ISECT,C255)
!              IF(ISECT .EQ. 0 .OR. C255(1:1) .EQ. '*') EXIT FileLoop
!              LN = LN + 1
!              READ(C255,'(I6,1X,F5.2)',IOSTAT=ERRNUM)SDEP(LN),RGIMPF(LN)
!              IF (ERRNUM .NE. 0) THEN
!                CALL SAUX_DEFAULT(RGIMPF, NL)   
!                EXIT FileLoop
!              ENDIF
!            ENDDO LayerLoop
!          ENDIF
!        ENDDO FileLoop
!
!        CLOSE(LUNSL)
!      ELSE
!        IF (CONTROL % REPNO == 1) CALL SAUX_DEFAULT(RGIMPF, NL)
!      ENDIF
!
!      RETURN
!      END SUBROUTINE IPSAUX
!C=======================================================================
!
!
!C=======================================================================
!      SUBROUTINE SAUX_DEFAULT(RGIMPF, NL)
!      !Set default values for RGIMPF if auxiliary soil file not found
!      !  or has errors.
!
!      IMPLICIT NONE
!
!      CHARACTER*6, PARAMETER :: ERRKEY = 'IPSAUX'
!      CHARACTER*78  MSG(5)
!      INTEGER NL
!      REAL RGIMPF(NL)
!      
!      RGIMPF = 1.0        !Set array to 1.0
!      RGIMPF(4) = 0.30    !Set layer 4 value to 0.30
!
!      MSG(1)='Auxiliary soil file for hardpan layer does not exist or'
!      MSG(2)='  I/O error was detected. Default values will be used for'
!      MSG(3)='  root growth impedance factor:'
!      MSG(4)='  RGIMPF = 0.3 for soil layer 4'
!      MSG(5)='  RGIMPF = 1.0 for all other soil layers'
!!     Don't need message yet, because this version does not use RGIMPF
!!      CALL INFO(5, ERRKEY, MSG)
!
!      RETURN
!      END SUBROUTINE SAUX_DEFAULT
!C=======================================================================
!      
!!-----------------------------------------------------------------------
!!  DEFINITIONS
!!  FILESX - Name of SOIL.AUX file
!!  FILESS - Path plus FILESX string
!!  LUNSL - File number used to open FILESX
!!  PATHSL- path pointing to SOIL.AUX (passed in from CERES.for)
!!  PATHL - length of directory path of SOIL.AUX file
!!  SLNO - 10 character string containing soil id name
!!  LN - line number for soil depth when reading hard pan factor
!!  RGIMPF(L) - Root growth impedance factor, subscripted for each soil layer
!!  SDEP(L) - soil depth associated with each hard pan factor
!!=======================================================================

!=======================================================================
!   SoilLayerText, Subroutine
!-----------------------------------------------------------------------
!     Labels for soil layer depth info
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  11/17/2008  
!-----------------------------------------------------------------------
!  Called by: SoilDYN, CellInit_2D
!  Calls    : 
!=======================================================================
      SUBROUTINE SoilLayerText(DS, NLAYR,        !Input
     &    LayerText)                             !Output 
 
      USE ModuleDefs 
      INTEGER NLAYR, L
      REAL, DIMENSION(NL) :: DS
      CHARACTER*8 LayerText(11)
      INTEGER, DIMENSION(NL) :: ZB, ZT
      CHARACTER*14 FMT  
        
      !Establish soil layer depths for headers
      !Text describing soil layer depth data
      !1-9 describe depths for layers 1-9
      !10 depths for layers 10 thru NLAYR (if NLAYR > 9)
      !11 depths for layers 5 thru NLAYR (if NLAYR > 4)
      LayerText = '        '
      ZT = -99
      ZB = -99
 
      DO L = 1, 11
        SELECT CASE(L)
!       For layers 1-10, write depths to label, "LayerText"
        CASE (1)
          ZT(1) = 0
          ZB(1) = NINT(DS(1))
        CASE (2:9)
          IF (L <= NLAYR) THEN
            ZT(L) = ZB(L-1)
            ZB(L) = NINT(DS(L))
          ENDIF
        CASE(10)
!         Label 10 includes layers 10 thru NLAYR
          IF (NLAYR > 9) THEN
            ZT(L) = ZB(L-1)
            ZB(L) = DS(NLAYR)
          ENDIF
        CASE(11)
          IF (NLAYR > 4) THEN
!           Label 11 includes layers 5 thru NLAYR
            ZT(11) = ZB(4)
            ZB(11) = DS(NLAYR)
          ENDIF
        END SELECT
 
!       Format dependant on # digits
        IF (ZB(L) > 0) THEN
          SELECT CASE (ZT(L))
          CASE (:9)
            SELECT CASE (ZB(L))
            CASE(:9);    FMT = '(5X,I1,"-",I1)'
            CASE(10:99); FMT = '(4X,I1,"-",I2)'
            CASE(100:);  FMT = '(3X,I1,"-",I3)'
            END SELECT
          CASE (10:99)
            SELECT CASE (ZB(L))
            CASE(10:99); FMT = '(3X,I2,"-",I2)'
            CASE(100:);  FMT = '(2X,I2,"-",I3)'
            END SELECT
          CASE (100:);   FMT = '(1X,I3,"-",I3)'
          END SELECT
          WRITE(LayerText(L),FMT) ZT(L), ZB(L)
        ENDIF
      ENDDO

      RETURN      
      END SUBROUTINE SoilLayerText
C=======================================================================

!=======================================================================
!   SoilLayerClass, Subroutine
!-----------------------------------------------------------------------
!     Define the type of soil layer. 
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  08/17/2011  
!-----------------------------------------------------------------------
!  Called by: SoilDYN, CellInit_2D when (DYNAMIC = RUNINIT) and 
!=======================================================================
      SUBROUTINE SoilLayerClass(ISWITCH, 
     &    MULTI, DS, NLAYR, SLDESC, TAXON,                !Input
     &    CaCO3, PH, CEC, CLAY, SOILLAYERTYPE)            !Output 
 
      USE ModuleDefs
      IMPLICIT NONE
      EXTERNAL UPCASE, WARNING, LENSTRING, INFO

      TYPE (SwitchType) , INTENT(IN) :: ISWITCH  !Simulation options  
      INTEGER NLAYR, L, LENGTH, LEN1, LEN2, MULTI, LenString, I
      REAL, DIMENSION(NL) :: DS, CaCO3, PH, CEC, CLAY
      CHARACTER*1  UPCASE
      CHARACTER*17 SOILLAYERTYPE(NL)
      CHARACTER*50 SLDESC, TAXON 
      CHARACTER*7, PARAMETER :: ERRKEY = 'SOILLayerClass'  
      CHARACTER*78 MSG(NL+4)
      LOGICAL VOLCANIC
        
!     Define the type of soil layer.
!     First check soil name for occurrence of 'ANDOSOL', 'ANDISOL', 
!     'VOLCAN' or 'ANDEPT'.  These indicate volcanic soils.
      VOLCANIC = .FALSE.
      LENGTH = MAX0(LEN(TRIM(SLDESC)), LEN(TRIM(TAXON)))
      DO I = 1, LENGTH
        SLDESC(I:I) = UPCASE(SLDESC(I:I))
        TAXON(I:I)  = UPCASE(TAXON(I:I))
      ENDDO 

      IF (INDEX(SLDESC // TAXON,'ANDOSOL') > 0 .OR.
     &    INDEX(SLDESC // TAXON,'ANDISOL') > 0 .OR.
     &    INDEX(SLDESC // TAXON,'VOLCAN' ) > 0 .OR.
     &    INDEX(SLDESC // TAXON,'ANDEPT' ) > 0) THEN
        VOLCANIC = .TRUE.
      ENDIF

!     Calcareous soil is with >15% (w:w) of CaCO3. 
!     Highly weathered is CEC:CLAY < 16 cmol/kg (meq/100g) 
!     Slightly weathered is CEC:CLAY >16 mmol/kg
      DO L = 1, NLAYR
!       First check for calcareous soil
        IF (CaCO3(L) > 0.15) THEN
          SOILLAYERTYPE(L) = 'CALCAREOUS       '
          CYCLE
        ELSEIF (CaCO3(L) < 0.) THEN
          IF (pH(L) > 7 .AND. ISWITCH%ISWPHO /= 'N' .AND. MULTI < 2)THEN
            WRITE(MSG(1),'("CaCO3 value missing for soil layer. ",I2)')L
            MSG(2) = "Soil classification may be in error."
            WRITE(MSG(2),'("pH =",F8.1,
     &        " Possible calcareous soil type.")') pH(L)
            CALL WARNING(2,ERRKEY,MSG)
          ENDIF
        ENDIF

!       Next check for andisol
        IF (VOLCANIC) THEN
          SOILLAYERTYPE(L) = 'ANDISOL          '
          CYCLE
        ENDIF

!       Last classify by slightly or higly weathered (or unknown)
        IF (CEC(L) > 0.0 .AND. CLAY(L) > 0.0) THEN
          IF (CEC(L)/(CLAY(L)/100.) > 16.) THEN
            SOILLAYERTYPE(L) = 'SLIGHTLYWEATHERED'
          ELSE
            SOILLAYERTYPE(L) = 'HIGHLYWEATHERED  '
          ENDIF
          CYCLE
        ELSE
          SOILLAYERTYPE(L) = 'UNKNOWN          '
          IF (ISWITCH%ISWPHO .NE. 'N' .AND. MULTI < 2) THEN
            WRITE(MSG(1),'("Insufficient data available to classify",
     &      " soil layer ",I2)') L
            WRITE(MSG(2),'("Default characteristics will be used.")')
            WRITE(MSG(3),'("  CaCO3 =",F8.2,"%")') CaCO3(L)
            WRITE(MSG(4),'("  CEC   =",F8.2," cmol/kg")') CEC(L)
            WRITE(MSG(5),'("  pH    =",F8.1)') pH(L)
            WRITE(MSG(6),'("  Clay  =",F8.1,"%")') CLAY(L)
            WRITE(MSG(7),
     &        '("This may affect results of soil phosphorus model.")')
            CALL INFO(7,ERRKEY,MSG)
          ENDIF
        ENDIF
      ENDDO
! CHP the following statement should be in here or SOILDYN??
      IF (ISWITCH%ISWPHO == 'Y') THEN
        MSG(1) = "Soil layer classifications (used for soil P model)"
        MSG(2) = "Layer Depth Soil_Layer_Type   Backup_Data"
        DO L = 1, NLAYR
          SELECT CASE(SOILLAYERTYPE(L))
          CASE('CALCAREOUS       ')
            WRITE(MSG(L+2),'(I3,I7,2X,A17," CaCO3:",F6.2,"%")')  
     &        L, NINT(DS(L)),SOILLAYERTYPE(L), CaCO3(L)
          CASE('ANDISOL          ')
            LEN1 = MIN(39,LENSTRING(SLDESC))
            LEN2 = MIN(47-LEN1,LENSTRING(TAXON))
            WRITE(MSG(L+2),'(I3,I7,2X,A17,1X,A,",",A)')  
     &        L, NINT(DS(L)),SOILLAYERTYPE(L), SLDESC(1:LEN1), 
     &        TAXON(1:LEN2)
          CASE DEFAULT
            WRITE(MSG(L+2),'(I3,I7,2X,A17," CaCO3:",F6.2,"%; CEC:",F6.1,
     &        " cmol/kg; CLAY:",F6.1,"%")')  
     &        L, NINT(DS(L)),SOILLAYERTYPE(L), CaCO3(L), CEC(L), CLAY(L)
          END SELECT
        ENDDO
        CALL INFO(NLAYR+2,ERRKEY,MSG)
      ENDIF


      RETURN      
      END SUBROUTINE SoilLayerClass
C=======================================================================

!==============================================================================
!     Subroutine SETPM
!     Calculate the fraction of plastic mulch cover 
      SUBROUTINE SETPM(SOILPROP)                 !input/output
!   ---------------------------------------------------------
      USE ModuleData
      Implicit NONE
      EXTERNAL ERROR, FIND, WARNING, GETLUN, INFO

      Type (SoilType) SOILPROP

      CHARACTER*6 SECTION
      CHARACTER*8, PARAMETER :: ERRKEY = 'SETPM'
      CHARACTER*125 MSG(50)
!     CHARACTER*180 CHAR
      INTEGER ERR, FOUND, LNUM, LUNIO
      REAL PMWD, ROWSPC_CM
      REAL PMALB, PMFRACTION, MSALB
      LOGICAL PMCover
    
      TYPE (ControlType) CONTROL
      CALL GET(CONTROL)

!   ---------------------------------------------------------
!     Get bed dimensions and row spacing
      CALL GETLUN('FILEIO', LUNIO)
      OPEN (LUNIO, FILE = CONTROL%FILEIO,STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,CONTROL%FILEIO,0)
      LNUM = 0

!-----------------------------------------------------------------------
      PMALB = -99.

!     Read plastic mulch albedo from FIELDS section
      SECTION = '*FIELD'
      CALL FIND(LUNIO, SECTION, LNUM, FOUND)
      IF (FOUND /= 0)  THEN
!     For 1D model, plastic mulch width is read from "bed width" variable in FileX
      READ(LUNIO,'(79X,F6.0,F6.0)',IOSTAT=ERR) PMALB, PMWD
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,CONTROL%FILEIO,LNUM)
        IF ((ERR == 0) .AND. (PMALB .eq. 0.)) THEN
          PMALB = -99.
        ENDIF
      ENDIF

!     Read Planting Details Section
      SECTION = '*PLANT'
      CALL FIND(LUNIO, SECTION, LNUM, FOUND) 
      IF (FOUND == 0) CALL ERROR(SECTION, 42, CONTROL%FILEIO, LNUM)
      READ(LUNIO,'(42X,F6.0,42X,2F6.0)',IOSTAT=ERR) ROWSPC_CM 
      LNUM = LNUM + 1
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,CONTROL%FILEIO,LNUM)

      CLOSE(LUNIO)
 
      IF (PMALB .GT. 0) THEN
        IF (PMWD .GT. 0 .AND. PMWD .GE. ROWSPC_CM) THEN
          PMWD = ROWSPC_CM
          PMCover   = .TRUE.
          WRITE(MSG(1),'("Plastic mulch width (cm) = ",F6.1)') PMWD
          WRITE(MSG(2),'("Row spacing (cm)         = ",F6.1)') ROWSPC_CM
          MSG(3) = "Simulating flat surface entirely covered " //
     &             "by by plastic mulch."
          call INFO(3,errkey,msg)
        ELSEIF (PMWD .GT. 0.) THEN 
          PMCover   = .TRUE.
          WRITE(MSG(1),'("Plastic mulch width (cm) = ",F6.1)') PMWD
          WRITE(MSG(2),'("Row spacing (cm)         = ",F6.1)') ROWSPC_CM
          MSG(3)= "Simulating flat surface partially covered " // 
     &            "by plastic mulch."
          call INFO(3,errkey,msg)
        ELSE
          PMCover   = .FALSE.
          MSG(1)= "Missing mulch cover width."
          MSG(2) = "Simulating flat surface with no plastic mulch."
          call INFO(2,errkey,msg)
        ENDIF
      ELSE
        IF (PMWD .GT. 0) THEN
          PMCover   = .FALSE.
          MSG(1)= "Missing albedo for plastic mulch. "
          MSG(2)= "Simulating flat surface with no plastic mulch."
          call INFO(2,errkey,msg)
        ELSE
          PMCover   = .FALSE.
          MSG(1)= "Simulating flat surface with no plastic mulch."
          call INFO(1,errkey,msg)
        ENDIF
      ENDIF

!     Default = no plastic mulch cover
      PMFRACTION = 0.0  !no cover

      IF (PMCover) THEN
        if (PMWD .GE. ROWSPC_CM) THEN
          SOILPROP % SALB   = PMALB
        ENDIF
        PMFRACTION = PMWD / ROWSPC_CM
        MSALB = PMALB * PMFRACTION + SOILPROP % SALB * (1.0 -PMFRACTION)
        SOILPROP % MSALB  = MSALB
        SOILPROP % CMSALB = MSALB
      ENDIF

      CALL PUT("PM", "PMFRACTION", PMFRACTION)

      RETURN      
      END SUBROUTINE SETPM