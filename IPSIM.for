C=======================================================================
C  IPSIM, Subroutine
C
C  Reads parameters related to field operation from FILEX file
C-----------------------------------------------------------------------
C  Revision history
C  01/01/1990 JWJ Written
C  05/28/1993 PWW Header revision and minor changes            
C  11/19/2003 CHP Added check for MEPHO and incompatible models.
C  02/21/2006 GH  Removed crop model selection
!  10/25/2006 CHP CRMODEL from FILEX overrides MODEL in DSSATPRO 
!  05/09/2007 CHP Make Sulieman-Ritchie the default soil evaporation method
!  04/28/2008 CHP Added switch for CO2 from file (ICO2)
!  12/09/2009 CHP IPSIM separate file.  
!  02/11/2010 CHP Added checks for P model linked with crop models.

C-----------------------------------------------------------------------
C  INPUT  : LUNEXP,FILEX,LNSIM
C
C  LOCAL  : LN
C
C  OUTPUT : NYRS,NREPSQ,ISWWAT,ISWNIT,ISWSYM,ISWPHO,ISWPOT,ISWDIS,MEWTH,
C           MESIC,MELI,MEEVP,MEINF,MEPHO,ISIMI,ISIM,IPLTI,IIRRI,IFERI,
C           IRESI,IHARI,IOX,IDETO,IDETS,IDETG,IDETC,IDETW,IDETN,IDETP,IDETD,
C           PWDINF,PWDINL,SWPLTL,SWPLTH,SWPLTD,PTX,PTTN,DSOILX,THETACX,
C           IEPTX,IOFFX,IAMEX,DSOILN,SOILNC,SOILNX,NEND,RIP,NRESDL,
C           DRESMG,HDLAY,HLATE
!           MESOM, METMP, MESOL, MESEV
C-----------------------------------------------------------------------
C  Called : IPEXP
C
C  Calls  : ERROR IGNORE FIND YR_DOY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IPSIM (LUNEXP,LNSIM,TITSIM,NYRS,RUN,NREPSQ,
     & ISIMI,PWDINF,PWDINL,SWPLTL,NCODE,SWPLTH,SWPLTD,YEAR,
     & PTX,PTTN,DSOIL,THETAC,IEPT,IOFF,IAME,DSOILN,SOILNC,YRSIM,
     & SOILNX,NEND,RIP,NRESDL,DRESMG,HDLAY,HLATE,HPP,HRP,FTYPEN,
     & RSEED1,LINEXP,AIRAMT,EFFIRR,CROP,FROP,MODEL,RNMODE,FILEX,
     & CONTROL, ISWITCH, UseSimCtr, FILECTL, MODELARG, YRPLT)

      USE ModuleDefs
      USE ModuleData
      IMPLICIT NONE
      SAVE

      INCLUDE 'COMSWI.BLK'

      CHARACTER*1   UPCASE,ISIMI, RNMODE
      CHARACTER*2   CROP
      CHARACTER*5   NEND,NCODE,IOFF,IAME
      CHARACTER*6   ERRKEY,FINDCH
      CHARACTER*8   MODEL, MODELARG, CRMODEL, TRY_MODEL
      CHARACTER*12  FILEX
      CHARACTER*16  CROPD
      CHARACTER*25  TITSIM
      CHARACTER*78  MSG(7)
      CHARACTER*120 FILECTL
      CHARACTER*128 CHARTEST

      INTEGER LNSIM,LUNEXP,ISECT,LINEXP,ISIM,NYRS,NREPSQ,FROP
      INTEGER PLDATE,PWDINF,PWDINL,HLATE,HDLAY,NRESDL
      INTEGER IFIND,LN,ERRNUM,FTYPEN,YRSIM,YEAR,RUN,RSEED1,RRSEED1
      INTEGER YRPLT

      REAL DSOIL,THETAC,DSOILN,SOILNC,SOILNX,SWPLTL,SWPLTH,SWPLTD
      REAL PTX,PTTN,DRESMG,RIP,IEPT,HPP,HRP,AIRAMT,EFFIRR

      LOGICAL UseSimCtr, MulchWarn

      TYPE (SwitchType)  ISWITCH
      TYPE (ControlType) CONTROL

      PARAMETER (ERRKEY='IPSIM ')
                 FINDCH='*SIMUL'
                 
      DATA MulchWarn /.FALSE./

      IF (LNSIM .EQ. 0) THEN
         LNSIM   = 0
         NYRS    = 1
         NREPSQ  = 1
         ISIMI   = 'S'
         YRSIM   = -99
         RSEED1  = 2150
         ISWWAT  = 'Y'
         ISWNIT  = 'Y'
         ISWSYM  = 'Y'
         ISWPHO  = 'N'
         ISWPOT  = 'N'
         ISWDIS  = 'N'
         ISWCHE  = 'N'
         ISWTIL  = 'Y'

         IF (INDEX('FNQS',RNMODE) > 0) THEN
           ICO2 = 'D' !Default CO2 from CO2???.WDA file
         ELSE
           ICO2 = 'M' !Measured CO2 from CO2???.WDA file
         ENDIF

         MEWTH   = 'M'
         MESIC   = 'M'
         MELI    = 'E'
         MEEVP   = 'R'
         MEINF   = 'S'
         MEPHO   = 'L'
         MEHYD   = 'R'
         NSWITCH =  1
         MESOM   = 'G'
         MESOL   = '2'    !was '1'
         MESEV   = 'S'    !new Sulieman-Ritchie (2006)
         METMP   = 'D'    !DSSAT original soil temperature
!        METMP   = 'E' => EPIC soil temp routine.
         IPLTI   = 'R'
         IIRRI   = 'R'
         IFERI   = 'R'
         IRESI   = 'R'
         IHARI   = 'M'
         IOX     = 'N'
         FROP    =  3
         IDETO   = 'Y'
         IDETS   = 'Y'
         IDETG   = 'Y'
         IDETN   = 'N'
         IDETC   = 'N'
         IDETW   = 'N'
         IDETP   = 'N'
         IDETD   = 'N'
         IDETL   = 'N'
         IDETH   = 'N'
         IDETR   = 'Y'
         EFFIRR  = 1.00
         THETAC  = 75.0
         IEPT    = 100.0
         DSOIL   = 30.0
         DSOILN  = 30.0
         AIRAMT  = 10.0
         IOFF    = 'GS000'
         IAME    = 'IR001'
         CRMODEL = '        '
         NCODE = "-99  "
         NEND  = "-99  "
       ELSE
 40      CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
         IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)
 50      CALL IGNORE(LUNEXP,LINEXP,ISECT,CHARTEST)
         IF (ISECT .EQ. 1) THEN
            READ (CHARTEST,55,IOSTAT=ERRNUM) LN
            IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEX,LINEXP)
            IF (LN .NE. LNSIM) GO TO 50
            READ (CHARTEST,55,IOSTAT=ERRNUM) LN,NYRS,NREPSQ,ISIMI,
     &            YRSIM,RRSEED1,TITSIM,CRMODEL
            IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEX,LINEXP)
            IF (INDEX('G',RNMODE) .GT. 0) NYRS = 1
            IF ((RNMODE .NE. 'Q') .OR. (RNMODE .EQ. 'Q'
     &       .AND. RUN .EQ. 1)) THEN
               RSEED1 = RRSEED1
               IF (RSEED1 .LE. 0) THEN
                 RSEED1 = 2150
               ENDIF
            ENDIF
            CALL Y2K_DOY (YRSIM)
            CALL YR_DOY (YRSIM,YEAR,ISIM)
          ELSE
            BACKSPACE (LUNEXP)
            GO TO 40
         ENDIF
C
C        Read SECOND line of simulation control
C
         CALL IGNORE(LUNEXP,LINEXP,ISECT,CHARTEST)
         READ (CHARTEST,60,IOSTAT=ERRNUM) LN,ISWWAT,ISWNIT,ISWSYM,
     &        ISWPHO,ISWPOT,ISWDIS,ISWCHE,ISWTIL, ICO2
         IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEX,LINEXP)

         ISWWAT = UPCASE(ISWWAT)
         ISWNIT = UPCASE(ISWNIT)
         ISWSYM = UPCASE(ISWSYM)
         ISWPHO = UPCASE(ISWPHO)
         ISWPOT = UPCASE(ISWPOT)
         ISWDIS = UPCASE(ISWDIS)
         ISWCHE = UPCASE(ISWCHE)
         ISWTIL = UPCASE(ISWTIL)
         ICO2   = UPCASE(ICO2)

!        IF (INDEX ('BNSBPNPECHPPVBCPCBFB',CROP) .EQ. 0) THEN
         SELECT CASE (CROP)
         CASE ('BN','SB','PN','PE','CH','PP',
     &          'VB','CP','CB','FB','GB','LT')
!          Do nothing -- these crops fix N and can have Y or N
         CASE DEFAULT; ISWSYM = 'N'  !other crops don't have a choice
         END SELECT
!        ENDIF
         IF (ISWCHE .EQ. ' ') THEN
            ISWCHE = 'N'
         ENDIF
         IF (ISWTIL .EQ. ' ') THEN
            ISWTIL = 'N'
         ENDIF
         IF (ISWWAT .EQ. 'N') THEN
            ISWNIT = 'N'
            ISWPHO = 'N'
            ISWCHE = 'N'
         ENDIF

         IF (INDEX('FNQS',RNMODE) > 0) THEN
!          For sequence, seasonal runs, default CO2 uses static value
           IF (INDEX ('WMD', ICO2) < 1) ICO2 = 'D'
         ELSE
!          For experimental runs, default CO2 uses measured values
           IF (INDEX ('WMD', ICO2) < 1) ICO2 = 'M'
         ENDIF

C        Read THIRD line of simulation control
C
         CALL IGNORE(LUNEXP,LINEXP,ISECT,CHARTEST)
         READ (CHARTEST,61,IOSTAT=ERRNUM) LN,MEWTH,MESIC,
     &        MELI,MEEVP,MEINF,MEPHO,MEHYD,NSWITCH, 
     &        MESOM, MESEV, MESOL, METMP
         !IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEX,LINEXP)
         MEWTH = UPCASE(MEWTH)
         MESIC = UPCASE(MESIC)
         MELI  = UPCASE(MELI)
         MEEVP = UPCASE(MEEVP)
         MEINF = UPCASE(MEINF)
         MEPHO = UPCASE(MEPHO)
         MESOM = UPCASE(MESOM)
         MEHYD = UPCASE(MEHYD)
         MESEV = UPCASE(MESEV)
         METMP = UPCASE(METMP)

         IF (INDEX('PG',MESOM) .EQ. 0) THEN
            MESOM = 'G'
         ENDIF
         
         IF (INDEX('G',MESOM)   > 0 .AND. 
     &       INDEX('FQ',RNMODE) > 0 .AND. 
     &       INDEX('N',MEINF)  == 0) THEN
           MEINF = 'N'
           IF (.NOT. MulchWarn) THEN
             MSG(1)=
     &  "Long-term simulation of surface residues may not be accurate"
             MSG(2)=
     &  "when using Godwin soil organic matter module.  The effects of"
             MSG(3)=
     &  "a surface mulch layer on runoff and evaporation will " //
     &       "not be modeled."  
             MSG(4)=
     &  "Simulation Options/Methods/Infiltration = 'No mulch effects'"
             MSG(5)=
     &  "You may want to consider using the Parton (CENTURY) method of"
             MSG(6)= "modeling soil organic matter."
             CALL WARNING(6,ERRKEY,MSG)
             MulchWarn = .TRUE.
           ENDIF
         ENDIF

         IF (INDEX('123',MESOL) < 1) THEN
            MESOL = '2'
         ENDIF

!        Default soil temperature method is DSSAT
         IF (INDEX('ED',METMP) < 1) METMP = 'D'

         SELECT CASE(MESEV)
         CASE('R','r'); MESEV = 'R'
         CASE DEFAULT;  MESEV = 'S'   !Default method -- use NEW
         END SELECT

         IF (MEEVP == 'Z' .AND. MEPHO /= 'L') CALL ERROR(ERRKEY,3,' ',0)

         IF (MEHYD .EQ. ' ') THEN
            MEHYD = 'R'
         ENDIF

         IF (NSWITCH .LE. 0 .AND. ISWNIT .EQ. 'Y') THEN
           NSWITCH = 1
         ENDIF
C
C        Read FOURTH line of simulation control
C
         CALL IGNORE(LUNEXP,LINEXP,ISECT,CHARTEST)
         READ (CHARTEST,60,IOSTAT=ERRNUM) LN,IPLTI,IIRRI,
     &        IFERI,IRESI,IHARI
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
         IPLTI = UPCASE(IPLTI)
         IIRRI = UPCASE(IIRRI)
         IFERI = UPCASE(IFERI)
         IRESI = UPCASE(IRESI)
         IHARI = UPCASE(IHARI)

         IF ((INDEX('CSPT',CROP)) .GT. 0) THEN
           IF (IHARI .EQ. 'A') THEN
              CALL ERROR (ERRKEY,4,FILEX,LINEXP)
           ENDIF
         ENDIF

         IF ((INDEX('PT',CROP)) .GT. 0) THEN
           IF (IPLTI .EQ. 'A') THEN
              CALL ERROR (ERRKEY,5,FILEX,LINEXP)
           ENDIF
         ENDIF

C
C        Read FIFTH line of simulation control
C
         CALL IGNORE(LUNEXP,LINEXP,ISECT,CHARTEST)
         IF (INDEX('FQ',RNMODE) < 1 .OR. RUN == 1) THEN
            READ (CHARTEST,65,IOSTAT=ERRNUM) LN,IOX,IDETO,
     &      IDETS,FROP,IDETG,IDETC,IDETW,IDETN,IDETP,IDETD,
     &      IDETL,IDETH,IDETR
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            IOX   = UPCASE(IOX)
            IDETO = UPCASE(IDETO)
            IDETS = UPCASE(IDETS)
            IDETG = UPCASE(IDETG)
            IDETC = UPCASE(IDETC)
            IDETW = UPCASE(IDETW)
            IDETN = UPCASE(IDETN)
            IDETP = UPCASE(IDETP)
            IDETD = UPCASE(IDETD)
            IF (IDETL .EQ. ' ') THEN
               IDETL = 'N'
            ENDIF
            IDETL = UPCASE(IDETL)
            IF (IDETH .EQ. ' ') THEN
               IDETH = 'N'
            ENDIF
            IDETH = UPCASE(IDETH)
            IF (IDETR .EQ. ' ') THEN
               IDETR = 'Y'
            ENDIF
            IDETR = UPCASE(IDETR)

!           Verbose output switch
            IF (IDETL == '0') THEN
!             VBOSE = zero, suppress all output except Summary and Evaluate
              IDETS = 'Y'
              IDETG = 'N' 
              IDETC = 'N' 
              IDETW = 'N' 
              IDETN = 'N' 
              IDETP = 'N' 
              IDETD = 'N' 
              IDETH = 'N' 
              IDETR = 'N' 
              IDETO = 'E'
!             Seasonal and spatial runs do not get evaluate file when IDETL=0
              IF (INDEX('SN',RNMODE) > 0) IDETO = 'N'
            ELSEIF (IDETL == 'A') THEN
!             VBOSE = 'A', generate all output
              IDETS = 'A'
              IDETO = 'Y'
              IDETG = 'Y' 
              IDETC = 'Y' 
              IDETW = 'Y' 
              IDETN = 'Y' 
              IDETP = 'Y' 
              IDETD = 'Y' 
              IDETH = 'Y' 
              IDETR = 'Y' 
!             Set IDETL back to "D" so no need for changes elsewhere
!             IDETL = 'D' 
              FROP  = 1
            ENDIF

            IF (FROP .LE. 0) FROP = 10
         ENDIF
C
C        Read SIXTH line of simulation control
C
         CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
         IF (ISECT .EQ. 1) THEN
            READ (CHARTEST,66,IOSTAT=ERRNUM) LN,PWDINF,PWDINL,
     &           SWPLTL,SWPLTH,SWPLTD,PTX,PTTN
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            IF (PWDINF .LT. 1000) PWDINF = YEAR * 1000 + PWDINF
            IF (PWDINL .LT. 1000) PWDINL = YEAR * 1000 + PWDINL
            CALL Y2K_DOY (PWDINF)
            CALL Y2K_DOY (PWDINL)
C
C           Read SEVENTH line of simulation control
C
            CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
            READ (CHARTEST,67,IOSTAT=ERRNUM) LN,DSOIL,THETAC,
     &           IEPT,IOFF,IAME,AIRAMT,EFFIRR
            IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEX,LINEXP)
C
C           Read EIGHTH line of simulation control
C
            CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
            READ (CHARTEST,67,IOSTAT=ERRNUM) LN,DSOILN,SOILNC,
     &           SOILNX,NCODE,NEND
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            READ (NCODE,70,IOSTAT=ERRNUM) FTYPEN
C
C           Read NINTH line of simulation control
C
            CALL IGNORE(LUNEXP,LINEXP,ISECT,CHARTEST)
            READ (CHARTEST,68,IOSTAT=ERRNUM) LN,RIP,NRESDL,DRESMG
            IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEX,LINEXP)
C
C           Read TENTH line of simulation control
C
            CALL IGNORE(LUNEXP,LINEXP,ISECT,CHARTEST)
            READ (CHARTEST,66,IOSTAT=ERRNUM) LN,HDLAY,HLATE,
     &           HPP,HRP
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            CALL Y2K_DOY (HLATE)
            IF (HPP   .LT. 0.0)  HPP   = 100.
            IF (HRP   .LT. 0.0)  HRP   = 0.0
          ELSE
            PWDINF  =   1
            PWDINL  =   366
            SWPLTL  =   1.0
            SWPLTH  =   100.0
            SWPLTD  =   200.0
            PTX     =   50.0
            PTTN    =   1.0
            DSOIL   =   200.0
            THETAC  =   10.0
            IEPT    =   100.0
            IOFF    =   ' '
            HPP     =   100.0
            HRP     =     0.0
         ENDIF
      ENDIF

      REWIND (LUNEXP)

C-----------------------------------------------------------------------
C    Select Model Name and Path -- order of priority:
!     CTRMODEL is value from control file override -- this is used
!         over all other values if valid. (Done in Default_SimControls)
!     CRMODEL is read from FILEX.  Use this if no control file.  
!     MODELARG is from command line argument list. Third priority. 
!     Last, use value from DSSATPRO.v??.
C-----------------------------------------------------------------------
!     First check model name from FILEX
      TRY_MODEL = CRMODEL
      CALL MODEL_NAME (CROP, DSSATP, TRY_MODEL, MODEL)

!     If FILEX model name was not acceptable, then try the 
!       model name read from command line.  If this is not OK, 
!       MODEL contains value from DSSATPRO file
      IF (TRY_MODEL /= MODEL) THEN
        CALL MODEL_NAME (CROP, DSSATP, MODELARG, MODEL)
      ENDIF

      IF (MEPHO .EQ. 'L' .AND. MODEL(1:5) .NE. 'CRGRO') THEN
        MEPHO = 'C'
        WRITE(MSG(1),80)
        WRITE (MSG(2),81) MODEL(1:5)
        CALL WARNING(2, "IPEXP ", MSG)

   80 FORMAT('Photosynthesis method (PHOTO in FILEX) has been changed')
   81 FORMAT('from "L" to "C" for compatibility with crop model, '
     &            ,A5,'.') 
      ENDIF

      CALL FILL_ISWITCH(
     &      CONTROL, ISWITCH, FROP, MODEL, NYRS, RNMODE)

!     Planting date needed for generic start of simulation
      SELECT CASE(IPLTI)
      CASE('R'); PLDATE = YRPLT
      CASE('A'); PLDATE = PWDINF
      END SELECT

!     Check Simulation control file for control overrides 
      CALL Default_SimControls(
     &    CONTROL, CRMODEL, DSSATP, FILECTL, ISWITCH,     !Input
     &    MODELARG, PLDATE,                               !Input
     &    UseSimCtr, MODEL)                               !Output

      IF (UseSimCtr) THEN
        IOX     = ISWITCH % FNAME 
        ISIMI   = ISWITCH % ISIMI 
        ISWWAT  = ISWITCH % ISWWAT
        ISWNIT  = ISWITCH % ISWNIT
        ISWSYM  = ISWITCH % ISWSYM
        ISWPHO  = ISWITCH % ISWPHO
        ISWPOT  = ISWITCH % ISWPOT
        ISWDIS  = ISWITCH % ISWDIS
        ISWCHE  = ISWITCH % ISWCHE
        ISWTIL  = ISWITCH % ISWTIL
        ICO2    = ISWITCH % ICO2
        MEWTH   = ISWITCH % MEWTH 
        MESOM   = ISWITCH % MESOM 
        MELI    = ISWITCH % MELI  
        MEEVP   = ISWITCH % MEEVP 
        MEINF   = ISWITCH % MEINF 
        MEPHO   = ISWITCH % MEPHO 
        MEHYD   = ISWITCH % MEHYD 
        MESEV   = ISWITCH % MESEV 
        MESOL   = ISWITCH % MESOL 
        METMP   = ISWITCH % METMP 
        IPLTI   = ISWITCH % IPLTI 
        IIRRI   = ISWITCH % IIRRI 
        IFERI   = ISWITCH % IFERI 
        IRESI   = ISWITCH % IRESI 
        IHARI   = ISWITCH % IHARI 
        IDETO   = ISWITCH % IDETO 
        IDETS   = ISWITCH % IDETS 
        IDETG   = ISWITCH % IDETG 
        IDETC   = ISWITCH % IDETC 
        IDETW   = ISWITCH % IDETW 
        IDETN   = ISWITCH % IDETN 
        IDETP   = ISWITCH % IDETP 
        IDETD   = ISWITCH % IDETD 
        IDETL   = ISWITCH % IDETL 
        IDETH   = ISWITCH % IDETH 
        IDETR   = ISWITCH % IDETR 
        NSWITCH = ISWITCH % NSWI     
      
        NYRS  = CONTROL % NYRS  
        YRSIM = CONTROL % YRSIM 
        MODEL = CONTROL % MODEL 
!       MESIC = CONTROL % MESIC     
        FROP  = CONTROL % FROP

      ENDIF

      CALL PUT(CONTROL)  
      CALL PUT(ISWITCH)

!     --------------------------------------------------------------------
!     Check for N model compatible with crop model
      IF (ISWNIT /= 'N') THEN
        SELECT CASE(MODEL(1:5))
        CASE ('SALUS', 'SCCAN', 'SCCSP')
!           N model has NOT been linked for these models
!           Print a warning message.
            CALL GET_CROPD(CROP, CROPD)
            CROPD = ADJUSTL(CROPD)

            WRITE(MSG(1),
     &         '("Nitrogen dynamics model has not been developed for "
     &         ,A5,1X,A,".")') MODEL(1:5), TRIM(CROPD)
!            MSG(2)="Model will run if soils and species P data" //
!     &         " are supplied."
!            MSG(3)="User must verify validity of crop response."
            MSG(2)="Please contact the CSM development team if " // 
     &         "you wish to "
            WRITE(MSG(3),'("contribute to development of a N model for "
     &          ,A5,1X,A,".")') MODEL(1:5), TRIM(CROPD)
            MSG(4) = "N simulation will be switched off."
            CALL WARNING(4,ERRKEY,MSG)
            ISWNIT = 'N'
            ISWPHO = 'N'
            ISWPOT = 'N'
!           CALL ERROR('IPSIM', 6, "", 0)
        END SELECT
      ENDIF

!     --------------------------------------------------------------------
!     Check for phosphorus model compatible with crop model
!      IF (ISWPHO /= 'N') THEN
!       Check for validity of P model for this crop
!        SELECT CASE(MODEL(1:5))
        !CASE('CRGRO','MZCER','RICER')
        !  SELECT CASE(CONTROL % CROP)
        !  CASE('SB','FA','MZ','RI','PN') 
!           Phosphorus model has been enabled and tested for these crops, do nothing
         
! MA (19dec2013) to test P coupling to SG ceres 
       IF (ISWPHO /= 'N') THEN
        SELECT CASE(MODEL(1:5))
        CASE('CRGRO','MZCER','RICER','SGCER')
          SELECT CASE(CONTROL % CROP)
          CASE('SB','FA','MZ','RI','PN','SG') 
!           Phosphorus model has been enabled and tested for these crops, do nothing

          CASE DEFAULT
!           P model has NOT been tested for the remainder of the CROPGRO crops
!           Print a warning message.
            CALL GET_CROPD(CROP, CROPD)
            CROPD = ADJUSTL(CROPD)

            WRITE(MSG(1),
     &         '("Phosphorus model has not been tested for "
     &         ,A5,1X,A,".")') MODEL(1:5), TRIM(CROPD)
!            MSG(2)="Model will run if soils and species P data" //
!     &         " are supplied."
!            MSG(3)="User must verify validity of crop response."
            MSG(2)="Please contact the CSM development team if " // 
     &         "you wish to "
            WRITE(MSG(3),'("contribute to development of a P model for "
     &          ,A5,1X,A,".")') MODEL(1:5), TRIM(CROPD)
            CALL WARNING(3,ERRKEY,MSG)
            CALL ERROR('IPSIM', 6, "", 0)
          END SELECT

        CASE DEFAULT
!         Crop model has not been linked to P model.
!         Print a warning message. stop the run.
          WRITE(MSG(1),
     &       '("Phosphorus model has not been enabled for ",
     &       A5," model.")') MODEL(1:5)
          MSG(2)="Please contact the CSM development team if you " //
     &          "wish to contribute to "
          WRITE(MSG(3),'("development of a P model for ",A5,".")')
     &        MODEL(1:5)
          CALL WARNING(3,ERRKEY,MSG)
          CALL ERROR('IPSIM', 6, "", 0)
        END SELECT
      ENDIF

!     --------------------------------------------------------------------
!     Check for potassium model compatible with crop model
      IF (ISWPOT /= 'N') THEN
!       Check for validity of K model for this crop
        SELECT CASE(MODEL(1:5))
        CASE('MZCER','RICER')
!          SELECT CASE(CONTROL % CROP)
!          CASE('MZ','RI') 
!!           Potassium model has been enabled and tested for these crops, do nothing
!
!          CASE DEFAULT
!           K model has NOT been tested for the remainder of the CROPGRO crops
!           Print a warning message, but allow the user to continue.
            CALL GET_CROPD(CROP, CROPD)
            CROPD = ADJUSTL(CROPD)

            WRITE(MSG(1),
     &         '("Potassium model has not been tested for "
     &         ,A5,1X,A,".")') MODEL(1:5), TRIM(CROPD)
!            MSG(2)="Model will run if soils and species K data" //
!     &         " are supplied."
!            MSG(3)="User must verify validity of crop response."
            MSG(2)="Please contact the CSM development team if " // 
     &         "you wish to "
            WRITE(MSG(3),'("contribute to development of a K model for "
     &          ,A5,1X,A,".")') MODEL(1:5), TRIM(CROPD)
            CALL WARNING(3,ERRKEY,MSG)
            CALL ERROR('IPSIM', 7, "", 0)
!          END SELECT

        CASE DEFAULT
!         Crop model has not been linked to K model.
!         Print a warning message. stop the run.
          WRITE(MSG(1),
     &       '("Potassium model has not been enabled for ",
     &       A5," model.")') MODEL(1:5)
          MSG(2)="Please contact the CSM development team if you " //
     &          "wish to contribute to "
          WRITE(MSG(3),'("development of a K model for ",A5,".")')
     &        MODEL(1:5)
          CALL WARNING(3,ERRKEY,MSG)
          CALL ERROR('IPSIM', 7, "", 0)
        END SELECT
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

  55  FORMAT (I3,11X,2(1X,I5),5X,A1,1X,I5,1X,I5,1X,A25,1X,A8)
  60  FORMAT (I3,11X,9(5X,A1))
  61  FORMAT (I3,11X,7(5X,A1),5X,I1,4(5X,A1))
  65  FORMAT (I3,11X,3(5X,A1),4X,I2,9(5X,A1))
  66  FORMAT (I3,11X,2(1X,I5),5(1X,F5.0))
  67  FORMAT (I3,11X,3(1X,F5.0),2(1X,A5),1X,F5.0,1X,F5.0)
  68  FORMAT (I3,11X,1X,F5.0,1X,I5,1X,F5.0)
  70  FORMAT (3X,I2)

      END SUBROUTINE IPSIM

!=======================================================================
!  FILL_ISWITCH, Subroutine
!
!  Copies values to ISWITCH variable, determines what values are carried
!     over in sequence runs.
!-----------------------------------------------------------------------
!  Revision history
!  10/26/2007 CHP Written
!-----------------------------------------------------------------------
!  Called : IPSIM
!  Calls  : none
!=======================================================================
      SUBROUTINE FILL_ISWITCH(
     &      CONTROL, ISWITCH, FROP, MODEL, NYRS, RNMODE)
      USE ModuleDefs 
      USE ModuleData
      INCLUDE 'COMSWI.BLK'
      INCLUDE 'COMIBS.BLK'

      CHARACTER*1 RNMODE
      CHARACTER*8 MODEL
      INTEGER FROP, NYRS
      
      TYPE (SwitchType) ISWITCH
      TYPE (ControlType)CONTROL

!     Skip some variables for sequenced runs -- need to keep values
!     from first run
      IF (INDEX('FQ',RNMODE) <= 0 .OR. CONTROL % RUN == 1) THEN
        ISWITCH % ISWWAT = ISWWAT  !water simulation
        ISWITCH % ISWNIT = ISWNIT  !N simulation
        ISWITCH % ISWPHO = ISWPHO  !P simulation
        ISWITCH % ISWPOT = ISWPOT  !K simulation
        ISWITCH % ISIMI  = ISIMI   !start of simulation switch
        ISWITCH % ICO2   = ICO2    !atmospheric CO2 data source
        ISWITCH % MEWTH  = MEWTH   !weather data source
        ISWITCH % MESOM  = MESOM   !SOM method
        ISWITCH % MEINF  = MEINF   !infiltration method (mulch effects)
        ISWITCH % MEHYD  = MEHYD   !hydrology
        ISWITCH % MESEV  = MESEV   !soil evaporation
        ISWITCH % MESOL  = MESOL   !soil layer distribution
        ISWITCH % METMP  = METMP
        ISWITCH % IDETO  = IDETO   !overview file
        ISWITCH % IDETS  = IDETS   !summary file
        ISWITCH % IDETG  = IDETG   !growth output files
        ISWITCH % IDETC  = IDETC   !carbon output
        ISWITCH % IDETW  = IDETW   !water output
        ISWITCH % IDETN  = IDETN   !N output
        ISWITCH % IDETP  = IDETP   !P output
        ISWITCH % IDETD  = IDETD   !disease and pest output 
        ISWITCH % IDETL  = IDETL   !detail output (verbosity)
        ISWITCH % IDETH  = IDETH   !chemical output
        ISWITCH % IDETR  = IDETR   !management operations output
        ISWITCH % NSWI   = NSWITCH !N computations switch
        CONTROL % NYRS   = NYRS    !number of years simulated
        CONTROL % FROP   = FROP    !frequency of output

!       chp moved 12/9/2009
        ISWITCH % MEEVP  = MEEVP     !potential ET method
        ISWITCH % FNAME  = IOX       !output file name
      ENDIF
 
!     Use these values for all runs
      ISWITCH % ISWDIS = ISWDIS    !pests and disease
      ISWITCH % ISWCHE = ISWCHE    !chemical application
      ISWITCH % ISWTIL = ISWTIL    !tillage
      ISWITCH % MELI   = MELI      !light interception
      ISWITCH % IPLTI  = IPLTI     !planting switch
      ISWITCH % IIRRI  = IIRRI     !irrigation switch
      ISWITCH % IFERI  = IFERI     !fertilizer switch
      ISWITCH % IRESI  = IRESI     !residue addition switch
      ISWITCH % IHARI  = IHARI     !harvest switch
    
      CONTROL % YRSIM  = YRSIM     !simulation start date
      CONTROL % MODEL  = MODEL     !crop growth model
!     CONTROL % MESIC  = MESIC     !initial conditions (no longer used)

!     chp moved 12/9/2009
      ISWITCH % ISWSYM = ISWSYM    !symbiosis (N-fixation)
      ISWITCH % MEPHO  = MEPHO     !photsynthesis method

      CALL PUT(ISWITCH)
      CALL PUT(CONTROL)

      RETURN
      END SUBROUTINE FILL_ISWITCH
!=======================================================================


!=======================================================================
!  Default_SimControls, Subroutine
!
!  Reads default simulation controls file. These values override the 
!     values in the FileX simulation controls.
!-----------------------------------------------------------------------
!  Revision history
!  06/29/2007 CHP Written
!-----------------------------------------------------------------------
!  Called : IPSIM
!
!  Calls  : ERROR IGNORE FIND YR_DOY
!=======================================================================

      SUBROUTINE Default_SimControls(
     &    CONTROL, CRMODEL, DSSATP, FILECTL, ISWITCH,     !Input
     &    MODELARG, PLDATE,                               !Input
     &    UseSimCtr, MODEL)                               !Output

      USE ModuleDefs
      IMPLICIT NONE
      SAVE

      CHARACTER*1 UPCASE,ISIMI, MEPHO_SAVE, ISWSYM_SAVE
      CHARACTER*1 ISWWAT,ISWNIT,ISWSYM,ISWPHO,ISWPOT,ISWDIS,MEWTH,MESIC
      CHARACTER*1 ICO2
      CHARACTER*1 MELI,MEEVP,MEINF,MEPHO,IPLTI,IIRRI,IFERI,IRESI,IHARI
      CHARACTER*1 ISWCHE,ISWTIL,MEHYD,MESOM, MESOL, MESEV, METMP
      CHARACTER*1 IDETO,IDETS,IDETG,IDETC,IDETW,IDETN,IDETP,IDETD,IOX
      CHARACTER*1 IDETH,IDETL, IDETR
      CHARACTER*6 ERRKEY,FINDCH, SECTION
      CHARACTER*8 MODEL, CRMODEL, CTRMODEL, MODELARG, TRY_MODEL
      CHARACTER*12 FILEX  !, DSSATS
      CHARACTER*78 MSG(50)
      CHARACTER*102 DSSATP, SIMCTR
      CHARACTER*120 INPUTX, FILECTL
      CHARACTER*128 CHARTEST

      INTEGER CTRNO, ERRNUM, FOUND, FROP, I, IFIND, IPX, ISECT, ISIM
      INTEGER LEVEL, LINEXP, NMSG, NREPSQ, NSWITCH, NYRS
      INTEGER PLDATE, RSEED1, SCLun, YEAR, YRSIM
      INTEGER SimLen, LenString, FIND_IN_FILE

      TYPE (SwitchType)  ISWITCH
      TYPE (ControlType) CONTROL

      LOGICAL FIRST, FEXIST, UseSimCtr
      DATA FIRST /.TRUE./

      PARAMETER (ERRKEY = 'SIMCTR')

!-----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        UseSimCtr = .FALSE.

        IF (LEN(TRIM(FILECTL)) < 4 .OR. INDEX(FILECTL," ") < 4) RETURN

        I = INDEX(FILECTL,SLASH)
        IF (I < 1) THEN
!         No path provided -- look first in DSSAT46 directory
          CALL GETARG (0,INPUTX)      !Name of model executable
          IPX = LEN_TRIM(INPUTX)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   Temporarily fix DSSATPRO file name for debugging purposes:
!     To use these Debug lines of code (letter D in column 1) with CVF:
!     1) Go to pull down menu Project -> Settings -> Fortran (Tab) ->
!       Debug (Category) -> Check box for Compile Debug(D) Lines
!     2)  Specify name of DSSATPRO file here:
D     INPUTX = 'C:\DSSAT46\DSCSM046.EXE'
D     IPX = 23
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          IF (IPX > 12) THEN
            DO I = IPX, 0, -1
              IF (INPUTX(I:I) .EQ. SLASH) EXIT
            END DO
            SIMCTR = INPUTX(1:I) // FILECTL
          ELSE
            RETURN
          ENDIF
        ELSE
          SIMCTR = FILECTL
        ENDIF

        INQUIRE (FILE = SIMCTR, EXIST = FEXIST)
        IF (.NOT. FEXIST) THEN
          SIMCTR = STDPATH // FILECTL
        ENDIF

        INQUIRE (FILE = SIMCTR, EXIST = FEXIST)
        IF (.NOT. FEXIST) THEN
          MSG(1) = "Simulation controls file does not exist."
          MSG(2) = SIMCTR
          MSG(3) = "Use controls from experiment file."
          CALL WARNING(3,ERRKEY,MSG)
          RETURN
        ENDIF

        CALL GETLUN('SIMCNTL', SCLun)
        OPEN (UNIT = SCLun, FILE = SIMCTR, IOSTAT =ERRNUM)
        IF (ERRNUM /= 0) RETURN

        SECTION = '@CTRNO'
        FOUND = FIND_IN_FILE(SECTION,SCLun)
        IF (FOUND == 1) THEN
          CALL IGNORE(SCLun,LINEXP,ISECT,CHARTEST)
          IF (ISECT == 1) THEN
            READ(CHARTEST,"(I6)",IOSTAT=ERRNUM) CTRNO
          ELSE
            ERRNUM = 100
          ENDIF
          IF (ERRNUM > 0) THEN
            MSG(1) = "Error reading simulation control file."
            WRITE(MSG(2),'(A,A)') "File: ",SIMCTR(1:72)
            CALL WARNING(2, ERRKEY, MSG)
            RETURN
          ENDIF
          IF (CTRNO < 1) THEN
            MSG(1) = "External control file:"
            WRITE(MSG(2),'(A,A)') "  ", SIMCTR(1:76)
            WRITE(MSG(3),'(A,I6,A)') "Control level: ", CTRNO, 
     &        "    No external controls will be used."
            CALL WARNING(3, ERRKEY, MSG)
            RETURN
          ENDIF
        ELSE
          MSG(1) = "Error reading simulation control file."
          WRITE(MSG(2),'(A,A)') "File: ",SIMCTR(1:72)
          WRITE(MSG(3),'(A,A)') "Control section not found: ", SECTION
          CALL WARNING(3, ERRKEY, MSG)
          RETURN
        ENDIF

        CONTROL % SimControl = SIMCTR
        SimLen = LenString(CONTROL % SimControl)
        MSG(1) = "Simulation Controls override with file:"
        WRITE(MSG(2),'(A)') CONTROL % SimControl(1:SimLen)
        WRITE(MSG(3),'(A,I6)') "Control Level: ", CTRNO
        MSG(4)="The following switches and options will override values"
        MSG(5)="  found in the Experiment files, if appropriate:"
        NMSG = 5

!       Initialize override values
        NYRS    = -99
        NREPSQ  = -99
        ISIMI   = ' '
        YRSIM   = -99
        RSEED1  = -99

        ISWWAT  = ' '
        ISWNIT  = ' '
        NSWITCH = -99
        ISWSYM  = ' '
        ISWPHO  = ' '
        ISWPOT  = ' '
        ISWDIS  = ' '
        ISWCHE  = ' '
        ISWTIL  = ' '

        ICO2    = ' '
        MEWTH   = ' '
        MESIC   = ' '
        MELI    = ' '
        MEEVP   = ' '
        MEINF   = ' '
        MEPHO   = ' '
        MEHYD   = ' '
        NSWITCH = -99
        MESOM   = ' '
        METMP   = ' '
        MESOL   = ' '
        MESEV   = ' '
        IPLTI   = ' '
        IIRRI   = ' '
        IFERI   = ' '
        IRESI   = ' '
        IHARI   = ' '
        IOX     = ' '
        FROP    = -99
        IDETO   = ' '
        IDETS   = ' '
        IDETG   = ' '
        IDETN   = ' '
        IDETC   = ' '
        IDETW   = ' '
        IDETP   = ' '
        IDETD   = ' '
        IDETL   = ' '
        IDETH   = ' '
        IDETR   = ' '
!        CRMODEL = '     '

!       Read FIRST line of simulation control
        REWIND(SCLun)

        LEVEL = 0
        FINDCH = '@N CON'
        DO WHILE (LEVEL /= CTRNO)
          CALL FIND (SCLun,FINDCH,LINEXP,IFIND)
          IF (IFIND == 1) THEN
!           Found a good section
            CALL IGNORE(SCLun,LINEXP,ISECT,CHARTEST)
            IF (ISECT == 1) THEN
              READ (CHARTEST,'(I2)',IOSTAT=ERRNUM) LEVEL
              IF (ERRNUM /= 0) RETURN
            ENDIF
          ELSE
            RETURN
          ENDIF
        ENDDO
          
!       Read simulation controls
        DO WHILE (ERRNUM == 0)
          CALL IGNORE2(SCLun,LINEXP,ISECT,CHARTEST)
          IF (ISECT == 0) EXIT
          SELECT CASE(CHARTEST(1:6))

!         First line of simulation controls
          CASE('@N GEN')
            CALL IGNORE(SCLun,LINEXP,ISECT,CHARTEST)
            READ (CHARTEST,'(I2)',IOSTAT=ERRNUM) LEVEL
            IF (ERRNUM /= 0) EXIT
            IF (LEVEL /= CTRNO) EXIT 

!           READ (CHARTEST,55,IOSTAT=ERRNUM) NYRS,NREPSQ,ISIMI,
!     &           YRSIM,RRSEED1,TITSIM,CRMODEL
!  55       FORMAT (14X,2(1X,I5),5X,A1,1X,I5,1X,I5,1X,A25,1X,A8)

            READ (CHARTEST,'(15X,I5)',IOSTAT=ERRNUM) NYRS
            CALL CHECK_I('NYRS', NYRS, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(21X,I5)',IOSTAT=ERRNUM) NREPSQ
            CALL CHECK_I('NREPSQ', NREPSQ, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(31X,A1)',IOSTAT=ERRNUM) ISIMI
            CALL CHECK_A('ISIMI', ISIMI, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(33X,I5)',IOSTAT=ERRNUM) YRSIM
            IF (YRSIM == -99) YRSIM = PLDATE
            CALL CHECK_I('YRSIM', YRSIM, ERRNUM, MSG, NMSG)
            IF (ERRNUM == 0) THEN
              CALL Y2K_DOY (YRSIM)
              CALL YR_DOY (YRSIM,YEAR,ISIM)
            ENDIF

!            READ (CHARTEST,'(39X,I5)',IOSTAT=ERRNUM) RSEED1
!            CALL CHECK_I(ERRNUM, 'NYRS', NYRS, MSG, NMSG)

            READ (CHARTEST,'(71X,A8)',IOSTAT=ERRNUM) CTRMODEL
            CALL CHECK_A('CTRMODEL', CTRMODEL, ERRNUM, MSG, NMSG)

!         Second line of simulation controls
          CASE('@N OPT')
            CALL IGNORE(SCLun,LINEXP,ISECT,CHARTEST)
            READ (CHARTEST,'(I2)',IOSTAT=ERRNUM) LEVEL
            IF (ERRNUM /= 0) EXIT
            IF (LEVEL /= CTRNO) EXIT 

!           READ (CHARTEST,60,IOSTAT=ERRNUM) LN,ISWWAT,ISWNIT,ISWSYM,
!     &         ISWPHO,ISWPOT,ISWDIS,ISWCHE,ISWTIL, ISWFWT
!  60       FORMAT (I3,11X,9(5X,A1))

            READ (CHARTEST,'(19X,A1)',IOSTAT=ERRNUM) ISWWAT
            CALL CHECK_A('ISWWAT', ISWWAT, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(25X,A1)',IOSTAT=ERRNUM) ISWNIT
            CALL CHECK_A('ISWNIT', ISWNIT, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(31X,A1)',IOSTAT=ERRNUM) ISWSYM
            CALL CHECK_A('ISWSYM', ISWSYM, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(37X,A1)',IOSTAT=ERRNUM) ISWPHO
            CALL CHECK_A('ISWPHO', ISWPHO, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(43X,A1)',IOSTAT=ERRNUM) ISWPOT
            CALL CHECK_A('ISWPOT', ISWPOT, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(49X,A1)',IOSTAT=ERRNUM) ISWDIS
            CALL CHECK_A('ISWDIS', ISWDIS, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(55X,A1)',IOSTAT=ERRNUM) ISWCHE
            CALL CHECK_A('ISWCHE', ISWCHE, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(61X,A1)',IOSTAT=ERRNUM) ISWTIL
            CALL CHECK_A('ISWTIL', ISWTIL, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(67X,A1)',IOSTAT=ERRNUM) ICO2
            CALL CHECK_A('ICO2  ', ICO2, ERRNUM, MSG, NMSG)

            ISWWAT = UPCASE(ISWWAT)
            ISWNIT = UPCASE(ISWNIT)
            ISWSYM = UPCASE(ISWSYM)
            ISWPHO = UPCASE(ISWPHO)
            ISWPOT = UPCASE(ISWPOT)
            ISWDIS = UPCASE(ISWDIS)
            ISWCHE = UPCASE(ISWCHE)
            ISWTIL = UPCASE(ISWTIL)
            ICO2   = UPCASE(ICO2)

            IF (ISWWAT .EQ. 'N') THEN
              ISWNIT = 'N'
              ISWCHE = 'N'
            ENDIF

            IF (ISWNIT .EQ. 'N') THEN
              ISWPHO = 'N'
              ISWPOT = 'N'
            ENDIF

!         Third line of simulation controls
          CASE('@N MET')
            CALL IGNORE(SCLun,LINEXP,ISECT,CHARTEST)
            READ (CHARTEST,'(I2)',IOSTAT=ERRNUM) LEVEL
            IF (ERRNUM /= 0) EXIT
            IF (LEVEL /= CTRNO) EXIT 

!           READ (CHARTEST,61,IOSTAT=ERRNUM) LN,MEWTH,MESIC,
!    &           MELI,MEEVP,MEINF,MEPHO,MEHYD,NSWITCH, 
!    &           MESOM, MESEV, MESOL, METMP
! 61        FORMAT (I3,11X,7(5X,A1),5X,I1,5X,A1,2(5X,A1),5X,I1)

            READ (CHARTEST,'(19X,A1)',IOSTAT=ERRNUM) MEWTH
            CALL CHECK_A('MEWTH', MEWTH, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(25X,A1)',IOSTAT=ERRNUM) MESIC
            CALL CHECK_A('MESIC', MESIC, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(31X,A1)',IOSTAT=ERRNUM) MELI
            CALL CHECK_A('MELI', MELI, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(37X,A1)',IOSTAT=ERRNUM) MEEVP
            CALL CHECK_A('MEEVP', MEEVP, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(43X,A1)',IOSTAT=ERRNUM) MEINF
            CALL CHECK_A('MEINF', MEINF, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(49X,A1)',IOSTAT=ERRNUM) MEPHO
            CALL CHECK_A('MEPHO', MEPHO, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(55X,A1)',IOSTAT=ERRNUM) MEHYD
            CALL CHECK_A('MEHYD', MEHYD, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(61X,I1)',IOSTAT=ERRNUM) NSWITCH
            CALL CHECK_I('NSWITCH', NSWITCH, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(67X,A1)',IOSTAT=ERRNUM) MESOM
            CALL CHECK_A('MESOM', MESOM, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(73X,A1)',IOSTAT=ERRNUM) MESEV
            CALL CHECK_A('MESEV', MESEV, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(79X,A1)',IOSTAT=ERRNUM) MESOL
            CALL CHECK_A('MESOL', MESOL, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(85X,A1)',IOSTAT=ERRNUM) METMP
            CALL CHECK_A('METMP', METMP, ERRNUM, MSG, NMSG)

            MEWTH = UPCASE(MEWTH)
            MESIC = UPCASE(MESIC)
            MELI  = UPCASE(MELI)
            MEEVP = UPCASE(MEEVP)
            MEINF = UPCASE(MEINF)
            MEPHO = UPCASE(MEPHO)
            MESOM = UPCASE(MESOM)
            MEHYD = UPCASE(MEHYD)
            MESEV = UPCASE(MESEV)
            METMP = UPCASE(METMP)

            IF (INDEX('PG' ,MESOM) == 0) MESOM = ' '
            IF (INDEX('123',MESOL) == 0) MESOL = ' '
            IF (INDEX('RS' ,MESEV) == 0) MESEV = ' '
            IF (INDEX('Z'  ,MEEVP)  > 0) MEPHO = 'L'
            IF (INDEX('ED' ,METMP) == 0) METMP = 'D'

!         Fourth line of simulation controls
          CASE('@N MAN')
            CALL IGNORE(SCLun,LINEXP,ISECT,CHARTEST)
            READ (CHARTEST,'(I2)',IOSTAT=ERRNUM) LEVEL
            IF (ERRNUM /= 0) EXIT
            IF (LEVEL /= CTRNO) EXIT 

!           READ (CHARTEST,60,IOSTAT=ERRNUM) LN,IPLTI,IIRRI,
!    &           IFERI,IRESI,IHARI
!  60       FORMAT (I3,11X,9(5X,A1))

            READ (CHARTEST,'(19X,A1)',IOSTAT=ERRNUM) IPLTI
            CALL CHECK_A('IPLTI', IPLTI, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(25X,A1)',IOSTAT=ERRNUM) IIRRI
            CALL CHECK_A('IIRRI', IIRRI, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(31X,A1)',IOSTAT=ERRNUM) IFERI
            CALL CHECK_A('IFERI', IFERI, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(37X,A1)',IOSTAT=ERRNUM) IRESI
            CALL CHECK_A('IRESI', IRESI, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(43X,A1)',IOSTAT=ERRNUM) IHARI
            CALL CHECK_A('IHARI', IHARI, ERRNUM, MSG, NMSG)

            IPLTI = UPCASE(IPLTI)
            IIRRI = UPCASE(IIRRI)
            IFERI = UPCASE(IFERI)
            IRESI = UPCASE(IRESI)
            IHARI = UPCASE(IHARI)

!         Fifth line of simulation controls
          CASE('@N OUT')
            CALL IGNORE(SCLun,LINEXP,ISECT,CHARTEST)
            READ (CHARTEST,'(I2)',IOSTAT=ERRNUM) LEVEL
            IF (ERRNUM /= 0) EXIT
            IF (LEVEL /= CTRNO) EXIT 

!           READ (CHARTEST,65,IOSTAT=ERRNUM) LN,IOX,IDETO,
!    &      IDETS,FROP,IDETG,IDETC,IDETW,IDETN,IDETP,IDETD,
!    &      IDETL,IDETH,IDETR
! 65        FORMAT (I3,11X,3(5X,A1),4X,I2,9(5X,A1))

            READ (CHARTEST,'(19X,A1)',IOSTAT=ERRNUM) IOX
            CALL CHECK_A('FNAME', IOX, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(25X,A1)',IOSTAT=ERRNUM) IDETO
            CALL CHECK_A('IDETO', IDETO, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(31X,A1)',IOSTAT=ERRNUM) IDETS
            CALL CHECK_A('IDETS', IDETS, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(35X,I3)',IOSTAT=ERRNUM) FROP
            CALL CHECK_I('FROP', FROP, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(43X,A1)',IOSTAT=ERRNUM) IDETG
            CALL CHECK_A('IDETG', IDETG, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(49X,A1)',IOSTAT=ERRNUM) IDETC
            CALL CHECK_A('IDETC', IDETC, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(55X,A1)',IOSTAT=ERRNUM) IDETW
            CALL CHECK_A('IDETW', IDETW, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(61X,A1)',IOSTAT=ERRNUM) IDETN
            CALL CHECK_A('IDETN', IDETN, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(67X,A1)',IOSTAT=ERRNUM) IDETP
            CALL CHECK_A('IDETP', IDETP, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(73X,A1)',IOSTAT=ERRNUM) IDETD
            CALL CHECK_A('IDETD', IDETD, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(79X,A1)',IOSTAT=ERRNUM) IDETL
            CALL CHECK_A('IDETL', IDETL, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(85X,A1)',IOSTAT=ERRNUM) IDETH
            CALL CHECK_A('IDETH', IDETH, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(91X,A1)',IOSTAT=ERRNUM) IDETR
            CALL CHECK_A('IDETR', IDETR, ERRNUM, MSG, NMSG)

            IOX   = UPCASE(IOX)
            IDETO = UPCASE(IDETO)
            IDETS = UPCASE(IDETS)
            IDETG = UPCASE(IDETG)
            IDETC = UPCASE(IDETC)
            IDETW = UPCASE(IDETW)
            IDETN = UPCASE(IDETN)
            IDETP = UPCASE(IDETP)
            IDETD = UPCASE(IDETD)
            IDETL = UPCASE(IDETL)
            IDETH = UPCASE(IDETH)
            IDETR = UPCASE(IDETR)

!           Verbose output switch
            IF (IDETL == '0') THEN
!             VBOSE = zero, suppress all output except Summary and Evaluate
              IDETS = 'Y'
              IDETG = 'N' 
              IDETC = 'N' 
              IDETW = 'N' 
              IDETN = 'N' 
              IDETP = 'N' 
              IDETD = 'N' 
              IDETH = 'N' 
              IDETR = 'N' 
              IDETO = 'E'
!             Seasonal and spatial runs do not get evaluate file when IDETL=0
              IF (INDEX('SN',CONTROL%RNMODE) > 0) IDETO = 'N'
            ELSEIF (IDETL == 'A') THEN
!             VBOSE = 'A', generate all output
              IDETS = 'A'
              IDETO = 'Y'
              IDETG = 'Y' 
              IDETC = 'Y' 
              IDETW = 'Y' 
              IDETN = 'Y' 
              IDETP = 'Y' 
              IDETD = 'Y' 
              IDETH = 'Y' 
              IDETR = 'Y' 
!             Set IDETL back to "D" so no need for changes elsewhere
!             IDETL = 'D' 
              FROP  = 1 
            ENDIF

          END SELECT
        ENDDO

        CLOSE (SCLun)

        IF (NMSG < 6) THEN
          MSG(4)='No default simulation controls read.'
          NMSG = 4
        ELSE
          UseSimCtr = .TRUE.
        ENDIF
        CALL WARNING(NMSG,ERRKEY,MSG)
        CALL INFO (NMSG,ERRKEY,MSG)

      ELSE
        IF (.NOT. UseSimCtr) RETURN
        MEPHO  = MEPHO_SAVE
        ISWSYM = ISWSYM_SAVE
      ENDIF

C-----------------------------------------------------------------------
C    Select Model Name and Path -- order of priority:
!     CTRMODEL is value from control file override -- this is used
!         over all other values if valid. (Done in Default_SimControls)
!     CRMODEL is read from FILEX.  Use this if no control file.  
!     MODELARG is from command line argument list. Third priority. 
!     Last, use value from DSSATPRO file
C-----------------------------------------------------------------------
!     First check model from simulation control file 
      TRY_MODEL = CTRMODEL
      CALL MODEL_NAME (CONTROL%CROP, DSSATP, TRY_MODEL, MODEL)

!     If model name from simulation control file is not acceptable,
!     try value from FILEX
      IF (TRY_MODEL /= MODEL) THEN
        TRY_MODEL = CRMODEL
        CALL MODEL_NAME (CONTROL%CROP, DSSATP, TRY_MODEL, MODEL)

!       If FILEX model name was not acceptable, then try the 
!       model name read from command line.  If this is not OK, 
!       MODEL contains value from DSSATPRO file
        IF (TRY_MODEL /= MODEL) THEN
          TRY_MODEL = MODELARG
          CALL MODEL_NAME (CONTROL%CROP, DSSATP, TRY_MODEL, MODEL)
        ENDIF
      ENDIF

      MEPHO_SAVE = MEPHO
      IF (MEPHO .EQ. 'L' .AND. CTRMODEL(1:5) .NE. 'CRGRO') THEN
        MEPHO = 'C'
        MSG(1)='Photosynthesis method (PHOTO in FILEX) has been changed'
        WRITE (MSG(2),81) CTRMODEL(1:5)
   81   FORMAT('from "L" to "C" for compatibility with crop model, '
     &            ,A5,'.') 
        CALL WARNING(2, "IPSIM ", MSG)
      ENDIF

      ISWSYM_SAVE = ISWSYM
      SELECT CASE (CONTROL % CROP)
        CASE ('BN','SB','PN','PE','CH','PP',
     &          'VB','CP','CB','FB','GB','LT')
!         Do nothing -- CROPGRO crops can have Y or N
        CASE DEFAULT; ISWSYM = 'N'  !other crops don't have a choice
      END SELECT

      IF ((INDEX('CSPT',CONTROL % CROP)) .GT. 0) THEN
        IF (IHARI .EQ. 'A') THEN
          MSG(1) = "Default Simulation controls file used."
          WRITE(MSG(2),'("Automatic harvest option is not valid for ",
     &    "crop type: ",A2)') CONTROL%CROP
          CALL WARNING(2, ERRKEY, MSG)
          CALL ERROR ('IPSIM ',4,FILEX,LINEXP)
        ENDIF
      ENDIF

      IF ((INDEX('PT',CONTROL % CROP)) .GT. 0) THEN
        IF (IPLTI .EQ. 'A') THEN
          MSG(1) = "Default Simulation controls file used."
          WRITE(MSG(2),'("Automatic planting option is not valid for ",
     &    "crop type: ",A2)') CONTROL%CROP
          CALL WARNING(2, ERRKEY, MSG)
          CALL ERROR ('IPSIM ',5,FILEX,LINEXP)
        ENDIF
      ENDIF

!     Fill ISWITCH variable (complete)
      IF (IOX    /= ' ' .AND. IOX /= '.')    ISWITCH % FNAME  = IOX 
      IF (ISIMI  /= ' ' .AND. ISIMI  /= '.') ISWITCH % ISIMI  = ISIMI 
      IF (ISWWAT /= ' ' .AND. ISWWAT /= '.') ISWITCH % ISWWAT = ISWWAT
      IF (ISWNIT /= ' ' .AND. ISWNIT /= '.') ISWITCH % ISWNIT = ISWNIT
      IF (ISWSYM /= ' ' .AND. ISWSYM /= '.') ISWITCH % ISWSYM = ISWSYM
      IF (ISWPHO /= ' ' .AND. ISWPHO /= '.') ISWITCH % ISWPHO = ISWPHO
      IF (ISWPOT /= ' ' .AND. ISWPOT /= '.') ISWITCH % ISWPOT = ISWPOT
      IF (ISWDIS /= ' ' .AND. ISWDIS /= '.') ISWITCH % ISWDIS = ISWDIS
      IF (ISWCHE /= ' ' .AND. ISWCHE /= '.') ISWITCH % ISWCHE = ISWCHE
      IF (ISWTIL /= ' ' .AND. ISWTIL /= '.') ISWITCH % ISWTIL = ISWTIL
      IF (ICO2   /= ' ' .AND. ICO2   /= '.') ISWITCH % ICO2   = ICO2
      IF (MEWTH  /= ' ' .AND. MEWTH  /= '.') ISWITCH % MEWTH  = MEWTH
      IF (MESOM  /= ' ' .AND. MESOM  /= '.') ISWITCH % MESOM  = MESOM
      IF (MELI   /= ' ' .AND. MELI   /= '.') ISWITCH % MELI   = MELI 
      IF (MEEVP  /= ' ' .AND. MEEVP  /= '.') ISWITCH % MEEVP  = MEEVP
      IF (MEINF  /= ' ' .AND. MEINF  /= '.') ISWITCH % MEINF  = MEINF
      IF (MEPHO  /= ' ' .AND. MEPHO  /= '.') ISWITCH % MEPHO  = MEPHO
      IF (MEHYD  /= ' ' .AND. MEHYD  /= '.') ISWITCH % MEHYD  = MEHYD
      IF (MESEV  /= ' ' .AND. MESEV  /= '.') ISWITCH % MESEV  = MESEV
      IF (MESOL  /= ' ' .AND. MESOL  /= '.') ISWITCH % MESOL  = MESOL
      IF (METMP  /= ' ' .AND. METMP  /= '.') ISWITCH % METMP  = METMP
      IF (IPLTI  /= ' ' .AND. IPLTI  /= '.') ISWITCH % IPLTI  = IPLTI
      IF (IIRRI  /= ' ' .AND. IIRRI  /= '.') ISWITCH % IIRRI  = IIRRI
      IF (IFERI  /= ' ' .AND. IFERI  /= '.') ISWITCH % IFERI  = IFERI
      IF (IRESI  /= ' ' .AND. IRESI  /= '.') ISWITCH % IRESI  = IRESI
      IF (IHARI  /= ' ' .AND. IHARI  /= '.') ISWITCH % IHARI  = IHARI
      IF (IDETO  /= ' ' .AND. IDETO  /= '.') ISWITCH % IDETO  = IDETO
      IF (IDETS  /= ' ' .AND. IDETS  /= '.') ISWITCH % IDETS  = IDETS
      IF (IDETG  /= ' ' .AND. IDETG  /= '.') ISWITCH % IDETG  = IDETG
      IF (IDETC  /= ' ' .AND. IDETC  /= '.') ISWITCH % IDETC  = IDETC
      IF (IDETW  /= ' ' .AND. IDETW  /= '.') ISWITCH % IDETW  = IDETW
      IF (IDETN  /= ' ' .AND. IDETN  /= '.') ISWITCH % IDETN  = IDETN
      IF (IDETP  /= ' ' .AND. IDETP  /= '.') ISWITCH % IDETP  = IDETP
      IF (IDETD  /= ' ' .AND. IDETD  /= '.') ISWITCH % IDETD  = IDETD
      IF (IDETL  /= ' ' .AND. IDETL  /= '.') ISWITCH % IDETL  = IDETL
      IF (IDETH  /= ' ' .AND. IDETH  /= '.') ISWITCH % IDETH  = IDETH
      IF (IDETR  /= ' ' .AND. IDETR  /= '.') ISWITCH % IDETR  = IDETR

      IF (NSWITCH /=-99) ISWITCH % NSWI   = NSWITCH
    
!       Fill CONTROL variable (partial)
      IF (MODEL(1:1) /= ' ' .AND. MODEL(1:1) /= '.') 
     &                                     CONTROL % MODEL = MODEL
!     IF (MESIC /= ' ' .AND. MESIC /= '.') CONTROL % MESIC = MESIC  
      IF (NYRS  /= -99) CONTROL % NYRS  = NYRS
      IF (YRSIM /= -99) CONTROL % YRSIM = YRSIM  
      IF (FROP  > 0)    CONTROL % FROP  = FROP   

      RETURN
      END SUBROUTINE Default_SimControls
!=======================================================================

      SUBROUTINE CHECK_A(LABEL, VALUE, ERRNUM, MSG, NMSG)
      IMPLICIT NONE

      CHARACTER*(*) VALUE
      CHARACTER*(*) LABEL
      CHARACTER*30 MSG_TEXT
      CHARACTER*78 MSG(50)
      INTEGER ERRNUM, NMSG

      IF (ERRNUM /= 0)  THEN
        VALUE = ' '
      ENDIF

      IF (VALUE /= ' ' .AND. VALUE /= '.') THEN
        NMSG = NMSG + 1
        WRITE(MSG(NMSG),'(A8,A,A,2X,A30)') LABEL, " = ", VALUE, 
     &    MSG_TEXT(LABEL)
      ENDIF

      RETURN
      END SUBROUTINE CHECK_A

!=======================================================================

      SUBROUTINE CHECK_I(LABEL, VALUE, ERRNUM, MSG, NMSG)
      IMPLICIT NONE

      INTEGER VALUE
      CHARACTER*(*) LABEL
      CHARACTER*30 MSG_TEXT
      CHARACTER*78 MSG(50)
      INTEGER ERRNUM, NMSG

      IF (ERRNUM /= 0)  THEN
        VALUE = -99
      ENDIF

      IF (VALUE > 0) THEN
        NMSG = NMSG + 1
        IF (VALUE < 10) THEN
          WRITE(MSG(NMSG),'(A8,A,I1,2X,A30)') LABEL," = ",VALUE, 
     &      MSG_TEXT(LABEL)  
        ELSE
          WRITE(MSG(NMSG),'(A8,A,I8,2X,A30)') LABEL," = ",VALUE, 
     &      MSG_TEXT(LABEL)  
        ENDIF
      ELSE
        VALUE = -99
      ENDIF

      RETURN
      END SUBROUTINE CHECK_I

!=======================================================================

!=======================================================================
      CHARACTER*30 FUNCTION MSG_TEXT(LABEL)

      CHARACTER*(*) LABEL

      SELECT CASE(LABEL)  !    "123456789012345678901234567890"
      CASE('FNAME');  MSG_TEXT="Alternate file name option    "
      CASE('ISIMI');  MSG_TEXT="Start of simulation code      "
      CASE('ISWWAT'); MSG_TEXT="Soil water simulation switch  "
      CASE('ISWNIT'); MSG_TEXT="Soil N simulation switch      "
      CASE('ISWSYM'); MSG_TEXT="N fixation switch             "
      CASE('ISWPHO'); MSG_TEXT="P simulation switch           "
      CASE('ISWPOT'); MSG_TEXT="Potassium simulation switch   "
      CASE('ISWDIS'); MSG_TEXT="Pest & disease simulation     "
      CASE('ISWCHE'); MSG_TEXT="Chemical application switch   "
      CASE('ISWTIL'); MSG_TEXT="Tillage option switch         "
      CASE('ICO2');   MSG_TEXT="Option to read CO2 from file  "
      CASE('MEWTH');  MSG_TEXT="Weather method                "
      CASE('MESOM');  MSG_TEXT="Soil organic matter method    "
      CASE('MELI') ;  MSG_TEXT="Light interception method     "
      CASE('MEEVP');  MSG_TEXT="Pot. evapotranspiration method"
      CASE('MEINF');  MSG_TEXT="Infiltration method           "
      CASE('MEPHO');  MSG_TEXT="Photosynthesis method         "
      CASE('MEHYD');  MSG_TEXT="Hydrology method              "
      CASE('MESEV');  MSG_TEXT="Soil evaporation method       "
      CASE('MESOL');  MSG_TEXT="Soil input and partitioning   "
      CASE('METMP');  MSG_TEXT="Soil temperature method       "
      CASE('IPLTI');  MSG_TEXT="Planting method switch        "
      CASE('IIRRI');  MSG_TEXT="Irrigation method switch      "
      CASE('IFERI');  MSG_TEXT="Fertilizer switch             "
      CASE('IRESI');  MSG_TEXT="Organic matter switch         "
      CASE('IHARI');  MSG_TEXT="Harvest simulation switch     "
      CASE('IDETO');  MSG_TEXT="Overview output switch        "
      CASE('IDETS');  MSG_TEXT="Summary output switch         "
      CASE('IDETG');  MSG_TEXT="Growth output switch          "
      CASE('IDETC');  MSG_TEXT="Carbon output switch          "
      CASE('IDETW');  MSG_TEXT="Water output switch           "
      CASE('IDETN');  MSG_TEXT="Nitrogen output switch        "
      CASE('IDETP');  MSG_TEXT="Phosphorus output switch      "
      CASE('IDETD');  MSG_TEXT="Pest & disease output switch  "
      CASE('IDETL');  MSG_TEXT="Output detail switch          "
      CASE('IDETH');  MSG_TEXT="Chemial output file switch    "
      CASE('IDETR');  MSG_TEXT="Operations output file switch "
      CASE('NSWITCH');MSG_TEXT="Nitrogen options switch       "
      CASE('NYRS')  ; MSG_TEXT="Number of years of simulation "
      CASE('YRSIM') ; MSG_TEXT="Start of simulation date      "
      CASE('MODEL') ; MSG_TEXT="Crop model                    "
      CASE('MESIC') ; MSG_TEXT="Sequence code (not used)      "
      CASE('FROP')  ; MSG_TEXT="Frequency of output code      "
      CASE DEFAULT;   MSG_TEXT="                              "
      END SELECT

      RETURN
      END FUNCTION MSG_TEXT
!=======================================================================
