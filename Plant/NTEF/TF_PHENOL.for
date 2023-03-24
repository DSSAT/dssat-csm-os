!======================================================================
!  TF_PHENOL Subroutine
!
!  Determines Phenological Stage and Growing Degree Days
!----------------------------------------------------------------------
!  Revision history
!
!                 Written
!  02/07/1993 PWW Header revision and minor changes
!  02/07/1993 PWW Added switch block, code cleanup
!  02/07/1993 PWW Modified TT calculations to reduce line #'s
!  05/  /1994 WTB Modified for MILLET model
!  03/29/2001 WDB Converted to modular format
!  12/01/2001 WDB Major restructuring for 2002 release
!  06/11/2002 GH  Modified for Y2K
!  08/12/2003 CHP Added I/O error checking
!  10/12/2005 CHP/JIL Added optional temperature sensitivity parameter
!                 to ecotype file (TSEN)
!  07/13/2006 CHP Added P model
!  06/21/2011 FSR created WH_PHENOL.for for APSIM NWheat (WHAPS) adaptation
!  01/11/2018 KEP converted WH_ sub-routines to TF_.
!  03/18/2018 KEP changed  nwheats_ppfac = 1. - PPSEN * (20-TWILEN)**2 to  ntef_ppfac = 1. - PPSEN * (TWILEN-20)**2
!             All uses of nwheats_ppfac were changed to ntef_ppfac
!  05/07/2018 KEP added if statements to ntef_ppfac routine to account for TWILEN < 1
!  01/21/2020 JG moved some CUL parameters to ECO file
!  07/24/2020 JG moved ozone parameters to ECO file
!  06/15/2022 CHP Added CropStatus
!---------------------------------------------------------------------
!  Called by : TF_APSIM
!  when DYNAMIC = RUNINIT, SEASINIT and INTEGRATE only
!----------------------------------------------------------------------
      SUBROUTINE TF_PHENOL (CONTROL, ISWITCH,
     &    FILEIO, IDETO,  CUMDEP, DAYL, DLAYR,                   !INPUT
     &    fstage, LEAFNO, LL, NLAYR, nwheats_dc_code,            !INPUT
     &    nwheats_vfac,  pl_la, plsc, PLTPOP, SDEPTH,            !INPUT
     &    sen_la, SNOW, stage_gpla, stgdur, SW,                  !INPUT
     &    TBASE,  tiln, TMAX, TMIN, TWILEN, weather,             !INPUT
     &    vd, vd1, vd2, VSEN, XN,  YRDOY, YRSIM,                 !INPUT
     &    CUMDTT, DTT, GPP, ISDATE, ISTAGE,                      !OUTPT
     &    MDATE, nwheats_kvalue, Pgdd, STGDOY,                   !OUTPT
     &    sumstgdtt, XNTI, TLNO, XSTAGE, YREMRG,                 !OUTPT
     &    KCAN, KEP, P3, TSEN, CDAY, cumph_nw, CropStatus,       !OUTPT 
     &    SeedFrac, TEMPCR, VegFrac, VREQ, xstag_nw, zstage)     !OUTPT 
C-----------------------------------------------------------------------
      USE ModuleDefs
      USE TF_module
      IMPLICIT  NONE
      EXTERNAL GETLUN, FIND, ERROR, IGNORE, WARNING, SNOWFALL, TF_COLD, 
     &  StageFlags, ntefs_germn
      SAVE
!----------------------------------------------------------------------
!                             Define Variables
!----------------------------------------------------------------------
      INTEGER         DYNAMIC

!  FSR added coefficients from WHAPS cultivar file
!     REAL        ADF1
!     REAL        ADF2
!     REAL        ADF3
      REAL        ADLAI
      REAL        ADPHO
      REAL        ADTIL
!     REAL        AFS1
!     REAL        AFS2     
      INTEGER     CropStatus
      real   dc_code(7) ! Growth stage code, similar to Zadocs scale
      REAL        DTNP1
      REAL        EXNH4
      REAL        EXNO3
!     REAL        FDSW1
!     REAL        FDSW2
!     REAL        FDSW3
      REAL        FOZ1  ! Added by JG for ozone calculation
      REAL        FREAR
      REAL        GPPES
      REAL        GPPSS
      REAL        GRNO
      REAL        INGNC
      REAL        INGWT
      real   istageno(7) ! istage input for interpolation of DC code
      INTEGER     lstage
      REAL        MNNCR
      REAL        MNNH4
      REAL        MNNO3
      REAL        MNRTN
      REAL        MXGWT
      REAL        MXNCR
      REAL        MXNUP
      REAL        NOMOB
      REAL        nwheats_vfac
      REAL        P1
      REAL        P2AF
      REAL        P3AF
      REAL        P4AF
      REAL        P5
      REAL        P5AF
      REAL        P6AF
      REAL        PHINT
      REAL        pl_la  ! Plant leaf area mm2
      REAL        PLGP1
      REAL        PLGP2
      REAL        plsc(20) !Plant leaf area array by phylochron interval
      REAL        PNUPR
      REAL        PPSEN
      REAL        VREQ
      REAL        MXFIL
      REAL        RTDP1
      REAL        RTDP2
      REAL        SFOZ1
!     REAL        SFOZ2
!     REAL        SLA
      REAL        SLAP1
      REAL        SLAP2
!     REAL        STAG1
!     REAL        STAG2
      INTEGER     stagno
      REAL        STEMN
      real        STMMX
      REAL        TC1P1
      REAL        TC1P2
      REAL        tiln
      REAL        vd
      REAL        vd1
      REAL        vd2
      REAL        VSEN
      REAL        WFNU

! FSR added variables from APSIM NWheat routines
      real        dtt
      REAL        tdif
      REAL        tt
      REAL        tcor
      real        nwheats_dc_code
      REAL        nwheats_degdy
      REAL        kvalue
      REAL        nwheats_kvalue
      real        fstage
      REAL        nwheats_pstag(11)
      real        stage_gpla

! End WHAPS/NWheat coefficients
!     REAL            ABSTRES
      REAL            ACOEF
!     REAL            BARFAC 
      CHARACTER*1     BLANK
      PARAMETER (BLANK = ' ')
      REAL            C1
      INTEGER         CDAY
      REAL            CUMDEP
      REAL            CUMDTT
!     REAL            CUMDTT_M ! to keep old maize code for comparison 
      REAL            DAYL
      REAL            DEC
      REAL            DGET
!     REAL            DJTI
      REAL            DLAYR(NL)
      REAL            DLV
      REAL            DOPT
      REAL            DSGT
      REAL            DSGFT
      REAL            DTT_M
      REAL            DUMMY
!     REAL            EARS 
      CHARACTER*5  temp_Chr
      CHARACTER*6     ECONO
      INTEGER         ERR
      CHARACTER*6     ERRKEY
      PARAMETER       (ERRKEY='TFAPS')
!*!      PARAMETER       (ERRKEY='WHPHEN')
      INTEGER         ERRNUM
      CHARACTER*12    FILEC
      CHARACTER*12    FILES
      CHARACTER*12    FILEE
      CHARACTER*92    FILEGC
      CHARACTER*30    FILEIO
      CHARACTER*78 MSG(2)
      INTEGER         FOUND
!     REAL            G2             
!     REAL            G3             
      REAL            GDDE
      REAL            GPP
      INTEGER         grf_date   !*! WHAPS: grain fill date
      INTEGER         I
      integer         II
      CHARACTER*1     IDETO
      INTEGER         IDURP
      INTEGER         ISTAGE
      CHARACTER*1     ISWWAT
      CHARACTER*1     IDETR
      REAL            KCAN
      REAL            KEP
      REAL            KVAL1  ! JG added for pre-anthesis kvalue
      REAL            KVAL2  ! JG added for post-anthesis kvalue
      INTEGER         LEAFNO
      INTEGER         L
      INTEGER         L0
      INTEGER         LINC
      REAL            LL(NL)
      INTEGER         LNUM
      INTEGER         LUNIO
      INTEGER         mat_date   !*! WHAPS: maturity date
      INTEGER         MDATE
      INTEGER         NDAS
      INTEGER         NLAYR
      INTEGER         NOUTDO
!     INTEGER         nwheats_status         
      REAL            P2O
      REAL            P3
      REAL            P9
      CHARACTER*80    PATHCR
      CHARACTER*80    PATHSR
      CHARACTER*80    PATHER
      REAL            PDTT
      REAL            pgdd(20)  ! WHAPS:  TT required for each istage
                                ! (from nwheats.for)
!*!   REAL            PHINT
      REAL            PLTPOP
!     REAL            nwheats_ppfac !WHAPS wheat model day-length factor
      REAL            ntef_ppfac !TFAPS wheat model day-length factor
      REAL            PSKER
      REAL            RATEIN
      REAL            ROPT
!     REAL            RUE
      REAL            RUE1  ! JG added for pre-anthesis RUE
      REAL            RUE2  ! JG added for post-anthesis RUE
      REAL            SDEPTH
      CHARACTER*6     SECTION
      REAL            S1
!     REAL            SI1(6)         
!     REAL            SI3(6)         
      REAL            SIND
      REAL            sen_la  ! Senesced leaf area
      REAL            SNDN
      REAL            SNOW
      integer SNOWky
      REAL            SNUP
!     INTEGER         sowmx  !Maximum sowing-to- days before plant dies
!     REAL            SRAD           
      INTEGER         STGDOY(20)
      INTEGER         stgdur(20)
      REAL            SUMDTT
!     REAL            SUMDTT_M  !to keep old maize code for comparison
      REAL            sumstgdtt(20)   ! WHAPS wheat model real array
      REAL            SUMDTT_2 !introduced for plant P routine
!     REAL            SUMP           
      REAL            SW(NL)
      REAL            SWCG
      REAL            SWSD
      !REAL            TBASE
      REAL            TDSOIL
      REAL            TEMPCN
      Real        Tthrshld, frostf, tbase, crownT !JZW add in Apr, 2014
      REAL            TEMPCR
      REAL            TEMPCX
      REAL            TH
      REAL            TLNO
      REAL            TMAX
      REAL            TMIN
      REAL RAIN, WATAVL
      REAL            TMSOIL
      REAL            TNSOIL
      REAL            TOPT
      REAL            TTOP
      REAL            TSEN  !10/12/2005 chp
      REAL            TWILEN
      CHARACTER*6     VARNO
      CHARACTER*16    VRNAME
      REAL            XN
      REAL            XNTI
!     REAL            XS           
      real   xs_nw(6)
      REAL            XSTAGE
      REAL            xstag_nw
      real   zs_nw(6)          ! Zadoks growth stage number
      REAL            zstage
      INTEGER         YRDOY
      INTEGER         YREMRG
      INTEGER         YRSIM
      INTEGER ISDATE

      INTEGER PATHL
      INTEGER LUNECO

      CHARACTER*6 ECOTYP
      INTEGER ISECT
      CHARACTER*355 C255  ! JG increased for large ecotype file
      CHARACTER*16  ECONAM
      INTEGER LUNCRP
      CHARACTER*92 FILECC
      CHARACTER*80 C80
      CHARACTER*78 MESSAGE(10)

!     CHP added for P model
      REAL SeedFrac, VegFrac

      REAL cumph_nw(11) !add by JZW
C-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
!     Type (ResidueType) SENESCE
      TYPE (WeatherType) WEATHER

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      ISWWAT  = ISWITCH % ISWWAT
      IDETR   = ISWITCH % IDETR
      DAYL    = WEATHER % DAYL
      TWILEN= WEATHER % TWILEN
      TMAX    = WEATHER % TMAX
      TMIN    = WEATHER % TMIN
      RAIN    = WEATHER % RAIN

!----------------------------------------------------------------------
!         DYNAMIC = RUNINIT OR DYNAMIC = SEASINIT
! ---------------------------------------------------------------------
      IF (DYNAMIC.EQ.RUNINIT .OR. DYNAMIC.EQ.SEASINIT) THEN

!       Do this just once in RUNINIT
        IF (DYNAMIC .EQ. RUNINIT) THEN
          CALL GETLUN('OUTO', NOUTDO)

          !-------------------------------------------------------
          !     Read input file name (ie. DSSAT45.INP) and path
          !-------------------------------------------------------
          CALL GETLUN('FILEIO', LUNIO)
          OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERR)
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)

          READ(LUNIO,50,IOSTAT=ERR) FILES, PATHSR; LNUM = 7
   50     FORMAT(//////,15X,A12,1X,A80)
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

          READ(LUNIO,51,IOSTAT=ERR) FILEE, PATHER; LNUM = LNUM + 1
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
   51     FORMAT(15X,A12,1X,A80)

          READ(LUNIO,51,IOSTAT=ERR) FILEC, PATHCR; LNUM = LNUM + 1
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

          !------------------------------------------------------
          !   Read Planting Details Section
          !------------------------------------------------------
          SECTION = '*PLANT'
          CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
          IF (FOUND .EQ. 0) THEN
            CALL ERROR(SECTION, 42, FILEIO, LNUM)
          ELSE

            READ(LUNIO,60,IOSTAT=ERR) YREMRG,PLTPOP,SDEPTH
            LNUM = LNUM + 1

 60         FORMAT(11X,I7,7X,F5.2,25X,F5.2)
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
          ENDIF
!     -----------------------------------------------------------------
!             Read crop cultivar coefficients
!     -----------------------------------------------------------------
          SECTION = '*CULTI'
          CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
          IF (FOUND .EQ. 0) THEN
            CALL ERROR(SECTION, 42, FILEIO, LNUM)
          ELSE
            READ (LUNIO,1800,IOSTAT=ERR) VARNO,VRNAME,ECONO,
     &            VSEN,PPSEN,P1,P5,PHINT,GRNO,MXFIL,
     &            STMMX,SLAP1
            LNUM = LNUM + 1
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

1800           FORMAT (A6,1X,A16,1X,A6,1X,9F6.0)
          ENDIF

      VSEN = VSEN * 0.0054545 + 0.0003
      PPSEN = PPSEN *0.002
          CLOSE(LUNIO)

!     -----------------------------------------------------------------
!              Read Species Coefficients
!     -----------------------------------------------------------------

          FILECC =  TRIM(PATHSR) // FILES
          CALL GETLUN('FILEC', LUNCRP)
          OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)
      !----------------------------------------------------------------
      !       Find and Read TEMPERATURE Section
      ! Variable in *.spe          Variable in Fortran code
      ! TTHLD                     Tthrshld
      ! FRSTF                     frostf
      ! CRWNT                     crownt
      ! SNOW                      SNOWky
      !----------------------------------------------------------------
          SECTION = '*TEMPE'
          CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
          IF (FOUND .EQ. 0) THEN
            CALL ERROR(SECTION, 42, FILECC, LNUM)
          ELSE
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            ! Temperature threshold for frost  ! TTHLD in *.spe
            READ(C80,'(7X,4(1X,F5.0))',IOSTAT=ERR) Tthrshld 
            READ(C80,'(2X, A5)')temp_Chr
            if (temp_Chr(1:5) .ne. "TTHLD") then
              ERR =1
              WRITE(MSG(1),'(A,A,A)')'Read ',temp_Chr,'instead of TTHLD'
              WRITE(MSG(2),'(A)') "Program will stop."
              CALL WARNING(2,ERRKEY,MSG)
            endif
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!           FRSTF in *.spe
            READ(C80,'(7X,4(1X,F5.0))',IOSTAT=ERR) frostf 
            READ(C80,'(2X, A5)')temp_Chr
              if (temp_Chr(1:5) .ne. "FRSTF") then
              ERR =1
              WRITE(MSG(1),'(A,A,A)')'Read ',temp_Chr,'instead of FRSTF'
              WRITE(MSG(2),'(A)') "Program will stop."
              CALL WARNING(2,ERRKEY,MSG)
            endif
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)          
           ! crown temperature
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!           CRWNT in *.spe
            READ(C80,'(7X,4(1X,F5.0))',IOSTAT=ERR) crownT 
            READ(C80,'(2X, A5)')temp_Chr
            if (temp_Chr(1:5) .ne. "CRWNT") then
              ERR =1
              WRITE(MSG(1),'(A,A,A)')'Read ',temp_Chr,'instead of CRWNT'
              WRITE(MSG(2),'(A)') "Program will stop."
              CALL WARNING(2,ERRKEY,MSG)
            endif
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!           SNOW in *.spe, switch for snow effect on frost
            READ(C80,'(9X,I3)',IOSTAT=ERR) SNOWky 
            READ(C80,'(2X, A5)')temp_Chr
            if (temp_Chr(1:4) .ne. "SNOW") then
              ERR =1
              WRITE(MSG(1),'(A,A,A)')'Read ',temp_Chr,' instead of SNOW'
              WRITE(MSG(2),'(A)') "Program will stop."
              CALL WARNING(2,ERRKEY,MSG)
            endif
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
          ENDIF
!         ----------------------------------------------------------------
!                Find and Read from SEED GROWTH Section
!         ----------------------------------------------------------------

          SECTION = '*SEED '
          CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
          IF (FOUND .EQ. 0) THEN
            CALL ERROR(SECTION, 42, FILECC, LNUM)
          ELSE

            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            READ(C80,'(9X,F7.3)',IOSTAT=ERR) DSGT
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            READ(C80,'(9X,F7.3)',IOSTAT=ERR) DGET
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            READ(C80,'(9X,F7.3)',IOSTAT=ERR) SWCG
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
          ENDIF

      REWIND(LUNCRP)

      !---------------------------------------------------------------
      !       Find and Read GROWTH STAGE (Nwheat)
      !---------------------------------------------------------------
      SECTION = '*GROWT'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(10X,7(1X,F5.1))',IOSTAT=ERR) (istageno(II),II=1,7)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(10X,7(1X,F5.1))',IOSTAT=ERR) (dc_code(II),II=1,7)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(10X,6(1X,F5.1))',IOSTAT=ERR) (xs_nw(II),II=1,6)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(10X,6(1X,F5.1))',IOSTAT=ERR) (zs_nw(II),II=1,6)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

      ENDIF
      CLOSE (LUNCRP)

!-----------------------------------------------------------------------
!     Open Ecotype File FILEE
!-----------------------------------------------------------------------
          LNUM = 0
          PATHL  = INDEX(PATHER,BLANK)
          IF (PATHL .LE. 1) THEN
            FILEGC = FILEE
          ELSE
            FILEGC = PATHER(1:(PATHL-1)) // FILEE
          ENDIF

!-----------------------------------------------------------------------
!    Read Ecotype Parameter File
!-----------------------------------------------------------------------
          CALL GETLUN('FILEE', LUNECO)
          OPEN (LUNECO,FILE = FILEGC,STATUS = 'OLD',IOSTAT=ERRNUM)
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEE,0)
          ECOTYP = '      '
          LNUM = 0
          DO WHILE (ECOTYP .NE. ECONO)
            CALL IGNORE(LUNECO, LNUM, ISECT, C255)
            IF (ISECT .EQ. 1 .AND. C255(1:1) .NE. ' ' .AND.
     &            C255(1:1) .NE. '*') THEN
              READ(C255,3100,IOSTAT=ERRNUM) ECOTYP,ECONAM,TBASE,TOPT,
     &             ROPT,TTOP, P2O,VREQ,GDDE,DSGFT,RUE1,RUE2,KVAL1,KVAL2,
     &             SLAP2,TC1P1,TC1P2,DTNP1,PLGP1,PLGP2,P2AF,P3AF,P4AF,
     &             P5AF,P6AF,ADLAI,ADTIL,ADPHO,STEMN,MXNUP,MXNCR,WFNU,
     &             PNUPR,EXNO3,MNNO3,EXNH4,MNNH4,INGWT,INGNC,FREAR,
     &             MNNCR,GPPSS,GPPES,MXGWT,MNRTN,NOMOB,RTDP1,RTDP2,
     &             FOZ1,SFOZ1
3100          FORMAT (A6,1X,A16,1X,10(1X,F5.1),2(1X,F5.2),3(1X,F5.1),
     &                1(1X,F5.3),1(1x,F5.0),11(1X,F5.2),1(1X,F5.3),
     &                1(1X,F5.2),1(1X,F5.3),5(1X,F5.2),3(1X,F5.3),
     &                2(1X,F5.2),1(1X,F5.1),1(1X,F5.2),1(1X,F5.3),
     &                2(1X,F5.0),2(1X,F5.2))
              IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEE,LNUM)

              IF (ECOTYP .EQ. ECONO) THEN
!               Read optional cold sensitivity paramter.
!               Default to TSEN = 6.0 if no value given.
!                 JG changed column numbers to match updated ecotype file
                IF (C255(315:319) == '     ') THEN
                  TSEN = 6.0
                ELSE
                  READ(C255(315:319),'(F5.0)',IOSTAT=ERRNUM) TSEN
                  IF (ERRNUM .NE. 0 .OR. TSEN < 1.E-6) TSEN = 6.0
                ENDIF

!               Read optional number of cold days paramter.
!               Default to CDAY = 15.0 if no value given.
!                 JG changed column numbers to match updated ecotype file
                IF (C255(321:325) == '     ') THEN
                  CDAY = 15
                ELSE
                  READ(C255(321:325),'(I5)',IOSTAT=ERRNUM) CDAY
                  IF (ERRNUM .NE. 0 .OR. CDAY < 0) CDAY = 15
                ENDIF

                EXIT
              ENDIF

            ELSEIF (ISECT .EQ. 0) THEN
              CALL ERROR(ERRKEY,7,FILEE,LNUM)
            ENDIF
          ENDDO

          CLOSE (LUNECO)
        ENDIF

      KEP = KCAN/(1-0.07)*(1-0.25)
!----------------------------------------------------------------------
!*!       Begin WHAPS kvalue calculation
!*!       (from APSIM NWheat real function nwheats_kvalue)
!----------------------------------------------------------------------
!*!      kvalue - extinction coefficient for light interception
!*!      kvalue = lai**2 * 3 ! kvalue will eventually be some function
!*!                         of lai. In NWheat, as here, this formula is
!*!                         overridden by the following statements:
      kvalue = 0.60
cbak  adjust k upwards during grain fill to allow for light intercepted by
cbak  ears that is not included in lai calculation.
      if (istage .eq. 5) kvalue = 0.7   !*! 5 = grnfil
      nwheats_kvalue = kvalue
!----------------------------------------------------------------------
!*!       End WHAPS kvalue calculation
!----------------------------------------------------------------------

! KEP  Energy extinction coefficient for partitioning EO to EP
! KCAN Canopy light extinction coefficient for daily PAR, for
!        equidistant plant spacing, modified when in-row and
!        between row spacings are not equal (ecotype input)
! EO   Potential evapotranspiration rate (mm/d)
! EP   Actual plant transpiration rate (mm/d)

          DO I=1,20
              STGDOY(I) = 9999999
              pgdd(I) = 0.0 ! NWheat array: TT required for each istage
              plsc(I) = 0.0
              sumstgdtt(I) = 0.0  ! NWheat array
 !*!          stgdur(I) = 0.0     ! NWheat array
          ENDDO
          ! STGDOY(14) = YRSIM  JIN Changed
          STGDOY(7) = YRSIM
          YREMRG = -99  !CHP 5/19/2011

      CUMDTT = 0.0
      SUMDTT = 0.0
      DTT_M = 0.0
      GPP = 0.0
      ISTAGE = 7  ! Fallow: No crop present to Sowing
      XSTAGE = 0.1
      MDATE      = -99
      DUMMY = 0

      ISDATE = 0
      TNSOIL = 0.0
      TMSOIL = 0.0
      TH = 00.0
      TEMPCX = 0.
      TEMPCR = 0.0
      TDSOIL = 0.0
      SWSD = 0.0
      SNUP = 0.0
      SNDN = 0.0
      S1 = 0.0
      RATEIN = 0.0
      PSKER = 0.0
      PDTT = 0.0
      P9 = 0.0
      P3 = 0.0
      NDAS = 0.0
      L0 = 0.0
      L = 0
      DLV = 0.0
      DEC = 0.0
      C1 = 0.0
      ACOEF = 0.0
      DOPT = 0.0

!     CHP 9/10/2004  P model
      SeedFrac = 0.0
      VegFrac  = 0.0
!      Initialize snow accumulation
       if (SNOWky .eq. 1) then
         CALL SNOWFALL (SEASINIT,
     &    TMAX, RAIN,                                     !Input
     &    SNOW, WATAVL)                                   !Output
        endif

!----------------------------------------------------------------------
      CALL TF_COLD (CONTROL, ISWITCH, FILEIO,              !Input
     &    istage, leafno, PLTPOP,  VREQ, tbase,            !Input
     &    tempcr, tiln, VSEN, weather,                     !Input
     &    pl_la, plsc,  Tthrshld, frostf, crownT, SNOWky,  !In/Out
     &    nwheats_vfac, sen_la, vd, vd1, vd2)              !Output
!-----------------------------------------------------------------------
      CALL StageFlags (CONTROL, FILEIO,
     &    dc_code, fstage, istage, istageno, pgdd, pl_la,     !INPUT
     &    PLTPOP, sen_la, sumstgdtt, xs_nw, zs_nw,            !INPUT
     &    lstage, nwheats_dc_code, nwheats_pstag,             !OUTPT
     &    stage_gpla, stagno, stgdur, xstag_nw, zstage)       !OUTPT
!-----------------------------------------------------------------------

!*! the following replaces Nwheats subroutine nwheats_pgdin
        !pgdd(9) = 40. + 10.2 * sdepth    ! Apsim code
      pgdd(9) = 40. + GDDE * sdepth    ! sdepth in NWheats was mm;
                                       !        here it is cm - FSR
      P9 = pgdd(9) ! GDDE is a DSSAT parameter from ECO file used to
                   ! replace NWheat hard-wired 10.2 leave P9 for DSSAT
                   ! compatibility; delete later
      pgdd(1) = P1  ! P1 from CUL file
      pgdd(2) = phint * 3.0 ! phint is from *.cul
      pgdd(3) = phint * 2.0 !
      !pgdd(2) = phint * 1.6 ! !JZW make wrong setting to test
      !pgdd(3) = phint * 3.4 !
      P3 = pgdd(3)          ! for DSSAT compatibility
      pgdd(4) = DSGFT ! was hard-wired = 200 in NWHEAT, but FSR
                      ! changed to DSSAT equivalent from ECO input file
      pgdd(5) = p5
!-----------------------------------------------------------------------
!         DYNAMIC = RATE OR INTEGRATE
! ---------------------------------------------------------------------
      ELSE    ! pass here only DYNAMIC = INTEGR
!-----------------------------------------------------------------------
!*!   Begin NWheats subroutine nwheats_crown_temp (tempcn, tempcx).

      ! Original Ceres-Wheat had snow in it - the code is still here but
      ! we set snow to zero.
      ! JZW add in May, 2014
      if (SNOWky .eq. 1) then
        CALL SNOWFALL (RATE,
     &    TMAX, RAIN,                                 !Input
     &    SNOW, WATAVL)                               !Output
      else
        snow = 0.0
      endif

!       Calculate max crown temperature, which is calculated in nwheats_crown_temp (tempcn, tempcx)
!*!   if (tempmx .lt. 0.) then
!*!      tempcx = 2.0 + tempmx * (0.4 + 0.0018 * (snow - 15.)**2)
      if (TMAX .lt. 0.) then
         tempcx = 2.0 + TMAX   * (0.4 + 0.0018 * (snow - 15.)**2)
      else
!*!      tempcx = tempmx
         tempcx = TMAX
      endif

      ! Calculate min crown temperature
!*!   if (tempmn .lt. 0.) then
!*!      tempcn = 2.0 + tempmn * (0.4 + 0.0018 * (snow - 15.)**2)
      if (TMIN .lt. 0.) then
         tempcn = 2.0 + TMIN * (0.4 + 0.0018 * (snow - 15.)**2)
      else
!*!      tempcn = tempmn
         tempcn = TMIN
      endif

!*!   End NWheats subroutine nwheats_crown_temp (tempcn, tempcx).
!-----------------------------------------------------------------------
!----------------------------------------------------------------------
!*!       Begin WHAPS thermal time calculation
!*!       (from APSIM NWheat real function nwheats_degdy)
!----------------------------------------------------------------------
! TEMPCN     Crown temperature when snow is present and TMIN < 0. This f
! TEMPCR     Crown temperature, C
! TEMPCX     Crown temperature for maximum development rate, C
! TBASE      Base temperature for development from ecotype file, C
! TOPT       Optimum temperature for development from species file, C
! Ttop       Maximum temperature for development from species file, C
      tempcr = (tempcx + tempcn)/2.0
      tdif = tempcx - tempcn
      if (tdif.eq.0.) tdif = 1.0

      if (tempcx .lt. tbase) then
         tt = 0.0
      else if (tempcx .lt. topt) then
         if (tempcn .lt. tbase) then
            ! min < base and max < opt  (max = TTOP)
            ! ------------------------
            tcor = (tempcx - tbase)/tdif
            tt = (tempcx - tbase)/2. * tcor
         else
            ! min > base and max < opt
            ! ------------------------
            tt = tempcr - tbase !JZW when I debug, stage 1 & 2 use this
         endif

      else if (tempcx .lt. TTOP) then
         ! opt<tmax<max
         if (tempcn .lt. topt) then
            ! min < opt and tmax < max  (tmax = tempcx)
            ! ------------------------
!           JZW when I debug, stage 2 use this
            tcor = (tempcx - topt)/tdif  
            tt = (topt - tbase)/2. * (1. + tcor) + tempcn/2.*(1.-tcor)
         else
            ! opt < (min and max)< max
            ! ------------------------
            tt = topt - tbase
         endif

      else ! tempcx > tmax
         if (tempcn .lt. topt) then
            ! min < opt and tmax > max
            ! ------------------------
            tcor = (tempcx - TTOP) / tdif
            tt = (topt + TTOP - tempcx) * tcor + topt * (1.-tcor)
            tcor =  (topt - tempcn)/tdif
            tt = tt * (1. - tcor) + (tempcn + topt)/2 * tcor
         else
            ! min > opt and tmax > max
            ! -----------------------
            tcor = (tempcx - TTOP) / tdif
            tt = (topt + TTOP - tempcx) * tcor + topt * (1.-tcor)
         endif
      endif

      nwheats_degdy = tt
      dtt = nwheats_degdy
      SUMDTT = SUMDTT + dtt
      CUMDTT = CUMDTT + dtt
!----------------------------------------------------------------------
!*!   END    WHAPS thermal time calculation
!----------------------------------------------------------------------
!*!   END    WHAPS code from subroutine nwheats_pgdin
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!*!       End WHAPS thermal time calculation (from APSIM NWheat)
!-----------------------------------------------------------------------
! NWheat daylength function nwheats_ppfac changes when istage .eq. 1 (emerge)
      if (istage .NE. 1) then
!         nwheats_ppfac = 1.0
          ntef_ppfac = 1.0
      endif

!-----------------------------------------------------------------------
!     OLD ISTAGE Definitions      NEW istage definitions (TF_APSIM)
!
!     7 - Sowing date             7 - fallow      No crop present to Sowing
!     8 - Germination             8 - sowing      Sowing to Germination
!     9 - Emergence               9 - germ        Germination to Emergence
!     1 - End juvenile            1 - emergence   Emergence to End of Juvenile
!     2 - Pannicle initiation     2 - endjuv      End of Juvenile to End of Vegetative growth
!     3 - End leaf growth         3 - endveg      End of Vegetative Growth to End of Ear Grow
!     4 - End pannicle growth     4 - endear      End of Ear Growth to Start of Grain Filling
!     5 - Grain fill              5 - grnfil      Start of Grain Filling to Maturity
!     6 - Maturity                6 - mature      End Gr Fil
!
! note that OLD Pannicle initiation has no direct NEW counterpart, and that NEW has "fallow".
!----------------------------------------------------------------------
!      ---------------------------------------------------------
!           ISTAGE = 7 - No crop-to-Sowing date, start calling TF_PHENOL on Planting date
!      ---------------------------------------------------------
             IF (ISTAGE .EQ. 7) THEN
!*!          IF (istage .EQ. fallow) THEN
!              STGDOY(istage) = YRDOY ! JZW This is wrong, call here on planting day
              NDAS           = 0.0   !NDAS - number of days after sowing
               ! from nwheats_phase: JZW add in Aug, 2014
              sumstgdtt(istage) = sumstgdtt(istage) + dtt
!----------------------------------------------------------------------
              CALL StageFlags (CONTROL, FILEIO,
              ! fstage is output
     &            dc_code, fstage, istage, istageno, pgdd, pl_la, !INPUT
     &            PLTPOP, sen_la, sumstgdtt, xs_nw, zs_nw,        !INPUT
     &            lstage, nwheats_dc_code, nwheats_pstag,         !OUTPT
     &            stage_gpla, stagno, stgdur, xstag_nw, zstage)   !OUTPT
!-----------------------------------------------------------------------
              ISTAGE = 8
!             JZW add this in Feb, 2015, istage=8 is sowing day, not germ day !
              STGDOY(istage) = YRDOY 
!*!           istage = germ 
              SUMDTT = 0.0
              IF (ISWWAT .EQ. 'N') RETURN

!              ---------------------------------------------------------
!               New Growth Stage Occurred Today. Initialize Some Variables
!              ---------------------------------------------------------
              CUMDEP = 0.0
              DO L = 1, NLAYR
                  CUMDEP = CUMDEP + DLAYR(L)
                  IF (SDEPTH .LT. CUMDEP) GO TO 100   ! Was EXIT
              END DO
  100         CONTINUE                                ! Sun Fix
              L0 = L               !L0 is layer that seed is in.

             RETURN
      !-----------------------------------------------------------------
      ! ISTAGE = 8 - Sowing to Germination
      ! Once water stress is introduces, include ntefs_germn below?
      !-----------------------------------------------------------------
!     8 = Sowing-to-Germination ! JZW change elseif to if on Feb 17, 2015
         ELSEIF (ISTAGE .EQ. 8) THEN   
        ! IF (ISTAGE .EQ. 8) THEN   !*! 8 = Sowing-to-Germination    
              !*! compare to nwheats_germn() code
              !*! This DSSAT code promotes seed to germination 1 DAP,  
              !*! IF seed-layer soil water is above LL
      !!*! The following IF statement delayed germination
      !!*! and helped align DTT with the original nwheat output.  FSR
      !       !---------------------------------------------------------
!       JZW move these lines from after CALL StageFlags to here in Sep, 2014 
!              if(YRDOY > STGDOY(7)+1) then !JZW istage 7 is starting on simulation day
!              if(YRDOY > STGDOY(8)) then !JZW replace this code by codes bellow to determing the Germ stage
!                ISTAGE =    9 !*! germ (germination to emergence)
            call ntefs_germn (sdepth, stgdur, SW,            !Input
     &          DLAYR*10, LL,                                    !Input
     &          ISTAGE)   !Input ISTAGE=8, & calculate output
              If (ISTAGE .eq. 9) then
                   !JZW add the following for the case of istage=9
                CALL TF_COLD (CONTROL, ISWITCH, FILEIO,        !Input
     &        istage, leafno, PLTPOP,  VREQ, tbase,            !Input
     &        tempcr, tiln, VSEN, weather,                     !Input
     &        pl_la, plsc,  Tthrshld, frostf, crownT, SNOWky,  !In/Out
     &        nwheats_vfac, sen_la, vd, vd1, vd2)              !Output
              endif
              ! from nwheats_phase: JZW add in Aug, 2014
              sumstgdtt(istage) = sumstgdtt(istage) + dtt
!----------------------------------------------------------------------
              CALL StageFlags (CONTROL, FILEIO,
     &        dc_code, fstage, istage, istageno, pgdd, pl_la,     !INPUT
     &        PLTPOP, sen_la, sumstgdtt, xs_nw, zs_nw,            !INPUT
     &        lstage, nwheats_dc_code, nwheats_pstag,             !OUTPT
     &        stage_gpla, stagno, stgdur, xstag_nw, zstage)       !OUTPT
!-----------------------------------------------------------------------
              IF (ISWWAT .NE. 'N') THEN
                  IF (SW(L0) .LE. LL(L0)) THEN
                      SWSD = (SW(L0)-LL(L0))*0.65 +
     &                    (SW(L0+1)-LL(L0+1))*0.35
                      NDAS = NDAS + 1

                      IF (NDAS .GE. DSGT) THEN
                          ISTAGE = 6 !Is this wrong?? how from 8 to 6?
                          PLTPOP = 0.00
                          GPP    = 1.0

                          WRITE(MESSAGE(1),3500)
                          CALL WARNING(1,'MZPHEN',MESSAGE)

                          IF (IDETO .EQ. 'Y') THEN
                              WRITE (NOUTDO,3500)
                          ENDIF
                          MDATE  = YRDOY
                          CropStatus = 12
                          RETURN
                      ENDIF
                 !Germinate when soil water > 0.02 cm3/cm3

                  IF (SWSD .LT. SWCG) RETURN
                  ENDIF
              ENDIF
!              ---------------------------------------------------------
!               New Growth Stage Occurred Today. Initialize Some Variables
!              ---------------------------------------------------------
!             Jin Changed, should not have two sowing days
              if (ISTAGE .GE. 9) STGDOY(ISTAGE) = YRDOY 

      !!*! The following IF statement delayed germination
      !!*! and helped align DTT with the original nwheat output.  FSR
      !       !---------------------------------------------------------
      ! JZW move these lines before CALL StageFlags in Sep 2014
      !        if(YRDOY > STGDOY(7)+1) then
      !          ISTAGE =    9 !*! germ (germination to emergence)
      !        endif
             !---------------------------------------------------------
             ! CUMDTT = Cumulative daily thermal time after germination
              CUMDTT =  0.0
              SUMDTT =  0.0

              RETURN   ! In NWheat: 40. + 10.2 * sdepth/10
                       ! NWheat sdepth is mm

!      -----------------------------------------------------------------
!       ISTAGE = 9 (Germination to Emergence) Determine Seedling Emergence Date
!      -----------------------------------------------------------------
          ELSEIF (ISTAGE .EQ. 9) THEN  !*!  9 = Germination-to-Emergence
!      -----------------------------------------------------------------
              sumstgdtt(istage) = sumstgdtt(istage) + dtt
              NDAS = NDAS + 1        !NDAS - number of days after sowing
!      -----------------------------------------------------------------
!       introduced to align DC Code with APSIM Nwheat  FSR             !
              if(sumstgdtt(istage) .ge. pgdd(istage)) then             !
                lstage = istage                                        !
                istage = 1                                      !
!               sumstgdtt(istage) = (sumstgdtt(lstage) - pgdd(lstage)) !
!     &                            *  min(nwheats_vfac, nwheats_ppfac)  !
                sumstgdtt(istage) = (sumstgdtt(lstage) - pgdd(lstage)) !
     &                            *  min(nwheats_vfac, ntef_ppfac)  !
              endif                                                    !
      !-----------------------------------------------------------------
          CALL TF_COLD (CONTROL, ISWITCH, FILEIO,                 !Input
     &    istage, leafno, PLTPOP,  VREQ, tbase,                   !Input
     &    tempcr, tiln, VSEN, weather,                            !Input
     &    pl_la, plsc, Tthrshld, frostf, crownT, SNOWky,          !I/O
     &    nwheats_vfac, sen_la, vd, vd1, vd2)                     !Outpt
      !-----------------------------------------------------------------
      !-----------------------------------------------------------------
          CALL StageFlags (CONTROL, FILEIO,
     &        dc_code, fstage, istage, istageno, pgdd, pl_la,     !INPUT
     &        PLTPOP, sen_la, sumstgdtt, xs_nw, zs_nw,            !INPUT
     &        lstage, nwheats_dc_code, nwheats_pstag,             !OUTPT
     &        stage_gpla, stagno, stgdur, xstag_nw, zstage)       !OUTPT
      !-----------------------------------------------------------------
              ! Emerge when P9 GDD's have been accumulated
              IF (YREMRG .LE. 0) THEN
	          IF (sumstgdtt(9) .LT. P9) RETURN
	        ELSE
	          IF (YRDOY .LT. YREMRG) RETURN
	        ENDIF

!               If GDD's pass DGET threshold, terminate model run
!               DGET - growing degree days between germination and emergence.
              IF (P9 .GT. DGET) THEN !*! Does not make much sense - FSR
                  ISTAGE = 6
                  PLTPOP = 0.00
                  GPP    = 1.0

                  WRITE(MESSAGE(1),1399)
                  CALL WARNING(1,'TFPHEN',MESSAGE)
                  IF (IDETO .EQ. 'Y') THEN
                      WRITE (NOUTDO,1399)
                  ENDIF
                  MDATE = YRDOY
                  CropStatus = 12
                  RETURN
              ENDIF

           !---------------------------------------------------------
           ! New Growth Stage Occurred Today. Initialize Some Variables
           !---------------------------------------------------------
              STGDOY(ISTAGE) = YRDOY
              ISTAGE = 1
              SUMDTT = SUMDTT - P9
              TLNO   = 30.0
!             STGDOY(9) !Passed back into water balance, JZW changed
              YREMRG = YRDOY 
              RETURN

      !-----------------------------------------------------------------
      !       ISTAGE = 1 - Emergence to End of Juvenile Stage
      !-----------------------------------------------------------------
         ELSEIF (ISTAGE .EQ. 1) THEN
!-----------------------------------------------------------------
!*! Begin NWheat function nwheats_ppfac: calculate day-length factor
!*!    if (istage .eq. emergence) then  ! not needed: specified above
!*!        hrlt = day_length (day_of_year, lat, -6.0)
!*!          substitute DAYL (DSSAT) for hrlt (NWheat)
!*!          alternatively, could use TWILEN (DSSAT)
!*!                  ppfac = 1. -   p1d * (20. - hrlt)**2
!             nwheats_ppfac = 1. - PPSEN * (20. - DAYL)**2
!            n Apsim: The parameter twilight is set to the angle (degrees) the geometric centre of the sun is relative to the horizon, -6 degrees for APSIM crops being Civil twilight.
!             nwheats_ppfac = 1. - PPSEN * (20. - TWILEN)**2
! Swap 20 and TWILEN to go from long to short day response
!Added if statements to account for TWILEN being less than the critical day length of 11 h
            if (TWILEN .LT. 11) then
                ntef_ppfac = 1.0
            endif
            if (TWILEN .GE. 11) then
                ntef_ppfac = 1. - PPSEN * (TWILEN-11)**2
                ntef_ppfac = MAX(ntef_ppfac, 0.0)
                ntef_ppfac = MIN(ntef_ppfac, 1.0)
            endif
!             ntef_ppfac = 1. - PPSEN * (TWILEN-11)**2
!             DSSAT and APSIM may calculate DAYL differently, thus affect DCCD slightly
!*!          nwheats_ppfac = bound (nwheats_ppfac, 0.0, 1.0)
!             nwheats_ppfac = MAX(nwheats_ppfac, 0.0)
!             nwheats_ppfac = MIN(nwheats_ppfac, 1.0)
!             ntef_ppfac = MAX(ntef_ppfac, 0.0)
!             ntef_ppfac = MIN(ntef_ppfac, 1.0)
!*!       else
!*!          nwheats_ppfac = 1.0  ! moved (search to find)
!*!       endif
!*! End NWheat function nwheats_ppfac
      !-----------------------------------------------------------------
          CALL TF_COLD (CONTROL, ISWITCH, FILEIO,                 !Input
     &    istage, leafno, PLTPOP,  VREQ, tbase,                   !Input
     &    tempcr, tiln, VSEN, weather,                            !Input
     &    pl_la, plsc, Tthrshld, frostf, crownT, SNOWky,          !I/O
     &    nwheats_vfac, sen_la, vd, vd1, vd2)                     !Outpt
      !-----------------------------------------------------------------  
            ! from nwheats_phase
        if (nwheats_vfac .gt. 1.0) then
            nwheats_vfac = 1.0
        endif
!            sumstgdtt(istage) = sumstgdtt(istage)
!     &                        + dtt * min(nwheats_vfac, nwheats_ppfac)
            sumstgdtt(istage) = sumstgdtt(istage)
     &                        + dtt * min(nwheats_vfac, ntef_ppfac)
!-----------------------------------------------------------------
              NDAS = NDAS + 1   !NDAS - number of days after sowing
      !-----------------------------------------------------------------
      ! introduced to align DC Code with APSIM Nwheat  FSR             !
              if(sumstgdtt(istage) .ge. pgdd(istage)) then             !
                lstage = istage                                        !
                istage = istage +1                              !
                sumstgdtt(istage) = (sumstgdtt(lstage) - pgdd(lstage)) !
                sumstgdtt(lstage) = pgdd(lstage)
                cumph_nw(istage) = cumph_nw(lstage)
              endif                                                    !
      !-----------------------------------------------------------------

              XSTAGE = SUMDTT/P1  !   Used to compute N demand
              ! Stage occurs when GDD threshold reached
              !Return if end of juvenile stage is not reached
      !-----------------------------------------------------------------
              CALL StageFlags (CONTROL, FILEIO,
     &        dc_code, fstage, istage, istageno, pgdd, pl_la,     !INPUT
     &        PLTPOP, sen_la, sumstgdtt, xs_nw, zs_nw,            !INPUT
     &        lstage, nwheats_dc_code, nwheats_pstag,             !OUTPT
     &        stage_gpla, stagno, stgdur, xstag_nw, zstage)       !OUTPT
      !-----------------------------------------------------------------
              IF (sumstgdtt(emergence) .LT. pgdd(emergence)) RETURN

!              ---------------------------------------------------------
!               New Growth Stage Occurred Today. Initialize Some Variables
!              ---------------------------------------------------------
              STGDOY(ISTAGE) = YRDOY
!*!           ISTAGE = 2   ! already accomplished above
              SUMDTT  = 0.0
              SIND   = 0.0

      !-----------------------------------------------------------------
      !   ISTAGE = 2 - End of Juvenile Stage to End of Vegetative growth
      !-----------------------------------------------------------------
          ELSEIF (ISTAGE .EQ. 2) THEN
              !NDAS - number of days after sowing
              NDAS = NDAS + 1
              !XSTAGE - noninteger growth stage (1-1.5)
              XSTAGE = 1.0 + 0.5*SIND !      Used to compute N demand.
              ! from nwheats_phase:
              sumstgdtt(istage) = sumstgdtt(istage) + dtt
      !-----------------------------------------------------------------
      ! introduced to align DC Code with APSIM Nwheat  FSR             !
              if(sumstgdtt(istage) .ge. pgdd(istage)) then             !
                lstage = istage                                        !
                istage = istage +1                              !
                sumstgdtt(istage) = (sumstgdtt(lstage) - pgdd(lstage)) !
                sumstgdtt(lstage) = pgdd(lstage)
!                 JZW try to add above statement as Apsim. Is it necessary to set previous stage?
                cumph_nw(istage) = cumph_nw(lstage)
              endif                                                    !
               if (YRDOY .eq. 1983145) then
!                   istage = 3 !jzw made wrong setting for test
!                sumstgdtt(istage) = dtt/2                            !
!                ! JZW try to add above statement as Apsim. Is it necessary to set previous stage?
!                cumph_nw(istage) = cumph_nw(2)
              endif
      !-----------------------------------------------------------------
          CALL StageFlags (CONTROL, FILEIO,
     &        dc_code, fstage, istage, istageno, pgdd, pl_la,     !INPUT
     &        PLTPOP, sen_la, sumstgdtt, xs_nw, zs_nw,            !INPUT
     &        lstage, nwheats_dc_code, nwheats_pstag,             !OUTPT
     &        stage_gpla, stagno, stgdur, xstag_nw, zstage)       !OUTPT
      !-----------------------------------------------------------------
!*!           PDTT = DTT_M
              IF (ISWWAT .EQ. 'N') THEN
                  DUMMY = DUMMY + 1
              ENDIF

              IF (DUMMY .EQ. 1) THEN
                  PDTT = SUMDTT - P1
              ENDIF

              !*! from NWheat
              IF (sumstgdtt(endjuv) .LT. pgdd(endjuv)) RETURN

!              ---------------------------------------------------------
!               New Growth Stage Occurred Today. Initialize Some Variables
!              ---------------------------------------------------------
              STGDOY(ISTAGE) = YRDOY
!*!           ISTAGE = 3
              XNTI   = SUMDTT/43.0
           !Next 2 lines: Change implemented at CIMMYT 1999 - JTR,US
!*!              TLNO    = SUMDTT/(PHINT*0.5)+ 5.0
!*!              P3      = ((TLNO + 0.5) * PHINT) - SUMDTT
              XNTI    = XN

!             chp 5/11/2005
              SUMDTT_2 = SUMDTT   !SUMDTT_2 = P1 + P2
              VegFrac = MAX(VegFrac,SUMDTT_2 / (SUMDTT_2 + P3 + DSGFT))

              SUMDTT  = 0.0

!             chp 9/23/2004, removed 5/11/2005
!              VegFrac = 1.0

      !-----------------------------------------------------------------
      ! ISTAGE = 3 - End of Vegetative Growth to End of Ear Growth
      !-----------------------------------------------------------------
          ELSEIF (ISTAGE .EQ. 3) THEN
              ! NDAS - number of days after sowing
              NDAS = NDAS + 1
              ! XSTAGE - noninteger growth stage (1.5-4.5)
              !    Used to compute N demand.
              XSTAGE = 1.5 + 3.0*SUMDTT/P3

              ! from nwheats_phase:
              sumstgdtt(istage) = sumstgdtt(istage) + dtt
      !-----------------------------------------------------------------
      ! introduced to align DC Code with APSIM Nwheat  FSR             !
              if(sumstgdtt(istage) .ge. pgdd(istage)) then             !
                lstage = istage                                        !
                istage = istage +1                                     !
                sumstgdtt(istage) = (sumstgdtt(lstage) - pgdd(lstage)) !
                sumstgdtt(lstage) = pgdd(lstage)
!                 JZW try to add above statement as Apsim. Is it necessary set previous stage?
!               JZW this will not need if we are not use array for cumph_nw
                cumph_nw(istage) = cumph_nw(lstage) 
              endif                                                    !
      !-----------------------------------------------------------------
          CALL StageFlags (CONTROL, FILEIO,
     &        dc_code, fstage, istage, istageno, pgdd, pl_la,     !INPUT
     &        PLTPOP, sen_la, sumstgdtt, xs_nw, zs_nw,            !INPUT
     &        lstage, nwheats_dc_code, nwheats_pstag,             !OUTPT
     &        stage_gpla, stagno, stgdur, xstag_nw, zstage)       !OUTPT
      !-----------------------------------------------------------------
              IF (sumstgdtt(endveg) .LT. pgdd(endveg)) RETURN

!              ---------------------------------------------------------
!               New Growth Stage Occurred Today. Initialize Some Variables
!              ---------------------------------------------------------
              STGDOY(ISTAGE) = YRDOY
              ISDATE = YRDOY
!*!           ISTAGE = 4
              SUMDTT = SUMDTT - P3
              IDURP  = 0

      !-----------------------------------------------------------------
      ! ISTAGE = 4 - End of Ear Growth to Start of Grain Filling
      !-----------------------------------------------------------------
          ELSEIF (ISTAGE .EQ. 4) THEN

              NDAS = NDAS + 1
              IDURP  = IDURP + 1
              ! Determine beginning of effective grain filling period fo
              !  maize.  Silking to beginning EFG is assumed to be 170 G
              XSTAGE = 4.5+5.5*SUMDTT/(P5*0.95)

              ! from nwheats_phase:
              sumstgdtt(istage) = sumstgdtt(istage) + dtt
      !-----------------------------------------------------------------
      ! introduced to align DC Code with APSIM Nwheat  FSR             !
              if(sumstgdtt(istage) .ge. pgdd(istage)) then             !
                lstage = istage                                        !
                istage = istage +1                              !
                sumstgdtt(istage) = (sumstgdtt(lstage) - pgdd(lstage)) !
                sumstgdtt(lstage) = pgdd(lstage)
                cumph_nw(istage) = cumph_nw(lstage)
              endif                                                    !
      !-----------------------------------------------------------------
          CALL StageFlags (CONTROL, FILEIO,
     &        dc_code, fstage, istage, istageno, pgdd, pl_la,     !INPUT
     &        PLTPOP, sen_la, sumstgdtt, xs_nw, zs_nw,            !INPUT
     &        lstage, nwheats_dc_code, nwheats_pstag,             !OUTPT
     &        stage_gpla, stagno, stgdur, xstag_nw, zstage)       !OUTPT
      !-----------------------------------------------------------------
!     CHP 5/25/2007 Move inflection point back to end of stage 3
              SeedFrac = SUMDTT / P5

              ! from nwheats_phase:
              IF (sumstgdtt(endear) .LT. pgdd(endear)) RETURN

              STGDOY(ISTAGE) = YRDOY
!*!           ISTAGE = 5
              grf_date = YRDOY  !*! from NWheat

      !-----------------------------------------------------------------
      ! ISTAGE = 5 - Start of Grain Filling to Maturity (End grain fill)
      !-----------------------------------------------------------------

          ELSEIF (ISTAGE .EQ. 5) THEN
cbak  adjust k upwards during grain fill to allow for light intercepted by
cbak  ears that is not included in lai calculation.
              if (istage .eq. 5) kvalue = 0.7   !*! 5 = grnfil
              nwheats_kvalue = kvalue

              NDAS = NDAS + 1
              XSTAGE = 4.5 + 5.5*SUMDTT/P5

              ! from nwheats_phase:
              sumstgdtt(istage) = sumstgdtt(istage) + dtt
      !-----------------------------------------------------------------
      ! introduced to align DC Code with APSIM Nwheat  FSR             !
              if(sumstgdtt(istage) .ge. pgdd(istage)) then             !
                lstage = istage                                        !
                istage = istage +1                                     !
                sumstgdtt(istage) = (sumstgdtt(lstage) - pgdd(lstage)) !
                sumstgdtt(lstage) = pgdd(lstage)
                cumph_nw(istage) = cumph_nw(lstage)
              endif                                                    !
      !-----------------------------------------------------------------
          CALL StageFlags (CONTROL, FILEIO,
     &        dc_code, fstage, istage, istageno, pgdd, pl_la,     !INPUT
     &        PLTPOP, sen_la, sumstgdtt, xs_nw, zs_nw,            !INPUT
     &        lstage, nwheats_dc_code, nwheats_pstag,             !OUTPT
     &        stage_gpla, stagno, stgdur, xstag_nw, zstage)       !OUTPT
      !-----------------------------------------------------------------

              IF (sumstgdtt(grnfil) .LT. pgdd(grnfil)) RETURN

              STGDOY (ISTAGE) = YRDOY
!*!           ISTAGE = 6
              mat_date = YRDOY  !*! from NWheat

      !-----------------------------------------------------------------
      !       ISTAGE = 6 - End Effective Grain Filling
      !-----------------------------------------------------------------
          ELSEIF (ISTAGE .EQ. 6) THEN

              ! from nwheats_phase:
              sumstgdtt(istage) = sumstgdtt(istage) + dtt
      !-----------------------------------------------------------------
          CALL StageFlags (CONTROL, FILEIO,
     &        dc_code, fstage, istage, istageno, pgdd, pl_la,     !INPUT
     &        PLTPOP, sen_la, sumstgdtt, xs_nw, zs_nw,            !INPUT
     &        lstage, nwheats_dc_code, nwheats_pstag,             !OUTPT
     &        stage_gpla, stagno, stgdur, xstag_nw, zstage)       !OUTPT
      !-----------------------------------------------------------------

  !**!            IF (SUMDTT .LT. P5)  RETURN
!              ---------------------------------------------------------
!               New Growth Stage Occurred Today. Initialize Some Variables
!              ---------------------------------------------------------
              STGDOY(ISTAGE) = YRDOY
              MDATE          = YRDOY
              ISTAGE = 7  !*! was 10  !CHP - Prevents growth parameters 
!                           from being set back to initial values.  08/11/03
              CUMDTT = 0.0

              IF (PLTPOP .NE. 0.0) THEN
                  IF (GPP .LE. 0.0) THEN
                      GPP = 1.0
                  ENDIF
              ENDIF

!             chp 5/11/2005
!*!           SeedFrac = 1.0

! ----------------------------------------------------------------------
          ENDIF            ! End ISTAGE Loop
! ----------------------------------------------------------------------

      ENDIF  ! End DYNAMIC STRUCTURE

      RETURN

!-----------------------------------------------------------------------
!     Format Strings
!-----------------------------------------------------------------------

1399  FORMAT (10X,'Seed ran out of metabolite due to deep planting')
3500  FORMAT ('Crop failure because of lack of germination ',
     &           'within 15 days of sowing')

!----------------------------------------------------------------------
      END SUBROUTINE TF_PHENOL

!=======================================================================
!  StageFlags, Subroutine
!
!  Sets several stage flages used or output by the NWheat model:
!       nwheats_status, istage, stagno, lstage, xstage, zstage,
!       fstage, dc_code.
!-----------------------------------------------------------------------
!  Revision history
!  09-15-2011 FSR Created
!-----------------------------------------------------------------------
!  Called by: TF_PHENOL
!=======================================================================
*      SUBROUTINE StageFlags (CONTROL, ISWITCH, FILEIO,
*      ! fstage is output
*     &    dc_code, fstage, istage, istageno, pgdd, pl_la,         !INPUT
*     &    plsc, PLTPOP, sen_la, sumstgdtt, xs_nw, zs_nw,          !INPUT
*     &    lstage, nwheats_dc_code, nwheats_pstag,                 !OUTPT
*     &    stage_gpla, stagno, stgdur, xstag_nw, zstage)           !OUTPT
*      !JZW stage_gpla can be removed
*!-----------------------------------------------------------------------
*      USE ModuleDefs
*      USE TF_module
*      IMPLICIT NONE
*      SAVE
*!-----------------------------------------------------------------------
*!                             Define Variables
*!-----------------------------------------------------------------------
*      INTEGER    DYNAMIC, I, istage, lstage, stagno
*      INTEGER    ERR
*      INTEGER    ERRNUM
*      INTEGER    FOUND
*      INTEGER    II
*      INTEGER    ISECT
*      INTEGER    LNUM
*      INTEGER    LUNCRP
*      INTEGER    mxstag
*      PARAMETER (mxstag = 9)
*      INTEGER   nwheats_status
*      INTEGER   sowmx  !Maximum sowing-to- days before plant dies
*      INTEGER   stgdur(20)
*      Integer   YRDOY
*      CHARACTER*80 C80
*      CHARACTER*6  ERRKEY
*      PARAMETER    (ERRKEY='MZPHEN')
*!*!      PARAMETER    (ERRKEY='TFPHEN')
*      CHARACTER*92 FILECC
*      CHARACTER*30 FILEIO
*      CHARACTER*12 FILES
*      CHARACTER*80 PATHSR
*      CHARACTER*6  SECTION
*
*      REAL   cumph_nw(11)
*      real   dc_code(7) ! Growth stage code, similar to Zadocs scale
*      real   istageno(7) ! istage input for interpolation of DC code
*      real   nwheats_dc_code
*      REAL   nwheats_pstag(11)
*      real   p_adf(3)
*      real   p_afs(2)
*      real   p_fdsw(3)
*      real   p_stage(2)
*      real   pl_la
*      REAL   pstag
*      REAL   pgdd(20)
*      REAL   plsc(20)    ! mxleaf
*      REAL   PLTPOP
*      real   rootfr(9)
*      REAL   sen_la
*      real   stage_gpla
*      real   sumstgdtt(20)   ! WHAPS wheat model real array
*      REAL   ALIN            !TABEX
*      real   xs_nw(6)  ! xstage input for interpolating Zadoks stage
*      real   xstag_nw
*      real   zstage
*
**+  Local Variables
*      real   fstage         ! stage function (0-1)
*      real   stime          ! total growing deg days for current stage/s
*      real   ttime          ! growing deg days for current stage/s.
*      real   xstgmn(mxstag) ! value at beginning of a stage
*      real   xstgmx(mxstag) ! maximum value at end of a stage
*      real   zs_nw(6)          ! Zadoks growth stage number
*!----------------------------------------------------------------------
*!     Define constructed variable types based on definitions in
*!     ModuleDefs.for.
*      TYPE (ControlType) CONTROL
*      TYPE (SwitchType)  ISWITCH
*!----------------------------------------------------------------------
*
**+  Initial Data Values
*      save       xstgmn
*      save       xstgmx
**
*      data   xstgmn(emergence) / 1.0/  ,xstgmx(emergence) / 2.0/
*      data   xstgmn(endjuv)    / 2.0/  ,xstgmx(endjuv)    / 3.0/
*      data   xstgmn(endveg)    / 3.0/  ,xstgmx(endveg)    / 4.0/
*      data   xstgmn(endear)    / 4.0/  ,xstgmx(endear)    / 5.0/
*      data   xstgmn(grnfil)    / 5.0/  ,xstgmx(grnfil)    / 6.0/
*      data   xstgmn(mature)    / 0.0/  ,xstgmx(mature)    / 0.0/
*      data   xstgmn(fallow)    / 8.0/  ,xstgmx(fallow)    / 8.0/
*      data   xstgmn(sowing)    / 9.0/  ,xstgmx(sowing)    / 9.0/
*      data   xstgmn(germ)      / 9.0/  ,xstgmx(germ)      /10.0/
*!----------------------------------------------------------------------
*!     Transfer values from constructed data types into local variables.
*      DYNAMIC = CONTROL % DYNAMIC
*      FILEIO  = CONTROL % FILEIO
*      YRDOY   = CONTROL % YRDOY
*!-----------------------------------------------------------------------
*      IF (DYNAMIC.EQ.RUNINIT .OR. DYNAMIC.EQ.SEASINIT) THEN
*!     Do this just once in RUNINIT
*!     Initialize: variables and arrays used here that are not initialized above
*
*          DO I=1,20
*              stgdur(I) = 0.0     ! NWheat array
*          ENDDO
*
*          DO I=1,11
*              nwheats_pstag(I) = 0.0
*          ENDDO
*
*      lstage = 0
*      pstag  = 0.0
*      sowmx = 40  !*! move to input file
*      stagno = 0  !*! temporary testing
*!*!      stagno = 11
*      xstag_nw = 0.0
*      zstage = 0.0
*
*      ELSE    !  DYNAMIC = anything except RUNINIT OR SEASINIT   FSR
*
*!-----------------------------------------------------------------------
*!*! These statements added by FSR to update stagno daily:
*          stagno = istage
*          IF(stagno .gt. 0) THEN
*!-----------------------------------------------------------------------
*!*! following code is nwheats function nwheats_pstag:
*!*!       pstag = divide (sumdtt(stagno), pgdd(stagno), 0.0)
*!*!       nwheats_pstag = l_bound (pstag, 0.0)  ! original nwheat code
*              if (pgdd(stagno) .GT. 0.) then !JZW add this if statement
*                 pstag = sumstgdtt(stagno) / pgdd(stagno)
*              else
*                 pstag = 0.
*              endif
*              nwheats_pstag(stagno) = MAX(pstag, 0.0)
*              if (YRDOY .ge. 1983130) then
*                  continue
*              endif
*!-----------------------------------------------------------------------
*!*!   code from subroutine nwheats_phase (partial):
*
*!*!   call ntefs_germn (stagno)
*!*!   call nwheats_cfail (stagno)
*                    lstage = stagno !JZW is this statement is necessary?
*          if (nwheats_pstag (stagno).ge.1.0) then
*
*             if (stagno.eq.mxstag) then  !*! i.e., .eq. 9, or "germ"
*                stagno = 1
*             else
*                stagno = stagno + 1
*                !JZW never go here, because these codes already excute before call StageFlags
*             endif
*
*             sumstgdtt(stagno) = sumstgdtt(lstage) - pgdd(lstage) ! JZW never go here, these statement are repeated in call routing
*             sumstgdtt(lstage) = pgdd(lstage)
*          else
*          endif
*
*          if (stgdur(stagno).le.1) then
*             cumph_nw(stagno) = cumph_nw(lstage)
*             ! JZW move above statement to calling routine because stagno is always equal lstage in this subroutine
*             ! cumph_nw is not passsed to this subroutine
*             stage_gpla = pl_la - sen_la
*          else
*          endif
*      !*! Appears to be a daily count:
*          stgdur(stagno) = stgdur(stagno) + 1
*       !*! in Apsim there the following codes
*       !*! call nwheats_phase (istage)
*       !*! subroutine nwheats_phase (stagno)
*       !*! thus stagno is returned as istage in APSIM, thus JZW add the following code on Aug 26, 2014
*          Istage = stagno
*!----------------------------------------------------------------------
*    !*! introduce nwheats_cfail code here
*
*          if (      stgdur(8).ge.sowmx  ! 8 = sowing
*     &    .and. stagno.eq.8) then
*             stagno = 7 ! 7 = fallow
*
*             nwheats_status = 10
*
*!*!         write (string, '(4a)')   !*! introduce later
*!*!     &                 module_name
*!*!     &                ,' crop failure because of lack of'
*!*!     &                ,new_line
*!*!     &                ,'         germination within 40 days of sowing'
*!*!         call Report_date_and_event (day_of_year, year, string)
*
*!*!   elseif (plants .lt. 5 .and. istage .lt. mature) then
*          elseif (PLTPOP .lt. 5 .and. istage .lt. 6) then !JZW is it reasonable for PLTPOP<5?
*             stagno = 7 ! 7 = fallow
*cbak Need to set crop status to absent so that variables are zeroed
*cbak (this is important if a crop fails to germinate in a long term run
*cbak to crop maturity
*             nwheats_status = 10
*
*!*!         write (string, '(4a)')
*!*!     :                 module_name
*!*!     :                ,' crop failure because of low stand'
*!*!         call Report_date_and_event (day_of_year, year, string)
*
*!*!       else
*          endif
*
*!----------------------------------------------------------------------
*          ENDIF ! IF(stagno .gt. 0.0)
*!----------------------------------------------------------------------
*!*! BEGIN calculations from NWheats subroutine nwheats_set_xstag
*      ttime = sumstgdtt(istage)
*      stime = pgdd(istage)
*      if (stime .gt. 0.) then  !JZW add this to avoid divide zero
*         fstage = ttime / stime
*      endif
*
*!*! The following subroutine probably sends a message when fstage
*!*! is "out of bounds".  I have not reproduced that here.  FSR
*!*!   call bound_check_real_var (fstage, 0.0, 1.0, 'fstage')
*
*!*!   fstage = bound (fstage, 0.0, 1.0)
*      fstage = MAX(fstage, 0.0)
*      fstage = MIN(fstage, 1.0)
*
*      xstag_nw = xstgmn(istage)
*     &         + (xstgmx(istage) - xstgmn(istage)) *fstage
*
*      if (istage .eq. 8) then
*         if (xstag_nw.gt.10) xstag_nw = 1.
*      else
*      endif
*!*! END calculations from NWheats subroutine nwheats_set_xstag
*!----------------------------------------------------------------------
*!*! BEGIN calculations from NWheats subroutine nwheats_set_zstag
*      if (xstag_nw .ge. 1. .and. xstag_nw .le. 6.0) then
*!*!      zstage = linear_interp_real (xstage,xs,zs,6) ! original NWheat
*         zstage = ALIN (xs_nw,zs_nw,6,xstag_nw)
*      else
*         zstage = 0.0
*
*      endif
*!*! END calculations from NWheats subroutine nwheats_set_zstag
*!----------------------------------------------------------------------
*!*! BEGIN calculations from NWheats real function nwheats_dc_code
*      fstage = 0.0 !default for zero divide
*      if (istage.eq.9 .and. pgdd(istage) .gt. 0.0 ) then ! 9=germination
*!*!      fstage = divide (sumdtt(istage), pgdd(istage), 0.0)
*         fstage = sumstgdtt(istage) / pgdd(istage)
*
*!*!      call bound_check_real_var (fstage, 0.0, 1.0, 'fstage')
*         fstage = MAX(fstage, 0.0)
*         fstage = MIN(fstage, 1.0)
*
*!*!      nwheats_dc_code = bound (fstage, 0.0, 1.0) * dc_code(1)
*         nwheats_dc_code =        fstage            * dc_code(1)
*
*      elseif ((istage.ge.1).and.(istage.le.6)) then
*
*         ttime = sumstgdtt(istage)
*         stime = pgdd(istage)
*         if (pgdd(istage) > 0.001) then
*           fstage = ttime / stime ! JZW this is repeated calculate fstage from above
*         endif
*
*!*!      call bound_check_real_var (fstage, 0.0, 1.0, 'fstage')
*
*         if (istage.eq.6) then
*            fstage = 6.
*         else
*            fstage = MAX(fstage, 0.0)
*            fstage = MIN(fstage, 1.0)
*            fstage = fstage + real(istage)
*         endif
*
*!*!      nwheats_dc_code = linear_interp_real
*!*!  :                     (
*!*!  :                      fstage
*!*!  :                     ,istageno
*!*!  :                     ,dc_code
*!*!  :                     , 7
*!*!  :                     )
*         nwheats_dc_code = ALIN (istageno,dc_code,7,fstage)
*      else
*         nwheats_dc_code = 0.0
*      endif
*
*!*! END calculations from NWheats real function nwheats_dc_code
*!----------------------------------------------------------------------
*      ENDIF ! DYNAMIC.EQ.RUNINIT .OR. DYNAMIC.EQ.SEASINIT
*      RETURN
*      END SUBROUTINE StageFlags
!=======================================================================

C=====================================================================
!     TF_PHENOL VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! DYNAMIC    Modular control
! ABSTRES    Maximum of water stress stage 1 and 3
! ACOEF      Coefficient
! BARFAC     Factor to reduce ears/plant under high populations (barrenn
! C1         Used to comptue daylength (computed in maize.for)
! CUMDEP     Cumulative depth of soil, cm
! CUMDTT     Cumulative daily thermal time after germination, C
! DAYL       Daylength, hours
! DEC        Used to compute daylength
! DGET       Threshold defined as growing degree days between germination and emergence.
!            If this threshold is exceeded, crop failure ocurrs.
! DJTI       Minimum days from end of juvenile stage to tassel initiation if the cultivar
!            is not photoperiod sensitive, DJTI
! DLAYR(L)   Soil thickness in layer L (cm)
! DLV        Used to compute daylength
! DOPT       Development optimum temperature
! DSGFT      GDD from silking to effective grain filling period, C
! DSGT       Maximum number of days from sowing to germination before crop failure occurs.
! dtt        NWheats growing degree days calculation
! DTT_M      Growing degree days today, C - from DSSAT Maize
! DUMMY      Temporary variable
! EARS       Ears per m2, computed here and used in grosub.
! ECONO      Ecotype number for the variety (not really used in maize ye
! ERR        Determines if error in reading file (0=ok, 1=error)
! ERRKEY     Variable containing routine where error occurred
! (ERRKEY='TF_PHENL')
! FILEC      Filename of .SPE or species file
! FILEIO     Filename containing model inputs (IBSNAT35.INP)
! FOUND      Indicates if a section in a file is found
! G2         Potential kernel number, kernels/plant
! G3         Potential kernel growth rate mg/kernel/day
! GDDE       Growing degree days per cm seed depth required for emergence, GDD/cm
! GPP        Grain number per plant, grains/plant
! grpp       Grain number per plant, grains/plant (NWheat)
! GRNO       Coefficient of kernel number per stem weight at the beginning of
!            grain filling [kernels (g stem)-1]   (NWheat)
! I          Loop counter
! IDETO      Screen output switch (Y/N)
! IDURP      Duration of ISTAGE 4, calendar days
! ISTAGE     Growth stage
! ISWWAT     Water balance switch (Y/N)
! LEAFNO     Number of oldest leaf per plant (same as XN)
! L          Loop counter
! L0         Temporary soil layer number
! LINC       Indicates if a line is a good line
! LL(NL)     Soil water lower limit, cm3/cm3
! LNUM       Line number in an input file
! lstage     Flag for previous (last) stage (NWheat)
! LUNIO      Logical input number for model input file
! LUNIO      Assign value to LUNIO for local use.
! MDATE      Year and day of year of maturity
! NDAS       Number of days after sowing
! NLAYR      Number of soil layers
! NOUTDO     Output file number
! P1         GDD from seedling emergence to end of juvenile phase, C
! P2         Photoperiod sensitivity coefficient, 1/hr
! P2O        Minimum daylength below which daylength does not affect dev
! P3         Cumulative GDD required to complete ISTAGE 3, C
! P5         GDD from silking to physiological maturity, C
! P9         Growing degree days from germination to emergence, C
! PATHCR     Pathname of species file
! pgdd(stagno)  Cumulative growing degree days required for each phenological stage (NWheat)
! PHINT      Phyllochron interval. Number of GDD required for new leaf emergence
! pl_la      Total leaf area per plant (green and senesced) (NWheat)
! plsc(mxleaf)  Accumulates leaf area in an array for each phylochron interval (NWheat)
! PLTPOP     Plant population, no./m2
! PSKER      Average rate of photosynthesis during ISTAGE 4
! pstag      Intermediate variable which is corrected to nwheats_pstag (NWheat)
! RATEIN     Rate of floral induction
! ROPT       Second optimum temperature for development from species fil
! SDEPTH     Sowing depth, cm
! SECTION    Temporary variable used to identify section in a file
! sen_la     Senesced leaf area for entire plant   (NWheat)
! S1         Used to compute daylength (computed in maize.for)
! SI1(6)     Water stress during a growth stage used for output
! SI3(6)     Water stress during a growth stage used for output
! SIND       Summed photoperiod induction rate
! SNDN       Sun down
! SNOW       Snow, mm
! SNUP       Sun up
! SRAD       Daily solar radiation, MJ/m2/day
! stage_gpla Green leaf area per plant ??  (NWheat)
! STGDOY(20) Year and day of year that a growth stage occurred on
! stgdur(20) ? Number of days accum in a growth stage as expressed by stgdur(istage)
! SUMDTT     Sum of GDD for a given stage, C
! sumstgdtt(20) Sum of GDD for a given stage, deg days
!              (substituted for sumdtt from nwheats.for, since DSSAT uses SUMDTT already)
! SUMP       Cumulative plant growth during ISTAGE 4, g/plant
! SW(NL)     Soil water content in layer, cm3/cm3
! SWCG       Minimum soil water available required for germination to occur, cm3/cm3
! SWSD       Modified soil water content for computing emergence
! TBASE      Base temperature for development from ecotype file, C
! TDSOIL     Weighted average soil temperature, C
! TEMPCN     Crown temperature when snow is present and TMIN < 0. This f
!            computes crown temperature as higher than TMIN, C.
! TEMPCR     Crown temperature, C
! TEMPCX     Crown temperature for maximum development rate, C
! TH         Intermedate variable for computing GDD today, C
! TLNO       Total leaf numbers that will eventually develop
! TMAX       Daily maximum temperature, C
! TMIN       Daily minimum temperature, C
! TMSOIL     Weighted average soil temperature, C
! TNSOIL     Weighted average soil temperture, C
! TOPT       Optimum temperature for development from species file, C
! Ttop       Maximum temperature for development from species file, C
! TWILEN     Twilight definition of daylength
! VARNO      Variety identification number
! VegFrac    Fraction of time completed between emergence and tassel initiation,
! VRNAME     Variety name
! VSEN       CULTIVAR parameter: sensitivity to vernalisation  (scale 1-5; NWheat)
! WTHADJ(2,8)Note, used here, but not passed into maize.for from cropgro
! WMODB*1    Note, used here, but not passed into maize.for from cropgro
! XLAT       Latitude
! XN         Number of oldest expanding leaf
! XNTI       Number of leaves at tassel initiation (used in grosub)
! XS         Temporary snow depth variable
! XSTAGE     Non-integer growth stage indicator
! xstag_nw   Same as XSTAGE, but NWHEATS version.
! YRDOY      Year and day of year
! YREMRG     Year and day of year of emergence (passed back to water bal
! YRSIM      Year and day of year of first day of simulation

