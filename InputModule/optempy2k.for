C=======================================================================
C  OPTEMPY2K, Subroutine
C
C  Determines experiment and treatment selection
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1990 GH  Written
C  05/28/1993 PWW Header revision and minor changes
C  06/07/2002 GH  Modifed for Y2K Output
!  10/29/2004 CHP/PST Added new sorghum cultivar coefficients (optional)
!  04/07/2005 CHP Added EXCA, exchangable calcium (cmol/kg)
!  06/15/2005 CHP Modified output for sequenced runs (no soils output,
!                 except for 1st run of sequence)
!  12/14/2005 CHP/PST Added new sorghum cultivar coefficients (optional)
!  02/22/2006 GH  Fix format for depth of chemical applications
!  07/26/2006 CHP Added previous management code for lookup in
!       SOMFR045.SDA file to FIELDS section
!  08/25/2006 CHP Added FILEX method codes MESOL, MESEV, METMP
!                 MESOL = alternate soil layer distribution
!                 MESEV = soil evaporation method (S=Sulieman (default),
!                                                  R=Ritchie (old))
!                 METMP = soil temperature options
!  02/05/2007 CHP Reverse location of MESEV and METMP in FILEX
!  02/06/2007 CHP Added alternate sugarcane parameters for CASUPRO
!  04/21/2007 GH  Modified sorghum cultivar coefficients
!  11/26/2007 CHP THRESH, SDPRO, SDLIP moved from eco to cul file
!  01/16/2007 GH  Modified sorghum cultivar coefficients
!  04/28/2008 CHP Added switch for CO2 from file (ICO2)
!  12/09/2008 CHP Remove METMP
!  08/03/2009 FSR Added numerous variables for CASUPRO
!  06/30/2010 FSR Added PLF2 variable for CASUPRO
!  05/19/2011 GH  Updated for sorghum
!  08/09/2012 GH  Updated cassava model
!  09/01/2011 CHP Added van Genuchten parameters for ORYZA
!  11/14/2012 GH  Add READWRITE for temp file
!  04/16/2013 CHP/KAD Added SALUS model
!  Apr-May 2015 KJB added G0 and G5 for pearl millet
!  05/09/2013 CHP/FR/JZW Added N-wheat module
!  03/20/2018 Created new FORMAT to read in CUL file for tef
!  01/21/2020 JG moved some CUL parameters to ECO file
C-----------------------------------------------------------------------
C  INPUT  : YRIC,PRCROP,WRESR,WRESND,EFINOC,EFNFIX,SWINIT,INH4,INO3,
C           TOTN,NYRS,VARNO,VRNAME,CROP,MODEL,PATHMO,ECONO,FROP,RUN,FILEIO
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : INPUT
C
C  Calls  : ERROR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE OPTEMPY2K (RNMODE, FILEX,PATHEX,
     & YRIC,PRCROP,WRESR,WRESND,EFINOC,EFNFIX,SWINIT,INH4,INO3,
     & NYRS,VARNO,VRNAME,CROP,MODEL,RUN,FILEIO,EXPN,ECONO,FROP,TRTALL,
     & TRTN,CHEXTR,NFORC,PLTFOR,NDOF,PMTYPE,ISENS,PMWD)

      USE ModuleDefs
      IMPLICIT NONE
      EXTERNAL ERROR

      INCLUDE 'COMIBS.blk'
      INCLUDE 'COMSOI.blk'
      INCLUDE 'COMSWI.blk'
      INCLUDE 'COMGEN.blk'

      CHARACTER*1  RNMODE
      CHARACTER*2  CROP,PRCROP
      CHARACTER*6  VARNO,ECONO
      CHARACTER*9  ERRKEY
      CHARACTER*8  MODEL
      CHARACTER*12 FILEX
      CHARACTER*16 VRNAME
      CHARACTER*30 FILEIO
      CHARACTER*42 CHEXTR(NAPPL)
      CHARACTER*75 FMT
      CHARACTER*80 PATHEX

      INTEGER NYRS,RUN,I,EXPN,LUNIO,LINIO,ERRNUM,FROP,YRIC,TRTALL
      INTEGER TRTN,NFORC,NDOF,PMTYPE,ISENS

      REAL    PLTFOR, PMWD
      REAL    SWINIT(NL),WRESR,WRESND,EFINOC,EFNFIX,INO3(NL),INH4(NL)

      PARAMETER (LUNIO = 21)
      PARAMETER (ERRKEY = 'OPTEMPY2K')
      LINIO  = 0

C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      OPEN (LUNIO, FILE = FILEIO,STATUS = 'UNKNOWN',IOSTAT=ERRNUM,
     &      ACTION = 'READWRITE')
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,0)
C-----------------------------------------------------------------------
C     Write temp. required variables on top of file
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,2000) RNMODE,RUN,EXPN,TRTN,TRTALL,ISENS
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'*FILES              '
      LINIO = LINIO + 1
      WRITE (LUNIO,2040,IOSTAT=ERRNUM) MODEL
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,2050,IOSTAT=ERRNUM) FILEX,PATHEX
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,2100,IOSTAT=ERRNUM) FILEA,PATHEX
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,2200,IOSTAT=ERRNUM) FILET,PATHEX
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,2300,IOSTAT=ERRNUM) FILEC,PATHCR
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,2400,IOSTAT=ERRNUM) FILEE,PATHEC
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,2500,IOSTAT=ERRNUM) FILEG,PATHGE
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,2600,IOSTAT=ERRNUM) FILEP,PATHPE
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,2700,IOSTAT=ERRNUM) FILES,PATHSL
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      IF (MEWTH .EQ. 'M' .OR. RNMODE .EQ. 'Y') THEN
        LINIO = LINIO + 1
        WRITE (LUNIO,'(A8,7X,A12,1X,A80)',IOSTAT=ERRNUM) 
     &     'WEATHERW',FILEW,PATHWTW
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
      ENDIF
      IF (MEWTH .EQ. 'S' .OR. MEWTH .EQ. 'W') THEN
        LINIO = LINIO + 1
        WRITE (LUNIO,'(A8,7X,A12,1X,A80)',IOSTAT=ERRNUM) 
     &     'WEATHERC',FILEWC,PATHWTC
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
      ENDIF
      IF (MEWTH .EQ. 'G') THEN
        LINIO = LINIO + 1
        WRITE (LUNIO,'(A8,7X,A12,1X,A80)',IOSTAT=ERRNUM) 
     &     'WEATHERG',FILEWG,PATHWTG
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
      ENDIF
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,2900,IOSTAT=ERRNUM) OUTO(1:8)
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'*SIMULATION CONTROL '
      LINIO = LINIO + 1
      WRITE (LUNIO,900,IOSTAT=ERRNUM) NYRS,NREPSQ,ISIMI,YRSIM,RSEED1,
     &       TITSIM
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,910,IOSTAT=ERRNUM) ISWWAT,ISWNIT,ISWSYM,ISWPHO,
     &       ISWPOT,ISWDIS,ISWCHE,ISWTIL, ICO2
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,915,IOSTAT=ERRNUM) MEWTH,MESIC,MELI,MEEVP,
     & MEINF,MEPHO,MEHYD,NSWITCH,MESOM, MESEV, MESOL, METMP, MEGHG
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,910,IOSTAT=ERRNUM) IPLTI,IIRRI,IFERI,IRESI,IHARI
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,920,IOSTAT=ERRNUM) IOX,IDETO,IDETS,FROP,IDETG,IDETC,
     &       IDETW,IDETN,IDETP,IDETD,IDETL,IDETH,IDETR
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'!AUTOMATIC MANAGEM  '
      LINIO = LINIO + 1
      WRITE (LUNIO,930,IOSTAT=ERRNUM) PWDINF,PWDINL,SWPLTL,SWPLTH,
     &       SWPLTD,PTX,PTTN
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,940,IOSTAT=ERRNUM) DSOIL,THETAC,IEPT,IOFF,IAME,
     &       AIRAMT,EFFIRR
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,940,IOSTAT=ERRNUM) DSOILN,SOILNC,SOILNX,NCODE,NEND
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,950,IOSTAT=ERRNUM) RIP,NRESDL,DRESMG
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,930,IOSTAT=ERRNUM) HDLAY,HLATE,HPP,HRP
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'*EXP.DETAILS        '
      LINIO = LINIO + 1
      WRITE (LUNIO,50,IOSTAT=ERRNUM)EXPN,EXPER,CG,ENAME
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'*TREATMENTS         '
      LINIO = LINIO + 1
      WRITE (LUNIO,55,IOSTAT=ERRNUM) TRTNO,ROTNO,ROTOPT,CRPNO,TITLER
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'*CULTIVARS          '
      LINIO = LINIO + 1
      WRITE (LUNIO,56,IOSTAT=ERRNUM) CROP,VARNO,VRNAME
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'*FIELDS             '
      LINIO = LINIO + 1
!     2023-07-14 chp changed order of PMALB and PMWD variables to allow 
!                    1D and 2D models to use the same file format.
      WRITE (LUNIO,59,IOSTAT=ERRNUM) FLDNAM,FILEW(1:8),SLOPE,FLOB,DFDRN,
     &       FLDD,SFDRN,FLST,SLTX,SLDP,SLNO,PMALB,PMWD
   59 FORMAT (3X,A8,1X,A8,1X,F5.1,1X,F5.0,1X,A5,1X,F5.0,1X,F5.1,
     &        2(1X,A5),1X,F5.0,1X,A10,F6.2,2F6.1)
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
      WRITE (LUNIO,60,IOSTAT=ERRNUM) XCRD,YCRD,ELEV,AREA,SLEN,FLWR,SLAS
     &            , FldHist, FHDur
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'*INITIAL CONDITIONS '
      LINIO = LINIO + 1
      EFINOC = MAX(EFINOC,-9.0)
      EFNFIX = MAX(EFNFIX,-9.0)
      ICREN  = MAX(ICREN,-9.0)
      ICREP  = MAX(ICREP,-9.0)
      WRITE (LUNIO,61,IOSTAT=ERRNUM) PRCROP,YRIC,NINT(WRESR),
     &NINT(WRESND),EFINOC,EFNFIX,ICWD,INT(ICRES),ICREN,ICREP,ICRIP,ICRID
   61 FORMAT (3X,A2,4X,I7,2(1X,I5),2(1X,F5.2),1X,F5.1,1X,I5,
     &        2(1X,F5.2),2(1X,F5.0))
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      DO I = 1, NLAYR
         LINIO = LINIO + 1
         FMT = "(3X,F5.0,1X,F5.3,"
         IF (INH4(I) < 1.0 .AND. INH4(I) > 0) THEN
           FMT = TRIM(FMT) // "F6.3,"
         ELSEIF (INH4(I) < 10.0 .AND. INH4(I) > 0) THEN
           FMT = TRIM(FMT) // "F6.2,"
         ELSE
           FMT = TRIM(FMT) // "F6.1,"
         ENDIF
         IF (INO3(I) < 1.0 .AND. INO3(I) > 0) THEN
           FMT = TRIM(FMT) // "F6.3)"
         ELSEIF (INO3(I) < 10.0 .AND. INO3(I) > 0) THEN
           FMT = TRIM(FMT) // "F6.2)"
         ELSE
           FMT = TRIM(FMT) // "F6.1)"
         ENDIF
         WRITE (LUNIO,TRIM(FMT),IOSTAT=ERRNUM)
     &      DS(I),SWINIT(I),INH4(I),INO3(I)
!  62    FORMAT (3X,F5.0,1X,F5.3,2(1X,F5.1))
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
      END DO
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'*PLANTING DETAILS   '
      LINIO = LINIO + 1
      IF ((INDEX('PI',CROP)) .GT. 0) THEN
         WRITE (LUNIO,70,IOSTAT=ERRNUM) YRPLT,IEMRG,PLANTS,PLTPOP,PLME,
     &          PLDS,ROWSPC,AZIR,SDEPTH,SDWTPL,SDAGE,ATEMP,PLPH,SPRLAP,
     &          NFORC,PLTFOR,NDOF,PMTYPE
      ELSE

         IF (SDWTPL .LE. 9999. .AND. PLANTS .LE. 9999.) THEN
           WRITE(LUNIO,70,IOSTAT=ERRNUM) YRPLT,IEMRG,PLANTS,PLTPOP,PLME,
     &          PLDS,ROWSPC,AZIR,SDEPTH,SDWTPL,SDAGE,ATEMP,PLPH,SPRLAP
         ELSE IF (SDWTPL .GT. 9999. .AND. PLANTS .LE. 9999.) THEN
           WRITE(LUNIO,71,IOSTAT=ERRNUM) YRPLT,IEMRG,PLANTS,PLTPOP,PLME,
     &          PLDS,ROWSPC,AZIR,SDEPTH,SDWTPL,NINT(SDWTPL),
     &          SDAGE,ATEMP,PLPH,SPRLAP
         ELSE IF (SDWTPL .LE. 9999. .AND. PLANTS .GT. 9999.) THEN
           WRITE(LUNIO,72,IOSTAT=ERRNUM) YRPLT,IEMRG,NINT(PLANTS),
     &          NINT(PLTPOP),PLME,PLDS,ROWSPC,AZIR,SDEPTH,
     &          SDWTPL,SDAGE,ATEMP,PLPH,SPRLAP
         ELSE
           WRITE(LUNIO,73,IOSTAT=ERRNUM) YRPLT,IEMRG,NINT(PLANTS),
     &          NINT(PLTPOP),PLME,PLDS,ROWSPC,AZIR,SDEPTH,
     &          NINT(SDWTPL),SDAGE,ATEMP,PLPH,SPRLAP
         ENDIF
      ENDIF
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)

C-----------------------------------------------------------------------
C     IRRIGATION
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'*IRRIGATION         '
      LINIO = LINIO + 1
      WRITE (LUNIO,75,IOSTAT=ERRNUM) EFFIRX,DSOILX,THETCX,IEPTX,IOFFX,
     &       IAMEX,AIRAMX
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      IF (NIRR .GT. 0) THEN
         DO I = 1, NIRR
            LINIO = LINIO + 1
            WRITE (LUNIO,76,IOSTAT=ERRNUM) IDLAPL(I),IRRCOD(I),AMT(I)!,
     &             !IIRV(I)
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
         END DO
      ENDIF
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'*FERTILIZERS        '
      IF (NFERT .GT. 0) THEN
         DO I = 1, NFERT
            LINIO = LINIO + 1
            WRITE (LUNIO,77,IOSTAT=ERRNUM)FDAY(I),IFTYPE(I),FERCOD(I),
     &          DFERT(I),ANFER(I),APFER(I),AKFER(I),ACFER(I),AOFER(I),
     &          FOCOD(I)
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
         END DO
      ENDIF
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'*RESIDUES           '
      IF (NARES .GT. 0) THEN
         DO I = 1,NARES
            LINIO = LINIO + 1
            WRITE (LUNIO,79,IOSTAT=ERRNUM)RESDAY(I),RESCOD(I),
     &         INT(RESIDUE(I)),RESN(I),RESP(I),RESK(I),RINP(I),
     &         DEPRES(I),RMET(I)
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
         END DO
      ENDIF
C-----------------------------------------------------------------------
C     Chemicals ....
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'*CHEMICALS          '
      IF (NCHEM .GT. 0) THEN
         DO I = 1,NCHEM
            LINIO = LINIO + 1
            WRITE (LUNIO,78,IOSTAT=ERRNUM) CDATE(I),CHCOD(I),
     &         CHAMT(I),CHMET(I),CHDEP(I),CHT(I),CHEXTR(I)
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
         END DO
      ENDIF

C-----------------------------------------------------------------------
C    Tillage  ....
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'*TILLAGE            '
      IF (NTIL .GT. 0) THEN
         DO I = 1,NTIL
            LINIO = LINIO + 1
            WRITE (LUNIO,80,IOSTAT=ERRNUM)TDATE(I),TIMPL(I),TDEP(I)
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
         END DO
      ENDIF

C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'*ENVIRONMENT        '
      IF (NEV .GT. 0) THEN
         DO I = 1,NEV
            LINIO = LINIO + 1
            IF (DAYFAC(I) .EQ. 'M' .AND. DAYADJ(I) .LE. 10.0) THEN
            WRITE (LUNIO,91,IOSTAT=ERRNUM)
     &            WMDATE(I),DAYFAC(I),DAYADJ(I),RADFAC(I),
     &            RADADJ(I),TXFAC(I),TXADJ(I),TMFAC(I),TMADJ(I),
     &            PRCFAC(I),PRCADJ(I),CO2FAC(I),INT(CO2ADJ(I)),
     &            DPTFAC(I),DPTADJ(I),WNDFAC(I),WNDADJ(I)
            ELSE IF (RADFAC(I) .EQ. 'M' .AND. RADADJ(I) .LE. 10.0) THEN
            WRITE (LUNIO,92,IOSTAT=ERRNUM)
     &            WMDATE(I),DAYFAC(I),DAYADJ(I),RADFAC(I),
     &            RADADJ(I),TXFAC(I),TXADJ(I),TMFAC(I),TMADJ(I),
     &            PRCFAC(I),PRCADJ(I),CO2FAC(I),INT(CO2ADJ(I)),
     &            DPTFAC(I),DPTADJ(I),WNDFAC(I),WNDADJ(I)
            ELSE IF (PRCFAC(I) .EQ. 'M' .AND. PRCADJ(I) .LE. 10.0) THEN
            WRITE (LUNIO,93,IOSTAT=ERRNUM)
     &            WMDATE(I),DAYFAC(I),DAYADJ(I),RADFAC(I),
     &            RADADJ(I),TXFAC(I),TXADJ(I),TMFAC(I),TMADJ(I),
     &            PRCFAC(I),PRCADJ(I),CO2FAC(I),INT(CO2ADJ(I)),
     &            DPTFAC(I),DPTADJ(I),WNDFAC(I),WNDADJ(I)
            ELSE IF ((TXFAC(I) .EQ. 'R' .AND. TXADJ(I) .LE. -10.0) .OR.
     &              (TMFAC(I) .EQ. 'R' .AND. TMADJ(I) .LE. -10.0)) THEN
            WRITE (LUNIO,94,IOSTAT=ERRNUM)
     &            WMDATE(I),DAYFAC(I),DAYADJ(I),RADFAC(I),
     &            RADADJ(I),TXFAC(I),TXADJ(I),TMFAC(I),TMADJ(I),
     &            PRCFAC(I),PRCADJ(I),CO2FAC(I),INT(CO2ADJ(I)),
     &            DPTFAC(I),DPTADJ(I),WNDFAC(I),WNDADJ(I)
            ELSE
            WRITE (LUNIO,90,IOSTAT=ERRNUM)
     &            WMDATE(I),DAYFAC(I),DAYADJ(I),RADFAC(I),
     &            RADADJ(I),TXFAC(I),TXADJ(I),TMFAC(I),TMADJ(I),
     &            PRCFAC(I),PRCADJ(I),CO2FAC(I),INT(CO2ADJ(I)),
     &            DPTFAC(I),DPTADJ(I),WNDFAC(I),WNDADJ(I)
            ENDIF
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
         END DO
      ENDIF
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'*HARVEST            '
      IF (NHAR .GT. 0) THEN
         DO I = 1,NHAR
            LINIO = LINIO + 1
            WRITE (LUNIO,100,IOSTAT=ERRNUM)HDATE(I),HSTG(I),HCOM(I),
     &             HSIZ(I),HPC(I),HBPC(I)
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
         END DO
      ENDIF   !End of non-sequence soils write
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
!     SOIL DATA
!     Do not need soil input for sequenced runs (except first year)
      IF (INDEX('FQ',RNMODE) .LE. 0 .OR. RUN == 1) THEN
        LINIO = LINIO + 1
        WRITE (LUNIO,40)'*SOIL               '
        LINIO = LINIO + 1
        WRITE (LUNIO,960,IOSTAT=ERRNUM) SLNO,SLSOUR,SLTX,SLDP,SLDESC
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
        LINIO = LINIO + 1
        WRITE (LUNIO,970,IOSTAT=ERRNUM) SSITE,SCOUNT,SLAT,SLONG,TAXON
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
        WRITE (LUNIO,980,IOSTAT=ERRNUM) SCOM,SALB,U,SWCON,CN2,SLNF,SLPF,
     &         SMHB,SMPX,SMKE,SGRP
        LINIO = LINIO + 1
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
!       1st tier soils
        DO I = 1, NLAYR
          LINIO = LINIO + 1
!          IF (TOTN(I) .LT. -9.0) THEN
!            TOTN(I) = -9.0
!          ENDIF
!          IF (BD(I) .LT. -9.0) THEN
!            BD(I) = -9.0
!          ENDIF
!          IF (OC(I) .LT. -9.0) THEN
!            OC(I) = -9.0
!          ENDIF

!         Construct format string depending on info avail.
!         DS(I),LL(I),DUL(I),SAT(I),SHF(I)
          FMT="(1X,F5.0,6X,4(1X,F5.3)"

         IF (SWCN(I) < 1.E-6) THEN
!           SWCN(I), BD(I)                         !SWCN output:
            FMT = TRIM(FMT) // ",1X,F5.0,1X,F5.2"  !"  -99."
          ELSEIF (SWCN(I) < 0.1) THEN
            FMT = TRIM(FMT) // ",1X,F5.4,1X,F5.2"  !"0.0001" to "0.0999"
          ELSEIF (SWCN(I) < 1.0) THEN
            FMT = TRIM(FMT) // ",1X,F5.3,1X,F5.2"  !" 0.100" to " 0.999"
          ELSEIF (SWCN(I) < 10.) THEN
            FMT = TRIM(FMT) // ",1X,F5.2,1X,F5.2"  !"  1.00" to "  9.99"
          ELSEIF (SWCN(I) < 100.) THEN
            FMT = TRIM(FMT) // ",1X,F5.1,1X,F5.2"  !"  10.0" to "  99.9"
          ELSE
            FMT = TRIM(FMT) // ",1X,F5.0,1X,F5.2"  !"  100." to " 9999."
          ENDIF
          IF (OC(I) > 0.0 .AND. OC(I) < 9.99) THEN
!           OC(I),CLAY(I),SILT(I),STONES(I)
            FMT = TRIM(FMT) // ",1X,F5.3,3(1X,F5.1)"
          ELSE
            FMT = TRIM(FMT) // ",1X,F5.1,3(1X,F5.1)"
          ENDIF
          IF (TOTN(I) > 10.0) THEN
!           TOTN(I),PH(I),PHKCL(I),CEC(I),ADCOEF(I)
            FMT = TRIM(FMT) // ",F6.2,4F6.2)"
          ELSEIF (TOTN(I) > 0.0) THEN
            FMT = TRIM(FMT) // ",F6.3,4F6.2)"
          ELSE
            FMT = TRIM(FMT) // ",F6.1,4F6.2)"
          ENDIF

          WRITE (LUNIO,TRIM(FMT),IOSTAT=ERRNUM) DS(I),
     &             LL(I),DUL(I),SAT(I),SHF(I),SWCN(I),BD(I),
     &             OC(I),CLAY(I),SILT(I),STONES(I),TOTN(I),
     &             PH(I),PHKCL(I),CEC(I),ADCOEF(I)
          IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
        END DO
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
!       2nd tier soils
        LINIO = LINIO + 1
        WRITE (LUNIO,40)'                    '
        DO I = 1, NLAYR
          LINIO = LINIO + 1
!         04/07/2005 CHP added EXCA - exchangable calcium
          WRITE (LUNIO,991,IOSTAT=ERRNUM,ADVANCE='NO')
     &      DS(I), EXTP(I), TOTP(I), ORGP(I), CACO(I), EXTAL(I),
     &      EXTFE(I), EXTMN(I), TOTBAS(I), PTERMA(I), PTERMB(I),
     &      EXK(I), EXMG(I), EXNA(I), EXTS(I), SLEC(I), EXCA(I)
  991     FORMAT (1X,F5.0,F6.2,9(1X,F5.1),F6.2,5F6.1)

!         04/21/2008 CHP added SASC - stable organic C (%)
          IF (SASC(I) > 0) THEN
            WRITE (LUNIO,'(F6.3)',IOSTAT=ERRNUM,ADVANCE='NO') SASC(I)
          ELSE
            WRITE (LUNIO,'("  -99.")',IOSTAT=ERRNUM,ADVANCE='NO')
          ENDIF
          IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)

!         2023-01-24 CHP added SAEA - soil alternate electron acceptors (mol Ceq/m3)
          IF (SAEA(I) > 0) THEN
            WRITE (LUNIO,'(F6.1)',IOSTAT=ERRNUM) SAEA(I)
          ELSE
            WRITE (LUNIO,'("  -99.")',IOSTAT=ERRNUM)
          ENDIF
          IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
        END DO
        LINIO = LINIO + 1
C-------------------------------------------------------------------------

C-----------------------------------------------------------------------
!       3rd tier soils - chp added 9/01/2011 for van Genuchten parameters
        LINIO = LINIO + 1
        WRITE (LUNIO,40)'                    '
        DO I = 1, NLAYR
          LINIO = LINIO + 1
          WRITE (LUNIO,992,IOSTAT=ERRNUM)
     &      DS(I), alphaVG(I), mVG(I), nVG(I), WCR(I)
  992     FORMAT (1X,F5.0,4F6.2)
          IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
        END DO
      ENDIF   !End of non-sequence soils write
C-------------------------------------------------------------------------

C-----------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
         WRITE (LUNIO,40)'*CULTIVAR           '
         LINIO = LINIO + 1

!     ------------------------------------------------------------------
        SELECT CASE (MODEL(1:5))

!       Generic SALUS crops
        CASE('SALUS')
          WRITE(LUNIO,'(A6,1X,A16,7X,A)',IOSTAT=ERRNUM) VARNO, VRNAME,
     &         trim(PLAINTXT)

!       CROPGRO crops
        CASE('CRGRO','PRFRM')

!        CASE ('BN','PN','SB','TM','PE','CH','PP','PR',
!     &        'C3','C4','G0','G1','G2','G3','G4','G5','G6','G7','G8',
!     &        'BR','VB','CP','CB','FB','CO','CT')
     &
            WRITE (LUNIO,1500,IOSTAT=ERRNUM) VARNO,VRNAME,ECONO,CSDVAR,
     &           PPSEN,PH2T5,PHTHRS(6),PHTHRS(8),PHTHRS(10),PHTHRS(13),
     &           LFMAX,SLAVAR,SIZELF,XFRUIT,WTPSD,SFDUR,SDPDVR,PODUR,
     &           THRESH, SDPRO, SDLIP

!       Ceres wheat, barley
!       CropSim - wheat, barley, cassava
        CASE('WHCER', 'BACER', 'CSCRP','CSCAS','CSYCA')
!       Do nothing - these models read the INH file written by OPTEMPXY2K

!       APSIM Wheat (NWheat)
!  JG moved CUL parameters to ECO file for WHAPS and TFAPS 01/21/2020
        CASE('WHAPS')
                WRITE (LUNIO,1850,IOSTAT=ERRNUM)
     &            VARNO,VRNAME,ECONO,VSEN,PPSEN,P2,P5,PHINT,GRNO,MXFIL,
     &            STMMX,SLAP1
!       Tef based on APSIM NWheat KEP
    !Created new FORMAT to read in CUL file for tef
        CASE('TFAPS')
                WRITE (LUNIO,1855,IOSTAT=ERRNUM)
     &            VARNO,VRNAME,ECONO,VSEN,PPSEN,P2,P5,PHINT,GRNO,MXFIL,
     &            STMMX,SLAP1
!       Ceres Maize, sweetcorn
        CASE('MZCER','SWCER')
		  WRITE (LUNIO,1800,IOSTAT=ERRNUM) VARNO,VRNAME,ECONO,
     &                    P1,P2,P5,G2,G3,PHINT
!WDB 7/2016 Added new coefficients for sugar beets
        CASE('BSCER')
		  WRITE (LUNIO,1802,IOSTAT=ERRNUM) VARNO,VRNAME,ECONO,
     &                    P1,P2,P5,G2,G3,PHYL1,PHYL2,FRSUG,DRCER

!       Ixim maize
        CASE('MZIXM')
            WRITE (LUNIO,1801,IOSTAT=ERRNUM) VARNO,VRNAME,ECONO,
     &                    P1,P2,P5,G2,G3,PHINT,AX,LX,LFN
!       Ceres sorghum
        CASE('SGCER')
               WRITE (LUNIO,1900,IOSTAT=ERRNUM) VARNO,VRNAME,ECONO,
     &               P1,P2,P2O,P2R,PANTH,P3,P4,P5,PHINT,G1,G2
C-GH &               P1,P2O,P2R,P5,G1,G2,PHINT,P3,P4,P2,PANTH
C-GH &               P1,P2O,P2R,P5,G1,G2,PHINT,P3,P4
!              Write optional cultivar parameters if present
               IF (PBASE > 1E-2 .AND. PSAT > 1E-2) THEN
                 WRITE(LUNIO, 1901) PBASE, PSAT
               ENDIF

!       Ceres millet
        CASE('MLCER')
               WRITE (LUNIO,1950,IOSTAT=ERRNUM) VARNO,VRNAME,ECONO,
     &               P1,P2O,P2R,P5,G1,G4,PHINT,G0,G5

!       Ceres rice
        CASE ('RICER')
            WRITE (LUNIO,1985,IOSTAT=ERRNUM) VARNO,VRNAME,ECONO,
!    &             P1,P2R,P5,P2O,G1,G2,G3,G4,PHINT, G5
     &             P1,P2R,P5,P2O,G1,G2,G3,PHINT, THOT, TCLDP, TCLDF
 1985 FORMAT (A6,1X,A16,1X,A6,5(F6.1),F6.3,2(F6.2),3F6.1)

!       Ceres TEFF
        CASE ('TFCER')
            WRITE (LUNIO,1986,IOSTAT=ERRNUM) VARNO,VRNAME,ECONO,
!    &             P1,P2R,P5,P2O,G1,G2,G3,G4,PHINT, G5
     &             P1,P2R,P5,P2O,G1,G2,G3,PHINT, THOT, TCLDP, TCLDF
 1986 FORMAT (A6,1X,A16,1X,A6,5(F6.1),F6.3,2(F6.2),3F6.1)

!!       ORYZA rice
!        CASE ('RIORZ')
!            WRITE (LUNIO,'(A6,1X,A16,1X,A)',IOSTAT=ERRNUM) VARNO,VRNAME,
!     &          TRIM(PLAINTXT)

!       Substor potato
        CASE ('PTSUB')
               WRITE (LUNIO,1400,IOSTAT=ERRNUM) VARNO,VRNAME,ECONO,
     &               G2,G3,PD,P2,TC
!     &               G2,G3,G4,PD,P2,TC

!       CaneGro sugarcane
        CASE ('SCCAN')
              WRITE (LUNIO,3000,IOSTAT=ERRNUM) VARNO,VRNAME,
     &          trim(PLAINTXT)

!       Casupro sugarcane
        CASE ('SCCSP')
              WRITE (LUNIO,1055,IOSTAT=ERRNUM) VARNO,VRNAME,ECONO,
     &          LFMAX,PHTMAX,StkH2OFac,SuH2OFac,empty,PLF1,PLF2,
     &          Gamma,StkB,StkM,empty,
     &          SIZLF,LIsun,LIshd,empty,TB(1),TO1(1),TO2(1),TM(1),
     &          PI1,PI2,DTPI,LSFAC,empty,LI1,TELOM,TB(2),TO1(2),
     &          TO2(2),TM(2),Ph1P,Ph1R,Ph2,Ph3,Ph4,StkHrNO,RTNFAC,
     &          MinGr,empty,RES30C,RLF30C,R30C2,empty,empty

!       Sunflower
        CASE ('SUOIL')
            WRITE (LUNIO,1960,IOSTAT=ERRNUM) VARNO,VRNAME,ECONO,
     &             P1,P2,P5,G2,G3,O1

!       Pineapple
        CASE ('PIALO')
            WRITE (LUNIO,1970,IOSTAT=ERRNUM) VARNO,VRNAME,ECONO,
     &            P1,P2,P3,P4,P5,P6,G2,G3,PHINT
 1970 FORMAT (A6,1X,A16,1X,A6,1X,F6.1,4F6.0,F6.1,F6.0,2F6.1) 
!B0067 SC-ANGUE         IB0001  60.0   500   500  2195   400  60.0   200  14.0  95.0

!       Aroids taro & tanier
        CASE ('TRARO','TNARO')
            WRITE (LUNIO,1975,IOSTAT=ERRNUM) VARNO,VRNAME,ECONO,
     &             P1,P3,P4,P5,G3,G4,PHINT,PCINT,PCGRD

        END SELECT

c          ELSEIF (INDEX ('CO',CROP) .GT. 0) THEN
c            WRITE (LUNIO,1995,IOSTAT=ERRNUM) VARNO,VRNAME,ECONO,
c     &             SCPB,RESPC,SQCON,FCUT,FLAI,DDISQ

      ENDIF

      CLOSE (LUNIO)
      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

   40 FORMAT (A20)
   50 FORMAT (I3,A8,1X,A2,1X,A60)
   55 FORMAT (I3,I2,2(1X,I1),1X,A25)
   56 FORMAT (3X,A2,1X,A6,1X,A16)
   60 FORMAT (3X,2(F15.10,1X),F9.3,1X,F17.1,1X,F5.0,2(1X,F5.1),1X,A5,I6)
   70 FORMAT (3X,I7,1X,I7,2F6.1,2(5X,A1),2(1X,F5.0),1X,F5.1,
     &        2(1X,F5.0),3(1X,F5.1),I6,F6.1,2I6)
   71 FORMAT (3X,I7,1X,I7,2F6.1,2(5X,A1),2(1X,F5.0),1X,F5.1,
     &        I6,1X,F5.0,3(1X,F5.1),I6,F6.1,2I6)
   72 FORMAT (3X,I7,1X,I7,2I6,2(5X,A1),2(1X,F5.0),1X,F5.1,
     &        2(1X,F5.0),3(1X,F5.1),I6,F6.1,2I6)
   73 FORMAT (3X,I7,1X,I7,2I6,2(5X,A1),2(1X,F5.0),1X,F5.1,
     &        I6,1(1X,F5.0),3(1X,F5.1),I6,F6.1,2I6)
   74 FORMAT (3X,I7,1X,I7,2I6,2(5X,A1),2(1X,F5.0),1X,F5.1,
     &        I6,1X,F5.0,3(1X,F5.1),I6,F6.1,2I6)
   75 FORMAT (2X,1X,F5.3,3(1X,F5.0),2(1X,A5),1X,F5.1)
   76 FORMAT (3X,I7,1X,A5,1X,F5.1)
   77 FORMAT (3X,I7,2(1X,A5),6(1X,F5.0),1X,A5)
   78 FORMAT (3X,I7,1X,A5,1X,F5.2,1X,A5,1X,F5.1,1X,A5,A42)
   79 FORMAT (3X,I7,1X,A5,1X,I5,3(1X,F5.2),2(1X,F5.0),1X,A5)
   80 FORMAT (3X,I7,1X,A5,1X,F5.1)
   91 FORMAT (3X,I7,1X,A1,F4.2,4(1X,A1,F4.1),1X,A1,I4,2(1X,A1,F4.1))
   92 FORMAT (3X,I7,1X,A1,F4.1,1X,A1,F4.2,3(1X,A1,F4.1),1X,A1,I4,
     &       2(1X,A1,F4.1))
   93 FORMAT (3X,I7,4(1X,A1,F4.1),1X,A1,F4.2,1X,A1,I4,
     &       2(1X,A1,F4.1))
   94 FORMAT (3X,I7,2(1X,A1,F4.1),2(1X,A1,F4.0),1X,A1,F4.1,1X,A1,I4,
     &       2(1X,A1,F4.1))
   90 FORMAT (3X,I7,5(1X,A1,F4.1),1X,A1,I4,2(1X,A1,F4.1))
  100 FORMAT (3X,I7,3(1X,A5),2(1X,F5.0))
  900 FORMAT (14X,I6,1X,I5,5X,A1,1X,I7,1X,I5,1X,A25)
  910 FORMAT (14X,9(5X,A1),2I6)
  915 FORMAT (14X,7(5X,A1),5X,I1,5(5X,A1))
  920 FORMAT (14X,3(5X,A1),4X,I2,9(5X,A1))
  930 FORMAT (14X,2(1X,I7),5(1X,F5.0))
  940 FORMAT (14X,3(1X,F5.0),2(1X,A5),1X,F5.1,1X,F5.3)
  950 FORMAT (15X,F5.0,1X,I5,1X,F5.0)
  960 FORMAT (1X,A10,2X,A11,1X,A5,1X,F5.0,1X,A50)
  970 FORMAT (2(1X,A11),2(F8.3),1X,A50)

  980 FORMAT (1X,A5,1X,F5.2,1X,F5.1,1X,F5.2,1X,F5.0,2(1X,F5.2),4(1X,A5),
     &        F6.2)

 1000 FORMAT (A6,1X,A16,1X,A6,1X,7(F6.1))
 1050 FORMAT (A6,1X,A16,1X,A6,9(1X,F5.0),1X,I5,1X,F5.2,1X,F5.0,1X,F5.1)
 1055 FORMAT (A6,1X,A16,1X,A6,F6.3,F6.0,2F6.3,F6.2,5F6.3,F6.2,
     &        F6.0,2F6.2,F6.2,7F6.1, 2F6.2, 1X, F5.4,7F6.1,F6.2,2F6.0,
     &        F6.1,F6.2,F6.2,F6.2,3F6.2,2F6.2)

 1400 FORMAT (A6,1X,A16,1X,A6,1X,F6.0,4(F6.1))
 1500 FORMAT (A6,1X,A16,1X,A6,F6.2,F6.3,5F6.2,F6.3,2F6.1,F6.2,
     &        F6.3,3F6.2,F6.1,2F6.3)
 1550 FORMAT (A6,1X,A16,1X,A6,A)

 1600 FORMAT (A6,1X,A16,1X,A6,2(F6.1),F6.2,2(F6.1),F6.2,F6.0,
     &        F6.3,F6.2,6(F6.0))
 1700 FORMAT (A6,1X,A16,1X,A6,1X,5(F6.1),F6.2, F6.1)
 1800 FORMAT (A6,1X,A16,1X,A6,1X,F6.1,F6.3,2(F6.1),2(F6.2))
! JG modified to move CUL parameters to ECO file
 1850 FORMAT (A6,1X,A16,1X,A6,1X,
 !             1     2     3     4     5     6     7     8     9     0
     &       F6.2, F6.2, F6.1, F6.1, F6.1, F6.1, F6.2, F6.2, F6.1)

! JG modified to move CUL parameters to ECO file
 1855 FORMAT (A6,1X,A16,1X,A6,1X,
 !             1     2     3     4     5     6     7     8     9     0
     &       F6.2, F6.2, F6.1, F6.1, F6.1, F6.1, F6.2, F6.2, F6.1)

 1801 FORMAT (A6,1X,A16,1X,A6,1X,F6.1,F6.3,2(F6.1),2(F6.2),2(F6.1),I4)
 1802 FORMAT (A6,1X,A16,1X,A6,1X,F6.1,F6.3,2(F6.1),5(F6.2))
 1900 FORMAT (A6,1X,A16,1X,A6,1X,F6.1,F6.2,4(F6.1),F6.2,4(F6.1))
C-GH 1900 FORMAT (A6,1X,A16,1X,A6,1X,F6.1,F6.2,4(F6.1),F6.2,2(F6.1))
 1901 FORMAT (2F6.2)
 1950 FORMAT (A6,1X,A16,1X,A6,1X,F6.1,F6.2,2(F6.1),3(F6.2),2(F6.0))
c1960 FORMAT (A6,1X,A16,1X,A6,1X,F6.2,F8.4,F7.2,F8.2,F7.3,F4.0)
 1960 FORMAT (A6,1X,A16,1X,A6,1X,F5.1,1X,F5.2,1X,F5.1,1X,F5.0,1X,F5.2,
     &        1X,F5.0)
 1975 FORMAT (A6,1X,A16,1X,A6,4(F6.0),2(F6.2),3(F6.1))
 1995 FORMAT (A6,1X,A16,1X,A6,F6.1,3(F6.3),F6.2,F6.1)
 2000 FORMAT ('*MODEL INPUT FILE   ',9X,A1,5(1X,I5))
 2040 FORMAT ('MODEL          ',A8,5X,A80)
 2050 FORMAT ('FILEX          ',A12,1X,A80)
 2100 FORMAT ('FILEA          ',A12,1X,A80)
 2200 FORMAT ('FILET          ',A12,1X,A80)
 2300 FORMAT ('SPECIES        ',A12,1X,A80)
 2400 FORMAT ('ECOTYPE        ',A12,1X,A80)
 2500 FORMAT ('CULTIVAR       ',A12,1X,A80)
 2600 FORMAT ('PESTS          ',A12,1X,A80)
 2700 FORMAT ('SOILS          ',A12,1X,A80)
! 2800 FORMAT ('WEATHER        ',A12,1X,A80)
 2900 FORMAT ('OUTPUT         ',A8)
 3000 FORMAT (A6,1X,A16,1X,A255)

      END SUBROUTINE OPTEMPY2K

