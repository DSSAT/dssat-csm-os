C=======================================================================
C  SENS, Subroutine
C
C  Determines sensitivity analysis
C-----------------------------------------------------------------------
C  Revision history
C
C  01/09/1989 GH  Written
C  05/28/1993 PWW Header revision and minor changes
C  04/02/1996 GH  Minor changes
C  02/02/1998 GH  Rename SENSDM to SEPEST
C  01/31/2002 GH  Added RNMODE=E for new CSM model
C  06/09/2002 GH  Modified for Y2K
C  02/21/2006 GH  Use MODEL variable for information exchange
C-----------------------------------------------------------------------
C  INPUT  : NSENS,VARNO,VARTY,VRNAME,FTYPEN,LNIC,LNSA,WRESR,WRESND,ISIM,
C           NYRS,IPLT,WMODI,ECONO,ECONAM,ECOTYP,PRCROP,SWINIT,INO3,INH4,
C           RUN,FROP,YRIC,EFINOC,EFNFIX,CROP,IVRGRP
C
C  LOCAL  : LINE,ERRKEY,MENU,NLOOP,FLAG,EFF,DSOIL,THETAC,AIRAM,TOTAPW
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : INPUT
C
C  Calls  : ERROR CLEAR YR_DOY NAILUJ SETIME SECROP IPVAR IPECO SEVAR
C           SEWTH DATEC SECLI SESOIL SEINIT SEPLT SEHAR SEFERT SEIRR
C           SERES SESIM
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

       SUBROUTINE SENS (NSENS,VARNO,VARTY,VRNAME,FTYPEN,LNIC,LNSA,
     &   WRESR,WRESND,ISIM,NYRS,IPLT,WMODI,ECONO,ECONAM,ECOTYP,
     &   PRCROP,SWINIT,INO3,INH4,RUN,FROP,YRIC,EFINOC,EFNFIX,
     &   CROP,IVRGRP,ISENS,MODEL, RNMODE, FILEX,FILEX_P,ISWITCH,CONTROL)

      USE ModuleDefs
      IMPLICIT NONE
      EXTERNAL CLEAR, DATEC, ERROR, FILL_ISWITCH, IPECO, IPVAR, NAILUJ, 
     &  SECLI, SECROP, SEFERT, SEFLD, SEFREQ, SEHARV, SEINIT, SEIRR, 
     &  SEPEST, SEPLT, SERES, SESIM, SESOIL, SETIME, SEVAR, SEWTH, 
     &  YR_DOY

      INCLUDE 'COMIBS.blk'
      INCLUDE 'COMSOI.blk'
      INCLUDE 'COMSWI.blk'

      CHARACTER*1  ANS,WMODI,SWSPRF, RNMODE
      CHARACTER*2  CROPC,CROP,PRCROP
      CHARACTER*3  MSSIM,MSPLT
      CHARACTER*6  VARTY,VARNO,ERRKEY,ECOTYP,ECONO
      CHARACTER*7  FILEWT
	CHARACTER*8  MODEL
      CHARACTER*12 FILEWP, FILEX
      CHARACTER*15 NFMANT,INMANT,WTMANT
      CHARACTER*16 VRNAME,ECONAM
      CHARACTER*25 HARMAN,NIMANT,REMANT
      CHARACTER*30 IRMANT
      CHARACTER*40 PSMANT
	CHARACTER*92 FILEX_P
      CHARACTER*1000 ATLINE

      INTEGER MENU,FROP,NSENS,NLOOP
      INTEGER LNIC,LNSA,YRIC,ISIM,NYRS
      INTEGER IPLT,IPYRS,DSSIM,IPYRP,DSPLT
      INTEGER RUN,IVRGRP,FTYPEN,ISENS,YRPLTX

      REAL    SWINIT(NL),WRESR,WRESND,INO3(NL),INH4(NL),EFNFIX,EFINOC

      TYPE (SwitchType)  ISWITCH
      TYPE (ControlType) CONTROL

      PARAMETER (ERRKEY = 'SENS  ')
      FILEWT = 'WTH.LST'

      MENU   = 0

      IF (RUN .EQ. 1 .AND. INDEX('IE',RNMODE) .GT. 0) THEN
         CALL CLEAR
         WRITE (*,100)
      ENDIF

      IF (RUN .EQ. 1) READ (5,'(A1)') ANS
      NLOOP = 0

  400 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 50) CALL ERROR (ERRKEY,1,' ',0)

      IF (MEWTH .EQ. 'M') THEN
         WTMANT = 'OBSERVED   DATA'
       ELSEIF (MEWTH .EQ. 'G') THEN
         WTMANT = 'EXT. SIMUL.    '
       ELSEIF (MEWTH .EQ. 'S') THEN
         WTMANT = 'INT. GENER.    '
      ENDIF

      IF (ISWDIS .EQ. 'Y') THEN
         PSMANT = 'PEST & DISEASE INTERACTION SIMULATED    '
       ELSE
         PSMANT = 'PEST & DISEASE INTERACTION NOT SIMULATED'
      ENDIF

      IF (MESIC .EQ. 'M') THEN
         INMANT = 'AS REPORTED    '
       ELSEIF (MESIC .EQ. 'S') THEN
         INMANT = 'PREVIOUS RUN   '
      ENDIF

      IF (IHARI .EQ. 'A') THEN
         HARMAN = 'AUTOMATIC                '
       ELSEIF (IHARI .EQ. 'G') THEN
         HARMAN = 'AT REPORTED GROWTH STAGES'
       ELSEIF (IHARI .EQ. 'M') THEN
         HARMAN = 'AT HARVEST MATURITY      '
       ELSEIF (IHARI .EQ. 'R' .OR. IHARI .EQ. 'W' .OR.
     &   IHARI .EQ. 'X' .OR. IHARI .EQ. 'Y' .OR. IHARI .EQ. 'Z') THEN
         HARMAN = 'ON REPORTED DATE(S)      '
       ELSEIF (IHARI .EQ. 'D') THEN
         HARMAN = 'ON REPORTED DAP          '
      ENDIF

      IF (ISWWAT .EQ. 'N') THEN
         IRMANT = 'NO WATER BALANCE SIMULATION   '
       ELSEIF (IIRRI .EQ. 'A') THEN
         IRMANT = 'AUTOMATIC IRRIGAT-REFILL PROF '
       ELSEIF (IIRRI .EQ. 'F') THEN
         IRMANT = 'AUTOMATIC IRRIGAT-FIXED AMOUNT'
       ELSEIF (IIRRI .EQ. 'N') THEN
         IRMANT = 'RAINFED                       '
       ELSEIF (IIRRI .EQ. 'P') THEN
         IRMANT = 'FIELD SCHEDULE & AUT REFILL PR'
       ELSEIF (IIRRI .EQ. 'R') THEN
         IRMANT = 'ON REPORTED DATE(S)           '
       ELSEIF (IIRRI .EQ. 'D') THEN
         IRMANT = 'ON REPORTED DAP               '
       ELSE IF (IIRRI .EQ. 'W') THEN
         IRMANT = 'FIELD SCHEDULE & AUT. FIXED AM'
       ELSE
         IRMANT = '                              '
      ENDIF

      IF (ISWNIT .NE. 'Y') THEN
         NIMANT = 'NO N-BALANCE SIMULATION  '
       ELSEIF (IFERI .EQ. 'A') THEN
         NIMANT = 'AUTOMATIC N-FERTILIZER AP'
       ELSEIF (IFERI .EQ. 'F') THEN
         NIMANT = 'AUTOMATIC N-FERTILIZER AP'
       ELSEIF (IFERI .EQ. 'N') THEN
         NIMANT = 'NO N-FERTILIZER APPLICAT.'
       ELSEIF (IFERI .EQ. 'R') THEN
         NIMANT = 'ON REPORTED DATE(S)      '
       ELSEIF (IFERI .EQ. 'D') THEN
         NIMANT = 'ON REPORTED DAP          '
       ELSE
         NIMANT = '                         '
      ENDIF

      IF (ISWNIT .EQ. 'Y') THEN
         IF (ISWSYM .EQ. 'Y') THEN
            NFMANT = 'N-FIX SIMULAT. '
          ELSEIF (ISWSYM .EQ. 'N') THEN
            NFMANT = 'NO N-FIX SIMUL.'
          ELSEIF (ISWSYM .EQ. 'U') THEN
            NFMANT = 'UNLIMITED N-FIX'
          ELSE
            NFMANT = 'N-FIX NON LIMIT'
         ENDIF
      ENDIF

      IF (ISWNIT .NE. 'Y') THEN
         REMANT = 'NO N-BALANCE SIMULATION  '
       ELSE IF (IRESI .EQ. 'F') THEN
         REMANT = 'AUTOMATIC RESIDUE APPLIC.'
       ELSE IF (IRESI .EQ. 'A') THEN
         REMANT = 'AUTOMATIC RESIDUE APPLIC.'
       ELSE IF (IRESI .EQ. 'N') THEN
         REMANT = 'NO RESIDUE APPLICATION   '
       ELSE IF (IRESI .EQ. 'R') THEN
         REMANT = 'ON REPORTED DATE(S)      '
       ELSEIF (IRESI .EQ. 'D') THEN
         REMANT = 'ON REPORTED DAP          '
       ELSE
         REMANT = '                         '
      ENDIF

      CALL YR_DOY (YRSIM,IPYRS,ISIM)
      CALL NAILUJ (ISIM,IPYRS,MSSIM,DSSIM)
      CALL YR_DOY (YRPLT,IPYRP,IPLT)
      CALL NAILUJ (IPLT,IPYRP,MSPLT,DSPLT)

      IF (INDEX('IE',RNMODE) .GT. 0) THEN
         CALL CLEAR
         IF (ISWWAT .EQ. 'Y') THEN
         WRITE(*,500) MSSIM,DSSIM,IPYRS,
     &     CROPD,FILEC,FILEG,
     &     VRNAME,IVRGRP,ECONAM,
     &     WSTA,WTMANT,WMODI,
     &     PEDON,SLTX,
     &     INMANT,
     &     MSPLT,DSPLT,IPYRP,ROWSPC,PLTPOP,
     &     HARMAN,
     &     IRMANT,
     &     NIMANT,NFMANT,
     &     REMANT,
     &     PSMANT,
     &     MEHYD,ISWNIT,ISWSYM,ISWDIS,MEPHO,MEWTH,MEEVP
         ELSE
         WRITE(*,500) MSSIM,DSSIM,IPYRS,
     &     CROPD,FILEC,FILEG,
     &     VRNAME,IVRGRP,ECONAM,
     &     WSTA,WTMANT,WMODI,
     &     PEDON,SLTX,
     &     INMANT,
     &     MSPLT,DSPLT,IPYRP,ROWSPC,PLTPOP,
     &     HARMAN,
     &     IRMANT,
     &     NIMANT,NFMANT,
     &     REMANT,
     &     PSMANT,
     &     ISWWAT,ISWNIT,ISWSYM,ISWDIS,MEPHO,MEWTH,MEEVP
         ENDIF
         IF (RUN .EQ. 1) THEN
            WRITE (*,550) FROP,IDETO,IDETS,IDETG,IDETW,IDETN,IDETD
         ENDIF
         WRITE (*,575)
      ENDIF
C
C     Get menu choice
C
      READ (5,'(I2)',ERR = 400) MENU
C
C     Branch to menu choice
C
      IF (MENU .EQ. 0) THEN
          CALL FILL_ISWITCH(
     &      CONTROL, ISWITCH, FROP, MODEL, NYRS, RNMODE)
          RETURN
      ELSE IF (MENU .EQ. 1) THEN
          CALL SETIME (ISIM,YEAR,NYRS,FILEW,RNMODE,MESIC,YRSIM,YRPLT,
     &         IHARI,HDATE,NHAR,PATHWT)
      ELSE IF (MENU .EQ. 2) THEN
          CROPC = CROP
          CALL SECROP (FILEC,FILEE,FILEG,RNMODE,CROP,CROPD,PATHCR)
          IF (CROP .NE. CROPC) THEN
             CALL IPVAR (FILEG,NSENS,RNMODE,VARNO,VARTY,VRNAME,
     &                  PATHGE,ECONO, MODEL, ATLINE) !, CROP)
             IF (INDEX('GRO,CSM,CAN,CER',MODEL(3:6)) .GT. 0) THEN 
                NSENS =  0
                CALL IPECO (FILEE,NSENS,RNMODE,PATHEC,ECOTYP,ECONAM,
     &               ECONO,IVRGRP,MODEL)
                NSENS =  1
             ENDIF
          ENDIF
      ELSE IF (MENU .EQ. 3) THEN
          CALL SEVAR (FILEE,FILEG,NSENS,RNMODE,VARNO,VARTY,VRNAME,
     &         IVRGRP,PATHGE,PATHEC,ECOTYP,ECONAM,ECONO,CROP,MODEL)
      ELSE IF (MENU .EQ. 4) THEN
          IF (MEWTH .NE. 'S' .AND. MEWTH .NE. 'W') THEN
             FILEWP = FILEW
             CALL SEWTH (FILEW,RNMODE,WMODI,WTHADJ,PATHWT,MEWTH,YEAR,
     &            DSSATP,FILEWT,RSEED1)
             WSTA = FILEW(1:4)
             IF (MEWTH .NE. 'S' .AND. MEWTH .NE. 'W'
     &                          .AND. FILEW .NE. FILEWP) THEN
                CALL DATEC (FILEW,YRSIM,YRPLT,YEAR,PATHWT,
     &               IDLAPL,NIRR,RESDAY,NARES,FDAY,NFERT,
     &               HLATE,PWDINF,PWDINL,NHAR,HDATE,
     &               IIRRI,IFERI,IHARI,IRESI,ISWCHE,ISWTIL,
     &               CDATE,TDATE,YRIC,NTIL,NCHEM,NEV,WMDATE)
             ENDIF
           ELSEIF (MEWTH .EQ. 'S' .OR. MEWTH .EQ. 'W' ) THEN
             CALL SECLI (FILEW,RNMODE,WMODI,WTHADJ,PATHWT,MEWTH,YEAR,
     &            RSEED1,DSSATP,FILEWT)
          ENDIF
          IF (WMODI .EQ. 'Y' .AND. NEV .EQ. 0)  THEN
             NEV = 1
             WMDATE(1) = YRSIM
          ENDIF
      ELSE IF (MENU .EQ. 5) THEN
          CALL SESOIL (FILES,FILEX,FILEX_P,NSENS,RNMODE,SLNF,SLPF,U,
     &		 SWCON,CN2,SALB,DEPMAX,PEDON,SLNO,SLTX,LL,DUL,SAT,
     &         SHF,BD,OC,PH,DLAYR,NLAYR,DS,LNIC,LNSA,YRIC,PRCROP,
     &         WRESR,WRESND,EFINOC,EFNFIX,PATHSL,SWINIT,INO3,INH4,
     &         EXTP,ICWD,ICRES,ICREN,ICREP,ICRIP,
!    &         ICRID,SWCN,ADCOEF,TOTN,YRSIM, SMPX, EXK,
     &         ICRID,SWCN,ADCOEF,TOTN, SMPX, EXK,
     &         PHKCL, SMHB, SMKE, ISWITCH)

      ELSE IF (MENU .EQ. 6) THEN
          SWSPRF = 'N'
          CALL SEINIT (RNMODE,NLAYR,SWINIT,PRCROP,WRESR,WRESND,DLAYR,
     &                SLPF,DS,SWSPRF,INO3,INH4,ICWD,ICRES,ICREN,
     &                ICREP,ICRIP,ICRID)
      ELSE IF (MENU .EQ. 7) THEN
          YRPLTX = YRPLT
          CALL SEPLT (RNMODE,PLTPOP,ROWSPC,AZIR,SDEPTH,SDWTPL,YRSIM,
     &         YRPLT,IPLT,IPLTI,YEAR,PWDINF,PWDINL,SWPLTL,SWPLTH,
     &         SWPLTD,PTX,PTTN,PLME,SDAGE,ATEMP)
          IF (YRPLTX .NE. YRPLT) THEN
             ISENS = 1
          ENDIF
      ELSE IF (MENU .EQ. 8) THEN
          CALL SEHARV (RNMODE,IHARI,HDLAY,HLATE,HPP,HRP,YEAR,
     &                 HSTG,HCOM,HSIZ,HDATE,HPC,CROP,NHAR,HBPC,
     &                 YRPLT)
      ELSE IF (MENU .EQ. 9) THEN
          CALL SEIRR (IDETW,RNMODE,ISWWAT,IIRRI,IRMANT,EFFIRR,ISWNIT,
     &         AIRAMT,NAPW,IDLAPL,AMT,TOTAPW,NIRR,IRRCOD,DSOIL,THETAC,
     &         EFFIRX)
      ELSE IF (MENU .EQ. 10) THEN
          CALL SEFERT (IDETN,RNMODE,ISWNIT,ISWWAT,NIMANT,NFMANT,
     &         IFERI,ISWSYM,EFINOC,EFNFIX,DSOILN,SOILNC,SOILNX,FTYPEN,
     &         NFERT,IFTYPE,ANFER,DFERT,TOTNAP,FDAY,NSWITCH,CROP,NCODE,
     &         FERCOD)
      ELSE IF (MENU .EQ. 11) THEN
          WRITE(*,650)
          PAUSE
      ELSE IF (MENU .EQ. 12) THEN
          CALL SERES (RNMODE,CROP,RESIDUE,DEPRES,ISWWAT,ISWNIT,
     &                  RESN,RESP,RESDAY,NARES,RESAMT,RESCOD,RINP,RESK,
     &                  IRESI,NSWITCH,IDETN,REMANT)
      ELSE IF (MENU .EQ. 13) THEN
          CALL SEPEST (IDETD,RNMODE,ISWDIS)
      ELSE IF (MENU .EQ. 14) THEN
          CALL SEFLD (RNMODE,XCRD,YCRD,ELEV,SLOPE)
      ELSE IF (MENU .EQ. 15) THEN
          CALL SESIM (RNMODE,ISWNIT,ISWWAT,NFMANT,ISWSYM,ISWPHO,
     &         ISWDIS,MEWTH,MESIC,MEEVP,MEPHO,FILEW,YEAR,PATHWT,
     &         NSWITCH,CROP,MEHYD,MESOM,MODEL)
      ELSE IF (MENU .EQ. 16) THEN
          IF (RUN .EQ. 1) THEN
             CALL SEFREQ (RNMODE,IDETO,IDETS,IDETG,IDETC,IDETW,IDETN,
     &            IDETP,IDETD,IOX,FROP,RUN,IDETL)
             IF (RUN .EQ. 1) THEN
                IF (IOX .EQ. 'Y') THEN
                   WRITE(OUTO(1:12),80) EXPER,'.',CG,'O'
c                   WRITE(OUTG(1:12),80) EXPER,'.',CG,'G'
c                   WRITE(OUTW(1:12),80) EXPER,'.',CG,'W'
c                   WRITE(OUTN(1:12),80) EXPER,'.',CG,'N'
c                   WRITE(OUTC(1:12),80) EXPER,'.',CG,'C'
c                   WRITE(OUTD(1:12),80) EXPER,'.',CG,'D'
C-GH               WRITE(OUTP(1:12),80) EXPER,'.',CG,'P'
c                   WRITE(OUTF(1:12),80) EXPER,'.',CG,'F'
c                   WRITE(OUTH(1:12),80) EXPER,'.',CG,'H'
c                   WRITE(OUTR(1:12),80) EXPER,'.',CG,'R'
                 ELSE
                   OUTO  = 'OVERVIEW.OUT'
c                   OUTG  = 'GROWTH.OUT'
c                   OUTW  = 'WATER.OUT'
c                   OUTN  = 'NITROGEN.OUT'
c                   OUTC  = 'CARBON.OUT'
c                   OUTD  = 'PEST.OUT'
C-GH               OUTP  = 'PHOSPHOR.OUT'
c                   OUTF  = 'FLOOD.OUT'
c                   OUTH  = 'CHEMICAL.OUT'
c                   OUTR  = 'OPERAT.OUT'
                ENDIF
             ENDIF
          ENDIF
      ENDIF

      GO TO 400

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

   80 FORMAT (A8,A1,A2,A1)
  100 FORMAT (//20X,'MANAGEMENT / SENSITIVITY ANALYSIS',
     &/20X,         '=================================',/,
     &/T10,'The options which follow relate to the initial',
     &/T10,'experiment and treatment you selected.  These ',
     &/T10,'default values allow you to validate the simulation',
     &/T10,'results.  To evaluate alternative management strategies'
     &/T10,'or make tactical or strategical decisions, you can',
     &/T10,'modify or change the default values.',/,
     &/T10,'If you choose not to change any of the default values,',
     &/T10,'press the ENTER key in response to the questions.',/,
     &/T10,'Please press < ENTER > to continue    ===> ',$)
  500 FORMAT(5X,'MANAGEMENT / SENSITIVITY ANALYSIS OPTIONS',/,
     &   5X,'=========================================',//,
     &   1X,' 0. RETURN TO THE MAIN MENU',//,
     &   1X,' 1. Simulation Timing .......',1X,A3,1X,I2,1X,I4,/,
!     &   1X,' 2. Crop ....................',1X,A10,10X,A12,1X,A12,/,
     &   1X,' 2. Crop ....................',1X,A16,4X,A12,1X,A12,/,
     &   1X,' 3. Cultivar ................',1X,A16,4X,'MAT :',I2,
     &   5X,A16,/,
     &   1X,' 4. Weather .................',1X,A4,16X,A11,1X,
     &   'WMOD:',A1,/,
     &   1X,' 5. Soil ....................',1X,A10,10X,A5,/,
     &   1X,' 6. Initial Conditions ......',1X,A15,/,
     &   1X,' 7. Planting ................',1X,A3,1X,I2,1X,I4,
     &   9X,'ROW SP:',F4.0,1X,'PLANTS/m2:',F6.2,/,
     &   1X,' 8. Harvest .................',1X,A25,/,
     &   1X,' 9. Water and Irrigation ....',1X,A30,/
     &   1X,'10. Nitrogen ................',1X,A25,5X,A15,/,
     &   1X,'11. Phosphorus ..............',1X,'N/A',/,
     &   1X,'12. Residue .................',1X,A25,/,
     &   1X,'13. Pests and Diseases ......',1X,A40,/,
     &   1X,'14. Field ...................',1X,/,
     &   1X,'15. Crop Process Options ....',1X,'H20:',A1,
     &   1X,'NIT:',A1,1X,'N-FIX:',A1,1X,'PEST:',A1,
     &   1X,'PHOTO:',A1,1X,'WTH:',A1,1X,'ET:',A1)
  550 FORMAT (1X,'16. Output Control ..........',1X,'FREQ:',I2,
     &   ' OVV:',A1,' SUM:',A1,' GROWTH:',A1,' H20:',A1,
     &   ' NIT:',A1,' PEST:',A1)
  575 FORMAT (/,5X,'SELECTION ?  [Default = 0] ===> ',$)
  650 FORMAT (5X,'Option Currently Not Available')

      END SUBROUTINE SENS

C=======================================================================
C  DATEC, Subroutine
C
C  This subroutine resets values when a new weather year is
C  selected interactively
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2. Header revision and minor changes           P.W.W.      2-7-93
C  2. Added switch common block, restructured     P.W.W.      2-7-93
C  3. Added additional variables                  G.H.       04-2-96
C  07/16/2002 CHP Increased number of applications to 200 (NAPPL)
C  05/07/2020 FO  Added new Y4K subroutine call to convert YRDOY
C-----------------------------------------------------------------------
C  INPUT  : FILEW,YRSIM,YRPLT,YEAR,PATHWT,IDLAPL,NIRR,RESDAY,NARES,FDAY,
C           NFERT,HLATE,PWDINF,PWDINL,NHAR,HDATE
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called :
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  PP3    :
C=======================================================================

      SUBROUTINE DATEC (FILEW,YRSIM,YRPLT,YEAR,PATHWT,
     &           IDLAPL,NIRR,RESDAY,NARES,FDAY,NFERT,
     &           HLATE,PWDINF,PWDINL,NHAR,HDATE,
     &           IIRRI,IFERI,IHARI,IRESI,ISWCHE,ISWTIL,
     &           CDATE,TDATE,YRIC,NTIL,NCHEM,NEV,WMDATE)

      USE ModuleDefs
      IMPLICIT NONE
      EXTERNAL ERROR, IGNORE, Y4K_DOY, YR_DOY

      CHARACTER*1  BLANK,IIRRI,IFERI,IHARI,IRESI,ISWTIL,ISWCHE
      CHARACTER*6  ERRKEY
      CHARACTER*12 FILEW
      CHARACTER*80 PATHWT,LINE
      CHARACTER*92 FILEWW

      INTEGER YRSIM,YRPLT,YRDIF,IPLT,ISIM,YR,YEAR,NIRR,NARES,NFERT
      INTEGER FDAY(NAPPL),IDLAPL(NAPPL),RESDAY(NAPPL),I,IDATE
      INTEGER LINWTH,LUNWTH,PATHL,ERRNUM,YEARN,FOUND
      INTEGER HLATE,PWDINF,PWDINL,NHAR,HDATE(3)
      INTEGER IDAYIC,YRIC
      INTEGER TDATE(NAPPL),CDATE(NAPPL),NTIL,NCHEM
      INTEGER YEARNDOY
      INTEGER NEV,WMDATE(NAPPL)

      PARAMETER (ERRKEY = 'DATEC ')
      PARAMETER (LUNWTH = 11 )
      PARAMETER (BLANK = ' ')

      LINWTH = 1
      PATHL  = INDEX (PATHWT,BLANK)

      IF (PATHL .LE. 1) THEN
          FILEWW = FILEW
       ELSE
          FILEWW = PATHWT(1:(PATHL-1)) // FILEW
      ENDIF

      OPEN (LUNWTH,FILE=FILEWW,STATUS='OLD',IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEW,LINWTH)
C
C     Read in weather file header.
C
   20 CONTINUE
      CALL IGNORE (LUNWTH,LINWTH,FOUND,LINE)
      IF (FOUND .EQ. 2) GO TO 20
      IF (FOUND .EQ. 0) CALL ERROR (ERRKEY,-1,FILEW,LINWTH)
      CALL IGNORE (LUNWTH,LINWTH,FOUND,LINE)
      IF (FOUND .NE. 1) CALL ERROR (ERRKEY,-1,FILEW,LINWTH)
      READ (LINE,'(I2)',IOSTAT=ERRNUM) YEARN
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEW,LINWTH-1)
      CLOSE (LUNWTH)

      !Y2K shift for YEARN
      YEARNDOY = YEARN * 1000 + 1
C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY
	    !CALL Y2K_DOY(YEARNDOY)
	    CALL Y4K_DOY(YEARNDOY,FILEWW,LINWTH,ERRKEY,1)
      
      YEARN = (YEARNDOY - 1) / 1000

      YRDIF = YEARN - YEAR
      CALL YR_DOY (YRSIM,YR,ISIM)
      YRSIM = (YR + YRDIF) * 1000 + ISIM
      CALL YR_DOY (YRPLT,YR,IPLT)
      YRPLT = (YR + YRDIF) * 1000 + IPLT
      CALL YR_DOY(YRIC,YR,IDAYIC)
      YRIC  = (YR + YRDIF) * 1000 + IDAYIC
      IF (NIRR .GT. 0 .AND. IIRRI .NE. 'D') THEN
         DO I = 1, NIRR
            IF (IDLAPL(I) .GT. 0) THEN
              CALL YR_DOY (IDLAPL(I),YR,IDATE)
               IDLAPL(I) = (YR + YRDIF) * 1000 + IDATE
            ENDIF
         END DO
      ENDIF
      IF (NFERT .GT. 0 .AND. IFERI .NE. 'D') THEN
         DO I = 1, NFERT
            CALL YR_DOY (FDAY(I),YR,IDATE)
            FDAY(I) = (YR + YRDIF) * 1000 + IDATE
         END DO
      ENDIF
      IF (NARES .GT. 0 .AND. IRESI .NE. 'D') THEN
         DO I = 1, NARES
            CALL YR_DOY (RESDAY(I),YR,IDATE)
            RESDAY(I) = (YR + YRDIF) * 1000 + IDATE
         END DO
      ENDIF
      IF (NHAR .GT. 0 .AND. IHARI .NE. 'D') THEN
         DO I = 1, NHAR
            CALL YR_DOY (HDATE(I),YR,IDATE)
            HDATE(I) = (YR + YRDIF) * 1000 + IDATE
         END DO
      ENDIF
      IF (NTIL .GT. 0 .AND. ISWTIL .NE. 'N') THEN
         DO I = 1, NTIL
            CALL YR_DOY (TDATE(I),YR,IDATE)
            TDATE(I) = (YR + YRDIF) * 1000 + IDATE
         END DO
      ENDIF
      IF (NCHEM .GT. 0 .AND. ISWCHE .NE. 'N') THEN
         DO I = 1, NCHEM
            CALL YR_DOY (CDATE(I),YR,IDATE)
            CDATE(I) = (YR + YRDIF) * 1000 + IDATE
         END DO
      ENDIF
      IF (NEV .GT. 0) THEN
         DO I = 1, NEV
            CALL YR_DOY(WMDATE(I), YR, IDATE)
            WMDATE(I) = (YR + YRDIF) * 1000 + IDATE
         ENDDO
      ENDIF
      CALL YR_DOY (HLATE,YR,IDATE)
      HLATE  = (YR +  YRDIF) * 1000 + IDATE
      CALL YR_DOY (PWDINF,YR,IDATE)
      PWDINF = (YR +  YRDIF) * 1000 + IDATE
      CALL YR_DOY (PWDINL,YR,IDATE)
      PWDINL = (YR +  YRDIF) * 1000 + IDATE
      YEAR = YEARN

      RETURN
      END SUBROUTINE DATEC
