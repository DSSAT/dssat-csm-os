C=======================================================================
C  Aloha_OPHARV, Subroutine
C
C  Write the harvest report
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2. Header revision and minor changes           P.W.W.      2-7-93
C  3. Added switch block, etc.                    P.W.W.      2-7-93
C-----------------------------------------------------------------------
C  INPUT  : TRTNO,YRDOY,YRSIM,YRPLT,CROP,CROPD,
C           WTNSD,NAP,TOTIR,CRAIN,CET,TRUNOF,PESW,TDRAIN,TSON,
C           TSOC,TLCH,NAPNIT,ISDATE,MDATE,
C           YIELD,SKERWT,GPSM,GPP,MAXLAI,PBIOMS,STOVER,XGNP,TOTNUP,APTNUP,
C           GNUP,BIOMAS,CGPE,CLAI,NYRS,FLDNAM,NSENS,EXPER,WTNFX,WTNCAN,
C           TSIN,WTNUP,NREP,AMTNIT,SDWTAM,TITLET,NIRR,STGDOY,CANNAA,
C           CANWAA,XN,ENAME,ROTNO,ROTOPT,CRPNO,BEXIST
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : PINE
C
C  Calls  : OPTAB OPSTRS OPSUM OPBAT
C-----------------------------------------------------------------------

C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE Aloha_OPHARV (TRTNO,YRDOY,YRSIM,YRPLT,CROP,CROPD,
     &   WTNSD,NAP,TOTIR,CRAIN,CET,TRUNOF,PESW,TDRAIN,TSON,TSOC,TLCH,
     &   NAPNIT,ISDATE,MDATE,YIELD,SKERWT,GPSM,GPP,MAXLAI,
     &   PBIOMS,STOVER,XGNP,TOTNUP,APTNUP,GNUP,BIOMAS,
     &   NYRS,FLDNAM,EXPER,WTNFX,WTNCAN,TSIN,WTNUP,NREP,AMTNIT,
     &   SDWTAM,TITLET,STGDOY,ENAME,SDWT,ROTNO,ROTOPT,CRPNO,
     &   SDRATE,TOPWT,AMTRES,HBPC,FBIOM,EYEWT,PMDATE,FHDATE,
     &   WTINITIAL,BWAH,SDWTAH,TSEDM,TRON,TOTPST,H2OLOS,SEPLOS,CHMCOD,
     &   ISENS)

      IMPLICIT     NONE

      INCLUDE     'SWITCH.BLK'

      CHARACTER*1  UPCASE
      CHARACTER*2  CROP
      CHARACTER*5  CHMCOD(10)
      CHARACTER*8  EXPER,FLDNAM
      CHARACTER*10 CROPD
      CHARACTER*25 TITLET
      CHARACTER*60 ENAME

      INTEGER TRTNO,ROTNO,ROTOPT,CRPNO,ISENS
      INTEGER NAPNIT,NAP,ISDATE,PMDATE,FHDATE
      INTEGER YRDOY,YRPLT,YRSIM,YRNR1,YRNR2,YRNR5,YRNR7,DNR1,DNR7
      INTEGER YRNR3,NYRS,MDATE,NREP,STGDOY(20)

      REAL    MAXLAI,ACREFC,TOTNUP,PSDWT,PSPP,HI
      REAL    YIELD,YIELDB,TLCH
      REAL    AMTNIT,WTNCAN,TSON,TSOC,CRESAP
      REAL    TOTIR,CRAIN,CET,TRUNOF,PESW,TDRAIN,WTNSD,WTNFX,WTNUP
      REAL    PBIOMS,SDWTAM,SDWTAH,APTNUP
      REAL    SKERWT,STOVER,TSIN,SEEDNO,SDWT
      REAL    TOPWT,GPP,GPSM,GNUP,BIOMAS,XGNP,FBIOM,EYEWT,PEYEWT
      REAL    SDRATE,AMTRES,HBPC(3),BWAH,WTINITIAL
      REAL    TOTPST(*),H2OLOS(*),SEPLOS(*),TSEDM,TRON

      PARAMETER (ACREFC = 2.47)

      SAVE

C-----------------------------------------------------------------------
C     Calculate variables for output.
C     Update nitrogen and residue applications after routines have been
C     modified to handle automatic management.
C-----------------------------------------------------------------------

      CRESAP = AMTRES
      YRNR1  = ISDATE
      YRNR2  = STGDOY(2)
      YRNR3  = STGDOY(3)
      YRNR5  = STGDOY(5)
      YRNR7  = MDATE
      WTNUP  = TOTNUP/10.0
      WTNCAN = TOTNUP/10.0
      WTNSD  = GNUP  /10.0

      IF (INDEX ('PI',CROP) .EQ. 0) THEN
         TOTNUP = WTNUP
      ENDIF
      PSDWT  = 0.0
      SDRATE = WTINITIAL
      IF (SEEDNO .GT. 0.0 .AND. SDWT  .GE. 0.0) THEN
         PSDWT = SDWT/SEEDNO
      ENDIF
      IF (BIOMAS .GT. 0.0 .AND. YIELD .GE. 0.0) THEN
         HI = YIELD/(BIOMAS*10.0)
       ELSE
         HI = 0.0
      ENDIF

C-----------------------------------------------------------------------
C     Adjust dates since Pineapple grows after harvest
C-----------------------------------------------------------------------

      MDATE = FHDATE

C-----------------------------------------------------------------------
C     Actual yield harvested (default is 100 %)
C-----------------------------------------------------------------------

      SDWT   = YIELD / 10.0
      SDWTAM = YIELD / 10.0
      SDWTAH = SDWT * HPC(1)/100.0
      TOPWT  = BIOMAS

C-----------------------------------------------------------------------
C     Actual byproduct harvested (default is 0 %)
C     Byproduct not harvested is incorporated
C-----------------------------------------------------------------------

      BWAH   = STOVER * HBPC(1)/100.0

C-----------------------------------------------------------------------
C     Call OPTAB for comparing measured and simulated data
C     for yield, yield components and major development stages
C-----------------------------------------------------------------------

      CALL OPTAB (TRTNO,NOUTDO,FILEA,IDETO,RNMODE,IDETS,
     &    CROP,ISDATE,MDATE,YIELD,GPSM,GPP,MAXLAI,PBIOMS,
     &    STOVER,XGNP,TOTNUP,APTNUP,GNUP,BIOMAS,NYRS,
     &    YRSIM,YRPLT,IPLTI,NREP,TITLET,FBIOM,EYEWT,PMDATE,
     &    DNR1,DNR7,ISENS)

C-----------------------------------------------------------------------
C     Resource input and stress summary
C-----------------------------------------------------------------------

      CALL OPSTRS (IDETO,RNMODE,NOUTDO,CROP,NYRS)

C-----------------------------------------------------------------------
C     Crop yield summary report
C-----------------------------------------------------------------------

      IF (CROP .EQ.'PI') THEN
         YIELDB = YIELD/0.8914         ! Fresh fruit yield (lb/acre)
      ELSE
         YIELDB = SDWT*10.0/ACREFC * 2.2046
      ENDIF

      PEYEWT = EYEWT*1000.          ! Eye weight (mg/eye)

      IF (RNMODE .EQ. 'I' .AND. NYRS .LE. 1) THEN
         WRITE (     *,1200) CROPD,NINT(YIELD),NINT(YIELDB),NINT(GPSM),
     &                       NINT(PEYEWT)
      ENDIF

      IF (UPCASE(IDETO) .EQ. 'Y') THEN
         WRITE (NOUTDO,1200) CROPD,NINT(YIELD),NINT(YIELDB),NINT(GPSM),
     &                       NINT(PEYEWT)
      ENDIF

C-----------------------------------------------------------------------
C     Currently not computing N stress in veg and rep stages
C-----------------------------------------------------------------------

C-------------------------------------------------------------------
C     Call Simulation Summary File
C-------------------------------------------------------------------

      IF (IDETS .EQ. 'Y' .OR. IDETS .EQ. 'A') THEN
         CALL OPSUM (NREP,EXPER,CROP,TRTNO,ROTNO,ROTOPT,CRPNO,TITLET,
     &        YRDOY,YRPLT,YRSIM,YRNR1,YRNR7,TOPWT,SEEDNO,PSPP,NAP,
     &        TOTIR,CRAIN,CET,TRUNOF,PESW,TDRAIN,NAPNIT,AMTNIT,WTNUP,
     &        WTNFX,TLCH,TSIN,CRESAP,TSON,TSOC,WTNSD,WTNCAN,PSDWT,
     &        IDETS,NOUTDS,FLDNAM,OUTS,CG,SDWTAM,
     &        GPSM,SKERWT,GPP,ENAME,SDRATE,BWAH,SDWTAH)
c        CALL OPENV (NREP,EXPER,CROP,TRTNO,ROTNO,ROTOPT,CRPNO,TITLET,
c    &        TLCH,IDETS,NOUTDE,FLDNAM,ENAME,OUTE,CG,AMTNIT,TRON,TSEDM,
c    &        TOTPST,H2OLOS,SEPLOS,CHMCOD)
      ELSE
        IF (NREP .EQ. 1) THEN
           OPEN(UNIT = NOUTDS,  FILE = OUTS , STATUS = 'UNKNOWN')
c          OPEN(UNIT = NOUTDE,  FILE = OUTE , STATUS = 'UNKNOWN')
        ENDIF
      ENDIF

C-------------------------------------------------------------------
C
C-------------------------------------------------------------------

      IF (INDEX('NQSA',RNMODE) .GT. 0 .OR. NYRS .GT. 1) THEN
           CALL OPBAT (CROP,NREP,RNMODE,
     &     TRTNO,YRNR1,YRNR7,TOPWT,SDWT,CRAIN,TOTIR,
     &     CET,PESW,WTNUP,TLCH,TSOC,TSON,TSIN,NYRS,DNR1,DNR7)
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------
 1200 FORMAT (/,2X,A10,'Yield (kg/ha)=',I6,' (lb/a)=',I7,1X,
     1       'Eye mý=',I4,' Eye wt.(mg)=',I4)

      END SUBROUTINE Aloha_OPHARV

