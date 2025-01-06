!=======================================================================
! OPHEAD.for
! Includes:
! Module HeaderMod contains header data
! OPHEAD generates general header for OVERVIEW.OUT & daily output files
! OPSOIL generates soil and genetics header for OVERVIEW.OUT
! HEADER writes headers for OVERVIEW.OUT and daily output files 
!-----------------------------------------------------------------------
!  Revision history
!  09/25/2007 CHP Created HeaderMod 
!                 Moved OPSOIL to OPHEAD.for
!                 Moved HEADER to OPHEAD.for
!                 Subroutines rewritten to use array of header data, 
!                   rather than saved ASCII file
!=======================================================================
!     Module to generate header info for output files.
      MODULE HeaderMod
      Use ModuleDefs
      Use ModuleData

        TYPE HeaderType
          INTEGER ICOUNT, ShortCount, RUN
          CHARACTER*120 Header(100)   !header for Overview
        END TYPE
        TYPE (HeaderType) Headers

!       ICOUNT = number of lines in long (OVERVIEW) header
!       ShortCount = number of lines in short (DAILY) header
!       RUN = run number associated with this header
!       Header = array of text lines for short and long headers

        CONTAINS

        SUBROUTINE MULTIRUN(RUN, YRPLT)
!       Updates header for multi-year runs
        IMPLICIT NONE
        EXTERNAL YR_DOY, NAILUJ, LENSTRING

        CHARACTER*3 RMS
        CHARACTER*8 WSTAT
        CHARACTER*11 TEXT
        CHARACTER*120 HEADER2
        INTEGER I, IDYS, IPLT, IPYRS, ISIM, RUN, YRPLT
        INTEGER LenString
        TYPE (ControlType) CONTROL
        CALL GET(CONTROL)

        DO I = 2, Headers%ShortCount
          IF (HEADERS % Header(I)(1:4) .EQ. '*RUN') THEN
!           Update run number
            HEADER2 = HEADERS % Header(I)
            WRITE(HEADERS % Header(I),'(A5,I3,A72)')
     &        HEADER2(1:5), MOD(RUN,1000), HEADER2(9:80)
            HEADERS % RUN = RUN
            EXIT
          ENDIF
        ENDDO

        DO I = 2, Headers%ICOUNT
          IF (HEADERS % Header(I)(1:6) .EQ. ' START') THEN
!           Update simulation start date
            CALL YR_DOY (CONTROL%YRSIM,IPYRS,ISIM)
            CALL NAILUJ (ISIM,IPYRS,RMS,IDYS)
            WRITE(HEADERS%Header(I),400) RMS,IDYS,IPYRS
  400       FORMAT (1X,'STARTING DATE  :',1X,A3,1X,I2,1X,I4)
            EXIT
          ENDIF
        ENDDO

        IF (YRPLT > 0) THEN
          DO I = 2, Headers%ICOUNT
            IF (HEADERS % Header(I)(1:6) .EQ. ' PLANT') THEN
!             Update planting date
              CALL YR_DOY (YRPLT,IPYRS,IPLT)
              CALL NAILUJ (IPLT,IPYRS,RMS,IDYS)
              WRITE(TEXT,'(A3,1X,I2,1X,I4)') RMS,IDYS,IPYRS
              HEADERS%Header(I)(19:29) = TEXT
              HEADERS%Header(I)(30:36) = '       '
              EXIT
            ENDIF
          ENDDO
        ENDIF

        DO I = 2, Headers%ICOUNT
          IF (HEADERS % Header(I)(1:6) .EQ. ' WEATH') THEN
!           Update WEATHER file
            CALL GET("WEATHER", "WSTA", WSTAT)
            IF (LenString(WSTAT) > 0) THEN
             WRITE(HEADERS%HEADER(I),"(1X,'WEATHER',8X,':',1X,A8)")WSTAT
            ENDIF
            EXIT
          ENDIF
        ENDDO

        RETURN
        END SUBROUTINE MULTIRUN

      END MODULE HeaderMod
!=======================================================================


C=======================================================================
C  OPHEAD, Subroutine
C
C  Prints inputs for the simulation to the screen
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1993 GH  Written
C  05/28/1993 PWW Header revision and minor changes
C  05/28/1993 PWW Added switch block, etc.
C  04/01/1996 GH  Added comsoi.blk and comibs.blk
C  12/11/2000 GH  Minor output fixes due to 80 character stringlimit
C  01/29/2002 CHP OPHEAD writes to generic HEADER.OUT file for use
C                   by model output routines.
C  08/19/2002 GH  Modified for Y2K
C  05/08/2003 CHP Added version information and date-time stamp to header
!  09/12/2007 CHP Save to array instead of write to file.
C
C-----------------------------------------------------------------------
C  INPUT  : WSTA,PEDON,TRTNO,CUMDEP,NFERT,TOTNAP,NARES,RESAMT,NAPW,TOTAPW,
C           SLDESC,TPESW,ROWSPC,PLTPOP,VRNAME,AINO3,AINH4,YEAR,SLTX,LUNOV,
C           YRSIM,YRPLT,SOILNX,DSOILN,SOILNC,ECONAM,RUN,MODEL,
C           EXPER,CROP,CROPD,DSOIL,THETAC,TITLET
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : OPDAY
C
C  Calls  : YR_DOY NAILUJ WTHSUM
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE OPHEAD (DYNAMIC, LUNOV,CUMDEP,TPESW,VRNAME,AINO3,AINH4,
     &     ECONO,RUN,MODEL,TITLET,WTHSTR, RNMODE,
     &     CONTROL, ISWITCH, UseSimCtr, PATHEX)

      USE ModuleDefs
      USE HeaderMod
      IMPLICIT NONE
      EXTERNAL LENSTRING, NAILUJ, YR_DOY
      SAVE

      INCLUDE 'COMSWI.blk'
      INCLUDE 'COMSOI.blk'
      INCLUDE 'COMIBS.blk'

      CHARACTER*1   RNMODE
      CHARACTER*3   RMP,RMS
      CHARACTER*6   ECONO
      CHARACTER*8   MODEL
      CHARACTER*16  VRNAME
      CHARACTER*25  TITLET
      CHARACTER*80  PATHEX
      CHARACTER*120 HEADER(100) !Simulation header
      CHARACTER*120 WTHSTR

      INTEGER       DYNAMIC
      INTEGER       ICOUNT, I, IDYP,IDYS,IPYRP,IPYRS,NNFERT
      INTEGER       NNAPW,ISIM,IPLT,LUNOV,RUN
      INTEGER       SimLen, LenString

      REAL          AINH4,AINO3,CUMDEP,TPESW

c     MJ, Mar 2008: added HDATE_YR and HDATE_DOY
      INTEGER HDATE_YR, HDATE_DOY

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH

      INTEGER DATE_TIME(8)
      LOGICAL UseSimCtr

!     Write header to console when LUNOV=6
      IF (LUNOV == 6) THEN
        DO I = 1, HEADERS%ICOUNT
          WRITE(LUNOV,'(A)') HEADER(I)
        ENDDO 
        RETURN
      ENDIF

	 IF (TITLER(1:5) .EQ. '     ') THEN
         TITLER = TITLET
       ENDIF
	
!     ******************************************************************
!     ******************************************************************
!     Generate short header 
      IF (DYNAMIC == RUNINIT) THEN
!     ------------------------------------------------------------------
      CALL DATE_AND_TIME (VALUES=DATE_TIME)
!      date_time(1)  The 4-digit year  
!      date_time(2)  The month of the year  
!      date_time(3)  The day of the month  
!      date_time(4)  The time difference with respect to Coordinated Universal Time (UTC) in minutes  
!      date_time(5)  The hour of the day (range 0 to 23) - local time  
!      date_time(6)  The minutes of the hour (range 0 to 59) - local time  
!      date_time(7)  The seconds of the minute (range 0 to 59) - local time  
!      date_time(8)  The milliseconds of the second (range 0 to 999) - local time  
      
!     Version information stored in ModuleDefs.for
      WRITE (HEADER(1),100) Version, VBranch, MonthTxt(DATE_TIME(2)),
     &    DATE_TIME(3), DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), 
     &    DATE_TIME(7)
  100 FORMAT ("*DSSAT Cropping System Model Ver. ",I1,".",I1,".",I1,
     &    ".",I3.3,1X,A,2X,
     &    A3," ",I2.2,", ",I4,I3.2,":",I2.2,":",I2.2)
      WRITE (HEADER(2),'(" ")')
!     WRITE (HEADER(3),200) MOD(RUN,1000),TITLET
!     WRITE (HEADER(3),201) MOD(RUN,1000),TITLET,MODEL

      WRITE (HEADER(3),201) MOD(RUN,1000),TITLET,MODEL,EXPER,TRTNO
  201 FORMAT ('*RUN ',I3,8X,': ',A25,1X,A8,1X,A8,I5)

!      IF (TRTNO.LT.10) THEN
!        WRITE (HEADER(3),201) MOD(RUN,1000),TITLET,MODEL
!      ELSEIF (TRTNO.GE.10.AND.TRTNO.LT.100) THEN
!        WRITE (HEADER(3),202) MOD(RUN,1000),TRTNO,TITLET,EXPER,MODEL
!      ELSEIF (TRTNO.GE.100.AND.TRTNO.LT.1000) THEN
!        WRITE (HEADER(3),203) MOD(RUN,1000),TRTNO,TITLET,EXPER,MODEL
!      ELSE
!        WRITE (HEADER(3),204) MOD(RUN,1000),TRTNO,TITLET,EXPER,MODEL
!      ENDIF
!  201 FORMAT ('*RUN ',I3,2X,'Tr ',I1,'  : ',A25,1X,A8,1X,A8)
!  202 FORMAT ('*RUN ',I3,2X,'Tr ',I2,' : ',A25,1X,A8,1X,A8)
!  203 FORMAT ('*RUN ',I3,2X,'Tr ',I3,': ',A25,1X,A8,1X,A8)
!  204 FORMAT ('*RUN ',I3,2X,'Tr',I4,': ',A25,1X,A8,1X,A8)

      I = INDEX(MODEL," ")
      IF (I < 1) THEN
        WRITE (HEADER(4),300) MODEL(1:8),CROPD
      ELSE
        WRITE(HEADER(4),*) " "
      ENDIF
      WRITE (HEADER(5),310) EXPER,CG,ENAME(1:50)
      WRITE (HEADER(6),312) PATHEX(1:62)
  312 FORMAT (1X,'DATA PATH ',5X,':',1X,A)
      IF (INDEX('FQ',RNMODE) > 0) THEN
        WRITE (HEADER(7),320) MOD(CONTROL%ROTNUM,1000),TITLET, MODEL
      ELSE
        WRITE (HEADER(7),320) MOD(TRTNO,1000),TITLET, MODEL
      ENDIF
      WRITE (HEADER(8),'(" ")')
      I = 9; HEADERS%ShortCount = I-2
      HEADERS % ICOUNT  = I-1
      HEADERS % HEADER = HEADER
      HEADERS % RUN    = RUN

!     ******************************************************************
!     ******************************************************************
      ELSE
!     Continue with long header
!     ------------------------------------------------------------------
      ICO2   = ISWITCH % ICO2
      IFERI  = ISWITCH % IFERI
      IHARI  = ISWITCH % IHARI
      IIRRI  = ISWITCH % IIRRI
      IPLTI  = ISWITCH % IPLTI
      IRESI  = ISWITCH % IRESI
      ISWNIT = ISWITCH % ISWNIT
      ISWPHO = ISWITCH % ISWPHO
      ISWSYM = ISWITCH % ISWSYM
      ISWTIL = ISWITCH % ISWTIL
      ISWWAT = ISWITCH % ISWWAT
      MEEVP  = ISWITCH % MEEVP
      MEHYD  = ISWITCH % MEHYD
      MEINF  = ISWITCH % MEINF
      MEPHO  = ISWITCH % MEPHO
      MESEV  = ISWITCH % MESEV
      MESOL  = ISWITCH % MESOL
      METMP  = ISWITCH % METMP
      MEWTH  = ISWITCH % MEWTH
      NSWITCH= ISWITCH % NSWI

      IF (UseSimCtr) THEN
        SimLen = LenString(CONTROL % SimControl)
        WRITE (HEADER(I),1105); I=I+1
        WRITE (HEADER(I),1106) CONTROL % SimControl(1:SimLen); I=I+1
        WRITE (HEADER(I),1107); I=I+1; ; HEADERS%ShortCount = I-1
 1105   FORMAT("!Simulation control file used for this simulation.")
 1106   FORMAT("!File: ",A)
 1107   FORMAT("!See top of WARNING.OUT file for specific controls.")
      ENDIF

      WRITE (HEADER(I),'(" ")'); I=I+1
      WRITE (HEADER(I),330) CROPD,VRNAME,ECONO; I=I+1

      CALL YR_DOY (CONTROL%YRSIM,IPYRS,ISIM)
      CALL NAILUJ (ISIM,IPYRS,RMS,IDYS)
      WRITE (HEADER(I),400) RMS,IDYS,IPYRS; I=I+1
      CALL YR_DOY (YRPLT,IPYRP,IPLT)

c     MJ, Mar 2008: output the line with planting date, density and rowspacing
      IF (IPLT .LE. 366 .AND. IPLTI .EQ. 'R' ) THEN
         CALL NAILUJ (IPLT,IPYRP,RMP,IDYP)
         WRITE (HEADER(I),450) RMP,IDYP,IPYRP,PLTPOP,ROWSPC; I=I+1
       ELSE
         WRITE (HEADER(I),475) PLTPOP,ROWSPC; I=I+1
      ENDIF

c     MJ, Mar 2008: output harvest date in HEADER.OUT
c     (OVERVIEW.OUT).  Only do this if harvest is on a 
c     'reported date'.
c     IHARI is harvest management option 
c     (e.g. 'R' = 'On reported date', 'M' = 'At maturity')
c     HDATE(1) is harvest date (YYYYDOY)
c     ::::::::::::::::::::::::::::::::::
      IF (IHARI  .EQ. 'R' .AND. INDEX('FQNY',RNMODE)<1) THEN

c         Get Day Of Year (DOY) date
          HDATE_YR = HDATE(1)/1000
          HDATE_DOY = HDATE(1) - (HDATE_YR*1000)
          CALL NAILUJ (HDATE_DOY,HDATE_YR,RMS,IDYS)

c         Write to output
          WRITE (HEADER(I),425) RMS,IDYS,HDATE_YR; I=I+1
      ENDIF
c     ::::::::::::::::::::::::::::::::::

c     MJ, Mar 2008: weather station and year:
      WRITE (HEADER(I),500) WSTA, YEAR; I=I+1

c     MJ, Mar 2008: Soil information
      WRITE (HEADER(I),600) PEDON,SLTX,SLDESC; I=I+1

      IF (ISWWAT .NE. 'N') THEN
!        CHP 08/12/2005 Don't report initial conditions for
!             sequenced runs.
         IF (INDEX('FQ',RNMODE) .LE. 0 .OR. RUN == 1) THEN
           WRITE (HEADER(I),625) NINT(CUMDEP),TPESW*10,AINO3,AINH4
           I=I+1
         ENDIF

         IF (IIRRI .EQ. 'R' .OR. IIRRI .EQ. 'D') THEN
            IF (IIRRI .EQ. 'R') THEN
               WRITE (HEADER(I),650)
             ELSE IF (IIRRI .EQ. 'D') THEN
               WRITE (HEADER(I),655)
            ENDIF
            I=I+1
            IF (TOTAPW .EQ. 0 .AND. NAPW .GE. 1) THEN
               NNAPW = NAPW
             ELSE
               NNAPW = NAPW
            ENDIF
            WRITE (HEADER(I),660) NINT(TOTAPW),NNAPW; I=I+1
          ELSE IF (IIRRI .EQ. 'A') THEN
            WRITE (HEADER(I),665); I=I+1
            WRITE (HEADER(I),666) DSOIL/100.,THETAC; I=I+1
          ELSE IF (IIRRI .EQ. 'F') THEN
            WRITE (HEADER(I),670); I=I+1
            WRITE (HEADER(I),666) DSOIL/100.,THETAC; I=I+1
          ELSE IF (IIRRI .EQ. 'E') THEN
            WRITE (HEADER(I),675); I=I+1
            WRITE (HEADER(I),676) DSOIL; I=I+1
          ELSE IF (IIRRI .EQ. 'T') THEN
            WRITE (HEADER(I),680); I=I+1
            WRITE (HEADER(I),676) DSOIL; I=I+1
          ELSE IF (IIRRI .EQ. 'N') THEN
            WRITE (HEADER(I),690); I=I+1
            WRITE (HEADER(I),691); I=I+1
         ENDIF

  660 FORMAT(' IRRIGATION     : ',I8,' mm IN ',I5,' APPLICATIONS')
  665 FORMAT(' WATER BALANCE  : AUTOMATIC IRRIGATION - REFILL PROFILE')
  666 FORMAT(' IRRIGATION     : AUTOMATIC [SOIL DEPTH:',F5.2,' m',1X,
     &            F3.0,'%]')
  670 FORMAT(' WATER BALANCE  : AUTOMATIC IRRIGATION - FIXED AMOUNT')
  675 FORMAT(' WATER BALANCE  : ET AUTO IRRIGATION - REFILL PROFILE')
  676 FORMAT(' IRRIGATION     : AUTOMATIC [ET ACCUM:',F5.2,' mm ]')
  680 FORMAT(' WATER BALANCE  : ET AUTOMATIC IRRIGATION - FIXED AMOUNT')
  690 FORMAT(' WATER BALANCE  : RAINFED')
  691 FORMAT(' IRRIGATION     : NOT IRRIGATED')

      ELSE IF (ISWWAT .EQ. 'N') THEN
         WRITE (HEADER(I),705); I=I+1
         WRITE (HEADER(I),710); I=I+1
      ENDIF



      IF (ISWNIT .EQ. 'Y') THEN
         IF (ISWSYM .EQ. 'Y') THEN
            WRITE (HEADER(I),720); I=I+1
          ELSE IF (ISWSYM .EQ. 'U') THEN
            WRITE (HEADER(I),730); I=I+1
          ELSE IF (ISWSYM .EQ. 'N') THEN
            WRITE (HEADER(I),740); I=I+1
         ENDIF
       ELSE
         WRITE (HEADER(I),750); I=I+1
         WRITE (HEADER(I),751); I=I+1
         WRITE (HEADER(I),752); I=I+1
      ENDIF

      IF (ISWNIT .EQ. 'Y') THEN
         IF (IFERI .EQ. 'R' .OR. IFERI .EQ. 'D') THEN
            IF (TOTNAP .EQ. 0 .AND. NFERT .GE. 1) THEN
               NNFERT = 0
             ELSE
               NNFERT = NFERT
            ENDIF
            WRITE (HEADER(I),800) NINT(TOTNAP),NNFERT; I=I+1
!         10/14/2008 CHP added "F" option
          ELSE IF (IFERI .EQ. 'A' .OR. IFERI .EQ. 'F') THEN
            WRITE (HEADER(I),810) SOILNX,DSOILN,SOILNC; I=I+1
          ELSE IF (IFERI .EQ. 'N') THEN
            WRITE (HEADER(I),820); I=I+1
         ENDIF
         WRITE (HEADER(I),1000) NINT(ICRES),NINT(RESAMT),NARES; I=I+1
      ENDIF

      IF (LenString(WTHSTR) > 1) THEN
        WRITE (HEADER(I),1200) WTHSTR(1:60); I=I+1
        WRITE (HEADER(I),1210) WTHSTR(61:120); I=I+1
      ENDIF
      WRITE (HEADER(I),1300) ISWWAT,ISWNIT,ISWSYM,ISWPHO,ISWDIS; I=I+1
      WRITE (HEADER(I),1350) MEPHO,MEEVP,MEINF,MEHYD,MESOM; I=I+1
      WRITE (HEADER(I),1355) ICO2, NSWITCH, MESEV, MESOL, METMP; I=I+1
      WRITE (HEADER(I),1400) IPLTI,IIRRI,IFERI,IRESI,IHARI; I=I+1
      WRITE (HEADER(I),1405) MEWTH, ISWTIL; I=I+1


      ICOUNT = I-1
      Headers % ICOUNT  = ICOUNT
      Headers % Header = HEADER
      Headers % RUN    = RUN

!     ******************************************************************
!     ******************************************************************
      ENDIF
C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

  200 FORMAT ('*RUN ',I3,8X,': ',A25)
! 201 FORMAT ('*RUN ',I3,8X,': ',A25,1X,A8)

  300 FORMAT (1X,'MODEL',10X,':',1X,A8,' - ',A16)
  310 FORMAT (1X,'EXPERIMENT',5X,':',1X,A8,1X,A2,1X,A50)
  320 FORMAT (1X,'TREATMENT',I3, 3X,':',1X,A25,1X,A8)
  330 FORMAT (1X,'CROP',11X,':',1X,A16,1X,'CULTIVAR :',A17,1X,
     &        'ECOTYPE :',A6)

  400 FORMAT (1X,'STARTING DATE  :',1X,A3,1X,I2,1X,I4)
  425 FORMAT (1X,'HARVEST DATE   :',1X,A3,1X,I2,1X,I4)
  450 FORMAT (1X,'PLANTING DATE  :',1X,A3,1X,I2,1X,I4,6X,
     &       'PLANTS/m2 :',F8.1,4X,'ROW SPACING :',F5.0,'cm ')
  475 FORMAT (1X,'PLANTING DATE  :',1X,'AUTOMATIC PLANTING',1X,
     &       'PLANTS/m2 :',F5.1,5X,'ROW SPACING :',F5.0,'cm ')
  500 FORMAT (1X,'WEATHER',8X,':',1X,A4,3X,I4)
  600 FORMAT (1X,'SOIL',11X,':',1X,A10,5X,'TEXTURE : ',A5,' - ',A25)
  625 FORMAT (1X,'SOIL INIT COND ',':',1X,'DEPTH:',I3,'cm',1X,
     &     'EXTR. H2O:',F5.1,'mm  NO3:',F5.1,'kg/ha  NH4:',F5.1,'kg/ha')
  650 FORMAT (1X,'WATER BALANCE',2X,':',1X,'IRRIGATE ON',
     &           ' REPORTED DATE(S)')
  655 FORMAT (1X,'WATER BALANCE',2X,':',1X,'IRRIGATE ON REPORTED',
     &           ' DAP')
  705 FORMAT (1X,'WATER BALANCE',2X,':',1X,'NOT SIMULATED ;',
     &           ' NO H2O-STRESS')
  710 FORMAT (1X,'IRRIGATION',5X,':')
  720 FORMAT (1X,'NITROGEN BAL.',2X,':',1X,
     &           'SOIL-N, N-UPTAKE & DYNAMIC N-FIXATION SIMULATION')
  730 FORMAT (1X,'NITROGEN BAL.',2X,':',1X,
     &           'SOIL-N, N-UPTAKE & UNLIMITED N-FIXATION SIMULATION')
  740 FORMAT (1X,'NITROGEN BAL.',2X,':',1X,
     &           'SOIL-N & N-UPTAKE SIMULATION; NO N-FIXATION')
  750 FORMAT (1X,'NITROGEN BAL.',2X,':',1X,
     &           'NOT SIMULATED ; NO N-STRESS')
  751 FORMAT (1X,'N-FERTILIZER',3X,':')
  752 FORMAT (1X,'RESIDUE/MANURE',1X,':')
  800 FORMAT (1X,'N-FERTILIZER',3X,':',1X,I8,' kg/ha IN ',I5,
     &           ' APPLICATIONS')
  810 FORMAT (1X,'N-FERTILIZER',3X,':',1X,'AUTO APPLICATIONS ',F5.0,
     &           ' kg/ha AT ',F6.2,' cm AND',F5.2,' % STRESS')
  820 FORMAT (1X,'N-FERTILIZER',3X,':',1X,'NO N-FERTILIZER APPLIED')
 1000 FORMAT (1X,'RESIDUE/MANURE',1X,':',1X,'INITIAL : ',I5,' kg/ha ;',
     &        I8,' kg/ha IN ',I5,
     &           ' APPLICATIONS')

 1200 FORMAT (1X,'ENVIRONM. OPT. :',1X,A60)
 1210 FORMAT (18X,A60)

 1300 FORMAT (1X,'SIMULATION OPT : WATER',3X,':',A1,2X,'NITROGEN:',A1,
     & 2X,'N-FIX:',A1,2X,'PHOSPH :',A1,2X,'PESTS  :',A1)
 1350 FORMAT (18X,'PHOTO',3X,':',A1,2X,'ET      :',A1,
     & 2X,'INFIL:',A1,2X,'HYDROL :',A1,2X,'SOM    :',A1)
 1355 FORMAT (18X,'CO2  ',3X,':',A1,2X,'NSWIT   :',I1,
     & 2X,'EVAP :',A1,2X,'SOIL   :',A1,2X,'STEMP  :',A1)

 1400 FORMAT (1X,'MANAGEMENT OPT : PLANTING:',A1,2X,'IRRIG',3X,':',A1,
     & 2X,'FERT :',A1,2X,'RESIDUE:',A1,2X,'HARVEST:',A1,2X)
 1405 FORMAT (18X,'WEATHER :',A1,2X,'TILLAGE :',A1)

      RETURN
      END SUBROUTINE OPHEAD
C=======================================================================

C=======================================================================
C  OPSOIL, Subroutine

C  Generates output for soil data
C-----------------------------------------------------------------------
C  Revision history

C  01/01/1990 GH  Written
C  05/28/1999 PWW Header revision and minor changes 
C  03/11/2005 GH  Remove ANS, RNMODE and NYRS
!  02/06/2007 CHP Added alternate sugarcane parameters for CASUPRO
!  11/26/2007 CHP THRESH, SDPRO, SDLIP moved from eco to cul file
C  08/09/2012 GH  Updated for cassava
C  09/18/2020 GH  Update for quinoa, safflower, sunflower
C  07/08/2022 GH  Update for cucumber
C-----------------------------------------------------------------------
C  INPUT  : IDETO,NOUTDO,NYRS,LL,DUL,SAT,DLAYR,SWINIT,DS,NLAYR,ESW
C           SHF,BD,PH,INO3,INH4,OC,TLL,TDUL,TSAT,TPESW,TSWINI,AINO3,AINH4
C           TSOC,SWCON,U,SALB,CN2,CROPD,VRNAME,VARTY,SLPF,ECONO
C           SLNF,LUNOV,CROP

C  LOCAL  :

C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : OPIBS3
C
C  Calls  : CLEAR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE OPSOIL (LL,DUL,SAT,
     &   DLAYR,SWINIT,DS,NLAYR,ESW,SHF,BD,PH,INO3,INH4,OC,
     &   TLL,TDUL,TSAT,TPESW,TSWINI,AINO3,AINH4,TSOC,
     &   SWCON,U,SALB,CN2,CROPD,VRNAME,VARTY,SLPF,
     &   ECONO,SLNF,CROP, RNMODE, RUN, MODEL, ISWITCH, ATLINE)

      USE ModuleDefs
      USE HeaderMod
      IMPLICIT NONE
      EXTERNAL LenString

      INCLUDE 'COMGEN.blk'

      CHARACTER*1  ISWWAT, RNMODE
      CHARACTER*2  CROP
      CHARACTER*6  VARTY,ECONO
      CHARACTER*8  MODEL
      CHARACTER*16 CROPD
      CHARACTER*16 VRNAME
      CHARACTER*120  HEADER(100) !Simulation header
      CHARACTER*1000 ATLINE

      INTEGER      NLAYR,I,L, RUN
      INTEGER      J, J1, J2, LENGTH, LENSTRING

      REAL         LL(NL),DUL(NL),SAT(NL),DLAYR(NL),DS(NL),SWINIT(NL)
      REAL         ESW(NL),SHF(NL),BD(NL),PH(NL),INO3(NL),INH4(NL)
      REAL         OC(NL),TLL,TDUL,TSAT,TPESW,TSWINI,AINO3,AINH4,TSOC
      REAL         SWCON,U,SALB,CN2,SLPF,SLNF

      TYPE (SwitchType) ISWITCH

      I = HEADERS % ICOUNT + 1
      HEADER = HEADERS % Header

      ISWWAT = ISWITCH % ISWWAT

!=======================================================================
!     SOILS
!-----------------------------------------------------------------------
!      CHP 08/12/2005 Don't report initial conditions for
!           sequenced runs.
      IF (INDEX('FQ',RNMODE) .LE. 0 .OR. RUN == 1) THEN

        WRITE (HEADER(I),'(" ")'); I=I+1
        WRITE (HEADER(I),300); I=I+1
  300   FORMAT ('*SUMMARY OF SOIL AND GENETIC INPUT PARAMETERS')
        WRITE (HEADER(I),'(" ")'); I=I+1
!-----------------------------------------------------------------------
!       Write soils info
        IF (ISWWAT .NE. 'N') THEN
          WRITE(HEADER(I),360); I=I+1
          WRITE(HEADER(I),361); I=I+1
          WRITE(HEADER(I),362); I=I+1
          WRITE(HEADER(I),363); I=I+1

          DO L = 1, NLAYR
            WRITE (HEADER(I),410) NINT(DS(L)-DLAYR(L)),NINT(DS(L)),
     &        LL(L),DUL(L),SAT(L),ESW(L),SWINIT(L),SHF(L),BD(L),
     &        PH(L),INO3(L),INH4(L),OC(L)
            I=I+1
          ENDDO
          WRITE (HEADER(I),'(" ")') ; I=I+1
          WRITE (HEADER(I),610) NINT(DS(NLAYR)),TLL,TDUL,TSAT,TPESW,
     &                        TSWINI,AINO3,AINH4,NINT(TSOC); I=I+1
          WRITE (HEADER(I),710) SALB,U,SLNF; I=I+1
          WRITE (HEADER(I),711) CN2,SWCON,SLPF; I=I+1
          WRITE (HEADER(I),'(" ")') ; I=I+1
        ENDIF

      ELSE
        WRITE(HEADER(I),310); I=I+1
  310   FORMAT ('*SUMMARY OF GENETIC INPUT PARAMETERS')
        WRITE(HEADER(I),'(" ")'); I=I+1
      ENDIF

!=======================================================================
!     GENOTYPE
!-----------------------------------------------------------------------
!     Write genetic coefficients
      WRITE (HEADER(I),800) CROPD(1:16),VARTY,VRNAME,ECONO; I=I+1

      SELECT CASE (MODEL(1:5))

!-----------------------------------------------------------------------
!     CROPGRO
      CASE ('CRGRO','PRFRM')
!      IF (INDEX (MODEL, 'CRGRO') > 0) THEN
        IF (INDEX ('BG,BN,CH,CP,FB,GB,LT,PE,PN,PP,SB,VB',CROP) 
     &    > 0) THEN
           WRITE (HEADER(I), 850) CSDVAR,PPSEN,PH2T5,
     &                        PHTHRS(8),PHTHRS(10); I=I+1
           WRITE (HEADER(I),851) WTPSD,SDPDVR,SFDUR,PODUR,XFRUIT; I=I+1

        ELSEIF (INDEX ('AM,BC,BH,BM,BR,CB,CI,CN,CO,CU,GY,NP,PR,QU,
     &     SF,SR,SU,TM',CROP) .GT. 0) THEN
           WRITE (HEADER(I), 850) CSDVAR,PPSEN,PH2T5,
     &                        PHTHRS(8),PHTHRS(10); I=I+1
           WRITE (HEADER(I),852) WTPSD,SDPDVR,SFDUR,PODUR,XFRUIT; I=I+1
        ENDIF

        WRITE (HEADER(I),853) THRESH, SDPRO, SDLIP; I=I+1

!-----------------------------------------------------------------------
!     CSCER - Wheat, barley
      CASE ('CSCER')
        WRITE (HEADER(I),'(A)')
     &    "   P1V   P1D    P5    G1    G2    G3 PHINT"
        I=I+1
        WRITE (HEADER(I),'(5(1X,F5.1),1X,F5.2,1X,F5.1)') 
     &    P1V,P1D,P5,G1,G2,G3,PHINT
        I=I+1

!       Print optional extra stuff from ecotype file
        LENGTH = LenString(PLAINTXT)
        IF (LENGTH > 0) THEN
          DO J=1,5
            J1 = (J-1)*78+1
            J2 = J1 + 77
            WRITE( HEADER(I),'(A)') TRIM(ATLINE(J1+29:J2+29))
            WRITE( HEADER(I+1),'(A)') TRIM(PLAINTXT(J1:J2))
            I = I + 2
            IF (J2 > LENGTH) EXIT
          ENDDO
        ENDIF

!-----------------------------------------------------------------------
!     CSCRP - Wheat, barley, cassava
      CASE ('CSCRP')
!       -----------------------------------
!       wheat, barley
        IF (INDEX('WH,BA',CROP) > 0) THEN
          WRITE (HEADER(I),'(A,A)')
     &      "  VREQ VBASE  VEFF  PPS1  PPS2",
     &      "    P1    P2    P3    P4    P5    P6    P7    P8"
          I=I+1
          WRITE (HEADER(I),'(2F6.1,2F6.2,9F6.1)') 
     &      VREQ, VBASE, VEFF, PPS1, PPS2, 
     &      P1, P2, P3, P4, P5, P6, P7, P8
          I=I+1
          WRITE (HEADER(I),'(A)')
     &      " GNOWT  GWTS SHWTS PHINT  LA1S  LAFV  LAFR"
          I=I+1
          WRITE (HEADER(I),'(2F6.1, F6.2,F6.1, 3F6.2)') 
     &      GNOWT, GWTS, SHWTS, PHINT, LA1S, LAFV, LAFR 
          I=I+1
!       -----------------------------------
!       cassava
        ELSEIF (INDEX('CS',CROP) > 0) THEN
          WRITE (HEADER(I),'(A,A)')
     &      "  PPS1   P1L   P2L   P4L   P5L SRNOW  SRFR"
C    &      "  LA1S  LAXS"
C    &      " LAXNO  LAFS LAFNO  LAWS"
          I=I+1
c          WRITE (HEADER(I),'(4F6.1,F6.0,2F6.2,6F6.0)') 
c     &      PPS1, P1L, P2L, P4L, P5L, SRNOW, SRFR, LA1S, LAXS, 
c     &      LAXNO, LAFS, LAFNO, LAWS
          I=I+1
          WRITE (HEADER(I),'(A)') " PHINT LLIFA  STFR"
          I=I+1
c          WRITE (HEADER(I),'(2F6.0,F6.2)') PHINT, LLIFA, STFR
          I=I+1
        ENDIF

!       Print optional extra stuff from ecotype file
        LENGTH = LenString(PLAINTXT)
        IF (LENGTH > 0) THEN
          DO J=1,3
            J1 = (J-1)*78+1
            J2 = J1 + 77
            WRITE( HEADER(I),'(A)') TRIM(ATLINE(J1+149:J2+149))
            WRITE( HEADER(I+1),'(A)') TRIM(PLAINTXT(J1:J2))
            I = I + 2
            IF (J2 > LENGTH) EXIT
          ENDDO
        ENDIF
!-----------------------------------------------------------------------
!     Cassava       
      CASE ('CSCAS')
         WRITE (HEADER(I),'(A,A)')
     &     "  PPS1 B01ND B12ND B23ND B34ND B45ND B56ND",
     &     " SR#WT  SRFR  HMPC PHINT"
         I=I+1
          WRITE (HEADER(I),'(F6.2,6F6.1,4F6.2)') 
     &     PPS1, B01ND, B12ND, B23ND, B34ND, B45ND, B56ND,
     &     SRNWT, SRFR, HMPC, PHINT
         I=I+1
         WRITE (HEADER(I),'(A,A)') 
     &     "  LA1S  LAXS LAXND LAXN2  LAFS LAFND  SLAS",
     &     " LLIFA LPEFR  STFR"
        I=I+1
         WRITE (HEADER(I),'(F6.1,F6.0,6F6.1,2F6.2)') 
     &    LA1S, LAXS, LAXND, LAXN2, LAFS, LAFND, SLASS, LLIFA,
     &    LPEFR, STFR
       I=I+1
!       Print optional extra stuff from ecotype file
        LENGTH = LenString(PLAINTXT)
        IF (LENGTH > 0) THEN
          DO J=1,3
            J1 = (J-1)*78+1
            J2 = J1 + 77
            WRITE( HEADER(I),'(A)') TRIM(ATLINE(J1+149:J2+149))
            WRITE( HEADER(I+1),'(A)') TRIM(PLAINTXT(J1:J2))
            I = I + 2
            IF (J2 > LENGTH) EXIT
          ENDDO
        ENDIF
!-----------------------------------------------------------------------
!     Cassava CIAT      
      CASE ('CSYCA')
          WRITE (HEADER(I),'(A,A)')
     &     "  B01ND B12ND B23ND B34ND BR1FX BR2FX BR3FX BR4FX "
          I=I+1
          WRITE (HEADER(I),'(4F6.0,4F6.2)') 
     &     B01ND, B12ND, B23ND, B34ND, BR1FX, BR2FX, BR3FX, BR4FX
         I=I+1
         WRITE (HEADER(I),'(A,A)') 
     &     "  LAXS  SLAS",
     &     " LLIFA LPEFR LNSLP NODWT NODLT"
        I=I+1
         WRITE (HEADER(I),'(F6.1,F6.0,2F6.1,3F6.2, 1F6.1)') 
     &    LAXS, SLASS, LLIFA,
     &    LPEFR, LNSLP, NODWT, NODLT
       I=I+1
!       Print optional extra stuff from ecotype file
        LENGTH = LenString(PLAINTXT)
        IF (LENGTH > 0) THEN
          DO J=1,3
            J1 = (J-1)*78+1
            J2 = J1 + 77
            WRITE( HEADER(I),'(A)') TRIM(ATLINE(J1+149:J2+149))
            WRITE( HEADER(I+1),'(A)') TRIM(PLAINTXT(J1:J2))
            I = I + 2
            IF (J2 > LENGTH) EXIT
          ENDDO
        ENDIF

!-----------------------------------------------------------------------
!     CERES-Maize
      CASE ('MZCER')
            WRITE (HEADER(I),900) P1,P2,P5; I=I+1
            WRITE (HEADER(I),901) G2,G3,PHINT; I=I+1

!-----------------------------------------------------------------------
!     IXIM-Maize
      CASE ('MZIXM')
             WRITE (HEADER(I),915) P1,P2,P5,AX; I=I+1
             WRITE (HEADER(I),916) G2,G3,PHINT,LX; I=I+1
             
!-----------------------------------------------------------------------
!     CERES-Sugarbeet
      CASE ('BSCER')
            WRITE (HEADER(I),900) P1,P2,P5; I=I+1
            WRITE (HEADER(I),901) G2,G3,PHINT; I=I+1

!-----------------------------------------------------------------------
!     Sorghum
      CASE ('SGCER')
            WRITE (HEADER(I), 902) P1,P2O,P2R,P5; I=I+1
            WRITE (HEADER(I),1002) G1,G2,PHINT,P3,P4; I=I+1
            !Print optional parameters if used.
            IF (PBASE > 1.E-2 .AND. PSAT > 1.E-2) THEN
              WRITE(HEADER(I), 1010) PBASE, PSAT; I=I+1
            ENDIF

!-----------------------------------------------------------------------
!     Millet
      CASE ('MLCER')
            WRITE (HEADER(I), 903) P1,P2O,P2R,P5; I=I+1
            WRITE (HEADER(I),1003) G1,G4,PHINT; I=I+1

!-----------------------------------------------------------------------
!     Potato
      CASE ('PTSUB')
!      ELSEIF (INDEX ('PT',CROP) .GT. 0) THEN
         WRITE (HEADER(I), 905) G2,G3; I=I+1
!         WRITE (HEADER(I), 905) G2,G3,G4; I=I+1
         WRITE (HEADER(I),1005) PD,P2,TC; I=I+1

!-----------------------------------------------------------------------
!     Rice
      CASE ('RICER')
!      ELSEIF (INDEX ('RI',CROP) .GT. 0) THEN
         WRITE (HEADER(I), 906) P1,P2R,P5,P2O; I=I+1
         WRITE (HEADER(I),1006) G1,G2,G3,THOT,TCLDP,TCLDF; I=I+1

!-----------------------------------------------------------------------
!     Aroids
      CASE ('TNARO','TRARO')
!      ELSEIF (INDEX ('TNTR',CROP) .GT. 0) THEN
         WRITE (HEADER(I), 911) P1,P3,P4,P5; I=I+1
         WRITE (HEADER(I),1011) G3,G4,PHINT,PCINT,PCGRD; I=I+1

!-----------------------------------------------------------------------
!     Pineapple **
      CASE ('PIALO')
         WRITE (HEADER(I),2010) P1,P2,P3,P4,P5,P6; I=I+1
         WRITE (HEADER(I),2011) G2,G3,PHINT; I=I+1
 2010 FORMAT (1X,'    P1:',F6.1,'    P2:',F6.1,
     &           '    P3:',F6.1,'    P4:',F6.0,
     &           '    P5:',F6.1,'    P6:',F6.1)
 2011 FORMAT (1X,'    G2:',F6.1,'    G3:',F6.2,
     &           ' PHINT:',F6.1)

!-----------------------------------------------------------------------
!     Sugarcane - Canegro
      CASE ('SCCAN')

!-----------------------------------------------------------------------
!     Sugarcane - CASUPRO
      CASE ('SCCSP')
!     Not currently correct for either CaneGro or Casupro
!     CHP removed 8/3/07
!      ELSEIF (INDEX ('SC',CROP) .GT. 0) THEN
!        IF (INDEX(MODEL,'SCCAN') > 0) THEN
!          WRITE (HEADER(I), 907) P1,RATPOT,LFMAX; I=I+1
!          WRITE (HEADER(I),1007) G1,PI1,PI2,DTTPI; I=I+1
!        ELSEIF (INDEX(MODEL,'SCCSP') > 0) THEN
!          WRITE (HEADER(I),957) PI1, PI2, DTPI, SMAX, SO, GMAX, GO; I=I+1
!          WRITE (HEADER(I),1057) M1, M2, LA, WTPSD, SLAVAR, SIZLF; I=I+1
!        ENDIF

!-----------------------------------------------------------------------
!!     Sunflower
!      ELSEIF (INDEX ('SU',CROP) .GT. 0) THEN
!         WRITE (HEADER(I), 908) P1,P2,P5; I=I+1
!         WRITE (HEADER(I),1008) G2,G3,O1; I=I+1
!!     Pineapple
!      ELSEIF (INDEX ('PI',CROP) .GT. 0) THEN
!         WRITE (HEADER(I), 909) P2,P3,P4; I=I+1
!         WRITE (HEADER(I),1009) G2,G3,PHINT; I=I+1

!!     ?? old cotton model??
!      ELSEIF (INDEX ('CO',CROP) .GT. 0) THEN
!         WRITE (HEADER(I), 912) SCPB,RESPC,SQCON; I=I+1
!         WRITE (HEADER(I),1012) FCUT,FLAI,DDISQ
!      ENDIF

      END SELECT

      Headers % ICOUNT  = I - 1
      Headers % Header = HEADER

      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

  360 FORMAT (3X,'SOIL LOWER UPPER   SAT  EXTR  INIT   ROOT   BULK',
     &    5X,'pH    NO3    NH4    ORG')
  361 FORMAT (2X,
     &    'DEPTH LIMIT LIMIT    SW    SW    SW   DIST   DENS',26X,'C')
  362 FORMAT (3X,'cm',3X,3('cm3/cm3',4X),5X,'g/cm3',9X,'ugN/g  ugN/g',
     &    5X,'%')
  363 FORMAT (79('-'))

  410 FORMAT (I3,'-',I3,5(1X,F5.3),7(1X,F6.2))
  610 FORMAT ('TOT-',I3,5F6.1,2X,'<--cm   -','  kg/ha-->',2F7.1,I7)

  710 FORMAT ('SOIL ALBEDO    :',F5.2,6X,'EVAPORATION LIMIT :',F5.2,
     &        9X,'MIN. FACTOR  :',F5.2)
  711 FORMAT ('RUNOFF CURVE # :',F5.2,
     &        6X,'DRAINAGE RATE     :',F5.2,9X,'FERT. FACTOR :',F5.2)

  800 FORMAT (1X,A16,1X,'CULTIVAR: ',A6,'-',A16,3X,'ECOTYPE: ',
     &        A6)
  850 FORMAT (1X,'CSDVAR :',F5.2,'  PPSEN  :',F5.2,
     &         '  EMG-FLW:',F5.2,'  FLW-FSD:',F5.2,'  FSD-PHM :',F6.2)
  851 FORMAT (1X,'WTPSD  :',F5.3,'  SDPDVR :',F5.2,
     &         '  SDFDUR :',F5.2,'  PODDUR :',F5.2,'  XFRUIT  :',F6.2)
  853 FORMAT (1X,'THRESH :',F5.1,'  SDPRO  :',F5.3,'  SDLIP   :',F6.3)

  852 FORMAT (1X,'WTPSD  :',F5.3,'  SDPDVR :',F5.1,
     &         '  SDFDUR :',F5.2,'  PODDUR :',F5.2,'  XFRUIT  :',F6.2)

  870 FORMAT (1X,'VREQ   :',F6.1,'  VBASE  :',F6.1,'  VEFF   :',F6.2,
     &         '  PPS1   :',F6.3,'  PPS2   :',F6.1)
  871 FORMAT (1X,'P1     :',F6.1,'  P2     :',F6.1,'  P3     :',F6.1,
     &         '  P4     :',F6.1)
  872 FORMAT (1X,'P5     :',F6.1,'  P6     :',F6.1,'  P7     :',F6.1,
     &         '  P8     :',F6.1)
  873 FORMAT (1X,'GRNOW  :',F6.1,'  GRWTX  :',F6.1,'  SHWMS  :',F6.2,
     &         '  PHINT  :',F6.1)

  880 FORMAT (1X,'PPS1   :',F6.1,'  P1L    :',F6.1,'  P2L    :',F6.1,
     &         '  P4L    :',F6.1)
  881 FORMAT (1X,'P5L    :',F6.0,'  SRNOW  :',F6.2,'  SRFR   :',F6.2,
     &         '  LA1S   :',F6.1)
  882 FORMAT (1X,'LAXS   :',F6.1,'  LAXNO  :',F6.1,'  LAFS   :',F6.1,
     &         '  LAFNO  :',F6.1)
  883 FORMAT (1X,'LAWS   :',F6.1,'  PHINT  :',F6.1,'  LLIFA  :',F6.1,
     &         '  STFR   :',F6.2)

  900 FORMAT (1X,'P1     :',F7.2,'  P2     :',F7.4,
     &         '  P5     :',F7.2)
  915 FORMAT (1X,'P1     :',F7.2,'  P2     :',F7.4,
     &         '  P5     :',F7.2,'  AX     :',F6.1)
     
  902 FORMAT (1X,'P1     :',F5.1,'  P2O    :',F5.2,
     &         '  P2R    :',F6.1,'  P5     :',F6.2)
  903 FORMAT (1X,'P1     :',F6.2,'  P2O    :',F6.3,
     &         '  P2R    :',F6.1,'  P5     :',F6.2)

  904 FORMAT (1X,'P1V    :',F8.3,'  P1D    :',F8.3,
     &         '  P5     :',F8.2)
  954 FORMAT (1X,'P1V    :',F8.3,'  P1D    :',F8.4,
     &         '  P5     :',F8.2)      !,'  LT50H  :',F8.2)
  905 FORMAT (1X,'G2     :',F7.2,'  G3     :',F7.4)
!  905 FORMAT (1X,'G2     :',F7.2,'  G3     :',F7.4,'  G4     :',F7.2)
  906 FORMAT (1X,'P1     :',F6.1,'  P2R    :',F6.1,
     &         '  P5     :',F6.1,'  P2O    :',F6.1)
  907 FORMAT (1X,'P1     :',F6.1,'  RATPOT :',F6.1,
     &         '  LFMAX  :',F6.1)
  957 FORMAT (1X,' PI1   :',F6.1,'   PI2  :',F6.1,
     &         '   DTPI  :',F6.1,'   SMAX :',F6.1,
     &         '   SO    :',F6.1,'   GMAX :',F6.1,
     &         '   GO    :',F6.1)
  908 FORMAT (1X,'P1     :',F7.2,'  P2     :',F7.4,
     &         '  P5     :',F7.2)
  909 FORMAT (1X,'P2     :',F6.1,'  P3     :',F6.1,
     &         '  P4     :',F6.0)
  911 FORMAT (1X,'P1     :',F7.1,' P3     :',F7.2,
     &          ' P4     :',F7.1,' P5     :',F7.2)
  912 FORMAT (1X,'SCPB   :',F7.1,' RESPC  :',F7.3,
     &          ' SQCON  :',F7.3)
  901 FORMAT (1X,'G2     :',F7.2,'  G3     :',F7.3,'  PHINT  :',F7.3)
  916 FORMAT (1X,'G2     :',F7.2,'  G3     :',F7.3,
     &         '  PHINT  :',F7.3,'  LX     :',F6.1)  
 1002 FORMAT (1X,'G1     :',F5.1,'  G2     :',F5.2,'  PHINT  :',F6.2,
     &           '  P3     :',F6.1,'  P4    :',F6.1)     
 1003 FORMAT (1X,'G1     :',F6.2,'  G4     :',F6.2,'  PHINT  :',F6.2)
 1004 FORMAT (1X,'G1     :',F8.3,'  G2     :',F8.3,'  G3     :',F8.3,
     &         '  PHINT  :',F8.3)
 1054 FORMAT (1X,'G1     :',F8.3,'  G2     :',F8.3,'  G3     :',F8.3,
     &         '  PHINT  :',F8.3)
 1005 FORMAT (1X,'PD     :',F7.2,'  P2     :',F7.3,'  TC     :',F7.3)
 1006 FORMAT (1X,'G1     :',F6.1,'  G2     :',F6.4,
     &         '  G3     :',F6.2,'  THOT   :',F6.1,'  TCLDP  :',F6.1,
     &         '  TCLDF  :',F6.1)
 1007 FORMAT (1X,'G1     :',F6.1,'  PI1    :',F6.1,
     &         '  PI2    :',F6.1,'  DTTPI  :',F6.1)
 1057 FORMAT (1X,' M1    :',F6.1,'   M2   :',F6.1,
     &         '   LA    :',I6  ,'   WTPSD: ',F6.2,
     &         '  SLAVAR:',F6.1,'   SIZLF:',F6.1)

 1008 FORMAT (1X,'G2     :',F7.2,'  G3     :',F7.3,'  O1     :',F4.0)
 1009 FORMAT (1X,'G2     :',F6.1,'  G3     :',F6.2,'  PHINT  :',F6.1)
 1011 FORMAT (1X,'G3     :',F7.1,' G4     :',F7.1,
     &          ' PHINT  :',F7.1,' PCINT  :',F7.1,' PCGRD  :',F7.1)
 1012 FORMAT (1X,'FCUT   :',F7.3,' FLAI   :',F7.2,
     &          ' DDISQ  :',F7.1)

 2000 FORMAT (1X,'DUB1   :',F6.1,'  DUBR   :',F6.1,'  DESP   :',F6.2,
     &         '  PHCX   :',F6.2,'  S#PE   :',F6.1)
 2001 FORMAT (1X,'S#FX   :',F6.1,'  S#PX   :',F6.1,'  SWNX   :',F6.1,
     &         '  L#IS   :',F6.2,'  L#IP   :',F6.2)
 2002 FORMAT (1X,'LALX   :',F6.0,'  LAXA   :',F6.2,'  LAL3   :',F6.0,
     &         '  LAWS   :',F6.0,'  LFLI   :',F6.0)

 1010 FORMAT (1X,'PBASE  :',F6.2,'  PSAT   :',F6.2)
 
      END SUBROUTINE OPSOIL
C=======================================================================

C=======================================================================
C  HEADER, Subroutine, C. H. Porter
C-----------------------------------------------------------------------
C  Writes simulation header info to file.

!     Currently, the routine reads header info from a file (written 
!     previously by input module or elsewhere in the code).  Probably
!     should generate the header info here.

C-----------------------------------------------------------------------
C  REVISION HISTORY
C  10/18/2001 CHP Written.
C  08/12/2003 CHP Added I/O error checking
!  05/09/2007 CHP Can get CONTROL variable from here, don't need to send
!                 name of FILEIO from suborutine.  Keep sending DYNAMIC 
!                 and RUN, because these do not always have the same 
!                 values as that in the CONTROL variable.
C-----------------------------------------------------------------------
! Called by: IRRIG, OPWBAL, OPGROW, . . . 
! Calls: None
C========================================================================

      SUBROUTINE HEADER(DYNAMIC, LUNDES, RUN)

!     Reads up to 100 lines of header information and prints to output
!       files as needed.
!     DYNAMIC = 0 : Prints version number and date/time only
!     DYNAMIC = 1 (RUNINIT) : Complete header is printed
!     DYNAMIC = 2 (SEASINIT): Short header is printed 
!-----------------------------------------------------------------------
      USE ModuleDefs
      USE ModuleData
      USE HeaderMod
      IMPLICIT NONE
      INCLUDE 'COMIBS.blk'
      INCLUDE 'COMSWI.blk'
      SAVE

      CHARACTER*6, PARAMETER :: ERRKEY = 'HEADER'

      INTEGER DATE_TIME(8)
      INTEGER DYNAMIC, I, LUNDES
      INTEGER ICOUNT, RUN, PREV_RUN
      INTEGER ShortCount

      DATA PREV_RUN /0/
      INTEGER, PARAMETER :: MAXLUN = 200
      logical NOHEADER(MAXLUN)

      TYPE (ControlType) CONTROL
      CALL GET(CONTROL)

      ICOUNT = HEADERS % ICOUNT
      ShortCount = HEADERS % ShortCount

!-----------------------------------------------------------------------
      IF (RUN /= PREV_RUN) THEN
        NOHEADER = .TRUE.
        PREV_RUN = RUN
!        IF (CONTROL % MULTI > 1 .OR. CONTROL % RNMODE == 'Q') THEN
!          CALL MULTIRUN(RUN)
!        ENDIF
      ENDIF

!     Write Model info and date-time stamp to all headers
      IF (ICOUNT .GT. 0) THEN
        WRITE(LUNDES,'(/,A)') TRIM(HEADERS % Header(1))
      ELSE
        CALL DATE_AND_TIME (VALUES=DATE_TIME)
        WRITE (LUNDES,500)Version, VBranch, MonthTxt(DATE_TIME(2)),
     &    DATE_TIME(3), DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), 
     &    DATE_TIME(7)
        WRITE(LUNDES,1200) MOD(RUN,1000)
        RETURN
      ENDIF

!***********************************************************************
!***********************************************************************
!     Run Initialization
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
!     Write OVERVIEW header info to destination file.
      DO I = 2, ICOUNT
        WRITE(LUNDES,'(A)',ERR=200) TRIM(HEADERS % Header(I))
      ENDDO
  200 CONTINUE

!***********************************************************************
!***********************************************************************
!     Seasonal initialization
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      IF (LUNDES > MAXLUN) RETURN

      IF (NOHEADER(LUNDES) .AND. RUN > 0 .AND. ICOUNT > 0)THEN
!       Header exists, write it
        DO I = 2, ShortCount
          WRITE (LUNDES,'(A)') TRIM(HEADERS % HEADER(I))
        ENDDO
        WRITE(LUNDES,*) " "  
        NOHEADER(LUNDES) = .FALSE.

!      ELSE
!!       Header already written for this run, or header not avail yet.
!        CALL DATE_AND_TIME (VALUES=DATE_TIME)
!        WRITE (LUNDES,500)Version,MonthTxt(DATE_TIME(2)),DATE_TIME(3), 
!     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
!        WRITE(LUNDES,1200) MOD(RUN,1000)
      ENDIF

!***********************************************************************
      ENDIF
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      RETURN

  500   FORMAT ("*DSSAT Cropping System Model Ver. ",I1,".",I1,".",I1,
     &    ".",I3.3,1X,A,4X,
     &    A3," ",I2.2,", ",I4,"; ",I2.2,":",I2.2,":",I2.2)
 1200   FORMAT (/,'*RUN ',I3,/)

      END SUBROUTINE HEADER

!=======================================================================

!=======================================================================
      SUBROUTINE CO2Header(CO2)
!     Writes value of CO2 at beginning of run to header
      USE HeaderMod
      IMPLICIT NONE

      INTEGER I
      REAL CO2
      CHARACTER*80 TEXT

      DO I = Headers%ShortCount + 1, Headers%ICOUNT
        TEXT = Headers%Header(I)
        IF (TEXT(19:21) == 'CO2') THEN
          WRITE(Headers%Header(I),'(A,I4,A,A)') 
     &      TEXT(1:21), NINT(CO2), "ppm", TEXT(29:80)
          EXIT
        ENDIF
      ENDDO

      RETURN
      END SUBROUTINE CO2Header
!=======================================================================
