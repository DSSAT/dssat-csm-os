!=======================================================================
!  CROPSIM UTILITIES    
!  Routines/functions used by Cropaim modules
!  Last edit 20/02/14 LAH Minor changes to VCHECK following GH 180214
!=======================================================================

      ! Subroutines: Calendar, Csopline, Csdaylen, Csincmth, Csyr_doy
      !              Emod, Emod2
      !              Finddir, Fvcheck
      !              Getpos, Getstri, Getstrr, Getstr, Getstr1
      !              Linechar, Linevals, Ltrim, Ltrim2
      !              Standard, Standc, Start
      !              Ucase, Csclear5

      ! Functions    Csdoc, Csyeardoy, Csydoy, Cstabex,
      !              Csincdat, Csendyr, Csvpsat, Csmthend, Csmthmid,
      !              Cstimdif
      !              Dapcalc
      !              Tfac4, Tfac4_0, Tfac424
      !              Tvicolnm, Tvilent, Tvinstr, Tvifromc, Tvrfromc
      !              Tl1upcase, Tl10fromi
      !              Yval, Yvalxy, Yval1
      !              Csidlayr

      ! Following are used in the weather routine.
      ! Subroutines  Csfind, Csignore, Wthdata
      ! Function     Csupcase

      ! Following are used in the soil routines.
      ! Subroutines  Sllayers, Sldepth

      ! And the following is a generalization of Sllayers
      ! Subroutine   Numlayers

      ! Following is used in the Legumes module.
      ! Function     Cscurv

!-----------------------------------------------------------------------

      SUBROUTINE Calendar(iyr,doy,DOM,MONTH)

      ! Provide calendar date for specified day of year

      IMPLICIT NONE

      CHARACTER (LEN=3) mon(12),month
      INTEGER           dom,doy,idim(12),i,mo,iyr

      INTRINSIC         MOD

      SAVE

      DATA Mon(1:6)/'Jan','Feb','Mar','Apr','May','Jun'/
      DATA Mon(7:12)/'Jul','Aug','Sep','Oct','Nov','Dec'/

      IF (doy.GT.366 .OR. iyr.GT.3000) THEN
        dom = 0
        month = '-99'
        RETURN
      ENDIF

      DO i=1,12
       idim(i)=31
      ENDDO
      idim(4)=30
      idim(6)=30
      idim(9)=30
      idim(11)=30
      idim(2)=28
      IF(MOD(iyr,4).EQ.0)idim(2)=29
      mo=1
      dom=31
      DO WHILE(dom.LT.doy)
       mo=mo+1
       IF (mo.GT.12) THEN
         dom=0
         month='-99'
         RETURN
       ENDIF
       dom=dom+idim(mo)
      END DO
      dom=doy-dom+idim(mo)
      month=mon(mo)

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE Csopline(opline,opreal)

      ! Creates an output line from a real variable

      IMPLICIT NONE

      CHARACTER (LEN=*) opline
      REAL              opreal

      INTRINSIC         NINT

      SAVE

      IF (OPREAL.GT.0.00 .AND. OPREAL.LE.0.099) THEN
        WRITE (OPLINE,'(1X,F5.4)') opreal
      ELSEIF (OPREAL.GT.0.099 .AND. OPREAL.LE.0.99) THEN
        WRITE (OPLINE,'(1X,F5.3)') opreal
      ELSEIF (OPREAL.GT.0.999 .AND. OPREAL.LE.9.999) THEN
        WRITE (OPLINE,'(1X,F5.3)') opreal
      ELSEIF (OPREAL.GT.9.99 .AND. OPREAL.LE.99.99) THEN
        WRITE (OPLINE,'(1X,F5.2)') opreal
      ELSEIF (OPREAL.GT.99.99 .AND. OPREAL.LT.999.9)THEN
        WRITE (OPLINE,'(1X,F5.1)') OPREAL
      ELSE
        !TF - added if statement to avoid crashing when 
        ! OPREAL is larger than 99999
        IF(OPREAL .GT. 99999) THEN
          WRITE (OPLINE,'(1X,I5)') -99
        ELSE
          WRITE (OPLINE,'(1X,I5)') NINT(OPREAL)
        ENDIF
      ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE CSDAYLEN (DAY,XLAT,DAYL,DAYLC)

      IMPLICIT    NONE

      INTEGER   DAY, HEMIS
      REAL      LAT, PI, CFDATR, CFDGTR, CFRATH, ANGLE, RDATE
      REAL      DECLIN, HAS, SISI, COCO, DAYLC, DOY, XLAT, DAYL

      INTRINSIC ABS, ACOS, AMAX1, COS, FLOAT, SIN

      SAVE

      DOY = FLOAT(DAY)

      ! Algorithm from Simtag model

      IF (XLAT .LT. 0.0) THEN
        HEMIS = 1
        LAT = ABS(XLAT)
      ELSE
        HEMIS = 0
        LAT = XLAT
      ENDIF
      PI=3.141593
      CFDATR=2.*PI/365.
      CFDGTR=2.*PI/360.
      CFRATH=24./(2.*PI)
      ANGLE=96.
      RDATE=DOY*CFDATR
      DECLIN=0.397-22.980*COS(RDATE)+3.631*SIN(RDATE)-0.388*COS(2*RDATE)
      DECLIN=DECLIN+0.039*SIN(2*RDATE)-0.160*COS(3*RDATE)
      IF(HEMIS.EQ.1) DECLIN=-DECLIN
      COCO=COS(LAT*CFDGTR)*COS(DECLIN*CFDGTR)
      SISI=SIN(LAT*CFDGTR)*SIN(DECLIN*CFDGTR)
   3  HAS=ACOS(AMAX1(-1.,(COS(ANGLE*CFDGTR)-SISI)/COCO))
      DAYL=2.*HAS*CFRATH
      IF(ANGLE.LT.96.) GO TO 4
      DAYLC=DAYL
      ANGLE=90.+50./60.
      GO TO 3
   4  CONTINUE

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE CSINCMTH(YR,MTH,INC)

      ! Increases/decreases month,adjusts YR based on INC (ABS(INC)<=12)

      IMPLICIT NONE

      INTEGER YR,MTH,INC

      SAVE

      MTH = MTH + INC
      IF (MTH .GT. 12) THEN
        YR = YR + 1
        MTH = MTH - 12
      ELSE IF (MTH .LT. 1) THEN
        YR = YR - 1
        MTH = MTH + 12
      ENDIF

      END

!-----------------------------------------------------------------------

      SUBROUTINE CSYR_DOY(YRDOY,YEAR,DOY)

      !  Converts YRDOY (or YEARDOY) to YEAR and DOY.

      IMPLICIT NONE

      INTEGER   DOY,YEAR,YRDOY,YR,YRDOYWRK

      INTRINSIC INT

      SAVE

      YRDOYWRK = YRDOY
      IF(YRDOY.GE.2000000)YRDOYWRK=YRDOY-2000000
      IF(YRDOY.GE.1900000)YRDOYWRK=YRDOY-1900000

      IF(YRDOY.LE.0)THEN
        YEAR  = - 99
        DOY = - 99
      ELSE
        YR  = INT(YRDOYWRK / 1000)
        DOY = YRDOYWRK - YR * 1000
      ENDIF

      IF (YR.GE.19) THEN
        YEAR=1900+YR
      ELSEIF (YR.GE.0 .AND. YR.LT.19) THEN
        YEAR=2000+YR
      ENDIF

      END

!-----------------------------------------------------------------------

      SUBROUTINE EMOD(FACADJ,FAC,ADJ,NEV)

      ! Generates values for environmental adjustments from input string

      IMPLICIT NONE
      EXTERNAL ltrim, Tvilent, GETLUN

      CHARACTER (LEN=*)  facadj(10), fac(10)
      REAL               adj(10)
      INTEGER            i,l,tvilent,nev,fnumerr

      SAVE

      DO I = 1,NEV
       CALL ltrim(facadj(i))
       L=Tvilent(facadj(i))
       IF(l.LT.2)THEN
         fac(i)='0'
         adj(i)=-99
       ELSEIF(l.EQ.2)THEN
         fac(i)=facadj(i)(1:1)
         READ(facadj(i),'(1x,F1.0)')adj(i)
       ELSEIF(l.EQ.3)THEN
         IF(facadj(i)(1:1).EQ.'-')THEN
           fac(i)='0'
           adj(i)=-99
         ELSE
           fac(i)=facadj(i)(1:1)
           READ(facadj(i),'(1x,F2.0)')adj(i)
         ENDIF
       ELSEIF(l.EQ.4)THEN
         fac(i)=facadj(i)(1:1)
         READ(facadj(i),'(1x,F3.0)')adj(i)
       ELSEIF(l.EQ.5)THEN
         fac(i)=facadj(i)(1:1)
         READ(facadj(i),'(1x,F4.0)')adj(i)
       ELSEIF(l.GT.5)THEN
         CALL Getlun('ERROR.OUT',fnumerr)
         OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
         WRITE(fnumerr,*)' Problem reading Environmental Modifications!'
         WRITE(fnumerr,*) ' Check WORK.OUT for details'
         WRITE (*,*) ' Problem reading Environmental Modifications!'
         WRITE (*,*) ' Program will have to stop'
         WRITE (*,*) ' Check WORK.OUT for details'
         CLOSE (fnumerr)
         STOP ' '
       ENDIF
      ENDDO

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE EMOD2(EM,EC,FAC,ADJ,NEV)

      ! Generates values for environmental adjustments for ICASA files

      IMPLICIT NONE

      CHARACTER (LEN=*)  em(10), ec(10), fac(10)
      REAL               adj(10)
      INTEGER            i,l,nev

      INTRINSIC          LEN,TRIM

      SAVE

      DO I = 1,NEV
        fac(i)='0'
        adj(i)=-99
        IF (ec(I)(1:3).NE.'-99') THEN
          IF (ec(I)(1:1).NE.'0') THEN
            EC(I) = TRIM(EC(I))
            L = LEN(EC(I))
            fac(I) = EC(I)(L:L)
            READ(EM(I),'(F6.0)') adj(I)
          ENDIF
        ENDIF
      ENDDO

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE Finddir(fnumcfg,cfgdfile,cfgcode,fname,FDIRNAME)

      ! Returns full name (inc directory) for specified file
      ! Uses cfg.file

      USE OSDefinitions

      IMPLICIT NONE
      EXTERNAL Getstr, Tvilent, Ucase, GETLUN

      CHARACTER (LEN=80) tline
      CHARACTER (LEN=2)  tl2
      CHARACTER (LEN=37) tl37
      CHARACTER (LEN=*)  cfgdfile,cfgcode,fname,fdirname
      INTEGER            fnumcfg,l,tvilent,loop,fnumerr
      LOGICAL            fflag

      SAVE

      fdirname='-99'

      INQUIRE(FILE=cfgdfile,EXIST=fflag)
      IF(.NOT.fflag) RETURN

      OPEN(fnumcfg,FILE=cfgdfile)

      DO loop=1,2000
       READ(fnumcfg,'(A80)',ERR=1000,END=1000)tline
       IF(tline(1:6).EQ.'*DIREC')EXIT
      ENDDO

      DO loop=1,2000
       READ(fnumcfg,'(A80)',ERR=1000,END=1000)tline
       IF(tline(1:1).EQ.'*')THEN
        CLOSE (fnumcfg)
        fdirname=' '
        fdirname(1:3)='-99'
        RETURN
       ENDIF
       IF(tline(1:3).EQ.cfgcode(1:3))THEN
        CALL Getstr(tline,2,tl2)
        CALL Getstr(tline,3,tl37)
        l=Tvilent(tl37)
        IF(tl37(l:l).NE.SLASH.AND.l.LT.37)THEN
         tl37(l+1:l+1)=SLASH
         l=l+1
        ENDIF
        fdirname=tl2//tl37(1:l)//fname
        CALL Ucase(fdirname)
        INQUIRE(FILE=fdirname,EXIST=fflag)
        IF(fflag)EXIT
       ENDIF
      ENDDO
      CLOSE (fnumcfg)
      RETURN

 1000 CONTINUE
      CALL Getlun ('ERROR.OUT',fnumerr)
      OPEN (UNIT = fnumerr,FILE = 'ERROR.OUT')
      WRITE (fnumerr,*) ' Problem with configuration file!'
      WRITE (fnumerr,*)' Looking for code: ',cfgcode
      WRITE (fnumerr,*)' Looking for file: ',fname
      WRITE (fnumerr,*) ' Check WORK.OUT for details'
      WRITE (*,*) ' Problem with configuration file!'
      WRITE (*,*) ' Looking for code: ',cfgcode
      WRITE (*,*) ' Looking for file: ',fname
      WRITE (*,*) ' Program will have to stop'
      WRITE (*,*) ' Check WORK.OUT for details'
      CLOSE (fnumerr)
      STOP ' '

      END

!-----------------------------------------------------------------------

      SUBROUTINE FVCHECK(DIRFILE,VCODE)

      ! Checks file version (ie.compares what needed in model with what
      ! in header of file.

      IMPLICIT NONE
      EXTERNAL GETLUN, TVILENT

      CHARACTER (LEN=*)    DIRFILE, VCODE
      CHARACTER (LEN=80)   TLINE
!     CHARACTER (LEN=30)   TLINE2
      CHARACTER (LEN=1)    COLON
      INTEGER              L,FNUMERR,FNUMTMP,TVILENT,ERRNUM
      LOGICAL              FFLAG  !,FOPEN

!      INTRINSIC EOF   !portability

      SAVE

      CALL Getlun ('FVCHECK',fnumtmp)

      INQUIRE (FILE = dirfile,EXIST = fflag)
      IF (.NOT.fflag) THEN
        CALL Getlun('ERROR.OUT',fnumerr)
        OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
        WRITE (fnumerr,*) ' Could not find input file! '
        WRITE (fnumerr,*) ' File was: ',dirfile(1:60)
        WRITE (fnumerr,*) ' Version code sought was: ',vcode
        WRITE (*,*) ' Could not find input file! '
        WRITE (*,*) ' File was: ',dirfile(1:60)
        WRITE (*,*) ' Version code sought was: ',vcode
        WRITE (*,*) ' Program will have to stop'
        CLOSE (fnumerr)
        STOP ' '
      ELSE
        COLON = 'N'
        OPEN (UNIT = FNUMTMP,FILE = DIRFILE)
        READ(FNUMTMP,'(A80)',IOSTAT=ERRNUM) TLINE    !portability      
        IF (ERRNUM < 0) THEN       !EOF - file is empty
          CALL Getlun('ERROR.OUT',fnumerr)
          OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
          WRITE (fnumerr,*) ' Input file empty! '
          WRITE (fnumerr,*) ' File was: ',dirfile(1:60)
          WRITE (*,*) ' Input file empty! '
          WRITE (*,*) ' File was: ',dirfile(1:60)
          WRITE (*,*) ' Program will have to stop'
          CLOSE (fnumerr)
          STOP ' '
        ENDIF
        IF (TVILENT(TLINE).LT.10) THEN
          CALL Getlun('ERROR.OUT',fnumerr)
          OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
          WRITE (fnumerr,*) ' Input file empty! '
          WRITE (fnumerr,*) ' File was: ',dirfile(1:60)
          WRITE (*,*) ' Input file empty! '
          WRITE (*,*) ' File was: ',dirfile(1:60)
          WRITE (*,*) ' Program will have to stop'
          CLOSE (fnumerr)
          STOP ' '
        ENDIF
        DO L = 1,50
          IF (COLON.EQ.'Y' .AND. TLINE(L:L).NE.' ') EXIT
          IF (TLINE(L:L).EQ.':' .OR. TLINE(L:L).EQ.' ') COLON='Y'
        ENDDO
        CLOSE (FNUMTMP)
      
        IF (TLINE(L:L+14) .NE. VCODE(1:15)) THEN
          CALL Getlun ('ERROR.OUT',fnumerr)
          OPEN (UNIT = fnumerr,FILE = 'ERROR.OUT')
          WRITE(fnumerr,*) ' '
          WRITE(fnumerr,'(A32)')' Input file not correct version!'
          WRITE(fnumerr,'(A8,A60)')  '  File: ',dirfile(1:60)
          WRITE(fnumerr,'(A11,A15)') '  Version: ',tline(L:L+14)
          WRITE(fnumerr,'(A18,A15)') '  Needed version: ',vcode(1:15)
          WRITE(fnumerr,'(A21)')     '  Program had to stop'
          WRITE(*,*) ' Input file not correct version!'
          WRITE(*,*) '  File: ',dirfile(1:60)
          WRITE(*,*) '  Version: ',tline(L:L+14)
          WRITE(*,*) '  Needed version: ',vcode(1:15)
          WRITE(*,*) '  Program will have to stop'
          STOP ' '
        ENDIF
      ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE Getpos(tline,number,SPOS)

      ! Returns start position for specified string in line

      IMPLICIT NONE
      EXTERNAL Getstr1

      CHARACTER (LEN=*)  tline
      CHARACTER (LEN=30) value
      INTEGER            number,spos,epos

      SAVE

      CALL Getstr1(tline,number,value,spos,epos)

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE Getstri(tline,number,VALUEI)

      ! Returns integer for specifed position in line

      IMPLICIT NONE
      EXTERNAL Getstr1, TVIFROMC

      CHARACTER (LEN=*)  tline
      CHARACTER (LEN=10) value
      INTEGER            number,epos,spos,valuei,tvifromc

      SAVE

      CALL Getstr1(tline,number,value,spos,epos)
      valuei = TVIFROMC(value)

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE Getstrr(tline,number,VALUER)

      ! Returns real for specified position in line

      IMPLICIT NONE
      EXTERNAL Getstr1, TVRFROMCCDE, TVRFROMC

      CHARACTER (LEN=*)  tline
      CHARACTER (LEN=10) value
      INTEGER            number,epos,spos
      REAL               valuer,tvrfromc,tvrfromccde

      SAVE

      CALL Getstr1(tline,number,value,spos,epos)
      valuer = TVRFROMCCDE(tline,value)
      valuer = TVRFROMC(value)

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE Getstr(tline,number,VALUE)

      ! Returns variable string for specifed position in line

      IMPLICIT NONE
      EXTERNAL Getstr1

      CHARACTER (LEN=*) tline,value
      INTEGER           number,epos,spos

      SAVE

      CALL Getstr1(tline,number,value,spos,epos)

      RETURN
      END

!-----------------------------------------------------------------------

      SUBROUTINE Getstr1(tline,number,VALUE,SPOS,EPOS)

      ! Returns variable string for specifed position in line

      IMPLICIT NONE
      EXTERNAL tvinstr

      CHARACTER (LEN=*) tline, value
      INTEGER           number,pos1,pos2,i,loc,switch,spos,epos
      INTEGER           tvinstr,l

      INTRINSIC         LEN,TRIM

      SAVE

      value=' '

      IF(number.LE.0)THEN
       value='-99'
       RETURN
      ENDIF

      i=tvinstr(tline)
      tline=TRIM(tline)
      l=LEN(tline)
      IF(number.GT.i)THEN
       value='-99'
       spos=-99
       epos=-99
       RETURN
      ENDIF

  10  CONTINUE
      pos1=0
      pos2=0
      switch=0
      loc=0
      i=0
      DO WHILE (i.LE.l-1)
       i=i+1
       IF(i.LE.l .AND. tline(i:i).NE.' ')THEN
        IF(switch.EQ.0)THEN
         switch=1
        ELSE
         switch=2
        ENDIF
        IF(pos1.NE.0)THEN
         pos2=i
        ELSE
         pos1=i
         pos2=i
        ENDIF
       ELSE
        IF(number.EQ.loc.AND.(switch.EQ.1.OR.switch.EQ.2))THEN
         value=tline(pos1:pos2)
         spos=pos1
         epos=pos2
        ENDIF
        switch=0
        pos1=0
       ENDIF
       IF(switch.EQ.1)THEN
        loc=loc+1
       ENDIF
      ENDDO

      IF(number.EQ.2.AND.value.EQ.'=')THEN
       number=3
       GOTO 10
      ENDIF

      IF(value.EQ.' ') value='-99'
      IF(value.EQ.'.') value='-99'

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE Linechar(tline,ARRAY)

      ! Reads a character string and returns individual components

      IMPLICIT NONE
      EXTERNAL Tvilent

      INTEGER            ncol,l,istart,iend,i,l1,loop
      INTEGER            ineg,LENLINE,TVILENT,LENLINET
      CHARACTER (LEN=*)  tline
      CHARACTER (LEN=31) datatmp
      CHARACTER (LEN=10) array(20)

      INTRINSIC          LEN,MIN

      SAVE

      Lenline  = Len(tline)
      Lenlinet = Tvilent(tline)
      Lenline  = MIN(lenline,lenlinet+2)

      l=0
      i=0
      ineg = 0

      DO loop=1,20
       array(loop)=' '
      ENDDO
      ncol=0

      DO WHILE (i.LE.lenline)
       istart=0
       iend=0
       i=l
       DO WHILE (istart.LT.1)
        IF(ineg.EQ.0)THEN
         i=i+1
        ELSEIF(ineg.GT.0)THEN
         i=i
         ineg=0
        ENDIF
        IF(i.GT.LENLINE) GOTO 9999
        IF(tline(i:i).EQ.'-'.OR.tline(i:i).NE.' ')THEN
         istart=i
         EXIT
        ENDIF
       ENDDO
       i=istart
       DO WHILE (iend.LT.1)
        i=i+1
        IF(i.GT.LENLINE)EXIT
        IF(tline(i:i).EQ.'-'.OR.tline(i:i).EQ.' ')THEN
         IF(tline(i:i).EQ.'-') ineg=1
         iend=i-1
         EXIT
        ENDIF
       ENDDO
       l=i

       IF(i.GT.lenline)EXIT
       ncol=ncol+1
       IF(ncol.GT.20)EXIT

       l1=(iend-istart+1)
       datatmp='                               '
       datatmp(1:l1)=tline(istart:iend)

       IF(ncol.GT.0)array(ncol)=datatmp(1:10)

      END DO

9999  CONTINUE

      RETURN
      END

!-----------------------------------------------------------------------

      SUBROUTINE Linevals(tline,ARRAY)

      ! Reads a character string and returns values to max of 20 items

      IMPLICIT NONE
      EXTERNAL Tvilent

      INTEGER            ncol,l,istart,iend,i,l1,loop,ineg
      INTEGER            LENLINE,TVILENT,LENLINET,I2
      CHARACTER (LEN=*)  tline
      CHARACTER (LEN=31) datatmp
      CHARACTER (LEN=3)  vartype
      REAL               array(20),valuetmp

      INTRINSIC          ICHAR,LEN,MIN

      SAVE

      Lenline=Len(tline)
      Lenlinet=Tvilent(tline)
      Lenline = Min(lenline,lenlinet+2)

      ! Remove flags (eg.A,N) and old values after flag
      I=0
      VARTYPE = 'CHA'
      DO WHILE (I.LE.LENLINE)
        I=I+1
        IF (ICHAR(TLINE(I:I)).LE.57.AND.TLINE(I:I).NE.' ')VARTYPE='NUM'
        IF (VARTYPE.EQ.'NUM' .AND. ICHAR(TLINE(I:I)).GT.57) THEN
          TLINE(I:I) = ' '
          DO I2 = 1,10
            IF (I+I2.GT.LENLINE) EXIT
            IF (TLINE((I+I2):(I+I2)).EQ.' ') THEN
              VARTYPE = 'CHA'
              EXIT
            ENDIF
            TLINE((I+I2):(I+I2)) =  ' '
          ENDDO
        ENDIF
      ENDDO

      l=0                                              ! Find column #
      i=0
      ineg=0
      DO loop=1,20
       array(loop)=0.0
      ENDDO
      ncol=0

      DO WHILE (i.LE.lenline)
       istart=0
       iend=0
       i=l
       DO WHILE (istart.LT.1)
        IF(ineg.EQ.0)THEN
         i=i+1
        ELSEIF(ineg.GT.0)THEN
         i=i
         ineg=0
        ENDIF
        IF(i.GT.LENLINE) GOTO 9999
        IF(tline(i:i).EQ.'-'.OR.tline(i:i).NE.' ')THEN
         istart=i
         EXIT
        ENDIF
       ENDDO
       i=istart
       DO WHILE (iend.LT.1)
        i=i+1
        IF(i.GT.LENLINE)EXIT
        IF(tline(i:i).EQ.'-'.OR.tline(i:i).EQ.' ')THEN
         IF(tline(i:i).EQ.'-') ineg=1
         iend=i-1
         EXIT
        ENDIF
       ENDDO
       l=i

       IF(i.GT.lenline)EXIT
       ncol=ncol+1
       IF(ncol.GT.20)EXIT

       l1=(iend-istart+1)
       datatmp='                               '
       datatmp(1:l1)=tline(istart:iend)
       READ(datatmp,300,ERR=301)valuetmp
  300  FORMAT(F10.0)
       GOTO 302
  301  CONTINUE
       datatmp(l1:l1)=' '
       READ(datatmp,300,ERR=303)valuetmp
       GOTO 302
  303  CONTINUE
       valuetmp=-99
  302  CONTINUE
       IF(ncol.GT.0)array(ncol)=valuetmp

      END DO

 9999 CONTINUE

      RETURN
      END

!-----------------------------------------------------------------------

      SUBROUTINE Ltrim(tchar)

      ! Left trims a character variable

      IMPLICIT NONE
      EXTERNAL Tvilent, Getlun

      CHARACTER (LEN=*)   tchar
      CHARACTER (LEN=500) tchar2
      INTEGER             l,i,k,tvilent,lentc,fnumerr

      INTRINSIC           LEN

      SAVE

      lentc = LEN(tchar)
      IF (lentc.GT.500) THEN
        CALL Getlun('ERROR.OUT',fnumerr)
        OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
        WRITE (fnumerr,*)' Problem in Ltrim! Input line too long!'
        WRITE (fnumerr,*)' Check WORK.OUT for details'
        WRITE (*,*) ' Problem in Ltrim! Input line too long!'
        WRITE (*,*) ' Program will have to stop'
        WRITE (*,*) ' Check WORK.OUT for details'
        CLOSE (fnumerr)
        STOP ' '
      ENDIF

      i=1
      l=Tvilent(tchar)
      IF(l.LE.0)RETURN
      IF(l.GT.1)THEN
       DO i=1,l
        IF(tchar(i:i).NE.' ')EXIT
       ENDDO
      ENDIF
      k=l-(i-1)

      tchar2=' '
      tchar2(1:k)=tchar(i:l)
      tchar = ' '

      tchar(1:k)=tchar2(1:k)

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE Ltrim2(tchar,tchar2)

      ! Left trims a character variable - used when string passed

      IMPLICIT NONE
      EXTERNAL Tvilent

      CHARACTER(*)  tchar, tchar2
      INTEGER       l,i,k,tvilent

      SAVE

      i=1
      l=Tvilent(tchar)
      IF(l.LE.0)RETURN
      IF(l.GT.1)THEN
       DO i=1,l
        IF(tchar(i:i).NE.' ')EXIT
       ENDDO
      ENDIF
      k=l-(i-1)

      tchar2=' '
      tchar2(1:k)=tchar(i:l)

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE Standard(tlinein,TLINEOUT,widthc)

      ! Standardises variables along a string to widthc spaces each

      IMPLICIT NONE
      EXTERNAL Tvilent, Tvifromc, Getlun

      CHARACTER (LEN=*)  tlinein,tlineout,widthc
      INTEGER            i,j,l,itmp,tvilent,blanks,width,tvifromc
      INTEGER            lenin,lenout,fnumerr

      INTRINSIC          CHAR,LEN

      SAVE

      width=Tvifromc(widthc)

      tlineout=' '
      i=0
      l=0
      j=Tvilent(tlinein)
      lenin=LEN(tlinein)
      lenout=LEN(tlineout)
      blanks=0

      DO WHILE(l.LT.j)
       l=l+1
       blanks=blanks+1
       IF(blanks.GT.50)EXIT
       IF(tlinein(l:l).NE.CHAR(32))THEN
        IF(tlinein(l:l).NE.CHAR(0))THEN
        blanks=0
        itmp=0
        DO WHILE(tlinein(l:l).NE.' ')
         IF(tlinein(l:l).EQ.CHAR(0))EXIT
         IF(tlinein(l:l).EQ.CHAR(32))EXIT
         blanks=blanks+1
         IF(blanks.GT.50)EXIT
         i=i+1
         IF(l.EQ.lenin)GOTO 9998
         itmp=itmp+1
         tlineout(i:i)=tlinein(l:l)
         l=l+1
         IF(tlinein(l:l).EQ.CHAR(0))EXIT
         IF(itmp.EQ.width-1)THEN
          DO WHILE(tlinein(l:l).NE.' ')
           IF(tlinein(l:l).EQ.CHAR(0))EXIT
           IF(tlinein(l:l).EQ.CHAR(32))EXIT
           l=l+1
          ENDDO
          EXIT
         ENDIF
        ENDDO
        DO WHILE(itmp.LT.width)
         i=i+1
         IF(i.EQ.lenout)GOTO 9999
         itmp=itmp+1
         tlineout(i:i)=' '
        ENDDO
       ENDIF
       ENDIF
       IF(tlinein(l:l).EQ.CHAR(0))EXIT
      ENDDO

      RETURN

 9998 CONTINUE
      CALL Getlun ('ERROR.OUT',fnumerr)
      OPEN (UNIT = fnumerr,FILE = 'ERROR.OUT')
      WRITE (fnumerr,*)' Problem in standard! End of line, or no blank!'
      WRITE (fnumerr,*)' Linein : ',tlinein(1:65)
      WRITE (fnumerr,*)' Lineout: ',tlineout(1:65)
      WRITE (fnumerr,*)' Check WORK.OUT for details'
      WRITE (*,*) ' Problem in standard! End of line, or no blank!'
      WRITE (*,*) ' Linein : ',tlinein(1:65)
      WRITE (*,*) ' Lineout: ',tlineout(1:65)
      WRITE (*,*) ' Program will have to stop'
      WRITE (*,*) ' Check WORK.OUT for details'
      CLOSE (fnumerr)
      STOP ' '

 9999 CONTINUE
      CALL Getlun ('ERROR.OUT',fnumerr)
      OPEN (UNIT = fnumerr,FILE = 'ERROR.OUT')
      WRITE (fnumerr,*) ' Problem in standard! Line too long!'
      WRITE (fnumerr,*) ' Linein : ',tlinein(1:65)
      WRITE (fnumerr,*) ' Lineout: ',tlineout(1:65)
      WRITE (fnumerr,*)' Check WORK.OUT for details'
      WRITE (*,*) ' Problem in standard! Line too long!'
      WRITE (*,*) ' Linein : ',tlinein(1:65)
      WRITE (*,*) ' Lineout: ',tlineout(1:65)
      WRITE (*,*) ' Program will have to stop'
      WRITE (*,*) ' Check WORK.OUT for details'
      CLOSE (fnumerr)
      STOP ' '

      END

!-----------------------------------------------------------------------

      SUBROUTINE Standc(atlinein,tlineini,TLINEOUT,TCHAROUT,widthc)

      ! Removes one string of text from below data code with dots, or
      ! at end of line, then standardises variables to 'widthc' spaces
      ! each. Returns both text from below code with dots, or at end
      ! (tcharout) and standardised line.
      ! NB 'TEXT' iserted in place of string so that variable position
      ! along line is not changed.
      ! NB April 2002 gave specific string lengths to tlineout,tcharout

      ! NB Tcharout is limited to 52 characters. This may truncate
      ! a long string, as for example with soil classification from
      ! soil files.

      IMPLICIT NONE
      EXTERNAL Tvilent, Tvifromc, Getlun, ltrim

      CHARACTER (LEN=1000) tlinetmp, atline, tlinein, tlineout
      CHARACTER (LEN=52)   tcharout
      CHARACTER (LEN=1)    dottype
      CHARACTER (LEN=*)    tlineini, atlinein, widthc
      INTEGER              i,j,k,l,itmp,tvilent,blanks,width,start
      INTEGER              tvifromc,lenout,lenafter,kk,lendata
      INTEGER              lentchar,fnumerr,enddot  !fnumwrk,
!     LOGICAL              FOPEN         

      INTRINSIC            CHAR,LEN,MAX0,MIN,MIN0

      SAVE

      dottype='I'    ! Dots inside line

      j=Tvilent(atlinein)
      IF(j.LT.3)THEN
        CALL Getlun ('ERROR.OUT',fnumerr)
        OPEN (UNIT = fnumerr,FILE = 'ERROR.OUT')
        WRITE(fnumerr,*)' Abbreviation line not found! Wrong file type?'
        WRITE(fnumerr,*)' Abvr.line: ',atlinein(1:60)
        WRITE(fnumerr,*)' Data line: ',tlineini(1:60)
        WRITE (fnumerr,*) ' Check WORK.OUT. Routine was STANDC.'
        WRITE (*,*) ' Abbreviation line not found! Wrong file type?'
        WRITE (*,*) ' Abvr.line: ',atlinein(1:60)
        WRITE (*,*) ' Data line: ',tlineini(1:60)
        WRITE (*,*) ' Program will have to stop'
        WRITE (*,*) ' Check WORK.OUT for details. Routine was STANDC.'
        CLOSE (fnumerr)
        STOP ' '
      ENDIF

      DO l=1,j
        IF (atlinein(l:l).EQ.'!') EXIT
      ENDDO
      IF (l.LT.j) j = l-1
      atline = ' '
      IF (j.LE.10) THEN
        atline(1:1) = atlinein(1:1)
        atline(3:J+1) = atlinein(2:j)
      ELSE
        atline(1:j) = atlinein(1:j)
      ENDIF

      j=Tvilent(tlineini)
      IF(j.LT.3)THEN
!        INQUIRE (FILE = 'WORK.OUT',OPENED = FOPEN)
!        IF (.NOT.FOPEN)THEN
!          CALL Getlun ('WORK.OUT',fnumwrk)
!          OPEN (UNIT = FNUMWRK,FILE = 'WORK.OUT')
!        ENDIF
!        WRITE (fnumwrk,*)' Data line sent for standardising too short!'
!        WRITE (fnumwrk,*)' Abvr.line: ',atlinein(1:60)
!        WRITE (fnumwrk,*)' Data line: ',tlineini(1:60)
!        WRITE (fnumwrk,*)' Check WORK.OUT for details. Routine STANDC.'
!        WRITE (*,*) ' Data line sent for standardising too short!'
!        WRITE (*,*) ' Abvr.line: ',atlinein(1:60)
!        WRITE (*,*) ' Data line: ',tlineini(1:60)
!        WRITE (*,*) ' Check WORK.OUT for details. Routine STANDC.'
        RETURN
      ENDIF

      tlinein=' '
      DO l=1,j
        IF (tlineini(l:l).EQ.'!') THEN
          tlinein=tlineini(1:l-1)
          EXIT
        ENDIF
        IF (l.EQ.j) tlinein=tlineini(1:j)
      ENDDO

      kk=LEN(tlineout)

      width=0
      width=Tvifromc(widthc)

      ! Establish position of dotted variable
      j=Tvilent(atline)
      start=0
      enddot=0
      DO l=2,j
       IF(atline(l:l).NE.' '.AND.atline(l-1:l-1).EQ.' ')start=l
       IF(atline(l:l).EQ.' '.AND.atline(l-1:l-1).EQ.'.')THEN
        enddot=l-1
        EXIT
       ENDIF
      ENDDO

      IF(enddot.EQ.0)THEN
       enddot=Tvilent(tlinein)
       dottype='E'   ! Dots at end of line
      ENDIF

      ! Create new lineout
      tlineout=' '
      tcharout=' '
      j=Tvilent(tlinein)
      IF(start.GT.1)tlineout(1:start-1)=tlinein(1:start-1)
      IF(enddot.NE.0)THEN
       lenafter=j-enddot
       IF(lenafter.GT.0.AND.dottype.EQ.'I')THEN
        tlineout(start:start+5)=' TEXT '
        tlineout((start+6):((start+6)+lenafter))=tlinein(enddot+1:j)
       ENDIF
       IF(dottype.EQ.'E')THEN
        IF(start.GT.1)THEN
          IF(tlineout(start-1:start-1).NE.' ')start=start-1
          IF(tlineout(start-1:start-1).NE.' ')start=start-1
          tlineout(1:start-1)=tlinein(1:start-1)
          tlineout(start:start+5)=' TEXT '
          lendata=Tvilent(tlinein)
          lentchar=MIN(52,lendata-start+1)
          tcharout(1:lentchar)=tlinein(start:start+lentchar-1)
         ELSE
          tlineout=tlinein
         ENDIF
       ENDIF
      ENDIF

      ! Create output character string
      IF(enddot.GT.start+1)THEN
       i=MIN0(52,enddot-start+1)
       i=MAX0(i,1)
       tcharout(1:i)=tlinein(start:enddot)
       CALL ltrim(tcharout)
      ENDIF

      tlinetmp=' '
      lenout=Tvilent(tlineout)
      tlinetmp(1:lenout)=tlineout(1:lenout)
      tlineout=' '

      i=0
      l=0
      j=Tvilent(tlinetmp)
      k=LEN(tlinetmp)
      kk=LEN(tlineout)
      blanks=0

      DO WHILE(l.LT.j)
       l=l+1
       blanks=blanks+1
       IF(blanks.GT.50)EXIT
       IF(tlinetmp(l:l).NE.CHAR(32))THEN
        IF(tlinetmp(l:l).NE.CHAR(0))THEN
        blanks=0
        itmp=0
        DO WHILE(tlinetmp(l:l).NE.' ')
         IF(tlinetmp(l:l).EQ.CHAR(0))EXIT
         IF(tlinetmp(l:l).EQ.CHAR(32))EXIT
         blanks=blanks+1
         IF(blanks.GT.50)EXIT
         i=i+1
         IF(i.EQ.k)GOTO 9998
         itmp=itmp+1
         tlineout(i:i)=tlinetmp(l:l)
         l=l+1
         IF(tlinetmp(l:l).EQ.CHAR(0))EXIT
         IF(itmp.EQ.width-1)EXIT
        ENDDO
        DO WHILE(itmp.LT.width)
         i=i+1
         IF(i.EQ.k)RETURN
         IF(i.EQ.kk)RETURN
         itmp=itmp+1
         tlineout(i:i)=' '
        ENDDO
       ENDIF
       ENDIF
       IF(tlinetmp(l:l).EQ.CHAR(0))EXIT
      ENDDO

      RETURN

 9998 CONTINUE
      CALL Getlun ('ERROR.OUT',fnumerr)
      OPEN (UNIT = fnumerr,FILE = 'ERROR.OUT')
      WRITE (fnumerr,*) ' Problem in STANDC!! Did not find blank! '
      WRITE (fnumerr,*) ' Linein : ',tlinein(1:65)
      WRITE (fnumerr,*) ' Lineout: ',tlineout(1:65)
      WRITE (fnumerr,*) ' Check WORK.OUT for details'
      WRITE (*,*) ' Problem in STANDC!! Did not find blank! '
      WRITE (*,*) ' Linein : ',tlinein(1:65)
      WRITE (*,*) ' Lineout: ',tlineout(1:65)
      WRITE (*,*) ' Program will have to stop'
      WRITE (*,*) ' Check WORK.OUT for details'
      CLOSE (fnumerr)
      STOP ' '

      END

!-----------------------------------------------------------------------

      SUBROUTINE Start(tchar,K)

      ! Finds where first character on line is positioned

      IMPLICIT NONE
      EXTERNAL Tvilent

      CHARACTER (LEN=*) tchar
      INTEGER           l,i,k,tvilent

      SAVE

      k=0
      l=Tvilent(tchar)
      IF(l.LE.0)RETURN
      IF(l.GT.1)THEN
       DO i=1,l
        IF(tchar(i:i).NE.' ')EXIT
       ENDDO
      ENDIF
      k=i

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE Ucase(inchar)

      ! Returns the upper case of a string

      IMPLICIT  NONE
      EXTERNAL Tvilent, ltrim, Tl1upcase

      CHARACTER (LEN=*) inchar
      CHARACTER (LEN=1) tl1upcase
      INTEGER           l,i,tvilent

      SAVE

      CALL ltrim(inchar)
      i=Tvilent(inchar)

      DO l=1,i
       inchar(l:l)=Tl1upcase(inchar(l:l))
      ENDDO

      END

!-----------------------------------------------------------------------

      SUBROUTINE CSUcase(inchar)

      ! Returns the upper case of a string

      IMPLICIT  NONE
      EXTERNAL Tvilent, ltrim, Tl1upcase

      CHARACTER (LEN=*) inchar
      CHARACTER (LEN=1) tl1upcase
      INTEGER           l,i,tvilent

      SAVE

      CALL ltrim(inchar)
      i=Tvilent(inchar)

      DO l=1,i
       inchar(l:l)=Tl1upcase(inchar(l:l))
      ENDDO

      END

!-----------------------------------------------------------------------

      SUBROUTINE CSCLEAR5

       WRITE  (*,'(/////)')

      END

!-----------------------------------------------------------------------

      FUNCTION CSDOC(YR,DOY)

      ! Calculates day-of-century (01/01/1901 : CSDOC = 1)

      IMPLICIT NONE

      INTEGER DOY,NLEAP,YR,CSDOC

      SAVE

      NLEAP = (YR-1)/4
      CSDOC = NLEAP*366 + (YR-NLEAP-1)*365 + DOY

      END

!-----------------------------------------------------------------------

      FUNCTION CSYEARDOY(YRDOY)

      ! Converts YRDOY to YEARDOY

      IMPLICIT NONE

      INTEGER    DOY,YR,YRDOY,CSYEARDOY

      INTRINSIC  INT

      SAVE

      IF (YRDOY.GT.0 .AND. YRDOY.LE.99365) THEN
        YR  = INT(YRDOY / 1000)
        DOY = YRDOY - YR * 1000
        IF (YR .LE. 10) THEN
          CSYEARDOY = (2000 + YR) * 1000 + DOY
        ELSE
          CSYEARDOY = (1900 + YR) * 1000 + DOY
        ENDIF
      ELSE
        CSYEARDOY = YRDOY
      ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      FUNCTION CSYDOY(YEAR,DOY)

      ! Converts YR and DOY to YRDOY.

      IMPLICIT NONE

      INTEGER DOY,YR,YEAR,CSYDOY

      SAVE

      IF(year.GE.2000)THEN
        YR=YEAR-2000
      ELSEIF(year.GE.1900)THEN
        YR=YEAR-1900
      ELSE
        YR=YEAR
      ENDIF

      CSYDOY = YR*1000 + DOY

      END

!-----------------------------------------------------------------------

      FUNCTION CSTABEX(VAL,ARG,DUMMY,K)

      !  Looks up data from 'table'

      IMPLICIT NONE
      EXTERNAL Getlun

      INTEGER   K,J,FNUMERR
      REAL      VAL(K),ARG(K),DUMMY,CSTABEX,CS

      INTRINSIC ABS

      SAVE

      DO 100  J = 2,K
        IF (DUMMY .GT. ARG(J)) GO TO 100
        GO TO 200
  100 CONTINUE
      J = K
      IF (ABS(ARG(J)-ARG(J-1)).LT.0.00001) THEN
        CALL Getlun('ERROR.OUT',fnumerr)
        OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
        WRITE (fnumerr,*) ' Divisor in CSTABEX = 0.0!!'
        WRITE (fnumerr,*) ' Check WORK.OUT for details'
        WRITE (*,*) ' Divisor in CSTABEX = 0.0!!'
        WRITE (*,*) ' Program will have to stop'
        WRITE (*,*) ' Check WORK.OUT for details'
        CLOSE (fnumerr)
        STOP ' '
      ENDIF
  200 CS = (DUMMY-ARG(J-1))*(VAL(J)-VAL(J-1))/(ARG(J)-ARG(J-1))+VAL(J-1)
      CSTABEX=CS

      END

!-----------------------------------------------------------------------

      FUNCTION CSINCDAT(ADATE, DELTA)

      ! Increases or decrease date (no restriction to 365 days)

      IMPLICIT NONE
      EXTERNAL YR_DOY

      INTEGER NDYR, AYR, ADOY, ADATE, DELTA, CSENDYR, CSINCDAT

      EXTERNAL CSENDYR

      SAVE

      !CALL CSYR_DOY(ADATE, AYR, ADOY)
      CALL YR_DOY(ADATE, AYR, ADOY)
      NDYR = CSENDYR(AYR)
      ADOY = ADOY + DELTA
  100 CONTINUE
      IF (ADOY .GT. NDYR) THEN
        AYR = AYR + 1
        ADOY = ADOY - NDYR
        GO TO 100
      END IF
  200 IF (ADOY .LE. 0) THEN
        AYR = AYR - 1
        NDYR = CSENDYR(AYR)
        ADOY = ADOY + NDYR
        GO TO 200
      END IF
      CSINCDAT = AYR*1000+ADOY

      RETURN

      END

!-----------------------------------------------------------------------

      FUNCTION CSENDYR(YR)

      ! Computes end-of-year (365 or 366) depending if leap year.

      IMPLICIT NONE

      INTEGER   YR,CSENDYR

      INTRINSIC MOD

      SAVE

      IF (MOD(YR,4) .EQ. 0) THEN
        CSENDYR = 366
      ELSE
        CSENDYR = 365
      ENDIF

      END

!-----------------------------------------------------------------------

      REAL FUNCTION CSVPSAT(T)

      ! Calculates saturated vapor pressure of air (Tetens, 1930).

      IMPLICIT NONE

      REAL      T

      INTRINSIC EXP

      SAVE

      CSVPSAT = 610.78 * EXP(17.269*T/(T+237.30))

      END

!-----------------------------------------------------------------------

      FUNCTION CSMTHEND(YR,MTH)

      ! Calculates day-of-year that is end of month.

      IMPLICIT NONE

      INTEGER   MTH,MEND(12),YR,CSMTHEND
      LOGICAL   LEAPYR

      INTRINSIC MOD

      SAVE

      DATA MEND/31,59,90,120,151,181,212,243,273,304,334,365/

      LEAPYR = MOD(YR,4) .EQ. 0
      IF (LEAPYR .AND. MTH.GE.2) THEN
        CSMTHEND = MEND(MTH) + 1
      ELSE
        CSMTHEND = MEND(MTH)
      ENDIF

      END

!-----------------------------------------------------------------------

      FUNCTION CSMTHMID(YR,MTH)

      ! Calculates day-of-year that is midpoint of month.

      IMPLICIT NONE

      INTEGER   MTH,YR,MIDPT(12),CSMTHMID
      LOGICAL   LEAPYR

      INTRINSIC MOD

      SAVE

      DATA MIDPT/16,46,75,106,136,167,197,228,259,289,320,350/

      LEAPYR = MOD(YR,4) .EQ. 0
      IF (LEAPYR .AND. MTH.GE.2) THEN
        CSMTHMID = MIDPT(MTH) + 1
      ELSE
        CSMTHMID = MIDPT(MTH)
      ENDIF

      END

!-----------------------------------------------------------------------

      FUNCTION CSTIMDIF(YRDOY1IN,YRDOY2IN)

      ! Calculates time difference between two YRDOY or YEARDOY dates

      IMPLICIT NONE

!     INTEGER   CSDOC,DOY1,DOY2,YR1,YR2,YRDOY1,YRDOY2,YRDOY1IN,YRDOY2IN
      INTEGER   DOY1,DOY2,YR1,YR2,YRDOY1,YRDOY2,YRDOY1IN,YRDOY2IN
      INTEGER   CSTIMDIF
      INTEGER   NLEAP,DOC1,DOC2

      INTRINSIC INT

      SAVE

      YRDOY1 = YRDOY1IN
      YRDOY2 = YRDOY2IN

      ! Simple time difference two days in the same year attempted first
      CSTIMDIF = YRDOY2 - YRDOY1

      ! If time difference involves year change
      IF (CSTIMDIF .GT. 365 .OR. CSTIMDIF .LT. -365) THEN
        IF (YRDOY1.GT.2000000) YRDOY1 = YRDOY1-2000000
        IF (YRDOY1.GT.1900000) YRDOY1 = YRDOY1-1900000
        YR1  = INT(YRDOY1 / 1000)
        DOY1 = YRDOY1 - YR1 * 1000
        NLEAP = (YR1-1)/4
        DOC1 = NLEAP*366 + (YR1-NLEAP-1)*365 + DOY1
        IF (YRDOY2.GT.2000000) YRDOY2 = YRDOY2-2000000
        IF (YRDOY2.GT.1900000) YRDOY2 = YRDOY2-1900000
        YR2  = INT(YRDOY2 / 1000)
        DOY2 = YRDOY2 - YR2 * 1000
        NLEAP = (YR2-1)/4
        DOC2 = NLEAP*366 + (YR2-NLEAP-1)*365 + DOY2
        ! No 'trap' for inverted inputs
        ! Comparison used to shows if into new millenium
        IF (YR2.LT.YR1) DOC2 = DOC2 + 36524
        CSTIMDIF = DOC2 - DOC1
      ENDIF

      END

!-----------------------------------------------------------------------

      FUNCTION Dapcalc(datein,pyrin,pdoyin)

      ! Calculates days after planting from date and planting DATA

      IMPLICIT NONE
      EXTERNAL CSTIMDIF

      INTEGER   dapcalc,datein,dateinadj !day,yeartmp,dateinwr,year,
      INTEGER   pdoyin,pyrin,pyeardoyin,cstimdif  !pdoy,pyr,

      SAVE

      IF(datein.LT.-90 .OR. pyrin.LT.-90 .OR. pdoyin.LT.-90)THEN
       dapcalc=-99
       RETURN
      ENDIF

      dapcalc=-99
      
      PYEARDOYIN = PYRIN*1000 + PDOYIN

! TF - check if fileA inputs are in YYYYDDD format 04-16-2024
      IF (DATEIN.GE.999999.AND.DATEIN.LE.PYEARDOYIN) THEN
          DATEIN = DATEIN - PYRIN*1000
      ENDIF

      IF (DATEIN.LT.1000) THEN
        IF (DATEIN.GT.PDOYIN) THEN
          DATEINADJ = PYRIN*1000 + DATEIN
        ELSE  
          ! Simply add 1 year
          DATEINADJ = (PYRIN+1)*1000 + DATEIN
        ENDIF
      ELSE
         DATEINADJ = DATEIN  
      ENDIF
      
      DAPCALC = MAX(0,CSTIMDIF(PYEARDOYIN,DATEINADJ))

      IF(dapcalc.LT.0)dapcalc=-99

      RETURN

      END
!-----------------------------------------------------------------------

      FUNCTION TfacBeta(tcard,temp,TUNIT)

      ! Calculate temp factor and thermal units from cardinal temps
      ! using a Beta function

      IMPLICIT NONE

      REAL      tcard(4),temp,tunit,topt,tfacbeta  !tfac4,
!      INTEGER   fnumwrk
!     LOGICAL   fopen

      INTRINSIC AMAX1,AMIN1

      SAVE
      
!      CALL Getlun ('WORK.OUT',fnumwrk)
!      INQUIRE (FILE = 'WORK.OUT',OPENED = fopen)
!      IF (.NOT.fopen) OPEN (UNIT = fnumwrk,FILE = 'WORK.OUT')
      
      TOPT = TCARD(2)+((TCARD(3)-TCARD(2))/2.0)
      
      TFACBETA = ((TCARD(4)-TEMP)/(TCARD(4)-TOPT))*
     & ((TEMP/TOPT)**(TOPT/(TCARD(4)-TOPT)))

      tfacbeta = AMAX1(0.0,AMIN1(1.0,tfacbeta))
      tunit = tfacbeta * (topt-tcard(1))

      RETURN
      END

!-----------------------------------------------------------------------

      FUNCTION Tfac4(tcard,temp,TUNIT)

      ! Calculate temp factor and thermal units from cardinal temps

      IMPLICIT NONE

      REAL      tfac4,tcard(4),temp,tunit

      INTRINSIC AMAX1,AMIN1

      SAVE

      IF (temp.LE.tcard(1)) THEN
        tfac4 = 0.0
      ELSEIF (temp.GT.tcard(1) .AND. temp.LE.tcard(2)) THEN
        tfac4 = (temp-tcard(1))/(tcard(2)-tcard(1))
      ELSEIF (temp.GT.tcard(2) .AND. temp.LE.tcard(3)) THEN
        tfac4 = 1.0
      ELSEIF (temp.GT.tcard(3) .AND. temp.LE.tcard(4)) THEN
        tfac4 = 1.0 - ((temp-tcard(3))/(tcard(4)-tcard(3)))
      ELSEIF (temp.GT.tcard(4)) THEN
        tfac4 = 0.0
      ENDIF

      tfac4 = AMAX1(0.0,AMIN1(1.0,tfac4))
      tunit = tfac4 * (tcard(2)-tcard(1))

      RETURN
      END

!-----------------------------------------------------------------------

      FUNCTION Tfac4p(tcardp,temp,rstage,TUNIT)

      ! Calculate temp factor and thermal units from cardinal temps

      IMPLICIT NONE

      REAL      tfac4p,tcardp(4,9),temp,tunit,rstage,tvr1,tvr2
      INTEGER   I

      INTRINSIC AMAX1,AMIN1

      SAVE
      
      i = MAX(1,INT(rstage))
      i = MIN(i,9)

      IF (temp.LE.tcardp(1,i)) THEN
        tfac4p = 0.0
      ELSEIF (temp.GT.tcardp(1,i) .AND. temp.LE.tcardp(2,i)) THEN
        tfac4p = (temp-tcardp(1,i))/(tcardp(2,i)-tcardp(1,i))
        tvr1 = AMAX1(0.0,1.0 - exp(-0.03*(tcardp(2,1)-tcardp(1,i))))
        if (tvr1.gt.0.0) then
          tvr2 = AMAX1(0.0,1.0 - exp(-0.03*(temp-tcardp(1,i))))/tvr1
        else
          tvr2 = 0.0
        endif
        tfac4p = tvr2
      ELSEIF (temp.GT.tcardp(2,i) .AND. temp.LE.tcardp(3,i)) THEN
        tfac4p = 1.0
      ELSEIF (temp.GT.tcardp(3,i) .AND. temp.LE.tcardp(4,i)) THEN
        tfac4p = 1.0 - ((temp-tcardp(3,i))/(tcardp(4,i)-tcardp(3,i)))
      ELSEIF (temp.GT.tcardp(4,i)) THEN
        tfac4p = 0.0
      ENDIF

      tfac4p = AMAX1(0.0,AMIN1(1.0,tfac4p))
      tunit = tfac4p * (tcardp(2,i)-tcardp(1,i))

      RETURN
      END

!-----------------------------------------------------------------------

      FUNCTION Tfac424(tcard,tmax,tmin,TUNIT)

      ! Calculate temp factor and thermal units from cardinal temps and
      ! hourly temperatures.

      IMPLICIT NONE

      REAL      tfac4,tcard(4),temp,tunit,tfac4sum,tfac424,tairhr(24)
      REAL      tmax,tmin
      INTEGER   l

      INTRINSIC AMAX1,AMIN1

      SAVE

      tfac4sum = 0.0
      DO L = 1,24
        temp = tairhr(l)
        temp = tmin + (l-1)*(tmax-tmin)/23.0
        IF (temp.LE.tcard(1)) THEN
          tfac4 = 0.0
        ELSEIF (temp.GT.tcard(1) .AND. temp.LE.tcard(2)) THEN
          tfac4 = (temp-tcard(1))/(tcard(2)-tcard(1))
        ELSEIF (temp.GT.tcard(2) .AND. temp.LE.tcard(3)) THEN
          tfac4 = 1.0
        ELSEIF (temp.GT.tcard(3) .AND. temp.LE.tcard(4)) THEN
          tfac4 = 1.0 - ((temp-tcard(3))/(tcard(4)-tcard(3)))
        ELSEIF (temp.GT.tcard(4)) THEN
          tfac4 = 0.0
        ENDIF
          tfac4sum = tfac4sum + tfac4/24.0
      ENDDO

      tfac424 = AMAX1(0.0,AMIN1(1.0,tfac4sum))
      tunit = tfac424 * (tcard(2)-tcard(1))

      RETURN
      END

!-----------------------------------------------------------------------

      FUNCTION Tfac4_0(tcard,temp,TUNIT)

      ! Calculate temp factor and thermal units from cardinal temps
      ! Differs from TFAC4 in that goes down to 0C even if base > 0C

      IMPLICIT NONE

      REAL    tfac4_0,tcard(4),temp,tunit,tcardn(0:4)
      REAL    tfacwr1,tfacwr2,tfacwr3
      REAL    tfacnew, tcutoff

      INTRINSIC AMAX1,AMIN1

      SAVE

      tcutoff = 5.0

      IF (tcard(2)-tcard(1).GT.tcutoff) THEN
        tcardn(0) = tcard(1)+tcutoff
        tcardn(1) = tcard(2)-tcutoff
      ELSE
        tcardn(0) = (tcard(1)+tcard(2))/2.0
        tcardn(1) = (tcard(1)+tcard(2))/2.0
      ENDIF
      IF (tcard(3)-tcard(2).GT.10.0) THEN
        tcardn(2) = tcard(2)+tcutoff
      ELSE
        tcardn(2) = (tcard(2)+tcard(3))/2.0
      ENDIF
      IF (tcard(3)-tcard(2).GT.10.0) THEN
        tcardn(3) = tcard(3)-tcutoff
      ELSE
        tcardn(3) = (tcard(2)+tcard(3))/2.0
      ENDIF
      IF (tcard(4)-tcard(3).GT.tcutoff) THEN
        tcardn(4) = tcard(3)+tcutoff
      ELSE
        tcardn(4) = (tcard(3)+tcard(4))/2.0
      ENDIF

      IF (temp.LE.tcardn(0)) THEN
        IF (tcard(1).LE.0.0) THEN
          tfac4_0 = AMAX1(0.0,(temp+tcard(1))/(tcard(2)+tcard(1)))
        ELSEIF (tcard(1).GT.0.0) THEN
          ! New to slowly increase from 0C even if tbase over 0
          tfacwr1 = 0.0
          tfacwr2 = (tcardn(0)-tcard(1))/(tcard(2)-tcard(1))
          !tfacwr2 = (tcardn(0))/(tcard(2))
          tfacwr3 = (temp/(tcardn(0)))**2
          tfac4_0 = tfacwr1 + tfacwr2*tfacwr3
        ENDIF
      ELSEIF (temp.GT.tcardn(0) .AND. temp.LE.tcardn(1)) THEN
        tfac4_0 = (temp-tcard(1))/(tcard(2)-tcard(1))
      ELSEIF (temp.GT.tcardn(1) .AND. temp.LE.tcardn(2)) THEN
        tfacwr1 = (tcardn(1)-tcard(1))/(tcard(2)-tcard(1))
        tfacwr2 = 1.0-tfacwr1
        tfacwr3 = 1.0 - ((tcardn(2)-temp)/(tcardn(2)-tcardn(1)))**2
        tfac4_0 = tfacwr1 + tfacwr2*tfacwr3
      ELSEIF (temp.GT.tcardn(2) .AND. temp.LE.tcardn(3)) THEN
        tfac4_0 = 1.0
      ELSEIF (temp.GT.tcardn(3) .AND. temp.LE.tcardn(4)) THEN
        tfacwr1 = (tcard(4)-tcardn(4))/(tcard(4)-tcard(3))
        tfacwr2 = 1.0-tfacwr1
        tfacwr3 = ((temp-tcardn(3))/(tcardn(4)-tcardn(3)))**2
        tfac4_0 = 1.0 - tfacwr2*tfacwr3
      ELSEIF (temp.GT.tcardn(4) .AND. temp.LE.tcard(4)) THEN
        tfac4_0 = 1.0 - ((temp-tcard(3))/(tcard(4)-tcard(3)))
      ELSEIF (temp.GT.tcard(4)) THEN
        tfac4_0 = 0.0
      ENDIF
      tfacnew = AMAX1(0.0,AMIN1(1.0,tfac4_0))
      tfac4_0 = tfacnew

      tfac4_0 = AMAX1(0.0,AMIN1(1.0,tfac4_0))
      tunit = tfac4_0 * (tcard(2)-tcard(1))

      RETURN
      END

!-----------------------------------------------------------------------

      FUNCTION Tvicolnm(tl,header)

      ! Reads a character string and returns column # for identifier

      IMPLICIT NONE
      EXTERNAL Tvilent

      CHARACTER (LEN=*)   header,tl
      CHARACTER (LEN=254) tl2541
      CHARACTER (LEN=31)  headtmp
      INTEGER             tvicolnm,ncol,l,headfound,istart,iend,i,lhead
      INTEGER             tvilent,ltl

      SAVE

      lhead=Tvilent(header)
      ltl=Tvilent(tl)
      tl2541 = ' '
      IF (ltl.LE.254) THEN
        tl2541(1:ltl) = tl
      ELSE
        tl2541 = tl(1:254)
      ENDIF
      l=0                                              ! Find column #
      ncol=0
      tvicolnm=0
      headfound=0
      headtmp='                               '

      IF(tl2541(1:1).EQ.'@')tl2541(1:1)=' '

      DO WHILE(headfound.LT.1.AND.l.LT.250)
       istart=0
       iend=0
       i=l
       DO WHILE (istart.LT.1)
        i=i+1
        IF(i.GE.250)THEN
         istart=i
         EXIT
        ENDIF
        IF(tl2541(i:i).NE.' ')THEN
         istart=i
         EXIT
        ENDIF
       ENDDO
       i=istart
       DO WHILE (iend.LT.1)
        i=i+1
        IF(i.GE.250)THEN
         iend=i
         EXIT
        ENDIF
        IF(tl2541(i:i).EQ.' ')THEN
         iend=i-1
         EXIT
        ENDIF
       ENDDO
       l=i
       IF((iend-istart).GT.0.OR.iend.EQ.istart)ncol=ncol+1
       IF(iend-istart.LT.30)THEN
        headtmp=tl2541(istart:iend)
       ELSE
        headtmp=tl2541(istart:istart+30)
       ENDIF
       IF(headtmp(1:lhead).EQ.header.AND.
     &    lhead.EQ.((iend-istart)+1))THEN
        headfound=1
        EXIT
       ENDIF
      END DO

      IF(headfound.GT.0)tvicolnm=ncol

      RETURN

      END

!-----------------------------------------------------------------------

      FUNCTION Tvilent(tchar)

      ! Returns the string length after trimming off trailing blanks

      IMPLICIT NONE

      CHARACTER (LEN=*) tchar
      INTEGER           tvilent
      INTEGER           i,length,l

      INTRINSIC         LEN,CHAR

      SAVE

      l=LEN(tchar)
      length=l
      DO i=l,1,-1
       IF(tchar(i:i).NE.CHAR(32))THEN
        IF(tchar(i:i).NE.CHAR(0))EXIT
       ENDIF
       IF(tchar(i:i).EQ.CHAR(32).OR.tchar(i:i).EQ.CHAR(0))THEN
        length=length-1
       ENDIF
      ENDDO
      tvilent=length

      END

!-----------------------------------------------------------------------

      FUNCTION Tvinstr(tline)

      ! Returns number of strings in a line

      IMPLICIT NONE
      EXTERNAL Tvilent

      CHARACTER (LEN=*) tline
      INTEGER           tvinstr,i,n,tint,tvilent,switch,psw

      SAVE

      n=0
      psw=0
      tint=Tvilent(tline)
      DO i=1,tint
       IF(tline(i:i).EQ.' ')THEN
        switch=0
       ELSE
        switch=1
       ENDIF
       IF(switch.GT.psw) n=n+1
       psw=switch
      ENDDO
      tvinstr=n

      END

!-----------------------------------------------------------------------

      FUNCTION Tvifromc(charnum)

      ! Change character string to an integer

      IMPLICIT NONE
      EXTERNAL Tvilent, Getlun, WARNING

      CHARACTER (LEN=*)  charnum
      CHARACTER (LEN=10) newchar
      CHARACTER (LEN=78) message(10)
      INTEGER            number,pos,i,lenchar,tvifromc,tvilent,fnumerr
!     INTEGER            fnumwrk
!     LOGICAL            fopen

      SAVE

      lenchar=Tvilent(charnum)
      IF (lenchar.LE.0)THEN
        CALL Getlun('ERROR.OUT',fnumerr)
        OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
        WRITE (fnumerr,*) ' Problem in Tvifromc (integer<-character)!'
        WRITE (fnumerr,*) ' Input character was: ',charnum            
        WRITE (fnumerr,*) ' Length of input character was ',lenchar   
        CLOSE (fnumerr)
        STOP ' '
      ENDIF

      !CALL CSUCASE(charnum(lenchar:lenchar))
      IF(charnum(lenchar:lenchar).EQ.'R')THEN
       i=lenchar-1
       charnum=charnum(1:i)
      ENDIF
      IF(charnum(lenchar:lenchar).EQ.'r')THEN
       i=lenchar-1
       charnum=charnum(1:i)
      ENDIF
      lenchar=Tvilent(charnum)
      IF(lenchar.EQ.0)THEN
       tvifromc=-99
       RETURN
      ENDIF
      DO i=1,lenchar
       IF(charnum(i:i).NE.' '.OR.charnum(i:i).NE.'0')THEN
        pos=i
        newchar=charnum(pos:lenchar)
        EXIT
       ENDIF
      ENDDO
      lenchar=Tvilent(newchar)
      IF((lenchar.EQ.1).AND.(IACHAR(newchar(1:1)).NE.26))THEN
       READ(newchar,'(i1)',ERR=9991)number
      ELSEIF(lenchar.EQ.2)THEN
       READ(newchar,'(i2)',ERR=9991)number
      ELSEIF(lenchar.EQ.3)THEN
       READ(newchar,'(i3)',ERR=9991)number
      ELSEIF(lenchar.EQ.4)THEN
       READ(newchar,'(i4)',ERR=9991)number
      ELSEIF(lenchar.EQ.5)THEN
       READ(newchar,'(i5)',ERR=9991)number
      ELSEIF(lenchar.EQ.6)THEN
       READ(newchar,'(i6)',ERR=9991)number
      ELSEIF(lenchar.EQ.7)THEN
       READ(newchar,'(i7)',ERR=9991)number
      ELSEIF(lenchar.EQ.8)THEN
       READ(newchar,'(i8)',ERR=9991)number
      ELSEIF(lenchar.EQ.9)THEN
       READ(newchar,'(i9)',ERR=9991)number
      ELSEIF(lenchar.EQ.10)THEN
       READ(newchar,'(i10)',ERR=9991)number
      ELSEIF(lenchar.EQ.11)THEN
       READ(newchar,'(i11)',ERR=9991)number
      ELSEIF(lenchar.EQ.12)THEN
       READ(newchar,'(i12)',ERR=9991)number
      ELSEIF(lenchar.GE.12)THEN
       CALL Getlun('ERROR.OUT',fnumerr)
       OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
       WRITE(fnumerr,*)' Cannot convert to integer - too large ',newchar
       WRITE(fnumerr,*)' Check WORK.OUT for details'
       WRITE (*,*) ' Cannot convert to integer - too large ',newchar
       WRITE (*,*) ' Program will have to stop'
       WRITE (*,*) ' Check WORK.OUT for details'
       CLOSE (fnumerr)
       STOP ' '
      ENDIF
      tvifromc=number
      RETURN
 9991 CONTINUE
      tvifromc=-99
      MESSAGE(1) = 'Problem in Tvifromc (integer<-character)!'
      WRITE(MESSAGE(2),'(A19,A10)')'Trying to convert: ',newchar
      MESSAGE(3) = 'Returned -99. This may affect results'      
      CALL WARNING(3,'CSUTS',MESSAGE)

!      CALL Getlun ('WORK.OUT',fnumwrk)
!      INQUIRE (FILE = 'WORK.OUT',OPENED = fopen)
!      IF (.NOT.fopen) OPEN (UNIT = fnumwrk,FILE = 'WORK.OUT')
      lenchar=Tvilent(newchar)
!      WRITE (fnumwrk,*) ' '
!      WRITE (fnumwrk,*) ' WARNING Problem in Tvifromc utility.'
!      WRITE (fnumwrk,*) ' Trying to convert to integer character: ',
!     & newchar
!      WRITE (fnumwrk,*) ' Length of input character was ',lenchar   
!      WRITE (fnumwrk,*) ' Returned -99. This may affect results'      
!      WRITE (fnumwrk,*) ' '
!      IF (.NOT.fopen) CLOSE (fnumwrk)
      
      RETURN

      END

!-----------------------------------------------------------------------

      FUNCTION Tvrfromc(charnum)

      ! Change character string to a real

      IMPLICIT NONE
      EXTERNAL Tvilent, Getlun, WARNING

      CHARACTER (LEN=*)  charnum
      CHARACTER (LEN=10) newchar
      CHARACTER (LEN=78) message(10)
      INTEGER            pos,i,lenchar,tvilent,fnumerr
!     INTEGER            fnumwrk
      REAL               tvrfromc,number
!     LOGICAL            fopen

      SAVE

      lenchar=Tvilent(charnum)
      IF (lenchar.LE.0)THEN
        CALL Getlun('ERROR.OUT',fnumerr)
        OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
        WRITE (fnumerr,*) ' Problem in Tvrfromc (real<-character)!'
        WRITE (fnumerr,*) ' Input character was: ',charnum            
        WRITE (fnumerr,*) ' Length of input character was ',lenchar   
        CLOSE (fnumerr)
        STOP ' '
      ENDIF


      IF(charnum(lenchar:lenchar).EQ.'R')THEN
       i=lenchar-1
       charnum=charnum(1:i)
      ENDIF
      IF(charnum(lenchar:lenchar).EQ.'r')THEN
       i=lenchar-1
       charnum=charnum(1:i)
      ENDIF
      lenchar=Tvilent(charnum)
      IF(lenchar.EQ.0)THEN
       tvrfromc=-99.0
       RETURN
      ENDIF
      DO i=1,lenchar
       IF(charnum(i:i).NE.' '.OR.charnum(i:i).NE.'0')THEN
        pos=i
        newchar=charnum(pos:lenchar)
        EXIT
       ENDIF
      ENDDO
      lenchar=Tvilent(newchar)
      IF(lenchar.EQ.1)THEN
       READ(newchar,'(F1.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.2)THEN
       READ(newchar,'(F2.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.3)THEN
       READ(newchar,'(F3.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.4)THEN
       READ(newchar,'(F4.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.5)THEN
       READ(newchar,'(F5.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.6)THEN
       READ(newchar,'(F6.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.7)THEN
       READ(newchar,'(F7.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.8)THEN
       READ(newchar,'(F8.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.9)THEN
       READ(newchar,'(F9.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.10)THEN
       READ(newchar,'(F10.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.11)THEN
       READ(newchar,'(F11.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.12)THEN
       READ(newchar,'(F12.0)',ERR=9991)number
      ELSEIF(lenchar.GE.12)THEN
       CALL Getlun('ERROR.OUT',fnumerr)
       OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
       WRITE (fnumerr,*) ' Cannot convert to real - too large ',newchar
       WRITE (fnumerr,*) ' Check WORK.OUT for details'
       WRITE (*,*) ' Cannot convert to real - too large ',newchar
       WRITE (*,*) ' Program will have to stop'
       WRITE (*,*) ' Check WORK.OUT for details'
       CLOSE (fnumerr)
       STOP ' '
      ENDIF
      tvrfromc=number
      RETURN
 9991 CONTINUE
      tvrfromc=-99.0
      MESSAGE(1) = 'Problem in Tvrfromc (real<-character)!'
      WRITE(MESSAGE(2),'(A19,A10)')'Trying to convert: ',newchar
      MESSAGE(3) = 'Returned -99. This may affect results'      
      CALL WARNING(3,'CSUTS',MESSAGE)
            
!      CALL Getlun ('WORK.OUT',fnumwrk)
!      INQUIRE (FILE = 'WORK.OUT',OPENED = FOPEN)
!      IF (.NOT.FOPEN)THEN
!        OPEN (UNIT = FNUMWRK,FILE = 'WORK.OUT')
!      ENDIF  
!      WRITE (fnumwrk,*) ' Problem in Tvrfromc (real<-character)!'
!      WRITE (fnumwrk,*) ' Trying to convert: ',newchar


      RETURN

      END
!-----------------------------------------------------------------------

      FUNCTION Tvrfromccde(code,charnum)

      ! Change character string to a real

      IMPLICIT NONE
      EXTERNAL Tvilent, Getlun, WARNING

      CHARACTER (LEN=*)  charnum,code
      CHARACTER (LEN=10) newchar
      CHARACTER (LEN=78) message(10)
      INTEGER            pos,i,lenchar,tvilent,fnumerr
!     INTEGER            fnumwrk
      REAL               tvrfromc,number,tvrfromccde
!     LOGICAL            fopen

      SAVE

      lenchar=Tvilent(charnum)
      IF (lenchar.LE.0)THEN
        CALL Getlun('ERROR.OUT',fnumerr)
        OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
        WRITE (fnumerr,*) ' Problem in Tvrfromccde (real<-character)!'
        WRITE (fnumerr,*) ' Input character was: ',charnum            
        WRITE (fnumerr,*) ' Length of input character was ',lenchar   
        WRITE (fnumerr,*) ' Code was ',code                           
        WRITE (*,*) ' Problem in Tvrfromccde (real<-character)!'
        WRITE (*,*) ' Input character was: ',charnum            
        WRITE (*,*) ' Length of input character was ',lenchar   
        WRITE (*,*) ' Code was ',code                           
        CLOSE (fnumerr)
        STOP ' '
      ENDIF

      IF(charnum(lenchar:lenchar).EQ.'R')THEN
       i=lenchar-1
       charnum=charnum(1:i)
      ENDIF
      IF(charnum(lenchar:lenchar).EQ.'r')THEN
       i=lenchar-1
       charnum=charnum(1:i)
      ENDIF
      lenchar=Tvilent(charnum)
      IF(lenchar.EQ.0)THEN
       tvrfromc=-99.0
       RETURN
      ENDIF
      DO i=1,lenchar
       IF(charnum(i:i).NE.' '.OR.charnum(i:i).NE.'0')THEN
        pos=i
        newchar=charnum(pos:lenchar)
        EXIT
       ENDIF
      ENDDO
      lenchar=Tvilent(newchar)
      IF(lenchar.EQ.1)THEN
       READ(newchar,'(F1.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.2)THEN
       READ(newchar,'(F2.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.3)THEN
       READ(newchar,'(F3.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.4)THEN
       READ(newchar,'(F4.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.5)THEN
       READ(newchar,'(F5.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.6)THEN
       READ(newchar,'(F6.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.7)THEN
       READ(newchar,'(F7.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.8)THEN
       READ(newchar,'(F8.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.9)THEN
       READ(newchar,'(F9.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.10)THEN
       READ(newchar,'(F10.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.11)THEN
       READ(newchar,'(F11.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.12)THEN
       READ(newchar,'(F12.0)',ERR=9991)number
      ELSEIF(lenchar.GE.12)THEN
       CALL Getlun('ERROR.OUT',fnumerr)
       OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
       WRITE (fnumerr,*) ' Cannot convert to real - too large ',newchar
       WRITE (fnumerr,*) ' Code was ',code                           
       WRITE (*,*) ' Cannot convert to real - too large ',newchar
       WRITE (*,*) ' Code was ',code                           
       WRITE (*,*) ' Program will have to stop'
       CLOSE (fnumerr)
       STOP ' '
      ENDIF
      tvrfromccde=number
      RETURN
 9991 CONTINUE
      tvrfromccde=-99.0
      MESSAGE(1) = 'Problem in Tvrfromccde (real<-character)!'
      WRITE(MESSAGE(2),'(A19,A10)')'Trying to convert: ',newchar
      MESSAGE(3) = 'Returned -99. This may affect results'      
      CALL WARNING(3,'CSUTS',MESSAGE)

!      CALL Getlun ('WORK.OUT',fnumwrk)
!      INQUIRE (FILE = 'WORK.OUT',OPENED = FOPEN)
!      IF (.NOT.FOPEN)THEN
!        OPEN (UNIT = FNUMWRK,FILE = 'WORK.OUT')
!      ENDIF  
!      WRITE (fnumwrk,*) ' Problem in Tvrfromccde (real<-character)!'
!      WRITE (fnumwrk,*) ' Trying to convert: ',newchar

      RETURN

      END

!-----------------------------------------------------------------------

      FUNCTION Tl1upcase(inchar)

      ! Returns the upper case of a lower case letter.

      IMPLICIT  NONE

      CHARACTER (LEN=1) inchar,tl1upcase
      INTEGER           chaval

      INTRINSIC         ICHAR,CHAR

      SAVE

      chaval=Ichar(inchar)

      IF((chaval.LE.122).AND.(chaval.GE.97))THEN
       tl1upcase=CHAR(chaval-32)
      ELSE
       tl1upcase=inchar
      ENDIF

      END

!-----------------------------------------------------------------------

      FUNCTION tl10fromi(number)

      ! Changes integer to character

      IMPLICIT NONE
      EXTERNAL Tvilent, ltrim

      CHARACTER (LEN=10) tl10fromi
      CHARACTER (LEN=80) tchar
      INTEGER            number,digitnew(20),numb,newn
      INTEGER            i,tvilent,lentvc,lentchar,l

      INTRINSIC          MOD,CHAR

      SAVE

      IF(number.LT.10)THEN
       tl10fromi=CHAR(48+number)
      ELSE
       tl10fromi=' '
       DO i=1,80
        tchar(i:i)=' '
       ENDDO

       numb=0
       newn=number
       DO WHILE (newn.GE.10)
        numb=numb+1
        digitnew(numb)=MOD(newn,10)
        newn=newn/10
       ENDDO
       numb=numb+1
       digitnew(numb)=newn

       DO L=NUMB,1,-1
        tchar=CHAR(48+digitnew(l))
        CALL Ltrim(tchar)
        lentchar=Tvilent(tchar)
        tchar=tchar(1:lentchar)
        CALL Ltrim(tl10fromi)
        lentvc=Tvilent(tl10fromi)
        IF(lentvc.GT.0)THEN
         tl10fromi=tl10fromi(1:lentvc)//tchar(1:lentchar)
        ELSE
         tl10fromi=tchar(1:lentchar)
        ENDIF
       ENDDO
      ENDIF
      RETURN

      END

!-----------------------------------------------------------------------

      FUNCTION Yval(xyvals,xval)

      ! Y value corresponding to input x

      IMPLICIT NONE
      EXTERNAL Getlun

      INTEGER i,l,n,fnumerr
      REAL    xyvals(10,2),xval,yval

      SAVE

      n=0
      DO l=1,10
       n=n+1
       IF(xyvals(l,1).LE.-98.0)THEN
         n=l-1
        EXIT
       ENDIF
      ENDDO

      IF (n.LE.0) THEN
        CALL Getlun('ERROR.OUT',fnumerr)
        OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
        WRITE(fnumerr,*)' An array entered the Yval function with no  '
        WRITE(fnumerr,*)' values. This would cause an error in output '
        WRITE(fnumerr,*)' values, and may also cause the computer to'
        WRITE(fnumerr,*)' hang. Please check.'
        WRITE (*,*) ' An array entered the Yval function with no  '
        WRITE (*,*) ' values. This would cause an error in output '
        WRITE (*,*) ' values, and may also cause the computer to'
        WRITE (*,*) ' to hang.'
        WRITE (*,*) ' Program will have to stop'
        WRITE (*,*) ' Check WORK.OUT for details'
        CLOSE (fnumerr)
        STOP ' '
      ENDIF

      IF(xval.LE.xyvals(1,1))THEN
       Yval=xyvals(1,2)
       RETURN
      ENDIF
      IF(xval.GE.xyvals(n,1))THEN
       Yval=xyvals(n,2)
       RETURN
      ENDIF
      DO i=2,n
       IF(xval.LE.xyvals(i,1))EXIT
      ENDDO
      Yval=(xval-xyvals(i-1,1))*(xyvals(i,2)-xyvals(i-1,2))
      Yval=Yval/(xyvals(i,1)-xyvals(i-1,1))+xyvals(i-1,2)
      RETURN

      END

!-----------------------------------------------------------------------

      FUNCTION Yvalxy(xarray,yarray,xval)

      ! Y value corresponding to input x using two input arrays

      IMPLICIT NONE
      EXTERNAL Getlun

      REAL    xarray(10),yarray(10),yvalxy,xval
      INTEGER n,l,i,fnumerr

      SAVE

      n=0
      DO l=1,10
       n=n+1
       IF(xarray(l).LT.-98.0)THEN
         n=l-1
        EXIT
       ENDIF
      ENDDO

      IF(n.LE.0)THEN
        CALL Getlun('ERROR.OUT',fnumerr)
        OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
        WRITE(fnumerr,*)' An array entered the Yvalxy function with no'
        WRITE(fnumerr,*)' values. This would cause an error in output '
        WRITE(fnumerr,*)' values, and may also  cause the computer to'
        WRITE(fnumerr,*)' hang. Please check.'
        WRITE (*,*) ' An array entered the Yvalxy function with no  '
        WRITE (*,*) ' values. This would cause an error in output '
        WRITE (*,*) ' values, and may also cause the computer to'
        WRITE (*,*) ' to hang.'
        WRITE (*,*) ' Program will have to stop'
        WRITE (*,*) ' Check WORK.OUT for details'
        CLOSE (fnumerr)
        STOP ' '
      ENDIF

      IF(xval.LE.xarray(1))THEN
       Yvalxy=yarray(1)
       RETURN
      ENDIF

      IF(xval.GE.xarray(n))THEN
       Yvalxy=yarray(n)
       RETURN
      ENDIF

      DO i=2,n
       IF(xval.LE.xarray(i))EXIT
      ENDDO
      Yvalxy=(xval-xarray(i-1))*(yarray(i)-yarray(i-1))
      Yvalxy=Yvalxy/(xarray(i)-xarray(i-1))+yarray(i-1)

      RETURN

      END

!-----------------------------------------------------------------------

      FUNCTION Yval1(yvals,lowerc,upperc,xval)

      ! Y value corresponding to input x from array where x values
      ! are given at values that equal the indices of the y array.

      IMPLICIT NONE
      EXTERNAL Tvifromc, GETLUN

      INTEGER           l,n,lower,upper,xvali,tvifromc,adjust,fnumerr
      INTEGER           loweradj,upperadj,xvaliadj,lowerpos,upperpos
      REAL              yvals(*),xval,yval1,xvalfr
      CHARACTER (LEN=*) lowerc,upperc

      INTRINSIC         FLOAT,INT

      SAVE

      LOWER = TVIFROMC(LOWERC)
      UPPER = TVIFROMC(UPPERC)
      XVALI = INT(XVAL)
      XVALFR = XVAL - FLOAT(XVALI)

      ! Adjust for offsett
      ADJUST = 1 - LOWER
      LOWERADJ = LOWER + ADJUST
      UPPERADJ = UPPER + ADJUST
      XVALIADJ = INT(XVAL) + ADJUST

      n=0
      lowerpos = 0
      upperpos = 0
      DO l=LOWERADJ,UPPERADJ
       IF (yvals(l).GT.-98.0) THEN
         n = n+1
         IF (LOWERPOS.LE.0) lowerpos = l
       ELSE
         IF (LOWERPOS.GT.0) THEN
           IF (UPPERPOS.LE.0) UPPERPOS = L-1
         ENDIF
       ENDIF
      ENDDO
      IF (UPPERPOS.EQ.0) UPPERPOS=UPPERADJ

      IF(n.LE.0)THEN
        CALL Getlun('ERROR.OUT',fnumerr)
        OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
        WRITE(fnumerr,*)' An array entered the Yval1 function with no  '
        WRITE(fnumerr,*)' values. This would cause an error in output '
        WRITE(fnumerr,*)' values, and may also cause the computer to'
        WRITE(fnumerr,*)' hang. Please check.'
        WRITE (*,*) ' An array entered the Yval1 function with no  '
        WRITE (*,*) ' values. This would cause an error in output '
        WRITE (*,*) ' values, and may also cause the computer to'
        WRITE (*,*) ' to hang.'
        WRITE (*,*) ' Program will have to stop'
        WRITE (*,*) ' Check WORK.OUT for details'
        CLOSE (fnumerr)
        STOP ' '
      ENDIF

      IF (XVALIADJ.LT.LOWERPOS) THEN
        YVAL1 = YVALS(LOWERPOS)
      ELSEIF (XVALIADJ.GE.LOWERPOS .AND. XVALIADJ.LT.UPPERPOS) THEN
        YVAL1=YVALS(XVALIADJ)+XVALFR*(YVALS(XVALIADJ+1)-YVALS(XVALIADJ))
      ELSE
        YVAL1 = YVALS(UPPERPOS)
      ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE CSFIND(LUNUM,NAME,LNUM,FOUND)

      IMPLICIT NONE
      EXTERNAL CSUPCASE

      INTEGER           FOUND,I,LNUM,LUNUM
      CHARACTER (LEN=6) SECTION,NAME
      CHARACTER (LEN=1) CSUPCASE

      INTRINSIC LEN

      SAVE

      FOUND = 0
      LNUM  = 1

      DO I = 1, LEN(NAME)
         NAME(I:I) = CSUPCASE(NAME(I:I))
      END DO

      ! Loop to read through data file.

   10 IF (.TRUE.) THEN
         READ(LUNUM,'(A)',END=20) SECTION
         DO I = 1,LEN(SECTION)
            SECTION(I:I) = CSUPCASE(SECTION(I:I))
         END DO


         IF (NAME .EQ. SECTION) then
            ! String found, set FOUND to 1, and exit loop.
            FOUND = 1
            GOTO 20

          ELSE
            ! String not found, set FOUND to 0.
            FOUND = 0
         ENDIF

         LNUM = LNUM + 1
         GOTO 10
      ENDIF

   20 RETURN
      END


!-----------------------------------------------------------------------

      SUBROUTINE CSIGNORE(LUN,LINEXP,ISECT,CHARTEST)

      IMPLICIT NONE

      CHARACTER (LEN=80) BLANK
      CHARACTER (LEN=*)  CHARTEST
      INTEGER            LUN,LINEXP,ISECT

      SAVE

      DATA BLANK/'                                                    '/

      ISECT = 1
 30   READ(LUN,'(A)',ERR=70,END=70)CHARTEST
      LINEXP = LINEXP + 1
      ! Check to see if all of this section has been read
      IF(CHARTEST(1:1) .EQ. '*' )THEN
         ! End of section encountered
         ISECT = 2
         RETURN
      ENDIF

      ! Check for blank lines and comments (denoted by ! in column 1)
      IF(CHARTEST(1:1).NE.'!' .AND. CHARTEST(1:1).NE.'@') THEN
         IF(CHARTEST(1:80).NE.BLANK)THEN
            ! FOUND A GOOD LINE TO READ
            RETURN
         ENDIF
      ENDIF
      GO TO 30
      ! To read the next line
 70   ISECT = 0
      RETURN
      END

!-----------------------------------------------------------------------

      SUBROUTINE WTHDATA(FILEIO,WTHSTA,WTHFTYPE,wthdirfl,wthss)
      ! To find the name and location of weather data

      USE OSDefinitions

      IMPLICIT NONE
      EXTERNAL Tvilent, GETLUN, Finddir

      CHARACTER (LEN=*)   WTHSTA
      CHARACTER (LEN=128) ARG
      CHARACTER (LEN=250) CFGDFILE, FILEIO
      CHARACTER (LEN=250) WTHDIRFL
      CHARACTER (LEN=12)  WTHFLE, WTHTFLE, WTHSS
      CHARACTER (LEN=4)   TL4
      CHARACTER (LEN=3)   CFGCODE,WTHFTYPE
      CHARACTER (LEN=1)   WTHFLAG
      INTEGER             TVI1, TVI2, TVI3, TVILENT, ARGLEN, L
      INTEGER             FNUMERR, FILENUM
      LOGICAL             FFLAG

      INTRINSIC           LEN,MIN,TRIM

      SAVE

      CALL GETLUN('FILETMP',FILENUM)

      arg=' '
      tvi3=0
      arglen=0
!     CALL GETARG (0,arg,arglen)
!portability
      CALL GETARG (0,arg)
      arglen = len_trim(arg)
      DO tvi1=1,arglen
        IF (arg(tvi1:tvi1).EQ.'.') tvi3=tvi1-1
      ENDDO
      IF (tvi3.EQ.0) tvi3=arglen
      cfgdfile=ARG(1:TVI3)//'.CFG'
      ! Change cfg file name from module specific to general
      cfgdfile=TRIM(cfgdfile)
      DO L=LEN(CFGDFILE),1,-1
        IF (cfgdfile(L:L).EQ.slash) EXIT
      ENDDO
      IF (L.GT.1) THEN
        cfgdfile=CFGDFILE(1:L-1)//SLASH//'CROPSIM.CFG'
      ELSE
        cfgdfile(1:12)='CROPSIM.CFG '
      ENDIF

      cfgcode='WED'
      tl4='.'//WTHFTYPE
      wthdirfl=' '

      ! General file
      tvi1=MIN(4,tvi1)
      wthfle=wthsta(1:tvi1)//tl4

      ! Specific file
      tvi2=Tvilent(wthsta)
      tvi2=MIN(8,tvi2)
      wthtfle=wthsta(1:tvi2)//tl4

      ! Determine if 'special' weather file
      wthflag='N'
      IF (WTHSTA(TVI2-1:TVI2).NE.'01') wthflag='Y'

      tvi3=Tvilent(wthfle)
      IF(tvi3.LT.6)THEN
        CALL Getlun('ERROR.OUT',fnumerr)
        OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
        WRITE (fnumerr,*) ' No weather file nor station defined!! '
        WRITE (fnumerr,*) ' Check WORK.OUT for details'
        WRITE (*,*) ' No weather file nor station defined!! '
        WRITE (*,*) ' Program will have to stop'
        WRITE (*,*) ' Check WORK.OUT for details'
        CLOSE (fnumerr)
        STOP ' '
      ENDIF

      IF (WTHFLAG.EQ.'Y') THEN
        IF (LEN(TRIM(FILEIO)).GT.3) THEN
          fileio=TRIM(fileio)
          DO L=LEN(FILEIO),1,-1
            IF (fileio(L:L).EQ.slash) EXIT
          ENDDO
          WTHDIRFL=FILEIO(1:L-1)//SLASH//wthtfle
        ELSE
          WTHDIRFL=wthtfle
        ENDIF
        INQUIRE(FILE=wthdirfl,EXIST=fflag)
        wthss = wthtfle(1:TVI2)
        IF (fflag) RETURN
      ENDIF

      IF (L.GT.3) THEN
        WTHDIRFL=FILEIO(1:L-1)//SLASH//wthfle
      ELSE
        WTHDIRFL=wthfle
      ENDIF
      INQUIRE(FILE=wthdirfl,EXIST=fflag)
      wthss = wthfle(1:TVI1)

      IF(.NOT.fflag)THEN
       wthss = wthtfle(1:TVI2)
       IF (LEN(TRIM(FILEIO)).GT.3) THEN
         WTHDIRFL=FILEIO(1:L-1)//SLASH//wthtfle
       ELSE
         WTHDIRFL=wthtfle
       ENDIF
       INQUIRE(FILE=wthdirfl,EXIST=fflag)
      ENDIF

      IF (.NOT.fflag) THEN
       CALL Finddir(filenum,cfgdfile,cfgcode,wthfle,wthdirfl)
       INQUIRE(FILE=wthdirfl,EXIST=fflag)
       IF (.NOT.fflag) THEN
         CALL Finddir(filenum,cfgdfile,cfgcode,wthtfle,wthdirfl)
         INQUIRE(FILE=wthdirfl,EXIST=fflag)
         IF (.NOT.fflag) THEN
           CALL Getlun('ERROR.OUT',fnumerr)
           OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
           WRITE (fnumerr,*) ' Weather file not found!'
           WRITE (fnumerr,*) ' Station file     : ',wthfle
           WRITE (fnumerr,*) ' Station+year file: ',wthtfle
           WRITE (fnumerr,*) ' Check WORK.OUT for details'
           WRITE (*,*) ' Weather file not found!'
           WRITE (*,*) ' Station file     : ',wthfle
           WRITE (*,*) ' Station+year file: ',wthtfle
           WRITE (*,*) ' Program will have to stop'
           WRITE (*,*) ' Check WORK.OUT for details'
           CLOSE (fnumerr)
           STOP ' '
         ENDIF
       ENDIF
      ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      FUNCTION SLLAYERS(ARRAY,ASIZE)

      ! Returns number of layers in soil from array of layer bases

      IMPLICIT NONE
      EXTERNAL TVRFROMCCDE, TVRFROMC

      CHARACTER (LEN=*)  ASIZE
      REAL               ARRAY(*), TVR1, TVRFROMC, TVRFROMCCDE
      INTEGER            SIZE, SLLAYERS, I

      INTRINSIC          INT

      TVR1=TVRFROMCCDE('ASIZE',ASIZE)
      TVR1=TVRFROMC(ASIZE)
      SIZE=INT(TVR1)

      SLLAYERS=1
      DO I=2, SIZE
        IF (ARRAY(I) .GT. ARRAY(I-1)) THEN
          SLLAYERS=SLLAYERS + 1
        ELSE
          EXIT
        ENDIF
      ENDDO

      IF (ARRAY(1).LE.0.0) SLLAYERS=0

      RETURN

      END

!-----------------------------------------------------------------------

      FUNCTION SLDEPTH(ARRAY,ASIZE)

      ! Returns depth of soil profile

      IMPLICIT NONE
      EXTERNAL TVRFROMCCDE, TVRFROMC

      CHARACTER (LEN=*)  ASIZE
      REAL               ARRAY(*), TVR1, TVRFROMC, SLDEPTH, TVRFROMCCDE
      INTEGER            SIZE, I

      INTRINSIC          NINT

      TVR1=TVRFROMCCDE('ASIZE',ASIZE)
      TVR1=TVRFROMC(ASIZE)
      SIZE=NINT(TVR1)

      SLDEPTH=ARRAY(1)
      DO I=2,SIZE
        IF (ARRAY(I) .GT. ARRAY(I-1)) THEN
          SLDEPTH=SLDEPTH + ARRAY(I)
        ENDIF
      ENDDO

      RETURN

      END

!-----------------------------------------------------------------------

      FUNCTION NUMLAYER(ARRAY,ASIZE)

      ! Returns number of layers from an array of values

      IMPLICIT NONE
      EXTERNAL TVRFROMCCDE, TVRFROMC

      CHARACTER (LEN=*)  ASIZE
      REAL               ARRAY(*), TVR1, TVRFROMC, TVRFROMCCDE
      INTEGER            SIZE, NUMLAYER, I

      INTRINSIC          INT

      TVR1=TVRFROMCCDE('ASIZE',ASIZE)
      TVR1=TVRFROMC(ASIZE)
      SIZE=INT(TVR1)

      NUMLAYER=1
      DO I=2, SIZE
        IF (ARRAY(I).GT.0.0) THEN
          NUMLAYER=NUMLAYER + 1
        ELSE
          EXIT
        ENDIF
      ENDDO

      RETURN

      END

!-----------------------------------------------------------------------

      FUNCTION CSIDLAYR (NLAYR,DLAYR,FDEPTH)

      ! Determines layer for fertilizer placement, root elongation, etc.

      IMPLICIT  NONE

      INTEGER   NLAYR,L,CSIDLAYR
      REAL      DLAYR(NLAYR)
      REAL      FDEPTH
      REAL      DEPTH

      DEPTH  = 0.0
      CSIDLAYR = 1
      DO L = 1, NLAYR
         DEPTH  = DEPTH + DLAYR (L)
         IF (FDEPTH .LE. DEPTH) THEN
            CSIDLAYR = L
            GO TO 10
         ENDIF
      END DO

      CSIDLAYR = NLAYR

   10 CONTINUE

      RETURN

      END ! CSIDLAYR

!-----------------------------------------------------------------------

      FUNCTION CSUPCASE (INCHAR)

      IMPLICIT  NONE

      CHARACTER (LEN=1) INCHAR, CSUPCASE
      INTEGER           CHAVAL

      INTRINSIC         ICHAR,CHAR

      SAVE

      CHAVAL = ICHAR(INCHAR)

      IF ((CHAVAL.LE.122).AND.(CHAVAL.GE.97)) THEN
         CSUPCASE = CHAR(CHAVAL-32)
       ELSE
         CSUPCASE = INCHAR
      ENDIF

      END   !FUNCTION CSUPCASE


!-----------------------------------------------------------------------

      FUNCTION CSCURV(CTYPE,XB,X1,X2,XM,X)

      IMPLICIT NONE
      EXTERNAL Getlun

      CHARACTER (LEN=3) CTYPE
      REAL              CSCURV,XB,X1,X2,XM,X

      INTRINSIC         COS,MAX,MIN

      SAVE

      CSCURV = 1.0
      IF (CTYPE .EQ. 'NON' .OR. CTYPE .EQ. 'non') RETURN

      IF(CTYPE .EQ. 'LIN' .OR. CTYPE .EQ. 'lin') THEN
        CSCURV = 0.
        IF(X .GT. XB .AND. X .LT. X1)CSCURV = (X-XB)/(X1-XB)
        IF(X .GE. X1 .AND. X .LE. X2)CSCURV = 1.
        IF(X .GT. X2 .AND. X .LT. XM)CSCURV = 1.0 - (X-X2)/(XM-X2)
        CSCURV = MAX(CSCURV,0.0)
        CSCURV = MIN(CSCURV,1.0)
      ENDIF

      IF(CTYPE .EQ. 'QDR' .OR. CTYPE .EQ. 'qdr') THEN
        CSCURV = 0.
        IF(X .GT. XB .AND. X .LT. X1)CSCURV = 1. -((X1-X)/(X1-XB))**2
        IF(X .GE. X1 .AND. X .LE. X2)CSCURV = 1.
        IF(X .GT. X2 .AND. X .LT. XM)CSCURV = 1. - ((X-X2)/(XM-X2))**2
        CSCURV = MAX(CSCURV,0.0)
        CSCURV = MIN(CSCURV,1.0)
      ENDIF

      ! Type INL is the inverse linear with a minimum for use in
      ! photoperiod calculations. In this case, XM is the lowest
      ! relative rate, X1 and X2 are critical dayl

      IF(CTYPE .EQ. 'INL' .OR. CTYPE .EQ. 'inl') THEN
        CSCURV = 1.0
        IF(X.GT.X1 .AND. X.LT.X2)CSCURV = 1.-(1.-XM)*((X-X1)/(X2-X1))
        IF(X .GT. X2) CSCURV = XM
        CSCURV = MAX(CSCURV,XM)
        CSCURV = MIN(CSCURV,1.0)
      ENDIF

      ! Type SHO for use with short day plants.
      ! The curve is the inverse linear with a minimum for use in
      ! photoperiod calculations. In this case, XM is the lowest
      ! relative rate, X1 and X2 are critical dayl

      IF(CTYPE .EQ. 'SHO' .OR. CTYPE .EQ. 'sho') THEN
        IF (X .LE. X1) THEN
           CSCURV = 1.0
        ELSE IF ((X .GT. X1) .AND. (X .LT. X2)) THEN
           CSCURV = 1.-(1.-XM)*((X-X1)/(X2-X1))
        ELSE IF (X .GE. X2) THEN
          CSCURV = XM
        ENDIF
        CSCURV = MAX(CSCURV,XM)
        CSCURV = MIN(CSCURV,1.0)
      ENDIF

      ! Curve type LON for use with long day plants.
      ! Type is the inverse linear with a minimum for use in
      ! photoperiod calculations. In this case, XM is the
      ! lowest relative rate, X1 and X2 are critical dayl

      IF(CTYPE .EQ. 'LON' .OR. CTYPE .EQ. 'LON') THEN
        IF (X .LT. X2) THEN
           CSCURV = XM
        ELSE IF ((X .GE. X2) .AND. (X .LT. X1)) THEN
           CSCURV = 1.-(1.-XM)*((X1-X)/(X1-X2))
        ELSE
           CSCURV = 1.0
        ENDIF
        CSCURV = MAX(CSCURV,XM)
        CSCURV = MIN(CSCURV,1.0)
      ENDIF

      IF(CTYPE .EQ. 'SIN' .OR. CTYPE .EQ. 'sin') THEN
        CSCURV = 0.
        IF(X.GT.XB.AND.X.LT.X1)THEN
          CSCURV=0.5*(1.+COS(2.*22./7.*(X-X1)/(2.*(X1-XB))))
        ENDIF
        IF(X.GE.X1.AND.X.LE.X2)CSCURV = 1.
        IF(X.GT.X2.AND.X.LT.XM)THEN
          CSCURV = 0.5*(1.+COS(2.*22./7.*(X2-X)/(2.*(XM-X2))))
        ENDIF
        CSCURV = MAX(CSCURV,0.0)
        CSCURV = MIN(CSCURV,1.0)
      ENDIF

      END   !FUNCTION CURV
!-----------------------------------------------------------------------

      REAL FUNCTION CSYVAL (X,X1,Y1,X2,Y2)

      ! Function to Interpolate between two points (X1,Y1) and (X2,Y2)
      ! From Interpolate function in original Cassava model

      IMPLICIT NONE
      EXTERNAL Getlun

      REAL     X,X1,Y1,X2,Y2
      INTEGER  FNUMERR

      IF (X2 .EQ. X1) THEN
        CALL Getlun('ERROR.OUT',fnumerr)
        OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
        WRITE(fnumerr,*)' Division by zero in Function CS_YVAL!'
        WRITE(fnumerr,*)' Values were: '
        WRITE(fnumerr,*)'   Y1 ',Y1,' Y2 ',Y2
        WRITE(fnumerr,*)'   X  ',X,' X1 ',X1,' X2 ',X2
        WRITE(fnumerr,*)' Check WORK.OUT for more information'
        WRITE (*,*) ' Division by zero in Function CS-YVAL!'
        WRITE (*,*) ' Program will have to stop'
        WRITE (*,*) ' Check ERROR.OUT and WORK.OUT for details'
        CLOSE (fnumerr)
        STOP ' '
      ELSE
        CSYVAL = AMIN1(Y2,Y1+(Y2-Y1)*(X-X1)/(X2-X1))
      ENDIF

      END

