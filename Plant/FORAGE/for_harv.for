C=======================================================================
C  for_harv, Subroutine
C
C  Description
C-----------------------------------------------------------------------
C  Revision history
C
C  07/02/2003 KJB/SJR/PA?  Written.
C  05/07/2020 FO  Added new Y4K subroutine call to convert YRDOY
C  10/15/2020 FO  Fixed path issue for MOWFILE.
C  06/23/2021 FO  Update MOWFILE to handle paths with spaces.
C  01/28/2022 DP/FO/TF Added AutomaticMOW
C  01/28/2022 DP/TF  Added GDD option for AutomaticMOW
C-----------------------------------------------------------------------
C  INPUT  : 
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called :
C
C  Calls  :
C=======================================================================
      SUBROUTINE forage_harvest(CONTROL,FILECC, ATMOW, ATTP,
     &              RHOL,RHOS,PCNL,PCNST,SLA,RTWT,STRWT,   !Input
     &              WTLF,STMWT,TOPWT,TOTWT,WCRLF,WCRST,    !Input/Output
     &              WTNLF,WTNST,WNRLF,WNRST,WTNCAN,        !Input/Output
     &              AREALF,XLAI,XHLAI,VSTAGE,vstagp,canht, !Input/Output
     &              fhtot,FHTOTN, fhpctlf,fhpctn,FREQ,
     &              MOWC,RSPLC,HMFRQ,HMGDD,HMCUT, HMMOW,HRSPL,
     &              DWTCO, DWTLO, DWTSO, PWTCO, PWTLO, PWTSO,
     &              HMVS, WTCO, WTLO, WTSO, TAVG, MOWGDD,
     &              MOWCOUNT, TGMIN, VTO1, VTB1, MOWREF, 
     &              RSREF, YFREQ, YRSREF, YCUTHT, YCHMOW,
     &              XCUTHT, XCHMOW, XFRGDD, XFREQ, CUTDAY,
     &              PROLFF, PROSTF, pliglf, pligst)

!     2023-04-28 TF Removed unused variables in argument list:
!    CUHT

      USE MODULEDEFS
      USE ModuleData

      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, ERROR, IGNORE, Y2K_DOY, Y4K_DOY, yr_doy
      EXTERNAL TABEX, PARSE_HEADERS

      INTEGER MOWLUN,ISECT,ERR
      INTEGER,ALLOCATABLE,DIMENSION(:) :: TRNO,DATE
      INTEGER TRTNO,YRDOY,year,doy,run
!     INTEGER SEASON
      INTEGER LUNCRP,fhlun
      INTEGER LNUM,FOUND
      INTEGER I,MOWCOUNT,j
      integer,dimension(8) :: date_time
      INTEGER DYNAMIC,ERRNUM,LUNIO,PATHL  !LUNEXP,LINEXP,LNHAR,

      REAL,ALLOCATABLE,DIMENSION(:) :: MOW,RSPLF,MVS,rsht
      REAL FHLEAF,FHSTEM,FHVSTG
      REAL RHOL,RHOS,PCNL,PCNST,SLA
      REAL WTLF,STMWT,TOPWT,TOTWT,WCRLF,WCRST
      REAL WTNLF,WTNST,WNRLF,WNRST,WTNCAN,RTWT,STRWT
      REAL AREALF,XLAI,XHLAI,VSTAGE  !AREAH,
      REAL PROLFF,PROSTF,pliglf,pligst
      real canht,fhcrlf,fhcrst,fhtotn,fhtot,fhlfn,fhstn
      real fhpcho,fhpctlf,fhpctn,fhplig
      real vstagp,MOWC,RSPLC
! Unused variables: y,z,PELF,FMOW,RHMOW,CHMOW,FLFP,RHLFP,RSPLM

      REAL DWTCO, DWTLO, DWTSO, PWTCO, PWTLO, PWTSO
      REAL WTCO, WTLO, WTSO
      REAL FREQ,MOWREF !CUHT
!     REAL YHT

      REAL TABEX  ! Function subroutine - Lookup utility
      REAL HMCUT, RSREF
      INTEGER,dimension(6) :: IXFREQ
      REAL,dimension(6) :: XFREQ
      REAL,dimension(6) :: YFREQ
      INTEGER,dimension(6) :: IXCUTHT
      REAL,dimension(6) :: XCUTHT
      REAL,dimension(6) :: YCUTHT
      INTEGER,dimension(6) :: IXCHMOW
      REAL,dimension(6) :: XCHMOW
      REAL,dimension(6) :: YCHMOW
      INTEGER,dimension(6) :: IXFRGDD
      REAL,dimension(6) :: XFRGDD
      REAL,dimension(6) :: YRSREF
      REAL GDD, MOWGDD
      INTEGER HMFRQ, HMGDD, CUTDAY, HMVS
      INTEGER HMMOW, HRSPL !TF 2022-01-31 Smart version AutoMOW
      INTEGER CUTNO !Count number of cuts for AutoMOW
      REAL TAVG, TGMIN
      REAL TB(5), TO1(5) !, TO2(5) , TM(5)
      REAL VTO1, VTB1 !Vegetative coefficients
!      REAL,ALLOCATABLE,DIMENSION(:) :: canht
      
      character(len=1)  BLANK
      character(len=2)  crop
      CHARACTER(len=6)  SECTION,ERRKEY,trtchar
      character(len=10),parameter :: fhout='FORAGE.OUT'
      CHARACTER*12 MOWFILE
      CHARACTER*30 FILEIO
!     CHARACTER*78 MSG(2)
      CHARACTER*80 FILECC
      CHARACTER*80 PATHEX
      character(len=60) ename
      CHARACTER*80 MOW80
      character(len=180) fhoutfmt
      CHARACTER*80 C80
      CHARACTER*255 C255
!     CHARACTER*80 CHARTEST
!     CHARACTER*92 FILEX_P
      CHARACTER*92 FILEMOW
!     CHARACTER*6  FINDCH
!     CHARACTER*12 FILEX
!     CHARACTER*78 MESSAGE(2)

      INTEGER, PARAMETER :: MAXCOL = 50
      CHARACTER*15  HEADER(MAXCOL)
      INTEGER COL(MAXCOL,2), C1, C2, COUNT
      LOGICAL ATMOW
      CHARACTER*1 ATTP
      logical exists

      TYPE(CONTROLTYPE) CONTROL

      SAVE FILEMOW,TRNO,DATE,MOW,RSPLF,MVS,rsht,CUTNO


      PARAMETER  (ERRKEY = 'FRHARV')
      PARAMETER (BLANK  = ' ')

      DYNAMIC  = CONTROL % DYNAMIC
      FILEIO = CONTROL % FILEIO
      YRDOY  = CONTROL % YRDOY
      crop   = control % crop
      trtno  = control % trtnum
      run    = control % run
      ename  = control % ename

      MOWC = 0.0
      RSPLC = 0.0

C***********************************************************************
C***********************************************************************
!     Run Initialization - Called once per simulation
C***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN

        MOWGDD = 0.0
        MOWCOUNT = 1

        CALL PUT('MHARVEST','ISH_date',-99)
        CALL PUT('MHARVEST','ISH_wt',  -99.)

C----------------------------------------------------------
C     Open and read MOWFILE and PATH
C----------------------------------------------------------
C FO - 10/15/2020 Fixed path issue for MOWFILE.
        IF (ATMOW .EQV. .FALSE.) THEN
          CALL GETLUN('FILEIO', LUNIO)
          OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)

          READ (LUNIO,'(3(/),15X,A12,1X,A80)',IOSTAT=ERRNUM) mowfile,
     &       PATHEX
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,5)
          mowfile(10:12) = 'MOW'

          PATHL  = INDEX(PATHEX,BLANK)
          IF (PATHL .LE. 1) THEN
            FILEMOW = mowfile
          ELSE
            PATHL = LEN(TRIM(PATHEX))
            FILEMOW = PATHEX(1:(PATHL)) // mowfile
          ENDIF

          CLOSE(LUNIO)

          INQUIRE(FILE = MOWFILE, EXIST = exists)

          IF (ALLOCATED(MOW)) THEN
            deallocate(mow,trno,date,rsplf,mvs,rsht)
          ENDIF
          CALL GETLUN('MOWFILE',MOWLUN)
          OPEN (UNIT=MOWLUN,FILE=FILEMOW,STATUS='OLD',IOSTAT=ERR)
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,29,FILEMOW,LNUM)
  
          REWIND(MOWLUN)
  
          ISECT = 0
          MOWCOUNT = 0
          write(trtchar,'(i6)') trtno
          DO WHILE (ISECT.EQ.0)
            READ (MOWLUN,'(A80)',IOSTAT=ISECT) MOW80
            IF (MOW80(1:1).NE."@"
     &         .AND.MOW80(1:1).NE."!"
     &         .AND.MOW80(1:20).NE."                    "
     &         .and.mow80(1:6)==trtchar
     &         .AND.ISECT.EQ.0)THEN
               MOWCOUNT = MOWCOUNT + 1
            END IF
          END DO
          REWIND(MOWLUN)
  
          IF (MOWCOUNT.GT.0) THEN
            ALLOCATE(TRNO(MOWCOUNT),DATE(MOWCOUNT),MOW(MOWCOUNT))
            ALLOCATE(RSPLF(MOWCOUNT),MVS(MOWCOUNT),rsht(mowcount))
          ELSE
C           MOW file has no data for this treatment
            CALL ERROR(ERRKEY,2,MOWFILE,0)
            ALLOCATE(MOW(1))
            MOW (1) = -99
            RETURN
          END IF
  
          I = 0
          ISECT = 0
          DO WHILE (ISECT.EQ.0)
            READ (MOWLUN,'(A80)',IOSTAT=ISECT) MOW80
!           TF 05/22/2023 - Updated read method for mow file to handle 
!            dates in YYDDD and YYYYDDD format
            IF(MOW80(1:1).EQ."@") THEN
              CALL PARSE_HEADERS(MOW80, MAXCOL, HEADER, COUNT, COL)
            ENDIF
            IF (MOW80(1:1).NE."@"
     &         .AND.MOW80(1:1).NE."!"
     &         .AND.MOW80(1:20).NE."                    "
     &         .and.mow80(1:6)==trtchar
     &         .AND.ISECT.EQ.0)THEN
              I = I + 1
              DO J = 1, COUNT
                C1 = COL(J,1)
                C2 = COL(J,2)
                SELECT CASE (TRIM(HEADER(J)))
                 CASE('TRNO');READ(MOW80(C1:C2+1),*,IOSTAT=ERR) TRNO(I)
                 CASE('DATE');READ(MOW80(C1:C2),*,IOSTAT=ERR) DATE(I)
                 CASE('MOW');READ(MOW80(C1:C2),*,IOSTAT=ERR) MOW(I)
                 CASE('RSPLF');READ(MOW80(C1:C2),*,IOSTAT=ERR) RSPLF(I)
                 CASE('MVS');READ(MOW80(C1:C2),*,IOSTAT=ERR) MVS(I)
                 CASE('RSHT');READ(MOW80(C1:C2),*,IOSTAT=ERR) rsht(I)
                END SELECT
              END DO
!              READ (MOW80,'(2I6,4F6.0)',IOSTAT=ISECT)
!     &                  TRNO(I),DATE(I),MOW(I),RSPLF(I),MVS(I),rsht(i)
C   FO -  05/07/2020 Add new Y4K subroutine call to convert YRDOY
              !CALL Y2K_DOY(DATE(I))
              CALL Y4K_DOY(DATE(I),MOWFILE,I,ERRKEY,1)
            END IF
          END DO
        ELSE
          IF(ATTP .EQ. 'W' .AND. HMFRQ .LE. 0) THEN
            CALL ERROR (ERRKEY,3,MOWFILE,LNUM)
          ENDIF
          IF(ATTP .EQ. 'X' .AND. HMGDD .LE. 0) THEN
            CALL ERROR (ERRKEY,4,MOWFILE,LNUM)
          ENDIF        
          IF(ATTP .EQ. 'Y' .AND. HMFRQ .LE. 0) THEN
            CALL ERROR (ERRKEY,3,MOWFILE,LNUM)
          ENDIF              
          IF(ATTP .EQ. 'Z' .AND. HMGDD .LE. 0) THEN
            CALL ERROR (ERRKEY,4,MOWFILE,LNUM)
          ENDIF
          IF(HMCUT .LT. 0.0) CALL ERROR (ERRKEY,6,MOWFILE,LNUM)
          IF(HMVS .LT. 0 .OR. HMVS .GT. 80) THEN
            CALL ERROR (ERRKEY,8,MOWFILE,LNUM)
          ENDIF
          !HMMOW and HRSPL are used only for SmartMOW
          IF(ATTP .EQ. 'Y' .OR. ATTP .EQ. 'Z') THEN
            IF(HMMOW .LT. 0.0) CALL ERROR (ERRKEY,7,MOWFILE,LNUM)
            IF(HRSPL .GT. 100 .OR. HRSPL .LT. 0) THEN
              CALL ERROR (ERRKEY,5,MOWFILE,LNUM)
            ENDIF
          ENDIF
          
        ENDIF

        ! OPEN AND READ SPECIES FILE
        CALL GETLUN('FILEC', LUNCRP)
        OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)
        SECTION = '!*PLAN'
        CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(ERRKEY, 1, FILECC, LNUM)
        ELSE
          CALL IGNORE(LUNCRP,LNUM,ISECT,MOW80)
          READ(MOW80,'(12X,F6.0,12X,F6.0)',IOSTAT=ERR) PROLFF, PROSTF
          do j=1,5; CALL IGNORE(LUNCRP,LNUM,ISECT,MOW80); end do
          READ(MOW80,'(2f6.0)',IOSTAT=ERR) pliglf, pligst
!-----------------------------------------------------------------------
!       Find Phenology Section in FILEC and read cardinal temperatures
!       for GDD calculations as harvest frequency option
!-----------------------------------------------------------------------
          CALL GETLUN('FILEC', LUNCRP)
          OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)

          SECTION = '!*PHEN'
          CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          READ(C80,'(4F6.1)') TB(1), TO1(1)
  
          IF(ATTP .EQ. 'W' .OR. ATTP .EQ. 'X') THEN
            SECTION = '!*STUB'
            CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
            IF (FOUND .EQ. 0) CALL ERROR (ERRKEY,9,MOWFILE,LNUM)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
            READ(C255,'(2F6.0)',IOSTAT=ERRNUM)  MOWREF, RSREF
            CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
            READ(C255,'(6I6)',IOSTAT=ERRNUM) (IXFREQ(I),I=1,6)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
            READ(C255,'(6I6)',IOSTAT=ERRNUM) (IXFRGDD(I),I=1,6)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
            READ(C255,'(6F6.2)',IOSTAT=ERRNUM) (YFREQ(I),I=1,6)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
            READ(C255,'(6F6.2)',IOSTAT=ERRNUM) (YRSREF(I),I=1,6)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
            READ(C255,'(6I6)',IOSTAT=ERRNUM) (IXCUTHT(I),I=1,6)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
            READ(C255,'(6F6.2)',IOSTAT=ERRNUM) (YCUTHT(I),I=1,6)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
            READ(C255,'(6I6)',IOSTAT=ERRNUM) (IXCHMOW(I),I=1,6)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
            READ(C255,'(6F6.2)',IOSTAT=ERRNUM) (YCHMOW(I),I=1,6)
  
            XCUTHT = IXCUTHT
            XCHMOW = IXCHMOW
            XFRGDD = IXFRGDD
            IF(ATTP .EQ. 'W') THEN
              XFREQ = IXFREQ
            ELSEIF( ATTP .EQ. 'X') THEN
              XFREQ = IXFRGDD
            ENDIF
          ENDIF
  
          VTO1 = TO1(1)
          VTB1 = TB(1)
          TGMIN = VTO1 - VTB1
        
          CLOSE(LUNCRP)
          
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        END IF
!***********************************************************************
!***********************************************************************
!     Seasonal Initialization
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
        MOWGDD = 0.0
        CUTNO = 1

        CALL PUT('MHARVEST','ISH_date',-99)
        CALL PUT('MHARVEST','ISH_wt',  -99.)

!***********************************************************************
!***********************************************************************
!     Daily Rate Calculations
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. RATE) THEN
C-----------------------------------------------------------------------
        IF(ATMOW .EQV. .TRUE.) THEN
          IF(ATTP .EQ. 'W' .OR. ATTP .EQ. 'Y') THEN
            FREQ = HMFRQ
            CUTDAY = MOD(MOWCOUNT,HMFRQ)
            MOWGDD = 0 !It will not accumulate GDD if there is HMFRQ
          ENDIF
          IF(ATTP .EQ. 'Z' .OR. ATTP .EQ. 'X') THEN
            FREQ = HMGDD
            CUTDAY = 1
          ENDIF
        ENDIF

!***********************************************************************
!***********************************************************************
!     Daily Integration
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
C-----------------------------------------------------------------------
        !Daily Senescence
        DWTCO = WTCO - PWTCO
        DWTLO = WTLO - PWTLO
        DWTSO = WTSO - PWTSO
        DWTSO = WTSO - PWTSO       
        DWTSO = WTSO - PWTSO
!----------------------------------------------------------------------

      IF (.NOT.ALLOCATED(MOW) .AND. ATMOW .EQV. .FALSE.) THEN

        DO I=1,SIZE(MOW)
          if(date(i)==yrdoy) then
            IF (MOW(I).GE.0.and.trno(i)==trtno)then
              if(mow(i)/10<topwt) THEN
                FHLEAF=0
                FHSTEM=0
                FHVSTG=0
                IF(RSPLF(I)>=0)THEN
                  FHLEAF=WTLF-(MOW(I)/10)*RSPLF(I)/100
                  FHSTEM=STMWT-(MOW(I)/10)*(1.0-RSPLF(I)/100)
                ELSE
                  FHLEAF=WTLF-(MOW(I)/10)*WTLF/(WTLF+STMWT)
                  FHSTEM=STMWT-(MOW(I)/10)*STMWT/(WTLF+STMWT)
                END IF

                FHLEAF=MAX(FHLEAF,0.0)
                FHSTEM=MAX(FHSTEM,0.0)
                FHVSTG=MAX(MVS(I),0.0)
                canht=max(rsht(i)/100,0.0)
!               canht=max(rsht(i),0.0)     !enter rsht in cm

                fhtot = fhleaf+fhstem

                fhlfn = fhleaf*pcnl/100
                fhstn = fhstem*pcnst/100
                fhtotn = fhlfn+fhstn

                fhcrlf = fhleaf*rhol
                fhcrst = fhstem*rhos

                fhpctn = fhtotn/fhtot*100
                fhplig = (fhleaf*pliglf+fhstem*pligst)/fhtot*100
                fhpcho = (fhcrlf+fhcrst)/fhtot*100
                fhpctlf = fhleaf/fhtot*100

                WTLF  = WTLF - FHLEAF
                STMWT = STMWT - FHSTEM
                TOPWT = TOPWT - FHLEAF - FHSTEM
                TOTWT = TOTWT - FHLEAF - FHSTEM

                WCRLF = WTLF*RHOL
                WCRST = STMWT*RHOS

                WTNLF  = WTLF*PCNL/100.
                WTNST  = STMWT*PCNST/100.
                WTNCAN = WTNCAN - FHLEAF*PCNL/100. - FHSTEM*PCNST/100.

                IF ((WTLF - WCRLF) .GT. 0.0) THEN
                  WNRLF = MAX (WTNLF - PROLFF*0.16*(WTLF-WCRLF), 0.0)
                ELSE
                  WNRLF = 0.0
                ENDIF

                IF ((STMWT - WCRST) .GT. 0.0) THEN
                  WNRST = MAX (WTNST - PROSTF*0.16*(STMWT-WCRST), 0.0)
                ELSE
                  WNRST = 0.0
                ENDIF

                AREALF = WTLF*SLA
                XLAI   = AREALF/10000.
                XHLAI  = XLAI
                
                VSTAGE = FHVSTG     
                vstagp = vstage

              else

                fhtot = 0
                
                fhlfn = 0
                fhstn = 0
                fhtotn = 0
                
                fhcrlf = 0
                fhcrst = 0
                
                fhpctn = 0
                fhplig = 0
                fhpcho = 0
                
                fhpctlf = 0


              ENDIF

              CALL GETLUN('FORHARV', fhlun)

              INQUIRE(file=FHOUT,EXIST=EXISTS)
              IF (exists.and.(run/=1.or.i/=1)) THEN
                OPEN(FILE=FHOUT,UNIT=FHLUN,POSITION='APPEND')
              ELSE
                 call date_and_time(values=date_time)
                 OPEN(FILE=FHOUT,UNIT=FHLUN)
                 rewind(fhlun)
                 fhoutfmt =
     &     "('*Forage Model Harvest Output: ',A8,A2,1X,A,1X,"//
     &     "'DSSAT Cropping System Model Ver. '"//
     &     ",I1,'.',I1,'.',I1,'.',"//
     &     "I3.3,1X,A10,4X,"//
     &     "A3,' ',I2.2,', ',I4,'; ',I2.2,':',I2.2,':',I2.2/)"
                 WRITE (fhlun,fhoutfmt) mowfile(1:8),crop,trim(ename),
     &             Version,VBranch,
     &             MonthTxt(DATE_TIME(2)), DATE_TIME(3), DATE_TIME(1),
     &             DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
                 WRITE(fhlun,'(a)')
     &           '@RUN FILEX    CR TRNO FHNO YEAR DOY'//
     &           ' RCWAH RLWAH RSWAH RSRWH RRTWH RLAIH'//
     &           ' FHWAH FHNAH FHN%H FHC%H FHLGH FHL%H'//
!     &           ' FHWAH FHNAH FHN%H FHC%H FHLGH FHL%H FHAGE IVOMD'//
     &           '  MOWC RSPLC'
               end if
               call yr_doy(yrdoy,year,doy)
               write(fhoutfmt,'(a)') '(i4,x,a8,a3,2(i5),i5,i4,'//
     &            '5(i6),f6.2,2(i6),3(f6.2),f6.1,x,f5.0,F6.1,F6.1)'
               WRITE(fhlun,fhoutfmt)
     &           run,mowfile(1:8),crop,trtno,i,year,doy,
     &           Nint(topwt*10.),Nint(wtlf*10.),Nint(stmwt*10.),
     &           Nint(strwt*10.),Nint(rtwt*10.),xlai,
     &           Nint(fhtot*10.),Nint(fhtotn*10.),
     &           fhpctn,fhpcho,fhplig,fhpctlf,
     &           MOWC,RSPLC

!     &           -99,-99.0,MOWC,RSPLC
               close(fhlun)

!              Send out amount harvested today for MgmtEvent.OUT file
               CALL PUT('MHARVEST','ISH_date',YRDOY)
               CALL PUT('MHARVEST','ISH_wt',  fhtot*10.)
               
               if(date(i)==yrdoy.and.trno(i)==trtno) then
                PWTCO = WTCO
                PWTLO = WTLO
                PWTSO = WTSO
                DWTCO = WTCO - PWTCO
                DWTLO = WTLO - PWTLO
                DWTSO = WTSO - PWTSO
               endif
              !TF - Variables are being deallocated on RUNINIT
              !if(i==size(mow)) deallocate(mow,trno,date,rsplf,mvs,rsht)

               RETURN
             end if  !(MOW(I).GE.0.and.trno(i)==trtno)
           ENDIF  !date(i)==yrdoy
        ENDDO
      ENDIF

!***********************************************************************
! AUTOMOW calculations (DP,KJB,WP,FO,TF)
!***********************************************************************
      ! DP/TF - 01/28/2022 Added degree days (GDD) option
      IF(ATMOW .EQV. .TRUE.) THEN
        IF(CUTDAY .EQ. 0 .OR.
     &        (MOWGDD .GE. HMGDD .AND. HMGDD .GT. 0)) THEN
            !DP/TF 2022-01-31 Switch to complete version AutoMOW
          IF(ATTP .EQ. 'W' .OR. ATTP .EQ. 'X') THEN
            MOWC = (TABEX(YFREQ, XFREQ, FREQ, 6) * MOWREF) *
     &          (TABEX(YCUTHT, XCUTHT, HMCUT*100, 6)) *
     &          (TABEX(YCHMOW, XCHMOW, topwt, 6))
                RSPLC = (TABEX(YRSREF, XFREQ, FREQ, 6) * RSREF)
            !DP/TF 2022-01-31 Switch to simple version AutoMOW
          ELSEIF(ATTP .EQ. 'Y' .OR. ATTP .EQ. 'Z') THEN
            MOWC = MAX(HMMOW,0)
            RSPLC = MAX(HRSPL,0)
          ENDIF
          MOWGDD = 0.0
        ELSE
          MOWCOUNT = MOWCOUNT + 1
      !DP/TF 2022-01-31 GDD calculations as harvest frequency option
          IF(TAVG .GT. VTB1) THEN
            GDD = TAVG - VTB1
                  !GDD = (((TMAX+TMIN)/2) - TB(1)) 
          ELSE
            GDD = 0.0
          ENDIF
          IF (GDD .GT. TGMIN) GDD = TGMIN
          GDD = MAX(GDD, 0.0)
          MOWGDD = MOWGDD + GDD
          RETURN
        ENDIF

        IF (MOWC.GE.0) THEN
          IF(MOWC/10<topwt) THEN
            FHLEAF=0
            FHSTEM=0
            FHVSTG=0
            IF(RSPLC>=0)THEN
              FHLEAF=WTLF-(MOWC/10)*RSPLC/100
              FHSTEM=STMWT-(MOWC/10)*(1.0-RSPLC/100)
            ELSE
              FHLEAF=WTLF-(MOWC/10)*WTLF/(WTLF+STMWT)
              FHSTEM=STMWT-(MOWC/10)*STMWT/(WTLF+STMWT)
            END IF
            FHLEAF = MAX(FHLEAF,0.0)
            FHSTEM = MAX(FHSTEM,0.0)
            FHVSTG = HMVS
            canht  = max(HMCUT/100,0.0)
            !             canht=max(rsht(i),0.0)     !enter rsht in cm


            fhtot = fhleaf+fhstem

            fhlfn = fhleaf*pcnl/100
            fhstn = fhstem*pcnst/100
            fhtotn = fhlfn+fhstn

            fhcrlf = fhleaf*rhol
            fhcrst = fhstem*rhos

            IF(fhtot .GT. 0.0) THEN
              fhpctn = fhtotn/fhtot*100
              fhplig = (fhleaf*pliglf+fhstem*pligst)/fhtot*100
              fhpcho = (fhcrlf+fhcrst)/fhtot*100
              fhpctlf = fhleaf/fhtot*100
            ELSE
              fhpctn = 0
              fhplig = 0
              fhpcho = 0
              fhpctlf = 0
            ENDIF           

            WTLF  = WTLF - FHLEAF
            STMWT = STMWT - FHSTEM
            TOPWT = TOPWT - FHLEAF - FHSTEM
            TOTWT = TOTWT - FHLEAF - FHSTEM

            WCRLF = WTLF*RHOL
            WCRST = STMWT*RHOS

            WTNLF  = WTLF*PCNL/100.
            WTNST  = STMWT*PCNST/100.
            WTNCAN = WTNCAN - FHLEAF*PCNL/100. - FHSTEM*PCNST/100.

            IF ((WTLF - WCRLF) .GT. 0.0) THEN
              WNRLF = MAX (WTNLF - PROLFF*0.16*(WTLF-WCRLF), 0.0)
            ELSE
              WNRLF = 0.0
            ENDIF

            IF ((STMWT - WCRST) .GT. 0.0) THEN
              WNRST = MAX (WTNST - PROSTF*0.16*(STMWT-WCRST), 0.0)
            ELSE
              WNRST = 0.0
            ENDIF

            AREALF = WTLF*SLA
            XLAI   = AREALF/10000.
            XHLAI  = XLAI

            VSTAGE = FHVSTG
            vstagp = vstage

          ELSE
            fhtot = 0

            fhlfn = 0
            fhstn = 0
            fhtotn = 0

            fhcrlf = 0
            fhcrst = 0

            fhpctn = 0
            fhplig = 0
            fhpcho = 0

            fhpctlf = 0

          ENDIF


          CALL GETLUN('FORHARV', FHLUN)

          INQUIRE(file=FHOUT,EXIST=EXISTS)

          IF (exists) THEN
            OPEN(FILE=FHOUT,UNIT=FHLUN,POSITION='APPEND')
          ELSE
            CALL date_and_time(values=date_time)
            OPEN(FILE=FHOUT,UNIT=FHLUN)
            REWIND(FHLUN)
            fhoutfmt =
     &     "('*Forage Model Harvest Output: ',A8,A2,1X,A,1X,"//
     &     "'DSSAT Cropping System Model Ver. '"//
     &     ",I1,'.',I1,'.',I1,'.',"//
     &     "I3.3,1X,A10,4X,"//
     &     "A3,' ',I2.2,', ',I4,'; ',I2.2,':',I2.2,':',I2.2/)"
            WRITE (fhlun,fhoutfmt) mowfile(1:8),crop,trim(ename),
     &             Version,VBranch,
     &             MonthTxt(DATE_TIME(2)), DATE_TIME(3), DATE_TIME(1),
     &             DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
            WRITE(fhlun,'(a)')
     &           '@RUN FILEX    CR TRNO FHNO YEAR DOY'//
     &           ' RCWAH RLWAH RSWAH RSRWH RRTWH RLAIH'//
     &           ' FHWAH FHNAH FHN%H FHC%H FHLGH FHL%H'//
     &           '  MOWC RSPLC'
          ENDIF

          call yr_doy(yrdoy,year,doy)
          write(fhoutfmt,'(a)') '(i4,x,a8,a3,2(i5),i5,i4,'//
     &          '5(i6),f6.2,2(i6),3(f6.2),f6.1,x,f5.0,F6.1,F6.1)'
          WRITE(fhlun,fhoutfmt)
     &         run,mowfile(1:8),crop,trtno,CUTNO,year,doy,
     &         Nint(topwt*10.),Nint(wtlf*10.),Nint(stmwt*10.),
     &         Nint(strwt*10.),Nint(rtwt*10.),xlai,
     &         Nint(fhtot*10.),Nint(fhtotn*10.),
     &         fhpctn,fhpcho,fhplig,fhpctlf,
     &         MOWC,RSPLC
          CUTNO = CUTNO + 1
          close(fhlun)

!         Send out amount harvested today for MgmtEvent.OUT file
          IF (fhtot > 0.0) THEN
            CALL PUT('MHARVEST','ISH_date',YRDOY)
            CALL PUT('MHARVEST','ISH_wt',  fhtot*10.)
          ENDIF

          IF(CUTDAY .EQ. 0) THEN
            PWTCO = WTCO
            PWTLO = WTLO
            PWTSO = WTSO
            DWTCO = WTCO - PWTCO
            DWTLO = WTLO - PWTLO
            DWTSO = WTSO - PWTSO

            MOWCOUNT = 1
          ENDIF
        ENDIF


!***********************************************************************
!***********************************************************************
!     End of Season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------






      ENDIF
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************

      END !SUBROUTINE FORAGEHARVEST
