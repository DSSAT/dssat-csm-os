C=======================================================================
C  for_harv, Subroutine
C
C  Description
C-----------------------------------------------------------------------
C  Revision history
C
C  --/--/---- DP  Written.
C  05/07/2020 FO  Added new Y4K subroutine call to convert YRDOY
C  10/15/2020 FO  Fixed path issue for MOWFILE.
C  06/23/2021 FO  Update MOWFILE to handle paths with spaces.
C  01/28/2022 FO/DP/TF  Added AutomaticMOW
C  01/28/2022 TF/DP  Added GDD option for AutomaticMOW
C  01/29/2022 TF  Added protections and warnings for AutomaticMOW
C-----------------------------------------------------------------------
C  INPUT  : 
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called :
C
C  Calls  :
C=======================================================================
      SUBROUTINE forage_harvest(CONTROL,FILECC, ATMOW,
     &                RHOL,RHOS,PCNL,PCNST,SLA,RTWT,STRWT,!Input
     &                WTLF,STMWT,TOPWT,TOTWT,WCRLF,WCRST, !Input/Output
     &                WTNLF,WTNST,WNRLF,WNRST,WTNCAN,     !Input/Output
     &                AREALF,XLAI,XHLAI,VSTAGE,vstagp,canht,     !Input/Output
     &                fhtot,FHTOTN, fhpctlf,fhpctn,FREQ,CUHT,
     &                MOWC,RSPLC,HMFRQ,HMGDD,HMCUT, HMMOW,HRSPL, 
     &                DWTCO, DWTLO, DWTSO, PWTCO, PWTLO, PWTSO,
     &                WTCO, WTLO, WTSO,TMAX,TMIN)
      
      USE MODULEDEFS

      IMPLICIT NONE

      INTEGER MOWLUN,ISECT,ERR
      INTEGER,ALLOCATABLE,DIMENSION(:) :: TRNO,DATE
      INTEGER TRTNO,YRDOY,year,doy,run
      INTEGER SEASON
      INTEGER LUNCRP,fhlun
      INTEGER LNUM,FOUND
      INTEGER I,MOWCOUNT,j
      integer,dimension(8) :: date_time
      INTEGER DYNAMIC,LUNEXP,ERRNUM,LINEXP,LNHAR,LUNIO,PATHL

      REAL,ALLOCATABLE,DIMENSION(:) :: MOW,RSPLF,MVS,rsht
      REAL FHLEAF,FHSTEM,FHVSTG
      REAL RHOL,RHOS,PCNL,PCNST,SLA
      REAL WTLF,STMWT,TOPWT,TOTWT,WCRLF,WCRST
      REAL WTNLF,WTNST,WNRLF,WNRST,WTNCAN,RTWT,STRWT
      REAL AREALF,XLAI,AREAH,XHLAI,VSTAGE
      REAL PROLFF,PROSTF,pliglf,pligst
      real canht,fhcrlf,fhcrst,fhtotn,fhtot,fhlfn,fhstn
      real fhpcho,fhpctlf,fhpctn,fhplig
      real vstagp,MOWC,RSPLC,y,z,PELF,FMOW,RHMOW,CHMOW,FLFP,RHLFP,RSPLM
      REAL DWTCO, DWTLO, DWTSO, PWTCO, PWTLO, PWTSO
      REAL WTCO, WTLO, WTSO
      REAL FREQ,CUHT,YHT,MOWREF
      REAL TABEX  ! Function subroutine - Lookup utility
      REAL HMCUT, RSREF, CUTPL, CNHREF
      INTEGER,dimension(6) :: IXFREQ
      REAL,dimension(6) :: XFREQ
      REAL,dimension(6) :: YFREQ
      INTEGER,dimension(6) :: IXCUTHT
      REAL,dimension(6) :: XCUTHT
      REAL,dimension(6) :: YCUTHT
      INTEGER,dimension(6) :: IXCHMOW
      REAL,dimension(6) :: XCHMOW
      REAL,dimension(6) :: YCHMOW
      INTEGER,dimension(6) :: IXRSREF
      REAL,dimension(6) :: XRSREF
      REAL,dimension(6) :: YRSREF
      INTEGER,dimension(6) :: IXCUTPL
      REAL,dimension(6) :: XCUTPL
      REAL,dimension(6) :: YCUTPL
      REAL GDD, MOWGDD
      INTEGER HMFRQ, HMGDD, CUTDAY
      INTEGER HMMOW, HRSPL !TF 2022-01-31 Simple version AutoMOW 
      REAL TMAX
      REAL TMIN
      REAL TB(5), TO1(5), TO2(5), TM(5)
!      REAL,ALLOCATABLE,DIMENSION(:) :: canht
      
      character(len=1)  BLANK
      character(len=2)  crop
      CHARACTER(len=6)  SECTION,ERRKEY,trtchar
      character(len=10),parameter :: fhout='FORAGE.OUT'
      CHARACTER*12 MOWFILE
      CHARACTER*30 FILEIO
      CHARACTER*78 MSG(3)
      CHARACTER*80 FILECC
      CHARACTER*80 PATHEX
      character(len=60) ename
      CHARACTER*80 MOW80
      character(len=180) fhoutfmt
      CHARACTER*80 C80
      CHARACTER*255 C255
      CHARACTER*80 CHARTEST
      CHARACTER*92 FILEX_P
      CHARACTER*92 FILEMOW
      CHARACTER*6  FINDCH
      CHARACTER*12 FILEX
      CHARACTER*78 MESSAGE(2)

      LOGICAL ATMOW
      logical exists
      
      TYPE(CONTROLTYPE) CONTROL
      
      SAVE MOWGDD
      SAVE MOWCOUNT


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

C----------------------------------------------------------      
C     Open and read MOWFILE and PATH 
C----------------------------------------------------------
C FO - 10/15/2020 Fixed path issue for MOWFILE.
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
        IF (exists .EQV. .FALSE.) THEN
          MSG(1) = "Warning: File not found."
          WRITE(MSG(2), 100) MOWFILE
  100     FORMAT('File: ', A12)
          MSG(3) = "Automatic MOW will be used for this simulation."
          CALL WARNING (3, ERRKEY, MSG)

          IF(HMFRQ .LE. 0 .AND. HMGDD .LE. 0) THEN
            MSG(1) = "Values of harvest frequency (day and GDD) are missing."
            WRITE(MSG(2), 110)
  110       FORMAT('A default value 28 harvest frequency (days) ',
     &                "is being used for this simulation.")
            MSG(3) = "Which may produce inaccurate results."
            CALL WARNING(3,ERRKEY,MSG)
          ELSEIF(HMFRQ .LE. 0) THEN
            MSG(1) = "Value of harvest frequency (days) is missing."
            MSG(3) = 'Value of harvest frequency (GDD) will be used.'
            CALL WARNING(3,ERRKEY,MSG)
          ELSEIF(HMGDD .LE. 0) THEN
            MSG(1) = "Value of harvest frequency (GDD) is missing."
            MSG(3) = 'Value of harvest frequency (days) will be used.'
            CALL WARNING(3,ERRKEY,MSG)
          ELSEIF(HMFRQ .GT. 0 .AND. HMGDD .GT. 0) THEN
            MSG(1) = "Values of harvest frequency (day and GDD) were provided."
            MSG(3) = 'Value of harvest frequency (days) will be used.'
            CALL WARNING(3,ERRKEY,MSG)
          ENDIF
          
          IF(HMCUT .LE. 0.0) THEN
            MSG(1) = "Value of cutting height is missing. "
            WRITE(MSG(2), 120)
  120       FORMAT('A default value 0.10 is being used for ',
     &                 "this simulation.")
            MSG(3) = "Which may produce inaccurate results."
            CALL WARNING(3,ERRKEY,MSG)
          ENDIF
        
        ENDIF


!***********************************************************************
!***********************************************************************
!     Daily integration
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
C-----------------------------------------------------------------------

C----------------------------------------------------------
      
!      YHT=canht
!      do j=1,size(canht); YHT(size(canht))=canht; end do
!      WRITE(5000,'(F6.3)') YHT
!C----------------------------------------------------------      
!!MOWC - Automatic MOW - post harvest stubble mass and %leaf 
!!       in the stubble calculation (Diego): 
!C----------------------------------------------------------
!! OPEN AND READ SPECIES FILE
!      CALL GETLUN('FILEC', LUNCRP)      
!      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
!        SECTION = '!*CANO'
!      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
!      do j=1,8; CALL IGNORE(LUNCRP,LNUM,ISECT,C255); end do
!        READ(C255,'(F6.1)') FMOW
!      CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
!        READ(C255,'(F6.1)') RHMOW
!      CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
!        READ(C255,'(F6.1)') CHMOW
!      CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
!        READ(C255,'(F6.2)') FLFP
!      CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
!        READ(C255,'(F6.2)') RHLFP
!      CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
!        READ(C255,'(F6.1)') RSPLM
!      CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
!        CLOSE (LUNCRP)
!        WRITE(1050,'(F10.3)') NHGT
!C--------------------------------------------------
!TEMPORARY AUTO-HARVEST ROUTINE
!!OPEN AND READ FILEX
!      !CALL IPHAR (LUNEXP) 
!      !OPEN (LUNEXP,FILE=FILEX_P, STATUS = 'OLD',IOSTAT=ERRNUM)
!      !    FINDCH='*HARVE'
!      !CALL FIND (LUNEXP,FINDCH,LINEXP)
!      !CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
!      !READ (CHARTEST,'(44X,2F5.0)') FREQ,CUHT
!      !CLOSE(LUNEXP)
!      !WRITE(5000,'(2F5.0)') FREQ,RSHT 
!      FREQ=42 !should be in the file X
!      CUHT=0.10 !should be in the file X
!C--------------------------------------------------      
!!           MOWC = 209.69*rsht(i)+0.0
!           !y = (209.69*rsht(i))
!           !MOWC= (-11.084*28)+y
!           !MOWC= (-11.084*28)+(209.69*rsht(i)*100) !original not dynamic
!           !MOWC= (FMOW*28)+(RHMOW*rsht(i)*100)+(CHMOW*canht*100)
!            MOWC= (FMOW*FREQ)+(RHMOW*CUHT*100)+(CHMOW*canht*100)
!           if (canht*100 .GE. 0.0) then
!!           RSPLC = -0.3373*rsht(i)+52.903
!           !z = (-0.3373*rsht(i))+52.903
!           !RSPLC= (-0.0418*28)+z
!           !RSPLC= (-0.0418*28)+((-0.3373*rsht(i))+52.903)!original not dynamic
!           !RSPLC=(FLFP*28)+((RHLFP*((canht-rsht(i))/canht*100))+RSPLM)
!            RSPLC=(FLFP*FREQ)+((RHLFP*((canht-CUHT)/canht*100))+RSPLM)
!           else
!           RSPLC=0.0
!           endif
!           if (RSPLC .ge. RSPLM) then
!           RSPLC=RSPLM    
!           endif
!!          WRITE(5000,'(2F10.0)') MOWC,RSPLC
C---------------------------------------------------------              
      !Daily Senescence
      DWTCO = WTCO - PWTCO
      DWTLO = WTLO - PWTLO
      DWTSO = WTSO - PWTSO       


        CALL GETLUN('FILEC', LUNCRP)
        OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)
        LNUM = 1
        SECTION = '!*PLAN'
        CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(ERRKEY, 1, FILECC, LNUM)
        ELSE
          CALL IGNORE(LUNCRP,LNUM,ISECT,MOW80)
          READ(MOW80,'(12X,F6.0,12X,F6.0)',IOSTAT=ERR)
     &                    PROLFF, PROSTF
          do j=1,5; CALL IGNORE(LUNCRP,LNUM,ISECT,MOW80); end do
          READ(MOW80,'(2f6.0)',IOSTAT=ERR)
     &                    pliglf, pligst
          CLOSE(LUNCRP)
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        END IF

      !C----------------------------------------------------------
      !!MOWC - Automatic MOW - post harvest stubble mass and %leaf
      !!       in the stubble calculation (Diego):
      !C----------------------------------------------------------
      ! OPEN AND READ SPECIES FILE
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      SECTION = '!*CANO'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      do j=1,8; CALL IGNORE(LUNCRP,LNUM,ISECT,C255); end do
        READ(C255,'(F6.0)') MOWREF
        CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
        READ(C255,'(6I6)',IOSTAT=ERRNUM) (IXFREQ(I),I=1,6)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
        READ(C255,'(6F6.2)',IOSTAT=ERRNUM) (YFREQ(I),I=1,6)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
        READ(C255,'(F6.0)') CNHREF
        CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
        READ(C255,'(6I6)',IOSTAT=ERRNUM) (IXCUTHT(I),I=1,6)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
        READ(C255,'(6F6.2)',IOSTAT=ERRNUM) (YCUTHT(I),I=1,6)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
        READ(C255,'(F6.0)') CHMOW
        CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
        READ(C255,'(6I6)',IOSTAT=ERRNUM) (IXCHMOW(I),I=1,6)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
        READ(C255,'(6F6.2)',IOSTAT=ERRNUM) (YCHMOW(I),I=1,6)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C255)


        READ(C255,'(F6.0)') RSREF
        CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
        READ(C255,'(6I6)',IOSTAT=ERRNUM) (IXRSREF(I),I=1,6)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
        READ(C255,'(6F6.2)',IOSTAT=ERRNUM) (YRSREF(I),I=1,6)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
        READ(C255,'(F6.0)') CUTPL
        CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
        READ(C255,'(6I6)',IOSTAT=ERRNUM) (IXCUTPL(I),I=1,6)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
        READ(C255,'(6F6.2)',IOSTAT=ERRNUM) (YCUTPL(I),I=1,6)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
        CLOSE (LUNCRP) 

!-----------------------------------------------------------------------
!     Find Phenology Section in FILEC and read
!-----------------------------------------------------------------------
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      LNUM = 1
      SECTION = '!*PHEN'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
      READ(C80,'(4F6.1)') TB(1), TO1(1), TO2(1), TM(1)
       CLOSE (LUNCRP)
!----------------------------------------------------------------------

        XFREQ = IXFREQ
        XCUTHT = IXCUTHT
        XCHMOW = IXCHMOW
        XRSREF = IXRSREF
        XCUTPL = IXCUTPL

      INQUIRE(FILE = MOWFILE, EXIST = exists)
      IF(exists .EQV. .FALSE.) ATMOW = .TRUE.
      IF (.NOT.ALLOCATED(MOW) .AND. ATMOW .EQV. .FALSE.) THEN
      
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
     &       .AND.MOW80(1:1).NE."!"
     &       .AND.MOW80(1:20).NE."                    "
     &       .and.mow80(1:6)==trtchar
     &       .AND.ISECT.EQ.0)THEN
             MOWCOUNT = MOWCOUNT + 1
          END IF
        END DO
        REWIND(MOWLUN)
			 

        IF (MOWCOUNT.GT.0) THEN
          ALLOCATE(TRNO(MOWCOUNT),DATE(MOWCOUNT),MOW(MOWCOUNT))
          ALLOCATE(RSPLF(MOWCOUNT),MVS(MOWCOUNT),rsht(mowcount))
        ELSE
C         MOW file has no data for this treatment
          CALL ERROR(ERRKEY,2,MOWFILE,0)
          ALLOCATE(MOW(1))
          MOW (1) = -99
          RETURN
        END IF
     
        I = 0
        ISECT = 0
        DO WHILE (ISECT.EQ.0)
          READ (MOWLUN,'(A80)',IOSTAT=ISECT) MOW80
          IF (MOW80(1:1).NE."@"
     &       .AND.MOW80(1:1).NE."!"
     &       .AND.MOW80(1:20).NE."                    "
     &       .and.mow80(1:6)==trtchar
     &       .AND.ISECT.EQ.0)THEN
            I = I + 1
            READ (MOW80,'(2I6,4F6.0)',IOSTAT=ISECT)
     &                TRNO(I),DATE(I),MOW(I),RSPLF(I),MVS(I),rsht(i)
C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY
            !CALL Y2K_DOY(DATE(I))
            CALL Y4K_DOY(DATE(I),MOWFILE,I,ERRKEY,1)
          END IF
        END DO


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
!             canht=max(rsht(i),0.0)     !enter rsht in cm


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
     &           ' FHWAH FHNAH FHN%H FHC%H FHLGH FHL%H'
!     &           ' FHWAH FHNAH FHN%H FHC%H FHLGH FHL%H FHAGE IVOMD'//
!     &           '   MOWC RSPLC'
            end if
               call yr_doy(yrdoy,year,doy)
               write(fhoutfmt,'(a)') '(i4,x,a8,a3,2(i5),i5,i4,'//
     &            '5(i6),f6.2,2(i6),3(f6.2),f6.1,i6,f6.1,x,f8.0,F6.1)'
            WRITE(fhlun,fhoutfmt)
     &           run,mowfile(1:8),crop,trtno,i,year,doy,
     &           Nint(topwt*10.),Nint(wtlf*10.),Nint(stmwt*10.),
     &           Nint(strwt*10.),Nint(rtwt*10.),xlai,
     &           Nint(fhtot*10.),Nint(fhtotn*10.),
     &           fhpctn,fhpcho,fhplig,fhpctlf
!     &           -99,-99.0,MOWC,RSPLC
            close(fhlun)
            
            if(date(i)==yrdoy.and.trno(i)==trtno) then
             PWTCO = WTCO 
             PWTLO = WTLO
             PWTSO = WTSO
             DWTCO = WTCO - PWTCO
             DWTLO = WTLO - PWTLO
             DWTSO = WTSO - PWTSO
            endif
              if(i==size(mow)) deallocate(mow,trno,date,rsplf,mvs,rsht)
            RETURN
            end if
        ENDIF
      ENDDO

      ENDIF

!***********************************************************************
! AUTOMOW
!***********************************************************************
      IF(HMFRQ .LE. 0 .AND. HMGDD .LE. 0) HMFRQ = 28
      IF(HMCUT .LE. 0.0) HMCUT = 0.10

      ! TF - 01/29/2022 Protection to avoid divisions and mod by zero 
      IF(HMFRQ .GT. 0) THEN
        CUTDAY = MOD(MOWCOUNT,HMFRQ)
        MOWGDD = 0 !It will not accumalute GDD if there is HMFRQ 
      ELSE
        CUTDAY = 1
      ENDIF
      INQUIRE(FILE = MOWFILE, EXIST = exists)
      ! TF/DP - 01/28/2022 Added degree days (GDD) option
      IF(ATMOW .EQV. .TRUE. .OR. 
     &  (exists .EQV. .FALSE. .AND. ATMOW .EQV. .FALSE.)) then
            IF(CUTDAY .EQ. 0 .OR. 
     &        (MOWGDD .GE. HMGDD .AND. HMGDD .GT. 0)) THEN 
              IF(HMFRQ .GT. 0) THEN
                FREQ = HMFRQ
              ELSE
                FREQ = HMGDD
              ENDIF
              !TF 2022-01-31 Simple version AutoMOW 
               MOWC = MAX(HMMOW,0)
               RSPLC = MAX(HRSPL,0)
               MOWGDD = 0.0
            ELSE
                MOWCOUNT = MOWCOUNT + 1
                GDD = (((TMAX+TMIN)/2) - TB(1))
                IF (GDD .GT. TM(1)) GDD = TM(1)
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
              FHVSTG = 3
              canht  = max(HMCUT/100,0.0)
              !             canht=max(rsht(i),0.0)     !enter rsht in cm


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
     &            '5(i6),f6.2,2(i6),3(f6.2),f6.1,x,f5.0,F6.1,F6.1)'
            WRITE(fhlun,fhoutfmt)
     &           run,mowfile(1:8),crop,trtno,i,year,doy,
     &           Nint(topwt*10.),Nint(wtlf*10.),Nint(stmwt*10.),
     &           Nint(strwt*10.),Nint(rtwt*10.),xlai,
     &           Nint(fhtot*10.),Nint(fhtotn*10.),
     &           fhpctn,fhpcho,fhplig,fhpctlf,
     &           MOWC,RSPLC
            close(fhlun)

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

      ENDIF
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************

      END !SUBROUTINE FORAGEHARVEST