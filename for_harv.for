      SUBROUTINE FORAGEHARVEST(CONTROL,FILECC,
     &                RHOL,RHOS,PCNL,PCNST,SLA,           !Input
     &                WTLF,STMWT,TOPWT,TOTWT,WCRLF,WCRST, !Input/Output
     &                WTNLF,WTNST,WNRLF,WNRST,WTNCAN,     !Input/Output
     &                AREALF,XLAI,XHLAI,VSTAGE,canht)     !Input/Output
!     &               WLDOTN,CRUSLF,NRUSLF,WSDOTN,CRUSST,NRUSST,
!     &               CADLF,NADLF,CADST,NADST,
!     &               SLDOT,WLIDOT,WLFDOT,SSDOT,WSIDOT,WSFDOT,
!     &               FHLEAF,FHSTEM,FHVSTG)

      USE MODULEDEFS

      IMPLICIT NONE

      INTEGER MOWLUN,ISECT,ERR
      INTEGER,ALLOCATABLE,DIMENSION(:) :: TRNO,DATE
      INTEGER TRTNO,YRDOY
      INTEGER SEASON
      INTEGER LUNIO,LUNCRP
      INTEGER LNUM,FOUND
      INTEGER I,MOWCOUNT

!      REAL WLDOTN,CRUSLF,NRUSLF
!      REAL WSDOTN,CRUSST,NRUSST
!      REAL CADLF,LFCADDM,NADLF,LFNADDM
!      REAL CADST,STCADDM,NADST,STNADDM
!      REAL SLDOT,WLIDOT,WLFDOT
!      REAL SSDOT,WSIDOT,WSFDOT
      REAL,ALLOCATABLE,DIMENSION(:) :: MOW,RSPLF,MVS,rsht
      REAL FHLEAF,FHSTEM,FHVSTG
      REAL RHOL,RHOS,PCNL,PCNST,SLA
      REAL WTLF,STMWT,TOPWT,TOTWT,WCRLF,WCRST
      REAL WTNLF,WTNST,WNRLF,WNRST,WTNCAN
      REAL AREALF,XLAI,AREAH,XHLAI,VSTAGE
      REAL PROLFF,PROSTF
      real canht

      CHARACTER*6  SECTION,ERRKEY
      CHARACTER*12 MOWFILE
      CHARACTER*30 FILEIO,FILECC
      CHARACTER*80 MOW80
      
      TYPE(CONTROLTYPE) CONTROL

      LUNIO  = CONTROL % LUNIO
      FILEIO = CONTROL % FILEIO
      YRDOY  = CONTROL % YRDOY

      OPEN (FILE=FILEIO,UNIT=LUNIO)

      ERRKEY = 'FRHARV'

      SECTION = '*TREAT'
      CALL FIND(LUNIO, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILEIO, LNUM)
      ELSE
        READ(LUNIO, '(I3)') TRTNO
      ENDIF

      CLOSE(LUNIO)

      IF (.NOT.ALLOCATED(MOW)) THEN

        OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD')

        REWIND(LUNIO)

        READ(LUNIO,'(///,15X,A12)') MOWFILE
        MOWFILE(10:12)='MOW'

        CLOSE (LUNIO)

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
          CLOSE(LUNCRP)
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        END IF

        MOWLUN=999
        OPEN (UNIT=MOWLUN,FILE=MOWFILE,STATUS='OLD')
        REWIND(MOWLUN)
!      DATE=0
!      ISECT=1
!      TRNO=0
        ISECT = 0
        MOWCOUNT = 0
        DO WHILE (ISECT.EQ.0)
          READ (MOWLUN,'(A80)',IOSTAT=ISECT) MOW80
          IF (MOW80(1:1).NE."@"
     &       .AND.MOW80(1:1).NE."!"
     &       .AND.MOW80(1:20).NE."                    "
     &       .AND.ISECT.EQ.0)THEN
             MOWCOUNT = MOWCOUNT + 1
          END IF
        END DO
        REWIND(MOWLUN)

        IF (MOWCOUNT.GT.0) THEN
          ALLOCATE(TRNO(MOWCOUNT),DATE(MOWCOUNT),MOW(MOWCOUNT))
          ALLOCATE(RSPLF(MOWCOUNT),MVS(MOWCOUNT),rsht(mowcount))
        ELSE
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
     &       .AND.ISECT.EQ.0)THEN
            I = I + 1
            READ (MOW80,'(2I6,4F6.0)',IOSTAT=ISECT)
     &                TRNO(I),DATE(I),MOW(I),RSPLF(I),MVS(I),rsht(i)
            CALL Y2K_DOY(DATE(I))
          END IF
        END DO

      END IF

      DO I=1,SIZE(MOW) !WHILE (DATE.NE.YRDOY.OR.TRNO.NE.TRTNO)
           IF (MOW(I).GE.0) THEN
             IF (DATE(I).EQ.YRDOY.AND.TRNO(I).EQ.TRTNO)THEN
              FHLEAF=0
              FHSTEM=0
              FHVSTG=0
!              FHLEAF=WTLF-(MOW(I)/10)*RSPLF(I)/100+WLDOTN-CRUSLF-NRUSLF/0.16
              IF(RSPLF(I)>=0)THEN
                FHLEAF=WTLF-(MOW(I)/10)*RSPLF(I)/100
                FHSTEM=STMWT-(MOW(I)/10)*(1.0-RSPLF(I)/100)
              ELSE
                FHLEAF=WTLF-(MOW(I)/10)*WTLF/(WTLF+STMWT)
                FHSTEM=STMWT-(MOW(I)/10)*STMWT/(WTLF+STMWT)
              END IF
              FHLEAF=MAX(FHLEAF,0.0)
!            IF (FHLEAF.GT.0)THEN
!             IF (WLIDOT+WLFDOT+SLDOT.GT.FHLEAF)THEN
!               IF(WLIDOT+WLFDOT+SLDOT.GT.0)THEN
!                WLIDOT=(WLIDOT+WLFDOT+SLDOT-FHLEAF)*
!     &                    WLIDOT/(WLIDOT+WLFDOT+SLDOT)
!                WLFDOT=(WLIDOT+WLFDOT+SLDOT-FHLEAF)*
!     &                    WLFDOT/(WLIDOT+WLFDOT+SLDOT)
!                SLDOT=(SLDOT+WLFDOT+SLDOT-FHLEAF)*
!     &                   SLDOT/(WLIDOT+WLFDOT+SLDOT)
!             LFCADDM = CADLF * (MIN(1.0,(WLIDOT+WLFDOT)/(WTLF - SLDOT)))
!             LFNADDM = NADLF * (MIN(1.0,(WLIDOT+WLFDOT)/(WTLF - SLDOT)))
!              FHLEAF = FHLEAF - LFCADDM - LFNADDM/0.16
!               ENDIF
!             ELSE
!              WLIDOT=0
!              WLFDOT=0
!              SLDOT=0
!             ENDIF
!            ENDIF

!            FHSTEM=STMWT-(MOW/10)*(1.0-RSPLF/100)+WSDOTN
!     &             -CRUSST-NRUSST/0.16
              FHSTEM=MAX(FHSTEM,0.0)
!            IF (FHSTEM.GT.0)THEN
!             IF (WSIDOT+WSFDOT+SSDOT.GT.FHSTEM)THEN
!               IF (WSIDOT+WSFDOT+SSDOT.GT.0)THEN
!                 WSIDOT=(WSIDOT+WSFDOT+SSDOT-FHSTEM)*
!     &                    WSIDOT/(WSIDOT+WSFDOT+SSDOT)
!                 WSFDOT=(WSIDOT+WSFDOT+SSDOT-FHSTEM)*
!     &                    WSFDOT/(WSIDOT+WSFDOT+SSDOT)
!                 SSDOT=(WSIDOT+WSFDOT+SSDOT-FHSTEM)*
!     &                   SSDOT/(WSIDOT+WSFDOT+SSDOT)
!            STCADDM = CADST * (MIN(1.0,(WSIDOT+WSFDOT)/(STMWT - SSDOT)))
!            STNADDM = NADST * (MIN(1.0,(WSIDOT+WSFDOT)/(STMWT - SSDOT)))
!            FHSTEM = FHSTEM - STCADDM - STNADDM/0.16
!               ENDIF
!             ELSE
!              WSIDOT=0
!              WSFDOT=0
!              SSDOT=0
!             ENDIF
!            ENDIF
              FHVSTG=MAX(MVS(I),0.0)
              
              canht=max(rsht(i)/100,0.0)

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

!            END IF
!          END IF

!            CLOSE(MOWLUN)

!            INQUIRE(FHOUT,EXIST=EXISTS)
!            IF (EXISTS) THEN
!              OPEN(FILE=FHOUT,UNIT=FHLUN,POSITION=APPEND)
!            ELSE
!              OPEN(FILE=FHOUT,UNIT=FHLUN,STATUS=NEW)
!            WRITE(FHLUN,'(3I6)')
!     &           '  FHLF  FHST FHTOT'
!            ENDIF
!            WRITE(FHLUN,'(3I6)')
!     &           INT(FHLEAF*10),INT(FHSTEM*10),INT(FHTOT*10)
            RETURN
          ENDIF
        ENDIF
      ENDDO
      
      END !SUBROUTINE FORAGEHARVEST
