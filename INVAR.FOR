C=======================================================================
C  INVRLE, Subroutine
C
C  Interactively edit genetic parameters
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1991 GH  Written
C  05/28/1993 PWW Header revision and minor changes
C  03/10/2005 GH  Change limits for P2R to handle photoperiod 
C                 sensitivity
!  12/14/2005 CHP Added sorghum cultivar coefficients, PBASE and PSAT
!  11/26/2007 CHP THRESH, SDPRO, SDLIP moved from eco to cul file
C-----------------------------------------------------------------------
C  INPUT  : FILEG,VARTY,VRNAME,CLDVAR,THVAR,PATHGE,PM06,PM09
C
C  LOCAL  : IERR,IPARAM,NDEX
C
C  OUTPUT : None
C-----------------------------------------------------------------------
C  Called : SEVAR
C
C  Calls  : CLEAR SELPRO GETREAL
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  IERR   : Error flag
C  IPARAM : Menu choice
C  NDEX   : Error flag for FSELPRO
C  CROP   : Crop code
C=======================================================================

      SUBROUTINE INVRLE (FILEG,RNMODE,VARTY,VRNAME,PATHGE,ECONO)

      IMPLICIT     NONE

      INCLUDE     'COMGEN.BLK'

      CHARACTER*1  LINE(80),ANS,RNMODE,BLANK,UPCASE
      CHARACTER*6  GNAME(18),VARTY,ECONO
      CHARACTER*12 FILEG
      CHARACTER*16 VRNAME
      CHARACTER*80 PATHGE
      CHARACTER*92 FILEGG

      INTEGER      I,IG,PATHL
      LOGICAL      FEXIST
      REAL         GVALUE(18),GENP,GENNEW,FLAG

      PARAMETER (BLANK = ' ')

      GNAME  (1) = 'CSDL  '
      GVALUE (1) = CSDVAR
      GNAME  (2) = 'PPSEN '
      GVALUE (2) = PPSEN
      GNAME  (3) = 'EM-FL '
      GVALUE (3) = PH2T5
      GNAME  (4) = 'FL-SH'
      GVALUE (4) = PHTHRS(6)
      GNAME  (5) = 'FL-SD'
      GVALUE (5) = PHTHRS(8)
      GNAME  (6) = 'SD-PM'
      GVALUE (6) = PHTHRS(10)
      GNAME  (7) = 'FL-LF'
      GVALUE (7) = PHTHRS(13)
      GNAME  (8) = 'LFMAX'
      GVALUE (8) = LFMAX
      GNAME  (9) = 'SLAVR'
      GVALUE (9) = SLAVAR
      GNAME (10) = 'SIZLF'
      GVALUE(10) = SIZELF
      GNAME (11) = 'XFRT '
      GVALUE(11) = XFRUIT
      GNAME (12) = 'WTPSD'
      GVALUE(12) = WTPSD
      GNAME (13) = 'SFDUR'
      GVALUE(13) = SFDUR
      GNAME (14) = 'SDPDV'
      GVALUE(14) = SDPDVR
      GNAME (15) = 'PODUR'
      GVALUE(15) = PODUR
      GNAME (16) = 'THRSH'
      GVALUE(16) = THRESH
      GNAME (17) = 'SDPRO'
      GVALUE(17) = SDPRO
      GNAME (18) = 'SDLIP'
      GVALUE(18) = SDLIP

  700 CONTINUE
      IF (INDEX('IE',RNMODE) .GT. 0) THEN
         CALL CLEAR
         WRITE (*,15000)
         WRITE (*,16000) VRNAME
         WRITE (*,17000) (I,GNAME(I),GVALUE(I),I =  1, 2)
         WRITE (*,17000) (I,GNAME(I),GVALUE(I),I =  3, 4)
         WRITE (*,17000) (I,GNAME(I),GVALUE(I),I =  5, 6)
         WRITE (*,17000) (I,GNAME(I),GVALUE(I),I =  7, 8)
         WRITE (*,17000) (I,GNAME(I),GVALUE(I),I =  9,10)
         WRITE (*,17000) (I,GNAME(I),GVALUE(I),I = 11,12)
         WRITE (*,17000) (I,GNAME(I),GVALUE(I),I = 13,14)
         WRITE (*,17000) (I,GNAME(I),GVALUE(I),I = 15,16)
         WRITE (*,17000) (I,GNAME(I),GVALUE(I),I = 17,18)
         WRITE (*,18000) FILEG
      ENDIF
      READ (5,19000) LINE
      CALL VERIFY (LINE,GENP,FLAG)
      IG = NINT(GENP)
      IF (FLAG .GT. 1 .OR. GENP .LT. -1 .OR. GENP .GT. 34) GO TO 700
      IF (GENP .EQ. -1) GO TO  900
      IF (GENP .EQ.  0 .OR. FLAG .EQ. 1) GO TO 1000
  800 IF (INDEX('IE',RNMODE) .GT. 0) THEN
          WRITE (*,20000) GNAME(IG),GVALUE(IG)
	ENDIF
      READ (5,19000) LINE
      CALL VERIFY (LINE,GENNEW,FLAG)
      IF (GENNEW .LT. 0) THEN
         WRITE (*,21000) GENNEW
         GO TO 800
      ENDIF
      GVALUE(IG) = GENNEW
      GO TO 700
  900 CONTINUE
      IF (FILEG(8:8) .EQ. '0' .OR. FILEG(8:8) .EQ. '1') THEN
         WRITE (*,22000) FILEG
         READ  (5,19000) ANS
         IF (ANS .EQ. 'N' .OR. ANS .EQ. 'n') GO TO 1000
      ENDIF
      I = 0
  950 CONTINUE
      I = I + 1
      WRITE (FILEG(8:8),13000) I

      PATHL  = INDEX (PATHGE,BLANK)
      IF (PATHL .LE. 1) THEN
         FILEGG = FILEG
       ELSE
         FILEGG = PATHGE(1:(PATHL-1)) // FILEG
      ENDIF

      INQUIRE (FILE = FILEGG, EXIST = FEXIST)
      IF (FEXIST) THEN
         IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,23000) FILEG
         READ (5,19000) ANS
         ANS = UPCASE(ANS)
         IF (ANS .EQ. 'N' .AND. I .LT. 9) THEN
            GO TO 950
         ELSE IF (ANS .EQ. 'N') THEN
            GO TO 1000
         ENDIF
      ENDIF
      OPEN (19,FILE = FILEGG,STATUS = 'UNKNOWN')
      VRNAME = 'NEW CULTIVAR    '
C     VARTY  = 'NEW001'
      WRITE (19,23500) (I,I=1,15)
      WRITE (19,24000) VARTY,VRNAME,ECONO,(GVALUE(I),I = 1,15)
      CLOSE (19)

 1000 CONTINUE

      CSDVAR     =  GVALUE( 1)
      PPSEN      =  GVALUE( 2)
      PH2T5      =  GVALUE( 3)
      PHTHRS(6)  =  GVALUE( 4)
      PHTHRS(8)  =  GVALUE( 5)
      PHTHRS(10) =  GVALUE( 6)
      PHTHRS(13) =  GVALUE( 7)
      LFMAX      =  GVALUE( 8)
      SLAVAR     =  GVALUE( 9)
      SIZELF     =  GVALUE(10)
      XFRUIT     =  GVALUE(11)
      WTPSD      =  GVALUE(12)
      SFDUR      =  GVALUE(13)
      SDPDVR     =  GVALUE(14)
      PODUR      =  GVALUE(15)
      THRESH     =  GVALUE(16)
      SDPRO      =  GVALUE(17)
      SDLIP      =  GVALUE(18)

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

13000 FORMAT (I1)
15000 FORMAT (20X,'CULTIVAR SPECIFIC COEFFICIENTS',/,20X,30('='),/)
16000 FORMAT (5X,'CULTIVAR :',1X,A20,/,5X,31('-'),//,
     &        5X,'Please Check the User''s Guide For Definitions,',/,
     &        5X,'Units, and Ranges of Coefficients.',/)
17000 FORMAT (5X,2(1X,I2,') ',A6,': ',F7.3,10X))
18000 FORMAT (/,5X,
     &       'Please Enter Parameter # You Would Like ',
     &       'To Modify [Default = 0] : ',/,5X,
     &       '(Enter -1 to Save Values in File ',A12,'). ',$)
19000 FORMAT (80A1)
20000 FORMAT (/,
     & 5X,' Current Value for Cultivar Coefficient ',A6,'  ===>',F8.3,/,
     & 5X,' Enter New Cultivar Coefficient                 --->  ',$)
21000 FORMAT(/,5X,
     &   'Please enter a value > 0; your current value is ',F8.3)
22000 FORMAT (/,5X,'File ',A12,' can NOT be modified!',
     &        /,5X,'Do you want to save the data in another file ? ',$)
23000 FORMAT (/,5X,'File ',A12,' exists.',/,5X,
     &         'Do you want to overwrite the existing data (Y/N) ? ',$)
23500 FORMAT ('*GENETICS PARAMETER INPUT FILE',//,
     &'@VAR#  VAR-NAME........   ECO#  CSDL PPSEN PH2-5 PHT-7',
     &' PHT-8 PHT10 PHT13 LFMAX SLAVR SIZLF  XFRT WTPSD SFDUR',
     &' SDPDV PODUR THRSH SDPRO SDLIP',/,'!',29X,18(4X,I2))
24000 FORMAT (A6,1X,A16,1X,A6,F6.2,F6.3,3F6.1,2F6.2,F6.3,
     &        F6.0,F6.1,F6.2,F6.3,F6.1,F6.2,F6.1,
     &        F6.1,2F6.3)

      END SUBROUTINE INVRLE

C=======================================================================
C  INVRCE, Subroutine
C
C  Interactively edit genetic parameters - Cereal crops
c
C-----------------------------------------------------------------------
C  Revision history
C
C  06/12/1992 BB  Written
C  05/28/1993 PWW Header revision and minor changes
!  12/14/2005 CHP Added sorghum cultivar coefficients, PBASE and PSAT
!  02/06/2007 CHP Added alternate sugarcane parameters for CASUPRO
!  09/16/2007 JIL Added new inputs for IXIM
C
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  : IERR,IPARAM,NDEX
C
C  OUTPUT : None
C-----------------------------------------------------------------------
C  Called : SEVAR
C
C  Calls  : CLEAR SELPRO GETREAL
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  IERR   : Error flag
C  IPARAM : Menu choice
C  NDEX   : Error flag for FSELPRO
C  CROP   : Crop code
C=======================================================================

      SUBROUTINE INVRCE (CROP, MODEL)

      IMPLICIT  NONE

      INCLUDE  'COMGEN.BLK'

      INTEGER     IERR,IPARAM,NDEX
      CHARACTER*2 CROP
      CHARACTER*8 MODEL
C
C     Repeat until user chooses to quit
C
3001  CONTINUE
      CALL CLEAR
      WRITE (*,5000)

      IF (CROP .EQ. 'ML') THEN
         WRITE (*,5300) P1,P2O,P2R,P5,G1,G4,PHINT
         WRITE (*,5100)
C
C        Get menu choice
C
         READ  (5,'(I2)',IOSTAT=IERR) IPARAM

         CALL SELPRO (0,7,IPARAM,NDEX,IERR)

         IF (NDEX .EQ. 2) GOTO 3001
C
C        Branch to menu choice
C
         IF (IPARAM .EQ. 0) THEN
             RETURN
         ELSE IF (IPARAM .EQ. 1) THEN
             CALL GETREAL (P1,'P1   ',  1.0,800.0)
         ELSE IF (IPARAM .EQ. 2) THEN
             CALL GETREAL (P2O,'P2O  ',  5.0, 19.0)
         ELSE IF (IPARAM .EQ. 3) THEN
             CALL GETREAL (P2R,'P2R  ',  5.0,9000.0)
         ELSE IF (IPARAM .EQ. 4) THEN
             CALL GETREAL (P5,'P5   ',100.0,900.0)
         ELSE IF (IPARAM .EQ. 5) THEN
             CALL GETREAL (G1,'G1   ',  0.0, 50.0)
         ELSE IF (IPARAM .EQ. 6) THEN
             CALL GETREAL (G4,'G4   ',  0.0,  1.0)
         ELSE IF (IPARAM .EQ. 7) THEN
             CALL GETREAL (PHINT,'PHINT',1.0,200.0)
         ENDIF

!       ELSE IF (CROP .EQ. 'MZ') THEN
       ELSE IF(MODEL(1:5).EQ.'MZCER'.OR.MODEL(1:5).EQ.'SWCER') THEN
         WRITE (*,5400) P1,P2,P5,G2,G3,PHINT
         WRITE (*,5100)
C
C        Get menu choice
C
         READ  (5,'(I2)',IOSTAT=IERR) IPARAM
         CALL SELPRO (0,6,IPARAM,NDEX,IERR)
         IF (NDEX .EQ. 2) GOTO 3001
C
C        Branch to menu choice
C
         IF (IPARAM .EQ. 0) THEN
             RETURN
         ELSE IF (IPARAM .EQ. 1) THEN
             CALL GETREAL (P1,'P1   ', 80.0, 500.0)
         ELSE IF (IPARAM .EQ. 2) THEN
             CALL GETREAL (P2,'P2   ',  0.0,  10.0)
         ELSE IF (IPARAM .EQ. 3) THEN
             CALL GETREAL (P5,'P5   ',100.0,2000.0)
         ELSE IF (IPARAM .EQ. 4) THEN
             CALL GETREAL (G2,'G2   ',100.0,2000.0)
         ELSE IF (IPARAM .EQ. 5) THEN
             CALL GETREAL (G3,'G3   ',  1.0,  15.0)
         ELSE IF (IPARAM .EQ. 6) THEN
             CALL GETREAL (PHINT,'PHINT',1.0,200.0)
         ENDIF

       ELSE IF (MODEL(1:5) .EQ. 'MZIXM') THEN
         WRITE (*,5450) P1,P2,P5,G2,G3,PHINT,AX,LX
         WRITE (*,5100)
C
C        Get menu choice
C
         READ  (5,'(I2)',IOSTAT=IERR) IPARAM
         CALL SELPRO (0,8,IPARAM,NDEX,IERR)
         IF (NDEX .EQ. 2) GOTO 3001
C
C        Branch to menu choice
C
         IF (IPARAM .EQ. 0) THEN
             RETURN
         ELSE IF (IPARAM .EQ. 1) THEN
             CALL GETREAL (P1,'P1   ', 80.0, 500.0)
         ELSE IF (IPARAM .EQ. 2) THEN
             CALL GETREAL (P2,'P2   ',  0.0,  10.0)
         ELSE IF (IPARAM .EQ. 3) THEN
             CALL GETREAL (P5,'P5   ',100.0,2000.0)
         ELSE IF (IPARAM .EQ. 4) THEN
             CALL GETREAL (G2,'G2   ',100.0,2000.0)
         ELSE IF (IPARAM .EQ. 5) THEN
             CALL GETREAL (G3,'G3   ',  1.0,  15.0)
         ELSE IF (IPARAM .EQ. 6) THEN
             CALL GETREAL (PHINT,'PHINT',1.0,200.0)
         ELSE IF (IPARAM .EQ. 7) THEN
             CALL GETREAL (AX,'AX   ',400.0,1500.0)
         ELSE IF (IPARAM .EQ. 8) THEN
             CALL GETREAL (LX,'LX   ',500.0,2000.0)
         ENDIF


       ELSE IF (CROP .EQ. 'SG') THEN
         WRITE (*,5500) P1,P2O,P2R,P5,G1,G2,PHINT,P3,P4,PBASE,PSAT
         WRITE (*,5100)
C
C        Get menu choice
C
         READ  (5,'(I2)',IOSTAT=IERR) IPARAM
         CALL SELPRO (0,11,IPARAM,NDEX,IERR)
         IF(NDEX .EQ. 2) GOTO 3001
C
C        Branch to menu choice
C
         SELECT CASE(IPARAM)
           CASE(0);  RETURN
           CASE(1);  CALL GETREAL (P1,   'P1   ',150.0, 500.0)
           CASE(2);  CALL GETREAL (P2O,  'P2O  ',  5.0,  30.0)
           CASE(3);  CALL GETREAL (P2R,  'P2R  ', 10.0,9000.0)
           CASE(4);  CALL GETREAL (P5,   'P5   ',100.0, 900.0)
           CASE(5);  CALL GETREAL (G1,   'G1   ',  1.0,  20.0)
           CASE(6);  CALL GETREAL (G2,   'G2   ',  1.0,   8.0)
           CASE(7);  CALL GETREAL (PHINT,'PHINT',  1.0, 200.0)
	     CASE(8);  CALL GETREAL (P3,   'P3   ',100.0, 900.0)
           CASE(9);  CALL GETREAL (P4,   'P4   ',100.0, 900.0)
           CASE(10); CALL GETREAL (PBASE,'PBASE',  5.0,  24.0)
           CASE(11); CALL GETREAL (PSAT, 'PSAT ',  5.0,  24.0)
         END SELECT

	ELSE IF ((CROP .EQ. 'WH') .OR. (CROP .EQ. 'BA')) THEN
	   WRITE (*,5600) P1V,P1D,P5,G1,G2,G3,PHINT
         WRITE (*,5100)
C
C        Get menu choice
C
         READ  (5,'(I2)',IOSTAT=IERR) IPARAM
         CALL SELPRO (0,10,IPARAM,NDEX,IERR)
         IF (NDEX .EQ. 2) GOTO 3001
C
C        Branch to menu choice
C
         IF (IPARAM .EQ. 0) THEN
             RETURN
         ELSE IF (IPARAM .EQ. 1) THEN
             CALL GETREAL (P1V,'P1V  ',0.0,10000.)
         ELSE IF (IPARAM .EQ. 2) THEN
             CALL GETREAL (P1D,'P1D  ',0.0,10000.)
         ELSE IF (IPARAM .EQ. 3) THEN
             CALL GETREAL (P5,'P5   ',0.0,10000.0)
         ELSE IF (IPARAM .EQ. 4) THEN
             CALL GETREAL (G1,'G1   ',0.0,10000.0)
         ELSE IF (IPARAM .EQ. 5) THEN
             CALL GETREAL (G2,'G2   ',0.0,10000.0)
         ELSE IF (IPARAM .EQ. 6) THEN
             CALL GETREAL (G3,'G3   ',0.0,10000.0)
         ELSE IF (IPARAM .EQ. 7) THEN
             CALL GETREAL (PHINT,'PHINT',0.0,10000.0)
	   ENDIF

       ELSE IF (CROP .EQ. 'PT') THEN
         WRITE (*,5700) G2,G3,PD,P2,TC
         WRITE (*,5100)
C
C        Get menu choice
C
         READ  (5,'(I2)',IOSTAT=IERR) IPARAM
         CALL SELPRO (0,5,IPARAM,NDEX,IERR)
         IF (NDEX .EQ. 2) GOTO 3001
C
C        Branch to menu choice
C
         IF (IPARAM .EQ. 0) THEN
             RETURN
         ELSE IF (IPARAM .EQ. 1) THEN
             CALL GETREAL (G2,'G2   ',0.0,3000.0)
         ELSE IF (IPARAM .EQ. 2) THEN
             CALL GETREAL (G3,'G3   ',0.0,3000.0)
         ELSE IF (IPARAM .EQ. 3) THEN
             CALL GETREAL (PD,'PD   ',0.0,3000.0)
         ELSE IF (IPARAM .EQ. 4) THEN
             CALL GETREAL (P2,'P2   ',0.0,3000.0)
         ELSE IF (IPARAM .EQ. 5) THEN
             CALL GETREAL (TC,'TC   ',0.0,3000.0)
         ENDIF

       ELSE IF (CROP .EQ. 'RI') THEN
         WRITE (*,5800) P1,P2R,P5,P2O,G1,G2,G3,G4
         WRITE (*,5100)
C
C        Get menu choice
C
         READ  (5,'(I2)',IOSTAT=IERR) IPARAM
         CALL SELPRO (0,8,IPARAM,NDEX,IERR)
         IF (NDEX .EQ. 2) GOTO 3001
C
C        Branch to menu choice
C
         IF (IPARAM .EQ. 0) THEN
             RETURN
         ELSE IF (IPARAM .EQ. 1) THEN
             CALL GETREAL (P1,  'P1   ',0.0,5000.0)
         ELSE IF (IPARAM .EQ. 2) THEN
             CALL GETREAL (P2R,'P2R  ',0.0,5000.0)
         ELSE IF (IPARAM .EQ. 3) THEN
             CALL GETREAL (P5,  'P5   ',0.0,5000.0)
         ELSE IF (IPARAM .EQ. 4) THEN
             CALL GETREAL (P2O,'P2O  ',0.0,5000.0)
         ELSE IF (IPARAM .EQ. 5) THEN
             CALL GETREAL (G1,  'G1   ',0.0,5000.0)
         ELSE IF (IPARAM .EQ. 6) THEN
             CALL GETREAL (G2,  'G2   ',0.0,5000.0)
         ELSE IF (IPARAM .EQ. 7) THEN
             CALL GETREAL (G3,  'G3   ',0.0,5000.0)
         ELSE IF (IPARAM .EQ. 8) THEN
             CALL GETREAL (G4,  'G4   ',0.0,5000.0)
         ENDIF

       ELSE IF (CROP .EQ. 'SC') THEN
!-----------------------------------------------
!        CASUPRO sugarcane model
         IF (INDEX(MODEL,'SCCSP') > 0) THEN
           WRITE (*,5950) PI1, PI2, DTPI, SMAX, SO, GMAX, GO,
     &                    M1, M2, LA, WTPSD, SLAVAR, SIZLF
           WRITE (*,5100)

C          Get menu choice

           READ  (5,'(I2)',IOSTAT=IERR) IPARAM
           CALL SELPRO (0,13,IPARAM,NDEX,IERR)
           IF (NDEX .EQ. 2) GOTO 3001
C
C          Branch to menu choice
           SELECT CASE (IPARAM)
             CASE (0); RETURN
             CASE (1); CALL GETREAL (PI1,   'PI1  ', 1.0, 500.0)
             CASE (2); CALL GETREAL (PI2,   'PI2  ', 1.0, 500.0)
             CASE (3); CALL GETREAL (DTPI,  'DTPI ', 0.0,9999.9)
             CASE (4); CALL GETREAL (SMAX,  'SMAX ', 0.0,9999.9)
             CASE (5); CALL GETREAL (SO,    'SO   ', 0.0,9999.9)
             CASE (6); CALL GETREAL (GMAX,  'GMAX ', 1.0,9999.9)
             CASE (7); CALL GETREAL (GO,    'GO   ', 1.0,9999.9)
             CASE (8); CALL GETREAL (M1,    'M1   ', 1.0,9999.9)
             CASE (9); CALL GETREAL (M2,    'M2   ', 1.0,9999.9)
             CASE(10); CALL GETINT  (LA,    'LA   ',   0,9999)
             CASE(11); CALL GETREAL (WTPSD, 'WTPSD', 0.0,9999.9)
             CASE(12); CALL GETREAL (SLAVAR,'SLAVAR',1.0,9999.9)
             CASE(13); CALL GETREAL (SIZLF, 'SIZLF', 1.0,9999.9)
           END SELECT
!-----------------------------------------------
!        CANEGRO sugarcane model
         ELSEIF (INDEX(MODEL,'SCCAN') > 0) THEN
           WRITE (*,5900) P1,RATPOT,LFMAX,G1,PI1,PI2,DTTPI
           WRITE (*,5100)
C
C          Get menu choice

           READ  (5,'(I2)',IOSTAT=IERR) IPARAM
           CALL SELPRO (0,7,IPARAM,NDEX,IERR)
           IF (NDEX .EQ. 2) GOTO 3001
C
C          Branch to menu choice
C
           IF (IPARAM .EQ. 0) THEN
               RETURN
           ELSE IF (IPARAM .EQ. 1) THEN
               CALL GETREAL (P1,'P1   ',500.0,9999.9)
           ELSE IF (IPARAM .EQ. 2) THEN
               CALL GETREAL (RATPOT,'RATPT',0.0,10.0)
           ELSE IF (IPARAM .EQ. 3) THEN
               CALL GETREAL (LFMAX,'LFMAX',1.0,100.0)
           ELSE IF (IPARAM .EQ. 4) THEN
               CALL GETREAL (G1,'G1   ',1.0,3.0)
           ELSE IF (IPARAM .EQ. 5) THEN
               CALL GETREAL (PI1,'PI1  ',1.0,500.0)
           ELSE IF (IPARAM .EQ. 6) THEN
               CALL GETREAL (PI2,'PI2  ',1.0,500.0)
           ELSE IF (IPARAM .EQ. 7) THEN
               CALL GETREAL (DTTPI,'DTTPI',0.0,9999.9)
           ENDIF

!-----------------------------------------------
!        Invalid sugarcane model
         ELSE
           WRITE (*,'(A)') "Invalid sugarcane model."
         ENDIF

       ELSE IF (CROP .EQ. 'SU') THEN
         WRITE (*,6000) P1,P2,P5,G2,G3,O1
         WRITE (*,5100)
C
C        Get menu choice
C
         READ  (5,'(I2)',IOSTAT=IERR) IPARAM
         CALL SELPRO (0,6,IPARAM,NDEX,IERR)
         IF (NDEX .EQ. 2) GOTO 3001
C
C        Branch to menu choice
C
         IF (IPARAM .EQ. 0) THEN
             RETURN
         ELSE IF (IPARAM .EQ. 1) THEN
             CALL GETREAL (P1,'P1   ', 80.0, 700.0)
         ELSE IF (IPARAM .EQ. 2) THEN
             CALL GETREAL (P2,'P2   ',  0.0,  15.0)
         ELSE IF (IPARAM .EQ. 3) THEN
             CALL GETREAL (P5,'P5   ',400.0,1500.0)
         ELSE IF (IPARAM .EQ. 4) THEN
             CALL GETREAL (G2,'G2   ',300.0,4000.0)
         ELSE IF (IPARAM .EQ. 5) THEN
             CALL GETREAL (G3,'G3   ',  1.0,  15.0)
         ELSE IF (IPARAM .EQ. 6) THEN
             CALL GETREAL (O1,'O1   ',  1.0, 100.0)
         ENDIF
       ELSE IF (CROP .EQ. 'PI') THEN
         WRITE (*,6100) P2,P3,P4,G2,G3,PHINT
         WRITE (*,5100)
C
C        Get menu choice
C
         READ  (5,'(I2)',IOSTAT=IERR) IPARAM
         CALL SELPRO (0,6,IPARAM,NDEX,IERR)
         IF (NDEX .EQ. 2) GOTO 3001
C
C        Branch to menu choice
C
         IF (IPARAM .EQ. 0) THEN
             RETURN
         ELSE IF (IPARAM .EQ. 1) THEN
             CALL GETREAL (P2,'P2   ',  1.0, 999.0)
         ELSE IF (IPARAM .EQ. 2) THEN
             CALL GETREAL (P3,'P3   ',  1.0, 999.0)
         ELSE IF (IPARAM .EQ. 3) THEN
             CALL GETREAL (P4,'P4   ',  1.0,9999.0)
         ELSE IF (IPARAM .EQ. 4) THEN
             CALL GETREAL (G2,'G2   ',  1.0, 999.0)
         ELSE IF (IPARAM .EQ. 5) THEN
             CALL GETREAL (G3,'G3   ',  1.0,  99.0)
         ELSE IF (IPARAM .EQ. 6) THEN
             CALL GETREAL (PHINT,'PHINT',  1.0, 999.0)
         ENDIF
       ELSE IF ((CROP .EQ. 'TR') .OR. (CROP .EQ. 'TN')) THEN
         WRITE (*,6200) P1,P3,P4,P5,G3,G4,PHINT,PCINT,PCGRD
         WRITE (*,5100)
C
C        Get menu choice
C
         READ  (5,'(I2)',IOSTAT=IERR) IPARAM
         CALL SELPRO (0,9,IPARAM,NDEX,IERR)
         IF (NDEX .EQ. 2) GOTO 3001
C
C        Branch to menu choice
C
         IF (IPARAM .EQ. 0) THEN
             RETURN
         ELSE IF (IPARAM .EQ. 1) THEN
             CALL GETREAL (P1,'P1  ',0.0,4000.0)
         ELSE IF (IPARAM .EQ. 2) THEN
             CALL GETREAL (P3,'P3  ',1.0,4000.0)
         ELSE IF (IPARAM .EQ. 3) THEN
             CALL GETREAL (P4,'P4   ',1.0,4000.0)
         ELSE IF (IPARAM .EQ. 4) THEN
             CALL GETREAL (P5,'P5   ',1.0,4000.0)
         ELSE IF (IPARAM .EQ. 5) THEN
             CALL GETREAL (G3,'G3   ',1.0,7.0)
         ELSE IF (IPARAM .EQ. 6) THEN
             CALL GETREAL (G4,'G4   ',1.0,7.0)
         ELSE IF (IPARAM .EQ. 7) THEN
             CALL GETREAL (PHINT,'PHINT',1.0,2000.0)
         ELSE IF (IPARAM .EQ. 7) THEN
             CALL GETREAL (PCINT,'PCINT',1.0,2000.0)
         ELSE IF (IPARAM .EQ. 7) THEN
             CALL GETREAL (PCGRD,'PCGRD',1.0,2000.0)
         ENDIF
       ELSE IF (CROP .EQ. 'CO') THEN
         WRITE (*,6300) SCPB,RESPC,SQCON,FCUT,FLAI,DDISQ
         WRITE (*,5100)
C
C        Get menu choice
C
         READ  (5,'(I2)',IOSTAT=IERR) IPARAM
         CALL SELPRO (0,7,IPARAM,NDEX,IERR)
         IF (NDEX .EQ. 2) GOTO 3001
C
C        Branch to menu choice
C
         IF (IPARAM .EQ. 0) THEN
             RETURN
         ELSE IF (IPARAM .EQ. 1) THEN
             CALL GETREAL (SCPB,'SCPB ',0.0,100.0)
         ELSE IF (IPARAM .EQ. 2) THEN
             CALL GETREAL (RESPC,'RESPC',0.0,1.0)
         ELSE IF (IPARAM .EQ. 3) THEN
             CALL GETREAL (SQCON,'SQCON',0.0,1.0)
         ELSE IF (IPARAM .EQ. 4) THEN
             CALL GETREAL (FCUT,'FCUT ',0.0,1.0)
         ELSE IF (IPARAM .EQ. 5) THEN
             CALL GETREAL (FLAI,'FLAI ',0.0,10.0)
         ELSE IF (IPARAM .EQ. 6) THEN
             CALL GETREAL (DDISQ,'DDISQ',0.0,1000.0)
         ENDIF
      ENDIF

      GOTO 3001

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

5000  FORMAT (/,20X,'========================================',/,
     1          20X,'Current Values of Coefficients to Modify',/,
     2          20X,'========================================',///)
5100  FORMAT (/,'  Parameter choice [Default = 0] ===>  ',$)

5300  FORMAT (12X,'0. End of changes ',//,
     1  12X,'1. P1  (Cumulative growing degree days from',/,
     2  12X,'        seedling emergence to end of juvenile)..[',F7.1,/,
     3  12X,'2. P2O (Optimal photoperiod for development)....[',F7.1,/,
     4  12X,'3. P2R (Cumulative growing degree days delay',/,
     5  12X,'        for each hour increase above P2O........[',F7.1,/,
     6  12X,'4. P5  (Cumulative growing degree days from',/,
     7  12X,'        begin grain fill to phys. maturity).....[',F7.1,/,
     8  12X,'5. G1  (Scalar for relative leaf size)..........[',F7.2,/,
     9  12X,'6. G4  (Scalar for partitioning to panicle).....[',F7.2,/,
     9  12X,'7. PHINT (Phyllochron interval).................[',F7.1,/)

5400  FORMAT (12X,'0. End of changes',//,
     1  12X,'1. P1 (Growing degree days from emergence to',/,
     2  12X,'       end of juvenile phase)...................[',F7.1,/,
     3  12X,'2. P2 (Photoperiod sensitivity).................[',F7.1,/,
     4  12X,'3. P5 (Cumulative growing degree days from',/,
     5  12X,'       silking to maturity).....................[',F7.1,/,
     6  12X,'4. G2 (Potential kernel number).................[',F7.1,/,
     7  12X,'5. G3 (Potential kernel growth rate)............[',F7.1,/,
     8  12X,'6. PHINT (Phyllochron interval).................[',F7.1,/)

5450  FORMAT (12X,'0. End of changes',//,
     1  12X,'1. P1 (Growing degree days from emergence to',/,
     2  12X,'       end of juvenile phase)...................[',F7.1,/,
     3  12X,'2. P2 (Photoperiod sensitivity).................[',F7.1,/,
     4  12X,'3. P5 (Cumulative growing degree days from',/,
     5  12X,'       silking to maturity).....................[',F7.1,/,
     6  12X,'4. G2 (Potential kernel number).................[',F7.1,/,
     7  12X,'5. G3 (Potential kernel growth rate)............[',F7.1,/,
!JIL     8  12X,'6. PHINT (Phyllochron interval).................[',F7.1,/)
     8  12X,'6. PHINT (Phyllochron interval).................[',F7.1,/,
     9  12X,'7. AX (Surface area of largest leaf, cm2).......[',F7.1,/,
     9  12X,'8. LX (Longevity of most long-lived leaf, GDD)..[',F7.1,/)

5500  FORMAT 
     & (12X,'0. End of changes',//,
     1  12X,'1. P1    (Cumulative growing degree days from',/,
     2  12X,'          seedling emergence to end of juvenile)[',F7.1,/,
     3  12X,'2. P2O   (Optimal photoperiod for development)..[',F7.1,/,
     4  12X,'3. P2R   (Cumulative growing degree days delay',/,
     5  12X,'          for each hour increase above P2O......[',F7.1,/,
     6  12X,'4. P5    (Cumulative growing degree days from',/,
     7  12X,'          begin grain fill to phys. maturity)...[',F7.1,/,
     8  12X,'5. G1    (Scalar for relative leaf size)........[',F7.1,/,
     9  12X,'6. G2    (Scalar for partitioning to panicle)...[',F7.1,/,
     9  12X,'7. PHINT (Phyllochron interval).................[',F7.1,/,
     &  12X,'8. P3    (Cumulative growing degree days from',/,
     &  12X,'          panicle init to end of leaf growth....[',F7.1,/,
     &  12X,'9. P4    (Cum growing degree days from end of',/,
     &  12X,'          leaf growth to end of pannicle growth)[',F7.1,/,
     &  12X,'10.PBASE (Ceiling ppd. to delay devel. indef.)..[',F7.2,/,
     &  12X,'11.PSAT  (Photoperiod below which no dev. delay)[',F7.2,/)

5600  FORMAT (12X,'0. End of changes',//,
     1  12X,'1. P1V (Vernalization, days)....................[',F7.2,/,
     3  12X,'2. P1D (Photoperiod effect).....................[',F7.2,/,
     3  12X,'3. P5  (Grain filling duration, oC.d)...........[',F7.2,/,
     5  12X,'4. G1  (Kernel number per unit weight at anth.).[',F7.2,/,
     6  12X,'5. G2  (Kernel weight under optimum conditions).[',F7.2,/,
     7  12X,'6. G3  (Standard stem+spike dry weight at mat.).[',F7.2,/,
     9  12X,'7. PHINT (Phyllochron interval).................[',F7.2)

5700  FORMAT (12X,'0. End of changes ',//,
     2  12X,'1. G2 (Leaf expansion rate (cmý/mý/d))..........[',F7.1,/,
     3  12X,'2. G3 (Tuber growth rate (g/mý/d))..............[',F7.1,/,
     4  12X,'3. PD (Determinancy)............................[',F7.1,/,
     5  12X,'4. P2 (Photoperiod sensitivity (dimensionless)).[',F7.2,/,
     6  12X,'5. TC (Critical temperature (øC)................[',F7.1,/)

5800  FORMAT (12X,'0. End of changes ',//,
     1  12X,'1. P1..(10.00 -  800.0)..........................[',F7.1,/,
     2  12X,'2. P2R.( 5.00 -  500.0)..........................[',F7.2,/,
     3  12X,'3. P5..(50.00 - 1500.0)..........................[',F7.2,/,
     4  12X,'4. P2O.( 5.00 -   19.0)..........................[',F7.2,/,
     5  12X,'5. G1..(10.00 -  500.0)..........................[',F7.2,/,
     6  12X,'6. G2..( 0.00 -    0.5)..........................[',F7.2,/,
     7  12X,'7. G3..( 0.01 -    2.0)..........................[',F7.2,/,
     8  12X,'8. G4..( 0.01 -    2.0)..........................[',F7.2,/)

5900  FORMAT (12X,'0. End of changes',//,
     &  12X,'1. P1  (Growing degree days from emergence to',/,
     &  12X,'        harvest maturity)........................[',F7.1,/,
     &  12X,'2. RATPOT (Maximum # of ratoon crops before',/,
     &  12X,'        reseeding)...............................[',F7.1,/,
     &  12X,'3. LFMAX (Maximum # of green leaves on shoot)....[',F7.1,/,
     &  12X,'4. G1  (General leaf type : 1=narrow,width<30 mm',/,
     &  12X,'        2=medium,30<width<50 mm;3=broad width)...[',F7.1,/,
     &  12X,'5. PI1 (Phyllochron interval # 1)................[',F7.1,/,
     &  12X,'6. PI2 (Phylochron interval # 2).................[',F7.1,/,
     &  12X,'7. DTTPI (Growing degree day threshold between',/,
     &  12X,'        phyllochron interval #1 and #2)..........[',F7.1,/)

5950  FORMAT (12X,'0. End of changes',//,
     &  12X,'1. PI1   (Phyllocron interval 1).................[',F7.1,/,
     &  12X,'2. PI2   (Phyllocron interval 2).................[',F7.1,/,
     &  12X,'3. DTPI  (Thermal time threshold for phyllocron  ',/,
     &  12X,'          interval changes from Ph1 to Ph2)......[',F7.1,/,
     &  12X,'4. SMAX  (Max # of stalks).......................[',F7.1,/,
     &  12X,'5. SO    (Max stable stalk # in a stubble).......[',F7.1,/, 
     &  12X,'6. GMAX  (Threshold thermal time at which max ',/,
     &  12X,'          stalk number reached)..................[',F7.1,/,
     &  12X,'7. GO    (Threshold thermal time at which stable ',/,
     &  12X,'          stalk number set)......................[',F7.1,/,
     &  12X,'8. M1    (Slope of accumulated leaf area 1)......[',F7.1,/,
     &  12X,'9. M2    (Slope of accumulated leaf area 2)......[',F7.1,/,
     &  12X,'10.LA    (Leaf # at which no change to leaf area)[',I7,/,
     &  12X,'11.WTPSD (Maximum weight per seed g).............[',F7.3,/,
     &  12X,'12.SLAVAR(Specific leaf area of cultivar cm2/g)..[',F7.1,/,
     &  12X,'13.SIZLF (Max size of full leaf cm2).............[',F7.1,/)

6000  FORMAT (12X,'0. End of changes',//,
     1  12X,'1. P1 (Growing degree days from emergence to',/,
     2  12X,'       end of juvenile phase - 4ø base).........[',F7.1,/,
     3  12X,'2. P2 (Photoperiod sensitivity).................[',F7.2,/,
     4  12X,'3. P5 (Cumulative growing degree days from',/,
     5  12X,'       first anthesis to maturity)..............[',F7.1,/,
     6  12X,'4. G2 (Potential grains per head)...............[',F7.0,/,
     7  12X,'5. G3 (Potential kernel growth rate)............[',F7.2,/,
     8  12X,'6. Maximum kernel oil content (%)...............[',F7.0,/)


6100  FORMAT (12X,'0. End of changes',//,
     1  12X,'1. P2 (Growing degree days from forcing sepals',/,
     2  12X,'       closed on youngest flower)...............[',F6.1,/,
     3  12X,'2. P3 (Growing degree days from SCY to',/,
     4  12X,'       ealy flowering)..........................[',F6.1,/,
     5  12X,'3. P4 (Cumulative growing degree days from',/,
     6  12X,'       EF to maturity)..........................[',F6.0,/,
     7  12X,'4. G2 (Potential eye number)....................[',F6.1,/,
     8  12X,'5. G3 (Potential eye growth rate)...............[',F6.2,/,
     9  12X,'6. PHINT (Phyllochron interval).................[',F6.1,/)

6200  FORMAT (12X,'0. End of changes',//,
     1  12X,'1. P1 ..........................................[',F7.1,/,
     3  12X,'2. P3 ..........................................[',F7.2,/,
     4  12X,'3. P4 ..........................................[',F7.1,/,
     6  12X,'4. P5 ..........................................[',F7.2,/,
     7  12X,'5. G3 ..........................................[',F7.3,/,
     8  12X,'6. G4 ..........................................[',F7.3,/,
     9  12X,'6. PHINT .......................................[',F7.3,/,
     1  12X,'6. PCINT .......................................[',F7.3,/,
     2  12X,'6. PCGRD .......................................[',F7.3,/)

6300  FORMAT (12X,'0. End of changes',//,
     1  12X,'1. SCPB (Seed cotton per boll, g)...............[',F7.1,/,
     3  12X,'2. RESPC (Respiration constant).................[',F7.3,/,
     4  12X,'3. SQCON (Squaring constant)....................[',F7.3,/,
     6  12X,'4. FCUT (Cut out factor)........................[',F7.3,/,
     7  12X,'5. FLAI (Relative difference in leaf area)......[',F7.2,/,
     8  12X,'6. DDISQ (Thermal time to 1st square)...........[',F7.1,/)

      END SUBROUTINE INVRCE

C=======================================================================
C  INVRCS, Subroutine
C
C  Interactively edit genetic parameters - Cassava
c
C-----------------------------------------------------------------------
C  Revision history
C
C  04/15/93 PWW Written
C  04/18/93 PWW Header revision and minor changes
C
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  : IERR,IPARAM,NDEX
C
C  OUTPUT : None
C-----------------------------------------------------------------------
C  Called : SEVAR
C
C  Calls  : CLEAR SELPRO GETREAL
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  IERR   : Error flag
C  IPARAM : Menu choice
C  NDEX   : Error flag for FSELPRO
C=======================================================================

      SUBROUTINE INVRCS

      IMPLICIT NONE

      INCLUDE 'COMGEN.BLK'

      INTEGER  IERR,IPARAM,NDEX,L
C
C     Repeat until user chooses to quit
C
3001  CONTINUE

      CALL CLEAR
      WRITE (*,5000)
      WRITE (*,5300) (GCOEFF(L),L=1,15)
      WRITE (*,5100)
C
C     Get menu choice
C
      READ  (5,'(I2)',IOSTAT=IERR) IPARAM
      CALL SELPRO (0,25,IPARAM,NDEX,IERR)
      IF (NDEX .EQ. 2) GOTO 3001
C
C     Branch to menu choice
C
      IF (IPARAM .EQ. 0) THEN
          RETURN
      ELSE IF (IPARAM .EQ. 1) THEN
          CALL GETREAL (GCOEFF( 1),'DUB1 ', 0.01,7000.0)
      ELSE IF (IPARAM .EQ. 2) THEN
          CALL GETREAL (GCOEFF( 2),'DUBR ', 0.01,7000.0)
      ELSE IF (IPARAM .EQ. 3) THEN
          CALL GETREAL (GCOEFF( 3),'DESP ', 0.01,7000.0)
      ELSE IF (IPARAM .EQ. 4) THEN
          CALL GETREAL (GCOEFF( 4),'PHCX ', 0.01,7000.0)
      ELSE IF (IPARAM .EQ. 5) THEN
          CALL GETREAL (GCOEFF( 5),'S#PE ', 0.01,7000.0)
      ELSE IF (IPARAM .EQ. 6) THEN
          CALL GETREAL (GCOEFF( 6),'S#FX ', 0.01,7000.0)
      ELSE IF (IPARAM .EQ. 7) THEN
          CALL GETREAL (GCOEFF( 7),'S#PX ', 0.01,7000.0)
      ELSE IF (IPARAM .EQ. 8) THEN
          CALL GETREAL (GCOEFF( 8),'SWNX ', 0.01,7000.0)
      ELSE IF (IPARAM .EQ. 9) THEN
          CALL GETREAL (GCOEFF( 9),'L#IS ', 0.01,7000.0)
      ELSE IF (IPARAM .EQ. 10) THEN
          CALL GETREAL (GCOEFF(10),'L#IP ', 0.01,7000.0)
      ELSE IF (IPARAM .EQ. 11) THEN
          CALL GETREAL (GCOEFF(11),'LALX ', 0.01,7000.0)
      ELSE IF (IPARAM .EQ. 12) THEN
          CALL GETREAL (GCOEFF(12),'LAXA ', 0.01,7000.0)
      ELSE IF (IPARAM .EQ. 13) THEN
          CALL GETREAL (GCOEFF(13),'LAL3 ', 0.01,7000.0)
      ELSE IF (IPARAM .EQ. 14) THEN
          CALL GETREAL (GCOEFF(14),'LAWS ', 0.01,7000.0)
      ELSE IF (IPARAM .EQ. 15) THEN
          CALL GETREAL (GCOEFF(15),'LFLI ', 0.01,7000.0)
      ENDIF

      GOTO 3001
C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 5000 FORMAT (/,20X,'========================================',/
     1          20X,'Current Values of Coefficients to Modify',/
     2          20X,'========================================',///)
 5100 FORMAT (/,14X,'  Parameter choice [Default = 0] ===>  ',$)
 5300 FORMAT (
     &15X,' 1. DUB1..........[',F6.1,']   2. DUBR..........[',F6.1,']'/,
     &15X,' 3. DESP..........[',F6.2,']   4. PHCX..........[',F6.1,']'/,
     &15X,' 5. S#PE..........[',F6.1,']   6. S#FX..........[',F6.2,']'/,
     &15X,' 7. S#PX..........[',F6.1,']   8. SWNX..........[',F6.3,']'/,
     &15X,' 9. L#IS..........[',F6.2,']  10. L#IP..........[',F6.1,']'/,
     &15X,'11. LALX..........[',F6.1,']  12. LAXA..........[',F6.1,']'/,
     &15X,'13. LAL3..........[',F6.1,']  14. LAWS..........[',F6.1,']'/,
     &15X,'15. LFLI..........[',F6.1,']')

      END SUBROUTINE INVRCS

C=======================================================================
C  SELPRO, Subroutine
C
C  Procedure for checking integer input selections
C
C     Range is indicated nlow-nhigh
C     Message printer is mess
C     NTEST is the integer input selection
C-----------------------------------------------------------------------
C  Revision history
C
C  06/12/92 BB  Written
C  05/28/93 PWW Header revision and minor changes
C
C-----------------------------------------------------------------------
C  INPUT  : NLOW,NHIGH,NTEST,INDEX,IOSTAT
C
C  LOCAL  : BEEP,IOSTAT
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : INVAR
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  MESS   : Error message
C  MESS1  : Error message
C  NLOW   : Low range value
C  NHIGH  : High range value
C  NTEST  : Variable
C  INDEX  : Return value for variable 1 = OK, 2 = not
C  IOSTAT : Error indicator
C=======================================================================

      SUBROUTINE SELPRO (NLOW,NHIGH,NTEST,INDEX,IOSTAT)

      IMPLICIT     NONE

      CHARACTER*1  BEEP
      CHARACTER*70 MESS,MESS1

      INTEGER      NLOW,NTEST,IOSTAT,NHIGH,INDEX

      DATA MESS1/'Input out of range - Please select again'/
      DATA MESS /'Invalid selection  - Please select again'/

      BEEP = CHAR(7)

      IF (IOSTAT .NE. 0) THEN
         WRITE (*,10) MESS
         INDEX = 2
         RETURN
       ELSE
         INDEX = 1
      ENDIF
      IF (NTEST .LT. NLOW .OR. NTEST .GT. NHIGH) THEN
         WRITE (*, 5) BEEP
         WRITE (*,10) MESS1
         INDEX = 2
       ELSE
         INDEX = 1
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

    5 FORMAT (1X,A1)
   10 FORMAT (2X,A70,/)

      END SUBROUTINE SELPRO

C=======================================================================
C  GETREAL, Subroutine
C
C  Edits a variable
C-----------------------------------------------------------------------
C  Revision history
C
C  06/12/92 BB  Written
C  05/28/93 PWW Header revision and minor changes
C
C-----------------------------------------------------------------------
C  INPUT  : VARIABLE,VNAME,VLO,VHI
C
C  LOCAL  :
C
C  OUTPUT : VARIABLE
C-----------------------------------------------------------------------
C  Called : INVAR
C
C  Calls  : FSELPRO
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  VARIABLE : Variable value
C  VNAME    : Variable name
C  VLO      : Low boundary
C  VHI      : High boundary
C=======================================================================

      SUBROUTINE GETREAL (VARIABLE,VNAME,VLO,VHI)

      IMPLICIT    NONE

      CHARACTER*5 VNAME
      INTEGER     IERR,INDEX
      REAL        VARIABLE,VHI,VLO,VARTMP

  100 WRITE (*,200) VNAME,VARIABLE
      WRITE (*,300)
      READ  (5,*,IOSTAT=IERR) VARTMP
      CALL FSELPRO (VLO,VHI,VARTMP,INDEX,IERR)
      IF (INDEX .EQ. 1) VARIABLE = VARTMP
      IF (INDEX .EQ. 2) GOTO 100

      RETURN

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

  200 FORMAT (/,' The current value of ',A5, ' is ',F8.3)
  300 FORMAT (/,' Input new value : ')

      END SUBROUTINE GETREAL

C=======================================================================
C  FSELPRO, Subroutine
C
C  Procedure for checking floating point input selections
C
C    Range is indicated flow-fhigh
C    Message printer is mess
C    FTEST is the floating point input selection
C-----------------------------------------------------------------------
C  Revision history
C
C  06/12/92 BB  Written
C  05/28/93 PWW Header revision and minor changes
C
C-----------------------------------------------------------------------
C  INPUT  : FLOW,FHIGH,FTEST,IOSTAT
C
C  LOCAL  : MESS,MESS1,BEEP
C
C  OUTPUT : INDEX
C-----------------------------------------------------------------------
C  Called : GETREAL
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  MESS   : Error message
C  MESS1  : Error message
C  FLOW   : Low range value
C  FHIGH  : High range value
C  FTEST  : Variable
C  INDEX  : Return value for variable 1 = OK, 2 = not
C  IOSTAT : Error indicator
C=======================================================================

      SUBROUTINE FSELPRO (FLOW,FHIGH,FTEST,INDEX,IOSTAT)

      IMPLICIT     NONE

      CHARACTER*70 MESS,MESS1
      CHARACTER*1  BEEP
      INTEGER      IOSTAT,INDEX
      REAL         FTEST,FHIGH,FLOW

      DATA MESS1 /'Input out of range - Please select again'/
      DATA MESS  /'Invalid selection  - Please select again'/

      BEEP = CHAR(7)
      IF (IOSTAT .NE. 0) THEN
         WRITE (*,10) MESS
         INDEX = 2
         RETURN
       ELSE
         INDEX = 1
      ENDIF
      IF (FTEST .LT. FLOW .OR. FTEST .GT. FHIGH) THEN
         WRITE (*, 5) BEEP
         WRITE (*,10) MESS1
         INDEX = 2
       ELSE
         INDEX = 1
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

    5 FORMAT (1X,A1)
   10 FORMAT (2X,A70,/)

      END SUBROUTINE FSELPRO

C=======================================================================
C  GETINT, Subroutine
C
C  Edits a variable
C-----------------------------------------------------------------------
C  Revision history
C
C  01/06/2005 CHP Written based on GETREAL and FSELPRO
C
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  VARIABLE : Variable value
C  VNAME    : Variable name
C  VLO      : Low boundary
C  VHI      : High boundary
C=======================================================================

      SUBROUTINE GETINT (VARIABLE,VNAME,VLO,VHI)

      IMPLICIT    NONE

      CHARACTER*1  BEEP
      CHARACTER*5 VNAME
      CHARACTER*70 MESS,MESS1
      INTEGER     IERR,INDEX
      INTEGER     VARIABLE,VHI,VLO,VARTMP


      DATA MESS1 /'Input out of range - Please select again'/
      DATA MESS  /'Invalid selection  - Please select again'/

  100 WRITE (*,200) VNAME, VARIABLE
      WRITE (*,300)
      READ  (5,*,IOSTAT=IERR) VARTMP

      BEEP = CHAR(7)
      IF (IERR .NE. 0) THEN
         WRITE (*,10) MESS
         INDEX = 2
         !RETURN
       ELSE
         INDEX = 1
      ENDIF

      IF (VARTMP .LT. VLO .OR. VARTMP .GT. VHI) THEN
         WRITE (*, 5) BEEP
         WRITE (*,10) MESS1
         INDEX = 2
       ELSE
         INDEX = 1
      ENDIF

      IF (INDEX .EQ. 1) VARIABLE = VARTMP
      IF (INDEX .EQ. 2) GOTO 100

      RETURN

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

    5 FORMAT (1X,A1)
   10 FORMAT (2X,A70,/)
  200 FORMAT (/,' The current value of ',A5, ' is ',I8)
  300 FORMAT (/,' Input new value : ')

      END SUBROUTINE GETINT

C=======================================================================
