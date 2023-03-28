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
C  12/14/2005 CHP Added sorghum cultivar coefficients, PBASE and PSAT
C  11/26/2007 CHP THRESH, SDPRO, SDLIP moved from eco to cul file
C
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
      EXTERNAL CLEAR, UPCASE, VERIFY

      INCLUDE     'COMGEN.blk'

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
C  Interactively edit genetic parameters - Other models
c
C-----------------------------------------------------------------------
C  Revision history
C
C  06/12/1992 BB  Written
C  05/28/1993 PWW Header revision and minor changes
!  12/14/2005 CHP Added sorghum cultivar coefficients, PBASE and PSAT
!  02/06/2007 CHP Added alternate sugarcane parameters for CASUPRO
!  09/16/2007 JIL Added new inputs for IXIM
!  06/30/2010 FSR Added PLF2 variable for CASUPRO
C  08/09/2012 GH  Updated for cassava
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
      EXTERNAL CLEAR, SELPRO, GETREAL

      INCLUDE  'COMGEN.blk'

      INTEGER     IERR,IPARAM,NDEX
      CHARACTER*2 CROP
      CHARACTER*8 MODEL
C
C     Repeat until user chooses to quit
C
3001  CONTINUE
      CALL CLEAR
      WRITE (*,5000)
5000  FORMAT (/,20X,'========================================',/,
     1          20X,'Current Values of Coefficients to Modify',/,
     2          20X,'========================================',///)

      SELECT CASE (MODEL(1:5))

!=======================================================================
!     Millet
      CASE ('MLCER')
        WRITE (*,5300) P1,P2O,P2R,P5,G1,G4,PHINT
5300    FORMAT (12X,'0. End of changes ',//,
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

         WRITE (*,5100)
5100     FORMAT (/,'  Parameter choice [Default = 0] ===>  ',$)

C        Get menu choice
         READ  (5,'(I2)',IOSTAT=IERR) IPARAM
         CALL SELPRO (0,7,IPARAM,NDEX,IERR)
         IF (NDEX .EQ. 2) GOTO 3001
C
C        Branch to menu choice
         SELECT CASE (IPARAM)
         CASE (0); RETURN
         CASE (1); CALL GETREAL (P1,   'P1   ',   1.0,  800.0)
         CASE (2); CALL GETREAL (P2O,  'P2O  ',   5.0,   19.0)
         CASE (3); CALL GETREAL (P2R,  'P2R  ',   5.0, 9000.0)
         CASE (4); CALL GETREAL (P5,   'P5   ', 100.0,  900.0)
         CASE (5); CALL GETREAL (G1,   'G1   ',   0.0,   50.0)
         CASE (6); CALL GETREAL (G4,   'G4   ',   0.0,    1.0)
         CASE (7); CALL GETREAL (PHINT,'PHINT',   1.0,  200.0)
         END SELECT

!=======================================================================
!     Maize, sweet corn
      CASE ('MZCER', 'SWCER')

        WRITE (*,5400) P1,P2,P5,G2,G3,PHINT
5400    FORMAT (12X,'0. End of changes',//,
     1  12X,'1. P1 (Growing degree days from emergence to',/,
     2  12X,'       end of juvenile phase)...................[',F7.1,/,
     3  12X,'2. P2 (Photoperiod sensitivity).................[',F7.1,/,
     4  12X,'3. P5 (Cumulative growing degree days from',/,
     5  12X,'       silking to maturity).....................[',F7.1,/,
     6  12X,'4. G2 (Potential kernel number).................[',F7.1,/,
     7  12X,'5. G3 (Potential kernel growth rate)............[',F7.1,/,
     8  12X,'6. PHINT (Phyllochron interval).................[',F7.1,/)

         WRITE (*,5100)
C
C        Get menu choice
         READ  (5,'(I2)',IOSTAT=IERR) IPARAM
         CALL SELPRO (0,6,IPARAM,NDEX,IERR)
         IF (NDEX .EQ. 2) GOTO 3001
C
C        Branch to menu choice
         SELECT CASE (IPARAM)
         CASE (0); RETURN
         CASE (1); CALL GETREAL (P1,'P1   ', 80.0, 500.0)
         CASE (2); CALL GETREAL (P2,'P2   ',  0.0,  10.0)
         CASE (3); CALL GETREAL (P5,'P5   ',100.0,2000.0)
         CASE (4); CALL GETREAL (G2,'G2   ',100.0,2000.0)
         CASE (5); CALL GETREAL (G3,'G3   ',  1.0,  15.0)
         CASE (6); CALL GETREAL (PHINT,'PHINT',1.0,200.0)
         END SELECT

!=======================================================================
C**WDB 12/2015 Added this section for Sugarbeet

      CASE ('BSCER')

        WRITE (*,5401) P1,P2,P5,G2,G3,PHINT
5401    FORMAT (12X,'0. End of changes',//,
     1  12X,'1. P1 (Growing degree days from emergence to',/,
     2  12X,'       end of juvenile phase)...................[',F7.1,/,
     3  12X,'2. P2 (Photoperiod sensitivity).................[',F7.1,/,
     4  12X,'3. P5 (Cumulative growing degree days from',/,
     5  12X,'       silking to maturity).....................[',F7.1,/,
     6  12X,'4. G2 (Potential kernel number).................[',F7.1,/,
     7  12X,'5. G3 (Potential kernel growth rate)............[',F7.1,/,
     8  12X,'6. PHINT (Phyllochron interval).................[',F7.1,/)
        
C**WDB end changes for Sugarbeets
         WRITE (*,5100)
C
C        Get menu choice
         READ  (5,'(I2)',IOSTAT=IERR) IPARAM
         CALL SELPRO (0,6,IPARAM,NDEX,IERR)
         IF (NDEX .EQ. 2) GOTO 3001
C
C        Branch to menu choice
         SELECT CASE (IPARAM)
         CASE (0); RETURN
         CASE (1); CALL GETREAL (P1,'P1   ', 450.0, 1100.0)
         CASE (2); CALL GETREAL (P2,'P2   ',  0.0,  0.0001)
         CASE (3); CALL GETREAL (P5,'P5   ',600.0,1000.0)
         CASE (4); CALL GETREAL (G2,'G2   ',170.0,220.0)
         CASE (5); CALL GETREAL (G3,'G3   ', 20.0,  50.0)
         CASE (6); CALL GETREAL (PHINT,'PHINT',1.0,200.0)
         END SELECT
         
!=======================================================================
!     Maize - IXIM
      CASE ('MZIXM')
        WRITE (*,5450) P1,P2,P5,G2,G3,PHINT,AX,LX
5450    FORMAT (12X,'0. End of changes',//,
     1  12X,'1. P1 (Growing degree days from emergence to',/,
     2  12X,'       end of juvenile phase)...................[',F7.1,/,
     3  12X,'2. P2 (Photoperiod sensitivity).................[',F7.1,/,
     4  12X,'3. P5 (Cumulative growing degree days from',/,
     5  12X,'       silking to maturity).....................[',F7.1,/,
     6  12X,'4. G2 (Potential kernel number).................[',F7.1,/,
     7  12X,'5. G3 (Potential kernel growth rate)............[',F7.1,/,
     8  12X,'6. PHINT (Phyllochron interval).................[',F7.1,/,
     9  12X,'7. AX (Surface area of largest leaf, cm2).......[',F7.1,/,
     9  12X,'8. LX (Longevity of most long-lived leaf, GDD)..[',F7.1,/)

         WRITE (*,5100)
C
C        Get menu choice
         READ  (5,'(I2)',IOSTAT=IERR) IPARAM
         CALL SELPRO (0,8,IPARAM,NDEX,IERR)
         IF (NDEX .EQ. 2) GOTO 3001
C
C        Branch to menu choice
         SELECT CASE (IPARAM)
         CASE (0); RETURN
C**WDB 12/2015 The value for P1 for Sugarbeet may need to be higher
C** than the maximum value of 500 as defined in the original code.
C** Changed maximum allowable value to 999.
             
C** Original code         CASE (1); CALL GETREAL (P1,'P1   ', 80.0, 500.0)
         CASE (1); CALL GETREAL (P1,'P1   ', 80.0, 999.0)
C** WDB end changes
         CASE (2); CALL GETREAL (P2,'P2   ',  0.0,  10.0)
         CASE (3); CALL GETREAL (P5,'P5   ',100.0,2000.0)
         CASE (4); CALL GETREAL (G2,'G2   ',100.0,2000.0)
         CASE (5); CALL GETREAL (G3,'G3   ',  1.0,  15.0)
         CASE (6); CALL GETREAL (PHINT,'PHINT',1.0,200.0)
         CASE (7); CALL GETREAL (AX,'AX   ',400.0,1500.0)
         CASE (8); CALL GETREAL (LX,'LX   ',500.0,2000.0)
         END SELECT

!=======================================================================
!     Sorghum
      CASE ('SGCER')
        WRITE (*,5500) P1,P2O,P2R,P5,G1,G2,PHINT,P3,P4,P2,PBASE,PSAT
5500    FORMAT 
     &  (12X,'0. End of changes',//,
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
     &  12X,'10.P2    (Thermal time from end of juvenile to ',/,
     &  12X,'          tassel initiation under short days    [',F7.1,/,
     &  12X,'11.PANTH (Thermal time, end tassel init to anth)[',F7.1,/,
     &  12X,'12.PBASE (Ceiling ppd. to delay devel. indef.)..[',F7.2,/,
     &  12X,'13.PSAT  (Photoperiod below which no dev. delay)[',F7.2,/)

         WRITE (*,5100)
C
C        Get menu choice
         READ  (5,'(I2)',IOSTAT=IERR) IPARAM
         CALL SELPRO (0,13,IPARAM,NDEX,IERR)
         IF(NDEX .EQ. 2) GOTO 3001
C
C        Branch to menu choice
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
         CASE(10); CALL GETREAL (P2,   'P2   ',100.0, 900.0)
         CASE(11); CALL GETREAL (PANTH,'PANTH',  0.0,9000.0)
         CASE(12); CALL GETREAL (PBASE,'PBASE',  5.0,  24.0)
         CASE(13); CALL GETREAL (PSAT, 'PSAT ',  5.0,  24.0)
         END SELECT

!=======================================================================
!     FV added case for sunflower model OilcropSun 10/20/2020
      CASE ('SUOIL')

        WRITE (*,5410) P1,P2,P5,G2,G3,PHINT
5410    FORMAT (12X,'0. End of changes',//,
     1  12X,'1. P1 (Growing degree days from emergence to',/,
     2  12X,'       end of juvenile phase)...................[',F7.1,/,
     3  12X,'2. P2 (Photoperiod sensitivity).................[',F7.1,/,
     4  12X,'3. P5 (Cumulative growing degree days from',/,
     5  12X,'       anthesis to maturity).....................[',F7.1,/,
     6  12X,'4. G2 (Potential grain number).................[',F7.1,/,
     7  12X,'5. G3 (Potential kernel growth rate)............[',F7.1,/,
     8  12X,'6. O1 (Oil kernel concentration)........[',F7.1,/)

         WRITE (*,5100)
C
C        Get menu choice
         READ  (5,'(I2)',IOSTAT=IERR) IPARAM
         CALL SELPRO (0,6,IPARAM,NDEX,IERR)
         IF (NDEX .EQ. 2) GOTO 3001
C
C        Branch to menu choice
         SELECT CASE (IPARAM)
         CASE (0); RETURN
         CASE (1); CALL GETREAL (P1,'P1   ',250.0, 450.0)
         CASE (2); CALL GETREAL (P2,'P2   ',  0.5,  12.0)
         CASE (3); CALL GETREAL (P5,'P5   ',550.0, 750.0)
         CASE (4); CALL GETREAL (G2,'G2   ',900.0,3000.0)
         CASE (5); CALL GETREAL (G3,'G3   ',  1.2,   2.4)
         CASE (6); CALL GETREAL (O1,'O1   ', 35.0,  70.0)
         END SELECT
         
!=======================================================================
!     Potato
      CASE ('PTSUB')
        WRITE (*,5700) G2,G3,PD,P2,TC
5700    FORMAT (12X,'0. End of changes ',//,
     2  12X,'1. G2 (Leaf expansion rate (cm²/m²/d))..........[',F7.1,/,
     3  12X,'2. G3 (Tuber growth rate (g/m²/d))..............[',F7.1,/,
     4  12X,'3. PD (Determinancy)............................[',F7.1,/,
     5  12X,'4. P2 (Photoperiod sensitivity (dimensionless)).[',F7.2,/,
     6  12X,'5. TC (Critical temperature (˚C)................[',F7.1,/)

         WRITE (*,5100)
C
C        Get menu choice
         READ  (5,'(I2)',IOSTAT=IERR) IPARAM
         CALL SELPRO (0,5,IPARAM,NDEX,IERR)
         IF (NDEX .EQ. 2) GOTO 3001
C
C        Branch to menu choice
         SELECT CASE(IPARAM)
         CASE(0);  RETURN
         CASE(1);  CALL GETREAL (G2,'G2   ',0.0,3000.0)
         CASE(2);  CALL GETREAL (G3,'G3   ',0.0,3000.0)
         CASE(3);  CALL GETREAL (PD,'PD   ',0.0,3000.0)
         CASE(4);  CALL GETREAL (P2,'P2   ',0.0,3000.0)
         CASE(5);  CALL GETREAL (TC,'TC   ',0.0,3000.0)
         END SELECT

!=======================================================================
!     Rice
      CASE ('RICER')
        WRITE (*,5800) P1,P2R,P5,P2O,G1,G2,G3,G4
5800    FORMAT (12X,'0. End of changes ',//,
     1  12X,'1. P1..(10.00 -  800.0)..........................[',F7.1,/,
     2  12X,'2. P2R.( 5.00 -  500.0)..........................[',F7.2,/,
     3  12X,'3. P5..(50.00 - 1500.0)..........................[',F7.2,/,
     4  12X,'4. P2O.( 5.00 -   19.0)..........................[',F7.2,/,
     5  12X,'5. G1..(10.00 -  500.0)..........................[',F7.2,/,
     6  12X,'6. G2..( 0.00 -    0.5)..........................[',F7.2,/,
     7  12X,'7. G3..( 0.01 -    2.0)..........................[',F7.2,/,
     8  12X,'8. G4..( 0.01 -    2.0)..........................[',F7.2,/)

         WRITE (*,5100)
C
C        Get menu choice
         READ  (5,'(I2)',IOSTAT=IERR) IPARAM
         CALL SELPRO (0,8,IPARAM,NDEX,IERR)
         IF (NDEX .EQ. 2) GOTO 3001
C
C        Branch to menu choice
         SELECT CASE(IPARAM)
         CASE(0);  RETURN
         CASE(1);  CALL GETREAL (P1,  'P1   ',0.0,5000.0)
         CASE(2);  CALL GETREAL (P2R, 'P2R  ',0.0,5000.0)
         CASE(3);  CALL GETREAL (P5,  'P5   ',0.0,5000.0)
         CASE(4);  CALL GETREAL (P2O, 'P2O  ',0.0,5000.0)
         CASE(5);  CALL GETREAL (G1,  'G1   ',0.0,5000.0)
         CASE(6);  CALL GETREAL (G2,  'G2   ',0.0,5000.0)
         CASE(7);  CALL GETREAL (G3,  'G3   ',0.0,5000.0)
         CASE(8);  CALL GETREAL (G4,  'G4   ',0.0,5000.0)
         END SELECT

!=======================================================================
!     CSCER - Wheat, barley
      CASE ('CSCER')
	  WRITE (*,5600) P1V,P1D,P5,G1,G2,G3,PHINT
5600    FORMAT (12X,'0. End of changes',//,
     1  12X,'1. P1V (Vernalization, days)....................[',F7.2,/,
     3  12X,'2. P1D (Photoperiod effect).....................[',F7.2,/,
     3  12X,'3. P5  (Grain filling duration, oC.d)...........[',F7.2,/,
     5  12X,'4. G1  (Kernel number per unit weight at anth.).[',F7.2,/,
     6  12X,'5. G2  (Kernel weight under optimum conditions).[',F7.2,/,
     7  12X,'6. G3  (Standard stem+spike dry weight at mat.).[',F7.2,/,
     9  12X,'7. PHINT (Phyllochron interval).................[',F7.2)

         WRITE (*,5100)
C
C        Get menu choice
         READ  (5,'(I2)',IOSTAT=IERR) IPARAM
         CALL SELPRO (0,10,IPARAM,NDEX,IERR)
         IF (NDEX .EQ. 2) GOTO 3001
C
C        Branch to menu choice
         SELECT CASE(IPARAM)
         CASE(0);  RETURN
         CASE(1);  CALL GETREAL (P1V,  'P1V  ',  0.0,  60.0)
         CASE(2);  CALL GETREAL (P1D,  'P1D  ',  0.0,  200.)
         CASE(3);  CALL GETREAL (P5,   'P5   ', 100.,  999.)
         CASE(4);  CALL GETREAL (G1,   'G1   ', 10.0,  50.0)
         CASE(5);  CALL GETREAL (G2,   'G2   ', 10.0,  80.0)
         CASE(6);  CALL GETREAL (G3,   'G3   ',  0.5,   8.0)
         CASE(7)
           SELECT CASE (CROP)
           CASE('WH'); CALL GETREAL (PHINT,'PHINT', 30.0,  150.)
           CASE('BA'); CALL GETREAL (PHINT,'PHINT', 30.0,  100.)
           END SELECT
         END SELECT

!=======================================================================
!     CSCRP - CropSim wheat, barley
      CASE ('CSCRP')
        SELECT CASE (CROP)

!     --------------------------------------------------------------
!       CSCRP wheat
        CASE ('WH')
          WRITE (*,5920) 
     &      VREQ, PPS1, P8, GNOWT, GWTS, SHWTS, PHINT, 
     &      P1, P2, P3, P4, P5, P6, P7, 
     &      LA1S, LAFV, LAFR, VBASE, VEFF, PPS2
5920      FORMAT (12X,'0. End of changes ',//,
     1  12X,' 1. VREQ ( 0.00 -   60.0)............[',F7.1,/,
     4  12X,' 2. PPS1 ( 0.00 -  300.0)............[',F7.2,/,
     6  12X,' 3. P8.. (100.0 -  800.0)............[',F7.2,/,
     7  12X,' 4. GNOWT( 10.0 -   50.0)............[',F7.2,/,
     7  12X,' 5. GWTS ( 10.0 -   75.0)............[',F7.2,/,
     7  12X,' 6. SHWTS( 0.50 -   5.00)............[',F7.2,/,
     7  12X,' 7. PHINT( 40.0 -  120.0)............[',F7.2,/,
     5  12X,' 8. P1.. (100.0 -  800.0)............[',F7.2,/,
     6  12X,' 9. P2.. ( 60.0 -  100.0)............[',F7.2,/,
     6  12X,'10. P3.. (100.0 -  300.0)............[',F7.2,/,
     6  12X,'11. P4.. ( 50.0 -  500.0)............[',F7.2,/,
     6  12X,'12. P5.. ( 50.0 -  400.0)............[',F7.2,/,
     6  12X,'13. P6.. ( 10.0 -  100.0)............[',F7.2,/,
     6  12X,'14. P7.. ( 50.0 -  300.0)............[',F7.2,/,
     7  12X,'15. LA1S ( 0.10 -   10.0)............[',F7.2,/,
     7  12X,'16. LAFV (0.010 -   5.00)............[',F7.3,/,
     7  12X,'17. LAFR ( 0.10 -   5.00)............[',F7.2,/,
     2  12X,'18. VBASE( 0.00 -   30.0)............[',F7.2,/,
     3  12X,'19. VEFF ( 0.00 -   1.00)............[',F7.2,/,
     4  12X,'20. PPS2 ( 0.00 -  200.0)............[',F7.2,/)

          WRITE (*,5100)
C
C         Get menu choice
          READ  (5,'(I2)',IOSTAT=IERR) IPARAM
          CALL SELPRO (0,20,IPARAM,NDEX,IERR)
          IF (NDEX .EQ. 2) GOTO 3001
C
C         Branch to menu choice
          SELECT CASE(IPARAM)
          CASE(0);  RETURN
          CASE( 1);  CALL GETREAL (VREQ ,'VREQ ', 0.00,  60.0)
          CASE( 2);  CALL GETREAL (PPS1 ,'PPS1 ', 0.00, 300.0)
          CASE( 3);  CALL GETREAL (P8   ,'P8.. ',100.0, 800.0)
          CASE( 4);  CALL GETREAL (GNOWT,'GNOWT', 10.0,  50.0)
          CASE( 5);  CALL GETREAL (GWTS ,'GWTS ', 10.0,  75.0)
          CASE( 6);  CALL GETREAL (SHWTS,'SHWTS', 0.50,  5.00)
          CASE( 7);  CALL GETREAL (PHINT,'PHINT', 40.0, 120.0)
          CASE( 8);  CALL GETREAL (P1   ,'P1.. ',100.0, 800.0)
          CASE( 9);  CALL GETREAL (P2   ,'P2.. ', 60.0, 100.0)
          CASE(10);  CALL GETREAL (P3   ,'P3.. ',100.0, 300.0)
          CASE(11);  CALL GETREAL (P4   ,'P4.. ', 50.0, 500.0)
          CASE(12);  CALL GETREAL (P5   ,'P5.. ', 50.0, 400.0)
          CASE(13);  CALL GETREAL (P6   ,'P6.. ', 10.0, 100.0)
          CASE(14);  CALL GETREAL (P7   ,'P7.. ', 50.0, 300.0)
          CASE(15);  CALL GETREAL (LA1S ,'LA1S ', 0.10,  10.0)
          CASE(16);  CALL GETREAL (LAFV ,'LAFV ',0.010,  5.00)
          CASE(17);  CALL GETREAL (LAFR ,'LAFR ', 0.10,  5.00)
          CASE(18);  CALL GETREAL (VBASE,'VBASE', 0.00,  30.0)
          CASE(19);  CALL GETREAL (VEFF ,'VEFF ', 0.00,  1.00)
          CASE(20);  CALL GETREAL (PPS2 ,'PPS2 ', 0.00, 200.0)
          END SELECT

!     --------------------------------------------------------------
!       CSCRP barley
        CASE ('BA')
          WRITE (*,5930) 
     &      VREQ, PPS1, P8, GNOWT, GWTS, SHWTS, PHINT, 
     &      P1, P2, P3, P4, P5, P6, P7, 
     &      LA1S, LAFV, LAFR, VBASE, VEFF, PPS2
5930      FORMAT (12X,'0. End of changes ',//,
     1  12X,' 1. VREQ ( 0.00 -   60.0)............[',F7.1,/,
     4  12X,' 2. PPS1 ( 0.00 -  300.0)............[',F7.2,/,
     6  12X,' 3. P8.. (100.0 -  800.0)............[',F7.2,/,
     7  12X,' 4. GNOWT( 10.0 -   50.0)............[',F7.2,/,
     7  12X,' 5. GWTS ( 10.0 -   75.0)............[',F7.2,/,
     7  12X,' 6. SHWTS( 0.50 -   5.00)............[',F7.2,/,
     7  12X,' 7. PHINT( 40.0 -  120.0)............[',F7.2,/,
     5  12X,' 8. P1.. (100.0 -  800.0)............[',F7.2,/,
     6  12X,' 9. P2.. ( 60.0 -  100.0)............[',F7.2,/,
     6  12X,'10. P3.. (100.0 -  300.0)............[',F7.2,/,
     6  12X,'11. P4.. ( 50.0 -  500.0)............[',F7.2,/,
     6  12X,'12. P5.. ( 50.0 -  400.0)............[',F7.2,/,
     6  12X,'13. P6.. ( 10.0 -  100.0)............[',F7.2,/,
     6  12X,'14. P7.. ( 50.0 -  300.0)............[',F7.2,/,
     7  12X,'15. LA1S ( 0.10 -   10.0)............[',F7.2,/,
     7  12X,'16. LAFV (0.010 -   5.00)............[',F7.3,/,
     7  12X,'17. LAFR ( 0.10 -   5.00)............[',F7.2,/,
     2  12X,'18. VBASE( 0.00 -   30.0)............[',F7.2,/,
     3  12X,'19. VEFF ( 0.00 -   1.00)............[',F7.2,/,
     4  12X,'20. PPS2 ( 0.00 -  200.0)............[',F7.2,/)

          WRITE (*,5100)
C
C         Get menu choice
          READ  (5,'(I2)',IOSTAT=IERR) IPARAM
          CALL SELPRO (0,20,IPARAM,NDEX,IERR)
          IF (NDEX .EQ. 2) GOTO 3001
C
C         Branch to menu choice
          SELECT CASE(IPARAM)
          CASE(0);  RETURN
          CASE( 1);  CALL GETREAL (VREQ ,'VREQ ', 0.00,  60.0)
          CASE( 2);  CALL GETREAL (PPS1 ,'PPS1 ', 0.00, 300.0)
          CASE( 3);  CALL GETREAL (P8   ,'P8.. ',100.0, 800.0)
          CASE( 4);  CALL GETREAL (GNOWT,'GNOWT', 10.0,  50.0)
          CASE( 5);  CALL GETREAL (GWTS ,'GWTS ', 10.0,  75.0)
          CASE( 6);  CALL GETREAL (SHWTS,'SHWTS', 0.50,  5.00)
          CASE( 7);  CALL GETREAL (PHINT,'PHINT', 40.0, 120.0)
          CASE( 8);  CALL GETREAL (P1   ,'P1.. ',100.0, 800.0)
          CASE( 9);  CALL GETREAL (P2   ,'P2.. ', 60.0, 100.0)
          CASE(10);  CALL GETREAL (P3   ,'P3.. ',100.0, 300.0)
          CASE(11);  CALL GETREAL (P4   ,'P4.. ', 50.0, 500.0)
          CASE(12);  CALL GETREAL (P5   ,'P5.. ', 50.0, 400.0)
          CASE(13);  CALL GETREAL (P6   ,'P6.. ', 10.0, 100.0)
          CASE(14);  CALL GETREAL (P7   ,'P7.. ', 50.0, 300.0)
          CASE(15);  CALL GETREAL (LA1S ,'LA1S ', 0.10,  10.0)
          CASE(16);  CALL GETREAL (LAFV ,'LAFV ',0.010,  5.00)
          CASE(17);  CALL GETREAL (LAFR ,'LAFR ', 0.10,  5.00)
          CASE(18);  CALL GETREAL (VBASE,'VBASE', 0.00,  30.0)
          CASE(19);  CALL GETREAL (VEFF ,'VEFF ', 0.00,  1.00)
          CASE(20);  CALL GETREAL (PPS2 ,'PPS2 ', 0.00, 200.0)
          END SELECT
      END SELECT

!     --------------------------------------------------------------
!       CSCAS cassava
        CASE ('CSCAS')
          WRITE(*,5940)  
     &     PPS1, B01ND, B12ND, B23ND, B34ND, B45ND, B56ND,
     &     SRNWT, SRFR, HMPC, PHINT, LA1S, LAXS, LAXND, LAXN2,
     &     LAFS, LAFND, SLASS, LLIFA, LPEFR, STFR
5940      FORMAT (12X,'0. End of changes ',//,
     1  12X,' 1. PPS1  ( 0.00 -   0.00).......................[',F7.3,/,
     2  12X,' 2. B01ND ( 10.0 -  100.0).......................[',F7.2,/,
     3  12X,' 3. B12ND ( 10.0 -  100.0).......................[',F7.2,/,
     4  12X,' 4. B23ND ( 10.0 -  100.0).......................[',F7.2,/,
     5  12X,' 5. B34ND ( 10.0 -  100.0).......................[',F7.2,/,
     6  12X,' 6. B45ND ( 10.0 -  200.0).......................[',F7.2,/,
     7  12X,' 7. B56ND ( 10.0 -  200.0).......................[',F7.2,/,
     6  12X,' 8. SR#WT ( 0.20 -   0.35).......................[',F7.3,/,
     6  12X,' 9. SRFR  ( 0.25 -   0.35).......................[',F7.3,/,
     6  12X,'10. HMPC  ( 40.0 -   80.0).......................[',F7.3,/,
     6  12X,'11. PHINT ( 15.0 -   30.0).......................[',F7.3,/,
     6  12X,'12. LA1S  (200.0 -  400.0).......................[',F7.2,/,
     6  12X,'13. LAXS  ( 1000 -   2000).......................[',F7.1,/,
     6  12X,'14. LAXND ( 80.0 -  200.0).......................[',F7.2,/,
     6  12X,'15. LAXN2 ( 80.0 -  200.0).......................[',F7.2,/,
     7  12X,'16. LAFS  ( 20.0 -   50.0).......................[',F7.3,/,
     7  12X,'17. LAFND (100.0 -  350.0).......................[',F7.2,/,
     7  12X,'18. SLAS  (100.0 -  400.0).......................[',F7.2,/,
     7  12X,'19. LLIFA (300.0 - 1200.0).......................[',F7.1,/, !LPM redefine limit values
     7  12X,'20. LPEFR (0.250 -  0.400).......................[',F7.3,/,
     7  12X,'21. STFR  (0.250 -  0.450).......................[',F7.3,/)

          WRITE (*,5100)
C
C         Get menu choice
          READ  (5,'(I2)',IOSTAT=IERR) IPARAM
          CALL SELPRO (0,22,IPARAM,NDEX,IERR)
          IF (NDEX .EQ. 2) GOTO 3001
C
C         Branch to menu choice
          SELECT CASE(IPARAM)
          CASE(0);  RETURN
          CASE( 1);  CALL GETREAL (PPS1  ,'PPS1 ', 0.00,  0.00)
          CASE( 2);  CALL GETREAL (B01ND ,'B01ND ', 10.0, 100.0)
          CASE( 3);  CALL GETREAL (B12ND ,'B12ND ', 10.0, 100.0)
          CASE( 4);  CALL GETREAL (B23ND ,'B23ND ', 10.0, 100.0)
          CASE( 5);  CALL GETREAL (B34ND ,'B34ND ', 10.0, 100.0)
          CASE( 6);  CALL GETREAL (B45ND ,'B45ND ', 10.0, 200.0)
          CASE( 7);  CALL GETREAL (B56ND ,'B56ND ', 10.0, 200.0)
          CASE( 8);  CALL GETREAL (SRNWT ,'SR#WT ',0.200, 0.350)
          CASE( 9);  CALL GETREAL (SRFR  ,'SRFR  ',0.250, 0.350)
          CASE(10);  CALL GETREAL (HMPC  ,'HMPC  ', 40.0,  80.0)
          CASE(11);  CALL GETREAL (PHINT ,'PHINT', 15.0,  30.0)
          CASE(12);  CALL GETREAL (LA1S  ,'LA1S ',200.0, 400.0)
          CASE(13);  CALL GETREAL (LAXS  ,'LAXS',1000.,2000.0)
          CASE(14);  CALL GETREAL (LAXND ,'LAXND ', 80.0, 200.0)
          CASE(15);  CALL GETREAL (LAXN2 ,'LAXN2', 80.0, 200.0)
          CASE(16);  CALL GETREAL (LAFS  ,'LAFS', 20.0,  50.0)
          CASE(17);  CALL GETREAL (LAFND ,'LAFND ',100.0, 350.0)
          CASE(18);  CALL GETREAL (SLASS ,'SLAS ',100.0, 400.0)
          CASE(19);  CALL GETREAL (LLIFA ,'LLIFA ',300.0,1200.0)
          CASE(20);  CALL GETREAL (LPEFR ,'LPEFR ',0.200, 0.400)
          CASE(21);  CALL GETREAL (STFR  ,'STFR ',0.250, 0.450)
          END SELECT

!     --------------------------------------------------------------
!       CSYCA cassava
        CASE ('CSYCA')
            
         
          WRITE(*,5941)  
     &     B01ND, B12ND, B23ND, B34ND,
     &     BR1FX, BR2FX, BR3FX, BR4FX, 
     &     LAXS, SLASS, LLIFA, LPEFR, LNSLP, NODWT, NODLT
          
     
 5941 FORMAT (12X,'0. End of changes ',//,
     3  12X,' 1. B01ND ( 10.0 -  100.0).......................[',F6.0,/,
     2  12X,' 2. B12ND ( 10.0 -  100.0).......................[',F6.0,/,
     2  12X,' 3. B23ND ( 10.0 -  100.0).......................[',F6.0,/,
     2  12X,' 4. B34ND ( 10.0 -  100.0).......................[',F6.0,/,
     4  12X,' 5. BR1FX ( 00.0 -   10.0).......................[',F7.3,/,
     5  12X,' 6. BR2FX ( 00.0 -   10.0).......................[',F7.3,/,
     6  12X,' 7. BR3FX ( 00.0 -   10.0).......................[',F7.3,/,
     7  12X,' 8. BR4FX ( 00.0 -   10.0).......................[',F7.3,/,
     8  12X,' 9. LAXS  ( 1000 -   2000).......................[',F7.1,/,
     9  12X,'10. SLAS  (100.0 -  400.0).......................[',F7.2,/,
     1  12X,'11. LLIFA (300.0 - 1200.0).......................[',F7.1,/,  !LPM redefine limit values
     2  12X,'12. LPEFR (0.250 -  0.400).......................[',F7.2,/,
     3  12X,'13. LNSLP (0.60 -    1.60).......................[',F7.2,/,
     4  12X,'14. NODWT (3.00 -    7.00).......................[',F7.2,/,
     5  12X,'15. NODLT (1.00 -    4.00).......................[',F7.1,/)
 
          WRITE (*,5100)
C
C         Get menu choice
          READ  (5,'(I2)',IOSTAT=IERR) IPARAM
          CALL SELPRO (0,22,IPARAM,NDEX,IERR)
          IF (NDEX .EQ. 2) GOTO 3001
C
C         Branch to menu choice
          SELECT CASE(IPARAM)
          CASE(0);  RETURN
          CASE( 1);  CALL GETREAL (B01ND ,'B01ND ', 10.0, 100.0)
          CASE( 2);  CALL GETREAL (B12ND ,'B12ND ', 10.0, 100.0)
          CASE( 3);  CALL GETREAL (B23ND ,'B23ND ', 10.0, 100.0)
          CASE( 4);  CALL GETREAL (B34ND ,'B34ND ', 10.0, 100.0)
          CASE( 5);  CALL GETREAL (BR1FX ,'BR1FX ', 0.0, 10.0)
          CASE( 6);  CALL GETREAL (BR2FX ,'BR2FX ', 0.0, 10.0)
          CASE( 7);  CALL GETREAL (BR3FX ,'BR3FX ', 0.0, 10.0)
          CASE( 8);  CALL GETREAL (BR4FX ,'BR4FX ', 0.0, 10.0)
          CASE( 9);  CALL GETREAL (LAXS  ,'LAXS',1000.,2000.0)
          CASE(10);  CALL GETREAL (SLASS ,'SLAS ',100.0, 400.0)
          CASE(11);  CALL GETREAL (LLIFA ,'LLIFA ',300.0,1200.0) 
          CASE(12);  CALL GETREAL (LPEFR ,'LPEFR ',0.200, 0.400)
          CASE(13);  CALL GETREAL (LNSLP ,'LNSLP ', 0.60, 1.60)
          CASE(14);  CALL GETREAL (NODWT ,'NODWT ', 3.00, 7.00)
          CASE(15);  CALL GETREAL (NODLT ,'NODLT ', 1.00, 4.00) 
        END SELECT

    
!=======================================================================
!     CANEGRO sugarcane model
      CASE ('SCCAN')
        WRITE (*,5900) 
     &      MaxPARCE, APFMX, STKPFMAX, SUCA, TBFT,  
     &      LFMAX, MXLFAREA, MXLFARNO, PI1, PI2, PSWITCH, TTPLNTEM, 
     &      TTRATNEM, CHUPIBASE, TT_POPGROWTH, POPTT16, 
     &      TAR0, TDELAY, LER0, SER0, LG_AMBASE, AQP_UP5 

5900    FORMAT (12X,'0. End of changes',//,
     &  12X,' 1. MaxPARCE (Max radiation conversion eff).....[',F7.2,/,
     &  12X,' 2. APFMX (Max fraction to aerial DM)...........[',F7.1,/,
     &  12X,' 3. STKPFMAX (Frac aerial DM to stalk)..........[',F7.1,/,
     &  12X,' 4. SUCA (Max sucrose in stalk base)............[',F7.1,/,
     &  12X,' 5. TBFT (Temp for 50% partitioning to sucrose).[',F7.1,/,
     &  12X,' 6. LFMAX (Max number leaves)...................[',F7.1,/,
     &  12X,' 7. MXLFAREA (Max leaf area above MXLFARNO).....[',F7.1,/,
     &  12X,' 8. MXLFARNO (Leaf # for MXLFAREA)..............[',F7.1,/,
     &  12X,' 9. PI1 (Phyllocron interal 1)..................[',F7.1,/,
     &  12X,'10. PI2 (Phyllocron interal 2)..................[',F7.1,/,
     &  12X,'11. PSWITCH (Leaf #, switch between PI1 & PI2)..[',F7.1,/,
     &  12X,'12. TTPLNTEM (Thermal time to emergence, plant).[',F7.1,/,
     &  12X,'13. TTRATNEM (Thermal time to emergence, ratoon)[',F7.1,/,
     &  12X,'14. CHUPIBASE (Thermal time emerg to stalk gr)..[',F7.1,/,
     &  12X,'15. TT_POPGROWTH (Thermal time to peak tiller)..[',F7.1,/,
     &  12X,'16. POPTT16 (Stalk population, 1600 dd).........[',F7.1,/,
     &  12X,'17. TAR0 (Tiller apprnc rate, t/stalk/dd).......[',F7.1,/,
     &  12X,'18. TDELAY (Delay from p shoot to 1st tiller)...[',F7.1,/,
     &  12X,'19. LER0 (Leaf elong. rate, cm/dd)..............[',F7.1,/,
     &  12X,'20. SER0 (Stalk elong. rate, cm/dd).............[',F7.1,/,
     &  12X,'21. LG_AMBASE (Aerial mass for lodging).........[',F7.1,/,
     &  12X,'22. AQP_UP5 (Soil water stress deplet., 0-1)....[',F7.1,/)

           WRITE (*,5100)
C
C          Get menu choice
           READ  (5,'(I2)',IOSTAT=IERR) IPARAM
           CALL SELPRO (0,20,IPARAM,NDEX,IERR)
           IF (NDEX .EQ. 2) GOTO 3001
C
C          Branch to menu choice
           SELECT CASE (IPARAM)
             CASE (0)
                RETURN
             CASE (1); CALL GETREAL (MaxPARCE,'MaxPARCE', 0.0, 100.)
             CASE (2); CALL GETREAL (APFMX,   'APFMX',    0.0, 1.)
             CASE (3); CALL GETREAL (STKPFMAX,'STKPFMAX', 0.0, 1.)
             CASE (4); CALL GETREAL (SUCA,    'SUCA',     0.0, 1.)
             CASE (5); CALL GETREAL (TBFT,    'TBFT',     0.0, 45.)
             CASE (6); CALL GETREAL (LFMAX,   'LFMAX',    0.0, 10000.)
             CASE (7); CALL GETREAL (MXLFAREA,'MXLFAREA', 0.0, 10000.)
             CASE (8); CALL GETREAL (MXLFARNO,'MXLFARNO', 0.0, 10000.)
             CASE (9); CALL GETREAL (PI1,     'PI1',      0.0, 10000.)
             CASE(10); CALL GETREAL (PI2,     'PI2',      0.0, 10000.)
             CASE(11); CALL GETREAL (PSWITCH, 'PSWITCH',  0.0, 10000.)
             CASE(12); CALL GETREAL (TTPLNTEM,'TTPLNTEM', 0.0, 10000.)
             CASE(13); CALL GETREAL (TTRATNEM,'TTRATNEM', 0.0, 10000.)
             CASE(14); CALL GETREAL (CHUPIBASE,'CHUPIBASE',0.0, 10000.)
             CASE(15); CALL GETREAL (TT_POPGROWTH,'TT_POPGROWTH',0.0,
     &                                                         10000.)
             CASE(16); CALL GETREAL (POPTT16, 'POPTT16',  0.0, 10000.)
             CASE(17); CALL GETREAL (TAR0,'TAR0',0.0,10000.)
             CASE(18); CALL GETREAL (TDELAY,'TDELAY',0.0,10000.)
             CASE(19); CALL GETREAL (LER0,'LER0',0.0,10000.)
             CASE(20); CALL GETREAL (SER0,'SER0',0.0,10000.)
             CASE(21); CALL GETREAL (LG_AMBASE,'LG_AMBASE',0.0,10000.)
             CASE(22); CALL GETREAL (AQP_UP5,'AQP_UP5',0.0,10000.)
           END SELECT 

!=======================================================================
!     CASUPRO sugarcane model
      CASE ('SCCSP')
        WRITE (*,5950) 
     &          LFMAX,PHTMAX,StkH2OFac,SuH2OFac,PLF1,
     &          PLF2,Gamma,StkB,StkM,
     &          SIZLF,LIsun,LIshd,TB(1),TO1(1),TO2(1),TM(1),
     &          PI1,PI2,DTPI,LSFAC,LI1,TELOM,TB(2),TO1(2),
     &          TO2(2),TM(2),Ph1P,Ph1R,Ph2,Ph3,Ph4,StkHrNO,RTNFAC,
     &          MinGr,RES30C,RLF30C,R30C2

5950    FORMAT (12X,'0. End of changes',//,
     &  12X,'1. LFMAX (Max leaf photosynthesis rate)..........[',F7.1,/,
     &  12X,'2. PHTMAX(Max CH20 which can be produced)........[',F7.1,/,
     &  12X,'3. StkH2OFac(Stalk fresh weight factor for DM)...[',F7.1,/,
     &  12X,'4. SuH2OFac (Stalk fresh wt factor for sucrose)..[',F7.1,/,
     &  12X,'5. PLF1  (Max young stk portion CH2O to leaves)..[',F7.1,/,
     &  12X,'6. PLF2  (Max old stalk portion CH2O to leaves)..[',F7.1,/,
     &  12X,'7. Gamma (fraction excess CH2O to stalk)......  .[',F7.1,/,
     &  12X,'8. StkB  (Intercept for rate stalk wt increase)..[',F7.1,/,
     &  12X,'9. StkM  (Slope for rate of stalk wt increase)...[',F7.1,/,
     &  12X,'10.SIZLF (Maximum size of largest leaf)..........[',F7.1,/,
     &  12X,'11.LIsun (PAR intercepted by uppermost canopy)...[',F7.1,/,
     &  12X,'12.LIshd (PAR intercepted by top & mid canopy)...[',F7.1,/,
     &  12X,'13.TB(1) (Base temp for leaf primary stalk emerg)[',F7.1,/,
     &  12X,'14.TO1(1)(Lower optimum temp, leaf development)..[',F7.1,/,
     &  12X,'15.TO2(1)(Upper optimum temp, leaf development)..[',F7.1,/,
     &  12X,'16.TM(1) (Maximum temperature, leaf development).[',F7.1,/,
     &  12X,'17.PI1   (Phyllocron interval 1).................[',F7.1,/,
     &  12X,'18.PI2   (Phyllocron interval 2).................[',F7.1,/,
     &  12X,'19.DTPI  (Therm time for change from PT1 to PT2).[',F7.1,/,
     &  12X,'20.LSFAC (Ratio leaf sheath DM to leaf blade DM).[',F7.1,/,
     &  12X,'21.LI1   (Light intercep for no tiller comp).....[',F7.1,/,
     &  12X,'22.TELOM (Threshold to tiller emergence).........[',F7.1,/,
     &  12X,'23.TB(2) (Base temperature for tiller appearance)[',F7.1,/,
     &  12X,'24.TO1(2)(Lower optimum temp, tiller development)[',F7.1,/,
     &  12X,'25.TO2(2)(Upper optimum temp, tiller development)[',F7.1,/,
     &  12X,'26.TM(2) (Max temperature, tiller development)...[',F7.1,/,
     &  12X,'27.Ph1P  (Threshold to sprouting, plant cane)....[',F7.1,/,
     &  12X,'28.Ph1R  (Threshold to sprouting, ratoon cane)...[',F7.1,/,
     &  12X,'29.Ph2   (Growth rate of primary stalk)..........[',F7.1,/,
     &  12X,'30.Ph3   (Threshold for growth of primary stalk).[',F7.1,/,
     &  12X,'31.Ph4   (Threshold  for tillering)..............[',F7.1,/,
     &  12X,'32.StkHrNO(# mature stalks/stool, recent harvest)[',F7.1,/,
     &  12X,'33.RTNFAC(# primary shoots/mature stalk )........[',F7.1,/,
     &  12X,'34.MinGr (Threshold wt for tiller senesce).......[',F7.1,/,
     &  12X,'35.RES30C(Respiration coef for stalk & root).....[',F7.1,/,
     &  12X,'36.RLF30C(Respiration coef for leaf).............[',F7.1,/,
     &  12X,'37.R30C2 (Respiration coef for photosynthesis)...[',F7.1,/)

           WRITE (*,5100)

C          Get menu choice
           READ  (5,'(I2)',IOSTAT=IERR) IPARAM
           CALL SELPRO (0,36,IPARAM,NDEX,IERR)
           IF (NDEX .EQ. 2) GOTO 3001
C
C          Branch to menu choice
           SELECT CASE (IPARAM)
             CASE (0); RETURN
             CASE (1); CALL GETREAL (LFMAX    ,'LFMAX    ', 0.5, 1.5)
             CASE (2); CALL GETREAL (PHTMAX   ,'PHTMAX   ', 100.,400.)
             CASE (3); CALL GETREAL (StkH2OFac,'StkH2OFac', 2.0, 7.0)
             CASE (4); CALL GETREAL (SuH2OFac ,'SuH2OFac ', 1.0, 4.0)
             CASE (5); CALL GETREAL (PLF1     ,'PLF1     ', 0.1, 0.5)
             CASE (6); CALL GETREAL (PLF2     ,'PLF2     ', 0.1, 0.4)
             CASE (7); CALL GETREAL (Gamma    ,'Gamma    ', 0.0, 1.0)
             CASE (8); CALL GETREAL (StkB     ,'StkB     ', 0.3, 1.3)
             CASE (9); CALL GETREAL (StkM     ,'StkM     ', 0.0, 2.0)
             CASE(10); CALL GETREAL (SIZLF    ,'SIZLF    ', 300., 400.)
             CASE(11); CALL GETREAL (LIsun    ,'LIsun    ', 0.0, 1.0)
             CASE(12); CALL GETREAL (LIshd    ,'LIshd    ', 0.0, 1.0)
             CASE(13); CALL GETREAL (TB(1)    ,'TB(1)    ',5.0, 20.)
             CASE(14); CALL GETREAL (TO1(1)   ,'TO1(1)   ', 15.0, 40.)
             CASE(15); CALL GETREAL (TO2(1)   ,'TO2(1)   ', 20., 40.)
             CASE(16); CALL GETREAL (TM(1)    ,'TM(1)    ', 20., 50.)
             CASE(17); CALL GETREAL (PI1      ,'PI1      ', 60.0, 120.)
             CASE(18); CALL GETREAL (PI2      ,'PI2      ', 60.0, 140.)
             CASE(19); CALL GETREAL (DTPI     ,'DTPI     ', 800., 1600.)
             CASE(20); CALL GETREAL (LSFAC    ,'LSFAC    ', 0.25, 0.75)
             CASE(21); CALL GETREAL (LI1      ,'LI1      ', 0.0, 1.0)
             CASE(22); CALL GETREAL (TELOM    ,'TELOM    ', 100., 300.)
             CASE(23); CALL GETREAL (TB(2)    ,'TB(2)    ',  5.0, 24.)
             CASE(24); CALL GETREAL (TO1(2)   ,'TO1(2)   ', 15.0, 40.)
             CASE(25); CALL GETREAL (TO2(2)   ,'TO2(2)   ', 20.0, 40.)
             CASE(26); CALL GETREAL (TM(2)    ,'TM(2)    ', 20.0, 50.)
             CASE(27); CALL GETREAL (Ph1P     ,'Ph1P     ', 50., 500.)
             CASE(28); CALL GETREAL (Ph1R     ,'Ph1R     ', 50., 500.)
             CASE(29); CALL GETREAL (Ph2      ,'Ph2      ', 0.5, 3.0)
             CASE(30); CALL GETREAL (Ph3      ,'Ph3      ', 1000.,2000.)
             CASE(31); CALL GETREAL (Ph4      ,'Ph4      ', 5000.,8000.)
             CASE(32); CALL GETREAL (StkHrNO  ,'StkHrNO  ', 1.0, 20.0)
             CASE(33); CALL GETREAL (RTNFAC   ,'RTNFAC   ', 0.5, 4.0)
             CASE(34); CALL GETREAL (MinGr    ,'MinGr    ', 40.,100.)
             CASE(35); CALL GETREAL (RES30C   ,'RES30C   ', 0.1, 0.6)
             CASE(36); CALL GETREAL (RLF30C   ,'RLF30C   ', 0.5, 3.0)
             CASE(37); CALL GETREAL (R30C2    ,'R30C2    ', 3.0, 5.0)
           END SELECT                                 
                                                      
!=======================================================================
      CASE ('TRARO', 'TNARO')
        WRITE (*,6200) P1,P3,P4,P5,G2,G3,G4,PHINT,PCINT,PCGRD
6200    FORMAT (12X,'0. End of changes',//,
     1  12X,'1. P1 ..........................................[',F7.1,/,
     3  12X,'2. P3 ..........................................[',F7.2,/,
     4  12X,'3. P4 ..........................................[',F7.1,/,
     6  12X,'4. P5 ..........................................[',F7.2,/,
     6  12X,'5. G2 ..........................................[',F7.2,/,
     7  12X,'6. G3 ..........................................[',F7.3,/,
     8  12X,'7. G4 ..........................................[',F7.3,/,
     9  12X,'8. PHINT .......................................[',F7.3,/,
     1  12X,'9. PCINT .......................................[',F7.3,/,
     2  12X,'10.PCGRD .......................................[',F7.3,/)

         WRITE (*,5100)
C
C        Get menu choice
         READ  (5,'(I2)',IOSTAT=IERR) IPARAM
         CALL SELPRO (0,10,IPARAM,NDEX,IERR)
         IF (NDEX .EQ. 2) GOTO 3001
C
C        Branch to menu choice
         SELECT CASE (IPARAM)
         CASE (0); RETURN
         CASE (1); CALL GETREAL (P1,   'P1   ', 0.0, 4000.0)
         CASE (2); CALL GETREAL (P3,   'P3   ', 1.0, 4000.0)
         CASE (3); CALL GETREAL (P4,   'P4   ', 1.0, 4000.0)
         CASE (4); CALL GETREAL (P5,   'P5   ', 1.0, 4000.0)
         CASE (5); CALL GETREAL (G2,   'G2   ', 1.0, 7.0)
         CASE (6); CALL GETREAL (G3,   'G3   ', 1.0, 7.0)
         CASE (7); CALL GETREAL (G4,   'G4   ', 1.0, 7.0)
         CASE (8); CALL GETREAL (PHINT,'PHINT', 1.0, 2000.0)
         CASE (9); CALL GETREAL (PCINT,'PCINT', 1.0, 2000.0)
         CASE (10);CALL GETREAL (PCGRD,'PCGRD', 1.0, 2000.0)
         END SELECT                                  

      END SELECT
      GOTO 3001

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

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
      EXTERNAL CLEAR, GETREAL, SELPRO
      INCLUDE 'COMGEN.blk'

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
      EXTERNAL FSELPRO

      CHARACTER*(*) VNAME
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

  200 FORMAT (/,' The current value of ',A, ' is ',F10.3)
  300 FORMAT (/,' Input new value : ',$)

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
         WRITE (*,20) FLOW, FHIGH
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
   20 FORMAT (2X,"Minimum: ",F12.5,10X,"Maximum: ",F12.5)

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
         WRITE (*,20) VLO, VHI
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
   20 FORMAT (2X,"Minimum: ",I12,10X,"Maximum: ",I12)
  200 FORMAT (/,' The current value of ',A5, ' is ',I8)
  300 FORMAT (/,' Input new value : ',$)

      END SUBROUTINE GETINT

C=======================================================================
