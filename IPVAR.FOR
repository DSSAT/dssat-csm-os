C=======================================================================
C  IPVAR, Subroutine
C
C  Reads in genetic information for crop
C-----------------------------------------------------------------------
C  Revision       History
C  01/01/1990 GH  Written
C  05/28/1993 PWW Header revision and minor changes  
C  12/14/2000 GH  Version correction
C  12/14/2005 CHP/PST Added new sorghum cultivar coefficients (optional)
C  02/06/2007 CHP Added alternate sugarcane parameters for CASUPRO
C  04/21/2007 GH  Added P3 and P4 coefficients for sorghum 
C  09/16/2007 JIL New inputs for IXIM
!  11/08/2007 CHP Added 6X field for quality (= number of experiments used
!                 in estimation of parameters).  This field is read here,
!                 but not written to INP or INH files.
C  11/26/2007 CHP THRESH, SDPRO, SDLIP moved from eco to cul file
C  01/16/2008 GH  Added P2 and PANTH coefficients for sorghum
C  08/03/2009 FSR Added numerous variables for CASUPRO
C-----------------------------------------------------------------------
C  INPUT  : FILEG,NSENS,VARNO,VARTY,VRNAME,PATHGE,ECONO
C
C  LOCAL  : LINE,BLANK,ANS,ERRKEY,C360,FILEGG,ERRNUM,LINVAR,LUNVAR,I,ISECT,
C           PATHL,NLOOP,NLVAR,FLAG,VAR
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SEVAR SENS INPUT
C
C  Calls  : ERROR CLEAR IGNORE VERIFY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================
      SUBROUTINE IPVAR (FILEG,NSENS,RNMODE,VARNO,VARTY,VRNAME,
     &                  PATHGE,ECONO, MODEL, ATLINE)

      IMPLICIT NONE

      INCLUDE 'COMGEN.BLK'

      CHARACTER*1   LINE(80),RNMODE,BLANK,ANS
      CHARACTER*6   VARTY,VARNO,ERRKEY,ECONO
      CHARACTER*8   MODEL
      CHARACTER*12  FILEG
      CHARACTER*16  VRNAME
      CHARACTER*80  PATHGE
      CHARACTER*92  FILEGG
      CHARACTER*360 C360,ATLINE

      INTEGER       I,NSENS,NLVAR,LUNVAR,LINVAR,ISECT,NLOOP
      INTEGER       ERRNUM,PATHL
      REAL          FLAG,VAR

      PARAMETER (LUNVAR = 19)
      PARAMETER (ERRKEY = 'IPVAR ')
      PARAMETER (BLANK  = ' ')

      DATA NLVAR /0/

      PATHL  = INDEX (PATHGE,BLANK)

      IF (PATHL .LE. 1) THEN
         FILEGG = FILEG
       ELSE
         FILEGG = PATHGE(1:(PATHL-1)) // FILEG
      ENDIF
C-----------------------------------------------------------------------
C    Read Cultivar Specific Genetics/Cultivar Parameter File
C-----------------------------------------------------------------------
      OPEN (LUNVAR,FILE = FILEGG,STATUS = 'OLD',IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEG,0)
      IF (NSENS .EQ. 1) THEN
         I  = 1
         NLOOP = 0
         IF (INDEX('IE',RNMODE) .GT. 0) THEN
            CALL CLEAR
            WRITE (*,100)
         ENDIF
  200    CONTINUE
         CALL IGNORE (LUNVAR,LINVAR,ISECT,C360)
         IF (ISECT .EQ. 0) GO TO 211
         IF (ISECT .EQ. 2) GO TO 200
         IF (C360(1:1) .EQ. ' ' .OR. C360(1:1) .EQ. '*' 
     &   .OR. C360(1:1) .EQ. '$') GO TO 200
         READ (C360, 110, IOSTAT=ERRNUM) VARTY,VRNAME,ECONO
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEG,LINVAR)
         IF (INDEX('IE',RNMODE) .GT. 0) THEN
            WRITE (*,120) I,VARTY,VRNAME,ECONO,ECONO(3:4)
         ENDIF
         IF (VARTY .EQ. VARNO) NLVAR = I
C
C        Write Pause Statement Every 15 lines
C
         IF (MOD(I,15) .EQ. 0 .AND. INDEX('IE',RNMODE) .GT. 0) THEN
            WRITE (*,300)
            READ  (5,'(A1)') ANS
         ENDIF
         I  = I + 1
         GOTO 200

  211    CONTINUE
         NLOOP = NLOOP + 1
         IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,FILEG,LINVAR)
         LINE(1) = ' '
         IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,350) NLVAR
         READ (5, 400) LINE
         CALL VERIFY (LINE,VAR,FLAG)
         IF (VAR .LE. 0) THEN
            VAR = NLVAR
          ELSE IF ((FLAG .GT. 0) .OR. (VAR .GT. (I-1))) THEN
            WRITE (*,500) I -1
            GO TO 211
          ELSE IF (VAR .NE. NINT(VAR)) THEN
            WRITE (*,510)
            GO TO 211
          ELSE IF (VAR .GT. 0) THEN
            NLVAR = NINT(VAR)
          ELSE
            GO TO 211
         ENDIF

         REWIND (LUNVAR)
      ENDIF

      I = 0

 2010 CONTINUE
      I = I + 1
 2000 CONTINUE
      CALL IGNORE (LUNVAR, LINVAR, ISECT, C360)
      IF (ISECT .EQ. 0) CALL ERROR (ERRKEY,2,FILEG,LINVAR)
      IF (ISECT .EQ. 2) GO TO 2000
      IF (C360(1:1) .EQ. ' ' .OR. C360(1:1) .EQ. '*') GO TO 2000

!     ------------------------------------------------------------------
      SELECT CASE (MODEL(1:5))

!     ** Formats now include space (6X) for quality parameter between 
!     variety name and ecotype name

!     CROPGRO crops **
      CASE ('CRGRO')  
        READ (C360, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,CSDVAR,
     &       PPSEN,PH2T5,PHTHRS(6),PHTHRS(8),PHTHRS(10),PHTHRS(13),
     &       LFMAX,SLAVAR,SIZELF,XFRUIT,WTPSD,SFDUR,SDPDVR,PODUR,
     &       THRESH, SDPRO, SDLIP
        IF (LFMAX  .LE. 0) CALL ERROR (ERRKEY,22,FILEG,LINVAR)
        IF (SLAVAR .LE. 0) CALL ERROR (ERRKEY,23,FILEG,LINVAR)
        IF (SIZELF .LE. 0) CALL ERROR (ERRKEY,23,FILEG,LINVAR)
        IF (XFRUIT .LE. 0) CALL ERROR (ERRKEY,24,FILEG,LINVAR)
        IF (WTPSD  .LE. 0) CALL ERROR (ERRKEY,25,FILEG,LINVAR)
        IF (SDPDVR .LE. 0) CALL ERROR (ERRKEY,26,FILEG,LINVAR)
        IF (SFDUR  .LE. 0) CALL ERROR (ERRKEY,27,FILEG,LINVAR)
        IF (PODUR  .LE. 0) CALL ERROR (ERRKEY,28,FILEG,LINVAR)
        IF (THRESH .LE. 0) CALL ERROR (ERRKEY,50,FILEG,LINVAR)
!        IF (SDPRO  .LE. 0) CALL ERROR (ERRKEY,51,FILEG,LINVAR)
!        IF (SDLIP  .LE. 0) CALL ERROR (ERRKEY,52,FILEG,LINVAR)

!     CropSim: cassava, wheat **
      CASE ('CSCRP')
!       For now, CropSim will just have a text string for cultivar info
        READ (C360,820,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO, PLAINTXT

!        IF (INDEX('WH',CROP) > 0) THEN
!          READ (C360,800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO, 
!     &      VNEED, VNMIN, VNODR, PPS1, PPS2, 
!     &      P1, P2, P3, P4, P5, P6, P7, P8, 
!     &      GRNOW, GRWTX, SHWMS, PHINT
!
!        ELSEIF (INDEX('CS',CROP) > 0) THEN
!          READ (C360,800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO, 
!     &      PPS1, P1L, P2L, P4L, P5L, SRNOW, SRFR, LA1S, LAXS, 
!     &      LAXNO, LAFS, LAFNO, LAWS, PHINT, LLIFA, STFR
!        ENDIF

!     Ceres-wheat: wheat, barley **
      CASE ('CSCER')
        READ (C360, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1V,P1D,P5,G1,G2,G3,PHINT

!     Ceres Maize: maize, sweet corn **
      CASE ('MZCER','SWCER')
        READ (C360,'(A6,1X,A16,7X,A6,6F6.0)',IOSTAT=ERRNUM) 
     &            VARTY,VRNAME,ECONO,P1,P2,P5,G2,G3,PHINT

!     Ixim maize **
      CASE ('MZIXM')
        READ (C360, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1,P2,P5,G2,G3,PHINT,AX,LX
C ** Use default values if inputs not available
        LFN = ((1.4*P1)/(PHINT*0.5))+(10.55-0.0216*P1)
	IF (AX .EQ. 0.0) THEN
	  AX = 1000.0*EXP(-1.17 + (0.047*LFN))  !From Birch et al, 1998
	ENDIF
        IF (LX .EQ. 0.0) THEN
	  LX = 1.1138 * AX                      !From regression, JIL 
	ENDIF

!     Ceres Sorghum **
      CASE ('SGCER')
        READ (C360, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1,P2O,P2R,P5,G1,G2,PHINT,P3,P4,P2,PANTH
C-GH &            P1,P2O,P2R,P5,G1,G2,PHINT,P3,P4
!           Read optional PBASE and PSAT parameters
!           If these parameters are non-zero, then optional 
!             method is used
!***        NOTE: these new sorghum parameters have not been added to 
!             sensitivity analysis yet.  chp 10/29/2004
     &            ,PBASE, PSAT
!       Optional cultivar parameters
        IF (PBASE < 1.E-2) PBASE = -99.
        IF (PSAT  < 1.E-2) PSAT  = -99.

!     Ceres Millet **
      CASE ('MLCER')
        READ (C360, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1,P2O,P2R,P5,G1,G4,PHINT

!     Substor Potato **
      CASE ('PTSUB')
        READ (C360, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            G2,G3,G4,PD,P2,TC

!     Ceres Rice **
      CASE ('RICER')
        READ (C360,800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1,P2R,P5,P2O,G1,G2,G3,G4

!     CaneGro: South African Sugarcane model **
      CASE ('SCCAN')
        READ (C360,1500,IOSTAT=ERRNUM) VARTY, VRNAME, PLAINTXT

!     This is another change

!     Casupro: Florida-Colombia Sugarcane model **
      CASE ('SCCSP')
!!!     READ (C255,1050,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
!!!  &          PI1,PI2,DTPI,SMAX,SO,GMAX,GO,M1,M2,LA,WTPSD,SLAVR,SIZLF 

        READ (C360,1055,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &          LFMAX,PHTMAX,StkH2OFac,SuH2OFac,empty,PLF,
     &          Gamma,StkB,StkM,empty,empty,
     &          SIZLF,LIsun,LIshd,empty,TB(1),TO1(1),TO2(1),TM(1),
     &          PI1,PI2,DTPI,LSFAC,empty,LI1,TELOM,TB(2),TO1(2),
     &          TO2(2),TM(2),Ph1P,Ph1R,Ph2,Ph3,Ph4,StkHrNO,RTNFAC,
     &          MinKg,empty,RES30C,RLF30C,R30C2,empty,empty 

!     Sunflower **
      CASE ('SUOIL')
        READ (C360,800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &           P1,P2,P5,G2,G3,O1

!     Pineapple **
      CASE ('PIALO')
        READ (C360,800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &           P2,P3,P4,G2,G3,PHINT

!     Taro, tanier **
      CASE ('TRARO','TNARO')
        READ (C360,800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &           P1,P3,P4,P5,G3,G4,PHINT,PCINT,PCGRD

      END SELECT

      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEG,LINVAR)
      IF (((ADJUSTL(VARTY) .NE. ADJUSTL(VARNO)) .AND. (NSENS .EQ. 0)) 
     &       .OR. ((I .LT. NLVAR) .AND. (NSENS .EQ. 1))) GO TO 2010

      VARNO = VARTY
      CLOSE (LUNVAR)

      ! Added LAH Jan 1 2008
      OPEN (LUNVAR,FILE = FILEGG,STATUS = 'OLD',IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEG,0)
      DO I = 1, 100
        READ (LUNVAR,'(A)') C360
        IF (C360(1:1).EQ.'@') THEN
!         Skip the EXPNO column (CHP 1/29/2009)
          ATLINE = C360(1:23) // C360(31:360)
          EXIT
        ENDIF
      ENDDO
          
      CLOSE (LUNVAR)

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

  100 FORMAT (T30,'VARIETY SELECTION',/,T30,'=================',
     &     //,T43,'ECOTYPE',2X,'MATURITY',
     &      /,2X,'NO.',1X,'ENTRY',3X,'VARIETY',22X,'GROUP',5X,'GROUP',
     &      /,2X,'---',1X,'------',2X,20('-'),8X,'-------',2X,
     &           '--------')
  110 FORMAT (A6,1X,A16,1X,A6,6X,A2)
  120 FORMAT (I4,') ',A6,2X,A16,13X,A6,6X,A2)
  300 FORMAT (/,'  More.... press < ENTER > key')
  350 FORMAT (/,6X,'VARIETY SELECTED ===>',1X,I4,
     &        /,6X,'NEW SELECTION ?  --->',3X,' ',$)
  400 FORMAT (80A1)
  500 FORMAT (6X,'ERROR! Variety Selection must be between 1 & ',I3,/)
  510 FORMAT (6X,'ERROR! Variety Selection must be an INTEGER value',/)

! 800 FORMAT (A6,1X,A16,1X,A6,15(F6.0))
  800 FORMAT (A6,1X,A16,7X,A6,18F6.0)                     !11/8/07

! 820 FORMAT (A6,1X,A16,1X,A6,1X,A)
  820 FORMAT (A6,1X,A16,7X,A6,A)                       !11/8/07

! 1050 FORMAT (A6,1X,A16,7X,A6,9F6.0,1X,I5,3F6.0)          !11/8/07
 1055 FORMAT (A6,1X,A16,7X,A6,44F6.0)                   !02/10/2009 
!!! 1055 FORMAT (A6,1X,A16,7X,A6,37F6.0,G8.0,4F6.1)       !02/10/2009 

!1500 FORMAT (A6,1X,A16,1X,A255)
 1500 FORMAT (A6,1X,A16,7X,A)                             !11/8/07

      END SUBROUTINE IPVAR
