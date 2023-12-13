
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
C  06/30/2010 FSR Added PLF2 variable for CASUPRO
C  05/19/2011 GH  Updated for sorghum
C  02/25/2012 JZW add the PHINT data reading from *.cul for RICER
C  08/09/2012 GH  Updated for cassava
!  04/16/2013 CHP/KAD Added SALUS model
!  05/09/2013 CHP/FR/JZW Added N-wheat module
!  01/21/2020 JG moved some CUL parameters to ECO file
!  10/20/2020 FV added SUOIL (OilcropSun)
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

!     2023-01-26 chp removed unused variables in argument list:
!       CROP

      IMPLICIT NONE
      EXTERNAL CLEAR, ERROR, IGNORE, VERIFY, WARNING

      INCLUDE 'COMGEN.blk'

      CHARACTER*1   LINE(80),RNMODE,BLANK,ANS
!     CHARACTER*2   CROP
      CHARACTER*6   VARTY,VARNO,ERRKEY,ECONO
      CHARACTER*8   MODEL
      CHARACTER*12  FILEG
      CHARACTER*16  VRNAME
      CHARACTER*78  MSG(3)
      CHARACTER*80  PATHGE
      CHARACTER*92  FILEGG
      CHARACTER*1000 C360,ATLINE

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
     &     .OR. C360(1:1) .EQ. '$') GO TO 200
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

!     2019-02-12
!     CHP moved this check before the select case. This effectively disables
!     the cultivar parameter sensitivity, but since we have the sensitivity
!     analysis tool in the DSSAT shell, it should be OK.
      READ (C360, 800, IOSTAT=ERRNUM) VARTY
      IF (ADJUSTL(VARTY) .NE. ADJUSTL(VARNO)) GO TO 2010

!     ------------------------------------------------------------------
      SELECT CASE (MODEL(1:5))

!     ** Formats now include space (6X) for quality parameter between
!     variety name and ecotype name

!     Generic SALUS simple crop models
!     09/21/2009 CHP/KAD
      CASE ('SALUS')
        READ (C360,'(6X,A6,1X,A16,7X,A)',IOSTAT=ERRNUM) VARTY,
     &         VRNAME, PLAINTXT

!     CROPGRO crops **
      CASE ('CRGRO','PRFRM')
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

C-GH Remove cassava
!     CropSim: wheat, barley
      CASE ('CSCRP')
        READ (C360,810,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO, 
     &      P1, P2, P3, P4, P5, P6, P7, P8, VREQ, VBASE, VEFF,
     &      PPS1, PPS2, PHINT, LA1S, LAFV, LAFR, SHWTS, GNOWT, GWTS,
     &      PLAINTXT                
     
!WHEAT & BARLEY
!@VAR#  VAR-NAME........  EXP#   ECO#  VREQ  PPS1    P8 G#WTS  GWTS SHWTS PHINT    P1    P2    P3    P4    P5    P6    P7  LA1S  LAFV  LAFR VBASE  VEFF  PPS2
!DFAULT DEFAULTS             . DFAULT     0     0   500    25    40   2.5    80   380    70   200   200    60    25   150   3.0   0.1   0.5     0     0     0

C-GH Tony update February, 2014
!     &      VREQ, PPS1, P8, GNOWT, GWTS, SHWTS, PHINT,
!     &      P1, P2, P3, P4, P5, P6, P7,
!     &      LA1S, LAFV, LAFR, VBASE, VEFF, PPS2, PLAINTXT

C-GH  Add cassava model
!     CASSAVA: cassava **
      CASE ('CSCAS')
        READ (C360,820,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO, 
     &      PPS1, B01ND, B12ND, B23ND, B34ND, B45ND, B56ND,
C-GH &      SNFX, SRNWT, SRFR, HMPC, PHINT, LA1S, LAXS, LAXND, LAXN2,
     &      SRNWT, SRFR, HMPC, PHINT, LA1S, LAXS, LAXND, LAXN2,
     &      LAFS, LAFND, SLASS, LLIFA, LPEFR, STFR, PLAINTXT

C-LPM  Add CIAT cassava model
!     CASSAVA: cassava **
      CASE ('CSYCA')
          READ (C360,821,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO, 
     &      B01ND, B12ND, B23ND, B34ND, BR1FX, BR2FX, BR3FX, BR4FX, 
     &      LAXS, SLASS, LLIFA, LPEFR, LNSLP, NODWT, NODLT 

!     Ceres-wheat: wheat, barley **
      CASE ('CSCER')
!       READ (C360, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
!     &            P1V,P1D,P5,G1,G2,G3,PHINT, PLAINTXT
!       READ (C360,820,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO, PLAINTXT
        READ (C360, 830,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1V,P1D,P5,G1,G2,G3,PHINT, PLAINTXT

!     APSIM-NWheat wheat  **
!     Tef model based on APSIM-NWheat created by KEP **
!     JG moved CUL parameters to ECO file 01/21/2020
      CASE ('WHAPS','TFAPS')
        READ (C360,850,IOSTAT=ERRNUM)
     &            VARTY,VRNAME,ECONO,VSEN,PPSEN,P2,P5,PHINT,GRNO,MXFIL,
     &            STMMX,SLAP1

!     Ceres Maize: maize, sweet corn **
      CASE ('MZCER','SWCER')
        READ (C360,'(A6,1X,A16,7X,A6,6F6.0)',IOSTAT=ERRNUM)
     &            VARTY,VRNAME,ECONO,P1,P2,P5,G2,G3,PHINT

!WDB 7/2016 Added cultivar coefficients for sugar beet model
      CASE ('BSCER')       
        READ (C360,'(A6,1X,A16,7X,A6,9F6.0)',IOSTAT=ERRNUM)         
     &        VARTY,VRNAME,ECONO,P1,P2,P5,G2,G3,PHYL1,PHYL2,FRSUG,DRCER
!WDB** end changes

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
     &            P1,P2,P2O,P2R,PANTH,P3,P4,P5,PHINT,G1,G2
C-GH &            P1,P2O,P2R,P5,G1,G2,PHINT,P3,P4,P2,PANTH
C-GH &            P1,P2O,P2R,P5,G1,G2,PHINT,P3,P4
!           Read optional PBASE and PSAT parameters
!           If these parameters are non-zero, then optional
!             method is used
     &            ,PBASE, PSAT
!       Optional cultivar parameters
        IF (PBASE < 1.E-2) PBASE = -99.
        IF (PSAT  < 1.E-2) PSAT  = -99.

!     Ceres Millet **
      CASE ('MLCER')
        READ (C360, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1,P2O,P2R,P5,G1,G4,PHINT,G0,G5
        IF (G4 > 1.2) THEN
          MSG(1)="G4 is fraction partitioning and should not exceed 1.2"
          MSG(2)=
     &   "G4 (from cultivar file) set equal to 1.2 for this simulation."
          CALL WARNING(2, ERRKEY, MSG)
          G4 = 1.2
        ENDIF

!     Substor Potato **
      CASE ('PTSUB')
        READ (C360, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            G2,G3,PD,P2,TC
!     &            G2,G3,G4,PD,P2,TC

!     Ceres Rice **
      CASE ('RICER')
        READ (C360,800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
!    &            P1,P2R,P5,P2O,G1,G2,G3,G4, PHINT, G5
     &            P1,P2R,P5,P2O,G1,G2,G3,PHINT, THOT, TCLDP, TCLDF

        IF (ERRNUM .GT. 0) THEN
          MSG(1) = "CULTIVAR FILE MAY BE OLD"
          CALL WARNING(1,ERRKEY,MSG)
          CALL ERROR(ERRKEY,ERRNUM,FILEGG,LINVAR)
        ENDIF

!     Ceres Teff **
      CASE ('TFCER')
        READ (C360,800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
!    &            P1,P2R,P5,P2O,G1,G2,G3,G4, PHINT, G5
     &            P1,P2R,P5,P2O,G1,G2,G3,PHINT, THOT, TCLDP, TCLDF

!        READ (C360,'(90X,F6.0)',IOSTAT=ERRNUM) G5
!!       For backwards compatibility for cultivar files with no G5.
!        IF (ERRNUM /= 0 .OR. ABS(G5-1.0) .LT. 1.E-3) THEN
!          MSG(1) = 'Parameter G5 has been activated in Ceres-rice.'
!          MSG(2) = 'Your results may not be accurate.'
!          MSG(3) = 'Please recalibrate your cultivar.'
!          CALL WARNING(3,ERRKEY,MSG)
!        ENDIF
!        !IF (G5 < 0.0) G5 = 1.0    Comment out to activate G5.

!!     ORYZA Rice **
!!     Read name of OYRZA crop file
!      CASE ('RIORZ')
!        READ (C360,'(A6,1X,A16,7X,A80)',IOSTAT=ERRNUM) VARTY,VRNAME,
!     &            PLAINTXT
!        ECONO = '      '

!     CaneGro: South African Sugarcane model **
      CASE ('SCCAN') 
        !WRITE(*, '(A)') C360
        READ (C360,1060,IOSTAT=ERRNUM) VARTY, VRNAME, ECONO,
     &      MaxPARCE, APFMX, STKPFMAX, SUCA, TBFT,  
     &      LFMAX, MXLFAREA, MXLFARNO, PI1, PI2, PSWITCH, TTPLNTEM, 
     &      TTRATNEM, CHUPIBASE, TT_POPGROWTH, POPTT16, 
     &      TAR0, TDELAY, LER0, SER0, LG_AMBASE, AQP_UP5 
     
! MJ removed 2018-02-08:
!            TBASE_GE_EM, TOPT_GE_EM, TFin_GE_EM, TBASE_LFEM,
!     &      TOPT_LFEM, TFinLFEM, TBASE_TLREM, TOPT_TLREM, TFin_TLREM, 
!     &      TBASE_LFSEN, TOPT_LFSEN, TFin_LFSEN, TBASE_STKEX, 
!     &      TOPT_STKEX,  TFin_STKEX,  TBASE_LFEX, TOPT_LFEX, TFin_LFEX,
!     &      TBASE_REX,  TOPT_REX, TFin_REX, TOPT_PHOT, TOPT_PHO2, 
!     &      TFin_PHOT,  TBASE_RESP,   TOPT_RESP,   TFin_RESP,      
     
        !WRITE(*, '(A, F10.5)') 'SER0 is ', SER0     

!     Casupro: Florida-Colombia Sugarcane model **
      CASE ('SCCSP')
        READ (C360,1055,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &          LFMAX,PHTMAX,StkH2OFac,SuH2OFac,empty,PLF1,
     &          PLF2,Gamma,StkB,StkM,empty,
     &          SIZLF,LIsun,LIshd,empty,TB(1),TO1(1),TO2(1),TM(1),
     &          PI1,PI2,DTPI,LSFAC,empty,LI1,TELOM,TB(2),TO1(2),
     &          TO2(2),TM(2),Ph1P,Ph1R,Ph2,Ph3,Ph4,StkHrNO,RTNFAC,
     &          MinGr,empty,RES30C,RLF30C,R30C2,empty,empty

!MV   SAMuCA: Agronomic Modular Simulator for Sugarcane
!     Simulador Agronomico Modular de Cana-de-Acucar
      CASE ('SCSAM') 
        READ (C360,1070,IOSTAT=ERRNUM) VARTY, VRNAME, ECONO,
     &      maxgl_r       		     ,
     &      n_lf_when_stk_emerg_r    ,
     &      n_lf_it_form_r           ,
     &      maxdgl_r       		     ,
     &      amax       			     ,
     &      eff       			     ,
     &      chustk       		     ,
     &      chupeak     		     ,
     &      chudec     			     ,
     &      chumat     			     ,
     &      popmat     			     ,
     &      poppeak      		     ,
     &      tillochron      	     ,
     &      sla       			     ,
     &      mla       			     ,
     &      plastochron       	     ,
     &      init_leaf_area           ,
     &      max_ini_la       	     ,
     &      max_it_dw                ,
     &      mid_tt_it_growth         ,
     &      end_tt_it_growth         ,
     &      mid_tt_lf_growth         ,
     &      end_tt_lf_growth         

!!     Sunflower **
       CASE ('SUOIL')
         READ (C360,800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &           P1,P2,P5,G2,G3,O1
     
!     Taro, tanier **
      CASE ('TRARO','TNARO')
        READ (C360,800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &           P1,P3,P4,P5,G3,G4,PHINT,PCINT,PCGRD

!!     Sunflower **
!      CASE ('SUOIL')
!        READ (C360,800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
!     &           P1,P2,P5,G2,G3,O1
!
!     Pineapple **
      CASE ('PIALO')
        READ (C360,800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &           P1,P2,P3,P4,P5,P6,G2,G3,PHINT
      END SELECT

      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEG,LINVAR)
!      IF (((ADJUSTL(VARTY) .NE. ADJUSTL(VARNO)) .AND. (NSENS .EQ. 0)) 
!     &       .OR. ((I .LT. NLVAR) .AND. (NSENS .EQ. 1))) GO TO 2010

      VARNO = VARTY
      CLOSE (LUNVAR)

      ! Added LAH Jan 1 2008
      OPEN (LUNVAR,FILE = FILEGG,STATUS = 'OLD',IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEG,0)
      DO I = 1, 100
        READ (LUNVAR,'(A)') C360
        IF (C360(1:1).EQ.'@') THEN
!         Skip the EXPNO column (CHP 1/29/2009)
!         ATLINE = C360(1:23) // C360(31:360)
          ATLINE = C360(1:23) // C360(31:1000)
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
  110 FORMAT (A6,1X,A16,7X,A6)
  120 FORMAT (I4,') ',A6,2X,A16,13X,A6,6X,A2)
  300 FORMAT (/,'  More.... press < ENTER > key')
  350 FORMAT (/,6X,'VARIETY SELECTED ===>',1X,I4,
     &        /,6X,'NEW SELECTION ?  --->',3X,' ',$)
  400 FORMAT (80A1)
  500 FORMAT (6X,'ERROR! Variety Selection must be between 1 & ',I3,/)
  510 FORMAT (6X,'ERROR! Variety Selection must be an INTEGER value',/)

  800 FORMAT (A6,1X,A16,7X,A6,21F6.0)      !11/8/07
  810 FORMAT (A6,1X,A16,7X,A6,20F6.0,A)    !WHCRP, BACRP 03/16/2010
  820 FORMAT (A6,1X,A16,7X,A6,21F6.0,A)    !CSCAS        02/18/2014
  821 FORMAT (A6,1X,A16,7X,A6,15F6.0)      !CSYCA        09/09/2020 
  830 FORMAT (A6,1X,A16,7X,A6,7F6.0,A)     !WHCER, BACER 03/16/2010
!JG moved parameters to ECO, 01/09/2020
  850 FORMAT (A6,1X,A16,7X,A6,9F6.0,A)     
 1055 FORMAT (A6,1X,A16,7X,A6,44F6.0)      ! 02/10/2009 
 1060 FORMAT (A6,1X,A16,7X,A6,22F15.0)     ! 02/21/2018 
 1070 FORMAT (A6,1X,A16,7X,A6,24F15.0)     ! 01/07/2020 (SAMUCA)
 1500 FORMAT (A6,1X,A16,7X,A)              ! 11/8/07

      END SUBROUTINE IPVAR
