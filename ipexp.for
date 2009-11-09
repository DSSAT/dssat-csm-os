C=======================================================================
C  IPEXP, Subroutine
C
C  Determines experiment and treatment selection
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1990 JWW Written
C  05/28/1993 PWW Header revision and minor changes            
C  12/10/1995 NBP Soil top 2 layers=10 cm, error check on MEEVP & MEEVP
C  09/30/1997 GH  Added cotton                                 
C  01/19/1998 PWW Added species file for Ceres and OilCrop 
C  05/06/1998 GH  Changed date and version number to v3.5, May 15, 1998  
C  08/17/1999 GH  Added cabbage  
C  09/20/2000 GH  Replaced G9 with BR for Brachiaria decumbens
C  09/20/2000 GH  Changed MESOM for 'Parton and Godwin options
C  09/20/2000 GH  Changed to version 3.90 (990)
C  11/03/2001 GH  Add CASUPRO
C  12/12/2001 GH  Extract model information
C  01/30/2002 GH  Modify inputs for the wheat model
C  06/07/2002 GH  Modify dates for Y2K
C  11/21/2002 GH  Modify rotation options and reps
C  02/20/2006 GH  Added RNMODE=G for GENCALC
C  02/21/2006 GH  Moved logic for crop model selection to DSSATPRO.V45
!  07/26/2006 CHP Added previous management code for lookup in 
!       SOMFR045.SDA file to FIELDS section
!  08/25/2006 CHP Added FILEX method codes MESOL, MESEV, METMP
!                 MESOL = alternate soil layer distribution
!                 MESEV = soil evaporation method (S=Sulieman (default), 
!                                                  R=Ritchie (old))
!                 METMP = soil temperature options
!  01/12/2007 CHP Rotation number used for sequence runs.
!  02/05/2007 CHP Reverse location of MESEV and METMP in FILEX
C  02/09/2007 GH  Allow for improved path selection for input files
!  03/26/2007 CHP MESOL = 2 is default (see LYRSET2 in LMATCH.FOR)
!  07/04/2007 CHP MESEV = 2 is default (Sulieman-Ritchie soil evap)
!  07/05/2007 CHP Default simulation controls file added.
!  09/18/2007 CHP Added codes for IXIM maize model 
!  04/21/2008 CHP Added MESOL = 3, user-specified soil layer dist.
!  12/09/2008 CHP Remove METMP
C-----------------------------------------------------------------------
C  INPUT  : MODEL,RUN,DS,SLNO,LNIC,LNSA,NYRS,VARNO,CROP,PATHMO,WMODI
C           FROP,SLTX
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : INPUT
C
C  Calls  : ERROR IGNORE VERIFY CLEAR FIND IPCUL PATH IPPLNT IPFLD IPSIM
C           YR_DOY IPENV IPHAR IPIRR IPRES IPFERT
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IPEXP (MODEL,RUN,RNMODE,FILEX,PATHEX,FILEX_P, FILECTL,
     &           SLNO,NYRS,VARNO, 
     &           CROP,WMODI,FROP,TRTN,EXPP,EXPN,TITLET,TRTALL,
     &           TRTNUM,ROTNUM, IIRV,FTYPEN,CHEXTR,
     &           NFORC,PLTFOR,NDOF,PMTYPE,
     &           LNSIM,LNCU,LNHAR,LNENV,LNTIL,LNCHE,
     &           LNFLD,LNSA,LNIC,LNPLT,LNIR,LNFER,LNRES,
     &           CONTROL, ISWITCH, UseSimCtr, MODELARG)


      USE ModuleDefs
      IMPLICIT NONE
      SAVE

      INCLUDE 'COMIBS.BLK'
      INCLUDE 'COMSWI.BLK'

      CHARACTER* 1 LINE(80),BLANK, RNMODE
      CHARACTER* 1 WMODI,ANS
      CHARACTER* 2 CROP
      CHARACTER* 3 PROCOD,ALN(13),ALLN
      CHARACTER* 4 WSTA1
      CHARACTER* 6 VARNO,ERRKEY,FINDCH
      CHARACTER* 7 FILELS
      CHARACTER* 8 FILES_a, FILES_b, MODEL, MODELARG
      CHARACTER*10 SLNO
      CHARACTER*12 NAMEF, FILEX
      CHARACTER*25 TITLET
      CHARACTER*42 CHEXTR(NAPPL)
      CHARACTER*78 MSG(4)
      CHARACTER*80 CHARTEST,PATHEX
      CHARACTER*92 FILEX_P,FILETMP
      CHARACTER*120 FILECTL, WTHSTR

      INTEGER I,L,NLOOP,LINF,ISECT,LUNEXP,LUNLST
      INTEGER LNFLD,LNSA,LNIC,LNPLT,LNIR,LNFER,LNRES,LNCHE,LNCU
      INTEGER LNHAR,LNENV,LNTIL,LNSIM,LINEXP
      INTEGER NYRS,FROP,EXPN,EXPP,TRTN,ERRNUM,IFIND,FTYPEN
      INTEGER PATHL,RUN,ISIM,TRTALL,IIRV(NAPPL)   !,CRID
      INTEGER NFORC,NDOF,PMTYPE,YR,ROTN
      INTEGER TRTNUM, ROTNUM

      REAL    FLAG,EXP,TRT,PLTFOR

      LOGICAL FEXIST, UseSimCtr, SimLevel

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH

C-----------------------------------------------------------------------

      PARAMETER (LUNEXP = 16)
      PARAMETER (LUNLST = 17)
      PARAMETER (ERRKEY = 'IPEXP ')
      PARAMETER (BLANK = ' ')
                 FINDCH = '*TREAT'

C-----------------------------------------------------------------------
      FILELS = 'EXP.LST'
      
C-----------------------------------------------------------------------
C     Set depths of individual soil layers
C-----------------------------------------------------------------------
C     This subroutine assumes that DS(L) values are depths to the bottom
C     of layer L
C
C     DS(L) can be interactively modified in the sensitivity analysis

!     M = Soil layer at which profile transitions from 
!             30 cm to 60 cm thickness.
!      M = 18  !soil layers 18, 19, 20 with thickness of 60 cm.

!      DS(1) =  5.
!      DS(2) = 15.
!      DS(3) = 30.
!      DS(4) = 45.
!      DS(5) = 60.
!
!      DO L = 6, M-1
!         DS(L) = DS(L - 1) + 30.
!      END DO
!
!      DO L = M, NL
!         DS(L) = DS(L - 1) + 60.
!      END DO

C-----------------------------------------------------------------------
      NLOOP = 0
      IF (RUN .EQ. 1) THEN
         EXPN   = 1
         EXPP   = 0
         TRTN   = 1
         TRTALL = 999
       ELSE
         EXPN   = EXPP
      ENDIF

      IF (RNMODE .EQ. 'I') THEN
         OPEN (LUNLST, FILE = FILELS,STATUS = 'OLD',IOSTAT=ERRNUM)
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILELS,0)

         WRITE (*,200)
         I = 0
  300    CONTINUE
         I = I + 1
         LINF = 0
  350    CONTINUE
         CALL IGNORE (LUNLST,LINF,ISECT,CHARTEST)
         IF (ISECT .EQ. 2) GO TO 350

         IF (ISECT .EQ. 1) THEN
            READ (CHARTEST,410,IOSTAT=ERRNUM) EXPER,CG,ENAME
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILELS,LINEXP)
            IF (MOD(I,16) .EQ. 0) THEN
               WRITE (*,600)
               READ (5,'(A1)') ANS
            ENDIF
            READ(EXPER(5:6),'(I2)') YR
            IF (YR .GE. 10) THEN
              WRITE (*,500) I,CG,ENAME(1:45),EXPER(1:2),EXPER(3:4),
     &                    EXPER(5:6),EXPER(7:8)
            ELSE
              WRITE (*,501) I,CG,ENAME(1:45),EXPER(1:2),EXPER(3:4),
     &                      EXPER(5:6),EXPER(7:8)
            ENDIF

          ELSE
            GO TO 800
         ENDIF

         GO TO 300
  800    CONTINUE
         REWIND (LUNLST)

C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------

  850    CONTINUE
         LINE(1) = ' '
         NLOOP = NLOOP + 1
         IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,2,FILELS,0)
         WRITE (*,1000) EXPN
         READ  (5,1100) LINE
         CALL VERIFY (LINE,EXP,FLAG)

         IF (EXP .LE. 0.0) THEN
            EXP = EXPN
          ELSEIF ((FLAG .GT. 0) .OR. (EXP .GT. (I-1))) THEN
            WRITE (*,1101) (I-1)
            GO TO 850
          ELSEIF (EXP .NE. NINT(EXP)) THEN
            WRITE (*,1102)
            GO TO 850
          ELSEIF (EXP .GT. 0.0) THEN
            EXPN = NINT(EXP)
          ELSE
            CALL ERROR (ERRKEY,2,FILELS,0)
         ENDIF
C
C     Establish the name of the experiment input file
C
        I = 0
 950    CONTINUE
        I = I + 1
 975    CONTINUE
        CALL IGNORE (LUNLST,LINF,ISECT,CHARTEST)
        IF (ISECT .EQ. 2) GO TO 975
        READ (CHARTEST,410,IOSTAT=ERRNUM) EXPER,CG,ENAME
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILELS,LINEXP)
        IF (I .LT. EXPN) GO TO 950
        CLOSE (LUNLST)
        FILEX(1:12) = EXPER//'.'//CG//'X'
        FILEX_P(1:12) = FILEX 
      ELSE
        READ(FILEX(10:11),'(A2)') CG
        READ(FILEX(1:8),'(A8)') EXPER
      ENDIF

C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
        
      FILEA(1:12) = EXPER//'.'//CG//'A'
      FILET(1:12) = EXPER//'.'//CG//'T'

C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------

      NLOOP = 0
      I     = 0

      OPEN (LUNEXP,FILE = FILEX_P,STATUS = 'OLD',IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) THEN
        MSG(1) = "File not found:"
        MSG(2) = FILEX_P
        CALL WARNING(2,ERRKEY,MSG)
        CALL ERROR (ERRKEY,29,FILEX_P,0)
      ENDIF
      READ(LUNEXP,1500) ENAME
 1500 FORMAT(25X,A60)
      IF (RNMODE .EQ. 'I') CALL CLEAR
      IF (EXPN .NE. EXPP) THEN
         TRTN = 1
      ENDIF
      IF (RNMODE .EQ. 'I' .OR. RNMODE .EQ. 'A' .AND.
     &   TRTALL .EQ. 999) THEN
         IF (RNMODE .EQ. 'I') WRITE (*,2300) ENAME(1:40)
         CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
         IF(IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)
 2400    CONTINUE
         I = I + 1
         CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
         IF (ISECT .EQ. 1) THEN
            READ (CHARTEST,54,IOSTAT=ERRNUM) (ALN(L),L=1,13)
   54       FORMAT (34X,13A3)
            DO L=1,13
              ALLN = ALN(L)
              IF (ALLN(3:3) .EQ. '?') THEN
                 ERRNUM = 99
                 CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
              ENDIF
            ENDDO
            IF (RNMODE .EQ. 'Q') THEN
              READ (CHARTEST,56,IOSTAT=ERRNUM)TRTNO,ROTNO,ROTOPT,CRPNO,
     &              TITLET,LNCU,LNFLD,LNSA,LNIC,LNPLT,LNIR,LNFER,LNRES,
     &              LNCHE,LNTIL,LNENV,LNHAR,LNSIM
            ELSE 
              READ (CHARTEST,55,IOSTAT=ERRNUM)TRTNO,ROTNO,ROTOPT,CRPNO,
     &              TITLET,LNCU,LNFLD,LNSA,LNIC,LNPLT,LNIR,LNFER,LNRES,
     &              LNCHE,LNTIL,LNENV,LNHAR,LNSIM
            ENDIF
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            IF (MOD(I,16) .EQ. 0 .AND. RNMODE .EQ. 'I') THEN
               WRITE (*,600)
               READ (5,'(A1)') ANS
            ENDIF
            READ(EXPER(5:6),'(I2)') YR
            IF (YR .GE. 10) THEN
            IF (RNMODE .EQ. 'I') WRITE (*,2600) I,TITLET,
     &          EXPER(1:2),EXPER(3:4),EXPER(5:6),EXPER(7:8),TRTNO
          ELSE
              IF (RNMODE .EQ. 'I') WRITE (*,2601) I,TITLET,
     &            EXPER(1:2),EXPER(3:4),EXPER(5:6),EXPER(7:8),TRTNO
            ENDIF
          ELSE
            GO TO 2700
         ENDIF
         GO TO 2400
 2700    CONTINUE
         TRTALL = I - 1
         IF (RNMODE .EQ. 'A') TRTN = 1
C-GH     IF (RNMODE .EQ. 'I') 
C-GH     &   WRITE (*,2650) I,EXPER(1:2),EXPER(3:4),EXPER(5:6),EXPER(7:8)
         
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
 2750    CONTINUE
         NLOOP = NLOOP + 1
         LINE(1) = ' '
         IF (NLOOP .GT. 25) CALL ERROR(ERRKEY,3,FILEX,LINEXP)
         IF (RNMODE .EQ. 'I') THEN
           WRITE (*,2900) TRTN
C
C        Read the correct treatment number
C
           READ (5,1100) LINE
           CALL VERIFY (LINE,TRT,FLAG)
         ENDIF
         IF (TRT .LE. 0.0) THEN
            TRT = TRTN
C-GH      ELSEIF (TRT .EQ. (TRTALL+1) .AND. RNMODE .EQ. 'I') THEN
C-GH        RNMODE = 'A'
C-GH        TRTN   = 1
          ELSEIF ((FLAG .GT. 0) .OR. (TRT .GT. I)) THEN
            WRITE (*,2751) (I-1)
            GO TO 2750
          ELSEIF (TRT .NE. NINT(TRT)) THEN
            WRITE(*,2752)
            GO TO 2750
          ELSEIF (TRT .GT. 0.) THEN
            TRTN = NINT(TRT)
          ELSE
            CALL ERROR (ERRKEY,3,FILEX,LINEXP)
         ENDIF
       ELSEIF (INDEX ('Q',RNMODE) .GT. 0) THEN
         !READ (TRNARG(1:6),'(I6)') TRTN
         !READ (ROTNARG(1:6),'(I6)') ROTN
         TRTN = TRTNUM
         ROTN = ROTNUM
         I = 999
       ELSEIF (INDEX ('NQGSFBECT',RNMODE) .GT. 0) THEN
!         READ (TRNARG(1:6),'(I6)') TRTN
         TRTN = TRTNUM
         I = 999
       ELSEIF (INDEX ('A',RNMODE) .GT. 0) THEN
         TRTN = TRTN + 1
      ENDIF
      
C-----------------------------------------------------------------------
C     Find treatment number and appropriate levels
C-----------------------------------------------------------------------
      REWIND (LUNEXP)
      CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
      IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)

!     Read @Line, check that 13th column = "SM"
      CALL IGNORE2(LUNEXP,LINEXP,ISECT,CHARTEST)
      SimLevel = .TRUE.
      IF (ISECT == 3) THEN
        IF (CHARTEST(72:73) /= 'SM') THEN
          SimLevel = .FALSE.
        ENDIF
      ENDIF

      I = 0
 50   CONTINUE
      I = I + 1
      CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
      IF (RNMODE .EQ. 'Q') THEN
        READ (CHARTEST,56,IOSTAT=ERRNUM) TRTNO,ROTNO,ROTOPT,CRPNO,
     &     TITLET,LNCU,LNFLD,LNSA,LNIC,LNPLT,LNIR,LNFER,LNRES,
     &     LNCHE,LNTIL,LNENV,LNHAR,LNSIM
      ELSE
        READ (CHARTEST,55,IOSTAT=ERRNUM) TRTNO,ROTNO,ROTOPT,CRPNO,
     &     TITLET,LNCU,LNFLD,LNSA,LNIC,LNPLT,LNIR,LNFER,LNRES,
     &     LNCHE,LNTIL,LNENV,LNHAR,LNSIM
      ENDIF
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)

C     IF (I .LT. TRTN) GO TO 50
      IF ((INDEX('BEDNSGFCT',RNMODE) .GT. 0 .AND. TRTN .NE. TRTNO) .OR.
     &    (INDEX('Q',RNMODE) .GT. 0 .AND. 
     &                     (TRTN .NE. TRTNO .OR. ROTN .NE. ROTNO)) .OR. 
     &    (INDEX('AI',RNMODE) .GT. 0 .AND. I .LT. TRTN))
     &    GO TO 50

!     Generate header information for Warnings or Errors in input module
      CALL OPHEAD (RUNINIT,99,0.0,0.0,"                ",0.0,0.0, 
     &     "      ",RUN,"        ",TITLET,WTHSTR, RNMODE,
     &     CONTROL, ISWITCH, UseSimCtr)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------

c      IF (RNMODE .EQ. 'I' .OR. RNMODE .EQ. 'A' .AND. TRTN .EQ. 1) THEN
c         CALL CLEAR
c         WRITE(*,3450)
c      ENDIF

C-----------------------------------------------------------------------
C     Call input section for cultivar selection
C-----------------------------------------------------------------------

      CALL IPCUL (LUNEXP,FILEX,LNCU,CROP,VARNO)
      IF (CROP   .EQ. '  ') CALL ERROR (ERRKEY,10,FILEX,LINEXP)
      IF (VARNO  .EQ. '  ') CALL ERROR (ERRKEY,11,FILEX,LINEXP)
      CONTROL % CROP = CROP

!     CHP 10/25/2006 Move this to IPSIM 
!C-----------------------------------------------------------------------
!C    Select Model Name and Path
!C-----------------------------------------------------------------------
!
!      PROCOD = 'M' // CROP  
!      CALL MODEL_NAME (PROCOD,DSSATP,MODEL)

C-----------------------------------------------------------------------
C  12/12/2008 Move planting date read to above simulation controls -- 
!     needed for Tony's generic simulation controls.
C-----------------------------------------------------------------------

      CALL IPPLNT_Inp (LUNEXP,FILEX,LNPLT,PLME,PLDS,ROWSPC,AZIR,CROP,
     &     SDEPTH,SDWTPL,PLTPOP,PLANTS,SDAGE,ATEMP,PLPH,IEMRG,
     &     YRPLT,SPRLAP,NFORC,PLTFOR,NDOF,PMTYPE,IPLTI)

C-----------------------------------------------------------------------
C     Call IPSIM
C-----------------------------------------------------------------------

      IF (.NOT. SimLevel) THEN
        LNSIM = 0
        YRSIM = YRPLT
      ENDIF

      CALL IPSIM (LUNEXP,LNSIM,TITSIM,NYRS,RUN,NREPSQ,ISIMI,PWDINF,
     &     PWDINL,SWPLTL,NCODE,SWPLTH,SWPLTD,YEAR,PTX,PTTN,DSOIL,THETAC,
     &     IEPT,IOFF,IAME,DSOILN,SOILNC,YRSIM,SOILNX,NEND,RIP,NRESDL,
     &     DRESMG,HDLAY,HLATE,HPP,HRP,FTYPEN,RSEED1,LINEXP,AIRAMT,
     &     EFFIRR,CROP,FROP,MODEL,RNMODE,FILEX,
     &     CONTROL, ISWITCH, UseSimCtr, FILECTL, MODELARG, YRPLT)
      
C-----------------------------------------------------------------------
C        Select crop parameter input file
C-----------------------------------------------------------------------

      IF (CROP .NE. 'FA') THEN
        ! IF ((INDEX('GROSIM',MODEL(3:5)) .GT. 0) .OR.
     &  !    (INDEX('ALOCERSUBOIL',MODEL(3:5)) .GT. 0) .OR.
     &  !    (INDEX('CSPCAN',MODEL(3:5)) .GT.0) .OR. 
     &  !    (INDEX('CRP',MODEL(3:5)) .GT.0) .OR.  !Cassava
     &  !    (INDEX('IXM',MODEL(3:5)) .GT.0) .OR.  !IXIM MAIZE
     &  !    (INDEX('CSM',MODEL(3:5)) .GT.0)) THEN
           FILEC(1:12) = CROP//MODEL(3:8)//'.SPE'
           INQUIRE (FILE = FILEC,EXIST = FEXIST)
           IF (.NOT. FEXIST) THEN
              CALL PATH('CRD',DSSATP,PATHCR,1,NAMEF)
            ELSE
              PATHCR = BLANK
           ENDIF
        ! ENDIF
C-----------------------------------------------------------------------
C        Select genetic parameter input file
C
C        READ GENCALC2.CUL if RNMODE = G & T
C        READ ???????0.CUL for other modes
C-----------------------------------------------------------------------
         FILEG(1:12) = CROP//MODEL(3:8)//'.CUL'
         IF (INDEX('GT',RNMODE) .GT. 0) THEN
            WRITE(FILEG(1:8),'(A8)') 'GENCALC2'
         ENDIF
         INQUIRE (FILE = FILEG,EXIST = FEXIST)
         IF (.NOT. FEXIST) THEN
            FILETMP = TRIM(PATHEX)//FILEG
            INQUIRE (FILE = FILETMP,EXIST = FEXIST)
            IF (.NOT. FEXIST) THEN
               CALL PATH('CRD',DSSATP,PATHGE,1,NAMEF)
            ELSE 
               PATHGE = TRIM(PATHEX)
            ENDIF
         ELSE
            PATHGE = BLANK
         ENDIF

C-----------------------------------------------------------------------
C        Select ecotype parameter input file
C
C        READ GENCALC2.ECO if RNMODE = G & T
C        READ ???????0.ECO for other modes;
C-----------------------------------------------------------------------
         FILEE(1:12) = CROP//MODEL(3:8)//'.ECO'
         IF (INDEX('GT',RNMODE) .GT. 0) THEN
            WRITE(FILEE(1:8),'(A8)') 'GENCALC2'
         ENDIF
         INQUIRE (FILE = FILEE,EXIST = FEXIST)
         IF (.NOT. FEXIST) THEN
            FILETMP = TRIM(PATHEX)//FILEE
            INQUIRE (FILE = FILETMP,EXIST = FEXIST)
            IF (.NOT. FEXIST) THEN
               CALL PATH ('CRD',DSSATP,PATHEC,1,NAMEF)
            ELSE
               PATHEC = TRIM(PATHEX)
            ENDIF
         ELSE
              PATHEC = BLANK
         ENDIF
        
C-----------------------------------------------------------------------
C        Select pest parameter input file
C-----------------------------------------------------------------------

         FILEP(1:12) = CROP//MODEL(3:8)//'.PST'
         INQUIRE (FILE = FILEP,EXIST = FEXIST)
         IF (.NOT. FEXIST) THEN
            CALL PATH('PSD',DSSATP,PATHPE,1,NAMEF)
          ELSE
            PATHPE = BLANK
         ENDIF

C-----------------------------------------------------------------------
C        End of IF NOT fallow
C-----------------------------------------------------------------------
      ENDIF

      CALL GET_CROPD(CROP, CROPD)

      REWIND(LUNEXP)

!     Regen short headers now that MODEL is known.
      CALL OPHEAD (RUNINIT,99,0.0,0.0,"                ",0.0,0.0, 
     &     "      ",RUN,MODEL,TITLET,WTHSTR, RNMODE,
     &     CONTROL, ISWITCH, UseSimCtr)

C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
!     Skip soils field and soils input for sequence mode
      IF (INDEX('FQ',RNMODE) .LE. 0 .OR. RUN == 1) THEN

        CALL IPFLD (LUNEXP,FILEX,LNFLD,FLDNAM,WSTA,WSTA1,SLNO,
     &     SLTX,FLST,SLOPE,DFDRN,FLDD,SFDRN,FLOB,SLDP,
     &     XCRD,YCRD,ELEV,AREA,SLEN,FLWR,SLAS,FldHist, FHDur)

C-----------------------------------------------------------------------
C     Select soil profile input file
C       1. SOIL.SOL
C       2. ??.SOL where ?? = Institute ID from Soil Profile Number
C       3. From C:\DSSAT45\DSSATPRO.V45  SOIL.SOL
C       4. From C:\DSSAT45\DSSATPRO.V45  ??.SOL
C-----------------------------------------------------------------------

        FILES_a = 'SOIL.SOL'
        FILES_b = SLNO(1:2)//'.SOL  '

        INQUIRE (FILE = FILES_a,EXIST = FEXIST)
        IF (FEXIST) THEN
!          SOIL.SOL in current directory
           FILES = FILES_a
           PATHSL = BLANK

        ELSE
           INQUIRE (FILE = FILES_b,EXIST = FEXIST)
           IF (FEXIST) THEN
!             Alt soil name in current directory
              FILES = FILES_b
              PATHSL = BLANK 

           ELSE
              FILETMP = TRIM(PATHEX)//FILES_a
              INQUIRE (FILE = FILETMP,EXIST = FEXIST)
              IF (FEXIST) THEN
!                SOIL.SOL in experiment directory
                 FILES = FILES_a
                 PATHSL = TRIM(PATHEX)

              ELSE
                 FILETMP = TRIM(PATHEX)//FILES_b
                 INQUIRE (FILE = FILETMP,EXIST = FEXIST)
                 IF (FEXIST) THEN
!                   Alt soil name in experiment directory
                    FILES = FILES_b
                    PATHSL = TRIM(PATHEX)

                 ELSE
                    PROCOD = 'SLD'
                    CALL PATH (PROCOD,DSSATP,PATHSL,1,NAMEF)
                    PATHL  = INDEX(PATHSL,BLANK)
                    FILETMP = PATHSL(1:(PATHL-1)) // FILES_a
                    INQUIRE (FILE = FILETMP,EXIST = FEXIST)
                    IF (FEXIST) THEN
!                      SOIL.SOL in DSSAT soil directory
                       FILES = FILES_a

                    ELSE
                       FILETMP = PATHSL(1:(PATHL-1)) // FILES_b
                       INQUIRE (FILE = FILETMP,EXIST = FEXIST)
                       IF (FEXIST) THEN
!                         Alt soil name in DSSAT soil directory
                          FILES = FILES_b
                       ELSE
!                         No valid soils file found
                          WRITE(MSG(1),5000) FILES_a, FILES_b 
                          WRITE(MSG(2),5010) 
                          WRITE(MSG(3),5020) PATHEX(1:76) 
                          WRITE(MSG(4),5030) PATHSL(1:76)
                          CALL WARNING(4,ERRKEY,MSG) 
                          CALL ERROR(ERRKEY,80,FILES_a,0)
                       ENDIF
                    ENDIF
                 ENDIF
              ENDIF
           ENDIF
        ENDIF

      ENDIF   !Skip for sequence

C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      IF (ISIMI .EQ. 'S') THEN
         IF (YRSIM .LT. 0) THEN
           YRSIM = YRPLT
         ENDIF
      ELSE IF (ISIMI .EQ. 'P') THEN
           YRSIM = YRPLT
      ELSE IF (ISIMI .EQ. 'E') THEN
           YRSIM = IEMRG
           YRPLT = IEMRG
      ENDIF
      IF (CROP .EQ. 'FA' .AND. YRPLT .EQ. YRSIM) THEN
         YRSIM = YRSIM - 1
      ENDIF
      CALL YR_DOY (YRSIM,YEAR,ISIM)
      CONTROL % YRSIM = YRSIM

C-----------------------------------------------------------------------
C     Now establish the weather file FILEW as WSTA + .WT?  where ? :
C
C          M = observed data
C          G = generated data
C          S = interactively generated
C-----------------------------------------------------------------------

      IF (MEWTH .EQ. 'G') THEN
         IF (WSTA1(4:4) .EQ. BLANK) THEN
            IF (YEAR .LT. 2000) THEN
              YR = YEAR - 1900
            ELSE IF (YEAR .LT. 3000) THEN
              YR = YEAR - 2000
            ENDIF
            WRITE (FILEW(1:12),75) WSTA,YR,'01.WTG'
         ELSE
            WRITE (FILEW(1:12),76) WSTA,WSTA1,'.WTG'
         ENDIF
         PROCOD = 'WGD'
      ELSEIF (MEWTH .EQ. 'S' .OR. MEWTH .EQ. 'W') THEN
         WRITE (FILEW(1:12),77) WSTA,'.CLI    '
         PROCOD = 'CLD'
      ELSEIF (MEWTH .EQ. 'M') THEN
         IF (WSTA1(4:4) .EQ. BLANK) THEN
           IF (YEAR .LT. 2000) THEN
             YR = YEAR - 1900
           ELSE IF (YEAR .LT. 3000) THEN
             YR = YEAR - 2000
           ENDIF
           WRITE (FILEW(1:12),75) WSTA,YR,'01.WTH'
         ELSE
            WRITE(FILEW(1:12),76) WSTA,WSTA1,'.WTH'
         ENDIF
         PROCOD = 'WED'
      ELSE
         CALL ERROR (ERRKEY,22,FILEX,LINEXP)
      ENDIF

      INQUIRE (FILE = FILEW,EXIST = FEXIST)
      IF (.NOT. FEXIST) THEN
        FILETMP = TRIM(PATHEX)//FILEW
        INQUIRE (FILE = FILETMP,EXIST = FEXIST)
        IF (.NOT. FEXIST) THEN
          CALL PATH(PROCOD,DSSATP,PATHWT,1,NAMEF)
          FILETMP = TRIM(PATHWT) // FILEW
          INQUIRE (FILE=FILETMP, EXIST = FEXIST)
          IF (.NOT. FEXIST) THEN
            FILEW = FILEW(1:4) // ".WTH"
            FILETMP = TRIM(PATHWT) // FILEW
            INQUIRE (FILE=FILETMP, EXIST = FEXIST)
!            IF (.NOT. FEXIST .AND. INDEX('T',RNMODE) < 1) THEN
!!             For RNMODE = 'T', let weather routine end run for missing weather file.
!              MSG(1) = "Weather file not found. Program will end."
!              MSG(2) = FILEW // 
!     &          " not found in weather or experiment directories."
!              CALL WARNING(2,ERRKEY,MSG)
!              CALL ERROR(ERRKEY,29,FILEW,0)
!            ENDIF
          ENDIF
        ELSE
          PATHWT = TRIM(PATHEX)
        ENDIF
      ELSE
        PATHWT = BLANK
      ENDIF

C-----------------------------------------------------------------------
C     Build output files.
C
C     Generic output file names with extension 'OUT' are overwritten
C     at the start of each simulation.
C
C     IOX = 'Y' creates experiment specific output file names
C-----------------------------------------------------------------------
      IF (IOX .EQ. 'Y') THEN
         WRITE (OUTO(1:12),80) EXPER,'.',CG,'O'
       ELSE
         OUTO  = 'OVERVIEW.OUT'
      ENDIF

C-----------------------------------------------------------------------
C     Call IPENV
C-----------------------------------------------------------------------
      CALL IPENV (FILEX,LNENV,LUNEXP,CO2ADJ,CO2FAC,DAYADJ,
     &     DAYFAC,DPTADJ,DPTFAC,NEV,PRCADJ,PRCFAC,RADADJ,RADFAC,
     &     TMADJ,TMFAC,TXADJ,TXFAC,WMDATE,WMODI,WNDADJ,WNDFAC,
     &     WTHADJ)

C-----------------------------------------------------------------------
C     Call IPHAR
C-----------------------------------------------------------------------
      CALL IPHAR (LUNEXP,FILEX,LNHAR,HDATE,HSTG,HCOM,HSIZ,HPC,
     &     NHAR,IHARI,YRSIM,CROP,HBPC)

C-----------------------------------------------------------------------
C     Call IPIRR
C-----------------------------------------------------------------------
      CALL IPIRR (LUNEXP,FILEX,LNIR,YRSIM,ISWWAT,
     &     NIRR,EFFIRX,DSOILX,THETCX,IEPTX,IOFFX,IAMEX,LNSIM,
     &     NAPW,TOTAPW,AIRAMX,IDLAPL,IRRCOD,AMT,IIRV,IIRRI)

C-----------------------------------------------------------------------
C     Call IPFERT
C-----------------------------------------------------------------------
      CALL IPFERT (LUNEXP,FILEX,LNFER,YRSIM,ISWNIT,ISWPHO,ISWPOT,
     &     NFERT,FDAY,IFTYPE,FERCOD,DFERT,ANFER,APFER,AKFER,ACFER,
     &     AOFER,FOCOD,TOTNAP,IFERI,ISWWAT,LNSIM)

C-----------------------------------------------------------------------
C     Call IPRES
C-----------------------------------------------------------------------
      CALL IPRES (LUNEXP,FILEX,LNRES,RESDAY,RESCOD,RESIDUE,
     &     RINP,DEPRES,RESN,RESP,RESK,NARES,RESAMT,ISWNIT,YRSIM,
     &     ISWPHO,ISWPOT,IRESI,ISWWAT,RMET,LNSIM)

C-----------------------------------------------------------------------
C     Call IPCHEM - Chemical applications
C-----------------------------------------------------------------------
      CALL IPCHEM (LUNEXP,FILEX,LNCHE,YRSIM,ISWWAT,NCHEM,CDATE,
     &    CHCOD,CHAMT,CHMET,CHDEP,CHT,ISWCHE,LNSIM,CHEXTR)

C-----------------------------------------------------------------------
C     Call IPTILL - Tillage operations
C-----------------------------------------------------------------------
      CALL IPTILL (LUNEXP,FILEX,LNTIL,YRSIM,ISWTIL,NTIL,TDATE,
     &    TIMPL,TDEP,LNSIM)

      CLOSE(LUNEXP)
      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

   55 FORMAT (I3,I1,2(1X,I1),1X,A25,14I3)
   56 FORMAT (2I2,2(1X,I1),1X,A25,14I3)

   75 FORMAT (A4,I2.2,A6)
   76 FORMAT (3A4)
   77 FORMAT (A4,A8)
   80 FORMAT (A8,A1,A2,A1)
  200 FORMAT (T57,'INST.',T64,'SITE',T70,'YEAR',T75,'EXPT.',
     &  /,T7,'CROP EXPERIMENTAL CASE STUDIES',T58,'ID',T65,'ID',
     &  T76,'NO',/T7,4('-'),1X,31('-'),T57,'----',
     &    T64,'----',T70,'----',T75,'----')
  410 FORMAT (3X,A8,1X,A2,2X,A60)
  500 FORMAT (1X,I3,'.',2X,A2,2X,A45,1X,A2,5X,A2,3X,'19',A2,2X,A2)
  501 FORMAT (1X,I3,'.',2X,A2,2X,A45,1X,A2,5X,A2,3X,'20',A2,2X,A2)
  600 FORMAT (/,'  More.... press < ENTER > key',$)
 1000 FORMAT (/,6X,'EXPERIMENT SELECTED ===>',1X,I3,
     &        /,6X,'NEW SELECTION ?     --->',2X,' ',$)
 1100 FORMAT (80A1)
 1101 FORMAT (10X,'ERROR! Experiment Selection must be between 1',
     &            ' and ',I3,/)
 1102 FORMAT (10X,'ERROR! Experiment Selection must be an',
     &            ' INTEGER value',/)
 2300 FORMAT (T47,'INST.',T54,'SITE',T60,'YEAR',T66,'EXPT.',
     &  T72,'TRT.',/,T7,A40,T48,'ID',T55,'ID',T67,'NO',
     &  T73,'NO', /,T7,37('-'),T47,'----',
     &  T54,'----',T60,'----',T66,'----',T72,'----')
 2600 FORMAT (1X,I3,'.',1X,A25,16X,A2,5X,A2,3X,'19',A2,3X,A2,3X,I3)
 2601 FORMAT (1X,I3,'.',1X,A25,16X,A2,5X,A2,3X,'20',A2,3X,A2,3X,I3)
 2650 FORMAT (1X,I3,'.',1X,'RUN ALL TREATMENTS',23X,
     &        A2,5X,A2,3X,'19',A2,3X,A2,4X,I2)
 2751 FORMAT (10X,'ERROR! Treatment Selection must be between 1',
     &            ' and ',I3,/)
 2752 FORMAT (10X,'ERROR! Treatment Selection must be an INTEGER',/)
 2900 FORMAT (/,6X,'TREATMENT SELECTED ===>',1X,I3,
     &        /,6X,'NEW SELECTION ?    --->',2X,' ',$)
 3450 FORMAT (//////,15X,' Reading Data.  Please be patient.',/,
     &               15X,' Do not touch the keyboard !',/,16X,33('='))
 5000 FORMAT("Soil files not found: ",A,", ",A)
 5010 FORMAT("Searched current directory and the following:")
 5020 FORMAT(2X,A76)
 5030 FORMAT(2X,A76)

      END SUBROUTINE IPEXP

C=======================================================================
C  IPPLNT, Subroutine
C
C  Reads parameters related to planting operation from FILEX file
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1990 JWJ Written
C  05/28/1993 PWW Header revision and minor changes            
C  02/21/2006 GH  Update 
C-----------------------------------------------------------------------
C  INPUT  : LUNEXP,FILEX,LNPLT
C
C  LOCAL  : LN
C
C  OUTPUT : IPLT,PLME,PLDS,ROWSPC,AZIR,SDEPTH,SDWTPL,PLTPOP,PLANTS,
C           SDAGE,ATEMP,PLPH,IEMRG
C-----------------------------------------------------------------------
C  Called : IPEXP
C
C  Calls  : ERROR IGNORE FIND YR_DOY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IPPLNT_Inp (
     &     LUNEXP,FILEX,LNPLT,PLME,PLDS,ROWSPC,AZIR,CROP,
     &     SDEPTH,SDWTPL,PLTPOP,PLANTS,SDAGE,ATEMP,PLPH,IEMRG,
     &     YRPLT,SPRLAP,NFORC,PLTFOR,NDOF,PMTYPE,IPLTI)

      IMPLICIT NONE

      CHARACTER*1   PLME,PLDS,IPLTI
      CHARACTER*2   CROP
      CHARACTER*6   ERRKEY,FINDCH
      CHARACTER*12  FILEX
      CHARACTER*110 CHARTEST

      INTEGER   LUNEXP,LNPLT,IEMRG,LN,LINEXP,ISECT,IFIND,ERRNUM
      INTEGER   IPLT,YRPLT,YR,NFORC,NDOF,PMTYPE

      REAL      ROWSPC,AZIR,SDEPTH,SDWTPL,PLTPOP,PLANTS,SDAGE,ATEMP
      REAL      PLPH,SPRLAP,PLTFOR

      PARAMETER (ERRKEY='IPPLNT')
                 FINDCH='*PLANT'
      LINEXP = 0
      REWIND(LUNEXP)
      IF (LNPLT .GT. 0) THEN
         CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
         IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)
 50      CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
         IF (ISECT .EQ. 1) THEN
C
C           Actual read statement for Planting inputs
C
            READ (CHARTEST,60,IOSTAT=ERRNUM) LN,YRPLT,IEMRG,PLANTS,
     &      PLTPOP,PLME,PLDS,ROWSPC,AZIR,SDEPTH,SDWTPL,SDAGE,ATEMP,
     &      PLPH,SPRLAP,NFORC,PLTFOR,NDOF,PMTYPE
C New variables for pineapple
            IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEX,LINEXP)
            CALL Y2K_DOY(YRPLT)
            CALL Y2K_DOY(IEMRG)
            CALL YR_DOY (YRPLT,YR,IPLT)
          ELSE
            CALL ERROR (ERRKEY,2,FILEX,LINEXP)
         ENDIF
         IF (LN .NE. LNPLT) GO TO 50
         IF (IPLTI .EQ. 'R') THEN
           IF ((YRPLT .LT. 1 .OR. YRPLT .GT. 9999999)
     &       .AND. IEMRG .LT. 1) THEN
              CALL ERROR (ERRKEY,10,FILEX,LINEXP)
           ENDIF
         ENDIF
         IF (PLTPOP .LE. 0.0 .AND. PLANTS .GT. 0.0) THEN
            PLTPOP = PLANTS
         ENDIF
         IF (PLTPOP .LE. 0.0 .OR. PLTPOP .GT. 999.) THEN
            IF (CROP /= 'SC') CALL ERROR (ERRKEY,11,FILEX,LINEXP)
         ENDIF
         IF ((ROWSPC .GT. -90. .AND. ROWSPC .LE. 0.0)
     &      .OR. ROWSPC .GT. 99999.) THEN
            CALL ERROR (ERRKEY,12,FILEX,LINEXP)
         ENDIF
         IF ((AZIR .GT. -90. .AND. AZIR .LT. 0.0)
     &      .OR. AZIR .GT. 99999.) THEN
            CALL ERROR (ERRKEY,13,FILEX,LINEXP)
         ENDIF
         IF (SDEPTH .LE. 0.0 .OR. SDEPTH .GT. 100.0) THEN
            IF (CROP /= 'SC') CALL ERROR (ERRKEY,14,FILEX,LINEXP)
         ENDIF
         IF ((INDEX('PT',CROP)) .GT. 0) THEN
           IF (SPRLAP .LE. 0.0) THEN
              CALL ERROR (ERRKEY,16,FILEX,LINEXP)
           ENDIF
           IF (SDWTPL .LE. 0.0) THEN
              CALL ERROR (ERRKEY,17,FILEX,LINEXP)
           ENDIF
         ENDIF

         IF (INDEX('TSPNRCBHIV',PLME) .LE. 0) 
     &                    CALL ERROR (ERRKEY,19,FILEX,LINEXP)
         IF (INDEX('RI',CROP) .LE. 0) THEN    !CHP ADDED
           IF ((INDEX('T',PLME)) .GT. 0) THEN
             IF (SDWTPL .LT. 0.0) THEN
               CALL ERROR (ERRKEY,18,FILEX,LINEXP)
             ENDIF
           ENDIF
         ENDIF
      ENDIF

      REWIND (LUNEXP)

      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

 60   FORMAT (I3,I5,1X,I5,2(1X,F5.0),2(5X,A1),8(1X,F5.0),I6,F6.0,2I6)

      END SUBROUTINE IPPLNT_Inp

C=======================================================================
C  IPFLD, Subroutine
C
C  Reads parameters related to field operation from FILEX file
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1990 JWJ Written
C  05/28/1993 PWW Header revision and minor changes            
C  02/21/2006 GH  Update 
!  07/26/2006 CHP Added previous management code for lookup in 
!       SOMFR045.SDA file to FIELDS section
C-----------------------------------------------------------------------
C  INPUT  : LUNEXP,FILEX,LNFLD
C
C  LOCAL  : LN
C
C  OUTPUT : FLDNAM,WSTA,SLNO,SLOPE,DFDRN,FLDD,SFDRN,SLTX,FLST,FILEW,FLOB
C-----------------------------------------------------------------------
C  Called : IPEXP
C
C  Calls  : ERROR IGNORE FIND YR_DOY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IPFLD (LUNEXP,FILEX,LNFLD,FLDNAM,WSTA,WSTA1,SLNO,
     &           SLTX,FLST,SLOPE,DFDRN,FLDD,SFDRN,FLOB,SLDP,
     &           XCRD,YCRD,ELEV,AREA,SLEN,FLWR,SLAS,FldHist, FHDUR)

      IMPLICIT NONE

      CHARACTER*1  UPCASE
      CHARACTER*4  WSTA,WSTA1,HFNDCH
      CHARACTER*5  DFDRN,FLST,SLTX, FldHist
      CHARACTER*6  ERRKEY,FINDCH
      CHARACTER*8  FLDNAM
      CHARACTER*10 SLNO
      CHARACTER*12 FILEX
      CHARACTER*92 CHARTEST

      INTEGER LUNEXP,LNFLD,LN,LINEXP,ISECT,IFIND,ERRNUM,I, FHDUR

      REAL    FLDD,SFDRN,FLOB,SLDP,SLOPE
      REAL    XCRD,YCRD,ELEV,AREA,SLEN,FLWR,SLAS

      PARAMETER (ERRKEY='IPFLD ')
                 FINDCH='*FIELD'
      LINEXP = 0
      REWIND (LUNEXP)
      CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
      IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)
 50   CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
      IF (ISECT .EQ. 1) THEN
         READ (CHARTEST,60,IOSTAT=ERRNUM) LN,FLDNAM,WSTA,WSTA1,SLOPE,
     &                     FLOB,DFDRN,FLDD,SFDRN,FLST,SLTX,SLDP,SLNO
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
       ELSE
         CALL ERROR (ERRKEY,2,FILEX,LINEXP)
      ENDIF
      IF (LN .NE. LNFLD) GO TO 50
      DO I = 1, 4
        WSTA(I:I)  = UPCASE(WSTA(I:I))
        WSTA1(I:I) = UPCASE(WSTA1(I:I))
      END DO
      IF (WSTA(1:3) .EQ. '-99' .AND. SLNO(1:3) .EQ. '-99') THEN
        CLOSE(LUNEXP)
        STOP
      ENDIF

      IF (WSTA .EQ. '    ') THEN
         CALL ERROR (ERRKEY,10,FILEX,LINEXP)
      ENDIF
      IF (SLNO .EQ. '          ') THEN
         CALL ERROR(ERRKEY,11,FILEX,LINEXP)
      ENDIF
      IF (SLOPE .LT. 0.0) THEN
         SLOPE = 0.0
      ENDIF
      IF (SFDRN .LE. 0.0) THEN
        SFDRN = 100.
      ENDIF

C
C    New section
C
C    Find header and read second line of field information
C
      HFNDCH='SLAS'
      CALL HFIND(LUNEXP,HFNDCH,LINEXP,IFIND)
      IF (IFIND .EQ. 1) THEN
 70     CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
        IF (ISECT .EQ. 1) THEN
           READ (CHARTEST,80,IOSTAT=ERRNUM) LN,
     &                XCRD,YCRD,ELEV,AREA,SLEN,FLWR,SLAS, FldHist, FHDUR
           IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
         ELSE
           CALL ERROR (ERRKEY,2,FILEX,LINEXP)
         ENDIF
         IF (LN .NE. LNFLD) GO TO 70
      ENDIF
      IF (AREA .LE. 0.0) AREA = 1.0
      IF (FLWR .LE. 0.0) FLWR = 1.0
      IF (SLEN .LE. 0.0) SLEN = SQRT(AREA*FLWR*10000.0)

C
C    End New section

      REWIND(LUNEXP)

      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

 60   FORMAT (I3,A8,1X,2A4,1X,F5.0,1X,F5.0,1X,A5,2(1X,F5.0),
     &         2(1X,A5),1X,F5.0,1X,A10)
!     chp 7/26/2006
! 80   FORMAT (I3,2(F15.0,1X),F9.0,1X,F17.0,3(1X,F5.0))
 80   FORMAT (I3,2(F15.0,1X),F9.0,1X,F17.0,3(1X,F5.0),1X,A5,I6)

      END SUBROUTINE IPFLD

C=======================================================================
C  IPSIM, Subroutine
C
C  Reads parameters related to field operation from FILEX file
C-----------------------------------------------------------------------
C  Revision history
C  01/01/1990 JWJ Written
C  05/28/1993 PWW Header revision and minor changes            
C  11/19/2003 CHP Added check for MEPHO and incompatible models.
C  02/21/2006 GH  Removed crop model selection
!  10/25/2006 CHP CRMODEL from FILEX overrides MODEL in DSSATPRO 
!  05/09/2007 CHP Make Sulieman-Ritchie the default soil evaporation method
!  04/28/2008 CHP Added switch for CO2 from file (ICO2)
C-----------------------------------------------------------------------
C  INPUT  : LUNEXP,FILEX,LNSIM
C
C  LOCAL  : LN
C
C  OUTPUT : NYRS,NREPSQ,ISWWAT,ISWNIT,ISWSYM,ISWPHO,ISWPOT,ISWDIS,MEWTH,
C           MESIC,MELI,MEEVP,MEINF,MEPHO,ISIMI,ISIM,IPLTI,IIRRI,IFERI,
C           IRESI,IHARI,IOX,IDETO,IDETS,IDETG,IDETC,IDETW,IDETN,IDETP,IDETD,
C           PWDINF,PWDINL,SWPLTL,SWPLTH,SWPLTD,PTX,PTTN,DSOILX,THETACX,
C           IEPTX,IOFFX,IAMEX,DSOILN,SOILNC,SOILNX,NEND,RIP,NRESDL,
C           DRESMG,HDLAY,HLATE
!           MESOM, METMP, MESOL, MESEV
C-----------------------------------------------------------------------
C  Called : IPEXP
C
C  Calls  : ERROR IGNORE FIND YR_DOY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IPSIM (LUNEXP,LNSIM,TITSIM,NYRS,RUN,NREPSQ,
     & ISIMI,PWDINF,PWDINL,SWPLTL,NCODE,SWPLTH,SWPLTD,YEAR,
     & PTX,PTTN,DSOIL,THETAC,IEPT,IOFF,IAME,DSOILN,SOILNC,YRSIM,
     & SOILNX,NEND,RIP,NRESDL,DRESMG,HDLAY,HLATE,HPP,HRP,FTYPEN,
     & RSEED1,LINEXP,AIRAMT,EFFIRR,CROP,FROP,MODEL,RNMODE,FILEX,
     & CONTROL, ISWITCH, UseSimCtr, FILECTL, MODELARG, YRPLT)

      USE ModuleDefs
      USE ModuleData
      IMPLICIT NONE
      SAVE

      INCLUDE 'COMSWI.BLK'

      CHARACTER*1   UPCASE,ISIMI, RNMODE
      CHARACTER*2   CROP
      CHARACTER*5   NEND,NCODE,IOFF,IAME
      CHARACTER*6   ERRKEY,FINDCH
      CHARACTER*8   MODEL, MODELARG, CRMODEL, TRY_MODEL
      CHARACTER*12  FILEX
      CHARACTER*25  TITSIM
      CHARACTER*78  MSG(7)
      CHARACTER*120 FILECTL
      CHARACTER*128 CHARTEST

      INTEGER LNSIM,LUNEXP,ISECT,LINEXP,ISIM,NYRS,NREPSQ,FROP
      INTEGER PLDATE,PWDINF,PWDINL,HLATE,HDLAY,NRESDL
      INTEGER IFIND,LN,ERRNUM,FTYPEN,YRSIM,YEAR,RUN,RSEED1,RRSEED1
      INTEGER YRPLT

      REAL DSOIL,THETAC,DSOILN,SOILNC,SOILNX,SWPLTL,SWPLTH,SWPLTD
      REAL PTX,PTTN,DRESMG,RIP,IEPT,HPP,HRP,AIRAMT,EFFIRR

      LOGICAL UseSimCtr, MulchWarn

      TYPE (SwitchType)  ISWITCH
      TYPE (ControlType) CONTROL

      PARAMETER (ERRKEY='IPSIM ')
                 FINDCH='*SIMUL'
                 
      DATA MulchWarn /.FALSE./

      IF (LNSIM .EQ. 0) THEN
         LNSIM   = 0
         NYRS    = 1
         NREPSQ  = 1
         ISIMI   = 'S'
         YRSIM   = -99
         RSEED1  = 2150
         ISWWAT  = 'Y'
         ISWNIT  = 'Y'
         ISWSYM  = 'Y'
         ISWPHO  = 'N'
         ISWPOT  = 'N'
         ISWDIS  = 'N'
         ISWCHE  = 'N'
         ISWTIL  = 'Y'

         IF (INDEX('FNQS',RNMODE) > 0) THEN
           ICO2 = 'D' !Default CO2 from CO2045.WDA file
         ELSE
           ICO2 = 'M' !Measured CO2 from CO2045.WDA file
         ENDIF

         MEWTH   = 'M'
         MESIC   = 'M'
         MELI    = 'E'
         MEEVP   = 'R'
         MEINF   = 'S'
         MEPHO   = 'L'
         MEHYD   = 'R'
         NSWITCH =  1
         MESOM   = 'G'
!        METMP   = 'E'
         MESOL   = '2'    !was '1'
         MESEV   = 'S'    !new Sulieman-Ritchie (2006)
         IPLTI   = 'R'
         IIRRI   = 'R'
         IFERI   = 'R'
         IRESI   = 'R'
         IHARI   = 'M'
         IOX     = 'N'
         FROP    =  3
         IDETO   = 'Y'
         IDETS   = 'Y'
         IDETG   = 'Y'
         IDETN   = 'N'
         IDETC   = 'N'
         IDETW   = 'N'
         IDETP   = 'N'
         IDETD   = 'N'
         IDETL   = 'N'
         IDETH   = 'N'
         IDETR   = 'Y'
         EFFIRR  = 1.00
         THETAC  = 75.0
         IEPT    = 100.0
         DSOIL   = 30.0
         DSOILN  = 30.0
         AIRAMT  = 10.0
         IOFF    = 'GS000'
         IAME    = 'IR001'
         CRMODEL = '        '
         NCODE = "-99  "
         NEND  = "-99  "
       ELSE
 40      CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
         IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)
 50      CALL IGNORE(LUNEXP,LINEXP,ISECT,CHARTEST)
         IF (ISECT .EQ. 1) THEN
            READ (CHARTEST,55,IOSTAT=ERRNUM) LN
            IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEX,LINEXP)
            IF (LN .NE. LNSIM) GO TO 50
            READ (CHARTEST,55,IOSTAT=ERRNUM) LN,NYRS,NREPSQ,ISIMI,
     &            YRSIM,RRSEED1,TITSIM,CRMODEL
            IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEX,LINEXP)
            IF (INDEX('G',RNMODE) .GT. 0) NYRS = 1
            IF ((RNMODE .NE. 'Q') .OR. (RNMODE .EQ. 'Q'
     &       .AND. RUN .EQ. 1)) THEN
               RSEED1 = RRSEED1
               IF (RSEED1 .LE. 0) THEN
                 RSEED1 = 2150
               ENDIF
            ENDIF
            CALL Y2K_DOY (YRSIM)
            CALL YR_DOY (YRSIM,YEAR,ISIM)
          ELSE
            BACKSPACE (LUNEXP)
            GO TO 40
         ENDIF
C
C        Read SECOND line of simulation control
C
         CALL IGNORE(LUNEXP,LINEXP,ISECT,CHARTEST)
         READ (CHARTEST,60,IOSTAT=ERRNUM) LN,ISWWAT,ISWNIT,ISWSYM,
     &        ISWPHO,ISWPOT,ISWDIS,ISWCHE,ISWTIL, ICO2
         IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEX,LINEXP)

         ISWWAT = UPCASE(ISWWAT)
         ISWNIT = UPCASE(ISWNIT)
         ISWSYM = UPCASE(ISWSYM)
         ISWPHO = UPCASE(ISWPHO)
         ISWPOT = UPCASE(ISWPOT)
         ISWDIS = UPCASE(ISWDIS)
         ISWCHE = UPCASE(ISWCHE)
         ISWTIL = UPCASE(ISWTIL)
         ICO2   = UPCASE(ICO2)

!        IF (INDEX ('BNSBPNPECHPPVBCPCBFB',CROP) .EQ. 0) THEN
         SELECT CASE (CROP)
         CASE ('BN','SB','PN','PE','CH','PP',
     &          'VB','CP','CB','FB','GB','LT')
!          Do nothing -- CROPGRO crops can have Y or N
         CASE DEFAULT; ISWSYM = 'N'  !other crops don't have a choice
         END SELECT
!        ENDIF
         IF (ISWCHE .EQ. ' ') THEN
            ISWCHE = 'N'
         ENDIF
         IF (ISWTIL .EQ. ' ') THEN
            ISWTIL = 'N'
         ENDIF
         IF (ISWWAT .EQ. 'N') THEN
            IF (ISWNIT .EQ. 'Y') ISWNIT = 'N'
            IF (ISWPHO .EQ. 'Y') ISWPHO = 'N'
!            IF (ISWTIL .EQ. 'Y') ISWTIL = 'N'
            IF (ISWCHE .EQ. 'Y') ISWCHE = 'N'
         ENDIF

         IF (INDEX('FNQS',RNMODE) > 0) THEN
           IF (INDEX ('WMD', ICO2) < 1) ICO2 = 'D'
         ELSE
           IF (INDEX ('WMD', ICO2) < 1) ICO2 = 'M'
         ENDIF

C        Read THIRD line of simulation control
C
         CALL IGNORE(LUNEXP,LINEXP,ISECT,CHARTEST)
         READ (CHARTEST,61,IOSTAT=ERRNUM) LN,MEWTH,MESIC,
     &        MELI,MEEVP,MEINF,MEPHO,MEHYD,NSWITCH, 
     &        MESOM, MESEV, MESOL   !, METMP
         !IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEX,LINEXP)
         MEWTH = UPCASE(MEWTH)
         MESIC = UPCASE(MESIC)
         MELI  = UPCASE(MELI)
         MEEVP = UPCASE(MEEVP)
         MEINF = UPCASE(MEINF)
         MEPHO = UPCASE(MEPHO)
         MESOM = UPCASE(MESOM)
         MEHYD = UPCASE(MEHYD)
         MESEV = UPCASE(MESEV)
!        METMP = UPCASE(METMP)

         IF (INDEX('PG',MESOM) .EQ. 0) THEN
            MESOM = 'G'
         ENDIF
         
         IF (INDEX('G',MESOM)   > 0 .AND. 
     &       INDEX('FQ',RNMODE) > 0 .AND. 
     &       INDEX('N',MEINF)  == 0) THEN
           MEINF = 'N'
           IF (.NOT. MulchWarn) THEN
             MSG(1)=
     &  "Long-term simulation of surface residues may not be accurate"
             MSG(2)=
     &  "when using Godwin soil organic matter module.  The effects of"
             MSG(3)=
     &  "a surface mulch layer on runoff and evaporation will " //
     &       "not be modeled."  
             MSG(4)=
     &  "Simulation Options/Methods/Infiltration = 'No mulch effects'"
             MSG(5)=
     &  "You may want to consider using the Parton (CENTURY) method of"
             MSG(6)= "modeling soil organic matter."
             CALL WARNING(6,ERRKEY,MSG)
             MulchWarn = .TRUE.
           ENDIF
         ENDIF

         IF (INDEX('123',MESOL) < 1) THEN
            MESOL = '2'
         ENDIF

!        Default soil temperature method is original w/ bugfix
!        IF (INDEX('EPSO',METMP) < 1) METMP = 'E'

         SELECT CASE(MESEV)
         CASE('R','r'); MESEV = 'R'
         CASE DEFAULT;  MESEV = 'S'   !Default method -- use NEW
         END SELECT

         IF (MEEVP == 'Z' .AND. MEPHO /= 'L') CALL ERROR(ERRKEY,3,' ',0)

         IF (MEHYD .EQ. ' ') THEN
            MEHYD = 'R'
         ENDIF

         IF (NSWITCH .LE. 0 .AND. ISWNIT .EQ. 'Y') THEN
           NSWITCH = 1
         ENDIF
C
C        Read FOURTH line of simulation control
C
         CALL IGNORE(LUNEXP,LINEXP,ISECT,CHARTEST)
         READ (CHARTEST,60,IOSTAT=ERRNUM) LN,IPLTI,IIRRI,
     &        IFERI,IRESI,IHARI
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
         IPLTI = UPCASE(IPLTI)
         IIRRI = UPCASE(IIRRI)
         IFERI = UPCASE(IFERI)
         IRESI = UPCASE(IRESI)
         IHARI = UPCASE(IHARI)

         IF ((INDEX('CSPT',CROP)) .GT. 0) THEN
           IF (IHARI .EQ. 'A') THEN
              CALL ERROR (ERRKEY,4,FILEX,LINEXP)
           ENDIF
         ENDIF

         IF ((INDEX('PT',CROP)) .GT. 0) THEN
           IF (IPLTI .EQ. 'A') THEN
              CALL ERROR (ERRKEY,5,FILEX,LINEXP)
           ENDIF
         ENDIF

C
C        Read FIFTH line of simulation control
C
         CALL IGNORE(LUNEXP,LINEXP,ISECT,CHARTEST)
         IF (INDEX('FQ',RNMODE) < 1 .OR. RUN == 1) THEN
            READ (CHARTEST,65,IOSTAT=ERRNUM) LN,IOX,IDETO,
     &      IDETS,FROP,IDETG,IDETC,IDETW,IDETN,IDETP,IDETD,
     &      IDETL,IDETH,IDETR
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            IOX   = UPCASE(IOX)
            IDETO = UPCASE(IDETO)
            IDETS = UPCASE(IDETS)
            IDETG = UPCASE(IDETG)
            IDETC = UPCASE(IDETC)
            IDETW = UPCASE(IDETW)
            IDETN = UPCASE(IDETN)
            IDETP = UPCASE(IDETP)
            IDETD = UPCASE(IDETD)
            IF (IDETL .EQ. ' ') THEN
               IDETL = 'N'
            ENDIF
            IDETL = UPCASE(IDETL)
            IF (IDETH .EQ. ' ') THEN
               IDETH = 'N'
            ENDIF
            IDETH = UPCASE(IDETH)
            IF (IDETR .EQ. ' ') THEN
               IDETR = 'Y'
            ENDIF
            IDETR = UPCASE(IDETR)
!           VBOSE = zero, suppress all output except SUMMARY.OUT
            IF (IDETL == '0') THEN
              IDETO = 'N'
              IDETG = 'N' 
              IDETC = 'N' 
              IDETW = 'N' 
              IDETN = 'N' 
              IDETP = 'N' 
              IDETD = 'N' 
              IDETH = 'N' 
              IDETR = 'N' 
            ENDIF
            IF (FROP .LE. 0) FROP = 10
         ENDIF
C
C        Read SIXTH line of simulation control
C
         CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
         IF (ISECT .EQ. 1) THEN
            READ (CHARTEST,66,IOSTAT=ERRNUM) LN,PWDINF,PWDINL,
     &           SWPLTL,SWPLTH,SWPLTD,PTX,PTTN
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            IF (PWDINF .LT. 1000) PWDINF = YEAR * 1000 + PWDINF
            IF (PWDINL .LT. 1000) PWDINL = YEAR * 1000 + PWDINL
            CALL Y2K_DOY (PWDINF)
            CALL Y2K_DOY (PWDINL)
C
C           Read SEVENTH line of simulation control
C
            CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
            READ (CHARTEST,67,IOSTAT=ERRNUM) LN,DSOIL,THETAC,
     &           IEPT,IOFF,IAME,AIRAMT,EFFIRR
            IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEX,LINEXP)
C
C           Read EIGHTH line of simulation control
C
            CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
            READ (CHARTEST,67,IOSTAT=ERRNUM) LN,DSOILN,SOILNC,
     &           SOILNX,NCODE,NEND
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            READ (NCODE,70,IOSTAT=ERRNUM) FTYPEN
C
C           Read NINTH line of simulation control
C
            CALL IGNORE(LUNEXP,LINEXP,ISECT,CHARTEST)
            READ (CHARTEST,68,IOSTAT=ERRNUM) LN,RIP,NRESDL,DRESMG
            IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEX,LINEXP)
C
C           Read TENTH line of simulation control
C
            CALL IGNORE(LUNEXP,LINEXP,ISECT,CHARTEST)
            READ (CHARTEST,66,IOSTAT=ERRNUM) LN,HDLAY,HLATE,
     &           HPP,HRP
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            CALL Y2K_DOY (HLATE)
            IF (HPP   .LT. 0.0)  HPP   = 100.
            IF (HRP   .LT. 0.0)  HRP   = 0.0
          ELSE
            PWDINF  =   1
            PWDINL  =   366
            SWPLTL  =   1.0
            SWPLTH  =   100.0
            SWPLTD  =   200.0
            PTX     =   50.0
            PTTN    =   1.0
            DSOIL   =   200.0
            THETAC  =   10.0
            IEPT    =   100.0
            IOFF    =   ' '
            HPP     =   100.0
            HRP     =     0.0
         ENDIF
      ENDIF

      REWIND (LUNEXP)

C-----------------------------------------------------------------------
C    Select Model Name and Path -- order of priority:
!     CTRMODEL is value from control file override -- this is used
!         over all other values if valid. (Done in Default_SimControls)
!     CRMODEL is read from FILEX.  Use this if no control file.  
!     MODELARG is from command line argument list. Third priority. 
!     Last, use value from DSSATPRO.v45.
C-----------------------------------------------------------------------
!     First check model name from FILEX
      TRY_MODEL = CRMODEL
      CALL MODEL_NAME (CROP, DSSATP, TRY_MODEL, MODEL)

!     If FILEX model name was not acceptable, then try the 
!       model name read from command line.  If this is not OK, 
!       MODEL contains value from DSSATPRO.V45
      IF (TRY_MODEL /= MODEL) THEN
        CALL MODEL_NAME (CROP, DSSATP, MODELARG, MODEL)
      ENDIF

      IF (MEPHO .EQ. 'L' .AND. MODEL(1:5) .NE. 'CRGRO') THEN
        MEPHO = 'C'
        WRITE(MSG(1),80)
        WRITE (MSG(2),81) MODEL(1:5)
        CALL WARNING(2, "IPEXP ", MSG)

   80 FORMAT('Photosynthesis method (PHOTO in FILEX) has been changed')
   81 FORMAT('from "L" to "C" for compatibility with crop model, '
     &            ,A5,'.') 
      ENDIF

      CALL FILL_ISWITCH(
     &      CONTROL, ISWITCH, FROP, MODEL, NYRS, RNMODE)

!     Planting date needed for generic start of simulation
      SELECT CASE(IPLTI)
      CASE('R'); PLDATE = YRPLT
      CASE('A'); PLDATE = PWDINF
      END SELECT

!     Check Simulation control file for control overrides 
      CALL Default_SimControls(
     &    CONTROL, DSSATP, FILECTL, ISWITCH, MODELARG, PLDATE, !Input
     &    UseSimCtr, CRMODEL, MODEL)                           !Output

      IF (UseSimCtr) THEN
        IOX     = ISWITCH % FNAME 
        ISIMI   = ISWITCH % ISIMI 
        ISWWAT  = ISWITCH % ISWWAT
        ISWNIT  = ISWITCH % ISWNIT
        ISWSYM  = ISWITCH % ISWSYM
        ISWPHO  = ISWITCH % ISWPHO
        ISWPOT  = ISWITCH % ISWPOT
        ISWDIS  = ISWITCH % ISWDIS
        ISWCHE  = ISWITCH % ISWCHE
        ISWTIL  = ISWITCH % ISWTIL
        ICO2    = ISWITCH % ICO2
        MEWTH   = ISWITCH % MEWTH 
        MESOM   = ISWITCH % MESOM 
        MELI    = ISWITCH % MELI  
        MEEVP   = ISWITCH % MEEVP 
        MEINF   = ISWITCH % MEINF 
        MEPHO   = ISWITCH % MEPHO 
        MEHYD   = ISWITCH % MEHYD 
        MESEV   = ISWITCH % MESEV 
        MESOL   = ISWITCH % MESOL 
!       METMP   = ISWITCH % METMP 
        IPLTI   = ISWITCH % IPLTI 
        IIRRI   = ISWITCH % IIRRI 
        IFERI   = ISWITCH % IFERI 
        IRESI   = ISWITCH % IRESI 
        IHARI   = ISWITCH % IHARI 
        IDETO   = ISWITCH % IDETO 
        IDETS   = ISWITCH % IDETS 
        IDETG   = ISWITCH % IDETG 
        IDETC   = ISWITCH % IDETC 
        IDETW   = ISWITCH % IDETW 
        IDETN   = ISWITCH % IDETN 
        IDETP   = ISWITCH % IDETP 
        IDETD   = ISWITCH % IDETD 
        IDETL   = ISWITCH % IDETL 
        IDETH   = ISWITCH % IDETH 
        IDETR   = ISWITCH % IDETR 
        NSWITCH = ISWITCH % NSWI     
      
        NYRS  = CONTROL % NYRS  
        YRSIM = CONTROL % YRSIM 
        MODEL = CONTROL % MODEL 
        MESIC = CONTROL % MESIC     
        FROP  = CONTROL % FROP

      ENDIF

      CALL PUT(CONTROL)  
      CALL PUT(ISWITCH)
      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

  55  FORMAT (I3,11X,2(1X,I5),5X,A1,1X,I5,1X,I5,1X,A25,1X,A8)
  60  FORMAT (I3,11X,9(5X,A1))
  61  FORMAT (I3,11X,7(5X,A1),5X,I1,4(5X,A1))
  65  FORMAT (I3,11X,3(5X,A1),4X,I2,9(5X,A1))
  66  FORMAT (I3,11X,2(1X,I5),5(1X,F5.0))
  67  FORMAT (I3,11X,3(1X,F5.0),2(1X,A5),1X,F5.0,1X,F5.0)
  68  FORMAT (I3,11X,1X,F5.0,1X,I5,1X,F5.0)
  70  FORMAT (3X,I2)

      END SUBROUTINE IPSIM

!=======================================================================
!  FILL_ISWITCH, Subroutine
!
!  Copies values to ISWITCH variable
!-----------------------------------------------------------------------
!  Revision history
!  10/26/2007 CHP Written
!-----------------------------------------------------------------------
!  Called : IPSIM
!  Calls  : none
!=======================================================================
      SUBROUTINE FILL_ISWITCH(
     &      CONTROL, ISWITCH, FROP, MODEL, NYRS, RNMODE)
      USE ModuleDefs 
      USE ModuleData
      INCLUDE 'COMSWI.BLK'
      INCLUDE 'COMIBS.BLK'

      CHARACTER*1 RNMODE
      CHARACTER*8 MODEL
      INTEGER FROP, NYRS
      
      TYPE (SwitchType) ISWITCH
      TYPE (ControlType)CONTROL

!     Skip some variables for sequenced runs -- need to keep values
!     from first run
      IF (INDEX('FQ',RNMODE) <= 0 .OR. CONTROL % RUN == 1) THEN
        ISWITCH % ISWWAT = ISWWAT
        ISWITCH % ISWNIT = ISWNIT
        ISWITCH % ISWSYM = ISWSYM
        ISWITCH % ISWPHO = ISWPHO
        ISWITCH % ISWPOT = ISWPOT
        ISWITCH % ISWDIS = ISWDIS
        ISWITCH % ISWCHE = ISWCHE
        ISWITCH % ISWTIL = ISWTIL
        ISWITCH % ICO2   = ICO2
        ISWITCH % MEWTH  = MEWTH
        ISWITCH % MESOM  = MESOM
        ISWITCH % MEINF  = MEINF
        ISWITCH % MEPHO  = MEPHO
        ISWITCH % MEHYD  = MEHYD
        ISWITCH % MESEV  = MESEV
        ISWITCH % MESOL  = MESOL
!       ISWITCH % METMP  = METMP
        ISWITCH % IDETO  = IDETO
        ISWITCH % IDETS  = IDETS
        ISWITCH % IDETG  = IDETG
        ISWITCH % IDETC  = IDETC
        ISWITCH % IDETW  = IDETW
        ISWITCH % IDETN  = IDETN
        ISWITCH % IDETP  = IDETP
        ISWITCH % IDETD  = IDETD
        ISWITCH % IDETL  = IDETL
        ISWITCH % IDETH  = IDETH
        ISWITCH % IDETR  = IDETR
        ISWITCH % NSWI   = NSWITCH
        CONTROL % NYRS   = NYRS   
        CONTROL % FROP   = FROP   
      ENDIF
 
      ISWITCH % MEEVP  = MEEVP
      ISWITCH % FNAME  = IOX 
      ISWITCH % ISIMI  = ISIMI 
      ISWITCH % MELI   = MELI 
      ISWITCH % IPLTI  = IPLTI
      ISWITCH % IIRRI  = IIRRI
      ISWITCH % IFERI  = IFERI
      ISWITCH % IRESI  = IRESI
      ISWITCH % IHARI  = IHARI
    
      CONTROL % YRSIM  = YRSIM  
      CONTROL % MODEL  = MODEL
      CONTROL % MESIC  = MESIC  

      CALL PUT(ISWITCH)
      CALL PUT(CONTROL)

      RETURN
      END SUBROUTINE FILL_ISWITCH
!=======================================================================


!=======================================================================
!  Default_SimControls, Subroutine
!
!  Reads default simulation controls 
!-----------------------------------------------------------------------
!  Revision history
!  06/29/2007 CHP Written
!-----------------------------------------------------------------------
!  Called : IPSIM
!
!  Calls  : ERROR IGNORE FIND YR_DOY
!=======================================================================

      SUBROUTINE Default_SimControls(
     &    CONTROL, DSSATP, FILECTL, ISWITCH, MODELARG, PLDATE, !Input
     &    UseSimCtr, CRMODEL, MODEL)                           !Output

      USE ModuleDefs
      IMPLICIT NONE
      SAVE

      CHARACTER*1 UPCASE,ISIMI, MEPHO_SAVE, ISWSYM_SAVE
      CHARACTER*1 ISWWAT,ISWNIT,ISWSYM,ISWPHO,ISWPOT,ISWDIS,MEWTH,MESIC
      CHARACTER*1 ICO2
      CHARACTER*1 MELI,MEEVP,MEINF,MEPHO,IPLTI,IIRRI,IFERI,IRESI,IHARI
      CHARACTER*1 ISWCHE,ISWTIL,MEHYD,MESOM, MESOL, MESEV   !, METMP
      CHARACTER*1 IDETO,IDETS,IDETG,IDETC,IDETW,IDETN,IDETP,IDETD,IOX
      CHARACTER*1 IDETH,IDETL, IDETR
      CHARACTER*6 ERRKEY,FINDCH, SECTION
      CHARACTER*8 MODEL, CRMODEL, CTRMODEL, MODELARG, TRY_MODEL
      CHARACTER*12 FILEX  !, DSSATS
      CHARACTER*78 MSG(50)
      CHARACTER*102 DSSATP, SIMCTR
      CHARACTER*120 INPUTX, FILECTL
      CHARACTER*128 CHARTEST

      INTEGER CTRNO, ERRNUM, FOUND, FROP, I, IFIND, IPX, ISECT, ISIM
      INTEGER LEVEL, LINEXP, NMSG, NREPSQ, NSWITCH, NYRS
      INTEGER PLDATE, RSEED1, SCLun, YEAR, YRSIM
      INTEGER SimLen, LenString, FIND_IN_FILE

      TYPE (SwitchType)  ISWITCH
      TYPE (ControlType) CONTROL

      LOGICAL FIRST, FEXIST, UseSimCtr
      DATA FIRST /.TRUE./

      PARAMETER (ERRKEY = 'SIMCTR')
!      PARAMETER (DSSATS = 'DSCSM045.CTR')

!-----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        UseSimCtr = .FALSE.

        IF (LEN(TRIM(FILECTL)) < 4 .OR. INDEX(FILECTL," ") < 4) RETURN

        I = INDEX(FILECTL,SLASH)
        IF (I < 1) THEN
!         No path provided -- look first in DSSAT45 directory
          CALL GETARG (0,INPUTX)      !Name of model executable
          IPX = LEN_TRIM(INPUTX)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   Temporarily fix DSSATPRO file name for debugging purposes:
!     To use these Debug lines of code (letter D in column 1) with CVF:
!     1) Go to pull down menu Project -> Settings -> Fortran (Tab) ->
!       Debug (Category) -> Check box for Compile Debug(D) Lines
!     2)  Specify name of DSSATPRO file here:
D     INPUTX = 'C:\DSSAT45\DSCSM045.EXE'
D     IPX = 22
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          IF (IPX > 12) THEN
            DO I = IPX, 0, -1
              IF (INPUTX(I:I) .EQ. SLASH) EXIT
            END DO
            SIMCTR = INPUTX(1:I) // FILECTL
          ELSE
            RETURN
          ENDIF
        ELSE
          SIMCTR = FILECTL
        ENDIF

        INQUIRE (FILE = SIMCTR, EXIST = FEXIST)
        IF (.NOT. FEXIST) THEN
          MSG(1) = "Simulation controls file does not exist."
          MSG(2) = SIMCTR
          MSG(3) = "Use controls from experiment file."
          CALL WARNING(3,ERRKEY,MSG)
          RETURN
        ENDIF

        CALL GETLUN('SIMCNTL', SCLun)
        OPEN (UNIT = SCLun, FILE = SIMCTR, IOSTAT =ERRNUM)
        IF (ERRNUM /= 0) RETURN

        SECTION = '@CTRNO'
        FOUND = FIND_IN_FILE(SECTION,SCLun)
        IF (FOUND == 1) THEN
          READ(SCLun,"(I6)",IOSTAT=ERRNUM) CTRNO
          IF (ERRNUM > 0) THEN
            MSG(1) = "Error reading simulation control file."
            WRITE(MSG(2),'(A,A)') "File: ",SIMCTR(1:72)
            CALL WARNING(2, ERRKEY, MSG)
            RETURN
          ENDIF
          IF (CTRNO < 1) THEN
            MSG(1) = "External control file:"
            WRITE(MSG(2),'(A,A)') "  ", SIMCTR(1:76)
            WRITE(MSG(3),'(A,I6,A)') "Control level: ", CTRNO, 
     &        "    No external controls will be used."
            CALL WARNING(3, ERRKEY, MSG)
            RETURN
          ENDIF
        ELSE
          MSG(1) = "Error reading simulation control file."
          WRITE(MSG(2),'(A,A)') "File: ",SIMCTR(1:72)
          WRITE(MSG(3),'(A,A)') "Control section not found: ", SECTION
          CALL WARNING(3, ERRKEY, MSG)
          RETURN
        ENDIF

        CONTROL % SimControl = SIMCTR
        SimLen = LenString(CONTROL % SimControl)
        MSG(1) = "Simulation Controls override with file:"
        WRITE(MSG(2),'(A)') CONTROL % SimControl(1:SimLen)
        WRITE(MSG(3),'(A,I6)') "Control Level: ", CTRNO
        MSG(4) = "The following switches and options will override"
        MSG(5) = "  values found in the Experiment files:"
        NMSG = 5

!       Initialize override values
        NYRS    = -99
        NREPSQ  = -99
        ISIMI   = ' '
        YRSIM   = -99
        RSEED1  = -99

        ISWWAT  = ' '
        ISWNIT  = ' '
        NSWITCH = -99
        ISWSYM  = ' '
        ISWPHO  = ' '
        ISWPOT  = ' '
        ISWDIS  = ' '
        ISWCHE  = ' '
        ISWTIL  = ' '

        ICO2    = ' '
        MEWTH   = ' '
        MESIC   = ' '
        MELI    = ' '
        MEEVP   = ' '
        MEINF   = ' '
        MEPHO   = ' '
        MEHYD   = ' '
        NSWITCH = -99
        MESOM   = ' '
!       METMP   = ' '
        MESOL   = ' '
        MESEV   = ' '
        IPLTI   = ' '
        IIRRI   = ' '
        IFERI   = ' '
        IRESI   = ' '
        IHARI   = ' '
        IOX     = ' '
        FROP    = -99
        IDETO   = ' '
        IDETS   = ' '
        IDETG   = ' '
        IDETN   = ' '
        IDETC   = ' '
        IDETW   = ' '
        IDETP   = ' '
        IDETD   = ' '
        IDETL   = ' '
        IDETH   = ' '
        IDETR   = ' '
        CRMODEL = '     '

!       Read FIRST line of simulation control
        REWIND(SCLun)

        LEVEL = 0
        FINDCH = '@N CON'
        DO WHILE (LEVEL /= CTRNO)
          CALL FIND (SCLun,FINDCH,LINEXP,IFIND)
          IF (IFIND == 1) THEN
!           Found a good section
            CALL IGNORE(SCLun,LINEXP,ISECT,CHARTEST)
            IF (ISECT == 1) THEN
              READ (CHARTEST,'(I2)',IOSTAT=ERRNUM) LEVEL
              IF (ERRNUM /= 0) RETURN
            ENDIF
          ELSE
            RETURN
          ENDIF
        ENDDO
          
!       Read simulation controls
        DO WHILE (ERRNUM == 0)
          CALL IGNORE2(SCLun,LINEXP,ISECT,CHARTEST)
          IF (ISECT == 0) EXIT
          SELECT CASE(CHARTEST(1:6))

!         First line of simulation controls
          CASE('@N GEN')
            CALL IGNORE(SCLun,LINEXP,ISECT,CHARTEST)
            READ (CHARTEST,'(I2)',IOSTAT=ERRNUM) LEVEL
            IF (ERRNUM /= 0) EXIT
            IF (LEVEL /= CTRNO) EXIT 

!           READ (CHARTEST,55,IOSTAT=ERRNUM) NYRS,NREPSQ,ISIMI,
!     &           YRSIM,RRSEED1,TITSIM,CRMODEL
!  55       FORMAT (14X,2(1X,I5),5X,A1,1X,I5,1X,I5,1X,A25,1X,A8)

            READ (CHARTEST,'(15X,I5)',IOSTAT=ERRNUM) NYRS
            CALL CHECK_I('NYRS', NYRS, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(21X,I5)',IOSTAT=ERRNUM) NREPSQ
            CALL CHECK_I('NREPSQ', NREPSQ, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(31X,A1)',IOSTAT=ERRNUM) ISIMI
            CALL CHECK_A('ISIMI', ISIMI, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(33X,I5)',IOSTAT=ERRNUM) YRSIM
            IF (YRSIM == -99) YRSIM = PLDATE
            CALL CHECK_I('YRSIM', YRSIM, ERRNUM, MSG, NMSG)
            IF (ERRNUM == 0) THEN
              CALL Y2K_DOY (YRSIM)
              CALL YR_DOY (YRSIM,YEAR,ISIM)
            ENDIF

!            READ (CHARTEST,'(39X,I5)',IOSTAT=ERRNUM) RSEED1
!            CALL CHECK_I(ERRNUM, 'NYRS', NYRS, MSG, NMSG)

            READ (CHARTEST,'(71X,A8)',IOSTAT=ERRNUM) CTRMODEL
            CALL CHECK_A('CTRMODEL', CTRMODEL, ERRNUM, MSG, NMSG)

!         Second line of simulation controls
          CASE('@N OPT')
            CALL IGNORE(SCLun,LINEXP,ISECT,CHARTEST)
            READ (CHARTEST,'(I2)',IOSTAT=ERRNUM) LEVEL
            IF (ERRNUM /= 0) EXIT
            IF (LEVEL /= CTRNO) EXIT 

!           READ (CHARTEST,60,IOSTAT=ERRNUM) LN,ISWWAT,ISWNIT,ISWSYM,
!     &         ISWPHO,ISWPOT,ISWDIS,ISWCHE,ISWTIL, ISWFWT
!  60       FORMAT (I3,11X,9(5X,A1))

            READ (CHARTEST,'(19X,A1)',IOSTAT=ERRNUM) ISWWAT
            CALL CHECK_A('ISWWAT', ISWWAT, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(25X,A1)',IOSTAT=ERRNUM) ISWNIT
            CALL CHECK_A('ISWNIT', ISWNIT, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(31X,A1)',IOSTAT=ERRNUM) ISWSYM
            CALL CHECK_A('ISWSYM', ISWSYM, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(37X,A1)',IOSTAT=ERRNUM) ISWPHO
            CALL CHECK_A('ISWPHO', ISWPHO, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(43X,A1)',IOSTAT=ERRNUM) ISWPOT
            CALL CHECK_A('ISWPOT', ISWPOT, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(49X,A1)',IOSTAT=ERRNUM) ISWDIS
            CALL CHECK_A('ISWDIS', ISWDIS, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(55X,A1)',IOSTAT=ERRNUM) ISWCHE
            CALL CHECK_A('ISWCHE', ISWCHE, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(61X,A1)',IOSTAT=ERRNUM) ISWTIL
            CALL CHECK_A('ISWTIL', ISWTIL, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(67X,A1)',IOSTAT=ERRNUM) ICO2
            CALL CHECK_A('ICO2  ', ICO2, ERRNUM, MSG, NMSG)

            ISWWAT = UPCASE(ISWWAT)
            ISWNIT = UPCASE(ISWNIT)
            ISWSYM = UPCASE(ISWSYM)
            ISWPHO = UPCASE(ISWPHO)
            ISWPOT = UPCASE(ISWPOT)
            ISWDIS = UPCASE(ISWDIS)
            ISWCHE = UPCASE(ISWCHE)
            ISWTIL = UPCASE(ISWTIL)
            ICO2   = UPCASE(ICO2)

            IF (ISWWAT .EQ. 'N') THEN
              ISWNIT = 'N'
              ISWCHE = 'N'
            ENDIF

            IF (ISWNIT .EQ. 'N') ISWPHO = 'N'
            IF (ISWNIT .EQ. 'N') ISWPOT = 'N'

!         Third line of simulation controls
          CASE('@N MET')
            CALL IGNORE(SCLun,LINEXP,ISECT,CHARTEST)
            READ (CHARTEST,'(I2)',IOSTAT=ERRNUM) LEVEL
            IF (ERRNUM /= 0) EXIT
            IF (LEVEL /= CTRNO) EXIT 

!           READ (CHARTEST,61,IOSTAT=ERRNUM) LN,MEWTH,MESIC,
!    &           MELI,MEEVP,MEINF,MEPHO,MEHYD,NSWITCH, 
!    &           MESOM, MESEV, MESOL, METMP
! 61        FORMAT (I3,11X,7(5X,A1),5X,I1,5X,A1,2(5X,A1),5X,I1)

            READ (CHARTEST,'(19X,A1)',IOSTAT=ERRNUM) MEWTH
            CALL CHECK_A('MEWTH', MEWTH, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(25X,A1)',IOSTAT=ERRNUM) MESIC
            CALL CHECK_A('MESIC', MESIC, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(31X,A1)',IOSTAT=ERRNUM) MELI
            CALL CHECK_A('MELI', MELI, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(37X,A1)',IOSTAT=ERRNUM) MEEVP
            CALL CHECK_A('MEEVP', MEEVP, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(43X,A1)',IOSTAT=ERRNUM) MEINF
            CALL CHECK_A('MEINF', MEINF, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(49X,A1)',IOSTAT=ERRNUM) MEPHO
            CALL CHECK_A('MEPHO', MEPHO, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(55X,A1)',IOSTAT=ERRNUM) MEHYD
            CALL CHECK_A('MEHYD', MEHYD, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(61X,I1)',IOSTAT=ERRNUM) NSWITCH
            CALL CHECK_I('NSWITCH', NSWITCH, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(67X,A1)',IOSTAT=ERRNUM) MESOM
            CALL CHECK_A('MESOM', MESOM, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(73X,A1)',IOSTAT=ERRNUM) MESEV
            CALL CHECK_A('MESEV', MESEV, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(79X,A1)',IOSTAT=ERRNUM) MESOL
            CALL CHECK_A('MESOL', MESOL, ERRNUM, MSG, NMSG)

!           READ (CHARTEST,'(85X,A1)',IOSTAT=ERRNUM) METMP
!           CALL CHECK_A('METMP', METMP, ERRNUM, MSG, NMSG)

            MEWTH = UPCASE(MEWTH)
            MESIC = UPCASE(MESIC)
            MELI  = UPCASE(MELI)
            MEEVP = UPCASE(MEEVP)
            MEINF = UPCASE(MEINF)
            MEPHO = UPCASE(MEPHO)
            MESOM = UPCASE(MESOM)
            MEHYD = UPCASE(MEHYD)

            IF (INDEX('PG',MESOM) == 0) MESOM = ' '
            IF (INDEX('123',MESOL) == 0) MESOL = ' '
            IF (INDEX('RS',MESEV) == 0) MESEV = ' '
            IF (INDEX('Z',MEEVP) > 0) MEPHO = 'L'

!         Fourth line of simulation controls
          CASE('@N MAN')
            CALL IGNORE(SCLun,LINEXP,ISECT,CHARTEST)
            READ (CHARTEST,'(I2)',IOSTAT=ERRNUM) LEVEL
            IF (ERRNUM /= 0) EXIT
            IF (LEVEL /= CTRNO) EXIT 

!           READ (CHARTEST,60,IOSTAT=ERRNUM) LN,IPLTI,IIRRI,
!    &           IFERI,IRESI,IHARI
!  60       FORMAT (I3,11X,9(5X,A1))

            READ (CHARTEST,'(19X,A1)',IOSTAT=ERRNUM) IPLTI
            CALL CHECK_A('IPLTI', IPLTI, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(25X,A1)',IOSTAT=ERRNUM) IIRRI
            CALL CHECK_A('IIRRI', IIRRI, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(31X,A1)',IOSTAT=ERRNUM) IFERI
            CALL CHECK_A('IFERI', IFERI, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(37X,A1)',IOSTAT=ERRNUM) IRESI
            CALL CHECK_A('IRESI', IRESI, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(43X,A1)',IOSTAT=ERRNUM) IHARI
            CALL CHECK_A('IHARI', IHARI, ERRNUM, MSG, NMSG)

            IPLTI = UPCASE(IPLTI)
            IIRRI = UPCASE(IIRRI)
            IFERI = UPCASE(IFERI)
            IRESI = UPCASE(IRESI)
            IHARI = UPCASE(IHARI)

!         Fifth line of simulation controls
          CASE('@N OUT')
            CALL IGNORE(SCLun,LINEXP,ISECT,CHARTEST)
            READ (CHARTEST,'(I2)',IOSTAT=ERRNUM) LEVEL
            IF (ERRNUM /= 0) EXIT
            IF (LEVEL /= CTRNO) EXIT 

!           READ (CHARTEST,65,IOSTAT=ERRNUM) LN,IOX,IDETO,
!    &      IDETS,FROP,IDETG,IDETC,IDETW,IDETN,IDETP,IDETD,
!    &      IDETL,IDETH,IDETR
! 65        FORMAT (I3,11X,3(5X,A1),4X,I2,9(5X,A1))

            READ (CHARTEST,'(19X,A1)',IOSTAT=ERRNUM) IOX
            CALL CHECK_A('FNAME', IOX, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(25X,A1)',IOSTAT=ERRNUM) IDETO
            CALL CHECK_A('IDETO', IDETO, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(31X,A1)',IOSTAT=ERRNUM) IDETS
            CALL CHECK_A('IDETS', IDETS, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(35X,I3)',IOSTAT=ERRNUM) FROP
            CALL CHECK_I('FROP', FROP, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(43X,A1)',IOSTAT=ERRNUM) IDETG
            CALL CHECK_A('IDETG', IDETG, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(49X,A1)',IOSTAT=ERRNUM) IDETC
            CALL CHECK_A('IDETC', IDETC, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(55X,A1)',IOSTAT=ERRNUM) IDETW
            CALL CHECK_A('IDETW', IDETW, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(61X,A1)',IOSTAT=ERRNUM) IDETN
            CALL CHECK_A('IDETN', IDETN, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(67X,A1)',IOSTAT=ERRNUM) IDETP
            CALL CHECK_A('IDETP', IDETP, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(73X,A1)',IOSTAT=ERRNUM) IDETD
            CALL CHECK_A('IDETD', IDETD, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(79X,A1)',IOSTAT=ERRNUM) IDETL
            CALL CHECK_A('IDETL', IDETL, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(85X,A1)',IOSTAT=ERRNUM) IDETH
            CALL CHECK_A('IDETH', IDETH, ERRNUM, MSG, NMSG)

            READ (CHARTEST,'(91X,A1)',IOSTAT=ERRNUM) IDETR
            CALL CHECK_A('IDETR', IDETR, ERRNUM, MSG, NMSG)

            IOX   = UPCASE(IOX)
            IDETO = UPCASE(IDETO)
            IDETS = UPCASE(IDETS)
            IDETG = UPCASE(IDETG)
            IDETC = UPCASE(IDETC)
            IDETW = UPCASE(IDETW)
            IDETN = UPCASE(IDETN)
            IDETP = UPCASE(IDETP)
            IDETD = UPCASE(IDETD)
            IDETL = UPCASE(IDETL)
            IDETH = UPCASE(IDETH)
            IDETR = UPCASE(IDETR)

!           VBOSE = zero, suppress all output except SUMMARY.OUT
            IF (IDETL == '0') THEN
              IDETO = 'N'
              IDETG = 'N' 
              IDETC = 'N' 
              IDETW = 'N' 
              IDETN = 'N' 
              IDETP = 'N' 
              IDETD = 'N' 
              IDETH = 'N' 
              IDETR = 'N' 
            ENDIF

          END SELECT
        ENDDO

        CLOSE (SCLun)

        IF (NMSG < 6) THEN
          MSG(4)='No default simulation controls read.'
          NMSG = 4
        ELSE
          UseSimCtr = .TRUE.
        ENDIF
        CALL WARNING(NMSG,ERRKEY,MSG)
        CALL INFO (NMSG,ERRKEY,MSG)

      ELSE
        IF (.NOT. UseSimCtr) RETURN
        MEPHO  = MEPHO_SAVE
        ISWSYM = ISWSYM_SAVE
      ENDIF

C-----------------------------------------------------------------------
C    Select Model Name and Path -- order of priority:
!     CTRMODEL is value from control file override -- this is used
!         over all other values if valid. (Done in Default_SimControls)
!     CRMODEL is read from FILEX.  Use this if no control file.  
!     MODELARG is from command line argument list. Third priority. 
!     Last, use value from DSSATPRO.v45.
C-----------------------------------------------------------------------
!     First check model from simulation control file 
      TRY_MODEL = CTRMODEL
      CALL MODEL_NAME (CONTROL%CROP, DSSATP, TRY_MODEL, MODEL)

!     If model name from simulation control file is not acceptable,
!     try value from FILEX
      IF (TRY_MODEL /= MODEL) THEN
        TRY_MODEL = CRMODEL
        CALL MODEL_NAME (CONTROL%CROP, DSSATP, TRY_MODEL, MODEL)

!       If FILEX model name was not acceptable, then try the 
!       model name read from command line.  If this is not OK, 
!       MODEL contains value from DSSATPRO.V45
        IF (TRY_MODEL /= MODEL) THEN
          TRY_MODEL = MODELARG
          CALL MODEL_NAME (CONTROL%CROP, DSSATP, TRY_MODEL, MODEL)
        ENDIF
      ENDIF

      MEPHO_SAVE = MEPHO
      IF (MEPHO .EQ. 'L' .AND. CTRMODEL(1:5) .NE. 'CRGRO') THEN
        MEPHO = 'C'
        MSG(1)='Photosynthesis method (PHOTO in FILEX) has been changed'
        WRITE (MSG(2),81) CTRMODEL(1:5)
   81   FORMAT('from "L" to "C" for compatibility with crop model, '
     &            ,A5,'.') 
        CALL WARNING(2, "IPSIM ", MSG)
      ENDIF

      ISWSYM_SAVE = ISWSYM
      SELECT CASE (CONTROL % CROP)
        CASE ('BN','SB','PN','PE','CH','PP',
     &          'VB','CP','CB','FB','GB','LT')
!         Do nothing -- CROPGRO crops can have Y or N
        CASE DEFAULT; ISWSYM = 'N'  !other crops don't have a choice
      END SELECT

      IF ((INDEX('CSPT',CONTROL % CROP)) .GT. 0) THEN
        IF (IHARI .EQ. 'A') THEN
          MSG(1) = "Default Simulation controls file used."
          WRITE(MSG(2),'("Automatic harvest option is not valid for ",
     &    "crop type: ",A2)') CONTROL%CROP
          CALL WARNING(2, ERRKEY, MSG)
          CALL ERROR ('IPSIM ',4,FILEX,LINEXP)
        ENDIF
      ENDIF

      IF ((INDEX('PT',CONTROL % CROP)) .GT. 0) THEN
        IF (IPLTI .EQ. 'A') THEN
          MSG(1) = "Default Simulation controls file used."
          WRITE(MSG(2),'("Automatic planting option is not valid for ",
     &    "crop type: ",A2)') CONTROL%CROP
          CALL WARNING(2, ERRKEY, MSG)
          CALL ERROR ('IPSIM ',5,FILEX,LINEXP)
        ENDIF
      ENDIF

!     Fill ISWITCH variable (complete)
      IF (IOX    /= ' ' .AND. IOX /= '.')    ISWITCH % FNAME  = IOX 
      IF (ISIMI  /= ' ' .AND. ISIMI  /= '.') ISWITCH % ISIMI  = ISIMI 
      IF (ISWWAT /= ' ' .AND. ISWWAT /= '.') ISWITCH % ISWWAT = ISWWAT
      IF (ISWNIT /= ' ' .AND. ISWNIT /= '.') ISWITCH % ISWNIT = ISWNIT
      IF (ISWSYM /= ' ' .AND. ISWSYM /= '.') ISWITCH % ISWSYM = ISWSYM
      IF (ISWPHO /= ' ' .AND. ISWPHO /= '.') ISWITCH % ISWPHO = ISWPHO
      IF (ISWPOT /= ' ' .AND. ISWPOT /= '.') ISWITCH % ISWPOT = ISWPOT
      IF (ISWDIS /= ' ' .AND. ISWDIS /= '.') ISWITCH % ISWDIS = ISWDIS
      IF (ISWCHE /= ' ' .AND. ISWCHE /= '.') ISWITCH % ISWCHE = ISWCHE
      IF (ISWTIL /= ' ' .AND. ISWTIL /= '.') ISWITCH % ISWTIL = ISWTIL
      IF (ICO2   /= ' ' .AND. ICO2   /= '.') ISWITCH % ICO2   = ICO2
      IF (MEWTH  /= ' ' .AND. MEWTH  /= '.') ISWITCH % MEWTH  = MEWTH
      IF (MESOM  /= ' ' .AND. MESOM  /= '.') ISWITCH % MESOM  = MESOM
      IF (MELI   /= ' ' .AND. MELI   /= '.') ISWITCH % MELI   = MELI 
      IF (MEEVP  /= ' ' .AND. MEEVP  /= '.') ISWITCH % MEEVP  = MEEVP
      IF (MEINF  /= ' ' .AND. MEINF  /= '.') ISWITCH % MEINF  = MEINF
      IF (MEPHO  /= ' ' .AND. MEPHO  /= '.') ISWITCH % MEPHO  = MEPHO
      IF (MEHYD  /= ' ' .AND. MEHYD  /= '.') ISWITCH % MEHYD  = MEHYD
      IF (MESEV  /= ' ' .AND. MESEV  /= '.') ISWITCH % MESEV  = MESEV
      IF (MESOL  /= ' ' .AND. MESOL  /= '.') ISWITCH % MESOL  = MESOL
!     IF (METMP  /= ' ' .AND. METMP  /= '.') ISWITCH % METMP  = METMP
      IF (IPLTI  /= ' ' .AND. IPLTI  /= '.') ISWITCH % IPLTI  = IPLTI
      IF (IIRRI  /= ' ' .AND. IIRRI  /= '.') ISWITCH % IIRRI  = IIRRI
      IF (IFERI  /= ' ' .AND. IFERI  /= '.') ISWITCH % IFERI  = IFERI
      IF (IRESI  /= ' ' .AND. IRESI  /= '.') ISWITCH % IRESI  = IRESI
      IF (IHARI  /= ' ' .AND. IHARI  /= '.') ISWITCH % IHARI  = IHARI
      IF (IDETO  /= ' ' .AND. IDETO  /= '.') ISWITCH % IDETO  = IDETO
      IF (IDETS  /= ' ' .AND. IDETS  /= '.') ISWITCH % IDETS  = IDETS
      IF (IDETG  /= ' ' .AND. IDETG  /= '.') ISWITCH % IDETG  = IDETG
      IF (IDETC  /= ' ' .AND. IDETC  /= '.') ISWITCH % IDETC  = IDETC
      IF (IDETW  /= ' ' .AND. IDETW  /= '.') ISWITCH % IDETW  = IDETW
      IF (IDETN  /= ' ' .AND. IDETN  /= '.') ISWITCH % IDETN  = IDETN
      IF (IDETP  /= ' ' .AND. IDETP  /= '.') ISWITCH % IDETP  = IDETP
      IF (IDETD  /= ' ' .AND. IDETD  /= '.') ISWITCH % IDETD  = IDETD
      IF (IDETL  /= ' ' .AND. IDETL  /= '.') ISWITCH % IDETL  = IDETL
      IF (IDETH  /= ' ' .AND. IDETH  /= '.') ISWITCH % IDETH  = IDETH
      IF (IDETR  /= ' ' .AND. IDETR  /= '.') ISWITCH % IDETR  = IDETR

      IF (NSWITCH /=-99) ISWITCH % NSWI   = NSWITCH
    
!       Fill CONTROL variable (partial)
      IF (MODEL(1:1) /= ' ' .AND. MODEL(1:1) /= '.') 
     &                                     CONTROL % MODEL = MODEL
      IF (MESIC /= ' ' .AND. MESIC /= '.') CONTROL % MESIC = MESIC  
      IF (NYRS  /= -99) CONTROL % NYRS  = NYRS
      IF (YRSIM /= -99) CONTROL % YRSIM = YRSIM  
      IF (FROP  > 0)    CONTROL % FROP  = FROP   

      RETURN
      END SUBROUTINE Default_SimControls
!=======================================================================

      SUBROUTINE CHECK_A(LABEL, VALUE, ERRNUM, MSG, NMSG)
      IMPLICIT NONE

      CHARACTER*(*) VALUE
      CHARACTER*(*) LABEL
      CHARACTER*30 MSG_TEXT
      CHARACTER*78 MSG(50)
      INTEGER ERRNUM, NMSG

      IF (ERRNUM /= 0)  THEN
        VALUE = ' '
      ENDIF

      IF (VALUE /= ' ' .AND. VALUE /= '.') THEN
        NMSG = NMSG + 1
        WRITE(MSG(NMSG),'(A8,A,A,2X,A30)') LABEL, " = ", VALUE, 
     &    MSG_TEXT(LABEL)
      ENDIF

      RETURN
      END SUBROUTINE CHECK_A

!=======================================================================

      SUBROUTINE CHECK_I(LABEL, VALUE, ERRNUM, MSG, NMSG)
      IMPLICIT NONE

      INTEGER VALUE
      CHARACTER*(*) LABEL
      CHARACTER*30 MSG_TEXT
      CHARACTER*78 MSG(50)
      INTEGER ERRNUM, NMSG

      IF (ERRNUM /= 0)  THEN
        VALUE = -99
      ENDIF

      IF (VALUE > 0) THEN
        NMSG = NMSG + 1
        IF (VALUE < 10) THEN
          WRITE(MSG(NMSG),'(A8,A,I1,2X,A30)') LABEL," = ",VALUE, 
     &      MSG_TEXT(LABEL)  
        ELSE
          WRITE(MSG(NMSG),'(A8,A,I8,2X,A30)') LABEL," = ",VALUE, 
     &      MSG_TEXT(LABEL)  
        ENDIF
      ELSE
        VALUE = -99
      ENDIF

      RETURN
      END SUBROUTINE CHECK_I

!=======================================================================

!=======================================================================
      CHARACTER*30 FUNCTION MSG_TEXT(LABEL)

      CHARACTER*(*) LABEL

      SELECT CASE(LABEL)  !    "123456789012345678901234567890"
      CASE('FNAME');  MSG_TEXT="Alternate file name option    "
      CASE('ISIMI');  MSG_TEXT="Start of simulation code      "
      CASE('ISWWAT'); MSG_TEXT="Soil water simulation switch  "
      CASE('ISWNIT'); MSG_TEXT="Soil N simulation switch      "
      CASE('ISWSYM'); MSG_TEXT="N fixation switch             "
      CASE('ISWPHO'); MSG_TEXT="P simulation switch           "
      CASE('ISWPOT'); MSG_TEXT="Potassium simulation switch   "
      CASE('ISWDIS'); MSG_TEXT="Pest & disease simulation     "
      CASE('ISWCHE'); MSG_TEXT="Chemical application switch   "
      CASE('ISWTIL'); MSG_TEXT="Tillage option switch         "
      CASE('ICO2');   MSG_TEXT="Option to read CO2 from file  "
      CASE('MEWTH');  MSG_TEXT="Weather method                "
      CASE('MESOM');  MSG_TEXT="Soil organic matter method    "
      CASE('MELI') ;  MSG_TEXT="Light interception method     "
      CASE('MEEVP');  MSG_TEXT="Pot. evapotranspiration method"
      CASE('MEINF');  MSG_TEXT="Infiltration method           "
      CASE('MEPHO');  MSG_TEXT="Photosynthesis method         "
      CASE('MEHYD');  MSG_TEXT="Hydrology method              "
      CASE('MESEV');  MSG_TEXT="Soil evaporation method       "
      CASE('MESOL');  MSG_TEXT="Soil input and partitioning   "
!     CASE('METMP');  MSG_TEXT="Soil temperature method       "
      CASE('IPLTI');  MSG_TEXT="Planting method switch        "
      CASE('IIRRI');  MSG_TEXT="Irrigation method switch      "
      CASE('IFERI');  MSG_TEXT="Fertilizer switch             "
      CASE('IRESI');  MSG_TEXT="Organic matter switch         "
      CASE('IHARI');  MSG_TEXT="Harvest simulation switch     "
      CASE('IDETO');  MSG_TEXT="Overview output switch        "
      CASE('IDETS');  MSG_TEXT="Summary output switch         "
      CASE('IDETG');  MSG_TEXT="Growth output switch          "
      CASE('IDETC');  MSG_TEXT="Carbon output switch          "
      CASE('IDETW');  MSG_TEXT="Water output switch           "
      CASE('IDETN');  MSG_TEXT="Nitrogen output switch        "
      CASE('IDETP');  MSG_TEXT="Phosphorus output switch      "
      CASE('IDETD');  MSG_TEXT="Pest & disease output switch  "
      CASE('IDETL');  MSG_TEXT="Output detail switch          "
      CASE('IDETH');  MSG_TEXT="Chemial output file switch    "
      CASE('IDETR');  MSG_TEXT="Operations output file switch "
      CASE('NSWITCH');MSG_TEXT="Nitrogen options switch       "
      CASE('NYRS')  ; MSG_TEXT="Number of years of simulation "
      CASE('YRSIM') ; MSG_TEXT="Start of simulation date      "
      CASE('MODEL') ; MSG_TEXT="Crop model                    "
      CASE('MESIC') ; MSG_TEXT="Sequence code (not used)      "
      CASE('FROP')  ; MSG_TEXT="Frequency of output code      "
      CASE DEFAULT;   MSG_TEXT="                              "
      END SELECT

      RETURN
      END FUNCTION MSG_TEXT
!=======================================================================
