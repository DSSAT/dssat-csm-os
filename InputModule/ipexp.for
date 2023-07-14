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
!  03/26/2007 CHP MESOL = 2 is default (see LYRSET2 in LMATCH.for)
!  07/04/2007 CHP MESEV = 2 is default (Sulieman-Ritchie soil evap)
!  07/05/2007 CHP Default simulation controls file added.
!  09/18/2007 CHP Added codes for IXIM maize model 
!  04/21/2008 CHP Added MESOL = 3, user-specified soil layer dist.
!  12/09/2008 CHP Remove METMP
!  04/16/2013 CHP/KD Added SALUS model
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
     &           CONTROL, ISWITCH, UseSimCtr, MODELARG,PMWD)

      USE ModuleDefs
      USE ModuleData    
      Use CsvOutput   ! VSH
      IMPLICIT NONE
      EXTERNAL ERROR, FIND, WARNING, YR_DOY, IGNORE, VERIFY, CLEAR, 
     &  IGNORE2, OPHEAD, MAKEFILEW, IPCUL, IPPLNT_INP, IPSIM, PATH, 
     &  GET_CROPD, IPFLD, IPENV, IPHAR, IPIRR, IPFERT, IPRES, IPCHEM, 
     &  IPTILL

      SAVE

      INCLUDE 'COMIBS.blk'
      INCLUDE 'COMSWI.blk'

      CHARACTER* 1 LINE(80),BLANK, RNMODE
      CHARACTER* 1 WMODI,ANS
      CHARACTER* 2 CROP
      CHARACTER* 3 PROCOD,ALN(13),ALLN, PROCODG, PROCODC, PROCODW
      CHARACTER* 4 WSTA1
      CHARACTER* 6 VARNO,ERRKEY,FINDCH
      CHARACTER* 7 FILELS
      CHARACTER* 8 FILES_a, FILES_b, MODEL, MODELARG, FILEW4
      CHARACTER*10 SLNO
      CHARACTER*12 NAMEF, FILEX, FILE_CHECK
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
!     NEW FORAGE VARIABLES (DIEGO-2/14/2017)
      INTEGER TRTNUM, ROTNUM!,FREQ(3),CUHT(3) 
      REAL    FLAG,EXP,TRT,PLTFOR !,FREQ,CUHT 
      REAL    PMWD

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

!     VSH
      EXPNAME = EXPER
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
      control % ename = ename
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
         IF (NLOOP .GT. 25) CALL ERROR(ERRKEY,4,FILEX,LINEXP)
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
            CALL ERROR (ERRKEY,4,FILEX,LINEXP)
         ENDIF
       ELSEIF (INDEX ('Q',RNMODE) .GT. 0) THEN
         !READ (TRNARG(1:6),'(I6)') TRTN
         !READ (ROTNARG(1:6),'(I6)') ROTN
         TRTN = TRTNUM
         ROTN = ROTNUM
         I = 999
       ELSEIF (INDEX ('NQGSFBECTY',RNMODE) .GT. 0) THEN
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
      IF ((INDEX('BEDNSGFCTY',RNMODE) .GT. 0 .AND. TRTN .NE. TRTNO) .OR.
     &    (INDEX('Q',RNMODE) .GT. 0 .AND. 
     &                     (TRTN .NE. TRTNO .OR. ROTN .NE. ROTNO)) .OR. 
     &    (INDEX('AI',RNMODE) .GT. 0 .AND. I .LT. TRTN))
     &    GO TO 50

!     Generate header information for Warnings or Errors in input module
      CALL OPHEAD (RUNINIT,99,0.0,0.0,"                ",0.0,0.0, 
     &     "      ",RUN,"        ",TITLET,WTHSTR, RNMODE,
     &     CONTROL, ISWITCH, UseSimCtr, PATHEX)
     
C-----------------------------------------------------------------------
C     Call MAKEFILEW to read FILEX and 
C-----------------------------------------------------------------------
      CALL MAKEFILEW(LUNEXP,DSSATP,PATHEX,FILEX,
     &               SimLevel,LNSIM,LNPLT,LNFLD)
      
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
      REWIND (LUNEXP)

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
     &     MODEL,SDEPTH,SDWTPL,PLTPOP,PLANTS,SDAGE,ATEMP,PLPH,IEMRG,
     &     YRPLT,SPRLAP,NFORC,PLTFOR,NDOF,PMTYPE,IPLTI)

C-----------------------------------------------------------------------
C     Call IPSIM
C-----------------------------------------------------------------------
      !FO - Removed from ipexp and added into IPSIM
      !IF (.NOT. SimLevel) THEN
      !  LNSIM = 0
      !  YRSIM = YRPLT
      !ENDIF
      
      CALL IPSIM (LUNEXP,LNSIM,SimLevel,TITSIM,NYRS,RUN,NREPSQ,ISIMI,
     &     PWDINF,PWDINL,SWPLTL,NCODE,SWPLTH,SWPLTD,YEAR,PTX,PTTN,
     &     DSOIL,THETAC,IEPT,IOFF,IAME,DSOILN,SOILNC,YRSIM,SOILNX,
     &     NEND,RIP,NRESDL,DRESMG,HDLAY,HLATE,HPP,HRP,FTYPEN,RSEED1,
     &     LINEXP,AIRAMT,EFFIRR,CROP,FROP,MODEL,RNMODE,FILEX,
     &     CONTROL,ISWITCH,UseSimCtr,FILECTL,MODELARG,YRPLT)

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
!        09/21/2009
!        CHP/KD Generic SALUS model used for several crops with single  
!          cultivar file. Do not include crop code in file name.
         IF (MODEL(1:5) == 'SALUS') THEN
            FILEG(1:12) = MODEL(1:8)//'.CUL'
         ENDIF
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
     &     CONTROL, ISWITCH, UseSimCtr, PATHEX)

C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
!     Skip soils field and soils input for sequence mode
      IF (INDEX('FQ',RNMODE) .LE. 0 .OR. RUN == 1) THEN

        CALL IPFLD (LUNEXP,FILEX,LNFLD,FLDNAM,WSTA,WSTA1,SLNO,
     &     SLTX,FLST,SLOPE,DFDRN,FLDD,SFDRN,FLOB,SLDP,PMWD,
     &     XCRD,YCRD,ELEV,AREA,SLEN,FLWR,SLAS,FldHist, FHDur,PMALB)

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

!-----------------------------------------------------------------------
! 2020-10-11 CHP RNMODE = 'Y' indicates yield forecast mode. May need multiple
!     weather files. 
!     If RNMODE = 'Y' and MEWTH = 'G','W','S', then also need a WTH file for
!     forecast year weather data.
!-----------------------------------------------------------------------
!     Generated weather data files
      IF (MEWTH .EQ. 'G') THEN
         IF (WSTA1(4:4) .EQ. BLANK) THEN
            IF (YEAR .LT. 2000) THEN
              YR = YEAR - 1900
            ELSE IF (YEAR .LT. 3000) THEN
              YR = YEAR - 2000
            ENDIF
            WRITE (FILEWG(1:12),75) WSTA,YR,'01.WTG'
         ELSE
            WRITE (FILEWG(1:12),76) WSTA,WSTA1,'.WTG'
         ENDIF
         PROCODG = 'WGD'
      ENDIF
!     Interactively generated weather 
      IF (MEWTH .EQ. 'S' .OR. MEWTH .EQ. 'W') THEN
         WRITE (FILEWC(1:12),77) WSTA,'.CLI    '
         PROCODC = 'CLD'
      ENDIF
!     Measured weather data
      IF (MEWTH .EQ. 'M' .OR. RNMODE .EQ. 'Y') THEN
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
         PROCODW = 'WED'
      ENDIF
      IF (INDEX('GSWM',RNMODE) .LT. 0) THEN
         CALL ERROR (ERRKEY,22,FILEX,LINEXP)
      ENDIF

!     Check for existing FILEW, FILEWC, and FILEWG
      DO I = 1, 3
        SELECT CASE (I)
          CASE (1)
            IF (MEWTH .EQ. 'M' .OR. RNMODE .EQ. 'Y') THEN
              FILE_CHECK = FILEW
              PROCOD = PROCODW
            ELSE
              CYCLE
            ENDIF
          CASE (2)
            IF (MEWTH .EQ. 'G') THEN
              FILE_CHECK = FILEWG
              PROCOD = PROCODG
            ELSE
              CYCLE
            ENDIF
          CASE (3)
            IF (MEWTH .EQ. 'S' .OR. MEWTH .EQ. 'W') THEN
              FILE_CHECK = FILEWC
              PROCOD = PROCODC
            ELSE
              CYCLE
            ENDIF
          CASE DEFAULT; CYCLE
        END SELECT

!       Check weather filename in current directory
        INQUIRE (FILE = FILE_CHECK,EXIST = FEXIST)
        IF (FEXIST) THEN
          PATHWT = BLANK
!       Check weather filename in data directory
        ELSE
          FILETMP = TRIM(PATHEX)//FILE_CHECK
          INQUIRE (FILE = FILETMP,EXIST = FEXIST)
          IF (FEXIST) THEN
            PATHWT = TRIM(PATHEX)
!         Check weather filename in default DSSAT directory
          ELSE
            CALL PATH(PROCOD,DSSATP,PATHWT,1,NAMEF)
            FILETMP = TRIM(PATHWT) // FILE_CHECK
            INQUIRE (FILE=FILETMP, EXIST = FEXIST)
            IF (FEXIST) THEN
              PATHWT = PATHWT
!           Check 4-character file name in data directory
            ELSE
              FILEW4 = FILE_CHECK(1:4) // ".WTH"
              FILETMP = TRIM(PATHEX) // FILEW4
              INQUIRE (FILE=FILETMP, EXIST = FEXIST)
              IF (FEXIST) THEN
                PATHWT = TRIM(PATHEX)
                FILE_CHECK = FILEW4
!             Check 4-character filename in default DSSAT directory
              ELSE
                FILETMP = TRIM(PATHWT) // FILE_CHECK
                INQUIRE (FILE=FILETMP, EXIST = FEXIST)
                IF (FEXIST) THEN
                  PATHWT = PATHWT
                  FILE_CHECK = FILEW4
                ELSE
                  MSG(1) = "Weather file not found."
                  MSG(2) = "  Neither " // FILE_CHECK // " nor "//FILEW4
                  MSG(3) = 
     &              "  were found in weather or experiment directories."
                  MSG(4) = "Simulation will end."
                  CONTROL % ErrCode = 29
                  CALL PUT(CONTROL)
                  CALL WARNING(4,ERRKEY,MSG)
!                 CALL ERROR(ERRKEY,29,FILEW,0)
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF

        SELECT CASE(I)
          CASE (1); FILEW  = FILE_CHECK; PATHWTW = PATHWT
          CASE (2); FILEWG = FILE_CHECK; PATHWTG = PATHWT
          CASE (3); FILEWC = FILE_CHECK; PATHWTC = PATHWT
        END SELECT
      ENDDO

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
     &     WTHADJ)    !,YRSIM)

C-----------------------------------------------------------------------
C     Call IPHAR
C-----------------------------------------------------------------------
!     NEW FORAGE VARIABLES (DIEGO-2/14/2017)
      CALL IPHAR (LUNEXP,FILEX,LNHAR,HDATE,HSTG,HCOM,HSIZ,HPC,
     &     NHAR,IHARI,YRSIM,CROP,HBPC)    !,FREQ,CUHT

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
      CALL IPCHEM (LUNEXP,FILEX,LNCHE,YRSIM,NCHEM,CDATE,
     &    CHCOD,CHAMT,CHMET,CHDEP,CHT,ISWCHE,CHEXTR)

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
C  04/26/2013 GH  Update planting method for cassava
C  05/07/2020 FO  Added new Y4K subroutine call to convert YRDOY
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
     &     LUNEXP,FILEX,LNPLT,PLME,PLDS,ROWSPC,AZIR,CROP,MODEL,
     &     SDEPTH,SDWTPL,PLTPOP,PLANTS,SDAGE,ATEMP,PLPH,IEMRG,
     &     YRPLT,SPRLAP,NFORC,PLTFOR,NDOF,PMTYPE,IPLTI)

      IMPLICIT NONE
      EXTERNAL ERROR, FIND, WARNING, YR_DOY, IGNORE, Y4K_DOY

      CHARACTER*1   PLME,PLDS,IPLTI
      CHARACTER*2   CROP
      CHARACTER*6   ERRKEY,FINDCH
      CHARACTER*8   MODEL
      CHARACTER*12  FILEX
      CHARACTER*78  MSG(2)
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
            
C 05/07/2020 FO Add new Y4K subroutine call to convert YRDOY
            !CALL Y2K_DOY(YRPLT)
            !CALL Y2K_DOY(IEMRG)
            IF(LN .EQ. LNPLT) THEN
              CALL Y4K_DOY(YRPLT,FILEX,LINEXP,ERRKEY,10)
              CALL Y4K_DOY(IEMRG,FILEX,LINEXP,ERRKEY,15)
              CALL YR_DOY (YRPLT,YR,IPLT)
            ENDIF
            
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
C-GH     IF (PLTPOP .LE. 0.0 .OR. PLTPOP .GT. 999.) THEN
         IF (PLTPOP .LE. 0.0) THEN

            IF (CROP /= 'SC') CALL ERROR (ERRKEY,11,FILEX,LINEXP)
         ENDIF

!        chp 5/17/2011
!        Need to handle missing row spacing data
!         IF ((ROWSPC .GT. -90. .AND. ROWSPC .LE. 0.0)
!     &      .OR. ROWSPC .GT. 99999.) THEN
!            CALL ERROR (ERRKEY,12,FILEX,LINEXP)
!         ENDIF
         IF (ROWSPC <= 0.0) THEN
            SELECT CASE (MODEL(1:5))
            CASE ('SCCAN'); ROWSPC = 0.0  !Canegro does not need this
            CASE DEFAULT
              ROWSPC = 1.0 / SQRT(PLTPOP) * 100.
              MSG(1) = "Missing row spacing in experiment file."
              WRITE(MSG(2),'(A,F8.1,A)') "Value set to ", ROWSPC," cm"
              CALL WARNING(2,ERRKEY,MSG)
            END SELECT
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
C-GH
         IF (INDEX('CS',CROP) .GT. 0) THEN
           IF (INDEX('VHI',PLME) .LE. 0)
     &         CALL ERROR (ERRKEY,19,FILEX,LINEXP)
         ENDIF
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

C 60 FORMAT (I3,I5,1X,I5,2(1X,F5.0),2(5X,A1),8(1X,F5.0),I6,F6.0,2I6)
 60   FORMAT (I3,I5,1X,I5,2(F6.0),2(5X,A1),8(1X,F5.0),I6,F6.0,2I6)

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
!  05/28/2021 FO  Added code for LAT,LONG and ELEV output in Summary.OUT
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
     &           SLTX,FLST,SLOPE,DFDRN,FLDD,SFDRN,FLOB,SLDP,PMWD,
     &           XCRD,YCRD,ELEV,AREA,SLEN,FLWR,SLAS,FldHist,FHDUR,PMALB)

      USE ModuleData
      IMPLICIT NONE
      EXTERNAL ERROR, FIND, UPCASE, WARNING, INFO, IGNORE, HFIND, 
     &  SUMVALS

      CHARACTER*1  UPCASE
      CHARACTER*4  WSTA,WSTA1,HFNDCH
      CHARACTER*5  DFDRN,FLST,SLTX, FldHist
      CHARACTER*6  ERRKEY,FINDCH
      CHARACTER*8  FLDNAM
      CHARACTER*9  CELEV
      CHARACTER*10 SLNO
      CHARACTER*12 FILEX
      CHARACTER*15 CXCRD, CYCRD
      CHARACTER*78 MSG(2)
      CHARACTER*92 CHARTEST
      LOGICAL      CKELEV
      DATA CKELEV /.TRUE./

      INTEGER LUNEXP,LNFLD,LN,LINEXP,ISECT,IFIND,ERRNUM,I, FHDUR

      REAL    FLDD,SFDRN,FLOB,SLDP,SLOPE,PMWD,PMALB
      REAL    XCRD,YCRD,ELEV,AREA,SLEN,FLWR,SLAS

!     Arrays which contain data for printing in SUMMARY.OUT file
      INTEGER, PARAMETER :: SUMNUM = 3
      CHARACTER*4, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

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
     &           CXCRD,CYCRD,CELEV,AREA,SLEN,FLWR,SLAS, FldHist, FHDUR
           IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
         ELSE
           CALL ERROR (ERRKEY,2,FILEX,LINEXP)
         ENDIF
         IF (LN .NE. LNFLD) GO TO 70
      ENDIF
      IF (AREA .LE. 0.0) AREA = 1.0
      IF (FLWR .LE. 0.0) FLWR = 1.0
      IF (SLEN .LE. 0.0) SLEN = SQRT(AREA*FLWR*10000.0)
      
C FO - Store Summary.out labels and values in arrays to send to
C     OPSUM routines for printing.  Integers are temporarily 
C     saved as real numbers for placement in real array.

      READ(CXCRD,'(F15.0)', IOSTAT=ERRNUM) XCRD
      IF(ERRNUM .NE. 0) THEN
         XCRD = -999.0
         MSG(1) = 'Error reading latitude from experimental file'
         MSG(2) = FILEX
         CALL WARNING(2, ERRKEY, MSG)
      ENDIF
      READ(CYCRD,'(F15.0)', IOSTAT=ERRNUM) YCRD
      IF(ERRNUM .NE. 0) THEN
         YCRD = -99.0
         MSG(1) = 'Error reading longitude from experimental file'
         MSG(2) = FILEX
         CALL WARNING(2, ERRKEY, MSG)
      ENDIF
      READ(CELEV,'(F9.0)', IOSTAT=ERRNUM)  ELEV
      IF(ERRNUM .NE. 0) THEN
        ELEV = -99.0
        MSG(1) = 'Error reading elevation from experimental file'
        MSG(2) = FILEX
        CALL WARNING(2, ERRKEY, MSG)
      ENDIF
      
      IF(YCRD .GE. -90.0 .AND. YCRD .LE. 90.0 .AND.
     &   XCRD .GE.-180.0 .AND. XCRD .LE. 180.0 .AND.
     &   LEN_TRIM(CYCRD).GT. 0.0 .AND. LEN_TRIM(CXCRD).GT.0.0
     &   .AND.
     &   (ABS(YCRD) .GT. 1.E-15 .OR. ABS(XCRD) .GT. 1.E-15))THEN
!     Transfer data to the modules
         CALL PUT('FIELD','CYCRD',CYCRD)
         CALL PUT('FIELD','CXCRD',CXCRD)   
         LABEL(1) = 'YCRD'; VALUE(1) = YCRD 
         LABEL(2) = 'XCRD'; VALUE(2) = XCRD
         CKELEV = .TRUE.
      ELSE
        !     Transfer data to the modules
        CALL PUT('FIELD','CYCRD','            -99')
        CALL PUT('FIELD','CXCRD','            -99')
        LABEL(1) = 'YCRD'; VALUE(1) = -99.0 
        LABEL(2) = 'XCRD'; VALUE(2) = -999.0         
        CKELEV = .FALSE.
      ENDIF
  
!     Check elevation (CKELEV) based on latitude and longitude  
      IF(CKELEV .EQV. .TRUE.) THEN
         IF(ELEV .GT. -99.0 .AND. LEN_TRIM(CELEV) .GT. 0.0) THEN
           CALL PUT('FIELD','CELEV',CELEV)
           LABEL(3) = 'ELEV'; VALUE(3) = ELEV      
         ELSE
           CALL PUT('FIELD','CELEV','      -99')
           LABEL(3) = 'ELEV'; VALUE(3) = -99.0
           
         ENDIF
      ELSE
        IF(ELEV .GT. -99.0 .AND. LEN_TRIM(CELEV) .GT. 0.0 .AND.
     &     ABS(ELEV) .GT. 1.E-15) THEN
          CALL PUT('FIELD','CELEV',CELEV)
          LABEL(3) = 'ELEV'; VALUE(3) = ELEV      
        ELSE
          CALL PUT('FIELD','CELEV','      -99')
          LABEL(3) = 'ELEV'; VALUE(3) = -99.0
        ENDIF
      ENDIF
               
C     Send labels and values to OPSUM      
      CALL SUMVALS (SUMNUM, LABEL, VALUE)    
C
C    End New section

C
C    New section (3rd)
C
C    Find header and read second line of field information
C
      HFNDCH='PMALB'
      CALL HFIND(LUNEXP,HFNDCH,LINEXP,IFIND)
      IF (IFIND .EQ. 1) THEN
 71     CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
        IF (ISECT .EQ. 1) THEN
           READ (CHARTEST,90,IOSTAT=ERRNUM) LN,
!     2023-07-14 chp changed order of these three variables to allow 
!                    1D and 2D models to use the same file format.
!    &                PMWD,PMALB
     &         PMALB, PMWD

           IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
         ELSE
           CALL ERROR (ERRKEY,2,FILEX,LINEXP)
         ENDIF
         IF (LN .NE. LNFLD) GO TO 71
      ELSE
        PMWD = -99
        PMALB = -99
      ENDIF
      IF (PMWD .LE. 0.0) PMWD = -99
      IF (PMALB .LE. 0.0) PMALB = -99

C
C    End New section (3rd)

      REWIND(LUNEXP)

      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

 60   FORMAT (I3,A8,1X,2A4,1X,F5.0,1X,F5.0,1X,A5,2(1X,F5.0),
     &         2(1X,A5),1X,F5.0,1X,A10)
!     chp 7/26/2006
! 80   FORMAT (I3,2(F15.0,1X),F9.0,1X,F17.0,3(1X,F5.0))
 80   FORMAT (I3,2(A15,1X),A9,1X,F17.0,3(1X,F5.0),1X,A5,I6)
 90   FORMAT (I3, 4F6.0)

      END SUBROUTINE IPFLD

