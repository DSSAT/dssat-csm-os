      SUBROUTINE RDDATA (ITASK,CALPRG,IUNIT,IULOG,FILNAM,IS,
     $    XNAME,VARTYP,DX,RX,IX,CX,LX,
     $    NDDEC,NRDEC,NIDEC,NCDEC,NLDEC,NREQ,
     $    xDM,xRM,xIM,xCM,xLM)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER ITASK,IUNIT,IULOG,IS
      INTEGER NDDEC,NRDEC,NIDEC,NCDEC,NLDEC,NREQ
      DOUBLE  PRECISION DX(NDDEC),xDM
      REAL    RX(NRDEC),xRM
      INTEGER IX(NIDEC),xIM
      CHARACTER*80      xCM
      LOGICAL LX(NLDEC),xLM
      CHARACTER*(*) CALPRG,CX(NCDEC),FILNAM,XNAME,VARTYP*1

**    machine constants and buffer length
      INCLUDE 'rdmachin.inc'

*     read rddata common block
      INCLUDE 'rddata.inc'

*     some other variables on rerun and data values
      INTEGER IUNREP,IULR,INR,ITR,IRREP,PNTRNX,ILR
      INTEGER IUNDAT,IULD,IND,ITD,IRDAT,PNTDNX,ILD
      INTEGER INSETS,ISLOC,IPP,IRCHK,IDUM,IPS
      CHARACTER RFIL*80,DFIL*80,REPTMP*80,DATTMP*80,INQNAM*512
      DIMENSION IRCHK(ILNREP)
      LOGICAL LOGR,REPL,LOGD,INIDAT

*     filename list
      INTEGER INLIST,INFILS
      CHARACTER*80 FILLIS,TMPLIS
      DIMENSION FILLIS(INFDEC),TMPLIS(INFDEC)

*     rerun file buffer
      DOUBLE PRECISION RRBUF(0:ILBUF/IRL-1)

*     data file buffer
      DOUBLE PRECISION RDBUF(0:ILBUF/IRL-1)

*     other local variables
      INTEGER JLD, JLQ
      INTEGER I,J,IEL,N,ISX,ILX,IMES,IWN,IREC,ILTMP,IRECL
      INTEGER NDEC,NELMT,IXL,IT1,IT2,IWAR,IVT,IV1,IV2,IVL,IVP,IPN
      INTEGER IUL,ISTREC,ILA,ILT,ILW,IPNT,IREP,IXLMAX,ITMP1
      DOUBLE PRECISION AD
      CHARACTER ERRTXT*26,MESSAG*42,LXNM31*31,ONFILE*9,GETSCA*2,SCARR*6
      CHARACTER BYTE*1,PPP*3,VALUE*12,LINE*76,ASKED*16,REQTYP*1,GETONF*5
      CHARACTER WHAT*35,TARGET*10,RECORD*79,ARROW*7
      DIMENSION MESSAG(28),ONFILE(5),ASKED(6),REQTYP(6),RECORD(4)
      DIMENSION SCARR(2)
      LOGICAL OPUNIT,OVERWR,EMPTY,THERE,FATAL,FOUND,ON,OFF
      LOGICAL SINGLE,UNKNOW,FIXED,TEMP,GIVEN,FILLED,REPMES,LOGF
      LOGICAL INIT,DREAD,PARSE

*     variables to hold missing values
      REAL RM_D, RM
      INTEGER IM_D, IM
      DOUBLE PRECISION DM_D, DM, TM_D, TM
      CHARACTER*80 CM_D, CM
      LOGICAL LM_D, LM

*     message output
      LOGICAL TOSCR, TOLOG
      INTEGER UNLOG

*     used functions
      INTEGER IFINDC, ISTART
      LOGICAL FLEXIST

      SAVE
      DATA IUNDAT/0/, IUNREP/0/
      DATA ONFILE /'CHARACTER','INTEGER  ','REAL     ',
     $             'LOGICAL  ','DATE_TIME'/
      DATA GETONF /'CIFLT'/
      DATA ASKED  /'CHARACTER       ','DOUBLE PRECISION',
     $             'INTEGER         ','REAL            ',
     $             'LOGICAL         ','DATE_TIME       '/
      DATA GETSCA /'sa'/
      DATA SCARR  /'scalar','array'/
      DATA REQTYP /'C','F','I','F','L','T'/
      DATA ISLOC/0/, INIDAT/.FALSE./, REPL/.FALSE./, THERE/.FALSE./
      DATA ON/.TRUE./, OFF/.FALSE./, INFILS/0/, GIVEN/.FALSE./
      DATA ERRTXT /'Routine RDDATA, called by '/
      DATA INIT /.FALSE./
      DATA RM_D /-99.99/ , IM_D /-99/, DM_D /-99.99D0/,
     &     CM_D /'- MISSING -'/, LM_D /.FALSE./, TM_D /-99.99D0/
      DATA (MESSAG(I),I=1,14)
     $/'Variable name too long                    ',
     $ 'Under ITASK=5 declared length should be 1 ',
     $ 'Requested number of values not positive   ',
     $ 'Variable name not in data file            ',
     $ 'On data file this is a CHARACTER string   ',
     $ 'On data file this variable is INTEGER     ',
     $ 'On data file this variable is REAL        ',
     $ 'On data file this variable is LOGICAL     ',
     $ 'On data file this is a DATE_TIME          ',
     $ 'On data file this variable is an array    ',
     $ 'On data file this variable is scalar      ',
     $ 'Array on data file not of requested size  ',
     $ 'Array on data file too large              ',
     $ 'The rerun file contains a CHARACTER string'/
      DATA (MESSAG(I),I=15,28)
     $/'The rerun file contains an INTEGER        ',
     $ 'The rerun file contains a REAL variable   ',
     $ 'The rerun file contains a LOGICAL variable',
     $ 'The rerun file contains a DATE_TIME       ',
     $ 'On rerun file this variable is an array   ',
     $ 'On rerun file this variable is scalar     ',
     $ 'Array on rerun file not of requested size ',
     $ 'Array on rerun file too large to fit      ',
     $ 'Single precision underflow on data file   ',
     $ 'Single precision overflow on data file    ',
     $ 'String(s) on data file too long           ',
     $ 'Single precision underflow on rerun file  ',
     $ 'Single precision overflow on rerun file   ',
     $ 'String(s) on rerun file too long          '/


      IF (.NOT.INIT) THEN
*        calculate IXLMAX as the maximum type length
         IXLMAX = MAX(IIL,IRL,ILL)

*        assign default missing values
         RM = RM_D
         IM = IM_D
         DM = DM_D
         CM = CM_D
         LM = LM_D
         TM = TM_D

*        copy set number to common block
         ISCOM  = 0

         INFDAT = 0
         INFREP = 0

         INIT = .TRUE.
      END IF

      IF (ITASK.EQ.1) THEN
*        read rerun file
         IF (REPL) THEN
*           there may be a previous TMP file open
            INQUIRE (UNIT=IUNREP, OPENED=OPUNIT, NAME=INQNAM)
            IF (OPUNIT) THEN
*              "own" tmp file is closed
               CALL UPPERC (INQNAM)
               JLQ = LEN_TRIM(INQNAM)
               JLD = LEN_TRIM(REPTMP)
               IF (JLQ.GE.JLD) THEN
                  IF (REPTMP(1:JLD).EQ.INQNAM(JLQ-JLD+1:JLQ))
     &             CLOSE(IUNREP)
               END IF
            END IF
         END IF

         I = LEN_TRIM (FILNAM)
         EMPTY = I.EQ.0
         IF (.NOT.EMPTY) THERE = FLEXIST (FILNAM)

         IF (EMPTY .OR. .NOT.THERE) THEN
*           disactivate replacements
            REPL  = OFF
            IS    = 0
            ISLOC = 0

*           delete swap files if they exist, release units
            CALL SWPI4 (3,'RD\IARREP',0,0,0,0)
            CALL SWPI4 (3,'RD\IPTREP',0,0,0,0)

            CALL AMBUSY (1,'RDFROM',ISLOC)
            RETURN
         END IF

*        set (new) local unit number
         IUNREP = IUNIT

*        desired message output
         CALL MESSINQ (TOSCR, TOLOG, UNLOG)

*        local logfile settings
         IF (TOLOG) THEN
*           use global logfile unit
            LOGR = ON
            IULR = UNLOG
         ELSE
*           user supplied logfile (?)
            LOGR   = IULOG.GT.0
            IULR   = IULOG
         END IF

*        get local copy of filename
         RFIL = FILNAM
         ILR  = LEN_TRIM (RFIL)
         CALL UPPERC (RFIL)
         CALL EXTENS (RFIL,'TMP',1,REPTMP)

*        analyze file and check for errors
         CALL RDINDX (IWAR,IUNREP,ON,TOSCR,LOGR,IULR,FILNAM,REPTMP,
     $    REPLIS,REPTYP,REPARR,ILNREP,
     $    IARREP,IPTREP,ILPREP,INFREP,INSETS)
         IF (IWAR.GT.0) THEN
            IF (TOSCR) WRITE (*,'(/,/,3A,I4)')
     $      ' Number of errors in rerun file ',RFIL(1:ILR),':',IWAR
            IF (LOGR) WRITE (IULR,'(/,/,3A,I4)')
     $      ' Number of errors in rerun file ',RFIL(1:ILR),':',IWAR
            CALL FATALERR (' ',' ')
         END IF

*        reset set number and record number ; set replace flag
         ISLOC  = 0
         IRREP  = 0
         REPL   = ON

*        message to AMBUSY ; set 0 selected
         CALL AMBUSY (1,'RDFROM',ISLOC)

*        return number of sets
         IS = INSETS

         IF (INSETS.GT.0) THEN
*           report to logfile
            IF (LOGR) THEN
               WRITE (IULR,'(/,1X,3A,I5,A,I3,A,/,1X,A,/,1X,A)')
     $          'File ',RFIL(1:ILR),' contains',INSETS,' set(s) of',
     $           INFREP,' variable(s):',
     $          'type sc/ar variable name',
     $          '---- ----- -------------'
               DO 10 I=1,INFREP
                  LXNM31 = REPLIS(I)
                  WRITE (IULR,'(2X,A,5X,A,3X,A)')
     $             REPTYP(I),REPARR(I),REPLIS(I)
10             CONTINUE

               WRITE (IULR,'(/,1X,A)')
     $          '= Data file contents (set 0) selected ='
            END IF

*           reset replace counters
            DO 20 I=1,INFREP
               IRCHK(I) = 0
20          CONTINUE
         END IF

*        copy set number to common block
         ISCOM  = ISLOC

      ELSE IF (ITASK.EQ.2) THEN
*        change set number
         IF (IS.EQ.ISLOC) RETURN
         IF (.NOT.REPL) CALL FATALERR ('RDFROM','no rerun file active')
         FATAL = NREQ.GT.0

         IF (ISLOC.GE.1) THEN
*           check variable use of previous set
            DO 30 I=1,INFREP
               IF (IRCHK(I).EQ.0) THEN
                  ILX = LEN_TRIM (REPLIS(I))
                  IF (LOGR) WRITE (IULR,'(3X,2A)')
     $             REPLIS(I)(1:ILX),'   !!!!!!   NOT USED   !!!!!!'

                  IF (FATAL) THEN
                     IF (TOSCR)
     $                WRITE (*,'(/,1X,A,I5,A,I5,3A,/,1X,3A,I5,A)')
     $                '- Moving from set',ISLOC,' to set',IS,' of ',
     $                RFIL(1:ILR),':','  Variable ',
     $                REPLIS(I)(1:ILX),' of set',ISLOC,' was NOT USED'
                     IF (LOGR)
     $                WRITE (IULR,'(/,1X,A,I5,A,I5,3A,/,1X,3A,I5,A)')
     $                '- Moving from set',ISLOC,' to set',IS,' of ',
     $                RFIL(1:ILR),':','  Variable ',
     $                REPLIS(I)(1:ILX),' of set',ISLOC,' was NOT USED'
                     CALL FATALERR (' ',' ')
                  END IF
               END IF
               IRCHK(I) = 0
30          CONTINUE
         END IF

*        change set
         IF (IS.GE.0 .AND. IS.LE.INSETS) THEN
            ISLOC = IS
         ELSE
            CALL FATALERR ('RDFROM','illegal set number requested')
         END IF

*        message to AMBUSY ; set IS selected
         CALL AMBUSY (1,'RDFROM',IS)

*        report to logfile
         IF (IS.EQ.0 .AND. LOGR) WRITE (IULR,'(/,1X,A)')
     $    '= Data file contents (set 0) selected ='
         IF (IS.GE.1 .AND. LOGR) WRITE (IULR,'(/,1X,A,I5,3A)')
     $    '= Set',IS,' of rerun file ',RFIL(1:ILR),' selected ='

*        copy set number to common block
         ISCOM = ISLOC

      ELSE IF (ITASK.EQ.3 .OR. ITASK.EQ.4) THEN
*        initialize data file
         IF (INIDAT) THEN
*           close old unit if left open
            INQUIRE (UNIT=IUNDAT, OPENED=OPUNIT, NAME=INQNAM)
            IF (OPUNIT) THEN
*              "own" tmp file is closed
               CALL UPPERC (INQNAM)
               JLQ = LEN_TRIM(INQNAM)
               JLD = LEN_TRIM(DATTMP)
               IF (JLQ.GE.JLD) THEN
                  IF (DATTMP(1:JLD).EQ.INQNAM(JLQ-JLD+1:JLQ))
     &             CLOSE(IUNDAT)
               END IF
            END IF
         END IF

*        local unit number
         IUNDAT = IUNIT

*        desired message output
         CALL MESSINQ (TOSCR, TOLOG, UNLOG)

*        local logfile settings
         IF (TOLOG) THEN
*           use global logfile unit
            LOGD = ON
            IULD = UNLOG
         ELSE
*           user supplied logfile (?)
            LOGD = IULOG.GT.0
            IULD = IULOG
         END IF

*        get local copy of filename
         DFIL = FILNAM
         ILD  = LEN_TRIM (DFIL)
         CALL UPPERC (DFIL)
*        position of last part of filename for logfile output
         IPS = MAX (ILD-11,1)

*        file name in list ?
         INLIST = IFINDC (FILLIS,INFDEC,1,INFILS,DFIL)

         IF (INLIST.EQ.0.AND.INFILS.LT.INFDEC) THEN

*           new file name does not occur in list and there
*           is place to store it
            INFILS         = INFILS + 1
            FILLIS(INFILS) = DFIL

            PARSE = .TRUE.

*           build tmp filename
            DATTMP = 'RD$'
            ITMP1 = 3
            CALL ADDINF (DATTMP,ITMP1,INFILS,'I5.5')
            CALL ADDSTR (DATTMP,ITMP1,'.TMP')
            TMPLIS(INFILS) = DATTMP

         ELSE IF (INLIST.EQ.0.AND.INFILS.GE.INFDEC) THEN

            PARSE = .TRUE.

*           default tmp file name
            DATTMP = 'RD$00000.TMP'

*           filename not in list, but list is full
            IF (.NOT.GIVEN.AND.ITASK.EQ.3) THEN
*              the new name is not included
               IF (TOSCR) WRITE (*,'(9(/,2A),2(/,A))')
     $          ' MESSAGE from RDDATA: After reading with RDINIT',
     $          ' the data',' file ',DFIL(IPS:ILD),' the list of',
     $          ' file names appears to be full. Hence, the name',
     $          ' of the analyzed file cannot be included in the',
     $          ' list, which',' implies that RDINIT will not',
     $          ' recover the file contents from',' temporary file',
     $          ' when you initialize the same file once again.',
     $          ' When you, in a rerun loop for instance,',
     $          ' repeatedly use this',' file, the data will be',
     $          ' parsed again and again. To avoid that,',
     $          ' use RDPARS for files you initialize only once',
     $          ' and/or increase',' PARAMETER INFDEC in RDDATA.',
     $          ' This message will not be repeated for other files.'
               IF (LOGD) WRITE (IULD,'(9(/,2A),2(/,A))')
     $          ' MESSAGE from RDDATA: After reading with RDINIT',
     $          ' the data',' file ',DFIL(IPS:ILD),' the list of',
     $          ' file names appears to be full. Hence, the name',
     $          ' of the analyzed file cannot be included in the',
     $          ' list, which',' implies that RDINIT will not',
     $          ' recover the file contents from',' temporary file',
     $          ' when you initialize the same file once again.',
     $          ' When you, in a rerun loop for instance,',
     $          ' repeatedly use this',' file, the data will be',
     $          ' parsed again and again. To avoid that,',
     $          ' use RDPARS for files you initialize only once',
     $          ' and/or increase',' PARAMETER INFDEC in RDDATA.',
     $          ' This message will not be repeated for other files.'
               GIVEN = .TRUE.
            END IF

         ELSE IF (INLIST.GT.0) THEN
*           filename was found in list, what to do ?
            DATTMP = TMPLIS(INLIST)
            IF (ITASK.EQ.3) THEN
*              call was from rdinit, recover when tmp file exists,
*              parse when it does not exist
               IF (FLEXIST (DATTMP)) THEN
                  PARSE = .FALSE.
               ELSE
                  PARSE = .TRUE.
               END IF
            ELSE IF (ITASK.EQ.4) THEN
*              call was from rdpars, parse anyway
               PARSE = .TRUE.
            END IF
         END IF

         IF (PARSE) THEN
*           analyze file and check for errors
            CALL RDINDX (IWAR,IUNDAT,OFF,TOSCR,LOGD,IULD,FILNAM,DATTMP,
     $       DATLIS,DATTYP,DATARR,ILNDAT,
     $       IARDAT,IPTDAT,ILNDAT,INFDAT,IDUM)
            IF (IWAR.GT.0) THEN
               IF (TOSCR) WRITE (*,'(/,/,3A,I4)')
     $         ' Number of errors in data file ',DFIL(1:ILD),':',IWAR
               IF (LOGD) WRITE (IULD,'(/,/,3A,I4)')
     $         ' Number of errors in data file ',DFIL(1:ILD),':',IWAR
               CALL FATALERR (' ',' ')
            ELSE
*              save index
               IDUM = 1
               CALL RDTMP2 (1,IUNDAT,DATLIS,
     $          DATTYP,DATARR,ILNDAT,IARDAT,IPTDAT,ILNDAT,INFDAT,IDUM)
               IF (LOGD) WRITE (IULD,'(3A,I4,A)')
     $          ' Data file   ',DFIL(1:ILD),
     $          ' with',INFDAT,' variables parsed by RDINDX'
            END IF

         ELSE
*           filename previously used ; open available temporary
            IRECL = ILBUF + IIL
            CALL FOPENG (IUNIT,DATTMP,'OLD','UD',IRECL,' ')

*           recover pointer info
            CALL RDTMP2 (2,IUNDAT,DATLIS,
     $       DATTYP,DATARR,ILNDAT,IARDAT,IPTDAT,ILNDAT,INFDAT,IDUM)

            IF (LOGD) WRITE (IULD,'(1X,3A)') 'Contents of ',
     $       DFIL(1:ILD),' recovered from TMP file'
         END IF

*        reset record number and set flags
         IRDAT  = 0
         INIDAT = ON
         REPMES = OFF

*        when the rerun facility is active
*        check type and array/scalar consistency
         IF (REPL) THEN
*           message file
            LOGF = LOGR .OR. LOGD
            IF (LOGR) THEN
*              message to rerun logfile
               IUL = IULR
            ELSE IF (LOGD) THEN
*              message to data file logfile
               IUL = IULD
            END IF
            DO 60 I=1,INFDAT
               LXNM31 = DATLIS(I)
               INR = IFINDC (REPLIS,ILNREP,1,INFREP,LXNM31)
               IF (INR.GT.0) THEN
                  IF (DATTYP(I).NE.'-' .AND. REPTYP(INR).NE.'-') THEN
*                    typed on either datafile or rerun file
                     IF (DATTYP(I).NE.REPTYP(INR)) THEN
*                       type inconsistency
                        ITD = INDEX (GETONF,DATTYP(I))
                        ITR = INDEX (GETONF,REPTYP(INR))
                        ILX = LEN_TRIM (LXNM31)
                        IT1 = LEN_TRIM (ONFILE(ITD))
                        IT2 = LEN_TRIM (ONFILE(ITR))

                        IF (TOSCR) WRITE (*,'(/,1X,6A,/,5A)')
     $                   ONFILE(ITD)(1:IT1),' variable ',LXNM31(1:ILX),
     $                   ' on data file ',DFIL(1:ILD),' occurs',
     $                   ' on rerun file ',RFIL(1:ILR),' as ',
     $                     ONFILE(ITR)(1:IT2),' variable'
                        IF (LOGF) WRITE (IUL,'(/,1X,6A,/,5A)')
     $                   ONFILE(ITD)(1:IT1),' variable ',LXNM31(1:ILX),
     $                   ' on data file ',DFIL(1:ILD),' occurs',
     $                   ' on rerun file ',RFIL(1:ILR),' as ',
     $                     ONFILE(ITR)(1:IT2),' variable'
                        CALL FATALERR (' ',' ')
                     END IF
                  END IF

                  IF (DATARR(I).NE.REPARR(INR)) THEN
*                    type inconsistency
                     ITD = INDEX (GETSCA,DATARR(I))
                     ITR = INDEX (GETSCA,REPARR(INR))
                     ILX = LEN_TRIM (LXNM31)
                     IT1 = LEN_TRIM (SCARR(ITD))
                     IT2 = LEN_TRIM (SCARR(ITR))

                     IF (TOSCR) WRITE (*,'(/,1X,6A,/,5A)')
     $                SCARR(ITD)(1:IT1),' variable ',LXNM31(1:ILX),
     $                ' on data file ',DFIL(1:ILD),' occurs',
     $                ' on rerun file ',RFIL(1:ILR),' as ',
     $                  SCARR(ITR)(1:IT2),' variable'
                     IF (LOGF) WRITE (IUL,'(/,1X,6A,/,5A)')
     $                SCARR(ITD)(1:IT1),' variable ',LXNM31(1:ILX),
     $                ' on data file ',DFIL(1:ILD),' occurs',
     $                ' on rerun file ',RFIL(1:ILR),' as ',
     $                  SCARR(ITR)(1:IT2),' variable'
                     CALL FATALERR (' ',' ')
                  END IF
               END IF
60          CONTINUE
         END IF


      ELSE IF (ITASK.EQ.5 .OR. ITASK.EQ.6 .OR. ITASK.EQ.7) THEN
*        get data ; error checks
         IF (.NOT.INIDAT) CALL FATALERR ('RDDATA','no data file active')

*        classify call
         SINGLE = ITASK.EQ.5
         UNKNOW = ITASK.EQ.6
         FIXED  = ITASK.EQ.7

*        copy values for missing data to external variables
         xDM = DM
         xRM = RM
         xIM = IM
         xCM = CM
         xLM = LM

         IF (VARTYP.EQ.'C') THEN
*           get character variable
            IVT   = 1
            NDEC  = NCDEC
            IXL   = IRL
            NELMT = LEN(CX(1))
         ELSE IF (VARTYP.EQ.'D') THEN
*           get double precision variable
            IVT  = 2
            NDEC = NDDEC
            IXL  = IRL
         ELSE IF (VARTYP.EQ.'I') THEN
*           get integer variable
            IVT  = 3
            NDEC = NIDEC
            IXL  = IRL
         ELSE IF (VARTYP.EQ.'R') THEN
*           get real variable
            IVT  = 4
            NDEC = NRDEC
            IXL  = IRL
         ELSE IF (VARTYP.EQ.'L') THEN
*           get logical variable
            IVT  = 5
            NDEC = NLDEC
            IXL  = IRL
         ELSE IF (VARTYP.EQ.'T') THEN
*           get date_time
            IVT   = 6
            NDEC  = NDDEC
            IXL   = IRL
         END IF

*        get local copy of significant part of name in uppercase and
*        the length of the name in ILX
         ISX    = MAX (1,ISTART (XNAME))
         ILX    = MAX (1,LEN_TRIM (XNAME))
         LXNM31 = XNAME(ISX:ILX)
         CALL UPPERC (LXNM31)

*        check data file index
         IND  = IFINDC (DATLIS,ILNDAT,1,INFDAT,LXNM31)

*        check rerun file for overwrite facility
         INR = 0
*        variable occurs on active rerun file ?
         IF (REPL) INR = IFINDC (REPLIS,ILNREP,1,INFREP,LXNM31)
*        overwrite data file value(s) ?
         OVERWR = INR.GT.0 .AND. ISLOC.GT.0

*        reset error ; length of name
         IMES   = 0

         IF (ILX.GT.31) THEN
*           requested name too long
            IMES = 1
         ELSE IF (SINGLE .AND. NDEC.NE.1) THEN
*           under ITASK=5 declared length should be 1
            IMES = 2
         ELSE IF (FIXED .AND. NREQ.LE.0) THEN
*           requested number of values is not positive
            IMES = 3
         ELSE IF (IND.EQ.0) THEN
*           name does not occur in data file
            IMES = 4
         ELSE IF (DATTYP(IND).NE.'-' .AND.
     $            DATTYP(IND).NE.REQTYP(IVT)) THEN
*           variable type mismatch ; error 5,6,7,8 or 9
            IMES = 4 + INDEX (GETONF,DATTYP(IND))
         ELSE
*           number of values on file N
            N = IARDAT(IND)

*           error checks
            IF (SINGLE .AND. DATARR(IND).EQ.'a') THEN
               IMES = 10
            ELSE IF ((UNKNOW .OR. FIXED) .AND. DATARR(IND).EQ.'s') THEN
               IMES = 11
            ELSE IF (FIXED .AND. N.NE.NREQ .AND. .NOT.OVERWR) THEN
               IMES = 12
            ELSE IF (N.GT.NDEC) THEN
               IMES = 13
            END IF

            IF (IMES.EQ.0 .AND. INR.GT.0) THEN
*              variable occurs on active rerun file ; type checking,
*              also in case set 0 is selected and OVERWR is .false.
               IF (REPTYP(INR).NE.'-' .AND.
     $             REPTYP(INR).NE.REQTYP(IVT)) THEN
*                 variable type mismatch ; error 14,15,16,17 or 18
                  IMES = 13 + INDEX (GETONF,REPTYP(INR))
               END IF
            END IF

            IF (IMES.EQ.0 .AND. OVERWR) THEN
*              the other checks only in case of overwriting
*              get byte pointer and array length N
               IPP = (ISLOC-1) * INFREP + INR
               IF (IPP.LE.ILPREP) THEN
                  N = IARREP(IPP)
               ELSE
                  CALL SWPI4 (2,'RD\IARREP',0,ILPREP,IPP,N)
               END IF

*              error checks
               IF (SINGLE .AND. REPARR(INR).EQ.'a') THEN
                  IMES = 19
               ELSE IF ((UNKNOW .OR. FIXED) .AND.
     $          REPARR(INR).EQ.'s') THEN
                  IMES = 20
               ELSE IF (FIXED .AND. N.NE.NREQ) THEN
                  IMES = 21
               ELSE IF (N.GT.NDEC) THEN
                  IMES = 22
               END IF
            END IF
         END IF

         IF (IMES.EQ.0) THEN
            IF (.NOT.OVERWR) THEN
*              read data from normal data file ; initial value pointer
               IPNT = IPTDAT(IND)
               IEL  = 0

80             IF (IEL.LT.N) THEN
*                 read repeat value
                  IREC = 1 + IPNT/ILBUF

                  IF (IREC.NE.IRDAT) THEN
*                    read new record from file
                     READ (IUNDAT,REC=IREC) RDBUF,PNTDNX
                     IRDAT = IREC
                  END IF

*                 set position of repeat value
                  IWN = MOD(IPNT,ILBUF) / IRL
*                 get repeat value
                  IREP = NINT(RDBUF(IWN))
*                 after repeater set pointer to first data item
                  IPNT = IPNT + MAX(IXL,IRL)
*                 end of record buffer reached ?
                  IF (MOD(IPNT,ILBUF).EQ.0) IPNT=PNTDNX

                  IF (IREP.GT.0) THEN
*                    read data item(s) until complete value found
                     FOUND  = OFF
                     J      = 0

70                   IF (.NOT.FOUND) THEN
*                       read data item
                        IREC = 1 + IPNT/ILBUF

                        IF (IREC.NE.IRDAT) THEN
*                          read new record from file
                           READ (IUNDAT,REC=IREC) RDBUF,PNTDNX
                           IRDAT = IREC
                        END IF

*                       read value ; position in record
                        IWN = MOD(IPNT,ILBUF) / IXL

*                       get data item ; FOUND flags complete value
                        IF (VARTYP.EQ.'R') THEN
*                          single precision real
C                      write (*,*) rdbuf(iwn)
                           AD = ABS(RDBUF(IWN))
                           IF (AD.EQ.0.0D0 .OR.
     $                      (AD.GT.1.0D-38 .AND. AD.LT.1.0D+38)) THEN
*                             accept single precision value
                              DO 400 I=IEL+1,IEL+IREP
                                 RX(I) = REAL (RDBUF(IWN))
C                      write (*,*) rx(i)
400                           CONTINUE
                           ELSE IF (AD.LT.1.0D-38) THEN
*                             underflow
                              IMES = 23
                           ELSE  IF (AD.GT.1.0D+38) THEN
*                             overflow
                              IMES = 24
                           END IF
                           FOUND = ON
                        ELSE IF (VARTYP.EQ.'I') THEN
*                          integer
                           DO 401 I=IEL+1,IEL+IREP
                              IX(I) = NINT(RDBUF(IWN))
401                        CONTINUE
                           FOUND   = ON
                        ELSE IF (VARTYP.EQ.'D' .OR. VARTYP.EQ.'T') THEN
*                          double precision real or date_time
                           DO 404 I=IEL+1,IEL+IREP
                              DX(I) = RDBUF(IWN)
C                              write (*,*) DX(i)
404                        CONTINUE
C                           read (*,*)
                           FOUND   = ON
                        ELSE IF (VARTYP.EQ.'L') THEN
*                          logical
                           DO 405 I=IEL+1,IEL+IREP
                              if (RDBUF(IWN) > 0.0) then
                                 LX(I) = .true.
                              else
                                 LX(I) = .false.
                              end if
405                        CONTINUE
                           FOUND   = ON
                        ELSE IF (VARTYP.EQ.'C') THEN
*                          character string
                           BYTE = CHAR(MOD(NINT(RDBUF(IWN)),256))
                           IF (BYTE.NE.CHAR(0)) THEN
                              IF (J.EQ.0) THEN
*                                first character found ; empty CX
                                 DO 403 I=IEL+1,IEL+IREP
                                    CX(I) = ' '
403                              CONTINUE
                              END IF
                              J = J + 1
                              IF (J.LE.NELMT) THEN
*                                accept byte
                                 DO 402 I=IEL+1,IEL+IREP
                                    CX(I)(J:J) = BYTE
402                              CONTINUE
                              ELSE
*                                error
                                 IMES = 25
                              END IF
                           ELSE
                              FOUND = ON
                           END IF
                        END IF

*                       next position on file
                        IPNT = IPNT + IXL
*                       end of record buffer reached ?
                        IF (MOD(IPNT,ILBUF).EQ.0) IPNT=PNTDNX

*                       value found ; set element counter
                        IF (FOUND) IEL=IEL+IREP
                     GOTO 70
                     END IF

                  ELSE
*                    missing value: do not read from file
                     DO 408 I=IEL+1,IEL-IREP
                        IF (VARTYP.EQ.'R') THEN
*                          default single precision real
                           RX(I) = RM
                        ELSE IF (VARTYP.EQ.'I') THEN
*                          default integer
                           IX(I) = IM
                        ELSE IF (VARTYP.EQ.'D') THEN
*                          default double precision real
                           DX(I) = DM
                        ELSE IF (VARTYP.EQ.'C') THEN
*                          default character string
                           CX(I) = CM
                        ELSE IF (VARTYP.EQ.'L') THEN
*                          default logical
                           LX(I) = LM
                        ELSE IF (VARTYP.EQ.'T') THEN
*                          default date_time
                           DX(I) = TM
                        END IF
408                  CONTINUE

*                    increase element counter
                     IEL=IEL-IREP
                  END IF

*                 next repeater + value at multiple of IXLMAX bytes
                  IPNT = IXLMAX * ((IXLMAX-1 + IPNT)/IXLMAX)
*                 end of record buffer reached ?
                  IF (MOD(IPNT,ILBUF).EQ.0) IPNT=PNTDNX
               GOTO 80
               END IF

            ELSE
*              read data from rerun file ; initial value pointer
               IF (IPP.LE.ILPREP) THEN
                  IPNT = IPTREP(IPP)
               ELSE
                  CALL SWPI4 (2,'RD\IPTREP',0,ILPREP,IPP,IPNT)
               END IF

               IEL  = 0

100            IF (IEL.LT.N) THEN
*                 read repeat value
                  IREC = 1 + IPNT/ILBUF

                  IF (IREC.NE.IRREP) THEN
*                    read new record from file
                     READ (IUNREP,REC=IREC) RRBUF,PNTRNX
                     IRREP = IREC
                  END IF

*                 set position of repeat value
                  IWN = MOD(IPNT,ILBUF) / IRL
*                 get repeat value
                  IREP = NINT(RRBUF(IWN))
*                 after repeater set pointer to first data item
                  IPNT = IPNT + MAX(IXL,IRL)
*                 end of record buffer reached ?
                  IF (MOD(IPNT,ILBUF).EQ.0) IPNT=PNTRNX

                  IF (IREP.GT.0) THEN
*                    read data item(s) until complete value found
                     DREAD  = .TRUE.
                     FOUND  = OFF
                     J      = 0

90                   IF (.NOT.FOUND) THEN
*                       read data item
                        IREC = 1 + IPNT/ILBUF

                        IF (IREC.NE.IRREP) THEN
*                          read new record from file
                           READ (IUNREP,REC=IREC) RRBUF,PNTRNX
                           IRREP = IREC
                        END IF

*                       read value ; position in record
                        IWN = MOD(IPNT,ILBUF) / IXL

*                       get data item ; FOUND flags complete value
                        IF (VARTYP.EQ.'R') THEN
*                          single precision real
                           AD = ABS(RRBUF(IWN))
                           IF (AD.EQ.0.0D0 .OR.
     $                      (AD.GT.1.0D-38 .AND. AD.LT.1.0D+38)) THEN
*                             accept single precision value
                              DO 500 I=IEL+1,IEL+IREP
                                 RX(I) = REAL (RRBUF(IWN))
500                           CONTINUE
                           ELSE IF (AD.LT.1.0D-38) THEN
*                             underflow
                              IMES = 26
                           ELSE  IF (AD.GT.1.0D+38) THEN
*                             overflow
                              IMES = 27
                           END IF
                           FOUND = ON
                        ELSE IF (VARTYP.EQ.'I') THEN
*                          integer
                           DO 501 I=IEL+1,IEL+IREP
                              IX(I) = NINT(RRBUF(IWN))
501                        CONTINUE
                           FOUND   = ON
                        ELSE IF (VARTYP.EQ.'D' .OR. VARTYP.EQ.'T') THEN
*                          double precision real or date_time
                           DO 504 I=IEL+1,IEL+IREP
                              DX(I) = RRBUF(IWN)
504                        CONTINUE
                           FOUND   = ON
                        ELSE IF (VARTYP.EQ.'L') THEN
*                          logical
                           DO 505 I=IEL+1,IEL+IREP
                              if (RRBUF(IWN) > 0.0) then
                                 LX(I) = .true.
                              else
                                 LX(I) = .false.
                              end if
505                        CONTINUE
                           FOUND   = ON
                        ELSE IF (VARTYP.EQ.'C') THEN
*                          character string
                           BYTE = CHAR(MOD(NINT(RRBUF(IWN)),256))
                           IF (BYTE.NE.CHAR(0)) THEN
                              IF (J.EQ.0) THEN
*                                first character found ; empty CX
                                 DO 503 I=IEL+1,IEL+IREP
                                    CX(I) = ' '
503                              CONTINUE
                              END IF
                              J = J + 1
                              IF (J.LE.NELMT) THEN
*                                accept byte
                                 DO 502 I=IEL+1,IEL+IREP
                                    CX(I)(J:J) = BYTE
502                              CONTINUE
                              ELSE
*                                error
                                 IMES = 28
                              END IF
                           ELSE
                              FOUND = ON
                           END IF
                        END IF

*                       next position on file
                        IPNT = IPNT + IXL
*                       end of record buffer reached ?
                        IF (MOD(IPNT,ILBUF).EQ.0) IPNT=PNTRNX

*                       value found ; set element counter
                        IF (FOUND) IEL=IEL+IREP
                     GOTO 90
                     END IF

                  ELSE
*                    missing value: do not read from file
                     DREAD = .FALSE.
                     DO 508 I=IEL+1,IEL-IREP
                        IF (VARTYP.EQ.'R') THEN
*                          default single precision real
                           RX(I) = RM
                        ELSE IF (VARTYP.EQ.'I') THEN
*                          default integer
                           IX(I) = IM
                        ELSE IF (VARTYP.EQ.'D') THEN
*                          default double precision real
                           DX(I) = DM
                        ELSE IF (VARTYP.EQ.'C') THEN
*                          default character string
                           CX(I) = CM
                        ELSE IF (VARTYP.EQ.'L') THEN
*                          default logical
                           LX(I) = LM
                        ELSE IF (VARTYP.EQ.'T') THEN
*                          default date_time
                           DX(I) = TM
                        END IF
508                  CONTINUE

*                    increase element counter
                     IEL=IEL-IREP
                  END IF

*                 next repeater + value at multiple of IXLMAX bytes
                  IPNT = IXLMAX * ((IXLMAX-1 + IPNT)/IXLMAX)
*                 end of record buffer reached ?
                  IF (MOD(IPNT,ILBUF).EQ.0) IPNT=PNTRNX
               GOTO 100
               END IF

*              count overwriting and write message
               IRCHK(INR) = IRCHK(INR) + 1
               IF (LOGR.AND.IMES.EQ.0) THEN
*                 write report to logfile
                  IF (.NOT.REPMES) THEN
                     WRITE (IULR,'(3X,2A,I5,A)')
     $                'Variable values have been overwritten by ',
     $                'rerun set',ISLOC,':'
                     REPMES = ON
                  END IF

                  IF (DREAD) THEN
*                    data have been read
                     ARROW = ' ----->'
                  ELSE
*                    missing value
                     ARROW = ' -DEF->'
                  END IF

                  WRITE (LINE,'(3X,2A)') LXNM31(1:ILX),ARROW
                  PPP    = ' '
                  I      = 0
                  IVP    = LEN_TRIM(LINE)
                  IVL    =  LEN(LINE) - 1
                  FILLED = OFF

110               IF (I.LT.N .AND. .NOT.FILLED) THEN
*                    write next value to help string
                     IF (VARTYP.EQ.'D'.OR.VARTYP.EQ.'T') THEN
                        WRITE (VALUE,'(1P,G12.4)') DX(I+1)
                     ELSE IF (VARTYP.EQ.'R') THEN
                        WRITE (VALUE,'(1P,G12.4)') RX(I+1)
                     ELSE IF (VARTYP.EQ.'I') THEN
                        WRITE (VALUE,'(I12)') IX(I+1)
                     ELSE IF (VARTYP.EQ.'C') THEN
                        J = LEN_TRIM (CX(I+1))
                        J = MIN (J,10)
                        VALUE = CHAR(39)//CX(I+1)(1:J)//CHAR(39)
                     ELSE IF (VARTYP.EQ.'L') THEN
                        IF (LX(I+1)) THEN
                           VALUE = '.TRUE.'
                        ELSE
                           VALUE = '.FALSE.'
                        END IF
                     END IF

*                    length of text and position of last character
                     IV1 = ISTART (VALUE)
                     IV2 = LEN_TRIM (VALUE)
                     IPN = IVP + 2 + IV2 - IV1

                     FILLED = IPN.GT.IVL
                     IF (.NOT.FILLED) THEN
*                       add value to message
                        LINE(IVP+2:IPN) = VALUE(IV1:IV2)
                        I   = I + 1
                        IVP = IPN
                        IF (I.LT.N) THEN
*                          add comma
                           IVP = IVP + 1
                           LINE(IVP:IVP) = ','
                        END IF
                     END IF
                  GOTO 110
                  END IF

*                 write replacement message to log file
                  IF (I.LT.N) PPP='...'
                  WRITE (IULR,'(2A)') LINE(1:IVP),PPP
               END IF
            END IF

*           return number of values on file when requested to do so
            IF (UNKNOW) NREQ = N
         END IF

         IF (IMES.GT.0) THEN
*           error message
            LOGF = (REPL.AND.LOGR) .OR. LOGD
            IF (REPL.AND.LOGR) THEN
*              message to rerun logfile
               IUL = IULR
            ELSE IF (LOGD) THEN
*              message to data file logfile
               IUL = IULD
            END IF

            IF (SINGLE) THEN
*              a single value into a non-array variable
               WHAT   = 'a single value'
               TARGET = ' variable '
               ISTREC = 2
               RECORD(1) = ' '
            ELSE IF (UNKNOW) THEN
*              requested number of values unspecified
               WHAT   = 'an unspecified number of values'
               TARGET = ' array '
               ISTREC = 1
            ELSE IF (FIXED) THEN
*              number of values specified
               WRITE (WHAT,'(A,I6,A)') 'precisely',NREQ,' values'
               TARGET = ' array '
               ISTREC = 1
            END IF
            ILA = LEN_TRIM (ASKED(IVT))
            ILW = LEN_TRIM (WHAT)
            ILT = LEN_TRIM (TARGET) + 1

*           error message
            RECORD(ISTREC) =
     $       ERRTXT//CALPRG//', attempts to read'
            RECORD(ISTREC+1) =
     $       WHAT(1:ILW)//' from file '//DFIL(1:ILD)
            RECORD(ISTREC+2) =
     $       'into '//ASKED(IVT)(1:ILA)//TARGET(1:ILT)//LXNM31(1:ILX)
            IF (.NOT.SINGLE) WRITE (RECORD(4),'(A,I5,A)')
     $       '(with declared length',NDEC,') :'
            IF (TOSCR) WRITE (*,'(4(/,1X,A))') RECORD
            IF (LOGF) WRITE (IUL,'(6(/,1X,A))')
     $       'INPUT ERROR','===========',RECORD

*           in case of rerun value
            IF (OVERWR) THEN
               RECORD(1) =
     $    '   | RDDATA was instructed to replace data file contents by'
               WRITE (RECORD(2),'(A,I5,3A)')
     $    '   | value(s) in set',ISLOC,' of rerun file ',RFIL(1:ILR),'.'
               RECORD(3) =
     $    '   | Data are replaced only if variable type is correct '
               RECORD(4) =
     $    '   | on both data file and rerun file.'
               IF (TOSCR) WRITE (*,'(1X,A,3(/,1X,A))') RECORD
               IF (LOGF) WRITE (IUL,'(1X,A,3(/,1X,A))') RECORD
            END IF

*           stop
            IF (TOSCR) WRITE (*,'(1X,2A)')
     $       'ERROR in RDDATA: ',MESSAG(IMES)
            IF (LOGF) WRITE (IUL,'(1X,2A)')
     $       'ERROR in RDDATA: ',MESSAG(IMES)
            CALL FATALERR ('RDDATA',' ')
         END IF


      ELSE IF (ITASK.EQ.8) THEN
*        check presence of variable name on data file ; error check
         IF (.NOT.INIDAT) CALL FATALERR ('RDINQR','no data file active')

*        get local copy of significant part of name in uppercase
         ISX    = MAX (1,ISTART (XNAME))
         ILX    = MAX (1,LEN_TRIM (XNAME))
         LXNM31 = XNAME(ISX:ILX)
         CALL UPPERC (LXNM31)

*        check data file index
         IS  = IFINDC (DATLIS,ILNDAT,1,INFDAT,LXNM31)


      ELSE IF (ITASK.EQ.9) THEN
*        delete all known "TMP files"
         IRECL = ILBUF + IIL

         IF (INIDAT) THEN
*           close old unit if left open
            INQUIRE (UNIT=IUNDAT, OPENED=OPUNIT)
            IF (OPUNIT) CLOSE(IUNDAT)
         END IF

         IF (REPL) THEN
*           close old unit if left open
            INQUIRE (UNIT=IUNREP, OPENED=OPUNIT)
            IF (OPUNIT) CLOSE (IUNREP)
         END IF

         IF (INIDAT) THEN
*           close temporary data files
            DO 120 I=1,INFILS
*              name of temporary file ; it exists ?
               DATTMP = TMPLIS(I)
               ILTMP  = LEN_TRIM (DATTMP)
               TEMP   = FLEXIST (DATTMP(1:ILTMP))

               IF (TEMP) THEN
                  CALL FOPENG (IUNIT,DATTMP,'OLD','UD',IRECL,' ')
                  CLOSE (IUNIT,STATUS='DELETE')
                  IF (LOGD) WRITE (IULD,'(1X,3A)') 'Temporary file ',
     $             DATTMP(1:ILTMP),' deleted by RDDTMP'
               END IF
120         CONTINUE

*           disable variable reading
            INIDAT = OFF
         END IF

         IF (REPL) THEN
*           delete rerun TMP file if it exists
            CALL EXTENS (RFIL,'TMP',1,REPTMP)
            ILTMP = LEN_TRIM (REPTMP)
            TEMP  = FLEXIST (REPTMP(1:ILTMP))
            IF (TEMP) THEN
               CALL FOPENG (IUNIT,REPTMP,'OLD','UD',IRECL,' ')
               CLOSE (IUNIT,STATUS='DELETE')
               IF (LOGR) WRITE (IULR,'(1X,3A)') 'Temporary file ',
     $          REPTMP(1:ILTMP),' deleted by RDDTMP'
            END IF

*           disable reruns
            REPL = OFF
            CALL AMBUSY (1,'RDFROM',0)
         END IF

      ELSE IF (ITASK.EQ.10) THEN
*        set return value for missing values
         IF (VARTYP.EQ.'R') THEN
*           default single precision real
            RM = RX(1)
         ELSE IF (VARTYP.EQ.'I') THEN
*           default integer
            IM = IX(1)
         ELSE IF (VARTYP.EQ.'D') THEN
*           default double precision real
            DM = DX(1)
         ELSE IF (VARTYP.EQ.'C') THEN
*           default character string
            CM = CX(1)
         ELSE IF (VARTYP.EQ.'L') THEN
*           default logical
            LM = LX(1)
         ELSE IF (VARTYP.EQ.'T') THEN
*           default date_time
            TM = DX(1)
         END IF

      ELSE IF (ITASK.EQ.11) THEN

*        assign default missing values
         RM = RM_D
         IM = IM_D
         DM = DM_D
         CM = CM_D
         LM = LM_D
         TM = TM_D

      ELSE
         CALL FATALERR ('RDDATA','illegal ITASK value')
      END IF

      RETURN
      END

