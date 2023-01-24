!=======================================================================
!  CROPSIM GENERIC READ ROUTINES    
!  Utilities for generic reading of standard files
!  Last edit 06/06/13 LAH
!=======================================================================

!  NOTE: Need to get array sizes with: ASIZE=UBOUND(ARRAYOUT,1)

!    XREADD   - Subroutine to find number of components, options, etc..
!    XREADT   - Subroutine to read line of text from X-file
!    XREADC   - Subroutine to read character from X-file
!    XREADR   - Subroutine to read real value from X-file
!    XREADI   - Subroutine to read integer value from X-file
!    XREADCA  - Subroutine to read array of characters from X-file
!    XREADRA  - Subroutine to read array of real values from X-file
!    XREADIA  - Subroutine to read array of integer values from X-file

!    AREADT   - Subroutine to read line of text from summary file
!    AREADC   - Subroutine to read character from summary file
!    AREADR   - Subroutine to read real value from summary file
!    AREADI   - Subroutine to read integer value from summary file

!    CUREADT  - Subroutine to read line of text from cultivar file
!    CUREADC  - Subroutine to read character from cultivar file
!    CUREADR  - Subroutine to read real value from cultivar file

!    ECREADT  - Subroutine to read line(s) of text from ecotype file
!    ECREADC  - Subroutine to read character from ecotype file
!    ECREADR  - Subroutine to read real value from ecotype file
!    ECREADRA - Subroutine to read real array from eco file (String<25)

!    SPREADT  - Subroutine to read line of text from species file
!    SPREADC  - Subroutine to read character from species file
!    SPREADR  - Subroutine to read real value from species file
!    SPREADRA - Subroutine to read array of real value from species file
!    SPREADIA - Subroutine to read array of integers from species file
!    SPREADCA - Subroutine to read array of characters from species file

!    SLREADT  - Subroutine to read line of text from soil file
!    SLREADC  - Subroutine to read character from soil file
!    SLREADR  - Subroutine to read real value from soil file
!    SLREADCA - Subroutine to read array of characters from soil file
!    SLREADRA - Subroutine to read array of real values from soil file

!    SMREADT  - Subroutine to read line of text from SOM file
!    SMREADC  - Subroutine to read character from SOM file
!    SMREADR  - Subroutine to read real value from SOM file
!    SMREADRA - Subroutine to read array of real value from SOM file
!    SMREADR2 - Subroutine to read 2 dim array,real value from SOM file

!    AMAKEABV - Subroutine to make array of abbreviations with synonyms
!    AMAKE1   - Subroutine to make array when data in columns,1 level
!    AMAKER0  - Subroutine to make array when data in rows and 0 levels
!    AMAKE5   - Subroutine to make array when data in columns,5 levels
!    ARAYREAD - Subroutine to read internal array and return string

!    Following no longer used but retained for possible later use
!    E1READT  - Subroutine to read line of text from 'soils' type eco file
!    E1READC  - Subroutine to read character from 'soils' type eco file
!    E1READR  - Subroutine to read real value from 'soils' type eco file

!    S1READT  - Subroutine to read line of text from old species file
!    S1READC  - Subroutine to read character from old species file
!    S1READR  - Subroutine to read real value from old species file
!    S1READRA - Subroutine to read array of real value from old species file
!    S1READIA - Subroutine to read array of integers from old species file
!    S1READCA - Subroutine to read array of characters from old species file

!-----------------------------------------------------------------------

      SUBROUTINE XREADD(FILENAME,TN,RN,snnum,onnum,cnnum)

      IMPLICIT NONE
      EXTERNAL GETLUN, TVILENT, TL10FROMI, Ltrim, Tl1upcase, Tvicolnm, 
     &  Standard

      INTEGER       snx
      PARAMETER     (snx=10)
      INTEGER       onx
      PARAMETER     (onx=3)

      CHARACTER (LEN=1000) DATASTD
      CHARACTER (LEN=600)  DATATMP
      CHARACTER (LEN=254)  TL2541, DATALINE
      CHARACTER (LEN=80)   Tl0801
      CHARACTER (LEN=30)   TL30
      CHARACTER (LEN=20)   GROUP, LEVELSTD
      CHARACTER (LEN=10)   LEVEL, TL10FROMI
      CHARACTER (LEN=1)    TL1UPCASE, FTYPE
      CHARACTER (LEN=*)    FILENAME
      INTEGER              FILENUM, GROUPLEN, TVILENT
      INTEGER              LEVELLEN, FILELEN, LENSTD
      INTEGER              FNUMERR, I, L
      INTEGER              CNNUM(SNX,ONX), SNNUM, ONNUM(SNX)
      INTEGER              SN, ON(SNX), CN(SNX,ONX), TN, RN, COLRN
      INTEGER              RNCOL, SNCOL, ONCOL, CNCOL, TVICOLNM

      INTRINSIC            MAX

      SAVE
      
      CALL GETLUN('FILETMP',FILENUM)

      FTYPE='*' ! Default file type
      GROUP='*TREATMENTS'

      LENSTD=12

      FILELEN=TVILENT(FILENAME)

      GROUPLEN=TVILENT(GROUP)

      OPEN(filenum,FILE=filename(1:FILELEN),STATUS='OLD')

      LEVEL=TL10FROMI(TN)
      LEVELLEN=TVILENT(LEVEL)
      IF(LEVELLEN.GT.LENSTD)LEVELLEN=LENSTD
      LEVELSTD='      '
      LEVELSTD(1:LEVELLEN)=LEVEL(1:LEVELLEN)

      SNNUM=1
      DO I=1,SNX
        onnum(i)=1
      ENDDO
      DO I=1,SNX
        DO L=1,ONX
          CNNUM(I,L)=1
        ENDDO
      ENDDO

      tl0801=' '
      DO WHILE(tl0801(1:GROUPLEN).NE.GROUP(1:GROUPLEN))
        READ(filenum,'(A80)',END=9996,ERR=9999)tl0801
        CALL Ltrim(tl0801)
        tl0801(1:1)=Tl1upcase(tl0801(1:1))
        tl0801(2:2)=Tl1upcase(tl0801(2:2))
        IF(tl0801(1:1).EQ.'$')ftype='$'
      END DO

      DO WHILE (TL0801(1:1).NE.'@')
        READ (FILENUM,'(A80)') TL0801
      ENDDO
      RNCOL=Tvicolnm(TL0801,'RN ')
      SNCOL=Tvicolnm(TL0801,'SN ')
      ONCOL=Tvicolnm(TL0801,'ON ')
      CNCOL=Tvicolnm(TL0801,'CN ')

      tl2541=' '

      DO
        DO WHILE(tl30(1:levellen).NE.levelstd(1:levellen))
          READ(filenum,'(A254)',END=9996,ERR=9999)tl2541
          IF(tl2541(1:1).EQ.'*')GOTO 999
          TL30=TL2541(1:30)
          CALL Ltrim(tl30)
          IF(tl30(1:1).EQ.'0'.AND.tl30(2:2).NE.' ')THEN
            tl30(1:1)=' '
            IF(tl30(2:2).EQ.'0'.AND.tl30(3:3).NE.' ')THEN
              tl30(2:2)=' '
              IF(tl30(3:3).EQ.'0'.AND.tl30(4:4).NE.' ')THEN
                tl30(3:3)=' '
              ENDIF
            ENDIF
          ENDIF
          CALL Ltrim(tl30)
          tl30(1:1)=Tl1upcase(tl30(1:1))
          tl30(2:2)=Tl1upcase(tl30(2:2))
        END DO
        DATALINE=tl2541(1:20)//'     '
        CALL Standard(dataline,datatmp,'6')

        IF(ftype.EQ.'*')THEN
          DATASTD(1:48)=DATATMP(1:48)
        ELSEIF(ftype.EQ.'$'.AND.rncol.GT.0)THEN
          IF (rncol.GT.0)THEN
            READ(DATATMP,'(6X,I6)')COLRN
            IF (COLRN.NE.RN) GOTO 998
          ENDIF
          DATASTD(1:48)=DATATMP(7:54)
        ENDIF

        IF (SNCOL.GT.0) THEN
          READ(DATASTD,'(6X,I6)')sn
        ELSE
          SN = 1
        ENDIF
        IF(SN.LE.0)SN=1
        IF (SN.GT.0)THEN
          IF (SNCOL.GT.0) THEN
            READ(DATASTD,'(12X,I6)')on(sn)
            IF(ON(SN).LE.0)ON(SN)=1
            IF(ON(SN).GT.0)THEN
              READ(DATASTD,'(18X,I6)')cn(sn,on(sn))
              IF(CN(SN,ON(SN)).LE.0)cn(sn,on(sn))=1
            ENDIF
          ENDIF
        ENDIF
        IF (sn.LT.1) sn = 1
        snnum=MAX(snnum,sn)
        IF (on(sn).LT.1) on(sn) = 1
        onnum(sn)=MAX(onnum(sn),on(sn))
        cnnum(sn,on(sn))=MAX(cnnum(sn,on(sn)),cn(sn,on(sn)))

  998   CONTINUE

        READ(filenum,'(A254)',END=9996,ERR=9999)tl2541
        IF(tl2541(1:1).EQ.'*')GOTO 999
        TL30=TL2541(1:30)
        CALL Ltrim(tl30)
        IF(tl30(1:1).EQ.'0'.AND.tl30(2:2).NE.' ')THEN
          tl30(1:1)=' '
          IF(tl30(2:2).EQ.'0'.AND.tl30(3:3).NE.' ')THEN
            tl30(2:2)=' '
            IF(tl30(3:3).EQ.'0'.AND.tl30(4:4).NE.' ')THEN
              tl30(3:3)=' '
            ENDIF
          ENDIF
        ENDIF
        CALL Ltrim(tl30)
        tl30(1:1)=Tl1upcase(tl30(1:1))
        tl30(2:2)=Tl1upcase(tl30(2:2))
      ENDDO

  999 CONTINUE

 9996 CONTINUE

 9999 CONTINUE

      CLOSE(filenum)

      IF (SN .LE. 0)THEN
        CALL Getlun('ERROR.OUT',fnumerr)
        OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
        WRITE (fnumerr,*) ' Problem in XREADD (ie.treatment details)!'
        WRITE (fnumerr,*) ' Sequence number <=0 in ',filename(1:60)
        WRITE (fnumerr,*) ' Check READS.OUT for details of reads'
        WRITE (*,*) ' Problem in XREADD (ie.treatment details)!'
        WRITE (*,*) ' Could not read: ',filename(1:60)
        WRITE (*,*) ' Sequence number <=0 in ',filename(1:60)
        WRITE (*,*) ' Program will have to stop'
        WRITE (*,*) ' Check READS.OUT for details'
        CLOSE (fnumerr)
        STOP ' '
      ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE XREADT(FILENAME,COL1,COL1R,COL2,COL3,COL4,CODE,TXTO)

      USE OSDefinitions

      IMPLICIT NONE
      EXTERNAL GETLUN, LTRIM2
      EXTERNAL TVILENT, TVICOLNM, TL10FROMI, LTRIM, TL1UPCASE, 
     &  STANDARD, TVINSTR, STANDC, AMAKEABV, ARAYREAD

      CHARACTER (LEN=1000) DATASTD, ABVSTD
      CHARACTER (LEN=254)  FILENEW, FILEOLD, ABVLINE, DATALINE
      CHARACTER (LEN=254)  TL2541,  DATARRAY(500), ABVARRAY(1000)
      CHARACTER (LEN=128)  ARG
      CHARACTER (LEN=100)  DATAROC
      CHARACTER (LEN=132)  ABVFILE
      CHARACTER (LEN=64)   CTRDIRFL
      CHARACTER (LEN=60)   ENAME
      CHARACTER (LEN=52)   TEXTSTD
      CHARACTER (LEN=30)   TL30
      CHARACTER (LEN=30)   LEVELTMP, LEVELSTD, LE
      CHARACTER (LEN=12)   COEFFC, ABVC, COEFFCTM
      CHARACTER (LEN=10)   EXPER, TL10FROMI, LEVEL, TL10
      CHARACTER (LEN=4)    COEFFCHK
      CHARACTER (LEN=3)    CU,FL,SA,IC,MP,MI,MF,MR,MC,MT,ME,MH,SM,AM,FI
      CHARACTER (LEN=3)    DI
      CHARACTER (LEN=1)    TL1UPCASE, TR, CTR, FTYPE, COLON, HEADLINE
      CHARACTER (LEN=1)    FNUMFLAG, CTRFLAG
      CHARACTER (LEN=*)    FILENAME, CODE, TXTO
      INTEGER              COL2TMP, COL3TMP, COL4TMP, COL1, COL1R, L
      INTEGER              COL2, COL3, COL4, COLRN, STARNUM, STARTRE
      INTEGER              COL1P, COL1RP, COL2P, COL3P, COL4P, XREADNUM
      INTEGER              TVILENT, TVI1, TVI2, TVI3, TVI4
      INTEGER              STARTCOL, LEVELLEN, FILELEN, LINE, LENTMP
      INTEGER              TVINSTR, ABVNUM, DATANUM, LENSTD, ABVLEN, DN
      INTEGER              DATANUMS, RETURNUM, ARGLEN, ABVSYN  !,FNUMREA
      INTEGER              FILENUM, FNUMERR
      INTEGER              RNCOL, SNCOL, ONCOL, CNCOL, TVICOLNM
      LOGICAL              FFLAG  !, FOPEN

      PARAMETER  (LENSTD=13)

      INTRINSIC  LEN,MIN,TRIM   !,EOF

      SAVE

      IF (FNUMFLAG.NE.'Y')THEN
        CALL GETLUN('FILETMP',FILENUM)
        FNUMFLAG = 'Y'
! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.
!        CALL Getlun('READS.OUT',fnumrea)
!        INQUIRE (FILE='READS.OUT',OPENED=fopen)
!        IF (.NOT.fopen) OPEN(UNIT=fnumrea,FILE='READS.OUT')
      ENDIF


      CALL LTRIM2(FILENAME,filenew)
      FILELEN=TVILENT(FILENEW)
      FILELEN=MIN(254,FILELEN)
      FILENEW=' '
      FILENEW(1:FILELEN)=FILENAME(1:FILELEN)

      IF (FILENEW(1:FILELEN) .EQ. FILEOLD(1:FILELEN)) THEN
        IF(COL1.EQ.COL1P)THEN
          IF(COL1R.EQ.COL1RP)THEN
            IF(COL2.EQ.COL2P)THEN
              IF(COL3.EQ.COL3P)THEN
                IF(COL4.EQ.COL4P)THEN
                  GO TO 5555
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF

      TVI1 = TVILENT(FILENEW)
      ! Pre-processor
      OPEN(filenum,FILE=filenew,STATUS='OLD')
      DO WHILE (TL10(1:6).NE.'*TREAT')
        READ (FILENUM,'(A10)') TL10
      ENDDO
      IF (FILENEW(TVI1-2:TVI1).EQ.'INH') THEN
        ! Find TN and SN if not provided (DS4 files)
        READ (FILENUM,'(A10)') TL10
        READ (FILENUM,'(A10)') TL10
        READ (TL10,'(I3)') COL1
        READ (TL10,'(3X,I3)') COL2
        COL1R = 0
        COL3 = 1
        COL4 =1
      ELSE
        ! Find if RN,SN,ON,CN present (X-files)
        READ (FILENUM,'(A10)') TL10
        DO WHILE (TL10(1:1).NE.'@')
          READ (FILENUM,'(A30)') TL30
        ENDDO
        RNCOL=Tvicolnm(TL30,'RN ')
        SNCOL=Tvicolnm(TL30,'SN ')
        ONCOL=Tvicolnm(TL30,'ON ')
        CNCOL=Tvicolnm(TL30,'CN ')
      ENDIF
      CLOSE (filenum)

      IF (COL1.LT.0) THEN
        CALL Getlun('ERROR.OUT',fnumerr)
        OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
        WRITE(fnumerr,*) ' Treatment # <0 !'
        WRITE(fnumerr,*) ' Check READS.OUT for details'
        WRITE(*,*) '  Treatment # <0 !'
        WRITE(*,*) '  Program will have to stop!'
        WRITE(*,*) '  Check READS.OUT for details'
        CLOSE (fnumerr)
        STOP ' '
      ENDIF

      IF (COL1R.LT.0) COL1R=0

! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.
!      WRITE(fnumrea,*)' '
!      WRITE(fnumrea,*)'FILE ',FILENAME(1:FILELEN)
!      WRITE(fnumrea,*)' TREATMENT: ',COL1
!      WRITE(fnumrea,*)' REPLICATE: ',COL1R
!      WRITE(fnumrea,*)' SN,ON,CN (as provided): ',COL2,COL3,COL4
      IF (COL2.LE.0) COL2=1
      IF (COL3.LE.0) COL3=1
      IF (COL4.LE.0) COL4=1
!      WRITE(fnumrea,*)' SN,ON,CN (as used)    : ',COL2,COL3,COL4

      LEVEL=TL10FROMI(COL1)
      LEVELLEN=TVILENT(LEVEL)
      IF(LEVELLEN.GT.LENSTD)LEVELLEN=LENSTD
      LEVELSTD='      '
      LEVELSTD(1:LEVELLEN)=LEVEL(1:LEVELLEN)

      FTYPE='*'

      OPEN(filenum,FILE=filename(1:FILELEN),STATUS='OLD')

      DATANUM=0
      LINE=0
      RETURNUM=0
      TR='N'
      CTR='N'
      STARTCOL=1
      ABVLINE=' '

      XREADNUM=0

 4444 CONTINUE

      STARTRE=0
      STARNUM=0

 8888 CONTINUE

      XREADNUM=XREADNUM + 1

      DO
        tl2541=' '
        READ(filenum,'(A254)',END=9996,ERR=9999)tl2541

        IF(tl2541(1:1).EQ.'$')ftype='$'

        IF(TVILENT(tl2541).LT.3)GOTO 999
        IF(TL2541(1:1).EQ.'!')GOTO 999

        ! Controls to be taken from CTR file (lower case x)
        IF (FILENEW(FILELEN:FILELEN).EQ.'x') THEN
          IF(TL2541(1:11).EQ.'*SIMULATION'.AND.CTR.EQ.'N') THEN
            READ(filenum,'(A254)',END=9996,ERR=9999)TL2541
            DO
              !IF (EOF(filenum)) GOTO 9996    !chp portability to g95
              READ(filenum,'(A254)',END=9996,ERR=9999)TL2541
              IF(TL2541(1:1).EQ.'*') THEN
                IF(TL2541(1:3).NE.'*SI') EXIT
              ENDIF
            ENDDO
          ENDIF
        ENDIF

        COLON='N'
        HEADLINE = 'N'
        IF(tl2541(1:4).EQ.'*EXP')HEADLINE = 'Y'
        IF(tl2541(1:1).EQ.'$')THEN
          IF(tl2541(2:4).EQ.'EXP')HEADLINE='Y'
          IF(tl2541(8:10).EQ.'EXP')HEADLINE='Y'
          IF(tl2541(8:10).EQ.'DET')HEADLINE='Y'
        ENDIF
        IF(HEADLINE.EQ.'Y')THEN
          DO TVI1=1,30
            IF(COLON.EQ.'Y'.AND.TL2541(TVI1:TVI1).NE.' ')EXIT
            IF(TL2541(TVI1:TVI1).EQ.':')COLON='Y'
            TL2541(TVI1:TVI1)=' '
          ENDDO
          CALL LTRIM(TL2541)
          READ (TL2541,'(A10,A60)')EXPER,ENAME
          CALL Ltrim(ENAME)
          DATANUM=DATANUM+1
          DATARRAY(DATANUM)='EXPER '//' '//exper
          IF(FTYPE.EQ.'*') THEN
            DATANUM=DATANUM+1
            DATARRAY(DATANUM)='ENAME '//' '//ename
          ENDIF
          GO TO 999
        ENDIF

        IF(tl2541(1:1).EQ.'$')GOTO 999
        IF(tl2541(1:3).EQ.'*EX')GOTO 999

        IF(TR.EQ.'Y')THEN
          RETURNUM=RETURNUM+1
          IF (RETURNUM .GT. 250) THEN
            CALL Getlun('ERROR.OUT',fnumerr)
            OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
            WRITE(fnumerr,*)' Problem reading treatment ',COL1
            WRITE(fnumerr,*)' Component,option,species  ',COL2,COL3,COL4
            WRITE(fnumerr,*)' Check READS.OUT for details'
            WRITE (*,*) ' Problem reading treatment ',COL1
            WRITE (*,*) ' Component,option,species: ',COL2,COL3,COL4
            WRITE (*,*) ' Program will have to stop'
            WRITE (*,*) ' Check READS.OUT for details'
            CLOSE (fnumerr)
            STOP ' '
          ENDIF
        ENDIF

        tl30=' '
        TL30=TL2541(1:30)
        CALL Ltrim(tl30)
        tl30(1:1)=Tl1upcase(tl30(1:1))
        tl30(2:2)=Tl1upcase(tl30(2:2))
        IF (TL30(2:2).EQ.' ') TL30(3:3)=' '

        IF (tl2541(1:1).EQ.'*') THEN
          IF (tl2541(1:8).NE.'*CONTROL') THEN
           IF (TL2541(2:4).EQ.'EXP') GO TO 999
           IF (TL2541(2:8).NE.'GENERAL') STARNUM=STARNUM + 1

            ! Following is to accomodate files with treatment group
            ! low in file
            IF (STARNUM .EQ. STARTRE+1 .AND. STARNUM .GT. 2) THEN
              ! Following is necessary to stop entering
              ! when treatments at end of file
              IF (XREADNUM .EQ. 1) THEN
                CLOSE (FILENUM)
                OPEN (filenum,FILE=filename(1:FILELEN),STATUS='OLD')

                DATANUM=0
                LINE=0
                RETURNUM=0
                TR='N'
                CTR='N'
                STARTCOL=1
                ABVLINE=' '
                GO TO 8888
              ENDIF
            ENDIF

           IF(TL2541(2:4).EQ.'TRE')THEN
             TR='Y'
             STARTCOL=5
             IF (RNCOL.LE.0) STARTCOL = STARTCOL - 1
             IF (SNCOL.LE.0) STARTCOL = STARTCOL - 1
             IF (ONCOL.LE.0) STARTCOL = STARTCOL - 1
             IF (CNCOL.LE.0) STARTCOL = STARTCOL - 1
             IF (STARTRE.EQ.0) STARTRE=STARNUM
           ELSEIF(TL2541(2:5).EQ.'GENE')THEN
             TR='N'
             STARTCOL=1
           ELSE
             TR='N'
             STARTCOL=2
           ENDIF
           LE=' '
           IF(TL2541(2:5).EQ.'GENE')LE(1:3)='000'
           IF(TL2541(2:4).EQ.'TRE')LE(1:LEVELLEN)=LEVELSTD(1:LEVELLEN)
           IF(TL2541(2:4).EQ.'CUL')LE(1:3)=CU
           IF(TL2541(2:5).EQ.'GENO')LE(1:3)=CU
           IF(TL2541(2:4).EQ.'FIE')LE(1:3)=FL
           IF(TL2541(2:4).EQ.'SOI')LE(1:3)=SA
           IF(TL2541(2:4).EQ.'INI')LE(1:3)=IC
           IF(TL2541(2:4).EQ.'PLA')LE(1:3)=MP
           IF(TL2541(2:4).EQ.'IRR')LE(1:3)=MI
           IF(TL2541(2:4).EQ.'FER')LE(1:3)=MF
           IF(TL2541(2:4).EQ.'RES')LE(1:3)=MR
           IF(TL2541(2:4).EQ.'CHE')LE(1:3)=MC
           IF(TL2541(2:4).EQ.'TIL')LE(1:3)=MT
           IF(TL2541(2:4).EQ.'ENV')LE(1:3)=ME
           IF(TL2541(2:4).EQ.'HAR')LE(1:3)=MH
           IF(TL2541(2:4).EQ.'FIL')LE(1:3)=FI
           IF(TL2541(2:4).EQ.'DIS')LE(1:3)=DI
           IF(TL2541(2:4).EQ.'SIM')THEN
             LE(1:3)=SM
             IF(SM(1:1).NE.'0')THEN
               IF(CTR.EQ.'N')THEN
                 IF(Ctrflag.NE.'Y') THEN
!                   WRITE(fnumrea,*)' CONTROLS FROM: ',filenew(1:60)
                   Ctrflag = "Y"
                 ENDIF
               ENDIF
               CTR='Y'
             ENDIF
           ENDIF
           IF(TL2541(2:4).EQ.'AUT')LE(1:3)=AM
          ENDIF

          GO TO 999
        ENDIF

        IF(tl2541(1:1).EQ.'@')THEN
          ! Strip off leading @
          tl2541(1:1)=' '

          ! Following code introduced to move header across.
          ! Maybe necessary in case first column is character field
          ! eg General section, but not for data fields where
          ! first column must be numeric!!!
          TVI2=0
          TVI3=0
          DO tvi1=1,30
            IF(tl2541(tvi1:tvi1).EQ.' ' .AND. TVI2.GT.0)THEN
              TVI3=MIN(10,TVI1-1)
              EXIT
            ENDIF
            IF(tl2541(tvi1:tvi1).NE.' '.AND. TVI2.EQ.0)TVI2=TVI1
          ENDDO
          TL10(1:(TVI3-TVI2+1))=TL2541(TVI2:TVI3) 
          DO TVI1=1,TVI3
            TL2541(TVI1:TVI1)=' '
          ENDDO
          TL2541(1:(TVI3-TVI2+1))=TL10(1:(TVI3-TVI2+1))

          ! Strip out leading periods
          ABVLEN=TVILENT(TL2541)
          DO TVI1=1,ABVLEN-1
            IF(TL2541(TVI1:TVI1+1).EQ.' .')TL2541(TVI1:TVI1+1)='  '
            ! NEW JULY 05
            IF(TL2541(TVI1:TVI1).EQ.'!')TL2541(TVI1:ABVLEN)=' '
          ENDDO
          IF (ABVLINE(2:30).NE.TL2541(2:30))THEN
            ABVLINE(1:1)=' '
            ABVLINE=tl2541
            ABVSTD=' '
            CALL STANDARD(ABVLINE,ABVSTD,'13')
            ABVNUM=TVINSTR(ABVSTD)
            ! NB MAXIMUM NUMBER OF VARIABLES ALONG LINE=40
            IF(ABVNUM.GT.40)ABVNUM=40
            LINE=1
            GO TO 999
          ENDIF
        ENDIF

        DATALINE=tl2541

        IF(ABVNUM.EQ.1)THEN
          IF(line.EQ.1)THEN
            datanum=datanum+1
            CALL LTRIM(abvline)
            CALL LTRIM(dataline)
            DATARRAY(datanum)=abvline(1:6)//' '//dataline(1:247)
            LINE=LINE+1
          ELSE
            CALL LTRIM(dataline)
            tvi1=TVILENT(DATARRAY(datanum))
            tvi2=TVILENT(dataline)
            IF(tvi1+tvi2.GT.241)tvi2=254-tvi1
            dn=datanum
            DATARRAY(dn)=DATARRAY(dn)(1:tvi1)//' '//dataline(1:tvi2)
          ENDIF
          GO TO 999
        ENDIF

        DATASTD=' '
        TEXTSTD=' '
        IF(TVILENT(abvline).LT.3)THEN
          CALL Getlun('ERROR.OUT',fnumerr)
          OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
          WRITE(fnumerr,*)' Header line not found! '
          WRITE(fnumerr,*)' Possibly wrong file type!'
          WRITE(fnumerr,*)' Header line: ',abvline(1:60)
          WRITE(fnumerr,*)' Data line  : ',dataline(1:60)
          WRITE(fnumerr,*)' Text       : ',textstd
          WRITE(fnumerr,*)' Check READS.OUT for details'
          WRITE (*,*) ' Header line not found! '
          WRITE (*,*) ' Possibly wrong file type!'
          WRITE (*,*) ' Header line: ',abvline(1:60)
          WRITE (*,*) ' Data line  : ',dataline(1:60)
          WRITE (*,*) ' Text       : ',textstd
          WRITE (*,*) ' Program will have to stop'
          WRITE (*,*) ' Check READS.OUT for details'
          CLOSE (fnumerr)
          STOP ' '
        ENDIF

        IF(MP(1:1).EQ.'0' .AND. MH(1:1).EQ.'0')THEN
          CALL Getlun('ERROR.OUT',fnumerr)
          OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
          WRITE(fnumerr,*)' Both planting and harvest levels = 0!'
          WRITE(fnumerr,*)' Hence no crop nor fallow specified!'
          WRITE(fnumerr,*)' Check READS.OUT for details of reads'
          WRITE(*,*)' Both planting and harvest levels = 0!'
          WRITE(*,*)' Hence no crop nor fallow specified!'
          WRITE(*,*)' Program will have to stop'
          WRITE(*,*)' Check READS.OUT for details of reads'
          CLOSE (fnumerr)
          STOP ' '
        ENDIF

        IF (TVILENT(DATALINE).LT.5) GO TO 999
        CALL STANDC(ABVLINE,DATALINE,DATASTD,TEXTSTD,'13')
        ! Below is for debugging purposes only
        ! WRITE(fnumrea,*)' ABVLINE  ',abvline(1:60)
        ! WRITE(fnumrea,*)' ABVSTD   ',abvstd(1:60)
        ! WRITE(fnumrea,*)' DATALINE ',dataline(1:60)
        ! WRITE(fnumrea,*)' DATASTD  ',datastd(1:60)

        IF(le(1:3).NE.'000')THEN
          leveltmp=' '
          READ(DATASTD,'(A13)',ERR=7777)leveltmp(1:13)
          CALL Ltrim(leveltmp)
          IF(leveltmp(1:1).EQ.'0')THEN
            leveltmp(1:1)=' '
            IF(leveltmp(2:2).EQ.'0')THEN
              leveltmp(2:2)=' '
              IF(leveltmp(3:3).EQ.'0')THEN
                leveltmp(3:3)=' '
              ENDIF
            ENDIF
          ENDIF
          CALL Ltrim(leveltmp)
        ENDIF

        IF(TR.EQ.'Y')THEN
          DATAROC=' '
          IF(ftype.EQ.'*')THEN
            DATAROC(1:52)=DATASTD(1:52)
          ELSEIF(ftype.EQ.'$')THEN
            IF (RNCOL.GT.0) THEN
              READ(DATASTD,'(13X,I13)',ERR=7777)COLRN
              IF (COLRN.NE.COL1R)GO TO 999
              DATAROC(1:13)=DATASTD(1:13)
              DATAROC(14:52)=DATASTD(26:64)
            ENDIF
          ENDIF
          IF (SNCOL.GT.0.AND.COL2.GT.0) THEN
            READ(DATAROC,'(13X,I13)',ERR=7777)COL2TMP
            IF (COL2TMP.EQ.0)COL2TMP=COL2
            IF (COL2TMP.NE.COL2)GO TO 999
          ENDIF
          IF (ONCOL.GT.0.AND.COL3.GT.0) THEN
            READ(DATAROC,'(26X,I13)',ERR=7777)COL3TMP
            IF (COL3TMP.EQ.0)COL3TMP=COL3
            IF (COL3TMP.NE.COL3)GO TO 999
          ENDIF
          IF (CNCOL.GT.0.AND.COL4.GT.0) THEN
            READ(DATAROC,'(39X,I13)',ERR=7777)COL4TMP
            IF (COL4TMP.EQ.0)COL4TMP=COL4
            IF (COL4TMP.NE.COL4)GO TO 999
          ENDIF
        ENDIF

        IF (LE .NE. '00') THEN
          IF (LE(1:1).EQ.'0') THEN
            LE(1:1)=' '
            IF (LE(2:2).EQ.'0') LE(2:2)=' '  ! NEW JUL 06
          ENDIF
          CALL Ltrim(le)
          LENTMP=TVILENT(LE)
          ! Below is for debugging
          ! Write (fnumrea,*)' Leveltmp ie what read ',leveltmp
          ! Write (fnumrea,*)' Le ie what needed     ',le
          IF(LEVELTMP(1:LENTMP).NE.LE(1:LENTMP))GO TO 999
          IF(LEVELTMP(LENTMP+1:LENTMP+1).NE.' ')GO TO 999
        ENDIF

        DO TVI1=1,ABVNUM-(STARTCOL-1)
          IF(TVI1.EQ.1)THEN
            IF(LINE.EQ.1)THEN
              datanums=datanum
            ELSE
              datanum=datanums
            ENDIF
          ENDIF
          datanum=datanum+1
          TVI2=1 + ((STARTCOL-1)*LENSTD) + (TVI1-1)*LENSTD
          TVI4=TVI2 + (LENSTD-1)
          COEFFC=DATASTD(TVI2:TVI4)
          ABVC=ABVSTD(TVI2:TVI4)
          ! Below are for debugging
          ! Write(fnumrea,*)' abvnum    ',abvnum
          ! Write(fnumrea,*)' lenstd    ',lenstd
          ! Write(fnumrea,*)' startcol  ',startcol
          ! Write(fnumrea,*)' abvc      ',abvc
          ! Write(fnumrea,*)' tvi2,tvi4 ',tvi2,tvi4
          ! Write(fnumrea,*)' coeffc    ',coeffc
          COEFFCTM=COEFFC
          CALL LTRIM(COEFFCTM)
          IF (TVILENT(COEFFCTM).LT.1)COEFFC='-99         '
          CALL LTRIM(TEXTSTD)
          IF (TVILENT(TEXTSTD).LT.1)TEXTSTD(1:3)='-99'
          COEFFCHK=COEFFCTM(1:4)

          IF (LINE.EQ.1) THEN
            IF(COEFFCHK.EQ.'TEXT')THEN
              DATARRAY(datanum)=abvc//' '//TEXTSTD
            ELSE
              DATARRAY(datanum)=abvc//' '//coeffc
            ENDIF
            IF(TVI1.EQ.(ABVNUM-(STARTCOL-1)))LINE=LINE+1
          ELSE
            LENTMP=TVILENT(DATARRAY(datanum))
            IF(COEFFCHK.EQ.'TEXT')THEN
              TL2541=DATARRAY(datanum)(1:LENTMP)//' '//TEXTSTD
              DATARRAY(datanum)=TL2541
            ELSE
              TL2541=DATARRAY(datanum)(1:LENTMP)//' '//coeffc
              DATARRAY(datanum)=TL2541
            ENDIF
          ENDIF


          IF(ABVC(1:3).EQ.'CU '.OR.ABVC(1:3).EQ.'GE ') CU=COEFFC(1:3)
          IF(ABVC(1:3).EQ.'FL ')FL=COEFFC(1:3)
          IF(ABVC(1:3).EQ.'SA ')SA=COEFFC(1:3)
          IF(ABVC(1:3).EQ.'IC ')IC=COEFFC(1:3)
          IF(ABVC(1:3).EQ.'MP '.OR.ABVC(1:3).EQ.'PL ')MP=COEFFC(1:3)
          IF(ABVC(1:3).EQ.'MI '.OR.ABVC(1:3).EQ.'IR ')MI=COEFFC(1:3)
          IF(ABVC(1:3).EQ.'MF '.OR.ABVC(1:3).EQ.'FE ')MF=COEFFC(1:3)
          IF(ABVC(1:3).EQ.'MR '.OR.ABVC(1:3).EQ.'RE ')MR=COEFFC(1:3)
          IF(ABVC(1:3).EQ.'MC '.OR.ABVC(1:3).EQ.'CH ')MC=COEFFC(1:3)
          IF(ABVC(1:3).EQ.'MT '.OR.ABVC(1:3).EQ.'TI ')MT=COEFFC(1:3)
          IF(ABVC(1:3).EQ.'ME '.OR.ABVC(1:3).EQ.'EN ')ME=COEFFC(1:3)
          IF(ABVC(1:3).EQ.'ME '.OR.ABVC(1:3).EQ.'EM ')ME=COEFFC(1:3)
          IF(ABVC(1:3).EQ.'MH '.OR.ABVC(1:3).EQ.'HA ')MH=COEFFC(1:3)
          IF(ABVC(1:3).EQ.'DI '.OR.ABVC(1:3).EQ.'DI ')DI=COEFFC(1:3)
          IF(ABVC(1:3).EQ.'SM ')THEN
            SM=COEFFC(1:3)
            FI=COEFFC(1:3)
          ENDIF
          IF(ABVC(1:3).EQ.'AM ')AM=COEFFC(1:3)
          IF(ABVC(1:3).EQ.'FI ')FI=COEFFC(1:3)

        ENDDO

  999   CONTINUE

      END DO

 9996 CONTINUE

      ! Following is to accomodate files with treatment group
      ! at the end of the file
      IF (STARNUM.EQ.STARTRE .AND. XREADNUM.EQ.1) THEN
        CLOSE (FILENUM)
        OPEN (filenum,FILE=filename(1:FILELEN),STATUS='OLD')
        DATANUM=0
        LINE=0
        RETURNUM=0
        TR='N'
        CTR='N'
        STARTCOL=1
        ABVLINE=' '
        GO TO 8888
      ENDIF

 9999 CONTINUE

      CLOSE(filenum)

      IF (CTR.EQ.'N')THEN
        arg=' '
        tvi2=0
        tvi3=0
!       CALL GETARG (0,arg,arglen)
!portability
        CALL GETARG (0,arg)
        arglen = len_trim(arg)

        DO tvi1=1,arglen
          IF (arg(tvi1:tvi1).EQ.SLASH) tvi2=tvi1
          IF (arg(tvi1:tvi1).EQ.'.') tvi3=tvi1-1
        ENDDO
        IF (tvi3.EQ.0) THEN
          tvi3=arglen
        ENDIF
        ctrdirfl=ARG(1:TVI3)//'.CTR'
        ctrdirfl = TRIM(ctrdirfl)
        ! Change control file name from module specific to general
        DO L=LEN(CTRDIRFL),1,-1
          IF (CTRDIRFL(L:L).EQ.SLASH) EXIT
        ENDDO
        IF (L.GT.1) THEN
          ctrdirfl=CTRDIRFL(1:L-1)//SLASH//'CROPSIM.CTR'
        ELSE
          ctrdirfl(1:12)='CROPSIM.CTR '
        ENDIF

        INQUIRE(FILE=ctrdirfl,EXIST=fflag)
        IF(.NOT.fflag)THEN
          CALL Getlun('ERROR.OUT',fnumerr)
          OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
          WRITE (fnumerr,*) ' No control file: ',ctrdirfl(1:60)
          WRITE (fnumerr,*) ' Check READS.OUT for details'
          WRITE (*,*) ' No control file: ',ctrdirfl(1:60)
          WRITE (*,*) ' Program will have to stop'
          WRITE (*,*) ' Check READS.OUT for details'
          CLOSE (fnumerr)
          STOP ' '
        ENDIF
        OPEN(filenum,FILE=ctrdirfl,STATUS='OLD')
        IF(Ctrflag.NE.'Y') THEN
!          WRITE(fnumrea,*)' CONTROLS FROM: ',filenew(1:60)
          Ctrflag = "Y"
        ENDIF  
        STARTCOL=2
        CTR='Y'
        TR='N'
        LE='1 '
        GO TO 4444
      ENDIF

      CLOSE(filenum)

      IF (DATANUM.LE.0.0)THEN
        CALL Getlun('ERROR.OUT',fnumerr)
        OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
        WRITE (fnumerr,*) ' Problem reading: ',filename(1:55)
        WRITE (fnumerr,*) ' No elements in array!',datarray(1)(1:50)
        WRITE (fnumerr,*) ' Check READS.OUT for details'
        WRITE (*,*) ' Problem reading: ',filename(1:55)
        WRITE (*,*) ' No elements in array!',datarray(1)(1:50)
        WRITE (*,*) ' Program will have to stop'
        WRITE (*,*) ' Check READS.OUT for details'
        CLOSE (fnumerr)
        STOP ' '
      ENDIF

      IF (DATANUM.GT.0.AND.DATARRAY(1)(1:6) .EQ. '      ')THEN
        CALL Getlun('ERROR.OUT',fnumerr)
        OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
        WRITE (fnumerr,*) ' Problem reading: ',filename(1:55)
        WRITE (fnumerr,*) ' First array element: ',datarray(1)(1:50)
        WRITE (fnumerr,*) ' 2nd array element: ',datarray(2)(1:50)
        WRITE (fnumerr,*) ' Number in array: ',datanum
        WRITE (fnumerr,*) ' Check READS.OUT for details'
        WRITE (*,*) ' Problem reading: ',filename(1:55)
        WRITE (*,*) ' First array element: ',datarray(1)(1:50)
        WRITE (*,*) ' 2nd array element: ',datarray(2)(1:50)
        WRITE (*,*) ' Number in array: ',datanum
        WRITE (*,*) ' Program will have to stop'
        WRITE (*,*) ' Check READS.OUT for details'
        CLOSE (fnumerr)
        STOP ' '
      ENDIF

! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.
!      WRITE(fnumrea,'(A13)')'  Data array:'
!      DO TVI3=1,DATANUM
!        WRITE(fnumrea,'(I6,A2,A60)')tvi3,'  ',datarray(TVI3)(1:60)
!      ENDDO

      abvfile=' '
      abvfile(1:10)='DETAIL.CDE'
      INQUIRE(FILE=abvfile,EXIST=fflag)
      IF(.NOT.fflag)THEN
!       CALL GETARG (0,arg,arglen)
!portability
        CALL GETARG (0,arg)
        arglen = len_trim(arg)

        DO tvi1=1,arglen
          IF(arg(tvi1:tvi1).EQ.SLASH)tvi2=tvi1
        ENDDO
        abvfile=ARG(1:TVI2)//'DETAIL.CDE'
        INQUIRE(FILE=abvfile,EXIST=fflag)
      ENDIF
      IF(fflag)THEN
        ! Read abbreviation file
        CALL AMAKEABV(ABVFILE,abvarray,abvsyn)
! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.        
!        WRITE(fnumrea,*)' '
!        WRITE(fnumrea,*)' Reading abbrev file: ',abvfile(1:55)
      ELSE
!        WRITE(fnumrea,*)' '
        abvsyn=0
      ENDIF

 5555 CONTINUE

      FILEOLD=' '
      FILEOLD=' '
      FILEOLD(1:FILELEN)=FILENEW(1:FILELEN)

      COL1P=COL1
      COL1RP=COL1R
      COL2P=COL2
      COL3P=COL3
      COL4P=COL4

      CALL ARAYREAD(CODE,DATARRAY,DATANUM,ABVARRAY,ABVSYN,TXTO)
      !WRITE(FNUMREA,*) 'CODE,TXTO ',CODE,TXTO

      RETURN

 7777 CONTINUE

      CALL Getlun('ERROR.OUT',fnumerr)
      OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
      WRITE(fnumerr,*)' Problem in reading treatments in X-file file!'
      WRITE(fnumerr,*)' Original line was: ',dataline(1:50)
      WRITE(fnumerr,*)' Standard line was: ',datastd(1:50)
      WRITE(fnumerr,*)' Text was: ',textstd
      WRITE(fnumerr,*)' Maybe that numbers run together!'
      WRITE(fnumerr,*)' Or dots in header line over numeric fields!'
      WRITE(fnumerr,*)' Check READS.OUT for details'
      WRITE (*,*) ' Problem in reading treatments in X-file file!'
      WRITE (*,*) ' Original line was: ',dataline(1:50)
      WRITE (*,*) ' Standard line was: ',datastd(1:50)
      WRITE (*,*) ' Text was: ',textstd
      WRITE (*,*) ' Maybe that numbers run together!'
      WRITE (*,*) ' Or dots in header line over numeric fields!'
      WRITE (*,*) ' Program will have to stop'
      WRITE (*,*) ' Check READS.OUT for details'
      CLOSE (fnumerr)
      STOP ' '

      END

!-----------------------------------------------------------------------

      SUBROUTINE XREADC(FILENAME,COL1,COL1R,COL2,COL3,COL4,CODE,charout)

      ! Returns one character string for particular code

      IMPLICIT NONE
      EXTERNAL Ltrim, LTRIM2, XREADT

      CHARACTER (LEN=*)   FILENAME, CODE, CHAROUT
      CHARACTER (LEN=254) TL2541, TXTO
      CHARACTER (LEN=100) CODENEW
!chp 2/23/07      CHARACTER (LEN=20)  COEFF
      CHARACTER (LEN=50)  COEFF
      CHARACTER (LEN=1)   FOUND
      INTEGER             TVI1, STARTPOS, ENDPOS
      INTEGER             LENGTH, TVI3, CODECLEN
      INTEGER             COL1, COL1R, COL2, COL3, COL4

      INTRINSIC           LEN

      SAVE

      CALL XREADT (FILENAME,COL1,COL1R,COL2,COL3,COL4,CODE,TXTO)

      CALL LTRIM2(CODE,codenew)
      CODECLEN=LEN(CHAROUT)

      DO TVI1=1,CODECLEN
        CHAROUT(TVI1:TVI1)=' '
      ENDDO

      DO TVI1=1,20
        Coeff(TVI1:TVI1)=' '
      ENDDO

      FOUND='N'
      STARTPOS=1
      ENDPOS=0

      TL2541=TXTO
      CALL Ltrim(TL2541)

      DO TVI3=1, 100
        IF(TL2541(TVI3:TVI3).EQ.' ')THEN
          ENDPOS=TVI3
          FOUND='Y'
          GO TO 1000
        ENDIF
      ENDDO
 1000 CONTINUE

      IF(FOUND.EQ.'Y')THEN
        LENGTH=ENDPOS-STARTPOS+1
        COEFF(1:LENGTH)=TL2541(STARTPOS:ENDPOS)
        CHAROUT=' '
        IF (CODECLEN.LT.20)THEN
          CHAROUT=COEFF(1:CODECLEN)
        ELSE
          CHAROUT(1:20)=COEFF
        ENDIF
       ELSE
         CHAROUT(1:3)='-99'
       ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE XREADR(FILENAME,COL1,COL1R,COL2,COL3,COL4,CODE,valuout)

      ! Returns one real value for particular code

      IMPLICIT NONE
      EXTERNAL XREADC, TVRFROMCCDE, TVRFROMC

      CHARACTER (LEN=*)  FILENAME, CODE
      CHARACTER (LEN=30) CHAROUT
      REAL               VALUOUT, TVRFROMC, TVRFROMCCDE
      INTEGER            COL1, COL1R, COL2, COL3, COL4

      SAVE

      CALL XREADC(FILENAME,COL1,COL1R,COL2,COL3,COL4,CODE,charout)
      VALUOUT=TVRFROMCCDE(Code,Charout(1:12))
      VALUOUT=TVRFROMC(Charout(1:12))

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE XREADI(FILENAME,COL1,COL1R,COL2,COL3,COL4,CODE,valuout)

      ! Returns one real value for particular code

      IMPLICIT NONE
      EXTERNAL Getlun, XREADC, Y4K_DOY, TVRFROMCCDE, TVRFROMC

      CHARACTER (LEN=*)  FILENAME, CODE
      CHARACTER (LEN=30) CHAROUT
      INTEGER            COL1, COL1R, COL2, COL3, COL4
      INTEGER            VALUOUT, YEAR, DAY, FNUMERR  !CSYEARDOY, 
      REAL               VALUETMP, TVRFROMC, TVRFROMCCDE

      INTRINSIC          ICHAR, NINT

      SAVE

      CALL XREADC(FILENAME,COL1,COL1R,COL2,COL3,COL4,CODE,charout)

      ! Following is to remove leading characters if present
      IF(ICHAR(charout(1:1)).GT.64)charout(1:1)=' '
      IF(ICHAR(charout(2:2)).GT.64)charout(2:2)=' '

      VALUOUT=TVRFROMCCDE(Code,Charout(1:12))
      VALUETMP=TVRFROMC(Charout(1:12))
      VALUOUT=NINT(VALUETMP)

      ! Following is to return yeardoy instead of yrdoy
      IF (VALUOUT.GT.0 .AND. VALUOUT.LE.99365) THEN
        !IF (CODE.EQ.'SDATE') VALUOUT=CSYEARDOY (valuout)
        IF (CODE.EQ.'SDATE') CALL Y4K_DOY(VALUOUT,FILENAME,0,'CSREAD',3)
        !IF (CODE.EQ.'PDATE') VALUOUT=CSYEARDOY (valuout)
        IF (CODE.EQ.'PDATE') CALL Y4K_DOY(VALUOUT,FILENAME,0,'CSREAD',3)
        !IF (CODE.EQ.'ICDAT') VALUOUT=CSYEARDOY (valuout)
        IF (CODE.EQ.'ICDAT') CALL Y4K_DOY(VALUOUT,FILENAME,0,'CSREAD',3)
        !IF (CODE.EQ.'ICRDT') VALUOUT=CSYEARDOY (valuout)
        IF (CODE.EQ.'ICRDT') CALL Y4K_DOY(VALUOUT,FILENAME,0,'CSREAD',3)
        !IF (CODE.EQ.'IDATE') VALUOUT=CSYEARDOY (valuout)
        IF (CODE.EQ.'IDATE') CALL Y4K_DOY(VALUOUT,FILENAME,0,'CSREAD',3)
        !IF (CODE.EQ.'FDATE') VALUOUT=CSYEARDOY (valuout)
        IF (CODE.EQ.'FDATE') CALL Y4K_DOY(VALUOUT,FILENAME,0,'CSREAD',3)
        !IF (CODE.EQ.'RDATE') VALUOUT=CSYEARDOY (valuout)
        IF (CODE.EQ.'RDATE') CALL Y4K_DOY(VALUOUT,FILENAME,0,'CSREAD',3)
        !IF (CODE.EQ.'PFRST') VALUOUT=CSYEARDOY (valuout)
        IF (CODE.EQ.'PFRST') CALL Y4K_DOY(VALUOUT,FILENAME,0,'CSREAD',3)
        !IF (CODE.EQ.'PLAST') VALUOUT=CSYEARDOY (valuout)
        IF (CODE.EQ.'PLAST') CALL Y4K_DOY(VALUOUT,FILENAME,0,'CSREAD',3)
        !IF (CODE.EQ.'CDATE') VALUOUT=CSYEARDOY (valuout)
        IF (CODE.EQ.'CDATE') CALL Y4K_DOY(VALUOUT,FILENAME,0,'CSREAD',3)
        !IF (CODE.EQ.'TDATE') VALUOUT=CSYEARDOY (valuout)
        IF (CODE.EQ.'TDATE') CALL Y4K_DOY(VALUOUT,FILENAME,0,'CSREAD',3)
        !IF (CODE.EQ.'ODATE') VALUOUT=CSYEARDOY (valuout)
        IF (CODE.EQ.'ODATE') CALL Y4K_DOY(VALUOUT,FILENAME,0,'CSREAD',3)
        !IF (CODE.EQ.'HDATE') VALUOUT=CSYEARDOY (valuout)
        IF (CODE.EQ.'HDATE') CALL Y4K_DOY(VALUOUT,FILENAME,0,'CSREAD',3)
      ENDIF

      ! Following is to read years and days instead of date
      IF(CODE.EQ.'ICDAT'.AND.VALUOUT.EQ.-99)THEN
        CALL XREADC(FILENAME,COL1,COL1R,COL2,COL3,COL4,'ICDAY',charout)
        VALUETMP=TVRFROMCCDE(Code,Charout(1:12))
        VALUETMP=TVRFROMC(Charout(1:12))
        IF (VALUETMP.LE.0) THEN
          CALL XREADC(FILENAME,COL1,COL1R,COL2,COL3,COL4,'ICDY',charout)
        ENDIF
        VALUETMP=TVRFROMCCDE(Code,Charout(1:12))
        VALUETMP=TVRFROMC(Charout(1:12))
        DAY=NINT(VALUETMP)
        CALL XREADC(FILENAME,COL1,COL1R,COL2,COL3,COL4,'ICYR',charout)
        VALUETMP=TVRFROMCCDE(Code,Charout(1:12))
        VALUETMP=TVRFROMC(Charout(1:12))
        YEAR=NINT(VALUETMP)
        VALUOUT=YEAR*1000+DAY
        !VALUOUT=CSYEARDOY (valuout)
        CALL Y4K_DOY(VALUOUT,FILENAME,0,'CSREAD',3)
      ENDIF
      IF(CODE.EQ.'ICRDT'.AND.VALUOUT.EQ.-99)THEN
        CALL XREADC(FILENAME,COL1,COL1R,COL2,COL3,COL4,'ICRDY',charout)
        VALUETMP=TVRFROMCCDE(Code,Charout(1:12))
        VALUETMP=TVRFROMC(Charout(1:12))
        IF (VALUETMP.LE.0) THEN
          CALL XREADC(FILENAME,COL1,COL1R,COL2,COL3,COL4,'ICRD',charout)
        ENDIF
        VALUETMP=TVRFROMCCDE(Code,Charout(1:12))
        VALUETMP=TVRFROMC(Charout(1:12))
        DAY=NINT(VALUETMP)
        CALL XREADC(FILENAME,COL1,COL1R,COL2,COL3,COL4,'ICRYR',charout)
        VALUETMP=TVRFROMCCDE(Code,Charout(1:12))
        VALUETMP=TVRFROMC(Charout(1:12))
        YEAR=NINT(VALUETMP)
        VALUOUT=YEAR*1000+DAY
        !VALUOUT=CSYEARDOY (valuout)
        CALL Y4K_DOY(VALUOUT,FILENAME,0,'CSREAD',3)
      ENDIF
      IF(CODE.EQ.'PDATE'.AND.VALUOUT.EQ.-99)THEN
        CALL XREADC(FILENAME,COL1,COL1R,COL2,COL3,COL4,'PLDAY',charout)
        VALUETMP=TVRFROMCCDE(Code,Charout(1:12))
        VALUETMP=TVRFROMC(Charout(1:12))
        DAY=NINT(VALUETMP)
        IF(DAY.LE.0)THEN
         CALL XREADC(FILENAME,COL1,COL1R,COL2,COL3,COL4,'PLDOY',charout)
         VALUETMP=TVRFROMCCDE(Code,Charout(1:12))
         VALUETMP=TVRFROMC(Charout(1:12))
         DAY=NINT(VALUETMP)
        ENDIF
        CALL XREADC(FILENAME,COL1,COL1R,COL2,COL3,COL4,'PLYR',charout)
        VALUETMP=TVRFROMCCDE(Code,Charout(1:12))
        VALUETMP=TVRFROMC(Charout(1:12))
        YEAR=NINT(VALUETMP)
        IF(DAY.LE.0.OR.YEAR.LE.0)THEN
          CALL Getlun('ERROR.OUT',fnumerr)
          OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
          WRITE (fnumerr,*) ' No planting date,day,or year!'
          WRITE (fnumerr,*) ' Check READS.OUT for details'
          WRITE (*,*) ' No planting date,day,or year!'
          WRITE (*,*) ' Program will have to stop'
          WRITE (*,*) ' Check READS.OUT for details'
          CLOSE (fnumerr)
          STOP ' '
        ENDIF
        VALUOUT=YEAR*1000+DAY
        !VALUOUT=CSYEARDOY (valuout)
        CALL Y4K_DOY(VALUOUT,FILENAME,0,'CSREAD',3)
      ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE XREADCA(FNAME,C1,C1R,C2,C3,C4,CODE,AOUTSZ,arrayout)

      ! Returns ARRAY for particular code

      IMPLICIT NONE
      EXTERNAL Standard, LTRIM2, XREADT, TVIFROMC

      CHARACTER (LEN=*)    FNAME, CODE, AOUTSZ
      CHARACTER (LEN=*)    ARRAYOUT(*)
      CHARACTER (LEN=354)  TL3541
      CHARACTER (LEN=254)  TL2541,TXTO
      CHARACTER (LEN=100)  CODENEW
      INTEGER              TVIFROMC
      INTEGER              TVI1, L, L2, SIZE
      INTEGER              C1, C1R, C2, C3, C4, LENARVAR

      INTRINSIC            LEN

      SAVE

      CALL XREADT(FNAME,C1,C1R,C2,C3,C4,CODE,TXTO)

      CALL LTRIM2(CODE,codenew)

      SIZE=TVIFROMC(AOUTSZ)

      DO TVI1=1,SIZE
        ARRAYOUT(TVI1)=' '
      ENDDO

      LENARVAR=LEN(arrayout(1))

      TL2541=TXTO
      CALL Standard(TL2541,TL3541,'7')
      DO L=1,SIZE
         L2=(L-1)*7 + 1
         IF (TL3541(L2:L2).EQ.'!')GO TO 1000
         IF (L2.GT.354)GO TO 1000
         IF (L2+LENARVAR.GT.354)GO TO 1000
         !IF (TL3541(L2:L2).EQ.' ')GO TO 1000
         IF (TL3541(L2:L2).EQ.' ') THEN
           IF (LENARVAR.GE.3) THEN 
             ARRAYOUT(L)(1:3)='-99'
           ELSEIF (LENARVAR.EQ.2) THEN
             ARRAYOUT(L)(1:2)=' .'
           ELSE 
             ARRAYOUT(L)(1:1)='.'
           ENDIF  
         ELSE
           ARRAYOUT(L)(1:LENARVAR)=TL3541(L2:L2+(LENARVAR))
         ENDIF
      END DO
 1000 CONTINUE

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE XREADRA(FNAME,C1,C1R,C2,C3,C4,CODE,AOUTSIZE,arrayout)

      ! Returns ARRAY for particular code

      IMPLICIT NONE
      EXTERNAL Standard, LTRIM2, XREADT, TVRFROMCCDE, TVRFROMC

      CHARACTER (LEN=*)   FNAME, CODE, AOUTSIZE
      CHARACTER (LEN=354) TL3541
      CHARACTER (LEN=254) TL2541,TXTO
      CHARACTER (LEN=100) CODENEW
      REAL                TVR1, TVRFROMC, ARRAYOUT(*), TVRFROMCCDE
      INTEGER             TVI1, L, L2, SIZE
      INTEGER             C1, C1R, C2, C3, C4

      INTRINSIC           NINT

      SAVE

      CALL XREADT(FNAME,C1,C1R,C2,C3,C4,CODE,TXTO)

      CALL LTRIM2(CODE,codenew)

      TVR1=TVRFROMCCDE(CODE,AOUTSIZE)
      TVR1=TVRFROMC(AOUTSIZE)
      SIZE=NINT(TVR1)

      DO TVI1=1,SIZE
        ARRAYOUT(TVI1)=-99
      ENDDO

      TL2541=TXTO
      CALL Standard(TL2541,TL3541,'7')
      DO L=1,SIZE
         L2=(L-1)*7 + 1
         IF (TL3541(L2:L2).EQ.'!')GO TO 1000
         IF(ICHAR(tl3541(L2:L2)).GT.64)tl3541(L2:L2)=' '
         IF(ICHAR(tl3541(L2+1:L2+1)).GT.64)tl3541(L2+1:L2+1)=' '
         IF (TL3541(L2:L2).EQ.' ')GO TO 1000
         ARRAYOUT(L)=TVRFROMCCDE(TL3541,TL3541(L2:L2+5))
         ARRAYOUT(L)=TVRFROMC(TL3541(L2:L2+5))
      END DO
 1000 CONTINUE

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE XREADIA(FNAME,C1,C1R,C2,C3,C4,CODE,AOUTSIZE,arrayout)

      ! Returns ARRAY for particular code

      IMPLICIT NONE
      EXTERNAL Standard, LTRIM2
      EXTERNAL TVRFROMCCDE, TVRFROMC, XREADT, CSYEARDOY, TVIFROMC

      CHARACTER (LEN=*)   FNAME, CODE, AOUTSIZE
      CHARACTER (LEN=354) TL3541, TL3542
      CHARACTER (LEN=254) TL2541, TXTO, TXTO2, TL2542
      CHARACTER (LEN=100) CODENEW
      CHARACTER (LEN=1)   CODEFLAG
      REAL                TVR1, VALUETMP, TVRFROMC, TVRFROMCCDE
      INTEGER             TVIFROMC, YEAR, C1, C1R, C2, C3, C4
      INTEGER             TVI1, L, L2, SIZE
      INTEGER             CSYEARDOY, ARRAYOUT(*)

      INTRINSIC           ICHAR,NINT

      SAVE

      CALL XREADT(FNAME,C1,C1R,C2,C3,C4,CODE,TXTO)

      CALL LTRIM2(CODE,codenew)

      TVR1=TVRFROMCCDE('AOUTSIZE',AOUTSIZE)
      TVR1=TVRFROMC(AOUTSIZE)
      SIZE=NINT(TVR1)

      DO TVI1=1,SIZE
        ARRAYOUT(TVI1)=-99
      ENDDO

      TL2541=TXTO
      CALL Standard(TL2541,TL3541,'8')
      DO L=1,SIZE
         L2=(L-1)*8 + 1
         IF (TL3541(L2:L2).EQ.'!')GO TO 1000
         IF(ICHAR(tl3541(L2:L2)).GT.64)tl3541(L2:L2)=' '
         IF(ICHAR(tl3541(L2+1:L2+1)).GT.64)tl3541(L2+1:L2+1)=' '
         IF (TL3541(L2:L2).EQ.' ')GO TO 1000
         VALUETMP=TVRFROMCCDE(TL3541,TL3541(L2:L2+6))
         VALUETMP=TVRFROMC(TL3541(L2:L2+6))
         ARRAYOUT(L)=NINT(VALUETMP)
         ! Following is to return yeardoy instead of yrdoy
         IF (ARRAYOUT(L).GT.0 .AND. ARRAYOUT(L).LE.99365) THEN
           IF (CODE.EQ.'IDATE') ARRAYOUT(L)=CSYEARDOY (arrayout(l))
           IF (CODE.EQ.'FDATE') ARRAYOUT(L)=CSYEARDOY (arrayout(l))
           IF (CODE.EQ.'RDATE') ARRAYOUT(L)=CSYEARDOY (arrayout(l))
           IF (CODE.EQ.'TDATE') ARRAYOUT(L)=CSYEARDOY (arrayout(l))
           IF (CODE.EQ.'ODATE') ARRAYOUT(L)=CSYEARDOY (arrayout(l))
           IF (CODE.EQ.'HDATE') ARRAYOUT(L)=CSYEARDOY (arrayout(l))
         ENDIF
      END DO
 1000 CONTINUE

      IF(ARRAYOUT(1).EQ.-99)THEN
        CODEFLAG='N'
        IF(CODE.EQ.'FDATE'.OR.CODE.EQ.'IDATE')CODEFLAG='Y'
        IF(CODE.EQ.'WMDAT'.OR.CODE.EQ.'ODATE')CODEFLAG='Y'
        IF(CODEFLAG.EQ.'Y')THEN
          IF(CODE.EQ.'FDATE')THEN
            CALL XREADT(FNAME,C1,C1R,C2,C3,C4,'FEDAY',TXTO)
            CALL XREADT(FNAME,C1,C1R,C2,C3,C4,'FEYR',TXTO2)
          ELSEIF(CODE.EQ.'IDATE')THEN
            CALL XREADT(FNAME,C1,C1R,C2,C3,C4,'IRDAY',TXTO)
            CALL XREADT(FNAME,C1,C1R,C2,C3,C4,'IRYR',TXTO2)
          ELSEIF(CODE.EQ.'ODATE'.OR.CODE.EQ.'WMDATE')THEN
            CALL XREADT(FNAME,C1,C1R,C2,C3,C4,'EMDAY',TXTO)
            CALL XREADT(FNAME,C1,C1R,C2,C3,C4,'EMYR',TXTO2)
          ENDIF
          IF (TXTO(1:3).EQ.'-99') GO TO 1001
          TL2541=TXTO
          TL2542=TXTO2
          CALL Standard(TL2541,TL3541,'8')
          CALL Standard(TL2542,TL3542,'8')
          DO L=1,SIZE
            L2=(L-1)*8 + 1
            IF (TL3541(L2:L2).EQ.'!')GO TO 1001
            IF (TL3541(L2:L2).EQ.' ')GO TO 1001
            IF(ICHAR(tl3541(L2:L2)).GT.64)tl3541(L2:L2)=' '
            IF(ICHAR(tl3541(L2+1:L2+1)).GT.64)tl3541(L2+1:L2+1)=' '
            ARRAYOUT(L)=TVIFROMC(TL3541(L2:L2+6))
            IF (TL3542(L2:L2).EQ.'!')GO TO 1001
            IF (TL3542(L2:L2).EQ.' ')GO TO 1001
            IF(ICHAR(tl3542(L2:L2)).GT.64)tl3542(L2:L2)=' '
            IF(ICHAR(tl3542(L2+1:L2+1)).GT.64)tl3542(L2+1:L2+1)=' '
            YEAR=TVIFROMC(TL3542(L2:L2+6))
            ARRAYOUT(L)=YEAR*1000+ARRAYOUT(L)
            ARRAYOUT(L)=CSYEARDOY(ARRAYOUT(L))
          END DO
 1001     CONTINUE
        ENDIF
      ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE AREADT(FILENAME,LEVELI,RN,SN,ON,CN,CODE,TXTO)

      USE OSDefinitions

      IMPLICIT NONE
      EXTERNAL TVILENT, TL10FROMI, LTRIM2, AMAKE5, AMAKEABV, ARAYREAD

      CHARACTER (LEN=254) FILENEW, FILEOLD, DATARRAY(500),ABVARRAY(1000)
      CHARACTER (LEN=128) ARG
      character (LEN=100) CODENEW
      CHARACTER (LEN=132) ABVFILE
      CHARACTER (LEN=93)  ADIRFILE
      CHARACTER (LEN=10)  TL10FROMI, LEVEL
!     CHARACTER (LEN=1)   FNUMFLAG
      CHARACTER (LEN=*)   FILENAME, CODE, TXTO
      INTEGER             TVILENT, TVI1, TVI2
      INTEGER             LEVELLEN, FILELEN, LEVELI, LEVELIP
      INTEGER             DATANUM, LENSTD, ARGLEN
      INTEGER             ABVSYN  !, FNUMREA
      INTEGER             RN, SN, ON, CN
      LOGICAL             FFLAG  !, FOPEN

      INTRINSIC           MIN

      SAVE

! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.
!      IF (FNUMFLAG.NE.'Y')THEN
!        CALL Getlun('READS.OUT',fnumrea)
!        INQUIRE (FILE='READS.OUT',OPENED=fopen)
!        IF (.NOT.fopen) OPEN(UNIT=fnumrea,FILE='READS.OUT')
!        FNUMFLAG = 'Y'
!      ENDIF

      ! File
      CALL LTRIM2(FILENAME,filenew)
      FILELEN=TVILENT(FILENEW)
      FILELEN=MIN(254,FILELEN)
      FILENEW=' '
      FILENEW(1:FILELEN)=FILENAME(1:FILELEN)

      ! Level
      LENSTD=12
      LEVEL=TL10FROMI(LEVELI)
      LEVELLEN=TVILENT(LEVEL)
      IF(LEVELLEN.GT.LENSTD)LEVELLEN=LENSTD

      ! Code
      CALL LTRIM2(CODE,codenew)

      ! Check if have already read file
      IF(FILENEW.EQ.FILEOLD)THEN
        IF(LEVELI.EQ.LEVELIP)THEN
          GOTO 5555
        ENDIF
      ENDIF

      ! Check A-file name
      ADIRFILE=' '
      ADIRFILE=FILENAME(1:FILELEN-1)//'A'

      CALL AMAKE5(ADIRFILE,LEVEL,RN,SN,ON,CN,DATARRAY,datanum)

      abvfile=' '
      abvfile(1:8)='DATA.CDE'
      INQUIRE(FILE=abvfile,EXIST=fflag)
      IF(.NOT.fflag)THEN
!       CALL GETARG (0,arg,arglen)
!portability
        CALL GETARG (0,arg)
        arglen = len_trim(arg)

        DO tvi1=1,arglen
          IF(arg(tvi1:tvi1).EQ.SLASH)tvi2=tvi1
        ENDDO
        abvfile=ARG(1:TVI2)//'DATA.CDE'
        INQUIRE(FILE=abvfile,EXIST=fflag)
      ENDIF
      IF(fflag)THEN
        ! Read abbreviation file
        CALL AMAKEABV(ABVFILE,abvarray,abvsyn)
! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.        
!        WRITE(fnumrea,*)' '
!        WRITE(fnumrea,*)' Reading abbrev file: ',abvfile(1:55)
      ELSE
!        WRITE(fnumrea,*)' '
!        WRITE(fnumrea,*)' No abbreviation file: ',abvfile(1:55)
        abvsyn=0
      ENDIF

 5555 CONTINUE

      FILEOLD=' '
      FILEOLD(1:FILELEN)=FILENEW(1:FILELEN)
      LEVELIP=LEVELI

      CALL ARAYREAD(CODE,DATARRAY,DATANUM,ABVARRAY,ABVSYN,TXTO)
      !WRITE(FNUMREA,*) 'CODE,TXTO ',CODE,TXTO

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE AREADC(FILENAME,LEVELI,RN,SN,ON,CN,CODE,charout)

      ! Returns one character string for particular code

      IMPLICIT NONE
      EXTERNAL Ltrim, LTRIM2, AREADT

      CHARACTER (LEN=*)   CODE,CHAROUT,FILENAME
      CHARACTER (LEN=254) TL2541, TXTO
      CHARACTER (LEN=100) CODENEW
      CHARACTER (LEN=20)  COEFF
      CHARACTER (LEN=1)   FOUND
      INTEGER             TVI1, STARTPOS, ENDPOS
      INTEGER             LENGTH, TVI3, LEVELI, CODECLEN, RN, SN, ON, CN

      INTRINSIC           LEN

      SAVE

      CALL AREADT(FILENAME,LEVELI,RN,SN,ON,CN,CODE,TXTO)

      CALL LTRIM2(CODE,codenew)
      CODECLEN=LEN(CHAROUT)

      DO TVI1=1,CODECLEN
        CHAROUT(TVI1:TVI1)=' '
      ENDDO

      DO TVI1=1,20
        Coeff(TVI1:TVI1)=' '
      ENDDO

      FOUND='N'
      STARTPOS=1
      ENDPOS=0

      TL2541=TXTO
      CALL Ltrim(TL2541)

      DO TVI3=1, 100
        IF(TL2541(TVI3:TVI3).EQ.' ')THEN
          ENDPOS=TVI3
          FOUND='Y'
          GO TO 1000
        ENDIF
      ENDDO
 1000 CONTINUE
      IF(FOUND.EQ.'Y')THEN
        LENGTH=ENDPOS-STARTPOS+1
        COEFF(1:LENGTH)=TL2541(STARTPOS:ENDPOS)
        CHAROUT=' '
        IF (CODECLEN.LT.20)THEN
          CHAROUT=COEFF(1:CODECLEN)
        ELSE
          CHAROUT(1:20)=COEFF
        ENDIF
       ELSE
         CHAROUT=' '
         CHAROUT(1:3)='-99'
       ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE AREADR(FILENAME,LEVELI,RN,SN,ON,CN,CODE,valueout)

      ! Returns one real value for particular code

      IMPLICIT NONE
      EXTERNAL AREADC, TVRFROMCCDE, TVRFROMC

      CHARACTER (LEN=*)  CODE,FILENAME
      CHARACTER (LEN=30) CHAROUT
      INTEGER            LEVELI, RN, SN, ON, CN
      REAL               VALUEOUT, TVRFROMC, TVRFROMCCDE

      SAVE

      CALL AREADC(FILENAME,LEVELI,RN,SN,ON,CN,CODE,charout)
      VALUEOUT=TVRFROMCCDE(CODE,Charout(1:12))
      VALUEOUT=TVRFROMC(Charout(1:12))

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE AREADI(FILENAME,LEVELI,RN,SN,ON,CN,CODE,valueout)

      ! Returns one real value for particular code

      IMPLICIT NONE
      EXTERNAL AREADC, TVRFROMCCDE, TVRFROMC

      CHARACTER (LEN=*)  CODE,FILENAME
      CHARACTER (LEN=30) CHAROUT
      INTEGER            LEVELI, VALUEOUT, RN, SN, ON, CN
      REAL               VALUETMP, TVRFROMC, TVRFROMCCDE

      INTRINSIC          NINT

      SAVE

      CALL AREADC(FILENAME,LEVELI,RN,SN,ON,CN,CODE,charout)
      VALUETMP=TVRFROMCCDE(Code,Charout(1:12))
      VALUETMP=TVRFROMC(Charout(1:12))
      VALUEOUT=NINT(VALUETMP)

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE CUREADT(CUDIRFLE,LEVEL,CODE,TXTO)

      IMPLICIT NONE
      EXTERNAL TVILENT, LTRIM2, AMAKE1, ARAYREAD

      CHARACTER (LEN=*)   CODE, TXTO, cudirfle, LEVEL
      CHARACTER (LEN=254) DATARRAY(500), ABVARRAY(1000)
      character (LEN=100) codenew
      CHARACTER (LEN=93)  CUDIRFLP, CUDIRFLN
      CHARACTER (LEN=10)  LEVELP,LEVELSTD
!     CHARACTER (LEN=1)   FNUMFLAG
      INTEGER             TVILENT !, FNUMREA
      INTEGER             LEVELLEN, FILELEN, DATANUM, LENSTD
      INTEGER             ABVSYN
!     LOGICAL             FOPEN 

      INTRINSIC           MIN

      SAVE

! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.
!      IF (FNUMFLAG.NE.'Y')THEN
!        CALL Getlun('READS.OUT',fnumrea)
!        INQUIRE (FILE='READS.OUT',OPENED=fopen)
!        IF (.NOT.fopen) OPEN(UNIT=fnumrea,FILE='READS.OUT')
!        FNUMFLAG = 'Y'
!      ENDIF

      LENSTD=12

      LEVELLEN=TVILENT(LEVEL)
      IF(LEVELLEN.GT.LENSTD)LEVELLEN=LENSTD
      LEVELSTD='      '
      LEVELSTD(1:LEVELLEN)=LEVEL(1:LEVELLEN)

      CALL LTRIM2(CODE,codenew)

      CALL LTRIM2(CUDIRFLE,cudirfln)
      FILELEN=TVILENT(CUDIRFLN)
      FILELEN=MIN(93,FILELEN)
      CUDIRFLN=' '
      CUDIRFLN(1:FILELEN)=CUDIRFLE(1:FILELEN)

      LEVELLEN=TVILENT(LEVEL)
      IF(LEVELLEN.GT.LENSTD)LEVELLEN=LENSTD

      ! Check if have already read file
      IF(CUDIRFLN.EQ.CUDIRFLP)THEN
        IF(LEVEL(1:LEVELLEN).EQ.LEVELP(1:LEVELLEN))THEN
          GOTO 5555
        ENDIF
      ENDIF

      CALL AMAKE1(CUDIRFLN,LEVELSTD,DATARRAY,datanum)

      ! No abbreviation file. Headers must be standard!
      abvsyn=0

 5555 CONTINUE

      CALL ARAYREAD(CODE,DATARRAY,DATANUM,ABVARRAY,ABVSYN,TXTO)
      !WRITE(FNUMREA,*) 'CODE,TXTO ',CODE,TXTO

      CUDIRFLP=' '
      CUDIRFLP(1:FILELEN)=CUDIRFLN(1:FILELEN)

      levelp=' '
      levelp(1:levellen)=levelstd(1:levellen)

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE CUREADC(CUDIRFLE,LEVEL,CODE,charout)

      ! Returns one character string for particular code

      IMPLICIT NONE
      EXTERNAL Getlun, LTRIM, LTRIM2, CUREADT

      CHARACTER (LEN=*)   CODE, CHAROUT, LEVEL, CUDIRFLE
      CHARACTER (LEN=254) TL2541, TXTO
      CHARACTER (LEN=100) CODENEW
      CHARACTER (LEN=20)  COEFF
      CHARACTER (LEN=1)   FOUND
      INTEGER             TVI1, STARTPOS, ENDPOS
      INTEGER             LENGTH, TVI3, CODECLEN, FNUMERR

      INTRINSIC           LEN

      SAVE

      CALL CUREADT(CUDIRFLE,LEVEL,CODE,TXTO)
      CALL LTRIM2(CODE,codenew)
      CODECLEN=LEN(CHAROUT)

      DO TVI1=1,CODECLEN
        CHAROUT(TVI1:TVI1)=' '
      ENDDO

      DO TVI1=1,20
        Coeff(TVI1:TVI1)=' '
      ENDDO

      FOUND='N'
      STARTPOS=1
      ENDPOS=0

      TL2541=TXTO
      CALL Ltrim(TL2541)

      DO TVI3=1, 100
        IF(TL2541(TVI3:TVI3).EQ.' ')THEN
          ENDPOS=TVI3
          FOUND='Y'
          GO TO 1000
        ENDIF
      ENDDO
 1000 CONTINUE
      IF(FOUND.EQ.'Y')THEN
        LENGTH=ENDPOS-STARTPOS+1
        COEFF(1:LENGTH)=TL2541(STARTPOS:ENDPOS)
        CHAROUT=' '
        IF (CODECLEN.LT.20)THEN
          CHAROUT=COEFF(1:CODECLEN)
        ELSE
          CHAROUT(1:20)=COEFF
        ENDIF
       ELSE
         CALL Getlun('ERROR.OUT',fnumerr)
         OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
         WRITE (fnumerr,*) ' Problem reading cultivar file'
         WRITE (fnumerr,*) ' File was: ',cudirfle(1:60)
         WRITE (fnumerr,*) ' Could not find code: ',Code
         WRITE (fnumerr,*) ' Check READS.OUT for details'
         WRITE (*,*) ' Problem reading cultivar file'
         WRITE (*,*) ' File was: ',cudirfle(1:60)
         WRITE (*,*) ' Could not find code: ',Code
         WRITE (*,*) ' Program will have to stop'
         WRITE (*,*) ' Check READS.OUT for details'
         CLOSE (fnumerr)
         STOP ' '
       ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE CUREADR(CUDIRFLE,LEVEL,CODE,valueout)

      ! Returns one real value for particular code

      IMPLICIT NONE
      EXTERNAL CUREADC, TVRFROMCCDE, TVRFROMC

      CHARACTER (LEN=*)  CODE,LEVEL,CUDIRFLE
      CHARACTER (LEN=30) CHAROUT
      REAL               VALUEOUT,TVRFROMC, TVRFROMCCDE

      SAVE

      CALL CUREADC(CUDIRFLE,LEVEL,CODE,charout)
      VALUEOUT=TVRFROMCCDE(Code,Charout(1:12))
      VALUEOUT=TVRFROMC(Charout(1:12))

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE ECREADT(ECDIRFLE,LEVEL,CODE,TXTO)

      IMPLICIT NONE
      EXTERNAL TVILENT, LTRIM2, AMAKE1, ARAYREAD

      CHARACTER (LEN=254) DATARRAY(500), ABVARRAY(1000)
      character (LEN=100) CODENEW
      CHARACTER (LEN=93)  ECDIRFLT, ECDIRFLP
      CHARACTER (LEN=10)  LEVELP, LEVELSTD
!     CHARACTER (LEN=1)   FNUMFLAG
      CHARACTER (LEN=*)   CODE, TXTO, ECDIRFLE, LEVEL
      INTEGER             TVILENT
      INTEGER             LEVELLEN, FILELEN, DATANUM, LENSTD
      INTEGER             ABVSYN !, FNUMREA
!     LOGICAL             FOPEN

      INTRINSIC           MIN

      SAVE

! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.
!      IF (FNUMFLAG.NE.'Y')THEN
!        CALL Getlun('READS.OUT',fnumrea)
!        INQUIRE (FILE='READS.OUT',OPENED=fopen)
!        IF (.NOT.fopen) OPEN(UNIT=fnumrea,FILE='READS.OUT')
!        FNUMFLAG = 'Y'
!      ENDIF

      CALL LTRIM2(ECDIRFLE,ecdirflt)
      FILELEN=TVILENT(ECDIRFLT)
      FILELEN=MIN(93,FILELEN)
      ECDIRFLT=' '
      ECDIRFLT(1:FILELEN)=ECDIRFLE(1:FILELEN)

      LENSTD=12

      LEVELLEN=TVILENT(LEVEL)
      IF(LEVELLEN.GT.LENSTD)LEVELLEN=LENSTD
      LEVELSTD='      '
      LEVELSTD(1:LEVELLEN)=LEVEL(1:LEVELLEN)

      CALL LTRIM2(CODE,codenew)

      IF(ECDIRFLT(1:FILELEN).EQ.ECDIRFLP(1:FILELEN))THEN
        IF(LEVEL(1:LEVELLEN).EQ.LEVELP(1:LEVELLEN))GO TO 5555
      ENDIF

      CALL AMAKE1(ECDIRFLT,LEVELSTD,DATARRAY,datanum)

      ! No abbreviation file. Headers must be standard!
      abvsyn=0

 5555 CONTINUE

      CALL ARAYREAD(CODE,DATARRAY,DATANUM,ABVARRAY,ABVSYN,TXTO)

      ECDIRFLP=' '
      ECDIRFLP(1:FILELEN)=ECDIRFLT(1:FILELEN)
      levelp(1:levellen)=levelstd(1:levellen)

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE ECREADC(ECDIRFLE,LEVEL,CODE,charout)

      ! Returns one character string for particular code

      IMPLICIT NONE
      EXTERNAL Ltrim, LTRIM2, ECREADT

      CHARACTER (LEN=*)   CODE,CHAROUT,LEVEL,ECDIRFLE
      CHARACTER (LEN=254) TL2541, TXTO
      CHARACTER (LEN=100) CODENEW
      CHARACTER (LEN=40)  COEFF
      CHARACTER (LEN=1)   FOUND
      INTEGER             TVI1, STARTPOS, ENDPOS
      INTEGER             LENGTH, TVI3, CODECLEN

      INTRINSIC           LEN

      SAVE

      CALL ECREADT(ECDIRFLE,LEVEL,CODE,TXTO)

      CALL LTRIM2(CODE,codenew)
      CODECLEN=LEN(CHAROUT)

      DO TVI1=1,CODECLEN
        CHAROUT(TVI1:TVI1)=' '
      ENDDO

      DO TVI1=1,40
        Coeff(TVI1:TVI1)=' '
      ENDDO

      FOUND='N'
      STARTPOS=1
      ENDPOS=0

      TL2541=TXTO
      CALL Ltrim(TL2541)

      DO TVI3=1, 100
        IF(TL2541(TVI3:TVI3).EQ.' ')THEN
          ENDPOS=TVI3
          FOUND='Y'
          GO TO 1000
        ENDIF
      ENDDO
 1000 CONTINUE
      IF(FOUND.EQ.'Y')THEN
        LENGTH=ENDPOS-STARTPOS+1
        COEFF(1:LENGTH)=TL2541(STARTPOS:ENDPOS)
        CHAROUT=' '
        IF (CODECLEN.LT.40)THEN
          CHAROUT=COEFF(1:CODECLEN)
        ELSE
          CHAROUT(1:40)=COEFF
        ENDIF
       ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE ECREADR(ECDIRFLE,LEVEL,CODE,valueout)

      ! Returns one real value for particular code

      IMPLICIT NONE
      EXTERNAL ECREADC, TVRFROMCCDE, TVRFROMC

      CHARACTER (LEN=*)  CODE,LEVEL,ECDIRFLE
      CHARACTER (LEN=30) CHAROUT
      REAL               VALUEOUT, TVRFROMC, TVRFROMCCDE

      SAVE

      CALL ECREADC(ECDIRFLE,LEVEL,CODE,charout)
      VALUEOUT=TVRFROMCCDE(Code,Charout(1:12))
      VALUEOUT=TVRFROMC(Charout(1:12))

      RETURN

      END


!-----------------------------------------------------------------------

      SUBROUTINE SPREADT(SPDIRFLE,CODE,TXTO)

      IMPLICIT NONE
      EXTERNAL Getlun, TVILENT, LTRIM
      EXTERNAL Standard, LTRIM2, STANDC, TVINSTR, ARAYREAD

      CHARACTER (LEN=1000) DATASTD, ABVSTD
      CHARACTER (LEN=254)  DATARRAY(500), TL2541, ABVLINE, DATALINE
      CHARACTER (LEN=254)  ABVARRAY(1000)
      CHARACTER (LEN=100)  CODENEW
      CHARACTER (LEN=93)   SPDIRFLT, SPDIRFLP
      CHARACTER (LEN=52)   TEXTSTD
      CHARACTER (LEN=12)   COEFFC(200), ABVC(200), COEFFCTM
      CHARACTER (LEN=4)    COEFFCHK
      CHARACTER (LEN=1)    FNUMFLAG
      CHARACTER (LEN=*)    SPDIRFLE, CODE, TXTO
      INTEGER              TVILENT, TVI1, TVI2, TVI0, TVI4 !, TVI3
      INTEGER              FILENUM, FILELEN, LINE, LENTMP
      INTEGER              TVINSTR, ABVNUM, LENSTD, ABVLEN, DATANUM
      INTEGER              ABVSYN, L, FNUMERR  !, FNUMREA
!     LOGICAL              FOPEN

      INTRINSIC            MIN

      SAVE

      IF (FNUMFLAG.NE.'Y')THEN
        CALL GETLUN('FILETMP',FILENUM)
        FNUMFLAG = 'Y'
! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.
!        CALL Getlun('READS.OUT',fnumrea)
!        INQUIRE (FILE='READS.OUT',OPENED=fopen)
!        IF (.NOT.fopen) OPEN(UNIT=fnumrea,FILE='READS.OUT')
      ENDIF

      LENSTD=12

      CALL LTRIM2(CODE,codenew)

      CALL LTRIM2(SPDIRFLE,spdirflt)
      FILELEN=TVILENT(SPDIRFLT)
      FILELEN=MIN(93,FILELEN)
      SPDIRFLT=' '
      SPDIRFLT(1:FILELEN)=SPDIRFLE(1:FILELEN)

      IF(SPDIRFLE(1:FILELEN).EQ.SPDIRFLP(1:FILELEN))GO TO 5555

      OPEN(filenum,FILE=SPDIRFLE(1:FILELEN),STATUS='OLD')

      DO L=1,500
        DATARRAY=' '
      ENDDO
      DATANUM=0
      ABVNUM=0
      LINE=0
      TVI0=0
      TVI1=0

      DO

        tl2541=' '
        LINE=LINE+1
        DO
          READ(filenum,'(A254)',END=999,ERR=9999)tl2541
          IF(tl2541(1:1).EQ.'*')GOTO 888
          IF(tl2541(1:1).EQ.'$')GOTO 888
          IF(tl2541(1:1).EQ.'!')GOTO 888
          IF(tvilent(tl2541).LT.6)GOTO 888
          IF(tl2541(1:1).EQ.'@')THEN
            ! Strip out leading periods
            ABVLEN=TVILENT(TL2541)
            DO TVI1=1,ABVLEN-1
              IF(TL2541(TVI1:TVI1+1).EQ.' .')TL2541(TVI1:TVI1+1)='  '
              ! NEW JULY 05
              IF(TL2541(TVI1:TVI1).EQ.'!')TL2541(TVI1:ABVLEN)=' '
            ENDDO
            IF (ABVLINE(2:30).NE.TL2541(2:30))THEN
              TVI0=TVI0+ABVNUM
              ABVLINE=tl2541
              LINE=1
            ENDIF
            GO TO 888
          ENDIF
          EXIT
  888     CONTINUE
        END DO

        DATALINE=tl2541
        
        ABVSTD=' '
        DATASTD=' '
        TEXTSTD=' '
        COEFFC(TVI0+1)=' '
        
        ! No variables if length < 5. 
        ! For when sub-set present but 0 in treatments sub-set
        IF (TVILENT(DATALINE).LT.5) GO TO 999
        CALL STANDC(ABVLINE,DATALINE,DATASTD,TEXTSTD,'12')
        IF(ABVLINE(1:1).EQ.'@')ABVLINE(1:1)=' '
        CALL STANDARD(ABVLINE,ABVSTD,'12')
        ABVNUM=TVINSTR(ABVSTD)
        ! NB MAXIMUM NUMBER OF VARIABLES ALONG LINE=40
        IF(ABVNUM.GT.40)ABVNUM=40
        DO TVI1=1,ABVNUM
          TVI2=1 + (TVI1-1)*LENSTD
          TVI4=TVI2 + (LENSTD-1)
          COEFFC(TVI0+TVI1)=DATASTD(TVI2:TVI4)
          ABVC(TVI0+TVI1)=ABVSTD(TVI2:TVI4)
          COEFFCTM=COEFFC(TVI0+TVI1)
          CALL LTRIM(COEFFCTM)
          IF (TVILENT(COEFFCTM).LT.1)COEFFC(TVI0+TVI1)='-99         '
          CALL LTRIM(TEXTSTD)
          IF (TVILENT(TEXTSTD).LT.1)TEXTSTD(1:3)='-99'
          COEFFCHK=COEFFCTM(1:4)
          IF (LINE.EQ.1) THEN
           IF(COEFFCHK.EQ.'TEXT')THEN
            datarray(TVI0+tvi1)=abvc(TVI0+tvi1)//' '//TEXTSTD
           ELSE
            datarray(TVI0+tvi1)=abvc(TVI0+tvi1)//' '//coeffc(TVI0+tvi1)
           ENDIF
          ELSE
           LENTMP=TVILENT(DATARRAY(TVI0+TVI1))
           IF(COEFFCHK.EQ.'TEXT')THEN
            TL2541=datarray(tvi0+tvi1)(1:LENTMP)//' '//TEXTSTD
            datarray(TVI0+tvi1)=TL2541
           ELSE
            TL2541=datarray(tvi0+tvi1)(1:LENTMP)//' '//coeffc(TVI0+tvi1)
            datarray(TVI0+tvi1)=TL2541
           ENDIF
          ENDIF
        ENDDO

        datanum=tvi0+abvnum

      ENDDO

  999 CONTINUE

      CLOSE(FILENUM)

! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.
!      WRITE(fnumrea,*)' '
!      WRITE(fnumrea,*)'FILE ',SPDIRFLE(1:FILELEN)
!      DO TVI3=1,DATANUM
!        WRITE(fnumrea,'(I6,A2,A60)')tvi3,'  ',datarray(TVI3)(1:60)
!      ENDDO

      ! No abbreviation file. Headers must be standard!
      abvsyn=0

 5555 CONTINUE

      CALL ARAYREAD(CODE,DATARRAY,DATANUM,ABVARRAY,ABVSYN,TXTO)
      !WRITE(FNUMREA,*) 'CODE,TXTO ',CODE,TXTO

      SPDIRFLP=' '
      SPDIRFLP(1:FILELEN)=SPDIRFLT(1:FILELEN)

      RETURN

 9999 CONTINUE
      CLOSE (filenum)
      CALL Getlun('ERROR.OUT',fnumerr)
      OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
      WRITE (fnumerr,*) ' Problem reading: ',SPDIRFLE(1:60)
      WRITE (fnumerr,*) ' Check READS.OUT for details'
      WRITE (*,*) ' Problem reading: ',spdirfle(1:50)
      WRITE (*,*) ' Program will have to stop'
      WRITE (*,*) ' Check READS.OUT for details'
      CLOSE (fnumerr)
      STOP ' '

      END

!-----------------------------------------------------------------------

      SUBROUTINE SPREADC(SPDIRFLE,CODE,charout)

      ! Returns one character string for particular code

      IMPLICIT NONE
      EXTERNAL Ltrim, LTRIM2, SPREADT

      CHARACTER (LEN=*)   SPDIRFLE, CODE, CHAROUT
      CHARACTER (LEN=254) TL2541, TXTO
      CHARACTER (LEN=100) CODENEW
      CHARACTER (LEN=20)  COEFF
      CHARACTER (LEN=1)   FOUND
      INTEGER             TVI1, STARTPOS, ENDPOS
      INTEGER             LENGTH, TVI3, CODECLEN

      intrinsic           LEN

      SAVE

      CALL SPREADT(SPDIRFLE,CODE,TXTO)

      CALL LTRIM2(CODE,codenew)
      CODECLEN=LEN(CHAROUT)

      DO TVI1=1,CODECLEN
        CHAROUT(TVI1:TVI1)=' '
      ENDDO

      DO TVI1=1,20
        Coeff(TVI1:TVI1)=' '
      ENDDO

      FOUND='N'
      STARTPOS=1
      ENDPOS=0

      TL2541=TXTO
      CALL Ltrim(TL2541)

      DO TVI3=1, 100
        IF(TL2541(TVI3:TVI3).EQ.' ')THEN
          ENDPOS=TVI3
          FOUND='Y'
          GO TO 1000
        ENDIF
      ENDDO
 1000 CONTINUE
      IF(FOUND.EQ.'Y')THEN
        LENGTH=ENDPOS-STARTPOS+1
        COEFF(1:LENGTH)=TL2541(STARTPOS:ENDPOS)
        CHAROUT=' '
        IF (CODECLEN.LT.20)THEN
          CHAROUT=COEFF(1:CODECLEN)
        ELSE
          CHAROUT(1:20)=COEFF
        ENDIF
       ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE SPREADR(SPDIRFLE,CODE,valueout)

      ! Returns one real value for particular code

      IMPLICIT NONE
      EXTERNAL SPREADC, TVRFROMCCDE, TVRFROMC

      CHARACTER (LEN=*)  SPDIRFLE, CODE
      CHARACTER (LEN=30) CHAROUT
      REAL               VALUEOUT, TVRFROMC, TVRFROMCCDE

      SAVE

      CALL SPREADC(SPDIRFLE,CODE,charout)
      VALUEOUT=TVRFROMCCDE(Code,Charout(1:12))
      VALUEOUT=TVRFROMC(Charout(1:12))

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE SPREADCA(SPDIRFLE,CODE,AOUTSIZE,arrayout)

      ! Returns ARRAY for particular code

      IMPLICIT NONE
      EXTERNAL Standard, LTRIM2, SPREADT, TVRFROMCCDE, TVRFROMC

      CHARACTER (LEN=*)   SPDIRFLE, CODE, AOUTSIZE, ARRAYOUT(*)
      CHARACTER (LEN=354) TL3541
      CHARACTER (LEN=254) TL2541,TXTO
      CHARACTER (LEN=100) CODENEW
      REAL                TVR1, TVRFROMC, TVRFROMCCDE
      INTEGER             TVI1, L, L2, SIZE, LENARVAR

      INTRINSIC           NINT,LEN

      SAVE

      CALL SPREADT(SPDIRFLE,CODE,TXTO)

      CALL LTRIM2(CODE,codenew)

      TVR1=TVRFROMCCDE('AOUTSIZE',AOUTSIZE)
      TVR1=TVRFROMC(AOUTSIZE)
      SIZE=NINT(TVR1)

      DO TVI1=1,SIZE
        ARRAYOUT(TVI1)=' '
      ENDDO

      LENARVAR=LEN(arrayout(1))

      TL2541=TXTO
      CALL Standard(TL2541,TL3541,'13')
      DO L=1,SIZE
         L2=(L-1)*13 + 1
         IF (TL3541(L2:L2).EQ.'!')GO TO 1000
         IF (TL3541(L2:L2).EQ.' ')GO TO 1000
         ARRAYOUT(L)(1:LENARVAR)=TL3541(L2:L2+LENARVAR)
      END DO
 1000 CONTINUE

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE SPREADRA(SPDIRFLE,CODE,AOUTSIZE,arrayout)

      ! Returns ARRAY for particular code

      IMPLICIT NONE
      EXTERNAL Standard, LTRIM2, SPREADT, TVRFROMCCDE, TVRFROMC

      CHARACTER (LEN=*)   SPDIRFLE, CODE, AOUTSIZE
      CHARACTER (LEN=354) TL3541
      CHARACTER (LEN=254) TL2541,TXTO
      CHARACTER (LEN=100) CODENEW
      REAL                TVR1, TVRFROMC, ARRAYOUT(*), TVRFROMCCDE
      INTEGER             TVI1, L, L2, SIZE

      INTRINSIC           NINT

      SAVE

      CALL SPREADT(SPDIRFLE,CODE,TXTO)
      CALL LTRIM2(CODE,codenew)

      TVR1=TVRFROMCCDE('AOUTSIZE',AOUTSIZE)
      TVR1=TVRFROMC(AOUTSIZE)
      SIZE=NINT(TVR1)

      DO TVI1=1,SIZE
        ARRAYOUT(TVI1)=-99
      ENDDO

      TL2541=TXTO
      CALL Standard(TL2541,TL3541,'7')
      DO L=1,SIZE
         L2=(L-1)*7 + 1
         IF (TL3541(L2:L2).EQ.'!')GO TO 1000
         IF (TL3541(L2:L2).EQ.' ')GO TO 1000
         ARRAYOUT(L)=TVRFROMCCDE(TL3541,TL3541(L2:L2+5))
         ARRAYOUT(L)=TVRFROMC(TL3541(L2:L2+5))
      END DO
 1000 CONTINUE

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE SPREADIA(SPDIRFLE,CODE,AOUTSIZE,arrayout)

      ! Returns ARRAY for particular code

      IMPLICIT NONE
      EXTERNAL Standard, LTRIM2, SPREADT, TVIFROMC

      CHARACTER (LEN=*)   SPDIRFLE, CODE, AOUTSIZE
      CHARACTER (LEN=354) TL3541
      CHARACTER (LEN=254) TL2541,TXTO
      CHARACTER (LEN=100) CODENEW
      INTEGER             TVIFROMC, ARRAYOUT(*)
      INTEGER             TVI1, L, L2, SIZE

      SAVE

      CALL SPREADT(SPDIRFLE,CODE,TXTO)
      CALL LTRIM2(CODE,codenew)

      SIZE=TVIFROMC(AOUTSIZE)

      DO TVI1=1,SIZE
        ARRAYOUT(TVI1)=-99
      ENDDO

      TL2541=TXTO
      CALL Standard(TL2541,TL3541,'7')
      DO L=1,SIZE
         L2=(L-1)*7 + 1
         IF (TL3541(L2:L2).EQ.'!')GO TO 1000
         IF (TL3541(L2:L2).EQ.' ')GO TO 1000
         ARRAYOUT(L)=TVIFROMC(TL3541(L2:L2+5))
      END DO
 1000 CONTINUE

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE ARAYREAD(CODE,DATARRAY,DATANUM,ABVARRAY,ABVSYN,TXTO)

      IMPLICIT NONE
      EXTERNAL TVILENT, LTRIM, LTRIM2, Getstr

      CHARACTER (LEN=*)   CODE, DATARRAY(500),TXTO
      CHARACTER (LEN=254) TL2541, TL2542, ABVARRAY(1000)
      CHARACTER (LEN=100) CODENEW
      CHARACTER (LEN=12)  SYNCODE
      CHARACTER (LEN=1)   ABVFOUND, FOUND
      INTEGER             DATANUM, TVILENT, ABVSYN
      INTEGER             TVI1, CODENUM, LENTEXT, CODELEN  !, fnumrea
      INTEGER             LENTMP, LENTMP2, TVI2, TVI3, TVIABV
!     LOGICAL             FOPEN

      INTRINSIC           LEN

      SAVE

! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.
!      IF (fnumrea.LE.0.OR.fnumrea.GT.1000) THEN
!        CALL Getlun('READS.OUT',fnumrea)
!        INQUIRE (FILE='READS.OUT',OPENED=fopen)
!        IF (.NOT.fopen) OPEN(UNIT=fnumrea,FILE='READS.OUT')
!      ENDIF

      SYNCODE=' '
      CODENUM=1

 5556 CONTINUE

      IF(TVILENT(SYNCODE).LT.1)THEN
        CALL LTRIM2(CODE,codenew)
      ELSE
        CALL LTRIM2(SYNCODE,codenew)
      ENDIF
      CODELEN=Tvilent(CODENEW)

      FOUND='N'
      DO TVI1=1,DATANUM
        TL2541=' '
        TL2541=DATARRAY(TVI1)

        ! Remove trailing blanks
        IF(TL2541((CODELEN+1):(CODELEN+1)).EQ.'.')THEN
          DO TVI2=CODELEN+1,CODELEN+20
            IF(TL2541(TVI2:TVI2).EQ.' ')EXIT
            TL2541(TVI2:TVI2)=' '
          ENDDO
        ENDIF

        IF(CODENEW(1:CODELEN).EQ.TL2541(1:CODELEN))THEN
          IF(TL2541((CODELEN+1):(CODELEN+1)).EQ.' ')THEN
            DO TVI2=CODELEN,100
              IF(TL2541(TVI2:TVI2).EQ.' ')EXIT
            ENDDO
            DO TVI3=TVI2, 100
              IF (TL2541(TVI3:TVI3).NE.' ')EXIT
            ENDDO
            LENTMP=TVILENT(TL2541)
            LENTMP2=LENTMP-TVI3+1
            TL2542(1:LENTMP2)=TL2541(TVI3:LENTMP)
            IF(LENTMP2.GT.0)FOUND='Y'
            GO TO 1000
          ENDIF
        ENDIF
      ENDDO
 1000 CONTINUE
      IF(FOUND.EQ.'Y')THEN
        LENTEXT=LEN(TXTO)
        IF (LENTEXT.LE.LENTMP2) THEN
          TXTO=TL2542(1:LENTEXT)
        ELSE
          TXTO=' '
          TXTO(1:LENTMP2)=TL2542(1:LENTMP2)
        ENDIF
      ELSE
        IF(ABVSYN.GT.0)THEN
          IF(CODENUM.LE.1)THEN
            ABVFOUND='N'
            DO TVIABV=1,ABVSYN
              CALL LTRIM(ABVARRAY(TVIABV))
              IF(CODENEW(1:CODELEN).EQ.ABVARRAY(TVIABV)(1:CODELEN))THEN
                ABVFOUND='Y'
                GO TO 8000
              ENDIF
            ENDDO
          ENDIF
 8000     CONTINUE
          IF(ABVFOUND.EQ.'Y')THEN
            CODENUM=CODENUM+1
            CALL Getstr(ABVARRAY(TVIABV),CODENUM,SYNCODE)
            IF(TVILENT(SYNCODE).GT.0.AND.SYNCODE(1:3).NE.'-99')THEN
! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.              
!              WRITE(fnumrea,*)' Abbreviations  ',abvarray(tviabv)(1:55)
!              WRITE(fnumrea,*)' Trying synonym ',syncode
              GO TO 5556
            ELSE
              ! WRITE(fnumrea,*)' Nothing found for ',Code
            ENDIF
          ENDIF
        ENDIF

!        WRITE(fnumrea,*)' Nothing found for ',Code
        TXTO=' '
        TXTO(1:3)='-99'

      ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE AMAKEABV(ABVFILE,abvarray,abvsyn)

      IMPLICIT NONE
      EXTERNAL Getlun, TVILENT


      CHARACTER (LEN=254) TL2541, ABVARRAY(1000)
      CHARACTER (LEN=132) ABVFILE
      INTEGER             TVILENT, ABVSYN, ABVLEN, FILENUM
      INTEGER             FILELEN, TVI1, FNUMERR

      SAVE

      CALL GETLUN('FILETMP',FILENUM)

      FILELEN=TVILENT(abvfile)
      OPEN(filenum,FILE=abvfile(1:FILELEN),STATUS='OLD')
      abvsyn=0
      DO
        tl2541=' '
        READ(filenum,'(A254)',END=7776,ERR=7777)tl2541
        IF(TVILENT(tl2541).LT.3)GOTO 777
        IF(tl2541(1:1).EQ.'!')GOTO 777
        IF(tl2541(1:1).EQ.'@')GOTO 777
        abvlen=tvilent(tl2541)
        IF(abvlen.GT.81)THEN
          abvsyn=abvsyn+1
          IF (abvsyn .EQ. 1000) THEN
            CALL Getlun('ERROR.OUT',fnumerr)
            OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
            WRITE (fnumerr,*) ' Abbreviation number is at the limit'
            WRITE (fnumerr,*) ' If abbreviation # cannot be reduced,'
            WRITE (fnumerr,*) ' the program will have to be changed'
            WRITE (fnumerr,*) ' Check READS.OUT for details'
            WRITE (*,*) ' Abbreviation number is at the limit'
            WRITE (*,*) ' If abbreviation # cannot be reduced,'
            WRITE (*,*) ' the program will have to be changed'
            WRITE (*,*) ' Program will have to stop'
            WRITE (*,*) ' Check READS.OUT for details'
            CLOSE (fnumerr)
            STOP ' '
          ENDIF

          DO tvi1=1,9
            IF(tl2541(tvi1:tvi1).EQ.' ')EXIT
          ENDDO
          abvarray(abvsyn)=tl2541(1:tvi1)//'  '//tl2541(81:abvlen)
        ENDIF
 777    CONTINUE
      ENDDO
 7776 CONTINUE
 7777 CONTINUE

      CLOSE(filenum)

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE AMAKE1(ADIRFILE,LEVEL,DATARRAY,datanum)

      IMPLICIT NONE
      EXTERNAL Getlun, TVILENT, LTRIM, Tl1upcase
      EXTERNAL Standard, TVINSTR, STANDC

      CHARACTER (LEN=1000) DATASTD, ABVSTD
      CHARACTER (LEN=254)  TL2541, ABVLINE, DATALINE, DATARRAY(500)
      CHARACTER (LEN=52)   TEXTSTD
      CHARACTER (LEN=30)   TL30, LEVELTMP
      CHARACTER (LEN=24)   COEFFC, ABVC, COEFFCTM
      CHARACTER (LEN=10)   LEVEL
      CHARACTER (LEN=4)    COEFFCHK
      CHARACTER (LEN=1)    TL1UPCASE
      CHARACTER (LEN=*)    ADIRFILE
      INTEGER              FILENUM, TVILENT, TVI1, TVI2, TVI4
      INTEGER              STARTCOL, LEVELLEN, FILELEN, LENTMP
      INTEGER              TVINSTR, ABVNUM, DATANUM, LENSTD, ABVLEN, DN
      INTEGER              DATANUMS, LINE, FNUMERR  !, FNUMREA
!     LOGICAL              FOPEN

      SAVE

      CALL GETLUN('FILETMP',FILENUM)

! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.
!      CALL Getlun('READS.OUT',fnumrea)
!      INQUIRE (FILE='READS.OUT',OPENED=fopen)
!      IF (.NOT.fopen) OPEN(UNIT=fnumrea,FILE='READS.OUT')

      ! File
      FILELEN=TVILENT(ADIRFILE)

      ! Level
      LENSTD=24
      LEVELLEN=TVILENT(LEVEL)
      IF(LEVELLEN.GT.LENSTD)LEVELLEN=LENSTD
      OPEN(filenum,FILE=ADIRFILE,STATUS='OLD')

! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.
!      WRITE(fnumrea,*)' '
!      WRITE(fnumrea,*)'FILE ',ADIRFILE(1:FILELEN)
!      WRITE(fnumrea,*)' LEVEL ',LEVEL(1:LEVELLEN)

      DATANUM=0
      STARTCOL=1
      ABVLINE=' '

        DO
          tl2541=' '
          READ(filenum,'(A254)',END=9996,ERR=9999)tl2541

          IF(TVILENT(tl2541).LT.3)GOTO 999
          IF(tl2541(1:1).EQ.'!')GOTO 999
          IF(tl2541(1:1).EQ.'$')GOTO 999

          tl30=' '
          TL30=TL2541(1:30)
          CALL Ltrim(tl30)
          tl30(1:1)=Tl1upcase(tl30(1:1))
          tl30(2:2)=Tl1upcase(tl30(2:2))
          IF(TL30(2:2).EQ.' ')TL30(3:3)=' '

          IF(tl2541(1:1).EQ.'*')THEN
            IF(tl2541(2:4).NE.'COE')THEN
              IF(tl2541(2:5).NE.'GENE')startcol=2
            ENDIF
            GO TO 999
          ENDIF

          IF(tl2541(1:1).EQ.'@')THEN
            ! Strip off leading @
            tl2541(1:1)=' '
            ! Strip out leading periods
            ABVLEN=TVILENT(TL2541)
            DO TVI1=1,ABVLEN-1
              IF(TL2541(TVI1:TVI1+1).EQ.' .')TL2541(TVI1:TVI1+1)='  '
              ! NEW JULY 05
              IF(TL2541(TVI1:TVI1).EQ.'!')TL2541(TVI1:ABVLEN)=' '
            ENDDO
            IF (ABVLINE(2:40).NE.TL2541(2:40))THEN
              ABVLINE(1:1)=' '
              ABVLINE=tl2541
              ABVSTD=' '
              CALL STANDARD(ABVLINE,ABVSTD,'24')
              ABVNUM=TVINSTR(ABVSTD)
              ! NB MAXIMUM NUMBER OF VARIABLES ALONG LINE=40
              IF(ABVNUM.GT.40)ABVNUM=40
              LINE=1
              GO TO 999
            ENDIF
          ENDIF

          DATALINE=tl2541

          IF(ABVNUM.EQ.1)THEN
            IF(line.EQ.1)THEN
              datanum=datanum+1
              CALL LTRIM(abvline)
              CALL LTRIM(dataline)
              DATARRAY(datanum)=abvline(1:6)//' '//dataline(1:247)
              LINE=LINE+1
            ELSE
              CALL LTRIM(dataline)
              tvi1=TVILENT(DATARRAY(datanum))
              tvi2=TVILENT(dataline)
              IF(tvi1+tvi2.GT.241)tvi2=254-tvi1
              dn=datanum
              DATARRAY(dn)=DATARRAY(dn)(1:tvi1)//' '//dataline(1:tvi2)
            ENDIF
            GO TO 999
          ENDIF

          DATASTD=' '
          TEXTSTD=' '
          IF (TVILENT(DATALINE).LT.5) GO TO 999
          CALL STANDC(ABVLINE,DATALINE,DATASTD,TEXTSTD,'24')

          leveltmp=' '
          READ(DATASTD,'(a24)')leveltmp(1:24)

          IF(LEVELTMP(1:LEVELLEN).NE.LEVEL(1:LEVELLEN))GO TO 999
          IF(LEVELTMP(LEVELLEN+1:LEVELLEN+1).NE.' ')GO TO 999

          DO TVI1=1,ABVNUM-(STARTCOL-1)
            IF(TVI1.EQ.1)THEN
              IF(LINE.EQ.1)THEN
                datanums=datanum
              ELSE
                datanum=datanums
              ENDIF
            ENDIF
            datanum=datanum+1
            TVI2=1 + ((STARTCOL-1)*LENSTD) + (TVI1-1)*LENSTD
            TVI4=TVI2 + (LENSTD-1)
            COEFFC=DATASTD(TVI2:TVI4)
            ABVC=ABVSTD(TVI2:TVI4)
            COEFFCTM=COEFFC
            CALL LTRIM(COEFFCTM)
            IF (TVILENT(COEFFCTM).LT.1)COEFFC='-99         '
            CALL LTRIM(TEXTSTD)
            IF (TVILENT(TEXTSTD).LT.1)TEXTSTD(1:3)='-99'
            COEFFCHK=COEFFCTM(1:4)

            IF (LINE.EQ.1) THEN
              IF(COEFFCHK.EQ.'TEXT')THEN
                DATARRAY(datanum)=abvc//' '//TEXTSTD
              ELSE
                DATARRAY(datanum)=abvc//' '//coeffc
              ENDIF
              IF(TVI1.EQ.(ABVNUM-(STARTCOL-1)))LINE=LINE+1
            ELSE
              LENTMP=TVILENT(DATARRAY(datanum))
              IF(COEFFCHK.EQ.'TEXT')THEN
                TL2541=DATARRAY(datanum)(1:LENTMP)//' '//TEXTSTD
                DATARRAY(datanum)=TL2541
              ELSE
                TL2541=DATARRAY(datanum)(1:LENTMP)//' '//coeffc
                DATARRAY(datanum)=TL2541
              ENDIF
            ENDIF
  
          ENDDO

  999     CONTINUE

        END DO

 9996 CONTINUE

 9999 CONTINUE

      CLOSE(filenum)

! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.
!      DO TVI1=1,DATANUM
!        WRITE(fnumrea,'(I6,A2,A60)')tvi1,'  ',DATARRAY(TVI1)(1:60)
!      ENDDO

      IF (DATANUM .LE. 0)THEN
        CALL Getlun('ERROR.OUT',fnumerr)
        OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
        WRITE(fnumerr,*)' Problem in generic reads subroutine AMAKE1'
        WRITE(fnumerr,*)' Most likely the level indicator was not found'
        WRITE(fnumerr,*)' Either the indicator was not present'
        WRITE(fnumerr,*)' Or the indicator or code was wrong'
        WRITE(fnumerr,*)' The file being used was:      '
        WRITE(fnumerr,*)' ',ADIRFILE(1:FILELEN)
        WRITE(fnumerr,*)' AND the level indicator was  ',LEVEL
        WRITE(fnumerr,*)' Program had to stop'
        WRITE(fnumerr,*)' Check READS.OUT for details'
        WRITE(*,*)' Problem in generic reads subroutine AMAKE1!'
        WRITE(*,*)' Most likely the level indicator was not found'
        WRITE(*,*)' Either the indicator was not present'
        WRITE(*,*)' Or the indicator or code was wrong'
        WRITE(*,*)' The file being used was:      '
        WRITE(*,*)' ',ADIRFILE(1:FILELEN)
        WRITE(*,*)' AND the level indicator was  ',LEVEL
        WRITE(*,*)' Program will have to stop'
        WRITE(*,*)' Check READS.OUT for details'
        CLOSE (fnumerr)
        STOP ' '
      ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE AMAKER0(SPDIRFLT,datarray,datanum)

      IMPLICIT NONE
      EXTERNAL Getlun, TVILENT, LTRIM


      CHARACTER (LEN=254) DATARRAY(500), TL2541
      CHARACTER (LEN=93)  SPDIRFLT
      INTEGER             DATANUM, FILENUM, TVI1, SIZE, FNUMERR
      INTEGER             SPLEN, TVILENT, L  !, fnumrea
      LOGICAL             FFLAG  !, FOPEN

      !INTRINSIC EOF  !chp portability to G95

      SAVE

      SIZE=500

      CALL GETLUN('FILETMP',FILENUM)

! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.
!      CALL Getlun('READS.OUT',fnumrea)
!      INQUIRE (FILE='READS.OUT',OPENED=fopen)
!      IF (.NOT.fopen) OPEN(UNIT=fnumrea,FILE='READS.OUT')
!      WRITE(fnumrea,*)' '
!      WRITE(fnumrea,*)'FILE ',SPDIRFLT(1:50)

      OPEN(filenum,FILE=spdirflt,STATUS='OLD')


      TVI1=0
      FFLAG=.FALSE.
      DO WHILE(.TRUE.)  !(.NOT.fflag) chp for portability
        READ(filenum,'(A254)',END=9996,ERR=9999)tl2541
        IF (tl2541(1:1).NE.'$') THEN
          IF (tl2541(1:1).NE.'*') THEN
            IF (tl2541(1:1).NE.'@') THEN
              IF (tl2541(1:1).NE.'!') THEN
                IF (tl2541(1:6).NE.'      ') THEN
                  TVI1=TVI1+1
                  IF(TVI1.GE.SIZE)THEN
                    CALL Getlun('ERROR.OUT',fnumerr)
                    OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
                    WRITE(fnumerr,*)' Too many coeffs in ',spdirflt
                    WRITE(fnumerr,*)' Size permitted is ',size
                    WRITE(fnumerr,*)' Check READS.OUT for details'
                    WRITE(*,*)' Too many coeffs in ',spdirflt(1:40)
                    WRITE(*,*)' Size permitted is ',size
                    WRITE(*,*)' Program will have to stop'
                    WRITE(*,*)' Check READS.OUT for details'
                    CLOSE (fnumerr)
                    STOP ' '
                  ENDIF
                  CALL LTRIM(TL2541)
                  SPLEN=TVILENT(TL2541)
                  DO L=1,SPLEN
                    IF (TL2541(L:L).EQ.'!') THEN
                      TL2541(L:SPLEN)=' '
                      EXIT
                    ENDIF
                  ENDDO
                  datarray(tvi1)=tl2541
! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.                  
!                  WRITE(fnumrea,'(I6,A2,A60)')TVI1,'  ',TL2541(1:60)
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        !IF (EOF(FILENUM))fflag=.TRUE.  !chp should catch EOF next read
      ENDDO

 9996 CONTINUE

 9999 CONTINUE

      CLOSE(FILENUM)

      DATARRAY(TVI1+1)='-9999'
      datanum=tvi1

      IF (DATARRAY(1) .EQ. '      ')THEN
        CALL Getlun('ERROR.OUT',fnumerr)
        OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
        WRITE (fnumerr,*) ' Problem reading: ',spdirflt(1:60)
        WRITE (fnumerr,*) ' Datanum: ',datanum
        WRITE (fnumerr,*) ' Datarray:'
        DO TVI1=1, DATANUM
          WRITE(fnumerr,*)datarray(tvi1)
        ENDDO
        WRITE (fnumerr,*) ' Check READS.OUT for details'
        CLOSE (fnumerr)
        WRITE (*,*) ' Problem reading: ',spdirflt(1:60)
        WRITE (*,*) ' Datanum: ',datanum
        WRITE (*,*) ' Program will have to stop'
        WRITE (*,*) ' Check READS.OUT for details'
        STOP ' '
      ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE AMAKE5(ADIRFILE,LEVEL,RN,SN,ON,CN,DATARRAY,datanum)

      IMPLICIT NONE
      EXTERNAL Getlun, TVILENT, LTRIM, Tl1upcase, Tvicolnm
      EXTERNAL Standard, TVINSTR, STANDC, GETSTR, TVIFROMC

      CHARACTER (LEN=1000) DATASTD, ABVSTD
      CHARACTER (LEN=254)  TL2541, ABVLINE, DATALINE, DATARRAY(500)
      CHARACTER (LEN=52)   TEXTSTD
      CHARACTER (LEN=30)   TL30, LEVELTMP
      CHARACTER (LEN=12)   COEFFC, ABVC, COEFFCTM, VALUE
      CHARACTER (LEN=10)   LEVEL
      CHARACTER (LEN=4)    COEFFCHK
      CHARACTER (LEN=1)    TL1UPCASE
      CHARACTER (LEN=*)    ADIRFILE
      INTEGER              FILENUM, TVILENT, TVI1, TVI2, TVI4
      INTEGER              STARTCOL, LEVELLEN, FILELEN, LENTMP
      INTEGER              TVINSTR, ABVNUM, DATANUM, LENSTD, ABVLEN, DN
      INTEGER              DATANUMS, LINE, TVICOLNM, TVIFROMC  !,FNUMREA
      INTEGER              RNCOL, SNCOL, ONCOL, CNCOL, RN, SN, ON, CN
      INTEGER              RNVAL, SNVAL, ONVAL,CNVAL
!     LOGICAL              FOPEN

      SAVE

      CALL GETLUN('FILETMP',FILENUM)

! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.
!      CALL Getlun('READS.OUT',fnumrea)
!      INQUIRE (FILE='READS.OUT',OPENED=fopen)
!      IF (.NOT.fopen) OPEN(UNIT=fnumrea,FILE='READS.OUT')

      ! Currently SN, ON, AND CN are not currently used .. but will be!

      TVI1=0

      ! File
      FILELEN=TVILENT(ADIRFILE)

      ! Level
      LENSTD=12
      LEVELLEN=TVILENT(LEVEL)
      IF(LEVELLEN.GT.LENSTD)LEVELLEN=LENSTD
      OPEN(filenum,FILE=ADIRFILE,STATUS='OLD')

! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.
!      WRITE(fnumrea,*)' '
!      WRITE(fnumrea,*)'FILE   ',ADIRFILE(1:FILELEN)
!      WRITE(fnumrea,*)' TRT   ',LEVEL(1:LEVELLEN)
!      WRITE(fnumrea,*)'  RP   ',RN

      DATANUM=0
      STARTCOL=1
      ABVLINE=' '

        DO
          tl2541=' '
          READ(filenum,'(A254)',END=9996,ERR=9999)tl2541

          IF(TVILENT(tl2541).LT.3)GOTO 999
          IF(tl2541(1:1).EQ.'!')GOTO 999
          IF(tl2541(1:1).EQ.'$')GOTO 999

          tl30=' '
          TL30=TL2541(1:30)
          CALL Ltrim(tl30)
          tl30(1:1)=Tl1upcase(tl30(1:1))
          tl30(2:2)=Tl1upcase(tl30(2:2))
          IF(TL30(2:2).EQ.' ')TL30(3:3)=' '

          IF(tl2541(1:1).EQ.'*')THEN
            IF(tl2541(2:5).NE.'GENE')startcol=2
            GO TO 999
          ENDIF

          IF(tl2541(1:1).EQ.'@')THEN
            ! Strip off leading @
            tl2541(1:1)=' '
            ! Strip out leading periods
            ABVLEN=TVILENT(TL2541)
            DO TVI1=1,ABVLEN-1
              IF(TL2541(TVI1:TVI1+1).EQ.' .')TL2541(TVI1:TVI1+1)='  '
              ! NEW JULY 05
              IF(TL2541(TVI1:TVI1).EQ.'!')TL2541(TVI1:ABVLEN)=' '
            ENDDO
            IF (ABVLINE(2:30).NE.TL2541(2:30))THEN
              ABVLINE(1:1)=' '
              ABVLINE=tl2541
              ABVSTD=' '
              CALL STANDARD(ABVLINE,ABVSTD,'12')
              ABVNUM=TVINSTR(ABVSTD)
              ! NB MAXIMUM NUMBER OF VARIABLES ALONG LINE=40
              IF(ABVNUM.GT.40)ABVNUM=40
              LINE=1
              GO TO 999
            ENDIF
          ENDIF

          DATALINE=tl2541

          IF(ABVNUM.EQ.1)THEN
            IF(line.EQ.1)THEN
              datanum=datanum+1
              CALL LTRIM(abvline)
              CALL LTRIM(dataline)
              DATARRAY(datanum)=abvline(1:6)//' '//dataline(1:247)
              LINE=LINE+1
            ELSE
              CALL LTRIM(dataline)
              tvi1=TVILENT(DATARRAY(datanum))
              tvi2=TVILENT(dataline)
              IF(tvi1+tvi2.GT.241)tvi2=254-tvi1
              dn=datanum
              DATARRAY(dn)=DATARRAY(dn)(1:tvi1)//' '//dataline(1:tvi2)
            ENDIF
            GO TO 999
          ENDIF

          DATASTD=' '
          TEXTSTD=' '
          IF (TVILENT(DATALINE).LT.5) GO TO 999
          CALL STANDC(ABVLINE,DATALINE,DATASTD,TEXTSTD,'12')

          leveltmp=' '
          READ(DATASTD,'(a12)')leveltmp(1:12)

          IF(LEVELTMP(1:LEVELLEN).NE.LEVEL(1:LEVELLEN))GO TO 999
          IF(LEVELTMP(LEVELLEN+1:LEVELLEN+1).NE.' ')GO TO 999

          RNCOL=Tvicolnm(ABVLINE,'RN ')
          IF (RNCOL.LE.0) RNCOL=Tvicolnm(ABVLINE,'RP ')
          SNCOL=Tvicolnm(ABVLINE,'SN ')
          ONCOL=Tvicolnm(ABVLINE,'ON ')
          CNCOL=Tvicolnm(ABVLINE,'CN ')
          IF (RNCOL.GT.0)THEN
            !WRITE(fnumrea,*)' RN column ',rncol
            CALL Getstr(datastd,rncol,value)
            RNVAL=Tvifromc(value)
            !WRITE(fnumrea,*)' RN valu   ',rnval
            IF (RNVAL.NE.RN) GO TO 999
          ENDIF
          IF (SNCOL.GT.0)THEN
            CALL Getstr(datastd,sncol,value)
            SNVAL=Tvifromc(value)
            IF (SNVAL.NE.SN) GO TO 999
          ENDIF
          IF (ONCOL.GT.0)THEN
            CALL Getstr(datastd,oncol,value)
            ONVAL=Tvifromc(value)
            IF (ONVAL.NE.ON) GO TO 999
          ENDIF
          IF (CNCOL.GT.0)THEN
            CALL Getstr(datastd,cncol,value)
            CNVAL=Tvifromc(value)
            IF (CNVAL.NE.CN) GO TO 999
          ENDIF

          DO TVI1=1,ABVNUM-(STARTCOL-1)
            IF(TVI1.EQ.1)THEN
              IF(LINE.EQ.1)THEN
                datanums=datanum
              ELSE
                datanum=datanums
              ENDIF
            ENDIF
            datanum=datanum+1
            TVI2=1 + ((STARTCOL-1)*LENSTD) + (TVI1-1)*LENSTD
            TVI4=TVI2 + (LENSTD-1)
            COEFFC=DATASTD(TVI2:TVI4)
            ABVC=ABVSTD(TVI2:TVI4)
            COEFFCTM=COEFFC
            CALL LTRIM(COEFFCTM)
            IF (TVILENT(COEFFCTM).LT.1)COEFFC='-99         '
            CALL LTRIM(TEXTSTD)
            IF (TVILENT(TEXTSTD).LT.1)TEXTSTD(1:3)='-99'
            COEFFCHK=COEFFCTM(1:4)

            IF (LINE.EQ.1) THEN
              IF(COEFFCHK.EQ.'TEXT')THEN
                DATARRAY(datanum)=abvc//' '//TEXTSTD
              ELSE
                DATARRAY(datanum)=abvc//' '//coeffc
              ENDIF
              IF(TVI1.EQ.(ABVNUM-(STARTCOL-1)))LINE=LINE+1
            ELSE
              LENTMP=TVILENT(DATARRAY(datanum))
              IF(COEFFCHK.EQ.'TEXT')THEN
                TL2541=DATARRAY(datanum)(1:LENTMP)//' '//TEXTSTD
                DATARRAY(datanum)=TL2541
              ELSE
                TL2541=DATARRAY(datanum)(1:LENTMP)//' '//coeffc
                DATARRAY(datanum)=TL2541
              ENDIF
            ENDIF

          ENDDO

  999     CONTINUE

        END DO

 9996 CONTINUE

 9999 CONTINUE

      CLOSE(filenum)

! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.
!      DO TVI1=1,DATANUM
!        WRITE(fnumrea,'(I6,A2,A60)')tvi1,'  ',DATARRAY(TVI1)(1:60)
!      ENDDO
!      IF (DATANUM .LE. 0)THEN
!        WRITE(fnumrea,*)' Problem reading: ',ADIRFILE(1:60)
!        WRITE (fnumrea,*) ' Nothing in the array of input data!'
!      ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE SLREADT(SLDIRFLE,SLCODE,CODE,TXTO)

      USE OSDefinitions

      IMPLICIT NONE
      EXTERNAL Getlun, TVILENT, LTRIM, Tl1upcase
      EXTERNAL Standard, STANDC, TVINSTR, AMAKEABV, ARAYREAD

      CHARACTER (LEN=1000) DATASTD, ABVSTD
      CHARACTER (LEN=254)  DATARRAY(500), TL2541, ABVLINE, DATALINE
      CHARACTER (LEN=254)  ABVARRAY(1000)
      CHARACTER (LEN=132)  ABVFILE
      CHARACTER (LEN=128)  ARG
      CHARACTER (LEN=120)  CFGDFILE
      CHARACTER (LEN=80)   TL0801
      CHARACTER (LEN=52)   TEXTSTD
      CHARACTER (LEN=12)   COEFFC(200), ABVC(200), COEFFCTM
      CHARACTER (LEN=11)   GROUP
      CHARACTER (LEN=10)   SLCODEP
      CHARACTER (LEN=4)    COEFFCHK
      CHARACTER (LEN=1)    TL1UPCASE, FNUMFLAG
      CHARACTER (LEN=*)    SLDIRFLE, SLCODE, CODE, TXTO
      INTEGER              TVILENT, TVI1, TVI2, TVI0, TVI3, TVI4
      INTEGER              FILENUM, GROUPLEN, FILELEN, LINE, LENTMP
      INTEGER              TVINSTR, ABVNUM, LENSTD, ABVLEN, DATANUM
      INTEGER              ARGLEN, ABVSYN, L, FNUMERR  !, fnumrea
      LOGICAL              FFLAG  !,FOPEN

      INTRINSIC            LEN,TRIM

      SAVE

      IF (FNUMFLAG.NE.'Y')THEN
        CALL GETLUN('FILETMP',FILENUM)
        FNUMFLAG = 'Y'
! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.        
!        CALL Getlun('READS.OUT',fnumrea)
!        INQUIRE (FILE='READS.OUT',OPENED=fopen)
!        IF (.NOT.fopen) OPEN(UNIT=fnumrea,FILE='READS.OUT')
      ENDIF

      arg=' '
      tvi2=0
      tvi3=0
!     CALL GETARG (0,arg,arglen)
!portability
      CALL GETARG (0,arg)
      arglen = len_trim(arg)

      IF (arglen.GT.100) ARGLEN=TVILENT(ARG)
      DO tvi1=1,arglen
        IF (arg(tvi1:tvi1).EQ.SLASH) tvi2=tvi1
        IF (arg(tvi1:tvi1).EQ.'.') tvi3=tvi1-1
      ENDDO
      IF (tvi3.EQ.0) tvi3=arglen
      cfgdfile=ARG(1:TVI3)//'.CFG'
      cfgdfile=TRIM(cfgdfile)
      ! Change cfg file name from module specific to general
      DO L=LEN(CFGDFILE),1,-1
        IF (CFGDFILE(L:L).EQ.SLASH) EXIT
      ENDDO
      IF (L.GT.1) THEN
        cfgdfile=CFGDFILE(1:L-1)//SLASH//'CROPSIM.CFG'
      ELSE
        cfgdfile(1:12)='CROPSIM.CFG '
      ENDIF

      abvfile=ARG(1:TVI2)//'SOIL.CDE'

      LENSTD=12

      IF (LEN(TRIM(SLCODE)).LT.3 .OR. SLCODE(1:3).EQ.'   ') THEN
        CALL Getlun('ERROR.OUT',fnumerr)
        OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
        WRITE (fnumerr,*) ' No soil code in soil reads!! '
        WRITE (fnumerr,*) ' Check READS.OUT for details'
        WRITE (*,*) ' No soil code in soil reads!! '
        WRITE (*,*) ' Program will have to stop'
        WRITE (*,*) ' Check READS.OUT for details'
        CLOSE (fnumerr)
        STOP ' '
      ENDIF

      IF(SLCODE(1:10).EQ.SLCODEP(1:10))GO TO 5555
      
! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.      
!      WRITE(fnumrea,*)' '
!      WRITE(fnumrea,*)' SOIL FILENAME ',SLDIRFLE(1:60)
!      WRITE(fnumrea,*)' SOIL CODE     ',SLCODE

      FILELEN=TVILENT(SLDIRFLE)

      GROUP(1:1)='*'
      GROUP(2:11)=SLCODE(1:10)
      GROUPLEN=TVILENT(GROUP)

      OPEN(filenum,FILE=SLDIRFLE(1:FILELEN),STATUS='OLD')

      tl0801=' '
      DO WHILE(tl0801(1:GROUPLEN).NE.GROUP(1:GROUPLEN))
        READ(filenum,'(A80)',END=9998,ERR=9999)tl0801
        IF (tl0801(2:6).EQ.'SOIL:')THEN
          tl0801(1:5)='     '
          tl0801(6:6)='*'
        ENDIF
        IF (tl0801(2:14).EQ.'SOIL_PROFILE:')THEN
          tl0801(1:13)='             '
          tl0801(14:14)='*'
        ENDIF
        CALL Ltrim(tl0801)
        tl0801(1:1)=Tl1upcase(tl0801(1:1))
        tl0801(2:2)=Tl1upcase(tl0801(2:2))
        IF(tl0801(1:GROUPLEN).EQ.GROUP(1:GROUPLEN))EXIT
      END DO

! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.
!      WRITE(fnumrea,*)' '
!      WRITE(fnumrea,*)'FILE ',SLDIRFLE(1:FILELEN)
!      WRITE(fnumrea,*)' GROUP ',GROUP(1:GROUPLEN)

      DO L=1,500
        DATARRAY=' '
      ENDDO
      DATANUM=0
      ABVNUM=0
      LINE=0
      TVI0=0
      TVI1=0

      DO

        tl2541=' '
        LINE=LINE+1
        DO
          READ(filenum,'(A254)',END=999,ERR=9999)tl2541
          IF(tl2541(1:1).EQ.'*')GOTO 999
          IF(tl2541(1:1).EQ.'!')GOTO 888
          IF(tvilent(tl2541).LT.3)GOTO 888
          IF(tl2541(1:1).EQ.'@')THEN
            ! Strip out leading periods
            ABVLEN=TVILENT(TL2541)
            DO TVI1=1,ABVLEN-1
              IF(TL2541(TVI1:TVI1+1).EQ.' .')TL2541(TVI1:TVI1+1)='  '
              ! NEW JULY 05
              IF(TL2541(TVI1:TVI1).EQ.'!')TL2541(TVI1:ABVLEN)=' '
            ENDDO
            IF (ABVLINE(2:30).NE.TL2541(2:30))THEN
              TVI0=TVI0+ABVNUM
              ABVLINE=tl2541
              LINE=1
            ENDIF
            GO TO 888
          ENDIF
          EXIT
  888     CONTINUE
        END DO

        DATALINE=tl2541

        ABVSTD=' '
        DATASTD=' '
        TEXTSTD=' '
        COEFFC(TVI0+1)=' '

        IF (TVILENT(DATALINE).LT.5) GO TO 999
        CALL STANDC(ABVLINE,DATALINE,DATASTD,TEXTSTD,'12')
        IF(ABVLINE(1:1).EQ.'@')ABVLINE(1:1)=' '
        CALL STANDARD(ABVLINE,ABVSTD,'12')
        ABVNUM=TVINSTR(ABVSTD)
        ! NB MAXIMUM NUMBER OF VARIABLES ALONG LINE=40
        IF(ABVNUM.GT.40)ABVNUM=40
        DO TVI1=1,ABVNUM
          TVI2=1 + (TVI1-1)*LENSTD
          TVI4=TVI2 + (LENSTD-1)
          COEFFC(TVI0+TVI1)=DATASTD(TVI2:TVI4)
          ABVC(TVI0+TVI1)=ABVSTD(TVI2:TVI4)
          COEFFCTM=COEFFC(TVI0+TVI1)
          CALL LTRIM(COEFFCTM)
          IF (TVILENT(COEFFCTM).LT.1)COEFFC(TVI0+TVI1)='-99         '
          CALL LTRIM(TEXTSTD)
          IF (TVILENT(TEXTSTD).LT.1)TEXTSTD(1:3)='-99'
          COEFFCHK=COEFFCTM(1:4)
          IF (LINE.EQ.1) THEN
           IF(COEFFCHK.EQ.'TEXT')THEN
            datarray(TVI0+tvi1)=abvc(TVI0+tvi1)//' '//TEXTSTD
           ELSE
            datarray(TVI0+tvi1)=abvc(TVI0+tvi1)//' '//coeffc(TVI0+tvi1)
           ENDIF
          ELSE
           LENTMP=TVILENT(DATARRAY(TVI0+TVI1))
           IF(COEFFCHK.EQ.'TEXT')THEN
            TL2541=datarray(tvi0+tvi1)(1:LENTMP)//' '//TEXTSTD
            datarray(TVI0+tvi1)=TL2541
           ELSE
            TL2541=datarray(tvi0+tvi1)(1:LENTMP)//' '//coeffc(TVI0+tvi1)
            datarray(TVI0+tvi1)=TL2541
           ENDIF
          ENDIF
        ENDDO

        datanum=tvi0+abvnum

      ENDDO

  999 CONTINUE

      CLOSE(FILENUM)

! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.
!      DO TVI3=1,datanum
!        WRITE(fnumrea,'(I6,A2,A60)')tvi3,'  ',datarray(TVI3)(1:60)
!      enddo

      SLCODEP(1:10)=SLCODE(1:10)

      abvfile=' '
      abvfile(1:8)='SOIL.CDE'
      INQUIRE(FILE=abvfile,EXIST=fflag)
      IF(.NOT.fflag)THEN
!       CALL GETARG(0,arg,arglen)
!portability
        CALL GETARG (0,arg)
        arglen = len_trim(arg)

        DO tvi1=1,arglen
          IF(arg(tvi1:tvi1).EQ.SLASH)tvi2=tvi1
        ENDDO
        abvfile=ARG(1:TVI2)//'SOIL.CDE'
        INQUIRE(FILE=abvfile,EXIST=fflag)
      ENDIF
      IF(fflag)THEN
        ! Read abbreviation file
! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.        
!        WRITE(fnumrea,*)' '
!        WRITE(fnumrea,*)' Reading abbrev file: ',abvfile(1:55)
        CALL AMAKEABV(ABVFILE,abvarray,abvsyn)
      ELSE
!        WRITE(fnumrea,*)' '
!        WRITE(fnumrea,*)' No abbreviation file: ',abvfile(1:55)
        abvsyn=0
      ENDIF

 5555 CONTINUE
      CALL ARAYREAD(CODE,DATARRAY,DATANUM,ABVARRAY,ABVSYN,TXTO)
      !WRITE(FNUMREA,*) 'CODE,TXTO ',CODE,TXTO

      RETURN

 9998 CONTINUE
      CLOSE(filenum)
      CALL Getlun('ERROR.OUT',fnumerr)
      OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
      WRITE (fnumerr,*) ' Did not find soil: ',slcode
      WRITE (fnumerr,*) ' File: ',sldirfle(1:50)
      WRITE (fnumerr,*) ' Check READS.OUT for details'
      WRITE (*,*) ' Did not find soil: ',slcode
      WRITE (*,*) ' File: ',sldirfle(1:50)
      WRITE (*,*) ' Program will have to stop'
      WRITE (*,*) ' Check READS.OUT for details'
      CLOSE (fnumerr)
      STOP ' '

 9999 CONTINUE
      CLOSE(filenum)
      CALL Getlun('ERROR.OUT',fnumerr)
      OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
      WRITE (fnumerr,*) ' Problem reading: ',sldirfle(1:50)
      WRITE (fnumerr,*) ' Check READS.OUT for details'
      WRITE (*,*) ' Problem reading: ',sldirfle(1:50)
      WRITE (*,*) ' Program will have to stop'
      WRITE (*,*) ' Check READS.OUT for details'
      CLOSE (fnumerr)
      STOP ' '

      END

!-----------------------------------------------------------------------

      SUBROUTINE SLREADC(SLDIRFLE,SLCODE,CODE,charout)

      ! Returns one character string for particular code

      IMPLICIT NONE
      EXTERNAL Getlun, LTRIM, LTRIM2, SLREADT

      CHARACTER (LEN=*)   SLDIRFLE, SLCODE, CODE, CHAROUT
      CHARACTER (LEN=254) TL2541, TXTO
      CHARACTER (LEN=100) CODENEW
      CHARACTER (LEN=20)  COEFF
      CHARACTER (LEN=1)   FOUND
      INTEGER             TVI1, STARTPOS, ENDPOS
      INTEGER             LENGTH, TVI3, CODECLEN, FNUMERR

      INTRINSIC           LEN

      SAVE

      CALL SLREADT(SLDIRFLE,SLCODE,CODE,TXTO)

      CALL LTRIM2(CODE,codenew)
      CODECLEN=LEN(CHAROUT)

      DO TVI1=1,CODECLEN
        CHAROUT(TVI1:TVI1)=' '
      ENDDO

      DO TVI1=1,20
        Coeff(TVI1:TVI1)=' '
      ENDDO

      FOUND='N'
      STARTPOS=1
      ENDPOS=0

      TL2541=TXTO
      CALL Ltrim(TL2541)

      DO TVI3=1, 100
        IF(TL2541(TVI3:TVI3).EQ.' ')THEN
          ENDPOS=TVI3
          FOUND='Y'
          GO TO 1000
        ENDIF
      ENDDO
 1000 CONTINUE
      IF(FOUND.EQ.'Y')THEN
        LENGTH=ENDPOS-STARTPOS+1
        COEFF(1:LENGTH)=TL2541(STARTPOS:ENDPOS)
        CHAROUT=' '
        IF (CODECLEN.LT.20)THEN
          CHAROUT=COEFF(1:CODECLEN)
        ELSE
          CHAROUT(1:20)=COEFF
        ENDIF
       ELSE
         CALL Getlun('ERROR.OUT',fnumerr)
         OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
         WRITE (fnumerr,*) ' Could not find code: ',Code
         WRITE (fnumerr,*) ' File was: ',sldirfle(1:50)
         WRITE (fnumerr,*) ' Check READS.OUT for details'
         WRITE (*,*) ' Could not find code: ',Code
         WRITE (*,*) ' File was: ',sldirfle(1:50)
         WRITE (*,*) ' Program will have to stop'
         WRITE (*,*) ' Check READS.OUT for details'
         CLOSE (fnumerr)
         STOP ' '
       ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE SLREADR(SLDIRFLE,SLCODE,CODE,valueout)

      ! Returns one real value for particular code

      IMPLICIT NONE
      EXTERNAL SLREADC, TVRFROMCCDE, TVRFROMC

      CHARACTER (LEN=*)  SLDIRFLE, SLCODE, CODE
      CHARACTER (LEN=30) CHAROUT
      REAL               VALUEOUT, TVRFROMC, TVRFROMCCDE

      SAVE

      CALL SLREADC(SLDIRFLE,SLCODE,CODE,charout)
      VALUEOUT=TVRFROMCCDE(Code,Charout(1:12))
      VALUEOUT=TVRFROMC(Charout(1:12))

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE SLREADCA(SLDIRFLE,SLCODE,CODE,AOUTSIZE,arrayout)

      ! Returns ARRAY for particular code

      IMPLICIT NONE
      EXTERNAL Standard, LTRIM2, TVRFROMCCDE, TVRFROMC, SLREADT

      CHARACTER (LEN=*)   SLDIRFLE, SLCODE, CODE, AOUTSIZE, ARRAYOUT(*)
      CHARACTER (LEN=354) TL3541
      CHARACTER (LEN=254) TL2541,TXTO
      CHARACTER (LEN=100) CODENEW
      REAL                TVR1, TVRFROMC, TVRFROMCCDE
      INTEGER             TVI1, L, L2, SIZE, LENARVAR

      INTRINSIC           LEN,NINT

      SAVE

      CALL SLREADT(SLDIRFLE,SLCODE,CODE,TXTO)

      CALL LTRIM2(CODE,codenew)

      TVR1=TVRFROMCCDE('AOUTSIZE',AOUTSIZE)
      TVR1=TVRFROMC(AOUTSIZE)
      SIZE=NINT(TVR1)

      DO TVI1=1,SIZE
        ARRAYOUT(TVI1)=' '
      ENDDO

      LENARVAR=LEN(arrayout(1))

      TL2541=TXTO
      CALL Standard(TL2541,TL3541,'7')
      DO L=1,SIZE
         L2=(L-1)*7 + 1
         IF (TL3541(L2:L2).EQ.'!')GO TO 1000
         IF (TL3541(L2:L2).EQ.' ')GO TO 1000
         ARRAYOUT(L)(1:LENARVAR)=TL3541(L2:L2+LENARVAR)
      END DO
 1000 CONTINUE

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE SLREADRA(SLDIRFLE,SLCODE,CODE,AOUTSIZE,arrayout)

      ! Returns ARRAY for particular code

      IMPLICIT NONE
      EXTERNAL Standard, LTRIM2, TVRFROMCCDE, TVRFROMC, SLREADT

      CHARACTER (LEN=*)   SLDIRFLE, SLCODE, CODE, AOUTSIZE
      CHARACTER (LEN=354) TL3541
      CHARACTER (LEN=254) TL2541,TXTO
      CHARACTER (LEN=100) CODENEW
      REAL                TVR1, TVRFROMC, ARRAYOUT(*), TVRFROMCCDE
      INTEGER             TVI1, L, L2, SIZE, I

      INTRINSIC           NINT

      SAVE

      CALL LTRIM2(CODE,codenew)
      IF(CODENEW(1:5).EQ.'DLAYR')THEN
        CALL SLREADT(SLDIRFLE,SLCODE,'SLB',TXTO)
      ELSE
        CALL SLREADT(SLDIRFLE,SLCODE,CODE,TXTO)
        CALL LTRIM2(CODE,codenew)
      ENDIF

      TVR1=TVRFROMCCDE('AOUTSIZE',AOUTSIZE)
      TVR1=TVRFROMC(AOUTSIZE)
      SIZE=NINT(TVR1)

      DO TVI1=1,SIZE
        ARRAYOUT(TVI1)=-99
      ENDDO

      TL2541=TXTO
      CALL Standard(TL2541,TL3541,'7')
      DO L=1,SIZE
         L2=(L-1)*7 + 1
         IF (TL3541(L2:L2).EQ.'!')GO TO 1000
         IF (TL3541(L2:L2).EQ.' ')GO TO 1000
         ARRAYOUT(L)=TVRFROMCCDE(TL3541,TL3541(L2:L2+5))
         ARRAYOUT(L)=TVRFROMC(TL3541(L2:L2+5))
      END DO
 1000 CONTINUE

      IF(CODENEW(1:5).EQ.'DLAYR')THEN
        DO I=SIZE,2,-1
          IF (ARRAYOUT(I) .GT. ARRAYOUT(I-1)) THEN
            ARRAYOUT(I)=ARRAYOUT(I) - ARRAYOUT(I-1)
          ENDIF
        ENDDO
      ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE SMREADT(MODEL,CODE,TXTO)

      USE OSDefinitions

      IMPLICIT NONE
      EXTERNAL Getlun, LTRIM2, AMAKER0, AMAKEABV, ARAYREAD

      CHARACTER (LEN=254) DATARRAY(500), ABVARRAY(1000)
      CHARACTER (LEN=250) ARG
      character (LEN=250) CODENEW
      CHARACTER (LEN=250) ABVFILE
      CHARACTER (LEN=250) SMDIRFLE
      CHARACTER (LEN=12)  SMFILE
      CHARACTER (LEN=8)   MODEL, MODELP
!     CHARACTER (LEN=1)   FNUMFLAG
      CHARACTER (LEN=*)   TXTO, CODE
      INTEGER             TVI2, DATANUM
      INTEGER             TVI1
      INTEGER             ARGLEN, FNUMERR, ABVSYN  !, FNUMREA
      LOGICAL             FFLAG  !, FOPEN

      SAVE

! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.
!      IF (FNUMFLAG.NE.'Y')THEN
!        CALL Getlun('READS.OUT',fnumrea)
!        INQUIRE (FILE='READS.OUT',OPENED=fopen)
!        IF (.NOT.fopen) OPEN(UNIT=fnumrea,FILE='READS.OUT')
!        FNUMFLAG = 'Y'
!      ENDIF

      CALL LTRIM2(CODE,codenew)

      IF(MODEL.EQ.MODELP) GOTO 5555

      ! FILE NAME
!     CALL GETARG(0,arg,arglen)
!portability
      CALL GETARG (0,arg)
      arglen = len_trim(arg)

      DO tvi1=1,arglen
        IF(arg(tvi1:tvi1).EQ.SLASH)tvi2=tvi1
      ENDDO
      smfile=model(1:8)//'.SOM'
      ! Establish location
      INQUIRE(FILE=smfile,EXIST=fflag)
      IF (fflag) THEN
        smdirfle=smfile
      ELSE
        smdirfle=ARG(1:TVI2)//SMFILE
        INQUIRE(FILE=smdirfle,EXIST=fflag)
        IF (.NOT.fflag) THEN
          CALL Getlun('ERROR.OUT',fnumerr)
          OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
          WRITE (fnumerr,*) ' SOM coefficients file not found!!'
          WRITE (fnumerr,*) ' File sought was: ',smdirfle(1:50)
          WRITE (fnumerr,*) ' Check READS.OUT for details'
          WRITE (*,*) ' SOM coefficients file not found!!'
          WRITE (*,*) ' File sought was: ',smdirfle(1:50)
          WRITE (*,*) ' Program will have to stop'
          WRITE (*,*) ' Check READS.OUT for details'
          CLOSE (fnumerr)
          STOP ' '
        ENDIF
      ENDIF

      CALL AMAKER0(SMDIRFLE,datarray,datanum)

      IF (DATANUM .LE. 0)THEN
        CALL Getlun('ERROR.OUT',fnumerr)
        OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
        WRITE (fnumerr,*) ' Problem reading: ',SMDIRFLE(1:50)
        WRITE (fnumerr,*) ' Nothing in the array of input data!'
        WRITE (fnumerr,*) ' Check READS.OUT for details'
        WRITE (*,*) ' Problem reading: ',SMDIRFLE(1:50)
        WRITE (*,*) ' Likely that level not found '
        WRITE (*,*) ' Program will have to stop'
        WRITE (*,*) ' Check READS.OUT for details'
        CLOSE (fnumerr)
        STOP ' '
      ENDIF

      abvfile=' '
      abvfile(1:10)='SOM.CDE'
      INQUIRE(FILE=abvfile,EXIST=fflag)
      IF(.NOT.fflag)THEN
!       CALL GETARG(0,arg,arglen)
!portability
        CALL GETARG (0,arg)
        arglen = len_trim(arg)

        DO tvi1=1,arglen
          IF(arg(tvi1:tvi1).EQ.SLASH)tvi2=tvi1
        ENDDO
        abvfile=ARG(1:TVI2)//'SOM.CDE'
        INQUIRE(FILE=abvfile,EXIST=fflag)
      ENDIF
      IF(fflag)THEN
        ! Read abbreviation file
        CALL AMAKEABV(ABVFILE,abvarray,abvsyn)
      ELSE
        abvsyn=0
      ENDIF

 5555 CONTINUE

      CALL ARAYREAD(CODE,DATARRAY,DATANUM,ABVARRAY,ABVSYN,TXTO)
      !WRITE(FNUMREA,*) 'CODE,TXTO ',CODE,TXTO

      modelp=model

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE SMREADC(MODEL,CODE,charout)

      ! Returns one character string for particular code

      IMPLICIT NONE
      EXTERNAL Getlun, LTRIM, LTRIM2, SMREADT

      CHARACTER (LEN=*)   CODE, CHAROUT, MODEL
      CHARACTER (LEN=254) TXTO, TL2541
      CHARACTER (LEN=100) CODENEW
      CHARACTER (LEN=20)  COEFF
      CHARACTER (LEN=1)   FOUND
      INTEGER             TVI1, STARTPOS, ENDPOS
      INTEGER             LENGTH, TVI3, CODECLEN, FNUMERR

      INTRINSIC           LEN

      SAVE

      CALL SMREADT(MODEL,CODE,TXTO)

      CALL LTRIM2(CODE,codenew)
      CODECLEN=LEN(CHAROUT)

      DO TVI1=1,CODECLEN
        CHAROUT(TVI1:TVI1)=' '
      ENDDO

      DO TVI1=1,20
        Coeff(TVI1:TVI1)=' '
      ENDDO

      FOUND='N'
      STARTPOS=1
      ENDPOS=0

      TL2541=TXTO
      CALL Ltrim(TL2541)

      DO TVI3=1, 100
        IF(TL2541(TVI3:TVI3).EQ.' ')THEN
          ENDPOS=TVI3
          FOUND='Y'
          GO TO 1000
        ENDIF
      ENDDO
 1000 CONTINUE
      IF(FOUND.EQ.'Y')THEN
        LENGTH=ENDPOS-STARTPOS+1
        COEFF(1:LENGTH)=TL2541(STARTPOS:ENDPOS)
        CHAROUT=' '
        IF (CODECLEN.LT.20)THEN
          CHAROUT=COEFF(1:CODECLEN)
        ELSE
          CHAROUT(1:20)=COEFF
        ENDIF
       ELSE
         CALL Getlun('ERROR.OUT',fnumerr)
         OPEN(UNIT=fnumerr,FILE='ERROR.OUT')
         WRITE (fnumerr,*) ' Could not find code: ',Code
         WRITE (fnumerr,*) ' Working with SOM parameter file'
         WRITE (fnumerr,*) ' Check READS.OUT for details'
         WRITE (*,*) ' Could not find code: ',Code
         WRITE (*,*) ' Working with SOM parameter file'
         WRITE (*,*) ' Program will have to stop'
         WRITE (*,*) ' Check READS.OUT for details'
         CLOSE (fnumerr)
         STOP ' '
       ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE SMREADR(MODEL,CODE,valueout)

      ! Returns one real value for particular code

      IMPLICIT NONE
      EXTERNAL SMREADC, TVRFROMCCDE, TVRFROMC

      CHARACTER (LEN=*)  CODE, MODEL
      CHARACTER (LEN=30) CHAROUT
      REAL               VALUEOUT, TVRFROMC, TVRFROMCCDE

      SAVE

      CALL SMREADC(MODEL,CODE,charout)
      VALUEOUT=TVRFROMCCDE(Code,Charout(1:12))
      VALUEOUT=TVRFROMC(Charout(1:12))

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE SMREADRA(MODEL,CODE,AOUTSIZE,arrayout)

      ! Returns ARRAY for particular code

      IMPLICIT NONE
      EXTERNAL Standard, LTRIM2, TVRFROMCCDE, TVRFROMC, SMREADT

      CHARACTER (LEN=*)   MODEL, CODE, AOUTSIZE
      CHARACTER (LEN=354) TL3541
      CHARACTER (LEN=254) TL2541,TXTO
      CHARACTER (LEN=100) CODENEW
      REAL                TVR1, TVRFROMC, ARRAYOUT(*), TVRFROMCCDE
      INTEGER             TVI1, L, L2, SIZE

      INTRINSIC           NINT

      SAVE

      CALL SMREADT(MODEL,CODE,TXTO)

      CALL LTRIM2(CODE,codenew)

      TVR1=TVRFROMCCDE('AOUTSIZE',AOUTSIZE)
      TVR1=TVRFROMC(AOUTSIZE)
      SIZE=NINT(TVR1)

      DO TVI1=1,SIZE
        ARRAYOUT(TVI1)=-99
      ENDDO

      TL2541=TXTO
      CALL Standard(TL2541,TL3541,'7')
      DO L=1,SIZE
         L2=(L-1)*7 + 1
         IF (TL3541(L2:L2).EQ.'!')GO TO 1000
         IF (TL3541(L2:L2).EQ.' ')GO TO 1000
         ARRAYOUT(L)=TVRFROMCCDE(TL3541,TL3541(L2:L2+5))
         ARRAYOUT(L)=TVRFROMC(TL3541(L2:L2+5))
      END DO
 1000 CONTINUE

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE SMREADR2(MODEL,CODE,DIMVAL,arrayout)

      ! Returns ARRAY for particular code

      IMPLICIT NONE
      EXTERNAL Standard, LTRIM2, TVRFROMCCDE, TVRFROMC, SMREADT

      CHARACTER (LEN=*)   CODE, DIMVAL, MODEL
      CHARACTER (LEN=354) TL3541
      CHARACTER (LEN=254) TL2541,TXTO
      CHARACTER (LEN=100) CODENEW
      INTEGER             TVI1, L, L2, ARRAYVAL, TVI2
      REAL                ARRAYOUT(0:20,3),TVRFROMC,DIMVALR,TVRFROMCCDE

      INTRINSIC           NINT

      SAVE

      CALL SMREADT(MODEL,CODE,TXTO)

      CALL LTRIM2(CODE,codenew)

      DIMVALR=TVRFROMCCDE('DIMVAL',DIMVAL)
      DIMVALR=TVRFROMC(DIMVAL)
      IF(DIMVALR.LT.2.0)THEN
        ARRAYVAL=NINT(10.0*(DIMVALR-1.0))
      ELSE
        ARRAYVAL=NINT(10.0*(DIMVALR-2.0))
      ENDIF

      DO TVI1=0,20
        DO TVI2=1,3
          ARRAYOUT(TVI1,tvi2)=-99
        enddo
      ENDDO

      TL2541=TXTO
      CALL Standard(TL2541,TL3541,'7')
      DO L=1,3
         L2=(L-1)*7 + 1
         IF (TL3541(L2:L2).EQ.'!')GO TO 1000
         IF (TL3541(L2:L2).EQ.' ')GO TO 1000
         IF(DIMVALR.LT.2)THEN
           ARRAYOUT(ARRAYVAL,L)=TVRFROMCCDE(TL3541,TL3541(L2:L2+5))
           ARRAYOUT(ARRAYVAL,L)=TVRFROMC(TL3541(L2:L2+5))
         ELSE
           ARRAYOUT(L,ARRAYVAL)=TVRFROMCCDE(TL3541,TL3541(L2:L2+5))
           ! QUESTION IS THIS CORRECT
           ARRAYOUT(L,ARRAYVAL)=TVRFROMC(TL3541(L2:L2+5))
         ENDIF
      END DO
 1000 CONTINUE

      RETURN

      END

!-----------------------------------------------------------------------
!
!     SUBROUTINE ECREADRA(ECDIRFLE,LEVEL,CODE,AOUTSIZE,arrayout)
!
!     ! Returns real array for particular code. Not now used
!
!     IMPLICIT NONE
!
!     CHARACTER*(*)  CODE,LEVEL,ECDIRFLE,AOUTSIZE
!     CHARACTER*254  TXTO
!     CHARACTER*1    POS1DONE
!     REAL           TVRFROMC,TVR1,ARRAYOUT(*)
!     INTEGER        TVI1, L, SIZE
!     INTEGER        ARRAYNUM, POSCOMMA
!
!     SAVE
!
!     TXTO=' '
!     CALL ECREADC(ECDIRFLE,LEVEL,CODE,txto)
!
!     TVR1=TVRFROMC(AOUTSIZE)
!     SIZE=NINT(TVR1)
!
!     DO TVI1=1,SIZE
!       ARRAYOUT(TVI1)=-99
!     ENDDO
!
!     POS1DONE='N'
!     ARRAYNUM=1
!     POSCOMMA=0
!     DO L=1,24
!        IF (TXTO(L:L).EQ.','.OR.TXTO(L:L).EQ.' ') THEN
!          IF (POS1DONE.EQ.'N') THEN
!            ARRAYOUT(1)=TVRFROMC(TXTO(1:L-1))
!            POS1DONE='Y'
!          ELSE
!            ARRAYOUT(ARRAYNUM)=TVRFROMC(TXTO(POSCOMMA+1:L-1))
!          ENDIF
!          IF (TXTO(L:L).EQ.'!') GO TO 1000
!          IF (TXTO(L:L).EQ.' ') GO TO 1000
!          ARRAYNUM=ARRAYNUM + 1
!          POSCOMMA=L
!        ENDIF
!     END DO
!1000 CONTINUE
!
!     RETURN
!
!     END
!
!-----------------------------------------------------------------------
!
!     SUBROUTINE E1READT(ECDIRFLE,ECCODE,CODE,TXTO)
!
!     IMPLICIT NONE
!
!     USE OSDefinitions
!
!     CHARACTER*(*) ECDIRFLE, ECCODE, CODE, TXTO
!     CHARACTER*1000 DATASTD, ABVSTD
!     CHARACTER*254 DATARRAY(500), TL2541, ABVLINE, DATALINE
!     CHARACTER*254 ABVARRAY(1000)
!     CHARACTER*132 ABVFILE
!     CHARACTER*128 ARG
!     CHARACTER*80  TL0801
!     CHARACTER*52  TEXTSTD, tl0521
!     CHARACTER*30  TL30
!     CHARACTER*12  COEFFC(200), ABVC(200), COEFFCTM, SYNCODE
!     CHARACTER*11  GROUP
!     CHARACTER*6   ECCODEP
!     CHARACTER*4   COEFFCHK
!     CHARACTER*1   TL1UPCASE, TEXT, BLANK, CSWOUT
!     INTEGER       TVILENT, TVI1, TVI2, TVI0, TVI3, TVI4
!     INTEGER       FILENUM, GROUPLEN, FILELEN, LINE, LENTMP
!     INTEGER       TVINSTR, ABVNUM, LENSTD, ABVLEN, DATANUM
!     INTEGER       ARGLEN, CODENUM, ABVSYN, fnumrea, L
!     LOGICAL       FFLAG,FOPEN
!
!     SAVE
!
!     PARAMETER     (BLANK=' ')
!
!     SAVE          ECCODEP
!
!     CSWOUT='Y'
!
!     IF (fnumrea.LE.0.OR.fnumrea.GT.1000) THEN
!       CALL Getlun('READS.OUT',fnumrea)
!       INQUIRE (FILE='READS.OUT',OPENED=fopen)
!       IF (.NOT.fopen) OPEN(UNIT=fnumrea,FILE='READS.OUT')
!     ENDIF
!     IF (FILENUM.LE.0.OR.FILENUM.GT.1000)
!    & CALL GETLUN('FILETMP',FILENUM)
!
!     LENSTD=12
!     CODENUM=1
!     SYNCODE=' '
!
!     IF (LEN(TRIM(ECCODE)).LT.3 .OR. ECCODE(1:3).EQ.'   ') THEN
!       WRITE(fnumrea,*)' No ecotype code in ecotype reads!! '
!       STOP
!     ENDIF
!
!     IF(ECCODE(1:6).EQ.ECCODEP(1:6))GO TO 5555
!
!     FILELEN=TVILENT(ECDIRFLE)
!
!     GROUP(1:1)='*'
!     GROUP(2:7)=ECCODE(1:6)
!     GROUPLEN=TVILENT(GROUP)
!
!     OPEN(filenum,FILE=ECDIRFLE(1:FILELEN),STATUS='OLD')
!
!     tl0801=' '
!     DO WHILE(tl0801(1:GROUPLEN).NE.GROUP(1:GROUPLEN))
!       READ(filenum,'(A80)',END=9998,ERR=9999)tl0801
!       IF (tl0801(2:9).EQ.'ECOTYPE:')THEN
!         tl0801(1:8)='       '
!         tl0801(9:9)='*'
!       ENDIF
!       CALL Ltrim(tl0801)
!       tl0801(1:1)=Tl1upcase(tl0801(1:1))
!       tl0801(2:2)=Tl1upcase(tl0801(2:2))
!       IF(tl0801(1:GROUPLEN).EQ.GROUP(1:GROUPLEN))EXIT
!     END DO
!
!     DO L=1,500
!       DATARRAY=' '
!     ENDDO
!     DATANUM=0
!     ABVNUM=0
!     LINE=0
!     TVI0=0
!     TVI1=0
!
!     DO
!
!       tl2541=' '
!       tl30=' '
!       LINE=LINE+1
!       DO
!         READ(filenum,'(A254)',END=999,ERR=9999)tl2541
!         IF(tl2541(1:1).EQ.'*')GOTO 999
!         IF(tl2541(1:1).EQ.'!')GOTO 888
!         IF(tvilent(tl2541).LT.3)GOTO 888
!         IF(tl2541(1:1).EQ.'@')THEN
!           ! Strip out leading periods
!           ABVLEN=TVILENT(TL2541)
!           DO TVI1=1,ABVLEN-1
!             IF(TL2541(TVI1:TVI1+1).EQ.' .')TL2541(TVI1:TVI1+1)='  '
!           ENDDO
!           IF (ABVLINE(2:30).NE.TL2541(2:30))THEN
!             TVI0=TVI0+ABVNUM
!             ABVLINE=tl2541
!             LINE=1
!           ENDIF
!           GO TO 888
!         ENDIF
!         EXIT
! 888     CONTINUE
!       END DO
!
!       DATALINE=tl2541
!
!       TEXT='N'
!
!       ABVSTD=' '
!       DATASTD=' '
!       tl0521=' '
!       TEXTSTD=' '
!       COEFFC(TVI0+1)=' '
!
!       CALL STANDC(ABVLINE,DATALINE,DATASTD,TEXTSTD,'12')
!       IF(ABVLINE(1:1).EQ.'@')ABVLINE(1:1)=' '
!       CALL STANDARD(ABVLINE,ABVSTD,'12')
!       ABVNUM=TVINSTR(ABVSTD)
!       ! NB MAXIMUM NUMBER OF VARIABLES ALONG LINE=40
!       IF(ABVNUM.GT.40)ABVNUM=40
!       DO TVI1=1,ABVNUM
!         TVI2=1 + (TVI1-1)*LENSTD
!         TVI4=TVI2 + (LENSTD-1)
!         COEFFC(TVI0+TVI1)=DATASTD(TVI2:TVI4)
!         ABVC(TVI0+TVI1)=ABVSTD(TVI2:TVI4)
!         COEFFCTM=COEFFC(TVI0+TVI1)
!         CALL LTRIM(COEFFCTM)
!         IF (TVILENT(COEFFCTM).LT.1)COEFFC(TVI0+TVI1)='-99         '
!         CALL LTRIM(TEXTSTD)
!         IF (TVILENT(TEXTSTD).LT.1)TEXTSTD(1:3)='-99'
!         COEFFCHK=COEFFCTM(1:4)
!         IF (LINE.EQ.1) THEN
!          IF(COEFFCHK.EQ.'TEXT')THEN
!           datarray(TVI0+tvi1)=abvc(TVI0+tvi1)//' '//TEXTSTD
!          ELSE
!           datarray(TVI0+tvi1)=abvc(TVI0+tvi1)//' '//coeffc(TVI0+tvi1)
!          ENDIF
!         ELSE
!          LENTMP=TVILENT(DATARRAY(TVI0+TVI1))
!          IF(COEFFCHK.EQ.'TEXT')THEN
!           TL2541=datarray(tvi0+tvi1)(1:LENTMP)//' '//TEXTSTD
!           datarray(TVI0+tvi1)=TL2541
!          ELSE
!           TL2541=datarray(tvi0+tvi1)(1:LENTMP)//' '//coeffc(TVI0+tvi1)
!           datarray(TVI0+tvi1)=TL2541
!          ENDIF
!         ENDIF
!       ENDDO
!
!       datanum=tvi0+abvnum
!
!     ENDDO
!
! 999 CONTINUE
!
!     CLOSE(FILENUM)
!
!     IF (CSWOUT.EQ.'Y') THEN
!       WRITE(fnumrea,*)' '
!       WRITE(fnumrea,*)'FILE ',ECDIRFLE(1:FILELEN)
!       WRITE(fnumrea,*)' GROUP ',GROUP(1:GROUPLEN)
!       DO TVI3=1,DATANUM
!         WRITE(fnumrea,'(I6,A2,A60)')tvi3,'  ',datarray(TVI3)(1:60)
!       ENDDO
!     ENDIF
!
!     arg=' '
!     abvfile=' '
!     abvfile(1:12)='ECCOEFF.CDE '
!     INQUIRE(FILE=abvfile,EXIST=fflag)
!     IF(.NOT.fflag)THEN
!       CALL GETARG(0,arg,arglen)
!       DO tvi1=1,arglen
!         IF(arg(tvi1:tvi1).EQ.SLASH)tvi2=tvi1
!       ENDDO
!       abvfile=ARG(1:TVI2)//'ECCOEFF.CDE'
!       INQUIRE(FILE=abvfile,EXIST=fflag)
!     ENDIF
!     IF(fflag)THEN
!       ! Read abbreviation file
!       WRITE(fnumrea,*)'  Reading abbrev file: ',abvfile(1:55)
!       CALL AMAKEABV(ABVFILE,abvarray,abvsyn)
!     ELSE
!       WRITE(fnumrea,*)'  No abbreviation file: ',abvfile(1:55)
!       abvsyn=0
!     ENDIF
!
!     ECCODEP(1:6)=ECCODE(1:6)
!
!5555 CONTINUE
!
!     CALL ARAYREAD(CODE,DATARRAY,DATANUM,ABVARRAY,ABVSYN,TXTO)
!     IF (CSWOUT.EQ.'Y') WRITE(fnumrea,*)'  E1READ ',CODE,' ',TXTO(1:40)
!
!     RETURN
!
!9998 CONTINUE
!     WRITE(fnumrea,*)'  Did not find ecotype: ',ECCODE
!     WRITE(fnumrea,*)'  File: ',ECDIRFLE(1:50)
!     CLOSE(filenum)
!     STOP
!
!9999 CONTINUE
!     WRITE(fnumrea,*)'  Problem reading: ',ECDIRFLE(1:50)
!     CLOSE(filenum)
!     STOP
!
!     END
!
!-----------------------------------------------------------------------
!
!     SUBROUTINE E1READC(ECDIRFLE,ECCODE,CODE,charout)
!
!     ! Returns one character string for particular code
!
!     IMPLICIT NONE
!
!     CHARACTER*(*) ECDIRFLE, ECCODE, CODE, CHAROUT
!     CHARACTER*254 TL2541, TXTO
!     CHARACTER*100 CODENEW
!     CHARACTER*20  COEFF
!     CHARACTER*1   FOUND
!     INTEGER       CODELEN, TVI1, TVILENT, STARTPOS, ENDPOS, LENGTH
!     INTEGER       TVI3, CODECLEN, fnumrea
!     LOGICAL       FOPEN
!
!     SAVE
!
!     CALL E1READT(ECDIRFLE,ECCODE,CODE,TXTO)
!
!     CALL LTRIM2(CODE,codenew)
!     CODELEN=Tvilent(CODENEW)
!     CODECLEN=LEN(CHAROUT)
!
!     DO TVI1=1,CODECLEN
!       CHAROUT(TVI1:TVI1)=' '
!     ENDDO
!
!     DO TVI1=1,20
!       Coeff(TVI1:TVI1)=' '
!     ENDDO
!
!     FOUND='N'
!     STARTPOS=1
!     ENDPOS=0
!
!     TL2541=TXTO
!     CALL Ltrim(TL2541)
!
!     DO TVI3=1, 100
!       IF(TL2541(TVI3:TVI3).EQ.' ')THEN
!         ENDPOS=TVI3
!         FOUND='Y'
!         GO TO 1000
!       ENDIF
!     ENDDO
!1000 CONTINUE
!     IF(FOUND.EQ.'Y')THEN
!       LENGTH=ENDPOS-STARTPOS+1
!       COEFF(1:LENGTH)=TL2541(STARTPOS:ENDPOS)
!       CHAROUT=' '
!       IF (CODECLEN.LT.20)THEN
!         CHAROUT=COEFF(1:CODECLEN)
!       ELSE
!         CHAROUT(1:20)=COEFF
!       ENDIF
!      ELSE
!        CALL Getlun('READS.OUT',fnumrea)
!        INQUIRE (FILE='READS.OUT',OPENED=fopen)
!        IF (.NOT.fopen) OPEN(UNIT=fnumrea,FILE='READS.OUT')
!        WRITE(fnumrea,*)'  Could not find code: ',Code
!        STOP
!      ENDIF
!
!     RETURN
!
!     END
!
!-----------------------------------------------------------------------
!
!     SUBROUTINE E1READR(ECDIRFLE,ECCODE,CODE,valueout)
!
!     ! Returns one real value for particular code
!
!     IMPLICIT NONE
!
!     CHARACTER*(*) ECDIRFLE, ECCODE, CODE
!     CHARACTER*30  CHAROUT
!     REAL          VALUEOUT, TVRFROMC
!
!     SAVE
!
!     CALL E1READC(ECDIRFLE,ECCODE,CODE,charout)
!     VALUEOUT=TVRFROMC(Charout(1:12))
!
!     RETURN
!
!     END
!
!-----------------------------------------------------------------------
!
!     SUBROUTINE E1READCA(ECDIRFLE,ECCODE,CODE,AOUTSIZE,arrayout)
!
!     ! Returns ARRAY for particular code
!
!     IMPLICIT NONE
!
!     CHARACTER*(*) ECDIRFLE, ECCODE, CODE, AOUTSIZE, ARRAYOUT(*)
!     CHARACTER*354 TL3541
!     CHARACTER*254 TL2541,TXTO
!     CHARACTER*100 CODENEW
!     REAL          TVR1, TVRFROMC
!     INTEGER       CODELEN, TVI1, TVILENT, L, L2, SIZE, LENARVAR
!
!     SAVE
!
!     CALL E1READT(ECDIRFLE,ECCODE,CODE,TXTO)
!
!     CALL LTRIM2(CODE,codenew)
!     CODELEN=Tvilent(CODEnew)
!
!     TVR1=TVRFROMC(AOUTSIZE)
!     SIZE=NINT(TVR1)
!
!     DO TVI1=1,SIZE
!       ARRAYOUT(TVI1)=' '
!     ENDDO
!
!     LENARVAR=LEN(arrayout(1))
!
!     TL2541=TXTO
!     CALL Standard(TL2541,TL3541,'7')
!     DO L=1,SIZE
!        L2=(L-1)*7 + 1
!        IF (TL3541(L2:L2).EQ.'!')GO TO 1000
!        IF (TL3541(L2:L2).EQ.' ')GO TO 1000
!        ARRAYOUT(L)(1:LENARVAR)=TL3541(L2:L2+LENARVAR)
!     END DO
!1000 CONTINUE
!
!     RETURN
!
!     END
!
!-----------------------------------------------------------------------
!
!     SUBROUTINE E1READRA(ECDIRFLE,ECCODE,CODE,AOUTSIZE,arrayout)
!
!     ! Returns ARRAY for particular code
!
!     IMPLICIT NONE
!
!     CHARACTER*(*) ECDIRFLE, ECCODE, CODE, AOUTSIZE
!     CHARACTER*354 TL3541
!     CHARACTER*254 TL2541,TXTO
!     CHARACTER*100 CODENEW
!     REAL          TVR1, TVRFROMC, ARRAYOUT(*)
!     INTEGER       CODELEN, TVI1, TVILENT, L, L2, SIZE
!
!     SAVE
!
!     CALL E1READT(ECDIRFLE,ECCODE,CODE,TXTO)
!     CALL LTRIM2(CODE,codenew)
!     CODELEN=Tvilent(CODEnew)
!
!     TVR1=TVRFROMC(AOUTSIZE)
!     SIZE=NINT(TVR1)
!
!     DO TVI1=1,SIZE
!       ARRAYOUT(TVI1)=-99
!     ENDDO
!
!     TL2541=TXTO
!     CALL Standard(TL2541,TL3541,'7')
!     DO L=1,SIZE
!        L2=(L-1)*7 + 1
!        IF (TL3541(L2:L2).EQ.'!')GO TO 1000
!        IF (TL3541(L2:L2).EQ.' ')GO TO 1000
!        ARRAYOUT(L)=TVRFROMC(TL3541(L2:L2+5))
!     END DO
!1000 CONTINUE
!
!     RETURN
!
!     END
!
!-----------------------------------------------------------------------
!
!     SUBROUTINE S1READT(SPDIRFLE,CODE,TXTO)
!
!     IMPLICIT NONE
!
!     USE OSDefinitions
!
!     CHARACTER*(*)   TXTO, CODE, SPDIRFLE
!     CHARACTER*254   DATARRAY(500), ABVARRAY(1000)
!     CHARACTER*128   ARG
!     character*100   CODENEW
!     CHARACTER*132   ABVFILE
!     CHARACTER*93    SPDIRFLT, SPDIRFLP
!     INTEGER         CODELEN, TVI2, FILELEN, DATANUM
!     INTEGER         TVILENT, TVI1, ARGLEN
!     INTEGER         fnumrea, ABVSYN
!     LOGICAL         FFLAG, FOPEN
!
!     SAVE
!
!     CALL LTRIM2(CODE,codenew)
!     CODELEN=Tvilent(CODEnew)
!
!     CALL LTRIM2(SPDIRFLE,spdirflt)
!     FILELEN=TVILENT(SPDIRFLT)
!     FILELEN=MIN(93,FILELEN)
!     SPDIRFLT=' '
!     SPDIRFLT(1:FILELEN)=SPDIRFLE(1:FILELEN)
!
!     IF(SPDIRFLE(1:FILELEN).EQ.SPDIRFLP(1:FILELEN))GO TO 5555
!
!     CALL AMAKER0(SPDIRFLT,datarray,datanum)
!     IF (DATANUM .LE. 0)THEN
!       WRITE(fnumrea,*)'  Problem reading: ',SPDIRFLT
!       WRITE (fnumrea,*) ' Nothing in the array of input data!'
!       STOP
!     ENDIF
!
!     IF (fnumrea.LE.0.OR.fnumrea.GT.1000) THEN
!       CALL Getlun('READS.OUT',fnumrea)
!       INQUIRE (FILE='READS.OUT',OPENED=fopen)
!       IF (.NOT.fopen) OPEN(UNIT=fnumrea,FILE='READS.OUT')
!     ENDIF
!
!     abvfile=' '
!     abvfile(1:10)='GCOEFF.CDE'
!     INQUIRE(FILE=abvfile,EXIST=fflag)
!     IF(.NOT.fflag)THEN
!       CALL GETARG(0,arg,arglen)
!       DO tvi1=1,arglen
!         IF(arg(tvi1:tvi1).EQ.SLASH)tvi2=tvi1
!       ENDDO
!       abvfile=ARG(1:TVI2)//'GCOEFF.CDE'
!       INQUIRE(FILE=abvfile,EXIST=fflag)
!       IF(fflag)THEN
!         ! Read abbreviation file
!         WRITE(fnumrea,*)'  Reading abbrev file: ',abvfile(1:55)
!         CALL AMAKEABV(ABVFILE,abvarray,abvsyn)
!       ELSE
!         WRITE(fnumrea,*)'  No abbreviation file: ',abvfile(1:55)
!         abvsyn=0
!       ENDIF
!     ENDIF
!
!5555 CONTINUE
!
!     CALL ARAYREAD(CODE,DATARRAY,DATANUM,ABVARRAY,ABVSYN,TXTO)
!     ! WRITE(fnumrea,*)'  S1READ ',CODE,' ',TXTO(1:40)
!
!     SPDIRFLP=' '
!     SPDIRFLP(1:FILELEN)=SPDIRFLT(1:FILELEN)
!
!     RETURN
!
!     END
!
!-----------------------------------------------------------------------
!
!     SUBROUTINE S1READC(SPDIRFLE,CODE,charout)
!
!     ! Returns one character string for particular code
!
!     IMPLICIT NONE
!
!     CHARACTER*(*)  SPDIRFLE
!     CHARACTER(*)   CODE, CHAROUT
!     CHARACTER*254  TXTO, TL2541
!     CHARACTER*100  CODENEW
!     CHARACTER*20   COEFF
!     CHARACTER*1    FOUND
!     INTEGER        CODELEN, TVI1, TVILENT, STARTPOS, ENDPOS, LENGTH
!     INTEGER        TVI3, CODECLEN, fnumrea
!     LOGICAL        FOPEN
!
!     SAVE
!
!     CALL S1READT(SPDIRFLE,CODE,TXTO)
!
!     CALL LTRIM2(CODE,codenew)
!     CODELEN=Tvilent(CODENEW)
!     CODECLEN=LEN(CHAROUT)
!
!     DO TVI1=1,CODECLEN
!       CHAROUT(TVI1:TVI1)=' '
!     ENDDO
!
!     DO TVI1=1,20
!       Coeff(TVI1:TVI1)=' '
!     ENDDO
!
!     FOUND='N'
!     STARTPOS=1
!     ENDPOS=0
!
!     TL2541=TXTO
!     CALL Ltrim(TL2541)
!
!     DO TVI3=1, 100
!       IF(TL2541(TVI3:TVI3).EQ.' ')THEN
!         ENDPOS=TVI3
!         FOUND='Y'
!         GO TO 1000
!       ENDIF
!     ENDDO
!1000 CONTINUE
!     IF(FOUND.EQ.'Y')THEN
!       LENGTH=ENDPOS-STARTPOS+1
!       COEFF(1:LENGTH)=TL2541(STARTPOS:ENDPOS)
!       CHAROUT=' '
!       IF (CODECLEN.LT.20)THEN
!         CHAROUT=COEFF(1:CODECLEN)
!       ELSE
!         CHAROUT(1:20)=COEFF
!       ENDIF
!      ELSE
!        CALL Getlun('READS.OUT',fnumrea)
!        INQUIRE (FILE='READS.OUT',OPENED=fopen)
!        IF (.NOT.fopen) OPEN(UNIT=fnumrea,FILE='READS.OUT')
!        WRITE(fnumrea,*)'  Could not find code: ',Code
!        STOP
!      ENDIF
!
!     RETURN
!
!     END
!
!-----------------------------------------------------------------------
!
!     SUBROUTINE S1READR(SPDIRFLE,CODE,valueout)
!
!     ! Returns one real value for particular code
!
!     IMPLICIT NONE
!
!     CHARACTER*(*)  SPDIRFLE, CODE
!     CHARACTER*30   CHAROUT
!     REAL           VALUEOUT, TVRFROMC
!
!     SAVE
!
!     CALL S1READC(SPDIRFLE,CODE,charout)
!     VALUEOUT=TVRFROMC(Charout(1:12))
!
!     RETURN
!
!     END
!
!-----------------------------------------------------------------------
!
!     SUBROUTINE S1READRA(SPDIRFLE,CODE,AOUTSIZE,arrayout)
!
!     ! Returns ARRAY for particular code
!
!     IMPLICIT NONE
!
!     CHARACTER*(*)   SPDIRFLE, CODE, AOUTSIZE
!     CHARACTER*354   TL3541
!     CHARACTER*254   TL2541,TXTO
!     CHARACTER*100   CODENEW
!     REAL            TVR1, TVRFROMC, ARRAYOUT(*)
!     INTEGER         CODELEN, TVI1, TVILENT, L, L2, SIZE
!
!     SAVE
!
!     CALL S1READT(SPDIRFLE,CODE,TXTO)
!
!     CALL LTRIM2(CODE,codenew)
!     CODELEN=Tvilent(CODEnew)
!
!     TVR1=TVRFROMC(AOUTSIZE)
!     SIZE=NINT(TVR1)
!
!     DO TVI1=1,SIZE
!       ARRAYOUT(TVI1)=-99
!     ENDDO
!
!     TL2541=TXTO
!     CALL Standard(TL2541,TL3541,'7')
!     DO L=1,SIZE
!        L2=(L-1)*7 + 1
!        IF (TL3541(L2:L2).EQ.'!')GO TO 1000
!        IF (TL3541(L2:L2).EQ.' ')GO TO 1000
!        ARRAYOUT(L)=TVRFROMC(TL3541(L2:L2+5))
!     END DO
!1000 CONTINUE
!
!     RETURN
!
!     END
!
!-----------------------------------------------------------------------
!
!     SUBROUTINE S1READIA(SPDIRFLE,CODE,AOUTSIZE,arrayout)
!
!     ! Returns ARRAY for particular code
!
!     IMPLICIT NONE
!
!     CHARACTER*(*)   SPDIRFLE, CODE, AOUTSIZE
!     CHARACTER*354   TL3541
!     CHARACTER*254   TL2541,TXTO
!     CHARACTER*100   CODENEW
!     INTEGER         CODELEN, TVI1, TVILENT, L, L2, SIZE, ARRAYOUT(*)
!     INTEGER         TVIFROMC
!
!     SAVE
!
!     CALL S1READT(SPDIRFLE,CODE,TXTO)
!
!     CALL LTRIM2(CODE,codenew)
!     CODELEN=Tvilent(CODEnew)
!
!     SIZE=TVIFROMC(AOUTSIZE)
!
!     DO TVI1=1,SIZE
!       ARRAYOUT(TVI1)='-99'
!     ENDDO
!
!     TL2541=TXTO
!     CALL Standard(TL2541,TL3541,'7')
!     DO L=1,SIZE
!        L2=(L-1)*7 + 1
!        IF (TL3541(L2:L2).EQ.'!')GO TO 1000
!        IF (TL3541(L2:L2).EQ.' ')GO TO 1000
!        ARRAYOUT(L)=TVIFROMC(TL3541(L2:L2+5))
!     END DO
!1000 CONTINUE
!
!     RETURN
!
!     END
!
!-----------------------------------------------------------------------
!
!     SUBROUTINE S1READCA(SPDIRFLE,CODE,AOUTSIZE,arrayout)
!
!     ! Returns ARRAY for particular code
!
!     IMPLICIT NONE
!
!     CHARACTER*(*)   SPDIRFLE, CODE, AOUTSIZE, ARRAYOUT(*)
!     CHARACTER*354   TL3541
!     CHARACTER*254   TL2541,TXTO
!     CHARACTER*100   CODENEW
!     REAL            TVR1, TVRFROMC
!     INTEGER         CODELEN, TVI1, TVILENT, L, L2, SIZE, LENARVAR
!
!     SAVE
!
!     CALL S1READT(SPDIRFLE,CODE,TXTO)
!
!     CALL LTRIM2(CODE,codenew)
!     CODELEN=Tvilent(CODEnew)
!
!     TVR1=TVRFROMC(AOUTSIZE)
!     SIZE=NINT(TVR1)
!
!     DO TVI1=1,SIZE
!       ARRAYOUT(TVI1)=' '
!     ENDDO
!
!     LENARVAR=LEN(arrayout(1))
!
!     TL2541=TXTO
!     CALL Standard(TL2541,TL3541,'7')
!     DO L=1,SIZE
!        L2=(L-1)*7 + 1
!        IF (TL3541(L2:L2).EQ.'!')GO TO 1000
!        IF (TL3541(L2:L2).EQ.' ')GO TO 1000
!        ARRAYOUT(L)(1:LENARVAR)=TL3541(L2:L2+LENARVAR)
!     END DO
!1000 CONTINUE
!
!     RETURN
!
!     END
