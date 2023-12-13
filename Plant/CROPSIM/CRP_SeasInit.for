!***********************************************************************
! This is the code from the section (DYNAMIC == SEASINIT) 
! lines 2161 - 5249 of the original CSCRP code.
!***********************************************************************

      SUBROUTINE CRP_SeasInit (ALBEDOS, GSTAGE, LAI, CANHT, CLOUDS,
     &     CN, DEWDUR, DOY, HARVFRAC, ISWDIS, ISWNIT,
     &     KCAN, KEP, LAIL, LAILA, NFP, ON, PARIP,
     &     PARIPA, RESCALG, RESLGALG, RESNALG, RLV, RN, RNMODE,
     &     RUN, RUNI, RWUMX, RWUPM, SENCALG,
     &     UH2O, UNH4, UNO3, YEAR, SENLALG, SENNALG, SLPF, SN,
     &     STGYEARDOY, TAIRHR, TN, TRWUP)

! 2023-04-13 TF removed unused variables in argument list
!     IDETG, ISWWAT

      USE ModuleData
      USE CRP_First_Trans_m

      IMPLICIT NONE
        EXTERNAL YR_DOY, GETLUN, Y4K_DOY, TVILENT, LTRIM, XREADC, 
     &    XREADT, SPREADRA, XREADI, XREADR, UCASE, XREADIA, XREADRA, 
     &    FVCHECK, FINDDIR, CUREADC, CUREADR, ECREADR, SPREADR, 
     &    CRP_SeasInit_VarInit, XREADCA, LTRIM2, CSUCASE, SPREADC,
     &    SPREADCA, WARNING
      
      INTEGER TVILENT
      INTEGER STGYEARDOY(20), CN, DOY, ON, RN, RUN, RUNI        
      INTEGER SN, TN, YEAR
      
      REAL ALBEDOS, GSTAGE, LAI, CANHT, CLOUDS, DEWDUR, HARVFRAC(2)
      REAL KCAN        
      REAL KEP, LAIL(30), LAILA(30), NFP, PARIP, PARIPA, RESCALG(0:NL)             
      REAL RESLGALG(0:NL), RESNALG(0:NL), RLV(NL), RWUMX, RWUPM       
      REAL SENCALG(0:NL), SENLALG(0:NL), SENNALG(0:NL), SLPF, TAIRHR(24)  
      REAL TRWUP, UH2O(NL), UNH4(NL), UNO3(NL)
      
      CHARACTER(LEN=1) ISWDIS, ISWNIT, RNMODE !IDETG, ISWWAT
!-----------------------------------------------------------------------
!       Initialize both state and rate variables                       
!-----------------------------------------------------------------------
        
      CALL CRP_SeasInit_VarInit (GSTAGE, LAI, CANHT, DEWDUR, LAIL,
     &   LAILA, NFP, PARIP, PARIPA, RESCALG, RESLGALG, RESNALG, RLV,
     &   SENCALG, SENLALG, SENNALG, STGYEARDOY, TRWUP, UH2O,
     &   UNH4, UNO3)
        
!-----------------------------------------------------------------------
!       Read experiment information from Dssat input or X- file
!-----------------------------------------------------------------------

        ! Methods
        CALL XREADC(FILEIO,TN,RN,SN,ON,CN,'PHOTO',mephs)
        CALL XREADC(FILEIO,TN,RN,SN,ON,CN,'MEWNU',mewnu)
        CALL XREADC(FILEIO,TN,RN,SN,ON,CN,'METHODS',meexp)

        ! Experiment, treatment, and run control names
        CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'ENAME',ename)
        CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'EXPER',excode)
        CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'TNAME',tname)
        CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'SNAME',runname)

        ! Planting date information
        CALL XREADC(FILEIO,TN,RN,SN,ON,CN,'PLANT',iplti)
!       IF(IPLTI.EQ.'A'.OR.IPLTI.EQ.'a')THEN
        IF(IPLTI.EQ.'A'.OR.IPLTI.EQ.'a'.OR.
     &     IPLTI.EQ.'F'.OR.IPLTI.EQ.'f')THEN
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'PFRST',pwdinf)
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'PLAST',pwdinl)
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'PH2OL',swpltl)
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'PH2OU',swplth)
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'PH2OD',swpltd)
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'PSTMX',ptx)
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'PSTMN',pttn)
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'HFRST',hfirst)
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'HLAST',hlast)
        ELSE
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'PDATE',pdate)
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'IDATE',idate1)
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'EDATE',edatmx)
        ENDIF

        ! Other planting information
        CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'CR',crop)
        CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'INGENO',varno)
        CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'CNAME',vrname)
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPOP',pltpopp)
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPOE',pltpope)
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PLRS',rowspc)
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PLDP',sdepth)
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PLWT',sdrate)
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PAGE',plmage)
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'SPRL',sprl)
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PLPH',plph)
        CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'PLME',plme)

        ! Harvest instructions
        CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'HARVS',ihari)
        CALL XREADRA (FILEIO,TN,RN,SN,ON,CN,'HPC','40',hpc)
        CALL XREADRA (FILEIO,TN,RN,SN,ON,CN,'HBPC','40',hbpc)

        CALL XREADIA(FILEIO,TN,RN,SN,ON,CN,'HDATE','40',hyrdoy)
        CALL XREADCA(FILEIO,TN,RN,SN,ON,CN,'HOP','40',hop)
        CALL XREADRA(FILEIO,TN,RN,SN,ON,CN,'HAMT','40',hamt)
        CALL XREADRA(FILEIO,TN,RN,SN,ON,CN,'CWAN','40',cwan)
        CALL XREADRA(FILEIO,TN,RN,SN,ON,CN,'LSNUM','40',lsnum)
        CALL XREADRA(FILEIO,TN,RN,SN,ON,CN,'LSWT','40',lswt)
        
            ! LAH Following inserted to allow examination of grazing
            IF (EXCODE.EQ.'KSAS8101WH'.AND.TN.EQ.1) THEN
              tvi1 = 0
            ELSE
              tvi1 = 0
            ENDIF  
            IF (tvi1.EQ.1) THEN
              HYRDOY(1) = 82100
              HOP(1) = 'G'
              HAMT(1) = 500.0
              CWAN(1) = 100.0
              HPC(1) = -99.0
              HBPC(1) = -99.0
              HYRDOY(2) = 82130
              HOP(2) = 'G'
              HAMT(2) = 900.0
              CWAN(2) = 100.0
              HPC(2) = -99.0
              HBPC(2) = -99.0
              HYRDOY(3) = 82180
              HOP(3) = 'F'
              HAMT(3) = -99.0
              CWAN(3) = -99.0
              HPC(3) = 100.0
              HBPC(3) = 100.0
            ENDIF
        
        DO I = 1,20
          IF (hyrdoy(i).EQ.-99) THEN
            hnumber = i - 1
            EXIT  
          ENDIF
C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY          
          !hyeardoy(i) = CSYEARDOY(hyrdoy(i))
          CALL Y4K_DOY(hyrdoy(i),FILEX,0,ERRKEY,3)
          hyeardoy(i) = hyrdoy(i)
        ENDDO
        IF (hnumber.LE.1) HOP(1) = 'F' 
        yeardoyharf = -99
        DO I = 1, 20
          IF (HYEARDOY(I).GT.0) THEN
            hnumber = I
            IF (hop(i).EQ.'F') THEN
              hpcf = hpc(i)
              hbpcf = hbpc(i)
              yeardoyharf = hyeardoy(i)
            ENDIF 
          ENDIF
        END DO
        IF (hnumber.EQ.1) THEN
          hpcf = hpc(1)
          hbpcf = hbpc(1)
          yeardoyharf = hyeardoy(1)
        ENDIF 
        ! If running CSM use harvfrac so as to handle automatic mngement
        IF (FILEIOT .NE. 'DS4') THEN
          hpcf = harvfrac(1)*100.0   ! Harvest %
          hbpcf = harvfrac(2)*100.0
        ENDIF

        ! Fertilization information (to calculate N appl during cycle)
        CALL XREADC(FILEIO,TN,RN,SN,ON,CN,'FERTI',iferi)
        CALL XREADIA(FILEIO,TN,RN,SN,ON,CN,'FDATE','200',fday)
        CALL XREADRA(FILEIO,TN,RN,SN,ON,CN,'FAMN','200',anfer)

        ! Water table depth
        CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'ICWD',icwd)

        ! Disease information
        LENDIS = TVILENT(ISWDIS)
        DIDAT = -99
        DIGFAC = -99
        DIFFACR = -99
        DCDAT = -99
        DCDUR = -99
        DCFAC = -99
        DCTAR = -99
        IF (LENDIS.EQ.1.AND.ISWDIS(LENDIS:LENDIS).EQ.'R') THEN
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'D1DAT',didat(1))
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'D2DAT',didat(2))
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'D3DAT',didat(3))
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'D1GF',digfac(1))
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'D2GF',digfac(2))
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'D3GF',digfac(3))
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'D1FFR',diffacr(1))
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'D2FFR',diffacr(2))
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'D3FFR',diffacr(3))
          CALL XREADIA(FILEIO,TN,RN,SN,ON,CN,'DCDAT','10',dcdat)
          CALL XREADRA(FILEIO,TN,RN,SN,ON,CN,'DCDUR','10',dcdur)
          CALL XREADRA(FILEIO,TN,RN,SN,ON,CN,'DCFAC','10',dcfac)
          CALL XREADIA(FILEIO,TN,RN,SN,ON,CN,'DCTAR','10',dctar)
        ELSEIF (LENDIS.EQ.1.AND.ISWDIS(LENDIS:LENDIS).EQ.'Y') THEN
          DIGFAC(1) = 1.0
          DIGFAC(2) = 1.0
          DIGFAC(3) = 1.0
        ELSEIF (LENDIS.GT.1) THEN
          ! LAH ISWDIS DEFINED AS NUMBER FOR WHICH DISEASES
          ! NOT JUST ONE NUMBER OR CHARACTER
          ! DISCUSS WITH CHP 
          CALL LTRIM(ISWDIS)
          !READ(ISWDIS,'(I1)') DIGFACTMP
          !DIGFAC(1) = DIGFACTMP/10.0
          !READ(ISWDIS,'(2X,I1)') DIGFACTMP
          !DIGFAC(2) = DIGFACTMP/10.0
          !READ(ISWDIS,'(4X,I1)') DIGFACTMP
          !DIGFAC(3) = DIGFACTMP/10.0
        ENDIF

        IF (FILEIOT(1:2).EQ.'DS') THEN
          ! Genotype file names and locations
          CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'CFILE',cufile)
          CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'CDIR',pathcr)
          CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'EFILE',ecfile)
          CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'EDIR',pathec)
          CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'SPFILE',spfile)
          CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'SPDIR',pathsp)
          ! A-file location
          CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'ADIR',fileadir)
          
          ! Additional controls that not handled by CSM
          ! To get name and location of x-file to -> special controls.
          CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'AFILE',filea)

!     CHP 2021-03-19
          IF (INDEX(FILEADIR,"-99") > 0) THEN
            FILEX = FILEA(1:TVILENT(FILEA))
          ELSE
            FILEX=FILEADIR(1:TVILENT(FILEADIR))//FILEA(1:TVILENT(FILEA))
          ENDIF

          CALL LTRIM2 (FILEX,filenew)
          FILELEN = TVILENT(FILENEW)
          FILENEW(FILELEN:FILELEN)= 'X'
          FILEX = FILENEW
          ! Experimental controls
          CALL XREADC(FILEX,TN,RN,SN,ON,CN,'PHASE',cflphaseadj)
          IF (CFLPHASEADJ.NE.'N'.AND.CFLPHASEADJ.NE.'Y') THEN
           CFLPHASEADJ = 'Y'  ! Default to adjustment 
          ENDIF   
          CALL XREADC(FILEX,TN,RN,SN,ON,CN,'PHINT',cflphintadj)
          IF (CFLPHINTADJ.NE.'N'.AND.CFLPHINTADJ.NE.'Y') THEN
           CFLPHINTADJ = 'N'   ! Default to no adjustment
          ENDIF   
        ENDIF

!-----------------------------------------------------------------------
!       Correct case and dates
!-----------------------------------------------------------------------

        CALL CSUCASE (CROP)
        GENFLCHK = CROP//GENFLCHK(3:15)
        CALL CSUCASE (EXCODE)

C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY
        !HLAST = CSYEARDOY(hlast)
        CALL Y4K_DOY(hlast,FILEX,0,ERRKEY,3)
        !HFIRST = CSYEARDOY(hfirst)
        CALL Y4K_DOY(hfirst,FILEX,0,ERRKEY,3)
        !PWDINF = CSYEARDOY(pwdinf)
        CALL Y4K_DOY(pwdinf,FILEX,0,ERRKEY,3)
        !PWDINL = CSYEARDOY(pwdinl)
        CALL Y4K_DOY(pwdinl,FILEX,0,ERRKEY,3)
        DO L = 1,DINX
          !DIDAT(L) = CSYEARDOY(DIDAT(L))
          CALL Y4K_DOY(DIDAT(L),FILEX,0,ERRKEY,3)
        ENDDO
        DO L = 1,DCNX
          !DCDAT(L) = CSYEARDOY(DCDAT(L))
          CALL Y4K_DOY(DCDAT(L),FILEX,0,ERRKEY,3)
        ENDDO

!        CALL CSYR_DOY(PWDINF,PWYEARF,PWDOYF)
!        CALL CSYR_DOY(PWDINL,PWYEARL,PWDOYL)
!        CALL CSYR_DOY(HFIRST,HYEARF,HDOYF)
!        CALL CSYR_DOY(HLAST,HYEARL,HDOYL)
!        CALL CSYR_DOY(PDATE,PLYEARTMP,PLDAY)
        CALL YR_DOY(PWDINF,PWYEARF,PWDOYF)
        CALL YR_DOY(PWDINL,PWYEARL,PWDOYL)
        CALL YR_DOY(HFIRST,HYEARF,HDOYF)
        CALL YR_DOY(HLAST,HYEARL,HDOYL)
        CALL YR_DOY(PDATE,PLYEARTMP,PLDAY)
        
        PLYEARREAD = PLYEARTMP

!-----------------------------------------------------------------------
!       Insert defaults for missing non-critical aspects
!-----------------------------------------------------------------------

        IF (digfac(1).LT.0.0) digfac(1) = 1.0
        IF (digfac(2).LT.0.0) digfac(2) = 1.0
        IF (digfac(3).LT.0.0) digfac(3) = 1.0
        IF (plmage.LE.-98.0) plmage = 0.0
        IF (hnumber.LE.0) THEN 
          hpcf = 100.0
          hbpcf = 0.0
        ENDIF  
        IF (spnumhfac.LE.0.0) spnumhfac = 0.1

!-----------------------------------------------------------------------
!       Set planting/harvesting dates (Will change if runs repeated)
!-----------------------------------------------------------------------

!         CHP 5/4/09 - for DSSAT runs, always set PLYEAR = YEAR
!         CHP 09/28/2009 account for planting date >> simulation date.
!        LPM 07/17/20 - account for simulation date when is a year before planting date
!        Avoid wrong value of yeardoyharf
        IF (FILEIOT(1:2) == 'DS' .AND. YEAR > PLYEAR) THEN
            IF (YEAR < PLYEARREAD) THEN
                PLYEAR = PLYEARREAD
                PLYEARTMP = PLYEARREAD
            ELSE
                PLYEAR = YEAR
                PLYEARTMP = YEAR
            ENDIF
        ENDIF

!        IF (IDATE1.GT.0.AND.CFLPDATE.EQ.'I') THEN
!          WRITE(FNUMWRK,*)' '
!          WRITE(FNUMWRK,*)
!     &     ' Planting date set at time of first irrigation'
!          WRITE(FNUMWRK,*)'  Planting date    : ',pdate
!          WRITE(FNUMWRK,*)'  Irrigation 1 date: ',idate1
!          PDATE = IDATE1
!        ENDIF

        ! Check final harvest date for seasonal runs        
!        CALL CSYR_DOY(YEARDOYHARF,HYEAR,HDAY)
        CALL YR_DOY(YEARDOYHARF,HYEAR,HDAY)
        PLTOHARYR = HYEAR - PLYEARREAD
        ! Upgrade harvest date for seasonal and sequential runs
        yeardoyharf = (plyear+pltoharyr)*1000 +hday

!       IF (IPLTI.NE.'A') THEN
        IF (IPLTI.NE.'A' .AND. IPLTI.NE.'F') THEN
          IF (PLDAY.GE.DOY) THEN
            PLYEARDOYT = PLYEARTMP*1000 + PLDAY
          ELSEIF (PLDAY.LT.DOY) THEN
            PLYEARDOYT = (YEAR+1)*1000 + PLDAY
          ENDIF
        ELSE
          PLYEARDOYT = 9999999
          IF (PWDINF.GT.0 .AND. PWDINF.LT.YEARDOY) THEN
            TVI1 = INT((YEARDOY-PWDINF)/1000)
            PWDINF = PWDINF + TVI1*1000
            PWDINL = PWDINL + TVI1*1000
            IF (HFIRST.GT.0) HFIRST = HFIRST + TVI1*1000
            IF (HLAST.GT.0)  HLAST  = HLAST + (TVI1+1)*1000
          ENDIF
        ENDIF

!-----------------------------------------------------------------------
!       Create genotype file names
!-----------------------------------------------------------------------

        IF (FILEIOT(1:2).EQ.'DS') THEN
            ! Cultivar
            PATHL = INDEX(PATHCR,BLANK)
            IF (PATHL.LE.5.OR.PATHCR(1:3).EQ.'-99') THEN
              CUDIRFLE = CUFILE
            ELSE
              IF (PATHCR(PATHL-1:PATHL-1) .NE. SLASH) THEN
                CUDIRFLE = PATHCR(1:(PATHL-1)) // SLASH // CUFILE
              ELSE
                CUDIRFLE = PATHCR(1:(PATHL-1)) // CUFILE
              ENDIF
            ENDIF
            ! Ecotype
            PATHL = INDEX(PATHEC,BLANK)
            IF (PATHL.LE.5.OR.PATHEC(1:3).EQ.'-99') THEN
              ECDIRFLE = ECFILE
            ELSE
              IF (PATHEC(PATHL-1:PATHL-1) .NE. SLASH) THEN
                ECDIRFLE = PATHEC(1:(PATHL-1)) // SLASH // ECFILE
              ELSE
                ECDIRFLE = PATHEC(1:(PATHL-1)) // ECFILE
              ENDIF
            ENDIF
            ! Species
            PATHL = INDEX(PATHSP,BLANK)
            IF (PATHL.LE.5.OR.PATHSP(1:3).EQ.'-99') THEN
              SPDIRFLE = SPFILE
            ELSE
              IF (PATHSP(PATHL-1:PATHL-1) .NE. SLASH) THEN
                SPDIRFLE = PATHSP(1:(PATHL-1)) // SLASH // SPFILE
              ELSE
                SPDIRFLE = PATHSP(1:(PATHL-1)) // SPFILE
              ENDIF
            ENDIF
        ELSE
          IF (CUDIRFLE.NE.CUDIRFLPREV .OR. VARNO.NE.VARNOPREV) THEN
            ! Cultivar
            CUFILE = CROP//MODNAME(3:8)//'.CUL'
            INQUIRE (FILE = CUFILE,EXIST = FFLAG)
            IF (FFLAG) THEN
              CUDIRFLE = CUFILE
            ELSE
              CALL FINDDIR (FNUMTMP,CFGDFILE,'CRD',CUFILE,CUDIRFLE)
            ENDIF
            IF (RNMODE.EQ.'G'.OR.RNMODE.EQ.'T') THEN
              CUFILE = 'GENCALC2.CUL'
              CUDIRFLE = ' '
              CUDIRFLE(1:12) = CUFILE
            ENDIF
            ! Ecotype
            ECFILE = CROP//MODNAME(3:8)//'.ECO'
            INQUIRE (FILE = ECFILE,EXIST = FFLAG)
            IF (FFLAG) THEN
              ECDIRFLE = ECFILE
            ELSE
              CALL FINDDIR (FNUMTMP,CFGDFILE,'CRD',ECFILE,ECDIRFLE)
            ENDIF
            IF (RNMODE.EQ.'G'.OR.RNMODE.EQ.'T') THEN
              ECFILE = 'GENCALC2.ECO'
              ECDIRFLE = ' '
              ECDIRFLE(1:12) = ECFILE
            ENDIF
            ! Species
            SPFILE = CROP//MODNAME(3:8)//'.SPE'
            INQUIRE (FILE = SPFILE,EXIST = FFLAG)
            IF (FFLAG) THEN
              SPDIRFLE = SPFILE
            ELSE
              CALL FINDDIR (FNUMTMP,CFGDFILE,'CRD',SPFILE,SPDIRFLE)
            ENDIF
          ENDIF
        ENDIF     ! End Genotype file names creation

!-----------------------------------------------------------------------
!       Check for cultivar number, genotype files existance and version
!-----------------------------------------------------------------------

        IF (VARNO.EQ.'-99   ') THEN
          OPEN (UNIT = FNUMERR,FILE = 'ERROR.OUT')
          WRITE(fnumerr,*)' '
          WRITE(fnumerr,*)'Cultivar number not found '
          WRITE(fnumerr,*)'Maybe an error in the the X-file headings'
          WRITE(fnumerr,*)'(eg.@-line dots connected to next header)'
          WRITE(fnumerr,*)' (OR sequence or crop components > 1,no 1)'
          WRITE(fnumerr,*)'Please check'
          WRITE (*,*) ' Problem reading the X-file'
          WRITE (*,*) ' Cultivar number not found '
          WRITE (*,*) ' Maybe an error in the the X-file headings'
          WRITE (*,*) ' (eg.@-line dots connected to next header)'
          WRITE (*,*) ' (OR sequence or crop components > 1,no 1)'
          WRITE (*,*) ' Program will have to stop'
          WRITE (*,*) ' Check WORK.OUT for details'
          CLOSE (fnumerr)
          STOP ' '
        ENDIF

        INQUIRE (FILE = CUDIRFLE,EXIST = FFLAG)
        IF (.NOT.(FFLAG)) THEN
          ! Following added Sept 2008 for running under VB environment.
!          WRITE(fnumwrk,*) ' '
!          WRITE(fnumwrk,*) 'Cultivar file not found!     '
!          WRITE(fnumwrk,*) 'File sought was:          '  
!          WRITE(fnumwrk,*) Cudirfle(1:78)
!          WRITE(fnumwrk,*) 'Will search in the working directory for:'
          CUDIRFLE = CUFILE
!          WRITE(fnumwrk,*)  Cudirfle(1:78)
          INQUIRE (FILE = CUDIRFLE,EXIST = FFLAG)
          IF (.NOT.(FFLAG)) THEN
            OPEN (UNIT = FNUMERR,FILE = 'ERROR.OUT')
            WRITE(fnumerr,*) ' '
            WRITE(fnumerr,*) 'Cultivar file not found!     '
            WRITE(fnumerr,*) 'File sought was:          '  
            WRITE(fnumerr,*) Cudirfle(1:78)
            WRITE(fnumerr,*) 'Please check'
            WRITE (*,*) ' Cultivar file not found!     '
            WRITE(*,*) 'File sought was:          '
            WRITE(*,*) Cudirfle(1:78)
            WRITE(*,*) ' Program will have to stop'
            CLOSE (fnumerr)
            STOP ' '
          ENDIF
        ENDIF

        INQUIRE (FILE = ECDIRFLE,EXIST = FFLAGEC)
        IF (.NOT.(FFLAGEC)) THEN
          ! Following added Sept 2008 for running under VB environment.
!          WRITE(fnumwrk,*) ' '
!          WRITE(fnumwrk,*) 'Ecotype file not found!     '
!          WRITE(fnumwrk,*) 'File sought was: ',Ecdirfle(1:60)  
          ECDIRFLE = ECFILE
!          WRITE(fnumwrk,*) 
!     &     'Will search in the working directory for:',Ecdirfle(1:60)
          INQUIRE (FILE = ECDIRFLE,EXIST = FFLAGEC)
          IF (.NOT.(FFLAGEC)) THEN
            OPEN (UNIT = FNUMERR,FILE = 'ERROR.OUT')
!            WRITE(fnumwrk,*) 'File not found in working directory!'
!            WRITE(fnumwrk,*) 'Please check'
            WRITE(*,*) ' Ecotype file not found!     '
            WRITE(*,*) ' File sought was: ',Ecdirfle(1:60)
            WRITE(*,*) ' Program will have to stop'
            STOP ' '
          ENDIF
        ENDIF

        INQUIRE (FILE = SPDIRFLE,EXIST = FFLAG)
        IF (.NOT.(FFLAG)) THEN
          OPEN (UNIT = FNUMERR,FILE = 'ERROR.OUT')
          WRITE(fnumerr,*) ' '
          WRITE(fnumerr,*) 'Species file not found!     '
          WRITE(fnumerr,*) 'File sought was:          '
          WRITE(fnumerr,*) Spdirfle
          WRITE(fnumerr,*) 'Please check'
          WRITE(*,*) ' Species file not found!     '
          WRITE(*,*) 'File sought was:          '
          WRITE(*,*) Spdirfle
          WRITE(*,*) ' Program will have to stop'
          CLOSE (fnumerr)
          STOP ' '
        ENDIF
        
!-----------------------------------------------------------------------
!       Read cultivar information
!-----------------------------------------------------------------------

        ! Ecotype coefficients re-set
        gmpch = -99
        htstd = -99
        pps = -99
        awns = -99
        rspca = -99
        ti1lf = -99
        tifac = -99
        tilpe = -99
        tdphs = -99
        tdphe = -99
        gnpcs = -99
        tkfh = -99
        lsphs = -99
        lsphe = -99
        ssphs = -99
        ssphe = -99
        lseni = -99
        phintl = -99
        phintf = -99
        gnorf = -99
        gnort = -99
        gwtaf = -99
        gwtat = -99
        parue = -99
        paru2 = -99
        la1s = -99
        laxs = -99
        laws = -99
        lafv = -99
        lafr = -99
        rdgs = -99
        tdfac = -99
        tdsf = -99
        nfgu = -99
        nfgl = -99
        nupcf = -99
        nupnf = -99
        nupwf = -99
        rtnup = -99
        lsphs = -99
        lsphe = -99

        ! Species coefficients re-set
        pd = -99
        lsphe = -99 
        tdphe = -99 
        tdphs = -99 
        tilpe = -99 
        llifa = -99 
        phintl = -99 
        phintf = -99 
        tdfac = -99 
        rdgs = -99 
        laxs = -99 
        rlwr = -99 
        nfgl = -99 
        nfgu = -99 
        nfpu = -99 
        nfpl = -99 
        kcan = -99 
        lafr = -99 
        lafv = -99 
        lawcf = -99
        pgerm = -99
        pemrg = -99 
        ppfpe = -99 
        ppexp = -99 
        tdsf = -99 
        gwtaf = -99
        gwtat = -99
        gnorf = -99 
        gnort = -99 
        nh4mn = -99
        no3mn = -99
        
        IF (FILEIOT(1:2).EQ.'DS') THEN
          IF (RNMODE.NE.'T') CALL FVCHECK(CUDIRFLE,GENFLCHK)

          CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'ECO#',econo)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'VREQ',vreq)
          IF (VREQ.LT.0.0)
     &     CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'VREQX',vreq)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'VBASE',vbase)
          IF (VBASE.LT.0.0)
     &     CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'VREQN',vbase)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'VEFF',veff)
          IF (VEFF.LT.0.0)
     &     CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'VEFFX',veff)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS1',pps(1))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS2',pps(2))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS3',pps(3))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS4',pps(4))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS5',pps(5))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS6',pps(6))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS7',pps(7))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS8',pps(8))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS9',pps(9))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPEXP',ppexp) ! Trial
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPFPE',ppfpe)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'G#WTS',gnowts)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'GWTS',gwts)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'SHWTS',g3)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PHINT',phints)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P1',pd(1))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P2',pd(2))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P3',pd(3))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P4',pd(4))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P5',pd(5))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P6',pd(6))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P7',pd(7))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P8',pd(8))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P9',pd(9))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P1L',pdl(1))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P2L',pdl(2))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P3L',pdl(3))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P4L',pdl(4))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P5L',pdl(5))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P6L',pdl(6))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P7L',pdl(7))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P8L',pdl(8))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P9L',pdl(9))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LLIFA',llifa)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'STFR',swfrs)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LAXS',laxs)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'SLAS',laws)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFPU',nfpu)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFPL',nfpl)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFGU',nfgu)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFGL',nfgl)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'RDGS',rdgs)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'RLWR',rlwr)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PARUE',parue)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PARU2',paru2)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'TDFAC',tdfac)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'TDSF',tdsf)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'GWTAT',gwtat)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'GWTAF',gwtaf)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'G#RF',gnorf)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'G#RT',gnort)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LA1S',la1s)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LAFV',lafv)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LAFR',lafr)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PHL2',phintl(2))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PHF3',phintf(3))
          ! New (Nov 2011) N uptake variables
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NUPNF',nupnf)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NUPWF',nupwf)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'RTNUP',rtnup)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LSPHS',lsphs)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LSPHE',lsphe)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NO3MN',no3mn)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NH4MN',nh4mn)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PGERM',pgerm)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PEMRG',pemrg)
        ELSE
          IF (RNMODE.NE.'T') CALL FVCHECK(CUDIRFLE,GENFLCHK)
          CALL CUREADC (CUDIRFLE,VARNO,'ECO#',econo)
          CALL CUREADR (CUDIRFLE,VARNO,'VREQ',vreq)
          IF (VREQ.LT.0.0)CALL CUREADR (CUDIRFLE,VARNO,'VREQX',vreq)
          CALL CUREADR (CUDIRFLE,VARNO,'VBASE',vbase)
          IF (VBASE.LT.0.0)CALL CUREADR (CUDIRFLE,VARNO,'VREQN',vbase)
          CALL CUREADR (CUDIRFLE,VARNO,'VEFF',veff)
          IF (VEFF.LT.0.0)CALL CUREADR (CUDIRFLE,VARNO,'VEFFX',veff)
          CALL CUREADR (CUDIRFLE,VARNO,'PPS1',pps(1))
          CALL CUREADR (CUDIRFLE,VARNO,'PPS2',pps(2))
          CALL CUREADR (CUDIRFLE,VARNO,'PPS3',pps(3))
          CALL CUREADR (CUDIRFLE,VARNO,'PPS4',pps(4))
          CALL CUREADR (CUDIRFLE,VARNO,'PPS5',pps(5))
          CALL CUREADR (CUDIRFLE,VARNO,'PPS6',pps(6))
          CALL CUREADR (CUDIRFLE,VARNO,'PPS7',pps(7))
          CALL CUREADR (CUDIRFLE,VARNO,'PPS8',pps(8))
          CALL CUREADR (CUDIRFLE,VARNO,'PPS9',pps(9))
          CALL CUREADR (CUDIRFLE,VARNO,'PPEXP',ppexp)! Trial
          CALL CUREADR (CUDIRFLE,VARNO,'PPFPE',ppfpe)
          CALL CUREADR (CUDIRFLE,VARNO,'P1',pd(1))
          CALL CUREADR (CUDIRFLE,VARNO,'P2',pd(2))
          CALL CUREADR (CUDIRFLE,VARNO,'P3',pd(3))
          CALL CUREADR (CUDIRFLE,VARNO,'P4',pd(4))
          CALL CUREADR (CUDIRFLE,VARNO,'P5',pd(5))
          CALL CUREADR (CUDIRFLE,VARNO,'P6',pd(6))
          CALL CUREADR (CUDIRFLE,VARNO,'P7',pd(7))
          CALL CUREADR (CUDIRFLE,VARNO,'P8',pd(8))
          CALL CUREADR (CUDIRFLE,VARNO,'P9',pd(9))
          CALL CUREADR (CUDIRFLE,VARNO,'P1L',pdl(1))
          CALL CUREADR (CUDIRFLE,VARNO,'P2L',pdl(2))
          CALL CUREADR (CUDIRFLE,VARNO,'P3L',pdl(3))
          CALL CUREADR (CUDIRFLE,VARNO,'P4L',pdl(4))
          CALL CUREADR (CUDIRFLE,VARNO,'P5L',pdl(5))
          CALL CUREADR (CUDIRFLE,VARNO,'P6L',pdl(6))
          CALL CUREADR (CUDIRFLE,VARNO,'P7L',pdl(7))
          CALL CUREADR (CUDIRFLE,VARNO,'P8L',pdl(8))
          CALL CUREADR (CUDIRFLE,VARNO,'P9L',pdl(9))
          CALL CUREADR (CUDIRFLE,VARNO,'G#WTS',gnowts)
          CALL CUREADR (CUDIRFLE,VARNO,'GWTS',gwts)
          CALL CUREADR (CUDIRFLE,VARNO,'SHWTS',g3)
          CALL CUREADR (CUDIRFLE,VARNO,'PHINT',phints)
          CALL CUREADR (CUDIRFLE,VARNO,'LLIFA',llifa)
          CALL CUREADR (CUDIRFLE,VARNO,'STFR',swfrs)
          CALL CUREADR (CUDIRFLE,VARNO,'LAXS',laxs)
          CALL CUREADR (CUDIRFLE,VARNO,'SLAS',laws)
          CALL CUREADR (CUDIRFLE,VARNO,'NFPU',nfpu)
          CALL CUREADR (CUDIRFLE,VARNO,'NFPL',nfpl)
          CALL CUREADR (CUDIRFLE,VARNO,'NFGU',nfgu)
          CALL CUREADR (CUDIRFLE,VARNO,'NFGL',nfgl)
          CALL CUREADR (CUDIRFLE,VARNO,'RDGS',rdgs)
          CALL CUREADR (CUDIRFLE,VARNO,'RLWR',rlwr)
          CALL CUREADR (CUDIRFLE,VARNO,'PARUE',parue)
          CALL CUREADR (CUDIRFLE,VARNO,'PARU2',paru2)
          CALL CUREADR (CUDIRFLE,VARNO,'TDFAC',tdfac)
          CALL CUREADR (CUDIRFLE,VARNO,'TDSF',tdsf)
          CALL CUREADR (CUDIRFLE,VARNO,'GWTAT',gwtat)
          CALL CUREADR (CUDIRFLE,VARNO,'GWTAF',gwtaf)
          CALL CUREADR (CUDIRFLE,VARNO,'G#RF',gnorf)
          CALL CUREADR (CUDIRFLE,VARNO,'G#RT',gnort)
          CALL CUREADR (CUDIRFLE,VARNO,'LA1S',la1s)
          CALL CUREADR (CUDIRFLE,VARNO,'LAFV',lafv)
          CALL CUREADR (CUDIRFLE,VARNO,'LAFR',lafr)
          CALL CUREADR (CUDIRFLE,VARNO,'PHL2',phintl(2))
          CALL CUREADR (CUDIRFLE,VARNO,'PHF3',phintf(3))
          CALL CUREADR (CUDIRFLE,VARNO,'LSHPS',lsphs)
          CALL CUREADR (CUDIRFLE,VARNO,'LSPHE',lsphe)
          ! New (Nov 2011) N uptake variables
          CALL CUREADR (CUDIRFLE,VARNO,'NUPNF',nupnf)
          CALL CUREADR (CUDIRFLE,VARNO,'NUPWF',nupwf)
          CALL CUREADR (CUDIRFLE,VARNO,'RTNUP',rtnup)
          CALL CUREADR (CUDIRFLE,VARNO,'NO3MN',no3mn)
          CALL CUREADR (CUDIRFLE,VARNO,'NH4MN',nh4mn)
          CALL CUREADR (CUDIRFLE,VARNO,'PGERM',pgerm)
          CALL CUREADR (CUDIRFLE,VARNO,'PEMRG',pemrg)
        ENDIF     ! End Cultivar reads

!-----------------------------------------------------------------------
!       Read ecotype information
!-----------------------------------------------------------------------

        IF (RNMODE.NE.'T') CALL FVCHECK(ECDIRFLE,GENFLCHK)
        CALL ECREADR (ECDIRFLE,ECONO,'HTSTD',htstd)
        CALL ECREADR (ECDIRFLE,ECONO,'AWNS',awns)
        CALL ECREADR (ECDIRFLE,ECONO,'RS%A',rspca)
        CALL ECREADR (ECDIRFLE,ECONO,'TIL#S',ti1lf)
        IF (TI1LF.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'TIPHS',ti1lf)
        IF (PPS(2).LT.0.0) 
     &   CALL ECREADR (ECDIRFLE,ECONO,'PPS2',pps(2))
        CALL ECREADR (ECDIRFLE,ECONO,'TIFAC',tifac)
        CALL ECREADR (ECDIRFLE,ECONO,'TILPE',tilpe)
        CALL ECREADR (ECDIRFLE,ECONO,'TDPHS',tdphs)
        CALL ECREADR (ECDIRFLE,ECONO,'TDPHE',tdphe)
        CALL ECREADR (ECDIRFLE,ECONO,'GN%MN',gnpcmn)
        CALL ECREADR (ECDIRFLE,ECONO,'GN%S',gnpcs)
        CALL ECREADR (ECDIRFLE,ECONO,'TKFH',tkfh)
        IF (LSPHS.LE.0) CALL ECREADR (ECDIRFLE,ECONO,'LSPHS',lsphs)
        IF (LSPHE.LE.0) CALL ECREADR (ECDIRFLE,ECONO,'LSPHE',lsphe)
        CALL ECREADR (ECDIRFLE,ECONO,'SSPHS',ssphs)
        CALL ECREADR (ECDIRFLE,ECONO,'SSPHE',ssphe)
        CALL ECREADR (ECDIRFLE,ECONO,'LSENI',lseni)
        IF (PHINTL(1).LE.0) 
     &   CALL ECREADR(ECDIRFLE,ECONO,'PHL1',phintl(1))
        IF (PHINTL(2).LE.0) 
     &   CALL ECREADR(ECDIRFLE,ECONO,'PHL2',phintl(2))
        IF (PHINTF(2).LE.0)
     &   CALL ECREADR(ECDIRFLE,ECONO,'PHF2',phintf(2))
        IF (PHINTF(3).LE.0)
     &   CALL ECREADR(ECDIRFLE,ECONO,'PHF3',phintf(3))
        ! LAH Following set up to allow for change in stem fraction
        ! Currently not used ... just one stem fraction (STFR)
        CALL ECREADR (ECDIRFLE,ECONO,'SWFRX',swfrx)
        CALL ECREADR (ECDIRFLE,ECONO,'SWFRN',swfrn)
        CALL ECREADR (ECDIRFLE,ECONO,'SWFNL',swfrnl)
        CALL ECREADR (ECDIRFLE,ECONO,'SWFXL',swfrxl)
        CALL ECREADR (ECDIRFLE,ECONO,'SLACF',lawcf)
        CALL ECREADR (ECDIRFLE,ECONO,'KCAN',kcan)
        ! Following may have been (temporarily) in the CUL file
        ! Grains
        IF (GMPCH.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'GM%H',gmpch)
        IF (GNORF.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'G#RF',gnorf)
        IF (GNORT.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'G#RT',gnort)
        IF (GWTAT.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'GWTAT',gwtat)
        IF (GWTAF.LT.-10.0) CALL ECREADR (ECDIRFLE,ECONO,'GWTAF',gwtaf)
        ! Radiation use efficiency
        IF (PARUE.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'PARUE',parue)
        IF (PARU2.LT.-89.0) CALL ECREADR (ECDIRFLE,ECONO,'PARU2',paru2)
        ! Leaf area
        IF (LA1S.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'LA1S',la1s)
        IF (LAXS.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'LAXS',laxs)
        IF (LAWS.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'SLAS',laws)
        IF (LAFV.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'LAFV',lafv)
        IF (LAFR.LT.-90.0) CALL ECREADR (ECDIRFLE,ECONO,'LAFR',lafr)
        ! Roots
        IF (RDGS.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'RDGS',rdgs)
        ! Tillers
        IF (TDFAC.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'TDFAC',tdfac)
        IF (TDSF.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'TDSF',tdsf)
        ! Reduction factors
        IF (NFGU.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'NFGU',nfgu)
        IF (NFGL.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'NFGL',nfgl)
        ! N uptake
        IF (NUPNF.LT.-90.0) CALL ECREADR (ECDIRFLE,ECONO,'NUPNF',nupnf)
        IF (NUPWF.LT.-90.0) CALL ECREADR (ECDIRFLE,ECONO,'NUPWF',nupwf)
        IF (NUPCF.LT.-90.0) CALL ECREADR (ECDIRFLE,ECONO,'NUPCF',nupcf)
        IF (RTNUP.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'RTNUP',rtnup)
        IF (NO3MN.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'NO3MN',no3mn)
        IF (NH4MN.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'NH4MN',nh4mn)
        
!-----------------------------------------------------------------------
!       Read species information
!-----------------------------------------------------------------------

        IF (RNMODE.NE.'T') CALL FVCHECK(SPDIRFLE,GENFLCHK)
        CALL SPREADR (SPDIRFLE,'CHFR' ,chfr)
        CALL SPREADR (SPDIRFLE,'CO2CC',co2compc)
        CALL SPREADR (SPDIRFLE,'CO2EX',co2ex)
        CALL SPREADR (SPDIRFLE,'GLIG%',gligpc)
        CALL SPREADR (SPDIRFLE,'GN%MX',gnpcmx)
        CALL SPREADR (SPDIRFLE,'GWLAG',gwlagfr)
        CALL SPREADR (SPDIRFLE,'GWLIN',gwlinfr)
        CALL SPREADR (SPDIRFLE,'HDUR' ,hdur)
        CALL SPREADR (SPDIRFLE,'HLOSF',hlosf)
        CALL SPREADR (SPDIRFLE,'HLOST',hlost)
        CALL SPREADR (SPDIRFLE,'LAFST',lafst)
        CALL SPREADR (SPDIRFLE,'SLAFF',lawff)
        CALL SPREADR (SPDIRFLE,'SLATR',lawtr)
        CALL SPREADR (SPDIRFLE,'SLATS',lawts)
        CALL SPREADR (SPDIRFLE,'SLAWR',lawwr)
        CALL SPREADR (SPDIRFLE,'LLIFG',LLIFG)
        CALL SPREADR (SPDIRFLE,'LLIFS',llifs)
        CALL SPREADR (SPDIRFLE,'LLIG%',lligpc)
        CALL SPREADR (SPDIRFLE,'LLOSA',llosa)
        CALL SPREADR (SPDIRFLE,'LWLOS',lwlos)
        CALL SPREADR (SPDIRFLE,'NFSU' ,nfsu)
        CALL SPREADR (SPDIRFLE,'NFSF' ,nfsf)
        CALL SPREADR (SPDIRFLE,'NFTL ',nftl)
        CALL SPREADR (SPDIRFLE,'NFTU ',nftu)
        CALL SPREADR (SPDIRFLE,'LSHAR',lshar)
        CALL SPREADR (SPDIRFLE,'LSHAV',lshav)
        CALL SPREADR (SPDIRFLE,'LSHFR',lshfr)
        CALL SPREADR (SPDIRFLE,'NCRG',ncrg)
        CALL SPREADR (SPDIRFLE,'NTUPF',ntupf)
        CALL SPREADR (SPDIRFLE,'PARIX',parix)
        CALL SPREADR (SPDIRFLE,'LAIXX',laixx)
        CALL SPREADR (SPDIRFLE,'PARFC',parfc)
        IF (PEMRG.LE.0.0) CALL SPREADR (SPDIRFLE,'PEMRG',pemrg)
        IF (PGERM.LE.0.0) CALL SPREADR (SPDIRFLE,'PGERM',pgerm)
        CALL SPREADR (SPDIRFLE,'PDMH' ,pdmtohar)
        CALL SPREADR (SPDIRFLE,'PHSV' ,phsv)
        CALL SPREADR (SPDIRFLE,'PHTV' ,phtv)
        CALL SPREADR (SPDIRFLE,'PPTHR',ppthr)
        CALL SPREADR (SPDIRFLE,'PTFXS',ptfxs)
        CALL SPREADR (SPDIRFLE,'PTFA' ,ptfa)
        CALL SPREADR (SPDIRFLE,'PTFMN',ptfmn)
        CALL SPREADR (SPDIRFLE,'PTFMX',ptfmx)
        CALL SPREADR (SPDIRFLE,'RATM' ,ratm)
        CALL SPREADR (SPDIRFLE,'RCROP',rcrop)
        CALL SPREADR (SPDIRFLE,'EORAT',eoratio)
        CALL SPREADR (SPDIRFLE,'RDGAF',rdgaf)
        CALL SPREADR (SPDIRFLE,'RLIG%',rligpc)
        !CALL SPREADR (SPDIRFLE,'RNUMX',rnumx) ! Taken out of spp file
        CALL SPREADR (SPDIRFLE,'RRESP',rresp)
        CALL SPREADR (SPDIRFLE,'RS%LX',rspclx)
        CALL SPREADR (SPDIRFLE,'RS%X' ,rspcx)
        CALL SPREADR (SPDIRFLE,'RSEN',rsen)
        IF (RSEN.LT.0.0) CALL SPREADR (SPDIRFLE,'RSEN%',rsen)
        CALL SPREADR (SPDIRFLE,'RSFPL',rsfpl)
        CALL SPREADR (SPDIRFLE,'RSFPU',rsfpu)
        CALL SPREADR (SPDIRFLE,'RSUSE',rsuse)
        CALL SPREADR (SPDIRFLE,'RTUFR',rtufr)
        CALL SPREADR (SPDIRFLE,'RUESG',ruestg)
        CALL SPREADR (SPDIRFLE,'RWUMX',rwumx)
        CALL SPREADR (SPDIRFLE,'RWUPM',rwupm)
        CALL SPREADR (SPDIRFLE,'SAWS' ,saws)
        CALL SPREADR (SPDIRFLE,'SDDUR',sddur)
        CALL SPREADR (SPDIRFLE,'SDN%',sdnpci)
        CALL SPREADR (SPDIRFLE,'SDRS%',sdrspc)
        CALL SPREADR (SPDIRFLE,'SDWT' ,sdwt)
        CALL SPREADR (SPDIRFLE,'SLIG%',sligpc)
        CALL SPREADR (SPDIRFLE,'TGR02',tgr(2))
        CALL SPREADR (SPDIRFLE,'TGR20',tgr(20))
        CALL SPREADR (SPDIRFLE,'TILIP',tilip)
        CALL SPREADR (SPDIRFLE,'TIL#X',tilnox)
        CALL SPREADR (SPDIRFLE,'TKDLF',tkdlf)
        CALL SPREADR (SPDIRFLE,'TKSPN',tkspn)
        CALL SPREADR (SPDIRFLE,'TKDTI',tkdti)
        CALL SPREADR (SPDIRFLE,'TKUH' ,tkuh)
        CALL SPREADR (SPDIRFLE,'TKGF' ,tkgf)
        CALL SPREADR (SPDIRFLE,'TPAR' ,tpar)
        CALL SPREADR (SPDIRFLE,'TSRAD',tsrad)
        CALL SPREADR (SPDIRFLE,'VEEND',veend)
        CALL SPREADR (SPDIRFLE,'VLOSF',vlosf)
        CALL SPREADR (SPDIRFLE,'VLOSS',vloss)
        CALL SPREADR (SPDIRFLE,'VLOST',vlost)
        CALL SPREADR (SPDIRFLE,'VPEND',vpend)
        CALL SPREADR (SPDIRFLE,'WFEU' ,wfeu)
        CALL SPREADR (SPDIRFLE,'WFGEU',wfgeu)
        CALL SPREADR (SPDIRFLE,'WFGU' ,wfgu)
        CALL SPREADR (SPDIRFLE,'WFGL' ,wfgl)
        CALL SPREADR (SPDIRFLE,'WFPU' ,wfpu)
        CALL SPREADR (SPDIRFLE,'WFPL' ,wfpl)
        CALL SPREADR (SPDIRFLE,'WFRGU',wfrgu)
        CALL SPREADR (SPDIRFLE,'WFSU' ,wfsu)
        CALL SPREADR (SPDIRFLE,'WFSF' ,wfsf)
        CALL SPREADR (SPDIRFLE,'WFTL' ,wftl)
        CALL SPREADR (SPDIRFLE,'WFTU' ,wftu)
        CALL SPREADR (SPDIRFLE,'NLAB%',nlabpc)
        ! LAH Following set up to allow for change in stem fraction
        ! Currently not used ... just one stem fraction (STFR)
        IF (SWFRN.LE.0.0) CALL SPREADR (SPDIRFLE,'SWFRN',swfrn)
        IF (SWFRNL.LE.0.0) CALL SPREADR (SPDIRFLE,'SWFRNL',swfrnl)
        IF (SWFRXL.LE.0.0) CALL SPREADR (SPDIRFLE,'SWFXL',swfrxl)
        IF (SWFRX.LE.0.0) CALL SPREADR (SPDIRFLE,'SWFRX',swfrx)
        ! Following may be temporarily in ECO or CUL file
        IF (PD(9).LE.0.0) CALL SPREADR (SPDIRFLE,'P9',pd(9))
        IF (lsphe.LE.0.0) CALL SPREADR (SPDIRFLE,'LSPHE',lsphe)
        IF (tdphe.LE.0.0) CALL SPREADR (SPDIRFLE,'TDPHE',tdphe)
        IF (tdphs.LE.0.0) CALL SPREADR (SPDIRFLE,'TDPHS',tdphs)
        IF (tilpe.LE.0.0) CALL SPREADR (SPDIRFLE,'TILPE',tilpe)
        IF (LLIFA.LE.0.0) CALL SPREADR (SPDIRFLE,'LLIFA',llifa)

        IF (PHINTL(1).LE.0) CALL SPREADR (SPDIRFLE,'PHL1',phintl(1))
        IF (PHINTF(1).LE.0) CALL SPREADR (SPDIRFLE,'PHF1',phintf(1))
        IF (PHINTL(2).LE.0) CALL SPREADR (SPDIRFLE,'PHL2',phintl(2))
        IF (PHINTF(2).LE.0) CALL SPREADR (SPDIRFLE,'PHF2',phintf(2))
        IF (PHINTF(3).LE.0) CALL SPREADR (SPDIRFLE,'PHF3',phintf(3))

        IF (TDFAC.LE.0.0) CALL SPREADR (SPDIRFLE,'TDFAC',tdfac)
        IF (RDGS.LE.0.0) CALL SPREADR (SPDIRFLE,'RDGS',rdgs)
        IF (LAXS.LE.0.0) CALL SPREADR (SPDIRFLE,'LAXS',laxs)
        IF (RLWR.LE.0.0) CALL SPREADR (SPDIRFLE,'RLWR',rlwr)
        IF (NFGL.LT.0.0) CALL SPREADR (SPDIRFLE,'NFGL',nfgl)
        IF (NFGU.LE.0.0) CALL SPREADR (SPDIRFLE,'NFGU',nfgu)
        IF (NFPU.LE.0.0) CALL SPREADR (SPDIRFLE,'NFPU',nfpu)
        IF (NFPL.LE.0.0) CALL SPREADR (SPDIRFLE,'NFPL',nfpl)
        IF (KCAN.LE.0.0) CALL SPREADR (SPDIRFLE,'KCAN',kcan)
        IF (LAFR.LE.-90.0) CALL SPREADR (SPDIRFLE,'LAFR',lafr)
        IF (LAFV.LE.0.0) CALL SPREADR (SPDIRFLE,'LAFV',lafv)
        IF (LAWCF.LE.0.0) CALL SPREADR (SPDIRFLE,'SLACF',lawcf)
        IF (PPFPE.LT.0.0) CALL SPREADR (SPDIRFLE,'PPFPE',ppfpe)
        IF (PPEXP.LT.0.0) CALL SPREADR (SPDIRFLE,'PPEXP',ppexp)
        IF (TDSF.LT.0.0) CALL SPREADR (SPDIRFLE,'TDSF',tdsf)
        ! Grain coefficients
        IF (GWTAF.LT.-10.0) CALL SPREADR (SPDIRFLE,'GWTAF',gwtaf)
        IF (GWTAT.LT.0.0) CALL SPREADR (SPDIRFLE,'GWTAT',gwtat)
        IF (GNORF.LT.0.0) CALL SPREADR (SPDIRFLE,'G#RF',gnorf)
        IF (GNORT.LT.0.0) CALL SPREADR (SPDIRFLE,'G#RT',gnort)
        ! N uptake 
        IF (NUPNF.LT.-90.0) CALL SPREADR (SPDIRFLE,'NUPNF',nupnf)
        IF (NUPWF.LT.-90.0) CALL SPREADR (SPDIRFLE,'NUPWF',nupwf)
        IF (RTNUP.LT.0.0) CALL SPREADR (SPDIRFLE,'RTNUP',rtnup)
        IF (NO3MN.LT.0.0) CALL SPREADR (SPDIRFLE,'NO3MN',no3mn)
        IF (NH4MN.LT.0.0) CALL SPREADR (SPDIRFLE,'NH4MN',nh4mn)

        CALL SPREADC (SPDIRFLE,'PPSEN',ppsen)

        CALL SPREADRA (SPDIRFLE,'LN%S','2',lnpcs)
        CALL SPREADRA (SPDIRFLE,'RN%S','2',rnpcs)
        CALL SPREADRA (SPDIRFLE,'SN%S','2',snpcs)
        CALL SPREADRA (SPDIRFLE,'LN%MN','2',lnpcmn)
        CALL SPREADRA (SPDIRFLE,'RN%MN','2',rnpcmn)
        CALL SPREADRA (SPDIRFLE,'SN%MN','2',snpcmn)

        CALL SPREADRA (SPDIRFLE,'CHT%','10',chtpc)
        CALL SPREADRA (SPDIRFLE,'CLA%','10',clapc)

        CALL SPREADRA (SPDIRFLE,'CO2RF','10',co2rf)
        CALL SPREADRA (SPDIRFLE,'CO2F','10',co2f)
        
!         Temperature responses
!        RRATE TRGEM TRDV1 TRDV4 TRDV8 TRLFG TRPHS TRVRN TRHAR TRGFW TRGFN  
!            0     1     0     0     0     0     0    -5    -5     0     0  
!          1.0    26    26    26    30    10     5     0     0    16    16  
!          1.0    50    50    50    50    20    25     7     5    35    35
!            0    60    60    60    60    35    35    15    10    45    45
        CALL SPREADRA (SPDIRFLE,'TRDV1','4',trdv1)
        IF (trdv1(1).LT.-98.0) THEN
          OPEN (UNIT = FNUMERR,FILE = 'ERROR.OUT')
          WRITE(fnumerr,*) ' '
          WRITE(fnumerr,*) ' No temp response data for phase 1'
          WRITE(fnumerr,*) ' Please check'
          WRITE(*,*) ' No temperature response data for phase 1'
          WRITE(*,*) ' Program will have to stop'
          CLOSE (fnumerr)
          STOP ' '
        ENDIF        
        CALL SPREADRA (SPDIRFLE,'TRDV2','4',trdv2)
        IF (trdv2(1).LT.-98.0) TRDV2 = TRDV1
        CALL SPREADRA (SPDIRFLE,'TRDV3','4',trdv3)
        IF (trdv3(1).LT.-98.0) TRDV3 = TRDV2
        CALL SPREADRA (SPDIRFLE,'TRDV4','4',trdv4)
        IF (trdv4(1).LT.-98.0) TRDV4 = TRDV3
        CALL SPREADRA (SPDIRFLE,'TRDV5','4',trdv5)
        IF (trdv5(1).LT.-98.0) TRDV5 = TRDV4
        CALL SPREADRA (SPDIRFLE,'TRDV6','4',trdv6)
        IF (trdv6(1).LT.-98.0) TRDV6 = TRDV5
        CALL SPREADRA (SPDIRFLE,'TRDV7','4',trdv7)
        IF (trdv7(1).LT.-98.0) TRDV7 = TRDV6
        CALL SPREADRA (SPDIRFLE,'TRDV8','4',trdv8)
        IF (trdv8(1).LT.-98.0) TRDV8 = TRDV7

        CALL SPREADRA (SPDIRFLE,'TRGEM','4',trgem)
        CALL SPREADRA (SPDIRFLE,'TRGFW','4',trgfc)
        CALL SPREADRA (SPDIRFLE,'TRGFN','4',trgfn)
        CALL SPREADRA (SPDIRFLE,'TRLFG','4',trlfg)
        CALL SPREADRA (SPDIRFLE,'TRHAR','4',trcoh)
        CALL SPREADRA (SPDIRFLE,'TRPHS','4',trphs)
        CALL SPREADRA (SPDIRFLE,'TRVRN','4',trvrn)
        IF (diffacr(1).LT.0.0)
     &   CALL SPREADRA (SPDIRFLE,'DIFFR','3',diffacr)

        CALL SPREADCA (SPDIRFLE,'PSNAME','20',psname)
        CALL SPREADCA (SPDIRFLE,'SSNAME','20',ssname)
        CALL SPREADCA (SPDIRFLE,'PSABV','20',psabv)
        CALL SPREADCA (SPDIRFLE,'SSABV','20',ssabv)
        CALL SPREADCA (SPDIRFLE,'PSTYP','20',pstyp)
        CALL SPREADCA (SPDIRFLE,'SSTYP','20',sstyp)
        CALL SPREADRA (SPDIRFLE,'SSTG','20',sstg)

        ! The following are to allow examination of the functioning of 
        ! different parts of the module, and comparison with CSCER     
        
        ! Commented acronyms are those used in the genotype files
       
        ! Set which sections to be examined
        EXAMINE(1)  = 'N'    ! Basic leaf and root growth
        EXAMINE(2)  = 'N'    ! Leaf senescencs
        EXAMINE(3)  = 'N'    ! Tillering
        EXAMINE(4)  = 'N'    ! Reproductive development
        EXAMINE(20) = 'N'    ! Everything from all genotype files
        
        IF (EXAMINE(1).EQ.'Y') THEN                                     

!          WRITE(fnumwrk,*)' '
!          WRITE(fnumwrk,*)' RUNNING EXAMINE '

              ! CSCRP
              PGERM = 10.0      
              PEMRG =  8.0      

              Pd(0) = 0.0       
              PARUE = 2.7       
              PARU2 = 2.7       
              KCAN = 0.85       
              PTFMN = 0.75                                              
              PTFMX = 0.98      
              PTFA = 0.00                                              
              PTFMX = 0.75 ! From 0.98
              PTFXS = 6.0 
          
          
          
          
          
          
          

              PPFPE = 1.0   
              PPS(1)= 97.0
              VREQ = 10000.0
              VBASE = 0.0                                               
              VEFF = 1.0    
              TI1LF = 200.0 
              LLIFA = 200.0 
              LLIFG = 1.0                                              
              LLIFS = 3.0                                              
              TKDLF = 2.0    
              TKUH = -50.0  
              TKFH = -90.0  
              TKSPN = 2.0                                              
              HDUR = 10.0   
              HLOST = 50.0                                             
              HLOSF = 0.2                                               
          
          
          
          
          
          
          
          
          
          LA1S = 5.0        
          LAFV = 0.1        
          LAFR = 0.5        
          PHINT = 95.0
          PHINTL(1) =2.0!PHL
          PHINTF(1) =0.8!PHF
          PHINTL(2) = 150    
          PHINTF(3) = 1.8   
          LAWS = 400        
          LAWCF = 0.02      
          
          LSHFR = 0.33      
          
          LSHAV =50.0    
          LSHAR = 50.0                                         
          RSPCX = 80.0
          RSPCA = 15.0      

          RRESP = 0.40 ! From 0.8
          RSEN = 0.1
          RLWR = 0.90 

          SDDUR = 5.0

          RTNUP = .006      
          NUPCF = 0.0          
          NUPNF = 0.0          
          NUPWF = 0.0       
          NH4MN = 0.5  
          NO3MN = 0.5  
          NTUPF = 0.05 
          LNPCS(0) = 6.5                             
          LNPCS(1) = 6.5                             
          SNPCS(0) = 1.5                              
          SNPCS(1) = 1.5                            
          RNPCS(0) = 2.0                             
          RNPCS(1) = 2.0                             
          LNPCMN(0) = 0.8                            
          LNPCMN(1) = 0.8                            
          SNPCMN(0) = 0.5                            
          SNPCMN(1) = 0.5                            
          RNPCMN(0) = 1.5                            
          RNPCMN(1) = 1.5                            
          
        ENDIF    ! EXAMINE(1)  Basic growth
        
        IF (EXAMINE(2).EQ.'Y') THEN                                    
          ! Leaf senescence 
          LLIFA = 4.0     
          LLIFG = 1.0
          LLIFS = 3.0
          LWLOS = 0.5        
          ! LNPCMN  Determines N loss
       ENDIF  ! EXAMINE = 2  Senescence
        
        IF (EXAMINE(3).EQ.'Y') THEN                                    
          ! Tillering        
          TI1LF = 3.5 ! 4.45  
          TIFAC = 1.0   
          TDPHS = 3.0   
          TDPHE = 7.0   
          TDFAC = 5.0   
          TGR(2) = 0.80
          TGR(20) = 0.10                                                
          TILIP = 6.0                                                  
          TILNOX = 20                                          
          TDSF = 1.0                                                   
        ENDIF    ! EXAMINE(3)  Tillering

        IF (EXAMINE(4).EQ.'Y') THEN                                    
          ! Reproductive development
          ! CSCRP           CSCER
          VREQ = 8.0            !  P1V =    8.0           
          PPS(1) = 97.0         !  P1D =     97.0         
          PD(8) = 500.0         !  PD(5) =   500          
          PD(1) = 200.0 !380    !  PD(1) = 200            
          PD(2) = 50.0  ! 70    !  PD(2) = 200            
          PD(3) = 150.0 !200    !  PD(3) = 200            
          PD(4) = 200.0 !200    !  PD(4) = 200            
                        !       !  PD2FR(1) = 0.25        
                        !       !  PD4FR(1) = 0.25        
                        !       !  PD4FR(2) = 0.10        
          PD(5) = 50.0  ! 60                                            
          PD(6) = 20.0  ! 25                                            
          PD(7) = 130.0 !150
          
          ! PHASE DURATIONS FROM CERES RUN                                      
          !  CERES INPUTS     =  200.0 200.0 200.0 200.0 500.0
          !  CERES from PHINT =  400.0 285.0 190.0 200.0 500.0
          !  ZADOKS FROM CERES INPUTS AND PHINT  
          !   P1   ->TS  2 200.0  400.0
          !   P2   ->Jt  3  50.0   71.2
          !   P3  TS->LL 4 150.0  213.8
          !   P4  LL->SE 5 200.0  190.0
          !   P5 ESG->AN 6  50.0   50.0
          !   P6  AN->EA 7  20.0   20.0
          !   P7  EA->EL 8 130.0  130.0
          !   P8  EL->PM 9 500.0  500.0

!           PSNO PSNAME                        ! PSNO PSTYP PSABV PSNAME                                          
!             1  T.Spikelet                    !   1      S GDAT  Germinate                                       
!             2  EndVegetative                 !   2      K TSAT  T.Spikelet                                      
!             3  EndEarGrowth                  !   3      S PSDAT Pseudo_Stem                                     
!             4  BeginGrainFill                !   4      S LLDAT End_Leaf                                        
!             5  EndGrainFill                  !   5      S IEDAT Head_Emerge                                     
!             6  Harvest                       !   6      K ADAT  Anthesis                                        
!             7  Sowing                        !   7      S AEDAT EndAnthesis                                    
!             8  Germinate                     !   8      S GFDAT MilkToDough                                     
!             9  Emergance                     !   9      M MDAT  HardDough                                       
          
        ENDIF   ! Reproductive development
        
!         The 'Examine' section is to alllow standardisation of coefficients for
!         comparison with CERES. 
        IF (EXAMINE(20).EQ.'Y') THEN  
          ! Everything from all genotype files    
!          WRITE(fnumwrk,*)' '
!          WRITE(fnumwrk,*)' RUNNING EXAMINE. CODE = 2'
!          WRITE(fnumwrk,*)' '
        
          ! CULTIVAR:
          VREQ = 8.0
          PPS(1) = 100.0
          PD(8) = 500.0
          GNOWTS = 25.0
          GWTS = 40.0
          G3 = 2.5
          PHINT = 80
          PD(1) = 380
          PD(2) =  70
          PD(3) = 200
          PD(4) = 200 
          PD(5) =  60
          PD(6) =  25
          PD(7) = 150 
          ! PSNO PSTYP PSABV PSNAME
          !   1      S GDAT  Germinate
          !   2      K TSAT  T.Spikelet
          !   3      S PSDAT Pseudo_Stem
          !   4      S LLDAT End_Leaf
          !   5      S IEDAT Head_Emerge
          !   6      K ADAT  Anthesis
          !   7      S AEDAT End_Anthesis
          !   8      S GFDAT MilkToDough
          !   9      M MDAT  HardDough
          LA1S = 3.0
          LAFV = 0.1
          LAFR = 0.5
          VBASE = 0.0
          VEFF = 1.0
          PPS(2) = 0.0
          
       ! ECOTYPE:
          PARUE = 2.7
          PARU2 = 2.7
          PHINTL(2) = 12
          PHINTF(3) = 1.3
          LAWS = 400
          LSENI = 1.5
          LSPHS = 8.0
          LSPHE = 9.3
          TI1LF = 4.45
          TIFAC = 1.0
          TDPHS = 3.0
          TDPHE = 7.0
          TDFAC = 5.0
          TDSF = 1.0
          RDGS = 3.0
          HTSTD = 100
          AWNS = 5.0
          KCAN = 0.85
          RSPCA = 15.0
          GNPCS = 2.0
          TKFH = -15
          SSPHS = 8.0
          SSPHE = 9.3
          GWTAF = 0.0        
          GWTAT = 26.0         
          RTNUP = 0.006          
          NUPCF = 0.0               
          NUPNF = 0.0               
          NUPWF = 0.0
          
        ! SPECIES  
          GMPCH = 15
!           Secondary Stages (S=Standard,K=Key(those that outputed to Evaluate.out)
!           SSNO SSTYP  SSTG SSABV SSNAME
!             1      S   1.5 LAFST Lafac_change          
!             2      K   1.6 DRAT  DoubleRidges
!             3      S 3.143 JDAT  Jointing
!             4      S   3.3 SGPHS StemStart             
!             5      S   4.0 RUESG RUE_change            
!             6      S   4.0 LGPHE LeafEnd               
!             7      S   4.0 LRPHS LfRetention           
!             8      S   4.8 CHPHS ChaffStart            
!             9      S   5.2 SVPHS StemVisible           
!            10      S   6.5 SGPHE StemEnd               
!            11      S   6.7 CHPHE ChaffEnd              
!            12      S   7.0 GGPHS GrainStart            
!            13      S   8.0 GLPHS GrLinearStart         
!            14      S   8.7 GLPHE GrLinearEnd           
!            15      S   9.0 GGPHE GrainEnd              
          PGERM = 10.0
          PEMRG = 8.0
          Pd(0) = 0.0
          Pd(9) = 200.0
          PPFPE = 1.0
          PPTHR = 20.0
          PPEXP = 2.0
          VPEND = 2.0
          VEEND = 2.0
          VLOSS = 0.2
          VLOST = 30.0
          VLOSF = 0.5
          RLIGPC = 10.0
          RLWR = 0.9
          RSEN = 0.1
          RRESP = 0.80
          RDGAF = 0.5
          RTUFR = 0.20
          LLIGPC = 10.0
          LAXS = 100.0
          LAWCF = 0.02
          LAWFF = 0.20
          LSHFR = 0.33
          LSHAV = 50.0      
          LSHAR = 80.0       
          PHINTL(1) = 2.0    ! PHL
          PHINTF(1) = 0.8    ! PHF
          LLIFA = 4.0
          LLIFG = 1.0
          LLIFS = 3.0
          LWLOS = 0.5        ! LWLOS
          LAIXX = 14.0
          ! CHT%  CLA% ! Height%, cumulative leaf area %
          !    0     0
          !   50    50
          !  100   100
          TPAR = 0.07
          TSRAD = 0.25
          TGR(2) = 0.80
          TGR(20) = 0.10
          TILIP = 6.0
          TILNOX = 20   
          RSPCX = 80
          RSPCLX = 80.0
          RSUSE = 0.1
          ! Stems          
          SLIGPC = 10.0
          SAWS = 25.0
          ! Chaff
          CHFR = 0.65
          ! Grain  
          GLIGPC = 10.0
          GWLAGFR = 0.05  ! GWLAG
          GWLINFR = 0.90  ! GWLIN 
          ! Seed
          SDWT = 0.28
          SDDUR = 20.0
          SDRSPC = 80.0
          ! Photosynthesis
          RSFPU = 3.0
          RSFPL = 2.0
          !CO2RF  CO2F
          !    0  0.00
          !  220  0.71
          !  330  1.00
          !  440  1.08
          !  550  1.17
          !  660  1.25
          !  770  1.32
          !  880  1.38
          !  990  1.43
          ! 9999  1.50
          ! CH2O distribution/mobilization
          PTFMN = 0.75
          PTFMX = 0.98
          PTFA = 0.10
          PTFXS = 6.0
          ! Cold hardiness
          TKUH = -6.0
          TKDTI = 2.0
          TKDLF = 2.0
          TKSPN = 2.0
          HDUR = 10.0
          HLOST = 10.0
          HLOSF = 0.2

          ! Water shortage effects
          WFPU = 1.0
          WFPL = 0.0
          WFGU = 1.3
          WFGL = 0.0
          WFTU = 1.0
          WFTL = 0.5
          WFSU = 0.6
          WFSF = 0.5
          LLOSA = 0.02
          WFGEU = 0.5
          WFRGU = 0.25

          ! NITROGEN uptake,distribution,etc.
           NH4MN = 0.5 
           NO3MN = 0.5
           NTUPF = 0.05
           ! Nitrogen concentrations
           GNPCMX = 4.5 
           GNPCMN = 0.5 
           SDNPCI = 1.9             
           ! LN%S LN%MN  SN%S SN%MN  RN%S RN%MN   
           !  8.0  0.80  2.50  0.65  2.00  1.50                        
           !  1.0  0.55  0.40  0.40  1.70  1.25                         
           NLABPC = 20
           NFPU = 1.0 
           NFPL = 0.0 
           NFGU = 1.0 
           NFGL = 0.0 
           NFTU = 1.0 
           NFTL = 0.0 
           NFSU = 0.4 
           NFSF = 0.1
           NCRG = 30

        ENDIF   ! EXAMINE(20)

!-----------------------------------------------------------------------
!       Determine 'key' principal and secondary stages,and adjust names
!-----------------------------------------------------------------------

        KEYPSNUM = 0
        KEYSSNUM = 0
        SSNUM = 0
        PSNUM = 0
        TSSTG = -99
        LLSTG = -99
        DO L = 1,PSX
          IF (TVILENT(PSTYP(L)).GT.0) THEN
            IF (PSTYP(L).EQ.'K'.OR.PSTYP(L).EQ.'k'.OR.
     &          PSTYP(L).EQ.'M')THEN
              KEYPSNUM = KEYPSNUM + 1
              KEYPS(KEYPSNUM) = L
            ENDIF
            IF (PSABV(L).EQ.'ADAT') ASTG = L
            IF (PSABV(L).EQ.'ECDAT') ECSTG = L
            IF (PSABV(L).EQ.'HDAT') HSTG = L
            IF (PSABV(L).EQ.'IEDAT') IESTG = L
            IF (PSABV(L).EQ.'LLDAT') LLSTG = L
            IF (PSABV(L).EQ.'MDAT') MSTG = L
            IF (PSABV(L).EQ.'TSAT') TSSTG = L
            PSNUM = PSNUM + 1
          ENDIF
        ENDDO
        ! IF MSTG not found, use maximum principal stage number
        IF (MSTG.LE.0) THEN
          MSTG = KEYPSNUM
        ENDIF
        ! IF HSTG not found, use maximum principal stage number
        IF (HSTG.LE.0) THEN
          HSTG = MSTG+1  ! LAH 230311
          PSTART(HSTG) = 5000   ! Set to very long cycle
        ENDIF
        
        KEYSS = -99
        SSNUM = 0
        DO L = 1,SSX
          IF (TVILENT(SSTYP(L)).GT.0) THEN
            SSNUM = SSNUM + 1
            IF (SSTYP(L).EQ.'K') THEN
              KEYSSNUM = KEYSSNUM + 1
              KEYSS(KEYSSNUM) = L
            ENDIF
          ENDIF
        ENDDO
        ! Check and adjust stage abbreviations (DAT -> DAP)
        DO L = 1,PSNUM
          IF (TVILENT(PSABV(L)).GT.3) THEN
            IF (TVILENT(PSABV(L)).EQ.4) THEN
              DO L1 = 5,1,-1
                IF (L1.GT.1) THEN
                  PSABV(L)(L1:L1) = PSABV(L)(L1-1:L1-1)
                ELSE
                  PSABV(L)(L1:L1) = ' '
                ENDIF
              ENDDO
            ENDIF
            PSABVO(L) = PSABV(L)
            ! DAS -> DAP for output
            PSABVO(L)(5:5) = 'P'
          ENDIF
        ENDDO
        DO L = 1,SSNUM
          IF (TVILENT(SSABV(L)).GT.3) THEN
            IF (TVILENT(SSABV(L)).EQ.4) THEN
              DO L1 = 5,1,-1
                IF (L1.GT.1) THEN
                  SSABV(L)(L1:L1) = SSABV(L)(L1-1:L1-1)
                ELSE
                  SSABV(L)(L1:L1) = ' '
                ENDIF
              ENDDO
            ENDIF
            SSABVO(L) = SSABV(L)
            ! DAS -> DAP for output
            SSABVO(L)(5:5) = 'P'
          ENDIF
        ENDDO

        ! Set stage numbers
        LAFST = -99.0
        LRETS = -99.0
        GGPHASE = -99.0
        LGPHASE = -99.0
        RUESTG = -99.0
        SGPHASE = -99.0
        DO L = 1,SSNUM
          IF (SSABV(L).EQ.'LAFST') LAFST = SSTG(L)
          IF (SSABV(L).EQ.'LGPHE') LGPHASE(2) = SSTG(L)
          IF (SSABV(L).EQ.'LRPHS') LRETS = SSTG(L)
          IF (SSABV(L).EQ.'GGPHS') GGPHASE(1) = SSTG(L)
          IF (SSABV(L).EQ.'GLPHS') GGPHASE(2) = SSTG(L)
          IF (SSABV(L).EQ.'GLPHE') GGPHASE(3) = SSTG(L)
          IF (SSABV(L).EQ.'GGPHE') GGPHASE(4) = SSTG(L)
          IF (SSABV(L).EQ.'RUESG') RUESTG = SSTG(L)
          IF (SSABV(L).EQ.'SGPHS') SGPHASE(1) = SSTG(L)
          IF (SSABV(L).EQ.'SGPHE') SGPHASE(2) = SSTG(L)
          IF (SSABV(L).EQ.'SVPHS') STVSTG = SSTG(L)
          IF (SSABV(L).EQ.'CHPHS') CHPHASE(1) = SSTG(L)
          IF (SSABV(L).EQ.'CHPHE') CHPHASE(2) = SSTG(L)
        ENDDO
        
!-----------------------------------------------------------------------
!       Calculate PHASE DURATIONS FROM phints if missing
!-----------------------------------------------------------------------
                  
        ! BASED ON ORIGINAL CERES -- FOR INITIAL CALIBRATION
        IF (CROP.EQ.'WH') THEN
          IF (PD(1).LE.0.0) THEN
            PD(1) = 400 * PHINTS / 95
            TVR1 = (3.0*PHINTS)
            PD(2) = 0.25 * (3.0*PHINTS)
            PD(3) = 0.75 * (3.0*PHINTS)
            PD(4) = 2.4 * PHINTS
            PD(5) = 0.25 * (2.4*PHINTS)
            PD(6) = 0.10 * (2.4*PHINTS)
            PD(7) = 0.65 * (2.4*PHINTS)
!            Write (fnumwrk,*) ' '
!            Write (fnumwrk,*) 'CALCULATED phase duration being used'
!            Write (fnumwrk,*) ' P1 (400*PHINT/95)     = ',PD(1)
!            Write (fnumwrk,*) ' P2 (0.25*(3.0*PHINT)) = ',PD(2)
!            Write (fnumwrk,*) ' P3 (0.75*(3.0*PHINT)) = ',PD(3)
!            Write (fnumwrk,*) ' P4 (2.40*PHINT)       = ',PD(4)
!            Write (fnumwrk,*) ' P5 (0.25*(2.4*PHINT)) = ',PD(5)
!            Write (fnumwrk,*) ' P6 (0.10*(2.4*PHINT)) = ',PD(6)
!            Write (fnumwrk,*) ' P7 (0.65*(2.4*PHINT)) = ',PD(7)
!            Write (fnumwrk,*) ' '
!            Write (fnumwrk,*) ' PHINT                 = ',PHINTS
          ENDIF  
        ENDIF  
        IF (CROP.EQ.'BA') THEN
          IF (PD(1).LE.0.0) THEN
            PD(1) = 300 * PHINTS / 70
            TVR1 = 225.0   ! Original vallue in CERES
            PD(2) = 0.25 * (3.2*PHINTS)
            PD(3) = 0.75 * (3.2*PHINTS)
            ! Original = 150
            PD(4) = 2.15 * PHINTS
            ! Original = 200
            PD(5) = 0.25 * (2.9*PHINTS)    ! Original = 60
            PD(6) = 0.10 * (2.9*PHINTS)
            PD(7) = 0.65 * (2.9*PHINTS)
!            Write (fnumwrk,*) ' '
!            Write (fnumwrk,*) 'CALCULATED phase duration being used'
!            Write (fnumwrk,*) ' P1 (300*PHINT/70)     = ',PD(1)
!            Write (fnumwrk,*) ' P2 (0.25*(3.2*PHINT)) = ',PD(2)
!            Write (fnumwrk,*) ' P3 (0.75*(3.2*PHINT)) = ',PD(3)
!            Write (fnumwrk,*) ' P4 (2.415*PHINT)      = ',PD(4)
!            Write (fnumwrk,*) ' P5 (0.25*(2.9*PHINT)) = ',PD(5)
!            Write (fnumwrk,*) ' P6 (0.10*(2.9*PHINT)) = ',PD(6)
!            Write (fnumwrk,*) ' P7 (0.65*(2.9*PHINT)) = ',PD(7)
!            Write (fnumwrk,*) ' '
!            Write (fnumwrk,*) ' PHINT                 = ',PHINTS
          ENDIF  
        ENDIF  

!-----------------------------------------------------------------------
!       Calculate/adjust phase durations and thresholds
!-----------------------------------------------------------------------
        
        ! Check if phase durations input as leaf units
        DO L = 1,8
          IF (PDL(L).GT.0.0) PD(L) = PDL(L) * PHINTS
        ENDDO
        
        ! Check for missing phase durations and if so use previous
        Ctrnumpd = 0
        DO L = 2,MSTG
          IF (PD(L).LT.0.0) THEN
            PD(L) = PD(L-1)
            CTRNUMPD = CTRNUMPD + 1
          ENDIF
        ENDDO
        IF (PD(MSTG).LT.0.0) PD(MSTG) = PDMTOHAR
        IF (CTRNUMPD.GT.0) THEN
          WRITE(MESSAGE(1),'(A11,I2,A23,A34)')
     &    'Duration of',CTRNUMPD,' phases less than zero.',
     &    'Used value(s) for preceding phase.'
          CALL WARNING(1,'CSCRP',MESSAGE)
!          WRITE(Fnumwrk,*)' '
!          WRITE(Fnumwrk,'(A12,I2,A23,A34)')
!     &    ' Duration of',CTRNUMPD,' phases less than zero.',
!     &    'Used value(s) for preceding phase.'
        ENDIF

        ! Calculate thresholds
        GGPHASEDU = 0.
        CHPHASEDU = 0.
        LAFSTDU = 0.
        LGPHASEDU = 0.
        LRETSDU = 0. 
        LSPHSDU = 0.
        SSPHSDU = 0.
        SSPHEDU = 0.
        RUESTGDU = 0.
        SGPHASEDU = 0.
        STVSTGDU =  0.
        TDPHEDU = 0.
        TDPHSDU = 0.
        TILPEDU = 0.
        VEENDDU = 0.
        VPENDDU = 0.
        DO L = 0,MSTG
          PSTART(L) = 0.0
        ENDDO
        DO L = 1,MSTG
          PSTART(L) = PSTART(L-1) + AMAX1(0.0,PD(L-1))
          DO L1 = 1,SSNUM
            IF (INT(SSTG(L1)).EQ.L)
     &        SSTH(L1) = PSTART(L)+(SSTG(L1)-FLOAT(INT(SSTG(L1))))*PD(L)
          ENDDO       
          IF (L.EQ.INT(LAFST))
     &      LAFSTDU = PSTART(L)+(LAFST-FLOAT(INT(LAFST)))*PD(L)
          IF (L.EQ.INT(VEEND))
     &      VEENDDU = PSTART(L)+(VEEND-FLOAT(INT(VEEND)))*PD(L)
          IF (L.EQ.INT(VPEND))
     &      VPENDDU = PSTART(L)+(VPEND-FLOAT(INT(VPEND)))*PD(L)
          IF (L.EQ.INT(RUESTG))
     &      RUESTGDU = PSTART(L)+(RUESTG-FLOAT(INT(RUESTG)))*PD(L)
          IF (L.EQ.INT(LSPHS))
     &      LSPHSDU = PSTART(L)+(LSPHS-FLOAT(INT(LSPHS)))*PD(L)
          IF (L.EQ.INT(LSPHE))
     &      LSPHEDU = PSTART(L)+(LSPHE-FLOAT(INT(LSPHE)))*PD(L)
          IF (L.EQ.INT(SSPHS)) SSPHSDU =
     &     PSTART(L)+(SSPHS-FLOAT(INT(SSPHS)))*PD(L)
          IF (L.EQ.INT(SSPHE)) SSPHEDU =
     &     PSTART(L)+(SSPHE-FLOAT(INT(SSPHE)))*PD(L)
          IF (L.EQ.INT(TILPE))
     &      TILPEDU = PSTART(L)+(TILPE-FLOAT(INT(TILPE)))*PD(L)
          IF (L.EQ.INT(TDPHS))
     &      TDPHSDU = PSTART(L)+(TDPHS-FLOAT(INT(TDPHS)))*PD(L)
          IF (L.EQ.INT(TDPHE))
     &      TDPHEDU = PSTART(L)+(TDPHE-FLOAT(INT(TDPHE)))*PD(L)
          IF (L.EQ.INT(LRETS))
     &      LRETSDU = PSTART(L)+(LRETS-FLOAT(INT(LRETS)))*PD(L)
          IF (L.EQ.INT(CHPHASE(1))) CHPHASEDU(1) = 
     &     PSTART(L)+(CHPHASE(1)-FLOAT(INT(CHPHASE(1))))*PD(L)
          IF (L.EQ.INT(CHPHASE(2))) CHPHASEDU(2) = 
     &     PSTART(L)+(CHPHASE(2)-FLOAT(INT(CHPHASE(2))))*PD(L)
          IF (L.EQ.INT(GGPHASE(1))) GGPHASEDU(1) =
     &     PSTART(L)+(GGPHASE(1)-FLOAT(INT(GGPHASE(1))))*PD(L)
          IF (L.EQ.INT(GGPHASE(2))) GGPHASEDU(2) =
     &     PSTART(L)+(GGPHASE(2)-FLOAT(INT(GGPHASE(2))))*PD(L)
          IF (L.EQ.INT(GGPHASE(3))) GGPHASEDU(3) =
     &     PSTART(L)+(GGPHASE(3)-FLOAT(INT(GGPHASE(3))))*PD(L)
          IF (L.EQ.INT(GGPHASE(4))) GGPHASEDU(4) =
     &     PSTART(L)+(GGPHASE(4)-FLOAT(INT(GGPHASE(4))))*PD(L)
          IF (L.EQ.INT(LGPHASE(1))) LGPHASEDU(1) =
     &     PSTART(L)+(LGPHASE(1)-FLOAT(INT(LGPHASE(1))))*PD(L)
          IF (L.EQ.INT(LGPHASE(2))) LGPHASEDU(2) =
     &     PSTART(L)+(LGPHASE(2)-FLOAT(INT(LGPHASE(2))))*PD(L)
          IF (L.EQ.INT(SGPHASE(1))) SGPHASEDU(1) =
     &     PSTART(L)+(SGPHASE(1)-FLOAT(INT(SGPHASE(1))))*PD(L)
          IF (L.EQ.INT(SGPHASE(2))) SGPHASEDU(2) =
     &     PSTART(L)+(SGPHASE(2)-FLOAT(INT(SGPHASE(2))))*PD(L)
          IF (L.EQ.INT(STVSTG)) STVSTGDU =
     &     PSTART(L)+(STVSTG-FLOAT(INT(STVSTG)))*PD(L)
        ENDDO
        
        DUTOMSTG = 0.0
        DO L = 1, MSTG-1
          DUTOMSTG = DUTOMSTG + PD(L)
        ENDDO
        
        IF (PHINTS.LE.0.0) THEN
            OPEN (UNIT = FNUMERR,FILE = 'ERROR.OUT')
            WRITE(fnumerr,*) ' '
            WRITE(fnumerr,*)
     &       'PHINT <= 0! Please correct genotype files.'
            WRITE(*,*)
     &       ' PHINT <= 0! Please correct genotype files.'
            WRITE(*,*) ' Program will have to stop'
            PAUSE
            CLOSE (fnumerr)
            STOP ' '
        ENDIF
        
        ! Adjust germination phase for seed dormancy
        IF (PLMAGE.LT.5.0.AND.PLMAGE.GT.-90.0) THEN
          PEGD = AMAX1(0.0,PGERM - (PLMAGE*STDAY)) ! Dormancy -ve age
        ELSE
          PEGD = PGERM
        ENDIF

!-----------------------------------------------------------------------
!       Check and/or adjust coefficients and set defaults if not present
!-----------------------------------------------------------------------

        ! Phases
        IF (LGPHASE(1).LT.0.0) LGPHASEDU(1) = 0.0
        IF (LGPHASE(2).LE.0.0) LGPHASEDU(2) = DUTOMSTG
        IF (SGPHASEDU(1).LT.0.0) SGPHASEDU(1) = 0.0
        IF (SGPHASEDU(2).LE.0.0) SGPHASEDU(2) = DUTOMSTG
        IF (GGPHASE(1).LE.0.0) GGPHASEDU(1) = 99999
        IF (CHPHASE(1).LE.0.0) CHPHASEDU(1) = 99999
        IF (CHPHASE(2).LE.0.0) CHPHASEDU(2) = 99999
        ! Photoperiodism
        IF (PPEXP.LT.0.0) PPEXP = 2.0
        IF (PPFPE.LT.0.0) THEN  
          PPFPE = 1.0
          WRITE(MESSAGE(1),'(A51)')
     &    'Pre-emergence development factor missing. Set to 1.'
          CALL WARNING(1,'CSCRP',MESSAGE)
!          WRITE(FNUMWRK,*) ' '
!          WRITE(FNUMWRK,'(A51)')
!     &    ' Pre-emergence development factor missing. Set to 1'
        ENDIF
        ! Vernalization
        IF (VEEND.LE.0.0) VEEND = 0.0
        IF (VEFF.LT.0.0) VEFF = 0.0
        IF (VEFF.GT.1.0) VEFF = 1.0
        IF (VLOSS.LT.0.0) VLOSS = 0.2
        IF (VLOSF.LT.0.0) VLOSF = 0.2
        IF (VLOST.LT.0.0) VLOST = 30.0
        IF (VREQ.LE.0.0) VREQ = 0.0
        IF (VBASE.LT.0.0) VBASE = 0.0
        IF (VPEND.LE.0.0) VPEND = 0.0
        ! Photosynthesis
        IF (PARUE.LE.0.0) PARUE = 2.3
        IF (SLPF.LE.0.0 .OR. SLPF.GT.1.0) SLPF = 1.0
!        IF (SLPF.LT.1.0) THEN
!          WRITE (fnumwrk,*) ' '
!          WRITE (fnumwrk,*)
!     &     'WARNING  Soil fertility factor was less than 1.0: ',slpf
!        ENDIF  
        ! Partitioning
        IF (PTFMN.LE.0.0) PTFMN = 0.75
        IF (PTFMX.LE.0.0) PTFMX = 0.09
        IF (PTFA.LT.0.0) PTFA = 0.10
        IF (PTFXS.LE.0.0) PTFXS = 6.0
        ! Leaves
        IF (LA1S.LE.0.0) THEN
          LA1S = 5.0
          WRITE(MESSAGE(1),'(A47)')
     &    'Initial leaf size (LA1S) missing. Set to 5 cm2.'
          CALL WARNING(1,'CSCRP',MESSAGE)
!          WRITE(Fnumwrk,*)' '
!          WRITE(Fnumwrk,'(A48)')
!     &    ' Initial leaf size (LA1S) missing. Set to 5 cm2.'
        ENDIF
        IF (LAFV.LT.0.0) LAFV = 0.1
        IF (LAFR.LT.-90.0) LAFR = 0.1
        IF (LAXS.LT.0.0) LAXS = 200.0
        IF (LAFST.LE.0.0) LAFST = MIN(PSX,MSTG+1)
        IF (LWLOS.LT.0.0) LWLOS = 0.3
        IF (LRETS.GE.10.OR.LRETS.LE.0.0) LRETSDU = 99999
        IF (LSPHE.LE.0.0) LSPHEDU = 99999
        IF (LSENI.LT.0.0) LSENI = 0.5
        IF (PARIX.LE.0.0) PARIX = 0.995
        IF (LSPHS.GE.10.OR.LSPHS.LE.0.0) LSPHSDU = 99999
        ! Tillers
        IF (G3.LE.0.0) THEN  
          G3 = 1.0
          WRITE(MESSAGE(1),'(A41)')
     &    'Standard shoot weight missing. Set to 1.0'
          CALL WARNING(1,'CSCRP',MESSAGE)
!          WRITE(Fnumwrk,*)' '
!          WRITE(Fnumwrk,'(A42)')
!     &    ' Standard shoot weight missing. Set to 1.0'
        ENDIF
        IF (TDFAC.LT.0.0) TDFAC = 0.0
        IF (TDSF.LT.0.0) TDSF = 1.0
        IF (TI1LF.LT.0.0) TI1LF = 1000
        IF (TIFAC.LT.0.0) TIFAC = 1.0
        IF (TDPHE.LT.0.0) TDPHEDU = 99999
        IF (TDPHS.LT.0.0) TDPHSDU = 99999
        IF (TILIP.LT.0.0) TILIP = 6.0
        IF (TILPE.LT.0.0) TILPEDU = 99999
        IF (TILNOX.LE.0.0) TILNOX = 30
        IF (TGR(20).LT.0.0) THEN 
          DO L = 3,22
            TGR(L) = 1.0 ! Tiller (shoot) sizes relative to main shoot
          ENDDO
        ENDIF
        ! Stems
        IF (SSPHSDU.LT.0.0) SSPHSDU = 99999
        IF (SSPHEDU.LT.0.0) SSPHEDU = 99999
        IF (SWFRS.GT.0.0) THEN
          SWFRX = SWFRS  ! (Constant stem fr)
          SWFRXL = 9999
          SWFRN = SWFRS
          SWFRNL = 0
        ENDIF 
        ! Grain                
        IF (GMPCH.LT.0.0) THEN
          GMPCH = 15.0
!          WRITE(Fnumwrk,*)' '
!          WRITE(Fnumwrk,'(A38)')
!     &    ' Grain moisture % missing. Set to 15.0'
        ENDIF
        IF (GWTS.LE.0.0) THEN  
          GWTS = 30.0
          WRITE(MESSAGE(1),'(A33)')
     &    'Grain weight missing. Set to 30.0'
          CALL WARNING(1,'CSCRP',MESSAGE)
!          WRITE(Fnumwrk,*)' '
!          WRITE(Fnumwrk,'(A34)')
!     &    ' Grain weight missing. Set to 30.0'
        ENDIF
        IF (GWTAT.LT.0.0) GWTAT = 20.0
        IF (GWTAF.LT.0.0) GWTAF = 0.0
        IF (GNOWTS.LE.0.0) THEN  
          GNOWTS = 20.0
          WRITE(MESSAGE(1),'(A49)')
     &    'Grain number per unit weight missing. Set to 20.0'
          CALL WARNING(1,'CSCRP',MESSAGE)
!          WRITE(Fnumwrk,*)' '
!          WRITE(Fnumwrk,'(A50)')
!     &    ' Grain number per unit weight missing. Set to 20.0'
        ENDIF
        IF (GNORF.LT.0.0) GNORF = 0.0
        IF (GNORT.LT.0.0) GNORT = 10.0
        IF (GNPCS.LE.0.0) GNPCS = 2.0
        IF (GNPCMX.LE.0.0) GNPCMX = 5.0
        IF (GNPCMN.LT.0.0) GNPCMN = 0.0
        IF (GWLAGFR.LE.0.0) GWLAGFR = 0.05
        IF (GWLINFR.LE.0.0) GWLINFR = 0.95
        IF (TRGFC(2).LE.0.0) THEN        ! Grain temperature responses
          DO L = 1,4
             TRGFC(L) = TRPHS(L)
             TRGFN(L) = TRPHS(L)
          ENDDO
        ENDIF
        ! Roots
        IF (RDGS.LT.0.0) RDGS = 3.0
        IF (RSEN.LT.0.0) RSEN = 0.1
        IF (RTUFR.LT.0.0) RTUFR = 0.05   ! (Useable to reestablish tops)
        ! Reserves
        IF (RSPCA.LT.0.0) RSPCA = 15.0
        IF (RSPCX.LT.0.0) RSPCX = 80.0
        IF (RSUSE.LT.0.0) RSUSE = 0.10
        ! Water uptake
        IF (RLFWU.LT.0.0) RLFWU = 0.5    ! (Only alternate method)
        ! N uptake  
        IF (rtnup.le.0.0) RTNUP = 0.006  ! NO3 uptake/root lgth (mgN/cm)
        IF (nupcf.lt.-90.0) NUPCF = 0.0  ! H2O conc factor for N uptake 
        IF (nupnf.lt.-90.0) NUPNF = 0.0  ! NO3 uptake conc factor (exp)
        IF (nupwf.lt.-90.0) NUPWF = 0.0  ! H2O conc factor for N uptake 
        IF (no3mn.lt.0.0) no3mn = 0.5
        IF (nh4mn.lt.0.0) nh4mn = 0.5
        IF (NTUPF.LT.0.0) NTUPF = 0.2
        DO L = 0,1
        LNCXS(L) = LNPCS(L)/100.0 
        SNCXS(L) = SNPCS(L)/100.0 
        RNCXS(L) = RNPCS(L)/100.0 
        LNCMN(L) = LNPCMN(L)/100.0
        SNCMN(L) = SNPCMN(L)/100.0
        RNCMN(L) = RNPCMN(L)/100.0
        ENDDO
        ! Stress factors
        IF (WFPL.LT.0.0) WFPL = 0.0
        IF (WFPU.LT.0.0) WFPU = 1.0
        IF (WFGL.LT.0.0) WFGL = 0.0
        IF (WFGU.LT.0.0) WFGU = 1.0
        IF (NFTL.LT.0.0) NFTL = 0.0
        IF (NFTU.LT.0.0) NFTU = 1.0
        IF (NFPL.LT.0.0) NFPL = 0.0
        IF (NFPU.LT.0.0) NFPU = 1.0
        IF (NFGL.LT.0.0) NFGL = 0.0
        IF (NFGU.LT.0.0) NFGU = 1.0
        IF (NFSU.LT.0.0) NFSU = 0.2
        IF (NFSF.LT.0.0) NFSF = 0.1
        ! Cold hardiness
        IF (HLOSF.LT.0.0) HLOSF = 0.2
        IF (HLOST.LT.0.0) HLOST = 20.0
        IF (TKDLF.LT.-90.0) TKDLF = 2.0
        IF (TKSPN.LT.0.0) TKSPN = 6.0
        IF (TKDTI.LT.0.0) TKDTI = 2.0
        ! High temperature sensitivity
        IF (TKGF.LE.0.0) TKGF = 45.0
        
!-----------------------------------------------------------------------
!       Calculate derived coefficients and set equivalences
!-----------------------------------------------------------------------

        ! Initial leaf growth aspects
        Lapotx(1) = La1s
        ! If max LAI not read-in,calculate from max interception
        IF (LAIXX.LE.0.0) LAIXX = LOG(1.0-PARIX)/(-KCAN)
        
        ! New lah march 2010
        DO L = 1,PHSX
          IF (PHINTL(L).LE.0.0) PHINTL(L) = 1000.0
          IF (PHINTF(L).LE.0.0) PHINTF(L) = 1.0
        ENDDO
        phintstg = 1
        IF (phintf(1).GT.0.0) THEN
          phint = phints*phintf(1)
        ELSE
          phint = phints
        ENDIF
        phintstore = phint
        LLIFGTT = LLIFG * PHINT 
        LLIFATT = LLIFA * PHINT 
        LLIFSTT = LLIFS * PHINT 
        ! End New

        ! Extinction coeff for SRAD
        KEP = (KCAN/(1.0-TPAR)) * (1.0-TSRAD)

        ! Photoperiod sensitivities
        DO L = 0,10
          IF (PPS(L).LE.0.0) PPS(L) = 0.0
        ENDDO
        IF (Pps(1).EQ.0.0.AND.Ppfpe.LT.1.0) THEN
          WRITE(MESSAGE(1),'(A36,A41)')
     &    'Cultivar insensitive to photoperiod ',
     &    'but pre-emergence photoperiod factor < 1.' 
          WRITE(MESSAGE(2),'(A40)')
     &    'May be worthwhile to change PPFPE to 1.0'
          CALL WARNING(2,'CSCRP',MESSAGE)
!          WRITE(Fnumwrk,*)' '
!          WRITE(Fnumwrk,'(A37,A41)')
!     &    ' Cultivar insensitive to photoperiod ',
!     &    'but pre-emergence photoperiod factor < 1.' 
!          WRITE(Fnumwrk,'(A41)')
!     &    ' May be worthwhile to change PPFPE to 1.0'
        ENDIF

        ! Tiller growth rates relative to main shoot
        IF (TGR(20).GE.0.0) THEN
          DO L = 3,22
            IF (L.LT.20) THEN
              TGR(L) = TGR(2)-((TGR(2)-TGR(20))/18)*(L-2)
            ELSEIF (L.GT.20) THEN
              TGR(L) = TGR(20)
            ENDIF  
          ENDDO
        ENDIF

        ! Critical and starting N concentrations
        LNCX = LNCXS(0)
        SNCX = SNCXS(0)
        RNCX = RNCXS(0)
        LNCM = LNCMN(0)
        SNCM = SNCMN(0)
        RNCM = RNCMN(0)

        ! Height growth
        SERX = HTSTD/(PD(1)+PD(2)+PD(3)+PD(4)+PD(5)+PD(6))

!-----------------------------------------------------------------------
!       Set coefficients that dependent on input switch
!-----------------------------------------------------------------------

        IF (ISWWATCROP.EQ.'E') THEN
          ! Plant water status effects on growth turned off
          WFTU = 0.0
          WFGU = 0.0
          WFPU = 0.0
          WFSU = 0.0
          WFSF = 0.0
          WFRGU = 0.0
        ENDIF

!-----------------------------------------------------------------------
!       Calculate/set initial states
!-----------------------------------------------------------------------

        IF (SDRATE.LE.0.0) SDRATE = SDWT*PLTPOPP*10.0
        ! Reserves = 80% of seed (42% Ceres3.5)
        SEEDRSI = (SDRATE/(PLTPOPP*10.0))*SDRSPC/100.0
        SEEDRS = SEEDRSI
        SEEDRSAV = SEEDRS
        SDCOAT = (SDRATE/(PLTPOPP*10.0))*(1.0-SDRSPC/100.0)
        ! Seed N calculated from total seed
        SDNAP = (SDNPCI/100.0)*SDRATE
        SEEDNI = (SDNPCI/100.0)*(SDRATE/(PLTPOPP*10.0))
        IF (ISWNIT.NE.'N') THEN
          SEEDN = SEEDNI
        ELSE
          SEEDN = 0.0
          SDNAP = 0.0
          SEEDNI = 0.0
        ENDIF
        TKILL = TKUH
        VF = (1.0-VEFF)    

        ! Water table depth
!       WTDEP = ICWD
        CALL GET('WATER','WTDEP',WTDEP)

        ! Initial shoot and root placement
        IF (SPRL.LT.0.0) SPRL = 0.0
        sdepthu = -99.0
        IF (PLME.EQ.'H') THEN
          sdepthu = sdepth
        ELSEIF (PLME.EQ.'I') THEN
          ! Assumes that inclined at 45o
          sdepthu = AMAX1(0.0,sdepth - 0.707*sprl)
        ELSEIF (PLME.EQ.'V') THEN
          sdepthu = AMAX1(0.0,sdepth - sprl)
        ENDIF
        IF (sdepthu.LT.0.0) sdepthu = sdepth

!-----------------------------------------------------------------------
!       Create output descriptors
!-----------------------------------------------------------------------

        ! Run name
        IF (runname(1:6).EQ.'      ' .OR.
     &    runname(1:3).EQ.'-99') runname = tname

        ! Composite run variable
        IF (RUNI.LT.10) THEN
          WRITE (RUNRUNI,'(I3,A1,I1,A3)') RUN,',',RUNI,'   '
        ELSEIF (RUNI.GE.10.AND.RUNI.LT.100) THEN
          WRITE (RUNRUNI,'(I3,A1,I2,A2)') RUN,',',RUNI,'  '
        ELSE
          WRITE (RUNRUNI,'(I3,A1,I3,A1)') RUN,',',RUNI,' '
        ENDIF
        IF (RUN.LT.10) THEN
          RUNRUNI(1:6) = RUNRUNI(3:8)
          RUNRUNI(7:8) = '  '
          ! Below is to give run number only for first run
          IF (RUNI.LE.1) RUNRUNI(2:8) = '       '
        ELSEIF (RUN.GE.10.AND.RUN.LT.100) THEN
          RUNRUNI(1:7) = RUNRUNI(2:8)
          RUNRUNI(8:8) = ' '
          ! Below is to give run number only for first run
          IF (RUNI.LE.1) RUNRUNI(3:8) = '      '
        ENDIF

        ! Composite treatment+run name
        CALL LTRIM (RUNNAME)
        RUNNAME = TRIM(RUNNAME)
        CALL LTRIM (TNAME)
        TNAME = TRIM(TNAME)
        LENTNAME = MIN(15,TVILENT(TNAME))
        LENRNAME = MIN(15,TVILENT(RUNNAME))
        IF (LENRNAME.GT.5) THEN
          TRUNNAME = RUNNAME(1:LENRNAME)//' '//MODNAME
        ELSE
          TRUNNAME = TNAME(1:LENTNAME)//' '//MODNAME
        ENDIF
        IF (MEEXP.EQ.'E') THEN
          CALL LTRIM (TRUNNAME)
          LENTNAME = TVILENT(TRUNNAME)
          TRUNNAME = TRUNNAME(1:LENTNAME)//' EXPERIMENTAL'
        ENDIF

        ! File header
        IF (CN.GT.1) THEN
          IF (TN.LT.10) THEN
            WRITE (OUTHED,7104) RUNRUNI(1:5),EXCODE,TN,RN,CN,TRUNNAME
 7104       FORMAT ('*RUN ',A5,A10,' ',I1,',',I1,' C',I1,' ',A40,'  ')
          ELSEIF (TN.GE.10.AND.TN.LT.100) THEN
            WRITE (OUTHED,7105) RUNRUNI,EXCODE,TN,RN,CN,TRUNNAME
 7105       FORMAT ('*RUN ',A5,A10,' ',I2,',',I1,' C',I1,' ',A40,' ')
          ELSEIF (TN.GE.10 .AND. TN.LT.100) THEN
            WRITE (OUTHED,7106) RUNRUNI,EXCODE,TN,RN,CN,TRUNNAME
 7106       FORMAT ('*RUN ',A5,A10,' ',I3,',',I1,' C',I1,' ',A40)
          ENDIF
        ELSE
          IF (TN.LT.10) THEN
            WRITE (OUTHED,7107) RUNRUNI(1:5),EXCODE,TN,TRUNNAME
 7107       FORMAT ('*RUN ',A5,': ',A10,' ',I1,' ',A40,'  ')
          ELSEIF (TN.GE.10.AND.TN.LT.100) THEN
            WRITE (OUTHED,7108) RUNRUNI,EXCODE,TN,RN,TRUNNAME
 7108       FORMAT ('*RUN ',A5,': 'A10,' ',I2,',',I1,' ',A40,' ')
          ELSEIF (TN.GE.10 .AND. TN.LT.100) THEN
            WRITE (OUTHED,7109) RUNRUNI,EXCODE,TN,RN,TRUNNAME
 7109       FORMAT ('*RUN ',A5,': 'A10,' ',I3,',',I1,' ',A40)
          ENDIF
        ENDIF

!-----------------------------------------------------------------------
!       Check controls
!-----------------------------------------------------------------------

        ! Water and N uptake methods .. MEWNU 
        ! R=RLV+LL complex,W=RLV for h20,N=RLV for N,B=RLV for both
        IF (MEWNU.NE.'R') THEN 
          IF (MEWNU.NE.'W') THEN 
            IF (MEWNU.NE.'N') THEN 
              IF (MEWNU.NE.'B') THEN 
                MEWNU = 'R'
              ENDIF        
            ENDIF        
          ENDIF        
        ENDIF        
        IF (MEPHS.NE.'I') THEN
          IF (MEPHS.NE.'M') THEN
            IF (MEPHS.NE.'R') THEN
              WRITE(MESSAGE(1),'(A22,A1,A15,A19)')
     &          'Photosynthesis method ',MEPHS,' not an option ',
     &          ' Changed to R (RUE)'
              CALL WARNING(1,'CSCRP',MESSAGE)
!              WRITE(FNUMWRK,*)' '
!              WRITE(FNUMWRK,'(A32,A1,A15,A19)')
!     &          ' WARNING  Photosynthesis method ',MEPHS,
!     &          ' not an option ',
!     &          ' Changed to R (RUE)'
              MEPHS = 'R'
            ENDIF
          ENDIF
        ENDIF
        ! Other CSM codes are:
        !  C Canopy photosynthesis curve.
        !  L Leaf photosynthesis response curve
        IF (IHARI.NE.'M') THEN
          IF (hnumber.LE.0) THEN 
            WRITE(MESSAGE(1),'(A37,A13,A1)')
     &        'No harvest date set although planting',
     &        'flag set to: ',IHARI
            MESSAGE(2)='Flag reset to M.'
            CALL WARNING(1,'CSCRP',MESSAGE)
!            WRITE(Fnumwrk,*)' '
!            WRITE(Fnumwrk,'(A38,A13,A1)')
!     &        ' No harvest date set although planting',
!     &        'flag set to: ',IHARI
!            WRITE(Fnumwrk,'(A17)')
!     &       ' Flag reset to M.'
            IHARI = 'M'                      
          ENDIF
        ENDIF

!-------------------------------------------------------------------
!       Write run information to Overview and Work output files
!-------------------------------------------------------------------

        ! To avoid problems of writing to closed file in Sequence mode 
!        INQUIRE (FILE = 'WORK.OUT',OPENED = FOPEN)
!        IF (.NOT.FOPEN) THEN
!          OPEN (UNIT = FNUMWRK,FILE = 'WORK.OUT')
!          WRITE(fnumwrk,*) 'CSCRP  Cropsim Cereal Crop Module '
!        ENDIF
                    
!        WRITE(fnumwrk,*)' '
!        WRITE(fnumwrk,'(A18,A10,I3)')' GENERAL INFO FOR ',
!     &       excode,tn
!        WRITE(fnumwrk,*)' FILE       ',FILEIO(1:60)
!        WRITE(fnumwrk,*)' EXPERIMENT ',EXCODE
!        WRITE(fnumwrk,*)' TREATMENT  ',TN
!        WRITE(fnumwrk,*)' REPLICATE  ',RN
!        WRITE(fnumwrk,*)' '
!        WRITE(fnumwrk,*)' MODEL      ',MODEL
!        WRITE(fnumwrk,*)' MODULE     ',MODNAME
        !WRITE(fnumwrk,'(A13,I6)')'  VERSION    ',VERSIONCSCRP
!        WRITE(fnumwrk, "(A13,I1,'.',I1,'.',I1,'.',I3.3)")
!     &   '  VERSION    ', VERSION
!        WRITE(fnumwrk,*)' RNMODE     ',RNMODE
!        IF (RUN.LT.10) THEN
!          WRITE(fnumwrk,'(A13,I1)')' RUN        ',RUN   
!        ELSEIF (RUN.GE.10.AND.RUN.LT.1000) THEN
!          WRITE(fnumwrk,'(A13,I2)')' RUN        ',RUN   
!        ELSE
!          WRITE(fnumwrk,'(A13,I3)')' RUN        ',RUN   
!        ENDIF
!        WRITE(fnumwrk,*)' CULTIVAR   ',CUDIRFLE(1:60)
!        WRITE(fnumwrk,*)' ECOTYPE    ',ECDIRFLE(1:60)
!        WRITE(fnumwrk,*)' SPECIES    ',SPDIRFLE(1:60)
!        WRITE(fnumwrk,*)' METHODS '
!        IF (MEEXP.EQ.'E')
!     &   WRITE(fnumwrk,'(A26,A1)')'   EXPERIMENTAL ALGORITHM ',MEEXP
!         WRITE(fnumwrk,'(A26,A1)') '   PHOTOSYNTHESIS         ',MEPHS
!         WRITE(fnumwrk,'(A26,A1,1X,A1)') '   WATER & N SWITCHES     '
!     &     ,ISWWAT,ISWNIT
!         WRITE(fnumwrk,'(A26,A1,1X,A1)') '   PHASE & PHINT SWITCHES '
!     &     ,CFLPHASEADJ,CFLPHINTADJ
!         WRITE(fnumwrk,'(A54)')                              
!     &     '   (NB.Default phase switch is Y = use Aitken formula;'
!         WRITE(fnumwrk,'(A58)')                              
!     &     '   Default phint switch is N = no daylength change effect)'
!         WRITE(fnumwrk,'(A26,I1)') ' '
!         WRITE(fnumwrk,'(A26,I1)') '  CROP COMPONENT          ',CN
!         WRITE(fnumwrk,'(A26,A6,2X,A16)')
!     &     '  CULTIVAR                ',VARNO,VRNAME
!       IF (IPLTI.NE.'A') THEN
!        IF (IPLTI.NE.'A' .AND. IPLTI.NE.'F') THEN
!          WRITE(fnumwrk,'(A23,I7)')
!     &     '  PLANTING DATE TARGET:',PLYEARDOYT
!        ELSE
!          WRITE(fnumwrk,'(A23)')
!     &     '  AUTOMATIC PLANTING   '              
!          WRITE (fnumwrk,*) '  PFIRST,PLAST :',pwdinf,pwdinl
!          WRITE (fnumwrk,*) '  HFIRST,HLAST :',hfirst,hlast
!        ENDIF
!        WRITE (fnumwrk,'(A15,2F7.1)')'  PLTPOP,ROWSPC',PLTPOPP,ROWSPC
!        WRITE (fnumwrk,'(A15,2F7.1)')'  SDEPTH,SDRATE',SDEPTH,SDRATE
!        WRITE (fnumwrk,'(A15,F7.4,F7.2)')'  SDWT,SDNPCI  ',SDWT,SDNPCI
!        IF (sdepthu.LT.sdepth)
!     &   WRITE (fnumwrk,'(A15,F7.1)')'  SHOOT DEPTH  ',SDEPTHU      
!        WRITE (fnumwrk,'(A15,2F7.1,A6)')'  SEEDRS,SEEDN ',
!     &                   SEEDRSI*PLTPOPP*10.0,SEEDNI*PLTPOPP*10.0,
!     &                   ' kg/ha'
!        WRITE (fnumwrk,'(A15, F7.1)') '  PLMAGE       ',PLMAGE
        ! LAH NEED TO CHECK HARVEST OPTIONS FOR dap,growth stage.
        ! DISCUSS WITH CHP
!        IF (IHARI.NE.'M') THEN
!          IF (IHARI.NE.'A') THEN
!            WRITE(fnumwrk,'(A22,I7)')
!     &      '  HARVEST DATE TARGET:',YEARDOYHARF 
!          ELSE
!            WRITE(fnumwrk,'(A22,A9)')
!     &      '  HARVEST DATE TARGET:','AUTOMATIC'  
!          ENDIF 
!        ELSE
!          WRITE(fnumwrk,'(A22,A8)')
!     &     '  HARVEST DATE TARGET:','MATURITY'  
!        ENDIF
!        WRITE (fnumwrk,'(A15,2F7.1)') '  HPCF,HBPCF   ',HPCF,HBPCF

!        IF (IDETG.NE.'N') THEN
!          WRITE(fnumwrk,*)' '
!          WRITE(fnumwrk,*)' MAJOR COEFFICIENTS AFTER CHECKING'
!          WRITE(fnumwrk,*)' '
!          WRITE(fnumwrk,*)' SEEDLING ASPECTS'
!          WRITE(fnumwrk,*)'  Seed '
!          WRITE(fnumwrk,*)'   Sdwt,Sdrs% ',Sdwt,Sdrspc 
!          WRITE(fnumwrk,*)'   Sddur  ',Sddur 
!          WRITE(fnumwrk,*)'  Germination and Emergence '
!          WRITE(fnumwrk,*)'   Pgerm  ',Pgerm 
!          WRITE(fnumwrk,*)'   Pemrg  ',Pemrg 
!          WRITE(fnumwrk,*)' DEVELOPMENT '
!          WRITE(fnumwrk,*)'  Thermal time between major stages'
!          WRITE(fnumwrk,*)'   P0     ',Pd(0)
!          WRITE(fnumwrk,*)'   P1     ',Pd(1)
!          WRITE(fnumwrk,*)'   P2     ',Pd(2)
!          WRITE(fnumwrk,*)'   P3     ',Pd(3)
!          WRITE(fnumwrk,*)'   P4     ',Pd(4)
!          WRITE(fnumwrk,*)'   P5     ',Pd(5)
!          WRITE(fnumwrk,*)'   P6     ',Pd(6)
!          WRITE(fnumwrk,*)'   P7     ',Pd(7)
!          WRITE(fnumwrk,*)'   P8     ',Pd(8)
!          WRITE(fnumwrk,*)'   P9     ',Pd(9)
!          WRITE(fnumwrk,*)'  Stage at which intermediate events occur '
!          WRITE(fnumwrk,*)'   Chaff '
!          WRITE(fnumwrk,*)'    Chphs  ',Chphase(1)               
!          WRITE(fnumwrk,*)'    Chphe  ',Chphase(2)             
!          WRITE(fnumwrk,*)'   Grain  '
!          WRITE(fnumwrk,*)'    Ggphs  ',GGphase(1)               
!          WRITE(fnumwrk,*)'    Glphs  ',GGphase(2)               
!          WRITE(fnumwrk,*)'    Glphe  ',GGphase(3)              
!          WRITE(fnumwrk,*)'    Ggphe  ',GGphase(4)              
!          WRITE(fnumwrk,*)'  Thermal time modifiers '              
!          WRITE(fnumwrk,*)'   Vernalization'
!          WRITE(fnumwrk,*)'    Vreq,Vbase,Veff ',Vreq,Vbase,Veff
!          WRITE(fnumwrk,*)'    Vpend,Veend ',Vpend,Veend
!          WRITE(fnumwrk,*)'    Vloss,Vlost,Vlosf ',Vloss,vlost,vlosf
!          WRITE(fnumwrk,*)'   Photoperiodism '
!          IF (Ppsen.EQ.'LQ') WRITE(fnumwrk,*)'    Ppexp  ',Ppexp   
!          WRITE(fnumwrk,*)'    Ppsen,Ppfpe ',Ppsen,Ppfpe   
!          WRITE(fnumwrk,*)'    Ppthr  ',Ppthr   
!          WRITE(fnumwrk,*)'    Pps1,2 ',Pps(1),Pps(2)
!          WRITE(fnumwrk,*)' '              
!          WRITE(fnumwrk,*)' GROWTH '              
!          WRITE(fnumwrk,*)' Photosynthesis '
!          WRITE(fnumwrk,*)'  Parue  ',Parue
!          WRITE(fnumwrk,*)'  Rsfpu,Rsfpl ',Rsfpu,Rsfpl
!          WRITE(fnumwrk,*)'  Parfc,Co2cc,Co2ex ',Parfc,Co2cc,Co2ex
!          WRITE(fnumwrk,*)'  Co2rf,   Co2f  '
!          Do l = 1,10
!            WRITE(fnumwrk,'(I8,F6.1)')Nint(Co2rf(l)),Co2f(l)
!          Enddo
!          WRITE(fnumwrk,*)' Partitioning     '
!          WRITE(fnumwrk,*)'  Ptfmx,Ptfmn  ',Ptfmx,Ptfmn
!          WRITE(fnumwrk,*)'  Ptfxs,Ptfa   ',Ptfxs,Ptfa
!          WRITE(fnumwrk,*)'  Paru2  ',Paru2
!          WRITE(fnumwrk,*)' CH2O reserves '
!          WRITE(fnumwrk,*)'  Rspca,Rspcx ',rspca,rspcx 
!          WRITE(fnumwrk,*)'  Rspclx      ',rspclx 
!          WRITE(fnumwrk,*)'  Rsuse       ',rsuse 
!          WRITE(fnumwrk,*)' Leaves     '
!          WRITE(fnumwrk,*)'  Phyllochron  '
!          WRITE(fnumwrk,*)'   Phints ',Phints               
!          WRITE(fnumwrk,*)'   Phl1,Phf1 ',Phintl(1),Phintf(1)
!          WRITE(fnumwrk,*)'   Phl2,Phf2 ',Phintl(2),Phintf(2)
!          WRITE(fnumwrk,*)'   Phl3,Phf3 ',Phintl(3),Phintf(3)
!          WRITE(fnumwrk,*)'  Lamina areas    '
!          WRITE(fnumwrk,*)'   Laxs   ',Laxs
!          WRITE(fnumwrk,*)'   La1s   ',La1s
!          WRITE(fnumwrk,*)'   Lafv   ',Lafv
!          WRITE(fnumwrk,*)'   Lafr   ',Lafr
!          WRITE(fnumwrk,*)'  Lamina weights '
!          WRITE(fnumwrk,*)'   Slas   ',Laws
!          WRITE(fnumwrk,*)'   Slacf,Slaff  ',Lawcf,Lawff
!          WRITE(fnumwrk,*)'  Sheaths '
!          WRITE(fnumwrk,*)'   Lshfr  ',Lshfr
!          WRITE(fnumwrk,*)'   Lshav,Lshar ',Lshav,Lshar
!          WRITE(fnumwrk,*)'  Longevity and senescence '
!          WRITE(fnumwrk,*)'   Llifg,a,s ',Llifg,Llifa,Llifs  
!          WRITE(fnumwrk,*)'   Lseni  ',Lseni                
!          WRITE(fnumwrk,*)'   Lsphs,Lsphsdu ',Lsphs,lsphsdu
!          WRITE(fnumwrk,*)'   Lsphe,Lsphedu ',Lsphe,lsphedu
!          WRITE(fnumwrk,*)'   Lwlos  ',Lwlos                
!          WRITE(fnumwrk,*)'   Llig%  ',Lligpc                
!          WRITE(fnumwrk,*)' Stems         '
!          WRITE(fnumwrk,*)'  Ssphs,Ssphe ',Ssphs,Ssphe
!          WRITE(fnumwrk,*)'  Saws   ',Saws                 
!          WRITE(fnumwrk,*)'  Slig%  ',Sligpc              
!          WRITE(fnumwrk,*)' Tillers  '
!          WRITE(fnumwrk,*)'  Shwts  ',g3
!          WRITE(fnumwrk,*)'  Til#x  ',Tilnox
!          WRITE(fnumwrk,*)'  Tgro2,Tgr20  ',Tgr(2),Tgr(20)
!          WRITE(fnumwrk,*)'  Til#s,Tilpe  ',ti1lf,Tilpe
!          WRITE(fnumwrk,*)'  Tifac  ',tifac
!          WRITE(fnumwrk,*)'  Tilip  ',tilip
!          WRITE(fnumwrk,*)'  Tdphs,Tdphe ',tdphs,Tdphe
!          WRITE(fnumwrk,*)'  Tdfac,Tdsf  ',Tdfac,Tdsf
!          WRITE(fnumwrk,*)' Chaff '
!          WRITE(fnumwrk,*)'  Chfrp  ',chfr 
!          WRITE(fnumwrk,*)' Grain '
!          WRITE(fnumwrk,*)'  Number '
!          WRITE(fnumwrk,*)'   G#wts  ',Gnowts
!          WRITE(fnumwrk,*)'   G#rf,G#rt   ',Gnorf,Gnort
!          WRITE(fnumwrk,*)'  Weight '
!          WRITE(fnumwrk,*)'   Gwts   ',Gwts 
!          WRITE(fnumwrk,*)'   Gwlag,Gwlin ',Gwlagfr,gwlinfr
!          WRITE(fnumwrk,*)'   Gwtat,Gwtaf ',Gwtat,gwtaf
!          WRITE(fnumwrk,*)'  Lignin '
!          WRITE(fnumwrk,*)'   Glig%  ',Gligpc
!          WRITE(fnumwrk,*)' Roots  '
!          WRITE(fnumwrk,*)'  Rdgs,Rdgaf   ',Rdgs,rdgaf
!          WRITE(fnumwrk,*)'  Rlwr,Rlig%   ',Rlwr,rligpc
!          WRITE(fnumwrk,*)'  Rresp,Rsen   ',Rresp,rsen
!          WRITE(fnumwrk,*)' Moisture contents      '
!          WRITE(fnumwrk,*)'  Gm%h   ',GMPCH           
!          WRITE(fnumwrk,*)' Canopy '
!          WRITE(fnumwrk,*)'  Morphology '
!          WRITE(fnumwrk,*)'   Htstd  ',Htstd
!          WRITE(fnumwrk,'(A10,3F5.1)')'    Cht%  ',Chtpc          
!          WRITE(fnumwrk,'(A10,3F5.1)')'    Cla%  ',Clapc          
!          WRITE(fnumwrk,*)'   Awns   ',Awns 
!          WRITE(fnumwrk,*)'   Laixx  ',Laixx             
!          WRITE(fnumwrk,*)'  Radiation interception'
!          WRITE(fnumwrk,*)'   Kcan   ',Kcan 
!          WRITE(fnumwrk,*)'   Tpar,Tsrad ',Tpar,Tsrad 
!          WRITE(fnumwrk,*)' Kill   '
!          WRITE(fnumwrk,*)'  Tkuh,Tkfh   ',Tkuh,Tkfh
!          WRITE(fnumwrk,*)'  Tkdlf,Tkdti ',Tkdlf,Tkdti
!          WRITE(fnumwrk,*)'  Tkspn   ',Tkspn
!          WRITE(fnumwrk,*)'  Hdur,Hlost,Hlosf ',Hdur,Hlost,Hlosf
!          WRITE(fnumwrk,*)'  Tkgf    ',Tkgf
!          WRITE(fnumwrk,*)' '        
!          WRITE(fnumwrk,*)' WATER ASPECTS'
!          WRITE(fnumwrk,*)'  Ratm,Rcrop,EOratio ',Ratm,Rcrop,Eoratio   
!          WRITE(fnumwrk,*)'  Rwupm(Pormin)',Rwupm      
!          WRITE(fnumwrk,*)'  Rwumx        ',Rwumx      
!          WRITE(fnumwrk,*)'  Llosa        ',Llosa      
!          WRITE(fnumwrk,*)' Water stress limits '
!          WRITE(fnumwrk,*)'  Wfpu,L ',WFPU,WFPL       
!          WRITE(fnumwrk,*)'  Wfgu,L ',WFGU,WFGL       
!          WRITE(fnumwrk,*)'  Wftu,L ',WFTU,WFTL       
!          WRITE(fnumwrk,*)'  Wfsu,Sf',WFSU,WFSF      
!          WRITE(fnumwrk,*)'  Wfeu   ',WFEU           
!          WRITE(fnumwrk,*)'  Wfgeu,Wfrgu ',Wfgeu,Wfrgu
!          WRITE(fnumwrk,*)' '        
!          WRITE(fnumwrk,*)' NITROGEN ASPECTS'
!          WRITE(fnumwrk,*)' Uptake '
!          WRITE(fnumwrk,*)'  Rtnup             ',rtnup       
!          WRITE(fnumwrk,*)'  Nupnf,Nupwf,Nupcf ',nupnf,nupwf,nupcf
!          WRITE(fnumwrk,*)'  No3mn,Nh4mn       ',no3mn,nh4mn
!          WRITE(fnumwrk,*)'  Ntupf             ',ntupf       
!          WRITE(fnumwrk,*)' Concentrations'
!          WRITE(fnumwrk,*)'  Sdn%        ',Sdnpci       
!          WRITE(fnumwrk,*)'  Ln%s,Ln%mn ',LnpcS,Lnpcmn    
!          WRITE(fnumwrk,*)'  Sn%s,Sn%mn ',Snpcs,Snpcmn           
!          WRITE(fnumwrk,*)'  Rn%s,Rn%mn ',Rnpcs,Rnpcmn    
!          WRITE(fnumwrk,*)'  Gn%s,Gn%mn,Gn%mx ',Gnpcs,Gnpcmn,Gnpcmx
!          WRITE(fnumwrk,*)' Stress limits '
!          WRITE(fnumwrk,*)'  Nfpu,l ',Nfpu,Nfpl       
!          WRITE(fnumwrk,*)'  Nfgu,l ',Nfgu,Nfgl       
!          WRITE(fnumwrk,*)'  Nftu,l ',Nftu,Nftl       
!          WRITE(fnumwrk,*)'  Nfsu,Sf',Nfsu,Nfsf      
!          WRITE(fnumwrk,*)'  Ncrg   ',Ncrg
!          WRITE(fnumwrk,*)' Mobilization'
!          WRITE(fnumwrk,*)'  Nlab%  ',Nlabpc
!          WRITE(fnumwrk,*)' '
!          WRITE(fnumwrk,*)' TEMPERATURE RESPONSES  '
!          WRITE(fnumwrk,'(A10,4F5.1)')'   Trgem  ',TRGEM           
!          WRITE(fnumwrk,'(A10,4F5.1)')'   Trdv1  ',TRDV1          
!          WRITE(fnumwrk,'(A10,4F5.1)')'   Trdv4  ',TRDV4          
!          WRITE(fnumwrk,'(A10,4F5.1)')'   Trdv8  ',TRDV8          
!          WRITE(fnumwrk,'(A10,4F5.1)')'   Trvrn  ',TRVRN          
!          WRITE(fnumwrk,'(A10,4F5.1)')'   Trphs  ',TRPHS          
!          WRITE(fnumwrk,'(A10,4F5.1)')'   Trlfg  ',TRLFG          
!          WRITE(fnumwrk,'(A10,4F5.1)')'   Trgfw  ',TRGFC           
!          WRITE(fnumwrk,'(A10,4F5.1)')'   Trgfn  ',TRGFN           
!          WRITE(fnumwrk,'(A10,4F5.1)')'   Trhar  ',TRCOH          
         
!          WRITE(FNUMWRK,*)' '
!          WRITE(FNUMWRK,'(A17)')' PHASE THRESHOLDS'
!          WRITE(FNUMWRK,'(A23)')'   PHASE START(DU) NAME'
!          DO L = 1,10
!            WRITE(FNUMWRK,'(I6,I10,3X,A13)')L,NINT(PSTART(L)),PSNAME(L)
!          ENDDO
         
!          IF (ISWDIS(LENDIS:LENDIS).NE.'N') THEN
!            WRITE (fnumwrk,*) ' '
!            WRITE (fnumwrk,*) 'DISEASE INITIATION AND GROWTH ASPECTS'
!            WRITE (fnumwrk,'(A13,A49)')'             ',
!     &       '  DATE   GROWTH FACTOR  FAVOURABILITY REQUIREMENT'
!            WRITE (fnumwrk,'(A12,I10,2F10.1)')
!     &       '  DISEASE 1 ',DIDAT(1),DIGFAC(1),DIFFACR(1)
!            WRITE (fnumwrk,'(A12,I10,2F10.1)')
!     &       '  DISEASE 2 ',DIDAT(2),DIGFAC(2),DIFFACR(2)
!            WRITE (fnumwrk,'(A12,I10,2F10.1)')
!     &       '  DISEASE 3 ',DIDAT(3),DIGFAC(3),DIFFACR(3)
!            WRITE (fnumwrk,*) ' '
!            IF (DCTAR(1).GT.0) WRITE (fnumwrk,*)
!     &       'DISEASE CONTROL DATES,GROWTH FACTORS,AND DURATION'
!            DO L=1,DCNX
!              IF (DCTAR(L).EQ.1) WRITE (fnumwrk,'(A12,I10,2F10.1)')
!     &         '  DISEASE 1 ',DCDAT(L),DCFAC(L),DCDUR(L)
!              IF (DCTAR(L).EQ.2) WRITE (fnumwrk,'(A12,I10,2F10.1)')
!     &         '  DISEASE 2 ',DCDAT(L),DCFAC(L),DCDUR(L)
!              IF (DCTAR(L).EQ.3) WRITE (fnumwrk,'(A12,I10,2F10.1)')
!     &         '  DISEASE 3 ',DCDAT(L),DCFAC(L),DCDUR(L)
!            ENDDO
!          ENDIF

!        ENDIF

!-----------------------------------------------------------------------
!       Set equivalences to avoid compile errors
!-----------------------------------------------------------------------
  
        tvr1 = tairhr(1)
        ! When running in CSM
        IF (FILEIOT.EQ.'DS4') THEN
          ALBEDO = ALBEDOS  ! Previously 0.2
          CLOUDS = 0.0
        ELSE
          ALBEDO = ALBEDOS  
        ENDIF

        ! Set flags

!-----------------------------------------------------------------------
!       Record starting values and files
!-----------------------------------------------------------------------

        CNI = CN
        KCANI = KCAN
        ONI = ON
        RNI = RN
        RWUMXI = RWUMX
        SNI = SN
        TNI = TN
        KEPI = KEP

!-----------------------------------------------------------------------
!       Create and write warning messages re. input and output variables
!-----------------------------------------------------------------------

!        WRITE(FNUMWRK,*)' '
!        WRITE(FNUMWRK,'(A22)')' OUTPUTS              '

        ! Control switch for OUTPUT file names
        CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'FNAME',fname)
!        IF (FNAME.EQ.'Y') THEN
!          WRITE(FNUMWRK,*)' File names switched from standard. '
!        ELSE  
!          WRITE(FNUMWRK,*)' Using standard file names. '
!        ENDIF   
        
        ! Output choices
        ! 1=Reserves in laminae,stem,crown,SLA;stem includes sheaths
        ! 2=No reserves;stem includes sheaths
        ! 3=No reserves;stem does not include sheaths 
        OUTCHOICE = 1
        IF (OUTCHOICE.EQ.2) THEN
          WRITE(MESSAGE(1),'(A34,A37)')
     &      'NO reserve CH2O included in the wt',
     &      ' of leaves,stems,and crown.          ' 
          CALL WARNING(1,'CSCAS',MESSAGE)
!          WRITE(Fnumwrk,*)' '
!          WRITE(FNUMWRK,'(A35,A38)')
!     &      ' NO reserve CH2O included in the wt',
!     &      ' of leaves,stems,and crown.           '
!          WRITE(FNUMWRK,'(A35)')
!     &      ' Stem weight includes the sheaths..'
        ELSEIF (OUTCHOICE.EQ.3) THEN
          WRITE(MESSAGE(1),'(A34,A37)')
     &      'NO reserve CH2O included in the wt',
     &      ' of leaves,stems,and crown.          ' 
          CALL WARNING(1,'CSCAS',MESSAGE)
!          WRITE(Fnumwrk,*)' '
!          WRITE(FNUMWRK,'(A35,A38)')
!     &      ' NO reserve CH2O included in the wt',
!     &      ' of leaves,stems,and crown.           '
!          WRITE(FNUMWRK,'(A44)')
!     &      ' Stem weight does NOT includes the sheaths. '
        ENDIF  

!        WRITE(FNUMWRK,*)' '
!        WRITE(FNUMWRK,'(A22)')' DURING RUN STATUS:   '
      END SUBROUTINE CRP_SeasInit
