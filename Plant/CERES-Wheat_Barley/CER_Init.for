!***********************************************************************
! This is the code from the section (DYNAMIC == RUNINIT & SEASINIT) 
! lines 1420 - 3522 of the original CSCER code.
!***********************************************************************

      SUBROUTINE CER_Init (LAI, CANHT,
     &     CN, DOY, HARVFRAC,
     &     FILEIOIN, FROP, IDETL,
     &     KCAN, KEP, NFP, ON,
     &     RESCALG, RESLGALG, RESNALG, RLV, RN, RNMODE,
     &     RUN, RUNI, RWUMX, RWUPM, 
     &     UH2O, YEAR, SLPF, SN,
     &     STGDOY, TN, TRWUP, DYNAMIC)

! 2023-01-25 CHP removed unused variables from argument list
!     ISWWAT, ISWNIT, SENCALG, UNH4ALG, UNO3ALG, SENNALG

        USE ModuleDefs
        USE CER_First_Trans_m
        IMPLICIT NONE
        EXTERNAL YR_DOY, GETLUN, Y4K_DOY, TVILENT, LTRIM, XREADC, 
     &    XREADT, SPREADRA, XREADI, XREADR, UCASE, XREADIA, XREADRA, 
     &    FVCHECK, FINDDIR, CUREADC, CUREADR, ECREADR, SPREADR, 
     &    CER_INIT_VARINIT
        SAVE
        
        INTEGER CN, DOY, FROP, ON, RN, RUN, RUNI, SN, TN
        
        INTEGER YEAR, STGDOY(20), DYNAMIC, TVILENT
        
        REAL LAI, CANHT, DEWDUR, HARVFRAC(2)  !, CLOUDS
        REAL KCAN, KEP, NFP
        REAL RESCALG(0:20), RESLGALG(0:20), RESNALG(0:20), RLV(20)
        REAL SLPF !SENCALG(0:20), SENNALG(0:20), 
        REAL RWUMX, RWUPM
        REAL TRWUP, UH2O(NL) !, UNH4ALG(NL), UNO3ALG(NL)
        
        CHARACTER*1   IDETL, RNMODE !ISWNIT, ISWWAT
        CHARACTER*250 FILEIOIN  
               
        FROPADJ = FROP
        IF (RNMODE.EQ.'T') FROPADJ = 1
        IF (IDETL.EQ.'D'.OR.IDETL.EQ.'A') FROPADJ = 1
        
        ! Following is used because Dssat has seasinit after runinit

        IF (CFLINIT.EQ.'Y') THEN
          TN = TNI
          CN = CNI
          SN = SNI
          ON = ONI
          RN = RNI
          KCAN = KCANI
          KEP = KEPI
          RWUPM = RWUPMI
          RWUMX = RWUMXI
          STGDOY = 9999999
          RETURN
        ENDIF

         ! Open Work.out file
!         IF (FNUMWRK.LE.0.OR.FNUMWRK.GT.1000) 
!      &    CALL Getlun ('WORK.OUT',fnumwrk)
!         INQUIRE (FILE = 'WORK.OUT',OPENED = fopen)
!         IF (.NOT.fopen) THEN
!           IF (RUN.EQ.1) THEN
!             OPEN (UNIT = fnumwrk,FILE = 'WORK.OUT')
!             WRITE(fnumwrk,*) 'CSCER  Cropsim-Ceres Crop Module '
!           ELSE
!             OPEN (UNIT = fnumwrk,FILE = 'WORK.OUT',POSITION='APPEND',
!      &      ACTION = 'READWRITE')
!             WRITE(fnumwrk,*) ' '
!             WRITE(fnumwrk,*) 'CSCER  Cropsim-Ceres Crop Module '
!           ENDIF
!         ENDIF

! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.          
!        ! Set Reads file #
!        IF (FNUMREA.LE.0.OR.FNUMREA.GT.1000) 
!     &      CALL Getlun('READS.OUT',fnumrea)
        ! Set temporary file #
        IF (FNUMTMP.LE.0.OR.FNUMTMP.GT.1000) 
     &      CALL Getlun ('FNAMETMP',fnumtmp)
      
        ! Set input file type
        TVI1 = TVILENT(FILEIOIN)
        IF (FILEIOIN(TVI1-2:TVI1).EQ.'INP') THEN
          FILEIOIN(TVI1:TVI1) = 'H'
          fileiot = 'DS4'
        ELSE
          fileiot = 'XFL'
        ENDIF
        FILEIO = ' '
        FILEIO(1:TVI1) = FILEIOIN(1:TVI1)

        IF (DYNAMIC.EQ.RUNINIT) THEN

          MODNAME = 'CSCER048'
          VERSIONCSCER = 010115
          GENFLCHK(3:15) = 'CER048.20200721'

          ! Parameters
          STDAY = 20.0    ! TT in standard day
          PARADFAC = 0.5  ! PAR in SRAD (fr)
          STEPNUM = 1     ! Step number per day set to 1

          YEARSIM = YEARDOY

          ! Control switches for error outputs and input echo
          IF (FILEIOT.NE.'DS4') THEN
            CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'OUTPUTS',idetou)
          ENDIF

        ENDIF

!-----------------------------------------------------------------------
!       Initialize both state and rate variables                       
!-----------------------------------------------------------------------
        
        CALL CER_Init_VarInit (LAI, CANHT, DEWDUR,
     &   NFP, RESCALG, RESLGALG, RESNALG, RLV,
     &   STGDOY, TRWUP, UH2O)
!-----------------------------------------------------------------------


        IF (RUN.EQ.1.AND.RUNI.LE.1) THEN
          CFGDFILE = ' '
          CUFILE = ' '
          CUDIRFLE = ' '
          CROPP = '  '
        ENDIF

        ! Planting information
        CALL XREADC(FILEIO,TN,RN,SN,ON,CN,'PLANT',iplti)
!       IF(IPLTI.EQ.'A')THEN
        IF(IPLTI.EQ.'A' .OR. IPLTI.EQ.'F')THEN
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'PFRST',pwdinf)
C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY          
          !PWDINF = CSYEARDOY(pwdinf)
          CALL Y4K_DOY(pwdinf,FILEIO,0,ERRKEY,3)
!          CALL CSYR_DOY(PWDINF,PWYEARF,PWDOYF)
          CALL YR_DOY(PWDINF,PWYEARF,PWDOYF)
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'PLAST',pwdinl)
          !PWDINL = CSYEARDOY(pwdinl)
          CALL Y4K_DOY(pwdinl,FILEIO,0,ERRKEY,3)
!          CALL CSYR_DOY(PWDINL,PWYEARL,PWDOYL)
          CALL YR_DOY(PWDINL,PWYEARL,PWDOYL)
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'PH2OL',swpltl)
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'PH2OU',swplth)
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'PH2OD',swpltd)
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'PSTMX',ptx)
          CALL XREADR(FILEIO,TN,RN,SN,ON,CN,'PSTMN',pttn)
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'HFRST',hfirst)
          !HFIRST = CSYEARDOY(hfirst)
          CALL Y4K_DOY(hfirst,FILEIO,0,ERRKEY,3)
!          CALL CSYR_DOY(HFIRST,HYEARF,HDOYF)
          CALL YR_DOY(HFIRST,HYEARF,HDOYF)
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'HLAST',hlast)
          !HLAST = CSYEARDOY(hlast)
          CALL Y4K_DOY(hlast,FILEIO,0,ERRKEY,3)
!          CALL CSYR_DOY(HLAST,HYEARL,HDOYL)
          CALL YR_DOY(HLAST,HYEARL,HDOYL)
          !IF (DYNAMIC.EQ.SEASINIT) THEN
          ! IF (PWDINF.GT.0 .AND. PWDINF.LT.YEARDOY) THEN
          !   WRITE (fnumwrk,*) ' '
          !   WRITE (fnumwrk,*) 'Automatic planting set: '              
          !   WRITE (fnumwrk,*)'PFIRST,PLAST AS READ  : ',pwdinf,pwdinl
          !   WRITE (fnumwrk,*)'HFIRST,HLAST AS READ  : ',hfirst,hlast
          !   TVI1 = INT((YEARDOY-PWDINF)/1000)
          !   PWDINF = PWDINF + TVI1*1000
          !   PWDINL = PWDINL + TVI1*1000
          !   IF (HFIRST.GT.0) HFIRST = HFIRST + TVI1*1000
          !   HLAST  = HLAST + (TVI1+1)*1000
          ! ENDIF
          ! WRITE (fnumwrk,*) 'PFIRST,PLAST AS USED  : ',pwdinf,pwdinl
          ! WRITE (fnumwrk,*) 'HFIRST,HLAST AS USED  : ',hfirst,hlast
          !ENDIF
        ELSE
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'PDATE',pdate)
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'EDATE',emdatm)
!          CALL CSYR_DOY(PDATE,PLYEAR,PLDAY)
          CALL YR_DOY(PDATE,PLYEAR,PLDAY)
          PLYEARREAD = PLYEAR
          
!           CHP 2/13/2009 - increment yr for seasonal and sequence runs
!           IF (INDEX('FQN',RNMODE) > 0) THEN
!           What about multiple years with other RNMODE's?  This only 
!             fixes sequence and seasonal runs.
!           CHP 5/4/09 - for DSSAT runs, always set PLYEAR = YEAR
!           09/28/2009 CHP need to account for planting date >> simulation date.
!            IF (FILEIOT(1:2).EQ.'DS') THEN
!          LPM 07/17/20 - account for simulation date when is a year before planting date
!          Avoid wrong value of yeardoyharf
          IF (FILEIOT(1:2) == 'DS' .AND. YEAR > PLYEAR) THEN
              IF (YEAR < PLYEARREAD) THEN
                  PLYEAR = PLYEARREAD
              ELSE
                  PLYEAR = YEAR
              ENDIF
          ENDIF

          IF (PLDAY.GE.DOY) THEN
            YEARPLTP = PLYEAR*1000 + PLDAY
          ELSEIF (PLDAY.LT.DOY) THEN
            YEARPLTP = (YEAR+1)*1000 + PLDAY
          ENDIF
        ENDIF

        arg = ' '
        tvi2 = 0
        tvi3 = 0
        tvi4 = 0
        call getarg(0,arg)
        arglen = len_trim(arg)

        DO tvi1 = 1,arglen
          IF (arg(tvi1:tvi1).EQ. SLASH) tvi2=tvi1
          IF (arg(tvi1:tvi1).EQ.'.') tvi3=tvi1
          IF (arg(tvi1:tvi1).EQ.' ' .AND. tvi4.EQ.0) tvi4=tvi1
        ENDDO
        IF (TVI3.EQ.0 .AND. TVI4.GT.0) THEN
          tvi3 = tvi4
        ELSEIF (TVI3.EQ.0 .AND. TVI4.EQ.0) THEN
          tvi3 = arglen+1
        ENDIF
        MODEL = ARG(TVI2+1:TVI3-1)
        CALL UCASE(MODEL)
        
        ! Re-open Work.out and Reads.out if only require 1 run info..   
!         IF (IDETL.EQ.'0'.OR.IDETL.EQ.'Y'.OR.IDETL.EQ.'N') THEN
!           CLOSE (FNUMWRK, STATUS = 'DELETE')
!           OPEN (UNIT = FNUMWRK,FILE = 'WORK.OUT', STATUS = 'NEW',
!      &      ACTION = 'READWRITE')
!           WRITE(FNUMWRK,*) 'CSCER  Cropsim-Ceres Crop Module '
! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.          
!          CLOSE (FNUMREA, STATUS = 'DELETE')
!          OPEN (UNIT = FNUMREA,FILE = 'READS.OUT', STATUS = 'NEW',
!     &      ACTION = 'READWRITE')
!          WRITE(FNUMREA,*)' '
!          WRITE(FNUMREA,*)
!     &      ' File closed and re-opened to avoid generating huge file'
!         ENDIF

        ! Create composite run variable
        IF (RUNI.LT.10) THEN
          WRITE (RUNRUNI,'(I3,A1,I1,A3)') MOD(RUN,1000),',',RUNI,'   '
        ELSEIF (RUNI.GE.10.AND.RUNI.LT.100) THEN
          WRITE (RUNRUNI,'(I3,A1,I2,A2)') MOD(RUN,1000),',',RUNI,'  '
        ELSE
          WRITE (RUNRUNI,'(I3,A1,I3,A1)') MOD(RUN,1000),',',RUNI,' '
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

        CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'CR',crop)
        CALL UCASE (CROP)
        GENFLCHK = CROP//GENFLCHK(3:15)
        CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'INGENO',varno)
        IF (varno.EQ.'-99   ') THEN
!          WRITE(fnumwrk,*)' '
!          WRITE(fnumwrk,*)'Cultivar number not found!'
!          WRITE(fnumwrk,*)'Maybe an error in the the X-file headings'
!          WRITE(fnumwrk,*)'(eg.@-line dots connected to next header)'
!          WRITE(fnumwrk,*)'Please check'
          WRITE (*,*) ' Problem reading the X-file'
          WRITE (*,*) ' Cultivar number not found!'
          WRITE (*,*) ' Maybe an error in the the X-file headings'
          WRITE (*,*) ' (eg.@-line dots connected to next header)'
          WRITE (*,*) ' Program will have to stop'
          !WRITE (*,*) ' Check WORK.OUT for details of run'
          STOP ' '
        ENDIF
        IF (varno.EQ.'-99   ') THEN
!          WRITE(fnumwrk,*)' '
!          WRITE(fnumwrk,*)'Cultivar number not found!'
!          WRITE(fnumwrk,*)'Maybe an error in the the X-file headings'
!          WRITE(fnumwrk,*)'(eg.@-line dots connected to next header)'
!          WRITE(fnumwrk,*)'Please check'
          WRITE (*,*) ' Problem reading the X-file'
          WRITE (*,*) ' Cultivar number not found!'
          WRITE (*,*) ' Maybe an error in the the X-file headings'
          WRITE (*,*) ' (eg.@-line dots connected to next header)'
          WRITE (*,*) ' Program will have to stop'
          !WRITE (*,*) ' Check WORK.OUT for details of run'
          STOP ' '
        ENDIF
        CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'CNAME',vrname)
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPOP',pltpopp)
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PLRS',rowspc)
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PLDP',sdepth)
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PLWT',sdrate)
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PLMAG',plmage)
        IF (PLMAGE.LE.-90.0) THEN
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PAGE',plmage)
          IF (PLMAGE.LE.-99.0) PLMAGE = 0.0
        ENDIF

        CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'TNAME',tname)
        CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'SNAME',runname)

        CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'ENAME',ename)
        CALL LTRIM (ENAME)
        CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'EXPER',excode)
        CALL UCASE (EXCODE)

        CALL XREADI (FILEIO,TN,RN,SN,ON,CN,'HDATE',yrharf)
        !YEARHARF = CSYEARDOY(yrharf)
C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY        
        CALL Y4K_DOY(yrharf,FILEIO,0,ERRKEY,3)
!        CALL CSYR_DOY(YRHARF,HYEAR,HDAY)
        CALL YR_DOY(YRHARF,HYEAR,HDAY)
        PLTOHARYR = HYEAR - PLYEARREAD
        ! Upgrade harvest date for seasonal and sequential runs
        yearharf = (plyear+pltoharyr)*1000 +hday
        
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'HPC',hpc)
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'HBPC',hbpc)
        CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'HARVS',ihari)
        IF (hpc .LT. 0.0) hpc = 100.0   ! Harvest %
        IF (hbpc .LT. 0.0) hbpc = 0.0
        ! If running CSM use harvfrac so as to handle automatic mngement
        IF (FILEIOT .NE. 'DS4') THEN
          hpc = harvfrac(1)*100.0   ! Harvest %
          hbpc = harvfrac(2)*100.0
        ENDIF  

        ! Read fertilizer info (for calculation of N appln during cycle)
        CALL XREADC(FILEIO,TN,RN,SN,ON,CN,'FERTI',iferi)
        CALL XREADIA(FILEIO,TN,RN,SN,ON,CN,'FDATE','200',fday)
        CALL XREADRA(FILEIO,TN,RN,SN,ON,CN,'FAMN','200',anfer)
        NFERT = 0
        DO I = 1, 200
          IF (anfer(I).LE.0.0) EXIT
C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY          
          !FDAY(I) = CSYEARDOY(fday(i))
          CALL Y4K_DOY(fday(i),FILEIO,0,ERRKEY,3)
          NFERT = NFERT + 1
        ENDDO

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

        IF (FILEIOT(1:2).EQ.'DS') THEN
          !IF (CROP.NE.CROPP .OR. VARNO.NE.VARNOP) THEN
            CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'CFILE',cufile)
            CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'CDIR',pathcr)

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

            CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'EFILE',ecfile)
            CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'EDIR',pathcr)

            PATHL = INDEX(PATHCR,BLANK)
            IF (PATHL.LE.5.OR.PATHCR(1:3).EQ.'-99') THEN
              ECDIRFLE = ECFILE
            ELSE
              IF (PATHCR(PATHL-1:PATHL-1) .NE. SLASH) THEN
                ECDIRFLE = PATHCR(1:(PATHL-1)) // SLASH // ECFILE
              ELSE
                ECDIRFLE = PATHCR(1:(PATHL-1)) // ECFILE
              ENDIF
            ENDIF

            CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'SPFILE',spfile)
            CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'SPDIR',pathcr)

            PATHL = INDEX(PATHCR,BLANK)
            IF (PATHL.LE.5.OR.PATHCR(1:3).EQ.'-99') THEN
              SPDIRFLE = SPFILE
            ELSE
              IF (PATHCR(PATHL-1:PATHL-1) .NE. SLASH) THEN
                SPDIRFLE = PATHCR(1:(PATHL-1)) // SLASH // SPFILE
              ELSE
                SPDIRFLE = PATHCR(1:(PATHL-1)) // SPFILE
              ENDIF
            ENDIF
          !ENDIF
 
          IF (RNMODE.NE.'T') CALL FVCHECK(CUDIRFLE,GENFLCHK)

          CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'ECO#',econo)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P1V',p1v)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P1D',p1d)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P5',pd(5))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'G1',g1cwt)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'G2',g2kwt)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'G3',g3)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PHINT',phints)
          ! Eco characteristics that may temporarily added to cul file
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P1',pd(1))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P2',pd(2))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P2FR1',pd2fr(1))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P3',pd(3))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P4',pd(4))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P4FR1',pd4fr(1))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P4FR2',pd4fr(2))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'VEFF',veff)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PARUE',paruv)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PARU2',parur)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LA1S',lapot(1))
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'SLAS',laws)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LAFV',lafv)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LAFR',lafr)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'TBAM',tbam)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'GN%S',grns)
          ! N uptake variables in species file
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NCNU',ncnu)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'RLFNU',rlfnu)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'WFNUU',wfnuu)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFPU',nfpu)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFGU',nfgu)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFTU',nftu)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFPL',nfpl)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFGL',nfgl)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFTL',nftl)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'RTNO3',rtno3)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'RTNH4',rtnh4)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NSFAC',nsfac)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NMNFC',nmnfc)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NLAB%',xnfs)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'RDGS',rdgs1)
          CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'GN%MN',grnmn)
!           NB. TBAM is only used experimentally;should not be in coeff.files
          
          CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'ADIR',fileadir)

        ELSE

          IF (CUDIRFLE.NE.CUDIRFLP .OR. VARNO.NE.VARNOP) THEN
            arg=' '
            tvi3=0
            ! CALL GETARG (0,ARG,ARGLEN)
            ! Portability
            call getarg(0,arg)
            arglen = len_trim(arg)

            DO tvi1 = 1,arglen
              IF(arg(tvi1:tvi1).EQ. SLASH)tvi2=tvi1
              IF(arg(tvi1:tvi1).EQ.'.')tvi3=tvi1
            ENDDO
            IF(TVI3.EQ.0)then
              tvi3=arglen+1
              ARG(TVI3:TVI3)='.'
            ENDIF
            cfgdfile = ' '
            cfgdfile = ARG(1:TVI3)//'CFG'
            ! Change cfg file name from module specific to general
            DO L = LEN(TRIM(CFGDFILE)),1,-1
              IF (CFGDFILE(L:L).EQ. SLASH) EXIT
            ENDDO
            IF (L.GT.1) THEN
              cfgdfile = CFGDFILE(1:L-1)// SLASH //'CROPSIM.CFG'
            ELSE
              cfgdfile(1:11) = 'CROPSIM.CFG'
            ENDIF
            INQUIRE (FILE = cfgdfile,EXIST = fflag)
            IF (.NOT.fflag) THEN
!              WRITE (fnumwrk,*)
!     &         'Could not find Cfgdfile: ',cfgdfile(1:60)
              WRITE (*,*) ' Could not find Cfgdfile: ',cfgdfile(1:60)
              WRITE (*,*) ' Program will have to stop'
              !WRITE (*,*) ' Check WORK.OUT for details of run'
              STOP ' '
            ELSE
!              WRITE (fnumwrk,*) ' Config.file: ',CFGDFILE(1:60)
            ENDIF

            cufile = crop//modname(3:8)//'.CUL'
            INQUIRE (FILE = cufile,EXIST = fflag)
            IF (fflag) THEN
              cudirfle = cufile
            ELSE
              CALL Finddir (fnumtmp,cfgdfile,'CRD',cufile,cudirfle)
            ENDIF
            !IF (RNMODE.EQ.'G'.OR.RNMODE.EQ.'T') THEN  LAH
            !  CUFILE = 'GENCALC2.CUL'
            !  CUDIRFLE = ' '
            !  CUDIRFLE(1:12) = CUFILE
            !ENDIF

            ecfile = crop//modname(3:8)//'.ECO'
            INQUIRE (FILE = ecfile,EXIST = fflag)
            IF (fflag) THEN
              ecdirfle = ecfile
            ELSE
              CALL Finddir (fnumtmp,cfgdfile,'CRD',ecfile,ecdirfle)
            ENDIF

            spfile = crop//modname(3:8)//'.SPE'
            INQUIRE (FILE = ecfile,EXIST = fflag)
            IF (fflag) THEN
              spdirfle = spfile
            ELSE
              CALL Finddir (fnumtmp,cfgdfile,'CRD',spfile,spdirfle)
            ENDIF
          ENDIF

        ENDIF     ! End Genotype file names creation

        IF (RNMODE.EQ.'G'.OR.RNMODE.EQ.'T') THEN
          CUFILE = 'GENCALC2.CUL'
          CUDIRFLE = ' '
          CUDIRFLE(1:12) = CUFILE
        ENDIF

        IF (FILEIOT .NE. 'DS4') THEN
          !IF (CUDIRFLE.NE.CUDIRFLP .OR. VARNO.NE.VARNOP) THEN
           IF (RNMODE.NE.'T') CALL FVCHECK(CUDIRFLE,GENFLCHK)
!            WRITE (fnumwrk,*) ' '
            CALL CUREADC (CUDIRFLE,VARNO,'ECO#',econo)
            CALL CUREADR (CUDIRFLE,VARNO,'P1V',p1v)
            CALL CUREADR (CUDIRFLE,VARNO,'P1D',p1d)
            CALL CUREADR (CUDIRFLE,VARNO,'P5',pd(5))
            CALL CUREADR (CUDIRFLE,VARNO,'G1',g1cwt)
            CALL CUREADR (CUDIRFLE,VARNO,'G2',g2kwt)
            CALL CUREADR (CUDIRFLE,VARNO,'G3',g3)
            CALL CUREADR (CUDIRFLE,VARNO,'PHINT',phints)
          ! Eco characteristics that may temporarily added to cul file
            CALL CUREADR (CUDIRFLE,VARNO,'P1',pd(1))
            CALL CUREADR (CUDIRFLE,VARNO,'P2',pd(2))
            CALL CUREADR (CUDIRFLE,VARNO,'P2FR1',pd2fr(1))
            CALL CUREADR (CUDIRFLE,VARNO,'P3',pd(3))
            CALL CUREADR (CUDIRFLE,VARNO,'P4',pd(4))
            CALL CUREADR (CUDIRFLE,VARNO,'P4FR1',pd4fr(1))
            CALL CUREADR (CUDIRFLE,VARNO,'P4FR2',pd4fr(2))
            CALL CUREADR (CUDIRFLE,VARNO,'VEFF',veff)
            CALL CUREADR (CUDIRFLE,VARNO,'PARUE',paruv)
            CALL CUREADR (CUDIRFLE,VARNO,'PARU2',parur)
            CALL CUREADR (CUDIRFLE,VARNO,'LA1S',lapot(1))
            CALL CUREADR (CUDIRFLE,VARNO,'SLAS',laws)
            CALL CUREADR (CUDIRFLE,VARNO,'LAFV',lafv)
            CALL CUREADR (CUDIRFLE,VARNO,'LAFR',lafr)
            CALL CUREADR (CUDIRFLE,VARNO,'TBAM',tbam)
            CALL CUREADR (CUDIRFLE,VARNO,'GN%S',grns)
            ! N uptake variables in species file
            CALL CUREADR (CUDIRFLE,VARNO,'NCNU',ncnu)
            CALL CUREADR (CUDIRFLE,VARNO,'RLFNU',rlfnu)
            CALL CUREADR (CUDIRFLE,VARNO,'WFNUU',wfnuu)
            CALL CUREADR (CUDIRFLE,VARNO,'NFPL',nfpl)
            CALL CUREADR (CUDIRFLE,VARNO,'NFGL',nfgl)
            CALL CUREADR (CUDIRFLE,VARNO,'NFTL',nftl)
            CALL CUREADR (CUDIRFLE,VARNO,'NFPU',nfpu)
            CALL CUREADR (CUDIRFLE,VARNO,'NFGU',nfgu)
            CALL CUREADR (CUDIRFLE,VARNO,'NFTU',nftu)
            CALL CUREADR (CUDIRFLE,VARNO,'RTNO3',rtno3)
            CALL CUREADR (CUDIRFLE,VARNO,'RTNH4',rtnh4)
            CALL CUREADR (CUDIRFLE,VARNO,'NSFAC',nsfac)
            CALL CUREADR (CUDIRFLE,VARNO,'NMNFC',nmnfc)
            CALL CUREADR (CUDIRFLE,VARNO,'NLAB%',xnfs)
            CALL CUREADR (CUDIRFLE,VARNO,'RDGS',rdgs1)
            CALL CUREADR (CUDIRFLE,VARNO,'GN%MN',grnmn)
!             NB. TBAM is only used experimentally;should not be in coeff.files
!             Below are 3.5 expressions
!            P1V = P1V*0.0054545 + 0.0003
!            P1D = P1D*0.002
!            PD(5) = 430.0 + PD(5)*20.00
!            IF (CROP.EQ.'BA') PD(5) = 300.0 + PD(5)*40.00
!            IF (G1 .NE. 0.0) G1 = 5.0 + G1* 5.00
!            IF (G2 .NE. 0.0) G2 = 0.65 + G2* 0.35
!            IF (G3 .NE. 0.0) G3 = -0.005 + G3* 0.35
          !ENDIF
        ENDIF

        IF (RNMODE.NE.'T') CALL FVCHECK(ECDIRFLE,GENFLCHK)
        IF (PD(1).LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'P1',PD(1))
        IF (PD2FR(1).LE.0)CALL ECREADR (ECDIRFLE,ECONO,'P2FR1',PD2FR(1))
        IF (PD(2).LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'P2',PD(2))
        IF (PD(3).LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'P3',PD(3))
        IF (PD(4).LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'P4',PD(4))
        IF (PD4FR(1).LE.0)CALL ECREADR (ECDIRFLE,ECONO,'P4FR1',PD4FR(1))
        IF (PD4FR(2).LE.0)CALL ECREADR (ECDIRFLE,ECONO,'P4FR2',PD4FR(2))
        IF (PARUV.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'PARUE',paruv)
        IF (PARUR.LT.-92.0) CALL ECREADR (ECDIRFLE,ECONO,'PARU2',parur)
        IF (LAPOT(1).LE.0.0) 
     &   CALL ECREADR (ECDIRFLE,ECONO,'LA1S',lapot(1))
        IF (LAWS.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'SLAS',laws)
        IF (LAFV.LE.0.0)  CALL ECREADR (ECDIRFLE,ECONO,'LAFV',lafv)
        IF (LAFR.LE.0.0)  CALL ECREADR (ECDIRFLE,ECONO,'LAFR',lafr)
        IF (VEFF.LT.0.0)  CALL ECREADR (ECDIRFLE,ECONO,'VEFF',veff)
        IF (GRNMN.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'GN%MN',grnmn)
        IF (GRNS.LT.0.0)  CALL ECREADR (ECDIRFLE,ECONO,'GN%S',grns)
        IF (NFPU.LT.0.0)  CALL ECREADR (ECDIRFLE,ECONO,'NFPU',nfpu)
        IF (NFPL.LT.0.0)  CALL ECREADR (ECDIRFLE,ECONO,'NFPL',nfpl)
        IF (NFGU.LT.0.0)  CALL ECREADR (ECDIRFLE,ECONO,'NFGU',nfgu)
        IF (NFGL.LT.0.0)  CALL ECREADR (ECDIRFLE,ECONO,'NFGL',nfgl)
        IF (RDGS1.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'RDGS',rdgs1)
        IF (RTNO3.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'RTNO3',rtno3)
        IF (RTNH4.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'RTNH4',rtnh4)
        CALL ECREADR (ECDIRFLE,ECONO,'PPFPE',ppfpe)
        CALL ECREADR (ECDIRFLE,ECONO,'SGPHE',P4SGE)
        CALL ECREADR (ECDIRFLE,ECONO,'HTSTD',canhts)
        CALL ECREADR (ECDIRFLE,ECONO,'AWNS',awns)
        CALL ECREADR (ECDIRFLE,ECONO,'KCAN',kcan)
        CALL ECREADR (ECDIRFLE,ECONO,'LLIFE',tvr1)
        LLIFE = INT(TVR1)
        CALL ECREADR (ECDIRFLE,ECONO,'RS%S',rspcs)
        CALL ECREADR (ECDIRFLE,ECONO,'TIL#S',ti1lf)
        CALL ECREADR (ECDIRFLE,ECONO,'WFPU',wfpu)
        CALL ECREADR (ECDIRFLE,ECONO,'WFPGF',wfpgf)
        CALL ECREADR (ECDIRFLE,ECONO,'WFGU',wfgu)
        CALL ECREADR (ECDIRFLE,ECONO,'TKFH',lt50h)
        CALL ECREADR (ECDIRFLE,ECONO,'RDG2',rdgs2)
        CALL ECREADR (ECDIRFLE,ECONO,'TIFAC',tifac)
        CALL ECREADR (ECDIRFLE,ECONO,'TIPHE',tilpe)
        CALL ECREADR (ECDIRFLE,ECONO,'TDPHS',tilds)
        CALL ECREADR (ECDIRFLE,ECONO,'TDPHE',tilde)
        CALL ECREADR (ECDIRFLE,ECONO,'TDFAC',tildf)
        CALL ECREADR (ECDIRFLE,ECONO,'LSPHS',lsens)
        CALL ECREADR (ECDIRFLE,ECONO,'LSPHE',lsene)
        CALL ECREADR (ECDIRFLE,ECONO,'PHF3',phintf(3))
        CALL ECREADR (ECDIRFLE,ECONO,'PHL2',phintl(2))

        CALL FVCHECK(SPDIRFLE,GENFLCHK)
        IF (PD4FR(1).LE.0.0) CALL SPREADR (SPDIRFLE,'P4FR1',PD4FR(1))
        IF (PD4FR(2).LE.0.0) CALL SPREADR (SPDIRFLE,'P4FR2',PD4FR(2))
        IF (ppfpe.LT.0.0)    CALL SPREADR (SPDIRFLE,'PPFPE',ppfpe)
        IF (LSENS.LE.0.0)    CALL SPREADR (SPDIRFLE,'LSPHS',lsens)
        IF (LSENE.LE.0.0)    CALL SPREADR (SPDIRFLE,'LSPHE',lsene)
        IF (LWLOS.LE.0.0)    CALL SPREADR (SPDIRFLE,'LWLOS',lwlos)
        IF (TILDF.LT.0.0)    CALL SPREADR (SPDIRFLE,'TDFAC',tildf)
        IF (PD2FR(1).LE..0)  CALL SPREADR (SPDIRFLE,'P2FR1',pd2fr(1))
        IF (P4SGE.LE..0)     CALL SPREADR (SPDIRFLE,'SGPHE',p4sge)
        IF (NFGU.LE.0.0)     CALL SPREADR (SPDIRFLE,'NFGU',nfgu)
        IF (NFGL.LT.0.0)     CALL SPREADR (SPDIRFLE,'NFGL',nfgl)
        IF (NFPU.LE.0.0)     CALL SPREADR (SPDIRFLE,'NFPU',nfpu)
        IF (NFPL.LT.0.0)     CALL SPREADR (SPDIRFLE,'NFPL',nfpl)
        IF (WFPU.LE.0.0)     CALL SPREADR (SPDIRFLE,'WFPU',wfpu)
        IF (WFPGF.LE.0.0)    CALL SPREADR (SPDIRFLE,'WFPGF',wfpgf)
        IF (WFGU.LT.0.0)     CALL SPREADR (SPDIRFLE,'WFGU',wfgu)
        IF (LLIFE.LE.0)      CALL SPREADR (SPDIRFLE,'LLIFE',tvr1)
        LLIFE = INT(TVR1)
        IF (NFTU.LT.0.0)     CALL SPREADR (SPDIRFLE,'NFTU',nftu)
        IF (NFTL.LT.0.0)     CALL SPREADR (SPDIRFLE,'NFTL',nftl)
        IF (WFSU.LT.0.0)     CALL SPREADR (SPDIRFLE,'WFS',wfsu)
        IF (NFSU.LT.0.0)     CALL SPREADR (SPDIRFLE,'NFS',nfsu)
        IF (WFNUU.LE.0.0)    CALL SPREADR (SPDIRFLE,'WFNUU',wfnuu)
        IF (RLFNU.LE.0.0)    CALL SPREADR (SPDIRFLE,'RLFNU',rlfnu)
        IF (NCNU.LE.0.0)     CALL SPREADR (SPDIRFLE,'NCNU',ncnu)
        IF (XNFS.LT.0.0)     CALL SPREADR (SPDIRFLE,'NLAB%',xnfs)
        IF (RTNO3.LT.0.0)    CALL SPREADR (SPDIRFLE,'RTNO3',rtno3)
        IF (RTNH4.LT.0.0)    CALL SPREADR (SPDIRFLE,'RTNH4',rtnh4)
        CALL SPREADR (SPDIRFLE,'PGERM',pgerm)
        CALL SPREADR (SPDIRFLE,'PEMRG',pemrg)
        CALL SPREADR (SPDIRFLE,'P0',PD(0))
        CALL SPREADR (SPDIRFLE,'PPTHR',p1dt)
        CALL SPREADR (SPDIRFLE,'PPEND',ppend)
        ! In spe file main stem is considered as tiller 1 whereas below
        ! ths is not the case and tiller 1 is the first branch.
        CALL SPREADR (SPDIRFLE,'TGR02',latfr(1))
        !                                    CSCRP
        LATFR(2) =  LATFR(1)       ! 0.8      0.80
        LATFR(3) =  LATFR(1)       ! 0.8      0.76
        LATFR(4) =  LATFR(1)       ! 0.8      0.72
        LATFR(5) =  0.8 * LATFR(1) ! 0.6      0.68
        LATFR(6) =  0.8 * LATFR(1) ! 0.6      0.64
        LATFR(7) =  0.6 * LATFR(1) ! 0.4      0.61
        LATFR(8) =  0.6 * LATFR(1) ! 0.4      0.57
        LATFR(9) =  0.6 * LATFR(1) ! 0.4      0.53
        LATFR(10) = 0.4 * LATFR(1) !  0.3     0.49
        LATFR(11) = 0.4 * LATFR(1) !  0.3     0.45
        LATFR(12) = 0.4 * LATFR(1) !  0.3     0.41
        LATFR(13) = 0.4 * LATFR(1) !  0.3     0.37  
        LATFR(14) = 0.2 * LATFR(1) !  0.2     0.33
        LATFR(15) = 0.2 * LATFR(1) !  0.2     0.29
        LATFR(16) = 0.2 * LATFR(1) !  0.2     0.26
        LATFR(17) = 0.1 * LATFR(1) !  0.1     0.22
        LATFR(18) = 0.1 * LATFR(1) !  0.1     0.18
        LATFR(19) = 0.1 * LATFR(1) !  0.1     0.14
        LATFR(20) = 0.1 * LATFR(1) !  0.1
        CALL SPREADR  (SPDIRFLE,'LRPHS',lrets)

        CALL SPREADRA (SPDIRFLE,'PTFS','10',ptfs)
        CALL SPREADRA (SPDIRFLE,'PTFA','10',ptfa)
        CALL SPREADRA (SPDIRFLE,'STFR','10',stfr)
        CALL SPREADRA (SPDIRFLE,'CHT%','10',chtpc)
        CALL SPREADRA (SPDIRFLE,'CLA%','10',clapc)
        
        CALL SPREADR (SPDIRFLE,'LLIG%',lligp)
        CALL SPREADR (SPDIRFLE,'SLIG%',sligp)
        CALL SPREADR (SPDIRFLE,'RLIG%',rligp)
        CALL SPREADR (SPDIRFLE,'GLIG%',gligp)
        CALL SPREADR (SPDIRFLE,'RLWR',rlwr)
        CALL SPREADR (SPDIRFLE,'RWUMX',rwumxs)
        rwumx = rwumxs
        CALL SPREADR (SPDIRFLE,'RSUSE',rsuse)
        CALL SPREADR (SPDIRFLE,'WFRGU',wfrgu)
        CALL SPREADR (SPDIRFLE,'NCRG',ncrg)
        CALL SPREADR (SPDIRFLE,'RWUPM',rwupm)
        CALL SPREADR (SPDIRFLE,'SDWT',sdsz)
        CALL SPREADR (SPDIRFLE,'SDN%',sdnpci)
        CALL SPREADR (SPDIRFLE,'WFGEU',wfgeu)
        CALL SPREADR (SPDIRFLE,'TKUH',lt50s)
        CALL SPREADR (SPDIRFLE,'TKLF',tklf)
        CALL SPREADR (SPDIRFLE,'WFTU',wftu)
        CALL SPREADR (SPDIRFLE,'WFTL',wftl)
        CALL SPREADR (SPDIRFLE,'WFSU',wfsu)
        CALL SPREADR (SPDIRFLE,'LLOSW',llosw)
        CALL SPREADR (SPDIRFLE,'NFSU',nfsu)
        CALL SPREADR (SPDIRFLE,'NFSF',nfsf)
        CALL SPREADR (SPDIRFLE,'LLOSN',llosn)
        CALL SPREADR (SPDIRFLE,'WFNUL',wfnul)
        CALL SPREADR (SPDIRFLE,'NO3MN',no3mn)
        CALL SPREADR (SPDIRFLE,'NH4MN',nh4mn)
        CALL SPREADR (SPDIRFLE,'P6',pd(6))
        CALL SPREADR (SPDIRFLE,'LSHFR',lshfr)
        CALL SPREADR (SPDIRFLE,'LAXS',laxs)
        CALL SPREADR (SPDIRFLE,'PHF1',phintf(1))
        CALL SPREADR (SPDIRFLE,'PHL1',phintl(1))
        IF (PHINTF(3).LE.0) CALL SPREADR (SPDIRFLE,'PHF3',phintf(3))
        IF (PHINTL(2).LE.0) CALL SPREADR (SPDIRFLE,'PHL2',phintl(2))
        CALL SPREADR (SPDIRFLE,'GN%MX',grnmx)
        CALL SPREADR (SPDIRFLE,'RSEN',rsen)
        IF (RSEN.LT.0.0) CALL SPREADR (SPDIRFLE,'RSEN%',rsen)
        CALL SPREADR (SPDIRFLE,'RS%X' ,rspcx)
        CALL SPREADR (SPDIRFLE,'SSEN',ssen)
        IF (SSEN.LT.0.0) CALL SPREADR (SPDIRFLE,'SSEN%',ssen)
        CALL SPREADR (SPDIRFLE,'SSPHS',ssstg)
        CALL SPREADR (SPDIRFLE,'SAWS',saws)
        CALL SPREADR (SPDIRFLE,'LSHAW',lshaws)
        CALL SPREADR (SPDIRFLE,'SDAFR',sdafr)
        CALL SPREADR (SPDIRFLE,'RLDGR',rldgr)
        CALL SPREADR (SPDIRFLE,'PTFMX',ptfx)
        CALL SPREADR (SPDIRFLE,'RRESP',rresp)
        CALL SPREADR (SPDIRFLE,'SLAMN',lawfrmn)
        CALL SPREADR (SPDIRFLE,'HDUR',hdur)
        CALL SPREADR (SPDIRFLE,'CHFR',chfr)
        CALL SPREADR (SPDIRFLE,'CHSTG',chstg)
        CALL SPREADR (SPDIRFLE,'NTUPF',ntupf)
        CALL SPREADR (SPDIRFLE,'RDGTH',rdgth)
        CALL SPREADR (SPDIRFLE,'TPAR',part)
        CALL SPREADR (SPDIRFLE,'TSRAD',sradt)
        KEP = (KCAN/(1.0-PART)) * (1.0-SRADT)
        IF (LAWCF.LE.0.0) CALL SPREADR (SPDIRFLE,'SLACF',lawcf)

        CALL SPREADRA (SPDIRFLE,'LASF','10',plasf)
        CALL SPREADRA (SPDIRFLE,'LN%S','10',lnpcs)
        CALL SPREADRA (SPDIRFLE,'SN%S','10',snpcs)
        CALL SPREADRA (SPDIRFLE,'RN%S','10',rnpcs)
        CALL SPREADRA (SPDIRFLE,'LN%MN','10',lnpcmn)
        CALL SPREADRA (SPDIRFLE,'SN%MN','10',snpcmn)
        CALL SPREADRA (SPDIRFLE,'RN%MN','10',rnpcmn)
        CALL SPREADRA (SPDIRFLE,'TRGEM','4',trgem)
        CALL SPREADRA (SPDIRFLE,'TRDV1','4',trdv1)
        CALL SPREADRA (SPDIRFLE,'TRDV2','4',trdv2)
        IF (tbam.gt.-90.0) THEN
!          WRITE (FNUMWRK,*)' '
!          WRITE (FNUMWRK,*)' Base temperature for post anthesis period'
!          WRITE (FNUMWRK,*)' changed from ',trdv2(1),' to ',tbam        
          trdv2(1) = tbam
!           NB. TBAM is only used experimentally;should not be in coeff.files
        ENDIF
        CALL SPREADRA (SPDIRFLE,'TRLFG','4',trlfg)
        CALL SPREADRA (SPDIRFLE,'TRPHS','4',trphs)
        CALL SPREADRA (SPDIRFLE,'TRVRN','4',trvrn)
        CALL SPREADRA (SPDIRFLE,'TRHAR','4',trlth)
        CALL SPREADRA (SPDIRFLE,'TRGFW','4',trgfw)
        CALL SPREADRA (SPDIRFLE,'TRGFN','4',trgfn)
        CALL SPREADRA (SPDIRFLE,'CO2RF','10',co2rf)
        CALL SPREADRA (SPDIRFLE,'CO2F','10',co2f)

        ! Variables that should be read-in!
        LALOSSF = 0.2     ! Leaf area lost when tillers die
        LAFST = 1.6       ! Stage at which incremment in lf size changes
        RSCLX = 0.2       ! Reserves concentration in leaves,maximum   
        
!        WRITE(fnumwrk,*) ' '
!        WRITE(fnumwrk,'(A18)')' RUN OVERVIEW     '
!        WRITE(fnumwrk,*)' MODEL   ',MODEL
!        WRITE(fnumwrk,*)' MODULE  ',MODNAME
!        !WRITE(fnumwrk,'(A10,I6)')'  VERSION ',VERSION
!        WRITE(fnumwrk, "(A10,I1,'.',I1,'.',I1,'.',I3.3)")
!     &   '  VERSION ', VERSION
!        WRITE(fnumwrk,*)' RNMODE  ',RNMODE
!
!        WRITE(fnumwrk,*)' '
!        WRITE(fnumwrk,'(A13,A1)')'  N SWITCH   ',ISWNIT
!        WRITE(fnumwrk,'(A13,A1)')'  H2O SWITCH ',ISWWAT
!        WRITE(fnumwrk,'(A18,A1)')'  PLANTING SWITCH ',IPLTI
!
!        WRITE(fnumwrk,*)' '
!        WRITE(fnumwrk,'(A10,I8  )')'  RUN     ',RUN    
!        WRITE(fnumwrk,*)' '
!        WRITE(fnumwrk,'(A23,A10)')'  EXPERIMENT           ',excode
!        WRITE(fnumwrk,'(A21, I3)')'  TREATMENT          ',tn
!        WRITE(fnumwrk,'(A23,I1)') '  CROP COMPONENT       ',CN
!       IF (IPLTI.NE.'A') THEN
        IF (IPLTI.NE.'A' .AND. IPLTI.NE.'F') THEN
!          WRITE(fnumwrk,'(A23,I7)') '  PLANTING DATE TARGET ',YEARPLTP
!        ELSE  
!          WRITE(fnumwrk,'(A40)')
!     &      '  AUTOMATIC PLANTING.  THRESHOLD DAYS:  '
!          CALL CSYR_DOY(PWDINF,TVI1,TVI2)
          CALL YR_DOY(PWDINF,TVI1,TVI2)
!          WRITE(fnumwrk,'(A14,I7)') '   EARLIEST   ',TVI2
!          CALL CSYR_DOY(PWDINL,TVI1,TVI2)
          CALL YR_DOY(PWDINL,TVI1,TVI2)
!          WRITE(fnumwrk,'(A14,I7)') '   LATEST     ',TVI2
        ENDIF
        
        ! The following are to allow examination of the functioning of 
        ! different parts of the module, and comparison with CSCRP     
        
        ! Commented acronyms are those used in the genotype files
       
        ! Set which sections to be examined
        EXAMINE(1) = 'N'    ! Basic leaf and root growth
        EXAMINE(2) = 'N'    ! Leaf senescencs
        EXAMINE(3) = 'N'    ! Tillering
        EXAMINE(4) = 'N'    ! Reproductive development
        EXAMINE(20)= 'N'    ! Everything
        
        IF (EXAMINE(1).EQ.'Y') THEN                                     

          ! No tillering,no senescence,no reproductive development 
          
!          WRITE(fnumwrk,*)' '
!          WRITE(fnumwrk,*)' RUNNING EXAMINE. '
!          WRITE(fnumwrk,*)' '
          
          ! CSCER                                    !  CSCRP
          
          PGERM = 10.0                               !    PGERM = 10.0  
          PEMRG = 8.0                                !    PEMRG =  8.0  

          Pd(0) = 0.0                                !    Pd(0) = 0.0   
          
          PARUV = 2.7                                !    PARUE = 2.7   
          PARUR = 2.7                                !    PARU2 = 2.7   
          
          KCAN = 0.85                                !    KCAN = 0.85   
                                                                                                            
          PTFX = 0.98  ! PTFMX                       !    PTFMX = 0.98 
                                                                        
          PTFS(1) = 0.75                             !    PTFMN = 0.75  
          PTFS(2) = 0.75                             !    PTFMX = 0.75 
          PTFS(3) = 0.75                                             
          PTFS(4) = 0.75                             
          PTFS(5) = 0.75                             
          PTFS(6) = 0.75                             
          
          PTFA(1) = 0.0                              !    PTFA = 0.00
          PTFA(2) = 0.0
          PTFA(3) = 0.0
          PTFA(4) = 0.0
          PTFA(5) = 0.0
          PTFA(6) = 0.0
          
          STFR(1) = 0.0                                              
          STFR(2) = 0.0                                              
          STFR(3) = 0.0                                              
          STFR(4) = 0.0                                              
          STFR(5) = 0.0                                              
          STFR(6) = 0.0                                              
          
          ! Actuals in SPE file:
          ! PTFS  PTFA  STFR                         
          ! 0.65  0.10  0.00
          ! 0.70  0.10  0.15
          ! 0.75  0.10  0.51
          ! 0.80  0.10  1.00
          ! 0.98  0.35  1.00
          ! 0.98  0.35  1.00

          PPFPE = 1.0                                !    PPFPE = 1.0   
          P1D = 97.0                                 !    PPS(1)= 97.0
          
          VEFF = 1.0                                 !    VEFF = 1.0    
                                                     !    VBASE = 0.0                                                        
          
           
          PHINT =    95.0                            !    PHINT = 95.0
          PHINTL(1) = 2.0    ! Headed as PHLx        !    PHINTL(1) =2.0
          PHINTF(1) = 0.8    ! Headed as PHFx        !    PHINTF(1) =0.8
          PHINTL(2) = 150                            !    PHINTL(2) =150
          PHINTF(3) = 1.8                            !    PHINTF(3) =1.8
                                                     
          LAPOT(1) = 5.0                             !    LA1S = 5.0    
          LAFV = 0.1                                 !    LAFV = 0.1    
          LAFR = 0.5                                 !    LAFR = 0.5    
            
          LAWS = 400                                 !    LAWS = 400    
          LAWCF = 0.02                               !    LAWCF = 0.02  
          LAWFRMN = 0.5  ! SLAMN
            
          LSHFR = 0.33                               !    LSHFR = 0.33  
          LRETS = 3.0                                !    LRPHS
          
          LSHAWS = 50.0  ! LSHAW  !    LSHAWV = 50.0   ! LSHAV
                                  !    LSHAWR = 50.0   ! LSHAR

          TKLF = -50.0                               !    TKDLF = 2.0 
          LT50S = -50.0                              !    TKUH = -50.0  
          LT50H = -90.0                              !    TKFH = -90.0  
                                                     !    TKSPAN = 2.0                                                       
          HDUR = 10.0                                !    HDUR = 10.0   
          
                                                     !    HLOST = 50.0                                                       
                                                     !    HLOSF = 0.2 

          RSPCS = 20.0                               !    RSPCA = 15.0 
          RSPCX = 80.0                               !    RSPCO = 80.0

          RRESP = 0.40                               !    RRESP = 0.40 
          RSEN = 0.1                                 !    RSEN = 0.1
          RLWR = 0.90  ! 0.98                        !    RLWR = 0.90 
          
          SDAFR = 0.50  ! From 0.50

          RTNO3 = 0.006  ! RTNUP             !    RTNO3 = .006!RTNUP
          RTNH4 = 0.006                      !    RTNH4 = .006
                                             !    NO3CF = 1.0 !NUPNF
                                             !    NUPWF = 1.0       
          NH4MN = 0.5                        !    NH4MN = 0.5  
          NO3MN = 0.5                        !    NO3MN = 0.5  
          NTUPF = 0.05                       !    NTUPF = 0.05 

             ! CSCER
          LNPCS(0) = 6.5                             !    LNPCS(0) = 6.5
          LNPCS(1) = 6.5                             !    LNPCS(1) = 6.5
          LNPCS(2) = 6.5                             
          LNPCS(3) = 6.5   
          LNPCS(4) = 6.5   
          LNPCS(5) = 6.5   
          LNPCS(6) = 6.5   
          LNPCS(7) = 6.5   
          LNPCS(8) = 6.5   
          SNPCS(0) = 1.5                             !    SNPCS(0) = 1.5
          SNPCS(1) = 1.5                             !    SNPCS(1) = 1.5
          SNPCS(2) = 1.5                             
          SNPCS(3) = 1.5   
          SNPCS(4) = 1.5   
          SNPCS(5) = 1.5   
          SNPCS(6) = 1.5   
          SNPCS(7) = 1.5   
          SNPCS(8) = 1.5   
          RNPCS(0) = 2.0                             !    RNPCS(0) = 2.0
          RNPCS(1) = 2.0                             !    RNPCS(1) = 2.0
          RNPCS(2) = 2.0                             
          RNPCS(3) = 2.0   
          RNPCS(4) = 2.0   
          RNPCS(5) = 2.0   
          RNPCS(6) = 2.0   
          RNPCS(7) = 2.0   
          RNPCS(8) = 2.0   
          LNPCMN(0) = 0.8                            !   LNPCMN(0) = 0.8
          LNPCMN(1) = 0.8                            !   LNPCMN(1) = 0.8
          LNPCMN(2) = 0.8                            
          LNPCMN(3) = 0.8   
          LNPCMN(4) = 0.8   
          LNPCMN(5) = 0.8   
          LNPCMN(6) = 0.8   
          LNPCMN(7) = 0.8   
          LNPCMN(8) = 0.8   
          SNPCMN(0) = 0.5                            !   SNPCMN(0) = 0.5
          SNPCMN(1) = 0.5                            !   SNPCMN(1) = 0.5
          SNPCMN(2) = 0.5                            
          SNPCMN(3) = 0.5   
          SNPCMN(4) = 0.5   
          SNPCMN(5) = 0.5   
          SNPCMN(6) = 0.5   
          SNPCMN(7) = 0.5   
          SNPCMN(8) = 0.5   
          RNPCMN(0) = 1.5                            !   RNPCMN(0) = 1.5
          RNPCMN(1) = 1.5                            !   RNPCMN(1) = 1.5
          RNPCMN(2) = 1.5                            
          RNPCMN(3) = 1.5   
          RNPCMN(4) = 1.5   
          RNPCMN(5) = 1.5   
          RNPCMN(6) = 1.5   
          RNPCMN(7) = 1.5   
          RNPCMN(8) = 1.5   
          ! Actuals in SPE file:
          ! CSCRP
          ! LN%S LN%MN  SN%S SN%MN  RN%S RN%MN                              
          !  8.0  0.80  2.50  0.65  2.00  1.50                              
          !  1.0  0.55  0.40  0.40  1.70  1.25                              
          ! CSCER
          ! LN%S  SN%S  RN%S LN%MN SN%MN RN%MN
          !  8.0   2.5  2.04  0.80  0.65  1.53     
          !  6.4   2.0  1.97  0.75  0.60  1.47
          !  5.3   1.0  1.89  0.70  0.55  1.41
          !  4.0   0.8  1.82  0.65  0.50  1.36
          !  3.1   0.6  1.77  0.60  0.45  1.33
          !  2.7   0.5  1.73  0.60  0.40  1.29
          !  1.5   0.4  1.68  0.60  0.40  1.26
          !  1.0   0.4  1.68  0.55  0.40  1.26
          
          XNFS = 20     ! NLABPC                     !     NLABPC = 20  
          NFPU = 1.0                                 !     NFPU = 1.0   
          NFPL = 0.0                                 !     NFPL = 0.0   
          NFGU = 1.0                                 !     NFGU = 1.0   
          NFGL = 0.0                                 !     NFGL = 0.0   
          NFTU = 1.0   ! NFTU                        !     NFTU = 1.0   
          NFTL = 0.0   ! NFTL                        !     NFTL = 0.0   
          NFSU = 0.4                                 !     NFSU = 0.4   
          NFSF = 0.1                                 !     NFSF = 0.1   
          NCRG = 30                                  !     NCRG = 30    
          LLOSN = 0.02 
          ! For low N accelerated senescence

          ! To eliminate leaf senescence
          LLIFE = 200.0                              !    LLIFA = 200.0 
          ! To eliminate tillering                      
          TI1LF = 200.0                              !    TI1LF = 200.0 
          ! To eliminate reproductive development
          P1V = 100000.0                             !    VREQ = 10000.0

        ENDIF    ! EXAMINE(1)
        
        
        IF (EXAMINE(2).EQ.'Y') THEN                                    
          ! Leaf senescence 
          LLIFE = 6.0                                !     LLIFA = 4.0 
          ! Must be 6 because of senescence period differences (1 vs 3)
          ! Growth = 1 phyllochron                   !     LLIFG = 1.0
          ! Senescence =  1 phyllochron              !     LLIFS = 3.0
          LWLOS = 0.5                                !     LWLOS = 0.5  
          ! LNPCMN -> N loss                         !  LNPCMN -> N loss
          ! Calculates LSENNF(fr N > min)        
        ENDIF    ! EXAMINE(2)  Senescence

        
        IF (EXAMINE(3).EQ.'Y') THEN                                    
          ! Tillering        
          TI1LF = 3.5                             !    TI1LF = 4.45  
          TILPE = 2.5
          TIFAC = 1.0                             !    TIFAC = 1.0   
          TILDS = 2.5                             !    TDPHS = 3.0   
          TILDE = 6.0                             !    TDPHE = 7.0   
          TILDF = 4.0                             !    TDFAC = 5.0   
          LATFR(1) = 0.80   ! TGR(2)              !    TGR(2) = 0.80
                                                  !    TGR(20) = 0.10    
                                                  !    TILIP = 6.0       
                                                  ! TINOX = 20 ! TIL#X
                                                  !    TDSF = 1.0        
          !          CSCRP    CSCER
          LATFR(2)  = 0.80   ! 0.8  LATFR(1)       
          LATFR(3)  = 0.76   ! 0.8  LATFR(1)       
          LATFR(4)  = 0.72   ! 0.8  LATFR(1)       
          LATFR(5)  = 0.68   ! 0.6  0.8 * LATFR(1) 
          LATFR(6)  = 0.64   ! 0.6  0.8 * LATFR(1) 
          LATFR(7)  = 0.61   ! 0.4  0.6 * LATFR(1) 
          LATFR(8)  = 0.57   ! 0.4  0.6 * LATFR(1) 
          LATFR(9)  = 0.53   ! 0.4  0.6 * LATFR(1) 
          LATFR(10) = 0.49   ! 0.3  0.4 * LATFR(1)
          LATFR(11) = 0.45   ! 0.3  0.4 * LATFR(1)
          LATFR(12) = 0.41   ! 0.3  0.4 * LATFR(1)
          LATFR(13) = 0.37   ! 0.3  0.4 * LATFR(1)
          LATFR(14) = 0.33   ! 0.2  0.2 * LATFR(1)
          LATFR(15) = 0.29   ! 0.2  0.2 * LATFR(1)
          LATFR(16) = 0.26   ! 0.2  0.2 * LATFR(1)
          LATFR(17) = 0.22   ! 0.1  0.1 * LATFR(1)
          LATFR(18) = 0.18   ! 0.1  0.1 * LATFR(1)
          LATFR(19) = 0.14   ! 0.1  0.1 * LATFR(1)
          LATFR(20) = 0.10   ! 0.1  0.1 * LATFR(1)
          
        ENDIF    ! EXAMINE(3)  Tillering

        IF (EXAMINE(4).EQ.'Y') THEN                                    
          ! Reproductive development
            P1V =    8.0                     !45 !    VREQ = 8.0        
            P1D =     97.0                       !    PPS(1) = 97.0    
            PD(5) =   500                        !    PD(8) = 500.0     
            PD(1) = 200                          !    PD(1) = 380       
            PD(2) = 200                          !    PD(2) =  70       
            PD(3) = 200                          !    PD(3) = 200       
            PD(4) = 200                          !    PD(4) = 200       
            PD2FR(1) = 0.25
            PD4FR(1) = 0.25
            PD4FR(2) = 0.10
                                                 !    PD(5) =  60
                                                 !    PD(6) =  25
                                                 !    PD(7) = 150
            ! PSNO PSNAME                ! PSNO PSTYP PSABV PSNAME     
            !   1  T.Spikelet            !   1      S GDAT  Germinate  
            !   2  EndVegetative         !   2      K TSAT  T.Spikelet 
            !   3  EndEarGrowth          !   3      S PSDAT Pseudo_Stem
            !   4  BeginGrainFill        !   4      S LLDAT End_Leaf   
            !   5  EndGrainFill          !   5      S IEDAT Head_Emerge
            !   6  Harvest               !   6      K ADAT  Anthesis   
            !   7  Sowing                !   7      S AEDAT EndAnthesis
            !   8  Germinate             !   8      S GFDAT MilkToDough
            !   9  Emergance             !   9      M MDAT  HardDough  
        ENDIF
            
        IF (EXAMINE(20).EQ.'Y') THEN  
          ! Everything from all genotype files    
          
        ! CSCER                                  !  CSCRP
        
        ! CULTIVAR  Newton(IB0488)  
            P1V =    1000.0                  !45 !    VREQ = 8.0        
            P1D =     75.0                       !    PPS(1) = 100.0    
            PD(5) =   500                        !    PD(8) = 500.0     
            G1CWT =    26.0                      !    GNOWTS = 25.0     
            G2KWT =    25.0                      !    GWTS = 40.0       
            G3 =        2.0                      !    G3 = 2.5          
            PHINT =    95.0                      !    PHINT = 80        
            
        ! ECOTYPE
            PD(1) = 200                          !    PD(1) = 380       
            PD(2) = 200                          !    PD(2) =  70       
            PD(3) = 200                          !    PD(3) = 200       
            PD(4) = 200                          !    PD(4) = 200       
            PD2FR(1) = 0.25
            PD4FR(1) = 0.25
            PD4FR(2) = 0.10
                                                 !    PD(5) =  60              
                                                 !    PD(6) =  25              
                                                 !    PD(7) = 150              
            ! PSNO PSNAME                ! PSNO PSTYP PSABV PSNAME     
            !   1  T.Spikelet            !   1      S GDAT  Germinate  
            !   2  EndVegetative         !   2      K TSAT  T.Spikelet 
            !   3  EndEarGrowth          !   3      S PSDAT Pseudo_Stem
            !   4  BeginGrainFill        !   4      S LLDAT End_Leaf   
            !   5  EndGrainFill          !   5      S IEDAT Head_Emerge
            !   6  Harvest               !   6      K ADAT  Anthesis   
            !   7  Sowing                !   7      S AEDAT EndAnthesis
            !   8  Germinate             !   8      S GFDAT MilkToDough
            !   9  Emergance             !   9      M MDAT  HardDough  

            LAPOT(1) = 5.0                       !    LA1S = 3.0        
            LAFV = 0.1                           !    LAFV = 0.1        
            LAFR = 0.5                           !    LAFR = 0.5        

            VEFF = 0.5                           !    VEFF = 1.0        
                                                 !    VBASE = 0.0                                                        

                                                 !    PPS(2) = 0.0                                                       
                                                                                                                         
                                             !   ECOTYPE:
                                             
            PARUV = 2.7                          !    PARUE = 2.7       
            PARUR = 2.7                          !    PARU2 = 2.7       
            
          ! Leaves 
            PHINTL(2) = 15                       !    PHINTL(2) = 12    
            PHINTF(3) = 1.8                      !    PHINTF(3) = 1.3   
            LAWS = 400                           !    LAWS = 400        
                                                 !    LSENI = 1.5                                                        
            LSENS = 5.0                          !    LSPHS = 8.0       
            LSENE = 6.3                          !    LSPHE = 9.3
            
          ! Tillers 
            TI1LF = 3.5                          !    TI1LF = 4.45      
            TILPE = 2.5
            TIFAC = 1.0                          !    TIFAC = 1.0       
            TILDS = 2.5                          !    TDPHS = 3.0       
            TILDE = 6.0                          !    TDPHE = 7.0       
            TILDF = 4.0                          !    TDFAC = 5.0       
                                                 !    TDSF = 1.0                                                         
           ! Roots                                                 
            RDGS1 = 3.0                          !    RDGS = 3.0        
            CANHTS = 100                         !    HTSTD = 100       
            AWNS = 0.0                           !    AWNS = 5.0        
            KCAN = 0.85                          !    KCAN = 0.85       
            RSPCS = 20.0                         !    RSPCA = 15.0      
          ! Grain 
            GRNS = 3.0                           !    GNPCS = 2.0       
            GRNMN = 0.0
                                                 ! GWTAA = 0.0    ! GWWF
          ! Hardiness  
            LT50H = -10                          !    TKFH = -15        
            
          ! Stems  
                                                 !    SSPHS = 8.0       
                                                 !    SSPHE = 9.3                                                        
                                                 ! GWTAT = 1.0  ! SHWTA                                             
          ! N uptake                                                 
            RTNO3 = 0.006  ! RTNUP               !    RTNO3 = .006!RTNUP
                                                 !    NO3CF = 1.0! NUPNF
                                                 !    NUPWF = 1.0       
                                                 !                                                                       
        ! SPECIES                                !                                                                       
                                                 !    HMPC = 15                                                          
                                   ! Secondary Stages (S=Standard,K=Key)
                                   ! SSNO SSTYP  SSTG SSABV SSNAME      
                                   !   1      S   1.5 LAFST Lafac_change
                                   !   2      K   1.6 DRAT  DoubleRidges
                                   !   3      S 3.143 JDAT  Jointing    
                                   !   4      S   3.3 SGPHS StemStart   
                                   !   5      S   4.0 RUESG RUE_change  
                                   !   6      S   4.0 LGPHE LeafEnd     
                                   !   7      S   4.0 LRPHS LfRetention 
                                   !   8      S   4.8 CHPHS ChaffStart  
                                   !   9      S   5.2 SVPHS StemVisible 
                                   !  10      S   6.5 SGPHE StemEnd     
                                   !  11      S   6.7 CHPHE ChaffEnd    
                                   !  12      S   7.0 GGPHS GrainStart  
                                   !  13      S   8.0 GLPHS GrLinearStar
                                   !  14      S   8.7 GLPHE GrLinearEnd 
                                   !  15      S   9.0 GGPHE GrainEnd    
          ! Phase durations and modifiers 
            PGERM = 10.0                         !    PGERM = 10.0      
            PEMRG =  8.0                         !    PEMRG = 8.0      
            Pd(0) = 0.0                          !    Pd(0) = 0.0       
            Pd(6) = 200.0                        !    Pd(9) = 200.0     
            PPFPE= 1.0                           !    PPFPE = 1.0       
            P1DT = 20.0  ! PPTHR                 !    PPTHR = 20.0      
            PPEND = 2.0
                                                 !    PPEXP = 2.0                                                        
                                                 !    VPEND = 2.0                                                        
                                                 !    VEEND = 2.0                                                        
                                                 !    VLOSS = 0.2                                                        
                                                 !    VLOST = 30.0                                                       
                                                 !    VLOSF = 0.5                                                        
          ! Roots
            RLIGP = 10.0  ! RLIG%                !    RLIGPC = 10.0     
            RLWR = 0.98                          !    RLWR = 0.9        
            RSEN = 0.10                          !    RSEN = 0.10     
            RRESP = 0.40                         !    RRESP = 0.80      
            RLDGR = 500.0
                                                 !    RDGAF = 0.5                                                        
                                                 !    RTUFR = 0.20                                                       
          ! Leaves
            LLIGP = 10.0  ! LLIG%                !    LLIGPC = 10.0     
            LAXS = 900.0                         !    LAXS = 100.0      
            LAWCF = 0.02                         !    LAWCF = 0.02      
            LAWFRMN = 0.5  ! SLAMN
            LSHFR = 0.33                         !    LSHFR = 0.33      
            LRETS = 3.0        ! LRPHS
            LSHAWS = 50.0      ! LSHAWS          ! LSHAWV =50.0  ! LSHAV
                                                 ! LSHAWR = 80.0 ! LSHAR                                         
            PHINTL(1) = 2.0    ! PHL             !    PHINTL(1) =2.0!PHL
            PHINTF(1) = 0.8    ! PHF             !    PHINTF(1) =0.8!PHF
            
            LLIFE = 4.0                          !    LLIFA = 4.0       
                                                 !    LLIFG = 1.0                                                        
                                                 !    LLIFS = 3.0                                                        
            !   LASF
            !   0.00
            !   0.10
            !   0.10
            !   0.10
            !   0.10
            !   1.00

            
            LWLOS = 0.50    ! LWLOS              !    LWLOS = 0.5!LWLOS
                                                 !    LAIXX = 14.0
                                                                                                        
            ! CHT%  CLA%                         !    ! CHT%  CLA%                                      
            !    0     0                         !    !    0     0                                                       
            !   50    50                         !    !   50    50                                                       
            !  100   100                         !    !  100   100                                                       

            PART = 0.07  ! TPAR                  !    TPAR = 0.07       
            SRADT = 0.25 ! TSRAD                 !    TSRAD = 0.25      
          ! Tillers      
            LATFR(1) = 0.80   ! TGR(2)           !    TGR(2) = 0.80  
                                                 !    TGR(20) = 0.10                                                     
                                                 !    TILIP = 6.0                                                        
                                                 ! TINOX = 20   ! TIL#X                                               
          ! Reserves
                                                 !    RSPCS = 20                                                         
            RSPCX = 80.0                         !    RSPCX = 80.0      
                                                 !    RSUSE = 0.1                                                        
                                                 !    RSPCLX = 80.0                                                      
          ! Stems                                                                                                        
            SLIGP = 10.0  ! SLIG%                !    SLIGPC = 10.0     
            SAWS = 25.0                          !    SAWS = 25.0       
            P4SGE = 4.45   ! SGPHE
            SSSTG = 5.8    ! SSPHS
            SSEN = 0.53  ! SSEN%
          ! Chaff                                !    ! Chaff                                                            
            CHFR = 0.65                          !    CHFR = 0.65       
            CHSTG = 3.8
          ! Grain                                !    ! Grain                                                            
            GLIGP = 10.0  ! GLIG%                !    GLIGPC = 10.0     
                                               ! GWLAGFR = 0.05 ! GWLAG                                            
                                               ! GWLINFR = 0.90 ! GWLIN                                            
          ! Seed                                 !    ! Seed                                                             
            SDSZ = 0.284   ! SDWT                !    SDWT = 0.28       
                                                 !    SDDUR = 20.0                                                       
                                                 !    SDRSPC = 80.0                                                      
            SDAFR = 0.50  ! From 0.50
          ! Photosynthesis                       !    ! Photosynthesis                                                   
                                                 !    RSFPU = 3.0                                                        
                                                 !    RSFPL = 2.0                                                        
            !CO2RF  CO2F                         !    !CO2RF  CO2F                                                       
            !    0  0.00                         !    !    0  0.00                                                       
            !  220  0.71                         !    !  220  0.71                                                       
            !  330  1.00                         !    !  330  1.00                                                       
            !  440  1.08                         !    !  440  1.08                                                       
            !  550  1.17                         !    !  550  1.17                                                       
            !  660  1.25                         !    !  660  1.25                                                       
            !  770  1.32                         !    !  770  1.32                                                       
            !  880  1.38                         !    !  880  1.38                                                       
            !  990  1.43                         !    !  990  1.43                                                       
            ! 9999  1.50                         !    ! 9999  1.50                                                       

          ! CH2O distribution/mobilization                                     
                                                 !    PTFMN = 0.75                                                       
            PTFX = 0.98  ! PTFMX                 !    PTFMX = 0.98      
                                                 !    PTFA = 0.10                                                        
            ! PTFS  PTFA  STFR
            ! 0.65  0.10  0.00
            ! 0.70  0.10  0.15
            ! 0.75  0.10  0.51
            ! 0.80  0.10  1.00
            ! 0.98  0.35  1.00
            ! 0.98  0.35  1.00
            
            ! Cold hardiness                     !    ! Cold hardiness                                                   
            LT50S = -6.0  ! TKUH                 !    TKUH = -6.0       
                                                 !    TKDTI = 2.0                                                        
                                                 !    TKLF = 0.0                                                         
                                                 !    TKSPAN = 2.0                                                       
            HDUR = 10.0                          !    HDUR = 10.0       
                                                 !    HLOST = 10.0                                                       
                                                 !    HLOSF = 0.2                                                        
!             Temperature responses
!             CSCRP                                       
!            RRATE TRGEM TRDV1 TRDV4 TRDV8 TRLFG TRPHS TRVRN TRHAR TRGFW TRGFN 
!                0     1     0     0     0     0     0    -5    -5     0     0 
!              1.0    26    26    26    30    10     5     0     0    16    16 
!              1.0    50    50    50    50    20    25     7     5    35    35 
!                0    60    60    60    60    35    35    15    10    45    45 
!             CSCER                              
!            RRATE TRGEM TRDV1 TRDV2 TRLFG TRPHS TRVRN TRHAR TRGFW TRGFN             
!                0     1     0     0     0     0    -5    -5     0     0             
!              1.0    26    26    30    10     5     0     0    16    16             
!              1.0    50    50    50    20    25     7     5    35    35             
!                0    60    60    60    35    35    15    10    45    45             
                                                                                                                         
          ! Water shortage effects                                                                                     
            RWUPM = 0.02
            RWUMX = 0.03
            WFPU = 1.0                           !    WFPU = 1.0        
            WFPGF = 1.0
                                                 !    WFPL = 0.0                                                         
            WFGU = 1.3                           !    WFGU = 1.3        
                                                 !    WFGL = 0.0                                                         
            WFTU = 1.0   ! WFTU                  !    WFTU = 1.0        
            WFTL = 0.5   ! WFTL                  !    WFTL = 0.5        
            WFSU = 0.6                           !    WFSU = 0.6        
                                                 !    WFSF = 0.5                                                         
            LLOSW = 0.02                         !    LLOSA = 0.02      
                                                 !    WFGEU = 0.5                                                        
                                                 !    WFRGU = 0.25                                                       
                                                 !                                                                       
           ! NITROGEN uptake,distribution,      
             NH4MN = 0.5                         !     NH4MN = 0.5      
             NO3MN = 0.5                         !     NO3MN = 0.5      
             NTUPF = 0.05                        !     NTUPF = 0.05     
           ! Nitrogen concentrations                                                                         
             GRNMX = 3.2  ! GN%MS                !     GNPCMX = 4.5     
                                                 !     GNPCMN = 0.5                                                      
             SDNPCI = 1.9                        !     SDNPCI = 1.9     
             ! CSCRP
             ! LN%S LN%MN  SN%S SN%MN  RN%S RN%MN                              
             !  8.0  0.80  2.50  0.65  2.00  1.50                              
             !  1.0  0.55  0.40  0.40  1.70  1.25                              
             ! CSCER
             ! LN%S  SN%S  RN%S LN%MN SN%MN RN%MN
             !  8.0   2.5  2.04  0.80  0.65  1.53     
             !  6.4   2.0  1.97  0.75  0.60  1.47
             !  5.3   1.0  1.89  0.70  0.55  1.41
             !  4.0   0.8  1.82  0.65  0.50  1.36
             !  3.1   0.6  1.77  0.60  0.45  1.33
             !  2.7   0.5  1.73  0.60  0.40  1.29
             !  1.5   0.4  1.68  0.60  0.40  1.26
             !  1.0   0.4  1.68  0.55  0.40  1.26
             XNFS = 20     ! NLABPC              !     NLABPC = 20      
             NFPU = 1.0                          !     NFPU = 1.0       
             NFPL = 0.0                          !     NFPL = 0.0       
             NFGU = 1.0                          !     NFGU = 1.0       
             NFGL = 0.0                          !     NFGL = 0.0       
             NFTU = 1.0   ! NFTU                 !     NFTU = 1.0       
             NFTL = 0.0   ! NFTL                 !     NFTL = 0.0       
             NFSU = 0.4                          !     NFSU = 0.4       
             NFSF = 0.1                          !     NFSF = 0.1       
             NCRG = 30                           !     NCRG = 30        
             LLOSN = 0.02
         ENDIF    !  EXAMINE(20)                                      
        
        ! DEFAULTS
        IF (LAXS.LE.0.0) LAXS = 900.0
        IF (LAWCF.LE.0.0) THEN
          LAWCF = 0.01    
!          WRITE (fnumwrk,*) ' '
!          WRITE (fnumwrk,*) ' Default of 0.01 used for LAWCF'
        ENDIF
        IF (TRGEM(3).LE.0.0) TRGEM = TRDV1
        IF (PPEND.LE.0.0) PPEND = 2.0          
        IF (VEFF.LE.0.0) VEFF = 0.5
        IF (VEFF.GT.1.0) VEFF = 1.0

        IF (PHINTL(1).LE.0.0) PHINTL(1) = 2.0
        IF (PHINTL(2).LE.0.0) PHINTL(2) = 20.0
        IF (PHINTL(3).LE.0.0) PHINTL(3) = 200.0
        IF (PHINTF(1).LE.0.0) PHINTF(1) = 0.8
        IF (PHINTF(2).LE.0.0) PHINTF(2) = 1.0
        IF (PHINTF(3).LE.0.0) PHINTF(3) = 1.0

        IF (RDGS2.LE.0.0) RDGS2 = RDGS1
        IF (RDGTH.LE.0.0) RDGTH = 275
        IF (RSEN.LT.0.0) RSEN = 0.1
        IF (RSPCX.LT.0.0) RSPCX = 80.0
        IF (RSUSE.LT.0.0) RSUSE = 0.10

        IF (TKLF.LT.-98.0) TKLF = -10.0

        ! For new (but not used!) N uptake routine.
        IF (NCNU.LE.0.0) NCNU = 30
        IF (RLFNU.LE.0.0) RLFNU = 2.0
        IF (WFNUU.LE.0.0) WFNUU = 1.0
        IF (WFNUL.LE.0.0) WFNUL = 0.0

        ! For CSM N uptake routine 
        IF (rtno3.le.0.0) THEN
          RTNO3 = 0.006    ! N uptake/root length (mgN/cm,.006)
!          WRITE (fnumwrk,*) ' '
!          WRITE (fnumwrk,*) ' Default of 0.006 used for RTNO3'
        ENDIF  
        IF (rtnh4.le.0.0) THEN
          RTNH4 = RTNO3     ! N uptake/root length (mgN/cm,.006)
!          WRITE (fnumwrk,*) ' '
!          WRITE (fnumwrk,*) ' Default of ',RTNO3,' used for RTNH4'
        ENDIF  
        
        ! BASED ON ORIGINAL CERES -- FOR INITIAL CALIBRATION
        IF (CROP.EQ.'WH') THEN
          IF (PD(1).LE.0.0) THEN
            PD(1) = 400 * PHINTS / 95
            PD(2) = 3.0 * PHINTS
            PD(3) = 2.0 * PHINTS
            PD(4) = 200.0
!            Write (fnumwrk,*) ' '
!            Write (fnumwrk,*) 'CALCULATED phase duration being used'
!            Write (fnumwrk,'(2X,4(F5.1,2X))') PD(1),PD(2),PD(3),PD(4)
!            Write (fnumwrk,*) ' (P1=400*PHINT/95;P2=3.0*PHINT'
!            Write (fnumwrk,*) ' (P3=2.0*PHINT;P4=200.0)'
          ENDIF  
        ENDIF  
        IF (CROP.EQ.'BA') THEN
          IF (PD(1).LE.0.0) THEN
            PD(1) = 300 * PHINTS / 70
            PD(2) = 3.2 * PHINTS
            PD(3) = 2.15* PHINTS
            PD(4) = 200.0
!            Write (fnumwrk,*) ' '
!            Write (fnumwrk,*) 'CALCULATED phase duration being used'
!            Write (fnumwrk,'(2X,4(F5.1,2X))') PD(1),PD(2),PD(3),PD(4)
!            Write (fnumwrk,*) ' (P1=300*PHINT/70;P2=3.2*PHINT'
!            Write (fnumwrk,*) ' (P3=2.15*PHINT;P4=200.0)'
          ENDIF  
        ENDIF  

!        WRITE (fnumwrk,*) ' '
!        WRITE (fnumwrk,*) 'DERIVED COEFFICIENTS'
        
!         NSFAC and NMNFAC are used for checking the component N concentrations only
        IF (nsfac.LE.0.0) NSFAC = 1.0
        IF (nmnfc.LE.0.0) NMNFC = 1.0
        DO L =0,9
          LCNCS(L) = LNPCS(L)/100.0*NSFAC
          SCNCS(L) = SNPCS(L)/100.0*NSFAC
          RCNCS(L) = RNPCS(L)/100.0*NSFAC
          LMNCS(L) = LNPCMN(L)/100.0*NMNFC
          SMNCS(L) = SNPCMN(L)/100.0*NMNFC
          RMNCS(L) = RNPCMN(L)/100.0*NMNFC
        ENDDO

        PD2(1) = PD2FR(1) * PD(2)
!        Write (fnumwrk,*) '  PD2,PD2FR1 ',
!     &     PD(2),PD2FR(1)
        PD4(1) = PD4FR(1) * PD(4)
        PD4(2) = PD4FR(2) * PD(4)
        PD4(3) = PD(4) -  PD4(1) - PD4(2)
!        Write (fnumwrk,*) '  PD4,PD4FR1,PD4FR2 ',
!     &     PD(4),PD4FR(1),PD4FR(2)
        IF (PD4(3).LE.0.0) THEN
!          Write (fnumwrk,*) 'Lag phase duration <= 0.0!   '
          Write (*,*) 'Lag phase duration <= 0.0!   '
          WRITE (*,*) 'Program will have to stop'
!          WRITE (*,*) 'Check WORK.OUT for details of run'
          STOP ' '
        ENDIF
        ! Kernel growing at half rate during lag period
        ! (=full rate for half period)
        G2 = G2KWT / (PD(5)+(PD(4)-PD4(1)-PD4(2))*0.50)
        
!        WRITE (fnumwrk,*) '  Pd2(1)      :  ',pd2(1)
!        WRITE (fnumwrk,*) '  Pd4(1)      :  ',pd4(1)
!        WRITE (fnumwrk,*) '  Pd4(2)      :  ',pd4(2)
!        WRITE (fnumwrk,*) '  Pd4(3)      :  ',pd4(3)
!        WRITE (fnumwrk,*) '  G2          :  ',g2

        ! Critical stages
        ASTAGE = 4.0 + PD4(1) / PD(4)
        ASTAGEND = 4.0 + (PD4(1)+PD4(2)) / PD(4)
!        WRITE (fnumwrk,*) '  Astage      :  ',astage
!        WRITE (fnumwrk,*) '  Astagend    :  ',astagend

        ! Phase thresholds
        DO L = 0,10
          PTH(L) = 0.0
        ENDDO
        PTH(0) = PD(0)
        DO L = 1,10
          PTH(L) = PTH(L-1) + AMAX1(0.0,PD(L))
        ENDDO

!        WRITE (fnumwrk,*) ' '
!        WRITE (fnumwrk,*) 'DERIVED DATA'
        ! Check seedrate and calculate seed reserves
        IF (SDRATE.LE.0.0) SDRATE = SDSZ*PLTPOPP*10.0
        ! Reserves = 80% of seed (42% Ceres3.5)
        SEEDRSI = (SDRATE/(PLTPOPP*10.0))*0.8
        SDCOAT = (SDRATE/(PLTPOPP*10.0))*0.2 ! Non useable material
        ! Seed N calculated from total seed
        SDNAP = (SDNPCI/100.0)*SDRATE
        SEEDNI = (SDNPCI/100.0)*(SDRATE/(PLTPOPP*10.0))
!        WRITE (fnumwrk,'(A16,2F7.1,A6)') '   Seedrs,Seedn:',
!     &        SEEDRSI*PLTPOPP*10.0,SEEDNI*PLTPOPP*10.0,' kg/ha'

        ! Check dormancy
        IF (PLMAGE.LT.0.0) THEN
          PEGD = PGERM - (PLMAGE*STDAY)
!          WRITE (fnumwrk,*)' '
!          WRITE (fnumwrk,'(A30,F6.2)')
!     &     '   Planting material dormancy ',plmage
!          WRITE (fnumwrk,'(A30,F6.2)')
!     &     '   Emergence+dormancy degdays ',pegd
        ELSE
          PEGD = PGERM
        ENDIF

        TKILL = LT50S 
        DF = PPFPE
        ! Define names for growth stages
        DO I = 1, 20
          STNAME(I) = '          '
          IF (CROP.EQ.'BA') THEN
            STNAME(I) = BASTGNAM (I)
          ELSEIF (CROP.EQ.'WH') THEN
            STNAME(I) = WHSTGNAM (I)
          ENDIF
        END DO
        IF (SLPF.LE.0.0 .OR. SLPF.GT.1.0) SLPF = 1.0
        IF (SLPF.LT.1.0) THEN
!          WRITE (fnumwrk,*) ' '
!          WRITE (fnumwrk,*)
!     &     'WARNING  Soil fertility factor was less than 1.0: ',slpf
        ENDIF  

        ! Write-out inputs if required
!        WRITE (fnumwrk,*) ' '
!        WRITE (fnumwrk,*) 'EXPERIMENTAL DETAILS'
!        WRITE (fnumwrk,*) '  TRUNNAME      ',TRUNNAME
!        WRITE (fnumwrk,'(A18,2F7.1)')'   PLTPOP,ROWSPC  ',PLTPOPP,ROWSPC
!        WRITE (fnumwrk,'(A18,2F7.1)')'   SDEPTH,SDRATE  ',SDEPTH,SDRATE
!        WRITE (fnumwrk,'(A18,F7.4,F7.2)')
!     &                               '   SDSZ,SDNPCI    ',SDSZ,SDNPCI
!        WRITE (fnumwrk,'(A18, F7.1)')'   PLMAGE         ',PLMAGE
!        WRITE (fnumwrk,'(A18,I7,A7)')'   YRHARF,IHARI   ',YRHARF,IHARI
!        WRITE (fnumwrk,'(A18,2F7.1)')'   HPC,HBPC       ',HPC,HBPC
!        WRITE (fnumwrk,'(A18,2A7  )')'   CROP,VARNO     ',CROP,VARNO
! 
!        WRITE (fnumwrk,*) ' '
        !IF (CUFILE.EQ.CUDIRFLE) THEN
          ! Following not used because cfg file set as Cropsim.cfg        
          !IF (CROP.EQ.'WH') THEN
          !  CALL Finddir (fnumtmp,cfgdfile,'WHD',cufile,cudirfle)
          !ELSEIF (CROP.EQ.'BA') THEN
          !  CALL Finddir (fnumtmp,cfgdfile,'BAD',cufile,cudirfle)
          !ENDIF
        !ENDIF
!        IF (FILEIOT .EQ. 'DS4') THEN
!          WRITE (fnumwrk,*) 'CULTIVAR DETAILS FROM: ',FILEIO(1:12)
!          WRITE (fnumwrk,*) ' Originals from: ',CUDIRFLE(1:60)
!        ELSE  
!          WRITE (fnumwrk,*) 'CULTIVAR DETAILS FROM: ',CUDIRFLE
!        ENDIF
!        WRITE (fnumwrk,*) '  Varno,econo :  ',varno,' ',econo
!        WRITE (fnumwrk,*) '  P1v,p1d,p5  :  ',p1v,p1d,pd(5)
!        WRITE (fnumwrk,*) '  G1,g2kwt,g3 :  ',g1cwt,g2kwt,g3
!        WRITE (fnumwrk,*) '  G2 mg/oC.d  :  ',g2
!        WRITE (fnumwrk,*) '  Phint,Veff  :  ',phints,veff
 
!        WRITE (fnumwrk,*) ' '
!        WRITE (fnumwrk,*) 'ECOTYPE DETAILS FROM: ',ECDIRFLE
!        WRITE (fnumwrk,*) '  TIl#S,TIPHE :  ',ti1lf,tilpe
!        WRITE (fnumwrk,*) '  P1,2,3      :  ',(pd(i),i = 1,3)
!        WRITE (fnumwrk,*) '  P4,5,6      :  ',(pd(i),i = 4,6)
!        WRITE (fnumwrk,*) '  P2(1)       :  ',pd2(1)
!        WRITE (fnumwrk,*) '  P4(1),P4(2) :  ',pd4(1),pd4(2)
!        WRITE (fnumwrk,*) '  P4(1),P4(2) :  ',pd4(1),pd4(2)
!        WRITE (fnumwrk,*) '  PHL1,PHF1   :  ',phintl(1),phintf(1)
!        WRITE (fnumwrk,*) '  PHL2,PHF2   :  ',phintl(2),phintf(2)
!        WRITE (fnumwrk,*) '  PHL3,PHF3   :  ',phintl(3),phintf(3)
!        WRITE (fnumwrk,*) '  PARUE,PARU2 :  ',paruv,parur
!        WRITE (fnumwrk,*) '  WFGU,WFPU   :  ',wfgu,wfpu
!        WRITE (fnumwrk,*) '  WFPGF       :  ',wfpgf
!        WRITE (fnumwrk,*) '  NFPU,NFPL   :  ',nfpu,nfpl
!        WRITE (fnumwrk,*) '  KCAN,KEP    :  ',kcan,kep
!        WRITE (fnumwrk,*) '  LA1S,LLIFE  :  ',lapot(1),llife
!        WRITE (fnumwrk,*) '  LAFV,LAFR   :  ',lafv,lafr      
!        WRITE (fnumwrk,*) '  AWNS,SGPHE  :  ',awns,p4sge
!        WRITE (fnumwrk,*) '  TKFH        :  ',lt50h
!        WRITE (fnumwrk,*) '  HTSTD       :  ',canhts
!        WRITE (fnumwrk,*) '  LAWS,LAWCF  :  ',laws,lawcf
!        WRITE (fnumwrk,*) '  RS%S,RS%X   :  ',rspcs,rspcx
!        WRITE (fnumwrk,*) '  RSUSE       :  ',rsuse        
!        WRITE (fnumwrk,*) '  NB.Rs%s is the percentage of stem ',
!     &                          'assimilates going to reserves'
!        WRITE (fnumwrk,*) '     instead of structural material.'
!        WRITE (fnumwrk,*) '  GN%S        :  ',grns 
!        WRITE (fnumwrk,*) '  LSPHS,LSPHE :  ',lsens,lsene

!        WRITE(fnumwrk,*) ' '
!        WRITE(fnumwrk,*) 'SPECIES DETAILS  FROM: ',SPDIRFLE

!        WRITE(fnumwrk,'(A17,2F8.2)')'  PGERM,PEMRG :  ',pgerm,pemrg
!        WRITE(fnumwrk,*)            '  P0          :  ',pd(0)
!        WRITE(fnumwrk,*)            '  PPTHR,PPFPE :  ',p1dt,ppfpe
!        WRITE(fnumwrk,*)            '  PPEND       :  ',ppend
!        WRITE(fnumwrk,'(A17,2F8.2)')'   P6         : ',pd(6)
!        WRITE(fnumwrk,*)            '  TDFAC       :  ',tildf
!        WRITE(fnumwrk,*)            '  LWLOS,LRPHS :  ',lwlos,lrets
!        WRITE(fnumwrk,*)            '  TGO02       :  ',latfr(1)
!        WRITE(fnumwrk,'(A8, 4F7.1)')   ' TRGEM',(trgem(i),i = 1,4)
!        WRITE(fnumwrk,'(A8, 4F7.1)')   ' TRDV1',(trdv1(i),i = 1,4)
!        WRITE(fnumwrk,'(A8, 4F7.1)')   ' TRDV2',(trdv2(i),i = 1,4)
!        WRITE(fnumwrk,'(A8, 4F7.1)')   ' TRLFG',(trlfg(i),i = 1,4)
!        WRITE(fnumwrk,'(A8, 4F7.1)')   ' TRPHS',(trphs(i),i = 1,4)
!        WRITE(fnumwrk,'(A8, 4F7.1)')   ' TRVRN',(trvrn(i),i = 1,4)
!        WRITE(fnumwrk,'(A8, 4F7.1)')   ' TRHAR',(trlth(i),i = 1,4)
!        WRITE(fnumwrk,'(A8, 4F7.1)')   ' TRGFW',(trgfw(i),i = 1,4)
!        WRITE(fnumwrk,'(A8, 4F7.1)')   ' TRGFN',(trgfn(i),i = 1,4)
! 
!        WRITE(fnumwrk,'(A8,10F7.1)')   ' CO2RF',(co2rf(i),i = 1,10)
!        WRITE(fnumwrk,'(A8,10F7.1)')   ' CO2F ',(co2f(i),i = 1,10)
!        WRITE(fnumwrk,'(A8,10F7.2)')   ' PTFS ',(ptfs(i),i = 1,10)
!        WRITE(fnumwrk,'(A8,10F7.2)')   ' PTFA ',(ptfa(i),i = 1,10)
!        WRITE(fnumwrk,'(A8,10F7.2)')   ' STFR ',(stfr(i),i = 1,10)
!        WRITE(fnumwrk,'(A8,10F7.2)')   ' LASF ',(plasf(i),i = 1,10)
!        WRITE(fnumwrk,'(A8,10F7.2)')   ' CHT% ',(chtpc(i),i = 1,10)
!        WRITE(fnumwrk,'(A8,10F7.2)')   ' CLA% ',(clapc(i),i = 1,10)
!        WRITE(fnumwrk,'(A8, 7F7.4)')   ' LCNCS',(lcncs(i),i = 0,6)
!        WRITE(fnumwrk,'(A8, 7F7.4)')   ' SCNCS',(scncs(i),i = 0,6)
!        WRITE(fnumwrk,'(A8, 7F7.4)')   ' RCNCS',(rcncs(i),i = 0,6)
!        WRITE(fnumwrk,'(A8, 7F7.4)')   ' LMNCS',(lmncs(i),i = 0,6)
!        WRITE(fnumwrk,'(A8, 7F7.4)')   ' SMNCS',(smncs(i),i = 0,6)
!        WRITE(fnumwrk,'(A8, 7F7.4)')   ' RMNCS',(rmncs(i),i = 0,6)
!        WRITE(fnumwrk,'(A17,3F8.2)')   ' L,S,R LIGNIN  ',
!     &                                   lligp,sligp,rligp
!        WRITE(fnumwrk,'(A17, F8.2)')'   GRAIN LIGNIN  ',gligp
!        WRITE(fnumwrk,'(A17,2F8.2)')'   RWUMXS,RWUMX  ',rwumxs,rwumx
!        WRITE(fnumwrk,'(A17, F8.2)')'   RWUPM         ',rwupm 
!        WRITE(fnumwrk,'(A17, F8.2)')'   RLWR cm/g     ',rlwr
!        WRITE(fnumwrk,'(A17,2F8.4)')'   WFRGU,NCRG    ',wfrgu,ncrg
!        WRITE(fnumwrk,'(A17,2F8.2)')'   WFGEU,SDAFR   ',wfgeu,sdafr
!        WRITE(fnumwrk,'(A17,2F8.4)')'   SDWT,SDN%I    ',sdsz,sdnpci
!        WRITE(fnumwrk,'(A17,2F8.4)')'   SDRATE,SDNI   ',sdrate,seedni
!        WRITE(fnumwrk,'(A17, F8.2)')'   RDGTH         ',rdgth
!        WRITE(fnumwrk,'(A17,2F8.2)')'   RDGS,RDG2     ',rdgs1,rdgs2
!        WRITE(fnumwrk,'(A17,2F8.2)')'   RLDGR,RRESP   ',rldgr,rresp
!        WRITE(fnumwrk,'(A17,2F8.2)')'   WFTU,WFTL     ',wftu,wftl
!        WRITE(fnumwrk,'(A17,2F8.2)')'   NFTU,NFTL     ',nftu,nftl
!        WRITE(fnumwrk,'(A17,2F8.2)')'   NFGU,NFGL     ',nfgu,nfgl
!        WRITE(fnumwrk,'(A17,2F8.2)')'   WFSU,LLOSW    ',wfsu,llosw
!        WRITE(fnumwrk,'(A17,2F8.2)')'   NFSU,LLOSN    ',nfsu,llosn
!        WRITE(fnumwrk,'(A17, F8.2)')'   NFSF          ',nfsf
!        WRITE(fnumwrk,'(A17,2F8.2)')'   WFNUU,WFNUL   ',wfnuu,wfnul
!        WRITE(fnumwrk,'(A17,2F8.2)')'   NO3MN,NH4MN   ',no3mn,nh4mn
!        WRITE(fnumwrk,'(A17,2F8.2)')'   RLFNU,NCNU    ',rlfnu,ncnu
!        WRITE(fnumwrk,'(A17,2F8.2)')'   TKUH,HDUR     ',lt50s,hdur
!        WRITE(fnumwrk,'(A17, F8.2)')'   TKLF          ',tklf
!        WRITE(fnumwrk,'(A17,2F8.2)')'   PTFMX         ',ptfx
!        WRITE(fnumwrk,'(A17,2F8.2)')'   SSEN,RSEN     ',ssen,rsen
!        WRITE(fnumwrk,'(A17,2F8.2)')'   SAWS,LSHAWS   ',saws,lshaws
!        WRITE(fnumwrk,'(A17,2F8.2)')'   LSHFR,LAXS    ',lshfr,laxs
!        WRITE(fnumwrk,'(A17,2F8.2)')'   PHL1,PHF1     ',
!     &   phintl(1),phintf(1)
!        WRITE(fnumwrk,'(A17,2F8.2)')'   SSPHS         ',ssstg
!        WRITE(fnumwrk,'(A17,2F8.2)')'   NLAB%,GN%MX   ',xnfs,grnmx
!        WRITE(fnumwrk,'(A17,2F8.2)')'   NTUPF         ',ntupf       
!        WRITE(fnumwrk,'(A17,2F8.2)')'   CHFR,CHSTG    ',chfr,chstg
!        WRITE(fnumwrk,'(A17,2F8.2)')'   TPAR,TSRAD    ',part,sradt

!        IF (CROP.EQ.'WH') THEN
!          WRITE(fnumwrk,*) ' '
!          WRITE(fnumwrk,'(A55)')
!     &     ' PHASE DURATIONS                                       '
!          WRITE(fnumwrk,'(A21,5F6.1)') '  CERES INPUTS     = ',
!     &     PD(1),PD(2),PD(3),PD(4),PD(5)
!          WRITE(fnumwrk,'(A21,5F6.1)') '  CERES from PHINT = ',
!     &     PHINTS*400.0/95.0,PHINTS*3.0,PHINTS*2.0,200.0,PD(5)
!          WRITE(fnumwrk,'(A38)')
!     &     '  ZADOKS FROM CERES INPUTS AND PHINT  '
!          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P1   ->TS  2 ',
!     &      PD(1),PHINTS*400.0/95.0  
!          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P2   ->Jt  3 ',
!     &      PD(2)*PD2FR(1),PHINTS*3.0*PD2FR(1)  
!          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P3  TS->LL 4 ',
!     &     PD(2)*(1.0-PD2FR(1)),PHINTS*3.0*(1.0-PD2FR(1))             
!          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P4  LL->SE 5 ',
!     &     PD(3),PHINTS*2.0      
!          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P5 ESG->AN 6 ',
!     &     PD(4)*PD4FR(1),200.0*PD4FR(1)           
!          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P6  AN->EA 7 ', 
!     &     PD(4)*PD4FR(2),200.0*PD4FR(2)
!          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P7  EA->EL 8 ',
!     &     PD(4)*(1.0-(PD4FR(1)+PD4FR(2))),200*(1.0-(PD4FR(1)+PD4FR(2)))
!          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P8  EL->PM 9 ',
!     &     PD(5),PD(5)
!        ENDIF

!        IF (CROP.EQ.'BA') THEN
!          WRITE(fnumwrk,*) ' '
!          WRITE(fnumwrk,'(A55)')
!     &     ' PHASE DURATIONS                                       '
!          WRITE(fnumwrk,'(A21,5F6.1)') '  CERES INPUTS     = ',
!     &     PD(1),PD(2),PD(3),PD(4),PD(5)
!          WRITE(fnumwrk,'(A21,5F6.1)') '  CERES from PHINT = ',
!     &     PHINTS*300.0/75.0,225.0,150.0,200.0,PD(5)
!          WRITE(fnumwrk,'(A38)')
!     &     '  ZADOKS FROM CERES INPUTS AND PHINT  '
!          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P1   ->TS  2 ',
!     &      PD(1),PHINTS*300.0/75.0  
!          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P2   ->Jt  3 ',
!     &      PD(2)*PD2FR(1),225.0*PD2FR(1)  
!          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P3  TS->LL 4 ',
!     &     PD(2)*(1.0-PD2FR(1)),225.0*(1.0-PD2FR(1))             
!          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P4  LL->SE 5 ',
!     &     PD(3),150.0      
!          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P5 ESG->AN 6 ',
!     &     PD(4)*PD4FR(1),200.0*PD4FR(1)           
!          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P6  AN->EA 7 ', 
!     &     PD(4)*PD4FR(2),200.0*PD4FR(2)
!          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P7  EA->EL 8 ',
!     &     PD(4)*(1.0-(PD4FR(1)+PD4FR(2))),200*(1.0-(PD4FR(1)+PD4FR(2)))
!          WRITE(fnumwrk,'(A16,F5.1,2X,F5.1)') '   P8  EL->PM 9 ',
!     &     PD(5),PD(5)
!        ENDIF

        ! End of initiation flags,etc..
        CFLINIT = 'Y'
        TNI = TN
        CNI = CN
        SNI = SN
        ONI = ON
        RNI = RN
        KEPI = KEP
        KCANI = KCAN
        RWUPMI = RWUPM 
        RWUMXI = RWUMX

        CROPP = CROP
        VARNOP = ' '
        VARNOP = VARNO
        CUDIRFLP = ' '
        CUDIRFLP = CUDIRFLE
        IF (RNMODE.EQ.'T') CUDIRFLP = ' '
        ECONOP = ' '
        ECONOP = ECONO
        ECDIRFLP = ' '
        ECDIRFLP = ECDIRFLE
        SPDIRFLP = ' '
        SPDIRFLP = SPDIRFLE

        SEEDRSUX = 0.0
        LAGSTAGE = -99.0
        TNUMOUT = 0.0
        GPLASENS = -99.0
        DO L=1,LNUMX
          AFLFSUM(L) = 0.0
          WFLFSUM(L) = 0.0
          NFLFSUM(L) = 0.0
          TFLFSUM(L) = 0.0
          WFLFNUM(L) = 0
          AFLF(L) = 0.0
          WFLF(L) = 0.0
          NFLF(L) = 0.0
          TFLF(L) = 0.0
        ENDDO

!        IF (FILEIOT.EQ.'DS4') WRITE(fnumwrk,*)' '
!        WRITE(FNUMWRK,'(A22)')' OUTPUTS              '
        ! Control switch for OUTPUT file names
        CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'FNAME',fname)
!        IF (FNAME.EQ.'Y') THEN
!          WRITE(FNUMWRK,*)'File names switched from standard. '
!        ELSE  
!          WRITE(FNUMWRK,*)'Standard file names. '
!        ENDIF   

!        WRITE(FNUMWRK,*)' '
!        WRITE(FNUMWRK,'(A22)')' DURING RUN STATUS:   '
        
      END SUBROUTINE CER_Init
