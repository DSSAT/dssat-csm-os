!***************************************************************************************************************************
! This is the code feom the section (DYNAMIC == RUNINIT) ! Initialization lines 2333 - 2525 of the original CSCAS code. 
! The names of the dummy arguments are the same as in the original CSCAS code and the call statement and are declared 
! here. The variables that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all 
! comments are those of the original CSCAS.FOR code.
!
! Subroutine YCA_SeasInit_ReadXfile reads the Xfile (why are you surprised?)
!***************************************************************************************************************************

      SUBROUTINE YCA_SeasInit_ReadXfile ( &
          CN          , HARVFRAC    , ISWDIS      , ON          , RN          , SN          , TN          &
          )

      USE ModuleDefs
      USE YCA_First_Trans_m

      IMPLICIT NONE
      EXTERNAL YR_DOY, Y4K_DOY, TVILENT, LTRIM, XREADC, XREADT, XREADI, XREADR, XREADIA, XREADRA, LTRIM2, XREADCA, CSUCASE

      CHARACTER(LEN=1) ISWDIS
      CHARACTER(LEN=6),PARAMETER::ERRKEY = 'CSYCA '
      
      INTEGER CN          , ON          , RN          , SN          , TN  
      INTEGER TVILENT     ! CSYEARDOY   ,                                                     ! Integer function calls.        
      
      REAL    HARVFRAC(2) 

    
!-----------------------------------------------------------------------
!      Read experiment information from Dssat input or X- file
!-----------------------------------------------------------------------

        ! Methods
        CALL XREADC(FILEIO,TN,RN,SN,ON,CN,'PHOTO',mepho)
        CALL XREADC(FILEIO,TN,RN,SN,ON,CN,'EVAPO',meevp) 
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
        IF(IPLTI.EQ.'A'.OR.IPLTI.EQ.'a'.OR.IPLTI.EQ.'F'.OR.IPLTI.EQ.'f')THEN
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
          CALL XREADI(FILEIO,TN,RN,SN,ON,CN,'EDATE',edatmx)
        ENDIF

        ! Other planting information
        CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'CR',crop)
        CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'INGENO',varno)
        CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'CNAME',vrname)
        !CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPOP',pltpopp) !LPM 06MAR2016 To have just PPOP 
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPOP',ppop)
        CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPOE',ppoe) !LPM 06MAR2016 To have just one name for PPOP
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
        
        DO I = 1,20
          IF (hyrdoy(i) == -99) THEN
            hnumber = i - 1
            EXIT  
          ENDIF
!  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY          
          !hyeardoy(i) = CSYEARDOY(hyrdoy(i))
          CALL Y4K_DOY(hyrdoy(i),FILEX,0,ERRKEY,3)
          hyeardoy(i) = hyrdoy(i)
        ENDDO 
        IF (hnumber <= 1) HOP(1) = 'F' 
        yeardoyharf = -99
        DO I = 1, 20
          IF (HYEARDOY(I) > 0) THEN
            hnumber = I
            IF (hop(i) == 'F') THEN
              hpcf = hpc(i)
              hbpcf = hbpc(i)
              yeardoyharf = hyeardoy(i)
            ENDIF 
          ENDIF
        END DO
        IF (hnumber == 1) THEN
          hpcf = hpc(1)
          hbpcf = hbpc(1)
          yeardoyharf = hyeardoy(1)
        ENDIF 
        ! If running CSM use harvfrac so as to handle automatic mngement
        IF (FILEIOT  /=  'DS4') THEN
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
        IF (LENDIS == 1.AND.ISWDIS(LENDIS:LENDIS) == 'R') THEN
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
        ELSEIF (LENDIS == 1.AND.ISWDIS(LENDIS:LENDIS) == 'Y') THEN
          DIGFAC(1) = 1.0
          DIGFAC(2) = 1.0
          DIGFAC(3) = 1.0
        ELSEIF (LENDIS > 1) THEN
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

        IF (FILEIOT(1:2) == 'DS') THEN
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
          !LPM 05MAY2017 modified to allow the use of default path
          !FILEX = FILEADIR(1:TVILENT(FILEADIR))//FILEA(1:TVILENT(FILEA))
          CALL LTRIM2 (FILEX,filenew)
          FILELEN = TVILENT(FILENEW)
          !LPM 05MAY2017 modified to allow the use of default path
          !FILENEW(FILELEN:FILELEN)= 'X'
          !FILEX = FILENEW
          FILELEN = MAX(FILELEN-12, 0) 
            
            IF (TVILENT(FILEADIR) > 3) THEN
              IF (FILEADIR(TVILENT(FILEADIR): &
                  TVILENT(FILEADIR)) /= SLASH)THEN
                FILEX = FILEADIR(1:TVILENT(FILEADIR))//  &
                SLASH //EXCODE(1:8)//'.'//EXCODE(9:10)//'X'
              ELSE
                FILEX = FILEADIR(1:TVILENT(FILEADIR))// &
                EXCODE(1:8)//'.'//EXCODE(9:10)//'X'
              ENDIF
            ELSE
              FILEX = FILENEW(1:FILELEN-12)//EXCODE(1:8)//'.'// &
              EXCODE(9:10)//'X'
            ENDIF  
          ! Experimental controls
          CALL XREADC(FILEX,TN,RN,SN,ON,CN,'LLIFE',cfllflife)
        ENDIF

!-----------------------------------------------------------------------
!       Correct case and dates
!-----------------------------------------------------------------------

        CALL CSUCASE (CROP)
        CALL CSUCASE (EXCODE)

!  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY
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

        IF (digfac(1) < 0.0) digfac(1) = 1.0
        IF (digfac(2) < 0.0) digfac(2) = 1.0
        IF (digfac(3) < 0.0) digfac(3) = 1.0
        IF (plmage <= -98.0) plmage = 0.0
        IF (hnumber <= 0) THEN 
          hpcf = 100.0
          hbpcf = 0.0
        ENDIF
    
    END SUBROUTINE YCA_SeasInit_ReadXfile