!***********************************************************************
! THIS IS THE CODE FROM THE SECTION (DYNAMIC.EQ.RUNINIT) ! Initialization
! LINES 2333 - 2525 OF THE ORIGINAL CSCAS CODE. THE NAMES OF THE 
! DUMMY ARGUMENTS ARE THE SAME AS IN THE ORIGINAL CSCAS CODE AND THE CALL 
! STATEMENT. THE VARIABLES ARE DESCRIBED IN CSCAS.
!
! THIS SUBROUTINE READS THE XFILE.
!
!***********************************************************************

!-----------------------------------------------------------------------
!       Read experiment information from Dssat input or X- file
!-----------------------------------------------------------------------

      SUBROUTINE CS_Read_Xfile ( &
          CN          , HARVFRAC    , ISWDIS      , ON          , RN          , SN          , TN          &
          )

      !USE CRSIMDEF                                                                  MF 15SE14 Declared in ModuleDefs        
      USE Module_CSCAS_Vars_List

      IMPLICIT NONE

      !INTEGER DCNX        , HANUMX      , TVILENT                                   ! Array bounds, set as parameters in CSCAS  MF 15SE14 Redundant, declared in Module_CSCAS_Vars_List and INTEGER FUNCTION
      CHARACTER(LEN=1) ISWDIS      
      
      INTEGER CN          , ON          , RN          , SN          , TN  
      INTEGER CSYEARDOY   , TVILENT                                                  ! Integer function calls.        
      
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
        IF(IPLTI.EQ.'A'.OR.IPLTI.EQ.'a')THEN
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
        
        DO I = 1,20
          IF (hyrdoy(i).EQ.-99) THEN
            hnumber = i - 1
            EXIT  
          ENDIF
          hyeardoy(i) = CSYEARDOY(hyrdoy(i))
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
          FILEX = FILEADIR(1:TVILENT(FILEADIR))//FILEA(1:TVILENT(FILEA))
          CALL LTRIM2 (FILEX,filenew)
          FILELEN = TVILENT(FILENEW)
          FILENEW(FILELEN:FILELEN)= 'X'
          FILEX = FILENEW
          ! Experimental controls
          CALL XREADC(FILEX,TN,RN,SN,ON,CN,'LLIFE',cfllflife)
        ENDIF

!-----------------------------------------------------------------------
!       Correct case and dates
!-----------------------------------------------------------------------

        CALL CSUCASE (CROP)
        CALL CSUCASE (EXCODE)

        HLAST = CSYEARDOY(hlast)
        HFIRST = CSYEARDOY(hfirst)
        PWDINF = CSYEARDOY(pwdinf)
        PWDINL = CSYEARDOY(pwdinl)
        DO L = 1,DINX
          DIDAT(L) = CSYEARDOY(DIDAT(L))
        ENDDO
        DO L = 1,DCNX
          DCDAT(L) = CSYEARDOY(DCDAT(L))
        ENDDO

        CALL CSYR_DOY(PWDINF,PWYEARF,PWDOYF)
        CALL CSYR_DOY(PWDINL,PWYEARL,PWDOYL)
        CALL CSYR_DOY(HFIRST,HYEARF,HDOYF)
        CALL CSYR_DOY(HLAST,HYEARL,HDOYL)
        CALL CSYR_DOY(PDATE,PLYEARTMP,PLDAY)
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
        END SUBROUTINE