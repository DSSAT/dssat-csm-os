!=======================================================================
!  CROPSIM DISEASE (LEAF) ROUTINES    
!  Developed from the original diseases module for
!  incorporation into the Dssat version of Cropsim
!  Last edit 06/06/13 LAH
!=======================================================================

      SUBROUTINE Disease(Spdirfle,run,runi,step, ! Run+crop info
     X cvalo,outhed,                             ! Loop info.
     X year,doy,dap,                             ! Dates
     X didat,digfac,diffacr,                     ! Disease details
     X dcdat,dcfac,dcdur,dctar,                  ! Disease control
     X tmax,tmin,dewhr,                          ! Drivers - weather
     X llapd,llapsd,pltpop,                      ! States - leaves
     X lfcnum,llap,LLAPP,llaps,                  ! States - leaves
!    X stgdoy,                                   ! Stage dates
     X didoy,                                    ! Disease initiation
     X dynamic)                                  ! Control

!     2023-01-20 CHP removed unused variables in argument list:
!    X stgdoy,                                   ! Stage dates

      ! Calculates daily progress of leaf disease of wheat as a function
      ! of cultivar resistance, amount of susceptible tissue and weather
      ! Leaf rust based on:
      ! Berger, R. D. and Jones, J. W. 1985.Phytopathology 75:792-797
      ! Johnson, K. B. and Teng, P. S. 1990.Phytopathology 80:416-425
      ! Berger, R. D. et. all 1995. Phytopathology 85:715-721
      ! Vallavieille-Pope et al. 1995.Comparative effects of temperature
      !  and interrupted wet periods on germination, penetration, and
      !  infection of Puccinia recondita f.sp.tritici and P.striiformis
      !  on wheat seedlings. Phytopathology 85:409-415
      ! Wadia,K D R & Butler,D R. 1994 Relationships between temperature
      !  and latent periods of rust and leaf-spot diseases of groundnut
      !  Plant Pathology 1994, V 43, 121-129
      ! Eversmeyer et al.1980 Phytopathology 70:938-941
      !  tmax=32.2,tmin=10,Topt=26.5
      !  Beta=(32.2-26.5)/(26.5-10)           Beta=0.345455
      !  a=0.1/((26.5-10)*(32.2-26.5)**Beta)  a=0.003322  Ymax=0.1
      ! Prates,L.G. and Fernandes,J.M.C 2001 Avakiando a taxa de
      ! expansao de lesoes de Bipolaris soroiniana em trigo. Fitopal.
      ! bras 26:185-191


      IMPLICIT NONE
      EXTERNAL GETLUN, TL10FROMI, SPREADCA, SPREADRA, SPREADIA, 
     &  TFAC4, CSTIMDIF, CSOPLINE

      INTEGER,PARAMETER::DINX = 3  ! Disease #,maximum
      INTEGER,PARAMETER::DCNX = 10 ! Disease control #,max
      INTEGER,PARAMETER::FINAL= 6  ! Program ending variable
      INTEGER,PARAMETER::INTEGR=4  ! Program update variable
      INTEGER,PARAMETER::LCX  = 25 ! Leaf # (cohort),maximum
      INTEGER,PARAMETER::OUTPUT=5  ! Program output variable
      INTEGER,PARAMETER::PCNUMX=400! Disease cohorts,maximum
      INTEGER,PARAMETER::RATE = 3  ! Program rate calc.var
      INTEGER,PARAMETER::RUNINIT=1 ! Program initiation var
      INTEGER,PARAMETER::SEASINIT=2! Reinitialisation indicator

      INTEGER       cnum          ! Cohort number for loop         #
      INTEGER       cstimdif      ! Time difference function       C
      INTEGER       cvalo         ! Control value,op frequency     d
      INTEGER       dao           ! Days after writing outputs     d
      INTEGER       dap           ! Days after planting            d
      INTEGER       das           ! Days after start (start=1)     d
      INTEGER       dcdat(dcnx)   ! Disease control application    YrDoy
      INTEGER       dctar(dcnx)   ! Disease control target         #
      INTEGER       dcnum         ! Disease control number         #
      INTEGER       didat(dinx)   ! Disease initiation dates       YrDoy
      INTEGER       didate(dinx)  ! Disease infestation yr+d       #
      INTEGER       didoy(dinx)   ! Disease infestation yr+d       #
      INTEGER       dinum         ! Disease,number (1=Mil,2=LfR)   #
      INTEGER       dinuminit     ! Disease,number initiated       #
      INTEGER       ditype2(dinx) ! Disease type 2 (1=no necrotic) #
      INTEGER       doy           ! Day of year                    d
      INTEGER       dynamic       ! Program control variable       #
      INTEGER       fnumdis(0:5)  ! File number,disease outputs    #
      INTEGER       fnumlfdi      ! File number,leaves,disease mod #
!     INTEGER       fnumwrk       ! File number,work file          #
      INTEGER       i             ! Loop counter                   #
      INTEGER       l             ! Loop counter                   #
      INTEGER       lfcnum        ! Leaf cohort number             #
      INTEGER       lfnum         ! Leaf number in loop            #
      INTEGER       lfstart       ! Leaf cohort,oldest             d
      INTEGER       pcnum(0:dinx) ! Pustule,cohort number          #
      INTEGER       psizegf(dinx) ! Pustule growth phase (1,2,3)   #
      INTEGER       run           ! Run number                     #
      INTEGER       runi          ! Run (internal) number          #
      !INTEGER       sppnumo       ! Species number,overall         #
      INTEGER       step          ! Step                           #
      INTEGER       stepnum       ! Step number per day            #
!     INTEGER       stgdoy(20)    ! Stage dates (Year+Doy)         #
      INTEGER       year          ! Year (4 digits)                #
      INTEGER       yeardoy       ! Year+Doy (7digits)             #
      INTEGER       yearsim       ! Year+Doy for simulation start  #

      REAL          dcfac(dcnx)   ! Disease control gr factor 0-1  #
      REAL          dcdur(dcnx)   ! Disease control duration       d
      REAL          dcday(dcnx)   ! Disease control day after appn d
      REAL          dewfac(0:dinx) ! Disease,dew factor  0-1       #
      REAL          dewhr         ! Dew duration                   h
      REAL          dewmin(dinx)  ! Disease,dew requirement        h
      REAL          dicfac(dinx)  ! Disease control factor 0-1     #
      REAL          dicfacd(dinx) ! Disease control fac decay rate /d
      REAL          didl(dinx)    ! Disease,duration latent per    d
      REAL          dids(dinx)    ! Disease,duration,sporulation   d
      REAL          diffacr(dinx) ! Dis favourability requirement  #
      REAL          digfac(dinx)  ! Disease growth factor 0-1      #
      REAL          diid(0:dinx)  ! Disease,intensity %            #
      REAL          diiscf(0:dinx)  ! Disease,spore cloud factor   #
      REAL          diiscfi(0:dinx) ! Disease,spore cloud fac,init #
      !REAL          diit          ! Disease,intensity,all %        #
      REAL          diitf(0:dinx) ! Disease,temperature fact,inf   #
      REAL          dill(lcx,dinx,pcnumx)  ! Disease,late,by co,lf cm2
      REAL          diltf(dinx)   ! Disease,temp factor,latent     #
      REAL          diltot(dinx)  ! Disease,latent area,total      cm2/p
      REAL          din(lcx,dinx) ! Disease,necrotic area,cohort   cm2
      REAL          dinco(dinx)   ! Disease,interference coeff     #
      REAL          dinl(lcx,dinx,pcnumx) ! Disease,necr,by co,lf  cm2
      REAL          dintot(0:dinx)! Disease,necrotic area,total    cm2/p
!     REAL          dis(lcx,dinx) ! Disease,sporulating area,coh   cm2/p
      REAL          disl(lcx,dinx,pcnumx)  ! Disease,spor,by co,lf cm2
      REAL          distot(0:dinx)! Disease,sporulating area,tot   cm2/p
      REAL          dit(lcx,dinx) ! Disease,total area,lf cohort   cm2
      REAL          ditot(0:dinx) ! Disease,total area             cm2/p
      REAL          favfac(0:dinx)! Favourability factor           #
      REAL          favfacsm(dinx)! Favourability factor sum       #
      REAL          intfac(lcx)   ! Disease init,interference fac  #
      REAL          llap(0:lcx)   ! Leaf lamina area,cohort        cm2/p
      REAL          llapd         ! Leaf lamina area               cm2/p
      REAL          llapp(lcx)    ! Leaf lamina area,infected      cm2/p
      REAL          llaps(lcx)    ! Leaf lamina area,senescent     cm2/p
      REAL          llapsd        ! Leaf lamina area senesced      cm2/p
      REAL          llapsfr       ! Leaf lamina area,newly sen fr  fr
      REAL          llapsy(lcx)   ! Leaf lamin area,senescent,yday cm2/p
      REAL          llapy(0:lcx)  ! Leaf lamina area,cohort,yestdy cm2/p
      REAL          page(dinx,pcnumx)     ! Disease,cohort age     did
      REAL          pltpop        ! Plant Population               #/m2
      REAL          pnumc(dinx)   ! Pustule no per cohort          #
      REAL          pnumcs        ! Disease cohort size,standard   m2/m2
      REAL          pnumg(dinx)   ! Disease,pustule initiation #   #/p
      REAL          pnumgx(dinx)  ! Disease,pustule initiation,max #/cm2
      REAL          pnumlc(lcx,dinx,pcnumx) ! Pustule #/c.leaf     #
      REAL          pnumlg(lcx,dinx) ! Disease,pustule init #/leaf #/lfp
      REAL          pnumtot(dinx) ! Disease,pustule number/plant   #/p
      REAL          pnumtota(0:dinx) ! Disease,pustule number/area #/cm2
      REAL          pnumtots      ! Disease,pustules/area,all dis  #/cm2
      REAL          pnumvis(0:dinx) ! Pustule number,visible       #
      !REAL          pnumvisa(dinx)! Pustule number,visible         #
      REAL          psize         ! Pustule size                   cm2
      REAL          psizeg(lcx,dinx)      ! Pustule area growth    cm2
      REAL          psizex(dinx)  ! Pustule area,maximum           cm2
      REAL          tbase(dinx)   ! Base temperature               C
      REAL          tfac4         ! Temperature factor function    #
      REAL          tkill(dinx)   ! Temperature at which killed    C
      REAL          tmax          ! Temperature maximum for day    C
      REAL          tmean         ! Temperature mean for day       C
      REAL          tmin          ! Temperature minimum for day    C
      REAL          topt1(dinx)   ! Lowermost optimum temperature  C
      REAL          topt2(dinx)   ! Uppermost optimum temperature  C
      REAL          tresp(4)      ! Temperature response cardinals C
      REAL          ttop(dinx)    ! Ceiling temperature            C
      REAL          ttout         ! Thermal units output from func C.d
      REAL          tvr1          ! Temporary real variable        #

      CHARACTER (LEN=10)  dinxc         ! Disease number,character
      CHARACTER (LEN=10)  tl10fromi     ! Character from integer
      CHARACTER (LEN=12)  diname(dinx)  ! Disease name
      CHARACTER (LEN=12)  fnamedis(0:5) ! File name,disease outputs
      CHARACTER (LEN=12)  fnamelf       ! File name,leaves
      CHARACTER (LEN=6)   diidc         ! Disease intensity
      CHARACTER (LEN=6)   dintotc       ! Necrotic area,total cm2/p
      CHARACTER (LEN=6)   distotc       ! Sporulating area,total cm2/p
      CHARACTER (LEN=6)   ditotc        ! Total area cm2/p
      CHARACTER (LEN=6)   psizec        ! Pustule size cm2
      CHARACTER (LEN=64)  spdirfle      ! Species directory+file
      CHARACTER (LEN=79)  outhed        ! Output file heading

!     LOGICAL             fopen         ! File open indicator

      INTRINSIC  AMAX1,AMIN1,EXP,FLOAT,INT,MAX,MIN,MOD

      SAVE

      YEARDOY = YEAR*1000 + DOY

      IF (DYNAMIC.EQ.RUNINIT .OR. DYNAMIC.EQ.SEASINIT) THEN

        IF (DYNAMIC.EQ.RUNINIT) THEN
          YEARSIM = YEARDOY
!          IF (FNUMWRK.LE.0.OR.FNUMWRK.GT.1000) THEN
!            CALL Getlun ('WORK.OUT',fnumwrk)
!            INQUIRE (FILE = 'WORK.OUT',OPENED = fopen)
!            IF (.NOT.fopen) OPEN (UNIT = fnumwrk,FILE = 'WORK.OUT')
!          ENDIF
          CALL Getlun ('DISEASE0',fnumdis(0))
          CALL Getlun ('DISEASE1',fnumdis(1))
          CALL Getlun ('DISEASE2',fnumdis(2))
          CALL Getlun ('DISEASE3',fnumdis(3))
          CALL Getlun ('DISEASE4',fnumdis(4))
          CALL Getlun ('DISEASE5',fnumdis(5))
          CALL Getlun ('LEAVESDI',fnumlfdi)
          diiscfi(0) = 0.1
          fnamedis(0)='DISEASE0.OUT' ! File name,disease outputs text
          fnamedis(1)='DISEASE1.OUT' ! File name,disease outputs text
          fnamedis(2)='DISEASE2.OUT' ! File name,disease outputs text
          fnamedis(3)='DISEASE3.OUT' ! File name,disease outputs text
          fnamedis(4)='DISEASE4.OUT' ! File name,disease outputs text
          fnamedis(5)='DISEASE5.OUT' ! File name,disease outputs text
          fnamelf='LEAVESDI.OUT'     ! File name,individual lves text
        ENDIF

        ! Model parameters
        !sppnumo=1                                 ! Output only for spp
        stepnum = 1
        pnumcs=0.01  ! Standard pustule number/cm2 per cohort

        dinxc = tl10fromi(dinx)

        diid = 0.0
        diitf = 0.0
        diltf = 0.0
        dewfac = 0.0
        diiscf = 0.0
        pcnum = 0
        didate = -99
        didoy = 0
        diiscfi = -99.0
        pnumg = 0.0
        disl = 0.0
        dill = 0.0
        dinl = 0.0
        dicfac = 0.0
        dicfacd = 0.0
        dcday = -99.0
        favfac = 0.0
        favfacsm = 0.0

        ! Interference factor at start
        intfac = 0.0

        CALL SPREADCA (SPDIRFLE,'DI_NAME',dinxc,diname)

        CALL SPREADRA (SPDIRFLE,'DAPX',dinxc,psizex)
        CALL SPREADRA (SPDIRFLE,'DLDUR',dinxc,didl)
        CALL SPREADRA (SPDIRFLE,'DSDUR',dinxc,dids)
        CALL SPREADRA (SPDIRFLE,'DDEWN',dinxc,dewmin)
        CALL SPREADRA (SPDIRFLE,'DINMX',dinxc,pnumgx)

        CALL SPREADIA (SPDIRFLE,'DITYP',dinxc,psizegf)

        DO dinum = 1,dinx
          ditype2(dinum) = INT(MOD(FLOAT(psizegf(dinum)),10.0))
          psizegf(dinum) = psizegf(dinum)/10
        ENDDO

        CALL SPREADRA (SPDIRFLE,'TBASE',dinxc,tbase)
        CALL SPREADRA (SPDIRFLE,'TOPT1',dinxc,topt1)
        CALL SPREADRA (SPDIRFLE,'TOPT2',dinxc,topt2)
        CALL SPREADRA (SPDIRFLE,'TTOP',dinxc,ttop)
        CALL SPREADRA (SPDIRFLE,'TKILL',dinxc,tkill)
        CALL SPREADRA (SPDIRFLE,'DINCO',dinxc,dinco)

!        WRITE (fnumwrk,*) 'DISEASE DETAILS '
!        WRITE (fnumwrk,'(A13,A12)') '  DISEASE 1  ',DINAME(1)
!        WRITE (fnumwrk,'(A13,A12)') '  DISEASE 2  ',DINAME(2)
!        WRITE (fnumwrk,'(A13,A12)') '  DISEASE 3  ',DINAME(3)

!        WRITE (fnumwrk,'(A7, 3F7.1)') '  TBASE',(tbase(i),i = 1,3)
!        WRITE (fnumwrk,'(A7, 3F7.1)') '  TOPT1',(topt2(i),i = 1,3)
!        WRITE (fnumwrk,'(A7, 3F7.1)') '  TOPT2',(topt2(i),i = 1,3)
!        WRITE (fnumwrk,'(A7, 3F7.1)') '  TTOP',(ttop(i),i = 1,3)

!        WRITE (fnumwrk,'(A7, 3F7.3)') '  psizx',(psizex(i),i = 1,3)
!        WRITE (fnumwrk,'(A7, 3I7  )') '  psizf',(psizegf(i),i = 1,3)
!        WRITE (fnumwrk,'(A7, 3I7  )') '  DITY2',(ditype2(i),i = 1,3)
!        WRITE (fnumwrk,'(A7, 3F7.1)') '  DLDUR',(didl(i),i = 1,3)
!        WRITE (fnumwrk,'(A7, 3F7.1)') '  DSDUR',(dids(i),i = 1,3)
!        WRITE (fnumwrk,'(A7, 3F7.1)') '  DDEWN',(dewmin(i),i = 1,3)
!        WRITE (fnumwrk,'(A7, 3F7.1)') '  DINMX',(pnumgx(i),i = 1,3)
!        WRITE (fnumwrk,'(A7, 3F7.1)') '  TKILL',(tkill(i),i = 1,3)
!        WRITE (fnumwrk,'(A7, 3F7.1)') '  DINCO',(dinco(i),i = 1,3)

        DO dinum=1,dinx
         diiscfi(dinum)=digfac(dinum)/10.0
         IF(diiscfi(dinum).GT.1.0)diiscfi(dinum)=1.0
         IF(diiscfi(dinum).LT.0.0)diiscfi(dinum)=1.0
         IF(digfac(dinum).LT.0.0)digfac(dinum)=1.0
        ENDDO

        IF (RUN.EQ.1.AND.RUNI.LE.1) THEN
          OPEN(UNIT=fnumlfdi,FILE=fnamelf)
          dinum = 1
          WRITE(fnumlfdi,'(A9,I1,A26)')
     &     '$DISEASE ',DINUM,' LEAF DISTRIBUTION DETAILS'
          CLOSE (fnumlfdi)
          ! Disease#.out file dealing with details for 1 disease
          DO l=0,dinx
            OPEN(UNIT=fnumdis(l),FILE=fnamedis(l))
            dinum = l
            IF(dinum.GT.dinx)dinum=dinx
            WRITE(fnumdis(l),'(A9,I1,A8)')
     &       '$DISEASE ',DINUM,' DETAILS'
            CLOSE (fnumdis(l))
          End Do
        ENDIF

        ! Leaf details
        OPEN (UNIT = fnumlfdi, FILE = fnamelf, STATUS='UNKNOWN',
     &     POSITION = 'APPEND')
        WRITE (fnumlfdi,'(/,A79,/)') OUTHED
        WRITE (fnumlfdi,2103)
 2103   FORMAT('@YEAR DOY   DAP  LAP1  LAD1  LAS1  LAP2  LAD2  LAS2 ',
     x  ' LAP3  LAD3  LAS3  LAP4  LAD4  LAS4  LAP5  LAD5  LAS5  LAP6 ',
     x  ' LAD6  LAS6  LAP7  LAD7  LAS7  LAP8  LAD8  LAS8  LAP9  LAD9 ',
     x  ' LAS9 LAP10 LAD10 LAS10')

        ! Disease#.out file dealing with details for 1 disease
        DO l=0,dinx
        OPEN (UNIT = fnumdis(l), FILE = fnamedis(l), STATUS='UNKNOWN',
     &     POSITION = 'APPEND')
        WRITE (fnumdis(l),'(/,A79,/)') OUTHED
        WRITE (fnumdis(l),2102)
 2102   FORMAT('@YEAR DOY   DAP PCNUM  PNUM PSIZE  DIST  DINT   DIT ',
     x  ' DIID  DIDF DISCF  DITF  DIFF  INF1 INF10')
        ENDDO
      ENDIF                                      ! End of initialization

      IF(lfcnum.LE.0)RETURN                      ! If no leaf cohorts
      IF(step.NE.stepnum)RETURN                  ! If not at end of day

      IF (DYNAMIC.EQ.RATE) THEN

        TMEAN = (TMAX+TMIN)/2.0

        IF(lfstart.EQ.0.AND.lfcnum.GT.0)THEN      ! If no cohort number
         DO lfnum = 1,lfcnum
           IF (LLAP(LFNUM)-LLAPS(LFNUM).GT.0) THEN
             lfstart=lfnum
             EXIT
           ENDIF
         ENDDO
        ENDIF

        DO dinum=1,dinx                           ! Do over all diseases

          ! Control factors
          IF (dcday(dinum).GE.0) dcday(dinum) = dcday(dinum)+1.0
          IF (dcday(dinum).GT.dcdur(dinum)*0.75)
     &     dicfac(dinum) = AMAX1(0.0,dicfac(dinum)-dicfacd(dinum))
          DO dcnum = 1,dcnx
            IF (dctar(dcnum).EQ.dinum) THEN
              IF (dcdat(dcnum).EQ.yeardoy) THEN
                dicfac(dinum) = dcfac(dinum)
!                WRITE(fnumwrk,'(A33,I1)')
!     &           ' Control application for disease ',dinum
                IF (dcdur(dinum).GT.0.0) THEN
                  dicfacd(dinum) = dicfac(dinum)/(dcdur(dinum)*0.25)
                ENDIF
                dcday(dinum) = 0.0
              ENDIF
            ENDIF
          ENDDO

!          IF (dinum.EQ.dinx) THEN
!            WRITE(fnumwrk,*)' '
!            WRITE(fnumwrk,'(A34,3F7.3)')
!     &       ' Control factors for diseases 123 ',
!     &       dicfac(1),dicfac(2),dicfac(3)
!          ENDIF

          ! Temperature factors
          tresp(1) = tbase(dinum)
          tresp(2) = topt1(dinum)
          tresp(3) = topt2(dinum)
          tresp(4) = ttop(dinum)
          diitf(dinum) = TFAC4(tresp,tmean,TTOUT)
          diltf(dinum) = TFAC4(tresp,tmean,TTOUT)

          ! Dew factor
          IF (dewmin(dinum).gt.0.0) THEN
            dewhr = 8.0  ! Temporary
            IF(((tmax+tmin)/2).LT.10.0.AND.
     &        dewhr.GT.(tmax+tmin)/2*2/5)THEN
            dewfac(dinum)=(1-EXP(-0.1*(dewhr-10)))
            ELSE
             IF(((tmax+tmin)/2).LT.15..AND.
     &            dewhr.GT.10-(tmax+tmin)/5)THEN
              tvr1=0.1+0.02*((tmax+tmin)/2-10.0)
              dewfac(dinum)=
     &         (1-EXP(-tvr1*(dewhr-(10-(tmax+tmin)/2*2/5))))
             Else
              IF(((tmax+tmin)/2).GE.13.0.AND.dewhr.GT.8.0)THEN
               tvr1=0.1+0.02*((tmax+tmin)/2-10.0)
               dewfac(dinum)=(1-EXP(-tvr1*(dewhr-8.0)))
              Else
               dewfac(dinum)=0.0
              END IF
             END IF
            END IF
            dewfac(dinum)=AMAX1(0.0,dewfac(dinum))
          ELSE
            dewfac(dinum)=1.0
          ENDIF

          ! Spore cloud factor
          diiscf(dinum) =
     &       MIN(1.0,100.0*(distot(dinum)*pltpop*0.0001)+diiscfi(dinum))
          ! IF(rain.GT.30)diiscf=0.0

          ! Favourability factor (ie.temperature*cloud factors)
          IF (dinum.eq.2) then
            dewhr = 8.0  ! Temporary
            favfac(dinum) = ((0.0107*((tmean-0.8501)**1.0598)*
     &                      ((31.0935-tmean)**0.5839))*
     &                      1.0943 /
     &                      (1.0+55.1014*exp(-0.4188*dewhr)))
          ELSEIF (dinum.eq.3) then
            dewhr = 8.0  ! Temporary
            favfac(dinum) = ((0.06*((tmean-0.80)**0.733))*
     &                      ((35.0-tmean)**0.32))*
     &                      0.9842 /
     &                      (1.0+exp(5.82-(0.386*dewhr)))
          ELSE
            favfac(dinum) = diitf(dinum) * dewfac(dinum)
          ENDIF

          DO lfnum = lfstart,lfcnum

            ! Interference factor
            IF(llap(lfnum)-llaps(lfnum).GT.0)THEN
              IF(dit(lfnum,dinum).LE.0)THEN
                intfac(lfnum) = 0.0
              ELSE
                IF ((llap(lfnum)-llaps(lfnum)-llapp(lfnum)).GT.0.) THEN
                 intfac(lfnum) = MIN(1.0,dit(lfnum,dinum)
     &           /(dinco(dinum)*(llap(lfnum)-llaps(lfnum))))
                 ! Originally used the following. Need work on int.fac
                 !/(0.37*(llap(lfnum)-llaps(lfnum))))
                ELSE
                 intfac(lfnum) = 1.0
                ENDIF
              ENDIF
            ENDIF

            ! Rate of appearance of new pustules
            pnumlg(lfnum,dinum)=AMIN1(1.0,AMAX1(0.0,
     X       AMIN1((1.0-dicfac(dinum)),digfac(dinum))
     X       * favfac(dinum)*diiscf(dinum)*(1.0-intfac(lfnum))))
     x       * pnumgx(dinum)
     x       * AMAX1(0.0,(llap(lfnum)-llaps(lfnum)-llapp(lfnum)))
            pnumg(dinum) = pnumg(dinum) + pnumlg(lfnum,dinum)

            ! Rate of area growth of pustules (To max at end of phase)
            psizeg(lfnum,dinum) =
     X      AMIN1((1.0-dicfac(dinum)),digfac(dinum))
     &      * diltf(dinum)*(1.0-intfac(lfnum))
     x      * psizex(dinum)/didl(dinum)

          ENDDO

        ENDDO                                     ! End of diseases loop

      ENDIF                                      ! End of rates 'if'


      IF (DYNAMIC.EQ.INTEGR) THEN

       DAS = MAX(0,CSTIMDIF(YEARSIM,YEARDOY))

       pnumtots=0.0

       ! Growth
       DO dinum=1,dinx                           ! Do over all diseases

         IF(didate(dinum).GT.0.AND.
     X    yeardoy.GE.didate(dinum))THEN           ! IF disease initiated

           IF(lfstart.GT.0)THEN                    ! If available leaf
            pnumvis(dinum) = 0.0
            pnumtot(dinum)=0.0
            diid(dinum)=0.0                        ! Disease intensity
            DO lfnum=lfstart,lfcnum
             !dis(lfnum,dinum)=0.0
             din(lfnum,dinum)=0.0
            ENDDO

            ! Add pustules to initiating cohort
            pnumc(dinum)=pnumc(dinum)+pnumg(dinum)

            ! Add new cohort IF enough pustules
            ! Check below
            ! IF (pnumc(dinum)/(llapd-llapsd-llappd).GT.pnumcs)THEN
            IF (pnumc(dinum)/(llapd-llapsd).GT.pnumcs)THEN
               pcnum(dinum)=pcnum(dinum)+1
               IF(pcnum(dinum).EQ.pcnumx)THEN
!                 Write (fnumwrk,*) ' Working with disease: ',dinum
!                 Write (fnumwrk,*)
!     &            ' Disease cohort # reached : ',pcnum(dinum)
                 Write (*,*) ' Working with disease: ',dinum
                 Write (*,*) ' Disease cohort # reached : ',pcnum(dinum)
                 WRITE (*,*) ' Program will have to stop'
                 WRITE (*,*) ' Check WORK.OUT for details of run'
                 STOP ' '
               ENDIF
               ! Distribute pustules over leaves
               DO lfnum=lfstart,lfcnum
                pnumlc(lfnum,dinum,pcnum(dinum))=pnumlg(lfnum,dinum)
                pnumlg(lfnum,dinum) = 0.0
              ENDDO
              pnumc(dinum)=0.0
            ENDIF

            ! Cohort age
            IF(pcnum(dinum).GT.0)THEN
             DO cnum=1,pcnum(dinum)
              page(dinum,cnum)=page(dinum,cnum)+diltf(dinum)
             END DO
            ENDIF

            DO lfnum=lfstart,lfcnum

              DO cnum=1,pcnum(dinum)

                ! Below sums pustule number
                pnumtot(dinum)=pnumtot(dinum)+pnumlc(lfnum,dinum,cnum)
                IF(page(dinum,cnum).GE.didl(dinum))
     &           pnumvis(dinum) = pnumvis(dinum) +
     &            pnumlc(lfnum,dinum,cnum)
                IF (llapd-llapsd.GT.0.0) THEN
                  pnumtota(dinum) = pnumtot(dinum)/(llapd-llapsd)
                  !pnumvisa(dinum) = pnumvis(dinum)/(llapd-llapsd)
                  pnumtots = pnumtots + pnumtot(dinum)/(llapd-llapsd)
                ELSE
                  pnumtota(dinum) = -99.0
                ENDIF

                ! Below sums latent,sporulating,and necrotic areas
                ! Latent
                IF(page(dinum,cnum).GE.0.0.AND.
     x             page(dinum,cnum).LT.(didl(dinum)))THEN

                   IF (psizegf(dinum).EQ.1.OR.psizegf(dinum).EQ.3) THEN
                     dill(lfnum,dinum,cnum) =
     X               dill(lfnum,dinum,cnum) +
     X                psizeg(lfnum,dinum)*pnumlc(lfnum,dinum,cnum)
                   ENDIF

                ! Sporulating
                ELSEIF(page(dinum,cnum).GE.didl(dinum).AND.
     x           page(dinum,cnum).LT.(didl(dinum)+dids(dinum)))THEN

                  IF (dill(lfnum,dinum,cnum).GT.0.0) THEN
                    disl(lfnum,dinum,cnum) = dill(lfnum,dinum,cnum)
                    dill(lfnum,dinum,cnum) = 0.0
                  ENDIF
                  IF (psizegf(dinum).EQ.2.OR.psizegf(dinum).EQ.3) THEN
                    disl(lfnum,dinum,cnum) =
     X               disl(lfnum,dinum,cnum) +
     X               psizeg(lfnum,dinum)*pnumlc(lfnum,dinum,cnum)
                  ENDIF

                ! Necrotic area
                ELSEIF (ditype2(dinum).EQ.0 .AND. page(dinum,cnum).GE.
     &            (didl(dinum)+dids(dinum)))THEN
                  IF (disl(lfnum,dinum,cnum).GT.0.0) THEN
                    dinl(lfnum,dinum,cnum) = disl(lfnum,dinum,cnum)
                    disl(lfnum,dinum,cnum) = 0.0
                  ENDIF
                  din(lfnum,dinum) = din(lfnum,dinum) +
     &             dinl(lfnum,dinum,cnum)

                ENDIF

              ENDDO

            ENDDO

           ENDIF ! End leaf cohort 'if'

         ENDIF ! Diseases initiated

         ! Adjustment for senescence of leaves
         IF(lfstart.GT.0)THEN
           DO lfnum=lfstart,lfcnum
             IF (LLAPS(lfnum)-LLAPSY(lfnum).GT.0.0) THEN
               LLAPSFR = (LLAPS(lfnum)-LLAPSY(lfnum))
     &                 / (LLAPY(lfnum)-LLAPSY(lfnum))
               IF (LLAPSFR.GT.1.0) THEN
!                 WRITE(fnumwrk,*)
!     &            '  Warning. New senesent leaf greater than green area'
                 llapsfr = 1.0
               ENDIF
               DO cnum = 1,pcnum(dinum)
                 pnumlc(lfnum,dinum,cnum) =
     &            pnumlc(lfnum,dinum,cnum)*(1.0-llapsfr)
                 dill(lfnum,dinum,cnum) =
     &            dill(lfnum,dinum,cnum)*(1.0-llapsfr)
                 disl(lfnum,dinum,cnum) =
     &            disl(lfnum,dinum,cnum)*(1.0-llapsfr)
                 dinl(lfnum,dinum,cnum) =
     &            dinl(lfnum,dinum,cnum)*(1.0-llapsfr)
               END DO
             ENDIF
           ENDDO
         ENDIF ! End senescence adjustment

         ! Kill
         IF(tmax.GE.tkill(dinum).AND.tkill(dinum).GT.0.0) THEN
           IF(lfstart.GT.0)THEN
             DO lfnum=lfstart,lfcnum
               DO cnum = 1,pcnum(dinum)
                 dinl(lfnum,dinum,cnum) = dinl(lfnum,dinum,cnum)
     &                                  + disl(lfnum,dinum,cnum)
                 disl(lfnum,dinum,cnum) = 0.0
               END DO
             ENDDO
           ENDIF
         ENDIF ! End temperature kill

         ! Sums for each disease
         diltot(dinum)=0.0
         distot(dinum)=0.0
         dintot(dinum)=0.0
         ditot(dinum)=0.0
         pnumtot(dinum)=0.0
         pnumvis(dinum)=0.0
         DO lfnum=lfstart,lfcnum
           dit(lfnum,dinum) = 0.0
         ENDDO
         IF(lfstart.GT.0)THEN
          DO lfnum=lfstart,lfcnum
            DO cnum = 1,pcnum(dinum)
              pnumtot(dinum)=pnumtot(dinum)+pnumlc(lfnum,dinum,cnum)
              IF(page(dinum,cnum).GE.didl(dinum))
     &         pnumvis(dinum) = pnumvis(dinum) +
     &         pnumlc(lfnum,dinum,cnum)
              diltot(dinum)=diltot(dinum)+dill(lfnum,dinum,cnum)
              distot(dinum)=distot(dinum)+disl(lfnum,dinum,cnum)
              dintot(dinum)=dintot(dinum)+dinl(lfnum,dinum,cnum)
              dit(lfnum,dinum) = dit(lfnum,dinum) +
     &         disl(lfnum,dinum,cnum) + dinl(lfnum,dinum,cnum)
            ENDDO
          ENDDO
          ditot(dinum)=distot(dinum)+dintot(dinum)
          diid(dinum)=MIN(100.0,(ditot(dinum)/(llapd-llapsd)*100))
         ENDIF

       ENDDO ! Dinum loop

       ! Sums for all diseases together
       distot(0)=0.0
       dintot(0)=0.0
       ditot(0)=0.0
       pcnum(0)=0
       pnumtota(0)=0.0
       pnumvis(0)=0.0
       diid(0)=0.0
       dewfac(0)=0.0
       diiscf(0)=0.0
       diitf(0)=0.0
       favfac(0)=0.0
       DO dinum=1,dinx
         pcnum(0)=pcnum(0)+pcnum(dinum)
         pnumtota(0)=pnumtota(0)+pnumtota(dinum)
         pnumvis(0)=pnumvis(0)+pnumvis(dinum)
         distot(0)=distot(0)+distot(dinum)
         dintot(0)=dintot(0)+dintot(dinum)
         ditot(0)=ditot(0)+ditot(dinum)
         diid(0)=diid(0)+diid(dinum)
         dewfac(0)=dewfac(0)+dewfac(dinum)/dinx
         diiscf(0)=diiscf(0)+diiscf(dinum)/dinx
         diitf(0)=diitf(0)+diitf(dinum)/dinx
         favfac(0)=favfac(0)+favfac(dinum)/dinx
       ENDDO
       IF(llapd-llapsd.GT.0) THEN
         !diit=MIN(100.0,(distot(0)+dintot(0))/(llapd-llapsd)*100)
       ELSE
         !diit=0.0
       ENDIF

       IF(lfstart.GT.0)THEN
        DO lfnum=lfstart,lfcnum
         llapp(lfnum)=0.0
         DO dinum=1,dinx
          llapp(lfnum)=llapp(lfnum)+dit(lfnum,dinum) ! Add all diseased
         ENDDO
        ENDDO
       ENDIF

       ! Initiation
       DO dinum=1,dinx
         favfacsm(dinum) = favfacsm(dinum) + favfac(dinum)
         IF (didat(dinum).GT.0.AND.didate(dinum).LE.0) THEN
           IF (didat(dinum).EQ.yeardoy) THEN
!             WRITE(fnumwrk,'(A36,I1,A4,I7)')
!     &        ' Initiation (fixed date) of disease ',dinum,' on ',
!     &        yeardoy
             dinuminit = dinuminit + 1
             didate(dinum) = yeardoy
             didoy(dinum) = doy
             !diiscfi(dinum) = 0.1
           ENDIF
         ELSE
           IF (favfacsm(dinum).GT.diffacr(dinum).AND.
     &       didate(dinum).LE.0) THEN
!             WRITE(fnumwrk,'(A35,I1,A4,I7)')
!     &        ' Initiation (automatic) of disease ',dinum,' on ',
!     &        yeardoy
             dinuminit = dinuminit + 1
             didate(dinum) = yeardoy
             didoy(dinum) = doy
             diiscfi(dinum) = diiscfi(0)
           ENDIF
         ENDIF
       ENDDO

       ! Produced and senesced leaf area stored for use next day
       DO lfnum = 1,lfcnum
         LLAPSY(LFNUM) = LLAPS(LFNUM)
         LLAPY(LFNUM) = LLAP(LFNUM)
       ENDDO

      ENDIF                                      ! End integr if

      ! File operations
      IF (DYNAMIC.EQ.OUTPUT) THEN
        dao=dao+1                                 ! Output counter
        IF(cvalo.GT.0.AND.dao.EQ.cvalo.OR.        ! IF OK to write
     x   dap.EQ.0.AND.dao.GT.1.OR.
     x   das.EQ.1)THEN

         ! Leaf details
         WRITE(fnumlfdi,607)year,doy,dap,
     x   (llap(i),llapp(i),llaps(i),i=3,12)
 607     FORMAT(I5,I4,I6,30F6.2)

         ! Details for each disease
         DO dinum = 0, DINX
          IF (pnumvis(dinum).GT.0.0) THEN
            PSIZE = (DISTOT(DINUM)+DINTOT(DINUM))/PNUMVIS(DINUM)
          ELSE
            PSIZE = 0.0
          ENDIF
          CALL Csopline(psizec,psize)
          CALL Csopline(dintotc,dintot(dinum))
          CALL Csopline(ditotc,ditot(dinum))
          CALL Csopline(distotc,distot(dinum))
          CALL Csopline(diidc,diid(dinum))
          WRITE(fnumdis(dinum),300)year,doy,dap,
     X    pcnum(dinum),pnumtota(dinum),psizec,distotc,
     X    dintotc,ditotc,diidc,dewfac(dinum),
     X    diiscf(dinum),diitf(dinum),favfac(dinum),intfac(1),
     X    intfac(10)
 300      FORMAT(I5,I4,I6,I6,F6.2,A6,4A6,F6.2,F6.2,3(F6.2),F6.1)
         ENDDO

         dao=0                                    ! Reset counter
       ENDIF

       IF (DYNAMIC.EQ.FINAL) THEN
        DO L = 0,DINX
          CLOSE(fnumdis(l))
        ENDDO
        CLOSE(fnumlfdi)
       ENDIF

      ENDIF

      RETURN

      END
