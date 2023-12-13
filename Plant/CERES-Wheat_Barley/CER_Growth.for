!***********************************************************************
! This is the code from the section (DYNAMIC == RATE) 
! lines 3524 - 5005 of the original CSCER code.
!***********************************************************************

      SUBROUTINE CER_Growth (BD, CANHT, CO2, DAYLT,
     &     DLAYR, DUL, EO, EOP, ISWNIT, ISWWAT,
     &     KEP, LL, NFP, NH4LEFT, NLAYR , NO3LEFT,
     &     RLV, RNMODE, SAT , SENCALG, SENNALG,
     &     SHF, SLPF, SNOW, SRAD, ST, STGDOY, SW,
     &     TMAX, TMIN, TRWUP, UH2O, UNH4ALG, UNO3ALG, 
     &     WINDSP, YEARPLTCSM, LAI)

! 2023-01-25 chp removed unused variables in argument list
!     DOY, KCAN, WEATHER, SOILPROP, CONTROL, YEAR, IDETG, 
     
        USE ModuleDefs
        USE CER_First_Trans_m
        IMPLICIT NONE
        EXTERNAL CSTIMDIF, Y4K_DOY, CSINCDAT, TFAC4, CSROOTWU, CSTRANS, 
     &    YVALXY

!!       Contructed types defined in ModuleDefs
!        TYPE (ControlType), intent (in) :: CONTROL
!        TYPE (WeatherType), intent (in) :: WEATHER
!        TYPE (SoilType), intent (in) ::   SOILPROP

        INTEGER ADAT10, CSTIMDIF, CSINCDAT, DYNAMICI
        INTEGER NLAYR, STGDOY(20)  !CN, DOY, YEAR
        INTEGER YEARPLTCSM!, YEARPLT         
        REAL BD(20), CANHT, CO2
        REAL DLAYR(20), UNO3ALG(20), SENLGALG(0:20), UNH4ALG(20)
        REAL DUL(20), EO, EOP, KEP, LL(20), NFP, NH4LEFT(20)  !, KCAN
        REAL NO3LEFT(20), RLV(20), SAT(20)
        REAL SENCALG(0:20), SENNALG(0:20), SHF(20), SLPF
        REAL SRAD, ST(0:20), SW(20), TMAX, TMIN, TRWUP
        REAL UH2O(20), WINDSP, LAI
        REAL DAYLT, RWUMX, RWUPM, SNOW, TFAC4, YVALXY
        
        CHARACTER*1 ISWNIT, ISWWAT, RNMODE  !IDETG, 
        
        ! Update so that temporary outputs in rate have correct DAP
        DAE = MAX(0,CSTIMDIF(STGDOY(9),YEARDOY))
        DAP = MAX(0,CSTIMDIF(STGDOY(7),YEARDOY))
        DAS = MAX(0,CSTIMDIF(YEARSIM,YEARDOY))
        IF (ISTAGE.EQ.6) DAPM = DAPM + 1
      
        ! LAH 07/12/2008 For problem of date calculation.
        ! YEARPLTCSM established by CSM;brought across in argument list.
        ! LAH 29/06/11 Added automatic as well
        IF (FILEIOT.EQ.'DS4') THEN
!         IF (IPLTI.EQ.'A' .OR. (INDEX('FQN',RNMODE) > 0)) THEN
          IF (IPLTI.EQ.'A' .OR. IPLTI.EQ.'F' .OR. 
     &       (INDEX('FQNY',RNMODE) > 0)) THEN
            YEARPLTP = YEARPLTCSM
          ENDIF  
        ENDIF

!        IF (FILEIOT.EQ.'XFL') WRITE(fnumwrk,'(A28,I3,I8,2F6.2)')
!     &   ' CN,YEARDOY,XSTAGE1,LEAFNUM ',cn,yeardoy,xstage,lnumsd
!     
!        IF (YEARDOY.LT.YEARPLTP)
!     &  WRITE(fnumwrk,*) 'yeardoy,YEARPLT,YEARPLTP   ',
!     &                     yeardoy,YEARPLT,YEARPLTP

        CFLINIT = 'N'    ! Reset initiation flag for next run

        IF (YEARPLT.GT.9000000) THEN            ! If before planting
          ! Initialize planting depth temperature and water variables
          TSDEP = 0.0
          CUMSW = 0.0
          AVGSW = 0.0
          IF(YEARPLTP.GT.0 .AND. YEARPLTP.LT.9000000)THEN
            IF(YEARDOY.EQ.YEARPLTP)THEN
C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY              
              !YEARPLT = CSYEARDOY(YEARPLTP)
              CALL Y4K_DOY(YEARPLTP,FILEIO,0,ERRKEY,3)
              YEARPLT = YEARPLTP
              PLTPOP = PLTPOPP
              TNUM = 1.0
            ENDIF
          ELSE
            ! Automatic planting
            ! Check window for automatic planting,PWDINF<YEARPLTP<PWDINL
            IF (YEARDOY.GE.PWDINF.AND.YEARDOY.LE.PWDINL) THEN
              ! Within planting window.
              ! Determine if soil temperature and soil moisture ok
              ! Obtain soil temperature, TSDEP, at 10 cm depth
              I = 1
              XDEP = 0.0
              DO WHILE (XDEP .LT. 10.0)
                XDEP = XDEP + DLAYR(I)
                TSDEP = ST(I)
                I = I + 1
              END DO
              ! Compute average soil moisture as percent, AVGSW
              I = 1
              XDEP = 0.0
              CUMSW = 0.0
              DO WHILE (XDEP .LT. SWPLTD)
                XDEPL = XDEP
                XDEP = XDEP + DLAYR(I)
                IF (DLAYR(I) .LE. 0.) THEN
                  !IF SOIL DEPTH IS LOWER THAN SWPLTD -US
                  XDEP = SWPLTD
                  CYCLE
                ENDIF
                DTRY = MIN(DLAYR(I),SWPLTD - XDEPL)
                CUMSW = CUMSW + DTRY *
     &           (MAX(SW(I) - LL(I),0.0)) / (DUL(I) - LL(I))
                I = I + 1
              END DO
              AVGSW = (CUMSW / SWPLTD) * 100.0
!              WRITE (fnumwrk,*) 'Date thresholds ',pwdinf,pwdinl
!              WRITE (fnumwrk,*) 'Water thresholds ',swpltl,swplth
!              WRITE (fnumwrk,*) 'Water ',avgsw
!              WRITE (fnumwrk,*) 'Temperature thresholds ',pttn,ptx
!              WRITE (fnumwrk,*) 'Temperature ',tsdep
              IF (TSDEP .GE. PTTN .AND. TSDEP .LE. PTX) THEN
                IF (AVGSW .GE. SWPLTL .AND. AVGSW .LE. SWPLTH) THEN
                  YEARPLT = YEARDOY
                  PLTPOP = PLTPOPP
                  CFLFAIL = 'N'
                ENDIF
              ENDIF
            ELSE
              IF (YEARDOY.GT.PWDINL) THEN
                CFLFAIL = 'Y'
                STGDOY(1) = -99
                STGDOY(2) = -99
                STGDOY(3) = -99
                STGDOY(4) = -99
                STGDOY(5) = -99
                STGDOY(6) = -99
                STGDOY(7) = -99
                STGDOY(8) = -99
                STGDOY(9) = -99
                STGDOY(10) = YEARDOY
                STGDOY(11) = YEARDOY
                ISTAGE = 7
                XSTAGE = 7.0
!                WRITE (fnumwrk,*) ' '
!                WRITE (fnumwrk,*)
!     &           'Automatic planting failure on ',yeardoy
              ENDIF
            ENDIF
          ENDIF

!          IF (YEARDOY.EQ.YEARPLTP) WRITE (fnumwrk,*)
!     &      'Planting on: ',yeardoy
!          WRITE (fnumwrk,*)
!     &      'Initialising soil profile and other N aspects on: ',yeardoy

          STGDOY(7) = YEARPLT
          SEEDN = SEEDNI
          SEEDRS = SEEDRSI
          SEEDRSAV = SEEDRS
                    
          H2OPROFILEI = 0.0
          H2OROOTZONEI = 0.0
          AH2OPROFILEI = 0.0
          AH2OROOTZONEI = 0.0
          SNO3PROFILEI = 0.0
          SNH4PROFILEI = 0.0
          SNO3ROOTZONEI = 0.0
          SNH4ROOTZONEI = 0.0
          DO L = 1, NLAYR
            FAC(L) = 10.0/(BD(L)*DLAYR(L))
            SNO3PROFILEI = SNO3PROFILEI + (NO3LEFT(L)/FAC(L))
            SNH4PROFILEI = SNH4PROFILEI + (NH4LEFT(L) / FAC(L))
            AH2OPROFILEI = AH2OPROFILEI + ((SW(L)-LL(L))*DLAYR(L))*10.0
            H2OPROFILEI = H2OPROFILEI + SW(L)*DLAYR(L)*10.0
            IF (RLV(L).GT.0.0) THEN
              AH2OROOTZONEI = AH2OROOTZONEI+((SW(L)-LL(L))*DLAYR(L))*10.
              H2OROOTZONEI = H2OROOTZONEI+SW(L)*DLAYR(L)*10.
              SNO3ROOTZONEI = SNO3ROOTZONEI + (NO3LEFT(L)/FAC(L))
              SNH4ROOTZONEI = SNH4ROOTZONEI + (NH4LEFT(L)/FAC(L))
            ENDIF
          END DO

        ENDIF

        ! Reset control flag
        !IF (YEARDOY.GE.YEARPLT) DYNAMICI = 0

        IF (YEARDOY.GE.YEARPLT) THEN   

          ! Photosynthetically active radiation
          PARAD = PARADFAC*SRAD

          ! Mean temperature
          TMEAN = (TMAX+TMIN)/2.0
C-GH      IF (snow.GT.0) THEN
          IF (snow .GT. 0.0) THEN
            tmeans = 0.0
          ELSE
            tmeans = tmean
          ENDIF

          ! Day and night temperatures
          TDAY = TMEAN
          TNIGHT = TMEAN
          ! NB.These could be set in various ways. In Ceres 3.5,there
          ! were various modifications for different processes. Work
          ! with G.McMaster, however, showed that (at least for
          ! development) these were no better than using the daily
          ! mean. Hence the day and night temperatures are set equal
          ! to the daily mean. Other simple settings could be:
          ! TDAY = TMAX
          ! TNIGHT = TMIN
          ! TDAY = TMEAN + 0.5*(TMAX-TMEAN)
          ! TNIGHT = TMIN + 0.5*(TMEAN-TMIN)
          ! And more complex settings could involve using the hourly
          ! temperatures, or modifying values depending on the
          ! difference between TMAX and TMIN.
          TMEAN20S = 0.0
          SRAD20S = 0.0
          STRESS20S = 0.0
          TMEANNUM = TMEANNUM + 1.0
          DO L = 20,2,-1
            TMEAND(L) = TMEAND(L-1)
            TMEAN20S = TMEAN20S + TMEAND(L)
            SRADD(L) = SRADD(L-1)
            SRAD20S = SRAD20S + SRADD(L)
            STRESS(L) = STRESS(L-1)
            STRESS20S = STRESS20S + STRESS(L)
          ENDDO
          TMEAND(1) = TMEAN
          TMEAN20S = TMEAN20S + TMEAND(1)
          SRADD(1) = SRAD
          SRAD20S = SRAD20S + SRAD
          STRESS(1) = AMIN1(WFG,NFG)
          STRESS20S = STRESS20S + STRESS(1)
          IF (TMEANNUM.GE.20.0) THEN
            IF (TMEANNUM.EQ.20.0) TMEAN20P = TMEAN20S/20.0
            TMEAN20 = TMEAN20S/20.0
            SRAD20 = SRAD20S/20.0
            STRESS20 = STRESS20S/20.0
          ELSE
            TMEAN20 = 0.0
            SRAD20 = 0.0
            STRESS20 = 0.0
          ENDIF
          IF (ADAT.GT.0) THEN
            ADAT10 = CSINCDAT(ADAT,10)
            !IF (YEARDOY.EQ.ADAT10) TMEAN20A = TMEAN20
            !IF (YEARDOY.EQ.ADAT10) SRAD20A = SRAD20
            IF (XSTAGE.GE.ASTAGEND.AND.TMEAN20A.LE.0.0) THEN
              TMEAN20A = TMEAN20
              SRAD20A = SRAD20
              STRESS20A = STRESS20
            ENDIF
          ENDIF

!  For investigation of temperature responses
!          WRITE(fnumwrk,*)' tmean   tfd   tfg  tfdg    tt'
!          DO 9821 TVI1 = 0,40
!           TMEAN = FLOAT(TVI1)
!           TMEANS = TMEAN
!         End of stuff to investigate temperature responses          
           
          ! Thermal time
          IF (ISTAGE.GT.4 .AND. ADAT.GT.0 .AND. ISTAGE.LE.6) THEN
            Tfout = TFAC4(trdv2,tmean,TT)
          ELSE
            Tfout = TFAC4(trdv1,tmeans,TT)
            TTmax = trdv1(2) - trdv1(1)
            IF (trgem(3).GT.0.0) THEN
              Tfgem = TFAC4(trgem,tmeans,TTGEM)
            ELSE
              Ttgem = tt
            ENDIF    
          ENDIF

          ! Thermal time averages for various periods
          IF (ISTAGE.LT.7.AND.ISTAGE.GE.4) THEN
            TT20S = 0.0
            TTNUM = TTNUM + 1
            DO L = 20,2,-1
              TTD(L) = TTD(L-1)
              TT20S = TT20S + TTD(L)
            ENDDO
            TTD(1) = TT
            TT20S = TT20S + TTD(1)
            IF (TTNUM.GE.20.0) THEN
              TT20 = TT20S/20.0
            ELSE
              TT20 = -99.0
            ENDIF
          ENDIF

          ! Temperature factors
          IF (ISTAGE.GE.9 .OR. ISTAGE.LE.2) THEN
            Tfv = TFAC4(trvrn,tmeans,TTOUT)
            Tfh = TFAC4(trlth,tmeans,TTOUT)
          ELSE
            TFV = 0.0
            TFH = 0.0
          ENDIF
          !IF (ISTAGE.LE.2) THEN
            Tfg = TFAC4(trlfg,tmean,TTOUT)
          !ENDIF
          IF (ISTAGE.LE.6) THEN
            ! The original temperature response for photosynthesis had
            ! a more rapid rise at low temperatures than the 4 cardinal
            ! temperatures response now used. This is shown below in the
            ! original data:
            !TREFS  TFGR  TFPR  TFVR  TFHR TFGFR TFGNR TFGNM
            ! -5.0   -99   -99  0.00  0.00   -99   -99   -99
            !  0.0  0.00  0.00  1.00  1.00  0.00  0.00  0.00
            !  2.0   -99  0.40   -99   -99   -99   -99   -99
            !  5.0   -99   -99   -99  1.00   -99   -99   -99
            !  7.0   -99  0.70  1.00   -99   -99   -99   -99
            ! 10.0  1.00  0.85   -99  0.00   -99   -99   -99
            ! 15.0  1.00  1.00  0.00   -99   -99   -99   -99
            ! 16.0   -99   -99   -99   -99  1.00  1.00  1.00
            ! 20.0  1.00  1.00   -99   -99   -99   -99   -99
            ! 25.0  1.00   -99   -99   -99   -99   -99   -99
            ! 26.0   -99  0.85   -99   -99   -99   -99   -99
            ! 30.0   -99  0.50   -99   -99   -99   -99   -99
            ! 35.0  0.00  0.00   -99   -99  1.00  1.00  1.00
            ! 45.0   -99   -99   -99   -99  1.00  1.00  1.00
            ! The original call to obtain TFP was:
            ! TFP = TFCALC2(TREFS,TFPR,'20',TDAY,TDAY,SNOW)
            Tfp = TFAC4(trphs,tmean,TTOUT)
            ! Ceres35 PRFT = 1.0-0.0025*((0.25*TMIN+0.75*TMAX)-18.0)**2
          ENDIF
          IF (ISTAGE.EQ.4.OR.ISTAGE.EQ.5) THEN
            Tfgf = TFAC4(trgfw,tmean,TTOUT)
            Tfgn = TFAC4(trgfn,tmean,TTOUT)
          ENDIF
!         To investigate temperature responses          
!          write(fnumwrk,'(5f6.2)')tmean,tfout,tfg,tfout*tfg,tt 
!9821      continue
!          write(fnumwrk,*)'! ttmax = ',trdv1(2)-trdv1(1)
!          stop
!         End of stuff to investigate temperature responses          
          ! Radiation interception (if from competition model)
          IF (PARIP.GE.0.0) THEN
            PARI = PARIP/100.0
!            WRITE(fnumwrk,'(A39,F6.2,A11,I2)')
!     &       ' PARI from competition model          :',PARI,
!     &       ' Component:',CN
!            WRITE(fnumwrk,'(A39,F6.2,7X,F6.2)')
!     &       ' Leaf area (laminae). Index,Per plant: ',
!     &       LAI,PLA-SENLA
          ENDIF

          IF (fileiot(1:2).NE.'DS')
     &     CALL CSTRANS(ISWWAT,                            !Control
     &     TMAX, TMIN, WINDSP, CO2, EO,                    !Weather
     &     CROP, LAI, KEP,                                 !Crop,LAI
     &     eop,                                            !Pot.pl.evap
     &     DYNAMICI)                                       !Control
          IF (fileiot(1:2).NE.'DS')
     &     CALL CSROOTWU(ISWWAT,                           !Control
     &     NLAYR, DLAYR, LL, SAT,                          !Soil
     &     EOP,                                            !Pot.evap.
     &     RLV, RWUPM, RWUMX,                              !Crop state
     &     SW,                                             !Soil h2o
     &     uh2o, trwup,                                    !H2o uptake
     &     DYNAMICI)                                       !Control
          ! Water status factors
          WFG = 1.0
          WFP = 1.0
          WFT = 1.0
          IF (ISWWAT.EQ.'Y' .AND. ISTAGE.LT.7) THEN
            IF (EOP.GT.0.0) THEN
              WUPR = TRWUP/(EOP*0.1)
              WFG = AMAX1(0.0,AMIN1(1.0,WUPR/WFGU))
              WFP = AMAX1(0.0,AMIN1(1.0,WUPR/WFPU))
              IF (XSTAGE.GE.4.0) WFP = 1.0-(1.0-WFP)*WFPGF !Grain fill
              WFT = AMAX1(0.0,AMIN1(1.0,(WUPR-WFTL)/(WFTU-WFTL)))
            ENDIF
          ENDIF

          ! Nitrogen status factors
          NFG = 1.0
          NFP = 1.0
          NFT = 1.0
          IF (ISWNIT.EQ.'Y' .AND. ISTAGE.LT.7) THEN
            LMNCG = LMNC + NFGL * (LCNC-LMNC)
            LCNCG = LMNC + NFGU * (LCNC-LMNC)
            LMNCP = LMNC + NFPL * (LCNC-LMNC)
            LCNCP = LMNC + NFPU * (LCNC-LMNC)
            LCNCSEN = LMNC + NFSU * (LCNC-LMNC)
            LMNCT = LMNC + NFTL * (LCNC-LMNC)
            LCNCT = LMNC + NFTU * (LCNC-LMNC)
            IF (LFWT.GT.0.0) THEN
              NFG = AMIN1(1.0,AMAX1(0.0,(LANC-LMNCG)/(LCNCG-LMNCG)))
              NFP = AMIN1(1.0,AMAX1(0.0,(LANC-LMNCP)/(LCNCP-LMNCP)))
              NFT = AMIN1(1.0,AMAX1(0.0,(LANC-LMNCT)/(LCNCT-LMNCT)))
            ENDIF
          ENDIF
          
          ! Daylength factor. 
          IF (ISTAGE.GE.7) THEN
            DF = PPFPE
          ELSEIF (ISTAGE.GE.1) THEN
            IF (P1D.GE.0.0) THEN      ! Long day plants
              ! Below is a possible age adjusted P1D. But not used
              P1DA=AMAX1(.0,(P1D/10000)-(P1D/10000)*P1DAFAC*(LNUMSD-5.))
              !P1DAFAC is an age adjustment factor
              DF = AMAX1(0.0,AMIN1(1.0,1.0-(P1D/10000)*(P1DT-DAYLT)**2))
            ELSE                      ! Short day plants
              DF = 
     &         AMAX1(0.0,AMIN1(1.0,1.0-(ABS(P1D)/1000)*(DAYLT-P1DT)))
            ENDIF
          ENDIF  
          
          IF (RSTAGE.GT.PPEND .AND.ISTAGE.LT.7) THEN
            DF = 1.0
          ENDIF
          
          ! Light intensity factor
          LIF2 = 1.0
          IF (CROP.EQ.'BA' .AND. SRAD.LE.10.0) THEN
            LIF2 = 1.0 - ( 10.0-SRAD)**2/PLTPOP
          ENDIF
          
          ! Developmental units
          DU = TT*VF*DF*LIF2    ! NB. Changed from Ceres 3.5
          ! DU = TT*AMIN1(VF,DF)*LIF2    ! Ceres 3.5
          DU = AMAX1(0.0, DU)
          
          ! Water factor for germination
          IF (ISWWAT.EQ.'Y' .AND. ISTAGE.GT.7) THEN
            DO L = 1, NLAYR
              CUMDEP = CUMDEP + DLAYR(L)
             IF (SDEPTH.LT.CUMDEP) GO TO 100
            END DO
  100       CONTINUE
            L0 = L                       ! L0 is layer with seed
            DO L = 1,NLAYR
              SWP(L) = 
     &         AMIN1(1.0,AMAX1(.0,((SW(L)-LL(L))/(DUL(L)-LL(L)))))
            ENDDO
            ! LAH Changed after query by Hong via GH
            !SWP(0) = AMIN1(1.0,AMAX1(0.0,(SWP(1)-(SWP(2)-SWP(1)))))
            !SWP(0) is a value at the soil surface
            SWP(0) = AMIN1(1.0,AMAX1(0.0,(SWP(1)-0.5*(SWP(2)-SWP(1)))))
            IF (L0.GT.1) THEN
              SWPSD = SWP(L0)
            ELSE
              ! LAH Changed after query by Hong via GH
              ! SDEPTH*(SWP(2)-SWP(0))))
              SWPSD = SWP(0) + (SDEPTH/DLAYR(1))*(SWP(2)-SWP(0))
            ENDIF
            WFGE = AMAX1(0.0,AMIN1(1.0,(SWPSD/WFGEU)))
          ELSE
            WFGE = 1.0
          ENDIF
          
          ! Germination units
          !GEU = TT*WFGE
          GEU = TTGEM*WFGE
          
          ! Initializations for Growth calculations
          CARBO = 0.0
          GROST = 0.0
          GROLF = 0.0
          GROLFP = 0.0
          GROGRP = 0.0
          GROGRPA = 0.0
          GROGRST = 0.0
          GRORS = 0.0
          GRORSSD = 0.0
          GRORSPM = 0.0
          GRORSP = 0.0
          GROST = 0.0
          GROSTP = 0.0
          PCARB = 0.0
          PLAS = 0.0
          PLASS = 0.0
          PLTLOSS = 0.0
          RSFR = 0.0
          RTRESP = 0.0
          RTWTG = 0.0
          RTWTGRS = 0.0
          TNUMD = 0.0
          TNUMG = 0.0
          TNUMLOSS = 0.0
          DO L = 1,2
            PLAG(L) = 0.0
            PLAGT(L) = 0.0
            PLAGTP(L) = 0.0
          ENDDO
          XSTAGEFS = 0.0
          
          ! 'Light intensity' factor (0.6,1.0 at SRAD's of 0,7)
          LIF1 = 1.0
          IF (CROP.EQ.'BA' .AND. SRAD.LE.10.0) THEN
            LIF1 = 1.0 - ((10.0-SRAD)**2*PLTPOP*0.000025)
          ENDIF
          
          ! CO2 factor
          CO2FP = YVALXY(CO2RF,CO2F,CO2)
          
          ! Temperature factors
          ! Must check. One cold night --> no phs next day!!
          
          ! Tops partition fraction; standard first,then adjustment
          IF (PTFS(ISTAGE+1).GT.0)
     &     PTFSS = PTFS(ISTAGE) + (PTFS(ISTAGE+1)-PTFS(ISTAGE))*SSTAGE
          IF (PTFA(ISTAGE).GT.0) THEN
           PTF = AMIN1(PTFX,PTFSS + PTFA(ISTAGE)*AMIN1(WFP,NFG,LIF1))
          ELSE
           PTF = AMIN1(PTFX,PTFSS)
          ENDIF
          
          ! Within tops distribution fractions
          LFFR = 1.0
          STFRSTG = 0.0
          STFRSTG = (STFR(ISTAGE)
     &       + ((STFR(ISTAGE+1)-STFR(ISTAGE))*(XSTAGE-FLOAT(ISTAGE))))
          IF (ISTAGE.GE.3.AND.ISTAGE.LT.7) STFRSTG = 1.0
          IF (ISTAGE.GT.6) STFRSTG = 0.0
          LFFR = 1.0 - STFRSTG                                          
          IF (XSTAGE.GT.P4SGE) THEN
            RSFR = 1.0
          ELSE
            RSFR = RSPCS/100.0
          ENDIF
          
          ! CO2 Assimilation
          PCARB = PARUV * PARAD/PLTPOP * PARI
          IF (XSTAGE.GE.3.0) THEN
            IF (PARUR.GT.0.0) THEN
             PCARB = PARUR * PARAD/PLTPOP * PARI
            ELSE
             PCARB = PARUV * PARAD/PLTPOP * PARI
            ENDIF  
          ENDIF
          
          ! PCARB = 7.5 * PARAD**0.6/PLTPOP*(1.0-EXP(-0.85*LAI)) !Ceres3
          ! Y1 = 1.5 - 0.768 * ((ROWSPC * 0.01)**2 * PLTPOP)**0.1
          ! PCARB = 1.48 * SRAD/PLTPOP * (1.0 - EXP(-Y1 * LAI)) ! Maize
          ! Following is as in Ceres 3.5. Now eliminated minimum choice
          ! CARBO = AMAX1(0.0,PCARB*CO2FP*TFP*AMIN1(WFP,NFP)*RSFP)
          
          ! LAH 07/08/2008 Added SLPF to match other models.
          !CARBO = AMAX1(0.0,PCARB*CO2FP*TFP*WFP*NFP*RSFP)
          CARBO = AMAX1(0.0,PCARB*CO2FP*TFP*WFP*NFP*RSFP*SLPF)
          ! Following is to stop assim once mature
          IF (XSTAGE.GT.6.0) CARBO = AMAX1(0.0,CARBO*(6.3-XSTAGE)/0.3)
          
          ! Available carbohydrate for growth
          IF (ISTAGE.EQ.6) THEN
            CARBOAT = 0.0
            CARBOR = 0.0
            CARBOASD = 0.0
            CARBOAPM = CARBO
          ELSE
            ! Can mobilize more than required! 
            ! Hence CH2O reserves may increase
            ! SDAVFR operates for both tops and roots
            IF (SEEDRSAV.GT.1.0E-9) THEN
              CARBOASD = SDAFR*(TT/STDAY)*SEEDRSAV
            ELSE
              CARBOASD = 0.0 
            ENDIF  
            SEEDRSAV = SEEDRSAV - CARBOASD
            CARBOAT = CARBOASD+CARBO*PTF
            CARBOR = CARBO*(1.0-PTF)
            CARBOAPM = 0.0
          ENDIF

          IF (XSTAGE.GE.LAFST.AND.XSTAGE.LT.7.0) THEN
            IF(LNUMSG.GT.0 .AND. LNSWITCH.LE.0.0) THEN
              LNSWITCH = LNUMSD
!              WRITE(fnumwrk,*)' '
!              WRITE(fnumwrk,*)
!     &         'Leaf number when size increment changed ',lnswitch
              LASWITCH = lapot(lnumsg)
!              WRITE(fnumwrk,*)
!     &         'Leaf p.size when size increment changed ',Laswitch
!               WRITE(fnumwrk,*)
!     &         'Next p.size when size increment changed ',
!     &          Lapot(lnumsg+1)
            ENDIF
          ENDIF   
          
          IF (LNUMSG.GT.0) THEN
            IF (LNUMSG.GT.0.AND.LNUMSG.LT.LNUMX) THEN
              IF (LNSWITCH.LE.0.0) THEN
                LAPOT(LNUMSG+1) = 
     &            AMAX1(LAPOT(1),LAPOT(LNUMSG))*(1.0+LAFV)
              ELSE  
                LAPOT(LNUMSG+1) = LAPOT(LNUMSG)*(1.0+LAFR)
              ENDIF
              IF (LAPOT(LNUMSG+1).GT.LAXS) LAPOT(LNUMSG+1) = LAXS
              ! NB. In Ceres 3.5 LAPOT(n) was LAPOT(1)*(LNUMSD**0.5) 
            ENDIF    
          ENDIF
          
          ! Growth potentials
          IF (ISTAGE.LE.2) THEN
            LAW = AMAX1(LAWS*LAWFRMN,LAWS-(LAWS*LAWCF)*(LNUMSG-1))
            ! LAW=Leaf area/weight (specific leaf area).Chages with lf #
            IF (LNUMSG.GT.0) THEN
!               In Ceres overall temperature response for lf growth was:
!               EGFT = 1.2 - 0.0042*(TEMPM-17.0)**2 
!               Here, temperature response is a composite of temp response
!               of development (leaf # increase) and leaf expansion.
!               So, EGFT would equal TFD*TFG 
!               Assimilates may control expansion if no reserve available
!               Current leaf expands completely at current phint
!               Leaves expand for 1 PHINT
!               For leaf area growth (PLAG) Ceres 3.5 used: 
!                PLAG(1) = LA1S * (LNUMSD**0.5) * ....
!               (with LA1S = 7.5 (= LAPOT(1),potential area of leaf 1)
              PLAG(1) = LAPOT(LNUMSG) * AMIN1(WFG,NFG) * TFG *
     &         AMIN1(TT/PHINT,(FLOAT(LNUMSG)-LNUMSD)) 
              ! NB. Temp response of development (TT) taken into acount 
              ! If new leaf. Will expand at current or new phint
              IF ((TT/PHINT).GT.(FLOAT(LNUMSG)-LNUMSD)) THEN
                ! If new leaf will be first leaf in 3rd phint phase
                IF (LNUMSD.LE.(PHINTL(1)+PHINTL(2)).AND.
     &              LNUMSD+(TT/PHINT).GT.(PHINTL(1)+PHINTL(2))) THEN
                    TTTMP = TT - PHINT*(FLOAT(LNUMSG)-LNUMSD)
                    PLAG(2) = LAPOT(lnumsg+1) * AMIN1(WFG,NFG) * TFG *
     &                        TTTMP/(PHINTS*PHINTF(3))
                ! If new leaf will be first in 2nd phint phase
                ELSEIF (LNUMSD.LE.PHINTL(1).AND.
     &                  LNUMSD+(TT/PHINT).GT.PHINTL(1)) THEN
                  TTTMP = TT - PHINT*(FLOAT(LNUMSG)-LNUMSD)
                  PLAG(2) = LAPOT(lnumsg+1) * AMIN1(WFG,NFG) * TFG *
     &             TTTMP/(PHINTS)
                ELSE
                ! New leaf in current phint phase
                  PLAG(2) = LAPOT(lnumsg+1) * AMIN1(WFG,NFG) * TFG *
     &            (TT/PHINT-(FLOAT(LNUMSG)-LNUMSD))
                ENDIF
              ! No new leaf
              ELSE
                PLAG(2) = 0.0
              ENDIF
              IF (TT.GT.0.0) THEN
                WFLFNUM(LNUMSG) = WFLFNUM(LNUMSG)+1.0
                WFLFSUM(LNUMSG) = WFLFSUM(LNUMSG)+WFG
                NFLFSUM(LNUMSG) = NFLFSUM(LNUMSG)+NFG
                TFLFSUM(LNUMSG) = TFLFSUM(LNUMSG)+TFG
                WFLF(LNUMSG) = WFLFSUM(LNUMSG)/WFLFNUM(LNUMSG)
                NFLF(LNUMSG) = NFLFSUM(LNUMSG)/WFLFNUM(LNUMSG)
                TFLF(LNUMSG) = TFLFSUM(LNUMSG)/WFLFNUM(LNUMSG)
              ENDIF
              PLAGTP(1) = PLAG(1)
              PLAGTP(2) = PLAG(2)
              DO L = 1,INT(TNUM)
                ! LAH MAY 2013 CHECK THIS ... PLAG SHOULD BE PLAGTP??
                IF (L.LT.20) THEN
                  PLAGTP(1) = PLAGTP(1) + PLAG(1)*LATFR(L+1)
     &                      * AMAX1(0.0,AMIN1(1.0,(TNUM-1.0)))
                  PLAGTP(2) = PLAGTP(2) + PLAG(2)*LATFR(L+1)
     &                      * AMAX1(0.0,AMIN1(1.0,(TNUM-1.0)))
                ELSE
                  TNUMOUT = TNUMOUT + 1
!                  IF (TNUMOUT.LT.2)
!     &             WRITE(fnumwrk,*)'Tiller number at limit of 20! '
                ENDIF
              ENDDO
            ENDIF
          ENDIF
          
          IF (LAW.GT.0.0) 
     &      GROLFP = ((PLAGTP(1)+PLAGTP(2))/LAW)/(1.0-LSHFR)          
          IF (LFFR.GT.0.0) THEN
            GROSTP = GROLFP*(STFRSTG/LFFR)
          ELSE
            GROSTP = CARBOAT*(1.0-RSFR)
          ENDIF  
          GRORSP = CARBOAT*RSFR
          IF (ISTAGE.EQ.4.OR.ISTAGE.EQ.5) THEN
            GROGRP = AMAX1(0.0,LAGSTAGE*TFGF*GRNUM*G2*DU*0.001)
            IF (LAGSTAGE.GT.0.0.AND.TFGF.LT.1.0) THEN
!              WRITE(fnumwrk,'(A44,F6.2)')
!     &         ' Temperature limit on grain growth at xstage',xstage
              TLIMIT = TLIMIT+1
            ENDIF
          ENDIF
          
          ! Actual growth
          GROLF = AMIN1(GROLFP,CARBOAT*LFFR)
                  ! From CSCRP
                  ! Leaf weight increase from plant reserves
                  GROLFRS = 0.0
                  IF (GROLF.LT.GROLFP) THEN
                    GROLFRS = AMIN1(RSWT*RSUSE,GROLFP-GROLF)
                    GROLF = GROLF+GROLFRS
                  ENDIF
          IF (LNUMSG.GT.0.AND.GROLFP.GT.0.0) THEN
            AFLFSUM(LNUMSG) = AFLFSUM(LNUMSG)
     &       + AMIN1(1.0,((CARBOAT*LFFR)/GROLFP))            
          ENDIF
          IF (LNUMSG.GT.0.AND.WFLFNUM(LNUMSG).GT.0.0)
     &     AFLF(LNUMSG) = AFLFSUM(LNUMSG)/WFLFNUM(LNUMSG)
          
          GROST = AMIN1(GROSTP,CARBOAT*STFRSTG*(1.0-RSFR))
         
          IF (GROGRP.GT.GRORSP+RSWT) THEN
            GROGRST = 0.0
            IF (GROST.GT.0.0) THEN
              GROGRST = AMIN1(GROST,GROGRP-(GRORSP+RSWT))
              IF (GROGRST.GT.0.0) THEN
!                WRITE(fnumwrk,*)'CH2O destined for stem used for grain'
              ENDIF
            ENDIF
            IF (GROGRP.GT.GRORSP+RSWT+GROGRST) THEN
!              WRITE(fnumwrk,*)'CH2O limit on grain growth.'
              CH2OLIM = CH2OLIM+1
              if (grnum > 1.e-6) then
!              WRITE(fnumwrk,'(A15,F6.2,A5,F6.3,A10)') ' CH2O shortage:',
!     &         (GROGRP-(GRORSP+RSWT+GROGRST)),' g/p ',
!     &         (GROGRP-(GRORSP+RSWT+GROGRST))/GRNUM,' g/kernel '
              endif
            ENDIF
          ENDIF
          GROGRPA = AMIN1(GROGRP,GRORSP+RSWT+GROGRST)
          
          GRORSSD = -(CARBOASD-AMAX1(.0,CARBOASD-GROLF-GROST-GROGRPA))
          GRORS = AMAX1(-RSWT,
     &     (CARBOAT-CARBOASD-GRORSSD)-GROLF-GROST-GROGRPA)
     

!               Reserves to ROOT if conc too great (overflow!)
              RTWTGRS = 0.0
!               Determine potential new concentration
!               NB. Chaff is simply a part of stem;hence not separate here

!----------------------------------------------------------------
! palderman commit of 2019-07-29 in private repo e5c680514bce4af85d9f463c11a3e5ad522252f8
!              IF (LFWT+GROLF+STWT+GROST.GT.0.0) TVR1 = ! Conc
!     &          (RSWT+GRORS-SENRS)/
!     &          ((LFWT+GROLF-SENLFG-SENLFGRS)
!     &          +(STWT+GROST)+(RSWT+GRORS))

              IF (((LFWT+GROLF-SENLFG-SENLFGRS) ! Prevent divide by zero
     &              +(STWT+GROST)+(RSWT+GRORS)) .GT. 0.0)THEN
                  TVR1 = ! Conc
     &              (RSWT+GRORS-SENRS)/
     &              ((LFWT+GROLF-SENLFG-SENLFGRS)
     &              +(STWT+GROST)+(RSWT+GRORS))
              ELSE
                  TVR1 = 0.0
              END IF
!----------------------------------------------------------------

              IF(TVR1.LT.0.0.AND.TVR1.GT.-1.0E-07) TVR1 = 0.0
              IF (TVR1.GT.RSPCX/100.0) THEN   ! If potential>max        
                TVR2 = RSWT+GRORS-SENRS       ! What rswt could be
                TVR3 =                        ! What rswt should be 
     &           ((RSPCX/100.0)
     &           *(LFWT+GROLF-SENLFG-SENLFGRS
     &           +STWT+GROST))
     &           /(1.0-(RSPCX/100.0))
                RTWTGRS = (TVR2 - TVR3) 
                ! Determine FINAL new concentration
                IF (LFWT+GROLF+STWT+GROST.GT.0.0) TVR5 = 
     &            (RSWT+GRORS-SENRS-RTWTGRS)/
     &            ((LFWT+GROLF-SENLFG-SENLFGRS)
     &            +(STWT+GROST)
     &            +(RSWT+GRORS-SENRS-RTWTGRS))
              ENDIF
     
          GRORSPM = CARBOAPM
          
          IF (PLAGTP(1)+PLAGTP(2).GT.0.0) THEN
            PLAGT(1) = GROLF*(1.0-LSHFR)*LAW*
     &       (PLAGTP(1)/(PLAGTP(1)+PLAGTP(2)))
            PLAGT(2) = GROLF*(1.0-LSHFR)*LAW*
     &       (PLAGTP(2)/(PLAGTP(1)+PLAGTP(2)))
          ENDIF
          DO L = 1,2
           IF(PLAGTP(1)+PLAGTP(2).GT.0.0)
     &       PLAG(L) = PLAG(L)*(PLAGT(1)+PLAGT(2))/(PLAGTP(1)+PLAGTP(2))
          ENDDO
          
          ! Growth adjusted for respiration
          RTWTG = AMAX1(0.0,(CARBOR+RTWTGRS)*(1.0-RRESP)) ! LAH 280211 
         
          ! Leaf senescence
          IF (LNUMSG.GT.0.AND.LNUMSG.GT.LLIFE) THEN
            PLASTMP = AMAX1(0.0,LAP(LNUMSG-LLIFE)*TT/PHINT)
            ! Senesces over 1 PHINT
            ! Senescence cannot be greater than area remaining for
            ! senescing leaf. May have been killed by cold,etc..
            LASENLF = AMAX1(0.0,LAP(LNUMSG-LLIFE)-LAPS(LNUMSG-LLIFE))
            PLASTMP = AMIN1(LASENLF,PLASTMP)
          ELSE
            PLASTMP = 0.0
          ENDIF
          
          PLAS = 0.0
          IF (ISTAGE.EQ.1) THEN
            PLAS = PLASTMP          ! Dependent on leaf longevity
          ELSEIF (ISTAGE.EQ.2) THEN
            PLAS = AMIN1(PLASTMP,   ! Dependent on input coefficient 
     &       PLASF(ISTAGE)*GPLA(ISTAGE-1)*DU/PD(ISTAGE))
          ELSEIF (ISTAGE.EQ.3.OR.ISTAGE.EQ.4.OR.ISTAGE.EQ.5) THEN
            ! Determine if N shortage triggers final senescence
            IF (ISWNIT.NE.'N'.AND.XSTAGE.GT.5.0.AND.LCNF.LT.NFSF) THEN
              XSTAGEFS = XSTAGE
              GPLASENF = AMAX1(0.0,PLA-SENLA)
            ENDIF
            ! Calculate leaf area senesced
            IF (XSTAGE.GT.5.0.AND.
     &        XSTAGEFS.GT.0.0.AND.XSTAGE.LT.LSENS) THEN
              PLAS = GPLASENF*(XSTAGE-XSTAGEFS)/(LSENE-XSTAGEFS)
            ELSE
              IF (XSTAGE.GT.LSENS) THEN
                IF (GPLASENS.LT.0.0) GPLASENS = AMAX1(0.0,PLA-SENLA)
                ! NB. Leaf senescence ends at stage LSENE (6.?)
                PLAS = GPLASENS*(XSTAGE-XSTAGEP)/(LSENE-LSENS)
              ELSE
                PLAS = PLASF(ISTAGE) * GPLA(ISTAGE-1)*DU/PD(ISTAGE)
              ENDIF
            ENDIF
          ELSEIF (ISTAGE.EQ.6) THEN 
            ! Originally senesced over 10 standard days after stage 6
            !PLAS = GPLA(ISTAGE-1)*TT/20.0*0.1
            ! Following is to use LSPHE
            PLAS = GPLASENS*(XSTAGE-XSTAGEP)/(LSENE-LSENS)
          ENDIF
          
          ! Increased senescence if reserves fall too low
          ! NB. 3% of green leaf area senesces ... must check
          IF (RSCD.LT.0.10 .AND. ISTAGE.GE.4. AND. PLA.GT.0.0) THEN
           PLAS = PLAS + AMAX1(0.0,0.03*(PLA-PLAS-SENLA))
!           WRITE(fnumwrk,'(A52,I4)')
!     &      ' Senescence accelerated because low reserves on day:',doy
          ENDIF
          
          ! Overall check to restrict senescence to what available
          PLAS = AMAX1(0.0,AMIN1(PLAS,PLA-SENLA))
          
          ! Tillering
          IF (GROLFP+GROSTP.GT.0.0) THEN
            TNUMAFAC = AMIN1(1.0,(GROLF+GROST)/(GROLFP+GROSTP))
          ELSE
            TNUMAFAC = 0.0
          ENDIF
          IF (LNUMSD.GE.TI1LF) THEN
            ! LAH SEPT 2009  Maybe introduce TILPE and make variable
            !TILPE = 2.0   ! Had been changed to 3.0
            IF (XSTAGE.LT.TILPE) THEN
              IF (LNUMSD.LT.ti1lf+3) THEN    ! Fibonacci factors
                tnumiff=1.0
              ELSEIF(LNUMSD.GE.ti1lf+3 .AND. LNUMSD.LT.ti1lf+4) THEN
                tnumiff=1.5
              ! PUT = 1.5 TO MATCH ADJUSTMENT IN CSCRP  
              ELSEIF(LNUMSD.GE.ti1lf+4 .AND. LNUMSD.LT.ti1lf+5) THEN
                tnumiff=1.5  ! 3.0
              ELSEIF(LNUMSD.GE.ti1lf+5 .AND. LNUMSD.LT.ti1lf+6) THEN
                tnumiff=1.5  ! 4.0
              ELSEIF(LNUMSD.GE.ti1lf+6 .AND. LNUMSD.LT.ti1lf+7) THEN
                tnumiff=1.5  ! 6.0
              ENDIF
              ! CHANGED BACK TO USING AMIN1 FUNCTION TO MATCH CSCRP
              TNUMG = TT/PHINT * TNUMIFF * AMIN1(WFT,NFT,LIF1)
              !TNUMG = TT/PHINT * TNUMIFF * WFT*NFT*LIF1
              ! TAKEN OUT TO MATCH CSCRP
              ! IF (LNUMSD.GT.TI1LF+3) TNUMG = TNUMG * TNUMAFAC
            ENDIF
          ELSE
            TNUMG = 0.0
          ENDIF
          
          ! TIFAC = Tillering rate factor (multiplier) 
          TNUMG = TNUMG * TIFAC
          
          ! Tiller death
          RTSW = 1.0
          TILWT = 0.0
          TILSW = G3 * CUMDU/PTH(5)
          IF (TNUM.GT.0.0) TILWT = (LFWT+STWT+RSWT+GRWT)/TNUM
          IF (TILSW.GT.0.0) RTSW = TILWT/TILSW
          !TILDS = 2.0     ! Ceres 3.5 = 2.4
          !TILDE = 4.0     ! Ceres 3.5 = 5.8
          IF (XSTAGE.GE.TILDS .AND. XSTAGE.LT.TILDE)
     &     TNUMD = 
     &       AMAX1(0.0,(TNUM-1.0)*(1.0-RTSW)*(TT/20.0)*(TILDF/100.0))
             ! 20.0 is to change TT to standard days          
          ! Root respiration
          RTRESP = (CARBOR+RTWTGRS) * RRESP
          
          XSTAGEP = XSTAGE

          ! Canopy height    
          IF (XSTAGE.LT.7) THEN
            IF (XSTAGE.GT.1.0) THEN
              CANHTG =
     &         AMAX1(0.0,(CANHTS*AMIN1(1.0,(XSTAGE-1.0)/4.0)-CANHT))
            ELSEIF (XSTAGE.EQ.1.0 .AND. PLAGT(1).GT.0.0) THEN
              ! Height growth on day of emergence or if no development
              CANHTG = 0.5
            ENDIF
          ENDIF
          
          ! Root growth initializations
          RTWTGS = 0.0
          DO L = 1, NLAYR
            RTWTGL(L) = 0.0
            RTWTSL(L) = 0.0
            RTNSL(L) = 0.0
          ENDDO
      
          ! Root growth calculations
          IF (ISTAGE.LT.6 .OR. ISTAGE.GE.8) THEN
         
            ! Establish root tip layer
            CUMDEP = 0.0
            L = 0
            DO WHILE ((RTDEP.GE.CUMDEP) .AND. (L.LT.NLAYR))
              L = L + 1
              CUMDEP = CUMDEP + DLAYR(L)
            END DO
          
            ! Water factor for root depth growth
            IF (ISWWAT.NE.'N') THEN
              WFRG = AMAX1(0.1,AMIN1(1.0,
     &         ((SW(L)-LL(L))/(DUL(L)-LL(L)))/WFRGU))
              IF (L.EQ.1) THEN
                ! Layer 2 SWDF used if less stress, because
                ! elongation most likely at base of layer
                TVR1 = AMAX1(0.1,AMIN1(1.0,
     &          ((SW(2)-LL(2))/(DUL(2)-LL(2)))/WFRGU))
                IF (TVR1.GT.WFRG) WFRG = TVR1
              ENDIF
            ELSE  
              WFRG = 1.0
            ENDIF
          
            ! Root depth growth
            RTDEPG = 0.0                                                
            IF (ISTAGE.LT.7) THEN                                      
              IF (RTWTG.GT.0.0) THEN                                   
                IF (CUMTT.LT.RDGTH) THEN
                  RTDEPG = (TT/STDAY)*RDGS1                           
                ELSE                                                   
                  RTDEPG = (TT/STDAY)*RDGS2                           
                ENDIF                     
                ! Add water content and hospitality factors 
                IF (ISWWAT.NE.'N') 
     &            RTDEPG = RTDEPG * SQRT(AMAX1(0.3,SHF(L))) * WFRG
              ELSE                                                     
                ! This is to handle planting on the surface and to     
                ! allow root depth growth immediately after emergence  
                IF (DAE.LT.20) RTDEPG = (TT/STDAY)*RDGS1               
              ENDIF                                                    
            ELSEIF (ISTAGE.GE.9) THEN      ! Germination to emergence  
              RTDEPG = (TT/STDAY)*RDGS1                                
            ENDIF                                                      
          
            ! Root dry matter from seed if not enough assimilate
            ! Root length/depth growth ratio (120) derived from Ceres3.5
            ! Used in pre-emergence stage to calculate depth growth 
            ! and then need to calculate total root growth
            IF (RTWTG.LT.RLDGR*RTDEPG/((RLWR*1.0E4)*PLTPOP)) THEN
              RTWTGS = AMAX1(0.0,AMIN1(SDAFR*SEEDRSAV,
     &        (RLDGR*RTDEPG/((RLWR*1.0E4)*PLTPOP) - RTWTG)))
              IF (RTWTGS < 1.E-10) RTWTGS = 0.0
              SEEDRSAV = SEEDRSAV - RTWTGS
            ENDIF
                                                                       
            ! Root dry matter distribution with depth
            L = 0                                                      
            CUMDEP = 0.0
            RTDEPTMP = RTDEP+RTDEPG
            DO WHILE ((CUMDEP.LE.RTDEPTMP) .AND. (L.LT.NLAYR))
              L = L + 1
              CUMDEP = CUMDEP + DLAYR(L)
              SWDF = AMAX1(0.1,AMIN1(1.0,
     &         ((SW(L)-LL(L))/(DUL(L)-LL(L)))/WFRG))
              ! Original CERES
              !F (ISWNIT.EQ.'Y') THEN
              !  RNFAC = AMAX1(0.01,
              !       (1.0-(1.17*EXP(NFRG*(NO3LEFT(L)+NH4LEFT(L))))))
              !ELSE
              !  RNFAC = 1.0
              !ENDIF
              ! Below from Cropsim
                IF (ISWNIT.NE.'N'.AND.NCRG.GT.0.0) THEN
                  NFRG = AMIN1(1.0,
     &             AMAX1(0.1,(NO3LEFT(L)+NH4LEFT(L))/NCRG))
                ELSE
                  NFRG = 1.0
                ENDIF 
              ! LAH HAWAII 2007   N factor taken out.
              ! LAH GUELPH 2010   Replaced with Cropsim function.
              ! RLDF(L) = AMIN1(SWDF,RNFAC)*SHF(L)*DLAYR(L)
              ! RLDF(L) = SWDF*SHF(L)*DLAYR(L)
              ! Below from CROPSIM
              RLDF(L) = AMIN1(WFRG,NFRG)*SHF(L)*DLAYR(L)
            END DO
            
            ! Distribution factor adjusted for root tip layer
            IF (L.GT.0) RLDF(L) = 
     &        RLDF(L)*(1.0-(CUMDEP-RTDEPTMP)/DLAYR(L)) 
            L1 = L
            
            TRLDF = 0.0
            DO  L = 1, L1
              TRLDF = TRLDF + RLDF(L)
            END DO
            RTWTGL = 0.0 ! LAH Feb 28 2011
            IF (TRLDF.GT.1.E-8 .AND. (RTWTG+RTWTGS)> 1.E-10) THEN
              DO  L = 1, L1
                RTWTGL(L) = (RLDF(L)/TRLDF)*(RTWTG+RTWTGS)
              END DO
            ENDIF
                   
            ! Root senescence   
            RTWTS = 0.0
            RTWTSL = 0.0 
            RTNSL = 0.0 
            DO L = 1, L1
              RTWTSL(L) = RTWTL(L)*RSEN/100.0*TT/STDAY ! NB. Temp effect
              RTWTS = RTWTS + RTWTL(L)*RSEN/100.0*TT/STDAY
              RTNSL(L) = AMIN1(RTWTSL(L)*RMNC,RTWTSL(L)*RANC)
            ENDDO

          ENDIF
          
          ! Devernalization
          VDLOST = 0.0                                                  
          IF (CUMVD.LT.10.0 .AND. TMAX.GT.30.0) THEN                    
            VDLOST = 0.5*(TMAX-30.0)
          ENDIF
          
          ! Loss of cold hardiness
          IF (ISTAGE.GE.9 .OR. ISTAGE.LT.7) THEN
            IF (ISTAGE.GE.2) THEN
              HARDILOS = AMAX1(0.0,(TMAX-10.0)*0.1)
            ELSE
              HARDILOS = AMAX1(0.0,(TMAX-10.0)*0.01)
            ENDIF
          ENDIF
          
          IF (ISTAGE.LE.7.0) THEN
            TNUMLOSS = 0.0
            PLTLOSS = 0.0
            PLASC = 0.0
            CKCOLD = 0.0
            ! Leaf senescence
            IF ((TMIN+TMAX)/2.0.LT.TKLF) THEN
              CKCOLD = (TKLF-(TMIN+TMAX)/2.0)*0.20
              ! 20% loss for every degree below Tklf
            ENDIF
            ! Following is original (as CK)
            ! CKCOLD = AMAX1(0.00,AMIN1(0.96,
            ! (0.020*HARDI-0.10)*(TMIN*0.85+TMAX*0.15+10.0+.25*SNOW)))
            PLASC = AMAX1(0.0,
     &            AMIN1(CKCOLD*(PLA-SENLA),((PLA-SENLA)-TNUM*0.035)))
!            IF (PLASC.GT.0.0.AND.(PLA-SENLA).GT.0.0) 
!     &       WRITE(fnumwrk,'(A30,I4,A14,F4.1)')
!     &       ' Leaves damaged by cold on day',doy,
!     &       ' Fraction lost',plasc/(pla-senla)
          
            ! Tiller and plant death
            IF (TKILL.GT.(TMIN+TMAX)/2.0) THEN
              IF (TNUM.GE.1.0) THEN
                TNUMLOSS=TNUM *
     &           (1.0-(0.9-0.02*ABS(((TMIN+TMAX)/2.0-TKILL))**2))
              ENDIF
              IF (TNUM-TNUMLOSS.GE.1.0) THEN
!                WRITE (FNUMWRK,900)
!     &           DOY,TKILL,(TMIN+TMAX)/2.0,HARDI,TNUM,PLTPOP
 900            FORMAT (' Crop was damaged by cold on day',I4,/,
     &            ' TKILL =',F5.1,5X,'TMEAN=',F5.1,5X,
     &            'HARDI=',F5.2,5X,'TNUM =',  F7.2,5X,'PLTPOP=',F4.0)
              ELSE
                PLTLOSS =
     &           PLTPOP*(1.0-(0.95-0.02*((TMIN+TMAX)/2.0-TKILL)**2))
                 IF (ISTAGE.GE.4) PLTLOSS = 0.0
                IF (PLTPOP-PLTLOSS.GE.0.05*PLTPOPP) THEN
!                  WRITE (FNUMWRK,900) DOY,TKILL,
!     &             (TMIN+TMAX)/2.0,HARDI,TNUM,PLTPOP
                ELSE
                  CFLFAIL = 'Y'
                  PLTLOSS = AMIN1(PLTPOP,PLTLOSS)
                  IF (ISTAGE.GE.4) PLTLOSS = 0.0
!                  WRITE (FNUMWRK,1100) DOY,TKILL,(TMIN+TMAX)/2.0,HARDI
! 1100             FORMAT (' At least 95% killed by cold on day',I4,/,
!     &            ' TKILL =',F5.1,5X,'TMEAN =',F5.1,5X,
!     &            'HARDII =',F5.2)
                ENDIF
              ENDIF
            ENDIF
          ENDIF

          ! Senescent leaf - NB. N loss has a big effect if low N
          LSENNF = 0.0
          IF (LANC.GT.0.0) LSENNF = (1.0-((LANC-LMNC)/LANC))
          PLASS = 0.0
          PLAST = 0.0
          SENLFG = 0.0
          SENLFGRS = 0.0
          SENRS = 0.0
          SENNLFG = 0.0
          SENNLFGRS = 0.0
          ! Following is to lose some leaf area when tillers die
          IF (TNUMD.GT.0.0) PLAST = TNUMD * ((PLA-SENLA)/TNUM) * LALOSSF
          IF (ISWWAT.NE.'N') THEN
            IF (PLA-SENLA.GT.0.0.AND.WUPR.LT.WFSU) THEN
              PLASS = AMAX1(0.0,AMIN1((PLA-SENLA)-PLAS,
     &            (PLA-SENLA)*LLOSW))
            ENDIF
          ENDIF
          IF (ISWNIT.NE.'N') THEN
            ! Low N accelerated senescence
            IF (PLA-SENLA.GT.0.0.AND.LANC.LT.LCNCSEN) THEN
              PLASS = PLASS +  AMAX1(0.0,AMIN1((PLA-SENLA)-PLAS,
     &            (PLA-SENLA)*LLOSN))
            ENDIF
          ENDIF
          IF (PLA-SENLA.GT.0.0) THEN
            IF (PLASC.GT.0.0) THEN
              ! If cold kill
              SENLFG = AMIN1(LFWT,LFWT*PLASC/(PLA-SENLA))
              SENRS = AMIN1(RSWT,RSWT*PLASC/(PLA-SENLA))
            ELSE
              ! If normal senescence
              SENLFG = AMIN1(LFWT*LWLOS,(AMAX1(0.0,         
     &         (LFWT*((PLAS+PLASS)/(PLA-SENLA))*LWLOS))))
              SENLFGRS = AMIN1(LFWT*(1.0-LWLOS),(AMAX1(0.0,         
     &        (LFWT*((PLAS+PLASS)/(PLA-SENLA))*(1.0-LWLOS)))))
            ENDIF
            SENNLFG = AMIN1((LEAFN-GRAINNGL)*LSENNF,
     &                (SENLFG+SENLFGRS)*LANC*LSENNF)
            SENNLFGRS = AMIN1((LEAFN-GRAINNGL)*(1.0-LSENNF),
     &                  (SENLFG+SENLFGRS)*LANC*(1.0-LSENNF))
            IF (((SENNLFG+SENNLFGRS)-LEAFN).GT.1.0E-8) THEN
!              WRITE(fnumwrk,'(A40,F6.2)')
!     &         ' Adjusted N removal from leaves at stage',xstage
              SENNLFGRS = LEAFN-SENNLFG
              IF (SENNLFGRS.LT.0.0) THEN
                SENNLFG = SENNLFG - ABS(SENNLFGRS)
                SENNLFGRS = 0.0
                IF (SENNLFG.LT.0.0) SENNLFG = 0.0
              ENDIF
            ENDIF
          ENDIF

          ! Senescent stem
          SENSTG = 0.0
          SENNSTG = 0.0
          SENNSTGRS = 0.0
          SSENF = 0.0
          IF (XSTAGE.GT.SSSTG .AND. XSTAGE.LT.6.0) THEN
            SENSTG = AMAX1(0.0,STWT*(SSEN/100.0)*(TT/STDAY))
            IF (SANC.GT.0.0) SSENF = (1.0-((SANC-SMNC)/SANC))
            SENNSTG = SENSTG*SANC*SSENF
            SENNSTGRS = SENSTG*SANC*(1.0-SSENF)
            IF (SENNSTG+SENNSTGRS.GT.STEMN) THEN
!              WRITE(fnumwrk,*)'N removal from stem > stem N'
              SENNSTG = STEMN-SENNSTGRS
            ENDIF
          ENDIF

          ! N Uptake
          IF (ISWNIT.EQ.'Y') THEN                                       
      
            ! N uptake initializations                        
            DTOPN = 0.0
            DROOTN = 0.0
            DSTEMN = 0.0
            DLEAFN = 0.0
            DSTOVN = 0.0
            GRAINNG = 0.0
            NUPD = 0.0
            NUAG = 0.0
            NUF = 0.0
            SEEDNR = 0.0
            SEEDNRA = 0.0
            SEEDNRB = 0.0
            SEEDNT = 0.0
            SEEDNTA = 0.0
            SEEDNTB = 0.0
            RSNUSET = 0.0
            RSNUSER = 0.0
            RSNV = 0.0
            ANDEM = 0.0
            NDEMSOIL = 0.0
            RNDEM = 0.0
            RNDEMSOIL = 0.0
            RNDEM1 = 0.0
            TNDEM = 0.0
            TNDEMSOIL = 0.0
            TRLV = 0.0
            DROOTNA = 0.0
            GRAINNGR = 0.0
            GRAINNGV = 0.0
            NPOOLL = 0.0
            NPOOLST = 0.0
            NPOOLV = 0.0
              
            SNO3PROFILE = 0.0
            SNH4PROFILE = 0.0
            SNO3ROOTZONE = 0.0
            SNH4ROOTZONE = 0.0
            DO L = 1, NLAYR
              UNO3ALG(L) = 0.0
              UNH4ALG(L) = 0.0
              RNO3U(L) = 0.0
              RNH4U(L) = 0.0
              TRLV = TRLV + RLV(L)
              FAC(L) = 10.0/(BD(L)*DLAYR(L))
              SNO3(L) = NO3LEFT(L) / FAC(L)
              SNH4(L) = NH4LEFT(L) / FAC(L)
              SNO3PROFILE = SNO3PROFILE + SNO3(L)
              SNH4PROFILE = SNH4PROFILE + SNH4(L)
              IF (RLV(L).GT.0.0) THEN
                SNO3ROOTZONE = SNO3ROOTZONE + SNO3(L)
                SNH4ROOTZONE = SNH4ROOTZONE + SNH4(L)
              ENDIF
            END DO
          
            ! Grain N uptake.
            NSINK = 0.0
            IF (GRNUM.GT.0.0 .AND. XSTAGE.LT.6.0) THEN
              NSINK = AMIN1(GROGRPA*(GRNMX/100.0),
     &         LAGSTAGE*TFGN*GRNUM*G2*DU*.001*(GRNS/100.))
            ENDIF
          
            ! N uptake factor after maturity
            IF (XSTAGE.GT.6.0 .AND. XSTAGE.LT.7.0) THEN
              NUFACM = AMAX1(0.0,1.0 - (XSTAGE-6.0)/(6.5-6.0))
            ELSE
              NUFACM = 1.0
            ENDIF
            
            LNDEM = NUFACM *
     &              ((LFWT-SENLFG-SENLFGRS)*AMAX1(0.0,NTUPF*(LCNC-LANC))
     &              + GROLF*LCNC)
            SNDEM = NUFACM *
     &              ((STWT-SENSTG)*AMAX1(0.0,NTUPF*(SCNC-SANC))
     &              + AMAX1(0.0,GROST)*SCNC)
            RNDEM = NUFACM *
     &              ((RTWT-RTWTS) * AMAX1(0.0,NTUPF*(RCNC-RANC))
     &              + (RTWTG+RTWTGS) * RCNC)
     
            ! Reserve N use
            RSNUSEG = 0.0
            RSNUSET = 0.0
            RSNUSER = 0.0
            IF (NSINK.GT.0.0) RSNUSEG = AMAX1(0.0,AMIN1(NSINK,RSN))
            RSNV = RSN - RSNUSEG
            IF (LNDEM+SNDEM+RNDEM.GT.0.0 .AND. RSNV.GT.0.0) THEN
              IF (LNDEM+SNDEM+RNDEM.LT.RSNV) THEN
                RSNUSET = LNDEM + SNDEM
                RSNUSER = RNDEM
                TNDEM = 0.0
                RNDEM = 0.0
              ELSE
                RSNUSET = RSNV * (LNDEM+SNDEM)/(LNDEM+SNDEM+RNDEM)
                RSNUSER = RSNV * RNDEM/(LNDEM+SNDEM+RNDEM)
                TNDEM = (LNDEM+SNDEM) - RSNUSET
                RNDEM = RNDEM - RSNUSER
              ENDIF
              IF (RSNUSET.LT.1E-20) RSNUSET = 0.0
              IF (RSNUSER.LT.1E-20) RSNUSER = 0.0
            ELSE
              TNDEM = LNDEM + SNDEM
              RNDEM = RNDEM
            ENDIF
          
            ! Seed N use (Basic)
            ! Need a little seed use regardless of deficit after uptake
            SEEDNTB = 0.0
            SEEDNRB = 0.0
            
            IF (XSTAGE.LT.4 .OR. XSTAGE.GT.6) THEN
              ! Originaal: 
              !IF (SEEDN.GT.1.0E-6) THEN
              !  SEEDNTB = AMIN1(TNDEM,SEEDN*0.020)  
              !  SEEDNRB = AMIN1(RNDEM,AMAX1(0.0,SEEDN*0.025-SEEDNTB))
              !ENDIF
              ! To match Cropsim  
              IF (RTWT.LE.0.0) THEN
                SEEDNTB = AMIN1(TNDEM,SEEDN*0.2)  
                SEEDNRB = AMIN1(RNDEM,SEEDN*0.2)
              ELSE
                ! Some use of seed (0.2 need) even if may not be needed
                SEEDNTB = AMAX1(0.0,AMIN1(0.2*TNDEM,SEEDN*0.2))
                SEEDNRB = AMAX1(0.0,AMIN1(0.2*RNDEM,SEEDN*0.2))
              ENDIF
            ENDIF
            IF ((NSINK-RSNUSEG).GT.0.0) THEN    
              TNDEMSOIL = TNDEM - SEEDNTB + (NSINK-RSNUSEG)
            ELSE
              TNDEMSOIL = TNDEM - SEEDNTB
            ENDIF  
            RNDEMSOIL = RNDEM - SEEDNRB

            NDEMSOIL = TNDEMSOIL + RNDEMSOIL
            ANDEM = NDEMSOIL * PLTPOP*10.0
            TRNU = 0.0
            RNO3U(L) = 0.0
            RNH4U(L) = 0.0
            ! Calculate potential N uptake in soil layers with roots
            DO L=1,NLAYR
              IF (RLV(L) .GT. 1.E-6) THEN
                FNH4 = 1.0 - EXP(-0.08 * NH4LEFT(L))
                FNO3 = 1.0 - EXP(-0.08 * NO3LEFT(L))
                ! The following limits are those in NUPTAK
                IF (FNO3 .LT. 0.04) FNO3 = 0.0  
                IF (FNO3 .GT. 1.0)  FNO3 = 1.0
                IF (FNH4 .LT. 0.04) FNH4 = 0.0  
                IF (FNH4 .GT. 1.0)  FNH4 = 1.0
                SMDFR = (SW(L) - LL(L)) / (DUL(L) - LL(L))
                IF (SMDFR .LT. 0.0) SMDFR = 0.0
                IF (SW(L) .GT. DUL(L)) THEN
                  SMDFR = 1.0 - (SW(L) - DUL(L)) / (SAT(L) - DUL(L))
                  ! Wet soil effect not implemented
                  SMDFR = 1.0
                ENDIF
                RFAC = RLV(L) * SMDFR * SMDFR * DLAYR(L) * 100.0
!                  RLV = Rootlength density (cm/cm3);SMDFR = relative drought factor
!                  RTNO3 + RTNH4 = Nitrogen uptake / root length (mg N/cm)  
!                  RNO3U + RNH4  = Nitrogen uptake (kg N/ha)
                RNO3U(L) = RFAC * FNO3 * RTNO3
                RNH4U(L) = RFAC * FNH4 * RTNH4
                RNO3U(L) = MAX(0.0,RNO3U(L))
                RNH4U(L) = MAX(0.0,RNH4U(L))
                TRNU = TRNU + RNO3U(L) + RNH4U(L) !kg[N]/ha
              ENDIF
            ENDDO
 
            ! Ratio (NUPR) to indicate N supply for output
            IF (ANDEM.GT.0) THEN
              NUPR = TRNU/ANDEM
            ELSE
              IF (TRNU.LE.0) THEN
                NUPR = 0.0
              ELSE
                NUPR = 10.0
              ENDIF
            ENDIF
            ! Factor (NUF) to reduce N uptake to level of demand
            IF (TRNU.GT.0.0) NUF = AMIN1(1.0,ANDEM/TRNU)
            ! Actual N uptake by layer roots based on demand (kg/ha)
            DO L = 1, MIN(L1,NLAYR)
              UNO3ALG(L) = RNO3U(L)*NUF
              XMIN = NO3MN/FAC(L) ! Original XMIN = 0.25/FAC(L)
              UNO3ALG(L) = MAX(0.0,MIN (UNO3ALG(L),SNO3(L) - XMIN))
              UNH4ALG(L) = RNH4U(L)*NUF
              IF (FAC(L).GT.0.0) THEN
                XMIN = NH4MN/FAC(L) ! Original XMIN = 0.5/FAC(L)
              ELSE
                XMIN = 0.0
              ENDIF  
              UNH4ALG(L) = MAX(0.0,MIN (UNH4ALG(L),SNH4(L) - XMIN))
              NUAG = NUAG + UNO3ALG(L) + UNH4ALG(L)
            END DO
          
            NUPD = NUAG/(PLTPOP*10.0)
          
            ! Change in above and below ground N
            IF (NDEMSOIL.LE.0.0 .OR. NUPD.LE.0.0) THEN
              DTOPN = 0.0 + RSNUSET
              DROOTN = 0.0 + RSNUSER
            ELSE
              DTOPN = (TNDEMSOIL/NDEMSOIL)*NUPD + RSNUSET
              DROOTN = (RNDEMSOIL/NDEMSOIL)*NUPD + RSNUSER
            ENDIF
          
            ! Make sure that roots do not fall below minimum
            IF ((RTWTG+RTWTGS)*RMNC.GT.DROOTN) THEN
              DROOTNA = AMIN1(DTOPN,((RTWTG+RTWTGS)*RMNC)-DROOTN)
              DROOTN = DROOTN + DROOTNA
              DTOPN = DTOPN - DROOTNA
            ENDIF
          
            ! Use N allotted to tops and roots for grain if filling     
            IF ((NSINK-RSNUSEG).GT.0.0) THEN
              IF ((NSINK-RSNUSEG).GE.DTOPN+DROOTN) THEN
                GRAINNG = DTOPN + DROOTN
                DSTOVN = 0.0
                DROOTN = 0.0
              ELSE
                GRAINNG = (NSINK-RSNUSEG)
                TVR1 = DTOPN+DROOTN-(NSINK-RSNUSEG)
                IF (TNDEM+RNDEM > 1.E-6) THEN
                DSTOVN = TNDEM/(TNDEM+RNDEM) * TVR1
                DROOTN = RNDEM/(TNDEM+RNDEM) * TVR1
                ELSE
                  DSTOVN = 0.0
                  DROOTN = 0.0
                ENDIF
              ENDIF
            ELSE
              DSTOVN = DTOPN
            ENDIF
          
            ! Use additional seed if not sufficient so far              
            IF (XSTAGE.LT.4 .OR. XSTAGE.GT.6) THEN
              SEEDNTA = AMAX1(0.0,
     &         AMIN1(SEEDN-SEEDNTB-SEEDNRB,TNDEMSOIL-DSTOVN))
              SEEDNRA = AMAX1 (0.0,
     &         AMIN1(SEEDN-SEEDNTB-SEEDNTA-SEEDNRB,RNDEMSOIL-DROOTN))
              SEEDNT = SEEDNTB + SEEDNTA
              SEEDNR = SEEDNRB + SEEDNRA
            ENDIF
          
            ! Move N to grain from roots and tops if necessary
            IF((NSINK-RSNUSEG).GT.0..AND.(NSINK-RSNUSEG).GT.GRAINNG)THEN
              NPOOLR = AMAX1 (0.0,(XNFS/100.0)*(RTWT*(RANC-RMNC)))
              IF (NPOOLR.GT.(NSINK-RSNUSEG)-GRAINNG) THEN
                GRAINNGR = (NSINK-RSNUSEG)-GRAINNG
              ELSE
                GRAINNGR = NPOOLR
              ENDIF
              IF ((NSINK-RSNUSEG).GT.GRAINNG+GRAINNGR) THEN
                ! N use limit 
                IF (XSTAGE.GE.5.0.AND.XSTAGE.LE.6.0) THEN
                  NUSELIM = AMIN1(1.0,DU/((6.0-XSTAGE)*PD(5)))
                ELSEIF (XSTAGE.GE.4.0.AND.XSTAGE.LE.5.0) THEN
                  NUSELIM = DU/(((5.0-XSTAGE)*PD(4))+PD(5))
                ENDIF
                NUSEFAC = AMAX1(NUSELIM,(XNFS/100.0))
                NPOOLL = AMAX1 (0.0,
     &           NUSEFAC*((LFWT-SENLFG-SENLFGRS)*(LANC-LMNC)))
                NPOOLST = AMAX1 (0.0,
     &           NUSEFAC*((STWT-SENSTG)*(SANC-SMNC)))
                NPOOLV = NPOOLL + NPOOLST
                IF (NPOOLV.GT.(NSINK-RSNUSEG)-(GRAINNG+GRAINNGR)) THEN
                  GRAINNGV = (NSINK-RSNUSEG)-GRAINNG-GRAINNGR
                ELSE
                  GRAINNGV = NPOOLV
                ENDIF
              ENDIF
            ENDIF
          
            ! Split tops aspects into leaf and stem aspects
            IF (LNDEM+SNDEM.GT.0.0) THEN
              DLEAFN = DSTOVN * LNDEM / (LNDEM+SNDEM)
              DSTEMN = DSTOVN * SNDEM / (LNDEM+SNDEM)
            ELSE
              DLEAFN = 0.0
              DSTEMN = 0.0
            ENDIF
            IF (NPOOLL+NPOOLST.GT.0.0) THEN
              GRAINNGL = GRAINNGV * NPOOLL / (NPOOLL+NPOOLST)
              GRAINNGS = GRAINNGV * NPOOLST / (NPOOLL+NPOOLST)
            ELSE
              GRAINNGL = 0.0
              GRAINNGS = 0.0
            ENDIF
          
          ENDIF

          ! Minimum grain N control
          GRORSGR = 0.0
          IF (ISWNIT.NE.'N'.AND.GRNMN.GT.0.0) THEN
            GROGRPN =
     &       (GRAINNG+GRAINNGR+GRAINNGL+GRAINNGS+RSNUSEG)*(100./GRNMN)
            IF (GROGRPN.LT.GROGRPA.AND.GRAINANC*100.0.LE.GRNMN) THEN
              GRORSGR = GROGRPA - GROGRPN
              GROGR = GROGRPN
            ELSE
              GROGR = GROGRPA
              GRWTTMP = GRWT + GROGRPA
              IF (GRWTTMP.GT.0.0) THEN
                GRAINNTMP = GRAINN +
     &          (GRAINNG+GRAINNGR+GRAINNGL+GRAINNGS+RSNUSEG)
                IF (GRAINNTMP/GRWTTMP*100.0 .LT. GRNMN) THEN
                  GRWTTMP = GRAINNTMP*(100.0/GRNMN)
                  GROGR = GRWTTMP - GRWT
                  IF (GROGR.LT.GROGRPA) GRORSGR = GROGRPA - GROGR
                ENDIF
              ENDIF
            ENDIF
            IF (GROGR.LT.GROGRPA) THEN
!              WRITE(fnumwrk,'(A42,F4.2)')
!     &         ' N limit on grain growth. N at minimum of ',grnmn
              NLIMIT = NLIMIT + 1
            ENDIF
          ELSE
            GROGR = GROGRPA
          ENDIF

          ! Maximum grain weight control
          GROGRADJ = 0.0
          IF (GRNUM.GT.0.0) THEN
            IF ((GRWT+GROGR)/GRNUM - G2KWT/1000.0 > 1.E-5) THEN
!              WRITE(fnumwrk,*)'Maximum kernel wt reached on:',YEARDOY
              GROGRADJ = GROGR - (G2KWT/1000.0-(GRWT/GRNUM))*GRNUM
            ENDIF
          ENDIF

          ! Growth variables expressed on area basis
          ! C assimilation
          CARBOA = CARBO*PLTPOP*10.0
          ! N assimilation
          !NUAG = NUPD*PLTPOP*10.0
          ! Above ground senescence
          SENWALG(0) = 0.0
          SENNALG(0) = 0.0
          SENCALG(0) = 0.0
          SENLGALG(0) = 0.0

        ENDIF

      END SUBROUTINE CER_Growth
