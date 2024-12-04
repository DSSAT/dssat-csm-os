!***********************************************************************
! This is the code from the section (DYNAMIC == RATE) 
! lines 5252 - 7645 of the original CSCRP code.
!***********************************************************************
 
      SUBROUTINE CRP_Growth (BD, CLOUDS, CO2, DAYLT,
     &    DLAYR, DOY, DUL, EO, EOP, ES, ISWDIS, ISWNIT , ISWWAT,
     &    KCAN, KEP, LL, NFP, NH4LEFT, NLAYR , NO3LEFT, PARIP,
     &    PARIPA, RLV, RNMODE, SAT , SENCALG, SENLALG, SENNALG,
     &    SHF, SLPF, SNOW, SRAD, ST, STGYEARDOY, SW, TDEW,
     &    TMAX, TMIN, TRWUP, UH2O, UNH4, UNO3, 
     &    WINDSP, YEAR, YEARPLTCSM, LAI,
     &    IDETG)

!     2023-01-20 chp removed unused variables from argument list
!     ALBEDOS, TAIRHR, WEATHER, SOILPROP, CONTROL, 

      USE ModuleDefs
      USE CRP_First_Trans_m
  
      IMPLICIT NONE
      EXTERNAL WARNING, TFAC4, YVALXY, CSUCASE, EVAPO, CSCRPROOTWU, 
     &  CSIDLAYR, CSVPSAT, CSYVAL
      
!     TYPE (ControlType), intent (in) :: CONTROL ! Defined in ModuleDefs
!     TYPE (WeatherType), intent (in) :: WEATHER ! Defined in ModuleDefs
!     TYPE (SoilType), intent (in) ::   SOILPROP ! Defined in ModuleDefs
  
      INTEGER DOY, NLAYR, STGYEARDOY(20), YEAR, YEARPLTCSM          
      INTEGER CSIDLAYR                 
      REAL BD(NL), CLOUDS, CO2, DLAYR(NL)  !ALBEDOS, GSTAGE
      REAL DUL(NL), EO, EOP, ES, KCAN, kep, LL(NL), NFP, NH4LEFT(NL)
      REAL NO3LEFT(NL), PARIP, PARIPA, RLV(NL), SAT(NL)
      REAL SENCALG(0:NL), SENLALG(0:NL), SENNALG(0:NL), SHF(NL), SLPF
      REAL SRAD, ST(0:NL), SW(NL), TDEW, TMAX, TMIN, TRWUP  !TAIRHR(24), 
      REAL UH2O(NL), UNH4(NL), UNO3(NL), WINDSP, LAI
      REAL DAYLT, RWUMX, RWUPM, SNOW
      
      REAL CSVPSAT, CSYVAL, TFAC4             ! Real function calls 
      REAL YVALXY
      CHARACTER(LEN=1) IDETG, ISWDIS, ISWNIT, ISWWAT, RNMODE  
      
        IF (YEARDOY.GE.PLYEARDOY) THEN
          DAP = DAP + 1
          IF (EYEARDOY.GT.0) DAE = DAE + 1
        ELSE
          DAP = 0
        ENDIF

!-----------------------------------------------------------------------
!       Set 'establishment' switches
!-----------------------------------------------------------------------

        ! EARLY? are parameters that alllow for water and N stresses 
        ! to be switched off early in the life cycle. If they are set
        ! to -1.0, nothing is switched off. (NB. Not 0 because this 
        ! results in the stresses being switched off on the emergence
        ! day.)  
        ! NB. When N stress is switched off, the accumulation of N in 
        ! the plant still proceeds as it would otherwise have done,  
        ! and N stress may be more severe than it otherwise would 
        ! have been once N stress is switched back on.
        EARLYN = -1.0
        EARLYW = -1.0
        IF (LNUM.LE.EARLYN) THEN
          ISWNITEARLY = 'N'
        ELSE 
          ISWNITEARLY = 'Y'
        ENDIF  
        IF (LNUM.LE.EARLYW) THEN
          ISWWATEARLY = 'N'
        ELSE 
          ISWWATEARLY = 'Y'
        ENDIF  

!-----------------------------------------------------------------------
!       Set date and environmental equivalents
!-----------------------------------------------------------------------

        YEARDOY = YEAR*1000 + DOY
        TMEAN = (TMAX+TMIN)/2.0
        IF (SNOW.LE.0.0) THEN
          TMEANSURF = TMEAN
        ELSE
          TMEANSURF = 0.0
        ENDIF
        CO2AIR = 1.0E12*CO2*1.0E-6*44.0 /       ! CO2 in g/m3
     &   (8.314*1.0E7*((TMAX+TMIN)*0.5+273.0))

!-----------------------------------------------------------------------
!       Determine if today is planting day
!-----------------------------------------------------------------------

        ! YEARPLTCSM established by CSM and brought across in argument.
        !IF (FILEIOT.EQ.'DS4'.AND.RNMODE.EQ.'Q') THEN
        !  PLYEARDOYT = YEARPLTCSM
        !ENDIF
        IF (FILEIOT.EQ.'DS4') THEN
!         IF (IPLTI.EQ.'A' .OR. (INDEX('FQN',RNMODE) > 0)) THEN
          IF (IPLTI.EQ.'A' .OR. IPLTI.EQ.'F' .OR. 
     &       (INDEX('FQNY',RNMODE) > 0)) THEN
            PLYEARDOYT = YEARPLTCSM
          ENDIF  
        ENDIF

        IF (PLYEARDOY.GT.9000000) THEN            ! If before planting
          IF(PLYEARDOYT.GT.0 .AND. PLYEARDOYT.LT.9000000)THEN
            ! Specified planting date
            IF(YEARDOY.EQ.PLYEARDOYT) THEN
              PLYEARDOY = YEARDOY
              PLYEAR = YEAR
            ENDIF
          ELSE
            IF (FILEIOT.NE.'DS4') THEN
!               Automatic planting
!               Check window for automatic planting,PWDINF<PLYEART<PWDINL
              IF (YEARDOY.GE.PWDINF.AND.YEARDOY.LE.PWDINL) THEN
                ! Within planting window.
                ! Determine if soil temperature and soil moisture ok
                ! Obtain soil temperature, TSDEP, at 10 cm depth
                I = 1
                TSDEP = 0.0
                XDEP = 0.0
                DO WHILE (XDEP .LT. 10.0)
                  XDEP = XDEP + DLAYR(I)
                  TSDEP = ST(I)
                  I = I + 1
                END DO
                ! Compute average soil moisture as percent, AVGSW
                I = 1
                AVGSW = 0.0
                CUMSW = 0.0
                XDEP = 0.0
                DO WHILE (XDEP .LT. SWPLTD)
                  XDEPL = XDEP
                  XDEP = XDEP + DLAYR(I)
                  IF (DLAYR(I) .LE. 0.) THEN
                    !If soil depth is less than SWPLTD
                    XDEP = SWPLTD
                    CYCLE
                  ENDIF
                  DTRY = MIN(DLAYR(I),SWPLTD - XDEPL)
                  CUMSW = CUMSW + DTRY *
     &             (MAX(SW(I) - LL(I),0.0)) / (DUL(I) - LL(I))
                  I = I + 1
                END DO
                AVGSW = (CUMSW / SWPLTD) * 100.0
                IF (TSDEP .GE. PTTN .AND. TSDEP .LE. PTX) THEN
                  IF (AVGSW .GE. SWPLTL .AND. AVGSW .LE. SWPLTH) THEN
                    PLYEARDOY = YEARDOY
                    PLYEAR = YEAR
                  ENDIF
                ENDIF
              ELSE
                IF (YEARDOY.GT.PWDINL) THEN
                  CFLFAIL = 'Y'
                  STGYEARDOY(12) = YEARDOY  ! Failure
                  STGYEARDOY(11) = YEARDOY  ! End Crop
                  Message(1) = 'Automatic planting failure '
                  CALL WARNING(1,'CSCRP',MESSAGE)
!                  Write(Fnumwrk,*)' '
!                  Write(Fnumwrk,*)' Automatic planting failure '
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF

        IF (YEARDOY.EQ.PLYEARDOY) THEN
          ! Initial soil N and H2O
          SOILNI = 0.0
          ISOILH2O = 0.0
          DO I = 1, NLAYR
            SOILNI = SOILNI + NO3LEFT(I)/(10.0/(BD(I)*(DLAYR(I))))
     &                      + NH4LEFT(I)/(10.0/(BD(I)*(DLAYR(I))))
            ISOILH2O = ISOILH2O + SW(I)*DLAYR(I)
          ENDDO
          ! Plant population as established; if no data,as planted
          IF (PLTPOPE.GT.0) THEN
            PLTPOP = PLTPOPE
          ELSE
            PLTPOP = PLTPOPP
          ENDIF  
          ! Tiller # set equal to plants per hill
          IF (PLPH.GT.0.0) THEN
            TNUM = PLPH
            TNUML(1) = TNUM
          ELSE
            TNUM = 1.0
          ENDIF
        ENDIF

!=======================================================================
        IF (YEARDOY.GT.PLYEARDOY) THEN ! If planted (assumed in evening)
!=======================================================================

          IF (PLYEAR.LE.0) PLYEAR = YEAR

!-----------------------------------------------------------------------
!         Calculate potential plant evaporation,water uptake if neeeded
!-----------------------------------------------------------------------

          ! EO is brought into the module. The following calculations
          ! (apart from the root water uptake module) are for 
          ! comparative purposes only. The root water uptake module 
          ! is not necessary when running in CSM, but is necessary for
          ! CROPSIM.
          
          ! Leaf (stomatal) resistances
          IF(CROP.EQ.'MZ'.OR.CROP.EQ.'ML'.OR.CROP.EQ.'SG')THEN
            ! C-4 Crops  EQ 7 from Allen (1986) for corn.
            RB = 10.0
            RLF =(1.0/(0.0328-5.49E-5*330.0+2.96E-8*330.0**2))+RB
            RLFC=(1.0/(0.0328-5.49E-5*CO2+2.96E-8*CO2**2))+RB
          ELSE
            ! C-3 Crops
            RLF  = 9.72 + 0.0757 * 330.0 + 10.0
            RLFC = 9.72 + 0.0757 *  CO2  + 10.0
          ENDIF

                                                         
          IF (FILEIOT.EQ.'DS4'.AND.IDETG.NE.'N'.OR.
     &        FILEIOT.NE.'DS4') THEN                  
            ! Evapotranspiration calculations for comparison
            IF (ISWWAT.NE.'N') THEN
            ! Call 1  Basic calculations with rcrop = 0  
            CALL EVAPO('M',SRAD,CLOUDS,TMAX,TMIN,TDEW,WINDSP,
     &       ALBEDO,RATM,RCROP*0.0,
     &       EO,EOPEN,EOMPEN,EOPT,EOEBUD,TCAN,'M')
            EOPENC = EOPENC + EOPEN 
            EOPTC = EOPTC + EOPT
            EOMPENC = EOMPENC + EOMPEN
            EOEBUDC = EOEBUDC + EOEBUD
             
            ! CSM with LAI=1.0,CROP=WH,TAVG=20.0,WINDSP=86.4 has
            ! RATM = 55  RCROP = 45
            ! Monteith had RATM = 300, RCROP = 150->500
            
            ! Call 2  Using rcrop as read-in from spe file
            CALL EVAPO('M',SRAD,CLOUDS,TMAX,TMIN,TDEW,WINDSP,
     &       ALBEDO,RATM,RCROP,
     &       EO,EOPEN,EOMPCRP,EOPT,EOEBUDCRP,TCAN,'M')
            EOMPCRPC = EOMPCRPC + EOMPCRP
            EOEBUDCRPC = EOEBUDCRPC + EOEBUDCRP


            ! Call 3 Using rcrop adjusted for CO2 effect     
            IF (RLF.GT.0.0)
     &       CALL EVAPO('M',SRAD,CLOUDS,TMAX,TMIN,TDEW,WINDSP,
     &       ALBEDO,RATM,RCROP*RLFC/RLF,
     &       EO,EOPEN,EOMPCRPCO2,EOPT,EOEBUDCRPCO2,TCAN,'M')
            EOMPCRPCO2C = EOMPCRPCO2C + EOMPCRPCO2
            EOEBUDCRPCO2C = EOEBUDCRPCO2C + EOEBUDCRPCO2

            ! Transpiration ratio (Pot.pl.evap/Pot.soil evap)
            EPSRATIO = 1.0
            IF (EOMPEN.GT.0.0) EPSRATIO = EOMPCRPCO2 / EOMPEN
            TRATIO = 1.0
            IF (EOMPCRP.GT.0.0) TRATIO = EOMPCRPCO2 / EOMPCRP
            ENDIF
            
            IF (fileiot(1:2).NE.'DS') THEN
              ! Calculate plant potential evaporation 
              EOP = MAX(0.0,EO/EOMPEN*EOMPCRPCO2 * (1.0-EXP(-LAI*KEP)))
              ! Ratio necessary because EO method may not be Monteith
              CALL CSCRPROOTWU(ISWWAT, 
     &         NLAYR, DLAYR, LL, SAT, WFEU, MEWNU,
     &         EOP, RLV, RWUPM, RLFWU, RWUMX, RTDEP,
     &         SW, WTDEP, uh2o, trwup, trwu)
            ENDIF
            
            ! Call 4 Using rcrop adjusted for CO2 & H2O effect     
            ! NB. Using previous days WFP. 
            ! If use this for other than comparison, must check  LAH
            IF (RLF.GT.0.0.AND.WFP.LE.1.0.AND.WFP.GT.0.0) THEN
             CALL EVAPO('M',SRAD,CLOUDS,TMAX,TMIN,TDEW,WINDSP,
     &        ALBEDO,RATM,RCROP*RLFC/RLF*(1.0+(1.0-WFP)),
     &        EO,EOPEN,EOMPCRPCO2H2O,EOPT,EOEBUDCRPCO2H2O,TCAN,'M')
            ELSE
!              IF (RLF.GT.0.0)
!     &         WRITE(FNUMWRK,*)' WFP OUT OF RANGE. WFP = ',WFP
            ENDIF
            EOMPCRPCO2H2OC = EOMPCRPCO2H2OC + EOMPCRPCO2H2O
            EOEBUDCRPCO2H2OC = EOEBUDCRPCO2H2OC + EOEBUDCRPCO2H2O
            
            ! Call 5 to calcuate canopy temperature
            TVR1 = (TRWU*10.0+ES)
            CALL EVAPO('M',SRAD,CLOUDS,TMAX,TMIN,TDEW,WINDSP,
     &      ALBEDO,RATM,RCROP*RLFC/RLF*(1.0+(1.0-WFP)),
     &      EO,tvr1,tvr2,tvr3,tvr4,TCAN,'C')
          ENDIF
          
          ! Cumulative potential ET as used
          IF (EO.GT.0.0) EOC = EOC + EO
          
          ! Cumulative canopy-air temperature difference
          TDIFSUM = TDIFSUM+(TCAN-TMEAN)
          TDIFNUM = TDIFNUM + 1
          TDIFAV = TDIFSUM/TDIFNUM

!-----------------------------------------------------------------------
!         Calculate thermal time
!-----------------------------------------------------------------------
          Tfd = TFAC4(trdv1,tmean,TT)
          ! Used when when working with Doug Stewart's function
          !TTOLD = TT  
          !TFDOLD = TFD
          Tfdnext = TFAC4(trdv2,tmean,TTNEXT)
          IF (rstage.GE.2.0.AND.rstage.LT.3.0)THEN
            Tfd = TFAC4(trdv2,tmean,TT)
            Tfdnext = TFAC4(trdv3,tmean,TTNEXT)
          ELSEIF (rstage.GE.3.0.AND.rstage.LT.4.0)THEN
            Tfd = TFAC4(trdv3,tmean,TT)
            Tfdnext = TFAC4(trdv4,tmean,TTNEXT)
          ELSEIF (rstage.GE.4.0.AND.rstage.LT.5.0)THEN
            Tfd = TFAC4(trdv4,tmean,TT)
            Tfdnext = TFAC4(trdv5,tmean,TTNEXT)
          ELSEIF (rstage.GE.5.0.AND.rstage.LT.6.0)THEN
            Tfd = TFAC4(trdv5,tmean,TT)
            Tfdnext = TFAC4(trdv6,tmean,TTNEXT)
          ELSEIF (rstage.GE.6.0.AND.rstage.LT.7.0)THEN
            Tfd = TFAC4(trdv6,tmean,TT)
            Tfdnext = TFAC4(trdv7,tmean,TTNEXT)
          ELSEIF (rstage.GE.7.0.AND.rstage.LT.8.0)THEN
            Tfd = TFAC4(trdv7,tmean,TT)
            Tfdnext = TFAC4(trdv8,tmean,TTNEXT)
          ELSEIF (rstage.GE.8.0)THEN
            Tfd = TFAC4(trdv8,tmean,TT)
          ENDIF

          ! To test Beta function
          !Tfd = TFACBETA(trdv1,tmean,TT)
          
!         ! To test Doug Stewart's function
!         tmaxwheat = trdv1(2)
!         tminwheat = trdv1(1)
!         ! 1. Calculate where would be along temp axis for maize
!         Tequiv = tmean*(32.226/(Tmaxwheat-tminwheat))
!         ! 2.Calculate maize value according to Stewart
!         TTmaize =  0.043*tequiv*tequiv - 0.000894*tequiv*tequiv*tequiv
!         ! 3. Calculte TTwheat (from temperature span from max to min)
!         TTwheat = TTmaize * (Tmaxwheat-Tminwheat)/14.869
!         IF (tmean.LE.tmaxwheat) THEN
!           TT = TTWHEAT 
!           TTNEXT = TTWHEAT  
!           Tfd = TT/(TMAXWHEAT-TMINWHEAT)
!           Tfdnext = TT/(TMAXWHEAT-TMINWHEAT)
!         ELSE
!           ! Use standard function if temp above maximum 
!           TT = TTOLD 
!           TTNEXT = TTOLD  
!           Tfd = TFDOLD
!           Tfdnext = TFDOLD
!         ENDIF  
          
          IF (trgem(3).GT.0.0) THEN
            Tfgem = TFAC4(trgem,tmean,TTGEM)
          ELSE
            Ttgem = tt
          ENDIF    

          Tflflife = TFAC4(trdv1,tmean,TTlflife)

          ! Angus approach
          !IF (RSTAGE.LT.2.0) THEN
          !  TT = AMAX1(0.0,1.0 - exp(-0.231*(tmean-3.28)))*(TMEAN-3.28)
          !ELSE 
          !  TT = AMAX1(0.0,1.0 - exp(-0.116*(tmean-5.11)))*(TMEAN-5.11)
          !ENDIF
          !IF (RSTAGE.LT.1.0) THEN
          !  TTNEXT = AMAX1(0.0,1.0 - exp(-0.231*(tmean-3.28)))
     ^    !   *(TMEAN-3.28)
          !ELSE  
          !  TTNEXT = AMAX1(0.0,1.0 - exp(-0.116*(tmean-5.11)))
     &    !   *(TMEAN-5.11)
          !ENDIF  
          ! End Angus

!-----------------------------------------------------------------------
!         Calculate soil water 'status' (Used as a sw 'potential')
!-----------------------------------------------------------------------

          DO L = 1,NLAYR
            SWP(L) =
     &       AMIN1(1.0,AMAX1(.0,((SW(L)-LL(L))/(DUL(L)-LL(L)))))
          ENDDO
          
!-----------------------------------------------------------------------
!         Calculate water factor for germination
!-----------------------------------------------------------------------

          WFGE = 1.0
          IF (ISWWAT.NE.'N') THEN
            ! If not yet germinated
            IF (GESTAGE.LT.1.0) THEN
              ! If the layer in which the seed is located not defined
              IF (LSEED.LT.0) LSEED = CSIDLAYR (NLAYR, DLAYR, SDEPTH)
              IF (LSEED.GT.1) THEN
                ! If the seed layer is > 1, use the water 'potential' 
                ! of the layer 
                SWPSD = SWP(LSEED)
              ELSE
                ! If the seed layer is = 1, use a weighted average of 
                ! the 'water potentials' of layers 1 and 2 
               SWP(0) = AMIN1(1.,AMAX1(.0,(SWP(1)-0.5*(SWP(2)-SWP(1)))))
               SWPSD = SWP(0) + (SDEPTH/DLAYR(1))*(SWP(2)-SWP(0))
              ENDIF
              IF (WFGEU.GT.0.0)
     &         WFGE = AMAX1(0.0,AMIN1(1.0,(SWPSD/WFGEU)))
            ENDIF
          ENDIF
          ! If the water effect is turned off for crop growth.
          IF (ISWWATCROP.EQ.'E') WFGE = 1.0

!=======================================================================
          IF (GEUCUM+TTGEM*WFGE.GE.PEGD) THEN  ! If germinated by endday
!=======================================================================

!-----------------------------------------------------------------------
!           Determine when in day germination and emergence occurred
!-----------------------------------------------------------------------

            ! Germination
            IF (GEUCUM.LT.PEGD.AND.GEUCUM+TTGEM*WFGE.LT.PEGD) THEN
              GERMFR = 0.0
            ELSEIF (GEUCUM.LE.PEGD.AND.GEUCUM+TTGEM*WFGE.GE.PEGD) THEN
              GERMFR = 1.0 - (PEGD-GEUCUM)/(TTGEM*WFGE)
              STGYEARDOY(1) = YEARDOY
            ELSEIF (GEUCUM.GT.PEGD) THEN
              GERMFR = 1.0
            ENDIF

            ! Emergence
            IF (GEUCUM.LT.PEGD+PEMRG*SDEPTHU.AND.
     &       GEUCUM+TTGEM*WFGE.LE.PEGD+PEMRG*SDEPTHU) THEN
              EMRGFR = 0.0
            ELSEIF (GEUCUM.LE.PEGD+PEMRG*SDEPTHU.AND.
     &       GEUCUM+TTGEM*WFGE.GT.PEGD+PEMRG*SDEPTHU) THEN
              EMRGFR = 1.0 - (PEGD+PEMRG*SDEPTHU-GEUCUM)/(TTGEM*WFGE)
            ELSEIF (GEUCUM.GT.PEGD+PEMRG*SDEPTHU) THEN
              EMRGFR = 1.0
            ENDIF

!-----------------------------------------------------------------------
!           Calculate temperature factors for vernalization,hardening
!-----------------------------------------------------------------------

            ! Vernalization
            Tfv = TFAC4(trvrn,tmeansurf,TTOUT)
            ! Loss of vernalization (De-vernalization)
            VDLOS = 0.0
            IF (RSTAGE.LT.2.0) THEN
              IF (CUMVD.LT.VREQ*VLOSS .AND. TMEAN.GE.VLOST) THEN
                ! In Ceres was 0.5*(TMAX-30.0)
                VDLOS = VLOSF * CUMVD  ! From AFRC
!                WRITE(Fnumwrk,*)' '
!                WRITE(FNUMWRK,*)' Warning. De-vernalization! ' 
              ENDIF
            ENDIF  

            ! Cold hardening
            Tfh = TFAC4(trcoh,tmeansurf,TTOUT)
            ! Loss of cold hardiness
            HARDILOS = 0.0
            IF (TMEAN.GE.HLOST) THEN
              HARDILOS = HLOSF * HARDAYS
            ENDIF
     
!-----------------------------------------------------------------------
!           Calculate daylength factors for development
!-----------------------------------------------------------------------
            DF = 1.0
            DFNEXT = 1.0
            ! To ensure correct sensitivity on emergence day
            IF (RSTAGE.LE.0.0) THEN
              RSTAGETMP = 1.0
            ELSE
              RSTAGETMP = RSTAGE
            ENDIF
            IF (PPSEN.EQ.'SL') THEN      ! Short day response,linear 
              DF = 1.0 - PPS(INT(RSTAGETMP))/1000.*(PPTHR-DAYLT)
              IF (RSTAGETMP.LT.FLOAT(MSTG)) THEN
               DFNEXT = 1.-PPS(INT(RSTAGETMP+1))/1000.*(PPTHR-DAYLT)
              ELSE
               DFNEXT = DF
              ENDIF 
            ELSEIF (PPSEN.EQ.'LQ') THEN  ! Long day response,quadratic
              DF = AMAX1(0.0,AMIN1(1.0,1.0-
     &        (PPS(INT(RSTAGETMP))/10000.*(PPTHR-DAYLT)**PPEXP)))
              IF (RSTAGETMP.LT.10) DFNEXT = AMAX1(0.0,AMIN1(1.0,1.0-
     &        (PPS(INT(RSTAGETMP+1))/10000.*(PPTHR-DAYLT)**PPEXP)))
              ! Angus approach
              !IF (rstage.lt.2.0) then
              !  DF = AMAX1(0.0,1.0-EXP(-0.0927*(DAYLT-4.77))) 
              !else
              !  DF = AMAX1(0.0,1.0-EXP(-0.283*(DAYLT-9.27))) 
              !endif  
              !IF (rstage.lt.1.0) then
              !  DFNEXT = AMAX1(0.0,1.0-EXP(-0.0927*(DAYLT-4.77))) 
              !else
              !  DFNEXT = AMAX1(0.0,1.0-EXP(-0.283*(DAYLT-9.27))) 
              !endif  
              ! End Angus
              Tfdf = AMAX1(0.0,1.0-AMAX1(0.0,(TMEAN-10.0)/10.0))
              Tfdf = 1.0  ! LAH No temperature effect on DF ! 
              DF = DF + (1.0-DF)*(1.0-TFDF)
              DFNEXT = DFNEXT + (1.0-DFNEXT)*(1.0-TFDF)
            ENDIF
            
            ! Set daylength factor for output (= ppfpe before emergence)
            IF (EMRGFR.GE.1.0) THEN
              DFOUT = DF
            ELSE
              DFOUT = PPFPE
            ENDIF 

!-----------------------------------------------------------------------
!           Calculate development units
!-----------------------------------------------------------------------

            DU = 0.0
            DUPHASE = 0.0
            DUPNEXT = 0.0
            ! To avoid exceeding the array sizes
            IF (RSTAGETMP.LT.10) THEN
              DUNEED = PSTART(INT(RSTAGETMP+1))-CUMDU
              IF (DUNEED.GE.TT*VF*(PPFPE*(GERMFR-EMRGFR)+DF*EMRGFR))THEN
                DUPHASE = TT*VF*(PPFPE*(GERMFR-EMRGFR)+DF*EMRGFR)
                ! CERES:
                !DU = TT*VF*DF*LIF2    ! NB. Changed from Ceres 3.5
                TIMENEED = 1.0
                DUPNEXT = 0.0
              ELSE  
                DUPHASE = DUNEED
                TIMENEED = DUNEED/
     &           (TT*VF*(PPFPE*(GERMFR-EMRGFR)+DF*EMRGFR))
                DUPNEXT = TTNEXT*(1.0-TIMENEED)*VFNEXT*DFNEXT
              ENDIF
            ENDIF
            
            DU = DUPHASE+DUPNEXT

            ! Leaf growth units
            IF (CUMDU.LT.LGPHASEDU(2)) THEN
            
              IF (DU.GT.0.0) THEN
               LFENDFR = AMAX1(0.0,AMIN1(1.0,(LGPHASEDU(2)-CUMDU)/DU))
              ELSE
                LFENDFR = 1.0
              ENDIF
              
              ! Leaf appearance driven by its own temp response till TS
              ! (DULF based solely on temperature response till TS)
              ! Thereafter linked to crop development units
              IF (CUMDU+DU.LT.PSTART(TSSTG)) THEN
                ! Not reached TS yet
         !       DULF = TTLA*EMRGFR*LFENDFR  ! Thermal time
                DULF = TT*EMRGFR*LFENDFR  ! Thermal time
              ELSE
                IF (CUMDU.LT.PSTART(TSSTG)) THEN
                  ! Reach TS this day
!                  WRITE(fnumwrk,*)' '
!                  WRITE(fnumwrk,'(A37,2F7.2)')
!     &              ' Terminal spikelet. Rstage,Leaf#:    ',
!     &              RSTAGE,LNUM
 !                 DULF = (TTLA*TIMENEED+DUPNEXT)*EMRGFR*LFENDFR
                  DULF = (TT*TIMENEED+DUPNEXT)*EMRGFR*LFENDFR
 !                 LNUMTS = LNUM + (TTLA*TIMENEED*EMRGFR*LFENDFR)/PHINT
                  LNUMTS = LNUM + (TT*TIMENEED*EMRGFR*LFENDFR)/PHINT
                  FLN = LNUMTS+(PD(2)+PD(3))/PHINT
                  ! Calculate FLN using Aitken formula
                  FLNAITKEN = LNUMTS + (2.8 + 0.1*LNUMTS) 

                  IF (CFLPHASEADJ.EQ.'Y') THEN  
                    ! Use Aitken formula for FLN 
                    FLN = FLNAITKEN               
!                     Re-calculate PD(3) and PD(4). PDADJ = PD(2+3)ADJUSTED     
                    IF (PHINTL(2).GT.0..AND.FLN+1..LE.PHINTL(2)) THEN
                      ! Final leaf# < (leaf# for change in phint)
                      PDADJ = (FLN-LNUMTS) * PHINTS*PHINTF(2)
                    ELSEIF (PHINTL(2).GT.0. .AND.
     &               FLN+1..GT.PHINTL(2)) THEN
                      ! Final leaf# > (leaf# for change in phint)
                      IF (LNUM.GE.PHINTL(2)) THEN
                        ! Leaf# at TS > (leaf# for change in phint)
                        PDADJ = (FLN-LNUMTS) * PHINTS*PHINTF(3)  
                      ELSE
                        ! Leaf# at TS < (leaf# for change in phint)
                        PDADJ = ((FLN-PHINTL(2)))*PHINTS*PHINTF(3)  
     &                      + (PHINTL(2)-LNUMTS)*PHINTS*PHINTF(2) 
                      ENDIF   
                    ENDIF
!                    WRITE(fnumwrk,*)' '
!                    WRITE(fnumwrk,'(A47)')
!     &               ' Phase 3 (Pseudo-stem to last leaf) adjusted   '
                     PD3AITKEN = PDADJ-PD(2)
                    IF ((PD3AITKEN-PD(3)).GT.(PD(4)-10.0)) THEN
                      PD3NEW = PD(3) + (PD(4)-10.0)
!                      WRITE(fnumwrk,'(A47)')
!     &                '   Pd3 Aitken adjustment > Pd(4). Not possible.'
!                      WRITE(fnumwrk,'(A29,2F7.1)')
!     &                '   Pd3 Aitken,what possible: ',pd3aitken,pd3new
                    ELSE
                      PD3NEW = PD3AITKEN  
                    ENDIF
!                    WRITE(fnumwrk,'(A16,2F7.1)')
!     &               '   Pd3 old,new  ',PD(3),PD3NEW
     
!                    WRITE(fnumwrk,'(A16,2F7.1)')
!     &               '   Pd4 old,new  ',
!     &                   PD(4),PD(4)+(PD(3)-PD3NEW)
                    ! Re-set thresholds
                    PSTART(4) = PSTART(4) + PD3NEW - PD(3)
                    ! Need to re-calculate following: 
                    ! (If set to occur in phase 3 or 4)
                    ! TILPEDU = 
                    ! TDPHSDU = 
                    ! TDPHEDU = 
                    ! Re-set end of leaf growth phase
!                    WRITE(fnumwrk,'(A16,2F7.1)')
!     &               '   End of leaf growth phase. Old,new ',
!     &               LGPHASEDU(2),LGPHASEDU(2) + PD3NEW - PD(3)
                    LGPHASEDU(2) = LGPHASEDU(2) + PD3NEW - PD(3)
                    ! Re-set PD(3) and PD(4)
                    PD(4) = PD(4) + (PD(3)-PD3NEW) 
                    PD(3) = PD3NEW
                  ENDIF    ! End of CFLADJ calculations 
                  
                ELSE  ! After TS 
                  DULF = DU*EMRGFR*LFENDFR  ! Development units
                ENDIF                
                
              ENDIF  ! End of before,afer terminal spikelet
              
            ENDIF  ! End of leaf growth phase calculations
            
!-----------------------------------------------------------------------
!           Set seed reserve use for root growth and update av.reserves
!-----------------------------------------------------------------------

            IF (GERMFR.GT.0.0.OR.GESTAGE.GE.0.5) THEN
              ! SDDUR control on what available for roots
              SEEDRSAVR =
     &         AMIN1(SEEDRS,SEEDRSI/SDDUR*(TT/STDAY)*GERMFR)
            ELSE
              SEEDRSAVR = 0.0
            ENDIF
            ! Seed reserves available
            SEEDRSAV = SEEDRSAV-SEEDRSAVR

!=======================================================================
            IF (GEUCUM+TTGEM*WFGE.GT.PGERM+PEMRG*SDEPTHU) THEN  !If emrg
!=======================================================================

!-----------------------------------------------------------------------
!             Determine if today has a harvest instruction
!-----------------------------------------------------------------------

              HANUM = -99
              HAFR = 0.0
              DO I = 1, 20
                IF (HYEARDOY(I).EQ.YEARDOY) THEN
                  HANUM = I
!                  WRITE(fnumwrk,*) ' '
!                  WRITE(fnumwrk,'(A20,i2,A12,A1,A6,i8)')
!     &             ' Harvest instruction',hanum,
!     &             '  Operation ',hop(i),
!     &             '  Day ',yeardoy
                  CALL CSUCASE(HOP(I)) 
                  IF (hop(i).EQ.'F') YEARDOYHARF = YEARDOY 
                ENDIF
              END DO

!-----------------------------------------------------------------------
!             Determine amounts removed by grazing,etc.   
!-----------------------------------------------------------------------

	        IF (HANUM.GT.0) THEN
	          IF (HOP(HANUM).EQ.'G'.AND.
     &	          CWAD.GT.0.0.AND.CWAD.GT.CWAN(HANUM)) THEN
                    IF (LSNUM(HANUM).GT.0) THEN
                      ! LAH To fit with Zhang should have Animal routine
!                     CALL ANIMAL(TMAX,TMIN,LSNUM(HANUM),
!    &                LSWT(HANUM),CWAD,CWAN(HANUM),hawad)
                    ELSE
	              HAWAD = AMIN1((CWAD-CWAN(HANUM)),HAMT(HANUM))
	            ENDIF  
	            HAWAD = AMAX1(0.0,HAWAD)
                  HAFR = AMAX1(0.0,HAWAD/CWAD)
	          ELSE   
	            HAWAD = 0.0
	            HAFR = 0.0
	          ENDIF
              ENDIF
              
!              IF (HAFR.GT.0.0)
!     &         WRITE(fnumwrk,'(A23,3F6.1)')' HARVEST  FR,CWAN,CWAD ',
!     &          HAFR,CWAN(HANUM),CWAD

              ! For grazing 
              lwph = lfwt * hafr
              laph = lapd * hafr
              swph = stwt * hafr
              rswph = rswt * hafr
              gwph = grwt * hafr
              dwrph = sentopretained * hafr
              lnph = leafn * hafr
              snph = stemn * hafr
              rsnph = rsn * hafr
              gnph = grainn * hafr
              IF (rstage.GT.3.0) spnumh = tnum * hafr*spnumhfac

!-----------------------------------------------------------------------
!             Set aspects that determined on emergence day
!-----------------------------------------------------------------------

              IF (DAE.LT.0) THEN
                IF (CROP.EQ.'BA') THEN
!                  WRITE(FNUMWRK,*) 'Emergence day ',yeardoy,plyeardoy
!                  WRITE(FNUMWRK,*)
!     &             ' PHINTS as read-in ',phints
                  PHINTS = PHINTS - 232.6*(DAYLT-DAYLPREV)
                ENDIF
                LNUMSG = 1  
              ENDIF

!-----------------------------------------------------------------------
!             Check for or calculate PAR interception at start of day
!-----------------------------------------------------------------------

              ! Updated here rather than in integrate because may need
              ! interception from companion crops.
              PARI = 0.0
              PARI1 = (1.0 - EXP((-KCAN)*LAI))
              IF (PARIP.GT.0.0) THEN
                ! From competition model
                IF (ISWDIS(LENDIS:LENDIS).NE.'N') THEN
                  PARI = PARIPA/100.0
                ELSE
                  PARI = PARIP/100.0
                ENDIF
                ! LAH  Should use canopy area during grain filling
                !IF (CUMDU.GT.PSTART(IESTG)) THEN
                !  PARI = AMAX1(PARI,(1.0-EXP(-KCAN*CAID)))
                !ENDIF
              ELSE
                PARI = PARI1
                ! LAH For row crops may need to change 
                ! In original Ceres maize, kcan is calculated as:
                ! 1.5 - 0.768*((rowspc*0.01)**2*pltpop)**0.1
                ! eg. 1.5 - 0.768*((75*0.01)**2*6.0)**0.1  =  0.63
              ENDIF

!-----------------------------------------------------------------------
!             Calculate adjustment to yesterday's C assimilation
!-----------------------------------------------------------------------

              ! End of day interception = today's starting interception

              IF (MEPHS.EQ.'M') CARBOEND = CARBOTMPM * PARI/PLTPOP
              IF (MEPHS.EQ.'I') CARBOEND = CARBOTMPI * PARI/PLTPOP
              IF (MEPHS.EQ.'R') CARBOEND = CARBOTMPR * PARI/PLTPOP

              CARBOADJ = (CARBOEND-CARBOBEG)/2.0*EMRGFRPREV
              ! But note, no adjustment if leaf kill
              PARMJIADJ = PARMJFAC*SRADPREV*(PARI-PARIPREV)/2.0*EMRGFR

!-----------------------------------------------------------------------
!             Calculate process rate factors
!-----------------------------------------------------------------------

              ! Water
              ! No water stress after emergence on day that emerges
              WFG = 1.0
              WFP = 1.0
              WFT = 1.0
              IF (ISWWAT.NE.'N') THEN
                IF (EOP.GT.0.0) THEN
                  WUPR = TRWUP/(EOP*0.1)
                  IF (WFGU-WFGL.GT.0.0)
     &             WFG = AMAX1(0.0,AMIN1(1.0,(WUPR-WFGL)/(WFGU-WFGL)))
                  IF (WFPU-WFPL.GT.0.0)
     &             WFP = AMAX1(0.0,AMIN1(1.0,(WUPR-WFPL)/(WFPU-WFPL)))
                  IF (WFTU-WFTL.GT.1.0E-6) WFT =
     &              AMAX1(0.0,AMIN1(1.0,(WUPR-WFTL)/(WFTU-WFTL)))
                ENDIF
                IF (ISWWATEARLY.EQ.'N') THEN
                  WFG = 1.0
                  WFP = 1.0
                  WFT = 1.0
                ENDIF
              ENDIF

              ! Nitrogen
              ! WARNING No N stress after emergence on day that emerges
              IF (ISWNIT.NE.'N') THEN
                IF (LFWT.GT.1.0E-5) THEN
                  !NFG =AMIN1(1.0,AMAX1(0.0,(LANC-LNCGL)/(LNCGU-LNCGL)))
                  LNCGL = LNCM + NFGL * (LNCX-LNCM)
                  LNCGU = LNCM + NFGU * (LNCX-LNCM)
                  IF (LNCGU - LNCGL > 1.E-6) THEN
                   NFG =AMIN1(1.0,AMAX1(0.0,(LANC-LNCGL)/(LNCGU-LNCGL)))
                  ELSE
                   NFG = 1.0 
                  ENDIF
                  LNCTL = LNCM + NFTL * (LNCX-LNCM)
                  LNCTU = LNCM + NFTU * (LNCX-LNCM)
                  IF (LNCTU - LNCTL > 1.E-6) THEN
                   NFT =AMIN1(1.0,AMAX1(0.0,(LANC-LNCTL)/(LNCTU-LNCTL)))
                  ELSE
                   NFT = 1.0 
                  ENDIF
                  LNCPL = LNCM + NFPL * (LNCX-LNCM)
                  LNCPU = LNCM + NFPU * (LNCX-LNCM)
                  IF (LNCPU - LNCPL > 1.E-6) THEN
                   NFP =AMIN1(1.0,AMAX1(0.0,(LANC-LNCPL)/(LNCPU-LNCPL)))
                  ELSE
                   NFP = 1.0 
                  ENDIF
                ELSE
                  NFG = 1.0
                  NFP = 1.0  
                  NFT = 1.0
                ENDIF
              ENDIF

              ! If N stress switched off early in cycle. 
              IF (ISWNITEARLY.EQ.'N') THEN
                NFG = 1.0
                NFP = 1.0  
                NFT = 1.0
              ENDIF

              ! Reserves
              IF (RSFPU.GT.0.0.AND.RSFPL.GT.0.0) THEN
                RSFP = 1.-AMIN1(1.,AMAX1(0.,(RSCD-RSFPL)/(RSFPU-RSFPL)))
              ELSE
                RSFP = 1.0
              ENDIF

              ! Temperature
              ! LAH No cold night effect.
              ! Maybe,one cold night --> reduced phs next day!!
              ! May want to introduce:
              ! IF (TMEAN20.LT.0.0) TFG = 0.0
              ! IF (TMEAN20.LT.0.0) TFP = 0.0
              Tfp = TFAC4(trphs,tmean,TTOUT)
              Tfg = TFAC4(trlfg,tmean,TTOUT)
              Tfgf = TFAC4(trgfc,tmean,TTOUT)
              Tfgn = TFAC4(trgfn,tmean,TTOUT)

              ! Vapour pressure
              VPDFP = 1.0
              IF (PHTV.GT.0.0) THEN
                IF (TDEW.LE.-98.0) TDEW = TMIN
                VPD = CSVPSAT(tmax) - CSVPSAT(TDEW)    ! Pa 
                IF (VPD/1000.0.GT.PHTV)
     &           VPDFP = AMAX1(0.0,1.0+PHSV*(VPD/1000.0-PHTV))
              ENDIF

!               Co2 factor using CROPGRO formula
!               CO2EX Exponent for CO2-PHS relationship (0.05)  
!               COCCC CO2 compensation concentration (80 vpm)
!              CO2FP = PARFC*((1.-EXP(-CO2EX*CO2))-(1.-EXP(-CO2EX*CO2COMPC)))
!               CO2 factor
              CO2FP = YVALXY(CO2RF,CO2F,CO2)

!-----------------------------------------------------------------------
!             Calculate leaf number at end of day;adjust PHINT if needed
!-----------------------------------------------------------------------
                                               
              DULFNEXT = 0.0
              LAGEG = 0.0
              LNUMG = 0.0
              ! If in leaf growth phase
              IF(CUMDU.GE.LGPHASEDU(1).AND.CUMDU.LT.LGPHASEDU(2)) THEN
              
                ! NEW LAH MARCH 2010
                IF (PHINTL(PHINTSTG).LE.0.0.
     &               OR.LNUM+DULF/PHINT.LE.PHINTL(PHINTSTG)) THEN
                  LNUMEND = LNUM + DULF/PHINT
                  IF (INT(LNUMEND).GT.INT(LNUM)) 
     &             DULFNEXT = (LNUMEND-FLOAT(INT(LNUMEND))) * PHINT
                ELSEIF(PHINTL(PHINTSTG).GT.0.
     &            .AND. LNUM+DULF/PHINT.GT.PHINTL(PHINTSTG))THEN
                  TVR1 = AMAX1(0.0,DULF-((PHINTL(PHINTSTG)-LNUM)*PHINT))
                  LNUMEND = PHINTL(PHINTSTG) 
     &                    + TVR1/(PHINT*PHINTF(PHINTSTG+1))
                  IF (INT(LNUMEND).GT.INT(LNUM)) THEN
                    ! Below not fully accurate - assumes that new 
                    ! leaf growing entirely at new PHINT
                    DULFNEXT = LNUMEND
     &              - FLOAT(INT(LNUMEND))*(PHINT*PHINTF(PHINTSTG+1))
                  ENDIF
!                  WRITE(FNUMWRK,*)' '
!                  WRITE(FNUMWRK,'(A20,I3,A4,I3,A11,F4.1,A2,I7)')
!     &             ' PHINT changed from ',INT(PHINT),
!     &             ' to ',INT(PHINTS*PHINTF(PHINTSTG+1)),
!     &             '. Leaf # = ',lnum,'  ',yeardoy
                  PHINT = PHINTS * PHINTF(PHINTSTG+1)
                  PHINTSTORE = PHINT
                  PHINTSTG = PHINTSTG + 1
                  ! Leaf growth,active,senescence phases adjusted
                  LLIFGTT = LLIFG * PHINT 
                  LLIFATT = LLIFA * PHINT 
                  LLIFSTT = LLIFS * PHINT 
                ENDIF
                IF (CFLPHINTADJ.EQ.'Y') THEN
                  ! Adjust phint for rate of change of daylength.Kirby
                  !tvr1 = 1.0/(0.00949+0.000988*((DAYLT-DAYLPREV)*60.0))
                  TVR2 = 
     &             1.0/((1.0/PHINTSTORE)+.000988*((DAYLT-DAYLPREV)*60.))
!                  WRITE(FNUMWRK,*)
!     &              'PHINT adjusted. Normal,f(Delta daylngth)',
!     &              PHINTSTORE,TVR2
                    PHINT = PHINTSTORE 
     &               - PHINTSTORE*0.1*((DAYLT-DAYLPREV)*60.0)
                ENDIF
              ENDIF
              
              ! Restrict to maximum
              LNUMEND = AMIN1(FLOAT(LNUMX),LNUMEND)
              IF(FLN.GT.0.0) LNUMEND = AMIN1(FLN,LNUMEND)
              LNUMG = LNUMEND - LNUM
              ! Calculate an overall PHINT for output
              IF (LNUMG.GT.0.0) PHINTOUT = DULF/LNUMG

!-----------------------------------------------------------------------
!             Calculate senescence of leaves,stems,etc..
!-----------------------------------------------------------------------

              TNUMLOSS = 0.0
              PLTLOSS = 0.0
              PLASC = 0.0
              PLASP = 0.0
              PLASI = 0.0
              PLASL = 0.0
              PLASFS = 0.0
              PLASPM = 0.0
              PLASS = 0.0
              PLAST = 0.0
              PLAST1 = 0.0
              PLAST2 = 0.0
              SENRS = 0.0
              SENNRS = 0.0
              SENSTG = 0.0
              SENSTGRS = 0.0
              SENNSTG = 0.0
              SENNSTGRS = 0.0
              SSENF = 0.0

              ! Leaf senescence - cold kill
              IF (PLA-SENLA.GT.0.0.AND.TMEAN.LT.(TKILL+TKDLF)) THEN
                SPANPOS = AMIN1(1.0,((TKILL+TKDLF)-TMEAN)/TKSPN)
                PLASC = (PLA-SENLA)*SPANPOS
                PLASCSUM = PLASCSUM + PLASC
                WRITE (Message(1),899)
     &           (TKILL+TKDLF),(TMIN+TMAX)/2.0,HSTAGE,
     &           (PLA-SENLA)*PLTPOP*0.0001,
     &           (PLA-SENLA)*PLTPOP*0.0001*SPANPOS
                CALL WARNING(1,'CSCRP',MESSAGE)
!                WRITE (Fnumwrk,*) ' '
!                WRITE (Fnumwrk,893) yeardoy
! 893            FORMAT (' Leaf kill on ',I7)
!                WRITE (Fnumwrk,899)
!     &           (TKILL+TKDLF),(TMIN+TMAX)/2.0,HSTAGE,
!     &           (PLA-SENLA)*PLTPOP*0.0001,
!     &           (PLA-SENLA)*PLTPOP*0.0001*SPANPOS
 899            FORMAT (
     &           ' TKLF =',F5.1,1X,'TMEAN =',F5.1,1X,
     &           'HSTAGE =',F5.1,1X,'LAI =',  F5.3,1X,'LOSS =',F6.3)
              ENDIF

              ! Tiller kill
              TNUMLOSS = 0.0
              IF (TNUM.GT.1.0.AND.TMEAN.LT.(TKILL+TKDTI)) THEN
                SPANPOS = AMIN1(1.0,((TKILL+TKDTI)-TMEAN)/TKSPN)
                TNUMLOSS = (TNUM-1.0)*SPANPOS
                IF (TNUMLOSS.GT.0.0)THEN
                  WRITE (Message(1),900)
     &            (TKILL+TKDTI),(TMIN+TMAX)/2.0,HSTAGE,TNUM,TNUMLOSS
                  CALL WARNING(1,'CSCRP',MESSAGE)
!                  WRITE (Fnumwrk,*) ' '
!                  WRITE (Fnumwrk,900)
!     &            (TKILL+TKDTI),(TMIN+TMAX)/2.0,HSTAGE,TNUM,TNUMLOSS
                  CALL WARNING(1,'CSCRP',MESSAGE)
                ENDIF
 900             FORMAT (' Tiller kill',
     &           ' TKTI =',F5.1,1X,'TMEAN =',F5.1,1X,
     &           'HSTAGE =',F5.1,1X,'TNO =',  F5.2,1X,'LOSS =',F5.2)
              ENDIF

              ! Plant kill
              IF (TMEAN.LT.TKILL) THEN
                SPANPOS = AMIN1(1.0,(TKILL-TMEAN)/TKSPN)
                PLTLOSS = PLTPOP*SPANPOS
                IF (PLTPOP-PLTLOSS.GE.0.05*PLTPOPP) THEN
                  WRITE (Message(1),901)
     &            TKILL,(TMIN+TMAX)/2.0,HSTAGE,PLTPOP,PLTLOSS
 901              FORMAT (' Plant kill',
     &             ' TKILL =',F5.1,1X,'TMEAN =',F5.1,1X,
     &             'HSTAGE =',F5.1,1X,'PLNO =',  F5.1,1X,'LOSS =',F5.1)
                  CALL WARNING(1,'CSCRP',MESSAGE)
!                  WRITE(Fnumwrk,*)' '
!                  WRITE(Fnumwrk,991)' Plant kill on ',YEARDOY
! 991              FORMAT(A15,I7)                 
!                  WRITE(fnumwrk,902)
!     &            TKILL,(TMIN+TMAX)/2.0,HSTAGE,PLTPOP,PLTLOSS
! 902              FORMAT (
!     &             ' TKILL =',F5.1,1X,'TMEAN =',F5.1,1X,
!     &             'HSTAGE =',F5.1,1X,'PLNO =',  F5.1,1X,'LOSS =',F5.1)
                ELSE
                  CFLFAIL = 'Y'
                  WRITE (Message(1),1100)
     &            TKILL,(TMIN+TMAX)/2.0,HSTAGE,PLTPOP,PLTLOSS
 1100             FORMAT (' Kill>95% ',
     &             ' TKILL =',F5.1,1X,'TMEAN =',F5.1,1X,
     &             'HSTAGE =',F5.1,1X,'P# =',F5.1,1X,'LOSS =',F5.1)
                  CALL WARNING(1,'CSCRP',MESSAGE)
!                  WRITE(Fnumwrk,*)' '
!                  WRITE(Fnumwrk,992)' Plant kill > 95% on ',YEARDOY
! 992              FORMAT(A21,I7)                 
!                  WRITE(fnumwrk,1101)
!     &            TKILL,(TMIN+TMAX)/2.0,HSTAGE,PLTPOP,PLTLOSS
! 1101             FORMAT (
!     &             ' TKILL =',F5.1,1X,'TMEAN =',F5.1,1X,
!     &             'HSTAGE =',F5.1,1X,'P# =',F5.1,1X,'LOSS =',F5.1)
                ENDIF
              ENDIF

              ! If cold kill, other senescence not calculated
              IF (PLASC.LE.0.0) THEN
              
!                 Leaf senescence - phyllochron (really thermal time) driven
                LAPSTMP = 0.0
                IF (CUMDU+DU.LE.LGPHASEDU(2)) THEN
                  DO L = 1,LNUMSG
                    IF (LAGEDU(L)+DULF.LE.LLIFATT+LLIFGTT) EXIT
                    IF (LAP(L)-LAPS(L).GT.0.0) THEN
                      LAPSTMP = AMIN1((LAP(L)-LAPS(L)),LAP(L)/LLIFSTT
     &                 *AMIN1((LAGEDU(L)+DULF-(LLIFATT+LLIFGTT)),
     &                 DULF))
                      LAPS(L) = LAPS(L) + LAPSTMP
                      PLASP = PLASP + LAPSTMP
                    ENDIF
                  ENDDO
                ENDIF

                ! Leaf senescence - final triggers
                IF (RSTAGEFS.LE.0.0) THEN
                  IF (CUMDU+DU.GE.LSPHSDU) THEN
                    RSTAGEFS = LSPHS
                    FSDU = LSPHSDU
                  ENDIF
                  IF (ISWNIT.NE.'N') THEN
                   LNCSENF = LNCM + NFSF * (LNCX-LNCM)
                   IF (CUMDU.GE.PSTART(MSTG-1).AND.LANC.LE.LNCSENF) THEN
                     RSTAGEFS = RSTAGE
                     FSDU = CUMDU
                     WRITE(Message(1),'(A34)')
     &                'Final senescence trigger(Nitrogen)'
                     CALL WARNING(1,'CSCRP',MESSAGE)
!                     WRITE(Fnumwrk,*)' '
!                     WRITE(Fnumwrk,'(A43,F6.1)')
!     &                ' Final senescence trigger(Nitrogen). Stage:'
!     &                ,gstage
!                     WRITE(Fnumwrk,'(A42,2F6.3)')
!     &                ' N factor,N conc.for final senescence      ',
!     &                nfsf,lncsenf
!                     WRITE(Fnumwrk,'(A42,3F6.3)')
!     &                ' N conc; actual,min,max                    ',
!     &                lanc,lncm,lncx
                   ENDIF
                  ENDIF 
                  IF (ISWWAT.NE.'N') THEN
                   IF (CUMDU.GE.PSTART(MSTG-1).AND.WUPR.LE.WFSF) THEN
                     RSTAGEFS = RSTAGE
                     FSDU = CUMDU
                     WRITE(Message(1),'(A32)')
     &                'Final senescence trigger(Water) '         
                     CALL WARNING(1,'CSCRP',MESSAGE)
!                     WRITE(Fnumwrk,*)' '
!                     WRITE(Fnumwrk,'(A33)')
!     &                ' Final senescence trigger(Water) '         
                   ENDIF
                  ENDIF
                  ! Determine duration of final senescence phase
                  IF (FSDU.GT.0.0) THEN
                    PDFS = LSPHEDU - FSDU
                  ELSE
                    PDFS = LSPHEDU
                  ENDIF  
                ENDIF

                ! Leaf senescence - injury
                IF (CUMDU+DU.GT.LGPHASEDU(2)) THEN
                  IF (CUMDU.LT.LGPHASEDU(2)) THEN
                    PLASI = PLA*(LSENI/100.0)*(DU-DUNEED)/STDAY
                  ELSE
                    IF (RSTAGEFS.GT.0.0.AND.DU.GT.0.0) THEN
                      PLASI = AMAX1(0.0,PLA*(LSENI/100.0)*DU/STDAY*
     &                 (FSDU-CUMDU)/DU)
                      IF (GPLASENF.LE.0.0)
     &                 GPLASENF = AMAX1(0.0,PLA-SENLA-PLASI)
                    ELSE
                      PLASI = PLA*(LSENI/100.0)*DU/STDAY
                    ENDIF
                  ENDIF
                ENDIF

                ! Leaf senescence - final,before end of grain filling
                IF (RSTAGEFS.GT.0.0.AND.PDFS.GT.0.0) THEN
                  IF (CUMDU.LT.FSDU.AND.CUMDU+DU.GT.FSDU) THEN
                    PLASFS = AMAX1(0.0,GPLASENF*(CUMDU+DU-FSDU)/PDFS)
                  ELSEIF(CUMDU.GE.FSDU.AND.
     &             CUMDU+DU.LE.PSTART(MSTG))THEN
                    PLASFS = AMIN1(PLA-SENLA,AMAX1(0.,GPLASENF*DU/PDFS))
                  ELSEIF(CUMDU.GE.FSDU.AND.
     &             CUMDU+DU.LT.PSTART(MSTG))THEN
                    PLASFS = AMIN1(PLA-SENLA,
     &                       AMAX1(0.,GPLASENF*DUNEED/PDFS))
                  ENDIF
                ENDIF

                ! Leaf senescence - final,after end of grain filling
                IF (CUMDU+DU.GT.PSTART(MSTG) .AND. PDFS > 1.E-6) THEN
                  IF (CUMDU.LT.PSTART(MSTG)) THEN
                    PLASPM  = AMIN1(PLA-SENLA-PLASFS,
     &                        AMAX1(0.0,GPLASENF*(DU-DUNEED)/PDFS))
                  ELSE
                    PLASPM  = AMIN1(PLA-SENLA-PLASFS,
     &                        AMAX1(0.0,GPLASENF*DU/PDFS))
                  ENDIF
                ELSE
                  PLASPM = 0.0
                ENDIF

                ! Leaf senescence - water or N stress
                PLASW = 0.0
                PLASN = 0.0
                IF (ISWWAT.NE.'N') THEN
                  ! LAH NEED WATER STUFF 
                  IF (PLA-SENLA.GT.0.0.AND.WUPR.LT.WFSU)
     &              PLASW = AMAX1(0.0,AMIN1(
     &                  (PLA-SENLA)-PLAS,(PLA-SENLA)*LLOSA))
                ENDIF
                IF (ISWNIT.NE.'N') THEN
                  LNCSEN = LNCM + NFSU * (LNCX-LNCM)
                  IF (PLA-SENLA.GT.0.0.AND.LANC.LT.LNCSEN)
     &              PLASN = AMAX1(0.0,AMIN1(
     &              (PLA-SENLA)-PLAS,(PLA-SENLA)*LLOSA))
                ENDIF
                PLASS = PLASW + PLASN    ! Loss because of stress

                ! Tiller death - physiological
                TILWTR = 1.0
                TILWT = 0.0
                TNUMD = 0.0
                ! Originally no tdsf
                  TILSW = G3 * (CUMDU+DU/2.0)/PSTART(MSTG)
     &                     * (1.0+((1.0-AMIN1(WFT,NFT))*TDSF))
                ! LAH JAN 2009 ADDED STRESSES TO INCREASE 
                !                        TARGET SIZE->MORE DEATH
                IF (TNUM.GT.0.0)
     &           TILWT = (LFWT+STWT+RSWT+GRWT+CARBOT/2.0)/TNUM
                IF (TILSW.GT.0.0) TILWTR = TILWT/TILSW
                IF (CUMDU+DU.GT.TDPHSDU.AND.CUMDU.LT.TDPHEDU) THEN
                  TNUMD = AMAX1(0.0,
     &             (TNUM-1.)*(1.-TILWTR)*TT/STDAY*(TDFAC/100.0))
                   ! 100.0 because tdfac read as a percentage 
                   ! CSceres TNUMD = AMAX1(0.0,
                   ! (TNUM-1.0)*(1.0-RTSW)*TT*(TDFAC/100.0))
                   ! 
                ENDIF

                ! Leaf senescence when tillers die
                IF (TNUM.GT.0.0) THEN
                  IF (INT(TNUM).EQ.INT(TNUM-(TNUMD+TNUMLOSS))) THEN
                    PLAST1 = (TNUMD+TNUMLOSS)
     &                     * (TLA(INT(TNUM+1.0))-TLAS(INT(TNUM+1.0)))
                  ELSE
                    PLAST1 = (TNUM-INT(TNUM))*TLA(INT(TNUM+1.0))
                    PLAST2 = (TNUMD+TNUMLOSS-(TNUM-INT(TNUM)))
     &                     * (TLA(INT(TNUM+1.0))-TLAS(INT(TNUM+1.0)))
                  ENDIF
                  PLAST = PLAST1 + PLAST2
                ENDIF
                
                ! Leaf senescence - low light at base of canopy
                ! NB. Just senesces any leaf below critical light fr 
                PLASL = 0.0
                IF (LAI.GT.LAIXX) THEN
                 PLASL = (LAI-LAIXX) / (PLTPOP*0.0001)
                ENDIF
              ENDIF

              ! Leaf senescence - overall
              PLAS =  PLASP + PLASI + PLASFS + PLASPM + PLASS + PLASC
     &             + PLAST + PLASL
              ! Overall check to restrict senescence to what available
              PLAS = AMAX1(0.0,AMIN1(PLAS,PLA-SENLA))

!-----------------------------------------------------------------------
!             Calculate C and N made available through senescence
!-----------------------------------------------------------------------

              SENLFG = 0.0
              SENLFGRS = 0.0
              SENNLFG = 0.0
              SENNLFGRS = 0.0
              SENRS = 0.0
              IF (PLA-SENLA.GT.0.0) THEN
                IF (PLASC.GT.0.0) THEN
                  ! If cold kill
                  SENLFG = AMIN1(LFWT,LFWT*PLASC/(PLA-SENLA))
                  SENRS = AMIN1(RSWT,RSWT*PLASC/(PLA-SENLA))
                ELSE
                  ! If normal senescence
                  SENLFG = AMIN1(LFWT*LWLOS,(AMAX1(0.0,         
     &            (LFWT*(PLAS/(PLA-SENLA))*LWLOS))))
                  SENLFGRS = AMIN1(LFWT*(1.0-LWLOS),(AMAX1(0.0,         
     &            (LFWT*(PLAS/(PLA-SENLA))*(1.0-LWLOS)))))
                ENDIF
              ENDIF

              ! Stem senescence
              ! LAH JAN2010 NEED TO MAKE SURE IS LINKED TO ACCELERATED 
              ! SENESCENCE OF LEAVES, AND TO STEM AREA
              !  Start may be accelerated,same rate as normal
              SENSTFR = 0.0
              SENSTG = 0.0
              SENSTGRS = 0.0
              IF (SSPHSDU.GT.0.0.AND.SSPHEDU.GT.0.0) THEN
                IF (CUMDU+DU.GT.SSPHSDU+(FSDU-LSPHSDU)) THEN
                  IF (CUMDU.GT.SSPHS+(FSDU-LSPHSDU)) THEN
!                 IF (CUMDU+DU.GT.SSPHSDU) THEN
!                  IF (CUMDU.GT.SSPHSDU) THEN
                    IF ((SSPHEDU-SSPHSDU).GT.0.0) SENSTFR = 
     &              AMAX1(0.0,AMIN1(1.0,DU/(SSPHEDU-SSPHSDU)))
                  ELSE
                   IF ((SSPHSDU-SSPHEDU).GT.0.0)
     &               SENSTFR = AMAX1(0.0,AMIN1(1.0,
     &                 ((CUMDU+DU)-SSPHSDU)/
     &                             (SSPHEDU-SSPHSDU)))
                  ENDIF
                  ! LAH JAN 2010 NB No weight loss from stem. 
                  ! SENSTFR used currently for area only
                  !IF (SENSTFR.GT.0.0) SENSTG = STWT*SENSTFR
                ENDIF
              ENDIF  
              
              IF (ISWNIT.NE.'N') THEN
                ! NB. N loss has a big effect if low N
                IF (PLASC.GT.0.0) THEN   ! When leaf kill
                  SENNLFG = AMIN1(LEAFN,SENLFG*LANC) ! Loss functional N
                  SENNRS = AMIN1(RSN,RSN*PLASC/(PLA-SENLA)) ! Loss rs N
                ELSE   ! When normal senescence .. N>minimum to reserves
                    SENNLFG = AMAX1(0.0,AMIN1(LEAFN,
     &                             (SENLFG+SENLFGRS)*LNCM))
                    SENNLFGRS = AMAX1(0.0,AMIN1(LEAFN-SENNLFG,
     &                               (SENLFG+SENLFGRS)*(LANC-LNCM)))
                ENDIF

                IF (SANC.GT.0.0) SSENF = (1.0-((SANC-SNCM)/SANC))
                SENNSTG = SENSTG*SANC*SSENF
                SENNSTGRS = SENSTG*SANC*(1.0-SSENF)
                IF (SENNSTG+SENNSTGRS.GT.STEMN) THEN
                  WRITE(Message(1),'(A28)')
     &              'N removal from stem > stem N'
                  CALL WARNING(1,'CSCRP',MESSAGE)
!                  WRITE(Fnumwrk,*)' '
!                  WRITE(Fnumwrk,'(A29)')
!     &              ' N removal from stem > stem N'
                  SENNSTG = STEMN-SENNSTGRS
                ENDIF
              ENDIF

!-----------------------------------------------------------------------
!             Calculate overall senescence loss from tops
!-----------------------------------------------------------------------

              SENFR = 1.0
              SENTOPLITTERG = 0.0
              SENTOPLITTERGGF = 0.0
              IF (DU.GT.0.0) SENFR =
     &         1.0 - AMAX1(0.0,AMIN1(1.0,(CUMDU+DU-LRETSDU)/DU))
              SENTOPLITTERG = (SENLFG+SENSTG+SENRS)*SENFR
              ! Following for checking purposes only
              IF (CUMDU.GE.SGPHASEDU(2).AND.CUMDU.LT.PSTART(MSTG))
     &         SENTOPLITTERGGF = (SENLFG+SENSTG+SENRS)*SENFR

!-----------------------------------------------------------------------
!             Calculate C assimilation at beginning of day
!-----------------------------------------------------------------------

              ! PAR utilization efficiency
              IF (RUESTGDU.GT.0.0) THEN
                IF (CUMDU+DU/2.0.LT.RUESTGDU) THEN
                  PARU = PARUE
                ELSE
                  IF (PARU2.LT.0.0) THEN
                    PARU = PARUE
                  ELSE     
                    ! Following is to make a gradual changeover
                    PARURFR = AMIN1(1.0,(CUMDU+DU/2-RUESTGDU)/150.0)
                    PARU = PARUE+(PARU2-PARUE)*PARURFR
                  ENDIF
                ENDIF
              ELSE
                  PARU = PARUE
              ENDIF  

              ! Conventional method using PAR utilization efficiency (P)
              CARBOTMPR = AMAX1(0.0,(PARMJFAC*SRAD)*PARU*CO2FP*TFP
                ! Note possible change.  LAH
!     &         * WFP * NFP * RSFP * VPDFP * SLPF)
     &         * AMIN1(WFP,NFP,RSFP,VPDFP,SLPF))
              CARBOBEGR = CARBOTMPR * PARI / PLTPOP

              ! Modified conventional using internal CO2 (I)
              CARBOTMP = AMAX1(0.,PARMJFAC*SRAD*PARU*TFP*NFP*RSFP)
              ! Calculate for no water stress for WFPI determination
              CARBOTMPI = CARBOTMP
              CO2INTPPMP = CO2
              DO L = 1,20
                CO2INT = CO2AIR - CARBOTMPI * 
     &          (RATM+RCROP*RLFC/RLF*WFP*(1.0*(1.0-WFP)))*1.157407E-05
                CO2INTPPM = AMAX1(CO2COMPC+20.0,CO2INT *
     &          (8.314*1.0E7*((TMAX+TMIN)*.5+273.))/(1.0E12*1.0E-6*44.))
                CO2FPI = PARFC*
     &           ((1.-EXP(-CO2EX*CO2INTPPM))-(1.-EXP(-CO2EX*CO2COMPC)))
                CARBOTMPI = CARBOTMP * CO2FPI
                IF (ABS(CO2INTPPM-CO2INTPPMP).LT.1.0) EXIT
                CO2INTPPMP = CO2INTPPM
              ENDDO
              CARBOBEGIA = 0.0
              IF (CARBOTMPI.GT.0) CARBOBEGIA =(CARBOTMP*CO2FP)/CARBOTMPI
              CARBOTMPI = CARBOTMP
              CO2INTPPMP = CO2
              DO L = 1,20
                CO2INT = CO2AIR - CARBOTMPI * (RATM+RCROP*RLFC/RLF*WFP*
     &          (1.*(1.-WFP)))*1.157407E-05
                CO2INTPPM = AMAX1(CO2COMPC,CO2INT *
     &          (8.314*1.0E7*((TMAX+TMIN)*0.5+273.))/
     &          (1.0E12*1.0E-6*44.0))
                CO2FPI = PARFC*
     &          ((1.-EXP(-CO2EX*CO2INTPPM))-(1.-EXP(-CO2EX*CO2COMPC)))
                CARBOTMPI = CARBOTMP * CO2FPI
                IF (ABS(CO2INTPPM-CO2INTPPMP).LT.1.0) EXIT
                IF (ABS(CO2INTPPM-CO2COMPC).LT.1.0) EXIT
                CO2INTPPMP = CO2INTPPM
              ENDDO
              CARBOBEGI = CARBOTMPI * SLPF * PARI / PLTPOP * CARBOBEGIA

              ! Alternate method using resistances as per Monteith (M)
              ! Calculate photosynthetic efficiency
              ! Use 10 Mj.m2.d PAR to establish quantum requirement
              PHOTQR = (CO2AIR/(10.0*PARU)-
     &         ((RATM+RCROP)*1.157407E-05))* 
     &         (10.0*30.0)/(CO2AIR*MJPERE) ! 30 = MW Ch2o
              RM = CO2AIR/(((SRAD*PARMJFAC/MJPERE)/PHOTQR)*30.0)
              CARBOTMPM = AMAX1(0.,
     &         (CO2AIR/((RATM+RCROP*RLFC/RLF*WFP*(1.*(1.-WFP)))*
     &         1.157407E-05+RM))*TFP*NFP*RSFP)
              CARBOBEGM = CARBOTMPM * SLPF * PARI / PLTPOP

              ! Select method depending on choice in CONTROL FILE
              IF (MEPHS.EQ.'R') CARBOBEG = CARBOBEGR  ! PAR conversion
              IF (MEPHS.EQ.'M') CARBOBEG = CARBOBEGM  ! Monteith 
              IF (MEPHS.EQ.'I') CARBOBEG = CARBOBEGI  ! Internal CO2

!-----------------------------------------------------------------------
!             C available to roots (minimum) and stem
!-----------------------------------------------------------------------

              IF (PSTART(MSTG).GT.0) THEN
                PTF = AMIN1(PTFMX,
!    &           PTFMN+((PTFMX-PTFMN)*((CUMDU+DU/2.0))/PSTART(MSTG)))
     &           PTFMN+((PTFMX-PTFMN)*
     &                 ((CUMDU+DU/2.0))/PSTART(INT(PTFXS))))
              ELSE
                PTF = (PTFMX+PTFMN)/2.0
              ENDIF
              ! Partition adjustment for stress effects
              PTF = AMIN1(PTFMX,PTF-PTFA*(1.0-AMIN1(WFG,NFG)))
              CARBOR = AMAX1(0.0,(CARBOBEG+CARBOADJ))*(1.0-PTF)
              CARBOT = AMAX1(0.0,(CARBOBEG+CARBOADJ)) - CARBOR

              ! Stem fraction or ratio to leaf whilst leaf still growing
              IF (SWFRX.GT.0.0.AND.SWFRN.GT.0.0) THEN
                ! Increases linearly between specified limits
                SWFR = CSYVAL (LNUM,SWFRNL,SWFRN,SWFRXL,SWFRX)
              ELSE
                IF (CUMDU+DU.GT.SGPHASEDU(1)) THEN
                  ! If stem growth started
                  IF (CUMDU.LT.LGPHASEDU(2).AND.
     &              ABS(LGPHASEDU(2)-SGPHASEDU(1))>1.E-6) THEN 
                    ! Increases linearly from start stem to end leaf
                    IF (CUMDU+DU.LT.LGPHASEDU(2)) THEN
                      SWFR =  AMAX1(0.0,AMIN1(1.0,
     &                 (CUMDU+DU/2.0-SGPHASEDU(1))/
     &                 (LGPHASEDU(2)-SGPHASEDU(1))))
                    ELSE
                      ! Adjust for period when only stem growing
                      SWFR = (SWFRPREV+(1.0-SWFRPREV)/2.0)*
     &                 (LGPHASEDU(2)-CUMDU)/DU
                    ENDIF
                  ELSE
                    ! All to stem after end leaf
                    SWFR = 1.0
                  ENDIF
                ELSE  
                  ! Zero before stem growth starts 
                  SWFR = 0.0
                ENDIF 
              ENDIF

              ! Chaff fraction 
              GROCHFR = 0.0
              GROCH = 0.0
              IF (CHPHASE(1).GT.0.0.AND.CHPHASE(1).LT.9000) THEN
               ! Increases linearly from start chaff to end stem
               IF (CUMDU+DU/2.GT.CHPHASEDU(1)) THEN
                 GROCHFR = CHFR * AMAX1(0.0,AMIN1(1.0,
     &           ((CUMDU+DU/2)-CHPHASE(1))/(CHPHASE(2)-CHPHASE(1))))
                ENDIF
              ENDIF

!-----------------------------------------------------------------------
!                Grain set and potential growth
!-----------------------------------------------------------------------

              GROGRP = 0.0
              GROGRPA = 0.0
              ! If in graingrowth phase
              IF(CUMDU+DU.GT.GGPHASEDU(1))THEN
              ! Just entering lag phase
              IF(CUMDU.LE.GGPHASEDU(1).AND.CUMDU+DU.GE.GGPHASEDU(1))THEN
                IF (DU.GE.0.0) THEN
                  ADAYEFR = (GGPHASEDU(1)-CUMDU)/DU
                ELSE 
                  ADAYEFR = 0.0
                ENDIF 
                GNOPD = GNOWTS*((LFWT+STWT+RSWT)+ADAYEFR*CARBOT)
                ! Based on radiation. Experimental
                IF (GNOWTS.LT.5.0) THEN
                  GNOPD = GNOWTS*SRADPAV(5)
!                  WRITE(FNUMWRK,*)' '
!                  WRITE(FNUMWRK,'(A50)')
!     &             ' GRAIN NUMBER BASED ON RADIATION IN PHASE 5       '
!                WRITE(FNUMWRK,'(A35,3F6.1)')
!     &          '  Radiation average,Coefficient    ',
!     &          sradpav(5),gnowts             
                ENDIF
                
                ! Tiller and canopy weights at grain set
                TILWTGS = TILWT
                CWADGS = CWAD
                
                ! Possible strees factor for grain set
                STRESS20GS = STRESS20N*STRESS20W
                
                ! Kernel number adjustment based on phase(5) radiation 
                IF (SRADPAV(5).GT.GNORT) THEN
                  GNOPAS = GNOPD+GNOPD*((SRADPAV(5)-GNORT)*GNORF)
                ELSE
                  GNOPAS = GNOPD 
                ENDIF  
!                WRITE(FNUMWRK,*)' '
!                WRITE(FNUMWRK,'(A50)')
!     &          ' GRAIN NUMBER ADJUSTMENT                          '
!                WRITE(FNUMWRK,'(A42,F6.2)')
!     &            '  Grain number adjustment factor (fr/MJ)  ',gnorf
!                WRITE(FNUMWRK,'(A42,F6.2)')
!     &            '  Adjustment threshold (MJ/m2.d)          ',gnort
!                WRITE(FNUMWRK,'(A35,3F6.1)')
!     &          '  20 day stress averages N,H2O,Min ',
!     &          stress20n,stress20w,stress20
!                WRITE(FNUMWRK,'(A35,2F6.1)')
!     &          '  Radiation averages,20d,phase5    ',
!     &          srad20,sradpav(5)               
!                WRITE(FNUMWRK,'(A48,2I6)')
!     &          '  Grain#/plant before,after radiation adjustment   '
!     &          ,NINT(GNOPD),NINT(GNOPAS)    
                GNOPD = GNOPAS
                
                ! Potential kernel size adjustment
!                WRITE(FNUMWRK,'(A22)')
!     &          ' GRAIN SIZE ADJUSTMENT'

                IF (TMEANPAV(5).GT.GWTAT) THEN
                  GWTA = GWTS - GWTS*((TMEANPAV(5)-GWTAT)*GWTAF)
                ELSE
                  GWTA = GWTS 
                ENDIF  
                
!                WRITE(FNUMWRK,'(A42,F6.2)')
!     &            '  Grain size adjustment factor (fr/oC)    ',gwtaf
!                WRITE(FNUMWRK,'(A42,F6.2)')
!     &            '  Adjustment threshold (oC)               ',gwtat
!                WRITE(FNUMWRK,'(A42,F6.2)')
!     &            '  Temperature during phase 5 (oC)         ',
!     &            tmeanpav(5)
!                WRITE(FNUMWRK,'(A42,2F6.1)')
!     &            '  Grain size before,after adjustment (mg) '
!     &          ,GWTS,GWTA    
     
                ! Kernel growth rates in lag,linear,end phases
                G2A(1) =(GWTA*GWLAGFR)/(GGPHASEDU(2)-GGPHASEDU(1))
                G2A(2) =(GWTA*(GWLINFR-GWLAGFR))/
     &           (GGPHASEDU(3)-GGPHASEDU(2))
                G2A(3) =(GWTA*(1.0-GWLINFR))/(GGPHASEDU(4)-GGPHASEDU(3))
                G2A(0) = G2A(1)* (CUMDU+DU-GGPHASEDU(1))/DU
              ELSEIF
     &        (CUMDU.GT.GGPHASEDU(1).AND.CUMDU+DU.LE.GGPHASEDU(2))THEN
              ! In lag phase
                G2A(0) = G2A(1)
              ELSEIF
     &         (CUMDU.LE.GGPHASEDU(2).AND.CUMDU+DU.GT.GGPHASEDU(2))THEN
              ! Just entering linear phase
                G2A(0) = G2A(1)*(1.0-(CUMDU+DU-GGPHASEDU(2))/DU) 
     &                 + G2A(2)*(CUMDU+DU-GGPHASEDU(2))/DU!
              ELSEIF
     &        (CUMDU.GT.GGPHASEDU(2).AND.CUMDU+DU.LE.GGPHASEDU(3))THEN
              ! In linear phase
                G2A(0) = G2A(2)
              ELSEIF
     &        (CUMDU.LE.GGPHASEDU(3).AND.CUMDU+DU.GT.GGPHASEDU(3))THEN
              ! Just entering ending phase
                G2A(0) = G2A(2)*(1.0-(CUMDU+DU-GGPHASEDU(3))/DU) 
     &                 + G2A(3)*(CUMDU+DU-GGPHASEDU(3))/DU
              ELSEIF
     &       (CUMDU.GT.GGPHASEDU(3).AND.CUMDU+DU.LE.GGPHASEDU(4))THEN
              ! In ending phase
                G2A(0) = G2A(3)
              ELSEIF
     &        (CUMDU.LE.GGPHASEDU(4).AND.CUMDU+DU.GT.GGPHASEDU(4))THEN
              ! Finishing ending phase
                G2A(0) = G2A(3)*(1.0-(CUMDU+DU-GGPHASEDU(4))/DU) 
              ENDIF

              ! Potential grain growth rate overall 
              GROGRP = GNOPD*G2A(0)*0.001*TFGF*DU
 
              ! Grain growth rate as limited by potential or assimilates
              GROGRA = AMIN1(GROGRP,AMAX1(0.,CARBOT))
              GROGRRS = AMIN1(GROGRP-GROGRA,RSWT)
              GROGRPA = GROGRA + GROGRRS

              ! Record days of stress
              IF (CUMDU+DU.LE.PSTART(MSTG).AND.
     &         CUMDU+DU.GT.PSTART(MSTG-1))THEN
                IF (GROGRPA.LT.GROGRP) CARBOLIM = CARBOLIM+1
                IF (TFGF.LT.1.0) THEN
                  TLIMIT = TLIMIT+1
                ENDIF 
                IF (TMAX.GE.TKGF) THEN
                  CFLFAIL = 'Y'
!                  WRITE(FNUMWRK,'(A47)')
!     &              ' Premature maturity because of high temperature'
!                  WRITE(FNUMWRK,'(A9,F6.1,A13,F6.1)')
!     &              '  TMAX = ',TMAX,' Fail temp = ',TKGF
                ENDIF
              ENDIF
 
              ENDIF
                     
!-----------------------------------------------------------------------
!             Specific leaf area
!-----------------------------------------------------------------------

              ! LAH Not yet implemented
              IF (LAWTR.GT.0.0.AND.LAWTS.GT.0.0) THEN
                TFLAW = 1.0+LAWTR*(TMEAN-LAWTS)
              ELSE
                TFLAW = 1.0
              ENDIF
              ! LAH Not yet implemented
              IF (LAWWR.GT.0.0) THEN
                WFLAW = (1.0-LAWWR)+LAWWR*WFG
              ELSE
                WFLAW = 1.0
              ENDIF

              LAWL(1) = AMAX1(LAWS*LAWFF,LAWS-(LAWS*LAWCF)*(LNUMSG-1))
              LAWL(2) = AMAX1(LAWS*LAWFF,LAWS-(LAWS*LAWCF)*LNUMSG)
              LAWL(1) = LAWL(1) * TFLAW * WFLAW
              LAWL(2) = LAWL(2) * TFLAW * WFLAW

!-----------------------------------------------------------------------
!             Leaf growth
!-----------------------------------------------------------------------

              CARBOLSD = 0.0
              GROLF = 0.0
              GROLFP = 0.0
              GROLS = 0.0
              GROLSP = 0.0
              GROLFRT = 0.0
              GROLFRTN = 0.0
              PLAG = 0.0
              PLAGLF = 0.0
              PLAGT = 0.0
              PLAGTP = 0.0
              PLAGTTEMP = 0.0
              TLAG = 0.0
              TLAGP = 0.0
              LAGEG = 0.0
              
              ! If just ended leaf growth phase
              IF (CUMDU.GT.LGPHASEDU(2).AND.FLDAP.LE.0) THEN
                FLDAP = DAP-1
              ENDIF

              ! If in leaf growth phase
              IF (CUMDU.GE.LGPHASEDU(1).AND.CUMDU.LT.LGPHASEDU(2)) THEN
                ! Potential leaf sizes
                IF (LNUMSG.LT.LNUMX) THEN
                  IF (LAFSWITCH.LE.0) THEN
                    LAPOTX(LNUMSG+1) = LAPOTX(LNUMSG)*(1.0+LAFV)
                  ELSE  
                    LAPOTX(LNUMSG+1) = LAPOTX(LNUMSG)*(1.0+LAFR)
                  ENDIF
                ENDIF    
                IF(LNUMSG .LT. LNUMX) THEN
                  IF (LAPOTX(LNUMSG+1).GT.LAXS) LAPOTX(LNUMSG+1) = LAXS
                ENDIF  

                IF (RSTAGE.GE.LAFST) THEN
                  IF(LNUMSG.GT.0 .AND. LAFSWITCH.LE.0.0) THEN
                  LAFSWITCH = LNUMSG
!                  WRITE(fnumwrk,*) '    '
!                  WRITE(fnumwrk,*)
!     &              'Leaf size increment factor changed ',yeardoy
!                  WRITE(fnumwrk,*) 
!     &             '  Leaf number             ',lafswitch
!                  Lapotxchange = lapotx(lnumsg)
!                  WRITE(fnumwrk,*)
!     &             '  Leaf potential size     ',
!     &              Lapotxchange
!                    LAPOTX(LNUMSG+1) = LAPOTX(LNUMSG)*(1.0+LAFR)
!                  WRITE(fnumwrk,*)
!     &             '  Next leaf potential size',
!     &              Lapotx(lnumsg+1)
                  ENDIF     
                ENDIF
                
                ! If it is the final leaf,next leaf potential = 0
                IF (FLN.GT.0.0.AND.LNUMSG.EQ.INT(FLN+1)) THEN
                  LAPOTX(LNUMSG+1) = 0.0
                ENDIF

                ! Leaf area increase (no assim) - main shoot (tiller 1)
                LNUMNEED = FLOAT(INT(LNUM+1)) - LNUM
                IF (ABS(LNUMNEED).LE.1.0E-6) LNUMNEED = 0.0

                DO L = MAX(1,LNUMSG-INT(LLIFG)),LNUMSG
                  LLIFEG(L) = AMIN1(AMAX1(0.0,LLIFG-LAGEP(L)),LNUMG)
                  PLAGLF(L) = LAPOTX(L)*LLIFEG(L)/LLIFG
     &                      * AMIN1(WFG,NFG)*TFG
                  IF (LNUMG*EMRGFR.GT.0.0) THEN
                    DGLF(L) = DGLF(L)+LLIFEG(L)/LNUMG*EMRGFR
                  ENDIF
                  WFLF(L) = WFLF(L)+WFG*LLIFEG(L)/LLIFG
                  WFLFP(L) = WFLFP(L)+WFP*LLIFEG(L)/LLIFG
                  NFLF(L) = NFLF(L)+NFG*LLIFEG(L)/LLIFG
                  NFLFP(L) = NFLFP(L)+NFP*LLIFEG(L)/LLIFG
                  TFLF(L) = TFLF(L)+TFG*LLIFEG(L)/LLIFG
                  PLAG(1) = PLAG(1) + LAPOTX(L)
     &                    * AMIN1(LLIFEG(L),LNUMNEED)/LLIFG
     &                    * AMIN1(WFG,NFG)*TFG
                  PLAG(2) = AMAX1(0.0,PLAG(2)+(PLAGLF(L)-PLAG(1)))
                ENDDO

                ! New leaf
                IF (LNUMSG.LT.LNUMX) THEN
                L = LNUMSG + 1
                LLIFEG(L) = AMAX1(0.0,(LNUMG-LNUMNEED))
                PLAGLF(L)=LAPOTX(L)*LLIFEG(L)/LLIFG
     &                   * AMIN1(WFG,NFG)*TFG
                IF (LNUMG.GT.0.0) DGLF(L) = DGLF(L)+LLIFEG(L)/LNUMG
                WFLF(L) = WFLF(L)+WFG*LLIFEG(L)/LLIFG
                WFLFP(L) = WFLFP(L)+WFP*LLIFEG(L)/LLIFG
                NFLF(L) = NFLF(L)+NFG*LLIFEG(L)/LLIFG
                NFLFP(L) = NFLFP(L)+NFP*LLIFEG(L)/LLIFG
                TFLF(L) = TFLF(L)+TFG*LLIFEG(L)/LLIFG
                PLAG(2) = PLAG(2) + PLAGLF(L)
                ENDIF

                ! Potential leaf area increase - all tillers
                ! Tilip is the # of phyllochrns that elapse before a 
                ! new tiller reaches its standard size
                TLAGP(1) = PLAG(1)+PLAG(2)
                PLAGTP(1) = PLAG(1)
                PLAGTP(2) = PLAG(2)
                DO L = 2,INT(TNUM+2) ! L is tiller cohort,main=cohort 1
                  IF (TNUM-FLOAT(L-1).GT.0.0) THEN
                    TILIFAC = 1.0
                    IF (TILIP.GT.0) TILIFAC =
     &                           AMIN1(1.0,(LNUM-TILBIRTHL(L))/TILIP)
                    PLAGTP(1) = PLAGTP(1)+PLAG(1)*TGR(L)*TILIFAC
     &                       * AMAX1(0.,AMIN1(FLOAT(L),TNUM)-FLOAT(L-1))
                    PLAGTP(2) = PLAGTP(2)+PLAG(2)*TGR(L)*TILIFAC
     &                       * AMAX1(0.,AMIN1(FLOAT(L),TNUM)-FLOAT(L-1))
                    TLAGP(L) = (PLAG(1)+PLAG(2))*TGR(L)*TILIFAC
     &                       * AMAX1(0.,AMIN1(FLOAT(L),TNUM)-FLOAT(L-1))
                  ENDIF
                ENDDO

                ! Potential leaf weight increase.
                IF (LAWL(1).GT.0.0 .AND. LAWL(2).GT.0.0)
     &           GROLFP = ( PLAGTP(1)/LAWL(1) + PLAGTP(2)/LAWL(2))
     &                  / (1.0-LSHFR)

                ! Potential leaf+stem weight increase.
                IF (SWFR.GT.0.0.AND.SWFR.LT.1.0) THEN
                  GROLSP = GROLFP * (1.0 + SWFR/(1.0-SWFR))
                ELSE
                  GROLSP = GROLFP
                ENDIF

                IF (GROLSP.GT.0.0) THEN
                  ! Leaf+stem weight increase from assimilates
                  GROLS = AMAX1(0.,AMIN1(GROLSP,CARBOT-GROGRPA))
                
                  IF (GROLSP.GT.0.0.AND.GROLS.LT.GROLSP) THEN
                    ! Leaf weight increase from seed reserves
                    ! LAH Takes all that needed. No restriction!
                    CARBOLSD = AMIN1((GROLSP-GROLS),SEEDRSAV)
                    SEEDRSAV = SEEDRSAV - CARBOLSD
                    GROLS = GROLS + CARBOLSD
                  ENDIF
                  ! Leaf weight increase from plant reserves
                  GROLFRS = 0.0
                  IF (GROLS.LT.GROLSP) THEN
                    GROLFRS = AMIN1(RSWT*RSUSE,GROLSP-GROLS)
                    GROLS = GROLS+GROLFRS
                  ENDIF
                  ! Leaf weight increase from roots (eg.,after winter)
                  GROLFRT = 0.0
                  GROLFRTN = 0.0
                  IF (GROLS.LT.GROLSP.AND.SHRTD.LT.1.0.AND.
     &                RTUFR.GT.0.0.AND.ESTABLISHED.EQ.'Y') THEN
                    GROLFRT = AMIN1(RTWT*RTUFR,(GROLSP-GROLS))
                    IF (ISWNIT.NE.'N') THEN
                      GROLFRTN = GROLFRT * RANC
                    ELSE
                      GROLFRTN = 0.0
                    ENDIF  
                    WRITE(Message(1),
     &               '(A16,A12,F3.1,A8,F7.4,A7,F7.4,A9,F7.4)')
     &               'Roots -> leaves ',
     &               ' Shoot/root ',shrtd,
     &               ' Grolsp ',grolsp,' Grols ',grols,
     &               ' Grolfrt ',grolfrt
                    CALL WARNING(1,'CSCRP',MESSAGE)
!                    WRITE(Fnumwrk,*)' '
!                    WRITE(Fnumwrk,'(A39,I7)')
!     &               ' Root material used for leaf growth on ',yeardoy
!                    WRITE(Fnumwrk,
!     &               '(A5,F6.3,A12,F3.1,A8,F7.4,A7,F7.4,A9,F7.4)')
!     &               ' LAI ',lai,
!     &               ' Shoot/root ',shrtd,
!     &               ' Grolsp ',grolsp,' Grols ',grols,
!     &               ' Grolfrt ',grolfrt
                    GROLS = GROLS + GROLFRT
                  ENDIF

                  IF ((GROLSP).GT.0.0) THEN
                    GROLF = GROLS * GROLFP/GROLSP
                  ELSE  
                    GROLF = 0.0
                  ENDIF

                  ! Assimilate factor overall for today
                  !IF (GROLS.LT.(GROLSP*(1.0-LAWFF))) THEN
                  IF (GROLS.LT.GROLSP.AND.GROLSP.GT.0.0) THEN
                    !AFLF(0) = GROLS/(GROLSP*(1.0-LAWFF))
                    AFLF(0) = GROLS/GROLSP
                  ELSE
                    AFLF(0) = 1.0
                  ENDIF

                  ! Assimilate factor average for each leaf
                  DO L = MAX(1,LNUMSG-(INT(LLIFG)+1)),LNUMSG+1
                    IF (LNUMSG.LT.LNUMX) THEN
                      AFLF(L) = AFLF(L)+AFLF(0)*(LLIFEG(L)/LLIFG)
                      PLAGLF(L) = PLAGLF(L) * AFLF(0)
                    ENDIF  
                  ENDDO

                  ! Actual leaf cohort expansion
                  PLAGT(1) = PLAGTP(1)*AFLF(0)
                  PLAGT(2) = PLAGTP(2)*AFLF(0)

                  ! Actual leaf area growth - each tiller
                  DO L = 1,INT(TNUM+1)
                    TLAG(L) = TLAGP(L)*
     &               (PLAGT(1)+PLAGT(2))/(PLAGTP(1)+PLAGTP(2))
                  ENDDO

                ENDIF

              ENDIF
                          
!-----------------------------------------------------------------------
!             Stem and chaff growth
!-----------------------------------------------------------------------

              GROSTP = 0.0
              GROST = 0.0
              STAIG = 0.0
              STAIS = 0.0
              ! Potential stem weight increase.
              IF (SWFR.LT.1.0) THEN
                GROSTP = GROLFP * SWFR/(1.0-SWFR)
                GROSTPSTORE = AMAX1(GROLFP,GROSTPSTORE)
              ELSE  
                GROSTP = GROSTPSTORE
                ! LAH May need to change GROSTP as progress thru phase
              ENDIF
              IF (CUMDU+DU.LE.LGPHASEDU(2)) THEN  
                IF (GROLFP+GROSTP.GT.0.0)
     &           GROST = GROLS * GROSTP/(GROLFP+GROSTP)
              ELSE
                IF (CUMDU+DU.GT.LGPHASEDU(2).AND.
     &           CUMDU+DU.LE.GGPHASEDU(1))
     &           GROST = AMAX1(0.,CARBOT-GROGRPA)*(1.0-(RSPCA/100))
                 ! RSPCA is the % of assim going to reserves
                 ! May need to have this change as stem growth proceeds
              ENDIF
              ! Chaff (In balance with stem growth). 
              ! NB. Chaff is not just part of stem
              GROCH = GROST * GROCHFR
              GROST = GROST * (1.0-GROCHFR)
              ! Visible stem
              STVSTG = 0.0
              STVSTGDU = 0.0
              IF (CUMDU.LE.STVSTGDU.AND.CUMDU+DU.GT.STVSTGDU) THEN
                STVWTG =
     &           GROST*AMAX1(0.0,(AMIN1(1.0,(CUMDU+DU-STVSTG)/DU)))
              ELSEIF (CUMDU.GT.STVSTGDU) THEN
                STVWTG = GROST
              ENDIF

              IF (FSDU.LE.0.0) THEN     ! Visible stem growing
                STAIG = STVWTG*SAWS*PLTPOP*0.0001
              ELSE    ! Visible stem senescing
                IF (CUMDU.LE.FSDU) THEN
                  STAIG = STVWTG*SAWS*PLTPOP*0.0001
                  STAISS = STAI + STAIG
                  IF ((PSTART(MSTG)-FSDU).GT.0.0) THEN
                    STAIS = STAISS*(CUMDU+DU-FSDU)/(PSTART(MSTG)-FSDU)
                    IF (STAIS.LT.1.0E-6) STAIS = 0.0
                  ELSE
                    STAIS = 0.0
                  ENDIF 
                ELSE
                  STAIG = 0.0
                  IF ((PSTART(MSTG)-FSDU).GT.0.0) THEN
                    STAIS = AMIN1(STAI,STAISS*DU/(PSTART(MSTG)-FSDU))
                  ELSE
                    STAIS = 0.0
                  ENDIF  
                ENDIF
                STAIS = AMIN1(STAI,STAISS*SENSTFR)
              ENDIF

!-----------------------------------------------------------------------
!             Reserves growth
!-----------------------------------------------------------------------

              RTWTGRS = 0.0   ! Not calculated until later
              GRORS = 
     &         CARBOT+CARBOLSD+GROLFRT-GROLF-GROST-GROCH-GROGRPA
              ! Check if RSWT -ve (Generally v.small computer error)
              ! If so adjust growth aspect
              RSWTTMP = RSWT+GRORS+GRORSGR-SENRS-RSWPH-RTWTGRS
              IF (RSWTTMP.LT.-1.E-6) THEN ! Reduce growth 
                GROST = AMAX1(0.0,GROST-ABS(RSWTTMP))
                GRORS =
     &           CARBOT+CARBOLSD+GROLFRT-GROLF-GROST-GROCH-GROGRPA
                RSWTTMP = RSWT+GRORS+GRORSGR-SENRS-RSWPH-RTWTGRS
                GROLF = AMAX1(0.0,GROLF-ABS(RSWTTMP))
                GRORS =
     &           CARBOT+CARBOLSD+GROLFRT-GROLF-GROST-GROCH-GROGRPA
                RSWTTMP = RSWT+GRORS+GRORSGR-SENRS-RSWPH-RTWTGRS
                GROGRPA = AMAX1(0.0,GROGRPA-ABS(RSWTTMP))
                GRORS =
     &           CARBOT+CARBOLSD+GROLFRT-GROLF-GROST-GROCH-GROGRPA
                RSWTTMP = RSWT+GRORS+GRORSGR-SENRS-RSWPH-RTWTGRS
              ENDIF
              GRORS = 
     &          CARBOT+CARBOLSD+GROLFRT+SENLFGRS
     &                -GROLF-GROST-GROCH-GROGRPA

              ! Reserves to ROOT if conc too great (overflow!)
              RTWTGRS = 0.0
              ! Determine potential new concentration
              IF (LFWT+GROLF+STWT+CHWT+GROST+GROCH.GT.0.0) TVR1 = ! Conc
     &          (RSWT+GRORS-SENRS)/
     &          ((LFWT+GROLF-SENLFG-SENLFGRS)
     &          +(STWT+GROST+CHWT+GROCH)+(RSWT+GRORS))
              IF(TVR1.LT.0.0.AND.TVR1.GT.-1.0E-07) TVR1 = 0.0
              IF (TVR1.GT.RSPCX/100.0) THEN   ! If potential>max        
                TVR2 = RSWT+GRORS-SENRS       ! What rswt could be
                TVR3 =                        ! What rswt should be 
     &           ((RSPCX/100.0)
     &           *(LFWT+GROLF-SENLFG-SENLFGRS
     &           +STWT+CHWT+GROST+GROCH))
     &           /(1.0-(RSPCX/100.0))
                RTWTGRS = (TVR2 - TVR3) 
                ! Determine FINAL new concentration
                IF (LFWT+GROLF+STWT+CHWT+GROST+GROCH.GT.0.0
     &           .AND. ((LFWT+GROLF-SENLFG-SENLFGRS)
     &            +(STWT+GROST+CHWT+GROCH)
     &            +(RSWT+GRORS-SENRS-RTWTGRS)).GT.0.0) THEN
                !TF - Added protection for division by zero 
                  TVR5 = (RSWT+GRORS-SENRS-RTWTGRS)/
     &            ((LFWT+GROLF-SENLFG-SENLFGRS)
     &            +(STWT+GROST+CHWT+GROCH)
     &            +(RSWT+GRORS-SENRS-RTWTGRS))
                ELSE
                  TVR5 = 0.0
                ENDIF
              ENDIF
              
              IF (RSTAGE.GE.8.2) RTWTGRS = 0.0
              ! No overflow if fillint grain

              GRORSPRM = 0.0
              GRORSPM = 0.0
              IF (LSPHEDU.GT.PSTART(MSTG).AND.
     &         CUMDU+DU.GT.PSTART(MSTG).AND.
     &         CUMDU.LT.PSTART(MSTG)) THEN
                GRORSPRM =
     &           (CARBOT+CARBOLSD-GROLF-GROST)*TIMENEED-GROGRPA
                GRORSPM = GRORS - GRORSPRM
              ELSEIF (CUMDU.GE.PSTART(MSTG)) THEN
                GRORSPM = CARBOPM
              ENDIF

!-----------------------------------------------------------------------
!             Tiller number increase
!-----------------------------------------------------------------------

              TNUMG = 0.0
              IF (LNUM.GT.TI1LF.AND.LNUM.GT.0.0) THEN
                IF (LNUM.LT.ti1lf+3) THEN    ! Fibonacci factors
                 tnumiff=1.0
                ELSEIF(LNUM.GE.ti1lf+3 .AND. LNUM.LT.ti1lf+4) THEN
                 tnumiff=1.5
                ELSEIF(LNUM.GE.ti1lf+4 .AND. LNUM.LT.ti1lf+5) THEN
                 tnumiff = 1.5     ! tnumiff=3.0
                ELSEIF(LNUM.GE.ti1lf+5 .AND. LNUM.LT.ti1lf+6) THEN
                  tnumiff = 1.5     ! tnumiff=4.0
                ELSEIF(LNUM.GE.ti1lf+6 .AND. LNUM.LT.ti1lf+7) THEN
                 tnumiff = 1.5     ! tnumiff=6.0
                ENDIF
                IF ((CUMDU+DU).LT.TILPEDU) THEN
                  TNUMG = DULF/PHINT * TNUMIFF * (AMIN1(WFT,NFT))
                ELSE
                  TNUMG = 0.0
                ENDIF  
              ENDIF
              ! Tillering factor
              TNUMG = TNUMG * TIFAC

!-----------------------------------------------------------------------
!             Height growth
!-----------------------------------------------------------------------

              CANHTG = 0.0
              IF (RSTAGE.LT.7.0) CANHTG = SERX*DU
              !  IF (TT.GT.0.0) CANHTG = 0.5

!=======================================================================
            ENDIF ! End of above-ground growth (after emerged) section
!=======================================================================

!-----------------------------------------------------------------------
!           Root growth and respiration
!-----------------------------------------------------------------------

            RTWTG = 0.0
            RTRESP = 0.0
            RTWTG = (CARBOR+RTWTGRS+SEEDRSAVR)*(1.0-RRESP)
            RTRESP = (CARBOR+RTWTGRS+SEEDRSAVR)*RRESP

            RTWTGL = 0.0
            RTWTSL = 0.0
            RTWTUL = 0.0
            RTNSL = 0.0

            IF (GERMFR.GT.0.0.OR.GESTAGE.GE.0.5) THEN

              ! Establish water factor for root depth growth
              IF (ISWWAT.NE.'N') THEN
                LRTIP = CSIDLAYR (NLAYR, DLAYR, RTDEP) ! Root tip layer
                IF (LRTIP.GT.1) THEN
                  SWPRTIP = SWP(LRTIP)
                ELSE
                  SWPRTIP = AMIN1(SWP(2),
     &             (SWP(2)-((DLAYR(1)-RTDEP)/DLAYR(1))*(SWP(2)-SWP(1))))
                ENDIF
                WFRG = 1.0
                IF (WFRGU.GT.0.0)
     &           WFRG = AMAX1(0.0,AMIN1(1.0,(SWPRTIP/WFRGU)))
              ELSE
                WFRG = 1.0
              ENDIF

              ! Root depth growth
              RTDEPG = 0.0
              IF (ISWWAT.NE.'N') THEN
                ! LAH Note reduced effect of SHF, AND no acceleration
                RTDEPG = TT*RDGS/STDAY*GERMFR
     &                 * SQRT(AMAX1(0.3,SHF(LRTIP)))
     &                 * WFRG
!     &                 * 1.0+AMAX1(0.0,RDGAF*(10.0-WUPR))
              ELSE
                RTDEPG = TT*RDGS/STDAY*GERMFR
              ENDIF
              L = 0
              CUMDEP = 0.0
              RTDEPTMP = RTDEP+RTDEPG
              DO WHILE ((CUMDEP.LE.RTDEPTMP) .AND. (L.LT.NLAYR))
                L = L + 1
                CUMDEP = CUMDEP + DLAYR(L)
                ! LAH Limit on WFRG. 0 WFRG (when 1 layer) -> 0 TRLDF.
                IF (ISWWAT.NE.'N'.AND.WFRGU.GT.0.0) THEN
                  WFRG = AMIN1(1.0,AMAX1(0.1,SWP(L)/WFRGU))
                ELSE
                  WFRG = 1.0
                ENDIF
                IF (ISWNIT.NE.'N'.AND.NCRG.GT.0.0) THEN
                  NFRG = AMIN1(1.0,
     &             AMAX1(0.1,(NO3LEFT(L)+NH4LEFT(L))/NCRG))
                ELSE
                  NFRG = 1.0
                ENDIF 
                ! LAH Tried to use AMAX1 here because layer may have 
                ! lots H20,no N,or inverse, and therefore need roots
                ! But with KSAS8101,AMAX1 lowered yield. Return to AMIN1
                !RLDF(L) = AMAX1(WFRG,NFRG)*SHF(L)*DLAYR(L)
                RLDF(L) = AMIN1(WFRG,NFRG)*SHF(L)*DLAYR(L)
              END DO
              IF (L.GT.0.AND.CUMDEP.GT.RTDEPTMP)
     &         RLDF(L) = RLDF(L)*(1.0-((CUMDEP-RTDEPTMP)/DLAYR(L)))
              NLAYRROOT = L
              ! Root senescence
              SENRTG = 0.0
              SENRTGGF = 0.0
              DO L = 1, NLAYRROOT
                RTWTSL(L) = RTWTL(L)*(RSEN/100.0)*TT/STDAY 
                ! LAH Temperature effect above is not from soil temp
                IF (RTWT.GT.0.0) RTWTUL(L) = RTWTL(L)*GROLFRT/RTWT
                SENRTG = SENRTG + RTWTSL(L)
                IF (ISWNIT.NE.'N') THEN
                  RTNSL(L) = RTWTSL(L)*RANC
                ELSE
                  RTNSL(L) = 0.0
                ENDIF  
              ENDDO
              ! Following for checking purposes only
              IF (CUMDU.GE.SGPHASEDU(2).AND.CUMDU.LT.PSTART(MSTG))
     &         SENRTGGF = SENRTG

              ! Root weight growth by layer
              TRLDF = 0.0
              DO  L = 1, NLAYRROOT
                TRLDF = TRLDF + RLDF(L)
              END DO
              IF (TRLDF.GT.0.0) THEN
                DO  L = 1, NLAYRROOT
                  RTWTGL(L) = (RLDF(L)/TRLDF)*(RTWTG)
                END DO
              ENDIF
            ENDIF

!-----------------------------------------------------------------------
!           Water in profile and rootzone
!-----------------------------------------------------------------------
            
            AH2OPROFILE = 0.0
            H2OPROFILE = 0.0
            AH2OROOTZONE = 0.0
            H2OROOTZONE = 0.0
            DO L = 1, NLAYR
              AH2OPROFILE = AH2OPROFILE+((SW(L)-LL(L))*DLAYR(L))*10.
              H2OPROFILE = H2OPROFILE + SW(L)*DLAYR(L)*10.0
              IF (RLV(L).GT.0.0) THEN
               AH2OROOTZONE=AH2OROOTZONE+((SW(L)-LL(L))*DLAYR(L))*10.
               H2OROOTZONE = H2OROOTZONE+SW(L)*DLAYR(L)*10.
              ENDIF
            END DO

!-----------------------------------------------------------------------
!           Nitrogen movement and uptake
!-----------------------------------------------------------------------

            GRAINNGU = 0.0
            GRAINNGL = 0.0
            GRAINNGR = 0.0
            GRAINNGS = 0.0

            IF (ISWNIT.NE.'N') THEN

              ANDEM = 0.0
              RNDEM = 0.0
              LNDEM = 0.0
              SNDEM = 0.0
              SEEDNUSE = 0.0
              SEEDNUSE2 = 0.0
              RSNUSED = 0.0

              SNO3PROFILE = 0.0
              SNH4PROFILE = 0.0
              SNO3ROOTZONE = 0.0
              SNH4ROOTZONE = 0.0
              TRLV = 0.0
              DO L = 1, NLAYR
                TRLV = TRLV + RLV(L)
                FAC(L) = 10.0/(BD(L)*DLAYR(L))
                SNO3(L) = NO3LEFT(L) / FAC(L)
                SNH4(L) = NH4LEFT(L) / FAC(L)
                SNO3PROFILE = SNO3PROFILE + SNO3(L)
                SNH4PROFILE = SNH4PROFILE + SNH4(L)
                IF (RLV(L).GT.0.0) THEN
                 SNO3ROOTZONE = SNO3ROOT ZONE + SNO3(L)
                 SNH4ROOTZONE = SNH4ROOTZONE + SNH4(L)
                ENDIF
              END DO

              ! Grain N demand
              GRAINNDEM = 0.0
              IF (GNOPD.GT.0.0 .AND. CUMDU.LT.PSTART(MSTG)) GRAINNDEM =
              ! lah working may 2014
     &         AMIN1(GROGRPA*(GNPCMX/100.0),TFGN*GROGRP*(GNPCS/100.))
!    &         AMIN1(GROGRPA*(GNPCMX/100.0),
!    &                GNOPD*G2A(0)*(GNPCS/100.)*0.001*TFGN*DU)

              ! Leaf,stem,root N demand
              LNDEM = GROLF*LNCX +
     &            (LFWT-SENLFG-SENLFGRS)*AMAX1(0.0,NTUPF*(LNCX-LANC)) -
     &            GROLFRTN
              SNDEM = AMAX1(0.0,GROST)*SNCX +
     &              (STWT-SENSTG)*AMAX1(0.0,NTUPF*(SNCX-SANC))
              RNDEM = RTWTG*RNCX + 
     &              (RTWT-SENRTG-GROLFRT)*AMAX1(0.0,NTUPF*(RNCX-RANC))
              
              ! Seed use if no roots
              ! N use same % of initial as for CH20,if needed.
              IF (RTWT.LE.0.0) THEN
                SEEDNUSE = AMAX1(0.0,
     &           AMIN1(SEEDN,LNDEM+SNDEM+RNDEM,SEEDNI/SDDUR*(TT/STDAY)))
              ELSE
                ! Some use of seed (0.5 need) even if may not be needed
                SEEDNUSE = AMAX1(0.0,AMIN1(SEEDN,
     &               0.5*(LNDEM+SNDEM+RNDEM),SEEDNI/SDDUR*(TT/STDAY)))
              ENDIF
              
              ! Reserves used before uptake
              RSNUSED = AMIN1(GRAINNDEM+LNDEM+SNDEM+RNDEM,RSN)

              ! N uptake needed 
              ANDEM = PLTPOP*10.0*
     &        (GRAINNDEM+LNDEM+SNDEM+RNDEM-SEEDNUSE-RSNUSED)

              ! Original from CSM with some 'modification'.  
              ! RNUMX = RTNO3,RTNH4 = N uptake/root length (mgN/cm,.006)
              ! RNO3U,RNH4U  = Nitrogen uptake (kg N/ha)
              ! RNUMX = 0.006    
              WFNU = 1.0
              NUPAP = 0.0
              RNO3U = 0.0
              RNH4U = 0.0
!              IF (CFLNOUTPUTS.EQ.'Y') THEN
!                WRITE(FNUMWRK,*)' '
!                WRITE(FNUMWRK,*)
!     &           'Potential uptake and limitants  ',YearDoy
!                WRITE(FNUMWRK,'(A48,3F7.2)')
!     &           '  Previous day N demand,uptake,shortage (kg/ha) ',
!     &           andem,nupap,AMAX1(0.0,andem-nupap)
!                WRITE(FNUMWRK,'(A48,3F3.2)')
!     $           '  Adjustment factors for Water,N.Conc,Cultivar: ',
!     &           nupwf,nupnf,nupcf
!                WRITE(fnumwrk,'(A38,A39)')
!     &            '   Layer   BLayer Soiln(g/Mg)  (kg/ha)',
!     &            '  Pot.uptake   WFac  NConcFac       RLV'
!              ENDIF
              DO L=1,NLAYR
                IF (RLV(L) .GT. 0.0) THEN
                  NLAYRROOT = L
                  ! N concentration effects
                  FNH4 = 1.0-EXP(-0.08*(1.0+NUPNF) * NH4LEFT(L))
                  FNO3 = 1.0-EXP(-0.08*(1.0+NUPNF) * NO3LEFT(L))
                  ! The following limits are not those in NUPTAK,CERES
                  ! Here set to 0 when NO3,NH4 < minimum,
                  ! In NUPTAK when FNO3,FNH4 < 0.04
                  IF (NO3LEFT(L) .LE. NO3MN) FNO3 = 0.0  
                  IF (FNO3 .GT. 1.0)  FNO3 = 1.0
                  IF (NH4LEFT(L) .LE. NH4MN) FNH4 = 0.0  
                  IF (FNH4 .GT. 1.0)  FNH4 = 1.0
                  ! Water effects
                  IF (SW(L) .LE. DUL(L)) THEN
                    WFNU = (SW(L) - LL(L)) / (DUL(L) - LL(L)) 
                  ELSE
                    WFNU = 1.0-(SW(L)-DUL(L))/(SAT(L)-DUL(L))
                    WFNU = 1.0 ! Wet soil effect not implemented
                  ENDIF
                  IF (WFNU.LT.0.0) WFNU = 0.0
                  ! LAH Note that WFNU squared
                  TVR2 = (1.0+NUPWF)*(WFNU*WFNU)
                  RFAC = RLV(L) * TVR2 * DLAYR(L) * 100.0
                  RNO3U(L) = RFAC * FNO3 * RTNUP*(1.0+NUPCF)
                  RNH4U(L) = RFAC * FNH4 * RTNUP*(1.0+NUPCF)
!                  IF (CFLNOUTPUTS.EQ.'Y') THEN
!                    WRITE(Fnumwrk,'(I7,F10.3,6F10.3)')
!     &               l,blayr(l),(no3left(l)+nh4left(l)),sno3(l)+snh4(l),
!     &               (rno3u(l)+rnh4u(l)),tvr2,fno3,rlv(l) 
!                  ENDIF
                  RNO3U(L) = MAX(0.0,RNO3U(L))
                  RNH4U(L) = MAX(0.0,RNH4U(L))
                  NUPAP = NUPAP + RNO3U(L) + RNH4U(L) !kg[N]/ha
                ENDIF
              ENDDO

              ! Ratio (NUPRATIO) to indicate N supply for output
              IF (ANDEM.GT.0) THEN
                NUPRATIO = NUPAP/ANDEM
              ELSE
                IF (NUPAP.GT.0.0) THEN
                  NUPRATIO = 10.0
                ELSE  
                  NUPRATIO = 0.0
                ENDIF  
              ENDIF
              ! Factor (NUF) to reduce N uptake to level of demand
              NUF = 1.0
              IF (NUPAP.GT.0.0) THEN
                NUF = AMIN1(1.0,ANDEM/NUPAP)
              ENDIF 

              ! Actual N uptake by layer roots based on demand (kg/ha)
              UNO3 = 0.0
              UNH4 = 0.0
              NUPD = 0.0
              NUPAD = 0.0
              DO L = 1, NLAYRROOT
                UNO3(L) = RNO3U(L)*NUF
                UNH4(L) = RNH4U(L)*NUF
                IF (FAC(L).LE.0.0) THEN
                  XMIN = 0.0
                ELSE  
                  XMIN = NO3MN/FAC(L) 
                ENDIF  
                UNO3(L) = MAX(0.0,MIN (UNO3(L),SNO3(L)-XMIN))
                IF (FAC(L).LE.0.0) THEN
                  XMIN = 0.0
                ELSE  
                  XMIN = NH4MN/FAC(L) 
                ENDIF  
                XMIN = NH4MN/FAC(L) 
                UNH4(L) = MAX(0.0,MIN (UNH4(L),SNH4(L)-XMIN))
                NUPAD = NUPAD + UNO3(L) + UNH4(L)                  
              END DO
              IF (PLTPOP > 1.E-6) THEN
                NUPD = NUPAD/(PLTPOP*10.0)
              ELSE
                NUPD = 0.
              ENDIF

              SEEDNUSE2 = 0.0
              ! Seed use after using reserves and uptake
              ! (Assumes all seed gone by time of grain filling)
              IF (RTWT.GT.0.0.AND.ISWNIT.NE.'N') THEN
                SEEDNUSE2 = AMAX1(0.0,
     &           AMIN1(SEEDN-SEEDNUSE,GRAINNDEM+LNDEM+SNDEM+
     &           RNDEM-RSNUSED-SEEDNUSE-NUPD,SEEDNI/SDDUR*(TT/STDAY)))
              ELSE
                SEEDNUSE2 = 0.0
              ENDIF
              SEEDNUSE = SEEDNUSE + SEEDNUSE2

              ! N available for distribution
              NULEFT = SEEDNUSE+RSNUSED+NUPD

              ! Distribute N to grain,leaves,stem,root
              LNUSE = 0.0
              SNUSE = 0.0
              RNUSE = 0.0
              ! 1.For grain
              GRAINNGU = AMIN1(NULEFT,GRAINNDEM)
              NULEFT = NULEFT - GRAINNGU
              ! 2.For new leaf at minimum
              LNUSE(1) = AMIN1(NULEFT,GROLF*LNCM)
              NULEFT = NULEFT - LNUSE(1)
              ! 3.For new root at minimum
              RNUSE(1) = AMIN1(NULEFT,RTWTG*RNCM)
              NULEFT = NULEFT - RNUSE(1) 
              ! 4.For new stem at minimum     
              SNUSE(1) = AMIN1(NULEFT,GROST*SNCM)
              NULEFT = NULEFT - SNUSE(1)
              ! 5.For leaf growth and topping-up (N to leaves first)
              LNUSE(2) = AMIN1(NULEFT,LNDEM-LNUSE(1))  
              NULEFT = NULEFT - LNUSE(2)
              ! 6.For distribution between root,stem
              IF (NULEFT.GT.0.0.AND.
     &         SNDEM-SNUSE(1)+RNDEM-RNUSE(1).GT.0.0) THEN
                IF (NULEFT.GE.
     &           (SNDEM-SNUSE(1))+(RNDEM-RNUSE(1))) THEN
                  SNUSE(2) = SNDEM-SNUSE(1)
                  RNUSE(2) = RNDEM-RNUSE(1)
                  NULEFT = NULEFT - SNUSE(2) - RNUSE(2)
                ELSE 
                  SNUSE(2) = NULEFT * (SNDEM-SNUSE(1))/
     &                   ((SNDEM-SNUSE(1))+(RNDEM-RNUSE(1)))
                  RNUSE(2) = NULEFT * (RNDEM-RNUSE(1))/
     &                   ((SNDEM-SNUSE(1))+(RNDEM-RNUSE(1)))
                  NULEFT = NULEFT - SNUSE(2) - RNUSE(2)
                ENDIF   
              ENDIF
              LNUSE(0) = LNUSE(1) + LNUSE(2) 
              SNUSE(0) = SNUSE(1) + SNUSE(2)
              RNUSE(0) = RNUSE(1) + RNUSE(2) 

              ! N Pools available for re-mobilization
              ! (Labile N increases during grain fill)
              ! LAH Nuselim allows labile to go to 100%!
              !IF (RSTAGE.GE.8.0.AND.RSTAGE.LE.9.0) THEN
              !  NUSELIM = AMIN1(1.0,RSTAGE-8.0)
              !  NUSEFAC = AMAX1(NUSELIM,(NLABPC/100.0))
              !ELSE  
              NUSEFAC = NLABPC/100.0
              !ENDIF
              NPOOLR = AMAX1 (0.0,
     &         ((RTWT-SENRTG)*(RANC-RNCM)*NUSEFAC))
              NPOOLL = AMAX1 (0.0, 
     &            ((LFWT-SENLFG-SENLFGRS)*(LANC-LNCM)*NUSEFAC))
              NPOOLS = AMAX1 (0.0,
     &         ((STWT-SENSTG)*(SANC-SNCM)*NUSEFAC))

              ! Move N to grain from tops if necessary (not from roots)
              GRAINNGR = 0.0
              GRAINNGL = 0.0
              GRAINNGS = 0.0
              GRAINNDEMLSR = AMAX1
     &           (0.0,(GRAINNDEM-GRAINNGRS-GRAINNGU))
              ! Draw N from stems,then leaves.
              !GRAINNGR = AMIN1(NPOOLR,GRAINNDEMLSR) Taken out RORO7491
              GRAINNGS = AMIN1(NPOOLS,GRAINNDEMLSR-GRAINNGR)
              GRAINNGL = AMIN1(NPOOLL,
     &                            GRAINNDEMLSR-GRAINNGR-GRAINNGS)

              ! Move N to stem from leaves if needed to keep stem at min
              STEMNGL = 0.0
              IF (CUMDU.LT.SGPHASEDU(2)) THEN
                IF (LANC.GT.LNCM.AND.SANC.LT.SNCM) THEN
                  STEMNGL = AMIN1(0.01*LFWT*(LANC-LNCM),
     &            ((SNCM-SANC)*(STWT+GROST)-STEMN-SNUSE(0)))
                  ! The 0.01 is the fraction of leaf N that is available
                  ! to try to maintain stem N at the minimum     
!                  IF (STEMNGL.GT.0.0) THEN 
!                    WRITE(fnumwrk,'(A34,F7.3)')
!     &              ' N moved from leaves to stem      ',stemngl    
!                  ENDIF
                ENDIF 
              ENDIF

            ENDIF

!-----------------------------------------------------------------------
!           Actual grain growth
!-----------------------------------------------------------------------

            GRORSGR = 0.0
            IF (ISWNIT.EQ.'N') THEN
              GROGR = GROGRPA
            ELSE
              IF (GNPCMN.LE.0.0) THEN
                GROGR = GROGRPA
              ELSE
                ! Minimum grain N% control
                GRWTTMP = GRWT + GROGRPA
                GRAINNTMP = GRAINN + (GRAINNGU+GRAINNGR+GRAINNGL+
     &            GRAINNGS+GRAINNGRS)
                IF (GRWTTMP > 1.E-6) THEN
                  ! TF - new if statment to protect from divisions by 0
                  ! on GRAINNTMP/GRWTTMP
                  IF (GRAINNTMP/GRWTTMP*100.0 .LT. GNPCMN) THEN
                    GRWTTMP = GRAINNTMP*(100.0/GNPCMN)
                  ENDIF
                  GROGR = GRWTTMP - GRWT
                  GRORSGR = GROGRPA - GROGR
                  NLIMIT = NLIMIT + 1
                ELSE
                  GROGR = GROGRPA
                ENDIF
              ENDIF
            ENDIF

!-----------------------------------------------------------------------
!           Rate variables expressed on an area basis
!-----------------------------------------------------------------------

            ! C assimilation
            ! Senesced material added to litter or soil
            SENWALG = 0.0
            SENNALG = 0.0
            SENCALG = 0.0
            SENLALG = 0.0
            SENWAGS = 0.0
            SENCAGS = 0.0
            SENLAGS = 0.0
            SENNAGS = 0.0
            SENWALG(0) = SENTOPLITTERG * PLTPOP*10.0
            SENCALG(0) = SENWALG(0) * 0.4 
            SENLALG(0) =
     &       (SENLFG*LLIGPC/100+SENSTG*SLIGPC/100) * PLTPOP*10.0
            SENNALG(0) = (SENNLFG+SENNSTG) * SENFR * PLTPOP*10.0
            ! Root senescence
            DO L = 1, NLAYR
              SENWALG(L) = RTWTSL(L) * PLTPOP*10.0
              SENNALG(L) = RTNSL(L) * PLTPOP*10.0
              SENCALG(L) = SENWALG(L) * 0.4
              SENLALG(L) = SENWALG(L) * RLIGPC/100.0
              SENWAGS = SENWAGS + SENWALG(L)
              SENCAGS = SENCAGS + SENCALG(L)
              SENLAGS = SENLAGS + SENLALG(L)
              SENNAGS = SENNAGS + SENNALG(L)
            ENDDO

            ! Set established flag (Used to determine if to fail when
            ! seed reserves used)
            IF (ESTABLISHED.NE.'Y'.AND.LAI.GT.0.0) ESTABLISHED = 'Y'

!=======================================================================
          ENDIF  ! End of after germinated section
!=======================================================================
!=======================================================================
        ENDIF  ! End of after planted (rate) section
!=======================================================================

      END SUBROUTINE CRP_Growth
