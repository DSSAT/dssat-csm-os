!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.RATE) lines 4075 - 4215 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in Module_CSCAS_Vars_List. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine CS_Growth_Germ calculates the water and thermal conditions for initiation of growth of the planting material 
! (called germination).
!***************************************************************************************************************************

    SUBROUTINE CS_Growth_Germ ( & 
        ALBEDO      , BRSTAGE     , CLOUDS      , CO2         , DLAYR       , DUL         , EO          , EOP         , &
        ES          , ISWWAT      , KEP         , LL          , NLAYR       , RLV         , RWUMX       , RWUPM       , &
        SAT         , SRAD        , SW          , TAIRHR      , TDEW        , TMAX        , TMIN        , TRWUP       , &
        UH2O        , WEATHER     , WINDSP      , YEAR        & 
        )
        
        USE ModuleDefs
        USE Module_CSCAS_Vars_List
        
        IMPLICIT NONE
        
        TYPE (WeatherType) WEATHER                                                            ! Defined in ModuleDefs

        INTEGER NLAYR       , YEAR         
        INTEGER CSIDLAYR                                                                      ! Integer function call.
        
        REAL    ALBEDO      , BRSTAGE     , CLOUDS      , CO2         , DLAYR(NL)   , DUL(NL)     , EO          , EOP         
        REAL    ES          , KEP         , LL(NL)      , RLV(NL)     , RWUMX       , RWUPM       , SAT(NL)     , SRAD        
        REAL    SW(NL)      , TAIRHR(24)  , TDEW        , TMAX        , TMIN        , TRWUP       , UH2O(NL)    , WINDSP      
        REAL    CSVPSAT     , TFAC4                                                           ! Real function call.
        
        CHARACTER(LEN=1) IDETG       , ISWWAT      
        
          IF (PLYEAR.LE.0) PLYEAR = YEAR

!-----------------------------------------------------------------------
!         Calculate potential plant evaporation,water uptake if neeeded
!-----------------------------------------------------------------------

          ! EO is brought into the module. The following calculations
          ! (apart from the root water uptake module) are for 
          ! comparative purposes only. The root water uptake module 
          ! is not necessary when running in CSM, but is necessary for
          ! CROPSIM.

          ! Co2 effect on stomatal resistances. General for C3 crops 
          RLF  = 9.72 + 0.0757 * 330.0 + 10.0
          RLFC = 9.72 + 0.0757 *  CO2  + 10.0                                                                          !EQN 139
                                                         
          IF (FILEIOT.EQ.'DS4'.AND.IDETG.NE.'N'.OR.FILEIOT.NE.'DS4') THEN                  

            IF (ISWWAT.NE.'N') THEN    
            
            ! Call 1  Basic calculations with rcrop = 0  
            CALL EVAPO('M',SRAD,CLOUDS,TMAX,TMIN,TDEW,WINDSP,ALBEDO,RATM,RCROP*0.0, &
                EO,EOPEN,EOMPEN,EOPT,EOEBUD,TCAN,'M')
            EOPENC = EOPENC + EOPEN 
            EOPTC = EOPTC + EOPT
            EOMPENC = EOMPENC + EOMPEN
            EOEBUDC = EOEBUDC + EOEBUD
             
            ! CSM with LAI=1.0,CROP=WH,TAVG=20.0,WINDSP=86.4 has
            ! RATM = 55  RCROP = 45
            ! Monteith had RATM = 300, RCROP = 150->500
            
            ! Call 2  Using rcrop as read-in from spe file
            CALL EVAPO('M',SRAD,CLOUDS,TMAX,TMIN,TDEW,WINDSP,ALBEDO,RATM,RCROP, &
                EO,EOPEN,EOMPCRP,EOPT,EOEBUDCRP,TCAN,'M')
            EOMPCRPC = EOMPCRPC + EOMPCRP
            EOEBUDCRPC = EOEBUDCRPC + EOEBUDCRP

            ! Call 3 Using rcrop adjusted for CO2 effect     
            IF (RLF.GT.0.0)CALL EVAPO('M',SRAD,CLOUDS,TMAX,TMIN,TDEW,WINDSP,ALBEDO,RATM,RCROP*RLFC/RLF, &
                EO,EOPEN,EOMPCRPCO2,EOPT,EOEBUDCRPCO2,TCAN,'M')
            EOMPCRPCO2C = EOMPCRPCO2C + EOMPCRPCO2
            EOEBUDCRPCO2C = EOEBUDCRPCO2C + EOEBUDCRPCO2

            ! Transpiration ratio (Pot.pl.evap/Pot.soil evap)
            EPSRATIO = 1.0
            IF (EOMPEN.GT.0.0) EPSRATIO = EOMPCRPCO2 / EOMPEN                                                          !EQN 140
            TRATIO = 1.0
            IF (EOMPCRP.GT.0.0) TRATIO = EOMPCRPCO2 / EOMPCRP                                                          !EQN 141
            ENDIF
            
            IF (fileiot(1:2).NE.'DS') THEN
              ! Calculate plant potential evaporation 
              EOP = MAX(0.0,EO/EOMPEN*EOMPCRPCO2 * (1.0-EXP(-LAI*KEP)))
              ! Ratio necessary because EO method may not be Monteith
              CALL CSCRPROOTWU(ISWWAT,NLAYR, DLAYR, LL, SAT, WFEU, MEWNU,EOP, RLV, RWUPM, RLFWU, RWUMX, RTDEP, &
                  SW, WTDEP, uh2o, trwup, trwu)
            ENDIF
            
            ! Call 4 Using rcrop adjusted for CO2 & H2O effect     
            ! NB. Using previous days WFP. 
            ! If use this for other than comparison, must check  LAH
            IF (RLF.GT.0.0.AND.WFP.LE.1.0.AND.WFP.GT.0.0) THEN
             CALL EVAPO('M',SRAD,CLOUDS,TMAX,TMIN,TDEW,WINDSP,ALBEDO,RATM,RCROP*RLFC/RLF*(1.0+(1.0-WFP)), &            !EQN 142
                 EO,EOPEN,EOMPCRPCO2H2O,EOPT,EOEBUDCRPCO2H2O,TCAN,'M')
            ELSE
              WRITE(FNUMWRK,*)' WFP OUT OF RANGE! = ',WFP
            ENDIF
            EOMPCRPCO2H2OC = EOMPCRPCO2H2OC + EOMPCRPCO2H2O
            EOEBUDCRPCO2H2OC = EOEBUDCRPCO2H2OC + EOEBUDCRPCO2H2O
            
            ! Call 5 to calcuate canopy temperature
            TVR1 = (TRWU*10.0+ES)
            CALL EVAPO('M',SRAD,CLOUDS,TMAX,TMIN,TDEW,WINDSP,ALBEDO,RATM,RCROP*RLFC/RLF*(1.0+(1.0-WFP)), &
                EO,tvr1,tvr2,tvr3,tvr4,TCAN,'C')
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

          Tfd = TFAC4(trdv1,tmean,TT)                                                                                  !EQN 036
          IF (brstage+1.0.LT.10.0) &
           Tfdnext = TFAC4(trdv2,tmean,TTNEXT)
          IF (trgem(3).GT.0.0) THEN
            Tfgem = TFAC4(trgem,tmean,TTGEM)
          ELSE
            Ttgem = tt
          ENDIF    
          IF (Cfllflife.EQ.'D') THEN
            ! Leaf life read-in as days (eg.7 phyllochrons->7 days)
            Ttlflife = Phints   
          ELSE  
            Tflflife = TFAC4(trdv1,tmean,TTlflife)
          ENDIF  
  
!-----------------------------------------------------------------------
!         Calculate soil water 'status' (Used as a sw 'potential')
!-----------------------------------------------------------------------

          DO L = 1,NLAYR
            SWP(L) = &                                                                                                 !EQN 044b
             AMIN1(1.0,AMAX1(.0,((SW(L)-LL(L))/(DUL(L)-LL(L)))))
          ENDDO
          
!-----------------------------------------------------------------------
!         Calculate water factor for germination
!-----------------------------------------------------------------------

          WFGE = 1.0
          IF (ISWWAT.NE.'N') THEN
            IF (GESTAGE.LT.1.0) THEN
              IF (LSEED.LT.0) LSEED = CSIDLAYR (NLAYR, DLAYR, SDEPTH)
              IF (LSEED.GT.1) THEN
                SWPSD = SWP(LSEED)
              ELSE
               SWP(0) = AMIN1(1.,AMAX1(.0,(SWP(1)-0.5*(SWP(2)-SWP(1)))))                                               !EQN 044b
               SWPSD = SWP(0) + (SDEPTH/DLAYR(1))*(SWP(2)-SWP(0))                                                      !EQN 043
              ENDIF
              IF (WFGEM.GT.0.0) &
               WFGE = AMAX1(0.0,AMIN1(1.0,(SWPSD/WFGEM)))                                                              !EQN 042
            ENDIF
          ENDIF
          IF (ISWWATCROP.EQ.'N') WFGE = 1.0
          
      END SUBROUTINE CS_Growth_Germ