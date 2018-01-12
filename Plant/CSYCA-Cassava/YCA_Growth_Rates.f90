!***************************************************************************************************************************
! This is the code from the section (DYNAMIC == RATE) lines 4385 - 4537 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine YCA_Growth_Rates calcualates PAR interception and rate factors.
!
!   NOTE: The hidden code contains MF/JC's VPD response. After checking that the packaged code gives identical results to 
!   the original, re-enable and evaluate the hidden code,
!***************************************************************************************************************************
    
    SUBROUTINE YCA_Growth_Rates ( &
        CO2         , EOP         , ISWDIS      , ISWNIT      , ISWWAT      , KCAN        , NFP         , PARIP       , &
        PARIPA      , TDEW        , TMAX        , TMIN        , TRWUP       , RLV         , SRAD        , SLPF  &                                !LPM 26MAR2016 RLV added
        )
        USE ModuleDefs
        USE YCA_First_Trans_m
        USE YCA_Control_Photosyntesis
        USE YCA_Control_Environment

        IMPLICIT NONE
        
        
        REAL    CO2         , EOP         , KCAN        , NFP         , PARIP       , PARIPA      , TDEW        , TMAX        
        REAL    TMIN        , TRWUP       , RLV(NL)     , SRAD        , SLPF
        REAL    CSVPSAT     , TFAC4       , YVALXY                                    ! Real function calls !LPM 19SEP2017 Added tfac5
        REAL    availableCH2O
        
        CHARACTER(LEN=1) ISWDIS      , ISWNIT      , ISWWAT      

!-----------------------------------------------------------------------
!           Check for or calculate PAR interception at start of day
!-----------------------------------------------------------------------

            PARI = 0.0
            PARI1 = calculatePortionOfRadiation(KCAN, LAI)                                                                           !EQN 260
            IF (PARIP > 0.0) THEN
              ! From competition model
              IF (ISWDIS(LENDIS:LENDIS) /= 'N') THEN
                PARI = PARIPA/100.0
              ELSE
                PARI = PARIP/100.0
              ENDIF
            ELSE
              PARI = PARI1                                                                                             !EQN 260
              ! LAH For row crops may need to change 
              ! In original Ceres maize, kcan is calculated as:
              ! 1.5 - 0.768*((rowspc*0.01)**2*pltpop)**0.1
              ! eg. 1.5 - 0.768*((75*0.01)**2*6.0)**0.1  =  0.63
            ENDIF

!-----------------------------------------------------------------------
!           Calculate adjustment to yesterday's C assimilation
!-----------------------------------------------------------------------

        ! End of day interception = today's starting interception
        select case(MEPHO)
            case ('R')
                availableCH2O = availableCarbohydrate_methodR(PARMJFAC, SRAD, PARU, CO2FP, TFP, RSFP, VPDFP, SLPF, PARI, PLTPOP)
            case ('I')
                availableCH2O = availableCarbohydrate_methodI(CO2, CO2AIR, CO2EX, CO2FP, CO2COMPC, PARMJFAC, PARFC, PARI, PARU, PLTPOP, RATM, RCROP, RLFC, RLF, RSFP, SLPF, SRAD, TMAX, TMIN, TFP, WFP)
            case ('M')
                availableCH2O = availableCarbohydrate_methodM(CO2AIR,PARU, RATM, RCROP,RLFC, RLF, WFP, MJPERE, PARMJFAC, SRAD, TFP, RSFP, SLPF, PARI, PLTPOP)
            case ('V')
                availableCH2O = availableCarbohydrate_methodV(TMin, TMax, TDEW, SRAD, PHTV, PHSV, KCANI, LAI, PARUE)
        end select
        CARBOEND = availableCH2O
            
            CARBOADJ = (CARBOEND-CARBOBEG)/2.0*EMRGFRPREV                                                              !EQN 278
            ! But note, no adjustment if leaf kill
            PARMJIADJ = PARMJFAC*SRADPREV*(PARI-PARIPREV)/2.0*EMRGFR                                                   !EQN 279

!-----------------------------------------------------------------------
!           Calculate process rate factors
!-----------------------------------------------------------------------

            ! Water
            ! No water stress after emergence on day that emerges
            WFG = 1.0
            WFP = 1.0
            IF (ISWWAT /= 'N') THEN !LPM 25MAR2016 Water stress modified by the readily available water SWP
              !IF (EOP > 0.0) THEN
              !  WUPR = TRWUP/(EOP*0.1)                                                                                 !EQN 146
              !  IF (WFGU-WFGL > 0.0) &
              !   WFG = AMAX1(0.0,AMIN1(1.0,(WUPR-WFGL)/(WFGU-WFGL)))                                                   !EQN 147
              !  
              !  IF (WFPU-WFPL > 0.0) &
              !   WFP = AMAX1(0.0,AMIN1(1.0,(WUPR-WFPL)/(WFPU-WFPL)))                                                   !EQN 145
              !ENDIF
                RAW = 0.0
                TRLV = 0.0
                DO L = 1, NLAYRROOT
                    RAW = RAW + (SWP(L)*DLAYRTMP(L)*RLV(L))
                    TRLV = TRLV + (RLV(L)*DLAYRTMP(L))
                ENDDO
                IF (EMRGFR >= 1.0) THEN
                    IF (TRLV > 0.0) THEN
                        RAW = RAW/(TRLV)
                    ELSE
                        !RAW = 0.0 !LPM 11JUL2017 to avoid RAW of 0 with the roots just start to growth at planting
                        RAW = SWP(LSEED)
                    ENDIF
                    !Linear decrease according SWP
                    IF (WFGU-WFGL > 0.0) &
                        WFG = AMAX1(0.0,AMIN1(1.0,(RAW-WFGL)/(WFGU-WFGL)))                                                   !EQN 147
                    IF (WFPU-WFPL > 0.0) &
                        WFP = AMAX1(0.0,AMIN1(1.0,(RAW-WFPL)/(WFPU-WFPL)))                                                   !EQN 145
                    
                    !Fix factor of 0.5 when SWP<0.5
                    !IF (RAW <= 0.5) THEN
                    !    IF (WFGU-WFGL > 0.0) &
                    !        WFG = 0.5                                                   !EQN 147
                    !    IF (WFPU-WFPL > 0.0) &
                    !        WFP = 0.5                                                 !EQN 145
                    !ELSE
                    !    WFG = 1.0
                    !    WFP = 1.0 
                    !ENDIF
                    
                    IF (ISWWATEARLY == 'N') THEN
                        WFG = 1.0
                        WFP = 1.0
                    ENDIF
                ENDIF
            ENDIF

            
            ! Nitrogen
            ! WARNING No N stress after emergence on day that emerges
            IF (ISWNIT /= 'N') THEN
              IF (LFWT > ZERO) THEN
                !NFG =AMIN1(1.0,AMAX1(0.0,(LANC-LNCGL)/(LNCGU-LNCGL)))
                LNCGL = LNCM + NFGL * (LNCX-LNCM)                                                                      !EQN 164
                LNCGU = LNCM + NFGU * (LNCX-LNCM)                                                                      !EQN 165
                IF (LNCGU - LNCGL > ZERO) THEN
                 !NFG =AMIN1(1.0,AMAX1(0.0,(LANC-LNCGL)/(LNCGU-LNCGL)))                                                 !EQN 163 !LPM 02SEP2016 To keep NFG as NFLF2
                 NFG = node(0,0)%NFLF2                                                 !EQN 163
                ELSE
                 NFG = 1.0 
                ENDIF
                LNCPL = LNCM + NFPL * (LNCX-LNCM)
                LNCPU = LNCM + NFPU * (LNCX-LNCM)
                IF (LNCPU - LNCPL > ZERO) THEN                                                                        !EQN 167
                 !NFP =AMIN1(1.0,AMAX1(0.0,(LANC-LNCPL)/(LNCPU-LNCPL)))                                                 !EQN 166 !LPM 02SEP2016 Use NFLF2 intead of original equation
                 NFP =node(0,0)%NFLF2 
                ELSE
                 NFP = 1.0 
                ENDIF
              ELSE
                NFG = 1.0
                NFP = 1.0  
              ENDIF
            ELSE  
              NFG = 1.0
              NFP = 1.0  
            ENDIF

            ! If N stress switched off early in cycle. 
            IF (ISWNITEARLY == 'N') THEN
              NFG = 1.0
              NFP = 1.0  
            ENDIF

            ! Reserves
            IF (RSFPU > 0.0.AND.RSFPU > 0.0) THEN
              RSFP = 1.-AMIN1(1.,AMAX1(0.,(RSCD-RSFPL)/(RSFPU-RSFPL)))                                                 !EQN 260
            ELSE
              RSFP = 1.0
            ENDIF

            ! Temperature
            ! LAH No cold night effect.
            ! Maybe,one cold night --> reduced phs next day!!
            ! May want to introduce:
            ! IF (TMEAN20 < 0.0) TFG = 0.0
            ! IF (TMEAN20 < 0.0) TFP = 0.0
            Tfp = TFAC4(trphs,tmean,TTOUT)                                                                             !EQN 056
            !Tfg = TFAC4(trlfg,tmean,TTB)                                                                               !EQN 058 LPM 21MAR15 TTB will be used to determine DU and branches
            Tfg = calculateTemperatureFactor(trdv3,tmean,TTL)                                                                               !EQN 058 LPM 19APR2016 TTB will be estimated using trbrg 
            IF (CFLTFG == 'N') TFG = 1.0
            Tfb = TFAC4(trbrg,tmean,TTB)                                                                               !EQN 058 LPM 19APR2016 TTB will be estimated using trbrg 
            ! Vapour pressure
            VPDFP = 1.0
            IF (PHTV > 0.0) THEN
              IF (TDEW <= -98.0) TDEW = TMIN
              VPD = CSVPSAT(tmax) - CSVPSAT(TDEW)    ! Pa                                                              !EQN 262
              IF (VPD/1000.0 > PHTV) &
               VPDFP = AMAX1(0.0,1.0+PHSV*(VPD/1000.0-PHTV))                                                           !EQN 263
            ENDIF

            ! CO2 factor using look-up table
            CO2FP = YVALXY(CO2RF,CO2F,CO2)
            ! Co2 factor using CROPGRO formula
            ! CO2EX Exponent for CO2-PHS relationship (0.05)  
            ! COCCC CO2 compensation concentration (80 vpm)
            ! CO2FP = 
            !   PARFC*((1.-EXP(-CO2EX*CO2))-(1.-EXP(-CO2EX*CO2COMPC)))

            !  LAH Notes from original cassava model                                                          
            !  IF (TEMPM  <  SenCritTemp) THEN
            !     Life(I) = Life(I)-SenTempFac*(SenCritTemp-TEMPM)
            !  ENDIF
            !  IF (CumLAI  >  SenCritLai) THEN
            !     Life(I) = Life(I)-SenLaiFac*(CumLAI-SenCritLai)
            !  ENDIF
            !  IF (Life(I)  <  0.0) Life(I) = 0.0
            !  LSCL   0.4  Leaf senescence,critical LAI
            !  LSCT  18.0  Leaf senescence,critical temperature (C)
            !  LSSL  0.05  Leaf senescence,sensitivity to LAI
            !  LSST   0.3  Leaf senescence,sensitivity to temp (fr/d)

!-----------------------------------------------------------------------
!           Calculate leaf number at end of day
!-----------------------------------------------------------------------
            
            
            !LAGEG = 0.0      !LPM28MAR15 This variable is not used
            LNUMG = 0.0
            ! Reduce PHINT with development stage (LAH Sept 2012)
            !PHINT = 1.0/((1.0/PHINTS)*(1.0-PHINTFAC*DSTAGE))                                                          !EQN 003 !LPM 21MAY2015 PHINT is not used
            !LNUMEND = LNUM + (TT*EMRGFR)/PHINT !LPM 17MY14  FLN and LNUMEND are not used, deleted 
            !
            !! Restrict to maximum
            !LNUMEND = AMIN1(FLOAT(LNUMX),LNUMEND)
            !IF(FLN > 0.0) LNUMEND = AMIN1(FLN,LNUMEND)
            !!LNUMG = LNUMEND - LNUM
            !LNUMG = (TT*EMRGFR)/PHINT                                                                                  !EQN 347
            !LPM 24MAR2016 
            IF (DAE > 0) THEN
                IF (ISWWAT == 'Y') THEN
                    LNUMG = ((1.048488E6*LNSLP)/((((3.5986E3))+DAWWP)**2))*(TT*WFG)                                      !LPM 31JUL2015 to consider water stress
                ELSE
                    LNUMG = ((1.048488E6*LNSLP)/(((3.5986E3)+TTCUM)**2))*TT                                              !LPM 21/02/2015 leaf number curve
                ENDIF
            ELSEIF (DAG > 0) THEN
                IF (ISWWAT == 'Y') THEN
                    LNUMG = ((1.048488E6*LNSLP)/((((3.5986E3))+DAWWP)**2))*(TTGEM*WFG)                                      !LPM 31JUL2015 to consider water stress
                ELSE
                    LNUMG = ((1.048488E6*LNSLP)/(((3.5986E3)+TTCUM)**2))*TTGEM                                              !LPM 21/02/2015 leaf number curve
                ENDIF
            ENDIF
    END SUBROUTINE YCA_Growth_Rates
    
