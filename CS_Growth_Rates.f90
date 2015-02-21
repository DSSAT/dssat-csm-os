<<<<<<< HEAD
!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.RATE) lines 4385 - 4537 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module CS_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine CS_Growth_Rates calcualates PAR interception and rate factors.
!
!   NOTE: The hidden code contains MF/JC's VPD response. After checking that the packaged code gives identical results to 
!   the original, re-enable and evaluate the hidden code,
!***************************************************************************************************************************
    
    SUBROUTINE CS_Growth_Rates ( &
        CO2         , EOP         , ISWDIS      , ISWNIT      , ISWWAT      , KCAN        , NFP         , PARIP       , &
        PARIPA      , TDEW        , TMAX        , TMIN        , TRWUP       & 
        )
    
        USE CS_First_Trans_m
    
        IMPLICIT NONE
        
        REAL    CO2         , EOP         , KCAN        , NFP         , PARIP       , PARIPA      , TDEW        , TMAX        
        REAL    TMIN        , TRWUP       
        REAL    CSVPSAT     , TFAC4       , YVALXY                              ! Real function calls
        
        CHARACTER(LEN=1) ISWDIS      , ISWNIT      , ISWWAT      

!-----------------------------------------------------------------------
!           Check for or calculate PAR interception at start of day
!-----------------------------------------------------------------------

            PARI = 0.0
            PARI1 = (1.0 - EXP((-KCAN)*LAI))                                                                           !EQN 260
            IF (PARIP.GT.0.0) THEN
              ! From competition model
              IF (ISWDIS(LENDIS:LENDIS).NE.'N') THEN
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
            IF (MEPHO.EQ.'M') CARBOEND = CARBOTMPM * PARI/PLTPOP                                                       !EQN 277
            IF (MEPHO.EQ.'I') CARBOEND = CARBOTMPI * PARI/PLTPOP                                                       !EQN 277
            IF (MEPHO.EQ.'R') CARBOEND = CARBOTMPR * PARI/PLTPOP                                                       !EQN 277

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
            IF (ISWWAT.NE.'N') THEN
              IF (EOP.GT.0.0) THEN
                WUPR = TRWUP/(EOP*0.1)                                                                                 !EQN 146
                IF (WFGU-WFGL.GT.0.0) &
                 WFG = AMAX1(0.0,AMIN1(1.0,(WUPR-WFGL)/(WFGU-WFGL)))                                                   !EQN 147
                IF (WFPU-WFPL.GT.0.0) &
                 WFP = AMAX1(0.0,AMIN1(1.0,(WUPR-WFPL)/(WFPU-WFPL)))                                                   !EQN 145
              ENDIF
              IF (ISWWATEARLY.EQ.'N') THEN
                WFG = 1.0
                WFP = 1.0
              ENDIF
            ENDIF

            ! Nitrogen
            ! WARNING No N stress after emergence on day that emerges
            IF (ISWNIT.NE.'N') THEN
              IF (LFWT.GT.1.0E-5) THEN
                !NFG =AMIN1(1.0,AMAX1(0.0,(LANC-LNCGL)/(LNCGU-LNCGL)))
                LNCGL = LNCM + NFGL * (LNCX-LNCM)                                                                      !EQN 164
                LNCGU = LNCM + NFGU * (LNCX-LNCM)                                                                      !EQN 165
                IF (LNCGU - LNCGL > 1.E-6) THEN
                 NFG =AMIN1(1.0,AMAX1(0.0,(LANC-LNCGL)/(LNCGU-LNCGL)))                                                 !EQN 163
                ELSE
                 NFG = 1.0 
                ENDIF
                LNCPL = LNCM + NFPL * (LNCX-LNCM)
                LNCPU = LNCM + NFPU * (LNCX-LNCM)
                IF (LNCPU - LNCPL > 1.E-6) THEN                                                                        !EQN 167
                 NFP =AMIN1(1.0,AMAX1(0.0,(LANC-LNCPL)/(LNCPU-LNCPL)))                                                 !EQN 166
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
            IF (ISWNITEARLY.EQ.'N') THEN
              NFG = 1.0
              NFP = 1.0  
            ENDIF

            ! Reserves
            IF (RSFPU.GT.0.0.AND.RSFPU.GT.0.0) THEN
              RSFP = 1.-AMIN1(1.,AMAX1(0.,(RSCD-RSFPL)/(RSFPU-RSFPL)))                                                 !EQN 260
            ELSE
              RSFP = 1.0
            ENDIF

            ! Temperature
            ! LAH No cold night effect.
            ! Maybe,one cold night --> reduced phs next day!!
            ! May want to introduce:
            ! IF (TMEAN20.LT.0.0) TFG = 0.0
            ! IF (TMEAN20.LT.0.0) TFP = 0.0
            Tfp = TFAC4(trphs,tmean,TTOUT)                                                                             !EQN 056
            Tfg = TFAC4(trlfg,tmean,TTOUT)                                                                             !EQN 058
            IF (CFLTFG.EQ.'N') TFG = 1.0

            ! Vapour pressure
            VPDFP = 1.0
            IF (PHTV.GT.0.0) THEN
              IF (TDEW.LE.-98.0) TDEW = TMIN
              VPD = CSVPSAT(tmax) - CSVPSAT(TDEW)    ! Pa                                                              !EQN 262
              IF (VPD/1000.0.GT.PHTV) &
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
            !  IF (TEMPM .LT. SenCritTemp) THEN
            !     Life(I) = Life(I)-SenTempFac*(SenCritTemp-TEMPM)
            !  ENDIF
            !  IF (CumLAI .GT. SenCritLai) THEN
            !     Life(I) = Life(I)-SenLaiFac*(CumLAI-SenCritLai)
            !  ENDIF
            !  IF (Life(I) .LT. 0.0) Life(I) = 0.0
            !  LSCL   0.4  Leaf senescence,critical LAI
            !  LSCT  18.0  Leaf senescence,critical temperature (C)
            !  LSSL  0.05  Leaf senescence,sensitivity to LAI
            !  LSST   0.3  Leaf senescence,sensitivity to temp (fr/d)

!-----------------------------------------------------------------------
!           Calculate leaf number at end of day;adjust PHINT if needed
!-----------------------------------------------------------------------
                                               
            LAGEG = 0.0
            LNUMG = 0.0
            ! Reduce PHINT with development stage (LAH Sept 2012)
            PHINT = 1.0/((1.0/PHINTS)*(1.0-PHINTFAC*DSTAGE))                                                           !EQN 003
            !LNUMEND = LNUM + (TT*EMRGFR)/PHINT !LPM 17MY14  FLN and LNUMEND are not used, deleted 
            !
            !! Restrict to maximum
            !LNUMEND = AMIN1(FLOAT(LNUMX),LNUMEND)
            !IF(FLN.GT.0.0) LNUMEND = AMIN1(FLN,LNUMEND)
            !!LNUMG = LNUMEND - LNUM
            LNUMG = (TT*EMRGFR)/PHINT                                                                                  !EQN 347
        
    END SUBROUTINE CS_Growth_Rates
    
=======
!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.RATE) lines 4372 - 4524 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in Module_CSCAS_Vars_List. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine CS_Growth_Rates calcualates PAR interception and rate factors.
!
!   NOTE: The hidden code contains MF/JC's VPD response. After checking that the packaged code gives identical results to 
!   the original, re-enable and evaluate the hidden code,
!***************************************************************************************************************************
    
    SUBROUTINE CS_Growth_Rates ( &
        CO2         , EOP         , ISWDIS      , ISWNIT      , ISWWAT      , KCAN        , NFP         , PARIP       , &
        PARIPA      , TDEW        , TMAX        , TMIN        , TRWUP       & 
        )
    
        USE ModuleDefs
        USE Module_CSCAS_Vars_List
    
        IMPLICIT NONE
        
        REAL    CO2         , EOP         , KCAN        , NFP         , PARIP       , PARIPA      , TDEW        , TMAX        
        REAL    TMIN        , TRWUP       
        REAL    CSVPSAT     , TFAC4       , YVALXY                              ! Real function calls
        
        CHARACTER(LEN=1) ISWDIS      , ISWNIT      , ISWWAT      
        !!-----------------------------------------------------------------------
        !!           Check for or calculate PAR interception at start of day
        !!-----------------------------------------------------------------------
        !
        !PARI = 0.0
        !PARI1 = (1.0 - EXP((-KCAN)*LAI))
        !IF (PARIP.GT.0.0) THEN
        !    ! From competition model
        !    IF (ISWDIS(LENDIS:LENDIS).NE.'N') THEN
        !        PARI = PARIPA/100.0
        !    ELSE
        !        PARI = PARIP/100.0
        !    ENDIF
        !ELSE
        !    PARI = PARI1! LAH For row crops may need to change 
        !    ! In original Ceres maize, kcan is calculated as:
        !    ! 1.5 - 0.768*((rowspc*0.01)**2*pltpop)**0.1
        !    ! eg. 1.5 - 0.768*((75*0.01)**2*6.0)**0.1  =  0.63
        !ENDIF
        !
        !!-----------------------------------------------------------------------
        !!           Calculate adjustment to yesterday's C assimilation
        !!-----------------------------------------------------------------------
        !
        !! End of day interception = today's starting interception
        !IF (MEPHO.EQ.'M') CARBOEND = CARBOTMPM * PARI/PLTPOP
        !IF (MEPHO.EQ.'I') CARBOEND = CARBOTMPI * PARI/PLTPOP
        !IF (MEPHO.EQ.'R') CARBOEND = CARBOTMPR * PARI/PLTPOP
        !
        !CARBOADJ = (CARBOEND-CARBOBEG)/2.0*EMRGFRPREV
        !! But note, no adjustment if leaf kill
        !PARMJIADJ = PARMJFAC*SRADPREV*(PARI-PARIPREV)/2.0*EMRGFR
        !
        !!-----------------------------------------------------------------------
        !!           Calculate process rate factors
        !!-----------------------------------------------------------------------
        !
        !! Water
        !! No water stress after emergence on day that emerges
        !WFG = 1.0
        !WFP = 1.0
        !IF (ISWWAT.NE.'N') THEN
        !    IF (EOP.GT.0.0) THEN
        !        WUPR = TRWUP/(EOP*0.1)
        !        IF (WFGU-WFGL.GT.0.0) WFG = AMAX1(0.0,AMIN1(1.0,(WUPR-WFGL)/(WFGU-WFGL)))
        !        IF (WFPU-WFPL.GT.0.0) WFP = AMAX1(0.0,AMIN1(1.0,(WUPR-WFPL)/(WFPU-WFPL)))
        !    ENDIF
        !    IF (ISWWATEARLY.EQ.'N') THEN
        !        WFG = 1.0
        !        WFP = 1.0
        !    ENDIF
        !ENDIF
        !
        !! Nitrogen
        !! WARNING No N stress after emergence on day that emerges
        !IF (ISWNIT.NE.'N') THEN
        !    IF (LFWT.GT.1.0E-5) THEN
        !        !NFG =AMIN1(1.0,AMAX1(0.0,(LANC-LNCGL)/(LNCGU-LNCGL)))
        !        LNCGL = LNCM + NFGL * (LNCX-LNCM)
        !        LNCGU = LNCM + NFGU * (LNCX-LNCM)
        !        IF (LNCGU - LNCGL > 1.E-6) THEN
        !            NFG =AMIN1(1.0,AMAX1(0.0,(LANC-LNCGL)/(LNCGU-LNCGL)))
        !        ELSE
        !            NFG = 1.0 
        !        ENDIF
        !        LNCPL = LNCM + NFPL * (LNCX-LNCM)
        !        LNCPU = LNCM + NFPU * (LNCX-LNCM)
        !        IF (LNCPU - LNCPL > 1.E-6) THEN
        !            NFP =AMIN1(1.0,AMAX1(0.0,(LANC-LNCPL)/(LNCPU-LNCPL)))
        !        ELSE
        !            NFP = 1.0 
        !        ENDIF
        !    ELSE
        !        NFG = 1.0
        !        NFP = 1.0  
        !    ENDIF
        !ELSE  
        !    NFG = 1.0
        !    NFP = 1.0  
        !ENDIF
        !
        !! If N stress switched off early in cycle. 
        !IF (ISWNITEARLY.EQ.'N') THEN
        !    NFG = 1.0
        !    NFP = 1.0  
        !ENDIF
        !
        !! Reserves
        !IF (RSFPU.GT.0.0.AND.RSFPU.GT.0.0) THEN
        !    RSFP = 1.-AMIN1(1.,AMAX1(0.,(RSCD-RSFPL)/(RSFPU-RSFPL)))
        !ELSE
        !    RSFP = 1.0
        !ENDIF
        !
        !! Temperature
        !! LAH No cold night effect.
        !! Maybe,one cold night --> reduced phs next day!!
        !! May want to introduce:
        !! IF (TMEAN20.LT.0.0) TFG = 0.0
        !! IF (TMEAN20.LT.0.0) TFP = 0.0
        !Tfp = TFAC4(trphs,tmean,TTOUT)
        !Tfg = TFAC4(trlfg,tmean,TTOUT)
        !IF (CFLTFG.EQ.'N') TFG = 1.0
        !
        !!! Vapour pressure                                                           ! MF 14SE14 Code for VPD reponse moved to CS_Photo for hourly response.
        !!VPDFP = 1.0                                                                 ! MF 14SE14 This code snippet was to calculate and overall VPD respomse, which is not how cassava woeks.
        !!IF (PHTV.GT.0.0) THEN
        !!    IF (TDEW.LE.-98.0) TDEW = TMIN
        !!    VPD = CSVPSAT(tmax) - CSVPSAT(TDEW)    ! Pa 
        !!    IF (VPD/1000.0.GT.PHTV) VPDFP = AMAX1(0.0,1.0+PHSV*(VPD/1000.0-PHTV))
        !!ENDIF
        !
        !
        !! CO2 factor using look-up table
        !CO2FP = YVALXY(CO2RF,CO2F,CO2)
        !! Co2 factor using CROPGRO formula
        !! CO2EX Exponent for CO2-PHS relationship (0.05)  
        !! COCCC CO2 compensation concentration (80 vpm)
        !! CO2FP = 
        !!   PARFC*((1.-EXP(-CO2EX*CO2))-(1.-EXP(-CO2EX*CO2COMPC)))
        !
        !!  LAH Notes from original cassava model                                                          
        !!  IF (TEMPM .LT. SenCritTemp) THEN
        !!     Life(I) = Life(I)-SenTempFac*(SenCritTemp-TEMPM)
        !!  ENDIF
        !!  IF (CumLAI .GT. SenCritLai) THEN
        !!     Life(I) = Life(I)-SenLaiFac*(CumLAI-SenCritLai)
        !!  ENDIF
        !!  IF (Life(I) .LT. 0.0) Life(I) = 0.0
        !!  LSCL   0.4  Leaf senescence,critical LAI
        !!  LSCT  18.0  Leaf senescence,critical temperature (C)
        !!  LSSL  0.05  Leaf senescence,sensitivity to LAI
        !!  LSST   0.3  Leaf senescence,sensitivity to temp (fr/d)
        !
        !!-----------------------------------------------------------------------
        !!           Calculate leaf number at end of day;adjust PHINT if needed
        !!-----------------------------------------------------------------------
        !                                       
        !LAGEG = 0.0
        !LNUMG = 0.0
        !! Reduce PHINT with development stage (LAH Sept 2012)
        !PHINT = 1.0/((1.0/PHINTS)*(1.0-PHINTFAC*DSTAGE))
        !!LNUMEND = LNUM + (TT*EMRGFR)/PHINT !LPM 17MY14  FLN and LNUMEND are not used, deleted 
        !!
        !!! Restrict to maximum
        !!LNUMEND = AMIN1(FLOAT(LNUMX),LNUMEND)
        !!IF(FLN.GT.0.0) LNUMEND = AMIN1(FLN,LNUMEND)
        !!!LNUMG = LNUMEND - LNUM
        !LNUMG = (TT*EMRGFR)/PHINT
!-----------------------------------------------------------------------
!           Check for or calculate PAR interception at start of day
!-----------------------------------------------------------------------

            PARI = 0.0
            PARI1 = (1.0 - EXP((-KCAN)*LAI))                                                                           !EQN 260
            IF (PARIP.GT.0.0) THEN
              ! From competition model
              IF (ISWDIS(LENDIS:LENDIS).NE.'N') THEN
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
            IF (MEPHO.EQ.'M') CARBOEND = CARBOTMPM * PARI/PLTPOP                                                       !EQN 277
            IF (MEPHO.EQ.'I') CARBOEND = CARBOTMPI * PARI/PLTPOP                                                       !EQN 277
            IF (MEPHO.EQ.'R') CARBOEND = CARBOTMPR * PARI/PLTPOP                                                       !EQN 277

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
            IF (ISWWAT.NE.'N') THEN
              IF (EOP.GT.0.0) THEN
                WUPR = TRWUP/(EOP*0.1)                                                                                 !EQN 146
                IF (WFGU-WFGL.GT.0.0) &
                 WFG = AMAX1(0.0,AMIN1(1.0,(WUPR-WFGL)/(WFGU-WFGL)))                                                   !EQN 147
                IF (WFPU-WFPL.GT.0.0) &
                 WFP = AMAX1(0.0,AMIN1(1.0,(WUPR-WFPL)/(WFPU-WFPL)))                                                   !EQN 145
              ENDIF
              IF (ISWWATEARLY.EQ.'N') THEN
                WFG = 1.0
                WFP = 1.0
              ENDIF
            ENDIF

            ! Nitrogen
            ! WARNING No N stress after emergence on day that emerges
            IF (ISWNIT.NE.'N') THEN
              IF (LFWT.GT.1.0E-5) THEN
                !NFG =AMIN1(1.0,AMAX1(0.0,(LANC-LNCGL)/(LNCGU-LNCGL)))
                LNCGL = LNCM + NFGL * (LNCX-LNCM)                                                                      !EQN 164
                LNCGU = LNCM + NFGU * (LNCX-LNCM)                                                                      !EQN 165
                IF (LNCGU - LNCGL > 1.E-6) THEN
                 NFG =AMIN1(1.0,AMAX1(0.0,(LANC-LNCGL)/(LNCGU-LNCGL)))                                                 !EQN 163
                ELSE
                 NFG = 1.0 
                ENDIF
                LNCPL = LNCM + NFPL * (LNCX-LNCM)
                LNCPU = LNCM + NFPU * (LNCX-LNCM)
                IF (LNCPU - LNCPL > 1.E-6) THEN                                                                        !EQN 167
                 NFP =AMIN1(1.0,AMAX1(0.0,(LANC-LNCPL)/(LNCPU-LNCPL)))                                                 !EQN 166
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
            IF (ISWNITEARLY.EQ.'N') THEN
              NFG = 1.0
              NFP = 1.0  
            ENDIF

            ! Reserves
            IF (RSFPU.GT.0.0.AND.RSFPU.GT.0.0) THEN
              RSFP = 1.-AMIN1(1.,AMAX1(0.,(RSCD-RSFPL)/(RSFPU-RSFPL)))                                                 !EQN 260
            ELSE
              RSFP = 1.0
            ENDIF

            ! Temperature
            ! LAH No cold night effect.
            ! Maybe,one cold night --> reduced phs next day!!
            ! May want to introduce:
            ! IF (TMEAN20.LT.0.0) TFG = 0.0
            ! IF (TMEAN20.LT.0.0) TFP = 0.0
            Tfp = TFAC4(trphs,tmean,TTOUT)                                                                             !EQN 056
            Tfg = TFAC4(trlfg,tmean,TTOUT)                                                                             !EQN 058
            IF (CFLTFG.EQ.'N') TFG = 1.0

            ! Vapour pressure
            VPDFP = 1.0
            IF (PHTV.GT.0.0) THEN
              IF (TDEW.LE.-98.0) TDEW = TMIN
              VPD = CSVPSAT(tmax) - CSVPSAT(TDEW)    ! Pa                                                              !EQN 262
              IF (VPD/1000.0.GT.PHTV) &
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
            !  IF (TEMPM .LT. SenCritTemp) THEN
            !     Life(I) = Life(I)-SenTempFac*(SenCritTemp-TEMPM)
            !  ENDIF
            !  IF (CumLAI .GT. SenCritLai) THEN
            !     Life(I) = Life(I)-SenLaiFac*(CumLAI-SenCritLai)
            !  ENDIF
            !  IF (Life(I) .LT. 0.0) Life(I) = 0.0
            !  LSCL   0.4  Leaf senescence,critical LAI
            !  LSCT  18.0  Leaf senescence,critical temperature (C)
            !  LSSL  0.05  Leaf senescence,sensitivity to LAI
            !  LSST   0.3  Leaf senescence,sensitivity to temp (fr/d)

!-----------------------------------------------------------------------
!           Calculate leaf number at end of day;adjust PHINT if needed
!-----------------------------------------------------------------------
                                               
            LAGEG = 0.0
            LNUMG = 0.0
            ! Reduce PHINT with development stage (LAH Sept 2012)
            PHINT = 1.0/((1.0/PHINTS)*(1.0-PHINTFAC*DSTAGE))                                                           !EQN 003
            !LNUMEND = LNUM + (TT*EMRGFR)/PHINT !LPM 17MY14  FLN and LNUMEND are not used, deleted 
            !
            !! Restrict to maximum
            !LNUMEND = AMIN1(FLOAT(LNUMX),LNUMEND)
            !IF(FLN.GT.0.0) LNUMEND = AMIN1(FLN,LNUMEND)
            !!LNUMG = LNUMEND - LNUM
            LNUMG = (TT*EMRGFR)/PHINT                                                                                  !EQN 347
        
    END SUBROUTINE CS_Growth_Rates
    
>>>>>>> cassava-modifications
