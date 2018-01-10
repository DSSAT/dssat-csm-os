!***************************************************************************************************************************
! This module is intended to calculate photosynthesis effects in the plant
! 12/09/2017 converted from UTF-8 to ANSI
! Atributes:
!   
! Object functions:
!        
! Static functions:
!        
! Authors
! @danipilze
!*********

    Module YCA_Control_Photosyntesis !Module of environment

    contains
    
    
    !-------------------------------------------
    ! STATIC FUNCTIONS
    !-------------------------------------------
    

    ! Conventional method using PAR utilization efficiency (P)
    real function availableCarbohydrate_methodR(PARMJFAC, SRAD, PARU, CO2FP, TFP, RSFP, VPDFP, SLPF, PARI, PLTPOP)
        implicit none
        real, intent (in) :: PARMJFAC, SRAD, PARU, CO2FP, TFP, RSFP, VPDFP, SLPF, PARI, PLTPOP
        real :: CARBOTMPR = 0
        
        
        !CARBOTMPR = 0.0
        !DO L = 1, TS
        !    CARBOTMPRHR(L) = AMAX1(0.0,(PARMJFAC*RADHR(L)*3.6/1000.)*PARU*CO2FP*TFP* WFP * NFP * RSFP * VPDFPHR(L) * SLPF)       ! MF 17SE14 RADHR is in J/m2/s. Multiply by 3600 for hour, divide by 10^6 for MJ.
        !    CARBOTMPR = CARBOTMPR + CARBOTMPRHR(L)
        !END DO
        !  
        !CARBOTMPR = AMAX1(0.0,(PARMJFAC*SRAD)*PARU*CO2FP*TFP* WFP * NFP * RSFP * VPDFP * SLPF) !LPM 02SEP2016 Deleted WFP and NFP 
        CARBOTMPR = AMAX1(0.0,(PARMJFAC*SRAD)*PARU*CO2FP*TFP* RSFP * VPDFP * SLPF)
        
        availableCarbohydrate_methodR = CARBOTMPR * PARI / PLTPOP																		   !EQN 259
    end function availableCarbohydrate_methodR
        
    ! Modified conventional using internal CO2 (I)
    real function availableCarbohydrate_methodI(CO2, CO2AIR, CO2EX, CO2FP, CO2COMPC, PARMJFAC, PARFC, PARI, PARU, PLTPOP, RATM, RCROP, RLFC, RLF, RSFP, SLPF, SRAD, TMAX, TMIN, TFP, WFP)
        implicit none
        real, intent (in) :: CO2, CO2AIR, CO2EX, CO2FP, CO2COMPC, PARMJFAC, PARFC, PARI, PARU, PLTPOP, RATM, RCROP, RLFC, RLF, RSFP, SLPF, SRAD, TMAX, TMIN, TFP, WFP
        real :: CARBOTMP, CARBOTMPI, CO2INTPPM, CO2INTPPMP, CO2INT, CO2FPI, CARBOBEGIA  ! temp variables
        integer :: L !                                                                  ! temp variables
        real :: CARBOBEGI ! result

        
            !CARBOTMP = AMAX1(0.,PARMJFAC*SRAD*PARU*TFP*NFP*RSFP)                                                       !EQN 264 !LPM 02SEP2016 Deleted WFP and NFP 
            CARBOTMP = AMAX1(0.,PARMJFAC*SRAD*PARU*TFP*RSFP)                                                       !EQN 264
        ! Calculate for no water stress for WFPI determination
        CARBOTMPI = CARBOTMP
        CO2INTPPMP = CO2
        DO L = 1,20
            CO2INT = CO2AIR - CARBOTMPI * (RATM+RCROP*RLFC/RLF*WFP*(1.0*(1.0-WFP)))*1.157407E-05						!EQN 265
            CO2INTPPM = AMAX1(CO2COMPC+20.0,CO2INT *(8.314*1.0E7*((TMAX+TMIN)*.5+273.))/(1.0E12*1.0E-6*44.))			!EQN 269
            CO2FPI = PARFC*((1.-EXP(-CO2EX*CO2INTPPM))-(1.-EXP(-CO2EX*CO2COMPC)))										!EQN 268
            CARBOTMPI = CARBOTMP * CO2FPI		
            IF (ABS(CO2INTPPM-CO2INTPPMP) < 1.0) EXIT
            CO2INTPPMP = CO2INTPPM
        ENDDO
        CARBOBEGIA = 0.0
            IF (CARBOTMPI > 0) CARBOBEGIA =(CARBOTMP*CO2FP)/CARBOTMPI                                                 !EQN 270
        CARBOTMPI = CARBOTMP
        CO2INTPPMP = CO2
        DO L = 1,20
            CO2INT = CO2AIR - CARBOTMPI * (RATM+RCROP*RLFC/RLF*WFP*(1.*(1.-WFP)))*1.157407E-05
            CO2INTPPM = AMAX1(CO2COMPC,CO2INT *(8.314*1.0E7*((TMAX+TMIN)*0.5+273.))/(1.0E12*1.0E-6*44.0))				!EQN 271
            CO2FPI = PARFC*((1.-EXP(-CO2EX*CO2INTPPM))-(1.-EXP(-CO2EX*CO2COMPC)))
            CARBOTMPI = CARBOTMP * CO2FPI
            !---- DA 25APR2017 is this used at all?
            IF (ABS(CO2INTPPM-CO2INTPPMP) < 1.0) EXIT
            IF (ABS(CO2INTPPM-CO2COMPC) < 1.0) EXIT
            CO2INTPPMP = CO2INTPPM
            !----
            
        ENDDO
            CARBOBEGI = CARBOTMPI * SLPF * PARI / PLTPOP * CARBOBEGIA                                                  !EQN 272
        
        availableCarbohydrate_methodI = CARBOBEGI

    end function availableCarbohydrate_methodI
    
    
    ! Alternate method using resistances as per Monteith (M)
    ! Calculate photosynthetic efficiency
    ! Use 10 Mj.m2.d PAR to establish quantum requirement
    real function availableCarbohydrate_methodM(CO2AIR,PARU, RATM, RCROP,RLFC, RLF, WFP, MJPERE, PARMJFAC, SRAD, TFP, RSFP, SLPF, PARI, PLTPOP)
        implicit none
        real, intent (in) :: CO2AIR,PARU, RATM, RCROP,RLFC, RLF, WFP, MJPERE, PARMJFAC, SRAD, TFP, RSFP, SLPF, PARI, PLTPOP
        real :: PHOTQR, RM
        real :: CARBOTMPM = 0
        

        PHOTQR = (CO2AIR/(10.0*PARU)-((RATM+RCROP*RLFC/RLF*WFP*(1.*(1.-WFP)))*1.157407E-05))*(10.0*30.0)/(CO2AIR*MJPERE) ! 30 = MW Ch2o!EQN 273
        RM = CO2AIR/(((SRAD*PARMJFAC/MJPERE)/PHOTQR)*30.0)                                                               !EQN 274
        !CARBOTMPM = AMAX1(0.,(CO2AIR/((RATM+RCROP*RLFC/RLF*WFP*(1.*(1.-WFP)))*1.157407E-05+RM))*TFP*NFP*RSFP)           !EQN 275 !LPM 02SEP2016 Deleted NFP
        CARBOTMPM = AMAX1(0.,(CO2AIR/((RATM+RCROP*RLFC/RLF*WFP*(1.*(1.-WFP)))*1.157407E-05+RM))*TFP*RSFP)                    !EQN 275
        
        availableCarbohydrate_methodM = CARBOTMPM * SLPF * PARI / PLTPOP																	 !EQN 276

    end function availableCarbohydrate_methodM
    
    ! Alternate method V
    real function availableCarbohydrate_methodV(TMin, TMax, TDEW, SRAD, PHTV, PHSV, KCANI, LAI, PARUE)
        USE YCA_Control_Environment
        USE YCA_Control_VPDEffect
        implicit none
        real, intent (in) :: TMin, TMax, TDEW, SRAD, PHTV, PHSV, KCANI, LAI, PARUE
        type (DailyEnvironment_type)                     :: env
        type (VPDEffect_type)                            :: vpde
        real hourlyStomatalConductance
        real dailyBiomass
        integer I

        env = DailyEnvironment_type(TMin, TMax, TDEW, SRAD)
        vpde = VPDEffect_type(PHTV, PHSV)
        dailyBiomass = 0.0
        DO I = 1, 24
            hourlyStomatalConductance = vpde%affectStomatalConductance(env%hourlyVPD(I))
            dailyBiomass = dailyBiomass + env%hourlyBiomass(I, KCANI, LAI, PARUE,hourlyStomatalConductance )
        END DO
        
        availableCarbohydrate_methodV = dailyBiomass

    end function availableCarbohydrate_methodV
    
    
END Module YCA_Control_Photosyntesis
    