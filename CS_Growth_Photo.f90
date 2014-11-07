!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.RATE) lines 4630 - 4693 of the original CSCAS code.The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in Module_CSCAS_Vars_List. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine CS_Growth_Photo calculates assimilation.
!
! NOTE: Hidden code (commented out) gives very different output. Why?
!***************************************************************************************************************************
    
    SUBROUTINE CS_Growth_Photo ( &
        CO2         , NFP         , SLPF        , SRAD        , TAIRHR      , TDEW        , TMAX        , TMIN        , &
        WEATHER     &        
        )
        
        USE ModuleDefs
        USE ModuleData
        USE Module_CSCAS_Vars_List
        
        IMPLICIT  NONE
        
        REAL    CO2         , NFP         , SLPF        , SRAD        , TAIRHR(24)  , TDEW        , TMAX        , TMIN        
        REAL    CARBOTMPRHR(TS)                                                      ! MF 17SE14 Added for VPD response of photosynthesis  

        TYPE (WeatherType) WEATHER    ! Defined in ModuleDefs
          
        SAVE
        
        TDEW   = WEATHER % TDEW
        PARHR  = WEATHER % PARHR                                                     ! MF 14SE14
        RADHR  = WEATHER % RADHR                                                     ! MF 14SE14
        RHUMHR = WEATHER % RHUMHR                                                    ! MF 14SE14
        TAIRHR = WEATHER % TAIRHR
        RADHR  = WEATHER % RADHR
        
!-----------------------------------------------------------------------
!           Calculate C assimilation at beginning of day
!-----------------------------------------------------------------------

            ! PAR utilization efficiency
            PARU = PARUE

            ! Conventional method using PAR utilization efficiency (P)
            CARBOTMPR = AMAX1(0.0,(PARMJFAC*SRAD)*PARU*CO2FP*TFP &
             * WFP * NFP * RSFP * VPDFP * SLPF)
            CARBOBEGR = CARBOTMPR * PARI / PLTPOP

            ! Modified conventional using internal CO2 (I)
            CARBOTMP = AMAX1(0.,PARMJFAC*SRAD*PARU*TFP*NFP*RSFP)
            ! Calculate for no water stress for WFPI determination
            CARBOTMPI = CARBOTMP
            CO2INTPPMP = CO2
            DO L = 1,20
              CO2INT = CO2AIR - CARBOTMPI *  &
              (RATM+RCROP*RLFC/RLF*WFP*(1.0*(1.0-WFP)))*1.157407E-05
              CO2INTPPM = AMAX1(CO2COMPC+20.0,CO2INT * &
              (8.314*1.0E7*((TMAX+TMIN)*.5+273.))/(1.0E12*1.0E-6*44.))
              CO2FPI = PARFC* &
               ((1.-EXP(-CO2EX*CO2INTPPM))-(1.-EXP(-CO2EX*CO2COMPC)))
              CARBOTMPI = CARBOTMP * CO2FPI
              IF (ABS(CO2INTPPM-CO2INTPPMP).LT.1.0) EXIT
              CO2INTPPMP = CO2INTPPM
            ENDDO
            CARBOBEGIA = 0.0
            IF (CARBOTMPI.GT.0) CARBOBEGIA =(CARBOTMP*CO2FP)/CARBOTMPI
            CARBOTMPI = CARBOTMP
            CO2INTPPMP = CO2
            DO L = 1,20
              CO2INT = CO2AIR - CARBOTMPI * (RATM+RCROP*RLFC/RLF*WFP* &
              (1.*(1.-WFP)))*1.157407E-05
              CO2INTPPM = AMAX1(CO2COMPC,CO2INT * &
              (8.314*1.0E7*((TMAX+TMIN)*0.5+273.))/ &
              (1.0E12*1.0E-6*44.0))
              CO2FPI = PARFC* &
              ((1.-EXP(-CO2EX*CO2INTPPM))-(1.-EXP(-CO2EX*CO2COMPC)))
              CARBOTMPI = CARBOTMP * CO2FPI
              IF (ABS(CO2INTPPM-CO2INTPPMP).LT.1.0) EXIT
              IF (ABS(CO2INTPPM-CO2COMPC).LT.1.0) EXIT
              CO2INTPPMP = CO2INTPPM
            ENDDO
            CARBOBEGI = CARBOTMPI * SLPF * PARI / PLTPOP * CARBOBEGIA

            ! Alternate method using resistances as per Monteith (M)
            ! Calculate photosynthetic efficiency
            ! Use 10 Mj.m2.d PAR to establish quantum requirement
            PHOTQR = (CO2AIR/(10.0*PARU)- &
             ((RATM+RCROP*RLFC/RLF*WFP*(1.*(1.-WFP)))*1.157407E-05))* &
             (10.0*30.0)/(CO2AIR*MJPERE) ! 30 = MW Ch2o
            RM = CO2AIR/(((SRAD*PARMJFAC/MJPERE)/PHOTQR)*30.0)
            CARBOTMPM = AMAX1(0., &
             (CO2AIR/((RATM+RCROP*RLFC/RLF*WFP*(1.*(1.-WFP)))* &
             1.157407E-05+RM))*TFP*NFP*RSFP)
            CARBOBEGM = CARBOTMPM * SLPF * PARI / PLTPOP

            ! Select method depending on choice in CONTROL FILE
            IF (MEPHO.EQ.'R') CARBOBEG = CARBOBEGR
            IF (MEPHO.EQ.'M') CARBOBEG = CARBOBEGM
            IF (MEPHO.EQ.'I') CARBOBEG = CARBOBEGI
        

 !       !-----------------------------------------------------------------------
 !       !           Calculate C assimilation at beginning of day
 !       !-----------------------------------------------------------------------
 !
 !       !! MF 14SE14
 !       !! Vapour pressure                                                            ! Code for VPD reponse moved from CS_Growth and modified for hourly response.
 !       !
 !       !!Hourly loop for VPD                                                         ! MF 16SE14
 !       !
 !       !IF (TDEW.LE.-98.0) TDEW = TMIN                                               ! MF 14SE14
 !       !DO L = 1, TS
 !       !    VPDHR(L) = (CSVPSAT(TAIRHR(L)) - CSVPSAT(TDEW))/1000.0                   ! VPDHR in kPa MF 14SE14
 !       !END DO 
 !       !
 !       !
 !       !VPDFPHR = 1.0
 !       !VPDFPPREV = 1.0
 !       !DO L = 1, TS
 !       !    IF (PHTV.GT.0.0) THEN                                                    ! If PHTV has a value (.NE. -99)
 !       !        IF (VPDHR(L).GT.PHTV) VPDFPHR(L) = AMAX1(0.0,1.0+PHSV*(VPDHR(L)-PHTV))
 !       !    ENDIF
 !       !    IF(VPDFPHR(L).GT.VPDFPPREV) THEN                                         ! If stomata close in response to VPD (VPDFPHR < 1.0), do not reopen (maintain VPDFPHR at its minimum value).
 !       !        VPDFPHR(L) = VPDFPPREV
 !       !    ELSE
 !       !        VPDFPPREV = VPDFPHR(L)
 !       !    ENDIF
 !       !END DO
 !       
 !       ! PAR utilization efficiency
 !       PARU = PARUE
 !       
 !       ! Conventional method using PAR utilization efficiency (P)
 !       CARBOTMPR = 0.0
 !       DO L = 1, TS
 !           CARBOTMPRHR(L) = AMAX1(0.0,(PARMJFAC*RADHR(L)*3.6/1000.)*PARU*CO2FP*TFP* WFP * NFP * RSFP * VPDFPHR(L) * SLPF)       ! MF 17/SE14 RADHR is in J/m2/s. Multiply by 3600 for hour, divide by 10^6 for MJ.
 !           CARBOTMPR = CARBOTMPR + CARBOTMPRHR(L)
 !       END DO
 !         
 !!       CARBOTMPR = AMAX1(0.0,(PARMJFAC*SRAD)*PARU*CO2FP*TFP* WFP * NFP * RSFP * VPDFP * SLPF)                                  ! MF 17SE14 Replaced by hourly estimation. 
 !       CARBOBEGR = CARBOTMPR * PARI / PLTPOP
 !       
 !       ! Modified conventional using internal CO2 (I)
 !       CARBOTMP = AMAX1(0.,PARMJFAC*SRAD*PARU*TFP*NFP*RSFP)
 !       ! Calculate for no water stress for WFPI determination
 !       CARBOTMPI = CARBOTMP
 !       CO2INTPPMP = CO2
 !       DO L = 1,20
 !           CO2INT = CO2AIR - CARBOTMPI * (RATM+RCROP*RLFC/RLF*WFP*(1.0*(1.0-WFP)))*1.157407E-05
 !           CO2INTPPM = AMAX1(CO2COMPC+20.0,CO2INT *(8.314*1.0E7*((TMAX+TMIN)*.5+273.))/(1.0E12*1.0E-6*44.))
 !           CO2FPI = PARFC*((1.-EXP(-CO2EX*CO2INTPPM))-(1.-EXP(-CO2EX*CO2COMPC)))
 !           CARBOTMPI = CARBOTMP * CO2FPI
 !           IF (ABS(CO2INTPPM-CO2INTPPMP).LT.1.0) EXIT
 !           CO2INTPPMP = CO2INTPPM
 !       ENDDO
 !       CARBOBEGIA = 0.0
 !       IF (CARBOTMPI.GT.0) CARBOBEGIA =(CARBOTMP*CO2FP)/CARBOTMPI
 !       CARBOTMPI = CARBOTMP
 !       CO2INTPPMP = CO2
 !       DO L = 1,20
 !           CO2INT = CO2AIR - CARBOTMPI * (RATM+RCROP*RLFC/RLF*WFP*(1.*(1.-WFP)))*1.157407E-05
 !           CO2INTPPM = AMAX1(CO2COMPC,CO2INT *(8.314*1.0E7*((TMAX+TMIN)*0.5+273.))/(1.0E12*1.0E-6*44.0))
 !           CO2FPI = PARFC*((1.-EXP(-CO2EX*CO2INTPPM))-(1.-EXP(-CO2EX*CO2COMPC)))
 !           CARBOTMPI = CARBOTMP * CO2FPI
 !           IF (ABS(CO2INTPPM-CO2INTPPMP).LT.1.0) EXIT
 !           IF (ABS(CO2INTPPM-CO2COMPC).LT.1.0) EXIT
 !           CO2INTPPMP = CO2INTPPM
 !       ENDDO
 !       CARBOBEGI = CARBOTMPI * SLPF * PARI / PLTPOP * CARBOBEGIA
 !       
 !       ! Alternate method using resistances as per Monteith (M)
 !       ! Calculate photosynthetic efficiency
 !       ! Use 10 Mj.m2.d PAR to establish quantum requirement
 !       PHOTQR = (CO2AIR/(10.0*PARU)-((RATM+RCROP*RLFC/RLF*WFP*(1.*(1.-WFP)))*1.157407E-05))*(10.0*30.0)/(CO2AIR*MJPERE) ! 30 = MW Ch2o
 !       RM = CO2AIR/(((SRAD*PARMJFAC/MJPERE)/PHOTQR)*30.0)
 !       CARBOTMPM = AMAX1(0.,(CO2AIR/((RATM+RCROP*RLFC/RLF*WFP*(1.*(1.-WFP)))*1.157407E-05+RM))*TFP*NFP*RSFP)
 !       CARBOBEGM = CARBOTMPM * SLPF * PARI / PLTPOP
 !       
 !       ! Select method depending on choice in CONTROL FILE
 !       IF (MEPHO.EQ.'R') CARBOBEG = CARBOBEGR
 !       IF (MEPHO.EQ.'M') CARBOBEG = CARBOBEGM
 !       IF (MEPHO.EQ.'I') CARBOBEG = CARBOBEGI
 !       
    END SUBROUTINE CS_Growth_Photo
    