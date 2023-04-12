!**********************************************************************************************************************
! This is the code from the section (DYNAMIC == RUNINIT) ! Initialization, lines 3109 - 3549 of the original CSCAS code.
! The names of the dummy arguments are the same as in the original CSCAS code and the call statement and are declared 
! here. The variables that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all 
! comments are those of the original CSCAS.FOR code.
!
! Subroutine YCA_SeasInit_SetStage sets up the growth stages including branching, check coefficients, sets defaults and 
! calculates/sets initial states.
!**********************************************************************************************************************

    SUBROUTINE YCA_SeasInit_SetStage( &
        ISWNIT      , KCAN        , KEP         , SLPF          &
        )

! 2023-01-25 chp removed unused variables
!       CN          , RN          , RUN         , RUNI        , TN           

        USE OSDefinitions
        USE ModuleData
        USE YCA_First_Trans_m

        
        IMPLICIT NONE
        EXTERNAL TVILENT, WARNING, ERROR
        
!       INTEGER CN          , RN          , RUN         , RUNI        , TN          
        INTEGER TVILENT                                                                       ! Integer function call.        
        
        REAL    KCAN        , KEP         , SLPF        
        
        CHARACTER(LEN=1) ISWNIT         
         
        !-----------------------------------------------------------------------
        !       Determine 'key' principal and secondary stages,and adjust names
        !-----------------------------------------------------------------------

        KEYPSNUM = 0
        PSNUM = 0
        !DO L = 1,PSX  !LPM 28MAR15 the first stage is 0 (before branching)
        DO L = 0,PSX-1
            IF (TVILENT(PSTYP(L)) > 0) THEN                                            ! TVILENT is a function in CSUTS.FOR the same as the intrinsic function LEN_TRIM
                IF (PSTYP(L) == 'K'.OR.PSTYP(L) == 'k'.OR.PSTYP(L) == 'M')THEN          ! PSNO PSTYP PSABV PSNAME    (From .SPE file, S=Standard, K=Key)
                    KEYPSNUM = KEYPSNUM + 1                                             !    0     S GDAT  Germinate
                    KEYPS(KEYPSNUM) = L                                                 !    1     K B1DAT 1stBranch
                ENDIF                                                                   !    2     K B2DAT 2ndBranch
                IF (PSABV(L) == 'HDAT') HSTG = L                                        !    3     K B3DAT 3rdBranch
                !IF (PSABV(L) == 'MDAT') MSTG = L                                       !    4     K B4DAT 4thBranch !LPM 07MAR15 There is not a MSTG for cassava
                PSNUM = PSNUM + 1                                                       !    5     K B5DAT 5thBranch
            ENDIF                                                                       !    6     K B6DAT 6thBranch until branch level 10
        ENDDO                                                                           !   10     K B0DAT 10thBranch 

        IF (HSTG <= 0) THEN
            HSTG = PSX
            !HSTG = MSTG+1                  !LPM 07MAR15 MSTG to PSX
            !PSTART(HSTG) = 5000   ! Set to very long cycle !LPM 06MAR15 to avoid low values of PSTART with DEVU
        ENDIF
        ! Check and adjust stage abbreviations (DAT -> DAP)
        DO L = 1,PSNUM
            IF (TVILENT(PSABV(L)) > 3) THEN
                IF (TVILENT(PSABV(L)) == 4) THEN
                    DO L1 = 5,1,-1
                        IF (L1 > 1) THEN
                            PSABV(L)(L1:L1) = PSABV(L)(L1-1:L1-1)
                        ELSE
                            PSABV(L)(L1:L1) = ' '
                        ENDIF
                    ENDDO
                ENDIF
                PSABVO(L) = PSABV(L)
                PSABVO(L)(5:5) = 'P'
            ENDIF
        ENDDO
        

        DUTOMSTG = 0.0

            
        DO L = 0,PSX  !LPM 04MAR15 used to define PD as PDL (directly from the cultivar file as thermal time) 
            IF (L <= 4)THEN
                PD(L) = PDL(L)
            ELSE
                PD(L) = PDL(4)
            ENDIF
            IF (PD(L) == 0.0 .AND. L > 0) THEN
                  MESSAGE(1) = "Coefficient BxyND should be" // &
                      " greater than zero. Correct the cultivar file."
                  MESSAGE(2) = "Program will stop."
                  CALL WARNING(2, 'CSYCA',MESSAGE)
                  CALL ERROR('IPCUL',3,"",0)
            ENDIF
            
        ENDDO
            
            !IF (MSTG > 2) THEN !LPM 07MAR15 MSTG to PSX
            IF (PSX > 4) THEN
                Ctrnumpd = 0
                !DO L = 2,MSTG-1  !LPM 04MAR15 It is not necessary the -1 because there is not a MSTG with a different value 
                DO L = 1,PSX
                    !LPM 04MAR15 We use the same input data PDL instead of create a new coefficient (PD) 
                    IF (PDL(L) < 0.0) THEN  
                        PDL(L) = PDL(L-1)
                       !LPM 04MAR15 We use the same input data PDL instead of create a new coefficient (PD)
                        PD(L) = PDL(L-1)
                        CTRNUMPD = CTRNUMPD + 1
                    ENDIF
                ENDDO
            ENDIF  

            DO L = 0,PSX
                PSTART(L) = 0.0
            ENDDO

            DO L = 1,PSX
                PSTART(L) = PSTART(L-1) + AMAX1(0.0,PD(L))
            ENDDO
            

            DO L = 1, PSX-1
                IF (PD(L) > 0.0) THEN
                    DUTOMSTG = DUTOMSTG + PD(L)
                ENDIF  
            ENDDO

        
        !-----------------------------------------------------------------------
        !       Check and/or adjust coefficients and set defaults if not present
        !-----------------------------------------------------------------------
        
        DO L = 0,1
            LNCXS(L) = LNPCS(L)/100.0 
            SNCXS(L) = SNPCS(L)/100.0 
            RNCXS(L) = RNPCS(L)/100.0 
            LNCMN(L) = LNPCMN(L)/100.0
            SNCMN(L) = SNPCMN(L)/100.0
            RNCMN(L) = RNPCMN(L)/100.0
        ENDDO
        
        LNSC =  LNSC/100.0
        
        IF (DFPE < 0.0) THEN  
            DFPE = 1.0
            WRITE(MESSAGE(1),'(A51)') 'Pre-emergence development factor missing. Set to 1.'
            CALL WARNING(1,'CSYCA',MESSAGE)
        ENDIF
        
        ! Nitrogen uptake                  
        IF (rtno3.le.0.0) RTNO3 = 0.006  ! NO3 uptake/root lgth (mgN/cm)
        IF (rtnh4.le.0.0) RTNH4 = RTNO3  ! NH4 uptake/root lgth (mgN/cm)
        IF (h2ocf.lt.0.0) H2OCF = 1.0    ! H2O conc factor for N uptake 
        IF (no3cf.lt.0.0) NO3CF = 1.0    ! NO3 uptake conc factor (exp)
        IF (nh4cf.lt.0.0) NH4CF = NO3CF  ! NH4 uptake factor (exponent) 
        
        ! Radiation use efficiency
        IF (PARUE <= 0.0) PARUE = 2.3
        
        ! Leaves
        IF (PHINTFAC <= 0.0) PHINTFAC = 0.8
        !IF (LA1S < 0.0) LA1S = 5.0                             !DA 03OCT2016 Removing LA1S variable, is not used according to LPM 07MAR15     
        IF (LAXS < 0.0) LAXS = 200.0
        IF (LWLOS < 0.0) LWLOS = 0.3
        IF (LPEFR < 0.0) LPEFR = 0.33
        
        ! Roots
        IF (RDGS < 0.0) RDGS = 3.0
        IF (RSEN < 0.0) RSEN = 5.0
        
        ! Reduction factor limits
        !IF (WFPL < 0.0) WFPL = 0.0
        !IF (WFPU < 0.0) WFPU = 1.0
        IF (WFGL < 0.0) WFGL = 0.0
        IF (WFGU < 0.0) WFGU = 1.0
        IF (NFPL < 0.0) NFPL = 0.0
        IF (NFPU < 0.0) NFPU = 1.0
        IF (NFGL < 0.0) NFGL = 0.0
        IF (NFGU < 0.0) NFGU = 1.0
        IF (NLLG <= 0.0) NLLG = 0.95
        !IF (NFSU < 0.0) NFSU = 0.2
        
        ! Various
        !IF (RSFRS < 0.0) RSFRS = 0.05 !LPM 09OCT2019 Remove the reserve fraction to the stems (RSFRS)
        IF (LSENI < 0.0) LSENI = 0.0
        IF (PARIX <= 0.0) PARIX = 0.995
        !LPM 15NOV2020 Remove the N top-up fraction
        !IF (NTUPF < 0.0) NTUPF = 0.2
        IF (PPEXP < 0.0) PPEXP = 2.0
        IF (RTUFR < 0.0) RTUFR = 0.05
        IF (SHGR(20) < 0.0) THEN 
            DO L = 3,22
                SHGR(L) = 1.0 !  Shoot sizes relative to main shoot
            ENDDO
        ENDIF
        
        IF (SLPF <= 0.0 .OR. SLPF > 1.0) SLPF = 1.0
        
        IF (PLPH <= 0.0) THEN
            WRITE (MESSAGE,'(A27,F6.1,A14)') ' Plants per hill <= 0.0 at ',PLPH,'  Reset to 1.0'
            CALL WARNING(1,'CSYCA',MESSAGE)
            PLPH = 1.0
        ENDIF  
        
        ! Soil inorganic N uptake aspects
        IF (NO3MN < 0.0) NO3MN = 0.5
        IF (NH4MN < 0.0) NH4MN = 0.5
        
        
        !-----------------------------------------------------------------------
        !       Calculate derived coefficients and set equivalences
        !-----------------------------------------------------------------------
        
        ! Initial leaf growth aspects
        
        ! If max LAI not read-in,calculate from max interception
        IF (LAIXX <= 0.0) LAIXX = LOG(1.0-PARIX)/(-KCAN)                                                               ! EQN 008
        
        !PHINT = PHINTS  !LPM 21MAY2015 this variable is not used
        
        ! Leaf life
        IF (CFLLFLIFE=='T')THEN
            ! Read-in as thermal time 
            LLIFGTT = LLIFG                                                                                            !EQN 352
            LLIFATT = LLIFA                                                                                            !EQN 353 
            LLIFSTT = LLIFS                                                                                            !EQN 354
        ENDIF  
        
        ! Extinction coeff for SRAD
        KEP = (KCAN/(1.0-TPAR)) * (1.0-TSRAD)                                                                          ! EQN 009
        
        ! Photoperiod sensitivities
        DO L = 1,PSX
            IF (L<=3) THEN
                IF (DAYLS(L)< 0.0) DAYLS(L) = 0.0
                IF (BRFX(L) <= 0.0) BRFX(L) = 3.0
            ELSE
                DAYLS(L) = DAYLS(L-1)
                IF (BRFX(L) <= 0.0) BRFX(L) = BRFX(L-1)
            ENDIF
        ENDDO
        IF (Dayls(1) == 0.0.AND.dfpe < 1.0) THEN
            WRITE(MESSAGE(1),'(A36,A41)') 'Cultivar insensitive to photoperiod ', 'but pre-emergence photoperiod factor < 1.' 
            WRITE(MESSAGE(2),'(A40)') 'May be worthwhile to change PPFPE to 1.0'
            CALL WARNING(2,'CSYCA',MESSAGE)
        ENDIF
        
        ! Shoot growth rates relative to main shoot
        IF (SHGR(20) >= 0.0) THEN
            DO L = 3,22
                IF (L < 20) THEN
                    SHGR(L) = SHGR(2)-((SHGR(2)-SHGR(20))/18)*(L-2)                                                    !EQN 010
                ELSEIF (L > 20) THEN
                    SHGR(L) = SHGR(20)
                ENDIF  
            ENDDO
        ENDIF
        
        ! Critical and starting N concentrations
        node%LNCX = LNCXS(0)
        node%SNCX = SNCXS(0)
        RNCX = RNCXS(0)
        node%LNCM = LNCMN(0)
        node%SNCM = SNCMN(0)
        RNCM = RNCMN(0)
        
        ! Storage root N  NB.Conversion protein->N factor = 6.25
        IF (SRNPCS <= 0.0) THEN
            IF(SRPRS > 0.0) THEN
                SRNPCS = SRPRS / 6.25                                                                                  !EQN 023
            ELSE
                SRNPCS = 0.65
            ENDIF
        ENDIF       
        
        ! Height growth
        !SERX = CANHTS/PSTART(MSTG) change to br. level 6 to avoid that it takes long time to increase CANHT            !EQN 315
        !SERX = CANHTS/PSTART(6) LPM 06JUL2017 SERX will not be used
        
        !-----------------------------------------------------------------------
        !       Set coefficients that dependent on input switch
        !-----------------------------------------------------------------------
        
        IF (ISWWATCROP == 'N') THEN
            ! Plant water status effects on growth turned off
            WFGU = 0.0
            !WFPU = 0.0
            !WFSU = 0.0
            WFRTG = 0.0
        ENDIF
        
        !-----------------------------------------------------------------------
        !       Calculate/set initial states
        !-----------------------------------------------------------------------
        
        !LPM 22MAR2016 To use SEEDRS from emergence 
        !IF (SDRATE <= 0.0) SDRATE = SDSZ*PPOP*10.0                                                                  !EQN 024 !LPM 06MAR2016 To have just one name for PPOP
        IF (SDRATE <= 0.0) SDRATE = SDSZ*SPRL*PPOP*10.0   
        ! Reserves = SDRS% of seed                                                                                  !LPM 22MAR2016 Keep value SDRS  
        SEEDRSI = (SDRATE/(PPOP*10.0))*SDRS/100.0                                                                  !EQN 284 !LPM 06MAR2016 To have just one name for PPOP
        SEEDRS = SEEDRSI
        SEEDRSAV = SEEDRS
        SDCOAT = (SDRATE/(PPOP*10.0))*(1.0-SDRS/100.0)                                                             !EQN 025 !LPM 06MAR2016 To have just one name for PPOP
        ! Seed N calculated from total seed
        SDNAP = (SDNPCI/100.0)*SDRATE                                                                                  !EQN 026
        SEEDNI = (SDNPCI/100.0)*(SDRATE/(PPOP*10.0))                                                                !EQN 027
        IF (ISWNIT /= 'N') THEN
            SEEDN = SEEDNI
        ELSE
            SEEDN = 0.0
            SDNAP = 0.0
            SEEDNI = 0.0
        ENDIF
        
        ! Water table depth
!       WTDEP = ICWD
        CALL GET('WATER','WTDEP',WTDEP)
        
        ! Initial shoot and root placement
        IF (PLME /= 'I') THEN
            IF (PLME /= 'H') THEN
                IF (PLME /= 'V') THEN
                    WRITE(MESSAGE(1),'(A16,A1,A15,A24)') 'PLANTING method ',PLME,' not an option ', ' Changed to V (Vertical)'
                    CALL WARNING(1,'CSYCA',MESSAGE)
                    PLME = 'V'
                ENDIF
            ENDIF
        ENDIF
        IF (SPRL <= 0.0) THEN
            WRITE(MESSAGE(1),'(A30,A20)') 'Planting stick length <= 00  ', ' Changed to 25.0 cm '
            CALL WARNING(1,'CSYCA',MESSAGE)
            SPRL = 25.0
        ENDIF
        sdepthu = -99.0
        IF (PLME == 'H') THEN
            sdepthu = sdepth
        ELSEIF (PLME == 'I') THEN
            ! Assumes that inclined at 45o
            !sdepthu = AMAX1(0.0,sdepth - 0.707*sprl)                                                                   !EQN 028 !LPM 22MAR2016 Modified to consider buds at 2.5 cm from the top end of the stake
            sdepthu = sdepth - 0.707*(sprl-2.5)                                                                  !EQN 028
        ELSEIF (PLME == 'V') THEN
            !sdepthu = AMAX1(0.0,sdepth - sprl)                                                                         !EQN 029 !LPM 22MAR2016 Modified to consider buds at 2.5 cm from the top end of the stake
            sdepthu = sdepth - (sprl-2.5)                                                                        !EQN 029
        ENDIF
        !IF (sdepthu < 0.0) sdepthu = sdepth                                                                    !LPM 22MAR2016 Modified to assume that top of stake is in the soil surface (when it is above)
        IF (sdepthu < 0.0) sdepthu = 0.0 
        
    END SUBROUTINE YCA_SeasInit_SetStage