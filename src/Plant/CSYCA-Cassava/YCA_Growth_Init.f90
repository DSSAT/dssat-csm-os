!***************************************************************************************************************************
! This is the code from the section (DYNAMIC == RATE) lines 4234 - 4384 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine YCA_Growth_Init calculates germination timing, daylength and development units, reserves and grazing (?). 
! TO DO: Check to see if grazing is for pest damage, and if not eliminate the relevant code.
!***************************************************************************************************************************
    
    SUBROUTINE YCA_Growth_Init ( &
        BRSTAGE     , DAYL        &  
        )
    
        USE ModuleDefs
        USE YCA_First_Trans_m
    
        IMPLICIT NONE
        
        REAL    BRSTAGE     , DAYL        
        REAL    :: BRSTAGETMP              ! Branching stage                #          ! (From Growth)    
    
        !-----------------------------------------------------------------------
        !           Determine when in day germination and emergence occurred
        !-----------------------------------------------------------------------

        ! Germination 
        !LPM 21MAR2016 GEUCUM modified to DAGERM
        IF (DAGERM < PGERM .AND. DAGERM+TTGEM*WFGE < PGERM) THEN
            GERMFR = 0.0                                                                                               !EQN 046a
        ELSEIF (DAGERM <= PGERM .AND. DAGERM+TTGEM*WFGE >= PGERM) THEN
            GERMFR = 1.0 - (PGERM-DAGERM)/(TTGEM*WFGE)                                                                  !EQN 046b
        ELSEIF (DAGERM > PGERM) THEN
            GERMFR = 1.0                                                                                               !EQN 046c
        ENDIF

        ! Emergence
       
        ! LPM 21MAR2016 Added reserves in the planting stake
        !LPM 02SEP2016 Deleted SDEPTHU > 0.0 as conditional:Reserves could be used even if the bud is above ground
        IF (GERMFR > 0.0.AND.EMRGFR < 1.0) THEN
            SEEDUSED = AMIN1(SEEDRS, PEMRG*TTGEM*WFGE)
            SEEDUSES = SEEDUSES + SEEDUSED
            SEEDUSE = SEEDUSES
            SEEDRS = AMAX1(0.0,SEEDRS - SEEDUSED)
            SELONG = SESR * SEEDUSED
            IF (SELONGT+SELONG < SDEPTHU) THEN
                EMRGFR = 0.0                                                                                               !EQN 047a
            ELSE                                                                         
                EMRGFR = 1.0
                IF (EMFLAG /= 'Y') THEN
                    EMFLAG = 'Y'
                ENDIF
                LNUMSG = 1     ! LAH NEW
            ENDIF
            SELONGT = AMIN1(SDEPTHU, SELONGT + SELONG)
        ELSE
            SEEDUSES = 0.0     
            EMRGFR = 1.0
            IF (EMFLAG /= 'Y') THEN
                EMFLAG = 'Y'
            ENDIF
            LNUMSG = 1     ! LAH NEW
        ENDIF
        !-----------------------------------------------------------------------
        !           Calculate daylength factors for development
        !-----------------------------------------------------------------------

        DF = 1.0
        DFNEXT = 1.0
        ! To ensure correct sensitivity on emergence day
        IF (BRSTAGE < 1.0) THEN
            BRSTAGETMP = 1.0
        ELSE
            BRSTAGETMP = BRSTAGE
        ENDIF
        IF (PPSEN == 'SL') THEN      ! Short day response,linear 
            DF = 1.0 - DAYLS(INT(BRSTAGETMP))/1000.*(PPTHR-DAYL)
            IF (BRSTAGETMP < FLOAT(PSX)) THEN
                DFNEXT = 1.-DAYLS(INT(BRSTAGETMP+1))/1000.*(PPTHR-DAYL)
            ELSE
                DFNEXT = DF
            ENDIF !EQN 050
        ELSEIF (PPSEN == 'LQ') THEN  ! Long day response,quadratic
            DF = AMAX1(0.0,AMIN1(1.0,1.0-(DAYLS(INT(BRSTAGETMP))/10000.*(PPTHR-DAYL)**PPEXP)))                         !EQN 048
            IF (BRSTAGETMP < FLOAT(PSX)) DFNEXT = AMAX1(0.0,AMIN1(1.0,1.0-(DAYLS(INT(BRSTAGETMP+1.0))/10000.*(PPTHR-DAYL)**PPEXP)))
            Tfdf = AMAX1(0.0,1.0-AMAX1(0.0,(TMEAN-10.0)/10.0))
            Tfdf = 1.0  ! LAH No temperature effect on DF ! 
            DF = DF + (1.0-DF)*(1.0-TFDF)                                                                              !EQN 049
            DFNEXT = DFNEXT + (1.0-DFNEXT)*(1.0-TFDF)
        ELSEIF (PPSEN == 'LL') THEN      ! Long day response,linear 
            IF (DAYL >= PPTHR) THEN
                DF = 1.0 
                DFNEXT = 1.0
            ELSE
                IF (BRSTAGE < 1.0) THEN
                    DF = 1.0 - AMIN1(1.0,DAYLS(INT(BRSTAGETMP))*(PPTHR-DAYL)) 
                ELSEIF (BRSTAGETMP < FLOAT(PSX)) THEN
                    DF = 1.0 - AMIN1(1.0,DAYLS(INT(BRSTAGETMP+1))*(PPTHR-DAYL)) 
                ENDIF 
                IF ((BRSTAGETMP+ 2.0) < FLOAT(PSX)) THEN
                    IF (BRSTAGE < 1.0) THEN
                        DFNEXT = 1.-AMIN1(1.0,DAYLS(INT(BRSTAGETMP+1))*(PPTHR-DAYL))
                    ELSE
                        DFNEXT = 1.-AMIN1(1.0,DAYLS(INT(BRSTAGETMP+2))*(PPTHR-DAYL))
                    ENDIF    
                ELSE
                    DFNEXT = DF
                ENDIF!EQN 050
            ENDIF
        ENDIF

        ! Set daylength factor for output (Is dfpe before emergence)
        IF (EMRGFR >= 1.0) THEN
            DFOUT = DF
        ELSE
            DFOUT = DFPE
        ENDIF 

        !-----------------------------------------------------------------------
        !           Calculate development units
        !-----------------------------------------------------------------------

        DU = 0.0
        DUPHASE = 0.0
        DUPNEXT = 0.0
        ! To avoid exceeding the array sizes
        IF (BRSTAGETMP < FLOAT(PSX)) THEN ! LPM 23JUL19 To use PSX instead of a fixed value
            !DUNEED = PSTART(INT(BRSTAGETMP+1.0))-CUMDU                                                                 !EQN 051
            !LPM 23JUL19 to use BRSTAGETMP for the first branching level
            IF (BRSTAGE < 1.0) THEN
                DUNEED = PSTART(INT(BRSTAGETMP))-DABR 
            ELSE
                DUNEED = PSTART(INT(BRSTAGETMP+1.0))-DABR     !EQN 051!LPM 24APR2016 using the thermal clock with stress
            ENDIF                                              
            IF (DUNEED >= TTB*(DFPE*(GERMFR-EMRGFR)+DF*EMRGFR))THEN                                                    !LPM 21MAR15 use TTB instead of TT (TO1=24=TO2)
                DUPHASE = TTB*(DFPE*(GERMFR-EMRGFR)+DF*EMRGFR)                                                          !EQN 052a !LPM 21MAR15/19APR2016 use TTB instead of TT (TO1=24=TO2)
                TIMENEED = 1.0
                DUPNEXT = 0.0
            ELSE  
                DUPHASE = DUNEED                                                                                       !EQN 052b
                TIMENEED = DUNEED/(TTB*(DFPE*(GERMFR-EMRGFR)+DF*EMRGFR))                                                !EQN 054 !LPM 21MAR15/19APR2016 use TTB instead of TT (TO1=24=TO2)
                DUPNEXT = TTNEXT*(1.0-TIMENEED)*DFNEXT                                                                 !EQN 053 
            ENDIF
        ENDIF
            
        DU = DUPHASE+DUPNEXT                                                                                           !EQN 055
        !LPM 22DEC2016 Root growth based on top growth (deleted SEEDRSAVR) 
        !-----------------------------------------------------------------------
        !           Set seed reserve use for root growth and update av.reserves
        !-----------------------------------------------------------------------
        !IF (GERMFR > 0.0.OR.GESTAGE >= 0.5) THEN !LPM 21MAR2016 To separate germination and emergence
        !IF (GERMFR > 0.0.OR.GESTAGE >= 1.0) THEN
        !    SEEDRSAVR = AMIN1(SEEDRS,SEEDRSI/SDDUR*(TTGEM/STDAY)*GERMFR)                                                  !EQN 286 
        !    !LPM 22MAR2016 use for roots the same amount of reserves than aboveground (SEEDUSED)
        !    !SEEDRSAVR = AMIN1(SEEDUSED,SEEDRS) 
        !ELSE
        !    SEEDRSAVR = 0.0
        !ENDIF
        ! Seed reserves available
        !SEEDRS = SEEDRS-SEEDRSAVR                                                                                  !EQN 287
        SEEDRSAV =  SEEDRS
        !-----------------------------------------------------------------------
        !           Determine if today has a harvest instruction
        !-----------------------------------------------------------------------

        !DO I = 1, 20 !LPM 27SEP2019 This section is not necessary and defines the wrong harvesting the second year for multiple years simulations  
        !    IF (HYEARDOY(I) == YEARDOY) THEN  
        !        HANUM = I
        !        CALL CSUCASE(HOP(I)) 
        !        IF (hop(i) == 'F') YEARDOYHARF = YEARDOY 
        !    ENDIF
        !END DO

        !-----------------------------------------------------------------------
        !           Determine amounts removed by grazing, etc.   
        !-----------------------------------------------------------------------

        IF (HANUM > 0) THEN
            IF (HOP(HANUM) == 'G'.AND. CWAD > 0.0 .AND. CWAD > CWAN(HANUM)) THEN
                HAWAD = AMIN1((CWAD-CWAN(HANUM)),HAMT(HANUM))                                                          !EQN 414
                HAWAD = AMAX1(0.0,HAWAD)
                HAFR = AMAX1(0.0,HAWAD/CWAD)                                                                           !EQN 415
            ELSE   
                HAWAD = 0.0
                HAFR = 0.0
            ENDIF
        ENDIF
              

        ! For grazing 
        lwph = lfwt * hafr                                                                                             !EQN 416
        laph = lapd * hafr                                                                                             !EQN 417
        swph = stwt * hafr                                                                                             !EQN 418
        rswph = rswt * hafr                                                                                            !EQN 419
        lnph = leafn * hafr                                                                                            !EQN 420
        snph = stemn * hafr                                                                                            !EQN 421
        node%snphn = node%stemnn * hafr                                                                         !LPM 23MAY2015 to consider stem N by node cohort 
        rsnph = rsn * hafr                                                                                             !EQN 422
        
    END SUBROUTINE YCA_Growth_Init