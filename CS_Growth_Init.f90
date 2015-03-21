!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.RATE) lines 4234 - 4384 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module CS_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine CS_Growth_Init calculates germination timing, daylength and development units, reserves and grazing (?). 
! TO DO: Check to see if grazing is for pest damage, and if not eliminate the relevant code.
!***************************************************************************************************************************
    
    SUBROUTINE CS_Growth_Init ( &
        BRSTAGE     , DAYL        &  
        )
    
        USE ModuleDefs
        USE CS_First_Trans_m
    
        IMPLICIT NONE
        
        REAL    BRSTAGE     , DAYL        
    
        !-----------------------------------------------------------------------
        !           Determine when in day germination and emergence occurred
        !-----------------------------------------------------------------------

        ! Germination
        IF (GEUCUM.LT.PEGD.AND.GEUCUM+TTGEM*WFGE.LT.PEGD) THEN
            GERMFR = 0.0                                                                                               !EQN 046a
        ELSEIF (GEUCUM.LE.PEGD.AND.GEUCUM+TTGEM*WFGE.GE.PEGD) THEN
            GERMFR = 1.0 - (PEGD-GEUCUM)/(TTGEM*WFGE)                                                                  !EQN 046b
        ELSEIF (GEUCUM.GT.PEGD) THEN
            GERMFR = 1.0                                                                                               !EQN 046c
        ENDIF

        ! Emergence
        IF (GEUCUM.LT.PEGD+PECM*SDEPTHU.AND.GEUCUM+TTGEM*WFGE.LE.PEGD+PECM*SDEPTHU) THEN
            EMRGFR = 0.0                                                                                               !EQN 047a
        ELSEIF (GEUCUM.LE.PEGD+PECM*SDEPTHU.AND.GEUCUM+TTGEM*WFGE.GT.PEGD+PECM*SDEPTHU) THEN
            EMRGFR = 1.0 - (PEGD+PECM*SDEPTHU-GEUCUM)/(TTGEM*WFGE)                                                     !EQN 047b
        IF (EMFLAG.NE.'Y') THEN
            WRITE(FNUMWRK,*)' ' 
            WRITE(FNUMWRK,'(A18,I8)')' Emergence on day ',yeardoy 
            EMFLAG = 'Y'
        ENDIF
        LNUMSG = 1     ! LAH NEW
        ELSEIF (GEUCUM.GT.PEGD+PECM*SDEPTHU) THEN
            EMRGFR = 1.0                                                                                               !EQN 047c
        ENDIF
     
        !-----------------------------------------------------------------------
        !           Calculate daylength factors for development
        !-----------------------------------------------------------------------

        DF = 1.0
        DFNEXT = 1.0
        ! To ensure correct sensitivity on emergence day
        IF (BRSTAGE.LE.0.0) THEN
            BRSTAGETMP = 1.0
        ELSE
            BRSTAGETMP = BRSTAGE
        ENDIF
        IF (PPSEN.EQ.'SL') THEN      ! Short day response,linear 
            DF = 1.0 - DAYLS(INT(BRSTAGETMP))/1000.*(PPTHR-DAYL)                                                       !EQN 050
            !IF (BRSTAGETMP.LT.FLOAT(MSTG)) THEN  !LPM 06MAR15 MSTG to PSX
            IF (BRSTAGETMP.LT.FLOAT(PSX)) THEN
                DFNEXT = 1.-DAYLS(INT(BRSTAGETMP+1))/1000.*(PPTHR-DAYL)
            ELSE
                DFNEXT = DF
            ENDIF 
        ELSEIF (PPSEN.EQ.'LQ') THEN  ! Long day response,quadratic
            DF = AMAX1(0.0,AMIN1(1.0,1.0-(DAYLS(INT(BRSTAGETMP))/10000.*(PPTHR-DAYL)**PPEXP)))                         !EQN 048
            IF (BRSTAGETMP.LT.10.0) DFNEXT = AMAX1(0.0,AMIN1(1.0,1.0-(DAYLS(INT(BRSTAGETMP+1.0))/10000.*(PPTHR-DAYL)**PPEXP)))
            Tfdf = AMAX1(0.0,1.0-AMAX1(0.0,(TMEAN-10.0)/10.0))
            Tfdf = 1.0  ! LAH No temperature effect on DF ! 
            DF = DF + (1.0-DF)*(1.0-TFDF)                                                                              !EQN 049
            DFNEXT = DFNEXT + (1.0-DFNEXT)*(1.0-TFDF)
        ENDIF

        ! Set daylength factor for output (Is dfpe before emergence)
        IF (EMRGFR.GE.1.0) THEN
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
        IF (BRSTAGETMP.LT.10.0) THEN
            DUNEED = PSTART(INT(BRSTAGETMP+1.0))-CUMDU                                                                 !EQN 051
            IF (DUNEED.GE.TTB*(DFPE*(GERMFR-EMRGFR)+DF*EMRGFR))THEN                                                    !LPM 21MAR15 use TTB instead of TT (TO1=24)
                DUPHASE = TTB*(DFPE*(GERMFR-EMRGFR)+DF*EMRGFR)                                                          !EQN 052a !LPM 21MAR15 use TTB instead of TT (TO1=24)
                TIMENEED = 1.0
                DUPNEXT = 0.0
            ELSE  
                DUPHASE = DUNEED                                                                                       !EQN 052b
                TIMENEED = DUNEED/(TTB*(DFPE*(GERMFR-EMRGFR)+DF*EMRGFR))                                                !EQN 054 !LPM 21MAR15 use TTB instead of TT (TO1=24)
                DUPNEXT = TTNEXT*(1.0-TIMENEED)*DFNEXT                                                                 !EQN 053 
            ENDIF
        ELSE
        ENDIF
            
        DU = DUPHASE+DUPNEXT                                                                                           !EQN 055
            
        !-----------------------------------------------------------------------
        !           Set seed reserve use for root growth and update av.reserves
        !-----------------------------------------------------------------------

        IF (GERMFR.GT.0.0.OR.GESTAGE.GE.0.5) THEN
            SEEDRSAVR = AMIN1(SEEDRS,SEEDRSI/SDDUR*(TT/STDAY)*GERMFR)                                                  !EQN 286
        ELSE
            SEEDRSAVR = 0.0
        ENDIF
        ! Seed reserves available
        SEEDRSAV = SEEDRSAV-SEEDRSAVR                                                                                  !EQN 287

        !-----------------------------------------------------------------------
        !           Determine if today has a harvest instruction
        !-----------------------------------------------------------------------

        DO I = 1, 20
            IF (HYEARDOY(I).EQ.YEARDOY) THEN
                HANUM = I
                WRITE(fnumwrk,*) ' '
                WRITE(fnumwrk,'(A20,i2,A12,A1,A6,i8)')' Harvest instruction ',hanum,'  Operation ',hop(i),'  Day ',yeardoy
                CALL CSUCASE(HOP(I)) 
                IF (hop(i).EQ.'F') YEARDOYHARF = YEARDOY 
            ENDIF
        END DO

        !-----------------------------------------------------------------------
        !           Determine amounts removed by grazing, etc.   
        !-----------------------------------------------------------------------

        IF (HANUM.GT.0) THEN
            IF (HOP(HANUM).EQ.'G'.AND.CWAD.GT.0.0.AND.CWAD.GT.CWAN(HANUM)) THEN
                HAWAD = AMIN1((CWAD-CWAN(HANUM)),HAMT(HANUM))                                                          !EQN 414
                HAWAD = AMAX1(0.0,HAWAD)
                HAFR = AMAX1(0.0,HAWAD/CWAD)                                                                           !EQN 415
            ELSE   
                HAWAD = 0.0
                HAFR = 0.0
            ENDIF
        ENDIF
              
        IF (HAFR.GT.0.0) WRITE(fnumwrk,'(A23,3F6.1)')' HARVEST  FR,CWAN,CWAD ',HAFR,CWAN(HANUM),CWAD

        ! For grazing 
        lwph = lfwt * hafr                                                                                             !EQN 416
        laph = lapd * hafr                                                                                             !EQN 417
        swph = stwt * hafr                                                                                             !EQN 418
        rswph = rswt * hafr                                                                                            !EQN 419
        lnph = leafn * hafr                                                                                            !EQN 420
        snph = stemn * hafr                                                                                            !EQN 421
        rsnph = rsn * hafr                                                                                             !EQN 422
        
    END SUBROUTINE CS_Growth_Init