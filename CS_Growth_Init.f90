!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.RATE) lines 4221 - 4370 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in Module_CSCAS_Vars_List. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine CS_Growth_Init calculates germination timing, daylength and development units, reserves and grazing (?). 
! TO DO: Check to see if grazing is for pest damage, and if not eliminate the relevant code.
!***************************************************************************************************************************
    
    SUBROUTINE CS_Growth_Init ( &
        BRSTAGE     , DAYL        &  
        )
    
        USE ModuleDefs
        USE Module_CSCAS_Vars_List
    
        IMPLICIT NONE
        
        !TYPE (WeatherType) WEATHER    ! Defined in ModuleDefs
    
        REAL    BRSTAGE     , DAYL        
    

        !-----------------------------------------------------------------------
        !           Determine when in day germination and emergence occurred
        !-----------------------------------------------------------------------

        ! Germination
        IF (GEUCUM.LT.PEGD.AND.GEUCUM+TTGEM*WFGE.LT.PEGD) THEN
            GERMFR = 0.0
        ELSEIF (GEUCUM.LE.PEGD.AND.GEUCUM+TTGEM*WFGE.GE.PEGD) THEN
            GERMFR = 1.0 - (PEGD-GEUCUM)/(TTGEM*WFGE)
        ELSEIF (GEUCUM.GT.PEGD) THEN
            GERMFR = 1.0
        ENDIF

        ! Emergence
        IF (GEUCUM.LT.PEGD+PECM*SDEPTHU.AND.GEUCUM+TTGEM*WFGE.LE.PEGD+PECM*SDEPTHU) THEN
            EMRGFR = 0.0
        ELSEIF (GEUCUM.LE.PEGD+PECM*SDEPTHU.AND.GEUCUM+TTGEM*WFGE.GT.PEGD+PECM*SDEPTHU) THEN
            EMRGFR = 1.0 - (PEGD+PECM*SDEPTHU-GEUCUM)/(TTGEM*WFGE)
        IF (EMFLAG.NE.'Y') THEN
            WRITE(FNUMWRK,*)' ' 
            WRITE(FNUMWRK,'(A18,I8)')' Emergence on day ',yeardoy 
            EMFLAG = 'Y'
        ENDIF
        LNUMSG = 1     ! LAH NEW
        ELSEIF (GEUCUM.GT.PEGD+PECM*SDEPTHU) THEN
            EMRGFR = 1.0
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
            DF = 1.0 - DAYLS(INT(BRSTAGETMP))/1000.*(PPTHR-DAYL)
            IF (BRSTAGETMP.LT.FLOAT(MSTG)) THEN
                DFNEXT = 1.-DAYLS(INT(BRSTAGETMP+1))/1000.*(PPTHR-DAYL)
            ELSE
                DFNEXT = DF
            ENDIF 
        ELSEIF (PPSEN.EQ.'LQ') THEN  ! Long day response,quadratic
            DF = AMAX1(0.0,AMIN1(1.0,1.0-(DAYLS(INT(BRSTAGETMP))/10000.*(PPTHR-DAYL)**PPEXP)))
            IF (BRSTAGETMP.LT.10.0) DFNEXT = AMAX1(0.0,AMIN1(1.0,1.0-(DAYLS(INT(BRSTAGETMP+1.0))/10000.*(PPTHR-DAYL)**PPEXP)))
            Tfdf = AMAX1(0.0,1.0-AMAX1(0.0,(TMEAN-10.0)/10.0))
            Tfdf = 1.0  ! LAH No temperature effect on DF ! 
            DF = DF + (1.0-DF)*(1.0-TFDF)
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
            DUNEED = PSTART(INT(BRSTAGETMP+1.0))-CUMDU
            IF (DUNEED.GE.TT*(DFPE*(GERMFR-EMRGFR)+DF*EMRGFR))THEN
                DUPHASE = TT*(DFPE*(GERMFR-EMRGFR)+DF*EMRGFR)
                TIMENEED = 1.0
                DUPNEXT = 0.0
            ELSE  
                DUPHASE = DUNEED
                TIMENEED = DUNEED/(TT*(DFPE*(GERMFR-EMRGFR)+DF*EMRGFR))
                DUPNEXT = TTNEXT*(1.0-TIMENEED)*DFNEXT
            ENDIF
        ELSE
        ENDIF
            
        DU = DUPHASE+DUPNEXT
            
        !-----------------------------------------------------------------------
        !           Set seed reserve use for root growth and update av.reserves
        !-----------------------------------------------------------------------

        IF (GERMFR.GT.0.0.OR.GESTAGE.GE.0.5) THEN
            SEEDRSAVR = AMIN1(SEEDRS,SEEDRSI/SDDUR*(TT/STDAY)*GERMFR)
        ELSE
            SEEDRSAVR = 0.0
        ENDIF
        ! Seed reserves available
        SEEDRSAV = SEEDRSAV-SEEDRSAVR

        !-----------------------------------------------------------------------
        !           Determine if today has a harvest instruction
        !-----------------------------------------------------------------------

        DO I = 1, 20
            IF (HYEARDOY(I).EQ.YEARDOY) THEN
                HANUM = I
                WRITE(fnumwrk,*) ' '
                WRITE(fnumwrk,'(A20,i2,A12,A1,A6,i8)')' Harvest instruction',hanum,'  Operation ',hop(i),'  Day ',yeardoy
                CALL CSUCASE(HOP(I)) 
                IF (hop(i).EQ.'F') YEARDOYHARF = YEARDOY 
            ENDIF
        END DO

        !-----------------------------------------------------------------------
        !           Determine amounts removed by grazing, etc.   
        !-----------------------------------------------------------------------

        IF (HANUM.GT.0) THEN
            IF (HOP(HANUM).EQ.'G'.AND.CWAD.GT.0.0.AND.CWAD.GT.CWAN(HANUM)) THEN
                HAWAD = AMIN1((CWAD-CWAN(HANUM)),HAMT(HANUM))
                HAWAD = AMAX1(0.0,HAWAD)
                HAFR = AMAX1(0.0,HAWAD/CWAD)
            ELSE   
                HAWAD = 0.0
                HAFR = 0.0
            ENDIF
        ENDIF
              
        IF (HAFR.GT.0.0) WRITE(fnumwrk,'(A23,3F6.1)')' HARVEST  FR,CWAN,CWAD ',HAFR,CWAN(HANUM),CWAD

        ! For grazing 
        lwph = lfwt * hafr
        laph = lapd * hafr
        swph = stwt * hafr
        rswph = rswt * hafr
        lnph = leafn * hafr
        snph = stemn * hafr
        rsnph = rsn * hafr
        
    END SUBROUTINE CS_Growth_Init