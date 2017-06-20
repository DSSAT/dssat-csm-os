!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.INTEGR) lines 6542 - 6649 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module CS_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine CS_Integ_EndCrop calculates available water and yields of plant parts at the end of the crop. 
!***************************************************************************************************************************
    
    SUBROUTINE CS_Integ_EndCrop ( &
        ALBEDOS     , DLAYR       , EOP         , FERNIT      , ISWWAT      , LL          , NLAYR       , STGYEARDOY  , &
        SW          & 
        )
        
        USE ModuleDefs
        USE CS_First_Trans_m
    
        IMPLICIT NONE
        
        CHARACTER(LEN=1) ISWWAT 
        
        INTEGER NLAYR       , STGYEARDOY(0:19)
        
        REAL    ALBEDOS     , DLAYR(NL)   , EOP         , FERNIT      , LL(NL)      , SW(NL)        
    
        !-----------------------------------------------------------------------
        !         Determine if nitrogen fertilizer applied
        !-----------------------------------------------------------------------
                
        IF (FERNIT.GT.FERNITPREV) THEN
            FAPPNUM = FAPPNUM + 1
            AMTNIT = FERNIT
            WRITE(fappline(fappnum),'(A1,I4,A10,I7,A13,I4,A6)') ' ',NINT(FERNIT-FERNITPREV),' kg/ha on ', YEARDOY, &
                '     To date ',NINT(amtnit),' kg/ha'
            FERNITPREV = FERNIT
        ENDIF
                
        !-----------------------------------------------------------------------
        !         Calculate water availability ratio
        !-----------------------------------------------------------------------
                
        BASELAYER = 0.0
        H2OA = 0.0
        IF (ISWWAT.NE.'N') THEN
            DO L = 1, NLAYR
                DLAYRTMP(L) = DLAYR(L)
                BASELAYER = BASELAYER + DLAYR(L)
                IF (RTDEP.GT.0.0.AND.RTDEP.LT.BASELAYER) THEN
                    DLAYRTMP(L) = RTDEP-(BASELAYER-DLAYR(L))
                    IF (DLAYRTMP(L).LE.0.0) EXIT
                ENDIF
                H2OA = H2OA + 10.0*AMAX1(0.0,(SW(L)-LL(L))*DLAYRTMP(L))
            ENDDO
            IF (EOP.GT.0.0) THEN
                WAVR = H2OA/EOP
            ELSE
                WAVR = 99.9
            ENDIF
        ENDIF
                
        !-----------------------------------------------------------------------
        !         Upgrade albedo
        !-----------------------------------------------------------------------
                
        ! When running in CSM
        IF (FILEIOT.EQ.'DS4') THEN
            IF (LAI .LE. 0.0) THEN
                ALBEDO = ALBEDOS
            ELSE
                ALBEDO = 0.23-(0.23-ALBEDOS)*EXP(-0.75*LAI)                                                            !EQN 073
            ENDIF
        ELSE
            ALBEDO = ALBEDOS  
        ENDIF
                
        !-----------------------------------------------------------------------
        !         Compute weights,etc. at end crop
        !-----------------------------------------------------------------------
                
        IF (STGYEARDOY(11).EQ.YEARDOY) THEN
                    
            ! LAH No adjustment for fraction of day to maturity
            RSWTM = RSWT
            RTWTM = RTWT
            LFWTM = LFWT
            STWTM = STWT
            CRWTM = CRWT                   
            LNUMSM = LNUM
                    
            IF (LFWTM+STWTM+CRWTM+RSWTM.GT.0.0) RSCM = RSWTM/(LFWTM+STWTM+CRWTM)
            IF (RTWTM.GT.0.0) SHRTM = (LFWTM+STWTM+CRWTM+RSWTM)/RTWTM
                    
            CWAM = (LFWTM+STWTM+CRWTM+RSWTM)*PLTPOP*10.0
            VWAM = (LFWTM+STWTM+CRWTM+RSWTM)*PLTPOP * 10.0
                    
            ! For Grazing
            cwahc = (lwphc+swphc+rswphc)*pltpop*10.0
            ! Adjustments for spikes that removed by grazing,etc..
                    
            RWAM = RTWTM*PLTPOP*10.0
            SDWAM = (SEEDRS+SDCOAT)*PLTPOP*10.0
                    
            IF (CWAM.GT.0.0) THEN
                HIAM = HIAD
            ENDIF
                    
            SENWACM = SENTOPLITTERA+SENROOTA
                    
            RSWAM = RSWAD
                    
            CNAM = CNAD
            VNAM = VNAD
            VNPCM = VANC*100.0
            RNAM = RNAD
            SRNAM = SRNAD
                    
            HINM = HIND
                    
            ! Set harvest product outputs
            HWAM = SRWT * PLTPOP * 10.0
            HNAM = SRNAM
            !IF (SRNOPD.GT.0.0) HWUM = SRWT/FLOAT(SRNOPD) !issue 50
            !HNUMAM = FLOAT(SRNOPD)*PLTPOP !issue 50
            !HNUMGM = FLOAT(SRNOPD)        !issue 50
            !HNUMPM = FLOAT(SRNOPD)        !issue 50
            BRNUMSH = BRNUMST
            IF (SRWT.GT.0.0) HNPCM = SROOTN/SRWT*100.0
                    
        ENDIF    
    
    END SUBROUTINE CS_Integ_EndCrop         
        
