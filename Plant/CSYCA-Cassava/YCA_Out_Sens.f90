!***************************************************************************************************************************
! This is the code from the section (DYNAMIC == INTEGR) lines 9351 - 9458 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
!Subroutine YCA_Out_Sens outputs screens for sensitivity mode. 
!***************************************************************************************************************************
    
    SUBROUTINE YCA_Out_Sens ( & 
        CN          , DOY         , RNMODE      , STGYEARDOY  , TN          , YEAR        , LAI      &
        )
        
        USE ModuleDefs                                                                        ! MF 31AU14 ADDED FOR ACCESS TO WEATHER
        USE YCA_First_Trans_m
        USE YCA_Formats_m
     
        IMPLICIT NONE 
        EXTERNAL YR_DOY, CSOPLINE, CALENDAR, DAPCALC, CSCLEAR5

        INTEGER :: CN          , DOY         , STGYEARDOY(0:19)            , TN          , YEAR
        INTEGER :: DAPCALC                                                                    ! Integer function calls
        
        REAL    :: CNCTMP                  ! Canopy N concentration,tempry  %          ! (From Output) 
        REAL    :: LAI
        CHARACTER(LEN=1)  :: RNMODE      

        !-----------------------------------------------------------------------------------------------------------
        !         Screens for sensitivity mode
        !-----------------------------------------------------------------------------------------------------------
        
        IF (FILEIOT(1:3) == 'DS4' .AND. CN == 1.AND. RNMODE == 'E') THEN         
            CALL CSCLEAR5
            WRITE(*,*) ' '
            DO L = 0, PSNUM
                CALL Csopline(laic,laistg(l))
                IF (STGYEARDOY(L) < 9999999.AND.L /= PSX.AND.L /= PSX+1) THEN
!                    CALL CSYR_DOY(STGYEARDOY(L),YEAR,DOY)
                    CALL YR_DOY(STGYEARDOY(L),YEAR,DOY)
                    CALL Calendar(year,doy,dom,month)
                    CNCTMP = 0.0
                    IF (CWADSTG(L) > 0.0)CNCTMP = CNADSTG(L)/CWADSTG(L)*100
                    WRITE (*,'(I8,I4,1X,A3,I4,1X,I1,1X,A13,I6,A6,F6.1,I6,F6.2,F6.2,F6.2)')STGYEARDOY(L),DOM,MONTH, &
                        Dapcalc(stgyeardoy(L),(plyeardoy/1000),plday),l,psname(l),NINT(CWADSTG(L)),LAIC,LNUMSTG(L), &
                        NINT(CNADSTG(L)),CNCTMP,1.0-WFPPAV(L),1.0-NFPPAV(L)
                ENDIF
            ENDDO
            ! For harvest at specified date
            IF (YEARDOYHARF == YEARDOY) THEN
                CALL Csopline(laic,lai)
!                CALL CSYR_DOY(YEARDOYHARF,YEAR,DOY)
                CALL YR_DOY(YEARDOYHARF,YEAR,DOY)
                CALL Calendar(year,doy,dom,month)
                CNCTMP = 0.0
                IF (CWAD > 0.0)CNCTMP = CNAD/CWAD*100
                WRITE (*,'(I8,I4,1X,A3,I4,1X,I1,1X,A13,I6,A6,F6.1,I6,F6.2,F6.2,F6.2)') YEARDOY,DOM,MONTH,Dapcalc(yeardoy, &
                    (plyeardoy/1000),plday),l,'Harvest      ',NINT(CWAD),LAIC,LNUM,NINT(CNAD),CNCTMP,1.0-WFPCAV,1.0-NFPCAV
            ENDIF 
                    
            WRITE(*,*)' '
            WRITE(*,*)' Press ENTER to continue'
            PAUSE ' '
            CALL CSCLEAR5
                    
            !WRITE (*, FMT206)                                                                       !Test format
            WRITE(*,'(A36,A10,I3)') ' SIMULATED-MEASURED COMPARISONS FOR ',EXCODE,TN
            WRITE(*,*)' '
            !DO L = 1,KEYSTX
            DO L = 0,KEYSTX
                IF (KEYPS(L) > 0) WRITE (*, FMT291) psname(KEYPS(L)),psdap(KEYPS(L)),psdapm(KEYPS(L))
            ENDDO
            WRITE (*, FMT305)NINT(cwam),NINT(cwamm),MAX(-99,NINT(rwam+sdwam)),NINT(rwamm),NINT(senwacm),NINT(senwacmm), &
                NINT(hwam),NINT(hwamm),NINT(vwam),NINT(vwamm),hiam,hiamm, NINT(rswam),NINT(rswamm)
            IF (lwphc+swphc > 0.0) WRITE (*, FMT306) NINT(cwahc),NINT(cwahcm)
            WRITE(*,*)' '
            WRITE(*,*)' Press ENTER to continue'
            PAUSE ' '
            CALL CSCLEAR5
                    
            WRITE (*, FMT2061)
            WRITE (*, FMT307) hwumchar,hwummchar,NINT(hnumam),NINT(hnumamm),hnumgm,hnumgmm, laix,laixm,lnumsm,lnumsmm, &
                nupac,nupacm,cnam,cnamm,rnam,rnamm,sennatc,sennatcm,hnam,hnamm,vnam,vnamm,hinm,hinmm,hnpcm,hnpcmm, &
                vnpcm,vnpcmm
                    
            WRITE(*,*)' '
            WRITE(*,*)' Press ENTER to continue'
            PAUSE ' '
            CALL CSCLEAR5
            CALL CSCLEAR5
            CALL CSCLEAR5
            CALL CSCLEAR5
                    
        ENDIF ! END OF SCREEN WRITES FOR DSSAT SENSITIVITY MODE
    
    END SUBROUTINE YCA_Out_Sens