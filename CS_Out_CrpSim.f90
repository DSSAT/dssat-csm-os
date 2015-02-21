!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.INTEGR) lines 9280 - 9350 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
<<<<<<< HEAD
! that are not arguments are declared in module CS_First_Trans_m. Unless identified as by MF, all comments are those of 
=======
! that are not arguments are declared in Module_CSCAS_Vars_List. Unless identified as by MF, all comments are those of 
>>>>>>> cassava-modifications
! the original CSCAS.FOR code.
!
! SUBROUTINE CS_Out_Crp_Sim outputs screens for CROPSIM SHELL (IDETD). 
!***************************************************************************************************************************
    
    SUBROUTINE CS_Out_CrpSim ( & 
        CN          , DOY         , ON          , REP         , RN          , RUN         , RUNI        , &
        SN          , STGYEARDOY  , TN          , YEAR        &
        )
        
<<<<<<< HEAD
        USE CS_First_Trans_m
        USE CS_Formats_m
=======
        USE Module_CSCAS_Vars_List
        USE Module_CS_Formats
>>>>>>> cassava-modifications
     
        IMPLICIT NONE 
     
        INTEGER :: CN          , DOY         , ON          , REP         , RN          , RUN         , RUNI         
        INTEGER :: SN          , STGYEARDOY(20)            , TN          , YEAR
        INTEGER :: DAPCALC                                                                    ! Integer function calls

        ! Screen writes
        IF (IDETD.EQ.'S') THEN
            IF (OUTCOUNT.LE.0) THEN
                CALL CSCLEAR5
                WRITE(*,*)' SIMULATION SUMMARY'
                WRITE(*,*)' '
                WRITE (*, FMT499)
            ENDIF
            IF (OUTCOUNT .EQ. 25) THEN
                OUTCOUNT = 1
            ELSE
                OUTCOUNT = OUTCOUNT + 1
            ENDIF
            WRITE (*, FMT410) run,excode,tn,rn,tname(1:25),rep,runi,sn,on,cn,crop,NINT(hwam)
        ELSEIF (IDETD.EQ.'M') THEN
            ! Simulation and measured data
            CALL CSCLEAR5
            WRITE(*,'(A20,A10,I3)')' STAGES SUMMARY FOR ',EXCODE,TN
            WRITE(*,*)' '
            !WRITE(*, FMT9600)
            DO L = 1, PSNUM
                CALL Csopline(laic,laistg(l))
                IF (STGYEARDOY(L).LT.9999999.AND.L.NE.10.AND.L.NE.11) THEN
                    CALL CSYR_DOY(STGYEARDOY(L),YEAR,DOY)
                    CALL Calendar(year,doy,dom,month)
                    CNCTMP = 0.0
                    IF (CWADSTG(L).GT.0.)CNCTMP = CNADSTG(L)/CWADSTG(L)*100
                    WRITE (*,'(I8,I4,1X,A3,I4,1X,I1,1X,A13,I6,A6,F6.1,I6,F6.2,F6.2,F6.2)')STGYEARDOY(L),DOM,MONTH, &
                        Dapcalc(stgyeardoy(L),plyear,plday),L,PSNAME(L),NINT(CWADSTG(L)),LAIC,LNUMSTG(L), &
                        NINT(CNADSTG(L)),CNCTMP,1.0-WFPPAV(L),1.0-NFPPAV(L)
                ENDIF
            ENDDO
            WRITE(*,*)' '
            WRITE(*,*)' Press ENTER to continue'
            PAUSE ' '
            CALL CSCLEAR5
            WRITE(*,'(A36,A10,I3)')' SIMULATED-MEASURED COMPARISONS FOR ',EXCODE,TN
            WRITE(*,*)' '
            !WRITE (*, FMT206)                                                                       !Test format
            !WRITE (*, FMT290) MAX(-99,gdap),MAX(-99,gdapm),MAX(-99,edap),MAX(-99,edapm)             !Test format
            DO L = 1,KEYSTX
                IF (KEYPS(L).GT.0) WRITE (*, FMT291)psname(KEYPS(L)),PSDap(KEYPS(L)),PSDapm(KEYPS(L))
            ENDDO
            WRITE (*, FMT305)NINT(cwam),NINT(cwamm),NINT(rwam+sdwam),NINT(rwamm),NINT(senwacm),NINT(senwacmm), &
                NINT(hwam),NINT(hwamm),NINT(vwam),NINT(vwamm),hiam,hiamm,NINT(rswam),NINT(rswamm)
        ENDIF ! End IDETD.EQ.'S' 
                
    END SUBROUTINE CS_Out_CrpSim
