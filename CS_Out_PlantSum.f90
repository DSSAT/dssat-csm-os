!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.INTEGR) lines 8316 - 8386 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module CS_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine CS_Out_PlantSum outputs data in PlantSum.OUT.
!***************************************************************************************************************************
    
    SUBROUTINE CS_Out_PlantSum ( & 
        CN          , IDETL       , IDETS       , ON          , REP         , RN          , RUN         , RUNI        , &
        SN          , TN          &
        )
        
    USE CS_First_Trans_m
    USE CS_Formats_m
     
    IMPLICIT NONE 
     
    INTEGER :: CN          , ON          , REP         , RN          , RUN         , RUNI        , SN          , TN          

    CHARACTER(LEN=1)  :: IDETL       , IDETS       
    
    !-----------------------------------------------------------------------------------------------------------
    !         IDETS OUTPUTS (Plantsum)
    !-----------------------------------------------------------------------------------------------------------
                
    IF ((IDETS.NE.'N'.AND.IDETL.NE.'0').OR.IDETL.EQ.'A') THEN
                    
        ! PLANT SUMMARY (SIMULATED)'
        IF (CROP.NE.CROPPREV.OR.RUN.EQ.1) THEN
            OPEN (UNIT=fnumpsum,FILE=FNAMEPSUM,POSITION='APPEND')
            WRITE (FNUMPSUM,'(/,A)') '*SUMMARY'
            WRITE (FNUMPSUM,'(3A)',ADVANCE='NO') '@  RUN EXCODE    TRNO RN',' TNAME....................', &
                ' REP  RUNI S O C    CR PYEAR  PDAT'
            DO L = 1,KEYSTX
                IF (KEYPS(L).GT.0) THEN
                    WRITE (FNUMPSUM,'(A6)',ADVANCE='NO') PSABVO(KEYPS(L))
                    !IF (PSABVO(KEYPS(L)).EQ.'TSDAP') THEN
                    ! WRITE (FNUMPSUM,'(A6)',ADVANCE='NO') '  DAYL'
                    !ENDIF
                ENDIF
            ENDDO
            WRITE (FNUMPSUM,'(4A)') &
                '   FLN FLDAP HYEAR  HDAY SDWAP',' CWAHC  CWAM PARUE  HWAM  HWAH  VWAM  HWUM  H#AM  H#UM', &
                ' SDNAP  CNAM  HNAM  RNAM  TNAM  NUCM  HN%M  VN%M',' D1INI D2INI D3INI '
            CLOSE(fnumpsum)  
        ENDIF  ! End of Plantsum.Out headers
        OPEN (UNIT=fnumpsum,FILE=FNAMEPSUM,POSITION='APPEND')
        WRITE (fnumpsum, FMT400, ADVANCE='NO') run,excode,tn,rn,tname,rep,runi,sn,on,cn,crop,plyear,plday
        DO L = 1,KEYSTX
            IF (KEYPS(L).GT.0) THEN
                WRITE (FNUMPSUM,'(I6)',ADVANCE='NO') PSDAP(KEYPS(L))
                IF (PSABVO(KEYPS(L)).EQ.'TSDAP') THEN
                    WRITE (FNUMPSUM,'(F6.1)',ADVANCE='NO') DAYLST(L)
                ENDIF  
            ENDIF
        ENDDO
        WRITE (fnumpsum, FMT401)FLN, FLDAP,hayear,hadoy,NINT(sdrate),NINT(cwahc),NINT(cwam),pariued,NINT(hwam), &
            NINT(hwam*hpcf/100.0),NINT(vwam),hwumchar,NINT(hnumam),NINT(hnumgm),sdnap,NINT(cnam),NINT(hnam), &
            NINT(rnam),NINT(AMAX1(-99.0,cnam+rnam)),NINT(nupac),hnpcmchar,vnpcmchar,didoy(1),didoy(2),didoy(3)
            CLOSE(fnumpsum)  
    ELSE  
        OPEN (UNIT=FNUMPSUM,FILE=FNAMEPSUM,STATUS='UNKNOWN')
        CLOSE (UNIT=FNUMPSUM, STATUS = 'DELETE')
    ENDIF
    ! End IDETS Outputs (Plantsum.Out)          
                

    END SUBROUTINE CS_Out_PlantSum
        

