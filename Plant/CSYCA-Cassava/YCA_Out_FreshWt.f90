!***************************************************************************************************************************
! This is the code from the section (DYNAMIC == INTEGR) lines 6901 - 7148 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! SUBROUTINE YCA_Out_PlGrow covers outputs of plant growth factors (Plantgro, gr2, grf, N) (IDETG  /=  N). 
!***************************************************************************************************************************
    
    SUBROUTINE YCA_Out_FreshWt ( & 
        DOY         ,  IDETG       , IDETL       , RUN         , YEAR        &
        )
        
        USE ModuleDefs
        USE YCA_First_Trans_m
        USE YCA_Formats_m
        USE YCA_Control_Plant
     
        IMPLICIT NONE 
        EXTERNAL HEADER
     
        INTEGER :: DOY         , RUN         , YEAR
        

        CHARACTER(LEN=1)  :: IDETG       , IDETL            
        
        !---------------------------------------------------------------------------------------------------------------
        !         Fresh weight output (for IDETG NE N )
        !---------------------------------------------------------------------------------------------------------------     
            
        IF ((IDETG /= 'N'.AND.IDETL /= '0').OR.IDETL == 'A') THEN
                
            ! PlantGro
            IF (YEARDOY == PLYEARDOY) THEN
                OPEN (UNIT = NOUTPF, FILE = OUTPG3,POSITION = 'APPEND')
                CALL HEADER(2, NOUTPF, RUN)

                WRITE (NOUTPF, FMT2202)
            ENDIF  ! End FreshWt header writes
            WRITE (NOUTPF, FMT504)YEAR,DOY,DAS,DAP,TMEAN,BRSTAGEC,LNUM, NINT(HWAD), NINT(FHWAD), &
                PDMCD,HIAD, Tfdmc, 1.0-WFG
            ! End FreshWt writes
        ENDIF 
    
    END SUBROUTINE YCA_Out_FreshWt
            
            


