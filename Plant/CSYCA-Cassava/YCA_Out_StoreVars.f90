!***************************************************************************************************************************
! This is the code from the section (DYNAMIC == INTEGR) lines 9459 - 9573 of the original CSCAS code.  The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine YCA_Out_StoreVars stores variables for sending to CSM summary output routines. 
!***************************************************************************************************************************
    
    SUBROUTINE YCA_Out_StoreVars ( & 
        IDETO       , ISWNIT      , STGYEARDOY  &
        )
        
        USE ModuleDefs                                                                        ! MF 31AU14 Added for access to WEATHER
        USE YCA_First_Trans_m
        USE YCA_Formats_m
     
        IMPLICIT NONE 
        EXTERNAL SUMVALS
     
        INTEGER :: STGYEARDOY(0:19)            
        
        CHARACTER(LEN=1)  :: IDETO       , ISWNIT       
        
        !-----------------------------------------------------------------------------------------------------------
        !         Store variables for sending to CSM summary output routines
        !-----------------------------------------------------------------------------------------------------------
                
        ! Store summary labels and values in arrays to send to
        ! CSM OPSUM routine for printing.  Integers are temporarily
        ! saved as real numbers for placement in real array.
                
        IF (IDETO == 'E'.OR.IDETO == 'N') THEN
            ! Resource productivity calculations 
            ! (Nnot done earlier because no Overview called for)
            DMP_Rain = -99.
            GrP_Rain = -99.
            DMP_ET = -99.
            GrP_ET = -99.
            DMP_EP = -99.
            GrP_EP = -99.
            DMP_Irr = -99.    
            GrP_Irr = -99.
            DMP_NApp = -99.
            GrP_NApp = -99.
            DMP_NUpt = -99.
            GrP_NUpt = -99.
            IF (RAINCC > 1.E-3) THEN
                DMP_Rain = CWAM / RAINCC 
                GrP_Rain = HWAM  / RAINCC
            ENDIF
            IF (ETCC > 1.E-3) THEN
                DMP_ET = CWAM / ETCC 
                GrP_ET = HWAM  / ETCC 
            ENDIF
            IF (EPCC > 1.E-3) THEN
                DMP_EP = CWAM / EPCC 
                GrP_EP = HWAM  / EPCC 
            ENDIF
            IF (IRRAMTC > 1.E-3) THEN
                DMP_Irr = CWAM / IRRAMTC 
                GrP_Irr = HWAM  / IRRAMTC
            ENDIF
            IF (ISWNIT /= 'N') THEN
                IF (Amtnit > 1.E-3) THEN
                    DMP_NApp = CWAM / Amtnit
                    GrP_NApp = HWAM  / Amtnit
                ENDIF
                IF (NUPAC > 1.E-3) THEN
                    DMP_NUpt = CWAM / NUPAC
                    GrP_NUpt = HWAM  / NUPAC
                ENDIF
            ENDIF ! ISWNIT NE 'N'
        ENDIF
                
        LABEL(1) = 'ADAT'; VALUE(1) = -99.0
        !IF (stgyeardoy(mstg) < 9999999) THEN                     !LPM  07MAR15 MSTG TO PSX
        !    LABEL(2) = 'MDAT'; VALUE(2) = FLOAT(stgyeardoy(mstg))
        IF (stgyeardoy(PSX) < 9999999) THEN
            LABEL(2) = 'MDAT'; VALUE(2) = FLOAT(stgyeardoy(PSX))
        ELSE
            LABEL(2) = 'MDAT'; VALUE(2) = -99.0
        ENDIF
        LABEL(3) = 'DWAP'; VALUE(3) = sdrate
        LABEL(4) = 'CWAM'; VALUE(4) = cwam
        LABEL(5) = 'HWAM'; VALUE(5) = hwam
        LABEL(6) = 'HWAH '; VALUE(6) = hwam * hpcf/100.0
        LABEL(7) = 'BWAH '; VALUE(7) = vwam * hbpcf/100.0
        LABEL(8) = 'HWUM'; VALUE(8) = hwum
        LABEL(9) = 'H#AM'; VALUE(9) = hnumam
        LABEL(10) = 'H#UM'; VALUE(10) = hnumgm
        LABEL(11) = 'NUCM'; VALUE(11) = nupac
        LABEL(12) = 'CNAM'; VALUE(12) = cnam
        LABEL(13) = 'GNAM'; VALUE(13) = hnam
        LABEL(14) = 'PWAM'; VALUE(14) = -99.0
        LABEL(15) = 'LAIX'; VALUE(15) = laix
        LABEL(16) = 'HIAM'; VALUE(16) = hiam
                
        LABEL(17) = 'DMPPM'; VALUE(17) = DMP_Rain 
        LABEL(18) = 'DMPEM'; VALUE(18) = DMP_ET                     
        LABEL(19) = 'DMPTM'; VALUE(19) = DMP_EP                     
        LABEL(20) = 'DMPIM'; VALUE(20) = DMP_Irr
        LABEL(21) = 'DPNAM'; VALUE(21) = DMP_NApp
        LABEL(22) = 'DPNUM'; VALUE(22) = DMP_NUpt
                
        LABEL(23) = 'YPPM ' ; VALUE(23) = GrP_Rain                  
        LABEL(24) = 'YPEM ' ; VALUE(24) = GrP_ET                   
        LABEL(25) = 'YPTM ' ; VALUE(25) = GrP_EP                    
        LABEL(26) = 'YPIM ' ; VALUE(26) = GrP_Irr
        LABEL(27) = 'YPNAM' ; VALUE(27) = GrP_NApp
        LABEL(28) = 'YPNUM' ; VALUE(28) = GrP_NUpt
        LABEL(29) = 'EDAT ' ; VALUE(29) = FLOAT(EYEARDOY)     
                
        LABEL(30) = 'NDCH ' ; VALUE(30) = FLOAT(CDAYS) 
        LABEL(31) = 'TMINA' ; VALUE(31) = TMINCAV       
        LABEL(32) = 'TMAXA' ; VALUE(32) = TMAXCAV       
        LABEL(33) = 'SRADA' ; VALUE(33) = SRADCAV       
        LABEL(34) = 'DAYLA' ; VALUE(34) = DAYLCAV       
        LABEL(35) = 'CO2A ' ; VALUE(35) = CO2CAV        
        LABEL(36) = 'PRCP ' ; VALUE(36) = RAINCC       
        LABEL(37) = 'ETCP ' ; VALUE(37) = ETCC 
        
        !LPM 20APR2021 Add fresh weight variables
        LABEL(38) = 'FCWAM' ; VALUE(38) = -99.0
        LABEL(39) = 'FHWAM' ; VALUE(39) = FHWAM
        LABEL(40) = 'HWAHF' ; VALUE(40) = FHWAM * hpcf/100.0
        !FBWAH Multiplied by 10. in OPSUM
        LABEL(41) = 'FBWAH' ; VALUE(41) = -9.9 
        LABEL(42) = 'FPWAM' ; VALUE(42) = -99.0
                
        IF (FILEIOT(1:2) == 'DS') CALL SUMVALS (SUMNUM, LABEL, VALUE)
    
    END SUBROUTINE YCA_Out_StoreVars     
