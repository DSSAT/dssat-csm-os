!***************************************************************************************************************************
! This is the code from the section (DYNAMIC == INTEGR) lines 6731 - 6900 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!    
! Subroutine YCA_Out_ModFail covers model failure. 
!***************************************************************************************************************************
    
    SUBROUTINE YCA_Out_ModFail ( &
        BRSTAGE     , LAI        , DYNAMIC     , KCAN        &
        )
        
        USE ModuleDefs
        USE YCA_First_Trans_m
        USE YCA_Formats_m
        USE YCA_Control_Environment
        USE YCA_Control_Leaf
     
        IMPLICIT NONE 
        EXTERNAL CSTIMDIF, CSOPLINE
     
        INTEGER :: DYNAMIC     
        INTEGER :: CSTIMDIF                                                                      ! Integer function calls

        REAL    :: BRSTAGE     , LAI        , KCAN         

        ! If model failure so that cycle not completed
        IF (DYNAMIC == SEASEND .AND. SEASENDOUT /= 'Y') THEN
            laix = -99.0
            cwahc = -99.0
            nupac = -99.0
            hwam = -99.0
            hiam = -99.0
            sennatc = -99.0
        ENDIF
        
        DAS = MAX(0,CSTIMDIF(YEARSIM,YEARDOY))
        SLAOUT = -99.0
        ! Note possibilities. To change must recompile.
        IF (OUTCHOICE == 1) THEN
            ! 1. Include reserves
            !LLWADOUT = LLWAD+LLRSWAD !LPM 21MAY2015 The reserves distribution will not be included, it needs to be reviewed
            !STWADOUT = STWAD+STRSWAD + LPEWAD+LPERSWAD
            !CRWADOUT = CRWAD+CRRSWAD
            LLWADOUT = LLWAD
            STWADOUT = STWAD+LPEWAD
            SDWADOUT = SDWAD
            !IF (LFWT > 1.0E-6) SLAOUT=(plantGreenLeafArea())/(LFWT*(1.0-LPEFR)+LLRSWT)  !LPM 21MAY2015 The reserves distribution will not be included, it needs to be reviewed
            IF (LFWT > 1.0E-6) SLAOUT=(plantGreenLeafArea())/(LFWT*(1.0-LPEFR)) 
        ELSEIF (OUTCHOICE == 2) THEN
            ! 2. No reserves, stem wt includes petioles
            LLWADOUT = LLWAD
            STWADOUT = STWAD + LPEWAD
            SDWADOUT = SDWAD
            IF (LFWT > 1.0E-6)SLAOUT=(plantGreenLeafArea())/(LFWT*(1.0-LPEFR))
        ELSEIF (OUTCHOICE == 3) THEN
            ! 3. No reserves, stem wt does not includes petioles
            LLWADOUT = LLWAD
            STWADOUT = STWAD
            SDWADOUT = SDWAD
            IF (LFWT > 1.0E-6)SLAOUT=(plantGreenLeafArea())/(LFWT*(1.0-LPEFR))
        ENDIF
        IF (SLA <= 0.0) SLAOUT = -99.0
        
        CALL Csopline(sentoplitterac,(sentoplittera))
        CALL Csopline(senrootc,(senroota))
        CALL Csopline(laic,lai)
        CALL Csopline(caic,caid)
        CALL Csopline(hindc,hind)
        ! CALL Csopline(hwudc,hwud) !issue 50
        CALL Csopline(sdwtc,sdwt)
        CALL Csopline(brstagec,brstage)
        
        ! Calculate Pari to equate to updated LAI
        PARIOUT =    calculatePortionOfRadiation(KCAN, LAI)
    END SUBROUTINE YCA_Out_ModFail
        
        
