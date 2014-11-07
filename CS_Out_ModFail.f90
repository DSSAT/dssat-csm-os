!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.INTEGR) lines 6731 - 6900 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in Module_CSCAS_Vars_List. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!    
! Subroutine CS_Out_ModFail covers model failure. 
!***************************************************************************************************************************
    
    SUBROUTINE CS_Out_ModFail ( &
        BRSTAGE     , CAID        , DYNAMIC     , KCAN        &
        )
        
        USE Module_CSCAS_Vars_List
        USE Module_CS_Formats
     
        IMPLICIT NONE 
     
        INTEGER :: DYNAMIC     
        INTEGER :: CSTIMDIF                                                                      ! Integer function calls

        REAL    :: BRSTAGE     , CAID        , KCAN         

        ! If model failure so that cycle not completed
        IF (DYNAMIC.EQ.SEASEND .AND. SEASENDOUT.NE.'Y') THEN
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
        IF (OUTCHOICE.EQ.1) THEN
            ! 1. Include reserves
            LLWADOUT = LLWAD+LLRSWAD
            STWADOUT = STWAD+STRSWAD + LPEWAD+LPERSWAD
            CRWADOUT = CRWAD+CRRSWAD
            IF (LFWT.GT.1.0E-6) SLAOUT=(PLA-SENLA-LAPHC)/(LFWT*(1.0-LPEFR)+LLRSWT)
        ELSEIF (OUTCHOICE.EQ.2) THEN
            ! 2. No reserves, stem wt includes petioles
            LLWADOUT = LLWAD
            STWADOUT = STWAD + LPEWAD
            CRWADOUT = CRWAD
            IF (LFWT.GT.1.0E-6)SLAOUT=(PLA-SENLA-LAPHC)/(LFWT*(1.0-LPEFR))
        ELSEIF (OUTCHOICE.EQ.3) THEN
            ! 3. No reserves, stem wt does not includes petioles
            LLWADOUT = LLWAD
            STWADOUT = STWAD
            CRWADOUT = CRWAD
            IF (LFWT.GT.1.0E-6)SLAOUT=(PLA-SENLA-LAPHC)/(LFWT*(1.0-LPEFR))
        ENDIF
        
        IF (SLA.LE.0.0) SLAOUT = -99.0
        
        CALL Csopline(sentoplitterac,(sentoplittera))
        CALL Csopline(senrootc,(senroota))
        CALL Csopline(laic,lai)
        CALL Csopline(caic,caid)
        CALL Csopline(hindc,hind)
        CALL Csopline(hwudc,hwud)
        CALL Csopline(sdwadc,sdwad)
        CALL Csopline(brstagec,brstage)
        
        ! Calculate Pari to equate to updated LAI
        PARIOUT = (1.0 - EXP((-KCAN)*LAI))
    END SUBROUTINE CS_Out_ModFail
        
        
