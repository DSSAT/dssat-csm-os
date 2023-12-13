!***************************************************************************************************************************
! This is the code from the section (DYNAMIC == INTEGR) lines 6205 - 6304 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine YCA_Integ_HstFail determines whether to harvest or fail. 
!***************************************************************************************************************************
    
    SUBROUTINE YCA_Integ_HstFail ( &   
        BRSTAGE     , DOY         , STGYEARDOY  , SW          , YEAR        , LAI       & 
        )
        
        USE ModuleDefs
        USE YCA_First_Trans_m
        
        IMPLICIT NONE
        EXTERNAL WARNING
        
        INTEGER DOY         , STGYEARDOY(0:19)            , YEAR  
        REAL    BRSTAGE     , SW(NL)      , LAI  
        
    ! Harvesting conditions
        !IF (IHARI == 'A' .AND. CUMDU >= PSTART(MSTG)) THEN !LPM 04MAR15 MSTG TO PSX
        IF (IHARI == 'A' .AND. DABR >= PSTART(PSX)) THEN !LPM 23JUL19 Use DABR instead of CUMDU
            ! Here need to check out if possible to harvest.
            IF (YEARDOY >= HFIRST) THEN
                IF (SW(1) >= SWPLTL.AND.SW(1) <= SWPLTH) YEARDOYHARF=YEARDOY
            ENDIF
            ! Check if past earliest date; check if not past latest date
            ! Check soil water
            ! If conditions met set YEARDOYHARF = YEARDOY
            ! (Change YEARDOYHARF to more something more appropriate)
        ENDIF
            
        ! Determine if crop failure LPM02OCT2019 Modified to 30 DAP
        IF (DAP >= 30 .AND. GESTAGE < 1.0) THEN
            CFLFAIL = 'Y'
            WRITE (Message(1),'(A40)') 'No germination within 30 days of sowing '
            CALL WARNING(1,'CSYCA',MESSAGE)
        ENDIF
        !IF (IHARI /= 'A'.AND.CUMDU >= PSTART(MSTG-1)) THEN !LPM 04MAR15 MSTG TO PSX
        !IF (IHARI /= 'A'.AND.CUMDU >= PSTART(PSX-1)) THEN !LPM 24APR2016 using DABR including water stress
        IF (IHARI /= 'A'.AND.DABR >= PSTART(PSX-1)) THEN
            ! NB. Not work if MSTG=2
            !IF (TT20 <= -98.0.AND.PSTART(MSTG-1) > 0.0) THEN !LPM 04MAR15 MSTG TO PSX
            IF (TT20 <= -98.0.AND.PSTART(PSX-1) > 0.0) THEN
                CFLFAIL = 'Y'
                WRITE (Message(1),'(A28)') '20day thermal time mean = 0 '
                CALL WARNING(1,'CSYCA',MESSAGE)
            ENDIF
        ENDIF
        ! Determine if to harvest
        CFLHAR = 'N'
        IF (IHARI == 'R'.AND.YEARDOY == YEARDOYHARF.OR. IHARI == 'D'.AND.YEARDOYHARF == DAP .OR. IHARI == 'G'.AND. &
            YEARDOYHARF <= BRSTAGE .OR. IHARI == 'A'.AND.YEARDOY >= YEARDOYHARF.OR. IHARI == 'M'.AND.DABR >=  &   
            PSTART(PSX)) THEN 
            !YEARDOYHARF <= BRSTAGE .OR. IHARI == 'A'.AND.YEARDOYHARF == YEARDOY .OR. IHARI == 'M'.AND.CUMDU >=  & !LPM 24APR2016 Using DABR instead of CUMDU
            !PSTART(MSTG)) THEN !LPM 04MAR15 MSTG TO PSX
             
            CFLHAR = 'Y'
        ENDIF
        IF(IHARI == 'R'.AND.CFLHAR == 'N')THEN
            !IF (CUMDU > PSTART(MSTG) .AND. CFLHARMSG  /=  'Y') THEN !LPM 04MAR15 MSTG TO PSX !LPM 24APR2016
            IF (DABR > PSTART(PSX) .AND. CFLHARMSG  /=  'Y') THEN
           
                WRITE(Message(1),'(A68,I7)') 'Maximum branching level reached but waiting for reported harvest on: ', YEARDOYHARF !LPM 04MAR15 Maybe this section it is not necessary for cassava
                CALL WARNING(1,'CSYCA',MESSAGE)
                CFLHARMSG = 'Y'
            ENDIF
        ENDIF
                
        IF (CFLFAIL == 'Y' .OR. CFLHAR == 'Y') THEN
                    
            IF (CFLFAIL == 'Y' .AND. BRSTAGE <= PSX+2 .AND. BRSTAGE >= 0 ) THEN       
                STGYEARDOY(PSX+2) = YEARDOY
                TMAXPAV(PSX+2) = TMAXPAV(BRSTAGEINT)
                TMINPAV(PSX+2) = TMINPAV(BRSTAGEINT)
                SRADPAV(PSX+2) = SRADPAV(BRSTAGEINT)
                DAYLPAV(PSX+2) = DAYLPAV(BRSTAGEINT)
                RAINPAV(PSX+2) = RAINPAV(BRSTAGEINT)
                CO2PAV(PSX+2) = CO2PAV(BRSTAGEINT)
                NFPPAV(PSX+2) = NFPPAV(BRSTAGEINT)
                WFPPAV(PSX+2) = WFPPAV(BRSTAGEINT)
                WFGPAV(PSX+2) = WFGPAV(BRSTAGEINT)
                NFGPAV(PSX+2) = NFGPAV(BRSTAGEINT)
            ENDIF
            IF (CFLFAIL == 'Y') THEN
                STGYEARDOY(PSX) = -99
            ELSE
                STGYEARDOY(PSX) = YEARDOY  ! Harvest
            ENDIF
            STGYEARDOY(PSX+1) = YEARDOY  ! Crop End
            ! IF (HSTG > 0) THEN
            !    PSDAPFR(HSTG) = FLOAT(DAP)
            !    PSDAP(HSTG) = DAP
            ! ENDIF  
            ! IF (MSTG > 0.AND.PSDAPFR(MSTG) <= 0.0) THEN
            !    PSDAPFR(MSTG) = FLOAT(DAP)
            !    PSDAP(MSTG) = DAP
            ! ENDIF
            HADOY = DOY
            HAYEAR = YEAR
            CWADSTG(INT(10)) = CWAD
            LAISTG(INT(10)) = LAI
            LNUMSTG(INT(10)) = LNUM
            CNADSTG(INT(10)) = CNAD
            IF (MDAYFR < 0.0) THEN
                IF (CFLFAIL == 'Y') THEN
                    WRITE(Message(1),'(A26)') 'Harvest/failure triggered '                 
                    CALL WARNING(1,'CSYCA',MESSAGE)
                ENDIF  
            ENDIF
        ENDIF
        
    END SUBROUTINE YCA_Integ_HstFail
                
