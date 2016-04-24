!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.INTEGR) lines 6205 - 6304 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement. The variables 
! that are not arguments are declared in module CS_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine CS_Integ_HstFail determines whether to harvest or fail. 
!***************************************************************************************************************************
    
    SUBROUTINE CS_Integ_HstFail ( &   
        BRSTAGE     , DOY         , STGYEARDOY  , SW          , YEAR        & 
        )
        
        USE ModuleDefs
        USE CS_First_Trans_m
        
        IMPLICIT NONE
        
        INTEGER DOY         , STGYEARDOY(0:19)            , YEAR 
        REAL    BRSTAGE     , SW(NL)        
        
    ! Harvesting conditions
        !IF (IHARI.EQ.'A' .AND. CUMDU.GE.PSTART(MSTG)) THEN !LPM 04MAR15 MSTG TO PSX
        IF (IHARI.EQ.'A' .AND. CUMDU.GE.PSTART(PSX)) THEN
            ! Here need to check out if possible to harvest.
            IF (YEARDOY.GE.HFIRST) THEN
                IF (SW(1).GE.SWPLTL.AND.SW(1).LE.SWPLTH) YEARDOYHARF=YEARDOY
            ENDIF
            ! Check if past earliest date; check if not past latest date
            ! Check soil water
            ! If conditions met set YEARDOYHARF = YEARDOY
            ! (Change YEARDOYHARF to more something more appropriate)
        ENDIF
            
        ! Determine if crop failure
        IF (DAP.GE.90 .AND. GESTAGE.LT.1.0) THEN
            CFLFAIL = 'Y'
            WRITE (Message(1),'(A40)') 'No germination within 90 days of sowing '
            CALL WARNING(1,'CSCGR',MESSAGE)
        ENDIF
        IF (IHARI.NE.'A'.AND.MDAT.GE.0.AND.DAP-MDAP.GE.300) THEN
            CFLFAIL = 'Y'
            WRITE (Message(1),'(A32)')'300 days after maturity         '
            WRITE (Message(2),'(A21)')'Harvesting triggered.'
            CALL WARNING(2,'CSCGR',MESSAGE)
        ENDIF
        !IF (IHARI.NE.'A'.AND.CUMDU.GE.PSTART(MSTG-1)) THEN !LPM 04MAR15 MSTG TO PSX
        !IF (IHARI.NE.'A'.AND.CUMDU.GE.PSTART(PSX-1)) THEN !LPM 24APR2016 using DABR including water stress
        IF (IHARI.NE.'A'.AND.DABR.GE.PSTART(PSX-1)) THEN
            ! NB. Not work if MSTG=2
            !IF (TT20.LE.-98.0.AND.PSTART(MSTG-1).GT.0.0) THEN !LPM 04MAR15 MSTG TO PSX
            IF (TT20.LE.-98.0.AND.PSTART(PSX-1).GT.0.0) THEN
                CFLFAIL = 'Y'
                WRITE (Message(1),'(A28)') '20day thermal time mean = 0 '
                CALL WARNING(1,'CSCGR',MESSAGE)
            ENDIF
        ENDIF
        ! Determine if to harvest
        CFLHAR = 'N'
        IF (IHARI.EQ.'R'.AND.YEARDOYHARF.EQ.YEARDOY .OR. IHARI.EQ.'D'.AND.YEARDOYHARF.EQ.DAP .OR. IHARI.EQ.'G'.AND. &
            YEARDOYHARF.LE.BRSTAGE .OR. IHARI.EQ.'A'.AND.YEARDOYHARF.EQ.YEARDOY .OR. IHARI.EQ.'M'.AND.DABR.GE. &   
            PSTART(PSX)) THEN 
            !YEARDOYHARF.LE.BRSTAGE .OR. IHARI.EQ.'A'.AND.YEARDOYHARF.EQ.YEARDOY .OR. IHARI.EQ.'M'.AND.CUMDU.GE. & !LPM 24APR2016 Using DABR instead of CUMDU
            !PSTART(MSTG)) THEN !LPM 04MAR15 MSTG TO PSX
             
            CFLHAR = 'Y'
        ENDIF
        IF(IHARI.EQ.'R'.AND.CFLHAR.EQ.'N')THEN
            !IF (CUMDU.GT.PSTART(MSTG) .AND. CFLHARMSG .NE. 'Y') THEN !LPM 04MAR15 MSTG TO PSX !LPM 24APR2016
            IF (DABR.GT.PSTART(PSX) .AND. CFLHARMSG .NE. 'Y') THEN
           
                WRITE(Message(1),'(A54,I7)') 'Maturity reached but waiting for reported harvest on: ', YEARDOYHARF !LPM 04MAR15 Maybe this section it is not necessary for cassava
                CALL WARNING(1,'CSCGR',MESSAGE)
                CFLHARMSG = 'Y'
            ENDIF
        ENDIF
                
        IF (CFLFAIL.EQ.'Y' .OR. CFLHAR.EQ.'Y') THEN
                    
            IF (CFLFAIL.EQ.'Y' .AND. BRSTAGE <= 12 .AND. BRSTAGE > 0 ) THEN       
                STGYEARDOY(12) = YEARDOY
                TMAXPAV(12) = TMAXPAV(INT(BRSTAGE))
                TMINPAV(12) = TMINPAV(INT(BRSTAGE))
                SRADPAV(12) = SRADPAV(INT(BRSTAGE))
                DAYLPAV(12) = DAYLPAV(INT(BRSTAGE))
                RAINPAV(12) = RAINPAV(INT(BRSTAGE))
                CO2PAV(12) = CO2PAV(INT(BRSTAGE))
                NFPPAV(12) = NFPPAV(INT(BRSTAGE))
                WFPPAV(12) = WFPPAV(INT(BRSTAGE))
                WFGPAV(12) = WFGPAV(INT(BRSTAGE))
                NFGPAV(12) = NFGPAV(INT(BRSTAGE))
            ENDIF
            STGYEARDOY(10) = YEARDOY  ! Harvest
            STGYEARDOY(11) = YEARDOY  ! Crop End
            ! IF (HSTG.GT.0) THEN
            !    PSDAPFR(HSTG) = FLOAT(DAP)
            !    PSDAP(HSTG) = DAP
            ! ENDIF  
            ! IF (MSTG.GT.0.AND.PSDAPFR(MSTG).LE.0.0) THEN
            !    PSDAPFR(MSTG) = FLOAT(DAP)
            !    PSDAP(MSTG) = DAP
            ! ENDIF
            HADOY = DOY
            HAYEAR = YEAR
            CWADSTG(INT(10)) = CWAD
            LAISTG(INT(10)) = LAI
            LNUMSTG(INT(10)) = LNUM
            CNADSTG(INT(10)) = CNAD
            IF (MDAYFR.LT.0.0) THEN
                IF (CFLFAIL.EQ.'Y') THEN
                    WRITE(Message(1),'(A26)') 'Harvest/failure triggered '                 
                    CALL WARNING(1,'CSCGR',MESSAGE)
                ENDIF  
            ENDIF
        ENDIF
        
    END SUBROUTINE CS_Integ_HstFail
                
