!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.INTEGR) lines 5737 - 5874 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in Module_CSCAS_Vars_List. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine CS_Integ_LA calculates the reserve concentration, shoot and total leaf area. It updates senesced, harvested, green 
! leaf area, stems and petioles, plant height, and root depth and length. 
!***************************************************************************************************************************
    
    SUBROUTINE CS_Integ_LA ( &
        CAID        , CANHT       , DEPMAX      , DLAYR       , NLAYR       , RLV            & 
        )
        
        USE ModuleDefs
        USE Module_CSCAS_Vars_List
        
        IMPLICIT NONE
        
        INTEGER NLAYR
        
        REAL    CAID        , CANHT       , DEPMAX      , DLAYR(NL)   , RLV(NL)        
       
        !-----------------------------------------------------------------------
        !         Calculate reserve concentration
        !-----------------------------------------------------------------------
        
        IF (LFWT+STWT+CRWT.GT.0.0) RSCD = RSWT/(LFWT+STWT+CRWT+RSWT)
        IF (RSCD.LT.0.0.AND.RSCD.GT.-1.0E-7) RSCD = 0.0
        RSCX = AMAX1(RSCX,RSCD)
        
        !-----------------------------------------------------------------------
        !         Update shoot leaf area (Must be done before PLA updated)
        !-----------------------------------------------------------------------
        
        ! First for leaf senescence
        DO L = 1,INT(SHNUM+1)
            IF (SHNUM-FLOAT(L-1).GT.0.0) THEN
                IF (PLA-SENLA.GT.0.0) SHLAS(L) = SHLAS(L) + PLAS*(SHLA(L)-SHLAS(L))/(PLA-SENLA)
            ENDIF
        ENDDO
        
        !-----------------------------------------------------------------------
        !         Update produced leaf area
        !-----------------------------------------------------------------------
        
        IF (TT*EMRGFR.GT.0.0) THEN
            PLA = PLA + PLAGSB4
            PLAX = AMAX1(PLAX,PLA)
            LAP(LNUMSG) = LAP(LNUMSG) + PLAGSB4
            
            DO L = 1,INT(SHNUM+1)
                IF (SHNUM.GE.1.0.OR.SHNUM-FLOAT(L-1).GT.0.0) THEN
                    SHLA(L) = SHLA(L) + SHLAGB4(L)
                ENDIF
            ENDDO
            
            IF (LCNUM.LT.LCNUMX) THEN
                IF (PLAGSB4.GT.0.0) THEN
                    LCNUM = LCNUM+1
                    LCOA(LCNUM) = PLAGSB4
                ENDIF
            ELSE
                LCOA(LCNUM) = LCOA(LCNUM) + PLAGSB4
            ENDIF
            
        ENDIF
        
        !-----------------------------------------------------------------------
        !         Update senesced and harvested leaf area
        !-----------------------------------------------------------------------
        
        SENLA = SENLA + PLAS
        SENLALITTER = SENLALITTER + PLAS * SENFR
        LAPHC = LAPHC + LAPH  ! Grazed leaf area
        ! Distribute senesced leaf over leaf positions and cohorts
        PLASTMP = PLAS - PLASP
        IF (LNUMSG.GT.0 .AND. PLASTMP.GT.0) THEN
            DO L = 1, LNUMSG
                IF (LAP(L)-LAPS(L).GT.PLASTMP) THEN
                    LAPS(L) = LAPS(L) + PLASTMP
                    PLASTMP = 0.0
                ELSE
                    PLASTMP = PLASTMP - (LAP(L)-LAPS(L))
                    LAPS(L) = LAP(L)
                ENDIF
                IF (PLASTMP.LE.0.0) EXIT
            ENDDO
            ! Cohorts
            PLASTMP2 = AMAX1(0.0,PLAS)
            DO L = 1, LCNUM
                IF (LCOA(L)-LCOAS(L).GT.PLASTMP2) THEN
                    LCOAS(L) = LCOAS(L) + PLASTMP2
                    PLASTMP2 = 0.0
                ELSE
                    PLASTMP2 = PLASTMP2 - (LCOA(L)-LCOAS(L))
                    LCOAS(L) = LCOA(L)
                ENDIF
                IF (PLASTMP2.LE.0.0) EXIT
            ENDDO
        ENDIF
        ! Distribute harvested leaf over leaf positions and cohorts
        ! Leaf positions
        IF (LNUMSG.GT.0 .AND. LAPH.GT.0) THEN
            DO L = 1, LNUMSG
                IF (LAP(L)-LAPS(L).GT.0.0) LAPS(L) = LAPS(L) + (LAP(L)-LAPS(L)) * HAFR
            ENDDO
            ! Cohorts
            DO L = 1, LCNUM
                IF (LCOA(L)-LCOAS(L).GT.0.0) THEN
                    LCOAS(L) = LCOAS(L) + (LCOA(L)-LCOAS(L)) * HAFR
                ENDIF
            ENDDO
        ENDIF
        
        !-----------------------------------------------------------------------
        !         Update (green) leaf area                            
        !-----------------------------------------------------------------------            
        LAPD = AMAX1(0.0,(PLA-SENLA-LAPHC))
        LAI = AMAX1(0.0,(PLA-SENLA-LAPHC)*PLTPOP*0.0001)
        LAIX = AMAX1(LAIX,LAI)
        
        !-----------------------------------------------------------------------
        !         Update specific leaf area
        !-----------------------------------------------------------------------
        
        SLA = -99.0
        IF (LFWT.GT.1.0E-6) SLA=(PLA-SENLA-LAPHC) / (LFWT*(1.0-LPEFR))
        
        !-----------------------------------------------------------------------
        !         Update leaf petiole and stem area
        !-----------------------------------------------------------------------
        
        LPEAI = (LFWT*LPEFR*LPEAW)*PLTPOP*0.0001
        STAI = STAI + STAIG - STAIS
        
        SAID = STAI+LPEAI
        CAID = LAI + SAID
        
        !-----------------------------------------------------------------------
        !         Update height
        !-----------------------------------------------------------------------
        
        CANHT = CANHT + CANHTG
        
        !-----------------------------------------------------------------------
        !         Update root depth and length
        !-----------------------------------------------------------------------
        
        IF (SDEPTH.GT.0.0 .AND.RTDEP.LE.0.0) RTDEP = SDEPTH
        RTDEP = AMIN1 (RTDEP+RTDEPG,DEPMAX)
        DO L = 1, NLAYR
            RLV(L)=RTWTL(L)*RLWR*PLTPOP/DLAYR(L)   ! cm/cm3
            IF (L.EQ.NLAYR.AND.RLV(L).GT.0.0)THEN
                IF (RTSLXDATE.LE.0.0) RTSLXDATE = YEARDOY
            ENDIF
        END DO
        
    END SUBROUTINE CS_Integ_LA
        
