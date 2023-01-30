!***************************************************************************************************************************
! This is the code from the section (DYNAMIC == INTEGR) lines 6901 - 7148 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! SUBROUTINE YCA_Out_PlGrow covers outputs of plant growth factors (Plantgro, gr2, grf, N) (IDETG  /=  N). 
!***************************************************************************************************************************
    
    SUBROUTINE YCA_Out_PlGrow ( & 
        BRSTAGE     , CANHT       , DOY         ,  EOP         , IDETG       , IDETL       , ISWNIT      , &
        NFP         , RLV         , RUN         , TN          , YEAR        &
        )
        
        USE ModuleDefs
        USE YCA_First_Trans_m
        USE YCA_Formats_m
        USE YCA_Control_Plant
     
        IMPLICIT NONE 
        EXTERNAL HEADER, CALENDAR, CSOPLINE
     
        INTEGER :: DOY         , RUN         , TN          , YEAR

        REAL    :: BRSTAGE     , CANHT       , EOP         , NFP         , RLV(NL)           

        CHARACTER(LEN=1)  :: IDETG       , IDETL       , ISWNIT       
        
        !---------------------------------------------------------------------------------------------------------------
        !         Plant growth factors: Plantgro, gr2, grf, N (Outputs for IDETG NE N )
        !---------------------------------------------------------------------------------------------------------------     
            
        IF ((IDETG /= 'N'.AND.IDETL /= '0').OR.IDETL == 'A') THEN
                
            ! PlantGro
            IF (YEARDOY == PLYEARDOY) THEN
                OPEN (UNIT = NOUTPG, FILE = OUTPG,POSITION = 'APPEND')
                IF (FILEIOT(1:2) == 'DS') THEN
                    CALL HEADER(2, NOUTPG, RUN)
                ELSE
                    WRITE (NOUTPG,'(/,A79,/)') OUTHED
                    WRITE (NOUTPG,'(A,A8)') ' MODEL            ', MODEL
                    WRITE (NOUTPG,'(A,A8)') ' MODULE           ',MODNAME
                    WRITE (NOUTPG,'(A,A8,A1,A2,A2,A47)')' EXPERIMENT       ',EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
                    WRITE (NOUTPG,'(A,I3,A,A25)') ' TREATMENT', TN,'     ',TNAME
                    WRITE (NOUTPG,'(A,A2,A6,A,A16)') ' GENOTYPE         ',CROP,VARNO,'  ',VRNAME
                    CALL Calendar (year,doy,dom,month)
                    WRITE(NOUTPG,'(A,A3,I3,I8,2X,I4,A,I3,A,/)')' PLANTING         ', month, dom, plyeardoy,  &
                        NINT(ppop), ' plants/m2 in ', NINT(rowspc), ' cm rows' !LPM 06MAR2016 To have just one name for PPOP
                ENDIF
                WRITE (NOUTPG, FMT2201)
            ENDIF  ! End Plantgro header writes
            !L = INT(BRSTAGE)
            WRITE (NOUTPG, FMT501)YEAR,DOY,DAS,DAP,TMEAN,BRSTAGEC,LNUM,PARIOUT,PARIUE,AMIN1(999.9,CARBOBEG*plantPopulation()), &
                LAIC,SAID,CAIC,NINT(TWAD),NINT(SDWADOUT),NINT(RWAD),NINT(CWAD),NINT(LLWADOUT),NINT(STWADOUT),NINT(HWAD), &
                HIAD, NINT(RSWAD),SENTOPLITTERAC,SENROOTC,RSCD*100.0,BRNUMSHM, & !issue 50
                !SLAOUT, RTDEP/100.0,PTF,H2OA,AMIN1(99.9,WAVR),AMIN1(15.0,WUPR),1.0-WFP,1.0-WFG,1.0-NFP,1.0-NFG, & !LPM 19MAY2015 to delete PTF
                SLAOUT, RTDEP/100.0,H2OA,AMIN1(99.9,WAVR),AMIN1(15.0,WUPR),1.0-WFP,1.0-WFG,1.0-NFP,1.0-NFG, &
                AMIN1(2.0,NUPRATIO),1.0-TFP,1.0-TFG,1.0-DFOUT
            ! End Plantgro writes
              !WRITE (*, FMT501)YEAR,DOY,DAS,DAP,TMEAN,BRSTAGEC,LNUM,PARIOUT,PARIUE,AMIN1(999.9,CARBOBEG*plantPopulation()), &
              !  LAIC,SAID,CAIC,NINT(TWAD),NINT(SDWAD),NINT(RWAD),NINT(CWAD),NINT(LLWADOUT),NINT(STWADOUT),NINT(HWAD), &
              !  HIAD, NINT(CRWADOUT),NINT(RSWAD),SENTOPLITTERAC,SENROOTC,RSCD*100.0,NINT(HNUMAD),HWUDC,NINT(BRNUMST(L)), &
              !  SLAOUT, RTDEP/100.0,PTF,H2OA,AMIN1(99.9,WAVR),AMIN1(15.0,WUPR),1.0-WFP,1.0-WFG,1.0-NFP,1.0-NFG, &
              !  AMIN1(2.0,NUPRATIO),1.0-TFP,1.0-TFG,1.0-DFOUT        
            ! PlantGroReductionFactors
            IF (YEARDOY > PLYEARDOY) THEN
                TCDIF = TCAN - TMEAN
            ELSE  
                TCDIF = -99
            ENDIF
            IF (YEARDOY == PLYEARDOY) THEN
                OPEN (UNIT = NOUTPGF, FILE = OUTPGF,POSITION = 'APPEND')
                IF (FILEIOT(1:2) == 'DS') THEN
                    CALL HEADER(2, NOUTPGF, RUN)
                ELSE
                    WRITE (NOUTPGF,'(/,A79,/)') OUTHED
                    WRITE (NOUTPGF,'(A,A8)') ' MODEL            ', MODEL
                    WRITE (NOUTPGF,'(A,A8)') ' MODULE           ',MODNAME
                    WRITE (NOUTPGF,'(A,A8,A1,A2,A2,A47)')' EXPERIMENT       ',EXCODE(1:8),' ',EXCODE(9:10),'  ', &
                        ENAME(1:47)
                    WRITE (NOUTPGF,'(A,I3,A,A25)') ' TREATMENT', TN,'     ',TNAME
                    WRITE (NOUTPGF,'(A,A2,A6,A,A16)') ' GENOTYPE         ',CROP,VARNO,'  ',VRNAME
                    CALL Calendar (year,doy,dom,month)
                    WRITE(NOUTPGF,'(A,A3,I3,I8,2X,I4,A,I3,A,/)')' PLANTING         ', month, dom, plyeardoy,  &
                        NINT(ppop), ' plants/m2 in ', NINT(rowspc), ' cm rows' !LPM 06MAR2016 To have just one name for PPOP
                ENDIF
                WRITE (NOUTPGF, FMT2215)
                WRITE (NOUTPGF, FMT2205)
            ENDIF  ! End Plantgro header writes
            WRITE (NOUTPGF, FMT507)YEAR,DOY,DAS,DAP,TMEAN,TCDIF,BRSTAGEC,DU,1.0-DFOUT,1.0-TFP,1.0-WFP,1.0-NFP, &
                1.0-CO2FP,1.0-RSFP,1.0-TFG,1.0-WFG,1.0-NFG,AMIN1(99.9,WAVR),AMIN1(15.0,WUPR),H2OA,EOP, &
                !SNO3PROFILE+SNH4PROFILE,LNCR,SNCR,RNCR  !LPM 25MAY2015 SNCR to SNCRM
                SNO3PROFILE+SNH4PROFILE,LNCRM,SNCRM,RNCR
            ! End Plantgro reduction factor writes
                         
            ! PlantGr2
            IF (YEARDOY == PLYEARDOY) THEN
                OPEN (UNIT = NOUTPG2, FILE = OUTPG2, STATUS='UNKNOWN',POSITION = 'APPEND')
                IF (FILEIOT(1:2) == 'DS') THEN
                    CALL HEADER(2, NOUTPG2, RUN)
                ELSE
                    WRITE (NOUTPG2,'(/,A79,/)') OUTHED
                    WRITE (NOUTPG2,'(A,A8)') ' MODEL            ', MODEL
                    WRITE (NOUTPG2,'(A,A8)') ' MODULE           ',MODNAME
                    WRITE (NOUTPG2,'(A,A8,A1,A2,A2,A47)')' EXPERIMENT       ',EXCODE(1:8),' ',EXCODE(9:10),'  ', &
                        ENAME(1:47)
                    WRITE (NOUTPG2,'(A,I3,A,A25)') ' TREATMENT', TN,'     ',TNAME
                    WRITE (NOUTPG2,'(A,A2,A6,A,A16)') ' GENOTYPE         ',CROP,VARNO,'  ',VRNAME
                    WRITE(NOUTPG2,'(A,A3,I3,I8,2X,I4,A,I3,A,/)')' PLANTING         ', month, dom, plyeardoy,  &
                        NINT(ppop), ' plants/m2 in ', NINT(rowspc), ' cm rows' !LPM 06MAR2016 To have just one name for PPOP
                ENDIF 
                WRITE (NOUTPG2, FMT2251)
            ENDIF   ! Plantgr2 header writes
            LAIPROD = PLA*PLTPOP*0.0001
            CALL Csopline(laiprodc,laiprod)
            CALL Csopline(canhtc,canht)
            L = MAX(1,LNUMSG-INT(LLIFG))
            WRITE (NOUTPG2, FMT502)YEAR,DOY,DAS,DAP,TMEAN,BRSTAGEC,BRSTAGE,LAIPRODC,SENLA*PLTPOP*0.0001,LAIC, &
                !CANHTC,SDWAD,SENTOPLITTERAC,SENROOTC,NINT(HNUMAD),HWUDC,SHRTD,PTF,RTDEP/100.0,(RLV(I),I=1,10) !LPM 19MAY2015 to delete PTF as output
                  CANHTC,SDWT,SENTOPLITTERAC,SENROOTC,SHRTD,RTDEP/100.0,(RLV(I),I=1,10)    ! issue 50
            ! End PlantGr2 writes
                             
                ! PlantN
            IF (ISWNIT /= 'N') THEN
                IF (YEARDOY == PLYEARDOY) THEN
                    OPEN (UNIT = NOUTPN, FILE = OUTPN, STATUS='UNKNOWN',POSITION = 'APPEND')
                    IF (FILEIOT(1:2) == 'DS') THEN
                        CALL HEADER(2, NOUTPN, RUN)
                    ELSE
                        WRITE (NOUTPN,'(/,A79,/)') OUTHED
                        WRITE (NOUTPN,'(A,A8)') ' MODEL            ', MODEL
                        WRITE (NOUTPN,'(A,A8)') ' MODULE           ',MODNAME
                        WRITE (NOUTPN,'(A,A8,A1,A2,A2,A47)')' EXPERIMENT       ',EXCODE(1:8),' ',EXCODE(9:10), &
                            '  ', ENAME(1:47)
                        WRITE (NOUTPN,'(A,I3,A,A25)') ' TREATMENT', TN,'     ',TNAME
                        WRITE (NOUTPN,'(A,A2,A6,A,A16)') ' GENOTYPE         ',CROP,VARNO,'  ',VRNAME
                        WRITE (NOUTPN,'(A,A3,I3,I8,2X,I4,A,I3,A,/)')' PLANTING         ', month, dom,  &
                            plyeardoy, NINT(ppop), ' plants/m2 in ', NINT(rowspc), ' cm rows' !LPM 06MAR2016 To have just one name for PPOP
                    ENDIF 
                    WRITE (NOUTPN, FMT2252)
                ENDIF  ! Plantn header writes
                CALL Csopline(senn0c,sennal(0))
                CALL Csopline(sennsc,sennas)
                WRITE (NOUTPN, FMT503)YEAR,DOY,DAS,DAP,TMEAN,BRSTAGEC,NUPAC,TNAD,SDNAD,RNAD,CNAD,LLNAD,SNAD,HNAD, &
                    HINDC,RSNAD,SENN0C,SENNSC,RANC*100.0,LANCM*100.0,SANCOUT*100.0,AMIN1(9.9,HNC*100.0), &
                    !SDNC*100.0,AMIN1(9.9,VANC*100.0),LNCR,SNCR,RNCR,VCNC*100.0,VMNC*100.0,AMIN1(2.,NUPRATIO), &        !LPM 25MAY2015 SNCR to SNCRM
                    SDNC*100.0,AMIN1(9.9,VANC*100.0),LNCRM,SNCRM,RNCR,VCNC*100.0,VMNC*100.0,AMIN1(2.,NUPRATIO), &
                    ANDEM,1.0-node(0,0)%NFLF2                                                                                !LPM 21MAR15 NFLF2(0) as NFLF2(0,0)
                                     
            ENDIF  ! ISWNIT  Plantn writes
                             
        ELSE ! (IDETG /= 'N'.AND.IDETL /= '0').OR.IDETL == 'A'
                
            IF (IDETGNUM <= 0) THEN
                OPEN (UNIT=FNUMTMP, FILE=OUTPG, STATUS = 'UNKNOWN', IOSTAT = IOCHECK)
                CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
                OPEN (UNIT=FNUMTMP, FILE=OUTPG2, STATUS = 'UNKNOWN')
                CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
                OPEN (UNIT=FNUMTMP, FILE=OUTPG3, STATUS = 'UNKNOWN')
                CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
                OPEN (UNIT=FNUMTMP, FILE=OUTPGF, STATUS = 'UNKNOWN')
                CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
                OPEN (UNIT=FNUMTMP, FILE=OUTPN, STATUS = 'UNKNOWN')
                CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
                IDETGNUM = IDETGNUM + 1 
            ENDIF  
                
        ENDIF ! End ((IDETG /= 'N'.AND.IDETL /= '0').OR.IDETL == 'A' 
    
    END SUBROUTINE YCA_Out_PlGrow
            
            


