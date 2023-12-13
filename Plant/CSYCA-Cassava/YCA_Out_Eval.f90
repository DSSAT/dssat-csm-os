!***************************************************************************************************************************
! This is the code from the section (DYNAMIC == INTEGR) lines 7158 - 8311 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine YCA_Out_Eval outputs evaluate and overview data and necessary input data for checking.
!***************************************************************************************************************************
    
    SUBROUTINE YCA_Out_Eval ( & 
        CN          , DOY         , DYNAMIC     , IDETG       , IDETL       , IDETO       , ISWNIT      , ISWWAT      , &
        MESOM       , ON          , RN          , RUN         , RUNI        , SN          , STGYEARDOY  , TN          , &
        TNIMBSOM    , TOMINSOM1   , YEAR        , LAI         &
        )
        
        USE ModuleDefs                                                                        ! MF 31AU14 ADDED FOR ACCESS TO WEATHER
        USE YCA_First_Trans_m
        USE YCA_Formats_m
     
        IMPLICIT NONE 
        EXTERNAL YR_DOY, WARNING, HEADER, TVILENT, TVICOLNM, TL10FROMI, LTRIM, CSTIMDIF, CSOPLINE, CALENDAR, DAPCALC, &
          LTRIM2, AREADR, AREADI, CSYDOY, GETSTRI, GETSTRR
     
        INTEGER :: CN          , DOY         , DYNAMIC     , ON          , RN          , RUN         , RUNI        , SN             
        INTEGER :: STGYEARDOY(0:19)            , TN          , YEAR
        INTEGER :: CSTIMDIF    , CSYDOY      , DAPCALC     , TVICOLNM    , TVILENT            ! Integer function calls

        REAL    :: TNIMBSOM    , TOMINSOM1   , LAI       
        REAL    :: CNCTMP                  ! Canopy N concentration,tempry  %          ! (From Output)    
        REAL    :: HIAMMTMP                ! Harvest index,mature,temporary #          ! (From Output)    

        CHARACTER(LEN=1)  :: IDETG       , IDETL       , IDETO       , ISWNIT      , ISWWAT      , MESOM       
        CHARACTER(LEN=10) :: TL10FROMI                                                        ! Character function call
        
        !INTEGER,PARAMETER::SEASEND= 6 ! Program ending indicator                             ! MF In ModuleDefs 
        
        !-----------------------------------------------------------------------------------------------------------
        !         Eval (IDETO) OUTPUTS AND NECESSARY DATA INPUTS (Evaluate & Overview)
        !-----------------------------------------------------------------------------------------------------------
          
        IF (IDETO /= 'N'.OR.IDETL == '0') THEN
                tiernum = 0
                cnamm = -99
                cnpchm = -99
                cwamm = -99
                edatm = -99
                edapm = -99
                gdapm = -99
                hnamm = -99
                hnpcmm = -99
                hiamm = -99
                hinmm = -99 
                hnumamm = -99
                srnoamm = -99
                srnogmm = -99
                srnogmm = -99
                srnpcm = -99
                hwahm = -99
                hwamm = -99
                hyamm = -99
                hwumm = -99
                srwumm = -99
                laixm = -99
                lnumsmm = -99
                mdapm = -99
                mdatm = -99
                nupacm = -99
                rnamm = -99
                rswamm = -99
                rscmm = -99
                rwamm = -99
                sennatcm = -99
                senwacmm = -99
                shrtmm = -99
                psdatm = -99
                vnamm = -99
                vnpcmm = -99
                vwamm = -99
                laixt = -99.0
                valuer = -99.0
                ! Variables from time-course file
                LAIXT = -99.0 
                LNUMT = -99.0 
                CWADT = -99.0
                FHWADT = -99.0
                HWADT = -99.0
                HIADT = -99.0 
                HWUT = -99.0 
                HNUMAT = -99.0 
                HNUMET = -99.0 
                MDATT = -99 
                    
                ! Reading A-file
                CALL LTRIM2 (FILEIO,filenew)
                FILELEN = TVILENT(FILENEW)
                FILELEN = MAX(FILELEN-12, 0) 
                    
                IF (TVILENT(FILEADIR) > 3) THEN
                    IF (FILEADIR(TVILENT(FILEADIR):TVILENT(FILEADIR)) /= SLASH) THEN                       
                    FILEA = FILEADIR(1:TVILENT(FILEADIR))//SLASH//EXCODE(1:8)//'.'//EXCODE(9:10)//'A'      
                    ELSE
                        FILEA = FILEADIR(1:TVILENT(FILEADIR))//EXCODE(1:8)//'.'//EXCODE(9:10)//'A'
                    ENDIF
                ELSE
                    FILEA = FILENEW(1:FILELEN-12)//EXCODE(1:8)//'.'//EXCODE(9:10)//'A'
                ENDIF       
                FEXISTA = .FALSE.
                INQUIRE (FILE = FILEA,EXIST = FEXISTA)
                IF (.not.FEXISTA) THEN
                    WRITE (Message(1),'(A23,A50)')'Could not find A-file: ',filea(1:50)
                    WRITE (Message(2),'(A23,A50)')'Experiment file:       ',fileio(1:50)
                    CALL WARNING(2,'CSYCA',MESSAGE)
                    OPEN (UNIT=FNUMTMP, FILE=FILEA, STATUS = 'UNKNOWN')
                    CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
                ELSE
                    ! Yield at harvest   
                    CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HWAM',hwamm)
                    IF (hwamm > 0.0.AND.HWAMM < 50.0) HWAMM = HWAMM*1000.0
                    CALL AREADR (FILEA,TN,RN,SN,ON,CN,'FHWAM',fhwamm)
                    IF (fhwamm > 0.0.AND.FHWAMM < 50.0) FHWAMM = FHWAMM*1000.0
                    IF (HWAMM <= 0.0) THEN
                        CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HYAM',hyamm)
                        IF (hyamm <= 0.0) THEN
                            CALL AREADR (FILEA,TN,RN,SN,ON,CN,'GYAM',hyamm)
                        ENDIF  
                        IF (hyamm > 0.0.AND.HYAMM < 50.0) HYAMM = HYAMM*1000.0
                    ENDIF
                    !IF (HWAMM <= 0.0.AND.HYAMM > 0..AND.HMPC > 0.0) HWAMM = HYAMM * (1.0-HMPC/100.0)
                    CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HWUM',hwumm)
                    CALL AREADR (FILEA,TN,RN,SN,ON,CN,'LAIX',laixm)
                    CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CWAM',cwamm)
                    CALL AREADR (FILEA,TN,RN,SN,ON,CN,'BWAH',vwamm)
                    CALL AREADR (FILEA,TN,RN,SN,ON,CN,'H#AM',hnumamm)
                    IF (hnumamm.le.0.0)CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HNOAM',hnumamm)
                    CALL AREADR (FILEA,TN,RN,SN,ON,CN,'H#SM',hnumgmm)
                    IF (hnumgmm.le.0.0)CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HNOSM',hnumgmm)
                    IF (hnumgmm.le.0.0)CALL AREADR (FILEA,TN,RN,SN,ON,CN,'H#UM',hnumgmm)
                    IF (hnumgmm.le.0.0)CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HNOUM',hnumgmm)
                    CALL AREADR (FILEA,TN,RN,SN,ON,CN,'L#SM',lnumsmm)
                    IF (lnumsmm.le.0.0)CALL AREADR (FILEA,TN,RN,SN,ON,CN,'LNOSM',lnumsmm)
                    CALL AREADR (FILEA,TN,RN,SN,ON,CN,'BR#SH',brnumshmm)
                        
                    CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CNAM',cnamm)
                    CALL AREADR (FILEA,TN,RN,SN,ON,CN,'VNAM',vnamm)
                    CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HNAM',hnamm)
                    CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CN%M',cnpchm)
                    IF (cnpchm.le.0.0)CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CNPCM',cnpchm)
                    CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HN%M',hnpcmm)
                    IF (hnpcmm.le.0.0)CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HNPCM',hnpcmm)
                    IF (HNPCMM <= 0.0) HNPCMM = -99   
                    CALL AREADR (FILEA,TN,RN,SN,ON,CN,'VN%M',vnpcmm)
                    IF (vnpcmm.le.0.0)CALL AREADR (FILEA,TN,RN,SN,ON,CN,'VNPCM',vnpcmm)
                        
                    CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HIAM',hiamm)
                    IF (HIAMM >= 1.0) HIAMM = HIAMM/100.0
                        
                    CALL AREADI (FILEA,TN,RN,SN,ON,CN,'EDAT',edatm)
                    CALL AREADI (FILEA,TN,RN,SN,ON,CN,'GDAT',gdatm)
                        
                    DO L = 1,PSNUM              
                        CALL AREADI (FILEA,TN,RN,SN,ON,CN,psabv(l),psdatm(l))
                        CALL LTRIM(PSABV(L)) 
                        IF (PSABV(L) == 'TSAT')CALL AREADI (FILEA,TN,RN,SN,ON,CN,'TSDAT',psdatm(l))
                        IF (PSDATM(L) > 0.0.AND.PSDATM(L) < 1000) THEN
                            CALL AREADI (FILEA,TN,RN,SN,ON,CN,'YEAR',yearm)
                            IF (YEARM > 0.0) PSDATM = CSYDOY(YEARM,PSDATM(L))
                        ENDIF
                        IF (psdatm(l).gt.0) then
                            psdapm(l) = Dapcalc(psdatm(l),plyear,plday)
                        ELSE
                            psdapm(l) = -99
                        ENDIF 
                    ENDDO
                ENDIF ! File-A exists
                    
                ! Reading T-file to complement A-data and writing MEASURED
                IF (IDETG /= 'N'.OR.IDETL == 'A') THEN 
                    STARNUMO = STARNUMO + 1 ! Number of datasets in Simop file
                    CALL LTRIM2 (FILEIO,filenew)
                    FILELEN = TVILENT(FILENEW)
                    FILET=FILENEW(1:FILELEN-12)//EXCODE(1:8)//'.'//EXCODE(9:10)//'T'
                    FEXISTT  = .FALSE.
                    INQUIRE (FILE = FILET,EXIST = FEXISTT)
                    IF (.not.FEXISTT) THEN
                        WRITE (Message(1),'(A23,A50)')'Could not find T-file: ',filet(1:50)
                        CALL WARNING(1,'CSYCA',MESSAGE)
                    ELSE
                        TLINENUM = 0
                        OPEN (UNIT=FNUMT,FILE=FILET)
                        OPEN (UNIT=FNUMMEAS,FILE=FNAMEMEAS,POSITION='APPEND')
                        COLNUM = 1
                        L1 = 0
                        DO
                            READ(FNUMT,'(A180)',END = 5555)LINET
                            TLINENUM = TLINENUM + 1  ! Used to check if file empty
                            L1 = 0
                            L2 = 0
                            ! First IF to jump over comments and blanks
                            IF (LEN(LINET) > 0.AND.LINET(1:1) /= '!') THEN
                                IF (LINET(1:7) == '*DATA(T' .OR.LINET(1:7) == '*EXP.DA' .OR.LINET(1:7) == '*EXP. D' .OR. &
                                    LINET(1:7) == '*TIME_C' .OR.LINET(1:7) == '$EXPERI') THEN
                                    TNCHAR = TL10FROMI(TN)
                                    LENLINE = TVILENT(LINET)
                                    IF(LINET(1:7) == '*EXP.DA'.OR.LINET(1:7) == '*EXP. D'.OR.LINET(1:7) == '$EXPERI')THEN
                                        GROUP = 'A'
                                        DO L = 1,30
                                            IF (LINET(L:L+1) == ': ') L1 = L+2
                                            IF (LINET(L:L) == ':'.AND.LINET(L+1:L+1) /= ' ')L1 = L+1   
                                            IF (L1 > 0.AND.L > L1+9.AND.LINET(L:L) /= ' ') THEN
                                                L2 = L ! Start of group information in tfile
                                                EXIT
                                            ENDIF
                                        ENDDO
                                        LENTNAME = MIN(15,TVILENT(TNAME))
                                        LENGROUP = MIN(L2+14,LENLINE)
                                        IF (TVILENT(TNCHAR) == 1) THEN
                                            LINESTAR = LINET(L1:L1+9)//' '//TNCHAR(1:1)//' '//TNAME(1:LENTNAME)
                                        ELSEIF (TVILENT(TNCHAR) == 2) THEN
                                            LINESTAR = LINET(L1:L1+9)//' '//TNCHAR(1:2)//' '//TNAME(1:LENTNAME)
                                        ELSEIF (TVILENT(TNCHAR) == 3) THEN
                                            LINESTAR = LINET(L1:L1+9)//' '//TNCHAR(1:3)//' '//TNAME(1:LENTNAME)
                                        ENDIF
                                        LENLINESTAR = TVILENT(LINESTAR)
                                    ENDIF
                                    ELSEIF (LINET(1:1) == '@') THEN
                                        DO L = 1,TVILENT(LINET)
                                            IF (LINET(L:L+2) == ' GW') LINET(L:L+2) = ' HW'
                                        END DO
                                        DATECOL = Tvicolnm(linet,'DATE')
                                        YEARCOL = Tvicolnm(linet,'YEAR')
                                        DOYCOL = Tvicolnm(linet,'DOY')
                                        IF (DOYCOL <= 0) DOYCOL = Tvicolnm(linet,'DAY')
                                        RPCOL = Tvicolnm(linet,'RP')
                                        LAIDCOL = Tvicolnm(linet,'LAID')
                                        LNUMCOL = Tvicolnm(linet,'L#SD')
                                        CWADCOL = Tvicolnm(linet,'CWAD')
                                        HWADCOL = Tvicolnm(linet,'HWAD')
                                        FHWADCOL = Tvicolnm(linet,'FHWAD')
                                        HIADCOL = Tvicolnm(linet,'HIAD')
                                        HWTUCOL = Tvicolnm(linet,'HWUD')
                                        HNUMACOL = Tvicolnm(linet,'H#AD')
                                        HNUMECOL = Tvicolnm(linet,'H#ED')
                                        GSTDCOL = Tvicolnm(linet,'GSTD')
                                        LENLINE = TVILENT(LINET)
                                        LINET(LENLINE+1:LENLINE+12) = '   DAP   DAS'
                                        LINET(1:1) = '@'
                                        TIERNUM = TIERNUM + 1
                                        IF (TIERNUM < 10) THEN
                                            WRITE(TIERNUMC,'(I1)') TIERNUM
                                        ELSE
                                            WRITE(TIERNUMC,'(I2)') TIERNUM
                                        ENDIF
                                        LINESTAR2 = '*TIER('//TIERNUMC//'):'//LINESTAR(1:LENLINESTAR)//LINET(14:LENLINE)
                                        IF (IDETG /= 'N') THEN 
                                            WRITE (FNUMMEAS,*) ' '
                                            WRITE (FNUMMEAS,'(A80)') LINESTAR2(1:80)
                                            WRITE (FNUMMEAS,*) ' '
                                            WRITE (FNUMMEAS,'(A180)') LINET(1:180)
                                        ENDIF  
                                        STARNUMM = STARNUMM + 1              ! # datasets
                                    ELSE
                                        CALL Getstri (LINET,COLNUM,VALUEI)
                                        IF (VALUEI == TN) THEN
                                            IF (DATECOL > 0.OR.DOYCOL > 0) THEN
                                                IF (DATECOL > 0) THEN
                                                    CALL Getstri (LINET,DATECOL,DATE)
                                                ELSEIF (DATECOL <= 0) THEN
                                                    CALL Getstri (LINET,DOYCOL,DOY)
                                                    CALL Getstri (LINET,YEARCOL,YEAR)
                                                    IF (YEAR > 2000) YEAR = YEAR-2000
                                                    IF (YEAR > 1900) YEAR = YEAR-1900
                                                    DATE = YEAR*1000+DOY
                                                ENDIF
                                                DAP = MAX(0,CSTIMDIF(PLYEARDOY,DATE))
                                                DAS = MAX(0,CSTIMDIF(YEARSIM,DATE))
                                                DAPCHAR = TL10FROMI(DAP)
                                                IF (TVILENT(DAPCHAR) == 1) THEN
                                                    DAPWRITE = '     '//DAPCHAR(1:1)
                                                ELSEIF (TVILENT(DAPCHAR) == 2) THEN
                                                    DAPWRITE = '    '//DAPCHAR(1:2)
                                                ELSEIF (TVILENT(DAPCHAR) == 3) THEN
                                                    DAPWRITE = '   '//DAPCHAR(1:3)
                                                ENDIF
                                                LENLINE = TVILENT(LINET)
                                                LINET(LENLINE+1:LENLINE+6) = DAPWRITE(1:6)
                                                DAPCHAR = TL10FROMI(DAS)
                                                IF (TVILENT(DAPCHAR) == 1) THEN
                                                    DAPWRITE = '     '//DAPCHAR(1:1)
                                                ELSEIF (TVILENT(DAPCHAR) == 2) THEN
                                                    DAPWRITE = '    '//DAPCHAR(1:2)
                                                ELSEIF (TVILENT(DAPCHAR) == 3) THEN
                                                    DAPWRITE = '   '//DAPCHAR(1:3)
                                                ENDIF
                                                LENLINE = TVILENT(LINET)
                                                LINET(LENLINE+1:LENLINE+6) = DAPWRITE(1:6)
                                            ENDIF
                                            CALL Getstri (LINET,RPCOL,VALUEI)
                                            IF (IDETG /= 'N') THEN 
                                                IF (VALUEI <= 0)WRITE (FNUMMEAS,'(A180)') LINET
                                            ENDIF  
                                                
                                            ! T-FILE STUFF FOR OUTPUT OF INDIVIDUAL VARS
                                            ! Below is to pick up variables for output files
                                            IF (IDETL == 'A') THEN
                                                IF (GROUP == 'A') THEN
                                                    CALL Getstrr (LINET,LAIDCOL,VALUER)
                                                    IF (VALUER > LAIXT) LAIXT = VALUER
                                                    CALL Getstrr (LINET,LNUMCOL,VALUER)
                                                    IF (VALUER > LNUMT) LNUMT = VALUER
                                                    CALL Getstrr (LINET,CWADCOL,VALUER)
                                                    IF (VALUER > 0.0) CWADT = VALUER
                                                    CALL Getstrr (LINET,HWADCOL,VALUER)
                                                    IF (VALUER > 0.0) HWADT = VALUER
                                                    CALL Getstrr (LINET,FHWADCOL,VALUER)
                                                    IF (VALUER > 0.0) FHWADT = VALUER
                                                    CALL Getstrr (LINET,HIADCOL,VALUER)
                                                    IF (VALUER > 0.0) HIADT = VALUER
                                                    IF (HIADT >= 1.0) HIADT = HIADT/100.0
                                                    CALL Getstrr (LINET,HWTUCOL,VALUER)
                                                    IF (VALUER > 0.0) HWUT = VALUER
                                                    CALL Getstrr (LINET,HNUMACOL,VALUER)
                                                    IF (VALUER > 0.0) HNUMAT = VALUER
                                                    CALL Getstrr (LINET,HNUMECOL,VALUER)
                                                    IF (VALUER > 0.0) HNUMET = VALUER
                                                    CALL Getstrr (LINET,GSTDCOL,VALUER)
                                                    !IF (VALUER > FLOAT(MSTG*10).AND.MDATT <= 0)MDATT = DATE !LPM 06MAR15 MSTG TO PSX
                                                    IF (VALUER > FLOAT(PSX*10).AND.MDATT <= 0)MDATT = DATE
                                                    ! To indicate that t data present
                                                    tdatanum = 1
                                                ENDIF ! End picking variables from t for a     
                                            ENDIF ! End of details flag 
                                        ENDIF ! End correct treatment 
                                    ENDIF ! End particular data lines
                            ENDIF ! End valid (ie.non comment) line
                        ENDDO
5555                        CONTINUE
                        ! If T-file was empty
                        IF (TLINENUM < 4) THEN
                            tdatanum = 0
                            WRITE (Message(1),'(A23,A50)')'T-file was empty '
                            CALL WARNING(1,'CSYCA',MESSAGE)
                        ENDIF
                        CLOSE(FNUMT)
                        CLOSE(FNUMMEAS)
                    ENDIF ! End t-file reads,measured.out writes
                        
                    IF (IDETL == 'A') THEN
                        ! Use T-data if A-data missing (whem output=all)
                        IF (FEXISTT) THEN
                            IF (HWAMM <= 0.0) THEN
                                IF (HWADT > 0.0) THEN
                                    HWAMM = HWADT
                                ENDIF
                            ENDIF
                            IF (HWFMM <= 0.0) THEN
                                IF (FHWADT > 0.0) THEN
                                    HWFMM = FHWADT
                                ENDIF
                            ENDIF
                            IF (CWAMM <= 0.0) THEN
                                IF (CWADT > 0.0) THEN
                                    CWAMM = CWADT
                                ENDIF
                            ELSE
                                IF (CWADT > 0.0) THEN
                                    IF (ABS(100.0*ABS(CWAMM-CWADT)/CWAMM) > 0.0) THEN
                                        WRITE(Message(1),'(A48,F8.2)')'Pc difference between final,time-course canopy =', &
                                            100.0*ABS(CWAMM-CWADT)/CWAMM
                                        WRITE(Message(2),'(A19,I6)')'Final canopy       ',NINT(CWAMM)
                                        WRITE(Message(3),'(A19,I6)')'Time-course canopy ',NINT(CWADT)
                                        CALL WARNING(3,'CSYCA',MESSAGE)
                                    ENDIF
                                ENDIF
                            ENDIF
                            IF (LAIXM <= 0.0.AND.LAIXT > 0.0) THEN
                                LAIXM = LAIXT
                                WRITE(Message(1),'(A31)')'Time-course data used for LAIXM'
                                CALL WARNING(1,'CSYCA',MESSAGE)
                            ENDIF
                            IF (LNUMSMM <= 0.0.AND.LNUMSMM > 0.0) THEN
                                LNUMSMM = LNUMT
                                WRITE(Message(1),'(A33)')'Time-course data used for LNUMSMM'
                                CALL WARNING(1,'CSYCA',MESSAGE)
                            ENDIF
                            IF (HIAMM <= 0.0.AND.HIADT > 0.0) THEN
                                HIAMM = HIADT
                                WRITE(Message(1),'(A31)')'Time-course data used for HIAMM'
                                CALL WARNING(1,'CSYCA',MESSAGE)
                            ENDIF
                            IF (HWUMM <= 0.0.AND.HWUT > 0.0) THEN
                                HWUMM = HWUT
                                WRITE(Message(1),'(A31)')'Time-course data used for HWUMM'
                                CALL WARNING(1,'CSYCA',MESSAGE)
                            ENDIF
                            IF (HNUMAMM <= 0.0.AND.HNUMAT > 0.0) THEN
                                HNUMAMM = HNUMAT
                                WRITE(Message(1),'(A31)')'Time-course data used for H#AT'
                                CALL WARNING(1,'CSYCA',MESSAGE)
                            ENDIF
                            IF (HNUMGMM <= 0.0.AND.HNUMET > 0.0) THEN
                                HNUMGMM = HNUMET
                                WRITE(Message(1),'(A32)')'Time-course data used for H#GMM'
                                CALL WARNING(1,'CSYCA',MESSAGE)
                            ENDIF
                        ENDIF
                        DO L = 1,PSNUM
                            IF (PSABV(L) == 'MDAT'.AND.PSDATM(L) <= 0.0) THEN
                                IF (MDATT > 0) THEN
                                    PSDATM(L) = INT(MDATT)
                                    WRITE(Message(1),'(A31)')'Time-course data used for MDATM'
                                    CALL WARNING(1,'CSYCA',MESSAGE)
                                ENDIF  
                            ENDIF
                        ENDDO
                    ENDIF ! END OF USE T-DATA TO FILL IN FOR MISSING A-DATA
                ELSE  ! For IDETG /= 'N'.OR.IDETL == 'A' 
                    ! No call for measured.out! Delete old files.
                    OPEN (UNIT=FNUMTMP,FILE=FNAMEMEAS,STATUS = 'UNKNOWN')
                    CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
                ENDIF ! End A-file reads,T-file reads,Measured writes,T->A
                    
                ! Check data and calculate equivalents,if needed
                    
                ! Emergence and maturity dates 
                IF (edatm <= 0) edatm = edatmx ! If no Afile data,use Xfile
                !IF (mdatm <= 0) mdatm = psdatm(mstg) !LPM 06MAR15 MSTG TO PSX
                IF (mdatm <= 0) mdatm = psdatm(psx)
                    
                ! Product wt at harvesst
                IF (hwahm > 0.AND.hwamm <= 0) hwamm = hwahm/(hpcf/100.0)
                    
                ! Product wt at harvest
                IF (hwamm > 0.AND.hwahm <= 0) hwahm = hwamm*(hpcf/100.0)
                    
                ! Canopy wt at harvest 
                IF (vwamm > 0.AND.hwamm > 0) cwamm = vwamm+hwamm
                    
                ! Vegetative wt at harvest 
                IF (HPROD /= 'SR') THEN
                    IF (hwamm > 0.AND.cwamm > 0) vwamm = cwamm-hwamm
                ELSE
                    IF (cwamm > 0) vwamm = cwamm
                ENDIF
                    
                ! Harvest index at harvest 
                IF (hiamm <= 0.0) THEN
                    IF (cwamm > 0.AND.hwamm > 0) THEN
                        IF (HPROD == 'SR') THEN
                            hiamm = hwamm/(cwamm+hwamm)
                        ELSE 
                            hiamm = hwamm/cwamm
                        ENDIF
                    ENDIF  
                ELSE
                    IF (cwamm > 0.AND.hwamm > 0) THEN
                        hiammtmp = hwamm/cwamm
                    ENDIF
                ENDIF
                    
                ! Product unit wt at harvest 
                IF (hwumm > 1.0) hwumm = hwumm/1000.0 ! mg->g
                IF (hwumm <= 0.AND.hnumamm > 0) THEN
                    IF (hwamm > 0.0) hwumm=hwamm*0.1/hnumamm  ! kg->g
                ELSE
                    IF (hwamm.gt.0.0.AND.hnumamm > 0.0) THEN
                        hwumyld = hwamm*0.1/hnumamm
                    ENDIF
                ENDIF
                    
                ! Product number at harvest 
                IF (HNUMAMM > 0.0) THEN
                    IF (PPOP > 0) THEN !LPM 06MAR2016 To have just one name for PPOP
                        HNUMPMM = HNUMAMM/PPOP
                    ELSE
                        HNUMPMM = -99.0
                    ENDIF  
                ELSE
                    HNUMPMM = -99.0
                ENDIF
                    
                ! Shoot/root ratio at harvest 
                IF (rwamm > 0.0) shrtmm = cwamm/rwamm
                    
                ! Reserves concentration at harvest 
                IF (vwamm+rwamm > 0.AND.rswamm > 0.0) rscmm = rswamm/(vwamm+rwamm)
                    
                ! Canopy N at harvest 
                IF (vnamm > 0.AND.cnamm <= 0) cnamm = vnamm
                    
                ! Total N at harvest 
                IF (CNAMM > 0.0.AND.RNAMM > 0.0) THEN
                    tnamm = cnamm+rnamm
                ELSE
                    tnamm = -99.0
                ENDIF
                    
                ! Vegetative N at harvest  
                IF (vnamm <= 0) THEN
                    IF (hnamm >= 0.AND.cnamm > 0) vnamm=cnamm-hnamm
                ENDIF
                    
                ! Product N harvest index at harvest 
                IF (cnamm > 0.AND.hnamm > 0) hinmm=hnamm/cnamm
                    
                ! Vegetative N concentration at harvest 
                IF (vnpcmm <= 0) THEN
                    IF (vwamm > 0.AND.vnamm > 0) vnpcmm = (vnamm/vwamm)*100
                ENDIF
                    
                ! Product N concentration at harvest 
                IF (hnpcmm <= 0) THEN
                    IF (hwamm > 0.AND.hnamm > 0) hnpcmm = (hnamm/hwamm)*100
                ENDIF
                    
                ! Leaf N concentration at harvest 
                IF (cnpchm <= 0.AND.cnamm > 0.AND.cwamm > 0.0)cnpchm = cnamm/cwamm
                    
                    
                ! Express dates as days after planting
                edapm = -99
                edapm = Dapcalc(edatm,plyear,plday)
                IF (edapm > 200) THEN
                    WRITE (Message(1),'(A31,A31,A11)')'Measured emergence over 200DAP ', &
                        'Maybe reported before planting.','Check files'
                    CALL WARNING(1,'CSYCA',MESSAGE)
                ENDIF
                gdapm = Dapcalc(gdatm,plyear,plday)
                    
                IF (mdapm <= 0) mdapm = Dapcalc(mdatm,plyear,plday)
                    
                ! Check that -99 not multiplied or divided 
                IF (hnumgm < 0.0) hnumgm = -99
                IF (hnumam < 0.0) hnumam = -99
                IF (hnumgmm < 0.0) hnumgmm = -99
                IF (hnumamm < 0.0) hnumamm = -99
                    
                ! Put N variables to -99 if N switched off
                IF (ISWNIT == 'N') THEN
                    hnpcm = -99
                    vnpcm = -99
                    cnam = -99
                    hnam = -99
                    hinm = -99
                    sdnap = -99
                    rnam = -99
                    nupac = -99
                ENDIF  
                    
                ! Create character equivalents for outputing
                CALL Csopline(hwumchar,hwum)
                CALL Csopline(hwummchar,AMAX1(-99.0,hwumm))
                CALL Csopline(hiamchar,AMAX1(-99.0,hiam))
                CALL Csopline(hiammchar,AMAX1(-99.0,hiamm))
                CALL Csopline(hinmchar,AMAX1(-99.0,hinm))
                CALL Csopline(hinmmchar,AMAX1(-99.0,hinmm))
                CALL Csopline(hnpcmchar,AMAX1(-99.0,hnpcm))
                CALL Csopline(hnpcmmchar,AMAX1(-99.0,hnpcmm))
                CALL Csopline(vnpcmchar,AMAX1(-99.0,vnpcm))
                CALL Csopline(vnpcmmchar,AMAX1(-99.0,vnpcmm))
                CALL Csopline(laixchar,AMAX1(-99.0,laix))
                CALL Csopline(laixmchar,AMAX1(-99.0,laixm))
                    
                ! Evaluate
                !LPM 21jul2021 Add more space to harvested weight
                EVHEADER = ' '
                EVHEADER(1:14) = '*EVALUATION : '
                IF (RUN == 1.OR.(EXCODE /= EXCODEPREV.AND.EVALOUT > 1))THEN
                    IF (RUN == 1) THEN
                        EVALOUT = 0
                        EVHEADNM = 0
                        EVHEADNMMAX = 1
                    ENDIF
                    IF (EXCODE /= EXCODEPREV) THEN
                        EVHEADNM = EVHEADNM + 1
                        OPEN (UNIT=FNUMEVAL,FILE=FNAMEEVAL,POSITION='APPEND')
                        IF (EVHEADNM <= EVHEADNMMAX.AND.EVHEADNMMAX >= 1) THEN
                            LENENAME = TVILENT(ENAME)
                            WRITE (FNUMEVAL,*) ' '
                            WRITE (FNUMEVAL, FMT993) EVHEADER,EXCODE,ENAME(1:25),MODNAME
                        ELSE
                            IF (EVHEADNMMAX > 1) THEN
                                WRITE (FNUMEVAL,*) ' '
                                WRITE (FNUMEVAL, FMT995) EVHEADER,MODNAME,'ALL REMAIN','ING EXPERIMENTS        '
                            ELSEIF (EVHEADNM <= EVHEADNMMAX) THEN
                                WRITE (FNUMEVAL,*) ' '
                                WRITE (FNUMEVAL, FMT995) EVHEADER,MODNAME,'ALL EXPERI','MENTS                  '
                            ENDIF 
                        ENDIF
                    ENDIF
                    IF (EVHEADNM <= EVHEADNMMAX) THEN
                        WRITE (FNUMEVAL, FMT994,ADVANCE='NO')
                        !DO L = 1,KEYSTX
                        DO L = 0,KEYSTX
                            IF (KEYPS(L) > 0) THEN
                                IF (PSABVO(KEYPS(L))(1:1) /= ' ') WRITE (FNUMEVAL,'(A1)',ADVANCE='NO') ' '
                                WRITE (FNUMEVAL,'(A5,A1)',ADVANCE='NO') PSABVO(KEYPS(L)),'S'
                                IF (PSABVO(KEYPS(L))(1:1) /= ' ') WRITE (FNUMEVAL,'(A1)',ADVANCE='NO') ' '
                                WRITE (FNUMEVAL,'(A5,A1)',ADVANCE='NO') PSABVO(KEYPS(L)),'M'
                            ENDIF 
                        ENDDO
                        WRITE (FNUMEVAL,'(5A)') &
                            '   HWAMS   HWAMM   FWAMS   FWAMM',' LAIXS LAIXM L#SMS L#SMM BR#SS BR#SM', &
                            ' CWAMS CWAMM VWAMS VWAMM',' HIAMS HIAMM HN%MS HN%MM VN%MS VN%MM', &
                            ' CNAMS CNAMM HNAMS HNAMM HINMS HINMM'
                        CLOSE(FNUMEVAL)
                    ENDIF  
                ENDIF  ! End Evaluate header writes
                IF (EXCODE /= EXCODEPREV) EVALOUT = 0
                EVALOUT = EVALOUT + 1
                OPEN (UNIT = FNUMEVAL,FILE = FNAMEEVAL,POSITION = 'APPEND')
                WRITE (FNUMEVAL,'(I4,1X,A10,I6,I3,1X,A2,2I6)',ADVANCE='NO') RUN,EXCODE,TN,RN,CROP,edap,edapm
                !DO L = 1,KEYSTX
                DO L = 0,KEYSTX
                    IF (KEYPS(L) > 0) THEN 
                        IF (PSABVO(KEYPS(L))(1:1) /= ' ') WRITE (FNUMEVAL,'(A1)',ADVANCE='NO') ' '
                        WRITE (FNUMEVAL,'(I6)',ADVANCE='NO') PSDAP(KEYPS(L))
                        IF (PSABVO(KEYPS(L))(1:1) /= ' ') WRITE (FNUMEVAL,'(A1)',ADVANCE='NO') ' '
                        WRITE (FNUMEVAL,'(I6)',ADVANCE='NO') PSDAPM(KEYPS(L))
                    ENDIF
                ENDDO
                WRITE (FNUMEVAL,'(4I8, 6F6.1, 4I6, 6A6, 4I6, 2A6)')NINT(hwam),NINT(hwamm),NINT(fhwam),NINT(fhwamm), &
                    laix,laixm,lnumsm,lnumsmm, brnam, brnumshmm,NINT(cwam),NINT(cwamm),NINT(vwam), &
                    NINT(vwamm),hiamchar,hiammchar,hnpcmchar,hnpcmmchar,vnpcmchar,vnpcmmchar,NINT(cnam),NINT(cnamm), &
                    NINT(hnam),NINT(hnamm),hinmchar,hinmmchar
                Close(FNUMEVAL)
                ! End of Evaluation.Out writes
                    
                ! Overview
                IF (IDETO /= 'E') THEN  ! No Overview if only need Evaluate
                    IF (FILEIOT(1:2) == 'DS') THEN  ! Overview headers for CSM
                        IF (RUN == 1 .AND. RUNI == 1) THEN
                            OPEN (UNIT = FNUMOV, FILE = FNAMEOV)
                            WRITE(FNUMOV,'("*SIMULATION OVERVIEW FILE")')
                        ELSE
                            INQUIRE (FILE = FNAMEOV, EXIST = FEXIST)
                            IF (FEXIST) THEN
                                OPEN (UNIT = FNUMOV, FILE = FNAMEOV, POSITION = 'APPEND')
                            ELSE
                                OPEN (UNIT = FNUMOV, FILE = FNAMEOV, STATUS = 'NEW')
                                WRITE(FNUMOV,'("*SIMULATION OVERVIEW FILE")')
                            ENDIF
                        ENDIF
                        WRITE (FNUMOV,*) ' '
                        CALL HEADER(1, FNUMOV, RUN)
                    ELSE    ! Overview header for standalone Cropsim
                        OPEN (UNIT = FNUMOV, FILE=FNAMEOV, POSITION='APPEND')
                        WRITE (FNUMOV,'(/,A79,/)') OUTHED
                        WRITE (FNUMOV, FMT203) MODEL
                        IF (ISWNIT == 'N') THEN
                            WRITE (FNUMOV,'(2(A, A1))') ' MODEL SWITCHES   Water: ', iswwat, '  Nitrogen: ', iswnit
                        ELSE
                            WRITE (FNUMOV,'(3(A, A1), A)') ' MODEL SWITCHES   Water: ', iswwat, '  Nitrogen: ', &
                                iswnit, ' (OM decay: ', mesom, ')'
                        ENDIF
                        WRITE (FNUMOV,'(A, A8)') ' MODULE           ', MODNAME
                        WRITE (FNUMOV,'(A, A1)') ' MODULE SWITCHES  Photosynthesis: ', MEPHO
                        ! P=PARU effic,I=P+internal CO2,R=resistances(Monteith)
                        WRITE (FNUMOV,'(A, A60)') ' FILE             ', FILENEW
                        WRITE (FNUMOV,'(6A)')' EXPERIMENT       ', EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
                        WRITE (FNUMOV,'(A, I3, 5X, A25)') ' TREATMENT', TN, TNAME
                        WRITE (FNUMOV,'(A, A2, A6, 2X, A16)') ' GENOTYPE         ', CROP, VARNO, VRNAME
                        WRITE(FNUMOV,*) ' '
                        CALL Calendar (plyear,plday,dom,month)
                        WRITE (FNUMOV, FMT208)month,dom,plyeardoy,NINT(pltpop),NINT(rowspc)
!                        CALL CSYR_DOY(EYEARDOY,YEAR,DOY)
                        CALL YR_DOY(EYEARDOY,YEAR,DOY)
                        CALL Calendar(year,doy,dom,month)
                        WRITE(FNUMOV, '(A, A3, I3, I8)') ' EMERGENCE        ', month, dom, eyeardoy                  
                        WRITE(FNUMOV,*) ' '
                        WRITE (FNUMOV, FMT209) tmaxx,tmaxm,tminn,tminm              
                        IF (ISWNIT /= 'N') THEN
                            !IF (ISWNIT /= 'N') THEN
                            WRITE(fnumov, FMT2095)cnad+rnad+hnad,hnad,vnad
                            WRITE(fnumov, FMT2096)sennal(0),sennas            
                            WRITE(fnumov, FMT2093)isoiln,amtnit,fsoiln
                            WRITE(fnumov, FMT2094)tnoxc,tlchc,tominfomc+tominsomc-tnimbsom  
                            WRITE(fnumov, FMT2099)tnimbsom,tominfomc,tominsomc   
                            IF (tominsom1 > 0.0)WRITE(fnumov, FMT2098)NINT(tominsom1c),NINT(tominsom2c),NINT(tominsom3c)
                        ENDIF  
                        IF (ISWWAT /= 'N') THEN
                            WRITE(fnumov, FMT2090)isoilh2o,rainc/10.0,irramtc/10.0
                            WRITE(fnumov, FMT2091)runoffc/10.0,drainc/10.0,fsoilh2o
                            WRITE(fnumov, FMT2089)eoc/10.0,eopenc/10.0,eompenc/10.0
                            WRITE(fnumov, FMT2097)eoptc/10.0,eoebudc/10.0
                        ENDIF
                    ENDIF  ! End of Overview header writes
                    WRITE(FNUMOV, FMT9589)
                    WRITE(fnumov,*)' '
                    WRITE(fnumov,'(A11,I4,A3,A60)')' RUN NO.   ',RUN,'  ',ENAME
                    IF (DYNAMIC == SEASEND) THEN
                        WRITE(fnumov,*)' '
                        WRITE(fnumov,'(A50,A25)')' NB. RUN TERMINATED PREMATURELY (PROBABLY BECAUSE ', &
                            'OF MISSING WEATHER DATA) '
                    ENDIF
                    WRITE(fnumov, FMT9588)
                    WRITE(fnumov, FMT9600)
                    DO L = 0, PSNUM
                        CALL Csopline(laic,laistg(l))
                        IF (STGYEARDOY(L) < 9999999.AND.L /= PSX.AND.L /= PSX+1) THEN
!                            CALL CSYR_DOY(STGYEARDOY(L),YEAR,DOY)
                            CALL YR_DOY(STGYEARDOY(L),YEAR,DOY)
                            CALL Calendar(year,doy,dom,month)
                            CNCTMP = -99
                            IF (CNADSTG(L) > 0.0 .AND. CWADSTG(L) > 0.0) CNCTMP = CNADSTG(L)/CWADSTG(L)*100
                            WRITE (FNUMOV,'(I8,I4,1X,A3,I4,1X,I1,1X,A13,I6,A6,F7.1,I5,F6.1,F6.2,F6.2)')STGYEARDOY(L), &
                                DOM,MONTH,Dapcalc(stgyeardoy(L),(plyeardoy/1000),plday),l,psname(l),NINT(CWADSTG(L)), &
                                LAIC,LNUMSTG(L),NINT(CNADSTG(L)),CNCTMP,1.0-WFPPAV(L-1),1.0-NFPPAV(L-1)
                        ENDIF
                    ENDDO
                    ! For harvest at specified date
                    IF (YEARDOYHARF == YEARDOY) THEN
                                
                        CALL Csopline(laic,lai)
!                        CALL CSYR_DOY(YEARDOYHARF,YEAR,DOY)
                        CALL YR_DOY(YEARDOYHARF,YEAR,DOY)
                        CALL Calendar(year,doy,dom,month)
                        CNCTMP = -99
                        IF (CNAD>0.0 .AND. CWAD > 0.0)CNCTMP = CNAD/CWAD*100
                        WRITE (FNUMOV,'(I8,I4,1X,A3,I4,1X,I1,1X,A13,I6,A6,F7.1,I5,F6.1,F6.2,F6.2)')YEARDOY,DOM, &
                            MONTH,Dapcalc(yeardoy,(plyeardoy/1000),plday),l,'Harvest      ',NINT(HWAD),LAIC,LNUM, &
                            NINT(CNAD),CNCTMP,1.0-WFPPAV(PSX-1),1.0-NFPPAV(PSX-1)
                            !NINT(CNAD),CNCTMP,1.0-WFPPAV(MSTG-1),1.0-NFPPAV(MSTG-1) !LPM 06MAR15 MSTG TO PSX
                    ENDIF 
                    IF (RUN == 1 .AND. RUNI == 1) THEN
                        WRITE(fnumov,*)' '
                        WRITE(fnumov,*)'BIOMASS  = Above-ground dry weight (Excl.seed;kg/ha)'
                        WRITE(fnumov,*)'LEAF AREA  = Leaf area index (m2/m2)'
                        WRITE(fnumov,*)'LEAF NUMBER  = Leaf number produced on main axis'
                        WRITE(fnumov,*)'CROP N  = Above-ground N (Excl.seed;kg/ha)'
                        WRITE(fnumov,*)'CROP N%  = Above-ground N concentration(%)'
                        WRITE(fnumov,*)'H2O STRESS = ','Photosynthesis stress, prior to stage (0=none,1=max)'
                        WRITE(fnumov,*)'N STRESS = ','Photosynthesis stress, prior to stage (0=none,1=max)'
                    ENDIF
                        WRITE(fnumov,*)' '
                        WRITE (FNUMOV, FMT206)
                        WRITE (FNUMOV, FMT290) MAX(-99,gdap),MAX(-99,gdapm),MAX(-99,edap),MAX(-99,edapm)
                        !DO L = 1,KEYSTX
                        DO L = 0,KEYSTX
                            IF (KEYPS(L) > 0) THEN
                                IF (psdap(keyps(l)) < -1) EXIT
                                WRITE (FNUMOV, FMT291)psname(KEYPS(L)),PSDap(KEYPS(L)),PSDapm(KEYPS(L))
                            ENDIF                 
                        ENDDO
                        ! For harvest at specified date
                    IF (YEARDOYHARF == YEARDOY) THEN
                        tvi1 = Dapcalc(yeardoy,(plyeardoy/1000),plday)
                        WRITE (FNUMOV,'(6X, A, 6X, I7, 4X, I7)')'Harvest      (dap)          ', NINT(hwam), NINT(hwamm)
                    ENDIF
                    WRITE (FNUMOV, FMT305)NINT(cwam),NINT(cwamm),MAX(-99,NINT(rwam+sdwam)),NINT(rwamm),NINT(senwacm), &
                        NINT(senwacmm),NINT(hwam),NINT(hwamm),NINT(vwam),NINT(vwamm),hiam,hiamm,NINT(rswam),NINT(rswamm)
                    IF (lwphc+swphc > 0.0) WRITE (FNUMOV, FMT306)NINT(cwahc),NINT(cwahcm)
                    WRITE (FNUMOV, FMT307)hwumchar,hwummchar,NINT(hnumam),NINT(hnumamm),hnumgm,hnumgmm,laix,laixm, &
                        lnumsm, lnumsmm,nupac,nupacm,cnam,cnamm,rnam,rnamm,sennatc,sennatcm,hnam,hnamm,vnam,vnamm, &
                        hinm,hinmm,hnpcm,hnpcmm,vnpcm,vnpcmm   
                            
                    WRITE(fnumov, FMT500)
                    PFPPAV = -99.0
                    PFGPAV = -99.0
                    !DO tvI1 = 1,mstg-2  !LPM 06MAR15 MSTG TO PSX
                    DO tvI1 = 0,PSX-2
                        IF (pdays(tvi1) > 0) THEN 
                            WRITE(fnumov, FMT600) psname(tvi1),' - ',psname(tvi1+1),pdays(tvI1),tmaxpav(tvI1), &          ! MF31AU14 REPLACED DASH  WITH A LITERAL
                            tminpav(tvI1),sradpav(tvI1),daylpav(tvI1),rainpc(tvI1),etpc(tvI1),1.-wfppav(tvi1), &
                                1.0-wfgpav(tvi1), 1.0-nfppav(tvi1), 1.0-nfgpav(tvi1), pfppav(tvi1), pfgpav(tvi1)
                        ENDIF
                    ENDDO
                    !IF(yeardoyharf == yeardoy)THEN                                                                        !LPM  07MAR15 MSTG TO PSX
                    !    WRITE(fnumov, FMT600) psname(mstg-1),' - ','Harvest   ', pdays(mstg-1),tmaxpav(mstg-1), &         ! MF31AU14 REPLACED DASH  WITH A LITERAL
                    !    tminpav(mstg-1),sradpav(mstg-1),daylpav(mstg-1),rainpc(mstg-1),etpc(mstg-1), &
                    !        1.-wfppav(mstg-1),1.0-wfgpav(mstg-1), 1.0-nfppav(mstg-1),1.0-nfgpav(mstg-1), &
                    !        pfppav(mstg-1),pfgpav(mstg-1)
                    !ELSE 
                    !    WRITE(fnumov, FMT600) psname(mstg-1),' - ',psname(mstg),pdays(mstg-1),tmaxpav(mstg-1), &           ! MF31AU14 REPLACED DASH  WITH A LITERAL
                    !    tminpav(mstg-1),sradpav(mstg-1),daylpav(mstg-1),rainpc(mstg-1),etpc(mstg-1), &
                    !        tminpav(mstg-1),sradpav(mstg-1),daylpav(mstg-1),rainpc(mstg-1),etpc(mstg-1), &
                    !        1.-wfppav(mstg-1),1.0-wfgpav(mstg-1),1.0-nfppav(mstg-1),1.0-nfgpav(mstg-1), &
                    !        pfppav(mstg-1),pfgpav(mstg-1)
                    !ENDIF
                    
                    IF(yeardoyharf == yeardoy)THEN
                        WRITE(fnumov, FMT600) psname(psx-1),' - ','Harvest   ', pdays(psx-1),tmaxpav(psx-1), &         ! MF31AU14 REPLACED DASH  WITH A LITERAL
                        tminpav(psx-1),sradpav(psx-1),daylpav(psx-1),rainpc(psx-1),etpc(psx-1), &
                            1.-wfppav(psx-1),1.0-wfgpav(psx-1), 1.0-nfppav(psx-1),1.0-nfgpav(psx-1), &
                            pfppav(psx-1),pfgpav(psx-1)
                    ELSE 
                        WRITE(fnumov, FMT600) psname(psx-1),' - ',psname(psx),pdays(psx-1),tmaxpav(psx-1), &           ! MF31AU14 REPLACED DASH  WITH A LITERAL
                        tminpav(psx-1),sradpav(psx-1),daylpav(psx-1),rainpc(psx-1),etpc(psx-1), &
                            tminpav(psx-1),sradpav(psx-1),daylpav(psx-1),rainpc(psx-1),etpc(psx-1), &
                            1.-wfppav(psx-1),1.0-wfgpav(psx-1),1.0-nfppav(psx-1),1.0-nfgpav(psx-1), &
                            pfppav(psx-1),pfgpav(psx-1)
                    ENDIF
                    !IF (pdays(mstg) > 0.OR.yeardoyharf == yeardoy) THEN                                                 !LPM  07MAR15 MSTG TO PSX
                    IF (pdays(psx) > 0.OR.yeardoyharf == yeardoy) THEN 
                        WRITE(fnumov,*) ' '
                        pfpcav = -99.0
                        pfgcav = -99.0 
                        !IF (pdays(mstg) > 0.) THEN                                                                        !LPM  07MAR15 MSTG TO PSX
                        !    WRITE(fnumov, FMT600) psname(1),' - ',psname(mstg), cdays, tmaxcav, tmincav, sradcav, &        ! MF31AU14 REPLACED DASH  WITH A LITERAL
                        
                        IF (pdays(psx) > 0.) THEN 
                            WRITE(fnumov, FMT600) psname(1),' - ',psname(psx), cdays, tmaxcav, tmincav, sradcav, &        ! MF31AU14 REPLACED DASH  WITH A LITERAL
                            daylcav, raincc, etcc, 1.0-wfpcav, 1.0-wfgcav, 1.0-nfpcav, 1.0-nfgcav,pfpcav, pfgcav
                        ELSE  
                            WRITE(fnumov, FMT600) psname(1),' - ','Harvest   ', cdays, tmaxcav, tmincav, sradcav, &        ! MF31AU14 REPLACED DASH  WITH A LITERAL
                            daylcav, raincc, etcc, 1.0-wfpcav, 1.0-wfgcav, 1.0-nfpcav, 1.0-nfgcav,pfpcav, pfgcav
                        ENDIF 
                        ! Resource productivity calculations
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
                        WRITE (FNUMOV, FMT1200) CDAYS,RAINCC, DMP_Rain*0.1, DMP_Rain, GrP_Rain*0.1, GrP_Rain, &
                            ETCC,DMP_ET*0.1,DMP_ET,GrP_ET*0.1,GrP_ET, EPCC,DMP_EP*0.1,DMP_EP,GrP_EP*0.1,GrP_EP
                        IF (IRRAMTC > 1.E-3) THEN
                            WRITE(FNUMOV, FMT1210) IRRAMTC, DMP_Irr*0.1, DMP_Irr, GrP_Irr*0.1, GrP_Irr
                        ENDIF  
                        IF (ISWNIT /= 'N') THEN
                            IF (Amtnit > 1.E-3) THEN
                                WRITE(FNUMOV, FMT1220) Amtnit, DMP_NApp, GrP_NApp 
                            ENDIF
                            IF (NUPAC > 1.E-3) THEN
                                WRITE(FNUMOV, FMT1230) NUPAC, DMP_NUpt,GrP_NUpt 
                            ENDIF
                        ENDIF ! ISWNIT NE 'N'
                        WRITE(FNUMOV, FMT270)
                        WRITE(FNUMOV, FMT300) 'CASSAVA', NINT(HWAM)
                        WRITE(FNUMOV,'(110("*"))')
                        CLOSE(FNUMOV)  ! Overview.out
                    ENDIF 
            ELSE   ! For Overview
                            
                OPEN (UNIT=FNUMOV, FILE=FNAMEOV, STATUS = 'UNKNOWN')
                CLOSE (UNIT=FNUMOV, STATUS = 'DELETE')
                            
            ENDIF  ! For Overview  (IDETO /= 'E')                    
                        
        ELSE ! For Evaluate,Overview  IDETL == '0'.OR.IDETO /= 'N'
                    
            OPEN (UNIT=FNUMMEAS, FILE=FNAMEMEAS, STATUS = 'UNKNOWN')
            CLOSE (UNIT=FNUMMEAS, STATUS = 'DELETE')
            OPEN (UNIT=FNUMEVAL, FILE=FNAMEEVAL, STATUS = 'UNKNOWN')
            CLOSE (UNIT=FNUMEVAL, STATUS = 'DELETE')
            OPEN (UNIT=FNUMOV, FILE=FNAMEOV, STATUS = 'UNKNOWN')
            CLOSE (UNIT=FNUMOV, STATUS = 'DELETE')
                    
        ENDIF  ! End Eval (IDETO) outputs (Evaluate,Overview)
        
    END SUBROUTINE YCA_Out_Eval 

        
