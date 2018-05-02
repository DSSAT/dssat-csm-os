!***************************************************************************************************************************
! This is the code from the section (DYNAMIC == INTEGR) lines 8517 - 8963 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine YCA_Out_WrkPhenRes outputs work details, phenology and plant responses (IDETL = D). 
!***************************************************************************************************************************
    
    SUBROUTINE YCA_Out_WrkPhenRes ( & 
        DYNAMIC     , IDETL       , IDETO       , ISWNIT      , NLAYR       , RLV         , RN          , RUN         , TN          &
        )
        
        USE ModuleDefs
        USE YCA_First_Trans_m
        USE YCA_Formats_m
        USE YCA_Control_Plant
     
        IMPLICIT NONE 
     
        INTEGER :: DYNAMIC     , NLAYR       , RN          , RUN         , TN          
        INTEGER :: DAPCALC     , TVILENT                                                      ! Integer function calls

        REAL    :: RLV(NL)      

        CHARACTER(LEN=1)  :: IDETL       , IDETO       , ISWNIT      
        
        ! If have not read measured data cannot produce A summaries
        IF (IDETL == 'D'.AND.IDETO == 'N') THEN
            WRITE(Message(1),'(A35)')'IDETL flag called for detail files.'
            WRITE(Message(2),'(A31,A31)')'But IDETO flag set at N so that','measured data not read.        '
            WRITE(Message(3),'(A45)')'Therefore,could not write detailed summaries.'
            CALL WARNING(3,'CSYCA',MESSAGE)
        ENDIF
                
        !-----------------------------------------------------------------------------------------------------------
        !         IDETL = D OUTPUTS (Work details; Phenols,m; Plantres,m)
        !-----------------------------------------------------------------------------------------------------------
                
        IF ((IDETL == 'D'.AND.IDETO /= 'N').OR.IDETL == 'A') THEN
                    
            ! WORK
            WRITE(fnumwrk,*) ' '
            WRITE(fnumwrk,'(A26,A10,I3)')' HARVEST/FAILURE DATA FOR ',excode,tn
            WRITE(fnumwrk,*)' '
            IF (DYNAMIC == SEASEND .AND. SEASENDOUT /= 'Y') THEN
                WRITE(fnumwrk,*)  ' Program terminated      ',YEARDOY
            ELSE 
                WRITE(fnumwrk,*)  ' Harvest reached         ',YEARDOY
            ENDIF  
            WRITE (fnumwrk,*)' '
            WRITE (fnumwrk,'(A54,F5.1,F4.1)')'  Overall PAR use efficientcy(incident,intercepted) = ',paruec,pariued
            WRITE(fnumwrk,*) ' '
            WRITE(fnumwrk,'(A27,F11.2)')'  Harvest product (kg/ha)  ',HWAM
            WRITE(fnumwrk,'(A27,F11.2)')'  Product/Total wt (HI)    ',HIAM
            WRITE(fnumwrk,*) ' '
            WRITE(fnumwrk,'(A26,A10,I3)')' CH2O BALANCE (kg/ha) FOR ',excode,tn
            WRITE(fnumwrk,'(A27,3F11.1)')'  SEED+FIXED (1) Seed,fixed',(SEEDRSI+SDCOAT+CARBOC)*plantPopulation(), &
                (SEEDRSI+SDCOAT)*plantPopulation(),CARBOC*plantPopulation()
            TVR1 = (SEEDRSI+SDCOAT+CARBOC)*plantPopulation()
            WRITE(fnumwrk,'(A27,3F11.1)')'  RESPIRED (2)  Tops,root  ',RESPC*plantPopulation(),RESPTC*plantPopulation(), &
                RESPRC*plantPopulation() 
            TVR2 = RESPC*plantPopulation()
            WRITE(fnumwrk,'(A27,3F11.1)')'  SENESCED (3)  Tops,root  ',(SENTOPLITTER+SENROOT)*plantPopulation(), &
                SENTOPLITTER*plantPopulation(),SENROOT*plantPopulation()
            TVR3 = (SENTOPLITTER+SENROOT)*plantPopulation()
            TVR4 = (SEEDRS+SDCOAT+RTWT+SRWT+canopyWeight())*plantPopulation()
            WRITE(fnumwrk,'(A27,3F11.1)')'  PLANT+SEED_RESIDUE Pl,sd ',(SEEDRS+SDCOAT+RTWT+SRWT+canopyWeight()) &
                *plantPopulation(),(RTWT+SRWT+canopyWeight())*plantPopulation(),(SEEDRS+SDCOAT)*plantPopulation()
            WRITE(fnumwrk,'(A27,2F11.1)')'  RESERVES (5)             ',RSWT*plantPopulation() 
            TVR5 = RSWT*plantPopulation()
            WRITE(fnumwrk,'(A29, F9.1)')'  HARVESTED DURING CYCLE (6) ',(LWPHC+SWPHC+RSWPHC)*plantPopulation()
            TVR6 = (LWPHC+SWPHC+RSWPHC)*plantPopulation()
            WRITE(fnumwrk,'(A27, F11.2)')'  BALANCE (1-(2+3+4+6))    ',TVR1 -(TVR2+TVR3+TVR4+TVR6)
            IF (ABS(TVR1-(TVR2+TVR3+TVR4+TVR6)) > 0.05)WRITE(fnumwrk,'(A29,A10,A1,I2)') &
                '  *PROBLEM WITH CH2O BALANCE ',EXCODE,' ',TN
                    
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A21,A10,I3)')' RESERVES STATUS FOR ',excode,tn
            WRITE (fnumwrk,'(A22,F7.1)')'  Kg/ha at maximum    ',RSWTX*plantPopulation()
            WRITE (fnumwrk,'(A22,F7.1)')'  % above ground      ',RSCX*100.
            WRITE (fnumwrk,'(A22,F7.1)')'  Kg/ha at harvest    ',RSWAD
            IF (canopyWeight() > 0) WRITE (fnumwrk,'(A22,F7.1)')'  % above ground      ', &
                rswt/(canopyWeight())*100.0
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A34,A10,I3)')' SEED USE (KG/HA or PER CENT) FOR ',excode,tn
            WRITE (fnumwrk,'(A22,F7.3)')'  Initial reserves    ',seedrsi*plantPopulation()
            WRITE (fnumwrk,'(A22,F7.3)')'  Use for tops        ',seeduset*plantPopulation()
            WRITE (fnumwrk,'(A22,F7.3)')'  Use for roots       ',seeduser*plantPopulation()
            WRITE (fnumwrk,'(A22,F7.3)')'  Total use           ',(seeduset+seeduser)*plantPopulation()
            IF (seeduser+seeduset > 0.0)WRITE (fnumwrk,'(A22,F7.3)')'  Percent to tops     ', &
                seeduset/(seeduset+seeduser)*100.0
            WRITE(fnumwrk,*)' '
            WRITE (fnumwrk,'(A35,A10,I3)')' DEAD MATTER AND ROOTS (KG/HA) FOR ',excode,tn
            WRITE(fnumwrk,'(A32,F8.1)')'  DEAD MATERIAL LEFT ON SURFACE  ',SENTOPLITTERA
            WRITE(fnumwrk,'(A32,F8.1)')'  DEAD MATERIAL LEFT IN SOIL     ',SENROOTA
            WRITE(fnumwrk,'(A32,F8.1)')'  ROOT WEIGHT AT HARVEST/FAILURE ',RWAD
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A20,A10,I3)')' ROOTS BY LAYER FOR ',excode,tn
            WRITE (fnumwrk,'(A19)')'  LAYER  RTWT   RLV'
            DO L=1,NLAYR
                IF (RTWTAL(L) > 0.0) WRITE (fnumwrk,'(I6,F7.1,F6.2)')L,RTWTAL(L),RLV(L)
            ENDDO
            IF (RTSLXDATE > 0) THEN
                WRITE(fnumwrk,'(A30,I7)')'  FINAL SOIL LAYER REACHED ON ',RTSLXDATE
                WRITE(fnumwrk,'(A15,I7,A1)')'  (MATURITY ON ',YEARDOY,')'
            ELSE
                WRITE(fnumwrk,*)' FINAL SOIL LAYER NOT REACHED '
            ENDIF
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A40)')' PRINCIPAL AND SECONDARY STAGES         '
            WRITE (fnumwrk,'(A40)')'  STAGE NAME   DAYS > PLANTING  LEAF #  '
            WRITE (fnumwrk,'(A15,F7.1)')'   Germination ',gdapfr
            WRITE (fnumwrk,'(A15,F7.1)')'   Emergence   ',edapfr
            DO L = 2,PSNUM
                CALL CSUCASE (PSNAME(L))
                !IF (PSNAME(L)(1:3) == 'HAR'.AND.PSDAPFR(l) <= 0.0) psdapfr(l) = psdapfr(mstg) !LPM  07MAR15 MSTG TO PSX
                !IF (PSNAME(L)(1:3) == 'END'.AND.PSDAPFR(l) <= 0.0) psdapfr(l) = psdapfr(mstg)
                IF (PSNAME(L)(1:3) == 'HAR'.AND.PSDAPFR(l) <= 0.0) psdapfr(l) = psdapfr(PSX)
                IF (PSNAME(L)(1:3) == 'END'.AND.PSDAPFR(l) <= 0.0) psdapfr(l) = psdapfr(PSX)
                IF (PSNAME(L)(1:3) /= 'FAI') THEN
                    IF (psdapfr(l) > 0) WRITE (FNUMWRK,'(A3,A13,F6.1,9X,F6.1)')'   ',psname(l),psdapfr(l),lnumsimtostg(l)
                ELSE
                    IF (CFLFAIL == 'Y'.AND.psdapfr(l) > 0)WRITE (FNUMWRK,'(A3,A13,F6.1)')'   ',psname(l),psdapfr(l)
                ENDIF
                IF (TVILENT(PSNAME(L)) < 5) EXIT
            ENDDO
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A28,A10,I3)')' STRESS FACTOR AVERAGES FOR ',excode,tn
            WRITE (fnumwrk,'(A55)')'  TIER   H2O(PS)   H2O(GR)   N(PS)     N(GR)   TIER_END'
            !DO L=1,MSTG-2                  !LPM  07MAR15 MSTG TO PSX
            DO L=0,PSX-2 
                WRITE (fnumwrk,'(I6,F8.2,3F10.2,2X,A13)')l,1.0-wfppav(l),1.0-wfgpav(l),1.0-nfppav(l),1.0-nfgpav(l), &
                    psname(MIN(L+1,PSX))
            ENDDO
            IF(yeardoyharf == yeardoy)THEN
                WRITE (fnumwrk,'(I6,F8.2,3F10.2,2X,A13)')l,1.0-wfppav(l),1.0-wfgpav(l),1.0-nfppav(l),1.0-nfgpav(l), &
                    'HARVEST      '
            ELSE 
                !WRITE (fnumwrk,'(I6,F8.2,3F10.2,2X,A13)')l,1.0-wfppav(mstg-1),1.0-wfgpav(mstg-1),1.0-nfppav(mstg-1), &    !LPM  07MAR15 MSTG TO PSX
                !    1.0-nfgpav(mstg-1),psname(mstg)
                WRITE (fnumwrk,'(I6,F8.2,3F10.2,2X,A13)')l,1.0-wfppav(psx-1),1.0-wfgpav(psx-1),1.0-nfppav(psx-1), &
                    1.0-nfgpav(psx-1),psname(psx)
            ENDIF
            WRITE (fnumwrk,'(A42)')'  NB 0.0 = minimum ; 1.0 = maximum stress.'
            ! LAH  Must change from daily leaf cohorts to bigger unit
            ! Too much to output if daily
            !WRITE (fnumwrk,*)' '
            !WRITE (fnumwrk,'(A23)') ' COHORT   AREA    AREAS'
            !DO L = 1, LCNUM
            !  WRITE (fnumwrk,'(I7,2F8.3)') L,LCOA(L),LCOAS(L)
            !ENDDO
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A24,A10,I3)')' SHOOT SIZES (cm2) FOR  ',excode,tn
            WRITE (fnumwrk,'(A32)') '   SHOOT  BIRTH   AREAP   AREAS '
            DO I = 1,INT(SHNUM)
                WRITE (fnumwrk,'(I7,I8,2I8)')I,SHDAT,NINT(SHLA(I)),NINT(SHLAS(I))
            ENDDO
            IF (ISWNIT /= 'N') THEN
                WRITE (fnumwrk,*) ' '
                WRITE (fnumwrk,'(A25,A10,I3)')' N BALANCE (kg/ha) FOR ',excode,tn
                WRITE (fnumwrk,'(A34,F8.2,2F11.2)')'   N UPTAKE + SEED (1)            ', (NUPC+SEEDNI)*plantPopulation(), &
                    NUPC*PLTPOP*10.,SEEDNI*PLTPOP*10.
                TVR1 = (NUPC+SEEDNI)*plantPopulation()  
                WRITE (fnumwrk,'(A33,F9.2,2F11.2)')'   TOTAL N SENESCED (2) Tops,Root',(SENNL(0)+SENNS)*plantPopulation(), &
                    SENNL(0)*plantPopulation(),SENNS*plantPopulation()
                TVR2 = (SENNL(0)+SENNS)*plantPopulation() 
                WRITE (fnumwrk,'(A34,F8.2)')'   TOTAL N IN PLANT (3)           ', &
                    plantPopulation()*(ROOTN+SROOTN+LEAFN+STEMN+RSN+SEEDN)
                TVR3 = (ROOTN+SROOTN+LEAFN+STEMN+RSN+SEEDN)*plantPopulation()         
                WRITE (fnumwrk,'(A33, F9.2)')'   HARVESTED DURING CYCLE (4)    ',plantPopulation()*LNPHC+SNPHC+RSNPHC
                TVR4 = (LNPHC+SNPHC+RSNPHC)* plantPopulation()
                WRITE (fnumwrk,'(A34,F8.3)')'   BALANCE (1-(2+3+4))            ',TVR1-TVR2-TVR3-TVR4
                IF (ABS(TVR1-(TVR2+TVR3+TVR4)) > 0.005)WRITE(fnumwrk,'(A26,A10,A1,I2)')'  *PROBLEM WITH N BALANCE ' &
                    ,EXCODE,' ',TN
            ENDIF
            ! End of Detailed WORK writes
                    
            ! Phenology (Simulated; PHENOLS.OUT)
            INQUIRE (FILE = FNAMEPHENOLS,EXIST = FFLAG)
            OPEN(UNIT=FNUMPHES,FILE=FNAMEPHENOLS,POSITION='APPEND')
            IF (CROP /= CROPPREV.OR.RUN == 1.OR.(.NOT.(FFLAG))) THEN
                WRITE (FNUMPHES,'(/,A14,A10)')'*PHENOLOGY(S):',EXCODE
                WRITE (FNUMPHES,'(A16,A24)',ADVANCE='NO') '@ EXCODE    TRNO',' PYEAR  PDAT  GDAP  EDAP'
                !DO L = 1,KEYSTX
                DO L = 0,KEYSTX
                    IF (KEYPS(L) > 0)THEN
                        WRITE (FNUMPHES,'(A6)',ADVANCE='NO') PSABVO(KEYPS(L))
                    ENDIF
                ENDDO
                WRITE (fnumphes,'(/,A10,I6,2I6,2F6.1)',ADVANCE='NO')EXCODE,TN,PLYEAR,PLDAY,gdapfr,edapfr
            ELSE  ! End Phenology simulated header writes
                WRITE (fnumphes,'(A10,I6,2I6,2F6.1)',ADVANCE='NO')EXCODE,TN,PLYEAR,PLDAY,gdapfr,edapfr
            ENDIF
            !DO L = 1,KEYSTX
            DO L = 0,KEYSTX
                IF (KEYPS(L) > 0) WRITE (FNUMPHES,'(I6)',ADVANCE='NO')PSDAP(KEYPS(L))
            ENDDO
            CLOSE (FNUMPHES)
            ! End Phenology simulated writes              
                    
            ! Phenology (Measured;PHENOLM.OUT)
            IF (TDATANUM <= 0 .AND. .NOT.FEXISTA) THEN
                WRITE (fnumwrk,*)' '
                WRITE (fnumwrk,*)' No data so cannot write PHENOLOGY (MEASURED)' 
                OPEN (UNIT=FNUMPHEM,FILE=FNAMEPHENOLM,STATUS ='UNKNOWN')
                CLOSE (UNIT=FNUMPHEM, STATUS = 'DELETE')
            ELSE
                INQUIRE (FILE = FNAMEPHENOLM,EXIST = FFLAG)
                OPEN(UNIT=FNUMPHEM,FILE=FNAMEPHENOLM,POSITION='APPEND')
                IF (CROP /= CROPPREV.OR.RUN == 1.OR.(.NOT.(FFLAG))) THEN
                    WRITE (FNUMPHEM,'(/,A14,A10)')'*PHENOLOGY(M):',EXCODE
                    WRITE (FNUMPHEM,'(A16,A24)',ADVANCE='NO')'@EXCODE     TRNO',' PYEAR  PDAT  GDAP  EDAP'
                    !DO L = 1,KEYSTX
                    DO L = 0,KEYSTX
                        IF (KEYPS(L) > 0) THEN
                            WRITE (FNUMPHEM,'(A6)',ADVANCE='NO')PSABVO(KEYPS(L))
                        ENDIF 
                    ENDDO
                    WRITE (FNUMPHEM,'(/,A10,I6,2I6,2F6.1)',ADVANCE='NO')EXCODE,TN,PLYEAR,PLDAY,gdapfr,edapfr
                ELSE ! End Phenology measured header writes
                    WRITE (FNUMPHEM,'(A10,I6,2I6,2F6.1)',ADVANCE='NO')EXCODE,TN,PLYEAR,PLDAY,gdapfr,edapfr
                ENDIF
                !DO L = 1,KEYSTX
                DO L = 0,KEYSTX
                    IF (KEYPS(L) > 0) WRITE (FNUMPHEM,'(I6)',ADVANCE='NO')PSDAPM(KEYPS(L))
                ENDDO
                CLOSE (FNUMPHEM)
            ENDIF  
            ! End Phenology (Measured)
                    
            ! Plant responses (Simulated)'
            ! Set temporary planting date for overlapping year end
            IF (RUNCRP == 1) PLDAYTMP = -99
            IF (PLDAY < PLDAYTMP) THEN
                IF (VARNO == VARNOPREV) THEN
                    PLDAYTMP = PLDAY + 365
                ELSE
                    PLDAYTMP = PLDAY
                ENDIF
            ELSE
                PLDAYTMP = PLDAY
            ENDIF
            PLDAYTMP = PLDAY
            IF (EXCODE /= EXCODEPREV.OR.TNAME(1:1) == '*') THEN
                OPEN (UNIT=FNUMPRES,FILE=FNAMEPRES,POSITION='APPEND')
                WRITE (FNUMPRES,*) ' '
                WRITE (TLINETMP,'(A,A10,A8)') '*RESPONSES(S):',EXCODE,MODNAME
                IF (TNAME(1:1) == '*') THEN
                    WRITE (FNUMPRES,'(A180)') TLINETMP
                ELSE
                    WRITE (FNUMPRES,'(A180)') TLINETMP
                ENDIF
                WRITE (FNUMPRES,'(4A)',ADVANCE='NO')'@  RUN',' EXCODE   ',' TRNO RN    CR','  PDAT  EDAP'
                !DO L = 1,KEYSTX
                DO L = 0,KEYSTX
                    IF (KEYPS(L) > 0) THEN
                        WRITE (FNUMPRES,'(A6)',ADVANCE='NO') PSABVO(KEYPS(L))
                    ENDIF  
                ENDDO
                WRITE (FNUMPRES,'(9A)') '  HWAH  HWUH','  H#AH  H#GH  LAIX  L#SH ', &
                    'BR0AH BR1AH BR2AH BR3AH BR4AH BR5AH BR6AH BR7AH BR8AH BR9AH BR0AH BR1AH BR2AH ', &
                    ' CWAH  VWAH  HIAH  RWAH', '  HN%H  TNAH','  CNAH  HNAH','  HINH PLPOP', &
                    '  NICH',' SRADA TMAXA TMINA  PRCP'
            ELSE
                OPEN (UNIT=FNUMPRES,FILE=FNAMEPRES,POSITION='APPEND')
            ENDIF  ! End Responses simulated header writes
            WRITE (FNUMPRES,'(I6,1X,A10,I4,I3,4X,A2, I6, F6.1)',ADVANCE='NO') RUN,EXCODE,TN,RN,CROP,PLDAYTMP,EDAPFR
            !DO L = 1,KEYSTX
            DO L = 0,KEYSTX
                !IF (L == MSTG.AND.HNUMBER == 1) THEN          !LPM  07MAR15 MSTG TO PSX
                IF (L == PSX.AND.HNUMBER == 1) THEN
                    ! If harvested at a specific date
                    tvi1 = Dapcalc(yeardoyharf,plyear,plday)
                    WRITE (FNUMPRES,'(I6)',ADVANCE='NO') tvi1
                ELSE
                    IF (KEYPS(L) > 0) WRITE(FNUMPRES,'(I6)',ADVANCE='NO')PSDAP(KEYPS(L))
                ENDIF
            ENDDO
            WRITE (fnumpres, FMT409)NINT(hwam),hwumchar, NINT(hnumam),NINT(hnumgm),laixchar,lnumsm,brnumsh,NINT(cwam), &
                NINT(vwam),hiamchar,NINT(rwam),hnpcmchar,NINT(AMAX1(-99.0,cnam+rnam)),NINT(cnam),NINT(hnam),hinmchar, &
                pltpop,NINT(amtnit),sradcav,tmaxcav,tmincav,NINT(raincc)
            CLOSE(FNUMPRES)
            ! End Responses simulated writes
                    
            ! Plant responses (Measured)
            IF (TDATANUM <= 0 .AND. .NOT.FEXISTA) THEN
                WRITE (fnumwrk,*)' '
                WRITE (fnumwrk,*)' No data so cannot write PLANT RESPONSES (MEASURED)'
                OPEN (UNIT = FNUMTMP,FILE = FNAMEPREM,STATUS='UNKNOWN')
                CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
            ELSE
                IF (EXCODE /= EXCODEPREV.OR.TNAME(1:1) == '*') THEN
                    OPEN (UNIT=FNUMPREM,FILE=FNAMEPREM,POSITION='APPEND')
                    WRITE (FNUMPREM,*) ' '
                    WRITE (TLINETMP, FMT99511) EXCODE,MODNAME
                    IF (TNAME(1:1) == '*') THEN
                        WRITE (FNUMPREM,'(A180)') TLINETMP
                    ELSE
                        WRITE (FNUMPREM,'(A180)') TLINETMP
                    ENDIF
                    WRITE (FNUMPREM,'(4A)',ADVANCE='NO')'@  RUN',' EXCODE   ',' TRNO RN    CR','  PDOY  EDAP'

                    !DO L = 1,KEYSTX
                    DO L = 0,KEYSTX
                        IF (KEYPS(L) > 0) THEN
                            WRITE (FNUMPREM,'(A6)',ADVANCE='NO') PSABVO(KEYPS(L))
                        ENDIF 
                    ENDDO
                    WRITE (FNUMPREM, FMT298)
                ELSE
                    OPEN (UNIT=FNUMPREM,FILE=FNAMEPREM,POSITION='APPEND')
                ENDIF ! End Responses measured header writes
                WRITE (FNUMPREM,'(I6,1X,A10,I4,I3,4X,A2, I6, F6.1)',ADVANCE='NO') &
                    RUN,EXCODE,TN,RN,CROP,PLDAYTMP,FLOAT(MAX(-99,edapm))
                !DO L = 1,KEYSTX
                DO L = 0,KEYSTX
                    !IF (L == MSTG.AND.HNUMBER == 1) THEN          !LPM  07MAR15 MSTG TO PSX
                    IF (L == PSX.AND.HNUMBER == 1) THEN
                        ! If harvested at a specific date
                        tvi1 = Dapcalc(yeardoyharf,plyear,plday)
                        WRITE (FNUMPREM,'(I6)',ADVANCE='NO') tvi1
                    ELSE
                        IF (KEYPS(L) > 0) WRITE(FNUMPREM,'(I6)',ADVANCE='NO')PSDAPM(KEYPS(L))
                    ENDIF
                ENDDO
                !WRITE (FNUMPREM,'(I6)',ADVANCE='NO') PSDAPM(HSTG)
                WRITE (FNUMPREM, FMT411) NINT(hwamm),hwummchar,NINT(hnumamm),NINT(hnumgmm),laixmchar,lnumsmm,brnumshm, &
                    NINT(cwamm),NINT(vwamm),hiammchar,NINT(rwamm),hnpcmmchar,NINT(tnamm),NINT(cnamm),NINT(hnamm), &
                    hinmmchar,ppop,NINT(amtnit),sradcav,tmaxcav,tmincav,NINT(raincc) !LPM 06MAR2016 To have just one name for PPOP
                CLOSE(FNUMPREM)
            ENDIF  
            ! End Responses (Measured)
                    
        ELSE  ! IDETL = 'D'
                    
            OPEN (UNIT=FNUMPHES,FILE=FNAMEPHENOLS,STATUS = 'UNKNOWN')
            CLOSE (UNIT=FNUMPHES, STATUS = 'DELETE')
            OPEN (UNIT=FNUMPHEM,FILE=FNAMEPHENOLM,STATUS = 'UNKNOWN')
            CLOSE (UNIT=FNUMPHEM, STATUS = 'DELETE')
            OPEN (UNIT = FNUMPRES,FILE = FNAMEPRES,STATUS='UNKNOWN')
            CLOSE (UNIT=FNUMPRES, STATUS = 'DELETE')
            OPEN (UNIT = FNUMPREM,FILE = FNAMEPREM,STATUS='UNKNOWN')
            CLOSE (UNIT=FNUMPREM, STATUS = 'DELETE')
                    
        ENDIF ! IDETL = 'D'

    END SUBROUTINE YCA_Out_WrkPhenRes 
        
