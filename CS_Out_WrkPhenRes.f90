!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.INTEGR) lines 8517 - 8963 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in Module_CSCAS_Vars_List. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine CS_Out_WrkPhenRes outputs work details, phenology and plant responses (IDETL = D). 
!***************************************************************************************************************************
    
    SUBROUTINE CS_Out_WrkPhenRes ( & 
        DYNAMIC     , IDETL       , IDETO       , ISWNIT      , NLAYR       , RLV         , RN          , RUN         , TN          &
        )
        
        USE Module_CSCAS_Vars_List
        USE Module_CS_Formats
     
        IMPLICIT NONE 
     
        INTEGER :: DYNAMIC     , NLAYR       , RN          , RUN         , TN          
        INTEGER :: DAPCALC     , TVILENT                                                      ! Integer function calls

        REAL    :: RLV(NL)      

        CHARACTER(LEN=1)  :: IDETL       , IDETO       , ISWNIT      
        
        ! If have not read measured data cannot produce A summaries
        IF (IDETL.EQ.'D'.AND.IDETO.EQ.'N') THEN
            WRITE(Message(1),'(A35)')'IDETL flag called for detail files.'
            WRITE(Message(2),'(A31,A31)')'But IDETO flag set at N so that','measured data not read.        '
            WRITE(Message(3),'(A45)')'Therefore,could not write detailed summaries.'
            CALL WARNING(3,'CSCAS',MESSAGE)
        ENDIF
                
        !-----------------------------------------------------------------------------------------------------------
        !         IDETL = D OUTPUTS (Work details; Phenols,m; Plantres,m)
        !-----------------------------------------------------------------------------------------------------------
                
        IF ((IDETL.EQ.'D'.AND.IDETO.NE.'N').OR.IDETL.EQ.'A') THEN
                    
            ! WORK
            WRITE(fnumwrk,*) ' '
            WRITE(fnumwrk,'(A26,A10,I3)')' HARVEST/FAILURE DATA FOR ',excode,tn
            WRITE(fnumwrk,*)' '
            IF (DYNAMIC.EQ.SEASEND .AND. SEASENDOUT.NE.'Y') THEN
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
            WRITE(fnumwrk,'(A27,3F11.1)')'  SEED+FIXED (1) Seed,fixed',(SEEDRSI+SDCOAT+CARBOC)*PLTPOP*10.0, &
                (SEEDRSI+SDCOAT)*PLTPOP*10.0,CARBOC*PLTPOP*10.0
            TVR1 = (SEEDRSI+SDCOAT+CARBOC)*PLTPOP*10.0
            WRITE(fnumwrk,'(A27,3F11.1)')'  RESPIRED (2)  Tops,root  ',RESPC*PLTPOP*10.0,RESPTC*PLTPOP*10.0, &
                RESPRC*PLTPOP*10.0 
            TVR2 = RESPC*PLTPOP*10.0
            WRITE(fnumwrk,'(A27,3F11.1)')'  SENESCED (3)  Tops,root  ',(SENTOPLITTER+SENROOT)*PLTPOP*10.0, &
                SENTOPLITTER*PLTPOP*10.0,SENROOT*PLTPOP*10.0
            TVR3 = (SENTOPLITTER+SENROOT)*PLTPOP*10.0
            TVR4 = (SEEDRS+SDCOAT+RTWT+SRWT+LFWT+STWT+CRWT+RSWT)*PLTPOP*10.0
            WRITE(fnumwrk,'(A27,3F11.1)')'  PLANT+SEED_RESIDUE Pl,sd ',(SEEDRS+SDCOAT+RTWT+SRWT+LFWT+STWT+CRWT+RSWT) &
                *PLTPOP*10.0,(RTWT+SRWT+LFWT+STWT+CRWT+RSWT)*PLTPOP*10.0,(SEEDRS+SDCOAT)*PLTPOP*10.0
            WRITE(fnumwrk,'(A27,2F11.1)')'  RESERVES (5)             ',RSWT*PLTPOP*10.0 
            TVR5 = RSWT*PLTPOP*10.0
            WRITE(fnumwrk,'(A29, F9.1)')'  HARVESTED DURING CYCLE (6) ',(LWPHC+SWPHC+RSWPHC)*PLTPOP*10.0
            TVR6 = (LWPHC+SWPHC+RSWPHC)*PLTPOP*10.0
            WRITE(fnumwrk,'(A27, F11.2)')'  BALANCE (1-(2+3+4+6))    ',TVR1 -(TVR2+TVR3+TVR4+TVR6)
            IF (ABS(TVR1-(TVR2+TVR3+TVR4+TVR6)).GT.0.05)WRITE(fnumwrk,'(A29,A10,A1,I2)') &
                '  *PROBLEM WITH CH2O BALANCE ',EXCODE,' ',TN
                    
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A21,A10,I3)')' RESERVES STATUS FOR ',excode,tn
            WRITE (fnumwrk,'(A22,F7.1)')'  Kg/ha at maximum    ',RSWTX*PLTPOP*10.0
            WRITE (fnumwrk,'(A22,F7.1)')'  % above ground      ',RSCX*100.
            WRITE (fnumwrk,'(A22,F7.1)')'  Kg/ha at harvest    ',RSWAD
            IF (lfwt+stwt+crwt+rswt.GT.0) WRITE (fnumwrk,'(A22,F7.1)')'  % above ground      ', &
                rswt/(lfwt+stwt+crwt+rswt)*100.0
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A34,A10,I3)')' SEED USE (KG/HA or PER CENT) FOR ',excode,tn
            WRITE (fnumwrk,'(A22,F7.3)')'  Initial reserves    ',seedrsi*pltpop*10.0
            WRITE (fnumwrk,'(A22,F7.3)')'  Use for tops        ',seeduset*pltpop*10.0
            WRITE (fnumwrk,'(A22,F7.3)')'  Use for roots       ',seeduser*pltpop*10.0
            WRITE (fnumwrk,'(A22,F7.3)')'  Total use           ',(seeduset+seeduser)*pltpop*10.0
            IF (seeduser+seeduset.GT.0.0)WRITE (fnumwrk,'(A22,F7.3)')'  Percent to tops     ', &
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
                IF (RTWTAL(L).GT.0.0) WRITE (fnumwrk,'(I6,F7.1,F6.2)')L,RTWTAL(L),RLV(L)
            ENDDO
            IF (RTSLXDATE.GT.0) THEN
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
                IF (PSNAME(L)(1:3).EQ.'HAR'.AND.PSDAPFR(l).LE.0.0) psdapfr(l) = psdapfr(mstg)
                IF (PSNAME(L)(1:3).EQ.'END'.AND.PSDAPFR(l).LE.0.0) psdapfr(l) = psdapfr(mstg)
                IF (PSNAME(L)(1:3).NE.'FAI') THEN
                    IF (psdapfr(l).GT.0) WRITE (FNUMWRK,'(A3,A13,F6.1,9X,F6.1)')'   ',psname(l),psdapfr(l),lnumsimtostg(l)
                ELSE
                    IF (CFLFAIL.EQ.'Y'.AND.psdapfr(l).GT.0)WRITE (FNUMWRK,'(A3,A13,F6.1)')'   ',psname(l),psdapfr(l)
                ENDIF
                IF (TVILENT(PSNAME(L)).LT.5) EXIT
            ENDDO
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A28,A10,I3)')' STRESS FACTOR AVERAGES FOR ',excode,tn
            WRITE (fnumwrk,'(A55)')'  TIER   H2O(PS)   H2O(GR)   N(PS)     N(GR)   TIER_END'
            DO L=1,MSTG-2 
                WRITE (fnumwrk,'(I6,F8.2,3F10.2,2X,A13)')l,1.0-wfppav(l),1.0-wfgpav(l),1.0-nfppav(l),1.0-nfgpav(l), &
                    psname(MIN(L+1,PSX))
            ENDDO
            IF(yeardoyharf.EQ.yeardoy)THEN
                WRITE (fnumwrk,'(I6,F8.2,3F10.2,2X,A13)')l,1.0-wfppav(l),1.0-wfgpav(l),1.0-nfppav(l),1.0-nfgpav(l), &
                    'HARVEST      '
            ELSE 
                WRITE (fnumwrk,'(I6,F8.2,3F10.2,2X,A13)')l,1.0-wfppav(mstg-1),1.0-wfgpav(mstg-1),1.0-nfppav(mstg-1), &
                    1.0-nfgpav(mstg-1),psname(mstg)
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
            IF (ISWNIT.NE.'N') THEN
                WRITE (fnumwrk,*) ' '
                WRITE (fnumwrk,'(A25,A10,I3)')' N BALANCE (kg/ha) FOR ',excode,tn
                WRITE (fnumwrk,'(A34,F8.2,2F11.2)')'   N UPTAKE + SEED (1)            ', (NUPC+SEEDNI)*PLTPOP*10.0, &
                    NUPC*PLTPOP*10.,SEEDNI*PLTPOP*10.
                TVR1 = (NUPC+SEEDNI)*PLTPOP*10.0  
                WRITE (fnumwrk,'(A33,F9.2,2F11.2)')'   TOTAL N SENESCED (2) Tops,Root',(SENNL(0)+SENNS)*PLTPOP*10.0, &
                    SENNL(0)*PLTPOP*10.0,SENNS*PLTPOP*10.0
                TVR2 = (SENNL(0)+SENNS)*PLTPOP*10.0 
                WRITE (fnumwrk,'(A34,F8.2)')'   TOTAL N IN PLANT (3)           ', &
                    PLTPOP*10.0*(ROOTN+SROOTN+LEAFN+STEMN+RSN+SEEDN)
                TVR3 = (ROOTN+SROOTN+LEAFN+STEMN+RSN+SEEDN)*PLTPOP*10.0         
                WRITE (fnumwrk,'(A33, F9.2)')'   HARVESTED DURING CYCLE (4)    ',PLTPOP*10.0*LNPHC+SNPHC+RSNPHC
                TVR4 = (LNPHC+SNPHC+RSNPHC)* PLTPOP*10.0
                WRITE (fnumwrk,'(A34,F8.3)')'   BALANCE (1-(2+3+4))            ',TVR1-TVR2-TVR3-TVR4
                IF (ABS(TVR1-(TVR2+TVR3+TVR4)).GT.0.005)WRITE(fnumwrk,'(A26,A10,A1,I2)')'  *PROBLEM WITH N BALANCE ' &
                    ,EXCODE,' ',TN
            ENDIF
            ! End of Detailed WORK writes
                    
            ! Phenology (Simulated; PHENOLS.OUT)
            INQUIRE (FILE = FNAMEPHENOLS,EXIST = FFLAG)
            OPEN(UNIT=FNUMPHES,FILE=FNAMEPHENOLS,POSITION='APPEND')
            IF (CROP.NE.CROPPREV.OR.RUN.EQ.1.OR.(.NOT.(FFLAG))) THEN
                WRITE (FNUMPHES,'(/,A14,A10)')'*PHENOLOGY(S):',EXCODE
                WRITE (FNUMPHES,'(A16,A24)',ADVANCE='NO') '@ EXCODE    TRNO',' PYEAR  PDAT  GDAP  EDAP'
                DO L = 1,KEYSTX
                    IF (KEYPS(L).GT.0)THEN
                        WRITE (FNUMPHES,'(A6)',ADVANCE='NO') PSABVO(KEYPS(L))
                    ENDIF
                ENDDO
                WRITE (fnumphes,'(/,A10,I6,2I6,2F6.1)',ADVANCE='NO')EXCODE,TN,PLYEAR,PLDAY,gdapfr,edapfr
            ELSE  ! End Phenology simulated header writes
                WRITE (fnumphes,'(A10,I6,2I6,2F6.1)',ADVANCE='NO')EXCODE,TN,PLYEAR,PLDAY,gdapfr,edapfr
            ENDIF
            DO L = 1,KEYSTX
                IF (KEYPS(L).GT.0) WRITE (FNUMPHES,'(I6)',ADVANCE='NO')PSDAP(KEYPS(L))
            ENDDO
            CLOSE (FNUMPHES)
            ! End Phenology simulated writes              
                    
            ! Phenology (Measured;PHENOLM.OUT)
            IF (TDATANUM.LE.0 .AND. .NOT.FEXISTA) THEN
                WRITE (fnumwrk,*)' '
                WRITE (fnumwrk,*)' No data so cannot write PHENOLOGY (MEASURED)' 
                OPEN (UNIT=FNUMPHEM,FILE=FNAMEPHENOLM,STATUS ='UNKNOWN')
                CLOSE (UNIT=FNUMPHEM, STATUS = 'DELETE')
            ELSE
                INQUIRE (FILE = FNAMEPHENOLM,EXIST = FFLAG)
                OPEN(UNIT=FNUMPHEM,FILE=FNAMEPHENOLM,POSITION='APPEND')
                IF (CROP.NE.CROPPREV.OR.RUN.EQ.1.OR.(.NOT.(FFLAG))) THEN
                    WRITE (FNUMPHEM,'(/,A14,A10)')'*PHENOLOGY(M):',EXCODE
                    WRITE (FNUMPHEM,'(A16,A24)',ADVANCE='NO')'@EXCODE     TRNO',' PYEAR  PDAT  GDAP  EDAP'
                    DO L = 1,KEYSTX
                        IF (KEYPS(L).GT.0) THEN
                            WRITE (FNUMPHEM,'(A6)',ADVANCE='NO')PSABVO(KEYPS(L))
                        ENDIF 
                    ENDDO
                    WRITE (FNUMPHEM,'(/,A10,I6,2I6,2F6.1)',ADVANCE='NO')EXCODE,TN,PLYEAR,PLDAY,gdapfr,edapfr
                ELSE ! End Phenology measured header writes
                    WRITE (FNUMPHEM,'(A10,I6,2I6,2F6.1)',ADVANCE='NO')EXCODE,TN,PLYEAR,PLDAY,gdapfr,edapfr
                ENDIF
                DO L = 1,KEYSTX
                    IF (KEYPS(L).GT.0) WRITE (FNUMPHEM,'(I6)',ADVANCE='NO')PSDAPM(KEYPS(L))
                ENDDO
                CLOSE (FNUMPHEM)
            ENDIF  
            ! End Phenology (Measured)
                    
            ! Plant responses (Simulated)'
            ! Set temporary planting date for overlapping year end
            IF (RUNCRP.EQ.1) PLDAYTMP = -99
            IF (PLDAY.LT.PLDAYTMP) THEN
                IF (VARNO.EQ.VARNOPREV) THEN
                    PLDAYTMP = PLDAY + 365
                ELSE
                    PLDAYTMP = PLDAY
                ENDIF
            ELSE
                PLDAYTMP = PLDAY
            ENDIF
            PLDAYTMP = PLDAY
            IF (EXCODE.NE.EXCODEPREV.OR.TNAME(1:1).EQ.'*') THEN
                OPEN (UNIT=FNUMPRES,FILE=FNAMEPRES,POSITION='APPEND')
                WRITE (FNUMPRES,*) ' '
                WRITE (TLINETMP,'(A,A10,A8)') '*RESPONSES(S):',EXCODE,MODNAME
                IF (TNAME(1:1).EQ.'*') THEN
                    WRITE (FNUMPRES,'(A180)') TLINETMP
                ELSE
                    WRITE (FNUMPRES,'(A180)') TLINETMP
                ENDIF
                WRITE (FNUMPRES,'(4A)',ADVANCE='NO')'@  RUN',' EXCODE   ',' TRNO RN    CR','  PDAT  EDAP'
                DO L = 1,KEYSTX
                    IF (KEYPS(L).GT.0) THEN
                        WRITE (FNUMPRES,'(A6)',ADVANCE='NO') PSABVO(KEYPS(L))
                    ENDIF  
                ENDDO
                WRITE (FNUMPRES,'(8A)') '  HWAH  HWUH','  H#AH  H#GH  LAIX  L#SH BR#AH','  CWAH  VWAH  HIAH  RWAH', &
                    '  HN%H  TNAH','  CNAH  HNAH','  HINH PLPOP','  NICH',' SRADA TMAXA TMINA  PRCP'
            ELSE
                OPEN (UNIT=FNUMPRES,FILE=FNAMEPRES,POSITION='APPEND')
            ENDIF  ! End Responses simulated header writes
            WRITE (FNUMPRES,'(I6,1X,A10,I4,I3,4X,A2, I6, F6.1)',ADVANCE='NO') RUN,EXCODE,TN,RN,CROP,PLDAYTMP,EDAPFR
            DO L = 1,KEYSTX
                IF (L.EQ.MSTG.AND.HNUMBER.EQ.1) THEN
                    ! If harvested at a specific date
                    tvi1 = Dapcalc(yeardoyharf,plyear,plday)
                    WRITE (FNUMPRES,'(I6)',ADVANCE='NO') tvi1
                ELSE
                    IF (KEYPS(L).GT.0) WRITE(FNUMPRES,'(I6)',ADVANCE='NO')PSDAP(KEYPS(L))
                ENDIF
            ENDDO
            WRITE (fnumpres, FMT409)NINT(hwam),hwumchar, NINT(hnumam),NINT(hnumgm),laixchar,lnumsm,brnumsh,NINT(cwam), &
                NINT(vwam),hiamchar,NINT(rwam),hnpcmchar,NINT(AMAX1(-99.0,cnam+rnam)),NINT(cnam),NINT(hnam),hinmchar, &
                pltpop,NINT(amtnit),sradcav,tmaxcav,tmincav,NINT(raincc)
            CLOSE(FNUMPRES)
            ! End Responses simulated writes
                    
            ! Plant responses (Measured)
            IF (TDATANUM.LE.0 .AND. .NOT.FEXISTA) THEN
                WRITE (fnumwrk,*)' '
                WRITE (fnumwrk,*)' No data so cannot write PLANT RESPONSES (MEASURED)'
                OPEN (UNIT = FNUMTMP,FILE = FNAMEPREM,STATUS='UNKNOWN')
                CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
            ELSE
                IF (EXCODE.NE.EXCODEPREV.OR.TNAME(1:1).EQ.'*') THEN
                    OPEN (UNIT=FNUMPREM,FILE=FNAMEPREM,POSITION='APPEND')
                    WRITE (FNUMPREM,*) ' '
                    WRITE (TLINETMP, FMT99511) EXCODE,MODNAME
                    IF (TNAME(1:1).EQ.'*') THEN
                        WRITE (FNUMPREM,'(A180)') TLINETMP
                    ELSE
                        WRITE (FNUMPREM,'(A180)') TLINETMP
                    ENDIF
                    WRITE (FNUMPREM,'(4A)',ADVANCE='NO')'@  RUN',' EXCODE   ',' TRNO RN    CR','  PDOY  EDAP'

                    DO L = 1,KEYSTX
                        IF (KEYPS(L).GT.0) THEN
                            WRITE (FNUMPREM,'(A6)',ADVANCE='NO') PSABVO(KEYPS(L))
                        ENDIF 
                    ENDDO
                    WRITE (FNUMPREM, FMT298)
                ELSE
                    OPEN (UNIT=FNUMPREM,FILE=FNAMEPREM,POSITION='APPEND')
                ENDIF ! End Responses measured header writes
                WRITE (FNUMPREM,'(I6,1X,A10,I4,I3,4X,A2, I6, F6.1)',ADVANCE='NO') &
                    RUN,EXCODE,TN,RN,CROP,PLDAYTMP,FLOAT(MAX(-99,edapm))
                DO L = 1,KEYSTX
                    IF (L.EQ.MSTG.AND.HNUMBER.EQ.1) THEN
                        ! If harvested at a specific date
                        tvi1 = Dapcalc(yeardoyharf,plyear,plday)
                        WRITE (FNUMPREM,'(I6)',ADVANCE='NO') tvi1
                    ELSE
                        IF (KEYPS(L).GT.0) WRITE(FNUMPREM,'(I6)',ADVANCE='NO')PSDAPM(KEYPS(L))
                    ENDIF
                ENDDO
                !WRITE (FNUMPREM,'(I6)',ADVANCE='NO') PSDAPM(HSTG)
                WRITE (FNUMPREM, FMT409) NINT(hwamm),hwummchar,NINT(hnumamm),NINT(hnumgmm),laixmchar,lnumsmm,brnumshm, &
                    NINT(cwamm),NINT(vwamm),hiammchar,NINT(rwamm),hnpcmmchar,NINT(tnamm),NINT(cnamm),NINT(hnamm), &
                    hinmmchar,pltpopp,NINT(amtnit),sradcav,tmaxcav,tmincav,NINT(raincc)
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

    END SUBROUTINE CS_Out_WrkPhenRes 
        
