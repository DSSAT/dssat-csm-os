!***************************************************************************************************************************
! This is the code from the section (DYNAMIC == INTEGR) lines 8517 - 8963 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine YCA_Out_WrkPhenRes outputs work details, phenology and plant responses (IDETL = D). 
!***************************************************************************************************************************
    
    SUBROUTINE YCA_Out_WrkPhenRes ( & 
        IDETL       , IDETO       , ISWNIT      , RN          , RUN         , TN          &
        )

! 2023-01-25 chp removed unused variables
!       DYNAMIC     , NLAYR       , RLV         , 

        USE ModuleDefs
        USE YCA_First_Trans_m
        USE YCA_Formats_m
        USE YCA_Control_Plant
     
        IMPLICIT NONE 
        EXTERNAL WARNING, CSUCASE, TVILENT, DAPCALC
     
        INTEGER :: RN          , RUN         , TN          ! DYNAMIC     , NLAYR       , 
        INTEGER :: DAPCALC     , TVILENT                                                      ! Integer function calls

!       REAL    :: RLV(NL)      

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
                    
            TVR1 = (SEEDRSI+SDCOAT+CARBOC)*plantPopulation()
            TVR2 = RESPC*plantPopulation()
            TVR3 = (SENTOPLITTER+SENROOT)*plantPopulation()
            TVR4 = (SEEDRS+SDCOAT+RTWT+SRWT+canopyWeight())*plantPopulation()
            TVR5 = RSWT*plantPopulation()
            TVR6 = (LWPHC+SWPHC+RSWPHC)*plantPopulation()
                    
            DO L = 2,PSNUM
                CALL CSUCASE (PSNAME(L))
                !IF (PSNAME(L)(1:3) == 'HAR'.AND.PSDAPFR(l) <= 0.0) psdapfr(l) = psdapfr(mstg) !LPM  07MAR15 MSTG TO PSX
                !IF (PSNAME(L)(1:3) == 'END'.AND.PSDAPFR(l) <= 0.0) psdapfr(l) = psdapfr(mstg)
                IF (PSNAME(L)(1:3) == 'HAR'.AND.PSDAPFR(l) <= 0.0) psdapfr(l) = psdapfr(PSX)
                IF (PSNAME(L)(1:3) == 'END'.AND.PSDAPFR(l) <= 0.0) psdapfr(l) = psdapfr(PSX)
                IF (TVILENT(PSNAME(L)) < 5) EXIT
            ENDDO
            !DO L=1,MSTG-2                  !LPM  07MAR15 MSTG TO PSX
            IF (ISWNIT /= 'N') THEN
                TVR1 = (NUPC+SEEDNI)*plantPopulation()  
                TVR2 = (SENNL(0)+SENNS)*plantPopulation() 
                TVR3 = (ROOTN+SROOTN+LEAFN+STEMN+RSN+SEEDN)*plantPopulation()         
                TVR4 = (LNPHC+SNPHC+RSNPHC)* plantPopulation()
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
                    IF (L == PSX.AND.HNUMBER == 1) THEN
                        WRITE (FNUMPRES,'(A6)',ADVANCE='NO') ' HDAT'
                    ELSE
                        IF (KEYPS(L) > 0) THEN
                            WRITE (FNUMPRES,'(A6)',ADVANCE='NO') PSABVO(KEYPS(L))
                        ENDIF
                    ENDIF
                ENDDO
                WRITE (FNUMPRES,'(10A)') '  HWAH  HWUH','  H#AH  H#GH  LAIX  L#SH ', &
                    'BR0AH BR1AH BR2AH BR3AH BR4AH BR5AH BR6AH BR7AH BR8AH BR9AH B10AH B11AH B12AH ', &
                    'B13AH B14AH B15AH B16AH B17AH ', &
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
            WRITE (fnumpres, FMT409)NINT(hwam),hwumchar, NINT(hnumam),NINT(hnumgm),laixchar,lnumsm,BRNUMST,NINT(cwam), &
                NINT(vwam),hiamchar,NINT(rwam),hnpcmchar,NINT(AMAX1(-99.0,cnam+rnam)),NINT(cnam),NINT(hnam),hinmchar, &
                pltpop,NINT(amtnit),sradcav,tmaxcav,tmincav,NINT(raincc)
            CLOSE(FNUMPRES)
            ! End Responses simulated writes
                    
            ! Plant responses (Measured)
            IF (TDATANUM <= 0 .AND. .NOT.FEXISTA) THEN
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
        
