!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.INTEGR) lines 8965 - 9278 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in Module_CSCAS_Vars_List. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! SUBROUTINE CS_Out_Error outputs Errora, Errort, Errors (IDETL = A). 
!***************************************************************************************************************************
    
    SUBROUTINE CS_Out_Error ( & 
        IDETL       , RN          , RUN         , TN          &
        )
        
        USE Module_CSCAS_Vars_List
        USE Module_CS_Formats
     
        IMPLICIT NONE 
     
        INTEGER :: RN          , RUN         , TN          
        INTEGER :: TVICOLNM                                                                   ! Integer function calls

        CHARACTER(LEN=1)  :: IDETG       , IDETL       
        
        !-----------------------------------------------------------------------------------------------------------
        !         Errora, Errort, Errors (IDETL = A)
        !-----------------------------------------------------------------------------------------------------------
                
        IF (IDETL.EQ.'A') THEN     ! Write some error outputs
                    
            ! Find intermediate stage dates
            DO L = MSTG-1,1,-1
                IF (psdapm(l).GT.0.0) THEN
                    psidapm = psdapm(l)
                    EXIT
                ENDIF
            ENDDO
            IF (L.EQ.0) L = INT((FLOAT(MSTG)/2.0)+1)
            IF (L.GT.0) THEN
                PSIDAP = PSDAP(L)
            ELSE
                WRITE (fnumwrk,*)' '
                WRITE (fnumwrk,*)' Problem in finding intermediate stage '
                WRITE (fnumwrk,*)'  Mature stage       = ',mstg          
                WRITE (fnumwrk,*)'  Intermediate stage = ',l             
                WRITE (fnumwrk,*)' '
            ENDIF
                    
            ! Errors (A-data)
            IF (TDATANUM.LE.0 .AND. .NOT.FEXISTA) THEN
                WRITE (fnumwrk,*)' '
                WRITE (fnumwrk,*)' No data so cannot write PLANTERA'
                OPEN (UNIT=FNUMTMP,FILE=FNAMEERA,STATUS='UNKNOWN')
                CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
            ELSE ! If data availabe
                IF (edapm.GT.0) THEN
                    emdaterr = 100.0*(Edap-Edapm)/edapm
                ELSE
                    emdaterr = -99
                Endif
                IF (psidapm.GT.0) THEN
                    psidaterr = 100.0*(psidap-psidapm)/psidapm
                ELSE
                    psidaterr = -99
                Endif
                IF (mdatm.GT.0) THEN
                    mdaterr = 100.0*(mdap-mdapm)/mdapm
                ELSE
                    mdaterr = -99
                Endif
                IF (hwahm.GT.0.AND.hwam.GT.0.AND.hpcf.GT.0) THEN
                    hwaherr = 100.*(hwam*hpcf/100.-hwahm)/(hwahm*hpcf/100.)
                    IF (hwaherr.GT.99999.0) hwaherr = 99999.0
                    IF (hwaherr.LT.-9999.0) hwaherr = -9999.0
                ELSE
                    hwaherr = -99
                ENDIF
                IF (hwumm.GT.0.AND.hwum.GT.0) THEN
                    hwumerr = 100.0*(hwum-hwumm)/hwumm
                ELSE
                    hwumerr = -99
                ENDIF
                IF (hnumamm.GT.0.AND.hnumam.GT.0) THEN
                    hnumaerr = 100.0*(hnumam-hnumamm)/(hnumamm)
                ELSE
                    hnumaerr = -99
                ENDIF
                IF (hnumgmm.GT.0.AND.hnumgm.GT.0) THEN
                    hnumgerr = 100.0*((hnumgm-hnumgmm)/hnumgmm)
                ELSE
                    hnumgerr = -99
                ENDIF
                IF (laixm.GT.0.AND.laix.GT.0) THEN
                    laixerr = 100.0*((laix-laixm)/laixm)
                ELSE
                    laixerr = -99
                ENDIF
                IF (lnumsmm.GT.0.AND.lnumsm.GT.0) THEN
                    lnumserr = 100.0*((lnumsm-lnumsmm)/lnumsmm)
                ELSE
                    lnumserr = -99
                ENDIF
                IF (cwamm.GT.0.AND.cwam.GT.0) THEN
                    cwamerr = 100.0*(cwam-cwamm)/cwamm
                ELSE
                    cwamerr = -99
                Endif
                IF (vwamm.GT.0.AND.vwam.GT.0) THEN
                    vwamerr = 100.0*(vwam-vwamm)/vwamm
                ELSE
                    vwamerr = -99
                Endif
                IF (hiamm.GT.0.AND.hiam.GT.0) THEN
                    hiamerr = 100.0*(hiam-hiamm)/hiamm
                ELSE
                    hiamerr = -99
                Endif
                IF (hnpcmm.GT.0.AND.hnpcm.GT.0) THEN
                    hnpcmerr = 100.0*(hnpcm-hnpcmm)/hnpcmm
                ELSE
                    hnpcmerr = -99
                Endif
                IF (cnamm.GT.0.AND.cnam.GT.0) THEN
                    cnamerr = 100.0*(cnam-cnamm)/cnamm
                ELSE
                    cnamerr = -99
                Endif
                IF (RUN.EQ.1) THEN
                    OPEN (UNIT=FNUMERA,FILE=FNAMEERA,POSITION='APPEND')
                    WRITE (FNUMERA, FMT996)
                    WRITE (FNUMERA, FMT896)
                            
                    CLOSE(FNUMERA)
                ENDIF  ! End ErrorA header writes
                OPEN (UNIT = FNUMERA,FILE = FNAMEERA,POSITION = 'APPEND')
                WRITE (FNUMERA, FMT8401) RUN,EXCODE,TN,RN,CROP,Edap,emdaterr,mdap,mdaterr,NINT(hwam),NINT(hwaherr),hwum, &
                    NINT(hwumerr),NINT(hnumam),NINT(hnumaerr),hnumgm,NINT(hnumgerr),laix,NINT(laixerr),lnumsm, &
                    NINT(lnumserr),NINT(cwam),NINT(cwamerr),NINT(vwam),NINT(vwamerr),hiam,NINT(hiamerr),hnpcm, &
                    NINT(hnpcmerr),NINT(cnam),NINT(cnamerr),NINT(hnam),NINT(hnamerr)
                    CLOSE(FNUMERA)
            ENDIF ! End ErrorA writes (If data available)
                    
            ! Errors (T)
            IF (.NOT.FEXISTT .OR. FROPADJ.GT.1 .OR. IDETG.EQ.'N') THEN
                WRITE (fnumwrk,*) ' '
                IF (FROPADJ.GT.1) THEN
                    WRITE (fnumwrk,*) ' Cannot write PLANT ERRORS (T).',' Frequency of output > 1 day'
                ELSE  
                    WRITE (fnumwrk,*)' No data so cannot write PLANT ERRORS (T)'
                ENDIF      
                OPEN (UNIT=FNUMTMP,FILE=FNAMEERT,STATUS='UNKNOWN')
                CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
            ELSE
                INQUIRE (FILE = 'PlantGro.OUT',OPENED = FOPEN)
                IF (FOPEN) CLOSE (NOUTPG)
                STARNUM = 0
                OPEN (UNIT=FNUMT,FILE='Measured.out',STATUS='UNKNOWN')
                DO WHILE (TLINET(1:1).NE.'@')
                    TLINET = ' '
                    READ (FNUMT, FMT1502, END=1600, ERR=1600) TLINET
                    IF (TLINET(1:1).EQ.'*') STARNUM = STARNUM + 1
                    IF (TLINET(1:1).EQ.'@') THEN
                        IF (STARNUM.NE.STARNUMM) THEN
                            TLINET = ' '
                            READ (FNUMT, FMT1502, END=1600, ERR=1600) TLINET
                        ENDIF
                    ENDIF
                ENDDO
                tlinet(1:1) = ' '
                STARNUM = 0
                OPEN (UNIT=NOUTPG,FILE='PlantGro.OUT',STATUS='UNKNOWN')
                DO WHILE (TLINEGRO(1:1).NE.'@')
                    TLINEGRO = ' '
                    READ (NOUTPG,'(A254)') TLINEGRO
                    IF (TLINEGRO(1:4).EQ.'*RUN') STARNUM = STARNUM + 1
                    IF (TLINEGRO(1:1).EQ.'@') THEN
                        IF (STARNUM.NE.STARNUMO) THEN
                            TLINEGRO = ' '
                            READ (NOUTPG,'(A254)') TLINEGRO
                        ENDIF
                    ENDIF
                ENDDO
                tlinegro(1:1) = ' '
                ! Find headers from Measured file
                DO L = 1,20
                    CALL Getstr(tlinet,l,thead(l))
                    IF (THEAD(L)(1:3).EQ.'-99') EXIT
                    IF (THEAD(L)(1:3).EQ.'DAP') tfdapcol = l
                ENDDO
                TFCOLNUM = L-1
                IF (TFCOLNUM.LE.0) THEN
                    WRITE (FNUMWRK,*) 'No columns found in T-file '
                    GO TO 7777
                ENDIF
                ! Make new header line
                TLINETMP = ' '
                TLINETMP(1:1) = '@'
                DO L = 1, TFCOLNUM
                    TLPOS = (L-1)*6+1
                    IF (THEAD(L).EQ.'TRNO'.OR.THEAD(L).EQ.'YEAR'.OR.THEAD(L).EQ.'DATE') THEN
                        TLINETMP(TLPOS+2:TLPOS+5)=THEAD(L)(1:4)
                    ELSEIF(THEAD(L).EQ.'DOY'.OR.THEAD(L).EQ.'DAP' .OR.THEAD(L).EQ.'DAS'.OR.THEAD(L).EQ.'DAY') THEN
                        TLINETMP(TLPOS+3:TLPOS+5)=THEAD(L)(1:3)
                    ELSE
                        WRITE (TCHAR,'(I6)') NINT(ERRORVAL*100.0)
                        TLINETMP(TLPOS+1:TLPOS+4) = THEAD(L)(1:4)
                        TLINETMP(TLPOS+5:TLPOS+5) = 'E'
                    ENDIF
                ENDDO
                ! Find corresponding columns in PlantGro.OUT
                DO L = 1,TFCOLNUM
                    pgrocol(l) = Tvicolnm(tlinegro,thead(l))
                ENDDO
                OPEN (UNIT=FNUMERT,FILE=FNAMEERT,POSITION='APPEND')
                WRITE (FNUMERT, FMT2996) OUTHED(12:79)
                tlinet(1:1) = '@'
                WRITE (FNUMERT,'(A180)') TLINETMP
                ! Read data lines, match dates, calculate errors, write
                DO L1 = 1,200
                    TLINET = ' '
                    READ (FNUMT, FMT7778,ERR=7777,END=7777) TLINET
                    IF (TLINET(1:1).EQ.'*') EXIT
                    IF (TLINET(1:6).EQ.'      ') GO TO 7776
                    CALL Getstri(tlinet,tfdapcol,tfdap)
                    IF (TFDAP.LE.0.0) THEN
                        EXIT
                    ENDIF
                    DO WHILE (tfdap.NE.pgdap)
                        TLINEGRO = ' '
                        READ (NOUTPG, FMT7779, ERR=7777, END=7777) TLINEGRO
                        CALL Getstri(tlinegro,pgrocol(tfdapcol),pgdap)
                        IF (PGDAP.LT.0) THEN
                            WRITE (FNUMWRK,*) 'DAP in Plantgro file < 0 '
                            EXIT
                        ENDIF
                    ENDDO
                    TLINETMP = ' '
                    DO L = 1, TFCOLNUM
                        CALL Getstrr(tlinet,l,tfval)
                        CALL Getstrr(tlinegro,pgrocol(l),pgval)
                        ERRORVAL = 0.0
                        IF(TFVAL.GT.0.0.AND.PGVAL.GT.-99.AND.PGVAL.NE.0.0)THEN
                            ERRORVAL = 100.0 * (PGVAL - TFVAL) / TFVAL
                        ELSE
                            ERRORVAL = -99.0
                        ENDIF
                        IF (THEAD(L).EQ.'TRNO'.OR.THEAD(L).EQ.'YEAR' .OR.THEAD(L).EQ.'DOY'.OR.THEAD(L).EQ.'DAP' .OR. &
                            THEAD(L).EQ.'DAY' .OR.THEAD(L).EQ.'DAS'.OR.THEAD(L).EQ.'DATE') THEN
                            CALL Getstri(tlinet,l,tvi1)
                            WRITE (TCHAR,'(I6)') TVI1
                        ELSE
                            WRITE (TCHAR,'(I6)') NINT(ERRORVAL)
                        ENDIF
                        TLPOS = (L-1)*6+1
                        TLINETMP(TLPOS:TLPOS+5)=TCHAR
                    ENDDO
                    WRITE (FNUMERT,'(A180)') TLINETMP
7776                        CONTINUE
                ENDDO
7777                    CONTINUE
                GO TO 1601
1600                    CONTINUE
                WRITE(fnumwrk,*)'End of file reading Measured.out'
                WRITE(fnumwrk,*)'Starnum and starnumm were: ',starnum,starnumm
1601                    CONTINUE
                CLOSE (FNUMERT)
                CLOSE (FNUMT)
                CLOSE (NOUTPG)
                ! Re-open file if open at start of work here
                IF (FOPEN) OPEN (UNIT=NOUTPG,FILE='PlantGro.OUT',POSITION='APPEND')
            ENDIF  ! .NOT.FEXISTT .OR. FROPADJ.GT.1 .OR. IDETG.EQ.'N'
            ! End of ErrorT writes
                    
        ELSE ! No ERROR files called for ... must be deleted          
                    
            OPEN (UNIT=FNUMTMP,FILE=FNAMEERA,STATUS='UNKNOWN')
            CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
            OPEN (UNIT=FNUMTMP,FILE=FNAMEERT,STATUS='UNKNOWN')
            CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
                    
        ENDIF ! End of Error writes  IDETL.EQ.'A'

    END SUBROUTINE CS_Out_Error   

    
