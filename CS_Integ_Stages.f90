!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.INTEGR) lines 5925 - 6147 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module CS_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine CS_Integ_Stages updates stages, states and dates, calculate branch interval and crop cycle conditions. 
!***************************************************************************************************************************
    
    SUBROUTINE CS_Integ_Stages ( &
        BRSTAGE     , CO2         , DAYL        , DOY         , EP          , ET          , NFP         , TMAX        , &
        TMIN        , RAIN        , SRAD        , STGYEARDOY   & 
        )
        
        USE CS_First_Trans_m
        
        IMPLICIT NONE
        
        INTEGER DOY         , STGYEARDOY(0:19) 
        REAL    BRSTAGE     , CO2         , DAYL        , EP          , ET          , NFP         , RAIN        , SRAD          
        REAL    TMAX        , TMIN        
       
        !-----------------------------------------------------------------------
        !         Update stages
        !-----------------------------------------------------------------------
        
        ! STAGES:Germination and emergence (Gestages)
        ! NB 0.5 factor used to equate to Zadoks)
        GEUCUM = GEUCUM + TTGEM                                                                                   !EQN 038
        DAGERM = DAGERM + TTGEM*WFGE                                                           !LPM 21MAR2016 DA for germination
        !IF (GEUCUM.LT.PEGD) THEN
            !GESTAGE = AMIN1(1.0,GEUCUM/PEGD*0.5)                                                       !EQN 039a !LPM 21MAR2016 To separate germination and emergence
        IF (DAGERM.LT.PGERM) THEN
            GESTAGE = AMIN1(1.0,DAGERM/PGERM)  
        ELSE
            !IF (PECM*SDEPTHU > 1.E-6) THEN                                    !LPM 21MAR2016 To separate germination and emergence
            !    GESTAGE = AMIN1(1.0,0.5+0.5*(GEUCUM-PEGD)/(PECM*SDEPTHU))                                              !EQN 039b
            !ELSE
            GESTAGE = 1.0                                                                                          !EQN 039c
            !ENDIF    
        ENDIF
        
        ! Germination conditions  
        IF (GESTAGEPREV.LT.1.0) THEN                               !LPM 23MAR2016 To separate germination and emergence
            TMEANGC = TMEANGC + TMEAN
            GEDAYSG = GEDAYSG + 1
            TMEANG = TMEANGC/GEDAYSG                                                                                   !EQN 040
        ENDIF  
        
        ! Germination to emergence conditions  
        !IF (GESTAGE.LT.0.5) THEN                      !LPM 21MAR2016 To separate germination and emergence
        IF (GESTAGE.LT.1.0) THEN
            RETURN !GOTO 6666                                                             ! MF 27AU14 This is not a very nice construction! See note in Integrate_Subroutines.docx. Change to RETuRN
        ELSEIF (GESTAGEPREV.LT.1.0) THEN                                                  
            TMEANEC = TMEANEC + TMEAN                                                                                  !EQN 041
            GEDAYSE = GEDAYSE + 1
            TMEANE = TMEANEC/GEDAYSE
        ENDIF
        
        ! STAGES:Overall development
        CUMDU = CUMDU + DU
        DABR = DABR + (DU*WFG)
        ! BRANCH NUMBER     !LPM 07MAR15 This section was moved from CS_Growth_Part (has to be before of the estimation of brstage)       
        ! Old method (1 fork number throughout)
        ! BRNUMST = AMAX1(1.0,BRNUMFX**(INT(brstage)-1))
        ! New method (fork number specified for each forking point)
        ! First calculate new BRSTAGE as temporary variable
        ! (LAH Check whether can move brstage calc up here! 
        ! (If do this, brstage in brfx below must be reduced by 1))
        IF (MEDEV.EQ.'LNUM') THEN 
            IF (PDL(INT(BRSTAGE)).GE.0.0) THEN                                                          ! MSTG = KEYPSNUM
                TVR1 = FLOAT(INT(BRSTAGE)) + (LNUM-LNUMTOSTG(INT(BRSTAGE)))/PDL(INT(BRSTAGE)+1)           ! EQN 004
            ELSE
                TVR1 = FLOAT(INT(BRSTAGE))
            ENDIF
        ELSE
            IF (PD(INT(BRSTAGE)).GE.0.0) THEN                                                          ! MSTG = KEYPSNUM
                TVR1 = FLOAT(INT(BRSTAGE)) + (CUMDU-PSTART(INT(BRSTAGE)))/PD(INT(BRSTAGE)+1)              ! EQN 004
            ELSE
                TVR1 = FLOAT(INT(BRSTAGE))
            ENDIF        
        ENDIF    
        
        IF (INT(TVR1) > INT(BRSTAGEPREV)) THEN
            !IF (BRSTAGE.EQ.0.0) THEN
            !    BRNUMST = 1                                                                         ! BRNUMST          ! Branch number/shoot (>forking) # (Actually the total number of apices)! to have the apex number by branch level
            !ELSEIF (BRSTAGE.GT.0.0) THEN
            !    BRNUMST = BRNUMST*BRFX(INT(BRSTAGE))                                                ! BRFX(PSX)        ! EQN 005 ! # of branches at each fork # (This is where new branch is initiated)
            !ENDIF
            BRDAE(TVR1) = DAE
            IF (BRSTAGE.EQ.0.0) THEN
                BRNUMST(TVR1) = 1                                                                                    ! BRNUMST          ! Branch number/shoot (>forking) # (Actually the total number of apices)
            ELSEIF (BRSTAGE.GT.0.0) THEN
                IF (BRFX(INT(TVR1)).LE.0.0) BRFX(INT(TVR1)) = BRFX(INT(TVR1-1))                                 !LPM 09JUN2015 To avoid number of branches 0 for BRSTAGE>6
                BRNUMST(TVR1) = BRNUMST(BRSTAGE)*BRFX(INT(TVR1))                                                ! BRFX(PSX)        ! EQN 005 ! # of branches at each fork # (This is where new branch is initiated)
            ENDIF
        ENDIF 
        
        !LPM 05JUN2015 DSTAGE is not used
        !IF (MEDEV.EQ.'DEVU'.AND.PSTART(MSTG).GT.0.0) THEN   !LPM 04MAR15 MSTG TO PSX
        !IF (MEDEV.EQ.'DEVU'.AND.PSTART(PSX).GT.0.0) THEN                                 ! MEDEV is hard coded in CS_RunInit.f90(53) CHARACTER (LEN=4)  :: MEDEV         ! Switch,development control
        !    ! Calculate dstage from developmental unit accumulation 
        !    !IF (PSTART(MSTG).GT.0.0) DSTAGE = CUMDU/PSTART(MSTG) !LPM 04MAR15 MSTG TO PSX
        !    IF (PSTART(PSX).GT.0.0) DSTAGE = CUMDU/PSTART(PSX)
        !ELSE
        !    ! Alternative method.Calculate dstage from leaf number
        !    !IF (LNUMTOSTG(MSTG).GT.0.0) DSTAGE = LNUM/LNUMTOSTG(MSTG)                                                  !EQN 037b !LPM 04MAR15 MSTG TO PSX
        !    IF (LNUMTOSTG(PSX).GT.0.0) DSTAGE = LNUM/LNUMTOSTG(PSX)                                                  !EQN 037b
        !    IF (LAFND.GT.0.0) DSTAGE = LNUM/LAFND                                                                      !EQN 037a
        !ENDIF 
        
        ! STAGES:Branching
        !IF (GESTAGE.GE.0.5) THEN !LPM 21MAR2016 To separate germination and emergence
        IF (GESTAGE >= 1.0) THEN
            IF (MEDEV == 'DEVU') THEN                                                     ! MEDEV is hard coded in CS_RunInit.f90(53) CHARACTER (LEN=4)  :: MEDEV         ! Switch,development control
                !DO L = HSTG,1,-1                                                          !LPM 03MAR15 It should be as MEDEV = LNUM DO L = HSTG,0,-1
                DO L = HSTG-1,0,-1  
                    !IF (CUMDU.GE.PSTART(L).AND.PD(L+1).GT.0.0) THEN !LPM24APR2016 Using DABR instead of CUMDU
                    !    BRSTAGE = FLOAT(L) + (CUMDU-PSTART(L))/PD(L+1)
                    IF (DABR >= PSTART(L) .AND. PD(L+1) > 0.0) THEN
                        BRSTAGE = FLOAT(L) + (DABR-PSTART(L))/PD(L+1)
                        ! Brstage cannot go above harvest stage 
                        BRSTAGE = AMIN1(FLOAT(HSTG),BRSTAGE)
                        LNUMSIMTOSTG(L+1) = LNUM  ! To record simulated # 
                        LNUMSIMSTG(L) = LNUMSIMTOSTG(L+1)-LNUMSIMTOSTG(L)                 !LPM 21MAR15 LNUMSIMSTG Introduces to save the number of leaves by cohort
                        EXIT
                    ENDIF
                ENDDO
            ELSEIF (MEDEV == 'LNUM') THEN 
                ! Alternative method based on leaf numbers 
                DO L = HSTG-1,0,-1
                    IF (LNUM >= LNUMTOSTG(L)) THEN
                        IF (PDL(L) > 0) THEN 
                            BRSTAGE = FLOAT(L) + (LNUM-LNUMTOSTG(L))/PDL(L)                               ! EQN 007
                        ENDIF
                        LNUMSIMTOSTG(L+1) = LNUM  ! To record simulated # 
                        ! Brstage cannot go above harvest stage 
                        !LPM 21MAR15 LNUMSIMSTG Introduces to save the number of leaves by cohort, necessary to check if has to be +1 as LNUMSG 
                        LNUMSIMSTG(L) = LNUMSIMTOSTG(L+1)-LNUMSIMTOSTG(L)                 
                        BRSTAGE = AMIN1(FLOAT(HSTG),BRSTAGE)
                        EXIT
                    ENDIF  
                ENDDO
            ENDIF  
        ENDIF
        
        ! STAGES:leaf numbers 
        LNUM = AMAX1(0.0,(AMIN1(FLOAT(LNUMX-1),(LNUM+LNUMG))))                                                         !EQN 348
        LNUMSG = INT(LNUM)+1  ! Youngest growing leaf
        
        !-----------------------------------------------------------------------
        !         Record stage dates and states
        !-----------------------------------------------------------------------
        
        !IF (INT(BRSTAGE).GT.10 .OR. INT(BRSTAGE).LT.0.AND.GESTAGE.GT.0.5) THEN !LPM 21MAR2016 To separate germination and emergence
        IF (INT(BRSTAGE).GT.10 .OR. INT(BRSTAGE).LT.0.AND.GESTAGE.GT.1.0) THEN
            OPEN (UNIT = FNUMERR,FILE = 'ERROR.OUT')
            WRITE(fnumerr,*) ' '
            WRITE(fnumerr,*) 'Brstage out of range allowed for branching        '
            WRITE(fnumerr,*) 'Brstage was: ',brstage
            WRITE(fnumerr,*) 'Please contact model developer'
            WRITE(*,*) ' Brstage out of range allowed       '
            WRITE(*,*) ' Brstage was: ',brstage
            WRITE(*,*) ' Program will have to stop'
            PAUSE
            CLOSE (fnumerr)
            STOP ' '
        ENDIF
        ! NB. Status at start of branch tier
        IF (INT(BRSTAGE).GT.INT(BRSTAGEPREV).AND.STGYEARDOY(INT(BRSTAGE)).GE.9999999) THEN
            STGYEARDOY(INT(BRSTAGE)) = YEARDOY
            LAISTG(INT(BRSTAGE)) = LAIPREV 
            LNUMSTG(INT(BRSTAGE)) = LNUMPREV
            ! CWAD and CNAD not include the seed
            CWADSTG(INT(BRSTAGE)) = CWADPREV
            CNADSTG(INT(BRSTAGE)) = CNADPREV
        ENDIF
        
        ! Primary stages.   Calculated using Pstart
        IF (BRSTAGEPREV.LT.0.0) BRSTAGEPREV = 0.0
        L = INT(BRSTAGEPREV) + 1
        !IF (PSDAT(L).LE.0.0.AND.CUMDU.GE.PSTART(L)) THEN !LPM 24APR2016 Using DABR instead of CUMDU
        IF (PSDAT(L).LE.0.0.AND.DABR.GE.PSTART(L)) THEN
            PSDAT(L) = YEARDOY
            !IF (DU.GT.0.0) PSDAPFR(L)=(PSTART(L)-(CUMDU-DU))/DU !LPM 24APR2016 Using DABR instead of CUMDU
            IF (DU.GT.0.0) PSDAPFR(L)=(PSTART(L)-(DABR-DU))/DU
            PSDAPFR(L) = FLOAT(DAP) + PSDAPFR(L)
            PSDAP(L) = DAP
            !IF (PSABV(L).EQ.'MDAT '.OR.L.EQ.MSTG) THEN  !LPM 06MAR15 MSTG TO PSX
            IF (PSABV(L).EQ.'MDAT '.OR.L.EQ.PSX) THEN
                MDAT = YEARDOY
                MDOY = DOY
                MDAP = DAP
                MDAYFR = TIMENEED
                MDAPFR = FLOAT(MDAP) + MDAYFR
            ENDIF
        ENDIF
        
        IF (GYEARDOY.LE.0.0.AND.GERMFR.GT.0.0) THEN
            GYEARDOY = PLYEARDOY
            GDAP = DAP
            GDAYFR = 1.0 - GERMFR
            GDAPFR = FLOAT(DAP) + GDAYFR
        ENDIF
        
        IF (EYEARDOY.LE.0.0.AND.EMRGFR.GT.0.0) THEN
            EYEARDOY = YEARDOY
            EDAP = DAP
            EDAYFR = 1.0 - EMRGFR
            EDAPFR = FLOAT(DAP) + EDAYFR
            DAE = 0
        ENDIF
        
        IF (SHDAT.LE.0.0.AND.SHNUM.GT.1.0) THEN
            SHDAT = YEARDOY
            SHDAP = FLOAT(DAP)
        ENDIF
        
        !-----------------------------------------------------------------------
        !         Calculate branch interval and crop cycle conditions
        !-----------------------------------------------------------------------
        
        IF (WFG.LT.0.99999) WSDAYS = WSDAYS + 1
        IF (NFG.LT.0.99999) NSDAYS = NSDAYS + 1
        
        !IF (GESTAGE.GT.0.1) THEN !LPM 21MAR2016 Branching after emergence
        IF (GESTAGE.GT.1.0) THEN
            IF (INT(BRSTAGE).NE.INT(BRSTAGEPREV)) THEN
                TMAXPC = 0.0
                TMINPC = 0.0
                TMEANPC = 0.0
                SRADPC = 0.0
                DAYLPC = 0.0
                NFPPC = 0.0
                NFGPC = 0.0
                WFPPC = 0.0
                WFGPC = 0.0
                CO2PC = 0.0
            ENDIF
            TMAXPC = TMAXPC + TMAX
            TMINPC = TMINPC + TMIN
            TMEANPC = TMEANPC + TMEAN
            SRADPC = SRADPC + SRAD
            DAYLPC = DAYLPC + DAYL
            TMAXCC = TMAXCC + TMAX
            TMINCC = TMINCC + TMIN
            TMEANCC = TMEANCC + TMEAN
            SRADCC = SRADCC + SRAD
            CO2CC = CO2CC + CO2
            DAYLCC = DAYLCC + DAYL
            RAINCC = RAINCC + RAIN
            
            RAINPC(INT(BRSTAGE)) = RAINPC(INT(BRSTAGE)) + RAIN
            ETPC(INT(BRSTAGE))   = ETPC(INT(BRSTAGE)) + ET 
            EPPC(INT(BRSTAGE))   = EPPC(INT(BRSTAGE)) + EP 
            
            CO2PC = CO2PC + CO2
            NFPPC = NFPPC + NFP
            NFGPC = NFGPC + NFG
            WFPPC = WFPPC + WFP
            WFGPC = WFGPC + WFG
            NFPCC = NFPCC + NFP
            NFGCC = NFGCC + NFG
            WFPCC = WFPCC + WFP
            WFGCC = WFGCC + WFG
            ETCC   = ETCC + ET
            EPCC   = EPCC + EP 
            
            PDAYS(INT(BRSTAGE)) = PDAYS(INT(BRSTAGE)) + 1
            CDAYS = CDAYS + 1
            IF (PDAYS(INT(BRSTAGE)).GT.0) THEN
                TMAXPAV(INT(BRSTAGE)) = TMAXPC / PDAYS(INT(BRSTAGE))
                TMINPAV(INT(BRSTAGE)) = TMINPC / PDAYS(INT(BRSTAGE))
                TMEANAV(INT(BRSTAGE)) = TMEANPC / PDAYS(INT(BRSTAGE))
                SRADPAV(INT(BRSTAGE)) = SRADPC / PDAYS(INT(BRSTAGE))
                DAYLPAV(INT(BRSTAGE)) = DAYLPC / PDAYS(INT(BRSTAGE))
                DAYLST(INT(BRSTAGE)) = DAYL
                CO2PAV(INT(BRSTAGE)) = CO2PC / PDAYS(INT(BRSTAGE))
                RAINPAV(INT(BRSTAGE)) = RAINPC(INT(BRSTAGE)) / PDAYS(INT(BRSTAGE))
                NFPPAV(INT(BRSTAGE)) = NFPPC / PDAYS(INT(BRSTAGE))
                NFGPAV(INT(BRSTAGE)) = NFGPC / PDAYS(INT(BRSTAGE))
                WFPPAV(INT(BRSTAGE)) = WFPPC / PDAYS(INT(BRSTAGE))
                WFGPAV(INT(BRSTAGE)) = WFGPC / PDAYS(INT(BRSTAGE))
            ENDIF
            IF (CDAYS.GT.0) THEN              
                TMAXCAV = TMAXCC / CDAYS
                TMINCAV = TMINCC / CDAYS 
                SRADCAV = SRADCC / CDAYS
                DAYLCAV = DAYLCC / CDAYS
                CO2CAV = CO2CC / CDAYS
                NFPCAV = NFPCC / CDAYS
                NFGCAV = NFGCC / CDAYS
                WFPCAV = WFPCC / CDAYS
                WFGCAV = WFGCC / CDAYS
            ENDIF
        ENDIF
    
        ! -------------------------------------------------------------------------------------------------------
        !                    Assigning groups to the nodes, each 20 nodes a different group
        ! -------------------------------------------------------------------------------------------------------
        ! DA 13DIC2016 Calculating the group in which the node belongs, knowing that a group is for each 20 nodes
        Lcount = 1.0 ! this will be used as an index of all the nodes
        Bcount = 1.0 ! this will be used to indicate the group number
        NodeGroupSize = 0
        DO BR = 0, BRSTAGE               ! for each branch   
            DO LF = 1, LNUMSIMSTG(BR)    ! and each node of the branches
                
                IF(MOD(NINT(Lcount),20) == 0)THEN    ! first, in each 20 nodes
                    Bcount = Bcount+1           ! increment the number of the group
                ENDIF
                
                IF(NDDAE(Bcount) == 0.0 .AND. NodeGroup(BR,LF) == 0.0) THEN 
                    NDDAE(Bcount) = DAE                                             ! calculate date of new group appereance
                ENDIF

                NodeGroup(BR,LF) = Bcount       ! Assign the group number the node belongs to
                Lcount = Lcount+1               ! incrementing overall count of leaves
                ENDDO
        ENDDO
        NodeGroupSize = Bcount
        ! -------------------------------------------------------------------------------------------------------
        
    END SUBROUTINE CS_Integ_Stages 
        
