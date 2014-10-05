!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.INTEGR) lines 5534 - 6649 of the original CSCAS code. the names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement. The variables are described in CSCAS.
!
! This subroutine updates the seasonal data with the data for the current day: dry weights, produced and senesced leaf 
! area, plant height, root and length, and nitrogen. It updates stages, dates and times. It calculates branch interval, 
! N concentrations and whether to harvest. It calculates yields of plant parts and summaries of weather and soil 
! variables and par utilization. 
      
!     TO DO: Divide SUBROUTINE INTEGRATE into several subroutines to account for (at least) growth stages, branch intervals, 
!       plant yields, and soil and weather summaries.
!     
! The routine has 514 dummy arguments comprising 13 character variables, 43 integers, 455 reals and 3 parameters.
!
!***************************************************************************************************************************
    
    
    SUBROUTINE CS_Integrate( &
        ALBEDO      , BD          , BRSTAGE     , CAID        , CANHT       , CO2         , DAYL        , DEPMAX      , &
        DLAYR       , DOY         , DRAIN       , EOP         , EP          , ET          , FERNIT      , IRRAMT      , &
        ISWNIT      , ISWWAT      , LL          , NFP         , NH4LEFT     , NLAYR       , NO3LEFT     , RAIN        , &
        RESCALG     , RESLGALG    , RESNALG     , RLV         , RUNOFF      , SRAD        , STGYEARDOY  , SW          , &
        TLCHD       , TNIMBSOM    , TNOXD       , TOMINFOM    , TOMINSOM    , TOMINSOM1   , &                             ! MF 15SE14 removed <TMAX        , TMIN        > (In Module_CSCas_Vars_List)
        TOMINSOM2   , TOMINSOM3   , WEATHER     , YEAR        & 
        )
        
        USE ModuleDefs
        !USE CRSIMDEF                                                                MF 15SE14 Declared in ModuleDefs
        USE Module_CSCAS_Vars_List
        
        
        TYPE (WeatherType) WEATHER
        
        CHARACTER(LEN=1) ISWNIT      , ISWWAT      
        INTEGER DOY         , NLAYR       , STGYEARDOY(20)            , YEAR        
        REAL    ALBEDO      , BD(NL)      , BRSTAGE     , CAID        , CANHT       , CO2         , DAYL        , DEPMAX      
        REAL    DLAYR(NL)   , DRAIN       , EOP         , EP          , ET          , FERNIT      , IRRAMT      , LL(NL)      
        REAL    NFP         , NH4LEFT(NL) , NO3LEFT(NL) , RAIN        , RESCALG(0:NL)             , RESLGALG(0:NL)            
        REAL    RESNALG(0:NL)             , RLV(NL)     , RUNOFF      , SRAD        , SW(NL)      , TLCHD                    ! MF 15SE14 removed <, TMAX > (In Module_CSCas_Vars_List)        
        REAL    TNIMBSOM    , TNOXD       , TOMINFOM    , TOMINSOM    , TOMINSOM1   , TOMINSOM2   , TOMINSOM3                ! MF 15SE14 removed <TMIN        ,> (In Module_CSCas_Vars_List) 
        
        
        !-----------------------------------------------------------------------
        !         Update ages
        !-----------------------------------------------------------------------
        IF (YEARDOY.GT.PLYEARDOY) THEN
            DAP = DAP + 1
            IF (EYEARDOY.GT.0) DAE = DAE + 1
        ENDIF
        TVI1 = LNUMSOLDESTA
        LNUMSOLDESTA = -99
        ! Leaves that present at beginning of day
        DO L = 1,LNUMSG
            LAGETT(L) = LAGETT(L) + TTLFLIFE*EMRGFR
            ! Accelerated senescence at base of dense leaf canopy
            IF (LAI.GT.LAIXX) THEN
                IF (L.EQ.TVI1) THEN
                    ! Increase age if deep shading at base of canopy
                    ! (Maximum accelerated ageing set in SPE file)
                    ! Accelerated ageing of lowermost active leaf
                    IF (LAGETT(L).LT.LLIFGTT+LLIFATT) THEN
                        LAGETT(L) = AMIN1(LAGETT(L)+LLIFX,LLIFGTT+LLIFATT)
                        LLIFXUNUSED = (LAGETT(L)+LLIFX)-(LLIFGTT+LLIFATT)
                    ELSE
                        LLIFXUNUSED = LLIFX
                    ENDIF
                ENDIF
                ! If not all acceleration used up
                IF (L.EQ.TVI1+1) THEN
                    IF (LLIFXUNUSED.GT.0.0) THEN
                        IF (LAGETT(L).LT.LLIFGTT+LLIFATT) LAGETT(L) = AMIN1(LAGETT(L)+LLIFXUNUSED,LLIFGTT+LLIFATT)
                    ENDIF
                ENDIF
            ENDIF
            LAGEP(L) = LAGEP(L) + (TTLFLIFE*EMRGFR)/PHINT
            ! Days growing
            IF (LAGETT(L).LE.LLIFGTT) THEN
                DGLF(L) = DGLF(L) + EMRGFR
                IF (LAGETT(L)-TTLFLIFE*EMRGFR.LT.LLIFGTT) THEN
                    TVR1 = (LLIFGTT-(LAGETT(L)-TTLFLIFE*EMRGFR))/(TTLFLIFE*EMRGFR)
                    DGLF(L) = DGLF(L) + TVR1
                ENDIF
            ENDIF
            ! Days active
            IF (LAGETT(L).GT.LLIFGTT.AND.LAGETT(L).LT.LLIFGTT+LLIFATT) THEN
                IF (LNUMSOLDESTA.LT.0) THEN
                    LNUMSOLDESTA = L
                ENDIF
                IF (LAGETT(L)-TTLFLIFE*EMRGFR.LT.LLIFGTT) THEN
                    TVR1 = (LLIFGTT-(LAGETT(L)-TTLFLIFE*EMRGFR))/(TTLFLIFE*EMRGFR)
                    DALF(L) = DALF(L) + (1.0-TVR1)
                ELSE
                    IF (LAGETT(L).LE.LLIFGTT+LLIFATT) THEN
                        DALF(L) = DALF(L) + 1.0
                    ELSE
                        IF (LAGETT(L)-TTLFLIFE*EMRGFR.LT.LLIFGTT+LLIFATT) THEN
                            TVR1 = ((LLIFGTT+LLIFATT)-(LAGETT(L)-TTLFLIFE*EMRGFR))/(TTLFLIFE*EMRGFR)
                            DALF(L) = DALF(L) + TVR1
                        ENDIF
                    ENDIF
                ENDIF
            ENDIF
            ! Days senescing
            IF (LAGETT(L).GT.LLIFGTT+LLIFATT) THEN
                IF (LAGETT(L)-TTLFLIFE*EMRGFR.LT.LLIFGTT+LLIFATT) THEN
                    TVR1 = ((LLIFGTT+LLIFATT)-(LAGETT(L)-TTLFLIFE*EMRGFR))/(TTLFLIFE*EMRGFR)
                    DSLF(L) = DSLF(L) + (1.0-TVR1)
                ELSE
                    IF (LAGETT(L).LE.LLIFGTT+LLIFATT+LLIFSTT) THEN
                        DSLF(L) = DSLF(L) + 1.0
                    ELSE
                        IF (LAGETT(L)-TTLFLIFE*EMRGFR.LT.LLIFGTT+LLIFATT+LLIFSTT) THEN
                            TVR1 = ((LLIFGTT+LLIFATT+LLIFSTT)-(LAGETT(L)-TTLFLIFE*EMRGFR))/(TTLFLIFE*EMRGFR)
                            DSLF(L) = DSLF(L) + TVR1
                            LDEATHDAP(L) = DAP
                        ENDIF
                    ENDIF
                ENDIF
            ENDIF
        ENDDO
        ! Leaves that emerged during day
        IF (LNUMG.GT.0.0) THEN
            IF (LNUMSG.LT.LNUMX) THEN
                LAGETT(LNUMSG+1) = LAGETT(LNUMSG+1)+(TTLFLIFE*EMRGFR)*AMAX1(0.0,LNUMG-LNUMNEED)/LNUMG
                LAGEP(LNUMSG+1)=LAGEP(LNUMSG+1)+AMAX1(0.0,LNUMG-LNUMNEED)
                DGLF(LNUMSG+1) = AMAX1(0.0,LNUMG-LNUMNEED)/LNUMG
            ENDIF
        ENDIF
        
        !-----------------------------------------------------------------------
        !         Update dry weights
        !-----------------------------------------------------------------------
        
        ! Dry weights
        ! LAH No growth/development on planting day
        IF (YEARDOY.GE.PLYEARDOY) THEN
            ! Assimilation and respiration
            CARBOC = CARBOC + AMAX1(0.0,(CARBOBEG+CARBOADJ))
            RESPRC = RESPRC + RTRESPADJ
            RESPTC = 0.0  ! Respiration tops - not yet used
            RESPC = RESPRC + RESPTC
            LFWT = LFWT + GROLFADJ - SENLFG - SENLFGRS - LWPH
            LWPHC = LWPHC +  LWPH
            IF (LFWT.LT.-1.0E-8) THEN
                WRITE(Message(1),'(A35,F4.1,A14)') 'Leaf weight less than 0! Weight of ',lfwt,' reset to zero'
                CALL WARNING(1,'CSCAS',MESSAGE)
                LFWT = 0.0
            ENDIF
            RSWT = RSWT+GRORS-RSWPH-SRWTGRS+RSWTGLFADJ
            RSWPHC = RSWPHC +  RSWPH
            ! Reserves distribution 
            ! Max concentration in leaves increases through life cycle.
            IF (PSTART(MSTG).GT.0.0) LLRSWT = AMIN1(RSWT,LFWT*(1.0-LPEFR)*(RSCLX/100.0)*DSTAGE)
            IF (PSTART(MSTG).GT.0.0) LPERSWT = AMIN1(RSWT-LLRSWT,LFWT*LPEFR*(RSCLX/100.0)*DSTAGE)
            IF (STWT+CRWT.GT.0.0) THEN
                STRSWT = (RSWT-LLRSWT-LPERSWT)*STWT/(STWT+CRWT)
                CRRSWT = (RSWT-LLRSWT-LPERSWT)*CRWT/(STWT+CRWT)
            ELSE
                STRSWT = (RSWT-LLRSWT-LPERSWT)
                CRRSWT = 0.0
            ENDIF
            IF (RSWT.LT.0.0) THEN
                IF (ABS(RSWT).GT.1.0E-6) THEN
                    WRITE(Message(1),'(A30,A11,F12.9)') 'Reserves weight reset to zero.', 'Weight was ',rswt
                    CALL WARNING(1,'CSCAS',MESSAGE)
                    RSWT = 0.0
                ENDIF
            ENDIF
            RSWTX = AMAX1(RSWTX,RSWT)
            STWT = STWT + GROSTADJ - SWPH
            IF (STWT.LT.1.0E-06) THEN
                IF (STWT.LT.0.0) WRITE(fnumwrk,*)'Stem weight less than 0! ',STWT
                STWT = 0.0
            ENDIF
            SWPHC = SWPHC +  SWPH
            CRWT = CRWT + GROCRADJ 
            
            SENTOPLITTER = SENTOPLITTER + SENTOPLITTERG
            SENCL(0) = SENCL(0) + SENTOPLITTERG*0.4
            SENLL(0) = SENLL(0) + (SENLFG*LLIGP/100)*(SENFR)
            RTWT = 0.0
            DO L = 1, NLAYR
                RTWTL(L) = RTWTL(L) + RTWTGL(L) - RTWTSL(L) - RTWTUL(L)
                SENWL(L) = SENWL(L) + RTWTSL(L)
                SENCL(L) = SENCL(L) + RTWTSL(L) * 0.4
                SENLL(L) = SENLL(L) + RTWTSL(L) * RLIGP/100.0
                ! Totals
                RTWT = RTWT + RTWTL(L)
                SENROOT = SENROOT + RTWTSL(L)
                SENCS = SENCS + RTWTSL(L) * 0.4
                SENLS = SENLS + RTWTSL(L) * RLIGP/100.0
            END DO
            SRWT = SRWT + GROSR + SRWTGRS + (RTWTG-RTWTGADJ+RTRESP-RTRESPADJ) ! Root N adjustment
        ENDIF
        
        SEEDRS = AMAX1(0.0,SEEDRS-GROLSSD-SEEDRSAVR)
        IF (CFLSDRSMSG.NE.'Y'.AND.SEEDRS.LE.0.0.AND.LNUM.LT.4.0) THEN
            WRITE(Message(1),'(A44,F3.1)') 'Seed reserves all used but leaf number only ',lnum
            WRITE(Message(2),'(A58)') 'For good establishment seed reserves should last to leaf 4'
            WRITE(Message(3),'(A55)') 'Maybe stick too small or specific leaf area set too low'
            CALL WARNING(3,'CSCAS',MESSAGE)
            CFLSDRSMSG = 'Y'
        ENDIF
        SEEDUSE = SEEDUSE + GROLSSD+SEEDRSAVR
        SEEDUSER = SEEDUSER + SEEDRSAVR
        SEEDUSET = SEEDUSET + GROLSSD
        SEEDRSAV = SEEDRS
        IF (SRNOPD.GT.0.0) SRWUD = SRWT/SRNOPD
        
        IF ((LFWT+STWT+CRWT+RSWT).GT.0.0) THEN
            HIAD = SRWT/(LFWT+STWT+CRWT+SRWT+RSWT)
        ENDIF
        IF (RTWT.GT.0.0) SHRTD = (LFWT+STWT+CRWT+RSWT) / RTWT
        
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
        
        !-----------------------------------------------------------------------
        !         Update nitrogen amounts
        !-----------------------------------------------------------------------
        NUPC = NUPC + NUPD
        LEAFNEXCESS = 0.0
        IF (LANC.GT.LNCX) LEAFNEXCESS = (LFWT-SENLFG-SENLFGRS)*(LANC-LNCX)
        LEAFN = LEAFN + GROLSRTN + LNUSE(0) - SENNLFG - SENNLFGRS - lnph - LEAFNEXCESS
        LNPHC = LNPHC +  LNPH
        IF (LEAFN.LT.1.0E-10) LEAFN = 0.0
        STEMNEXCESS = 0.0
        IF (SANC.GT.SNCX) STEMNEXCESS = (STWT+CRWT)*(SANC-SNCX)
        STEMN = STEMN + SNUSE(0) - SNPH - STEMNEXCESS
        SNPHC = SNPHC +  SNPH
        IF (STEMN.LT.1.0E-10) STEMN = 0.0
        ROOTNS = 0.0
        SENNGS = 0.0
        DO L = 1, NLAYR
            SENNL(L) = SENNL(L) + RTNSL(L)
            ROOTNS = ROOTNS + RTNSL(L)
            SENNS = SENNS + RTNSL(L)
            SENNGS = SENNGS + RTNSL(L)
        END DO
        ! LAH Maybe also need LEAFNEXESS if LANC > LNCX
        ROOTNEXCESS = 0.0
        IF (RANC.GT.RNCX) ROOTNEXCESS = (RTWT-(SENWALG(L)/(PLTPOP*10.0)))*(RANC-RNCX)
        ROOTN = ROOTN + (RNUSE(0)-ROOTNS-GROLSRTN) - ROOTNEXCESS
        SROOTN = SROOTN + SRNUSE(0)
        SEEDN = SEEDN - SEEDNUSE - SEEDNUSE2
        IF (SEEDN.LT.1.0E-6) SEEDN = 0.0
        RSN = RSN - RSNUSED + SENNLFGRS - RSNPH + LEAFNEXCESS + STEMNEXCESS + ROOTNEXCESS
        RSNPHC = RSNPHC +  RSNPH
        SENNL(0) = SENNL(0) + SENNLFG
        
        HPRODN = SROOTN
        
        ! Harvest index for N
        HIND = 0.0
        IF ((LEAFN+STEMN+RSN).GT.0.0) HIND = HPRODN/(LEAFN+STEMN+HPRODN+RSN)
        
        !-----------------------------------------------------------------------
        !         Update stages
        !-----------------------------------------------------------------------
        
        ! STAGES:Germination and emergence (Gestages)
        ! NB 0.5 factor used to equate to Zadoks)
        GEUCUM = GEUCUM + TTGEM*WFGE
        IF (GEUCUM.LT.PEGD) THEN
            GESTAGE = AMIN1(1.0,GEUCUM/PEGD*0.5)
        ELSE
            IF (PECM*SDEPTHU > 1.E-6) THEN 
                GESTAGE = AMIN1(1.0,0.5+0.5*(GEUCUM-PEGD)/(PECM*SDEPTHU))
            ELSE
                GESTAGE = 1.0
            ENDIF    
        ENDIF
        
        ! Germination conditions  
        IF (GESTAGEPREV.LT.0.5) THEN
            TMEANGC = TMEANGC + TMEAN
            GEDAYSG = GEDAYSG + 1
            TMEANG = TMEANGC/GEDAYSG
        ENDIF  
        
        ! Germination to emergence conditions  
        IF (GESTAGE.LT.0.5) THEN
            GOTO 6666  ! MF 27AU14 This is not a very nice construction!                
        ELSEIF (GESTAGEPREV.LT.1.0) THEN
            TMEANEC = TMEANEC + TMEAN
            GEDAYSE = GEDAYSE + 1
            TMEANE = TMEANEC/GEDAYSE
        ENDIF
        
        ! STAGES:Overall development
        CUMDU = CUMDU + DU
        IF (MEDEV.EQ.'DEVU'.AND.PSTART(MSTG).GT.0.0) THEN
            ! Calculate dstage from developmental unit accumulation
            IF (PSTART(MSTG).GT.0.0) DSTAGE = CUMDU/PSTART(MSTG)
        ELSE
            ! Alternative method.Calculate dstage from leaf number
            IF (LNUMTOSTG(MSTG).GT.0.0) DSTAGE = LNUM/LNUMTOSTG(MSTG)
            IF (LAFND.GT.0.0) DSTAGE = LNUM/LAFND
        ENDIF 
        
        ! STAGES:Branching
        IF (GESTAGE.GE.0.5) THEN
            IF (MEDEV.EQ.'DEVU') THEN
                DO L = HSTG,1,-1
                    IF (CUMDU.GE.PSTART(L).AND.PD(L).GT.0.0) THEN
                        BRSTAGE = FLOAT(L) + (CUMDU-PSTART(L))/PD(L)
                        ! Brstage cannot go above harvest stage 
                        BRSTAGE = AMIN1(FLOAT(HSTG),BRSTAGE)
                        LNUMSIMTOSTG(L+1) = LNUM  ! To record simulated # 
                        EXIT
                    ENDIF
                ENDDO
            ELSEIF (MEDEV.EQ.'LNUM') THEN 
                ! Alternative method based on leaf numbers 
                DO L = HSTG,0,-1
                    IF (LNUM.GE.LNUMTOSTG(L)) THEN
                        IF (PDL(L).GT.0) BRSTAGE = FLOAT(L) + (LNUM-LNUMTOSTG(L))/PDL(L)
                        LNUMSIMTOSTG(L+1) = LNUM  ! To record simulated # 
                        ! Brstage cannot go above harvest stage 
                        BRSTAGE = AMIN1(FLOAT(HSTG),BRSTAGE)
                        EXIT
                    ENDIF  
                ENDDO
            ENDIF  
        ENDIF
        
        ! STAGES:leaf numbers 
        LNUM = AMAX1(0.0,(AMIN1(FLOAT(LNUMX-1),(LNUM+LNUMG))))
        LNUMSG = INT(LNUM)+1  ! Youngest growing leaf
        
        !-----------------------------------------------------------------------
        !         Record stage dates and states
        !-----------------------------------------------------------------------
        
        IF (INT(BRSTAGE).GT.10 .OR. INT(BRSTAGE).LT.0.AND.GESTAGE.GT.0.5) THEN
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
        IF (PSDAT(L).LE.0.0.AND.CUMDU.GE.PSTART(L)) THEN
            PSDAT(L) = YEARDOY
            IF (DU.GT.0.0) PSDAPFR(L)=(PSTART(L)-(CUMDU-DU))/DU
            PSDAPFR(L) = FLOAT(DAP) + PSDAPFR(L)
            PSDAP(L) = DAP
            IF (PSABV(L).EQ.'MDAT '.OR.L.EQ.MSTG) THEN
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
        
        IF (GESTAGE.GT.0.1) THEN
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
        
        !-----------------------------------------------------------------------
        !         Calculate nitrogen concentrations
        !-----------------------------------------------------------------------
                    
        IF (ISWNIT.NE.'N') THEN
            ! Critical and minimum N concentrations
            LNCX = LNCXS(0) + DSTAGE*(LNCXS(1)-LNCXS(0))
            SNCX = SNCXS(0) + DSTAGE*(SNCXS(1)-SNCXS(0))
            RNCX = RNCXS(0) + DSTAGE*(RNCXS(1)-RNCXS(0))
            LNCM = LNCMN(0) + DSTAGE*(LNCMN(1)-LNCMN(0))
            SNCM = SNCMN(0) + DSTAGE*(SNCMN(1)-SNCMN(0))
            RNCM = RNCMN(0) + DSTAGE*(RNCMN(1)-RNCMN(0))
                
            ! N concentrations
            RANC = 0.0
            LANC = 0.0
            SANC = 0.0
            VANC = 0.0
            VCNC = 0.0
            VMNC = 0.0
            IF (RTWT.GT.1.0E-5) RANC = ROOTN / RTWT
            IF (LFWT.GT.1.0E-5) LANC = LEAFN / LFWT
            IF (STWT+CRWT.GT.1.0E-5) SANC = STEMN / (STWT+CRWT)
            IF (VWAD.GT.0.0) VANC = VNAD/VWAD
            IF (LANC.LT.0.0) THEN 
                WRITE(Message(1),'(A27,F4.1)') 'LANC below 0 with value of ',LANC
                WRITE(Message(2),'(A27,2F5.1)') 'LEAFN,LFWT had values of   ',LEAFN,LFWT
                CALL WARNING(2,'CSCAS',MESSAGE)
                LANC = AMAX1(0.0,LANC)
            ENDIF
            IF (LFWT+STWT+CRWT.GT.0.0) VCNC = (LNCX*AMAX1(0.0,LFWT)+SNCX*AMAX1(0.0,STWT+CRWT))/ &
                (AMAX1(0.0,LFWT)+AMAX1(0.0,STWT+CRWT))
            IF (LFWT+STWT+CRWT.GT.0.0) VMNC = (LNCM*AMAX1(0.0,LFWT)+SNCM*AMAX1(0.0,STWT+CRWT))/ &
                (AMAX1(0.0,LFWT)+AMAX1(0.0,STWT+CRWT))
                
            SDNC = 0.0
            SRANC = 0.0
            IF (SEEDRS.GT.0.0) SDNC = SEEDN/(SEEDRS+SDCOAT)
            IF (SRWT.GT.0) SRANC = SROOTN/SRWT
            LNCR = 0.0
            SNCR = 0.0
            RNCR = 0.0
            IF (LNCX.GT.0.0) LNCR = AMAX1(0.0,AMIN1(1.0,LANC/LNCX))
            IF (SNCX.GT.0.0) SNCR = AMAX1(0.0,AMIN1(1.0,SANC/SNCX))
            IF (RNCX.GT.0.0) RNCR = AMAX1(0.0,AMIN1(1.0,RANC/RNCX))
        ELSE
            LNCR = 1.0
            SNCR = 1.0
            RNCR = 1.0
        ENDIF
            
6666        CONTINUE  ! Jump to here if germinating
            
        !-----------------------------------------------------------------------
        !         Determine if to harvest or fail
        !-----------------------------------------------------------------------
            
        ! Harvesting conditions
        IF (IHARI.EQ.'A' .AND. CUMDU.GE.PSTART(MSTG)) THEN
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
            CALL WARNING(1,'CSCAS',MESSAGE)
        ENDIF
        IF (IHARI.NE.'A'.AND.MDAT.GE.0.AND.DAP-MDAP.GE.300) THEN
            CFLFAIL = 'Y'
            WRITE (Message(1),'(A32)')'300 days after maturity         '
            WRITE (Message(2),'(A21)')'Harvesting triggered.'
            CALL WARNING(2,'CSCAS',MESSAGE)
        ENDIF
        IF (IHARI.NE.'A'.AND.CUMDU.GE.PSTART(MSTG-1)) THEN
            ! NB. Not work if MSTG=2
            IF (TT20.LE.-98.0.AND.PSTART(MSTG-1).GT.0.0) THEN
                CFLFAIL = 'Y'
                WRITE (Message(1),'(A28)') '20day thermal time mean = 0 '
                CALL WARNING(1,'CSCAS',MESSAGE)
            ENDIF
        ENDIF
        ! Determine if to harvest
        CFLHAR = 'N'
        IF (IHARI.EQ.'R'.AND.YEARDOYHARF.EQ.YEARDOY .OR. IHARI.EQ.'D'.AND.YEARDOYHARF.EQ.DAP .OR. IHARI.EQ.'G'.AND. &
            YEARDOYHARF.LE.BRSTAGE .OR. IHARI.EQ.'A'.AND.YEARDOYHARF.EQ.YEARDOY .OR. IHARI.EQ.'M'.AND.CUMDU.GE. &
            PSTART(MSTG)) THEN
            CFLHAR = 'Y'
        ENDIF
        IF(IHARI.EQ.'R'.AND.CFLHAR.EQ.'N')THEN
            IF (CUMDU.GT.PSTART(MSTG) .AND. CFLHARMSG .NE. 'Y') THEN
                WRITE(Message(1),'(A54,I7)') 'Maturity reached but waiting for reported harvest on: ', YEARDOYHARF 
                CALL WARNING(1,'CSCAS',MESSAGE)
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
                    CALL WARNING(1,'CSCAS',MESSAGE)
                ENDIF  
            ENDIF
        ENDIF
                
        !-----------------------------------------------------------------------
        !         Calculate season end soil conditions                              
        !-----------------------------------------------------------------------
                
        FSOILN = 0.0
        FSOILH2O = 0.0
        DO I = 1, NLAYR
            FSOILN = FSOILN + NO3LEFT(I)/(10.0/(BD(I)*(DLAYR(I)))) + NH4LEFT(I)/(10.0/(BD(I)*(DLAYR(I))))
            FSOILH2O = FSOILH2O + SW(I)*DLAYR(I)
        ENDDO
                
        !-----------------------------------------------------------------------
        !         Calculate variables that are generally measured and presented
        !-----------------------------------------------------------------------
                
        ! Here,reserves are included in leaf,stem,and crown weights
        ! And weights are in kg/ha
        CWAD = (LFWT+STWT+CRWT+RSWT)*PLTPOP*10.0
        SRWAD = SRWT*PLTPOP*10.0
        LLWAD = LFWT*(1.0-LPEFR)*10.0*PLTPOP
        LPEWAD = LFWT*LPEFR*10.0*PLTPOP
        RWAD = RTWT*PLTPOP*10.0
        SDWAD = (SEEDRS+SDCOAT)*10.0*PLTPOP
        ! Leaf petioles NOT included in stem here
        STWAD = STWT*10.0*PLTPOP
        CRWAD = CRWT*PLTPOP*10.0
        RSWAD = RSWT*PLTPOP*10.0
        LLRSWAD = LLRSWT*PLTPOP*10.0
        LPERSWAD = LPERSWT*PLTPOP*10.0
        STRSWAD = STRSWT*PLTPOP*10.0
        CRRSWAD = CRRSWT*PLTPOP*10.0
                
        ! Need to CHECK these
        SENROOTA = SENROOT*10.0*PLTPOP
        SENCAS = SENCS*10.0*PLTPOP
        SENLAS = SENLS*10.0*PLTPOP
        SENTOPLITTERA = SENTOPLITTER*PLTPOP*10.0
        DO L =1,NLAYR
            RTWTAL(L) = RTWTL(L)*PLTPOP*10.0
            SENWAL(L) = SENWL(L)*PLTPOP*10.0
        ENDDO
                
        TWAD = (SEEDRS+SDCOAT+RTWT+LFWT+STWT+CRWT+SRWT+RSWT)* PLTPOP*10.0
                
        VWAD = (LFWT+STWT+CRWT+RSWT)*PLTPOP * 10.0
                
        SHNUMAD = SHNUM*PLTPOP
                
        IF (NUPAC.LT.0.0) THEN
            NUPAC = NUPAD
        ELSE 
            NUPAC = NUPAC+NUPAD
        ENDIF  
        CNAD = (LEAFN+STEMN+RSN)*PLTPOP*10.0
        SRNAD = SROOTN*PLTPOP*10.0
        LLNAD = LEAFN*(1.0-LPEFR)*PLTPOP*10.0
        RNAD = ROOTN*PLTPOP*10.0
        RSNAD = RSN*PLTPOP*10.0
        SDNAD = SEEDN*PLTPOP*10.0
        SNAD = STEMN*PLTPOP*10.0
        TNAD = (ROOTN+LEAFN+STEMN+RSN+HPRODN+SEEDN)*PLTPOP*10.0
        VNAD = (LEAFN+STEMN+RSN)*PLTPOP*10.0
                
        ! LAH Note that no reserves included in sancout
        ! SANCOUT = SNAD/(STWAD+STRSWAD + LPEWAD+LPERSWAD)  ! With rs
        IF (STWAD.GT.1.0E-5) SANCOUT = SNAD/STWAD
                
        HWAD = SRWAD
        HWUD = SRWUD
        HNUMAD = SRNOPD * PLTPOP
        HNAD = SRNAD
        HNC = SRANC
        SENNAS = SENNS*10.0*PLTPOP
        SENNAL(0) = SENNL(0)*PLTPOP*10.0
        SENNATC = SENNAL(0)+SENNAS
        DO L =1,NLAYR
            SENNAL(L) = SENNL(L)*PLTPOP*10.0
        ENDDO
                
        ! After harvest residues
        IF (STGYEARDOY(11).EQ.YEARDOY) THEN
            ! Surface
            RESWALG(0) = VWAD*(1.0-HBPCF/100.0)
            RESNALG(0) = (LEAFN+STEMN)*PLTPOP*10.*(1.0-HBPCF/100.)
            RESCALG(0) = RESWALG(0) * 0.4
            RESLGALG(0) = LLWAD*LLIGP/100.0*(1.0-HBPCF/100.0)+ LPEWAD*SLIGP/100.0*(1.0-HBPCF/100.0)+ &
                STWAD*SLIGP/100.0*(1.0-HBPCF/100.0)
            ! Soil
            DO L = 1, NLAYR
                RESWALG(L) = RTWTL(L)*PLTPOP*10.0
                RESNALG(L) = RTWTL(L)*PLTPOP*10.0 * RANC
                RESCALG(L) = RTWTL(L)*PLTPOP*10.0 * 0.4
                RESLGALG(L) = RTWTL(L)*PLTPOP*10.0 * RLIGP/100.0
            ENDDO
                    
            ! Surface
            RESWAL(0) = RESWAL(0) + RESWALG(0)
            RESNAL(0) = RESNAL(0) + RESNALG(0)
            RESCAL(0) = RESCAL(0) + RESCALG(0)
            RESLGAL(0) = RESLGAL(0) + RESLGALG(0)
            ! Soil
            DO L = 1, NLAYR
                RESWAL(L) = RESWAL(L) + RESWALG(L)
                RESNAL(L) = RESNAL(L) + RESNALG(L)
                RESCAL(L) = RESCAL(L) + RESCALG(L)
                RESLGAL(L) = RESLGAL(L) + RESLGALG(L)
            ENDDO
        ENDIF
                
        !-----------------------------------------------------------------------
        !         Calculate weather and soil summary variables
        !-----------------------------------------------------------------------
                
        ! Cumulatives
        TTCUM = TTCUM + TT
        RAINC = RAINC + RAIN
        DRAINC = DRAINC + DRAIN
        RUNOFFC = RUNOFFC + RUNOFF
        IRRAMTC = IRRAMTC + IRRAMT
        SRADC = SRADC + SRAD
        PARMJC = PARMJC + PARMJFAC*SRAD
        PARMJIC = PARMJIC + PARMJFAC*SRAD*PARI + PARMJIADJ
        TOMINC = TOMINC + TOMIN
        TOFIXC = TOFIXC + TNIMBSOM
        TOMINFOMC = TOMINFOMC + TOMINFOM
        TOMINSOMC = TOMINSOMC + TOMINSOM
        IF (TOMINSOM1.GE.0.0) THEN
            TOMINSOM1C = TOMINSOM1C + TOMINSOM1
            TOMINSOM2C = TOMINSOM2C + TOMINSOM2
            TOMINSOM3C = TOMINSOM3C + TOMINSOM3
        ELSE
            TOMINSOM1C = -99.0
            TOMINSOM2C = -99.0
            TOMINSOM3C = -99.0
        ENDIF
        TLCHC = TLCHC + TLCHD
        TNOXC = TNOXC + TNOXD
        ! Extremes
        TMAXX = AMAX1(TMAXX,TMAX)
        TMINN = AMIN1(TMINN,TMIN)
        CO2MAX = AMAX1(CO2MAX,CO2)
                
        ! Growing season means
                
        TMEANNUM = TMEANNUM + 1
        TMEANSUM = TMEANSUM + TMEAN
                
        ! 20-day means
        SRAD20S = 0.0
        TMEAN20S = 0.0
        STRESS20S = 0.0
        STRESS20NS = 0.0
        STRESS20WS = 0.0
        TT20S = 0.0
        DO L = 20,2,-1
            SRADD(L) = SRADD(L-1)
            SRAD20S = SRAD20S + SRADD(L)
            TMEAND(L) = TMEAND(L-1)
            TMEAN20S = TMEAN20S + TMEAND(L)
            STRESS(L) = STRESS(L-1)
            STRESSN(L) = STRESSN(L-1)
            STRESSW(L) = STRESSW(L-1)
            STRESS20S = STRESS20S + STRESS(L)
            STRESS20NS = STRESS20NS + STRESSN(L)
            STRESS20WS = STRESS20WS + STRESSW(L)
            TTD(L) = TTD(L-1)
            TT20S = TT20S + TTD(L)
            WUPRD(L) = WUPRD(L-1)
        ENDDO
        SRADD(1) = SRAD
        SRAD20S = SRAD20S + SRAD
        TMEAND(1) = TMEAN
        TMEAN20S = TMEAN20S + TMEAND(1)
        STRESS(1) = AMIN1(WFG,NFG)
        STRESSN(1) = NFG
        STRESSW(1) = WFG
        STRESS20S = STRESS20S + STRESS(1)
        STRESS20NS = STRESS20NS + STRESSN(1)
        STRESS20WS = STRESS20WS + STRESSW(1)
        TTD(1) = TT
        TT20S = TT20S + TTD(1)
        WUPRD(1) = AMAX1(0.0,AMIN1(10.0,WUPR))
        IF (TMEANNUM.GE.20) THEN
            IF (TMEANNUM.LE.20) TMEAN20P = TMEAN20S/20.0
            SRAD20 = SRAD20S/20.0
            TMEAN20 = TMEAN20S/20.0
            TT20 = TT20S/20.0
            STRESS20 = STRESS20S/20.0
            STRESS20N = STRESS20NS/20.0
            STRESS20W = STRESS20WS/20.0
        ELSE
            SRAD20 = 0.0
            TT20 = 0.0
            TMEAN20 = 0.0
            STRESS20 = 0.0
            STRESS20N = 0.0
            STRESS20N = 0.0
        ENDIF
                
        ! Monthly means
        CALL Calendar (year,doy,dom,month)
        IF (DOM.GT.1) THEN
            TMAXSUM = TMAXSUM + TMAX
            TMINSUM = TMINSUM + TMIN
            DAYSUM = DAYSUM + 1.0
        ELSE
            IF (DAYSUM.GT.0) THEN
                IF (TMAXM.LT.TMAXSUM/DAYSUM) TMAXM=TMAXSUM/DAYSUM
                IF (TMINM.GT.TMINSUM/DAYSUM) TMINM=TMINSUM/DAYSUM
            ENDIF
            TMAXSUM = TMAX
            TMINSUM = TMIN
            DAYSUM =  1
        ENDIF
                
        !-----------------------------------------------------------------------
        !         Calculate PAR utilization efficiencies
        !-----------------------------------------------------------------------
                
        IF (PARMJC.GT.0.0) PARUEC = AMAX1(0.0,(RTWT+LFWT+STWT+CRWT+SRWT+RSWT+SENTOPLITTER+SENROOT-SEEDUSE)* &
            PLTPOP / PARMJC)
        IF (PARMJIC.GT.0.0) PARIUED = AMAX1(0.0,(RTWT+LFWT+STWT+CRWT+SRWT+RSWT+SENTOPLITTER+SENROOT-SEEDUSE)* &
            PLTPOP / PARMJIC)
        IF (CARBOBEG.GT.0.0) THEN
            PARIUE = (CARBOBEG*PLTPOP)/(PARMJFAC*SRAD*PARI)
        ENDIF
                
        !-----------------------------------------------------------------------
        !         Determine if nitrogen fertilizer applied
        !-----------------------------------------------------------------------
                
        IF (FERNIT.GT.FERNITPREV) THEN
            FAPPNUM = FAPPNUM + 1
            AMTNIT = FERNIT
            WRITE(fappline(fappnum),'(A1,I4,A10,I7,A13,I4,A6)') ' ',NINT(FERNIT-FERNITPREV),' kg/ha on ', YEARDOY, &
                '     To date ',NINT(amtnit),' kg/ha'
            FERNITPREV = FERNIT
        ENDIF
                
        !-----------------------------------------------------------------------
        !         Calculate water availability ratio
        !-----------------------------------------------------------------------
                
        BASELAYER = 0.0
        H2OA = 0.0
        IF (ISWWAT.NE.'N') THEN
            DO L = 1, NLAYR
                DLAYRTMP(L) = DLAYR(L)
                BASELAYER = BASELAYER + DLAYR(L)
                IF (RTDEP.GT.0.0.AND.RTDEP.LT.BASELAYER) THEN
                    DLAYRTMP(L) = RTDEP-(BASELAYER-DLAYR(L))
                    IF (DLAYRTMP(L).LE.0.0) EXIT
                ENDIF
                H2OA = H2OA + 10.0*AMAX1(0.0,(SW(L)-LL(L))*DLAYRTMP(L))
            ENDDO
            IF (EOP.GT.0.0) THEN
                WAVR = H2OA/EOP
            ELSE
                WAVR = 99.9
            ENDIF
        ENDIF
                
        !-----------------------------------------------------------------------
        !         Upgrade albedo
        !-----------------------------------------------------------------------
                
        ! When running in CSM
        IF (FILEIOT.EQ.'DS4') THEN
            IF (LAI .LE. 0.0) THEN
                ALBEDO = ALBEDOS
            ELSE
                ALBEDO = 0.23-(0.23-ALBEDOS)*EXP(-0.75*LAI)
            ENDIF
        ELSE
            ALBEDO = ALBEDOS  
        ENDIF
                
        !-----------------------------------------------------------------------
        !         Compute weights,etc. at end crop
        !-----------------------------------------------------------------------
                
        IF (STGYEARDOY(11).EQ.YEARDOY) THEN
                    
            ! LAH No adjustment for fraction of day to maturity
            RSWTM = RSWT
            RTWTM = RTWT
            LFWTM = LFWT
            STWTM = STWT
            CRWTM = CRWT                   
            LNUMSM = LNUM
                    
            IF (LFWTM+STWTM+CRWTM+RSWTM.GT.0.0) RSCM = RSWTM/(LFWTM+STWTM+CRWTM)
            IF (RTWTM.GT.0.0) SHRTM = (LFWTM+STWTM+CRWTM+RSWTM)/RTWTM
                    
            CWAM = (LFWTM+STWTM+CRWTM+RSWTM)*PLTPOP*10.0
            VWAM = (LFWTM+STWTM+CRWTM+RSWTM)*PLTPOP * 10.0
                    
            ! For Grazing
            cwahc = (lwphc+swphc+rswphc)*pltpop*10.0
            ! Adjustments for spikes that removed by grazing,etc..
                    
            RWAM = RTWTM*PLTPOP*10.0
            SDWAM = (SEEDRS+SDCOAT)*PLTPOP*10.0
                    
            IF (CWAM.GT.0.0) THEN
                HIAM = HIAD
            ENDIF
                    
            SENWACM = SENTOPLITTERA+SENROOTA
                    
            RSWAM = RSWAD
                    
            CNAM = CNAD
            VNAM = VNAD
            VNPCM = VANC*100.0
            RNAM = RNAD
            SRNAM = SRNAD
                    
            HINM = HIND
                    
            ! Set harvest product outputs
            HWAM = SRWT * PLTPOP * 10.0
            HNAM = SRNAM
            IF (SRNOPD.GT.0.0) HWUM = SRWT/FLOAT(SRNOPD)
            HNUMAM = FLOAT(SRNOPD)*PLTPOP
            HNUMGM = FLOAT(SRNOPD)
            HNUMPM = FLOAT(SRNOPD)
            BRNUMSH = BRNUMST
            IF (SRWT.GT.0.0) HNPCM = SROOTN/SRWT*100.0
                    
        ENDIF
                
    END SUBROUTINE CS_Integrate
    
    